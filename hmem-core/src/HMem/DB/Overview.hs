module HMem.DB.Overview
  ( getTaskOverview
  , getContextInfo
  ) where

import Data.Function ((&))
import Data.Functor.Contravariant ((>$<))
import Data.List (sortBy)
import Data.Map.Strict qualified as Map
import Data.Ord (Down(..), comparing)
import Data.Pool (Pool)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime)
import Data.UUID (UUID)
import Hasql.Connection qualified as Hasql
import Hasql.Session qualified as Session
import Rel8 hiding (filter, null)

import HMem.DB.Memory qualified as Mem
import HMem.DB.Pool (runSession)
import HMem.DB.Project qualified as Proj
import HMem.DB.Schema
import HMem.DB.Task qualified as Task
import HMem.Types

emptyLq :: LinkedMemoryListQuery
emptyLq = LinkedMemoryListQuery Nothing Nothing Nothing Nothing Nothing

data MemoryCandidate = MemoryCandidate
  { candidateId         :: UUID
  , candidateSummary    :: Text
  , candidateScope      :: ContextMemoryScope
  , candidatePinned     :: Bool
  , candidateImportance :: Int
  , candidateUpdatedAt  :: UTCTime
  }

getTaskOverview :: Pool Hasql.Connection -> UUID -> Bool -> IO (Maybe TaskOverview)
getTaskOverview pool taskId extraContext = do
  mTask <- Task.getTask pool taskId
  case mTask of
    Nothing -> pure Nothing
    Just task -> do
      dependencies <- listTaskDependencySummaries pool taskId
      directMemories <- Mem.getTaskMemories pool taskId emptyLq
      projectMemories <- case (extraContext, task.projectId) of
        (True, Just projectId) -> Mem.getProjectMemories pool projectId emptyLq
        _                      -> pure []
      workspaceMemories <-
        if extraContext
          then listWorkspaceMemoryCandidates pool task.workspaceId
          else pure []
      let candidates =
            [ memoryCandidateFromMemory ScopeTask memory | memory <- directMemories ]
            <> [ memoryCandidateFromMemory ScopeProject memory | memory <- projectMemories ]
            <> workspaceMemories
          connectedMemories = summarizeCandidates candidates
      pure $ Just TaskOverview
        { task = task
        , dependencies = dependencies
        , connectedMemories = connectedMemories
        }

-- | Retrieve context info for a task, with memories grouped by scope
-- (task, project ancestors, workspace) and limited per scope according
-- to the detail level.
getContextInfo
  :: Pool Hasql.Connection -> UUID -> ContextDetailLevel -> IO (Maybe ContextInfo)
getContextInfo pool taskId level = do
  mTask <- Task.getTask pool taskId
  case mTask of
    Nothing -> pure Nothing
    Just task -> do
      let n = contextDetailLimit level

      -- Task-linked memories (top N by pinned, importance, recency)
      taskMems <- Mem.getTaskMemories pool taskId emptyLq
      let taskCandidates = map (memoryCandidateFromMemory ScopeTask) taskMems
          taskSummaries  = take n $ summarizeCandidates taskCandidates

      -- Walk parent project chain collecting memories
      projCandidates <- collectProjectMemories pool task.projectId []
      let projSummaries = take n $ summarizeCandidates projCandidates

      -- Workspace-level memories (top N)
      wsCandidates <- listWorkspaceMemoryCandidates pool task.workspaceId
      let wsSummaries = take n $ map toConnectedMemorySummary wsCandidates

      pure $ Just ContextInfo
        { task              = task
        , detailLevel       = level
        , taskMemories      = taskSummaries
        , projectMemories   = projSummaries
        , workspaceMemories = wsSummaries
        }

-- | Walk the project parent chain, collecting memory candidates from
-- each ancestor project.
collectProjectMemories
  :: Pool Hasql.Connection -> Maybe UUID -> [MemoryCandidate] -> IO [MemoryCandidate]
collectProjectMemories _pool Nothing acc = pure acc
collectProjectMemories pool (Just projId) acc = do
  mems <- Mem.getProjectMemories pool projId emptyLq
  let candidates = map (memoryCandidateFromMemory ScopeProject) mems
  mProj <- Proj.getProject pool projId
  let parentId = mProj >>= (.parentId)
  collectProjectMemories pool parentId (acc <> candidates)

listTaskDependencySummaries :: Pool Hasql.Connection -> UUID -> IO [TaskDependencySummary]
listTaskDependencySummaries pool taskId = do
  rows <- runSession pool $ Session.statement () $ run $ select $
    orderBy ((\(_, taskName) -> taskName) >$< asc) $ do
      dependency <- each taskDependencySchema
      where_ $ dependency.tdTaskId ==. lit taskId
      task <- each taskSchema
      where_ $ task.taskId ==. dependency.tdDependsOnId
      where_ $ activeTask task
      pure (task.taskId, task.taskTitle)
  pure
    [ TaskDependencySummary { id = dependencyId, name = dependencyName }
    | (dependencyId, dependencyName) <- rows
    ]

listWorkspaceMemoryCandidates :: Pool Hasql.Connection -> UUID -> IO [MemoryCandidate]
listWorkspaceMemoryCandidates pool workspaceId = do
  rows <- runSession pool $ Session.statement () $ run $ select $
    orderBy
      (((\row -> row.memPinned) >$< desc)
      <> ((\row -> row.memImportance) >$< desc)
      <> ((\row -> row.memUpdatedAt) >$< desc)) $ do
        row <- each memorySchema
        where_ $ row.memWorkspaceId ==. lit workspaceId
        where_ $ activeMemory row
        pure row
  pure [memoryCandidateFromRow ScopeWorkspace row | row <- rows]

summarizeCandidates :: [MemoryCandidate] -> [ConnectedMemorySummary]
summarizeCandidates candidates =
  candidates
    & Map.fromListWith pickPreferred
        . map (\candidate -> (candidate.candidateId, candidate))
    & Map.elems
    & sortBy (comparing candidateSortKey)
    & map toConnectedMemorySummary

pickPreferred :: MemoryCandidate -> MemoryCandidate -> MemoryCandidate
pickPreferred left right
  | candidateSortKey left <= candidateSortKey right = left
  | otherwise = right

candidateSortKey :: MemoryCandidate -> (Int, Down Bool, Down Int, Down UTCTime, Text)
candidateSortKey candidate =
  ( fromEnum candidate.candidateScope
  , Down candidate.candidatePinned
  , Down candidate.candidateImportance
  , Down candidate.candidateUpdatedAt
  , candidate.candidateSummary
  )

toConnectedMemorySummary :: MemoryCandidate -> ConnectedMemorySummary
toConnectedMemorySummary candidate = ConnectedMemorySummary
  { id = candidate.candidateId
  , summary = candidate.candidateSummary
  , scope = candidate.candidateScope
  }

memoryCandidateFromMemory :: ContextMemoryScope -> Memory -> MemoryCandidate
memoryCandidateFromMemory scope memory = MemoryCandidate
  { candidateId = memory.id
  , candidateSummary = memorySummaryText memory.summary memory.content
  , candidateScope = scope
  , candidatePinned = memory.pinned
  , candidateImportance = memory.importance
  , candidateUpdatedAt = memory.updatedAt
  }

memoryCandidateFromRow :: ContextMemoryScope -> MemoryT Result -> MemoryCandidate
memoryCandidateFromRow scope row = MemoryCandidate
  { candidateId = row.memId
  , candidateSummary = memorySummaryText row.memSummary row.memContent
  , candidateScope = scope
  , candidatePinned = row.memPinned
  , candidateImportance = fromIntegral row.memImportance
  , candidateUpdatedAt = row.memUpdatedAt
  }

memorySummaryText :: Maybe Text -> Text -> Text
memorySummaryText memorySummary content =
  case memorySummary of
    Just summaryText
      | not (T.null strippedSummary) -> strippedSummary
      where strippedSummary = T.strip summaryText
    _ -> truncateText content

truncateText :: Text -> Text
truncateText text
  | T.length text > 200 = T.take 200 text <> "..."
  | otherwise = text
