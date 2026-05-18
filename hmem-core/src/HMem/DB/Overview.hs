module HMem.DB.Overview
  ( getTaskOverview
  , getProjectOverview
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
import Data.ByteString.Char8 qualified as BS8
import Hasql.Connection qualified as Hasql
import Hasql.Decoders qualified as Dec
import Hasql.Encoders qualified as Enc
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement
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
      readinessRollup <- getTaskReadinessRollup pool taskId
      pure $ Just TaskOverview
        { task = task
        , dependencies = dependencies
        , connectedMemories = connectedMemories
        , readinessRollup = readinessRollup
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

getProjectOverview :: Pool Hasql.Connection -> UUID -> Bool -> IO (Maybe ProjectOverview)
getProjectOverview pool projId extraContext = do
  mProj <- Proj.getProject pool projId
  case mProj of
    Nothing -> pure Nothing
    Just proj -> do
      tasks <- Task.listTasksWithQuery pool TaskListQuery
        { workspaceId = Nothing, projectId = Just projId, status = Nothing
        , priority = Nothing, query = Nothing, searchLanguage = Nothing
        , createdAfter = Nothing, createdBefore = Nothing
        , updatedAfter = Nothing, updatedBefore = Nothing
        , limit = Just 200, offset = Just 0
        }
      allProjects <- Proj.listProjectsWithQuery pool ProjectListQuery
        { workspaceId = Just proj.workspaceId, status = Nothing
        , query = Nothing, searchLanguage = Nothing
        , createdAfter = Nothing, createdBefore = Nothing
        , updatedAfter = Nothing, updatedBefore = Nothing
        , limit = Just 200, offset = Just 0
        }
      let childProjects = Prelude.filter (\p -> p.parentId == Just projId) allProjects
      directMemories <- Mem.getProjectMemories pool projId emptyLq
      workspaceMemories <-
        if extraContext
          then listWorkspaceMemoryCandidates pool proj.workspaceId
          else pure []
      let candidates =
            [ memoryCandidateFromMemory ScopeProject memory | memory <- directMemories ]
            <> workspaceMemories
          connectedMemories = summarizeCandidates candidates
      readinessRollup <- getProjectReadinessRollup pool projId
      pure $ Just ProjectOverview
        { project = proj
        , tasks = tasks
        , subprojects = childProjects
        , linkedMemories = directMemories
        , connectedMemories = connectedMemories
        , readinessRollup = readinessRollup
        }

getTaskReadinessRollup :: Pool Hasql.Connection -> UUID -> IO TaskReadinessRollup
getTaskReadinessRollup pool taskId =
  runSession pool $ Session.statement taskId taskReadinessRollupStatement

taskReadinessRollupStatement :: Statement.Statement UUID TaskReadinessRollup
taskReadinessRollupStatement = Statement.Statement sql encoder decoder True
  where
    sql = BS8.pack $ unlines
      [ "WITH RECURSIVE task_tree(id, parent_id, status) AS ("
      , "  SELECT id, parent_id, status"
      , "    FROM tasks"
      , "   WHERE id = $1"
      , "     AND deleted_at IS NULL"
      , "  UNION"
      , "  SELECT child.id, child.parent_id, child.status"
      , "    FROM tasks child"
      , "    JOIN task_tree parent ON child.parent_id = parent.id"
      , "   WHERE child.deleted_at IS NULL"
      , "),"
      , "desc_tasks AS ("
      , "  SELECT * FROM task_tree WHERE id <> $1"
      , "),"
      , "open_dependency_edges AS ("
      , "  SELECT DISTINCT tree.id AS task_id, dep.id AS dep_id"
      , "    FROM task_tree tree"
      , "    JOIN task_dependencies dep_link ON dep_link.task_id = tree.id"
      , "    JOIN tasks dep ON dep.id = dep_link.depends_on_id"
      , "   WHERE dep.deleted_at IS NULL"
      , "     AND hmem_is_open_task_status(tree.status)"
      , "     AND hmem_is_open_task_status(dep.status)"
      , ")"
      , "SELECT (SELECT count(*)::bigint FROM desc_tasks WHERE hmem_is_open_task_status(status)),"
      , "       (SELECT count(*)::bigint FROM desc_tasks WHERE status = 'done'::task_status_enum),"
      , "       (SELECT count(*)::bigint FROM desc_tasks WHERE status = 'cancelled'::task_status_enum),"
      , "       (SELECT count(*)::bigint FROM desc_tasks WHERE status = 'blocked'::task_status_enum),"
      , "       (SELECT count(DISTINCT task_id)::bigint FROM open_dependency_edges),"
      , "       (SELECT count(*)::bigint FROM open_dependency_edges),"
      , "       NOT EXISTS (SELECT 1 FROM desc_tasks WHERE hmem_is_open_task_status(status))"
      ]
    encoder = Enc.param (Enc.nonNullable Enc.uuid)
    decoder = Dec.singleRow taskReadinessRollupRowDecoder

taskReadinessRollupRowDecoder :: Dec.Row TaskReadinessRollup
taskReadinessRollupRowDecoder = do
  openSubtaskCount <- Dec.column (Dec.nonNullable Dec.int8)
  doneSubtaskCount <- Dec.column (Dec.nonNullable Dec.int8)
  cancelledSubtaskCount <- Dec.column (Dec.nonNullable Dec.int8)
  blockedSubtaskCount <- Dec.column (Dec.nonNullable Dec.int8)
  dependencyBlockedTaskCount <- Dec.column (Dec.nonNullable Dec.int8)
  openDependencyCount <- Dec.column (Dec.nonNullable Dec.int8)
  completionReady <- Dec.column (Dec.nonNullable Dec.bool)
  pure TaskReadinessRollup
    { openSubtaskCount = fromIntegral openSubtaskCount
    , doneSubtaskCount = fromIntegral doneSubtaskCount
    , cancelledSubtaskCount = fromIntegral cancelledSubtaskCount
    , blockedSubtaskCount = fromIntegral blockedSubtaskCount
    , dependencyBlockedTaskCount = fromIntegral dependencyBlockedTaskCount
    , openDependencyCount = fromIntegral openDependencyCount
    , completionReady = completionReady
    }

getProjectReadinessRollup :: Pool Hasql.Connection -> UUID -> IO ProjectReadinessRollup
getProjectReadinessRollup pool projectId =
  runSession pool $ Session.statement projectId projectReadinessRollupStatement

projectReadinessRollupStatement :: Statement.Statement UUID ProjectReadinessRollup
projectReadinessRollupStatement = Statement.Statement sql encoder decoder True
  where
    sql = BS8.pack $ unlines
      [ "WITH RECURSIVE project_tree(id, parent_id, status, workspace_id) AS ("
      , "  SELECT id, parent_id, status, workspace_id"
      , "    FROM projects"
      , "   WHERE id = $1"
      , "     AND deleted_at IS NULL"
      , "  UNION"
      , "  SELECT child.id, child.parent_id, child.status, child.workspace_id"
      , "    FROM projects child"
      , "    JOIN project_tree parent ON child.parent_id = parent.id"
      , "   WHERE child.deleted_at IS NULL"
      , "     AND child.workspace_id = parent.workspace_id"
      , "),"
      , "desc_projects AS ("
      , "  SELECT * FROM project_tree WHERE id <> $1"
      , "),"
      , "task_tree(id, parent_id, status) AS ("
      , "  SELECT t.id, t.parent_id, t.status"
      , "    FROM tasks t"
      , "    JOIN project_tree project ON t.project_id = project.id"
      , "   WHERE t.deleted_at IS NULL"
      , "     AND t.workspace_id = project.workspace_id"
      , "  UNION"
      , "  SELECT child.id, child.parent_id, child.status"
      , "    FROM tasks child"
      , "    JOIN task_tree parent ON child.parent_id = parent.id"
      , "   WHERE child.deleted_at IS NULL"
      , "),"
      , "open_dependency_edges AS ("
      , "  SELECT DISTINCT tree.id AS task_id, dep.id AS dep_id"
      , "    FROM task_tree tree"
      , "    JOIN task_dependencies dep_link ON dep_link.task_id = tree.id"
      , "    JOIN tasks dep ON dep.id = dep_link.depends_on_id"
      , "   WHERE dep.deleted_at IS NULL"
      , "     AND hmem_is_open_task_status(tree.status)"
      , "     AND hmem_is_open_task_status(dep.status)"
      , ")"
      , "SELECT (SELECT count(*)::bigint FROM desc_projects WHERE status IN ('active'::project_status_enum, 'paused'::project_status_enum)),"
      , "       (SELECT count(*)::bigint FROM desc_projects WHERE status IN ('completed'::project_status_enum, 'archived'::project_status_enum)),"
      , "       (SELECT count(*)::bigint FROM task_tree WHERE hmem_is_open_task_status(status)),"
      , "       (SELECT count(*)::bigint FROM task_tree WHERE status = 'done'::task_status_enum),"
      , "       (SELECT count(*)::bigint FROM task_tree WHERE status = 'cancelled'::task_status_enum),"
      , "       (SELECT count(*)::bigint FROM task_tree WHERE status = 'blocked'::task_status_enum),"
      , "       (SELECT count(DISTINCT task_id)::bigint FROM open_dependency_edges),"
      , "       (SELECT count(*)::bigint FROM open_dependency_edges),"
      , "       NOT EXISTS (SELECT 1 FROM desc_projects WHERE status IN ('active'::project_status_enum, 'paused'::project_status_enum))"
      , "       AND NOT EXISTS (SELECT 1 FROM task_tree WHERE hmem_is_open_task_status(status))"
      ]
    encoder = Enc.param (Enc.nonNullable Enc.uuid)
    decoder = Dec.singleRow projectReadinessRollupRowDecoder

projectReadinessRollupRowDecoder :: Dec.Row ProjectReadinessRollup
projectReadinessRollupRowDecoder = do
  openProjectCount <- Dec.column (Dec.nonNullable Dec.int8)
  closedProjectCount <- Dec.column (Dec.nonNullable Dec.int8)
  openTaskCount <- Dec.column (Dec.nonNullable Dec.int8)
  doneTaskCount <- Dec.column (Dec.nonNullable Dec.int8)
  cancelledTaskCount <- Dec.column (Dec.nonNullable Dec.int8)
  blockedTaskCount <- Dec.column (Dec.nonNullable Dec.int8)
  dependencyBlockedTaskCount <- Dec.column (Dec.nonNullable Dec.int8)
  openDependencyCount <- Dec.column (Dec.nonNullable Dec.int8)
  completionReady <- Dec.column (Dec.nonNullable Dec.bool)
  pure ProjectReadinessRollup
    { openProjectCount = fromIntegral openProjectCount
    , closedProjectCount = fromIntegral closedProjectCount
    , openTaskCount = fromIntegral openTaskCount
    , doneTaskCount = fromIntegral doneTaskCount
    , cancelledTaskCount = fromIntegral cancelledTaskCount
    , blockedTaskCount = fromIntegral blockedTaskCount
    , dependencyBlockedTaskCount = fromIntegral dependencyBlockedTaskCount
    , openDependencyCount = fromIntegral openDependencyCount
    , completionReady = completionReady
    }

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
