module HMem.DB.Overview
  ( getTaskOverview
  , getWorkspaceVisualization
  ) where

import Data.Function ((&))
import Data.Functor.Contravariant ((>$<))
import Data.Int (Int16)
import Data.List (sortBy)
import Data.Map.Strict qualified as Map
import Data.Ord (Down(..), comparing)
import Data.Pool (Pool)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime)
import Data.UUID (UUID)
import Hasql.Connection qualified as Hasql
import Hasql.Session qualified as Session
import Rel8 hiding (filter, null)

import HMem.DB.Memory qualified as Mem
import HMem.DB.Pool (runSession)
import HMem.DB.Schema
import HMem.DB.Task qualified as Task
import HMem.Types

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
      directMemories <- Mem.getTaskMemories pool taskId
      projectMemories <- case (extraContext, task.projectId) of
        (True, Just projectId) -> Mem.getProjectMemories pool projectId
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

getWorkspaceVisualization
  :: Pool Hasql.Connection
  -> UUID
  -> WorkspaceVisualizationQuery
  -> IO (Maybe WorkspaceVisualization)
getWorkspaceVisualization pool workspaceId query = do
  mWorkspace <- getActiveWorkspace pool workspaceId
  case mWorkspace of
    Nothing -> pure Nothing
    Just workspace -> do
      allProjects <- listWorkspaceProjects pool workspaceId
      let projects = applyProjectFilters query allProjects
          selectedProjectIds = Set.fromList (map (.id) projects)
      tasks <- listWorkspaceTasks pool workspaceId query.taskStatuses
      let filteredTasks = filter (taskVisibleInProjects selectedProjectIds) tasks
      memories <- listVisualizationMemories pool workspaceId query.memoryFilter
      let projectIds = map (.id) projects
          taskIds = map (.id) filteredTasks
          memoryIds = map (.id) memories
      taskDependencies <- listVisualizationTaskDependencies pool taskIds
      projectMemoryLinks <- listVisualizationProjectMemoryLinks pool projectIds memoryIds
      taskMemoryLinks <- listVisualizationTaskMemoryLinks pool taskIds memoryIds
      memoryLinks <- listVisualizationMemoryLinks pool memoryIds
      pure $ Just WorkspaceVisualization
        { workspace = workspace
        , projects = projects
        , tasks = filteredTasks
        , taskDependencies = taskDependencies
        , memories = memories
        , projectMemoryLinks = projectMemoryLinks
        , taskMemoryLinks = taskMemoryLinks
        , memoryLinks = memoryLinks
        }

getActiveWorkspace :: Pool Hasql.Connection -> UUID -> IO (Maybe Workspace)
getActiveWorkspace pool workspaceId = do
  rows <- runSession pool $ Session.statement () $ run $ select $ do
    row <- each workspaceSchema
    where_ $ row.wsId ==. lit workspaceId
    where_ $ activeWorkspace row
    pure row
  case rows of
    []    -> pure Nothing
    (r:_) -> pure . Just $ rowToWorkspace r

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

listWorkspaceProjects :: Pool Hasql.Connection -> UUID -> IO [Project]
listWorkspaceProjects pool workspaceId = do
  rows <- runSession pool $ Session.statement () $ run $ select $
    orderBy (((\row -> row.projPriority) >$< desc) <> ((\row -> row.projName) >$< asc)) $ do
      row <- each projectSchema
      where_ $ row.projWorkspaceId ==. lit workspaceId
      where_ $ activeProject row
      pure row
  pure $ map rowToProject rows

applyProjectFilters :: WorkspaceVisualizationQuery -> [Project] -> [Project]
applyProjectFilters query = filter visible
  where
    includeIds = maybe Set.empty Set.fromList query.includeProjectIds
    excludeIds = maybe Set.empty Set.fromList query.excludeProjectIds
    hasIncludeFilter = not (Set.null includeIds)
    visible project =
      (not hasIncludeFilter || Set.member project.id includeIds)
      && not (Set.member project.id excludeIds)

listWorkspaceTasks :: Pool Hasql.Connection -> UUID -> Maybe [TaskStatus] -> IO [Task]
listWorkspaceTasks pool workspaceId mStatuses = do
  rows <- runSession pool $ Session.statement () $ run $ select $
    orderBy (((\row -> row.taskPriority) >$< desc) <> ((\row -> row.taskCreatedAt) >$< asc)) $ do
      row <- each taskSchema
      where_ $ row.taskWorkspaceId ==. lit workspaceId
      where_ $ activeTask row
      case mStatuses of
        Just statuses | not (null statuses) -> where_ $ in_ row.taskStatus (map lit statuses)
        _ -> pure ()
      pure row
  pure $ map rowToTask rows

taskVisibleInProjects :: Set.Set UUID -> Task -> Bool
taskVisibleInProjects selectedProjectIds task =
  case task.projectId of
    Nothing -> True
    Just projectId -> Set.member projectId selectedProjectIds

listVisualizationMemories
  :: Pool Hasql.Connection
  -> UUID
  -> Maybe WorkspaceVisualizationMemoryFilter
  -> IO [VisualizationMemory]
listVisualizationMemories pool workspaceId mFilter = do
  rows <- runSession pool $ Session.statement () $ run $ select $
    orderBy
      (((\row -> row.memPinned) >$< desc)
      <> ((\row -> row.memImportance) >$< desc)
      <> ((\row -> row.memUpdatedAt) >$< desc)) $ do
        row <- each memorySchema
        where_ $ row.memWorkspaceId ==. lit workspaceId
        where_ $ activeMemory row
        case mFilter >>= (\memoryFilter -> memoryFilter.memoryType) of
          Just memoryType -> where_ $ row.memMemoryType ==. lit memoryType
          Nothing -> pure ()
        case mFilter >>= (\memoryFilter -> memoryFilter.minImportance) of
          Just minimumImportance ->
            where_ $ row.memImportance >=. lit (fromIntegral minimumImportance :: Int16)
          Nothing -> pure ()
        case mFilter >>= (\memoryFilter -> memoryFilter.pinnedOnly) of
          Just True -> where_ $ row.memPinned ==. lit True
          _ -> pure ()
        case mFilter >>= (\memoryFilter -> memoryFilter.tags) of
          Just tags | not (null tags) -> present $ do
            tag <- each memoryTagSchema
            where_ $ tag.mtMemoryId ==. row.memId
            where_ $ in_ tag.mtTag (map lit tags)
          _ -> pure ()
        pure row
  pure $ map visualizationMemoryFromRow rows

visualizationMemoryFromRow :: MemoryT Result -> VisualizationMemory
visualizationMemoryFromRow row = VisualizationMemory
  { id = row.memId
  , summary = memorySummaryText row.memSummary row.memContent
  , memoryType = row.memMemoryType
  , importance = fromIntegral row.memImportance
  , pinned = row.memPinned
  }

listVisualizationTaskDependencies
  :: Pool Hasql.Connection
  -> [UUID]
  -> IO [VisualizationTaskDependency]
listVisualizationTaskDependencies _ [] = pure []
listVisualizationTaskDependencies pool taskIds = do
  rows <- runSession pool $ Session.statement () $ run $ select $ do
    row <- each taskDependencySchema
    where_ $ in_ row.tdTaskId (map lit taskIds)
    where_ $ in_ row.tdDependsOnId (map lit taskIds)
    pure row
  pure
    [ VisualizationTaskDependency
        { taskId = row.tdTaskId
        , dependsOnId = row.tdDependsOnId
        }
    | row <- rows
    ]

listVisualizationProjectMemoryLinks
  :: Pool Hasql.Connection
  -> [UUID]
  -> [UUID]
  -> IO [VisualizationProjectMemoryLink]
listVisualizationProjectMemoryLinks _ [] _ = pure []
listVisualizationProjectMemoryLinks _ _ [] = pure []
listVisualizationProjectMemoryLinks pool projectIds memoryIds = do
  rows <- runSession pool $ Session.statement () $ run $ select $ do
    row <- each projectMemoryLinkSchema
    where_ $ in_ row.pmlProjectId (map lit projectIds)
    where_ $ in_ row.pmlMemoryId (map lit memoryIds)
    pure row
  pure
    [ VisualizationProjectMemoryLink
        { projectId = row.pmlProjectId
        , memoryId = row.pmlMemoryId
        }
    | row <- rows
    ]

listVisualizationTaskMemoryLinks
  :: Pool Hasql.Connection
  -> [UUID]
  -> [UUID]
  -> IO [VisualizationTaskMemoryLink]
listVisualizationTaskMemoryLinks _ [] _ = pure []
listVisualizationTaskMemoryLinks _ _ [] = pure []
listVisualizationTaskMemoryLinks pool taskIds memoryIds = do
  rows <- runSession pool $ Session.statement () $ run $ select $ do
    row <- each taskMemoryLinkSchema
    where_ $ in_ row.tmlTaskId (map lit taskIds)
    where_ $ in_ row.tmlMemoryId (map lit memoryIds)
    pure row
  pure
    [ VisualizationTaskMemoryLink
        { taskId = row.tmlTaskId
        , memoryId = row.tmlMemoryId
        }
    | row <- rows
    ]

listVisualizationMemoryLinks :: Pool Hasql.Connection -> [UUID] -> IO [MemoryLink]
listVisualizationMemoryLinks _ [] = pure []
listVisualizationMemoryLinks pool memoryIds = do
  rows <- runSession pool $ Session.statement () $ run $ select $
    orderBy ((\row -> row.mlCreatedAt) >$< desc) $ do
      row <- each memoryLinkSchema
      where_ $ in_ row.mlSourceId (map lit memoryIds)
      where_ $ in_ row.mlTargetId (map lit memoryIds)
      pure row
  pure
    [ MemoryLink
        { sourceId = row.mlSourceId
        , targetId = row.mlTargetId
        , relationType = row.mlRelationType
        , strength = row.mlStrength
        , createdAt = row.mlCreatedAt
        }
    | row <- rows
    ]

rowToWorkspace :: WorkspaceT Result -> Workspace
rowToWorkspace row = Workspace
  { id = row.wsId
  , name = row.wsName
  , workspaceType = row.wsType
  , path = row.wsPath
  , ghOwner = row.wsGhOwner
  , ghRepo = row.wsGhRepo
  , createdAt = row.wsCreatedAt
  , updatedAt = row.wsUpdatedAt
  }

rowToProject :: ProjectT Result -> Project
rowToProject row = Project
  { id = row.projId
  , workspaceId = row.projWorkspaceId
  , parentId = row.projParentId
  , name = row.projName
  , description = row.projDescription
  , status = row.projStatus
  , priority = fromIntegral row.projPriority
  , metadata = row.projMetadata
  , createdAt = row.projCreatedAt
  , updatedAt = row.projUpdatedAt
  }

rowToTask :: TaskT Result -> Task
rowToTask row = Task
  { id = row.taskId
  , workspaceId = row.taskWorkspaceId
  , projectId = row.taskProjectId
  , parentId = row.taskParentId
  , title = row.taskTitle
  , description = row.taskDescription
  , status = row.taskStatus
  , priority = fromIntegral row.taskPriority
  , metadata = row.taskMetadata
  , dueAt = row.taskDueAt
  , completedAt = row.taskCompletedAt
  , dependencyCount = 0
  , memoryLinkCount = 0
  , createdAt = row.taskCreatedAt
  , updatedAt = row.taskUpdatedAt
  }