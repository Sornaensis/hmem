module HMem.DB.Search
  ( searchAll
  ) where

import Control.Concurrent.Async (concurrently)
import Data.Maybe (fromMaybe)
import Data.Pool (Pool)
import Data.Set qualified as Set
import Hasql.Connection qualified as Hasql

import HMem.DB.Memory qualified as Mem
import HMem.DB.Project qualified as Proj
import HMem.DB.Task qualified as Task
import HMem.Types

emptyLq :: LinkedMemoryListQuery
emptyLq = LinkedMemoryListQuery Nothing Nothing Nothing Nothing Nothing

-- | Run a unified full-text search across memories, projects, and tasks
-- in parallel. Returns per-entity results with linked memory summaries
-- attached to project and task hits.
searchAll :: Pool Hasql.Connection -> UnifiedSearchQuery -> IO UnifiedSearchResults
searchAll pool usq = do
  let types = maybe [SearchMemory, SearchProject, SearchTask]
                    id
                    usq.entityTypes
      typeSet = Set.fromList types
      lim     = fromMaybe 10 usq.limit
      off     = fromMaybe 0 usq.offset

      wantMem  = Set.member SearchMemory  typeSet
      wantProj = Set.member SearchProject typeSet
      wantTask = Set.member SearchTask    typeSet

  -- Build per-entity queries
  let memQuery = SearchQuery
        { workspaceId    = usq.workspaceId
        , query          = Just usq.query
        , memoryType     = usq.memoryType
        , tags           = usq.tags
        , minImportance  = usq.minImportance
        , minAccessCount = Nothing
        , sortBy         = Nothing
        , categoryId     = usq.categoryId
        , pinnedOnly     = usq.pinnedOnly
        , searchLanguage = usq.searchLanguage
        , limit          = Just lim
        , offset         = Just off
        }

      projQuery = ProjectListQuery
        { workspaceId    = usq.workspaceId
        , status         = usq.projectStatus
        , query          = Just usq.query
        , searchLanguage = usq.searchLanguage
        , createdAfter   = Nothing
        , createdBefore  = Nothing
        , updatedAfter   = Nothing
        , updatedBefore  = Nothing
        , limit          = Just lim
        , offset         = Just off
        }

      taskQuery = TaskListQuery
        { workspaceId    = usq.workspaceId
        , projectId      = usq.projectId
        , status         = usq.taskStatus
        , priority       = usq.taskPriority
        , query          = Just usq.query
        , searchLanguage = usq.searchLanguage
        , createdAfter   = Nothing
        , createdBefore  = Nothing
        , updatedAfter   = Nothing
        , updatedBefore  = Nothing
        , limit          = Just lim
        , offset         = Just off
        }

  -- Run all three searches in parallel (two concurrently pairs)
  ((mems, projs), tasks) <- concurrently
    (concurrently
      (if wantMem  then Mem.searchMemories pool memQuery   else pure [])
      (if wantProj then Proj.listProjectsWithQuery pool projQuery else pure []))
    (if wantTask then Task.listTasksWithQuery pool taskQuery else pure [])

  -- Enrich projects and tasks with linked memory summaries
  projResults <- mapM (enrichProjectResult pool) projs
  taskResults <- mapM (enrichTaskResult pool) tasks

  pure UnifiedSearchResults
    { memories = mems
    , projects = projResults
    , tasks    = taskResults
    }

-- | Fetch linked memories for a project and build a search result.
enrichProjectResult :: Pool Hasql.Connection -> Project -> IO ProjectSearchResult
enrichProjectResult pool proj = do
  linkedMems <- Mem.getProjectMemories pool proj.id emptyLq
  pure ProjectSearchResult
    { project = proj
    , linkedMemories = map toLinkedSummary linkedMems
    }

-- | Fetch linked memories for a task and build a search result.
enrichTaskResult :: Pool Hasql.Connection -> Task -> IO TaskSearchResult
enrichTaskResult pool task = do
  linkedMems <- Mem.getTaskMemories pool task.id emptyLq
  pure TaskSearchResult
    { task = task
    , linkedMemories = map toLinkedSummary linkedMems
    }

-- | Convert a full Memory to a compact linked summary.
toLinkedSummary :: Memory -> LinkedMemorySummary
toLinkedSummary m = LinkedMemorySummary
  { id         = m.id
  , summary    = m.summary
  , tags       = m.tags
  , importance = m.importance
  }
