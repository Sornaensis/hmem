module HMem.DB.Search
  ( searchAll
  ) where

import Control.Concurrent.Async (concurrently)
import Control.Exception (throwIO)
import Data.Maybe (fromMaybe)
import Data.Pool (Pool)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Hasql.Connection qualified as Hasql

import HMem.DB.Observation qualified as Observation
import HMem.DB.Pool (DBException(..))
import HMem.DB.Project qualified as Proj
import HMem.DB.Task qualified as Task
import HMem.Types

-- | Searches observations, projects, and tasks without cross-entity
-- enrichment. Observation search is workspace-scoped by contract.
searchAll :: Pool Hasql.Connection -> UnifiedSearchQuery -> IO UnifiedSearchResults
searchAll pool queryValue = do
  let entityTypes = fromMaybe [SearchObservation, SearchProject, SearchTask] queryValue.entityTypes
      wanted = Set.fromList entityTypes
      limitValue = fromMaybe 10 queryValue.limit
      offsetValue = fromMaybe 0 queryValue.offset
      wantsObservations = Set.member SearchObservation wanted
  if queryValue.workspaceId == Nothing
    then throwIO $ DBCheckViolation "workspace_id is required for unified search"
    else pure ()
  let observationQuery workspace = ObservationQuery
        { workspaceId = workspace
        , subjectKind = queryValue.subjectKind
        , subject = queryValue.subject
        , gitSha = queryValue.gitSha
        , query = queryValue.query
        , limit = Just limitValue
        , offset = Just offsetValue
        }
      projectQuery = ProjectListQuery
        { workspaceId = queryValue.workspaceId
        , status = queryValue.projectStatus
        , query = queryValue.query
        , searchLanguage = queryValue.searchLanguage
        , createdAfter = Nothing
        , createdBefore = Nothing
        , updatedAfter = Nothing
        , updatedBefore = Nothing
        , limit = Just limitValue
        , offset = Just offsetValue
        }
      taskQuery = TaskListQuery
        { workspaceId = queryValue.workspaceId
        , projectId = queryValue.projectId
        , status = queryValue.taskStatus
        , priority = queryValue.taskPriority
        , query = queryValue.query
        , searchLanguage = queryValue.searchLanguage
        , createdAfter = Nothing
        , createdBefore = Nothing
        , updatedAfter = Nothing
        , updatedBefore = Nothing
        , limit = Just limitValue
        , offset = Just offsetValue
        }
  ((observations, projects), tasks) <- concurrently
    (concurrently
      (case queryValue.workspaceId of
          Just workspace | wantsObservations -> Observation.listObservations pool (observationQuery workspace)
          _ -> pure [])
      (if Set.member SearchProject wanted then Proj.listProjectsWithQuery pool projectQuery else pure []))
    (if Set.member SearchTask wanted then Task.listTasksWithQuery pool taskQuery else pure [])
  pure UnifiedSearchResults { observations = map compactObservation observations, projects = projects, tasks = tasks }

compactObservation :: Observation -> ObservationSearchHit
compactObservation observation = ObservationSearchHit
  { id = observation.id, workspaceId = observation.workspaceId
  , subjectKind = observation.subjectKind, subject = observation.subject
  , gitSha = observation.gitSha, contentPreview = Text.take 280 observation.content
  , updatedAt = observation.updatedAt }
