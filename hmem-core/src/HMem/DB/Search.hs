module HMem.DB.Search
  ( searchAll
  , searchNextOffset
  , unifiedSearchContinuationError
  ) where

import Control.Concurrent.Async (concurrently)
import Control.Exception (throwIO)
import Control.Monad (forM)
import Data.Maybe (fromMaybe)
import Data.Map.Strict qualified as Map
import Data.Pool (Pool)
import Data.Set qualified as Set
import Data.Text (Text)
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
  case validateUnifiedSearchQuery queryValue of
    [] -> pure ()
    issues -> throwIO $ DBCheckViolation (Text.intercalate "; " issues)
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
          Just workspace | wantsObservations -> Observation.listObservationsSearchOverfetch pool (observationQuery workspace)
          _ -> pure [])
      (if Set.member SearchProject wanted then Proj.listProjectsForSearch pool projectQuery else pure []))
    (if Set.member SearchTask wanted then Task.listTasksForSearch pool taskQuery else pure [])
  let page :: Text -> Bool -> [a] -> Maybe (Text, Bool, Int)
      page key requested rows =
        if requested then Just (key, length rows > limitValue, min limitValue (length rows)) else Nothing
      pages = [ page "observations" wantsObservations observations
              , page "projects" (Set.member SearchProject wanted) projects
              , page "tasks" (Set.member SearchTask wanted) tasks ]
      hasMore = Map.fromList [(key, more) | Just (key, more, _) <- pages]
  nextPairs <- forM [(key, count) | Just (key, True, count) <- pages] $ \(key, count) -> do
    next <- either (throwIO . DBCheckViolation) pure (searchNextOffset offsetValue count)
    pure (key, next)
  let nextOffset = Map.fromList nextPairs
  pure UnifiedSearchResults
    { observations = map compactObservation (take limitValue observations)
    , projects = take limitValue projects
    , tasks = take limitValue tasks
    , hasMore = hasMore, nextOffset = nextOffset }

-- Calculate in an unbounded type before producing a cursor the next call can
-- actually use. A terminal page needs no successor and never calls this.
searchNextOffset :: Int -> Int -> Either Text Int
searchNextOffset offsetValue returnedCount =
  let next = toInteger offsetValue + toInteger returnedCount
  in if next <= toInteger maxUnifiedSearchOffset
       then Right (fromInteger next)
       else Left unifiedSearchContinuationError

unifiedSearchContinuationError :: Text
unifiedSearchContinuationError = "unified search continuation exceeds the supported offset range"

compactObservation :: Observation -> ObservationSearchHit
compactObservation observation = ObservationSearchHit
  { id = observation.id, workspaceId = observation.workspaceId
  , subjects = observation.subjects
  , gitSha = observation.gitSha, contentPreview = Text.take 280 observation.content
  , updatedAt = observation.updatedAt }
