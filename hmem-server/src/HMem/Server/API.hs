{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module HMem.Server.API
  ( HMemAPI
  , server
  , CleanupRunReq(..)
  , GroupMemberReq(..)
  , CategoryLink(..)
  ) where

import Control.Exception (throwIO, try)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON (..), ToJSON (..), genericToJSON, genericParseJSON, Value, object, (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Aeson (fromText)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Functor.Contravariant ((>$<))
import Data.Maybe (fromMaybe)
import Data.Pool (Pool, tryWithResource)
import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime, getCurrentTime)
import Data.UUID (UUID)
import Hasql.Connection qualified as Hasql
import Hasql.Session qualified as Session
import GHC.Generics (Generic)
import Rel8 hiding (Delete)
import Rel8 qualified (Delete(..))
import Servant
import System.IO (stderr)

import HMem.DB.Category qualified as Cat
import HMem.DB.Cleanup qualified as Cleanup
import HMem.DB.Memory qualified as Mem
import HMem.DB.Overview qualified as Overview
import HMem.DB.Pool (runSession, runTransaction, DBException(..), PoolMetrics(..), getPoolMetrics)
import HMem.DB.Project qualified as Proj
import HMem.DB.RequestContext (currentRequestId)
import HMem.DB.SavedView qualified as SV
import HMem.DB.Schema
import HMem.DB.Search qualified as Search
import HMem.DB.Task qualified as Task
import HMem.DB.WorkspaceGroup qualified as WG
import HMem.Server.AccessTracker (AccessTracker, trackAccess, bufferSize)
import HMem.Server.Event (Broadcast, ChangeEvent(..), ChangeType(..), EntityType(..))
import HMem.Types

------------------------------------------------------------------------
-- API type
------------------------------------------------------------------------

type HMemAPI = "api" :> "v1" :>
  (    "health"      :> HealthAPI
  :<|> "workspaces"  :> WorkspaceAPI
  :<|> "memories"    :> MemoryAPI
  :<|> "projects"    :> ProjectAPI
  :<|> "tasks"       :> TaskAPI
  :<|> "cleanup"     :> CleanupAPI
  :<|> "categories"  :> CategoryAPI
  :<|> "groups"      :> WorkspaceGroupAPI
  :<|> "activity"    :> ActivityAPI
  :<|> "saved-views" :> SavedViewAPI
  :<|> "search"      :> SearchAPI
  )

-- Health check
type HealthAPI = Get '[JSON] Value

-- Workspaces
type WorkspaceAPI =
       QueryParam "limit" Int :> QueryParam "offset" Int
         :> Get '[JSON] (PaginatedResult Workspace)
  :<|> ReqBody '[JSON] CreateWorkspace :> Post '[JSON] Workspace
  :<|> Capture "workspaceId" UUID :> Get '[JSON] Workspace
  :<|> Capture "workspaceId" UUID :> ReqBody '[JSON] UpdateWorkspace :> Put '[JSON] Workspace
  :<|> Capture "workspaceId" UUID :> Delete '[JSON] NoContent
  :<|> Capture "workspaceId" UUID :> "restore" :> Post '[JSON] NoContent
  :<|> Capture "workspaceId" UUID :> "purge" :> Delete '[JSON] NoContent

-- Memories
type MemoryAPI =
       QueryParam "workspace_id" UUID
         :> QueryParam "type" MemoryType
         :> QueryParam "created_after" UTCTime
         :> QueryParam "created_before" UTCTime
         :> QueryParam "updated_after" UTCTime
         :> QueryParam "updated_before" UTCTime
         :> QueryParam "limit" Int
         :> QueryParam "offset" Int
         :> QueryParam "compact" Bool
         :> Get '[JSON] (PaginatedResult Memory)
  :<|> ReqBody '[JSON] CreateMemory :> Post '[JSON] Memory
  :<|> "batch" :> ReqBody '[JSON] [CreateMemory] :> Post '[JSON] [Memory]
  :<|> "search" :> QueryParam "compact" Bool :> ReqBody '[JSON] SearchQuery :> Post '[JSON] [Memory]
  :<|> "contradictions" :> QueryParam "workspace_id" UUID :> Get '[JSON] [MemoryLink]
  :<|> "by-relation" :> QueryParam "workspace_id" UUID
         :> QueryParam "relation_type" RelationType :> Get '[JSON] [MemoryLink]
  :<|> "workspace-links" :> QueryParam "workspace_id" UUID :> Get '[JSON] [MemoryLink]
  :<|> Capture "memoryId" UUID :> Get '[JSON] Memory
  :<|> Capture "memoryId" UUID :> ReqBody '[JSON] UpdateMemory :> Put '[JSON] Memory
  :<|> Capture "memoryId" UUID :> Delete '[JSON] NoContent
  :<|> Capture "memoryId" UUID :> "restore" :> Post '[JSON] NoContent
  :<|> Capture "memoryId" UUID :> "purge" :> Delete '[JSON] NoContent
  :<|> Capture "memoryId" UUID :> "links" :> Get '[JSON] [MemoryLink]
  :<|> Capture "memoryId" UUID :> "links" :> ReqBody '[JSON] CreateMemoryLink
         :> Post '[JSON] NoContent
  :<|> Capture "memoryId" UUID :> "links" :> Capture "targetId" UUID
         :> Capture "relationType" RelationType :> Delete '[JSON] NoContent
  :<|> Capture "memoryId" UUID :> "tags" :> Get '[JSON] [Text]
  :<|> Capture "memoryId" UUID :> "tags" :> ReqBody '[JSON] [Text]
         :> Put '[JSON] NoContent
  :<|> Capture "memoryId" UUID :> "graph" :> QueryParam "depth" Int
         :> Get '[JSON] MemoryGraph
  :<|> Capture "memoryId" UUID :> "importance" :> ReqBody '[JSON] AdjustImportance
         :> Put '[JSON] Memory
  :<|> Capture "memoryId" UUID :> "pin" :> Post '[JSON] Memory
  :<|> Capture "memoryId" UUID :> "unpin" :> Post '[JSON] Memory
  :<|> "similar" :> ReqBody '[JSON] SimilarQuery :> Post '[JSON] [SimilarMemory]
  :<|> Capture "memoryId" UUID :> "embedding" :> ReqBody '[JSON] [Double]
         :> Put '[JSON] NoContent
  :<|> "batch-delete" :> ReqBody '[JSON] BatchDeleteRequest :> Post '[JSON] BatchResult
  :<|> "batch-set-tags" :> ReqBody '[JSON] BatchSetTagsRequest :> Post '[JSON] BatchResult
  :<|> "batch-update" :> ReqBody '[JSON] BatchUpdateMemoryRequest :> Post '[JSON] BatchResult

-- Projects
type ProjectAPI =
       QueryParam "workspace_id" UUID
         :> QueryParam "status" ProjectStatus
         :> QueryParam "query" Text
         :> QueryParam "search_language" Text
         :> QueryParam "created_after" UTCTime
         :> QueryParam "created_before" UTCTime
         :> QueryParam "updated_after" UTCTime
         :> QueryParam "updated_before" UTCTime
         :> QueryParam "limit" Int
         :> QueryParam "offset" Int
         :> Get '[JSON] (PaginatedResult Project)
  :<|> ReqBody '[JSON] CreateProject :> Post '[JSON] Project
  :<|> Capture "projectId" UUID :> Get '[JSON] Project
  :<|> Capture "projectId" UUID :> ReqBody '[JSON] UpdateProject :> Put '[JSON] Project
  :<|> Capture "projectId" UUID :> Delete '[JSON] NoContent
    :<|> Capture "projectId" UUID :> "restore" :> Post '[JSON] NoContent
  :<|> Capture "projectId" UUID :> "purge" :> Delete '[JSON] NoContent
  :<|> Capture "projectId" UUID :> "memories" :> ReqBody '[JSON] LinkMemory
         :> Post '[JSON] NoContent
  :<|> Capture "projectId" UUID :> "memories" :> Capture "memoryId" UUID
         :> Delete '[JSON] NoContent
  :<|> Capture "projectId" UUID :> "memories" :> Get '[JSON] [Memory]
  :<|> Capture "projectId" UUID :> "memories" :> "batch"
         :> ReqBody '[JSON] BatchMemoryLinkRequest :> Post '[JSON] BatchResult
    :<|> "batch-delete" :> ReqBody '[JSON] BatchDeleteRequest :> Post '[JSON] BatchResult
    :<|> "batch-update" :> ReqBody '[JSON] BatchUpdateProjectRequest :> Post '[JSON] BatchResult
  :<|> Capture "projectId" UUID :> "overview" :> Get '[JSON] ProjectOverview
type TaskAPI =
       QueryParam "workspace_id" UUID
         :> QueryParam "project_id" UUID
         :> QueryParam "status" TaskStatus
         :> QueryParam "priority" Int
         :> QueryParam "query" Text
         :> QueryParam "search_language" Text
         :> QueryParam "created_after" UTCTime
         :> QueryParam "created_before" UTCTime
         :> QueryParam "updated_after" UTCTime
         :> QueryParam "updated_before" UTCTime
         :> QueryParam "limit" Int
         :> QueryParam "offset" Int
         :> Get '[JSON] (PaginatedResult Task)
  :<|> ReqBody '[JSON] CreateTask :> Post '[JSON] Task
  :<|> Capture "taskId" UUID :> Get '[JSON] Task
  :<|> Capture "taskId" UUID :> ReqBody '[JSON] UpdateTask :> Put '[JSON] Task
  :<|> Capture "taskId" UUID :> Delete '[JSON] NoContent
  :<|> Capture "taskId" UUID :> "restore" :> Post '[JSON] NoContent
  :<|> Capture "taskId" UUID :> "purge" :> Delete '[JSON] NoContent
  :<|> Capture "taskId" UUID :> "memories" :> ReqBody '[JSON] LinkMemory
         :> Post '[JSON] NoContent
  :<|> Capture "taskId" UUID :> "memories" :> Capture "memoryId" UUID
         :> Delete '[JSON] NoContent
  :<|> Capture "taskId" UUID :> "dependencies" :> ReqBody '[JSON] LinkDependency
         :> Post '[JSON] NoContent
  :<|> Capture "taskId" UUID :> "dependencies" :> Capture "dependsOnId" UUID
         :> Delete '[JSON] NoContent
  :<|> Capture "taskId" UUID :> "memories" :> Get '[JSON] [Memory]
    :<|> Capture "taskId" UUID :> "overview"
      :> QueryParam "extra_context" Bool :> Get '[JSON] TaskOverview
  :<|> Capture "taskId" UUID :> "context"
      :> QueryParam "detail_level" ContextDetailLevel :> Get '[JSON] ContextInfo
  :<|> "batch-delete" :> ReqBody '[JSON] BatchDeleteRequest :> Post '[JSON] BatchResult
  :<|> "batch-move" :> ReqBody '[JSON] BatchMoveTasksRequest :> Post '[JSON] BatchResult
  :<|> "batch-update" :> ReqBody '[JSON] BatchUpdateTaskRequest :> Post '[JSON] BatchResult
  :<|> Capture "taskId" UUID :> "memories" :> "batch"
         :> ReqBody '[JSON] BatchMemoryLinkRequest :> Post '[JSON] BatchResult
type CleanupAPI =
       "run" :> ReqBody '[JSON] CleanupRunReq :> Post '[JSON] CleanupResult
  :<|> "policies" :> QueryParam "workspace_id" UUID
         :> QueryParam "limit" Int :> QueryParam "offset" Int
         :> Get '[JSON] (PaginatedResult CleanupPolicy)
  :<|> "policies" :> ReqBody '[JSON] UpsertCleanupPolicy :> Post '[JSON] CleanupPolicy

-- Small request body for cleanup/run
newtype CleanupRunReq = CleanupRunReq { workspaceId :: UUID }
  deriving (Show, Eq, Generic)

instance ToJSON CleanupRunReq where
  toJSON = genericToJSON jsonOptions
instance FromJSON CleanupRunReq where
  parseJSON = genericParseJSON jsonOptions

-- Categories
type CategoryAPI =
       QueryParam "workspace_id" UUID
         :> QueryParam "limit" Int :> QueryParam "offset" Int
         :> Get '[JSON] (PaginatedResult MemoryCategory)
  :<|> "global" :> QueryParam "limit" Int :> QueryParam "offset" Int
         :> Get '[JSON] (PaginatedResult MemoryCategory)
  :<|> ReqBody '[JSON] CreateMemoryCategory :> Post '[JSON] MemoryCategory
  :<|> Capture "categoryId" UUID :> Get '[JSON] MemoryCategory
  :<|> Capture "categoryId" UUID :> ReqBody '[JSON] UpdateMemoryCategory
         :> Put '[JSON] MemoryCategory
  :<|> Capture "categoryId" UUID :> Delete '[JSON] NoContent
    :<|> Capture "categoryId" UUID :> "restore" :> Post '[JSON] NoContent
    :<|> Capture "categoryId" UUID :> "purge" :> Delete '[JSON] NoContent
    :<|> Capture "categoryId" UUID :> "memories" :> Get '[JSON] [Memory]
    :<|> Capture "categoryId" UUID :> "memories" :> "batch"
      :> ReqBody '[JSON] BatchMemoryLinkRequest :> Post '[JSON] BatchResult
    :<|> "batch-delete" :> ReqBody '[JSON] BatchDeleteRequest :> Post '[JSON] BatchResult
  :<|> "link" :> ReqBody '[JSON] CategoryLink :> Post '[JSON] NoContent
  :<|> "unlink" :> ReqBody '[JSON] CategoryLink :> Post '[JSON] NoContent

-- Workspace Groups
type WorkspaceGroupAPI =
       QueryParam "limit" Int :> QueryParam "offset" Int
         :> Get '[JSON] (PaginatedResult WorkspaceGroup)
  :<|> ReqBody '[JSON] CreateWorkspaceGroup :> Post '[JSON] WorkspaceGroup
  :<|> Capture "groupId" UUID :> Get '[JSON] WorkspaceGroup
  :<|> Capture "groupId" UUID :> Delete '[JSON] NoContent
  :<|> Capture "groupId" UUID :> "members" :> ReqBody '[JSON] GroupMemberReq
         :> Post '[JSON] NoContent
  :<|> Capture "groupId" UUID :> "members" :> Capture "workspaceId" UUID
         :> Delete '[JSON] NoContent
  :<|> Capture "groupId" UUID :> "members" :> Get '[JSON] [UUID]

-- Activity timeline
type ActivityAPI =
       QueryParam "workspace_id" UUID
         :> QueryParam "entity_type" Text
         :> QueryParam "limit" Int
         :> Get '[JSON] (PaginatedResult ActivityEvent)

-- Saved views
type SavedViewAPI =
       QueryParam "workspace_id" UUID
         :> QueryParam "limit" Int
         :> QueryParam "offset" Int
         :> Get '[JSON] (PaginatedResult SavedView)
  :<|> ReqBody '[JSON] CreateSavedView :> Post '[JSON] SavedView
  :<|> Capture "viewId" UUID :> Get '[JSON] SavedView
  :<|> Capture "viewId" UUID :> ReqBody '[JSON] UpdateSavedView :> Put '[JSON] SavedView
  :<|> Capture "viewId" UUID :> Delete '[JSON] NoContent
  :<|> Capture "viewId" UUID :> "restore" :> Post '[JSON] NoContent
  :<|> Capture "viewId" UUID :> "purge" :> Delete '[JSON] NoContent
  :<|> Capture "viewId" UUID :> "execute"
         :> QueryParam "limit" Int :> QueryParam "offset" Int :> QueryParam "detail" Bool
         :> Post '[JSON] Value

-- Unified search
type SearchAPI =
  ReqBody '[JSON] UnifiedSearchQuery :> Post '[JSON] UnifiedSearchResults

-- Request body for group member operations
newtype GroupMemberReq = GroupMemberReq { workspaceId :: UUID }
  deriving (Show, Eq, Generic)

instance ToJSON GroupMemberReq where
  toJSON = genericToJSON jsonOptions
instance FromJSON GroupMemberReq where
  parseJSON = genericParseJSON jsonOptions

-- Request body for category ↔ memory linking
data CategoryLink = CategoryLink
  { memoryId   :: UUID
  , categoryId :: UUID
  } deriving (Show, Eq, Generic)

instance ToJSON CategoryLink where
  toJSON = genericToJSON jsonOptions
instance FromJSON CategoryLink where
  parseJSON = genericParseJSON jsonOptions

------------------------------------------------------------------------
-- Servant FromHttpApiData instances for query params
------------------------------------------------------------------------

instance FromHttpApiData MemoryType where
  parseQueryParam "short_term" = Right ShortTerm
  parseQueryParam "long_term"  = Right LongTerm
  parseQueryParam t            = Left ("Invalid memory type: " <> t)

instance FromHttpApiData ProjectStatus where
  parseQueryParam "active"    = Right ProjActive
  parseQueryParam "paused"    = Right ProjPaused
  parseQueryParam "completed" = Right ProjCompleted
  parseQueryParam "archived"  = Right ProjArchived
  parseQueryParam t           = Left ("Invalid project status: " <> t)

instance FromHttpApiData TaskStatus where
  parseQueryParam "todo"        = Right Todo
  parseQueryParam "in_progress" = Right InProgress
  parseQueryParam "blocked"     = Right Blocked
  parseQueryParam "done"        = Right Done
  parseQueryParam "cancelled"   = Right Cancelled
  parseQueryParam t             = Left ("Invalid task status: " <> t)

instance FromHttpApiData RelationType where
  parseQueryParam "related"        = Right Related
  parseQueryParam "supersedes"     = Right Supersedes
  parseQueryParam "contradicts"    = Right Contradicts
  parseQueryParam "elaborates"     = Right Elaborates
  parseQueryParam "inspires"       = Right Inspires
  parseQueryParam "depends_on"     = Right DependsOn
  parseQueryParam "derived_from"   = Right DerivedFrom
  parseQueryParam "alternative_to" = Right AlternativeTo
  parseQueryParam t                = Left ("Invalid relation type: " <> t)

instance FromHttpApiData WorkspaceType where
  parseQueryParam "repository"   = Right WsRepository
  parseQueryParam "planning"     = Right WsPlanning
  parseQueryParam "personal"     = Right WsPersonal
  parseQueryParam "organization" = Right WsOrganization
  parseQueryParam t              = Left ("Invalid workspace type: " <> t)

instance FromHttpApiData ContextDetailLevel where
  parseQueryParam "light"  = Right ContextLight
  parseQueryParam "medium" = Right ContextMedium
  parseQueryParam "heavy"  = Right ContextHeavy
  parseQueryParam t        = Left ("Invalid detail level: " <> t <> " (expected light, medium, or heavy)")

------------------------------------------------------------------------
-- Structured error handling
------------------------------------------------------------------------

-- | Run a database action, catching 'DBException' and mapping to
-- appropriate HTTP status codes with structured JSON error bodies.
handleDBErrors :: IO a -> Handler a
handleDBErrors io = do
  result <- liftIO (try io)
  case result of
    Right a -> pure a
    Left dbErr -> do
      liftIO $ logDatabaseError dbErr
      throwError (dbExceptionToServerError dbErr)
  where
    errorBody :: Text -> Text -> Value
    errorBody errType msg = object
      [ "error"   .= errType
      , "message" .= msg
      ]

    dbExceptionToServerError :: DBException -> ServerError
    dbExceptionToServerError = \case
      DBUniqueViolation _ -> err409
        { errBody = Aeson.encode $ errorBody "conflict" "Resource already exists" }
      DBForeignKeyViolation _ -> err400
        { errBody = Aeson.encode $ errorBody "invalid_reference" "Referenced resource does not exist" }
      DBCheckViolation _ -> err400
        { errBody = Aeson.encode $ errorBody "invalid_request" "Request violates a data constraint" }
      DBCycleDetected _ -> err409
        { errBody = Aeson.encode $ errorBody "cycle" "Operation would create a cycle" }
      DBStatementTimeout -> err504
        { errBody = Aeson.encode $ errorBody "timeout" "Database request timed out" }
      DBOtherError _ -> err500
        { errBody = Aeson.encode $ errorBody "internal" "Internal database error" }

logDatabaseError :: DBException -> IO ()
logDatabaseError dbErr = do
  requestId <- currentRequestId
  LBS8.hPutStrLn stderr $ Aeson.encode $ object $
    [ "level" .= ("error" :: Text)
    , "event" .= ("db_error" :: Text)
    , "error" .= dbErrorLabel dbErr
    ]
    ++ [ "request_id" .= rid | Just rid <- [requestId] ]
    ++ [ "detail" .= detail | Just detail <- [dbErrorDetail dbErr] ]

dbErrorLabel :: DBException -> Text
dbErrorLabel = \case
  DBUniqueViolation _ -> "unique_violation"
  DBForeignKeyViolation _ -> "foreign_key_violation"
  DBCheckViolation _ -> "check_violation"
  DBCycleDetected _ -> "cycle_detected"
  DBStatementTimeout -> "statement_timeout"
  DBOtherError _ -> "other_error"

dbErrorDetail :: DBException -> Maybe Text
dbErrorDetail = \case
  DBUniqueViolation detail -> Just detail
  DBForeignKeyViolation detail -> Just detail
  DBCheckViolation detail -> Just detail
  DBCycleDetected detail -> Just detail
  DBStatementTimeout -> Nothing
  DBOtherError detail -> Just detail

purgeConflict :: ServerError
purgeConflict = err409
  { errBody = Aeson.encode $ object
      [ "error" .= ("conflict" :: Text)
      , "message" .= ("Resource must be soft-deleted before purge" :: Text)
      ]
  }

------------------------------------------------------------------------
-- Server implementation
------------------------------------------------------------------------

server :: Pool Hasql.Connection -> AccessTracker -> Broadcast -> Bool -> Server HMemAPI
server pool tracker bc pgvec =
       healthHandler pool tracker
  :<|> workspaceHandlers pool bc
  :<|> memoryHandlers pool tracker bc pgvec
  :<|> projectHandlers pool bc
  :<|> taskHandlers pool bc
  :<|> cleanupHandlers pool bc
  :<|> categoryHandlers pool bc
  :<|> workspaceGroupHandlers pool bc
  :<|> activityHandlers pool
  :<|> savedViewHandlers pool bc
  :<|> searchHandler pool

-- | Emit a change event to all connected WebSocket clients.
emit :: Broadcast -> ChangeType -> EntityType -> UUID -> Maybe Value -> Handler ()
emit bc ct et eid mpayload = liftIO $ do
  now <- getCurrentTime
  bc ChangeEvent { changeType = ct, entityType = et, entityId = eid, timestamp = now, payload = mpayload }

-- | Emit one event per ID in a list. Used by batch handlers.
emitMany :: Broadcast -> ChangeType -> EntityType -> [UUID] -> Handler ()
emitMany bc ct et ids = mapM_ (\eid -> emit bc ct et eid Nothing) ids

-- Health handler ---------------------------------------------------

healthHandler :: Pool Hasql.Connection -> AccessTracker -> Server HealthAPI
healthHandler pool tracker = do
  -- Non-blocking pool check: try to acquire a connection and run SELECT 1
  mResult <- liftIO $ tryWithResource pool $ \conn ->
    Session.run (Session.sql "SELECT 1") conn
  bufSz <- liftIO $ bufferSize tracker
  metrics <- liftIO getPoolMetrics
  let (dbStatus, overallStatus) = case mResult of
        Just (Right _) -> ("connected" :: Text, "ok" :: Text)
        Just (Left _)  -> ("error",             "degraded")
        Nothing        -> ("pool_exhausted",    "degraded")
      utilPct | metrics.maxConnections > 0 =
                metrics.activeConnections * 100 `div` metrics.maxConnections
              | otherwise = 0
  pure $ object
    [ "status"  .= overallStatus
    , "version" .= ("0.1.0.0" :: Text)
    , "database" .= object
        [ "status" .= dbStatus
        ]
    , "pool" .= object
        [ "active_connections" .= metrics.activeConnections
        , "max_connections"    .= metrics.maxConnections
        , "idle_connections"   .= Prelude.max 0 (metrics.maxConnections - metrics.activeConnections)
        , "utilization_pct"    .= utilPct
        ]
    , "access_tracker" .= object
        [ "buffered_count" .= bufSz
        ]
    ]

-- Workspace helpers ------------------------------------------------

rowToWorkspace :: WorkspaceT Result -> Workspace
rowToWorkspace r = Workspace
  { id            = r.wsId
  , name          = r.wsName
  , ghOwner       = r.wsGhOwner
  , ghRepo        = r.wsGhRepo
  , workspaceType = r.wsType
  , createdAt     = r.wsCreatedAt
  , updatedAt     = r.wsUpdatedAt
  }

workspaceHandlers :: Pool Hasql.Connection -> Broadcast -> Server WorkspaceAPI
workspaceHandlers pool bc =
       listWorkspacesH
  :<|> createWorkspaceH
  :<|> getWorkspaceH
  :<|> updateWorkspaceH
  :<|> deleteWorkspaceH
  :<|> restoreWorkspaceH
  :<|> purgeWorkspaceH
  where
    listWorkspacesH :: Maybe Int -> Maybe Int -> Handler (PaginatedResult Workspace)
    listWorkspacesH mlimit moffset = handleDBErrors $ do
      let lim = capLimit mlimit
          off = capOffset moffset
      rows <- runSession pool $ Session.statement () $ run $ select $
        limit (fromIntegral lim + 1) $ offset (fromIntegral off) $
        orderBy ((\row -> row.wsName) >$< asc) $ do
          row <- each workspaceSchema
          where_ $ activeWorkspace row
          pure row
      let results = map rowToWorkspace rows
      pure PaginatedResult { items = take lim results, hasMore = length results > lim }

    createWorkspaceH :: CreateWorkspace -> Handler Workspace
    createWorkspaceH cw = do
      rejectValidationErrors (validateCreateWorkspaceInput cw)
      ws <- handleDBErrors $ do
        rows <- runSession pool $ Session.statement () $ run $
          insert Insert
            { into = workspaceSchema
            , rows = values
                [ WorkspaceT
                    { wsId        = unsafeDefault
                    , wsName      = lit cw.name
                    , wsGhOwner   = lit cw.ghOwner
                    , wsGhRepo    = lit cw.ghRepo
                    , wsType      = lit (fromMaybe WsRepository cw.workspaceType)
                    , wsDeletedAt = unsafeDefault
                    , wsCreatedAt = unsafeDefault
                    , wsUpdatedAt = unsafeDefault
                    }
                ]
            , onConflict = Abort
            , returning  = Returning id
            }
        case rows of
          (r:_) -> pure $ rowToWorkspace r
          []    -> throwIO $ DBOtherError "Insert failed"
      emit bc Created ETWorkspace ws.id (Just $ toJSON ws)
      pure ws

    getWorkspaceH :: UUID -> Handler Workspace
    getWorkspaceH wsId = do
      rows <- handleDBErrors $ runSession pool $ Session.statement () $ run $ select $ do
        row <- each workspaceSchema
        where_ $ row.wsId ==. lit wsId
        where_ $ activeWorkspace row
        pure row
      case rows of
        (r:_) -> pure $ rowToWorkspace r
        []    -> throwError err404

    updateWorkspaceH :: UUID -> UpdateWorkspace -> Handler Workspace
    updateWorkspaceH wsId uw = do
      rejectValidationErrors (validateUpdateWorkspaceInput uw)
      rows <- handleDBErrors $ runSession pool $ Session.statement () $ run $
        update Update
          { target = workspaceSchema
          , from = pure ()
          , set = \_ row -> row
              { wsName    = maybe row.wsName    lit uw.name
              , wsType    = maybe row.wsType    lit uw.workspaceType
              , wsGhOwner = applyNullableUpdate row.wsGhOwner uw.ghOwner
              , wsGhRepo  = applyNullableUpdate row.wsGhRepo uw.ghRepo
              }
          , updateWhere = \_ row -> row.wsId ==. lit wsId &&. activeWorkspace row
          , returning = Returning id
          }
      case rows of
        (r:_) -> do
          emit bc Updated ETWorkspace wsId (Just $ toJSON $ rowToWorkspace r)
          pure $ rowToWorkspace r
        []    -> throwError err404

    deleteWorkspaceH :: UUID -> Handler NoContent
    deleteWorkspaceH wsId = do
      ok <- handleDBErrors $ runTransaction pool $ do
        n <- Session.statement () $ runN $
          update Update
            { target = workspaceSchema
            , from = pure ()
            , set = \_ row -> row { wsDeletedAt = deletedNow }
            , updateWhere = \_ row -> row.wsId ==. lit wsId &&. activeWorkspace row
            , returning = NoReturning
            }
        if n == 0
          then pure False
          else do
            memIds <- Session.statement () $ run $ select $ do
              row <- each memorySchema
              where_ $ row.memWorkspaceId ==. lit wsId
              where_ $ activeMemory row
              pure row.memId
            projIds <- Session.statement () $ run $ select $ do
              row <- each projectSchema
              where_ $ row.projWorkspaceId ==. lit wsId
              where_ $ activeProject row
              pure row.projId
            taskIds <- Session.statement () $ run $ select $ do
              row <- each taskSchema
              where_ $ row.taskWorkspaceId ==. lit wsId
              where_ $ activeTask row
              pure row.taskId
            catIds <- Session.statement () $ run $ select $ do
              row <- each memoryCategorySchema
              where_ $ row.mcWorkspaceId ==. lit (Just wsId)
              where_ $ activeCategory row
              pure row.mcId

            case memIds of
              [] -> pure ()
              _ -> do
                Session.statement () $ run_ $
                  delete Rel8.Delete
                    { from = memoryLinkSchema
                    , using = pure ()
                    , deleteWhere = \_ row -> in_ row.mlSourceId (map lit memIds) ||. in_ row.mlTargetId (map lit memIds)
                    , returning = NoReturning
                    }
                Session.statement () $ run_ $
                  delete Rel8.Delete
                    { from = memoryTagSchema
                    , using = pure ()
                    , deleteWhere = \_ row -> in_ row.mtMemoryId (map lit memIds)
                    , returning = NoReturning
                    }

            case memIds of
              [] -> pure ()
              _ -> Session.statement () $ run_ $
                delete Rel8.Delete
                  { from = memoryCategoryLinkSchema
                  , using = pure ()
                  , deleteWhere = \_ row -> in_ row.mclMemoryId (map lit memIds)
                  , returning = NoReturning
                  }

            case catIds of
              [] -> pure ()
              _ -> Session.statement () $ run_ $
                delete Rel8.Delete
                  { from = memoryCategoryLinkSchema
                  , using = pure ()
                  , deleteWhere = \_ row -> in_ row.mclCategoryId (map lit catIds)
                  , returning = NoReturning
                  }

            case projIds of
              [] -> pure ()
              _ -> Session.statement () $ run_ $
                delete Rel8.Delete
                  { from = projectMemoryLinkSchema
                  , using = pure ()
                  , deleteWhere = \_ row -> in_ row.pmlProjectId (map lit projIds)
                  , returning = NoReturning
                  }

            case memIds of
              [] -> pure ()
              _ -> Session.statement () $ run_ $
                delete Rel8.Delete
                  { from = projectMemoryLinkSchema
                  , using = pure ()
                  , deleteWhere = \_ row -> in_ row.pmlMemoryId (map lit memIds)
                  , returning = NoReturning
                  }

            case taskIds of
              [] -> pure ()
              _ -> do
                Session.statement () $ run_ $
                  delete Rel8.Delete
                    { from = taskMemoryLinkSchema
                    , using = pure ()
                    , deleteWhere = \_ row -> in_ row.tmlTaskId (map lit taskIds)
                    , returning = NoReturning
                    }
                Session.statement () $ run_ $
                  delete Rel8.Delete
                    { from = taskDependencySchema
                    , using = pure ()
                    , deleteWhere = \_ row -> in_ row.tdTaskId (map lit taskIds) ||. in_ row.tdDependsOnId (map lit taskIds)
                    , returning = NoReturning
                    }

            case memIds of
              [] -> pure ()
              _ -> Session.statement () $ run_ $
                delete Rel8.Delete
                  { from = taskMemoryLinkSchema
                  , using = pure ()
                  , deleteWhere = \_ row -> in_ row.tmlMemoryId (map lit memIds)
                  , returning = NoReturning
                  }

            Session.statement () $ run_ $
              update Update
                { target = memorySchema
                , from = pure ()
                , set = \_ row -> row { memDeletedAt = deletedNow }
                , updateWhere = \_ row -> row.memWorkspaceId ==. lit wsId &&. activeMemory row
                , returning = NoReturning
                }
            Session.statement () $ run_ $
              update Update
                { target = projectSchema
                , from = pure ()
                , set = \_ row -> row { projDeletedAt = deletedNow }
                , updateWhere = \_ row -> row.projWorkspaceId ==. lit wsId &&. activeProject row
                , returning = NoReturning
                }
            Session.statement () $ run_ $
              update Update
                { target = taskSchema
                , from = pure ()
                , set = \_ row -> row { taskDeletedAt = deletedNow }
                , updateWhere = \_ row -> row.taskWorkspaceId ==. lit wsId &&. activeTask row
                , returning = NoReturning
                }
            Session.statement () $ run_ $
              update Update
                { target = memoryCategorySchema
                , from = pure ()
                , set = \_ row -> row { mcDeletedAt = deletedNow }
                , updateWhere = \_ row -> row.mcWorkspaceId ==. lit (Just wsId) &&. activeCategory row
                , returning = NoReturning
                }
            Session.statement () $ run_ $
              update Update
                { target = savedViewSchema
                , from = pure ()
                , set = \_ row -> row { svDeletedAt = deletedNow }
                , updateWhere = \_ row -> row.svWorkspaceId ==. lit wsId &&. activeSavedView row
                , returning = NoReturning
                }
            Session.statement () $ run_ $
              delete Rel8.Delete
                { from = cleanupPolicySchema
                , using = pure ()
                , deleteWhere = \_ row -> row.cpWorkspaceId ==. lit wsId
                , returning = NoReturning
                }
            Session.statement () $ run_ $
              delete Rel8.Delete
                { from = workspaceGroupMemberSchema
                , using = pure ()
                , deleteWhere = \_ row -> row.wgmWorkspaceId ==. lit wsId
                , returning = NoReturning
                }
            pure True
      if ok then do emit bc Deleted ETWorkspace wsId Nothing; pure NoContent else throwError err404

    restoreWorkspaceH :: UUID -> Handler NoContent
    restoreWorkspaceH wsId = do
      ok <- handleDBErrors $ runTransaction pool $ do
        rows <- Session.statement () $ run $ select $ do
          row <- each workspaceSchema
          where_ $ row.wsId ==. lit wsId
          pure row
        case rows of
          [] -> pure False
          (row:_)
            | Just deletedAt <- row.wsDeletedAt -> do
                Session.statement () $ run_ $
                  update Update
                    { target = workspaceSchema
                    , from = pure ()
                    , set = \_ workspace -> workspace { wsDeletedAt = lit (Nothing :: Maybe UTCTime) }
                    , updateWhere = \_ workspace -> workspace.wsId ==. lit wsId &&. workspace.wsDeletedAt ==. lit (Just deletedAt)
                    , returning = NoReturning
                    }
                Session.statement () $ run_ $
                  update Update
                    { target = memorySchema
                    , from = pure ()
                    , set = \_ memory -> memory { memDeletedAt = lit (Nothing :: Maybe UTCTime) }
                    , updateWhere = \_ memory -> memory.memWorkspaceId ==. lit wsId &&. memory.memDeletedAt ==. lit (Just deletedAt)
                    , returning = NoReturning
                    }
                Session.statement () $ run_ $
                  update Update
                    { target = projectSchema
                    , from = pure ()
                    , set = \_ project -> project { projDeletedAt = lit (Nothing :: Maybe UTCTime) }
                    , updateWhere = \_ project -> project.projWorkspaceId ==. lit wsId &&. project.projDeletedAt ==. lit (Just deletedAt)
                    , returning = NoReturning
                    }
                Session.statement () $ run_ $
                  update Update
                    { target = taskSchema
                    , from = pure ()
                    , set = \_ task -> task { taskDeletedAt = lit (Nothing :: Maybe UTCTime) }
                    , updateWhere = \_ task -> task.taskWorkspaceId ==. lit wsId &&. task.taskDeletedAt ==. lit (Just deletedAt)
                    , returning = NoReturning
                    }
                Session.statement () $ run_ $
                  update Update
                    { target = memoryCategorySchema
                    , from = pure ()
                    , set = \_ category -> category { mcDeletedAt = lit (Nothing :: Maybe UTCTime) }
                    , updateWhere = \_ category -> category.mcWorkspaceId ==. lit (Just wsId) &&. category.mcDeletedAt ==. lit (Just deletedAt)
                    , returning = NoReturning
                    }
                Session.statement () $ run_ $
                  update Update
                    { target = savedViewSchema
                    , from = pure ()
                    , set = \_ view -> view { svDeletedAt = lit (Nothing :: Maybe UTCTime) }
                    , updateWhere = \_ view -> view.svWorkspaceId ==. lit wsId &&. view.svDeletedAt ==. lit (Just deletedAt)
                    , returning = NoReturning
                    }
                pure True
            | otherwise -> pure False
      if ok then do emit bc Updated ETWorkspace wsId Nothing; pure NoContent else throwError err404

    purgeWorkspaceH :: UUID -> Handler NoContent
    purgeWorkspaceH wsId = do
      rows <- handleDBErrors $ runSession pool $ Session.statement () $ run $ select $ do
        row <- each workspaceSchema
        where_ $ row.wsId ==. lit wsId
        pure row
      case rows of
        [] -> throwError err404
        (r:_)
          | r.wsDeletedAt == Nothing -> throwError purgeConflict
          | otherwise -> do
              handleDBErrors $ runSession pool $ Session.statement () $ run_ $
                delete Rel8.Delete
                  { from = savedViewSchema
                  , using = pure ()
                  , deleteWhere = \_ row -> row.svWorkspaceId ==. lit wsId
                  , returning = NoReturning
                  }
              handleDBErrors $ runSession pool $ Session.statement () $ run_ $
                delete Rel8.Delete
                  { from = workspaceSchema
                  , using = pure ()
                  , deleteWhere = \_ row -> row.wsId ==. lit wsId
                  , returning = NoReturning
                  }
              pure NoContent

-- Memory handlers --------------------------------------------------

memoryHandlers :: Pool Hasql.Connection -> AccessTracker -> Broadcast -> Bool -> Server MemoryAPI
memoryHandlers pool tracker bc pgvec =
       listMemoriesH
  :<|> createMemoryH
  :<|> createMemoryBatchH
  :<|> searchMemoriesH
  :<|> contradictionsH
  :<|> byRelationH
  :<|> workspaceLinksH
  :<|> getMemoryH
  :<|> updateMemoryH
  :<|> deleteMemoryH
  :<|> restoreMemoryH
  :<|> purgeMemoryH
  :<|> getLinksH
  :<|> createLinkH
  :<|> unlinkH
  :<|> getTagsH
  :<|> setTagsH
  :<|> graphH
  :<|> adjustImportanceH
  :<|> pinH
  :<|> unpinH
  :<|> similarH
  :<|> setEmbeddingH
  :<|> batchDeleteH
  :<|> batchSetTagsH
  :<|> batchUpdateH
  where
    requireMemoryH mid = handleDBErrors (Mem.getMemory pool mid) >>= maybe (throwError err404) pure

    listMemoriesH mws mtype mcreatedAfter mcreatedBefore mupdatedAfter mupdatedBefore mlimit moffset mcompact = do
      let lim = capLimit mlimit
          off = capOffset moffset
          compact = mcompact == Just True
          query = MemoryListQuery
            { workspaceId = mws
            , memoryType = mtype
            , createdAfter = mcreatedAfter
            , createdBefore = mcreatedBefore
            , updatedAfter = mupdatedAfter
            , updatedBefore = mupdatedBefore
            , limit = Just (lim + 1)
            , offset = Just off
            }
      rejectValidationErrors (validateMemoryListQuery query)
      results <- handleDBErrors $ Mem.listMemoriesWithQuery pool query
      let items = (if compact then map compactMemory else Prelude.id) $ take lim results
      pure PaginatedResult { items = items, hasMore = length results > lim }

    createMemoryH cm = do
      rejectValidationErrors (validateCreateMemoryInput cm)
      mem <- handleDBErrors $ Mem.createMemory pool cm
      emit bc Created ETMemory mem.id (Just $ toJSON mem)
      pure mem

    createMemoryBatchH cms
      | otherwise = do
          rejectValidationErrors (validateCreateMemoryBatchInput cms)
          handleDBErrors $ Mem.createMemoryBatch pool cms

    searchMemoriesH mcompact sq = do
      let compact = mcompact == Just True
      results <- handleDBErrors $ Mem.searchMemories pool sq
      pure $ if compact then map compactMemory results else results

    contradictionsH mws = do
      wsId <- requireParam "workspace_id" mws
      handleDBErrors $ Mem.findByRelation pool wsId Contradicts

    byRelationH mws mrt = do
      wsId <- requireParam "workspace_id" mws
      rt   <- requireParam "relation_type" mrt
      handleDBErrors $ Mem.findByRelation pool wsId rt

    workspaceLinksH mws = do
      wsId <- requireParam "workspace_id" mws
      handleDBErrors $ Mem.findLinksForWorkspace pool wsId

    getMemoryH mid = do
      mem <- requireMemoryH mid
      liftIO $ trackAccess tracker mid
      pure mem

    updateMemoryH mid um = do
      rejectValidationErrors (validateUpdateMemoryInput um)
      mm <- handleDBErrors $ Mem.updateMemory pool mid um
      case mm of
        Nothing -> throwError err404
        Just mem -> do
          emit bc Updated ETMemory mid (Just $ toJSON mem)
          pure mem

    deleteMemoryH mid = do
      ok <- handleDBErrors $ Mem.deleteMemory pool mid
      if ok then do emit bc Deleted ETMemory mid Nothing; pure NoContent else throwError err404

    restoreMemoryH mid = do
      ok <- handleDBErrors $ Mem.restoreMemory pool mid
      if ok then do emit bc Updated ETMemory mid Nothing; pure NoContent else throwError err404

    purgeMemoryH mid = do
      rows <- handleDBErrors $ runSession pool $ Session.statement () $ run $ select $ do
        row <- each memorySchema
        where_ $ row.memId ==. lit mid
        pure row
      case rows of
        [] -> throwError err404
        (r:_)
          | r.memDeletedAt == Nothing -> throwError purgeConflict
          | otherwise -> do
              handleDBErrors $ runSession pool $ Session.statement () $ run_ $
                delete Rel8.Delete
                  { from = memorySchema
                  , using = pure ()
                  , deleteWhere = \_ row -> row.memId ==. lit mid
                  , returning = NoReturning
                  }
              pure NoContent

    getLinksH mid = do
      _ <- requireMemoryH mid
      handleDBErrors $ Mem.getMemoryLinks pool mid

    createLinkH mid cml = do
      _ <- requireMemoryH mid
      _ <- requireMemoryH cml.targetId
      handleDBErrors $ Mem.linkMemories pool mid cml
      emit bc Created ETMemoryLink mid (Just $ object ["source_id" .= mid, "target_id" .= cml.targetId])
      pure NoContent

    unlinkH mid tid rt = do
      _ <- requireMemoryH mid
      ok <- handleDBErrors $ Mem.unlinkMemories pool mid tid rt
      if ok then do emit bc Deleted ETMemoryLink mid (Just $ object ["source_id" .= mid, "target_id" .= tid]); pure NoContent else throwError err404

    getTagsH mid = do
      _ <- requireMemoryH mid
      handleDBErrors $ Mem.getTags pool mid

    setTagsH mid tags = do
      _ <- requireMemoryH mid
      handleDBErrors $ Mem.setTags pool mid tags
      emit bc Updated ETTag mid (Just $ object ["memory_id" .= mid, "tags" .= tags])
      pure NoContent

    graphH mid mdepth = do
      _ <- requireMemoryH mid
      let depth = fromMaybe 2 mdepth
      handleDBErrors $ Mem.getRelatedGraph pool mid depth

    adjustImportanceH mid adj = do
      mm <- handleDBErrors $ Mem.adjustImportance pool mid adj.importance
      case mm of
        Nothing -> throwError err404
        Just mem -> do
          emit bc Updated ETMemory mid (Just $ toJSON mem)
          pure mem

    pinH mid = do
      mm <- handleDBErrors $ Mem.togglePin pool mid True
      maybe (throwError err404) pure mm

    unpinH mid = do
      mm <- handleDBErrors $ Mem.togglePin pool mid False
      maybe (throwError err404) pure mm

    similarH sq
      | not pgvec = throwError err501
          { errBody = "pgvector extension is not installed; similarity search is unavailable" }
      | otherwise = handleDBErrors $ Mem.similarMemories pool sq

    setEmbeddingH mid vec
      | not pgvec = throwError err501
          { errBody = "pgvector extension is not installed; embeddings are unavailable" }
      | otherwise = do
          _ <- requireMemoryH mid
          handleDBErrors $ Mem.setEmbedding pool mid vec
          pure NoContent

    batchDeleteH br = do
      rejectValidationErrors (validateBatchDeleteRequest br)
      n <- handleDBErrors $ Mem.deleteMemoryBatch pool br.ids
      emitMany bc Deleted ETMemory br.ids
      pure BatchResult { affected = n }

    batchSetTagsH bst = do
      rejectValidationErrors (validateBatchSetTagsRequest bst)
      n <- handleDBErrors $ Mem.setTagsBatch pool [(item.memoryId, item.tags) | item <- bst.items]
      emitMany bc Updated ETTag (map (.memoryId) bst.items)
      pure BatchResult { affected = n }

    batchUpdateH bur = do
      rejectValidationErrors (validateBatchUpdateMemoryRequest bur)
      n <- handleDBErrors $ Mem.updateMemoryBatch pool [(item.id, item.update) | item <- bur.items]
      emitMany bc Updated ETMemory (map (.id) bur.items)
      pure BatchResult { affected = n }

-- Project handlers -------------------------------------------------

projectHandlers :: Pool Hasql.Connection -> Broadcast -> Server ProjectAPI
projectHandlers pool bc =
       listProjectsH
  :<|> createProjectH
  :<|> getProjectH
  :<|> updateProjectH
  :<|> deleteProjectH
  :<|> restoreProjectH
  :<|> purgeProjectH
  :<|> linkMemoryH
  :<|> unlinkMemoryH
  :<|> getProjectMemoriesH
  :<|> batchLinkMemoriesH
  :<|> batchDeleteH
  :<|> batchUpdateH
  :<|> projectOverviewH
  where
    requireProjectH :: UUID -> Handler Project
    requireProjectH pid = handleDBErrors (Proj.getProject pool pid) >>= maybe (throwError err404) pure

    listProjectsH mws mstatus mquery msearchLang mcreatedAfter mcreatedBefore mupdatedAfter mupdatedBefore mlimit moffset = do
      let lim = capLimit mlimit
          off = capOffset moffset
          query = ProjectListQuery
            { workspaceId = mws
            , status = mstatus
            , query = mquery
            , searchLanguage = msearchLang
            , createdAfter = mcreatedAfter
            , createdBefore = mcreatedBefore
            , updatedAfter = mupdatedAfter
            , updatedBefore = mupdatedBefore
            , limit = Just (lim + 1)
            , offset = Just off
            }
      rejectValidationErrors (validateProjectListQuery query)
      results <- handleDBErrors $ Proj.listProjectsWithQuery pool query
      pure PaginatedResult { items = take lim results, hasMore = length results > lim }

    createProjectH cp = do
      rejectValidationErrors (validateCreateProjectInput cp)
      proj <- handleDBErrors $ Proj.createProject pool cp
      emit bc Created ETProject proj.id (Just $ toJSON proj)
      pure proj
    getProjectH pid     = requireProjectH pid
    updateProjectH pid up = do
      rejectValidationErrors (validateUpdateProjectInput up)
      mp <- handleDBErrors (Proj.updateProject pool pid up)
      case mp of
        Nothing -> throwError err404
        Just proj -> do
          emit bc Updated ETProject pid (Just $ toJSON proj)
          pure proj

    deleteProjectH pid = do
      ok <- handleDBErrors $ Proj.deleteProject pool pid
      if ok then do emit bc Deleted ETProject pid Nothing; pure NoContent else throwError err404

    restoreProjectH pid = do
      ok <- handleDBErrors $ Proj.restoreProject pool pid
      if ok then do emit bc Updated ETProject pid Nothing; pure NoContent else throwError err404

    purgeProjectH pid = do
      rows <- handleDBErrors $ runSession pool $ Session.statement () $ run $ select $ do
        row <- each projectSchema
        where_ $ row.projId ==. lit pid
        pure row
      case rows of
        [] -> throwError err404
        (r:_)
          | r.projDeletedAt == Nothing -> throwError purgeConflict
          | otherwise -> do
              handleDBErrors $ runSession pool $ Session.statement () $ run_ $
                delete Rel8.Delete
                  { from = projectSchema
                  , using = pure ()
                  , deleteWhere = \_ row -> row.projId ==. lit pid
                  , returning = NoReturning
                  }
              pure NoContent

    linkMemoryH pid lm = do
      _ <- requireProjectH pid
      _ <- handleDBErrors (Mem.getMemory pool lm.memoryId) >>= maybe (throwError err404) pure
      handleDBErrors $ Proj.linkProjectMemory pool pid lm.memoryId
      emit bc Updated ETProject pid (Just $ object ["linked_memory" .= lm.memoryId])
      pure NoContent

    unlinkMemoryH pid mid = do
      _ <- requireProjectH pid
      handleDBErrors $ Proj.unlinkProjectMemory pool pid mid
      emit bc Updated ETProject pid (Just $ object ["unlinked_memory" .= mid])
      pure NoContent

    getProjectMemoriesH pid = do
      _ <- requireProjectH pid
      handleDBErrors $ Mem.getProjectMemories pool pid

    batchLinkMemoriesH pid blr = do
      _ <- requireProjectH pid
      rejectValidationErrors (validateBatchMemoryLinkRequest blr)
      n <- handleDBErrors $ Proj.linkProjectMemoryBatch pool pid blr.memoryIds
      emit bc Updated ETProject pid Nothing
      pure BatchResult { affected = n }

    batchDeleteH br = do
      rejectValidationErrors (validateBatchDeleteRequest br)
      n <- handleDBErrors $ Proj.deleteProjectBatch pool br.ids
      emitMany bc Deleted ETProject br.ids
      pure BatchResult { affected = n }

    batchUpdateH bur = do
      rejectValidationErrors (validateBatchUpdateProjectRequest bur)
      n <- handleDBErrors $ Proj.updateProjectBatch pool [(item.id, item.update) | item <- bur.items]
      emitMany bc Updated ETProject (map (.id) bur.items)
      pure BatchResult { affected = n }

    projectOverviewH pid = do
      proj <- requireProjectH pid
      tasks <- handleDBErrors $ Task.listTasksWithQuery pool TaskListQuery
        { workspaceId = Nothing, projectId = Just pid, status = Nothing
        , priority = Nothing, createdAfter = Nothing, createdBefore = Nothing
        , updatedAfter = Nothing, updatedBefore = Nothing
        , query = Nothing, searchLanguage = Nothing
        , limit = Just 200, offset = Just 0 }
      allProjects <- handleDBErrors $ Proj.listProjectsWithQuery pool ProjectListQuery
        { workspaceId = Just proj.workspaceId, status = Nothing
        , createdAfter = Nothing, createdBefore = Nothing
        , updatedAfter = Nothing, updatedBefore = Nothing
        , query = Nothing, searchLanguage = Nothing
        , limit = Just 200, offset = Just 0 }
      let childProjects = Prelude.filter (\p -> p.parentId == Just pid) allProjects
      mems <- handleDBErrors $ Mem.getProjectMemories pool pid
      pure ProjectOverview
        { project = proj
        , tasks = tasks
        , subprojects = childProjects
        , linkedMemories = mems
        }

-- Task handlers ----------------------------------------------------

taskHandlers :: Pool Hasql.Connection -> Broadcast -> Server TaskAPI
taskHandlers pool bc =
       listTasksH
  :<|> createTaskH
  :<|> getTaskH
  :<|> updateTaskH
  :<|> deleteTaskH
  :<|> restoreTaskH
  :<|> purgeTaskH
  :<|> linkMemoryH
  :<|> unlinkMemoryH
  :<|> addDepH
  :<|> removeDepH
  :<|> getTaskMemoriesH
  :<|> taskOverviewH
  :<|> contextInfoH
  :<|> batchDeleteH
  :<|> batchMoveH
  :<|> batchUpdateH
  :<|> batchLinkMemoriesH
  where
    requireTaskH :: UUID -> Handler Task
    requireTaskH tid = handleDBErrors (Task.getTask pool tid) >>= maybe (throwError err404) pure

    listTasksH mws mpid mstatus mpriority mquery msearchLang mcreatedAfter mcreatedBefore mupdatedAfter mupdatedBefore mlimit moffset = do
      let lim = capLimit mlimit
          off = capOffset moffset
          query = TaskListQuery
            { workspaceId = mws
            , projectId = mpid
            , status = mstatus
            , priority = mpriority
            , query = mquery
            , searchLanguage = msearchLang
            , createdAfter = mcreatedAfter
            , createdBefore = mcreatedBefore
            , updatedAfter = mupdatedAfter
            , updatedBefore = mupdatedBefore
            , limit = Just (lim + 1)
            , offset = Just off
            }
      rejectValidationErrors (validateTaskListQuery query)
      results <- handleDBErrors $ Task.listTasksWithQuery pool query
      pure PaginatedResult { items = take lim results, hasMore = length results > lim }

    createTaskH ct = do
      rejectValidationErrors (validateCreateTaskInput ct)
      task <- handleDBErrors $ Task.createTask pool ct
      emit bc Created ETTask task.id (Just $ toJSON task)
      pure task
    getTaskH tid     = requireTaskH tid
    updateTaskH tid ut = do
      rejectValidationErrors (validateUpdateTaskInput ut)
      mt <- handleDBErrors (Task.updateTask pool tid ut)
      case mt of
        Nothing -> throwError err404
        Just task -> do
          emit bc Updated ETTask tid (Just $ toJSON task)
          pure task

    deleteTaskH tid = do
      ok <- handleDBErrors $ Task.deleteTask pool tid
      if ok then do emit bc Deleted ETTask tid Nothing; pure NoContent else throwError err404

    restoreTaskH tid = do
      ok <- handleDBErrors $ Task.restoreTask pool tid
      if ok then do emit bc Updated ETTask tid Nothing; pure NoContent else throwError err404

    purgeTaskH tid = do
      rows <- handleDBErrors $ runSession pool $ Session.statement () $ run $ select $ do
        row <- each taskSchema
        where_ $ row.taskId ==. lit tid
        pure row
      case rows of
        [] -> throwError err404
        (r:_)
          | r.taskDeletedAt == Nothing -> throwError purgeConflict
          | otherwise -> do
              handleDBErrors $ runSession pool $ Session.statement () $ run_ $
                delete Rel8.Delete
                  { from = taskSchema
                  , using = pure ()
                  , deleteWhere = \_ row -> row.taskId ==. lit tid
                  , returning = NoReturning
                  }
              pure NoContent

    linkMemoryH tid lm = do
      _ <- requireTaskH tid
      _ <- handleDBErrors (Mem.getMemory pool lm.memoryId) >>= maybe (throwError err404) pure
      handleDBErrors $ Task.linkTaskMemory pool tid lm.memoryId
      emit bc Updated ETTask tid (Just $ object ["linked_memory" .= lm.memoryId])
      pure NoContent

    unlinkMemoryH tid mid = do
      _ <- requireTaskH tid
      handleDBErrors $ Task.unlinkTaskMemory pool tid mid
      emit bc Updated ETTask tid (Just $ object ["unlinked_memory" .= mid])
      pure NoContent

    addDepH tid ld = do
      _ <- requireTaskH tid
      _ <- requireTaskH ld.dependsOnId
      handleDBErrors $ Task.addDependency pool tid ld.dependsOnId
      emit bc Created ETTaskDependency tid (Just $ object ["task_id" .= tid, "depends_on_id" .= ld.dependsOnId])
      pure NoContent

    removeDepH tid depId = do
      _ <- requireTaskH tid
      handleDBErrors $ Task.removeDependency pool tid depId
      emit bc Deleted ETTaskDependency tid (Just $ object ["task_id" .= tid, "depends_on_id" .= depId])
      pure NoContent

    getTaskMemoriesH tid = do
      _ <- requireTaskH tid
      handleDBErrors $ Mem.getTaskMemories pool tid

    taskOverviewH tid mExtraContext = do
      let extraContext = fromMaybe False mExtraContext
      handleDBErrors (Overview.getTaskOverview pool tid extraContext) >>= maybe (throwError err404) pure

    contextInfoH tid mDetailLevel = do
      let level = fromMaybe ContextMedium mDetailLevel
      handleDBErrors (Overview.getContextInfo pool tid level) >>= maybe (throwError err404) pure

    batchDeleteH br = do
      rejectValidationErrors (validateBatchDeleteRequest br)
      n <- handleDBErrors $ Task.deleteTaskBatch pool br.ids
      emitMany bc Deleted ETTask br.ids
      pure BatchResult { affected = n }

    batchMoveH bmr = do
      rejectValidationErrors (validateBatchMoveTasksRequest bmr)
      n <- handleDBErrors $ Task.moveTasksBatch pool bmr.taskIds bmr.projectId
      emitMany bc Updated ETTask bmr.taskIds
      pure BatchResult { affected = n }

    batchUpdateH bur = do
      rejectValidationErrors (validateBatchUpdateTaskRequest bur)
      n <- handleDBErrors $ Task.updateTaskBatch pool [(item.id, item.update) | item <- bur.items]
      emitMany bc Updated ETTask (map (.id) bur.items)
      pure BatchResult { affected = n }

    batchLinkMemoriesH tid blr = do
      _ <- requireTaskH tid
      rejectValidationErrors (validateBatchMemoryLinkRequest blr)
      n <- handleDBErrors $ Task.linkTaskMemoryBatch pool tid blr.memoryIds
      emit bc Updated ETTask tid Nothing
      pure BatchResult { affected = n }

-- Cleanup handlers -------------------------------------------------

cleanupHandlers :: Pool Hasql.Connection -> Broadcast -> Server CleanupAPI
cleanupHandlers pool _bc =
       runCleanupH
  :<|> getPoliciesH
  :<|> upsertPolicyH
  where
    runCleanupH req = handleDBErrors $ Cleanup.runCleanup pool req.workspaceId
    getPoliciesH mws mlimit moffset = do
      wsId <- requireParam "workspace_id" mws
      let lim = capLimit mlimit
          off = capOffset moffset
      results <- handleDBErrors $ Cleanup.getCleanupPolicies pool wsId (Just (lim + 1)) (Just off)
      pure PaginatedResult { items = take lim results, hasMore = length results > lim }
    upsertPolicyH p = handleDBErrors $ Cleanup.upsertCleanupPolicy pool p

-- Category handlers ------------------------------------------------

categoryHandlers :: Pool Hasql.Connection -> Broadcast -> Server CategoryAPI
categoryHandlers pool bc =
       listCategoriesH
  :<|> listGlobalCategoriesH
  :<|> createCategoryH
  :<|> getCategoryH
  :<|> updateCategoryH
  :<|> deleteCategoryH
  :<|> restoreCategoryH
  :<|> purgeCategoryH
  :<|> getCategoryMemoriesH
  :<|> batchLinkMemoriesH
  :<|> batchDeleteH
  :<|> linkH
  :<|> unlinkH
  where
    requireCategoryH :: UUID -> Handler MemoryCategory
    requireCategoryH cid = handleDBErrors (Cat.getCategory pool cid) >>= maybe (throwError err404) pure

    listCategoriesH mws mlimit moffset = do
      let lim = capLimit mlimit
          off = capOffset moffset
      results <- case mws of
        Just wsId -> handleDBErrors $ Cat.listCategories pool wsId (Just (lim + 1)) (Just off)
        Nothing   -> handleDBErrors $ Cat.listGlobalCategories pool (Just (lim + 1)) (Just off)
      pure PaginatedResult { items = take lim results, hasMore = length results > lim }

    listGlobalCategoriesH mlimit moffset = do
      let lim = capLimit mlimit
          off = capOffset moffset
      results <- handleDBErrors $ Cat.listGlobalCategories pool (Just (lim + 1)) (Just off)
      pure PaginatedResult { items = take lim results, hasMore = length results > lim }

    createCategoryH cc = do
      rejectValidationErrors (validateCreateMemoryCategoryInput cc)
      cat <- handleDBErrors $ Cat.createCategory pool cc
      emit bc Created ETCategory cat.id (Just $ toJSON cat)
      pure cat
    getCategoryH cid   = requireCategoryH cid
    updateCategoryH cid uc = do
      rejectValidationErrors (validateUpdateMemoryCategoryInput uc)
      mc <- handleDBErrors (Cat.updateCategory pool cid uc)
      case mc of
        Nothing -> throwError err404
        Just cat -> do
          emit bc Updated ETCategory cid (Just $ toJSON cat)
          pure cat

    deleteCategoryH cid = do
      ok <- handleDBErrors $ Cat.deleteCategory pool cid
      if ok then do emit bc Deleted ETCategory cid Nothing; pure NoContent else throwError err404

    restoreCategoryH cid = do
      ok <- handleDBErrors $ Cat.restoreCategory pool cid
      if ok then do emit bc Updated ETCategory cid Nothing; pure NoContent else throwError err404

    purgeCategoryH cid = do
      rows <- handleDBErrors $ runSession pool $ Session.statement () $ run $ select $ do
        row <- each memoryCategorySchema
        where_ $ row.mcId ==. lit cid
        pure row
      case rows of
        [] -> throwError err404
        (r:_)
          | r.mcDeletedAt == Nothing -> throwError purgeConflict
          | otherwise -> do
              handleDBErrors $ runSession pool $ Session.statement () $ run_ $
                delete Rel8.Delete
                  { from = memoryCategorySchema
                  , using = pure ()
                  , deleteWhere = \_ row -> row.mcId ==. lit cid
                  , returning = NoReturning
                  }
              pure NoContent

    getCategoryMemoriesH cid = do
      _ <- requireCategoryH cid
      handleDBErrors $ Mem.getCategoryMemories pool cid

    batchLinkMemoriesH cid blr = do
      _ <- requireCategoryH cid
      rejectValidationErrors (validateBatchMemoryLinkRequest blr)
      n <- handleDBErrors $ Cat.linkMemoryCategoryBatch pool cid blr.memoryIds
      emit bc Updated ETCategory cid Nothing
      pure BatchResult { affected = n }

    batchDeleteH br = do
      rejectValidationErrors (validateBatchDeleteRequest br)
      n <- handleDBErrors $ Cat.deleteCategoryBatch pool br.ids
      emitMany bc Deleted ETCategory br.ids
      pure BatchResult { affected = n }

    linkH cl = do
      _ <- requireCategoryH cl.categoryId
      _ <- handleDBErrors (Mem.getMemory pool cl.memoryId) >>= maybe (throwError err404) pure
      handleDBErrors $ Cat.linkMemoryCategory pool cl.memoryId cl.categoryId
      emit bc Created ETCategoryLink cl.categoryId (Just $ object ["category_id" .= cl.categoryId, "memory_id" .= cl.memoryId])
      pure NoContent

    unlinkH cl = do
      _ <- requireCategoryH cl.categoryId
      handleDBErrors $ Cat.unlinkMemoryCategory pool cl.memoryId cl.categoryId
      emit bc Deleted ETCategoryLink cl.categoryId (Just $ object ["category_id" .= cl.categoryId, "memory_id" .= cl.memoryId])
      pure NoContent

-- Workspace group handlers -----------------------------------------

workspaceGroupHandlers :: Pool Hasql.Connection -> Broadcast -> Server WorkspaceGroupAPI
workspaceGroupHandlers pool bc =
       listGroupsH
  :<|> createGroupH
  :<|> getGroupH
  :<|> deleteGroupH
  :<|> addMemberH
  :<|> removeMemberH
  :<|> listMembersH
  where
    listGroupsH mlimit moffset = do
      let lim = capLimit mlimit
          off = capOffset moffset
      results <- handleDBErrors $ WG.listGroups pool (Just (lim + 1)) (Just off)
      pure PaginatedResult { items = take lim results, hasMore = length results > lim }

    createGroupH cg = do
      rejectValidationErrors (validateCreateWorkspaceGroupInput cg)
      grp <- handleDBErrors $ WG.createGroup pool cg
      emit bc Created ETWorkspaceGroup grp.id (Just $ toJSON grp)
      pure grp

    getGroupH gid = do
      mg <- handleDBErrors $ WG.getGroup pool gid
      maybe (throwError err404) pure mg

    deleteGroupH gid = do
      ok <- handleDBErrors $ WG.deleteGroup pool gid
      if ok then do emit bc Deleted ETWorkspaceGroup gid Nothing; pure NoContent else throwError err404

    addMemberH gid req = do
      handleDBErrors $ WG.addMember pool gid req.workspaceId
      emit bc Updated ETWorkspaceGroup gid (Just $ object ["added_workspace" .= req.workspaceId])
      pure NoContent

    removeMemberH gid wsId = do
      handleDBErrors $ WG.removeMember pool gid wsId
      emit bc Updated ETWorkspaceGroup gid (Just $ object ["removed_workspace" .= wsId])
      pure NoContent

    listMembersH gid = handleDBErrors $ WG.listGroupMembers pool gid

-- Activity handlers ------------------------------------------------

activityHandlers :: Pool Hasql.Connection -> Server ActivityAPI
activityHandlers pool mws mEntityType mlimit = do
  let lim = capLimit mlimit
  results <- handleDBErrors $ Mem.getRecentActivity pool mws mEntityType (Just (lim + 1))
  pure PaginatedResult { items = take lim results, hasMore = length results > lim }

-- Saved view handlers ------------------------------------------------

savedViewHandlers :: Pool Hasql.Connection -> Broadcast -> Server SavedViewAPI
savedViewHandlers pool bc =
       listViewsH
  :<|> createViewH
  :<|> getViewH
  :<|> updateViewH
  :<|> deleteViewH
  :<|> restoreViewH
  :<|> purgeViewH
  :<|> executeViewH
  where
    requireViewH :: UUID -> Handler SavedView
    requireViewH vid = handleDBErrors (SV.getSavedView pool vid) >>= maybe (throwError err404) pure

    listViewsH mws mlimit moffset = do
      wsId <- requireParam "workspace_id" mws
      let lim = capLimit mlimit
          off = capOffset moffset
      results <- handleDBErrors $ SV.listSavedViews pool wsId (Just (lim + 1)) (Just off)
      pure PaginatedResult { items = take lim results, hasMore = length results > lim }

    createViewH csv = do
      rejectValidationErrors (validateCreateSavedViewInput csv)
      sv <- handleDBErrors $ SV.createSavedView pool csv
      emit bc Created ETSavedView sv.id (Just $ toJSON sv)
      pure sv

    getViewH vid = requireViewH vid

    updateViewH vid usv = do
      rejectValidationErrors (validateUpdateSavedViewInput usv)
      msv <- handleDBErrors (SV.updateSavedView pool vid usv)
      case msv of
        Nothing -> throwError err404
        Just sv -> do
          emit bc Updated ETSavedView vid (Just $ toJSON sv)
          pure sv

    deleteViewH vid = do
      ok <- handleDBErrors $ SV.deleteSavedView pool vid
      if ok then do emit bc Deleted ETSavedView vid Nothing; pure NoContent else throwError err404

    restoreViewH vid = do
      ok <- handleDBErrors $ SV.restoreSavedView pool vid
      if ok then do emit bc Updated ETSavedView vid Nothing; pure NoContent else throwError err404

    purgeViewH vid = do
      ok <- handleDBErrors $ SV.purgeSavedView pool vid
      if ok then pure NoContent else throwError err409
        { errBody = Aeson.encode $ object
            [ "error" .= ("conflict" :: Text)
            , "message" .= ("Saved view must be soft-deleted before purge, or does not exist" :: Text)
            ]
        }

    executeViewH vid mlimit' moffset' mdetail = do
      view <- requireViewH vid
      let lim = capLimit mlimit'
          off = capOffset moffset'
          compact = mdetail /= Just True
      case view.entityType of
        "memory_search" -> do
          case Aeson.fromJSON @SearchQuery view.queryParams of
            Aeson.Error e -> throwError err400 { errBody = fromString $ "Invalid query_params: " <> e }
            Aeson.Success sq -> do
              let sq' = SearchQuery
                    { workspaceId = sq.workspaceId, query = sq.query
                    , memoryType = sq.memoryType, tags = sq.tags
                    , minImportance = sq.minImportance, categoryId = sq.categoryId
                    , pinnedOnly = sq.pinnedOnly, searchLanguage = sq.searchLanguage
                    , limit = Just lim, offset = Just off }
              results <- handleDBErrors $ Mem.searchMemories pool sq'
              pure $ Aeson.toJSON $ if compact then map compactMemory results else results
        "memory_list" -> do
          case Aeson.fromJSON @MemoryListQuery view.queryParams of
            Aeson.Error e -> throwError err400 { errBody = fromString $ "Invalid query_params: " <> e }
            Aeson.Success mq -> do
              let mq' = MemoryListQuery
                    { workspaceId = mq.workspaceId, memoryType = mq.memoryType
                    , createdAfter = mq.createdAfter, createdBefore = mq.createdBefore
                    , updatedAfter = mq.updatedAfter, updatedBefore = mq.updatedBefore
                    , limit = Just (lim + 1), offset = Just off }
              results <- handleDBErrors $ Mem.listMemoriesWithQuery pool mq'
              let items = (if compact then map compactMemory else Prelude.id) $ take lim results
              pure $ Aeson.toJSON PaginatedResult { items = items, hasMore = length results > lim }
        "project_list" -> do
          case Aeson.fromJSON @ProjectListQuery view.queryParams of
            Aeson.Error e -> throwError err400 { errBody = fromString $ "Invalid query_params: " <> e }
            Aeson.Success pq -> do
              let pq' = ProjectListQuery
                    { workspaceId = pq.workspaceId, status = pq.status
                    , query = pq.query, searchLanguage = pq.searchLanguage
                    , createdAfter = pq.createdAfter, createdBefore = pq.createdBefore
                    , updatedAfter = pq.updatedAfter, updatedBefore = pq.updatedBefore
                    , limit = Just (lim + 1), offset = Just off }
              results <- handleDBErrors $ Proj.listProjectsWithQuery pool pq'
              pure $ Aeson.toJSON PaginatedResult { items = take lim results, hasMore = length results > lim }
        "task_list" -> do
          case Aeson.fromJSON @TaskListQuery view.queryParams of
            Aeson.Error e -> throwError err400 { errBody = fromString $ "Invalid query_params: " <> e }
            Aeson.Success tq -> do
              let tq' = TaskListQuery
                    { workspaceId = tq.workspaceId, projectId = tq.projectId
                    , status = tq.status, priority = tq.priority
                    , query = tq.query, searchLanguage = tq.searchLanguage
                    , createdAfter = tq.createdAfter, createdBefore = tq.createdBefore
                    , updatedAfter = tq.updatedAfter, updatedBefore = tq.updatedBefore
                    , limit = Just (lim + 1), offset = Just off }
              results <- handleDBErrors $ Task.listTasksWithQuery pool tq'
              pure $ Aeson.toJSON PaginatedResult { items = take lim results, hasMore = length results > lim }
        "activity" -> do
          let parseField :: FromJSON a => Text -> Maybe a
              parseField key = case view.queryParams of
                Aeson.Object o -> case KeyMap.lookup (Aeson.fromText key) o of
                  Just v  -> case Aeson.fromJSON v of
                    Aeson.Success a -> Just a
                    _               -> Nothing
                  Nothing -> Nothing
                _ -> Nothing
              mws = parseField "workspace_id"
              met = parseField "entity_type"
          results <- handleDBErrors $ Mem.getRecentActivity pool mws met (Just (lim + 1))
          pure $ Aeson.toJSON PaginatedResult { items = take lim results, hasMore = length results > lim }
        other -> throwError err400
          { errBody = Aeson.encode $ object
              [ "error" .= ("invalid_entity_type" :: Text)
              , "message" .= ("Unknown entity_type: " <> other)
              ]
          }

------------------------------------------------------------------------
-- Unified search
------------------------------------------------------------------------

searchHandler :: Pool Hasql.Connection -> Server SearchAPI
searchHandler pool usq = do
  rejectValidationErrors (validateUnifiedSearchQuery usq)
  handleDBErrors $ Search.searchAll pool usq

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

requireParam :: Text -> Maybe a -> Handler a
requireParam name Nothing  = throwError $ err400 { errBody = fromString ("Missing required parameter: " <> T.unpack name) }
requireParam _    (Just a) = pure a

rejectValidationErrors :: [Text] -> Handler ()
rejectValidationErrors [] = pure ()
rejectValidationErrors errs = throwError err400
  { errBody = Aeson.encode $ object
      [ "error" .= ("validation" :: Text)
      , "message" .= ("Request validation failed" :: Text)
      , "details" .= errs
      ]
  }

-- | Clamp a user-supplied limit to [1, 200], defaulting to 50.
capLimit :: Maybe Int -> Int
capLimit = Prelude.min 200 . Prelude.max 1 . fromMaybe 50

-- | Clamp a user-supplied offset to [0, 10000], defaulting to 0.
capOffset :: Maybe Int -> Int
capOffset = Prelude.min 10000 . Prelude.max 0 . fromMaybe 0
