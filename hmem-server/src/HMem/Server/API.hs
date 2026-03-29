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
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Functor.Contravariant ((>$<))
import Data.Maybe (fromMaybe)
import Data.Pool (Pool, tryWithResource)
import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime)
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
import HMem.DB.Pool (runSession, runTransaction, DBException(..))
import HMem.DB.Project qualified as Proj
import HMem.DB.RequestContext (currentRequestId)
import HMem.DB.Schema
import HMem.DB.Task qualified as Task
import HMem.DB.WorkspaceGroup qualified as WG
import HMem.Server.AccessTracker (AccessTracker, trackAccess, bufferSize)
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
  :<|> Capture "memoryId" UUID :> Get '[JSON] Memory
  :<|> Capture "memoryId" UUID :> ReqBody '[JSON] UpdateMemory :> Put '[JSON] Memory
  :<|> Capture "memoryId" UUID :> Delete '[JSON] NoContent
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

-- Projects
type ProjectAPI =
       QueryParam "workspace_id" UUID
         :> QueryParam "status" ProjectStatus
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
  :<|> Capture "projectId" UUID :> "purge" :> Delete '[JSON] NoContent
  :<|> Capture "projectId" UUID :> "memories" :> ReqBody '[JSON] LinkMemory
         :> Post '[JSON] NoContent
  :<|> Capture "projectId" UUID :> "memories" :> Capture "memoryId" UUID
         :> Delete '[JSON] NoContent
  :<|> Capture "projectId" UUID :> "memories" :> Get '[JSON] [Memory]

-- Tasks
type TaskAPI =
       QueryParam "workspace_id" UUID
         :> QueryParam "project_id" UUID
         :> QueryParam "status" TaskStatus
         :> QueryParam "priority" Int
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

-- Cleanup
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
    :<|> Capture "categoryId" UUID :> "purge" :> Delete '[JSON] NoContent
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
         :> QueryParam "limit" Int
         :> Get '[JSON] (PaginatedResult ActivityEvent)

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

server :: Pool Hasql.Connection -> AccessTracker -> Bool -> Server HMemAPI
server pool tracker pgvec =
       healthHandler pool tracker
  :<|> workspaceHandlers pool
  :<|> memoryHandlers pool tracker pgvec
  :<|> projectHandlers pool
  :<|> taskHandlers pool
  :<|> cleanupHandlers pool
  :<|> categoryHandlers pool
  :<|> workspaceGroupHandlers pool
  :<|> activityHandlers pool

-- Health handler ---------------------------------------------------

healthHandler :: Pool Hasql.Connection -> AccessTracker -> Server HealthAPI
healthHandler pool tracker = do
  -- Non-blocking pool check: try to acquire a connection and run SELECT 1
  mResult <- liftIO $ tryWithResource pool $ \conn ->
    Session.run (Session.sql "SELECT 1") conn
  bufSz <- liftIO $ bufferSize tracker
  let (dbStatus, overallStatus) = case mResult of
        Just (Right _) -> ("connected" :: Text, "ok" :: Text)
        Just (Left _)  -> ("error",             "degraded")
        Nothing        -> ("pool_exhausted",    "degraded")
  pure $ object
    [ "status"  .= overallStatus
    , "version" .= ("0.1.0.0" :: Text)
    , "database" .= object
        [ "status" .= dbStatus
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
  , path          = r.wsPath
  , ghOwner       = r.wsGhOwner
  , ghRepo        = r.wsGhRepo
  , workspaceType = r.wsType
  , createdAt     = r.wsCreatedAt
  , updatedAt     = r.wsUpdatedAt
  }

workspaceHandlers :: Pool Hasql.Connection -> Server WorkspaceAPI
workspaceHandlers pool =
       listWorkspacesH
  :<|> createWorkspaceH
  :<|> getWorkspaceH
  :<|> updateWorkspaceH
  :<|> deleteWorkspaceH
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
      handleDBErrors $ do
        rows <- runSession pool $ Session.statement () $ run $
          insert Insert
            { into = workspaceSchema
            , rows = values
                [ WorkspaceT
                    { wsId        = unsafeDefault
                    , wsName      = lit cw.name
                    , wsPath      = lit cw.path
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
              , wsPath    = applyNullableUpdate row.wsPath uw.path
              , wsGhOwner = applyNullableUpdate row.wsGhOwner uw.ghOwner
              , wsGhRepo  = applyNullableUpdate row.wsGhRepo uw.ghRepo
              }
          , updateWhere = \_ row -> row.wsId ==. lit wsId &&. activeWorkspace row
          , returning = Returning id
          }
      case rows of
        (r:_) -> pure $ rowToWorkspace r
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
      if ok then pure NoContent else throwError err404

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
                  { from = workspaceSchema
                  , using = pure ()
                  , deleteWhere = \_ row -> row.wsId ==. lit wsId
                  , returning = NoReturning
                  }
              pure NoContent

-- Memory handlers --------------------------------------------------

memoryHandlers :: Pool Hasql.Connection -> AccessTracker -> Bool -> Server MemoryAPI
memoryHandlers pool tracker pgvec =
       listMemoriesH
  :<|> createMemoryH
  :<|> createMemoryBatchH
  :<|> searchMemoriesH
  :<|> contradictionsH
  :<|> byRelationH
  :<|> getMemoryH
  :<|> updateMemoryH
  :<|> deleteMemoryH
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
  where
    requireMemoryH :: UUID -> Handler Memory
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
      handleDBErrors $ Mem.createMemory pool cm

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

    getMemoryH mid = do
      mem <- requireMemoryH mid
      liftIO $ trackAccess tracker mid
      pure mem

    updateMemoryH mid um = do
      rejectValidationErrors (validateUpdateMemoryInput um)
      mm <- handleDBErrors $ Mem.updateMemory pool mid um
      maybe (throwError err404) pure mm

    deleteMemoryH mid = do
      ok <- handleDBErrors $ Mem.deleteMemory pool mid
      if ok then pure NoContent else throwError err404

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
      pure NoContent

    unlinkH mid tid rt = do
      _ <- requireMemoryH mid
      ok <- handleDBErrors $ Mem.unlinkMemories pool mid tid rt
      if ok then pure NoContent else throwError err404

    getTagsH mid = do
      _ <- requireMemoryH mid
      handleDBErrors $ Mem.getTags pool mid

    setTagsH mid tags = do
      _ <- requireMemoryH mid
      handleDBErrors $ Mem.setTags pool mid tags
      pure NoContent

    graphH mid mdepth = do
      _ <- requireMemoryH mid
      let depth = fromMaybe 2 mdepth
      handleDBErrors $ Mem.getRelatedGraph pool mid depth

    adjustImportanceH mid adj = do
      mm <- handleDBErrors $ Mem.adjustImportance pool mid adj.importance
      maybe (throwError err404) pure mm

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

-- Project handlers -------------------------------------------------

projectHandlers :: Pool Hasql.Connection -> Server ProjectAPI
projectHandlers pool =
       listProjectsH
  :<|> createProjectH
  :<|> getProjectH
  :<|> updateProjectH
  :<|> deleteProjectH
  :<|> purgeProjectH
  :<|> linkMemoryH
  :<|> unlinkMemoryH
  :<|> getProjectMemoriesH
  where
    requireProjectH :: UUID -> Handler Project
    requireProjectH pid = handleDBErrors (Proj.getProject pool pid) >>= maybe (throwError err404) pure

    listProjectsH mws mstatus mcreatedAfter mcreatedBefore mupdatedAfter mupdatedBefore mlimit moffset = do
      wsId <- requireParam "workspace_id" mws
      let lim = capLimit mlimit
          off = capOffset moffset
          query = ProjectListQuery
            { workspaceId = wsId
            , status = mstatus
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
      handleDBErrors $ Proj.createProject pool cp
    getProjectH pid     = requireProjectH pid
    updateProjectH pid up = do
      rejectValidationErrors (validateUpdateProjectInput up)
      handleDBErrors (Proj.updateProject pool pid up) >>= maybe (throwError err404) pure

    deleteProjectH pid = do
      ok <- handleDBErrors $ Proj.deleteProject pool pid
      if ok then pure NoContent else throwError err404

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
      pure NoContent

    unlinkMemoryH pid mid = do
      _ <- requireProjectH pid
      handleDBErrors $ Proj.unlinkProjectMemory pool pid mid
      pure NoContent

    getProjectMemoriesH pid = do
      _ <- requireProjectH pid
      handleDBErrors $ Mem.getProjectMemories pool pid

-- Task handlers ----------------------------------------------------

taskHandlers :: Pool Hasql.Connection -> Server TaskAPI
taskHandlers pool =
       listTasksH
  :<|> createTaskH
  :<|> getTaskH
  :<|> updateTaskH
  :<|> deleteTaskH
  :<|> purgeTaskH
  :<|> linkMemoryH
  :<|> unlinkMemoryH
  :<|> addDepH
  :<|> removeDepH
  :<|> getTaskMemoriesH
  where
    requireTaskH :: UUID -> Handler Task
    requireTaskH tid = handleDBErrors (Task.getTask pool tid) >>= maybe (throwError err404) pure

    listTasksH mws mpid mstatus mpriority mcreatedAfter mcreatedBefore mupdatedAfter mupdatedBefore mlimit moffset = do
      let lim = capLimit mlimit
          off = capOffset moffset
          query = TaskListQuery
            { workspaceId = mws
            , projectId = mpid
            , status = mstatus
            , priority = mpriority
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
      handleDBErrors $ Task.createTask pool ct
    getTaskH tid     = requireTaskH tid
    updateTaskH tid ut = do
      rejectValidationErrors (validateUpdateTaskInput ut)
      handleDBErrors (Task.updateTask pool tid ut) >>= maybe (throwError err404) pure

    deleteTaskH tid = do
      ok <- handleDBErrors $ Task.deleteTask pool tid
      if ok then pure NoContent else throwError err404

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
      pure NoContent

    unlinkMemoryH tid mid = do
      _ <- requireTaskH tid
      handleDBErrors $ Task.unlinkTaskMemory pool tid mid
      pure NoContent

    addDepH tid ld = do
      _ <- requireTaskH tid
      _ <- requireTaskH ld.dependsOnId
      handleDBErrors $ Task.addDependency pool tid ld.dependsOnId
      pure NoContent

    removeDepH tid depId = do
      _ <- requireTaskH tid
      handleDBErrors $ Task.removeDependency pool tid depId
      pure NoContent

    getTaskMemoriesH tid = do
      _ <- requireTaskH tid
      handleDBErrors $ Mem.getTaskMemories pool tid

-- Cleanup handlers -------------------------------------------------

cleanupHandlers :: Pool Hasql.Connection -> Server CleanupAPI
cleanupHandlers pool =
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

categoryHandlers :: Pool Hasql.Connection -> Server CategoryAPI
categoryHandlers pool =
       listCategoriesH
  :<|> listGlobalCategoriesH
  :<|> createCategoryH
  :<|> getCategoryH
  :<|> updateCategoryH
  :<|> deleteCategoryH
  :<|> purgeCategoryH
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
      handleDBErrors $ Cat.createCategory pool cc
    getCategoryH cid   = requireCategoryH cid
    updateCategoryH cid uc = do
      rejectValidationErrors (validateUpdateMemoryCategoryInput uc)
      handleDBErrors (Cat.updateCategory pool cid uc) >>= maybe (throwError err404) pure

    deleteCategoryH cid = do
      ok <- handleDBErrors $ Cat.deleteCategory pool cid
      if ok then pure NoContent else throwError err404

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

    linkH cl = do
      _ <- requireCategoryH cl.categoryId
      _ <- handleDBErrors (Mem.getMemory pool cl.memoryId) >>= maybe (throwError err404) pure
      handleDBErrors $ Cat.linkMemoryCategory pool cl.memoryId cl.categoryId
      pure NoContent

    unlinkH cl = do
      _ <- requireCategoryH cl.categoryId
      handleDBErrors $ Cat.unlinkMemoryCategory pool cl.memoryId cl.categoryId
      pure NoContent

-- Workspace group handlers -----------------------------------------

workspaceGroupHandlers :: Pool Hasql.Connection -> Server WorkspaceGroupAPI
workspaceGroupHandlers pool =
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
      handleDBErrors $ WG.createGroup pool cg

    getGroupH gid = do
      mg <- handleDBErrors $ WG.getGroup pool gid
      maybe (throwError err404) pure mg

    deleteGroupH gid = do
      ok <- handleDBErrors $ WG.deleteGroup pool gid
      if ok then pure NoContent else throwError err404

    addMemberH gid req = do
      handleDBErrors $ WG.addMember pool gid req.workspaceId
      pure NoContent

    removeMemberH gid wsId = do
      handleDBErrors $ WG.removeMember pool gid wsId
      pure NoContent

    listMembersH gid = handleDBErrors $ WG.listGroupMembers pool gid

-- Activity handlers ------------------------------------------------

activityHandlers :: Pool Hasql.Connection -> Server ActivityAPI
activityHandlers pool mws mlimit = do
  let lim = capLimit mlimit
  results <- handleDBErrors $ Mem.getRecentActivity pool mws (Just (lim + 1))
  pure PaginatedResult { items = take lim results, hasMore = length results > lim }

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
