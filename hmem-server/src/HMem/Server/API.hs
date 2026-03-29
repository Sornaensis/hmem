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
import Data.Functor.Contravariant ((>$<))
import Data.Maybe (fromMaybe)
import Data.Pool (Pool, tryWithResource)
import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.UUID (UUID)
import Hasql.Connection qualified as Hasql
import Hasql.Session qualified as Session
import GHC.Generics (Generic)
import Rel8 hiding (Delete)
import Rel8 qualified (Delete(..))
import Servant

import HMem.DB.Category qualified as Cat
import HMem.DB.Cleanup qualified as Cleanup
import HMem.DB.Memory qualified as Mem
import HMem.DB.Pool (runSession, DBException(..))
import HMem.DB.Project qualified as Proj
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

-- Memories
type MemoryAPI =
       QueryParam "workspace_id" UUID
         :> QueryParam "type" MemoryType
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
         :> QueryParam "limit" Int
         :> QueryParam "offset" Int
         :> Get '[JSON] (PaginatedResult Project)
  :<|> ReqBody '[JSON] CreateProject :> Post '[JSON] Project
  :<|> Capture "projectId" UUID :> Get '[JSON] Project
  :<|> Capture "projectId" UUID :> ReqBody '[JSON] UpdateProject :> Put '[JSON] Project
  :<|> Capture "projectId" UUID :> Delete '[JSON] NoContent
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
         :> QueryParam "limit" Int
         :> QueryParam "offset" Int
         :> Get '[JSON] (PaginatedResult Task)
  :<|> ReqBody '[JSON] CreateTask :> Post '[JSON] Task
  :<|> Capture "taskId" UUID :> Get '[JSON] Task
  :<|> Capture "taskId" UUID :> ReqBody '[JSON] UpdateTask :> Put '[JSON] Task
  :<|> Capture "taskId" UUID :> Delete '[JSON] NoContent
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
    Left (DBUniqueViolation detail) -> throwError err409
      { errBody = Aeson.encode $ errorBody "conflict" "Duplicate entry" detail }
    Left (DBForeignKeyViolation detail) -> throwError err400
      { errBody = Aeson.encode $ errorBody "foreign_key" "Referenced entity not found" detail }
    Left (DBCheckViolation detail) -> throwError err400
      { errBody = Aeson.encode $ errorBody "check_violation" "Constraint violation" detail }
    Left (DBCycleDetected detail) -> throwError err409
      { errBody = Aeson.encode $ errorBody "cycle" "Cycle detected" detail }
    Left DBStatementTimeout -> throwError err504
      { errBody = Aeson.encode $ errorBody "timeout" "Statement timeout" "" }
    Left (DBOtherError detail) -> throwError err500
      { errBody = Aeson.encode $ errorBody "internal" "Internal database error" detail }
  where
    errorBody :: Text -> Text -> Text -> Value
    errorBody errType msg detail = object $
      [ "error"   .= errType
      , "message" .= msg
      ] ++ [ "detail" .= detail | not (T.null detail) ]

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
  where
    listWorkspacesH :: Maybe Int -> Maybe Int -> Handler (PaginatedResult Workspace)
    listWorkspacesH mlimit moffset = handleDBErrors $ do
      let lim = capLimit mlimit
          off = capOffset moffset
      rows <- runSession pool $ Session.statement () $ run $ select $
        limit (fromIntegral lim + 1) $ offset (fromIntegral off) $
        orderBy ((\row -> row.wsName) >$< asc) $ each workspaceSchema
      let results = map rowToWorkspace rows
      pure PaginatedResult { items = take lim results, hasMore = length results > lim }

    createWorkspaceH :: CreateWorkspace -> Handler Workspace
    createWorkspaceH cw = handleDBErrors $ do
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
        pure row
      case rows of
        (r:_) -> pure $ rowToWorkspace r
        []    -> throwError err404

    updateWorkspaceH :: UUID -> UpdateWorkspace -> Handler Workspace
    updateWorkspaceH wsId uw = do
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
          , updateWhere = \_ row -> row.wsId ==. lit wsId
          , returning = Returning id
          }
      case rows of
        (r:_) -> pure $ rowToWorkspace r
        []    -> throwError err404

    deleteWorkspaceH :: UUID -> Handler NoContent
    deleteWorkspaceH wsId = do
      n <- handleDBErrors $ runSession pool $ Session.statement () $ runN $
        delete Rel8.Delete
          { from = workspaceSchema
          , using = pure ()
          , deleteWhere = \_ row -> row.wsId ==. lit wsId
          , returning = NoReturning
          }
      if n > 0 then pure NoContent else throwError err404

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
    listMemoriesH mws mtype mlimit moffset mcompact = do
      let lim = capLimit mlimit
          off = capOffset moffset
          compact = mcompact == Just True
      results <- handleDBErrors $ Mem.listMemories pool mws mtype (Just (lim + 1)) (Just off)
      let items = (if compact then map compactMemory else Prelude.id) $ take lim results
      pure PaginatedResult { items = items, hasMore = length results > lim }

    createMemoryH cm = handleDBErrors $ Mem.createMemory pool cm

    createMemoryBatchH cms
      | length cms > 100 = throwError err400 { errBody = "Batch size exceeds maximum of 100" }
      | otherwise = handleDBErrors $ Mem.createMemoryBatch pool cms

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
      -- Buffer access event for periodic batch flush
      liftIO $ trackAccess tracker mid
      mm <- handleDBErrors $ Mem.getMemory pool mid
      maybe (throwError err404) pure mm

    updateMemoryH mid um = do
      mm <- handleDBErrors $ Mem.updateMemory pool mid um
      maybe (throwError err404) pure mm

    deleteMemoryH mid = do
      ok <- handleDBErrors $ Mem.deleteMemory pool mid
      if ok then pure NoContent else throwError err404

    getLinksH mid = handleDBErrors $ Mem.getMemoryLinks pool mid

    createLinkH mid cml = do
      handleDBErrors $ Mem.linkMemories pool mid cml
      pure NoContent

    unlinkH mid tid rt = do
      ok <- handleDBErrors $ Mem.unlinkMemories pool mid tid rt
      if ok then pure NoContent else throwError err404

    getTagsH mid = handleDBErrors $ Mem.getTags pool mid

    setTagsH mid tags = do
      handleDBErrors $ Mem.setTags pool mid tags
      pure NoContent

    graphH mid mdepth = do
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
  :<|> linkMemoryH
  :<|> unlinkMemoryH
  :<|> getProjectMemoriesH
  where
    listProjectsH mws mstatus mlimit moffset = do
      wsId <- requireParam "workspace_id" mws
      let lim = capLimit mlimit
          off = capOffset moffset
      results <- handleDBErrors $ Proj.listProjects pool wsId mstatus (Just (lim + 1)) (Just off)
      pure PaginatedResult { items = take lim results, hasMore = length results > lim }

    createProjectH cp   = handleDBErrors $ Proj.createProject pool cp
    getProjectH pid     = handleDBErrors (Proj.getProject pool pid) >>= maybe (throwError err404) pure
    updateProjectH pid up = handleDBErrors (Proj.updateProject pool pid up) >>= maybe (throwError err404) pure

    deleteProjectH pid = do
      ok <- handleDBErrors $ Proj.deleteProject pool pid
      if ok then pure NoContent else throwError err404

    linkMemoryH pid lm = do
      handleDBErrors $ Proj.linkProjectMemory pool pid lm.memoryId
      pure NoContent

    unlinkMemoryH pid mid = do
      handleDBErrors $ Proj.unlinkProjectMemory pool pid mid
      pure NoContent

    getProjectMemoriesH pid = handleDBErrors $ Mem.getProjectMemories pool pid

-- Task handlers ----------------------------------------------------

taskHandlers :: Pool Hasql.Connection -> Server TaskAPI
taskHandlers pool =
       listTasksH
  :<|> createTaskH
  :<|> getTaskH
  :<|> updateTaskH
  :<|> deleteTaskH
  :<|> linkMemoryH
  :<|> unlinkMemoryH
  :<|> addDepH
  :<|> removeDepH
  :<|> getTaskMemoriesH
  where
    listTasksH mws mpid mstatus mlimit moffset = do
      let lim = capLimit mlimit
          off = capOffset moffset
      results <- case mws of
        Just wsId -> handleDBErrors $ Task.listTasksByWorkspace pool wsId mstatus mpid (Just (lim + 1)) (Just off)
        Nothing -> case mpid of
          Just pid -> handleDBErrors $ Task.listTasks pool pid mstatus (Just (lim + 1)) (Just off)
          Nothing  -> throwError $ err400 { errBody = "Either workspace_id or project_id is required" }
      pure PaginatedResult { items = take lim results, hasMore = length results > lim }

    createTaskH ct   = handleDBErrors $ Task.createTask pool ct
    getTaskH tid     = handleDBErrors (Task.getTask pool tid) >>= maybe (throwError err404) pure
    updateTaskH tid ut = handleDBErrors (Task.updateTask pool tid ut) >>= maybe (throwError err404) pure

    deleteTaskH tid = do
      ok <- handleDBErrors $ Task.deleteTask pool tid
      if ok then pure NoContent else throwError err404

    linkMemoryH tid lm = do
      handleDBErrors $ Task.linkTaskMemory pool tid lm.memoryId
      pure NoContent

    unlinkMemoryH tid mid = do
      handleDBErrors $ Task.unlinkTaskMemory pool tid mid
      pure NoContent

    addDepH tid ld = do
      handleDBErrors $ Task.addDependency pool tid ld.dependsOnId
      pure NoContent

    removeDepH tid depId = do
      handleDBErrors $ Task.removeDependency pool tid depId
      pure NoContent

    getTaskMemoriesH tid = handleDBErrors $ Mem.getTaskMemories pool tid

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
  :<|> linkH
  :<|> unlinkH
  where
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

    createCategoryH cc = handleDBErrors $ Cat.createCategory pool cc
    getCategoryH cid   = handleDBErrors (Cat.getCategory pool cid) >>= maybe (throwError err404) pure
    updateCategoryH cid uc = handleDBErrors (Cat.updateCategory pool cid uc) >>= maybe (throwError err404) pure

    deleteCategoryH cid = do
      ok <- handleDBErrors $ Cat.deleteCategory pool cid
      if ok then pure NoContent else throwError err404

    linkH cl = do
      handleDBErrors $ Cat.linkMemoryCategory pool cl.memoryId cl.categoryId
      pure NoContent

    unlinkH cl = do
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

    createGroupH cg = handleDBErrors $ WG.createGroup pool cg

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

-- | Clamp a user-supplied limit to [1, 200], defaulting to 50.
capLimit :: Maybe Int -> Int
capLimit = Prelude.min 200 . Prelude.max 1 . fromMaybe 50

-- | Clamp a user-supplied offset to [0, 10000], defaulting to 0.
capOffset :: Maybe Int -> Int
capOffset = Prelude.min 10000 . Prelude.max 0 . fromMaybe 0
