{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | The public HTTP contract.  Observations are deliberately a small,
-- repository-scoped immutable-provenance resource; the old memory graph and
-- categorisation API is intentionally not represented here.
module HMem.Server.API
  ( HMemAPI
  , ObservationEmbedding(..)
  , server
  ) where

import Control.Exception (try)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value, object, (.=), ToJSON(..))
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as AesonKey
import Data.Aeson.KeyMap qualified as AesonKeyMap
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Maybe (fromMaybe)
import Data.Pool (Pool, tryWithResource)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (UTCTime, getCurrentTime)
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Hasql.Connection qualified as Hasql
import Hasql.Session qualified as Session
import Rel8 hiding (Delete)
import Servant
import System.IO (stderr)

import HMem.Config qualified as Config
import HMem.DB.Audit qualified as Audit
import HMem.DB.Auth qualified as Auth
import HMem.DB.Observation qualified as Observation
import HMem.DB.Overview qualified as Overview
import HMem.DB.Pool (DBException(..), PoolMetrics(..), getPoolMetrics, runSession)
import HMem.DB.Project qualified as Project
import HMem.DB.RequestContext (Principal(..), PrincipalAuthority(..), currentPrincipal, currentRequestId, actorTypeToText, withWorkspaceIdContext)
import HMem.DB.Schema
import HMem.DB.Search qualified as Search
import HMem.DB.Task qualified as Task
import HMem.DB.Workspace qualified as Workspace
import HMem.DB.WorkspaceGroup qualified as WorkspaceGroup
import HMem.Server.AccessTracker (AccessTracker, bufferSize)
import HMem.Server.Event (Broadcast, ChangeEvent(..), ChangeType(..), EntityType(..))
import HMem.Server.WebSocket qualified as WS
import HMem.Types

------------------------------------------------------------------------
-- API type
------------------------------------------------------------------------

type HMemAPI = "api" :> "v1" :>
  (    "health"       :> Get '[JSON] Value
  :<|> "session"      :> QueryParam "workspace_id" UUID :> Get '[JSON] SessionContext
  :<|> "workspaces"   :> WorkspaceAPI
  :<|> "groups"       :> WorkspaceGroupAPI
  :<|> "observations" :> ObservationAPI
  :<|> "projects"     :> ProjectAPI
  :<|> "tasks"        :> TaskAPI
  :<|> "search"       :> ReqBody '[JSON] UnifiedSearchQuery :> Post '[JSON] UnifiedSearchResults
  :<|> "audit"        :> AuditAPI
  :<|> "ws-ticket"    :> ReqBody '[JSON] WebSocketTicketRequest :> Post '[JSON] WebSocketTicketResponse
  )

type WorkspaceAPI =
       QueryParam "limit" Int :> QueryParam "offset" Int :> Get '[JSON] (PaginatedResult Workspace)
  :<|> ReqBody '[JSON] CreateWorkspace :> Post '[JSON] Workspace
  :<|> Capture "workspaceId" UUID :> Get '[JSON] Workspace

type WorkspaceGroupAPI =
       QueryParam "limit" Int :> QueryParam "offset" Int :> Get '[JSON] (PaginatedResult WorkspaceGroup)
  :<|> ReqBody '[JSON] CreateWorkspaceGroup :> Post '[JSON] WorkspaceGroup
  :<|> Capture "groupId" UUID :> Get '[JSON] WorkspaceGroup
  :<|> Capture "groupId" UUID :> Delete '[JSON] NoContent
  :<|> Capture "groupId" UUID :> "members" :> Get '[JSON] [UUID]
  :<|> Capture "groupId" UUID :> "members" :> ReqBody '[JSON] WorkspaceGroupMemberInput :> Post '[JSON] NoContent
  :<|> Capture "groupId" UUID :> "members" :> Capture "workspaceId" UUID :> Delete '[JSON] NoContent

type ObservationAPI =
       QueryParam' '[Required] "workspace_id" UUID
         :> QueryParam "subject_kind" SubjectKind
         :> QueryParam "subject" Text
         :> QueryParam "git_sha" Text
         :> QueryParam "query" Text
         :> QueryParam "limit" Int :> QueryParam "offset" Int
         :> Get '[JSON] (PaginatedResult Observation)
  :<|> ReqBody '[JSON] CreateObservation :> Post '[JSON] Observation
  :<|> "similar" :> ReqBody '[JSON] SimilarObservationQuery :> Post '[JSON] [SimilarObservation]
  :<|> Capture "observationId" UUID :> Get '[JSON] Observation
  :<|> Capture "observationId" UUID :> ReqBody '[JSON] UpdateObservation :> Put '[JSON] Observation
  :<|> Capture "observationId" UUID :> Delete '[JSON] NoContent
  :<|> Capture "observationId" UUID :> "embedding" :> ReqBody '[JSON] ObservationEmbedding :> Put '[JSON] NoContent

type ProjectAPI =
       QueryParam "workspace_id" UUID :> QueryParam "status" ProjectStatus :> QueryParam "query" Text
         :> QueryParam "limit" Int :> QueryParam "offset" Int :> Get '[JSON] (PaginatedResult Project)
  :<|> ReqBody '[JSON] CreateProject :> Post '[JSON] Project
  :<|> Capture "projectId" UUID :> Get '[JSON] Project
  :<|> Capture "projectId" UUID :> ReqBody '[JSON] UpdateProject :> Put '[JSON] Project
  :<|> Capture "projectId" UUID :> Delete '[JSON] CascadeResult
  :<|> Capture "projectId" UUID :> "overview" :> Get '[JSON] ProjectOverview
  :<|> Capture "projectId" UUID :> "next-tasks" :> QueryParam "limit" Int :> QueryParam "include_blocked" Bool :> Get '[JSON] [NextTaskCandidate]

type TaskAPI =
       QueryParam "workspace_id" UUID :> QueryParam "project_id" UUID :> QueryParam "status" TaskStatus
         :> QueryParam "priority" Int :> QueryParam "query" Text :> QueryParam "limit" Int :> QueryParam "offset" Int
         :> Get '[JSON] (PaginatedResult Task)
  :<|> ReqBody '[JSON] CreateTask :> Post '[JSON] Task
  :<|> Capture "taskId" UUID :> Get '[JSON] Task
  :<|> Capture "taskId" UUID :> ReqBody '[JSON] UpdateTask :> Put '[JSON] TaskMutationResult
  :<|> Capture "taskId" UUID :> Delete '[JSON] CascadeResult
  :<|> Capture "taskId" UUID :> "overview" :> Get '[JSON] TaskOverview

type AuditAPI =
       QueryParam "workspace_id" UUID :> QueryParam "entity_type" Text :> QueryParam "entity_id" Text
         :> QueryParam "action" AuditAction :> QueryParam "since" UTCTime :> QueryParam "until" UTCTime
         :> QueryParam "limit" Int :> QueryParam "offset" Int :> Get '[JSON] (PaginatedResult AuditLogEntry)
  :<|> Capture "auditId" UUID :> "revert" :> Post '[JSON] RevertResult
  :<|> Capture "auditId" UUID :> Get '[JSON] AuditLogEntry

instance FromHttpApiData SubjectKind where
  parseQueryParam value = maybe (Left "subject_kind must be file or glob") Right (subjectKindFromText value)
instance FromHttpApiData ProjectStatus where
  parseQueryParam value = maybe (Left "invalid project status") Right (projectStatusFromText value)
instance FromHttpApiData TaskStatus where
  parseQueryParam value = maybe (Left "invalid task status") Right (taskStatusFromText value)
instance FromHttpApiData AuditAction where
  parseQueryParam value = maybe (Left "invalid audit action") Right (auditActionFromText value)

------------------------------------------------------------------------
-- Shared helpers
------------------------------------------------------------------------

handleDBErrors :: IO a -> Handler a
handleDBErrors action = do
  result <- liftIO (try action)
  case result of
    Right value -> pure value
    Left exception -> do
      liftIO $ LBS8.hPutStrLn stderr (Aeson.encode $ object ["level" .= ("error" :: Text), "error" .= show (exception :: DBException)])
      throwError $ case exception of
        DBUniqueViolation{} -> badRequest "conflict" "Resource already exists"
        DBForeignKeyViolation{} -> badRequest "invalid_reference" "Referenced resource does not exist"
        DBCheckViolation{} -> badRequest "invalid_request" "Request violates a data constraint"
        DBCapabilityUnavailable{} -> err503 { errBody = Aeson.encode (object ["error" .= ("capability_unavailable" :: Text), "message" .= ("pgvector embedding support is unavailable" :: Text)]) }
        DBStatementTimeout -> err504 { errBody = Aeson.encode (object ["error" .= ("timeout" :: Text)]) }
        _ -> err500 { errBody = Aeson.encode (object ["error" .= ("internal" :: Text)]) }

badRequest :: Text -> Text -> ServerError
badRequest kind message = err400 { errBody = Aeson.encode (object ["error" .= kind, "message" .= message]) }

reject :: [Text] -> Handler ()
reject [] = pure ()
reject errors = throwError $ badRequest "validation_error" (fromMaybe "Invalid request" (safeHead errors))
  where safeHead [] = Nothing; safeHead (x:_) = Just x

requireSuperadmin :: Pool Hasql.Connection -> Handler ()
requireSuperadmin pool = do
  principal <- liftIO currentPrincipal
  allowed <- liftIO $ Auth.authorizeGlobal pool principal Auth.GlobalSuperadmin
  either (throwError . authError) pure allowed

requireWorkspace :: Pool Hasql.Connection -> UUID -> Auth.WorkspaceRole -> Handler ()
requireWorkspace pool workspaceId role = do
  principal <- liftIO currentPrincipal
  authorized <- liftIO $ Auth.authorizeWorkspace pool principal workspaceId role
  either (throwError . authError) pure authorized
  rows <- handleDBErrors $ runSession pool $ Session.statement () $ run $ select $ do
    row <- each workspaceSchema
    where_ $ row.wsId ==. lit workspaceId
    where_ $ activeWorkspace row
    pure row.wsId
  case rows of [] -> throwError err404; _ -> pure ()

-- | Observation operations are valid only while their repository workspace is
-- active.  This is intentionally stricter than generic workspace auth.
requireObservationWorkspace :: Pool Hasql.Connection -> UUID -> Auth.WorkspaceRole -> Handler ()
requireObservationWorkspace pool workspaceId role = do
  requireWorkspace pool workspaceId role
  rows <- handleDBErrors $ runSession pool $ Session.statement () $ run $ select $ do
    row <- each workspaceSchema
    where_ $ row.wsId ==. lit workspaceId
    where_ $ row.wsType ==. lit WsRepository
    where_ $ activeWorkspace row
    pure row.wsId
  case rows of [] -> throwError err404; _ -> pure ()

requireEntity :: Pool Hasql.Connection -> Auth.EntityKind -> UUID -> Auth.WorkspaceRole -> Handler UUID
requireEntity pool kind entityId role = do
  resolved <- liftIO $ Auth.resolveEntityScopeRequired pool kind entityId
  scope <- either (throwError . authError) pure resolved
  principal <- liftIO currentPrincipal
  allowed <- liftIO $ Auth.authorizeScope pool principal scope role
  either (throwError . authError) pure allowed
  case scope of Auth.EntityWorkspaceScope workspaceId -> pure workspaceId; Auth.EntityGlobalScope -> throwError err404

authError :: Auth.AuthorizationError -> ServerError
authError = \case
  Auth.MissingPrincipal -> err401
  Auth.EntityScopeNotFound{} -> err404
  _ -> err403

emit :: Maybe UUID -> Broadcast -> ChangeType -> EntityType -> UUID -> Maybe Value -> Handler ()
emit workspaceId broadcast change entity entityId payload = liftIO $ do
  now <- getCurrentTime
  requestId <- currentRequestId
  principal <- currentPrincipal
  broadcast ChangeEvent
    { changeType = change, entityType = entity, entityId = entityId, workspaceId = workspaceId
    , timestamp = now, requestId = requestId
    , actorType = fmap (\(p :: Principal) -> actorTypeToText p.actorType) principal
    , actorId = fmap (\(p :: Principal) -> p.actorId) principal
    , actorLabel = fmap (\(p :: Principal) -> p.actorLabel) principal, payload = payload }

page :: Maybe Int -> Maybe Int -> (Int, Int)
page = capPagination

------------------------------------------------------------------------
-- Handlers
------------------------------------------------------------------------

server :: Config.AuthConfig -> Pool Hasql.Connection -> AccessTracker -> Broadcast -> WS.WSState -> Bool -> Server HMemAPI
server authConfig pool tracker broadcast wsState _ =
       health pool tracker
  :<|> session authConfig pool
  :<|> workspaces pool
  :<|> groups pool broadcast
  :<|> observations pool broadcast
  :<|> projects pool broadcast
  :<|> tasks pool broadcast
  :<|> search pool
  :<|> audit pool broadcast
  :<|> ticket pool wsState

session :: Config.AuthConfig -> Pool Hasql.Connection -> Maybe UUID -> Handler SessionContext
session config pool selectedWorkspace = do
  principal <- liftIO currentPrincipal >>= maybe (throwError err401) pure
  createAllowed <- liftIO $ Auth.hasGlobalPermission pool (Just principal) Auth.GlobalCreateWorkspace
  superAllowed <- liftIO $ Auth.hasGlobalPermission pool (Just principal) Auth.GlobalSuperadmin
  workspace <- traverse (sessionWorkspace pool principal superAllowed) selectedWorkspace
  pure SessionContext
    { authMode = case config.mode of Config.AuthModeLocal -> "local"; Config.AuthModeDeployed -> "deployed"
    , principal = SessionPrincipal { actorType = actorTypeToText principal.actorType, actorId = principal.actorId
                                   , actorLabel = principal.actorLabel, authority = authorityText principal.authority
                                   , grantUserId = case principal.authority of PrincipalGrantUser userId -> Just userId; _ -> Nothing }
    , globalPermissions = SessionGlobalPermissions { createWorkspace = createAllowed, superadmin = superAllowed }
    , workspace = workspace }
  where
    authorityText PrincipalNoAuthority = "none"
    authorityText PrincipalGrantUser{} = "grant_user"
    authorityText PrincipalSyntheticLocalSuperadmin = "local_superadmin"

sessionWorkspace :: Pool Hasql.Connection -> Principal -> Bool -> UUID -> Handler SessionWorkspaceContext
sessionWorkspace pool principal isSuperadmin workspaceId = do
  requireWorkspace pool workspaceId Auth.WorkspaceRoleRead
  storedRole <- liftIO $ case principal.authority of PrincipalGrantUser userId -> Auth.getWorkspaceRole pool workspaceId userId; _ -> pure Nothing
  let effective = if isSuperadmin then Just Auth.WorkspaceRoleAdmin else storedRole
      can required = maybe False (`Auth.roleSatisfies` required) effective
  pure SessionWorkspaceContext { workspaceId = workspaceId, role = Auth.roleToText <$> effective
                               , canRead = can Auth.WorkspaceRoleRead, canEdit = can Auth.WorkspaceRoleEdit
                               , canAdmin = can Auth.WorkspaceRoleAdmin }

health :: Pool Hasql.Connection -> AccessTracker -> Handler Value
health pool tracker = do
  database <- liftIO $ tryWithResource pool (\connection -> Session.run (Session.sql "SELECT 1") connection)
  buffered <- liftIO $ bufferSize tracker
  metrics <- liftIO getPoolMetrics
  pure $ object [ "status" .= (case database of Just (Right _) -> ("ok" :: Text); _ -> "degraded")
                , "access_tracker" .= object ["buffered_count" .= buffered]
                , "pool" .= object ["active_connections" .= metrics.activeConnections, "max_connections" .= metrics.maxConnections] ]

workspaces :: Pool Hasql.Connection -> Server WorkspaceAPI
workspaces pool = listH :<|> createH :<|> getH where
  listH limit offset = do
    principal <- liftIO currentPrincipal
    -- A caller without a principal cannot observe any workspace, including its names.
    case principal of Nothing -> throwError err401; Just _ -> pure ()
    let (takeN, skipN) = page limit offset
    allRows <- handleDBErrors $ Workspace.listActiveWorkspaces pool (takeN + 1) skipN
    pure PaginatedResult { items = take takeN allRows, hasMore = length allRows > takeN }
  createH input = do
    principal <- liftIO currentPrincipal
    allowed <- liftIO $ Auth.authorizeGlobal pool principal Auth.GlobalCreateWorkspace
    either (throwError . authError) pure allowed
    reject (validateCreateWorkspaceInput input)
    rows <- handleDBErrors $ runSession pool $ Session.statement () $ run $ insert Insert
      { into = workspaceSchema
      , rows = values [WorkspaceT
          { wsId = unsafeDefault, wsName = lit input.name
          , wsGhOwner = lit input.ghOwner, wsGhRepo = lit input.ghRepo
          , wsType = lit (fromMaybe WsRepository input.workspaceType)
          , wsDeletedAt = unsafeDefault, wsCreatedAt = unsafeDefault, wsUpdatedAt = unsafeDefault }]
      , onConflict = Abort, returning = Returning id }
    case rows of (row:_) -> pure Workspace { id = row.wsId, name = row.wsName, ghOwner = row.wsGhOwner, ghRepo = row.wsGhRepo, workspaceType = row.wsType, createdAt = row.wsCreatedAt, updatedAt = row.wsUpdatedAt }; [] -> throwError err500
  getH workspaceId = do
    requireWorkspace pool workspaceId Auth.WorkspaceRoleRead
    rows <- handleDBErrors $ runSession pool $ Session.statement () $ run $ select $ do
      row <- each workspaceSchema; where_ (row.wsId ==. lit workspaceId &&. activeWorkspace row); pure row
    case rows of (row:_) -> pure Workspace { id = row.wsId, name = row.wsName, ghOwner = row.wsGhOwner, ghRepo = row.wsGhRepo, workspaceType = row.wsType, createdAt = row.wsCreatedAt, updatedAt = row.wsUpdatedAt }; [] -> throwError err404

groups :: Pool Hasql.Connection -> Broadcast -> Server WorkspaceGroupAPI
groups pool broadcast = listH :<|> createH :<|> getH :<|> deleteH :<|> listMembersH :<|> addMemberH :<|> removeMemberH where
  listH limit offset = do
    requireSuperadmin pool
    let (takeN, skipN) = page limit offset
    rows <- handleDBErrors $ WorkspaceGroup.listGroups pool (Just (takeN + 1)) (Just skipN)
    pure PaginatedResult { items = take takeN rows, hasMore = length rows > takeN }
  createH input = do
    requireSuperadmin pool
    reject (validateCreateWorkspaceGroupInput input)
    created <- handleDBErrors $ WorkspaceGroup.createGroup pool input
    emit Nothing broadcast Created ETWorkspaceGroup created.id (Just (toJSON created))
    pure created
  getH groupId = do
    requireSuperadmin pool
    handleDBErrors (WorkspaceGroup.getGroup pool groupId) >>= maybe (throwError err404) pure
  deleteH groupId = do
    requireSuperadmin pool
    deleted <- handleDBErrors $ WorkspaceGroup.deleteGroup pool groupId
    if deleted then emit Nothing broadcast Deleted ETWorkspaceGroup groupId Nothing >> pure NoContent else throwError err404
  listMembersH groupId = do
    requireSuperadmin pool
    _ <- handleDBErrors (WorkspaceGroup.getGroup pool groupId) >>= maybe (throwError err404) pure
    handleDBErrors $ WorkspaceGroup.listGroupMembers pool groupId
  addMemberH groupId input = do
    requireSuperadmin pool
    _ <- handleDBErrors (WorkspaceGroup.getGroup pool groupId) >>= maybe (throwError err404) pure
    memberResult <- handleDBErrors $ WorkspaceGroup.addMember pool groupId input.workspaceId
    case memberResult of
      WorkspaceGroup.MemberAdded -> emit (Just input.workspaceId) broadcast Updated ETWorkspaceGroup groupId Nothing
      WorkspaceGroup.MemberAlreadyPresent -> pure ()
      WorkspaceGroup.MemberWorkspaceInactive -> throwError err404
    pure NoContent
  removeMemberH groupId workspaceId = do
    requireSuperadmin pool
    _ <- handleDBErrors (WorkspaceGroup.getGroup pool groupId) >>= maybe (throwError err404) pure
    requireWorkspace pool workspaceId Auth.WorkspaceRoleRead
    removed <- handleDBErrors $ WorkspaceGroup.removeMember pool groupId workspaceId
    if removed then emit (Just workspaceId) broadcast Updated ETWorkspaceGroup groupId Nothing else pure ()
    pure NoContent

observations :: Pool Hasql.Connection -> Broadcast -> Server ObservationAPI
observations pool broadcast = listH :<|> createH :<|> similarH :<|> getH :<|> updateH :<|> deleteH :<|> embeddingH where
  listH workspaceId kind subjectValue sha queryValue limit offset = do
    requireObservationWorkspace pool workspaceId Auth.WorkspaceRoleRead
    -- Validate client-supplied values before pagination defaults/caps are
    -- applied; otherwise invalid bounds would be silently normalized.
    let rawQuery = ObservationQuery workspaceId kind subjectValue sha queryValue limit offset
    reject (validateObservationQuery rawQuery)
    let (takeN, skipN) = page limit offset
        query = ObservationQuery workspaceId kind subjectValue sha queryValue (Just takeN) (Just skipN)
    rows <- handleDBErrors $ Observation.listObservationsOverfetch pool query
    pure PaginatedResult { items = take takeN rows, hasMore = length rows > takeN }
  createH input = do
    requireObservationWorkspace pool input.workspaceId Auth.WorkspaceRoleEdit
    reject (validateCreateObservationInput input)
    created <- handleDBErrors $ withWorkspaceIdContext (Just input.workspaceId) (Observation.createObservation pool input)
    emit (Just input.workspaceId) broadcast Created ETObservation created.id (Just (toJSON created))
    pure created
  similarH query = do
    requireObservationWorkspace pool query.workspaceId Auth.WorkspaceRoleRead
    reject (validateSimilarObservationQuery query)
    handleDBErrors $ Observation.similarObservations pool query
  getH observationId = do
    workspaceId <- requireEntity pool Auth.EntityObservation observationId Auth.WorkspaceRoleRead
    requireObservationWorkspace pool workspaceId Auth.WorkspaceRoleRead
    handleDBErrors (Observation.getObservation pool workspaceId observationId) >>= maybe (throwError err404) pure
  updateH observationId input = do
    workspaceId <- requireEntity pool Auth.EntityObservation observationId Auth.WorkspaceRoleEdit
    requireObservationWorkspace pool workspaceId Auth.WorkspaceRoleEdit
    reject (validateUpdateObservationInput input)
    updated <- handleDBErrors (Observation.updateObservation pool workspaceId observationId input) >>= maybe (throwError err404) pure
    emit (Just workspaceId) broadcast Updated ETObservation observationId (Just (toJSON updated))
    pure updated
  deleteH observationId = do
    workspaceId <- requireEntity pool Auth.EntityObservation observationId Auth.WorkspaceRoleEdit
    requireObservationWorkspace pool workspaceId Auth.WorkspaceRoleEdit
    deleted <- handleDBErrors (Observation.deleteObservation pool workspaceId observationId)
    if deleted then emit (Just workspaceId) broadcast Deleted ETObservation observationId Nothing >> pure NoContent else throwError err404
  embeddingH observationId (ObservationEmbedding vector) = do
    workspaceId <- requireEntity pool Auth.EntityObservation observationId Auth.WorkspaceRoleEdit
    requireObservationWorkspace pool workspaceId Auth.WorkspaceRoleEdit
    -- Verify existence so a no-op UPDATE is never reported as success.
    _ <- handleDBErrors (Observation.getObservation pool workspaceId observationId) >>= maybe (throwError err404) pure
    handleDBErrors $ Observation.setObservationEmbedding pool workspaceId observationId vector
    emit (Just workspaceId) broadcast Updated ETObservation observationId Nothing
    pure NoContent

projects :: Pool Hasql.Connection -> Broadcast -> Server ProjectAPI
projects pool broadcast = listH :<|> createH :<|> getH :<|> updateH :<|> deleteH :<|> overviewH :<|> nextH where
  listH workspaceId status queryValue limit offset = do
    workspace <- maybe (throwError err403) pure workspaceId
    requireWorkspace pool workspace Auth.WorkspaceRoleRead
    let (takeN, skipN) = page limit offset
        query = ProjectListQuery workspaceId status queryValue Nothing Nothing Nothing Nothing Nothing (Just (takeN + 1)) (Just skipN)
    reject (validateProjectListQuery query)
    rows <- handleDBErrors $ Project.listProjectsWithQuery pool query
    pure PaginatedResult { items = take takeN rows, hasMore = length rows > takeN }
  createH input = do
    requireWorkspace pool input.workspaceId Auth.WorkspaceRoleEdit; reject (validateCreateProjectInput input)
    created <- handleDBErrors $ Project.createProject pool input
    emit (Just input.workspaceId) broadcast Created ETProject created.id (Just (toJSON created)); pure created
  getH projectId = do
    _ <- requireEntity pool Auth.EntityProject projectId Auth.WorkspaceRoleRead
    handleDBErrors (Project.getProject pool projectId) >>= maybe (throwError err404) pure
  updateH projectId input = do
    workspaceId <- requireEntity pool Auth.EntityProject projectId Auth.WorkspaceRoleEdit; reject (validateUpdateProjectInput input)
    updated <- handleDBErrors (Project.updateProject pool projectId input) >>= maybe (throwError err404) pure
    emit (Just workspaceId) broadcast Updated ETProject projectId (Just (toJSON updated)); pure updated
  deleteH projectId = do
    workspaceId <- requireEntity pool Auth.EntityProject projectId Auth.WorkspaceRoleEdit
    handleDBErrors (Project.deleteProjectCascade pool projectId) >>= maybe (throwError err404) (\result -> emit (Just workspaceId) broadcast Deleted ETProject projectId (Just (toJSON result)) >> pure result)
  overviewH projectId = do
    _ <- requireEntity pool Auth.EntityProject projectId Auth.WorkspaceRoleRead
    handleDBErrors (Overview.getProjectOverview pool projectId) >>= maybe (throwError err404) pure
  nextH projectId limit includeBlocked = do
    _ <- requireEntity pool Auth.EntityProject projectId Auth.WorkspaceRoleRead
    handleDBErrors $ Task.listNextTasks pool projectId (fromMaybe False includeBlocked) (fromMaybe 5 limit)

tasks :: Pool Hasql.Connection -> Broadcast -> Server TaskAPI
tasks pool broadcast = listH :<|> createH :<|> getH :<|> updateH :<|> deleteH :<|> overviewH where
  listH workspaceId projectId status priority queryValue limit offset = do
    workspace <- case (workspaceId, projectId) of
      (Just id, _) -> requireWorkspace pool id Auth.WorkspaceRoleRead >> pure id
      (Nothing, Just project) -> requireEntity pool Auth.EntityProject project Auth.WorkspaceRoleRead
      (Nothing, Nothing) -> throwError err403
    let (takeN, skipN) = page limit offset
        query = TaskListQuery (Just workspace) projectId status priority queryValue Nothing Nothing Nothing Nothing Nothing (Just (takeN + 1)) (Just skipN)
    reject (validateTaskListQuery query)
    rows <- handleDBErrors $ Task.listTasksWithQuery pool query
    pure PaginatedResult { items = take takeN rows, hasMore = length rows > takeN }
  createH input = do
    requireWorkspace pool input.workspaceId Auth.WorkspaceRoleEdit; reject (validateCreateTaskInput input)
    created <- handleDBErrors $ Task.createTask pool input
    emit (Just input.workspaceId) broadcast Created ETTask created.id (Just (toJSON created)); pure created
  getH taskId = do
    _ <- requireEntity pool Auth.EntityTask taskId Auth.WorkspaceRoleRead
    handleDBErrors (Task.getTask pool taskId) >>= maybe (throwError err404) pure
  updateH taskId input = do
    workspaceId <- requireEntity pool Auth.EntityTask taskId Auth.WorkspaceRoleEdit; reject (validateUpdateTaskInput input)
    result <- handleDBErrors (Task.updateTaskWithDependencySnapshots pool taskId input) >>= maybe (throwError err404) pure
    let (updated, before, after) = result
        effects = [] -- snapshots are persisted by core; task updates do not expose Memory effects.
    emit (Just workspaceId) broadcast Updated ETTask taskId (Just (toJSON updated))
    pure TaskMutationResult { task = updated, dependencyEffects = effects }
  deleteH taskId = do
    workspaceId <- requireEntity pool Auth.EntityTask taskId Auth.WorkspaceRoleEdit
    handleDBErrors (Task.deleteTaskCascade pool taskId) >>= maybe (throwError err404) (\result -> emit (Just workspaceId) broadcast Deleted ETTask taskId (Just (toJSON result)) >> pure result)
  overviewH taskId = do
    _ <- requireEntity pool Auth.EntityTask taskId Auth.WorkspaceRoleRead
    handleDBErrors (Overview.getTaskOverview pool taskId) >>= maybe (throwError err404) pure

search :: Pool Hasql.Connection -> UnifiedSearchQuery -> Handler UnifiedSearchResults
search pool query = do
  reject (validateUnifiedSearchQuery query)
  workspaceId <- maybe (throwError err403) pure query.workspaceId
  if searchesObservations query
    then requireObservationWorkspace pool workspaceId Auth.WorkspaceRoleRead
    else requireWorkspace pool workspaceId Auth.WorkspaceRoleRead
  handleDBErrors $ Search.searchAll pool query
  where
    -- Omitting entity_types includes observations, so it is an observation
    -- operation and must use the same repository scope guard as every other
    -- Observation endpoint.
    searchesObservations searchQuery =
      SearchObservation `elem` fromMaybe [SearchObservation, SearchProject, SearchTask] searchQuery.entityTypes

audit :: Pool Hasql.Connection -> Broadcast -> Server AuditAPI
audit pool broadcast = listH :<|> revertH :<|> getH where
  listH workspaceId entityType entityId action since until limit offset = do
    case workspaceId of
      Just id
        | entityType == Just "observation" -> requireObservationWorkspace pool id Auth.WorkspaceRoleAdmin
        | otherwise -> requireWorkspace pool id Auth.WorkspaceRoleAdmin
      Nothing -> requireSuperadmin pool
    let (takeN, skipN) = page limit offset
        query = AuditLogQuery workspaceId entityType entityId action since until (Just (takeN + 1)) (Just skipN)
    rows <- handleDBErrors $ Audit.getAuditLog pool query
    pure PaginatedResult { items = take takeN rows, hasMore = length rows > takeN }
  getH auditId = do
    entry <- handleDBErrors (Audit.getAuditEntry pool auditId) >>= maybe (throwError err404) pure
    _ <- requireAuditEntryWorkspace entry
    pure entry
  revertH auditId = do
    entry <- handleDBErrors (Audit.getAuditEntry pool auditId) >>= maybe (throwError err404) pure
    workspaceId <- requireAuditEntryWorkspace entry
    entityId <- maybe (throwError err409) pure (UUID.fromText entry.entityId)
    entity <- case (entry.entityType, entry.action) of
      ("project", AuditUpdate) | isSoftDelete entry -> restoreProject workspaceId entityId
      ("project", AuditUpdate) -> do
        old <- decodeOld entry
        handleDBErrors (Project.updateProject pool entityId old) >>= maybe (throwError err409) (\project -> emit (Just workspaceId) broadcast Updated ETProject entityId (Just (toJSON project)) >> pure (Just (toJSON project)))
      ("project", AuditDelete) -> do
        restored <- handleDBErrors $ Project.restoreProject pool entityId
        if restored then do project <- handleDBErrors (Project.getProject pool entityId); emit (Just workspaceId) broadcast Updated ETProject entityId (toJSON <$> project); pure (toJSON <$> project) else throwError err409
      ("project", AuditCreate) -> do
        deleted <- handleDBErrors $ Project.deleteProjectCascade pool entityId
        case deleted of Nothing -> throwError err409; Just _ -> emit (Just workspaceId) broadcast Deleted ETProject entityId Nothing >> pure Nothing
      ("task", AuditUpdate) | isSoftDelete entry -> restoreTask workspaceId entityId
      ("task", AuditUpdate) -> do
        old <- decodeOld entry
        handleDBErrors (Task.updateTask pool entityId old) >>= maybe (throwError err409) (\task -> emit (Just workspaceId) broadcast Updated ETTask entityId (Just (toJSON task)) >> pure (Just (toJSON task)))
      ("task", AuditDelete) -> do
        restored <- handleDBErrors $ Task.restoreTask pool entityId
        if restored then do task <- handleDBErrors (Task.getTask pool entityId); emit (Just workspaceId) broadcast Updated ETTask entityId (toJSON <$> task); pure (toJSON <$> task) else throwError err409
      ("task", AuditCreate) -> do
        deleted <- handleDBErrors $ Task.deleteTaskCascade pool entityId
        case deleted of Nothing -> throwError err409; Just _ -> emit (Just workspaceId) broadcast Deleted ETTask entityId Nothing >> pure Nothing
      -- Observation records are hard-deleted and their provenance is
      -- immutable, so audit replay cannot safely recreate or alter them.
      ("observation", _) -> throwError unsupportedObservation
      _ -> throwError err409
    pure RevertResult { auditEntry = entry, entity = entity }
  decodeOld :: Aeson.FromJSON a => AuditLogEntry -> Handler a
  decodeOld entry = case entry.oldValues >>= Aeson.decode . Aeson.encode of
    Just value -> pure value
    Nothing -> throwError err409
  restoreProject workspace projectId = do
    restored <- handleDBErrors $ Project.restoreProject pool projectId
    if restored
      then do
        project <- handleDBErrors (Project.getProject pool projectId)
        emit (Just workspace) broadcast Updated ETProject projectId (toJSON <$> project)
        pure (toJSON <$> project)
      else throwError err409
  restoreTask workspace taskId = do
    restored <- handleDBErrors $ Task.restoreTask pool taskId
    if restored
      then do
        task <- handleDBErrors (Task.getTask pool taskId)
        emit (Just workspace) broadcast Updated ETTask taskId (toJSON <$> task)
        pure (toJSON <$> task)
      else throwError err409
  isSoftDelete auditEntry =
    auditField "deleted_at" auditEntry.oldValues == Just Aeson.Null
      && maybe False (/= Aeson.Null) (auditField "deleted_at" auditEntry.newValues)
  auditField key = (>>= \case Aeson.Object values -> AesonKeyMap.lookup (AesonKey.fromText key) values; _ -> Nothing)
  requireAuditEntryWorkspace entry = do
    workspace <- maybe (throwError err403) pure entry.workspaceId
    -- Observations are hard-deleted.  Their audit trail must remain readable
    -- and explicitly non-revertible after deletion, so its recorded workspace
    -- is the authoritative scope; the repository guard still prevents stale
    -- or non-repository workspace access.
    if entry.entityType == "observation"
      then requireObservationWorkspace pool workspace Auth.WorkspaceRoleAdmin
      else do
        entityId <- maybe (throwError err404) pure (UUID.fromText entry.entityId)
        kind <- case entry.entityType of
          "workspace" -> pure Auth.EntityWorkspace
          "project" -> pure Auth.EntityProject
          "task" -> pure Auth.EntityTask
          _ -> throwError err404
        actual <- liftIO $ case kind of
          Auth.EntityProject -> Auth.resolveEntityScopeIncludingDeleted pool kind entityId
          Auth.EntityTask -> Auth.resolveEntityScopeIncludingDeleted pool kind entityId
          _ -> Auth.resolveEntityScope pool kind entityId
        case actual of
          Just (Auth.EntityWorkspaceScope actualWorkspace) | actualWorkspace == workspace -> pure ()
          _ -> throwError err404
        requireWorkspace pool workspace Auth.WorkspaceRoleAdmin
    pure workspace
  unsupportedObservation = err409 { errBody = Aeson.encode (object ["error" .= ("unsupported_entity" :: Text), "message" .= ("Observation audit entries cannot be reverted" :: Text)]) }

ticket :: Pool Hasql.Connection -> WS.WSState -> WebSocketTicketRequest -> Handler WebSocketTicketResponse
ticket pool state request = do
  requireWorkspace pool request.workspaceId Auth.WorkspaceRoleRead
  principal <- liftIO currentPrincipal >>= maybe (throwError err401) pure
  receivesGlobalGroupEvents <- liftIO $ Auth.hasGlobalPermission pool (Just principal) Auth.GlobalSuperadmin
  liftIO $ WS.createTicket state principal request.workspaceId receivesGlobalGroupEvents
