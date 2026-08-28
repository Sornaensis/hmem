{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | The public HTTP contract.  Observations are deliberately a small,
-- repository-scoped immutable-provenance resource; the old memory graph and
-- categorisation API is intentionally not represented here.
module HMem.Server.API
  ( HMemAPI
  , ObservationEmbedding(..)
  , CreateObservationRequest(..)
  , ObservationMatchRequest(..)
  , server
  , serverWithChangeStream
  ) where

import Control.Exception (try)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, Value, object, (.=), ToJSON(..), Result(..))
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as AesonKey
import Data.Aeson.KeyMap qualified as AesonKeyMap
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Pool (Pool, tryWithResource)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (NominalDiffTime, UTCTime, diffUTCTime)
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Hasql.Connection qualified as Hasql
import Hasql.Session qualified as Session
import Rel8 hiding (Delete)
import Servant
import System.IO (stderr)

import HMem.Config qualified as Config
import HMem.DB.Audit qualified as Audit
import HMem.DB.ChangeStream qualified as ChangeStream
import HMem.DB.Auth qualified as Auth
import HMem.DB.Observation qualified as Observation
import HMem.DB.Overview qualified as Overview
import HMem.DB.Pool (DBException(..), PoolMetrics(..), getPoolMetrics, runSession)
import HMem.DB.Project qualified as Project
import HMem.DB.RequestContext (Principal(..), PrincipalAuthority(..), actorTypeToText, currentPrincipal, withWorkspaceIdContext)
import HMem.DB.Schema
import HMem.DB.Search qualified as Search
import HMem.DB.Task qualified as Task
import HMem.DB.Timeline qualified as Timeline
import HMem.DB.Workspace qualified as Workspace
import HMem.DB.WorkspaceGroup qualified as WorkspaceGroup
import HMem.Server.AccessTracker (AccessTracker, bufferSize)
import HMem.Server.WebSocket qualified as WS
import HMem.Server.Snapshot (materializeSnapshot)
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
  :<|> "change-stream" :> "resync" :> ReqBody '[JSON] ChangeStreamResyncRequest :> Post '[JSON] ChangeStreamResyncResponse
  :<|> "change-stream" :> "ticket" :> ReqBody '[JSON] CanonicalWebSocketTicketRequest :> Post '[JSON] WebSocketTicketResponse
  )

type WorkspaceAPI =
       QueryParam "limit" Int :> QueryParam "offset" Int :> Get '[JSON] (PaginatedResult Workspace)
  :<|> ReqBody '[JSON] CreateWorkspace :> Post '[JSON] Workspace
  :<|> Capture "workspaceId" UUID :> Get '[JSON] Workspace
  :<|> Capture "workspaceId" UUID :> "timeline" :> "buckets"
         :> QueryParam "since" UTCTime :> QueryParam "until" UTCTime :> QueryParam "bucket" Text
         :> Get '[JSON] WorkspaceTimelineBucketsResponse
  :<|> Capture "workspaceId" UUID :> "timeline"
         :> QueryParam "entity_type" Text :> QueryParam "event_type" Text
         :> QueryParam "since" UTCTime :> QueryParam "until" UTCTime
         :> QueryParam "limit" Int :> QueryParam "offset" Int
         :> Get '[JSON] (PaginatedResult WorkspaceTimelineEvent)

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
         :> Description "Exact stored subject kind. When supplied with subject, both values must match the same stored subject row."
         :> QueryParam "subject_kind" SubjectKind
         :> Description "Exact canonical repository-relative subject path or glob. When supplied with subject_kind, both values must match the same stored subject row."
         :> QueryParam "subject" Text
         :> Description "Exact immutable 40-character lowercase hexadecimal Git SHA."
         :> QueryParam "git_sha" Text
         :> QueryParam "query" Text
         :> Description "Page size; defaults to 50 and must be between 1 and 200."
         :> QueryParam "limit" Int
         :> Description "Zero-based page offset; defaults to 0 and must be at most 100000."
         :> QueryParam "offset" Int
         :> Get '[JSON] (PaginatedResult Observation)
  :<|> ReqBody '[JSON] CreateObservationRequest :> Post '[JSON] Observation
  :<|> "match" :> ReqBody '[JSON] ObservationMatchRequest :> Post '[JSON] (PaginatedResult ObservationMatch)
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
   :<|> Capture "taskId" UUID :> "dependencies" :> ReqBody '[JSON] LinkDependency :> Post '[JSON] DependencyMutationResult
   :<|> Capture "taskId" UUID :> "dependencies" :> Capture "dependsOnId" UUID :> Delete '[JSON] DependencyMutationResult

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

newtype CreateObservationRequest = CreateObservationRequest (Either Text CreateObservation)
newtype ObservationMatchRequest = ObservationMatchRequest (Either Text ObservationMatchQuery)

instance FromJSON CreateObservationRequest where
  parseJSON value = pure $ CreateObservationRequest $ case Aeson.fromJSON value of
    Aeson.Error message -> Left (Text.pack message)
    Aeson.Success parsed -> Right parsed

instance FromJSON ObservationMatchRequest where
  parseJSON value = pure $ ObservationMatchRequest $ case Aeson.fromJSON value of
    Aeson.Error message -> Left (Text.pack message)
    Aeson.Success parsed -> Right parsed

decodeRequest :: Either Text a -> Handler a
decodeRequest = either (throwError . badRequest "validation_error") pure

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

page :: Maybe Int -> Maybe Int -> (Int, Int)
page = capPagination

------------------------------------------------------------------------
-- Handlers
------------------------------------------------------------------------

server :: Config.AuthConfig -> Pool Hasql.Connection -> AccessTracker -> WS.WSState -> Bool -> Server HMemAPI
server authConfig pool tracker wsState =
  serverWithChangeStream authConfig Config.defaultConfig.changeStream pool tracker wsState

serverWithChangeStream :: Config.AuthConfig -> Config.ChangeStreamConfig -> Pool Hasql.Connection -> AccessTracker -> WS.WSState -> Bool -> Server HMemAPI
serverWithChangeStream authConfig changeStreamConfig pool tracker wsState _ =
       health pool tracker
  :<|> session authConfig pool
  :<|> workspaces pool
  :<|> groups pool
  :<|> observations pool
  :<|> projects pool
  :<|> tasks pool
  :<|> search pool
  :<|> audit pool
  :<|> ticket pool wsState
  :<|> resync changeStreamConfig pool
  :<|> canonicalTicket changeStreamConfig pool wsState

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
workspaces pool = listH :<|> createH :<|> getH :<|> timelineBucketsH :<|> timelineH where
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
  timelineBucketsH workspaceId mSince mUntil mBucket = do
    requireWorkspace pool workspaceId Auth.WorkspaceRoleRead
    since <- requireTimelineBucketParam "since" mSince
    untilTime <- requireTimelineBucketParam "until" mUntil
    let bucket = fromMaybe "week" mBucket
    reject (validateTimelineBucketQuery since untilTime bucket)
    buckets <- handleDBErrors $ Timeline.listWorkspaceTimelineBuckets pool workspaceId since untilTime bucket
    when (length buckets > maxTimelineBuckets) $
      reject ["timeline bucket range produces too many buckets; narrow the range or choose a larger bucket"]
    pure WorkspaceTimelineBucketsResponse
      { timelineBucketsWorkspaceId = workspaceId
      , timelineBucketsSince = since
      , timelineBucketsUntil = untilTime
      , timelineBucketsBucket = bucket
      , timelineBucketsBuckets = buckets }
  timelineH workspaceId entityType eventType since untilTime limit offset = do
    requireWorkspace pool workspaceId Auth.WorkspaceRoleRead
    reject (validateTimelineRangeQuery since untilTime <> validateTimelinePagination limit offset)
    let (takeN, skipN) = page limit offset
    rows <- handleDBErrors $ Timeline.listWorkspaceTimeline pool workspaceId entityType eventType since untilTime (Just (takeN + 1)) (Just skipN)
    pure PaginatedResult { items = take takeN rows, hasMore = length rows > takeN }

requireTimelineBucketParam :: Text -> Maybe a -> Handler a
requireTimelineBucketParam name = maybe (throwError (badRequest "validation_error" (name <> " is required"))) pure

validateTimelineRangeQuery :: Maybe UTCTime -> Maybe UTCTime -> [Text]
validateTimelineRangeQuery mSince mUntil = case (mSince, mUntil) of
  (Just since, Just untilTime) | since >= untilTime -> ["since must be before until"]
  _ -> []

validateTimelinePagination :: Maybe Int -> Maybe Int -> [Text]
validateTimelinePagination limit offset =
  ["limit must be between 1 and " <> Text.pack (show maxPaginationLimit) | maybe False (\n -> n < 1 || n > maxPaginationLimit) limit]
  <> ["offset must be between 0 and " <> Text.pack (show maxPaginationOffset) | maybe False (\n -> n < 0 || n > maxPaginationOffset) offset]

validateTimelineBucketQuery :: UTCTime -> UTCTime -> Text -> [Text]
validateTimelineBucketQuery since untilTime bucket =
  ["bucket must be one of day, week, month, or quarter" | bucket `notElem` validTimelineBuckets]
  <> validateTimelineRangeQuery (Just since) (Just untilTime)
  <> ["timeline bucket range must not exceed ten years" | diffUTCTime untilTime since > tenYearsSeconds]

validTimelineBuckets :: [Text]
validTimelineBuckets = ["day", "week", "month", "quarter"]

maxTimelineBuckets :: Int
maxTimelineBuckets = 366

tenYearsSeconds :: NominalDiffTime
tenYearsSeconds = 10 * 366 * 24 * 60 * 60

groups :: Pool Hasql.Connection -> Server WorkspaceGroupAPI
groups pool = listH :<|> createH :<|> getH :<|> deleteH :<|> listMembersH :<|> addMemberH :<|> removeMemberH where
  listH limit offset = do
    requireSuperadmin pool
    let (takeN, skipN) = page limit offset
    rows <- handleDBErrors $ WorkspaceGroup.listGroups pool (Just (takeN + 1)) (Just skipN)
    pure PaginatedResult { items = take takeN rows, hasMore = length rows > takeN }
  createH input = do
    requireSuperadmin pool
    reject (validateCreateWorkspaceGroupInput input)
    created <- handleDBErrors $ WorkspaceGroup.createGroup pool input
    pure created
  getH groupId = do
    requireSuperadmin pool
    handleDBErrors (WorkspaceGroup.getGroup pool groupId) >>= maybe (throwError err404) pure
  deleteH groupId = do
    requireSuperadmin pool
    deleted <- handleDBErrors $ WorkspaceGroup.deleteGroup pool groupId
    if deleted then pure NoContent else throwError err404
  listMembersH groupId = do
    requireSuperadmin pool
    _ <- handleDBErrors (WorkspaceGroup.getGroup pool groupId) >>= maybe (throwError err404) pure
    handleDBErrors $ WorkspaceGroup.listGroupMembers pool groupId
  addMemberH groupId input = do
    requireSuperadmin pool
    _ <- handleDBErrors (WorkspaceGroup.getGroup pool groupId) >>= maybe (throwError err404) pure
    memberResult <- handleDBErrors $ WorkspaceGroup.addMember pool groupId input.workspaceId
    case memberResult of
      WorkspaceGroup.MemberAdded -> pure ()
      WorkspaceGroup.MemberAlreadyPresent -> pure ()
      WorkspaceGroup.MemberWorkspaceInactive -> throwError err404
    pure NoContent
  removeMemberH groupId workspaceId = do
    requireSuperadmin pool
    _ <- handleDBErrors (WorkspaceGroup.getGroup pool groupId) >>= maybe (throwError err404) pure
    requireWorkspace pool workspaceId Auth.WorkspaceRoleRead
    removed <- handleDBErrors $ WorkspaceGroup.removeMember pool groupId workspaceId
    if removed then pure () else pure ()
    pure NoContent

observations :: Pool Hasql.Connection -> Server ObservationAPI
observations pool = listH :<|> createH :<|> matchH :<|> similarH :<|> getH :<|> updateH :<|> deleteH :<|> embeddingH where
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
  createH (CreateObservationRequest requestBody) = do
    input <- decodeRequest requestBody
    requireObservationWorkspace pool input.workspaceId Auth.WorkspaceRoleEdit
    reject (validateCreateObservationInput input)
    created <- handleDBErrors $ withWorkspaceIdContext (Just input.workspaceId) (Observation.createObservation pool input)
    pure created
  matchH (ObservationMatchRequest requestBody) = do
    query <- decodeRequest requestBody
    requireObservationWorkspace pool query.workspaceId Auth.WorkspaceRoleRead
    reject (validateObservationMatchQuery query)
    let (takeN, skipN) = page query.limit query.offset
        pagedQuery = ObservationMatchQuery
          query.workspaceId query.paths query.subjectKind query.gitSha query.query
          (Just takeN) (Just skipN)
    rows <- handleDBErrors $ Observation.matchObservationsOverfetch pool pagedQuery
    pure PaginatedResult { items = take takeN rows, hasMore = length rows > takeN }
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
    pure updated
  deleteH observationId = do
    workspaceId <- requireEntity pool Auth.EntityObservation observationId Auth.WorkspaceRoleEdit
    requireObservationWorkspace pool workspaceId Auth.WorkspaceRoleEdit
    deleted <- handleDBErrors (Observation.deleteObservation pool workspaceId observationId)
    if deleted then pure NoContent else throwError err404
  embeddingH observationId (ObservationEmbedding vector) = do
    workspaceId <- requireEntity pool Auth.EntityObservation observationId Auth.WorkspaceRoleEdit
    requireObservationWorkspace pool workspaceId Auth.WorkspaceRoleEdit
    -- Verify existence so a no-op UPDATE is never reported as success.
    _ <- handleDBErrors (Observation.getObservation pool workspaceId observationId) >>= maybe (throwError err404) pure
    handleDBErrors $ Observation.setObservationEmbedding pool workspaceId observationId vector
    pure NoContent

projects :: Pool Hasql.Connection -> Server ProjectAPI
projects pool = listH :<|> createH :<|> getH :<|> updateH :<|> deleteH :<|> overviewH :<|> nextH where
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
    pure created
  getH projectId = do
    _ <- requireEntity pool Auth.EntityProject projectId Auth.WorkspaceRoleRead
    handleDBErrors (Project.getProject pool projectId) >>= maybe (throwError err404) pure
  updateH projectId input = do
    workspaceId <- requireEntity pool Auth.EntityProject projectId Auth.WorkspaceRoleEdit; reject (validateUpdateProjectInput input)
    updated <- handleDBErrors (Project.updateProject pool projectId input) >>= maybe (throwError err404) pure
    pure updated
  deleteH projectId = do
    workspaceId <- requireEntity pool Auth.EntityProject projectId Auth.WorkspaceRoleEdit
    handleDBErrors (Project.deleteProjectCascade pool projectId) >>= maybe (throwError err404) pure
  overviewH projectId = do
    _ <- requireEntity pool Auth.EntityProject projectId Auth.WorkspaceRoleRead
    handleDBErrors (Overview.getProjectOverview pool projectId) >>= maybe (throwError err404) pure
  nextH projectId limit includeBlocked = do
    _ <- requireEntity pool Auth.EntityProject projectId Auth.WorkspaceRoleRead
    handleDBErrors $ Task.listNextTasks pool projectId (fromMaybe False includeBlocked) (fromMaybe 5 limit)

tasks :: Pool Hasql.Connection -> Server TaskAPI
tasks pool = listH :<|> createH :<|> getH :<|> updateH :<|> deleteH :<|> overviewH :<|> addDependencyH :<|> removeDependencyH where
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
    pure created
  getH taskId = do
    _ <- requireEntity pool Auth.EntityTask taskId Auth.WorkspaceRoleRead
    handleDBErrors (Task.getTask pool taskId) >>= maybe (throwError err404) pure
  updateH taskId input = do
    workspaceId <- requireEntity pool Auth.EntityTask taskId Auth.WorkspaceRoleEdit; reject (validateUpdateTaskInput input)
    result <- handleDBErrors (Task.updateTaskWithDependencySnapshots pool taskId input) >>= maybe (throwError err404) pure
    let (updated, before, after) = result
        effects = [] -- snapshots are persisted by core; task updates do not expose Memory effects.
    pure TaskMutationResult { task = updated, dependencyEffects = effects }
  deleteH taskId = do
    workspaceId <- requireEntity pool Auth.EntityTask taskId Auth.WorkspaceRoleEdit
    handleDBErrors (Task.deleteTaskCascade pool taskId) >>= maybe (throwError err404) pure
  overviewH taskId = do
    _ <- requireEntity pool Auth.EntityTask taskId Auth.WorkspaceRoleRead
    handleDBErrors (Overview.getTaskOverview pool taskId) >>= maybe (throwError err404) pure
  addDependencyH taskId input = mutateDependency "add" taskId input.dependsOnId Task.addDependencyWithSnapshots
  removeDependencyH taskId dependsOnId = mutateDependency "remove" taskId dependsOnId Task.removeDependencyWithSnapshots
  mutateDependency action taskId dependsOnId mutate = do
    taskWorkspace <- requireEntity pool Auth.EntityTask taskId Auth.WorkspaceRoleEdit
    dependencyWorkspace <- requireEntity pool Auth.EntityTask dependsOnId Auth.WorkspaceRoleRead
    when (taskId == dependsOnId) $ reject ["a task cannot depend on itself"]
    when (taskWorkspace /= dependencyWorkspace) $ reject ["task dependencies must belong to the same workspace"]
    (before, after) <- handleDBErrors $ mutate pool taskId dependsOnId
    let result = DependencyMutationResult
          { action = action
          , taskId = taskId
          , dependsOnId = dependsOnId
          , affectedTasks = dependencyStatusChanges action before after
          }
    pure result

dependencyStatusChanges :: Text -> [TaskDependencyAutoBlockSnapshot] -> [TaskDependencyAutoBlockSnapshot] -> [TaskDependencyStatusChange]
dependencyStatusChanges action before after = mapMaybe changed after
  where
    previous snapshot = case [value | value <- before, value.task.id == snapshot.task.id] of
      value : _ -> value
      [] -> snapshot
    changed snapshot
      | snapshot.task.status == old.task.status
          && snapshot.autoBlocked == old.autoBlocked
          && snapshot.openDependencyCount == old.openDependencyCount = Nothing
      | otherwise = Just TaskDependencyStatusChange
          { task = snapshot.task
          , previousStatus = old.task.status
          , currentStatus = snapshot.task.status
          , previousAutoBlocked = old.autoBlocked
          , autoBlocked = snapshot.autoBlocked
          , previousOpenDependencyCount = old.openDependencyCount
          , openDependencyCount = snapshot.openDependencyCount
           , reason = dependencyStatusChangeReason old snapshot
           }
      where old = previous snapshot

dependencyStatusChangeReason :: TaskDependencyAutoBlockSnapshot -> TaskDependencyAutoBlockSnapshot -> Text
dependencyStatusChangeReason previous current
  | not previous.autoBlocked && current.autoBlocked = "blocked_by_open_dependencies"
  | previous.autoBlocked && not current.autoBlocked = "unblocked_dependencies_resolved"
  | current.openDependencyCount > previous.openDependencyCount = "open_dependency_added"
  | current.openDependencyCount < previous.openDependencyCount = "open_dependency_removed"
  | otherwise = "dependency_status_changed"

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

audit :: Pool Hasql.Connection -> Server AuditAPI
audit pool = listH :<|> revertH :<|> getH where
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
        handleDBErrors (Project.updateProject pool entityId old) >>= maybe (throwError err409) (pure . Just . toJSON)
      ("project", AuditDelete) -> do
        restored <- handleDBErrors $ Project.restoreProject pool entityId
        if restored then do project <- handleDBErrors (Project.getProject pool entityId); pure (toJSON <$> project) else throwError err409
      ("project", AuditCreate) -> do
        deleted <- handleDBErrors $ Project.deleteProjectCascade pool entityId
        case deleted of Nothing -> throwError err409; Just _ -> pure Nothing
      ("task", AuditUpdate) | isSoftDelete entry -> restoreTask workspaceId entityId
      ("task", AuditUpdate) -> do
        old <- decodeOld entry
        handleDBErrors (Task.updateTask pool entityId old) >>= maybe (throwError err409) (pure . Just . toJSON)
      ("task", AuditDelete) -> do
        restored <- handleDBErrors $ Task.restoreTask pool entityId
        if restored then do task <- handleDBErrors (Task.getTask pool entityId); pure (toJSON <$> task) else throwError err409
      ("task", AuditCreate) -> do
        deleted <- handleDBErrors $ Task.deleteTaskCascade pool entityId
        case deleted of Nothing -> throwError err409; Just _ -> pure Nothing
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
        pure (toJSON <$> project)
      else throwError err409
  restoreTask workspace taskId = do
    restored <- handleDBErrors $ Task.restoreTask pool taskId
    if restored
      then do
        task <- handleDBErrors (Task.getTask pool taskId)
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
  receivesWorkspaceAdminEvents <- liftIO $ Auth.hasWorkspaceRole pool (Just principal) request.workspaceId Auth.WorkspaceRoleAdmin
  liftIO $ WS.createTicketWithAudience state principal request.workspaceId receivesGlobalGroupEvents receivesWorkspaceAdminEvents

-- Canonical resync uses the durable core state machine for both initial and
-- continuation pages.  The API never exposes its high-watermark or cursor.
resync :: Config.ChangeStreamConfig -> Pool Hasql.Connection -> ChangeStreamResyncRequest -> Handler ChangeStreamResyncResponse
resync changeStreamConfig pool request = do
  (scope', audience) <- changeStreamIdentity pool request.scope
  let pageSize = fromMaybe 100 request.pageSize
  when (pageSize < 1 || pageSize > 1000) $
    throwError (badRequest "validation_error" "page_size must be between 1 and 1000")
  result <- liftIO $ case request.pageToken of
    Nothing -> do
      case request.startIdempotencyKey of
        Nothing -> pure (Left ChangeStream.SnapshotOutOfOrder)
        Just startKey -> do
          begun <- ChangeStream.beginResyncWithStartKey pool (fromIntegral changeStreamConfig.snapshotSessionTtlSeconds) scope' audience startKey pageSize (materializeSnapshot scope')
          case begun of
            Left err -> pure (Left err)
            Right begin -> ChangeStream.readSnapshotPageWithTtls pool (fromIntegral changeStreamConfig.snapshotSessionTtlSeconds) (fromIntegral changeStreamConfig.resumeTokenTtlSeconds) scope' audience begin.snapshotToken pageSize
    Just token -> ChangeStream.readSnapshotPageWithTtls pool (fromIntegral changeStreamConfig.snapshotSessionTtlSeconds) (fromIntegral changeStreamConfig.resumeTokenTtlSeconds) scope' audience (ChangeStream.SnapshotToken token) pageSize
  case result of
    Left ChangeStream.ResyncUnauthorized -> throwError err403
    Left _ -> throwError resyncRequired
    Right snapshotPage -> do
      typedItems <- traverse decodeSnapshotItem snapshotPage.snapshotPageItems
      pure ChangeStreamResyncResponse
        { items = typedItems
        , hasMore = snapshotPage.snapshotPageHasMore
        , nextPageToken = renderSnapshotToken <$> snapshotPage.snapshotNextToken
        , resumeToken = renderResumeToken <$> snapshotPage.snapshotResumeToken
        }

canonicalTicket :: Config.ChangeStreamConfig -> Pool Hasql.Connection -> WS.WSState -> CanonicalWebSocketTicketRequest -> Handler WebSocketTicketResponse
canonicalTicket _changeStreamConfig pool state request = do
  (scope', audience) <- changeStreamIdentity pool request.scope
  principal <- liftIO currentPrincipal >>= maybe (throwError err401) pure
  valid <- liftIO $ ChangeStream.validateCanonicalResumeToken pool scope' audience (ChangeStream.ResumeToken request.resumeToken)
  case valid of
    Left ChangeStream.ResyncUnauthorized -> throwError err403
    Left _ -> throwError resyncRequired
    Right resumeExpiresAt -> liftIO $ WS.createCanonicalTicketWithExpiry state resumeExpiresAt principal scope' (ChangeStream.ResumeToken request.resumeToken)

-- The authorization check is done before bearer work, and the corresponding
-- database state machine checks it again under the scope lock.  A grant-user
-- audience binds to the stable authenticated user id; local synthetic
-- principals are confined to the private local-server audience.
changeStreamIdentity :: Pool Hasql.Connection -> ChangeStreamScopeRequest -> Handler (ChangeStream.ChangeScope, ChangeStream.ChangeAudience)
changeStreamIdentity pool scopeRequest = do
  principal <- liftIO currentPrincipal >>= maybe (throwError err401) pure
  scope' <- case scopeRequest of
    ChangeStreamWorkspace workspace -> requireWorkspace pool workspace Auth.WorkspaceRoleRead >> pure (ChangeStream.WorkspaceScope workspace)
    ChangeStreamGlobal -> requireSuperadmin pool >> pure ChangeStream.GlobalScope
  audience <- case principal.authority of
    PrincipalGrantUser userId -> pure (ChangeStream.AuthenticatedAudience (UUID.toText userId) userId)
    PrincipalSyntheticLocalSuperadmin -> pure (ChangeStream.TrustedAudience ("local:" <> principal.actorId))
    PrincipalNoAuthority -> throwError err401
  pure (scope', audience)

decodeSnapshotItem :: Value -> Handler ChangeStreamSnapshotItem
decodeSnapshotItem value = case Aeson.fromJSON value of
  Aeson.Success item -> pure item
  Aeson.Error _ -> throwError err500

resyncRequired :: ServerError
resyncRequired = err409
  { errBody = Aeson.encode (object ["error" .= ("resync_required" :: Text)]) }

renderSnapshotToken :: ChangeStream.SnapshotToken -> Text
renderSnapshotToken (ChangeStream.SnapshotToken value) = value

renderResumeToken :: ChangeStream.ResumeToken -> Text
renderResumeToken (ChangeStream.ResumeToken value) = value
