{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | The public HTTP contract.  Observations are deliberately a small,
-- repository-scoped immutable-provenance resource; the old memory graph and
-- categorisation API is intentionally not represented here.
module HMem.Server.API
  ( HMemAPI
  , ObservationEmbedding(..)
  , CreateObservationRequest(..)
  , ReviewedObservationRequest(..)
  , ObservationMatchRequest(..)
  , ObservationCountRequest(..)
  , LinkDependencyRequest(..)
  , UpdateWorkspaceRequest(..)
  , navigationPageWhenRequested
  , navigationOwnerOutcomes
  , server
  , serverWithChangeStream
  , serverWithDiscovery
  , serverWithPrefixProofs
  , prepareObservationDiscovery
  , AgentSessionRequest
  , authorizeAgentSessionWrite
  , authorizeTaskEvidenceRead
  ) where

import Control.Exception (try, throwIO)
import Control.Monad (forM, forM_, when, unless)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, Value, object, (.=), ToJSON(..), Result(..))
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as AesonKey
import Data.Aeson.KeyMap qualified as AesonKeyMap
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Int (Int64)
import Data.List (nub)
import Data.Map.Strict qualified as Map
import Data.Maybe (isNothing, isJust, fromMaybe, mapMaybe, catMaybes)
import Data.Pool (Pool, tryWithResource)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TE
import Data.Time (NominalDiffTime, UTCTime(..), diffUTCTime, addUTCTime, addDays, addGregorianMonthsClip, toGregorian, fromGregorian, dayOfWeek, DayOfWeek(..))
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Hasql.Connection qualified as Hasql
import Hasql.Session qualified as Session
import Network.HTTP.Media ((//))
import Rel8 hiding (Delete)
import Servant
import System.IO (stderr)

import HMem.Config qualified as Config
import HMem.AgentSession qualified as AgentSession
import HMem.AgentSession.Transport qualified as SessionWire
import HMem.DB.AgentSession qualified as AgentSessions
import HMem.DB.Audit qualified as Audit
import HMem.DB.ChangeStream qualified as ChangeStream
import HMem.DB.Auth qualified as Auth
import HMem.DB.Observation qualified as Observation
import HMem.DB.Embedding qualified as Embedding
import HMem.DB.Overview qualified as Overview
import HMem.DB.Pool (DBException(..), PoolMetrics(..), getPoolMetrics, runSession)
import HMem.DB.Project qualified as Project
import HMem.DB.RequestContext (Principal(..), EffectiveActor(..), PrincipalAuthority(..), actorTypeToText, currentPrincipal, currentEffectiveActor, withWorkspaceIdContext)
import HMem.DB.Schema
import HMem.DB.Search qualified as Search
import HMem.DB.Task qualified as Task
import HMem.DB.Timeline qualified as Timeline
import HMem.DB.Workspace qualified as Workspace
import HMem.DB.WorkspaceGroup qualified as WorkspaceGroup
import HMem.Server.AccessTracker (AccessTracker, bufferSize)
import HMem.Server.ObservationDiscovery qualified as Discovery
import HMem.Server.SessionPrefix qualified as Prefix
import HMem.Server.WebSocket qualified as WS
import HMem.Server.Snapshot (materializeSnapshotWithProfile)
import HMem.Types

------------------------------------------------------------------------
-- API type
------------------------------------------------------------------------

type HMemAPI = "api" :> "v1" :>
  (    "health"       :> Get '[JSON] Value
  :<|> "session"      :> QueryParam "workspace_id" UUID :> Get '[JSON] SessionContext
  :<|> "session-activity" :> QueryParams "workspace_id" Text :> QueryParam "project_workspace_id" Text :> QueryParams "project_id" Text
         :> Description "Boolean active-session projection for at most 50 combined unique workspace/project targets. Workspace targets must be readable; project targets require an explicit readable same-workspace scope and include all live project/task descendants. Missing or denied targets are omitted, never reported inactive. No freshness or task/project status filter."
         :> Get '[JSON] Value
  :<|> "session-activity" :> "ticket" :> Post '[JSON] WebSocketTicketResponse
  :<|> "workspaces"   :> WorkspaceAPI
  :<|> "groups"       :> WorkspaceGroupAPI
  :<|> "observations" :> ObservationAPI
  :<|> "observations" :> "discovery" :> ReqBody '[JSON] ObservationDiscoveryRequest
         :> Description "Authorized prepared Observation discovery. Create with filters; continue with context_token and view selectors only. Contexts expire after five minutes and pin FTS/hybrid mode. Complete content-only FTS and exact-space cosine>=0.5 candidates are deduplicated, reciprocal-rank fused (k=60), then paged. No semantic top-K truncation."
         :> Post '[JSON] Value
  :<|> "observation-discovery" :> "capability" :> QueryParam' '[Required] "workspace_id" UUID
         :> Description "Repository read authorization precedes optional provider/storage checks. Returns hybrid or fts_only with a fixed fallback reason; never returns vectors or provider configuration."
         :> Get '[JSON] Value
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
  :<|> Capture "workspaceId" UUID :> ReqBody '[TolerantJSON] UpdateWorkspaceRequest :> Put '[JSON] Workspace
  :<|> Capture "workspaceId" UUID :> Description "Requires workspace admin access. Soft-deletes the workspace without purging its contents; unavailable through MCP." :> Delete '[JSON] NoContent
  :<|> Capture "workspaceId" UUID :> "planning-counts" :> Get '[JSON] WorkspacePlanningCounts
  :<|> Capture "workspaceId" UUID :> "navigation"
         :> QueryParam "parent_kind" Text :> QueryParam "parent_id" UUID
         :> QueryParam "project_limit" Int :> QueryParam "project_offset" Int
         :> QueryParam "task_limit" Int :> QueryParam "task_offset" Int
         :> QueryParam "include_projects" Bool :> QueryParam "include_tasks" Bool
         :> QueryParam "show_only" Text :> QueryParams "project_status" ProjectStatus :> QueryParams "task_status" TaskStatus
          :> QueryParam "show_empty_projects" Bool :> QueryParam "has_active_session" Bool :> QueryParam "priority_mode" Text :> QueryParam "priority_value" Int :> QueryParam "query" Text
          :> Description "Returns separately paged direct child Project and Task card summaries. parent_kind is workspace_root, project, or task; project/task require parent_id. The full tree filter DTO includes show_only, project_status, task_status, priority_mode (any/exact/above/below), priority_value, case-insensitive query, and show_empty_projects (default true); false requires a matching task in the project subtree even in projects-only mode; matching descendants retain ancestors. include_projects and include_tasks default to true; false skips that kind's selection and decoration and returns an empty page with has_more=false, which carries no membership/completion authority. Authorization, parent checks and all validation still apply. Each requested page defaults to 50, is capped at 100, and uses deterministic lifecycle-rank, priority DESC, title/name, id ordering."
         :> Get '[JSON] NavigationBranchResponse
  :<|> Capture "workspaceId" UUID :> "navigation" :> "batch"
         :> ReqBody '[JSON] NavigationBatchRequest :> Post '[JSON] NavigationBatchResponse
  :<|> Capture "workspaceId" UUID :> "navigation" :> "focus" :> Capture "entityType" NavigationEntityType :> Capture "entityId" UUID :> QueryParam "ancestor_offset" Int
         :> Description "Fetches a target card plus a root-to-parent ancestor window for direct links outside the loaded branch. Missing or foreign targets return 404. Ancestors are capped at 64; use next_ancestor_offset as ancestor_offset to continue a truncated chain."
         :> Get '[JSON] NavigationFocusResponse
  :<|> Capture "workspaceId" UUID :> "navigation" :> "summaries"
         :> Description "Revalidates at most 100 unique project_ids/task_ids. Input order is preserved; missing/deleted IDs are reported without disclosing foreign IDs."
         :> ReqBody '[JSON] NavigationSummariesRequest :> Post '[JSON] NavigationSummariesResponse
  :<|> Capture "workspaceId" UUID :> "timeline" :> "buckets"
         :> QueryParam "since" UTCTime :> QueryParam "until" UTCTime :> QueryParam "bucket" Text :> QueryParam "paged" Bool
         :> Get '[JSON] WorkspaceTimelineBucketsResponse
  :<|> Capture "workspaceId" UUID :> "timeline"
         :> QueryParam "entity_type" Text :> QueryParam "event_type" Text
         :> QueryParam "since" UTCTime :> QueryParam "until" UTCTime
         :> QueryParam "limit" Int :> QueryParam "offset" Int
         :> Get '[JSON] (PaginatedResult WorkspaceTimelineEvent)
  :<|> Capture "workspaceId" UUID :> "sessions" :> AgentSessionAPI
  :<|> Capture "workspaceId" UUID :> "tasks" :> Capture "taskId" UUID :> "sessions"
         :> QueryParam "limit" Text :> QueryParam "before_created_at" Text :> QueryParam "before_id" Text
         :> QueryParam "state" Text :> QueryParam "freshness" Text
         :> Get '[JSON] SessionWire.SessionPageResponse

type AgentSessionAPI =
       QueryParam "limit" Text :> QueryParam "before_created_at" Text :> QueryParam "before_id" Text
         :> QueryParam "state" Text :> QueryParam "freshness" Text
         :> QueryParam "project_id" UUID :> QueryParam "task_id" UUID
         :> Get '[JSON] SessionWire.SessionPageResponse
  :<|> "filters" :> QueryParam "kind" Text :> QueryParam "project_id" UUID :> QueryParam "after_id" UUID :> QueryParam "selected_id" UUID :> QueryParam "limit" Text :> Get '[JSON] AgentSession.SessionFacetPage
  :<|> ReqBody '[TolerantJSON] (AgentSessionRequest SessionWire.CreateSessionRequest) :> Post '[JSON] AgentSession.AgentSession
  :<|> "imports" :> ReqBody '[TolerantJSON] (AgentSessionRequest SessionWire.ImportSessionRequest) :> Post '[JSON] AgentSession.AgentSession
  :<|> Capture "sessionId" UUID :> Get '[JSON] AgentSession.AgentSession
  :<|> Capture "sessionId" UUID :> "append" :> ReqBody '[TolerantJSON] (AgentSessionRequest SessionWire.AppendSessionRequest) :> Post '[JSON] SessionWire.AppendSessionResponse
  :<|> Capture "sessionId" UUID :> "heartbeat" :> Post '[JSON] AgentSession.AgentSession
  :<|> Capture "sessionId" UUID :> "runtime" :> ReqBody '[TolerantJSON] (AgentSessionRequest AgentSession.SetRuntimeIdentity) :> Put '[JSON] AgentSession.AgentSession
  :<|> Capture "sessionId" UUID :> "tasks" :> QueryParam "limit" Text :> QueryParam "after_task_id" Text :> Get '[JSON] SessionWire.SessionTaskPageResponse
  :<|> Capture "sessionId" UUID :> "tasks" :> Capture "taskId" UUID :> Put '[JSON] NoContent
  :<|> Capture "sessionId" UUID :> "terminate" :> ReqBody '[TolerantJSON] (AgentSessionRequest SessionWire.TerminateSessionRequest) :> Post '[JSON] AgentSession.AgentSession
  :<|> Capture "sessionId" UUID :> "transcript" :> QueryParam "offset" Text :> QueryParam "limit" Text :> Get '[JSON] SessionWire.SessionRangeResponse
  :<|> Capture "sessionId" UUID :> "evidence" :> QueryParam "offset" Text :> QueryParam "limit" Text :> Get '[JSON] SessionWire.SessionRangeResponse
  :<|> Capture "sessionId" UUID :> "prefix-verifications" :> ReqBody '[TolerantJSON] (AgentSessionRequest SessionWire.PrefixProofRequest) :> Post '[JSON] SessionWire.PrefixProofResponse
  :<|> Capture "sessionId" UUID :> "prefix-verifications" :> Capture "operationId" UUID :> Get '[JSON] SessionWire.PrefixProofResponse

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
         :> QueryParam "current_git_sha" Text
         :> QueryParam "history_git_sha" Text
         :> QueryParam "query" Text
         :> QueryParam "query_scope" ObservationQueryScope
         :> QueryParam "limit" Int
         :> QueryParam "offset" Int
         :> Description "Lists Observations from an active repository workspace after repository read authorization. query_scope=content searches and ranks content only; omitted or combined scope searches content and stored subjects. Exact subject_kind and subject filters must match the same stored subject row. Results rank by scoped text relevance, then updated_at DESC and id DESC. Pagination defaults to limit 50 and offset 0; limit is 1..200 and offset is 0..100000."
         :> Get '[JSON] (PaginatedResult Observation)
  :<|> ReqBody '[JSON] CreateObservationRequest :> Post '[JSON] Observation
  :<|> "subject-facets"
         :> QueryParam' '[Required] "workspace_id" UUID
         :> QueryParam "subject_kind" SubjectKind
         :> QueryParam "git_sha" Text
         :> QueryParam "current_git_sha" Text
         :> QueryParam "history_git_sha" Text
         :> QueryParam "query" Text
         :> QueryParam "limit" Int
         :> QueryParam "offset" Int
         :> Description "Requires repository read authorization for the requested active repository workspace. Each facet is identified by the exact (subject_kind, subject) tuple. Groups stored subjects after workspace, subject_kind, git_sha, and query filtering. The optional full-text query searches Observation content and all stored subject text before grouping. observation_count is COUNT DISTINCT over the full filtered Observation set before subject-group pagination. Results order by observation_count DESC, latest_updated_at DESC, subject_kind, then subject. Pagination defaults to limit 50 and offset 0; limit is 1..200 and offset is 0..100000."
         :> Get '[JSON] (PaginatedResult ObservationSubjectFacet)
  :<|> "match"
         :> ReqBody '[JSON] ObservationMatchRequest
         :> Description "Requires repository read authorization for the requested active repository workspace. Matches concrete canonical repository-relative paths against stored file and glob subjects. The optional full-text query searches Observation content and all stored subject text. Canonical path_matches correlates each caller path with its ordered matched stored subjects."
         :> Post '[JSON] (PaginatedResult ObservationMatch)
  :<|> "similar" :> ReqBody '[JSON] SimilarObservationQuery :> Post '[JSON] [SimilarObservation]
  :<|> "subject-tree"
         :> QueryParam' '[Required] "workspace_id" UUID
         :> QueryParam "prefix" Text
         :> QueryParam "subject_query" Text
         :> QueryParam "subject_kind" SubjectKind
         :> QueryParam "git_sha" Text
         :> QueryParam "current_git_sha" Text
         :> QueryParam "history_git_sha" Text
         :> QueryParam "limit" Int
         :> QueryParam "offset" Int
         :> Description "Authorized active repository workspace read. Filters the complete stored typed subject catalog by case-sensitive literal subject_query substring, kind and three AND SHA predicates, then derives immediate child paths below prefix (omitted/empty is root). Wildcard components remain literal; no filesystem lookup. Paths order lexically with C collation. Nodes expose distinct subtree observation_count, has_children for path descendants, and exact typed terminals with direct counts. Unrelated terminals on matching ancestors are omitted. total_count is the full workspace Observation count; match_count is the global filtered DISTINCT count independent of prefix. has_more applies only to the child page. Defaults limit 50, offset 0; limit 1..200, offset 0..2147483647. Clients must report representational exhaustion before a continuation would exceed that offset range."
         :> Get '[JSON] ObservationSubjectTreePage
  :<|> "count" :> QueryParam "query_scope" ObservationQueryScope :> ReqBody '[JSON] ObservationCountRequest
         :> Description "Requires active repository workspace read authorization. Returns full workspace total_count and distinct match_count from one database statement snapshot. query_scope=content searches content only; omitted or combined scope searches content and stored subjects. Filters share list same-subject provenance, SHA and scoped simple full-text predicates; optional concrete paths use canonical file/glob OR matching. subject and paths are mutually exclusive. Counts ignore pagination and are nonnegative exact integers at most 9007199254740991. Deployed cookie requests require CSRF."
         :> Post '[JSON] ObservationCounts
  :<|> Capture "observationId" UUID :> Get '[JSON] Observation
  :<|> Capture "observationId" UUID :> "history" :> QueryParam "limit" Int :> QueryParam "offset" Int
         :> Description "Authorized compact revision assertions in descending sequence order. Defaults limit 50, offset 0; limit 1..200, offset 0..100000. No previous content or diff is retained. Legacy creation claims have unknown digest/version/actor."
         :> Get '[JSON] (PaginatedResult ObservationRevisionEvent)
  :<|> Capture "observationId" UUID :> Header "If-Match" Text
         :> Description "Asserts content at required reviewed_git_sha with required If-Match, one strong quoted canonical content_version UUID. Missing If-Match returns 428; malformed tags return 400. A stale token returns 409 observation_content_conflict with latest canonical Observation and no mutation. Each accepted assertion, including repeated SHA/text, advances version and sequence and returns current provenance. X-Request-Id is correlation only."
         :> ReqBody '[TolerantJSON] ReviewedObservationRequest :> Put '[JSON] Observation
  :<|> Capture "observationId" UUID :> Delete '[JSON] NoContent
  :<|> Capture "observationId" UUID :> "embedding" :> ReqBody '[JSON] ObservationEmbedding :> Put '[JSON] NoContent

type ProjectAPI =
       QueryParam "workspace_id" UUID :> QueryParam "status" ProjectStatus :> QueryParam "query" Text
         :> QueryParam "limit" Int :> QueryParam "offset" Int :> Get '[JSON] (PaginatedResult Project)
  :<|> ReqBody '[JSON] CreateProject :> Post '[JSON] Project
  :<|> "spec" :> Description "Atomically creates one project and 1..50 initial top-level tasks in input order. Requires edit access to the supplied active workspace. Validation and authorization failures create nothing; database failures roll back the complete batch." :> ReqBody '[JSON] CreateProjectSpec :> Post '[JSON] ProjectSpecResult
  :<|> Capture "projectId" UUID :> Get '[JSON] Project
  :<|> Capture "projectId" UUID :> ReqBody '[JSON] UpdateProject :> Put '[JSON] ProjectMutationResult
  :<|> Capture "projectId" UUID :> Delete '[JSON] CascadeResult
  :<|> Capture "projectId" UUID :> "overview" :> Get '[JSON] ProjectOverview
  :<|> Capture "projectId" UUID :> "next-task-page" :> QueryParam "limit" Int :> QueryParam "offset" Int :> QueryParam "include_blocked" Bool :> Get '[JSON] (PaginatedResult NextTaskCandidate)
  :<|> Capture "projectId" UUID :> "next-tasks" :> QueryParam "limit" Int :> QueryParam "include_blocked" Bool :> Get '[JSON] [NextTaskCandidate]

type TaskAPI =
       QueryParam "workspace_id" UUID :> QueryParam "project_id" UUID :> QueryParam "status" TaskStatus
         :> QueryParam "priority" Int :> QueryParam "query" Text :> QueryParam "limit" Int :> QueryParam "offset" Int
         :> Get '[JSON] (PaginatedResult Task)
  :<|> ReqBody '[JSON] CreateTask :> Post '[JSON] Task
  :<|> "batch-move" :> ReqBody '[JSON] BatchMoveTasksRequest :> Post '[JSON] BatchResult
  :<|> Capture "taskId" UUID :> Get '[JSON] Task
  :<|> Capture "taskId" UUID :> ReqBody '[JSON] UpdateTask :> Put '[JSON] TaskMutationResult
    :<|> Capture "taskId" UUID :> Delete '[JSON] CascadeResult
    :<|> Capture "taskId" UUID :> "overview" :> Get '[JSON] TaskOverview
   :<|> Capture "taskId" UUID :> "dependencies" :> QueryParam "limit" Int :> QueryParam "offset" Int
          :> Description "Lists dependency card summaries on demand. Pagination defaults to 50, is capped at 100, and orders by lower(name), id."
          :> Get '[JSON] TaskDependencyPage
   :<|> Capture "taskId" UUID :> "dependencies" :> ReqBody '[TolerantJSON] LinkDependencyRequest :> Post '[JSON] DependencyMutationResult
   :<|> Capture "taskId" UUID :> "dependencies" :> Capture "dependsOnId" UUID :> Delete '[JSON] DependencyMutationResult

type AuditAPI =
       QueryParam "workspace_id" UUID :> QueryParam "entity_type" Text :> QueryParam "entity_id" Text
         :> QueryParam "action" AuditAction :> QueryParam "since" UTCTime :> QueryParam "until" UTCTime
         :> QueryParam "limit" Int :> QueryParam "offset" Int :> Get '[JSON] (PaginatedResult AuditLogEntry)
  :<|> Capture "auditId" UUID :> "revert" :> Post '[JSON] RevertResult
  :<|> Capture "auditId" UUID :> Get '[JSON] AuditLogEntry

instance FromHttpApiData SubjectKind where
  parseQueryParam value = maybe (Left "subject_kind must be file or glob") Right (subjectKindFromText value)
instance FromHttpApiData ObservationQueryScope where
  parseQueryParam "combined" = Right ObservationCombined
  parseQueryParam "content" = Right ObservationContent
  parseQueryParam _ = Left "query_scope must be combined or content"
instance FromHttpApiData ProjectStatus where
  parseQueryParam value = maybe (Left "invalid project status") Right (projectStatusFromText value)
instance FromHttpApiData TaskStatus where
  parseQueryParam value = maybe (Left "invalid task status") Right (taskStatusFromText value)
instance FromHttpApiData NavigationEntityType where
  parseUrlPiece "project" = Right NavigationProject
  parseUrlPiece "task" = Right NavigationTask
  parseUrlPiece _ = Left "entityType must be project or task"
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
        DBCheckViolation message
          | message == Search.unifiedSearchContinuationError -> badRequest "continuation_limit" message
          | otherwise -> badRequest "invalid_request" "Request violates a data constraint"
        -- The task-dependency trigger is the authoritative cycle guard. Its
        -- rejection is a caller-correctable graph validation error, not an
        -- unexpected database failure.
        DBTaskDependencyCycle{} -> badRequest "dependency_cycle" "Task dependency would create a cycle"
        DBLifecycleViolation code message detail hint -> err409
          { errBody = Aeson.encode $ object $
              [ "error" .= ("lifecycle_conflict" :: Text)
              , "code" .= code
              , "message" .= message
              ]
              <> ["detail" .= decodeLifecycleDetail value | Just value <- [detail]]
              <> ["hint" .= value | Just value <- [hint]]
          , errHeaders = [("Content-Type", "application/json")]
          }
        DBCapabilityUnavailable{} -> err503 { errBody = Aeson.encode (object ["error" .= ("capability_unavailable" :: Text), "message" .= ("pgvector embedding support is unavailable" :: Text)]) }
        DBStatementTimeout -> err504 { errBody = Aeson.encode (object ["error" .= ("timeout" :: Text)]) }
        _ -> err500 { errBody = Aeson.encode (object ["error" .= ("internal" :: Text)]) }

badRequest :: Text -> Text -> ServerError
badRequest kind message = err400
  { errBody = Aeson.encode (object ["error" .= kind, "message" .= message])
  , errHeaders = [("Content-Type", "application/json")]
  }

decodeLifecycleDetail :: Text -> Value
decodeLifecycleDetail detail = fromMaybe (Aeson.String detail) $
  Aeson.decode (LBS8.pack (Text.unpack detail))

newtype CreateObservationRequest = CreateObservationRequest (Either Text CreateObservation)
newtype ReviewedObservationRequest = ReviewedObservationRequest (Either Text ReviewedObservationUpdate)
newtype ObservationMatchRequest = ObservationMatchRequest (Either Text ObservationMatchQuery)
newtype ObservationCountRequest = ObservationCountRequest (Either Text ObservationCountQuery)
newtype LinkDependencyRequest = LinkDependencyRequest (Either Text LinkDependency)
newtype UpdateWorkspaceRequest = UpdateWorkspaceRequest (Either Text UpdateWorkspace)

-- Keep malformed bytes as a validation result so workspace authorization and
-- not-found privacy run before the caller sees a body error.
data TolerantJSON

-- Parsing is deferred until workspace/session authorization has succeeded.
-- Do not retain parser diagnostics: they can contain uploaded record fragments.
newtype AgentSessionRequest a = AgentSessionRequest (Either Text a)
instance FromJSON a => MimeUnrender TolerantJSON (AgentSessionRequest a) where
  mimeUnrender _ body = Right $ AgentSessionRequest $ case Aeson.eitherDecode body of
    Left _ -> Left "Invalid session request JSON"
    Right parsed -> Right parsed

instance Accept TolerantJSON where
  contentType _ = "application" // "json"

instance MimeUnrender TolerantJSON ReviewedObservationRequest where
  mimeUnrender _ body = Right $ ReviewedObservationRequest $
    case Aeson.eitherDecode body of
      Left message -> Left (Text.pack message)
      Right value -> case Aeson.fromJSON value of
        Aeson.Error message -> Left (Text.pack message)
        Aeson.Success parsed -> Right parsed

instance MimeUnrender TolerantJSON UpdateWorkspaceRequest where
  mimeUnrender _ body = Right $ UpdateWorkspaceRequest $
    case Aeson.eitherDecode body of
      Left message -> Left (Text.pack message)
      Right value -> case Aeson.fromJSON value of
        Aeson.Error message -> Left (Text.pack message)
        Aeson.Success parsed -> Right parsed

instance MimeUnrender TolerantJSON LinkDependencyRequest where
  mimeUnrender _ body = Right $ LinkDependencyRequest $
    case Aeson.eitherDecode body of
      Left message -> Left (Text.pack message)
      Right value -> case Aeson.fromJSON value of
        Aeson.Error message -> Left (Text.pack message)
        Aeson.Success parsed -> Right parsed

instance FromJSON CreateObservationRequest where
  parseJSON value = pure $ CreateObservationRequest $ case Aeson.fromJSON value of
    Aeson.Error message -> Left (Text.pack message)
    Aeson.Success parsed -> Right parsed

instance FromJSON ObservationMatchRequest where
  parseJSON value = pure $ ObservationMatchRequest $ case Aeson.fromJSON value of
    Aeson.Error message -> Left (Text.pack message)
    Aeson.Success parsed -> Right parsed

instance FromJSON ObservationCountRequest where
  parseJSON value = pure $ ObservationCountRequest $ case Aeson.fromJSON value of
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

requireAuthenticated :: Handler ()
requireAuthenticated = liftIO currentPrincipal >>= maybe (throwError err401) (const (pure ()))

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

-- | The workspace rename proxy is a public mutation boundary.  Unlike most
-- older endpoints, it promises a structured error body to its REST and MCP
-- callers.  Keep the authorization-first ordering identical to
-- 'requireWorkspace': a caller without edit access must not learn whether a
-- workspace identifier exists, while an authorized caller gets a stable
-- inactive/missing response.
requireWorkspaceRename :: Pool Hasql.Connection -> UUID -> Handler ()
requireWorkspaceRename pool workspaceId = do
  principal <- liftIO currentPrincipal
  authorized <- liftIO $ Auth.authorizeWorkspace pool principal workspaceId Auth.WorkspaceRoleEdit
  either (throwError . workspaceRenameAuthError) pure authorized
  rows <- handleDBErrors $ runSession pool $ Session.statement () $ run $ select $ do
    row <- each workspaceSchema
    where_ $ row.wsId ==. lit workspaceId
    where_ $ activeWorkspace row
    pure row.wsId
  case rows of [] -> throwError workspaceRenameNotFound; _ -> pure ()

workspaceRenameAuthError :: Auth.AuthorizationError -> ServerError
workspaceRenameAuthError = \case
  Auth.MissingPrincipal -> workspaceRenameError err401 "unauthorized" "Authentication is required."
  Auth.EntityScopeNotFound{} -> workspaceRenameNotFound
  _ -> workspaceRenameError err403 "forbidden" "Workspace edit permission is required."

workspaceRenameNotFound :: ServerError
workspaceRenameNotFound = workspaceRenameError err404 "not_found" "Workspace not found."

workspaceRenameError :: ServerError -> Text -> Text -> ServerError
workspaceRenameError status kind message =
  status { errBody = Aeson.encode (object ["error" .= kind, "message" .= message]) }

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

sessionError :: ServerError -> Text -> Text -> ServerError
sessionError status kind message = status
  { errBody = Aeson.encode (object ["error" .= kind, "message" .= message])
  , errHeaders = [("Content-Type","application/json")]
  }

-- Session payloads may contain secrets. Never pass an exception's Show value
-- through the generic database logger or echo database/parser details.
handleSessionErrors :: IO a -> Handler a
handleSessionErrors action = do
  result <- liftIO $ try @DBException (try @AgentSession.SessionException action)
  case result of
    Right (Right value) -> pure value
    Right (Left exception) -> throwError $ case exception.code of
      AgentSession.SessionNotFound -> sessionError err404 "session_not_found" "Session or task not found"
      AgentSession.SessionTaskNotFound -> err404
        { errBody = Aeson.encode (object ["error" .= ("session_task_not_found" :: Text), "field" .= ("task_id" :: Text), "message" .= ("task_id must identify a live task in this workspace. Start it before creating a session." :: Text)])
        , errHeaders = [("Content-Type","application/json")] }
      AgentSession.SessionConflict -> sessionError err409 "session_conflict" "Session cursor, receipt or immutable metadata conflicts"
      AgentSession.SessionRuntimeConflict -> sessionError err409 "session_runtime_conflict" "Runtime identity revision conflicts; refresh runtime_revision from session metadata"
      AgentSession.SessionTerminated -> sessionError err409 "session_terminated" "Session is terminated"
      AgentSession.SessionInvalidInput -> sessionError err400 "session_invalid_input" "Invalid session cursor, metadata or bounds"
      AgentSession.SessionTaskNotInProgress status -> err409
        { errBody = Aeson.encode (object ["error" .= ("session_task_not_in_progress" :: Text), "field" .= ("task_id" :: Text), "message" .= ("Start the task before creating or associating a session. Required status is in_progress; current status is " <> status <> "."), "required_status" .= ("in_progress" :: Text), "current_status" .= status])
        , errHeaders = [("Content-Type","application/json")] }
    Left exception -> throwError $ case exception of
      DBCheckViolation{} -> sessionError err400 "session_invalid_input" "Invalid JSONL record or session constraint"
      DBForeignKeyViolation{} -> sessionError err404 "session_not_found" "Session or task not found"
      DBUniqueViolation{} -> sessionError err409 "session_conflict" "Concurrent session mutation conflicts"
      DBSerializationFailure -> sessionError err409 "session_conflict" "Concurrent session mutation conflicts"
      DBStatementTimeout -> sessionError err504 "session_timeout" "Session operation timed out"
      _ -> sessionError err500 "session_internal" "Session operation failed"

requireSessionWorkspace :: Pool Hasql.Connection -> UUID -> Auth.WorkspaceRole -> Handler ()
requireSessionWorkspace pool workspace role = do
  principal <- liftIO currentPrincipal
  allowed <- handleSessionErrors (Auth.authorizeWorkspace pool principal workspace role)
  case allowed of
    Left Auth.MissingPrincipal -> throwError $ sessionError err401 "unauthorized" "Authentication required"
    Left _ -> throwError $ sessionError err403 "forbidden" "Workspace permission required"
    Right () -> pure ()
  active <- handleSessionErrors $ runSession pool $ Session.statement () $ run $ select $ do
    row <- each workspaceSchema
    where_ (row.wsId ==. lit workspace &&. activeWorkspace row)
    pure row.wsId
  case active of [] -> throwError $ sessionError err404 "workspace_not_found" "Active workspace not found"; _ -> pure ()

-- Used by the WAI body limiter before it consumes any session mutation body.
authorizeAgentSessionWrite :: Pool Hasql.Connection -> UUID -> Handler ()
authorizeAgentSessionWrite pool workspace = requireSessionWorkspace pool workspace Auth.WorkspaceRoleEdit

requireAgentSession :: Pool Hasql.Connection -> UUID -> UUID -> Handler AgentSession.AgentSession
requireAgentSession pool workspace identifier =
  handleSessionErrors (AgentSessions.getSession pool workspace identifier)
    >>= maybe (throwError $ sessionError err404 "session_not_found" "Session not found") pure

requireSessionTask :: Pool Hasql.Connection -> UUID -> UUID -> Handler ()
requireSessionTask pool workspace identifier = do
  task <- handleSessionErrors (Task.getTask pool identifier)
  case task of
    Just row | row.workspaceId == workspace -> pure ()
    _ -> throwError $ sessionError err404 "task_not_found" "Active same-workspace task not found"

sessionActivity :: Pool Hasql.Connection -> [Text] -> Maybe Text -> [Text] -> Handler Value
sessionActivity pool rawWorkspaces rawScope rawProjects = do
  principal <- liftIO currentPrincipal
  case principal of Nothing -> throwError $ sessionError err401 "unauthorized" "Authentication required"; _ -> pure ()
  let workspaceTexts=nub rawWorkspaces; projectTexts=nub rawProjects
  when (length workspaceTexts + length projectTexts > 50) $
    throwError $ sessionError err400 "validation_error" "At most 50 activity targets are allowed"
  let parse value=maybe (throwError $ sessionError err400 "validation_error" "Invalid activity target") pure (UUID.fromText value)
  workspaceIds <- mapM parse workspaceTexts
  projectIds <- mapM parse projectTexts
  scope <- mapM parse rawScope
  when (projectIds /= [] && scope == Nothing) $
    throwError $ sessionError err400 "validation_error" "Project workspace is required"
  readable <- fmap catMaybes $ forM workspaceIds $ \identifier -> do
    allowed <- handleSessionErrors $ Auth.authorizeWorkspace pool principal identifier Auth.WorkspaceRoleRead
    pure $ either (const Nothing) (const $ Just identifier) allowed
  workspaceRows <- handleSessionErrors $ AgentSessions.workspaceSessionActivity pool readable
  projectRows <- case scope of
    Just workspace | projectIds /= [] -> do
      requireSessionWorkspace pool workspace Auth.WorkspaceRoleRead
      handleSessionErrors $ AgentSessions.projectSessionActivity pool workspace projectIds
    _ -> pure []
  let row (identifier,active)=object ["id" .= identifier,"active" .= active]
  pure $ object ["workspaces" .= map row workspaceRows,"projects" .= map row projectRows]

authorizeTaskEvidenceRead :: Pool Hasql.Connection -> UUID -> UUID -> Handler ()
authorizeTaskEvidenceRead pool workspace task = do
  requireSessionWorkspace pool workspace Auth.WorkspaceRoleRead
  requireSessionTask pool workspace task

parseSessionParam :: FromHttpApiData a => Text -> Handler a
parseSessionParam value = either (const $ throwError $ sessionError err400 "validation_error" "Invalid session query parameter") pure (parseQueryParam value)

sessionCursor :: Maybe Text -> Maybe Text -> Handler (Maybe AgentSession.SessionPageCursor)
sessionCursor Nothing Nothing = pure Nothing
sessionCursor (Just timestamp) (Just identifier) = Just <$> (AgentSession.SessionPageCursor <$> parseSessionParam timestamp <*> parseSessionParam identifier)
sessionCursor _ _ = throwError $ sessionError err400 "validation_error" "Both before_created_at and before_id are required for continuation"

sessionFilters :: Maybe Text -> Maybe Text -> Handler AgentSession.SessionFilter
sessionFilters rawState rawFreshness = do
  state <- case rawState of
    Nothing -> pure Nothing
    Just "active" -> pure (Just AgentSession.Active)
    Just "terminated" -> pure (Just AgentSession.Terminated)
    _ -> throwError $ sessionError err400 "validation_error" "Session state must be active or terminated"
  freshness <- case rawFreshness of
    Nothing -> pure Nothing
    Just "fresh" -> pure (Just AgentSession.Fresh)
    Just "stale" -> pure (Just AgentSession.Stale)
    _ -> throwError $ sessionError err400 "validation_error" "Session freshness must be fresh or stale"
  pure $ AgentSession.SessionFilter state freshness Nothing Nothing

sessionLimit :: Maybe Text -> Handler Int
sessionLimit input = do
  limit <- maybe (pure 50) parseSessionParam input
  when (limit < 1 || limit > AgentSession.maxSessionPageSize) $
    throwError $ sessionError err400 "validation_error" "Session page limit must be between 1 and 100"
  pure limit

sessionRequest :: AgentSessionRequest a -> Handler a
sessionRequest (AgentSessionRequest result) = either (const $ throwError $ sessionError err400 "validation_error" "Invalid session request JSON") pure result

sessionIO :: Pool Hasql.Connection -> UUID -> IO a -> Handler a
sessionIO _ workspace = handleSessionErrors . withWorkspaceIdContext (Just workspace)

agentSessions :: Maybe Prefix.PrefixRegistry -> Pool Hasql.Connection -> UUID -> Server AgentSessionAPI
agentSessions prefixes pool workspace = listH :<|> facetsH :<|> createH :<|> importH :<|> getH :<|> appendH :<|> heartbeatH :<|> runtimeH :<|> tasksH :<|> associateH :<|> terminateH :<|> rangeH AgentSession.Active :<|> rangeH AgentSession.Terminated :<|> proofH :<|> proofResultH
  where
    readAccess = requireSessionWorkspace pool workspace Auth.WorkspaceRoleRead
    writeAccess = authorizeAgentSessionWrite pool workspace
    proofRegistry = maybe (throwError $ sessionError err503 "verification_unavailable" "Prefix verification service unavailable") pure prefixes
    proofAudience = do
      principal <- liftIO currentPrincipal
      case principal of
        Just original | PrincipalGrantUser identifier <- original.authority ->
          pure ("grant-user:" <> UUID.toText identifier <> ":" <> actorTypeToText original.actorType <> ":" <> original.actorId)
        Just original | PrincipalSyntheticLocalSuperadmin <- original.authority ->
          pure ("local-superadmin:" <> actorTypeToText original.actorType <> ":" <> original.actorId)
        _ -> throwError $ sessionError err401 "unauthorized" "Authentication required"
    proofH identifier request = do
      writeAccess
      _ <- requireAgentSession pool workspace identifier
      audience <- proofAudience
      input <- sessionRequest request
      registry <- proofRegistry
      result <- sessionIO pool workspace $ Prefix.submitPrefixProof registry (Prefix.PrefixTarget workspace identifier audience input)
      either (throwError . proofError) pure result
    proofResultH identifier operation = do
      writeAccess
      _ <- requireAgentSession pool workspace identifier
      audience <- proofAudience
      registry <- proofRegistry
      result <- liftIO $ Prefix.lookupPrefixProof registry workspace identifier audience operation
      maybe (throwError $ sessionError err404 "verification_unknown" "Prefix verification operation unknown; revalidate source with a new operation") pure result
    proofError = \case
      Prefix.PrefixInvalid -> sessionError err400 "verification_target_invalid" "Prefix target must be an exact committed append boundary and SHA-256"
      Prefix.PrefixConflict -> sessionError err409 "verification_operation_conflict" "Operation UUID is already bound to another target"
      Prefix.PrefixBusy -> sessionError err503 "verification_busy" "Prefix verification admission is full"
      Prefix.PrefixClosed -> sessionError err503 "verification_unavailable" "Prefix verification service unavailable"
    listH rawLimit timestamp identifier rawState rawFreshness project task = do
      readAccess
      limit <- sessionLimit rawLimit
      cursor <- sessionCursor timestamp identifier
      filters <- sessionFilters rawState rawFreshness
      pageResult <- sessionIO pool workspace $ AgentSessions.listWorkspaceSessionsFiltered pool workspace (filters {AgentSession.projectId = project, AgentSession.taskId = task}) cursor limit
      pure $ SessionWire.SessionPageResponse pageResult.items pageResult.nextCursor
    facetsH kind project after selected rawLimit = do
      readAccess
      limit <- sessionLimit rawLimit
      isProject <- case kind of
        Just "projects" -> pure True
        Just "tasks" -> pure False
        _ -> throwError $ sessionError err400 "validation_error" "Session filter kind must be projects or tasks"
      sessionIO pool workspace $ AgentSessions.listSessionFacets pool workspace isProject project after selected limit
    createH request = do
      writeAccess
      input <- case request of
        AgentSessionRequest (Right parsed) -> pure parsed
        _ -> throwError $ sessionError err400 "validation_error" "Session creation requires task_id (UUID), a very short title and a format_label; task_id must identify an in_progress task."
      sessionIO pool workspace $ AgentSessions.createSession pool (AgentSession.CreateSession input.id workspace input.formatLabel input.title input.parentSessionId input.initialRuntimeIdentity input.taskId)
    importH request = do
      writeAccess
      input <- sessionRequest request
      let metadata = input.session
      sessionIO pool workspace $ AgentSessions.importSession pool
        (AgentSession.ImportSession (AgentSession.CreateSession metadata.id workspace metadata.formatLabel metadata.title metadata.parentSessionId metadata.initialRuntimeIdentity metadata.taskId) input.finalBytes input.finalSha256)
    getH identifier = readAccess >> requireAgentSession pool workspace identifier
    appendH identifier request = do
      writeAccess
      _ <- requireAgentSession pool workspace identifier
      input <- sessionRequest request
      bytes <- case SessionWire.decodeSessionBytes input.dataBase64 of
        Left SessionWire.SessionBytesTooLarge -> throwError $ sessionError err413 "session_too_large" "Session chunk exceeds 1 MiB"
        Left SessionWire.SessionBase64Invalid -> throwError $ sessionError err400 "validation_error" "data_base64 must use canonical padded standard base64"
        Right value -> pure value
      result <- sessionIO pool workspace $ AgentSessions.appendSession pool workspace identifier
        (AgentSession.AppendSession input.expectedOffset input.expectedRevision bytes input.allowFinalRecord)
      pure $ SessionWire.AppendSessionResponse result.session result.replayed
    heartbeatH identifier = do
      writeAccess
      sessionIO pool workspace $ AgentSessions.heartbeatSession pool workspace identifier
    runtimeH identifier request = do
      writeAccess
      _ <- requireAgentSession pool workspace identifier
      input <- sessionRequest request
      sessionIO pool workspace $ AgentSessions.setRuntimeIdentity pool workspace identifier input
    tasksH identifier rawLimit rawAfter = do
      readAccess
      _ <- requireAgentSession pool workspace identifier
      limit <- sessionLimit rawLimit
      after <- traverse parseSessionParam rawAfter
      result <- sessionIO pool workspace $ AgentSessions.listSessionTasks pool workspace identifier after limit
      pure $ SessionWire.SessionTaskPageResponse result.items result.nextTaskId
    associateH identifier task = do
      writeAccess
      _ <- requireAgentSession pool workspace identifier
      requireSessionTask pool workspace task
      sessionIO pool workspace $ AgentSessions.associateSessionTask pool workspace identifier task
      pure NoContent
    terminateH identifier request = do
      writeAccess
      _ <- requireAgentSession pool workspace identifier
      input <- sessionRequest request
      sessionIO pool workspace $ AgentSessions.terminateSession pool workspace identifier
        (AgentSession.TerminateSession input.expectedOffset input.expectedRevision input.finalBytes input.finalSha256)
    rangeH expectedState identifier rawOffset rawLimit = do
      readAccess
      offset <- maybe (pure 0) (parseSessionParam @Int64) rawOffset
      limit <- maybe (pure SessionWire.defaultSessionRangeBytes) parseSessionParam rawLimit
      result <- sessionIO pool workspace $ AgentSessions.readSessionRange pool workspace identifier offset limit
      when (result.session.state /= expectedState) $
        throwError $ sessionError err409 "session_state_conflict" "Use transcript for active sessions and evidence for terminated sessions"
      pure $ SessionWire.SessionRangeResponse result.session result.offset (SessionWire.encodeSessionBytes result.bytes) result.nextOffset result.hasMore

taskAgentSessions :: Pool Hasql.Connection -> UUID -> UUID -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Handler SessionWire.SessionPageResponse
taskAgentSessions pool workspace task rawLimit timestamp identifier rawState rawFreshness = do
  requireSessionWorkspace pool workspace Auth.WorkspaceRoleRead
  requireSessionTask pool workspace task
  limit <- sessionLimit rawLimit
  cursor <- sessionCursor timestamp identifier
  filters <- sessionFilters rawState rawFreshness
  result <- sessionIO pool workspace $ AgentSessions.listTaskSessionsFiltered pool workspace task filters cursor limit
  pure $ SessionWire.SessionPageResponse result.items result.nextCursor

------------------------------------------------------------------------
-- Handlers
------------------------------------------------------------------------

server :: Config.AuthConfig -> Pool Hasql.Connection -> AccessTracker -> WS.WSState -> Bool -> Server HMemAPI
server authConfig pool tracker wsState =
  serverWithChangeStream authConfig Config.defaultConfig.changeStream pool tracker wsState

serverWithChangeStream :: Config.AuthConfig -> Config.ChangeStreamConfig -> Pool Hasql.Connection -> AccessTracker -> WS.WSState -> Bool -> Server HMemAPI
serverWithChangeStream authConfig changeStreamConfig = serverWithDiscovery authConfig changeStreamConfig Nothing

serverWithDiscovery :: Config.AuthConfig -> Config.ChangeStreamConfig -> Maybe Discovery.ObservationDiscovery -> Pool Hasql.Connection -> AccessTracker -> WS.WSState -> Bool -> Server HMemAPI
serverWithDiscovery = serverWithPrefixProofs Nothing

serverWithPrefixProofs :: Maybe Prefix.PrefixRegistry -> Config.AuthConfig -> Config.ChangeStreamConfig -> Maybe Discovery.ObservationDiscovery -> Pool Hasql.Connection -> AccessTracker -> WS.WSState -> Bool -> Server HMemAPI
serverWithPrefixProofs prefixes authConfig changeStreamConfig discovery pool tracker wsState _ =
       health pool tracker
  :<|> session authConfig pool
  :<|> sessionActivity pool
  :<|> activityTicket pool wsState
  :<|> workspaces prefixes pool
  :<|> groups pool
  :<|> observations pool
  :<|> discoveryH pool discovery
  :<|> discoveryCapabilityH pool discovery
  :<|> projects pool
  :<|> tasks pool
  :<|> search pool
  :<|> audit pool
  :<|> ticket pool wsState
  :<|> resync changeStreamConfig pool
  :<|> canonicalTicket changeStreamConfig pool wsState

-- | Shared authorized boundary for the subsequent hybrid retrieval handlers.
-- The query vector is private, and an FTS hit never suppresses preparation.
prepareObservationDiscovery :: Pool Hasql.Connection -> Maybe Discovery.ObservationDiscovery -> UUID -> Text -> Handler Discovery.DiscoveryQuery
prepareObservationDiscovery pool discovery workspace query = do
  requireObservationWorkspace pool workspace Auth.WorkspaceRoleRead
  case discovery of
    Nothing -> pure (Discovery.DiscoveryFts Discovery.DiscoveryDisabled)
    Just service -> liftIO $ Discovery.prepareDiscoveryQuery service (discoveryStorage pool workspace) query

discoveryCapabilityH :: Pool Hasql.Connection -> Maybe Discovery.ObservationDiscovery -> UUID -> Handler Value
discoveryCapabilityH pool discovery workspace = do
  requireObservationWorkspace pool workspace Auth.WorkspaceRoleRead
  case discovery of
    Nothing -> pure $ toJSON (Discovery.DiscoveryCapability (Just Discovery.DiscoveryDisabled))
    Just service -> toJSON <$> liftIO (Discovery.discoveryCapability service (discoveryStorage pool workspace))

discoveryStorage :: Pool Hasql.Connection -> UUID -> IO (Maybe Bool)
discoveryStorage pool workspace = case parseEmbeddingSpaceFingerprint Config.managedTeiSpaceFingerprint of
  Nothing -> pure Nothing
  Just space -> Observation.discoveryVectorAvailability pool workspace space

discoveryH :: Pool Hasql.Connection -> Maybe Discovery.ObservationDiscovery -> ObservationDiscoveryRequest -> Handler Value
discoveryH pool configured request = do
  requireObservationWorkspace pool request.workspaceId Auth.WorkspaceRoleRead
  reject (validateObservationDiscoveryRequest request)
  service <- maybe (throwError err503 { errBody = Aeson.encode (object ["error" .= ("discovery_unavailable" :: Text)]) }) pure configured
  principal <- liftIO currentPrincipal >>= maybe (throwError err401) pure
  actor <- liftIO currentEffectiveActor
  let audience = TE.decodeUtf8 $ LBS.toStrict $ Aeson.encode
        (actorTypeToText principal.actorType, principal.actorId, show principal.authority,
         fmap (\a -> (actorTypeToText a.actorType,a.actorId,a.botIdentity)) actor)
      resolve = case request.contextToken of
        Just token -> Discovery.lookupDiscoveryContext service audience request.workspaceId token
        Nothing -> case request.filters of
          Nothing -> pure (Left Discovery.DiscoveryContextUnavailable)
          Just raw -> let filters = normalizeObservationDiscoveryFilter raw in
            Discovery.createDiscoveryContextInSession service audience request.workspaceId filters request.replaceContextToken
              ((,) <$> request.searchSession <*> request.generation)
              (Discovery.prepareDiscoveryQuery service (discoveryStorage pool request.workspaceId) (fromMaybe "" filters.contentQuery))
  context <- liftIO resolve >>= either discoveryContextError pure
  prepared <- maybe (discoveryContextError Discovery.DiscoveryContextUnavailable) pure context.preparedQuery
  let semantic = case prepared of Discovery.DiscoveryFts _ -> Nothing; Discovery.DiscoveryHybrid vector space -> Just (vector,space)
  attempted <- liftIO $ try (Observation.discoverObservations pool context.filterInputs request semantic)
  result <- case attempted of
    Right value -> pure value
    Left (DBCapabilityUnavailable _) -> do
      liftIO $ Discovery.retireDiscoveryContext service audience request.workspaceId context.token
      discoveryContextError Discovery.DiscoveryContextUnavailable
    Left failure -> handleDBErrors (throwIO failure)
  -- A filter replacement can retire a context during SQL. Do not publish that
  -- old page after the newer filter lifetime has taken ownership.
  _ <- liftIO (Discovery.lookupDiscoveryContext service audience request.workspaceId context.token) >>= either discoveryContextError pure
  pure $ object
    [ "context_token" .= context.token, "expires_at" .= context.expiresAt
    , "retrieval" .= Discovery.discoveryQueryCapability prepared
    , "complete" .= True, "page" .= result ]

discoveryContextError :: Discovery.DiscoveryContextError -> Handler a
discoveryContextError failure = throwError $ case failure of
  Discovery.DiscoveryContextCapacity -> err429
    { errBody = Aeson.encode (object ["error" .= ("discovery_capacity" :: Text),"message" .= ("Discovery is busy; retry the current query." :: Text)]) }
  Discovery.DiscoveryContextUnavailable -> err409
    { errBody = Aeson.encode (object ["error" .= ("discovery_restart_required" :: Text),"message" .= ("Restart the current discovery query." :: Text)]) }

session :: Config.AuthConfig -> Pool Hasql.Connection -> Maybe UUID -> Handler SessionContext
session config pool selectedWorkspace = do
  principal <- liftIO currentPrincipal >>= maybe (throwError err401) pure
  actor <- liftIO currentEffectiveActor
  createAllowed <- liftIO $ Auth.hasGlobalPermission pool (Just principal) Auth.GlobalCreateWorkspace
  superAllowed <- liftIO $ Auth.hasGlobalPermission pool (Just principal) Auth.GlobalSuperadmin
  workspace <- traverse (sessionWorkspace pool principal superAllowed) selectedWorkspace
  pure SessionContext
    { authMode = case config.mode of Config.AuthModeLocal -> "local"; Config.AuthModeDeployed -> "deployed"
    , principal = SessionPrincipal { actorType = actorTypeToText principal.actorType, actorId = principal.actorId
                                   , actorLabel = principal.actorLabel, authority = authorityText principal.authority
                                   , grantUserId = case principal.authority of PrincipalGrantUser userId -> Just userId; _ -> Nothing }
    , globalPermissions = SessionGlobalPermissions { createWorkspace = createAllowed, superadmin = superAllowed }
    , effectiveActor = fmap (\a -> SessionActor (actorTypeToText a.actorType) a.actorId a.actorLabel a.botIdentity) actor
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

-- | Demand guards both database selection and summary decoration. A skipped
-- page has no membership authority for clients that did not request it.
navigationPageWhenRequested :: Monad m => Bool -> Int -> m [a] -> ([a] -> m [b]) -> m (NavigationPage b)
navigationPageWhenRequested requested pageSize selectRows decorate
  | not requested = pure NavigationPage { items = [], hasMore = False }
  | otherwise = do
      rows <- selectRows
      summaries <- decorate (take pageSize rows)
      pure NavigationPage { items = summaries, hasMore = length rows > pageSize }

navigationOwnerOutcomes :: [NavigationBatchItem] -> (NavigationBatchItem -> Handler NavigationBranchResponse) -> Handler [NavigationBatchOutcome]
navigationOwnerOutcomes requests readOwner = forM requests $ \item -> do
  outcome <- liftIO $ runHandler (readOwner item)
  pure $ case outcome of
    Right result -> NavigationBatchOutcome { requestId = item.requestId, parent = item.parent, status = 200, response = Just result, error = Nothing }
    Left failure -> NavigationBatchOutcome { requestId = item.requestId, parent = item.parent, status = errHTTPCode failure, response = Nothing
      , error = Just (if errHTTPCode failure == 404 then "Navigation owner was not found" else "Navigation owner read failed") }

workspaces :: Maybe Prefix.PrefixRegistry -> Pool Hasql.Connection -> Server WorkspaceAPI
workspaces prefixes pool = listH :<|> createH :<|> getH :<|> updateH :<|> deleteH :<|> planningCountsH :<|> navigationH :<|> navigationBatchH :<|> focusH :<|> summariesH :<|> timelineBucketsH :<|> timelineH :<|> agentSessions prefixes pool :<|> taskAgentSessions pool where
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
  updateH workspaceId (UpdateWorkspaceRequest requestBody) = do
    requireWorkspaceRename pool workspaceId
    input <- decodeRequest requestBody
    reject (validateUpdateWorkspaceInput input)
    handleDBErrors (Workspace.renameWorkspace pool workspaceId input) >>= maybe (throwError workspaceRenameNotFound) pure
  deleteH workspaceId = do
    requireWorkspace pool workspaceId Auth.WorkspaceRoleAdmin
    deleted <- handleDBErrors $ Workspace.deleteWorkspace pool workspaceId
    if deleted then pure NoContent else throwError err404
  planningCountsH workspaceId = do
    requireWorkspace pool workspaceId Auth.WorkspaceRoleRead
    handleDBErrors (Workspace.getWorkspacePlanningCounts pool workspaceId) >>= maybe (throwError err404) pure
  navigationH workspaceId maybeKind maybeParent projectLimit projectOffset taskLimit taskOffset includeProjects includeTasks maybeShowOnly projectStatuses taskStatuses maybeShowEmptyProjects maybeHasActiveSession maybePriorityMode maybePriorityValue maybeQuery = do
    requireWorkspace pool workspaceId Auth.WorkspaceRoleRead
    parent <- navigationParent maybeKind maybeParent
    reject (validateNavigationPage projectLimit projectOffset <> validateNavigationPage taskLimit taskOffset)
    reject (validateNavigationFilter maybeShowOnly maybePriorityMode maybePriorityValue maybeQuery)
    let filters = NavigationFilter { showOnly = maybeShowOnly, projectStatuses = projectStatuses, taskStatuses = taskStatuses, priorityMode = maybePriorityMode, priorityValue = maybePriorityValue, query = maybeQuery, showEmptyProjects = maybeShowEmptyProjects, hasActiveSession = maybeHasActiveSession }
    navigationPageH workspaceId parent projectLimit projectOffset taskLimit taskOffset includeProjects includeTasks filters

  navigationBatchH workspaceId (batch :: NavigationBatchRequest) = do
    requireWorkspace pool workspaceId Auth.WorkspaceRoleRead
    reject (validateNavigationBatchRequest batch)
    let filters = fromMaybe (NavigationFilter Nothing [] [] Nothing Nothing Nothing Nothing Nothing) batch.filters
    reject (validateNavigationFilter filters.showOnly filters.priorityMode filters.priorityValue filters.query)
    outcomes <- navigationOwnerOutcomes batch.requests $ \item ->
      navigationPageH workspaceId item.parent item.projectLimit item.projectOffset item.taskLimit item.taskOffset item.includeProjects item.includeTasks filters
    pure NavigationBatchResponse { workspaceId = workspaceId, outcomes = outcomes }

  navigationPageH workspaceId parent projectLimit projectOffset taskLimit taskOffset includeProjects includeTasks (filters :: NavigationFilter) = do
    let projectTake = navigationTake projectLimit
        projectSkip = navigationOffset projectOffset
        taskTake = navigationTake taskLimit
        taskSkip = navigationOffset taskOffset
    -- Resolve the parent even when neither stream was requested.
    (projectParent, taskProject, taskParent) <- case parent of
      NavigationWorkspaceRoot -> pure (Nothing, Nothing, Nothing)
      NavigationProjectBranch projectId -> do
        project <- navigationProjectInWorkspace workspaceId projectId
        pure (Just project.id, Just project.id, Nothing)
      NavigationTaskBranch taskId -> do
        task <- navigationTaskInWorkspace workspaceId taskId
        pure (Nothing, task.projectId, Just task.id)
    projectPage <- navigationPageWhenRequested (fromMaybe True includeProjects && taskParent == Nothing) projectTake
      (handleDBErrors $ Project.listFilteredProjectSummaryRows pool workspaceId projectParent filters (projectTake + 1) projectSkip)
      (handleDBErrors . Overview.projectCardSummariesFromRows pool)
    taskPage <- navigationPageWhenRequested (fromMaybe True includeTasks && filters.showOnly /= Just "projects") taskTake
      (handleDBErrors $ Task.listFilteredTaskSummaryRows pool workspaceId taskProject taskParent filters (taskTake + 1) taskSkip)
      (handleDBErrors . Overview.taskCardSummariesFromRows pool)
    pure NavigationBranchResponse
      { workspaceId = workspaceId, parent = parent, projects = projectPage, tasks = taskPage }

  focusH workspaceId entityType entityId maybeAncestorOffset = do
    requireWorkspace pool workspaceId Auth.WorkspaceRoleRead
    reject (validateNavigationPage (Just maxFocusAncestors) maybeAncestorOffset)
    let ancestorOffset = fromMaybe 0 maybeAncestorOffset
    (targetSummary, rawAncestors) <- case entityType of
      NavigationProject -> do
        project <- navigationProjectInWorkspace workspaceId entityId
        (summary, projectAncestors) <- projectFocusSummaries ancestorOffset workspaceId project
        pure (NavigationProjectSummary summary, map NavigationProjectSummary projectAncestors)
      NavigationTask -> do
        task <- navigationTaskInWorkspace workspaceId entityId
        (summary, projectAncestors, taskAncestors) <- taskFocusSummaries ancestorOffset workspaceId task
        pure
          ( NavigationTaskSummary summary
          , map NavigationProjectSummary projectAncestors ++ map NavigationTaskSummary taskAncestors
          )
    let ancestors = drop ancestorOffset rawAncestors
        truncated = length ancestors > maxFocusAncestors
    pure NavigationFocusResponse
      { workspaceId = workspaceId, target = targetSummary, ancestors = take maxFocusAncestors ancestors
      , ancestorsTruncated = truncated, nextAncestorOffset = if truncated then Just (ancestorOffset + maxFocusAncestors) else Nothing }
  summariesH workspaceId request = do
    requireWorkspace pool workspaceId Auth.WorkspaceRoleRead
    reject (validateNavigationSummariesRequest request)
    -- Resolve the whole accepted batch before decorating it; this deliberately
    -- avoids one entity lookup per invalidation ID.
    projectRows <- handleDBErrors $ Project.getProjectSummaryRowsByIds pool request.projectIds
    taskRows <- handleDBErrors $ Task.getTaskSummaryRowsByIds pool request.taskIds
    let projectsById = Map.fromList [ (value.id, value) | value <- projectRows, value.workspaceId == workspaceId ]
        tasksById = Map.fromList [ (value.id, value) | value <- taskRows, value.workspaceId == workspaceId ]
        projectsWithMissing = [ maybe (Left projectId) Right (Map.lookup projectId projectsById) | projectId <- request.projectIds ]
        tasksWithMissing = [ maybe (Left taskId) Right (Map.lookup taskId tasksById) | taskId <- request.taskIds ]
    projectSummaries <- liftIO $ Overview.projectCardSummariesFromRows pool [ value | Right value <- projectsWithMissing ]
    taskSummaries <- liftIO $ Overview.taskCardSummariesFromRows pool [ value | Right value <- tasksWithMissing ]
    pure NavigationSummariesResponse
      { -- Decoration is deliberately set-based, but its SQL is free to return
        -- rows in database order.  Reapply the accepted request order at the
        -- HTTP boundary so callers can safely associate each result with the
        -- corresponding invalidation without rebuilding a whole workspace.
        projects = orderedSummaries request.projectIds projectSummaries
      , tasks = orderedSummaries request.taskIds taskSummaries
      , missingProjectIds = [ value | Left value <- projectsWithMissing ], missingTaskIds = [ value | Left value <- tasksWithMissing ] }
  orderedSummaries ids values =
    let byId = Map.fromList [ (value.id, value) | value <- values ]
    in mapMaybe (`Map.lookup` byId) ids
  navigationParent maybeKind maybeParent = case (maybeKind, maybeParent) of
    (Just "workspace_root", Nothing) -> pure NavigationWorkspaceRoot
    (Just "project", Just projectId) -> pure (NavigationProjectBranch projectId)
    (Just "task", Just taskId) -> pure (NavigationTaskBranch taskId)
    _ -> throwError (badRequest "validation_error" "parent_kind must be workspace_root without parent_id, or project/task with parent_id")
  navigationTake value = Prelude.min maxNavigationPageSize (fromMaybe 50 value)
  navigationOffset value = Prelude.min maxNavigationOffset (fromMaybe 0 value)
  validateNavigationFilter maybeShowOnly maybePriorityMode maybePriorityValue maybeQuery =
    [ "show_only must be projects or tasks" | Just value <- [maybeShowOnly], value /= "projects" && value /= "tasks" ]
    ++ [ "priority_mode must be any, exact, above, or below" | Just value <- [maybePriorityMode], value `notElem` ["any", "exact", "above", "below"] ]
    ++ [ "priority_value is required for exact, above, or below priority_mode" | Just value <- [maybePriorityMode], value /= "any", maybePriorityValue == Nothing ]
    ++ [ "priority_value must be between 1 and 10" | Just value <- [maybePriorityValue], value < 1 || value > 10 ]
    ++ [ "query must not be blank" | Just value <- [maybeQuery], Text.null (Text.strip value) ]
  navigationProjectInWorkspace workspace projectId = do
    values <- handleDBErrors $ Project.getProjectSummaryRowsByIds pool [projectId]
    case values of
      value : _ | value.workspaceId == workspace -> pure value
      _ -> throwError err404
  navigationTaskInWorkspace workspace taskId = do
    values <- handleDBErrors $ Task.getTaskSummaryRowsByIds pool [taskId]
    case values of
      value : _ | value.workspaceId == workspace -> pure value
      _ -> throwError err404
  -- Focus ancestry is deliberately set-wise: the recursive ID walk is one
  -- query per entity class and all direct counts/readiness rollups are batched.
  -- This keeps a deep direct link bounded instead of recreating N+1 overview
  -- hydration for every breadcrumb segment.
  projectFocusSummaries ancestorOffset workspace project = do
    ancestorIds <- liftIO $ Project.getProjectAncestorIds pool workspace project.id (ancestorOffset + maxFocusAncestors + 1)
    rows <- handleDBErrors $ Project.getProjectSummaryRowsByIds pool (project.id : ancestorIds)
    let byId = Map.fromList [ (value.id, value) | value <- rows, value.workspaceId == workspace ]
        ordered = mapMaybe (`Map.lookup` byId) (project.id : ancestorIds)
    summaries <- liftIO $ Overview.projectCardSummariesFromRows pool ordered
    case summaries of
      targetSummary : ancestorSummaries -> pure (targetSummary, ancestorSummaries)
      [] -> throwError err404
  taskFocusSummaries ancestorOffset workspace task = do
    taskAncestorIds <- liftIO $ Task.getTaskAncestorIds pool workspace task.id (ancestorOffset + maxFocusAncestors + 1)
    taskRows <- handleDBErrors $ Task.getTaskSummaryRowsByIds pool (task.id : taskAncestorIds)
    let taskById = Map.fromList [ (value.id, value) | value <- taskRows, value.workspaceId == workspace ]
        orderedTasks = mapMaybe (`Map.lookup` taskById) (task.id : taskAncestorIds)
    taskSummaries <- liftIO $ Overview.taskCardSummariesFromRows pool orderedTasks
    (targetSummary, taskAncestorSummaries) <- case taskSummaries of
      targetValue : ancestorValues -> pure (targetValue, ancestorValues)
      [] -> throwError err404
    projectAncestorSummaries <- case task.projectId of
      Nothing -> pure []
      Just projectId -> do
        project <- navigationProjectInWorkspace workspace projectId
        ancestorIds <- liftIO $ Project.getProjectAncestorIds pool workspace project.id (ancestorOffset + maxFocusAncestors + 1)
        projectRows <- handleDBErrors $ Project.getProjectSummaryRowsByIds pool (ancestorIds ++ [project.id])
        let projectById = Map.fromList [ (value.id, value) | value <- projectRows, value.workspaceId == workspace ]
        liftIO $ Overview.projectCardSummariesFromRows pool (mapMaybe (`Map.lookup` projectById) (ancestorIds ++ [project.id]))
    pure (targetSummary, projectAncestorSummaries, taskAncestorSummaries)
  timelineBucketsH workspaceId mSince mUntil mBucket mPaged = do
    requireWorkspace pool workspaceId Auth.WorkspaceRoleRead
    untilTime <- requireTimelineBucketParam "until" mUntil
    let bucket = fromMaybe "week" mBucket
        paged = mPaged == Just True
    since <- case mSince of
      Just value -> pure value
      Nothing | paged -> fromMaybe (addUTCTime (-86400) untilTime) <$> handleDBErrors (Timeline.earliestWorkspaceLifecycle pool workspaceId untilTime)
      Nothing -> requireTimelineBucketParam "since" mSince
    let pageUntil = if paged then Prelude.min untilTime (timelineBucketPageEnd since bucket) else untilTime
    reject (validateTimelineBucketQuery since pageUntil bucket)
    buckets <- handleDBErrors $ Timeline.listWorkspaceTimelineBuckets pool workspaceId since pageUntil bucket
    when (length buckets > maxTimelineBuckets) $
      reject ["timeline bucket range produces too many buckets; narrow the range or choose a larger bucket"]
    pure WorkspaceTimelineBucketsResponse
      { timelineBucketsWorkspaceId = workspaceId
      , timelineBucketsSince = since
      , timelineBucketsUntil = untilTime
      , timelineBucketsBucket = bucket
      , timelineBucketsBuckets = buckets
      , timelineBucketsNextSince = if pageUntil < untilTime then Just pageUntil else Nothing }
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

-- Each opt-in page keeps the existing ten-year and 366-bucket query limits.
-- UTC bucket boundaries prevent partial buckets from being counted twice.
timelineBucketPageEnd :: UTCTime -> Text -> UTCTime
timelineBucketPageEnd since bucket = UTCTime end 0
  where
    day = utctDay since
    (year, month, _) = toGregorian day
    end = case bucket of
      "day" -> addDays 366 day
      "week" -> addDays (366 * 7) (addDays (negate (toInteger (fromEnum (dayOfWeek day)) - toInteger (fromEnum Monday))) day)
      "month" -> addGregorianMonthsClip 120 (fromGregorian year month 1)
      "quarter" -> addGregorianMonthsClip 120 (fromGregorian year (((month - 1) `div` 3) * 3 + 1) 1)
      _ -> addDays 1 day

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
observations pool = listH :<|> createH :<|> subjectFacetsH :<|> matchH :<|> similarH :<|> subjectTreeH :<|> countH :<|> getH :<|> historyH :<|> updateH :<|> deleteH :<|> embeddingH where
  countH scope (ObservationCountRequest requestBody) = do
    value <- decodeRequest requestBody
    requireObservationWorkspace pool value.workspaceId Auth.WorkspaceRoleRead
    reject (validateObservationCountQuery value)
    handleDBErrors $ Observation.countObservationsScoped pool (fromMaybe ObservationCombined scope) value
  listH workspaceId kind subjectValue sha currentSha historySha queryValue scope limit offset = do
    requireObservationWorkspace pool workspaceId Auth.WorkspaceRoleRead
    -- Validate client-supplied values before pagination defaults/caps are
    -- applied; otherwise invalid bounds would be silently normalized.
    let rawQuery = ObservationQuery workspaceId kind subjectValue sha queryValue limit offset currentSha historySha
    reject (validateObservationQuery rawQuery)
    let (takeN, skipN) = page limit offset
        query = ObservationQuery workspaceId kind subjectValue sha queryValue (Just takeN) (Just skipN) currentSha historySha
    rows <- handleDBErrors $ Observation.listObservationsScopedOverfetch pool (fromMaybe ObservationCombined scope) query
    pure PaginatedResult { items = take takeN rows, hasMore = length rows > takeN }
  createH (CreateObservationRequest requestBody) = do
    input <- decodeRequest requestBody
    requireObservationWorkspace pool input.workspaceId Auth.WorkspaceRoleEdit
    reject (validateCreateObservationInput input)
    created <- handleDBErrors $ withWorkspaceIdContext (Just input.workspaceId) (Observation.createObservation pool input)
    pure created
  subjectFacetsH workspaceId kind sha currentSha historySha queryValue limit offset = do
    requireObservationWorkspace pool workspaceId Auth.WorkspaceRoleRead
    let rawQuery = ObservationSubjectFacetQuery workspaceId kind sha queryValue limit offset currentSha historySha
    reject (validateObservationSubjectFacetQuery rawQuery)
    let (takeN, skipN) = page limit offset
        pagedQuery = ObservationSubjectFacetQuery workspaceId kind sha queryValue (Just takeN) (Just skipN) currentSha historySha
    rows <- handleDBErrors $ Observation.listObservationSubjectFacetsOverfetch pool pagedQuery
    pure PaginatedResult { items = take takeN rows, hasMore = length rows > takeN }
  subjectTreeH workspaceId prefix subjectQuery kind sha currentSha historySha limit offset = do
    requireObservationWorkspace pool workspaceId Auth.WorkspaceRoleRead
    let query = ObservationSubjectTreeQuery workspaceId (fromMaybe "" prefix) subjectQuery kind sha currentSha historySha limit offset
    reject (validateObservationSubjectTreeQuery query)
    handleDBErrors $ Observation.listObservationSubjectTree pool query
  matchH (ObservationMatchRequest requestBody) = do
    query <- decodeRequest requestBody
    requireObservationWorkspace pool query.workspaceId Auth.WorkspaceRoleRead
    reject (validateObservationMatchQuery query)
    let (takeN, skipN) = page query.limit query.offset
        pagedQuery = ObservationMatchQuery
          query.workspaceId query.paths query.subjectKind query.gitSha query.query
          (Just takeN) (Just skipN) query.currentGitSha query.historyGitSha
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
  historyH observationId limit offset = do
    workspaceId <- requireEntity pool Auth.EntityObservation observationId Auth.WorkspaceRoleRead
    requireObservationWorkspace pool workspaceId Auth.WorkspaceRoleRead
    _ <- handleDBErrors (Observation.getObservation pool workspaceId observationId) >>= maybe (throwError err404) pure
    let (takeN, _) = page limit offset
    rows <- handleDBErrors $ Observation.listObservationHistoryOverfetch pool workspaceId observationId limit offset
    pure PaginatedResult { items = take takeN rows, hasMore = length rows > takeN }
  updateH observationId ifMatch (ReviewedObservationRequest requestBody) = do
    workspaceId <- requireEntity pool Auth.EntityObservation observationId Auth.WorkspaceRoleEdit
    requireObservationWorkspace pool workspaceId Auth.WorkspaceRoleEdit
    input <- decodeRequest requestBody
    reject (validateReviewedObservationUpdate input)
    supplied <- maybe (throwError missingObservationPrecondition) pure ifMatch
    version <- maybe (throwError invalidIfMatch) pure (parseObservationIfMatch supplied)
    result <- handleDBErrors (Observation.updateObservationReviewed pool workspaceId observationId version input)
    case result of
      ObservationUpdated updated -> pure updated
      ObservationNotFound -> throwError err404
      ObservationVersionMismatch latest -> throwError err409
        { errBody = Aeson.encode (object ["code" .= ("observation_content_conflict" :: Text), "latest" .= latest])
        , errHeaders = [("Content-Type", "application/json")]
        }
  deleteH observationId = do
    workspaceId <- requireEntity pool Auth.EntityObservation observationId Auth.WorkspaceRoleEdit
    requireObservationWorkspace pool workspaceId Auth.WorkspaceRoleEdit
    deleted <- handleDBErrors (Observation.deleteObservation pool workspaceId observationId)
    if deleted then pure NoContent else throwError err404
  embeddingH observationId (ObservationEmbedding vector suppliedSpace) = do
    workspaceId <- requireEntity pool Auth.EntityObservation observationId Auth.WorkspaceRoleEdit
    requireObservationWorkspace pool workspaceId Auth.WorkspaceRoleEdit
    -- Verify existence so a no-op UPDATE is never reported as success.
    _ <- handleDBErrors (Observation.getObservation pool workspaceId observationId) >>= maybe (throwError err404) pure
    handleDBErrors $ Embedding.setObservationEmbeddingInSpace pool workspaceId observationId (fromMaybe legacyManualEmbeddingSpace suppliedSpace) vector
    pure NoContent

-- Only the canonical opaque UUID token is supported, with HTTP outer OWS.
-- No tag-list, wildcard, weak-tag or timestamp/request-ID interpretation.
parseObservationIfMatch :: Text -> Maybe UUID
parseObservationIfMatch supplied = do
  token <- Text.stripPrefix "\"" trimmed >>= Text.stripSuffix "\""
  version <- UUID.fromText token
  if UUID.toText version == token then Just version else Nothing
  where
    trimmed = Text.dropAround (\c -> c == ' ' || c == '\t') supplied

missingObservationPrecondition :: ServerError
missingObservationPrecondition = ServerError 428 "Precondition Required"
  (Aeson.encode (object ["code" .= ("observation_content_precondition_required" :: Text), "message" .= ("If-Match content_version is required" :: Text)]))
  [("Content-Type", "application/json")]

invalidIfMatch :: ServerError
invalidIfMatch = badRequest "validation_error" "If-Match must contain one strong quoted canonical content_version UUID"

projects :: Pool Hasql.Connection -> Server ProjectAPI
projects pool = listH :<|> createH :<|> createSpecH :<|> getH :<|> updateH :<|> deleteH :<|> overviewH :<|> nextPageH :<|> nextH where
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
  createSpecH input = do
    requireWorkspace pool input.workspaceId Auth.WorkspaceRoleEdit
    reject (validateCreateProjectSpecInput input)
    handleDBErrors $ Project.createProjectSpec pool input
  getH projectId = do
    _ <- requireEntity pool Auth.EntityProject projectId Auth.WorkspaceRoleRead
    handleDBErrors (Project.getProject pool projectId) >>= maybe (throwError err404) pure
  updateH projectId input = do
    workspaceId <- requireEntity pool Auth.EntityProject projectId Auth.WorkspaceRoleEdit; reject (validateUpdateProjectInput input)
    (updated, projectChanges, before, after, own, ancestor) <- handleDBErrors
      (Project.updateProjectWithSnapshots pool projectId input (Task.projectPauseSnapshotsS projectId))
      >>= maybe (throwError err404) pure
    let taskChanges = dependencyStatusChanges "project_updated" before after
        result = ProjectMutationResult updated $ ProjectPauseEffects
          (length projectChanges) (length taskChanges) (take 20 projectChanges) (take 20 taskChanges)
          (length projectChanges > 20) (length taskChanges > 20) own ancestor
    pure result
  deleteH projectId = do
    workspaceId <- requireEntity pool Auth.EntityProject projectId Auth.WorkspaceRoleEdit
    handleDBErrors (Project.deleteProjectCascade pool projectId) >>= maybe (throwError err404) pure
  overviewH projectId = do
    _ <- requireEntity pool Auth.EntityProject projectId Auth.WorkspaceRoleRead
    handleDBErrors (Overview.getProjectOverview pool projectId) >>= maybe (throwError err404) pure
  nextPageH projectId limit offset includeBlocked = do
    _ <- requireEntity pool Auth.EntityProject projectId Auth.WorkspaceRoleRead
    handleDBErrors $ Task.listNextTaskPage pool projectId (fromMaybe False includeBlocked) (fromMaybe 50 limit) (fromMaybe 0 offset)
  nextH projectId limit includeBlocked = do
    _ <- requireEntity pool Auth.EntityProject projectId Auth.WorkspaceRoleRead
    handleDBErrors $ Task.listNextTasks pool projectId (fromMaybe False includeBlocked) (fromMaybe 5 limit)

tasks :: Pool Hasql.Connection -> Server TaskAPI
tasks pool = listH :<|> createH :<|> batchMoveH :<|> getH :<|> updateH :<|> deleteH :<|> overviewH :<|> dependenciesH :<|> addDependencyH :<|> removeDependencyH where
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
  batchMoveH input = do
    requireAuthenticated
    reject (validateBatchMoveTasksRequest input)
    taskWorkspaces <- mapM (\taskId -> requireEntity pool Auth.EntityTask taskId Auth.WorkspaceRoleEdit) input.taskIds
    workspaceId <- case taskWorkspaces of
      [] -> throwError err400
      firstWorkspace : remaining -> do
        when (any (/= firstWorkspace) remaining) $
          reject ["task_ids must all belong to the same workspace"]
        pure firstWorkspace
    case input.projectId of
      Nothing -> pure ()
      Just projectId -> do
        projectWorkspace <- requireEntity pool Auth.EntityProject projectId Auth.WorkspaceRoleEdit
        when (projectWorkspace /= workspaceId) $
          reject ["project_id must belong to the tasks' workspace"]
    affected <- handleDBErrors $ Task.moveTasksBatch pool input.taskIds input.projectId
    pure BatchResult { affected = affected }
  getH taskId = do
    _ <- requireEntity pool Auth.EntityTask taskId Auth.WorkspaceRoleRead
    handleDBErrors (Task.getTask pool taskId) >>= maybe (throwError err404) pure
  updateH taskId input = do
    workspaceId <- requireEntity pool Auth.EntityTask taskId Auth.WorkspaceRoleEdit; reject (validateUpdateTaskInput input)
    result <- handleDBErrors (Task.updateTaskWithDependencySnapshots pool taskId input) >>= maybe (throwError err404) pure
    let (updated, before, after) = result
        effects = dependencyStatusChanges "task_update" before after
    pure TaskMutationResult { task = updated, dependencyEffects = effects }
  deleteH taskId = do
    workspaceId <- requireEntity pool Auth.EntityTask taskId Auth.WorkspaceRoleEdit
    handleDBErrors (Task.deleteTaskCascade pool taskId) >>= maybe (throwError err404) pure
  overviewH taskId = do
    _ <- requireEntity pool Auth.EntityTask taskId Auth.WorkspaceRoleRead
    handleDBErrors (Overview.getTaskOverview pool taskId) >>= maybe (throwError err404) pure
  dependenciesH taskId limit offset = do
    _ <- requireEntity pool Auth.EntityTask taskId Auth.WorkspaceRoleRead
    reject (validateNavigationPage limit offset)
    handleDBErrors $ Overview.listTaskDependencyPage pool taskId (fromMaybe 50 limit) (fromMaybe 0 offset)
  addDependencyH taskId (LinkDependencyRequest requestBody) = do
    taskWorkspace <- requireEntity pool Auth.EntityTask taskId Auth.WorkspaceRoleEdit
    input <- decodeRequest requestBody
    mutateDependencyForTask "add" taskId taskWorkspace input.dependsOnId Task.addDependencyWithSnapshots
  removeDependencyH taskId dependsOnId = mutateDependency "remove" taskId dependsOnId Task.removeDependencyWithSnapshots
  mutateDependency action taskId dependsOnId mutate = do
    taskWorkspace <- requireEntity pool Auth.EntityTask taskId Auth.WorkspaceRoleEdit
    mutateDependencyForTask action taskId taskWorkspace dependsOnId mutate
  mutateDependencyForTask action taskId taskWorkspace dependsOnId mutate = do
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
          && snapshot.openDependencyCount == old.openDependencyCount
          && snapshot.openSubtaskCount == old.openSubtaskCount
          && snapshot.projectPaused == old.projectPaused = Nothing
      | otherwise = Just TaskDependencyStatusChange
          { task = snapshot.task
          , previousStatus = old.task.status
          , currentStatus = snapshot.task.status
          , previousAutoBlocked = old.autoBlocked
          , autoBlocked = snapshot.autoBlocked
          , previousOpenDependencyCount = old.openDependencyCount
          , openDependencyCount = snapshot.openDependencyCount
          , previousOpenSubtaskCount = old.openSubtaskCount
          , openSubtaskCount = snapshot.openSubtaskCount
          , previousProjectPaused = old.projectPaused
          , projectPaused = snapshot.projectPaused
          , reason = dependencyStatusChangeReason old snapshot
           }
      where old = previous snapshot

dependencyStatusChangeReason :: TaskDependencyAutoBlockSnapshot -> TaskDependencyAutoBlockSnapshot -> Text
dependencyStatusChangeReason previous current
  | current.task.status == Blocked && not current.autoBlocked
      && (previous.autoBlocked || previous.task.status /= Blocked) = "manually_blocked"
  | not previous.autoBlocked && current.autoBlocked = "blocked_by_" <> causes current
  | previous.autoBlocked && not current.autoBlocked && current.task.status == Todo =
      "unblocked_" <> resolvedCauses previous <> "_resolved"
  | current.projectPaused && not previous.projectPaused = "project_pause_added"
  | previous.projectPaused && not current.projectPaused = "project_pause_removed"
  | current.openDependencyCount > previous.openDependencyCount = "open_dependency_added"
  | current.openDependencyCount < previous.openDependencyCount = "open_dependency_removed"
  | current.openSubtaskCount > previous.openSubtaskCount = "open_subtask_added"
  | current.openSubtaskCount < previous.openSubtaskCount = "open_subtask_removed"
  | otherwise = "dependency_status_changed"
  where
    causes snapshot
      | not snapshot.projectPaused && snapshot.openDependencyCount == 0 = "open_subtasks"
      | otherwise = Text.intercalate "_and_" $
          ["paused_project" | snapshot.projectPaused] <>
          ["open_dependencies" | snapshot.openDependencyCount > 0] <>
          ["subtasks" | snapshot.openSubtaskCount > 0]
    resolvedCauses snapshot = Text.intercalate "_and_" $
      ["project_pause" | snapshot.projectPaused] <>
      ["dependencies" | snapshot.openDependencyCount > 0] <>
      ["subtasks" | snapshot.openSubtaskCount > 0]

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
        (old, intent) <- projectAuditUpdate entry
        handleDBErrors (Project.updateProjectForAudit pool workspaceId entityId old intent)
          >>= maybe (throwError err409) (pure . Just . toJSON)
      ("project", AuditDelete) -> do
        restored <- handleDBErrors $ Project.restoreProject pool entityId
        if restored then do project <- handleDBErrors (Project.getProject pool entityId); pure (toJSON <$> project) else throwError err409
      ("project", AuditCreate) -> do
        deleted <- handleDBErrors $ Project.deleteProjectCascade pool entityId
        case deleted of Nothing -> throwError err409; Just _ -> pure Nothing
      ("task", AuditUpdate) | isSoftDelete entry -> restoreTask workspaceId entityId
      ("task", AuditUpdate) -> do
        old <- taskAuditUpdate entry
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
  -- Audit snapshots are private evidence. Replay only changed supported fields
  -- so unchanged placement and status do not establish new lifecycle intent.
  taskAuditUpdate :: AuditLogEntry -> Handler UpdateTask
  taskAuditUpdate entry = do
    old <- auditObject entry.oldValues
    new <- auditObject entry.newValues
    let mutable = ["title", "description", "project_id", "parent_id", "status", "priority", "metadata"]
        changed = [key | key <- mutable, AesonKeyMap.lookup key old /= AesonKeyMap.lookup key new]
        dueChanged = any (\key -> AesonKeyMap.lookup key old /= AesonKeyMap.lookup key new)
          ["due_at", "dueAt", "due", "due_date", "dueDate"]
        projected = Aeson.Object $ AesonKeyMap.fromList
          [(key, fromMaybe Aeson.Null (AesonKeyMap.lookup key old)) | key <- changed]
    when (changed == [] && dueChanged) $ throwError err409
      { errBody = Aeson.encode $ object
          [ "error" .= ("audit_due_operation_obsolete" :: Text)
          , "message" .= ("This historical update only changed due scheduling, which is no longer supported." :: Text) ] }
    case Aeson.fromJSON projected of
      Aeson.Success value -> pure value
      Aeson.Error _ -> throwError err409
  auditObject (Just (Aeson.Object values)) = pure values
  auditObject _ = throwError err409

  projectAuditUpdate :: AuditLogEntry -> Handler (UpdateProject, Maybe Project.ProjectAuditPauseIntent)
  projectAuditUpdate entry = do
    decoded <- (decodeOld entry :: Handler UpdateProject)
    let old :: UpdateProject
        old = if auditField "parent_id" entry.oldValues == auditField "parent_id" entry.newValues
          then decoded { parentId = Unchanged } else decoded
    oldOwn <- privateBoolean "own_paused" entry.oldValues
    newOwn <- privateBoolean "own_paused" entry.newValues
    oldRetained <- privateBoolean "retained_paused" entry.oldValues
    newRetained <- privateBoolean "retained_paused" entry.newValues
    let metadataOnly = auditField "status" entry.oldValues == auditField "status" entry.newValues
          && oldOwn == newOwn && oldRetained == newRetained
    if metadataOnly
      then pure (old { status = Nothing }, Nothing)
      else do
        let wasPaused = old.status == Just ProjPaused
            own = fromMaybe wasPaused oldOwn
        -- Missing legacy own provenance means the old status was an explicit
        -- intent. A known false own flag plus a missing retained flag is
        -- ambiguous: it may be modern inherited or deferred historical pause.
        when (wasPaused && oldOwn == Just False && isNothing oldRetained) $
          throwError err409 { errBody = Aeson.encode $ object
            ["error" .= ("audit_pause_provenance_missing" :: Text)
            ,"message" .= ("This historical transition lacks retained pause provenance; choose the intended project status explicitly." :: Text)] }
        pure (old, Just (Project.ProjectAuditPauseIntent own (fromMaybe False oldRetained)))
  privateBoolean key values = case auditField key values of
    Nothing -> pure Nothing
    Just (Aeson.Bool value) -> pure (Just value)
    Just _ -> throwError err409 { errBody = Aeson.encode $ object
      ["error" .= ("audit_pause_provenance_invalid" :: Text)
      ,"message" .= ("The canonical project audit contains malformed pause provenance." :: Text)] }
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

activityTicket :: Pool Hasql.Connection -> WS.WSState -> Handler WebSocketTicketResponse
activityTicket pool state = do
  principal <- liftIO currentPrincipal >>= maybe (throwError err401) pure
  allowed <- liftIO $ case principal.authority of
    PrincipalSyntheticLocalSuperadmin -> pure True
    PrincipalGrantUser user -> isJust <$> Auth.getUserGrants pool user
    _ -> pure False
  unless allowed $ throwError err401
  issued <- liftIO $ WS.createActivityTicket state principal
  maybe (throwError err503 {errHeaders=[("Retry-After","1")]}) pure issued

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
  let profile = fromMaybe FullV1 request.snapshotProfile
  when (profile == WorkspaceShellV1 && request.scope == ChangeStreamGlobal) $
    throwError (badRequest "validation_error" "workspace_shell_v1 is only valid for workspace scope")
  forM_ request.pageSize $ \pageSize ->
    when (pageSize < 1 || pageSize > 1000) $
      throwError (badRequest "validation_error" "page_size must be between 1 and 1000")
  result <- liftIO $ case request.pageToken of
    Nothing -> do
      let pageSize = fromMaybe 100 request.pageSize
      case request.startIdempotencyKey of
        Nothing -> pure (Left ChangeStream.SnapshotOutOfOrder)
        Just startKey -> do
          begun <- ChangeStream.beginResyncWithStartKeyAndProfile pool (fromIntegral changeStreamConfig.snapshotSessionTtlSeconds) scope' audience startKey pageSize profile (materializeSnapshotWithProfile profile scope')
          case begun of
            Left err -> pure (Left err)
            Right begin -> ChangeStream.readSnapshotPageWithTtls pool (fromIntegral changeStreamConfig.snapshotSessionTtlSeconds) (fromIntegral changeStreamConfig.resumeTokenTtlSeconds) scope' audience begin.snapshotToken pageSize
    Just token -> case request.pageSize of
      Just pageSize -> ChangeStream.readSnapshotPageWithTtls pool (fromIntegral changeStreamConfig.snapshotSessionTtlSeconds) (fromIntegral changeStreamConfig.resumeTokenTtlSeconds) scope' audience (ChangeStream.SnapshotToken token) pageSize
      Nothing -> ChangeStream.readSnapshotPageWithStoredTtls pool (fromIntegral changeStreamConfig.snapshotSessionTtlSeconds) (fromIntegral changeStreamConfig.resumeTokenTtlSeconds) scope' audience (ChangeStream.SnapshotToken token)
  case result of
    Left ChangeStream.ResyncUnauthorized -> throwError err403
    Left _ -> throwError resyncRequired
    Right snapshotPage -> do
      typedItems <- traverse decodeSnapshotItem snapshotPage.snapshotPageItems
      pure ChangeStreamResyncResponse
        { items = typedItems
        , snapshotProfile = snapshotPage.snapshotProfile
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

