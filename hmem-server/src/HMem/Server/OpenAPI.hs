{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | OpenAPI definitions are kept beside the Servant contract so removed
-- endpoints cannot accidentally survive in generated documentation.
module HMem.Server.OpenAPI (openApiSpec) where

import Control.Lens ((&), (.~), (%~), (?~), (^.), at, traversed, _Just)
import Data.Aeson (Value(..))
import Data.HashMap.Strict.InsOrd qualified as InsOrdMap
import Data.HashSet.InsOrd qualified as InsOrdSet
import Data.OpenApi
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import Data.List (isInfixOf)
import Servant.OpenApi (toOpenApi)

import HMem.Server.API (HMemAPI, CreateObservationRequest, ReviewedObservationRequest, ObservationMatchRequest, ObservationCountRequest, LinkDependencyRequest, UpdateWorkspaceRequest, AgentSessionRequest)
import HMem.AgentSession qualified as AgentSession
import HMem.AgentSession.Transport qualified as SessionWire
import HMem.Types
import HMem.BotIdentity (BotIdentity)

openApiSpec :: OpenApi
openApiSpec = toOpenApi (Proxy @HMemAPI)
  & info . title .~ "hmem API"
  & info . version .~ "0.2.0.0"
  & info . description ?~ "Repository-scoped observation API with immutable provenance. Automated callers declare self-reported role/model metadata in X-HMem-Bot-Identity; it grants no permissions and local bootstrap remains tokenless. Known credential bots must declare on unsafe API requests. Session effective_actor is attribution alongside the unchanged authority-bearing principal. Task placement mutations can return structured lifecycle conflicts when project, hierarchy, or dependency invariants reject the requested state."
  & components . schemas . at "LifecycleConflictError" ?~ lifecycleConflictSchema
  & components . schemas . at "ObservationContentConflict" ?~ observationContentConflictSchema
  & components . schemas . at "SessionArchiveManifest" ?~ sessionArchiveManifestSchema
  & paths . at "/api/v1/workspaces/{workspaceId}/tasks/{taskId}/evidence.zip" ?~ taskEvidenceArchivePath
  & paths . at "/api/v1/workspaces/{workspaceId}/sessions/{sessionId}/archive" ?~ sessionArchiveManifestPath
  & paths . at "/api/v1/workspaces/{workspaceId}/sessions/{sessionId}/archive/{generationId}/content" ?~ sessionArchiveContentPath
  & paths . traversed . parameters %~ (<> [Inline botIdentityHeader])
  & paths %~ InsOrdMap.mapWithKey documentSessionPath
  & tags .~ InsOrdSet.fromList
      [ Tag "Observations" (Just "Repository-scoped, provenance-bound observations.") Nothing
      , Tag "Workspace Groups" (Just "Global-superadmin workspace group management.") Nothing
      , Tag "Timeline" (Just "Workspace lifecycle timeline and histogram.") Nothing
      , Tag "Agent Sessions" (Just "Workspace-scoped JSONL capture and immutable task evidence.") Nothing
      ]
  & paths . at "/api/v1/groups" . _Just . get %~ fmap tagWorkspaceGroups
  & paths . at "/api/v1/groups" . _Just . post %~ fmap tagWorkspaceGroups
  & paths . at "/api/v1/groups/{groupId}" . _Just . get %~ fmap tagWorkspaceGroups
  & paths . at "/api/v1/groups/{groupId}" . _Just . delete %~ fmap tagWorkspaceGroups
  & paths . at "/api/v1/groups/{groupId}/members" . _Just . get %~ fmap tagWorkspaceGroups
  & paths . at "/api/v1/groups/{groupId}/members" . _Just . post %~ fmap tagWorkspaceGroups
  & paths . at "/api/v1/groups/{groupId}/members/{workspaceId}" . _Just . delete %~ fmap tagWorkspaceGroups
  & paths . at "/api/v1/observations" . _Just . get %~ fmap tagObservation
  & paths . at "/api/v1/observations" . _Just . post %~ fmap tagObservation
  & paths . at "/api/v1/observations/subject-facets" . _Just . get %~ fmap tagObservation
  & paths . at "/api/v1/observations/subject-tree" . _Just . get %~ fmap tagObservation
  & paths . at "/api/v1/observations/count" . _Just . post %~ fmap tagObservation
  & paths . at "/api/v1/observations/match" . _Just . post %~ fmap tagObservation
  & paths . at "/api/v1/observations/similar" . _Just . post %~ fmap tagObservation
  & paths . at "/api/v1/observations/{observationId}" . _Just . get %~ fmap tagObservation
  & paths . at "/api/v1/observations/{observationId}" . _Just . put %~ fmap (documentObservationUpdate . tagObservation)
  & paths . at "/api/v1/observations/{observationId}" . _Just . delete %~ fmap tagObservation
  & paths . at "/api/v1/observations/{observationId}/history" . _Just . get %~ fmap tagObservation
  & paths . at "/api/v1/observations/{observationId}/embedding" . _Just . put %~ fmap tagObservation
  & paths . at "/api/v1/search" . _Just . post %~ fmap documentUnifiedSearch
  & paths . at "/api/v1/workspaces/{workspaceId}" . _Just . put %~ fmap tagWorkspaceRename
  & paths . at "/api/v1/workspaces/{workspaceId}/planning-counts" . _Just . get %~ fmap tagPlanningCounts
  & paths . at "/api/v1/workspaces/{workspaceId}/timeline" . _Just . get %~ fmap tagTimeline
  & paths . at "/api/v1/workspaces/{workspaceId}/timeline/buckets" . _Just . get %~ fmap tagTimeline
  & paths . at "/api/v1/workspaces/{workspaceId}/navigation" . _Just . get . _Just . parameters . traversed %~ capNavigationParameter
  & paths . at "/api/v1/workspaces/{workspaceId}/navigation/focus/{entityType}/{entityId}" . _Just . get . _Just . parameters . traversed %~ capNavigationParameter
  & paths . at "/api/v1/projects/{projectId}" . _Just . put %~ fmap documentProjectPause
  & paths . at "/api/v1/audit/{auditId}/revert" . _Just . post . _Just . description ?~ "Authorized project replay restores canonical independent and retained pause intent. Ambiguous legacy transitions return audit_pause_provenance_missing (409); malformed canonical provenance returns audit_pause_provenance_invalid (409). Metadata replays preserve current pause intent. Task replay restores only supported fields changed by the historical operation. Due-only historical task updates return audit_due_operation_obsolete (409) without entity, timestamp, audit or outbox writes. Stored audit payloads remain unchanged. Lifecycle guards remain authoritative."
  & paths . at "/api/v1/tasks/batch-move" . _Just . post %~ fmap documentTaskBatchMove
  & paths . at "/api/v1/tasks/{taskId}/dependencies" . _Just . get . _Just . parameters . traversed %~ capNavigationParameter
  & paths . at "/api/v1/tasks/{taskId}/dependencies" . _Just . post %~ fmap documentDependencyAdd
  & paths . at "/api/v1/tasks/{taskId}/dependencies/{dependsOnId}" . _Just . delete %~ fmap documentDependencyRemove
  where
    documentSessionPath path item
      | "/archive" `isInfixOf` path = item
      | "/sessions" `isInfixOf` path = let document operation = documentSession operation
                                              & parameters . traversed %~ sessionParameter path
          in item & get %~ fmap document & post %~ fmap document & put %~ fmap document
      | otherwise = item
    sessionParameter path parameterRef = case parameterRef of
      Inline parameter -> let bound = if "/transcript" `isInfixOf` path || "/evidence" `isInfixOf` path then 1048576 else 100
          in case parameter ^. name of
            "limit" -> Inline (parameter & schema ?~ Inline (mempty & type_ ?~ OpenApiInteger & minimum_ ?~ 1 & maximum_ ?~ bound))
            "offset" -> Inline (parameter & schema ?~ Inline (mempty & type_ ?~ OpenApiInteger & format ?~ "int64" & minimum_ ?~ 0))
            "state" -> Inline (parameter & description ?~ "Lifecycle filter applied before keyset pagination: active or terminated. Omit for both.")
            "freshness" -> Inline (parameter & description ?~ "Heartbeat filter applied before keyset pagination: fresh or stale (30 seconds), independently of lifecycle. Omit for both.")
            "before_created_at" -> Inline (parameter & schema ?~ Inline (mempty & type_ ?~ OpenApiString & format ?~ "date-time"))
            name | name `elem` ["before_id","after_task_id"] -> Inline (parameter & schema ?~ Inline (mempty & type_ ?~ OpenApiString & format ?~ "uuid"))
            _ -> parameterRef
      _ -> parameterRef
    documentSession operation = operation
      & tags .~ InsOrdSet.singleton "Agent Sessions"
      & description ?~ "Workspace read permission for ordinary GETs; edit permission for prefix-proof GET and all mutations, with normal cookie CSRF and bot attribution. Session ownership is immutable and task links are additive within the workspace. Historical POST /sessions/imports binds metadata plus final_bytes/final_sha256 to a visible task in any status without changing it; pending imports are hidden from lists/activity, runtime/manifest are immutable and every seal must match the full manifest. Live create requires task_id for a live same-workspace in_progress task and atomically commits its initial association with the new session. It also uses caller id, format_label, a very short title, optional immutable parent and initial_runtime_identity for new-session target-model declarations. Existing-session retries preserve links without requiring the task to remain in_progress. Creator attribution remains the connection caller. PUT runtime requires an explicit nullable runtime_identity and expected_runtime_revision; it changes only active runtime metadata, not transcript revision/bytes/hash or activity. Lists use limit 1..100 (default50), paired before_created_at/before_id keyset cursors; session tasks use after_task_id. Ranges use offset>=0, limit1..1048576 (default65536). Transcript requires active state; evidence requires terminated state and includes the final receipt. Appends require expected_offset/expected_revision and canonical padded data_base64<=1398104 characters/1MiB decoded. Session mutation requests are capped at2MiB before JSON decode, including chunked bodies. Terminate independently verifies already committed prefix size/SHA256 and cursor; it never uploads or changes task status. An exact create/append/terminate retry is safe."
      & responses %~ (<> Responses Nothing (InsOrdMap.fromList
          [(400, Inline (mempty & description .~ "Invalid JSONL/base64/cursors/limits or missing credential-bot declaration; no mutation."))
          ,(401, Inline (mempty & description .~ "Authentication required."))
          ,(403, Inline (mempty & description .~ "Workspace permission or cookie CSRF verification required."))
          ,(404, Inline (mempty & description .~ "Authorized workspace/session/task missing, foreign or deleted."))
          ,(409, Inline (mempty & description .~ "Cursor, replay, receipt or session state conflict; no mutation."))
          ,(413, Inline (mempty & description .~ "Encoded body/base64 or decoded chunk exceeds a published limit; no mutation."))
          ,(504, Inline (mempty & description .~ "Session database operation timed out."))]))
    documentProjectPause operation = operation
      & description ?~ "Pausing cascades inherited pause to eligible descendant projects and automatic blocking to unfinished tasks/subtasks. Running descendants refuse the entire pause. Closed projects must reopen first. Resume returns eligible automatic blocks to todo while preserving manual, dependency, subtask and independent/ancestor pause causes. The flattened canonical Project response includes bounded pause_effects."
      & responses %~ (<> Responses Nothing (InsOrdMap.fromList [(409, Inline (mempty & description .~ "Authenticated lifecycle refusal with bounded running-task guidance; no mutation occurred."))]))
    tagObservation operation = operation & tags .~ InsOrdSet.singleton "Observations"
    documentObservationUpdate operation = operation
      & parameters . traversed %~ documentIfMatch
      & responses %~ (<> observationUpdateErrors)
    documentIfMatch parameterRef = case parameterRef of
      Inline parameter | parameter ^. name == "If-Match" -> Inline $ parameter
        & required ?~ True
        & description ?~ "Required strong quoted canonical UUID from content_version. Missing preconditions return 428; weak tags, lists, repeated header lines, wildcard and malformed forms return 400."
        & schema ?~ Inline (mempty & type_ ?~ OpenApiString & pattern ?~ "^[ \\t]*\"[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}\"[ \\t]*$")
      _ -> parameterRef
    observationUpdateErrors = Responses Nothing $ InsOrdMap.fromList
      [ (400, Inline (mempty & description .~ "Invalid reviewed content/SHA JSON or unsupported If-Match token form."))
      , (428, Inline (mempty & description .~ "Required If-Match content_version was omitted; no mutation occurred."))
      , (401, Inline (mempty & description .~ "Authentication is required."))
      , (403, Inline (mempty & description .~ "Edit authorization or deployed cookie CSRF verification failed."))
      , (404, Inline (mempty & description .~ "The authorized Observation is missing or permanently deleted."))
      , (409, Inline (mempty
          & description .~ "The expected content version is stale. No mutation occurred; latest is the canonical Observation for a conscious rebase."
          & content . at "application/json" ?~ (mempty & schema ?~ Ref (Reference "ObservationContentConflict"))))
      ]
    documentUnifiedSearch operation = operation
      & description ?~ "Searches requested observations, projects, and tasks with independent page boundaries. Results retain all three arrays. has_more contains a boolean for each requested plural type; next_offset contains only types with more results. The default limit is 10 per type (allowed 1..200), and the offset defaults to 0 (allowed 0..2147483647). Continue one type by repeating the request with entity_types set to that type and offset set to its next_offset, keeping workspace_id, query, and applicable filters unchanged. If another page would need an offset beyond the supported range, the server returns a structured continuation_limit error instead of an unusable cursor. Offset pagination assumes an unchanged result set between requests."
    tagWorkspaceGroups operation = operation & tags .~ InsOrdSet.singleton "Workspace Groups"
    tagTimeline operation = operation & tags .~ InsOrdSet.singleton "Timeline"
    tagWorkspaceRename operation = operation
      & tags .~ InsOrdSet.singleton "Workspaces"
      & description ?~ "Renames an active workspace. The request body accepts only name; malformed, unauthenticated, forbidden, and missing requests return the standard 400, 401, 403, and 404 responses."
      & responses %~ (<> workspaceRenameErrors)

    tagPlanningCounts operation = operation
      & tags .~ InsOrdSet.singleton "Workspaces"
      & description ?~ "Requires workspace read access. Returns constant-size authoritative totals from one database statement snapshot: live active/paused projects and todo/in_progress/blocked tasks, including unassigned tasks and subtasks. Each row counts once, independently of navigation filters, pagination and overlapping subtree rollups. Deleted and terminal rows are excluded."
      & responses %~ (<> Responses Nothing (InsOrdMap.fromList
          [ (401, Inline (mempty & description .~ "Unauthenticated."))
          , (403, Inline (mempty & description .~ "Forbidden for the requested workspace."))
          , (404, Inline (mempty & description .~ "The active workspace was not found.")) ]))

    workspaceRenameErrors = Responses Nothing $ InsOrdMap.fromList
      [ (400, Inline (mempty & description .~ "Validation error. The JSON body must be an object with exactly one string name field whose value is non-blank and no more than 1024 bytes."))
      , (401, Inline (mempty & description .~ "Unauthenticated."))
      , (403, Inline (mempty & description .~ "Forbidden for the requested workspace."))
      , (404, Inline (mempty & description .~ "The active workspace was not found."))
      ]

    documentDependencyAdd operation = operation
      & description ?~ "Mutates one prerequisite edge between roots or siblings with the same parent. Forbidden hierarchy scopes return HTTP 409 lifecycle_conflict with code TASK_DEPENDENCY_SCOPE_MISMATCH. Explicit dependency cycles return HTTP 400 dependency_cycle; rejected mutations preserve graph and readiness state."
      & responses %~ (<> dependencyAddErrors)

    dependencyAddErrors = Responses Nothing $ InsOrdMap.fromList
      [ (400, Inline (mempty & description .~ "Validation error. A cycle is reported as {error: dependency_cycle, message: Task dependency would create a cycle}; self-edges and cross-workspace edges are also rejected with 400."))
      , (409, Inline (mempty & description .~ "A root/subtask or cross-parent subtask edge is rejected as lifecycle_conflict with code TASK_DEPENDENCY_SCOPE_MISMATCH.")) ]

    documentDependencyRemove operation = operation
      & description ?~ "Removes one prerequisite edge. Self-edges and cross-workspace edges are rejected with 400."
      & responses %~ (<> dependencyRemoveErrors)

    dependencyRemoveErrors = Responses Nothing $ InsOrdMap.fromList
      [ (400, Inline (mempty & description .~ "Validation error. Self-edges and cross-workspace edges are rejected with 400.")) ]

    documentTaskBatchMove operation = operation
      & description ?~ "Atomically moves one to 100 requested tasks and their active descendants to one project or to no project. Every source task and the optional destination project require edit authorization. Callers must include every endpoint of dependency edges that would otherwise cross project boundaries."
      & responses %~ (<> taskBatchMoveErrors)

    taskBatchMoveErrors = Responses Nothing $ InsOrdMap.fromList
      [ (400, Inline (mempty & description .~ "Validation error: task_ids must contain 1 to 100 entries and all authorized source tasks and the destination project must belong to one workspace."))
      , (401, Inline (mempty & description .~ "Authentication is required."))
      , (403, Inline (mempty & description .~ "The principal lacks edit authorization for a requested task or destination project."))
      , (404, Inline (mempty & description .~ "A requested task or destination project is missing, deleted, or not visible."))
      , (409, Inline lifecycleConflictResponse)
      ]

    lifecycleConflictResponse = mempty
      & description .~ "A placement, hierarchy, dependency, completion, or closed-project invariant rejected the atomic move; no tasks were moved."
      & content . at "application/json" ?~ (mempty & schema ?~ Ref (Reference "LifecycleConflictError"))

-- Middleware accepts this optional metadata independently of authentication.
-- Path-level placement makes it discoverable for every generated operation.
botIdentityHeader :: Param
botIdentityHeader = mempty
  & name .~ "X-HMem-Bot-Identity"
  & in_ .~ ParamHeader
  & required ?~ False
  & description ?~ "One JSON object serialized as a header value, with exactly role and model-id strings (see BotIdentity). At most 4096 bytes. Malformed, repeated, oversized or invalid declarations return 400 invalid_bot_identity even on reads. Known credential-authenticated bots must include it on unsafe /api/v1 requests (all methods except GET, HEAD and OPTIONS), otherwise 400 bot_identity_required. Optional for humans and undeclared local bootstrap. Self-reported attribution grants no permissions; original principal, grants, CSRF and stream audiences remain unchanged."
  & schema ?~ Inline (mempty & type_ ?~ OpenApiString
      & example ?~ String "{\"role\":\"Reviewer\",\"model-id\":\"pi:gpt-6.1-sol:extra-high\"}")

capNavigationParameter parameterRef = case parameterRef of
  Inline parameter
    | parameter ^. name `elem` ["include_projects", "include_tasks"] ->
        Inline (parameter & description ?~ "Defaults to true. False skips selection and decoration; its empty page carries no membership or completion authority."
          & schema %~ fmap (\case Inline value -> Inline (value & default_ ?~ Bool True); value -> value))
    | parameter ^. name `elem` ["project_limit", "task_limit", "limit"] ->
        Inline (parameter & schema %~ fmap capNavigationLimitSchema)
    | parameter ^. name `elem` ["project_offset", "task_offset", "ancestor_offset", "offset"] ->
        Inline (parameter & schema %~ fmap capNavigationOffsetSchema)
  _ -> parameterRef

capNavigationLimitSchema schemaRef = case schemaRef of
  Inline limitSchema -> Inline (limitSchema & minimum_ ?~ 1 & maximum_ ?~ fromIntegral maxNavigationPageSize)
  _ -> schemaRef

capNavigationOffsetSchema schemaRef = case schemaRef of
  Inline offsetSchema -> Inline (offsetSchema & minimum_ ?~ 0 & maximum_ ?~ fromIntegral maxNavigationOffset)
  _ -> schemaRef

opts :: SchemaOptions
opts = defaultSchemaOptions { fieldLabelModifier = camelToSnake }

instance ToSchema AgentSession.SessionState where
  declareNamedSchema _ = pure $ NamedSchema (Just "AgentSessionState") (mempty & type_ ?~ OpenApiString & enum_ ?~ ["active","terminated"])
instance ToSchema AgentSession.AgentSession where
  declareNamedSchema proxy = do
    NamedSchema name schema <- genericDeclareNamedSchema opts proxy
    NamedSchema _ runtimeSchema <- declareNamedSchema (Proxy @AgentSession.RuntimeIdentity)
    pure $ NamedSchema name (schema & properties . at "runtime_identity" ?~ Inline (runtimeSchema & nullable ?~ True))
instance ToSchema AgentSession.RuntimeIdentity where
  declareNamedSchema proxy = do
    NamedSchema name schema <- genericDeclareNamedSchema opts proxy
    pure $ NamedSchema name (schema & additionalProperties ?~ AdditionalPropertiesAllowed False
      & description ?~ "Explicit running-agent role and model, separate from connection caller attribution. New nonnull declarations require role and model; legacy stored declarations may lack role. Provider and reasoning are optional.")
instance ToSchema AgentSession.SetRuntimeIdentity where
  declareNamedSchema proxy = do
    NamedSchema name schema <- genericDeclareNamedSchema opts proxy
    NamedSchema _ runtimeSchema <- declareNamedSchema (Proxy @AgentSession.RuntimeIdentity)
    pure $ NamedSchema name (schema & additionalProperties ?~ AdditionalPropertiesAllowed False
      & required .~ ["expected_runtime_revision","runtime_identity"]
      & properties . at "runtime_identity" ?~ Inline (runtimeSchema & required .~ ["model","role"] & nullable ?~ True)
      & description ?~ "Active-session runtime declaration CAS, separate from the transcript revision. Explicit null clears it to unknown. Does not update activity or transcript bytes.")
instance ToSchema AgentSession.SessionAncestor where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema AgentSession.SessionFacet where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema AgentSession.SessionFacetPage where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema AgentSession.SessionPageCursor where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema AgentSession.SessionTask where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema a => ToSchema (AgentSessionRequest a) where declareNamedSchema _ = declareNamedSchema (Proxy @a)
instance ToSchema SessionWire.CreateSessionRequest where
  declareNamedSchema proxy = do
    NamedSchema name schema <- genericDeclareNamedSchema opts proxy
    NamedSchema _ runtimeSchema <- declareNamedSchema (Proxy @AgentSession.RuntimeIdentity)
    pure $ NamedSchema name (schema & additionalProperties ?~ AdditionalPropertiesAllowed False
      & description ?~ "Create a new session and its required task_id association atomically. The same-workspace task must be in_progress. Identical existing-session retries preserve canonical metadata and links without revalidating task status."
      & properties . at "initial_runtime_identity" ?~ Inline (runtimeSchema & required .~ ["model","role"] & nullable ?~ True))
instance ToSchema SessionWire.ImportSessionRequest where
  declareNamedSchema proxy = do
    NamedSchema name schema <- genericDeclareNamedSchema opts proxy
    pure $ NamedSchema name (schema & additionalProperties ?~ AdditionalPropertiesAllowed False)
instance ToSchema SessionWire.AppendSessionRequest where
  declareNamedSchema proxy = do
    NamedSchema name schema <- genericDeclareNamedSchema opts proxy
    pure $ NamedSchema name (schema & additionalProperties ?~ AdditionalPropertiesAllowed False
      & required %~ filter (/="allow_final_record") & properties . at "data_base64" ?~ Inline
      (mempty & type_ ?~ OpenApiString & maxLength ?~ 1398104 & description ?~ "Canonical padded standard base64; at most1048576 decoded bytes."))
instance ToSchema SessionWire.TerminateSessionRequest where
  declareNamedSchema proxy = do
    NamedSchema name schema <- genericDeclareNamedSchema opts proxy
    pure $ NamedSchema name (schema & additionalProperties ?~ AdditionalPropertiesAllowed False
      & properties . at "final_sha256" ?~ Inline (mempty & type_ ?~ OpenApiString & pattern ?~ "^[0-9a-f]{64}$"))
instance ToSchema SessionWire.AppendSessionResponse where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema SessionWire.SessionPageResponse where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema SessionWire.SessionTaskPageResponse where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema SessionWire.SessionRangeResponse where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema SessionWire.PrefixProofRequest where
  declareNamedSchema proxy = do
    NamedSchema name schema <- genericDeclareNamedSchema opts proxy
    pure $ NamedSchema name (schema & additionalProperties ?~ AdditionalPropertiesAllowed False
      & description ?~ "Submit an asynchronous immutable-prefix proof. Exact retries bind operation UUID, authenticated audience, workspace/session, append-boundary offset/revision and SHA-256. Missing/evicted/restarted operations require a new UUID and source revalidation. Both submission and result reads require current edit permission; POST retains cookie CSRF and pre-decode body authorization. Two workers/four live jobs/sixteen retained operations; terminal cache lasts at most five minutes. Deploy server support before dependent MCP."
      & properties . at "sha256" ?~ Inline (mempty & type_ ?~ OpenApiString & pattern ?~ "^[0-9a-f]{64}$"))
instance ToSchema SessionWire.PrefixProofResponse where declareNamedSchema = genericDeclareNamedSchema opts

instance ToSchema Value where declareNamedSchema _ = pure (NamedSchema (Just "JSONValue") mempty)
instance ToSchema a => ToSchema (FieldUpdate a) where declareNamedSchema _ = declareNamedSchema (Proxy @a)
instance ToSchema SubjectKind where
  declareNamedSchema _ = pure $ NamedSchema (Just "SubjectKind") (mempty & type_ ?~ OpenApiString & enum_ ?~ ["file", "glob"])
instance ToParamSchema SubjectKind where toParamSchema _ = mempty & type_ ?~ OpenApiString & enum_ ?~ ["file", "glob"]
instance ToSchema EntitySearchType where
  declareNamedSchema _ = pure $ NamedSchema (Just "EntitySearchType") (mempty & type_ ?~ OpenApiString & enum_ ?~ ["observation", "project", "task"])
instance ToSchema ObservationSearchHit where
  declareNamedSchema _ = do
    NamedSchema name schema <- genericDeclareNamedSchema opts (Proxy @ObservationSearchHit)
    pure $ NamedSchema name (withLegacySubjectProperties schema
      & required %~ (<> ["current_provenance"])
      & properties . at "current_provenance" ?~ Inline (mempty & allOf ?~ [Ref (Reference "ObservationProvenance")] & nullable ?~ True))
instance ToParamSchema EntitySearchType where toParamSchema _ = mempty & type_ ?~ OpenApiString & enum_ ?~ ["observation", "project", "task"]
instance ToSchema ProjectStatus where declareNamedSchema _ = pure $ NamedSchema (Just "ProjectStatus") (mempty & type_ ?~ OpenApiString & enum_ ?~ ["active", "paused", "completed", "archived"])
instance ToParamSchema ProjectStatus where toParamSchema _ = mempty & type_ ?~ OpenApiString
instance ToSchema TaskStatus where declareNamedSchema _ = pure $ NamedSchema (Just "TaskStatus") (mempty & type_ ?~ OpenApiString & enum_ ?~ ["todo", "in_progress", "blocked", "done", "cancelled"])
instance ToParamSchema TaskStatus where toParamSchema _ = mempty & type_ ?~ OpenApiString
instance ToSchema NavigationEntityType where declareNamedSchema _ = pure $ NamedSchema (Just "NavigationEntityType") (mempty & type_ ?~ OpenApiString & enum_ ?~ ["project", "task"])
instance ToParamSchema NavigationEntityType where toParamSchema _ = mempty & type_ ?~ OpenApiString & enum_ ?~ ["project", "task"]
instance ToSchema AuditAction where declareNamedSchema _ = pure $ NamedSchema (Just "AuditAction") (mempty & type_ ?~ OpenApiString & enum_ ?~ ["create", "update", "delete"])
instance ToParamSchema AuditAction where toParamSchema _ = mempty & type_ ?~ OpenApiString
instance ToSchema WorkspaceType where declareNamedSchema _ = pure $ NamedSchema (Just "WorkspaceType") (mempty & type_ ?~ OpenApiString & enum_ ?~ ["repository", "planning", "personal", "organization"])
instance ToSchema UpdateWorkspace where
  declareNamedSchema _ = pure $ NamedSchema (Just "UpdateWorkspace")
    (mempty & type_ ?~ OpenApiObject & required .~ ["name"] & properties .~ InsOrdMap.fromList [("name", Inline (mempty & type_ ?~ OpenApiString))] & additionalProperties ?~ AdditionalPropertiesAllowed False)
instance ToSchema UpdateWorkspaceRequest where declareNamedSchema _ = declareNamedSchema (Proxy @UpdateWorkspace)
instance ToSchema LinkDependencyRequest where declareNamedSchema _ = declareNamedSchema (Proxy @LinkDependency)

instance ToSchema ObservationEmbedding where
  declareNamedSchema _ = pure $ NamedSchema (Just "ObservationEmbedding") $
    mempty & oneOf ?~ [Inline embeddingSchema, Inline embeddingEnvelopeSchema]

instance ToSchema EmbeddingSpaceFingerprint where
  declareNamedSchema _ = pure $ NamedSchema (Just "EmbeddingSpaceFingerprint") $
    mempty & type_ ?~ OpenApiString & minLength ?~ 1 & maxLength ?~ 128 & pattern ?~ "^[^\\s\\x7f]+$"
instance ToSchema ObservationSubject where
  declareNamedSchema _ = pure $ NamedSchema (Just "ObservationSubject") $ mempty
    & type_ ?~ OpenApiObject
    & description ?~ "A canonical repository-relative forward-slash path. File subjects must be concrete paths. Glob subjects use only *, ?, and **: ** must occupy a whole path component, while * and ? match only within one component and never cross '/'."
    & properties . at "subject_kind" ?~ Inline (mempty & type_ ?~ OpenApiString & enum_ ?~ ["file", "glob"])
    & properties . at "subject" ?~ Inline (mempty & type_ ?~ OpenApiString & maxLength ?~ fromIntegral maxObservationSubjectBytes)
    & required .~ ["subject_kind", "subject"]
instance ToSchema Observation where
  declareNamedSchema _ = do
    NamedSchema name schema <- genericDeclareNamedSchema opts (Proxy @Observation)
    pure $ NamedSchema name (withLegacySubjectProperties schema
      & required %~ (<> ["current_provenance"])
      & properties . at "current_provenance" ?~ Inline (mempty & allOf ?~ [Ref (Reference "ObservationProvenance")] & nullable ?~ True)
      & properties . at "content_version" ?~ Inline (uuidSchema
          & description ?~ "Opaque content precondition advanced by every accepted content write. Not ordered; embedding-only writes do not advance it. Send as a strong quoted If-Match token for conditional curation."))
instance ToParamSchema ObservationQueryScope where
  toParamSchema _ = mempty & type_ ?~ OpenApiString & enum_ ?~ ["combined", "content"]
instance ToSchema ObservationSubjectTreeTerminal where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema ObservationSubjectTreeNode where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema ObservationSubjectTreePage where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema ObservationSubjectFacet where
  declareNamedSchema _ = do
    NamedSchema name schema <- genericDeclareNamedSchema opts (Proxy @ObservationSubjectFacet)
    pure $ NamedSchema name $ schema
      & description ?~ "One exact stored subject group identified by the (subject_kind, subject) tuple. observation_count is the distinct Observation count over the complete filtered set before subject-group pagination; latest_updated_at is the newest matching Observation update time."
instance ToSchema CreateObservation where
  declareNamedSchema _ = pure $ NamedSchema (Just "CreateObservation") $ mempty
    & description ?~ "Create with exactly one non-empty canonical `subjects` array or the complete deprecated legacy `subject_kind` plus `subject` pair. The forms are mutually exclusive; missing or half legacy pairs are rejected. Duplicate canonical subjects are de-duplicated in first-occurrence order. Subjects and Git provenance are immutable after creation."
    & oneOf ?~ [Inline canonicalCreateObservationSchema, Inline legacyCreateObservationSchema]
instance ToSchema CreateObservationRequest where declareNamedSchema _ = declareNamedSchema (Proxy @CreateObservation)
instance ToSchema BotIdentity where
  declareNamedSchema _ = pure $ NamedSchema (Just "BotIdentity") (mempty
    & type_ ?~ OpenApiObject & required .~ ["role", "model-id"]
    & additionalProperties ?~ AdditionalPropertiesAllowed False
    & properties . at "role" ?~ Inline (mempty & type_ ?~ OpenApiString
        & description ?~ "Declared role, at most 128 UTF-8 bytes before stripping outer whitespace; no controls or format characters.")
    & properties . at "model-id" ?~ Inline (mempty & type_ ?~ OpenApiString
        & description ?~ "At most 256 UTF-8 bytes; harness:model:reasoning with ASCII letters, digits, dot, underscore, slash or hyphen per component."))

instance ToSchema ObservationProvenanceMatch where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema ObservationProvenance where
  declareNamedSchema _ = do
    NamedSchema name schema <- genericDeclareNamedSchema opts (Proxy @ObservationProvenance)
    pure $ NamedSchema name (schema & required .~ ["sequence","event_kind","reviewed_git_sha","content_version","content_digest","recorded_at","actor_type","actor_id","actor_label"]
      & properties . at "content_version" ?~ Inline (uuidSchema & nullable ?~ True)
      & properties . at "content_digest" ?~ Inline (mempty & type_ ?~ OpenApiString & pattern ?~ "^[0-9a-f]{64}$" & nullable ?~ True)
      & properties . at "actor_type" ?~ Inline (mempty & type_ ?~ OpenApiString & nullable ?~ True)
      & properties . at "actor_id" ?~ Inline (mempty & type_ ?~ OpenApiString & nullable ?~ True)
      & properties . at "actor_label" ?~ Inline (mempty & type_ ?~ OpenApiString & nullable ?~ True))
instance ToSchema ObservationRevisionEvent where
  declareNamedSchema _ = do
    NamedSchema _ schema <- declareNamedSchema (Proxy @ObservationProvenance)
    pure $ NamedSchema (Just "ObservationRevisionEvent") (schema & required %~ ("observation_id":) & properties . at "observation_id" ?~ Inline uuidSchema)
instance ToSchema ReviewedObservationUpdate where
  declareNamedSchema _ = do
    NamedSchema name schema <- genericDeclareNamedSchema opts (Proxy @ReviewedObservationUpdate)
    pure $ NamedSchema name (schema & required .~ ["content","reviewed_git_sha"] & additionalProperties ?~ AdditionalPropertiesAllowed False)
instance ToSchema ReviewedObservationRequest where declareNamedSchema _ = declareNamedSchema (Proxy @ReviewedObservationUpdate)
instance ToSchema SimilarObservationQuery where
  declareNamedSchema _ = do
    NamedSchema name schema <- genericDeclareNamedSchema opts (Proxy @SimilarObservationQuery)
    pure $ NamedSchema name (schema & properties . at "embedding" ?~ Inline embeddingSchema)
instance ToSchema SimilarObservation where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema ObservationMatchQuery where
  declareNamedSchema _ = do
    NamedSchema name schema <- genericDeclareNamedSchema opts (Proxy @ObservationMatchQuery)
    pure $ NamedSchema name $ schema
      & description ?~ "Match concrete canonical repository-relative paths against stored file and glob subjects after repository read authorization for the requested active repository workspace. Paths are ORed; optional subject_kind, git_sha, and query filters compose with the match. The optional full-text query searches Observation content and all stored subject text. Input paths never accept globs or touch the repository filesystem. Each Observation appears once; canonical path_matches follow deduplicated caller path order and each group's matched_subjects follow stored subject order. Results rank by text relevance when query is set, then updated_at DESC and id DESC. Pagination defaults to limit 50 and offset 0; limit is 1..200 and offset is 0..100000. git_sha is an exact immutable lowercase 40-character hexadecimal SHA."
      & properties . at "paths" ?~ Inline pathsSchema
instance ToSchema ObservationPathMatch where
  declareNamedSchema _ = do
    NamedSchema name schema <- genericDeclareNamedSchema opts (Proxy @ObservationPathMatch)
    pure $ NamedSchema name $ schema
      & description ?~ "Canonical evidence for one deduplicated caller path, with matching stored subjects in their Observation ordinal order."
instance ToSchema ObservationMatch where
  declareNamedSchema _ = do
    NamedSchema name schema <- genericDeclareNamedSchema opts (Proxy @ObservationMatch)
    pure $ NamedSchema name $ schema
      & description ?~ "One matching Observation with canonical path-correlated evidence. path_matches is canonical. matched_paths and matched_subjects are deprecated compatibility projections that do not express their correlation."
      & properties . at "matched_paths" ?~ Inline (pathsSchema
          & deprecated ?~ True
          & description ?~ "Deprecated compatibility projection of the distinct paths in path_matches, in caller order.")
      & properties . at "matched_subjects" ?~ Inline (subjectsSchema
          & deprecated ?~ True
          & description ?~ "Deprecated compatibility projection of the union of matched stored subjects, in Observation ordinal order.")
instance ToSchema ObservationMatchRequest where declareNamedSchema _ = declareNamedSchema (Proxy @ObservationMatchQuery)
instance ToSchema ObservationCountRequest where declareNamedSchema _ = declareNamedSchema (Proxy @ObservationCountQuery)
instance ToSchema ObservationCountQuery where
  declareNamedSchema _ = genericDeclareNamedSchema opts (Proxy @ObservationCountQuery)

instance ToSchema ObservationDiscoveryFilter where
  declareNamedSchema _ = genericDeclareNamedSchema
    (opts { fieldLabelModifier = \name -> if name == "subjectsText" then "subject_query" else camelToSnake name }) (Proxy @ObservationDiscoveryFilter)
instance ToSchema ObservationDiscoveryView where
  declareNamedSchema _ = pure $ NamedSchema (Just "ObservationDiscoveryView") $
    mempty & type_ ?~ OpenApiString & enum_ ?~ map String ["list","tree","count"]
instance ToSchema ObservationDiscoveryRequest where
  declareNamedSchema _ = do
    NamedSchema name schema <- genericDeclareNamedSchema
      (opts { fieldLabelModifier = \field -> if field == "treePrefix" then "prefix" else camelToSnake field }) (Proxy @ObservationDiscoveryRequest)
    pure $ NamedSchema name (schema & required .~ ["workspace_id"]
      & properties . at "generation" ?~ Inline (mempty & type_ ?~ OpenApiInteger & minimum_ ?~ 1 & maximum_ ?~ 2147483647))
instance ToSchema ObservationCounts where
  declareNamedSchema _ = do
    NamedSchema name schema <- genericDeclareNamedSchema opts (Proxy @ObservationCounts)
    let countSchema = mempty & type_ ?~ OpenApiInteger & minimum_ ?~ 0 & maximum_ ?~ 9007199254740991
    pure $ NamedSchema name (schema & properties . at "total_count" ?~ Inline countSchema & properties . at "match_count" ?~ Inline countSchema)
instance ToSchema WorkspaceTimelineEvent where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema TimelineActor where declareNamedSchema = genericDeclareNamedSchema timelineActorOpts
instance ToSchema TimelineProjectContext where declareNamedSchema = genericDeclareNamedSchema timelineProjectContextOpts
instance ToSchema TimelineTaskContext where declareNamedSchema = genericDeclareNamedSchema timelineTaskContextOpts
instance ToSchema TimelineStatusTransition where declareNamedSchema = genericDeclareNamedSchema timelineStatusTransitionOpts
instance ToSchema TimelineNavigation where declareNamedSchema = genericDeclareNamedSchema timelineNavigationOpts
instance ToSchema TimelineBucketCounts where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema TimelineBucketEntityCounts where declareNamedSchema = genericDeclareNamedSchema timelineBucketEntityCountsOpts
instance ToSchema TimelineBucketSeriesCounts where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema TimelineBucketSeries where declareNamedSchema = genericDeclareNamedSchema timelineBucketSeriesOpts
instance ToSchema TimelineOpenCounts where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema WorkspaceTimelineBucket where
  declareNamedSchema _ = do
    NamedSchema name schema <- genericDeclareNamedSchema timelineBucketOpts (Proxy @WorkspaceTimelineBucket)
    pure $ NamedSchema name $ schema
      & properties . at "counts" ?~ deprecatedTimelineProjection "TimelineBucketEntityCounts"
      & properties . at "totals" ?~ deprecatedTimelineProjection "TimelineBucketCounts"
      & properties . at "observation_total" ?~ Inline (mempty & type_ ?~ OpenApiInteger & format ?~ "int64" & nullable ?~ True)
      & properties . at "observation_as_of" ?~ Inline (mempty & type_ ?~ OpenApiString & format ?~ "date-time" & nullable ?~ True)
      & properties . at "observation_status" ?~ Inline (mempty & type_ ?~ OpenApiString & enum_ ?~ ["available", "history_unavailable", "future"])
instance ToSchema WorkspaceTimelineBucketsResponse where declareNamedSchema = genericDeclareNamedSchema timelineBucketsResponseOpts

-- Both vector endpoints accept precisely the storage dimension.  Keep this
-- schema separate from the runtime validation so the generated contract cannot
-- silently drift from pgvector(1536).
embeddingSchema :: Schema
embeddingSchema = mempty
  & type_ ?~ OpenApiArray
  & items ?~ OpenApiItemsObject (Inline (mempty & type_ ?~ OpenApiNumber))
  & minItems ?~ fromIntegral observationEmbeddingDimensions
  & maxItems ?~ fromIntegral observationEmbeddingDimensions

embeddingEnvelopeSchema :: Schema
embeddingEnvelopeSchema = mempty
  & type_ ?~ OpenApiObject
  & required .~ ["embedding", "space_fingerprint"]
  & additionalProperties ?~ AdditionalPropertiesAllowed False
  & properties .~ InsOrdMap.fromList
    [ ("embedding", Inline embeddingSchema)
    , ("space_fingerprint", Ref (Reference "EmbeddingSpaceFingerprint"))
    ]

subjectsSchema :: Schema
subjectsSchema = mempty
  & type_ ?~ OpenApiArray
  & description ?~ "One to 256 canonical subjects; total subject text is limited to 262144 UTF-8 bytes."
  & items ?~ OpenApiItemsObject (Ref (Reference "#/components/schemas/ObservationSubject"))
  & minItems ?~ 1
  & maxItems ?~ fromIntegral maxObservationSubjects

pathsSchema :: Schema
pathsSchema = mempty
  & type_ ?~ OpenApiArray
  & description ?~ "One to 256 canonical concrete repository-relative paths; glob metacharacters are rejected. Total path text is limited to 262144 UTF-8 bytes."
  & items ?~ OpenApiItemsObject (Inline (mempty & type_ ?~ OpenApiString & maxLength ?~ fromIntegral maxObservationSubjectBytes))
  & minItems ?~ 1
  & maxItems ?~ fromIntegral maxObservationSubjects

canonicalCreateObservationSchema :: Schema
canonicalCreateObservationSchema = mempty
  & type_ ?~ OpenApiObject
  & description ?~ "Canonical create form. subjects is required and must be a nonempty subject set."
  & properties .~ requiredCreateProperties
  & properties . at "subjects" ?~ Inline subjectsSchema
  & required .~ ["workspace_id", "subjects", "git_sha", "content"]
  & additionalProperties ?~ AdditionalPropertiesAllowed False

legacyCreateObservationSchema :: Schema
legacyCreateObservationSchema = mempty
  & type_ ?~ OpenApiObject
  & description ?~ "Deprecated singleton compatibility form. subject_kind and subject are required together."
  & properties .~ requiredCreateProperties
  & properties . at "subject_kind" ?~ Inline deprecatedSubjectKindSchema
  & properties . at "subject" ?~ Inline deprecatedSubjectSchema
  & required .~ ["workspace_id", "subject_kind", "subject", "git_sha", "content"]
  & additionalProperties ?~ AdditionalPropertiesAllowed False

requiredCreateProperties = InsOrdMap.fromList
  [ ("workspace_id", Inline (mempty & type_ ?~ OpenApiString & format ?~ "uuid"))
  , ("git_sha", Inline gitShaSchema)
  , ("content", Inline (mempty & type_ ?~ OpenApiString & minLength ?~ 1 & maxLength ?~ fromIntegral maxObservationContentBytes))
  ]

withLegacySubjectProperties :: Schema -> Schema
withLegacySubjectProperties schema = schema
  & description ?~ "An immutable-provenance observation. subjects is canonical. The deprecated top-level subject_kind and subject mirror subjects[0] for legacy clients."
  & properties . at "subjects" ?~ Inline subjectsSchema
  & properties . at "subject_kind" ?~ Inline deprecatedSubjectKindSchema
  & properties . at "subject" ?~ Inline deprecatedSubjectSchema

gitShaSchema :: Schema
gitShaSchema = mempty
  & type_ ?~ OpenApiString
  & minLength ?~ 40
  & maxLength ?~ 40
  & pattern ?~ "^[0-9a-f]{40}$"
  & description ?~ "Immutable lowercase 40-character hexadecimal Git SHA."

deprecatedSubjectKindSchema :: Schema
deprecatedSubjectKindSchema = mempty
  & type_ ?~ OpenApiString
  & enum_ ?~ ["file", "glob"]
  & deprecated ?~ True
  & description ?~ "Deprecated compatibility projection of subjects[0]."

deprecatedSubjectSchema :: Schema
deprecatedSubjectSchema = mempty
  & type_ ?~ OpenApiString
  & maxLength ?~ fromIntegral maxObservationSubjectBytes
  & deprecated ?~ True
  & description ?~ "Deprecated compatibility projection of subjects[0]."
instance ToSchema Workspace where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema WorkspacePlanningCounts where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema CreateWorkspace where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema WorkspaceGroup where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema CreateWorkspaceGroup where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema WorkspaceGroupMemberInput where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema Project where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema CreateProject where declareNamedSchema = fmap dueFreePlanningSchema . genericDeclareNamedSchema opts
instance ToSchema ProjectSpecTask where declareNamedSchema = fmap dueFreePlanningSchema . genericDeclareNamedSchema opts
instance ToSchema CreateProjectSpec where
  declareNamedSchema _ = do
    task <- declareSchemaRef (Proxy @ProjectSpecTask)
    pure $ NamedSchema (Just "CreateProjectSpec") $ mempty
      & type_ ?~ OpenApiObject
      & description ?~ "Creates a project and all initial tasks atomically. Every task is top-level under the new project. Retired due scheduling keys are rejected on the project and initial tasks, including null."
      & properties . at "workspace_id" ?~ Inline uuidSchema
      & properties . at "name" ?~ Inline (mempty & type_ ?~ OpenApiString)
      & properties . at "description" ?~ Inline (mempty & type_ ?~ OpenApiString & nullable ?~ True)
      & properties . at "priority" ?~ Inline (mempty & type_ ?~ OpenApiInteger & minimum_ ?~ 1 & maximum_ ?~ 10 & nullable ?~ True)
      & properties . at "tasks" ?~ Inline (mempty & type_ ?~ OpenApiArray & items ?~ OpenApiItemsObject task & minItems ?~ 1 & maxItems ?~ 50)
      & required .~ ["workspace_id", "name", "tasks"]
instance ToSchema ProjectSpecResult where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema UpdateProject where declareNamedSchema = fmap dueFreePlanningSchema . genericDeclareNamedSchema opts
instance ToSchema CascadeResult where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema ProjectOverview where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema ProjectCardSummary where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema TaskCardSummary where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema TaskDependencyPage where
  declareNamedSchema _ = do
    item <- declareSchemaRef (Proxy @TaskDependencySummary)
    pure $ NamedSchema (Just "TaskDependencyPage") $ mempty
      & type_ ?~ OpenApiObject
      & description ?~ "A bounded dependency-summary page with authoritative task status. items are ordered by lower(name), id and contain at most 100 entries."
      & properties . at "items" ?~ Inline (mempty & type_ ?~ OpenApiArray & items ?~ OpenApiItemsObject item & maxItems ?~ fromIntegral maxNavigationPageSize)
      & properties . at "has_more" ?~ Inline (mempty & type_ ?~ OpenApiBoolean)
      & required .~ ["items", "has_more"]
      & additionalProperties ?~ AdditionalPropertiesAllowed False

uuidSchema :: Schema
uuidSchema = mempty & type_ ?~ OpenApiString & format ?~ "uuid"

observationContentConflictSchema :: Schema
observationContentConflictSchema = mempty
  & type_ ?~ OpenApiObject
  & properties . at "code" ?~ Inline (mempty & type_ ?~ OpenApiString & enum_ ?~ ["observation_content_conflict"])
  & properties . at "latest" ?~ Ref (Reference "Observation")
  & required .~ ["code", "latest"]

uuidArrayLike :: Referenced Schema -> Int -> Schema
uuidArrayLike item maximum = mempty
  & type_ ?~ OpenApiArray
  & items ?~ OpenApiItemsObject item
  & maxItems ?~ fromIntegral maximum

instance ToSchema NavigationParent where
  declareNamedSchema _ = pure $ NamedSchema (Just "NavigationParent") $ mempty
    & type_ ?~ OpenApiObject
    & description ?~ "Typed branch selector. workspace_root omits parent_id; project and task require a UUID parent_id."
    & properties . at "kind" ?~ Inline (mempty & type_ ?~ OpenApiString & enum_ ?~ ["workspace_root", "project", "task"])
    & properties . at "parent_id" ?~ Inline (mempty & type_ ?~ OpenApiString & format ?~ "uuid")
    & required .~ ["kind"]
instance ToSchema NavigationFilter where
  declareNamedSchema _ = pure $ NamedSchema (Just "NavigationFilter") $ mempty
    & type_ ?~ OpenApiObject
    & description ?~ "Server-owned tree filter; matching descendants retain their ancestors. priority_mode is any, exact, above, or below; priority_value is required except for any."
    & properties . at "show_empty_projects" ?~ Inline (mempty & type_ ?~ OpenApiBoolean & description ?~ "Defaults true; false requires matching tasks anywhere in the project subtree.")
    & properties . at "has_active_session" ?~ Inline (mempty & type_ ?~ OpenApiBoolean & default_ ?~ Bool False & description ?~ "Require active sessions on the matched task/project subtree; matching descendants retain ancestor paths. Independent of heartbeat freshness.")
    & properties . at "show_only" ?~ Inline (mempty & type_ ?~ OpenApiString & enum_ ?~ ["projects", "tasks"])
    & properties . at "project_statuses" ?~ Inline (mempty & type_ ?~ OpenApiArray & items ?~ OpenApiItemsObject (Inline (mempty & type_ ?~ OpenApiString & enum_ ?~ ["active", "paused", "completed", "archived"])))
    & properties . at "task_statuses" ?~ Inline (mempty & type_ ?~ OpenApiArray & items ?~ OpenApiItemsObject (Inline (mempty & type_ ?~ OpenApiString & enum_ ?~ ["todo", "in_progress", "blocked", "done", "cancelled"])))
    & properties . at "priority_mode" ?~ Inline (mempty & type_ ?~ OpenApiString & enum_ ?~ ["any", "exact", "above", "below"])
    & properties . at "priority_value" ?~ Inline (mempty & type_ ?~ OpenApiInteger & minimum_ ?~ 1 & maximum_ ?~ 10)
    & properties . at "query" ?~ Inline (mempty & type_ ?~ OpenApiString & minLength ?~ 1)
instance ToSchema NavigationBranchRequest where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema NavigationBatchItem where
  declareNamedSchema _ = do
    NamedSchema name schema <- genericDeclareNamedSchema opts (Proxy @NavigationBatchItem)
    pure $ NamedSchema name $ schema
      & properties . at "request_id" ?~ Inline (mempty & type_ ?~ OpenApiString & minLength ?~ 1 & maxLength ?~ 256)
      & properties . at "parent" ?~ Inline navigationBatchParentSchema
      & properties . at "project_limit" ?~ Inline navigationLimitSchema
      & properties . at "task_limit" ?~ Inline navigationLimitSchema
      & properties . at "project_offset" ?~ Inline navigationOffsetSchema
      & properties . at "task_offset" ?~ Inline navigationOffsetSchema
      & properties . at "include_projects" ?~ Inline (mempty & type_ ?~ OpenApiBoolean & default_ ?~ Bool True)
      & properties . at "include_tasks" ?~ Inline (mempty & type_ ?~ OpenApiBoolean & default_ ?~ Bool True)
      & description ?~ "One independent owner page. Include flags default true; false returns an empty nonauthoritative companion page. Request IDs and owners must each be unique within the batch."
instance ToSchema NavigationBatchRequest where
  declareNamedSchema _ = do
    owner <- declareSchemaRef (Proxy @NavigationBatchItem)
    filters <- declareSchemaRef (Proxy @NavigationFilter)
    pure $ NamedSchema (Just "NavigationBatchRequest") $ mempty
      & type_ ?~ OpenApiObject
      & properties . at "filters" ?~ filters
      & properties . at "requests" ?~ Inline (mempty & type_ ?~ OpenApiArray & items ?~ OpenApiItemsObject owner & minItems ?~ 1 & maxItems ?~ 4)
      & required .~ ["requests"]
      & description ?~ "A common filter context and 1-4 distinct nonroot owners. Absent filters use GET defaults; absent/empty status arrays are unrestricted. Every owner is validated before any selection. Workspace authorization rejects the whole request; owner failures are independent outcomes."
instance ToSchema NavigationBatchOutcome where
  declareNamedSchema _ = do
    response <- declareSchemaRef (Proxy @NavigationBranchResponse)
    let common = mempty & type_ ?~ OpenApiObject
          & properties . at "request_id" ?~ Inline (mempty & type_ ?~ OpenApiString & minLength ?~ 1 & maxLength ?~ 256)
          & properties . at "parent" ?~ Inline navigationBatchParentSchema
          & additionalProperties ?~ AdditionalPropertiesAllowed False
        success = common & properties . at "status" ?~ Inline (mempty & type_ ?~ OpenApiInteger & minimum_ ?~ 200 & maximum_ ?~ 200)
          & properties . at "response" ?~ response & required .~ ["request_id", "parent", "status", "response"]
        failure = common & properties . at "status" ?~ Inline (mempty & type_ ?~ OpenApiInteger & minimum_ ?~ 400 & maximum_ ?~ 599)
          & properties . at "error" ?~ Inline (mempty & type_ ?~ OpenApiString & minLength ?~ 1 & maxLength ?~ 256)
          & required .~ ["request_id", "parent", "status", "error"]
    pure $ NamedSchema (Just "NavigationBatchOutcome") $ mempty & oneOf ?~ [Inline success, Inline failure]
instance ToSchema NavigationBatchResponse where
  declareNamedSchema _ = do
    outcome <- declareSchemaRef (Proxy @NavigationBatchOutcome)
    pure $ NamedSchema (Just "NavigationBatchResponse") $ mempty & type_ ?~ OpenApiObject
      & properties . at "workspace_id" ?~ Inline uuidSchema
      & properties . at "outcomes" ?~ Inline (mempty & type_ ?~ OpenApiArray & items ?~ OpenApiItemsObject outcome & minItems ?~ 1 & maxItems ?~ 4)
      & required .~ ["workspace_id", "outcomes"]
      & description ?~ "Exactly one ordered identified outcome per requested owner; successful siblings survive owner-local failures."

navigationBatchParentSchema :: Schema
navigationBatchParentSchema = mempty & type_ ?~ OpenApiObject
  & properties . at "kind" ?~ Inline (mempty & type_ ?~ OpenApiString & enum_ ?~ ["project", "task"])
  & properties . at "parent_id" ?~ Inline uuidSchema
  & required .~ ["kind", "parent_id"]

navigationLimitSchema :: Schema
navigationLimitSchema = mempty & type_ ?~ OpenApiInteger & minimum_ ?~ 1 & maximum_ ?~ fromIntegral maxNavigationPageSize

navigationOffsetSchema :: Schema
navigationOffsetSchema = mempty & type_ ?~ OpenApiInteger & minimum_ ?~ 0 & maximum_ ?~ fromIntegral maxNavigationOffset

instance ToSchema a => ToSchema (NavigationPage a) where
  declareNamedSchema _ = do
    item <- declareSchemaRef (Proxy @a)
    NamedSchema name schema <- genericDeclareNamedSchema opts (Proxy @(NavigationPage a))
    pure $ NamedSchema name $
      schema
        & properties . at "items" ?~ Inline (mempty & type_ ?~ OpenApiArray & items ?~ OpenApiItemsObject item & maxItems ?~ fromIntegral maxNavigationPageSize)
instance ToSchema NavigationBranchResponse where
  declareNamedSchema _ = do
    projectItem <- declareSchemaRef (Proxy @ProjectCardSummary)
    taskItem <- declareSchemaRef (Proxy @TaskCardSummary)
    parent <- declareSchemaRef (Proxy @NavigationParent)
    pure $ NamedSchema (Just "NavigationBranchResponse") $ mempty
      & type_ ?~ OpenApiObject
      & properties . at "workspace_id" ?~ Inline uuidSchema
      & properties . at "parent" ?~ parent
      -- Do not reference the polymorphic NavigationPage component here: OpenAPI
      -- component names erase its type parameter.  These inline pages retain the
      -- distinct ProjectCardSummary/TaskCardSummary item contracts.
      & properties . at "projects" ?~ Inline (navigationPageSchema projectItem)
      & properties . at "tasks" ?~ Inline (navigationPageSchema taskItem)
      & required .~ ["workspace_id", "parent", "projects", "tasks"]
      & additionalProperties ?~ AdditionalPropertiesAllowed False

navigationPageSchema :: Referenced Schema -> Schema
navigationPageSchema item = mempty
  & type_ ?~ OpenApiObject
  & properties . at "items" ?~ Inline (mempty & type_ ?~ OpenApiArray & items ?~ OpenApiItemsObject item & maxItems ?~ fromIntegral maxNavigationPageSize)
  & properties . at "has_more" ?~ Inline (mempty & type_ ?~ OpenApiBoolean)
  & required .~ ["items", "has_more"]
  & additionalProperties ?~ AdditionalPropertiesAllowed False
instance ToSchema NavigationSummary where
  declareNamedSchema _ = do
    projectSummary <- declareSchemaRef (Proxy @ProjectCardSummary)
    taskSummary <- declareSchemaRef (Proxy @TaskCardSummary)
    pure $ NamedSchema (Just "NavigationSummary") $ mempty
      & description ?~ "Tagged card summary. entity_type selects whether summary is a ProjectCardSummary or TaskCardSummary."
      & oneOf ?~ [Inline (navigationProjectSummarySchema projectSummary), Inline (navigationTaskSummarySchema taskSummary)]
instance ToSchema NavigationFocusResponse where
  declareNamedSchema _ = do
    summary <- declareSchemaRef (Proxy @NavigationSummary)
    pure $ NamedSchema (Just "NavigationFocusResponse") $ mempty
      & type_ ?~ OpenApiObject
      & description ?~ "A bounded direct-link response. ancestors are root-to-parent order and never exceed 64; next_ancestor_offset is present only when the chain was truncated."
      & properties . at "workspace_id" ?~ Inline uuidSchema
      & properties . at "target" ?~ summary
      & properties . at "ancestors" ?~ Inline (uuidArrayLike summary 64)
      & properties . at "ancestors_truncated" ?~ Inline (mempty & type_ ?~ OpenApiBoolean)
      & properties . at "next_ancestor_offset" ?~ Inline (mempty & type_ ?~ OpenApiInteger & minimum_ ?~ 0 & maximum_ ?~ fromIntegral maxNavigationOffset)
      & required .~ ["workspace_id", "target", "ancestors", "ancestors_truncated"]
instance ToSchema NavigationSummariesRequest where
  declareNamedSchema _ = pure $ NamedSchema (Just "NavigationSummariesRequest") $ mempty
    & type_ ?~ OpenApiObject
    & description ?~ "Bounded targeted revalidation. project_ids and task_ids are each ordered UUID lists; their combined total is at most 100 and duplicates are rejected."
    & properties . at "project_ids" ?~ Inline (uuidArrayLike (Inline uuidSchema) maxNavigationBatchIds)
    & properties . at "task_ids" ?~ Inline (uuidArrayLike (Inline uuidSchema) maxNavigationBatchIds)
    & required .~ ["project_ids", "task_ids"]
instance ToSchema NavigationSummariesResponse where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema ProjectReadinessRollup where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema ProjectPauseStatusChange where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema ProjectPauseEffects where
  declareNamedSchema _ = do
    NamedSchema name schema <- genericDeclareNamedSchema opts (Proxy @ProjectPauseEffects)
    project <- declareSchemaRef (Proxy @ProjectPauseStatusChange)
    task <- declareSchemaRef (Proxy @TaskDependencyStatusChange)
    pure $ NamedSchema name $ schema
      & description ?~ "Cascade counts and deterministic samples of at most twenty projects and twenty tasks; flags indicate remaining independent or ancestor pause."
      & properties . at "projects" ?~ Inline (uuidArrayLike project 20)
      & properties . at "tasks" ?~ Inline (uuidArrayLike task 20)
instance ToSchema ProjectMutationResult where
  declareNamedSchema _ = do
    NamedSchema _ schema <- genericDeclareNamedSchema opts (Proxy @Project)
    effects <- declareSchemaRef (Proxy @ProjectPauseEffects)
    pure $ NamedSchema (Just "ProjectMutationResult") $ schema
      & properties . at "pause_effects" ?~ effects
      & required %~ (<> ["pause_effects"])
instance ToSchema NextTaskCandidate where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema Task where declareNamedSchema = genericDeclareNamedSchema opts
dueFreePlanningSchema :: NamedSchema -> NamedSchema
dueFreePlanningSchema (NamedSchema name schema) = NamedSchema name $ schema
  & description ?~ "Modeled due scheduling fields (due_at, dueAt, due, due_date, dueDate) are rejected, including null. Other unknown-key behavior is unchanged."

instance ToSchema CreateTask where declareNamedSchema = fmap dueFreePlanningSchema . genericDeclareNamedSchema opts
instance ToSchema UpdateTask where declareNamedSchema = fmap dueFreePlanningSchema . genericDeclareNamedSchema opts
instance ToSchema TaskMutationResult where
  declareNamedSchema _ = do
    NamedSchema _ schema <- genericDeclareNamedSchema opts (Proxy @Task)
    effects <- declareSchemaRef (Proxy @[TaskDependencyStatusChange])
    pure $ NamedSchema (Just "TaskMutationResult") $ schema
      & properties . at "dependency_effects" ?~ effects
      & required %~ (<> ["dependency_effects"])
instance ToSchema BatchMoveTasksRequest where
  declareNamedSchema _ = pure $ NamedSchema (Just "BatchMoveTasksRequest") $ mempty
    & type_ ?~ OpenApiObject
    & description ?~ "Atomic task-project relocation. Duplicate task IDs are accepted but affect each task once; active descendants are moved with requested roots. Retired due scheduling keys are rejected, including null."
    & properties . at "task_ids" ?~ Inline (mempty
        & type_ ?~ OpenApiArray
        & items ?~ OpenApiItemsObject (Inline uuidSchema)
        & minItems ?~ 1
        & maxItems ?~ 100)
    & properties . at "project_id" ?~ Inline (uuidSchema & nullable ?~ True)
    & required .~ ["task_ids"]
instance ToSchema BatchResult where
  declareNamedSchema _ = pure $ NamedSchema (Just "BatchResult") $ mempty
    & type_ ?~ OpenApiObject
    & properties . at "affected" ?~ Inline (mempty & type_ ?~ OpenApiInteger & minimum_ ?~ 0)
    & required .~ ["affected"]
    & additionalProperties ?~ AdditionalPropertiesAllowed False
instance ToSchema TaskDependencyStatusChange where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema LinkDependency where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema DependencyMutationResult where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema TaskOverview where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema TaskReadinessRollup where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema TaskDependencySummary where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema UnifiedSearchQuery where
  declareNamedSchema _ = do
    NamedSchema searchName searchSchema <- genericDeclareNamedSchema opts (Proxy @UnifiedSearchQuery)
    pure $ NamedSchema searchName $ searchSchema
      & properties . at "entity_types" . _Just %~ \case
          Inline entitySchema -> Inline (entitySchema & minItems ?~ 1)
          reference -> reference
      & properties . at "limit" ?~ Inline (mempty & type_ ?~ OpenApiInteger & nullable ?~ True & minimum_ ?~ 1 & maximum_ ?~ 200)
      & properties . at "offset" ?~ Inline (mempty & type_ ?~ OpenApiInteger & nullable ?~ True & minimum_ ?~ 0 & maximum_ ?~ fromIntegral maxUnifiedSearchOffset)
instance ToSchema UnifiedSearchResults where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema AuditLogEntry where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema RevertResult where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema WebSocketTicketRequest where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema WebSocketTicketResponse where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema ChangeStreamScopeRequest where
  declareNamedSchema _ = pure $ NamedSchema (Just "ChangeStreamScopeRequest") changeStreamScopeSchema
instance ToSchema SnapshotProfile where
  declareNamedSchema _ = pure $ NamedSchema (Just "SnapshotProfile") (mempty & type_ ?~ OpenApiString & enum_ ?~ ["full_v1", "workspace_shell_v1"] & description ?~ "full_v1 is the backward-compatible default. workspace_shell_v1 is an explicit workspace-only bounded projection and is immutable for a snapshot session.")
instance ToSchema ChangeStreamResyncRequest where
  declareNamedSchema _ = pure $ NamedSchema (Just "ChangeStreamResyncRequest") changeStreamResyncSchema
instance ToSchema ChangeStreamSnapshotItem where declareNamedSchema = genericDeclareNamedSchema changeStreamItemOpts
instance ToSchema ChangeStreamResyncResponse where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema CanonicalWebSocketTicketRequest where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema SessionContext where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema SessionPrincipal where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema SessionActor where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema SessionGlobalPermissions where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema SessionWorkspaceContext where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema a => ToSchema (PaginatedResult a) where declareNamedSchema = genericDeclareNamedSchema opts

lifecycleConflictSchema :: Schema
lifecycleConflictSchema = mempty
  & type_ ?~ OpenApiObject
  & description ?~ "Stable lifecycle conflict response. detail is structured JSON when the invariant supplies it; hint is present when a safe remediation is available."
  & properties . at "error" ?~ Inline (mempty & type_ ?~ OpenApiString & enum_ ?~ ["lifecycle_conflict"])
  & properties . at "code" ?~ Inline (mempty & type_ ?~ OpenApiString)
  & properties . at "message" ?~ Inline (mempty & type_ ?~ OpenApiString)
  & properties . at "detail" ?~ Inline mempty
  & properties . at "hint" ?~ Inline (mempty & type_ ?~ OpenApiString)
  & required .~ ["error", "code", "message"]
  & additionalProperties ?~ AdditionalPropertiesAllowed False

timelineActorOpts :: SchemaOptions
timelineActorOpts = opts { fieldLabelModifier = \case
  "actorType" -> "type"; "actorId" -> "id"; "actorLabel" -> "label"; other -> camelToSnake other }

changeStreamItemOpts :: SchemaOptions
changeStreamItemOpts = opts { fieldLabelModifier = \case
  "data_" -> "data"
  other -> camelToSnake other }

-- The resync request has two disjoint wire states.  Keeping the distinction
-- explicit in OpenAPI prevents generated clients from accidentally combining
-- a continuation bearer with a new-start idempotency key.
changeStreamScopeSchema :: Schema
changeStreamScopeSchema = mempty
  & description ?~ "Exactly one scope form: global has no workspace_id; workspace requires workspace_id."
  & oneOf ?~ [Inline globalScopeSchema, Inline workspaceScopeSchema]

globalScopeSchema :: Schema
globalScopeSchema = mempty
  & type_ ?~ OpenApiObject
  & properties .~ InsOrdMap.fromList [("scope", Inline (mempty & type_ ?~ OpenApiString & enum_ ?~ ["global"]))]
  & required .~ ["scope"]
  & additionalProperties ?~ AdditionalPropertiesAllowed False

workspaceScopeSchema :: Schema
workspaceScopeSchema = mempty
  & type_ ?~ OpenApiObject
  & properties .~ InsOrdMap.fromList
      [ ("scope", Inline (mempty & type_ ?~ OpenApiString & enum_ ?~ ["workspace"]))
      , ("workspace_id", Inline (mempty & type_ ?~ OpenApiString & format ?~ "uuid")) ]
  & required .~ ["scope", "workspace_id"]
  & additionalProperties ?~ AdditionalPropertiesAllowed False

-- The JSON representation is tagged at the outer object, rather than merely
-- documenting an unconstrained `summary` field.  Generated clients must be
-- able to tell which card shape accompanies each entity_type.
navigationProjectSummarySchema :: Referenced Schema -> Schema
navigationProjectSummarySchema projectSummary = mempty
  & type_ ?~ OpenApiObject
  & properties .~ InsOrdMap.fromList
      [ ("entity_type", Inline (mempty & type_ ?~ OpenApiString & enum_ ?~ ["project"]))
      , ("summary", projectSummary) ]
  & required .~ ["entity_type", "summary"]
  & additionalProperties ?~ AdditionalPropertiesAllowed False

navigationTaskSummarySchema :: Referenced Schema -> Schema
navigationTaskSummarySchema taskSummary = mempty
  & type_ ?~ OpenApiObject
  & properties .~ InsOrdMap.fromList
      [ ("entity_type", Inline (mempty & type_ ?~ OpenApiString & enum_ ?~ ["task"]))
      , ("summary", taskSummary) ]
  & required .~ ["entity_type", "summary"]
  & additionalProperties ?~ AdditionalPropertiesAllowed False

changeStreamResyncSchema :: Schema
changeStreamResyncSchema = mempty
  & description ?~ "Exactly one resync form: a start uses a client-generated, high-entropy start_idempotency_key of at least 32 characters; a continuation uses page_token. Page size is immutable after the start."
  & oneOf ?~ [Inline resyncStartSchema, Inline resyncContinuationSchema]

changeStreamSharedProperties = InsOrdMap.fromList
  [ ("scope", Inline changeStreamScopeSchema)
  , ("page_size", Inline (mempty & type_ ?~ OpenApiInteger & minimum_ ?~ 1 & maximum_ ?~ 1000)) ]

resyncStartSchema :: Schema
resyncStartSchema = mempty
  & type_ ?~ OpenApiObject
  & properties .~ InsOrdMap.insert "start_idempotency_key" (Inline (mempty & type_ ?~ OpenApiString & minLength ?~ 32 & maxLength ?~ 512)) (InsOrdMap.insert "snapshot_profile" (Ref (Reference "#/components/schemas/SnapshotProfile")) changeStreamSharedProperties)
  & required .~ ["scope", "start_idempotency_key"]
  & additionalProperties ?~ AdditionalPropertiesAllowed False

resyncContinuationSchema :: Schema
resyncContinuationSchema = mempty
  & type_ ?~ OpenApiObject
  & properties .~ InsOrdMap.insert "page_token" (Inline (mempty & type_ ?~ OpenApiString & minLength ?~ 1)) changeStreamSharedProperties
  & required .~ ["scope", "page_token"]
  & additionalProperties ?~ AdditionalPropertiesAllowed False

timelineProjectContextOpts :: SchemaOptions
timelineProjectContextOpts = opts { fieldLabelModifier = \case
  "projectContextId" -> "id"; "projectContextName" -> "name"; other -> camelToSnake other }

timelineTaskContextOpts :: SchemaOptions
timelineTaskContextOpts = opts { fieldLabelModifier = \case
  "taskContextId" -> "id"; "taskContextTitle" -> "title"; other -> camelToSnake other }

timelineStatusTransitionOpts :: SchemaOptions
timelineStatusTransitionOpts = opts { fieldLabelModifier = \case
  "transitionFrom" -> "from"; "transitionTo" -> "to"; other -> camelToSnake other }

timelineNavigationOpts :: SchemaOptions
timelineNavigationOpts = opts { fieldLabelModifier = \case
  "navigationEntityType" -> "entity_type"; "navigationEntityId" -> "entity_id"; other -> camelToSnake other }

timelineBucketEntityCountsOpts :: SchemaOptions
timelineBucketEntityCountsOpts = opts { fieldLabelModifier = \case
  "projectCounts" -> "project"; "subprojectCounts" -> "subproject"; "taskCounts" -> "task"; "subtaskCounts" -> "subtask"; other -> camelToSnake other }

timelineBucketOpts :: SchemaOptions
timelineBucketOpts = opts { fieldLabelModifier = \case
  "timelineBucketStart" -> "bucket_start"; "timelineBucketEnd" -> "bucket_end"; "timelineBucketLabel" -> "label"; "timelineBucketCounts" -> "counts"; "timelineBucketTotals" -> "totals"; "timelineBucketSeries" -> "series"; "timelineBucketSeriesTotals" -> "series_totals"; "timelineBucketOpenCounts" -> "open_counts"; "timelineBucketOpenAsOf" -> "open_as_of"; "timelineBucketOpenStatus" -> "open_status"; "timelineBucketObservationTotal" -> "observation_total"; "timelineBucketObservationAsOf" -> "observation_as_of"; "timelineBucketObservationStatus" -> "observation_status"; other -> camelToSnake other }

timelineBucketSeriesOpts :: SchemaOptions
timelineBucketSeriesOpts = opts { fieldLabelModifier = \case
  "seriesProject" -> "project"; "seriesTask" -> "task"; "seriesSubtask" -> "subtask"; "seriesObservation" -> "observation"; other -> camelToSnake other }

deprecatedTimelineProjection :: Text -> Referenced Schema
deprecatedTimelineProjection schemaName = Inline $ mempty
  & deprecated ?~ True
  & description ?~ "Deprecated API-v1 histogram projection. Use series and series_totals for canonical lifecycle values."
  & allOf ?~ [Ref (Reference ("#/components/schemas/" <> schemaName))]

timelineBucketsResponseOpts :: SchemaOptions
timelineBucketsResponseOpts = opts { fieldLabelModifier = \case
  "timelineBucketsWorkspaceId" -> "workspace_id"; "timelineBucketsSince" -> "since"; "timelineBucketsUntil" -> "until"; "timelineBucketsBucket" -> "bucket"; "timelineBucketsBuckets" -> "buckets"; "timelineBucketsNextSince" -> "next_since"; other -> camelToSnake other }


sessionArchiveManifestSchema :: Schema
sessionArchiveManifestSchema = mempty
  & type_ ?~ OpenApiObject
  & required .~ ["workspace_id","session_id","state","generation_id","codec","codec_implementation","raw_bytes","raw_records","raw_sha256","compressed_bytes","compressed_sha256","content_path","progress","retry_after_ms","error","message","retryable"]
  & properties .~ InsOrdMap.fromList
    ([ (key,Inline (mempty & type_ ?~ OpenApiString & format ?~ "uuid")) | key<-["workspace_id","session_id"] ]
     <> [("state",Inline (mempty & type_ ?~ OpenApiString & enum_ ?~ ["preparing","retrying","ready","failed"]))]
     <> [(key,Inline (mempty & type_ ?~ OpenApiString & nullable ?~ True)) | key<-["generation_id","codec","codec_implementation","raw_sha256","compressed_sha256","content_path","error","message"]]
     <> [(key,Inline (decimalSchema & nullable ?~ (key=="compressed_bytes"))) | key<-["raw_bytes","raw_records","compressed_bytes"]]
     <> [("retryable",Inline (mempty & type_ ?~ OpenApiBoolean))
        ,("retry_after_ms",Inline (mempty & type_ ?~ OpenApiInteger & nullable ?~ True & minimum_ ?~ 1000 & maximum_ ?~ 60000))
        ,("progress",Inline (mempty & type_ ?~ OpenApiObject & required .~ ["phase","raw_bytes","compressed_bytes"]
            & properties .~ InsOrdMap.fromList
              [("phase",Inline (mempty & type_ ?~ OpenApiString & enum_ ?~ ["queued","compressing","verifying","ready"]))
              ,("raw_bytes",Inline decimalSchema),("compressed_bytes",Inline decimalSchema)]))])
  where decimalSchema=mempty & type_ ?~ OpenApiString & pattern ?~ "^(0|[1-9][0-9]{0,18})$"

archivePathParameters :: [Text] -> [Referenced Param]
archivePathParameters = map (\key -> Inline (mempty & name .~ key & in_ .~ ParamPath & required ?~ True
  & schema ?~ Inline (mempty & type_ ?~ OpenApiString & format ?~ "uuid")))

sessionArchiveManifestPath :: PathItem
sessionArchiveManifestPath = mempty
  & parameters .~ archivePathParameters ["workspaceId","sessionId"]
  & get ?~ (mempty & tags .~ InsOrdSet.singleton "Agent Sessions"
      & summary ?~ "Check complete sealed-session archive readiness"
      & description ?~ "Current workspace read permission and session identity are checked before metadata. Idempotently schedules a terminated session; never changes its receipt or serves staged generations. All counts use decimal strings. Preparing/retrying returns bounded current-attempt progress and Retry-After; failed requires operator repair. Cache-Control:no-store."
      & responses .~ Responses Nothing (InsOrdMap.fromList
          ([(code,Inline (mempty & description .~ message & content .~ InsOrdMap.singleton "application/json"
              (mempty & schema ?~ (if code == 503 then Inline (mempty & oneOf ?~
                [Ref (Reference "#/components/schemas/SessionArchiveManifest")
                ,Inline (mempty & type_ ?~ OpenApiObject & required .~ ["error","message"]
                    & properties .~ InsOrdMap.fromList
                      [("error",Inline (mempty & type_ ?~ OpenApiString & maxLength ?~ 64 & enum_ ?~ ["archive_unavailable"]))
                      ,("message",Inline (mempty & type_ ?~ OpenApiString & maxLength ?~ 128))])])
                else Ref (Reference "#/components/schemas/SessionArchiveManifest")))))
            | (code,message)<-[(200,"Complete immutable ready generation."),(202,"Preparing or retrying; Retry-After supplied."),(503,"Terminal failed manifest (retryable:false), or transient archive_unavailable error.")]]
          <> [(401,Inline (mempty & description .~ "Authentication required."))
             ,(403,Inline (mempty & description .~ "Workspace read permission required."))
             ,(404,Inline (mempty & description .~ "Active workspace or same-workspace session not found."))
             ,(409,Inline (mempty & description .~ "session_state_conflict: session is active."))])))

sessionArchiveContentPath :: PathItem
sessionArchiveContentPath = mempty
  & parameters .~ (archivePathParameters ["workspaceId","sessionId","generationId"] <>
      [Inline (mempty & name .~ "If-None-Match" & in_ .~ ParamHeader & schema ?~ Inline (mempty & type_ ?~ OpenApiString))])
  & get ?~ (mempty & tags .~ InsOrdSet.singleton "Agent Sessions"
      & summary ?~ "Download the complete published gzip generation"
      & description ?~ "Pins the exact published generation. Authorizes before all metadata and conditional responses, and revalidates credentials and workspace read access between bounded part fetches and writes. A bounded whole-object preflight checks stored integrity before 200/304; transfer reads it again with no database lease spanning network writes. Content-Length is compressed bytes. X-HMem-Archive-Generation/SHA256/Raw-Bytes/Raw-SHA256 bind the response to the manifest. ETag binds generation, digest and a secret-safe authentication-context fingerprint; private,no-cache,no-transform and Vary:Authorization,Cookie require revalidation. Never sets Content-Encoding:gzip. A post-header failure or cancellation aborts transfer; clients must verify the complete count/digest. Fifteen-minute preflight and transfer deadlines."
      & responses .~ Responses Nothing (InsOrdMap.fromList
          [(200,Inline (mempty & description .~ "Complete stored single-member gzip object."
              & content .~ InsOrdMap.singleton "application/gzip" (mempty & schema ?~ Inline (mempty & type_ ?~ OpenApiString & format ?~ "binary"))))
          ,(304,Inline (mempty & description .~ "Current authorization and complete object verified; matching generation/authentication-context ETag."))
          ,(401,Inline (mempty & description .~ "Authentication required or credential revoked/expired."))
          ,(403,Inline (mempty & description .~ "Workspace read permission required."))
          ,(404,Inline (mempty & description .~ "Unknown or unbound published generation, workspace or session."))
          ,(503,Inline (mempty & description .~ "archive_unavailable: complete object cannot be read; Retry-After:1."))]))

taskEvidenceArchivePath :: PathItem
taskEvidenceArchivePath = mempty
  & parameters .~ map (\key -> Inline (mempty & name .~ key & in_ .~ ParamPath & required ?~ True & schema ?~ Inline (mempty & type_ ?~ OpenApiString & format ?~ "uuid"))) ["workspaceId","taskId"]
  & get ?~ (mempty
      & tags .~ InsOrdSet.singleton "Agent Sessions"
      & summary ?~ "Download this task’s saved sessions as a compressed ZIP"
      & description ?~ "Requires read access to an active workspace and a visible same-workspace task before archive work. Includes only associated terminated sessions across all server keyset pages. Streams original JSONL bytes in ZIP64 entries using actual raw DEFLATE (method 8), with receipt/SHA-256/CRC verification and safe UUID filenames. Content-Type is application/zip with an attachment filename and Cache-Control:no-store. Two concurrent exports per application; overload returns 503 with Retry-After:1. A 15-minute deadline, disconnect, authority withdrawal or verification failure aborts the transfer without a successful ZIP footer. This is live keyset enumeration, not a historical association snapshot."
      & responses .~ Responses Nothing (InsOrdMap.fromList
          [(200,Inline (mempty & description .~ "Binary compressed ZIP attachment." & content .~ InsOrdMap.singleton "application/zip" (mempty & schema ?~ Inline (mempty & type_ ?~ OpenApiString & format ?~ "binary"))))
          ,(401,Inline (mempty & description .~ "Authentication required."))
          ,(403,Inline (mempty & description .~ "Workspace read permission required."))
          ,(404,Inline (mempty & description .~ "Active workspace or visible same-workspace task not found."))
          ,(503,Inline (mempty & description .~ "Both export admissions are occupied; retry after one second."))]))
