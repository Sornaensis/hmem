{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | OpenAPI definitions are kept beside the Servant contract so removed
-- endpoints cannot accidentally survive in generated documentation.
module HMem.Server.OpenAPI (openApiSpec) where

import Control.Lens ((&), (.~), (%~), (?~), at, _Just)
import Data.Aeson (Value)
import Data.HashMap.Strict.InsOrd qualified as InsOrdMap
import Data.HashSet.InsOrd qualified as InsOrdSet
import Data.OpenApi
import Data.Proxy (Proxy(..))
import Servant.OpenApi (toOpenApi)

import HMem.Server.API (HMemAPI, CreateObservationRequest, ObservationMatchRequest)
import HMem.Types

openApiSpec :: OpenApi
openApiSpec = toOpenApi (Proxy @HMemAPI)
  & info . title .~ "hmem API"
  & info . version .~ "0.2.0.0"
  & info . description ?~ "Repository-scoped observation API with immutable provenance."
  & tags .~ InsOrdSet.fromList
      [ Tag "Observations" (Just "Repository-scoped, provenance-bound observations.") Nothing
      , Tag "Workspace Groups" (Just "Global-superadmin workspace group management.") Nothing
      , Tag "Timeline" (Just "Workspace lifecycle timeline and histogram.") Nothing
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
  & paths . at "/api/v1/observations/match" . _Just . post %~ fmap tagObservation
  & paths . at "/api/v1/observations/similar" . _Just . post %~ fmap tagObservation
  & paths . at "/api/v1/observations/{observationId}" . _Just . get %~ fmap tagObservation
  & paths . at "/api/v1/observations/{observationId}" . _Just . put %~ fmap tagObservation
  & paths . at "/api/v1/observations/{observationId}" . _Just . delete %~ fmap tagObservation
  & paths . at "/api/v1/observations/{observationId}/embedding" . _Just . put %~ fmap tagObservation
  & paths . at "/api/v1/workspaces/{workspaceId}/timeline" . _Just . get %~ fmap tagTimeline
  & paths . at "/api/v1/workspaces/{workspaceId}/timeline/buckets" . _Just . get %~ fmap tagTimeline
  where
    tagObservation operation = operation & tags .~ InsOrdSet.singleton "Observations"
    tagWorkspaceGroups operation = operation & tags .~ InsOrdSet.singleton "Workspace Groups"
    tagTimeline operation = operation & tags .~ InsOrdSet.singleton "Timeline"

opts :: SchemaOptions
opts = defaultSchemaOptions { fieldLabelModifier = camelToSnake }

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
    pure $ NamedSchema name (withLegacySubjectProperties schema)
instance ToParamSchema EntitySearchType where toParamSchema _ = mempty & type_ ?~ OpenApiString & enum_ ?~ ["observation", "project", "task"]
instance ToSchema ProjectStatus where declareNamedSchema _ = pure $ NamedSchema (Just "ProjectStatus") (mempty & type_ ?~ OpenApiString & enum_ ?~ ["active", "paused", "completed", "archived"])
instance ToParamSchema ProjectStatus where toParamSchema _ = mempty & type_ ?~ OpenApiString
instance ToSchema TaskStatus where declareNamedSchema _ = pure $ NamedSchema (Just "TaskStatus") (mempty & type_ ?~ OpenApiString & enum_ ?~ ["todo", "in_progress", "blocked", "done", "cancelled"])
instance ToParamSchema TaskStatus where toParamSchema _ = mempty & type_ ?~ OpenApiString
instance ToSchema AuditAction where declareNamedSchema _ = pure $ NamedSchema (Just "AuditAction") (mempty & type_ ?~ OpenApiString & enum_ ?~ ["create", "update", "delete"])
instance ToParamSchema AuditAction where toParamSchema _ = mempty & type_ ?~ OpenApiString
instance ToSchema WorkspaceType where declareNamedSchema _ = pure $ NamedSchema (Just "WorkspaceType") (mempty & type_ ?~ OpenApiString & enum_ ?~ ["repository", "planning", "personal", "organization"])

instance ToSchema ObservationEmbedding where
  declareNamedSchema _ = pure $ NamedSchema (Just "ObservationEmbedding") embeddingSchema
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
    pure $ NamedSchema name (withLegacySubjectProperties schema)
instance ToSchema CreateObservation where
  declareNamedSchema _ = pure $ NamedSchema (Just "CreateObservation") $ mempty
    & description ?~ "Create with exactly one non-empty canonical `subjects` array or the complete deprecated legacy `subject_kind` plus `subject` pair. The forms are mutually exclusive; missing or half legacy pairs are rejected. Duplicate canonical subjects are de-duplicated in first-occurrence order. Subjects and Git provenance are immutable after creation."
    & oneOf ?~ [Inline canonicalCreateObservationSchema, Inline legacyCreateObservationSchema]
instance ToSchema CreateObservationRequest where declareNamedSchema _ = declareNamedSchema (Proxy @CreateObservation)
instance ToSchema UpdateObservation where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema SimilarObservationQuery where
  declareNamedSchema _ = do
    NamedSchema name schema <- genericDeclareNamedSchema opts (Proxy @SimilarObservationQuery)
    pure $ NamedSchema name (schema & properties . at "embedding" ?~ Inline embeddingSchema)
instance ToSchema SimilarObservation where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema ObservationMatchQuery where
  declareNamedSchema _ = do
    NamedSchema name schema <- genericDeclareNamedSchema opts (Proxy @ObservationMatchQuery)
    pure $ NamedSchema name $ schema
      & description ?~ "Match concrete canonical repository-relative paths against stored file and glob subjects. Paths are ORed; optional subject_kind, git_sha, and query filters compose with the match. Input paths never accept globs or touch the repository filesystem. Each Observation appears once; matched_paths follow deduplicated caller path order and matched_subjects follow stored subject order. Results rank by text relevance when query is set, then updated_at DESC and id DESC. Pagination defaults to limit 50 and offset 0; limit is 1..200 and offset is 0..100000. git_sha is an exact immutable lowercase 40-character hexadecimal SHA."
      & properties . at "paths" ?~ Inline pathsSchema
instance ToSchema ObservationMatch where
  declareNamedSchema _ = do
    NamedSchema name schema <- genericDeclareNamedSchema opts (Proxy @ObservationMatch)
    pure $ NamedSchema name $ schema
      & description ?~ "One matching Observation with evidence. matched_paths are in deduplicated caller path order; matched_subjects are in stored subject order."
instance ToSchema ObservationMatchRequest where declareNamedSchema _ = declareNamedSchema (Proxy @ObservationMatchQuery)
instance ToSchema WorkspaceTimelineEvent where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema TimelineActor where declareNamedSchema = genericDeclareNamedSchema timelineActorOpts
instance ToSchema TimelineProjectContext where declareNamedSchema = genericDeclareNamedSchema timelineProjectContextOpts
instance ToSchema TimelineTaskContext where declareNamedSchema = genericDeclareNamedSchema timelineTaskContextOpts
instance ToSchema TimelineStatusTransition where declareNamedSchema = genericDeclareNamedSchema timelineStatusTransitionOpts
instance ToSchema TimelineNavigation where declareNamedSchema = genericDeclareNamedSchema timelineNavigationOpts
instance ToSchema TimelineBucketCounts where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema TimelineBucketEntityCounts where declareNamedSchema = genericDeclareNamedSchema timelineBucketEntityCountsOpts
instance ToSchema WorkspaceTimelineBucket where declareNamedSchema = genericDeclareNamedSchema timelineBucketOpts
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
instance ToSchema CreateWorkspace where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema WorkspaceGroup where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema CreateWorkspaceGroup where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema WorkspaceGroupMemberInput where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema Project where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema CreateProject where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema UpdateProject where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema CascadeResult where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema ProjectOverview where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema ProjectReadinessRollup where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema NextTaskCandidate where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema Task where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema CreateTask where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema UpdateTask where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema TaskMutationResult where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema TaskDependencyStatusChange where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema LinkDependency where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema DependencyMutationResult where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema TaskOverview where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema TaskReadinessRollup where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema TaskDependencySummary where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema UnifiedSearchQuery where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema UnifiedSearchResults where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema AuditLogEntry where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema RevertResult where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema WebSocketTicketRequest where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema WebSocketTicketResponse where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema SessionContext where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema SessionPrincipal where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema SessionGlobalPermissions where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema SessionWorkspaceContext where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema a => ToSchema (PaginatedResult a) where declareNamedSchema = genericDeclareNamedSchema opts

timelineActorOpts :: SchemaOptions
timelineActorOpts = opts { fieldLabelModifier = \case
  "actorType" -> "type"; "actorId" -> "id"; "actorLabel" -> "label"; other -> camelToSnake other }

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
  "timelineBucketStart" -> "bucket_start"; "timelineBucketEnd" -> "bucket_end"; "timelineBucketLabel" -> "label"; "timelineBucketCounts" -> "counts"; "timelineBucketTotals" -> "totals"; other -> camelToSnake other }

timelineBucketsResponseOpts :: SchemaOptions
timelineBucketsResponseOpts = opts { fieldLabelModifier = \case
  "timelineBucketsWorkspaceId" -> "workspace_id"; "timelineBucketsSince" -> "since"; "timelineBucketsUntil" -> "until"; "timelineBucketsBucket" -> "bucket"; "timelineBucketsBuckets" -> "buckets"; other -> camelToSnake other }
