{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | OpenAPI definitions are kept beside the Servant contract so removed
-- endpoints cannot accidentally survive in generated documentation.
module HMem.Server.OpenAPI (openApiSpec) where

import Control.Lens ((&), (.~), (%~), (?~), (^.), at, traversed, _Just)
import Data.Aeson (Value)
import Data.HashMap.Strict.InsOrd qualified as InsOrdMap
import Data.HashSet.InsOrd qualified as InsOrdSet
import Data.OpenApi
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import Servant.OpenApi (toOpenApi)

import HMem.Server.API (HMemAPI, CreateObservationRequest, ObservationMatchRequest, LinkDependencyRequest, UpdateWorkspaceRequest)
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
  & paths . at "/api/v1/observations/subject-facets" . _Just . get %~ fmap tagObservation
  & paths . at "/api/v1/observations/match" . _Just . post %~ fmap tagObservation
  & paths . at "/api/v1/observations/similar" . _Just . post %~ fmap tagObservation
  & paths . at "/api/v1/observations/{observationId}" . _Just . get %~ fmap tagObservation
  & paths . at "/api/v1/observations/{observationId}" . _Just . put %~ fmap tagObservation
  & paths . at "/api/v1/observations/{observationId}" . _Just . delete %~ fmap tagObservation
  & paths . at "/api/v1/observations/{observationId}/embedding" . _Just . put %~ fmap tagObservation
  & paths . at "/api/v1/workspaces/{workspaceId}" . _Just . put %~ fmap tagWorkspaceRename
  & paths . at "/api/v1/workspaces/{workspaceId}/timeline" . _Just . get %~ fmap tagTimeline
  & paths . at "/api/v1/workspaces/{workspaceId}/timeline/buckets" . _Just . get %~ fmap tagTimeline
  & paths . at "/api/v1/workspaces/{workspaceId}/navigation" . _Just . get . _Just . parameters . traversed %~ capNavigationParameter
  & paths . at "/api/v1/workspaces/{workspaceId}/navigation/focus/{entityType}/{entityId}" . _Just . get . _Just . parameters . traversed %~ capNavigationParameter
  & paths . at "/api/v1/tasks/{taskId}/dependencies" . _Just . get . _Just . parameters . traversed %~ capNavigationParameter
  & paths . at "/api/v1/tasks/{taskId}/dependencies" . _Just . post %~ fmap documentDependencyAdd
  & paths . at "/api/v1/tasks/{taskId}/dependencies/{dependsOnId}" . _Just . delete %~ fmap documentDependencyRemove
  where
    tagObservation operation = operation & tags .~ InsOrdSet.singleton "Observations"
    tagWorkspaceGroups operation = operation & tags .~ InsOrdSet.singleton "Workspace Groups"
    tagTimeline operation = operation & tags .~ InsOrdSet.singleton "Timeline"
    tagWorkspaceRename operation = operation
      & tags .~ InsOrdSet.singleton "Workspaces"
      & description ?~ "Renames an active workspace. The request body accepts only name; malformed, unauthenticated, forbidden, and missing requests return the standard 400, 401, 403, and 404 responses."
      & responses %~ (<> workspaceRenameErrors)

    workspaceRenameErrors = Responses Nothing $ InsOrdMap.fromList
      [ (400, Inline (mempty & description .~ "Validation error. The JSON body must be an object with exactly one string name field whose value is non-blank and no more than 1024 bytes."))
      , (401, Inline (mempty & description .~ "Unauthenticated."))
      , (403, Inline (mempty & description .~ "Forbidden for the requested workspace."))
      , (404, Inline (mempty & description .~ "The active workspace was not found."))
      ]

    documentDependencyAdd operation = operation
      & description ?~ "Mutates one prerequisite edge. Requests that would introduce a direct or transitive task-dependency cycle return the structured 400 dependency_cycle error; the dependency graph and automatic task blocking state are unchanged."
      & responses %~ (<> dependencyAddErrors)

    dependencyAddErrors = Responses Nothing $ InsOrdMap.fromList
      [ (400, Inline (mempty & description .~ "Validation error. A cycle is reported as {error: dependency_cycle, message: Task dependency would create a cycle}; self-edges and cross-workspace edges are also rejected with 400.")) ]

    documentDependencyRemove operation = operation
      & description ?~ "Removes one prerequisite edge. Self-edges and cross-workspace edges are rejected with 400."
      & responses %~ (<> dependencyRemoveErrors)

    dependencyRemoveErrors = Responses Nothing $ InsOrdMap.fromList
      [ (400, Inline (mempty & description .~ "Validation error. Self-edges and cross-workspace edges are rejected with 400.")) ]

capNavigationParameter parameterRef = case parameterRef of
  Inline parameter
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
instance ToSchema WorkspaceTimelineBucket where
  declareNamedSchema _ = do
    NamedSchema name schema <- genericDeclareNamedSchema timelineBucketOpts (Proxy @WorkspaceTimelineBucket)
    pure $ NamedSchema name $ schema
      & properties . at "counts" ?~ deprecatedTimelineProjection "TimelineBucketEntityCounts"
      & properties . at "totals" ?~ deprecatedTimelineProjection "TimelineBucketCounts"
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
instance ToSchema ProjectCardSummary where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema TaskCardSummary where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema TaskDependencyPage where
  declareNamedSchema _ = do
    item <- declareSchemaRef (Proxy @TaskDependencySummary)
    pure $ NamedSchema (Just "TaskDependencyPage") $ mempty
      & type_ ?~ OpenApiObject
      & description ?~ "A bounded dependency-summary page. items are ordered by lower(name), id and contain at most 100 entries."
      & properties . at "items" ?~ Inline (mempty & type_ ?~ OpenApiArray & items ?~ OpenApiItemsObject item & maxItems ?~ fromIntegral maxNavigationPageSize)
      & properties . at "has_more" ?~ Inline (mempty & type_ ?~ OpenApiBoolean)
      & required .~ ["items", "has_more"]
      & additionalProperties ?~ AdditionalPropertiesAllowed False

uuidSchema :: Schema
uuidSchema = mempty & type_ ?~ OpenApiString & format ?~ "uuid"

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
    & properties . at "show_only" ?~ Inline (mempty & type_ ?~ OpenApiString & enum_ ?~ ["projects", "tasks"])
    & properties . at "project_statuses" ?~ Inline (mempty & type_ ?~ OpenApiArray)
    & properties . at "task_statuses" ?~ Inline (mempty & type_ ?~ OpenApiArray)
    & properties . at "priority_mode" ?~ Inline (mempty & type_ ?~ OpenApiString & enum_ ?~ ["any", "exact", "above", "below"])
    & properties . at "priority_value" ?~ Inline (mempty & type_ ?~ OpenApiInteger & minimum_ ?~ 1 & maximum_ ?~ 10)
    & properties . at "query" ?~ Inline (mempty & type_ ?~ OpenApiString & minLength ?~ 1)
instance ToSchema NavigationBranchRequest where declareNamedSchema = genericDeclareNamedSchema opts
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
instance ToSchema SessionGlobalPermissions where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema SessionWorkspaceContext where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema a => ToSchema (PaginatedResult a) where declareNamedSchema = genericDeclareNamedSchema opts

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
  "timelineBucketStart" -> "bucket_start"; "timelineBucketEnd" -> "bucket_end"; "timelineBucketLabel" -> "label"; "timelineBucketCounts" -> "counts"; "timelineBucketTotals" -> "totals"; "timelineBucketSeries" -> "series"; "timelineBucketSeriesTotals" -> "series_totals"; other -> camelToSnake other }

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
  "timelineBucketsWorkspaceId" -> "workspace_id"; "timelineBucketsSince" -> "since"; "timelineBucketsUntil" -> "until"; "timelineBucketsBucket" -> "bucket"; "timelineBucketsBuckets" -> "buckets"; other -> camelToSnake other }
