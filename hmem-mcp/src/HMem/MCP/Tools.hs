module HMem.MCP.Tools
  ( toolDefinitions
  , handleToolCall
  , parseToolCall
  , validateToolCall
  , ToolCall(..)
  , compactObservationSummary
  , compactObservationDetail
  , compactObservationList
  , compactObservationMatches
  , compactSimilarObservations
  , compactSearchResults
  , compactProjectSummary
  , compactTaskSummary
  , mcpProvenanceHeadersFor
  , mcpResultWith
  ) where

import Control.Exception (SomeAsyncException, SomeException, fromException, throwIO, try)
import Data.Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types (parseEither)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BL
import Data.Int (Int32)
import Data.Foldable (toList)
import Data.List (nub)
import Data.Maybe (catMaybes, fromMaybe)
import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUIDv4
import System.Environment (lookupEnv)
import Network.HTTP.Client
import Network.HTTP.Types (HeaderName)
import Network.HTTP.Types.Status (statusCode)
import Network.HTTP.Types.URI (urlEncode)

import HMem.Types

-- The MCP surface deliberately mirrors the Observation-only public API.  The
-- workspace id is supplied by the MCP session context rather than advertised
-- on every scoped tool.
toolDefinitions :: [Value]
toolDefinitions =
  [ tool "set_workspace" "Set the active workspace context. Scoped calls can then omit workspace_id." (schema ["workspace_id" .= nullableProp "Workspace UUID (or null to clear)"] [])
  , tool "get_workspace" "Get the active workspace context UUID, if any." (schema [] [])
  , tool "workspace_list" "List registered workspaces." (schema ["limit" .= prop "integer" "Maximum results (default 50)"] [])
  , tool "workspace_register" "Register a workspace." (schema ["name" .= prop "string" "Workspace name", "workspace_type" .= enumProp "Workspace type" ["repository", "planning", "personal", "organization"]] ["name"])
  , tool "workspace_update" "Rename a workspace. Only its display name is mutable." (strictSchema ["workspace_id" .= prop "string" "Workspace UUID", "name" .= prop "string" "New workspace display name"] ["workspace_id", "name"])
  , tool "search" "Search observations, projects, and tasks. An Observation is a durable, non-obvious repository insight tied to file or glob subjects; subject_kind, subject, and git_sha are exact provenance filters." (schema
      [ "query" .= prop "string" "Optional full-text query"
      , "entity_types" .= arrayEnum "Entity types (default: observation, project, task)" ["observation", "project", "task"]
      , "subject_kind" .= enumProp "Exact kind of repository subject tied to observations" ["file", "glob"]
      , "subject" .= prop "string" "Exact repository-relative subject tied to observations"
      , "git_sha" .= prop "string" "Exact Git SHA where an Observation insight was established; use it to select potentially stale insights for re-audit"
      , "project_status" .= enumProp "Project status" ["active", "paused", "completed", "archived"]
      , "task_status" .= enumProp "Task status" ["todo", "in_progress", "blocked", "done", "cancelled"]
      , "project_id" .= prop "string" "Filter tasks by project UUID"
      , "limit" .= prop "integer" "Maximum results per entity type"
      , "offset" .= prop "integer" "Result offset per entity type"
      ] [])
  , tool "observation_create" "Create an Observation: a durable, non-obvious repository insight tied to one or more repository-relative file or glob subjects. git_sha records the repository state where the insight was established; use it as a sentinel to decide whether the insight needs re-audit, not as timeless proof. Pass subjects as an ordered array; they are OR alternatives and, with git_sha, immutable after creation. File subjects must be concrete paths. Glob subjects may use only *, ?, and ** path components (for example my/src/proj/**/*.java)." (schema
      [ "subjects" .= object ["type" .= ("array" :: Text), "description" .= ("One to " <> T.pack (show maxObservationSubjects) <> " ordered repository-relative file or glob subjects tied to this durable insight; duplicate entries are removed in first-occurrence order"), "minItems" .= (1 :: Int), "maxItems" .= maxObservationSubjects, "items" .= object ["type" .= ("object" :: Text), "properties" .= object ["subject_kind" .= enumProp "Kind of repository subject tied to this durable insight" ["file", "glob"], "subject" .= prop "string" "Canonical repository-relative path or safe glob tied to this durable insight"], "required" .= (["subject_kind", "subject"] :: [Text])]]
      , "git_sha" .= prop "string" "Lowercase 40-character Git SHA for the repository state where this insight was established; a staleness-audit sentinel, not timeless proof"
      , "content" .= prop "string" "Durable, non-obvious repository insight about its subjects; not a progress update or routine fact"
      ] ["subjects", "git_sha", "content"])
  , tool "observation_get" "Get an observation by ID, including content and immutable provenance." (schema ["observation_id" .= prop "string" "Observation UUID"] ["observation_id"])
  , tool "observation_update" "Replace only the content of a durable, non-obvious repository insight. Subjects and git_sha are immutable provenance; git_sha remains the state where the insight was established and a staleness-audit sentinel, not timeless proof." (schema ["observation_id" .= prop "string" "Observation UUID", "content" .= prop "string" "Replacement durable, non-obvious repository insight about the existing subjects; not a progress update or routine fact"] ["observation_id", "content"])
  , tool "observation_list" "List durable, non-obvious repository insights using exact subject and git_sha provenance filters and optional text search. git_sha is a sentinel for deciding when an insight needs re-audit, not timeless proof. When has_more is true, pass next_offset to retrieve the next page." (schema
      [ "subject_kind" .= enumProp "Exact kind of repository subject tied to observations" ["file", "glob"]
      , "subject" .= prop "string" "Exact repository-relative subject tied to observations"
      , "git_sha" .= prop "string" "Exact Git SHA where an Observation insight was established; use it to select potentially stale insights for re-audit"
      , "query" .= prop "string" "Optional full-text query over durable repository insights"
      , "limit" .= prop "integer" "Maximum results (1-200)"
      , "offset" .= prop "integer" "Result offset"
      ] [])
  , tool "observation_match" "Find durable, non-obvious repository insights whose stored file subjects or safe glob subjects match any concrete repository-relative path supplied in paths. Paths are ORed; do not pass globs here and no repository filesystem is read. Optional filters compose with matching. git_sha is a staleness-audit sentinel, not timeless proof. Continue with next_offset until has_more is false." (schema
      [ "paths" .= object ["type" .= ("array" :: Text), "description" .= ("One to 256 concrete repository-relative files to match against stored Observation subjects; globs are rejected" :: Text), "minItems" .= (1 :: Int), "maxItems" .= (256 :: Int), "items" .= prop "string" "Concrete repository-relative file to match against Observation subjects"]
      , "subject_kind" .= enumProp "Filter matching repository subjects by kind" ["file", "glob"]
      , "git_sha" .= prop "string" "Exact Git SHA where an Observation insight was established; use it to select potentially stale insights for re-audit"
      , "query" .= prop "string" "Optional full-text query over durable repository insights"
      , "limit" .= prop "integer" "Maximum results (1-200)"
      , "offset" .= prop "integer" "Result offset"
      ] ["paths"])
  , tool "observation_delete" "Delete an observation by ID." (schema ["observation_id" .= prop "string" "Observation UUID"] ["observation_id"])
  , tool "observation_set_embedding" "Set the exact 1536-dimension embedding for an observation." (schema
      [ "observation_id" .= prop "string" "Observation UUID"
      , "embedding" .= object ["type" .= ("array" :: Text), "description" .= ("Exactly 1536 finite numeric dimensions" :: Text), "minItems" .= (observationEmbeddingDimensions :: Int), "maxItems" .= (observationEmbeddingDimensions :: Int), "items" .= object ["type" .= ("number" :: Text)]]
      ] ["observation_id", "embedding"])
  , tool "observation_similar" "Find semantically similar durable, non-obvious repository insights. Subject and git_sha filters are exact provenance filters; git_sha is a staleness-audit sentinel, not timeless proof. To continue, add returned_count to offset and repeat until returned_count is less than limit or zero." (schema
      [ "subject_kind" .= enumProp "Exact kind of repository subject tied to observations" ["file", "glob"]
      , "subject" .= prop "string" "Exact repository-relative subject tied to observations"
      , "git_sha" .= prop "string" "Exact Git SHA where an Observation insight was established; use it to select potentially stale insights for re-audit"
      , "embedding" .= object ["type" .= ("array" :: Text), "description" .= ("Exactly 1536 finite numeric dimensions" :: Text), "minItems" .= (observationEmbeddingDimensions :: Int), "maxItems" .= (observationEmbeddingDimensions :: Int), "items" .= object ["type" .= ("number" :: Text)]]
      , "min_similarity" .= prop "number" "Minimum similarity from 0 through 1"
      , "limit" .= prop "integer" "Maximum results (1-200)"
      , "offset" .= prop "integer" "Result offset"
      ] ["embedding"])
  , tool "project_create" "Create a project in the active workspace. Its description is a durable specification; status records execution state." (schema ["name" .= prop "string" "Project name", "description" .= prop "string" "Optional durable project specification: aims, scope, constraints, approach, and acceptance intent; not a log of progress or updates", "parent_id" .= prop "string" "Optional parent project UUID for hierarchy", "priority" .= prop "integer" "Priority 1 through 10"] ["name"])
  , tool "project_update" "Update a project's durable specification, hierarchy, or execution state. Descriptions are not logs of progress or updates." (schema ["project_id" .= prop "string" "Project UUID", "name" .= prop "string" "Project name", "description" .= prop "string" "Durable project specification: aims, scope, constraints, approach, and acceptance intent, or null; not a log of progress or updates", "parent_id" .= prop "string" "Parent project UUID for hierarchy, or null", "status" .= enumProp "Execution state; record progress here, not in the description" ["active", "paused", "completed", "archived"], "priority" .= prop "integer" "Priority"] ["project_id"])
  , tool "project_detail" "Get compact project details." (schema ["project_id" .= prop "string" "Project UUID"] ["project_id"])
  , tool "project_overview" "Get a compact project overview with tasks and subprojects." (schema ["project_id" .= prop "string" "Project UUID"] ["project_id"])
  , tool "project_next_tasks" "Get actionable tasks for a project subtree." (schema ["project_id" .= prop "string" "Project UUID", "limit" .= prop "integer" "Maximum candidates", "include_blocked" .= prop "boolean" "Include blocked candidates"] ["project_id"])
  , tool "project_spec" "Create a project and its initial atomic tasks in one call. Descriptions are durable specifications; status records execution state. Create later-discovered atomic work as subtasks." (schema
      [ "name" .= prop "string" "Project name"
      , "description" .= prop "string" "Optional durable project specification: aims, scope, constraints, approach, and acceptance intent; not a log of progress or updates"
      , "priority" .= prop "integer" "Project priority"
      , "tasks" .= object
          [ "type" .= ("array" :: Text)
          , "description" .= ("Initial atomic tasks; later-discovered atomic work must be created as subtasks, not appended to a parent description" :: Text)
          , "items" .= object
              [ "type" .= ("object" :: Text)
              , "properties" .= object
                  [ "title" .= prop "string" "Atomic task title"
                  , "description" .= prop "string" "Durable task specification: scope, constraints, approach, and acceptance intent; not a log of progress or updates"
                  , "priority" .= prop "integer" "Task priority"
                  ]
              , "required" .= (["title"] :: [Text])
              ]
          ]
      ] ["name", "tasks"])
  , tool "project_archive" "Archive a project by changing only its status." (schema ["project_id" .= prop "string" "Project UUID"] ["project_id"])
  , tool "task_create" "Create a task in the active workspace. Its description is a durable specification; status records execution state. Create later-discovered atomic work as subtasks." (schema ["project_id" .= prop "string" "Optional project UUID", "title" .= prop "string" "Atomic task title", "description" .= prop "string" "Optional durable task specification: scope, constraints, approach, and acceptance intent; not a log of progress or updates", "parent_id" .= prop "string" "Optional parent task UUID; use it to create a subtask for later-discovered atomic work", "priority" .= prop "integer" "Priority", "due_at" .= prop "string" "ISO-8601 due time"] ["title"])
  , tool "task_update" "Update a task's durable specification, hierarchy, or execution state. Descriptions are not logs of progress or updates; create later-discovered atomic work as subtasks." (schema ["task_id" .= prop "string" "Task UUID", "title" .= prop "string" "Task title", "description" .= prop "string" "Durable task specification: scope, constraints, approach, and acceptance intent, or null; not a log of progress or updates", "project_id" .= prop "string" "Project UUID or null", "parent_id" .= prop "string" "Parent task UUID to make this an atomic subtask for later-discovered work, or null", "status" .= enumProp "Execution state; record progress here, not in the description" ["todo", "in_progress", "blocked", "done", "cancelled"], "priority" .= prop "integer" "Priority", "due_at" .= prop "string" "ISO-8601 due time or null"] ["task_id"])
  , tool "task_move_batch" "Atomically move one to 100 tasks and their active descendants to one project or no project. Submit every dependency-connected task that must move so no dependency edge crosses a project boundary." (strictSchema
      [ "task_ids" .= object
          [ "type" .= ("array" :: Text)
          , "description" .= ("Task UUIDs to move atomically; duplicates affect each task once" :: Text)
          , "minItems" .= (1 :: Int)
          , "maxItems" .= (100 :: Int)
          , "items" .= prop "string" "Task UUID"
          ]
      , "project_id" .= nullableProp "Destination project UUID, or null/omitted to detach the tasks from a project"
      ] ["task_ids"])
  , tool "task_detail" "Get compact task details." (schema ["task_id" .= prop "string" "Task UUID"] ["task_id"])
  , tool "task_overview" "Get a compact task overview and dependency summaries." (schema ["task_id" .= prop "string" "Task UUID"] ["task_id"])
  , tool "task_dependency" "Add or remove a prerequisite edge: task_id cannot proceed until depends_on_id is complete. Use dependencies for ordering, not logs of progress or updates." (schema ["task_id" .= prop "string" "Dependent task UUID", "depends_on_id" .= prop "string" "Prerequisite task UUID", "action" .= enumProp "Dependency mutation" ["add", "remove"]] ["task_id", "depends_on_id", "action"])
  , tool "task_start" "Set a task's execution state to in_progress. Preserve its description as a durable specification; status records progress." (schema ["task_id" .= prop "string" "Task UUID"] ["task_id"])
  , tool "task_finish" "Set a task's execution state to done, blocked, or cancelled. Status records progress; this does not create an observation." (schema ["task_id" .= prop "string" "Task UUID", "status" .= enumProp "Final execution state" ["done", "blocked", "cancelled"]] ["task_id", "status"])
  ]
  where
    tool name description inputSchema = object ["name" .= (name :: Text), "description" .= (description :: Text), "inputSchema" .= inputSchema]
    schema properties required = object ["type" .= ("object" :: Text), "properties" .= object properties, "required" .= (required :: [Text])]
    strictSchema properties required = object ["type" .= ("object" :: Text), "properties" .= object properties, "required" .= (required :: [Text]), "additionalProperties" .= False]
    prop typ description = object ["type" .= (typ :: Text), "description" .= (description :: Text)]
    nullableProp description = object ["description" .= (description :: Text), "anyOf" .= [object ["type" .= ("string" :: Text)], object ["type" .= ("null" :: Text)]]]
    enumProp description choices = object ["type" .= ("string" :: Text), "description" .= (description :: Text), "enum" .= (choices :: [Text])]
    arrayEnum description choices = object ["type" .= ("array" :: Text), "description" .= (description :: Text), "items" .= object ["type" .= ("string" :: Text), "enum" .= (choices :: [Text])]]

-- Parsed calls retain current core DTOs; no compatibility aliases exist.
data ToolCall
  = ObservationCreate CreateObservation
  | ObservationGet UUID
  | ObservationUpdate UUID UpdateObservation
  | ObservationList ObservationQuery
  | ObservationMatchCall ObservationMatchQuery
  | ObservationDelete UUID
  | ObservationSetEmbedding UUID ObservationEmbedding
  | ObservationSimilar SimilarObservationQuery
  | WorkspaceList (Maybe Int)
  | WorkspaceRegister CreateWorkspace
  | WorkspaceUpdate UUID UpdateWorkspace
  | UnifiedSearch UnifiedSearchQuery
  | ProjectCreate CreateProject
  | ProjectUpdate UUID UpdateProject
  | ProjectDetail UUID
  | ProjectOverviewCall UUID
  | ProjectNextTasks UUID (Maybe Int) Bool
  | ProjectSpec UUID Text (Maybe Text) (Maybe Int32) [SpecTask]
  | ProjectArchive UUID
  | TaskCreate CreateTask
  | TaskUpdate UUID UpdateTask
  -- The workspace context is injected by HMem.MCP.Server and deliberately is
  -- not part of BatchMoveTasksRequest, which remains the REST body.
  | TaskMoveBatch (Maybe UUID) BatchMoveTasksRequest
  | TaskDetail UUID
  | TaskOverviewCall UUID
  | TaskDependency UUID UUID Text
  | TaskStart UUID
  | TaskFinish UUID TaskStatus
  deriving (Show, Eq)

data SpecTask = SpecTask { specTitle :: Text, specDescription :: Maybe Text, specPriority :: Maybe Int } deriving (Show, Eq)

parseToolCall :: Text -> Value -> Either String ToolCall
parseToolCall name args = case name of
  "observation_create" -> ObservationCreate <$> parseCreateObservation args
  "observation_get" -> ObservationGet <$> required "observation_id"
  "observation_update" -> ObservationUpdate <$> required "observation_id" <*> parseUpdateObservation args
  "observation_list" -> ObservationList <$> parse args
  "observation_match" -> ObservationMatchCall <$> parse args
  "observation_delete" -> ObservationDelete <$> required "observation_id"
  "observation_set_embedding" -> ObservationSetEmbedding <$> required "observation_id" <*> (ObservationEmbedding <$> required "embedding")
  "observation_similar" -> ObservationSimilar <$> parse args
  "workspace_list" -> WorkspaceList <$> optional "limit"
  "workspace_register" -> WorkspaceRegister <$> parse args
  "workspace_update" -> WorkspaceUpdate <$> required "workspace_id" <*> parseWorkspaceUpdate args
  "search" -> UnifiedSearch <$> parse args
  "project_create" -> ProjectCreate <$> parse args
  "project_update" -> ProjectUpdate <$> required "project_id" <*> parse args
  "project_detail" -> ProjectDetail <$> required "project_id"
  "project_overview" -> ProjectOverviewCall <$> required "project_id"
  "project_next_tasks" -> ProjectNextTasks <$> required "project_id" <*> optional "limit" <*> (fromMaybe False <$> optional "include_blocked")
  "project_spec" -> ProjectSpec <$> required "workspace_id" <*> required "name" <*> optional "description" <*> optional "priority" <*> parseTasks args
  "project_archive" -> ProjectArchive <$> required "project_id"
  "task_create" -> TaskCreate <$> parse args
  "task_update" -> TaskUpdate <$> required "task_id" <*> parse args
  "task_move_batch" -> uncurry TaskMoveBatch <$> parseBatchMoveTasks args
  "task_detail" -> TaskDetail <$> required "task_id"
  "task_overview" -> TaskOverviewCall <$> required "task_id"
  "task_dependency" -> TaskDependency <$> required "task_id" <*> required "depends_on_id" <*> required "action"
  "task_start" -> TaskStart <$> required "task_id"
  "task_finish" -> TaskFinish <$> required "task_id" <*> required "status"
  _ -> Left ("Unknown tool: " <> T.unpack name)
  where
    parse :: FromJSON a => Value -> Either String a
    parse = parseEither parseJSON
    required :: FromJSON a => Key.Key -> Either String a
    required key = parseEither (withObject "arguments" (.: key)) args
    optional :: FromJSON a => Key.Key -> Either String (Maybe a)
    optional key = parseEither (withObject "arguments" (.:? key)) args

parseUpdateObservation :: Value -> Either String UpdateObservation
parseUpdateObservation = parseEither $ withObject "observation_update" $ \o -> do
  contentValue <- o .: "content"
  let unexpected = filter (`notElem` ["observation_id", "content", "workspace_id"]) (Key.toText <$> KM.keys o)
  if null unexpected then pure (UpdateObservation contentValue)
  else fail ("observation_update accepts only observation_id and content; unexpected fields: " <> show unexpected)

parseWorkspaceUpdate :: Value -> Either String UpdateWorkspace
parseWorkspaceUpdate = parseEither $ withObject "workspace_update" $ \o -> do
  let unexpected = filter (`notElem` ["workspace_id", "name"]) (Key.toText <$> KM.keys o)
  if null unexpected then UpdateWorkspace <$> o .: "name"
  else fail ("workspace_update accepts only workspace_id and name; unexpected fields: " <> show unexpected)

parseBatchMoveTasks :: Value -> Either String (Maybe UUID, BatchMoveTasksRequest)
parseBatchMoveTasks = parseEither $ withObject "task_move_batch" $ \o -> do
  let unexpected = filter (`notElem` ["task_ids", "project_id", "workspace_id"]) (Key.toText <$> KM.keys o)
  if null unexpected then do
    injectedWorkspace <- case KM.lookup "workspace_id" o of
      Nothing -> pure Nothing
      Just (String workspaceText) -> case UUID.fromText workspaceText of
        Just workspace -> pure (Just workspace)
        Nothing -> fail "task_move_batch injected workspace_id must be a UUID"
      Just _ -> fail "task_move_batch injected workspace_id must be a UUID"
    batchRequest <- BatchMoveTasksRequest <$> o .: "task_ids" <*> o .:? "project_id"
    pure (injectedWorkspace, batchRequest)
  else fail ("task_move_batch accepts only task_ids and project_id; unexpected fields: " <> show unexpected)

-- | Core accepts the deprecated singleton form during the compatibility window.
-- The MCP registry advertises only @subjects@, but parsing retains the legacy
-- form so existing agents do not fail abruptly. Core parsing rejects mixed and
-- incomplete provenance forms before any HTTP request is made.
parseCreateObservation :: Value -> Either String CreateObservation
parseCreateObservation = parseEither $ withObject "observation_create" $ \o -> do
  let unexpected = filter (`notElem` ["workspace_id", "subjects", "subject_kind", "subject", "git_sha", "content"]) (Key.toText <$> KM.keys o)
  if null unexpected then parseJSON (Object o)
  else fail ("observation_create received unexpected fields: " <> show unexpected)

parseTasks :: Value -> Either String [SpecTask]
parseTasks = parseEither $ withObject "project_spec" $ \o -> do
  values <- o .: "tasks"
  mapM (withObject "task" $ \t -> SpecTask <$> t .: "title" <*> t .:? "description" <*> t .:? "priority") values

validateToolCall :: ToolCall -> Either String ToolCall
validateToolCall call = case call of
  ObservationCreate input -> checked (validateCreateObservationInput input) call
  ObservationUpdate _ input -> checked (validateUpdateObservationInput input) call
  ObservationList input -> checked (validateObservationQuery input) call
  ObservationMatchCall input -> checked (validateObservationMatchQuery input) call
  ObservationSetEmbedding _ (ObservationEmbedding values) -> checked (validateEmbedding values) call
  ObservationSimilar input -> checked (validateSimilarObservationQuery input) call
  WorkspaceRegister input -> checked (validateCreateWorkspaceInput input) call
  -- The rename endpoint deliberately authorizes before decoding/validating its
  -- body so malformed names cannot disclose workspace existence to callers
  -- without edit access.  Forward name values unchanged and let that
  -- authoritative endpoint return its structured 400 response.
  WorkspaceUpdate _ _ -> Right call
  UnifiedSearch input -> checked (validateUnifiedSearchQuery input) call
  ProjectCreate input -> checked (validateCreateProjectInput input) call
  ProjectUpdate _ input -> checked (validateUpdateProjectInput input) call
  TaskCreate input -> checked (validateCreateTaskInput input) call
  TaskUpdate _ input -> checked (validateUpdateTaskInput input) call
  TaskMoveBatch _ input -> checked (validateBatchMoveTasksRequest input) call
  TaskDependency taskId dependsOnId action
    | taskId == dependsOnId -> Left "task_dependency: a task cannot depend on itself"
    | action `notElem` ["add", "remove"] -> Left "task_dependency: action must be add or remove"
    | otherwise -> Right call
  ProjectSpec _ name _ _priority tasks
    | T.null (T.strip name) -> Left "project_spec: name must not be blank"
    | null tasks -> Left "project_spec: tasks must not be empty"
    | length tasks > 50 -> Left "project_spec: tasks must contain at most 50 tasks"
    | any (T.null . T.strip . (.specTitle)) tasks -> Left "project_spec: task titles must not be blank"
    | otherwise -> Right call
  WorkspaceList (Just n) | n < 1 || n > maxPaginationLimit -> Left "limit must be between 1 and 200"
  ProjectNextTasks _ (Just n) _ | n < 1 || n > maxPaginationLimit -> Left "limit must be between 1 and 200"
  _ -> Right call
  where
    checked errors value = case errors of [] -> Right value; _ -> Left (T.unpack (T.intercalate "; " errors))
    validateEmbedding values = ["embedding must contain exactly 1536 finite dimensions"
      | length values /= observationEmbeddingDimensions || any (\value -> isNaN value || isInfinite value) values]

handleToolCall :: Manager -> String -> Maybe Text -> Value -> IO Value
handleToolCall manager base apiKey params = case parseParams params >>= uncurry parseToolCall >>= validateToolCall of
  Left err -> pure (mcpError (T.pack err))
  Right call -> execute manager base apiKey call

parseParams :: Value -> Either String (Text, Value)
parseParams = parseEither $ withObject "params" $ \o -> (,) <$> o .: "name" <*> o .:? "arguments" .!= object []

execute :: Manager -> String -> Maybe Text -> ToolCall -> IO Value
execute manager base apiKey = \case
  ObservationCreate input -> request manager base apiKey "POST" "/api/v1/observations" (Just (encode input)) (mutationAck "created" "observation" . compactObservationSummary)
  ObservationGet oid -> request manager base apiKey "GET" ("/api/v1/observations/" <> uuidPath oid) Nothing compactObservationDetail
  ObservationUpdate oid input -> request manager base apiKey "PUT" ("/api/v1/observations/" <> uuidPath oid) (Just (encode input)) (mutationAck "updated" "observation" . compactObservationSummary)
  ObservationList input@(ObservationQuery _ _ _ _ _ _ offset) -> request manager base apiKey "GET" (observationListPath input) Nothing (compactObservationList (fromMaybe 0 offset))
  ObservationMatchCall input@(ObservationMatchQuery _ _ _ _ _ _ offset) -> request manager base apiKey "POST" "/api/v1/observations/match" (Just (encode input)) (compactObservationMatches (fromMaybe 0 offset))
  ObservationDelete oid -> noContentRequest manager base apiKey "DELETE" ("/api/v1/observations/" <> uuidPath oid) Nothing (statusAck "deleted" "observation" oid)
  ObservationSetEmbedding oid embeddingValue -> noContentRequest manager base apiKey "PUT" ("/api/v1/observations/" <> uuidPath oid <> "/embedding") (Just (encode embeddingValue)) (statusAck "embedding_set" "observation" oid)
  ObservationSimilar input@(SimilarObservationQuery _ _ _ _ _ _ limit offset) -> request manager base apiKey "POST" "/api/v1/observations/similar" (Just (encode input)) (compactSimilarObservations (fromMaybe 50 limit) (fromMaybe 0 offset))
  WorkspaceList limit -> request manager base apiKey "GET" ("/api/v1/workspaces" <> query [("limit", show <$> limit)]) Nothing compactWorkspaceList
  WorkspaceRegister input -> request manager base apiKey "POST" "/api/v1/workspaces" (Just (encode input)) (mutationAck "created" "workspace" . compactWorkspaceSummary)
  WorkspaceUpdate workspaceId input -> request manager base apiKey "PUT" ("/api/v1/workspaces/" <> uuidPath workspaceId) (Just (encode input)) (mutationAck "updated" "workspace" . compactWorkspaceSummary)
  UnifiedSearch input -> request manager base apiKey "POST" "/api/v1/search" (Just (encode input)) compactSearchResults
  ProjectCreate input -> request manager base apiKey "POST" "/api/v1/projects" (Just (encode input)) (mutationAck "created" "project" . compactProjectSummary)
  ProjectUpdate pid input -> request manager base apiKey "PUT" ("/api/v1/projects/" <> uuidPath pid) (Just (encode input)) (mutationAck "updated" "project" . compactProjectSummary)
  ProjectDetail pid -> request manager base apiKey "GET" ("/api/v1/projects/" <> uuidPath pid) Nothing compactProjectSummary
  ProjectOverviewCall pid -> request manager base apiKey "GET" ("/api/v1/projects/" <> uuidPath pid <> "/overview") Nothing compactProjectOverview
  ProjectNextTasks pid limit includeBlocked -> request manager base apiKey "GET" ("/api/v1/projects/" <> uuidPath pid <> "/next-tasks" <> query [("limit", show <$> limit), ("include_blocked", if includeBlocked then Just "true" else Nothing)]) Nothing compactNextTasks
  ProjectSpec wid name description priority tasks -> executeProjectSpec manager base apiKey wid name description priority tasks
  ProjectArchive pid -> request manager base apiKey "PUT" ("/api/v1/projects/" <> uuidPath pid) (Just (encode (object ["status" .= ("archived" :: Text)]))) (mutationAck "archived" "project" . compactProjectSummary)
  TaskCreate input -> request manager base apiKey "POST" "/api/v1/tasks" (Just (encode input)) (mutationAck "created" "task" . compactTaskSummary)
  TaskUpdate tid input -> request manager base apiKey "PUT" ("/api/v1/tasks/" <> uuidPath tid) (Just (encode input)) (mutationAck "updated" "task" . compactTaskSummary)
  TaskMoveBatch workspace input -> case workspace of
    Nothing -> dispatchBatchMove manager base apiKey input
    Just activeWorkspace -> do
      preflight <- validateBatchMoveWorkspace manager base apiKey activeWorkspace input
      case preflight of
        Left errorValue -> pure errorValue
        Right () -> dispatchBatchMove manager base apiKey input
  TaskDetail tid -> request manager base apiKey "GET" ("/api/v1/tasks/" <> uuidPath tid) Nothing compactTaskSummary
  TaskOverviewCall tid -> request manager base apiKey "GET" ("/api/v1/tasks/" <> uuidPath tid <> "/overview") Nothing compactTaskOverview
  TaskDependency tid depId "add" -> request manager base apiKey "POST" ("/api/v1/tasks/" <> uuidPath tid <> "/dependencies") (Just (encode (object ["depends_on_id" .= depId]))) compactTaskDependencyMutation
  TaskDependency tid depId "remove" -> request manager base apiKey "DELETE" ("/api/v1/tasks/" <> uuidPath tid <> "/dependencies/" <> uuidPath depId) Nothing compactTaskDependencyMutation
  TaskStart tid -> request manager base apiKey "PUT" ("/api/v1/tasks/" <> uuidPath tid) (Just (encode (object ["status" .= ("in_progress" :: Text)]))) (mutationAck "started" "task" . compactTaskSummary)
  TaskFinish tid status -> request manager base apiKey "PUT" ("/api/v1/tasks/" <> uuidPath tid) (Just (encode (object ["status" .= status]))) (mutationAck "finished" "task" . compactTaskSummary)

dispatchBatchMove :: Manager -> String -> Maybe Text -> BatchMoveTasksRequest -> IO Value
dispatchBatchMove manager base apiKey input =
  request manager base apiKey "POST" "/api/v1/tasks/batch-move" (Just (encode input)) (compactTaskBatchMove input.projectId)

-- | A session workspace is an authorization boundary rather than a field in
-- the batch-move REST DTO.  Verify every explicitly supplied endpoint before
-- allowing the mutation so a stale or foreign task/project cannot be moved by
-- a contextual call.
validateBatchMoveWorkspace :: Manager -> String -> Maybe Text -> UUID -> BatchMoveTasksRequest -> IO (Either Value ())
validateBatchMoveWorkspace manager base apiKey activeWorkspace input = do
  taskResults <- mapM (fetch "task" . ("/api/v1/tasks/" <>) . uuidPath) (nub input.taskIds)
  projectResult <- traverse (fetch "project" . ("/api/v1/projects/" <>) . uuidPath) input.projectId
  pure $ do
    mapM_ (>>= verifyWorkspace activeWorkspace) taskResults
    mapM_ (>>= verifyWorkspace activeWorkspace) projectResult
  where
    fetch entity path = do
      result <- rawRequest manager base apiKey "GET" path Nothing
      pure $ case result of
        Left errorValue -> Left errorValue
        Right value -> Right (entity, value)

verifyWorkspace :: UUID -> (Text, Value) -> Either Value ()
verifyWorkspace activeWorkspace (entity, value) = case textField "workspace_id" value >>= UUID.fromText of
  Just returnedWorkspace
    | returnedWorkspace == activeWorkspace -> Right ()
    | otherwise -> Left (mcpError ("task_move_batch " <> entity <> " workspace_id does not match the active workspace context"))
  Nothing -> Left (mcpError ("task_move_batch " <> entity <> " response omitted a valid workspace_id"))

executeProjectSpec :: Manager -> String -> Maybe Text -> UUID -> Text -> Maybe Text -> Maybe Int32 -> [SpecTask] -> IO Value
executeProjectSpec manager base apiKey wid name description priority specs = do
  projectResult <- rawRequest manager base apiKey "POST" "/api/v1/projects" (Just (encode (object ["workspace_id" .= wid, "name" .= name, "description" .= description, "priority" .= priority])))
  case projectResult of
    Left errorValue -> pure errorValue
    Right projectValue -> case textField "id" projectValue of
      Nothing -> pure (mcpError "Project creation response omitted id")
      Just projectId -> do
        created <- mapM (\spec -> rawRequest manager base apiKey "POST" "/api/v1/tasks" (Just (encode (object ["workspace_id" .= wid, "project_id" .= projectId, "title" .= spec.specTitle, "description" .= spec.specDescription, "priority" .= spec.specPriority])))) specs
        let taskValues = [value | Right value <- created]
        pure (mcpJSON (object ["ok" .= True, "action" .= ("created" :: Text), "entity_type" .= ("project_spec" :: Text), "project" .= compactProjectSummary projectValue, "tasks" .= map compactTaskSummary taskValues, "tasks_failed" .= length [() | Left _ <- created]]))

request :: Manager -> String -> Maybe Text -> String -> String -> Maybe BL.ByteString -> (Value -> Value) -> IO Value
request manager base apiKey method path body shape = do
  result <- rawRequest manager base apiKey method path body
  pure $ either id (mcpJSON . shape) result

noContentRequest :: Manager -> String -> Maybe Text -> String -> String -> Maybe BL.ByteString -> Value -> IO Value
noContentRequest manager base apiKey method path body acknowledgement = do
  outcome <- try $ do
    initial <- parseRequest (base <> path)
    provenance <- mcpProvenanceHeaders
    requestId <- UUIDv4.nextRandom
    let auth = maybe [] (\token -> [("Authorization", TE.encodeUtf8 ("Bearer " <> token))]) apiKey
        requestValue = initial { method = fromString method, requestHeaders = ("Content-Type", "application/json") : ("X-Request-Id", TE.encodeUtf8 (UUID.toText requestId)) : provenance <> auth, requestBody = maybe (RequestBodyBS mempty) RequestBodyLBS body }
    response <- httpLbs requestValue manager
    if statusCode (responseStatus response) >= 200 && statusCode (responseStatus response) < 300
      then pure (Right ())
      else pure (Left (httpError (statusCode (responseStatus response)) (responseBody response)))
  case outcome of
    Right result -> pure $ either id (const (mcpJSON acknowledgement)) result
    Left (exception :: SomeException) -> do
      case fromException exception :: Maybe SomeAsyncException of Just _ -> throwIO exception; Nothing -> pure ()
      pure (mcpError "Could not connect to hmem-server")

rawRequest :: Manager -> String -> Maybe Text -> String -> String -> Maybe BL.ByteString -> IO (Either Value Value)
rawRequest manager base apiKey method path body = do
  outcome <- try $ do
    initial <- parseRequest (base <> path)
    provenance <- mcpProvenanceHeaders
    requestId <- UUIDv4.nextRandom
    let auth = maybe [] (\token -> [("Authorization", TE.encodeUtf8 ("Bearer " <> token))]) apiKey
        requestValue = initial { method = fromString method, requestHeaders = ("Content-Type", "application/json") : ("X-Request-Id", TE.encodeUtf8 (UUID.toText requestId)) : provenance <> auth, requestBody = maybe (RequestBodyBS mempty) RequestBodyLBS body }
    response <- httpLbs requestValue manager
    if statusCode (responseStatus response) >= 200 && statusCode (responseStatus response) < 300
      then case eitherDecode (responseBody response) of
        Right value -> pure (Right value)
        Left err -> pure (Left (mcpError ("Invalid JSON response: " <> T.pack err)))
      else pure (Left (httpError (statusCode (responseStatus response)) (responseBody response)))
  case outcome of
    Right value -> pure value
    Left (exception :: SomeException) -> do
      case fromException exception :: Maybe SomeAsyncException of Just _ -> throwIO exception; Nothing -> pure ()
      pure (Left (mcpError "Could not connect to hmem-server"))

-- | Forwarded user authorization is not provenance.  The bridge adds this
-- independently configured private credential only when one is available.
mcpProvenanceHeaders :: IO [(HeaderName, ByteString)]
mcpProvenanceHeaders = mcpProvenanceHeadersFor <$> lookupEnv "HMEM_MCP_PROVENANCE_TOKEN"

mcpProvenanceHeadersFor :: Maybe String -> [(HeaderName, ByteString)]
mcpProvenanceHeadersFor configured =
  case configured >>= nonEmpty . T.strip . T.pack of
    Nothing -> [("X-HMem-Change-Cause", "mcp")]
    Just value -> [("X-HMem-Change-Cause", "mcp"), ("X-HMem-MCP-Provenance", TE.encodeUtf8 value)]
  where
    nonEmpty value
      | T.null value = Nothing
      | otherwise = Just value

compactObservationSummary :: Value -> Value
compactObservationSummary value = object (catMaybes [copy "id", copy "subjects", copy "subject_kind", copy "subject", copy "git_sha", preview])
  where
    copy key = (Key.fromText key .=) <$> field key value
    preview = case field "content_preview" value of
      Just value' -> Just ("content_preview" .= value')
      Nothing -> case field "content" value of
        Just (String contentValue) -> Just ("content_preview" .= T.take 500 contentValue)
        _ -> Nothing

compactObservationDetail :: Value -> Value
compactObservationDetail value = object (catMaybes [copy "id", copy "subjects", copy "subject_kind", copy "subject", copy "git_sha", copy "content"])
  where copy key = (Key.fromText key .=) <$> field key value

compactObservationList :: Int -> Value -> Value
compactObservationList offsetValue value = object
  ( [ "items" .= mapField "items" compactObservationSummary value
    , "has_more" .= hasMoreValue
    ] <> ["next_offset" .= (offsetValue + length (mapField "items" id value)) | hasMoreValue] )
  where
    hasMoreValue = field "has_more" value == Just (Bool True)

compactObservationMatches :: Int -> Value -> Value
compactObservationMatches offsetValue value = object
  ( [ "items" .= rows
    , "has_more" .= hasMoreValue
    , "returned_count" .= returnedCount
    ] <> ["next_offset" .= (offsetValue + returnedCount) | hasMoreValue] )
  where
    sourceRows = mapField "items" id value
    returnedCount = length sourceRows
    rows = deduplicateObservationMatches sourceRows
    hasMoreValue = field "has_more" value == Just (Bool True)

deduplicateObservationMatches :: [Value] -> [Value]
deduplicateObservationMatches = go []
  where
    go _ [] = []
    go seen (row:rest) = case field "observation" row >>= field "id" of
      Just identifier | identifier `elem` seen -> go seen rest
      Just identifier -> compactMatch row : go (identifier : seen) rest
      Nothing -> compactMatch row : go seen rest
    compactMatch row = object (catMaybes
      [ ("observation" .=) . compactObservationSummary <$> field "observation" row
      , ("path_matches" .=) <$> field "path_matches" row
      , ("matched_paths" .=) <$> field "matched_paths" row
      , ("matched_subjects" .=) <$> field "matched_subjects" row
      ])

compactSimilarObservations :: Int -> Int -> Value -> Value
compactSimilarObservations limitValue offsetValue value = object
  [ "items" .= rows
  , "offset" .= offsetValue
  , "limit" .= limitValue
  , "returned_count" .= length rows
  ]
  where
    rows = case value of
      Array observations -> map compactSimilar (toList observations)
      _ -> []
    compactSimilar observationValue = object (catMaybes
      [ ("observation" .=) . compactObservationSummary <$> field "observation" observationValue
      , ("similarity" .=) <$> field "similarity" observationValue
      ])

compactProjectSummary :: Value -> Value
compactProjectSummary value = object (catMaybes [copy "id", copy "name", copy "description", copy "status", copy "priority", copy "parent_id"])
  where copy key = (Key.fromText key .=) <$> field key value

compactTaskSummary :: Value -> Value
compactTaskSummary value = object (catMaybes [copy "id", copy "title", copy "description", copy "status", copy "priority", copy "project_id", copy "parent_id", copy "due_at"])
  where copy key = (Key.fromText key .=) <$> field key value

compactTaskBatchMove :: Maybe UUID -> Value -> Value
compactTaskBatchMove destination value = object
  [ "ok" .= True
  , "action" .= ("moved" :: Text)
  , "entity_type" .= ("task" :: Text)
  , "affected" .= fromMaybe (Number 0) (field "affected" value)
  , "project_id" .= destination
  ]

compactSearchResults :: Value -> Value
compactSearchResults value = object
  [ "observations" .= mapField "observations" compactObservationSummary value
  , "projects" .= mapField "projects" compactProjectSummary value
  , "tasks" .= mapField "tasks" compactTaskSummary value
  ]

compactProjectOverview :: Value -> Value
compactProjectOverview value = object
  [ "project" .= maybe Null compactProjectSummary (field "project" value)
  , "tasks" .= mapField "tasks" compactTaskSummary value
  , "subprojects" .= mapField "subprojects" compactProjectSummary value
  , "readiness_rollup" .= fromMaybe (object []) (field "readiness_rollup" value)
  ]

compactTaskOverview :: Value -> Value
compactTaskOverview value = object
  [ "task" .= maybe Null compactTaskSummary (field "task" value)
  , "dependencies" .= fromMaybe (Array mempty) (field "dependencies" value)
  , "readiness_rollup" .= fromMaybe (object []) (field "readiness_rollup" value)
  ]

compactTaskDependencyMutation :: Value -> Value
compactTaskDependencyMutation value = object
  [ "ok" .= True
  , "action" .= fromMaybe Null (field "action" value)
  , "entity_type" .= ("task_dependency" :: Text)
  , "task_id" .= fromMaybe Null (field "task_id" value)
  , "depends_on_id" .= fromMaybe Null (field "depends_on_id" value)
  , "affected_tasks" .= mapField "affected_tasks" compactChange value
  ]
  where
    compactChange change = object (catMaybes
      [ ("task" .=) . compactTaskSummary <$> field "task" change
      , ("previous_status" .=) <$> field "previous_status" change
      , ("current_status" .=) <$> field "current_status" change
      , ("auto_blocked" .=) <$> field "auto_blocked" change
      , ("open_dependency_count" .=) <$> field "open_dependency_count" change
      , ("reason" .=) <$> field "reason" change
      ])

compactNextTasks :: Value -> Value
compactNextTasks (Array rows) = toJSON (map compactRow (toList rows)) where
  compactRow row = case field "task" row of
    Just task -> object ["task" .= compactTaskSummary task, "completion_gated" .= fromMaybe (Bool False) (field "completion_gated" row), "dependency_blocked" .= fromMaybe (Bool False) (field "dependency_blocked" row), "open_dependency_count" .= fromMaybe (Number 0) (field "open_dependency_count" row)]
    Nothing -> compactTaskSummary row
compactNextTasks _ = toJSON ([] :: [Value])

compactWorkspaceSummary :: Value -> Value
compactWorkspaceSummary value = object (catMaybes [copy "id", copy "name", copy "workspace_type"])
  where copy key = (Key.fromText key .=) <$> field key value

compactWorkspaceList :: Value -> Value
compactWorkspaceList value = object ["items" .= mapField "items" compactWorkspaceSummary value, "has_more" .= fromMaybe (Bool False) (field "has_more" value)]

mutationAck :: Text -> Text -> Value -> Value
mutationAck action entity summary = object ( ["ok" .= True, "action" .= action, "entity_type" .= entity, "summary" .= summary] <> maybe [] (\identifier -> ["id" .= identifier]) (field "id" summary) <> maybe [] (\status -> ["status" .= status]) (field "status" summary) )

statusAck :: Text -> Text -> UUID -> Value
statusAck action entity identifier = object ["ok" .= True, "action" .= action, "entity_type" .= entity, "id" .= identifier]

field :: Text -> Value -> Maybe Value
field key (Object objectValue) = KM.lookup (Key.fromText key) objectValue
field _ _ = Nothing

textField :: Text -> Value -> Maybe Text
textField key value = case field key value of Just (String result) -> Just result; _ -> Nothing

mapField :: Text -> (Value -> Value) -> Value -> [Value]
mapField key shape value = case field key value of Just (Array rows) -> map shape (toList rows); _ -> []

query :: [(String, Maybe String)] -> String
query parameters = case [(key, value) | (key, Just value) <- parameters] of
  [] -> ""
  values -> "?" <> concat (zipWith (\index (key, value) -> (if index == 0 then "" else "&") <> key <> "=" <> value) [0 :: Int ..] values)

observationListPath :: ObservationQuery -> String
observationListPath (ObservationQuery workspace kind subjectValue sha queryValue limit offset) =
  "/api/v1/observations" <> query
    [ ("workspace_id", Just (show workspace))
    , ("subject_kind", T.unpack . subjectKindToText <$> kind)
    , ("subject", queryText <$> subjectValue)
    , ("git_sha", queryText <$> sha)
    , ("query", queryText <$> queryValue)
    , ("limit", show <$> limit)
    , ("offset", show <$> offset)
    ]

queryText :: Text -> String
queryText = T.unpack . TE.decodeUtf8 . urlEncode True . TE.encodeUtf8

uuidPath :: UUID -> String
uuidPath = show

mcpResultWith :: (Value -> Value) -> BL.ByteString -> Value
mcpResultWith shape body = case eitherDecode body of Right value -> mcpJSON (shape value); Left err -> mcpError ("Invalid JSON response: " <> T.pack err)

mcpJSON :: Value -> Value
mcpJSON value = object ["content" .= [object ["type" .= ("text" :: Text), "text" .= TE.decodeUtf8 (BL.toStrict (encode value))]]]

mcpError :: Text -> Value
mcpError message = object ["content" .= [object ["type" .= ("text" :: Text), "text" .= message]], "isError" .= True]

httpError :: Int -> BL.ByteString -> Value
httpError code body =
  mcpError ("[HTTP_" <> T.pack (show code) <> "] " <> structuredMessage <> structuredPayload)
  where
    fallback = T.take 1000 (TE.decodeUtf8With (\_ _ -> Just '?') (BL.toStrict body))
    decodedPayload = eitherDecode body :: Either String Value
    structuredMessage = case decodedPayload of
      Right value -> fromMaybe fallback (textField "message" value)
      Left _ -> fallback
    -- MCP tool results carry text content, so retain the server's complete
    -- structured error payload alongside the HTTP status instead of reducing
    -- a machine-readable REST error to an opaque message.
    structuredPayload = case decodedPayload of
      Right value -> "\n" <> TE.decodeUtf8 (BL.toStrict (encode value))
      Left _ -> ""
