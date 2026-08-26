module HMem.MCP.Tools
  ( toolDefinitions
  , handleToolCall
  , parseToolCall
  , validateToolCall
  , ToolCall(..)
  , compactObservationSummary
  , compactObservationDetail
  , compactObservationList
  , compactSimilarObservations
  , compactSearchResults
  , compactProjectSummary
  , compactTaskSummary
  , mcpResultWith
  ) where

import Control.Exception (SomeAsyncException, SomeException, fromException, throwIO, try)
import Data.Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types (parseEither)
import Data.ByteString.Lazy qualified as BL
import Data.Int (Int32)
import Data.Foldable (toList)
import Data.Maybe (catMaybes, fromMaybe)
import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.UUID (UUID)
import Network.HTTP.Client
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
  , tool "search" "Search observations, projects, and tasks. Observation filters subject_kind, subject, and git_sha are exact provenance filters." (schema
      [ "query" .= prop "string" "Optional full-text query"
      , "entity_types" .= arrayEnum "Entity types (default: observation, project, task)" ["observation", "project", "task"]
      , "subject_kind" .= enumProp "Exact observation subject kind" ["file", "glob"]
      , "subject" .= prop "string" "Exact observation repository-relative subject"
      , "git_sha" .= prop "string" "Exact observation Git SHA"
      , "project_status" .= enumProp "Project status" ["active", "paused", "completed", "archived"]
      , "task_status" .= enumProp "Task status" ["todo", "in_progress", "blocked", "done", "cancelled"]
      , "project_id" .= prop "string" "Filter tasks by project UUID"
      , "limit" .= prop "integer" "Maximum results per entity type"
      , "offset" .= prop "integer" "Result offset per entity type"
      ] [])
  , tool "observation_create" "Create a provenance-bound observation. subject_kind, subject, and git_sha are immutable after creation." (schema
      [ "subject_kind" .= enumProp "Repository subject kind" ["file", "glob"]
      , "subject" .= prop "string" "Canonical repository-relative path or glob"
      , "git_sha" .= prop "string" "Lowercase 40-character Git SHA"
      , "content" .= prop "string" "Observation content"
      ] ["subject_kind", "subject", "git_sha", "content"])
  , tool "observation_get" "Get an observation by ID, including content and immutable provenance." (schema ["observation_id" .= prop "string" "Observation UUID"] ["observation_id"])
  , tool "observation_update" "Replace observation content. Provenance fields cannot be updated." (schema ["observation_id" .= prop "string" "Observation UUID", "content" .= prop "string" "Replacement content"] ["observation_id", "content"])
  , tool "observation_list" "List observations using exact provenance filters and optional text search. When has_more is true, pass next_offset to retrieve the next page." (schema
      [ "subject_kind" .= enumProp "Exact observation subject kind" ["file", "glob"]
      , "subject" .= prop "string" "Exact repository-relative subject"
      , "git_sha" .= prop "string" "Exact observation Git SHA"
      , "query" .= prop "string" "Optional full-text query"
      , "limit" .= prop "integer" "Maximum results (1-200)"
      , "offset" .= prop "integer" "Result offset"
      ] [])
  , tool "observation_delete" "Delete an observation by ID." (schema ["observation_id" .= prop "string" "Observation UUID"] ["observation_id"])
  , tool "observation_set_embedding" "Set the exact 1536-dimension embedding for an observation." (schema
      [ "observation_id" .= prop "string" "Observation UUID"
      , "embedding" .= object ["type" .= ("array" :: Text), "description" .= ("Exactly 1536 finite numeric dimensions" :: Text), "minItems" .= (observationEmbeddingDimensions :: Int), "maxItems" .= (observationEmbeddingDimensions :: Int), "items" .= object ["type" .= ("number" :: Text)]]
      ] ["observation_id", "embedding"])
  , tool "observation_similar" "Find semantically similar observations. To continue, add returned_count to offset and repeat until returned_count is less than limit or zero." (schema
      [ "subject_kind" .= enumProp "Exact observation subject kind" ["file", "glob"]
      , "subject" .= prop "string" "Exact repository-relative subject"
      , "git_sha" .= prop "string" "Exact observation Git SHA"
      , "embedding" .= object ["type" .= ("array" :: Text), "description" .= ("Exactly 1536 finite numeric dimensions" :: Text), "minItems" .= (observationEmbeddingDimensions :: Int), "maxItems" .= (observationEmbeddingDimensions :: Int), "items" .= object ["type" .= ("number" :: Text)]]
      , "min_similarity" .= prop "number" "Minimum similarity from 0 through 1"
      , "limit" .= prop "integer" "Maximum results (1-200)"
      , "offset" .= prop "integer" "Result offset"
      ] ["embedding"])
  , tool "project_create" "Create a project in the active workspace." (schema ["name" .= prop "string" "Project name", "description" .= prop "string" "Optional description", "parent_id" .= prop "string" "Parent project UUID", "priority" .= prop "integer" "Priority 1 through 10"] ["name"])
  , tool "project_update" "Update a project." (schema ["project_id" .= prop "string" "Project UUID", "name" .= prop "string" "Name", "description" .= prop "string" "Description or null", "parent_id" .= prop "string" "Parent UUID or null", "status" .= enumProp "Status" ["active", "paused", "completed", "archived"], "priority" .= prop "integer" "Priority"] ["project_id"])
  , tool "project_detail" "Get compact project details." (schema ["project_id" .= prop "string" "Project UUID"] ["project_id"])
  , tool "project_overview" "Get a compact project overview with tasks and subprojects." (schema ["project_id" .= prop "string" "Project UUID"] ["project_id"])
  , tool "project_next_tasks" "Get actionable tasks for a project subtree." (schema ["project_id" .= prop "string" "Project UUID", "limit" .= prop "integer" "Maximum candidates", "include_blocked" .= prop "boolean" "Include blocked candidates"] ["project_id"])
  , tool "project_spec" "Create a project and initial tasks in one call." (schema ["name" .= prop "string" "Project name", "description" .= prop "string" "Optional description", "priority" .= prop "integer" "Project priority", "tasks" .= object ["type" .= ("array" :: Text), "items" .= object ["type" .= ("object" :: Text)]]] ["name", "tasks"])
  , tool "project_archive" "Archive a project by changing only its status." (schema ["project_id" .= prop "string" "Project UUID"] ["project_id"])
  , tool "task_create" "Create a task in the active workspace." (schema ["project_id" .= prop "string" "Optional project UUID", "title" .= prop "string" "Task title", "description" .= prop "string" "Optional description", "parent_id" .= prop "string" "Optional parent task UUID", "priority" .= prop "integer" "Priority", "due_at" .= prop "string" "ISO-8601 due time"] ["title"])
  , tool "task_update" "Update a task." (schema ["task_id" .= prop "string" "Task UUID", "title" .= prop "string" "Title", "description" .= prop "string" "Description or null", "project_id" .= prop "string" "Project UUID or null", "parent_id" .= prop "string" "Parent UUID or null", "status" .= enumProp "Status" ["todo", "in_progress", "blocked", "done", "cancelled"], "priority" .= prop "integer" "Priority", "due_at" .= prop "string" "ISO-8601 due time or null"] ["task_id"])
  , tool "task_detail" "Get compact task details." (schema ["task_id" .= prop "string" "Task UUID"] ["task_id"])
  , tool "task_overview" "Get a compact task overview and dependency summaries." (schema ["task_id" .= prop "string" "Task UUID"] ["task_id"])
  , tool "task_start" "Set a task status to in_progress." (schema ["task_id" .= prop "string" "Task UUID"] ["task_id"])
  , tool "task_finish" "Set a task status to done, blocked, or cancelled. This does not create an observation." (schema ["task_id" .= prop "string" "Task UUID", "status" .= enumProp "Final status" ["done", "blocked", "cancelled"]] ["task_id", "status"])
  ]
  where
    tool name description inputSchema = object ["name" .= (name :: Text), "description" .= (description :: Text), "inputSchema" .= inputSchema]
    schema properties required = object ["type" .= ("object" :: Text), "properties" .= object properties, "required" .= (required :: [Text])]
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
  | ObservationDelete UUID
  | ObservationSetEmbedding UUID ObservationEmbedding
  | ObservationSimilar SimilarObservationQuery
  | WorkspaceList (Maybe Int)
  | WorkspaceRegister CreateWorkspace
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
  | TaskDetail UUID
  | TaskOverviewCall UUID
  | TaskStart UUID
  | TaskFinish UUID TaskStatus
  deriving (Show, Eq)

data SpecTask = SpecTask { specTitle :: Text, specDescription :: Maybe Text, specPriority :: Maybe Int } deriving (Show, Eq)

parseToolCall :: Text -> Value -> Either String ToolCall
parseToolCall name args = case name of
  "observation_create" -> ObservationCreate <$> parse args
  "observation_get" -> ObservationGet <$> required "observation_id"
  "observation_update" -> ObservationUpdate <$> required "observation_id" <*> parseUpdateObservation args
  "observation_list" -> ObservationList <$> parse args
  "observation_delete" -> ObservationDelete <$> required "observation_id"
  "observation_set_embedding" -> ObservationSetEmbedding <$> required "observation_id" <*> (ObservationEmbedding <$> required "embedding")
  "observation_similar" -> ObservationSimilar <$> parse args
  "workspace_list" -> WorkspaceList <$> optional "limit"
  "workspace_register" -> WorkspaceRegister <$> parse args
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
  "task_detail" -> TaskDetail <$> required "task_id"
  "task_overview" -> TaskOverviewCall <$> required "task_id"
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

parseTasks :: Value -> Either String [SpecTask]
parseTasks = parseEither $ withObject "project_spec" $ \o -> do
  values <- o .: "tasks"
  mapM (withObject "task" $ \t -> SpecTask <$> t .: "title" <*> t .:? "description" <*> t .:? "priority") values

validateToolCall :: ToolCall -> Either String ToolCall
validateToolCall call = case call of
  ObservationCreate input -> checked (validateCreateObservationInput input) call
  ObservationUpdate _ input -> checked (validateUpdateObservationInput input) call
  ObservationList input -> checked (validateObservationQuery input) call
  ObservationSetEmbedding _ (ObservationEmbedding values) -> checked (validateEmbedding values) call
  ObservationSimilar input -> checked (validateSimilarObservationQuery input) call
  WorkspaceRegister input -> checked (validateCreateWorkspaceInput input) call
  UnifiedSearch input -> checked (validateUnifiedSearchQuery input) call
  ProjectCreate input -> checked (validateCreateProjectInput input) call
  ProjectUpdate _ input -> checked (validateUpdateProjectInput input) call
  TaskCreate input -> checked (validateCreateTaskInput input) call
  TaskUpdate _ input -> checked (validateUpdateTaskInput input) call
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
  ObservationDelete oid -> noContentRequest manager base apiKey "DELETE" ("/api/v1/observations/" <> uuidPath oid) Nothing (statusAck "deleted" "observation" oid)
  ObservationSetEmbedding oid embeddingValue -> noContentRequest manager base apiKey "PUT" ("/api/v1/observations/" <> uuidPath oid <> "/embedding") (Just (encode embeddingValue)) (statusAck "embedding_set" "observation" oid)
  ObservationSimilar input@(SimilarObservationQuery _ _ _ _ _ _ limit offset) -> request manager base apiKey "POST" "/api/v1/observations/similar" (Just (encode input)) (compactSimilarObservations (fromMaybe 50 limit) (fromMaybe 0 offset))
  WorkspaceList limit -> request manager base apiKey "GET" ("/api/v1/workspaces" <> query [("limit", show <$> limit)]) Nothing compactWorkspaceList
  WorkspaceRegister input -> request manager base apiKey "POST" "/api/v1/workspaces" (Just (encode input)) (mutationAck "created" "workspace" . compactWorkspaceSummary)
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
  TaskDetail tid -> request manager base apiKey "GET" ("/api/v1/tasks/" <> uuidPath tid) Nothing compactTaskSummary
  TaskOverviewCall tid -> request manager base apiKey "GET" ("/api/v1/tasks/" <> uuidPath tid <> "/overview") Nothing compactTaskOverview
  TaskStart tid -> request manager base apiKey "PUT" ("/api/v1/tasks/" <> uuidPath tid) (Just (encode (object ["status" .= ("in_progress" :: Text)]))) (mutationAck "started" "task" . compactTaskSummary)
  TaskFinish tid status -> request manager base apiKey "PUT" ("/api/v1/tasks/" <> uuidPath tid) (Just (encode (object ["status" .= status]))) (mutationAck "finished" "task" . compactTaskSummary)

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
    let auth = maybe [] (\token -> [("Authorization", TE.encodeUtf8 ("Bearer " <> token))]) apiKey
        requestValue = initial { method = fromString method, requestHeaders = ("Content-Type", "application/json") : auth, requestBody = maybe (RequestBodyBS mempty) RequestBodyLBS body }
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
    let auth = maybe [] (\token -> [("Authorization", TE.encodeUtf8 ("Bearer " <> token))]) apiKey
        requestValue = initial { method = fromString method, requestHeaders = ("Content-Type", "application/json") : auth, requestBody = maybe (RequestBodyBS mempty) RequestBodyLBS body }
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

compactObservationSummary :: Value -> Value
compactObservationSummary value = object (catMaybes [copy "id", copy "subject_kind", copy "subject", copy "git_sha", preview])
  where
    copy key = (Key.fromText key .=) <$> field key value
    preview = case field "content_preview" value of
      Just value' -> Just ("content_preview" .= value')
      Nothing -> case field "content" value of
        Just (String contentValue) -> Just ("content_preview" .= T.take 500 contentValue)
        _ -> Nothing

compactObservationDetail :: Value -> Value
compactObservationDetail value = object (catMaybes [copy "id", copy "subject_kind", copy "subject", copy "git_sha", copy "content"])
  where copy key = (Key.fromText key .=) <$> field key value

compactObservationList :: Int -> Value -> Value
compactObservationList offsetValue value = object
  ( [ "items" .= mapField "items" compactObservationSummary value
    , "has_more" .= hasMoreValue
    ] <> ["next_offset" .= (offsetValue + length (mapField "items" id value)) | hasMoreValue] )
  where
    hasMoreValue = field "has_more" value == Just (Bool True)

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
httpError code body = mcpError ("[HTTP_" <> T.pack (show code) <> "] " <> T.take 1000 (TE.decodeUtf8With (\_ _ -> Just '?') (BL.toStrict body)))
