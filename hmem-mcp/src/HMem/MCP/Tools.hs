module HMem.MCP.Tools
  ( toolDefinitions
  , handleToolCall
  -- * Testing
  , parseToolCall
  , validateToolCall
  , mcpResultWith
  , compactMemorySummary
  , compactMemoryDetail
  , compactProjectSummary
  , compactTaskSummary
  , compactSearchResults
  , compactProjectOverview
  , compactProjectOverviewWithDescriptions
  , compactTaskOverview
  , compactTaskOverviewWithDescription
  , compactContextInfo
  , compactTaskStartSuccess
  , compactTaskMutationAck
  , compactDependencyMutationAck
  , compactMemoryMutationAckWithTargets
  , compactMemoryMutationAckWithTags
  , compactTaskFinishAckWithNotes
  , compactProjectArchiveAck
  , compactMemoryLinksList
  , compactNextTaskCandidateSummary
  , addChangedFields
  , sanitizeServerResponse
  , mcpHttpError
  , rawHttpErrorText
  , mcpErrorCodeFromRaw
  , taskStartUpdateError
  , taskStartDependencyBlockError
  , taskStartOpenDependencyBlockers
  , taskStartParentGateError
  , taskStartProjectIdFromSelfOrAncestors
  , MemoryTarget(..)
  , isTopLevelTaskTarget
  , taskFinishNotesTarget
  , firstRawAuthError
  , bearerAuthHeaders
  , ToolCall(..)
  ) where

import Control.Exception (SomeException, try)
import Control.Applicative ((<|>))
import Data.Foldable (toList)
import Data.Aeson
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types (Parser, Pair, parseEither)
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as BL
import Data.Int (Int32)
import Data.List (intercalate)
import Data.Maybe (catMaybes, fromMaybe, listToMaybe, mapMaybe)
import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Network.HTTP.Client
import Network.HTTP.Types.Header (RequestHeaders)
import Network.HTTP.Types.Status (statusCode)
import Network.HTTP.Types.URI (urlEncode)

import HMem.Types

------------------------------------------------------------------------
-- Tool definition schemas (sent to LLM via tools/list)
------------------------------------------------------------------------

toolDefinitions :: [Value]
toolDefinitions = slimToolDefinitions

slimToolDefinitions :: [Value]
slimToolDefinitions =
    [ mkTool "set_workspace" "Set the active workspace context. Once set, scoped tool calls can omit workspace_id. Call this once at the start of a session. Pass null or omit workspace_id to clear the context." $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "workspace_id" .= prop "string" "UUID of the workspace to set as active context (omit or null to clear)"
          ]
      ]

    , mkTool "get_workspace" "Get the currently active workspace context UUID, if any." $ object
      [ "type" .= t "object"
      , "properties" .= object []
      ]

    , mkTool "workspace_list" "List registered workspaces to find IDs before setting context." $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "limit" .= prop "integer" "Max results (default 50)" ]
      ]

    , mkTool "workspace_register" "Register a new workspace — the top-level container for memories, projects, and tasks." $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "name" .= propMaxLength "string" "Workspace name" maxNameBytes
          , "workspace_type" .= propEnum "string" "Workspace type" ["repository", "planning", "personal", "organization"]
          ]
      , "required" .= [t "name"]
      ]

    , mkTool "search" "Unified search and browsing across memories, projects, and tasks in the active workspace. Query is optional so filtered browsing can replace list tools. Project and task results include linked memory summaries." $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "query" .= prop "string" "Full-text search query (optional for filtered browsing)"
          , "entity_types" .= object ["type" .= t "array", "items" .= propEnum "string" "Entity types to search" ["memory", "project", "task"],
                                       "description" .= t "Which entity types to search (default: all three)"]
          , "memory_type" .= propEnum "string" "Filter memories by type" ["short_term", "long_term"]
          , "tags" .= object ["type" .= t "array", "items" .= object ["type" .= t "string"],
                               "description" .= t "Filter memories by tags (any-match)"]
          , "project_status" .= propEnum "string" "Filter projects by status" ["active", "paused", "completed", "archived"]
          , "task_status" .= propEnum "string" "Filter tasks by status" ["todo", "in_progress", "blocked", "done", "cancelled"]
          , "project_id" .= prop "string" "Filter tasks by project UUID"
          , "limit" .= prop "integer" "Max results per entity type (default 10)"
          ]
      , "required" .= ([] :: [Text])
      ]

    , mkTool "memory_create" "Create a memory in the active workspace. Requires explicit memory_type and at least one explicit target: project_id and/or a top-level task_id. Subtask task IDs are rejected." $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "project_id" .= prop "string" "UUID of a project to link at creation time. Provide at least one of project_id or task_id."
          , "task_id" .= prop "string" "UUID of a top-level task to link at creation time. Subtask IDs are rejected. Provide at least one of project_id or task_id."
          , "content" .= propMaxLength "string" "The memory content" maxMemoryContentBytes
          , "summary" .= propMaxLength "string" "Optional short summary" maxMemorySummaryBytes
          , "memory_type" .= propEnum "string" "short_term or long_term" ["short_term", "long_term"]
          , "importance" .= prop "integer" "1 (lowest) to 10 (highest), default 5"
          , "tags" .= object ["type" .= t "array", "items" .= object ["type" .= t "string"],
                               "description" .= t "Tags for categorization"]
          ]
      , "required" .= [t "content", t "memory_type"]
      ]

    , mkTool "memory_get" "Get a memory by ID with full detail." $ object
      [ "type" .= t "object"
      , "properties" .= object [ "memory_id" .= prop "string" "UUID of the memory" ]
      , "required" .= [t "memory_id"]
      ]

    , mkTool "memory_update" "Enrich or correct an existing memory, including replacing tags. Use null to clear summary." $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "memory_id" .= prop "string" "UUID of the memory to update"
          , "content" .= propMaxLength "string" "New content" maxMemoryContentBytes
          , "summary" .= propMaxLength "string" "New summary (null to clear)" maxMemorySummaryBytes
          , "memory_type" .= propEnum "string" "New type" ["short_term", "long_term"]
          , "importance" .= prop "integer" "New importance, 1 (lowest) to 10 (highest)"
          , "pinned" .= prop "boolean" "Pin or unpin this memory"
          , "tags" .= object ["type" .= t "array", "items" .= object ["type" .= t "string"],
                               "description" .= t "Replacement tags for the memory"]
          ]
      , "required" .= [t "memory_id"]
      ]

    , mkTool "memory_link" "Manage typed links between memories. Actions: create, remove, or list. Relation types: related, supersedes, contradicts, elaborates, inspires, depends_on, derived_from, alternative_to." $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "action" .= propEnum "string" "Operation to perform" ["create", "remove", "list"]
          , "source_id" .= prop "string" "Source memory UUID (required for create, remove)"
          , "target_id" .= prop "string" "Target memory UUID (required for create, remove)"
          , "relation_type" .= propEnum "string" "Relation type (required for create, remove)" ["related", "supersedes", "contradicts", "elaborates", "inspires", "depends_on", "derived_from", "alternative_to"]
          , "memory_id" .= prop "string" "Memory UUID (required for list)"
          ]
      , "required" .= [t "action"]
      ]

    , mkTool "link_memory" "Attach or detach a memory to/from a project or top-level task. Subtask task IDs are not valid link targets." $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "entity_type" .= propEnum "string" "Type of entity to link memories to" ["project", "task"]
          , "entity_id" .= prop "string" "UUID of the entity"
          , "action" .= propEnum "string" "Whether to link or unlink" ["link", "unlink"]
          , "memory_id" .= prop "string" "UUID of the memory"
          ]
      , "required" .= [t "entity_type", t "entity_id", t "action", t "memory_id"]
      ]

    , mkTool "project_create" "Create a top-level or child project in the active workspace. Set parent_id to create a subproject under an existing project." $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "name" .= propMaxLength "string" "Project name" maxNameBytes
          , "description" .= propMaxLength "string" "Project description" maxDescriptionBytes
          , "parent_id" .= prop "string" "Parent project UUID for sub-projects"
          , "priority" .= prop "integer" "1 (lowest) to 10 (highest), default 5"
          ]
      , "required" .= [t "name"]
      ]

    , mkTool "project_update" "Update a project. Use parent_id=null to move it back to the top level. Completion/archive may be gated by open descendant projects or tasks." $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "project_id" .= prop "string" "UUID of the project"
          , "name" .= propMaxLength "string" "New name" maxNameBytes
          , "description" .= propMaxLength "string" "New description (null to clear)" maxDescriptionBytes
          , "parent_id" .= prop "string" "New parent project UUID (null to clear)"
          , "status" .= propEnum "string" "New status" ["active", "paused", "completed", "archived"]
          , "priority" .= prop "integer" "New priority, 1 (lowest) to 10 (highest)"
          ]
      , "required" .= [t "project_id"]
      ]

    , mkTool "project_overview" "Get a compact project overview with tasks, subprojects, linked memories, and readiness_rollup. Set include_descriptions=true only when project/task/subproject descriptions are needed." $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "project_id" .= prop "string" "UUID of the project"
          , "include_descriptions" .= prop "boolean" "Include project, task, and subproject descriptions (default false)"
          ]
      , "required" .= [t "project_id"]
      ]

    , mkTool "project_next_tasks" "Get the next actionable tasks for a project subtree, sorted by priority then creation time. By default returns only ready tasks; set include_blocked=true to include dependency-blocked/manual-blocked diagnostics. Open subtasks gate parent completion but are not dependency blockers." $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "project_id" .= prop "string" "UUID of the project"
          , "limit" .= prop "integer" "Maximum candidates to return (default 5, clamped 1..200)"
          , "include_blocked" .= prop "boolean" "Include blocked tasks with dependency/manual-blocking rationale (default false)"
          ]
      , "required" .= [t "project_id"]
      ]

    , mkTool "project_spec" "Create a project and its initial tasks in one call in the active workspace. Tasks are created under the new project in order." $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "name" .= propMaxLength "string" "Project name" maxNameBytes
          , "description" .= propMaxLength "string" "Project description" maxDescriptionBytes
          , "priority" .= prop "integer" "Project priority, 1 (lowest) to 10 (highest), default 5"
          , "tasks" .= object
              [ "type" .= t "array"
              , "description" .= t "Tasks to create under the project"
              , "minItems" .= (1 :: Int)
              , "maxItems" .= (50 :: Int)
              , "items" .= object
                  [ "type" .= t "object"
                  , "properties" .= object
                      [ "title" .= propMaxLength "string" "Task title" maxNameBytes
                      , "description" .= propMaxLength "string" "Task description" maxDescriptionBytes
                      , "priority" .= prop "integer" "Task priority, 1 (lowest) to 10 (highest), default 5"
                      ]
                  , "required" .= [t "title"]
                  ]
              ]
          ]
      , "required" .= [t "name", t "tasks"]
      ]

    , mkTool "project_archive" "Archive a completed project, optionally recording a summary as a linked long_term memory." $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "project_id" .= prop "string" "UUID of the project to archive"
          , "summary" .= prop "string" "Optional project summary to save as a linked long_term memory"
          ]
      , "required" .= [t "project_id"]
      ]

    , mkTool "task_create" "Create a workspace- or project-scoped task in the active workspace. Use parent_id only for direct subtasks of top-level tasks; subtasks cannot have children." $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "project_id" .= prop "string" "UUID of the project (optional)"
          , "title" .= propMaxLength "string" "Task title" maxNameBytes
          , "description" .= propMaxLength "string" "Task description" maxDescriptionBytes
          , "parent_id" .= prop "string" "Parent task UUID for sub-tasks"
          , "priority" .= prop "integer" "1 (lowest) to 10 (highest), default 5"
          , "due_at" .= prop "string" "ISO 8601 due date"
          ]
      , "required" .= [t "title"]
      ]

    , mkTool "task_update" "Update a task. Responses may include dependency_effects when dependency auto-blocking changes. Use project_id and/or parent_id to reorganize; null clears those fields." $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "task_id" .= prop "string" "UUID of the task"
          , "title" .= propMaxLength "string" "New title" maxNameBytes
          , "description" .= propMaxLength "string" "New description (null to clear)" maxDescriptionBytes
          , "project_id" .= prop "string" "New project UUID (null to clear)"
          , "parent_id" .= prop "string" "New parent task UUID (null to clear)"
          , "status" .= propEnum "string" "New status" ["todo", "in_progress", "blocked", "done", "cancelled"]
          , "priority" .= prop "integer" "New priority, 1 (lowest) to 10 (highest)"
          , "due_at" .= prop "string" "ISO 8601 due date (null to clear)"
          ]
      , "required" .= [t "task_id"]
      ]

    , mkTool "task_overview" "Get a compact task overview with dependency summaries, connected memories, and readiness_rollup. Set include_description=true only when the task description is needed." $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "task_id" .= prop "string" "UUID of the task"
          , "include_description" .= prop "boolean" "Include the task description (default false)"
          ]
      , "required" .= [t "task_id"]
      ]

    , mkTool "context_get" "Get relevant memories for a task, automatically collecting from the task itself, all ancestor projects, and the workspace." $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "task_id" .= prop "string" "UUID of the task"
          , "detail_level" .= propEnum "string" "How many memories per scope: light=2, medium=5 (default), heavy=10" ["light", "medium", "heavy"]
          ]
      , "required" .= [t "task_id"]
      ]

    , mkTool "task_dependency" "Add or remove an ordering dependency between tasks. Dependency changes may automatically move tasks to or from blocked status when open dependencies exist." $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "action" .= propEnum "string" "Whether to add or remove the dependency" ["add", "remove"]
          , "task_id" .= prop "string" "UUID of the task"
          , "depends_on_id" .= prop "string" "UUID of the task it depends on"
          ]
      , "required" .= [t "action", t "task_id", t "depends_on_id"]
      ]

    , mkTool "task_start" "Begin work on a task: preflights dependency blockers and subtask parent gates, then sets status to in_progress and loads relevant context. When blocked, returns actionable blockers and ready alternatives without changing status." $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "task_id" .= prop "string" "UUID of the task to start"
          , "detail_level" .= propEnum "string" "How many memories per scope: light=2, medium=5 (default), heavy=10" ["light", "medium", "heavy"]
          ]
      , "required" .= [t "task_id"]
      ]

    , mkTool "task_finish" "Finish working on a task: optionally records notes as a linked long_term memory, then updates task status." $ object
      [ "type" .= t "object"
      , "properties" .= object
          [ "task_id" .= prop "string" "UUID of the task"
          , "status" .= propEnum "string" "New task status" ["done", "blocked", "cancelled"]
          , "notes" .= prop "string" "Optional work notes to save as a linked memory"
          ]
      , "required" .= [t "task_id", t "status"]
      ]
    ]

------------------------------------------------------------------------
-- Typed tool calls
------------------------------------------------------------------------

data ToolCall
  = MemoryCreate   CreateMemory
  | MemoryGet      UUID
  | MemoryUpdate   UUID UpdateMemory (Maybe [Text])
  | LinkMemories   UUID CreateMemoryLink   -- source_id, link body
  | MemoryLinksList UUID
  | MemoryUnlink    UUID UUID RelationType -- source_id, target_id, relation_type
  | ProjectCreate  CreateProject
  | ProjectUpdate  UUID UpdateProject
  | ProjectLinkMem UUID UUID               -- project_id, memory_id
  | ProjectUnlinkMem UUID UUID             -- project_id, memory_id
  | TaskCreate     CreateTask
  | TaskOverviewCall UUID Bool
  | ContextGetCall UUID ContextDetailLevel
  | TaskUpdate     UUID UpdateTask
  | TaskLinkMem    UUID UUID               -- task_id, memory_id
  | TaskUnlinkMem  UUID UUID               -- task_id, memory_id
  | TaskDepAdd     UUID UUID               -- task_id, depends_on_id
  | TaskDepRemove  UUID UUID               -- task_id, depends_on_id
  | WorkspaceList (Maybe Int) (Maybe Int)
  | WorkspaceReg   CreateWorkspace
  | ProjectOverviewCall UUID Bool
  | ProjectNextTasksCall UUID (Maybe Int) Bool
  -- Workflow composite tools
  | TaskStartCall UUID ContextDetailLevel
  | TaskFinishCall UUID TaskStatus (Maybe Text)   -- task_id, status, notes
  | ProjectSpecCall UUID Text (Maybe Text) (Maybe Int32) [SpecTask] -- ws_id, name, desc, priority, tasks
  | ProjectArchiveCall UUID (Maybe Text)                         -- project_id, summary
  | UnifiedSearch UnifiedSearchQuery
  deriving (Show, Eq)

data MemoryTarget
  = MemoryTargetProject UUID
  | MemoryTargetTask UUID
  deriving (Show, Eq)

-- | A task stub for project_spec — just the fields needed to create a task.
data SpecTask = SpecTask
  { stTitle       :: Text
  , stDescription :: Maybe Text
  , stPriority    :: Maybe Int32
  } deriving (Show, Eq)

-- | Parse raw JSON-RPC params (containing "name" and "arguments") into
-- a typed ToolCall, validating all fields against HMem.Types.
parseToolCall :: Text -> Value -> Either String ToolCall
parseToolCall name args = case name of
    "memory_create"            -> MemoryCreate <$> parse args
    "memory_get"               -> MemoryGet <$> need "memory_id"
    "memory_update"            -> do
        tags <- opt "tags"
        MemoryUpdate <$> need "memory_id" <*> parse args <*> pure tags
    "memory_link"              -> do
        action <- need "action" :: Either String Text
        case action of
            "create" -> LinkMemories <$> need "source_id" <*> parse args
            "remove" -> MemoryUnlink <$> need "source_id" <*> need "target_id" <*> need "relation_type"
            "list"   -> MemoryLinksList <$> need "memory_id"
            _        -> Left "memory_link: action must be 'create', 'remove', or 'list'"
    "project_create"           -> ProjectCreate <$> parse args
    "project_update"           -> ProjectUpdate <$> need "project_id" <*> parse args
    "link_memory"              -> do
        entityType <- need "entity_type" :: Either String Text
        eid <- need "entity_id"
        action <- need "action" :: Either String Text
        mid <- need "memory_id" :: Either String UUID
        case (entityType, action) of
            ("project",  "link")   -> Right $ ProjectLinkMem eid mid
            ("project",  "unlink") -> Right $ ProjectUnlinkMem eid mid
            ("task",     "link")   -> Right $ TaskLinkMem eid mid
            ("task",     "unlink") -> Right $ TaskUnlinkMem eid mid
            _ -> Left $ "link_memory: invalid entity_type/action: " <> T.unpack entityType <> "/" <> T.unpack action
    "task_create"              -> TaskCreate <$> parse args
    "task_overview"            -> TaskOverviewCall <$> need "task_id" <*> (fromMaybe False <$> opt "include_description")
    "context_get"              -> ContextGetCall <$> need "task_id" <*> (maybe ContextMedium id <$> opt "detail_level")
    "task_update"              -> TaskUpdate <$> need "task_id" <*> parse args
    "task_dependency"          -> do
        action <- need "action" :: Either String Text
        case action of
            "add"    -> TaskDepAdd <$> need "task_id" <*> need "depends_on_id"
            "remove" -> TaskDepRemove <$> need "task_id" <*> need "depends_on_id"
            _        -> Left "task_dependency: action must be 'add' or 'remove'"
    "workspace_list"           -> WorkspaceList <$> opt "limit" <*> pure Nothing
    "workspace_register"       -> WorkspaceReg <$> parse args
    "project_overview"          -> ProjectOverviewCall <$> need "project_id" <*> (fromMaybe False <$> opt "include_descriptions")
    "project_next_tasks"        -> ProjectNextTasksCall <$> need "project_id" <*> opt "limit" <*> (fromMaybe False <$> opt "include_blocked")
    -- Workflow composite tools
    "task_start"                -> TaskStartCall <$> need "task_id" <*> (maybe ContextMedium id <$> opt "detail_level")
    "task_finish"               -> TaskFinishCall <$> need "task_id" <*> need "status" <*> opt "notes"
    "project_spec"              -> do
        wsId <- need "workspace_id"
        pName <- need "name"
        pDesc <- opt "description"
        pPri  <- opt "priority"
        tasks <- parseSpecTasks args
        Right $ ProjectSpecCall wsId pName pDesc pPri tasks
    "project_archive"           -> ProjectArchiveCall <$> need "project_id" <*> opt "summary"
    "search"                    -> UnifiedSearch <$> parse args
    _                           -> Left $ "Unknown tool: " <> T.unpack name
  where
    parse :: FromJSON a => Value -> Either String a
    parse = parseEither parseJSON

    need :: FromJSON a => Key -> Either String a
    need k = parseEither (withObject "args" (.: k)) args

    opt :: FromJSON a => Key -> Either String (Maybe a)
    opt k = parseEither (withObject "args" (.:? k)) args

-- | Parse the tasks array from project_spec arguments.
parseSpecTasks :: Value -> Either String [SpecTask]
parseSpecTasks = parseEither $ withObject "args" $ \o -> do
  items <- o .: "tasks" :: Parser [Value]
  mapM parseTask items
  where
    parseTask = withObject "SpecTask" $ \o -> do
      title <- o .: "title"
      desc  <- o .:? "description"
      pri   <- o .:? "priority"
      pure SpecTask { stTitle = title, stDescription = desc, stPriority = pri }

------------------------------------------------------------------------
-- Tool call dispatch
------------------------------------------------------------------------

handleToolCall :: Manager -> String -> Maybe Text -> Value -> IO Value
handleToolCall mgr serverUrl mApiKey params =
    case parseParams params of
        Left err -> pure $ mcpError (T.pack err)
        Right (name, args) ->
            case parseToolCall name args of
                Left err -> pure $ mcpError (T.pack err)
                Right call ->
                    either
                        (pure . mcpError . T.pack)
                        (executeToolCall mgr serverUrl mApiKey)
                        (validateToolCall call)

parseParams :: Value -> Either String (Text, Value)
parseParams = parseEither $ withObject "params" $ \o -> do
  name <- o .: "name"
  args <- o .:? "arguments" .!= object []
  pure (name, args)

------------------------------------------------------------------------
-- Input validation / clamping
------------------------------------------------------------------------

-- | Validate and clamp numeric fields on a parsed ToolCall so that
-- out-of-range values from the LLM are caught before hitting the
-- server.  Returns Left with a human-readable error on hard failures,
-- or Right with clamped values on soft corrections.
validateToolCall :: ToolCall -> Either String ToolCall
validateToolCall = \case
    MemoryCreate cm
        | not (validFtsLanguage cm.ftsLanguage) -> Left $ "Invalid fts_language: " <> show cm.ftsLanguage
        | otherwise ->
                let cm' = clampCreateMemory cm
                in MemoryCreate cm' <$ firstValidationError (validateCreateMemoryInput cm')
    MemoryUpdate mid um tags ->
        let um' = clampUpdateMemory um
        in MemoryUpdate mid um' tags <$ firstValidationError (validateUpdateMemoryInput um')
    ProjectCreate cp -> ProjectCreate cp <$ firstValidationError (validateCreateProjectInput cp)
    ProjectUpdate pid up -> ProjectUpdate pid up <$ firstValidationError (validateUpdateProjectInput up)
    TaskCreate ct -> TaskCreate ct <$ firstValidationError (validateCreateTaskInput ct)
    TaskOverviewCall tid includeDescription -> Right $ TaskOverviewCall tid includeDescription
    ContextGetCall tid level -> Right $ ContextGetCall tid level
    TaskUpdate tid ut -> TaskUpdate tid ut <$ firstValidationError (validateUpdateTaskInput ut)
    WorkspaceReg cw -> WorkspaceReg cw <$ firstValidationError (validateCreateWorkspaceInput cw)
    WorkspaceList ml mo -> Right $ WorkspaceList (clampMaybe 1 200 <$> ml) (clampMaybe 0 10000 <$> mo)
    ProjectOverviewCall pid includeDescriptions -> Right $ ProjectOverviewCall pid includeDescriptions
    ProjectNextTasksCall pid ml includeBlocked -> Right $ ProjectNextTasksCall pid (clampMaybe 1 200 <$> ml) includeBlocked
    -- Workflow composite tools — lightweight validation
    TaskStartCall tid level -> Right $ TaskStartCall tid level
    TaskFinishCall tid status mNotes -> Right $ TaskFinishCall tid status mNotes
    ProjectSpecCall wsId pName pDesc pPri tasks
        | null tasks -> Left "project_spec: tasks must not be empty"
        | length tasks > 50 -> Left "project_spec: tasks must contain at most 50 items"
        | T.null (T.strip pName) -> Left "project_spec: name must not be blank"
        | any (T.null . T.strip . (.stTitle)) tasks -> Left "project_spec: all tasks must have non-blank titles"
        | otherwise -> Right $ ProjectSpecCall wsId pName pDesc (clampMaybe 1 10 <$> pPri)
            [st { stPriority = clampMaybe 1 10 <$> st.stPriority } | st <- tasks]
    ProjectArchiveCall pid mSummary -> Right $ ProjectArchiveCall pid mSummary
    UnifiedSearch usq
        | not (validFtsLanguage usq.searchLanguage) -> Left $ "Invalid search_language: " <> show usq.searchLanguage
        | otherwise ->
            let usq' = usq
                  { limit = clampMaybe 1 200 <$> usq.limit
                  , offset = clampMaybe 0 10000 <$> usq.offset
                  , minImportance = clampMaybe 1 10 <$> usq.minImportance
                  , taskPriority = clampMaybe 1 10 <$> usq.taskPriority
                  }
            in UnifiedSearch usq' <$ firstValidationError (validateUnifiedSearchQuery usq')
    other -> Right other

clamp :: Ord a => a -> a -> a -> a
clamp lo hi = Prelude.max lo . Prelude.min hi

clampMaybe :: Ord a => a -> a -> a -> a
clampMaybe lo hi = clamp lo hi

firstValidationError :: [Text] -> Either String ()
firstValidationError [] = Right ()
firstValidationError errs = Left (T.unpack (T.intercalate "; " errs))

-- | Clamp numeric fields on a CreateMemory without record update syntax
-- (avoids -Wambiguous-fields with DuplicateRecordFields).
clampCreateMemory :: CreateMemory -> CreateMemory
clampCreateMemory cm = CreateMemory
  { workspaceId = cm.workspaceId
  , projectId   = cm.projectId
  , taskId      = cm.taskId
  , content     = cm.content
  , summary     = cm.summary
  , memoryType  = cm.memoryType
  , importance  = clampMaybe 1 10 <$> cm.importance
  , metadata    = cm.metadata
  , expiresAt   = cm.expiresAt
  , source      = cm.source
  , confidence  = clampMaybe 0.0 1.0 <$> cm.confidence
  , pinned      = cm.pinned
  , tags        = cm.tags
  , ftsLanguage = cm.ftsLanguage
  }

-- | Clamp numeric fields on an UpdateMemory without record update syntax.
clampUpdateMemory :: UpdateMemory -> UpdateMemory
clampUpdateMemory um = UpdateMemory
  { content    = um.content
  , summary    = um.summary
  , memoryType = um.memoryType
  , importance = clampMaybe 1 10 <$> um.importance
  , metadata   = um.metadata
  , expiresAt  = um.expiresAt
  , source     = um.source
  , confidence = clampMaybe 0.0 1.0 <$> um.confidence
  , pinned     = um.pinned
  }

-- | Execute a typed tool call against the hmem-server HTTP API.
executeToolCall :: Manager -> String -> Maybe Text -> ToolCall -> IO Value
executeToolCall mgr base mApiKey = \case
    MemoryCreate cm -> do
      mTargetErr <- ensureTopLevelMemoryTaskTargets mgr base mApiKey [cm]
      maybe (postJSONWith (compactMemoryMutationAckWithTargets "created" cm.projectId cm.taskId) mgr base mApiKey "/api/v1/memories" cm) pure mTargetErr
    MemoryGet mid       -> getJSONWith compactMemoryDetail mgr base mApiKey ("/api/v1/memories/" <> uuidPath mid)
    MemoryUpdate mid um mTags -> do
      let changedFields = memoryUpdateChangedFields um mTags
      updateResult <- rawPutJSON mgr base mApiKey ("/api/v1/memories/" <> uuidPath mid) um
      case updateResult of
        Left err -> pure $ mcpErrorCodeFromRaw "MEMORY_UPDATE_FAILED" err
        Right _ -> case mTags of
          Nothing -> getJSONWith (addChangedFields changedFields . compactMemoryMutationAckWithTags "updated" Nothing) mgr base mApiKey ("/api/v1/memories/" <> uuidPath mid)
          Just tags -> do
            tagResult <- rawPutJSON mgr base mApiKey ("/api/v1/memories/" <> uuidPath mid <> "/tags") tags
            case tagResult of
              Left err -> pure $ mcpErrorCodeFromRaw "MEMORY_TAG_UPDATE_FAILED" err
              Right _  -> getJSONWith (addChangedFields changedFields . compactMemoryMutationAckWithTags "updated" (Just tags)) mgr base mApiKey ("/api/v1/memories/" <> uuidPath mid)
    LinkMemories sid cl -> postJSONWith (const $ memoryLinkAck "linked" sid cl.targetId cl.relationType) mgr base mApiKey ("/api/v1/memories/" <> uuidPath sid <> "/links") cl
    MemoryLinksList mid -> getJSONWith compactMemoryLinksList mgr base mApiKey ("/api/v1/memories/" <> uuidPath mid <> "/links")
    MemoryUnlink sid tid rt -> delJSONWith (const $ memoryLinkAck "unlinked" sid tid rt) mgr base mApiKey ("/api/v1/memories/" <> uuidPath sid <> "/links/"
                               <> uuidPath tid <> "/" <> T.unpack (relationTypeToText rt))
    ProjectCreate cp    -> postJSONWith (compactProjectMutationAck "created") mgr base mApiKey "/api/v1/projects" cp
    ProjectUpdate pid up -> putJSONWith (addChangedFields (projectUpdateChangedFields up) . compactProjectMutationAck "updated") mgr base mApiKey ("/api/v1/projects/" <> uuidPath pid) up
    ProjectLinkMem pid mid -> postJSONWith (const $ entityMemoryLinkAck "linked" "project" pid mid) mgr base mApiKey ("/api/v1/projects/" <> uuidPath pid <> "/memories")
                               (object ["memory_id" .= mid])
    ProjectUnlinkMem pid mid -> delJSONWith (const $ entityMemoryLinkAck "unlinked" "project" pid mid) mgr base mApiKey ("/api/v1/projects/" <> uuidPath pid <> "/memories/"
                                 <> uuidPath mid)
    TaskCreate ct       -> postJSONWith (compactTaskMutationAck "created") mgr base mApiKey "/api/v1/tasks" ct
    TaskOverviewCall tid includeDescription ->
        getJSONWith (if includeDescription then compactTaskOverviewWithDescription else compactTaskOverview) mgr base mApiKey ("/api/v1/tasks/" <> uuidPath tid <> "/overview" <>
          buildQuery [("extra_context", Just "false")])
    ContextGetCall tid level ->
        let levelStr = case level of
              ContextLight  -> "light"
              ContextMedium -> "medium"
              ContextHeavy  -> "heavy"
        in getJSONWith compactContextInfo mgr base mApiKey ("/api/v1/tasks/" <> uuidPath tid <> "/context" <>
             buildQuery [("detail_level", Just levelStr)])
    TaskUpdate tid ut   -> putJSONWith (addChangedFields (taskUpdateChangedFields ut) . compactTaskMutationAck "updated") mgr base mApiKey ("/api/v1/tasks/" <> uuidPath tid) ut
    TaskLinkMem tid mid -> do
      mTargetErr <- ensureTopLevelTaskTarget mgr base mApiKey tid
      maybe
        (postJSONWith (const $ entityMemoryLinkAck "linked" "task" tid mid) mgr base mApiKey ("/api/v1/tasks/" <> uuidPath tid <> "/memories")
          (object ["memory_id" .= mid]))
        pure
        mTargetErr
    TaskUnlinkMem tid mid -> do
      mTargetErr <- ensureTopLevelTaskTarget mgr base mApiKey tid
      maybe
        (delJSONWith (const $ entityMemoryLinkAck "unlinked" "task" tid mid) mgr base mApiKey ("/api/v1/tasks/" <> uuidPath tid <> "/memories/"
          <> uuidPath mid))
        pure
        mTargetErr
    TaskDepAdd tid did  -> postJSONWith (compactDependencyMutationAck "add") mgr base mApiKey ("/api/v1/tasks/" <> uuidPath tid <> "/dependencies")
                            (object ["depends_on_id" .= did])
    TaskDepRemove tid did -> delJSONWith (compactDependencyMutationAck "remove") mgr base mApiKey ("/api/v1/tasks/" <> uuidPath tid <> "/dependencies/"
                               <> uuidPath did)
    WorkspaceList ml mo      -> getJSONWith compactWorkspaceList mgr base mApiKey ("/api/v1/workspaces" <> buildQuery
                            [ ("limit", show <$> ml)
                            , ("offset", show <$> mo)
                            ])
    WorkspaceReg cw     -> postJSONWith (compactWorkspaceMutationAck "created") mgr base mApiKey "/api/v1/workspaces" cw
    ProjectOverviewCall pid includeDescriptions ->
        getJSONWith (if includeDescriptions then compactProjectOverviewWithDescriptions else compactProjectOverview) mgr base mApiKey ("/api/v1/projects/" <> uuidPath pid <> "/overview" <>
          buildQuery [("extra_context", Just "false")])
    ProjectNextTasksCall pid ml includeBlocked ->
        getJSONWith compactNextTasks mgr base mApiKey ("/api/v1/projects/" <> uuidPath pid <> "/next-tasks" <>
          buildQuery
            [ ("limit", show <$> ml)
            , ("include_blocked", if includeBlocked then Just "true" else Nothing)
            ])
    UnifiedSearch usq -> postJSONWith compactSearchResults mgr base mApiKey "/api/v1/search" usq

    -- ================================================================
    -- WORKFLOW COMPOSITE TOOLS
    -- These chain multiple HTTP calls to implement common task/project
    -- workflows in a single MCP tool invocation.
    -- ================================================================

    TaskStartCall tid level -> do
      -- 1. Load the task so task_start can fail before mutating when the
      -- clarified workflow says the task is not startable.
      taskResult <- rawGetJSON mgr base mApiKey ("/api/v1/tasks/" <> uuidPath tid)
      case rawAuthErrorToMcp taskResult of
        Just authErr -> pure authErr
        Nothing -> case taskResult of
          Left err -> pure $ mcpErrorCodeFromRaw "TASK_LOOKUP_FAILED" err
          Right taskVal -> do
            parentGate <- taskStartParentGatePreflight mgr base mApiKey tid taskVal
            case parentGate of
              Just gateErr -> pure gateErr
              Nothing -> do
                dependencyGate <- taskStartDependencyPreflight mgr base mApiKey tid taskVal
                case dependencyGate of
                  Just depErr -> pure depErr
                  Nothing -> do
                    -- 2. Update task status to in_progress (best-effort; may already be in_progress)
                    updateResult <- rawPutJSON mgr base mApiKey ("/api/v1/tasks/" <> uuidPath tid)
                            (object ["status" .= ("in_progress" :: Text)])
                    case taskStartUpdateError updateResult of
                      Just startErr -> pure startErr
                      Nothing -> do
                        -- 3. Load context for the task
                        let levelStr = case level of
                              ContextLight  -> "light"
                              ContextMedium -> "medium"
                              ContextHeavy  -> "heavy"
                        getJSONWith compactTaskStartSuccess mgr base mApiKey ("/api/v1/tasks/" <> uuidPath tid <> "/context" <>
                          buildQuery [("detail_level", Just levelStr)])

    TaskFinishCall tid status mNotes -> do
      -- 1. If notes provided, create a linked memory
      notesResult <- case mNotes of
        Just notes | not (T.null (T.strip notes)) -> do
          -- First get the task to find workspace_id and an eligible memory target.
          taskResult <- rawGetJSON mgr base mApiKey ("/api/v1/tasks/" <> uuidPath tid)
          case rawAuthErrorToMcp taskResult of
            Just authErr -> pure (Left authErr)
            Nothing -> case taskResult of
              Right taskVal -> do
                let mWsId = objectTextField "workspace_id" taskVal
                case mWsId of
                  Just wsId -> do
                    targetResult <- resolveTaskFinishNotesTarget mgr base mApiKey tid taskVal
                    case targetResult of
                      Left targetErr -> pure (Left targetErr)
                      Right target -> do
                        -- Create the notes memory with its required eligible target link atomically.
                        let memBody = taskFinishNotesMemoryBody wsId target notes Nothing
                        memResult <- rawPostJSON mgr base mApiKey "/api/v1/memories" memBody
                        case rawAuthErrorToMcp memResult of
                          Just authErr -> pure (Left authErr)
                          Nothing -> case memResult of
                            Right memVal -> pure (Right (Just memVal))
                            Left err -> pure . Left $ mcpErrorCodeFromRaw "MEMORY_CREATE_FAILED" err
                  Nothing -> pure . Left $ mcpErrorCode "MEMORY_TARGET_REQUIRED" "Could not determine the task workspace for the notes memory."
              Left err -> pure . Left $ mcpErrorCodeFromRaw "TASK_LOOKUP_FAILED" err
        _ -> pure (Right Nothing)
      -- 2. Update task status
      case notesResult of
        Left authErr -> pure authErr
        Right mNotesMemory -> do
          updateResult <- rawPutJSON mgr base mApiKey ("/api/v1/tasks/" <> uuidPath tid)
            (object ["status" .= status])
          case rawAuthErrorToMcp updateResult of
            Just authErr -> pure authErr
            Nothing -> case updateResult of
              Left err -> pure $ mcpErrorCodeFromRaw "TASK_FINISH_FAILED" err
              Right taskVal -> pure $ mcpResultWith id (encode (addChangedFields ["status"] (compactTaskFinishAckWithNotes "finished" mNotesMemory taskVal)))

    ProjectSpecCall wsId pName pDesc pPri tasks -> do
      -- 1. Create the project
      let projBody = object $ filter ((/= Null) . snd)
            [ "workspace_id" .= wsId
            , "name"         .= pName
            , "description"  .= pDesc
            , "priority"     .= pPri
            ]
      projResult <- rawPostJSON mgr base mApiKey "/api/v1/projects" projBody
      case projResult of
        Left err -> pure $ mcpErrorCodeFromRaw "PROJECT_CREATE_FAILED" err
        Right projVal -> do
          let mProjId = case projVal of
                Object o -> case KM.lookup "id" o of
                  Just (String pid) -> Just pid
                  _                 -> Nothing
                _ -> Nothing
          case mProjId of
            Nothing -> pure $ mcpErrorCode "PROJECT_CREATE_FAILED" "Could not extract project ID"
            Just projId -> do
              -- 2. Create each task under the project
              taskResults <- mapM (\st -> do
                let taskBody = object $ filter ((/= Null) . snd)
                      [ "workspace_id" .= wsId
                      , "project_id"   .= projId
                      , "title"        .= st.stTitle
                      , "description"  .= st.stDescription
                      , "priority"     .= st.stPriority
                      ]
                rawPostJSON mgr base mApiKey "/api/v1/tasks" taskBody
                ) tasks
              case firstRawAuthError taskResults of
                Just authErr -> pure $ mcpErrorCodeFromRaw "TASK_CREATE_FAILED" authErr
                Nothing -> do
                  let createdTasks = [v | Right v <- taskResults]
                      failedCount  = length [() | Left _ <- taskResults]
                      result = object
                        [ "project" .= projVal
                        , "tasks"   .= createdTasks
                        , "tasks_failed" .= failedCount
                        ]
                  pure $ mcpResultWith compactProjectSpecSummary (encode result)

    ProjectArchiveCall pid mSummary -> do
      -- 1. Get project to find workspace_id
      projResult <- rawGetJSON mgr base mApiKey ("/api/v1/projects/" <> uuidPath pid)
      case projResult of
        Left err -> pure $ mcpErrorCodeFromRaw "PROJECT_NOT_FOUND" err
        Right projVal -> do
          -- 2. If summary provided, create a linked memory
          summaryResult <- case mSummary of
            Just summary | not (T.null (T.strip summary)) -> do
              let mWsId = case projVal of
                    Object o -> case KM.lookup "workspace_id" o of
                      Just (String ws) -> Just ws
                      _                -> Nothing
                    _ -> Nothing
              case mWsId of
                Just wsId -> do
                  let memBody = object
                        [ "workspace_id" .= wsId
                        , "project_id"   .= pid
                        , "content"      .= summary
                        , "memory_type"  .= ("long_term" :: Text)
                        , "importance"   .= (7 :: Int)
                        , "source"       .= ("inferred" :: Text)
                        , "tags"         .= (["project-summary" :: Text])
                        ]
                  memResult <- rawPostJSON mgr base mApiKey "/api/v1/memories" memBody
                  case rawAuthErrorToMcp memResult of
                    Just authErr -> pure (Left authErr)
                    Nothing -> case memResult of
                      Right memVal -> pure (Right (Just memVal))
                      Left err -> pure . Left $ mcpErrorCodeFromRaw "MEMORY_CREATE_FAILED" err
                Nothing -> pure (Right Nothing)
            _ -> pure (Right Nothing)
          -- 3. Archive the project
          case summaryResult of
            Left authErr -> pure authErr
            Right mSummaryMemory -> do
              archiveResult <- rawPutJSON mgr base mApiKey ("/api/v1/projects/" <> uuidPath pid)
                (object ["status" .= ("archived" :: Text)])
              case rawAuthErrorToMcp archiveResult of
                Just authErr -> pure authErr
                Nothing -> case archiveResult of
                  Left err -> pure $ mcpErrorCodeFromRaw "PROJECT_ARCHIVE_FAILED" err
                  Right archivedProject -> pure $ mcpResultWith id (encode (addChangedFields ["status"] (compactProjectArchiveAck mSummaryMemory archivedProject)))

------------------------------------------------------------------------
-- Typed HTTP helpers
------------------------------------------------------------------------

postJSONWith :: ToJSON a => (Value -> Value) -> Manager -> String -> Maybe Text -> String -> a -> IO Value
postJSONWith shaper mgr base mApiKey path body = httpJSONWith shaper mgr mApiKey "POST" (base <> path) (Just (encode body))

getJSONWith :: (Value -> Value) -> Manager -> String -> Maybe Text -> String -> IO Value
getJSONWith shaper mgr base mApiKey path = httpJSONWith shaper mgr mApiKey "GET" (base <> path) Nothing

putJSONWith :: ToJSON a => (Value -> Value) -> Manager -> String -> Maybe Text -> String -> a -> IO Value
putJSONWith shaper mgr base mApiKey path body = httpJSONWith shaper mgr mApiKey "PUT" (base <> path) (Just (encode body))

delJSONWith :: (Value -> Value) -> Manager -> String -> Maybe Text -> String -> IO Value
delJSONWith shaper mgr base mApiKey path = httpJSONWith shaper mgr mApiKey "DELETE" (base <> path) Nothing

httpJSONWith :: (Value -> Value) -> Manager -> Maybe Text -> String -> String -> Maybe BL.ByteString -> IO Value
httpJSONWith shaper mgr mApiKey httpMethod url mbody = do
  result <- try $ do
    initReq <- parseRequest url
    let authHeaders = bearerAuthHeaders mApiKey
    let req = initReq
          { method         = fromString httpMethod
        , requestHeaders = [("Content-Type", "application/json")] <> authHeaders
          , requestBody    = maybe (RequestBodyBS mempty) RequestBodyLBS mbody
          }
    resp <- httpLbs req mgr
    let code = statusCode (responseStatus resp)
        body = responseBody resp
    if code >= 200 && code < 300
      then pure $ mcpResultWith shaper body
      else pure $ mcpHttpError code body
  case result of
    Right v  -> pure v
    Left (_ :: SomeException) -> pure $ mcpErrorCode "CONNECTION_ERROR" connectionErrorMessage

-- | Raw HTTP helpers for composite tools — return Either instead of MCP-wrapped values.
-- These allow workflow handlers to chain calls and build combined responses.
rawHttpJSON' :: Manager -> Maybe Text -> String -> String -> Maybe BL.ByteString -> IO (Either Text Value)
rawHttpJSON' mgr mApiKey httpMethod url mbody = do
  result <- try $ do
    initReq <- parseRequest url
    let authHeaders = bearerAuthHeaders mApiKey
    let req = initReq
          { method         = fromString httpMethod
          , requestHeaders = [("Content-Type", "application/json")] <> authHeaders
          , requestBody    = maybe (RequestBodyBS mempty) RequestBodyLBS mbody
          }
    resp <- httpLbs req mgr
    let code = statusCode (responseStatus resp)
        body = responseBody resp
    if code >= 200 && code < 300
      then case eitherDecode body of
        Right v  -> pure (Right v)
        Left _   -> pure (Right (String (decodeUtf8 body)))
      else pure (Left (rawHttpErrorPayload code body))
  case result of
    Right v  -> pure v
    Left (_ :: SomeException) -> pure (Left ("[CONNECTION_ERROR] " <> connectionErrorMessage))

rawGetJSON :: Manager -> String -> Maybe Text -> String -> IO (Either Text Value)
rawGetJSON mgr base mApiKey path = rawHttpJSON' mgr mApiKey "GET" (base <> path) Nothing

rawPostJSON :: ToJSON a => Manager -> String -> Maybe Text -> String -> a -> IO (Either Text Value)
rawPostJSON mgr base mApiKey path body = rawHttpJSON' mgr mApiKey "POST" (base <> path) (Just (encode body))

rawPutJSON :: ToJSON a => Manager -> String -> Maybe Text -> String -> a -> IO (Either Text Value)
rawPutJSON mgr base mApiKey path body = rawHttpJSON' mgr mApiKey "PUT" (base <> path) (Just (encode body))


bearerAuthHeaders :: Maybe Text -> RequestHeaders
bearerAuthHeaders = maybe [] (\key -> [("Authorization", "Bearer " <> TE.encodeUtf8 key)])

------------------------------------------------------------------------
-- Compact MCP response shaping helpers
------------------------------------------------------------------------

compactWorkspaceSummary :: Value -> Value
compactWorkspaceSummary value = object $ catMaybes
  [ copyField "id" value
  , copyField "name" value
  , copyField "workspace_type" value
  , copyField "gh_owner" value
  , copyField "gh_repo" value
  ]


compactMemorySummary :: Value -> Value
compactMemorySummary value = object $ catMaybes
  [ copyField "id" value
  , copyField "summary" value
  , copyField "memory_type" value
  , copyField "importance" value
  , copyNonEmptyArrayField "tags" value
  , copyTrueBoolField "pinned" value
  ]


compactMemoryDetail :: Value -> Value
compactMemoryDetail value = object $ catMaybes
  [ copyField "id" value
  , copyField "summary" value
  , copyField "content" value
  , copyField "memory_type" value
  , copyField "importance" value
  , copyNonEmptyArrayField "tags" value
  , copyTrueBoolField "pinned" value
  , copyNonEmptyObjectField "metadata" value
  , copyField "expires_at" value
  , copyField "source" value
  , copyField "confidence" value
  ]


compactProjectSummary :: Value -> Value
compactProjectSummary value = object $ catMaybes
  [ copyField "id" value
  , copyField "name" value
  , copyField "status" value
  , copyField "priority" value
  , copyField "parent_id" value
  ]


compactProjectSummaryWithDescription :: Value -> Value
compactProjectSummaryWithDescription value = insertOptionalField "description" value (compactProjectSummary value)


compactTaskSummary :: Value -> Value
compactTaskSummary value = object $ catMaybes
  [ copyField "id" value
  , copyField "title" value
  , copyField "status" value
  , copyField "priority" value
  , copyField "project_id" value
  , copyField "parent_id" value
  , copyField "due_at" value
  ]


compactTaskSummaryWithDescription :: Value -> Value
compactTaskSummaryWithDescription value = insertOptionalField "description" value (compactTaskSummary value)


compactSearchResults :: Value -> Value
compactSearchResults value = object
  [ "memories" .= mapArrayFieldOrEmpty "memories" compactMemorySummary value
  , "projects" .= mapArrayFieldOrEmpty "projects" compactProjectSearchRow value
  , "tasks" .= mapArrayFieldOrEmpty "tasks" compactTaskSearchRow value
  ]


compactProjectSearchRow :: Value -> Value
compactProjectSearchRow value = object $ catMaybes
  [ fieldWithDefault "project" compactProjectSummary compactProjectSummary value
  , nonEmptyMappedArrayField "linked_memories" compactLinkedMemorySummary value
  ]


compactTaskSearchRow :: Value -> Value
compactTaskSearchRow value = object $ catMaybes
  [ fieldWithDefault "task" compactTaskSummary compactTaskSummary value
  , nonEmptyMappedArrayField "linked_memories" compactLinkedMemorySummary value
  ]


compactLinkedMemorySummary :: Value -> Value
compactLinkedMemorySummary value = object $ catMaybes
  [ copyField "id" value
  , copyField "summary" value
  , copyField "importance" value
  , copyNonEmptyArrayField "tags" value
  ]


compactConnectedMemorySummary :: Value -> Value
compactConnectedMemorySummary value = object $ catMaybes
  [ copyField "id" value
  , copyField "summary" value
  , copyField "scope" value
  ]


compactTaskDependencySummary :: Value -> Value
compactTaskDependencySummary value = object $ catMaybes
  [ copyField "id" value
  , copyField "title" value <|> copyField "name" value
  , copyField "status" value
  ]


compactProjectOverview :: Value -> Value
compactProjectOverview = compactProjectOverviewWith compactProjectSummary compactTaskSummary compactProjectSummary


compactProjectOverviewWithDescriptions :: Value -> Value
compactProjectOverviewWithDescriptions = compactProjectOverviewWith compactProjectSummaryWithDescription compactTaskSummaryWithDescription compactProjectSummaryWithDescription


compactProjectOverviewWith :: (Value -> Value) -> (Value -> Value) -> (Value -> Value) -> Value -> Value
compactProjectOverviewWith projectShaper taskShaper subprojectShaper value = object $ catMaybes
  [ fieldWith "project" projectShaper value
  , mappedArrayField "tasks" taskShaper value
  , mappedArrayField "subprojects" subprojectShaper value
  , nonEmptyMappedArrayField "linked_memories" compactMemorySummary value
  , mappedArrayField "connected_memories" compactConnectedMemorySummary value
  , fieldWith "readiness_rollup" compactReadinessRollup value
  ]


compactTaskOverview :: Value -> Value
compactTaskOverview = compactTaskOverviewWith compactTaskSummary


compactTaskOverviewWithDescription :: Value -> Value
compactTaskOverviewWithDescription = compactTaskOverviewWith compactTaskSummaryWithDescription


compactTaskOverviewWith :: (Value -> Value) -> Value -> Value
compactTaskOverviewWith taskShaper value = object $ catMaybes
  [ fieldWith "task" taskShaper value
  , mappedArrayField "dependencies" compactTaskDependencySummary value
  , mappedArrayField "connected_memories" compactConnectedMemorySummary value
  , fieldWith "readiness_rollup" compactReadinessRollup value
  ]


compactContextInfo :: Value -> Value
compactContextInfo value = object $ catMaybes
  [ fieldWith "task" compactTaskSummary value
  , copyField "detail_level" value
  , mappedArrayField "task_memories" compactConnectedMemorySummary value
  , mappedArrayField "project_memories" compactConnectedMemorySummary value
  , mappedArrayField "workspace_memories" compactConnectedMemorySummary value
  ]


compactTaskStartSuccess :: Value -> Value
compactTaskStartSuccess value = case compactContextInfo value of
  Object o -> Object (KM.insert "started" (Bool True) o)
  other    -> other


compactMemoryLinksList :: Value -> Value
compactMemoryLinksList value = object
  [ "links" .= map compactGraphEdge (objectArrayValue value) ]


compactGraphEdge :: Value -> Value
compactGraphEdge value = object $ catMaybes
  [ copyField "source_id" value
  , copyField "target_id" value
  , copyField "relation_type" value
  , copyField "strength" value
  ]


compactNextTasks :: Value -> Value
compactNextTasks value = object
  [ "items" .= map compactNextTaskCandidateSummary (objectArrayValue value) ]


compactNextTaskCandidateSummary :: Value -> Value
compactNextTaskCandidateSummary value = object $ catMaybes
  [ fieldWith "task" compactTaskSummary value
  , Just $ "dependency_blocked" .= fromMaybe False (objectBoolField "dependency_blocked" value)
  , copyTrueBoolField "completion_gated" value
  , copyNonZeroNumberField "open_descendant_count" value
  , copyNonZeroNumberField "open_dependency_count" value
  ]


compactDependencyMutationAck :: Text -> Value -> Value
compactDependencyMutationAck fallbackAction value = object $ catMaybes
  [ Just $ "ok" .= True
  , Just $ "action" .= actionTextFromValue fallbackAction value
  , Just $ "entity_type" .= ("task_dependency" :: Text)
  , copyField "task_id" value
  , copyField "depends_on_id" value
  , nonEmptyMappedArrayField "affected_tasks" compactDependencyEffectSummary value
  ]


compactDependencyEffectSummary :: Value -> Value
compactDependencyEffectSummary value = object $ catMaybes
  [ fieldWith "task" compactTaskSummary value
  , copyField "previous_status" value
  , copyField "current_status" value
  , copyTrueBoolField "auto_blocked" value
  , copyNonZeroNumberField "open_dependency_count" value
  , copyField "reason" value
  ]


compactTaskMutationAck :: Text -> Value -> Value
compactTaskMutationAck action value =
  let summary = compactTaskSummary value
  in mutationAck action "task" summary
      [ nonEmptyMappedArrayField "dependency_effects" compactDependencyEffectSummary value ]


compactTaskFinishAck :: Text -> Value -> Value
compactTaskFinishAck action = compactTaskMutationAck action


compactTaskFinishAckWithNotes :: Text -> Maybe Value -> Value -> Value
compactTaskFinishAckWithNotes action mNotesMemory value =
  insertOptionalSummary "notes_memory" mNotesMemory $ compactTaskFinishAck action value


compactMemoryMutationAckWithTargets :: Text -> Maybe UUID -> Maybe UUID -> Value -> Value
compactMemoryMutationAckWithTargets action mProjectId mTaskId value =
  mutationAck action "memory" (compactMemorySummary value)
    [ ("project_id" .=) <$> mProjectId
    , ("task_id" .=) <$> mTaskId
    ]


compactMemoryMutationAckWithTags :: Text -> Maybe [Text] -> Value -> Value
compactMemoryMutationAckWithTags action mTags value =
  mutationAck action "memory" (compactMemorySummary value)
    [ ("tags" .=) <$> mTags ]


compactProjectMutationAck :: Text -> Value -> Value
compactProjectMutationAck action value = mutationAck action "project" (compactProjectSummary value) []


compactProjectArchiveAck :: Maybe Value -> Value -> Value
compactProjectArchiveAck mSummaryMemory value =
  insertOptionalSummary "summary_memory" mSummaryMemory $ compactProjectMutationAck "archived" value


compactWorkspaceMutationAck :: Text -> Value -> Value
compactWorkspaceMutationAck action value = mutationAck action "workspace" (compactWorkspaceSummary value) []


addChangedFields :: [Text] -> Value -> Value
addChangedFields [] value = value
addChangedFields fields (Object obj) = Object $ KM.insert "changed_fields" (toJSON fields) obj
addChangedFields fields value = object
  [ "result" .= value
  , "changed_fields" .= fields
  ]


memoryUpdateChangedFields :: UpdateMemory -> Maybe [Text] -> [Text]
memoryUpdateChangedFields um mTags = catMaybes
  [ changedWhen "content" um.content
  , fieldUpdateChanged "summary" um.summary
  , changedWhen "memory_type" um.memoryType
  , changedWhen "importance" um.importance
  , changedWhen "metadata" um.metadata
  , fieldUpdateChanged "expires_at" um.expiresAt
  , fieldUpdateChanged "source" um.source
  , changedWhen "confidence" um.confidence
  , changedWhen "pinned" um.pinned
  , changedWhen "tags" mTags
  ]


projectUpdateChangedFields :: UpdateProject -> [Text]
projectUpdateChangedFields up = catMaybes
  [ changedWhen "name" up.name
  , fieldUpdateChanged "description" up.description
  , fieldUpdateChanged "parent_id" up.parentId
  , changedWhen "status" up.status
  , changedWhen "priority" up.priority
  , changedWhen "metadata" up.metadata
  ]


taskUpdateChangedFields :: UpdateTask -> [Text]
taskUpdateChangedFields ut = catMaybes
  [ changedWhen "title" ut.title
  , fieldUpdateChanged "description" ut.description
  , fieldUpdateChanged "project_id" ut.projectId
  , fieldUpdateChanged "parent_id" ut.parentId
  , changedWhen "status" ut.status
  , changedWhen "priority" ut.priority
  , changedWhen "metadata" ut.metadata
  , fieldUpdateChanged "due_at" ut.dueAt
  ]


changedWhen :: Text -> Maybe a -> Maybe Text
changedWhen field = fmap (const field)


fieldUpdateChanged :: Text -> FieldUpdate a -> Maybe Text
fieldUpdateChanged _ Unchanged = Nothing
fieldUpdateChanged field _ = Just field


actionTextFromValue :: Text -> Value -> Text
actionTextFromValue fallbackAction value = case objectNonNullField "action" value of
  Just (String action) -> action
  _                    -> fallbackAction


compactProjectSpecSummary :: Value -> Value
compactProjectSpecSummary value = object $ catMaybes
  [ fieldWith "project" compactProjectSummary value
  , renameMappedArrayField "tasks" "tasks_created" compactTaskSummary value
  , copyNonZeroNumberField "tasks_failed" value
  ]


mutationAck :: Text -> Text -> Value -> [Maybe Pair] -> Value
mutationAck action entityType summary extraPairs = object $
  [ "ok" .= True
  , "action" .= action
  , "entity_type" .= entityType
  ]
  <> catMaybes
       [ copyField "id" summary
       , copyField "status" summary
       , Just ("summary" .= summary)
       ]
  <> catMaybes extraPairs


memoryLinkAck :: Text -> UUID -> UUID -> RelationType -> Value
memoryLinkAck action sourceId targetId relationType = object
  [ "ok" .= True
  , "action" .= action
  , "entity_type" .= ("memory_link" :: Text)
  , "source_id" .= sourceId
  , "target_id" .= targetId
  , "relation_type" .= relationType
  ]


entityMemoryLinkAck :: Text -> Text -> UUID -> UUID -> Value
entityMemoryLinkAck action entityType entityId memoryId = object
  [ "ok" .= True
  , "action" .= action
  , "entity_type" .= entityType
  , "entity_id" .= entityId
  , "memory_id" .= memoryId
  ]


insertOptionalSummary :: Key -> Maybe Value -> Value -> Value
insertOptionalSummary _ Nothing base = base
insertOptionalSummary key (Just entityValue) (Object base) =
  let summary = compactMemorySummary entityValue
      withSummary = KM.insert key summary base
      withSummaryId = case objectNonNullField "id" entityValue of
        Just summaryId -> KM.insert (key <> "_id") summaryId withSummary
        Nothing        -> withSummary
  in Object withSummaryId
insertOptionalSummary key (Just entityValue) base = object
  [ "result" .= base
  , key .= compactMemorySummary entityValue
  ]


compactReadinessRollup :: Value -> Value
compactReadinessRollup value = object $ catMaybes
  [ copyField "completion_ready" value
  , copyNonZeroNumberField "open_project_count" value
  , copyNonZeroNumberField "closed_project_count" value
  , copyNonZeroNumberField "open_task_count" value
  , copyNonZeroNumberField "done_task_count" value
  , copyNonZeroNumberField "cancelled_task_count" value
  , copyNonZeroNumberField "blocked_task_count" value
  , copyNonZeroNumberField "dependency_blocked_task_count" value
  , copyNonZeroNumberField "open_dependency_count" value
  , copyNonZeroNumberField "open_subtask_count" value
  , copyNonZeroNumberField "done_subtask_count" value
  , copyNonZeroNumberField "cancelled_subtask_count" value
  , copyNonZeroNumberField "blocked_subtask_count" value
  ]


compactWorkspaceList :: Value -> Value
compactWorkspaceList value = object $ catMaybes
  [ Just $ "items" .= mapArrayFieldOrEmpty "items" compactWorkspaceSummary value
  , copyTrueBoolField "has_more" value
  ]


fieldWith :: Key -> (Value -> Value) -> Value -> Maybe Pair
fieldWith key shaper value = (key .=) . shaper <$> objectNonNullField key value


fieldWithDefault :: Key -> (Value -> Value) -> (Value -> Value) -> Value -> Maybe Pair
fieldWithDefault key shaper fallback value =
  Just $ key .= maybe (fallback value) shaper (objectNonNullField key value)


mappedArrayField :: Key -> (Value -> Value) -> Value -> Maybe Pair
mappedArrayField key shaper value = (key .=) <$> mappedArray key shaper value


renameMappedArrayField :: Key -> Key -> (Value -> Value) -> Value -> Maybe Pair
renameMappedArrayField fromKey toKey shaper value = (toKey .=) <$> mappedArray fromKey shaper value


nonEmptyMappedArrayField :: Key -> (Value -> Value) -> Value -> Maybe Pair
nonEmptyMappedArrayField key shaper value = do
  items <- mappedArray key shaper value
  if null items then Nothing else Just (key .= items)


mappedArray :: Key -> (Value -> Value) -> Value -> Maybe [Value]
mappedArray key shaper value = case objectNonNullField key value of
  Just (Array arr) -> Just $ map shaper (toList arr)
  _                -> Nothing


mapArrayFieldOrEmpty :: Key -> (Value -> Value) -> Value -> [Value]
mapArrayFieldOrEmpty key shaper value = fromMaybe [] (mappedArray key shaper value)


copyField :: Key -> Value -> Maybe Pair
copyField key value = (key .=) <$> objectNonNullField key value


copyNonEmptyArrayField :: Key -> Value -> Maybe Pair
copyNonEmptyArrayField key value = case objectNonNullField key value of
  Just (Array arr) | not (null arr) -> Just $ key .= Array arr
  _                                -> Nothing


copyNonEmptyObjectField :: Key -> Value -> Maybe Pair
copyNonEmptyObjectField key value = case objectNonNullField key value of
  Just (Object obj) | not (KM.null obj) -> Just $ key .= Object obj
  _                                    -> Nothing


copyTrueBoolField :: Key -> Value -> Maybe Pair
copyTrueBoolField key value = case objectNonNullField key value of
  Just (Bool True) -> Just $ key .= True
  _                -> Nothing


copyNonZeroNumberField :: Key -> Value -> Maybe Pair
copyNonZeroNumberField key value = case objectNonNullField key value of
  Just (Number n) | n /= 0 -> Just $ key .= Number n
  _                        -> Nothing


insertOptionalField :: Key -> Value -> Value -> Value
insertOptionalField key source (Object target) = case objectNonNullField key source of
  Just fieldValue -> Object $ KM.insert key fieldValue target
  Nothing         -> Object target
insertOptionalField key source target = object $ catMaybes
  [ Just $ "result" .= target
  , copyField key source
  ]


objectField :: Key -> Value -> Maybe Value
objectField key = \case
  Object o -> KM.lookup key o
  _        -> Nothing


objectNonNullField :: Key -> Value -> Maybe Value
objectNonNullField key value = case objectField key value of
  Just Null -> Nothing
  other     -> other


objectBoolField :: Key -> Value -> Maybe Bool
objectBoolField key value = case objectNonNullField key value of
  Just (Bool boolValue) -> Just boolValue
  _                     -> Nothing

------------------------------------------------------------------------
-- MCP content helpers
------------------------------------------------------------------------

-- | Wrap an API response for MCP after applying a tool-specific response shaper.
-- The generic 'trimForLLM' pass remains a safety net after the shaper runs.
mcpResultWith :: (Value -> Value) -> BL.ByteString -> Value
mcpResultWith shaper body =
  let trimmed = case eitherDecode body of
        Right v -> encode (trimForLLM (shaper v))
        Left _
          | BL.null body -> encode (trimForLLM (shaper Null))
          | otherwise    -> body   -- not JSON; pass through as-is
  in object
       [ "content" .= [object ["type" .= ("text" :: Text), "text" .= decodeUtf8 trimmed]] ]

-- | Strip fields that are noisy for LLM context but not actionable.
-- Removes: workspace_id, created_at, updated_at, last_accessed_at,
-- access_count, fts_language, metadata (when empty {}), confidence
-- (when null), source (when null), expires_at (when null),
-- dependency_count, memory_link_count.
trimForLLM :: Value -> Value
trimForLLM (Object o) =
  let cleaned = KM.filterWithKey keepField o
      keepField k v = not $ k `elem` dropKeys || isNullDrop k v || isEmptyMetadata k v
      dropKeys =
        [ "workspace_id", "created_at", "updated_at", "last_accessed_at"
        , "access_count", "fts_language", "dependency_count", "memory_link_count"
        ]
      isNullDrop k v = v == Null && k `elem` ["confidence", "source", "expires_at", "completed_at", "due_at", "parent_id", "project_id", "description", "gh_owner", "gh_repo"]
      isEmptyMetadata k v = k == "metadata" && v == Object mempty
  in Object (KM.map trimForLLM cleaned)
trimForLLM (Array arr) = Array (fmap trimForLLM arr)
trimForLLM v = v

mcpError :: Text -> Value
mcpError msg = object
  [ "isError" .= True
  , "content" .= [object ["type" .= ("text" :: Text), "text" .= msg]]
  ]

-- | Structured error with an error code for programmatic handling by LLMs.
mcpErrorCode :: Text -> Text -> Value
mcpErrorCode code msg = object
  [ "isError" .= True
  , "content" .= [object
      [ "type" .= ("text" :: Text)
      , "text" .= ("[" <> code <> "] " <> msg)
      ]]
  ]


mcpErrorCodeFromRaw :: Text -> Text -> Value
mcpErrorCodeFromRaw fallbackCode rawMessage =
  case mcpErrorFromRawPayload rawMessage of
    Just structuredError -> structuredError
    Nothing -> case rawErrorCode rawMessage of
      Just rawCode | shouldPromoteRawCode rawCode ->
        mcpErrorCode rawCode (stripRawErrorCode rawMessage)
      _ -> mcpErrorCode fallbackCode rawMessage


shouldPromoteRawCode :: Text -> Bool
shouldPromoteRawCode code =
  code `elem` ["AUTH_REQUIRED", "AUTH_FORBIDDEN"]
    || (not ("HTTP_" `T.isPrefixOf` code) && code `notElem` transportCodes)
  where
    transportCodes = ["NOT_FOUND", "RATE_LIMITED", "CONNECTION_ERROR"]


data ApiError = ApiError
  { apiErrorType           :: Text
  , apiErrorCode           :: Maybe Text
  , apiErrorMessage        :: Maybe Text
  , apiErrorDetail         :: Maybe Value
  , apiErrorHint           :: Maybe Text
  , apiErrorRequiredAction :: Maybe Text
  }


instance FromJSON ApiError where
  parseJSON = withObject "ApiError" $ \o -> do
    apiErrorType <- o .: "error"
    apiErrorCode <- o .:? "code"
    apiErrorMessage <- o .:? "message"
    apiErrorDetail <- o .:? "detail"
    apiErrorHint <- o .:? "hint"
    apiErrorRequiredAction <- o .:? "required_action"
    pure ApiError {..}


mcpHttpError :: Int -> BL.ByteString -> Value
mcpHttpError code body
  | code == 401 || code == 403 = fallback
  | otherwise = case decodeApiError body of
      Just apiErr | shouldUseStructuredApiError apiErr -> mcpApiError code apiErr
      _ -> fallback
  where
    fallback = mcpErrorCode (httpErrorCode code) (httpErrorMessage code body)


rawHttpErrorText :: Int -> BL.ByteString -> Text
rawHttpErrorText code body
  | code == 401 || code == 403 = fallback
  | otherwise = case decodeApiError body of
      Just apiErr | shouldUseStructuredApiError apiErr ->
        "[" <> apiErrorCodeText code apiErr <> "] " <> apiErrorDisplayText apiErr
      _ -> fallback
  where
    fallback = "[" <> httpErrorCode code <> "] " <> httpErrorMessage code body


rawHttpErrorPayload :: Int -> BL.ByteString -> Text
rawHttpErrorPayload code body
  | code == 401 || code == 403 = rawHttpErrorText code body
  | otherwise = case decodeApiError body of
      Just apiErr | shouldUseStructuredApiError apiErr -> decodeUtf8 (encode (mcpApiError code apiErr))
      _ -> rawHttpErrorText code body


decodeApiError :: BL.ByteString -> Maybe ApiError
decodeApiError = decode @ApiError


mcpErrorFromRawPayload :: Text -> Maybe Value
mcpErrorFromRawPayload raw = case decode @Value (BL.fromStrict (TE.encodeUtf8 raw)) of
  Just value@(Object obj)
    | KM.lookup "isError" obj == Just (Bool True)
    , KM.member "content" obj -> Just value
  _ -> Nothing


shouldUseStructuredApiError :: ApiError -> Bool
shouldUseStructuredApiError apiErr = case apiErr.apiErrorCode of
  Just code -> not (T.null (T.strip code))
  Nothing   -> False


mcpApiError :: Int -> ApiError -> Value
mcpApiError httpStatus apiErr = object
  [ "isError" .= True
  , "content" .= [object
      [ "type" .= ("text" :: Text)
      , "text" .= ("[" <> apiErrorCodeText httpStatus apiErr <> "] " <> apiErrorDisplayText apiErr)
      ]]
  , "error" .= object
      ([ "type" .= apiErr.apiErrorType
       , "http_status" .= httpStatus
       , "code" .= apiErrorCodeText httpStatus apiErr
       ]
       ++ [ "message" .= msg | Just msg <- [apiErrorMessageNonEmpty apiErr] ]
       ++ [ "hint" .= hint | Just hint <- [apiErrorHintNonEmpty apiErr] ]
       ++ [ "required_action" .= action | Just action <- [apiErrorRequiredActionNonEmpty apiErr] ]
       ++ [ "detail" .= detail | Just detail <- [apiErr.apiErrorDetail] ]
      )
  ]


apiErrorCodeText :: Int -> ApiError -> Text
apiErrorCodeText httpStatus apiErr = fromMaybe (httpErrorCode httpStatus) apiErr.apiErrorCode


apiErrorDisplayText :: ApiError -> Text
apiErrorDisplayText apiErr = firstNonEmpty
  [ apiErrorRequiredActionNonEmpty apiErr
  , apiErrorHintNonEmpty apiErr
  , apiErrorMessageNonEmpty apiErr
  , Just apiErr.apiErrorType
  ]


apiErrorMessageNonEmpty :: ApiError -> Maybe Text
apiErrorMessageNonEmpty apiErr = nonEmptyText apiErr.apiErrorMessage


apiErrorHintNonEmpty :: ApiError -> Maybe Text
apiErrorHintNonEmpty apiErr = nonEmptyText apiErr.apiErrorHint


apiErrorRequiredActionNonEmpty :: ApiError -> Maybe Text
apiErrorRequiredActionNonEmpty apiErr = case nonEmptyText apiErr.apiErrorRequiredAction of
  Just action -> Just action
  Nothing     -> apiErrorHintNonEmpty apiErr


nonEmptyText :: Maybe Text -> Maybe Text
nonEmptyText value = do
  text <- T.strip <$> value
  if T.null text then Nothing else Just text


firstNonEmpty :: [Maybe Text] -> Text
firstNonEmpty [] = "hmem-server rejected the request."
firstNonEmpty (Just value : _) = value
firstNonEmpty (Nothing : rest) = firstNonEmpty rest


rawErrorCode :: Text -> Maybe Text
rawErrorCode raw = case T.stripPrefix "[" raw of
  Nothing -> Nothing
  Just rest -> case T.breakOn "]" rest of
    (code, suffix) | not (T.null code) && "]" `T.isPrefixOf` suffix -> Just code
    _ -> Nothing


stripRawErrorCode :: Text -> Text
stripRawErrorCode raw = case rawErrorCode raw of
  Nothing -> raw
  Just code -> T.strip $ T.drop (T.length code + 2) raw


ensureTopLevelMemoryTaskTargets :: Manager -> String -> Maybe Text -> [CreateMemory] -> IO (Maybe Value)
ensureTopLevelMemoryTaskTargets mgr base mApiKey cms =
  firstTargetError (uniqueUUIDs [tid | cm <- cms, Just tid <- [cm.taskId]])
  where
    firstTargetError [] = pure Nothing
    firstTargetError (tid : rest) = do
      mErr <- ensureTopLevelTaskTarget mgr base mApiKey tid
      case mErr of
        Just err -> pure (Just err)
        Nothing  -> firstTargetError rest


ensureTopLevelTaskTarget :: Manager -> String -> Maybe Text -> UUID -> IO (Maybe Value)
ensureTopLevelTaskTarget mgr base mApiKey tid = do
  taskResult <- rawGetJSON mgr base mApiKey ("/api/v1/tasks/" <> uuidPath tid)
  case rawAuthErrorToMcp taskResult of
    Just authErr -> pure (Just authErr)
    Nothing -> case taskResult of
      Left err -> pure . Just $ mcpErrorCodeFromRaw "TASK_TARGET_LOOKUP_FAILED" err
      Right taskVal
        | isTopLevelTaskTarget taskVal -> pure Nothing
        | otherwise -> pure . Just $ mcpErrorCode "SUBTASK_MEMORY_TARGET_NOT_ALLOWED" $
            "Memories can only be linked to projects or top-level tasks. Provide a project_id or the nearest top-level task_id instead of subtask " <> T.pack (show tid) <> "."


isTopLevelTaskTarget :: Value -> Bool
isTopLevelTaskTarget taskVal = case taskParentId taskVal of
  Nothing -> True
  Just _  -> False


resolveTaskFinishNotesTarget :: Manager -> String -> Maybe Text -> UUID -> Value -> IO (Either Value MemoryTarget)
resolveTaskFinishNotesTarget mgr base mApiKey tid taskVal =
  case taskParentId taskVal of
    Nothing -> pure (Right $ MemoryTargetTask tid)
    Just parentId -> do
      ancestorsResult <- fetchTaskAncestors mgr base mApiKey parentId []
      case ancestorsResult of
        Left err -> case taskProjectId taskVal of
          Just projectId -> pure (Right $ MemoryTargetProject projectId)
          Nothing        -> pure (Left err)
        Right ancestors -> case taskFinishNotesTarget tid taskVal ancestors of
          Right target -> pure (Right target)
          Left msg -> pure . Left $ mcpErrorCode "MEMORY_TARGET_REQUIRED" msg


fetchTaskAncestors :: Manager -> String -> Maybe Text -> UUID -> [Value] -> IO (Either Value [Value])
fetchTaskAncestors mgr base mApiKey parentId acc
  | length acc > 100 = pure . Left $ mcpErrorCode "TASK_ANCESTOR_LOOKUP_FAILED" "Task ancestor chain is unexpectedly deep; cannot choose a safe notes memory target."
  | otherwise = do
      parentResult <- rawGetJSON mgr base mApiKey ("/api/v1/tasks/" <> uuidPath parentId)
      case rawAuthErrorToMcp parentResult of
        Just authErr -> pure (Left authErr)
        Nothing -> case parentResult of
          Left err -> pure . Left $ mcpErrorCodeFromRaw "TASK_ANCESTOR_LOOKUP_FAILED" err
          Right parentVal -> case taskParentId parentVal of
            Nothing -> pure (Right (acc <> [parentVal]))
            Just grandParentId -> fetchTaskAncestors mgr base mApiKey grandParentId (acc <> [parentVal])


taskFinishNotesTarget :: UUID -> Value -> [Value] -> Either Text MemoryTarget
taskFinishNotesTarget tid taskVal ancestors =
  case taskParentId taskVal of
    Nothing -> Right $ MemoryTargetTask tid
    Just _ -> case [ancestorId | ancestor <- ancestors, taskParentId ancestor == Nothing, Just ancestorId <- [taskIdValue ancestor]] of
      (rootId : _) -> Right $ MemoryTargetTask rootId
      [] -> case taskProjectId taskVal of
        Just projectId -> Right $ MemoryTargetProject projectId
        Nothing -> Left "Could not determine a top-level task or project for the notes memory; notes are never linked directly to subtasks."


taskFinishNotesMemoryBody :: Text -> MemoryTarget -> Text -> Maybe [Text] -> Value
taskFinishNotesMemoryBody wsId target notes mTags = object $
  [ "workspace_id" .= wsId
  , "content"      .= notes
  , "memory_type"  .= ("long_term" :: Text)
  , "importance"   .= (6 :: Int)
  , "source"       .= ("inferred" :: Text)
  , "tags"         .= maybe ["task-notes" :: Text] id mTags
  ] <> memoryTargetPairs target


memoryTargetPairs :: MemoryTarget -> [Pair]
memoryTargetPairs = \case
  MemoryTargetProject pid -> ["project_id" .= pid]
  MemoryTargetTask tid    -> ["task_id" .= tid]


taskIdValue :: Value -> Maybe UUID
taskIdValue = objectUUIDField "id"


taskParentId :: Value -> Maybe UUID
taskParentId = objectUUIDField "parent_id"


taskProjectId :: Value -> Maybe UUID
taskProjectId = objectUUIDField "project_id"


objectUUIDField :: Key -> Value -> Maybe UUID
objectUUIDField key value = objectTextField key value >>= UUID.fromText


uniqueUUIDs :: [UUID] -> [UUID]
uniqueUUIDs = foldr (\uid acc -> if uid `elem` acc then acc else uid : acc) []


objectTextField :: Key -> Value -> Maybe Text
objectTextField key = \case
  Object o -> case KM.lookup key o of
    Just (String value) -> Just value
    _                   -> Nothing
  _ -> Nothing


firstRawAuthError :: [Either Text a] -> Maybe Text
firstRawAuthError [] = Nothing
firstRawAuthError (Left err : rest)
  | rawErrorCode err `elem` [Just "AUTH_REQUIRED", Just "AUTH_FORBIDDEN"] = Just err
  | otherwise = firstRawAuthError rest
firstRawAuthError (Right _ : rest) = firstRawAuthError rest


rawAuthErrorToMcp :: Either Text a -> Maybe Value
rawAuthErrorToMcp result = mcpErrorCodeFromRaw "AUTH_FAILED" <$> firstRawAuthError [result]


taskStartParentGatePreflight :: Manager -> String -> Maybe Text -> UUID -> Value -> IO (Maybe Value)
taskStartParentGatePreflight mgr base mApiKey tid taskVal =
  case taskParentId taskVal of
    Nothing -> pure Nothing
    Just parentId -> do
      parentResult <- rawGetJSON mgr base mApiKey ("/api/v1/tasks/" <> uuidPath parentId)
      case rawAuthErrorToMcp parentResult of
        Just authErr -> pure (Just authErr)
        Nothing -> case parentResult of
          Left err -> pure . Just $ mcpErrorCodeFromRaw "TASK_PARENT_LOOKUP_FAILED" err
          Right parentVal -> pure $ taskStartParentGateError tid taskVal parentVal


taskStartDependencyPreflight :: Manager -> String -> Maybe Text -> UUID -> Value -> IO (Maybe Value)
taskStartDependencyPreflight mgr base mApiKey tid taskVal = do
  overviewResult <- rawGetJSON mgr base mApiKey ("/api/v1/tasks/" <> uuidPath tid <> "/overview" <>
    buildQuery [("extra_context", Just "false")])
  case rawAuthErrorToMcp overviewResult of
    Just authErr -> pure (Just authErr)
    Nothing -> case overviewResult of
      Left err -> pure . Just $ mcpErrorCodeFromRaw "TASK_OVERVIEW_FAILED" err
      Right overviewVal -> do
        depTaskResult <- fetchTaskStartDependencyTasks mgr base mApiKey (taskOverviewDependencies overviewVal)
        case depTaskResult of
          Left depErr -> pure (Just depErr)
          Right depTasks -> do
            let blockers = taskStartOpenDependencyBlockers (taskOverviewDependencies overviewVal) depTasks
            if null blockers
              then pure Nothing
              else do
                (alternatives, mAlternativesError) <- taskStartReadyAlternatives mgr base mApiKey taskVal
                pure . Just $ taskStartDependencyBlockError tid blockers alternatives mAlternativesError


fetchTaskStartDependencyTasks :: Manager -> String -> Maybe Text -> [Value] -> IO (Either Value [Value])
fetchTaskStartDependencyTasks _ _ _ [] = pure (Right [])
fetchTaskStartDependencyTasks mgr base mApiKey (depSummary : rest) =
  case objectUUIDField "id" depSummary of
    Nothing -> fetchTaskStartDependencyTasks mgr base mApiKey rest
    Just depId -> do
      depResult <- rawGetJSON mgr base mApiKey ("/api/v1/tasks/" <> uuidPath depId)
      case rawAuthErrorToMcp depResult of
        Just authErr -> pure (Left authErr)
        Nothing -> case depResult of
          Left err -> pure . Left $ mcpErrorCodeFromRaw "TASK_DEPENDENCY_LOOKUP_FAILED" err
          Right depTask -> do
            restResult <- fetchTaskStartDependencyTasks mgr base mApiKey rest
            pure $ (depTask :) <$> restResult


taskStartReadyAlternatives :: Manager -> String -> Maybe Text -> Value -> IO ([Value], Maybe Text)
taskStartReadyAlternatives mgr base mApiKey taskVal = do
  projectIdResult <- taskStartProjectId mgr base mApiKey taskVal
  case projectIdResult of
    Left err -> pure ([], Just err)
    Right Nothing -> pure ([], Nothing)
    Right (Just projectId) -> do
      alternativesResult <- rawGetJSON mgr base mApiKey ("/api/v1/projects/" <> uuidPath projectId <> "/next-tasks" <>
        buildQuery [("limit", Just "5")])
      case alternativesResult of
        Right alternatives -> pure (taskStartAlternativeCandidates alternatives, Nothing)
        Left err           -> pure ([], Just err)


taskStartProjectId :: Manager -> String -> Maybe Text -> Value -> IO (Either Text (Maybe UUID))
taskStartProjectId mgr base mApiKey taskVal =
  case taskProjectId taskVal of
    Just projectId -> pure (Right (Just projectId))
    Nothing -> case taskParentId taskVal of
      Nothing -> pure (Right Nothing)
      Just parentId -> do
        ancestorsResult <- fetchTaskAncestors mgr base mApiKey parentId []
        pure $ case ancestorsResult of
          Right ancestors -> Right (taskStartProjectIdFromSelfOrAncestors taskVal ancestors)
          Left err        -> Left (mcpErrorValueText err)


taskStartProjectIdFromSelfOrAncestors :: Value -> [Value] -> Maybe UUID
taskStartProjectIdFromSelfOrAncestors taskVal ancestors =
  taskProjectId taskVal <|> listToMaybe (mapMaybe taskProjectId ancestors)


taskStartParentGateError :: UUID -> Value -> Value -> Maybe Value
taskStartParentGateError tid taskVal parentVal =
  case taskParentId taskVal of
    Nothing -> Nothing
    Just parentId
      | taskStatus parentVal == Just "in_progress" -> Nothing
      | otherwise -> Just $ taskStartError
          "lifecycle_conflict"
          "TASK_SUBTASK_START_BLOCKED"
          "Cannot start this subtask because its parent task is not in progress."
          [ "task_id" .= tid
          , "parent_task_id" .= parentId
          , "parent_task" .= compactTaskSummary parentVal
          , "status_unchanged" .= True
          ]


taskStartOpenDependencyBlockers :: [Value] -> [Value] -> [Value]
taskStartOpenDependencyBlockers dependencySummaries dependencyTasks =
  [ taskStartDependencyBlockerSummary depSummary depTask
  | depSummary <- dependencySummaries
  , Just depId <- [objectUUIDField "id" depSummary]
  , depTask <- dependencyTasks
  , taskIdValue depTask == Just depId
  , taskIsOpen depTask
  ]


taskStartDependencyBlockerSummary :: Value -> Value -> Value
taskStartDependencyBlockerSummary depSummary depTask = object $
  [ "id" .= depId
  | Just depId <- [taskIdValue depTask]
  ] ++
  [ "title" .= title
  | Just title <- [taskTitle depTask <|> objectTextField "name" depSummary]
  ] ++
  [ "status" .= status
  | Just status <- [taskStatus depTask]
  ]


taskStartDependencyBlockError :: UUID -> [Value] -> [Value] -> Maybe Text -> Value
taskStartDependencyBlockError tid blockers alternatives mAlternativesError =
  let blockerCount = length blockers
      alternativeCount = length alternatives
      message = "Cannot start this task while "
        <> pluralCount blockerCount "dependency is" "dependencies are"
        <> " still open."
      action
        | Just _ <- mAlternativesError = "Finish or cancel the dependency blockers first; ready alternatives could not be loaded."
        | alternativeCount == 0 = "Finish or cancel the dependency blockers first; no ready alternatives were found in the same project."
        | otherwise = "Work on one of the ready alternatives, or finish/cancel the dependency blockers first."
  in taskStartError
      "workflow_conflict"
      "TASK_START_DEPENDENCY_BLOCKED"
      action
      ([ "task_id" .= tid
       , "reason" .= message
       , "blocker_count" .= blockerCount
       , "blockers" .= blockers
       , "ready_alternatives" .= alternatives
       , "status_unchanged" .= True
       ] ++
       [ "alternatives_unavailable" .= True
       | Just _ <- [mAlternativesError]
       ] ++
       [ "alternatives_error" .= rawMcpErrorDisplayText err
       | Just err <- [mAlternativesError]
       ])


taskStartError :: Text -> Text -> Text -> [Pair] -> Value
taskStartError errType code msg detailPairs = object
  [ "isError" .= True
  , "content" .= [object
      [ "type" .= ("text" :: Text)
      , "text" .= ("[" <> code <> "] " <> msg)
      ]]
  , "error" .= object
      ([ "type" .= errType
       , "code" .= code
       , "message" .= msg
       ] <> detailPairs)
  ]


taskOverviewDependencies :: Value -> [Value]
taskOverviewDependencies overviewVal = objectArrayField "dependencies" overviewVal


taskStartAlternativeCandidates :: Value -> [Value]
taskStartAlternativeCandidates candidatesVal =
  [ compactNextTaskCandidateSummary candidate
  | candidate <- objectArrayValue candidatesVal
  ]


taskIsOpen :: Value -> Bool
taskIsOpen taskVal = taskStatus taskVal `elem` map Just ["todo", "in_progress", "blocked"]


taskStatus :: Value -> Maybe Text
taskStatus = objectTextField "status"


taskTitle :: Value -> Maybe Text
taskTitle = objectTextField "title"


objectArrayField :: Key -> Value -> [Value]
objectArrayField key = \case
  Object o -> case KM.lookup key o of
    Just (Array arr) -> toList arr
    _                -> []
  _ -> []


objectArrayValue :: Value -> [Value]
objectArrayValue = \case
  Array arr -> toList arr
  _         -> []


mcpErrorValueText :: Value -> Text
mcpErrorValueText value@(Object o) =
  case KM.lookup "content" o of
    Just (Array contentItems) ->
      case listToMaybe [textValue | Object item <- toList contentItems, Just (String textValue) <- [KM.lookup "text" item]] of
        Just textValue -> textValue
        Nothing        -> fallback
    _ -> fallback
  where
    fallback = decodeUtf8 (encode value)
mcpErrorValueText value = decodeUtf8 (encode value)


rawMcpErrorDisplayText :: Text -> Text
rawMcpErrorDisplayText raw =
  case mcpErrorFromRawPayload raw of
    Just structuredError -> mcpErrorValueText structuredError
    Nothing              -> stripRawErrorCode raw


pluralCount :: Int -> Text -> Text -> Text
pluralCount 1 singular _ = T.pack (show (1 :: Int)) <> " " <> singular
pluralCount n _ plural = T.pack (show n) <> " " <> plural


taskStartUpdateError :: Either Text Value -> Maybe Value
taskStartUpdateError = \case
  Left err -> Just $ mcpErrorCodeFromRaw "TASK_START_FAILED" err
  Right _  -> Nothing

------------------------------------------------------------------------
-- Utility
------------------------------------------------------------------------

t :: Text -> Text
t = Prelude.id

-- | Current tool API version. Bump this when tool schemas change
-- (new required fields, renamed tools, changed semantics).
-- Adding new optional fields or new tools does not require a bump.
toolApiVersion :: Text
toolApiVersion = "0.7.0"

mkTool :: Text -> Text -> Value -> Value
mkTool name desc inputSchema = object
  [ "name"        .= name
  , "description" .= desc
  , "inputSchema" .= inputSchema
  , "annotations" .= object
      [ "version" .= toolApiVersion ]
  ]

prop :: Text -> Text -> Value
prop ty desc = object ["type" .= ty, "description" .= desc]

propMaxLength :: Text -> Text -> Int -> Value
propMaxLength ty desc maxLen = object
    [ "type" .= ty
    , "description" .= desc
    , "maxLength" .= maxLen
    ]

propEnum :: Text -> Text -> [Text] -> Value
propEnum ty desc vals = object ["type" .= ty, "description" .= desc, "enum" .= vals]

uuidPath :: UUID -> String
uuidPath = UUID.toString

httpErrorCode :: Int -> Text
httpErrorCode code = case code of
  401 -> "AUTH_REQUIRED"
  403 -> "AUTH_FORBIDDEN"
  404 -> "NOT_FOUND"
  429 -> "RATE_LIMITED"
  _   -> "HTTP_" <> T.pack (show code)


httpErrorMessage :: Int -> BL.ByteString -> Text
httpErrorMessage code body = case code of
  401 -> "hmem-server rejected the request as unauthenticated. Configure MCP auth forwarding with --auth-token, HMEM_MCP_AUTH_TOKEN, HMEM_AUTH_TOKEN, or local legacy auth.api_key where appropriate. Server response omitted for safety."
  403 -> "hmem-server rejected the request as unauthorized for the current principal or workspace. Check deployed PAT/service-token grants or workspace context. Server response omitted for safety."
  _   -> responseText
  where
    responseText = sanitizeServerResponse body


connectionErrorMessage :: Text
connectionErrorMessage =
  "Could not connect to hmem-server. Check the server URL, network/TLS settings, and MCP auth configuration."


sanitizeServerResponse :: BL.ByteString -> Text
sanitizeServerResponse body =
  let compact = redactSensitiveResponseText $ T.unwords $ T.words $ decodeUtf8 body
      maxChars = 512
  in if T.length compact > maxChars
    then T.take maxChars compact <> "…"
    else compact


redactSensitiveResponseText :: Text -> Text
redactSensitiveResponseText text
  | containsSensitiveMarker text = "[REDACTED]"
  | otherwise = text
  where
    containsSensitiveMarker value =
      let lowered = T.toLower value
      in any (`T.isInfixOf` lowered)
        [ "authorization:"
        , "authorization="
        , "\"authorization\""
        , "'authorization'"
        , "authorization"
        , "bearer"
        , "set-cookie"
        , "cookie"
        , "password"
        , "client_secret"
        , "client-secret"
        , "x-api-key"
        , "access_token="
        , "access_token:"
        , "\"access_token\""
        , "'access_token'"
        , "refresh_token="
        , "refresh_token:"
        , "\"refresh_token\""
        , "'refresh_token'"
        , "id_token="
        , "id_token:"
        , "\"id_token\""
        , "'id_token'"
        , "api_key="
        , "api_key:"
        , "\"api_key\""
        , "'api_key'"
        , "apikey="
        , "apikey:"
        , "\"apikey\""
        , "'apikey'"
        , "token"
        , "token:"
        , "token="
        , "\"token\":"
        , "'token':"
        ]

-- | Build a query string from optional key-value pairs.
-- Returns "" if all values are Nothing, otherwise "?k1=v1&k2=v2..."
-- Values are percent-encoded to prevent injection of special characters.
buildQuery :: [(String, Maybe String)] -> String
buildQuery params = case [(k, v) | (k, Just v) <- params] of
  [] -> ""
  ps -> "?" <> intercalate "&" [k <> "=" <> encodeQueryValue v | (k, v) <- ps]

encodeQueryValue :: String -> String
encodeQueryValue = BS8.unpack . urlEncode True . TE.encodeUtf8 . T.pack

decodeUtf8 :: BL.ByteString -> Text
decodeUtf8 = TE.decodeUtf8 . BL.toStrict
