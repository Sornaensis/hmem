{-# OPTIONS_GHC -Wno-x-partial #-}

module HMem.MCP.ToolsSpec (spec) where

import Data.Aeson
import Data.Aeson.Types (Pair)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Lazy qualified as BL
import Control.Exception (IOException, try)
import Control.Concurrent.STM (newTVarIO)
import Data.Foldable (toList)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.UUID qualified as UUID
import Test.Hspec

import HMem.MCP.Tools
import HMem.MCP.Server (injectWorkspaceContext)
import HMem.Types

testUUID :: Text
testUUID = "aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee"

testUUID2 :: Text
testUUID2 = "11111111-2222-3333-4444-555555555555"

parsedUUID :: UUID.UUID
parsedUUID = read "aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee"

parsedUUID2 :: UUID.UUID
parsedUUID2 = read "11111111-2222-3333-4444-555555555555"

parsedUUID3 :: UUID.UUID
parsedUUID3 = read "22222222-3333-4444-5555-666666666666"

slimToolNames :: [Text]
slimToolNames =
  [ "set_workspace"
  , "get_workspace"
  , "workspace_list"
  , "workspace_register"
  , "search"
  , "memory_create"
  , "memory_get"
  , "memory_update"
  , "memory_link"
  , "link_memory"
  , "project_create"
  , "project_update"
  , "project_overview"
  , "project_next_tasks"
  , "project_spec"
  , "project_archive"
  , "task_create"
  , "task_update"
  , "task_overview"
  , "context_get"
  , "task_dependency"
  , "task_start"
  , "task_finish"
  ]

removedToolNames :: [Text]
removedToolNames =
  [ "memory_search"
  , "memory_list"
  , "project_list"
  , "project_get"
  , "task_list"
  , "task_get"
  , "memory_set_tags"
  , "list_entity_memories"
  , "workspace_get"
  , "workspace_update"
  , "entity_lifecycle"
  , "category"
  , "saved_view"
  , "cleanup_run"
  ]

toolNames :: [Text]
toolNames = mapMaybe toolName toolDefinitions
  where
    toolName (Object o) = case KM.lookup (Key.fromText "name") o of
      Just (String name) -> Just name
      _ -> Nothing
    toolName _ = Nothing

toolSchemaProperties :: Text -> [Text]
toolSchemaProperties name = case [schema | Object tool <- toolDefinitions
                                         , KM.lookup (Key.fromText "name") tool == Just (String name)
                                         , Just schema <- [KM.lookup (Key.fromText "inputSchema") tool]] of
  Object schema : _ -> case KM.lookup (Key.fromText "properties") schema of
    Just (Object props) -> Key.toText <$> KM.keys props
    _ -> []
  _ -> []

spec :: Spec
spec = do
  describe "toolDefinitions" $ do
    it "exposes exactly the slim default MCP tool surface" $ do
      toolNames `shouldBe` slimToolNames

    it "does not expose removed tools" $ do
      toolNames `shouldNotSatisfy` any (`elem` removedToolNames)

    it "omits noisy workspace and batch inputs from retained schemas" $ do
      toolSchemaProperties "memory_create" `shouldNotContain` ["workspace_id", "metadata", "items", "fts_language", "source", "confidence", "expires_at", "pinned"]
      toolSchemaProperties "memory_update" `shouldNotContain` ["metadata", "items", "source", "confidence", "expires_at"]
      toolSchemaProperties "project_create" `shouldNotContain` ["workspace_id", "metadata"]
      toolSchemaProperties "project_update" `shouldNotContain` ["metadata", "items"]
      toolSchemaProperties "task_create" `shouldNotContain` ["workspace_id", "metadata"]
      toolSchemaProperties "task_update" `shouldNotContain` ["metadata", "items"]
      toolSchemaProperties "project_overview" `shouldContain` ["include_descriptions"]
      toolSchemaProperties "task_overview" `shouldContain` ["include_description"]
      toolSchemaProperties "search" `shouldNotContain` ["workspace_id", "search_language", "offset", "min_access_count", "min_importance", "category_id", "pinned_only", "task_priority"]

  describe "MCP compact response contract" $ do
    it "maps every retained slim MCP tool" $ do
      doc <- readContractDoc
      mapM_ (\name -> doc `shouldContain` ("| `" <> T.unpack name <> "` |")) slimToolNames

    it "documents the DTO vocabulary and default omission policy" $ do
      doc <- readContractDoc
      mapM_ (`shouldContainText` doc)
        [ "MutationAck"
        , "changed_fields"
        , "EntitySummary"
        , "MemoryDetail"
        , "SearchRow"
        , "OverviewSummary"
        , "ContextSummary"
        , "GraphSummary"
        , "NextTaskCandidateSummary"
        , "DependencyEffectSummary"
        , "DependencyMutationSummary"
        , "TaskDependencySummary"
        , "ConnectedMemorySummary"
        , "LinkedMemorySummary"
        , "WorkflowSummary"
        , "StructuredError"
        , "timestamps"
        , "workspace_id"
        , "empty `metadata`"
        , "null fields"
        , "full memory `content` and full project/task descriptions except detail tools"
        , "dependency/memory counts"
        ]

  describe "compact response shapers" $ do
    it "builds memory summaries without workspace, timestamps, metadata, or content" $ do
      compactMemorySummary fullMemoryValue `shouldBe` object
        [ "id" .= parsedUUID
        , "memory_type" .= ("long_term" :: Text)
        , "importance" .= (7 :: Int)
        , "tags" .= (["contract"] :: [Text])
        , "pinned" .= True
        ]

    it "allows explicit memory detail to keep full content while trimming generic noise" $ do
      let wrapped = mcpResultWith compactMemoryDetail (encode fullMemoryValue)
      case mcpTextValue wrapped of
        Just detailed -> do
          jsonField "content" detailed `shouldBe` Just (String "full memory content")
          jsonField "metadata" detailed `shouldBe` Just (object ["kept" .= True])
          jsonField "workspace_id" detailed `shouldBe` Nothing
          jsonField "created_at" detailed `shouldBe` Nothing
        Nothing -> expectationFailure $ "Expected MCP JSON text, got: " <> show wrapped

    it "shapes unified search rows into compact summaries" $ do
      let shaped = compactSearchResults $ object
            [ "memories" .= [fullMemoryValue]
            , "projects" .=
                [ object
                    [ "project" .= fullProjectValue
                    , "linked_memories" .= [linkedMemoryValue]
                    ]
                ]
            , "tasks" .=
                [ object
                    [ "task" .= fullTaskValue
                    , "linked_memories" .= [linkedMemoryValue]
                    ]
                ]
            ]
      jsonField "memories" shaped `shouldSatisfy` arrayLength 1
      jsonField "projects" shaped `shouldSatisfy` arrayLength 1
      jsonField "tasks" shaped `shouldSatisfy` arrayLength 1
      let memorySummary = firstArrayItem "memories" shaped
          projectSummary = firstArrayItem "projects" shaped >>= jsonField "project"
          taskSummary = firstArrayItem "tasks" shaped >>= jsonField "task"
      (memorySummary >>= jsonField "id") `shouldBe` Just (String testUUID)
      (memorySummary >>= jsonField "memory_type") `shouldBe` Just (String "long_term")
      (memorySummary >>= jsonField "importance") `shouldBe` Just (Number 7)
      (memorySummary >>= jsonField "tags") `shouldBe` Just (toJSON (["contract"] :: [Text]))
      (memorySummary >>= jsonField "content") `shouldBe` Nothing
      (memorySummary >>= jsonField "content_preview") `shouldBe` Nothing
      (projectSummary >>= jsonField "id") `shouldBe` Just (String testUUID)
      (projectSummary >>= jsonField "name") `shouldBe` Just (String "Project")
      (projectSummary >>= jsonField "status") `shouldBe` Just (String "active")
      (projectSummary >>= jsonField "priority") `shouldBe` Just (Number 8)
      (taskSummary >>= jsonField "id") `shouldBe` Just (String testUUID)
      (taskSummary >>= jsonField "title") `shouldBe` Just (String "Task")
      (taskSummary >>= jsonField "status") `shouldBe` Just (String "todo")
      (taskSummary >>= jsonField "priority") `shouldBe` Just (Number 9)
      (taskSummary >>= jsonField "project_id") `shouldBe` Just (String "22222222-3333-4444-5555-666666666666")
      show shaped `shouldNotContain` "workspace_id"
      show shaped `shouldNotContain` "created_at"
      show shaped `shouldNotContain` "metadata"
      show shaped `shouldNotContain` "full memory content"
      show shaped `shouldNotContain` "full project description"
      show shaped `shouldNotContain` "full task description"
      show shaped `shouldNotContain` "linked full content should be omitted"

    it "wraps task mutations as acknowledgements with dependency effects" $ do
      let ack = compactTaskMutationAck "updated" $ object
            [ "id" .= parsedUUID
            , "workspace_id" .= parsedUUID2
            , "title" .= ("Task" :: Text)
            , "status" .= ("blocked" :: Text)
            , "priority" .= (9 :: Int)
            , "created_at" .= ("2026-05-20T00:00:00Z" :: Text)
            , "dependency_effects" .=
                [ object
                    [ "task" .= fullTaskValue
                    , "previous_status" .= ("todo" :: Text)
                    , "current_status" .= ("blocked" :: Text)
                    , "auto_blocked" .= True
                    , "open_dependency_count" .= (1 :: Int)
                    , "reason" .= ("dependency_added" :: Text)
                    ]
                ]
            ]
      jsonField "ok" ack `shouldBe` Just (Bool True)
      jsonField "action" ack `shouldBe` Just (String "updated")
      jsonField "entity_type" ack `shouldBe` Just (String "task")
      jsonField "summary" ack `shouldSatisfy` hasObjectField "title"
      jsonField "dependency_effects" ack `shouldSatisfy` arrayLength 1
      jsonField "workspace_id" ack `shouldBe` Nothing
      jsonField "created_at" ack `shouldBe` Nothing

    it "adds changed_fields to update acknowledgements without exposing full entity fields" $ do
      let ack = addChangedFields ["title", "status"] (compactTaskMutationAck "updated" fullTaskValue)
      jsonField "changed_fields" ack `shouldBe` Just (toJSON (["title", "status"] :: [Text]))
      show ack `shouldNotContain` "full task description"
      show ack `shouldNotContain` "created_at"

    it "wraps dependency mutations as acknowledgements with compact affected tasks" $ do
      let ack = compactDependencyMutationAck "add" $ object
            [ "action" .= ("add" :: Text)
            , "task_id" .= parsedUUID
            , "depends_on_id" .= parsedUUID2
            , "affected_tasks" .=
                [ object
                    [ "task" .= fullTaskValue
                    , "previous_status" .= ("todo" :: Text)
                    , "current_status" .= ("blocked" :: Text)
                    , "auto_blocked" .= True
                    , "open_dependency_count" .= (1 :: Int)
                    , "reason" .= ("open_dependency_added" :: Text)
                    ]
                ]
            ]
      jsonField "ok" ack `shouldBe` Just (Bool True)
      jsonField "action" ack `shouldBe` Just (String "add")
      jsonField "entity_type" ack `shouldBe` Just (String "task_dependency")
      jsonField "task_id" ack `shouldBe` Just (String testUUID)
      jsonField "depends_on_id" ack `shouldBe` Just (String testUUID2)
      jsonField "affected_tasks" ack `shouldSatisfy` arrayLength 1
      show ack `shouldNotContain` "full task description"
      show ack `shouldNotContain` "created_at"

    it "preserves mutation-specific memory targets and empty tag replacements" $ do
      let createAck = compactMemoryMutationAckWithTargets "created" (Just parsedUUID2) (Just parsedUUID3) fullMemoryValue
          updateAck = compactMemoryMutationAckWithTags "updated" (Just []) fullMemoryValue
      jsonField "project_id" createAck `shouldBe` Just (String testUUID2)
      jsonField "task_id" createAck `shouldBe` Just (String "22222222-3333-4444-5555-666666666666")
      jsonField "tags" updateAck `shouldBe` Just (Array mempty)
      let createSummary = jsonField "summary" createAck
          updateSummary = jsonField "summary" updateAck
      (createSummary >>= jsonField "content") `shouldBe` Nothing
      (createSummary >>= jsonField "content_preview") `shouldBe` Nothing
      (updateSummary >>= jsonField "content") `shouldBe` Nothing
      (updateSummary >>= jsonField "content_preview") `shouldBe` Nothing
      show createAck `shouldNotContain` "full memory content"
      show updateAck `shouldNotContain` "full memory content"

    it "keeps workflow finish/archive acknowledgements to ids and statuses" $ do
      let finishAck = compactTaskFinishAckWithNotes "finished" (Just fullMemoryValue) fullTaskValue
          finishWithoutNotes = compactTaskFinishAckWithNotes "finished" Nothing fullTaskValue
          archiveAck = compactProjectArchiveAck (Just fullMemoryValue) fullProjectValue
          archiveWithoutSummary = compactProjectArchiveAck Nothing fullProjectValue
      jsonField "task_id" finishAck `shouldBe` Just (String testUUID)
      jsonField "status" finishAck `shouldBe` Just (String "todo")
      jsonField "notes_memory_id" finishAck `shouldBe` Just (String testUUID)
      jsonField "notes_memory" finishAck `shouldBe` Nothing
      jsonField "notes_memory_id" finishWithoutNotes `shouldBe` Nothing
      jsonField "project_id" archiveAck `shouldBe` Just (String testUUID)
      jsonField "status" archiveAck `shouldBe` Just (String "active")
      jsonField "summary_memory_id" archiveAck `shouldBe` Just (String testUUID)
      jsonField "summary_memory" archiveAck `shouldBe` Nothing
      jsonField "summary_memory_id" archiveWithoutSummary `shouldBe` Nothing
      show finishAck `shouldNotContain` "full memory content"
      show archiveAck `shouldNotContain` "full memory content"

    it "compacts project_spec workflow output for many created tasks" $ do
      let manyTasks = replicate 25 fullTaskValue
          shaped = compactProjectSpecSummary $ object
            [ "project" .= fullProjectValue
            , "tasks" .= manyTasks
            , "tasks_failed" .= (0 :: Int)
            ]
          firstTask = firstArrayItem "tasks_created" shaped
      jsonField "ok" shaped `shouldBe` Just (Bool True)
      jsonField "action" shaped `shouldBe` Just (String "created")
      jsonField "entity_type" shaped `shouldBe` Just (String "project_spec")
      jsonField "project_id" shaped `shouldBe` Just (String testUUID)
      jsonField "name" shaped `shouldBe` Just (String "Project")
      jsonField "tasks_created" shaped `shouldSatisfy` arrayLength 25
      jsonField "tasks_failed" shaped `shouldBe` Nothing
      (firstTask >>= jsonField "id") `shouldBe` Just (String testUUID)
      (firstTask >>= jsonField "title") `shouldBe` Just (String "Task")
      (firstTask >>= jsonField "priority") `shouldBe` Just (Number 9)
      (firstTask >>= jsonField "description") `shouldBe` Nothing
      jsonField "project" shaped `shouldBe` Nothing
      show shaped `shouldNotContain` "workspace_id"
      show shaped `shouldNotContain` "full project description"
      show shaped `shouldNotContain` "full task description"

    it "compacts memory link lists to graph edges without endpoint content or timestamps" $ do
      let shaped = compactMemoryLinksList $ toJSON
            [ object
                [ "source_id" .= parsedUUID
                , "target_id" .= parsedUUID2
                , "relation_type" .= ("related" :: Text)
                , "strength" .= (0.75 :: Double)
                , "created_at" .= ("2026-05-20T00:00:00Z" :: Text)
                , "source_memory" .= fullMemoryValue
                , "target_memory" .= fullMemoryValue
                ]
            ]
      jsonField "links" shaped `shouldSatisfy` arrayLength 1
      show shaped `shouldContain` "source_id"
      show shaped `shouldContain` "target_id"
      show shaped `shouldNotContain` "created_at"
      show shaped `shouldNotContain` "full memory content"
      show shaped `shouldNotContain` "source_memory"
      show shaped `shouldNotContain` "target_memory"

    it "compacts next-task candidates for task_start error alternatives" $ do
      let candidate = compactNextTaskCandidateSummary $ object
            [ "task" .= fullTaskValue
            , "dependency_blocked" .= False
            , "completion_gated" .= False
            , "open_descendant_count" .= (0 :: Int)
            , "open_dependency_count" .= (0 :: Int)
            ]
      jsonField "task" candidate `shouldSatisfy` hasObjectField "title"
      jsonField "dependency_blocked" candidate `shouldBe` Just (Bool False)
      show candidate `shouldNotContain` "full task description"
      show candidate `shouldNotContain` "workspace_id"
      show candidate `shouldNotContain` "open_descendant_count"

    it "compacts overview and context payloads while preserving actionable readiness" $ do
      let overviewInput = object
            [ "project" .= fullProjectValue
            , "tasks" .= [fullTaskValue]
            , "subprojects" .= [fullProjectValue]
            , "connected_memories" .= [object ["id" .= parsedUUID, "summary" .= ("Memory" :: Text), "scope" .= ("project" :: Text)]]
            , "readiness_rollup" .= object
                [ "completion_ready" .= False
                , "open_task_count" .= (2 :: Int)
                , "done_task_count" .= (0 :: Int)
                ]
            ]
          overview = compactProjectOverview overviewInput
          overviewWithDescriptions = compactProjectOverviewWithDescriptions overviewInput
          taskOverviewInput = object
            [ "task" .= fullTaskValue
            , "dependencies" .= [object ["id" .= parsedUUID2, "title" .= ("Dependency" :: Text), "status" .= ("todo" :: Text)]]
            , "connected_memories" .= [object ["id" .= parsedUUID, "summary" .= ("Task memory" :: Text), "scope" .= ("task" :: Text)]]
            , "readiness_rollup" .= object ["completion_ready" .= True]
            ]
          taskOverview = compactTaskOverview taskOverviewInput
          taskOverviewWithDescription = compactTaskOverviewWithDescription taskOverviewInput
          contextInfo = compactContextInfo $ object
            [ "task" .= fullTaskValue
            , "detail_level" .= ("medium" :: Text)
            , "task_memories" .= [object ["id" .= parsedUUID, "summary" .= ("Task memory" :: Text), "scope" .= ("task" :: Text)]]
            , "project_memories" .= ([] :: [Value])
            , "workspace_memories" .= ([] :: [Value])
            ]
          taskStart = compactTaskStartSuccess $ object
            [ "task" .= fullTaskValue
            , "detail_level" .= ("light" :: Text)
            , "task_memories" .= ([] :: [Value])
            , "project_memories" .= ([] :: [Value])
            , "workspace_memories" .= ([] :: [Value])
            ]
      (jsonField "project" overview >>= jsonField "description") `shouldBe` Nothing
      (jsonField "task" taskOverview >>= jsonField "description") `shouldBe` Nothing
      (jsonField "task" contextInfo >>= jsonField "description") `shouldBe` Nothing
      (jsonField "task" taskStart >>= jsonField "description") `shouldBe` Nothing
      (jsonField "project" overviewWithDescriptions >>= jsonField "description") `shouldBe` Just (String "full project description")
      (jsonField "task" taskOverviewWithDescription >>= jsonField "description") `shouldBe` Just (String "full task description")
      jsonField "started" taskStart `shouldBe` Just (Bool True)
      jsonField "result" taskStart `shouldBe` Just (String "started")
      jsonField "task_id" taskStart `shouldBe` Just (String testUUID)
      jsonField "status" taskStart `shouldBe` Just (String "todo")
      jsonField "tasks" overview `shouldSatisfy` arrayLength 1
      jsonField "dependencies" taskOverview `shouldSatisfy` arrayLength 1
      show overview `shouldNotContain` "workspace_id"
      show overview `shouldNotContain` "dependency_count"
      show overview `shouldNotContain` "full project description"
      show overview `shouldNotContain` "full task description"
      show taskOverview `shouldNotContain` "full task description"
      show contextInfo `shouldNotContain` "full task description"
      show taskStart `shouldNotContain` "full task description"
      show overview `shouldContain` "open_task_count"
      show overview `shouldNotContain` "done_task_count"
      jsonField "detail_level" contextInfo `shouldBe` Just (String "medium")
      jsonField "task_memories" contextInfo `shouldSatisfy` arrayLength 1

  describe "workspace context injection" $ do
    it "creates an arguments object when omitted so queryless tools stay workspace-scoped" $ do
      ctx <- newTVarIO (Just parsedUUID)
      injected <- injectWorkspaceContext ctx (object ["name" .= ("search" :: Text)])
      case injected of
        Object o -> case KM.lookup (Key.fromText "arguments") o of
          Just (Object args) -> KM.lookup (Key.fromText "workspace_id") args `shouldBe` Just (String testUUID)
          other -> expectationFailure $ "Expected injected arguments object, got: " <> show other
        other -> expectationFailure $ "Expected object params, got: " <> show other

  describe "parseToolCall" $ do
    it "parses slim memory_create with explicit target and type" $ do
      let args = object
            [ "workspace_id" .= testUUID
            , "project_id" .= testUUID2
            , "content" .= ("hello" :: Text)
            , "memory_type" .= ("short_term" :: Text)
            ]
      case parseToolCall "memory_create" args of
        Right (MemoryCreate cm) -> do
          cm.workspaceId `shouldBe` parsedUUID
          cm.projectId `shouldBe` Just parsedUUID2
          cm.content `shouldBe` "hello"
          cm.memoryType `shouldBe` ShortTerm
        other -> expectationFailure $ "Expected MemoryCreate, got: " <> show other

    it "parses memory_update with replacement tags folded in" $ do
      let args = object
            [ "memory_id" .= testUUID
            , "summary" .= ("summary" :: Text)
            , "tags" .= (["a", "b"] :: [Text])
            ]
      case parseToolCall "memory_update" args of
        Right (MemoryUpdate mid _um tags) -> do
          mid `shouldBe` parsedUUID
          tags `shouldBe` Just ["a", "b"]
        other -> expectationFailure $ "Expected MemoryUpdate, got: " <> show other

    it "parses singular project memory attach" $ do
      let args = object
            [ "entity_type" .= ("project" :: Text)
            , "entity_id" .= testUUID
            , "action" .= ("link" :: Text)
            , "memory_id" .= testUUID2
            ]
      parseToolCall "link_memory" args `shouldBe` Right (ProjectLinkMem parsedUUID parsedUUID2)

    it "parses overview and finish tools with optional description detail flags" $ do
      parseToolCall "project_overview" (object ["project_id" .= testUUID]) `shouldBe` Right (ProjectOverviewCall parsedUUID False)
      parseToolCall "project_overview" (object ["project_id" .= testUUID, "include_descriptions" .= True]) `shouldBe` Right (ProjectOverviewCall parsedUUID True)
      parseToolCall "task_overview" (object ["task_id" .= testUUID]) `shouldBe` Right (TaskOverviewCall parsedUUID False)
      parseToolCall "task_overview" (object ["task_id" .= testUUID, "include_description" .= True]) `shouldBe` Right (TaskOverviewCall parsedUUID True)
      parseToolCall "task_finish" (object ["task_id" .= testUUID, "status" .= ("done" :: Text), "notes" .= ("done" :: Text)])
        `shouldBe` Right (TaskFinishCall parsedUUID Done (Just "done"))

    it "parses project next-task tool options" $ do
      parseToolCall "project_next_tasks" (object ["project_id" .= testUUID])
        `shouldBe` Right (ProjectNextTasksCall parsedUUID Nothing False)
      parseToolCall "project_next_tasks" (object ["project_id" .= testUUID, "limit" .= (25 :: Int), "include_blocked" .= True])
        `shouldBe` Right (ProjectNextTasksCall parsedUUID (Just 25) True)

    it "parses task_update placement and reorder fields used by MCP move validation" $ do
      let args = object
            [ "task_id" .= testUUID
            , "project_id" .= testUUID2
            , "parent_id" .= testUUID2
            , "priority" .= (9 :: Int)
            ]
      case parseToolCall "task_update" args of
        Right (TaskUpdate tid ut) -> do
          tid `shouldBe` parsedUUID
          ut.projectId `shouldBe` SetTo parsedUUID2
          ut.parentId `shouldBe` SetTo parsedUUID2
          ut.priority `shouldBe` Just 9
        other -> expectationFailure $ "Expected TaskUpdate, got: " <> show other

    it "parses queryless unified search for browsing" $ do
      case parseToolCall "search" (object ["workspace_id" .= testUUID, "entity_types" .= (["project"] :: [Text])]) of
        Right (UnifiedSearch usq) -> do
          usq.workspaceId `shouldBe` Just parsedUUID
          usq.query `shouldBe` Nothing
          usq.entityTypes `shouldBe` Just [SearchProject]
        other -> expectationFailure $ "Expected UnifiedSearch, got: " <> show other

    it "returns unknown-tool errors for removed tools" $ do
      mapM_ (\tool -> parseToolCall tool (object []) `shouldSatisfy` isUnknownTool tool) removedToolNames

  describe "validateToolCall" $ do
    it "accepts queryless unified search after clamping limit" $ do
      let usq = UnifiedSearchQuery
            { workspaceId = Just parsedUUID
            , query = Nothing
            , entityTypes = Just [SearchMemory, SearchProject, SearchTask]
            , searchLanguage = Nothing
            , limit = Just 999
            , offset = Nothing
            , memoryType = Nothing
            , tags = Nothing
            , minImportance = Nothing
            , categoryId = Nothing
            , pinnedOnly = Nothing
            , projectStatus = Nothing
            , taskStatus = Nothing
            , taskPriority = Nothing
            , projectId = Nothing
            }
      case validateToolCall (UnifiedSearch usq) of
        Right (UnifiedSearch usq') -> usq'.limit `shouldBe` Just 200
        other -> expectationFailure $ "Expected UnifiedSearch, got: " <> show other

    it "rejects blank unified search query when provided" $ do
      let usq = UnifiedSearchQuery
            { workspaceId = Just parsedUUID
            , query = Just "   "
            , entityTypes = Nothing
            , searchLanguage = Nothing
            , limit = Nothing
            , offset = Nothing
            , memoryType = Nothing
            , tags = Nothing
            , minImportance = Nothing
            , categoryId = Nothing
            , pinnedOnly = Nothing
            , projectStatus = Nothing
            , taskStatus = Nothing
            , taskPriority = Nothing
            , projectId = Nothing
            }
      validateToolCall (UnifiedSearch usq) `shouldSatisfy` either (const True) (const False)

    it "clamps project next-task limits" $ do
      validateToolCall (ProjectNextTasksCall parsedUUID (Just 999) True)
        `shouldBe` Right (ProjectNextTasksCall parsedUUID (Just 200) True)
      validateToolCall (ProjectNextTasksCall parsedUUID (Just 0) False)
        `shouldBe` Right (ProjectNextTasksCall parsedUUID (Just 1) False)

  describe "task_start preflight helpers" $ do
    it "identifies direct open dependency blockers and ignores closed dependencies" $ do
      let blockers = taskStartOpenDependencyBlockers
            [ dependencySummary parsedUUID "Open dependency"
            , dependencySummary parsedUUID2 "Closed dependency"
            ]
            [ taskValue parsedUUID "Open dependency" "todo" Nothing (Just parsedUUID3)
            , taskValue parsedUUID2 "Closed dependency" "done" Nothing (Just parsedUUID3)
            ]
      blockers `shouldBe`
        [ object
            [ "id" .= parsedUUID
            , "title" .= ("Open dependency" :: Text)
            , "status" .= ("todo" :: Text)
            ]
        ]

    it "returns a parent-not-in-progress error for subtasks and allows running parents" $ do
      let child = taskValue parsedUUID "Child" "todo" (Just parsedUUID2) (Just parsedUUID3)
          parentTodo = taskValue parsedUUID2 "Parent" "todo" Nothing (Just parsedUUID3)
          parentRunning = taskValue parsedUUID2 "Parent" "in_progress" Nothing (Just parsedUUID3)
      taskStartParentGateError parsedUUID child parentTodo `shouldSatisfy` hasErrorCode "TASK_SUBTASK_START_BLOCKED"
      taskStartParentGateError parsedUUID child parentRunning `shouldBe` Nothing

    it "keeps dependency-blocked task_start status unchanged and includes ready alternatives" $ do
      let blockers =
            [ object
                [ "id" .= parsedUUID2
                , "title" .= ("Blocked by" :: Text)
                , "status" .= ("in_progress" :: Text)
                ]
            ]
          alternatives =
            [ object
                [ "task" .= taskValue parsedUUID3 "Ready alternative" "todo" Nothing (Just parsedUUID3)
                , "dependency_blocked" .= False
                ]
            ]
          startError = taskStartDependencyBlockError parsedUUID blockers alternatives Nothing
      startError `shouldSatisfy` hasErrorCodeValue "TASK_START_DEPENDENCY_BLOCKED"
      startError `shouldSatisfy` errorBoolField "status_unchanged" True
      startError `shouldSatisfy` errorArrayMinLength "ready_alternatives" 1

    it "reports no-ready-alternative cases explicitly" $ do
      let blockers =
            [ object
                [ "id" .= parsedUUID2
                , "title" .= ("Blocked by" :: Text)
                , "status" .= ("blocked" :: Text)
                ]
            ]
          startError = taskStartDependencyBlockError parsedUUID blockers [] Nothing
      startError `shouldSatisfy` hasErrorText "no ready alternatives"
      startError `shouldSatisfy` errorArrayMinLength "ready_alternatives" 0

    it "distinguishes unavailable alternatives from empty alternatives" $ do
      let blockers =
            [ object
                [ "id" .= parsedUUID2
                , "title" .= ("Blocked by" :: Text)
                , "status" .= ("todo" :: Text)
                ]
            ]
          startError = taskStartDependencyBlockError parsedUUID blockers [] (Just "[HTTP_500] next-task query failed")
      startError `shouldSatisfy` hasErrorText "could not be loaded"
      startError `shouldSatisfy` errorBoolField "alternatives_unavailable" True
      errorField "alternatives_error" startError `shouldBe` Just (String "next-task query failed")

    it "resolves project alternatives from one-layer subtask ancestors" $ do
      let child = taskValue parsedUUID "Child" "todo" (Just parsedUUID2) Nothing
          parent = taskValue parsedUUID2 "Parent" "in_progress" Nothing (Just parsedUUID3)
      taskStartProjectIdFromSelfOrAncestors child [parent] `shouldBe` Just parsedUUID3
  where
    isUnknownTool expected result = case result of
      Left msg -> msg == "Unknown tool: " <> T.unpack expected
      Right _ -> False


dependencySummary :: UUID.UUID -> Text -> Value
dependencySummary depId name = object
  [ "id" .= depId
  , "name" .= name
  ]


taskValue :: UUID.UUID -> Text -> Text -> Maybe UUID.UUID -> Maybe UUID.UUID -> Value
taskValue tid title status parentId projectId = object
  [ "id" .= tid
  , "title" .= title
  , "status" .= status
  , "parent_id" .= parentId
  , "project_id" .= projectId
  ]


fullMemoryValue :: Value
fullMemoryValue = object
  [ "id" .= parsedUUID
  , "workspace_id" .= parsedUUID2
  , "content" .= ("full memory content" :: Text)
  , "memory_type" .= ("long_term" :: Text)
  , "importance" .= (7 :: Int)
  , "tags" .= (["contract"] :: [Text])
  , "pinned" .= True
  , "metadata" .= object ["kept" .= True]
  , "created_at" .= ("2026-05-20T00:00:00Z" :: Text)
  , "updated_at" .= ("2026-05-20T00:00:00Z" :: Text)
  ]


linkedMemoryValue :: Value
linkedMemoryValue = object
  [ "id" .= parsedUUID
  , "summary" .= ("Linked memory" :: Text)
  , "importance" .= (5 :: Int)
  , "tags" .= (["linked"] :: [Text])
  , "content" .= ("linked full content should be omitted" :: Text)
  ]


fullProjectValue :: Value
fullProjectValue = object
  [ "id" .= parsedUUID
  , "workspace_id" .= parsedUUID2
  , "name" .= ("Project" :: Text)
  , "description" .= ("full project description" :: Text)
  , "status" .= ("active" :: Text)
  , "priority" .= (8 :: Int)
  , "metadata" .= object ([] :: [Pair])
  , "created_at" .= ("2026-05-20T00:00:00Z" :: Text)
  ]


fullTaskValue :: Value
fullTaskValue = object
  [ "id" .= parsedUUID
  , "workspace_id" .= parsedUUID2
  , "project_id" .= parsedUUID3
  , "title" .= ("Task" :: Text)
  , "description" .= ("full task description" :: Text)
  , "status" .= ("todo" :: Text)
  , "priority" .= (9 :: Int)
  , "dependency_count" .= (4 :: Int)
  , "memory_link_count" .= (3 :: Int)
  , "created_at" .= ("2026-05-20T00:00:00Z" :: Text)
  ]


hasErrorCode :: Text -> Maybe Value -> Bool
hasErrorCode code (Just value) = hasErrorCodeValue code value
hasErrorCode _ Nothing = False


hasErrorCodeValue :: Text -> Value -> Bool
hasErrorCodeValue code value = errorField "code" value == Just (String code)


hasErrorText :: Text -> Value -> Bool
hasErrorText needle (Object o) = case KM.lookup (Key.fromText "content") o of
  Just (Array contentItems) -> any contentItemHasText contentItems
  _ -> False
  where
    contentItemHasText (Object item) = case KM.lookup (Key.fromText "text") item of
      Just (String textValue) -> needle `T.isInfixOf` textValue
      _ -> False
    contentItemHasText _ = False
hasErrorText _ _ = False


errorBoolField :: Text -> Bool -> Value -> Bool
errorBoolField field expected value = errorField field value == Just (Bool expected)


errorArrayMinLength :: Text -> Int -> Value -> Bool
errorArrayMinLength field minLength value = case errorField field value of
  Just (Array arr) -> length arr >= minLength
  _ -> False


errorField :: Text -> Value -> Maybe Value
errorField field (Object o) = case KM.lookup (Key.fromText "error") o of
  Just (Object err) -> KM.lookup (Key.fromText field) err
  _ -> Nothing
errorField _ _ = Nothing


jsonField :: Text -> Value -> Maybe Value
jsonField field (Object o) = KM.lookup (Key.fromText field) o
jsonField _ _ = Nothing


hasObjectField :: Text -> Maybe Value -> Bool
hasObjectField field (Just (Object o)) = KM.member (Key.fromText field) o
hasObjectField _ _ = False


arrayLength :: Int -> Maybe Value -> Bool
arrayLength expected (Just (Array arr)) = length arr == expected
arrayLength _ _ = False


firstArrayItem :: Text -> Value -> Maybe Value
firstArrayItem field value = case jsonField field value of
  Just (Array arr) -> listToMaybe (toList arr)
  _ -> Nothing


mcpTextValue :: Value -> Maybe Value
mcpTextValue (Object o) = do
  Array contentItems <- KM.lookup (Key.fromText "content") o
  Object firstItem <- case toList contentItems of
    item : _ -> Just item
    []       -> Nothing
  String textValue <- KM.lookup (Key.fromText "text") firstItem
  decode (BL.fromStrict (TE.encodeUtf8 textValue))
mcpTextValue _ = Nothing


readContractDoc :: IO String
readContractDoc = do
  rootResult <- try @IOException (readFile "mcp-response-contract.md")
  case rootResult of
    Right doc -> pure doc
    Left _    -> readFile "../mcp-response-contract.md"


shouldContainText :: String -> String -> Expectation
shouldContainText needle haystack = haystack `shouldContain` needle
