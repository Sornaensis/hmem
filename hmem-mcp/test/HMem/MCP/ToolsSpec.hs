{-# OPTIONS_GHC -Wno-x-partial #-}

module HMem.MCP.ToolsSpec (spec) where

import Data.Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Control.Concurrent.STM (newTVarIO)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
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
      toolSchemaProperties "search" `shouldNotContain` ["workspace_id", "search_language", "offset", "min_access_count", "min_importance", "category_id", "pinned_only", "task_priority"]

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

    it "parses overview and finish tools without removed boolean/tag knobs" $ do
      parseToolCall "project_overview" (object ["project_id" .= testUUID]) `shouldBe` Right (ProjectOverviewCall parsedUUID)
      parseToolCall "task_overview" (object ["task_id" .= testUUID]) `shouldBe` Right (TaskOverviewCall parsedUUID)
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
  where
    isUnknownTool expected result = case result of
      Left msg -> msg == "Unknown tool: " <> T.unpack expected
      Right _ -> False
