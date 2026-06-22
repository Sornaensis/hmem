{-# OPTIONS_GHC -Wno-x-partial #-}

module HMem.MCP.ToolsSpec (spec) where

import Data.Aeson
import Data.Aeson.Types (Pair)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as BL
import Control.Exception (IOException, try)
import Control.Concurrent.STM (TVar, newTVarIO)
import Data.Foldable (toList)
import Data.List (sort)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time (UTCTime(..), fromGregorian, secondsToDiffTime)
import Data.UUID qualified as UUID
import Network.HTTP.Client (Manager, defaultManagerSettings, newManager)
import Network.HTTP.Types (hContentType, methodGet, methodPost, methodPut, status200, status400, status404)
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp (testWithApplication)
import Test.Hspec

import HMem.MCP.Tools
import HMem.MCP.Server (JsonRpcRequest(..), encodeStdioResponse, handleRequest, handleStdioLine, injectWorkspaceContext)
import HMem.Config qualified as Config
import HMem.DB.TestHarness (TestEnv(..), withTestEnv)
import HMem.Server.AccessTracker (newAccessTracker)
import HMem.Server.App (mkApp)
import HMem.Server.WebSocket (newWSState)
import HMem.Types

testUUID :: Text
testUUID = "aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee"

testUUID2 :: Text
testUUID2 = "11111111-2222-3333-4444-555555555555"

testUUID3 :: Text
testUUID3 = "22222222-3333-4444-5555-666666666666"

testNotesMemoryUUID :: Text
testNotesMemoryUUID = "33333333-4444-5555-6666-777777777777"

testSummaryMemoryUUID :: Text
testSummaryMemoryUUID = "44444444-5555-6666-7777-888888888888"

testProjectDetailUUID :: Text
testProjectDetailUUID = "55555555-6666-7777-8888-999999999999"

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
  , "project_detail"
  , "project_overview"
  , "project_next_tasks"
  , "project_spec"
  , "project_archive"
  , "task_create"
  , "task_update"
  , "task_detail"
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

toolSchemaRequired :: Text -> [Text]
toolSchemaRequired name = case [schema | Object tool <- toolDefinitions
                                       , KM.lookup (Key.fromText "name") tool == Just (String name)
                                       , Just schema <- [KM.lookup (Key.fromText "inputSchema") tool]] of
  Object schema : _ -> case KM.lookup (Key.fromText "required") schema of
    Just (Array fields) -> [field | String field <- toList fields]
    _ -> []
  _ -> []

toolDescription :: Text -> Maybe Text
toolDescription name = listToMaybe
  [ desc | Object tool <- toolDefinitions
         , KM.lookup (Key.fromText "name") tool == Just (String name)
         , Just (String desc) <- [KM.lookup (Key.fromText "description") tool]
  ]

toolArrayItemName :: Value -> Maybe Text
toolArrayItemName (Object tool) = case KM.lookup (Key.fromText "name") tool of
  Just (String name) -> Just name
  _                  -> Nothing
toolArrayItemName _ = Nothing

toolArrayItemDescription :: Text -> Array -> Maybe Text
toolArrayItemDescription name tools = listToMaybe
  [ desc | Object tool <- toList tools
         , KM.lookup (Key.fromText "name") tool == Just (String name)
         , Just (String desc) <- [KM.lookup (Key.fromText "description") tool]
  ]

spec :: Spec
spec = do
  describe "toolDefinitions" $ do
    it "exposes exactly the slim default MCP tool surface" $ do
      toolNames `shouldBe` slimToolNames

    it "does not expose removed tools" $ do
      toolNames `shouldNotSatisfy` any (`elem` removedToolNames)

    it "keeps saved_view execution out of MCP so it cannot bypass compact shapers" $ do
      toolNames `shouldNotContain` ["saved_view"]
      parseToolCall "saved_view" (object ["action" .= ("execute" :: Text)]) `shouldSatisfy` isUnknownTool "saved_view"

    it "omits noisy workspace and batch inputs from retained schemas" $ do
      toolSchemaProperties "memory_create" `shouldNotContain` ["workspace_id", "metadata", "items", "fts_language", "source", "confidence", "expires_at", "pinned"]
      toolSchemaProperties "memory_update" `shouldNotContain` ["metadata", "items", "source", "confidence", "expires_at"]
      toolSchemaProperties "project_create" `shouldNotContain` ["workspace_id", "metadata"]
      toolSchemaProperties "project_update" `shouldNotContain` ["metadata", "items"]
      toolSchemaProperties "project_detail" `shouldBe` ["project_id"]
      toolSchemaRequired "project_detail" `shouldBe` ["project_id"]
      toolSchemaProperties "task_create" `shouldNotContain` ["workspace_id", "metadata"]
      toolSchemaProperties "task_update" `shouldNotContain` ["metadata", "items"]
      toolSchemaProperties "task_detail" `shouldBe` ["task_id"]
      toolSchemaRequired "task_detail" `shouldBe` ["task_id"]
      toolSchemaProperties "project_overview" `shouldContain` ["include_descriptions"]
      toolSchemaProperties "task_overview" `shouldContain` ["include_description"]
      toolSchemaProperties "project_detail" `shouldNotContain` ["include_descriptions", "include_description"]
      toolSchemaProperties "task_detail" `shouldNotContain` ["include_descriptions", "include_description"]
      toolSchemaProperties "search" `shouldNotContain` ["workspace_id", "search_language", "offset", "min_access_count", "min_importance", "category_id", "pinned_only", "task_priority"]

    it "describes compact defaults and available detail paths" $ do
      toolDescriptionShouldContain "search" "Returns compact summaries"
      toolDescriptionShouldContain "search" "memory content/previews are omitted"
      toolDescriptionShouldContain "memory_get" "detail path for compact memory summaries"
      toolDescriptionShouldContain "project_detail" "Get compact details for one project"
      toolDescriptionShouldContain "task_detail" "Get compact details for one task"
      toolDescriptionShouldContain "project_overview" "capped to a bounded number of rows"
      toolDescriptionShouldContain "project_overview" "truncated per row"
      toolDescriptionShouldContain "project_overview" "prefer task_overview"
      toolDescriptionShouldContain "context_get" "detail_level controls how many summaries"
      toolDescriptionShouldContain "task_start" "detail_level controls context breadth"
      toolDescriptionShouldContain "task_finish" "optional notes_memory_id"
      toolDescriptionShouldContain "project_archive" "optional summary_memory_id"

  describe "MCP compact response contract" $ do
    it "keeps the slim MCP tool map executable in tool definitions" $ do
      toolNames `shouldBe` slimToolNames
      mapM_ (\name -> toolDescription name `shouldSatisfy` maybe False (not . T.null)) slimToolNames

    it "keeps the compact DTO vocabulary and omission policy in executable fixtures" $ do
      fixtures <- readCompactResponseFixtures
      let regressionNames = [name | (name, _, _) <- compactResponseRegressionCases]
      case fixtures of
        Object root -> sort (Key.toText <$> KM.keys root) `shouldBe` sort regressionNames
        _           -> expectationFailure "Expected compact response fixtures to be a JSON object"
      mapM_ (\name -> fixturePayload name fixtures `shouldSatisfy` maybe False (const True)) regressionNames
      mapM_ (\name -> fixtureMaxChars name fixtures `shouldSatisfy` maybe False (> 0)) regressionNames
      mapM_ (\name -> fixtureMaxMcpEnvelopeChars name fixtures `shouldSatisfy` maybe False (> 0)) regressionNames
      mapM_ (\name -> fixtureMaxJsonRpcStdioChars name fixtures `shouldSatisfy` maybe False (> 0)) regressionNames

      let Just taskAck = fixturePayload "task_create" fixtures
      jsonField "ok" taskAck `shouldBe` Just (Bool True)
      jsonField "entity_type" taskAck `shouldBe` Just (String "task")
      jsonField "summary" taskAck `shouldSatisfy` hasObjectField "title"
      jsonField "workspace_id" taskAck `shouldBe` Nothing

      let Just memoryAck = fixturePayload "memory_create" fixtures
          memorySummary = jsonField "summary" memoryAck
      (memorySummary >>= jsonField "content") `shouldBe` Nothing
      (memorySummary >>= jsonField "content_preview") `shouldBe` Nothing
      jsonField "workspace_id" memoryAck `shouldBe` Nothing

      let Just searchPayload = fixturePayload "unified_search" fixtures
      jsonField "memories" searchPayload `shouldSatisfy` arrayLength 1
      jsonField "projects" searchPayload `shouldSatisfy` arrayLength 1
      jsonField "tasks" searchPayload `shouldSatisfy` arrayLength 1
      mapM_ (\name -> maybe (expectationFailure $ "Missing fixture payload for " <> T.unpack name) (shouldOmitDefaultNoise name) (fixturePayload name fixtures)) regressionNames

    it "keeps project overview descriptions opt-in and bounded in executable shapers" $ do
      toolSchemaProperties "project_overview" `shouldContain` ["include_descriptions"]
      toolDescriptionShouldContain "project_overview" "descriptions are capped to a bounded number of rows"
      toolDescriptionShouldContain "project_overview" "prefer task_overview"
      toolSchemaProperties "task_overview" `shouldContain` ["include_description"]

      let compactOverview = compactProjectOverview projectOverviewRegressionValue
      (jsonField "project" compactOverview >>= jsonField "description") `shouldBe` Nothing
      (firstArrayItem "tasks" compactOverview >>= jsonField "description") `shouldBe` Nothing
      jsonField "descriptions_omitted" compactOverview `shouldBe` Nothing

      let detailedOverview = compactProjectOverviewWithDescriptions projectOverviewRegressionValue
      (jsonField "project" detailedOverview >>= jsonField "description") `shouldBe` Just (String "full project description")
      (firstArrayItem "tasks" detailedOverview >>= jsonField "description") `shouldBe` Just (String "full task description")
      jsonField "descriptions_omitted" detailedOverview `shouldBe` Nothing

    it "shapes project_detail as compact detail context with blocker diagnostics" $ do
      let detail = compactProjectDetail projectDetailRegressionValue
          taskRows = arrayFieldItems "tasks" detail
          blockedSummary = jsonField "blocked_tasks" detail
          blockedRows = maybe [] (arrayFieldItems "items") blockedSummary
          subprojectRows = arrayFieldItems "subprojects" detail
      (jsonField "project" detail >>= jsonField "description") `shouldBe` Just (String projectDetailDescription)
      (jsonField "project" detail >>= jsonField "description_truncated") `shouldBe` Nothing
      mapMaybe (textField "title") taskRows `shouldBe` ["Todo direct", "Blocked direct", "In progress direct"]
      mapMaybe (textField "status") taskRows `shouldBe` ["todo", "blocked", "in_progress"]
      mapM_ (\row -> jsonField "description" row `shouldBe` Nothing) taskRows
      length blockedRows `shouldBe` 1
      mapMaybe (textField "title") blockedRows `shouldBe` ["Blocked direct"]
      mapM_ (\row -> jsonField "description" row `shouldBe` Nothing) blockedRows
      (blockedSummary >>= jsonField "returned_count") `shouldBe` Just (Number 1)
      (blockedSummary >>= jsonField "subtree_blocked_task_count") `shouldBe` Just (Number 3)
      (blockedSummary >>= jsonField "subtree_dependency_blocked_task_count") `shouldBe` Just (Number 2)
      (blockedSummary >>= jsonField "subtree_open_dependency_count") `shouldBe` Just (Number 5)
      mapMaybe (textField "name") subprojectRows `shouldBe` ["Active child", "Archived child"]
      mapM_ (\row -> jsonField "description" row `shouldBe` Nothing) subprojectRows
      jsonField "linked_memories" detail `shouldSatisfy` arrayLength 1
      jsonField "connected_memories" detail `shouldSatisfy` arrayLength 1
      show detail `shouldNotContain` "Done direct"
      show detail `shouldNotContain` "Cancelled direct"
      show detail `shouldNotContain` "Other project blocked"
      show detail `shouldNotContain` "Grandchild"
      show detail `shouldNotContain` "full task description"
      show detail `shouldNotContain` "linked full content should be omitted"
      show detail `shouldNotContain` "workspace_id"

    it "keeps saved_view unavailable on the slim MCP surface" $ do
      toolNames `shouldNotContain` ["saved_view"]
      parseToolCall "saved_view" (object ["action" .= ("execute" :: Text)]) `shouldSatisfy` isUnknownTool "saved_view"

  describe "compact response regression fixtures" $ do
    it "matches golden default MCP payloads and stable size budgets" $ do
      fixtures <- readCompactResponseFixtures
      mapM_ (assertCompactResponseFixture fixtures) compactResponseRegressionCases

    it "matches golden payloads through dispatcher wiring and MCP envelope budgets" $ do
      fixtures <- readCompactResponseFixtures
      withMockHmemServer $ \mgr base ->
        mapM_ (assertDispatcherResponseFixture fixtures mgr base) dispatcherResponseRegressionCases

    it "matches golden payloads through JSON-RPC tools/call envelopes and outer budgets" $ do
      fixtures <- readCompactResponseFixtures
      withMockHmemServer $ \mgr base -> do
        initialized <- newTVarIO True
        wsContext <- newTVarIO Nothing
        mapM_ (assertJsonRpcResponseFixture fixtures mgr base initialized wsContext) dispatcherResponseRegressionCases

    it "treats saved_view execute as removed rather than a response-bloat bypass" $ do
      fixtures <- readCompactResponseFixtures
      fixturePayload "saved_view_execute" fixtures `shouldBe` Nothing
      toolNames `shouldNotContain` ["saved_view"]
      parseToolCall "saved_view" (object ["action" .= ("execute" :: Text)]) `shouldSatisfy` isUnknownTool "saved_view"

  describe "JSON-RPC MCP method coverage" $ do
    it "lists compact tool descriptions without saved_view" $ do
      mgr <- newManager defaultManagerSettings
      initialized <- newTVarIO True
      wsContext <- newTVarIO Nothing
      mResponse <- handleRequest mgr "http://127.0.0.1:9" Nothing initialized wsContext $
        JsonRpcRequest (Just (String "tools-list")) "tools/list" Nothing
      case mResponse >>= jsonField "result" >>= jsonField "tools" of
        Just (Array tools) -> do
          let listedNames = mapMaybe toolArrayItemName (toList tools)
          listedNames `shouldBe` slimToolNames
          listedNames `shouldNotContain` ["saved_view"]
          toolArrayItemDescription "search" tools `shouldSatisfy` maybe False ("Returns compact summaries" `T.isInfixOf`)
          toolArrayItemDescription "search" tools `shouldSatisfy` maybe False ("memory content/previews are omitted" `T.isInfixOf`)
          toolArrayItemDescription "project_overview" tools `shouldSatisfy` maybe False ("capped to a bounded number of rows" `T.isInfixOf`)
          toolArrayItemDescription "project_overview" tools `shouldSatisfy` maybe False ("truncated per row" `T.isInfixOf`)
          toolArrayItemDescription "memory_get" tools `shouldSatisfy` maybe False ("detail path for compact memory summaries" `T.isInfixOf`)
        other -> expectationFailure $ "Expected tools/list result tools array, got: " <> show other

    it "rejects saved_view execute through tools/call before any HTTP dispatch" $ do
      mgr <- newManager defaultManagerSettings
      initialized <- newTVarIO True
      wsContext <- newTVarIO Nothing
      mResponse <- handleRequest mgr "http://127.0.0.1:9" Nothing initialized wsContext $
        JsonRpcRequest (Just (String "saved-view-call")) "tools/call" $ Just $ object
          [ "name" .= ("saved_view" :: Text)
          , "arguments" .= object ["action" .= ("execute" :: Text)]
          ]
      let mResult = mResponse >>= jsonField "result"
      (mResult >>= jsonField "isError") `shouldBe` Just (Bool True)
      (mResult >>= mcpTextContent) `shouldBe` Just "Unknown tool: saved_view"

    it "routes search tools/call through JSON-RPC dispatch with workspace context injection" $ do
      fixtures <- readCompactResponseFixtures
      withMockHmemServer $ \mgr base -> do
        initialized <- newTVarIO True
        wsContext <- newTVarIO Nothing
        setResponse <- handleRequest mgr base Nothing initialized wsContext $
          JsonRpcRequest (Just (String "set-workspace")) "tools/call" $ Just $ object
            [ "name" .= ("set_workspace" :: Text)
            , "arguments" .= object ["workspace_id" .= testUUID2]
            ]
        (setResponse >>= jsonField "result" >>= mcpTextValue >>= jsonField "workspace_id") `shouldBe` Just (String testUUID2)

        mResponse <- handleRequest mgr base Nothing initialized wsContext $
          JsonRpcRequest (Just (String "search-call")) "tools/call" $ Just $ object
            [ "name" .= ("search" :: Text)
            , "arguments" .= object
                [ "entity_types" .= (["memory", "project", "task"] :: [Text])
                , "limit" .= (5 :: Int)
                ]
            ]
        (mResponse >>= jsonField "jsonrpc") `shouldBe` Just (String "2.0")
        case (fixturePayload "unified_search" fixtures, mResponse >>= jsonField "result" >>= mcpTextValue) of
          (Just expected, Just actual) -> do
            actual `shouldBe` expected
            shouldOmitDefaultNoise "unified_search" actual
          (Nothing, _) -> expectationFailure "Missing fixture payload for unified_search"
          (_, Nothing) -> expectationFailure $ "Expected compact search MCP payload, got: " <> show mResponse

    it "routes workspace-injected search through stdio line handling" $ do
      fixtures <- readCompactResponseFixtures
      withMockHmemServer $ \mgr base -> do
        initialized <- newTVarIO False
        wsContext <- newTVarIO Nothing
        _ <- handleStdioLine mgr base Nothing initialized wsContext $ jsonLine $ object
          [ "jsonrpc" .= ("2.0" :: Text)
          , "id" .= ("init" :: Text)
          , "method" .= ("initialize" :: Text)
          ]
        setResponse <- handleStdioLine mgr base Nothing initialized wsContext $ jsonLine $ object
          [ "jsonrpc" .= ("2.0" :: Text)
          , "id" .= ("set-workspace-stdio" :: Text)
          , "method" .= ("tools/call" :: Text)
          , "params" .= object
              [ "name" .= ("set_workspace" :: Text)
              , "arguments" .= object ["workspace_id" .= testUUID2]
              ]
          ]
        (setResponse >>= jsonField "result" >>= mcpTextValue >>= jsonField "workspace_id") `shouldBe` Just (String testUUID2)

        searchResponse <- handleStdioLine mgr base Nothing initialized wsContext $ jsonLine $ object
          [ "jsonrpc" .= ("2.0" :: Text)
          , "id" .= ("search-stdio" :: Text)
          , "method" .= ("tools/call" :: Text)
          , "params" .= object
              [ "name" .= ("search" :: Text)
              , "arguments" .= object
                  [ "entity_types" .= (["memory", "project", "task"] :: [Text])
                  , "limit" .= (5 :: Int)
                  ]
              ]
          ]
        case (fixturePayload "unified_search" fixtures, searchResponse >>= jsonField "result" >>= mcpTextValue) of
          (Just expected, Just actual) -> actual `shouldBe` expected
          (Nothing, _) -> expectationFailure "Missing fixture payload for unified_search"
          (_, Nothing) -> expectationFailure $ "Expected stdio compact search payload, got: " <> show searchResponse

    it "routes project_overview include_descriptions through stdio line handling" $ do
      withMockHmemServer $ \mgr base -> do
        initialized <- newTVarIO False
        wsContext <- newTVarIO Nothing
        _ <- handleStdioLine mgr base Nothing initialized wsContext $ jsonLine $ object
          [ "jsonrpc" .= ("2.0" :: Text)
          , "id" .= ("init" :: Text)
          , "method" .= ("initialize" :: Text)
          ]
        overviewResponse <- handleStdioLine mgr base Nothing initialized wsContext $ jsonLine $ object
          [ "jsonrpc" .= ("2.0" :: Text)
          , "id" .= ("overview-stdio" :: Text)
          , "method" .= ("tools/call" :: Text)
          , "params" .= object
              [ "name" .= ("project_overview" :: Text)
              , "arguments" .= object
                  [ "project_id" .= testUUID
                  , "include_descriptions" .= True
                  ]
              ]
          ]
        let mOverview = overviewResponse >>= jsonField "result" >>= mcpTextValue
        (mOverview >>= jsonField "project" >>= jsonField "description") `shouldBe` Just (String "full project description")
        (mOverview >>= firstArrayItem "tasks" >>= jsonField "description") `shouldBe` Just (String "full task description")
        (mOverview >>= jsonField "descriptions_omitted") `shouldBe` Nothing

  describe "live HTTP tool dispatch" $ do
    it "routes representative slim MCP tools through HTTP compact shapers" $ do
      withMockHmemServer $ \mgr base -> do
        searchValue <- callMockTool mgr base "search" $ object
          [ "workspace_id" .= testUUID2
          , "entity_types" .= (["memory", "project", "task"] :: [Text])
          , "limit" .= (5 :: Int)
          ]
        firstArrayItem "memories" searchValue `shouldBe` Just (object
          [ "id" .= parsedUUID
          , "summary" .= ("Search memory" :: Text)
          , "memory_type" .= ("long_term" :: Text)
          , "importance" .= (7 :: Int)
          , "tags" .= (["contract"] :: [Text])
          , "pinned" .= True
          ])
        show searchValue `shouldNotContain` "workspace_id"
        show searchValue `shouldNotContain` "full memory content"
        show searchValue `shouldNotContain` "linked preview should be omitted"

        memoryDetail <- callMockTool mgr base "memory_get" $ object
          [ "memory_id" .= testUUID ]
        jsonField "content" memoryDetail `shouldBe` Just (String "full memory content")
        jsonField "metadata" memoryDetail `shouldBe` Just (object ["kept" .= True])
        jsonField "workspace_id" memoryDetail `shouldBe` Nothing
        jsonField "created_at" memoryDetail `shouldBe` Nothing

        memoryLinks <- callMockTool mgr base "memory_link" $ object
          [ "action" .= ("list" :: Text)
          , "memory_id" .= testUUID
          ]
        jsonField "links" memoryLinks `shouldSatisfy` arrayLength 1
        show memoryLinks `shouldContain` "source_id"
        show memoryLinks `shouldNotContain` "source_memory"
        show memoryLinks `shouldNotContain` "full memory content"

        projectDetail <- callMockTool mgr base "project_detail" $ object
          [ "project_id" .= testProjectDetailUUID ]
        projectDetailWithIgnoredFlag <- callMockTool mgr base "project_detail" $ object
          [ "project_id" .= testProjectDetailUUID
          , "include_descriptions" .= True
          ]
        projectDetailWithIgnoredFlag `shouldBe` projectDetail
        (jsonField "project" projectDetail >>= jsonField "id") `shouldBe` Just (String testProjectDetailUUID)
        (jsonField "project" projectDetail >>= jsonField "name") `shouldBe` Just (String "Detail project")
        jsonField "blocked_tasks" projectDetail `shouldSatisfy` hasObjectField "returned_count"

        taskDetail <- callMockTool mgr base "task_detail" $ object
          [ "task_id" .= testUUID ]
        taskDetailWithIgnoredFlag <- callMockTool mgr base "task_detail" $ object
          [ "task_id" .= testUUID
          , "include_description" .= True
          ]
        taskDetailWithIgnoredFlag `shouldBe` taskDetail
        jsonField "id" taskDetail `shouldBe` Just (String testUUID)
        jsonField "title" taskDetail `shouldBe` Just (String "Task")

        missingProject <- callMockToolRaw mgr base "project_detail" $ object
          [ "project_id" .= testNotesMemoryUUID ]
        jsonField "isError" missingProject `shouldBe` Just (Bool True)
        mcpTextContent missingProject `shouldSatisfy` maybe False ("[NOT_FOUND]" `T.isInfixOf`)

        missingTask <- callMockToolRaw mgr base "task_detail" $ object
          [ "task_id" .= testNotesMemoryUUID ]
        jsonField "isError" missingTask `shouldBe` Just (Bool True)
        mcpTextContent missingTask `shouldSatisfy` maybe False ("[NOT_FOUND]" `T.isInfixOf`)

        overview <- callMockTool mgr base "project_overview" $ object
          [ "project_id" .= testUUID ]
        (jsonField "project" overview >>= jsonField "description") `shouldBe` Nothing
        (firstArrayItem "tasks" overview >>= jsonField "description") `shouldBe` Nothing
        jsonField "connected_memories" overview `shouldSatisfy` arrayLength 1
        show overview `shouldNotContain` "workspace_id"
        show overview `shouldNotContain` "full project description"
        show overview `shouldNotContain` "full task description"
        show overview `shouldNotContain` "full memory content"

        typedOverview <- callMockTool mgr base "project_overview" $ object
          [ "project_id" .= testUUID2 ]
        (jsonField "project" typedOverview >>= jsonField "name") `shouldBe` Just (String "Typed project")
        (firstArrayItem "tasks" typedOverview >>= jsonField "title") `shouldBe` Just (String "Typed task")
        show typedOverview `shouldNotContain` "typed project description"
        show typedOverview `shouldNotContain` "typed task description"
        show typedOverview `shouldNotContain` "workspace_id"

        overviewWithDescriptions <- callMockTool mgr base "project_overview" $ object
          [ "project_id" .= testUUID
          , "include_descriptions" .= True
          ]
        (jsonField "project" overviewWithDescriptions >>= jsonField "description") `shouldBe` Just (String "full project description")
        (jsonField "project" overviewWithDescriptions >>= jsonField "description_truncated") `shouldBe` Nothing
        (firstArrayItem "tasks" overviewWithDescriptions >>= jsonField "description") `shouldBe` Just (String "full task description")
        (firstArrayItem "tasks" overviewWithDescriptions >>= jsonField "description_truncated") `shouldBe` Nothing
        jsonField "descriptions_omitted" overviewWithDescriptions `shouldBe` Nothing

    it "round-trips compact MCP dispatch against the real hmem-server app" $ do
      withRealHmemServer $ \mgr base -> do
        workspaceAck <- callTool mgr base "workspace_register" $ object
          [ "name" .= ("mcp-real-dto-ws" :: Text) ]
        workspaceId <- expectTextField "id" workspaceAck

        projectAck <- callTool mgr base "project_create" $ object
          [ "workspace_id" .= workspaceId
          , "name" .= ("Real DTO project" :: Text)
          , "description" .= ("real project description" :: Text)
          , "priority" .= (4 :: Int)
          ]
        projectId <- expectTextField "id" projectAck

        taskAck <- callTool mgr base "task_create" $ object
          [ "workspace_id" .= workspaceId
          , "project_id" .= projectId
          , "title" .= ("Real DTO task" :: Text)
          , "description" .= ("real task description" :: Text)
          , "priority" .= (6 :: Int)
          ]
        _taskId <- expectTextField "id" taskAck

        overview <- callTool mgr base "project_overview" $ object
          [ "project_id" .= projectId ]
        (jsonField "project" overview >>= jsonField "name") `shouldBe` Just (String "Real DTO project")
        (firstArrayItem "tasks" overview >>= jsonField "title") `shouldBe` Just (String "Real DTO task")
        show overview `shouldNotContain` "real project description"
        show overview `shouldNotContain` "real task description"
        show overview `shouldNotContain` "workspace_id"

        overviewWithDescriptions <- callTool mgr base "project_overview" $ object
          [ "project_id" .= projectId
          , "include_descriptions" .= True
          ]
        (jsonField "project" overviewWithDescriptions >>= jsonField "description") `shouldBe` Just (String "real project description")
        (firstArrayItem "tasks" overviewWithDescriptions >>= jsonField "description") `shouldBe` Just (String "real task description")
        jsonField "descriptions_omitted" overviewWithDescriptions `shouldBe` Nothing

        initialized <- newTVarIO False
        wsContext <- newTVarIO Nothing
        _ <- handleStdioLine mgr base Nothing initialized wsContext $ jsonLine $ object
          [ "jsonrpc" .= ("2.0" :: Text)
          , "id" .= ("real-init" :: Text)
          , "method" .= ("initialize" :: Text)
          ]
        setResponse <- handleStdioLine mgr base Nothing initialized wsContext $ jsonLine $ object
          [ "jsonrpc" .= ("2.0" :: Text)
          , "id" .= ("real-set-workspace" :: Text)
          , "method" .= ("tools/call" :: Text)
          , "params" .= object
              [ "name" .= ("set_workspace" :: Text)
              , "arguments" .= object ["workspace_id" .= workspaceId]
              ]
          ]
        (setResponse >>= jsonField "result" >>= mcpTextValue >>= jsonField "workspace_id") `shouldBe` Just (String workspaceId)

        searchResponse <- handleStdioLine mgr base Nothing initialized wsContext $ jsonLine $ object
          [ "jsonrpc" .= ("2.0" :: Text)
          , "id" .= ("real-search" :: Text)
          , "method" .= ("tools/call" :: Text)
          , "params" .= object
              [ "name" .= ("search" :: Text)
              , "arguments" .= object
                  [ "entity_types" .= (["project", "task"] :: [Text])
                  , "limit" .= (5 :: Int)
                  ]
              ]
          ]
        let mSearch = searchResponse >>= jsonField "result" >>= mcpTextValue
        (searchProjectNames <$> mSearch) `shouldBe` Just ["Real DTO project"]
        (searchTaskTitles <$> mSearch) `shouldBe` Just ["Real DTO task"]
        show mSearch `shouldNotContain` "workspace_id"
        show mSearch `shouldNotContain` "real project description"
        show mSearch `shouldNotContain` "real task description"

    it "routes additional slim tools with live request body, query, and auth checks" $ do
      withMockHmemServer $ \mgr base -> do
        memoryUpdate <- callMockTool mgr base "memory_update" $ object
          [ "memory_id" .= testUUID
          , "content" .= ("revised memory content" :: Text)
          , "tags" .= (["new-tag"] :: [Text])
          ]
        jsonField "ok" memoryUpdate `shouldBe` Just (Bool True)
        jsonField "action" memoryUpdate `shouldBe` Just (String "updated")
        jsonField "changed_fields" memoryUpdate `shouldBe` Just (toJSON (["content", "tags"] :: [Text]))
        show memoryUpdate `shouldNotContain` "full memory content"

        taskOverview <- callMockTool mgr base "task_overview" $ object
          [ "task_id" .= testUUID ]
        (jsonField "task" taskOverview >>= jsonField "description") `shouldBe` Nothing
        jsonField "readiness_rollup" taskOverview `shouldSatisfy` hasObjectField "completion_ready"
        show taskOverview `shouldNotContain` "full task description"

        contextValue <- callMockTool mgr base "context_get" $ object
          [ "task_id" .= testUUID
          , "detail_level" .= ("medium" :: Text)
          ]
        jsonField "detail_level" contextValue `shouldBe` Just (String "medium")
        jsonField "task_memories" contextValue `shouldSatisfy` arrayLength 1
        show contextValue `shouldNotContain` "full memory content"

        nextTasks <- callMockToolWithApiKey mgr base (Just "request-shape-token") "project_next_tasks" $ object
          [ "project_id" .= testUUID
          , "limit" .= (3 :: Int)
          , "include_blocked" .= True
          ]
        jsonField "items" nextTasks `shouldSatisfy` arrayLength 1
        (firstArrayItem "items" nextTasks >>= jsonField "task" >>= jsonField "description") `shouldBe` Nothing
        show nextTasks `shouldNotContain` "workspace_id"
        show nextTasks `shouldNotContain` "full task description"
        show nextTasks `shouldNotContain` "project-only next-task regression row"

    it "routes composite workflow tools through HTTP compact workflow shaping" $ do
      withMockHmemServer $ \mgr base -> do
        finishAck <- callMockTool mgr base "task_finish" $ object
          [ "task_id" .= testUUID
          , "status" .= ("done" :: Text)
          ]
        jsonField "ok" finishAck `shouldBe` Just (Bool True)
        jsonField "task_id" finishAck `shouldBe` Just (String testUUID)
        jsonField "status" finishAck `shouldBe` Just (String "done")
        jsonField "changed_fields" finishAck `shouldBe` Just (toJSON (["status"] :: [Text]))
        jsonField "summary" finishAck `shouldBe` Nothing
        show finishAck `shouldNotContain` "full task description"

        finishWithNotes <- callMockTool mgr base "task_finish" $ object
          [ "task_id" .= testUUID
          , "status" .= ("done" :: Text)
          , "notes" .= ("finished with notes" :: Text)
          ]
        jsonField "ok" finishWithNotes `shouldBe` Just (Bool True)
        jsonField "task_id" finishWithNotes `shouldBe` Just (String testUUID)
        jsonField "status" finishWithNotes `shouldBe` Just (String "done")
        jsonField "notes_memory_id" finishWithNotes `shouldBe` Just (String testNotesMemoryUUID)
        jsonField "summary" finishWithNotes `shouldBe` Nothing
        show finishWithNotes `shouldNotContain` "finished with notes"
        show finishWithNotes `shouldNotContain` "full memory content"

        archiveAck <- callMockTool mgr base "project_archive" $ object
          [ "project_id" .= testUUID ]
        jsonField "ok" archiveAck `shouldBe` Just (Bool True)
        jsonField "project_id" archiveAck `shouldBe` Just (String testUUID)
        jsonField "status" archiveAck `shouldBe` Just (String "archived")
        jsonField "changed_fields" archiveAck `shouldBe` Just (toJSON (["status"] :: [Text]))
        jsonField "summary" archiveAck `shouldBe` Nothing
        jsonField "summary_memory" archiveAck `shouldBe` Nothing
        show archiveAck `shouldNotContain` "full project description"

        archiveWithSummary <- callMockTool mgr base "project_archive" $ object
          [ "project_id" .= testUUID
          , "summary" .= ("archive summary" :: Text)
          ]
        jsonField "ok" archiveWithSummary `shouldBe` Just (Bool True)
        jsonField "project_id" archiveWithSummary `shouldBe` Just (String testUUID)
        jsonField "status" archiveWithSummary `shouldBe` Just (String "archived")
        jsonField "summary_memory_id" archiveWithSummary `shouldBe` Just (String testSummaryMemoryUUID)
        jsonField "summary_memory" archiveWithSummary `shouldBe` Nothing
        show archiveWithSummary `shouldNotContain` "archive summary"
        show archiveWithSummary `shouldNotContain` "full memory content"

        specSummary <- callMockTool mgr base "project_spec" $ object
          [ "workspace_id" .= testUUID2
          , "name" .= ("Workflow project" :: Text)
          , "description" .= ("workflow project description" :: Text)
          , "priority" .= (6 :: Int)
          , "tasks" .=
              [ object
                  [ "title" .= ("First task" :: Text)
                  , "description" .= ("first task description" :: Text)
                  ]
              , object
                  [ "title" .= ("Second task" :: Text)
                  , "description" .= ("second task description" :: Text)
                  , "priority" .= (8 :: Int)
                  ]
              ]
          ]
        jsonField "ok" specSummary `shouldBe` Just (Bool True)
        jsonField "entity_type" specSummary `shouldBe` Just (String "project_spec")
        jsonField "project_id" specSummary `shouldBe` Just (String testUUID)
        jsonField "tasks_created" specSummary `shouldSatisfy` arrayLength 2
        show specSummary `shouldNotContain` "full project description"
        show specSummary `shouldNotContain` "full task description"

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
            [ "memories" .= [searchMemoryValue]
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
          expectedMemorySummary = object
            [ "id" .= parsedUUID
            , "summary" .= ("Search memory" :: Text)
            , "memory_type" .= ("long_term" :: Text)
            , "importance" .= (7 :: Int)
            , "tags" .= (["contract"] :: [Text])
            , "pinned" .= True
            ]
          expectedProjectSummary = object
            [ "id" .= parsedUUID
            , "name" .= ("Project" :: Text)
            , "status" .= ("active" :: Text)
            , "priority" .= (8 :: Int)
            ]
          expectedTaskSummary = object
            [ "id" .= parsedUUID
            , "title" .= ("Task" :: Text)
            , "status" .= ("todo" :: Text)
            , "priority" .= (9 :: Int)
            , "project_id" .= parsedUUID3
            ]
          expectedLinkedMemorySummary = object
            [ "id" .= parsedUUID
            , "summary" .= ("Linked memory" :: Text)
            , "importance" .= (5 :: Int)
            , "tags" .= (["linked"] :: [Text])
            ]
          expectedProjectRow = object
            [ "project" .= expectedProjectSummary
            , "linked_memories" .= [expectedLinkedMemorySummary]
            ]
          expectedTaskRow = object
            [ "task" .= expectedTaskSummary
            , "linked_memories" .= [expectedLinkedMemorySummary]
            ]
      jsonField "memories" shaped `shouldSatisfy` arrayLength 1
      jsonField "projects" shaped `shouldSatisfy` arrayLength 1
      jsonField "tasks" shaped `shouldSatisfy` arrayLength 1
      firstArrayItem "memories" shaped `shouldBe` Just expectedMemorySummary
      firstArrayItem "projects" shaped `shouldBe` Just expectedProjectRow
      firstArrayItem "tasks" shaped `shouldBe` Just expectedTaskRow
      show shaped `shouldNotContain` "workspace_id"
      show shaped `shouldNotContain` "created_at"
      show shaped `shouldNotContain` "metadata"
      show shaped `shouldNotContain` "full memory content"
      show shaped `shouldNotContain` "full project description"
      show shaped `shouldNotContain` "full task description"
      show shaped `shouldNotContain` "linked full content should be omitted"
      show shaped `shouldNotContain` "linked preview should be omitted"

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

    it "computes changed_fields from parsed update inputs" $ do
      let memoryArgs = object
            [ "memory_id" .= testUUID
            , "content" .= ("new content" :: Text)
            , "summary" .= Null
            , "memory_type" .= ("short_term" :: Text)
            , "importance" .= (4 :: Int)
            , "metadata" .= object ["source" .= ("test" :: Text)]
            , "expires_at" .= Null
            , "source" .= ("agent" :: Text)
            , "confidence" .= (0.75 :: Double)
            , "pinned" .= False
            , "tags" .= (["updated"] :: [Text])
            ]
          projectArgs = object
            [ "project_id" .= testUUID
            , "name" .= ("Renamed project" :: Text)
            , "description" .= Null
            , "parent_id" .= testUUID2
            , "status" .= ("paused" :: Text)
            , "priority" .= (3 :: Int)
            , "metadata" .= object ["source" .= ("test" :: Text)]
            ]
          taskArgs = object
            [ "task_id" .= testUUID
            , "title" .= ("Renamed task" :: Text)
            , "description" .= ("New description" :: Text)
            , "project_id" .= Null
            , "parent_id" .= testUUID2
            , "status" .= ("blocked" :: Text)
            , "priority" .= (6 :: Int)
            , "metadata" .= object ["source" .= ("test" :: Text)]
            , "due_at" .= Null
            ]
      case parseToolCall "memory_update" memoryArgs of
        Right (MemoryUpdate _ um tags) ->
          memoryUpdateChangedFields um tags `shouldBe`
            [ "content", "summary", "memory_type", "importance", "metadata"
            , "expires_at", "source", "confidence", "pinned", "tags"
            ]
        other -> expectationFailure $ "Expected MemoryUpdate, got: " <> show other
      case parseToolCall "project_update" projectArgs of
        Right (ProjectUpdate _ up) ->
          projectUpdateChangedFields up `shouldBe`
            [ "name", "description", "parent_id", "status", "priority", "metadata" ]
        other -> expectationFailure $ "Expected ProjectUpdate, got: " <> show other
      case parseToolCall "task_update" taskArgs of
        Right (TaskUpdate _ ut) ->
          taskUpdateChangedFields ut `shouldBe`
            [ "title", "description", "project_id", "parent_id", "status", "priority", "metadata", "due_at" ]
        other -> expectationFailure $ "Expected TaskUpdate, got: " <> show other

    it "omits changed_fields for omitted update inputs" $ do
      case parseToolCall "memory_update" (object ["memory_id" .= testUUID]) of
        Right (MemoryUpdate _ um tags) -> memoryUpdateChangedFields um tags `shouldBe` []
        other -> expectationFailure $ "Expected empty MemoryUpdate, got: " <> show other
      case parseToolCall "project_update" (object ["project_id" .= testUUID]) of
        Right (ProjectUpdate _ up) -> projectUpdateChangedFields up `shouldBe` []
        other -> expectationFailure $ "Expected empty ProjectUpdate, got: " <> show other
      case parseToolCall "task_update" (object ["task_id" .= testUUID]) of
        Right (TaskUpdate _ ut) -> taskUpdateChangedFields ut `shouldBe` []
        other -> expectationFailure $ "Expected empty TaskUpdate, got: " <> show other
      jsonField "changed_fields" (addChangedFields [] (compactTaskMutationAck "updated" fullTaskValue)) `shouldBe` Nothing

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
      let finishAck = compactTaskFinishAckWithNotes "finished" (Just notesMemoryValue) fullTaskValue
          finishWithoutNotes = compactTaskFinishAckWithNotes "finished" Nothing fullTaskValue
          archiveAck = compactProjectArchiveAck (Just summaryMemoryValue) fullProjectValue
          archiveWithoutSummary = compactProjectArchiveAck Nothing fullProjectValue
      jsonField "task_id" finishAck `shouldBe` Just (String testUUID)
      jsonField "status" finishAck `shouldBe` Just (String "todo")
      jsonField "notes_memory_id" finishAck `shouldBe` Just (String testNotesMemoryUUID)
      jsonField "notes_memory" finishAck `shouldBe` Nothing
      jsonField "summary" finishAck `shouldBe` Nothing
      jsonField "notes_memory_id" finishWithoutNotes `shouldBe` Nothing
      jsonField "summary" finishWithoutNotes `shouldBe` Nothing
      jsonField "project_id" archiveAck `shouldBe` Just (String testUUID)
      jsonField "status" archiveAck `shouldBe` Just (String "active")
      jsonField "summary_memory_id" archiveAck `shouldBe` Just (String testSummaryMemoryUUID)
      jsonField "summary_memory" archiveAck `shouldBe` Nothing
      jsonField "summary" archiveAck `shouldBe` Nothing
      jsonField "summary_memory_id" archiveWithoutSummary `shouldBe` Nothing
      jsonField "summary" archiveWithoutSummary `shouldBe` Nothing
      show finishAck `shouldNotContain` "finished with notes"
      show archiveAck `shouldNotContain` "archive summary"

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
      (jsonField "project" overviewWithDescriptions >>= jsonField "description_truncated") `shouldBe` Nothing
      (firstArrayItem "tasks" overviewWithDescriptions >>= jsonField "description_truncated") `shouldBe` Nothing
      (firstArrayItem "subprojects" overviewWithDescriptions >>= jsonField "description_truncated") `shouldBe` Nothing
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

    it "compacts HMem DTO-shaped project overview payloads" $ do
      let overview = compactProjectOverview typedProjectOverviewValue
          overviewWithDescriptions = compactProjectOverviewWithDescriptions typedProjectOverviewValue
      (jsonField "project" overview >>= jsonField "name") `shouldBe` Just (String "Typed project")
      (firstArrayItem "tasks" overview >>= jsonField "title") `shouldBe` Just (String "Typed task")
      jsonField "connected_memories" overview `shouldSatisfy` arrayLength 1
      show overview `shouldNotContain` "typed project description"
      show overview `shouldNotContain` "typed task description"
      show overview `shouldNotContain` "workspace_id"
      (jsonField "project" overviewWithDescriptions >>= jsonField "description") `shouldBe` Just (String "typed project description")
      (firstArrayItem "tasks" overviewWithDescriptions >>= jsonField "description") `shouldBe` Just (String "typed task description")
      jsonField "descriptions_omitted" overviewWithDescriptions `shouldBe` Nothing

    it "bounds project overview include_descriptions rows" $ do
      let longDescription = T.replicate (maxProjectOverviewDescriptionChars + 25) "x"
          overviewInput = object
            [ "project" .= object
                [ "id" .= parsedUUID
                , "name" .= ("Large project" :: Text)
                , "description" .= longDescription
                ]
            , "tasks" .=
                [ object
                    [ "id" .= parsedUUID2
                    , "title" .= ("Large task" :: Text)
                    , "description" .= longDescription
                    ]
                ]
            , "subprojects" .=
                [ object
                    [ "id" .= parsedUUID3
                    , "name" .= ("Large subproject" :: Text)
                    , "description" .= longDescription
                    ]
                ]
            ]
          overview = compactProjectOverviewWithDescriptions overviewInput
          projectRow = jsonField "project" overview
          taskRow = firstArrayItem "tasks" overview
          subprojectRow = firstArrayItem "subprojects" overview
      (projectRow >>= jsonField "description") `shouldSatisfy` boundedDescriptionValue
      (taskRow >>= jsonField "description") `shouldSatisfy` boundedDescriptionValue
      (subprojectRow >>= jsonField "description") `shouldSatisfy` boundedDescriptionValue
      (projectRow >>= jsonField "description_truncated") `shouldBe` Just (Bool True)
      (taskRow >>= jsonField "description_truncated") `shouldBe` Just (Bool True)
      (subprojectRow >>= jsonField "description_truncated") `shouldBe` Just (Bool True)
      show overview `shouldNotContain` T.unpack longDescription

    it "caps the number of project overview rows that include descriptions" $ do
      let describedTaskCount = maxProjectOverviewDescriptionRows + 2
          overviewInput = object
            [ "project" .= object
                [ "id" .= parsedUUID
                , "name" .= ("Large project" :: Text)
                , "description" .= ("project description" :: Text)
                ]
            , "tasks" .=
                [ object
                    [ "id" .= parsedUUID2
                    , "title" .= ("Task " <> T.pack (show i) :: Text)
                    , "description" .= ("task description" :: Text)
                    ]
                | i <- [1 .. describedTaskCount]
                ]
            , "subprojects" .= ([] :: [Value])
            ]
          overview = compactProjectOverviewWithDescriptions overviewInput
      describedOverviewRowCount overview `shouldBe` maxProjectOverviewDescriptionRows
      jsonField "description_limit" overview `shouldBe` Just (toJSON maxProjectOverviewDescriptionRows)
      jsonField "descriptions_omitted" overview `shouldBe` Just (toJSON (describedTaskCount + 1 - maxProjectOverviewDescriptionRows))

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

    it "parses detail, overview, and finish tools with expected description flag behavior" $ do
      parseToolCall "project_detail" (object ["project_id" .= testUUID]) `shouldBe` Right (ProjectDetailCall parsedUUID)
      parseToolCall "project_detail" (object ["project_id" .= testUUID, "include_descriptions" .= True]) `shouldBe` Right (ProjectDetailCall parsedUUID)
      parseToolCall "project_detail" (object []) `shouldSatisfy` isLeft
      parseToolCall "project_detail" (object ["project_id" .= ("not-a-uuid" :: Text)]) `shouldSatisfy` isLeft
      parseToolCall "project_overview" (object ["project_id" .= testUUID]) `shouldBe` Right (ProjectOverviewCall parsedUUID False)
      parseToolCall "project_overview" (object ["project_id" .= testUUID, "include_descriptions" .= True]) `shouldBe` Right (ProjectOverviewCall parsedUUID True)
      parseToolCall "task_detail" (object ["task_id" .= testUUID]) `shouldBe` Right (TaskDetailCall parsedUUID)
      parseToolCall "task_detail" (object ["task_id" .= testUUID, "include_description" .= True]) `shouldBe` Right (TaskDetailCall parsedUUID)
      parseToolCall "task_detail" (object []) `shouldSatisfy` isLeft
      parseToolCall "task_detail" (object ["task_id" .= ("not-a-uuid" :: Text)]) `shouldSatisfy` isLeft
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

    isLeft = \case
      Left _ -> True
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
  , "workspace_id" .= parsedUUID2
  , "summary" .= ("Linked memory" :: Text)
  , "memory_type" .= ("long_term" :: Text)
  , "importance" .= (5 :: Int)
  , "tags" .= (["linked"] :: [Text])
  , "content" .= ("linked full content should be omitted" :: Text)
  , "content_preview" .= ("linked preview should be omitted" :: Text)
  , "metadata" .= object ["kept" .= True]
  , "created_at" .= ("2026-05-20T00:00:00Z" :: Text)
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


finishedTaskValue :: Value
finishedTaskValue = object
  [ "id" .= parsedUUID
  , "workspace_id" .= parsedUUID2
  , "project_id" .= parsedUUID3
  , "title" .= ("Task" :: Text)
  , "description" .= ("full task description" :: Text)
  , "status" .= ("done" :: Text)
  , "priority" .= (9 :: Int)
  , "created_at" .= ("2026-05-20T00:00:00Z" :: Text)
  ]


archivedProjectValue :: Value
archivedProjectValue = object
  [ "id" .= parsedUUID
  , "workspace_id" .= parsedUUID2
  , "name" .= ("Project" :: Text)
  , "description" .= ("full project description" :: Text)
  , "status" .= ("archived" :: Text)
  , "priority" .= (8 :: Int)
  , "created_at" .= ("2026-05-20T00:00:00Z" :: Text)
  ]


typedProjectOverviewValue :: Value
typedProjectOverviewValue = toJSON $ ProjectOverview
  typedProject
  [typedTask]
  [typedProject]
  []
  [ConnectedMemorySummary parsedUUID "Typed memory" ScopeProject]
  (ProjectReadinessRollup 0 0 1 0 0 0 0 0 False)


typedProject :: Project
typedProject = Project
  parsedUUID
  parsedUUID2
  Nothing
  "Typed project"
  (Just "typed project description")
  ProjActive
  8
  (object ([] :: [Pair]))
  testTime
  testTime


typedTask :: Task
typedTask = Task
  parsedUUID
  parsedUUID2
  (Just parsedUUID3)
  Nothing
  "Typed task"
  (Just "typed task description")
  Todo
  9
  (object ([] :: [Pair]))
  Nothing
  Nothing
  4
  3
  testTime
  testTime


testTime :: UTCTime
testTime = UTCTime (fromGregorian 2026 5 20) (secondsToDiffTime 0)


compactResponseRegressionCases :: [(Text, Value -> Value, Value)]
compactResponseRegressionCases =
  [ ("task_create", compactTaskMutationAck "created", fullTaskValue)
  , ("project_create", compactProjectMutationAck "created", fullProjectValue)
  , ("memory_create", compactMemoryMutationAckWithTargets "created" (Just parsedUUID2) (Just parsedUUID3), fullMemoryValue)
  , ("project_spec_10_tasks", compactProjectSpecSummary, projectSpecRegressionValue)
  , ("project_overview", compactProjectOverview, projectOverviewRegressionValue)
  , ("project_detail", compactProjectDetail, projectDetailRegressionValue)
  , ("context_get", compactContextInfo, contextGetRegressionValue)
  , ("task_start", compactTaskStartSuccess, taskStartRegressionValue)
  , ("unified_search", compactSearchResults, unifiedSearchRegressionValue)
  , ("memory_graph", compactMemoryLinksList, memoryGraphRegressionValue)
  ]


dispatcherResponseRegressionCases :: [(Text, Text, Value)]
dispatcherResponseRegressionCases =
  [ ("task_create", "task_create", object
      [ "workspace_id" .= testUUID2
      , "project_id" .= parsedUUID3
      , "title" .= ("Task" :: Text)
      ])
  , ("project_create", "project_create", object
      [ "workspace_id" .= testUUID2
      , "name" .= ("Project" :: Text)
      ])
  , ("memory_create", "memory_create", object
      [ "workspace_id" .= testUUID2
      , "project_id" .= testUUID2
      , "task_id" .= parsedUUID3
      , "content" .= ("full memory content" :: Text)
      , "memory_type" .= ("long_term" :: Text)
      ])
  , ("project_spec_10_tasks", "project_spec", object
      [ "workspace_id" .= testUUID2
      , "name" .= ("Project" :: Text)
      , "tasks" .= replicate 10 (object ["title" .= ("Task" :: Text), "priority" .= (9 :: Int)])
      ])
  , ("project_overview", "project_overview", object
      [ "project_id" .= testUUID ])
  , ("project_detail", "project_detail", object
      [ "project_id" .= testProjectDetailUUID ])
  , ("context_get", "context_get", object
      [ "task_id" .= testUUID
      , "detail_level" .= ("medium" :: Text)
      ])
  , ("task_start", "task_start", object
      [ "task_id" .= testUUID
      , "detail_level" .= ("light" :: Text)
      ])
  , ("unified_search", "search", object
      [ "workspace_id" .= testUUID2
      , "entity_types" .= (["memory", "project", "task"] :: [Text])
      , "limit" .= (5 :: Int)
      ])
  , ("memory_graph", "memory_link", object
      [ "action" .= ("list" :: Text)
      , "memory_id" .= testUUID
      ])
  ]


projectSpecRegressionValue :: Value
projectSpecRegressionValue = object
  [ "project" .= fullProjectValue
  , "tasks" .= replicate 10 fullTaskValue
  , "tasks_failed" .= (0 :: Int)
  ]


projectOverviewRegressionValue :: Value
projectOverviewRegressionValue = object
  [ "project" .= fullProjectValue
  , "tasks" .= [fullTaskValue]
  , "subprojects" .= [fullProjectValue]
  , "connected_memories" .= [connectedMemoryValue parsedUUID "Project memory" "project"]
  , "readiness_rollup" .= object
      [ "completion_ready" .= False
      , "open_task_count" .= (2 :: Int)
      , "blocked_task_count" .= (1 :: Int)
      , "done_task_count" .= (0 :: Int)
      ]
  ]


projectDetailRegressionValue :: Value
projectDetailRegressionValue = object
  [ "project" .= object
      [ "id" .= testProjectDetailUUID
      , "workspace_id" .= parsedUUID2
      , "parent_id" .= testUUID2
      , "name" .= ("Detail project" :: Text)
      , "description" .= projectDetailDescription
      , "status" .= ("active" :: Text)
      , "priority" .= (6 :: Int)
      , "metadata" .= object ([] :: [Pair])
      , "created_at" .= ("2026-05-20T00:00:00Z" :: Text)
      ]
  , "tasks" .=
      [ projectDetailTaskValue testUUID "Todo direct" "todo" testProjectDetailUUID Nothing (Just "2026-06-01T00:00:00Z")
      , projectDetailTaskValue testUUID2 "Blocked direct" "blocked" testProjectDetailUUID (Just testUUID) Nothing
      , projectDetailTaskValue testUUID3 "In progress direct" "in_progress" testProjectDetailUUID Nothing Nothing
      , projectDetailTaskValue testNotesMemoryUUID "Done direct" "done" testProjectDetailUUID Nothing Nothing
      , projectDetailTaskValue testSummaryMemoryUUID "Cancelled direct" "cancelled" testProjectDetailUUID Nothing Nothing
      , projectDetailTaskValue testUUID "Other project blocked" "blocked" testUUID2 Nothing Nothing
      ]
  , "subprojects" .=
      [ projectDetailSubprojectValue testUUID2 "Active child" "active" testProjectDetailUUID
      , projectDetailSubprojectValue testUUID3 "Archived child" "archived" testProjectDetailUUID
      , projectDetailSubprojectValue testNotesMemoryUUID "Grandchild" "active" testUUID2
      ]
  , "linked_memories" .= [linkedMemoryValue]
  , "connected_memories" .= [connectedMemoryValue parsedUUID "Direct project memory" "project"]
  , "readiness_rollup" .= object
      [ "completion_ready" .= False
      , "closed_project_count" .= (1 :: Int)
      , "open_task_count" .= (4 :: Int)
      , "done_task_count" .= (1 :: Int)
      , "cancelled_task_count" .= (1 :: Int)
      , "blocked_task_count" .= (3 :: Int)
      , "dependency_blocked_task_count" .= (2 :: Int)
      , "open_dependency_count" .= (5 :: Int)
      ]
  ]


projectDetailDescription :: Text
projectDetailDescription = "Full project detail description that should not be truncated or copied to child rows."


projectDetailTaskValue :: Text -> Text -> Text -> Text -> Maybe Text -> Maybe Text -> Value
projectDetailTaskValue taskId title status projectId parentId dueAt = object
  [ "id" .= taskId
  , "workspace_id" .= parsedUUID2
  , "project_id" .= projectId
  , "parent_id" .= parentId
  , "title" .= title
  , "description" .= ("full task description" :: Text)
  , "status" .= status
  , "priority" .= (7 :: Int)
  , "due_at" .= dueAt
  , "dependency_count" .= (4 :: Int)
  , "memory_link_count" .= (3 :: Int)
  , "created_at" .= ("2026-05-20T00:00:00Z" :: Text)
  ]


projectDetailSubprojectValue :: Text -> Text -> Text -> Text -> Value
projectDetailSubprojectValue projectId name status parentId = object
  [ "id" .= projectId
  , "workspace_id" .= parsedUUID2
  , "parent_id" .= parentId
  , "name" .= name
  , "description" .= ("Child description should be omitted" :: Text)
  , "status" .= status
  , "priority" .= (5 :: Int)
  , "created_at" .= ("2026-05-20T00:00:00Z" :: Text)
  ]


taskOverviewRegressionValue :: Value
taskOverviewRegressionValue = object
  [ "task" .= fullTaskValue
  , "dependencies" .= ([] :: [Value])
  , "connected_memories" .= ([] :: [Value])
  , "readiness_rollup" .= object ["completion_ready" .= True]
  ]


contextGetRegressionValue :: Value
contextGetRegressionValue = object
  [ "task" .= fullTaskValue
  , "detail_level" .= ("medium" :: Text)
  , "task_memories" .= [connectedMemoryValue parsedUUID "Task memory" "task"]
  , "project_memories" .= [connectedMemoryValue parsedUUID2 "Project memory" "project"]
  , "workspace_memories" .= [connectedMemoryValue parsedUUID3 "Workspace memory" "workspace"]
  ]


taskStartRegressionValue :: Value
taskStartRegressionValue = object
  [ "task" .= fullTaskValue
  , "detail_level" .= ("light" :: Text)
  , "task_memories" .= ([] :: [Value])
  , "project_memories" .= ([] :: [Value])
  , "workspace_memories" .= ([] :: [Value])
  ]


unifiedSearchRegressionValue :: Value
unifiedSearchRegressionValue = object
  [ "memories" .= [searchMemoryValue]
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


memoryGraphRegressionValue :: Value
memoryGraphRegressionValue = toJSON
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


nextTasksRegressionValue :: Value
nextTasksRegressionValue = toJSON
  [ object
      [ "task" .= fullTaskValue
      , "dependency_blocked" .= False
      , "completion_gated" .= False
      , "open_descendant_count" .= (0 :: Int)
      , "open_dependency_count" .= (0 :: Int)
      ]
  , object
      [ "project" .= object
          [ "id" .= parsedUUID2
          , "name" .= ("project-only next-task regression row" :: Text)
          , "priority" .= (10 :: Int)
          ]
      ]
  ]


searchMemoryValue :: Value
searchMemoryValue = object
  [ "id" .= parsedUUID
  , "workspace_id" .= parsedUUID2
  , "summary" .= ("Search memory" :: Text)
  , "content" .= ("full memory content" :: Text)
  , "memory_type" .= ("long_term" :: Text)
  , "importance" .= (7 :: Int)
  , "tags" .= (["contract"] :: [Text])
  , "pinned" .= True
  , "metadata" .= object ([] :: [Pair])
  , "created_at" .= ("2026-05-20T00:00:00Z" :: Text)
  ]


connectedMemoryValue :: UUID.UUID -> Text -> Text -> Value
connectedMemoryValue mid summary scope = object
  [ "id" .= mid
  , "summary" .= summary
  , "scope" .= scope
  , "content" .= ("full memory content" :: Text)
  , "metadata" .= object ([] :: [Pair])
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


boundedDescriptionValue :: Maybe Value -> Bool
boundedDescriptionValue (Just (String textValue)) =
  T.length textValue <= maxProjectOverviewDescriptionChars && "…" `T.isSuffixOf` textValue
boundedDescriptionValue _ = False


describedOverviewRowCount :: Value -> Int
describedOverviewRowCount overview =
  projectDescriptionCount + taskDescriptionCount + subprojectDescriptionCount
  where
    projectDescriptionCount = if hasObjectField "description" (jsonField "project" overview) then 1 else 0
    taskDescriptionCount = length [() | task <- arrayItems "tasks" overview, hasObjectField "description" (Just task)]
    subprojectDescriptionCount = length [() | subproject <- arrayItems "subprojects" overview, hasObjectField "description" (Just subproject)]


arrayItems :: Text -> Value -> [Value]
arrayItems field value = case jsonField field value of
  Just (Array arr) -> toList arr
  _ -> []


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


mcpTextContent :: Value -> Maybe Text
mcpTextContent (Object o) = do
  Array contentItems <- KM.lookup (Key.fromText "content") o
  Object firstItem <- case toList contentItems of
    item : _ -> Just item
    []       -> Nothing
  case KM.lookup (Key.fromText "text") firstItem of
    Just (String textValue) -> Just textValue
    _                       -> Nothing
mcpTextContent _ = Nothing


jsonLine :: Value -> BS8.ByteString
jsonLine = BL.toStrict . encode


withMockHmemServer :: (Manager -> String -> IO a) -> IO a
withMockHmemServer action =
  testWithApplication (pure mockHmemApplication) $ \port -> do
    mgr <- newManager defaultManagerSettings
    action mgr ("http://127.0.0.1:" <> show port)


withRealHmemServer :: (Manager -> String -> IO a) -> IO a
withRealHmemServer action =
  withTestEnv $ \env -> do
    tracker <- newAccessTracker env.pool 3600
    wsState <- newWSState
    let cfg = Config.defaultConfig { Config.cors = Config.CorsConfig ["*"] }
    app <- mkApp id cfg.auth cfg.cors cfg.rateLimit env.pool tracker wsState Nothing True
    testWithApplication (pure app) $ \port -> do
      mgr <- newManager defaultManagerSettings
      action mgr ("http://127.0.0.1:" <> show port)


callTool :: Manager -> String -> Text -> Value -> IO Value
callTool mgr base = callToolWithApiKey mgr base Nothing


callToolWithApiKey :: Manager -> String -> Maybe Text -> Text -> Value -> IO Value
callToolWithApiKey mgr base mApiKey name args = do
  result <- callToolRawWithApiKey mgr base mApiKey name args
  case mcpTextValue result of
    Just value -> pure value
    Nothing    -> expectationFailure ("Expected MCP JSON text for " <> T.unpack name <> ", got: " <> show result) >> pure Null


callToolRaw :: Manager -> String -> Text -> Value -> IO Value
callToolRaw mgr base = callToolRawWithApiKey mgr base Nothing


callToolRawWithApiKey :: Manager -> String -> Maybe Text -> Text -> Value -> IO Value
callToolRawWithApiKey mgr base mApiKey name args = handleToolCall mgr base mApiKey $ object
  [ "name" .= name
  , "arguments" .= args
  ]


callMockTool :: Manager -> String -> Text -> Value -> IO Value
callMockTool = callTool


callMockToolWithApiKey :: Manager -> String -> Maybe Text -> Text -> Value -> IO Value
callMockToolWithApiKey = callToolWithApiKey


callMockToolRaw :: Manager -> String -> Text -> Value -> IO Value
callMockToolRaw = callToolRaw


expectTextField :: Text -> Value -> IO Text
expectTextField field value = case jsonField field value of
  Just (String textValue) -> pure textValue
  other -> expectationFailure ("Expected text field " <> T.unpack field <> ", got: " <> show other) >> pure ""


searchProjectNames :: Value -> [Text]
searchProjectNames value =
  [ name
  | row <- arrayFieldItems "projects" value
  , Just project <- [jsonField "project" row]
  , Just name <- [textField "name" project]
  ]


searchTaskTitles :: Value -> [Text]
searchTaskTitles value =
  [ title
  | row <- arrayFieldItems "tasks" value
  , Just task <- [jsonField "task" row]
  , Just title <- [textField "title" task]
  ]


arrayFieldItems :: Text -> Value -> [Value]
arrayFieldItems field value = case jsonField field value of
  Just (Array items) -> toList items
  _ -> []


textField :: Text -> Value -> Maybe Text
textField field value = case jsonField field value of
  Just (String textValue) -> Just textValue
  _ -> Nothing


mockHmemApplication :: Wai.Application
mockHmemApplication req respond = do
  let respondJson value = respond $ Wai.responseLBS status200 [(hContentType, "application/json")] (encode value)
      respondBad message = respond $ Wai.responseLBS status400 [(hContentType, "application/json")] (encode $ object ["error" .= (message :: Text)])
      jsonBodyValue = do
        body <- Wai.strictRequestBody req
        pure (decode body)
      jsonBodySatisfies predicate = maybe False predicate <$> jsonBodyValue
      hasBearer token = lookup "Authorization" (Wai.requestHeaders req) == Just ("Bearer " <> TE.encodeUtf8 token)
  case (Wai.requestMethod req, Wai.rawPathInfo req) of
    (method, "/api/v1/search")
      | method == methodPost -> do
          mBody <- jsonBodyValue
          if mBody == Just unifiedSearchRequestBody
            then respondJson unifiedSearchRegressionValue
            else respondBad $ "unexpected search body: " <> T.pack (show mBody)
    (method, "/api/v1/memories")
      | method == methodPost -> do
          mBody <- jsonBodyValue
          case mBody >>= mockMemoryCreateResponse of
            Just value -> respondJson value
            Nothing -> respondBad $ "unexpected memory create body: " <> T.pack (show mBody)
    (method, "/api/v1/memories/aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee")
      | method == methodGet -> respondJson fullMemoryValue
      | method == methodPut -> do
          ok <- jsonBodySatisfies $ \body ->
            jsonField "content" body == Just (String "revised memory content")
              && jsonField "workspace_id" body == Nothing
          if ok then respondJson fullMemoryValue else respondBad "unexpected memory_update body"
    (method, "/api/v1/memories/aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee/tags")
      | method == methodPut -> do
          ok <- jsonBodySatisfies (== toJSON (["new-tag"] :: [Text]))
          if ok then respondJson (object ["tags" .= (["new-tag"] :: [Text])]) else respondBad "unexpected memory_update tags body"
    (method, "/api/v1/memories/aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee/links")
      | method == methodGet -> respondJson memoryGraphRegressionValue
    (method, "/api/v1/projects/aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee/overview")
      | method == methodGet && Wai.rawQueryString req == "?extra_context=false" -> respondJson projectOverviewRegressionValue
    (method, "/api/v1/projects/11111111-2222-3333-4444-555555555555/overview")
      | method == methodGet && Wai.rawQueryString req == "?extra_context=false" -> respondJson typedProjectOverviewValue
    (method, "/api/v1/projects/55555555-6666-7777-8888-999999999999/overview")
      | method == methodGet && Wai.rawQueryString req == "?extra_context=false" -> respondJson projectDetailRegressionValue
    (method, "/api/v1/projects/aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee/next-tasks")
      | method == methodGet && queryMatches [("limit", Just "3"), ("include_blocked", Just "true")] req && hasBearer "request-shape-token" -> respondJson nextTasksRegressionValue
      | method == methodGet -> respondBad "unexpected project_next_tasks query or auth header"
    (method, "/api/v1/tasks/aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee/overview")
      | method == methodGet && Wai.rawQueryString req == "?extra_context=false" -> respondJson taskOverviewRegressionValue
    (method, "/api/v1/tasks/aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee/context")
      | method == methodGet && Wai.rawQueryString req == "?detail_level=light" -> respondJson taskStartRegressionValue
      | method == methodGet && Wai.rawQueryString req == "?detail_level=medium" -> respondJson contextGetRegressionValue
    (method, "/api/v1/tasks/aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee")
      | method == methodGet -> respondJson fullTaskValue
      | method == methodPut -> do
          mBody <- jsonBodyValue
          case mBody >>= mockTaskStatusUpdateResponse of
            Just value -> respondJson value
            Nothing -> respondBad $ "unexpected task status update body: " <> T.pack (show mBody)
    (method, "/api/v1/tasks/22222222-3333-4444-5555-666666666666")
      | method == methodGet -> respondJson (taskValue parsedUUID3 "Target task" "todo" Nothing (Just parsedUUID3))
    (method, "/api/v1/projects/aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee")
      | method == methodGet -> respondJson fullProjectValue
      | method == methodPut -> do
          mBody <- jsonBodyValue
          if maybe False isProjectArchiveBody mBody
            then respondJson archivedProjectValue
            else respondBad $ "unexpected project archive body: " <> T.pack (show mBody)
    (method, "/api/v1/projects")
      | method == methodPost -> do
          mBody <- jsonBodyValue
          if maybe False isProjectCreateBody mBody
            then respondJson fullProjectValue
            else respondBad $ "unexpected project create body: " <> T.pack (show mBody)
    (method, "/api/v1/tasks")
      | method == methodPost -> do
          mBody <- jsonBodyValue
          if maybe False isTaskCreateBody mBody
            then respondJson fullTaskValue
            else respondBad $ "unexpected task create body: " <> T.pack (show mBody)
    _ -> respond $ Wai.responseLBS status404 [(hContentType, "application/json")] "{\"error\":\"not found\"}"


queryMatches :: [(BS8.ByteString, Maybe BS8.ByteString)] -> Wai.Request -> Bool
queryMatches expected req = sort (Wai.queryString req) == sort expected


unifiedSearchRequestBody :: Value
unifiedSearchRequestBody = object
  [ "workspace_id" .= testUUID2
  , "entity_types" .= (["memory", "project", "task"] :: [Text])
  , "limit" .= (5 :: Int)
  ]


mockMemoryCreateResponse :: Value -> Maybe Value
mockMemoryCreateResponse body
  | body == standaloneMemoryCreateBody = Just fullMemoryValue
  | body == taskNotesMemoryCreateBody = Just notesMemoryValue
  | body == projectSummaryMemoryCreateBody = Just summaryMemoryValue
  | otherwise = Nothing


standaloneMemoryCreateBody :: Value
standaloneMemoryCreateBody = object
  [ "workspace_id" .= testUUID2
  , "project_id" .= testUUID2
  , "task_id" .= testUUID3
  , "content" .= ("full memory content" :: Text)
  , "memory_type" .= ("long_term" :: Text)
  ]


taskNotesMemoryCreateBody :: Value
taskNotesMemoryCreateBody = object
  [ "workspace_id" .= testUUID2
  , "task_id" .= testUUID
  , "content" .= ("finished with notes" :: Text)
  , "memory_type" .= ("long_term" :: Text)
  , "importance" .= (6 :: Int)
  , "source" .= ("inferred" :: Text)
  , "tags" .= (["task-notes"] :: [Text])
  ]


projectSummaryMemoryCreateBody :: Value
projectSummaryMemoryCreateBody = object
  [ "workspace_id" .= testUUID2
  , "project_id" .= testUUID
  , "content" .= ("archive summary" :: Text)
  , "memory_type" .= ("long_term" :: Text)
  , "importance" .= (7 :: Int)
  , "source" .= ("inferred" :: Text)
  , "tags" .= (["project-summary"] :: [Text])
  ]


notesMemoryValue :: Value
notesMemoryValue = object
  [ "id" .= testNotesMemoryUUID
  , "workspace_id" .= testUUID2
  , "content" .= ("finished with notes" :: Text)
  , "memory_type" .= ("long_term" :: Text)
  ]


summaryMemoryValue :: Value
summaryMemoryValue = object
  [ "id" .= testSummaryMemoryUUID
  , "workspace_id" .= testUUID2
  , "content" .= ("archive summary" :: Text)
  , "memory_type" .= ("long_term" :: Text)
  ]


mockTaskStatusUpdateResponse :: Value -> Maybe Value
mockTaskStatusUpdateResponse body
  | body == object ["status" .= ("done" :: Text)] = Just finishedTaskValue
  | body == object ["status" .= ("in_progress" :: Text)] = Just (taskValue parsedUUID "Task" "in_progress" Nothing (Just parsedUUID3))
  | otherwise = Nothing


isProjectArchiveBody :: Value -> Bool
isProjectArchiveBody body = body == object ["status" .= ("archived" :: Text)]


isProjectCreateBody :: Value -> Bool
isProjectCreateBody body = body `elem`
  [ object
      [ "workspace_id" .= testUUID2
      , "name" .= ("Project" :: Text)
      ]
  , object
      [ "workspace_id" .= testUUID2
      , "name" .= ("Workflow project" :: Text)
      , "description" .= ("workflow project description" :: Text)
      , "priority" .= (6 :: Int)
      ]
  ]


isTaskCreateBody :: Value -> Bool
isTaskCreateBody body = body `elem`
  [ object
      [ "workspace_id" .= testUUID2
      , "project_id" .= testUUID3
      , "title" .= ("Task" :: Text)
      ]
  , object
      [ "workspace_id" .= testUUID2
      , "project_id" .= testUUID
      , "title" .= ("Task" :: Text)
      , "priority" .= (9 :: Int)
      ]
  , object
      [ "workspace_id" .= testUUID2
      , "project_id" .= testUUID
      , "title" .= ("First task" :: Text)
      , "description" .= ("first task description" :: Text)
      ]
  , object
      [ "workspace_id" .= testUUID2
      , "project_id" .= testUUID
      , "title" .= ("Second task" :: Text)
      , "description" .= ("second task description" :: Text)
      , "priority" .= (8 :: Int)
      ]
  ]


readCompactResponseFixtures :: IO Value
readCompactResponseFixtures = do
  bytes <- readFirstExisting
    [ "hmem-mcp/test/fixtures/mcp-compact-responses.json"
    , "test/fixtures/mcp-compact-responses.json"
    , "../hmem-mcp/test/fixtures/mcp-compact-responses.json"
    ]
  case eitherDecode bytes of
    Right value -> pure value
    Left err    -> fail $ "Could not decode compact response fixture: " <> err


readFirstExisting :: [FilePath] -> IO BL.ByteString
readFirstExisting [] = fail "Could not find compact response fixture"
readFirstExisting (path : paths) = do
  result <- try @IOException (BL.readFile path)
  case result of
    Right bytes -> pure bytes
    Left _      -> readFirstExisting paths


assertCompactResponseFixture :: Value -> (Text, Value -> Value, Value) -> Expectation
assertCompactResponseFixture fixtures (name, shaper, raw) =
  case (fixturePayload name fixtures, fixtureMaxChars name fixtures, mcpTextValue (mcpResultWith shaper (encode raw))) of
    (Just expected, Just maxChars, Just actual) -> do
      actual `shouldBe` expected
      encodedCharCount actual `shouldSatisfy` (<= maxChars)
      shouldOmitDefaultNoise name actual
    (Nothing, _, _) -> expectationFailure $ "Missing fixture payload for " <> T.unpack name
    (_, Nothing, _) -> expectationFailure $ "Missing fixture max_chars for " <> T.unpack name
    (_, _, Nothing) -> expectationFailure $ "Expected MCP JSON text for " <> T.unpack name


assertDispatcherResponseFixture :: Value -> Manager -> String -> (Text, Text, Value) -> Expectation
assertDispatcherResponseFixture fixtures mgr base (name, toolName, args) = do
  result <- callMockToolRaw mgr base toolName args
  case (fixturePayload name fixtures, fixtureMaxChars name fixtures, fixtureMaxMcpEnvelopeChars name fixtures, mcpTextValue result) of
    (Just expected, Just maxChars, Just maxMcpEnvelopeChars, Just actual) -> do
      actual `shouldBe` expected
      assertEncodedCharBudget (name <> " payload") actual maxChars
      assertEncodedCharBudget (name <> " MCP envelope") result maxMcpEnvelopeChars
      shouldOmitDefaultNoise name actual
    (Nothing, _, _, _) -> expectationFailure $ "Missing fixture payload for " <> T.unpack name
    (_, Nothing, _, _) -> expectationFailure $ "Missing fixture max_chars for " <> T.unpack name
    (_, _, Nothing, _) -> expectationFailure $ "Missing fixture max_mcp_envelope_chars for " <> T.unpack name
    (_, _, _, Nothing) -> expectationFailure $ "Expected dispatcher MCP JSON text for " <> T.unpack name <> ", got: " <> show result


assertJsonRpcResponseFixture :: Value -> Manager -> String -> TVar Bool -> TVar (Maybe UUID.UUID) -> (Text, Text, Value) -> Expectation
assertJsonRpcResponseFixture fixtures mgr base initialized wsContext (name, toolName, args) = do
  mResponse <- handleRequest mgr base Nothing initialized wsContext $
    JsonRpcRequest (Just (String (name <> "-jsonrpc"))) "tools/call" $ Just $ object
      [ "name" .= toolName
      , "arguments" .= args
      ]
  case (mResponse, fixturePayload name fixtures, fixtureMaxChars name fixtures, fixtureMaxJsonRpcStdioChars name fixtures, mResponse >>= jsonField "result" >>= mcpTextValue) of
    (Just response, Just expected, Just maxChars, Just maxJsonRpcStdioChars, Just actual) -> do
      jsonField "jsonrpc" response `shouldBe` Just (String "2.0")
      jsonField "id" response `shouldBe` Just (String (name <> "-jsonrpc"))
      actual `shouldBe` expected
      assertEncodedCharBudget (name <> " payload") actual maxChars
      assertEncodedLineBudget (name <> " JSON-RPC stdio envelope") response maxJsonRpcStdioChars
      shouldOmitDefaultNoise name actual
    (Nothing, _, _, _, _) -> expectationFailure $ "Expected JSON-RPC response for " <> T.unpack name
    (_, Nothing, _, _, _) -> expectationFailure $ "Missing fixture payload for " <> T.unpack name
    (_, _, Nothing, _, _) -> expectationFailure $ "Missing fixture max_chars for " <> T.unpack name
    (_, _, _, Nothing, _) -> expectationFailure $ "Missing fixture max_jsonrpc_stdio_chars for " <> T.unpack name
    (_, _, _, _, Nothing) -> expectationFailure $ "Expected JSON-RPC MCP JSON text for " <> T.unpack name <> ", got: " <> show mResponse


assertEncodedCharBudget :: Text -> Value -> Int -> Expectation
assertEncodedCharBudget label value maxChars =
  let actualChars = encodedCharCount value
  in if actualChars <= maxChars
    then pure ()
    else expectationFailure $
      T.unpack label <> " encoded size " <> show actualChars <> " exceeds budget " <> show maxChars


assertEncodedLineBudget :: Text -> Value -> Int -> Expectation
assertEncodedLineBudget label value maxChars =
  let actualChars = encodedLineCharCount value
  in if actualChars <= maxChars
    then pure ()
    else expectationFailure $
      T.unpack label <> " encoded line size " <> show actualChars <> " exceeds budget " <> show maxChars


fixturePayload :: Text -> Value -> Maybe Value
fixturePayload name = fixtureField name "payload"


fixtureMaxChars :: Text -> Value -> Maybe Int
fixtureMaxChars name fixtures = do
  raw <- fixtureField name "max_chars" fixtures
  case fromJSON raw of
    Success maxChars -> Just maxChars
    Error _          -> Nothing


fixtureMaxMcpEnvelopeChars :: Text -> Value -> Maybe Int
fixtureMaxMcpEnvelopeChars name fixtures = do
  raw <- fixtureField name "max_mcp_envelope_chars" fixtures
  case fromJSON raw of
    Success maxChars -> Just maxChars
    Error _          -> Nothing


fixtureMaxJsonRpcStdioChars :: Text -> Value -> Maybe Int
fixtureMaxJsonRpcStdioChars name fixtures = do
  raw <- fixtureField name "max_jsonrpc_stdio_chars" fixtures
  case fromJSON raw of
    Success maxChars -> Just maxChars
    Error _          -> Nothing


fixtureField :: Text -> Text -> Value -> Maybe Value
fixtureField name field (Object root) = do
  Object entry <- KM.lookup (Key.fromText name) root
  KM.lookup (Key.fromText field) entry
fixtureField _ _ _ = Nothing


encodedCharCount :: Value -> Int
encodedCharCount = fromIntegral . BL.length . encode


encodedLineCharCount :: Value -> Int
encodedLineCharCount = fromIntegral . BL.length . encodeStdioResponse


shouldOmitDefaultNoise :: Text -> Value -> Expectation
shouldOmitDefaultNoise name payload =
  mapM_ assertAbsent (defaultNoisySubstringsFor name)
  where
    rendered = TE.decodeUtf8 $ BL.toStrict $ encode payload
    assertAbsent needle =
      if needle `T.isInfixOf` rendered
        then expectationFailure $ "Unexpected default-noise substring " <> T.unpack needle <> " in fixture " <> T.unpack name
        else pure ()


defaultNoisySubstringsFor :: Text -> [Text]
defaultNoisySubstringsFor "project_detail" = filter (/= "\"description\"") defaultNoisySubstrings
defaultNoisySubstringsFor _ = defaultNoisySubstrings


defaultNoisySubstrings :: [Text]
defaultNoisySubstrings =
  [ "\"workspace_id\""
  , "\"created_at\""
  , "\"updated_at\""
  , "\"metadata\""
  , "\"content\""
  , "\"content_preview\""
  , "\"description\""
  , "full memory content"
  , "full project description"
  , "full task description"
  , "linked full content should be omitted"
  ]


toolDescriptionShouldContain :: Text -> Text -> Expectation
toolDescriptionShouldContain name needle = case toolDescription name of
  Just desc -> desc `shouldSatisfy` T.isInfixOf needle
  Nothing   -> expectationFailure $ "Missing tool description for " <> T.unpack name
