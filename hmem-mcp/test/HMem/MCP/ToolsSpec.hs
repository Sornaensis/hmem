module HMem.MCP.ToolsSpec (spec) where

import Control.Concurrent.STM
import Control.Exception (bracket)
import Control.Monad (filterM, forM_)
import Data.Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Foldable (toList)
import Data.List (sort)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.UUID (UUID)
import Network.HTTP.Client (Manager, closeManager, defaultManagerSettings, managerIdleConnectionCount, newManager)
import Network.HTTP.Client qualified as Client
import Network.HTTP.Types (Status, methodDelete, methodGet, methodPost, methodPut, status200, status204, status400, status401, status403, status404, status409, status413, status429, status500, statusCode)
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp (testWithApplication)
import System.Directory (doesFileExist)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import Test.Hspec

import HMem.Config qualified as Config
import HMem.AgentSession qualified as AgentSession
import HMem.AgentSession.Transport qualified as SessionWire
import HMem.DB.ChangeStream (ChangeScope(..), OutboxRecord(..), listOutboxAfter)
import HMem.DB.Pool qualified as DBPool
import HMem.DB.RequestContext (ActorType(..), Principal(..), PrincipalAuthority(..), withPrincipalContext)
import HMem.DB.TestHarness (TestDb(..), TestEnv(..), createTestWorkspace, withTestEnv)
import HMem.BotIdentity (BotIdentity, mkBotIdentity)
import HMem.MCP.Server (MCPContext(..), handleStdioLine)
import HMem.MCP.Tools
import HMem.Server.AccessTracker (newAccessTracker)
import HMem.Server.App (mkAppWithChangeStream)
import HMem.Server.Snapshot (materializeSnapshot)
import HMem.Server.WebSocket (newWSState)
import HMem.Types (BatchMoveTasksRequest(..), CreateObservation(..), ObservationSubject(..), SubjectKind(..), ReviewedObservationUpdate(..), Workspace(..), maxObservationSubjects)

spec :: Spec
spec = do
  describe "Agent session MCP" $ do
    it "forwards explicit runtime declarations and null clearing separately from connection attribution" $ do
      requests <- newTVarIO []
      withSessionMock requests $ \manager base -> do
        let runtime=object ["model" .= ("Actual target model" :: Text)]
            args revision value=object ["workspace_id" .= workspaceId,"session_id" .= observationId,"expected_runtime_revision" .= (revision :: Int),"runtime_identity" .= value]
        changed <- call manager base "session_set_runtime" (args 0 runtime)
        jsonPath ["summary","runtime_identity"] changed `shouldBe` Just runtime
        jsonPath ["summary","creator_bot_identity"] changed `shouldBe` Just (toJSON testIdentity)
        cleared <- call manager base "session_set_runtime" (args 1 Null)
        jsonPath ["summary","runtime_identity"] cleared `shouldBe` Nothing
        jsonPath ["summary","runtime_revision"] cleared `shouldBe` Just (Number 2)
        missing <- handleToolCall manager base Nothing testIdentity (object ["name" .= ("session_set_runtime" :: Text),"arguments" .= object ["workspace_id" .= workspaceId,"session_id" .= observationId,"expected_runtime_revision" .= (2 :: Int)]])
        missing `shouldSatisfy` isMcpError
      observed <- readTVarIO requests
      map (.requestMethod) observed `shouldBe` [methodPut,methodPut]
      map (.requestPath) observed `shouldBe` replicate 2 "/api/v1/workspaces/11111111-2222-3333-4444-555555555555/sessions/aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee/runtime"
      decode (observed !! 1).requestBody `shouldBe` Just (object ["expected_runtime_revision" .= (1 :: Int),"runtime_identity" .= Null])
      withSearchResponse status409 (encode (object ["error" .= ("session_runtime_conflict" :: Text),"message" .= ("sensitive-content" :: Text)])) $ \manager base -> do
        result <- handleToolCall manager base Nothing testIdentity (object ["name" .= ("session_set_runtime" :: Text),"arguments" .= object ["workspace_id" .= workspaceId,"session_id" .= observationId,"expected_runtime_revision" .= (0 :: Int),"runtime_identity" .= Null]])
        jsonPath ["structuredContent","error","code"] result `shouldBe` Just (String "session_runtime_conflict")
        result `shouldSatisfy` contains "runtime_revision"
        result `shouldSatisfy` (not . contains "sensitive-content")

    it "advertises strict explicit management tools and bounded range/list windows without ingestion or local file controls" $ do
      let names = [name | Object definition <- toolDefinitions, Just (String name) <- [KM.lookup "name" definition], "session_" `T.isPrefixOf` name, not ("session_upload_" `T.isPrefixOf` name)]
      sort names `shouldBe` sort ["session_create", "session_list", "session_detail", "session_tasks", "session_associate", "session_terminate", "session_read", "session_evidence", "session_set_runtime"]
      forM_ names $ \name -> do
        let [definition] = [value | value <- toolDefinitions, jsonField "name" value == Just (String name)]
        jsonPath ["inputSchema", "additionalProperties"] definition `shouldBe` Just (Bool False)
        jsonPath ["inputSchema", "properties", "workspace_id", "type"] definition `shouldBe` Just (String "string")
      let [range] = [value | value <- toolDefinitions, jsonField "name" value == Just (String "session_read")]
      jsonPath ["inputSchema", "properties", "limit", "default"] range `shouldBe` Just (Number 4096)
      jsonPath ["inputSchema", "properties", "limit", "maximum"] range `shouldBe` Just (Number 1048576)
      let [create] = [value | value <- toolDefinitions, jsonField "name" value == Just (String "session_create")]
      jsonPath ["inputSchema","required"] create `shouldBe` Just (toJSON (["session_id","format_label","title"] :: [Text]))
      create `shouldSatisfy` contains "VERY SHORT DESCRIPTIVE TITLE"
      jsonPath ["inputSchema","properties","parent_session_id","type"] create `shouldBe` Just (String "string")

    it "rejects strict input, unsupported bounds and missing workspace with fixed errors before forwarding" $ do
      requests <- newTVarIO []
      withMock requests $ \manager base -> forM_
        [("session_detail", object ["session_id" .= observationId])
        ,("session_detail", object ["workspace_id" .= workspaceId, "session_id" .= observationId, "transcript-secret" .= ("secret-data" :: Text)])
        ,("session_read", object ["workspace_id" .= workspaceId, "session_id" .= observationId, "limit" .= (1048577 :: Int)])
        ,("session_read", object ["workspace_id" .= workspaceId, "session_id" .= observationId, "offset" .= (-1 :: Int)])
        ,("session_list", object ["workspace_id" .= workspaceId, "limit" .= (101 :: Int)])
        ,("session_list", object ["workspace_id" .= workspaceId, "before_id" .= observationId])
        ,("session_terminate", object ["workspace_id" .= workspaceId, "session_id" .= observationId, "expected_offset" .= ("secret-value" :: Text)])] $ \(name,args) -> do
          result <- handleToolCall manager base Nothing testIdentity (object ["name" .= (name :: Text), "arguments" .= args])
          jsonPath ["structuredContent", "error", "code"] result `shouldBe` Just (String "invalid_arguments")
          result `shouldSatisfy` (not . contains "secret")
      (length <$> readTVarIO requests) `shouldReturn` 0

    it "forwards one identity, authorization, provenance and exact terminate/create bodies without transcript echoes" $ do
      requests <- newTVarIO []
      withEnvironment "HMEM_MCP_PROVENANCE_TOKEN" "session-test-provenance" $
        withSessionMock requests $ \manager base -> do
          created <- callWithKey manager base (Just "session-credential") "session_create"
            (object ["workspace_id" .= workspaceId, "session_id" .= observationId, "title" .= ("Test session work" :: Text), "format_label" .= ("provider-jsonl" :: Text)])
          jsonField "action" created `shouldBe` Just (String "created")
          created `shouldSatisfy` contains "creator_actor_label"
          _ <- callWithKey manager base (Just "session-credential") "session_terminate" sessionTerminateArguments
          _ <- callWithKey manager base Nothing "session_associate" (object ["workspace_id" .= workspaceId, "session_id" .= observationId, "task_id" .= workspaceId])
          pure ()
      observed <- readTVarIO requests
      map (.requestMethod) observed `shouldBe` [methodPost,methodPost,methodPut]
      map (.requestIdentityHeaders) observed `shouldBe` replicate 3 [BL.toStrict (encode testIdentity)]
      map (.authorization) observed `shouldBe` [Just "Bearer session-credential",Just "Bearer session-credential",Nothing]
      map (.requestChangeCause) observed `shouldBe` replicate 3 (Just "mcp")
      map (.requestMcpProvenance) observed `shouldBe` replicate 3 (Just "session-test-provenance")
      map (.requestId) observed `shouldSatisfy` all (/= Nothing)
      decode (head observed).requestBody `shouldBe` Just (object ["id" .= observationId, "title" .= ("Test session work" :: Text), "format_label" .= ("provider-jsonl" :: Text)])
      decode (observed !! 1).requestBody `shouldBe` Just (object ["expected_offset" .= (0 :: Int), "expected_revision" .= (0 :: Int), "final_bytes" .= (0 :: Int), "final_sha256" .= AgentSession.sessionSha256 ""])

    it "projects HTTP status and allowlisted codes without arbitrary error messages or content" $
      forM_ [status400,status401,status403,status404,status409,status413,status500] $ \status ->
        withSearchResponse status (encode (object ["error" .= ("session_conflict" :: Text), "message" .= ("sensitive-bytea" :: Text), "data_base64" .= ("private-payload" :: Text)])) $ \manager base -> do
          result <- handleToolCall manager base (Just "private-token") testIdentity (object ["name" .= ("session_detail" :: Text), "arguments" .= sessionDetailArguments])
          jsonPath ["structuredContent", "error", "http_status"] result `shouldBe` Just (Number (fromIntegral (statusCode status)))
          jsonPath ["structuredContent", "error", "code"] result `shouldBe` Just (String "session_conflict")
          mapM_ (\secret -> result `shouldSatisfy` (not . contains secret)) ["sensitive-bytea", "private-payload", "private-token"]

    it "rejects oversized known and chunked responses, invalid JSON, invalid base64 and foreign metadata without raw diagnostics" $ do
      forM_ [status200,status500] $ \status -> forM_ [False,True] $ \known -> do
        let bytes = BL.replicate 2097153 120
            headers = [("Content-Length", "2097153") | known]
            app _ respond = respond (Wai.responseLBS status headers bytes)
        testWithApplication (pure app) $ \port -> bracket (newManager defaultManagerSettings {managerIdleConnectionCount = 0}) closeManager $ \manager -> do
          result <- handleToolCall manager ("http://127.0.0.1:" <> show port) Nothing testIdentity (object ["name" .= ("session_detail" :: Text), "arguments" .= sessionDetailArguments])
          jsonPath ["structuredContent", "error", "code"] result `shouldBe` Just (String "response_too_large")
          BL.length (encode result) `shouldSatisfy` (< 1000)
      forM_ ["sensitive-invalid-json", encode (setField "workspace_id" (String observationId) sessionMetadata)] $ \body ->
        withSearchResponse status200 body $ \manager base -> do
          result <- handleToolCall manager base Nothing testIdentity (object ["name" .= ("session_detail" :: Text), "arguments" .= sessionDetailArguments])
          result `shouldSatisfy` isMcpError
          result `shouldSatisfy` (not . contains "sensitive-invalid-json")
      withSearchResponse status200 (encode (object ["session" .= sessionMetadata, "offset" .= (0 :: Int), "next_offset" .= (0 :: Int), "has_more" .= False, "data_base64" .= ("%%%private-base64" :: Text)])) $ \manager base -> do
        result <- handleToolCall manager base Nothing testIdentity (object ["name" .= ("session_read" :: Text), "arguments" .= sessionDetailArguments])
        jsonPath ["structuredContent", "error", "code"] result `shouldBe` Just (String "invalid_response")
        result `shouldSatisfy` (not . contains "private-base64")

    it "returns the exact maximum 1 MiB range without clipping or a spurious continuation" $ do
      let bytes = BS.replicate 1048576 97
          metadata = setField "committed_bytes" (Number 1048576) sessionMetadata
          body = object ["session" .= metadata, "offset" .= (0 :: Int), "data_base64" .= SessionWire.encodeSessionBytes bytes, "next_offset" .= (1048576 :: Int), "has_more" .= False]
      withSearchResponse status200 (encode body) $ \manager base -> do
        result <- call manager base "session_read" (setField "limit" (Number 1048576) sessionDetailArguments)
        jsonField "data_base64" result `shouldBe` Just (String (SessionWire.encodeSessionBytes bytes))
        jsonField "next_offset" result `shouldBe` Just (Number 1048576)
        jsonField "next_request" result `shouldBe` Nothing

    it "pins list and task continuations to resolved workspace and omits next_request on final empty pages" $ do
      requests <- newTVarIO []
      withSessionMock requests $ \manager base -> do
        listed <- call manager base "session_list" (object ["workspace_id" .= workspaceId, "task_id" .= observationId, "limit" .= (1 :: Int)])
        let Just next = jsonPath ["next_request", "arguments"] listed
        jsonField "workspace_id" next `shouldBe` Just (String workspaceId)
        jsonField "task_id" next `shouldBe` Just (String observationId)
        jsonField "before_id" next `shouldBe` Just (String observationId)
        done <- call manager base "session_list" next
        jsonField "items" done `shouldBe` Just (toJSON ([] :: [Value]))
        jsonField "next_request" done `shouldBe` Nothing
        tasks <- call manager base "session_tasks" sessionDetailArguments
        let Just taskNext = jsonPath ["next_request", "arguments"] tasks
        jsonField "session_id" taskNext `shouldBe` Just (String observationId)
        finalTasks <- call manager base "session_tasks" taskNext
        jsonField "next_request" finalTasks `shouldBe` Nothing
      observed <- readTVarIO requests
      map (.requestQuery) observed `shouldSatisfy` any (BS.isInfixOf "before_created_at=2026-01-01T00%3A00%3A00Z")

    it "requires identity, accepts explicit workspace overrides and preserves a continuation across context changes" $ do
      requests <- newTVarIO []
      withSessionMock requests $ \manager base -> do
        initialized <- newTVarIO True
        context <- newTVarIO Nothing
        absent <- jsonRpcToolCall manager base initialized context "session_list" (object ["workspace_id" .= workspaceId])
        (absent >>= jsonPath ["result", "structuredContent", "error", "code"]) `shouldBe` Just (String "context_required")
        atomically $ writeTVar context (Just (MCPContext (Just (read (T.unpack observationId))) testIdentity))
        response <- jsonRpcToolCall manager base initialized context "session_list" (object ["workspace_id" .= workspaceId, "limit" .= (1 :: Int)])
        let Just next = response >>= jsonPath ["result", "structuredContent", "next_request", "arguments"]
        atomically $ writeTVar context (Just (MCPContext Nothing testIdentity))
        continued <- jsonRpcToolCall manager base initialized context "session_list" next
        (continued >>= jsonPath ["result", "structuredContent", "workspace_id"]) `shouldBe` Just (String workspaceId)
        missing <- jsonRpcToolCall manager base initialized context "session_detail" (object ["session_id" .= observationId])
        (missing >>= jsonPath ["result", "structuredContent", "error", "code"]) `shouldBe` Just (String "invalid_arguments")
      observed <- readTVarIO requests
      length observed `shouldBe` 2
      map (.requestPath) observed `shouldBe` replicate 2 ("/api/v1/workspaces/" <> T.unpack workspaceId <> "/sessions")

    it "reconstructs exact UTF-8 CRLF evidence through actual HTTP, retries an explicit seal, rejects later ingestion and allows later task links" $
      withSessionProxyApp $ \workspace manager base -> do
        let sid = read (T.unpack observationId) :: UUID
            arguments = object ["workspace_id" .= workspace, "session_id" .= sid]
            invoke name args = call manager base name args
            bytes = TE.encodeUtf8 "{\"provider\":\"雪\",\"event\":{\"x\":[1,true,null]}}\r\n{\"raw\":2}"
        _ <- invoke "session_create" (object ["workspace_id" .= workspace, "session_id" .= sid, "title" .= ("Test session work" :: Text), "format_label" .= ("unknown-provider" :: Text)])
        appended <- sessionHTTP manager base workspace sid "/append" (toJSON (SessionWire.AppendSessionRequest 0 0 (SessionWire.encodeSessionBytes bytes) True))
        statusCode (Client.responseStatus appended) `shouldBe` 200
        let Just receipt = decode (Client.responseBody appended) :: Maybe SessionWire.AppendSessionResponse
            terminateArgs = object ["workspace_id" .= workspace, "session_id" .= sid, "expected_offset" .= receipt.session.committedBytes, "expected_revision" .= receipt.session.revision
              , "final_bytes" .= BS.length bytes, "final_sha256" .= AgentSession.sessionSha256 bytes]
        active <- downloadSession invoke "session_read" (setField "limit" (Number 3) arguments)
        active `shouldBe` bytes
        sealed <- invoke "session_terminate" terminateArgs
        jsonPath ["summary", "final_sha256"] sealed `shouldBe` Just (String (AgentSession.sessionSha256 bytes))
        retried <- invoke "session_terminate" terminateArgs
        retried `shouldBe` sealed
        evidence <- downloadSession invoke "session_evidence" (setField "limit" (Number 3) arguments)
        evidence `shouldBe` bytes
        AgentSession.sessionSha256 evidence `shouldBe` AgentSession.sessionSha256 bytes
        wrongState <- handleToolCall manager base Nothing testIdentity (object ["name" .= ("session_read" :: Text), "arguments" .= arguments])
        jsonPath ["structuredContent", "error", "http_status"] wrongState `shouldBe` Just (Number 409)
        stale <- handleToolCall manager base Nothing testIdentity (object ["name" .= ("session_terminate" :: Text), "arguments" .= setField "expected_revision" (Number 99) terminateArgs])
        jsonPath ["structuredContent", "error", "http_status"] stale `shouldBe` Just (Number 409)
        later <- sessionHTTP manager base workspace sid "/append" (toJSON (SessionWire.AppendSessionRequest (fromIntegral (BS.length bytes)) (receipt.session.revision+1) (SessionWire.encodeSessionBytes "\n{}\n") False))
        statusCode (Client.responseStatus later) `shouldBe` 409
        task <- invoke "task_create" (object ["workspace_id" .= workspace, "title" .= ("Later task" :: Text)])
        let Just tid = jsonPath ["summary", "id"] task
        refused <- handleToolCall manager base Nothing testIdentity (object ["name" .= ("session_associate" :: Text), "arguments" .= setField "task_id" tid arguments])
        jsonPath ["structuredContent","error","code"] refused `shouldBe` Just (String "session_task_not_in_progress")
        jsonPath ["structuredContent","error","current_status"] refused `shouldBe` Just (String "todo")
        _ <- invoke "task_start" (object ["workspace_id" .= workspace,"task_id" .= tid])
        linked <- invoke "session_associate" (object ["workspace_id" .= workspace, "session_id" .= sid, "task_id" .= tid])
        jsonField "action" linked `shouldBe` Just (String "associated")
        _ <- invoke "session_associate" (object ["workspace_id" .= workspace, "session_id" .= sid, "task_id" .= tid])
        tasks <- invoke "session_tasks" arguments
        jsonField "items" tasks `shouldSatisfy` maybe False (contains "Later task")
        byTask <- invoke "session_list" (object ["workspace_id" .= workspace, "task_id" .= tid])
        jsonField "items" byTask `shouldSatisfy` maybe False (contains observationId)
        detail <- invoke "session_detail" arguments
        jsonField "creator_bot_identity" detail `shouldBe` Just (toJSON testIdentity)
        let emptySid = read (T.unpack workspaceId) :: UUID
            emptyArgs = object ["workspace_id" .= workspace, "session_id" .= emptySid]
            emptyTerminate = object ["workspace_id" .= workspace, "session_id" .= emptySid, "expected_offset" .= (0 :: Int), "expected_revision" .= (0 :: Int), "final_bytes" .= (0 :: Int), "final_sha256" .= AgentSession.sessionSha256 ""]
        _ <- invoke "session_create" (setField "title" (String "Empty evidence capture") (setField "format_label" (String "empty-session") emptyArgs))
        _ <- invoke "session_associate" (setField "task_id" tid emptyArgs)
        _ <- invoke "task_start" (object ["task_id" .= tid])
        _ <- invoke "task_finish" (object ["task_id" .= tid, "status" .= ("done" :: Text)])
        stillActive <- invoke "session_detail" emptyArgs
        jsonField "state" stillActive `shouldBe` Just (String "active")
        _ <- invoke "session_terminate" emptyTerminate
        emptyEvidence <- invoke "session_evidence" emptyArgs
        jsonField "data_base64" emptyEvidence `shouldBe` Just (String "")
        jsonField "next_request" emptyEvidence `shouldBe` Nothing
        foreignWorkspace <- invoke "workspace_register" (object ["name" .= ("Foreign session scope" :: Text)])
        let Just foreignId = jsonPath ["summary", "id"] foreignWorkspace
        foreignResult <- handleToolCall manager base Nothing testIdentity (object ["name" .= ("session_detail" :: Text), "arguments" .= setField "workspace_id" foreignId arguments])
        jsonPath ["structuredContent", "error", "http_status"] foreignResult `shouldBe` Just (Number 404)
        foreignTask <- invoke "task_create" (object ["workspace_id" .= foreignId, "title" .= ("Foreign task" :: Text)])
        let Just foreignTaskId = jsonPath ["summary", "id"] foreignTask
        mismatch <- handleToolCall manager base Nothing testIdentity (object ["name" .= ("session_associate" :: Text), "arguments" .= setField "task_id" foreignTaskId arguments])
        jsonPath ["structuredContent", "error", "http_status"] mismatch `shouldBe` Just (Number 404)

    it "retains actual deployed workspace permission refusals through the session adapter" $
      withRenameProxyApp renameUnprivileged $ \workspace manager base -> do
        forM_ ["session_list", "session_create"] $ \name -> do
          let args = if name == "session_create" then object ["workspace_id" .= workspace, "session_id" .= observationId, "title" .= ("Test session work" :: Text), "format_label" .= ("jsonl" :: Text)] else object ["workspace_id" .= workspace]
          result <- handleToolCall manager base Nothing testIdentity (object ["name" .= (name :: Text), "arguments" .= args])
          jsonPath ["structuredContent", "error", "http_status"] result `shouldBe` Just (Number 403)

  describe "Typed unified search contract" $ do
    it "advertises scoped overrides, nonempty selection and callable language and priority filters" $ do
      all (`elem` schemaProperties "search") ["workspace_id", "search_language", "task_priority"] `shouldBe` True
      (schemaProperty "search" "entity_types" >>= jsonField "minItems") `shouldBe` Just (Number 1)
      [name | Object tool <- toolDefinitions, Just (String name) <- [KM.lookup "name" tool], KM.member "outputSchema" tool] `shouldBe` ["search", "observation_discovery"]

    it "returns schema-valid all/default/subset results with complete legacy JSON text" $ do
      requests <- newTVarIO []
      withMock requests $ \manager base ->
        forM_ [[], ["entity_types" .= (["observation"] :: [Text])], ["entity_types" .= (["project", "task"] :: [Text])]] $ \selection -> do
          response <- handleToolCall manager base Nothing testIdentity (object ["name" .= ("search" :: Text), "arguments" .= object (["workspace_id" .= workspaceId] <> selection)])
          expectSearchStructured response
          jsonField "structuredContent" response `shouldBe` decodeMcpJSON response
          jsonPath ["structuredContent", "workspace_id"] response `shouldBe` Just (String workspaceId)

    it "reports only filters unused by the selection, preserving compatible mixed filters" $ do
      requests <- newTVarIO []
      withMock requests $ \manager base -> do
        let filters = ["workspace_id" .= workspaceId, "task_status" .= ("done" :: Text), "project_id" .= workspaceId, "subject" .= ("src/HMem/Types.hs" :: Text), "search_language" .= ("simple" :: Text), "task_priority" .= (7 :: Int)]
        projects <- call manager base "search" (object (filters <> ["entity_types" .= (["project"] :: [Text])]))
        jsonField "ignored_filters" projects `shouldBe` Just (toJSON (["project_id", "subject", "task_priority", "task_status"] :: [Text]))
        mixed <- call manager base "search" (object (filters <> ["entity_types" .= (["project", "task"] :: [Text])]))
        jsonField "ignored_filters" mixed `shouldBe` Just (toJSON (["subject"] :: [Text]))
        allTypes <- call manager base "search" (object filters)
        jsonField "ignored_filters" allTypes `shouldBe` Just (toJSON ([] :: [Text]))

    it "preserves full current provenance, exact selector evidence and full identifiers" $ do
      let provenance = object ["sequence" .= (3 :: Int), "event_kind" .= ("update" :: Text), "reviewed_git_sha" .= gitSha
            , "content_version" .= observationId, "content_digest" .= T.replicate 64 "a", "recorded_at" .= ("2026-10-21T00:00:00Z" :: Text)
            , "actor_type" .= ("bot" :: Text), "actor_id" .= workspaceId, "actor_label" .= Null, "bot_identity" .= testIdentity]
          evidence = object ["current_git_sha" .= gitSha, "history_git_sha" .= gitSha]
          observation = object ["id" .= observationId, "subjects" .= observationSubjects, "git_sha" .= gitSha, "content_version" .= observationId
            , "latest_sequence" .= (3 :: Int), "content_preview" .= ("bounded insight" :: Text), "current_provenance" .= provenance, "provenance_match" .= evidence]
          payload = object ["observations" .= [observation], "projects" .= ([] :: [Value]), "tasks" .= ([] :: [Value])
            , "has_more" .= object ["observations" .= False], "next_offset" .= object []]
      withSearchResponse status200 (encode payload) $ \manager base -> do
        response <- rawSearch manager base
        expectSearchStructured response
        jsonPath ["structuredContent", "observations"] response `shouldBe` Just (toJSON [observation])
        jsonField "structuredContent" response `shouldBe` decodeMcpJSON response

    it "executes narrowed continuations after a workspace switch without carrying irrelevant filters" $ do
      requests <- newTVarIO []
      withMock requests $ \manager base -> do
        initialized <- newTVarIO True
        context <- newTVarIO (Just (MCPContext (Just (read (T.unpack workspaceId))) testIdentity))
        response <- jsonRpcToolCall manager base initialized context "search" (object
          ["query" .= ("needle" :: Text), "subject" .= ("src/HMem/Types.hs" :: Text), "git_sha" .= gitSha
          , "project_status" .= ("archived" :: Text), "task_status" .= ("done" :: Text), "project_id" .= observationId
          , "search_language" .= ("simple" :: Text), "task_priority" .= (7 :: Int), "limit" .= (2 :: Int)])
        let Just result = response >>= jsonField "result"
            Just observationNext = jsonPath ["structuredContent", "next_requests", "observations"] result
            Just projectNext = jsonPath ["structuredContent", "next_requests", "projects"] result
        expectSearchStructured result
        jsonPath ["structuredContent", "next_requests", "tasks"] result `shouldBe` Nothing
        atomically $ modifyTVar' context (const (Just (MCPContext (Just (read (T.unpack observationId))) testIdentity)))
        forM_ [observationNext, projectNext] $ \next -> do
          let Just (String name) = jsonField "name" next
              Just arguments = jsonField "arguments" next
          (parseToolCall name arguments >>= validateToolCall) `shouldSatisfy` isRight
          continued <- jsonRpcToolCall manager base initialized context name arguments
          let Just continuedResult = continued >>= jsonField "result"
          expectSearchStructured continuedResult
          jsonPath ["structuredContent", "workspace_id"] continuedResult `shouldBe` Just (String workspaceId)
        observed <- readTVarIO requests
        let payloads = [value | request <- observed, Just value <- [decode request.requestBody]]
        map (jsonField "workspace_id") payloads `shouldBe` replicate 3 (Just (String workspaceId))
        let [_, observationPage, projectPage] = payloads
        jsonField "subject" observationPage `shouldBe` Just (String "src/HMem/Types.hs")
        jsonField "project_status" observationPage `shouldBe` Nothing
        jsonField "search_language" observationPage `shouldBe` Nothing
        jsonField "project_id" projectPage `shouldBe` Nothing
        jsonField "task_priority" projectPage `shouldBe` Nothing
        jsonField "search_language" projectPage `shouldBe` Just (String "simple")
        jsonField "query" projectPage `shouldBe` Just (String "needle")
        jsonField "limit" projectPage `shouldBe` Just (Number 2)

    it "preserves task language and priority in executable task continuations and emits no final requests" $ do
      let response = object ["observations" .= ([] :: [Value]), "projects" .= ([] :: [Value]), "tasks" .= ([] :: [Value]), "has_more" .= object ["tasks" .= True], "next_offset" .= object ["tasks" .= (4 :: Int)]]
      withSearchResponse status200 (encode response) $ \manager base -> do
        result <- handleToolCall manager base Nothing testIdentity (object ["name" .= ("search" :: Text), "arguments" .= object
          ["workspace_id" .= workspaceId, "entity_types" .= (["task"] :: [Text]), "task_priority" .= (7 :: Int), "search_language" .= ("simple" :: Text), "project_status" .= ("active" :: Text)]])
        expectSearchStructured result
        let Just next = jsonPath ["structuredContent", "next_requests", "tasks", "arguments"] result
        jsonField "task_priority" next `shouldBe` Just (Number 7)
        jsonField "search_language" next `shouldBe` Just (String "simple")
        jsonField "project_status" next `shouldBe` Nothing
        (parseToolCall "search" next >>= validateToolCall) `shouldSatisfy` isRight
        repeated <- handleToolCall manager base Nothing testIdentity (object ["name" .= ("search" :: Text), "arguments" .= next])
        expectSearchStructured repeated
      let final = object ["observations" .= ([] :: [Value]), "projects" .= ([] :: [Value]), "tasks" .= ([] :: [Value]), "has_more" .= object ["tasks" .= False], "next_offset" .= object []]
      withSearchResponse status200 (encode final) $ \manager base -> do
        result <- call manager base "search" (object ["workspace_id" .= workspaceId, "entity_types" .= (["task"] :: [Text])])
        jsonField "next_requests" result `shouldBe` Just (object [])

    it "returns schema-valid parser, validation and predeclaration errors before HTTP" $ do
      requests <- newTVarIO []
      withMock requests $ \manager base -> do
        forM_ [(object ["limit" .= ("bad" :: Text)], "invalid_arguments", "limit"), (object ["task_priority" .= ("bad" :: Text)], "invalid_arguments", "task_priority"), (object ["entity_types" .= ([] :: [Text])], "validation_error", "entity_types"), (object ["query" .= (" " :: Text)], "validation_error", "query")] $ \(arguments, code, errorField) -> do
          let Object fields = arguments
          response <- handleToolCall manager base Nothing testIdentity (object ["name" .= ("search" :: Text), "arguments" .= Object (KM.insert "workspace_id" (String workspaceId) fields)])
          expectSearchStructured response
          response `shouldSatisfy` isMcpError
          jsonPath ["structuredContent", "error", "code"] response `shouldBe` Just (String code)
          jsonPath ["structuredContent", "error", "field"] response `shouldBe` Just (String errorField)
        initialized <- newTVarIO True
        context <- newTVarIO Nothing
        response <- jsonRpcToolCall manager base initialized context "search" (object ["workspace_id" .= workspaceId])
        let Just result = response >>= jsonField "result"
        expectSearchStructured result
        jsonPath ["structuredContent", "error", "code"] result `shouldBe` Just (String "context_required")
      (length <$> readTVarIO requests) `shouldReturn` 0

    it "preserves bounded HTTP details and classifies invalid JSON and transport errors" $ do
      let details = object ["error" .= ("continuation_limit" :: Text), "message" .= ("Cannot continue" :: Text), "field" .= ("offset" :: Text), "detail" .= object ["max_offset" .= (2147483647 :: Int)]]
      withSearchResponse status400 (encode details) $ \manager base -> do
        response <- rawSearch manager base
        expectSearchStructured response
        jsonPath ["structuredContent", "error", "code"] response `shouldBe` Just (String "continuation_limit")
        jsonPath ["structuredContent", "error", "http_status"] response `shouldBe` Just (Number 400)
        jsonPath ["structuredContent", "error", "server_details"] response `shouldBe` Just details
      withSearchResponse status500 (encode (object ["detail" .= T.replicate 20000 "x"])) $ \manager base -> do
        response <- rawSearch manager base
        expectSearchStructured response
        jsonPath ["structuredContent", "error", "details_truncated"] response `shouldBe` Just (Bool True)
        BL.length (encode response) `shouldSatisfy` (< 16000)
      forM_ [(status401, "not json", "HTTP_401"), (status200, "not json", "invalid_response")] $ \(status, body, code) ->
        withSearchResponse status body $ \manager base -> do
          response <- rawSearch manager base
          expectSearchStructured response
          jsonPath ["structuredContent", "error", "code"] response `shouldBe` Just (String code)
      bracket (newManager defaultManagerSettings) closeManager $ \manager -> do
        response <- rawSearch manager "invalid URI"
        expectSearchStructured response
        jsonPath ["structuredContent", "error", "code"] response `shouldBe` Just (String "transport_error")

    it "attributes invalid SHA values to their actual selector and leaves ambiguous failures unassigned" $ do
      requests <- newTVarIO []
      withMock requests $ \manager base -> do
        forM_ [("git_sha", "current_git_sha"), ("current_git_sha", "history_git_sha"), ("history_git_sha", "git_sha")] $ \(invalidSelector, validSelector) -> do
          response <- handleToolCall manager base Nothing testIdentity (object ["name" .= ("search" :: Text), "arguments" .= object
            ["workspace_id" .= workspaceId, Key.fromText invalidSelector .= ("BAD" :: Text), Key.fromText validSelector .= gitSha]])
          expectSearchStructured response
          jsonPath ["structuredContent", "error", "code"] response `shouldBe` Just (String "validation_error")
          jsonPath ["structuredContent", "error", "field"] response `shouldBe` Just (String invalidSelector)
        ambiguous <- handleToolCall manager base Nothing testIdentity (object ["name" .= ("search" :: Text), "arguments" .= object
          ["workspace_id" .= workspaceId, "current_git_sha" .= ("BAD" :: Text), "history_git_sha" .= ("BAD" :: Text)]])
        expectSearchStructured ambiguous
        jsonPath ["structuredContent", "error", "field"] ambiguous `shouldBe` Just Null
      (length <$> readTVarIO requests) `shouldReturn` 0

  describe "Raw-text Observation discovery adapter" $ do
    it "advertises raw filters, context/session lifecycle and presentation without provider inputs" $ do
      all (`elem` schemaProperties "observation_discovery") ["workspace_id", "filters", "context_token", "replace_context_token", "search_session", "generation", "view", "prefix", "terminal", "limit", "offset"] `shouldBe` True
      schemaProperties "observation_discovery" `shouldNotContain` ["embedding", "provider", "endpoint"]
      forM_ ["server-owned", "five minutes", "discovery_restart_required", "discovery_capacity"] $ \phrase ->
        toolDescription "observation_discovery" `shouldSatisfy` maybe False (T.isInfixOf phrase)

    it "forwards one authorized creation and executes a workspace-pinned continuation without creation fields" $ do
      requests <- newTVarIO []
      withEnvironment "HMEM_MCP_PROVENANCE_TOKEN" "discovery-test-provenance" $
        withMock requests $ \manager base -> do
          initialized <- newTVarIO True
          context <- newTVarIO (Just (MCPContext (Just (read (T.unpack workspaceId))) testIdentity))
          response <- jsonRpcToolCall manager base initialized context "observation_discovery" (object
            ["filters" .= object ["subject_query" .= ("src/" :: Text), "content_query" .= ("authorization" :: Text)]
            , "replace_context_token" .= workspaceId, "search_session" .= observationId, "generation" .= (2 :: Int), "limit" .= (1 :: Int)])
          let Just result = response >>= jsonField "result"
              Just next = jsonPath ["structuredContent", "next_request"] result
              Just arguments = jsonField "arguments" next
          expectToolStructured "observation_discovery" result
          jsonField "structuredContent" result `shouldBe` decodeMcpJSON result
          jsonField "offset" arguments `shouldBe` Just (Number 1)
          forM_ ["filters", "replace_context_token", "search_session", "generation"] $ \key -> jsonField key arguments `shouldBe` Nothing
          atomically $ modifyTVar' context (const (Just (MCPContext (Just (read (T.unpack observationId))) testIdentity)))
          continued <- jsonRpcToolCall manager base initialized context "observation_discovery" arguments
          let Just continuedResult = continued >>= jsonField "result"
          expectToolStructured "observation_discovery" continuedResult
          jsonPath ["structuredContent", "workspace_id"] continuedResult `shouldBe` Just (String workspaceId)
      observed <- readTVarIO requests
      length observed `shouldBe` 2
      forM_ observed $ \request -> do
        request.requestMethod `shouldBe` methodPost
        request.requestPath `shouldBe` "/api/v1/observations/discovery"
        request.requestIdentityHeaders `shouldBe` [BL.toStrict (encode testIdentity)]
        request.requestChangeCause `shouldBe` Just "mcp"
        request.requestMcpProvenance `shouldBe` Just "discovery-test-provenance"
      let [Just created, Just continued] = map (decode . (.requestBody)) observed :: [Maybe Value]
      jsonPath ["filters", "content_query"] created `shouldBe` Just (String "authorization")
      jsonField "replace_context_token" created `shouldBe` Just (String workspaceId)
      jsonField "search_session" created `shouldBe` Just (String observationId)
      jsonField "generation" created `shouldBe` Just (Number 2)
      continued `shouldBe` object ["workspace_id" .= workspaceId, "context_token" .= observationId, "view" .= ("list" :: Text), "prefix" .= ("" :: Text), "limit" .= (1 :: Int), "offset" .= (1 :: Int)]

    it "preserves FTS and hybrid metadata and compacts content without exposing private preparation" $ do
      forM_ ["fts_only", "hybrid"] $ \mode -> do
        let Object fields = discoveryFixture mode "list" False
            payload = Object (KM.insert "embedding" (toJSON ([0.1, 0.2] :: [Double]))
              (KM.insert "retrieval" (object ["mode" .= mode, "reason" .= (if mode == "hybrid" then Null else String "provider_unavailable"), "endpoint" .= ("private-endpoint" :: Text)]) fields))
        withSearchResponse status200 (encode payload) $ \manager base -> do
          response <- handleToolCall manager base (Just "discovery-credential") testIdentity (object ["name" .= ("observation_discovery" :: Text), "arguments" .= discoveryArguments])
          expectToolStructured "observation_discovery" response
          jsonField "structuredContent" response `shouldBe` decodeMcpJSON response
          jsonPath ["structuredContent", "retrieval", "mode"] response `shouldBe` Just (String mode)
          jsonPath ["structuredContent", "retrieval", "reason"] response `shouldBe` Just (if mode == "hybrid" then Null else String "provider_unavailable")
          response `shouldNotSatisfy` contains "private-endpoint"
          jsonPath ["structuredContent", "embedding"] response `shouldBe` Nothing
          jsonPath ["structuredContent", "page", "items"] response `shouldSatisfy` maybe False (arrayFirst (\row -> jsonField "content" row == Nothing && jsonField "id" row == Just (String observationId) && jsonField "subjects" row == Just (toJSON observationSubjects)))
          jsonPath ["structuredContent", "next_request"] response `shouldBe` Nothing

    it "preserves tree/count metadata and forwards branch and typed leaf selectors" $ do
      golden <- compactFixtures
      requests <- newTVarIO []
      withMock requests $ \manager base -> do
        tree <- handleToolCall manager base (Just "discovery-credential") testIdentity (object ["name" .= ("observation_discovery" :: Text), "arguments" .= object
          ["workspace_id" .= workspaceId, "context_token" .= observationId, "view" .= ("tree" :: Text), "prefix" .= ("src" :: Text), "offset" .= (5 :: Int), "limit" .= (2 :: Int)]])
        expectToolStructured "observation_discovery" tree
        jsonPath ["structuredContent", "page", "tree"] tree `shouldBe` jsonPath ["page", "tree"] (discoveryFixture "fts_only" "tree" True)
        jsonPath ["structuredContent", "next_request", "arguments", "offset"] tree `shouldBe` Just (Number 7)
        jsonPath ["structuredContent", "next_request", "arguments", "prefix"] tree `shouldBe` Just (String "src")
        leaf <- call manager base "observation_discovery" (object ["workspace_id" .= workspaceId, "context_token" .= observationId, "terminal" .= ObservationSubject SubjectGlob "src/*.hs"])
        jsonPath ["next_request", "arguments", "terminal"] leaf `shouldBe` Just (toJSON (ObservationSubject SubjectGlob "src/*.hs"))
        count <- handleToolCall manager base Nothing testIdentity (object ["name" .= ("observation_discovery" :: Text), "arguments" .= object
          ["workspace_id" .= workspaceId, "context_token" .= observationId, "view" .= ("count" :: Text)]])
        expectToolStructured "observation_discovery" count
        jsonPath ["structuredContent", "page", "counts"] count `shouldBe` Just discoveryCounts
        jsonPath ["structuredContent", "next_request"] count `shouldBe` Nothing
        decodeMcpJSON count `shouldBe` Just (fixturePayload golden "observation_discovery_count")
      [treeRequest, leafRequest, countRequest] <- readTVarIO requests
      treeRequest.authorization `shouldBe` Just "Bearer discovery-credential"
      let Just treeBody = decode treeRequest.requestBody :: Maybe Value
          Just leafBody = decode leafRequest.requestBody :: Maybe Value
          Just countBody = decode countRequest.requestBody :: Maybe Value
      jsonField "prefix" treeBody `shouldBe` Just (String "src")
      jsonField "terminal" leafBody `shouldBe` Just (toJSON (ObservationSubject SubjectGlob "src/*.hs"))
      jsonField "view" countBody `shouldBe` Just (String "count")

    it "never invents count/final/empty continuation and reports list/tree offset exhaustion" $ do
      forM_ ["list", "tree"] $ \view -> do
        withSearchResponse status200 (encode (discoveryFixture "fts_only" view True)) $ \manager base -> do
          response <- handleToolCall manager base Nothing testIdentity (object ["name" .= ("observation_discovery" :: Text), "arguments" .= object
            ["workspace_id" .= workspaceId, "context_token" .= observationId, "view" .= view, "offset" .= (2147483647 :: Int)]])
          expectToolStructured "observation_discovery" response
          jsonPath ["structuredContent", "continuation_error", "code"] response `shouldBe` Just (String "continuation_limit")
          jsonPath ["structuredContent", "next_request"] response `shouldBe` Nothing
          jsonPath ["structuredContent", "page", "counts"] response `shouldBe` Just discoveryCounts
        withSearchResponse status200 (encode (discoveryFixture "fts_only" view False)) $ \manager base -> do
          result <- call manager base "observation_discovery" (object ["workspace_id" .= workspaceId, "context_token" .= observationId, "view" .= view])
          jsonField "next_request" result `shouldBe` Nothing
      let empty = object ["context_token" .= observationId, "expires_at" .= ("2026-10-21T10:05:00Z" :: Text), "retrieval" .= object ["mode" .= ("fts_only" :: Text), "reason" .= ("disabled" :: Text)], "complete" .= True
            , "page" .= object ["items" .= ([] :: [Value]), "has_more" .= False, "counts" .= discoveryCounts]]
      withSearchResponse status200 (encode empty) $ \manager base -> do
        result <- call manager base "observation_discovery" discoveryArguments
        jsonField "next_request" result `shouldBe` Nothing
      let brokenTree = object ["context_token" .= observationId, "expires_at" .= ("2026-10-21T10:05:00Z" :: Text), "retrieval" .= object ["mode" .= ("fts_only" :: Text), "reason" .= ("disabled" :: Text)], "complete" .= True
            , "page" .= object ["items" .= ([] :: [Value]), "has_more" .= False, "counts" .= discoveryCounts
                , "tree" .= object ["items" .= ([] :: [Value]), "has_more" .= True, "total_count" .= (100 :: Int), "match_count" .= (7 :: Int)]]]
      withSearchResponse status200 (encode brokenTree) $ \manager base -> do
        response <- handleToolCall manager base Nothing testIdentity (object ["name" .= ("observation_discovery" :: Text), "arguments" .= object ["workspace_id" .= workspaceId, "context_token" .= observationId, "view" .= ("tree" :: Text)]])
        expectToolStructured "observation_discovery" response
        jsonPath ["structuredContent", "continuation_error", "field"] response `shouldBe` Just (String "page.tree.items")
        jsonPath ["structuredContent", "next_request"] response `shouldBe` Nothing
        jsonPath ["structuredContent", "page", "tree"] response `shouldBe` jsonPath ["page", "tree"] brokenTree
      forM_ [("list", 2147483646), ("tree", 2147483645)] $ \(view, offset) ->
        withSearchResponse status200 (encode (discoveryFixture "fts_only" view True)) $ \manager base -> do
          result <- call manager base "observation_discovery" (object ["workspace_id" .= workspaceId, "context_token" .= observationId, "view" .= view, "offset" .= (offset :: Int)])
          jsonPath ["next_request", "arguments", "offset"] result `shouldBe` Just (Number 2147483647)

    it "rejects malformed creation/continuation/session/presentation and accurately labels nested SHA selectors" $ do
      requests <- newTVarIO []
      withMock requests $ \manager base -> do
        let root extra = object (["workspace_id" .= workspaceId] <> extra)
        forM_ [ (root [], "filters")
              , (root ["filters" .= object [], "context_token" .= observationId], "context_token")
              , (root ["filters" .= object [], "search_session" .= observationId], "generation")
              , (root ["filters" .= object ["content_query" .= T.replicate 4097 "x"]], "filters.content_query")
              , (root ["context_token" .= ("BAD" :: Text)], "context_token")
              , (root ["context_token" .= (5 :: Int)], "context_token")
              , (root ["filters" .= object [], "replace_context_token" .= ("BAD" :: Text)], "replace_context_token")
              , (root ["filters" .= object ["subject_kind" .= ("BAD" :: Text)]], "filters.subject_kind")
              , (root ["context_token" .= observationId, "view" .= ("tree" :: Text), "terminal" .= ObservationSubject SubjectFile "src/A.hs"], "terminal")
              , (root ["context_token" .= observationId, "prefix" .= ("src" :: Text)], "prefix")
              , (root ["filters" .= object ["current_git_sha" .= (5 :: Int)]], "filters.current_git_sha")
              ] $ \(arguments, errorField) -> do
          response <- handleToolCall manager base Nothing testIdentity (object ["name" .= ("observation_discovery" :: Text), "arguments" .= arguments])
          expectToolStructured "observation_discovery" response
          response `shouldSatisfy` isMcpError
          jsonPath ["structuredContent", "error", "field"] response `shouldBe` Just (String errorField)
        forM_ [("git_sha", "current_git_sha"), ("current_git_sha", "history_git_sha"), ("history_git_sha", "git_sha")] $ \(bad, valid) -> do
          response <- handleToolCall manager base Nothing testIdentity (object ["name" .= ("observation_discovery" :: Text), "arguments" .= root
            ["filters" .= object [Key.fromText bad .= ("BAD" :: Text), Key.fromText valid .= gitSha]]])
          expectToolStructured "observation_discovery" response
          jsonPath ["structuredContent", "error", "field"] response `shouldBe` Just (String ("filters." <> bad))
        ambiguous <- handleToolCall manager base Nothing testIdentity (object ["name" .= ("observation_discovery" :: Text), "arguments" .= root
          ["filters" .= object ["current_git_sha" .= ("BAD" :: Text), "history_git_sha" .= ("BAD" :: Text)]]])
        jsonPath ["structuredContent", "error", "field"] ambiguous `shouldBe` Just Null
        initialized <- newTVarIO True
        context <- newTVarIO Nothing
        predeclared <- jsonRpcToolCall manager base initialized context "observation_discovery" discoveryArguments
        let Just result = predeclared >>= jsonField "result"
        expectToolStructured "observation_discovery" result
        jsonPath ["structuredContent", "error", "code"] result `shouldBe` Just (String "context_required")
      (length <$> readTVarIO requests) `shouldReturn` 0

    it "preserves restart/capacity/authorization failures without automatic replacement or retry" $ do
      forM_ [(status409, "discovery_restart_required"), (status429, "discovery_capacity"), (status403, "forbidden")] $ \(status, code) -> do
        requests <- newTVarIO []
        let details = object ["error" .= code, "message" .= ("Retry or restart explicitly." :: Text)]
        testWithApplication (pure (\request respond -> do
          _ <- Wai.strictRequestBody request
          atomically $ modifyTVar' requests (<> [request.rawPathInfo])
          respond (Wai.responseLBS status [("Content-Type", "application/json")] (encode details)))) $ \port ->
          bracket (newManager defaultManagerSettings { managerIdleConnectionCount = 0 }) closeManager $ \manager -> do
            response <- handleToolCall manager ("http://127.0.0.1:" <> show port) Nothing testIdentity (object ["name" .= ("observation_discovery" :: Text), "arguments" .= object ["workspace_id" .= workspaceId, "context_token" .= observationId]])
            expectToolStructured "observation_discovery" response
            jsonPath ["structuredContent", "error", "code"] response `shouldBe` Just (String code)
            jsonPath ["structuredContent", "error", "http_status"] response `shouldBe` Just (Number (fromIntegral (statusCode status)))
            jsonPath ["structuredContent", "error", "server_details"] response `shouldBe` Just details
        readTVarIO requests `shouldReturn` ["/api/v1/observations/discovery"]

  describe "Declared identity forwarding" $ do
    it "adds exactly one accepted identity on JSON, conditional and no-content helpers without changing credentials or provenance" $ do
      requests <- newTVarIO []
      withEnvironment "HMEM_MCP_PROVENANCE_TOKEN" "identity-test-provenance" $
        withMock requests $ \manager base -> do
          _ <- callWithKey manager base (Just "existing-credential") "observation_get" (object ["observation_id" .= observationId])
          _ <- callWithKey manager base (Just "existing-credential") "observation_update" (object
            ["observation_id" .= observationId, "content" .= ("updated" :: Text), "reviewed_git_sha" .= gitSha, "expected_content_version" .= workspaceId])
          _ <- callWithKey manager base (Just "existing-credential") "observation_delete" (object ["observation_id" .= observationId])
          _ <- callWithKey manager base Nothing "observation_set_embedding" (object ["observation_id" .= observationId, "embedding" .= embedding])
          pure ()
      observed <- readTVarIO requests
      map (.requestIdentityHeaders) observed `shouldBe` replicate 4 [BL.toStrict (encode testIdentity)]
      map (.authorization) observed `shouldBe` [Just "Bearer existing-credential", Just "Bearer existing-credential", Just "Bearer existing-credential", Nothing]
      map (.requestChangeCause) observed `shouldBe` replicate 4 (Just "mcp")
      map (.requestMcpProvenance) observed `shouldBe` replicate 4 (Just "identity-test-provenance")
      map (.requestIfMatch) observed `shouldBe` [Nothing, Just (TE.encodeUtf8 ("\"" <> workspaceId <> "\"")), Nothing, Nothing]

  describe "Workspace deletion boundary" $ do
    it "does not advertise or dispatch workspace_delete" $ do
      toolNames `shouldNotContain` ["workspace_delete"]
      requests <- newTVarIO []
      withMock requests $ \manager base -> do
        result <- handleToolCall manager base Nothing testIdentity (object ["name" .= ("workspace_delete" :: Text), "arguments" .= object ["workspace_id" .= workspaceId]])
        result `shouldSatisfy` isMcpError
      (length <$> readTVarIO requests) `shouldReturn` 0

  describe "Revision transport MCP" $ do
    it "rejects content-only, absent/null/malformed preconditions and reviewed SHAs before HTTP dispatch" $ do
      requests <- newTVarIO []
      withMock requests $ \manager base -> do
        let complete = object ["observation_id" .= observationId, "content" .= ("reviewed" :: Text), "reviewed_git_sha" .= gitSha, "expected_content_version" .= observationId]
            replace key value (Object fields) = Object (KM.insert key value fields)
            replace _ _ value = value
            omit key (Object fields) = Object (KM.delete key fields)
            omit _ value = value
        forM_ [omit "reviewed_git_sha" complete, omit "expected_content_version" complete, replace "expected_content_version" Null complete, replace "expected_content_version" (String "BAD") complete, replace "reviewed_git_sha" Null complete, replace "reviewed_git_sha" (String "BAD") complete] $ \args -> do
          result <- handleToolCall manager base Nothing testIdentity (object ["name" .= ("observation_update" :: Text), "arguments" .= args])
          result `shouldSatisfy` isMcpError
      (length <$> readTVarIO requests) `shouldReturn` 0

    it "forwards all distinct exact SHA selectors and bounded history paging" $ do
      requests <- newTVarIO []
      withMock requests $ \manager base -> do
        let filters = ["workspace_id" .= workspaceId, "git_sha" .= gitSha, "current_git_sha" .= gitSha, "history_git_sha" .= gitSha]
        _ <- call manager base "observation_list" (object filters)
        _ <- call manager base "search" (object filters)
        _ <- call manager base "observation_match" (object (filters <> ["paths" .= (["src/HMem/Types.hs"] :: [Text])]))
        result <- call manager base "observation_history" (object ["observation_id" .= observationId, "limit" .= (1 :: Int), "offset" .= (1 :: Int)])
        jsonField "has_more" result `shouldBe` Just (Bool False)
        jsonField "next_offset" result `shouldBe` Nothing
      [listed, searched, matched, history] <- readTVarIO requests
      listed.requestQuery `shouldSatisfy` BS.isInfixOf "current_git_sha="
      listed.requestQuery `shouldSatisfy` BS.isInfixOf "history_git_sha="
      forM_ [searched, matched] $ \request -> do
        let Just body = decode request.requestBody :: Maybe Value
        jsonField "git_sha" body `shouldBe` Just (String gitSha)
        jsonField "current_git_sha" body `shouldBe` Just (String gitSha)
        jsonField "history_git_sha" body `shouldBe` Just (String gitSha)
      history.requestPath `shouldBe` "/api/v1/observations/aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee/history"
      history.requestQuery `shouldBe` "?limit=1&offset=1"

  describe "Workspace rename MCP registry" $ do
    it "advertises a strict name-only workspace_update call" $ do
      toolNames `shouldContain` ["workspace_update"]
      schemaProperties "workspace_update" `shouldBe` ["name", "workspace_id"]
      schemaRequired "workspace_update" `shouldBe` ["workspace_id", "name"]
      schemaAdditionalProperties "workspace_update" `shouldBe` Just (Bool False)
      parseToolCall "workspace_update" (object ["workspace_id" .= workspaceId, "name" .= ("Renamed" :: Text)]) `shouldSatisfy` isRight
      parseToolCall "workspace_update" (object ["workspace_id" .= workspaceId, "name" .= ("Renamed" :: Text), "workspace_type" .= ("repository" :: Text)]) `shouldSatisfy` isLeft
    it "forwards a strict PUT body and correlated request id" $ do
      requests <- newTVarIO []
      withMock requests $ \manager base -> do
        result <- call manager base "workspace_update" (object ["workspace_id" .= workspaceId, "name" .= ("Renamed" :: Text)])
        jsonField "action" result `shouldBe` Just (String "updated")
        jsonField "summary" result `shouldSatisfy` maybe False (hasFields ["id", "name"])
      [request] <- readTVarIO requests
      request.requestMethod `shouldBe` methodPut
      request.requestPath `shouldBe` "/api/v1/workspaces/11111111-2222-3333-4444-555555555555"
      decode request.requestBody `shouldBe` Just (object ["name" .= ("Renamed" :: Text)])
      request.requestId `shouldSatisfy` maybe False (not . BS.null)
    it "defers workspace name validation to the authorized REST boundary" $ do
      requests <- newTVarIO []
      withMock requests $ \manager base ->
        forM_ ["   ", T.replicate 1025 "x"] $ \workspaceName -> do
          response <- handleToolCall manager base Nothing testIdentity (object ["name" .= ("workspace_update" :: Text), "arguments" .= object ["workspace_id" .= workspaceId, "name" .= workspaceName]])
          response `shouldNotSatisfy` isMcpError
      readTVarIO requests >>= \observed -> length observed `shouldBe` 2
    it "surfaces structured workspace REST 400, 403, and 404 messages as MCP errors" $
      forM_ [(status400, "Workspace name cannot be blank."), (status403, "Workspace edit permission is required."), (status404, "Workspace not found.")] $ \(status, message) ->
        withWorkspaceStructuredStatusMock status message $ \manager base -> do
          response <- handleToolCall manager base Nothing testIdentity (object ["name" .= ("workspace_update" :: Text), "arguments" .= object ["workspace_id" .= workspaceId, "name" .= ("Valid workspace name" :: Text)]])
          response `shouldSatisfy` isMcpError
          response `shouldSatisfy` contains ("[HTTP_" <> T.pack (show (statusCode status)) <> "] " <> message)
    it "returns stable errors through the real deployed-mode application proxy" $
      forM_ renameProxyErrorCases $ \(principal, useMissingWorkspace, workspaceName, expected) ->
        withRenameProxyApp principal $ \workspaceId' manager base -> do
          let targetWorkspaceId = if useMissingWorkspace then missingWorkspaceId else workspaceId'
          response <- handleToolCall manager base Nothing testIdentity (object
            [ "name" .= ("workspace_update" :: Text)
            , "arguments" .= object ["workspace_id" .= targetWorkspaceId, "name" .= workspaceName]
            ])
          response `shouldSatisfy` isMcpError
          response `shouldSatisfy` contains expected
  describe "project_spec MCP contract" $ do
    it "states when atomic rejection is certain and when callers must check before retrying" $ do
      toolDescription "project_spec" `shouldSatisfy` maybe False (\description ->
        all (`T.isInfixOf` description)
          [ "Validation, authorization, or database rejection creates nothing"
          , "the outcome is uncertain"
          , "check the workspace's projects before retrying" ])

    it "sends one POST and returns full project and ordered task IDs" $ do
      requests <- newTVarIO []
      let arguments = object ["workspace_id" .= workspaceId, "name" .= ("new project" :: Text)
            , "tasks" .= [object ["title" .= ("first" :: Text)], object ["title" .= ("second" :: Text)], object ["title" .= ("third" :: Text)]]]
          expectedIds = [observationId, workspaceId, "99999999-2222-3333-4444-555555555555" :: Text]
      withMock requests $ \manager base -> do
        result <- call manager base "project_spec" arguments
        result `shouldNotSatisfy` isMcpError
        jsonField "project_id" result `shouldBe` Just (toJSON observationId)
        jsonField "task_ids" result `shouldBe` Just (toJSON expectedIds)
        jsonField "tasks_failed" result `shouldBe` Nothing
      [sent] <- readTVarIO requests
      sent.requestMethod `shouldBe` methodPost
      sent.requestPath `shouldBe` "/api/v1/projects/spec"
      decode sent.requestBody `shouldSatisfy` maybe False (\body -> jsonField "workspace_id" body == Just (toJSON workspaceId) && jsonField "tasks" body == jsonField "tasks" arguments)

    it "rejects invalid specifications before HTTP and preserves server failures as errors" $ do
      requests <- newTVarIO []
      let invoke manager base arguments = handleToolCall manager base Nothing testIdentity
            (object ["name" .= ("project_spec" :: Text), "arguments" .= arguments])
      withMock requests $ \manager base -> do
        forM_ [[], [object ["title" .= (" " :: Text)]], [object ["title" .= ("task" :: Text), "priority" .= (11 :: Int)]], replicate 51 (object ["title" .= ("task" :: Text)])] $ \tasks ->
          invoke manager base (object ["workspace_id" .= workspaceId, "name" .= ("project" :: Text), "tasks" .= tasks]) >>= (`shouldSatisfy` isMcpError)
      readTVarIO requests >>= (\observed -> length observed `shouldBe` 0)
      withWorkspaceStructuredStatusMock status400 "Middle task rejected" $ \manager base -> do
        result <- invoke manager base (object ["workspace_id" .= workspaceId, "name" .= ("project" :: Text), "tasks" .= [object ["title" .= ("task" :: Text)] ]])
        result `shouldSatisfy` isMcpError
        result `shouldSatisfy` contains "[HTTP_400] Middle task rejected"
      withStatusMock requests $ \manager base ->
        invoke manager base (object ["workspace_id" .= workspaceId, "name" .= ("project" :: Text), "tasks" .= [object ["title" .= ("task" :: Text)] ]]) >>= (`shouldSatisfy` isMcpError)
      withMock requests $ \manager base -> do
        let fourTasks = replicate 4 (object ["title" .= ("task" :: Text)])
        malformed <- invoke manager base (object ["workspace_id" .= workspaceId, "name" .= ("project" :: Text), "tasks" .= fourTasks])
        malformed `shouldSatisfy` isMcpError
        malformed `shouldSatisfy` contains "outcome is uncertain"

  describe "Partial and nullable project and task updates" $ do
    it "rejects removed due keys before any planning HTTP dispatch, including nested project specs" $ do
      requests <- newTVarIO []
      let add key value (Object fields) = Object (KM.insert (Key.fromText key) value fields)
          add _ _ value = value
          planning =
            [ ("project_create", object ["workspace_id" .= workspaceId, "name" .= ("project" :: Text)])
            , ("project_update", object ["project_id" .= observationId])
            , ("project_spec", object ["workspace_id" .= workspaceId, "name" .= ("project" :: Text), "tasks" .= [object ["title" .= ("task" :: Text)]]])
            , ("project_archive", object ["project_id" .= observationId])
            , ("task_create", object ["workspace_id" .= workspaceId, "title" .= ("task" :: Text)])
            , ("task_update", object ["task_id" .= observationId])
            , ("task_move_batch", object ["task_ids" .= [observationId]])
            , ("task_dependency", object ["task_id" .= observationId, "depends_on_id" .= workspaceId, "action" .= ("add" :: Text)])
            , ("task_start", object ["task_id" .= observationId])
            , ("task_finish", object ["task_id" .= observationId, "status" .= ("done" :: Text)]) ]
      withMock requests $ \manager base ->
        forM_ ["due_at", "dueAt", "due", "due_date", "dueDate"] $ \key ->
          forM_ [Null, toJSON ("2030-01-01T00:00:00Z" :: Text)] $ \value -> do
            forM_ planning $ \(name, arguments) -> do
              parseToolCall name (add key value arguments) `shouldSatisfy` isLeft
              handleToolCall manager base Nothing testIdentity (object ["name" .= name, "arguments" .= add key value arguments]) >>= (`shouldSatisfy` isMcpError)
            let nested = object ["workspace_id" .= workspaceId, "name" .= ("project" :: Text), "tasks" .= [object ["title" .= ("task" :: Text), Key.fromText key .= value]]]
            handleToolCall manager base Nothing testIdentity (object ["name" .= ("project_spec" :: Text), "arguments" .= nested]) >>= (`shouldSatisfy` isMcpError)
      readTVarIO requests >>= (\observed -> length observed `shouldBe` 0)

    it "advertises exactly the clearable fields as string or null and requires only the entity ID" $ do
      sort (schemaProperties "project_update") `shouldBe` sort ["project_id", "name", "description", "parent_id", "status", "priority"]
      sort (schemaProperties "task_update") `shouldBe` sort ["task_id", "title", "description", "project_id", "parent_id", "status", "priority"]
      schemaRequired "project_update" `shouldBe` ["project_id"]
      schemaRequired "task_update" `shouldBe` ["task_id"]
      forM_ [("project_update", ["description", "parent_id"]), ("task_update", ["description", "project_id", "parent_id"])] $ \(name, fields) ->
        forM_ fields $ \field ->
          (schemaProperty name field >>= jsonField "anyOf") `shouldSatisfy` maybe False stringOrNullTypes
      forM_ [("project_update", ["project_id", "name", "status", "priority"]), ("task_update", ["task_id", "title", "status", "priority"])] $ \(name, fields) ->
        forM_ fields $ \field ->
          (schemaProperty name field >>= jsonField "anyOf") `shouldBe` Nothing
      forM_ [("project_update", "project_id"), ("task_update", "task_id")] $ \(name, field) ->
        (schemaProperty name field >>= jsonField "type") `shouldBe` Just (String "string")

    it "parses and forwards ID-only, one-field, null, and value updates without resending other fields" $ do
      let projectCases =
            [ (object ["project_id" .= observationId], object [])
            , (object ["project_id" .= observationId, "name" .= ("New name" :: Text)]
              , object ["name" .= ("New name" :: Text)])
            , (object ["project_id" .= observationId, "description" .= Null, "parent_id" .= Null]
              , object ["description" .= Null, "parent_id" .= Null])
            , (object ["project_id" .= observationId, "description" .= ("Project specification" :: Text), "parent_id" .= workspaceId]
              , object ["description" .= ("Project specification" :: Text), "parent_id" .= workspaceId])
            ]
          taskCases =
            [ (object ["task_id" .= observationId], object [])
            , (object ["task_id" .= observationId, "title" .= ("New title" :: Text)]
              , object ["title" .= ("New title" :: Text)])
            , (object ["task_id" .= observationId, "description" .= Null, "project_id" .= Null, "parent_id" .= Null]
              , object ["description" .= Null, "project_id" .= Null, "parent_id" .= Null])
            , (object ["task_id" .= observationId, "description" .= ("Task specification" :: Text), "project_id" .= workspaceId, "parent_id" .= workspaceId]
              , object ["description" .= ("Task specification" :: Text), "project_id" .= workspaceId, "parent_id" .= workspaceId])
            ]
          cases = [("project_update", "/api/v1/projects/aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee", arguments, body) | (arguments, body) <- projectCases]
               <> [("task_update", "/api/v1/tasks/aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee", arguments, body) | (arguments, body) <- taskCases]
      requests <- newTVarIO []
      withMock requests $ \manager base ->
        forM_ cases $ \(name, _, arguments, _) -> do
          parseToolCall name arguments `shouldSatisfy` isRight
          result <- call manager base name arguments
          result `shouldNotSatisfy` isMcpError
      observed <- readTVarIO requests
      length observed `shouldBe` length cases
      forM_ (zip cases observed) $ \((_, path, _, expectedBody), request) -> do
        request.requestMethod `shouldBe` methodPut
        request.requestPath `shouldBe` path
        decode request.requestBody `shouldBe` Just expectedBody

  describe "Observation MCP registry" $ do
    it "advertises and parses every Observation capability, with no removed memory, link, or context tools" $ do
      toolNames `shouldContain` ["observation_create", "observation_get", "observation_update", "observation_history", "observation_list", "observation_match", "observation_delete", "observation_set_embedding", "observation_similar"]
      mapM_ (\(name, arguments) -> parseToolCall name arguments `shouldSatisfy` isRight)
        [ ("observation_create", observationArguments)
        , ("observation_get", object ["observation_id" .= observationId])
        , ("observation_update", object ["observation_id" .= observationId, "content" .= ("replacement" :: Text), "reviewed_git_sha" .= gitSha, "expected_content_version" .= observationId])
        , ("observation_history", object ["observation_id" .= observationId, "limit" .= (1 :: Int), "offset" .= (0 :: Int)])
        , ("observation_list", observationListArguments)
        , ("observation_match", observationMatchArguments)
        , ("observation_delete", object ["observation_id" .= observationId])
        , ("observation_set_embedding", object ["observation_id" .= observationId, "embedding" .= embedding])
        , ("observation_similar", similarArguments)
        ]
      toolNames `shouldNotSatisfy` any (`elem` ["memory_create", "memory_get", "memory_update", "memory_link", "link_memory", "context_get"])
      mapM_ (\name -> parseToolCall name (object []) `shouldSatisfy` isUnknown name)
        ["memory_create", "memory_get", "memory_update", "memory_link", "link_memory", "context_get"]

    it "advertises only provenance fields on observation tools and search" $ do
      all (`elem` schemaProperties "observation_create") ["subjects", "git_sha", "content"] `shouldBe` True
      schemaProperties "observation_create" `shouldNotContain` ["subject_kind", "subject", "workspace_id"]
      schemaRequired "observation_create" `shouldBe` ["subjects", "git_sha", "content"]
      (schemaProperty "observation_create" "subjects" >>= jsonField "maxItems") `shouldBe` Just (Number (fromIntegral maxObservationSubjects))
      all (`elem` schemaProperties "observation_update") ["observation_id", "content"] `shouldBe` True
      length (schemaProperties "observation_update") `shouldBe` 4
      schemaRequired "observation_update" `shouldBe` ["observation_id", "content", "reviewed_git_sha", "expected_content_version"]
      all (`elem` schemaProperties "search") ["subject_kind", "subject", "git_sha"] `shouldBe` True
      schemaProperties "search" `shouldContain` ["offset"]
      all (`elem` schemaProperties "observation_list") ["subject_kind", "subject", "git_sha", "query", "limit", "offset"] `shouldBe` True
      schemaProperties "observation_list" `shouldNotContain` ["workspace_id"]
      all (`elem` schemaProperties "observation_similar") ["embedding", "min_similarity", "limit", "offset"] `shouldBe` True
      schemaProperties "observation_similar" `shouldNotContain` ["workspace_id"]
      all (`elem` schemaProperties "observation_match") ["paths", "subject_kind", "git_sha", "query", "limit", "offset"] `shouldBe` True
      schemaProperties "observation_match" `shouldNotContain` ["workspace_id", "subject"]
      schemaRequired "observation_match" `shouldBe` ["paths"]
      toolDescription "observation_match" `shouldSatisfy` maybe False (\description -> all (\needle -> needle `T.isInfixOf` description) ["concrete", "OR", "glob", "next_offset"])
      schemaProperties "search" `shouldNotContain` ["memory_type", "tags", "pinned_only", "min_importance"]
      (schemaProperty "set_workspace" "workspace_id" >>= jsonField "anyOf")
        `shouldSatisfy` maybe False stringOrNullTypes
      schemaRequired "set_workspace" `shouldBe` ["workspace_id", "identity"]
      (schemaProperty "set_workspace" "identity" >>= jsonField "required") `shouldBe` Just (toJSON (["role", "model-id"] :: [Text]))
      (schemaProperty "set_workspace" "identity" >>= jsonField "additionalProperties") `shouldBe` Just (Bool False)
      toolDescription "task_finish" `shouldSatisfy` maybe False ("does not create an observation" `T.isInfixOf`)
      toolDescription "project_archive" `shouldSatisfy` maybe False (not . ("summary" `T.isInfixOf`))
      sort (schemaProperties "task_dependency") `shouldBe` sort ["task_id", "depends_on_id", "action"]
      schemaRequired "task_dependency" `shouldBe` ["task_id", "depends_on_id", "action"]
      sort (schemaProperties "task_move_batch") `shouldBe` sort ["task_ids", "project_id"]
      schemaRequired "task_move_batch" `shouldBe` ["task_ids"]
      schemaAdditionalProperties "task_move_batch" `shouldBe` Just (Bool False)
      (schemaProperty "task_move_batch" "task_ids" >>= jsonField "minItems") `shouldBe` Just (Number 1)
      (schemaProperty "task_move_batch" "task_ids" >>= jsonField "maxItems") `shouldBe` Just (Number 100)

    it "forwards both trusted-MCP headers only when the bridge is configured" $ do
      mcpProvenanceHeadersFor (Just "test-private-provenance")
        `shouldBe` [("X-HMem-Change-Cause", "mcp"), ("X-HMem-MCP-Provenance", "test-private-provenance")]
      mcpProvenanceHeadersFor (Just "  trimmed-private-provenance  ")
        `shouldBe` [("X-HMem-Change-Cause", "mcp"), ("X-HMem-MCP-Provenance", "trimmed-private-provenance")]
      mcpProvenanceHeadersFor (Just "  provenance-\955  ")
        `shouldBe` [("X-HMem-Change-Cause", "mcp"), ("X-HMem-MCP-Provenance", TE.encodeUtf8 "provenance-\955")]
      mcpProvenanceHeadersFor Nothing
        `shouldBe` [("X-HMem-Change-Cause", "mcp")]
      mcpProvenanceHeadersFor (Just " \t ")
        `shouldBe` [("X-HMem-Change-Cause", "mcp")]

    it "describes archive and cancellation cascades without automatic descendant reopening" $ do
      toolDescription "project_archive" `shouldSatisfy` maybe False (\description -> all (`T.isInfixOf` description)
        ["all descendant projects, including completed projects", "cancel non-done tasks", "Done tasks and dependency links are preserved", "unrelated dependent tasks are not cancelled", "Reopening does not reopen descendants"])
      toolDescription "task_finish" `shouldSatisfy` maybe False (\description -> all (`T.isInfixOf` description)
        ["Done requires closing open descendants", "cancels non-done subtasks", "preserving done subtasks and dependency links", "Reopening does not reopen descendants"])
      toolDescription "task_update" `shouldSatisfy` maybe False (T.isInfixOf "Reopen a cancelled parent before moving, reparenting, or reopening unfinished subtasks beneath it")

    it "guides concise project goals, deliverable tasks, checklist subtasks, and separate execution state" $ do
      toolDescriptionIs "project_create" "Create a project grouping tasks and subprojects in a hierarchy for long-running or short-lived work. Its description is a concise durable specification of goals and domain, excluding information already encoded in hmem's dependency graph; status records execution state. Project descriptions are Markdown and should be well-formatted for readability."
      toolDescriptionIs "project_update" "Update a project's durable specification, hierarchy, or execution state. Supply project_id plus only fields to change; omitted fields stay unchanged, including description and parent_id, so do not copy them from a prior read. Explicit null clears only clearable fields. Descriptions are concise durable specifications of goals and domain, excluding information already encoded in hmem's dependency graph; not logs of progress or updates. Project descriptions are Markdown and should be well-formatted for readability. Pausing requires no running descendant tasks or subtasks and cascades inherited pause/blocking. Resuming returns eligible automatically blocked tasks to todo and retains independent or ancestor pauses; acknowledgements include bounded pause effects. Completing requires closed descendants. Archiving also archives all descendant projects, including completed projects, and cancels non-done tasks while preserving done tasks."
      toolDescriptionIs "project_spec" "Atomically create a project and 1 to 50 initial atomic tasks in one request. Validation, authorization, or database rejection creates nothing. If the connection or response fails, the outcome is uncertain; check the workspace's projects before retrying. Returns the project ID and task IDs in input order. Projects group tasks and subprojects hierarchically for long-running or short-lived work. Tasks generally deliver a complete unit of work on its own; complex work can use subtasks as a directed checklist. Descriptions are concise durable specifications: project goals and domain, task steps and completion criteria, excluding information already encoded in hmem's dependency graph. Project, task, and subtask descriptions are Markdown and should be well-formatted for readability. Status records execution state. Create later-discovered atomic work as subtasks."
      toolDescriptionIs "task_create" "Create a task, generally a complete unit of work deliverable on its own. Complex work can use subtasks as a directed checklist. Its description is a concise durable specification of steps and completion criteria, excluding information already encoded in hmem's dependency graph; status records execution state. Task and subtask descriptions are Markdown and should be well-formatted for readability. Create later-discovered atomic work as subtasks. Reopen a cancelled or done parent before adding unfinished subtasks."
      toolDescriptionIs "task_update" "Update a task's durable specification, hierarchy, or execution state. Supply task_id plus only fields to change; omitted fields stay unchanged, including description, parent_id, and project_id, so do not copy them from a prior read. Explicit null clears only clearable fields. Descriptions are concise durable specifications of steps and completion criteria, excluding information already encoded in hmem's dependency graph; not logs of progress or updates. Task and subtask descriptions are Markdown and should be well-formatted for readability. Use subtasks as a directed checklist for complex work; create later-discovered atomic work as subtasks. Cancelling also cancels non-done subtasks while preserving done subtasks. Reopen a cancelled parent before moving, reparenting, or reopening unfinished subtasks beneath it. Reopening does not reopen descendants."
      toolDescriptionIs "task_dependency" "Add or remove a prerequisite edge: task_id cannot proceed until depends_on_id is complete. Use dependencies for ordering, not logs of progress or updates."
      toolDescriptionIs "task_start" "Set a task's execution state to in_progress. Preserve its description as a durable specification; status records progress."
      toolDescriptionIs "task_finish" "Set a task's execution state to done, blocked, or cancelled. Status records progress; this does not create an observation. Done requires closing open descendants. Cancelled also cancels non-done subtasks while preserving done subtasks and dependency links; unrelated dependent tasks are not cancelled. Reopening does not reopen descendants."
      schemaPropertyDescriptionIs "project_create" "description" "Optional concise durable project specification in Markdown, well-formatted for readability: goals and domain; omit information already encoded in hmem's dependency graph; not a log of progress or updates"
      schemaPropertyDescriptionIs "project_update" "project_id" "Project UUID; the only required field"
      schemaPropertyDescriptionIs "project_update" "description" "Concise durable project specification in Markdown, well-formatted for readability: goals and domain; omit information already encoded in hmem's dependency graph; omit this field to keep the existing description or use null to clear it; not a log of progress or updates"
      schemaPropertyDescriptionIs "project_update" "parent_id" "Parent project UUID for hierarchy; omit to keep the existing parent or use null to clear it"
      schemaPropertyDescriptionIs "project_update" "status" "Execution state; record progress here, not in the description"
      schemaPropertyDescriptionIs "project_spec" "description" "Optional concise durable project specification in Markdown, well-formatted for readability: goals and domain; omit information already encoded in hmem's dependency graph; not a log of progress or updates"
      schemaPropertyDescriptionIs "task_create" "description" "Optional concise durable task specification in Markdown, well-formatted for readability: steps and completion criteria; omit information already encoded in hmem's dependency graph; not a log of progress or updates"
      schemaPropertyDescriptionIs "task_create" "parent_id" "Optional parent task UUID; subtasks can form a directed checklist for complex work or capture later-discovered atomic work"
      schemaPropertyDescriptionIs "task_update" "task_id" "Task UUID; the only required field"
      schemaPropertyDescriptionIs "task_update" "description" "Concise durable task specification in Markdown, well-formatted for readability: steps and completion criteria; omit information already encoded in hmem's dependency graph; omit this field to keep the existing description or use null to clear it; not a log of progress or updates"
      schemaPropertyDescriptionIs "task_update" "project_id" "Project UUID; omit to keep the existing project or use null to clear it"
      schemaPropertyDescriptionIs "task_update" "parent_id" "Parent task UUID; subtasks can form a directed checklist for complex work or capture later-discovered atomic work; omit to keep the existing parent or use null to clear it"
      schemaPropertyDescriptionIs "task_update" "status" "Execution state; record progress here, not in the description"
      schemaPropertyDescriptionIs "project_spec" "tasks" "One to 50 initial tasks, generally complete units deliverable on their own, created in input order; use subtasks as a directed checklist for complex work and later-discovered atomic work, not a parent description"
      projectSpecTaskPropertyDescriptionIs "title" "Title for a complete unit of work generally deliverable on its own"
      projectSpecTaskPropertyDescriptionIs "description" "Concise durable task specification in Markdown, well-formatted for readability: steps and completion criteria; omit information already encoded in hmem's dependency graph; not a log of progress or updates"
      projectSpecTaskPropertyDescriptionIs "priority" "Task priority"
      (schemaProperty "project_spec" "tasks" >>= jsonField "items" >>= jsonField "required") `shouldBe` Just (toJSON (["title"] :: [Text]))
      planningGuidance `shouldSatisfy` planningGuidanceContract
      T.replace "durable specification" "progress log" planningGuidance `shouldNotSatisfy` planningGuidanceContract
      T.replace "atomic work as subtasks" "parent description" planningGuidance `shouldNotSatisfy` planningGuidanceContract

    it "guides repository notes to double-check when used and treats Git SHAs as staleness sentinels" $ do
      forM_ ["all-term lexical", "ignored_filters", "next_requests", "directly in that project"] $ \phrase -> toolDescription "search" `shouldSatisfy` maybe False (T.isInfixOf phrase)
      forM_ ["search", "observation_discovery", "observation_create", "observation_get", "observation_update", "observation_list", "observation_match", "observation_similar"] $ \name ->
        toolDescription name `shouldSatisfy` maybe False (\description -> all (`T.isInfixOf` description)
          ["notes about specific areas or files in a repository workspace", "double-check them against the repository when used"])
      toolDescriptionIs "observation_create" "Create an Observation: a durable, non-obvious repository insight tied to one or more repository-relative file or glob subjects. git_sha records the repository state where the insight was established; use it as a sentinel to decide whether the insight needs re-audit, not as timeless proof. Pass subjects as an ordered array; they are OR alternatives and, with git_sha, immutable after creation. File subjects must be concrete paths. Glob subjects may use only *, ?, and ** path components (for example my/src/proj/**/*.java). Observations are notes about specific areas or files in a repository workspace; double-check them against the repository when used."
      toolDescriptionIs "observation_update" "Assert current content at reviewed_git_sha with the expected_content_version returned by a read. Repeated SHA/text assertions are accepted and advance version/history. Subjects and creation git_sha remain immutable; conflicts preserve latest canonical content. Observations are notes about specific areas or files in a repository workspace; double-check them against the repository when used."
      toolDescriptionIs "observation_list" "List durable, non-obvious repository insights using exact subject and git_sha provenance filters and optional text search. git_sha is a sentinel for deciding when an insight needs re-audit, not timeless proof. When has_more is true, pass next_offset to retrieve the next page. Observations are notes about specific areas or files in a repository workspace; double-check them against the repository when used."
      toolDescriptionIs "observation_match" "Find durable, non-obvious repository insights whose stored file subjects or safe glob subjects match any concrete repository-relative path supplied in paths. Paths are ORed; do not pass globs here and no repository filesystem is read. Optional filters compose with matching. git_sha is a staleness-audit sentinel, not timeless proof. Continue with next_offset until has_more is false. Observations are notes about specific areas or files in a repository workspace; double-check them against the repository when used."
      toolDescriptionIs "observation_similar" "Find semantically similar durable, non-obvious repository insights. Subject and git_sha filters are exact provenance filters; git_sha is a staleness-audit sentinel, not timeless proof. To continue, add returned_count to offset and repeat until returned_count is less than limit or zero. Observations are notes about specific areas or files in a repository workspace; double-check them against the repository when used."
      schemaPropertyDescriptionIs "observation_create" "content" "Durable, non-obvious note about specific areas or files in a repository workspace; double-check against the repository when used; not a progress update or routine fact"
      schemaPropertyDescriptionIs "observation_create" "git_sha" "Lowercase 40-character Git SHA for the repository state where this insight was established; a staleness-audit sentinel, not timeless proof"
      schemaPropertyDescriptionIs "observation_update" "content" "Reviewed current content"
      schemaPropertyDescriptionIs "observation_match" "paths" "One to 256 concrete repository-relative files to match against stored Observation subjects; globs are rejected"
      observationGuidance `shouldSatisfy` observationGuidanceContract
      T.replace "durable, non-obvious" "routine, obvious" observationGuidance `shouldNotSatisfy` observationGuidanceContract
      T.replace "not timeless proof" "timeless proof" observationGuidance `shouldNotSatisfy` observationGuidanceContract

    it "parses, validates, dispatches, and JSON-RPC-routes task dependency mutations" $ do
      let addArguments = object ["task_id" .= observationId, "depends_on_id" .= workspaceId, "action" .= ("add" :: Text)]
          removeArguments = object ["task_id" .= observationId, "depends_on_id" .= workspaceId, "action" .= ("remove" :: Text)]
      parseToolCall "task_dependency" addArguments `shouldSatisfy` isRight
      parseToolCall "task_dependency" (object ["task_id" .= observationId, "depends_on_id" .= workspaceId, "action" .= ("replace" :: Text)])
        `shouldSatisfy` isRight
      case parseToolCall "task_dependency" (object ["task_id" .= observationId, "depends_on_id" .= workspaceId, "action" .= ("replace" :: Text)]) of
        Right parsed -> validateToolCall parsed `shouldSatisfy` isLeft
        Left err -> expectationFailure err
      requests <- newTVarIO []
      withMock requests $ \manager base -> do
        added <- call manager base "task_dependency" addArguments
        added `shouldSatisfy` hasFields ["ok", "action", "entity_type", "task_id", "depends_on_id", "affected_tasks"]
        removed <- call manager base "task_dependency" removeArguments
        removed `shouldSatisfy` hasFields ["ok", "action", "entity_type", "task_id", "depends_on_id", "affected_tasks"]
        initialized <- newTVarIO True
        workspaceContext <- newTVarIO (Just (MCPContext Nothing testIdentity))
        jsonRpcToolCall manager base initialized workspaceContext "task_dependency" addArguments
          >>= (`shouldSatisfy` maybe False (not . isJsonRpcMcpError))
      [addRequest, removeRequest, jsonRpcRequest] <- readTVarIO requests
      addRequest.requestMethod `shouldBe` methodPost
      addRequest.requestPath `shouldBe` "/api/v1/tasks/aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee/dependencies"
      decode addRequest.requestBody `shouldBe` Just (object ["depends_on_id" .= workspaceId])
      removeRequest.requestMethod `shouldBe` methodDelete
      removeRequest.requestPath `shouldBe` "/api/v1/tasks/aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee/dependencies/11111111-2222-3333-4444-555555555555"
      jsonRpcRequest.requestPath `shouldBe` "/api/v1/tasks/aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee/dependencies"

    it "preserves structured dependency-cycle errors through the MCP and JSON-RPC bridges" $
      withDependencyCycleMock $ \manager base -> do
        let arguments = object ["task_id" .= observationId, "depends_on_id" .= workspaceId, "action" .= ("add" :: Text)]
        response <- handleToolCall manager base Nothing testIdentity (object ["name" .= ("task_dependency" :: Text), "arguments" .= arguments])
        response `shouldSatisfy` isMcpError
        response `shouldSatisfy` contains "[HTTP_400] Task dependency would create a cycle"
        response `shouldSatisfy` contains "dependency_cycle"
        initialized <- newTVarIO True
        workspaceContext <- newTVarIO (Just (MCPContext Nothing testIdentity))
        jsonRpcToolCall manager base initialized workspaceContext "task_dependency" arguments
          >>= (`shouldSatisfy` maybe False (\value -> isJsonRpcMcpError value && contains "dependency_cycle" value))

    it "dependencyScope preserves structured hierarchy conflicts through MCP and JSON-RPC" $
      testWithApplication (pure dependencyScopeApp) $ \port ->
        bracket (newManager defaultManagerSettings { managerIdleConnectionCount = 0 }) closeManager $ \manager -> do
          let arguments = object ["action" .= ("add" :: Text), "task_id" .= observationId, "depends_on_id" .= workspaceId]
              base = "http://127.0.0.1:" <> show port
          response <- handleToolCall manager base Nothing testIdentity (object ["name" .= ("task_dependency" :: Text), "arguments" .= arguments])
          response `shouldSatisfy` isMcpError
          response `shouldSatisfy` contains "TASK_DEPENDENCY_SCOPE_MISMATCH"
          initialized <- newTVarIO True
          workspaceContext <- newTVarIO (Just (MCPContext Nothing testIdentity))
          jsonRpcToolCall manager base initialized workspaceContext "task_dependency" arguments
            >>= (`shouldSatisfy` maybe False (\value -> isJsonRpcMcpError value && contains "TASK_DEPENDENCY_SCOPE_MISMATCH" value))

    it "parses, validates, and dispatches one atomic task batch-move request" $ do
      let observationUuid = read (T.unpack observationId) :: UUID
          workspaceUuid = read (T.unpack workspaceId) :: UUID
          arguments = object ["task_ids" .= [observationId, workspaceId], "project_id" .= workspaceId]
      case parseToolCall "task_move_batch" arguments of
        Right (TaskMoveBatch Nothing request) -> request `shouldBe` BatchMoveTasksRequest [observationUuid, workspaceUuid] (Just workspaceUuid)
        Right _ -> expectationFailure "expected TaskMoveBatch"
        Left err -> expectationFailure err
      parseToolCall "task_move_batch" (object ["task_ids" .= [observationUuid], "unexpected" .= True])
        `shouldSatisfy` isLeft
      case parseToolCall "task_move_batch" (object ["task_ids" .= ([] :: [UUID])]) of
        Right parsed -> validateToolCall parsed `shouldSatisfy` isLeft
        Left err -> expectationFailure err
      case parseToolCall "task_move_batch" (object ["task_ids" .= replicate 101 observationUuid]) of
        Right parsed -> validateToolCall parsed `shouldSatisfy` isLeft
        Left err -> expectationFailure err
      requests <- newTVarIO []
      withMock requests $ \manager base -> do
        result <- call manager base "task_move_batch" arguments
        result `shouldBe` object
          [ "ok" .= True
          , "action" .= ("moved" :: Text)
          , "entity_type" .= ("task" :: Text)
          , "affected" .= (2 :: Int)
          , "project_id" .= workspaceId
          ]
      [batchRequest] <- readTVarIO requests
      batchRequest.requestMethod `shouldBe` methodPost
      batchRequest.requestPath `shouldBe` "/api/v1/tasks/batch-move"
      decode batchRequest.requestBody `shouldBe` Just arguments

    it "preflights every distinct task and destination through the active JSON-RPC workspace context" $ do
      let arguments = object ["task_ids" .= [observationId, workspaceId, observationId], "project_id" .= workspaceId]
      requests <- newTVarIO []
      withMock requests $ \manager base -> do
        initialized <- newTVarIO True
        workspaceContext <- newTVarIO (Just (MCPContext Nothing testIdentity))
        _ <- serverToolCall manager initialized workspaceContext "set_workspace" (object ["workspace_id" .= workspaceId, "identity" .= testIdentity])
        jsonRpcToolCall manager base initialized workspaceContext "task_move_batch" arguments
          >>= (`shouldSatisfy` maybe False (not . isJsonRpcMcpError))
      observed <- readTVarIO requests
      map (.requestMethod) observed `shouldBe` [methodGet, methodGet, methodGet, methodPost]
      map (.requestPath) observed `shouldBe`
        [ "/api/v1/tasks/aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee"
        , "/api/v1/tasks/11111111-2222-3333-4444-555555555555"
        , "/api/v1/projects/11111111-2222-3333-4444-555555555555"
        , "/api/v1/tasks/batch-move"
        ]
      case observed of
        [firstTaskRequest, secondTaskRequest, projectRequest, batchRequest] -> do
          map (.requestBody) [firstTaskRequest, secondTaskRequest, projectRequest] `shouldBe` ["", "", ""]
          decode batchRequest.requestBody `shouldBe` Just (object ["task_ids" .= [observationId, workspaceId, observationId], "project_id" .= workspaceId])
        _ -> expectationFailure "expected two task preflights, one project preflight, and one batch move"

    it "rejects invalid contextual batch-move fields and strict unknown fields before HTTP dispatch" $ do
      requests <- newTVarIO []
      withMock requests $ \manager base -> do
        initialized <- newTVarIO True
        workspaceContext <- newTVarIO (Just (MCPContext (Just (read (T.unpack workspaceId))) testIdentity))
        forM_
          [ object ["task_ids" .= [observationId], "workspace_id" .= ("not-a-uuid" :: Text)]
          , object ["task_ids" .= [observationId], "workspace_id" .= True]
          , object ["task_ids" .= [observationId], "unexpected" .= True]
          ] $ \arguments ->
            jsonRpcToolCall manager base initialized workspaceContext "task_move_batch" arguments
              >>= (`shouldSatisfy` maybe False isJsonRpcMcpError)
      observed <- readTVarIO requests
      length observed `shouldBe` 0

    it "fails closed when contextual task or destination provenance is missing, malformed, or mismatched" $
      forM_
        [ (object ["id" .= observationId, "workspace_id" .= ("99999999-2222-3333-4444-555555555555" :: Text)], projectWithWorkspace)
        , (taskWithWorkspace, object ["id" .= workspaceId, "workspace_id" .= ("99999999-2222-3333-4444-555555555555" :: Text)])
        , (object ["id" .= observationId], projectWithWorkspace)
        , (object ["id" .= observationId, "workspace_id" .= ("not-a-uuid" :: Text)], projectWithWorkspace)
        , (taskWithWorkspace, object ["id" .= workspaceId])
        , (taskWithWorkspace, object ["id" .= workspaceId, "workspace_id" .= ("not-a-uuid" :: Text)])
        ] $ \(taskResponse, projectResponse) -> do
          requests <- newTVarIO []
          withBatchMovePreflightMock requests taskResponse projectResponse $ \manager base -> do
            initialized <- newTVarIO True
            workspaceContext <- newTVarIO (Just (MCPContext (Just (read (T.unpack workspaceId))) testIdentity))
            jsonRpcToolCall manager base initialized workspaceContext "task_move_batch"
              (object ["task_ids" .= [observationId], "project_id" .= workspaceId])
              >>= (`shouldSatisfy` maybe False isJsonRpcMcpError)
          observed <- readTVarIO requests
          map (.requestMethod) observed `shouldNotSatisfy` elem methodPost

    it "checks every distinct task response before dispatching a contextual batch move" $ do
      requests <- newTVarIO []
      withBatchMoveEveryTaskMismatchMock requests $ \manager base -> do
        initialized <- newTVarIO True
        workspaceContext <- newTVarIO (Just (MCPContext (Just (read (T.unpack workspaceId))) testIdentity))
        jsonRpcToolCall manager base initialized workspaceContext "task_move_batch"
          (object ["task_ids" .= [observationId, workspaceId], "project_id" .= workspaceId])
          >>= (`shouldSatisfy` maybe False isJsonRpcMcpError)
      observed <- readTVarIO requests
      map (.requestMethod) observed `shouldBe` [methodGet, methodGet, methodGet]
      map (.requestMethod) observed `shouldNotSatisfy` elem methodPost

    it "forwards contextual preflight GET errors and never starts the batch move" $ do
      requests <- newTVarIO []
      withBatchMoveGetErrorMock requests $ \manager base -> do
        initialized <- newTVarIO True
        workspaceContext <- newTVarIO (Just (MCPContext (Just (read (T.unpack workspaceId))) testIdentity))
        response <- jsonRpcToolCall manager base initialized workspaceContext "task_move_batch"
          (object ["task_ids" .= [observationId], "project_id" .= workspaceId])
        response `shouldSatisfy` maybe False (\value -> isJsonRpcMcpError value && contains "[HTTP_404] Task not found." value)
      observed <- readTVarIO requests
      map (.requestMethod) observed `shouldBe` [methodGet, methodGet]
      map (.requestMethod) observed `shouldNotSatisfy` elem methodPost

    it "preserves structured lifecycle conflicts through the MCP and JSON-RPC bridges" $
      withLifecycleConflictMock $ \manager base -> do
        let arguments = object ["task_ids" .= [observationId], "project_id" .= workspaceId]
        response <- handleToolCall manager base Nothing testIdentity (object ["name" .= ("task_move_batch" :: Text), "arguments" .= arguments])
        response `shouldSatisfy` isMcpError
        response `shouldSatisfy` contains "[HTTP_409] Cannot move tasks because dependency endpoints would span projects."
        response `shouldSatisfy` contains "lifecycle_conflict"
        response `shouldSatisfy` contains "TASK_DEPENDENCY_CROSS_PROJECT"
        initialized <- newTVarIO True
        workspaceContext <- newTVarIO (Just (MCPContext Nothing testIdentity))
        jsonRpcToolCall manager base initialized workspaceContext "task_move_batch" arguments
          >>= (`shouldSatisfy` maybe False (\value -> isJsonRpcMcpError value && contains "TASK_DEPENDENCY_CROSS_PROJECT" value))

    it "preserves parent-start subtask blockers through MCP and JSON-RPC" $
      withLifecycleConflictMock $ \manager base -> do
        let arguments = object ["task_id" .= observationId]
        response <- handleToolCall manager base Nothing testIdentity (object ["name" .= ("task_start" :: Text), "arguments" .= arguments])
        response `shouldSatisfy` isMcpError
        response `shouldSatisfy` contains "TASK_START_BLOCKED_BY_SUBTASKS"
        response `shouldSatisfy` contains "blocker_count"
        initialized <- newTVarIO True
        workspaceContext <- newTVarIO (Just (MCPContext Nothing testIdentity))
        jsonRpcToolCall manager base initialized workspaceContext "task_start" arguments
          >>= (`shouldSatisfy` maybe False (\value -> isJsonRpcMcpError value && contains "TASK_START_BLOCKED_BY_SUBTASKS" value))

    it "keeps the advertised registry parser and dispatch reachability in lockstep" $ do
      sort toolNames `shouldBe` sort (serverOwnedTools <> map fst toolSamples)
      mapM_ (\(name, arguments) -> parseToolCall name arguments `shouldSatisfy` isRight) toolSamples
      requests <- newTVarIO []
      withMock requests $ \manager base ->
        forM_ toolSamples $ \(name, arguments) -> do
          response <- handleToolCall manager base Nothing testIdentity (object ["name" .= name, "arguments" .= arguments])
          response `shouldSatisfy` not . isMcpError
      dispatched <- readTVarIO requests
      all (`elem` map (.requestPath) dispatched)
        [ "/api/v1/workspaces", "/api/v1/search", "/api/v1/observations"
        , "/api/v1/observations/aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee"
         , "/api/v1/observations/aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee/embedding"
         , "/api/v1/observations/match", "/api/v1/observations/similar", "/api/v1/projects", "/api/v1/projects/spec", "/api/v1/tasks"
        ] `shouldBe` True
      manager <- newManager defaultManagerSettings
      initialized <- newTVarIO True
      workspaceContext <- newTVarIO (Just (MCPContext Nothing testIdentity))
      setResponse <- serverToolCall manager initialized workspaceContext "set_workspace" (object ["workspace_id" .= workspaceId, "identity" .= testIdentity])
      setResponse `shouldSatisfy` maybe False (contains "workspace_context")
      getResponse <- serverToolCall manager initialized workspaceContext "get_workspace" (object [])
      getResponse `shouldSatisfy` maybe False (contains workspaceId)

    it "routes every live tool through JSON-RPC, including server-owned workspace context" $ do
      requests <- newTVarIO []
      withMock requests $ \manager base -> do
        initialized <- newTVarIO True
        workspaceContext <- newTVarIO (Just (MCPContext Nothing testIdentity))
        forM_ toolSamples $ \(name, arguments) -> do
          response <- jsonRpcToolCall manager base initialized workspaceContext name arguments
          response `shouldSatisfy` maybe False (not . isJsonRpcMcpError)
        setResponse <- jsonRpcToolCall manager base initialized workspaceContext "set_workspace" (object ["workspace_id" .= workspaceId, "identity" .= testIdentity])
        setResponse `shouldSatisfy` maybe False (not . isJsonRpcMcpError)
        getResponse <- jsonRpcToolCall manager base initialized workspaceContext "get_workspace" (object [])
        getResponse `shouldSatisfy` maybe False (contains workspaceId)

    it "injects the session workspace into observation_match when agents omit it" $ do
      requests <- newTVarIO []
      withMock requests $ \manager base -> do
        initialized <- newTVarIO True
        workspaceContext <- newTVarIO (Just (MCPContext Nothing testIdentity))
        _ <- serverToolCall manager initialized workspaceContext "set_workspace" (object ["workspace_id" .= workspaceId, "identity" .= testIdentity])
        response <- jsonRpcToolCall manager base initialized workspaceContext "observation_match"
          (object ["paths" .= (["src/HMem/Types.hs"] :: [Text])])
        response `shouldSatisfy` maybe False (not . isJsonRpcMcpError)
      [request] <- readTVarIO requests
      decode request.requestBody `shouldBe` Just (object ["workspace_id" .= workspaceId, "paths" .= (["src/HMem/Types.hs"] :: [Text])])

  describe "Observation compact response shaping" $ do
    it "derives bounded content_preview from a create/get response when the server supplies full content" $ do
      let summary = compactObservationSummary (object ["id" .= observationId, "subjects" .= observationSubjects, "subject_kind" .= ("file" :: Text), "subject" .= ("src/HMem/Types.hs" :: Text), "git_sha" .= gitSha, "content" .= (T.replicate 600 "x")])
      jsonField "content_preview" summary `shouldBe` Just (String (T.replicate 500 "x"))
      summary `shouldSatisfy` hasFields ["id", "subjects", "subject_kind", "subject", "git_sha", "content_preview"]

    it "matches fixture-backed compact golden outputs for new Observation tools" $ do
      golden <- compactFixtures
      requests <- newTVarIO []
      withMock requests $ \manager base -> do
        created <- call manager base "observation_create" observationArguments
        created `shouldBe` fixturePayload golden "observation_create"
        searched <- call manager base "search" (object ["workspace_id" .= workspaceId, "entity_types" .= (["observation"] :: [Text])])
        searched `shouldBe` fixturePayload golden "unified_search"
        listed <- call manager base "observation_list" observationListArguments
        listed `shouldBe` fixturePayload golden "observation_list"
        deleted <- call manager base "observation_delete" (object ["observation_id" .= observationId])
        deleted `shouldBe` fixturePayload golden "observation_delete"
        embedded <- call manager base "observation_set_embedding" (object ["observation_id" .= observationId, "embedding" .= embedding])
        embedded `shouldBe` fixturePayload golden "observation_set_embedding"
        similar <- call manager base "observation_similar" similarArguments
        similar `shouldBe` fixturePayload golden "observation_similar"
        matched <- call manager base "observation_match" observationMatchArguments
        matched `shouldBe` fixturePayload golden "observation_match"

    it "forwards one search page request with its filters and preserves continuation" $ do
      requests <- newTVarIO []
      withMock requests $ \manager base -> do
        result <- call manager base "search" (object
          [ "workspace_id" .= workspaceId, "query" .= ("needle" :: Text)
          , "entity_types" .= (["project", "task"] :: [Text])
          , "project_status" .= ("archived" :: Text), "task_status" .= ("done" :: Text)
          , "project_id" .= workspaceId, "limit" .= (2 :: Int), "offset" .= (4 :: Int) ])
        jsonField "has_more" result `shouldBe` Just (object ["projects" .= True, "tasks" .= False])
        jsonField "next_offset" result `shouldBe` Just (object ["projects" .= (5 :: Int)])
        jsonField "projects" result `shouldSatisfy` maybe False (arrayFirst (\row -> jsonField "description" row == Nothing && jsonField "id" row == Just (toJSON observationId)))
        jsonField "tasks" result `shouldSatisfy` maybe False (arrayFirst (\row -> jsonField "description" row == Nothing && jsonField "id" row == Just (toJSON observationId)))
      [sent] <- readTVarIO requests
      sent.requestMethod `shouldBe` methodPost
      sent.requestPath `shouldBe` "/api/v1/search"
      let Just payload = decode sent.requestBody :: Maybe Value
      forM_ ["workspace_id", "query", "entity_types", "project_status", "task_status", "project_id", "limit", "offset"] $ \key ->
        jsonField key payload `shouldBe` jsonField key (object ["workspace_id" .= workspaceId, "query" .= ("needle" :: Text), "entity_types" .= (["project", "task"] :: [Text]), "project_status" .= ("archived" :: Text), "task_status" .= ("done" :: Text), "project_id" .= workspaceId, "limit" .= (2 :: Int), "offset" .= (4 :: Int)])

  describe "Project and Task response shaping" $ do
    it "omits due across every planning tool response even when old upstream task data contains it" $ do
      requests <- newTVarIO []
      let hasDue (Object fields) = any (`KM.member` fields) ["due_at", "dueAt", "due", "due_date", "dueDate"] || any hasDue (KM.elems fields)
          hasDue (Array values) = any hasDue values
          hasDue _ = False
      withMock requests $ \manager base ->
        forM_ [(name,args) | (name,args) <- toolSamples, "task_" `T.isPrefixOf` name || "project_" `T.isPrefixOf` name] $ \(name,args) -> do
          result <- call manager base name args
          result `shouldNotSatisfy` isMcpError
          result `shouldNotSatisfy` hasDue

    it "preserves project pause counts, retained guidance and compact task causes" $ do
      requests <- newTVarIO []
      withMock requests $ \manager base -> do
        result <- call manager base "project_update" (object ["project_id" .= observationId, "status" .= ("active" :: Text)])
        jsonPath ["pause_effects", "task_count"] result `shouldBe` Just (Number 23)
        jsonPath ["pause_effects", "remaining_ancestor_pause"] result `shouldBe` Just (Bool True)
        jsonPath ["pause_effects", "tasks"] result `shouldSatisfy` maybe False (arrayFirst (\effect ->
          jsonPath ["task", "description"] effect == Nothing && jsonField "project_paused" effect == Just (Bool True)))
        jsonPath ["summary", "description"] result `shouldBe` Nothing
      [sent] <- readTVarIO requests
      sent.requestMethod `shouldBe` methodPut
      jsonField "own_paused" (maybe Null id (decode sent.requestBody)) `shouldBe` Nothing
      withWorkspaceStructuredStatusMock status403 "Denied" $ \manager base -> do
        denied <- handleToolCall manager base Nothing testIdentity (object ["name" .= ("project_update" :: Text), "arguments" .= object ["project_id" .= observationId, "status" .= ("paused" :: Text)]])
        denied `shouldSatisfy` isMcpError
        jsonField "pause_effects" denied `shouldBe` Nothing

    it "matches compact summary fixtures while leaving full descriptions to detail calls" $ do
      golden <- compactFixtures
      compactProjectSummary projectWithLongDescription `shouldBe` fixturePayload golden "project_summary"
      compactTaskSummary taskWithLongDescription `shouldBe` fixturePayload golden "task_summary"
      BL.length (encode (compactProjectSummary projectWithLongDescription)) `shouldSatisfy` (< 300)
      BL.length (encode (compactTaskSummary taskWithLongDescription)) `shouldSatisfy` (< 350)
      let searched = compactSearchResults (object ["observations" .= ([] :: [Value]), "projects" .= [projectWithLongDescription], "tasks" .= [taskWithLongDescription], "has_more" .= object ["projects" .= True, "tasks" .= False], "next_offset" .= object ["projects" .= (2 :: Int)]])
      jsonField "projects" searched `shouldBe` Just (toJSON [fixturePayload golden "project_summary"])
      jsonField "tasks" searched `shouldBe` Just (toJSON [fixturePayload golden "task_summary"])
      jsonField "has_more" searched `shouldBe` Just (object ["projects" .= True, "tasks" .= False])
      jsonField "next_offset" searched `shouldBe` Just (object ["projects" .= (2 :: Int)])
      BL.length (encode searched) `shouldSatisfy` (< 700)

    it "preserves ready subtask identity and parent links in project next tasks" $ do
      requests <- newTVarIO []
      withMock requests $ \manager base -> do
        result <- call manager base "project_next_tasks" (object ["project_id" .= ("bbbbbbbb-cccc-dddd-eeee-ffffffffffff" :: Text)])
        case result of
          Array rows -> case toList rows of
            [candidate] -> do
              jsonPath ["task", "id"] candidate `shouldBe` Just (String "cccccccc-dddd-eeee-ffff-aaaaaaaaaaaa")
              jsonPath ["task", "parent_id"] candidate `shouldBe` Just (toJSON observationId)
              jsonPath ["task", "priority"] candidate `shouldBe` Just (Number 9)
              jsonPath ["task", "status"] candidate `shouldBe` Just (String "todo")
              jsonPath ["task", "description"] candidate `shouldBe` Nothing
              jsonField "dependency_blocked" candidate `shouldBe` Just (Bool False)
            _ -> expectationFailure "Expected one ready subtask candidate"
          _ -> expectationFailure "Expected next-task candidate array"

    it "keeps aggregate and mutation replies bounded and focused details lossless" $ do
      requests <- newTVarIO []
      withMock requests $ \manager base -> do
        let projectId = observationId
            taskId = observationId
            projectArgs = object ["project_id" .= projectId]
            taskArgs = object ["task_id" .= taskId]
        projectDetail <- call manager base "project_detail" projectArgs
        taskDetail <- call manager base "task_detail" taskArgs
        jsonField "description" projectDetail `shouldBe` jsonField "description" projectWithLongDescription
        jsonField "description" taskDetail `shouldBe` jsonField "description" taskWithLongDescription
        jsonField "id" projectDetail `shouldBe` Just (toJSON projectId)
        jsonField "id" taskDetail `shouldBe` Just (toJSON taskId)
        projectOverview <- call manager base "project_overview" projectArgs
        taskOverview <- call manager base "task_overview" taskArgs
        nextTasks <- call manager base "project_next_tasks" projectArgs
        searched <- call manager base "search" (object ["workspace_id" .= workspaceId, "entity_types" .= (["project", "task"] :: [Text])])
        forM_ [jsonPath ["project"] projectOverview, jsonPath ["task"] taskOverview] $ \result ->
          (result >>= jsonField "description") `shouldBe` Nothing
        jsonField "tasks" projectOverview `shouldSatisfy` maybe False (arrayFirst (\row -> jsonField "description" row == Nothing))
        jsonField "subprojects" projectOverview `shouldSatisfy` maybe False (arrayFirst (\row -> jsonField "description" row == Nothing))
        jsonPath ["readiness_rollup", "completion_ready"] taskOverview `shouldBe` Just (Bool True)
        nextTasks `shouldSatisfy` arrayFirst (\row -> jsonPath ["task", "description"] row == Nothing && jsonField "open_descendant_count" row == Just (Number 2))
        jsonField "projects" searched `shouldSatisfy` maybe False (arrayFirst (\row -> jsonField "id" row == Just (toJSON observationId) && jsonField "description" row == Nothing))
        jsonField "tasks" searched `shouldSatisfy` maybe False (arrayFirst (\row -> jsonField "id" row == Just (toJSON observationId) && jsonField "description" row == Nothing))
        jsonField "has_more" searched `shouldBe` Just (object ["projects" .= True, "tasks" .= False])
        jsonField "next_offset" searched `shouldBe` Just (object ["projects" .= (1 :: Int)])
        forM_ ["project_create", "project_update", "project_archive", "task_create", "task_update", "task_start", "task_finish"] $ \name -> do
          let arguments = case lookup name toolSamples of Just sample -> sample; Nothing -> object []
          response <- call manager base name arguments
          jsonPath ["summary", "description"] response `shouldBe` Nothing
          jsonField "id" response `shouldBe` Just (toJSON observationId)
          if "task_" `T.isPrefixOf` name
            then do
              jsonPath ["dependency_effects"] response `shouldSatisfy` maybe False (arrayFirst (\effect -> jsonPath ["task", "description"] effect == Nothing && jsonField "previous_status" effect == Just (String "todo") && jsonField "current_status" effect == Just (String "blocked") && jsonField "previous_auto_blocked" effect == Just (Bool False) && jsonField "previous_open_dependency_count" effect == Just (Number 0)))
              BL.length (encode response) `shouldSatisfy` (< 1000)
            else if name `elem` ["project_update", "project_archive"] then do
              jsonPath ["pause_effects", "task_count"] response `shouldBe` Just (Number 23)
              jsonPath ["pause_effects", "remaining_ancestor_pause"] response `shouldBe` Just (Bool True)
              jsonPath ["pause_effects", "tasks"] response `shouldSatisfy` maybe False (arrayFirst (\effect ->
                jsonPath ["task", "description"] effect == Nothing && jsonField "project_paused" effect == Just (Bool True)
                  && jsonField "previous_project_paused" effect == Just (Bool False)))
            else pure ()
        specResult <- call manager base "project_spec" (case lookup "project_spec" toolSamples of Just sample -> sample; Nothing -> object [])
        jsonField "project_id" specResult `shouldBe` Just (toJSON observationId)
        jsonField "task_ids" specResult `shouldBe` Just (toJSON [observationId])
        BL.length (encode specResult) `shouldSatisfy` (< 500)
        dependencyResult <- call manager base "task_dependency" (object ["task_id" .= taskId, "depends_on_id" .= workspaceId, "action" .= ("add" :: Text)])
        jsonPath ["affected_tasks"] dependencyResult `shouldSatisfy` maybe False (arrayFirst (\effect -> jsonPath ["task", "description"] effect == Nothing && jsonField "current_status" effect == Just (String "blocked")))

  describe "Observation parsing and validation" $ do
    it "parses provenance-bound creates and reviewed conditional updates" $ do
      case parseToolCall "observation_create" observationArguments of
        Right (ObservationCreate (CreateObservation _ values sha _)) -> do
          values `shouldBe` observationSubjects
          sha `shouldBe` gitSha
        result -> expectationFailure (show result)
      case parseToolCall "observation_update" (object ["workspace_id" .= workspaceId, "observation_id" .= observationId, "content" .= ("replacement" :: Text), "reviewed_git_sha" .= gitSha, "expected_content_version" .= observationId]) of
        Right (ObservationUpdate _ _ (ReviewedObservationUpdate content _)) -> content `shouldBe` "replacement"
        result -> expectationFailure (show result)

    it "rejects mutable provenance fields on update and malformed provenance on create" $ do
      parseToolCall "observation_update" (object ["observation_id" .= observationId, "content" .= ("replacement" :: Text), "reviewed_git_sha" .= gitSha, "expected_content_version" .= observationId, "git_sha" .= gitSha]) `shouldSatisfy` isLeft
      parseToolCall "observation_create" (object ["workspace_id" .= workspaceId, "subjects" .= observationSubjects, "subject_kind" .= ("file" :: Text), "subject" .= ("src/HMem/Types.hs" :: Text), "git_sha" .= gitSha, "content" .= ("content" :: Text)]) `shouldSatisfy` isLeft
      case parseToolCall "observation_create" (object ["workspace_id" .= workspaceId, "subjects" .= [object ["subject_kind" .= ("file" :: Text), "subject" .= ("/absolute" :: Text)]], "git_sha" .= ("bad" :: Text), "content" .= ("content" :: Text)]) of
        Right parsed -> validateToolCall parsed `shouldSatisfy` isLeft
        result -> expectationFailure (show result)

    it "accepts observation search provenance filters and rejects legacy entity types" $ do
      parseToolCall "search" (object ["workspace_id" .= workspaceId, "entity_types" .= (["observation"] :: [Text]), "subject_kind" .= ("file" :: Text), "subject" .= ("src/HMem/Types.hs" :: Text), "git_sha" .= gitSha]) `shouldSatisfy` isRight
      parseToolCall "search" (object ["workspace_id" .= workspaceId, "entity_types" .= (["memory"] :: [Text])]) `shouldSatisfy` isLeft
      (parseToolCall "search" (object ["workspace_id" .= workspaceId, "offset" .= (100001 :: Int)]) >>= validateToolCall) `shouldSatisfy` isRight
      (parseToolCall "search" (object ["workspace_id" .= workspaceId, "offset" .= (2147483648 :: Int)]) >>= validateToolCall) `shouldSatisfy` isLeft

    it "validates Observation pagination, similarity bounds, and fixed embeddings before dispatch" $ do
      case parseToolCall "observation_list" (object ["workspace_id" .= workspaceId, "limit" .= (201 :: Int)]) of
        Right parsed -> validateToolCall parsed `shouldSatisfy` isLeft
        result -> expectationFailure (show result)
      case parseToolCall "observation_similar" (object ["workspace_id" .= workspaceId, "embedding" .= ([0 :: Double] :: [Double]), "min_similarity" .= (1.1 :: Double)]) of
        Right parsed -> validateToolCall parsed `shouldSatisfy` isLeft
        result -> expectationFailure (show result)
      case parseToolCall "observation_set_embedding" (object ["observation_id" .= observationId, "embedding" .= ([0 :: Double] :: [Double])]) of
        Right parsed -> validateToolCall parsed `shouldSatisfy` isLeft
        result -> expectationFailure (show result)
      parseToolCall "observation_set_embedding" (object
        [ "observation_id" .= observationId, "embedding" .= embedding
        , "space_fingerprint" .= ("hmem:managed\x2003space" :: Text)
        ]) `shouldSatisfy` isLeft
      parseToolCall "observation_set_embedding" (object
        [ "observation_id" .= observationId, "embedding" .= embedding, "space_fingerprint" .= Null
        ]) `shouldSatisfy` isLeft
      parseToolCall "observation_similar" (object
        [ "workspace_id" .= workspaceId, "embedding" .= embedding, "space_fingerprint" .= Null
        ]) `shouldSatisfy` isLeft
      case parseToolCall "observation_set_embedding" (object
        [ "observation_id" .= observationId, "embedding" .= embedding
        , "space_fingerprint" .= ("hmem:managed-gte-qwen2:mcp-test" :: Text)
        ]) of
          Right parsed -> validateToolCall parsed `shouldBe` Right parsed
          result -> expectationFailure (show result)
      case parseToolCall "observation_match" (object ["workspace_id" .= workspaceId, "paths" .= (["src/**/*.hs"] :: [Text])]) of
        Right parsed -> validateToolCall parsed `shouldSatisfy` isLeft
        result -> expectationFailure (show result)

    it "supports canonical multi-subject creates and unadvertised legacy singleton compatibility" $ do
      case parseToolCall "observation_create" legacyObservationArguments of
        Right (ObservationCreate (CreateObservation _ values _ _)) -> values `shouldBe` [ObservationSubject SubjectFile "src/HMem/Types.hs"]
        result -> expectationFailure (show result)
      parseToolCall "observation_create" (object ["workspace_id" .= workspaceId, "subject_kind" .= ("file" :: Text), "git_sha" .= gitSha, "content" .= ("content" :: Text)]) `shouldSatisfy` isLeft

    it "rejects incomplete legacy create forms before any HTTP request" $ do
      requests <- newTVarIO []
      withMock requests $ \manager base -> do
        let malformed fields = handleToolCall manager base Nothing testIdentity
              (object ["name" .= ("observation_create" :: Text), "arguments" .= object (["workspace_id" .= workspaceId, "git_sha" .= gitSha, "content" .= ("content" :: Text)] <> fields)])
        subjectOnly <- malformed ["subject" .= ("src/HMem/Types.hs" :: Text)]
        kindOnly <- malformed ["subject_kind" .= ("file" :: Text)]
        subjectOnly `shouldSatisfy` isMcpError
        kindOnly `shouldSatisfy` isMcpError
      requestsAfter <- readTVarIO requests
      length requestsAfter `shouldBe` 0

    it "rejects 257 unique subjects and concrete match paths before HTTP dispatch" $ do
      requests <- newTVarIO []
      let tooManySubjects = [ObservationSubject SubjectFile ("src/File" <> T.pack (show index) <> ".hs") | index <- [1 .. maxObservationSubjects + 1]]
          tooManyPaths = ["src/File" <> T.pack (show index) <> ".hs" | index <- [1 .. 257 :: Int]]
      withMock requests $ \manager base -> do
        tooManyCreate <- handleToolCall manager base Nothing testIdentity (object
          [ "name" .= ("observation_create" :: Text)
          , "arguments" .= object ["workspace_id" .= workspaceId, "subjects" .= tooManySubjects, "git_sha" .= gitSha, "content" .= ("content" :: Text)]
          ])
        tooManyMatch <- handleToolCall manager base Nothing testIdentity (object
          [ "name" .= ("observation_match" :: Text)
          , "arguments" .= object ["workspace_id" .= workspaceId, "paths" .= tooManyPaths]
          ])
        tooManyCreate `shouldSatisfy` isMcpError
        tooManyMatch `shouldSatisfy` isMcpError
      requestsAfter <- readTVarIO requests
      length requestsAfter `shouldBe` 0

  describe "Observation dispatch and status-only workflows" $ do
    it "uses Observation paths and compact provenance shapes" $ do
      requests <- newTVarIO []
      withMock requests $ \manager base -> do
        created <- call manager base "observation_create" observationArguments
        jsonField "entity_type" created `shouldBe` Just (String "observation")
        jsonField "summary" created `shouldSatisfy` maybe False (hasFields ["id", "subject_kind", "subject", "git_sha", "content_preview"])
        jsonField "workspace_id" created `shouldBe` Nothing
        detail <- call manager base "observation_get" (object ["observation_id" .= observationId])
        jsonField "content" detail `shouldBe` Just (String "complete observation content")
        detail `shouldSatisfy` hasFields ["subjects", "subject_kind", "subject"]
        detail `shouldSatisfy` not . contains "workspace_id"
        update <- call manager base "observation_update" (object ["observation_id" .= observationId, "content" .= ("replacement" :: Text), "reviewed_git_sha" .= gitSha, "expected_content_version" .= observationId])
        jsonField "action" update `shouldBe` Just (String "updated")
      observed <- readTVarIO requests
      map (.requestPath) observed `shouldBe` ["/api/v1/observations", "/api/v1/observations/aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee", "/api/v1/observations/aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee"]
      map (.requestMethod) observed `shouldBe` [methodPost, methodGet, methodPut]
      (last observed).requestIfMatch `shouldBe` Just (TE.encodeUtf8 ("\"" <> observationId <> "\""))
      decode ((last observed).requestBody) `shouldBe` Just (object ["content" .= ("replacement" :: Text), "reviewed_git_sha" .= gitSha])

    it "passes exact provenance filters through unified search and compacts observations" $ do
      requests <- newTVarIO []
      withMock requests $ \manager base -> do
        result <- call manager base "search" (object ["workspace_id" .= workspaceId, "entity_types" .= (["observation"] :: [Text]), "subject_kind" .= ("file" :: Text), "subject" .= ("src/HMem/Types.hs" :: Text), "git_sha" .= gitSha])
        jsonField "observations" result `shouldSatisfy` maybe False (arrayFirst (hasFields ["id", "subject_kind", "subject", "git_sha", "content_preview"]))
        result `shouldSatisfy` not . contains "linked_memories"
      [RequestInfo _ _ _ body _ _ _ _ _ _] <- readTVarIO requests
      decode body `shouldSatisfy` maybe False (\value -> hasFields ["subject_kind", "subject", "git_sha"] value)

    it "forwards list filters and gives an exact next offset only when the server reports another page" $ do
      requests <- newTVarIO []
      withMock requests $ \manager base -> do
        first <- call manager base "observation_list" observationListArguments
        jsonField "has_more" first `shouldBe` Just (Bool True)
        jsonField "next_offset" first `shouldBe` Just (Number 2)
        jsonField "items" first `shouldSatisfy` maybe False (arrayFirst (hasFields ["id", "subjects", "subject_kind", "subject", "git_sha", "content_preview"]))
        final <- call manager base "observation_list" (object ["workspace_id" .= workspaceId, "limit" .= (2 :: Int), "offset" .= (2 :: Int)])
        jsonField "has_more" final `shouldBe` Just (Bool False)
        jsonField "next_offset" final `shouldBe` Nothing
      [firstRequest, finalRequest] <- readTVarIO requests
      firstRequest.requestPath `shouldBe` "/api/v1/observations"
      firstRequest.requestQuery `shouldBe` "?workspace_id=11111111-2222-3333-4444-555555555555&subject_kind=file&subject=src%2FHMem%2FTypes.hs&git_sha=0123456789abcdef0123456789abcdef01234567&query=types&limit=2&offset=0"
      finalRequest.requestQuery `shouldBe` "?workspace_id=11111111-2222-3333-4444-555555555555&limit=2&offset=2"

    it "sends canonical subject arrays for both advertised and legacy singleton creation" $ do
      requests <- newTVarIO []
      withMock requests $ \manager base -> do
        _ <- call manager base "observation_create" observationArguments
        _ <- call manager base "observation_create" legacyObservationArguments
        pure ()
      [canonicalRequest, legacyRequest] <- readTVarIO requests
      let expectedCanonical = object ["workspace_id" .= workspaceId, "subjects" .= observationSubjects, "git_sha" .= gitSha, "content" .= ("complete observation content" :: Text)]
          expectedLegacy = object ["workspace_id" .= workspaceId, "subjects" .= [ObservationSubject SubjectFile "src/HMem/Types.hs"], "git_sha" .= gitSha, "content" .= ("complete observation content" :: Text)]
      canonicalRequest.requestMethod `shouldBe` methodPost
      legacyRequest.requestMethod `shouldBe` methodPost
      canonicalRequest.requestPath `shouldBe` "/api/v1/observations"
      legacyRequest.requestPath `shouldBe` "/api/v1/observations"
      decode canonicalRequest.requestBody `shouldBe` Just expectedCanonical
      decode legacyRequest.requestBody `shouldBe` Just expectedLegacy

    it "matches concrete paths with deterministic evidence, bearer forwarding, and continuation metadata" $ do
      requests <- newTVarIO []
      withMock requests $ \manager base -> do
        result <- callWithKey manager base (Just "test-token") "observation_match" observationMatchArguments
        jsonField "has_more" result `shouldBe` Just (Bool True)
        jsonField "returned_count" result `shouldBe` Just (Number 2)
        jsonField "next_offset" result `shouldBe` Just (Number 2)
        jsonField "items" result `shouldSatisfy` maybe False (arrayFirst (hasFields ["observation", "path_matches", "matched_paths", "matched_subjects"]))
      [request] <- readTVarIO requests
      request.requestMethod `shouldBe` methodPost
      request.requestPath `shouldBe` "/api/v1/observations/match"
      request.authorization `shouldBe` Just "Bearer test-token"
      decode request.requestBody `shouldBe` Just (object ["workspace_id" .= workspaceId, "paths" .= (["src/HMem/Types.hs", "my/src/proj/Main.java"] :: [Text]), "subject_kind" .= ("glob" :: Text), "git_sha" .= gitSha, "query" .= ("types" :: Text), "limit" .= (2 :: Int), "offset" .= (0 :: Int)])

    it "continues observation_match without a final or empty cursor" $ do
      requests <- newTVarIO []
      withMock requests $ \manager base -> do
        first <- call manager base "observation_match" observationMatchArguments
        jsonField "has_more" first `shouldBe` Just (Bool True)
        jsonField "next_offset" first `shouldBe` Just (Number 2)
        final <- call manager base "observation_match" (withOffset observationMatchArguments 2)
        jsonField "has_more" final `shouldBe` Just (Bool False)
        jsonField "returned_count" final `shouldBe` Just (Number 0)
        jsonField "next_offset" final `shouldBe` Nothing
      observed <- readTVarIO requests
      map (.requestMethod) observed `shouldBe` [methodPost, methodPost]
      map (decode . (.requestBody)) observed `shouldBe`
        [ Just (object ["workspace_id" .= workspaceId, "paths" .= (["src/HMem/Types.hs", "my/src/proj/Main.java"] :: [Text]), "subject_kind" .= ("glob" :: Text), "git_sha" .= gitSha, "query" .= ("types" :: Text), "limit" .= (2 :: Int), "offset" .= (0 :: Int)])
        , Just (object ["workspace_id" .= workspaceId, "paths" .= (["src/HMem/Types.hs", "my/src/proj/Main.java"] :: [Text]), "subject_kind" .= ("glob" :: Text), "git_sha" .= gitSha, "query" .= ("types" :: Text), "limit" .= (2 :: Int), "offset" .= (2 :: Int)])
        ]

    it "deduplicates match rows and emits no cursor for nonempty-final or empty pages" $ do
      let matchedObservation = object ["id" .= observationId, "subjects" .= observationSubjects, "subject_kind" .= ("file" :: Text), "subject" .= ("src/HMem/Types.hs" :: Text), "git_sha" .= gitSha, "content_preview" .= ("preview" :: Text)]
          pathEvidence = [object ["path" .= ("src/HMem/Types.hs" :: Text), "matched_subjects" .= observationSubjects]]
          row = object ["observation" .= matchedObservation, "path_matches" .= pathEvidence, "matched_paths" .= (["src/HMem/Types.hs"] :: [Text]), "matched_subjects" .= observationSubjects]
          page = compactObservationMatches 4 (object ["items" .= [row, row], "has_more" .= True])
          nonemptyFinalPage = compactObservationMatches 6 (object ["items" .= [row], "has_more" .= False])
          finalPage = compactObservationMatches 6 (object ["items" .= ([] :: [Value]), "has_more" .= False])
      jsonField "returned_count" page `shouldBe` Just (Number 2)
      jsonField "next_offset" page `shouldBe` Just (Number 6)
      case jsonField "items" page of
        Just (Array values) -> length values `shouldBe` 1
        _ -> expectationFailure "match page did not contain items"
      jsonField "returned_count" nonemptyFinalPage `shouldBe` Just (Number 1)
      jsonField "next_offset" nonemptyFinalPage `shouldBe` Nothing
      jsonField "returned_count" finalPage `shouldBe` Just (Number 0)
      jsonField "next_offset" finalPage `shouldBe` Nothing

    it "uses raw embeddings, forwards authorization, and returns signal-only acknowledgements for 204 mutations" $ do
      requests <- newTVarIO []
      withMock requests $ \manager base -> do
        deleted <- callWithKey manager base (Just "test-token") "observation_delete" (object ["observation_id" .= observationId])
        deleted `shouldBe` object ["ok" .= True, "action" .= ("deleted" :: Text), "entity_type" .= ("observation" :: Text), "id" .= observationId]
        embedded <- callWithKey manager base (Just "test-token") "observation_set_embedding" (object ["observation_id" .= observationId, "embedding" .= embedding])
        embedded `shouldBe` object ["ok" .= True, "action" .= ("embedding_set" :: Text), "entity_type" .= ("observation" :: Text), "id" .= observationId]
      [deleteRequest, embeddingRequest] <- readTVarIO requests
      map (.requestMethod) [deleteRequest, embeddingRequest] `shouldBe` [methodDelete, methodPut]
      embeddingRequest.requestPath `shouldBe` "/api/v1/observations/aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee/embedding"
      decode embeddingRequest.requestBody `shouldBe` Just (toJSON embedding)
      map (.authorization) [deleteRequest, embeddingRequest] `shouldBe` [Just "Bearer test-token", Just "Bearer test-token"]

    it "compacts similar results and provides only repeat-until-short continuation metadata" $ do
      requests <- newTVarIO []
      withMock requests $ \manager base -> do
        result <- call manager base "observation_similar" similarArguments
        jsonField "items" result `shouldSatisfy` maybe False (arrayFirst (hasFields ["observation", "similarity"]))
        jsonField "offset" result `shouldBe` Just (Number 4)
        jsonField "limit" result `shouldBe` Just (Number 2)
        jsonField "returned_count" result `shouldBe` Just (Number 1)
        jsonField "has_more" result `shouldBe` Nothing
      [request] <- readTVarIO requests
      request.requestPath `shouldBe` "/api/v1/observations/similar"
      request.requestMethod `shouldBe` methodPost
      decode request.requestBody `shouldBe` Just (object ["workspace_id" .= workspaceId, "subject_kind" .= ("file" :: Text), "subject" .= ("src/HMem/Types.hs" :: Text), "git_sha" .= gitSha, "embedding" .= embedding, "min_similarity" .= (0.5 :: Double), "limit" .= (2 :: Int), "offset" .= (4 :: Int)])

    it "propagates REST failures, including pgvector capability errors" $ do
      withErrorMock $ \manager base -> do
        result <- handleToolCall manager base Nothing testIdentity (object ["name" .= ("observation_set_embedding" :: Text), "arguments" .= object ["observation_id" .= observationId, "embedding" .= embedding]])
        result `shouldSatisfy` contains "[HTTP_400] pgvector extension is required"

    it "propagates HTTP status failures and forwards Bearer authorization on Observation reads" $ do
      requests <- newTVarIO []
      withStatusMock requests $ \manager base -> do
        let run :: Text -> Value -> Text -> IO ()
            run name arguments expected = do
              response <- handleToolCall manager base (Just "test-token") testIdentity (object ["name" .= name, "arguments" .= arguments])
              response `shouldSatisfy` contains expected
        run "observation_get" (object ["observation_id" .= observationId]) "[HTTP_401] status 401"
        run "observation_list" observationListArguments "[HTTP_403] status 403"
        run "observation_delete" (object ["observation_id" .= observationId]) "[HTTP_404] status 404"
        run "observation_set_embedding" (object ["observation_id" .= observationId, "embedding" .= embedding]) "[HTTP_409] status 409"
        run "observation_similar" similarArguments "[HTTP_400] status 400"
        run "observation_match" observationMatchArguments "[HTTP_500] status 500"
      observed <- readTVarIO requests
      map (.authorization) observed `shouldBe` replicate 6 (Just "Bearer test-token")

    it "sends completion cancellation and archival as single authoritative status mutations" $ do
      requests <- newTVarIO []
      withMock requests $ \manager base -> do
        finished <- call manager base "task_finish" (object ["task_id" .= observationId, "status" .= ("done" :: Text)])
        jsonField "action" finished `shouldBe` Just (String "finished")
        cancelled <- call manager base "task_finish" (object ["task_id" .= observationId, "status" .= ("cancelled" :: Text)])
        jsonField "action" cancelled `shouldBe` Just (String "finished")
        archived <- call manager base "project_archive" (object ["project_id" .= observationId])
        jsonField "action" archived `shouldBe` Just (String "archived")
      observed <- readTVarIO requests
      map (.requestPath) observed `shouldBe` ["/api/v1/tasks/aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee", "/api/v1/tasks/aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee", "/api/v1/projects/aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee"]
      map (.requestMethod) observed `shouldBe` [methodPut, methodPut, methodPut]
      map (decode . (.requestBody)) observed `shouldBe` map (Just . (\status -> object ["status" .= status])) (["done", "cancelled", "archived"] :: [Text])

  describe "Canonical change-stream integration" $
    it "carries a real MCP proxy mutation through REST into the durable outbox and snapshot" $
      withTestEnv $ \env -> do
        if env.testDb.testDbUnsafeExternal
          then fail "MCP release gate refuses unsafe external DB mode"
          else pure ()
        tracker <- newAccessTracker env.pool 3600
        wsState <- newWSState
        let cfg = trustedMcpConfig
        app <- mkAppWithChangeStream cfg.changeStream id cfg.auth cfg.cors cfg.rateLimit
          env.pool tracker wsState Nothing True
        workspace <- createTestWorkspace env "mcp-change-stream-release-gate"
        withEnvironment "HMEM_MCP_PROVENANCE_TOKEN" trustedMcpProvenance $
          testWithApplication (pure app) $ \port ->
            bracket (newManager defaultManagerSettings { managerIdleConnectionCount = 0 }) closeManager $ \manager -> do
              let base = "http://127.0.0.1:" <> show port
              created <- callWithKey manager base (Just localMcpBotToken) "project_create"
                (object ["workspace_id" .= workspace.id, "name" .= ("MCP canonical project" :: Text)])
              projectId <- case jsonField "summary" created >>= jsonField "id" of
                Just (String value) -> pure value
                other -> expectationFailure ("MCP create omitted project identity: " <> show other) >> fail "unreachable"

              records <- listOutboxAfter env.pool (WorkspaceScope workspace.id) 0 20
              let matching =
                    [ record
                    | record <- records
                    , jsonPath ["entity", "type"] record.outboxEnvelope == Just (String "project")
                    , jsonPath ["entity", "id"] record.outboxEnvelope == Just (String projectId)
                    ]
              case matching of
                [record] -> do
                  jsonPath ["transaction", "cause"] record.outboxEnvelope `shouldBe` Just (String "mcp")
                  jsonField "invalidations" record.outboxEnvelope `shouldSatisfy` maybe False (contains ("project:" <> projectId))
                  mapM_ (\forbidden -> jsonField forbidden record.outboxEnvelope `shouldBe` Nothing)
                    ["old_values", "new_values", "embedding"]
                _ -> expectationFailure "expected exactly one canonical MCP project record"

              snapshot <- DBPool.runSession env.pool (materializeSnapshot (WorkspaceScope workspace.id))
              snapshot `shouldSatisfy` any (\item ->
                jsonField "kind" item == Just (String "project")
                  && jsonPath ["data", "id"] item == Just (String projectId))
              fetched <- callWithKey manager base (Just localMcpBotToken) "project_detail"
                (object ["project_id" .= projectId])
              jsonField "id" fetched `shouldBe` Just (String projectId)

sessionDetailArguments, sessionTerminateArguments, sessionMetadata :: Value
sessionDetailArguments = object ["workspace_id" .= workspaceId, "session_id" .= observationId]
sessionTerminateArguments = object ["workspace_id" .= workspaceId, "session_id" .= observationId
  , "expected_offset" .= (0 :: Int), "expected_revision" .= (0 :: Int), "final_bytes" .= (0 :: Int), "final_sha256" .= AgentSession.sessionSha256 ""]
sessionMetadata = object
  ["id" .= observationId, "workspace_id" .= workspaceId, "title" .= ("Test session work" :: Text), "format_label" .= ("provider-jsonl" :: Text), "state" .= ("active" :: Text)
  , "creator_actor_type" .= ("bot" :: Text), "creator_actor_id" .= ("actor-id" :: Text), "creator_actor_label" .= ("MCP actor" :: Text), "creator_bot_identity" .= testIdentity
  , "created_at" .= ("2026-01-01T00:00:00Z" :: Text), "last_seen_at" .= ("2026-01-01T00:00:00Z" :: Text)
  , "committed_bytes" .= (0 :: Int), "committed_records" .= (0 :: Int), "revision" .= (0 :: Int), "ends_with_newline" .= True
  , "terminated_at" .= Null, "final_bytes" .= Null, "final_sha256" .= Null]

setField :: Text -> Value -> Value -> Value
setField name value (Object fields) = Object (KM.insert (Key.fromText name) value fields)
setField _ _ value = value

withSessionMock :: TVar [RequestInfo] -> (Manager -> String -> IO a) -> IO a
withSessionMock requests action = testWithApplication (pure app) $ \port ->
  bracket (newManager defaultManagerSettings {managerIdleConnectionCount = 0}) closeManager $ \manager -> action manager ("http://127.0.0.1:" <> show port)
  where
    app request respond = do
      body <- Wai.strictRequestBody request
      atomically $ modifyTVar' requests (<> [RequestInfo request.requestMethod (T.unpack (TE.decodeUtf8 request.rawPathInfo)) request.rawQueryString body (lookup "Authorization" request.requestHeaders) (lookup "X-HMem-Change-Cause" request.requestHeaders) (lookup "X-HMem-MCP-Provenance" request.requestHeaders) (lookup "If-Match" request.requestHeaders) [value | (name, value) <- request.requestHeaders, name == "X-HMem-Bot-Identity"] (lookup "X-Request-Id" request.requestHeaders)])
      let isTaskList = "/tasks" `BS.isSuffixOf` request.rawPathInfo
          isList = "/sessions" `BS.isSuffixOf` request.rawPathInfo && request.requestMethod == methodGet
          later = any (`BS.isInfixOf` request.rawQueryString) ["before_id=", "after_task_id="]
          items = if later then [] else [if isTaskList then object ["id" .= workspaceId, "title" .= ("Linked task" :: Text), "status" .= ("todo" :: Text)] else sessionMetadata]
          value
            | "/runtime" `BS.isSuffixOf` request.rawPathInfo =
                let input=fromMaybe Null (decode body)
                    runtime=fromMaybe Null (jsonField "runtime_identity" input)
                    revision=case jsonField "expected_runtime_revision" input of Just (Number n) -> Number (n+1); _ -> Number 0
                in setField "runtime_revision" revision (setField "runtime_identity" runtime sessionMetadata)
            | isTaskList && request.requestMethod == methodGet = object ["items" .= items, "next_task_id" .= (if later then Null else String workspaceId)]
            | isList = object ["items" .= items, "next_cursor" .= (if later then Null else object ["created_at" .= ("2026-01-01T00:00:00Z" :: Text), "id" .= observationId])]
            | "/terminate" `BS.isSuffixOf` request.rawPathInfo = setField "state" (String "terminated") sessionMetadata
            | otherwise = sessionMetadata
      respond $ if request.requestMethod == methodPut && not ("/runtime" `BS.isSuffixOf` request.rawPathInfo) then Wai.responseLBS status204 [] ""
        else Wai.responseLBS status200 [("Content-Type", "application/json")] (encode value)

withSessionProxyApp :: (UUID -> Manager -> String -> IO a) -> IO a
withSessionProxyApp = withRenameProxyApp renameSuperadmin

sessionHTTP :: Manager -> String -> UUID -> UUID -> String -> Value -> IO (Client.Response BL.ByteString)
sessionHTTP manager base workspace sid suffix body = do
  initial <- Client.parseRequest (base <> "/api/v1/workspaces/" <> show workspace <> "/sessions/" <> show sid <> suffix)
  Client.httpLbs initial {Client.method = methodPost, Client.requestHeaders = [("Content-Type", "application/json"), ("X-HMem-Bot-Identity", BL.toStrict (encode testIdentity))], Client.requestBody = Client.RequestBodyLBS (encode body)} manager

downloadSession :: (Text -> Value -> IO Value) -> Text -> Value -> IO ByteString
downloadSession invoke name = go 0 []
  where
    go count chunks arguments
      | count > (1000 :: Int) = expectationFailure "Session download did not complete" >> pure BS.empty
      | otherwise = do
          result <- invoke name arguments
          let Just (String encoded) = jsonField "data_base64" result
          bytes <- case SessionWire.decodeSessionBytes encoded of
            Right value -> pure value
            Left _ -> expectationFailure "Noncanonical session bytes" >> pure BS.empty
          case jsonPath ["next_request", "arguments"] result of
            Nothing -> do
              jsonField "has_more" result `shouldBe` Just (Bool False)
              pure (BS.concat (reverse (bytes:chunks)))
            Just next -> go (count+1) (bytes:chunks) next

workspaceId, observationId, gitSha :: Text
workspaceId = "11111111-2222-3333-4444-555555555555"
observationId = "aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee"
gitSha = "0123456789abcdef0123456789abcdef01234567"

observationArguments :: Value
observationArguments = object ["workspace_id" .= workspaceId, "subjects" .= observationSubjects, "git_sha" .= gitSha, "content" .= ("complete observation content" :: Text)]

legacyObservationArguments :: Value
legacyObservationArguments = object ["workspace_id" .= workspaceId, "subject_kind" .= ("file" :: Text), "subject" .= ("src/HMem/Types.hs" :: Text), "git_sha" .= gitSha, "content" .= ("complete observation content" :: Text)]

observationSubjects :: [ObservationSubject]
observationSubjects =
  [ ObservationSubject SubjectFile "src/HMem/Types.hs"
  , ObservationSubject SubjectGlob "my/src/proj/**/*.java"
  ]

observationMatchArguments :: Value
observationMatchArguments = object
  [ "workspace_id" .= workspaceId
  , "paths" .= (["src/HMem/Types.hs", "my/src/proj/Main.java"] :: [Text])
  , "subject_kind" .= ("glob" :: Text)
  , "git_sha" .= gitSha
  , "query" .= ("types" :: Text)
  , "limit" .= (2 :: Int)
  , "offset" .= (0 :: Int)
  ]

discoveryArguments :: Value
discoveryArguments = object ["workspace_id" .= workspaceId, "filters" .= object []]

discoveryCounts :: Value
discoveryCounts = object ["workspace_id" .= workspaceId, "total_count" .= (100 :: Int), "match_count" .= (7 :: Int)]

discoveryFixture :: Text -> Text -> Bool -> Value
discoveryFixture mode view more = object
  [ "context_token" .= observationId, "expires_at" .= ("2026-10-21T10:05:00Z" :: Text)
  , "retrieval" .= object ["mode" .= mode, "reason" .= (if mode == "hybrid" then Null else String "disabled")]
  , "complete" .= True
  , "page" .= object
      (["items" .= (if view == "list" then [responseFor methodGet "/api/v1/observations/aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee" "" ""] else [])
       , "has_more" .= (view == "list" && more), "counts" .= discoveryCounts]
       <> ["tree" .= object ["has_more" .= more, "total_count" .= (100 :: Int), "match_count" .= (7 :: Int)
             , "items" .= [node "src/A.hs" "A.hs", node "src/B.hs" "B.hs"]] | view == "tree"])
  ]
  where
    node path segment = object ["path" .= (path :: Text), "segment" .= (segment :: Text), "has_children" .= False, "observation_count" .= (5 :: Int)
      , "terminals" .= [object ["subject_kind" .= ("file" :: Text), "subject" .= path, "observation_count" .= (3 :: Int)]]]

withOffset :: Value -> Int -> Value
withOffset (Object values) value = Object (KM.insert "offset" (toJSON value) values)
withOffset arguments _ = arguments

observationListArguments :: Value
observationListArguments = object
  [ "workspace_id" .= workspaceId
  , "subject_kind" .= ("file" :: Text)
  , "subject" .= ("src/HMem/Types.hs" :: Text)
  , "git_sha" .= gitSha
  , "query" .= ("types" :: Text)
  , "limit" .= (2 :: Int)
  , "offset" .= (0 :: Int)
  ]

embedding :: [Double]
embedding = replicate 1536 0.25

similarArguments :: Value
similarArguments = object
  [ "workspace_id" .= workspaceId
  , "subject_kind" .= ("file" :: Text)
  , "subject" .= ("src/HMem/Types.hs" :: Text)
  , "git_sha" .= gitSha
  , "embedding" .= embedding
  , "min_similarity" .= (0.5 :: Double)
  , "limit" .= (2 :: Int)
  , "offset" .= (4 :: Int)
  ]

serverOwnedTools :: [Text]
serverOwnedTools = ["set_workspace", "get_workspace"]

toolSamples :: [(Text, Value)]
toolSamples =
  [ ("workspace_list", object [])
  , ("workspace_register", object ["name" .= ("Repository" :: Text)])
  , ("workspace_update", object ["workspace_id" .= workspaceId, "name" .= ("Renamed workspace" :: Text)])
  , ("search", object ["workspace_id" .= workspaceId, "entity_types" .= (["observation"] :: [Text]), "offset" .= (0 :: Int)])
  , ("observation_discovery", discoveryArguments)
  , ("observation_create", observationArguments)
  , ("observation_get", object ["observation_id" .= observationId])
  , ("observation_update", object ["observation_id" .= observationId, "content" .= ("replacement" :: Text), "reviewed_git_sha" .= gitSha, "expected_content_version" .= observationId])
  , ("observation_history", object ["observation_id" .= observationId, "limit" .= (1 :: Int), "offset" .= (0 :: Int)])
        , ("observation_list", observationListArguments)
  , ("observation_match", observationMatchArguments)
  , ("observation_delete", object ["observation_id" .= observationId])
  , ("observation_set_embedding", object ["observation_id" .= observationId, "embedding" .= embedding])
  , ("observation_similar", similarArguments)
  , ("project_create", object ["workspace_id" .= workspaceId, "name" .= ("Project" :: Text)])
  , ("project_update", object ["project_id" .= observationId, "name" .= ("Updated project" :: Text)])
  , ("project_detail", object ["project_id" .= observationId])
  , ("project_overview", object ["project_id" .= observationId])
  , ("project_next_tasks", object ["project_id" .= observationId])
  , ( "project_spec"
    , object
        [ "workspace_id" .= workspaceId
        , "name" .= ("Project specification" :: Text)
        , "tasks" .= [object ["title" .= ("Initial task" :: Text)]]
        ]
    )
  , ("project_archive", object ["project_id" .= observationId])
  , ("task_create", object ["workspace_id" .= workspaceId, "title" .= ("Task" :: Text)])
  , ("task_update", object ["task_id" .= observationId, "title" .= ("Updated task" :: Text)])
  , ("task_detail", object ["task_id" .= observationId])
  , ("task_overview", object ["task_id" .= observationId])
  , ("task_dependency", object ["task_id" .= observationId, "depends_on_id" .= workspaceId, "action" .= ("add" :: Text)])
  , ("task_move_batch", object ["task_ids" .= [observationId], "project_id" .= workspaceId])
  , ("task_start", object ["task_id" .= observationId])
  , ("task_finish", object ["task_id" .= observationId, "status" .= ("done" :: Text)])
  ]

toolNames :: [Text]
toolNames = [name | Object tool <- toolDefinitions, Just (String name) <- [KM.lookup "name" tool]]

schemaProperties :: Text -> [Text]
schemaProperties name = case [schema | Object tool <- toolDefinitions, KM.lookup "name" tool == Just (String name), Just schema <- [KM.lookup "inputSchema" tool]] of
  Object schema : _ -> case KM.lookup "properties" schema of Just (Object properties) -> Key.toText <$> KM.keys properties; _ -> []
  _ -> []

schemaRequired :: Text -> [Text]
schemaRequired name = case [schema | Object tool <- toolDefinitions, KM.lookup "name" tool == Just (String name), Just schema <- [KM.lookup "inputSchema" tool]] of
  Object schema : _ -> case KM.lookup "required" schema of Just (Array fields) -> [field | String field <- toList fields]; _ -> []
  _ -> []

schemaAdditionalProperties :: Text -> Maybe Value
schemaAdditionalProperties name = case [schema | Object tool <- toolDefinitions, KM.lookup "name" tool == Just (String name), Just schema <- [KM.lookup "inputSchema" tool]] of
  Object schema : _ -> KM.lookup "additionalProperties" schema
  _ -> Nothing

toolDescription :: Text -> Maybe Text
toolDescription name = case [description | Object tool <- toolDefinitions, KM.lookup "name" tool == Just (String name), Just (String description) <- [KM.lookup "description" tool]] of description : _ -> Just description; [] -> Nothing

toolDescriptionIs :: Text -> Text -> Expectation
toolDescriptionIs name expected = toolDescription name `shouldBe` Just expected

planningGuidance :: Text
planningGuidance = T.intercalate "\n" $
  [ toolDescription name
  | name <- ["project_create", "project_update", "project_spec", "task_create", "task_update", "task_start", "task_finish"]
  ] >>= maybe [] pure

planningGuidanceContract :: Text -> Bool
planningGuidanceContract text =
  all (`T.isInfixOf` text)
    [ "durable specification"
    , "not logs of progress or updates"
    , "status records"
    , "atomic work as subtasks"
    , "complete unit of work deliverable on its own"
    , "directed checklist"
    , "tasks and subprojects in a hierarchy"
    , "long-running or short-lived work"
    , "goals and domain"
    , "steps and completion criteria"
    , "excluding information already encoded in hmem's dependency graph"
    ]

observationGuidance :: Text
observationGuidance = T.intercalate "\n" $
  [ toolDescription name
  | name <- ["search", "observation_discovery", "observation_create", "observation_get", "observation_update", "observation_list", "observation_match", "observation_similar"]
  ] >>= maybe [] pure

observationGuidanceContract :: Text -> Bool
observationGuidanceContract text =
  all (`T.isInfixOf` text)
    [ "durable, non-obvious repository insight"
    , "file or glob subjects"
    , "repository-relative"
    , "creation git_sha remain immutable"
    , "staleness-audit sentinel"
    , "not timeless proof"
    , "notes about specific areas or files in a repository workspace"
    , "double-check them against the repository when used"
    ]

schemaProperty :: Text -> Text -> Maybe Value
schemaProperty toolName propertyName = case
  [ property
  | Object tool <- toolDefinitions
  , KM.lookup "name" tool == Just (String toolName)
  , Just (Object inputSchema) <- [KM.lookup "inputSchema" tool]
  , Just (Object properties) <- [KM.lookup "properties" inputSchema]
  , Just property <- [KM.lookup (Key.fromText propertyName) properties]
  ] of
    property : _ -> Just property
    [] -> Nothing

schemaPropertyDescriptionIs :: Text -> Text -> Text -> Expectation
schemaPropertyDescriptionIs toolName propertyName expected =
  (schemaProperty toolName propertyName >>= jsonField "description")
    `shouldBe` Just (String expected)

projectSpecTaskPropertyDescriptionIs :: Text -> Text -> Expectation
projectSpecTaskPropertyDescriptionIs propertyName expected =
  (schemaProperty "project_spec" "tasks" >>= jsonField "items" >>= jsonField "properties" >>= objectField propertyName >>= jsonField "description")
    `shouldBe` Just (String expected)

objectField :: Text -> Value -> Maybe Value
objectField name (Object fields) = KM.lookup (Key.fromText name) fields
objectField _ _ = Nothing

stringOrNullTypes :: Value -> Bool
stringOrNullTypes (Array values) = map (jsonField "type") (toList values) == [Just (String "string"), Just (String "null")]
stringOrNullTypes _ = False

isUnknown :: Text -> Either String ToolCall -> Bool
isUnknown name = \case Left message -> ("Unknown tool: " <> T.unpack name) == message; Right _ -> False
isLeft :: Either a b -> Bool
isLeft = \case Left _ -> True; Right _ -> False
isRight :: Either a b -> Bool
isRight = not . isLeft

isMcpError :: Value -> Bool
isMcpError value = jsonField "isError" value == Just (Bool True)

isJsonRpcMcpError :: Value -> Bool
isJsonRpcMcpError value = maybe True isMcpError (jsonField "result" value)

jsonField :: Text -> Value -> Maybe Value
jsonField key (Object objectValue) = KM.lookup (Key.fromText key) objectValue
jsonField _ _ = Nothing
jsonPath :: [Text] -> Value -> Maybe Value
jsonPath keys value = foldl (\current key -> current >>= jsonField key) (Just value) keys
hasFields :: [Text] -> Value -> Bool
hasFields keys value = all (\key -> jsonField key value /= Nothing) keys
contains :: Text -> Value -> Bool
contains needle = T.isInfixOf needle . TE.decodeUtf8 . BL.toStrict . encode
arrayFirst :: (Value -> Bool) -> Value -> Bool
arrayFirst predicate (Array values) = case toList values of value : _ -> predicate value; [] -> False
arrayFirst _ _ = False

call :: Manager -> String -> Text -> Value -> IO Value
call manager base = callWithKey manager base Nothing

rawSearch :: Manager -> String -> IO Value
rawSearch manager base = handleToolCall manager base Nothing testIdentity
  (object ["name" .= ("search" :: Text), "arguments" .= object ["workspace_id" .= workspaceId]])

withSearchResponse :: Status -> BL.ByteString -> (Manager -> String -> IO a) -> IO a
withSearchResponse status body action =
  testWithApplication (pure (\_ respond -> respond (Wai.responseLBS status [("Content-Type", "application/json")] body))) $ \port ->
    bracket (newManager defaultManagerSettings { managerIdleConnectionCount = 0 }) closeManager $ \manager -> action manager ("http://127.0.0.1:" <> show port)

decodeMcpJSON :: Value -> Maybe Value
decodeMcpJSON response = do
  Array contents <- jsonField "content" response
  first : _ <- pure (toList contents)
  String text <- jsonField "text" first
  decodeStrict' (TE.encodeUtf8 text)

expectSearchStructured :: Value -> Expectation
expectSearchStructured = expectToolStructured "search"

expectToolStructured :: Text -> Value -> Expectation
expectToolStructured name response = do
  let [outputSchema] = [value | Object tool <- toolDefinitions, KM.lookup "name" tool == Just (String name), Just value <- [KM.lookup "outputSchema" tool]]
  case jsonField "structuredContent" response of
    Nothing -> expectationFailure ("Missing structured search result: " <> show response)
    Just structured -> do
      schemaAccepts outputSchema structured `shouldBe` True
      -- Negative controls ensure the validator checks required properties and
      -- nested types, rather than merely accepting any object-shaped envelope.
      schemaAccepts outputSchema (object []) `shouldBe` False
      schemaAccepts outputSchema (object ["error" .= object ["code" .= (2 :: Int), "field" .= Null, "message" .= ("bad" :: Text)]]) `shouldBe` False

-- Independent interpreter for the JSON Schema vocabulary used by this output
-- contract. It checks every nested property, including additionalProperties,
-- and does not import the result projection or its implementation helpers.
schemaAccepts :: Value -> Value -> Bool
schemaAccepts (Object rules) value = all id
  [ maybe True (\case Array choices -> any (\choice -> schemaAccepts choice value) choices; _ -> False) (KM.lookup "anyOf" rules)
  , maybe True (\case String typ -> hasType typ value; _ -> False) (KM.lookup "type" rules)
  , maybe True (\case Array choices -> value `elem` choices; _ -> False) (KM.lookup "enum" rules)
  , case value of
      Object fields ->
        let properties = case KM.lookup "properties" rules of Just (Object entries) -> entries; _ -> KM.empty
            required = case KM.lookup "required" rules of Just (Array keys) -> [Key.fromText key | String key <- toList keys]; _ -> []
        in all (`KM.member` fields) required
          && all (\(key, item) -> case KM.lookup key properties of
              Just property -> schemaAccepts property item
              Nothing -> KM.lookup "additionalProperties" rules /= Just (Bool False)) (KM.toList fields)
      Array items -> maybe True (\itemSchema -> all (schemaAccepts itemSchema) items) (KM.lookup "items" rules)
        && maybe True (\case Number lower -> fromIntegral (length items) >= lower; _ -> False) (KM.lookup "minItems" rules)
        && maybe True (\case Number upper -> fromIntegral (length items) <= upper; _ -> False) (KM.lookup "maxItems" rules)
      _ -> True
  ]
  where
    hasType "object" (Object _) = True
    hasType "array" (Array _) = True
    hasType "string" (String _) = True
    hasType "boolean" (Bool _) = True
    hasType "null" Null = True
    hasType "integer" (Number number) = number == fromInteger (round number)
    hasType _ _ = False
schemaAccepts _ _ = False

callWithKey :: Manager -> String -> Maybe Text -> Text -> Value -> IO Value
callWithKey manager base apiKey name arguments = do
  response <- handleToolCall manager base apiKey testIdentity (object ["name" .= name, "arguments" .= arguments])
  case jsonField "content" response >>= arrayText >>= (either (const Nothing) Just . eitherDecodeStrict' . TE.encodeUtf8) of
    Just value -> pure value
    Nothing -> expectationFailure ("Expected MCP JSON response, got " <> show response) >> pure Null
  where
    arrayText (Array values) = case toList values of
      Object first : _ -> case KM.lookup "text" first of Just (String text) -> Just text; _ -> Nothing
      _ -> Nothing
    arrayText _ = Nothing

serverToolCall :: Manager -> TVar Bool -> TVar (Maybe MCPContext) -> Text -> Value -> IO (Maybe Value)
serverToolCall manager initialized workspaceContext name arguments =
  handleStdioLine manager "http://unused.invalid" Nothing initialized workspaceContext
    (BL.toStrict (encode (object
      [ "jsonrpc" .= ("2.0" :: Text)
      , "id" .= ("workspace-context" :: Text)
      , "method" .= ("tools/call" :: Text)
      , "params" .= object ["name" .= name, "arguments" .= arguments]
      ])))

jsonRpcToolCall :: Manager -> String -> TVar Bool -> TVar (Maybe MCPContext) -> Text -> Value -> IO (Maybe Value)
jsonRpcToolCall manager base initialized workspaceContext name arguments =
  handleStdioLine manager base Nothing initialized workspaceContext
    (BL.toStrict (encode (object
      [ "jsonrpc" .= ("2.0" :: Text)
      , "id" .= ("jsonrpc-tool-call" :: Text)
      , "method" .= ("tools/call" :: Text)
      , "params" .= object ["name" .= name, "arguments" .= arguments]
      ])))

compactFixtures :: IO Value
compactFixtures = do
  let candidates = ["test/fixtures/mcp-compact-responses.json", "hmem-mcp/test/fixtures/mcp-compact-responses.json"]
  available <- filterM doesFileExist candidates
  case available of
    path : _ -> do
      decoded <- eitherDecode <$> BL.readFile path
      case decoded of Right fixture -> pure fixture; Left err -> expectationFailure err >> pure Null
    [] -> expectationFailure "mcp compact response fixture not found" >> pure Null

fixturePayload :: Value -> Text -> Value
fixturePayload fixtures name = case jsonField name fixtures >>= jsonField "payload" of
  Just payload -> payload
  Nothing -> error ("missing fixture payload: " <> T.unpack name)

data RequestInfo = RequestInfo
  { requestMethod :: ByteString
  , requestPath :: String
  , requestQuery :: ByteString
  , requestBody :: BL.ByteString
  , authorization :: Maybe ByteString
  , requestChangeCause :: Maybe ByteString
  , requestMcpProvenance :: Maybe ByteString
  , requestIfMatch :: Maybe ByteString
  , requestIdentityHeaders :: [ByteString]
  , requestId :: Maybe ByteString
  }

withMock :: TVar [RequestInfo] -> (Manager -> String -> IO a) -> IO a
withMock requests action = testWithApplication (pure (mockApp requests)) $ \port ->
  bracket (newManager defaultManagerSettings { managerIdleConnectionCount = 0 }) closeManager $ \manager ->
    action manager ("http://127.0.0.1:" <> show port)

withBatchMovePreflightMock :: TVar [RequestInfo] -> Value -> Value -> (Manager -> String -> IO a) -> IO a
withBatchMovePreflightMock requests taskResponse projectResponse action =
  testWithApplication (pure (batchMovePreflightApp requests taskResponse projectResponse)) $ \port ->
    bracket (newManager defaultManagerSettings { managerIdleConnectionCount = 0 }) closeManager $ \manager ->
      action manager ("http://127.0.0.1:" <> show port)

withBatchMoveGetErrorMock :: TVar [RequestInfo] -> (Manager -> String -> IO a) -> IO a
withBatchMoveGetErrorMock requests action =
  testWithApplication (pure (batchMoveGetErrorApp requests)) $ \port ->
    bracket (newManager defaultManagerSettings { managerIdleConnectionCount = 0 }) closeManager $ \manager ->
      action manager ("http://127.0.0.1:" <> show port)

withBatchMoveEveryTaskMismatchMock :: TVar [RequestInfo] -> (Manager -> String -> IO a) -> IO a
withBatchMoveEveryTaskMismatchMock requests action =
  testWithApplication (pure (batchMoveEveryTaskMismatchApp requests)) $ \port ->
    bracket (newManager defaultManagerSettings { managerIdleConnectionCount = 0 }) closeManager $ \manager ->
      action manager ("http://127.0.0.1:" <> show port)

batchMovePreflightApp :: TVar [RequestInfo] -> Value -> Value -> Wai.Application
batchMovePreflightApp requests taskResponse projectResponse request respond = do
  body <- Wai.strictRequestBody request
  atomically $ modifyTVar' requests (<> [RequestInfo request.requestMethod (T.unpack (TE.decodeUtf8 request.rawPathInfo)) request.rawQueryString body (lookup "Authorization" request.requestHeaders) (lookup "X-HMem-Change-Cause" request.requestHeaders) (lookup "X-HMem-MCP-Provenance" request.requestHeaders) (lookup "If-Match" request.requestHeaders) [value | (name, value) <- request.requestHeaders, name == "X-HMem-Bot-Identity"] (lookup "X-Request-Id" request.requestHeaders)])
  let path = request.rawPathInfo
      response
        | "/api/v1/tasks/" `BS.isPrefixOf` path = taskResponse
        | "/api/v1/projects/" `BS.isPrefixOf` path = projectResponse
        | otherwise = object ["affected" .= (1 :: Int)]
  respond $ Wai.responseLBS status200 [("Content-Type", "application/json")] (encode response)

batchMoveGetErrorApp :: TVar [RequestInfo] -> Wai.Application
batchMoveGetErrorApp requests request respond = do
  body <- Wai.strictRequestBody request
  atomically $ modifyTVar' requests (<> [RequestInfo request.requestMethod (T.unpack (TE.decodeUtf8 request.rawPathInfo)) request.rawQueryString body (lookup "Authorization" request.requestHeaders) (lookup "X-HMem-Change-Cause" request.requestHeaders) (lookup "X-HMem-MCP-Provenance" request.requestHeaders) (lookup "If-Match" request.requestHeaders) [value | (name, value) <- request.requestHeaders, name == "X-HMem-Bot-Identity"] (lookup "X-Request-Id" request.requestHeaders)])
  let response
        | "/api/v1/tasks/" `BS.isPrefixOf` request.rawPathInfo = Wai.responseLBS status404 [("Content-Type", "application/json")] (encode (object ["error" .= ("not_found" :: Text), "message" .= ("Task not found." :: Text)]))
        | otherwise = Wai.responseLBS status200 [("Content-Type", "application/json")] (encode projectWithWorkspace)
  respond response

batchMoveEveryTaskMismatchApp :: TVar [RequestInfo] -> Wai.Application
batchMoveEveryTaskMismatchApp requests request respond = do
  body <- Wai.strictRequestBody request
  atomically $ modifyTVar' requests (<> [RequestInfo request.requestMethod (T.unpack (TE.decodeUtf8 request.rawPathInfo)) request.rawQueryString body (lookup "Authorization" request.requestHeaders) (lookup "X-HMem-Change-Cause" request.requestHeaders) (lookup "X-HMem-MCP-Provenance" request.requestHeaders) (lookup "If-Match" request.requestHeaders) [value | (name, value) <- request.requestHeaders, name == "X-HMem-Bot-Identity"] (lookup "X-Request-Id" request.requestHeaders)])
  let response
        | request.rawPathInfo == "/api/v1/tasks/aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee" = taskWithWorkspace
        | request.rawPathInfo == "/api/v1/tasks/11111111-2222-3333-4444-555555555555" = object ["id" .= workspaceId, "workspace_id" .= ("99999999-2222-3333-4444-555555555555" :: Text)]
        | otherwise = projectWithWorkspace
  respond $ Wai.responseLBS status200 [("Content-Type", "application/json")] (encode response)

taskWithWorkspace, projectWithWorkspace :: Value
taskWithWorkspace = object ["id" .= observationId, "workspace_id" .= workspaceId]
projectWithWorkspace = object ["id" .= workspaceId, "workspace_id" .= workspaceId]

trustedMcpProvenance :: String
trustedMcpProvenance = "release-gate-mcp-provenance"

localMcpBotToken :: Text
localMcpBotToken = "release-gate-local-mcp-bot"

trustedMcpConfig :: Config.HMemConfig
trustedMcpConfig =
  let cfg = Config.defaultConfig
  in cfg
    { Config.auth = cfg.auth
        { Config.mode = Config.AuthModeLocal
        , Config.enabled = False
        , Config.apiKey = Nothing
        , Config.local = Config.LocalAuthConfig
            { Config.bootstrapEnabled = False
            , Config.allowRemoteBootstrap = False
            , Config.botTokens =
                [ Config.LocalBotTokenConfig
                    { Config.label = "Release gate MCP bot"
                    , Config.token = localMcpBotToken
                    }
                ]
            }
        , Config.mcpProvenanceToken = Just (T.pack trustedMcpProvenance)
        }
    }

-- These cases use the complete WAI application in deployed mode, not a
-- hand-written JSON mock.  The injected principals model the three distinct
-- authorization outcomes while keeping the MCP HTTP proxy as the client
-- under test.
renameProxyErrorCases :: [(Principal, Bool, Text, Text)]
renameProxyErrorCases =
  [ (renameSuperadmin, False, "   ", "[HTTP_400] name must not be empty")
  , (renameUnprivileged, False, "authorized-shape", "[HTTP_403] Workspace edit permission is required.")
  , (renameSuperadmin, True, "authorized-shape", "[HTTP_404] Workspace not found.")
  ]

renameSuperadmin, renameUnprivileged :: Principal
renameSuperadmin = Principal
  { actorType = ActorBot
  , actorId = "rename-proxy-superadmin"
  , actorLabel = "Rename proxy superadmin"
  , authority = PrincipalSyntheticLocalSuperadmin
  }
renameUnprivileged = Principal
  { actorType = ActorBot
  , actorId = "rename-proxy-unprivileged"
  , actorLabel = "Rename proxy unprivileged"
  , authority = PrincipalNoAuthority
  }

missingWorkspaceId :: UUID
missingWorkspaceId = read "00000000-0000-0000-0000-000000000001"

withRenameProxyApp :: Principal -> (UUID -> Manager -> String -> IO a) -> IO a
withRenameProxyApp principal action =
  withTestEnv $ \env -> do
    tracker <- newAccessTracker env.pool 3600
    wsState <- newWSState
    let cfg = Config.defaultConfig
          { Config.auth = Config.defaultConfig.auth
              { Config.mode = Config.AuthModeDeployed
              , Config.enabled = False
              }
          }
        asPrincipal application request respond =
          withPrincipalContext (Just principal) (application request respond)
    app <- mkAppWithChangeStream cfg.changeStream asPrincipal cfg.auth cfg.cors cfg.rateLimit
      env.pool tracker wsState Nothing True
    workspace <- createTestWorkspace env "workspace-rename-mcp-proxy-errors"
    testWithApplication (pure app) $ \port ->
      bracket (newManager defaultManagerSettings { managerIdleConnectionCount = 0 }) closeManager $ \manager ->
        action workspace.id manager ("http://127.0.0.1:" <> show port)

withEnvironment :: String -> String -> IO a -> IO a
withEnvironment name value = bracket acquire restore . const
  where
    acquire = do
      previous <- lookupEnv name
      setEnv name value
      pure previous
    restore Nothing = unsetEnv name
    restore (Just previous) = setEnv name previous

mockApp :: TVar [RequestInfo] -> Wai.Application
mockApp requests request respond = do
  body <- Wai.strictRequestBody request
  atomically $ modifyTVar' requests (<> [RequestInfo request.requestMethod (T.unpack (TE.decodeUtf8 request.rawPathInfo)) request.rawQueryString body (lookup "Authorization" request.requestHeaders) (lookup "X-HMem-Change-Cause" request.requestHeaders) (lookup "X-HMem-MCP-Provenance" request.requestHeaders) (lookup "If-Match" request.requestHeaders) [value | (name, value) <- request.requestHeaders, name == "X-HMem-Bot-Identity"] (lookup "X-Request-Id" request.requestHeaders)])
  case request.requestMethod of
    method | method == methodDelete && "/dependencies/" `T.isInfixOf` TE.decodeUtf8 request.rawPathInfo -> respond $ Wai.responseLBS status200 [("Content-Type", "application/json")] (encode (responseFor request.requestMethod request.rawPathInfo request.rawQueryString body))
    method | method == methodDelete -> respond $ Wai.responseLBS status204 [] ""
    method | method == methodPut && "/embedding" `T.isSuffixOf` TE.decodeUtf8 request.rawPathInfo -> respond $ Wai.responseLBS status204 [] ""
    _ -> respond $ Wai.responseLBS status200 [("Content-Type", "application/json")] (encode (responseFor request.requestMethod request.rawPathInfo request.rawQueryString body))

withErrorMock :: (Manager -> String -> IO a) -> IO a
withErrorMock action = testWithApplication (pure errorApp) $ \port ->
  bracket (newManager defaultManagerSettings { managerIdleConnectionCount = 0 }) closeManager $ \manager ->
    action manager ("http://127.0.0.1:" <> show port)

errorApp :: Wai.Application
errorApp _ respond = respond $ Wai.responseLBS status400 [("Content-Type", "text/plain")] "pgvector extension is required"

withStatusMock :: TVar [RequestInfo] -> (Manager -> String -> IO a) -> IO a
withStatusMock requests action = testWithApplication (pure (statusApp requests)) $ \port ->
  bracket (newManager defaultManagerSettings { managerIdleConnectionCount = 0 }) closeManager $ \manager ->
    action manager ("http://127.0.0.1:" <> show port)

withWorkspaceStructuredStatusMock :: Status -> Text -> (Manager -> String -> IO a) -> IO a
withWorkspaceStructuredStatusMock status message action =
  testWithApplication (pure (workspaceStructuredStatusApp status message)) $ \port ->
    bracket (newManager defaultManagerSettings { managerIdleConnectionCount = 0 }) closeManager $ \manager ->
      action manager ("http://127.0.0.1:" <> show port)

withDependencyCycleMock :: (Manager -> String -> IO a) -> IO a
withDependencyCycleMock action =
  testWithApplication (pure dependencyCycleApp) $ \port ->
    bracket (newManager defaultManagerSettings { managerIdleConnectionCount = 0 }) closeManager $ \manager ->
      action manager ("http://127.0.0.1:" <> show port)

dependencyCycleApp :: Wai.Application
dependencyCycleApp _ respond = respond $ Wai.responseLBS status400 [("Content-Type", "application/json")]
  (encode (object ["error" .= ("dependency_cycle" :: Text), "message" .= ("Task dependency would create a cycle" :: Text)]))

dependencyScopeApp :: Wai.Application
dependencyScopeApp _ respond = respond $ Wai.responseLBS status409 [("Content-Type", "application/json")]
  (encode (object ["error" .= ("lifecycle_conflict" :: Text), "code" .= ("TASK_DEPENDENCY_SCOPE_MISMATCH" :: Text), "message" .= ("Task prerequisites must be roots or siblings under the same parent." :: Text)]))

withLifecycleConflictMock :: (Manager -> String -> IO a) -> IO a
withLifecycleConflictMock action =
  testWithApplication (pure lifecycleConflictApp) $ \port ->
    bracket (newManager defaultManagerSettings { managerIdleConnectionCount = 0 }) closeManager $ \manager ->
      action manager ("http://127.0.0.1:" <> show port)

lifecycleConflictApp :: Wai.Application
lifecycleConflictApp request respond | request.requestMethod == methodPut = respond $ Wai.responseLBS status409 [("Content-Type", "application/json")]
  (encode (object
    [ "error" .= ("lifecycle_conflict" :: Text)
    , "code" .= ("TASK_START_BLOCKED_BY_SUBTASKS" :: Text)
    , "message" .= ("Cannot start a task with incomplete subtasks." :: Text)
    , "detail" .= object ["blocker_count" .= (1 :: Int), "blocker_ids" .= [workspaceId]]
    , "hint" .= ("Close incomplete subtasks before starting the parent task." :: Text)
    ]))
lifecycleConflictApp _ respond = respond $ Wai.responseLBS status409 [("Content-Type", "application/json")]
  (encode (object
    [ "error" .= ("lifecycle_conflict" :: Text)
    , "code" .= ("TASK_DEPENDENCY_CROSS_PROJECT" :: Text)
    , "message" .= ("Cannot move tasks because dependency endpoints would span projects." :: Text)
    , "detail" .= object ["task_ids" .= [observationId]]
    , "hint" .= ("Move the complete dependency-connected component together." :: Text)
    ]))

workspaceStructuredStatusApp :: Status -> Text -> Wai.Application
workspaceStructuredStatusApp status message _ respond =
  respond $ Wai.responseLBS status [("Content-Type", "application/json")] (encode (object ["error" .= ("workspace_error" :: Text), "message" .= message]))

statusApp :: TVar [RequestInfo] -> Wai.Application
statusApp requests request respond = do
  body <- Wai.strictRequestBody request
  atomically $ modifyTVar' requests (<> [RequestInfo request.requestMethod (T.unpack (TE.decodeUtf8 request.rawPathInfo)) request.rawQueryString body (lookup "Authorization" request.requestHeaders) (lookup "X-HMem-Change-Cause" request.requestHeaders) (lookup "X-HMem-MCP-Provenance" request.requestHeaders) (lookup "If-Match" request.requestHeaders) [value | (name, value) <- request.requestHeaders, name == "X-HMem-Bot-Identity"] (lookup "X-Request-Id" request.requestHeaders)])
  let status
        | request.rawPathInfo == "/api/v1/observations/match" = status500
        | request.rawPathInfo == "/api/v1/observations/similar" = status400
        | "/embedding" `T.isSuffixOf` TE.decodeUtf8 request.rawPathInfo = status409
        | request.requestMethod == methodDelete = status404
        | request.rawPathInfo == "/api/v1/observations" = status403
        | otherwise = status401
      message = "status " <> T.pack (show (statusCode status))
  respond $ Wai.responseLBS status [("Content-Type", "text/plain")] (BL.fromStrict (TE.encodeUtf8 message))

responseFor :: ByteString -> ByteString -> ByteString -> BL.ByteString -> Value
responseFor method path rawQuery body
  | path == "/api/v1/observations/discovery" = discoveryFixture "fts_only" (case decode body >>= jsonField "view" of Just (String view) -> view; _ -> "list") True
  | path == "/api/v1/search" = object
      [ "observations" .= (if requested "observation" then [observation] else [])
      , "projects" .= (if requested "project" then [project] else [])
      , "tasks" .= (if requested "task" then [task] else [])
      , "has_more" .= object (["observations" .= True | requested "observation"] <> ["projects" .= True | requested "project"] <> ["tasks" .= False | requested "task"])
      , "next_offset" .= object (["observations" .= (searchOffset + 1) | requested "observation"] <> ["projects" .= (searchOffset + 1) | requested "project"])
      ]
  | path == "/api/v1/observations/match" && "\"offset\":2" `T.isInfixOf` TE.decodeUtf8 (BL.toStrict body) = object ["items" .= ([] :: [Value]), "has_more" .= False]
  | path == "/api/v1/observations/match" = object ["items" .= [match, match], "has_more" .= True]
  | path == "/api/v1/observations/similar" = toJSON [object ["observation" .= observation, "similarity" .= (0.75 :: Double)]]
  | method == methodPost && path == "/api/v1/tasks/aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee/dependencies" = dependencyMutation "add"
  | method == methodPost && path == "/api/v1/tasks/batch-move" = object ["affected" .= (2 :: Int)]
  | method == methodDelete && path == "/api/v1/tasks/aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee/dependencies/11111111-2222-3333-4444-555555555555" = dependencyMutation "remove"
  | method == methodPost && path == "/api/v1/observations" = observation
  | method == methodPost && path == "/api/v1/projects" = project
  | method == methodPost && path == "/api/v1/projects/spec" = object ["project_id" .= observationId, "task_ids" .= take (taskCount body) [observationId, workspaceId, "99999999-2222-3333-4444-555555555555" :: Text]]
  | method == methodPost && path == "/api/v1/tasks" = task
  | path == "/api/v1/projects/aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee/overview" = object ["project" .= project, "tasks" .= [task], "subprojects" .= [project], "readiness_rollup" .= object ["completion_ready" .= True]]
  | path == "/api/v1/projects/bbbbbbbb-cccc-dddd-eeee-ffffffffffff/next-tasks" = toJSON [object ["task" .= object ["id" .= ("cccccccc-dddd-eeee-ffff-aaaaaaaaaaaa" :: Text), "parent_id" .= observationId, "project_id" .= ("bbbbbbbb-cccc-dddd-eeee-ffffffffffff" :: Text), "title" .= ("Ready subtask" :: Text), "description" .= T.replicate 10000 "s", "status" .= ("todo" :: Text), "priority" .= (9 :: Int)], "completion_gated" .= False, "open_descendant_count" .= (0 :: Int), "dependency_blocked" .= False, "open_dependency_count" .= (0 :: Int)]]
  | path == "/api/v1/projects/aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee/next-tasks" = toJSON [object ["task" .= task, "completion_gated" .= True, "open_descendant_count" .= (2 :: Int), "dependency_blocked" .= False, "open_dependency_count" .= (0 :: Int)]]
  | path == "/api/v1/tasks/aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee/overview" = object ["task" .= task, "dependencies" .= [object ["id" .= workspaceId, "name" .= ("Prerequisite" :: Text)]], "readiness_rollup" .= object ["completion_ready" .= True]]
  | method == methodPut && path == "/api/v1/workspaces/11111111-2222-3333-4444-555555555555" = object ["id" .= workspaceId, "name" .= ("Renamed" :: Text), "workspace_type" .= ("repository" :: Text)]
  | path == "/api/v1/observations" && "offset=2" `T.isInfixOf` TE.decodeUtf8 rawQuery = object ["items" .= [observation], "has_more" .= False]
  | path == "/api/v1/observations" = object ["items" .= [observation, observation], "has_more" .= True]
  | path == "/api/v1/observations/aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee/history" = object ["items" .= ([] :: [Value]), "has_more" .= False]
  | path == "/api/v1/observations/aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee" = observation
  | path == "/api/v1/tasks/aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee" = task
  | path == "/api/v1/tasks/11111111-2222-3333-4444-555555555555" = object ["id" .= workspaceId, "workspace_id" .= workspaceId, "title" .= ("Task" :: Text), "status" .= ("done" :: Text), "priority" .= (5 :: Int)]
  | path == "/api/v1/projects/aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee" = project
  | path == "/api/v1/projects/11111111-2222-3333-4444-555555555555" = object ["id" .= workspaceId, "workspace_id" .= workspaceId, "name" .= ("Project" :: Text), "status" .= ("archived" :: Text), "priority" .= (5 :: Int)]
  | otherwise = object []
  where
    observation = object ["id" .= observationId, "workspace_id" .= workspaceId, "subjects" .= observationSubjects, "subject_kind" .= ("file" :: Text), "subject" .= ("src/HMem/Types.hs" :: Text), "git_sha" .= gitSha, "content" .= ("complete observation content" :: Text), "content_preview" .= ("complete observation content" :: Text), "content_version" .= observationId, "latest_sequence" .= (1 :: Int), "current_provenance" .= Null]
    match = object
      [ "observation" .= observation
      , "path_matches" .=
          [ object ["path" .= ("my/src/proj/Main.java" :: Text), "matched_subjects" .= [ObservationSubject SubjectGlob "my/src/proj/**/*.java"]]
          , object ["path" .= ("src/HMem/Types.hs" :: Text), "matched_subjects" .= [ObservationSubject SubjectFile "src/HMem/Types.hs"]]
          ]
      , "matched_paths" .= (["my/src/proj/Main.java", "src/HMem/Types.hs"] :: [Text])
      , "matched_subjects" .= observationSubjects
      ]
    task = taskWithLongDescription
    project = projectWithLongDescription
    requested kind = case decode body >>= jsonField "entity_types" of
      Just (Array types) -> String kind `elem` toList types
      _ -> True
    searchOffset = case decode body >>= jsonField "offset" of
      Just (Number offset) -> round offset :: Int
      _ -> 0
    taskCount payload = case decode payload >>= jsonField "tasks" of
      Just (Array values) -> length values
      _ -> 0
    dependencyMutation :: Text -> Value
    dependencyMutation action = object ["action" .= action, "task_id" .= observationId, "depends_on_id" .= workspaceId, "affected_tasks" .= [dependencyChange]]

projectWithLongDescription, taskWithLongDescription, dependencyChange :: Value
projectWithLongDescription = object ["id" .= observationId, "workspace_id" .= workspaceId, "name" .= ("Project" :: Text), "description" .= T.replicate 10000 "p", "status" .= ("archived" :: Text), "priority" .= (5 :: Int), "parent_id" .= Null, "pause_effects" .= object ["project_count" .= (23 :: Int), "task_count" .= (23 :: Int), "projects" .= ([] :: [Value]), "tasks" .= [dependencyChange], "has_more_projects" .= True, "has_more_tasks" .= True, "remaining_own_pause" .= False, "remaining_ancestor_pause" .= True]]
-- Synthetic old upstream fields verify that MCP projections drop retired data.
taskWithLongDescription = object ["due_at" .= ("2030-01-01T00:00:00Z" :: Text), "id" .= observationId, "workspace_id" .= workspaceId, "title" .= ("Task" :: Text), "description" .= T.replicate 10000 "t", "status" .= ("done" :: Text), "priority" .= (5 :: Int), "project_id" .= workspaceId, "parent_id" .= Null, "dependency_effects" .= [dependencyChange]]
dependencyChange = object ["task" .= object ["due_at" .= Null, "id" .= observationId, "title" .= ("Task" :: Text), "description" .= T.replicate 10000 "t", "status" .= ("blocked" :: Text), "priority" .= (5 :: Int), "project_id" .= workspaceId, "parent_id" .= Null], "previous_status" .= ("todo" :: Text), "current_status" .= ("blocked" :: Text), "previous_auto_blocked" .= False, "auto_blocked" .= True, "previous_open_dependency_count" .= (0 :: Int), "open_dependency_count" .= (1 :: Int), "previous_open_subtask_count" .= (0 :: Int), "open_subtask_count" .= (0 :: Int), "previous_project_paused" .= False, "project_paused" .= True, "reason" .= ("blocked_by_paused_project_and_open_dependencies" :: Text)]

testIdentity :: BotIdentity
testIdentity = either (error . T.unpack) id (mkBotIdentity "Developer" "codex:gpt-6.1-sol:high")
