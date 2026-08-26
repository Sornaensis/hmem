module HMem.MCP.ToolsSpec (spec) where

import Control.Concurrent.STM
import Control.Monad (filterM)
import Data.Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BL
import Data.Foldable (toList)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Network.HTTP.Client (Manager, defaultManagerSettings, newManager)
import Network.HTTP.Types (methodDelete, methodGet, methodPost, methodPut, status200, status204, status400, status401, status403, status404, status409, statusCode)
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp (testWithApplication)
import System.Directory (doesFileExist)
import Test.Hspec

import HMem.MCP.Tools
import HMem.Types (CreateObservation(..), UpdateObservation(..))

spec :: Spec
spec = do
  describe "Observation MCP registry" $ do
    it "advertises and parses every Observation capability, with no removed memory, link, or context tools" $ do
      toolNames `shouldContain` ["observation_create", "observation_get", "observation_update", "observation_list", "observation_delete", "observation_set_embedding", "observation_similar"]
      mapM_ (\(name, arguments) -> parseToolCall name arguments `shouldSatisfy` isRight)
        [ ("observation_create", observationArguments)
        , ("observation_get", object ["observation_id" .= observationId])
        , ("observation_update", object ["observation_id" .= observationId, "content" .= ("replacement" :: Text)])
        , ("observation_list", observationListArguments)
        , ("observation_delete", object ["observation_id" .= observationId])
        , ("observation_set_embedding", object ["observation_id" .= observationId, "embedding" .= embedding])
        , ("observation_similar", similarArguments)
        ]
      toolNames `shouldNotSatisfy` any (`elem` ["memory_create", "memory_get", "memory_update", "memory_link", "link_memory", "context_get"])
      mapM_ (\name -> parseToolCall name (object []) `shouldSatisfy` isUnknown name)
        ["memory_create", "memory_get", "memory_update", "memory_link", "link_memory", "context_get"]

    it "advertises only provenance fields on observation tools and search" $ do
      all (`elem` schemaProperties "observation_create") ["subject_kind", "subject", "git_sha", "content"] `shouldBe` True
      length (schemaProperties "observation_create") `shouldBe` 4
      schemaRequired "observation_create" `shouldBe` ["subject_kind", "subject", "git_sha", "content"]
      all (`elem` schemaProperties "observation_update") ["observation_id", "content"] `shouldBe` True
      length (schemaProperties "observation_update") `shouldBe` 2
      all (`elem` schemaProperties "search") ["subject_kind", "subject", "git_sha"] `shouldBe` True
      schemaProperties "search" `shouldContain` ["offset"]
      all (`elem` schemaProperties "observation_list") ["subject_kind", "subject", "git_sha", "query", "limit", "offset"] `shouldBe` True
      schemaProperties "observation_list" `shouldNotContain` ["workspace_id"]
      all (`elem` schemaProperties "observation_similar") ["embedding", "min_similarity", "limit", "offset"] `shouldBe` True
      schemaProperties "observation_similar" `shouldNotContain` ["workspace_id"]
      schemaProperties "search" `shouldNotContain` ["memory_type", "tags", "pinned_only", "min_importance"]
      (schemaProperty "set_workspace" "workspace_id" >>= jsonField "anyOf")
        `shouldSatisfy` maybe False nullableWorkspaceTypes
      toolDescription "task_finish" `shouldSatisfy` maybe False ("does not create an observation" `T.isInfixOf`)
      toolDescription "project_archive" `shouldSatisfy` maybe False (not . ("summary" `T.isInfixOf`))

  describe "Observation compact response shaping" $ do
    it "derives bounded content_preview from a create/get response when the server supplies full content" $ do
      let summary = compactObservationSummary (object ["id" .= observationId, "subject_kind" .= ("file" :: Text), "subject" .= ("src/HMem/Types.hs" :: Text), "git_sha" .= gitSha, "content" .= (T.replicate 600 "x")])
      jsonField "content_preview" summary `shouldBe` Just (String (T.replicate 500 "x"))
      summary `shouldSatisfy` hasFields ["id", "subject_kind", "subject", "git_sha", "content_preview"]

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

  describe "Observation parsing and validation" $ do
    it "parses provenance-bound creates and content-only updates" $ do
      case parseToolCall "observation_create" observationArguments of
        Right (ObservationCreate (CreateObservation _ _ subject sha _)) -> do
          subject `shouldBe` "src/HMem/Types.hs"
          sha `shouldBe` gitSha
        result -> expectationFailure (show result)
      case parseToolCall "observation_update" (object ["workspace_id" .= workspaceId, "observation_id" .= observationId, "content" .= ("replacement" :: Text)]) of
        Right (ObservationUpdate _ (UpdateObservation content)) -> content `shouldBe` "replacement"
        result -> expectationFailure (show result)

    it "rejects mutable provenance fields on update and malformed provenance on create" $ do
      parseToolCall "observation_update" (object ["observation_id" .= observationId, "content" .= ("replacement" :: Text), "git_sha" .= gitSha]) `shouldSatisfy` isLeft
      case parseToolCall "observation_create" (object ["workspace_id" .= workspaceId, "subject_kind" .= ("file" :: Text), "subject" .= ("/absolute" :: Text), "git_sha" .= ("bad" :: Text), "content" .= ("content" :: Text)]) of
        Right parsed -> validateToolCall parsed `shouldSatisfy` isLeft
        result -> expectationFailure (show result)

    it "accepts observation search provenance filters and rejects legacy entity types" $ do
      parseToolCall "search" (object ["workspace_id" .= workspaceId, "entity_types" .= (["observation"] :: [Text]), "subject_kind" .= ("file" :: Text), "subject" .= ("src/HMem/Types.hs" :: Text), "git_sha" .= gitSha]) `shouldSatisfy` isRight
      parseToolCall "search" (object ["workspace_id" .= workspaceId, "entity_types" .= (["memory"] :: [Text])]) `shouldSatisfy` isLeft

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
        detail `shouldSatisfy` not . contains "workspace_id"
        update <- call manager base "observation_update" (object ["observation_id" .= observationId, "content" .= ("replacement" :: Text)])
        jsonField "action" update `shouldBe` Just (String "updated")
      observed <- readTVarIO requests
      map (.requestPath) observed `shouldBe` ["/api/v1/observations", "/api/v1/observations/aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee", "/api/v1/observations/aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee"]
      map (.requestMethod) observed `shouldBe` [methodPost, methodGet, methodPut]

    it "passes exact provenance filters through unified search and compacts observations" $ do
      requests <- newTVarIO []
      withMock requests $ \manager base -> do
        result <- call manager base "search" (object ["workspace_id" .= workspaceId, "entity_types" .= (["observation"] :: [Text]), "subject_kind" .= ("file" :: Text), "subject" .= ("src/HMem/Types.hs" :: Text), "git_sha" .= gitSha])
        jsonField "observations" result `shouldSatisfy` maybe False (arrayFirst (hasFields ["id", "subject_kind", "subject", "git_sha", "content_preview"]))
        result `shouldSatisfy` not . contains "linked_memories"
      [RequestInfo _ _ _ body _] <- readTVarIO requests
      decode body `shouldSatisfy` maybe False (\value -> hasFields ["subject_kind", "subject", "git_sha"] value)

    it "forwards list filters and gives an exact next offset only when the server reports another page" $ do
      requests <- newTVarIO []
      withMock requests $ \manager base -> do
        first <- call manager base "observation_list" observationListArguments
        jsonField "has_more" first `shouldBe` Just (Bool True)
        jsonField "next_offset" first `shouldBe` Just (Number 2)
        jsonField "items" first `shouldSatisfy` maybe False (arrayFirst (hasFields ["id", "subject_kind", "subject", "git_sha", "content_preview"]))
        final <- call manager base "observation_list" (object ["workspace_id" .= workspaceId, "limit" .= (2 :: Int), "offset" .= (2 :: Int)])
        jsonField "has_more" final `shouldBe` Just (Bool False)
        jsonField "next_offset" final `shouldBe` Nothing
      [firstRequest, finalRequest] <- readTVarIO requests
      firstRequest.requestPath `shouldBe` "/api/v1/observations"
      firstRequest.requestQuery `shouldBe` "?workspace_id=11111111-2222-3333-4444-555555555555&subject_kind=file&subject=src%2FHMem%2FTypes.hs&git_sha=0123456789abcdef0123456789abcdef01234567&query=types&limit=2&offset=0"
      finalRequest.requestQuery `shouldBe` "?workspace_id=11111111-2222-3333-4444-555555555555&limit=2&offset=2"

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
        result <- handleToolCall manager base Nothing (object ["name" .= ("observation_set_embedding" :: Text), "arguments" .= object ["observation_id" .= observationId, "embedding" .= embedding]])
        result `shouldSatisfy` contains "[HTTP_400] pgvector extension is required"

    it "propagates HTTP status failures and forwards Bearer authorization on Observation reads" $ do
      requests <- newTVarIO []
      withStatusMock requests $ \manager base -> do
        let run :: Text -> Value -> Text -> IO ()
            run name arguments expected = do
              response <- handleToolCall manager base (Just "test-token") (object ["name" .= name, "arguments" .= arguments])
              response `shouldSatisfy` contains expected
        run "observation_get" (object ["observation_id" .= observationId]) "[HTTP_401] status 401"
        run "observation_list" observationListArguments "[HTTP_403] status 403"
        run "observation_delete" (object ["observation_id" .= observationId]) "[HTTP_404] status 404"
        run "observation_set_embedding" (object ["observation_id" .= observationId, "embedding" .= embedding]) "[HTTP_409] status 409"
        run "observation_similar" similarArguments "[HTTP_400] status 400"
      observed <- readTVarIO requests
      map (.authorization) observed `shouldBe` replicate 5 (Just "Bearer test-token")

    it "makes task_finish and project_archive single status mutations without observation side effects" $ do
      requests <- newTVarIO []
      withMock requests $ \manager base -> do
        finished <- call manager base "task_finish" (object ["task_id" .= observationId, "status" .= ("done" :: Text)])
        jsonField "action" finished `shouldBe` Just (String "finished")
        archived <- call manager base "project_archive" (object ["project_id" .= observationId])
        jsonField "action" archived `shouldBe` Just (String "archived")
      observed <- readTVarIO requests
      map (.requestPath) observed `shouldBe` ["/api/v1/tasks/aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee", "/api/v1/projects/aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee"]
      map (.requestMethod) observed `shouldBe` [methodPut, methodPut]
      mapM_ (\request -> decode request.requestBody `shouldBe` Just (object ["status" .= if request.requestPath == "/api/v1/tasks/aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee" then ("done" :: Text) else "archived"])) observed

workspaceId, observationId, gitSha :: Text
workspaceId = "11111111-2222-3333-4444-555555555555"
observationId = "aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee"
gitSha = "0123456789abcdef0123456789abcdef01234567"

observationArguments :: Value
observationArguments = object ["workspace_id" .= workspaceId, "subject_kind" .= ("file" :: Text), "subject" .= ("src/HMem/Types.hs" :: Text), "git_sha" .= gitSha, "content" .= ("complete observation content" :: Text)]

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

toolDescription :: Text -> Maybe Text
toolDescription name = case [description | Object tool <- toolDefinitions, KM.lookup "name" tool == Just (String name), Just (String description) <- [KM.lookup "description" tool]] of description : _ -> Just description; [] -> Nothing

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

nullableWorkspaceTypes :: Value -> Bool
nullableWorkspaceTypes (Array values) = map (jsonField "type") (toList values) == [Just (String "string"), Just (String "null")]
nullableWorkspaceTypes _ = False

isUnknown :: Text -> Either String ToolCall -> Bool
isUnknown name = \case Left message -> ("Unknown tool: " <> T.unpack name) == message; Right _ -> False
isLeft :: Either a b -> Bool
isLeft = \case Left _ -> True; Right _ -> False
isRight :: Either a b -> Bool
isRight = not . isLeft

jsonField :: Text -> Value -> Maybe Value
jsonField key (Object objectValue) = KM.lookup (Key.fromText key) objectValue
jsonField _ _ = Nothing
hasFields :: [Text] -> Value -> Bool
hasFields keys value = all (\key -> jsonField key value /= Nothing) keys
contains :: Text -> Value -> Bool
contains needle = T.isInfixOf needle . TE.decodeUtf8 . BL.toStrict . encode
arrayFirst :: (Value -> Bool) -> Value -> Bool
arrayFirst predicate (Array values) = case toList values of value : _ -> predicate value; [] -> False
arrayFirst _ _ = False

call :: Manager -> String -> Text -> Value -> IO Value
call manager base = callWithKey manager base Nothing

callWithKey :: Manager -> String -> Maybe Text -> Text -> Value -> IO Value
callWithKey manager base apiKey name arguments = do
  response <- handleToolCall manager base apiKey (object ["name" .= name, "arguments" .= arguments])
  case jsonField "content" response >>= arrayText >>= (either (const Nothing) Just . eitherDecodeStrict' . TE.encodeUtf8) of
    Just value -> pure value
    Nothing -> expectationFailure ("Expected MCP JSON response, got " <> show response) >> pure Null
  where
    arrayText (Array values) = case toList values of
      Object first : _ -> case KM.lookup "text" first of Just (String text) -> Just text; _ -> Nothing
      _ -> Nothing
    arrayText _ = Nothing

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
  }

withMock :: TVar [RequestInfo] -> (Manager -> String -> IO a) -> IO a
withMock requests action = testWithApplication (pure (mockApp requests)) $ \port -> do
  manager <- newManager defaultManagerSettings
  action manager ("http://127.0.0.1:" <> show port)

mockApp :: TVar [RequestInfo] -> Wai.Application
mockApp requests request respond = do
  body <- Wai.strictRequestBody request
  atomically $ modifyTVar' requests (<> [RequestInfo request.requestMethod (T.unpack (TE.decodeUtf8 request.rawPathInfo)) request.rawQueryString body (lookup "Authorization" request.requestHeaders)])
  case request.requestMethod of
    method | method == methodDelete -> respond $ Wai.responseLBS status204 [] ""
    method | method == methodPut && "/embedding" `T.isSuffixOf` TE.decodeUtf8 request.rawPathInfo -> respond $ Wai.responseLBS status204 [] ""
    _ -> respond $ Wai.responseLBS status200 [("Content-Type", "application/json")] (encode (responseFor request.requestMethod request.rawPathInfo request.rawQueryString))

withErrorMock :: (Manager -> String -> IO a) -> IO a
withErrorMock action = testWithApplication (pure errorApp) $ \port -> do
  manager <- newManager defaultManagerSettings
  action manager ("http://127.0.0.1:" <> show port)

errorApp :: Wai.Application
errorApp _ respond = respond $ Wai.responseLBS status400 [("Content-Type", "text/plain")] "pgvector extension is required"

withStatusMock :: TVar [RequestInfo] -> (Manager -> String -> IO a) -> IO a
withStatusMock requests action = testWithApplication (pure (statusApp requests)) $ \port -> do
  manager <- newManager defaultManagerSettings
  action manager ("http://127.0.0.1:" <> show port)

statusApp :: TVar [RequestInfo] -> Wai.Application
statusApp requests request respond = do
  body <- Wai.strictRequestBody request
  atomically $ modifyTVar' requests (<> [RequestInfo request.requestMethod (T.unpack (TE.decodeUtf8 request.rawPathInfo)) request.rawQueryString body (lookup "Authorization" request.requestHeaders)])
  let status
        | request.rawPathInfo == "/api/v1/observations/similar" = status400
        | "/embedding" `T.isSuffixOf` TE.decodeUtf8 request.rawPathInfo = status409
        | request.requestMethod == methodDelete = status404
        | request.rawPathInfo == "/api/v1/observations" = status403
        | otherwise = status401
      message = "status " <> T.pack (show (statusCode status))
  respond $ Wai.responseLBS status [("Content-Type", "text/plain")] (BL.fromStrict (TE.encodeUtf8 message))

responseFor :: ByteString -> ByteString -> ByteString -> Value
responseFor method path rawQuery
  | path == "/api/v1/search" = object ["observations" .= [observation], "projects" .= ([] :: [Value]), "tasks" .= ([] :: [Value])]
  | path == "/api/v1/observations/similar" = toJSON [object ["observation" .= observation, "similarity" .= (0.75 :: Double)]]
  | method == methodPost && path == "/api/v1/observations" = observation
  | method == methodPost && path == "/api/v1/projects" = project
  | method == methodPost && path == "/api/v1/tasks" = task
  | path == "/api/v1/observations" && "offset=2" `T.isInfixOf` TE.decodeUtf8 rawQuery = object ["items" .= [observation], "has_more" .= False]
  | path == "/api/v1/observations" = object ["items" .= [observation, observation], "has_more" .= True]
  | path == "/api/v1/observations/aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee" = observation
  | path == "/api/v1/tasks/aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee" = task
  | path == "/api/v1/projects/aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee" = project
  | otherwise = object []
  where
    observation = object ["id" .= observationId, "workspace_id" .= workspaceId, "subject_kind" .= ("file" :: Text), "subject" .= ("src/HMem/Types.hs" :: Text), "git_sha" .= gitSha, "content" .= ("complete observation content" :: Text), "content_preview" .= ("complete observation content" :: Text)]
    task = object ["id" .= observationId, "workspace_id" .= workspaceId, "title" .= ("Task" :: Text), "status" .= ("done" :: Text), "priority" .= (5 :: Int)]
    project = object ["id" .= observationId, "workspace_id" .= workspaceId, "name" .= ("Project" :: Text), "status" .= ("archived" :: Text), "priority" .= (5 :: Int)]
