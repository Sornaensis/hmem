module HMem.MCP.ServerSpec (spec) where

import Control.Concurrent (ThreadId, forkIO, forkIOWithUnmask, killThread, threadDelay, throwTo)
import Control.Concurrent.MVar (MVar, modifyMVar, newEmptyMVar, newMVar, putMVar, readMVar, tryPutMVar, tryReadMVar)
import Control.Exception (AsyncException(..), SomeException, bracket, finally, fromException, throwIO, try)
import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVarIO)
import Data.Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.Text (Text)
import Data.Text qualified as T
import Data.UUID (UUID)
import Network.HTTP.Client (Manager, closeManager, defaultManagerSettings, newManager)
import Network.HTTP.Types (hContentType, status200)
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp (testWithApplication)
import System.Directory (removeFile)
import System.IO (Handle, SeekMode(..), hClose, hSeek, openTempFile)
import GHC.Conc (ThreadStatus(..), threadStatus)
import Test.Hspec

import HMem.MCP.Server (encodeStdioResponse, handleStdioLine, injectWorkspaceContext, runMCPServerWithHandles, runMCPServerWithHandlesObserved, runMCPServerWithHandlesObservedWithFork, sendResponseToHandle)
import HMem.MCP.Tools (toolDefinitions)

spec :: Spec
spec = do
  describe "stdio JSON-RPC line handling" $ do
    it "returns parse errors for invalid JSON lines" $
      withStdioState $ \mgr initialized wsContext -> do
        mResponse <- handleStdioLine mgr unusedServerUrl Nothing initialized wsContext "not json"
        (mResponse >>= jsonField "id") `shouldBe` Just Null
        (mResponse >>= errorField "code") `shouldBe` Just (Number (-32700))
        (mResponse >>= errorField "message") `shouldBe` Just (String "Parse error")

    it "returns invalid-request errors for valid JSON without a method and preserves id" $
      withStdioState $ \mgr initialized wsContext -> do
        mResponse <- handleStdioLine mgr unusedServerUrl Nothing initialized wsContext $ jsonLine $ object
          [ "jsonrpc" .= ("2.0" :: Text)
          , "id" .= ("missing-method" :: Text)
          ]
        (mResponse >>= jsonField "id") `shouldBe` Just (String "missing-method")
        (mResponse >>= errorField "code") `shouldBe` Just (Number (-32600))
        (mResponse >>= errorField "message") `shouldBe` Just (String "Invalid Request: missing required 'method' field")

    it "does not emit responses for blank lines or notifications" $
      withStdioState $ \mgr initialized wsContext -> do
        blank <- handleStdioLine mgr unusedServerUrl Nothing initialized wsContext BS8.empty
        blank `shouldBe` Nothing
        notification <- handleStdioLine mgr unusedServerUrl Nothing initialized wsContext $ jsonLine $ object
          [ "jsonrpc" .= ("2.0" :: Text)
          , "method" .= ("notifications/progress" :: Text)
          ]
        notification `shouldBe` Nothing

    it "enforces initialization before ordinary requests" $
      withStdioState $ \mgr initialized wsContext -> do
        mResponse <- handleStdioLine mgr unusedServerUrl Nothing initialized wsContext $ jsonLine $ object
          [ "jsonrpc" .= ("2.0" :: Text)
          , "id" .= ("tools-before-init" :: Text)
          , "method" .= ("tools/list" :: Text)
          ]
        (mResponse >>= jsonField "id") `shouldBe` Just (String "tools-before-init")
        (mResponse >>= errorField "code") `shouldBe` Just (Number (-32002))
        (mResponse >>= errorField "message") `shouldBe` Just (String "Server not initialized")

    it "decodes initialized stdio request lines and dispatches JSON-RPC methods" $
      withStdioState $ \mgr initialized wsContext -> do
        initResponse <- handleStdioLine mgr unusedServerUrl Nothing initialized wsContext $ jsonLine $ object
          [ "jsonrpc" .= ("2.0" :: Text)
          , "id" .= ("init" :: Text)
          , "method" .= ("initialize" :: Text)
          ]
        (initResponse >>= jsonField "id") `shouldBe` Just (String "init")
        (initResponse >>= jsonField "result" >>= jsonField "serverInfo" >>= jsonField "name")
          `shouldBe` Just (String "hmem-mcp")

        toolsResponse <- handleStdioLine mgr unusedServerUrl Nothing initialized wsContext $ jsonLine $ object
          [ "jsonrpc" .= ("2.0" :: Text)
          , "id" .= ("tools" :: Text)
          , "method" .= ("tools/list" :: Text)
          ]
        (toolsResponse >>= jsonField "id") `shouldBe` Just (String "tools")
        (toolsResponse >>= jsonField "result" >>= jsonField "tools")
          `shouldBe` Just (toJSON toolDefinitions)

    it "writes newline-delimited JSON responses to handles" $ do
      let response = object
            [ "jsonrpc" .= ("2.0" :: Text)
            , "id" .= ("handle-test" :: Text)
            , "result" .= object ["ok" .= True]
            ]
      bytes <- withTempResponseFile $ \handle -> do
        lock <- newMVar ()
        sendResponseToHandle handle lock response
        hSeek handle AbsoluteSeek 0
        strictHandleContents handle
      bytes `shouldBe` encodeStdioResponse response
      BL8.last bytes `shouldBe` '\n'

    it "runs a finite stdin-to-stdout loop over explicit handles" $
      withTempResponseFile $ \input ->
      withTempResponseFile $ \output ->
      withTempResponseFile $ \errHandle -> do
        let request = object
              [ "jsonrpc" .= ("2.0" :: Text)
              , "id" .= ("init-loop" :: Text)
              , "method" .= ("initialize" :: Text)
              ]
        BL.hPut input ("\n" <> encode request <> "\n")
        hSeek input AbsoluteSeek 0
        mgr <- newManager defaultManagerSettings
        runMCPServerWithHandles 1 4 input output errHandle mgr unusedServerUrl Nothing
        hSeek output AbsoluteSeek 0
        bytes <- strictHandleContents output
        case BL8.lines bytes of
          [line] -> case decode line of
            Just response -> do
              jsonField "id" response `shouldBe` Just (String "init-loop")
              (jsonField "result" response >>= jsonField "serverInfo" >>= jsonField "name")
                `shouldBe` Just (String "hmem-mcp")
            Nothing -> expectationFailure $ "Expected JSON-RPC response line, got: " <> show bytes
          linesOut -> expectationFailure $ "Expected one response line, got: " <> show linesOut

  describe "workspace context ordering" $ do
    it "sets, gets, clears, and rejects invalid values without mutating direct session state" $
      withStdioState $ \mgr initialized wsContext -> do
        atomically $ modifyTVar' initialized (const True)
        setResponse <- workspaceCall mgr initialized wsContext "set" (Just (String workspaceA))
        responseContains workspaceA setResponse `shouldBe` True
        getResponse <- workspaceCall mgr initialized wsContext "get" Nothing
        responseContains workspaceA getResponse `shouldBe` True
        invalidResponse <- workspaceCall mgr initialized wsContext "set" (Just (Number 5))
        responseContains "invalid workspace_id format" invalidResponse `shouldBe` True
        unchanged <- workspaceCall mgr initialized wsContext "get" Nothing
        responseContains workspaceA unchanged `shouldBe` True
        cleared <- workspaceCall mgr initialized wsContext "set" Nothing
        responseContains "cleared" cleared `shouldBe` True
        finalGet <- workspaceCall mgr initialized wsContext "get" Nothing
        responseContains workspaceA finalGet `shouldBe` False

    it "keeps workspace sessions isolated and preserves explicit workspace_id values" $
      withStdioState $ \mgr initialized firstContext -> do
        atomically $ modifyTVar' initialized (const True)
        secondInitialized <- newTVarIO True
        secondContext <- newTVarIO Nothing
        _ <- workspaceCall mgr initialized firstContext "set" (Just (String workspaceA))
        _ <- workspaceCall mgr secondInitialized secondContext "set" (Just (String workspaceB))
        firstGet <- workspaceCall mgr initialized firstContext "get" Nothing
        secondGet <- workspaceCall mgr secondInitialized secondContext "get" Nothing
        responseContains workspaceA firstGet `shouldBe` True
        responseContains workspaceB firstGet `shouldBe` False
        responseContains workspaceB secondGet `shouldBe` True
        responseContains workspaceA secondGet `shouldBe` False

        injected <- injectWorkspaceContext firstContext $ object
          [ "name" .= ("project_create" :: Text)
          , "arguments" .= object ["name" .= ("implicit" :: Text)]
          ]
        injected `shouldSatisfy` valueContains workspaceA
        explicitNull <- injectWorkspaceContext firstContext $ object
          [ "name" .= ("project_create" :: Text)
          , "arguments" .= object ["workspace_id" .= Null, "name" .= ("explicit null" :: Text)]
          ]
        explicitNull `shouldSatisfy` valueContains "explicit null"
        explicitNull `shouldSatisfy` not . valueContains workspaceA
        explicitInvalid <- injectWorkspaceContext firstContext $ object
          [ "name" .= ("project_create" :: Text)
          , "arguments" .= object ["workspace_id" .= (5 :: Int), "name" .= ("invalid" :: Text)]
          ]
        explicitInvalid `shouldSatisfy` valueContains ("5" :: Text)
        explicitInvalid `shouldSatisfy` not . valueContains workspaceA

    it "commits pipelined controls before later gets and scoped call snapshots" $
      withTempResponseFile $ \input ->
      withTempResponseFile $ \output ->
      withTempResponseFile $ \errHandle -> do
        bodies <- newTVarIO []
        withWorkspaceCaptureServer bodies $ \mgr base -> do
          let requests =
                [ initializeRequest "init"
                , setWorkspaceRequest "set-a" (Just (String workspaceA))
                , projectCreateRequest "call-a"
                , setWorkspaceRequest "set-b" (Just (String workspaceB))
                , getWorkspaceRequest "get-b"
                , projectCreateRequest "call-b"
                , setWorkspaceRequest "clear" Nothing
                , projectCreateRequest "call-after-clear"
                ]
          BL.hPut input (BL.intercalate "\n" (map encode requests) <> "\n")
          hSeek input AbsoluteSeek 0
          runMCPServerWithHandles 2 8 input output errHandle mgr base Nothing
        receivedBodies <- readTVarIO bodies
        receivedBodies `shouldBe`
          [ object ["workspace_id" .= workspaceA, "name" .= ("context ordering" :: Text)]
          , object ["workspace_id" .= workspaceB, "name" .= ("context ordering" :: Text)]
          ]
        hSeek output AbsoluteSeek 0
        outputLines <- BL8.lines <$> strictHandleContents output
        let responses = [response | Just response <- map decode outputLines]
        responseFor "get-b" responses `shouldSatisfy` maybe False (valueContains workspaceB)
        responseFor "call-after-clear" responses `shouldSatisfy` maybe False (valueContains "workspace_id")

    it "keeps ordinary requests concurrent within a workspace epoch" $
      withTempResponseFile $ \input ->
      withTempResponseFile $ \output ->
      withTempResponseFile $ \errHandle -> do
        requestCount <- newMVar (0 :: Int)
        bothRequestsSeen <- newEmptyMVar
        releaseRequests <- newEmptyMVar
        resultVar <- newEmptyMVar
        withTwoRequestBlockingServer requestCount bothRequestsSeen releaseRequests $ \mgr base -> do
          let requests =
                [ initializeRequest "init"
                , setWorkspaceRequest "set-a" (Just (String workspaceA))
                , projectCreateRequest "call-a"
                , projectCreateRequest "call-b"
                ]
          BL.hPut input (BL.intercalate "\n" (map encode requests) <> "\n")
          hSeek input AbsoluteSeek 0
          _ <- forkIO $
            try @SomeException
              (runMCPServerWithHandles 2 8 input output errHandle mgr base Nothing)
              >>= putMVar resultVar
          waitForSignal bothRequestsSeen
          putMVar releaseRequests ()
          result <- waitForResult "Timed out waiting for concurrent epoch requests" resultVar
          case result of
            Left err -> expectationFailure $ "MCP server failed: " <> show err
            Right () -> pure ()

    it "cleans up active worker threads when shutdown logging fails" $
      withTempResponseFile $ \input ->
      withTempResponseFile $ \output ->
      withClosedTempHandle $ \closedErrHandle -> do
        workerIdsVar <- newEmptyMVar
        requestSeen <- newEmptyMVar
        blocker <- newEmptyMVar
        withBlockingHmemServer requestSeen blocker $ \mgr base -> do
          let initRequest = object
                [ "jsonrpc" .= ("2.0" :: Text)
                , "id" .= ("init-before-block" :: Text)
                , "method" .= ("initialize" :: Text)
                ]
              blockingSearch = object
                [ "jsonrpc" .= ("2.0" :: Text)
                , "id" .= ("blocking-search" :: Text)
                , "method" .= ("tools/call" :: Text)
                , "params" .= object
                    [ "name" .= ("search" :: Text)
                    , "arguments" .= object
                        [ "workspace_id" .= ("aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee" :: Text)
                        , "entity_types" .= (["observation"] :: [Text])
                        , "limit" .= (1 :: Int)
                        ]
                    ]
                ]
          BL.hPut input (encode initRequest <> "\n" <> encode blockingSearch <> "\n")
          hSeek input AbsoluteSeek 0
          (runMCPServerWithHandlesObserved
              (\workerIds -> putMVar workerIdsVar workerIds >> waitForSignal requestSeen)
              1 4 input output closedErrHandle mgr base Nothing
            `finally` putMVar blocker ())
            `shouldThrow` anyIOException
          tryReadMVar requestSeen `shouldReturn` Just ()
          workerIds <- readMVar workerIdsVar
          workerIds `shouldSatisfy` ((== 1) . length)
          waitForStoppedThreads workerIds

    it "cleans up idle workers when interrupted immediately after startup" $
      withTempResponseFile $ \input ->
      withTempResponseFile $ \output ->
      withTempResponseFile $ \errHandle -> do
        workerIdsVar <- newEmptyMVar
        mgr <- newManager defaultManagerSettings
        runMCPServerWithHandlesObserved
            (\workerIds -> putMVar workerIdsVar workerIds >> throwIO ThreadKilled)
            2 4 input output errHandle mgr unusedServerUrl Nothing
          `shouldThrow` (== ThreadKilled)
        workerIds <- readMVar workerIdsVar
        workerIds `shouldSatisfy` ((== 2) . length)
        waitForStoppedThreads workerIds

    it "cleans up partially-started workers when startup fails during spawning" $
      withTempResponseFile $ \input ->
      withTempResponseFile $ \output ->
      withTempResponseFile $ \errHandle -> do
        firstWorkerVar <- newEmptyMVar
        spawnAttemptVar <- newMVar (0 :: Int)
        mgr <- newManager defaultManagerSettings
        runMCPServerWithHandlesObservedWithFork
            (failingSecondSpawn firstWorkerVar spawnAttemptVar)
            (const $ pure ())
            2 4 input output errHandle mgr unusedServerUrl Nothing
          `shouldThrow` (== ThreadKilled)
        firstWorker <- readMVar firstWorkerVar
        waitForStoppedThreads [firstWorker]

    it "cleans up partially-started workers when interrupted asynchronously during a blocking spawn" $
      withTempResponseFile $ \input ->
      withTempResponseFile $ \output ->
      withTempResponseFile $ \errHandle -> do
        firstWorkerVar <- newEmptyMVar
        spawnAttemptVar <- newMVar (0 :: Int)
        secondSpawnEntered <- newEmptyMVar
        releaseSpawn <- newEmptyMVar
        resultVar <- newEmptyMVar
        mgr <- newManager defaultManagerSettings
        parentThread <- forkIO $
          try @SomeException
            (runMCPServerWithHandlesObservedWithFork
              (blockingSecondSpawn firstWorkerVar spawnAttemptVar secondSpawnEntered releaseSpawn)
              (const $ pure ())
              2 4 input output errHandle mgr unusedServerUrl Nothing)
            >>= putMVar resultVar
        result <- (do
            waitForSignal secondSpawnEntered
            throwTo parentThread ThreadKilled
            waitForResult "Timed out waiting for async startup interruption" resultVar)
          `finally` (tryPutMVar releaseSpawn () >> killThread parentThread)
        assertThreadKilled result
        firstWorker <- readMVar firstWorkerVar
        waitForStoppedThreads [firstWorker]

unusedServerUrl :: String
unusedServerUrl = "http://127.0.0.1:9"

workspaceA, workspaceB :: Text
workspaceA = "11111111-2222-3333-4444-555555555555"
workspaceB = "66666666-7777-8888-9999-aaaaaaaaaaaa"

initializeRequest :: Text -> Value
initializeRequest requestId = object
  [ "jsonrpc" .= ("2.0" :: Text)
  , "id" .= requestId
  , "method" .= ("initialize" :: Text)
  ]

setWorkspaceRequest :: Text -> Maybe Value -> Value
setWorkspaceRequest requestId workspace = object
  [ "jsonrpc" .= ("2.0" :: Text)
  , "id" .= requestId
  , "method" .= ("tools/call" :: Text)
  , "params" .= object
      [ "name" .= ("set_workspace" :: Text)
      , "arguments" .= workspaceArguments workspace
      ]
  ]

workspaceArguments :: Maybe Value -> Value
workspaceArguments Nothing = object []
workspaceArguments (Just value) = object ["workspace_id" .= value]

getWorkspaceRequest :: Text -> Value
getWorkspaceRequest requestId = object
  [ "jsonrpc" .= ("2.0" :: Text)
  , "id" .= requestId
  , "method" .= ("tools/call" :: Text)
  , "params" .= object ["name" .= ("get_workspace" :: Text), "arguments" .= object []]
  ]

projectCreateRequest :: Text -> Value
projectCreateRequest requestId = object
  [ "jsonrpc" .= ("2.0" :: Text)
  , "id" .= requestId
  , "method" .= ("tools/call" :: Text)
  , "params" .= object
      [ "name" .= ("project_create" :: Text)
      , "arguments" .= object ["name" .= ("context ordering" :: Text)]
      ]
  ]

workspaceCall :: Manager -> TVar Bool -> TVar (Maybe UUID) -> Text -> Maybe Value -> IO (Maybe Value)
workspaceCall mgr initialized wsContext action workspace =
  handleStdioLine mgr unusedServerUrl Nothing initialized wsContext $ jsonLine request
  where
    request = case action of
      "get" -> getWorkspaceRequest "workspace-call"
      _ -> setWorkspaceRequest "workspace-call" workspace

responseContains :: Text -> Maybe Value -> Bool
responseContains needle = maybe False (valueContains needle)

valueContains :: Text -> Value -> Bool
valueContains needle value = needle `T.isInfixOf` T.pack (BL8.unpack (encode value))

responseFor :: Text -> [Value] -> Maybe Value
responseFor requestId = go
  where
    go [] = Nothing
    go (value:rest)
      | jsonField "id" value == Just (String requestId) = Just value
      | otherwise = go rest

withWorkspaceCaptureServer :: TVar [Value] -> (Manager -> String -> IO a) -> IO a
withWorkspaceCaptureServer bodies action =
  testWithApplication (pure app) $ \port ->
    bracket (newManager defaultManagerSettings) closeManager $ \mgr ->
      action mgr ("http://127.0.0.1:" <> show port)
  where
    app request respond = do
      body <- Wai.strictRequestBody request
      case eitherDecode body of
        Right value -> atomically $ modifyTVar' bodies (<> [value])
        Left _ -> pure ()
      respond $ Wai.responseLBS status200 [(hContentType, "application/json")] "{}"

withStdioState :: (Manager -> TVar Bool -> TVar (Maybe UUID) -> IO a) -> IO a
withStdioState action = bracket (newManager defaultManagerSettings) closeManager $ \mgr -> do
  initialized <- newTVarIO False
  wsContext <- newTVarIO Nothing
  action mgr initialized wsContext

withBlockingHmemServer :: MVar () -> MVar () -> (Manager -> String -> IO a) -> IO a
withBlockingHmemServer requestSeen blocker action =
  testWithApplication (pure blockingApp) $ \port ->
    bracket (newManager defaultManagerSettings) closeManager $ \mgr ->
      action mgr ("http://127.0.0.1:" <> show port)
  where
    blockingApp _req respond = do
      putMVar requestSeen ()
      readMVar blocker
      respond $ Wai.responseLBS status200 [(hContentType, "application/json")] "{}"

withTwoRequestBlockingServer :: MVar Int -> MVar () -> MVar () -> (Manager -> String -> IO a) -> IO a
withTwoRequestBlockingServer requestCount bothRequestsSeen releaseRequests action =
  testWithApplication (pure blockingApp) $ \port ->
    bracket (newManager defaultManagerSettings) closeManager $ \mgr ->
      action mgr ("http://127.0.0.1:" <> show port)
  where
    blockingApp _req respond = do
      requestNumber <- modifyMVar requestCount $ \current -> do
        let next = current + 1
        pure (next, next)
      if requestNumber == 2 then putMVar bothRequestsSeen () else pure ()
      readMVar releaseRequests
      respond $ Wai.responseLBS status200 [(hContentType, "application/json")] "{}"

failingSecondSpawn :: MVar ThreadId -> MVar Int -> IO () -> IO ThreadId
failingSecondSpawn firstWorkerVar spawnAttemptVar action = do
  attempt <- nextSpawnAttempt spawnAttemptVar
  if attempt == 1
    then recordStartedWorker firstWorkerVar action
    else throwIO ThreadKilled

blockingSecondSpawn :: MVar ThreadId -> MVar Int -> MVar () -> MVar () -> IO () -> IO ThreadId
blockingSecondSpawn firstWorkerVar spawnAttemptVar secondSpawnEntered releaseSpawn action = do
  attempt <- nextSpawnAttempt spawnAttemptVar
  if attempt == 1
    then recordStartedWorker firstWorkerVar action
    else do
      putMVar secondSpawnEntered ()
      readMVar releaseSpawn
      ioError $ userError "blocking spawn released without async interruption"

nextSpawnAttempt :: MVar Int -> IO Int
nextSpawnAttempt spawnAttemptVar = modifyMVar spawnAttemptVar $ \current -> do
  let next = current + 1
  pure (next, next)

recordStartedWorker :: MVar ThreadId -> IO () -> IO ThreadId
recordStartedWorker firstWorkerVar action = do
  workerId <- forkIOWithUnmask $ \unmask -> unmask action
  putMVar firstWorkerVar workerId
  pure workerId

withTempResponseFile :: (Handle -> IO a) -> IO a
withTempResponseFile action =
  bracket
    (openTempFile "." "mcp-response.jsonl")
    (\(path, handle) -> hClose handle >> removeFile path)
    (\(_, handle) -> action handle)

withClosedTempHandle :: (Handle -> IO a) -> IO a
withClosedTempHandle action =
  bracket
    (openTempFile "." "mcp-closed-response.jsonl")
    (\(path, _handle) -> removeFile path)
    (\(_, handle) -> hClose handle >> action handle)

strictHandleContents :: Handle -> IO BL.ByteString
strictHandleContents handle = do
  bytes <- BL.hGetContents handle
  BL.length bytes `seq` pure bytes

waitForSignal :: MVar () -> IO ()
waitForSignal signal = waitForResult "Timed out waiting for signal" signal

waitForResult :: String -> MVar a -> IO a
waitForResult label = go (100 :: Int)
  where
    go 0 _resultVar = expectationFailure label >> error label
    go attempts resultVar = do
      mResult <- tryReadMVar resultVar
      case mResult of
        Just result -> pure result
        Nothing -> threadDelay 10000 >> go (attempts - 1) resultVar

assertThreadKilled :: Either SomeException a -> Expectation
assertThreadKilled result = case result of
  Left e -> case fromException e of
    Just ThreadKilled -> pure ()
    other -> expectationFailure $ "Expected ThreadKilled, got: " <> show other <> " from " <> show e
  Right _ -> expectationFailure "Expected ThreadKilled, but server loop completed successfully"

waitForStoppedThreads :: [ThreadId] -> Expectation
waitForStoppedThreads = go (50 :: Int)
  where
    go 0 tids = do
      statuses <- mapM threadStatus tids
      expectationFailure $ "Expected worker threads to stop, got statuses: " <> show statuses
    go attempts tids = do
      statuses <- mapM threadStatus tids
      if all isStopped statuses
        then pure ()
        else threadDelay 10000 >> go (attempts - 1) tids

    isStopped ThreadFinished = True
    isStopped ThreadDied = True
    isStopped _ = False

jsonLine :: Value -> BS8.ByteString
jsonLine = BL.toStrict . encode

jsonField :: Text -> Value -> Maybe Value
jsonField field (Object o) = KM.lookup (Key.fromText field) o
jsonField _ _ = Nothing

errorField :: Text -> Value -> Maybe Value
errorField field value = jsonField "error" value >>= jsonField field
