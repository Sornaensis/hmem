module HMem.MCP.ServerSpec (spec) where

import Control.Concurrent (ThreadId, forkIO, forkIOWithUnmask, killThread, threadDelay, throwTo)
import Control.Concurrent.MVar (MVar, modifyMVar, newEmptyMVar, newMVar, putMVar, readMVar, tryPutMVar, tryReadMVar)
import Control.Exception (AsyncException(..), SomeException, bracket, finally, fromException, throwIO, try)
import Control.Concurrent.STM (TVar, newTVarIO)
import Data.Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.Foldable (toList)
import Data.Text (Text)
import Data.UUID (UUID)
import Network.HTTP.Client (Manager, defaultManagerSettings, newManager)
import Network.HTTP.Types (hContentType, status200)
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp (testWithApplication)
import System.Directory (removeFile)
import System.IO (Handle, SeekMode(..), hClose, hSeek, openTempFile)
import GHC.Conc (ThreadStatus(..), threadStatus)
import Test.Hspec

import HMem.MCP.Server (encodeStdioResponse, handleStdioLine, runMCPServerWithHandles, runMCPServerWithHandlesObserved, runMCPServerWithHandlesObservedWithFork, sendResponseToHandle)

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
        case toolsResponse >>= jsonField "result" >>= jsonField "tools" of
          Just (Array tools) -> length (toList tools) `shouldSatisfy` (> 0)
          other -> expectationFailure $ "Expected tools/list array from stdio line, got: " <> show other

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
                        , "entity_types" .= (["memory"] :: [Text])
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

withStdioState :: (Manager -> TVar Bool -> TVar (Maybe UUID) -> IO a) -> IO a
withStdioState action = do
  mgr <- newManager defaultManagerSettings
  initialized <- newTVarIO False
  wsContext <- newTVarIO Nothing
  action mgr initialized wsContext

withBlockingHmemServer :: MVar () -> MVar () -> (Manager -> String -> IO a) -> IO a
withBlockingHmemServer requestSeen blocker action =
  testWithApplication (pure blockingApp) $ \port -> do
    mgr <- newManager defaultManagerSettings
    action mgr ("http://127.0.0.1:" <> show port)
  where
    blockingApp _req respond = do
      putMVar requestSeen ()
      readMVar blocker
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
