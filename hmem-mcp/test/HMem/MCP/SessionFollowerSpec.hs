{-# LANGUAGE RankNTypes #-}
module HMem.MCP.SessionFollowerSpec (spec) where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception
import Control.Monad (forM, forM_, replicateM, unless, void, (>=>))
import Data.Aeson
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Key qualified as Key
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as BL
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time
import Data.Maybe (fromMaybe)
import Data.Char (toLower)
import Data.UUID (UUID)
import Data.UUID.V4 qualified as UUID
import GHC.Conc (threadStatus, ThreadStatus(..))
import HMem.AgentSession qualified as Core
import HMem.AgentSession.Transport qualified as Wire
import HMem.BotIdentity (BotIdentity, mkBotIdentity)
import HMem.Config qualified as Config
import HMem.DB.AgentSession qualified as Sessions
import HMem.DB.RequestContext
import HMem.DB.TestHarness (TestEnv(..), createTestWorkspace, withTestEnv)
import HMem.MCP.SessionFile
import HMem.MCP.SessionFollower
import HMem.MCP.SessionHTTP
import HMem.MCP.SessionProcess
import HMem.MCP.Server (MCPContext(..), handleStdioLineWithRegistry, runMCPServerWithHandlesObservedWithFollowerOptions)
import HMem.MCP.Tools (handleToolCall)
import HMem.Server.AccessTracker (newAccessTracker)
import HMem.Server.App (mkAppWithChangeStream)
import HMem.Server.WebSocket (newWSState)
import HMem.Types (Workspace(..))
import Network.HTTP.Client (Manager, newManager, closeManager, defaultManagerSettings, managerIdleConnectionCount)
import Network.Wai.Handler.Warp (testWithApplication)
import Network.Wai qualified as Wai
import Network.HTTP.Types (status200)
import Network.HTTP.Types.URI (parseQuery)
import System.Directory
import System.Environment (getEnv)
import System.IO
import System.Process (createPipe)
import System.Process qualified as Process
import System.FilePath ((</>))
import System.Info (os)
import Paths_hmem_mcp qualified as Paths
import System.Timeout
import Test.Hspec
import Text.Read (readMaybe)

spec :: Spec
spec = describe "Session follower" $ do
  it "poisons follower admission and retains its slot until an unconfirmed helper owner finishes cleanup" $
    withSourceFile "" $ \path -> withManager $ \manager -> do
      entered <- newEmptyMVar; release <- newEmptyMVar
      scope <- newProcessScopeWithReapHook (putMVar entered () >> readMVar release)
      sid <- UUID.nextRandom
      let app _ respond = respond (Wai.responseLBS status200 [] (encode (fakeMetadata sid 0)))
      testWithApplication (pure app) $ \port -> withRegistry fast {processScopeFactory = pure scope} $ \registry -> do
        let base = "http://127.0.0.1:" <> show port
        _ <- start registry manager base workspace sid path
        timeout 3000000 (readMVar entered) >>= (`shouldSatisfy` isJustValue)
        flip finally (void (tryPutMVar release ())) $ do
          _ <- await registry manager base workspace sid (hasStatus "failed")
          (slots,_,_) <- registryMetrics registry
          slots `shouldBe` 1
          processMetrics scope `shouldReturn` (1,0,True)
          nativeProcessCount `shouldReturn` 1
          another <- UUID.nextRandom
          refused <- start registry manager base workspace another path
          nested ["error","code"] refused `shouldBe` Just (String "registry_closed")
          putMVar release ()
          waitUntil $ do (leases,pipes,_) <- processMetrics scope; pure (leases == 0 && pipes == 0)
          nativeProcessCount `shouldReturn` 0

  it "supervises helper native DNS/connect/TLS/body waits, cleanup deadlines and malformed/oversized frames" $ do
    executable <- testHelperExecutable
    forM_ ["dns","connect","tls","body"] $ \phase -> do
      scope <- newProcessScope
      pid <- newEmptyMVar; entered <- newEmptyMVar; done <- newEmptyMVar
      let payload = helperProbe phase
          action = try @SomeException (runHelper scope executable (putMVar pid) (putMVar entered) payload) >>= putMVar done
      bracket (forkIO action) (\tid -> do atomically $ cancelProcessScope scope; _ <- forkIO (killThread tid); pure ()) $ \tid -> do
        timeout 3000000 (readMVar entered) `shouldReturn` Just phase
        _ <- forkIO (killThread tid)
        result <- timeout 2000000 (readMVar done)
        result `shouldSatisfy` maybe False (either (\e -> case fromException e :: Maybe AsyncException of Just ThreadKilled -> True; _ -> False) (const False))
        processMetrics scope `shouldReturn` (0,0,False)
        readMVar pid >>= (processAlive >=> (`shouldBe` False))
        nativeProcessCount `shouldReturn` 0
    scope <- newProcessScope
    pid <- newEmptyMVar
    began <- getCurrentTime
    runHelper scope executable (putMVar pid) (const (pure ())) (helperProbe "dns") `shouldReturn` Left ProcessDeadline
    finished <- getCurrentTime
    diffUTCTime finished began `shouldSatisfy` (< 5.5)
    readMVar pid >>= (processAlive >=> (`shouldBe` False))
    runHelper scope executable (const (pure ())) (const (pure ())) "{" `shouldReturn` Right (BS.singleton 2)
    runHelper scope executable (const (throwIO (userError "injected startup observation failure"))) (const (pure ())) "{}" `shouldReturn` Left ProcessProtocol
    runHelper scope executable (const (pure ())) (const (pure ())) (BS.replicate (frameCap+1) 0) `shouldReturn` Left ProcessUnavailable
    processMetrics scope `shouldReturn` (0,0,False)
    nativeProcessCount `shouldReturn` 0

  it "preserves the exact HTTP response cap and raw exponent bytes across private helper framing" $ do
    let boundary = "\"" <> BS.replicate 2097150 97 <> "\""
        app request respond = respond $ Wai.responseLBS status200 [] $ BL.fromStrict $
          case Wai.pathInfo request of ["overflow"] -> boundary <> " "; ["exponent"] -> "1e10000000"; _ -> boundary
    testWithApplication (pure app) $ \port -> do
      scope <- newProcessScope; executable <- testHelperExecutable
      let base = "http://127.0.0.1:" <> show port
      captured <- captureHTTP scope executable base Nothing identity
      boundaryResult <- followerRequest captured "GET" "/boundary" Nothing
      case boundaryResult of Right (String text) -> T.length text `shouldBe` 2097150; _ -> expectationFailure "HTTP cap boundary lost"
      followerRequest captured "GET" "/overflow" Nothing `shouldReturn` Left HTTPTooLarge
      raw <- runHelper scope executable (const (pure ())) (const (pure ())) (BL.toStrict $ encode $ object
        ["url" .= (base <> "/exponent"),"method" .= ("GET" :: Text),"headers" .= ([] :: [(Text,Text)]),"body" .= Null])
      fmap (BS.drop 3) raw `shouldBe` Right "1e10000000"
      processMetrics scope `shouldReturn` (0,0,False)
      nativeProcessCount `shouldReturn` 0

  it "kills production helpers on hard loss of the actual MCP parent" $
    withSourceFile "{}\n" $ \path -> do
      seen <- newTVarIO (0 :: Int); release <- newEmptyMVar
      let app _ respond = atomically (modifyTVar' seen (+1)) >> readMVar release >> respond (Wai.responseLBS status200 [] "{}")
      testWithApplication (pure app) $ \port -> do
        executable <- testHelperExecutable
        let config = (Process.proc executable ["--server-url","http://127.0.0.1:" <> show port,"--no-auth","+RTS","-N1","-RTS"])
              {Process.std_in=Process.CreatePipe,Process.std_out=Process.CreatePipe,Process.std_err=Process.CreatePipe}
        bracket (Process.createProcess config) (\(input,output,errors,process) -> do
          pid <- Process.getPid process
          forM_ pid $ \value -> void (killProcessId (fromIntegral value))
          _ <- timeout 3000000 (Process.waitForProcess process)
          forM_ [input,output,errors] $ mapM_ (\handle -> hIsClosed handle >>= \closed -> unless closed (hClose handle))) $ \(input,output,errors,process) ->
          case (input,output,errors) of
            (Just send,Just receive,Just diagnostics) -> do
              drained <- newEmptyMVar; errorsDrained <- newEmptyMVar
              let drain handle = BS.hGetSome handle 4096 >>= \bytes -> unless (BS.null bytes) (drain handle)
              _ <- forkIO (drain receive `finally` putMVar drained ())
              _ <- forkIO (drain diagnostics `finally` putMVar errorsDrained ())
              ids <- replicateM 2 UUID.nextRandom
              let call name arguments = object ["jsonrpc" .= ("2.0" :: Text),"id" .= (name :: Text),"method" .= ("tools/call" :: Text),"params" .= object ["name" .= name,"arguments" .= arguments]]
                  requests = [object ["jsonrpc" .= ("2.0" :: Text),"id" .= ("init" :: Text),"method" .= ("initialize" :: Text)]
                    ,call "set_workspace" (object ["workspace_id" .= workspace,"identity" .= identity])]
                    <> [call "session_upload_start" (object ["session_id" .= sid,"path" .= path,"format_label" .= ("jsonl" :: Text)]) | sid <- ids]
              BL.hPut send (BL.concat [encode request <> "\n" | request <- requests]); hFlush send
              flip finally (void (tryPutMVar release ())) $ do
                waitUntil $ (>= 2) <$> readTVarIO seen
                parent <- Process.getPid process >>= maybe (throwIO ProcessUnavailable) (pure . fromIntegral)
                children <- childProcessIds parent
                images <- mapM (processImage >=> canonicalizePath) children
                helper <- canonicalizePath executable
                consoles <- if os == "mingw32" then do
                  root <- getEnv "SystemRoot"
                  image <- canonicalizePath (root </> "system32" </> "conhost.exe")
                  pure [image]
                  else pure []
                let same left right = (if os == "mingw32" then map toLower else id) left == (if os == "mingw32" then map toLower else id) right
                length (filter (`same` helper) images) `shouldBe` 2
                images `shouldSatisfy` all (\image -> any (same image) (helper:consoles))
                length (filter (\image -> any (same image) consoles) images) `shouldSatisfy` (<= 1)
                killProcessId parent `shouldReturn` True
                timeout 3000000 (Process.waitForProcess process) >>= (`shouldSatisfy` isJustValue)
                waitUntil $ and <$> mapM (fmap not . processAlive) children
                timeout 1000000 (readMVar drained) >>= (`shouldSatisfy` isJustValue)
                timeout 1000000 (readMVar errorsDrained) >>= (`shouldSatisfy` isJustValue)
                readTVarIO seen `shouldReturn` 2
            _ -> expectationFailure "MCP parent private pipes missing"

  it "cleans Windows attribute-initialization failure without deleting uninitialized attributes or leaking handles" $ do
    helper <- testHelperExecutable
    code <- attributeFailureProbe helper
    if code == 2 then pendingWith "Windows process attribute API is unavailable on this platform" else code `shouldBe` 1
    nativeProcessCount `shouldReturn` 0

  it "reads from a real regular handle and detects replacement, truncation and same-size preserved-mtime rewrites" $
    withSourceFile "{}\r\n" $ \path -> bracket (openSource path) closeSource $ \source -> do
      checkSource source 4 `shouldReturn` 4
      readSource source 0 4 `shouldReturn` "{}\r\n"
      stamp <- getModificationTime path
      BS.writeFile path "[]\r\n"
      setModificationTime path stamp
      checkSource source 4 `shouldReturn` 4
      readSource source 0 4 `shouldReturn` "[]\r\n"
      BS.writeFile path "{}"
      checkSource source 4 `shouldThrow` (== SourceTruncated)
      BS.writeFile path "{}\r\n"
      renameFile path (path <> ".old")
      BS.writeFile path "{}\r\n"
      checkSource source 4 `shouldThrow` (== SourceChanged)

  it "fails closed for missing/nonregular sources without path disclosure" $
    withSourceFile "" $ \path -> do
      openSource (path <> ".missing") `shouldThrow` (== SourceUnavailable)
      directory <- getTemporaryDirectory
      openSource directory `shouldThrow` (== SourceUnavailable)
      withManager $ \manager -> withRegistry fast $ \registry -> do
        sid <- UUID.nextRandom
        started <- start registry manager "http://unused.invalid" workspace sid (path <> ".missing")
        started `shouldSatisfy` (not . contains (T.pack path))
        failed <- await registry manager "http://unused.invalid" workspace sid (hasStatus "failed")
        failed `shouldSatisfy` (not . contains (T.pack path))

  it "automatically uploads exact CRLF/UTF8, holds partial tails, flushes a final record and resumes across EOF separators" $
    withBackend $ \env wid manager base -> withSourceFile "{\"event\":1}\r\n" $ \path -> withRegistry fast $ \registry -> do
      sid <- UUID.nextRandom
      started <- start registry manager base wid sid path
      hasStatus "validating" started `shouldBe` True
      _ <- await registry manager base wid sid (hasOffset 13)
      let unicode = TE.encodeUtf8 "{\"s\":\"雪\"}"
      BS.appendFile path (BS.take 8 unicode)
      pending <- await registry manager base wid sid (hasStatus "pending_tail")
      pending `shouldSatisfy` hasOffset 13
      BS.appendFile path (BS.drop 8 unicode)
      _ <- control registry manager base wid sid "session_upload_flush"
      flushed <- await registry manager base wid sid hasReceipt
      let expected = "{\"event\":1}\r\n" <> unicode
      receiptHash flushed `shouldBe` Just (String (Core.sessionSha256 expected))
      BS.appendFile path "\r\n{\"next\":2}\n"
      _ <- await registry manager base wid sid (hasOffset (BS.length expected + BS.length "\r\n{\"next\":2}\n"))
      _ <- control registry manager base wid sid "session_upload_stop"
      stopped <- await registry manager base wid sid (hasStatus "stopped")
      receiptHash stopped `shouldBe` Just (String (Core.sessionSha256 (expected <> "\r\n{\"next\":2}\n")))
      session <- Sessions.getSession env.pool wid sid
      fmap (.state) session `shouldBe` Just Core.Active
      range <- Sessions.readSessionRange env.pool wid sid 0 1048576
      range.bytes `shouldBe` expected <> "\r\n{\"next\":2}\n"

  it "retains invalid EOF tails, fails malformed terminated records and detects persistent same-size committed rewrites" $
    withBackend $ \_ wid manager base -> do
      withSourceFile "{}\n{broken" $ \path -> withRegistry fast $ \registry -> do
        sid <- UUID.nextRandom
        _ <- start registry manager base wid sid path
        _ <- await registry manager base wid sid (hasStatus "pending_tail")
        _ <- control registry manager base wid sid "session_upload_stop"
        stopped <- await registry manager base wid sid (hasStatus "stopped")
        (field "pending_tail" =<< field "receipt" stopped) `shouldBe` Just (Bool True)
      withSourceFile "{broken}\n" $ \path -> withRegistry fast $ \registry -> do
        sid <- UUID.nextRandom
        _ <- start registry manager base wid sid path
        _ <- await registry manager base wid sid (hasStatus "failed")
        pure ()
      withSourceFile "{}\n" $ \path -> withRegistry fast $ \registry -> do
        sid <- UUID.nextRandom
        _ <- start registry manager base wid sid path
        _ <- await registry manager base wid sid (hasOffset 3)
        stamp <- getModificationTime path
        BS.writeFile path "[]\n"
        setModificationTime path stamp
        changed <- await registry manager base wid sid (hasStatus "failed")
        field "error" changed `shouldBe` Just (String "source_identity_or_prefix_changed")

  it "retries an exact lost-ack append, verifies the full prefix on restart, and refuses a mismatched resume" $
    withBackend $ \env wid manager base -> withSourceFile "{}\n" $ \path -> do
      sid <- UUID.nextRandom
      captured <- captureTestHTTP base
      lost <- newTVarIO False
      sent <- newTVarIO ([] :: [Value])
      let action verb target body = do
            result <- followerRequest captured verb target body
            if "/append" `T.isSuffixOf` T.pack target then do
              first <- atomically $ do
                previous <- readTVar lost
                writeTVar lost True
                modifyTVar' sent (<> maybe [] pure body)
                pure (not previous)
              if first then pure (Left HTTPUnavailable) else pure result
            else pure result
      withRegistry fast {requestOverride = Just action} $ \registry -> do
        _ <- start registry manager base wid sid path
        _ <- await registry manager base wid sid (hasOffset 3)
        requests <- readTVarIO sent
        length requests `shouldSatisfy` (>= 2)
        take 2 requests `shouldBe` replicate 2 (head requests)
      session <- Sessions.getSession env.pool wid sid
      fmap (.state) session `shouldBe` Just Core.Active
      BS.appendFile path "[]\n"
      withRegistry fast $ \registry -> do
        _ <- start registry manager base wid sid path
        _ <- await registry manager base wid sid (hasOffset 6)
        _ <- control registry manager base wid sid "session_upload_stop"
        _ <- await registry manager base wid sid (hasStatus "stopped")
        pure ()
      BS.writeFile path "[ ]\n[]\n"
      withRegistry fast $ \registry -> do
        _ <- start registry manager base wid sid path
        _ <- await registry manager base wid sid (hasStatus "failed")
        pure ()

  it "caps four jobs and every follower HTTP category at two while normal terminate and context changes progress" $
    withBackend $ \_ wid manager base -> withSourceFile "{}\n" $ \path -> do
      blocked <- newEmptyMVar
      seen <- newTVarIO ([] :: [String])
      let action _ target _ = atomically (modifyTVar' seen (<> [target])) >> readMVar blocked >> pure (Left HTTPUnavailable)
      withRegistry fast {requestOverride = Just action} $ \registry -> do
        ids <- replicateM 5 UUID.nextRandom
        forM_ (take 4 ids) $ \sid -> start registry manager base wid sid path >>= (\result -> hasStatus "validating" result `shouldBe` True)
        full <- start registry manager base wid (last ids) path
        nested ["error","code"] full `shouldBe` Just (String "capacity")
        duplicate <- start registry manager base wid (head ids) path
        nested ["error","code"] duplicate `shouldBe` Just (String "already_following")
        _ <- waitUntil $ do (active,peak,_) <- registryMetrics registry; pure (active == 2 && peak == 2)
        initialized <- newTVarIO True
        context <- newTVarIO (Just (MCPContext (Just wid) identity))
        normal <- UUID.nextRandom
        _ <- rpc registry manager base initialized context "session_create" (object ["session_id" .= normal, "format_label" .= ("jsonl" :: Text)])
        terminated <- timeout 1000000 $ rpc registry manager base initialized context "session_terminate" (object ["session_id" .= normal,"expected_offset" .= (0 :: Int),"expected_revision" .= (0 :: Int),"final_bytes" .= (0 :: Int),"final_sha256" .= Core.sessionSha256 ""])
        terminated `shouldSatisfy` isJustValue
        other <- UUID.nextRandom
        _ <- rpc registry manager base initialized context "set_workspace" (object ["workspace_id" .= other, "identity" .= identity])
        following <- readTVarIO seen
        following `shouldSatisfy` all (T.isInfixOf (T.pack (show wid)) . T.pack)
        controlResult <- timeout 200000 $ control registry manager base wid (head ids) "session_upload_stop"
        controlResult `shouldSatisfy` isJustValue
        (active,peak,total) <- registryMetrics registry
        (active,peak,total) `shouldBe` (2,2,4)

  it "observes explicit live termination and closes without another append or heartbeat" $
    withBackend $ \env wid manager base -> withSourceFile "{}\n" $ \path -> withRegistry fast $ \registry -> do
      sid <- UUID.nextRandom
      _ <- start registry manager base wid sid path
      _ <- await registry manager base wid sid (hasOffset 3)
      Just session <- Sessions.getSession env.pool wid sid
      _ <- Sessions.terminateSession env.pool wid sid (Core.TerminateSession session.committedBytes session.revision 3 (Core.sessionSha256 "{}\n"))
      BS.appendFile path "[]\n"
      _ <- await registry manager base wid sid (hasStatus "terminated")
      Just sealed <- Sessions.getSession env.pool wid sid
      sealed.committedBytes `shouldBe` 3

  it "freezes a stop boundary during long verification and returns pending instead of a premature receipt" $
    withBackend $ \env wid manager base -> do
      let original = BS.concat (replicate 80000 "{}\n")
      withSourceFile original $ \path -> do
        sid <- UUID.nextRandom
        _ <- Sessions.createSession env.pool (Core.CreateSession sid wid "jsonl")
        _ <- Sessions.appendSession env.pool wid sid (Core.AppendSession 0 0 original False)
        captured <- captureTestHTTP base
        seen <- newEmptyMVar; release <- newEmptyMVar; blocked <- newTVarIO False
        let action verb target body = do
              first <- if "/transcript?" `T.isInfixOf` T.pack target then atomically $ do previous <- readTVar blocked; writeTVar blocked True; pure (not previous) else pure False
              if first then putMVar seen () >> readMVar release else pure ()
              followerRequest captured verb target body
        withRegistry fast {requestOverride = Just action} $ \registry -> do
          _ <- start registry manager base wid sid path
          timeout 2000000 (readMVar seen) >>= (`shouldSatisfy` isJustValue)
          waiting <- control registry manager base wid sid "session_upload_stop"
          nested ["operation","pending"] waiting `shouldBe` Just (Bool True)
          field "receipt" waiting `shouldBe` Just Null
          putMVar release ()
          _ <- await registry manager base wid sid (\value -> nested ["operation","capture_boundary"] value == Just (Number (fromIntegral (BS.length original))))
          BS.appendFile path "[]\n"
          stopped <- await registry manager base wid sid (hasStatus "stopped")
          nested ["receipt","capture_boundary"] stopped `shouldBe` Just (Number (fromIntegral (BS.length original)))
          receiptHash stopped `shouldBe` Just (String (Core.sessionSha256 original))
          Just canonical <- Sessions.getSession env.pool wid sid
          canonical.committedBytes `shouldBe` fromIntegral (BS.length original)
          canonical.state `shouldBe` Core.Active

  it "fairly progresses four jobs through create/read/append/verification/heartbeat under the shared gate" $
    withBackend $ \_ wid manager base -> withSourceFile (BS.concat (replicate 30000 "{}\n")) $ \path -> do
      offset <- newTVarIO (0 :: NominalDiffTime)
      calls <- newTVarIO ([] :: [(BS.ByteString,String)])
      captured <- captureTestHTTP base
      let clock = do delta <- readTVarIO offset; addUTCTime delta <$> getCurrentTime
          action verb target body = do
            atomically $ modifyTVar' calls (<> [(verb,target)])
            threadDelay 10000
            followerRequest captured verb target body
      withRegistry fast {requestOverride = Just action, now = clock} $ \registry -> do
        ids <- replicateM 4 UUID.nextRandom
        forM_ ids $ \sid -> start registry manager base wid sid path >> pure ()
        forM_ ids $ \sid -> await registry manager base wid sid (hasOffset 90000) >> pure ()
        atomically $ writeTVar offset 11
        waitUntil $ any (T.isSuffixOf "/heartbeat" . T.pack . snd) <$> readTVarIO calls
        BS.appendFile path "[]\n"
        forM_ ids $ \sid -> await registry manager base wid sid (hasOffset 90003) >> pure ()
        forM_ ids $ \sid -> control registry manager base wid sid "session_upload_stop" >> pure ()
        forM_ ids $ \sid -> await registry manager base wid sid (hasStatus "stopped") >> pure ()
        (active,peak,total) <- registryMetrics registry
        active `shouldBe` 0
        peak `shouldSatisfy` (\n -> n >= 1 && n <= 2)
        total `shouldBe` 4
        observed <- readTVarIO calls
        forM_ ["/sessions","/append","/heartbeat","/transcript?"] $ \category ->
          observed `shouldSatisfy` any (T.isInfixOf category . T.pack . snd)

  it "fails closed on revoked permissions, reports staleness independently, and bounds terminal retention" $
    withBackend $ \_ wid manager base -> withSourceFile "{}\n" $ \path -> do
      offset <- newTVarIO (0 :: NominalDiffTime)
      revoked <- newTVarIO False
      captured <- captureTestHTTP base
      let clock = do delta <- readTVarIO offset; addUTCTime delta <$> getCurrentTime
          action verb target body = do
            deny <- readTVarIO revoked
            if deny then pure (Left (HTTPStatus 403)) else followerRequest captured verb target body
      withRegistry fast {requestOverride = Just action, now = clock} $ \registry -> do
        sid <- UUID.nextRandom
        _ <- start registry manager base wid sid path
        _ <- await registry manager base wid sid (hasOffset 3)
        -- An already accepted HTTP request may still acknowledge after
        -- revocation. Establish the terminal state before advancing staleness.
        atomically $ writeTVar revoked True >> writeTVar offset 11
        failed <- await registry manager base wid sid (hasStatus "failed")
        field "error" failed `shouldBe` Just (String "permission_revoked")
        field "stale" failed `shouldBe` Just (Bool False)
        atomically $ writeTVar offset 42
        stale <- await registry manager base wid sid (hasStatus "failed")
        field "stale" stale `shouldBe` Just (Bool True)
        forM_ [1..20 :: Int] $ \_ -> do
          another <- UUID.nextRandom
          _ <- start registry manager base wid another (path <> ".missing")
          _ <- await registry manager base wid another (hasStatus "failed")
          pure ()
        (_,_,retained) <- registryMetrics registry
        retained `shouldSatisfy` (<= 16)
        atomically $ writeTVar offset 400
        (_,_,expired) <- registryMetrics registry
        expired `shouldBe` 0

  it "refuses replacement/truncation and a missing EOF separator without advancing evidence" $
    withBackend $ \env wid manager base -> do
      forM_ [False,True] $ \replace -> withSourceFile "{}\n" $ \path -> withRegistry fast $ \registry -> do
        sid <- UUID.nextRandom
        _ <- start registry manager base wid sid path
        _ <- await registry manager base wid sid (hasOffset 3)
        if replace then renameFile path (path <> ".old") >> BS.writeFile path "{}\n" else BS.writeFile path ""
        _ <- await registry manager base wid sid (hasStatus "failed")
        pure ()
      withSourceFile "{}" $ \path -> withRegistry fast $ \registry -> do
        sid <- UUID.nextRandom
        _ <- start registry manager base wid sid path
        _ <- await registry manager base wid sid (hasStatus "pending_tail")
        _ <- control registry manager base wid sid "session_upload_flush"
        _ <- await registry manager base wid sid hasReceipt
        BS.appendFile path "[]\n"
        _ <- await registry manager base wid sid (hasStatus "failed")
        Just canonical <- Sessions.getSession env.pool wid sid
        canonical.committedBytes `shouldBe` 2
        canonical.state `shouldBe` Core.Active

  it "closes and joins workers on normal/exceptional lifetime and cleans partially started pools" $
    withManager $ \manager -> withSourceFile "{}\n" $ \path -> do
      ids <- newTVarIO []
      let fork :: ((forall a. IO a -> IO a) -> IO ()) -> IO ThreadId
          fork action = do tid <- forkIOWithUnmask action; atomically $ modifyTVar' ids (<> [tid]); pure tid
          options = fast {forkWorker = fork, requestOverride = Just (\_ _ _ -> atomically retry)}
      registry <- newRegistryWith options
      sid <- UUID.nextRandom
      _ <- start registry manager "http://unused.invalid" workspace sid path
      _ <- waitUntil $ do (active,_,_) <- registryMetrics registry; pure (active == 1)
      closeRegistry registry `shouldReturn` True
      openSourceCount `shouldReturn` 0
      threads <- readTVarIO ids
      forM_ threads $ \tid -> threadStatus tid >>= (`shouldSatisfy` (\s -> s == ThreadFinished || s == ThreadDied))
      BS.appendFile path "[]\n"
      counter <- newTVarIO (0 :: Int)
      let failingFork :: ((forall a. IO a -> IO a) -> IO ()) -> IO ThreadId
          failingFork action = do
            n <- atomically $ do n <- readTVar counter; writeTVar counter (n+1); pure n
            if n == 1 then throwIO (userError "injected startup failure") else fork action
      newRegistryWith options {forkWorker = failingFork} `shouldThrow` anyIOException
      allThreads <- readTVarIO ids
      waitUntil $ and <$> mapM (\tid -> do s <- threadStatus tid; pure (s == ThreadFinished || s == ThreadDied)) allThreads
      openSourceCount `shouldReturn` 0

  it "publishes startup readiness inside completion ownership before cancellation can enter either worker" $ do
    ids <- newTVarIO []
    entered <- newTVarIO (0 :: Int); release <- newEmptyMVar
    let fork :: ((forall a. IO a -> IO a) -> IO ()) -> IO ThreadId
        fork action = do
          tid <- forkIOWithUnmask $ \unmask -> action $ \work -> unmask $ do
            atomically $ modifyTVar' entered (+1)
            readMVar release
            work
          atomically $ modifyTVar' ids (<> [tid])
          pure tid
    registry <- newRegistryWith fast {forkWorker = fork}
    flip finally (void (tryPutMVar release ())) $ do
      waitUntil $ (== 2) <$> readTVarIO entered
      closeRegistry registry `shouldReturn` True
      threads <- readTVarIO ids
      length threads `shouldBe` 2
      forM_ threads $ \tid -> threadStatus tid >>= (`shouldSatisfy` (\state -> state == ThreadFinished || state == ThreadDied))
      openSourceCount `shouldReturn` 0
      nativeProcessCount `shouldReturn` 0

  it "interleaves idle heartbeats with a complete multi-megabyte resume verification" $
    withManager $ \manager -> do
      let chunk = BS.concat (replicate 30 ("\"" <> BS.replicate 32765 97 <> "\"\n"))
          original = chunk <> chunk
          wid = workspace
          base = "http://unused.invalid"
      withSourceFile original $ \path -> do
        sid <- UUID.nextRandom
        offset <- newTVarIO (0 :: NominalDiffTime)
        beats <- newTVarIO (0 :: Int)
        let clock = do delta <- readTVarIO offset; addUTCTime delta <$> getCurrentTime
            action _ target _ = do
              if "/transcript?" `T.isInfixOf` T.pack target then atomically $ modifyTVar' offset (+1) else pure ()
              if "/heartbeat" `T.isSuffixOf` T.pack target then atomically $ modifyTVar' beats (+1) else pure ()
              let startOffset = queryNumber "offset" target
                  count = queryNumber "limit" target
                  bytes = BS.take count (BS.drop startOffset original)
              pure $ Right $ if "/transcript?" `T.isInfixOf` T.pack target then object
                ["session" .= fakeMetadata sid (BS.length original), "offset" .= startOffset, "next_offset" .= (startOffset + BS.length bytes)
                , "data_base64" .= Wire.encodeSessionBytes bytes, "has_more" .= (startOffset + BS.length bytes < BS.length original)]
                else fakeMetadata sid (BS.length original)
        withRegistry fast {requestOverride = Just action, now = clock} $ \registry -> do
          _ <- start registry manager base wid sid path
          waitUntil $ (>= 2) <$> readTVarIO beats
          state <- control registry manager base wid sid "session_upload_status"
          hasStatus "validating" state `shouldBe` True

  it "anchors acknowledgement, idle heartbeat and retry deadlines to delayed HTTP completion, never local saves" $
    withManager $ \manager -> withSourceFile "" $ \path -> do
      sid <- UUID.nextRandom
      clock <- newTVarIO epoch
      gotMetadata <- newEmptyMVar; finishMetadata <- newEmptyMVar
      gotHeartbeat <- newEmptyMVar; finishHeartbeat <- newEmptyMVar
      beats <- newTVarIO (0 :: Int)
      let action _ target _
            | "/heartbeat" `T.isSuffixOf` T.pack target = do
                n <- atomically $ do n <- readTVar beats; writeTVar beats (n+1); pure n
                if n == 0 then putMVar gotHeartbeat () >> readMVar finishHeartbeat else pure ()
                pure (Right (fakeMetadata sid 0))
            | otherwise = putMVar gotMetadata () >> readMVar finishMetadata >> pure (Right (fakeMetadata sid 0))
          options = fast {intervalMicros = 0, now = readTVarIO clock, requestOverride = Just action}
      withRegistry options $ \registry -> do
        _ <- start registry manager "http://unused.invalid" workspace sid path
        timeout 2000000 (readMVar gotMetadata) >>= (`shouldSatisfy` isJustValue)
        atomically $ writeTVar clock (addUTCTime 5 epoch)
        putMVar finishMetadata ()
        timeout 2000000 (readMVar gotHeartbeat) >>= (`shouldSatisfy` isJustValue)
        ack <- control registry manager "http://unused.invalid" workspace sid "session_upload_status"
        field "last_ack_at" ack `shouldBe` Just (toJSON (addUTCTime 5 epoch))
        atomically $ writeTVar clock (addUTCTime 10 epoch)
        putMVar finishHeartbeat ()
        _ <- await registry manager "http://unused.invalid" workspace sid (\s -> field "last_ack_at" s == Just (toJSON (addUTCTime 10 epoch)))
        atomically $ writeTVar clock (addUTCTime 19 epoch)
        threadDelay 80000
        readTVarIO beats `shouldReturn` 1
        local <- control registry manager "http://unused.invalid" workspace sid "session_upload_status"
        field "last_ack_at" local `shouldBe` Just (toJSON (addUTCTime 10 epoch))
        atomically $ writeTVar clock (addUTCTime 20 epoch)
        waitUntil $ (== 2) <$> readTVarIO beats
      attempts <- newTVarIO (0 :: Int)
      began <- newEmptyMVar; finish <- newEmptyMVar
      atomically $ writeTVar clock epoch
      another <- UUID.nextRandom
      let transient _ _ _ = do
            n <- atomically $ do n <- readTVar attempts; writeTVar attempts (n+1); pure n
            if n == 0 then putMVar began () >> readMVar finish >> pure (Left HTTPUnavailable)
            else pure (Left (HTTPStatus 403))
      withRegistry fast {intervalMicros = 0, now = readTVarIO clock, requestOverride = Just transient} $ \registry -> do
        _ <- start registry manager "http://unused.invalid" workspace another path
        timeout 2000000 (readMVar began) >>= (`shouldSatisfy` isJustValue)
        atomically $ writeTVar clock (addUTCTime 5 epoch)
        putMVar finish ()
        _ <- await registry manager "http://unused.invalid" workspace another (hasStatus "retry")
        threadDelay 80000
        readTVarIO attempts `shouldReturn` 1
        atomically $ writeTVar clock (addUTCTime 6 epoch)
        _ <- await registry manager "http://unused.invalid" workspace another (hasStatus "failed")
        readTVarIO attempts `shouldReturn` 2

  it "atomically admits/completes controls across retirement and stop-finalization races" $
    withManager $ \manager -> withSourceFile "" $ \path -> do
      let base = "http://unused.invalid"
      sid <- UUID.nextRandom
      requestSeen <- newEmptyMVar; failRequest <- newEmptyMVar
      admissionSeen <- newEmptyMVar; admit <- newEmptyMVar
      let failing _ _ _ = putMVar requestSeen () >> readMVar failRequest >> pure (Left (HTTPStatus 403))
          admission name = if name == "before_control_admission" then putMVar admissionSeen () >> readMVar admit else pure ()
      withRegistry fast {requestOverride = Just failing, transition = admission} $ \registry -> do
        _ <- start registry manager base workspace sid path
        timeout 2000000 (readMVar requestSeen) >>= (`shouldSatisfy` isJustValue)
        reply <- newEmptyMVar
        bracket (forkIO (control registry manager base workspace sid "session_upload_stop" >>= putMVar reply)) killThread $ \_ -> do
          timeout 2000000 (readMVar admissionSeen) >>= (`shouldSatisfy` isJustValue)
          putMVar failRequest ()
          _ <- await registry manager base workspace sid (hasStatus "failed")
          putMVar admit ()
          refused <- timeout 2000000 (readMVar reply)
          refused `shouldSatisfy` maybe False (\value -> field "operation" value == Just Null && field "requested_operation" value == Nothing)
      another <- UUID.nextRandom
      terminalSeen <- newEmptyMVar; publishTerminal <- newEmptyMVar
      let terminalHook name = if name == "before_terminal_publication" then putMVar terminalSeen () >> readMVar publishTerminal else pure ()
      withRegistry fast {requestOverride = Just (\_ _ _ -> pure (Left (HTTPStatus 403))), transition = terminalHook} $ \registry -> do
        _ <- start registry manager base workspace another path
        timeout 2000000 (readMVar terminalSeen) >>= (`shouldSatisfy` isJustValue)
        accepted <- control registry manager base workspace another "session_upload_stop"
        nested ["requested_operation","pending"] accepted `shouldBe` Just (Bool True)
        putMVar publishTerminal ()
        failed <- await registry manager base workspace another (hasStatus "failed")
        nested ["operation","pending"] failed `shouldBe` Just (Bool False)
        nested ["operation","receipt","error"] failed `shouldBe` Just (String "permission_revoked")
      lastSid <- UUID.nextRandom
      finalizing <- newEmptyMVar; publishStop <- newEmptyMVar
      clock <- newTVarIO epoch
      let stopHook name = if name == "stop_finalizing" then atomically (writeTVar clock (addUTCTime 10 epoch)) >> putMVar finalizing () >> readMVar publishStop else pure ()
      withRegistry fast {intervalMicros = 0, now = readTVarIO clock, requestOverride = Just (\_ _ _ -> pure (Right (fakeMetadata lastSid 0))), transition = stopHook} $ \registry -> do
        _ <- start registry manager base workspace lastSid path
        _ <- await registry manager base workspace lastSid (hasStatus "caught_up")
        reply <- newEmptyMVar
        bracket (forkIO (control registry manager base workspace lastSid "session_upload_stop" >>= putMVar reply)) killThread $ \_ -> do
          timeout 2000000 (readMVar finalizing) >>= (`shouldSatisfy` isJustValue)
          stopping <- control registry manager base workspace lastSid "session_upload_status"
          hasStatus "finalizing" stopping `shouldBe` True
          nested ["operation","pending"] stopping `shouldBe` Just (Bool True)
          racingFlush <- control registry manager base workspace lastSid "session_upload_flush"
          field "requested_operation" racingFlush `shouldBe` Nothing
          nested ["operation","id"] racingFlush `shouldBe` nested ["operation","id"] stopping
          putMVar publishStop ()
          stopped <- await registry manager base workspace lastSid (hasStatus "stopped")
          nested ["operation","pending"] stopped `shouldBe` Just (Bool False)
          field "last_ack_at" stopped `shouldBe` Just (toJSON epoch)
          timeout 2000000 (readMVar reply) >>= (`shouldSatisfy` isJustValue)

  it "drains ordinary stdio epochs without awaiting blocked followers and closes the actual handle lifetime" $
    withSourceFile "{}\n" $ \path -> withPipe $ \(input,send) -> withPipe $ \(receive,output) -> withTempHandle $ \errors -> do
      seen <- newTVarIO (0 :: Int)
      release <- newEmptyMVar
      ids <- replicateM 2 UUID.nextRandom
      beginning <- getCurrentTime
      phases <- newTVarIO ([] :: [(Text,NominalDiffTime)])
      let stamp name = do time <- getCurrentTime; atomically $ modifyTVar' phases (<> [(name,diffUTCTime time beginning)])
          receipts = readTVarIO phases >>= putStrLn . ("stdio lifetime phase seconds: " <>) . show
      let call :: Text -> Value -> Value
          call name args = object ["jsonrpc" .= ("2.0" :: Text),"id" .= name,"method" .= ("tools/call" :: Text),"params" .= object ["name" .= name,"arguments" .= args]]
          initial = object ["jsonrpc" .= ("2.0" :: Text),"id" .= ("init" :: Text),"method" .= ("initialize" :: Text)]
          app _ respond = do
            atomically $ modifyTVar' seen (+1)
            readMVar release
            respond (Wai.responseLBS status200 [] "{}")
      testWithApplication (pure app) $ \port -> withManager $ \manager -> do
        workers <- newEmptyMVar
        finished <- newEmptyMVar
        followerIds <- newTVarIO []
        let observe tids = stamp "EOF observed" >> putMVar workers tids
            forkFollower :: ((forall a. IO a -> IO a) -> IO ()) -> IO ThreadId
            forkFollower action = do
              tid <- forkIOWithUnmask $ \unmask ->
                action (\work -> unmask $ do
                  masking <- getMaskingState
                  stamp ("follower worker entry " <> T.pack (show masking))
                  work `finally` stamp "follower worker complete")
              atomically $ modifyTVar' followerIds (<> [tid])
              pure tid
            options = defaultFollowerOptions {forkWorker = forkFollower, transition = stamp, helperExecutable = testHelperExecutable}
            run = try @SomeException (runMCPServerWithHandlesObservedWithFollowerOptions options (\action -> forkIOWithUnmask (\unmask -> unmask action)) observe 2 8 input output errors manager ("http://127.0.0.1:" <> show port) Nothing) >>= putMVar finished
            exchange label request = do
              BL.hPut send (encode request <> "\n"); hFlush send
              reply <- timeout 1000000 (BS.hGetLine receive)
              case reply >>= decodeStrict' of
                Nothing -> expectationFailure ("ordinary response missing: " <> T.unpack label) >> pure Null
                Just value -> do
                  stamp label
                  TE.decodeUtf8 (fromMaybe "" reply) `shouldSatisfy` (not . T.isInfixOf (T.pack path))
                  pure value
        bracket (forkIO run) (\tid -> do _ <- forkIO (killThread tid); pure ()) $ \_ ->
          within 25000000 (do
            stamp "setup complete"
            _ <- exchange "initialize response" initial
            _ <- exchange "workspace response" (call "set_workspace" (object ["workspace_id" .= workspace,"identity" .= identity]))
            forM_ ids $ \sid -> do
              response <- exchange "start response" (call "session_upload_start" (object ["session_id" .= sid,"path" .= path,"format_label" .= ("jsonl" :: Text)]))
              nested ["result","structuredContent","status"] response `shouldBe` Just (String "validating")
            waitUntil $ (>= 2) <$> readTVarIO seen
            stamp "both HTTP slots blocked"
            _ <- exchange "ordinary context response" (call "set_workspace" (object ["workspace_id" .= Null,"identity" .= identity]))
            cleared <- exchange "ordinary get response" (call "get_workspace" (object []))
            let contextValue = do
                  Object result <- field "result" cleared
                  Array content <- KM.lookup "content" result
                  first <- case foldr (:) [] content of Object value : _ -> Just value; _ -> Nothing
                  String encoded <- KM.lookup "text" first
                  decodeStrict' (TE.encodeUtf8 encoded)
            (contextValue >>= field "workspace_id") `shouldBe` Just Null
            cleanupStarted <- getCurrentTime
            stamp "EOF sent"
            hClose send
            tids <- timeout 1000000 (readMVar workers)
            tids `shouldSatisfy` isJustValue
            let joinWorkers = do
                  states <- mapM threadStatus (fromMaybe [] tids)
                  if all (\state -> state == ThreadFinished || state == ThreadDied) states then pure () else threadDelay 10000 >> joinWorkers
            joined <- timeout 2500000 joinWorkers
            joined `shouldSatisfy` isJustValue
            stamp "ordinary workers joined"
            result <- timeout 9500000 (readMVar finished)
            stamp "server returned"
            followerStates <- readTVarIO followerIds >>= mapM threadStatus
            stamp ("follower states " <> T.pack (show followerStates))
            result `shouldSatisfy` maybe False (either (const False) (const True))
            followerStates `shouldSatisfy` all (\state -> state == ThreadFinished || state == ThreadDied)
            openSourceCount `shouldReturn` 0
            nativeProcessCount `shouldReturn` 0
            stamp "native handles closed"
            cleanupFinished <- getCurrentTime
            diffUTCTime cleanupFinished cleanupStarted `shouldSatisfy` (< 9.5)
          ) `finally` (do closed <- hIsClosed send; unless closed (hClose send); putMVar release (); receipts)

fast :: FollowerOptions
fast = defaultFollowerOptions {intervalMicros = 1000, controlWaitMicros = 50000, helperExecutable = testHelperExecutable}

helperProbe :: Text -> BS.ByteString
helperProbe phase = BL.toStrict $ encode $ object ["url" .= ("http://127.0.0.1:0/" :: Text),"method" .= ("GET" :: Text),"headers" .= ([] :: [(Text,Text)]),"body" .= Null,"probe" .= phase]

testHelperExecutable :: IO FilePath
testHelperExecutable = do
  directory <- Paths.getBinDir
  canonicalizePath (directory </> if os == "mingw32" then "hmem-mcp.exe" else "hmem-mcp")

captureTestHTTP :: String -> IO CapturedHTTP
captureTestHTTP base = do
  scope <- newProcessScope
  helper <- testHelperExecutable
  captureHTTP scope helper base Nothing identity
workspace :: UUID
workspace = read "11111111-2222-3333-4444-555555555555"
identity :: BotIdentity
identity = either (error . T.unpack) id (mkBotIdentity "developer" "codex:gpt-6.1-sol:high")

epoch :: UTCTime
epoch = UTCTime (fromGregorian 2026 1 1) 0

fakeMetadata :: UUID -> Int -> Value
fakeMetadata sid count = toJSON (Core.AgentSession sid workspace "jsonl" Core.Active Nothing Nothing Nothing Nothing
  epoch epoch (fromIntegral count) 0 0 True Nothing Nothing Nothing)

queryNumber :: BS.ByteString -> String -> Int
queryNumber name target = fromMaybe 0 $ do
  query <- lookup name (parseQuery (BS8.pack (dropWhile (/= '?') target))) >>= id
  readMaybe (BS8.unpack query)

field :: Text -> Value -> Maybe Value
field name (Object fields) = KM.lookup (Key.fromText name) fields
field _ _ = Nothing
nested :: [Text] -> Value -> Maybe Value
nested keys value = foldl (\previous key -> previous >>= field key) (Just value) keys
contains :: Text -> Value -> Bool
contains text = T.isInfixOf text . TE.decodeUtf8 . BL.toStrict . encode
hasStatus :: Text -> Value -> Bool
hasStatus status value = field "status" value == Just (String status)
hasOffset :: Int -> Value -> Bool
hasOffset count value = field "committed_offset" value == Just (Number (fromIntegral count))
hasReceipt :: Value -> Bool
hasReceipt value = maybe False (/= Null) (field "receipt" value)
receiptHash :: Value -> Maybe Value
receiptHash = nested ["receipt", "sha256"]
isJustValue :: Maybe a -> Bool
isJustValue (Just _) = True
isJustValue _ = False

within :: Int -> IO () -> IO ()
within budget action = timeout budget action >>= (`shouldSatisfy` isJustValue)

withManager :: (Manager -> IO a) -> IO a
withManager = bracket (newManager defaultManagerSettings {managerIdleConnectionCount = 0}) closeManager
withRegistry :: FollowerOptions -> (Registry -> IO a) -> IO a
withRegistry options = bracket (newRegistryWith options) (\registry -> closeRegistry registry >>= (`shouldBe` True))
withSourceFile :: BS.ByteString -> (FilePath -> IO a) -> IO a
withSourceFile bytes action = bracket acquire cleanup action
  where
    acquire = do
      temp <- getTemporaryDirectory
      (path,handle) <- openBinaryTempFile temp "hmem-follower-jsonl"
      BS.hPut handle bytes
      hClose handle
      pure path
    cleanup path = forM_ [path, path <> ".old"] $ \owned -> do
      exists <- doesFileExist owned
      if exists then removeFile owned else pure ()

withTempHandle :: (Handle -> IO a) -> IO a
withTempHandle action = bracket acquire cleanup (action . snd)
  where
    acquire = do directory <- getTemporaryDirectory; openBinaryTempFile directory "hmem-follower-handle"
    cleanup (path,handle) = do
      closed <- hIsClosed handle
      unless closed (hClose handle)
      removeFile path

withPipe :: ((Handle,Handle) -> IO a) -> IO a
withPipe = bracket (do pair@(readHandle,writeHandle) <- createPipe; hSetBinaryMode readHandle True; hSetBinaryMode writeHandle True; pure pair)
  (\(readHandle,writeHandle) -> forM_ [writeHandle,readHandle] $ \handle -> hIsClosed handle >>= \closed -> unless closed (hClose handle))

payload :: Value -> Value
payload result = maybe Null id (field "structuredContent" result)
start :: Registry -> Manager -> String -> UUID -> UUID -> FilePath -> IO Value
start registry manager base wid sid path = payload <$> handleUploadTool registry manager base Nothing identity "session_upload_start"
  (object ["workspace_id" .= wid, "session_id" .= sid, "path" .= path, "format_label" .= ("jsonl" :: Text)])
control :: Registry -> Manager -> String -> UUID -> UUID -> Text -> IO Value
control registry manager base wid sid name = payload <$> handleUploadTool registry manager base Nothing identity name
  (object ["workspace_id" .= wid,"session_id" .= sid])
await :: Registry -> Manager -> String -> UUID -> UUID -> (Value -> Bool) -> IO Value
await registry manager base wid sid predicate = do
  result <- timeout 8000000 loop
  case result of
    Just value -> pure value
    Nothing -> do
      current <- control registry manager base wid sid "session_upload_status"
      expectationFailure ("Follower deadline: " <> show current)
      pure current
  where
    loop = do
      current <- control registry manager base wid sid "session_upload_status"
      if predicate current then pure current else threadDelay 10000 >> loop
waitUntil :: IO Bool -> IO ()
waitUntil condition = do
  result <- timeout 3000000 loop
  result `shouldSatisfy` isJustValue
  where loop = do ready <- condition; unless ready (threadDelay 10000 >> loop)
rpc :: Registry -> Manager -> String -> TVar Bool -> TVar (Maybe MCPContext) -> Text -> Value -> IO (Maybe Value)
rpc registry manager base initialized context name arguments = handleStdioLineWithRegistry registry manager base Nothing initialized context (BL.toStrict (encode
  (object ["jsonrpc" .= ("2.0" :: Text), "id" .= ("follower" :: Text), "method" .= ("tools/call" :: Text), "params" .= object ["name" .= name,"arguments" .= arguments]])))

withBackend :: (TestEnv -> UUID -> Manager -> String -> IO a) -> IO a
withBackend action = withTestEnv $ \env -> do
  tracker <- newAccessTracker env.pool 3600
  websocket <- newWSState
  let cfg = Config.defaultConfig {Config.auth = Config.defaultConfig.auth {Config.mode = Config.AuthModeDeployed, Config.enabled = False}}
      principal = Principal ActorBot "follower-tests" "Follower test" PrincipalSyntheticLocalSuperadmin
      authorized app req respond = withPrincipalContext (Just principal) (app req respond)
  app <- mkAppWithChangeStream cfg.changeStream authorized cfg.auth cfg.cors cfg.rateLimit env.pool tracker websocket Nothing True
  wid <- (.id) <$> createTestWorkspace env "follower-tests"
  testWithApplication (pure app) $ \port -> withManager $ \manager -> action env wid manager ("http://127.0.0.1:" <> show port)
