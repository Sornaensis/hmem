{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module HttpProcessSpec (spec, crashDriver) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, cancel, waitCatch)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, readMVar, takeMVar)
import Control.Exception (SomeException, bracket, evaluate, finally, try)
import Control.Monad (void, when)
import Data.Char (toLower)
import Data.List (isInfixOf)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as LBS
import Data.Default.Class (def)
import Data.IORef (newIORef, readIORef, modifyIORef')
import Data.Word (Word64)
import Foreign.C.Types (CInt (..))
import GHC.Clock (getMonotonicTimeNSec)
import Network.Socket
  ( AddrInfo (addrFlags), AddrInfoFlag (AI_NUMERICHOST), SocketOption (ReuseAddr)
  , accept, bind, close, defaultHints, getAddrInfo
  , getSocketName, listen, setSocketOption, socket, socketToHandle
  , withSocketsDo, addrAddress, addrFamily, addrProtocol, addrSocketType
  , SockAddr (SockAddrInet)
  )
import Network.TLS (Backend (..), Credentials (..), bye, contextNew, credentialLoadX509FromMemory,
                    defaultParamsServer, handshake, recvData, sendData,
                    serverShared, sharedCredentials)
import System.Directory (canonicalizePath, getTemporaryDirectory, removeFile)
import System.Environment (getEnv, getExecutablePath, lookupEnv, setEnv, unsetEnv)
import System.FilePath ((</>), takeDirectory)
import System.Info (os)
import System.IO (Handle, IOMode (ReadWriteMode), hClose, hFlush,
                  hGetLine, hSetBinaryMode, openBinaryTempFile, stdout)
import System.Process (CreateProcess (std_out), StdStream (CreatePipe),
                       createProcess, proc, terminateProcess, waitForProcess)
import System.Timeout (timeout)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe,
                   shouldReturn, shouldSatisfy)

import HMem.Embedding.HttpProtocol
import HMem.Embedding.HttpProcess

foreign import ccall unsafe "hmem_process_id_alive"
  processIdAlive :: CInt -> IO CInt
foreign import ccall unsafe "hmem_test_spawn_knob_visible"
  testSpawnKnobVisible :: IO CInt

spec :: Spec
spec = describe "bounded helper protocol" $ do
  it "round-trips one bounded request" $ do
    let call = HttpCall HttpPost "http://127.0.0.1:1/embed" "{}" 65536
    (decodeCall =<< maybe (Left InvalidFrame) Right (encodeCall call)) `shouldBe` Right call
  it "rejects oversized and incomplete lengths before payload allocation" $ do
    parseFrameHeader responseFrameCap "\255\255\255\255" `shouldBe` Nothing
    decodeWireReply "\1\0\0\200\0\0\0\5x" `shouldBe` Left InvalidFrame
    encodeCall (HttpCall HttpPost "http://x" (BS.replicate (512 * 1024 + 1) 97) 1)
      `shouldBe` Nothing
    encodeCall (HttpCall HttpPost "http://x" (BS.replicate 200000 97) 65536)
      `shouldSatisfy` (/= Nothing)

  describe "owned real helper process" $ do
    it "returns a bounded local socket response and acknowledges exit" $ do
      executable <- helperPath
      let policy = SessionPolicy executable (const (pure ())) (const (pure ()))
      withServer (\handle -> do
          receiveHeaders handle
          reply handle 200 "{\"value\":1}") $ \port -> do
        deadline <- deadlineAfter 3000
        result <- withSession policy deadline $ \session -> do
          pid <- sessionProcessId session
          answer <- callHttp policy session (HttpCall HttpGet (url port) BS.empty 65536)
          pure (pid, answer)
        case result of
          Right (pid, Right answer) -> do
            replyStatus answer `shouldBe` 200
            replyBody answer `shouldBe` "{\"value\":1}"
            processIdAlive (fromIntegral pid) `shouldReturn` 0
          other -> expectationFailure ("local helper success failed: " ++ show other)

    it "retires escaped sessions before native process memory is released" $ do
      executable <- helperPath
      escaped <- newEmptyMVar
      let policy = SessionPolicy executable (const (pure ())) (const (pure ()))
      deadline <- deadlineAfter 2000
      result <- withSession policy deadline $ \session -> do
        putMVar escaped session
        sessionProcessId session
      session <- takeMVar escaped
      case result of
        Right pid -> do
          sessionProcessId session `shouldReturn` pid
          callHttp policy session (HttpCall HttpGet "http://127.0.0.1:1/closed" BS.empty 1)
            `shouldReturn` Left ProcessOwnershipFailure
          processIdAlive (fromIntegral pid) `shouldReturn` 0
        other -> expectationFailure ("escaped session setup: " ++ show other)

    it "joins a concurrent call before retiring and freeing its process" $ do
      executable <- helperPath
      accepted <- newEmptyMVar
      let policy = SessionPolicy executable (const (pure ())) (const (pure ()))
      withServer (\handle -> receiveHeaders handle >> putMVar accepted () >>
                             threadDelay 1000000) $ \port -> do
        deadline <- deadlineAfter 400
        result <- withHttpSession (const (pure ())) policy deadline $ \session -> do
          worker <- async $ callHttp policy session
            (HttpCall HttpGet (url port) BS.empty 65536)
          takeMVar accepted
          pure (session, worker)
        case result of
          Right (session, worker) -> do
            outcome <- waitCatch worker
            outcome `shouldSatisfy` either (const False) (const True)
            callHttp policy session (HttpCall HttpGet (url port) BS.empty 1)
              `shouldReturn` Left ProcessOwnershipFailure
            pid <- sessionProcessId session
            processIdAlive (fromIntegral pid) `shouldReturn` 0
          Left problem -> expectationFailure ("concurrent close: " ++ show problem)

    it "kills and reaps a blocked header read before the absolute deadline" $ do
      executable <- helperPath
      pidReady <- newEmptyMVar
      let policy = SessionPolicy executable (const (pure ())) (const (pure ()))
      withServer (\handle -> receiveHeaders handle >> threadDelay 700000 >>
                             reply handle 200 "{}") $ \port -> do
        deadline <- deadlineAfter 350
        started <- getMonotonicTimeNSec
        result <- withSession policy deadline $ \session -> do
          pid <- sessionProcessId session
          putMVar pidReady pid
          answer <- callHttp policy session (HttpCall HttpGet (url port) BS.empty 65536)
          pure (pid, answer)
        elapsed <- getMonotonicTimeNSec
        result `shouldBe` Left DeadlineExceeded
        pid <- takeMVar pidReady
        processIdAlive (fromIntegral pid) `shouldReturn` 0
        fromIntegral (elapsed - started) / (1000000 :: Double)
          `shouldSatisfy` (< 600)

    it "kills and reaps a blocked body read without a later overlap" $ do
      executable <- helperPath
      pidReady <- newEmptyMVar
      let policy = SessionPolicy executable (const (pure ())) (const (pure ()))
      withServer (\handle -> do
          receiveHeaders handle
          BS.hPut handle "HTTP/1.1 200 OK\r\nContent-Length: 10\r\n\r\nabc"
          hFlush handle
          threadDelay 700000) $ \port -> do
        deadline <- deadlineAfter 350
        result <- withSession policy deadline $ \session -> do
          pid <- sessionProcessId session
          putMVar pidReady pid
          answer <- callHttp policy session (HttpCall HttpGet (url port) BS.empty 65536)
          pure (pid, answer)
        result `shouldBe` Left DeadlineExceeded
        pid <- takeMVar pidReady
        processIdAlive (fromIntegral pid) `shouldReturn` 0

    it "propagates cancellation after joining a blocked parent pipe reader" $ do
      executable <- helperPath
      accepted <- newEmptyMVar
      let policy = SessionPolicy executable (const (pure ())) (const (pure ()))
      withServer (\handle -> do
          receiveHeaders handle
          putMVar accepted ()
          threadDelay 700000) $ \port -> do
        deadline <- deadlineAfter 3000
        pidReady <- newEmptyMVar
        running <- async $ withSession policy deadline $ \session -> do
          putMVar pidReady =<< sessionProcessId session
          callHttp policy session (HttpCall HttpGet (url port) BS.empty 65536)
        pid <- takeMVar pidReady
        takeMVar accepted
        cancel running
        outcome <- waitCatch running
        outcome `shouldSatisfy` either (const True) (const False)
        processIdAlive (fromIntegral pid) `shouldReturn` 0

    it "uses the post-response hook only for successful HTTP status" $ do
      executable <- helperPath
      count <- newIORef (0 :: Int)
      let policy = SessionPolicy executable (const (pure ()))
                                  (\_ -> modifyIORef' count (+ 1))
      withServer (\handle -> receiveHeaders handle >> reply handle 429 "retry") $ \port -> do
        deadline <- deadlineAfter 3000
        result <- withSession policy deadline $ \session ->
          callHttp policy session (HttpCall HttpGet (url port) BS.empty 65536)
        case result of
          Right (Right answer) -> replyStatus answer `shouldBe` 429
          other -> expectationFailure ("429 outcome: " ++ show other)
        readIORef count `shouldReturn` 0

    it "preserves a live helper for a parent-controlled retry after a network failure" $ do
      executable <- helperPath
      let policy = SessionPolicy executable (const (pure ())) (const (pure ()))
      withServer (\handle -> receiveHeaders handle) $ \badPort ->
        withServer (\handle -> receiveHeaders handle >> reply handle 200 "recovered") $ \goodPort -> do
          deadline <- deadlineAfter 3000
          result <- withSession policy deadline $ \session -> do
            firstPid <- sessionProcessId session
            first <- callHttp policy session (HttpCall HttpGet (url badPort) BS.empty 65536)
            secondPid <- sessionProcessId session
            second <- callHttp policy session (HttpCall HttpGet (url goodPort) BS.empty 65536)
            pure (firstPid, first, secondPid, second)
          case result of
            Right (firstPid, Left (ChildFailure NetworkFailure), secondPid, Right answer) -> do
              secondPid `shouldBe` firstPid
              replyStatus answer `shouldBe` 200
              replyBody answer `shouldBe` "recovered"
              processIdAlive (fromIntegral firstPid) `shouldReturn` 0
            other -> expectationFailure ("retry in same helper failed: " ++ show other)

    it "times out and reaps when a real successful response hook blocks" $ do
      executable <- helperPath
      received <- newEmptyMVar
      pidReady <- newEmptyMVar
      release <- newEmptyMVar
      let policy = SessionPolicy executable (const (pure ()))
                                  (\_ -> putMVar received () >> readMVar release)
      withServer (\handle -> receiveHeaders handle >> reply handle 200 "{}") $ \port -> do
        deadline <- deadlineAfter 400
        result <- withSession policy deadline $ \session -> do
          pid <- sessionProcessId session
          putMVar pidReady pid
          answer <- callHttp policy session (HttpCall HttpGet (url port) BS.empty 65536)
          pure (pid, answer)
        result `shouldBe` Left DeadlineExceeded
        pid <- takeMVar pidReady
        processIdAlive (fromIntegral pid) `shouldReturn` 0
        -- Receipt is only posted by the parent-side hook after the child returned 2xx.
        void (takeMVar received)

    it "rejects a declared body over the caller cap before reading it" $ do
      executable <- helperPath
      let policy = SessionPolicy executable (const (pure ())) (const (pure ()))
      withServer (\handle -> do
          receiveHeaders handle
          BS.hPut handle "HTTP/1.1 200 OK\r\nContent-Length: 200000\r\n\r\n"
          hFlush handle
          threadDelay 500000) $ \port -> do
        deadline <- deadlineAfter 2000
        result <- withSession policy deadline $ \session ->
          callHttp policy session (HttpCall HttpGet (url port) BS.empty 65536)
        result `shouldBe` Right (Left (ChildFailure BodyTooLarge))

    it "reaps a child that exits before the ready frame" $ do
      executable <- earlyExitPath
      pidReady <- newEmptyMVar
      let policy = SessionPolicy executable (putMVar pidReady) (const (pure ()))
      deadline <- deadlineAfter 2000
      result <- withSession policy deadline (const (pure ()))
      result `shouldBe` Left PipeFailure
      pid <- takeMVar pidReady
      processIdAlive (fromIntegral pid) `shouldReturn` 0

    it "cancels during the startup hook and joins ownership before returning" $ do
      executable <- helperPath
      pidReady <- newEmptyMVar
      release <- newEmptyMVar
      let policy = SessionPolicy executable
            (\pid -> putMVar pidReady pid >> readMVar release)
            (const (pure ()))
      deadline <- deadlineAfter 3000
      running <- async $ withSession policy deadline (const (pure ()))
      pid <- takeMVar pidReady
      cancel running
      outcome <- waitCatch running
      outcome `shouldSatisfy` either (const True) (const False)
      processIdAlive (fromIntegral pid) `shouldReturn` 0
      -- A subsequent helper can start after the cancelled startup has closed
      -- its process and pipe reader, including on the minimum deadline path.
      next <- deadlineAfter 1000
      withSession (SessionPolicy executable (const (pure ())) (const (pure ())))
                      next (const (pure ())) `shouldReturn` Right ()

    it "preserves environment proxy routing with a local proxy fixture" $ do
      executable <- helperPath
      let policy = SessionPolicy executable (const (pure ())) (const (pure ()))
      withServer (\handle -> receiveHeaders handle >> reply handle 200 "proxy") $ \port ->
        withEnv "http_proxy" ("http://127.0.0.1:" ++ show port) $
        withEnv "no_proxy" "" $ withEnv "NO_PROXY" "" $ do
          deadline <- deadlineAfter 3000
          result <- withSession policy deadline $ \session ->
            callHttp policy session (HttpCall HttpGet "http://example.invalid/resource"
                                      BS.empty 65536)
          case result of
            Right (Right answer) -> do
              replyStatus answer `shouldBe` 200
              replyBody answer `shouldBe` "proxy"
            other -> expectationFailure ("proxy routing failed: " ++ show other)

    it "rejects a plaintext peer on an HTTPS URL" $ do
      executable <- helperPath
      let policy = SessionPolicy executable (const (pure ())) (const (pure ()))
      withServer (\handle -> do
          _ <- BS.hGetSome handle 4096
          BS.hPut handle "HTTP/1.1 200 OK\r\nContent-Length: 2\r\n\r\n{}"
          hFlush handle) $ \port -> do
        deadline <- deadlineAfter 3000
        let secureUrl = C8.pack ("https://127.0.0.1:" ++ show port ++ "/embed")
        result <- withSession policy deadline $ \session ->
          callHttp policy session (HttpCall HttpGet secureUrl BS.empty 65536)
        result `shouldBe` Right (Left (ChildFailure NetworkFailure))

    it "rejects an untrusted certificate from a controlled TLS server" $ do
      executable <- helperPath
      let policy = SessionPolicy executable (const (pure ())) (const (pure ()))
      (_, evidence) <- withTlsServerEvidence $ \port -> do
        deadline <- deadlineAfter 3000
        result <- withSession policy deadline $ \session ->
          callHttp policy session (HttpCall HttpGet (tlsUrl "localhost" port) BS.empty 65536)
        result `shouldBe` Right (Left (ChildFailure CertificateRejected))
      evidence `shouldSatisfy` isCertificateRejectionEvidence

    when (os == "linux") $ do
      it "accepts a valid localhost TLS response with a scoped trusted CA" $ do
        executable <- helperPath
        let policy = SessionPolicy executable (const (pure ())) (const (pure ()))
        withTemporaryTestCertificate $ \certificatePath ->
          withEnv "SYSTEM_CERTIFICATE_PATH" certificatePath $
            withTlsServer $ \port -> do
              deadline <- deadlineAfter 3000
              result <- withSession policy deadline $ \session ->
                callHttp policy session
                  (HttpCall HttpGet (tlsUrl "localhost" port) BS.empty 65536)
              case result of
                Right (Right answer) -> do
                  replyStatus answer `shouldBe` 200
                  replyBody answer `shouldBe` "tls-ok"
                other -> expectationFailure ("trusted TLS: " ++ show other)

      it "rejects a wrong hostname even when its certificate is trusted" $ do
        executable <- helperPath
        let policy = SessionPolicy executable (const (pure ())) (const (pure ()))
        withTemporaryTestCertificate $ \certificatePath ->
          withEnv "SYSTEM_CERTIFICATE_PATH" certificatePath $ do
            (_, evidence) <- withTlsServerEvidence $ \port -> do
              deadline <- deadlineAfter 3000
              result <- withSession policy deadline $ \session ->
                callHttp policy session
                  (HttpCall HttpGet (tlsUrl "127.0.0.1" port) BS.empty 65536)
              result `shouldBe` Right (Left (ChildFailure CertificateRejected))
            evidence `shouldSatisfy` isCertificateRejectionEvidence

    it "kills a stalled TLS handshake within the original deadline" $ do
      executable <- helperPath
      pidReady <- newEmptyMVar
      let policy = SessionPolicy executable (putMVar pidReady) (const (pure ()))
      withServer (\handle -> do
          _ <- BS.hGetSome handle 4096 -- ClientHello, but no ServerHello.
          threadDelay 800000) $ \port -> do
        deadline <- deadlineAfter 350
        result <- withSession policy deadline $ \session ->
          callHttp policy session
            (HttpCall HttpGet (tlsUrl "127.0.0.1" port) BS.empty 65536)
        result `shouldBe` Left DeadlineExceeded
        pid <- takeMVar pidReady
        processIdAlive (fromIntegral pid) `shouldReturn` 0

    it "kills an active helper after its owning parent process dies" $ do
      executable <- helperPath
      accepted <- newEmptyMVar
      withServer (\handle -> receiveHeaders handle >> putMVar accepted () >>
                             threadDelay 2000000) $ \port -> do
        pid <- crashDriverFromTest executable (C8.unpack (url port)) "active"
               (takeMVar accepted)
        processIdAlive (fromIntegral pid) `shouldReturn` 0

    it "kills a helper when its owning parent dies before readiness" $ do
      executable <- helperPath
      pid <- crashDriverFromTest executable "http://127.0.0.1:1/unused" "startup"
             (pure ())
      processIdAlive (fromIntegral pid) `shouldReturn` 0

    when (os == "linux") $
      it "kills a stopped pre-exec child if its creator dies before Main can start" $ do
        executable <- canonicalizePath =<< getExecutablePath
        withEnv "HMEM_HTTP_TEST_STOP_PRE_EXEC" "1" $ do
          pid <- crashDriverFromTest executable "http://127.0.0.1:1/unused"
                   "startup" (pure ())
          processIdAlive (fromIntegral pid) `shouldReturn` 0

    it "refuses work when the original deadline expired before acquisition" $ do
      executable <- helperPath
      pidReady <- newEmptyMVar
      let policy = SessionPolicy executable (putMVar pidReady) (const (pure ()))
      deadline <- deadlineAfter 0
      withSession policy deadline (const (pure ()))
        `shouldReturn` Left DeadlineExceeded
      timeout 10000 (takeMVar pidReady) `shouldReturn` Nothing

    it "owns a child acquired after the deadline until it is reaped" $ do
      executable <- canonicalizePath =<< getExecutablePath
      let policy = SessionPolicy executable (const (pure ())) (const (pure ()))
      withTemporaryPidMarker $ \marker ->
        withEnv "HMEM_HTTP_FAKE_HELPER_MODE" "no-read" $
          withEnv "HMEM_HTTP_TEST_SPAWN_PID_FILE" marker $
            withEnv "HMEM_HTTP_TEST_SPAWN_DELAY_MS" "400" $ do
              testSpawnKnobVisible `shouldReturn` 1
              deadline <- deadlineAfter 150
              started <- getMonotonicTimeNSec
              outcome <- withSession policy deadline (const (pure ()))
              ended <- getMonotonicTimeNSec
              markerBytes <- BS.readFile marker
              let elapsedMs = fromIntegral (ended - started) / (1000000 :: Double)
              when (outcome /= Left DeadlineExceeded) $
                expectationFailure ("late spawn: outcome=" ++ show outcome ++
                  " elapsed_ms=" ++ show elapsedMs ++
                  " pid_marker=" ++ show markerBytes)
              pid <- readPidMarker marker
              processIdAlive (fromIntegral pid) `shouldReturn` 0
              elapsedMs `shouldSatisfy` (>= 350)

    it "bounds generic session callback work under the original deadline" $ do
      executable <- helperPath
      pidReady <- newEmptyMVar
      let policy = SessionPolicy executable (putMVar pidReady) (const (pure ()))
      deadline <- deadlineAfter 250
      result <- withSession policy deadline $ \_ -> threadDelay 700000
      result `shouldBe` Left DeadlineExceeded
      pid <- takeMVar pidReady
      processIdAlive (fromIntegral pid) `shouldReturn` 0

    it "forces nested callback output before returning the session result" $ do
      executable <- helperPath
      let policy = SessionPolicy executable (const (pure ())) (const (pure ()))
          unforced = [1 :: Int, error "nested result was not forced"]
      deadline <- deadlineAfter 2000
      outcome <- try (withHttpSession (\values -> void (evaluate (sum values)))
                       policy deadline (const (pure unforced)))
                 :: IO (Either SomeException (Either SessionFailure [Int]))
      case outcome of
        Right (Left PipeFailure) -> pure ()
        other -> expectationFailure ("nested result was not forced: " ++ show other)

    it "interrupts CPU-bound callback work and joins its worker" $ do
      executable <- helperPath
      pidReady <- newEmptyMVar
      let policy = SessionPolicy executable (putMVar pidReady) (const (pure ()))
          burn values = burn ((0 :: Int) : values)
      deadline <- deadlineAfter 250
      outcome <- withHttpSession (\values -> void (evaluate (burn values)))
                   policy deadline (const (pure ([] :: [Int])))
      outcome `shouldBe` Left DeadlineExceeded
      pid <- takeMVar pidReady
      processIdAlive (fromIntegral pid) `shouldReturn` 0

    it "ignores a second cancellation until process reap and pipe joins finish" $ do
      executable <- canonicalizePath =<< getExecutablePath
      pidReady <- newEmptyMVar
      let policy = SessionPolicy executable (putMVar pidReady) (const (pure ()))
          largeCall = HttpCall HttpPost "http://127.0.0.1:1/no-read"
                       (BS.replicate 200000 97) 65536
      withEnv "HMEM_HTTP_FAKE_HELPER_MODE" "no-read" $
        withEnv "HMEM_HTTP_TEST_REAP_DELAY_MS" "350" $ do
          deadline <- deadlineAfter 3000
          running <- async $ withSession policy deadline $ \session ->
            callHttp policy session largeCall
          pid <- takeMVar pidReady
          started <- getMonotonicTimeNSec
          first <- async (cancel running)
          threadDelay 70000
          second <- async (cancel running)
          void (waitCatch first)
          void (waitCatch second)
          outcome <- waitCatch running
          ended <- getMonotonicTimeNSec
          outcome `shouldSatisfy` either (const True) (const False)
          fromIntegral (ended - started) / (1000000 :: Double)
            `shouldSatisfy` (>= 300)
          processIdAlive (fromIntegral pid) `shouldReturn` 0
          next <- deadlineAfter 1000
          withSession (SessionPolicy executable (const (pure ())) (const (pure ())))
            next (const (pure ())) `shouldReturn` Right ()

    it "reserves shutdown time with the minimum 100 ms logical deadline" $ do
      executable <- helperPath
      let policy = SessionPolicy executable (const (threadDelay 500000)) (const (pure ()))
      started <- getMonotonicTimeNSec
      deadline <- deadlineAfter 100
      result <- withSession policy deadline (const (pure ()))
      ended <- getMonotonicTimeNSec
      result `shouldBe` Left DeadlineExceeded
      fromIntegral (ended - started) / (1000000 :: Double) `shouldSatisfy` (< 300)

    it "terminates a child while a parent pipe writer is physically blocked" $ do
      executable <- canonicalizePath =<< getExecutablePath
      pidReady <- newEmptyMVar
      let policy = SessionPolicy executable (const (pure ())) (const (pure ()))
          largeCall = HttpCall HttpPost "http://127.0.0.1:1/no-read"
                       (BS.replicate 200000 97) 65536
      withEnv "HMEM_HTTP_FAKE_HELPER_MODE" "no-read" $ do
        deadline <- deadlineAfter 350
        result <- withSession policy deadline $ \session -> do
          pid <- sessionProcessId session
          putMVar pidReady pid
          answer <- callHttp policy session largeCall
          pure (pid, answer)
        result `shouldBe` Left DeadlineExceeded
        pid <- takeMVar pidReady
        processIdAlive (fromIntegral pid) `shouldReturn` 0

    it "reaps children that send malformed or oversized physical frames" $ do
      executable <- canonicalizePath =<< getExecutablePath
      let policy = SessionPolicy executable (const (pure ())) (const (pure ()))
          smallCall = HttpCall HttpGet "http://127.0.0.1:1/fake" BS.empty 65536
      mapM_ (\(mode, expected) -> withEnv "HMEM_HTTP_FAKE_HELPER_MODE" mode $ do
          deadline <- deadlineAfter 2000
          result <- withSession policy deadline $ \session -> do
            pid <- sessionProcessId session
            answer <- callHttp policy session smallCall
            pure (pid, answer)
          case result of
            Right (pid, Left problem) -> do
              problem `shouldBe` expected
              processIdAlive (fromIntegral pid) `shouldReturn` 0
            other -> expectationFailure ("physical frame " ++ mode ++ ": " ++ show other)
        ) [("malformed-frame", ProtocolViolation), ("oversized-frame", PipeFailure)]

    it "drains bounded stderr and fails closed on overflow" $ do
      executable <- canonicalizePath =<< getExecutablePath
      pidReady <- newEmptyMVar
      let policy = SessionPolicy executable (putMVar pidReady) (const (pure ()))
      withEnv "HMEM_HTTP_FAKE_HELPER_MODE" "stderr-flood" $ do
        deadline <- deadlineAfter 2000
        result <- withSession policy deadline (const (pure ()))
        result `shouldBe` Left PipeFailure
        pid <- takeMVar pidReady
        processIdAlive (fromIntegral pid) `shouldReturn` 0

    it "rejects an invalid helper before partial acquisition" $ do
      deadline <- deadlineAfter 1000
      let policy = SessionPolicy "relative-helper" (const (pure ())) (const (pure ()))
      result <- withSession policy deadline (const (pure ()))
      result `shouldBe` Left InvalidHelperPath

crashDriver :: FilePath -> String -> String -> IO ()
crashDriver executable endpoint mode = do
  let notify pid = do
        putStrLn (show pid)
        hFlush stdout
        if mode == "startup" then threadDelay 10000000 else pure ()
      policy = SessionPolicy executable notify (const (pure ()))
  deadline <- deadlineAfter 15000
  void $ withSession policy deadline $ \session ->
    if mode == "active"
    then void $ callHttp policy session
           (HttpCall HttpGet (C8.pack endpoint) BS.empty 65536)
    else pure ()

-- Test results have Show instances except for the deliberately escaped
-- HttpSession handle. Traversing the rendered value forces nested test data.
withSession :: Show a => SessionPolicy -> Word64 -> (HttpSession -> IO a)
            -> IO (Either SessionFailure a)
withSession = withHttpSession (\value -> void (evaluate (length (show value))))

crashDriverFromTest :: FilePath -> String -> String -> IO () -> IO Int
crashDriverFromTest executable endpoint mode afterStart = do
  testExecutable <- getExecutablePath
  (_, outputMaybe, _, process) <- createProcess
    (proc testExecutable ["--parent-crash-driver", executable, endpoint, mode])
      { std_out = CreatePipe }
  case outputMaybe of
    Nothing -> expectationFailure "missing parent-crash driver output" >> pure 0
    Just output -> do
      line <- timeout 3000000 (hGetLine output)
      case line of
        Nothing -> do
          terminateProcess process
          void (waitForProcess process)
          hClose output
          expectationFailure "parent-crash driver did not publish owned child PID"
          pure 0
        Just value -> do
          afterStart
          terminateProcess process
          void (waitForProcess process)
          hClose output
          let pid = read value
          gone <- timeout 1500000 (waitUntilGone pid)
          gone `shouldBe` Just ()
          pure pid

waitUntilGone :: Int -> IO ()
waitUntilGone pid = do
  alive <- processIdAlive (fromIntegral pid)
  if alive == 0 then pure () else threadDelay 10000 >> waitUntilGone pid

earlyExitPath :: IO FilePath
earlyExitPath = if os == "mingw32"
  then do
    root <- getEnv "SystemRoot"
    canonicalizePath (root </> "System32" </> "where.exe")
  else canonicalizePath "/bin/false"

withEnv :: String -> String -> IO a -> IO a
withEnv name value action = bracket (lookupEnv name)
  (\old -> maybe (unsetEnv name) (setEnv name) old)
  (\_ -> setEnv name value >> action)

helperPath :: IO FilePath
helperPath = do
  testExecutable <- getExecutablePath
  let buildDir = takeDirectory (takeDirectory testExecutable)
      name = "hmem-embedding-http-helper"
      suffix = if os == "mingw32" then ".exe" else ""
  canonicalizePath (buildDir </> name </> (name ++ suffix))

deadlineAfter :: Integer -> IO Word64
deadlineAfter milliseconds = do
  now <- getMonotonicTimeNSec
  pure (now + fromIntegral milliseconds * 1000000)

url :: Int -> BS.ByteString
url port = C8.pack ("http://127.0.0.1:" ++ show port ++ "/embed")

tlsUrl :: String -> Int -> BS.ByteString
tlsUrl host port = C8.pack ("https://" ++ host ++ ":" ++ show port ++ "/embed")

withTemporaryTestCertificate :: (FilePath -> IO a) -> IO a
withTemporaryTestCertificate use = do
  directory <- getTemporaryDirectory
  bracket (do
      (path, handle) <- openBinaryTempFile directory "hmem-http-helper-local-ca.pem"
      BS.hPut handle tlsTestCertificate
      hClose handle
      pure path)
    removeFile use

withTemporaryPidMarker :: (FilePath -> IO a) -> IO a
withTemporaryPidMarker use = do
  directory <- getTemporaryDirectory
  bracket (do
      (path, handle) <- openBinaryTempFile directory "hmem-http-helper-pid"
      hClose handle
      pure path)
    removeFile use

readPidMarker :: FilePath -> IO Int
readPidMarker path = do
  bytes <- BS.readFile path
  case reads (C8.unpack bytes) of
    [(pid, _)] -> pure pid
    _ -> threadDelay 10000 >> readPidMarker path

withTlsServer :: (Int -> IO a) -> IO a
withTlsServer use = fst <$> withTlsServerEvidence use

-- Preserve the server-side TLS alert, so a negative case proves certificate
-- validation was reached instead of accepting any generic connection error.
withTlsServerEvidence :: (Int -> IO a) -> IO (a, Maybe (Either String (), Int))
withTlsServerEvidence use = do
  credential <- either (\problem -> expectationFailure problem >> error "TLS credential")
                       pure
                       (credentialLoadX509FromMemory tlsTestCertificate tlsTestPrivateKey)
  seen <- newEmptyMVar
  sent <- newIORef (0 :: Int)
  withServer (\handle -> do
      let backend = Backend
            { backendFlush = hFlush handle
            , backendClose = pure ()
            , backendSend = \bytes -> do
                modifyIORef' sent (+ BS.length bytes)
                BS.hPut handle bytes
            , backendRecv = BS.hGetSome handle
            }
      context <- contextNew backend
        (defaultParamsServer { serverShared = def
          { sharedCredentials = Credentials [credential] } })
      handshaken <- try (handshake context) :: IO (Either SomeException ())
      sentBytes <- readIORef sent
      putMVar seen (either (Left . show) Right handshaken, sentBytes)
      case handshaken of
        Left _ -> pure ()
        Right () -> do
          _ <- recvData context
          sendData context (LBS.fromStrict
            "HTTP/1.1 200 OK\r\nContent-Length: 6\r\nConnection: close\r\n\r\ntls-ok")
          bye context) $ \port -> do
    result <- use port
    evidence <- timeout 1000000 (takeMVar seen)
    pure (result, evidence)

isCertificateRejectionEvidence :: Maybe (Either String (), Int) -> Bool
isCertificateRejectionEvidence (Just (Left message, sentBytes)) =
  let detail = map toLower message
      rejected = any (`isInfixOf` detail)
        ["certificate", "unknown_ca", "bad_certificate", "unknown ca",
         "tcp_terminate"]
  -- The server has emitted more than its small ServerHello flight. With this
  -- single-certificate fixture that means it transmitted the certificate
  -- flight before the peer rejected/closed the TLS handshake.
  in rejected && sentBytes >= 1024
isCertificateRejectionEvidence _ = False

-- A test-only localhost certificate and key. The private key is never passed to
-- the transport helper, written to disk, or used outside the loopback fixture.
tlsTestCertificate :: BS.ByteString
tlsTestCertificate = C8.unlines
  [ "-----BEGIN CERTIFICATE-----"
  , "MIIDHzCCAgegAwIBAgIUZLiGv6YbFdvWemxvoDVhRI8iHBUwDQYJKoZIhvcNAQEL"
  , "BQAwFDESMBAGA1UEAwwJbG9jYWxob3N0MB4XDTI2MDkxOTE4NTIwNloXDTM2MDkx"
  , "NjE4NTIwNlowFDESMBAGA1UEAwwJbG9jYWxob3N0MIIBIjANBgkqhkiG9w0BAQEF"
  , "AAOCAQ8AMIIBCgKCAQEAwr3WlibFFxeBlOXCZfuc9WiLIoFFE7c7li+YbzIySzib"
  , "1C44js8vsI4c66TkaADocahS6xWlzXIe85pH2XOHxgOBOPHqrBI8bEGV8oudIbyJ"
  , "3Mkj57pT5HXT6FAjHNQuJ2Zzp8tcoeE7d2aEeTqjW/VJ4a4kvUY15d9tRDRgLJuH"
  , "EEsM4QO70tATggK4Zfb+Aokez9afq1qur1AYJPbzOHP7ic1H/OKtoOrJIVF1t32R"
  , "IfK/VbCZn+tEyTRGwgZBtGrfPxL2m2P3mTvakGdRWn+piviAxGE5JkulMIT86XkF"
  , "BkXdtt4YPez89sgSP+gyek9DGZWfu+4F6VdTcNWLfQIDAQABo2kwZzAdBgNVHQ4E"
  , "FgQUtZ1PQYq+c36r/qC9slPLUl/legEwHwYDVR0jBBgwFoAUtZ1PQYq+c36r/qC9"
  , "slPLUl/legEwDwYDVR0TAQH/BAUwAwEB/zAUBgNVHREEDTALgglsb2NhbGhvc3Qw"
  , "DQYJKoZIhvcNAQELBQADggEBAFB/DlH4KvA5srphG3nQvp5kftjtkhyP1zOfu/c4"
  , "7IZ1WZz8JqSNOeNORIcs3fAsjmb5tqmho5i01cPi1/lZo9r3f6y6DRwvijovUQxs"
  , "uN+Ai5CPsWoLAiw4LnxsZITI71Aayn0tUIpWiEHH1crGCEPiqDoYuYahtkBCxevD"
  , "Om9bYOs6rPEStdQqi6nUAKaeXdVHcyyPVXWtoKtWqIjBpxLpTjB45FFh31rQlKUk"
  , "+QBNxIMZpGvT2eQme71q0uBkATvNNSoVNIKHwHraAj4ZtHVVCdhHpF3CFVMqGfAq"
  , "6wcgPok1wYgkclX3WePaXor3EtmX5W8HTLkJFJJJ50je7nk="
  , "-----END CERTIFICATE-----"
  ]

tlsTestPrivateKey :: BS.ByteString
tlsTestPrivateKey = C8.unlines
  [ "-----BEGIN PRIVATE KEY-----"
  , "MIIEvQIBADANBgkqhkiG9w0BAQEFAASCBKcwggSjAgEAAoIBAQDCvdaWJsUXF4GU"
  , "5cJl+5z1aIsigUUTtzuWL5hvMjJLOJvULjiOzy+wjhzrpORoAOhxqFLrFaXNch7z"
  , "mkfZc4fGA4E48eqsEjxsQZXyi50hvIncySPnulPkddPoUCMc1C4nZnOny1yh4Tt3"
  , "ZoR5OqNb9UnhriS9RjXl321ENGAsm4cQSwzhA7vS0BOCArhl9v4CiR7P1p+rWq6v"
  , "UBgk9vM4c/uJzUf84q2g6skhUXW3fZEh8r9VsJmf60TJNEbCBkG0at8/EvabY/eZ"
  , "O9qQZ1Faf6mK+IDEYTkmS6UwhPzpeQUGRd223hg97Pz2yBI/6DJ6T0MZlZ+77gXp"
  , "V1Nw1Yt9AgMBAAECggEAB4NAQErWDl3oyB69XLJfbsLILAyxLFXQSJPH15SE+0bQ"
  , "+fpCgNrMyQEoVKUWek3CTMgwaf16cKJ3ETbzShgAQVyFzCQj8/OdmmW5VOThZTxs"
  , "sR0qc4btHLEV5Fzmtpm6uVzCL3ZTlsoOQYLcI9I3kMP7oTi/oYvV8tPV12DAbuOu"
  , "rmc6EdytsvbZ6u12MMCqCWk856T6SIjP34p5w3zslfcTgYqqfiGFJC3ZjGGxeEfW"
  , "lKOB4jKCWrHOSH9XbOXOSUd8zRYE6pKfceqFzgcUmhQnIIMXL37hJV/Zlv5W93uW"
  , "315pngCKK76Y5rrY8DI6SxyHdRDhgFEA5rQYJZdbPQKBgQD6LYg/lh9i69GMyQPf"
  , "Jk5ya38X7Q4lBduXIT7nz82ujHDqbtMMMwCYsDSG0k6x4+JIrUfExV2QncDm+ZOs"
  , "SuU0VPjSfcpKBhIeTH5ygz2/a8a67qR5BPLLqi9QrsYgCe/mEu107IN9467Ys/iq"
  , "m3DHFvfRhrhcaVXrDh7LqgpMNwKBgQDHRglwbHEp5Evzwn5TZ6HIU3vJeQ6qTdiY"
  , "zaXDaedvlmx2V/+UAup8apr3WO1/gCaKfXV9rDjDdmiHrnQpCR/dVSPyUMCt7CkF"
  , "3J39jvIqeH/6drs/s2cuzPxymGpWXpKX+2YBq8SsRgGLUuFCYqu6V3wwB3URyfys"
  , "zbL7KlKT6wKBgQDpHzaf8fbrSc1pgALQlLRy4IJ8vBP7Ids+l+czQatq5Elv2rdk"
  , "3b3HiiJYI27bSvuYN4fx7uvCD44qbRRTbzLnsepu0nKGyeNmQmdts6f9UKPNmwS+"
  , "FINejwYqC8JpJnlajfahhqb8zwYlvoaQC+pqSpfAseXnjuxV7UF7DMctvwKBgA2p"
  , "WopPlO6HTUG36sszBp9iQdFNMFkynw/SwXOFNi2rRWJTpBz0mjjPYjJk8VtVYM8L"
  , "zNtBzF5yJrZuml4Z1wpohN9e8+a4kxNozZgNjcKlojh8nVe/p+pIeWIt2tRzBV/Q"
  , "B21D5mbdIcv4caMIerd6ufPc/wSqMV1zeLrJawHjAoGABFkvGe12yerZiDMt6hyz"
  , "r0LfnHYw0oeCEoTKL1jCIrBri3JQqy89ecJNyB5Bh8uKDmFapw6rIW0wYHvA28qf"
  , "BycNsSwCYpi5HOLvwov6FO6YjYfqBXlx+mVgJ0UnkP09bqGRTikNblbN08rWhKRS"
  , "CxGHyUd91FtklMlkJ1ivSxs="
  , "-----END PRIVATE KEY-----"
  ]

withServer :: (Handle -> IO ()) -> (Int -> IO a) -> IO a
withServer respond use = withSocketsDo $ do
  addresses <- getAddrInfo (Just defaultHints { addrFlags = [AI_NUMERICHOST] })
                           (Just "127.0.0.1") (Just "0")
  let address = head addresses
  bracket (socket (addrFamily address) (addrSocketType address) (addrProtocol address))
          close $ \listener -> do
    setSocketOption listener ReuseAddr 1
    bind listener (addrAddress address)
    listen listener 1
    bound <- getSocketName listener
    let port = case bound of
          SockAddrInet value _ -> fromIntegral value
          _ -> error "expected IPv4 loopback"
    serving <- async $ do
      (client, _) <- accept listener
      bracket (socketToHandle client ReadWriteMode) hClose $ \handle -> do
        hSetBinaryMode handle True
        respond handle
    use port `finally` (cancel serving >> void (waitCatch serving))

receiveHeaders :: Handle -> IO ()
receiveHeaders handle = go BS.empty
  where
    go bytes
      | "\r\n\r\n" `BS.isInfixOf` bytes = pure ()
      | BS.length bytes > 65536 = expectationFailure "request header exceeded test cap"
      | otherwise = do
          chunk <- BS.hGetSome handle 4096
          if BS.null chunk then expectationFailure "request ended before headers"
          else go (BS.append bytes chunk)

reply :: Handle -> Int -> BS.ByteString -> IO ()
reply handle status body = do
  BS.hPut handle $ BS.concat
    [ "HTTP/1.1 ", C8.pack (show status), " Result\r\nContent-Length: "
    , C8.pack (show (BS.length body)), "\r\nConnection: close\r\n\r\n", body
    ]
  hFlush handle
