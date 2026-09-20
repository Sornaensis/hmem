module HMem.Server.Embedding.ManagedTeiSpec (spec) where

import Control.Concurrent (newEmptyMVar, putMVar, readMVar, takeMVar, threadDelay, tryPutMVar)
import Control.Concurrent.Async (async, asyncWithUnmask, cancel, waitCatch)
import Control.Exception (MaskingState(..), SomeAsyncException, SomeException, finally, fromException, getMaskingState, mask, onException, throwIO, try)
import Control.Monad (void, when)
import Crypto.Hash (Digest, SHA256, hash)
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.IORef
import Data.List (isPrefixOf, isSubsequenceOf)
import Data.Maybe (isJust)
import Data.Word (Word64)
import Foreign.C.Error (ePERM, eSRCH)
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Text qualified as T
import GHC.Clock (getMonotonicTimeNSec)
import System.Directory
  ( canonicalizePath, createDirectoryIfMissing, createFileLink, doesFileExist
  , findExecutable, getCurrentDirectory, getPermissions, getTemporaryDirectory
  , removeFile, removePathForcibly, setCurrentDirectory, setOwnerExecutable
  , setPermissions )
import System.FilePath ((</>), takeDirectory)
import System.Exit (ExitCode(..))
import System.IO (hClose, openTempFile)
import System.IO.Unsafe (unsafePerformIO)
import System.Info (os)
import System.Process
  ( CreateProcess(..), StdStream(NoStream), createProcess, getProcessExitCode
  , proc, readProcessWithExitCode, terminateProcess, waitForProcess )
import System.Timeout (timeout)
import Test.Hspec

import HMem.Server.Embedding.ManagedTei
import HMem.Server.Embedding.Provider (EmbeddingAvailability(..), EmbeddingErrorCode(..), EmbeddingFailure(..))
import HMem.Config (EmbeddingProviderMode(..))

spec :: Spec
spec = describe "managed TEI supervisor" $ do
  it "uses only loopback-safe argument vectors" $ do
    launches <- newIORef []
    runtime <- startManagedTei config (fakeDeps launches (pure (Right expectedIdentity)) neverExit)
    waitForState runtime (ManagedTeiReady "http://127.0.0.1:43123")
    recorded <- readIORef launches
    recorded `shouldBe`
      [["--model-id", "snapshot", "--dtype", "float16", "--hostname", "127.0.0.1", "--port", "43123", "--prometheus-port", "0", "--auto-truncate", "false", "--max-batch-tokens", "32768", "--max-concurrent-requests", "4", "--max-client-batch-size", "4", "--tokenization-workers", "2", "--json-output"]]
    -- The pinned TEI router loads 1_Pooling/config.json when --pooling is
    -- absent.  The pinned GTE snapshot selects last-token pooling there, so
    -- do not invent a CLI override whose spelling is router-version-specific.
    concat recorded `shouldNotSatisfy` ("--pooling" `elem`)
    stopManagedTei runtime

  it "rejects a wrong model identity and becomes degraded at the bounded budget" $ do
    launches <- newIORef []
    let wrong = expectedIdentity { modelId = "other" }
    runtime <- startManagedTei config { restartBudget = 1 } (fakeDeps launches (pure (Right wrong)) neverExit)
    waitForState runtime ManagedTeiDegraded
    length <$> readIORef launches `shouldReturn` 2
    stopManagedTei runtime

  it "does not spawn when the verified local artifacts are absent" $ do
    launches <- newIORef []
    runtime <- startManagedTei config { restartBudget = 0 } ((fakeDeps launches (pure (Right expectedIdentity)) neverExit) { prepareLaunch = \_ _ -> pure (Left ()) })
    waitForState runtime ManagedTeiDegraded
    readIORef launches `shouldReturn` []
    stopManagedTei runtime

  it "rejects an altered installed manifest against the embedded trust anchor without spawning" $
    withManagedFixtureRoot $ \root -> do
      let localConfig = managedFixtureConfig root
      createDirectoryIfMissing True (takeDirectory localConfig.manifestPath)
      createDirectoryIfMissing True localConfig.artifactBundleRoot
      BS.writeFile localConfig.manifestPath (embeddedManagedTeiManifest <> "# altered\n")
      launches <- newIORef []
      let deps = (fakeDeps launches (pure (Right expectedIdentity)) neverExit)
            { prepareLaunch = verifyManagedTeiLaunch }
      runtime <- startManagedTei localConfig { restartBudget = 0 } deps
      waitForState runtime ManagedTeiDegraded
      readIORef launches `shouldReturn` []
      stopManagedTei runtime

  it "rejects a missing installed manifest as a non-retryable preflight failure" $
    withManagedFixtureRoot $ \root -> do
      let localConfig = managedFixtureConfig root
      createDirectoryIfMissing True localConfig.artifactBundleRoot
      verifyManagedTeiLaunch localConfig testLaunch `shouldReturn` Left ()

  it "verifies a complete real-hash fixture with canonical paths independent of cwd" $ do
    when (os == "mingw32") (pendingWith "native POSIX executable-mode fixture")
    withVerifierFixture $ \fixture -> do
      originalDirectory <- getCurrentDirectory
      result <- (setCurrentDirectory (takeDirectory fixture.vfManifestPath) >>
        verifyFixture fixture) `finally` setCurrentDirectory originalDirectory
      result `shouldBe` Right testLaunch
        { childExecutable = fixture.vfEntrypointPath
        , modelSnapshotPath = fixture.vfModelRoot
        , arguments = ["--model-id", fixture.vfModelRoot]
        , childEnvironment =
            [ ("PATH", fixture.vfRuntimeRoot <> ":/usr/local/cuda/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin")
            , ("LD_LIBRARY_PATH", "/usr/local/cuda/lib64:/usr/local/cuda/lib64")
            , ("HF_HUB_OFFLINE", "1")
            , ("TRANSFORMERS_OFFLINE", "1")
            , ("USE_FLASH_ATTENTION", "True")
            , ("NVIDIA_VISIBLE_DEVICES", "0")
            , ("NVIDIA_DRIVER_CAPABILITIES", "compute,utility")
            ]
        }

  it "rejects one-byte checksum drift, a missing file, and an extra file" $ do
    when (os == "mingw32") (pendingWith "native POSIX executable-mode fixture")
    withVerifierFixture $ \fixture -> do
      BS.appendFile fixture.vfRouterPath "x"
      verifyFixture fixture `shouldReturn` Left ()
    withVerifierFixture $ \fixture -> do
      removeFile fixture.vfModelFile
      verifyFixture fixture `shouldReturn` Left ()
    withVerifierFixture $ \fixture -> do
      BS.writeFile (fixture.vfRuntimeRoot </> "unexpected") "extra"
      verifyFixture fixture `shouldReturn` Left ()

  it "rejects duplicate and escaping paths in a coherent fixture authority" $ do
    when (os == "mingw32") (pendingWith "native POSIX executable-mode fixture")
    withVerifierFixture $ \fixture -> do
      let duplicateAuthority = fixture.vfAuthority <> BS8.pack
            ("  - path: config.json\n    bytes: 2\n    sha256: " <> sha256Bytes "{}" <> "\n")
      BS.writeFile fixture.vfManifestPath duplicateAuthority
      verifyManagedTeiLaunchAgainst duplicateAuthority fixture.vfConfig testLaunch
        `shouldReturn` Left ()
    withVerifierFixture $ \fixture -> do
      let needle = "  - path: config.json\n"
          (beforeNeedle, fromNeedle) = BS.breakSubstring needle fixture.vfAuthority
          escapeAuthority =
            beforeNeedle <> "  - path: ../escape\n" <> BS.drop (BS.length needle) fromNeedle
      BS.writeFile fixture.vfManifestPath escapeAuthority
      verifyManagedTeiLaunchAgainst escapeAuthority fixture.vfConfig testLaunch
        `shouldReturn` Left ()

  it "rejects a native POSIX symlink in an otherwise exact inventory" $ do
    when (os == "mingw32") (pendingWith "native POSIX symlink fixture")
    withVerifierFixture $ \fixture -> do
      let outside = takeDirectory fixture.vfModelRoot </> "outside-model-file"
      BS.writeFile outside "{}"
      removeFile fixture.vfModelFile
      createFileLink outside fixture.vfModelFile
      verifyFixture fixture `shouldReturn` Left ()

  it "rejects a native POSIX FIFO at an expected manifest path before hashing" $ do
    when (os == "mingw32") (pendingWith "native POSIX FIFO fixture")
    withVerifierFixture $ \fixture -> do
      removeFile fixture.vfModelFile
      (fifoExit, _, _) <- readProcessWithExitCode "mkfifo" [fixture.vfModelFile] ""
      fifoExit `shouldBe` ExitSuccess
      timeout 1000000 (verifyFixture fixture) `shouldReturn` Just (Left ())

  it "rejects a non-executable canonical router" $ do
    when (os == "mingw32") (pendingWith "native POSIX executable-mode fixture")
    withVerifierFixture $ \fixture -> do
      permissions <- getPermissions fixture.vfRouterPath
      setPermissions fixture.vfRouterPath (setOwnerExecutable False permissions)
      verifyFixture fixture `shouldReturn` Left ()

  it "keeps verifier failures redacted and never spawns" $ do
    when (os == "mingw32") (pendingWith "native POSIX executable-mode fixture")
    withVerifierFixture $ \fixture -> do
      BS.appendFile fixture.vfRouterPath "sensitive-router-drift"
      launches <- newIORef []
      events <- newIORef []
      let deps = (fakeDeps launches (pure (Right expectedIdentity)) neverExit)
            { prepareLaunch = verifyManagedTeiLaunchAgainst fixture.vfAuthority
            , emit = \eventValue -> modifyIORef' events (<> [eventValue])
            }
      runtime <- startManagedTei fixture.vfConfig { restartBudget = 0 } deps
      waitForState runtime ManagedTeiDegraded
      readIORef launches `shouldReturn` []
      readIORef events `shouldReturn` [ManagedTeiPreflightFailed]
      stopManagedTei runtime

  it "validates the full pinned readiness identity" $ do
    validateManagedTeiIdentity config expectedIdentity `shouldBe` True
    validateManagedTeiIdentity config expectedIdentity { modelRevision = "wrong-revision" } `shouldBe` False
    validateManagedTeiIdentity config expectedIdentity { maxInputLength = 32767 } `shouldBe` False
    validateManagedTeiIdentity config expectedIdentity { maxBatchTokens = 32767 } `shouldBe` False
    validateManagedTeiIdentity config expectedIdentity { maxBatchTokens = 32769 } `shouldBe` True
    validateManagedTeiIdentity config expectedIdentity { autoTruncate = True } `shouldBe` False

  it "rejects the wrong embedding dimension without starting an HTTP fallback" $ do
    launches <- newIORef []
    let wrongDimensions = expectedIdentity { dimensions = 1024 }
    runtime <- startManagedTei config { restartBudget = 0 } (fakeDeps launches (pure (Right wrongDimensions)) neverExit)
    waitForState runtime ManagedTeiDegraded
    length <$> readIORef launches `shouldReturn` 1
    stopManagedTei runtime

  it "keeps one child and provider generation while retryable GPU availability warms up" $ do
    launches <- newIORef []
    attempts <- newIORef (0 :: Int)
    let deps = fakeDeps launches (pure (Right expectedIdentity)) neverExit
        hooks = ManagedTeiHooks
          { clearTargetBeforeLaunch = pure ()
          , activateGeneration = \_ _ remaining register -> do
              available <- awaitManagedAvailability remaining $ do
                attempt <- atomicModifyIORef' attempts (\value -> (value + 1, value + 1))
                pure $ if attempt == 1
                  then EmbeddingUnavailable (EmbeddingFailure ProviderUnavailable True)
                  else EmbeddingAvailable
              case available of
                EmbeddingAvailable -> registeredActive register ManagedTeiActive
                  { activeInvalidate = pure ()
                  , activeStopAndJoin = pure ()
                  , activeDisableTarget = pure ()
                  , activeWorkerOutcome = neverExit >> pure ()
                  , activeAvailable = pure True }
                _ -> pure (Left ManagedTeiActivationRetryable)
          }
    runtime <- startManagedTeiWith config { readinessTimeoutMicros = 1000000, restartBudget = 1 } deps hooks
    waitForState runtime (ManagedTeiReady "http://127.0.0.1:43123")
    readIORef attempts `shouldReturn` 2
    length <$> readIORef launches `shouldReturn` 1
    runtime.generation `shouldReturn` 1
    stopManagedTei runtime

  it "does not restart a child after permanent GPU incompatibility" $ do
    launches <- newIORef []
    attempts <- newIORef (0 :: Int)
    let deps = fakeDeps launches (pure (Right expectedIdentity)) neverExit
        hooks = ManagedTeiHooks
          { clearTargetBeforeLaunch = pure ()
          , activateGeneration = \_ _ remaining _ -> do
              available <- awaitManagedAvailability remaining $ do
                modifyIORef' attempts (+ 1)
                pure (EmbeddingUnavailable (EmbeddingFailure ProviderConfigurationError False))
              available `shouldBe` EmbeddingUnavailable (EmbeddingFailure ProviderConfigurationError False)
              pure (Left ManagedTeiActivationPermanent)
          }
    runtime <- startManagedTeiWith config { readinessTimeoutMicros = 500000, restartBudget = 2 } deps hooks
    waitForState runtime ManagedTeiDegraded
    readIORef attempts `shouldReturn` 1
    length <$> readIORef launches `shouldReturn` 1
    stopManagedTei runtime

  it "runs the acquired supervisor body unmasked" $ do
    launches <- newIORef []
    masking <- newEmptyMVar
    let deps = (fakeDeps launches (pure (Right expectedIdentity)) neverExit)
          { selectLoopbackPort = getMaskingState >>= \value -> putMVar masking value >> pure 43123 }
    runtime <- mask $ \_ -> startManagedTei config { restartBudget = 0 } deps
    timeout 1000000 (readMVar masking) `shouldReturn` Just Unmasked
    stopManagedTei runtime

  it "runs external provider activation unmasked and joins its cancellation cleanup" $ do
    masking <- newEmptyMVar
    released <- newEmptyMVar
    owner <- asyncWithUnmask $ \unmask -> unmask $ withExternalActivation
      (\register -> do
        value <- getMaskingState
        mask $ \_ -> register () >> putMVar masking value >> pure (Right ()))
      (\_ -> neverExit >> pure ())
      (\_ -> putMVar released ())
    timeout 1000000 (readMVar masking) `shouldReturn` Just Unmasked
    cancel owner
    timeout 1000000 (readMVar released) `shouldReturn` Just ()
    waitCatch owner >>= \case
      Left failure -> (fromException failure :: Maybe SomeAsyncException) `shouldSatisfy` isJust
      Right _ -> expectationFailure "external provider cancellation was swallowed"

  it "joins a registered external worker cancelled before activation returns" $ do
    registered <- newEmptyMVar
    blockHandoff <- newEmptyMVar @()
    joined <- newEmptyMVar
    releases <- newIORef (0 :: Int)
    owner <- asyncWithUnmask $ \unmask -> unmask $ withExternalActivation
      (\register -> mask $ \restore -> do
        worker <- async neverExit
        register worker `onException` cancel worker
        putMVar registered ()
        restore (takeMVar blockHandoff)
        pure (Right worker))
      (\_ -> expectationFailure "uncompleted activation started its worker wait")
      (\worker -> do
        modifyIORef' releases (+ 1)
        cancel worker
        waitCatch worker >>= \case
          Left failure -> (fromException failure :: Maybe SomeAsyncException) `shouldSatisfy` isJust
          Right _ -> expectationFailure "registered worker did not end through cancellation"
        putMVar joined ())
    timeout 1000000 (readMVar registered) `shouldReturn` Just ()
    cancel owner
    timeout 1000000 (readMVar joined) `shouldReturn` Just ()
    readIORef releases `shouldReturn` 1
    waitCatch owner >>= \case
      Left failure -> (fromException failure :: Maybe SomeAsyncException) `shouldSatisfy` isJust
      Right _ -> expectationFailure "pre-handoff cancellation was swallowed"

  it "bounds readiness and degrades after a timeout" $ do
    launches <- newIORef []
    reaped <- newEmptyMVar
    let delayedProbe = threadDelay 1000000 >> pure (Right expectedIdentity)
        deps = (fakeDeps launches delayedProbe neverExit)
          { spawnChild = \_ launch register -> do
              modifyIORef' launches (<> [launch.arguments])
              registerChild register ManagedTeiChild
                { waitForChildExit = neverExit
                , terminateAndReap = void (tryPutMVar reaped ())
                , childProcessId = Nothing
                , childProcessGroupId = Nothing
                } }
    runtime <- startManagedTei config { readinessTimeoutMicros = 200000, restartBudget = 0 } deps
    waitForState runtime ManagedTeiDegraded
    timeout 1000000 (readMVar reaped) `shouldReturn` Just ()
    stopManagedTei runtime

  it "detects a crash before readiness without waiting for the readiness timeout" $ do
    launches <- newIORef []
    let immediateExit = pure (ExitFailure 1)
    runtime <- startManagedTei config { restartBudget = 0 } (fakeDeps launches neverReady immediateExit)
    waitForState runtime ManagedTeiDegraded
    length <$> readIORef launches `shouldReturn` 1
    stopManagedTei runtime

  it "restarts after a child exits post-readiness and honours the budget" $ do
    launches <- newIORef []
    exits <- newIORef (0 :: Int)
    let immediateExit = do
          count <- atomicModifyIORef' exits (\value -> (value + 1, value))
          pure (if count == 0 then ExitFailure 1 else ExitSuccess)
    runtime <- startManagedTei config { restartBudget = 1 } (fakeDeps launches (pure (Right expectedIdentity)) immediateExit)
    waitForState runtime ManagedTeiDegraded
    length <$> readIORef launches `shouldReturn` 2
    stopManagedTei runtime

  it "observes readiness before a child crash and then degrades without fallback" $ do
    launches <- newIORef []
    ready <- newEmptyMVar
    releaseCrash <- newEmptyMVar
    let probe = putMVar ready () >> pure (Right expectedIdentity)
        exitAfterReady = takeMVar releaseCrash >> pure (ExitFailure 1)
    runtime <- startManagedTei config { restartBudget = 0 } (fakeDeps launches probe exitAfterReady)
    takeMVar ready
    waitForState runtime (ManagedTeiReady "http://127.0.0.1:43123")
    putMVar releaseCrash ()
    waitForState runtime ManagedTeiDegraded
    length <$> readIORef launches `shouldReturn` 1
    stopManagedTei runtime

  it "retires one generation before publishing a replacement at the same URL" $ do
    events <- newIORef ([] :: [String])
    launches <- newIORef ([] :: [[String]])
    count <- newIORef (0 :: Int)
    firstExit <- newEmptyMVar
    let record value = modifyIORef' events (<> [value])
        deps = (fakeDeps launches (pure (Right expectedIdentity)) neverExit)
          { selectLoopbackPort = pure 43123
          , spawnChild = \_ launch register -> do
              n <- atomicModifyIORef' count (\value -> (value + 1, value + 1))
              modifyIORef' launches (<> [launch.arguments])
              record ("spawn" <> show n)
              registerChild register ManagedTeiChild
                { waitForChildExit = if n == 1 then readMVar firstExit else neverExit
                , terminateAndReap = record ("child" <> show n)
                , childProcessId = Nothing
                , childProcessGroupId = Nothing }
          }
        hooks = ManagedTeiHooks
          { clearTargetBeforeLaunch = record "clear"
          , activateGeneration = \token endpointValue _ register -> do
              endpointValue `shouldBe` "http://127.0.0.1:43123"
              let label = show token
              record ("activate" <> label)
              registeredActive register ManagedTeiActive
                { activeInvalidate = record ("invalidate" <> label)
                , activeStopAndJoin = record ("join" <> label)
                , activeDisableTarget = record ("disable" <> label)
                , activeWorkerOutcome = threadDelay (60 * 1000 * 1000)
                , activeAvailable = pure True
                } }
    runtime <- startManagedTeiWith config { restartBudget = 1 } deps hooks
    waitForState runtime (ManagedTeiReady "http://127.0.0.1:43123")
    runtime.generation `shouldReturn` 1
    putMVar firstExit ExitSuccess
    timeout 5000000 (waitForGeneration runtime 2) `shouldReturn` Just ()
    waitForState runtime (ManagedTeiReady "http://127.0.0.1:43123")
    stopManagedTei runtime
    observed <- readIORef events
    ["clear", "spawn1", "activate1", "invalidate1", "join1", "disable1", "child1",
      "clear", "spawn2", "activate2", "invalidate2", "join2", "disable2", "child2"]
      `isSubsequenceOf` observed `shouldBe` True

  it "withdraws during validation without publishing a target or child" $ do
    launches <- newIORef ([] :: [[String]])
    events <- newIORef ([] :: [String])
    validating <- newEmptyMVar
    blocked <- newEmptyMVar
    let record value = modifyIORef' events (<> [value])
        deps = (fakeDeps launches (pure (Right expectedIdentity)) neverExit)
          { spawnChild = \_ launch register -> do
              modifyIORef' launches (<> [launch.arguments])
              registerChild register ManagedTeiChild
                { waitForChildExit = neverExit
                , terminateAndReap = record "child"
                , childProcessId = Nothing
                , childProcessGroupId = Nothing }
          }
        hooks = ManagedTeiHooks
          { clearTargetBeforeLaunch = record "clear"
          , activateGeneration = \_ _ _ _ -> do
              putMVar validating ()
              _ <- readMVar blocked
              record "enabled"
              pure (Left ManagedTeiActivationRetryable) }
    runtime <- startManagedTeiWith config { restartBudget = 0 } deps hooks
    takeMVar validating
    stopManagedTei runtime
    managedTeiState runtime `shouldReturn` ManagedTeiStopped
    readIORef events `shouldReturn` ["clear", "child"]

  it "withdraws a ready generation when its provider becomes unavailable while the child lives" $ do
    launches <- newIORef ([] :: [[String]])
    events <- newIORef ([] :: [String])
    available <- newIORef True
    let record value = modifyIORef' events (<> [value])
        deps = (fakeDeps launches (pure (Right expectedIdentity)) neverExit)
          { spawnChild = \_ launch register -> do
              modifyIORef' launches (<> [launch.arguments])
              registerChild register ManagedTeiChild
                { waitForChildExit = neverExit
                , terminateAndReap = record "child"
                , childProcessId = Nothing
                , childProcessGroupId = Nothing }
          , sleepMicros = threadDelay }
        hooks = ManagedTeiHooks
          { clearTargetBeforeLaunch = record "clear"
          , activateGeneration = \_ _ _ register ->
              registeredActive register ManagedTeiActive
                { activeInvalidate = record "invalidate"
                , activeStopAndJoin = record "join"
                , activeDisableTarget = record "disable"
                , activeWorkerOutcome = neverExit >> pure ()
                , activeAvailable = readIORef available }
          }
    runtime <- startManagedTeiWith config { restartBudget = 0 } deps hooks
    waitForState runtime (ManagedTeiReady "http://127.0.0.1:43123")
    writeIORef available False
    waitForState runtime ManagedTeiDegraded
    observed <- readIORef events
    ["clear", "invalidate", "join", "disable", "child"]
      `isSubsequenceOf` observed `shouldBe` True
    length <$> readIORef launches `shouldReturn` 1
    stopManagedTei runtime

  it "shares one launch deadline across preflight, readiness, activation, and joined cleanup" $ do
    launches <- newIORef ([] :: [[String]])
    activated <- newIORef False
    joined <- newEmptyMVar
    reapedAt <- newEmptyMVar
    let deps = (fakeDeps launches (threadDelay 70000 >> pure (Right expectedIdentity)) neverExit)
          { prepareLaunch = \_ launch -> threadDelay 70000 >> pure (Right launch)
          , spawnChild = \_ launch register -> do
              modifyIORef' launches (<> [launch.arguments])
              registerChild register ManagedTeiChild
                { waitForChildExit = neverExit
                , terminateAndReap = do
                    threadDelay 30000
                    getMonotonicTimeNSec >>= void . tryPutMVar reapedAt
                , childProcessId = Nothing
                , childProcessGroupId = Nothing }
          }
        hooks = ManagedTeiHooks
          { clearTargetBeforeLaunch = pure ()
          , activateGeneration = \_ _ _ register -> do
              writeIORef activated True
              let active = ManagedTeiActive
                    { activeInvalidate = pure ()
                    , activeStopAndJoin = threadDelay 20000 >> void (tryPutMVar joined ())
                    , activeDisableTarget = pure ()
                    , activeWorkerOutcome = neverExit >> pure ()
                    , activeAvailable = pure True }
              register active
              threadDelay 1000000
              pure (Right active) }
    startedAt <- getMonotonicTimeNSec
    runtime <- startManagedTeiWith config { readinessTimeoutMicros = 400000, restartBudget = 0 } deps hooks
    joinedOutcome <- timeout 1000000 (takeMVar joined)
    joinedOutcome `shouldBe` Just ()
    completed <- timeout 1000000 (takeMVar reapedAt)
    completed `shouldSatisfy` isJust
    let elapsedMicros = maybe maxBound (\finished -> (finished - startedAt) `div` 1000) completed
    elapsedMicros `shouldSatisfy` (< 420000)
    readIORef activated `shouldReturn` True
    managedTeiState runtime `shouldReturn` ManagedTeiDegraded
    stopManagedTei runtime

  it "continues owned cleanup after the startup clock expires and reports the overrun" $ do
    launches <- newIORef []
    cleanupEntered <- newEmptyMVar
    releaseCleanup <- newEmptyMVar
    reaped <- newEmptyMVar
    calls <- newIORef (0 :: Int)
    let deps = (fakeDeps launches (threadDelay 1000000 >> pure (Right expectedIdentity)) neverExit)
          { spawnChild = \_ launch register -> do
              modifyIORef' launches (<> [launch.arguments])
              registerChild register ManagedTeiChild
                { waitForChildExit = neverExit
                , terminateAndReap = do
                    modifyIORef' calls (+ 1)
                    void (tryPutMVar cleanupEntered ())
                    readMVar releaseCleanup
                    void (tryPutMVar reaped ())
                , childProcessId = Nothing
                , childProcessGroupId = Nothing }
          }
    runtime <- startManagedTei config { readinessTimeoutMicros = 120000, restartBudget = 0 } deps
    timeout 1000000 (readMVar cleanupEntered) `shouldReturn` Just ()
    threadDelay 150000
    putMVar releaseCleanup ()
    timeout 1000000 (readMVar reaped) `shouldReturn` Just ()
    waitForState runtime ManagedTeiDegraded
    readIORef calls >>= (`shouldSatisfy` (>= 2))
    stopResult <- try @SomeException (stopManagedTei runtime)
    stopResult `shouldSatisfy` either (const True) (const False)

  it "treats uncertain process-group probes as failure and waits for real disappearance" $ do
    classifyProcessGroupErrno Nothing `shouldBe` Right True
    classifyProcessGroupErrno (Just eSRCH) `shouldBe` Right False
    classifyProcessGroupErrno (Just ePERM) `shouldBe` Left ()
    probes <- newIORef [True, True, False]
    absent <- awaitProcessGroupAbsent $ atomicModifyIORef' probes $ \values ->
      case values of
        value : rest -> (rest, value)
        [] -> ([], False)
    absent `shouldBe` True
    readIORef probes `shouldReturn` []
    uncertain <- try @SomeException (awaitProcessGroupAbsent (throwIO (userError "probe failed")))
    uncertain `shouldSatisfy` either (const True) (const False)

  it "rejects broad, malformed, and unrepresentable process-group IDs" $ do
    parseOwnedProcessGroupId "2" `shouldBe` Right 2
    map parseOwnedProcessGroupId ["0", "1", "-1", "not-a-pid", "999999999999999999999999"]
      `shouldBe` replicate 5 (Left ())

  it "resets restart debt only after a healthy monotonic interval" $ do
    launches <- newIORef ([] :: [[String]])
    count <- newIORef (0 :: Int)
    clock <- newIORef (0 :: Word64)
    secondExit <- newEmptyMVar
    let deps = (fakeDeps launches (pure (Right expectedIdentity)) neverExit)
          { monotonicNow = readIORef clock
          , spawnChild = \_ launch register -> do
              n <- atomicModifyIORef' count (\value -> (value + 1, value + 1))
              modifyIORef' launches (<> [launch.arguments])
              registerChild register ManagedTeiChild
                { waitForChildExit = case n of
                    1 -> pure (ExitFailure 1)
                    2 -> readMVar secondExit
                    _ -> neverExit
                , terminateAndReap = pure ()
                , childProcessId = Nothing
                , childProcessGroupId = Nothing }
          }
    runtime <- startManagedTei config { restartBudget = 1, restartWindowSeconds = 1 } deps
    timeout 5000000 (waitForGeneration runtime 2) `shouldReturn` Just ()
    waitForState runtime (ManagedTeiReady "http://127.0.0.1:43124")
    writeIORef clock 2000000000
    putMVar secondExit ExitSuccess
    timeout 5000000 (waitForGeneration runtime 3) `shouldReturn` Just ()
    waitForState runtime (ManagedTeiReady "http://127.0.0.1:43124")
    length <$> readIORef launches `shouldReturn` 3
    stopManagedTei runtime

  it "cancels a backoff and reaps the active child during shutdown" $ do
    launches <- newIORef []
    reaped <- newIORef (0 :: Int)
    let childExit = pure (ExitFailure 1)
        deps = (fakeDeps launches (pure (Right expectedIdentity)) childExit)
          { spawnChild = \_ launch register -> do
              modifyIORef' launches (<> [launch.arguments])
              registerChild register ManagedTeiChild
                { waitForChildExit = childExit
                , terminateAndReap = modifyIORef' reaped (+ 1)
                , childProcessId = Nothing
                , childProcessGroupId = Nothing }
          , sleepMicros = \_ -> threadDelay (10 * 1000 * 1000) }
    runtime <- startManagedTei config deps
    threadDelay 10000
    stopManagedTei runtime
    managedTeiState runtime `shouldReturn` ManagedTeiStopped
    value <- readIORef reaped
    value `shouldSatisfy` (> 0)

  it "propagates unexpected worker failures through one redacted lifecycle code" $ do
    reports <- newIORef ([] :: [String])
    worker <- async (throwIO (userError "sensitive provider body"))
    outcome <- try @SomeException $
      withEmbeddingWorkerMonitor
        (modifyIORef' reports (<> ["embedding_worker_failed"]))
        (Just worker)
        neverExit
    case outcome of
      Left failure ->
        show failure `shouldBe` "user error (embedding lifecycle failed (embedding_worker_failed))"
      Right _ -> expectationFailure "failed worker did not terminate the lifecycle"
    readIORef reports `shouldReturn` ["embedding_worker_failed"]

  it "surfaces supervisor child cleanup failure through one stable code" $ do
    launches <- newIORef []
    let deps = (fakeDeps launches (pure (Right expectedIdentity)) neverExit)
          { spawnChild = \_ launch register -> do
              modifyIORef' launches (<> [launch.arguments])
              registerChild register ManagedTeiChild
                { waitForChildExit = neverExit
                , terminateAndReap = throwIO (userError "sensitive taskkill output")
                , childProcessId = Nothing
                , childProcessGroupId = Nothing
                }
          }
    runtime <- startManagedTei config deps
    waitForState runtime (ManagedTeiReady "http://127.0.0.1:43123")
    outcome <- try @SomeException (stopManagedTei runtime)
    case outcome of
      Left failure ->
        show failure `shouldBe` "user error (managed-tei lifecycle failed (managed_tei_cleanup_failed))"
      Right () -> expectationFailure "cleanup failure was swallowed"
    managedTeiState runtime `shouldReturn` ManagedTeiDegraded

  it "reaps the child and refuses replacement when worker join fails" $ do
    launches <- newIORef ([] :: [[String]])
    events <- newIORef ([] :: [String])
    exitSignal <- newEmptyMVar
    let record value = modifyIORef' events (<> [value])
        deps = (fakeDeps launches (pure (Right expectedIdentity)) (readMVar exitSignal))
          { spawnChild = \_ launch register -> do
              modifyIORef' launches (<> [launch.arguments])
              registerChild register ManagedTeiChild
                { waitForChildExit = readMVar exitSignal
                , terminateAndReap = record "child"
                , childProcessId = Nothing
                , childProcessGroupId = Nothing }
          }
        hooks = ManagedTeiHooks
          { clearTargetBeforeLaunch = record "clear"
          , activateGeneration = \_ _ _ register ->
              registeredActive register ManagedTeiActive
                { activeInvalidate = record "invalidate"
                , activeStopAndJoin = record "join" >> throwIO (userError "lease release failed")
                , activeDisableTarget = record "disable"
                , activeWorkerOutcome = neverExit >> pure ()
                , activeAvailable = pure True }
          }
    runtime <- startManagedTeiWith config { restartBudget = 1 } deps hooks
    waitForState runtime (ManagedTeiReady "http://127.0.0.1:43123")
    putMVar exitSignal ExitSuccess
    waitForState runtime ManagedTeiDegraded
    outcome <- try @SomeException (stopManagedTei runtime)
    outcome `shouldSatisfy` either (const True) (const False)
    observed <- readIORef events
    ["clear", "invalidate", "join", "child"] `isSubsequenceOf` observed `shouldBe` True
    "disable" `elem` observed `shouldBe` False
    length <$> readIORef launches `shouldReturn` 1

  it "publishes degraded before a post-readiness cleanup failure exits the supervisor" $ do
    launches <- newIORef []
    events <- newIORef []
    exitSignal <- newEmptyMVar
    let deps = (fakeDeps launches (pure (Right expectedIdentity)) (readMVar exitSignal))
          { spawnChild = \_ launch register -> do
              modifyIORef' launches (<> [launch.arguments])
              registerChild register ManagedTeiChild
                { waitForChildExit = readMVar exitSignal
                , terminateAndReap = throwIO (userError "sensitive cleanup detail")
                , childProcessId = Nothing
                , childProcessGroupId = Nothing
                }
          , emit = \eventValue -> modifyIORef' events (<> [eventValue])
          }
    runtime <- startManagedTei config { restartBudget = 0 } deps
    waitForState runtime (ManagedTeiReady "http://127.0.0.1:43123")
    putMVar exitSignal ExitSuccess
    waitForState runtime ManagedTeiDegraded
    outcome <- try @SomeException (stopManagedTei runtime)
    case outcome of
      Left failure ->
        show failure `shouldBe` "user error (managed-tei lifecycle failed (managed_tei_cleanup_failed))"
      Right () -> expectationFailure "post-readiness cleanup failure was swallowed"
    readIORef events `shouldReturn`
      [ ManagedTeiSpawned, ManagedTeiReadyEvent, ManagedTeiChildExited, ManagedTeiDegradedEvent ]

  it "keeps event rendering redacted" $ do
    managedTeiEventText (ManagedTeiOutputSuppressed "stderr") `shouldBe` "managed-tei stderr output suppressed"
    managedTeiEventText ManagedTeiReadyEvent `shouldNotSatisfy` T.isInfixOf "snapshot"

  it "decodes TEI's actual info model schema and derives dimensions from /embed" $ do
    let info = LBS8.pack "{\"model_id\":\"snapshot\",\"served_model_name\":\"Alibaba-NLP/gte-Qwen2-1.5B-instruct\",\"model_type\":{\"embedding\":{\"pooling\":\"last_token\"}},\"model_sha\":\"1cad2ab3ff41c2671f34e135d29831368ee26b68\",\"max_input_length\":32768,\"max_batch_tokens\":32768,\"auto_truncate\":false}"
        embedding = Aeson.encode [(1 :: Double) : replicate 1535 0]
    decodeTeiInfoModelId info `shouldBe` Right (expectedIdentity { dimensions = 0 })
    decodeTeiReadinessEmbedding embedding `shouldBe` Right 1536

  it "rejects the obsolete dimensions field as insufficient readiness evidence" $ do
    let infoOnly = LBS8.pack "{\"model_id\":\"snapshot\",\"served_model_name\":\"Alibaba-NLP/gte-Qwen2-1.5B-instruct\",\"model_type\":{\"embedding\":{\"pooling\":\"last_token\"}},\"dimensions\":1536}"
    decodeTeiInfoModelId infoOnly `shouldBe` Left ()
    decodeTeiReadinessEmbedding infoOnly `shouldBe` Left ()

  it "keeps disabled and HTTP modes child-free" $ do
    embeddingModeStartsChild EmbeddingProviderDisabled `shouldBe` False
    embeddingModeStartsChild EmbeddingProviderHttp `shouldBe` False
    embeddingModeStartsChild EmbeddingProviderManagedTei `shouldBe` True

  it "cleans up an already-started child when readiness throws" $ do
    launches <- newIORef []
    reaped <- newIORef (0 :: Int)
    exitSignal <- newEmptyMVar
    let deps = (fakeDeps launches (throwIO (userError "probe failed")) (readMVar exitSignal))
          { spawnChild = \_ launch register -> do
              modifyIORef' launches (<> [launch.arguments])
              registerChild register ManagedTeiChild
                { waitForChildExit = readMVar exitSignal
                , terminateAndReap = do
                    void (tryPutMVar exitSignal ExitSuccess)
                    modifyIORef' reaped (+ 1)
                , childProcessId = Nothing
                , childProcessGroupId = Nothing
                } }
    runtime <- startManagedTei config { restartBudget = 0 } deps
    waitForState runtime ManagedTeiDegraded
    stopManagedTei runtime
    readIORef reaped `shouldReturn` 1

  it "orders worker, child, transport, database, then logger shutdown" $ do
    events <- newIORef ([] :: [String])
    let record value = modifyIORef' events (<> [value])
    shutdownEmbeddingLifecycle (record "worker") (record "child") (record "transport") (record "database") (record "logger")
    readIORef events `shouldReturn` ["worker", "child", "transport", "database", "logger"]

  it "continues ordered shutdown after a partial setup failure" $ do
    events <- newIORef ([] :: [String])
    let record value = modifyIORef' events (<> [value])
        failedWorker = record "worker" >> throwIO (userError "partial setup")
    outcome <- try @SomeException $ shutdownEmbeddingLifecycle failedWorker (record "child") (record "transport") (record "database") (record "logger")
    outcome `shouldSatisfy` either (const True) (const False)
    readIORef events `shouldReturn` ["worker", "child", "transport", "database", "logger"]

  it "releases the managed child when awaiting readiness fails" $ do
    events <- newIORef ([] :: [String])
    let record value = modifyIORef' events (<> [value])
    outcome <- (try @SomeException $ withManagedTeiLifecycle
      (record "managed-acquire" >> pure ())
      (\_ -> record "await" >> throwIO (userError "await failed"))
      (record "transport-acquire" >> pure ())
      (\_ _ -> record "provider-acquire" >> pure ())
      (\_ -> record "worker-acquire" >> pure ())
      (\_ -> record "worker-release")
      (\_ -> record "transport-release")
      (\_ -> record "managed-release")
      (\_ _ _ -> record "warp")) :: IO (Either SomeException ())
    outcome `shouldSatisfy` either (const True) (const False)
    readIORef events `shouldReturn` ["managed-acquire", "await", "managed-release"]

  it "releases the child before transport when worker acquisition fails" $ do
    events <- newIORef ([] :: [String])
    let record value = modifyIORef' events (<> [value])
    outcome <- (try @SomeException $ withManagedTeiLifecycle
      (record "managed-acquire" >> pure ())
      (\_ -> record "await" >> pure ())
      (record "transport-acquire" >> pure ())
      (\_ _ -> record "provider-acquire" >> pure ())
      (\_ -> record "worker-acquire" >> throwIO (userError "worker failed"))
      (\_ -> record "worker-release")
      (\_ -> record "transport-release")
      (\_ -> record "managed-release")
      (\_ _ _ -> record "warp")) :: IO (Either SomeException ())
    outcome `shouldSatisfy` either (const True) (const False)
    readIORef events `shouldReturn`
      [ "managed-acquire", "await", "transport-acquire", "provider-acquire"
      , "worker-acquire", "managed-release", "transport-release"
      ]

  it "releases worker, child, and transport after a Warp handoff failure" $ do
    events <- newIORef ([] :: [String])
    let record value = modifyIORef' events (<> [value])
    outcome <- (try @SomeException $ withManagedTeiLifecycle
      (record "managed-acquire" >> pure ())
      (\_ -> record "await" >> pure ())
      (record "transport-acquire" >> pure ())
      (\_ _ -> record "provider-acquire" >> pure ())
      (\_ -> record "worker-acquire" >> pure ())
      (\_ -> record "worker-release")
      (\_ -> record "transport-release")
      (\_ -> record "managed-release")
      (\_ _ _ -> record "warp" >> throwIO (userError "warp setup failed"))) :: IO (Either SomeException ())
    outcome `shouldSatisfy` either (const True) (const False)
    readIORef events `shouldReturn`
      [ "managed-acquire", "await", "transport-acquire", "provider-acquire", "worker-acquire"
      , "warp", "worker-release", "managed-release", "transport-release"
      ]

  it "cancels post-spawn pre-publication setup and reaps the child" $ do
    temporaryDirectory <- getTemporaryDirectory
    (pidPath, pidHandle) <- openTempFile temporaryDirectory "hmem-managed-tei-publication-pid"
    hClose pidHandle
    removeFile pidPath
    let launch
          | os == "mingw32" = (sleepLaunchForCurrentOS 30)
              { arguments = ["-NoProfile", "-Command", "[IO.File]::WriteAllText('" <> pidPath <> "',[string]$PID); Start-Sleep -Seconds 30"] }
          | otherwise = (sleepLaunchForCurrentOS 30)
              { arguments = ["-c", "printf '%s' \"$$\" > " <> show pidPath <> "; sleep 30"] }
        exists = if os == "mingw32" then processExists else posixProcessExists
        awaitAbsent = if os == "mingw32" then awaitNotRunning else awaitPosixNotRunning
    (do
        arrived <- newEmptyMVar
        held <- newEmptyMVar
        task <- async $ spawnDefaultManagedTeiChildWithPublicationBarrier
          (putMVar arrived () >> takeMVar held)
          (\_ -> pure ())
          launch
        arrivedResult <- timeout 5000000 (takeMVar arrived)
        case arrivedResult of
          Nothing -> do
            cancel task
            outcome <- waitCatch task
            case outcome of
              Left errorValue -> expectationFailure ("publication setup failed before its barrier: " <> show errorValue)
              Right _ -> expectationFailure "publication setup did not reach its barrier"
          Just () -> do
            pid <- awaitChildPid (error "unpublished child") pidPath Nothing Nothing Nothing Nothing
            exists pid `shouldReturn` True
            cancel task
            outcome <- waitCatch task
            case outcome of
              Left failure ->
                (fromException failure :: Maybe SomeAsyncException) `shouldSatisfy` isJust
              Right _ -> expectationFailure "publication-cancelled child was returned"
            timeout 1000000 (awaitAbsent pid) `shouldReturn` Just ()
      ) `finally` removeIfExists pidPath

  it "reaps a real job-backed Windows PowerShell process tree after bounded pipes" $ do
    when (os /= "mingw32") (pendingWith "Windows taskkill process-tree fixture")
    assertWrongExecutableCannotRun
    temporaryDirectory <- getTemporaryDirectory
    (pidPath, pidHandle) <- openTempFile temporaryDirectory "hmem-managed-tei-windows-child-pid"
    hClose pidHandle
    removeFile pidPath
    (failurePath, failureHandle) <- openTempFile temporaryDirectory "hmem-managed-tei-windows-child-failure"
    hClose failureHandle
    removeFile failurePath
    (startedPath, startedHandle) <- openTempFile temporaryDirectory "hmem-managed-tei-windows-child-started"
    hClose startedHandle
    removeFile startedPath
    (contextPath, contextHandle) <- openTempFile temporaryDirectory "hmem-managed-tei-windows-child-context"
    hClose contextHandle
    removeFile contextPath
    (afterStartPath, afterStartHandle) <- openTempFile temporaryDirectory "hmem-managed-tei-windows-child-after-start"
    hClose afterStartHandle
    removeFile afterStartPath
    pingExecutable <- findExecutable "ping.exe" >>= maybe (expectationFailure "ping.exe is unavailable for the Windows process-tree fixture" >> fail "unreachable") pure
    let quotedPingExecutable = "'" <> pingExecutable <> "'"
        quotedPidPath = "'" <> pidPath <> "'"
        quotedFailurePath = "'" <> failurePath <> "'"
        quotedStartedPath = "'" <> startedPath <> "'"
        quotedContextPath = "'" <> contextPath <> "'"
        quotedAfterStartPath = "'" <> afterStartPath <> "'"
        script = "[IO.File]::WriteAllText(" <> quotedStartedPath <> ",'started'); $names = 'ComSpec','PATHEXT','TEMP','TMP'; $context = (($names | ForEach-Object { $_ + '=' + [string](-not [string]::IsNullOrEmpty([Environment]::GetEnvironmentVariable($_))) }) -join ';'); [IO.File]::WriteAllText(" <> quotedContextPath <> ",$context); Write-Output managed-stdout; Write-Error managed-stderr; try { $start = [Diagnostics.ProcessStartInfo]::new(" <> quotedPingExecutable <> ", '-n 30 127.0.0.1'); $start.UseShellExecute = $false; [IO.File]::WriteAllText(" <> quotedAfterStartPath <> ",'before-start'); $descendant = [Diagnostics.Process]::Start($start); [IO.File]::WriteAllText(" <> quotedAfterStartPath <> ",'after-start'); if ($null -eq $descendant) { [IO.File]::WriteAllText(" <> quotedFailurePath <> ",'ProcessStartReturnedNull'); exit 1 }; [IO.File]::WriteAllText(" <> quotedPidPath <> ",[string]$descendant.Id) } catch { [IO.File]::WriteAllText(" <> quotedFailurePath <> ",$_.Exception.GetType().FullName); exit 1 }; Start-Sleep -Seconds 30"
    child <- spawnDefaultManagedTeiChild (\_ -> pure ()) ManagedTeiLaunch
      { childExecutable = "powershell.exe", modelSnapshotPath = "fixture", endpoint = "http://127.0.0.1:43123", metricsPort = 43124, arguments = ["-NoProfile", "-Command", script], childEnvironment = [] }
    (do
        pingPid <- awaitChildPid child pidPath (Just failurePath) (Just startedPath) (Just contextPath) (Just afterStartPath)
        processExists pingPid `shouldReturn` True
        child.terminateAndReap
        child.terminateAndReap
        exited <- timeout 1000000 child.waitForChildExit
        exited `shouldSatisfy` isJust
        timeout 1000000 (awaitNotRunning pingPid) `shouldReturn` Just ()
      ) `onException` (void $ try @SomeException child.terminateAndReap)
    removeIfExists pidPath
    removeIfExists failurePath
    removeIfExists startedPath
    removeIfExists contextPath
    removeIfExists afterStartPath

  it "reaps a resistant POSIX sh process-group descendant" $ do
    when (os == "mingw32") (pendingWith "POSIX process-group fixture")
    assertWrongExecutableCannotRun
    temporaryDirectory <- getTemporaryDirectory
    (pidPath, pidHandle) <- openTempFile temporaryDirectory "hmem-managed-tei-posix-child-pid"
    hClose pidHandle
    removeFile pidPath
    let script = "trap '' INT TERM; (trap '' INT TERM; sleep 30) & child=$!; printf '%s' \"$child\" > " <> show pidPath <> "; while :; do sleep 1; done"
    (_, _, _, sentinel) <- createProcess (proc "sleep" ["30"])
      { create_group = True, std_out = NoStream, std_err = NoStream }
    let reapSentinel = do
          void (try @SomeException (terminateProcess sentinel))
          timeout 5000000 (waitForProcess sentinel) >>= (`shouldSatisfy` isJust)
    (do
      child <- spawnDefaultManagedTeiChild (\_ -> pure ()) ManagedTeiLaunch
        { childExecutable = "sh", modelSnapshotPath = "fixture", endpoint = "http://127.0.0.1:43123", metricsPort = 43124, arguments = ["-c", script], childEnvironment = [] }
      (do
          descendantPid <- awaitChildPid child pidPath Nothing Nothing Nothing Nothing
          child.childProcessGroupId `shouldBe` child.childProcessId
          case child.childProcessGroupId of
            Just groupId -> parseOwnedProcessGroupId groupId `shouldSatisfy` either (const False) (const True)
            Nothing -> expectationFailure "spawned POSIX child has no owned process group"
          posixProcessExists descendantPid `shouldReturn` True
          child.terminateAndReap
          child.terminateAndReap
          exited <- timeout 1000000 child.waitForChildExit
          exited `shouldSatisfy` isJust
          timeout 1000000 (awaitPosixNotRunning descendantPid) `shouldReturn` Just ()
          getProcessExitCode sentinel `shouldReturn` Nothing
        ) `onException` (void $ try @SomeException child.terminateAndReap)
      ) `finally` reapSentinel
    removeIfExists pidPath

sleepLaunchForCurrentOS :: Int -> ManagedTeiLaunch
sleepLaunchForCurrentOS seconds
  | os == "mingw32" = ManagedTeiLaunch
      { childExecutable = "powershell.exe", modelSnapshotPath = "fixture", endpoint = "http://127.0.0.1:43123", metricsPort = 43124
      , arguments = ["-NoProfile", "-Command", "Start-Sleep -Seconds " <> show seconds]
      , childEnvironment = [] }
  | otherwise = ManagedTeiLaunch
      { childExecutable = "sh", modelSnapshotPath = "fixture", endpoint = "http://127.0.0.1:43123", metricsPort = 43124
      , arguments = ["-c", "sleep " <> show seconds]
      , childEnvironment = [] }

-- Each platform fixture explicitly verifies that its intentionally invalid
-- alternate executable path is not accepted before exercising the real shell.
assertWrongExecutableCannotRun :: IO ()
assertWrongExecutableCannotRun = do
  let wrongPath = if os == "mingw32"
        then "C:\\hmem-managed-tei-posix-fixture-must-not-run.exe"
        else "/hmem-managed-tei-windows-fixture-must-not-run"
  result <- try @SomeException $ spawnDefaultManagedTeiChild (\_ -> pure ())
    (sleepLaunchForCurrentOS 1) { childExecutable = wrongPath }
  case result of
    Left _ -> pure ()
    Right child -> child.terminateAndReap >> expectationFailure "wrong fixture executable unexpectedly ran"

data VerifierFixture = VerifierFixture
  { vfAuthority :: BS.ByteString
  , vfConfig :: ManagedTeiConfig
  , vfManifestPath :: FilePath
  , vfRuntimeRoot :: FilePath
  , vfEntrypointPath :: FilePath
  , vfRouterPath :: FilePath
  , vfModelRoot :: FilePath
  , vfModelFile :: FilePath
  }

withVerifierFixture :: (VerifierFixture -> IO value) -> IO value
withVerifierFixture action = withManagedFixtureRoot $ \rawRoot -> do
  root <- canonicalizePath rawRoot
  let localConfig = managedFixtureConfig root
      runtimeRoot = localConfig.artifactBundleRoot </> "tei-runtime"
      routerPath = runtimeRoot </> "text-embeddings-router"
      entrypointPath = runtimeRoot </> "entrypoint.sh"
      modelRoot = localConfig.artifactBundleRoot </> "model"
      modelFile = modelRoot </> "config.json"
      routerBytes = "fixture-router"
      entrypointBytes = "fixture-entrypoint"
      modelBytes = "{}"
      modelFiles =
        [ ("config.json", modelBytes)
        , ("1_Pooling/config.json", "{}")
        , ("tokenizer.json", "{}")
        , ("model-00001-of-00002.safetensors", "fixture-shard-1")
        , ("model-00002-of-00002.safetensors", "fixture-shard-2")
        , ("model.safetensors.index.json", "{}")
        ]
      authority = fixtureManifest localConfig routerBytes entrypointBytes modelFiles
  mapM_ (createDirectoryIfMissing True . takeDirectory)
    ([localConfig.manifestPath, routerPath, entrypointPath] <> map ((modelRoot </>) . fst) modelFiles)
  BS.writeFile routerPath routerBytes
  routerPermissions <- getPermissions routerPath
  setPermissions routerPath (setOwnerExecutable True routerPermissions)
  BS.writeFile entrypointPath entrypointBytes
  entrypointPermissions <- getPermissions entrypointPath
  setPermissions entrypointPath (setOwnerExecutable True entrypointPermissions)
  mapM_ (\(path, bytes) -> BS.writeFile (modelRoot </> path) bytes) modelFiles
  BS.writeFile localConfig.manifestPath authority
  action VerifierFixture
    { vfAuthority = authority
    , vfConfig = localConfig
    , vfManifestPath = localConfig.manifestPath
    , vfRuntimeRoot = runtimeRoot
    , vfEntrypointPath = entrypointPath
    , vfRouterPath = routerPath
    , vfModelRoot = modelRoot
    , vfModelFile = modelFile
    }

verifyFixture :: VerifierFixture -> IO (Either () ManagedTeiLaunch)
verifyFixture fixture =
  verifyManagedTeiLaunchAgainst fixture.vfAuthority fixture.vfConfig testLaunch

fixtureManifest :: ManagedTeiConfig -> BS.ByteString -> BS.ByteString -> [(FilePath, BS.ByteString)] -> BS.ByteString
fixtureManifest localConfig routerBytes entrypointBytes modelFiles = BS8.pack $ unlines $
  [ "schema_version: 2"
  , "profile: native-tei-gte-qwen2-1.5b-instruct-cuda-sm120-f16-v1"
  , "expected_embedding_dimensions: 1536"
  , "installation:"
  , "  root: " <> localConfig.installedRoot
  , "  manifest_path: " <> localConfig.manifestPath
  , "  model_root: " <> localConfig.modelSnapshot
  , "  runtime_root: " <> localConfig.installedRoot </> "tei-runtime"
  , "  entrypoint_path: " <> localConfig.executable
  , "  router_path: " <> localConfig.installedRoot </> "tei-runtime/text-embeddings-router"
  , "tei:"
  , "  release: v1.9.3"
  , "  source_commit: 06670157fb6c1523482219bdb2d1660277d38088"
  , "  source_compatibility:"
  , "    backend: candle-cuda"
  , "    model_class: FlashQwen2Model"
  , "    compile_compute_capability: 120"
  , "    runtime_compute_capability: 120"
  , "    requires_cuda: true"
  , "    requires_f16: true"
  , "    uses_config_is_causal: true"
  , "  gpu_image:"
  , "    reference: ghcr.io/huggingface/text-embeddings-inference@sha256:aedf3b34836dc57289583142adcf2b93836cda0736ac8e6ce43691b9c2c67170"
  , "    platform: { os: linux, architecture: amd64 }"
  , "    index_digest: sha256:aedf3b34836dc57289583142adcf2b93836cda0736ac8e6ce43691b9c2c67170"
  , "    platform_manifest_digest: sha256:144aaa80ddcb520d49df83f915dc188ddd7cc6b1b3b9684a829c21dd39cbe3c5"
  , "    config_digest: sha256:affa793eda6c6c6583d9c4041372dd710d74a55ab0a89025a1f322dd71b92eb2"
  , "  runtime_bundle:"
  , "    artifact_root: tei-runtime"
  , "    source_image_reference: ghcr.io/huggingface/text-embeddings-inference@sha256:aedf3b34836dc57289583142adcf2b93836cda0736ac8e6ce43691b9c2c67170"
  , "    source_platform_manifest_digest: sha256:144aaa80ddcb520d49df83f915dc188ddd7cc6b1b3b9684a829c21dd39cbe3c5"
  , "    source_config_digest: sha256:affa793eda6c6c6583d9c4041372dd710d74a55ab0a89025a1f322dd71b92eb2"
  , "    system_abi_source: exact-pinned-platform-image"
  , "    retrieval_phase: build-time-only"
  , "    runtime_use: verified-local-bundle"
  , "    loader:"
  , "      inherit_environment: false"
  , "      working_directory: " <> localConfig.installedRoot </> "tei-runtime"
  , "      path: " <> localConfig.installedRoot </> "tei-runtime" <> ":/usr/local/cuda/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin"
  , "      ld_library_path: /usr/local/cuda/lib64:/usr/local/cuda/lib64"
  , "      ld_preload: null"
  , "      hf_hub_offline: '1'"
  , "      transformers_offline: '1'"
  , "      use_flash_attention: 'True'"
  , "      argv: [--dtype, float16, --max-batch-tokens, '32768', --auto-truncate, 'false', --max-client-batch-size, '4']"
  , "    artifacts:"
  , "    - path: text-embeddings-router"
  , "      bytes: " <> show (BS.length routerBytes)
  , "      sha256: " <> sha256Bytes routerBytes
  , "    - path: entrypoint.sh"
  , "      bytes: " <> show (BS.length entrypointBytes)
  , "      sha256: " <> sha256Bytes entrypointBytes
  , "model:"
  , "  id: Alibaba-NLP/gte-Qwen2-1.5B-instruct"
  , "  revision: 1cad2ab3ff41c2671f34e135d29831368ee26b68"
  , "  artifact_root: model"
  , "  retrieval_phase: build-time-only"
  , "  runtime_use: local-read-only-snapshot"
  , "  serving:"
  , "    backend: candle-cuda"
  , "    dtype: float16"
  , "    pooling: last-valid-token"
  , "    normalize: true"
  , "    auto_truncate: false"
  , "    max_input_tokens: 32768"
  , "    hmem_max_formatted_utf8_bytes: 32767"
  , "  artifacts:"
  ] <> concatMap artifactLines modelFiles
  where
    artifactLines (path, bytes) =
      ["  - path: " <> path, "    bytes: " <> show (BS.length bytes), "    sha256: " <> sha256Bytes bytes]

sha256Bytes :: BS.ByteString -> String
sha256Bytes bytes = show (hash bytes :: Digest SHA256)

withManagedFixtureRoot :: (FilePath -> IO value) -> IO value
withManagedFixtureRoot action = do
  temporaryDirectory <- getTemporaryDirectory
  (root, rootHandle) <- openTempFile temporaryDirectory "hmem-managed-tei-fixture"
  hClose rootHandle
  removeFile root
  createDirectoryIfMissing True root
  action root `finally` removePathForcibly root

managedFixtureConfig :: FilePath -> ManagedTeiConfig
managedFixtureConfig root = config
  { installedRoot = root
  , manifestPath = root </> "manifest" </> "managed-embedding-provenance.yaml"
  , artifactBundleRoot = root
  , executable = root </> "tei-runtime" </> "entrypoint.sh"
  , modelSnapshot = root </> "model"
  }

testLaunch :: ManagedTeiLaunch
testLaunch = ManagedTeiLaunch
  { childExecutable = "unverified"
  , modelSnapshotPath = "unverified"
  , endpoint = "http://127.0.0.1:43123"
  , metricsPort = 0
  , arguments = ["--model-id", "unverified"]
  , childEnvironment = []
  }

config :: ManagedTeiConfig
config = defaultManagedTeiConfig
  { executable = "not-a-shell"
  , modelSnapshot = "snapshot"
  , expectedModelId = "Alibaba-NLP/gte-Qwen2-1.5B-instruct"
  , servedModelName = "Alibaba-NLP/gte-Qwen2-1.5B-instruct"
  , expectedDimensions = 1536
  , readinessTimeoutMicros = 100000
  , baseBackoffMicros = 1
  }

expectedIdentity :: ManagedTeiIdentity
expectedIdentity = ManagedTeiIdentity
  "snapshot"
  "Alibaba-NLP/gte-Qwen2-1.5B-instruct"
  "1cad2ab3ff41c2671f34e135d29831368ee26b68"
  "last_token"
  32768
  32768
  False
  1536

awaitChildPid :: ManagedTeiChild -> FilePath -> Maybe FilePath -> Maybe FilePath -> Maybe FilePath -> Maybe FilePath -> IO Int
awaitChildPid _ path failureMarker startedMarker contextMarker afterStartMarker = do
  result <- timeout 5000000 waitForPid
  case result of
    Nothing -> do
      enteredPayload <- maybe (pure False) doesFileExist startedMarker
      runtimeContext <- maybe (pure "context unavailable") readFile contextMarker
      afterStart <- maybe (pure "after-start unavailable") readFile afterStartMarker
      expectationFailure (if enteredPayload
        then "managed child entered its command payload but did not publish its descendant pid before the startup deadline: " <> runtimeContext <> "; " <> afterStart
        else "managed child did not enter its command payload before the startup deadline")
      fail "unreachable"
    Just pid -> pure pid
  where
    waitForPid = do
      failureExists <- maybe (pure False) doesFileExist failureMarker
      when failureExists $ do
        failureKind <- maybe (pure "unknown") readFile failureMarker
        expectationFailure ("managed synthetic child failed to start its descendant: " <> failureKind)
        fail "unreachable"
      exists <- doesFileExist path
      if exists
        then do
          value <- readFile path
          if null value then threadDelay 10000 >> waitForPid else pure (read value)
        else threadDelay 10000 >> waitForPid

removeIfExists :: FilePath -> IO ()
removeIfExists path = do
  exists <- doesFileExist path
  when exists (removeFile path)

processExists :: Int -> IO Bool
processExists pid = do
  (_, output, _) <- readProcessWithExitCode "tasklist" ["/FI", "PID eq " <> show pid, "/FO", "CSV", "/NH"] ""
  pure (not ("INFO:" `isPrefixOf` output))

awaitNotRunning :: Int -> IO ()
awaitNotRunning pid = do
  running <- processExists pid
  when running (threadDelay 10000 >> awaitNotRunning pid)

posixProcessExists :: Int -> IO Bool
posixProcessExists pid = do
  (exitCode, _, _) <- readProcessWithExitCode "kill" ["-0", show pid] ""
  pure (exitCode == ExitSuccess)

awaitPosixNotRunning :: Int -> IO ()
awaitPosixNotRunning pid = do
  running <- posixProcessExists pid
  when running (threadDelay 10000 >> awaitPosixNotRunning pid)

registerChild :: (ManagedTeiChild -> IO ()) -> ManagedTeiChild -> IO ManagedTeiChild
registerChild register child = register child >> pure child

registeredActive :: (ManagedTeiActive -> IO ()) -> ManagedTeiActive -> IO (Either ManagedTeiActivationFailure ManagedTeiActive)
registeredActive register active = register active >> pure (Right active)

fakeDeps
  :: IORef [[String]]
  -> IO (Either () ManagedTeiIdentity)
  -> IO ExitCode
  -> ManagedTeiDeps
fakeDeps launches readiness childExit = unsafePerformIO $ do
  ports <- newIORef [43123, 43124]
  let nextPort = atomicModifyIORef' ports $ \values -> case values of
        value : remaining -> (remaining, value)
        [] -> ([], 43124)
  pure $ ManagedTeiDeps
    { selectLoopbackPort = nextPort
    , prepareLaunch = \_ launch -> pure (Right launch)
    , spawnChild = \_ launch register -> do
        modifyIORef' launches (<> [launch.arguments])
        registerChild register ManagedTeiChild { waitForChildExit = childExit, terminateAndReap = pure (), childProcessId = Nothing, childProcessGroupId = Nothing }
    , probeIdentity = \_ -> readiness
    , monotonicNow = getMonotonicTimeNSec
    , sleepMicros = \_ -> pure ()
    , jitterMicros = \_ -> pure 0
    , emit = \_ -> pure ()
    }

neverExit :: IO ExitCode
neverExit = threadDelay (60 * 1000 * 1000) >> pure ExitSuccess

neverReady :: IO (Either () ManagedTeiIdentity)
neverReady = threadDelay (60 * 1000 * 1000) >> pure (Right expectedIdentity)

waitForGeneration :: ManagedTeiRuntime -> Word64 -> IO ()
waitForGeneration runtime expected = do
  actual <- runtime.generation
  if actual >= expected then pure () else threadDelay 10000 >> waitForGeneration runtime expected

waitForState :: ManagedTeiRuntime -> ManagedTeiState -> IO ()
waitForState runtime expected = go (100 :: Int)
  where
    go remaining = do
      actual <- managedTeiState runtime
      if actual == expected
        then pure ()
        else if remaining == 0
          then expectationFailure ("expected state " <> show expected <> ", got " <> show actual)
          else threadDelay 10000 >> go (remaining - 1)
