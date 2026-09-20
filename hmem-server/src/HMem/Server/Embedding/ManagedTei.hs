{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Supervision for the one locally managed TEI profile.  This module owns
-- process lifetime only: it never accepts input text, exposes a child port, or
-- changes the provider selected by configuration.
module HMem.Server.Embedding.ManagedTei
  ( ManagedTeiConfig(..)
  , ManagedTeiIdentity(..)
  , ManagedTeiState(..)
  , ManagedTeiEvent(..)
  , ManagedTeiLaunch(..)
  , ManagedTeiChild(..)
  , ManagedTeiDeps(..)
  , ManagedTeiHooks(..)
  , ManagedTeiActivationFailure(..)
  , ManagedTeiActive(..)
  , ManagedTeiRuntime(..)
  , defaultManagedTeiConfig
  , defaultManagedTeiDeps
  , embeddedManagedTeiManifest
  , verifyManagedTeiLaunch
  , verifyManagedTeiLaunchAgainst
  , withManagedTeiEmitter
  , managedTeiEventText
  , validateManagedTeiIdentity
  , startManagedTei
  , startManagedTeiWith
  , managedTeiState
  , awaitManagedTeiEndpoint
  , stopManagedTei
  , awaitManagedAvailability
  , decodeTeiInfoModelId
  , decodeTeiReadinessEmbedding
  , spawnDefaultManagedTeiChild
  , spawnDefaultManagedTeiChildWithPublicationBarrier
  , classifyProcessGroupErrno
  , parseOwnedProcessGroupId
  , awaitProcessGroupAbsent
  , embeddingModeStartsChild
  , withManagedTeiLifecycle
  , withEmbeddingWorkerMonitor
  , withPreservingCleanup
  , withExternalActivation
  , runAllReleases
  , shutdownEmbeddingLifecycle
  ) where

import Control.Concurrent (modifyMVar, newMVar, threadDelay)
import Control.Concurrent.Async (Async, async, asyncWithUnmask, cancel, race, waitCatch)
import Control.Concurrent.STM
  ( TVar, atomically, newTVarIO, readTVar, readTVarIO, throwSTM, writeTVar )
import Control.Exception (SomeAsyncException, SomeException, finally, fromException, mask, onException, throwIO, toException, try)
import Control.Monad (forM, unless, void, when)
import Crypto.Hash (Context, hashFinalize, hashInit, hashUpdate)
import Crypto.Hash.Algorithms (SHA256)
import Data.Aeson (FromJSON(..), (.:), (.:?))
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.Either (lefts)
import Data.FileEmbed (embedFile, makeRelativeToProject)
import Data.List (nub, sort)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word64)
import Foreign.C.Error (Errno, eSRCH)
import Foreign.C.Types (CInt(..))
#if !defined(mingw32_HOST_OS)
import Foreign.C.Error (getErrno)
#endif
import GHC.Clock (getMonotonicTimeNSec)
import Data.Yaml qualified as Yaml
import Data.ByteString.Lazy qualified as LBS
import Network.Socket
  ( Family(AF_INET), SocketType(Stream), bind, close, defaultProtocol
  , getSocketName, socket
  , withSocketsDo, SockAddr(SockAddrInet) )
import System.Exit (ExitCode(..))
import System.IO (Handle, IOMode(ReadMode), hClose, withBinaryFile)
import System.Info (os)
import System.Environment (lookupEnv)
import System.Directory
  ( canonicalizePath, doesDirectoryExist, doesFileExist, doesPathExist
  , getFileSize, getPermissions, getTemporaryDirectory, listDirectory, pathIsSymbolicLink )
import System.Directory qualified as Directory
import System.FilePath
  ( (</>), isAbsolute, makeRelative, normalise, splitDirectories
  , takeDirectory, takeDrive )
import System.Process
  ( CreateProcess(..), ProcessHandle, StdStream(CreatePipe, NoStream), createProcess
  , getPid, getProcessExitCode, interruptProcessGroupOf, proc, readProcessWithExitCode
  , terminateProcess, waitForProcess )
import System.Random (randomRIO)
import System.Timeout (timeout)
import Text.Read (readMaybe)

import HMem.Server.Embedding.GteQwen2 qualified as GteQwen2
import HMem.Server.Embedding.Http (decodeEmbeddingResponse)
import HMem.Server.Embedding.Provider (EmbeddingAvailability(..), EmbeddingErrorCode(..), EmbeddingFailure(..))
import HMem.Config (EmbeddingProviderMode(..))

#if !defined(mingw32_HOST_OS)
foreign import ccall unsafe "kill" cKill :: CInt -> CInt -> IO CInt
foreign import capi "signal.h value SIGKILL" cSigKill :: CInt
#endif

-- | Runtime values are deliberately supplied by the packaging layer rather
-- than loaded from arbitrary process environment values.  The argument vector
-- is built below and is never rendered in events.
data ManagedTeiConfig = ManagedTeiConfig
  { installedRoot :: !FilePath
  , manifestPath :: !FilePath
  , artifactBundleRoot :: !FilePath
  , executable :: !FilePath
  , modelSnapshot :: !FilePath
  , expectedModelId :: !Text
  , servedModelName :: !Text
  , expectedPooling :: !Text
  , expectedDimensions :: !Int
  , readinessTimeoutMicros :: !Int
  , restartBudget :: !Int
  , restartWindowSeconds :: !Int
  , baseBackoffMicros :: !Int
  } deriving stock (Show, Eq)

data ManagedTeiIdentity = ManagedTeiIdentity
  { modelId :: !Text -- ^ TEI's local snapshot /model_id.
  , servedModelId :: !Text -- ^ TEI's canonical /served_model_name.
  , modelRevision :: !Text
  , pooling :: !Text -- ^ The pool selected from the pinned artifact.
  , maxInputLength :: !Int
  , maxBatchTokens :: !Int
  , autoTruncate :: !Bool
  , dimensions :: !Int
  } deriving stock (Show, Eq)

data ManagedTeiState
  = ManagedTeiStarting
  | ManagedTeiReady !Text
  | ManagedTeiDegraded
  | ManagedTeiStopped
  deriving stock (Show, Eq)

-- | Events intentionally carry no child output, command line, endpoint other
-- than the already loopback-only ready endpoint, or environment values.
data ManagedTeiEvent
  = ManagedTeiSpawned
  | ManagedTeiReadyEvent
  | ManagedTeiReadinessFailed
  | ManagedTeiPreflightFailed
  | ManagedTeiChildExited
  | ManagedTeiWorkerFailed
  | ManagedTeiProviderUnavailable
  | ManagedTeiRestarting !Int
  | ManagedTeiDegradedEvent
  | ManagedTeiStoppedEvent
  | ManagedTeiOutputSuppressed !Text
  deriving stock (Show, Eq)

data ManagedTeiLaunch = ManagedTeiLaunch
  { childExecutable :: !FilePath
  , modelSnapshotPath :: !FilePath
  , endpoint :: !Text
  , metricsPort :: !Int
  , arguments :: ![String]
  , childEnvironment :: ![(String, String)]
  } deriving stock (Show, Eq)

-- | The child is an injected resource boundary.  Tests use in-memory children;
-- the default implementation uses an argument-vector 'CreateProcess'.
data ManagedTeiChild = ManagedTeiChild
  { waitForChildExit :: !(IO ExitCode)
  , terminateAndReap :: !(IO ())
  , childProcessId :: !(Maybe String)
  , childProcessGroupId :: !(Maybe String)
  }

data ManagedTeiDeps = ManagedTeiDeps
  { selectLoopbackPort :: !(IO Int)
  , prepareLaunch :: !(ManagedTeiConfig -> ManagedTeiLaunch -> IO (Either () ManagedTeiLaunch))
  , spawnChild :: !((ManagedTeiEvent -> IO ()) -> ManagedTeiLaunch -> (ManagedTeiChild -> IO ()) -> IO ManagedTeiChild)
  , probeIdentity :: !(Text -> IO (Either () ManagedTeiIdentity))
  , monotonicNow :: !(IO Word64)
  , sleepMicros :: !(Int -> IO ())
  , jitterMicros :: !(Int -> IO Int)
  , emit :: !(ManagedTeiEvent -> IO ())
  }

data ManagedTeiRuntime = ManagedTeiRuntime
  { state :: !(IO ManagedTeiState)
  , generation :: !(IO Word64)
  , shutdown :: !(IO ())
  }

-- | All callbacks run on the single lifecycle owner. The activation callback
-- must leave the target disabled if it fails or is cancelled. An active value
-- owns a fresh provider, worker and its cancellation path for one child only.
data ManagedTeiHooks = ManagedTeiHooks
  { clearTargetBeforeLaunch :: !(IO ())
  , activateGeneration :: !(Word64 -> Text -> IO Int -> (ManagedTeiActive -> IO ()) -> IO (Either ManagedTeiActivationFailure ManagedTeiActive))
  }

data ManagedTeiActivationFailure
  = ManagedTeiActivationRetryable
  | ManagedTeiActivationPermanent
  deriving stock (Show, Eq)

data ManagedTeiActive = ManagedTeiActive
  { activeInvalidate :: !(IO ())
  , activeStopAndJoin :: !(IO ())
  , activeDisableTarget :: !(IO ())
  , activeWorkerOutcome :: !(IO ())
  , activeAvailable :: !(IO Bool)
  }

-- | The only mode that may start a local child.  Keeping this decision at the
-- lifecycle boundary makes disabled and external HTTP paths auditable.
embeddingModeStartsChild :: EmbeddingProviderMode -> Bool
embeddingModeStartsChild EmbeddingProviderManagedTei = True
embeddingModeStartsChild _ = False

-- | Shutdown order is intentional: stop work first, then the child and its
-- transport, before database and logger teardown.
shutdownEmbeddingLifecycle :: IO () -> IO () -> IO () -> IO () -> IO () -> IO ()
shutdownEmbeddingLifecycle stopWorker stopChild closeTransport stopDatabase closeLogger =
  runAllReleases [stopWorker, stopChild, closeTransport, stopDatabase, closeLogger]

-- | Release every acquired component in order, then preserve the first error.
-- This keeps cleanup deterministic without hiding a failed release from Warp.
runAllReleases :: [IO ()] -> IO ()
runAllReleases releases = do
  outcomes <- mapM (try @SomeException) releases
  case lefts outcomes of
    firstError : _ -> throwIO firstError
    [] -> pure ()

-- | Cleanup cannot replace a primary failure from Warp or startup.
withPreservingCleanup :: IO result -> IO () -> IO result
withPreservingCleanup action release = mask $ \restore -> do
  actionResult <- try @SomeException (restore action)
  releaseResult <- try @SomeException release
  case actionResult of
    Left primaryFailure -> throwIO primaryFailure
    Right value -> either throwIO (const (pure value)) releaseResult

-- | The activation callback registers physical ownership before it returns.
-- Cancellation at the unmasked acquisition handoff can then still find and
-- join the worker, even if the successful result never reaches this scope.
withExternalActivation :: ((active -> IO ()) -> IO (Either failure active)) -> (active -> IO ()) -> (active -> IO ()) -> IO (Either failure ())
withExternalActivation acquire use release = mask $ \restore -> do
  owned <- newTVarIO Nothing
  let register active = atomically $ do
        current <- readTVar owned
        case current of
          Nothing -> writeTVar owned (Just active)
          Just _ -> throwSTM (userError "external embedding activation registered twice")
      releaseRegistered = do
        current <- atomically $ do
          active <- readTVar owned
          writeTVar owned Nothing
          pure active
        maybe (pure ()) release current
  acquired <- restore (acquire register) `onException` releaseRegistered
  case acquired of
    Left failure -> releaseRegistered >> pure (Left failure)
    Right active -> do
      readTVarIO owned >>= \case
        Nothing -> release active >> throwIO (userError "external embedding activation was not registered")
        Just registered -> restore (use registered) `finally` releaseRegistered
      pure (Right ())

-- | Acquire the managed embedding chain as one bracket.  This is deliberately
-- parameterised over the concrete provider and worker so Main can use the
-- same failure semantics that lifecycle tests exercise: every resource already
-- acquired is released in reverse dependency order, while the originating
-- acquisition or handoff exception remains the one reported to Warp.
--
-- The transport belongs after readiness because it is only useful once a
-- managed endpoint is available; it is nevertheless acquired for external
-- HTTP mode too, where 'awaitEndpoint' simply returns the configured absence.
withManagedTeiLifecycle
  :: IO managed
  -> (managed -> IO endpoint)
  -> IO transport
  -> (transport -> endpoint -> IO provider)
  -> (provider -> IO worker)
  -> (worker -> IO ())
  -> (transport -> IO ())
  -> (managed -> IO ())
  -> (endpoint -> provider -> worker -> IO result)
  -> IO result
withManagedTeiLifecycle acquireManaged awaitEndpoint acquireTransport acquireProvider acquireWorker releaseWorker releaseTransport releaseManaged handoff = mask $ \restore -> do
  managed <- acquireManaged
  endpoint <- acquireAfter [releaseManaged managed] (restore (awaitEndpoint managed))
  transport <- acquireAfter [releaseManaged managed] (restore acquireTransport)
  provider <- acquireAfter [releaseManaged managed, releaseTransport transport] (restore (acquireProvider transport endpoint))
  worker <- acquireAfter [releaseManaged managed, releaseTransport transport] (restore (acquireWorker provider))
  handoffResult <- try @SomeException (restore (handoff endpoint provider worker))
  releaseResult <- try @SomeException $ runAllReleases
    [ releaseWorker worker
    , releaseManaged managed
    , releaseTransport transport
    ]
  case handoffResult of
    Left primaryFailure -> throwIO primaryFailure
    Right value -> either throwIO (const (pure value)) releaseResult
  where
    acquireAfter releases action = do
      result <- try @SomeException action
      case result of
        Right value -> pure value
        Left primaryFailure -> do
          -- Cleanup failures are secondary to the original startup failure.
          void $ try @SomeException (runAllReleases releases)
          throwIO primaryFailure

defaultManagedTeiConfig :: ManagedTeiConfig
defaultManagedTeiConfig =
  let GteQwen2.GteQwen2Requirements { modelId = requiredModelId, dimensions = requiredDimensions } = GteQwen2.gteQwen2Requirements
   in ManagedTeiConfig
         { installedRoot = "/opt/hmem/managed-embedding"
         , manifestPath = "/opt/hmem/managed-embedding/manifest/managed-embedding-provenance.yaml"
         , artifactBundleRoot = "/opt/hmem/managed-embedding"
         , executable = "/opt/hmem/managed-embedding/tei-runtime/entrypoint.sh"
         , modelSnapshot = "/opt/hmem/managed-embedding/model"
         , expectedModelId = requiredModelId
         , servedModelName = requiredModelId
        , expectedPooling = "last_token"
        , expectedDimensions = requiredDimensions
        , readinessTimeoutMicros = 120 * 1000 * 1000
        , restartBudget = 3
        , restartWindowSeconds = 5 * 60
        , baseBackoffMicros = 250 * 1000
         }

-- | Build-time trust anchor.  Packaging installs an exact copy at the fixed
-- manifest location; startup accepts no mutable manifest as its own authority.
embeddedManagedTeiManifest :: BS.ByteString
embeddedManagedTeiManifest = $(makeRelativeToProject "../config/managed-embedding-provenance.yaml" >>= embedFile)

-- | Render only stable event names.  In particular, child output is never
-- interpolated into the server log.
managedTeiEventText :: ManagedTeiEvent -> Text
managedTeiEventText = \case
  ManagedTeiSpawned -> "managed-tei child spawned"
  ManagedTeiReadyEvent -> "managed-tei ready"
  ManagedTeiReadinessFailed -> "managed-tei readiness failed"
  ManagedTeiPreflightFailed -> "managed-tei unavailable (managed_tei_preflight_failed)"
  ManagedTeiChildExited -> "managed-tei child exited"
  ManagedTeiWorkerFailed -> "managed-tei worker stopped"
  ManagedTeiProviderUnavailable -> "managed-tei provider unavailable"
  ManagedTeiRestarting attempt -> "managed-tei restarting (attempt " <> T.pack (show attempt) <> ")"
  ManagedTeiDegradedEvent -> "managed-tei degraded; restart budget exhausted"
  ManagedTeiStoppedEvent -> "managed-tei stopped"
  ManagedTeiOutputSuppressed stream -> "managed-tei " <> stream <> " output suppressed"

validateManagedTeiIdentity :: ManagedTeiConfig -> ManagedTeiIdentity -> Bool
validateManagedTeiIdentity config identity =
  validateManagedTeiIdentityAt config.modelSnapshot config identity

validateManagedTeiIdentityAt :: FilePath -> ManagedTeiConfig -> ManagedTeiIdentity -> Bool
validateManagedTeiIdentityAt expectedSnapshot config identity =
  let requirements = GteQwen2.gteQwen2Requirements
      aliases = [T.pack expectedSnapshot, "/model", config.expectedModelId]
   in identity.modelId `elem` aliases
      && identity.servedModelId `elem` aliases
      && (identity.modelRevision == requirements.modelRevision || T.null identity.modelRevision)
      && identity.pooling == config.expectedPooling
      && identity.maxInputLength == requirements.tokenizerMaxLength
      && identity.maxBatchTokens >= requirements.maxBatchTokens
      && identity.autoTruncate == requirements.autoTruncate
      && identity.dimensions == config.expectedDimensions

-- | Treat any worker completion while Warp is active as a lifecycle failure.
-- The callback and thrown message are fixed so neither the worker exception nor
-- any lease/provider value can reach logs.
withEmbeddingWorkerMonitor :: IO () -> Maybe (Async ()) -> IO result -> IO result
withEmbeddingWorkerMonitor _ Nothing runServer = runServer
withEmbeddingWorkerMonitor reportFailure (Just worker) runServer = do
  outcome <- race runServer (waitCatch worker)
  case outcome of
    Left result -> pure result
    Right _ -> do
      reportFailure
      throwIO (userError "embedding lifecycle failed (embedding_worker_failed)")

trySynchronous :: IO value -> IO (Either SomeException value)
trySynchronous action = do
  outcome <- try @SomeException action
  case outcome of
    Left failure | Just asyncFailure <- fromException failure -> throwIO (asyncFailure :: SomeAsyncException)
    _ -> pure outcome

-- | The supervisor starts asynchronously so server startup cannot block on a
-- missing local artifact.  Managed mode remains managed-only: failures become
-- degraded, never an HTTP fallback.
startManagedTei :: ManagedTeiConfig -> ManagedTeiDeps -> IO ManagedTeiRuntime
startManagedTei config deps = startManagedTeiWith config deps ManagedTeiHooks
  { clearTargetBeforeLaunch = pure ()
  , activateGeneration = \_ _ _ register -> do
      let active = ManagedTeiActive
            { activeInvalidate = pure ()
            , activeStopAndJoin = pure ()
            , activeDisableTarget = pure ()
            , activeWorkerOutcome = threadDelay maxBound
            , activeAvailable = pure True
            }
      register active
      pure (Right active)
  }

-- | One controller owns every generation transition. A result from an old
-- child can never publish readiness for a replacement, including a child at
-- the same loopback URL. Shutdown waits for this owner before returning.
startManagedTeiWith :: ManagedTeiConfig -> ManagedTeiDeps -> ManagedTeiHooks -> IO ManagedTeiRuntime
startManagedTeiWith config deps hooks = do
  lifecycleState <- newTVarIO ManagedTeiStarting
  currentChild <- newTVarIO Nothing
  currentActive <- newTVarIO Nothing
  currentGeneration <- newTVarIO 0
  startupDeadline <- newTVarIO Nothing
  stopped <- newTVarIO False
  supervisor <- asyncWithUnmask $ \unmask -> unmask (superviseLifecycle lifecycleState
    (runSupervisor lifecycleState startupDeadline currentGeneration currentChild currentActive stopped []
      `finally` retireStartupBefore startupDeadline currentActive currentChild))
  pure ManagedTeiRuntime
    { state = readTVarIO lifecycleState
    , generation = readTVarIO currentGeneration
    , shutdown = do
        atomically $ writeTVar stopped True
        cancel supervisor
        supervisorOutcome <- waitCatch supervisor
        cleanupOutcome <- trySynchronous (retireStartupBefore startupDeadline currentActive currentChild)
        let cleanSupervisor = case supervisorOutcome of
              Left failure -> case fromException failure of
                Just (_ :: SomeAsyncException) -> True
                Nothing -> False
              Right () -> True
        case cleanupOutcome of
          Right () | cleanSupervisor -> do
            atomically $ writeTVar lifecycleState ManagedTeiStopped
            deps.emit ManagedTeiStoppedEvent
          _ -> do
            atomically $ writeTVar lifecycleState ManagedTeiDegraded
            cleanupFailed
    }
  where
    superviseLifecycle lifecycleState action = do
      outcome <- trySynchronous action
      case outcome of
        Right () -> pure ()
        Left _ -> do
          atomically $ writeTVar lifecycleState ManagedTeiDegraded
          void (trySynchronous (deps.emit ManagedTeiDegradedEvent))
          cleanupFailed

    runSupervisor lifecycleState startupDeadline currentGeneration currentChild currentActive stopped restartTimes = do
      started <- getMonotonicTimeNSec
      let deadline = started + fromIntegral (max 0 config.readinessTimeoutMicros) * 1000
          -- Reserve an honest part of the one startup ceiling for joined
          -- worker/provider and child cleanup after a failed attempt.
          cleanupReserve = min 20000000 (max 0 config.readinessTimeoutMicros `div` 6)
          workDeadline = deadline - fromIntegral cleanupReserve * 1000
      atomically $ writeTVar startupDeadline (Just deadline)
      token <- atomically $ do
        previous <- readTVar currentGeneration
        let next = previous + 1
        writeTVar currentGeneration next
        pure next
      port <- maybe (fail "managed loopback admission timed out") pure
        =<< withinDeadline workDeadline deps.selectLoopbackPort
      let metrics = 0
      let loopbackEndpoint = "http://127.0.0.1:" <> T.pack (show port)
          launch = ManagedTeiLaunch
            { childExecutable = config.executable
            , modelSnapshotPath = config.modelSnapshot
            , endpoint = loopbackEndpoint
            , metricsPort = metrics
            , arguments = managedTeiArguments config port metrics
            , childEnvironment = []
            }
      cleared <- withinDeadline workDeadline hooks.clearTargetBeforeLaunch
      when (cleared == Nothing) (fail "managed target clearing timed out")
      -- Publish the fully constructed child into supervisor ownership while
      -- masked.  Once create/drain construction succeeds, either this TVar
      -- owns it or the spawning action has synchronously reaped it.
      preparedResult <- withinDeadline workDeadline (deps.prepareLaunch config launch)
      case preparedResult of
        Nothing -> preflightFailure lifecycleState
        Just (Left ()) -> preflightFailure lifecycleState
        Just (Right verifiedLaunch) -> do
          spawned <- do
            remaining <- remainingMicros workDeadline
            result <- trySynchronous $ timeout remaining $ deps.spawnChild
              deps.emit verifiedLaunch (\child -> atomically (writeTVar currentChild (Just child)))
            case result of
              Left failure -> pure (Left failure)
              Right Nothing -> pure (Left (toException (userError "managed launch timed out")))
              Right (Just child) -> do
                registered <- readTVarIO currentChild
                case registered of
                  Just _ -> pure (Right child)
                  Nothing -> child.terminateAndReap >> pure (Left (toException (userError "managed child was not registered")))
          superviseSpawn lifecycleState startupDeadline currentGeneration currentChild currentActive stopped restartTimes token workDeadline loopbackEndpoint verifiedLaunch spawned

    preflightFailure lifecycleState = do
      atomically $ writeTVar lifecycleState ManagedTeiDegraded
      deps.emit ManagedTeiPreflightFailed

    superviseSpawn lifecycleState startupDeadline currentGeneration currentChild currentActive stopped restartTimes token workDeadline loopbackEndpoint verifiedLaunch = \case
      Left _ -> do
        deps.emit ManagedTeiReadinessFailed
        retireStartupBefore startupDeadline currentActive currentChild
        recover lifecycleState startupDeadline currentGeneration currentChild currentActive stopped restartTimes
      Right child -> do
        deps.emit ManagedTeiSpawned
        readinessAttempt <- trySynchronous (race child.waitForChildExit
          (withinDeadline workDeadline (deps.probeIdentity loopbackEndpoint)))
        case readinessAttempt of
          Right (Right (Just (Right identity)))
            | validateManagedTeiIdentityAt verifiedLaunch.modelSnapshotPath config identity -> do
                activated <- trySynchronous (race child.waitForChildExit
                  (withinDeadline workDeadline (hooks.activateGeneration token loopbackEndpoint
                    (remainingMicros workDeadline)
                    (\active -> atomically (writeTVar currentActive (Just (token, active)))))))
                case activated of
                  Right (Right (Just (Right active))) -> do
                    registered <- readTVarIO currentActive
                    case registered of
                      Just (activeToken, _) | activeToken == token -> pure ()
                      _ -> retireActive active >> fail "managed generation was not registered"
                    published <- mask $ \_ -> do
                      remaining <- remainingMicros workDeadline
                      stillCurrent <- atomically $ do
                        latest <- readTVar currentGeneration
                        stopping <- readTVar stopped
                        if latest == token && not stopping && remaining > 0
                          then do
                            writeTVar lifecycleState (ManagedTeiReady loopbackEndpoint)
                            writeTVar startupDeadline Nothing
                            pure True
                          else pure False
                      pure stillCurrent
                    when published $ do
                      deps.emit ManagedTeiReadyEvent
                      readyAt <- deps.monotonicNow
                      outcome <- race child.waitForChildExit
                        (race active.activeWorkerOutcome (awaitProviderLoss active))
                      deps.emit $ case outcome of
                        Left _ -> ManagedTeiChildExited
                        Right (Left _) -> ManagedTeiWorkerFailed
                        Right (Right _) -> ManagedTeiProviderUnavailable
                      finishedAt <- deps.monotonicNow
                      let healthyNanos = fromIntegral (max 1 config.restartWindowSeconds) * 1000000000
                          nextHistory = if finishedAt - readyAt >= healthyNanos then [] else restartTimes
                      atomically $ writeTVar lifecycleState ManagedTeiDegraded
                      retireCurrent currentActive currentChild
                      recover lifecycleState startupDeadline currentGeneration currentChild currentActive stopped nextHistory
                    unless published $ do
                      atomically $ writeTVar lifecycleState ManagedTeiDegraded
                      retireStartupBefore startupDeadline currentActive currentChild
                  Right (Right (Just (Left ManagedTeiActivationPermanent))) -> do
                    deps.emit ManagedTeiReadinessFailed
                    atomically $ writeTVar lifecycleState ManagedTeiDegraded
                    retireStartupBefore startupDeadline currentActive currentChild
                  _ -> readinessFailure lifecycleState startupDeadline currentChild currentActive stopped restartTimes currentGeneration
          Right (Left _) -> do
            deps.emit ManagedTeiChildExited
            readinessFailure lifecycleState startupDeadline currentChild currentActive stopped restartTimes currentGeneration
          _ -> readinessFailure lifecycleState startupDeadline currentChild currentActive stopped restartTimes currentGeneration

    readinessFailure lifecycleState startupDeadline currentChild currentActive stopped restartTimes currentGeneration = do
      deps.emit ManagedTeiReadinessFailed
      atomically $ writeTVar lifecycleState ManagedTeiDegraded
      retireStartupBefore startupDeadline currentActive currentChild
      recover lifecycleState startupDeadline currentGeneration currentChild currentActive stopped restartTimes

    awaitProviderLoss active = do
      checked <- trySynchronous (timeout 5000000 active.activeAvailable)
      case checked of
        Right (Just True) -> deps.sleepMicros 500000 >> awaitProviderLoss active
        _ -> pure ()

    recover lifecycleState startupDeadline currentGeneration currentChild currentActive stopped restartTimes = do
      isStopped <- readTVarIO stopped
      if isStopped
        then atomically $ writeTVar lifecycleState ManagedTeiStopped
        else do
          currentTime <- deps.monotonicNow
          let inWindow = filter (\previous -> currentTime - previous <= fromIntegral config.restartWindowSeconds * 1000000000) restartTimes
          if length inWindow >= config.restartBudget
            then do
              atomically $ writeTVar lifecycleState ManagedTeiDegraded
              deps.emit ManagedTeiDegradedEvent
            else do
              let attempt = length inWindow + 1
              atomically $ writeTVar lifecycleState ManagedTeiStarting
              deps.emit (ManagedTeiRestarting attempt)
              extraJitter <- deps.jitterMicros config.baseBackoffMicros
              deps.sleepMicros (config.baseBackoffMicros * (2 ^ (attempt - 1)) + max 0 extraJitter)
              runSupervisor lifecycleState startupDeadline currentGeneration currentChild currentActive stopped (currentTime : inWindow)

    retireStartupBefore startupDeadline currentActive currentChild = do
      ownedActive <- readTVarIO currentActive
      ownedChild <- readTVarIO currentChild
      case (ownedActive, ownedChild) of
        (Nothing, Nothing) -> pure ()
        _ -> readTVarIO startupDeadline >>= \case
          Nothing -> retireCurrent currentActive currentChild
          Just deadline -> do
            -- The deadline bounds ordinary acquisition and retirement. If a
            -- blocked OS operation crosses it, the owner must still attempt
            -- acknowledged release; an expired clock is never evidence that
            -- the child or worker has disappeared.
            outcome <- withinDeadline deadline (retireCurrent currentActive currentChild)
            case outcome of
              Just () -> pure ()
              Nothing -> retireCurrent currentActive currentChild >> cleanupFailed

    retireActive active = do
      invalidated <- trySynchronous active.activeInvalidate
      joined <- trySynchronous active.activeStopAndJoin
      -- The persisted target must remain enabled if joining could not return
      -- the worker's leases. A successful join permits disable even if the
      -- preceding invalidation reported an error.
      disabled <- case joined of
        Left _ -> pure (Right ())
        Right () -> trySynchronous active.activeDisableTarget
      case (invalidated, joined, disabled) of
        (Right (), Right (), Right ()) -> pure ()
        _ -> cleanupFailed

    retireCurrent currentActive currentChild = mask $ \_ -> do
      active <- readTVarIO currentActive
      activeResult <- try @SomeException $ case active of
        Nothing -> pure ()
        Just (_, owned) -> do
          retireActive owned
          atomically $ writeTVar currentActive Nothing
      -- A failed worker join or target disable must not strand the owned TEI
      -- child. Keep any unresolved active ownership for later retry and never
      -- publish a replacement after either failure.
      childResult <- try @SomeException (cleanupCurrent currentChild)
      case (activeResult, childResult) of
        (Left failure, _) -> throwIO failure
        (_, Left failure) -> throwIO failure
        _ -> pure ()

remainingMicros :: Word64 -> IO Int
remainingMicros deadline = do
  current <- getMonotonicTimeNSec
  pure $ if current >= deadline then 0
    else fromIntegral (min (fromIntegral (maxBound :: Int)) ((deadline - current) `div` 1000))

-- | Retry only the provider's transient availability result. The same
-- generation/provider is retained while TEI warms up; the supervisor supplies
-- the remaining part of its original startup deadline on every iteration.
awaitManagedAvailability :: IO Int -> IO EmbeddingAvailability -> IO EmbeddingAvailability
awaitManagedAvailability remaining availability = go
  where
    timedOut = EmbeddingUnavailable (EmbeddingFailure ProviderTimedOut True)
    go = do
      budget <- remaining
      if budget <= 10000
        then pure timedOut
        else timeout (budget - 10000) availability >>= \case
          Nothing -> pure timedOut
          Just EmbeddingAvailable -> pure EmbeddingAvailable
          Just unavailable@(EmbeddingUnavailable failure)
            | failure.retryable -> do
                left <- remaining
                if left <= 10000
                  then pure timedOut
                  else threadDelay (min 250000 (left - 10000)) >> go
            | otherwise -> pure unavailable
          Just EmbeddingDisabled -> pure EmbeddingDisabled

withinDeadline :: Word64 -> IO value -> IO (Maybe value)
withinDeadline deadline action = do
  remaining <- remainingMicros deadline
  if remaining <= 0 then pure Nothing else timeout remaining action

managedTeiArguments :: ManagedTeiConfig -> Int -> Int -> [String]
managedTeiArguments config port prometheusPort =
  let GteQwen2.GteQwen2Requirements { maxBatchTokens = maxTokens } = GteQwen2.gteQwen2Requirements
   in [ "--model-id", config.modelSnapshot
      , "--dtype", "float16"
      , "--hostname", "127.0.0.1"
      , "--port", show port
      , "--prometheus-port", show prometheusPort
      , "--auto-truncate", "false"
      , "--max-batch-tokens", show maxTokens
      , "--max-concurrent-requests", "4"
      , "--max-client-batch-size", "4"
      , "--tokenization-workers", "2"
      , "--json-output"
      ]

cleanupCurrent :: TVar (Maybe ManagedTeiChild) -> IO ()
cleanupCurrent currentChild = do
  child <- readTVarIO currentChild
  case child of
    Nothing -> pure ()
    Just activeChild -> do
      outcome <- trySynchronous activeChild.terminateAndReap
      case outcome of
        Left _ -> cleanupFailed
        Right () -> atomically $ writeTVar currentChild Nothing

cleanupFailed :: IO value
cleanupFailed =
  throwIO (userError "managed-tei lifecycle failed (managed_tei_cleanup_failed)")

data ManagedManifest = ManagedManifest
  { manifestSchemaVersion :: !Int
  , manifestProfile :: !Text
  , manifestDimensions :: !Int
  , manifestInstallation :: !InstallationManifest
  , manifestTei :: !TeiManifest
  , manifestModel :: !ModelManifest
  } deriving stock (Show, Eq)

data InstallationManifest = InstallationManifest
  { installationRoot :: !FilePath
  , installationManifestPath :: !FilePath
  , installationModelRoot :: !FilePath
  , installationRuntimeRoot :: !FilePath
  , installationEntrypointPath :: !FilePath
  , installationRouterPath :: !FilePath
  } deriving stock (Show, Eq)

data TeiManifest = TeiManifest
  { teiRelease :: !Text
  , teiSourceCommit :: !Text
  , teiCompatibility :: !SourceCompatibility
  , teiGpuImage :: !GpuImageManifest
  , teiRuntime :: !RuntimeManifest
  } deriving stock (Show, Eq)

data SourceCompatibility = SourceCompatibility
  { sourceBackend :: !Text
  , sourceModelClass :: !Text
  , sourceCompileCapability :: !Int
  , sourceRuntimeCapability :: !Int
  , sourceRequiresCuda :: !Bool
  , sourceRequiresF16 :: !Bool
  , sourceUsesIsCausal :: !Bool
  } deriving stock (Show, Eq)

data GpuImageManifest = GpuImageManifest
  { imageReference :: !Text
  , imagePlatform :: !Text
  , imageIndexDigest :: !Text
  , imagePlatformDigest :: !Text
  , imageConfigDigest :: !Text
  } deriving stock (Show, Eq)

data RuntimeManifest = RuntimeManifest
  { runtimeArtifactRoot :: !FilePath
  , runtimeImageReference :: !Text
  , runtimePlatformDigest :: !Text
  , runtimeConfigDigest :: !Text
  , runtimeSystemAbi :: !Text
  , runtimeRetrievalPhase :: !Text
  , runtimeUse :: !Text
  , runtimeLoader :: !LoaderManifest
  , runtimeArtifacts :: ![ManifestArtifact]
  } deriving stock (Show, Eq)

data LoaderManifest = LoaderManifest
  { loaderInheritsEnvironment :: !Bool
  , loaderWorkingDirectory :: !FilePath
  , loaderPath :: !FilePath
  , loaderLibraryPath :: !FilePath
  , loaderPreload :: !(Maybe FilePath)
  , loaderOffline :: !Text
  , loaderTransformersOffline :: !Text
  , loaderFlashAttention :: !Text
  , loaderArguments :: ![String]
  } deriving stock (Show, Eq)

data ModelManifest = ModelManifest
  { lockedModelId :: !Text
  , lockedModelRevision :: !Text
  , modelArtifactRoot :: !FilePath
  , modelRetrievalPhase :: !Text
  , modelRuntimeUse :: !Text
  , modelServing :: !ServingManifest
  , modelArtifacts :: ![ManifestArtifact]
  } deriving stock (Show, Eq)

data ServingManifest = ServingManifest
  { servingBackend :: !Text
  , servingDtype :: !Text
  , servingPooling :: !Text
  , servingNormalize :: !Bool
  , servingAutoTruncate :: !Bool
  , servingMaxInputTokens :: !Int
  , servingMaxBytes :: !Int
  } deriving stock (Show, Eq)

data ManifestArtifact = ManifestArtifact
  { artifactPath :: !FilePath
  , artifactBytes :: !Integer
  , artifactSha256 :: !String
  } deriving stock (Show, Eq, Ord)

instance FromJSON ManagedManifest where
  parseJSON = Aeson.withObject "managed TEI manifest" $ \value -> ManagedManifest
    <$> value .: "schema_version"
    <*> value .: "profile"
    <*> value .: "expected_embedding_dimensions"
    <*> value .: "installation"
    <*> value .: "tei"
    <*> value .: "model"

instance FromJSON InstallationManifest where
  parseJSON = Aeson.withObject "managed installation" $ \value -> InstallationManifest
    <$> value .: "root"
    <*> value .: "manifest_path"
    <*> value .: "model_root"
    <*> value .: "runtime_root"
    <*> value .: "entrypoint_path"
    <*> value .: "router_path"

instance FromJSON TeiManifest where
  parseJSON = Aeson.withObject "TEI manifest" $ \value -> TeiManifest
    <$> value .: "release"
    <*> value .: "source_commit"
    <*> value .: "source_compatibility"
    <*> value .: "gpu_image"
    <*> value .: "runtime_bundle"

instance FromJSON SourceCompatibility where
  parseJSON = Aeson.withObject "CUDA source compatibility" $ \value -> SourceCompatibility
    <$> value .: "backend"
    <*> value .: "model_class"
    <*> value .: "compile_compute_capability"
    <*> value .: "runtime_compute_capability"
    <*> value .: "requires_cuda"
    <*> value .: "requires_f16"
    <*> value .: "uses_config_is_causal"

instance FromJSON GpuImageManifest where
  parseJSON = Aeson.withObject "TEI GPU image" $ \value -> GpuImageManifest
    <$> value .: "reference"
    <*> (value .: "platform" >>= Aeson.withObject "GPU platform" (\platform -> do
      platformOs <- platform .: "os"
      architecture <- platform .: "architecture"
      pure (platformOs <> "/" <> architecture)))
    <*> value .: "index_digest"
    <*> value .: "platform_manifest_digest"
    <*> value .: "config_digest"

instance FromJSON RuntimeManifest where
  parseJSON = Aeson.withObject "TEI runtime bundle" $ \value -> RuntimeManifest
    <$> value .: "artifact_root"
    <*> value .: "source_image_reference"
    <*> value .: "source_platform_manifest_digest"
    <*> value .: "source_config_digest"
    <*> value .: "system_abi_source"
    <*> value .: "retrieval_phase"
    <*> value .: "runtime_use"
    <*> value .: "loader"
    <*> value .: "artifacts"

instance FromJSON LoaderManifest where
  parseJSON = Aeson.withObject "TEI loader" $ \value -> LoaderManifest
    <$> value .: "inherit_environment"
    <*> value .: "working_directory"
    <*> value .: "path"
    <*> value .: "ld_library_path"
    <*> value .: "ld_preload"
    <*> value .: "hf_hub_offline"
    <*> value .: "transformers_offline"
    <*> value .: "use_flash_attention"
    <*> value .: "argv"

instance FromJSON ModelManifest where
  parseJSON = Aeson.withObject "model manifest" $ \value -> ModelManifest
    <$> value .: "id"
    <*> value .: "revision"
    <*> value .: "artifact_root"
    <*> value .: "retrieval_phase"
    <*> value .: "runtime_use"
    <*> value .: "serving"
    <*> value .: "artifacts"

instance FromJSON ServingManifest where
  parseJSON = Aeson.withObject "model serving" $ \value -> ServingManifest
    <$> value .: "backend"
    <*> value .: "dtype"
    <*> value .: "pooling"
    <*> value .: "normalize"
    <*> value .: "auto_truncate"
    <*> value .: "max_input_tokens"
    <*> value .: "hmem_max_formatted_utf8_bytes"

instance FromJSON ManifestArtifact where
  parseJSON = Aeson.withObject "managed artifact" $ \value -> ManifestArtifact
    <$> value .: "path"
    <*> value .: "bytes"
    <*> value .: "sha256"

-- | Verify the installed bundle entirely in process.  All detailed failures
-- collapse to one opaque result so paths, hashes, and manifest contents never
-- reach events or logs.
verifyManagedTeiLaunch :: ManagedTeiConfig -> ManagedTeiLaunch -> IO (Either () ManagedTeiLaunch)
verifyManagedTeiLaunch config launch
  | os /= "linux" = pure (Left ())
  | otherwise = do
      gpu <- checkGpuContract
      if gpu then verifyManagedTeiLaunchAgainst embeddedManagedTeiManifest config launch
             else pure (Left ())

checkGpuContract :: IO Bool
checkGpuContract = do
  query <- trySynchronous (timeout 5000000 (readProcessWithExitCode "/usr/bin/nvidia-smi"
    ["--id=0", "--query-gpu=compute_cap,driver_version", "--format=csv,noheader"] ""))
  pure $ case query of
    Right (Just (ExitSuccess, output, _)) -> any qualified (lines (take 512 output))
    _ -> False
  where
    qualified line = case map T.strip (T.splitOn "," (T.pack line)) of
      capability : driver : _ -> capability == "12.0" && versionAtLeast driver
      _ -> False
    versionAtLeast value = case traverse readPart (T.splitOn "." value) of
      Just (major : minor : _) -> (major, minor) >= (596 :: Int, 49 :: Int)
      _ -> False
    readPart value = case reads (T.unpack value) of
      [(number, "")] -> Just number
      _ -> Nothing

-- | Exercise the same verifier against an explicit immutable authority.  The
-- production dependency always closes over 'embeddedManagedTeiManifest'; this
-- seam exists so tests can build small complete bundles with real hashes.
verifyManagedTeiLaunchAgainst :: BS.ByteString -> ManagedTeiConfig -> ManagedTeiLaunch -> IO (Either () ManagedTeiLaunch)
verifyManagedTeiLaunchAgainst authority config launch = do
  outcome <- trySynchronous (verifyManagedTeiLaunchUnsafe authority config launch)
  pure (either (const (Left ())) Right outcome)

verifyManagedTeiLaunchUnsafe :: BS.ByteString -> ManagedTeiConfig -> ManagedTeiLaunch -> IO ManagedTeiLaunch
verifyManagedTeiLaunchUnsafe authority config launch = do
  trusted <- decodeManifest authority
  requirePreflight (validManifest config trusted)
  requirePreflight (all isCanonicalAbsolute [config.installedRoot, config.manifestPath, config.artifactBundleRoot])
  mapM_ requireNoSymlinkComponents [config.installedRoot, config.manifestPath, config.artifactBundleRoot]
  requirePreflight =<< doesDirectoryExist config.installedRoot
  requirePreflight =<< doesDirectoryExist config.artifactBundleRoot
  requirePreflight =<< isRegularNonLink config.manifestPath
  canonicalInstalled <- canonicalizePath config.installedRoot
  canonicalManifest <- canonicalizePath config.manifestPath
  canonicalBundle <- canonicalizePath config.artifactBundleRoot
  requirePreflight (strictlyContained canonicalInstalled canonicalManifest)
  requirePreflight (canonicalInstalled == canonicalBundle || strictlyContained canonicalInstalled canonicalBundle)
  installedBytes <- BS.readFile canonicalManifest
  requirePreflight (installedBytes == authority)
  installed <- decodeManifest installedBytes
  requirePreflight (installed == trusted)
  runtimeRoot <- verifyArtifactTree canonicalBundle trusted.manifestTei.teiRuntime.runtimeArtifactRoot trusted.manifestTei.teiRuntime.runtimeArtifacts
  modelRoot <- verifyArtifactTree canonicalBundle trusted.manifestModel.modelArtifactRoot trusted.manifestModel.modelArtifacts
  entrypoint <- resolveLockedFile runtimeRoot "entrypoint.sh" trusted.manifestTei.teiRuntime.runtimeArtifacts
  router <- resolveLockedFile runtimeRoot "text-embeddings-router" trusted.manifestTei.teiRuntime.runtimeArtifacts
  permissions <- mapM getPermissions [entrypoint, router]
  requirePreflight (all Directory.executable permissions)
  let loader = trusted.manifestTei.teiRuntime.runtimeLoader
  pure launch
    { childExecutable = entrypoint
    , modelSnapshotPath = modelRoot
    , arguments = replaceModelId modelRoot launch.arguments
    , childEnvironment =
        [ ("PATH", loader.loaderPath)
        , ("LD_LIBRARY_PATH", loader.loaderLibraryPath)
        , ("HF_HUB_OFFLINE", T.unpack loader.loaderOffline)
        , ("TRANSFORMERS_OFFLINE", T.unpack loader.loaderTransformersOffline)
        , ("USE_FLASH_ATTENTION", T.unpack loader.loaderFlashAttention)
        , ("NVIDIA_VISIBLE_DEVICES", "0")
        , ("NVIDIA_DRIVER_CAPABILITIES", "compute,utility")
        ]
    }

decodeManifest :: BS.ByteString -> IO ManagedManifest
decodeManifest bytes = either (const (ioError (userError "invalid managed manifest"))) pure (Yaml.decodeEither' bytes)

validManifest :: ManagedTeiConfig -> ManagedManifest -> Bool
validManifest config manifest =
  let tei = manifest.manifestTei
      source = tei.teiCompatibility
      installation = manifest.manifestInstallation
      image = tei.teiGpuImage
      runtime = tei.teiRuntime
      loader = runtime.runtimeLoader
      model = manifest.manifestModel
      serving = model.modelServing
      runtimePaths = map (.artifactPath) runtime.runtimeArtifacts
      modelPaths = map (.artifactPath) model.modelArtifacts
      GteQwen2.GteQwen2Requirements
        { modelId = requiredModelId
        , modelRevision = requiredRevision
        , dimensions = requiredDimensions
        } = GteQwen2.gteQwen2Requirements
   in manifest.manifestSchemaVersion == 2
      && manifest.manifestProfile == "native-tei-gte-qwen2-1.5b-instruct-cuda-sm120-f16-v1"
      && manifest.manifestDimensions == requiredDimensions
      && config.expectedDimensions == requiredDimensions
      && config.expectedModelId == requiredModelId
      && config.servedModelName == requiredModelId
      && config.expectedPooling == "last_token"
      && installation.installationRoot == config.installedRoot
      && installation.installationManifestPath == config.manifestPath
      && installation.installationModelRoot == config.modelSnapshot
      && installation.installationRuntimeRoot == config.installedRoot </> "tei-runtime"
      && installation.installationEntrypointPath == config.executable
      && installation.installationRouterPath == config.installedRoot </> "tei-runtime/text-embeddings-router"
      && tei.teiRelease == "v1.9.3"
      && tei.teiSourceCommit == "06670157fb6c1523482219bdb2d1660277d38088"
      && source.sourceBackend == "candle-cuda"
      && source.sourceModelClass == "FlashQwen2Model"
      && source.sourceCompileCapability == 120
      && source.sourceRuntimeCapability == 120
      && source.sourceRequiresCuda
      && source.sourceRequiresF16
      && source.sourceUsesIsCausal
      && image.imagePlatform == "linux/amd64"
      && isSha256Digest image.imageIndexDigest
      && isSha256Digest image.imagePlatformDigest
      && isSha256Digest image.imageConfigDigest
      && image.imageIndexDigest == "sha256:aedf3b34836dc57289583142adcf2b93836cda0736ac8e6ce43691b9c2c67170"
      && image.imagePlatformDigest == "sha256:144aaa80ddcb520d49df83f915dc188ddd7cc6b1b3b9684a829c21dd39cbe3c5"
      && image.imageConfigDigest == "sha256:affa793eda6c6c6583d9c4041372dd710d74a55ab0a89025a1f322dd71b92eb2"
      && image.imageReference == "ghcr.io/huggingface/text-embeddings-inference@" <> image.imageIndexDigest
      && runtime.runtimeImageReference == image.imageReference
      && runtime.runtimePlatformDigest == image.imagePlatformDigest
      && runtime.runtimeConfigDigest == image.imageConfigDigest
      && runtime.runtimeSystemAbi == "exact-pinned-platform-image"
      && runtime.runtimeRetrievalPhase == "build-time-only"
      && runtime.runtimeUse == "verified-local-bundle"
      && not loader.loaderInheritsEnvironment
      && loader.loaderWorkingDirectory == installation.installationRuntimeRoot
      && loader.loaderPath == installation.installationRuntimeRoot <> ":/usr/local/cuda/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin"
      && loader.loaderLibraryPath == "/usr/local/cuda/lib64:/usr/local/cuda/lib64"
      && loader.loaderPreload == Nothing
      && loader.loaderOffline == "1"
      && loader.loaderTransformersOffline == "1"
      && loader.loaderFlashAttention == "True"
      && ["--dtype", "float16", "--max-batch-tokens", "32768", "--auto-truncate", "false", "--max-client-batch-size", "4"] `isOrderedSubsequence` loader.loaderArguments
      && isSafeRelative runtime.runtimeArtifactRoot
      && not (null runtime.runtimeArtifacts)
      && all validRuntimeArtifact runtime.runtimeArtifacts
      && unique runtimePaths
      && sort runtimePaths == ["entrypoint.sh", "text-embeddings-router"]
      && model.lockedModelId == requiredModelId
      && model.lockedModelRevision == requiredRevision
      && model.modelRetrievalPhase == "build-time-only"
      && model.modelRuntimeUse == "local-read-only-snapshot"
      && isSafeRelative model.modelArtifactRoot
      && model.modelArtifactRoot /= runtime.runtimeArtifactRoot
      && not (null model.modelArtifacts)
      && all validArtifact model.modelArtifacts
      && unique modelPaths
      && all (`elem` modelPaths)
           [ "config.json", "1_Pooling/config.json", "tokenizer.json"
           , "model-00001-of-00002.safetensors", "model-00002-of-00002.safetensors"
           , "model.safetensors.index.json" ]
      && serving.servingBackend == "candle-cuda"
      && serving.servingDtype == "float16"
      && serving.servingPooling == "last-valid-token"
      && serving.servingNormalize
      && not serving.servingAutoTruncate
      && serving.servingMaxInputTokens == 32768
      && serving.servingMaxBytes == 32767

isOrderedSubsequence :: Eq a => [a] -> [a] -> Bool
isOrderedSubsequence [] _ = True
isOrderedSubsequence _ [] = False
isOrderedSubsequence desired@(first : rest) (actual : remaining)
  | first == actual = isOrderedSubsequence rest remaining
  | otherwise = isOrderedSubsequence desired remaining

validRuntimeArtifact :: ManifestArtifact -> Bool
validRuntimeArtifact artifact =
  artifact.artifactPath `elem` ["entrypoint.sh", "text-embeddings-router"] && validArtifact artifact

validArtifact :: ManifestArtifact -> Bool
validArtifact artifact = isSafeRelative artifact.artifactPath
  && artifact.artifactBytes >= 0 && isHexOfLength 64 (T.pack artifact.artifactSha256)

unique :: Eq value => [value] -> Bool
unique values = length values == length (nub values)

isSha256Digest :: Text -> Bool
isSha256Digest value = case T.stripPrefix "sha256:" value of
  Just digest -> isHexOfLength 64 digest
  Nothing -> False

isHexOfLength :: Int -> Text -> Bool
isHexOfLength expected value = T.length value == expected && T.all (`elem` ("0123456789abcdef" :: String)) value

isSafeRelative :: FilePath -> Bool
isSafeRelative path =
  not (null path) && not (isAbsolute path) && null (takeDrive path)
    && '\\' `notElem` path
    && all (`notElem` ["", ".", ".."]) (slashSegments path)

slashSegments :: FilePath -> [FilePath]
slashSegments = foldr step [""]
  where
    step '/' values = "" : values
    step character (value : values) = (character : value) : values
    step _ [] = []

isCanonicalAbsolute :: FilePath -> Bool
isCanonicalAbsolute path = isAbsolute path && normalise path == path

requirePreflight :: Bool -> IO ()
requirePreflight condition = unless condition (ioError (userError "managed TEI preflight rejected"))

requireNoSymlinkComponents :: FilePath -> IO ()
requireNoSymlinkComponents path = mapM_ check (pathAncestors path)
  where
    check component = do
      exists <- doesPathExist component
      when exists $ do
        linked <- pathIsSymbolicLink component
        requirePreflight (not linked)

pathAncestors :: FilePath -> [FilePath]
pathAncestors = reverse . go . normalise
  where
    go path = path : let parent = takeDirectory path in if parent == path then [] else go parent

strictlyContained :: FilePath -> FilePath -> Bool
strictlyContained root child =
  let relative = makeRelative root child
   in relative /= "." && not (isAbsolute relative) && null (takeDrive relative)
        && all (/= "..") (splitDirectories relative)

isRegularNonLink :: FilePath -> IO Bool
isRegularNonLink path
  | os == "mingw32" = do
      exists <- doesPathExist path
      linked <- if exists then pathIsSymbolicLink path else pure False
      file <- if exists && not linked then doesFileExist path else pure False
      directory <- if exists && not linked then doesDirectoryExist path else pure False
      pure (exists && file && not directory && not linked)
  | otherwise = do
      linked <- pathIsSymbolicLink path
      if linked
        then pure False
        else do
          (status, _, _) <- readProcessWithExitCode "test" ["-f", path] ""
          pure (status == ExitSuccess)

verifyArtifactTree :: FilePath -> FilePath -> [ManifestArtifact] -> IO FilePath
verifyArtifactTree bundle relativeRoot artifacts = do
  requirePreflight (isSafeRelative relativeRoot)
  requirePreflight (all validArtifact artifacts)
  let rawRoot = bundle </> relativeRoot
  requireNoSymlinkComponents rawRoot
  requirePreflight =<< doesDirectoryExist rawRoot
  canonicalRoot <- canonicalizePath rawRoot
  requirePreflight (strictlyContained bundle canonicalRoot)
  actual <- sort <$> filesUnder canonicalRoot
  let expected = sort (map (normalise . (.artifactPath)) artifacts)
  requirePreflight (actual == expected)
  mapM_ (verifyArtifact canonicalRoot) artifacts
  pure canonicalRoot

filesUnder :: FilePath -> IO [FilePath]
filesUnder root = go ""
  where
    go relative = do
      entries <- sort <$> listDirectory (root </> relative)
      fmap concat . forM entries $ \entry -> do
        let next = if null relative then entry else relative </> entry
            full = root </> next
        linked <- pathIsSymbolicLink full
        requirePreflight (not linked)
        directory <- doesDirectoryExist full
        if directory
          then go next
          else do
            regular <- isRegularNonLink full
            requirePreflight regular
            pure [normalise next]

verifyArtifact :: FilePath -> ManifestArtifact -> IO ()
verifyArtifact root artifact = do
  path <- resolveLockedFile root artifact.artifactPath [artifact]
  size <- getFileSize path
  requirePreflight (fromIntegral size == artifact.artifactBytes)
  actual <- sha256File path
  requirePreflight (actual == artifact.artifactSha256)

resolveLockedFile :: FilePath -> FilePath -> [ManifestArtifact] -> IO FilePath
resolveLockedFile root relative artifacts = do
  requirePreflight (isSafeRelative relative)
  requirePreflight (relative `elem` map (.artifactPath) artifacts)
  let raw = root </> relative
  requireNoSymlinkComponents raw
  requirePreflight =<< isRegularNonLink raw
  canonical <- canonicalizePath raw
  requirePreflight (strictlyContained root canonical)
  pure canonical

sha256File :: FilePath -> IO String
sha256File path = withBinaryFile path ReadMode (go (hashInit :: Context SHA256))
  where
    go !context handle = do
      chunk <- BS.hGetSome handle (1024 * 1024)
      if BS.null chunk
        then pure (show (hashFinalize context))
        else let next = hashUpdate context chunk in next `seq` go next handle

defaultManagedTeiDeps :: ManagedTeiDeps
defaultManagedTeiDeps = ManagedTeiDeps
  { selectLoopbackPort = selectPort
  , prepareLaunch = verifyManagedTeiLaunch
  , spawnChild = \eventSink launch register ->
      spawnDefaultChildWithRegistration register (pure ()) eventSink launch
  -- The wire contract is validated by the generation's owned HTTP provider
  -- during activation. This test seam only orders the child and activation.
  , probeIdentity = \_ -> pure (Right pinnedDeclaredIdentity)
  , monotonicNow = getMonotonicTimeNSec
  , sleepMicros = threadDelay
  , jitterMicros = \bound -> randomRIO (0, max 1 (bound `div` 4))
  , emit = \_ -> pure ()
  }

pinnedDeclaredIdentity :: ManagedTeiIdentity
pinnedDeclaredIdentity = ManagedTeiIdentity
  { modelId = T.pack defaultManagedTeiConfig.modelSnapshot
  , servedModelId = defaultManagedTeiConfig.servedModelName
  , modelRevision = GteQwen2.gteQwen2Requirements.modelRevision
  , pooling = defaultManagedTeiConfig.expectedPooling
  , maxInputLength = GteQwen2.gteQwen2Requirements.tokenizerMaxLength
  , maxBatchTokens = GteQwen2.gteQwen2Requirements.maxBatchTokens
  , autoTruncate = GteQwen2.gteQwen2Requirements.autoTruncate
  , dimensions = defaultManagedTeiConfig.expectedDimensions
  }

replaceModelId :: FilePath -> [String] -> [String]
replaceModelId snapshot = go
  where
    go ("--model-id" : _ : remaining) = "--model-id" : snapshot : go remaining
    go (value : remaining) = value : go remaining
    go [] = []

selectPort :: IO Int
selectPort = withSocketsDo $ do
  socketValue <- socket AF_INET Stream defaultProtocol
  bind socketValue (SockAddrInet 0 0x0100007f)
  name <- getSocketName socketValue
  close socketValue
  case name of
    SockAddrInet port _ -> pure (fromIntegral port)
    _ -> fail "loopback port selection returned a non-IPv4 address"

stopManagedTei :: ManagedTeiRuntime -> IO ()
stopManagedTei runtime = runtime.shutdown

managedTeiState :: ManagedTeiRuntime -> IO ManagedTeiState
managedTeiState runtime = runtime.state

-- | A managed endpoint is only handed to the provider after its identity probe
-- succeeded.  Degraded/stopped states deliberately provide no fallback URL.
awaitManagedTeiEndpoint :: ManagedTeiRuntime -> IO (Maybe Text)
awaitManagedTeiEndpoint runtime = go
  where
    go = managedTeiState runtime >>= \case
      ManagedTeiReady value -> pure (Just value)
      ManagedTeiDegraded -> pure Nothing
      ManagedTeiStopped -> pure Nothing
      ManagedTeiStarting -> threadDelay 50000 >> go

withManagedTeiEmitter :: (ManagedTeiEvent -> IO ()) -> ManagedTeiDeps -> ManagedTeiDeps
withManagedTeiEmitter eventSink deps = deps { emit = eventSink }

spawnDefaultChild :: (ManagedTeiEvent -> IO ()) -> ManagedTeiLaunch -> IO ManagedTeiChild
spawnDefaultChild = spawnDefaultChildWithRegistration (const (pure ())) (pure ())

-- | Production has no publication delay; the seam lets the lifecycle test
-- cancel precisely after child creation and before the child is returned.
spawnDefaultChildWithPublicationBarrier :: IO () -> (ManagedTeiEvent -> IO ()) -> ManagedTeiLaunch -> IO ManagedTeiChild
spawnDefaultChildWithPublicationBarrier = spawnDefaultChildWithRegistration (const (pure ()))

-- The controller registers the exact child while this acquisition is masked.
-- A timeout or external cancellation can then find it in currentChild; before
-- registration, this constructor itself owns and reaps every created process.
spawnDefaultChildWithRegistration
  :: (ManagedTeiChild -> IO ())
  -> IO ()
  -> (ManagedTeiEvent -> IO ())
  -> ManagedTeiLaunch
  -> IO ManagedTeiChild
spawnDefaultChildWithRegistration register beforePublication eventSink launch = mask $ \restore -> do
  processEnv <- restore (minimalProcessEnvironment launch.childEnvironment)
  let command = (proc launch.childExecutable launch.arguments)
        { std_out = CreatePipe, std_err = CreatePipe
        , env = Just processEnv
        , cwd = Just (takeDirectory launch.childExecutable)
        , create_group = os /= "mingw32"
        , use_process_jobs = os == "mingw32" }
  (_, stdoutHandle, stderrHandle, processHandle) <- createProcess command
  processId <- fmap show <$> getPid processHandle
  reaper <- async (waitForProcess processHandle)
  cleanupState <- newMVar Nothing
  let sharedWait = waitCatch reaper >>= either throwIO pure
      cleanupOnce action = do
        outcome <- modifyMVar cleanupState $ \cached -> case cached of
          Just previous -> pure (cached, previous)
          Nothing -> do
            result <- trySynchronous action
            pure (Just result, result)
        either throwIO pure outcome
      cleanupBare = cleanupOnce (terminateDefaultChild processId processHandle sharedWait stdoutHandle stderrHandle Nothing Nothing)
  stdoutDrain <- (maybe (pure Nothing) (fmap Just . restore . async . drain "stdout") stdoutHandle)
    `onException` cleanupBare
  let cleanupWithStdout = cleanupOnce (terminateDefaultChild processId processHandle sharedWait stdoutHandle stderrHandle stdoutDrain Nothing)
  stderrDrain <- (maybe (pure Nothing) (fmap Just . restore . async . drain "stderr") stderrHandle)
    `onException` cleanupWithStdout
  let cleanupPublished = cleanupOnce (terminateDefaultChild processId processHandle sharedWait stdoutHandle stderrHandle stdoutDrain stderrDrain)
  let child = ManagedTeiChild
        { waitForChildExit = sharedWait
        , terminateAndReap = cleanupPublished
        , childProcessId = processId
        , childProcessGroupId = processId
        }
  restore beforePublication `onException` cleanupPublished
  register child `onException` cleanupPublished
  pure child
  where
    -- Drain pipes without retaining or emitting their content.  A finite bound
    -- is represented by one redacted event per stream, not by buffering text.
    drain streamName handle = do
      let go remaining warned = do
            bytes <- BS.hGetSome handle 4096
            unless (BS.null bytes) $ do
              let next = remaining - BS.length bytes
              when (next < 0 && not warned) (eventSink (ManagedTeiOutputSuppressed streamName))
              go next (warned || next < 0)
      go (64 * 1024) False `finally` hClose handle

spawnDefaultManagedTeiChild :: (ManagedTeiEvent -> IO ()) -> ManagedTeiLaunch -> IO ManagedTeiChild
spawnDefaultManagedTeiChild = spawnDefaultChild

spawnDefaultManagedTeiChildWithPublicationBarrier :: IO () -> (ManagedTeiEvent -> IO ()) -> ManagedTeiLaunch -> IO ManagedTeiChild
spawnDefaultManagedTeiChildWithPublicationBarrier = spawnDefaultChildWithPublicationBarrier

minimalProcessEnvironment :: [(String, String)] -> IO [(String, String)]
minimalProcessEnvironment controlledLoader
  | os /= "mingw32" = pure controlledLoader
  | otherwise = do
      systemRoot <- lookupEnv "SystemRoot"
      systemDrive <- lookupEnv "SystemDrive"
      tempDirectory <- canonicalizePath =<< getTemporaryDirectory
      pure $ controlledLoader
        <> maybe [] (\value -> [("SystemRoot", value)]) systemRoot
        <> maybe [] (\value -> [("SystemDrive", value)]) systemDrive
        <> [("TEMP", tempDirectory), ("TMP", tempDirectory)]

terminateDefaultChild
  :: Maybe String
  -> ProcessHandle
  -> IO ExitCode
  -> Maybe Handle
  -> Maybe Handle
  -> Maybe (Async ())
  -> Maybe (Async ())
  -> IO ()
terminateDefaultChild processId processHandle sharedWait stdoutHandle stderrHandle stdoutDrain stderrDrain = do
  runAllReleases
    [ terminateProcessTree processId processHandle sharedWait
    , mapM_ (maybe (pure ()) hClose) [stdoutHandle, stderrHandle]
    , mapM_ (maybe (pure ()) cancel) [stdoutDrain, stderrDrain]
    , mapM_ (maybe (pure ()) (void . timeout 5000000 . waitCatch)) [stdoutDrain, stderrDrain]
    ]

-- | Windows does not offer POSIX process groups.  taskkill's /T follows the
-- child tree and is invoked with an argument vector and discarded output; on
-- other platforms the process library provides the portable child shutdown.
terminateProcessTree :: Maybe String -> ProcessHandle -> IO ExitCode -> IO ()
terminateProcessTree persistedProcessId processHandle sharedWait
  | os == "mingw32" = do
      rootExit <- getProcessExitCode processHandle
      case rootExit of
        Just _ -> requireSharedReap sharedWait
        Nothing -> do
          void (trySynchronous (terminateProcess processHandle))
          jobReaped <- awaitSharedReap sharedWait
          unless jobReaped $ case persistedProcessId of
            Nothing -> cleanupFailed
            Just value -> do
              killed <- trySynchronous (runBoundedCommand "taskkill.exe" ["/PID", value, "/T", "/F"])
              case killed of
                Right (Just ExitSuccess) -> requireSharedReap sharedWait
                _ -> cleanupFailed
#if defined(mingw32_HOST_OS)
  | otherwise = cleanupFailed
#else
  | otherwise = do
      case persistedProcessId of
        Nothing -> do
          void (trySynchronous (terminateProcess processHandle))
          requireSharedReap sharedWait
        Just value -> do
          group <- either (const cleanupFailed) pure (parseOwnedProcessGroupId value)
          interruptResult <- trySynchronous (void (timeout 500000 (interruptProcessGroupOf processHandle)))
          aliveAfterInterrupt <- processGroupAlive value
          escalationResult <- if aliveAfterInterrupt
            then trySynchronous (signalOwnedProcessGroup group cSigKill)
            else pure (Right ())
          groupAbsent <- awaitProcessGroupAbsent (processGroupAlive value)
          case (interruptResult, escalationResult, groupAbsent) of
            (Left _, _, _) -> cleanupFailed
            (_, Left _, _) -> cleanupFailed
            (_, _, False) -> cleanupFailed
            _ -> requireSharedReap sharedWait
#endif

awaitSharedReap :: IO ExitCode -> IO Bool
awaitSharedReap sharedWait = do
  outcome <- timeout 5000000 (trySynchronous sharedWait)
  pure $ case outcome of
    Just (Right _) -> True
    _ -> False

requireSharedReap :: IO ExitCode -> IO ()
requireSharedReap sharedWait = do
  reaped <- awaitSharedReap sharedWait
  unless reaped cleanupFailed

-- Helper processes have their own sole waiter.  The managed child always uses
-- the shared reaper above and never calls raw waitForProcess during cleanup.
boundedTerminate :: ProcessHandle -> IO ()
boundedTerminate processHandle = do
  void (timeout 500000 (trySynchronous (terminateProcess processHandle)))
  outcome <- timeout 5000000 (trySynchronous (waitForProcess processHandle))
  case outcome of
    Just (Right _) -> pure ()
    _ -> cleanupFailed

runBoundedCommand :: FilePath -> [String] -> IO (Maybe ExitCode)
runBoundedCommand command arguments = mask $ \restore -> do
  (_, _, _, helper) <- createProcess (proc command arguments) { std_out = NoStream, std_err = NoStream }
  outcome <- try @SomeException (restore (timeout 5000000 (waitForProcess helper)))
  case outcome of
    Right (Just value) -> pure (Just value)
    Right Nothing -> boundedTerminate helper >> pure Nothing
    Left errorValue -> boundedTerminate helper >> throwIO errorValue

classifyProcessGroupErrno :: Maybe Errno -> Either () Bool
classifyProcessGroupErrno Nothing = Right True
classifyProcessGroupErrno (Just failure)
  | failure == eSRCH = Right False
  | otherwise = Left ()

-- Only the process handle's namespace-local group ID may be signalled.  In
-- particular, zero and one must never be negated into a broad group target.
parseOwnedProcessGroupId :: String -> Either () CInt
parseOwnedProcessGroupId value = case readMaybe value :: Maybe Integer of
  Just group | group > 1 && group <= fromIntegral (maxBound :: CInt) -> Right (fromInteger group)
  _ -> Left ()

#if !defined(mingw32_HOST_OS)
signalOwnedProcessGroup :: CInt -> CInt -> IO ()
signalOwnedProcessGroup group signal = do
  outcome <- cKill (negate group) signal
  when (outcome /= 0) $ do
    failure <- getErrno
    unless (failure == eSRCH) cleanupFailed
#endif

processGroupAlive :: String -> IO Bool
#if defined(mingw32_HOST_OS)
processGroupAlive _ = cleanupFailed
#else
processGroupAlive value = do
  group <- either (const cleanupFailed) pure (parseOwnedProcessGroupId value)
  outcome <- cKill (negate group) 0
  failure <- if outcome == 0 then pure Nothing else Just <$> getErrno
  either (const cleanupFailed) pure (classifyProcessGroupErrno failure)
#endif

-- A process group may remain visible for a short period after SIGKILL. A
-- single immediate probe cannot prove that all group descendants exited.
awaitProcessGroupAbsent :: IO Bool -> IO Bool
awaitProcessGroupAbsent probe = do
  result <- timeout 5000000 loop
  pure (result == Just ())
  where
    loop = probe >>= \case
      False -> pure ()
      True -> threadDelay 10000 >> loop

newtype InfoResponse = InfoResponse ManagedTeiIdentity

instance FromJSON InfoResponse where
  parseJSON = Aeson.withObject "TEI info" $ \objectValue ->
    InfoResponse <$> (ManagedTeiIdentity
      <$> objectValue .: "model_id"
      <*> objectValue .: "served_model_name"
      <*> (maybe "" id <$> objectValue .:? "model_sha")
      <*> (objectValue .: "model_type" >>= Aeson.withObject "TEI model type" (.: "embedding") >>= Aeson.withObject "TEI embedding model" (.: "pooling"))
      <*> objectValue .: "max_input_length"
      <*> objectValue .: "max_batch_tokens"
      <*> objectValue .: "auto_truncate"
      <*> pure 0)

decodeTeiInfoModelId :: LBS.ByteString -> Either () ManagedTeiIdentity
decodeTeiInfoModelId body = case Aeson.eitherDecode body of
  Right (InfoResponse identity) -> Right identity
  Left _ -> Left ()

-- | TEI's /info response describes routing limits but not vector dimension.
-- A single constant readiness embedding establishes the pinned dimension
-- without processing user input or retaining a vector.
decodeTeiReadinessEmbedding :: LBS.ByteString -> Either () Int
decodeTeiReadinessEmbedding body = case decodeEmbeddingResponse 1 body of
  Right [vector] -> Right (length vector)
  _ -> Left ()
