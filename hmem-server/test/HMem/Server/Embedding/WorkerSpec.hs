module HMem.Server.Embedding.WorkerSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (AsyncCancelled(..), cancel, cancelWith, poll, wait, waitCatch, withAsync)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar, tryPutMVar)
import Control.Exception (AsyncException(..), SomeAsyncException, SomeException, bracket, catch, finally, fromException, throwIO)
import Control.Monad (unless, void)
import Data.IORef
import Data.Int (Int32)
import Data.List (elemIndex, sort, sortOn)
import Data.Maybe (fromMaybe)
import Data.Pool (Pool)
import Data.Pool qualified as Pool
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime, getCurrentTime)
import Data.UUID (UUID)
import GHC.Clock (getMonotonicTimeNSec)
import Hasql.Connection qualified as Hasql
import Hasql.Decoders qualified as Dec
import Hasql.Encoders qualified as Enc
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement
import Test.Hspec
import System.Timeout (timeout)

import HMem.Config (managedTeiSpaceFingerprint)
import HMem.DB.Embedding
import HMem.DB.Observation
import HMem.DB.Pool (checkPgvector, createPool, runSession)
import HMem.DB.TestHarness qualified as Harness
import HMem.Server.Embedding.Provider
import HMem.Server.Embedding.Worker
import HMem.Types

spec :: Spec
spec = around Harness.withTestEnv $ do
  describe "durable embedding worker" $ do
    it "is inert while the injected provider is disabled" $ \env -> do
      calls <- newIORef (0 :: Int)
      let provider = EmbeddingProvider
            { availability = pure EmbeddingDisabled
            , embed = \_ -> modifyIORef' calls (+ 1) >> pure (Left (EmbeddingFailure ProviderDisabled False))
            }
          Harness.TestEnv { pool = testPool } = env
      runEmbeddingWorkerOnce EmbeddingWorker
        { pool = testPool
        , provider = provider
        , leaseOwner = "test-worker"
        , batchSize = 1
        , clock = getCurrentTime
        , cancelled = pure False
        }
      readIORef calls `shouldReturn` 0

    it "runs a cancellable loop with an injected wait policy" $ \env -> do
      checks <- newIORef [False, False, False, True]
      waits <- newIORef (0 :: Int)
      clockCalls <- newIORef (0 :: Int)
      let cancelled = atomicModifyIORef' checks $ \case
            [] -> ([], True)
            value:rest -> (rest, value)
          provider = EmbeddingProvider
            { availability = pure EmbeddingDisabled
            , embed = \_ -> expectationFailure "disabled worker must not embed" >> pure (Left (EmbeddingFailure ProviderDisabled False))
            }
      runEmbeddingWorker EmbeddingWorker
        { pool = env.pool, provider = provider, leaseOwner = "loop", batchSize = 1
        , clock = modifyIORef' clockCalls (+ 1) >> getCurrentTime, cancelled = cancelled }
        (\_ -> modifyIORef' waits (+ 1))
      readIORef waits `shouldReturn` 1
      readIORef clockCalls `shouldReturn` 1

    it "joins external cancellation while the worker loop is waiting idle" $ \env -> do
      waitEntered <- newEmptyMVar
      waitGate <- newEmptyMVar
      providerCalls <- newIORef (0 :: Int)
      let provider = EmbeddingProvider
            { availability = pure EmbeddingDisabled
            , embed = \_ -> modifyIORef' providerCalls (+ 1) >> pure (Left (EmbeddingFailure ProviderDisabled False))
            }
          worker = testWorker env provider "idle-cancellation" 1
          idleWait _ = putMVar waitEntered () >> takeMVar waitGate
      withAsync (runEmbeddingWorker worker idleWait) $ \workerTask ->
        (`finally` void (tryPutMVar waitGate ())) $ do
          timeout 5000000 (takeMVar waitEntered) `shouldReturn` Just ()
          outcome <- timeout 5000000 (cancelWith workerTask AsyncCancelled >> waitCatch workerTask)
          outcome `shouldSatisfy` maybe False (either isExternalAsyncCancelled (const False))
      readIORef providerCalls `shouldReturn` 0

    it "embeds claimed documents in order and completes their leases" $ \env -> do
      present <- checkPgvector env.pool
      unless present $ pendingWith "sandbox PostgreSQL does not expose pgvector"
      workspace <- Harness.createTestWorkspace env "worker-success"
      enableEmbeddingTarget env.pool (testSpace managedTeiSpaceFingerprint)
      first <- createObservation env.pool (newObservation workspace.id "first")
      second <- createObservation env.pool (newObservation workspace.id "second")
      seen <- newIORef []
      let provider = EmbeddingProvider
            { availability = pure EmbeddingAvailable
            , embed = \request -> do
                writeIORef seen request.inputs
                pure (Right (EmbeddingBatch [unitX, unitX] managedTeiSpaceFingerprint))
            }
      runEmbeddingWorkerOnce EmbeddingWorker { pool = env.pool, provider = provider, leaseOwner = "worker-success", batchSize = 2, clock = getCurrentTime, cancelled = pure False }
      fmap (sort . map (.inputText)) (readIORef seen) `shouldReturn` ["first", "second"]
      claimEmbeddingJobs env.pool "other" 2 `shouldReturn` []

    it "isolates oversized admission first, middle, and last without changing valid documents" $ \env -> do
      requireVector env
      mapM_ (exerciseMixedAdmission env) [0, 1, 2]

    it "makes no provider call when every claimed document fails local admission" $ \env -> do
      requireVector env
      workspace <- Harness.createTestWorkspace env "worker-all-invalid"
      enableEmbeddingTarget env.pool (testSpace managedTeiSpaceFingerprint)
      observations <- mapM
        (createObservation env.pool . newObservation workspace.id)
        [T.replicate 32768 "a", T.replicate 16384 "é"]
      calls <- newIORef (0 :: Int)
      let provider = EmbeddingProvider
            { availability = pure EmbeddingAvailable
            , embed = \_ -> modifyIORef' calls (+ 1) >> pure (Right (EmbeddingBatch [] managedTeiSpaceFingerprint))
            }
      runEmbeddingWorkerOnce (testWorker env provider "all-invalid" 2)
      readIORef calls `shouldReturn` 0
      traverse (jobState env.pool . (.id)) observations `shouldReturn` replicate 2 "failed"

    it "admits exact ASCII and multibyte byte boundaries without truncation" $ \env -> do
      requireVector env
      workspace <- Harness.createTestWorkspace env "worker-byte-boundaries"
      enableEmbeddingTarget env.pool (testSpace managedTeiSpaceFingerprint)
      let acceptedAscii = T.replicate 32767 "a"
          rejectedAscii = T.replicate 32768 "a"
          acceptedMultibyte = T.replicate 16383 "é" <> "a"
          rejectedMultibyte = T.replicate 16384 "é"
      observations <- mapM
        (createObservation env.pool . newObservation workspace.id)
        [acceptedAscii, rejectedAscii, acceptedMultibyte, rejectedMultibyte]
      seen <- newIORef []
      let provider = EmbeddingProvider
            { availability = pure EmbeddingAvailable
            , embed = \request -> do
                writeIORef seen (map (.inputText) request.inputs)
                pure (Right (EmbeddingBatch (replicate (length request.inputs) unitX) managedTeiSpaceFingerprint))
            }
      runEmbeddingWorkerOnce (testWorker env provider "boundaries" 4)
      fmap sort (readIORef seen) `shouldReturn` sort [acceptedAscii, acceptedMultibyte]
      traverse (jobState env.pool . (.id)) observations
        `shouldReturn` ["complete", "failed", "complete", "failed"]

    it "leaves an expired lease reclaimable when it expires during a worker provider call" $ \env -> do
      requireVector env
      workspace <- Harness.createTestWorkspace env "worker-lease-expiry"
      enableEmbeddingTarget env.pool (testSpace managedTeiSpaceFingerprint)
      observation <- createObservation env.pool (newObservation workspace.id "lease expiry")
      let provider = EmbeddingProvider
            { availability = pure EmbeddingAvailable
            , embed = \_ -> do
                runSession env.pool $ Session.statement observation.id expireLeaseStatement
                pure (Right (EmbeddingBatch [unitX] managedTeiSpaceFingerprint))
            }
      runEmbeddingWorkerOnce EmbeddingWorker { pool = env.pool, provider = provider, leaseOwner = "expiry", batchSize = 1, clock = getCurrentTime, cancelled = pure False }
      reclaimed <- claimEmbeddingJobs env.pool "other" 1
      map (.observationId) reclaimed `shouldBe` [observation.id]

    it "rejects a worker result when content changes during its provider call" $ \env -> do
      requireVector env
      workspace <- Harness.createTestWorkspace env "worker-cas-race"
      enableEmbeddingTarget env.pool (testSpace managedTeiSpaceFingerprint)
      observation <- createObservation env.pool (newObservation workspace.id "before worker race")
      let provider = EmbeddingProvider
            { availability = pure EmbeddingAvailable
            , embed = \_ -> do
                _ <- updateObservation env.pool workspace.id observation.id (UpdateObservation "after worker race")
                pure (Right (EmbeddingBatch [unitX] managedTeiSpaceFingerprint))
            }
      runEmbeddingWorkerOnce EmbeddingWorker { pool = env.pool, provider = provider, leaseOwner = "race", batchSize = 1, clock = getCurrentTime, cancelled = pure False }
      [replacement] <- claimEmbeddingJobs env.pool "other" 1
      replacement.observationId `shouldBe` observation.id
      replacement.content `shouldBe` "after worker race"

    it "rejects a worker result when the active target switches during its provider call" $ \env -> do
      requireVector env
      workspace <- Harness.createTestWorkspace env "worker-space-switch"
      let managedSpace = testSpace managedTeiSpaceFingerprint
          switchedSpace = testSpace "hmem:manual:worker-switch"
      enableEmbeddingTarget env.pool managedSpace
      observation <- createObservation env.pool (newObservation workspace.id "space switch")
      let provider = EmbeddingProvider
            { availability = pure EmbeddingAvailable
            , embed = \_ -> do
                enableEmbeddingTarget env.pool switchedSpace
                pure (Right (EmbeddingBatch [unitX] managedTeiSpaceFingerprint))
            }
      runEmbeddingWorkerOnce EmbeddingWorker { pool = env.pool, provider = provider, leaseOwner = "switch", batchSize = 1, clock = getCurrentTime, cancelled = pure False }
      _ <- reconcileEmbeddingJobs env.pool 1
      [replacement] <- claimEmbeddingJobs env.pool "other" 1
      replacement.observationId `shouldBe` observation.id
      replacement.spaceFingerprint `shouldBe` switchedSpace

    it "releases retryable failures and terminalizes nonretryable failures without another claim" $ \env -> do
      requireVector env
      workspace <- Harness.createTestWorkspace env "worker-failures"
      enableEmbeddingTarget env.pool (testSpace managedTeiSpaceFingerprint)
      _ <- createObservation env.pool (newObservation workspace.id "retry")
      let retryable = EmbeddingProvider { availability = pure EmbeddingAvailable, embed = \_ -> pure (Left (EmbeddingFailure ProviderUnavailable True)) }
      runEmbeddingWorkerOnce EmbeddingWorker { pool = env.pool, provider = retryable, leaseOwner = "retry", batchSize = 1, clock = getCurrentTime, cancelled = pure False }
      claimEmbeddingJobs env.pool "other" 1 `shouldReturn` []
      _ <- createObservation env.pool (newObservation workspace.id "terminal")
      let terminal = EmbeddingProvider { availability = pure EmbeddingAvailable, embed = \_ -> pure (Left (EmbeddingFailure ProviderProtocolError False)) }
      runEmbeddingWorkerOnce EmbeddingWorker { pool = env.pool, provider = terminal, leaseOwner = "terminal", batchSize = 2, clock = getCurrentTime, cancelled = pure False }
      claimEmbeddingJobs env.pool "other" 2 `shouldReturn` []

    it "releases a claimed lease retryably when cancellation arrives" $ \env -> do
      requireVector env
      workspace <- Harness.createTestWorkspace env "worker-cancel"
      enableEmbeddingTarget env.pool (testSpace managedTeiSpaceFingerprint)
      _ <- createObservation env.pool (newObservation workspace.id "cancel")
      checks <- newIORef (0 :: Int)
      let cancelled = atomicModifyIORef' checks (\n -> (n + 1, n > 0))
          provider = EmbeddingProvider { availability = pure EmbeddingAvailable, embed = \_ -> expectationFailure "must not call provider after cancellation" >> pure (Left (EmbeddingFailure ProviderCancelled True)) }
      runEmbeddingWorkerOnce EmbeddingWorker { pool = env.pool, provider = provider, leaseOwner = "cancel", batchSize = 1, clock = getCurrentTime, cancelled = cancelled }
      claimEmbeddingJobs env.pool "other" 1 `shouldReturn` []

    it "keeps cancellation retryable and nonterminal even at the attempt ceiling" $ \env -> do
      requireVector env
      workspace <- Harness.createTestWorkspace env "worker-cancel-ceiling"
      enableEmbeddingTarget env.pool (testSpace managedTeiSpaceFingerprint)
      observation <- createObservation env.pool (newObservation workspace.id "cancel-ceiling")
      runSession env.pool $ Session.statement observation.id setAttemptsStatement
      checks <- newIORef (0 :: Int)
      let cancelled = atomicModifyIORef' checks (\n -> (n + 1, n > 0))
          provider = EmbeddingProvider { availability = pure EmbeddingAvailable, embed = \_ -> expectationFailure "must not call provider after cancellation" >> pure (Left (EmbeddingFailure ProviderCancelled True)) }
      runEmbeddingWorkerOnce EmbeddingWorker { pool = env.pool, provider = provider, leaseOwner = "cancel-ceiling", batchSize = 1, clock = getCurrentTime, cancelled = cancelled }
      runSession env.pool (Session.statement observation.id jobStateAttemptsStatement) `shouldReturn` ("pending", 15)

    it "cleans a claim if cancellation is raised immediately after claiming" $ \env -> do
      requireVector env
      workspace <- Harness.createTestWorkspace env "worker-post-claim-cancel"
      enableEmbeddingTarget env.pool (testSpace managedTeiSpaceFingerprint)
      observation <- createObservation env.pool (newObservation workspace.id "post-claim cancellation")
      runSession env.pool $ Session.statement observation.id setAttemptsStatement
      checks <- newIORef (0 :: Int)
      let cancelled = do
            count <- atomicModifyIORef' checks (\value -> (value + 1, value))
            if count == 0 then pure False else throwIO ThreadKilled
          provider = EmbeddingProvider
            { availability = pure EmbeddingAvailable
            , embed = \_ -> expectationFailure "post-claim cancellation must precede provider work" >> pure (Right (EmbeddingBatch [] managedTeiSpaceFingerprint))
            }
          worker = (testWorker env provider "post-claim-cancel" 1 :: EmbeddingWorker) { cancelled = cancelled }
      runEmbeddingWorkerOnce worker `shouldThrow` (== ThreadKilled)
      runSession env.pool (Session.statement observation.id jobStateAttemptsStatement) `shouldReturn` ("pending", 15)

    it "propagates provider asynchronous cancellation after retryable owned cleanup" $ \env -> do
      requireVector env
      workspace <- Harness.createTestWorkspace env "worker-provider-async"
      enableEmbeddingTarget env.pool (testSpace managedTeiSpaceFingerprint)
      observation <- createObservation env.pool (newObservation workspace.id "provider async cancellation")
      runSession env.pool $ Session.statement observation.id setAttemptsStatement
      let provider = EmbeddingProvider
            { availability = pure EmbeddingAvailable
            , embed = \_ -> throwIO ThreadKilled
            }
      runEmbeddingWorkerOnce (testWorker env provider "provider-async" 1)
        `shouldThrow` (== ThreadKilled)
      runSession env.pool (Session.statement observation.id jobStateAttemptsStatement) `shouldReturn` ("pending", 15)

    it "propagates provider ThreadKilled raised while renewal loss cancels the provider" $ \env -> do
      requireVector env
      workspace <- Harness.createTestWorkspace env "worker-provider-race-thread-killed"
      enableEmbeddingTarget env.pool (testSpace managedTeiSpaceFingerprint)
      observation <- createObservation env.pool (newObservation workspace.id "provider race thread killed")
      providerStarted <- newEmptyMVar
      providerGate <- newEmptyMVar
      renewNow <- newEmptyMVar
      let provider = EmbeddingProvider
            { availability = pure EmbeddingAvailable
            , embed = \_ ->
                (putMVar providerStarted () >> takeMVar providerGate >> pure (Right (EmbeddingBatch [unitX] managedTeiSpaceFingerprint)))
                  `catch` \(_ :: SomeException) -> throwIO ThreadKilled
            }
          policy = EmbeddingWorkerLeasePolicy
            { leaseSeconds = 60
            , waitBeforeRenewal = takeMVar renewNow
            , renewClaim = \_ _ _ -> pure False
            }
      withAsync (runEmbeddingWorkerOnceWithLeasePolicy policy (testWorker env provider "provider-race-thread-killed" 1)) $ \workerTask -> do
        takeMVar providerStarted
        putMVar renewNow ()
        outcome <- timeout 5000000 (waitCatch workerTask)
        outcome `shouldSatisfy` maybe False (either isThreadKilled (const False))
      jobState env.pool observation.id `shouldReturn` "pending"

    it "propagates provider AsyncCancelled raised while renewal loss cancels the provider" $ \env -> do
      requireVector env
      workspace <- Harness.createTestWorkspace env "worker-provider-race-async-cancelled"
      enableEmbeddingTarget env.pool (testSpace managedTeiSpaceFingerprint)
      observation <- createObservation env.pool (newObservation workspace.id "provider race async cancelled")
      providerStarted <- newEmptyMVar
      providerGate <- newEmptyMVar
      renewNow <- newEmptyMVar
      let provider = EmbeddingProvider
            { availability = pure EmbeddingAvailable
            , embed = \_ ->
                (putMVar providerStarted () >> takeMVar providerGate >> pure (Right (EmbeddingBatch [unitX] managedTeiSpaceFingerprint)))
                  `catch` \(_ :: SomeException) -> throwIO AsyncCancelled
            }
          policy = EmbeddingWorkerLeasePolicy
            { leaseSeconds = 60
            , waitBeforeRenewal = takeMVar renewNow
            , renewClaim = \_ _ _ -> pure False
            }
      withAsync (runEmbeddingWorkerOnceWithLeasePolicy policy (testWorker env provider "provider-race-async-cancelled" 1)) $ \workerTask -> do
        takeMVar providerStarted
        putMVar renewNow ()
        outcome <- timeout 5000000 (waitCatch workerTask)
        outcome `shouldSatisfy` maybe False (either isExternalAsyncCancelled (const False))
      jobState env.pool observation.id `shouldReturn` "pending"

    it "propagates external AsyncCancelled from renewal instead of accepting it as an owned stop" $ \env -> do
      requireVector env
      workspace <- Harness.createTestWorkspace env "worker-renew-external-async"
      enableEmbeddingTarget env.pool (testSpace managedTeiSpaceFingerprint)
      observation <- createObservation env.pool (newObservation workspace.id "renew external async")
      providerStarted <- newEmptyMVar
      providerGate <- newEmptyMVar
      providerStopped <- newEmptyMVar
      renewNow <- newEmptyMVar
      let provider = EmbeddingProvider
            { availability = pure EmbeddingAvailable
            , embed = \_ ->
                (putMVar providerStarted () >> takeMVar providerGate >> pure (Right (EmbeddingBatch [unitX] managedTeiSpaceFingerprint)))
                  `finally` void (tryPutMVar providerStopped ())
            }
          policy = EmbeddingWorkerLeasePolicy
            { leaseSeconds = 60
            , waitBeforeRenewal = takeMVar renewNow >> throwIO AsyncCancelled
            , renewClaim = renewEmbeddingJobFor
            }
      withAsync (runEmbeddingWorkerOnceWithLeasePolicy policy (testWorker env provider "renew-external-async" 1)) $ \workerTask -> do
        timeout 5000000 (takeMVar providerStarted) `shouldReturn` Just ()
        putMVar renewNow ()
        outcome <- timeout 5000000 (waitCatch workerTask)
        outcome `shouldSatisfy` maybe False (either isExternalAsyncCancelled (const False))
      timeout 1000000 (takeMVar providerStopped) `shouldReturn` Just ()
      jobState env.pool observation.id `shouldReturn` "pending"

    it "prefers renewal external cancellation over a synchronous provider failure" $ \env -> do
      requireVector env
      workspace <- Harness.createTestWorkspace env "worker-provider-sync-renew-async"
      enableEmbeddingTarget env.pool (testSpace managedTeiSpaceFingerprint)
      observation <- createObservation env.pool (newObservation workspace.id "provider sync renewal async")
      providerStarted <- newEmptyMVar
      providerFailNow <- newEmptyMVar
      renewalStarted <- newEmptyMVar
      renewalGate <- newEmptyMVar
      let provider = EmbeddingProvider
            { availability = pure EmbeddingAvailable
            , embed = \_ -> do
                putMVar providerStarted ()
                takeMVar providerFailNow
                ioError (userError "injected synchronous provider failure")
            }
          policy = EmbeddingWorkerLeasePolicy
            { leaseSeconds = 60
            , waitBeforeRenewal =
                (putMVar renewalStarted () >> takeMVar renewalGate)
                  `catch` \(_ :: SomeException) -> throwIO AsyncCancelled
            , renewClaim = renewEmbeddingJobFor
            }
      withAsync (runEmbeddingWorkerOnceWithLeasePolicy policy (testWorker env provider "provider-sync-renew-async" 1)) $ \workerTask -> do
        timeout 5000000 (takeMVar providerStarted) `shouldReturn` Just ()
        timeout 5000000 (takeMVar renewalStarted) `shouldReturn` Just ()
        putMVar providerFailNow ()
        outcome <- timeout 5000000 (waitCatch workerTask)
        outcome `shouldSatisfy` maybe False (either isExternalAsyncCancelled (const False))
      jobState env.pool observation.id `shouldReturn` "pending"
      vectorHead env.pool observation.id `shouldReturn` Nothing

    it "prefers provider ThreadKilled over a synchronous renewal failure" $ \env -> do
      requireVector env
      workspace <- Harness.createTestWorkspace env "worker-renew-sync-provider-killed"
      enableEmbeddingTarget env.pool (testSpace managedTeiSpaceFingerprint)
      observation <- createObservation env.pool (newObservation workspace.id "renewal sync provider killed")
      providerStarted <- newEmptyMVar
      providerGate <- newEmptyMVar
      renewalStarted <- newEmptyMVar
      renewalFailNow <- newEmptyMVar
      let provider = EmbeddingProvider
            { availability = pure EmbeddingAvailable
            , embed = \_ ->
                (putMVar providerStarted () >> takeMVar providerGate >> pure (Right (EmbeddingBatch [unitX] managedTeiSpaceFingerprint)))
                  `catch` \(_ :: SomeException) -> throwIO ThreadKilled
            }
          policy = EmbeddingWorkerLeasePolicy
            { leaseSeconds = 60
            , waitBeforeRenewal = do
                putMVar renewalStarted ()
                takeMVar renewalFailNow
                ioError (userError "injected synchronous renewal failure")
            , renewClaim = renewEmbeddingJobFor
            }
      withAsync (runEmbeddingWorkerOnceWithLeasePolicy policy (testWorker env provider "renew-sync-provider-killed" 1)) $ \workerTask -> do
        timeout 5000000 (takeMVar providerStarted) `shouldReturn` Just ()
        timeout 5000000 (takeMVar renewalStarted) `shouldReturn` Just ()
        putMVar renewalFailNow ()
        outcome <- timeout 5000000 (waitCatch workerTask)
        outcome `shouldSatisfy` maybe False (either isThreadKilled (const False))
      jobState env.pool observation.id `shouldReturn` "pending"
      vectorHead env.pool observation.id `shouldReturn` Nothing

    it "propagates renewal database errors and joins the cancelled provider" $ \env -> do
      requireVector env
      workspace <- Harness.createTestWorkspace env "worker-renew-error"
      enableEmbeddingTarget env.pool (testSpace managedTeiSpaceFingerprint)
      observation <- createObservation env.pool (newObservation workspace.id "renew error")
      providerStarted <- newEmptyMVar
      providerStopped <- newEmptyMVar
      renewNow <- newEmptyMVar
      let provider = EmbeddingProvider
            { availability = pure EmbeddingAvailable
            , embed = \_ ->
                (putMVar providerStarted () >> takeMVar providerStopped >> fail "provider gate unexpectedly opened")
                  `finally` void (tryPutMVar providerStopped ())
            }
          policy = EmbeddingWorkerLeasePolicy
            { leaseSeconds = 60
            , waitBeforeRenewal = takeMVar renewNow
            , renewClaim = \_ _ _ -> ioError (userError "injected renewal database error")
            }
      withAsync (runEmbeddingWorkerOnceWithLeasePolicy policy (testWorker env provider "renew-error" 1)) $ \workerTask -> do
        takeMVar providerStarted
        putMVar renewNow ()
        outcome <- timeout 5000000 (waitCatch workerTask)
        outcome `shouldSatisfy` maybe False (either (const True) (const False))
      timeout 1000000 (takeMVar providerStopped) `shouldReturn` Just ()
      jobState env.pool observation.id `shouldReturn` "pending"

    it "cancels shared inference and releases survivors when one renewal loses ownership" $ \env -> do
      requireVector env
      workspace <- Harness.createTestWorkspace env "worker-partial-renewal-loss"
      enableEmbeddingTarget env.pool (testSpace managedTeiSpaceFingerprint)
      lostObservation <- createObservation env.pool (newObservation workspace.id "lost ownership")
      survivingObservation <- createObservation env.pool (newObservation workspace.id "surviving ownership")
      providerStarted <- newEmptyMVar
      providerGate <- newEmptyMVar
      providerStopped <- newEmptyMVar
      renewNow <- newEmptyMVar
      synchronousHandlerCalls <- newIORef (0 :: Int)
      let provider = EmbeddingProvider
            { availability = pure EmbeddingAvailable
            , embed = \_ -> catchSynchronousForTest
                ((putMVar providerStarted () >> takeMVar providerGate >> pure (Right (EmbeddingBatch [unitX, unitX] managedTeiSpaceFingerprint)))
                  `finally` void (tryPutMVar providerStopped ()))
                (\_ -> modifyIORef' synchronousHandlerCalls (+ 1) >> pure (Left (EmbeddingFailure ProviderUnavailable True)))
            }
          policy = EmbeddingWorkerLeasePolicy
            { leaseSeconds = 60
            , waitBeforeRenewal = takeMVar renewNow
            , renewClaim = renewEmbeddingJobFor
            }
      withAsync (runEmbeddingWorkerOnceWithLeasePolicy policy (testWorker env provider "partial-renewal-loss" 2)) $ \workerTask -> do
        takeMVar providerStarted
        runSession env.pool $ Session.statement lostObservation.id expireLeaseStatement
        [replacement] <- claimEmbeddingJobs env.pool "replacement" 1
        replacement.observationId `shouldBe` lostObservation.id
        putMVar renewNow ()
        timeout 5000000 (wait workerTask) `shouldReturn` Just ()
        timeout 1000000 (takeMVar providerStopped) `shouldReturn` Just ()
        renewEmbeddingJob env.pool replacement `shouldReturn` True
      readIORef synchronousHandlerCalls `shouldReturn` 0
      jobState env.pool lostObservation.id `shouldReturn` "leased"
      jobState env.pool survivingObservation.id `shouldReturn` "pending"
      vectorHead env.pool lostObservation.id `shouldReturn` Nothing
      vectorHead env.pool survivingObservation.id `shouldReturn` Nothing

    it "releases at the attempt ceiling and joins provider and renewal on caller cancellation" $ \env -> do
      requireVector env
      workspace <- Harness.createTestWorkspace env "worker-inference-cancel"
      enableEmbeddingTarget env.pool (testSpace managedTeiSpaceFingerprint)
      observation <- createObservation env.pool (newObservation workspace.id "inference cancellation")
      runSession env.pool $ Session.statement observation.id setAttemptsStatement
      providerStarted <- newEmptyMVar
      providerGate <- newEmptyMVar
      providerStopped <- newEmptyMVar
      renewalStarted <- newEmptyMVar
      renewalGate <- newEmptyMVar
      renewalStopped <- newEmptyMVar
      let provider = EmbeddingProvider
            { availability = pure EmbeddingAvailable
            , embed = \_ ->
                (putMVar providerStarted () >> takeMVar providerGate >> pure (Right (EmbeddingBatch [unitX] managedTeiSpaceFingerprint)))
                  `finally` void (tryPutMVar providerStopped ())
            }
          policy = EmbeddingWorkerLeasePolicy
            { leaseSeconds = 60
            , waitBeforeRenewal =
                (putMVar renewalStarted () >> takeMVar renewalGate)
                  `finally` void (tryPutMVar renewalStopped ())
            , renewClaim = renewEmbeddingJobFor
            }
      withAsync (runEmbeddingWorkerOnceWithLeasePolicy policy (testWorker env provider "inference-cancel" 1)) $ \workerTask -> do
        takeMVar providerStarted
        takeMVar renewalStarted
        cancel workerTask
        outcome <- timeout 5000000 (waitCatch workerTask)
        outcome `shouldSatisfy` maybe False (either (const True) (const False))
      timeout 1000000 (takeMVar providerStopped) `shouldReturn` Just ()
      timeout 1000000 (takeMVar renewalStopped) `shouldReturn` Just ()
      runSession env.pool (Session.statement observation.id jobStateAttemptsStatement) `shouldReturn` ("pending", 15)

    it "joins cancellation cleanup within the original deadline while preserving a replacement owner" $ \env -> do
      requireVector env
      workspace <- Harness.createTestWorkspace env "worker-real-lock-cancel"
      enableEmbeddingTarget env.pool (testSpace managedTeiSpaceFingerprint)
      observations <- sortOn (.id) <$> mapM
        (createObservation env.pool . newObservation workspace.id)
        ["blocked old attempt", "surviving companion"]
      blockedObservation <- requireOnly "blocked observation" (take 1 observations)
      survivingObservation <- requireOnly "surviving observation" (drop 1 observations)
      runSession env.pool $ Session.statement survivingObservation.id setAttemptsStatement
      providerStarted <- newEmptyMVar
      providerGate <- newEmptyMVar
      providerStopped <- newEmptyMVar
      renewNow <- newEmptyMVar
      renewalStopped <- newEmptyMVar
      let provider = EmbeddingProvider
            { availability = pure EmbeddingAvailable
            , embed = \_ ->
                (putMVar providerStarted () >> takeMVar providerGate >> pure (Right (EmbeddingBatch [unitX, unitX] managedTeiSpaceFingerprint)))
                  `finally` void (tryPutMVar providerStopped ())
            }
          policy = EmbeddingWorkerLeasePolicy
            { leaseSeconds = 60
            , waitBeforeRenewal = takeMVar renewNow
            , renewClaim = \pool lease job ->
                renewEmbeddingJobFor pool lease job
                  `finally` void (tryPutMVar renewalStopped ())
            }
      withAsync (runEmbeddingWorkerOnceWithLeasePolicy policy (testWorker env provider "real-lock-cancel" 2)) $ \workerTask -> do
        takeMVar providerStarted
        runSession env.pool $ Session.statement blockedObservation.id expireLeaseStatement
        [replacement] <- claimEmbeddingJobs env.pool "replacement-owner" 1
        replacement.observationId `shouldBe` blockedObservation.id
        withWorkerTestPool env $ \lockPool ->
          withWorkerTestPool env $ \observerPool -> do
            mapM_ establishWorkerTestPool [lockPool, observerPool]
            withHeldObservationLock lockPool blockedObservation.id $ \holderPid -> do
              putMVar renewNow ()
              blockedActivity <- waitForBlockedClaimMutation observerPool holderPid
              blockedActivity.waitEventType `shouldBe` "Lock"
              blockedActivity.query `shouldSatisfy` T.isInfixOf "FROM public.observations o"
              blockedActivity.query `shouldSatisfy` T.isInfixOf "FOR UPDATE OF o"
              cancellationStarted <- getMonotonicTimeNSec
              deadlineOutcome <- timeout 12000000 $ do
                cancelWith workerTask AsyncCancelled
                workerOutcome <- waitCatch workerTask
                takeMVar providerStopped
                takeMVar renewalStopped
                runSession observerPool (Session.statement holderPid claimMutationActivityStatement)
                  `shouldReturn` []
                runSession observerPool (Session.statement blockedObservation.id jobStateAttemptsOwnerStatement)
                  `shouldReturn` ("leased", replacement.attempts, Just replacement.leaseOwner)
                runSession observerPool (Session.statement survivingObservation.id jobStateAttemptsOwnerStatement)
                  `shouldReturn` ("pending", 15, Nothing)
                vectorHead observerPool blockedObservation.id `shouldReturn` Nothing
                vectorHead observerPool survivingObservation.id `shouldReturn` Nothing
                pure workerOutcome
              cancellationFinished <- getMonotonicTimeNSec
              let cancellationSeconds = fromIntegral (cancellationFinished - cancellationStarted) / 1000000000 :: Double
              putStrLn ("[worker-held-lock-cancellation] holder_pid=" <> show holderPid
                <> "; renewal_pid=" <> show blockedActivity.activityPid
                <> "; query_start=" <> show blockedActivity.queryStartedAt
                <> "; xact_start=" <> show blockedActivity.transactionStartedAt
                <> "; cancellation_to_complete_scenario_seconds=" <> show cancellationSeconds)
              deadlineOutcome `shouldSatisfy` maybe False (either isExternalAsyncCancelled (const False))
              cancellationSeconds `shouldSatisfy` (< 12)
        renewEmbeddingJob env.pool replacement `shouldReturn` True
      jobState env.pool blockedObservation.id `shouldReturn` "leased"
      runSession env.pool (Session.statement survivingObservation.id jobStateAttemptsStatement) `shouldReturn` ("pending", 15)
      vectorHead env.pool blockedObservation.id `shouldReturn` Nothing
      vectorHead env.pool survivingObservation.id `shouldReturn` Nothing

    it "joins an in-flight production renewal before applying a successful provider response" $ \env -> do
      requireVector env
      workspace <- Harness.createTestWorkspace env "worker-provider-success-renewal-race"
      enableEmbeddingTarget env.pool (testSpace managedTeiSpaceFingerprint)
      observation <- createObservation env.pool (newObservation workspace.id "successful provider during renewal")
      providerStarted <- newEmptyMVar
      providerRelease <- newEmptyMVar
      providerStopped <- newEmptyMVar
      renewNow <- newEmptyMVar
      renewalStopped <- newEmptyMVar
      let provider = EmbeddingProvider
            { availability = pure EmbeddingAvailable
            , embed = \_ ->
                (putMVar providerStarted () >> takeMVar providerRelease >> pure (Right (EmbeddingBatch [unitX] managedTeiSpaceFingerprint)))
                  `finally` void (tryPutMVar providerStopped ())
            }
          policy = EmbeddingWorkerLeasePolicy
            { leaseSeconds = 60
            , waitBeforeRenewal = takeMVar renewNow
            , renewClaim = \pool lease job ->
                renewEmbeddingJobFor pool lease job
                  `finally` void (tryPutMVar renewalStopped ())
            }
      withWorkerTestPool env $ \workerPool ->
        withWorkerTestPool env $ \lockPool ->
          withWorkerTestPool env $ \observerPool -> do
            mapM_ establishWorkerTestPool [workerPool, lockPool, observerPool]
            workerPid <- runSession workerPool (Session.statement () backendPidStatement)
            let worker = (testWorker env provider "provider-success-renewal-race" 1) { pool = workerPool }
            withAsync (runEmbeddingWorkerOnceWithLeasePolicy policy worker) $ \workerTask ->
              (`finally` void (tryPutMVar providerRelease ())) $ do
                takeMVar providerStarted
                holderAcquired <- newEmptyMVar
                holderRelease <- newEmptyMVar
                withAsync (holdObservationLock lockPool observation.id holderAcquired holderRelease) $ \holderTask ->
                  (`finally` (void (tryPutMVar holderRelease ()) >> void (waitCatch holderTask))) $ do
                    holderPid <- timeout 5000000 (takeMVar holderAcquired) >>= \case
                      Just pid -> pure pid
                      Nothing -> expectationFailure "timed out acquiring the provider-success Observation lock" >> fail "unreachable"
                    putMVar renewNow ()
                    renewalActivity <- waitForBlockedClaimMutationNamed "provider-success renewal" observerPool holderPid
                    renewalActivity.activityPid `shouldBe` workerPid
                    renewalActivity.waitEventType `shouldBe` "Lock"
                    renewalActivity.query `shouldSatisfy` T.isInfixOf "FROM public.observations o"
                    renewalActivity.query `shouldSatisfy` T.isInfixOf "FOR UPDATE OF o"
                    providerReleasedAt <- getMonotonicTimeNSec
                    deadlineOutcome <- timeout 12000000 $ do
                      putMVar providerRelease ()
                      takeMVar providerStopped
                      takeMVar renewalStopped
                      workerBeforeCompletion <- poll workerTask
                      workerBeforeCompletion `shouldSatisfy` maybe True (const False)
                      completionActivity <- waitForBlockedClaimMutationAfter
                        "provider-success completion" observerPool holderPid renewalActivity.queryStartedAt
                      completionActivity.queryStartedAt `shouldSatisfy` (> renewalActivity.queryStartedAt)
                      completionActivity.transactionStartedAt `shouldSatisfy` (> renewalActivity.transactionStartedAt)
                      completionActivity.waitEventType `shouldBe` "Lock"
                      completionActivity.query `shouldSatisfy` T.isInfixOf "FROM public.observations o"
                      completionActivity.query `shouldSatisfy` T.isInfixOf "FOR UPDATE OF o"
                      putMVar holderRelease ()
                      holderOutcome <- waitCatch holderTask
                      holderOutcome `shouldSatisfy` either (const False) (const True)
                      workerOutcome <- waitCatch workerTask
                      workerOutcome `shouldSatisfy` either (const False) (const True)
                      runSession observerPool (Session.statement holderPid claimMutationActivityStatement)
                        `shouldReturn` []
                      renewalBackend <- runSession observerPool
                        (Session.statement renewalActivity.activityPid backendTransactionStateStatement)
                      renewalBackend `shouldSatisfy` all backendIsQuiescent
                      completionBackend <- runSession observerPool
                        (Session.statement completionActivity.activityPid backendTransactionStateStatement)
                      completionBackend `shouldSatisfy` all backendIsQuiescent
                      runSession observerPool (Session.statement observation.id jobStateAttemptsOwnerStatement)
                        `shouldReturn` ("complete", 1, Nothing)
                      vectorHead observerPool observation.id `shouldReturn` Just (1, 0)
                      claimEmbeddingJobs observerPool "other-provider-success" 1 `shouldReturn` []
                      pure completionActivity
                    providerCompletedAt <- getMonotonicTimeNSec
                    let providerCompletionSeconds =
                          fromIntegral (providerCompletedAt - providerReleasedAt) / 1000000000 :: Double
                    case deadlineOutcome of
                      Nothing -> expectationFailure "timed out joining in-flight renewal before successful provider completion"
                      Just completionActivity -> putStrLn
                        ("[worker-provider-success-renewal-race] holder_pid=" <> show holderPid
                          <> "; renewal_pid=" <> show renewalActivity.activityPid
                          <> "; renewal_query_start=" <> show renewalActivity.queryStartedAt
                          <> "; renewal_xact_start=" <> show renewalActivity.transactionStartedAt
                          <> "; completion_pid=" <> show completionActivity.activityPid
                          <> "; completion_query_start=" <> show completionActivity.queryStartedAt
                          <> "; completion_xact_start=" <> show completionActivity.transactionStartedAt
                          <> "; provider_to_complete_scenario_seconds=" <> show providerCompletionSeconds)
                    providerCompletionSeconds `shouldSatisfy` (< 12)
      jobState env.pool observation.id `shouldReturn` "complete"
      vectorHead env.pool observation.id `shouldReturn` Just (1, 0)

    it "bounds cancellation cleanup while its worker pool is saturated" $ \env -> do
      requireVector env
      workspace <- Harness.createTestWorkspace env "worker-saturated-pool-cancel"
      enableEmbeddingTarget env.pool (testSpace managedTeiSpaceFingerprint)
      observation <- createObservation env.pool (newObservation workspace.id "saturated pool cancellation")
      providerStarted <- newEmptyMVar
      providerGate <- newEmptyMVar
      providerStopped <- newEmptyMVar
      bracket
        (createPool env.testDb.testDbConnStr 1 5 30000)
        Pool.destroyAllResources $ \workerPool -> do
          let provider = EmbeddingProvider
                { availability = pure EmbeddingAvailable
                , embed = \_ ->
                    (putMVar providerStarted () >> takeMVar providerGate >> pure (Right (EmbeddingBatch [unitX] managedTeiSpaceFingerprint)))
                      `finally` void (tryPutMVar providerStopped ())
                }
              worker = EmbeddingWorker
                { pool = workerPool, provider = provider, leaseOwner = "saturated-pool"
                , batchSize = 1, clock = getCurrentTime, cancelled = pure False }
          withAsync (runEmbeddingWorkerOnce worker) $ \workerTask -> do
            takeMVar providerStarted
            poolHeld <- newEmptyMVar
            releasePool <- newEmptyMVar
            withAsync
              (Pool.withResource workerPool $ \_ -> putMVar poolHeld () >> takeMVar releasePool)
              $ \poolHolder -> (`finally` (void (tryPutMVar releasePool ()) >> void (waitCatch poolHolder))) $ do
                takeMVar poolHeld
                outcome <- timeout 15000000 (cancelWith workerTask AsyncCancelled >> waitCatch workerTask)
                outcome `shouldSatisfy` maybe False (either isExternalAsyncCancelled (const False))
                timeout 1000000 (takeMVar providerStopped) `shouldReturn` Just ()
      jobState env.pool observation.id `shouldReturn` "leased"
      vectorHead env.pool observation.id `shouldReturn` Nothing

    it "preserves the first external cancellation across repeated delivery during owned cleanup" $ \env -> do
      requireVector env
      workspace <- Harness.createTestWorkspace env "worker-repeated-external-cancel"
      enableEmbeddingTarget env.pool (testSpace managedTeiSpaceFingerprint)
      observation <- createObservation env.pool (newObservation workspace.id "repeated external cancellation")
      providerStarted <- newEmptyMVar
      providerGate <- newEmptyMVar
      renewalGate <- newEmptyMVar
      let provider = EmbeddingProvider
            { availability = pure EmbeddingAvailable
            , embed = \_ -> putMVar providerStarted () >> takeMVar providerGate >> pure (Right (EmbeddingBatch [unitX] managedTeiSpaceFingerprint))
            }
          policy = EmbeddingWorkerLeasePolicy
            { leaseSeconds = 60
            , waitBeforeRenewal = takeMVar renewalGate
            , renewClaim = renewEmbeddingJobFor
            }
      withAsync (runEmbeddingWorkerOnceWithLeasePolicy policy (testWorker env provider "repeated-external-cancel" 1)) $ \workerTask -> do
        takeMVar providerStarted
        withWorkerTestPool env $ \lockPool ->
          withWorkerTestPool env $ \observerPool -> do
            mapM_ establishWorkerTestPool [lockPool, observerPool]
            withHeldObservationLock lockPool observation.id $ \holderPid ->
              withAsync (cancelWith workerTask ThreadKilled) $ \firstCancellation -> do
                waitForBlockedClaimMutationNamed "first external cancellation cleanup" observerPool holderPid
                outcome <- timeout 12000000 (cancelWith workerTask AsyncCancelled >> waitCatch workerTask)
                outcome `shouldSatisfy` maybe False (either isThreadKilled (const False))
                firstOutcome <- timeout 1000000 (waitCatch firstCancellation)
                firstOutcome `shouldSatisfy` maybe False (either (const False) (const True))
      jobState env.pool observation.id `shouldReturn` "leased"
      vectorHead env.pool observation.id `shouldReturn` Nothing

    it "propagates later cancellation over a provider error and earlier cleanup timeout" $ \env -> do
      requireVector env
      workspace <- Harness.createTestWorkspace env "worker-cleanup-async-precedence"
      enableEmbeddingTarget env.pool (testSpace managedTeiSpaceFingerprint)
      observations <- mapM
        (createObservation env.pool . newObservation workspace.id)
        ["first cleanup blocks", "second cleanup is cancelled"]
      providerStarted <- newEmptyMVar
      providerGate <- newEmptyMVar
      renewalGate <- newEmptyMVar
      let provider = EmbeddingProvider
            { availability = pure EmbeddingAvailable
            , embed = \request -> do
                putMVar providerStarted (map (.inputText) request.inputs)
                takeMVar providerGate
                ioError (userError "injected synchronous provider error")
            }
          policy = EmbeddingWorkerLeasePolicy
            { leaseSeconds = 60
            , waitBeforeRenewal = takeMVar renewalGate
            , renewClaim = renewEmbeddingJobFor
            }
      withAsync (runEmbeddingWorkerOnceWithLeasePolicy policy (testWorker env provider "cleanup-async-precedence" 2)) $ \workerTask -> do
        claimOrder <- timeout 5000000 (takeMVar providerStarted) >>= \case
          Just value -> pure value
          Nothing -> expectationFailure "timed out waiting for the cleanup precedence provider" >> fail "unreachable"
        firstContent <- requireOnly "first cleanup content" (take 1 claimOrder)
        secondContent <- requireOnly "second cleanup content" (drop 1 claimOrder)
        firstObservation <- requireOnly "first cleanup observation"
          [observation | observation <- observations, observation.content == firstContent]
        secondObservation <- requireOnly "second cleanup observation"
          [observation | observation <- observations, observation.content == secondContent]
        withWorkerTestPool env $ \firstLockPool ->
          withWorkerTestPool env $ \secondLockPool ->
            withWorkerTestPool env $ \observerPool ->
              withHeldObservationLock firstLockPool firstObservation.id $ \firstHolderPid ->
                withHeldObservationLock secondLockPool secondObservation.id $ \secondHolderPid -> do
                  putMVar providerGate ()
                  waitForBlockedClaimMutationNamed "first cleanup" observerPool firstHolderPid
                  waitForBlockedClaimMutationNamed "second cleanup" observerPool secondHolderPid
                  outcome <- timeout 25000000 (cancelWith workerTask AsyncCancelled >> waitCatch workerTask)
                  outcome `shouldSatisfy` maybe False (either isExternalAsyncCancelled (const False))
      mapM_ (\observation -> jobState env.pool observation.id `shouldReturn` "leased") observations
      mapM_ (\observation -> vectorHead env.pool observation.id `shouldReturn` Nothing) observations

    it "renews through the original sixty-second lease and completes afterward" $ \env -> do
      requireVector env
      workspace <- Harness.createTestWorkspace env "worker-renew-original-lease"
      enableEmbeddingTarget env.pool (testSpace managedTeiSpaceFingerprint)
      observation <- createObservation env.pool (newObservation workspace.id "long controlled provider")
      providerStarted <- newEmptyMVar
      providerRelease <- newEmptyMVar
      let provider = EmbeddingProvider
            { availability = pure EmbeddingAvailable
            , embed = \_ -> do
                started <- getMonotonicTimeNSec
                putMVar providerStarted started
                takeMVar providerRelease
                pure (Right (EmbeddingBatch [unitX] managedTeiSpaceFingerprint))
            }
      scenario <- timeout 70000000 $
        withAsync (runEmbeddingWorkerOnce (testWorker env provider "renew-original-lease" 1)) $ \workerTask -> do
          providerStartedAt <- takeMVar providerStarted
          originalExpiry <- requireLeaseExpiry env.pool observation.id
          waitForDbCondition 7000 "a lease renewal" $ do
            currentExpiry <- requireLeaseExpiry env.pool observation.id
            pure (currentExpiry > originalExpiry)
          renewedExpiry <- requireLeaseExpiry env.pool observation.id
          renewedExpiry `shouldSatisfy` (> originalExpiry)
          waitForDbCondition 7000 "the original lease to expire" $
            runSession env.pool (Session.statement originalExpiry dbClockAfterStatement)
          waitForDbCondition 7000 "more than sixty seconds of provider work" $ do
            now <- getMonotonicTimeNSec
            pure (now - providerStartedAt > 60000000000)
          providerReleasedAt <- getMonotonicTimeNSec
          let providerSeconds = fromIntegral (providerReleasedAt - providerStartedAt) / 1000000000 :: Double
          putStrLn ("[worker-literal-renewal] original_expiry=" <> show originalExpiry
            <> "; renewed_expiry=" <> show renewedExpiry
            <> "; provider_seconds=" <> show providerSeconds)
          providerSeconds `shouldSatisfy` (> 60)
          putMVar providerRelease ()
          wait workerTask
      scenario `shouldBe` Just ()
      jobState env.pool observation.id `shouldReturn` "complete"

    it "terminalizes a retryable failure at the fixed attempt ceiling" $ \env -> do
      requireVector env
      workspace <- Harness.createTestWorkspace env "worker-attempt-ceiling"
      enableEmbeddingTarget env.pool (testSpace managedTeiSpaceFingerprint)
      observation <- createObservation env.pool (newObservation workspace.id "ceiling")
      runSession env.pool $ Session.statement observation.id setAttemptsStatement
      let provider = EmbeddingProvider { availability = pure EmbeddingAvailable, embed = \_ -> pure (Left (EmbeddingFailure ProviderUnavailable True)) }
      runEmbeddingWorkerOnce EmbeddingWorker { pool = env.pool, provider = provider, leaseOwner = "ceiling", batchSize = 1, clock = getCurrentTime, cancelled = pure False }
      claimEmbeddingJobs env.pool "other" 1 `shouldReturn` []
  where
    unitX = 1 : replicate (observationEmbeddingDimensions - 1) 0
    newObservation workspace content = CreateObservation workspace [ObservationSubject SubjectFile "src/Worker.hs"] "0123456789abcdef0123456789abcdef01234567" content
    requireVector env = do
      present <- checkPgvector env.pool
      unless present $ pendingWith "sandbox PostgreSQL does not expose pgvector"

testWorker :: Harness.TestEnv -> EmbeddingProvider -> Text -> Int -> EmbeddingWorker
testWorker env provider owner size = EmbeddingWorker
  { pool = env.pool
  , provider = provider
  , leaseOwner = owner
  , batchSize = size
  , clock = getCurrentTime
  , cancelled = pure False
  }

exerciseMixedAdmission :: Harness.TestEnv -> Int -> IO ()
exerciseMixedAdmission env oversizedPosition = do
  workspace <- Harness.createTestWorkspace env ("worker-mixed-admission-" <> T.pack (show oversizedPosition))
  enableEmbeddingTarget env.pool (testSpace managedTeiSpaceFingerprint)
  let validFirst = "valid-first-" <> T.pack (show oversizedPosition)
      validSecond = "valid-second-" <> T.pack (show oversizedPosition)
      oversized = T.replicate 32768 "a"
      bodies = insertAt oversizedPosition oversized [validFirst, validSecond]
  placeholders <- mapM
    (createObservation env.pool . testObservation workspace.id)
    ["placeholder-a", "placeholder-b", "placeholder-c"]
  let observations = sortOn (.id) placeholders
  mapM_ (\(observation, body) -> void $
    updateObservation env.pool workspace.id observation.id (UpdateObservation body))
    (zip observations bodies)
  actualClaimOrder <- runSession env.pool $ Session.statement workspace.id claimOrderContentsStatement
  elemIndex oversized actualClaimOrder `shouldBe` Just oversizedPosition
  seen <- newIORef []
  let provider = EmbeddingProvider
        { availability = pure EmbeddingAvailable
        , embed = \request -> do
            writeIORef seen (map (.inputText) request.inputs)
            let vectorFor input
                  | input.inputText == validFirst = testUnitX
                  | otherwise = testUnitY
            pure (Right (EmbeddingBatch (map vectorFor request.inputs) managedTeiSpaceFingerprint))
        }
      expectedOrder =
        [ body
        | body <- bodies
        , body /= oversized
        ]
      observationFor body = requireOnly ("observation for " <> T.unpack body)
        [observation | (observation, actualBody) <- zip observations bodies, actualBody == body]
  runEmbeddingWorkerOnce (testWorker env provider "mixed-admission" 3)
  readIORef seen `shouldReturn` expectedOrder
  firstObservation <- observationFor validFirst
  secondObservation <- observationFor validSecond
  oversizedObservation <- observationFor oversized
  vectorHead env.pool firstObservation.id `shouldReturn` Just (1, 0)
  vectorHead env.pool secondObservation.id `shouldReturn` Just (0, 1)
  jobState env.pool oversizedObservation.id `shouldReturn` "failed"

insertAt :: Int -> a -> [a] -> [a]
insertAt index value values =
  let (before, after) = splitAt index values
  in before <> [value] <> after

requireOnly :: String -> [a] -> IO a
requireOnly _ [value] = pure value
requireOnly label values = expectationFailure
  (label <> ": expected one value, got " <> show (length values)) >> fail "unreachable"

testObservation :: UUID -> Text -> CreateObservation
testObservation workspace content = CreateObservation workspace
  [ObservationSubject SubjectFile "src/Worker.hs"]
  "0123456789abcdef0123456789abcdef01234567"
  content

testUnitX, testUnitY :: [Double]
testUnitX = 1 : replicate (observationEmbeddingDimensions - 1) 0
testUnitY = 0 : 1 : replicate (observationEmbeddingDimensions - 2) 0

jobState :: Pool Hasql.Connection -> UUID -> IO Text
jobState pool observation = runSession pool $ Session.statement observation jobStateStatement

vectorHead :: Pool Hasql.Connection -> UUID -> IO (Maybe (Double, Double))
vectorHead pool observation = do
  (first, second) <- runSession pool $ Session.statement observation vectorHeadStatement
  pure ((,) <$> first <*> second)

requireLeaseExpiry :: Pool Hasql.Connection -> UUID -> IO UTCTime
requireLeaseExpiry pool observation = do
  value <- runSession pool $ Session.statement observation leaseExpiryStatement
  case value of
    Just expiry -> pure expiry
    Nothing -> expectationFailure "expected a leased embedding job" >> fail "unreachable"

waitForDbCondition :: Int -> String -> IO Bool -> IO ()
waitForDbCondition attempts label condition = go attempts
  where
    go 0 = expectationFailure ("timed out waiting for " <> label) >> fail "unreachable"
    go remaining = condition >>= \case
      True -> pure ()
      False -> threadDelay 10000 >> go (remaining - 1)

isThreadKilled :: SomeException -> Bool
isThreadKilled exceptionValue =
  (fromException exceptionValue :: Maybe AsyncException) == Just ThreadKilled

isExternalAsyncCancelled :: SomeException -> Bool
isExternalAsyncCancelled exceptionValue =
  case fromException exceptionValue :: Maybe AsyncCancelled of
    Just _ -> True
    Nothing -> False

catchSynchronousForTest :: IO a -> (SomeException -> IO a) -> IO a
catchSynchronousForTest action handler = action `catch` \exceptionValue ->
  case fromException exceptionValue :: Maybe SomeAsyncException of
    Just _ -> throwIO exceptionValue
    Nothing -> handler exceptionValue

withHeldObservationLock
  :: Pool Hasql.Connection -> UUID -> (Int32 -> IO a) -> IO a
withHeldObservationLock pool observation action = do
  acquired <- newEmptyMVar
  release <- newEmptyMVar
  withAsync (holdObservationLock pool observation acquired release) $ \holder ->
    (`finally` (void (tryPutMVar release ()) >> void (waitCatch holder))) $ do
      holderPid <- timeout 5000000 (takeMVar acquired) >>= \case
        Just pid -> pure pid
        Nothing -> expectationFailure "timed out acquiring the WorkerSpec Observation lock" >> fail "unreachable"
      action holderPid

withWorkerTestPool :: Harness.TestEnv -> (Pool Hasql.Connection -> IO a) -> IO a
withWorkerTestPool env =
  bracket
    (createPool env.testDb.testDbConnStr 1 5 30000)
    Pool.destroyAllResources

establishWorkerTestPool :: Pool Hasql.Connection -> IO ()
establishWorkerTestPool pool =
  Pool.withResource pool $ \connection -> void (runDirect connection (Session.statement () backendPidStatement))

holdObservationLock
  :: Pool Hasql.Connection -> UUID -> MVar Int32 -> MVar () -> IO ()
holdObservationLock pool observation acquired release =
  Pool.withResource pool $ \connection ->
    (do
      runDirect connection (Session.sql "BEGIN")
      runDirect connection (Session.statement observation lockObservationStatement)
      backendPid <- runDirect connection (Session.statement () backendPidStatement)
      putMVar acquired backendPid
      takeMVar release)
    `finally` void (Session.run (Session.sql "ROLLBACK") connection)

runDirect :: Hasql.Connection -> Session.Session a -> IO a
runDirect connection session = Session.run session connection >>= \case
  Right value -> pure value
  Left err -> expectationFailure (show err) >> fail "direct WorkerSpec session failed"

data ClaimMutationActivity = ClaimMutationActivity
  { activityPid :: !Int32
  , queryStartedAt :: !UTCTime
  , transactionStartedAt :: !UTCTime
  , query :: !Text
  , waitEventType :: !Text
  } deriving stock (Show, Eq)

waitForBlockedClaimMutation :: Pool Hasql.Connection -> Int32 -> IO ClaimMutationActivity
waitForBlockedClaimMutation = waitForBlockedClaimMutationNamed "claim mutation"

waitForBlockedClaimMutationNamed :: String -> Pool Hasql.Connection -> Int32 -> IO ClaimMutationActivity
waitForBlockedClaimMutationNamed label pool holderPid = do
  result <- timeout 25000000 poll
  case result of
    Just activity -> pure activity
    Nothing -> expectationFailure ("timed out observing the Worker " <> label <> " lock wait") >> fail "unreachable"
  where
    poll = runSession pool (Session.statement holderPid claimMutationWaitingStatement) >>= \case
      [activity] -> pure activity
      _ -> threadDelay 10000 >> poll

waitForBlockedClaimMutationAfter
  :: String -> Pool Hasql.Connection -> Int32 -> UTCTime -> IO ClaimMutationActivity
waitForBlockedClaimMutationAfter label pool holderPid startedAfter = do
  result <- timeout 12000000 pollAfter
  case result of
    Just activity -> pure activity
    Nothing -> expectationFailure ("timed out observing the Worker " <> label <> " lock wait") >> fail "unreachable"
  where
    pollAfter = runSession pool (Session.statement holderPid claimMutationWaitingStatement) >>= \activities ->
      case filter (\activity -> activity.queryStartedAt > startedAfter) activities of
        [activity] -> pure activity
        _ -> threadDelay 10000 >> pollAfter

setAttemptsStatement :: Statement.Statement UUID ()
setAttemptsStatement = Statement.Statement "UPDATE public.embedding_jobs SET attempts = 15 WHERE observation_id = $1" (Enc.param (Enc.nonNullable Enc.uuid)) Dec.noResult True

expireLeaseStatement :: Statement.Statement UUID ()
expireLeaseStatement = Statement.Statement
  "UPDATE public.embedding_jobs SET lease_expires_at = now() - interval '1 second' WHERE observation_id = $1"
  (Enc.param (Enc.nonNullable Enc.uuid)) Dec.noResult True

jobStateAttemptsStatement :: Statement.Statement UUID (Text, Int)
jobStateAttemptsStatement = Statement.Statement "SELECT state, attempts FROM public.embedding_jobs WHERE observation_id = $1"
  (Enc.param (Enc.nonNullable Enc.uuid))
  (Dec.singleRow ((,) <$> Dec.column (Dec.nonNullable Dec.text) <*> (fromIntegral <$> Dec.column (Dec.nonNullable Dec.int4)))) True

jobStateAttemptsOwnerStatement :: Statement.Statement UUID (Text, Int, Maybe Text)
jobStateAttemptsOwnerStatement = Statement.Statement
  "SELECT state, attempts, lease_owner FROM public.embedding_jobs WHERE observation_id = $1"
  (Enc.param (Enc.nonNullable Enc.uuid))
  (Dec.singleRow ((,,)
    <$> Dec.column (Dec.nonNullable Dec.text)
    <*> (fromIntegral <$> Dec.column (Dec.nonNullable Dec.int4))
    <*> Dec.column (Dec.nullable Dec.text))) True

jobStateStatement :: Statement.Statement UUID Text
jobStateStatement = Statement.Statement
  "SELECT state FROM public.embedding_jobs WHERE observation_id = $1"
  (Enc.param (Enc.nonNullable Enc.uuid))
  (Dec.singleRow (Dec.column (Dec.nonNullable Dec.text))) True

vectorHeadStatement :: Statement.Statement UUID (Maybe Double, Maybe Double)
vectorHeadStatement = Statement.Statement
  "SELECT (embedding::real[])[1]::double precision, (embedding::real[])[2]::double precision FROM public.observations WHERE id = $1"
  (Enc.param (Enc.nonNullable Enc.uuid))
  (Dec.singleRow ((,) <$> Dec.column (Dec.nullable Dec.float8) <*> Dec.column (Dec.nullable Dec.float8))) True

leaseExpiryStatement :: Statement.Statement UUID (Maybe UTCTime)
leaseExpiryStatement = Statement.Statement
  "SELECT lease_expires_at FROM public.embedding_jobs WHERE observation_id = $1"
  (Enc.param (Enc.nonNullable Enc.uuid))
  (Dec.singleRow (Dec.column (Dec.nullable Dec.timestamptz))) True

dbClockAfterStatement :: Statement.Statement UTCTime Bool
dbClockAfterStatement = Statement.Statement
  "SELECT clock_timestamp() > $1"
  (Enc.param (Enc.nonNullable Enc.timestamptz))
  (Dec.singleRow (Dec.column (Dec.nonNullable Dec.bool))) True

claimOrderContentsStatement :: Statement.Statement UUID [Text]
claimOrderContentsStatement = Statement.Statement
  "SELECT o.content FROM public.embedding_jobs j JOIN public.observations o ON o.id = j.observation_id WHERE j.workspace_id = $1 ORDER BY j.observation_id"
  (Enc.param (Enc.nonNullable Enc.uuid))
  (Dec.rowList (Dec.column (Dec.nonNullable Dec.text))) True

lockObservationStatement :: Statement.Statement UUID ()
lockObservationStatement = Statement.Statement
  "SELECT id FROM public.observations WHERE id = $1 FOR UPDATE"
  (Enc.param (Enc.nonNullable Enc.uuid))
  (void (Dec.singleRow (Dec.column (Dec.nonNullable Dec.uuid)))) True

backendPidStatement :: Statement.Statement () Int32
backendPidStatement = Statement.Statement
  "SELECT pg_backend_pid()"
  Enc.noParams (Dec.singleRow (Dec.column (Dec.nonNullable Dec.int4))) True

claimMutationWaitingStatement :: Statement.Statement Int32 [ClaimMutationActivity]
claimMutationWaitingStatement = Statement.Statement
  "SELECT activity.pid, activity.query_start, activity.xact_start, activity.query, coalesce(activity.wait_event_type, '') FROM pg_stat_activity activity WHERE $1 = ANY(pg_blocking_pids(activity.pid)) AND activity.wait_event_type = 'Lock' AND activity.query LIKE '%FROM public.observations o%FOR UPDATE%' ORDER BY activity.pid"
  (Enc.param (Enc.nonNullable Enc.int4))
  (Dec.rowList (ClaimMutationActivity
    <$> Dec.column (Dec.nonNullable Dec.int4)
    <*> Dec.column (Dec.nonNullable Dec.timestamptz)
    <*> Dec.column (Dec.nonNullable Dec.timestamptz)
    <*> Dec.column (Dec.nonNullable Dec.text)
    <*> Dec.column (Dec.nonNullable Dec.text))) True

claimMutationActivityStatement :: Statement.Statement Int32 [(Text, Text, Double)]
claimMutationActivityStatement = Statement.Statement
  "SELECT activity.query, coalesce(activity.wait_event_type, ''), extract(epoch FROM clock_timestamp() - activity.query_start)::double precision FROM pg_stat_activity activity WHERE $1 = ANY(pg_blocking_pids(activity.pid)) ORDER BY activity.pid"
  (Enc.param (Enc.nonNullable Enc.int4))
  (Dec.rowList ((,,) <$> Dec.column (Dec.nonNullable Dec.text) <*> Dec.column (Dec.nonNullable Dec.text) <*> Dec.column (Dec.nonNullable Dec.float8))) True

backendTransactionStateStatement :: Statement.Statement Int32 [(Text, Bool)]
backendTransactionStateStatement = Statement.Statement
  "SELECT state, xact_start IS NULL FROM pg_stat_activity WHERE pid = $1"
  (Enc.param (Enc.nonNullable Enc.int4))
  (Dec.rowList ((,) <$> Dec.column (Dec.nonNullable Dec.text) <*> Dec.column (Dec.nonNullable Dec.bool))) True

backendIsQuiescent :: (Text, Bool) -> Bool
backendIsQuiescent (state, noTransaction) = state == "idle" && noTransaction

testSpace :: Text -> EmbeddingSpaceFingerprint
testSpace raw = fromMaybe (error "test embedding space must be valid") (parseEmbeddingSpaceFingerprint raw)
