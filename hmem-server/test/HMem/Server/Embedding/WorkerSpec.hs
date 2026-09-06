module HMem.Server.Embedding.WorkerSpec (spec) where

import Data.IORef
import Data.List (sort)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Time (getCurrentTime)
import Data.UUID (UUID)
import Hasql.Decoders qualified as Dec
import Hasql.Encoders qualified as Enc
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement
import Control.Monad (unless)
import Test.Hspec

import HMem.Config (managedTeiSpaceFingerprint)
import HMem.DB.Embedding
import HMem.DB.Observation
import HMem.DB.Pool (checkPgvector)
import HMem.DB.Pool (runSession)
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

testSpace :: Text -> EmbeddingSpaceFingerprint
testSpace raw = fromMaybe (error "test embedding space must be valid") (parseEmbeddingSpaceFingerprint raw)
