-- | Injectable, one-batch worker boundary.  Application lifecycle ownership
-- remains outside this module; callers may run this repeatedly with their own
-- cancellation and delay policy.
module HMem.Server.Embedding.Worker
  ( EmbeddingWorker(..)
  , defaultEmbeddingWorkerClock
  , runEmbeddingWorkerOnce
  , runEmbeddingWorker
  ) where

import Data.Pool (Pool)
import Data.Text (Text)
import Data.Time (UTCTime, getCurrentTime)
import Hasql.Connection qualified as Hasql

import HMem.DB.Embedding qualified as DB
import HMem.Server.Embedding.GteQwen2
import HMem.Server.Embedding.Provider

data EmbeddingWorker = EmbeddingWorker
  { pool :: !(Pool Hasql.Connection)
  , provider :: !EmbeddingProvider
  , leaseOwner :: !Text
  , batchSize :: !Int
  , clock :: !(IO UTCTime)
  , cancelled :: !(IO Bool)
  }

-- | The production clock.  Tests and lifecycle wiring may inject a deterministic
-- alternative through 'EmbeddingWorker.clock'.
defaultEmbeddingWorkerClock :: IO UTCTime
defaultEmbeddingWorkerClock = getCurrentTime

-- | A cancellable production loop with an injected wait policy.  Keeping the
-- wait action at the call boundary makes clock/backoff ownership testable and
-- avoids binding this persistence module to an application scheduler.
runEmbeddingWorker :: EmbeddingWorker -> (UTCTime -> IO ()) -> IO ()
runEmbeddingWorker worker waitForNext = go
  where
    go = do
      stopped <- worker.cancelled
      if stopped
        then pure ()
        else do
          runEmbeddingWorkerOnce worker
          stoppedAfterBatch <- worker.cancelled
          if stoppedAfterBatch then pure () else worker.clock >>= waitForNext >> go

-- | Claim one bounded batch, format documents through the pinned semantic
-- contract, and use the shared CAS for every result.  Cancellation does not
-- become a terminal provider failure; leases are returned to retryable work.
runEmbeddingWorkerOnce :: EmbeddingWorker -> IO ()
runEmbeddingWorkerOnce worker = do
  stopped <- worker.cancelled
  state <- worker.provider.availability
  case (stopped, state) of
    (True, _) -> pure ()
    (_, EmbeddingDisabled) -> pure ()
    (_, EmbeddingUnavailable _) -> pure () -- no claim without a usable provider
    (_, EmbeddingAvailable) -> do
      _ <- DB.reconcileEmbeddingJobs worker.pool worker.batchSize
      jobs <- DB.claimEmbeddingJobs worker.pool worker.leaseOwner worker.batchSize
      stoppedAfterClaim <- worker.cancelled
      if stoppedAfterClaim
        then mapM_ (releaseCancelled worker) jobs
        else process worker jobs

process :: EmbeddingWorker -> [DB.EmbeddingJob] -> IO ()
process _ [] = pure ()
process worker jobs = do
  let request = EmbeddingRequest [EmbeddingInput EmbeddingDocument job.content | job <- jobs]
  embedGteQwen2 worker.provider request >>= \case
    Left errorValue -> mapM_ (releaseFailure worker (failureFor errorValue)) jobs
    Right batch -> sequence_ [complete job vector | (job, vector) <- zip jobs batch.vectors]
  where
    complete job vector = do
      stopped <- worker.cancelled
      if stopped
        then releaseCancelled worker job
        else do
          _ <- DB.completeClaimedEmbeddingJob worker.pool worker.leaseOwner job.workspaceId job.observationId job.contentFingerprint job.spaceFingerprint vector
          pure ()

releaseCancelled :: EmbeddingWorker -> DB.EmbeddingJob -> IO ()
releaseCancelled worker job = DB.releaseEmbeddingJob worker.pool job.observationId worker.leaseOwner "provider_cancelled" True

releaseFailure :: EmbeddingWorker -> (Text, Bool) -> DB.EmbeddingJob -> IO ()
releaseFailure worker (code, retry) job = DB.releaseEmbeddingJob worker.pool job.observationId worker.leaseOwner code retry

failureFor :: GteQwen2InvocationError -> (Text, Bool)
failureFor = \case
  GteQwen2ProviderError failure -> case failure.errorCode of
    ProviderCancelled -> ("provider_cancelled", True)
    ProviderUnavailable -> ("provider_unavailable", True)
    ProviderTimedOut -> ("provider_timeout", True)
    ProviderProtocolError -> ("provider_protocol", failure.retryable)
    ProviderDisabled -> ("provider_protocol", False)
    ProviderConfigurationError -> ("provider_protocol", False)
  GteQwen2PreparationError{} -> ("provider_protocol", False)
  GteQwen2FinalizationError{} -> ("provider_protocol", False)
