-- | Injectable, one-batch worker boundary.  Application lifecycle ownership
-- remains outside this module; callers may run this repeatedly with their own
-- cancellation and delay policy.
module HMem.Server.Embedding.Worker
  ( EmbeddingWorker(..)
  , EmbeddingWorkerLeasePolicy(..)
  , defaultEmbeddingWorkerClock
  , defaultEmbeddingWorkerLeasePolicy
  , runEmbeddingWorkerOnce
  , runEmbeddingWorkerOnceWithLeasePolicy
  , runEmbeddingWorker
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (AsyncCancelled, cancelWith, waitCatch, waitEitherCatch, withAsync)
import Control.Exception (Exception(..), SomeAsyncException, SomeException, asyncExceptionFromException, asyncExceptionToException, catch, fromException, mask, throwIO, try)
import Control.Monad (void)
import Data.Maybe (catMaybes)
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

-- | Correctness-only lease policy.  The production values keep renewal well
-- inside the fixed lease; measured provider deadlines and batching remain a
-- separate integration concern.  Tests can shorten the lease and synchronize
-- the delay without changing the public 'EmbeddingWorker' constructor.
data EmbeddingWorkerLeasePolicy = EmbeddingWorkerLeasePolicy
  { leaseSeconds :: !Int
  , waitBeforeRenewal :: !(IO ())
  , renewClaim :: !(Pool Hasql.Connection -> Int -> DB.EmbeddingJob -> IO Bool)
  }

defaultEmbeddingWorkerLeasePolicy :: EmbeddingWorkerLeasePolicy
defaultEmbeddingWorkerLeasePolicy = EmbeddingWorkerLeasePolicy
  { leaseSeconds = 60
  , waitBeforeRenewal = threadDelay 20000000
  , renewClaim = DB.renewEmbeddingJobFor
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
runEmbeddingWorkerOnce = runEmbeddingWorkerOnceWithLeasePolicy defaultEmbeddingWorkerLeasePolicy

runEmbeddingWorkerOnceWithLeasePolicy :: EmbeddingWorkerLeasePolicy -> EmbeddingWorker -> IO ()
runEmbeddingWorkerOnceWithLeasePolicy leasePolicy worker = do
  stopped <- worker.cancelled
  state <- worker.provider.availability
  case (stopped, state) of
    (True, _) -> pure ()
    (_, EmbeddingDisabled) -> pure ()
    (_, EmbeddingUnavailable _) -> pure () -- no claim without a usable provider
    (_, EmbeddingAvailable) -> do
      _ <- DB.reconcileEmbeddingJobs worker.pool worker.batchSize
      mask $ \restore -> do
        jobs <- DB.claimEmbeddingJobsWithLease worker.pool worker.leaseOwner worker.batchSize leasePolicy.leaseSeconds
        restore (do
          stoppedAfterClaim <- worker.cancelled
          if stoppedAfterClaim
            then releaseCancelledAll worker jobs
            else process leasePolicy worker jobs)
          `catch` \(primary :: SomeException) -> do
            cleanupErrors <- collectReleaseErrors (releaseCancelled worker) jobs
            throwPreferredException (Just primary) cleanupErrors

process :: EmbeddingWorkerLeasePolicy -> EmbeddingWorker -> [DB.EmbeddingJob] -> IO ()
process _ _ [] = pure ()
process leasePolicy worker jobs = do
  let (rejected, admitted) = partitionAdmission jobs
  releaseFailureAll worker ("provider_protocol", False) rejected
  case admitted of
    [] -> pure ()
    _ -> do
      let request = EmbeddingRequest [EmbeddingInput EmbeddingDocument job.content | job <- admitted]
      (withAsync (renewClaims leasePolicy worker admitted) $ \renewalTask ->
        withAsync (embedGteQwen2 worker.provider request) $ \providerTask ->
          waitEitherCatch renewalTask providerTask >>= \case
            Left renewalResult -> do
              cancelWith providerTask StopProvider
              providerResult <- waitCatch providerTask
              case renewalResult of
                Right RenewalCancelled -> finishAfterRenewal admitted providerResult
                Right RenewalLost -> finishAfterRenewal admitted providerResult
                Left exceptionValue -> releaseForObservedExceptions worker exceptionValue
                  (unexpectedHelperException StopProvider providerResult) admitted
            Right providerResult -> do
              cancelWith renewalTask StopRenewal
              renewalResult <- waitCatch renewalTask
              case providerResult of
                Left exceptionValue -> releaseForObservedExceptions worker exceptionValue
                  (unexpectedHelperException StopRenewal renewalResult) admitted
                Right response -> case renewalResult of
                  Left exceptionValue
                    | isOwnedCancellation StopRenewal exceptionValue -> finishProviderResponse admitted response
                    | otherwise -> releaseForException worker exceptionValue admitted
                  Right _ -> releaseCancelledAll worker admitted)
  where
    finishAfterRenewal admitted providerResult = case providerResult of
      Left exceptionValue
        | isOwnedCancellation StopProvider exceptionValue -> releaseCancelledAll worker admitted
        | otherwise -> releaseForException worker exceptionValue admitted
      Right _ -> releaseCancelledAll worker admitted

    finishProviderResponse admitted = \case
      Left errorValue -> releaseFailureAll worker (failureFor errorValue) admitted
      Right batch -> completeBatch admitted batch.vectors

    completeBatch [] [] = pure ()
    completeBatch (job : remainingJobs) (vector : remainingVectors) = do
      stopped <- worker.cancelled
      if stopped
        then releaseCancelledAll worker (job : remainingJobs)
        else do
          outcome <- DB.completeClaimedEmbeddingJob worker.pool job vector
          case outcome of
            DB.EmbeddingApplied -> completeBatch remainingJobs remainingVectors
            DB.EmbeddingAlreadySatisfied -> completeBatch remainingJobs remainingVectors
            DB.EmbeddingStale -> completeBatch remainingJobs remainingVectors
            DB.EmbeddingNotFound -> completeBatch remainingJobs remainingVectors
    completeBatch remainingJobs _ = releaseFailureAll worker ("provider_protocol", False) remainingJobs

data WorkerOwnedCancellation = StopProvider | StopRenewal
  deriving stock (Show, Eq)

instance Exception WorkerOwnedCancellation where
  toException = asyncExceptionToException
  fromException = asyncExceptionFromException

isOwnedCancellation :: WorkerOwnedCancellation -> SomeException -> Bool
isOwnedCancellation expected exceptionValue =
  fromException exceptionValue == Just expected

data RenewalResult = RenewalCancelled | RenewalLost

renewClaims :: EmbeddingWorkerLeasePolicy -> EmbeddingWorker -> [DB.EmbeddingJob] -> IO RenewalResult
renewClaims leasePolicy worker jobs = loop
  where
    loop = do
      leasePolicy.waitBeforeRenewal
      stopped <- worker.cancelled
      if stopped
        then pure RenewalCancelled
        else do
          renewed <- traverse (leasePolicy.renewClaim worker.pool leasePolicy.leaseSeconds) jobs
          if and renewed then loop else pure RenewalLost

partitionAdmission :: [DB.EmbeddingJob] -> ([DB.EmbeddingJob], [DB.EmbeddingJob])
partitionAdmission = foldr classify ([], [])
  where
    classify job (rejected, admitted) =
      case prepareGteQwen2Request (EmbeddingRequest [EmbeddingInput EmbeddingDocument job.content]) of
        Left _ -> (job : rejected, admitted)
        Right _ -> (rejected, job : admitted)

releaseCancelled :: EmbeddingWorker -> DB.EmbeddingJob -> IO ()
releaseCancelled worker job = void $ DB.releaseEmbeddingJob worker.pool job "provider_cancelled" True

releaseFailure :: EmbeddingWorker -> (Text, Bool) -> DB.EmbeddingJob -> IO ()
releaseFailure worker (code, retry) job = void $ DB.releaseEmbeddingJob worker.pool job code retry

releaseForException :: EmbeddingWorker -> SomeException -> [DB.EmbeddingJob] -> IO ()
releaseForException worker exceptionValue jobs = do
  cleanupErrors <- collectReleaseErrors releaseOne jobs
  throwPreferredException (Just exceptionValue) cleanupErrors
  where
    releaseOne
      | isAsyncException exceptionValue = releaseCancelled worker
      | otherwise = releaseFailure worker ("provider_unavailable", True)

releaseForObservedExceptions
  :: EmbeddingWorker -> SomeException -> [SomeException] -> [DB.EmbeddingJob] -> IO ()
releaseForObservedExceptions worker primary observed jobs =
  case preferredException (Just primary) observed of
    Just exceptionValue -> releaseForException worker exceptionValue jobs
    Nothing -> error "releaseForObservedExceptions requires a primary exception"

unexpectedHelperException
  :: WorkerOwnedCancellation -> Either SomeException a -> [SomeException]
unexpectedHelperException ownedStop = \case
  Left exceptionValue
    | not (isOwnedCancellation ownedStop exceptionValue) -> [exceptionValue]
  _ -> []

releaseCancelledAll :: EmbeddingWorker -> [DB.EmbeddingJob] -> IO ()
releaseCancelledAll worker = releaseAll (releaseCancelled worker)

releaseFailureAll :: EmbeddingWorker -> (Text, Bool) -> [DB.EmbeddingJob] -> IO ()
releaseFailureAll worker failure = releaseAll (releaseFailure worker failure)

releaseAll :: (DB.EmbeddingJob -> IO ()) -> [DB.EmbeddingJob] -> IO ()
releaseAll releaseOne jobs = do
  errors <- collectReleaseErrors releaseOne jobs
  case preferredException Nothing errors of
    Nothing -> pure ()
    Just exceptionValue -> throwIO exceptionValue

collectReleaseErrors :: (DB.EmbeddingJob -> IO ()) -> [DB.EmbeddingJob] -> IO [SomeException]
collectReleaseErrors releaseOne = fmap catMaybes . traverse attempt
  where
    attempt job = try (releaseOne job) >>= \case
      Left exceptionValue -> pure (Just exceptionValue)
      Right () -> pure Nothing

throwPreferredException :: Maybe SomeException -> [SomeException] -> IO a
throwPreferredException primary cleanupErrors =
  case preferredException primary cleanupErrors of
    Just exceptionValue -> throwIO exceptionValue
    Nothing -> error "throwPreferredException requires a primary or cleanup exception"

preferredException :: Maybe SomeException -> [SomeException] -> Maybe SomeException
preferredException primary cleanupErrors =
  case primary of
    Just exceptionValue
      | isExternalAsyncException exceptionValue -> Just exceptionValue
    _ -> case filter isExternalAsyncException cleanupErrors of
      exceptionValue : _ -> Just exceptionValue
      [] -> case primary of
        Just exceptionValue -> Just exceptionValue
        Nothing -> case cleanupErrors of
          exceptionValue : _ -> Just exceptionValue
          [] -> Nothing

isExternalAsyncException :: SomeException -> Bool
isExternalAsyncException exceptionValue =
  isAsyncException exceptionValue
    && not (isOwnedCancellation StopProvider exceptionValue)
    && not (isOwnedCancellation StopRenewal exceptionValue)

isAsyncException :: SomeException -> Bool
isAsyncException exceptionValue =
  case fromException exceptionValue :: Maybe SomeAsyncException of
    Just _ -> True
    Nothing -> case fromException exceptionValue :: Maybe AsyncCancelled of
      Just _ -> True
      Nothing -> False

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
