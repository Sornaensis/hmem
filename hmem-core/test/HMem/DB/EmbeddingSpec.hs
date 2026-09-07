module HMem.DB.EmbeddingSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (concurrently, race, wait, waitCatch, withAsync)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar, tryPutMVar)
import Control.Exception (bracket, finally, throwIO, try)
import Control.Monad (unless, void)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as B8
import Data.Char (toLower)
import Data.Functor.Contravariant (contramap)
import Data.Int (Int32, Int64)
import Data.List (sortOn)
import Data.Pool qualified as Pool
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime)
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import Hasql.Connection qualified as Hasql
import Hasql.Decoders qualified as Dec
import Hasql.Encoders qualified as Enc
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement
import Test.Hspec
import System.Timeout (timeout)

import HMem.Config qualified as Config
import HMem.DB.Embedding
import HMem.DB.Observation
import HMem.DB.Pool (DBException(..), checkPgvector, createPool, runSession)
import HMem.DB.TestHarness
import HMem.Types

spec :: Spec
spec = around withTestEnv $ do
  describe "embedding space isolation and durable jobs" $ do
    it "never compares legacy and managed vectors in one similarity query" $ \env -> do
      requirePgvector env
      workspace <- createTestWorkspace env "embedding-space-isolation"
      legacy <- create env workspace "legacy"
      managed <- create env workspace "managed"
      let managedSpace = testSpace "hmem:managed-gte-qwen2:test"
      setObservationEmbeddingInSpace env.pool workspace.id legacy.id legacyManualEmbeddingSpace unitX
      setObservationEmbeddingInSpace env.pool workspace.id managed.id managedSpace unitX
      results <- similarObservations env.pool SimilarObservationQuery
        { workspaceId = workspace.id, subjectKind = Nothing, subject = Nothing, gitSha = Nothing
        , embedding = unitX, spaceFingerprint = Just managedSpace, minSimilarity = Just 1
        , limit = Nothing, offset = Nothing
        }
      map (.observation.id) results `shouldBe` [managed.id]

    it "preserves historical spaces until a guarded noncausal recomputation replaces one vector" $ \env -> do
      requirePgvector env
      workspace <- createTestWorkspace env "embedding-qualified-noncausal-isolation"
      historical <- create env workspace "historical unknown attention"
      legacy <- create env workspace "legacy manual"
      qualified <- create env workspace "already noncausal"
      let historicalSpace = testSpace historicalManagedFingerprint
          newSpace = testSpace Config.managedTeiSpaceFingerprint
      setObservationEmbeddingInSpace env.pool workspace.id historical.id historicalSpace unitX
      setObservationEmbeddingInSpace env.pool workspace.id legacy.id legacyManualEmbeddingSpace unitX
      setObservationEmbeddingInSpace env.pool workspace.id qualified.id newSpace unitX

      resultsBefore <- similarObservations env.pool (similarQueryFor workspace.id newSpace)
      map (.observation.id) resultsBefore `shouldBe` [qualified.id]

      enableEmbeddingTarget env.pool newSpace
      reconcileEmbeddingJobs env.pool 10 `shouldReturn` 2
      observationSpace env.pool historical.id `shouldReturn` Just historicalManagedFingerprint
      observationSpace env.pool legacy.id `shouldReturn` Just (embeddingSpaceFingerprintText legacyManualEmbeddingSpace)
      observationSpace env.pool qualified.id `shouldReturn` Just Config.managedTeiSpaceFingerprint

      claims <- claimEmbeddingJobs env.pool "noncausal-worker" 10
      map (.observationId) claims `shouldMatchList` [historical.id, legacy.id]
      historicalClaim <- case [claim | claim <- claims, claim.observationId == historical.id] of
        [claim] -> pure claim
        _ -> expectationFailure "expected one claimed historical vector" >> fail "unreachable"
      completeClaimedEmbeddingJob env.pool
        historicalClaim { contentFingerprint = historicalClaim.contentFingerprint <> "-stale" }
        unitX `shouldReturn` EmbeddingStale
      completeClaimedEmbeddingJob env.pool
        historicalClaim { spaceFingerprint = historicalSpace }
        unitX `shouldReturn` EmbeddingStale
      completeClaimedEmbeddingJob env.pool historicalClaim unitX `shouldReturn` EmbeddingApplied

      resultsAfter <- similarObservations env.pool (similarQueryFor workspace.id newSpace)
      map (.observation.id) resultsAfter `shouldMatchList` [historical.id, qualified.id]
      observationSpace env.pool historical.id `shouldReturn` Just Config.managedTeiSpaceFingerprint
      observationSpace env.pool legacy.id `shouldReturn` Just (embeddingSpaceFingerprintText legacyManualEmbeddingSpace)

    it "rejects stale content CAS and treats an exact completed space as idempotent" $ \env -> do
      requirePgvector env
      workspace <- createTestWorkspace env "embedding-cas"
      observation <- create env workspace "before"
      let managedSpace = testSpace "hmem:managed-gte-qwen2:test"
          expected = observationContentFingerprint observation.gitSha observation.subjects observation.content
      compareAndSetObservationEmbedding env.pool workspace.id observation.id expected managedSpace unitX `shouldReturn` EmbeddingApplied
      compareAndSetObservationEmbedding env.pool workspace.id observation.id expected managedSpace unitX `shouldReturn` EmbeddingAlreadySatisfied
      _ <- updateObservation env.pool workspace.id observation.id (UpdateObservation "after")
      compareAndSetObservationEmbedding env.pool workspace.id observation.id expected managedSpace unitX `shouldReturn` EmbeddingStale

    it "returns not-found when a compare-and-set target is deleted" $ \env -> do
      requirePgvector env
      workspace <- createTestWorkspace env "embedding-cas-deleted"
      let managedSpace = testSpace "hmem:managed-gte-qwen2:test"
      enableEmbeddingTarget env.pool managedSpace
      observation <- create env workspace "deleted before completion"
      let expected = observationContentFingerprint observation.gitSha observation.subjects observation.content
      runSession env.pool (Session.statement observation.id jobExistsStatement) `shouldReturn` True
      deleteObservation env.pool workspace.id observation.id `shouldReturn` True
      runSession env.pool (Session.statement observation.id jobExistsStatement) `shouldReturn` False
      compareAndSetObservationEmbedding env.pool workspace.id observation.id expected managedSpace unitX `shouldReturn` EmbeddingNotFound

    it "creates and supersedes active-target work atomically, and direct writes settle only the exact job" $ \env -> do
      requirePgvector env
      workspace <- createTestWorkspace env "embedding-jobs"
      let managedSpace = testSpace "hmem:managed-gte-qwen2:test"
      enableEmbeddingTarget env.pool managedSpace
      observation <- create env workspace "one"
      setObservationEmbeddingInSpace env.pool workspace.id observation.id managedSpace unitX
      claimEmbeddingJobs env.pool "worker-one" 1 `shouldReturn` []
      oldJob <- create env workspace "before"
      [claimed] <- claimEmbeddingJobs env.pool "worker-one" 1
      claimed.observationId `shouldBe` oldJob.id
      _ <- updateObservation env.pool workspace.id oldJob.id (UpdateObservation "after")
      completeClaimedEmbeddingJob env.pool claimed unitX
        `shouldReturn` EmbeddingStale
      [replacement] <- claimEmbeddingJobs env.pool "worker-two" 1
      replacement.observationId `shouldBe` oldJob.id
      replacement.content `shouldBe` "after"

    it "requeues a completed matching job when an unchanged update clears its vector, and reconciles same-space stale content" $ \env -> do
      requirePgvector env
      workspace <- createTestWorkspace env "embedding-content-reconcile"
      let managedSpace = testSpace "hmem:managed-gte-qwen2:content"
      enableEmbeddingTarget env.pool managedSpace
      observation <- create env workspace "same"
      setObservationEmbeddingInSpace env.pool workspace.id observation.id managedSpace unitX
      _ <- updateObservation env.pool workspace.id observation.id (UpdateObservation "same")
      [requeued] <- claimEmbeddingJobs env.pool "after-update" 1
      requeued.observationId `shouldBe` observation.id
      -- Model an interrupted/manual vector clear with an old job hash.  The
      -- target space remains the same, so reconcile must compare the content
      -- hash rather than treating the space as sufficient.
      runSession env.pool $ Session.statement observation.id clearVectorAndStaleJobStatement
      reconcileEmbeddingJobs env.pool 1 `shouldReturn` 1
      [reconciled] <- claimEmbeddingJobs env.pool "after-reconcile" 1
      reconciled.contentFingerprint `shouldBe`
        observationContentFingerprint observation.gitSha observation.subjects "same"

    it "requeues a completed exact job after a disabled same-content update clears the vector" $ \env -> do
      requirePgvector env
      workspace <- createTestWorkspace env "embedding-disabled-content"
      let managedSpace = testSpace "hmem:managed-gte-qwen2:disabled-content"
      enableEmbeddingTarget env.pool managedSpace
      observation <- create env workspace "same"
      setObservationEmbeddingInSpace env.pool workspace.id observation.id managedSpace unitX
      disableEmbeddingTarget env.pool
      _ <- updateObservation env.pool workspace.id observation.id (UpdateObservation "same")
      enableEmbeddingTarget env.pool managedSpace
      reconcileEmbeddingJobs env.pool 1 `shouldReturn` 1
      [requeued] <- claimEmbeddingJobs env.pool "disabled-content" 1
      requeued.observationId `shouldBe` observation.id

    it "requeues the managed completed job when a direct different-space vector replaces it" $ \env -> do
      requirePgvector env
      workspace <- createTestWorkspace env "embedding-different-space"
      let managedSpace = testSpace "hmem:managed-gte-qwen2:different-space"
      enableEmbeddingTarget env.pool managedSpace
      observation <- create env workspace "same"
      setObservationEmbeddingInSpace env.pool workspace.id observation.id managedSpace unitX
      setObservationEmbeddingInSpace env.pool workspace.id observation.id legacyManualEmbeddingSpace unitX
      reconcileEmbeddingJobs env.pool 1 `shouldReturn` 1
      [requeued] <- claimEmbeddingJobs env.pool "different-space" 1
      requeued.spaceFingerprint `shouldBe` managedSpace

    it "serializes reconciliation with a same-space direct setter so no satisfied job remains claimable" $ \env -> do
      requirePgvector env
      workspace <- createTestWorkspace env "embedding-reconcile-setter-race"
      let managedSpace = testSpace "hmem:managed-gte-qwen2:setter-race"
      enableEmbeddingTarget env.pool managedSpace
      observation <- create env workspace "same"
      setObservationEmbeddingInSpace env.pool workspace.id observation.id managedSpace unitX
      runSession env.pool $ Session.statement observation.id clearVectorStatement
      _ <- concurrently
        (reconcileEmbeddingJobs env.pool 1)
        (setObservationEmbeddingInSpace env.pool workspace.id observation.id managedSpace unitX)
      claimEmbeddingJobs env.pool "after-setter-race" 1 `shouldReturn` []

    it "settles an owned lease when its exact vector was restored before completion" $ \env -> do
      requirePgvector env
      workspace <- createTestWorkspace env "embedding-owned-already-satisfied"
      let managedSpace = testSpace "hmem:managed-gte-qwen2:already-satisfied"
      enableEmbeddingTarget env.pool managedSpace
      observation <- create env workspace "same"
      [claimed] <- claimEmbeddingJobs env.pool "owner" 1
      runSession env.pool $ Session.statement (workspace.id, observation.id, unitVectorText, embeddingSpaceFingerprintText managedSpace) setVectorWithoutSettlingStatement
      completeClaimedEmbeddingJob env.pool claimed unitX
        `shouldReturn` EmbeddingAlreadySatisfied
      claimEmbeddingJobs env.pool "after-already-satisfied" 1 `shouldReturn` []

    it "bounds a reconciliation scan and reaches stale work after valid exact pending jobs" $ \env -> do
      requirePgvector env
      workspace <- createTestWorkspace env "embedding-cursor"
      let managedSpace = testSpace "hmem:managed-gte-qwen2:cursor"
      enableEmbeddingTarget env.pool managedSpace
      observations <- traverse (\number -> create env workspace ("cursor-" <> T.pack (show number))) [1 :: Int .. 9]
      let ordered = sortOn (.id) observations
          stale = last ordered
      runSession env.pool $ Session.statement stale.id clearVectorAndStaleJobStatement
      -- A batch of one scans only eight candidates.  The stale final row is
      -- reached on the next call through the persisted cursor, not by an
      -- unbounded scan of every exact pending job.
      reconcileEmbeddingJobs env.pool 1 `shouldReturn` 0
      reconcileEmbeddingJobs env.pool 1 `shouldReturn` 1
      runSession env.pool (Session.statement stale.id jobStateStatement) `shouldReturn` "pending"

    it "switches the active space in bounded reconciliation and rejects old leased completion" $ \env -> do
      requirePgvector env
      workspace <- createTestWorkspace env "embedding-space-switch"
      let firstSpace = testSpace "hmem:managed-gte-qwen2:first"
          secondSpace = testSpace "hmem:managed-gte-qwen2:second"
      enableEmbeddingTarget env.pool firstSpace
      observation <- create env workspace "one"
      [firstClaim] <- claimEmbeddingJobs env.pool "worker-one" 1
      firstClaim.observationId `shouldBe` observation.id
      enableEmbeddingTarget env.pool secondSpace
      reconcileEmbeddingJobs env.pool 1 `shouldReturn` 1
      completeClaimedEmbeddingJob env.pool firstClaim unitX
        `shouldReturn` EmbeddingStale
      [secondClaim] <- claimEmbeddingJobs env.pool "worker-two" 1
      secondClaim.spaceFingerprint `shouldBe` secondSpace

    it "preserves current leases and advances a switched target one bounded row at a time" $ \env -> do
      requirePgvector env
      workspace <- createTestWorkspace env "embedding-reconcile-batches"
      let firstSpace = testSpace "hmem:managed-gte-qwen2:batch-first"
          secondSpace = testSpace "hmem:managed-gte-qwen2:batch-second"
      enableEmbeddingTarget env.pool firstSpace
      _ <- create env workspace "first"
      _ <- create env workspace "second"
      _ <- create env workspace "third"
      [_lease] <- claimEmbeddingJobs env.pool "preserved" 1
      reconcileEmbeddingJobs env.pool 1 `shouldReturn` 0
      enableEmbeddingTarget env.pool secondSpace
      reconcileEmbeddingJobs env.pool 1 `shouldReturn` 1
      reconcileEmbeddingJobs env.pool 1 `shouldReturn` 1
      reconcileEmbeddingJobs env.pool 1 `shouldReturn` 1
      reconcileEmbeddingJobs env.pool 1 `shouldReturn` 0
      claims <- claimEmbeddingJobs env.pool "new-target" 3
      map (.spaceFingerprint) claims `shouldBe` replicate 3 secondSpace

    it "claims concurrently without duplication and reclaims an expired lease" $ \env -> do
      requirePgvector env
      workspace <- createTestWorkspace env "embedding-claims"
      let managedSpace = testSpace "hmem:managed-gte-qwen2:test"
      enableEmbeddingTarget env.pool managedSpace
      _ <- create env workspace "one"
      _ <- create env workspace "two"
      (first, second) <- concurrently
        (claimEmbeddingJobs env.pool "worker-one" 1)
        (claimEmbeddingJobs env.pool "worker-two" 1)
      length first `shouldBe` 1
      length second `shouldBe` 1
      map (.observationId) first `shouldNotBe` map (.observationId) second
      expireLease env.pool (head first).observationId
      reclaimed <- claimEmbeddingJobs env.pool "worker-three" 1
      map (.observationId) reclaimed `shouldBe` map (.observationId) first

    it "uses fresh attempt ownership and fences callbacks after reclaim by the same logical worker" $ \env -> do
      requirePgvector env
      workspace <- createTestWorkspace env "embedding-attempt-fence"
      let managedSpace = testSpace "hmem:managed-gte-qwen2:attempt-fence"
      enableEmbeddingTarget env.pool managedSpace
      observation <- create env workspace "same logical worker"
      [oldAttempt] <- claimEmbeddingJobsWithLease env.pool "logical-worker" 1 1
      expireLease env.pool observation.id
      [newAttempt] <- claimEmbeddingJobsWithLease env.pool "logical-worker" 1 60
      newAttempt.leaseOwner `shouldNotBe` oldAttempt.leaseOwner
      newAttempt.attempts `shouldBe` oldAttempt.attempts + 1
      renewEmbeddingJob env.pool oldAttempt `shouldReturn` False
      completeClaimedEmbeddingJob env.pool oldAttempt unitX `shouldReturn` EmbeddingStale
      releaseEmbeddingJob env.pool oldAttempt "provider_cancelled" True `shouldReturn` False
      renewEmbeddingJob env.pool newAttempt `shouldReturn` True
      completeClaimedEmbeddingJob env.pool newAttempt unitX `shouldReturn` EmbeddingApplied
      runSession env.pool (Session.statement observation.id jobStateStatement) `shouldReturn` "complete"

    it "rejects a reclaimed stale release without waiting on its locked Observation" $ \env -> do
      requirePgvector env
      workspace <- createTestWorkspace env "embedding-release-negative-precheck"
      let managedSpace = testSpace "hmem:managed-gte-qwen2:release-negative"
      enableEmbeddingTarget env.pool managedSpace
      observation <- create env workspace "same logical release"
      withEmbeddingTestPool env $ \lockPool ->
        withEmbeddingTestPool env $ \actionPool ->
          withEmbeddingTestPool env $ \observerPool -> do
            mapM_ establishTestPool [lockPool, actionPool, observerPool]
            [oldAttempt] <- claimEmbeddingJobsWithLease actionPool "same-logical-worker" 1 1
            expireLease actionPool observation.id
            [replacement] <- claimEmbeddingJobsWithLease actionPool "same-logical-worker" 1 60
            replacement.leaseOwner `shouldNotBe` oldAttempt.leaseOwner
            before <- runSession observerPool (Session.statement observation.id jobAttemptSnapshotStatement)
            locked <- newEmptyMVar
            unlock <- newEmptyMVar
            withAsync (holdObservationLock lockPool observation.id locked unlock) $ \holder ->
              (`finally` (void (tryPutMVar unlock ()) >> void (waitCatch holder))) $ do
                holderPid <- timeout 5000000 (takeMVar locked) >>= \case
                  Just pid -> pure pid
                  Nothing -> expectationFailure "timed out acquiring stale-release Observation lock" >> fail "unreachable"
                started <- getMonotonicTimeNSec
                result <- timeout 4500000 (releaseEmbeddingJob actionPool oldAttempt "provider_cancelled" True)
                finished <- getMonotonicTimeNSec
                result `shouldBe` Just False
                let elapsed = secondsBetween started finished
                putStrLn ("[release-negative-precheck] elapsed_seconds=" <> show elapsed)
                elapsed `shouldSatisfy` (< 4.5)
                runSession observerPool (Session.statement holderPid blockedByHolderCountStatement) `shouldReturn` 0
                runSession observerPool (Session.statement observation.id jobAttemptSnapshotStatement) `shouldReturn` before
                runSession observerPool (Session.statement observation.id observationVectorPresentStatement) `shouldReturn` False

    it "rejects every captured release identity mismatch and fences rows sharing one batch token" $ \env -> do
      requirePgvector env
      workspace <- createTestWorkspace env "embedding-release-identity"
      otherWorkspace <- createTestWorkspace env "embedding-release-identity-other"
      let managedSpace = testSpace "hmem:managed-gte-qwen2:release-identity"
      enableEmbeddingTarget env.pool managedSpace
      firstObservation <- create env workspace "first identity"
      secondObservation <- create env workspace "second identity"
      nullOwnerObservation <- create env workspace "null owner"
      attempts <- claimEmbeddingJobs env.pool "shared-batch" 3
      firstAttempt <- requireOnly "first release identity attempt" [job | job <- attempts, job.observationId == firstObservation.id]
      secondAttempt <- requireOnly "second release identity attempt" [job | job <- attempts, job.observationId == secondObservation.id]
      nullOwnerAttempt <- requireOnly "null-owner release attempt" [job | job <- attempts, job.observationId == nullOwnerObservation.id]
      firstAttempt.leaseOwner `shouldBe` secondAttempt.leaseOwner
      let mismatches :: [EmbeddingJob]
          mismatches =
            [ firstAttempt { workspaceId = otherWorkspace.id }
            , firstAttempt { observationId = UUID.nil }
            , firstAttempt { observationId = secondAttempt.observationId }
            , firstAttempt { leaseOwner = "different-attempt-token" }
            , firstAttempt { contentFingerprint = firstAttempt.contentFingerprint <> "-different" }
            , firstAttempt { spaceFingerprint = testSpace "hmem:managed-gte-qwen2:release-identity-other" }
            , firstAttempt { attempts = firstAttempt.attempts + 1 }
            ]
      mapM (\job -> releaseEmbeddingJob env.pool job "provider_cancelled" True) mismatches
        `shouldReturn` replicate (length mismatches) False
      releaseEmbeddingJob env.pool nullOwnerAttempt "provider_cancelled" True `shouldReturn` True
      runSession env.pool (Session.statement nullOwnerObservation.id jobStateAttemptsOwnerStatement)
        `shouldReturn` ("pending", 1, Nothing)
      releaseEmbeddingJob env.pool nullOwnerAttempt "provider_cancelled" True `shouldReturn` False
      releaseEmbeddingJob env.pool firstAttempt "provider_cancelled" True `shouldReturn` True
      releaseEmbeddingJob env.pool firstAttempt "provider_cancelled" True `shouldReturn` False
      runSession env.pool (Session.statement secondObservation.id jobStateStatement) `shouldReturn` "leased"
      releaseEmbeddingJob env.pool secondAttempt "provider_cancelled" True `shouldReturn` True

    it "preserves matching release success and retry-ceiling semantics" $ \env -> do
      requirePgvector env
      workspace <- createTestWorkspace env "embedding-release-positive"
      enableEmbeddingTarget env.pool (testSpace "hmem:managed-gte-qwen2:release-positive")
      ordinaryObservation <- create env workspace "ordinary release"
      ceilingObservation <- create env workspace "ceiling release"
      [ordinaryAttempt] <- claimEmbeddingJobs env.pool "ordinary-release" 1
      releaseEmbeddingJob env.pool ordinaryAttempt "provider_unavailable" True `shouldReturn` True
      runSession env.pool (Session.statement ordinaryObservation.id jobStateAttemptsOwnerStatement)
        `shouldReturn` ("pending", 1, Nothing)
      [ceilingClaim] <- claimEmbeddingJobs env.pool "ceiling-release" 1
      ceilingClaim.observationId `shouldBe` ceilingObservation.id
      runSession env.pool (Session.statement ceilingObservation.id setAttemptCeilingStatement)
      let ceilingAttempt = ceilingClaim { attempts = 16 }
      releaseEmbeddingJob env.pool ceilingAttempt "provider_cancelled" True `shouldReturn` True
      runSession env.pool (Session.statement ceilingObservation.id jobStateAttemptsOwnerStatement)
        `shouldReturn` ("pending", 15, Nothing)

    it "rechecks a matching release after replacement races its Observation lock" $ \env -> do
      requirePgvector env
      workspace <- createTestWorkspace env "embedding-release-positive-race"
      enableEmbeddingTarget env.pool (testSpace "hmem:managed-gte-qwen2:release-positive-race")
      observation <- create env workspace "positive release race"
      withEmbeddingTestPool env $ \lockPool ->
        withEmbeddingTestPool env $ \releasePool ->
          withEmbeddingTestPool env $ \reclaimPool ->
            withEmbeddingTestPool env $ \observerPool -> do
              mapM_ establishTestPool [lockPool, releasePool, reclaimPool, observerPool]
              [oldAttempt] <- claimEmbeddingJobsWithLease releasePool "release-race-old" 1 60
              locked <- newEmptyMVar
              unlock <- newEmptyMVar
              withAsync (holdObservationLock lockPool observation.id locked unlock) $ \holder ->
                (`finally` (void (tryPutMVar unlock ()) >> void (waitCatch holder))) $ do
                  holderPid <- timeout 5000000 (takeMVar locked) >>= \case
                    Just pid -> pure pid
                    Nothing -> expectationFailure "timed out acquiring positive-release Observation lock" >> fail "unreachable"
                  withAsync (releaseEmbeddingJob releasePool oldAttempt "provider_cancelled" True) $ \releaseTask -> do
                    waitForBlockedClaimMutation observerPool holderPid oldAttempt.leaseExpiresAt
                    expireLease reclaimPool observation.id
                    [replacement] <- claimEmbeddingJobsWithLease reclaimPool "release-race-new" 1 60
                    replacement.leaseOwner `shouldNotBe` oldAttempt.leaseOwner
                    replacementSnapshot <- runSession observerPool (Session.statement observation.id jobAttemptSnapshotStatement)
                    putMVar unlock ()
                    wait holder
                    timeout 5000000 (wait releaseTask) `shouldReturn` Just False
                    runSession observerPool (Session.statement observation.id jobAttemptSnapshotStatement)
                      `shouldReturn` replacementSnapshot
                    runSession observerPool (Session.statement observation.id observationVectorPresentStatement)
                      `shouldReturn` False

    it "rejects renewal and release after content mutation while preserving replacement ownership" $ \env -> do
      requirePgvector env
      workspace <- createTestWorkspace env "embedding-renew-content-change"
      let managedSpace = testSpace "hmem:managed-gte-qwen2:renew-content"
      enableEmbeddingTarget env.pool managedSpace
      observation <- create env workspace "before mutation"
      [oldAttempt] <- claimEmbeddingJobs env.pool "content-old" 1
      _ <- updateObservation env.pool workspace.id observation.id (UpdateObservation "after mutation")
      renewEmbeddingJob env.pool oldAttempt `shouldReturn` False
      releaseEmbeddingJob env.pool oldAttempt "provider_cancelled" True `shouldReturn` False
      completeClaimedEmbeddingJob env.pool oldAttempt unitX `shouldReturn` EmbeddingStale
      [replacement] <- claimEmbeddingJobs env.pool "content-new" 1
      replacement.content `shouldBe` "after mutation"
      replacement.leaseOwner `shouldNotBe` oldAttempt.leaseOwner
      renewEmbeddingJob env.pool replacement `shouldReturn` True
      observationSpace env.pool observation.id `shouldReturn` Nothing

    it "rejects renewal and release after deletion without recreating work" $ \env -> do
      requirePgvector env
      workspace <- createTestWorkspace env "embedding-renew-delete"
      enableEmbeddingTarget env.pool (testSpace "hmem:managed-gte-qwen2:renew-delete")
      observation <- create env workspace "delete while leased"
      [oldAttempt] <- claimEmbeddingJobs env.pool "delete-old" 1
      deleteObservation env.pool workspace.id observation.id `shouldReturn` True
      renewEmbeddingJob env.pool oldAttempt `shouldReturn` False
      releaseEmbeddingJob env.pool oldAttempt "provider_cancelled" True `shouldReturn` False
      completeClaimedEmbeddingJob env.pool oldAttempt unitX `shouldReturn` EmbeddingNotFound
      claimEmbeddingJobs env.pool "delete-new" 1 `shouldReturn` []

    it "rejects renewal and release while the target is disabled" $ \env -> do
      requirePgvector env
      workspace <- createTestWorkspace env "embedding-renew-disable"
      enableEmbeddingTarget env.pool (testSpace "hmem:managed-gte-qwen2:renew-disable")
      observation <- create env workspace "disable while leased"
      [oldAttempt] <- claimEmbeddingJobs env.pool "disable-old" 1
      disableEmbeddingTarget env.pool
      renewEmbeddingJob env.pool oldAttempt `shouldReturn` False
      releaseEmbeddingJob env.pool oldAttempt "provider_cancelled" True `shouldReturn` False
      completeClaimedEmbeddingJob env.pool oldAttempt unitX `shouldReturn` EmbeddingStale
      runSession env.pool (Session.statement observation.id jobStateStatement) `shouldReturn` "leased"
      observationSpace env.pool observation.id `shouldReturn` Nothing

    it "rejects old callbacks after a target switch and preserves the new-space attempt" $ \env -> do
      requirePgvector env
      workspace <- createTestWorkspace env "embedding-renew-switch"
      let oldSpace = testSpace "hmem:managed-gte-qwen2:renew-old-space"
          newSpace = testSpace "hmem:managed-gte-qwen2:renew-new-space"
      enableEmbeddingTarget env.pool oldSpace
      observation <- create env workspace "switch while leased"
      [oldAttempt] <- claimEmbeddingJobs env.pool "switch-old" 1
      enableEmbeddingTarget env.pool newSpace
      renewEmbeddingJob env.pool oldAttempt `shouldReturn` False
      releaseEmbeddingJob env.pool oldAttempt "provider_cancelled" True `shouldReturn` False
      completeClaimedEmbeddingJob env.pool oldAttempt unitX `shouldReturn` EmbeddingStale
      _ <- reconcileEmbeddingJobs env.pool 1
      [replacement] <- claimEmbeddingJobs env.pool "switch-new" 1
      replacement.spaceFingerprint `shouldBe` newSpace
      replacement.leaseOwner `shouldNotBe` oldAttempt.leaseOwner
      renewEmbeddingJob env.pool replacement `shouldReturn` True
      observationSpace env.pool observation.id `shouldReturn` Nothing

    it "fences partial batch ownership loss without affecting surviving attempts" $ \env -> do
      requirePgvector env
      workspace <- createTestWorkspace env "embedding-partial-ownership"
      let managedSpace = testSpace "hmem:managed-gte-qwen2:partial"
      enableEmbeddingTarget env.pool managedSpace
      firstObservation <- create env workspace "first"
      secondObservation <- create env workspace "second"
      attempts <- claimEmbeddingJobsWithLease env.pool "logical-worker" 2 60
      attempts `shouldSatisfy` ((== 2) . length)
      firstAttempt <- requireOnly "first claimed attempt" [job | job <- attempts, job.observationId == firstObservation.id]
      secondAttempt <- requireOnly "second claimed attempt" [job | job <- attempts, job.observationId == secondObservation.id]
      expireLease env.pool firstObservation.id
      [replacement] <- claimEmbeddingJobs env.pool "replacement-worker" 1
      replacement.observationId `shouldBe` firstObservation.id
      renewEmbeddingJob env.pool firstAttempt `shouldReturn` False
      releaseEmbeddingJob env.pool firstAttempt "provider_cancelled" True `shouldReturn` False
      renewEmbeddingJob env.pool secondAttempt `shouldReturn` True
      completeClaimedEmbeddingJob env.pool secondAttempt unitX `shouldReturn` EmbeddingApplied
      renewEmbeddingJob env.pool replacement `shouldReturn` True

    it "evaluates renewal, completion, and release expiry after a contended Observation lock" $ \env -> do
      requirePgvector env
      workspace <- createTestWorkspace env "embedding-post-lock-clock"
      let managedSpace = testSpace "hmem:managed-gte-qwen2:post-lock-clock"
      enableEmbeddingTarget env.pool managedSpace
      observation <- create env workspace "lock contention"
      withEmbeddingTestPool env $ \lockPool ->
        withEmbeddingTestPool env $ \actionPool ->
          withEmbeddingTestPool env $ \observerPool -> do
            mapM_ establishTestPool [lockPool, actionPool, observerPool]
            ((renewAttempt, renewBefore), renewResult) <-
              runAfterLeaseExpiresBehindObservationLock lockPool observerPool observation.id $ do
                [attempt] <- claimEmbeddingJobsWithLease actionPool "clock-worker" 1 1
                before <- runSession observerPool (Session.statement observation.id jobAttemptSnapshotStatement)
                pure (attempt.leaseExpiresAt, (attempt, before), renewEmbeddingJobFor actionPool 60 attempt)
            renewResult `shouldBe` False
            runSession observerPool (Session.statement observation.id jobAttemptSnapshotStatement)
              `shouldReturn` renewBefore
            ((completionAttempt, completionBefore), completionResult) <-
              runAfterLeaseExpiresBehindObservationLock lockPool observerPool observation.id $ do
                [attempt] <- claimEmbeddingJobsWithLease actionPool "clock-worker" 1 1
                before <- runSession observerPool (Session.statement observation.id jobAttemptSnapshotStatement)
                pure (attempt.leaseExpiresAt, (attempt, before), completeClaimedEmbeddingJob actionPool attempt unitX)
            completionResult `shouldBe` EmbeddingStale
            runSession observerPool (Session.statement observation.id jobAttemptSnapshotStatement)
              `shouldReturn` completionBefore
            completionAttempt.leaseOwner `shouldNotBe` renewAttempt.leaseOwner
            ((releaseAttempt, releaseBefore), releaseResult) <-
              runAfterLeaseExpiresBehindObservationLock lockPool observerPool observation.id $ do
                [attempt] <- claimEmbeddingJobsWithLease actionPool "clock-worker" 1 1
                before <- runSession observerPool (Session.statement observation.id jobAttemptSnapshotStatement)
                pure (attempt.leaseExpiresAt, (attempt, before), releaseEmbeddingJob actionPool attempt "provider_cancelled" True)
            releaseResult `shouldBe` False
            runSession observerPool (Session.statement observation.id jobAttemptSnapshotStatement)
              `shouldReturn` releaseBefore
            releaseAttempt.leaseOwner `shouldNotBe` completionAttempt.leaseOwner
            runSession observerPool (Session.statement observation.id observationVectorPresentStatement)
              `shouldReturn` False

    it "applies server timeouts on the production renewal connection before locking" $ \env -> do
      requirePgvector env
      workspace <- createTestWorkspace env "embedding-renewal-timeout-proof"
      enableEmbeddingTarget env.pool (testSpace "hmem:managed-gte-qwen2:timeout-proof")
      observation <- create env workspace "held renewal"
      let marker = T.take 8 (T.pack (show observation.id))
          probe = void $ Session.statement marker renewalTimeoutProbeStatement
      withEmbeddingTestPool env $ \actionPool ->
        withEmbeddingTestPool env $ \lockPool ->
          withEmbeddingTestPool env $ \observerPool -> do
            mapM_ establishTestPool [actionPool, lockPool, observerPool]
            [claimed] <- claimEmbeddingJobs actionPool "timeout-proof" 1
            startRenewal <- newEmptyMVar
            withAsync
              (takeMVar startRenewal >> (try (renewEmbeddingJobForWithSessionProbeForTest actionPool 60 probe claimed) :: IO (Either DBException Bool)))
              $ \renewalTask -> do
                locked <- newEmptyMVar
                unlock <- newEmptyMVar
                withAsync (holdObservationLock lockPool observation.id locked unlock) $ \holder ->
                  (`finally` (void (tryPutMVar unlock ()) >> void (waitCatch holder))) $ do
                    holderPid <- timeout 5000000 (takeMVar locked) >>= \case
                      Just value -> pure value
                      Nothing -> expectationFailure "timed out acquiring the renewal timeout proof lock" >> fail "unreachable"
                    started <- getMonotonicTimeNSec
                    putMVar startRenewal ()
                    activity <- waitForRenewalActivity observerPool marker holderPid
                    activity.sessionPid `shouldBe` activity.activityPid
                    activity.lockTimeout `shouldBe` "5s"
                    activity.statementTimeout `shouldBe` "10s"
                    activity.waitEventType `shouldBe` "Lock"
                    activity.query `shouldSatisfy` T.isInfixOf "FROM public.observations o"
                    activity.query `shouldSatisfy` T.isInfixOf "FOR UPDATE OF o"
                    activity.queryStartedAt `shouldSatisfy` (>= activity.transactionStartedAt)
                    correlated <- waitForRenewalActivity observerPool marker holderPid
                    correlated.activityPid `shouldBe` activity.activityPid
                    correlated.queryStartedAt `shouldBe` activity.queryStartedAt
                    correlated.transactionStartedAt `shouldBe` activity.transactionStartedAt
                    outcome <- timeout 11500000 (wait renewalTask)
                    finished <- getMonotonicTimeNSec
                    outcome `shouldSatisfy` \case
                      Just (Left (DBOtherError message)) -> "lock timeout" `T.isInfixOf` T.toLower message
                      _ -> False
                    let elapsedSeconds = fromIntegral (finished - started) / 1000000000 :: Double
                    putStrLn ("[renewal-timeout-proof] lock_timeout=" <> T.unpack activity.lockTimeout
                      <> "; statement_timeout=" <> T.unpack activity.statementTimeout
                      <> "; pid=" <> show activity.activityPid
                      <> "; operation_seconds=" <> show elapsedSeconds)
                    elapsedSeconds `shouldSatisfy` \elapsed -> elapsed >= 4.5 && elapsed < 11.5
                    waitUntil "timed-out renewal transaction cleanup" $
                      null <$> runSession observerPool (Session.statement (marker, holderPid) renewalActivityStatement)
                    runSession observerPool (Session.statement observation.id jobStateStatement) `shouldReturn` "leased"

    it "enforces a native server lock timeout while the same row remains held" $ \env -> do
      workspace <- createTestWorkspace env "embedding-native-lock-timeout-control"
      observation <- create env workspace "held native control"
      withEmbeddingTestPool env $ \lockPool ->
        withEmbeddingTestPool env $ \controlPool -> do
          mapM_ establishTestPool [lockPool, controlPool]
          locked <- newEmptyMVar
          unlock <- newEmptyMVar
          withAsync (holdObservationLock lockPool observation.id locked unlock) $ \holder ->
            (`finally` (void (tryPutMVar unlock ()) >> void (waitCatch holder))) $ do
              _ <- timeout 5000000 (takeMVar locked) >>= \case
                Just value -> pure value
                Nothing -> expectationFailure "timed out acquiring the native timeout control lock" >> fail "unreachable"
              withAsync
                (Pool.withResource controlPool $ \connection -> do
                  totalStarted <- getMonotonicTimeNSec
                  settings <- runDirect connection $ do
                    Session.sql "BEGIN"
                    Session.sql "SET LOCAL lock_timeout = '1s'"
                    Session.sql "SET LOCAL statement_timeout = '3s'"
                    Session.statement () timeoutSettingsStatement
                  started <- getMonotonicTimeNSec
                  result <- Session.run (Session.statement observation.id lockObservationStatement) connection
                  operationFinished <- getMonotonicTimeNSec
                  cleanupResult <- Session.run (Session.sql "ROLLBACK") connection
                  cleanupFinished <- getMonotonicTimeNSec
                  pure
                    ( settings, result, cleanupResult
                    , secondsBetween started operationFinished
                    , secondsBetween operationFinished cleanupFinished
                    , secondsBetween totalStarted cleanupFinished
                    ))
                $ \controlTask -> do
                  boundedOutcome <- timeout 5000000 (waitCatch controlTask)
                  case boundedOutcome of
                    Just (Right (settings, outcome, cleanupResult, elapsedSeconds, cleanupSeconds, totalSeconds)) -> do
                      settings.lockTimeoutSetting `shouldBe` "1s"
                      settings.statementTimeoutSetting `shouldBe` "3s"
                      outcome `shouldSatisfy` \case
                        Left err -> isServerError "55P03" "lock timeout" err
                        Right () -> False
                      cleanupResult `shouldSatisfy` \case Right () -> True; Left _ -> False
                      putStrLn ("[lock-timeout-control] lock_timeout=" <> T.unpack settings.lockTimeoutSetting
                        <> "; statement_timeout=" <> T.unpack settings.statementTimeoutSetting
                        <> "; pid=" <> show settings.backendPid
                        <> "; operation_seconds=" <> show elapsedSeconds
                        <> "; cleanup_seconds=" <> show cleanupSeconds
                        <> "; total_seconds=" <> show totalSeconds)
                      elapsedSeconds `shouldSatisfy` \elapsed -> elapsed >= 0.8 && elapsed < 4.5
                    Just (Left exceptionValue) -> expectationFailure
                      ("native timeout control threw: " <> show exceptionValue)
                    Nothing -> do
                      -- Diagnostic teardown only: a broken server timer must
                      -- not strand the helper or inject cancellation into it.
                      void $ tryPutMVar unlock ()
                      void $ waitCatch holder
                      eventual <- timeout 5000000 (waitCatch controlTask)
                      expectationFailure
                        ("native statement timeout did not return in 5s; after releasing the holder: " <> show eventual)

    it "enforces a native server statement timeout during pg_sleep" $ \env -> do
      withEmbeddingTestPool env $ \controlPool -> do
        establishTestPool controlPool
        (settings, outcome, cleanupResult, elapsedSeconds, cleanupSeconds, totalSeconds) <-
          Pool.withResource controlPool $ \connection -> do
            totalStarted <- getMonotonicTimeNSec
            configured <- runDirect connection $ do
              Session.sql "BEGIN"
              Session.sql "SET LOCAL statement_timeout = '1s'"
              Session.statement () timeoutSettingsStatement
            started <- getMonotonicTimeNSec
            result <- Session.run (Session.statement () statementTimeoutSleepStatement) connection
            operationFinished <- getMonotonicTimeNSec
            cleaned <- Session.run (Session.sql "ROLLBACK") connection
            cleanupFinished <- getMonotonicTimeNSec
            pure
              ( configured, result, cleaned
              , secondsBetween started operationFinished
              , secondsBetween operationFinished cleanupFinished
              , secondsBetween totalStarted cleanupFinished
              )
        settings.statementTimeoutSetting `shouldBe` "1s"
        outcome `shouldSatisfy` \case
          Left err -> isServerError "57014" "statement timeout" err
          Right _ -> False
        cleanupResult `shouldSatisfy` \case Right () -> True; Left _ -> False
        putStrLn ("[statement-timeout-control] statement_timeout=" <> T.unpack settings.statementTimeoutSetting
          <> "; pid=" <> show settings.backendPid
          <> "; operation_seconds=" <> show elapsedSeconds
          <> "; cleanup_seconds=" <> show cleanupSeconds
          <> "; total_seconds=" <> show totalSeconds)
        elapsedSeconds `shouldSatisfy` \elapsed -> elapsed >= 0.8 && elapsed < 3

    it "settles an expired final-attempt lease without reclaiming it" $ \env -> do
      requirePgvector env
      workspace <- createTestWorkspace env "embedding-exhausted-lease"
      enableEmbeddingTarget env.pool (testSpace "hmem:managed-gte-qwen2:exhausted")
      observation <- create env workspace "one"
      _ <- claimEmbeddingJobs env.pool "first" 1
      runSession env.pool $ Session.statement observation.id exhaustLeaseStatement
      claimEmbeddingJobs env.pool "second" 1 `shouldReturn` []
      runSession env.pool (Session.statement observation.id jobStateStatement) `shouldReturn` "failed"
  where
    create env workspace body = createObservation env.pool CreateObservation
      { workspaceId = workspace.id
      , subjects = [ObservationSubject SubjectFile "src/Embedding.hs"]
      , gitSha = "0123456789abcdef0123456789abcdef01234567"
      , content = body
      }
    requirePgvector env = do
      present <- checkPgvector env.pool
      unless present $ pendingWith "sandbox PostgreSQL does not expose pgvector"
    unitX = 1 : replicate (observationEmbeddingDimensions - 1) 0

expireLease :: Pool.Pool Hasql.Connection -> UUID -> IO ()
expireLease pool observation = runSession pool $ Session.statement observation expireLeaseStatement

expireLeaseStatement :: Statement.Statement UUID ()
expireLeaseStatement = Statement.Statement
  "UPDATE public.embedding_jobs SET lease_expires_at = now() - interval '1 second' WHERE observation_id = $1"
  (Enc.param (Enc.nonNullable Enc.uuid)) Dec.noResult True

exhaustLeaseStatement :: Statement.Statement UUID ()
exhaustLeaseStatement = Statement.Statement
  "UPDATE public.embedding_jobs SET attempts = 16, lease_expires_at = now() - interval '1 second' WHERE observation_id = $1"
  (Enc.param (Enc.nonNullable Enc.uuid)) Dec.noResult True

jobStateStatement :: Statement.Statement UUID Text
jobStateStatement = Statement.Statement
  "SELECT state FROM public.embedding_jobs WHERE observation_id = $1"
  (Enc.param (Enc.nonNullable Enc.uuid)) (Dec.singleRow (Dec.column (Dec.nonNullable Dec.text))) True

jobExistsStatement :: Statement.Statement UUID Bool
jobExistsStatement = Statement.Statement
  "SELECT EXISTS (SELECT 1 FROM public.embedding_jobs WHERE observation_id = $1)"
  (Enc.param (Enc.nonNullable Enc.uuid)) (Dec.singleRow (Dec.column (Dec.nonNullable Dec.bool))) True

clearVectorAndStaleJobStatement :: Statement.Statement UUID ()
clearVectorAndStaleJobStatement = Statement.Statement
  "WITH cleared AS (UPDATE public.observations SET embedding = NULL, embedding_space_fingerprint = NULL WHERE id = $1) UPDATE public.embedding_jobs SET content_fingerprint = '0000000000000000000000000000000000000000000000000000000000000000', state = 'complete', lease_owner = NULL, lease_expires_at = NULL WHERE observation_id = $1"
  (Enc.param (Enc.nonNullable Enc.uuid)) Dec.noResult True

clearVectorStatement :: Statement.Statement UUID ()
clearVectorStatement = Statement.Statement
  "UPDATE public.observations SET embedding = NULL, embedding_space_fingerprint = NULL WHERE id = $1"
  (Enc.param (Enc.nonNullable Enc.uuid)) Dec.noResult True

setVectorWithoutSettlingStatement :: Statement.Statement (UUID, UUID, Text, Text) ()
setVectorWithoutSettlingStatement = Statement.Statement
  "UPDATE public.observations SET embedding = $3::vector, embedding_space_fingerprint = $4 WHERE workspace_id = $1 AND id = $2"
  (contramap (\(a,_,_,_) -> a) (Enc.param (Enc.nonNullable Enc.uuid)) <> contramap (\(_,b,_,_) -> b) (Enc.param (Enc.nonNullable Enc.uuid)) <> contramap (\(_,_,c,_) -> c) (Enc.param (Enc.nonNullable Enc.text)) <> contramap (\(_,_,_,d) -> d) (Enc.param (Enc.nonNullable Enc.text))) Dec.noResult True

unitVectorText :: Text
unitVectorText = "[" <> T.intercalate "," (map (T.pack . show) unitX) <> "]"
  where
    unitX = 1 : replicate (observationEmbeddingDimensions - 1) (0 :: Double)

testSpace :: Text -> EmbeddingSpaceFingerprint
testSpace raw = maybe (error "test embedding space must be valid") id (parseEmbeddingSpaceFingerprint raw)

historicalManagedFingerprint :: Text
historicalManagedFingerprint =
  "Alibaba-NLP/gte-Qwen2-1.5B-instruct@1cad2ab3ff41c2671f34e135d29831368ee26b68:1536"

similarQueryFor :: UUID -> EmbeddingSpaceFingerprint -> SimilarObservationQuery
similarQueryFor workspace space = SimilarObservationQuery
  { workspaceId = workspace, subjectKind = Nothing, subject = Nothing, gitSha = Nothing
  , embedding = 1 : replicate (observationEmbeddingDimensions - 1) 0
  , spaceFingerprint = Just space, minSimilarity = Just 1
  , limit = Nothing, offset = Nothing
  }

observationSpace :: Pool.Pool Hasql.Connection -> UUID -> IO (Maybe Text)
observationSpace pool observation = runSession pool $ Session.statement observation observationSpaceStatement

observationSpaceStatement :: Statement.Statement UUID (Maybe Text)
observationSpaceStatement = Statement.Statement
  "SELECT embedding_space_fingerprint FROM public.observations WHERE id = $1"
  (Enc.param (Enc.nonNullable Enc.uuid))
  (Dec.singleRow (Dec.column (Dec.nullable Dec.text))) True

runAfterLeaseExpiresBehindObservationLock
  :: Pool.Pool Hasql.Connection
  -> Pool.Pool Hasql.Connection
  -> UUID
  -> IO (UTCTime, evidence, IO a)
  -> IO (evidence, a)
runAfterLeaseExpiresBehindObservationLock lockPool observerPool observation prepare = do
  locked <- newEmptyMVar
  unlock <- newEmptyMVar
  withAsync (holdObservationLock lockPool observation locked unlock) $ \holder ->
    (`finally` void (tryPutMVar unlock ())) $ do
      holderReady <- timeout 5000000 $ race (waitCatch holder) (takeMVar locked)
      holderPid <- case holderReady of
        Just (Right pid) -> pure pid
        Just (Left (Left err)) -> throwIO err
        Just (Left (Right ())) -> expectationFailure "lock holder exited before publishing its backend" >> fail "unreachable"
        Nothing -> expectationFailure "timed out acquiring the Observation test lock" >> fail "unreachable"
      (leaseExpiry, evidence, action) <- prepare
      withAsync action $ \resultTask -> do
        -- Prove the production transaction started before expiry and is waiting
        -- on this exact holder. Releasing only after the database clock crosses
        -- expiry makes transaction-start now() accept incorrectly and the
        -- required post-lock clock reject deterministically.
        waitForBlockedClaimMutation observerPool holderPid leaseExpiry
        waitForDbClockAfter observerPool leaseExpiry
        putMVar unlock ()
        wait holder
        result <- timeout 5000000 (wait resultTask)
        case result of
          Just value -> pure (evidence, value)
          Nothing -> expectationFailure "contended claim mutation exceeded its bounded timeout" >> fail "unreachable"

holdObservationLock
  :: Pool.Pool Hasql.Connection -> UUID -> MVar Int32 -> MVar () -> IO ()
holdObservationLock pool observation locked unlock =
  Pool.withResource pool $ \connection ->
    (do
      runDirect connection (Session.sql "BEGIN")
      runDirect connection (Session.statement observation lockObservationStatement)
      backendPid <- runDirect connection (Session.statement () backendPidStatement)
      putMVar locked backendPid
      takeMVar unlock)
    `finally` void (Session.run (Session.sql "ROLLBACK") connection)

waitForBlockedClaimMutation :: Pool.Pool Hasql.Connection -> Int32 -> UTCTime -> IO ()
waitForBlockedClaimMutation pool holderPid leaseExpiry = waitUntil "claim mutation lock wait" $ do
  runSession pool $ Session.statement (holderPid, leaseExpiry) claimMutationWaitingStatement

waitForDbClockAfter :: Pool.Pool Hasql.Connection -> UTCTime -> IO ()
waitForDbClockAfter pool leaseExpiry = waitUntil "lease expiry" $ do
  runSession pool $ Session.statement leaseExpiry dbClockAfterStatement

waitUntil :: String -> IO Bool -> IO ()
waitUntil label condition = do
  result <- timeout 5000000 go
  case result of
    Just () -> pure ()
    Nothing -> expectationFailure ("timed out waiting for " <> label) >> fail "unreachable"
  where
    go = condition >>= \case
      True -> pure ()
      False -> threadDelay 10000 >> go

runDirect :: Hasql.Connection -> Session.Session a -> IO a
runDirect connection session = Session.run session connection >>= \case
  Right value -> pure value
  Left err -> expectationFailure (show err) >> fail "direct test session failed"

withEmbeddingTestPool :: TestEnv -> (Pool.Pool Hasql.Connection -> IO a) -> IO a
withEmbeddingTestPool env =
  bracket
    (createPool env.testDb.testDbConnStr 1 5 30000)
    Pool.destroyAllResources

establishTestPool :: Pool.Pool Hasql.Connection -> IO ()
establishTestPool pool =
  Pool.withResource pool $ \connection -> void (runDirect connection (Session.statement () backendPidStatement))

secondsBetween :: Word64 -> Word64 -> Double
secondsBetween started finished = fromIntegral (finished - started) / 1000000000

data TimeoutSettings = TimeoutSettings
  { lockTimeoutSetting :: !Text
  , statementTimeoutSetting :: !Text
  , backendPid :: !Int32
  } deriving stock (Show, Eq)

timeoutSettingsStatement :: Statement.Statement () TimeoutSettings
timeoutSettingsStatement = Statement.Statement
  "SELECT current_setting('lock_timeout'), current_setting('statement_timeout'), pg_backend_pid()"
  Enc.noParams
  (Dec.singleRow (TimeoutSettings
    <$> Dec.column (Dec.nonNullable Dec.text)
    <*> Dec.column (Dec.nonNullable Dec.text)
    <*> Dec.column (Dec.nonNullable Dec.int4))) True

isServerError :: ByteString -> ByteString -> Session.SessionError -> Bool
isServerError expectedState expectedMessage = \case
  Session.QueryError _ _ (Session.ResultError (Session.ServerError sqlstate message _ _ _)) ->
    sqlstate == expectedState && expectedMessage `B8.isInfixOf` B8.map toLower message
  Session.PipelineError (Session.ResultError (Session.ServerError sqlstate message _ _ _)) ->
    sqlstate == expectedState && expectedMessage `B8.isInfixOf` B8.map toLower message
  _ -> False

data JobAttemptSnapshot = JobAttemptSnapshot
  { snapshotWorkspace :: !UUID
  , snapshotContentFingerprint :: !Text
  , snapshotSpaceFingerprint :: !Text
  , snapshotState :: !Text
  , snapshotLeaseOwner :: !(Maybe Text)
  , snapshotLeaseExpiry :: !(Maybe UTCTime)
  , snapshotAttempts :: !Int32
  } deriving stock (Show, Eq)

jobAttemptSnapshotStatement :: Statement.Statement UUID JobAttemptSnapshot
jobAttemptSnapshotStatement = Statement.Statement
  "SELECT workspace_id, content_fingerprint, space_fingerprint, state, lease_owner, lease_expires_at, attempts FROM public.embedding_jobs WHERE observation_id = $1"
  (Enc.param (Enc.nonNullable Enc.uuid))
  (Dec.singleRow (JobAttemptSnapshot
    <$> Dec.column (Dec.nonNullable Dec.uuid)
    <*> Dec.column (Dec.nonNullable Dec.text)
    <*> Dec.column (Dec.nonNullable Dec.text)
    <*> Dec.column (Dec.nonNullable Dec.text)
    <*> Dec.column (Dec.nullable Dec.text)
    <*> Dec.column (Dec.nullable Dec.timestamptz)
    <*> Dec.column (Dec.nonNullable Dec.int4))) True

jobStateAttemptsOwnerStatement :: Statement.Statement UUID (Text, Int, Maybe Text)
jobStateAttemptsOwnerStatement = Statement.Statement
  "SELECT state, attempts, lease_owner FROM public.embedding_jobs WHERE observation_id = $1"
  (Enc.param (Enc.nonNullable Enc.uuid))
  (Dec.singleRow ((,,)
    <$> Dec.column (Dec.nonNullable Dec.text)
    <*> (fromIntegral <$> Dec.column (Dec.nonNullable Dec.int4))
    <*> Dec.column (Dec.nullable Dec.text))) True

setAttemptCeilingStatement :: Statement.Statement UUID ()
setAttemptCeilingStatement = Statement.Statement
  "UPDATE public.embedding_jobs SET attempts = 16 WHERE observation_id = $1"
  (Enc.param (Enc.nonNullable Enc.uuid)) Dec.noResult True

blockedByHolderCountStatement :: Statement.Statement Int32 Int64
blockedByHolderCountStatement = Statement.Statement
  "SELECT count(*) FROM pg_stat_activity activity WHERE $1 = ANY(pg_blocking_pids(activity.pid))"
  (Enc.param (Enc.nonNullable Enc.int4))
  (Dec.singleRow (Dec.column (Dec.nonNullable Dec.int8))) True

observationVectorPresentStatement :: Statement.Statement UUID Bool
observationVectorPresentStatement = Statement.Statement
  "SELECT embedding IS NOT NULL FROM public.observations WHERE id = $1"
  (Enc.param (Enc.nonNullable Enc.uuid))
  (Dec.singleRow (Dec.column (Dec.nonNullable Dec.bool))) True

data RenewalActivity = RenewalActivity
  { activityPid :: !Int32
  , sessionPid :: !Int32
  , lockTimeout :: !Text
  , statementTimeout :: !Text
  , queryStartedAt :: !UTCTime
  , transactionStartedAt :: !UTCTime
  , query :: !Text
  , waitEventType :: !Text
  } deriving stock (Show, Eq)

waitForRenewalActivity
  :: Pool.Pool Hasql.Connection -> Text -> Int32 -> IO RenewalActivity
waitForRenewalActivity pool marker holderPid = do
  result <- timeout 5000000 poll
  case result of
    Just activity -> pure activity
    Nothing -> expectationFailure "timed out observing the production renewal timeout settings" >> fail "unreachable"
  where
    poll = runSession pool (Session.statement (marker, holderPid) renewalActivityStatement) >>= \case
      [activity] -> pure activity
      _ -> threadDelay 10000 >> poll

lockObservationStatement :: Statement.Statement UUID ()
lockObservationStatement = Statement.Statement
  "SELECT id FROM public.observations WHERE id = $1 FOR UPDATE"
  (Enc.param (Enc.nonNullable Enc.uuid)) (void (Dec.singleRow (Dec.column (Dec.nonNullable Dec.uuid)))) True

backendPidStatement :: Statement.Statement () Int32
backendPidStatement = Statement.Statement
  "SELECT pg_backend_pid()"
  Enc.noParams (Dec.singleRow (Dec.column (Dec.nonNullable Dec.int4))) True

renewalTimeoutProbeStatement :: Statement.Statement Text Text
renewalTimeoutProbeStatement = Statement.Statement
  "SELECT set_config('application_name', 'hmem-renew-proof:' || $1 || ':' || pg_backend_pid()::text || ':' || current_setting('lock_timeout') || ':' || current_setting('statement_timeout'), TRUE)"
  (Enc.param (Enc.nonNullable Enc.text))
  (Dec.singleRow (Dec.column (Dec.nonNullable Dec.text))) False

renewalActivityStatement :: Statement.Statement (Text, Int32) [RenewalActivity]
renewalActivityStatement = Statement.Statement
  "SELECT activity.pid, split_part(activity.application_name, ':', 3)::int4, split_part(activity.application_name, ':', 4), split_part(activity.application_name, ':', 5), activity.query_start, activity.xact_start, activity.query, coalesce(activity.wait_event_type, '') FROM pg_stat_activity activity WHERE activity.application_name LIKE 'hmem-renew-proof:' || $1 || ':%' AND $2 = ANY(pg_blocking_pids(activity.pid)) AND activity.wait_event_type = 'Lock' ORDER BY activity.pid"
  (contramap fst (Enc.param (Enc.nonNullable Enc.text)) <> contramap snd (Enc.param (Enc.nonNullable Enc.int4)))
  (Dec.rowList (RenewalActivity
    <$> Dec.column (Dec.nonNullable Dec.int4)
    <*> Dec.column (Dec.nonNullable Dec.int4)
    <*> Dec.column (Dec.nonNullable Dec.text)
    <*> Dec.column (Dec.nonNullable Dec.text)
    <*> Dec.column (Dec.nonNullable Dec.timestamptz)
    <*> Dec.column (Dec.nonNullable Dec.timestamptz)
    <*> Dec.column (Dec.nonNullable Dec.text)
    <*> Dec.column (Dec.nonNullable Dec.text))) True

statementTimeoutSleepStatement :: Statement.Statement () Text
statementTimeoutSleepStatement = Statement.Statement
  "WITH delayed AS MATERIALIZED (SELECT pg_sleep(5)) SELECT current_setting('statement_timeout') FROM delayed"
  Enc.noParams
  (Dec.singleRow (Dec.column (Dec.nonNullable Dec.text))) False

claimMutationWaitingStatement :: Statement.Statement (Int32, UTCTime) Bool
claimMutationWaitingStatement = Statement.Statement
  "SELECT EXISTS (SELECT 1 FROM pg_stat_activity activity WHERE $1 = ANY(pg_blocking_pids(activity.pid)) AND activity.wait_event_type = 'Lock' AND activity.xact_start < $2 AND activity.query LIKE '%FROM public.observations o%FOR UPDATE%')"
  (contramap fst (Enc.param (Enc.nonNullable Enc.int4)) <> contramap snd (Enc.param (Enc.nonNullable Enc.timestamptz)))
  (Dec.singleRow (Dec.column (Dec.nonNullable Dec.bool))) True

dbClockAfterStatement :: Statement.Statement UTCTime Bool
dbClockAfterStatement = Statement.Statement
  "SELECT clock_timestamp() > $1"
  (Enc.param (Enc.nonNullable Enc.timestamptz))
  (Dec.singleRow (Dec.column (Dec.nonNullable Dec.bool))) True

requireOnly :: String -> [a] -> IO a
requireOnly _ [value] = pure value
requireOnly label values = do
  expectationFailure (label <> ": expected one value, got " <> show (length values))
  fail "unreachable"
