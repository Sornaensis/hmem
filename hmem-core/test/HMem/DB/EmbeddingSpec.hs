module HMem.DB.EmbeddingSpec (spec) where

import Control.Concurrent.Async (concurrently)
import Control.Monad (unless)
import Data.Functor.Contravariant (contramap)
import Data.List (sortOn)
import Data.Pool qualified as Pool
import Data.Text (Text)
import Data.Text qualified as T
import Data.UUID (UUID)
import Hasql.Connection qualified as Hasql
import Hasql.Decoders qualified as Dec
import Hasql.Encoders qualified as Enc
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement
import Test.Hspec

import HMem.Config qualified as Config
import HMem.DB.Embedding
import HMem.DB.Observation
import HMem.DB.Pool (checkPgvector, runSession)
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
      completeClaimedEmbeddingJob env.pool "noncausal-worker" workspace.id historical.id
        (historicalClaim.contentFingerprint <> "-stale") newSpace unitX `shouldReturn` EmbeddingStale
      completeClaimedEmbeddingJob env.pool "noncausal-worker" workspace.id historical.id
        historicalClaim.contentFingerprint historicalSpace unitX `shouldReturn` EmbeddingStale
      completeClaimedEmbeddingJob env.pool "noncausal-worker" workspace.id historical.id
        historicalClaim.contentFingerprint newSpace unitX `shouldReturn` EmbeddingApplied

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
      completeClaimedEmbeddingJob env.pool "worker-one" workspace.id oldJob.id claimed.contentFingerprint managedSpace unitX
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
      completeClaimedEmbeddingJob env.pool "owner" workspace.id observation.id claimed.contentFingerprint managedSpace unitX
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
      completeClaimedEmbeddingJob env.pool "worker-one" workspace.id observation.id firstClaim.contentFingerprint firstSpace unitX
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
