-- | The single persistence boundary for observation vectors and durable
-- embedding work.  Transport adapters and workers intentionally do not own
-- vector UPDATE statements.
module HMem.DB.Embedding
  ( EmbeddingCasOutcome(..)
  , EmbeddingJob(..)
  , observationContentFingerprint
  , setObservationEmbeddingInSpace
  , compareAndSetObservationEmbedding
  , completeClaimedEmbeddingJob
  , enableEmbeddingTarget
  , disableEmbeddingTarget
  , enqueueObservationForActiveTarget
  , enqueueObservationForContentChange
  , reconcileEmbeddingJobs
  , claimEmbeddingJobs
  , releaseEmbeddingJob
  ) where

import Control.Exception (throwIO)
import Crypto.Hash (Digest, SHA256, hash)
import Data.Aeson qualified as Aeson
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as LBS
import Data.Functor.Contravariant (contramap)
import Data.Int (Int32)
import Data.Pool (Pool)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time (UTCTime)
import Data.UUID (UUID)
import Hasql.Connection qualified as Hasql
import Hasql.Decoders qualified as Dec
import Hasql.Encoders qualified as Enc
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement

import HMem.DB.Pool (DBException(..), runSession, runTransaction)
import HMem.Types

data EmbeddingCasOutcome = EmbeddingApplied | EmbeddingAlreadySatisfied | EmbeddingStale | EmbeddingNotFound
  deriving stock (Show, Eq)

data EmbeddingJob = EmbeddingJob
  { observationId :: !UUID
  , workspaceId :: !UUID
  , contentFingerprint :: !Text
  , spaceFingerprint :: !EmbeddingSpaceFingerprint
  , attempts :: !Int
  , leaseOwner :: !Text
  , leaseExpiresAt :: !UTCTime
  , content :: !Text
  } deriving stock (Show, Eq)

observationContentFingerprint :: Text -> [ObservationSubject] -> Text -> Text
observationContentFingerprint gitShaValue subjectValues contentValue =
  T.pack $ show (hash payload :: Digest SHA256)
  where
    canonicalSubjects = [(subjectKindToText value.subjectKind, value.subject) | value <- subjectValues]
    payload = LBS.toStrict $ Aeson.encode
      ("hmem-observation-embedding-v1" :: Text, gitShaValue, canonicalSubjects, contentValue)

validateEmbedding :: [Double] -> IO ()
validateEmbedding values
  | length values /= observationEmbeddingDimensions || any (\value -> isNaN value || isInfinite value) values =
      throwIO $ DBCheckViolation "embedding must contain exactly 1536 finite dimensions"
  | otherwise = pure ()

requirePgvector :: Pool Hasql.Connection -> IO ()
requirePgvector pool = do
  available <- runSession pool $ Session.statement () vectorCapabilityStatement
  if available then pure () else throwIO $ DBCapabilityUnavailable "pgvector embedding support is unavailable for observations"

vectorCapabilityStatement :: Statement.Statement () Bool
vectorCapabilityStatement = Statement.Statement
  "SELECT EXISTS (SELECT 1 FROM pg_extension WHERE extname = 'vector') AND EXISTS (SELECT 1 FROM information_schema.columns WHERE table_schema = 'public' AND table_name = 'observations' AND column_name = 'embedding')"
  Enc.noParams (Dec.singleRow (Dec.column (Dec.nonNullable Dec.bool))) True

-- | An online/manual write is atomic with its target-space label.  If a
-- matching durable job exists it is settled without exposing request data.
setObservationEmbeddingInSpace :: Pool Hasql.Connection -> UUID -> UUID -> EmbeddingSpaceFingerprint -> [Double] -> IO ()
setObservationEmbeddingInSpace pool workspace observation space values = do
  validateEmbedding values
  requirePgvector pool
  runTransaction pool $ do
    current <- Session.statement (workspace, observation) currentObservationStatement
    Session.statement (workspace, observation, vectorText values, embeddingSpaceFingerprintText space) setEmbeddingStatement
    settleCurrentJob workspace observation space current

setEmbeddingStatement :: Statement.Statement (UUID, UUID, Text, Text) ()
setEmbeddingStatement = Statement.Statement
  "UPDATE public.observations SET embedding = $3::vector, embedding_space_fingerprint = $4 WHERE workspace_id = $1 AND id = $2"
  (contramap (\(a,_,_,_) -> a) (Enc.param (Enc.nonNullable Enc.uuid))
  <> contramap (\(_,b,_,_) -> b) (Enc.param (Enc.nonNullable Enc.uuid))
  <> contramap (\(_,_,c,_) -> c) (Enc.param (Enc.nonNullable Enc.text))
  <> contramap (\(_,_,_,d) -> d) (Enc.param (Enc.nonNullable Enc.text))) Dec.noResult True

settleMatchingJobStatement :: Statement.Statement (UUID, UUID, Text, Text) ()
settleMatchingJobStatement = Statement.Statement
  "UPDATE public.embedding_jobs SET state = 'complete', lease_owner = NULL, lease_expires_at = NULL, failure_code = NULL, updated_at = now() WHERE workspace_id = $1 AND observation_id = $2 AND space_fingerprint = $3 AND content_fingerprint = $4 AND state IN ('pending', 'leased')"
  (contramap (\(a,_,_,_) -> a) (Enc.param (Enc.nonNullable Enc.uuid))
  <> contramap (\(_,b,_,_) -> b) (Enc.param (Enc.nonNullable Enc.uuid))
  <> contramap (\(_,_,c,_) -> c) (Enc.param (Enc.nonNullable Enc.text))
  <> contramap (\(_,_,_,d) -> d) (Enc.param (Enc.nonNullable Enc.text))) Dec.noResult True

settleOwnedJobStatement :: Statement.Statement (UUID, UUID, Text, Text, Text) ()
settleOwnedJobStatement = Statement.Statement
  "UPDATE public.embedding_jobs SET state = 'complete', lease_owner = NULL, lease_expires_at = NULL, failure_code = NULL, updated_at = now() WHERE workspace_id = $1 AND observation_id = $2 AND lease_owner = $3 AND content_fingerprint = $4 AND space_fingerprint = $5 AND state = 'leased'"
  (contramap (\(a,_,_,_,_) -> a) (Enc.param (Enc.nonNullable Enc.uuid))
  <> contramap (\(_,b,_,_,_) -> b) (Enc.param (Enc.nonNullable Enc.uuid))
  <> contramap (\(_,_,c,_,_) -> c) (Enc.param (Enc.nonNullable Enc.text))
  <> contramap (\(_,_,_,d,_) -> d) (Enc.param (Enc.nonNullable Enc.text))
  <> contramap (\(_,_,_,_,e) -> e) (Enc.param (Enc.nonNullable Enc.text))) Dec.noResult True

-- | CAS prevents a provider result from landing on content or a space it was
-- never computed for.  The fingerprint is checked while the observation row
-- is locked, then vector and label are written together.
compareAndSetObservationEmbedding :: Pool Hasql.Connection -> UUID -> UUID -> Text -> EmbeddingSpaceFingerprint -> [Double] -> IO EmbeddingCasOutcome
compareAndSetObservationEmbedding pool workspace observation expected space values = do
  validateEmbedding values
  requirePgvector pool
  runTransaction pool $ compareAndSetInSession workspace observation expected space values

-- | The worker-only completion path additionally proves that it still owns an
-- unexpired lease for the currently enabled exact space.  A late result from a
-- switched target is therefore stale even when the observation text happens
-- to be unchanged.
completeClaimedEmbeddingJob :: Pool Hasql.Connection -> Text -> UUID -> UUID -> Text -> EmbeddingSpaceFingerprint -> [Double] -> IO EmbeddingCasOutcome
completeClaimedEmbeddingJob pool owner workspace observation expected space values = do
  validateEmbedding values
  requirePgvector pool
  runTransaction pool $ do
    -- Keep the Observation -> job lock order used by observation mutations.
    -- In particular, never lock a job and then wait for its observation.
    current <- Session.statement (workspace, observation) currentObservationStatement
    owned <- Session.statement
      (workspace, observation, owner, expected, embeddingSpaceFingerprintText space)
      ownedCurrentJobStatement
    if owned then do
      outcome <- compareAndSetCurrent workspace observation expected space values current
      case outcome of
        EmbeddingAlreadySatisfied -> Session.statement (workspace, observation, owner, expected, embeddingSpaceFingerprintText space) settleOwnedJobStatement
        _ -> pure ()
      pure outcome
    else pure EmbeddingStale

compareAndSetInSession :: UUID -> UUID -> Text -> EmbeddingSpaceFingerprint -> [Double] -> Session.Session EmbeddingCasOutcome
compareAndSetInSession workspace observation expected space values = do
  current <- Session.statement (workspace, observation) currentObservationStatement
  compareAndSetCurrent workspace observation expected space values current

compareAndSetCurrent :: UUID -> UUID -> Text -> EmbeddingSpaceFingerprint -> [Double] -> Maybe (Text, Text, Text, Bool, Maybe Text) -> Session.Session EmbeddingCasOutcome
compareAndSetCurrent workspace observation expected space values current =
  case current of
    Nothing -> pure EmbeddingNotFound
    Just (sha, subjectsJson, contentValue, hasVector, currentSpace) -> case Aeson.eitherDecodeStrict' (TE.encodeUtf8 subjectsJson) of
      Left _ -> pure EmbeddingStale
      Right subjectValues
        | observationContentFingerprint sha subjectValues contentValue /= expected -> pure EmbeddingStale
        | hasVector && currentSpace == Just (embeddingSpaceFingerprintText space) -> pure EmbeddingAlreadySatisfied
        | otherwise -> do
            Session.statement (workspace, observation, vectorText values, embeddingSpaceFingerprintText space) setEmbeddingStatement
            settleCurrentJob workspace observation space current
            pure EmbeddingApplied

ownedCurrentJobStatement :: Statement.Statement (UUID, UUID, Text, Text, Text) Bool
ownedCurrentJobStatement = Statement.Statement
  "SELECT TRUE FROM public.embedding_jobs j JOIN public.embedding_target_state t ON t.singleton AND t.enabled AND t.space_fingerprint = j.space_fingerprint WHERE j.workspace_id = $1 AND j.observation_id = $2 AND j.lease_owner = $3 AND j.content_fingerprint = $4 AND j.space_fingerprint = $5 AND j.state = 'leased' AND j.lease_expires_at > now() FOR UPDATE OF j, t"
  (contramap (\(a,_,_,_,_) -> a) (Enc.param (Enc.nonNullable Enc.uuid))
  <> contramap (\(_,b,_,_,_) -> b) (Enc.param (Enc.nonNullable Enc.uuid))
  <> contramap (\(_,_,c,_,_) -> c) (Enc.param (Enc.nonNullable Enc.text))
  <> contramap (\(_,_,_,d,_) -> d) (Enc.param (Enc.nonNullable Enc.text))
  <> contramap (\(_,_,_,_,e) -> e) (Enc.param (Enc.nonNullable Enc.text)))
  (maybe False id <$> Dec.rowMaybe (Dec.column (Dec.nonNullable Dec.bool))) True

settleCurrentJob :: UUID -> UUID -> EmbeddingSpaceFingerprint -> Maybe (Text, Text, Text, Bool, Maybe Text) -> Session.Session ()
settleCurrentJob workspace observation space = \case
  Just (sha, subjectsJson, contentValue, _, _) -> case Aeson.eitherDecodeStrict' (TE.encodeUtf8 subjectsJson) of
    Right subjectValues -> Session.statement
      (workspace, observation, embeddingSpaceFingerprintText space, observationContentFingerprint sha subjectValues contentValue)
      settleMatchingJobStatement
    Left _ -> pure ()
  Nothing -> pure ()

currentObservationStatement :: Statement.Statement (UUID, UUID) (Maybe (Text, Text, Text, Bool, Maybe Text))
currentObservationStatement = Statement.Statement sql encoder decoder True where
  sql = BS8.pack $ unlines
    [ "SELECT o.git_sha, (SELECT jsonb_agg(jsonb_build_object('subject_kind', s.subject_kind::text, 'subject', s.subject) ORDER BY s.ordinal)::text FROM public.observation_subjects s WHERE s.observation_id = o.id), o.content, o.embedding IS NOT NULL, o.embedding_space_fingerprint"
    , "FROM public.observations o WHERE o.workspace_id = $1 AND o.id = $2 FOR UPDATE OF o" ]
  encoder = contramap fst (Enc.param (Enc.nonNullable Enc.uuid)) <> contramap snd (Enc.param (Enc.nonNullable Enc.uuid))
  decoder = Dec.rowMaybe ((,,,,)
    <$> Dec.column (Dec.nonNullable Dec.text) <*> Dec.column (Dec.nonNullable Dec.text)
    <*> Dec.column (Dec.nonNullable Dec.text) <*> Dec.column (Dec.nonNullable Dec.bool) <*> Dec.column (Dec.nullable Dec.text))

enableEmbeddingTarget :: Pool Hasql.Connection -> EmbeddingSpaceFingerprint -> IO ()
enableEmbeddingTarget pool space = do
  requirePgvector pool
  runTransaction pool $ Session.statement (embeddingSpaceFingerprintText space) enableTargetStatement

enableTargetStatement :: Statement.Statement Text ()
enableTargetStatement = Statement.Statement
  "INSERT INTO public.embedding_target_state(singleton, enabled, space_fingerprint) VALUES (TRUE, TRUE, $1) ON CONFLICT (singleton) DO UPDATE SET enabled = TRUE, reconcile_cursor = CASE WHEN embedding_target_state.space_fingerprint IS DISTINCT FROM EXCLUDED.space_fingerprint THEN NULL ELSE embedding_target_state.reconcile_cursor END, space_fingerprint = EXCLUDED.space_fingerprint, updated_at = now()"
  (Enc.param (Enc.nonNullable Enc.text)) Dec.noResult True

disableEmbeddingTarget :: Pool Hasql.Connection -> IO ()
disableEmbeddingTarget pool = runTransaction pool $ Session.statement () disableTargetStatement

disableTargetStatement :: Statement.Statement () ()
disableTargetStatement = Statement.Statement "UPDATE public.embedding_target_state SET enabled = FALSE, updated_at = now() WHERE singleton" Enc.noParams Dec.noResult True

-- | Called by the Observation write transaction.  The active target is read
-- and the job upsert performed in that same transaction, so a newly-created
-- or edited row cannot be missed between the mutation and a later worker
-- reconciliation pass.
enqueueObservationForActiveTarget :: Observation -> Session.Session ()
enqueueObservationForActiveTarget observation = Session.statement
  ( observation.id
  , observation.workspaceId
  , observationContentFingerprint observation.gitSha observation.subjects observation.content
  ) enqueueActiveJobStatement

enqueueActiveJobStatement :: Statement.Statement (UUID, UUID, Text) ()
enqueueActiveJobStatement = Statement.Statement
  "INSERT INTO public.embedding_jobs(observation_id, workspace_id, content_fingerprint, space_fingerprint, state) SELECT $1, $2, $3, t.space_fingerprint, 'pending' FROM public.embedding_target_state t WHERE t.singleton AND t.enabled ON CONFLICT (observation_id) DO UPDATE SET workspace_id = EXCLUDED.workspace_id, content_fingerprint = EXCLUDED.content_fingerprint, space_fingerprint = EXCLUDED.space_fingerprint, state = 'pending', lease_owner = NULL, lease_expires_at = NULL, failure_code = NULL, next_attempt_at = now(), updated_at = now() WHERE embedding_jobs.content_fingerprint IS DISTINCT FROM EXCLUDED.content_fingerprint OR embedding_jobs.space_fingerprint IS DISTINCT FROM EXCLUDED.space_fingerprint"
  (contramap (\(a,_,_) -> a) (Enc.param (Enc.nonNullable Enc.uuid))
  <> contramap (\(_,b,_) -> b) (Enc.param (Enc.nonNullable Enc.uuid))
  <> contramap (\(_,_,c) -> c) (Enc.param (Enc.nonNullable Enc.text))) Dec.noResult True

-- | A successful content mutation clears the vector even when the replacement
-- bytes are identical.  Its matching completed job must therefore be made
-- pending again rather than being mistaken for evidence of a current vector.
enqueueObservationForContentChange :: Observation -> Session.Session ()
enqueueObservationForContentChange observation = Session.statement
  ( observation.id
  , observation.workspaceId
  , observationContentFingerprint observation.gitSha observation.subjects observation.content
  ) enqueueContentChangeJobStatement

enqueueContentChangeJobStatement :: Statement.Statement (UUID, UUID, Text) ()
enqueueContentChangeJobStatement = Statement.Statement
  "INSERT INTO public.embedding_jobs(observation_id, workspace_id, content_fingerprint, space_fingerprint, state) SELECT $1, $2, $3, t.space_fingerprint, 'pending' FROM public.embedding_target_state t WHERE t.singleton AND t.enabled ON CONFLICT (observation_id) DO UPDATE SET workspace_id = EXCLUDED.workspace_id, content_fingerprint = EXCLUDED.content_fingerprint, space_fingerprint = EXCLUDED.space_fingerprint, state = 'pending', lease_owner = NULL, lease_expires_at = NULL, failure_code = NULL, next_attempt_at = now(), updated_at = now()"
  (contramap (\(a,_,_) -> a) (Enc.param (Enc.nonNullable Enc.uuid))
  <> contramap (\(_,b,_) -> b) (Enc.param (Enc.nonNullable Enc.uuid))
  <> contramap (\(_,_,c) -> c) (Enc.param (Enc.nonNullable Enc.text))) Dec.noResult True

-- | Bounded backfill after enabling or changing the one target.  The job
-- stores only the stable content hash; document text never enters its row.
reconcileEmbeddingJobs :: Pool Hasql.Connection -> Int -> IO Int
reconcileEmbeddingJobs pool batchSize
  | batchSize < 1 || batchSize > 1000 = pure 0
  | otherwise = do
      target <- runSession pool $ Session.statement () reconcileCursorStatement
      let scanLimit = min 1000 (max batchSize (batchSize * 8))
          cursor = target >>= snd
      rows <- runSession pool $ Session.statement (fromIntegral scanLimit :: Int32, cursor) reconcileCandidatesStatement
      (count, lastScanned) <- enqueueRows rows 0 Nothing
      case lastScanned of
        Just (observation, space) -> runSession pool $ Session.statement (space, observation) advanceReconcileCursorStatement
        Nothing -> case target of
          Just (space, Just _) -> runSession pool $ Session.statement space clearReconcileCursorStatement
          Just (_, Nothing) -> pure ()
          Nothing -> pure ()
      pure count
  where
    enqueueRows [] count lastScanned = pure (count, lastScanned)
    enqueueRows (row:remaining) count lastScanned
      | count >= batchSize = pure (count, lastScanned)
      | otherwise = do
          changed <- enqueue row
          let (observation, _, _, _, _, space, _, _, _, _, _) = row
          enqueueRows remaining (count + changed) (Just (observation, space))
    enqueue (observation, workspace, _, _, _, _, _, _, _, _, _) = runTransaction pool $ reconcileOne workspace observation

-- | Candidate selection is deliberately separate from mutation only for
-- bounded cursor paging.  Every decision below is re-read while holding the
-- Observation, active target, and job locks, so a setter/import that wins in
-- between cannot leave an already-satisfied job pending.
reconcileOne :: UUID -> UUID -> Session.Session Int
reconcileOne workspace observation = do
  current <- Session.statement (workspace, observation) currentObservationStatement
  target <- Session.statement () currentTargetForUpdateStatement
  job <- Session.statement observation currentJobForUpdateStatement
  case (current, target) of
    (Just (sha, subjectJson, body, hasVector, observationSpace), Just space) -> case Aeson.eitherDecodeStrict' (TE.encodeUtf8 subjectJson) of
      Left _ -> pure 0
      Right subjectValues ->
        let expected = observationContentFingerprint sha subjectValues body
            exactJob = case job of
              Just (jobWorkspace, jobSpace, jobContent, _) -> jobWorkspace == workspace && jobSpace == embeddingSpaceFingerprintText space && jobContent == expected
              Nothing -> False
            vectorSatisfiesTarget = hasVector && observationSpace == Just (embeddingSpaceFingerprintText space)
        in case job of
          Just (_, _, _, "complete") | exactJob && not vectorSatisfiesTarget -> do
            Session.statement (observation, workspace, expected, embeddingSpaceFingerprintText space) requeueCompletedJobStatement
            pure 1
          _ | exactJob -> pure 0
            | otherwise -> do
                Session.statement (observation, workspace, expected, embeddingSpaceFingerprintText space) enqueueJobStatement
                pure 1
    _ -> pure 0

currentTargetForUpdateStatement :: Statement.Statement () (Maybe EmbeddingSpaceFingerprint)
currentTargetForUpdateStatement = Statement.Statement
  "SELECT space_fingerprint FROM public.embedding_target_state WHERE singleton AND enabled FOR UPDATE"
  Enc.noParams (maybe Nothing parseEmbeddingSpaceFingerprint <$> Dec.rowMaybe (Dec.column (Dec.nonNullable Dec.text))) True

currentJobForUpdateStatement :: Statement.Statement UUID (Maybe (UUID, Text, Text, Text))
currentJobForUpdateStatement = Statement.Statement
  "SELECT workspace_id, space_fingerprint, content_fingerprint, state FROM public.embedding_jobs WHERE observation_id = $1 FOR UPDATE"
  (Enc.param (Enc.nonNullable Enc.uuid))
  (Dec.rowMaybe ((,,,) <$> Dec.column (Dec.nonNullable Dec.uuid) <*> Dec.column (Dec.nonNullable Dec.text) <*> Dec.column (Dec.nonNullable Dec.text) <*> Dec.column (Dec.nonNullable Dec.text))) True

reconcileCursorStatement :: Statement.Statement () (Maybe (Text, Maybe UUID))
reconcileCursorStatement = Statement.Statement
  "SELECT space_fingerprint, reconcile_cursor FROM public.embedding_target_state WHERE singleton AND enabled"
  Enc.noParams (Dec.rowMaybe ((,) <$> Dec.column (Dec.nonNullable Dec.text) <*> Dec.column (Dec.nullable Dec.uuid))) True

advanceReconcileCursorStatement :: Statement.Statement (Text, UUID) ()
advanceReconcileCursorStatement = Statement.Statement
  "UPDATE public.embedding_target_state SET reconcile_cursor = $2, updated_at = now() WHERE singleton AND enabled AND space_fingerprint = $1"
  (contramap fst (Enc.param (Enc.nonNullable Enc.text)) <> contramap snd (Enc.param (Enc.nonNullable Enc.uuid))) Dec.noResult True

clearReconcileCursorStatement :: Statement.Statement Text ()
clearReconcileCursorStatement = Statement.Statement
  "UPDATE public.embedding_target_state SET reconcile_cursor = NULL, updated_at = now() WHERE singleton AND enabled AND space_fingerprint = $1"
  (Enc.param (Enc.nonNullable Enc.text)) Dec.noResult True

reconcileCandidatesStatement :: Statement.Statement (Int32, Maybe UUID) [(UUID, UUID, Text, Text, Text, Text, Bool, Maybe Text, Maybe Text, Maybe Text, Maybe Text)]
reconcileCandidatesStatement = Statement.Statement sql encoder decoder True where
  sql = BS8.pack $ unlines
    [ "SELECT o.id, o.workspace_id, o.git_sha, (SELECT jsonb_agg(jsonb_build_object('subject_kind', s.subject_kind::text, 'subject', s.subject) ORDER BY s.ordinal)::text FROM public.observation_subjects s WHERE s.observation_id = o.id), o.content, t.space_fingerprint, o.embedding IS NOT NULL, o.embedding_space_fingerprint, j.space_fingerprint, j.content_fingerprint, j.state"
    , "FROM public.observations o JOIN public.embedding_target_state t ON t.singleton AND t.enabled"
    , "LEFT JOIN public.embedding_jobs j ON j.observation_id = o.id"
    , "WHERE (o.embedding IS NULL OR o.embedding_space_fingerprint IS DISTINCT FROM t.space_fingerprint)"
    , "  AND ($2::uuid IS NULL OR o.id > $2)"
    , "ORDER BY o.id LIMIT $1" ]
  encoder = contramap fst (Enc.param (Enc.nonNullable Enc.int4)) <> contramap snd (Enc.param (Enc.nullable Enc.uuid))
  decoder = Dec.rowList ((,,,,,,,,,,)
    <$> Dec.column (Dec.nonNullable Dec.uuid) <*> Dec.column (Dec.nonNullable Dec.uuid)
    <*> Dec.column (Dec.nonNullable Dec.text) <*> Dec.column (Dec.nonNullable Dec.text)
    <*> Dec.column (Dec.nonNullable Dec.text) <*> Dec.column (Dec.nonNullable Dec.text)
    <*> Dec.column (Dec.nonNullable Dec.bool) <*> Dec.column (Dec.nullable Dec.text)
    <*> Dec.column (Dec.nullable Dec.text) <*> Dec.column (Dec.nullable Dec.text) <*> Dec.column (Dec.nullable Dec.text))

enqueueJobStatement :: Statement.Statement (UUID, UUID, Text, Text) ()
enqueueJobStatement = Statement.Statement
  "INSERT INTO public.embedding_jobs(observation_id, workspace_id, content_fingerprint, space_fingerprint, state) VALUES ($1, $2, $3, $4, 'pending') ON CONFLICT (observation_id) DO UPDATE SET workspace_id = EXCLUDED.workspace_id, content_fingerprint = EXCLUDED.content_fingerprint, space_fingerprint = EXCLUDED.space_fingerprint, state = 'pending', lease_owner = NULL, lease_expires_at = NULL, failure_code = NULL, next_attempt_at = now(), updated_at = now() WHERE embedding_jobs.content_fingerprint IS DISTINCT FROM EXCLUDED.content_fingerprint OR embedding_jobs.space_fingerprint IS DISTINCT FROM EXCLUDED.space_fingerprint"
  (contramap (\(a,_,_,_) -> a) (Enc.param (Enc.nonNullable Enc.uuid)) <> contramap (\(_,b,_,_) -> b) (Enc.param (Enc.nonNullable Enc.uuid)) <> contramap (\(_,_,c,_) -> c) (Enc.param (Enc.nonNullable Enc.text)) <> contramap (\(_,_,_,d) -> d) (Enc.param (Enc.nonNullable Enc.text))) Dec.noResult True

requeueCompletedJobStatement :: Statement.Statement (UUID, UUID, Text, Text) ()
requeueCompletedJobStatement = Statement.Statement
  "UPDATE public.embedding_jobs SET state = 'pending', lease_owner = NULL, lease_expires_at = NULL, failure_code = NULL, next_attempt_at = now(), updated_at = now() WHERE observation_id = $1 AND workspace_id = $2 AND content_fingerprint = $3 AND space_fingerprint = $4 AND state = 'complete'"
  (contramap (\(a,_,_,_) -> a) (Enc.param (Enc.nonNullable Enc.uuid)) <> contramap (\(_,b,_,_) -> b) (Enc.param (Enc.nonNullable Enc.uuid)) <> contramap (\(_,_,c,_) -> c) (Enc.param (Enc.nonNullable Enc.text)) <> contramap (\(_,_,_,d) -> d) (Enc.param (Enc.nonNullable Enc.text))) Dec.noResult True

-- | Concurrent workers claim only due pending/expired jobs.  SKIP LOCKED
-- makes bounded batches safe without serialising workers.
claimEmbeddingJobs :: Pool Hasql.Connection -> Text -> Int -> IO [EmbeddingJob]
claimEmbeddingJobs pool owner batchSize
  | T.null owner || T.length owner > 128 || batchSize < 1 || batchSize > 100 = pure []
  | otherwise = runTransaction pool $ do
      -- A lease which has already used the final allowed attempt cannot be
      -- reclaimed: settle it once, then leave claim selection below bounded.
      Session.statement () expireExhaustedLeasesStatement
      Session.statement (owner, fromIntegral batchSize :: Int32) claimJobsStatement

expireExhaustedLeasesStatement :: Statement.Statement () ()
expireExhaustedLeasesStatement = Statement.Statement
  "WITH exhausted AS (SELECT observation_id FROM public.embedding_jobs j WHERE state = 'leased' AND lease_expires_at <= now() AND attempts >= 16 ORDER BY lease_expires_at, observation_id FOR UPDATE OF j SKIP LOCKED LIMIT 100) UPDATE public.embedding_jobs j SET state = 'failed', lease_owner = NULL, lease_expires_at = NULL, failure_code = 'provider_exhausted', updated_at = now() FROM exhausted WHERE j.observation_id = exhausted.observation_id"
  Enc.noParams Dec.noResult True

claimJobsStatement :: Statement.Statement (Text, Int32) [EmbeddingJob]
claimJobsStatement = Statement.Statement sql encoder decoder True where
  sql = BS8.pack $ unlines
    [ "WITH chosen AS (SELECT j.observation_id FROM public.embedding_jobs j JOIN public.embedding_target_state t ON t.singleton AND t.enabled AND t.space_fingerprint = j.space_fingerprint WHERE ((j.state = 'pending' AND j.next_attempt_at <= now()) OR (j.state = 'leased' AND j.lease_expires_at <= now())) AND j.attempts < 16 ORDER BY j.next_attempt_at, j.observation_id FOR UPDATE OF j SKIP LOCKED LIMIT $2), claimed AS (UPDATE public.embedding_jobs j SET state = 'leased', lease_owner = $1, lease_expires_at = now() + interval '60 seconds', attempts = j.attempts + 1, updated_at = now() FROM chosen WHERE j.observation_id = chosen.observation_id RETURNING j.observation_id, j.workspace_id, j.content_fingerprint, j.space_fingerprint, j.attempts, j.lease_owner, j.lease_expires_at)"
    , "SELECT c.observation_id, c.workspace_id, c.content_fingerprint, c.space_fingerprint, c.attempts, c.lease_owner, c.lease_expires_at, o.content FROM claimed c JOIN public.observations o ON o.id = c.observation_id AND o.workspace_id = c.workspace_id ORDER BY c.observation_id" ]
  encoder = contramap fst (Enc.param (Enc.nonNullable Enc.text)) <> contramap snd (Enc.param (Enc.nonNullable Enc.int4))
  decoder = Dec.rowList $ EmbeddingJob <$> Dec.column (Dec.nonNullable Dec.uuid) <*> Dec.column (Dec.nonNullable Dec.uuid) <*> Dec.column (Dec.nonNullable Dec.text) <*> (toSpace <$> Dec.column (Dec.nonNullable Dec.text)) <*> (fromIntegral <$> Dec.column (Dec.nonNullable Dec.int4)) <*> Dec.column (Dec.nonNullable Dec.text) <*> Dec.column (Dec.nonNullable Dec.timestamptz) <*> Dec.column (Dec.nonNullable Dec.text)
  toSpace raw = maybe legacyManualEmbeddingSpace id (parseEmbeddingSpaceFingerprint raw)

releaseEmbeddingJob :: Pool Hasql.Connection -> UUID -> Text -> Text -> Bool -> IO ()
releaseEmbeddingJob pool observation owner failure retry = runSession pool $ Session.statement (observation, owner, failure, retry) releaseJobStatement

releaseJobStatement :: Statement.Statement (UUID, Text, Text, Bool) ()
releaseJobStatement = Statement.Statement
  "UPDATE public.embedding_jobs SET state = CASE WHEN $3 = 'provider_cancelled' OR ($4 AND attempts < 16) THEN 'pending' ELSE 'failed' END, attempts = CASE WHEN $3 = 'provider_cancelled' AND attempts >= 16 THEN 15 ELSE attempts END, next_attempt_at = now() + make_interval(secs => LEAST(3600, 2 ^ LEAST(attempts, 10))::int), lease_owner = NULL, lease_expires_at = NULL, failure_code = $3, updated_at = now() WHERE observation_id = $1 AND lease_owner = $2 AND state = 'leased'"
  (contramap (\(a,_,_,_) -> a) (Enc.param (Enc.nonNullable Enc.uuid)) <> contramap (\(_,b,_,_) -> b) (Enc.param (Enc.nonNullable Enc.text)) <> contramap (\(_,_,c,_) -> c) (Enc.param (Enc.nonNullable Enc.text)) <> contramap (\(_,_,_,d) -> d) (Enc.param (Enc.nonNullable Enc.bool))) Dec.noResult True

vectorText :: [Double] -> Text
vectorText = ("[" <>) . (<> "]") . T.intercalate "," . map (T.pack . show)
