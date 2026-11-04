-- | Operator controls and compact verification/progress, separate from evidence.
module HMem.DB.SessionArchive.Maintenance
  ( MaintenanceAction(..), maintenanceStatus, configureMaintenance
  , discoverArchives, ReclamationLease(..), claimReclamation, renewReclamation, releaseReclamation, recordVerification, reclaimArchiveBatch
  , verificationRecorded, verifiedCheckpoint, verifiedSanitizedCheckpoint
  , readReclamationSnapshot, readReclamationSourceChunk
  ) where

import Control.Exception (throwIO)
import Control.Monad (forM_)
import Data.Aeson (Value,fromJSON,Result(..))
import Data.ByteString (ByteString)
import Data.Functor.Contravariant (contramap)
import Data.Int (Int64)
import Data.Pool (Pool)
import Data.Text (Text)
import Data.Text qualified as T
import Data.UUID (UUID)
import Hasql.Connection qualified as H
import Hasql.Decoders qualified as D
import Hasql.Encoders qualified as E
import Hasql.Session qualified as S
import Hasql.Statement qualified as St

import HMem.DB.Pool (runSession,runTransaction)
import HMem.DB.SessionArchive qualified as A
import HMem.DB.SessionArchive.Reader qualified as R
import HMem.SessionArchive.Replacement qualified as Replacement

data MaintenanceAction = Pause | Resume | EnableBackfill | DisableBackfill
  | EnableReclamation | DisableReclamation | Capacity Int64 Int64
  | EnableSanitizedReclamation | DisableSanitizedReclamation
  | InstallReplacement Replacement.Policy | DisableReplacement
  | Retry UUID UUID deriving stock (Show,Eq)

data ReclamationLease = ReclamationLease
  { manifest :: !A.ArchiveManifest, leaseToken :: !UUID, sanitized :: !Bool, verificationBytes :: !Int64 }
  deriving stock (Show,Eq)

column :: D.Value a -> D.Row a
column=D.column . D.nonNullable
param :: (a -> b) -> E.Value b -> E.Params a
param f v=contramap f (E.param (E.nonNullable v))
statement :: ByteString -> E.Params p -> D.Result r -> St.Statement p r
statement sql enc dec=St.Statement sql enc dec True
lockControl :: S.Session ()
lockControl=S.statement () $ statement "SELECT singleton FROM session_archive_control WHERE singleton FOR UPDATE" E.noParams (() <$ D.singleRow (column D.bool))

maintenanceStatus :: Pool H.Connection -> IO Value
maintenanceStatus pool=runSession pool $ S.statement () $ statement
  ("SELECT to_jsonb(c)||jsonb_build_object('runtime_safe',hmem_archive_runtime_safe(),'sanitized_runtime_safe',hmem_archive_sanitized_runtime_safe(),"<>
  "'sanitized_cleanup',(SELECT jsonb_build_object('pending',count(*) FILTER (WHERE coalesce(r.deleted_bytes,0)=0 AND NOT coalesce(r.complete,false)),'in_progress',count(*) FILTER (WHERE r.deleted_bytes>0 AND NOT r.complete),'complete',count(*) FILTER (WHERE r.complete)) FROM "<>R.sanitizedRelation<>" g LEFT JOIN session_archive_reclamation r ON r.generation_id=g.id),"<>
  "'capacity_kind','configured_logical_allowance','physical_free_bytes',NULL,'actual_wal_limit',false,'state',CASE WHEN c.paused AND c.permit_expires_at>clock_timestamp() THEN 'draining' WHEN c.paused THEN 'paused' WHEN c.blocked_reason IS NOT NULL THEN c.blocked_reason ELSE 'running' END,'resource_blocked_jobs',(SELECT coalesce(jsonb_agg(to_jsonb(b)),'[]') FROM (SELECT j.session_id,j.workspace_id,hmem_archive_reservation(s.final_bytes,s.revision-1)::text required_bytes FROM session_archive_jobs j JOIN sessions s ON s.id=j.session_id JOIN workspaces w ON w.id=j.workspace_id WHERE w.deleted_at IS NULL AND j.state IN ('queued','retry') AND j.retry_at<=clock_timestamp() AND (c.backfill_enabled OR s.terminated_at>=c.cutover_at OR EXISTS(SELECT 1 FROM session_archive_requests demand WHERE demand.session_id=s.id AND demand.workspace_id=s.workspace_id)) AND (hmem_archive_reservation(s.final_bytes,s.revision-1)+c.staged_bytes>c.staging_budget OR hmem_archive_reservation(s.final_bytes,s.revision-1)*4>c.wal_budget) ORDER BY j.retry_at,j.session_id LIMIT 20) b),'jobs',(SELECT coalesce(jsonb_object_agg(state,n),'{}') FROM (SELECT state,count(*) n FROM session_archive_jobs GROUP BY state) counts),'verified',(SELECT count(*) FROM session_archive_reclamation WHERE verified_at IS NOT NULL),'reclaimed_bytes',(SELECT coalesce(sum(deleted_bytes),0) FROM session_archive_reclamation),'cleanup_complete',(SELECT count(*) FROM session_archive_reclamation WHERE complete),'verification_failures',(SELECT count(*) FROM session_archive_reclamation WHERE error_code IS NOT NULL)) FROM session_archive_control c WHERE singleton")
  E.noParams (D.singleRow (column D.jsonb))

-- Enabling either destructive/history operation is an explicit qualification
-- acknowledgement by the trusted operator. Pause keeps the current lease alive.
configureMaintenance :: Pool H.Connection -> MaintenanceAction -> IO ()
configureMaintenance pool (InstallReplacement policy)=do
  result <- runTransaction pool $ do
    lockControl
    activated <- S.statement () (statement "SELECT clock_timestamp()" E.noParams (D.singleRow (column D.timestamptz)))
    case Replacement.freezePolicies activated policy of
      Left failure -> pure (Left failure)
      Right frozen -> do
        clearPolicy
        forM_ frozen $ \row -> S.statement row $ statement
          "INSERT INTO session_archive_replacement_policies(format_label,eligible_from,snapshot) VALUES($1,$2,$3)"
          (param (\(label,_,_)->label) E.text<>param (\(_,cutoff,_)->cutoff) E.timestamptz<>param (\(_,_,bytes)->bytes) E.bytea) D.noResult
        pure (Right ())
  either throwIO pure result
configureMaintenance pool action=runTransaction pool $ do
  lockControl
  case action of
    Pause -> simple "UPDATE session_archive_control SET paused=true WHERE singleton"
    Resume -> simple "UPDATE session_archive_control SET paused=false,blocked_reason=NULL WHERE singleton"
    EnableBackfill -> simple "UPDATE session_archive_control SET qualified=true,backfill_enabled=true WHERE singleton"
    DisableBackfill -> simple "UPDATE session_archive_control SET backfill_enabled=false WHERE singleton"
    EnableReclamation -> simple "UPDATE session_archive_control SET qualified=true,reclaim_enabled=true WHERE singleton"
    DisableReclamation -> simple "UPDATE session_archive_control SET reclaim_enabled=false,sanitized_reclaim_enabled=false WHERE singleton"
    EnableSanitizedReclamation -> simple "UPDATE session_archive_control SET sanitized_qualified=true,sanitized_reclaim_enabled=true WHERE singleton"
    DisableSanitizedReclamation -> simple "UPDATE session_archive_control SET sanitized_reclaim_enabled=false WHERE singleton"
    DisableReplacement -> clearPolicy
    Capacity staging wal -> S.statement (staging,wal) $ statement
      "UPDATE session_archive_control SET staging_budget=$1,wal_budget=$2,blocked_reason=NULL WHERE singleton"
      (param fst E.int8<>param snd E.int8) D.noResult
    Retry workspace session -> do
      S.statement (workspace,session) (statement
        "UPDATE session_archive_jobs SET state='retry',retry_at=clock_timestamp(),error_code=NULL WHERE workspace_id=$1 AND session_id=$2 AND state IN ('failed','retry')"
        (param fst E.uuid<>param snd E.uuid) D.noResult)
      S.statement (workspace,session) (statement
        "UPDATE session_archive_reclamation SET retry_at=clock_timestamp(),error_code=NULL WHERE workspace_id=$1 AND session_id=$2 AND NOT complete AND (verified_at IS NULL OR (sanitized_attestation_sha256 IS NOT NULL AND error_code IS NOT NULL))"
        (param fst E.uuid<>param snd E.uuid) D.noResult)
  where simple sql=S.statement () (statement sql E.noParams D.noResult)

clearPolicy :: S.Session ()
clearPolicy=do
  S.sql "DELETE FROM session_archive_replacement_policies"
  -- Version the same row that enqueue locks. A repeatable-read transaction
  -- started before this policy transition cannot freeze a stale map.
  S.sql "UPDATE session_archive_control SET singleton=singleton WHERE singleton"

-- Candidate insertion and cursor advancement commit together. A completed
-- sweep restarts, covering a seal racing the prior keyset without a RAM cursor.
discoverArchives :: Pool H.Connection -> IO Int64
discoverArchives pool=runTransaction pool $ do
  lockControl
  S.statement () $ statement
    "WITH candidates AS MATERIALIZED (SELECT s.terminated_at,s.id,s.workspace_id FROM sessions s JOIN workspaces w ON w.id=s.workspace_id CROSS JOIN session_archive_control c WHERE c.singleton AND NOT c.paused AND s.state='terminated' AND w.deleted_at IS NULL AND (c.backfill_enabled OR s.terminated_at>=c.cutover_at) AND (c.cursor_time IS NULL OR (s.terminated_at,s.id)>(c.cursor_time,c.cursor_id)) ORDER BY s.terminated_at,s.id LIMIT 32), inserted AS (INSERT INTO session_archive_jobs(session_id,workspace_id) SELECT id,workspace_id FROM candidates ON CONFLICT DO NOTHING), advanced AS (UPDATE session_archive_control SET cursor_time=(SELECT terminated_at FROM candidates ORDER BY terminated_at DESC,id DESC LIMIT 1),cursor_id=(SELECT id FROM candidates ORDER BY terminated_at DESC,id DESC LIMIT 1) WHERE singleton AND NOT paused) SELECT count(*) FROM candidates"
    E.noParams (D.singleRow (column D.int8))

claimReclamation :: Pool H.Connection -> IO (Maybe ReclamationLease)
claimReclamation pool=runTransaction pool $ do
  lockControl
  S.statement () $ statement
    ("WITH controls AS MATERIALIZED (SELECT c.qualified AND c.reclaim_enabled AND hmem_archive_runtime_safe() AS exact,c.sanitized_qualified AND c.sanitized_reclaim_enabled AND hmem_archive_sanitized_runtime_safe() AS sanitized FROM session_archive_control c WHERE c.singleton AND NOT c.paused AND (c.permit_expires_at IS NULL OR c.permit_expires_at<=clock_timestamp()) AND NOT EXISTS(SELECT 1 FROM session_archive_generations WHERE published_at IS NULL)), "<>
    "published AS (SELECT g.id,g.workspace_id,g.session_id,g.published_at,false AS sanitized FROM session_archives g CROSS JOIN controls c WHERE c.exact UNION ALL SELECT g.id,g.workspace_id,g.session_id,g.published_at,true FROM "<>R.sanitizedRelation<>" g CROSS JOIN controls c WHERE c.sanitized), "<>
    "candidate AS MATERIALIZED (SELECT p.id,p.sanitized FROM published p JOIN workspaces w ON w.id=p.workspace_id LEFT JOIN session_archive_reclamation r ON r.generation_id=p.id WHERE w.deleted_at IS NULL AND (r.generation_id IS NULL OR NOT r.complete AND r.retry_at<=clock_timestamp()) AND (NOT p.sanitized OR r.deleted_bytes>0 OR EXISTS(SELECT 1 FROM session_occurrences o JOIN sessions s ON (s.id,s.workspace_id)=(o.session_id,o.workspace_id) WHERE (o.session_id,o.workspace_id)=(p.session_id,p.workspace_id) AND o.derivation_version=1 AND o.processed_bytes=s.final_bytes AND octet_length(o.pending)=0)) ORDER BY coalesce(r.retry_at,p.published_at),p.id LIMIT 1), "<>
    "admitted AS (UPDATE session_archive_control SET permit_generation=(SELECT id FROM candidate),permit_token=gen_random_uuid(),permit_kind='maintenance',permit_expires_at=clock_timestamp()+interval '60 seconds',reserved_bytes=0,blocked_reason=NULL WHERE singleton AND EXISTS(SELECT 1 FROM candidate) RETURNING permit_token) "<>
    "SELECT to_jsonb(g),a.permit_token,p.sanitized,least(g.raw_bytes::numeric+coalesce(v.content_bytes,0),9223372036854775807)::bigint FROM candidate p JOIN session_archive_generations g ON g.id=p.id LEFT JOIN session_archive_sanitized_contents v ON p.sanitized AND v.generation_id=g.id CROSS JOIN admitted a")
    E.noParams (D.rowMaybe (ReclamationLease <$> column (D.refine (\v -> case fromJSON v of Success x -> Right x; Error e -> Left (T.pack e)) D.jsonb) <*> column D.uuid <*> column D.bool <*> column D.int8))

leaseParams :: E.Params ReclamationLease
leaseParams=param ((.id) . (.manifest)) E.uuid<>param (.leaseToken) E.uuid
renewReclamation :: Pool H.Connection -> ReclamationLease -> IO Bool
renewReclamation pool lease=runTransaction pool $ do
  lockControl
  S.statement lease $ statement
    "WITH renewed AS (UPDATE session_archive_control SET permit_expires_at=clock_timestamp()+interval '60 seconds' WHERE singleton AND permit_kind='maintenance' AND permit_generation=$1 AND permit_token=$2 AND permit_expires_at>clock_timestamp() RETURNING 1) SELECT EXISTS(SELECT 1 FROM renewed)"
    leaseParams (D.singleRow (column D.bool))
releaseReclamation :: Pool H.Connection -> ReclamationLease -> IO ()
releaseReclamation pool lease=runTransaction pool $ do
  lockControl
  S.statement lease $ statement
    "UPDATE session_archive_control SET permit_generation=NULL,permit_token=NULL,permit_kind=NULL,permit_expires_at=NULL,reserved_bytes=0 WHERE singleton AND permit_kind='maintenance' AND permit_generation=$1 AND permit_token=$2"
    leaseParams D.noResult

-- Success is trusted worker attestation after bounded independent readback.
-- A failed read stores no verification capability; retry has a finite backoff.
recordVerification :: Pool H.Connection -> ReclamationLease -> Maybe Text -> IO ()
recordVerification pool lease failure=runTransaction pool $ do
  lockControl
  S.statement (lease,failure) $ statement
    ("INSERT INTO session_archive_reclamation(generation_id,session_id,workspace_id,metadata_sha256,verified_at,error_code,retry_at,sanitized_attestation_sha256) "<>
    "SELECT g.id,g.session_id,g.workspace_id,g.metadata_sha256,CASE WHEN $5::text IS NULL THEN clock_timestamp() END,$5,clock_timestamp()+CASE WHEN $5::text IS NULL THEN interval '0 seconds' ELSE interval '60 seconds' END,CASE WHEN $5::text IS NULL AND $7 THEN public.hmem_archive_sanitized_attestation(g,j.replacement_policy_sha256) END "<>
    "FROM session_archive_generations g JOIN session_archive_jobs j ON (j.generation_id,j.session_id,j.workspace_id)=(g.id,g.session_id,g.workspace_id) CROSS JOIN session_archive_control c WHERE c.singleton AND c.permit_kind='maintenance' AND c.permit_generation=$3 AND c.permit_token=$6 AND c.permit_expires_at>clock_timestamp() AND g.workspace_id=$1 AND g.session_id=$2 AND g.id=$3 AND g.metadata_sha256=$4 AND j.state='ready' AND g.published_at IS NOT NULL AND (j.replacement_snapshot IS NOT NULL)=$7 "<>
    "ON CONFLICT(generation_id) DO UPDATE SET "<>
    "verified_at=CASE WHEN EXCLUDED.error_code IS NOT NULL AND session_archive_reclamation.sanitized_attestation_sha256 IS NOT NULL THEN session_archive_reclamation.verified_at ELSE EXCLUDED.verified_at END,"<>
    "error_code=CASE WHEN EXCLUDED.error_code IS NOT NULL AND session_archive_reclamation.sanitized_attestation_sha256 IS NOT NULL THEN 'archive_reclamation_failed' ELSE EXCLUDED.error_code END,"<>
    "retry_at=EXCLUDED.retry_at,sanitized_attestation_sha256=CASE WHEN EXCLUDED.error_code IS NOT NULL THEN session_archive_reclamation.sanitized_attestation_sha256 ELSE EXCLUDED.sanitized_attestation_sha256 END "<>
    "WHERE session_archive_reclamation.metadata_sha256=EXCLUDED.metadata_sha256 AND NOT session_archive_reclamation.complete AND (session_archive_reclamation.deleted_revision=0 OR (session_archive_reclamation.sanitized_attestation_sha256 IS NOT NULL AND EXCLUDED.error_code IS NOT NULL))")
    (param ((.workspaceId) . (.manifest) . fst) E.uuid<>param ((.sessionId) . (.manifest) . fst) E.uuid<>param ((.id) . (.manifest) . fst) E.uuid<>param ((.metadataSha256) . (.manifest) . fst) E.text<>contramap snd (E.param (E.nullable E.text))<>param ((.leaseToken) . fst) E.uuid<>param ((.sanitized) . fst) E.bool) D.noResult

reclaimArchiveBatch :: Pool H.Connection -> ReclamationLease -> IO Int64
reclaimArchiveBatch pool lease=runTransaction pool $ S.statement lease $ statement
  "SELECT public.hmem_archive_reclaim($1,$2,$3,$4,$5)"
  (param ((.workspaceId) . (.manifest)) E.uuid<>param ((.sessionId) . (.manifest)) E.uuid<>param ((.id) . (.manifest)) E.uuid<>param ((.metadataSha256) . (.manifest)) E.text<>param (.leaseToken) E.uuid)
  (D.singleRow (column D.int8))

verificationRecorded :: Pool H.Connection -> A.ArchiveManifest -> IO Bool
verificationRecorded pool m=runSession pool $ S.statement m $ statement
  "SELECT EXISTS(SELECT 1 FROM session_archive_reclamation r JOIN session_archive_generations g ON (g.id,g.session_id,g.workspace_id)=(r.generation_id,r.session_id,r.workspace_id) JOIN session_archive_jobs j ON (j.generation_id,j.session_id,j.workspace_id)=(g.id,g.session_id,g.workspace_id) WHERE r.generation_id=$1 AND r.metadata_sha256=$2 AND g.metadata_sha256=$2 AND r.verified_at IS NOT NULL AND (r.error_code IS NULL OR r.sanitized_attestation_sha256 IS NOT NULL) AND r.sanitized_attestation_sha256 IS NOT DISTINCT FROM public.hmem_archive_sanitized_attestation(g,j.replacement_policy_sha256))"
  (param (.id) E.uuid<>param (.metadataSha256) E.text) (D.singleRow (column D.bool))

verifiedCheckpoint :: Pool H.Connection -> A.ArchiveManifest -> Int64 -> IO (Maybe A.ArchiveCheckpoint)
verifiedCheckpoint=verifiedCheckpointFrom "session_archives"

verifiedSanitizedCheckpoint :: Pool H.Connection -> A.ArchiveManifest -> Int64 -> IO (Maybe A.ArchiveCheckpoint)
verifiedSanitizedCheckpoint=verifiedCheckpointFrom R.sanitizedRelation

verifiedCheckpointFrom :: ByteString -> Pool H.Connection -> A.ArchiveManifest -> Int64 -> IO (Maybe A.ArchiveCheckpoint)
verifiedCheckpointFrom relation pool manifest ordinal=R.runArchiveRead pool (manifest,ordinal) $ statement
  ("SELECT c.ordinal,c.raw_offset,c.compressed_offset,c.unused_bits,c.dictionary,c.dictionary_sha256,c.binding_sha256 FROM "<>relation<>" g JOIN session_archive_checkpoints c ON c.generation_id=g.id WHERE g.workspace_id=$1 AND g.session_id=$2 AND g.id=$3 AND c.ordinal=$4 AND c.dictionary_sha256=encode(sha256(c.dictionary),'hex') AND c.binding_sha256=hmem_archive_binding(coalesce((SELECT p.binding_sha256 FROM session_archive_checkpoints p WHERE p.generation_id=c.generation_id AND p.ordinal=c.ordinal-1),''),jsonb_build_array(c.generation_id,c.ordinal,c.raw_offset,c.compressed_offset,c.unused_bits,c.dictionary_sha256))")
  (param ((.workspaceId) . fst) E.uuid<>param ((.sessionId) . fst) E.uuid<>param ((.id) . fst) E.uuid<>param snd E.int8)
  (D.rowMaybe (A.ArchiveCheckpoint <$> column D.int8 <*> column D.int8 <*> column D.int8 <*> column D.int2 <*> column D.bytea <*> column D.text <*> column D.text))

reclamationIdentity :: E.Params ReclamationLease
reclamationIdentity=param ((.workspaceId) . (.manifest)) E.uuid<>param ((.sessionId) . (.manifest)) E.uuid<>
  param ((.id) . (.manifest)) E.uuid<>param (.leaseToken) E.uuid

livePermit :: ByteString
livePermit="EXISTS(SELECT 1 FROM session_archive_control c WHERE c.singleton AND c.permit_kind='maintenance' AND c.permit_generation=$3 AND c.permit_token=$4 AND c.permit_expires_at>clock_timestamp())"

readReclamationSnapshot :: Pool H.Connection -> ReclamationLease -> IO (Maybe A.ArchiveReplacementSnapshot)
readReclamationSnapshot pool lease=R.runArchiveRead pool lease $ statement
  ("SELECT j.replacement_snapshot,j.replacement_eligible_from,s.format_label FROM session_archive_jobs j JOIN sessions s ON (s.id,s.workspace_id)=(j.session_id,j.workspace_id) JOIN session_archive_generations g ON (g.id,g.session_id,g.workspace_id)=(j.generation_id,j.session_id,j.workspace_id) WHERE j.workspace_id=$1 AND j.session_id=$2 AND j.generation_id=$3 AND j.state='ready' AND s.state='terminated' AND s.terminated_at>=j.replacement_eligible_from AND j.replacement_policy_sha256=encode(sha256(j.replacement_snapshot),'hex') AND (s.final_bytes,s.committed_bytes,s.committed_records,s.revision,s.final_sha256)=(g.raw_bytes,g.raw_bytes,g.raw_records,g.raw_revision,g.raw_sha256) AND "<>livePermit)
  reclamationIdentity (D.rowMaybe (A.ArchiveReplacementSnapshot <$> column D.bytea <*> column D.timestamptz <*> column D.text))

readReclamationSourceChunk :: Pool H.Connection -> ReclamationLease -> Int64 -> IO (Maybe A.ArchiveSourceChunk)
readReclamationSourceChunk pool lease offset=R.runArchiveRead pool (lease,offset) $ statement
  ("SELECT c.revision,c.start_offset,c.data,c.allow_final_record,c.record_count,c.ends_with_newline FROM session_chunks c JOIN sessions s ON s.id=c.session_id WHERE s.workspace_id=$1 AND s.id=$2 AND c.start_offset>=$5 AND "<>livePermit<>" ORDER BY c.start_offset LIMIT 1")
  (contramap fst reclamationIdentity<>param snd E.int8)
  (D.rowMaybe (A.ArchiveSourceChunk <$> column D.int8 <*> column D.int8 <*> column D.bytea <*> column D.bool <*> column D.int8 <*> column D.bool))
