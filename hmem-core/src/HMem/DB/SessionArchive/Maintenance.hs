-- | Operator controls and compact verification/progress, separate from evidence.
module HMem.DB.SessionArchive.Maintenance
  ( MaintenanceAction(..), maintenanceStatus, configureMaintenance
  , discoverArchives, ReclamationLease(..), claimReclamation, renewReclamation, releaseReclamation, recordVerification, reclaimArchiveBatch
  , verificationRecorded, verifiedCheckpoint
  ) where

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

data MaintenanceAction = Pause | Resume | EnableBackfill | DisableBackfill
  | EnableReclamation | DisableReclamation | Capacity Int64 Int64
  | Retry UUID UUID deriving stock (Show,Eq)

data ReclamationLease = ReclamationLease {manifest :: !A.ArchiveManifest, leaseToken :: !UUID} deriving stock (Show,Eq)

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
  "SELECT to_jsonb(c)||jsonb_build_object('runtime_safe',hmem_archive_runtime_safe(),'capacity_kind','configured_logical_allowance','physical_free_bytes',NULL,'actual_wal_limit',false,'state',CASE WHEN c.paused AND c.permit_expires_at>clock_timestamp() THEN 'draining' WHEN c.paused THEN 'paused' WHEN c.blocked_reason IS NOT NULL THEN c.blocked_reason ELSE 'running' END,'resource_blocked_jobs',(SELECT coalesce(jsonb_agg(to_jsonb(b)),'[]') FROM (SELECT j.session_id,j.workspace_id,hmem_archive_reservation(s.final_bytes,s.revision-1)::text required_bytes FROM session_archive_jobs j JOIN sessions s ON s.id=j.session_id JOIN workspaces w ON w.id=j.workspace_id WHERE w.deleted_at IS NULL AND j.state IN ('queued','retry') AND j.retry_at<=clock_timestamp() AND (c.backfill_enabled OR s.terminated_at>=c.cutover_at OR EXISTS(SELECT 1 FROM session_archive_requests demand WHERE demand.session_id=s.id AND demand.workspace_id=s.workspace_id)) AND (hmem_archive_reservation(s.final_bytes,s.revision-1)+c.staged_bytes>c.staging_budget OR hmem_archive_reservation(s.final_bytes,s.revision-1)*4>c.wal_budget) ORDER BY j.retry_at,j.session_id LIMIT 20) b),'jobs',(SELECT coalesce(jsonb_object_agg(state,n),'{}') FROM (SELECT state,count(*) n FROM session_archive_jobs GROUP BY state) counts),'verified',(SELECT count(*) FROM session_archive_reclamation WHERE verified_at IS NOT NULL),'reclaimed_bytes',(SELECT coalesce(sum(deleted_bytes),0) FROM session_archive_reclamation),'cleanup_complete',(SELECT count(*) FROM session_archive_reclamation WHERE complete),'verification_failures',(SELECT count(*) FROM session_archive_reclamation WHERE error_code IS NOT NULL)) FROM session_archive_control c WHERE singleton"
  E.noParams (D.singleRow (column D.jsonb))

-- Enabling either destructive/history operation is an explicit qualification
-- acknowledgement by the trusted operator. Pause keeps the current lease alive.
configureMaintenance :: Pool H.Connection -> MaintenanceAction -> IO ()
configureMaintenance pool action=runTransaction pool $ do
  lockControl
  case action of
    Pause -> simple "UPDATE session_archive_control SET paused=true WHERE singleton"
    Resume -> simple "UPDATE session_archive_control SET paused=false,blocked_reason=NULL WHERE singleton"
    EnableBackfill -> simple "UPDATE session_archive_control SET qualified=true,backfill_enabled=true WHERE singleton"
    DisableBackfill -> simple "UPDATE session_archive_control SET backfill_enabled=false WHERE singleton"
    EnableReclamation -> simple "UPDATE session_archive_control SET qualified=true,reclaim_enabled=true WHERE singleton"
    DisableReclamation -> simple "UPDATE session_archive_control SET reclaim_enabled=false WHERE singleton"
    Capacity staging wal -> S.statement (staging,wal) $ statement
      "UPDATE session_archive_control SET staging_budget=$1,wal_budget=$2,blocked_reason=NULL WHERE singleton"
      (param fst E.int8<>param snd E.int8) D.noResult
    Retry workspace session -> do
      S.statement (workspace,session) (statement
        "UPDATE session_archive_jobs SET state='retry',retry_at=clock_timestamp(),error_code=NULL WHERE workspace_id=$1 AND session_id=$2 AND state IN ('failed','retry')"
        (param fst E.uuid<>param snd E.uuid) D.noResult)
      S.statement (workspace,session) (statement
        "UPDATE session_archive_reclamation SET retry_at=clock_timestamp(),error_code=NULL WHERE workspace_id=$1 AND session_id=$2 AND verified_at IS NULL"
        (param fst E.uuid<>param snd E.uuid) D.noResult)
  where simple sql=S.statement () (statement sql E.noParams D.noResult)

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
    "WITH candidate AS MATERIALIZED (SELECT g.* FROM session_archives g JOIN workspaces w ON w.id=g.workspace_id CROSS JOIN session_archive_control c LEFT JOIN session_archive_reclamation r ON r.generation_id=g.id WHERE c.singleton AND w.deleted_at IS NULL AND hmem_archive_runtime_safe() AND c.qualified AND c.reclaim_enabled AND NOT c.paused AND (c.permit_expires_at IS NULL OR c.permit_expires_at<=clock_timestamp()) AND NOT EXISTS(SELECT 1 FROM session_archive_generations WHERE published_at IS NULL) AND (r.generation_id IS NULL OR NOT r.complete AND r.retry_at<=clock_timestamp()) ORDER BY coalesce(r.retry_at,g.published_at),g.id LIMIT 1), admitted AS (UPDATE session_archive_control SET permit_generation=(SELECT id FROM candidate),permit_token=gen_random_uuid(),permit_kind='maintenance',permit_expires_at=clock_timestamp()+interval '60 seconds',reserved_bytes=0,blocked_reason=NULL WHERE singleton AND EXISTS(SELECT 1 FROM candidate) RETURNING permit_token) SELECT to_jsonb(g),a.permit_token FROM candidate g CROSS JOIN admitted a"
    E.noParams (D.rowMaybe (ReclamationLease <$> column (D.refine (\v -> case fromJSON v of Success x -> Right x; Error e -> Left (T.pack e)) D.jsonb) <*> column D.uuid))

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
    "INSERT INTO session_archive_reclamation(generation_id,session_id,workspace_id,metadata_sha256,verified_at,error_code,retry_at) SELECT g.id,g.session_id,g.workspace_id,g.metadata_sha256,CASE WHEN $5::text IS NULL THEN clock_timestamp() END,$5,clock_timestamp()+CASE WHEN $5::text IS NULL THEN interval '0 seconds' ELSE interval '60 seconds' END FROM session_archives g CROSS JOIN session_archive_control c WHERE c.singleton AND c.permit_kind='maintenance' AND c.permit_generation=$3 AND c.permit_token=$6 AND c.permit_expires_at>clock_timestamp() AND g.workspace_id=$1 AND g.session_id=$2 AND g.id=$3 AND g.metadata_sha256=$4 ON CONFLICT(generation_id) DO UPDATE SET verified_at=EXCLUDED.verified_at,error_code=EXCLUDED.error_code,retry_at=EXCLUDED.retry_at WHERE session_archive_reclamation.metadata_sha256=EXCLUDED.metadata_sha256 AND session_archive_reclamation.deleted_revision=0"
    (param ((.workspaceId) . (.manifest) . fst) E.uuid<>param ((.sessionId) . (.manifest) . fst) E.uuid<>param ((.id) . (.manifest) . fst) E.uuid<>param ((.metadataSha256) . (.manifest) . fst) E.text<>contramap snd (E.param (E.nullable E.text))<>param ((.leaseToken) . fst) E.uuid) D.noResult

reclaimArchiveBatch :: Pool H.Connection -> ReclamationLease -> IO Int64
reclaimArchiveBatch pool lease=runTransaction pool $ S.statement lease $ statement
  "SELECT public.hmem_archive_reclaim($1,$2,$3,$4,$5)"
  (param ((.workspaceId) . (.manifest)) E.uuid<>param ((.sessionId) . (.manifest)) E.uuid<>param ((.id) . (.manifest)) E.uuid<>param ((.metadataSha256) . (.manifest)) E.text<>param (.leaseToken) E.uuid)
  (D.singleRow (column D.int8))

verificationRecorded :: Pool H.Connection -> A.ArchiveManifest -> IO Bool
verificationRecorded pool m=runSession pool $ S.statement m $ statement
  "SELECT EXISTS(SELECT 1 FROM session_archive_reclamation WHERE generation_id=$1 AND metadata_sha256=$2 AND verified_at IS NOT NULL AND error_code IS NULL)"
  (param (.id) E.uuid<>param (.metadataSha256) E.text) (D.singleRow (column D.bool))

verifiedCheckpoint :: Pool H.Connection -> A.ArchiveManifest -> Int64 -> IO (Maybe A.ArchiveCheckpoint)
verifiedCheckpoint pool manifest ordinal=runSession pool $ S.statement (manifest,ordinal) $ statement
  "SELECT c.ordinal,c.raw_offset,c.compressed_offset,c.unused_bits,c.dictionary,c.dictionary_sha256,c.binding_sha256 FROM session_archives g JOIN session_archive_checkpoints c ON c.generation_id=g.id WHERE g.workspace_id=$1 AND g.session_id=$2 AND g.id=$3 AND c.ordinal=$4 AND c.dictionary_sha256=encode(sha256(c.dictionary),'hex') AND c.binding_sha256=hmem_archive_binding(coalesce((SELECT p.binding_sha256 FROM session_archive_checkpoints p WHERE p.generation_id=c.generation_id AND p.ordinal=c.ordinal-1),''),jsonb_build_array(c.generation_id,c.ordinal,c.raw_offset,c.compressed_offset,c.unused_bits,c.dictionary_sha256))"
  (param ((.workspaceId) . fst) E.uuid<>param ((.sessionId) . fst) E.uuid<>param ((.id) . fst) E.uuid<>param snd E.int8)
  (D.rowMaybe (A.ArchiveCheckpoint <$> column D.int8 <*> column D.int8 <*> column D.int8 <*> column D.int2 <*> column D.bytea <*> column D.text <*> column D.text))
