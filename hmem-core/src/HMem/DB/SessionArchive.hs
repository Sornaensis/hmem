-- | Bounded archive persistence. The codec worker owns persisted-output
-- verification; publication here checks its attestation and immutable structure.
-- Sealed receipts remain immutable; V051 maintenance owns raw reclamation.
module HMem.DB.SessionArchive
  ( ArchiveJob(..), ArchiveManifest(..), ArchivePart(..), ArchiveCheckpoint(..), ArchiveLedger(..)
  , ArchiveLease(..), ArchiveVerification(..)
  , enqueueArchives, claimArchive, renewArchive, releaseArchive, archiveProgress
  , stageArchivePart, stageArchiveCheckpoint, stageArchiveLedger, publishArchive
  , getArchiveJob, getPublishedArchive, readArchivePart, findArchiveCheckpoint, readArchiveLedger
  , readStagedPart, cleanupArchiveGeneration, cleanupNextArchiveGeneration
  , ArchiveReceipt(..), ArchiveSourceChunk(..), readArchiveReceipt, readArchiveSourceChunk
  , readStagedCheckpoint, readStagedLedger
  ) where

import Control.Exception (throwIO)
import Control.Monad (unless)
import Data.Aeson (FromJSON(..), Value, genericParseJSON, Result(..), fromJSON)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Functor.Contravariant (contramap)
import Data.Int (Int16, Int32, Int64)
import Data.Pool (Pool)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Hasql.Connection qualified as H
import Hasql.Decoders qualified as D
import Hasql.Encoders qualified as E
import Hasql.Session qualified as S
import Hasql.Statement qualified as St

import HMem.AgentSession (sessionSha256)
import HMem.DB.Pool (DBException(..), runSession, runTransaction)
import HMem.Types (jsonOptions)

data ArchiveJob = ArchiveJob
  { sessionId :: !UUID, workspaceId :: !UUID, state :: !Text, attempt :: !Int64
  , generationId :: !(Maybe UUID), leaseToken :: !(Maybe UUID), leaseExpiresAt :: !(Maybe UTCTime)
  , retryAt :: !UTCTime, phase :: !Text, rawProgress :: !Int64, compressedProgress :: !Int64, errorCode :: !(Maybe Text)
  } deriving stock (Show, Eq, Generic)
instance FromJSON ArchiveJob where parseJSON=genericParseJSON jsonOptions

data ArchiveManifest = ArchiveManifest
  { id :: !UUID, sessionId :: !UUID, workspaceId :: !UUID, attempt :: !Int64
  , codec :: !Text, codecImplementation :: !Text, codecParameters :: !Value
  , rawBytes :: !Int64, rawRecords :: !Int64, rawRevision :: !Int64, rawSha256 :: !Text
  , compressedBytes :: !Int64, compressedSha256 :: !Text, partCount :: !Int64
  , checkpointCount :: !Int64, ledgerCount :: !Int64, metadataSha256 :: !Text, publishedAt :: !UTCTime
  } deriving stock (Show, Eq, Generic)
instance FromJSON ArchiveManifest where parseJSON=genericParseJSON jsonOptions

data ArchiveLease = ArchiveLease
  { workspaceId :: !UUID, sessionId :: !UUID, generationId :: !UUID, leaseToken :: !UUID, attempt :: !Int64
  } deriving stock (Show, Eq)

-- | This value is a trusted worker attestation AFTER bounded persisted readback,
-- not a public client input. SQL cannot establish gzip termination or raw SHA.
data ArchiveVerification = ArchiveVerification
  { compressedBytes :: !Int64, compressedSha256 :: !Text, partCount :: !Int64
  , checkpointCount :: !Int64, ledgerCount :: !Int64
  } deriving stock (Show, Eq)

data ArchivePart = ArchivePart
  { ordinal :: !Int64, startOffset :: !Int64, endOffset :: !Int64, bytes :: !ByteString
  , dataSha256 :: !Text, bindingSha256 :: !Text
  } deriving stock (Show, Eq)
data ArchiveCheckpoint = ArchiveCheckpoint
  { ordinal :: !Int64, rawOffset :: !Int64, compressedOffset :: !Int64, unusedBits :: !Int16
  , dictionary :: !ByteString, dictionarySha256 :: !Text, bindingSha256 :: !Text
  } deriving stock (Show, Eq)
data ArchiveLedger = ArchiveLedger
  { revision :: !Int64, startOffset :: !Int64, endOffset :: !Int64, allowFinalRecord :: !Bool
  , recordCount :: !Int64, endsWithNewline :: !Bool, rawSha256 :: !Text, bindingSha256 :: !Text
  } deriving stock (Show, Eq)

data ArchiveReceipt = ArchiveReceipt
  { rawBytes :: !Int64, rawRecords :: !Int64, rawRevision :: !Int64, rawSha256 :: !Text
  } deriving stock (Show, Eq)
data ArchiveSourceChunk = ArchiveSourceChunk
  { revision :: !Int64, startOffset :: !Int64, bytes :: !ByteString
  , allowFinalRecord :: !Bool, recordCount :: !Int64, endsWithNewline :: !Bool
  } deriving stock (Show, Eq)

param :: (a -> b) -> E.Value b -> E.Params a
param f v=contramap f (E.param (E.nonNullable v))
pair :: E.Params (UUID,UUID)
pair=param fst E.uuid<>param snd E.uuid
leaseParams :: E.Params ArchiveLease
leaseParams=param (.workspaceId) E.uuid<>param (.sessionId) E.uuid<>param (.generationId) E.uuid<>param (.leaseToken) E.uuid<>param (.attempt) E.int8
column :: D.Value a -> D.Row a
column=D.column . D.nonNullable
jsonRow :: FromJSON a => D.Row a
jsonRow=column $ D.refine (\v -> case fromJSON v of Success x -> Right x; Error e -> Left (T.pack e)) D.jsonb
statement :: ByteString -> E.Params p -> D.Result r -> St.Statement p r
statement sql enc dec=St.Statement sql enc dec True
guardInput :: Bool -> IO ()
guardInput valid=unless valid (throwIO (DBCheckViolation "Invalid bounded archive input"))

-- | Keyset scan allows a crash between seal and enqueue. At most 100 sessions;
-- pass the last (terminated_at,id) returned, then restart after an exhausted sweep.
enqueueArchives :: Pool H.Connection -> Maybe (UTCTime,UUID) -> Int -> IO [(UTCTime,UUID)]
enqueueArchives pool cursor limit=do
  guardInput (limit>=1 && limit<=100)
  runTransaction pool $ S.statement (fmap fst cursor,fmap snd cursor,fromIntegral limit::Int32) $ statement
    "WITH candidates AS MATERIALIZED (SELECT s.terminated_at,s.id,s.workspace_id FROM sessions s JOIN workspaces w ON w.id=s.workspace_id WHERE s.state='terminated' AND w.deleted_at IS NULL AND ($1::timestamptz IS NULL OR (s.terminated_at,s.id)>($1,$2::uuid)) ORDER BY s.terminated_at,s.id LIMIT $3), inserted AS (INSERT INTO session_archive_jobs(session_id,workspace_id) SELECT id,workspace_id FROM candidates ON CONFLICT DO NOTHING) SELECT terminated_at,id FROM candidates ORDER BY terminated_at,id"
    (contramap (\(a,_,_)->a) (E.param (E.nullable E.timestamptz))<>contramap (\(_,b,_)->b) (E.param (E.nullable E.uuid))<>param (\(_,_,c)->c) E.int4)
    (D.rowList ((,) <$> column D.timestamptz <*> column D.uuid))

claimArchive :: Pool H.Connection -> Text -> IO (Maybe ArchiveLease)
claimArchive pool implementation=do
  guardInput (not (T.null implementation) && T.length implementation<=128)
  runTransaction pool $ do
    lockControl
    -- Expiry detaches only one fenced attempt. Its staging must drain before
    -- any replacement can reserve another attempt's storage.
    S.statement () (statement
      "WITH expired AS (SELECT session_id FROM session_archive_jobs WHERE state='running' AND lease_expires_at<=clock_timestamp() ORDER BY session_id LIMIT 1 FOR UPDATE) UPDATE session_archive_jobs j SET state='retry',generation_id=NULL,lease_token=NULL,lease_expires_at=NULL,phase='queued',raw_progress=0,compressed_progress=0,error_code='archive_lease_expired' FROM expired e WHERE j.session_id=e.session_id"
      E.noParams D.noResult)
    S.statement () (statement
      "UPDATE session_archive_control c SET blocked_reason=CASE WHEN paused THEN 'paused' WHEN EXISTS(SELECT 1 FROM session_archive_generations WHERE published_at IS NULL AND id IS DISTINCT FROM c.permit_generation) THEN 'staging_draining' WHEN permit_expires_at>clock_timestamp() THEN 'attempt_running' WHEN EXISTS(SELECT 1 FROM session_archive_jobs j JOIN sessions s ON s.id=j.session_id JOIN workspaces w ON w.id=j.workspace_id WHERE w.deleted_at IS NULL AND j.state IN ('queued','retry') AND j.retry_at<=clock_timestamp() AND (c.backfill_enabled OR s.terminated_at>=c.cutover_at OR EXISTS(SELECT 1 FROM session_archive_requests demand WHERE demand.session_id=s.id AND demand.workspace_id=s.workspace_id)) AND (hmem_archive_reservation(s.final_bytes,s.revision-1)+c.staged_bytes>c.staging_budget OR hmem_archive_reservation(s.final_bytes,s.revision-1)*4>c.wal_budget)) THEN 'resource_blocked' ELSE NULL END WHERE singleton"
      E.noParams D.noResult)
    claimed <- S.statement () $ statement
      "WITH candidate AS (SELECT j.session_id FROM session_archive_jobs j JOIN workspaces w ON w.id=j.workspace_id JOIN sessions s ON s.id=j.session_id CROSS JOIN session_archive_control c WHERE c.singleton AND (c.blocked_reason IS NULL OR c.blocked_reason='resource_blocked') AND hmem_archive_reservation(s.final_bytes,s.revision-1)+c.staged_bytes<=c.staging_budget AND hmem_archive_reservation(s.final_bytes,s.revision-1)*4<=c.wal_budget AND w.deleted_at IS NULL AND j.state IN ('queued','retry') AND j.retry_at<=clock_timestamp() AND (c.backfill_enabled OR s.terminated_at>=c.cutover_at OR EXISTS(SELECT 1 FROM session_archive_requests demand WHERE demand.session_id=s.id AND demand.workspace_id=s.workspace_id)) ORDER BY j.retry_at,j.session_id FOR UPDATE OF j SKIP LOCKED LIMIT 1) UPDATE session_archive_jobs j SET state='running',attempt=attempt+1,generation_id=gen_random_uuid(),lease_token=gen_random_uuid(),lease_expires_at=clock_timestamp()+interval '60 seconds',phase='compressing',raw_progress=0,compressed_progress=0,error_code=NULL FROM candidate c WHERE j.session_id=c.session_id RETURNING j.workspace_id,j.session_id,j.generation_id,j.lease_token,j.attempt"
      E.noParams (D.rowMaybe (ArchiveLease <$> column D.uuid <*> column D.uuid <*> column D.uuid <*> column D.uuid <*> column D.int8))
    case claimed of
      Nothing -> pure Nothing
      Just lease -> do
        S.statement (lease,implementation) $ statement
          "INSERT INTO session_archive_generations(id,workspace_id,session_id,attempt,lease_token,codec,codec_implementation,codec_parameters,raw_bytes,raw_records,raw_revision,raw_sha256) SELECT $3,$1,$2,$5,$4,'gzip-v1',$6,'{\"level\":6,\"window_bits\":31,\"memory_level\":8,\"strategy\":\"default\",\"mtime\":0,\"os\":255,\"checkpoint_raw_bytes\":1048576}',final_bytes,committed_records,revision,final_sha256 FROM sessions WHERE workspace_id=$1 AND id=$2"
          (contramap fst leaseParams<>param snd E.text) D.noResult
        pure (Just lease)

-- All worker transactions use control -> job -> generation/child row.
lockControl :: S.Session ()
lockControl=S.statement () $ statement "SELECT singleton FROM session_archive_control WHERE singleton FOR UPDATE" E.noParams (() <$ D.singleRow (column D.bool))

withLease :: Pool H.Connection -> ArchiveLease -> S.Session a -> IO (Maybe a)
withLease pool lease action=runTransaction pool $ do
  lockControl
  locked <- S.statement lease $ statement "SELECT session_id FROM session_archive_jobs WHERE workspace_id=$1 AND session_id=$2 FOR UPDATE"
    (contramap (\x->(x.workspaceId,x.sessionId)) pair) (D.rowMaybe (column D.uuid))
  case locked of
    Nothing -> pure Nothing
    Just _ -> do
      live <- S.statement lease $ statement
        "SELECT coalesce(j.state='running' AND j.generation_id=$3 AND j.lease_token=$4 AND j.attempt=$5 AND j.lease_expires_at>clock_timestamp() AND c.permit_kind='compression' AND c.permit_generation=$3 AND c.permit_token=$4 AND c.permit_expires_at>clock_timestamp(),false) FROM session_archive_jobs j CROSS JOIN session_archive_control c WHERE c.singleton AND j.workspace_id=$1 AND j.session_id=$2"
        leaseParams (D.singleRow (column D.bool))
      if live then Just <$> action else pure Nothing

renewArchive :: Pool H.Connection -> ArchiveLease -> IO Bool
renewArchive pool lease=fmap (maybe False (const True)) $ withLease pool lease $ S.statement lease $ statement
  "UPDATE session_archive_jobs SET lease_expires_at=clock_timestamp()+interval '60 seconds' WHERE workspace_id=$1 AND session_id=$2 AND generation_id=$3 AND lease_token=$4 AND attempt=$5"
  leaseParams D.noResult

releaseArchive :: Pool H.Connection -> ArchiveLease -> Bool -> Text -> IO Bool
releaseArchive pool lease terminal code=do
  guardInput (T.length code<=64 && not (T.null code))
  fmap (maybe False (const True)) $ withLease pool lease $ S.statement (lease,terminal,code) $ statement
    "UPDATE session_archive_jobs SET state=CASE WHEN $6 THEN 'failed' ELSE 'retry' END,generation_id=NULL,lease_token=NULL,lease_expires_at=NULL,retry_at=clock_timestamp()+least(60,power(2,least(attempt-1,6))) * interval '1 second',phase='queued',raw_progress=0,compressed_progress=0,error_code=$7 WHERE workspace_id=$1 AND session_id=$2 AND generation_id=$3 AND lease_token=$4 AND attempt=$5"
    (contramap (\(l,_,_)->l) leaseParams<>param (\(_,b,_)->b) E.bool<>param (\(_,_,c)->c) E.text) D.noResult

archiveProgress :: Pool H.Connection -> ArchiveLease -> Text -> Int64 -> Int64 -> IO Bool
archiveProgress pool lease phase raw compressed=do
  guardInput (phase `elem` ["compressing","verifying"] && raw>=0 && compressed>=0)
  fmap (maybe False (const True)) $ withLease pool lease $ S.statement (lease,phase,raw,compressed) $ statement
    "UPDATE session_archive_jobs SET phase=$6,raw_progress=$7,compressed_progress=$8 WHERE workspace_id=$1 AND session_id=$2 AND generation_id=$3 AND lease_token=$4 AND attempt=$5"
    (contramap (\(l,_,_,_)->l) leaseParams<>param (\(_,b,_,_)->b) E.text<>param (\(_,_,c,_)->c) E.int8<>param (\(_,_,_,d)->d) E.int8) D.noResult

stageArchivePart :: Pool H.Connection -> ArchiveLease -> Int64 -> Int64 -> ByteString -> IO Bool
stageArchivePart pool lease ordinal offset bytes=do
  guardInput (ordinal>=0 && offset>=0 && not (BS.null bytes) && BS.length bytes<=1048576 && offset<=maxBound-fromIntegral (BS.length bytes))
  fmap (maybe False (const True)) $ withLease pool lease $ S.statement (lease,ordinal,offset,bytes,sessionSha256 bytes) $ statement
    "INSERT INTO session_archive_parts(generation_id,session_id,workspace_id,ordinal,start_offset,end_offset,data,data_sha256,binding_sha256) VALUES($3,$2,$1,$6,$7,$7+octet_length($8::bytea),$8,$9,'')"
    (contramap (\(l,_,_,_,_)->l) leaseParams<>param (\(_,o,_,_,_)->o) E.int8<>param (\(_,_,s,_,_)->s) E.int8<>param (\(_,_,_,b,_)->b) E.bytea<>param (\(_,_,_,_,h)->h) E.text) D.noResult

stageArchiveCheckpoint :: Pool H.Connection -> ArchiveLease -> Int64 -> Int64 -> Int64 -> Int16 -> ByteString -> IO Bool
stageArchiveCheckpoint pool lease ordinal raw compressed bits dictionary=do
  guardInput (ordinal>=0 && raw>=0 && compressed>=10 && bits>=0 && bits<=7 && BS.length dictionary<=32768)
  fmap (maybe False (const True)) $ withLease pool lease $ S.statement (lease,ordinal,raw,compressed,bits,dictionary,sessionSha256 dictionary) $ statement
    "INSERT INTO session_archive_checkpoints(generation_id,session_id,workspace_id,ordinal,raw_offset,compressed_offset,unused_bits,dictionary,dictionary_sha256,binding_sha256) VALUES($3,$2,$1,$6,$7,$8,$9,$10,$11,'')"
    (contramap (\(l,_,_,_,_,_,_)->l) leaseParams<>param (\(_,o,_,_,_,_,_)->o) E.int8<>param (\(_,_,r,_,_,_,_)->r) E.int8<>param (\(_,_,_,c,_,_,_)->c) E.int8<>param (\(_,_,_,_,b,_,_)->b) E.int2<>param (\(_,_,_,_,_,d,_)->d) E.bytea<>param (\(_,_,_,_,_,_,h)->h) E.text) D.noResult

-- At most eight original chunks (8 MiB of hashing) per transaction. Cursor is
-- the last original revision copied; the raw source remains immutable.
stageArchiveLedger :: Pool H.Connection -> ArchiveLease -> Int64 -> Int -> IO (Maybe Int64)
stageArchiveLedger pool lease after limit=do
  guardInput (after>=0 && limit>=1 && limit<=8)
  withLease pool lease $ S.statement (lease,after,fromIntegral limit::Int32) $ statement
    "WITH copied AS (INSERT INTO session_archive_ledger(generation_id,session_id,workspace_id,revision,start_offset,end_offset,allow_final_record,record_count,ends_with_newline,raw_sha256,binding_sha256) SELECT $3,$2,$1,revision,start_offset,start_offset+octet_length(data),allow_final_record,record_count,ends_with_newline,encode(sha256(data),'hex'),'' FROM session_chunks WHERE session_id=$2 AND revision>$6 ORDER BY revision LIMIT $7 RETURNING revision) SELECT coalesce(max(revision),$6) FROM copied"
    (contramap (\(l,_,_)->l) leaseParams<>param (\(_,r,_)->r) E.int8<>param (\(_,_,n)->n) E.int4) (D.singleRow (column D.int8))

publishArchive :: Pool H.Connection -> ArchiveLease -> ArchiveVerification -> IO Bool
publishArchive pool lease verified=do
  guardInput (verified.compressedBytes>=20 && verified.partCount>0 && verified.checkpointCount>0 && verified.ledgerCount>=0)
  outcome <- withLease pool lease $ S.statement (lease,verified) $ statement
    "UPDATE session_archive_generations g SET published_at=clock_timestamp(),compressed_bytes=$6,compressed_sha256=$7,part_count=$8,checkpoint_count=$9,ledger_count=$10,metadata_sha256=hmem_archive_binding('',jsonb_build_array(g.id,g.session_id,g.workspace_id,g.codec,g.codec_implementation,g.codec_parameters,g.raw_bytes,g.raw_records,g.raw_revision,g.raw_sha256,$6::bigint,$7::text,$8::bigint,$9::bigint,$10::bigint,(SELECT binding_sha256 FROM session_archive_parts WHERE generation_id=g.id ORDER BY ordinal DESC LIMIT 1),(SELECT binding_sha256 FROM session_archive_checkpoints WHERE generation_id=g.id ORDER BY ordinal DESC LIMIT 1),coalesce((SELECT binding_sha256 FROM session_archive_ledger WHERE generation_id=g.id ORDER BY revision DESC LIMIT 1),''))) WHERE g.workspace_id=$1 AND g.session_id=$2 AND g.id=$3 AND g.lease_token=$4 AND g.attempt=$5"
    (contramap fst leaseParams<>param ((.compressedBytes) . snd) E.int8<>param ((.compressedSha256) . snd) E.text<>param ((.partCount) . snd) E.int8<>param ((.checkpointCount) . snd) E.int8<>param ((.ledgerCount) . snd) E.int8) D.noResult
  case outcome of
    Just () -> pure True
    Nothing -> do
      existing <- getPublishedArchive pool lease.workspaceId lease.sessionId
      pure $ case existing of
        Just m -> m.id==lease.generationId && m.attempt==lease.attempt && m.compressedBytes==verified.compressedBytes && m.compressedSha256==verified.compressedSha256 && m.partCount==verified.partCount && m.checkpointCount==verified.checkpointCount && m.ledgerCount==verified.ledgerCount
        _ -> False

getArchiveJob :: Pool H.Connection -> UUID -> UUID -> IO (Maybe ArchiveJob)
getArchiveJob pool workspace session=runSession pool $ S.statement (workspace,session) $ statement
  "SELECT to_jsonb(j) FROM session_archive_jobs j WHERE workspace_id=$1 AND session_id=$2" pair (D.rowMaybe jsonRow)
getPublishedArchive :: Pool H.Connection -> UUID -> UUID -> IO (Maybe ArchiveManifest)
getPublishedArchive pool workspace session=runSession pool $ S.statement (workspace,session) $ statement
  "SELECT to_jsonb(g) FROM session_archives g WHERE workspace_id=$1 AND session_id=$2" pair (D.rowMaybe jsonRow)

partRow :: D.Row ArchivePart
partRow=ArchivePart <$> column D.int8 <*> column D.int8 <*> column D.int8 <*> column D.bytea <*> column D.text <*> column D.text
readArchivePart :: Pool H.Connection -> UUID -> UUID -> UUID -> Int64 -> IO (Maybe ArchivePart)
readArchivePart pool workspace session generation ordinal=do
  guardInput (ordinal>=0)
  runSession pool $ S.statement (workspace,session,generation,ordinal) $ statement
    "SELECT p.ordinal,p.start_offset,p.end_offset,p.data,p.data_sha256,p.binding_sha256 FROM session_archives g JOIN session_archive_parts p ON p.generation_id=g.id WHERE g.workspace_id=$1 AND g.session_id=$2 AND g.id=$3 AND p.ordinal=$4"
    readParams (D.rowMaybe partRow)
readParams :: E.Params (UUID,UUID,UUID,Int64)
readParams=param (\(w,_,_,_)->w) E.uuid<>param (\(_,s,_,_)->s) E.uuid<>param (\(_,_,g,_)->g) E.uuid<>param (\(_,_,_,o)->o) E.int8

-- Staged readback is explicitly fenced and cannot be used by ordinary readers.
readStagedPart :: Pool H.Connection -> ArchiveLease -> Int64 -> IO (Maybe ArchivePart)
readStagedPart pool lease ordinal=do
  guardInput (ordinal>=0)
  result <- withLease pool lease $ S.statement (lease,ordinal) $ statement
    "SELECT p.ordinal,p.start_offset,p.end_offset,p.data,p.data_sha256,p.binding_sha256 FROM session_archive_parts p WHERE p.workspace_id=$1 AND p.session_id=$2 AND p.generation_id=$3 AND p.ordinal=$6 AND $4::uuid IS NOT NULL AND $5::bigint>0 AND p.data_sha256=encode(sha256(p.data),'hex') AND p.binding_sha256=hmem_archive_binding(coalesce((SELECT q.binding_sha256 FROM session_archive_parts q WHERE q.generation_id=p.generation_id AND q.ordinal=p.ordinal-1),''),jsonb_build_array(p.generation_id,p.ordinal,p.start_offset,p.end_offset,p.data_sha256))"
    (contramap fst leaseParams<>param snd E.int8) (D.rowMaybe partRow)
  pure (result >>= id)

-- Worker-only input/readback operations keep one original chunk, part or index
-- row in memory. Each call releases the job lock and pool connection on return.
readArchiveReceipt :: Pool H.Connection -> ArchiveLease -> IO (Maybe ArchiveReceipt)
readArchiveReceipt pool lease=withLease pool lease $ S.statement lease $ statement
  "SELECT raw_bytes,raw_records,raw_revision,raw_sha256 FROM session_archive_generations WHERE workspace_id=$1 AND session_id=$2 AND id=$3 AND lease_token=$4 AND attempt=$5"
  leaseParams (D.singleRow (ArchiveReceipt <$> column D.int8 <*> column D.int8 <*> column D.int8 <*> column D.text))

readArchiveSourceChunk :: Pool H.Connection -> ArchiveLease -> Int64 -> IO (Maybe ArchiveSourceChunk)
readArchiveSourceChunk pool lease offset=do
  guardInput (offset>=0)
  result<-withLease pool lease $ S.statement (lease,offset) $ statement
    "SELECT c.revision,c.start_offset,c.data,c.allow_final_record,c.record_count,c.ends_with_newline FROM session_chunks c JOIN sessions s ON s.id=c.session_id WHERE s.workspace_id=$1 AND s.id=$2 AND c.start_offset>=$6 AND $3::uuid IS NOT NULL AND $4::uuid IS NOT NULL AND $5::bigint>0 ORDER BY c.start_offset LIMIT 1"
    (contramap fst leaseParams<>param snd E.int8)
    (D.rowMaybe (ArchiveSourceChunk <$> column D.int8 <*> column D.int8 <*> column D.bytea <*> column D.bool <*> column D.int8 <*> column D.bool))
  pure (result >>= id)

readStagedCheckpoint :: Pool H.Connection -> ArchiveLease -> Int64 -> IO (Maybe ArchiveCheckpoint)
readStagedCheckpoint pool lease ordinal=do
  guardInput (ordinal>=0)
  result<-withLease pool lease $ S.statement (lease,ordinal) $ statement
    "SELECT c.ordinal,c.raw_offset,c.compressed_offset,c.unused_bits,c.dictionary,c.dictionary_sha256,c.binding_sha256 FROM session_archive_checkpoints c WHERE c.workspace_id=$1 AND c.session_id=$2 AND c.generation_id=$3 AND c.ordinal=$6 AND $4::uuid IS NOT NULL AND $5::bigint>0 AND c.dictionary_sha256=encode(sha256(c.dictionary),'hex') AND c.binding_sha256=hmem_archive_binding(coalesce((SELECT p.binding_sha256 FROM session_archive_checkpoints p WHERE p.generation_id=c.generation_id AND p.ordinal=c.ordinal-1),''),jsonb_build_array(c.generation_id,c.ordinal,c.raw_offset,c.compressed_offset,c.unused_bits,c.dictionary_sha256))"
    (contramap fst leaseParams<>param snd E.int8)
    (D.rowMaybe (ArchiveCheckpoint <$> column D.int8 <*> column D.int8 <*> column D.int8 <*> column D.int2 <*> column D.bytea <*> column D.text <*> column D.text))
  pure (result >>= id)

readStagedLedger :: Pool H.Connection -> ArchiveLease -> Int64 -> IO (Maybe ArchiveLedger)
readStagedLedger pool lease revision=do
  guardInput (revision>=1)
  result<-withLease pool lease $ S.statement (lease,revision) $ statement
    "SELECT l.revision,l.start_offset,l.end_offset,l.allow_final_record,l.record_count,l.ends_with_newline,l.raw_sha256,l.binding_sha256 FROM session_archive_ledger l WHERE l.workspace_id=$1 AND l.session_id=$2 AND l.generation_id=$3 AND l.revision=$6 AND $4::uuid IS NOT NULL AND $5::bigint>0 AND l.binding_sha256=hmem_archive_binding(coalesce((SELECT p.binding_sha256 FROM session_archive_ledger p WHERE p.generation_id=l.generation_id AND p.revision=l.revision-1),''),jsonb_build_array(l.generation_id,l.revision,l.start_offset,l.end_offset,l.allow_final_record,l.record_count,l.ends_with_newline,l.raw_sha256))"
    (contramap fst leaseParams<>param snd E.int8)
    (D.rowMaybe (ArchiveLedger <$> column D.int8 <*> column D.int8 <*> column D.int8 <*> column D.bool <*> column D.int8 <*> column D.bool <*> column D.text <*> column D.text))
  pure (result >>= id)

findArchiveCheckpoint :: Pool H.Connection -> UUID -> UUID -> UUID -> Int64 -> IO (Maybe ArchiveCheckpoint)
findArchiveCheckpoint pool workspace session generation raw=do
  guardInput (raw>=0)
  runSession pool $ S.statement (workspace,session,generation,raw) $ statement
    "SELECT c.ordinal,c.raw_offset,c.compressed_offset,c.unused_bits,c.dictionary,c.dictionary_sha256,c.binding_sha256 FROM session_archives g JOIN session_archive_checkpoints c ON c.generation_id=g.id WHERE g.workspace_id=$1 AND g.session_id=$2 AND g.id=$3 AND c.raw_offset<=$4 ORDER BY c.raw_offset DESC LIMIT 1"
    readParams (D.rowMaybe (ArchiveCheckpoint <$> column D.int8 <*> column D.int8 <*> column D.int8 <*> column D.int2 <*> column D.bytea <*> column D.text <*> column D.text))

-- One original chunk by offset, for later complete-chunk verification/slicing.
readArchiveLedger :: Pool H.Connection -> UUID -> UUID -> UUID -> Int64 -> IO (Maybe ArchiveLedger)
readArchiveLedger pool workspace session generation offset=do
  guardInput (offset>=0)
  runSession pool $ S.statement (workspace,session,generation,offset) $ statement
    "SELECT l.revision,l.start_offset,l.end_offset,l.allow_final_record,l.record_count,l.ends_with_newline,l.raw_sha256,l.binding_sha256 FROM session_archives g JOIN session_archive_ledger l ON l.generation_id=g.id WHERE g.workspace_id=$1 AND g.session_id=$2 AND g.id=$3 AND l.start_offset<=$4 AND l.end_offset>$4 ORDER BY l.start_offset DESC LIMIT 1"
    readParams (D.rowMaybe (ArchiveLedger <$> column D.int8 <*> column D.int8 <*> column D.int8 <*> column D.bool <*> column D.int8 <*> column D.bool <*> column D.text <*> column D.text))

-- Delete at most eight rows total from one obsolete staging generation. The
-- job/generation locks exclude publication or claim; raw evidence is untouched.
cleanupArchiveGeneration :: Pool H.Connection -> UUID -> UUID -> UUID -> IO Int64
cleanupArchiveGeneration pool workspace session generation=runTransaction pool $ do
  lockControl
  _ <- S.statement (workspace,session) $ statement "SELECT session_id FROM session_archive_jobs WHERE workspace_id=$1 AND session_id=$2 FOR UPDATE" pair (D.rowMaybe (column D.uuid))
  abandoned <- S.statement (workspace,session,generation,0) $ statement
    "SELECT g.id FROM session_archive_generations g JOIN session_archive_jobs j ON j.session_id=g.session_id WHERE g.workspace_id=$1 AND g.session_id=$2 AND g.id=$3 AND g.published_at IS NULL AND j.generation_id IS DISTINCT FROM g.id AND $4::bigint=0 FOR UPDATE OF g"
    readParams (D.rowMaybe (column D.uuid))
  case abandoned of
    Nothing -> pure 0
    Just _ -> do
      count <- S.statement generation $ statement
        "WITH p AS (DELETE FROM session_archive_parts WHERE (generation_id,ordinal) IN (SELECT generation_id,ordinal FROM session_archive_parts WHERE generation_id=$1 ORDER BY ordinal LIMIT 8) RETURNING 1), c AS (DELETE FROM session_archive_checkpoints WHERE (generation_id,ordinal) IN (SELECT generation_id,ordinal FROM session_archive_checkpoints WHERE generation_id=$1 ORDER BY ordinal LIMIT (8-(SELECT count(*) FROM p))) RETURNING 1), l AS (DELETE FROM session_archive_ledger WHERE (generation_id,revision) IN (SELECT generation_id,revision FROM session_archive_ledger WHERE generation_id=$1 ORDER BY revision LIMIT (8-(SELECT count(*) FROM p)-(SELECT count(*) FROM c))) RETURNING 1) SELECT (SELECT count(*) FROM p)+(SELECT count(*) FROM c)+(SELECT count(*) FROM l)"
        (E.param (E.nonNullable E.uuid)) (D.singleRow (column D.int8))
      if count/=0 then pure count else do
        S.statement generation $ statement "DELETE FROM session_archive_generations WHERE id=$1" (E.param (E.nonNullable E.uuid)) D.noResult
        pure 1

-- A single obsolete generation and at most eight child rows per fair worker
-- turn. Selection grants no deletion authority; the existing locked primitive
-- rechecks current/published ownership before deleting anything.
cleanupNextArchiveGeneration :: Pool H.Connection -> IO Int64
cleanupNextArchiveGeneration pool=do
  candidate<-runSession pool $ S.statement () $ statement
    "SELECT g.workspace_id,g.session_id,g.id FROM session_archive_generations g JOIN session_archive_jobs j ON j.session_id=g.session_id WHERE g.published_at IS NULL AND j.generation_id IS DISTINCT FROM g.id ORDER BY g.created_at,g.id LIMIT 1"
    E.noParams (D.rowMaybe ((,,) <$> column D.uuid <*> column D.uuid <*> column D.uuid))
  case candidate of Nothing -> pure 0; Just (w,s,g) -> cleanupArchiveGeneration pool w s g
