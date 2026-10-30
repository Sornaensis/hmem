-- | Cursor-CAS persistence for the version-one native occurrence projection.
module HMem.DB.SessionOccurrence
  ( available, checkpoint, checkpointSession, saveCheckpoint, publishCheckpoint, nextCandidate ) where

import Data.ByteString (ByteString)
import Data.Functor.Contravariant (contramap)
import Data.Int (Int64)
import Data.Pool (Pool)
import Data.UUID (UUID)
import Hasql.Connection qualified as H
import Hasql.Decoders qualified as D
import Hasql.Encoders qualified as E
import Hasql.Session qualified as S
import Hasql.Statement qualified as St
import HMem.AgentSession.Occurrence
import HMem.DB.Pool (runSession)

param :: (a -> b) -> E.Value b -> E.Params a
param f value = contramap f (E.param (E.nonNullable value))
optional :: (a -> Maybe b) -> E.Value b -> E.Params a
optional f value = contramap f (E.param (E.nullable value))
column :: D.Value a -> D.Row a
column = D.column . D.nonNullable

checkpoint :: Pool H.Connection -> UUID -> UUID -> IO (Maybe Occurrence)
checkpoint pool workspace identifier = runSession pool (checkpointSession workspace identifier)

-- Historical migration qualification runs current code against pre-projection
-- schemas. Such a schema has no derived authority and keeps the legacy null.
available :: Pool H.Connection -> IO Bool
available pool = runSession pool $ S.statement () $ St.Statement
  "SELECT to_regclass('public.session_occurrences') IS NOT NULL" E.noParams (D.singleRow (column D.bool)) True

checkpointSession :: UUID -> UUID -> S.Session (Maybe Occurrence)
checkpointSession workspace identifier = S.statement (workspace,identifier) $ St.Statement
  "SELECT processed_bytes,pending,last_activity_at,source_offset,pi_version FROM session_occurrences WHERE workspace_id=$1 AND session_id=$2 AND derivation_version=1"
  (param fst E.uuid <> param snd E.uuid)
  (D.rowMaybe (Occurrence <$> column D.int8 <*> column D.bytea
    <*> D.column (D.nullable D.timestamptz) <*> D.column (D.nullable D.int8)
    <*> (fmap fromIntegral <$> D.column (D.nullable D.int4)))) True

-- Caller holds the session row lock. A missing row is the zero checkpoint;
-- stale or concurrent work cannot replace a newer checkpoint or version.
saveCheckpoint :: UUID -> UUID -> Int64 -> Occurrence -> S.Session Bool
saveCheckpoint workspace identifier expected state = S.statement (workspace,identifier,expected,state) $ St.Statement
  "WITH saved AS (INSERT INTO session_occurrences(workspace_id,session_id,derivation_version,processed_bytes,pending,last_activity_at,source_offset,pi_version) SELECT $1,$2,1,$4,$5,$6,$7,$8 WHERE $3=0 OR EXISTS(SELECT 1 FROM session_occurrences WHERE workspace_id=$1 AND session_id=$2 AND derivation_version=1 AND processed_bytes=$3) ON CONFLICT(session_id) DO UPDATE SET derivation_version=1,processed_bytes=EXCLUDED.processed_bytes,pending=EXCLUDED.pending,last_activity_at=EXCLUDED.last_activity_at,source_offset=EXCLUDED.source_offset,pi_version=EXCLUDED.pi_version WHERE session_occurrences.workspace_id=$1 AND ((session_occurrences.derivation_version=1 AND session_occurrences.processed_bytes=$3) OR (session_occurrences.derivation_version<>1 AND $3=0)) AND (session_occurrences.derivation_version,session_occurrences.processed_bytes,session_occurrences.pending,session_occurrences.last_activity_at,session_occurrences.source_offset,session_occurrences.pi_version) IS DISTINCT FROM (1,$4,$5,$6,$7,$8) RETURNING 1) SELECT EXISTS(SELECT 1 FROM saved)"
  (param (\(w,_,_,_)->w) E.uuid <> param (\(_,s,_,_)->s) E.uuid <> param (\(_,_,n,_)->n) E.int8
    <> param (\(_,_,_,s)->s.processedBytes) E.int8 <> param (\(_,_,_,s)->s.pending) E.bytea
    <> optional (\(_,_,_,s)->s.lastActivityAt) E.timestamptz <> optional (\(_,_,_,s)->s.sourceOffset) E.int8
    <> optional (\(_,_,_,s)->fromIntegral <$> s.piVersion) E.int4)
  (D.singleRow (column D.bool)) True

-- Inline appends already have one canonical session invalidation. Only a
-- changed complete historical checkpoint needs its own metadata notification.
publishCheckpoint :: UUID -> UUID -> S.Session Bool
publishCheckpoint workspace identifier = S.statement (identifier,workspace) $ St.Statement
  "SELECT hmem_session_occurrence_publish($1,$2)"
  (param fst E.uuid <> param snd E.uuid) (D.singleRow (column D.bool)) True

-- One fair keyset candidate, across active workspaces. A failed candidate is
-- retried next sweep; a large transcript cannot monopolize the worker.
nextCandidate :: Pool H.Connection -> Maybe UUID -> IO (Maybe (UUID,UUID,Bool))
nextCandidate pool cursor = runSession pool $ case cursor of
  Nothing -> S.statement () (St.Statement (sql "") E.noParams decoder True)
  Just identifier -> S.statement identifier (St.Statement (sql " WHERE id>$1") (E.param (E.nonNullable E.uuid)) decoder True)
  where
    decoder = D.rowMaybe ((,,) <$> column D.uuid <*> column D.uuid <*> column D.bool)
    sql :: ByteString -> ByteString
    sql predicate = "WITH candidate AS MATERIALIZED (SELECT id,workspace_id,committed_bytes FROM sessions"<>predicate<>" ORDER BY id LIMIT 1) SELECT s.workspace_id,s.id,w.deleted_at IS NULL AND (o.session_id IS NULL OR o.derivation_version<>1 OR o.processed_bytes<>s.committed_bytes OR octet_length(o.pending)<>0) FROM candidate s JOIN workspaces w ON w.id=s.workspace_id LEFT JOIN session_occurrences o ON o.session_id=s.id"
