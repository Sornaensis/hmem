-- | Canonical transactional persistence for immutable JSONL evidence.
module HMem.DB.AgentSession
  ( createSession, getSession, listWorkspaceSessions, listTaskSessions, listSessionTasks
  , listWorkspaceSessionsFiltered, listTaskSessionsFiltered
  , appendSession, heartbeatSession, associateSessionTask, terminateSession, readSessionRange
  ) where

import Control.Exception (throwIO)
import Crypto.Hash (Context, Digest, SHA256, hashInit, hashUpdate, hashFinalize)
import Data.Aeson (FromJSON, Result(..), fromJSON)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Functor.Contravariant (contramap)
import Data.Int (Int32, Int64)
import Data.Pool (Pool)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time (UTCTime)
import Data.UUID (UUID)
import Hasql.Connection qualified as Hasql
import Hasql.Decoders qualified as D
import Hasql.Encoders qualified as E
import Hasql.Session qualified as S
import Hasql.Statement qualified as St

import HMem.AgentSession
import HMem.DB.Pool (runSession, runTransaction)
import HMem.DB.RequestContext (EffectiveActor(..), actorTypeToText, currentEffectiveActor)

problem :: SessionErrorCode -> Text -> Either SessionException a
problem code message = Left (SessionException code message)

checked :: Pool Hasql.Connection -> S.Session (Either SessionException a) -> IO a
checked pool action = runTransaction pool action >>= either throwIO pure

invalid :: Text -> IO a
invalid = throwIO . SessionException SessionInvalidInput

param :: (a -> b) -> E.Value b -> E.Params a
param field value = contramap field (E.param (E.nonNullable value))

optional :: (a -> Maybe b) -> E.Value b -> E.Params a
optional field value = contramap field (E.param (E.nullable value))

jsonRow :: FromJSON a => D.Row a
jsonRow = D.column $ D.nonNullable $ D.refine parse D.jsonb
  where parse value = case fromJSON value of
          Success row -> Right row
          Error message -> Left (T.pack message)

pairParams :: E.Params (UUID, UUID)
pairParams = param fst E.uuid <> param snd E.uuid

sessionSelect :: ByteString
sessionSelect = "SELECT to_jsonb(s) FROM sessions s JOIN workspaces w ON w.id=s.workspace_id WHERE s.workspace_id=$1 AND s.id=$2 AND w.deleted_at IS NULL"

getStatement :: Bool -> St.Statement (UUID,UUID) (Maybe AgentSession)
getStatement locked = St.Statement (sessionSelect <> if locked then " FOR UPDATE OF s" else "") pairParams (D.rowMaybe jsonRow) True

createSession :: Pool Hasql.Connection -> CreateSession -> IO AgentSession
createSession pool input = do
  if BS.length (TE.encodeUtf8 input.formatLabel) > 128 || T.null (T.strip input.formatLabel)
    then invalid "format_label must contain 1 to 128 UTF-8 bytes"
    else pure ()
  actor <- currentEffectiveActor
  checked pool $ do
    inserted <- S.statement input createStatement
    existing <- case inserted of
      Just row -> pure (Just row)
      Nothing -> S.statement (input.workspaceId,input.id) (getStatement True)
    pure $ case existing of
      Nothing -> problem SessionConflict "Session UUID already exists in another workspace, or workspace is inactive"
      Just row
        | row.formatLabel == input.formatLabel && sameCreator actor row -> Right row
        | otherwise -> problem SessionConflict "Session UUID was already created with different immutable metadata"
  where
    sameCreator Nothing row = row.creatorActorType == Nothing && row.creatorActorId == Nothing
      && row.creatorActorLabel == Nothing && row.creatorBotIdentity == Nothing
    sameCreator (Just actor) row = row.creatorActorType == Just (actorTypeToText actor.actorType)
      && row.creatorActorId == Just actor.actorId && row.creatorActorLabel == Just actor.actorLabel
      && row.creatorBotIdentity == actor.botIdentity

createStatement :: St.Statement CreateSession (Maybe AgentSession)
createStatement = St.Statement sql encoder (D.rowMaybe jsonRow) True
  where
    sql = "WITH workspace AS (SELECT id FROM workspaces WHERE id=$2 AND deleted_at IS NULL FOR SHARE), inserted AS (INSERT INTO sessions(id,workspace_id,format_label,creator_actor_type,creator_actor_id,creator_actor_label,creator_bot_identity) SELECT $1,id,$3,NULLIF(current_setting('hmem.actor_type',true),''),NULLIF(current_setting('hmem.actor_id',true),''),NULLIF(current_setting('hmem.actor_label',true),''),NULLIF(current_setting('hmem.bot_identity',true),'')::jsonb FROM workspace ON CONFLICT(id) DO NOTHING RETURNING *) SELECT to_jsonb(inserted) FROM inserted"
    encoder = param (.id) E.uuid <> param (.workspaceId) E.uuid <> param (.formatLabel) E.text

getSession :: Pool Hasql.Connection -> UUID -> UUID -> IO (Maybe AgentSession)
getSession pool workspace identifier = runSession pool $ S.statement (workspace,identifier) (getStatement False)

validateLimit :: Int -> IO ()
validateLimit limit
  | limit < 1 || limit > maxSessionPageSize = invalid "Session page limit must be between 1 and 100"
  | otherwise = pure ()

listWorkspaceSessions :: Pool Hasql.Connection -> UUID -> Maybe SessionPageCursor -> Int -> IO SessionPage
listWorkspaceSessions pool workspace = listWorkspaceSessionsFiltered pool workspace defaultSessionFilter

listWorkspaceSessionsFiltered :: Pool Hasql.Connection -> UUID -> SessionFilter -> Maybe SessionPageCursor -> Int -> IO SessionPage
listWorkspaceSessionsFiltered pool workspace filters = listSessions pool workspace Nothing filters

listTaskSessions :: Pool Hasql.Connection -> UUID -> UUID -> Maybe SessionPageCursor -> Int -> IO SessionPage
listTaskSessions pool workspace task = listTaskSessionsFiltered pool workspace task defaultSessionFilter

listTaskSessionsFiltered :: Pool Hasql.Connection -> UUID -> UUID -> SessionFilter -> Maybe SessionPageCursor -> Int -> IO SessionPage
listTaskSessionsFiltered pool workspace task filters = listSessions pool workspace (Just task) filters

listSessions :: Pool Hasql.Connection -> UUID -> Maybe UUID -> SessionFilter -> Maybe SessionPageCursor -> Int -> IO SessionPage
listSessions pool workspace task filters cursor limit = do
  validateLimit limit
  rows <- runSession pool $ S.statement
    (workspace, fmap (.createdAt) cursor, fmap (.id) cursor, task, fromIntegral (limit+1),
      fmap (\case Active -> "active"; Terminated -> "terminated") filters.state,
      fmap (\case Fresh -> True; Stale -> False) filters.freshness) listStatement
  let items = take limit rows
      nextCursor = if length rows > limit then case reverse items of
        row:_ -> Just (SessionPageCursor row.createdAt row.id)
        [] -> Nothing
        else Nothing
  pure SessionPage{..}

listStatement :: St.Statement (UUID,Maybe UTCTime,Maybe UUID,Maybe UUID,Int32,Maybe Text,Maybe Bool) [AgentSession]
listStatement = St.Statement sql encoder (D.rowList jsonRow) True
  where
    sql = "SELECT to_jsonb(s) FROM sessions s JOIN workspaces w ON w.id=s.workspace_id WHERE s.workspace_id=$1 AND w.deleted_at IS NULL AND ($2::timestamptz IS NULL OR (s.created_at,s.id)<($2,$3::uuid)) AND ($4::uuid IS NULL OR EXISTS (SELECT 1 FROM task_sessions link JOIN tasks t ON t.id=link.task_id WHERE link.session_id=s.id AND link.workspace_id=$1 AND t.id=$4 AND t.deleted_at IS NULL)) AND ($6::text IS NULL OR s.state=$6) AND ($7::boolean IS NULL OR (s.last_seen_at > statement_timestamp()-interval '30 seconds')=$7) ORDER BY s.created_at DESC,s.id DESC LIMIT $5"
    encoder = param (\(a,_,_,_,_,_,_) -> a) E.uuid
      <> optional (\(_,b,_,_,_,_,_) -> b) E.timestamptz
      <> optional (\(_,_,c,_,_,_,_) -> c) E.uuid
      <> optional (\(_,_,_,d,_,_,_) -> d) E.uuid
      <> param (\(_,_,_,_,e,_,_) -> e) E.int4
      <> optional (\(_,_,_,_,_,f,_) -> f) E.text
      <> optional (\(_,_,_,_,_,_,g) -> g) E.bool

listSessionTasks :: Pool Hasql.Connection -> UUID -> UUID -> Maybe UUID -> Int -> IO SessionTaskPage
listSessionTasks pool workspace identifier cursor limit = do
  validateLimit limit
  rows <- runSession pool $ S.statement (workspace,identifier,cursor,fromIntegral (limit+1)) statement
  let items = take limit rows
      nextTaskId = if length rows > limit then case reverse items of
        row:_ -> Just row.id
        [] -> Nothing
        else Nothing
  pure SessionTaskPage{..}
  where
    statement = St.Statement
      "SELECT jsonb_build_object('id',t.id,'title',t.title,'status',t.status::text) FROM task_sessions link JOIN tasks t ON t.id=link.task_id JOIN workspaces w ON w.id=link.workspace_id WHERE link.workspace_id=$1 AND link.session_id=$2 AND t.deleted_at IS NULL AND w.deleted_at IS NULL AND ($3::uuid IS NULL OR t.id>$3) ORDER BY t.id LIMIT $4"
      (param (\(a,_,_,_) -> a) E.uuid <> param (\(_,b,_,_) -> b) E.uuid
       <> optional (\(_,_,c,_) -> c) E.uuid <> param (\(_,_,_,d) -> d) E.int4)
      (D.rowList jsonRow) True

appendSession :: Pool Hasql.Connection -> UUID -> UUID -> AppendSession -> IO SessionAppendResult
appendSession pool workspace identifier input = do
  if BS.null input.bytes || BS.length input.bytes > maxSessionChunkBytes
    || input.expectedOffset < 0 || input.expectedRevision < 0
    then invalid "Append requires nonnegative cursor and 1 to 1048576 bytes"
    else pure ()
  checked pool $ do
    current <- S.statement (workspace,identifier) (getStatement True)
    case current of
      Nothing -> pure $ problem SessionNotFound "Session not found"
      Just row -> do
        replay <- S.statement (identifier,input.expectedOffset,input.expectedRevision+1,input.bytes,input.allowFinalRecord) replayStatement
        if replay then pure $ Right (SessionAppendResult row True)
        else if row.state == Terminated then pure $ problem SessionTerminated "Session is terminated"
        else if row.committedBytes /= input.expectedOffset || row.revision /= input.expectedRevision
          then pure $ problem SessionConflict "Append cursor does not match the committed session"
          else do
            S.statement (identifier,input) appendStatement
            result <- S.statement (workspace,identifier) (getStatement False)
            pure $ case result of
              Just updated -> Right (SessionAppendResult updated False)
              Nothing -> problem SessionNotFound "Session not found"

replayStatement :: St.Statement (UUID,Int64,Int64,ByteString,Bool) Bool
replayStatement = St.Statement
  "SELECT EXISTS(SELECT 1 FROM session_chunks WHERE session_id=$1 AND start_offset=$2 AND revision=$3 AND data=$4 AND allow_final_record=$5)"
  (param (\(a,_,_,_,_) -> a) E.uuid <> param (\(_,b,_,_,_) -> b) E.int8
   <> param (\(_,_,c,_,_) -> c) E.int8 <> param (\(_,_,_,d,_) -> d) E.bytea
   <> param (\(_,_,_,_,e) -> e) E.bool)
  (D.singleRow (D.column (D.nonNullable D.bool))) True

appendStatement :: St.Statement (UUID,AppendSession) ()
appendStatement = St.Statement
  "INSERT INTO session_chunks(session_id,start_offset,revision,data,allow_final_record,record_count,ends_with_newline) VALUES ($1,$2,$3,$4,$5,0,false)"
  (param fst E.uuid <> param (\(_,a) -> a.expectedOffset) E.int8
   <> param (\(_,a) -> a.expectedRevision+1) E.int8 <> param (\(_,a) -> a.bytes) E.bytea
   <> param (\(_,a) -> a.allowFinalRecord) E.bool) D.noResult True

heartbeatSession :: Pool Hasql.Connection -> UUID -> UUID -> IO AgentSession
heartbeatSession pool workspace identifier = checked pool $ do
  current <- S.statement (workspace,identifier) (getStatement True)
  case current of
    Nothing -> pure $ problem SessionNotFound "Session not found"
    Just row | row.state == Terminated -> pure $ problem SessionTerminated "Session is terminated"
    Just _ -> do
      row <- S.statement (workspace,identifier) $ St.Statement
        "UPDATE sessions s SET last_seen_at=greatest(last_seen_at,clock_timestamp()) WHERE workspace_id=$1 AND id=$2 RETURNING to_jsonb(s)"
        pairParams (D.singleRow jsonRow) True
      pure (Right row)

associateSessionTask :: Pool Hasql.Connection -> UUID -> UUID -> UUID -> IO ()
associateSessionTask pool workspace identifier task = checked pool $ do
  current <- S.statement (workspace,identifier) (getStatement False)
  case current of
    Nothing -> pure $ problem SessionNotFound "Session not found"
    Just _ -> do
      linked <- S.statement (workspace,identifier,task) $ St.Statement
        "WITH task AS (SELECT id FROM tasks WHERE workspace_id=$1 AND id=$3 AND deleted_at IS NULL FOR SHARE), inserted AS (INSERT INTO task_sessions(workspace_id,session_id,task_id) SELECT $1,$2,id FROM task ON CONFLICT DO NOTHING) SELECT EXISTS(SELECT 1 FROM task)"
        (param (\(a,_,_) -> a) E.uuid <> param (\(_,b,_) -> b) E.uuid <> param (\(_,_,c) -> c) E.uuid)
        (D.singleRow (D.column (D.nonNullable D.bool))) True
      pure $ if linked then Right () else problem SessionNotFound "Active same-workspace task not found"

-- | A bounded, indexed chunk read, including chunks intersecting the window.
rangeStatement :: St.Statement (UUID,Int64,Int64) ByteString
rangeStatement = St.Statement
  "SELECT coalesce(string_agg(substring(data FROM (greatest($2,start_offset)-start_offset+1)::integer FOR (least($3,start_offset+octet_length(data))-greatest($2,start_offset))::integer),''::bytea ORDER BY start_offset),''::bytea) FROM session_chunks WHERE session_id=$1 AND start_offset >= coalesce((SELECT max(start_offset) FROM session_chunks WHERE session_id=$1 AND start_offset <= $2),0) AND start_offset < $3"
  (param (\(a,_,_) -> a) E.uuid <> param (\(_,b,_) -> b) E.int8 <> param (\(_,_,c) -> c) E.int8)
  (D.singleRow (D.column (D.nonNullable D.bytea))) True

readSessionRange :: Pool Hasql.Connection -> UUID -> UUID -> Int64 -> Int -> IO SessionRange
readSessionRange pool workspace identifier offset limit = do
  if offset < 0 || limit < 1 || limit > maxSessionRangeBytes
    then invalid "Range requires nonnegative offset and 1 to 1048576 bytes"
    else pure ()
  checked pool $ do
    -- SHARE holds a consistent cursor while append/terminate waits.
    current <- S.statement (workspace,identifier) $ St.Statement (sessionSelect <> " FOR SHARE OF s") pairParams (D.rowMaybe jsonRow) True
    case current of
      Nothing -> pure $ problem SessionNotFound "Session not found"
      Just session
        | offset > session.committedBytes -> pure $ problem SessionConflict "Range begins beyond the committed prefix"
        | otherwise -> do
          let end = offset + min (session.committedBytes-offset) (fromIntegral limit)
          bytes <- S.statement (identifier,offset,end) rangeStatement
          let nextOffset = offset+fromIntegral (BS.length bytes)
              hasMore = nextOffset < session.committedBytes
          pure $ Right SessionRange{..}

terminateSession :: Pool Hasql.Connection -> UUID -> UUID -> TerminateSession -> IO AgentSession
terminateSession pool workspace identifier input = do
  if input.expectedOffset < 0 || input.expectedRevision < 0 || input.finalBytes < 0
    || T.length input.finalSha256 /= 64 || T.any (\c -> not (c `elem` ("0123456789abcdef" :: String))) input.finalSha256
    then invalid "Termination requires nonnegative cursor/count and lowercase SHA-256"
    else pure ()
  checked pool $ do
    current <- S.statement (workspace,identifier) (getStatement True)
    case current of
      Nothing -> pure $ problem SessionNotFound "Session not found"
      Just row
        | row.state == Terminated -> pure $
            if input.expectedOffset == row.committedBytes && input.expectedRevision == row.revision-1
              && Just input.finalBytes == row.finalBytes && Just input.finalSha256 == row.finalSha256
            then Right row else problem SessionConflict "Termination receipt conflicts with the sealed session"
        | row.committedBytes /= input.expectedOffset || row.revision /= input.expectedRevision
          || row.committedBytes /= input.finalBytes -> pure $ problem SessionConflict "Termination cursor/count does not match the committed session"
        | otherwise -> do
          digest <- hashCommitted identifier row.committedBytes 0 (hashInit :: Context SHA256)
          if digest /= input.finalSha256
            then pure $ problem SessionConflict "Termination SHA-256 does not match the committed prefix"
            else do
              sealed <- S.statement (workspace,identifier,input.finalSha256) $ St.Statement
                "UPDATE sessions s SET state='terminated',terminated_at=clock_timestamp(),final_bytes=committed_bytes,final_sha256=$3,revision=revision+1 WHERE workspace_id=$1 AND id=$2 RETURNING to_jsonb(s)"
                (param (\(a,_,_) -> a) E.uuid <> param (\(_,b,_) -> b) E.uuid <> param (\(_,_,c) -> c) E.text)
                (D.singleRow jsonRow) True
              pure (Right sealed)

-- Fetch exactly one indexed chunk per step, without retaining the transcript.
-- Counters and chunk continuity are also enforced by the insertion guards.
hashCommitted :: UUID -> Int64 -> Int64 -> Context SHA256 -> S.Session Text
hashCommitted identifier end offset context
  | offset == end = pure (T.pack (show (hashFinalize context :: Digest SHA256)))
  | otherwise = do
      chunks <- S.statement (identifier,offset) $ St.Statement
        "SELECT data FROM session_chunks WHERE session_id=$1 AND start_offset=$2"
        (param fst E.uuid <> param snd E.int8)
        (D.singleRow (D.column (D.nonNullable D.bytea))) True
      let next = offset+fromIntegral (BS.length chunks)
          updated = hashUpdate context chunks
      updated `seq` hashCommitted identifier end next updated
