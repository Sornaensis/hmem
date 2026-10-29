-- | Canonical transactional persistence for immutable JSONL evidence.
module HMem.DB.AgentSession
  ( createSession, importSession, getSession, listWorkspaceSessions, listTaskSessions, listSessionTasks
  , listWorkspaceSessionsFiltered, listTaskSessionsFiltered
  , workspaceSessionActivity, projectSessionActivity, listSessionFacets, listSessionFacetsFiltered
  , appendSession, heartbeatSession, associateSessionTask, terminateSession, terminateSessionWithHook, readSessionRange, setRuntimeIdentity
  , IdleSessionCursor, listIdleSessionCandidates, sealIdleSession, sealIdleSessionWithHook
  , listIdleSessionCandidatesAt, sealIdleSessionWithClock
  , validateSessionPrefixBoundary, readSessionPrefixWindow
  , SessionReader, newSessionReader, readSessionReader, closeSessionReader
  , ReaderCache, ArchiveAudience(..), ReaderCacheBusy(..), newReaderCache, closeReaderCache, readSessionRangeWithCache
  , ArchiveReadFailure(..)
  , readSessionEvidenceRange, readSessionEvidenceRangeWithCache
  ) where

import Control.Exception (throwIO, bracket, mask, onException, catch)
import Control.Concurrent.MVar
import Control.Monad (foldM, forM_)
import Crypto.Hash (Context, Digest, SHA256, hashInit, hashUpdate, hashFinalize)
import Data.Aeson (FromJSON, Result(..), fromJSON, toJSON)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Functor.Contravariant (contramap)
import Data.Int (Int32, Int64)
import Data.Pool (Pool)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time (UTCTime, addUTCTime)
import Data.UUID (UUID)
import Hasql.Connection qualified as Hasql
import Hasql.Decoders qualified as D
import Hasql.Encoders qualified as E
import Hasql.Session qualified as S
import Hasql.Pipeline qualified as P
import Hasql.Statement qualified as St

import HMem.AgentSession
import HMem.DB.Pool (runSession, runTransaction)
import HMem.DB.RequestContext (EffectiveActor(..), actorTypeToText, currentEffectiveActor)
import HMem.DB.SessionArchive.Reader (ReaderCache, ArchiveAudience(..), ReaderCacheBusy(..), ArchiveReadFailure(..), newReaderCache, closeReaderCache)
import HMem.DB.SessionArchive.Reader qualified as Archive
import HMem.DB.SessionArchive qualified

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
sessionSelect = "SELECT to_jsonb(s) || hmem_session_ancestor_context(s.id) FROM sessions s JOIN workspaces w ON w.id=s.workspace_id WHERE s.workspace_id=$1 AND s.id=$2 AND w.deleted_at IS NULL"

getStatement :: Bool -> St.Statement (UUID,UUID) (Maybe AgentSession)
getStatement locked = St.Statement (sessionSelect <> if locked then " FOR UPDATE OF s" else "") pairParams (D.rowMaybe jsonRow) True

createSession :: Pool Hasql.Connection -> CreateSession -> IO AgentSession
createSession pool input = createSessionWith pool input Nothing

importSession :: Pool Hasql.Connection -> ImportSession -> IO AgentSession
importSession pool input
  | input.finalBytes < 0 || T.length input.finalSha256 /= 64
    || T.any (`notElem` ("0123456789abcdef" :: String)) input.finalSha256 = invalid "Import requires a nonnegative byte count and lowercase SHA-256"
  | otherwise = createSessionWith pool input.session (Just (input.finalBytes,input.finalSha256))

createSessionWith :: Pool Hasql.Connection -> CreateSession -> Maybe (Int64,Text) -> IO AgentSession
createSessionWith pool input manifest = do
  if BS.length (TE.encodeUtf8 input.formatLabel) > 128 || T.null (T.strip input.formatLabel)
    then invalid "format_label must contain 1 to 128 UTF-8 bytes"
    else pure ()
  if BS.length (TE.encodeUtf8 (T.strip input.title)) > 80 || T.null (T.strip input.title)
    then invalid "A very short descriptive title is required (1 to 80 UTF-8 bytes)"
    else pure ()
  if maybe False (not . runtimeIdentityValid) input.initialRuntimeIdentity
    then invalid "Invalid initial runtime identity declaration"
    else pure ()
  actor <- currentEffectiveActor
  checked pool $ do
    let racedOr problemValue = do
          current <- S.statement (input.workspaceId,input.id) (getStatement True)
          pure $ maybe (Left problemValue) (Right . Just) current
    -- Read before INSERT: a legacy row may have no title and must remain
    -- unchanged on resume, including when it is permanently sealed.
    found <- S.statement (input.workspaceId,input.id) (getStatement True)
    existing <- case found of
      Just row -> pure (Right (Just row))
      Nothing | maybe False (not . runtimeDeclarationValid) input.initialRuntimeIdentity ->
        pure $ problem SessionInvalidInput "A new runtime declaration requires the running agent role"
      Nothing -> do
        target <- S.statement (input.workspaceId,input.id,input.taskId) taskAssociationStatement
        case target of
          Nothing -> racedOr (SessionException SessionTaskNotFound "task_id must identify a live same-workspace task")
          Just (status,_) | status /= "in_progress" && manifest == Nothing -> racedOr (SessionException (SessionTaskNotInProgress status) "Start the task before creating a session")
          _ -> do
            inserted <- S.statement ((input {title = T.strip input.title} :: CreateSession),manifest) createStatement
            case inserted of
              Just row -> do
                S.statement (input.workspaceId,input.id,input.taskId) associateStatement
                pure (Right (Just row))
              Nothing -> Right <$> S.statement (input.workspaceId,input.id) (getStatement True)
    pure $ case existing of
      Left exception -> Left exception
      Right Nothing -> problem SessionConflict "Session UUID already exists in another workspace, or workspace is inactive"
      Right (Just row)
        | row.formatLabel == input.formatLabel && row.parentSessionId == input.parentSessionId
          && maybe True (== T.strip input.title) row.title && sameCreator actor row
          && row.importTaskId == (input.taskId <$ manifest)
          && row.importBytes == fmap fst manifest && row.importSha256 == fmap snd manifest
          && (manifest == Nothing || row.runtimeIdentity == input.initialRuntimeIdentity) -> Right row
        | otherwise -> problem SessionConflict "Session UUID was already created with different immutable metadata"
  where
    sameCreator Nothing row = row.creatorActorType == Nothing && row.creatorActorId == Nothing
      && row.creatorActorLabel == Nothing && row.creatorBotIdentity == Nothing
    sameCreator (Just actor) row = row.creatorActorType == Just (actorTypeToText actor.actorType)
      && row.creatorActorId == Just actor.actorId && row.creatorActorLabel == Just actor.actorLabel
      && row.creatorBotIdentity == actor.botIdentity

createStatement :: St.Statement (CreateSession,Maybe (Int64,Text)) (Maybe AgentSession)
createStatement = St.Statement sql encoder (D.rowMaybe jsonRow) True
  where
    sql = "WITH workspace AS (SELECT id FROM workspaces WHERE id=$2 AND deleted_at IS NULL FOR SHARE), inserted AS (INSERT INTO sessions(id,workspace_id,format_label,title,parent_session_id,runtime_identity,import_task_id,import_bytes,import_sha256,creator_actor_type,creator_actor_id,creator_actor_label,creator_bot_identity) SELECT $1,id,$3,$4,$5,$6,$7,$8,$9,NULLIF(current_setting('hmem.actor_type',true),''),NULLIF(current_setting('hmem.actor_id',true),''),NULLIF(current_setting('hmem.actor_label',true),''),NULLIF(current_setting('hmem.bot_identity',true),'')::jsonb FROM workspace ON CONFLICT(id) DO NOTHING RETURNING *) SELECT to_jsonb(inserted) FROM inserted"
    encoder = param ((.id) . fst) E.uuid <> param ((.workspaceId) . fst) E.uuid <> param ((.formatLabel) . fst) E.text <> param ((.title) . fst) E.text <> optional ((.parentSessionId) . fst) E.uuid <> optional (fmap toJSON . (.initialRuntimeIdentity) . fst) E.jsonb
      <> optional (\(value,m) -> value.taskId <$ m) E.uuid <> optional (fmap fst . snd) E.int8 <> optional (fmap snd . snd) E.text

-- Runtime declarations have their own cursor. They never consume transcript
-- revisions, heartbeat activity or byte/hash receipts, and share the row lock
-- with append/termination so a sealed session cannot change underneath a write.
setRuntimeIdentity :: Pool Hasql.Connection -> UUID -> UUID -> SetRuntimeIdentity -> IO AgentSession
setRuntimeIdentity pool workspace identifier input = do
  if input.expectedRuntimeRevision < 0 || maybe False (not . runtimeIdentityValid) input.runtimeIdentity
    then invalid "Runtime updates require a nonnegative runtime revision and valid declaration"
    else pure ()
  checked pool $ do
    found <- S.statement (workspace,identifier) (getStatement True)
    case found of
      Nothing -> pure $ problem SessionNotFound "Session not found"
      Just row | row.state == Terminated -> pure $ problem SessionTerminated "Session is terminated"
      Just row | row.importTaskId /= Nothing -> pure $ problem SessionConflict "Historical runtime identity is fixed at import"
      Just row
        | row.runtimeRevision == input.expectedRuntimeRevision ->
            if row.runtimeIdentity == input.runtimeIdentity then pure (Right row)
            else if maybe False (not . runtimeDeclarationValid) input.runtimeIdentity
              then pure $ problem SessionInvalidInput "A new runtime declaration requires the running agent role"
            else do
              updated <- S.statement (workspace,identifier,input.runtimeIdentity) $ St.Statement
                "UPDATE sessions s SET runtime_identity=$3,runtime_revision=runtime_revision+1 WHERE workspace_id=$1 AND id=$2 RETURNING to_jsonb(s) || hmem_session_ancestor_context(s.id)"
                (param (\(a,_,_) -> a) E.uuid <> param (\(_,b,_) -> b) E.uuid <> optional (\(_,_,c) -> toJSON <$> c) E.jsonb)
                (D.singleRow jsonRow) True
              pure (Right updated)
        | input.expectedRuntimeRevision == row.runtimeRevision - 1 && row.runtimeIdentity == input.runtimeIdentity -> pure (Right row)
        | otherwise -> pure $ problem SessionRuntimeConflict "Runtime identity revision conflicts; refresh session metadata"

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
      fmap (\case Fresh -> True; Stale -> False) filters.freshness, filters.projectId, filters.taskId, activityMask <$> filters.activity) listStatement
  let items = take limit rows
      nextCursor = if length rows > limit then case reverse items of
        row:_ -> Just (SessionPageCursor row.createdAt row.id)
        [] -> Nothing
        else Nothing
  pure SessionPage{..}

listStatement :: St.Statement (UUID,Maybe UTCTime,Maybe UUID,Maybe UUID,Int32,Maybe Text,Maybe Bool,Maybe UUID,Maybe UUID,Maybe Int32) [AgentSession]
listStatement = St.Statement sql encoder (D.rowList jsonRow) True
  where
    sql = "SELECT to_jsonb(s) || hmem_session_ancestor_context(s.id) FROM sessions s JOIN workspaces w ON w.id=s.workspace_id WHERE s.workspace_id=$1 AND w.deleted_at IS NULL AND (s.import_task_id IS NULL OR s.state='terminated') AND ($2::timestamptz IS NULL OR (s.created_at,s.id)<($2,$3::uuid)) AND ($4::uuid IS NULL OR EXISTS (SELECT 1 FROM task_sessions link JOIN tasks t ON t.id=link.task_id WHERE link.session_id=s.id AND link.workspace_id=$1 AND t.id=$4 AND t.deleted_at IS NULL)) AND ($6::text IS NULL OR s.state=$6) AND ($7::boolean IS NULL OR (s.last_seen_at > statement_timestamp()-interval '30 seconds')=$7) AND ($10::int IS NULL OR (($10 & 1)<>0 AND s.state='active' AND s.last_seen_at>statement_timestamp()-interval '30 seconds') OR (($10 & 2)<>0 AND s.state='active' AND s.last_seen_at<=statement_timestamp()-interval '30 seconds') OR (($10 & 4)<>0 AND s.state='terminated')) AND ($8::uuid IS NULL OR EXISTS(SELECT 1 FROM task_sessions link JOIN hmem_session_project_tasks($1,$8) scoped ON scoped.id=link.task_id WHERE link.session_id=s.id AND link.workspace_id=$1)) AND ($8::uuid IS NULL OR $9::uuid IS NULL OR $9 IN (SELECT id FROM hmem_session_project_tasks($1,$8))) AND ($9::uuid IS NULL OR EXISTS(SELECT 1 FROM task_sessions link JOIN tasks t ON t.id=link.task_id WHERE link.session_id=s.id AND link.workspace_id=$1 AND t.id=$9 AND t.deleted_at IS NULL)) ORDER BY s.created_at DESC,s.id DESC LIMIT $5"
    encoder = param (\(a,_,_,_,_,_,_,_,_,_) -> a) E.uuid
      <> optional (\(_,b,_,_,_,_,_,_,_,_) -> b) E.timestamptz
      <> optional (\(_,_,c,_,_,_,_,_,_,_) -> c) E.uuid
      <> optional (\(_,_,_,d,_,_,_,_,_,_) -> d) E.uuid
      <> param (\(_,_,_,_,e,_,_,_,_,_) -> e) E.int4
      <> optional (\(_,_,_,_,_,f,_,_,_,_) -> f) E.text
      <> optional (\(_,_,_,_,_,_,g,_,_,_) -> g) E.bool
      <> optional (\(_,_,_,_,_,_,_,h,_,_) -> h) E.uuid
      <> optional (\(_,_,_,_,_,_,_,_,i,_) -> i) E.uuid
      <> optional (\(_,_,_,_,_,_,_,_,_,j) -> j) E.int4

-- Facets are independent of transcript pages and heartbeat freshness. Each
-- response is bounded; the UUID cursor makes every active option reachable.
listSessionFacets :: Pool Hasql.Connection -> UUID -> Bool -> Maybe UUID -> Maybe UUID -> Maybe UUID -> Int -> IO SessionFacetPage
listSessionFacets pool workspace projects project cursor selectedId =
  listSessionFacetsFiltered pool workspace projects project cursor selectedId Nothing

activityMask :: [SessionActivity] -> Int32
activityMask values = sum [bit | (value,bit) <- [(Live,1),(Quiet,2),(Completed,4)], value `elem` values]

-- Facet options retain baseline freshness independence: Live/Quiet select the
-- active branch. A heartbeat crossing never changes option membership.
facetActivityMask :: [SessionActivity] -> Int32
facetActivityMask values = (if Live `elem` values || Quiet `elem` values then 3 else 0) + (if Completed `elem` values then 4 else 0)

listSessionFacetsFiltered :: Pool Hasql.Connection -> UUID -> Bool -> Maybe UUID -> Maybe UUID -> Maybe UUID -> Maybe [SessionActivity] -> Int -> IO SessionFacetPage
listSessionFacetsFiltered pool workspace projects project cursor selectedId activity limit = do
  validateLimit limit
  rows <- runSession pool $ S.statement (workspace,project,cursor,fromIntegral (limit+1),facetActivityMask <$> activity) statement
  selected <- case selectedId of
    Nothing -> pure Nothing
    Just identifier | activity /= Nothing -> runSession pool $ S.statement (workspace,project,identifier) $
      St.Statement selectedSql
        (param (\(a,_,_) -> a) E.uuid <> optional (\(_,b,_) -> b) E.uuid <> param (\(_,_,c) -> c) E.uuid)
        (D.rowMaybe jsonRow) True
    Just identifier -> runSession pool $ S.statement (workspace,project,Just identifier,1,Nothing) $
      St.Statement (if projects then replace "($3::uuid IS NULL OR p.id>$3)" "p.id=$3" sql else replace "($3::uuid IS NULL OR t.id>$3)" "t.id=$3" sql) encoder (D.rowMaybe jsonRow) True
  let items = take limit rows
      nextId = if length rows > limit then (.id) <$> lastMaybe items else Nothing
  pure SessionFacetPage{..}
  where
    lastMaybe [] = Nothing
    lastMaybe xs = Just (last xs)
    replace before after = TE.encodeUtf8 . T.replace before after . TE.decodeUtf8
    encoder = param (\(a,_,_,_,_) -> a) E.uuid <> optional (\(_,b,_,_,_) -> b) E.uuid <> optional (\(_,_,c,_,_) -> c) E.uuid <> param (\(_,_,_,d,_) -> d) E.int4 <> optional (\(_,_,_,_,e) -> e) E.int4
    statement = St.Statement sql encoder (D.rowList jsonRow) True
    -- Activity absence is not evidence that a selected entity became invalid.
    selectedSql = if projects then
      "SELECT jsonb_build_object('id',p.id,'title',p.name) FROM projects p JOIN workspaces w ON w.id=p.workspace_id WHERE p.workspace_id=$1 AND p.deleted_at IS NULL AND w.deleted_at IS NULL AND ($2::uuid IS NULL OR p.id=$2) AND p.id=$3"
      else "SELECT jsonb_build_object('id',t.id,'title',t.title) FROM tasks t JOIN workspaces w ON w.id=t.workspace_id WHERE t.workspace_id=$1 AND t.deleted_at IS NULL AND w.deleted_at IS NULL AND ($2::uuid IS NULL OR t.id IN (SELECT id FROM hmem_session_project_tasks($1,$2))) AND t.id=$3"
    activityPredicate = "(s.import_task_id IS NULL OR s.state='terminated') AND (($5::int IS NULL AND s.state='active') OR (($5 & 1)<>0 AND s.state='active' AND s.last_seen_at>statement_timestamp()-interval '30 seconds') OR (($5 & 2)<>0 AND s.state='active' AND s.last_seen_at<=statement_timestamp()-interval '30 seconds') OR (($5 & 4)<>0 AND s.state='terminated'))"
    sql = if projects then
      "SELECT jsonb_build_object('id',p.id,'title',p.name) FROM projects p JOIN workspaces w ON w.id=p.workspace_id WHERE p.workspace_id=$1 AND p.deleted_at IS NULL AND w.deleted_at IS NULL AND ($2::uuid IS NULL OR p.id=$2) AND ($3::uuid IS NULL OR p.id>$3) AND EXISTS(SELECT 1 FROM task_sessions link JOIN sessions s ON s.id=link.session_id JOIN hmem_session_project_tasks($1,p.id) scoped ON scoped.id=link.task_id WHERE link.workspace_id=$1 AND s.workspace_id=$1 AND " <> activityPredicate <> ") ORDER BY p.id LIMIT $4"
      else "SELECT jsonb_build_object('id',t.id,'title',t.title) FROM tasks t JOIN workspaces w ON w.id=t.workspace_id WHERE t.workspace_id=$1 AND t.deleted_at IS NULL AND w.deleted_at IS NULL AND ($2::uuid IS NULL OR t.id IN (SELECT id FROM hmem_session_project_tasks($1,$2))) AND ($3::uuid IS NULL OR t.id>$3) AND EXISTS(SELECT 1 FROM task_sessions link JOIN sessions s ON s.id=link.session_id WHERE link.workspace_id=$1 AND link.task_id=t.id AND s.workspace_id=$1 AND " <> activityPredicate <> ") ORDER BY t.id LIMIT $4"

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
    || input.expectedOffset < 0 || input.expectedRevision < 0 || input.expectedRevision == maxBound
    then invalid "Append requires nonnegative cursor and 1 to 1048576 bytes"
    else pure ()
  -- Active appends retain the original transaction and query path. Only a
  -- terminal row with no exact raw replay leaves a candidate for archive
  -- decoding after this transaction has released its lock and pool lease.
  result <- checked pool $ do
    current <- S.statement (workspace,identifier) (getStatement True)
    case current of
      Nothing -> pure $ problem SessionNotFound "Session not found"
      Just row -> do
        replay <- S.statement (identifier,input.expectedOffset,input.expectedRevision+1,input.bytes,input.allowFinalRecord) replayStatement
        if replay then pure $ Right (Right (SessionAppendResult row True))
        else if row.state == Terminated then pure $ Right (Left row)
        else if maybe False (\total -> fromIntegral (BS.length input.bytes) > total-row.committedBytes) row.importBytes
          then pure $ problem SessionConflict "Append exceeds the historical import manifest"
        else if row.committedBytes /= input.expectedOffset || row.revision /= input.expectedRevision
          then pure $ problem SessionConflict "Append cursor does not match the committed session"
          else do
            S.statement (identifier,input) appendStatement
            result <- S.statement (workspace,identifier) (getStatement False)
            pure $ case result of
              Just updated -> Right (Right (SessionAppendResult updated False))
              Nothing -> problem SessionNotFound "Session not found"
  case result of
    Right appended -> pure appended
    Left sealed -> do
      -- Published generations and original append rows are immutable. Compare
      -- the complete verified original chunk and allow_final_record before
      -- rejecting the terminal state, without reopening a transaction.
      published <- Archive.verifiedManifest pool workspace identifier
      replay <- case published of
        Nothing -> pure False
        Just manifest -> do
          original <- Archive.verifiedLedger pool workspace identifier manifest.id input.expectedOffset
          case original of
            Just row | row.startOffset == input.expectedOffset && row.revision == input.expectedRevision+1
              && row.endOffset-row.startOffset == fromIntegral (BS.length input.bytes) && row.allowFinalRecord == input.allowFinalRecord ->
                bracket (Archive.openArchiveReader pool manifest) Archive.closeArchiveReader $ \reader ->
                  (== input.bytes) <$> Archive.readArchiveRange reader input.expectedOffset (BS.length input.bytes)
            _ -> pure False
      if replay then pure (SessionAppendResult sealed True)
        else throwIO (SessionException SessionTerminated "Session is terminated")

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
      target <- S.statement (workspace,identifier,task) taskAssociationStatement
      case target of
        Nothing -> pure $ problem SessionNotFound "Live same-workspace task not found"
        Just (_,True) -> pure (Right ())
        Just (status,False) | status /= "in_progress" -> pure $ problem (SessionTaskNotInProgress status) "Task must be in_progress before adding a session"
        _ -> do
          S.statement (workspace,identifier,task) associateStatement
          pure (Right ())

-- The task lock is held until session creation and its initial link commit.
-- Replays read the existing session before this creation-only validation.
taskAssociationStatement :: St.Statement (UUID,UUID,UUID) (Maybe (Text,Bool))
taskAssociationStatement = St.Statement
  "SELECT t.status::text,EXISTS(SELECT 1 FROM task_sessions link WHERE link.workspace_id=$1 AND link.session_id=$2 AND link.task_id=$3) FROM tasks t WHERE t.workspace_id=$1 AND t.id=$3 AND t.deleted_at IS NULL FOR SHARE OF t"
  (param (\(a,_,_) -> a) E.uuid <> param (\(_,b,_) -> b) E.uuid <> param (\(_,_,c) -> c) E.uuid)
  (D.rowMaybe ((,) <$> D.column (D.nonNullable D.text) <*> D.column (D.nonNullable D.bool))) True

associateStatement :: St.Statement (UUID,UUID,UUID) ()
associateStatement = St.Statement
  "INSERT INTO task_sessions(workspace_id,session_id,task_id) VALUES ($1,$2,$3) ON CONFLICT DO NOTHING"
  (param (\(a,_,_) -> a) E.uuid <> param (\(_,b,_) -> b) E.uuid <> param (\(_,_,c) -> c) E.uuid) D.noResult True

-- | A bounded, indexed chunk read, including chunks intersecting the window.
rangeStatement :: St.Statement (UUID,Int64,Int64) ByteString
rangeStatement = St.Statement
  "SELECT coalesce(string_agg(substring(data FROM (greatest($2,start_offset)-start_offset+1)::integer FOR (least($3,start_offset+octet_length(data))-greatest($2,start_offset))::integer),''::bytea ORDER BY start_offset),''::bytea) FROM session_chunks WHERE session_id=$1 AND start_offset >= coalesce((SELECT max(start_offset) FROM session_chunks WHERE session_id=$1 AND start_offset <= $2),0) AND start_offset < $3"
  (param (\(a,_,_) -> a) E.uuid <> param (\(_,b,_) -> b) E.int8 <> param (\(_,_,c) -> c) E.int8)
  (D.singleRow (D.column (D.nonNullable D.bytea))) True

-- Immutable append boundaries remain valid after append-only growth or seal.
-- The existing unique (session_id,revision) index bounds this lookup. Runtime
-- and terminal metadata revisions never impersonate an append boundary.
validateSessionPrefixBoundary :: Pool Hasql.Connection -> UUID -> UUID -> Int64 -> Int64 -> IO Bool
validateSessionPrefixBoundary pool workspace identifier end revision
  | end < 0 || revision < 0 = pure False
  | otherwise = runSession pool $ S.statement (workspace,identifier,end,revision) $ St.Statement
      "SELECT EXISTS(SELECT 1 FROM sessions s JOIN workspaces w ON w.id=s.workspace_id WHERE s.workspace_id=$1 AND s.id=$2 AND w.deleted_at IS NULL AND $3<=s.committed_bytes AND (($3=0 AND $4=0) OR EXISTS(SELECT 1 FROM session_chunks c WHERE c.session_id=s.id AND c.revision=$4 AND c.start_offset+octet_length(c.data)=$3) OR EXISTS(SELECT 1 FROM session_archives g JOIN session_archive_ledger l ON l.generation_id=g.id WHERE g.workspace_id=s.workspace_id AND g.session_id=s.id AND l.revision=$4 AND l.end_offset=$3 AND l.binding_sha256=hmem_archive_binding(coalesce((SELECT p.binding_sha256 FROM session_archive_ledger p WHERE p.generation_id=l.generation_id AND p.revision=l.revision-1),''),jsonb_build_array(l.generation_id,l.revision,l.start_offset,l.end_offset,l.allow_final_record,l.record_count,l.ends_with_newline,l.raw_sha256)))))"
      (param (\(a,_,_,_) -> a) E.uuid <> param (\(_,b,_,_) -> b) E.uuid <> param (\(_,_,c,_) -> c) E.int8 <> param (\(_,_,_,d) -> d) E.int8)
      (D.singleRow (D.column (D.nonNullable D.bool))) True

-- One bounded ordered byte window, with no session lock or connection retained
-- by its caller between steps. Never read past the frozen immutable target.
readSessionPrefixWindow :: Pool Hasql.Connection -> UUID -> UUID -> Int64 -> Int64 -> IO ByteString
readSessionPrefixWindow pool workspace identifier end offset
  | offset < 0 || end < offset = invalid "Invalid prefix window"
  | offset == end = pure BS.empty
  | otherwise = bracket (newSessionReader pool workspace identifier end) closeSessionReader $ \reader ->
      readSessionReader reader offset (fromIntegral (min 1048576 (end-offset)))

-- A caller-owned sequential scan, pinned to a frozen raw prefix and, once
-- needed, one immutable archive generation. No transaction/connection is held
-- between turns. Short raw reads re-resolve publication exactly once.
data SessionReader = SessionReader
  { readerPool :: !(Pool Hasql.Connection), readerWorkspace :: !UUID, readerSession :: !UUID
  , readerEnd :: !Int64, archived :: !(MVar (Bool,Maybe Archive.ArchiveReader)) }

newSessionReader :: Pool Hasql.Connection -> UUID -> UUID -> Int64 -> IO SessionReader
newSessionReader pool workspace identifier end = do
  row <- getSession pool workspace identifier
  case row of
    Just session | end >= 0 && end <= session.committedBytes ->
      SessionReader pool workspace identifier end <$> newMVar (False,Nothing)
    _ -> invalid "Committed prefix is missing or discontinuous"

closeSessionReader :: SessionReader -> IO ()
closeSessionReader reader = modifyMVar_ reader.archived $ \(_,archive) -> do
  forM_ archive Archive.closeArchiveReader
  pure (True,Nothing)

readSessionReader :: SessionReader -> Int64 -> Int -> IO ByteString
readSessionReader reader offset limit = modifyMVar reader.archived $ \(closed,archive) -> do
  if closed || offset < 0 || offset > reader.readerEnd || limit < 0 || limit > maxSessionRangeBytes
    then invalid "Invalid prefix window" else pure ()
  let count=min (fromIntegral limit) (reader.readerEnd-offset)
      readArchived source = do
        bytes <- Archive.readArchiveRange source offset (fromIntegral count)
        if fromIntegral (BS.length bytes) /= count then invalid "Committed prefix is missing or discontinuous" else pure bytes
  case archive of
    Just source -> do
      bytes <- readArchived source
      pure ((False,archive),bytes)
    Nothing -> do
      bytes <- runSession reader.readerPool $ S.statement (reader.readerSession,offset,offset+count) rangeStatement
      if fromIntegral (BS.length bytes) == count then pure ((False,Nothing),bytes) else mask $ \restore -> do
        manifest <- Archive.verifiedManifest reader.readerPool reader.readerWorkspace reader.readerSession
          >>= maybe (invalid "Committed prefix is missing or discontinuous") pure
        if manifest.rawBytes < reader.readerEnd then invalid "Committed prefix is missing or discontinuous" else pure ()
        source <- Archive.openArchiveReader reader.readerPool manifest
        result <- restore (readArchived source) `onException` Archive.closeArchiveReader source
        pure ((False,Just source),result)

readSessionRange :: Pool Hasql.Connection -> UUID -> UUID -> Int64 -> Int -> IO SessionRange
readSessionRange = readSessionRangeUsing False Nothing

readSessionRangeWithCache :: ReaderCache -> ArchiveAudience -> Pool Hasql.Connection -> UUID -> UUID -> Int64 -> Int -> IO SessionRange
readSessionRangeWithCache cache audience = readSessionRangeUsing False (Just (cache,audience))

-- The terminated evidence route keeps the same raw-first data contract while
-- bounding its metadata and raw queries with the archive read policy. Active
-- and direct legacy callers retain their existing query policy.
readSessionEvidenceRange :: Pool Hasql.Connection -> UUID -> UUID -> Int64 -> Int -> IO SessionRange
readSessionEvidenceRange = readSessionRangeUsing True Nothing

readSessionEvidenceRangeWithCache :: ReaderCache -> ArchiveAudience -> Pool Hasql.Connection -> UUID -> UUID -> Int64 -> Int -> IO SessionRange
readSessionEvidenceRangeWithCache cache audience = readSessionRangeUsing True (Just (cache,audience))

readSessionRangeUsing :: Bool -> Maybe (ReaderCache,ArchiveAudience) -> Pool Hasql.Connection -> UUID -> UUID -> Int64 -> Int -> IO SessionRange
readSessionRangeUsing guarded cached pool workspace identifier offset limit = do
  if offset < 0 || limit < 1 || limit > maxSessionRangeBytes
    then invalid "Range requires nonnegative offset and 1 to 1048576 bytes"
    else pure ()
  -- Keep the original one-lease raw fast path. Its metadata freezes the visible
  -- end; a short raw result becomes a candidate only after this transaction has
  -- committed and returned its connection. Archive decoding never nests here.
  let snapshot = do
        current <- if guarded
          then Archive.archiveReadSession (workspace,identifier) (getStatement False)
          else S.statement (workspace,identifier) (getStatement False)
        case current of
          Nothing -> pure (problem SessionNotFound "Session not found")
          Just row | offset > row.committedBytes -> pure (problem SessionConflict "Range begins beyond the committed prefix")
          Just row -> do
            let frozenEnd=offset+min (row.committedBytes-offset) (fromIntegral limit)
            rawBytes <- S.statement (identifier,offset,frozenEnd) rangeStatement
            pure (Right (row,frozenEnd,rawBytes))
      readSnapshot=runSession pool snapshot
  result <- if guarded then readSnapshot `catch` (throwIO . ArchiveReadFailure) else readSnapshot
  (session,end,raw) <- either throwIO pure result
  let archivedRead manifest = case cached of
        Just (cache,audience) -> Archive.withCachedReader cache audience pool manifest (\reader -> Archive.readArchiveRange reader offset (fromIntegral (end-offset)))
        Nothing -> bracket (Archive.openArchiveReader pool manifest) Archive.closeArchiveReader (\reader -> Archive.readArchiveRange reader offset (fromIntegral (end-offset)))
      fallback = do
        manifest <- Archive.verifiedManifest pool workspace identifier >>= maybe (invalid "Committed prefix is missing or discontinuous") pure
        if manifest.rawBytes /= session.committedBytes then invalid "Committed prefix is missing or discontinuous" else archivedRead manifest
  -- Intact raw evidence keeps its existing read path. A short READ COMMITTED
  -- result re-resolves publication once to cover concurrent reclamation.
  bytes <- if fromIntegral (BS.length raw) == end-offset then pure raw else fallback
  if fromIntegral (BS.length bytes) /= end-offset then invalid "Committed prefix is missing or discontinuous" else pure ()
  let nextOffset=offset+fromIntegral (BS.length bytes); hasMore=nextOffset < session.committedBytes
  pure SessionRange{..}

terminateSession :: Pool Hasql.Connection -> UUID -> UUID -> TerminateSession -> IO AgentSession
terminateSession pool = terminateSessionWithHook pool (pure ())

-- Hashing never owns the final session row lock. The hook observes a completed
-- bounded group after its pool lease has been returned, for causal race tests.
terminateSessionWithHook :: Pool Hasql.Connection -> IO () -> UUID -> UUID -> TerminateSession -> IO AgentSession
terminateSessionWithHook pool duringHash workspace identifier input = do
  if input.expectedOffset < 0 || input.expectedRevision < 0 || input.finalBytes < 0
    || T.length input.finalSha256 /= 64 || T.any (\c -> not (c `elem` ("0123456789abcdef" :: String))) input.finalSha256
    then invalid "Termination requires nonnegative cursor/count and lowercase SHA-256"
    else pure ()
  let validate row
        | row.importTaskId /= Nothing && (row.importBytes /= Just input.finalBytes || row.importSha256 /= Just input.finalSha256) =
            problem SessionConflict "Historical import must be sealed with its complete declared manifest"
        | row.state == Terminated =
            if input.expectedOffset == row.committedBytes && input.expectedRevision == row.revision-1
              && Just input.finalBytes == row.finalBytes && Just input.finalSha256 == row.finalSha256
            then Right row else problem SessionConflict "Termination receipt conflicts with the sealed session"
        | row.committedBytes /= input.expectedOffset || row.revision /= input.expectedRevision
          || row.committedBytes /= input.finalBytes = problem SessionConflict "Termination cursor/count does not match the committed session"
        | otherwise = Right row
      checkedRow = maybe (problem SessionNotFound "Session not found") validate
  initial <- getSession pool workspace identifier >>= either throwIO pure . checkedRow
  if initial.state == Terminated then pure initial else do
    digest <- hashCommitted pool identifier initial.committedBytes duringHash
    if digest /= input.finalSha256 then throwIO (SessionException SessionConflict "Termination SHA-256 does not match the committed prefix") else
      checked pool $ do
        current <- S.statement (workspace,identifier) (getStatement True)
        case checkedRow current of
          Left failure -> pure (Left failure)
          Right row | row.state == Terminated -> pure (Right row)
          Right _ -> Right <$> S.statement (workspace,identifier,digest) sealStatement

-- Four independent <=1MiB queries share one pipeline. The fixed <=4MiB group
-- releases its connection before hashing/observer callbacks and the next group.
-- No transcript-sized result, cursor, transaction or exclusive lock is retained.
hashCommitted :: Pool Hasql.Connection -> UUID -> Int64 -> IO () -> IO Text
hashCommitted pool identifier end duringHash = go 0 (hashInit :: Context SHA256)
  where
    go offset context
      | offset == end = pure (T.pack (show (hashFinalize context :: Digest SHA256)))
      | otherwise = do
          let starts = take 4 (iterate (\start -> start+min 1048576 (end-start)) offset)
              ranges = [(start,start+min 1048576 (end-start)) | start <- starts, start < end]
              nextOffset = offset+min 4194304 (end-offset)
          windows <- runSession pool $ S.pipeline $ traverse (\(start,stop) -> P.statement (identifier,start,stop) rangeStatement) ranges
          updated <- foldM (\digest ((start,stop),bytes) ->
            if fromIntegral (BS.length bytes) /= stop-start then invalid "Committed prefix is missing or discontinuous"
            else let next = hashUpdate digest bytes in next `seq` pure next) context (zip ranges windows)
          duringHash
          updated `seq` go nextOffset updated

-- | A fair, bounded metadata scan. Apply eligibility after this page, rather
-- than making PostgreSQL walk an unbounded population of old unfinished tasks.
type IdleSessionCursor = (UTCTime, UUID)

listIdleSessionCandidates :: Pool Hasql.Connection -> Maybe IdleSessionCursor -> IO [AgentSession]
listIdleSessionCandidates pool = listIdleSessionCandidatesBefore pool Nothing

listIdleSessionCandidatesAt :: Pool Hasql.Connection -> UTCTime -> Maybe IdleSessionCursor -> IO [AgentSession]
listIdleSessionCandidatesAt pool now = listIdleSessionCandidatesBefore pool (Just (addUTCTime (-3600) now))

listIdleSessionCandidatesBefore :: Pool Hasql.Connection -> Maybe UTCTime -> Maybe IdleSessionCursor -> IO [AgentSession]
listIdleSessionCandidatesBefore pool cutoff cursor = runSession pool $ S.statement (cutoff,cursor) $
  St.Statement
    "SELECT to_jsonb(s) FROM sessions s WHERE state='active' AND import_task_id IS NULL AND last_seen_at < coalesce($1,statement_timestamp()-interval '1 hour') AND ($2::timestamptz IS NULL OR (last_seen_at,id)>($2,$3)) ORDER BY last_seen_at,id LIMIT 32"
    (optional fst E.timestamptz <> optional (fmap fst . snd) E.timestamptz <> optional (fmap snd . snd) E.uuid)
    (D.rowList jsonRow) True

sealIdleSession :: Pool Hasql.Connection -> AgentSession -> IO (Maybe AgentSession)
sealIdleSession pool = sealIdleSessionWithHook pool (pure ())

-- | Hash immutable committed chunks without a session lock or a retained pool
-- connection. The hook is also useful to synchronize a concurrent writer in
-- tests: activity during hashing must survive the final locked recheck.
sealIdleSessionWithHook :: Pool Hasql.Connection -> IO () -> AgentSession -> IO (Maybe AgentSession)
sealIdleSessionWithHook pool = sealIdleSessionWithCutoff pool Nothing

-- | The supplied clock makes strict age boundaries deterministic without
-- weakening evidence guards or moving persisted activity backwards in tests.
sealIdleSessionWithClock :: Pool Hasql.Connection -> IO UTCTime -> IO () -> AgentSession -> IO (Maybe AgentSession)
sealIdleSessionWithClock pool clock = sealIdleSessionWithCutoff pool (Just (addUTCTime (-3600) <$> clock))

sealIdleSessionWithCutoff :: Pool Hasql.Connection -> Maybe (IO UTCTime) -> IO () -> AgentSession -> IO (Maybe AgentSession)
sealIdleSessionWithCutoff pool cutoffClock duringHash snapshot = do
  cutoff <- sequence cutoffClock
  eligible <- runSession pool $ S.statement (snapshot.id,cutoff) eligibleStatement
  if not eligible || snapshot.state /= Active then pure Nothing else do
    digest <- hashCommitted pool snapshot.id snapshot.committedBytes duringHash
    finalCutoff <- sequence cutoffClock
    runTransaction pool $ do
      current <- S.statement (snapshot.workspaceId,snapshot.id) $ St.Statement
        "SELECT to_jsonb(s) FROM sessions s WHERE workspace_id=$1 AND id=$2 FOR UPDATE"
        pairParams (D.rowMaybe (jsonRow :: D.Row AgentSession)) True
      case current of
        Just row | row.state == Active && row.lastSeenAt == snapshot.lastSeenAt
          && row.committedBytes == snapshot.committedBytes && row.revision == snapshot.revision -> do
            -- FOR UPDATE above conflicts with the FK KEY SHARE lock taken by
            -- a new association. Lock every actual linked task, including soft
            -- deleted rows, to serialize status changes with this decision.
            terminal <- S.statement row.id $ St.Statement
              "SELECT coalesce(bool_and(status IN ('done','cancelled')),true) FROM (SELECT t.status FROM tasks t JOIN task_sessions ts ON ts.task_id=t.id WHERE ts.session_id=$1 ORDER BY t.id FOR SHARE OF t) linked"
              (E.param (E.nonNullable E.uuid)) (D.singleRow (D.column (D.nonNullable D.bool))) True
            idle <- S.statement (row.id,finalCutoff) eligibleStatement
            if idle && terminal then
              Just <$> S.statement (row.workspaceId,row.id,digest) sealStatement
            else pure Nothing
        _ -> pure Nothing

eligibleStatement :: St.Statement (UUID,Maybe UTCTime) Bool
eligibleStatement = St.Statement
  "SELECT EXISTS(SELECT 1 FROM sessions s WHERE s.id=$1 AND s.state='active' AND s.import_task_id IS NULL AND s.last_seen_at < coalesce($2,clock_timestamp()-interval '1 hour') AND NOT EXISTS(SELECT 1 FROM task_sessions ts JOIN tasks t ON t.id=ts.task_id WHERE ts.session_id=s.id AND t.status NOT IN ('done','cancelled')))"
  (param fst E.uuid <> optional snd E.timestamptz) (D.singleRow (D.column (D.nonNullable D.bool))) True

sealStatement :: St.Statement (UUID,UUID,Text) AgentSession
sealStatement = St.Statement
  "UPDATE sessions s SET state='terminated',terminated_at=clock_timestamp(),final_bytes=committed_bytes,final_sha256=$3,revision=revision+1 WHERE workspace_id=$1 AND id=$2 RETURNING to_jsonb(s)"
  (param (\(a,_,_) -> a) E.uuid <> param (\(_,b,_) -> b) E.uuid <> param (\(_,_,c) -> c) E.text)
  (D.singleRow jsonRow) True

-- Boolean projections only: no transcript or descendant metadata leaves SQL.
workspaceSessionActivity :: Pool Hasql.Connection -> [UUID] -> IO [(UUID,Bool)]
workspaceSessionActivity pool identifiers = runSession pool $ S.statement identifiers $
  St.Statement "SELECT w.id,EXISTS(SELECT 1 FROM sessions s WHERE s.workspace_id=w.id AND s.state='active' AND s.import_task_id IS NULL) FROM workspaces w WHERE w.id=ANY($1) AND w.deleted_at IS NULL"
    (E.param (E.nonNullable (E.foldableArray (E.nonNullable E.uuid))))
    (D.rowList ((,) <$> D.column (D.nonNullable D.uuid) <*> D.column (D.nonNullable D.bool))) True

projectSessionActivity :: Pool Hasql.Connection -> UUID -> [UUID] -> IO [(UUID,Bool)]
projectSessionActivity pool workspace identifiers = runSession pool $ S.statement (workspace,identifiers) $
  St.Statement sql (param fst E.uuid <> param snd (E.foldableArray (E.nonNullable E.uuid)))
    (D.rowList ((,) <$> D.column (D.nonNullable D.uuid) <*> D.column (D.nonNullable D.bool))) True
  where
    sql = "WITH RECURSIVE project_tree(root,id) AS (\
      \ SELECT p.id,p.id FROM projects p JOIN workspaces w ON w.id=p.workspace_id WHERE p.workspace_id=$1 AND p.id=ANY($2) AND p.deleted_at IS NULL AND w.deleted_at IS NULL\
      \ UNION SELECT parent.root,child.id FROM projects child JOIN project_tree parent ON child.parent_id=parent.id WHERE child.workspace_id=$1 AND child.deleted_at IS NULL\
      \ ), task_tree(root,id) AS (\
      \ SELECT p.root,t.id FROM project_tree p JOIN tasks t ON t.project_id=p.id WHERE t.workspace_id=$1 AND t.deleted_at IS NULL\
      \ UNION SELECT parent.root,child.id FROM tasks child JOIN task_tree parent ON child.parent_id=parent.id WHERE child.workspace_id=$1 AND child.deleted_at IS NULL\
      \ ) SELECT p.id,EXISTS(SELECT 1 FROM task_tree t JOIN task_sessions link ON link.task_id=t.id JOIN sessions s ON s.id=link.session_id WHERE t.root=p.id AND s.workspace_id=$1 AND s.state='active' AND s.import_task_id IS NULL)\
      \ FROM projects p JOIN workspaces w ON w.id=p.workspace_id WHERE p.workspace_id=$1 AND p.id=ANY($2) AND p.deleted_at IS NULL AND w.deleted_at IS NULL"
