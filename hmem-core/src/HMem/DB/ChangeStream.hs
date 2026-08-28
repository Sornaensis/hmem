-- | Durable change-stream state machine.  It owns the snapshot/session/token
-- lifecycle so transports cannot accidentally compose paginated reads into an
-- unsafe resync hand-off.
module HMem.DB.ChangeStream
  ( ChangeScope(..), ChangeAudience(..), SnapshotToken(..), ResumeToken(..)
  , ChangeStreamError(..), OutboxRecord(..), SnapshotBegin(..), SnapshotPage(..), ReplayPage(..)
  , beginResync, readSnapshotPage, readSnapshotPageWithTtl, replayAndRotateResumeToken
  , listOutboxAfter, pruneOutboxBefore, cleanupChangeStream
  ) where

import Control.Monad (forM_)
import Control.Exception (catch, throwIO)
import Crypto.Hash (Digest, SHA256, hash)
import Data.Aeson (Value)
import Data.ByteString (ByteString)
import Data.Functor.Contravariant (contramap)
import Data.Int (Int32, Int64)
import Data.Pool (Pool)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time (NominalDiffTime, UTCTime, addUTCTime)
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID
import Hasql.Connection qualified as Hasql
import Hasql.Decoders qualified as Dec
import Hasql.Encoders qualified as Enc
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement

import HMem.DB.Pool (DBException(..), runSerializableTransaction, runSession, runTransaction)

data ChangeScope = WorkspaceScope !UUID | GlobalScope deriving stock (Show, Eq)

-- | A normal audience is bound to an authenticated user.  The trusted case is
-- reserved for private in-process/system callers, never a public header.
data ChangeAudience = AuthenticatedAudience !Text !UUID | TrustedAudience !Text
  deriving stock (Show, Eq)
newtype SnapshotToken = SnapshotToken { unSnapshotToken :: Text } deriving stock (Show, Eq)
newtype ResumeToken = ResumeToken { unResumeToken :: Text } deriving stock (Show, Eq)
data ChangeStreamError = ResyncUnauthorized | SnapshotNotFound | SnapshotExpired | SnapshotOutOfOrder
  | SnapshotSuperseded | ResumeNotFound | ResumeExpired | ResumeSuperseded | ReplayRetentionPruned
  deriving stock (Show, Eq)
data OutboxRecord = OutboxRecord
  { outboxEventId :: !UUID, outboxCursor :: !Int64, outboxOccurredAt :: !UTCTime, outboxEnvelope :: !Value
  } deriving stock (Show, Eq)
data SnapshotBegin = SnapshotBegin
  { snapshotToken :: !SnapshotToken, snapshotHighWatermark :: !Int64, snapshotAuthorizationEpoch :: !Int64
  } deriving stock (Show, Eq)
data SnapshotPage = SnapshotPage
  { snapshotPageItems :: ![Value], snapshotPageHasMore :: !Bool
  , snapshotNextToken :: !(Maybe SnapshotToken), snapshotResumeToken :: !(Maybe ResumeToken)
  } deriving stock (Show, Eq)
data ReplayPage = ReplayPage
  { replayPageRecords :: ![OutboxRecord], replayPageResumeToken :: !ResumeToken
  , replayPageHasMore :: !Bool
  } deriving stock (Show, Eq)

-- | The materializer runs only after the scope lock and authorization check,
-- within a serializable transaction.  Its rows become immutable ordinals
-- before this function returns a bearer token.
beginResync :: Pool Hasql.Connection -> NominalDiffTime -> ChangeScope -> ChangeAudience
  -> Session.Session [Value] -> IO (Either ChangeStreamError SnapshotBegin)
beginResync pool ttl scope audience materialize =
  (retrySerializable 3 $ do
    raw <- SnapshotToken <$> newOpaque snapshotPrefix
    let rawHash = tokenHash raw.unSnapshotToken
    runSerializableTransaction pool $ do
      (epoch, watermark) <- Session.statement (scopeName scope, scopeWorkspace scope) lockScopeStatement
      allowed <- authorized scope audience
      if not allowed
        then Session.sql "SELECT hmem_change_stream_abort_resync_unauthorized()" >> pure (Left ResyncUnauthorized)
        else do
          items <- materialize
          -- Take PostgreSQL's wall clock only after this transaction has both
          -- waited for the scope lock and materialized its immutable rows.
          -- The pending-session TTL therefore starts at actual creation, not
          -- at a client-process time from before a lock wait.
          now <- Session.statement () databaseClockStatement
          Session.statement (rawHash, scopeName scope, scopeWorkspace scope, audienceKind audience, audienceKey audience, audienceUser audience, epoch, watermark, addUTCTime ttl now, rawHash) insertSnapshotSessionStatement
          Session.statement (rawHash, rawHash, 0) insertSnapshotPageTokenStatement
          forM_ (zip [0 :: Int64 ..] items) $ \(ordinal, item) ->
            Session.statement (rawHash, ordinal, item) insertSnapshotItemStatement
          pure $ Right (SnapshotBegin raw watermark epoch)) `catch` \case
            DBResyncUnauthorized -> pure (Left ResyncUnauthorized)
            err -> throwIO err

-- | Page reads do not hold a transaction across requests.  They nevertheless
-- recheck current authorization and epoch before exposing any saved item.
readSnapshotPageWithTtl :: Pool Hasql.Connection -> NominalDiffTime -> ChangeScope -> ChangeAudience -> SnapshotToken
  -> Int -> IO (Either ChangeStreamError SnapshotPage)
readSnapshotPageWithTtl pool ttl scope audience token requestedLimit = do
  let limit = fromIntegral (max 1 (min 1000 requestedLimit)) :: Int32
  runTransaction pool $ do
    let pageHash = tokenHash token.unSnapshotToken
    found <- Session.statement pageHash lookupSnapshotStatement
    case found of
      Nothing -> pure $ Left SnapshotNotFound
      Just session
        | not (sameSession scope audience session) -> pure $ Left SnapshotNotFound
        | otherwise -> do
            -- Both the bearer and its authorization scope are locked before
            -- taking the database clock.  A request that waited on either
            -- lock must not use a stale client-process timestamp to revive an
            -- already-expired page bearer.
            epoch <- Session.statement (scopeName scope, scopeWorkspace scope) currentEpochStatement
            now <- Session.statement () databaseClockStatement
            if session.snapshotExpires <= now then pure (Left SnapshotExpired) else do
              allowed <- authorized scope audience
              if not allowed || epoch /= session.snapshotEpoch then pure (Left ResyncUnauthorized) else do
                case session.snapshotPageEnd of
                  Just endOrdinal -> cachedSnapshotPage session token endOrdinal
                  Nothing -> materializeSnapshotPage now ttl epoch session token limit

-- Compatibility adapter for the former offset API.  The offset is accepted
-- only to preserve source compatibility; persisted opaque page state, rather
-- than caller-selected offsets, now controls progression.
readSnapshotPage :: Pool Hasql.Connection -> ChangeScope -> ChangeAudience -> SnapshotToken
  -> Int64 -> Int -> IO (Either ChangeStreamError SnapshotPage)
readSnapshotPage pool scope audience token _offset requestedLimit =
  readSnapshotPageWithTtl pool 60 scope audience token requestedLimit

-- A page bearer remains valid for the whole session.  The first use records
-- its immutable ordinal range; a response lost after commit can therefore be
-- retried without a caller-selected offset or an accidental second advance.
cachedSnapshotPage :: StoredSnapshot -> SnapshotToken -> Int64 -> Session.Session (Either ChangeStreamError SnapshotPage)
cachedSnapshotPage session token endOrdinal = do
  page <- Session.statement (session.snapshotHash, session.snapshotPageStart, endOrdinal) snapshotRangeStatement
  if session.snapshotPageTerminal
    then pure $ Right $ SnapshotPage page False Nothing (Just (ResumeToken (resumeForSnapshot token.unSnapshotToken)))
    else pure $ Right $ SnapshotPage page True (Just (nextSnapshotToken token)) Nothing

materializeSnapshotPage :: UTCTime -> NominalDiffTime -> Int64 -> StoredSnapshot -> SnapshotToken -> Int32
  -> Session.Session (Either ChangeStreamError SnapshotPage)
materializeSnapshotPage now ttl epoch session token limit = do
  rows <- Session.statement (session.snapshotHash, session.snapshotPageStart, limit + 1) snapshotPageStatement
  let page = take (fromIntegral limit) rows
      endOrdinal = session.snapshotPageStart + fromIntegral (length page)
      pageHash = tokenHash token.unSnapshotToken
  if length rows > fromIntegral limit then do
    let next = nextSnapshotToken token
    Session.statement (pageHash, endOrdinal, session.snapshotHash) advanceSnapshotPageStatement
    Session.statement (tokenHash next.unSnapshotToken, session.snapshotHash, endOrdinal) insertSnapshotPageTokenStatement
    pure $ Right $ SnapshotPage page True (Just next) Nothing
  else do
    let resume = ResumeToken (resumeForSnapshot token.unSnapshotToken)
    Session.statement (tokenHash resume.unResumeToken, session.snapshotStoredScope, session.snapshotStoredWorkspace, session.snapshotStoredAudienceKind, session.snapshotStoredAudienceKey, session.snapshotStoredAudienceUser, epoch, session.snapshotWatermark, addUTCTime ttl now, Just session.snapshotHash) insertResumeStatement
    Session.statement (pageHash, endOrdinal, session.snapshotHash) terminalSnapshotPageStatement
    Session.statement (tokenHash resume.unResumeToken, session.snapshotHash) terminalSnapshotStatement
    pure $ Right $ SnapshotPage page False Nothing (Just resume)

-- | A replay response and its replacement bearer are one transaction: the old
-- token is locked, scanned through a fixed high-water mark, then superseded
-- only after the replacement has been inserted successfully.
replayAndRotateResumeToken :: Pool Hasql.Connection -> NominalDiffTime -> ChangeScope -> ChangeAudience
  -> ResumeToken -> Int -> IO (Either ChangeStreamError ReplayPage)
replayAndRotateResumeToken pool ttl scope audience oldToken requestedLimit = do
  newToken <- ResumeToken <$> newOpaque resumePrefix
  let oldHash = tokenHash oldToken.unResumeToken
  runTransaction pool $ do
    found <- Session.statement oldHash lookupResumeForUpdateStatement
    case validateResumeIdentity scope audience found of
      Left err -> pure (Left err)
      Right (epoch, watermark, expires, superseded) -> do
        (currentEpoch, highWater, retainedFrom) <- Session.statement (scopeName scope, scopeWorkspace scope) lockScopeReplayStatement
        -- The scope lock closes the authorization handoff.  Read wall-clock
        -- time only after both locks, and use it for both rejection and the
        -- replacement bearer so its TTL begins at the actual rotation point.
        now <- Session.statement () databaseClockStatement
        if expires <= now then pure (Left ResumeExpired)
        else if superseded /= Nothing then pure (Left ResumeSuperseded)
        else do
          allowed <- authorized scope audience
          if not allowed || currentEpoch /= epoch then pure (Left ResyncUnauthorized) else do
            if watermark < retainedFrom - 1 then pure (Left ReplayRetentionPruned) else do
              records <- Session.statement (scopeName scope, scopeWorkspace scope, watermark, highWater, fromIntegral (max 1 (min 1000 requestedLimit)) :: Int32) listOutboxRangeStatement
              if highWater > watermark && null records then pure (Left ReplayRetentionPruned) else do
                let scannedCursor = maybe watermark outboxCursor (lastMay records)
                    hasMore = scannedCursor < highWater
                Session.statement (tokenHash newToken.unResumeToken, scopeName scope, scopeWorkspace scope, audienceKind audience, audienceKey audience, audienceUser audience, epoch, scannedCursor, addUTCTime ttl now, Nothing) insertResumeStatement
                Session.statement (now, oldHash) supersedeResumeStatement
                pure $ Right $ ReplayPage records newToken hasMore

listOutboxAfter :: Pool Hasql.Connection -> ChangeScope -> Int64 -> Int -> IO [OutboxRecord]
listOutboxAfter pool scope watermark requestedLimit =
  runSession pool $ Session.statement (scopeName scope, scopeWorkspace scope, watermark, fromIntegral (max 1 (min 1000 requestedLimit)) :: Int32) listOutboxAfterStatement
pruneOutboxBefore :: Pool Hasql.Connection -> UTCTime -> IO Int64
pruneOutboxBefore pool cutoff = runTransaction pool $ Session.statement cutoff pruneOutboxBeforeStatement
cleanupChangeStream :: Pool Hasql.Connection -> UTCTime -> IO (Int64, Int64)
cleanupChangeStream pool now = runTransaction pool $ do
  tokens <- Session.statement now cleanupTokensStatement
  sessions <- Session.statement now cleanupSessionsStatement
  pure (tokens, sessions)

data StoredSnapshot = StoredSnapshot
  { snapshotStoredScope :: !Text, snapshotStoredWorkspace :: !(Maybe UUID)
  , snapshotStoredAudienceKind :: !Text, snapshotStoredAudienceKey :: !Text, snapshotStoredAudienceUser :: !(Maybe UUID)
  , snapshotEpoch :: !Int64, snapshotWatermark :: !Int64, snapshotExpires :: !UTCTime
  , snapshotHash :: !ByteString, snapshotPageStart :: !Int64, snapshotPageEnd :: !(Maybe Int64), snapshotPageTerminal :: !Bool }
sameSession :: ChangeScope -> ChangeAudience -> StoredSnapshot -> Bool
sameSession scope audience s =
  s.snapshotStoredScope == scopeName scope
    && s.snapshotStoredWorkspace == scopeWorkspace scope
    && s.snapshotStoredAudienceKind == audienceKind audience
    && s.snapshotStoredAudienceKey == audienceKey audience
    && s.snapshotStoredAudienceUser == audienceUser audience
validateResumeIdentity :: ChangeScope -> ChangeAudience
  -> Maybe (Text, Maybe UUID, Text, Text, Maybe UUID, Int64, Int64, UTCTime, Maybe UTCTime) -> Either ChangeStreamError (Int64, Int64, UTCTime, Maybe UTCTime)
validateResumeIdentity scope audience = \case
  Nothing -> Left ResumeNotFound
  Just (storedScope, storedWorkspace, storedAudienceKind, storedAudienceKey, storedAudienceUser, epoch, watermark, expires, superseded)
    | storedScope /= scopeName scope || storedWorkspace /= scopeWorkspace scope || storedAudienceKind /= audienceKind audience || storedAudienceKey /= audienceKey audience || storedAudienceUser /= audienceUser audience -> Left ResumeNotFound
    | otherwise -> Right (epoch, watermark, expires, superseded)
authorized :: ChangeScope -> ChangeAudience -> Session.Session Bool
authorized scope audience = Session.statement (scopeName scope, scopeWorkspace scope, audienceUser audience, audienceTrusted audience) authorizeStatement
scopeName :: ChangeScope -> Text
scopeName (WorkspaceScope _) = "workspace"
scopeName GlobalScope = "global"
scopeWorkspace :: ChangeScope -> Maybe UUID
scopeWorkspace (WorkspaceScope id') = Just id'
scopeWorkspace GlobalScope = Nothing
audienceKey :: ChangeAudience -> Text
audienceKey (AuthenticatedAudience key _) = key
audienceKey (TrustedAudience key) = key
audienceKind :: ChangeAudience -> Text
audienceKind AuthenticatedAudience{} = "authenticated"
audienceKind TrustedAudience{} = "trusted"
audienceUser :: ChangeAudience -> Maybe UUID
audienceUser (AuthenticatedAudience _ userId) = Just userId
audienceUser (TrustedAudience _) = Nothing
audienceTrusted :: ChangeAudience -> Bool
audienceTrusted TrustedAudience{} = True
audienceTrusted _ = False
snapshotPrefix, resumePrefix :: Text
snapshotPrefix = "hmem_snapshot_v1_"
resumePrefix = "hmem_resume_v1_"
resumeForSnapshot :: Text -> Text
resumeForSnapshot raw = resumePrefix <> T.drop (T.length snapshotPrefix) raw
nextSnapshotToken :: SnapshotToken -> SnapshotToken
nextSnapshotToken (SnapshotToken raw) = SnapshotToken $ snapshotPrefix <> T.pack (show (hash (TE.encodeUtf8 raw) :: Digest SHA256))
lastMay :: [a] -> Maybe a
lastMay [] = Nothing
lastMay xs = Just (last xs)
outboxCursor :: OutboxRecord -> Int64
outboxCursor = (.outboxCursor)
newOpaque :: Text -> IO Text
newOpaque prefix = do
  a <- UUID.nextRandom
  b <- UUID.nextRandom
  pure (prefix <> UUID.toText a <> UUID.toText b)
tokenHash :: Text -> ByteString
tokenHash token = TE.encodeUtf8 $ T.pack $ show (hash (TE.encodeUtf8 token) :: Digest SHA256)

-- Hasql keeps serializable failures typed at the pool boundary.  The concrete
-- retry policy is deliberately bounded and attempt-local; materialization is
-- run again only after PostgreSQL has rolled the failed attempt back.
retrySerializable :: Int -> IO a -> IO a
retrySerializable attempts action = action `catch` \err -> case err of
  DBSerializationFailure | attempts > 1 -> retrySerializable (attempts - 1) action
  _ -> throwIO err

lockScopeStatement :: Statement.Statement (Text, Maybe UUID) (Int64, Int64)
lockScopeStatement = Statement.Statement "SELECT authorization_epoch, next_cursor FROM hmem_change_stream_lock($1, $2)"
  ((contramap fst $ Enc.param $ Enc.nonNullable Enc.text) <> (contramap snd $ Enc.param $ Enc.nullable Enc.uuid))
  (Dec.singleRow ((,) <$> Dec.column (Dec.nonNullable Dec.int8) <*> Dec.column (Dec.nonNullable Dec.int8))) True
lockScopeReplayStatement :: Statement.Statement (Text, Maybe UUID) (Int64, Int64, Int64)
lockScopeReplayStatement = Statement.Statement "SELECT authorization_epoch, next_cursor, retained_from_cursor FROM hmem_change_stream_lock($1, $2)"
  ((contramap fst $ Enc.param $ Enc.nonNullable Enc.text) <> (contramap snd $ Enc.param $ Enc.nullable Enc.uuid))
  (Dec.singleRow ((,,) <$> Dec.column (Dec.nonNullable Dec.int8) <*> Dec.column (Dec.nonNullable Dec.int8) <*> Dec.column (Dec.nonNullable Dec.int8))) True
currentEpochStatement :: Statement.Statement (Text, Maybe UUID) Int64
currentEpochStatement = Statement.Statement "SELECT authorization_epoch FROM hmem_change_stream_lock($1, $2)"
  ((contramap fst $ Enc.param $ Enc.nonNullable Enc.text) <> (contramap snd $ Enc.param $ Enc.nullable Enc.uuid))
  (Dec.singleRow (Dec.column (Dec.nonNullable Dec.int8))) True
databaseClockStatement :: Statement.Statement () UTCTime
databaseClockStatement = Statement.Statement "SELECT clock_timestamp()"
  Enc.noParams (Dec.singleRow (Dec.column (Dec.nonNullable Dec.timestamptz))) True
authorizeStatement :: Statement.Statement (Text, Maybe UUID, Maybe UUID, Bool) Bool
authorizeStatement = Statement.Statement
  "SELECT CASE WHEN $4 THEN true WHEN $1 = 'global' THEN EXISTS (SELECT 1 FROM users WHERE id = $3 AND disabled_at IS NULL AND is_superadmin) ELSE EXISTS (SELECT 1 FROM users u JOIN workspaces w ON w.id = $2 WHERE u.id = $3 AND u.disabled_at IS NULL AND w.deleted_at IS NULL AND (u.is_superadmin OR EXISTS (SELECT 1 FROM workspace_memberships wm WHERE wm.workspace_id = w.id AND wm.user_id = u.id))) END"
  (contramap (\(a,_,_,_) -> a) (Enc.param (Enc.nonNullable Enc.text)) <> contramap (\(_,b,_,_) -> b) (Enc.param (Enc.nullable Enc.uuid)) <> contramap (\(_,_,c,_) -> c) (Enc.param (Enc.nullable Enc.uuid)) <> contramap (\(_,_,_,d) -> d) (Enc.param (Enc.nonNullable Enc.bool)))
  (Dec.singleRow (Dec.column (Dec.nonNullable Dec.bool))) True
insertSnapshotSessionStatement :: Statement.Statement (ByteString, Text, Maybe UUID, Text, Text, Maybe UUID, Int64, Int64, UTCTime, ByteString) ()
insertSnapshotSessionStatement = Statement.Statement
  "INSERT INTO change_stream_snapshot_sessions(session_hash, scope, workspace_id, audience_kind, audience_key, audience_user_id, authorization_epoch, high_watermark, expires_at, page_token_hash) VALUES ($1,$2,$3,$4,$5,$6,$7,$8,$9,$10)"
  (contramap (\(a,_,_,_,_,_,_,_,_,_) -> a) (Enc.param (Enc.nonNullable Enc.bytea)) <> contramap (\(_,b,_,_,_,_,_,_,_,_) -> b) (Enc.param (Enc.nonNullable Enc.text)) <> contramap (\(_,_,c,_,_,_,_,_,_,_) -> c) (Enc.param (Enc.nullable Enc.uuid)) <> contramap (\(_,_,_,d,_,_,_,_,_,_) -> d) (Enc.param (Enc.nonNullable Enc.text)) <> contramap (\(_,_,_,_,e,_,_,_,_,_) -> e) (Enc.param (Enc.nonNullable Enc.text)) <> contramap (\(_,_,_,_,_,f,_,_,_,_) -> f) (Enc.param (Enc.nullable Enc.uuid)) <> contramap (\(_,_,_,_,_,_,g,_,_,_) -> g) (Enc.param (Enc.nonNullable Enc.int8)) <> contramap (\(_,_,_,_,_,_,_,h,_,_) -> h) (Enc.param (Enc.nonNullable Enc.int8)) <> contramap (\(_,_,_,_,_,_,_,_,i,_) -> i) (Enc.param (Enc.nonNullable Enc.timestamptz)) <> contramap (\(_,_,_,_,_,_,_,_,_,j) -> j) (Enc.param (Enc.nonNullable Enc.bytea))) Dec.noResult True
insertSnapshotItemStatement :: Statement.Statement (ByteString, Int64, Value) ()
insertSnapshotItemStatement = Statement.Statement "INSERT INTO change_stream_snapshot_items(session_hash, ordinal, item) VALUES ($1,$2,$3)"
  (contramap (\(a,_,_) -> a) (Enc.param (Enc.nonNullable Enc.bytea)) <> contramap (\(_,b,_) -> b) (Enc.param (Enc.nonNullable Enc.int8)) <> contramap (\(_,_,c) -> c) (Enc.param (Enc.nonNullable Enc.jsonb))) Dec.noResult True
lookupSnapshotStatement :: Statement.Statement ByteString (Maybe StoredSnapshot)
lookupSnapshotStatement = Statement.Statement
  "SELECT s.scope, s.workspace_id, s.audience_kind, s.audience_key, s.audience_user_id, s.authorization_epoch, s.high_watermark, s.expires_at, s.session_hash, p.start_ordinal, p.end_ordinal, p.terminal FROM change_stream_snapshot_page_tokens p JOIN change_stream_snapshot_sessions s ON s.session_hash = p.session_hash WHERE p.token_hash = $1 FOR UPDATE OF s, p"
  (Enc.param (Enc.nonNullable Enc.bytea))
  (Dec.rowMaybe (StoredSnapshot <$> Dec.column (Dec.nonNullable Dec.text) <*> Dec.column (Dec.nullable Dec.uuid) <*> Dec.column (Dec.nonNullable Dec.text) <*> Dec.column (Dec.nonNullable Dec.text) <*> Dec.column (Dec.nullable Dec.uuid) <*> Dec.column (Dec.nonNullable Dec.int8) <*> Dec.column (Dec.nonNullable Dec.int8) <*> Dec.column (Dec.nonNullable Dec.timestamptz) <*> Dec.column (Dec.nonNullable Dec.bytea) <*> Dec.column (Dec.nonNullable Dec.int8) <*> Dec.column (Dec.nullable Dec.int8) <*> Dec.column (Dec.nonNullable Dec.bool))) True
snapshotPageStatement :: Statement.Statement (ByteString, Int64, Int32) [Value]
snapshotPageStatement = Statement.Statement "SELECT item FROM change_stream_snapshot_items WHERE session_hash = $1 AND ordinal >= $2 ORDER BY ordinal ASC LIMIT $3"
  (contramap (\(a,_,_) -> a) (Enc.param (Enc.nonNullable Enc.bytea)) <> contramap (\(_,b,_) -> b) (Enc.param (Enc.nonNullable Enc.int8)) <> contramap (\(_,_,c) -> c) (Enc.param (Enc.nonNullable Enc.int4))) (Dec.rowList (Dec.column (Dec.nonNullable Dec.jsonb))) True
snapshotRangeStatement :: Statement.Statement (ByteString, Int64, Int64) [Value]
snapshotRangeStatement = Statement.Statement "SELECT item FROM change_stream_snapshot_items WHERE session_hash = $1 AND ordinal >= $2 AND ordinal < $3 ORDER BY ordinal ASC"
  (contramap (\(a,_,_) -> a) (Enc.param (Enc.nonNullable Enc.bytea)) <> contramap (\(_,b,_) -> b) (Enc.param (Enc.nonNullable Enc.int8)) <> contramap (\(_,_,c) -> c) (Enc.param (Enc.nonNullable Enc.int8))) (Dec.rowList (Dec.column (Dec.nonNullable Dec.jsonb))) True
insertSnapshotPageTokenStatement :: Statement.Statement (ByteString, ByteString, Int64) ()
insertSnapshotPageTokenStatement = Statement.Statement "INSERT INTO change_stream_snapshot_page_tokens(token_hash, session_hash, start_ordinal) VALUES ($1,$2,$3)"
  (contramap (\(a,_,_) -> a) (Enc.param (Enc.nonNullable Enc.bytea)) <> contramap (\(_,b,_) -> b) (Enc.param (Enc.nonNullable Enc.bytea)) <> contramap (\(_,_,c) -> c) (Enc.param (Enc.nonNullable Enc.int8))) Dec.noResult True
insertResumeStatement :: Statement.Statement (ByteString, Text, Maybe UUID, Text, Text, Maybe UUID, Int64, Int64, UTCTime, Maybe ByteString) ()
insertResumeStatement = Statement.Statement
  "INSERT INTO change_stream_resume_tokens(token_hash, scope, workspace_id, audience_kind, audience_key, audience_user_id, authorization_epoch, watermark, expires_at, session_hash) VALUES ($1,$2,$3,$4,$5,$6,$7,$8,$9,$10)"
  (contramap (\(a,_,_,_,_,_,_,_,_,_) -> a) (Enc.param (Enc.nonNullable Enc.bytea)) <> contramap (\(_,b,_,_,_,_,_,_,_,_) -> b) (Enc.param (Enc.nonNullable Enc.text)) <> contramap (\(_,_,c,_,_,_,_,_,_,_) -> c) (Enc.param (Enc.nullable Enc.uuid)) <> contramap (\(_,_,_,d,_,_,_,_,_,_) -> d) (Enc.param (Enc.nonNullable Enc.text)) <> contramap (\(_,_,_,_,e,_,_,_,_,_) -> e) (Enc.param (Enc.nonNullable Enc.text)) <> contramap (\(_,_,_,_,_,f,_,_,_,_) -> f) (Enc.param (Enc.nullable Enc.uuid)) <> contramap (\(_,_,_,_,_,_,g,_,_,_) -> g) (Enc.param (Enc.nonNullable Enc.int8)) <> contramap (\(_,_,_,_,_,_,_,h,_,_) -> h) (Enc.param (Enc.nonNullable Enc.int8)) <> contramap (\(_,_,_,_,_,_,_,_,i,_) -> i) (Enc.param (Enc.nonNullable Enc.timestamptz)) <> contramap (\(_,_,_,_,_,_,_,_,_,j) -> j) (Enc.param (Enc.nullable Enc.bytea))) Dec.noResult True
advanceSnapshotPageStatement :: Statement.Statement (ByteString, Int64, ByteString) ()
advanceSnapshotPageStatement = Statement.Statement "UPDATE change_stream_snapshot_page_tokens SET end_ordinal = $2, terminal = false WHERE token_hash = $1 AND session_hash = $3"
  (contramap (\(a,_,_) -> a) (Enc.param (Enc.nonNullable Enc.bytea)) <> contramap (\(_,b,_) -> b) (Enc.param (Enc.nonNullable Enc.int8)) <> contramap (\(_,_,c) -> c) (Enc.param (Enc.nonNullable Enc.bytea))) Dec.noResult True
terminalSnapshotPageStatement :: Statement.Statement (ByteString, Int64, ByteString) ()
terminalSnapshotPageStatement = Statement.Statement "UPDATE change_stream_snapshot_page_tokens SET end_ordinal = $2, terminal = true WHERE token_hash = $1 AND session_hash = $3"
  (contramap (\(a,_,_) -> a) (Enc.param (Enc.nonNullable Enc.bytea)) <> contramap (\(_,b,_) -> b) (Enc.param (Enc.nonNullable Enc.int8)) <> contramap (\(_,_,c) -> c) (Enc.param (Enc.nonNullable Enc.bytea))) Dec.noResult True
terminalSnapshotStatement :: Statement.Statement (ByteString, ByteString) ()
terminalSnapshotStatement = Statement.Statement "UPDATE change_stream_snapshot_sessions SET terminal_at = now(), resume_token_hash = $1 WHERE session_hash = $2"
  ((contramap fst $ Enc.param $ Enc.nonNullable Enc.bytea) <> (contramap snd $ Enc.param $ Enc.nonNullable Enc.bytea)) Dec.noResult True
lookupResumeForUpdateStatement :: Statement.Statement ByteString (Maybe (Text, Maybe UUID, Text, Text, Maybe UUID, Int64, Int64, UTCTime, Maybe UTCTime))
lookupResumeForUpdateStatement = Statement.Statement "SELECT scope, workspace_id, audience_kind, audience_key, audience_user_id, authorization_epoch, watermark, expires_at, superseded_at FROM change_stream_resume_tokens WHERE token_hash = $1 FOR UPDATE"
  (Enc.param (Enc.nonNullable Enc.bytea))
  (Dec.rowMaybe ((,,,,,,,,) <$> Dec.column (Dec.nonNullable Dec.text) <*> Dec.column (Dec.nullable Dec.uuid) <*> Dec.column (Dec.nonNullable Dec.text) <*> Dec.column (Dec.nonNullable Dec.text) <*> Dec.column (Dec.nullable Dec.uuid) <*> Dec.column (Dec.nonNullable Dec.int8) <*> Dec.column (Dec.nonNullable Dec.int8) <*> Dec.column (Dec.nonNullable Dec.timestamptz) <*> Dec.column (Dec.nullable Dec.timestamptz))) True
supersedeResumeStatement :: Statement.Statement (UTCTime, ByteString) ()
supersedeResumeStatement = Statement.Statement "UPDATE change_stream_resume_tokens SET superseded_at = $1 WHERE token_hash = $2"
  ((contramap fst $ Enc.param $ Enc.nonNullable Enc.timestamptz) <> (contramap snd $ Enc.param $ Enc.nonNullable Enc.bytea)) Dec.noResult True
listOutboxAfterStatement :: Statement.Statement (Text, Maybe UUID, Int64, Int32) [OutboxRecord]
listOutboxAfterStatement = Statement.Statement "SELECT event_id, cursor, occurred_at, envelope FROM change_stream_outbox WHERE scope = $1 AND workspace_id IS NOT DISTINCT FROM $2 AND cursor > $3 ORDER BY cursor ASC LIMIT $4"
  (contramap (\(a,_,_,_) -> a) (Enc.param (Enc.nonNullable Enc.text)) <> contramap (\(_,b,_,_) -> b) (Enc.param (Enc.nullable Enc.uuid)) <> contramap (\(_,_,c,_) -> c) (Enc.param (Enc.nonNullable Enc.int8)) <> contramap (\(_,_,_,d) -> d) (Enc.param (Enc.nonNullable Enc.int4)))
  (Dec.rowList (OutboxRecord <$> Dec.column (Dec.nonNullable Dec.uuid) <*> Dec.column (Dec.nonNullable Dec.int8) <*> Dec.column (Dec.nonNullable Dec.timestamptz) <*> Dec.column (Dec.nonNullable Dec.jsonb))) True
listOutboxRangeStatement :: Statement.Statement (Text, Maybe UUID, Int64, Int64, Int32) [OutboxRecord]
listOutboxRangeStatement = Statement.Statement "SELECT event_id, cursor, occurred_at, envelope FROM change_stream_outbox WHERE scope = $1 AND workspace_id IS NOT DISTINCT FROM $2 AND cursor > $3 AND cursor <= $4 ORDER BY cursor ASC LIMIT $5"
  (contramap (\(a,_,_,_,_) -> a) (Enc.param (Enc.nonNullable Enc.text)) <> contramap (\(_,b,_,_,_) -> b) (Enc.param (Enc.nullable Enc.uuid)) <> contramap (\(_,_,c,_,_) -> c) (Enc.param (Enc.nonNullable Enc.int8)) <> contramap (\(_,_,_,d,_) -> d) (Enc.param (Enc.nonNullable Enc.int8)) <> contramap (\(_,_,_,_,e) -> e) (Enc.param (Enc.nonNullable Enc.int4)))
  (Dec.rowList (OutboxRecord <$> Dec.column (Dec.nonNullable Dec.uuid) <*> Dec.column (Dec.nonNullable Dec.int8) <*> Dec.column (Dec.nonNullable Dec.timestamptz) <*> Dec.column (Dec.nonNullable Dec.jsonb))) True
pruneOutboxBeforeStatement :: Statement.Statement UTCTime Int64
pruneOutboxBeforeStatement = Statement.Statement "SELECT hmem_change_stream_prune_outbox($1)"
  (Enc.param (Enc.nonNullable Enc.timestamptz)) (Dec.singleRow (Dec.column (Dec.nonNullable Dec.int8))) True
cleanupTokensStatement :: Statement.Statement UTCTime Int64
cleanupTokensStatement = Statement.Statement "WITH deleted AS (DELETE FROM change_stream_resume_tokens WHERE expires_at <= $1 OR superseded_at IS NOT NULL RETURNING 1) SELECT count(*)::bigint FROM deleted"
  (Enc.param (Enc.nonNullable Enc.timestamptz)) (Dec.singleRow (Dec.column (Dec.nonNullable Dec.int8))) True
cleanupSessionsStatement :: Statement.Statement UTCTime Int64
cleanupSessionsStatement = Statement.Statement "WITH deleted AS (DELETE FROM change_stream_snapshot_sessions WHERE expires_at <= $1 RETURNING 1) SELECT count(*)::bigint FROM deleted"
  (Enc.param (Enc.nonNullable Enc.timestamptz)) (Dec.singleRow (Dec.column (Dec.nonNullable Dec.int8))) True
