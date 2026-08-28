-- | Durable change-stream state machine.  It owns the snapshot/session/token
-- lifecycle so transports cannot accidentally compose paginated reads into an
-- unsafe resync hand-off.
module HMem.DB.ChangeStream
  ( ChangeScope(..), ChangeAudience(..), SnapshotToken(..), ResumeToken(..)
  , ChangeStreamError(..), OutboxRecord(..), SnapshotBegin(..), SnapshotPage(..), ReplayPage(..)
  , beginResync, beginResyncWithStartKey, readSnapshotPage, readSnapshotPageWithTtl, readSnapshotPageWithTtls, validateCanonicalResumeToken, replayAndRotateResumeToken, replayUnacknowledgedResumeToken, replacementResumeToken, acknowledgeReplayPage, rebaseResumeTokenAfterHidden
  , listOutboxAfter, listOutboxScopes, pruneOutboxBefore, cleanupChangeStream
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

data ChangeScope = WorkspaceScope !UUID | GlobalScope deriving stock (Show, Eq, Ord)

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

-- | Verify a terminal snapshot bearer before it is exchanged for a canonical
-- websocket ticket.  Unlike replay, validation does not rotate or expose a
-- cursor; it only proves the opaque token is still bound to this audience,
-- scope, authorization epoch, and a completed snapshot session. Its durable
-- expiry is returned as part of that proof so a transport cannot accidentally
-- mint a longer-lived connection capability from an aged bearer.
validateCanonicalResumeToken :: Pool Hasql.Connection -> ChangeScope -> ChangeAudience -> ResumeToken -> IO (Either ChangeStreamError UTCTime)
validateCanonicalResumeToken pool scope audience token = runTransaction pool $ do
  found <- Session.statement (tokenHash token.unResumeToken) lookupResumeForUpdateStatement
  case validateResumeIdentity scope audience found of
    Left err -> pure (Left err)
    Right (epoch, _watermark, expires, superseded, sessionHash) -> do
      (currentEpoch, _highWater, _retainedFrom) <- Session.statement (scopeName scope, scopeWorkspace scope) lockScopeReplayStatement
      now <- Session.statement () databaseClockStatement
      allowed <- authorized scope audience
      pure $ if expires <= now then Left ResumeExpired
        else if superseded /= Nothing then Left ResumeSuperseded
        else if sessionHash == Nothing then Left ResumeNotFound
        else if not allowed then Left ResyncUnauthorized
        else if currentEpoch /= epoch then Left ResumeSuperseded
        else Right expires

-- | The materializer runs only after the scope lock and authorization check,
-- within a serializable transaction.  Its rows become immutable ordinals
-- before this function returns a bearer token.
beginResync :: Pool Hasql.Connection -> NominalDiffTime -> ChangeScope -> ChangeAudience
  -> Session.Session [Value] -> IO (Either ChangeStreamError SnapshotBegin)
beginResync pool ttl scope audience materialize =
  beginResyncInternal pool ttl scope audience Nothing Nothing materialize

-- | Public starts bind an opaque client retry key and requested page size to
-- one durable session. Retrying the same start after response loss returns the
-- original bearer/session rather than materializing a second snapshot.
beginResyncWithStartKey :: Pool Hasql.Connection -> NominalDiffTime -> ChangeScope -> ChangeAudience
  -> Text -> Int -> Session.Session [Value] -> IO (Either ChangeStreamError SnapshotBegin)
beginResyncWithStartKey pool ttl scope audience startKey pageSize materialize =
  beginResyncInternal pool ttl scope audience (Just (tokenHash startKey)) (Just pageSize) materialize

beginResyncInternal :: Pool Hasql.Connection -> NominalDiffTime -> ChangeScope -> ChangeAudience
  -> Maybe ByteString -> Maybe Int -> Session.Session [Value] -> IO (Either ChangeStreamError SnapshotBegin)
beginResyncInternal pool ttl scope audience startHash pageSize materialize = do
  freshSessionHash <- tokenHash <$> newOpaque "hmem_snapshot_session_v1_"
  (retrySerializable 3 $ do
    let createSnapshot sessionHash epoch watermark = do
          let raw = snapshotTokenForSession sessionHash
          items <- materialize
          -- TTL begins after the snapshot is fully materialized. A slow
          -- materializer must not return an already-expired bearer.
          now <- Session.statement () databaseClockStatement
          Session.statement (sessionHash, scopeName scope, scopeWorkspace scope, audienceKind audience, audienceKey audience, audienceUser audience, epoch, watermark, addUTCTime ttl now, tokenHash raw.unSnapshotToken, fmap fromIntegral pageSize, startHash) insertSnapshotSessionStatement
          Session.statement (tokenHash raw.unSnapshotToken, sessionHash, 0) insertSnapshotPageTokenStatement
          forM_ (zip [0 :: Int64 ..] items) $ \(ordinal, item) ->
            Session.statement (sessionHash, ordinal, item) insertSnapshotItemStatement
          pure $ Right (SnapshotBegin raw watermark epoch)
    runSerializableTransaction pool $ do
      -- Serialize every start attempt on the scope before looking up its retry
      -- key.  Otherwise two concurrent response-loss retries can both observe
      -- an absent key and race the durable uniqueness constraint.
      (epoch, watermark) <- Session.statement (scopeName scope, scopeWorkspace scope) lockScopeStatement
      allowed <- authorized scope audience
      if not allowed
        then Session.sql "SELECT hmem_change_stream_abort_resync_unauthorized()" >> pure (Left ResyncUnauthorized)
        else do
          now <- Session.statement () databaseClockStatement
          existing <- case startHash of
            Nothing -> pure Nothing
            Just keyHash -> Session.statement (keyHash, scopeName scope, scopeWorkspace scope, audienceKind audience, audienceKey audience, audienceUser audience) lookupIdempotentStartStatement
          case existing of
            Just (sessionHash, existingEpoch, existingWatermark, expires, storedPageSize)
              | expires > now && storedPageSize /= fmap fromIntegral pageSize -> pure (Left SnapshotOutOfOrder)
              | expires > now -> pure (Right (SnapshotBegin (snapshotTokenForSession sessionHash) existingWatermark existingEpoch))
              | otherwise -> do
                  -- Expired retry state must not permanently reserve an opaque
                  -- client key.  Its full resume lineage is deleted before a
                  -- new server-random session is materialized, so a fresh
                  -- attempt cannot collide with an older still-live bearer.
                  Session.statement sessionHash deleteResumeTokensForSessionStatement
                  Session.statement sessionHash deleteSnapshotSessionStatement
                  createSnapshot freshSessionHash epoch watermark
            Nothing -> createSnapshot freshSessionHash epoch watermark)
  `catch` \case
    DBResyncUnauthorized -> pure (Left ResyncUnauthorized)
    err -> throwIO err

-- | Page reads do not hold a transaction across requests.  They nevertheless
-- recheck current authorization and epoch before exposing any saved item.
readSnapshotPageWithTtl :: Pool Hasql.Connection -> NominalDiffTime -> ChangeScope -> ChangeAudience -> SnapshotToken
  -> Int -> IO (Either ChangeStreamError SnapshotPage)
readSnapshotPageWithTtl pool ttl = readSnapshotPageWithTtls pool ttl ttl

-- | A snapshot session and its terminal replay bearer have deliberately
-- different lifetimes.  The page session is short-lived and idle-bound;
-- completing it issues the longer resume bearer without extending the session.
readSnapshotPageWithTtls :: Pool Hasql.Connection -> NominalDiffTime -> NominalDiffTime -> ChangeScope -> ChangeAudience -> SnapshotToken
  -> Int -> IO (Either ChangeStreamError SnapshotPage)
readSnapshotPageWithTtls pool _sessionTtl resumeTtl scope audience token requestedLimit = do
  let limit = fromIntegral (max 1 (min 1000 requestedLimit)) :: Int32
  runTransaction pool $ do
    let pageHash = tokenHash token.unSnapshotToken
    -- Select the immutable identity before locking. The actual page/session
    -- row is locked only after its scope, matching start's lock order.
    identity <- Session.statement pageHash lookupSnapshotIdentityStatement
    case identity of
      Nothing -> pure $ Left SnapshotNotFound
      Just claimed
        | not (sameSession scope audience claimed) -> pure $ Left SnapshotNotFound
        | otherwise -> do
            -- Both the bearer and its authorization scope are locked before
            -- taking the database clock.  A request that waited on either
            -- lock must not use a stale client-process timestamp to revive an
            -- already-expired page bearer.
            epoch <- Session.statement (scopeName scope, scopeWorkspace scope) currentEpochStatement
            found <- Session.statement pageHash lookupSnapshotStatement
            case found of
              Nothing -> pure $ Left SnapshotNotFound
              Just session
                | not (sameSession scope audience session) -> pure $ Left SnapshotNotFound
                | otherwise -> do
                    now <- Session.statement () databaseClockStatement
                    if session.snapshotPageSize /= Nothing && session.snapshotPageSize /= Just limit then pure (Left SnapshotOutOfOrder)
                    else if session.snapshotExpires <= now then pure (Left SnapshotExpired) else do
                      allowed <- authorized scope audience
                      if not allowed then pure (Left ResyncUnauthorized)
                      else if epoch /= session.snapshotEpoch then pure (Left SnapshotSuperseded) else do
                        case session.snapshotPageEnd of
                          Just endOrdinal -> cachedSnapshotPage session token endOrdinal
                          Nothing -> materializeSnapshotPage now resumeTtl epoch session token limit

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
      Right (epoch, watermark, expires, superseded, sessionHash) -> do
        (currentEpoch, highWater, retainedFrom) <- Session.statement (scopeName scope, scopeWorkspace scope) lockScopeReplayStatement
        -- The scope lock closes the authorization handoff.  Read wall-clock
        -- time only after both locks, and use it for both rejection and the
        -- replacement bearer so its TTL begins at the actual rotation point.
        now <- Session.statement () databaseClockStatement
        if expires <= now then pure (Left ResumeExpired)
        else if superseded /= Nothing then pure (Left ResumeSuperseded)
        else do
          allowed <- authorized scope audience
          if not allowed then pure (Left ResyncUnauthorized) else if currentEpoch /= epoch then pure (Left ResumeSuperseded) else do
            if watermark < retainedFrom - 1 then pure (Left ReplayRetentionPruned) else do
              records <- Session.statement (scopeName scope, scopeWorkspace scope, watermark, highWater, fromIntegral (max 1 (min 1000 requestedLimit)) :: Int32) listOutboxRangeStatement
              if highWater > watermark && null records then pure (Left ReplayRetentionPruned) else do
                let scannedCursor = maybe watermark outboxCursor (lastMay records)
                    hasMore = scannedCursor < highWater
                Session.statement (tokenHash newToken.unResumeToken, scopeName scope, scopeWorkspace scope, audienceKind audience, audienceKey audience, audienceUser audience, epoch, scannedCursor, addUTCTime ttl now, sessionHash) insertResumeStatement
                Session.statement (now, oldHash) supersedeResumeStatement
                pure $ Right $ ReplayPage records newToken hasMore

-- | Read a replay page without acknowledging it.  Canonical WebSocket
-- delivery uses this before writing frames, then calls 'acknowledgeReplayPage'
-- only after every frame was accepted by the socket.  A failed send therefore
-- leaves the old bearer usable for an at-least-once reconnect.
replayUnacknowledgedResumeToken :: Pool Hasql.Connection -> ChangeScope -> ChangeAudience
  -> ResumeToken -> Int -> IO (Either ChangeStreamError ReplayPage)
replayUnacknowledgedResumeToken pool scope audience token requestedLimit =
  runTransaction pool $ do
    found <- Session.statement (tokenHash token.unResumeToken) lookupResumeForUpdateStatement
    case validateResumeIdentity scope audience found of
      Left err -> pure (Left err)
      Right (epoch, watermark, expires, superseded, _sessionHash) -> do
        (currentEpoch, highWater, retainedFrom) <- Session.statement (scopeName scope, scopeWorkspace scope) lockScopeReplayStatement
        now <- Session.statement () databaseClockStatement
        if expires <= now then pure (Left ResumeExpired)
        else if superseded /= Nothing then pure (Left ResumeSuperseded)
        else do
          allowed <- authorized scope audience
          if not allowed then pure (Left ResyncUnauthorized)
          else if currentEpoch /= epoch then pure (Left ResumeSuperseded)
          else if watermark < retainedFrom - 1 then pure (Left ReplayRetentionPruned)
          else do
            records <- Session.statement (scopeName scope, scopeWorkspace scope, watermark, highWater, fromIntegral (max 1 (min 1000 requestedLimit)) :: Int32) listOutboxRangeStatement
            if highWater > watermark && null records then pure (Left ReplayRetentionPruned)
            else pure (Right (ReplayPage records token (maybe watermark outboxCursor (lastMay records) < highWater)))

-- | Durable acknowledgement following a successful canonical send.  It locks
-- the same bearer and scope used for the read, so a concurrent dispatcher
-- cannot supersede it between a client write and acknowledgement.
acknowledgeReplayPage :: Pool Hasql.Connection -> NominalDiffTime -> ChangeScope -> ChangeAudience
  -> ResumeToken -> Int64 -> IO (Either ChangeStreamError ResumeToken)
acknowledgeReplayPage pool ttl scope audience oldToken acknowledgedThrough = do
  let replacement = replacementResumeToken scope audience oldToken acknowledgedThrough
  let oldHash = tokenHash oldToken.unResumeToken
  runTransaction pool $ do
    found <- Session.statement oldHash lookupResumeForUpdateStatement
    case validateResumeIdentity scope audience found of
      Left err -> pure (Left err)
      Right (epoch, watermark, expires, superseded, sessionHash) -> do
        (currentEpoch, highWater, retainedFrom) <- Session.statement (scopeName scope, scopeWorkspace scope) lockScopeReplayStatement
        now <- Session.statement () databaseClockStatement
        if expires <= now then pure (Left ResumeExpired)
        else if superseded /= Nothing then pure (Left ResumeSuperseded)
        else do
          allowed <- authorized scope audience
          if not allowed then pure (Left ResyncUnauthorized)
          else if currentEpoch /= epoch || acknowledgedThrough < watermark || acknowledgedThrough > highWater || watermark < retainedFrom - 1
            then pure (Left ResumeSuperseded)
          else if acknowledgedThrough == watermark then pure (Right oldToken)
          else do
            Session.statement (tokenHash replacement.unResumeToken, scopeName scope, scopeWorkspace scope, audienceKind audience, audienceKey audience, audienceUser audience, epoch, acknowledgedThrough, addUTCTime ttl now, sessionHash) insertResumeStatement
            Session.statement (now, oldHash) supersedeResumeStatement
            pure (Right replacement)

-- | The successor is derived from the secret current bearer, its bound
-- scope/audience, and the durable acknowledged cursor. This lets the
-- transport write the final checkpoint before acknowledging it without making
-- a bearer valid in another identity context.
replacementResumeToken :: ChangeScope -> ChangeAudience -> ResumeToken -> Int64 -> ResumeToken
replacementResumeToken scope audience (ResumeToken old) cursor =
  ResumeToken $ resumePrefix <> digestText (old <> ":" <> scopeAudienceBinding scope audience <> ":" <> T.pack (show cursor))

-- | Advance an opaque bearer over a range the server has already projected as
-- invisible to its bound audience. This internal primitive never exposes
-- records or cursors: it only rechecks identity, expiry, retention, and the
-- current authorization before rotating the bearer to the supplied durable
-- cursor.
rebaseResumeTokenAfterHidden :: Pool Hasql.Connection -> NominalDiffTime -> ChangeScope -> ChangeAudience
  -> ResumeToken -> Int64 -> IO (Either ChangeStreamError ResumeToken)
rebaseResumeTokenAfterHidden pool _ttl scope audience oldToken hiddenThrough = do
  let oldHash = tokenHash oldToken.unResumeToken
  runTransaction pool $ do
    found <- Session.statement oldHash lookupResumeForUpdateStatement
    case validateResumeIdentity scope audience found of
      Left err -> pure (Left err)
      Right (_epoch, watermark, expires, superseded, _sessionHash) -> do
        (currentEpoch, highWater, retainedFrom) <- Session.statement (scopeName scope, scopeWorkspace scope) lockScopeReplayStatement
        now <- Session.statement () databaseClockStatement
        if expires <= now then pure (Left ResumeExpired)
        else if superseded /= Nothing then pure (Left ResumeSuperseded)
        else do
          allowed <- authorized scope audience
          if not allowed then pure (Left ResyncUnauthorized)
          else if hiddenThrough <= watermark then pure (Right oldToken)
          else if hiddenThrough > highWater || watermark < retainedFrom - 1
            then pure (Left ReplayRetentionPruned)
          else do
            -- This is an internal hidden-only acknowledgement. Keeping the
            -- same bearer and its original expiry prevents an invisible event
            -- from extending capability lifetime or creating an observable
            -- checkpoint cadence.
            Session.statement (currentEpoch, hiddenThrough, oldHash) rebaseResumeInPlaceStatement
            pure (Right oldToken)

listOutboxAfter :: Pool Hasql.Connection -> ChangeScope -> Int64 -> Int -> IO [OutboxRecord]
listOutboxAfter pool scope watermark requestedLimit =
  runSession pool $ Session.statement (scopeName scope, scopeWorkspace scope, watermark, fromIntegral (max 1 (min 1000 requestedLimit)) :: Int32) listOutboxAfterStatement

-- | Internal dispatcher primitive.  It is deliberately not a subscriber API:
-- callers receive the retained envelope including its cursor only to produce
-- an allowlisted legacy projection or schedule canonical replay.
-- | Enumerate scopes which currently have retained committed records.  The
-- dispatcher keeps an acknowledgement cursor for each of these independent
-- scope streams; a timestamp is not a safe cross-scope delivery watermark.
listOutboxScopes :: Pool Hasql.Connection -> IO [ChangeScope]
listOutboxScopes pool = runSession pool $ Session.statement () listOutboxScopesStatement
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
  , snapshotHash :: !ByteString, snapshotPageStart :: !Int64, snapshotPageEnd :: !(Maybe Int64), snapshotPageTerminal :: !Bool, snapshotPageSize :: !(Maybe Int32) }
sameSession :: ChangeScope -> ChangeAudience -> StoredSnapshot -> Bool
sameSession scope audience s =
  s.snapshotStoredScope == scopeName scope
    && s.snapshotStoredWorkspace == scopeWorkspace scope
    && s.snapshotStoredAudienceKind == audienceKind audience
    && s.snapshotStoredAudienceKey == audienceKey audience
    && s.snapshotStoredAudienceUser == audienceUser audience
validateResumeIdentity :: ChangeScope -> ChangeAudience
  -> Maybe (Text, Maybe UUID, Text, Text, Maybe UUID, Int64, Int64, UTCTime, Maybe UTCTime, Maybe ByteString) -> Either ChangeStreamError (Int64, Int64, UTCTime, Maybe UTCTime, Maybe ByteString)
validateResumeIdentity scope audience = \case
  Nothing -> Left ResumeNotFound
  Just (storedScope, storedWorkspace, storedAudienceKind, storedAudienceKey, storedAudienceUser, epoch, watermark, expires, superseded, sessionHash)
    | storedScope /= scopeName scope || storedWorkspace /= scopeWorkspace scope || storedAudienceKind /= audienceKind audience || storedAudienceKey /= audienceKey audience || storedAudienceUser /= audienceUser audience -> Left ResumeNotFound
    | otherwise -> Right (epoch, watermark, expires, superseded, sessionHash)
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
-- | The session identity is generated by the server from CSPRNG material and
-- stored only as an internal hash/lineage key.  This lets a response-loss retry
-- reconstruct its original opaque bearer without deriving a capability from a
-- caller supplied idempotency key.
snapshotTokenForSession :: ByteString -> SnapshotToken
snapshotTokenForSession sessionHash =
  SnapshotToken $ snapshotPrefix <> digestText (T.pack (show sessionHash))
nextSnapshotToken :: SnapshotToken -> SnapshotToken
nextSnapshotToken (SnapshotToken raw) = SnapshotToken $ snapshotPrefix <> digestText raw
scopeAudienceBinding :: ChangeScope -> ChangeAudience -> Text
scopeAudienceBinding scope audience =
  scopeName scope <> ":" <> maybe "" UUID.toText (scopeWorkspace scope)
    <> ":" <> audienceKind audience <> ":" <> audienceKey audience
    <> ":" <> maybe "" UUID.toText (audienceUser audience)
digestText :: Text -> Text
digestText = T.pack . show . (hash . TE.encodeUtf8 :: Text -> Digest SHA256)
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
insertSnapshotSessionStatement :: Statement.Statement (ByteString, Text, Maybe UUID, Text, Text, Maybe UUID, Int64, Int64, UTCTime, ByteString, Maybe Int32, Maybe ByteString) ()
insertSnapshotSessionStatement = Statement.Statement
  "INSERT INTO change_stream_snapshot_sessions(session_hash, scope, workspace_id, audience_kind, audience_key, audience_user_id, authorization_epoch, high_watermark, expires_at, page_token_hash, page_size, start_idempotency_hash) VALUES ($1,$2,$3,$4,$5,$6,$7,$8,$9,$10,$11,$12)"
  (contramap (\(a,_,_,_,_,_,_,_,_,_,_,_) -> a) (Enc.param (Enc.nonNullable Enc.bytea)) <> contramap (\(_,b,_,_,_,_,_,_,_,_,_,_) -> b) (Enc.param (Enc.nonNullable Enc.text)) <> contramap (\(_,_,c,_,_,_,_,_,_,_,_,_) -> c) (Enc.param (Enc.nullable Enc.uuid)) <> contramap (\(_,_,_,d,_,_,_,_,_,_,_,_) -> d) (Enc.param (Enc.nonNullable Enc.text)) <> contramap (\(_,_,_,_,e,_,_,_,_,_,_,_) -> e) (Enc.param (Enc.nonNullable Enc.text)) <> contramap (\(_,_,_,_,_,f,_,_,_,_,_,_) -> f) (Enc.param (Enc.nullable Enc.uuid)) <> contramap (\(_,_,_,_,_,_,g,_,_,_,_,_) -> g) (Enc.param (Enc.nonNullable Enc.int8)) <> contramap (\(_,_,_,_,_,_,_,h,_,_,_,_) -> h) (Enc.param (Enc.nonNullable Enc.int8)) <> contramap (\(_,_,_,_,_,_,_,_,i,_,_,_) -> i) (Enc.param (Enc.nonNullable Enc.timestamptz)) <> contramap (\(_,_,_,_,_,_,_,_,_,j,_,_) -> j) (Enc.param (Enc.nonNullable Enc.bytea)) <> contramap (\(_,_,_,_,_,_,_,_,_,_,k,_) -> k) (Enc.param (Enc.nullable Enc.int4)) <> contramap (\(_,_,_,_,_,_,_,_,_,_,_,l) -> l) (Enc.param (Enc.nullable Enc.bytea))) Dec.noResult True
insertSnapshotItemStatement :: Statement.Statement (ByteString, Int64, Value) ()
insertSnapshotItemStatement = Statement.Statement "INSERT INTO change_stream_snapshot_items(session_hash, ordinal, item) VALUES ($1,$2,$3)"
  (contramap (\(a,_,_) -> a) (Enc.param (Enc.nonNullable Enc.bytea)) <> contramap (\(_,b,_) -> b) (Enc.param (Enc.nonNullable Enc.int8)) <> contramap (\(_,_,c) -> c) (Enc.param (Enc.nonNullable Enc.jsonb))) Dec.noResult True
lookupSnapshotStatement :: Statement.Statement ByteString (Maybe StoredSnapshot)
lookupSnapshotStatement = Statement.Statement
  "SELECT s.scope, s.workspace_id, s.audience_kind, s.audience_key, s.audience_user_id, s.authorization_epoch, s.high_watermark, s.expires_at, s.session_hash, p.start_ordinal, p.end_ordinal, p.terminal, s.page_size FROM change_stream_snapshot_page_tokens p JOIN change_stream_snapshot_sessions s ON s.session_hash = p.session_hash WHERE p.token_hash = $1 FOR UPDATE OF s, p"
  (Enc.param (Enc.nonNullable Enc.bytea))
  (Dec.rowMaybe (StoredSnapshot <$> Dec.column (Dec.nonNullable Dec.text) <*> Dec.column (Dec.nullable Dec.uuid) <*> Dec.column (Dec.nonNullable Dec.text) <*> Dec.column (Dec.nonNullable Dec.text) <*> Dec.column (Dec.nullable Dec.uuid) <*> Dec.column (Dec.nonNullable Dec.int8) <*> Dec.column (Dec.nonNullable Dec.int8) <*> Dec.column (Dec.nonNullable Dec.timestamptz) <*> Dec.column (Dec.nonNullable Dec.bytea) <*> Dec.column (Dec.nonNullable Dec.int8) <*> Dec.column (Dec.nullable Dec.int8) <*> Dec.column (Dec.nonNullable Dec.bool) <*> Dec.column (Dec.nullable Dec.int4))) True
lookupSnapshotIdentityStatement :: Statement.Statement ByteString (Maybe StoredSnapshot)
lookupSnapshotIdentityStatement = Statement.Statement
  "SELECT s.scope, s.workspace_id, s.audience_kind, s.audience_key, s.audience_user_id, s.authorization_epoch, s.high_watermark, s.expires_at, s.session_hash, p.start_ordinal, p.end_ordinal, p.terminal, s.page_size FROM change_stream_snapshot_page_tokens p JOIN change_stream_snapshot_sessions s ON s.session_hash = p.session_hash WHERE p.token_hash = $1"
  (Enc.param (Enc.nonNullable Enc.bytea))
  (Dec.rowMaybe (StoredSnapshot <$> Dec.column (Dec.nonNullable Dec.text) <*> Dec.column (Dec.nullable Dec.uuid) <*> Dec.column (Dec.nonNullable Dec.text) <*> Dec.column (Dec.nonNullable Dec.text) <*> Dec.column (Dec.nullable Dec.uuid) <*> Dec.column (Dec.nonNullable Dec.int8) <*> Dec.column (Dec.nonNullable Dec.int8) <*> Dec.column (Dec.nonNullable Dec.timestamptz) <*> Dec.column (Dec.nonNullable Dec.bytea) <*> Dec.column (Dec.nonNullable Dec.int8) <*> Dec.column (Dec.nullable Dec.int8) <*> Dec.column (Dec.nonNullable Dec.bool) <*> Dec.column (Dec.nullable Dec.int4))) True

lookupIdempotentStartStatement :: Statement.Statement (ByteString, Text, Maybe UUID, Text, Text, Maybe UUID) (Maybe (ByteString, Int64, Int64, UTCTime, Maybe Int32))
lookupIdempotentStartStatement = Statement.Statement
  "SELECT session_hash, authorization_epoch, high_watermark, expires_at, page_size FROM change_stream_snapshot_sessions WHERE start_idempotency_hash=$1 AND scope=$2 AND workspace_id IS NOT DISTINCT FROM $3 AND audience_kind=$4 AND audience_key=$5 AND audience_user_id IS NOT DISTINCT FROM $6 FOR UPDATE"
  (contramap (\(a,_,_,_,_,_) -> a) (Enc.param (Enc.nonNullable Enc.bytea)) <> contramap (\(_,b,_,_,_,_) -> b) (Enc.param (Enc.nonNullable Enc.text)) <> contramap (\(_,_,c,_,_,_) -> c) (Enc.param (Enc.nullable Enc.uuid)) <> contramap (\(_,_,_,d,_,_) -> d) (Enc.param (Enc.nonNullable Enc.text)) <> contramap (\(_,_,_,_,e,_) -> e) (Enc.param (Enc.nonNullable Enc.text)) <> contramap (\(_,_,_,_,_,f) -> f) (Enc.param (Enc.nullable Enc.uuid)))
  (Dec.rowMaybe ((,,,,) <$> Dec.column (Dec.nonNullable Dec.bytea) <*> Dec.column (Dec.nonNullable Dec.int8) <*> Dec.column (Dec.nonNullable Dec.int8) <*> Dec.column (Dec.nonNullable Dec.timestamptz) <*> Dec.column (Dec.nullable Dec.int4))) True
deleteSnapshotSessionStatement :: Statement.Statement ByteString ()
deleteSnapshotSessionStatement = Statement.Statement
  "DELETE FROM change_stream_snapshot_sessions WHERE session_hash = $1"
  (Enc.param (Enc.nonNullable Enc.bytea)) Dec.noResult True
deleteResumeTokensForSessionStatement :: Statement.Statement ByteString ()
deleteResumeTokensForSessionStatement = Statement.Statement
  "DELETE FROM change_stream_resume_tokens WHERE session_hash = $1"
  (Enc.param (Enc.nonNullable Enc.bytea)) Dec.noResult True
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
lookupResumeForUpdateStatement :: Statement.Statement ByteString (Maybe (Text, Maybe UUID, Text, Text, Maybe UUID, Int64, Int64, UTCTime, Maybe UTCTime, Maybe ByteString))
lookupResumeForUpdateStatement = Statement.Statement "SELECT scope, workspace_id, audience_kind, audience_key, audience_user_id, authorization_epoch, watermark, expires_at, superseded_at, session_hash FROM change_stream_resume_tokens WHERE token_hash = $1 FOR UPDATE"
  (Enc.param (Enc.nonNullable Enc.bytea))
  (Dec.rowMaybe ((,,,,,,,,,) <$> Dec.column (Dec.nonNullable Dec.text) <*> Dec.column (Dec.nullable Dec.uuid) <*> Dec.column (Dec.nonNullable Dec.text) <*> Dec.column (Dec.nonNullable Dec.text) <*> Dec.column (Dec.nullable Dec.uuid) <*> Dec.column (Dec.nonNullable Dec.int8) <*> Dec.column (Dec.nonNullable Dec.int8) <*> Dec.column (Dec.nonNullable Dec.timestamptz) <*> Dec.column (Dec.nullable Dec.timestamptz) <*> Dec.column (Dec.nullable Dec.bytea))) True
supersedeResumeStatement :: Statement.Statement (UTCTime, ByteString) ()
supersedeResumeStatement = Statement.Statement "UPDATE change_stream_resume_tokens SET superseded_at = $1 WHERE token_hash = $2"
  ((contramap fst $ Enc.param $ Enc.nonNullable Enc.timestamptz) <> (contramap snd $ Enc.param $ Enc.nonNullable Enc.bytea)) Dec.noResult True
rebaseResumeInPlaceStatement :: Statement.Statement (Int64, Int64, ByteString) ()
rebaseResumeInPlaceStatement = Statement.Statement
  "UPDATE change_stream_resume_tokens SET authorization_epoch=$1, watermark=$2 WHERE token_hash=$3"
  (contramap (\(a,_,_) -> a) (Enc.param (Enc.nonNullable Enc.int8)) <> contramap (\(_,b,_) -> b) (Enc.param (Enc.nonNullable Enc.int8)) <> contramap (\(_,_,c) -> c) (Enc.param (Enc.nonNullable Enc.bytea))) Dec.noResult True
listOutboxAfterStatement :: Statement.Statement (Text, Maybe UUID, Int64, Int32) [OutboxRecord]
listOutboxAfterStatement = Statement.Statement "SELECT event_id, cursor, occurred_at, envelope FROM change_stream_outbox WHERE scope = $1 AND workspace_id IS NOT DISTINCT FROM $2 AND cursor > $3 ORDER BY cursor ASC LIMIT $4"
  (contramap (\(a,_,_,_) -> a) (Enc.param (Enc.nonNullable Enc.text)) <> contramap (\(_,b,_,_) -> b) (Enc.param (Enc.nullable Enc.uuid)) <> contramap (\(_,_,c,_) -> c) (Enc.param (Enc.nonNullable Enc.int8)) <> contramap (\(_,_,_,d) -> d) (Enc.param (Enc.nonNullable Enc.int4)))
  (Dec.rowList (OutboxRecord <$> Dec.column (Dec.nonNullable Dec.uuid) <*> Dec.column (Dec.nonNullable Dec.int8) <*> Dec.column (Dec.nonNullable Dec.timestamptz) <*> Dec.column (Dec.nonNullable Dec.jsonb))) True
listOutboxScopesStatement :: Statement.Statement () [ChangeScope]
listOutboxScopesStatement = Statement.Statement
  "SELECT DISTINCT scope, workspace_id FROM change_stream_outbox ORDER BY scope, workspace_id"
  Enc.noParams
  (Dec.rowList (decodeScope <$> Dec.column (Dec.nonNullable Dec.text) <*> Dec.column (Dec.nullable Dec.uuid))) True

decodeScope :: Text -> Maybe UUID -> ChangeScope
decodeScope "workspace" (Just workspace) = WorkspaceScope workspace
decodeScope _ _ = GlobalScope
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
cleanupSessionsStatement = Statement.Statement "WITH expired AS (SELECT session_hash FROM change_stream_snapshot_sessions WHERE expires_at <= $1 FOR UPDATE), resumes AS (DELETE FROM change_stream_resume_tokens WHERE session_hash IN (SELECT session_hash FROM expired)), deleted AS (DELETE FROM change_stream_snapshot_sessions WHERE session_hash IN (SELECT session_hash FROM expired) RETURNING 1) SELECT count(*)::bigint FROM deleted"
  (Enc.param (Enc.nonNullable Enc.timestamptz)) (Dec.singleRow (Dec.column (Dec.nonNullable Dec.int8))) True
