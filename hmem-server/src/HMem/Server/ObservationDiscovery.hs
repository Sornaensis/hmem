-- | Private query preparation for authorized Observation discovery. The caller
-- authorizes the repository workspace before invoking either operation. No
-- vectors, input text, endpoints or provider configuration cross the HTTP API.
module HMem.Server.ObservationDiscovery
  ( ObservationDiscovery
  , DiscoveryQuery(..)
  , DiscoveryReason(..)
  , DiscoveryCapability(..)
  , newObservationDiscovery
  , publishDiscoveryProvider
  , prepareDiscoveryQuery
  , discoveryCapability
  , discoveryTimeoutMicros
  , StoredDiscoveryContext(..)
  , DiscoveryContextError(..)
  , newObservationDiscoveryWithClock
  , newObservationDiscoveryWithClockAndBound
  , discoveryContextBound
  , createDiscoveryContext
  , createDiscoveryContextInSession
  , lookupDiscoveryContext
  , retireDiscoveryContext
  , discoveryQueryCapability
  , discoverySemantic
  , discoveryContextCount
  ) where

import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVar, writeTVar)
import Control.Exception (SomeAsyncException, SomeException, catch, evaluate, fromException, throwIO, onException, mask)
import Data.Aeson (ToJSON(..), object, (.=))
import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Time (UTCTime, getCurrentTime, addUTCTime)
import Data.Vector.Unboxed qualified as VU
import Data.UUID (UUID)
import Data.UUID.V4 qualified as UUID
import System.Timeout (timeout)

import HMem.DB.RequestTiming (timeProviderCall)
import HMem.Server.Embedding.GteQwen2
import HMem.Server.Embedding.Provider
import HMem.Types (ObservationDiscoveryFilter)

data ObservationDiscovery = ObservationDiscovery !Bool !(TVar (Integer, Maybe EmbeddingProvider))
  !(TVar Contexts) !(IO UTCTime) !Int

-- Retained contexts with an expiry-ordered index (expiry and eviction) and a
-- per-session index (generation supersession), so admission, lookup and
-- pruning cost O(log n) and write only when something changes. At most one
-- context exists per owner/workspace/search session.
data Contexts = Contexts
  { byToken :: !(Map UUID StoredDiscoveryContext)
  , byExpiry :: !(Set (UTCTime, UUID))
  , bySession :: !(Map (Text, UUID, UUID) UUID) }

emptyContexts :: Contexts
emptyContexts = Contexts Map.empty Set.empty Map.empty

sessionKey :: StoredDiscoveryContext -> Maybe (Text, UUID, UUID)
sessionKey entry = (\(key,_) -> (entry.owner, entry.workspace, key)) <$> entry.sessionGeneration

insertContext :: StoredDiscoveryContext -> Contexts -> Contexts
insertContext entry (Contexts tokens expiry sessions) = Contexts
  (Map.insert entry.token entry tokens)
  (Set.insert (entry.expiresAt, entry.token) expiry)
  (maybe sessions (\key -> Map.insert key entry.token sessions) (sessionKey entry))

deleteContext :: UUID -> Contexts -> Contexts
deleteContext token state@(Contexts tokens expiry sessions) = case Map.lookup token tokens of
  Nothing -> state
  Just entry -> Contexts (Map.delete token tokens) (Set.delete (entry.expiresAt, token) expiry)
    (maybe sessions (\key -> Map.update (\current -> if current == token then Nothing else Just current) key sessions) (sessionKey entry))

-- Drop contexts whose absolute expiry has passed; Nothing when none has.
expireContexts :: UTCTime -> Contexts -> Maybe Contexts
expireContexts now state = case Set.lookupMin state.byExpiry of
  Just (expiresAt, _) | expiresAt <= now ->
    let (due, _) = Set.spanAntitone (\(at, _) -> at <= now) state.byExpiry
    in Just (foldr (deleteContext . snd) state (Set.toList due))
  _ -> Nothing

-- Keep at most the given number of contexts, evicting those that expire
-- earliest. Never rejects a new context.
evictWithin :: Int -> Contexts -> Contexts
evictWithin limit state
  | Map.size state.byToken <= max 0 limit = state
  | otherwise = case Set.lookupMin state.byExpiry of
      Just (_, token) -> evictWithin limit (deleteContext token state)
      Nothing -> state

-- No result catalog or public vector serialization is retained. Contexts are
-- not capped per audience (2026-11-17 user decision); memory is bounded by
-- their absolute expiry, supersession and 'discoveryContextBound'. The
-- provider's own admission gate protects it from a keystroke flood.
data StoredDiscoveryContext = StoredDiscoveryContext
  { token :: !UUID, owner :: !Text, workspace :: !UUID
  , sessionGeneration :: !(Maybe (UUID,Int))
  , filterInputs :: !ObservationDiscoveryFilter, expiresAt :: !UTCTime
  , preparedQuery :: !(Maybe DiscoveryQuery)
  } deriving stock (Eq)

data DiscoveryContextError = DiscoveryContextUnavailable
  deriving stock (Eq, Show)

-- | Retained contexts across all audiences, a memory backstop. A worst-case
-- hybrid context holds a 1536-dimension unboxed Double vector (12 KiB), three
-- 4 KiB filter texts and small bindings: under 32 KiB, so at most about
-- 256 MiB. Creating one beyond it evicts the context that expires earliest,
-- including a pending reservation; the evicted context behaves as expired.
discoveryContextBound :: Int
discoveryContextBound = 8192

instance Show StoredDiscoveryContext where
  show value = "StoredDiscoveryContext {token = " <> show value.token <> ", query = <redacted>}"

data DiscoveryReason
  = DiscoveryDisabled | DiscoveryProviderUnavailable | DiscoveryStorageUnavailable
  | DiscoveryNoCompatibleVectors | DiscoveryTimedOut | DiscoveryInvalidResponse
  | DiscoveryRetired | DiscoveryInputTooLong | DiscoveryEmptyQuery
  deriving stock (Eq, Show)

-- | The hybrid vector is server-private and always in the pinned exact space.
data DiscoveryQuery
  = DiscoveryFts !DiscoveryReason
  | DiscoveryHybrid !(VU.Vector Double) !Text
  deriving stock (Eq)

-- | The semantic component for a discovery statement, if any.
discoverySemantic :: DiscoveryQuery -> Maybe ([Double], Text)
discoverySemantic = \case
  DiscoveryFts _ -> Nothing
  DiscoveryHybrid vector space -> Just (VU.toList vector, space)

instance Show DiscoveryQuery where
  show (DiscoveryFts reason) = "DiscoveryFts " <> show reason
  show (DiscoveryHybrid _ _) = "DiscoveryHybrid <redacted>"

newtype DiscoveryCapability = DiscoveryCapability (Maybe DiscoveryReason)
  deriving stock (Eq, Show)

instance ToJSON DiscoveryCapability where
  toJSON (DiscoveryCapability reason) = object
    [ "mode" .= (case reason of Nothing -> "hybrid"; Just _ -> "fts_only" :: Text)
    , "reason" .= fmap reasonText reason
    ]

reasonText :: DiscoveryReason -> Text
reasonText = \case
  DiscoveryDisabled -> "disabled"
  DiscoveryProviderUnavailable -> "provider_unavailable"
  DiscoveryStorageUnavailable -> "storage_unavailable"
  DiscoveryNoCompatibleVectors -> "no_compatible_vectors"
  DiscoveryTimedOut -> "timed_out"
  DiscoveryInvalidResponse -> "invalid_response"
  DiscoveryRetired -> "retired"
  DiscoveryInputTooLong -> "input_too_long"
  DiscoveryEmptyQuery -> "empty_query"

discoveryTimeoutMicros :: Int
discoveryTimeoutMicros = 2000000

newObservationDiscovery :: Bool -> IO ObservationDiscovery
newObservationDiscovery enabled = newObservationDiscoveryWithClock enabled getCurrentTime

newObservationDiscoveryWithClock :: Bool -> IO UTCTime -> IO ObservationDiscovery
newObservationDiscoveryWithClock enabled clock = newObservationDiscoveryWithClockAndBound enabled clock discoveryContextBound

-- | Test seam: a lower retained-context bound.
newObservationDiscoveryWithClockAndBound :: Bool -> IO UTCTime -> Int -> IO ObservationDiscovery
newObservationDiscoveryWithClockAndBound enabled clock bound =
  ObservationDiscovery enabled <$> newTVarIO (0, Nothing) <*> newTVarIO emptyContexts <*> pure clock <*> pure (max 1 bound)

-- | Retained contexts, including pending reservations and tombstones.
discoveryContextCount :: ObservationDiscovery -> IO Int
discoveryContextCount (ObservationDiscovery _ _ contexts _ _) = Map.size . (.byToken) <$> atomically (readTVar contexts)

-- | Publish the existing validated provider, including its transport admission
-- gate. Withdrawal precedes provider invalidation/join. An old withdrawal
-- cannot clear a replacement, even if both use the same endpoint.
publishDiscoveryProvider :: ObservationDiscovery -> EmbeddingProvider -> IO (IO ())
publishDiscoveryProvider (ObservationDiscovery _ state _ _ _) provider = atomically $ do
  (generation, _) <- readTVar state
  let next = generation + 1
  writeTVar state (next, Just provider)
  pure $ atomically $ do
    (current, _) <- readTVar state
    if current == next then writeTVar state (current + 1, Nothing) else pure ()

-- | The storage probe returns Nothing without optional vector storage, Just
-- False without same-space rows, or Just True. It never provisions storage.
-- Authorization must complete before supplying this action. Capability reads
-- do not embed text. Cached provider availability may revalidate under the
-- same gate/deadline; no second provider or transport is constructed here.
discoveryCapability :: ObservationDiscovery -> IO (Maybe Bool) -> IO DiscoveryCapability
discoveryCapability service storage = do
  result <- bounded $ withAvailable service storage $ \_ -> pure (DiscoveryHybrid VU.empty "")
  pure (discoveryQueryCapability result)

discoveryQueryCapability :: DiscoveryQuery -> DiscoveryCapability
discoveryQueryCapability result = DiscoveryCapability $ case result of
    DiscoveryFts reason -> Just reason
    DiscoveryHybrid _ _ -> Nothing

-- | Call for every nonempty body query, irrespective of whether FTS has hits.
-- FTS remains authoritative on every optional-path failure. Keep the formatted
-- request within the existing compact provider profile (2048 bytes with EOS).
prepareDiscoveryQuery :: ObservationDiscovery -> IO (Maybe Bool) -> Text -> IO DiscoveryQuery
prepareDiscoveryQuery service storage query = bounded $
  if T.null (T.strip query) then pure (DiscoveryFts DiscoveryEmptyQuery)
  else if BS.length (TE.encodeUtf8 (gteQwen2QueryPrefix <> query)) + 1 > 2048
    then pure (DiscoveryFts DiscoveryInputTooLong)
    else withAvailable service storage $ \provider ->
      embedGteQwen2 provider (EmbeddingRequest [EmbeddingInput EmbeddingQuery query]) >>= \case
        Left (GteQwen2ProviderError failure) -> pure (DiscoveryFts (failureReason failure))
        Left _ -> pure (DiscoveryFts DiscoveryInvalidResponse)
        Right batch -> case batch.vectors of
          [vector] -> do
            -- Force validation and every normalized coordinate inside the
            -- deadline; a lazy result must not escape the bounded operation.
            -- The retained copy is unboxed (8 bytes per coordinate).
            packed <- evaluate (VU.fromList vector)
            pure (DiscoveryHybrid packed batch.spaceFingerprint)
          _ -> pure (DiscoveryFts DiscoveryInvalidResponse)

withAvailable
  :: ObservationDiscovery -> IO (Maybe Bool)
  -> (EmbeddingProvider -> IO DiscoveryQuery) -> IO DiscoveryQuery
withAvailable (ObservationDiscovery enabled state _ _ _) storage action
  | not enabled = pure (DiscoveryFts DiscoveryDisabled)
  | otherwise = do
      (generation, provider) <- atomically (readTVar state)
      case provider of
        Nothing -> pure (DiscoveryFts DiscoveryProviderUnavailable)
        Just current -> do
          result <- storage >>= \case
            Nothing -> pure (DiscoveryFts DiscoveryStorageUnavailable)
            Just False -> pure (DiscoveryFts DiscoveryNoCompatibleVectors)
            -- Provider availability and embedding time; the storage probe is database time.
            Just True -> timeProviderCall $ current.availability >>= \case
              EmbeddingDisabled -> pure (DiscoveryFts DiscoveryDisabled)
              EmbeddingUnavailable failure -> pure (DiscoveryFts (failureReason failure))
              EmbeddingAvailable -> action current
          (latest, _) <- atomically (readTVar state)
          pure $ if latest == generation then result else DiscoveryFts DiscoveryRetired

failureReason :: EmbeddingFailure -> DiscoveryReason
failureReason failure = case failure.errorCode of
  ProviderDisabled -> DiscoveryDisabled
  ProviderTimedOut -> DiscoveryTimedOut
  ProviderCancelled -> DiscoveryRetired
  ProviderProtocolError -> DiscoveryInvalidResponse
  _ -> DiscoveryProviderUnavailable

bounded :: IO DiscoveryQuery -> IO DiscoveryQuery
bounded action = do
  result <- timeout discoveryTimeoutMicros (action `catch` redact)
  pure $ maybe (DiscoveryFts DiscoveryTimedOut) id result
  where
    redact (failure :: SomeException)
      | Just (_ :: SomeAsyncException) <- fromException failure = throwIO failure
      | otherwise = pure (DiscoveryFts DiscoveryProviderUnavailable)

-- | Authorize before calling. Owner is resolved from the authenticated
-- principal and effective actor, never from a client-supplied actor string.
-- The entire normalized filter is pinned; continuations carry selectors only.
createDiscoveryContext
  :: ObservationDiscovery -> Text -> UUID -> ObservationDiscoveryFilter -> Maybe UUID
  -> IO DiscoveryQuery -> IO (Either DiscoveryContextError StoredDiscoveryContext)
createDiscoveryContext service audience workspaceId filters predecessor =
  createDiscoveryContextInSession service audience workspaceId filters predecessor Nothing

-- | The client knows its session/generation before the initial response. A new
-- generation retires that session's pending or completed predecessor without
-- needing its server token. A cancelled reservation retains only a bounded
-- tombstone so a delayed older request cannot become authoritative again.
createDiscoveryContextInSession
  :: ObservationDiscovery -> Text -> UUID -> ObservationDiscoveryFilter -> Maybe UUID
  -> Maybe (UUID,Int) -> IO DiscoveryQuery -> IO (Either DiscoveryContextError StoredDiscoveryContext)
createDiscoveryContextInSession (ObservationDiscovery _ _ contexts clock bound) audience workspaceId filters predecessor session prepare = mask $ \restore -> do
  now <- clock
  contextId <- UUID.nextRandom
  let reserved = StoredDiscoveryContext contextId audience workspaceId session filters (addUTCTime 300 now) Nothing
  admitted <- atomically $ do
    stored <- readTVar contexts
    let previous = maybe stored id (expireContexts now stored)
        validOwner entry = entry.owner == audience && entry.workspace == workspaceId
        predecessorEntry = predecessor >>= (`Map.lookup` previous.byToken)
        foreignPredecessor = maybe False (not . validOwner) predecessorEntry
        incompatiblePredecessor = maybe False (\entry -> case entry.sessionGeneration of
          Nothing -> False
          Just (oldSession,_) -> case session of
            Just (newSession,_) -> oldSession /= newSession
            Nothing -> True) predecessorEntry
        -- The single retained context of this owner/workspace/session, if any.
        peer = session >>= \(key,_) -> Map.lookup (audience, workspaceId, key) previous.bySession
        outdated = case (session, peer >>= (`Map.lookup` previous.byToken)) of
          (Just (_,nextGeneration), Just entry) -> maybe False ((>= nextGeneration) . snd) entry.sessionGeneration
          _ -> False
        withoutPeer = maybe previous (`deleteContext` previous) peer
        entries = maybe withoutPeer (`deleteContext` withoutPeer) predecessor
    if foreignPredecessor || incompatiblePredecessor || outdated
      then mapM_ (writeTVar contexts) (expireContexts now stored) >> pure (Left DiscoveryContextUnavailable)
      else writeTVar contexts (insertContext reserved (evictWithin (bound - 1) entries)) >> pure (Right ())
  case admitted of
    Left failure -> pure (Left failure)
    Right () -> do
      let abandon = atomically $
            -- Keyed tombstones retain the high-water generation until its
            -- absolute expiry; the next edit reuses this same bounded slot.
            case session of
              Nothing -> modifyTVar' contexts (deleteContext contextId)
              Just _ -> pure ()
      result <- restore prepare `onException` abandon
      finished <- clock
      -- An evicted, expired or superseded reservation is unavailable: the
      -- caller receives 409 discovery_restart_required.
      atomically $ do
        entries <- readTVar contexts
        case Map.lookup contextId entries.byToken of
          Just entry | entry.expiresAt > finished -> do
            let completed = entry { preparedQuery = Just result }
            writeTVar contexts entries { byToken = Map.insert contextId completed entries.byToken }
            pure (Right completed)
          Just _ -> writeTVar contexts (deleteContext contextId entries) >> pure (Left DiscoveryContextUnavailable)
          Nothing -> pure (Left DiscoveryContextUnavailable)

lookupDiscoveryContext :: ObservationDiscovery -> Text -> UUID -> UUID -> IO (Either DiscoveryContextError StoredDiscoveryContext)
lookupDiscoveryContext (ObservationDiscovery _ _ contexts clock _) audience workspaceId contextId = do
  now <- clock
  atomically $ do
    stored <- readTVar contexts
    entries <- case expireContexts now stored of
      Nothing -> pure stored
      Just pruned -> writeTVar contexts pruned >> pure pruned
    pure $ case Map.lookup contextId entries.byToken of
      Just entry | entry.owner == audience && entry.workspace == workspaceId
        , Just _ <- entry.preparedQuery -> Right entry
      _ -> Left DiscoveryContextUnavailable

retireDiscoveryContext :: ObservationDiscovery -> Text -> UUID -> UUID -> IO ()
retireDiscoveryContext (ObservationDiscovery _ _ contexts _ _) audience workspaceId contextId = atomically $ do
  entries <- readTVar contexts
  case Map.lookup contextId entries.byToken of
    Just entry | entry.owner == audience && entry.workspace == workspaceId ->
      case entry.sessionGeneration of
        Nothing -> writeTVar contexts (deleteContext contextId entries)
        Just _ -> writeTVar contexts entries { byToken = Map.insert contextId (entry { preparedQuery = Nothing }) entries.byToken }
    _ -> pure ()
