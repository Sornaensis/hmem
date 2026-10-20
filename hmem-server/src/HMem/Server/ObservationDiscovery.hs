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
  , createDiscoveryContext
  , createDiscoveryContextInSession
  , lookupDiscoveryContext
  , retireDiscoveryContext
  , discoveryQueryCapability
  ) where

import Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVar, writeTVar)
import Control.Exception (SomeAsyncException, SomeException, catch, evaluate, fromException, throwIO, onException, mask)
import Data.Aeson (ToJSON(..), object, (.=))
import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Time (UTCTime, getCurrentTime, addUTCTime)
import Data.UUID (UUID)
import Data.UUID.V4 qualified as UUID
import System.Timeout (timeout)

import HMem.Server.Embedding.GteQwen2
import HMem.Server.Embedding.Provider
import HMem.Types (ObservationDiscoveryFilter)

data ObservationDiscovery = ObservationDiscovery !Bool !(TVar (Integer, Maybe EmbeddingProvider))
  !(TVar (Map UUID StoredDiscoveryContext)) !(IO UTCTime)

-- No result catalog or public vector serialization is retained. Pending slots
-- count toward admission before provider work, preventing a keystroke flood.
data StoredDiscoveryContext = StoredDiscoveryContext
  { token :: !UUID, owner :: !Text, workspace :: !UUID
  , sessionGeneration :: !(Maybe (UUID,Int))
  , filterInputs :: !ObservationDiscoveryFilter, expiresAt :: !UTCTime
  , preparedQuery :: !(Maybe DiscoveryQuery)
  } deriving stock (Eq)

data DiscoveryContextError = DiscoveryContextUnavailable | DiscoveryContextCapacity
  deriving stock (Eq, Show)

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
  | DiscoveryHybrid ![Double] !Text
  deriving stock (Eq)

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
newObservationDiscoveryWithClock enabled clock =
  ObservationDiscovery enabled <$> newTVarIO (0, Nothing) <*> newTVarIO Map.empty <*> pure clock

-- | Publish the existing validated provider, including its transport admission
-- gate. Withdrawal precedes provider invalidation/join. An old withdrawal
-- cannot clear a replacement, even if both use the same endpoint.
publishDiscoveryProvider :: ObservationDiscovery -> EmbeddingProvider -> IO (IO ())
publishDiscoveryProvider (ObservationDiscovery _ state _ _) provider = atomically $ do
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
  result <- bounded $ withAvailable service storage $ \_ -> pure (DiscoveryHybrid [] "")
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
            _ <- evaluate (foldr seq () vector)
            pure (DiscoveryHybrid vector batch.spaceFingerprint)
          _ -> pure (DiscoveryFts DiscoveryInvalidResponse)

withAvailable
  :: ObservationDiscovery -> IO (Maybe Bool)
  -> (EmbeddingProvider -> IO DiscoveryQuery) -> IO DiscoveryQuery
withAvailable (ObservationDiscovery enabled state _ _) storage action
  | not enabled = pure (DiscoveryFts DiscoveryDisabled)
  | otherwise = do
      (generation, provider) <- atomically (readTVar state)
      case provider of
        Nothing -> pure (DiscoveryFts DiscoveryProviderUnavailable)
        Just current -> do
          result <- storage >>= \case
            Nothing -> pure (DiscoveryFts DiscoveryStorageUnavailable)
            Just False -> pure (DiscoveryFts DiscoveryNoCompatibleVectors)
            Just True -> current.availability >>= \case
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
createDiscoveryContextInSession (ObservationDiscovery _ _ contexts clock) audience workspaceId filters predecessor session prepare = mask $ \restore -> do
  now <- clock
  contextId <- UUID.nextRandom
  let reserved = StoredDiscoveryContext contextId audience workspaceId session filters (addUTCTime 300 now) Nothing
  admitted <- atomically $ do
    previous <- Map.filter (\entry -> entry.expiresAt > now) <$> readTVar contexts
    let validOwner entry = entry.owner == audience && entry.workspace == workspaceId
        foreignPredecessor = maybe False (\key -> maybe False (not . validOwner) (Map.lookup key previous)) predecessor
        incompatiblePredecessor = maybe False (\key -> maybe False (\entry -> case entry.sessionGeneration of
          Nothing -> False
          Just (oldSession,_) -> case session of
            Just (newSession,_) -> oldSession /= newSession
            Nothing -> True) (Map.lookup key previous)) predecessor
        peers = Map.filter (\entry -> validOwner entry && case (session,entry.sessionGeneration) of
          (Just (key,_),Just (oldKey,_)) -> key == oldKey
          _ -> False) previous
        outdated = case session of
          Just (_,nextGeneration) -> any (\entry -> maybe False ((>= nextGeneration) . snd) entry.sessionGeneration) (Map.elems peers)
          Nothing -> False
        withoutPeers = foldr Map.delete previous (Map.keys peers)
        entries = maybe withoutPeers (`Map.delete` withoutPeers) predecessor
    if foreignPredecessor || incompatiblePredecessor || outdated then pure (Left DiscoveryContextUnavailable)
    else if Map.size entries >= 64 || length (filter ((== audience) . (.owner)) (Map.elems entries)) >= 8
      then writeTVar contexts previous >> pure (Left DiscoveryContextCapacity)
      else writeTVar contexts (Map.insert contextId reserved entries) >> pure (Right ())
  case admitted of
    Left failure -> pure (Left failure)
    Right () -> do
      let abandon = atomically $ do
            entries <- readTVar contexts
            -- Keyed tombstones retain the high-water generation until its
            -- absolute expiry; the next edit reuses this same bounded slot.
            case session of
              Nothing -> writeTVar contexts (Map.delete contextId entries)
              Just _ -> pure ()
      result <- restore prepare `onException` abandon
      finished <- clock
      atomically $ do
        entries <- readTVar contexts
        case Map.lookup contextId entries of
          Just entry | entry.expiresAt > finished -> do
            let completed = entry { preparedQuery = Just result }
            writeTVar contexts (Map.insert contextId completed entries)
            pure (Right completed)
          _ -> writeTVar contexts (Map.delete contextId entries) >> pure (Left DiscoveryContextUnavailable)

lookupDiscoveryContext :: ObservationDiscovery -> Text -> UUID -> UUID -> IO (Either DiscoveryContextError StoredDiscoveryContext)
lookupDiscoveryContext (ObservationDiscovery _ _ contexts clock) audience workspaceId contextId = do
  now <- clock
  atomically $ do
    entries <- Map.filter (\entry -> entry.expiresAt > now) <$> readTVar contexts
    writeTVar contexts entries
    pure $ case Map.lookup contextId entries of
      Just entry | entry.owner == audience && entry.workspace == workspaceId
        , Just _ <- entry.preparedQuery -> Right entry
      _ -> Left DiscoveryContextUnavailable

retireDiscoveryContext :: ObservationDiscovery -> Text -> UUID -> UUID -> IO ()
retireDiscoveryContext (ObservationDiscovery _ _ contexts _) audience workspaceId contextId = atomically $ do
  entries <- readTVar contexts
  case Map.lookup contextId entries of
    Just entry | entry.owner == audience && entry.workspace == workspaceId ->
      case entry.sessionGeneration of
        Nothing -> writeTVar contexts (Map.delete contextId entries)
        Just _ -> writeTVar contexts (Map.insert contextId (entry { preparedQuery = Nothing }) entries)
    _ -> pure ()
