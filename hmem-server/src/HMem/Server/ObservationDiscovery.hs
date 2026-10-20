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
  ) where

import Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVar, writeTVar)
import Control.Exception (SomeAsyncException, SomeException, catch, evaluate, fromException, throwIO)
import Data.Aeson (ToJSON(..), object, (.=))
import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import System.Timeout (timeout)

import HMem.Server.Embedding.GteQwen2
import HMem.Server.Embedding.Provider

data ObservationDiscovery = ObservationDiscovery !Bool !(TVar (Integer, Maybe EmbeddingProvider))

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
newObservationDiscovery enabled = ObservationDiscovery enabled <$> newTVarIO (0, Nothing)

-- | Publish the existing validated provider, including its transport admission
-- gate. Withdrawal precedes provider invalidation/join. An old withdrawal
-- cannot clear a replacement, even if both use the same endpoint.
publishDiscoveryProvider :: ObservationDiscovery -> EmbeddingProvider -> IO (IO ())
publishDiscoveryProvider (ObservationDiscovery _ state) provider = atomically $ do
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
  pure $ DiscoveryCapability $ case result of
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
withAvailable (ObservationDiscovery enabled state) storage action
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
