-- | Provider-neutral embedding contract.  This module intentionally owns no
-- process lifecycle and performs no network work while the provider is
-- disabled; managed process supervision is a separate concern.
module HMem.Server.Embedding.Provider
  ( EmbeddingInputKind(..)
  , EmbeddingInput(..)
  , EmbeddingRequest(..)
  , EmbeddingBatch(..)
  , EmbeddingAvailability(..)
  , EmbeddingErrorCode(..)
  , EmbeddingFailure(..)
  , EmbeddingProvider(..)
  , disabledEmbeddingProvider
  , resolveEmbeddingProviderEndpoint
  , selectEmbeddingProvider
  ) where

import Data.Text (Text)

import HMem.Config
  ( EmbeddingEndpointAuthority(..)
  , EmbeddingProviderConfig(..)
  , EmbeddingProviderMode(..)
  , normalizeEmbeddingEndpointRoute
  , parseEmbeddingEndpointAuthority
  )

-- | TEI receives documents and queries through the same endpoint but callers
-- preserve the distinction so the pinned semantic profile can apply its
-- query/document conventions without changing this boundary.
data EmbeddingInputKind = EmbeddingDocument | EmbeddingQuery
  deriving stock (Show, Eq)

data EmbeddingInput = EmbeddingInput
  { inputKind :: !EmbeddingInputKind
  , inputText :: !Text
  } deriving stock (Eq)

instance Show EmbeddingInput where
  show input = "EmbeddingInput {inputKind = " <> show input.inputKind <> ", inputText = <redacted>}"

data EmbeddingRequest = EmbeddingRequest
  { inputs :: ![EmbeddingInput]
  } deriving stock (Eq)

instance Show EmbeddingRequest where
  show request = "EmbeddingRequest {inputCount = " <> show (length request.inputs) <> "}"

data EmbeddingBatch = EmbeddingBatch
  { vectors :: ![[Double]]
  , spaceFingerprint :: !Text
  } deriving stock (Eq)

instance Show EmbeddingBatch where
  show batch = "EmbeddingBatch {vectorCount = " <> show (length batch.vectors)
    <> ", spaceFingerprint = " <> show batch.spaceFingerprint <> "}"

data EmbeddingAvailability
  = EmbeddingAvailable
  | EmbeddingDisabled
  | EmbeddingUnavailable !EmbeddingFailure
  deriving stock (Show, Eq)

data EmbeddingErrorCode
  = ProviderDisabled
  | ProviderUnavailable
  | ProviderTimedOut
  | ProviderCancelled
  | ProviderProtocolError
  | ProviderConfigurationError
  deriving stock (Show, Eq)

-- | A structured, secret-safe failure.  The code is intentionally sufficient
-- for retry/availability decisions; raw request URLs and response bodies are
-- neither stored nor rendered here.
data EmbeddingFailure = EmbeddingFailure
  { errorCode :: !EmbeddingErrorCode
  , retryable :: !Bool
  } deriving stock (Show, Eq)

data EmbeddingProvider = EmbeddingProvider
  { availability :: !(IO EmbeddingAvailability)
  , embed :: !(EmbeddingRequest -> IO (Either EmbeddingFailure EmbeddingBatch))
  }

-- | Disabled is a real provider state, not a fallback.  Constructing and
-- calling it performs no network or process work.
disabledEmbeddingProvider :: EmbeddingProvider
disabledEmbeddingProvider = EmbeddingProvider
  { availability = pure EmbeddingDisabled
  , embed = \_ -> pure (Left (EmbeddingFailure ProviderDisabled False))
  }

-- | Select exactly the configured provider.  The transport is supplied by the
-- HTTP adapter, making the mode boundary testable and ensuring this module
-- never starts a managed child process.  A managed provider obtains a
-- supervisor-provided loopback endpoint; it cannot consume an arbitrary
-- configured endpoint or silently fall back to HTTP.
selectEmbeddingProvider
  :: EmbeddingProviderConfig
  -> Maybe Text
  -> (Text -> EmbeddingRequest -> IO (Either EmbeddingFailure EmbeddingBatch))
  -> Either EmbeddingFailure EmbeddingProvider
selectEmbeddingProvider cfg supervisorEndpoint _transport = case cfg.mode of
  EmbeddingProviderDisabled -> Right disabledEmbeddingProvider
  _ -> do
    _ <- resolveEmbeddingProviderEndpoint cfg supervisorEndpoint
    -- Enabled GPU providers must pass the IO validation boundary in
    -- makeValidatedGpuEmbeddingProvider. Keep this legacy pure selector for
    -- callers that have not yet been migrated, but fail closed without
    -- invoking their transport.
    Right EmbeddingProvider
      { availability = pure (EmbeddingUnavailable unvalidatedFailure)
      , embed = \_ -> pure (Left unvalidatedFailure)
      }
  where
    unvalidatedFailure = EmbeddingFailure ProviderUnavailable False

-- | Resolve the endpoint selected by configuration without performing IO.
-- Successful resolution is only an input to the validated HTTP constructor;
-- it does not establish provider availability.
resolveEmbeddingProviderEndpoint
  :: EmbeddingProviderConfig
  -> Maybe Text
  -> Either EmbeddingFailure Text
resolveEmbeddingProviderEndpoint cfg supervisorEndpoint = case cfg.mode of
  EmbeddingProviderDisabled -> Left configurationFailure
  EmbeddingProviderHttp -> requireEndpoint cfg.endpoint
  EmbeddingProviderManagedTei -> requireManagedEndpoint supervisorEndpoint
  where

    requireEndpoint = \case
      Just endpointValue -> maybe (Left configurationFailure) Right (normalizeEmbeddingEndpointRoute endpointValue)
      Nothing -> Left configurationFailure

    requireManagedEndpoint = \case
      Just endpointValue
        | Just normalized <- normalizeEmbeddingEndpointRoute endpointValue
        , loopbackEndpoint normalized -> Right normalized
      _ -> Left configurationFailure

    configurationFailure = EmbeddingFailure ProviderConfigurationError False

loopbackEndpoint :: Text -> Bool
loopbackEndpoint endpointValue = case parseEmbeddingEndpointAuthority endpointValue of
  Just authority -> authority.scheme == "http" && authority.host `elem` ["127.0.0.1", "localhost", "::1"]
  Nothing -> False
