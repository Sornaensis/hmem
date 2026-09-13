{-# LANGUAGE TemplateHaskell #-}

-- | Bounded TEI HTTP transport and the validation boundary for explicitly
-- operated native GPU endpoints. There is deliberately no auth or credential
-- configuration in this adapter.
module HMem.Server.Embedding.Http
  ( httpEmbeddingTransport
  , makeValidatedGpuEmbeddingProvider
  , makeValidatedGpuEmbeddingProviderWithInvalidation
  , makeValidatedGpuEmbeddingProviderWithPublicationHook
  , makeValidatedGpuEmbeddingProviderWithLifecycleHooks
  , validateGpuEndpointCompatibility
  , validateGpuEndpointCompatibilityWithValidationStep
  , GpuEndpointMetadata(..)
  , decodeGpuEndpointMetadata
  , validateGpuEndpointMetadata
  , gpuValidationProbeRequest
  , validateGpuProbeResponse
  , embeddingRequestJson
  , decodeEmbeddingResponse
  , embeddingEndpointPath
  , normalizeEmbeddingEndpoint
  ) where

import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Concurrent.Async (AsyncCancelled(..))
import Control.Exception
  ( AsyncException(..)
  , SomeAsyncException
  , catch
  , evaluate
  , fromException
  , throwIO
  , toException
  , try
  )
import Crypto.Hash (Digest, SHA256, hash)
import Data.Aeson (FromJSON(..), Value, object, withObject, (.:), (.:?), (.=))
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as LBS
import Data.FileEmbed (embedFile, makeRelativeToProject)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as Text
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import Network.HTTP.Client
  ( BodyReader
  , HttpException(..)
  , HttpExceptionContent(..)
  , Manager
  , Request(..)
  , RequestBody(..)
  , brReadSome
  , parseRequest
  , requestHeaders
  , responseBody
  , responseHeaders
  , responseStatus
  , responseTimeoutMicro
  , withResponse
  )
import Network.HTTP.Types (statusCode)
import Network.HTTP.Types.Header (ResponseHeaders, hContentLength)
import System.Timeout (timeout)

import HMem.Config
  ( EmbeddingProviderConfig(..)
  , EmbeddingProviderMode(..)
  , managedTeiGpuProfile
  , managedTeiModelId
  , managedTeiSpaceFingerprint
  , normalizeEmbeddingEndpointRoute
  )
import HMem.Server.Embedding.GteQwen2
  ( GteQwen2Requirements(..)
  , gteQwen2QueryPrefix
  , gteQwen2Requirements
  )
import HMem.Server.Embedding.Provider
import HMem.Types (observationEmbeddingDimensions)

embeddingEndpointPath :: Text
embeddingEndpointPath = "/embed"

infoEndpointPath :: Text
infoEndpointPath = "/info"

gpuValidationTimeoutMs :: Int
gpuValidationTimeoutMs = 120000

maximumInfoResponseBytes :: Int
maximumInfoResponseBytes = 64 * 1024

responseReadChunkBytes :: Int
responseReadChunkBytes = 32 * 1024

embeddedFoundationContract :: BS.ByteString
embeddedFoundationContract = $(makeRelativeToProject "test/fixtures/embedding-gpu-viability/foundation-contract-v1.json" >>= embedFile)

embeddedParityProbes :: BS.ByteString
embeddedParityProbes = $(makeRelativeToProject "test/fixtures/embedding-gpu-viability/parity-probes-v1.json" >>= embedFile)

embeddedReferenceGolden :: BS.ByteString
embeddedReferenceGolden = $(makeRelativeToProject "test/fixtures/embedding-gpu-viability/reference-golden-v1.json" >>= embedFile)

data GpuEndpointMetadata = GpuEndpointMetadata
  { modelId :: !Text
  , servedModelName :: !Text
  , modelSha :: !(Maybe Text)
  , teiVersion :: !Text
  , teiSourceSha :: !Text
  , modelDtype :: !Text
  , pooling :: !Text
  , maxInputLength :: !Int
  , maxBatchTokens :: !Int
  , autoTruncate :: !Bool
  , defaultPrompt :: !(Maybe Text)
  , isCausal :: !(Maybe Bool)
  } deriving stock (Show, Eq)

instance FromJSON GpuEndpointMetadata where
  parseJSON = withObject "GpuEndpointMetadata" $ \o -> do
    modelType <- o .: "model_type"
    embedding <- Aeson.withObject "model_type" (.: "embedding") modelType
    poolingValue <- Aeson.withObject "embedding" (.: "pooling") embedding
    GpuEndpointMetadata
      <$> o .: "model_id"
      <*> o .: "served_model_name"
      <*> o .:? "model_sha"
      <*> o .: "version"
      <*> o .: "sha"
      <*> o .: "model_dtype"
      <*> pure poolingValue
      <*> o .: "max_input_length"
      <*> o .: "max_batch_tokens"
      <*> o .: "auto_truncate"
      <*> o .:? "default_prompt"
      <*> o .:? "is_causal"

decodeGpuEndpointMetadata :: LBS.ByteString -> Either EmbeddingFailure GpuEndpointMetadata
decodeGpuEndpointMetadata body = case Aeson.eitherDecode body of
  Left _ -> protocolFailure
  Right metadata -> validateGpuEndpointMetadata metadata >> pure metadata

validateGpuEndpointMetadata :: GpuEndpointMetadata -> Either EmbeddingFailure ()
validateGpuEndpointMetadata metadata
  | metadata.modelId `notElem` acceptedModelAliases = protocolFailure
  | metadata.servedModelName `notElem` acceptedModelAliases = protocolFailure
  | maybe False (/= pinnedRevision) metadata.modelSha = protocolFailure
  | metadata.teiVersion /= "1.9.3" = protocolFailure
  | metadata.teiSourceSha /= "06670157fb6c1523482219bdb2d1660277d38088" = protocolFailure
  | metadata.modelDtype /= "float16" = protocolFailure
  | metadata.pooling /= "last_token" = protocolFailure
  | metadata.maxInputLength /= 32768 = protocolFailure
  | metadata.maxBatchTokens /= 32768 = protocolFailure
  | metadata.autoTruncate = protocolFailure
  | metadata.defaultPrompt /= Nothing = protocolFailure
  | metadata.isCausal == Just True = protocolFailure
  | otherwise = Right ()
  where
    acceptedModelAliases = ["/opt/hmem/managed-embedding/model", "/model", managedTeiModelId]
    pinnedRevision = gteQwen2Requirements.modelRevision

data FoundationContract = FoundationContract
  { foundationSemanticSpace :: !Text
  , foundationModelId :: !Text
  , foundationRevision :: !Text
  , foundationDimensions :: !Int
  , foundationIsCausal :: !Bool
  , foundationDtype :: !Text
  , foundationPooling :: !Text
  , foundationNormalize :: !Bool
  , foundationMaxInputTokens :: !Int
  , foundationMaxFormattedBytes :: !Int
  , foundationQueryPrefix :: !Text
  , foundationMinimumCosine :: !Double
  , foundationMaximumL2 :: !Double
  , foundationMaximumCoordinateError :: !Double
  , foundationMaximumNormError :: !Double
  , foundationMaximumProbeTokens :: !Int
  }

instance FromJSON FoundationContract where
  parseJSON = withObject "FoundationContract" $ \o -> do
    model <- o .: "model"
    acceptance <- o .: "numerical_acceptance"
    Aeson.withObject "model" (\m ->
      Aeson.withObject "numerical_acceptance" (\a -> FoundationContract
        <$> o .: "semantic_space"
        <*> m .: "id"
        <*> m .: "revision"
        <*> m .: "dimensions"
        <*> m .: "is_causal"
        <*> m .: "dtype"
        <*> m .: "pooling"
        <*> m .: "normalize"
        <*> m .: "max_input_tokens"
        <*> m .: "hmem_max_formatted_utf8_bytes"
        <*> m .: "query_prefix"
        <*> a .: "minimum_cosine"
        <*> a .: "maximum_l2_distance"
        <*> a .: "maximum_coordinate_absolute_error"
        <*> a .: "maximum_unit_norm_absolute_error"
        <*> a .: "maximum_compact_probe_tokens") acceptance) model

data ProbeDocument = ProbeDocument
  { probeModelId :: !Text
  , probeRevision :: !Text
  , probeCases :: ![ProbeCase]
  }

instance FromJSON ProbeDocument where
  parseJSON = withObject "ProbeDocument" $ \o -> do
    model <- o .: "model"
    Aeson.withObject "model" (\m -> ProbeDocument
      <$> m .: "id"
      <*> m .: "revision"
      <*> o .: "cases") model

data ProbeCase = ProbeCase
  { probeId :: !Text
  , probeRawText :: !Text
  , probeRole :: !Text
  , probeText :: !Text
  , probeTokenIds :: ![Int]
  , probeUtf8Sha256 :: !Text
  }

instance FromJSON ProbeCase where
  parseJSON = withObject "ProbeCase" $ \o -> ProbeCase
    <$> o .: "id"
    <*> o .: "raw_text"
    <*> o .: "role"
    <*> o .: "text"
    <*> o .: "token_ids"
    <*> o .: "utf8_sha256"

data GoldenDocument = GoldenDocument
  { goldenSemanticSpace :: !Text
  , goldenReferenceMethod :: !Text
  , goldenModelId :: !Text
  , goldenRevision :: !Text
  , goldenCases :: ![GoldenCase]
  }

instance FromJSON GoldenDocument where
  parseJSON = withObject "GoldenDocument" $ \o -> do
    model <- o .: "model"
    Aeson.withObject "model" (\m -> GoldenDocument
      <$> o .: "semantic_space_fingerprint"
      <*> o .: "reference_method"
      <*> m .: "id"
      <*> m .: "revision"
      <*> o .: "cases") model

data GoldenCase = GoldenCase
  { goldenId :: !Text
  , goldenRole :: !Text
  , goldenUtf8Sha256 :: !Text
  , goldenTokenCount :: !Int
  , goldenTokenIds :: ![Int]
  , goldenVectorDimensions :: !Int
  , goldenVector :: ![Double]
  }

instance FromJSON GoldenCase where
  parseJSON = withObject "GoldenCase" $ \o -> GoldenCase
    <$> o .: "id"
    <*> o .: "role"
    <*> o .: "formatted_utf8_sha256"
    <*> o .: "token_count"
    <*> o .: "token_ids"
    <*> o .: "vector_dimensions"
    <*> o .: "vector"

data GpuValidationContract = GpuValidationContract
  { validationProbes :: ![EmbeddingInput]
  , validationGoldens :: ![[Double]]
  , minimumCosine :: !Double
  , maximumL2 :: !Double
  , maximumCoordinateError :: !Double
  , maximumNormError :: !Double
  }

embeddedGpuValidationContract :: Either EmbeddingFailure GpuValidationContract
embeddedGpuValidationContract = do
  ensureEmbeddedFile embeddedFoundationContract 7902 "ba776b330ff908a539b37fa41da7f0cd248a18f749c0ce606854171c6e6f6f0e"
  ensureEmbeddedFile embeddedParityProbes 68993 "1eded3104b9d3f18ae0a152aa6796d066c637ad1f3ceabea55b299d32b39aff7"
  ensureEmbeddedFile embeddedReferenceGolden 150199 "564a22ba414bd41e1e13d3d07db79eb88d45a30e3b2d9813ee5f02b5a32a6c93"
  foundation <- decodeStrict embeddedFoundationContract
  probes <- decodeStrict embeddedParityProbes
  golden <- decodeStrict embeddedReferenceGolden
  validateFixtureAuthority foundation probes golden

ensureEmbeddedFile :: BS.ByteString -> Int -> Text -> Either EmbeddingFailure ()
ensureEmbeddedFile bytes expectedBytes expectedSha
  | BS.length bytes == expectedBytes && sha256Bytes bytes == expectedSha = Right ()
  | otherwise = protocolFailure

decodeStrict :: FromJSON value => BS.ByteString -> Either EmbeddingFailure value
decodeStrict = either (const protocolFailure) Right . Aeson.eitherDecodeStrict'

validateFixtureAuthority
  :: FoundationContract
  -> ProbeDocument
  -> GoldenDocument
  -> Either EmbeddingFailure GpuValidationContract
validateFixtureAuthority foundation probes golden = do
  require $ foundation.foundationSemanticSpace == managedTeiSpaceFingerprint
  require $ foundation.foundationModelId == managedTeiModelId
  require $ foundation.foundationRevision == gteQwen2Requirements.modelRevision
  require $ foundation.foundationDimensions == observationEmbeddingDimensions
  require $ not foundation.foundationIsCausal
  require $ foundation.foundationDtype == "float16"
  require $ foundation.foundationPooling == "last-valid-token"
  require foundation.foundationNormalize
  require $ foundation.foundationMaxInputTokens == 32768
  require $ foundation.foundationMaxFormattedBytes == 32767
  require $ foundation.foundationQueryPrefix == gteQwen2QueryPrefix
  require $ foundation.foundationMinimumCosine == 0.9995
  require $ foundation.foundationMaximumL2 == 0.032
  require $ foundation.foundationMaximumCoordinateError == 0.005
  require $ foundation.foundationMaximumNormError == 0.0001
  require $ foundation.foundationMaximumProbeTokens == 2048
  require $ probes.probeModelId == managedTeiModelId
  require $ probes.probeRevision == gteQwen2Requirements.modelRevision
  require $ golden.goldenSemanticSpace == managedTeiSpaceFingerprint
  require $ golden.goldenReferenceMethod == "original-sdpa-math-cuda-f16-v3"
  require $ golden.goldenModelId == managedTeiModelId
  require $ golden.goldenRevision == gteQwen2Requirements.modelRevision
  require $ map (.probeId) probes.probeCases == expectedCaseIds
  require $ map (.goldenId) golden.goldenCases == expectedCaseIds
  inputs <- traverse validateLinkedCase (zip probes.probeCases golden.goldenCases)
  pure GpuValidationContract
    { validationProbes = inputs
    , validationGoldens = map (.goldenVector) golden.goldenCases
    , minimumCosine = foundation.foundationMinimumCosine
    , maximumL2 = foundation.foundationMaximumL2
    , maximumCoordinateError = foundation.foundationMaximumCoordinateError
    , maximumNormError = foundation.foundationMaximumNormError
    }
  where
    expectedCaseIds = ["doc_short", "mixed_long", "multilingual", "query_search"]
    require True = Right ()
    require False = protocolFailure

    validateLinkedCase (probe, expected) = do
      require $ probe.probeId == expected.goldenId
      require $ probe.probeRole == expected.goldenRole
      require $ probe.probeUtf8Sha256 == expected.goldenUtf8Sha256
      require $ probe.probeTokenIds == expected.goldenTokenIds
      require $ length probe.probeTokenIds == expected.goldenTokenCount
      require $ expected.goldenVectorDimensions == observationEmbeddingDimensions
      require $ sha256Bytes (Text.encodeUtf8 probe.probeText) == probe.probeUtf8Sha256
      kind <- case probe.probeRole of
        "document" | probe.probeText == probe.probeRawText -> Right EmbeddingDocument
        "query" | probe.probeText == gteQwen2QueryPrefix <> probe.probeRawText -> Right EmbeddingQuery
        _ -> protocolFailure
      validateVector foundation.foundationMaximumNormError expected.goldenVector
      pure (EmbeddingInput kind probe.probeText)

gpuValidationProbeRequest :: Either EmbeddingFailure EmbeddingRequest
gpuValidationProbeRequest = EmbeddingRequest . (.validationProbes) <$> embeddedGpuValidationContract

validateGpuProbeResponse :: [[Double]] -> Either EmbeddingFailure ()
validateGpuProbeResponse vectors = do
  contract <- embeddedGpuValidationContract
  if length vectors /= length contract.validationGoldens
    then protocolFailure
    else sequence_ (zipWith (validateAgainstGolden contract) vectors contract.validationGoldens)

validateAgainstGolden
  :: GpuValidationContract
  -> [Double]
  -> [Double]
  -> Either EmbeddingFailure ()
validateAgainstGolden contract actual expected = do
  validateVector contract.maximumNormError actual
  validateVector contract.maximumNormError expected
  let actualNorm = vectorNorm actual
      expectedNorm = vectorNorm expected
      cosine = sum (zipWith (*) actual expected) / (actualNorm * expectedNorm)
      differences = zipWith (-) actual expected
      l2 = sqrt (sum (map (\value -> value * value) differences))
      coordinateError = maximum (0 : map abs differences)
  if cosine >= contract.minimumCosine
      && l2 <= contract.maximumL2
      && coordinateError <= contract.maximumCoordinateError
    then Right ()
    else protocolFailure

validateVector :: Double -> [Double] -> Either EmbeddingFailure ()
validateVector maximumAllowedNormError vector
  | length vector /= observationEmbeddingDimensions = protocolFailure
  | any (\value -> isNaN value || isInfinite value) vector = protocolFailure
  | abs (vectorNorm vector - 1) > maximumAllowedNormError = protocolFailure
  | otherwise = Right ()

vectorNorm :: [Double] -> Double
vectorNorm = sqrt . sum . map (\value -> value * value)

sha256Bytes :: BS.ByteString -> Text
sha256Bytes bytes = T.pack (show (hash bytes :: Digest SHA256))

protocolFailure :: Either EmbeddingFailure value
protocolFailure = Left (EmbeddingFailure ProviderProtocolError False)

normalizeEmbeddingEndpoint :: Text -> Either EmbeddingFailure Text
normalizeEmbeddingEndpoint endpointValue =
  maybe (Left configurationFailure) Right (normalizeEmbeddingEndpointRoute endpointValue)

embeddingRequestJson :: EmbeddingRequest -> Value
embeddingRequestJson request = object
  [ "inputs" .= map (.inputText) request.inputs
  , "normalize" .= True
  , "truncate" .= False
  ]

decodeEmbeddingResponse
  :: Int
  -> LBS.ByteString
  -> Either EmbeddingFailure [[Double]]
decodeEmbeddingResponse expectedCount body = case Aeson.eitherDecode body of
  Left _ -> protocolFailure
  Right decoded
    | length decoded /= expectedCount -> protocolFailure
    | otherwise -> do
        sequence_ [validateVector 0.0001 vector | vector <- decoded]
        Right decoded

httpEmbeddingTransport
  :: Manager
  -> EmbeddingProviderConfig
  -> Text
  -> EmbeddingRequest
  -> IO (Either EmbeddingFailure EmbeddingBatch)
httpEmbeddingTransport manager cfg endpointValue request = catchCancelled $ do
  let totalMicros = cfg.timeoutMs * 1000
  completed <- timeout totalMicros $ case normalizeEmbeddingEndpoint endpointValue of
    Left failure -> pure (Left failure)
    Right endpoint -> go endpoint (cfg.retryAttempts + 1) totalMicros
  pure $ case completed of
    Nothing -> Left (EmbeddingFailure ProviderTimedOut True)
    Just result -> result
  where
    go routedEndpoint attempts totalMicros = do
      result <- oneAttempt routedEndpoint totalMicros
      case result of
        Left failure | failure.retryable && attempts > 1 -> go routedEndpoint (attempts - 1) totalMicros
        _ -> pure result

    oneAttempt routedEndpoint totalMicros = do
      let encoded = Aeson.encode (embeddingRequestJson request)
      response <- performBoundedRequest manager totalMicros "POST" routedEndpoint (Just encoded)
        (maximumEmbeddingResponseBytes (length request.inputs))
      evaluate $ response >>= decodeEmbeddingResponse (length request.inputs) >>= \vectors -> Right EmbeddingBatch
        { vectors = vectors
        , spaceFingerprint = cfg.spaceFingerprint
        }

makeValidatedGpuEmbeddingProvider
  :: Manager
  -> EmbeddingProviderConfig
  -> Maybe Text
  -> IO (Either EmbeddingFailure EmbeddingProvider)
makeValidatedGpuEmbeddingProvider manager cfg supervisorEndpoint =
  fmap (fmap fst) (makeValidatedGpuEmbeddingProviderWithInvalidation manager cfg supervisorEndpoint)

-- | Construct one provider for exactly one endpoint/configuration/managed-child
-- generation and return an explicit invalidation action. The supervisor must
-- invalidate first, cancel and join its request owner, then retire this value
-- whenever that generation is withdrawn; a replacement child, even at the
-- same URL, requires a new constructor call. Invalidation advances an epoch,
-- so validation or user responses already in flight cannot be admitted.
--
-- Construction and disabled availability perform no network IO. The first
-- availability check (or embed) performs one synchronous metadata+golden
-- validation. A lock coalesces concurrent checks, and a short failure cooldown
-- prevents queued callers from producing a probe storm. Transport retries are
-- owned solely by 'httpEmbeddingTransport'.
makeValidatedGpuEmbeddingProviderWithInvalidation
  :: Manager
  -> EmbeddingProviderConfig
  -> Maybe Text
  -> IO (Either EmbeddingFailure (EmbeddingProvider, IO ()))
makeValidatedGpuEmbeddingProviderWithInvalidation manager cfg supervisorEndpoint
  = makeValidatedGpuEmbeddingProviderWithLifecycleHooks (pure ()) (pure ()) (pure ()) manager cfg supervisorEndpoint

-- | Construct a generation with an action immediately before validation is
-- published. This narrow synchronization hook lets lifecycle tests withdraw a
-- generation in the otherwise unobservable interval between observing its
-- state and attempting the atomic publication.
makeValidatedGpuEmbeddingProviderWithPublicationHook
  :: IO ()
  -> Manager
  -> EmbeddingProviderConfig
  -> Maybe Text
  -> IO (Either EmbeddingFailure (EmbeddingProvider, IO ()))
makeValidatedGpuEmbeddingProviderWithPublicationHook beforePublication manager cfg supervisorEndpoint
  = makeValidatedGpuEmbeddingProviderWithLifecycleHooks beforePublication (pure ()) (pure ()) manager cfg supervisorEndpoint

-- | Construct a provider with synchronization points around validation
-- publication and the two state-publication gaps in a user invocation. These
-- hooks are for deterministic lifecycle tests; production callers should use
-- 'makeValidatedGpuEmbeddingProviderWithInvalidation'.
makeValidatedGpuEmbeddingProviderWithLifecycleHooks
  :: IO ()
  -> IO ()
  -> IO ()
  -> Manager
  -> EmbeddingProviderConfig
  -> Maybe Text
  -> IO (Either EmbeddingFailure (EmbeddingProvider, IO ()))
makeValidatedGpuEmbeddingProviderWithLifecycleHooks beforePublication afterAvailability afterTransport manager cfg supervisorEndpoint
  | Just profile <- cfg.gpuProfile, profile /= managedTeiGpuProfile = pure (Left configurationFailure)
  | otherwise = case cfg.mode of
    EmbeddingProviderDisabled
      | cfg.gpuProfile == Nothing -> pure (Right (disabledEmbeddingProvider, pure ()))
      | otherwise -> pure (Left configurationFailure)
    _ | cfg.gpuProfile /= Just managedTeiGpuProfile -> pure (Left configurationFailure)
    _ -> case resolveEmbeddingProviderEndpoint cfg supervisorEndpoint of
      Left failure -> pure (Left failure)
      Right endpointValue -> case embeddedGpuValidationContract of
        Left failure -> pure (Left failure)
        Right _ -> do
          state <- newIORef GpuRuntimeState
            { runtimeAvailability = EmbeddingUnavailable (EmbeddingFailure ProviderUnavailable True)
            , runtimeEpoch = 0
            , nextValidationNs = 0
            , runtimeRetired = False
            }
          validationLock <- newMVar ()
          let ensureAvailable = ensureGpuAvailability beforePublication manager endpointValue state validationLock
              invalidate = invalidateGpuRuntime state (EmbeddingFailure ProviderUnavailable True)
              invoke request
                | null request.inputs || length request.inputs > cfg.batchSize = pure (Left configurationFailure)
                | otherwise = catchEmbeddingCancellation state $ ensureAvailable >>= \case
                    EmbeddingAvailable -> do
                      afterAvailability
                      before <- readIORef state
                      if before.runtimeRetired || before.runtimeAvailability /= EmbeddingAvailable
                        then pure (Left (EmbeddingFailure ProviderCancelled False))
                        else do
                          result <- httpEmbeddingTransport manager cfg endpointValue request
                          afterTransport
                          case result of
                            Left failure -> invalidateGpuRuntimeIfEpoch state before.runtimeEpoch failure >> pure (Left failure)
                            Right batch -> do
                              after <- readIORef state
                              if after.runtimeEpoch == before.runtimeEpoch
                                  && after.runtimeAvailability == EmbeddingAvailable
                                then pure (Right batch)
                                else pure (Left (EmbeddingFailure ProviderCancelled False))
                    EmbeddingUnavailable failure -> pure (Left failure)
                    EmbeddingDisabled -> pure (Left (EmbeddingFailure ProviderDisabled False))
              provider = EmbeddingProvider
                { availability = ensureAvailable
                , embed = invoke
                }
          pure (Right (provider, invalidate))

data GpuRuntimeState = GpuRuntimeState
  { runtimeAvailability :: !EmbeddingAvailability
  , runtimeEpoch :: !Word64
  , nextValidationNs :: !Word64
  , runtimeRetired :: !Bool
  }

ensureGpuAvailability
  :: IO ()
  -> Manager
  -> Text
  -> IORef GpuRuntimeState
  -> MVar ()
  -> IO EmbeddingAvailability
ensureGpuAvailability beforePublication manager endpointValue state validationLock =
  catchAvailabilityCancellation state $ withMVar validationLock $ \_ -> do
    before <- readIORef state
    now <- getMonotonicTimeNSec
    case before.runtimeAvailability of
      _ | before.runtimeRetired -> pure before.runtimeAvailability
      EmbeddingAvailable -> pure EmbeddingAvailable
      unavailable | now < before.nextValidationNs -> pure unavailable
      _ -> do
        validation <- validateGpuEndpointCompatibility manager endpointValue
        _observedAfterValidation <- readIORef state
        beforePublication
        validationFinished <- getMonotonicTimeNSec
        let nextAvailability = either EmbeddingUnavailable (const EmbeddingAvailable) validation
            retryAfter = case nextAvailability of
              EmbeddingAvailable -> maxBound
              _ -> validationFinished + 1000000000
        atomicModifyIORef' state $ \current ->
          if current.runtimeEpoch /= before.runtimeEpoch || current.runtimeRetired
            then (current, current.runtimeAvailability)
            else
              let published = current
                    { runtimeAvailability = nextAvailability
                    , nextValidationNs = retryAfter
                    }
              in (published, published.runtimeAvailability)

catchAvailabilityCancellation
  :: IORef GpuRuntimeState
  -> IO EmbeddingAvailability
  -> IO EmbeddingAvailability
catchAvailabilityCancellation state action = action `catch` \(exception :: SomeAsyncException) ->
  if isProviderCancellation exception
    then invalidateGpuRuntimeForCancellation state
    else throwIO exception

catchEmbeddingCancellation
  :: IORef GpuRuntimeState
  -> IO (Either EmbeddingFailure value)
  -> IO (Either EmbeddingFailure value)
catchEmbeddingCancellation state action = action `catch` \(exception :: SomeAsyncException) ->
  if isProviderCancellation exception
    then invalidateGpuRuntimeForCancellation state
      >> pure (Left (EmbeddingFailure ProviderCancelled False))
    else throwIO exception

invalidateGpuRuntimeForCancellation :: IORef GpuRuntimeState -> IO EmbeddingAvailability
invalidateGpuRuntimeForCancellation state = do
  now <- getMonotonicTimeNSec
  atomicModifyIORef' state $ \current ->
    if current.runtimeRetired
      then (current, current.runtimeAvailability)
      else
        let cancelled = EmbeddingUnavailable (EmbeddingFailure ProviderCancelled False)
            invalidated = current
              { runtimeAvailability = cancelled
              , runtimeEpoch = current.runtimeEpoch + 1
              , nextValidationNs = now + 1000000000
              }
        in (invalidated, cancelled)

invalidateGpuRuntime :: IORef GpuRuntimeState -> EmbeddingFailure -> IO ()
invalidateGpuRuntime state failure = atomicModifyIORef' state $ \current ->
  ( current
      { runtimeAvailability = EmbeddingUnavailable failure
      , runtimeEpoch = current.runtimeEpoch + 1
      , nextValidationNs = 0
      , runtimeRetired = True
      }
  , ()
  )

invalidateGpuRuntimeIfEpoch :: IORef GpuRuntimeState -> Word64 -> EmbeddingFailure -> IO ()
invalidateGpuRuntimeIfEpoch state expectedEpoch failure = do
  now <- getMonotonicTimeNSec
  atomicModifyIORef' state $ \current ->
    if current.runtimeEpoch /= expectedEpoch
      then (current, ())
      else
        ( current
            { runtimeAvailability = EmbeddingUnavailable failure
            , runtimeEpoch = current.runtimeEpoch + 1
            , nextValidationNs = now + 1000000000
            }
        , ()
        )

validateGpuEndpointCompatibility
  :: Manager
  -> Text
  -> IO (Either EmbeddingFailure ())
validateGpuEndpointCompatibility =
  validateGpuEndpointCompatibilityWithValidationStep gpuValidationTimeoutMs (pure ())

-- | Validate with a test-only synchronization step executed as each decoded
-- probe vector enters its numerical comparison. The supplied deadline encloses
-- response decoding, every step, and all numerical validation.
validateGpuEndpointCompatibilityWithValidationStep
  :: Int
  -> IO ()
  -> Manager
  -> Text
  -> IO (Either EmbeddingFailure ())
validateGpuEndpointCompatibilityWithValidationStep timeoutMs validationStep manager endpointValue = catchCancelled $ do
  completed <- timeout (timeoutMs * 1000) $ case embeddedGpuValidationContract of
    Left failure -> pure (Left failure)
    Right contract -> case normalizeEmbeddingEndpoint endpointValue of
      Left failure -> pure (Left failure)
      Right embedEndpoint -> do
        let infoEndpoint = T.dropEnd (T.length embeddingEndpointPath) embedEndpoint <> infoEndpointPath
            totalMicros = timeoutMs * 1000
        infoResponse <- performBoundedRequest manager totalMicros "GET" infoEndpoint Nothing maximumInfoResponseBytes
        case infoResponse >>= decodeGpuEndpointMetadata of
          Left failure -> pure (Left failure)
          Right _ -> do
            let probeRequest = EmbeddingRequest contract.validationProbes
                encoded = Aeson.encode (embeddingRequestJson probeRequest)
            probeResponse <- performBoundedRequest manager totalMicros "POST" embedEndpoint (Just encoded)
              (maximumEmbeddingResponseBytes (length contract.validationProbes))
            case probeResponse >>= decodeEmbeddingResponse (length contract.validationProbes) of
              Left failure -> pure (Left failure)
              Right decoded -> validateGpuProbeResponseWithStep validationStep contract decoded
  pure $ case completed of
    Nothing -> Left (EmbeddingFailure ProviderTimedOut True)
    Just result -> result

validateGpuProbeResponseWithStep
  :: IO ()
  -> GpuValidationContract
  -> [[Double]]
  -> IO (Either EmbeddingFailure ())
validateGpuProbeResponseWithStep validationStep contract vectors
  | length vectors /= length contract.validationGoldens = pure protocolFailure
  | otherwise = go (zip vectors contract.validationGoldens)
  where
    go [] = pure (Right ())
    go ((actual, expected) : remaining) = do
      validationStep
      validated <- evaluate (validateAgainstGolden contract actual expected)
      case validated of
        Left failure -> pure (Left failure)
        Right () -> go remaining

performBoundedRequest
  :: Manager
  -> Int
  -> BS.ByteString
  -> Text
  -> Maybe LBS.ByteString
  -> Int
  -> IO (Either EmbeddingFailure LBS.ByteString)
performBoundedRequest manager requestTimeoutMicros methodValue endpointValue body maximumBytes = do
  attempted <- try @HttpException $ do
    baseRequest <- parseRequest (T.unpack endpointValue)
    let bodyHeaders = case body of
          Nothing -> []
          Just _ -> [("Content-Type", "application/json")]
        httpRequest = baseRequest
          { method = methodValue
          , requestHeaders = ("Accept", "application/json") : bodyHeaders <> baseRequest.requestHeaders
          , requestBody = maybe (RequestBodyBS BS.empty) RequestBodyLBS body
          , redirectCount = 0
          , responseTimeout = responseTimeoutMicro requestTimeoutMicros
          }
    withResponse httpRequest manager $ \response ->
      if statusCode (responseStatus response) < 200 || statusCode (responseStatus response) >= 300
        then pure (Left (EmbeddingFailure ProviderUnavailable (retryableStatus (statusCode (responseStatus response)))))
        else readBoundedResponseBody maximumBytes (responseHeaders response) (responseBody response)
  pure $ case attempted of
    Left (HttpExceptionRequest _ ResponseTimeout) -> Left (EmbeddingFailure ProviderTimedOut True)
    Left (HttpExceptionRequest _ ConnectionTimeout) -> Left (EmbeddingFailure ProviderTimedOut True)
    Left _ -> Left (EmbeddingFailure ProviderUnavailable True)
    Right result -> result

maximumEmbeddingResponseBytes :: Int -> Int
maximumEmbeddingResponseBytes itemCount = max 2 (itemCount * observationEmbeddingDimensions * 40 + itemCount * 2 + 2)

readBoundedResponseBody
  :: Int
  -> ResponseHeaders
  -> BodyReader
  -> IO (Either EmbeddingFailure LBS.ByteString)
readBoundedResponseBody maximumBytes headers reader = case lookup hContentLength headers of
  Just rawLength | Just declared <- parseContentLength rawLength
                 , declared > maximumBytes -> pure protocolFailure
                 | Nothing <- parseContentLength rawLength -> pure protocolFailure
  _ -> go 0 []
  where
    go bytesRead chunks = do
      let requested = min responseReadChunkBytes (maximumBytes - bytesRead + 1)
      chunk <- brReadSome reader requested
      let chunkBytes = fromIntegral (LBS.length chunk)
          total = bytesRead + chunkBytes
      if total > maximumBytes
        then pure protocolFailure
        else if LBS.null chunk
          then pure (Right (mconcat (reverse chunks)))
          else go total (chunk : chunks)

parseContentLength :: BS.ByteString -> Maybe Int
parseContentLength raw = case BS8.readInteger raw of
  Just (declared, rest)
    | BS.null rest && declared >= 0 && declared <= fromIntegral (maxBound :: Int) -> Just (fromInteger declared)
  _ -> Nothing

retryableStatus :: Int -> Bool
retryableStatus status = status == 408 || status == 429 || (status >= 500 && status <= 599)

configurationFailure :: EmbeddingFailure
configurationFailure = EmbeddingFailure ProviderConfigurationError False

catchCancelled
  :: IO (Either EmbeddingFailure value)
  -> IO (Either EmbeddingFailure value)
catchCancelled action = action `catch` \(exception :: SomeAsyncException) ->
  if isProviderCancellation exception
    then pure (Left (EmbeddingFailure ProviderCancelled False))
    else throwIO exception

isProviderCancellation :: SomeAsyncException -> Bool
isProviderCancellation exception =
  case fromException (toException exception) :: Maybe AsyncCancelled of
    Just AsyncCancelled -> True
    Nothing -> case fromException (toException exception) :: Maybe AsyncException of
      Just ThreadKilled -> True
      Just UserInterrupt -> True
      _ -> False
