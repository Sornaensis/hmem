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
  , logicalEmbeddingTimeoutMs
  , embeddingEndpointPath
  , normalizeEmbeddingEndpoint
  ) where

import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Concurrent.STM
  ( TVar
  , atomically
  , check
  , modifyTVar'
  , newTVarIO
  , readTVar
  , writeTVar
  )
import Control.Exception
  ( SomeAsyncException
  , catch
  , evaluate
  , finally
  , mask
  , onException
  , throwIO
  )
import Crypto.Hash (Digest, SHA256, hash)
import Data.Aeson (FromJSON(..), Value, object, withObject, (.:), (.:?), (.=))
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.FileEmbed (embedFile, makeRelativeToProject)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as Text
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import System.Timeout (timeout)

import HMem.Embedding.HttpProcess
  ( HttpSession, SessionFailure(..), SessionPolicy, callHttp, withHttpSession )
import HMem.Embedding.HttpProtocol
  ( HttpCall(..), HttpMethod(..), HttpReply(..), TransportCode(..) )

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

newtype LogicalDeadline = LogicalDeadline Word64

data ProviderGate = ProviderGate
  { gateActive :: !(TVar Bool)
  , gateWaiters :: !(TVar Int)
  }

data GateAdmission = GateAcquired | GateQueued | GateOverflow

data GateTicketState = GateWaiting | GateTicketAcquired | GateTicketCancelled
  deriving stock (Eq)

newProviderGate :: IO ProviderGate
newProviderGate = ProviderGate <$> newTVarIO False <*> newTVarIO 0

prepareLogicalDeadline
  :: EmbeddingProviderConfig
  -> EmbeddingRequest
  -> IO (Either EmbeddingFailure LogicalDeadline)
prepareLogicalDeadline cfg request = do
  started <- getMonotonicTimeNSec
  let classificationDeadline = LogicalDeadline
        (started + fromIntegral (min cfg.timeoutMs 30000) * 1000000)
  classified <- runWithinDeadline classificationDeadline
    (case logicalEmbeddingTimeoutMs cfg request of
      Left failure -> pure (Left failure)
      Right totalMilliseconds ->
        evaluate totalMilliseconds >> pure (Right totalMilliseconds))
  case classified of
    Left failure -> pure (Left failure)
    Right totalMilliseconds -> do
      let deadline = LogicalDeadline
            (started + fromIntegral totalMilliseconds * 1000000)
      remaining <- remainingDeadlineMicros deadline
      pure $ if remaining <= 0 then timeoutFailure else Right deadline

classifyLogicalRequest
  :: EmbeddingProviderConfig
  -> EmbeddingRequest
  -> Either EmbeddingFailure Bool
classifyLogicalRequest cfg request
  | null request.inputs = Left configurationFailure
  | length request.inputs > min cfg.batchSize 4 = Left configurationFailure
  | otherwise = go True request.inputs
  where
    go compact [] = Right compact
    go compact (input : remaining) =
      let formattedBytes = BS.length (Text.encodeUtf8 input.inputText)
          conservativeCost = formattedBytes + 1
       in if formattedBytes > 32767
            then Left configurationFailure
            else conservativeCost `seq` go (compact && conservativeCost <= 2048) remaining

-- | Resolve the configured whole-operation ceiling after applying the compact
-- or long-input profile. Inputs are already formatted by the semantic layer;
-- their kind therefore has no effect on byte accounting or prompt handling.
logicalEmbeddingTimeoutMs
  :: EmbeddingProviderConfig
  -> EmbeddingRequest
  -> Either EmbeddingFailure Int
logicalEmbeddingTimeoutMs cfg request = do
  compact <- classifyLogicalRequest cfg request
  pure (min cfg.timeoutMs (if compact then 30000 else 300000))

remainingDeadlineMicros :: LogicalDeadline -> IO Int
remainingDeadlineMicros (LogicalDeadline deadline) = do
  now <- getMonotonicTimeNSec
  pure $ if now >= deadline
    then 0
    else fromIntegral ((deadline - now) `div` 1000)

runWithinDeadline
  :: LogicalDeadline
  -> IO (Either EmbeddingFailure value)
  -> IO (Either EmbeddingFailure value)
runWithinDeadline deadline action = do
  remaining <- remainingDeadlineMicros deadline
  if remaining <= 0
    then pure timeoutFailure
    else timeout remaining action >>= \case
      Nothing -> pure timeoutFailure
      Just result -> pure result

withProviderGate
  :: ProviderGate
  -> LogicalDeadline
  -> IORef GpuRuntimeState
  -> IO (Either EmbeddingFailure value)
  -> IO (Either EmbeddingFailure value)
withProviderGate gate deadline state action = mask $ \restore -> do
  admission <- atomically $ do
    active <- readTVar gate.gateActive
    waiters <- readTVar gate.gateWaiters
    if not active
      then writeTVar gate.gateActive True >> pure GateAcquired
      else if waiters >= 2
        then pure GateOverflow
        else modifyTVar' gate.gateWaiters (+ 1) >> pure GateQueued
  acquired <- case admission of
    GateOverflow -> pure (Left (EmbeddingFailure ProviderUnavailable True))
    GateAcquired -> pure (Right ())
    GateQueued -> do
      ticket <- newTVarIO GateWaiting
      remaining <- remainingDeadlineMicros deadline
      let waitMicros = min 5000000 remaining
          cleanupWaiter = atomically $ readTVar ticket >>= \case
            GateWaiting -> do
              writeTVar ticket GateTicketCancelled
              modifyTVar' gate.gateWaiters (max 0 . subtract 1)
            GateTicketAcquired -> do
              writeTVar ticket GateTicketCancelled
              writeTVar gate.gateActive False
            GateTicketCancelled -> pure ()
          waitForSlot = atomically $ do
            active <- readTVar gate.gateActive
            ticketState <- readTVar ticket
            check (not active)
            check (ticketState == GateWaiting)
            writeTVar gate.gateActive True
            modifyTVar' gate.gateWaiters (max 0 . subtract 1)
            writeTVar ticket GateTicketAcquired
      if waitMicros <= 0
        then cleanupWaiter >> pure timeoutFailure
        else do
          waited <- restore (timeout waitMicros waitForSlot) `onException` cleanupWaiter
          case waited of
            Just () -> pure (Right ())
            Nothing -> do
              cleanupWaiter
              remainingAfterWait <- remainingDeadlineMicros deadline
              pure $ if remainingAfterWait <= 0
                then timeoutFailure
                else Left (EmbeddingFailure ProviderUnavailable True)
  case acquired of
    Left failure -> pure (Left failure)
    Right () -> do
      expectedEpoch <- (.runtimeEpoch) <$> readIORef state
      -- The worker also uses a custom asynchronous StopProvider exception.
      -- Every admitted async exit must fence the generation before slot release.
      let publishCancellation (exception :: SomeAsyncException) = do
            invalidateGpuRuntimeIfEpoch state expectedEpoch
              (EmbeddingFailure ProviderCancelled False)
            throwIO exception
      (restore action `catch` publishCancellation)
        `finally` atomically (writeTVar gate.gateActive False)

timeoutFailure :: Either EmbeddingFailure value
timeoutFailure = Left (EmbeddingFailure ProviderTimedOut True)

httpEmbeddingTransport
  :: SessionPolicy
  -> EmbeddingProviderConfig
  -> Text
  -> EmbeddingRequest
  -> IO (Either EmbeddingFailure EmbeddingBatch)
httpEmbeddingTransport policy cfg endpointValue request = do
  prepareLogicalDeadline cfg request >>= \case
    Left failure -> pure (Left failure)
    Right deadline -> do
      let LogicalDeadline absoluteDeadline = deadline
      completed <- withHttpSession forceEmbeddingResult policy absoluteDeadline $ \session ->
        httpEmbeddingTransportUntil policy session cfg endpointValue deadline request
      pure (flattenSessionResult completed)

httpEmbeddingTransportUntil
  :: SessionPolicy
  -> HttpSession
  -> EmbeddingProviderConfig
  -> Text
  -> LogicalDeadline
  -> EmbeddingRequest
  -> IO (Either EmbeddingFailure EmbeddingBatch)
httpEmbeddingTransportUntil policy session cfg endpointValue deadline request =
  case normalizeEmbeddingEndpoint endpointValue of
    Left failure -> pure (Left failure)
    Right endpoint -> collect endpoint [] request.inputs
  where
    collect _ reversed [] = do
      forceVectors reversed
      pure (Right EmbeddingBatch
        { vectors = reverse reversed
        , spaceFingerprint = cfg.spaceFingerprint
        })
    collect routedEndpoint reversed (input : remaining) = do
      result <- go routedEndpoint (cfg.retryAttempts + 1) input
      case result of
        Left failure -> pure (Left failure)
        Right vector -> collect routedEndpoint (vector : reversed) remaining

    go routedEndpoint attempts input = do
      result <- oneAttempt routedEndpoint input
      case result of
        Left failure | failure.retryable && attempts > 1 -> go routedEndpoint (attempts - 1) input
        _ -> pure result

    oneAttempt routedEndpoint input = do
      let singletonRequest = EmbeddingRequest [input]
          encoded = Aeson.encode (embeddingRequestJson singletonRequest)
      remaining <- remainingDeadlineMicros deadline
      if remaining <= 0
        then pure timeoutFailure
        else do
          response <- performBoundedRequest policy session HttpPost routedEndpoint (Just encoded)
            (maximumEmbeddingResponseBytes 1)
          decoded <- evaluate $ response >>= decodeEmbeddingResponse 1
          case decoded of
            Left failure -> pure (Left failure)
            Right [vector] -> forceVector vector >> pure (Right vector)
            Right _ -> pure protocolFailure

forceVectors :: [[Double]] -> IO ()
forceVectors = mapM_ forceVector

forceVector :: [Double] -> IO ()
forceVector = mapM_ (\coordinate -> evaluate coordinate >> pure ())

forceFailure :: EmbeddingFailure -> IO ()
forceFailure failure = evaluate failure.errorCode >> evaluate failure.retryable >> pure ()

forceEmbeddingResult :: Either EmbeddingFailure EmbeddingBatch -> IO ()
forceEmbeddingResult = \case
  Left failure -> forceFailure failure
  Right batch -> do
    forceVectors batch.vectors
    evaluate (T.length batch.spaceFingerprint) >> pure ()

forceAvailabilityResult :: EmbeddingAvailability -> IO ()
forceAvailabilityResult = \case
  EmbeddingUnavailable failure -> forceFailure failure
  EmbeddingAvailable -> pure ()
  EmbeddingDisabled -> pure ()

forceValidationResult :: Either EmbeddingFailure () -> IO ()
forceValidationResult = either forceFailure (const (pure ()))

flattenSessionResult
  :: Either SessionFailure (Either EmbeddingFailure value)
  -> Either EmbeddingFailure value
flattenSessionResult = either (Left . sessionFailure) id

sessionFailure :: SessionFailure -> EmbeddingFailure
sessionFailure = \case
  DeadlineExceeded -> EmbeddingFailure ProviderTimedOut True
  InvalidHelperPath -> EmbeddingFailure ProviderConfigurationError False
  SpawnFailed -> EmbeddingFailure ProviderUnavailable True
  ProtocolViolation -> EmbeddingFailure ProviderProtocolError False
  PipeFailure -> EmbeddingFailure ProviderUnavailable True
  ProcessOwnershipFailure -> EmbeddingFailure ProviderUnavailable True
  ChildFailure code -> case code of
    InvalidRequest -> EmbeddingFailure ProviderConfigurationError False
    NetworkFailure -> EmbeddingFailure ProviderUnavailable True
    InvalidResponse -> EmbeddingFailure ProviderProtocolError False
    BodyTooLarge -> EmbeddingFailure ProviderProtocolError False
    InvalidFrame -> EmbeddingFailure ProviderProtocolError False
    CertificateRejected -> EmbeddingFailure ProviderUnavailable False

makeValidatedGpuEmbeddingProvider
  :: SessionPolicy
  -> EmbeddingProviderConfig
  -> Maybe Text
  -> IO (Either EmbeddingFailure EmbeddingProvider)
makeValidatedGpuEmbeddingProvider policy cfg supervisorEndpoint =
  fmap (fmap fst) (makeValidatedGpuEmbeddingProviderWithInvalidation policy cfg supervisorEndpoint)

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
  :: SessionPolicy
  -> EmbeddingProviderConfig
  -> Maybe Text
  -> IO (Either EmbeddingFailure (EmbeddingProvider, IO ()))
makeValidatedGpuEmbeddingProviderWithInvalidation policy cfg supervisorEndpoint
  = makeValidatedGpuEmbeddingProviderWithLifecycleHooks (pure ()) (pure ()) (pure ()) policy cfg supervisorEndpoint

-- | Construct a generation with an action immediately before validation is
-- published. This narrow synchronization hook lets lifecycle tests withdraw a
-- generation in the otherwise unobservable interval between observing its
-- state and attempting the atomic publication.
makeValidatedGpuEmbeddingProviderWithPublicationHook
  :: IO ()
  -> SessionPolicy
  -> EmbeddingProviderConfig
  -> Maybe Text
  -> IO (Either EmbeddingFailure (EmbeddingProvider, IO ()))
makeValidatedGpuEmbeddingProviderWithPublicationHook beforePublication policy cfg supervisorEndpoint
  = makeValidatedGpuEmbeddingProviderWithLifecycleHooks beforePublication (pure ()) (pure ()) policy cfg supervisorEndpoint

-- | Construct a provider with synchronization points around validation
-- publication and the two state-publication gaps in a user invocation. These
-- hooks are for deterministic lifecycle tests; production callers should use
-- 'makeValidatedGpuEmbeddingProviderWithInvalidation'.
makeValidatedGpuEmbeddingProviderWithLifecycleHooks
  :: IO ()
  -> IO ()
  -> IO ()
  -> SessionPolicy
  -> EmbeddingProviderConfig
  -> Maybe Text
  -> IO (Either EmbeddingFailure (EmbeddingProvider, IO ()))
makeValidatedGpuEmbeddingProviderWithLifecycleHooks beforePublication afterAvailability afterTransport policy cfg supervisorEndpoint
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
          gate <- newProviderGate
          let ensureAvailable = do
                started <- getMonotonicTimeNSec
                let deadline = LogicalDeadline
                      (started + fromIntegral gpuValidationTimeoutMs * 1000000)
                snapshot <- readIORef state
                now <- getMonotonicTimeNSec
                case cachedGpuAvailability now snapshot of
                  Just available -> pure available
                  Nothing -> do
                    admitted <- withProviderGate gate deadline state $ do
                      current <- readIORef state
                      admittedAt <- getMonotonicTimeNSec
                      case cachedGpuAvailability admittedAt current of
                        Just available -> pure (Right available)
                        Nothing -> do
                          let expectedEpoch = current.runtimeEpoch
                              LogicalDeadline absoluteDeadline = deadline
                          completed <- withHttpSession forceAvailabilityResult policy absoluteDeadline $ \session ->
                            ensureGpuAvailabilityAdmitted beforePublication policy session endpointValue
                              state validationLock deadline
                          result <- publishAdmittedSessionFailure state expectedEpoch completed
                          latest <- readIORef state
                          pure $ if latest.runtimeEpoch /= expectedEpoch || latest.runtimeRetired
                            then Right latest.runtimeAvailability else result
                    pure (either EmbeddingUnavailable id admitted)
              invalidate = invalidateGpuRuntime state (EmbeddingFailure ProviderUnavailable True)
              invoke request =
                prepareLogicalDeadline cfg request >>= \case
                  Left failure -> pure (Left failure)
                  Right deadline -> withProviderGate gate deadline state $ do
                    initial <- readIORef state
                    if initial.runtimeRetired
                      then pure $ case initial.runtimeAvailability of
                        EmbeddingUnavailable failure -> Left failure
                        _ -> Left (EmbeddingFailure ProviderCancelled False)
                      else do
                        let expectedEpoch = initial.runtimeEpoch
                            LogicalDeadline absoluteDeadline = deadline
                        completed <- withHttpSession forceEmbeddingResult policy absoluteDeadline $ \session ->
                          ensureGpuAvailabilityAdmitted beforePublication policy session endpointValue
                            state validationLock deadline >>= \case
                              EmbeddingAvailable -> do
                                afterAvailability
                                before <- readIORef state
                                if before.runtimeRetired || before.runtimeAvailability /= EmbeddingAvailable
                                  then pure (Left (EmbeddingFailure ProviderCancelled False))
                                  else do
                                    result <- httpEmbeddingTransportUntil policy session cfg endpointValue deadline request
                                    case result of
                                      Left failure -> pure (Left failure)
                                      Right batch -> do
                                        afterTransport
                                        after <- readIORef state
                                        if after.runtimeEpoch == before.runtimeEpoch
                                            && after.runtimeAvailability == EmbeddingAvailable
                                          then pure (Right batch)
                                          else pure (Left (EmbeddingFailure ProviderCancelled False))
                              EmbeddingUnavailable failure -> pure (Left failure)
                              EmbeddingDisabled -> pure (Left (EmbeddingFailure ProviderDisabled False))
                        result <- case completed of
                          Left sessionProblem ->
                            publishAdmittedSessionFailure state expectedEpoch (Left sessionProblem)
                          Right inner -> do
                            case inner of
                              Left failure -> do
                                current <- readIORef state
                                if current.runtimeAvailability == EmbeddingAvailable
                                    || failure.errorCode == ProviderTimedOut
                                  then invalidateGpuRuntimeIfEpoch state expectedEpoch failure
                                  else pure ()
                              Right _ -> pure ()
                            pure inner
                        latest <- readIORef state
                        pure $ case result of
                          Left failure -> Left failure
                          Right batch
                            | latest.runtimeEpoch /= expectedEpoch || latest.runtimeRetired ->
                                Left (EmbeddingFailure ProviderCancelled False)
                            | otherwise -> Right batch
              provider = EmbeddingProvider
                { availability = ensureAvailable
                , embed = invoke
                }
          pure (Right (provider, invalidate))

publishAdmittedSessionFailure
  :: IORef GpuRuntimeState
  -> Word64
  -> Either SessionFailure value
  -> IO (Either EmbeddingFailure value)
publishAdmittedSessionFailure state expectedEpoch result =
  case result of
    Left problem -> do
      let failure = sessionFailure problem
      invalidateGpuRuntimeIfEpoch state expectedEpoch failure
      pure (Left failure)
    Right value -> pure (Right value)

data GpuRuntimeState = GpuRuntimeState
  { runtimeAvailability :: !EmbeddingAvailability
  , runtimeEpoch :: !Word64
  , nextValidationNs :: !Word64
  , runtimeRetired :: !Bool
  }

cachedGpuAvailability :: Word64 -> GpuRuntimeState -> Maybe EmbeddingAvailability
cachedGpuAvailability now runtime
  | runtime.runtimeRetired = Just runtime.runtimeAvailability
  | runtime.runtimeAvailability == EmbeddingAvailable = Just EmbeddingAvailable
  | now < runtime.nextValidationNs = Just runtime.runtimeAvailability
  | otherwise = Nothing

ensureGpuAvailabilityAdmitted
  :: IO ()
  -> SessionPolicy
  -> HttpSession
  -> Text
  -> IORef GpuRuntimeState
  -> MVar ()
  -> LogicalDeadline
  -> IO EmbeddingAvailability
ensureGpuAvailabilityAdmitted beforePublication policy session endpointValue state validationLock deadline =
  withMVar validationLock $ \_ -> do
    before <- readIORef state
    now <- getMonotonicTimeNSec
    case before.runtimeAvailability of
      _ | before.runtimeRetired -> pure before.runtimeAvailability
      EmbeddingAvailable -> pure EmbeddingAvailable
      unavailable | now < before.nextValidationNs -> pure unavailable
      _ -> do
        validation <- validateGpuEndpointCompatibilityUntil deadline (pure ()) policy session endpointValue
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
  :: SessionPolicy
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
  -> SessionPolicy
  -> Text
  -> IO (Either EmbeddingFailure ())
validateGpuEndpointCompatibilityWithValidationStep timeoutMs validationStep policy endpointValue = do
  started <- getMonotonicTimeNSec
  let deadline = LogicalDeadline (started + fromIntegral timeoutMs * 1000000)
      LogicalDeadline absoluteDeadline = deadline
  completed <- withHttpSession forceValidationResult policy absoluteDeadline $ \session ->
    validateGpuEndpointCompatibilityUntil deadline validationStep policy session endpointValue
  pure (flattenSessionResult completed)

validateGpuEndpointCompatibilityUntil
  :: LogicalDeadline
  -> IO ()
  -> SessionPolicy
  -> HttpSession
  -> Text
  -> IO (Either EmbeddingFailure ())
validateGpuEndpointCompatibilityUntil deadline validationStep policy session endpointValue =
  case embeddedGpuValidationContract of
    Left failure -> pure (Left failure)
    Right contract -> case normalizeEmbeddingEndpoint endpointValue of
      Left failure -> pure (Left failure)
      Right embedEndpoint -> do
        let infoEndpoint = T.dropEnd (T.length embeddingEndpointPath) embedEndpoint <> infoEndpointPath
        infoRemaining <- remainingDeadlineMicros deadline
        if infoRemaining <= 0
          then pure timeoutFailure
          else do
            infoResponse <- performBoundedRequest policy session HttpGet infoEndpoint Nothing maximumInfoResponseBytes
            decodedMetadata <- evaluate (infoResponse >>= decodeGpuEndpointMetadata)
            case decodedMetadata of
              Left failure -> pure (Left failure)
              Right _ -> do
                let probeRequest = EmbeddingRequest contract.validationProbes
                    encoded = Aeson.encode (embeddingRequestJson probeRequest)
                probeRemaining <- remainingDeadlineMicros deadline
                if probeRemaining <= 0
                  then pure timeoutFailure
                  else do
                    probeResponse <- performBoundedRequest policy session HttpPost embedEndpoint (Just encoded)
                      (maximumEmbeddingResponseBytes (length contract.validationProbes))
                    decodedProbe <- evaluate
                      (probeResponse >>= decodeEmbeddingResponse (length contract.validationProbes))
                    case decodedProbe of
                      Left failure -> pure (Left failure)
                      Right decoded -> do
                        validated <- validateGpuProbeResponseWithStep validationStep contract decoded
                        case validated of
                          Left failure -> pure (Left failure)
                          Right () -> forceVectors decoded >> pure (Right ())

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
  :: SessionPolicy
  -> HttpSession
  -> HttpMethod
  -> Text
  -> Maybe LBS.ByteString
  -> Int
  -> IO (Either EmbeddingFailure LBS.ByteString)
performBoundedRequest policy session methodValue endpointValue body maximumBytes = do
  result <- callHttp policy session (HttpCall methodValue
    (Text.encodeUtf8 endpointValue) (maybe BS.empty LBS.toStrict body) maximumBytes)
  pure $ case result of
    Left failure -> Left (sessionFailure failure)
    Right reply
      | reply.replyStatus < 200 || reply.replyStatus >= 300 ->
          Left (EmbeddingFailure ProviderUnavailable (retryableStatus reply.replyStatus))
      | otherwise -> Right (LBS.fromStrict reply.replyBody)

maximumEmbeddingResponseBytes :: Int -> Int
maximumEmbeddingResponseBytes itemCount = max 2 (itemCount * observationEmbeddingDimensions * 40 + itemCount * 2 + 2)

retryableStatus :: Int -> Bool
retryableStatus status = status == 408 || status == 429 || (status >= 500 && status <= 599)

configurationFailure :: EmbeddingFailure
configurationFailure = EmbeddingFailure ProviderConfigurationError False
