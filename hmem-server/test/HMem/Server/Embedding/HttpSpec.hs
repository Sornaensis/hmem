{-# LANGUAGE TemplateHaskell #-}

module HMem.Server.Embedding.HttpSpec (spec, testHelperExecutable) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async, AsyncCancelled(..), async, asyncThreadId, cancel, wait, waitCatch)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar, tryPutMVar)
import Control.Exception
  ( AsyncException(..), Exception(..), SomeException, asyncExceptionFromException
  , asyncExceptionToException, finally, fromException, throwTo, try )
import Control.Monad (unless, void)
import Data.Aeson (FromJSON(..), object, withObject, (.:), (.=))
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.ByteString.Builder (byteString)
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.IORef (atomicModifyIORef', modifyIORef', newIORef, readIORef)
import Data.List (isInfixOf)
import Data.FileEmbed (embedFile, makeRelativeToProject)
import Data.Text qualified as T
import Data.Text.Encoding qualified as Text
import GHC.Conc (BlockReason(..), ThreadStatus(..), threadStatus)
import GHC.Clock (getMonotonicTimeNSec)
import System.Directory (canonicalizePath, doesFileExist)
import System.Environment (getExecutablePath, lookupEnv)
import System.FilePath ((</>), takeDirectory, takeExtension, takeFileName)
import Network.HTTP.Types (mkStatus, status200, status302, status400, status408, status429, status500)
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp (testWithApplication)
import System.Timeout qualified as Timeout
import Test.Hspec

import HMem.Embedding.HttpProcess (SessionPolicy(..))
import HMem.Config
import HMem.Server.Embedding.GteQwen2 (gteQwen2QueryPrefix)
import HMem.Server.Embedding.Http
import HMem.Server.Embedding.Provider
import HMem.Types (observationEmbeddingDimensions)

embeddedReferenceGolden :: BS.ByteString
embeddedReferenceGolden = $(makeRelativeToProject "test/fixtures/embedding-gpu-viability/reference-golden-v1.json" >>= embedFile)

data TestStopProvider = TestStopProvider deriving (Eq, Show)

instance Exception TestStopProvider where
  toException = asyncExceptionToException
  fromException = asyncExceptionFromException

spec :: Spec
spec = describe "TEI HTTP adapter" $ do
  it "creates a provider-neutral TEI request fixture" $ do
    let request = EmbeddingRequest
          [ EmbeddingInput EmbeddingDocument "document"
          , EmbeddingInput EmbeddingQuery "query"
          ]
        encoded = Aeson.encode (embeddingRequestJson request)
    encoded `shouldSatisfy` (`contains` "\"inputs\"")
    encoded `shouldSatisfy` (`contains` "\"document\"")
    encoded `shouldSatisfy` (`contains` "\"normalize\":true")
    encoded `shouldSatisfy` (`contains` "\"truncate\":false")
    embeddingEndpointPath `shouldBe` "/embed"

  it "accepts the observed TEI /model alias with an unavailable model SHA" $ do
    decodeGpuEndpointMetadata validInfoResponse `shouldBe` Right validMetadata

  it "accepts only the three declared model aliases" $ do
    let installed = "/opt/hmem/managed-embedding/model"
    validateGpuEndpointMetadata validMetadata { modelId = installed, servedModelName = installed }
      `shouldBe` Right ()
    validateGpuEndpointMetadata validMetadata
      { modelId = managedTeiModelId, servedModelName = managedTeiModelId }
      `shouldBe` Right ()
    validateGpuEndpointMetadata validMetadata { modelId = "/arbitrary/model" }
      `shouldBe` Left (EmbeddingFailure ProviderProtocolError False)

  it "rejects false endpoint profile claims" $ do
    let rejected metadata = validateGpuEndpointMetadata metadata
          `shouldBe` Left (EmbeddingFailure ProviderProtocolError False)
    rejected validMetadata { modelDtype = "float32" }
    rejected validMetadata { teiSourceSha = "different-source" }
    rejected validMetadata { pooling = "mean" }
    rejected validMetadata { autoTruncate = True }
    rejected validMetadata { defaultPrompt = Just "query: " }
    rejected validMetadata { isCausal = Just True }
    rejected validMetadata { modelSha = Just "different-revision" }

  it "loads authoritative probes in exact order and rejects an ordering mismatch" $ do
    request <- expectRight gpuValidationProbeRequest
    map (.inputKind) request.inputs `shouldBe`
      [EmbeddingDocument, EmbeddingDocument, EmbeddingDocument, EmbeddingQuery]
    map (.inputText) request.inputs `shouldSatisfy` \case
      [_, _, _, queryText] -> "Instruct: Given a web search query" `T.isPrefixOf` queryText
      _ -> False
    validateGpuProbeResponse referenceVectors `shouldBe` Right ()
    validateGpuProbeResponse (reverse referenceVectors)
      `shouldBe` Left (EmbeddingFailure ProviderProtocolError False)

  it "normalizes the fixed embed route exactly once" $ do
    mapM_ (\endpointValue -> normalizeEmbeddingEndpoint endpointValue `shouldBe` Right "http://127.0.0.1:8080/embed")
      [ "http://127.0.0.1:8080"
      , "http://127.0.0.1:8080/"
      , "http://127.0.0.1:8080/embed"
      , "http://127.0.0.1:8080/embed/"
      ]
    normalizeEmbeddingEndpoint "http://:8080" `shouldBe` Left (EmbeddingFailure ProviderConfigurationError False)
    mapM_ (\endpointValue -> normalizeEmbeddingEndpoint endpointValue `shouldBe` Left (EmbeddingFailure ProviderConfigurationError False))
      [ "http://127.0.0.1:8080/admin"
      , "http://127.0.0.1:8080/admin/embed"
      , "http://127.0.0.1:8080/embed/embed"
      ]

  it "accepts exactly one finite 1536-dimensional vector per input" $ do
    let valid = vectorResponse 1
    decodeEmbeddingResponse 1 valid `shouldBe` Right [unitVector]
    decodeEmbeddingResponse 2 valid
      `shouldBe` Left (EmbeddingFailure ProviderProtocolError False)
    decodeEmbeddingResponse 1 (Aeson.encode [replicate 2 (0.25 :: Double)])
      `shouldBe` Left (EmbeddingFailure ProviderProtocolError False)
    decodeEmbeddingResponse 1 (Aeson.encode [replicate observationEmbeddingDimensions (0.25 :: Double)])
      `shouldBe` Left (EmbeddingFailure ProviderProtocolError False)

  it "rejects malformed TEI response fixtures without retaining body text" $ do
    let result = decodeEmbeddingResponse 1 "{not-json}"
    result `shouldBe` Left (EmbeddingFailure ProviderProtocolError False)
    show result `shouldNotSatisfy` isInfixOf "not-json"

  it "sends one JSON POST to the fixed embed route and decodes a successful response" $ do
    seen <- newIORef Nothing
    let app request respond = do
          body <- Wai.strictRequestBody request
          modifyIORef' seen (const (Just (Wai.requestMethod request, Wai.rawPathInfo request, body)))
          respond (Wai.responseLBS status200 [("Content-Type", "application/json")] (vectorResponse 1))
    withTransport app (httpConfig 0 1000) $ \transport -> do
      result <- transport requestOne
      result `shouldBe` Right (EmbeddingBatch [unitVector] managedTeiSpaceFingerprint)
    recorded <- readIORef seen
    recorded `shouldSatisfy` \case
      Just (method, path, body) -> method == "POST" && path == "/embed" && contains body "\"inputs\":[\"document\"]"
      Nothing -> False

  it "admits one through four logical inputs under legacy config and sends ordered singleton bodies" $ do
    seen <- newIORef ([] :: [[T.Text]])
    let app request respond = do
          body <- Wai.strictRequestBody request
          values <- expectInputValues body
          modifyIORef' seen (<> [values])
          respond (Wai.responseLBS status200 [] (vectorResponse (length values)))
        request = EmbeddingRequest
          [ EmbeddingInput EmbeddingDocument "first"
          , EmbeddingInput EmbeddingQuery (gteQwen2QueryPrefix <> "already formatted")
          , EmbeddingInput EmbeddingDocument longInput
          , EmbeddingInput EmbeddingDocument "fourth"
          ]
        longInput = T.replicate 2048 "l"
        config = (httpConfig 0 1000) { batchSize = 256 }
    withTransport app config $ \transport -> do
      result <- transport request
      result `shouldBe` Right (EmbeddingBatch (replicate 4 unitVector) managedTeiSpaceFingerprint)
    readIORef seen `shouldReturn`
      [ ["first"]
      , [gteQwen2QueryPrefix <> "already formatted"]
      , [longInput]
      , ["fourth"]
      ]

  it "rejects empty, over-logical-limit, and oversized formatted inputs before network IO" $ do
    calls <- newIORef (0 :: Int)
    let app _ respond = modifyIORef' calls (+ 1)
          >> respond (Wai.responseLBS status200 [] (vectorResponse 1))
        config = (httpConfig 0 1000) { batchSize = 32 }
        five = EmbeddingRequest (replicate 5 (EmbeddingInput EmbeddingDocument "small"))
        oversized = EmbeddingRequest [EmbeddingInput EmbeddingDocument (T.replicate 32768 "a")]
    withTransport app config $ \transport -> do
      transport (EmbeddingRequest []) `shouldReturn` Left (EmbeddingFailure ProviderConfigurationError False)
      transport five `shouldReturn` Left (EmbeddingFailure ProviderConfigurationError False)
      transport oversized `shouldReturn` Left (EmbeddingFailure ProviderConfigurationError False)
    readIORef calls `shouldReturn` 0

  it "accepts exact formatted UTF8 limits and counts multibyte input bytes without rewriting" $ do
    seen <- newIORef ([] :: [T.Text])
    let app _request respond = do
          -- Each logical input is physically isolated, so the response count is one.
          respond (Wai.responseLBS status200 [] (vectorResponse 1))
        exactAscii = T.replicate 32767 "a"
        exactMultibyte = T.replicate 16383 "é" <> "a"
        config = (httpConfig 0 1000) { batchSize = 4 }
        recording request respond = do
          body <- Wai.strictRequestBody request
          values <- expectInputValues body
          modifyIORef' seen (<> values)
          app request respond
    withTransport recording config $ \transport ->
      transport (EmbeddingRequest
        [ EmbeddingInput EmbeddingDocument exactAscii
        , EmbeddingInput EmbeddingDocument exactMultibyte
        ]) `shouldReturn` Right (EmbeddingBatch (replicate 2 unitVector) managedTeiSpaceFingerprint)
    readIORef seen `shouldReturn` [exactAscii, exactMultibyte]

  it "selects compact and full ceilings from formatted UTF8 bytes plus one EOS" $ do
    let config = (httpConfig 0 300000) { batchSize = 4 }
        timeoutFor kind value = logicalEmbeddingTimeoutMs config
          (EmbeddingRequest [EmbeddingInput kind value])
        prefixedPadding = T.replicate
          (2047 - BS.length (Text.encodeUtf8 gteQwen2QueryPrefix)) "q"
    timeoutFor EmbeddingDocument (T.replicate 2047 "a") `shouldBe` Right 30000
    timeoutFor EmbeddingDocument (T.replicate 2048 "a") `shouldBe` Right 300000
    timeoutFor EmbeddingDocument (T.replicate 1023 "é") `shouldBe` Right 30000
    timeoutFor EmbeddingDocument (T.replicate 1024 "é") `shouldBe` Right 300000
    timeoutFor EmbeddingQuery (gteQwen2QueryPrefix <> prefixedPadding) `shouldBe` Right 30000
    logicalEmbeddingTimeoutMs config (EmbeddingRequest
      [ EmbeddingInput EmbeddingDocument "short"
      , EmbeddingInput EmbeddingDocument (T.replicate 2048 "a")
      ]) `shouldBe` Right 300000
    logicalEmbeddingTimeoutMs config (EmbeddingRequest
      [ EmbeddingInput EmbeddingDocument (T.replicate 2048 "a")
      , EmbeddingInput EmbeddingDocument "short"
      ]) `shouldBe` Right 300000
    logicalEmbeddingTimeoutMs (config { timeoutMs = 30000 })
      (EmbeddingRequest [EmbeddingInput EmbeddingDocument (T.replicate 2048 "a")])
      `shouldBe` Right 30000

  it "returns no partial batch and stops after a later singleton failure" $ do
    seen <- newIORef ([] :: [T.Text])
    let app request respond = do
          body <- Wai.strictRequestBody request
          [value] <- expectInputValues body
          modifyIORef' seen (<> [value])
          if value == "fail"
            then respond (Wai.responseLBS status500 [] "failure")
            else respond (Wai.responseLBS status200 [] (vectorResponse 1))
        config = (httpConfig 0 1000) { batchSize = 4 }
    withTransport app config $ \transport ->
      transport (EmbeddingRequest
        [ EmbeddingInput EmbeddingDocument "first"
        , EmbeddingInput EmbeddingDocument "fail"
        , EmbeddingInput EmbeddingDocument "never-sent"
        ]) `shouldReturn` Left (EmbeddingFailure ProviderUnavailable True)
    readIORef seen `shouldReturn` ["first", "fail"]

  it "acquires availability only after metadata and exact golden validation" $ do
    calls <- newIORef ([] :: [(BS.ByteString, BS.ByteString, LBS.ByteString)])
    let app request respond = do
          body <- Wai.strictRequestBody request
          modifyIORef' calls (<> [(Wai.requestMethod request, Wai.rawPathInfo request, body)])
          serveCompatible request body respond
    withEndpoint app $ \policy endpointValue -> do
      provider <- expectRight =<< makeValidatedGpuEmbeddingProvider policy (gpuConfig endpointValue) Nothing
      readIORef calls `shouldReturn` []
      provider.availability `shouldReturn` EmbeddingAvailable
      firstCalls <- readIORef calls
      map (\(_, path, _) -> path) firstCalls `shouldBe` ["/info", "/embed"]
      case firstCalls of
        [_, (_, _, probeBody)] -> do
          expectedProbe <- expectRight gpuValidationProbeRequest
          Aeson.decode probeBody `shouldBe` Just (embeddingRequestJson expectedProbe)
        _ -> expectationFailure "unexpected validation request sequence"
      provider.availability `shouldReturn` EmbeddingAvailable
      readIORef calls `shouldReturn` firstCalls

  it "admits one active operation and two waiters, rejects overflow, and releases every slot" $ do
    userStarted <- newEmptyMVar
    releaseUser <- newEmptyMVar
    blockNextUser <- newIORef True
    let app request respond = do
          body <- Wai.strictRequestBody request
          if Wai.rawPathInfo request == "/info"
            then respond (Wai.responseLBS status200 [] validInfoResponse)
            else do
              count <- expectInputCount body
              if count == 4
                then respond (Wai.responseLBS status200 [] referenceVectorResponse)
                else do
                  shouldBlock <- atomicModifyIORef' blockNextUser (\value -> (False, value))
                  if shouldBlock then putMVar userStarted () >> takeMVar releaseUser else pure ()
                  respond (Wai.responseLBS status200 [] (vectorResponse 1))
    withEndpoint app $ \policy endpointValue -> do
      provider <- expectRight =<< makeValidatedGpuEmbeddingProvider policy
        (gpuConfig endpointValue) { batchSize = 4, timeoutMs = 2000 } Nothing
      provider.availability `shouldReturn` EmbeddingAvailable
      active <- async (provider.embed requestOne)
      takeMVar userStarted
      firstWaiter <- async (provider.embed requestOne)
      secondWaiter <- async (provider.embed requestOne)
      withinTestTimeout 1000000 (waitUntilBlockedOnAdmission firstWaiter)
      withinTestTimeout 1000000 (waitUntilBlockedOnAdmission secondWaiter)
      provider.embed requestOne `shouldReturn` Left (EmbeddingFailure ProviderUnavailable True)
      putMVar releaseUser ()
      mapM_ (\worker -> wait worker `shouldReturn` Right (EmbeddingBatch [unitVector] managedTeiSpaceFingerprint))
        [active, firstWaiter, secondWaiter]
      provider.embed requestOne `shouldReturn` Right (EmbeddingBatch [unitVector] managedTeiSpaceFingerprint)

  it "expires queued invocation admission at the shorter remaining operation deadline" $ do
    infoStarted <- newEmptyMVar
    releaseInfo <- newEmptyMVar
    blockInfo <- newIORef True
    let app request respond = do
          body <- Wai.strictRequestBody request
          if Wai.rawPathInfo request == "/info"
            then do
              shouldBlock <- atomicModifyIORef' blockInfo (\value -> (False, value))
              if shouldBlock then putMVar infoStarted () >> takeMVar releaseInfo else pure ()
              respond (Wai.responseLBS status200 [] validInfoResponse)
            else serveCompatible request body respond
    withEndpoint app $ \policy endpointValue -> do
      provider <- expectRight =<< makeValidatedGpuEmbeddingProvider policy
        (gpuConfig endpointValue) { timeoutMs = 150 } Nothing
      validating <- async provider.availability
      takeMVar infoStarted
      started <- getMonotonicTimeNSec
      provider.embed requestOne `shouldReturn` Left (EmbeddingFailure ProviderTimedOut True)
      finished <- getMonotonicTimeNSec
      let elapsedMs = fromIntegral (finished - started) / 1000000 :: Double
      elapsedMs `shouldSatisfy` (>= 100)
      elapsedMs `shouldSatisfy` (< 600)
      putMVar releaseInfo ()
      wait validating `shouldReturn` EmbeddingAvailable
      provider.embed requestOne `shouldReturn`
        Right (EmbeddingBatch [unitVector] managedTeiSpaceFingerprint)

  it "caps queued admission at five seconds and reuses the slot afterward" $ do
    infoStarted <- newEmptyMVar
    releaseInfo <- newEmptyMVar
    blockInfo <- newIORef True
    let app request respond = do
          body <- Wai.strictRequestBody request
          if Wai.rawPathInfo request == "/info"
            then do
              shouldBlock <- atomicModifyIORef' blockInfo (\value -> (False, value))
              if shouldBlock then putMVar infoStarted () >> takeMVar releaseInfo else pure ()
              respond (Wai.responseLBS status200 [] validInfoResponse)
            else serveCompatible request body respond
    withEndpoint app $ \policy endpointValue -> do
      provider <- expectRight =<< makeValidatedGpuEmbeddingProvider policy
        (gpuConfig endpointValue) { timeoutMs = 300000 } Nothing
      validating <- async provider.availability
      takeMVar infoStarted
      started <- getMonotonicTimeNSec
      provider.embed requestOne `shouldReturn` Left (EmbeddingFailure ProviderUnavailable True)
      finished <- getMonotonicTimeNSec
      let elapsedMs = fromIntegral (finished - started) / 1000000 :: Double
      elapsedMs `shouldSatisfy` (>= 4500)
      elapsedMs `shouldSatisfy` (< 7000)
      putMVar releaseInfo ()
      wait validating `shouldReturn` EmbeddingAvailable
      provider.embed requestOne `shouldReturn`
        Right (EmbeddingBatch [unitVector] managedTeiSpaceFingerprint)

  it "releases the active gate when an invocation hook throws" $ do
    failOnce <- newIORef True
    let app request respond = Wai.strictRequestBody request >>= \body -> serveCompatible request body respond
        afterAvailability = do
          shouldFail <- atomicModifyIORef' failOnce (\value -> (False, value))
          if shouldFail then ioError (userError "synthetic invocation failure") else pure ()
    withEndpoint app $ \policy endpointValue -> do
      (provider, _) <- expectRight =<<
        makeValidatedGpuEmbeddingProviderWithLifecycleHooks (pure ()) afterAvailability (pure ()) policy
          (gpuConfig endpointValue) Nothing
      provider.availability `shouldReturn` EmbeddingAvailable
      provider.embed requestOne `shouldReturn`
        Left (EmbeddingFailure ProviderUnavailable True)
      provider.availability `shouldReturn`
        EmbeddingUnavailable (EmbeddingFailure ProviderUnavailable True)
      threadDelay 1100000
      provider.availability `shouldReturn` EmbeddingAvailable
      provider.embed requestOne `shouldReturn`
        Right (EmbeddingBatch [unitVector] managedTeiSpaceFingerprint)

  it "publishes the transport lifecycle hook only after a decoded successful batch" $ do
    publications <- newIORef (0 :: Int)
    let app request respond = do
          body <- Wai.strictRequestBody request
          if Wai.rawPathInfo request == "/info"
            then respond (Wai.responseLBS status200 [] validInfoResponse)
            else expectInputCount body >>= \case
              4 -> respond (Wai.responseLBS status200 [] referenceVectorResponse)
              _ -> respond (Wai.responseLBS status500 [] "real user response failed")
    withEndpoint app $ \policy endpointValue -> do
      (provider, _) <- expectRight =<<
        makeValidatedGpuEmbeddingProviderWithLifecycleHooks
          (pure ()) (pure ()) (modifyIORef' publications (+ 1)) policy
          (gpuConfig endpointValue) Nothing
      provider.availability `shouldReturn` EmbeddingAvailable
      provider.embed requestOne `shouldReturn`
        Left (EmbeddingFailure ProviderUnavailable True)
      readIORef publications `shouldReturn` 0

  it "invalidates an active invocation timeout before releasing its gate and then revalidates" $ do
    publicationObserved <- newEmptyMVar
    releasePublication <- newEmptyMVar
    blockOnce <- newIORef True
    calls <- newIORef (0 :: Int)
    let app request respond = do
          modifyIORef' calls (+ 1)
          body <- Wai.strictRequestBody request
          serveCompatible request body respond
        afterTransport = do
          shouldBlock <- atomicModifyIORef' blockOnce (\value -> (False, value))
          if shouldBlock
            then putMVar publicationObserved () >> takeMVar releasePublication
            else pure ()
        scenario policy endpointValue = do
          (provider, _) <- expectRight =<<
            makeValidatedGpuEmbeddingProviderWithLifecycleHooks
              (pure ()) (pure ()) afterTransport policy
              ((gpuConfig endpointValue) { timeoutMs = 100 }) Nothing
          provider.availability `shouldReturn` EmbeddingAvailable
          invoking <- async (provider.embed requestOne)
          takeMVar publicationObserved
          wait invoking `shouldReturn` Left (EmbeddingFailure ProviderTimedOut True)
          provider.availability `shouldReturn`
            EmbeddingUnavailable (EmbeddingFailure ProviderTimedOut True)
          callsBeforeCooldown <- readIORef calls
          threadDelay 1100000
          provider.availability `shouldReturn` EmbeddingAvailable
          readIORef calls `shouldReturn` (callsBeforeCooldown + 2)
          provider.embed requestOne `shouldReturn`
            Right (EmbeddingBatch [unitVector] managedTeiSpaceFingerprint)
    withEndpoint app $ \policy endpointValue ->
      scenario policy endpointValue `finally` void (tryPutMVar releasePublication ())

  it "keeps a cold validation helper deadline as the cached timeout" $ do
    validationReached <- newEmptyMVar
    releaseValidation <- newEmptyMVar
    let app request respond = Wai.strictRequestBody request >>= \body -> serveCompatible request body respond
        beforePublication = putMVar validationReached () >> takeMVar releaseValidation
        scenario policy endpointValue = do
          (provider, _) <- expectRight =<<
            makeValidatedGpuEmbeddingProviderWithPublicationHook beforePublication policy
              ((gpuConfig endpointValue) { timeoutMs = 2000 }) Nothing
          invoking <- async (provider.embed requestOne)
          withinTestTimeout 1500000 (takeMVar validationReached)
          wait invoking `shouldReturn` Left (EmbeddingFailure ProviderTimedOut True)
          provider.availability `shouldReturn`
            EmbeddingUnavailable (EmbeddingFailure ProviderTimedOut True)
    withEndpoint app $ \policy endpointValue ->
      scenario policy endpointValue `finally` void (tryPutMVar releaseValidation ())

  it "caches admitted helper acquisition and readiness failures" $ do
    withTestPolicy $ \policy -> do
      let missing = policy { helperExecutable = helperExecutable policy <> ".absent" }
      provider <- expectRight =<< makeValidatedGpuEmbeddingProvider missing
        (gpuConfig "http://127.0.0.1:1") Nothing
      provider.availability `shouldReturn`
        EmbeddingUnavailable (EmbeddingFailure ProviderConfigurationError False)
      provider.availability `shouldReturn`
        EmbeddingUnavailable (EmbeddingFailure ProviderConfigurationError False)
    readinessCalls <- newIORef (0 :: Int)
    let app request respond = Wai.strictRequestBody request >>= \body -> serveCompatible request body respond
    withEndpoint app $ \policy endpointValue -> do
      let failReadiness = policy
            { afterSpawnBeforeReady = \_ ->
                modifyIORef' readinessCalls (+ 1) >> ioError (userError "fixture readiness failure")
            }
      provider <- expectRight =<< makeValidatedGpuEmbeddingProvider failReadiness
        (gpuConfig endpointValue) Nothing
      provider.availability `shouldReturn`
        EmbeddingUnavailable (EmbeddingFailure ProviderUnavailable True)
      provider.availability `shouldReturn`
        EmbeddingUnavailable (EmbeddingFailure ProviderUnavailable True)
      readIORef readinessCalls `shouldReturn` 1
      threadDelay 1100000
      provider.availability `shouldReturn`
        EmbeddingUnavailable (EmbeddingFailure ProviderUnavailable True)
      readIORef readinessCalls `shouldReturn` 2

  it "invalidates an available generation after a post-validation helper failure" $ do
    successfulResponses <- newIORef (0 :: Int)
    wireCalls <- newIORef (0 :: Int)
    let app request respond = do
          modifyIORef' wireCalls (+ 1)
          Wai.strictRequestBody request >>= \body -> serveCompatible request body respond
    withEndpoint app $ \policy endpointValue -> do
      let failUserReply = policy { afterSuccessfulResponse = \_ -> do
            count <- atomicModifyIORef' successfulResponses (\value -> let next = value + 1 in (next, next))
            if count == 3 then ioError (userError "fixture post-validation failure") else pure ()
            }
      provider <- expectRight =<< makeValidatedGpuEmbeddingProvider failUserReply
        (gpuConfig endpointValue) Nothing
      provider.availability `shouldReturn` EmbeddingAvailable
      provider.embed requestOne `shouldReturn`
        Left (EmbeddingFailure ProviderUnavailable True)
      readIORef successfulResponses `shouldReturn` 3
      beforeCached <- readIORef wireCalls
      provider.availability `shouldReturn`
        EmbeddingUnavailable (EmbeddingFailure ProviderUnavailable True)
      readIORef wireCalls `shouldReturn` beforeCached

  it "keeps cold validation and ordered singleton inference inside one invocation deadline" $ do
    calls <- newIORef (0 :: Int)
    let app request respond = do
          modifyIORef' calls (+ 1)
          body <- Wai.strictRequestBody request
          threadDelay 70000
          serveCompatible request body respond
        config endpointValue = (gpuConfig endpointValue)
          { batchSize = 2, timeoutMs = 220 }
        request = EmbeddingRequest
          [ EmbeddingInput EmbeddingDocument "first"
          , EmbeddingInput EmbeddingDocument "second"
          ]
    withEndpoint app $ \policy endpointValue -> do
      provider <- expectRight =<< makeValidatedGpuEmbeddingProvider policy (config endpointValue) Nothing
      started <- getMonotonicTimeNSec
      provider.embed request `shouldReturn` Left (EmbeddingFailure ProviderTimedOut True)
      finished <- getMonotonicTimeNSec
      let elapsedMs = fromIntegral (finished - started) / 1000000 :: Double
      elapsedMs `shouldSatisfy` (>= 140)
      elapsedMs `shouldSatisfy` (< 600)
      readIORef calls >>= (`shouldSatisfy` (\count -> count >= 2 && count <= 4))

  it "keeps queued recovery validation, retry, and ordered singletons inside one deadline" $ do
    firstInfo <- newIORef True
    recoveryInfoStarted <- newEmptyMVar
    releaseRecoveryInfo <- newEmptyMVar
    secondStarted <- newEmptyMVar
    failFirstUser <- newIORef True
    userInputs <- newIORef ([] :: [T.Text])
    let app request respond = do
          body <- Wai.strictRequestBody request
          if Wai.rawPathInfo request == "/info"
            then do
              shouldFail <- atomicModifyIORef' firstInfo (\value -> (False, value))
              if shouldFail
                then respond (Wai.responseLBS status500 [] "initial recovery failure")
                else do
                  putMVar recoveryInfoStarted ()
                  takeMVar releaseRecoveryInfo
                  threadDelay 60000
                  respond (Wai.responseLBS status200 [] validInfoResponse)
            else expectInputValues body >>= \case
              values | length values == 4 -> do
                threadDelay 60000
                respond (Wai.responseLBS status200 [] referenceVectorResponse)
              [value] -> do
                modifyIORef' userInputs (<> [value])
                if value == "second"
                  then do
                    getMonotonicTimeNSec >>= putMVar secondStarted
                    threadDelay 500000
                    respond (Wai.responseLBS status200 [] (vectorResponse 1))
                  else do
                    threadDelay 60000
                    shouldFail <- if value == "first"
                      then atomicModifyIORef' failFirstUser (\current -> (False, current))
                      else pure False
                    if shouldFail
                      then respond (Wai.responseLBS status500 [] "retry first singleton")
                      else respond (Wai.responseLBS status200 [] (vectorResponse 1))
              _ -> respond (Wai.responseLBS status400 [] "unexpected logical batch")
        config endpointValue = (gpuConfig endpointValue)
          { batchSize = 3, timeoutMs = 650, retryAttempts = 1 }
        request = EmbeddingRequest
          [ EmbeddingInput EmbeddingDocument "first"
          , EmbeddingInput EmbeddingDocument "second"
          , EmbeddingInput EmbeddingDocument "third"
          ]
        scenario policy endpointValue = do
          provider <- expectRight =<< makeValidatedGpuEmbeddingProvider policy
            (config endpointValue) Nothing
          provider.availability `shouldReturn`
            EmbeddingUnavailable (EmbeddingFailure ProviderUnavailable True)
          threadDelay 1100000
          recovering <- async provider.availability
          takeMVar recoveryInfoStarted
          started <- getMonotonicTimeNSec
          invoking <- async $ do
            invoked <- getMonotonicTimeNSec
            result <- provider.embed request
            returned <- getMonotonicTimeNSec
            pure (result, invoked, returned)
          withinTestTimeout 1000000 $ waitUntilBlockedOnAdmission invoking
          threadDelay 80000
          putMVar releaseRecoveryInfo ()
          withinTestTimeout 1000000 (wait recovering) `shouldReturn` EmbeddingAvailable
          secondAt <- withinTestTimeout 1000000 (takeMVar secondStarted)
          let secondElapsedMs = fromIntegral (secondAt - started) / 1000000 :: Double
          secondElapsedMs `shouldSatisfy` (>= 250)
          secondElapsedMs `shouldSatisfy` (< 600)
          (result, invoked, returned) <- withinTestTimeout 1000000 (wait invoking)
          result `shouldBe` Left (EmbeddingFailure ProviderTimedOut True)
          finished <- getMonotonicTimeNSec
          let elapsedMs = fromIntegral (finished - started) / 1000000 :: Double
              operationElapsedMs = fromIntegral (returned - invoked) / 1000000 :: Double
          elapsedMs `shouldSatisfy` (>= 600)
          elapsedMs `shouldSatisfy` (< 800)
          operationElapsedMs `shouldSatisfy` (< 800)
          (elapsedMs - secondElapsedMs) `shouldSatisfy` (< 500)
          seen <- readIORef userInputs
          take 3 seen `shouldBe` ["first", "first", "second"]
          seen `shouldNotContain` ["third"]
          provider.availability `shouldReturn`
            EmbeddingUnavailable (EmbeddingFailure ProviderTimedOut True)
    withEndpoint app $ \policy endpointValue ->
      scenario policy endpointValue `finally` void (tryPutMVar releaseRecoveryInfo ())

  it "requires the explicit native GPU assertion without network IO" $ do
    calls <- newIORef (0 :: Int)
    let app _ respond = modifyIORef' calls (+ 1)
          >> respond (Wai.responseLBS status500 [] "unused")
    withEndpoint app $ \policy endpointValue -> do
      rejected <- makeValidatedGpuEmbeddingProvider policy
        (gpuConfig endpointValue) { gpuProfile = Nothing } Nothing
      case rejected of
        Left failure -> failure `shouldBe` EmbeddingFailure ProviderConfigurationError False
        Right _ -> expectationFailure "provider accepted a missing GPU assertion"
      wrong <- makeValidatedGpuEmbeddingProvider policy
        (gpuConfig endpointValue) { gpuProfile = Just "cpu-or-unknown" } Nothing
      case wrong of
        Left failure -> failure `shouldBe` EmbeddingFailure ProviderConfigurationError False
        Right _ -> expectationFailure "provider accepted a wrong GPU assertion"
      disabled <- expectRight =<< makeValidatedGpuEmbeddingProvider policy
        defaultConfig.embeddingProvider Nothing
      disabled.availability `shouldReturn` EmbeddingDisabled
      readIORef calls `shouldReturn` 0

  it "invalidates validation already in flight and permanently retires that generation" $ do
    infoStarted <- newEmptyMVar
    releaseInfo <- newEmptyMVar
    blockInfo <- newIORef True
    calls <- newIORef (0 :: Int)
    let app request respond = do
          modifyIORef' calls (+ 1)
          body <- Wai.strictRequestBody request
          if Wai.rawPathInfo request == "/info"
            then do
              shouldBlock <- readIORef blockInfo
              if shouldBlock
                then modifyIORef' blockInfo (const False) >> putMVar infoStarted () >> takeMVar releaseInfo
                else pure ()
              respond (Wai.responseLBS status200 [("Content-Type", "application/json")] validInfoResponse)
            else serveCompatible request body respond
    withEndpoint app $ \policy endpointValue -> do
      (provider, invalidate) <- expectRight =<<
        makeValidatedGpuEmbeddingProviderWithInvalidation policy (gpuConfig endpointValue) Nothing
      checking <- async provider.availability
      takeMVar infoStarted
      invalidate
      putMVar releaseInfo ()
      wait checking `shouldReturn`
        EmbeddingUnavailable (EmbeddingFailure ProviderUnavailable True)
      callsAfterRetirement <- readIORef calls
      provider.availability `shouldReturn`
        EmbeddingUnavailable (EmbeddingFailure ProviderUnavailable True)
      readIORef calls `shouldReturn` callsAfterRetirement
      replacement <- expectRight =<<
        makeValidatedGpuEmbeddingProvider policy (gpuConfig endpointValue) Nothing
      replacement.availability `shouldReturn` EmbeddingAvailable
      readIORef calls `shouldReturn` (callsAfterRetirement + 2)

  it "cannot publish validation over retirement after observing the old epoch" $ do
    publicationObserved <- newEmptyMVar
    releasePublication <- newEmptyMVar
    calls <- newIORef (0 :: Int)
    let app request respond = do
          modifyIORef' calls (+ 1)
          body <- Wai.strictRequestBody request
          serveCompatible request body respond
        beforePublication = putMVar publicationObserved () >> takeMVar releasePublication
    withEndpoint app $ \policy endpointValue -> do
      (provider, invalidate) <- expectRight =<<
        makeValidatedGpuEmbeddingProviderWithPublicationHook beforePublication policy
          (gpuConfig endpointValue) Nothing
      checking <- async provider.availability
      takeMVar publicationObserved
      invalidate
      putMVar releasePublication ()
      wait checking `shouldReturn`
        EmbeddingUnavailable (EmbeddingFailure ProviderUnavailable True)
      callsAfterRetirement <- readIORef calls
      provider.availability `shouldReturn`
        EmbeddingUnavailable (EmbeddingFailure ProviderUnavailable True)
      readIORef calls `shouldReturn` callsAfterRetirement

  it "propagates queued cancellation without invalidating the active validation" $ do
    infoStarted <- newEmptyMVar
    releaseInfo <- newEmptyMVar
    blockInfo <- newIORef True
    calls <- newIORef (0 :: Int)
    let app request respond = do
          modifyIORef' calls (+ 1)
          body <- Wai.strictRequestBody request
          if Wai.rawPathInfo request == "/info"
            then do
              shouldBlock <- readIORef blockInfo
              if shouldBlock
                then modifyIORef' blockInfo (const False) >> putMVar infoStarted () >> takeMVar releaseInfo
                else pure ()
              respond (Wai.responseLBS status200 [("Content-Type", "application/json")] validInfoResponse)
            else serveCompatible request body respond
    withEndpoint app $ \policy endpointValue -> do
      provider <- expectRight =<< makeValidatedGpuEmbeddingProvider policy (gpuConfig endpointValue) Nothing
      validating <- async provider.availability
      takeMVar infoStarted
      queued <- async provider.availability
      withinTestTimeout 1000000 $ waitUntilBlockedOnAdmission queued
      cancel queued
      shouldBeCancelled queued
      putMVar releaseInfo ()
      wait validating `shouldReturn` EmbeddingAvailable
      provider.availability `shouldReturn` EmbeddingAvailable

  it "cancels a queued embed without stale transport and reuses the released gate" $ do
    activePublished <- newEmptyMVar
    releaseActive <- newEmptyMVar
    blockOnce <- newIORef True
    userCalls <- newIORef (0 :: Int)
    let app request respond = do
          body <- Wai.strictRequestBody request
          count <- if Wai.rawPathInfo request == "/info" then pure 0 else expectInputCount body
          if count == 1 then modifyIORef' userCalls (+ 1) else pure ()
          serveCompatible request body respond
        afterTransport = do
          shouldBlock <- atomicModifyIORef' blockOnce (\value -> (False, value))
          if shouldBlock then putMVar activePublished () >> takeMVar releaseActive else pure ()
        scenario policy endpointValue = do
          (provider, _) <- expectRight =<<
            makeValidatedGpuEmbeddingProviderWithLifecycleHooks
              (pure ()) (pure ()) afterTransport policy (gpuConfig endpointValue) Nothing
          provider.availability `shouldReturn` EmbeddingAvailable
          active <- async (provider.embed requestOne)
          takeMVar activePublished
          queued <- async (provider.embed requestOne)
          withinTestTimeout 1000000 $ waitUntilBlockedOnAdmission queued
          cancel queued
          shouldBeCancelled queued
          readIORef userCalls `shouldReturn` 1
          void (tryPutMVar releaseActive ())
          wait active `shouldReturn`
            Right (EmbeddingBatch [unitVector] managedTeiSpaceFingerprint)
          provider.availability `shouldReturn` EmbeddingAvailable
          provider.embed requestOne `shouldReturn`
            Right (EmbeddingBatch [unitVector] managedTeiSpaceFingerprint)
          readIORef userCalls `shouldReturn` 2
    withEndpoint app $ \policy endpointValue ->
      scenario policy endpointValue `finally` void (tryPutMVar releaseActive ())

  it "retires a provider while an embed waits without sending stale transport" $ do
    activePublished <- newEmptyMVar
    releaseActive <- newEmptyMVar
    userCalls <- newIORef (0 :: Int)
    blockOnce <- newIORef True
    let app request respond = do
          body <- Wai.strictRequestBody request
          count <- if Wai.rawPathInfo request == "/info" then pure 0 else expectInputCount body
          if count == 1 then modifyIORef' userCalls (+ 1) else pure ()
          serveCompatible request body respond
        afterTransport = do
          shouldBlock <- atomicModifyIORef' blockOnce (\value -> (False, value))
          if shouldBlock then putMVar activePublished () >> takeMVar releaseActive else pure ()
        scenario policy endpointValue = do
          (provider, invalidate) <- expectRight =<<
            makeValidatedGpuEmbeddingProviderWithLifecycleHooks
              (pure ()) (pure ()) afterTransport policy (gpuConfig endpointValue) Nothing
          provider.availability `shouldReturn` EmbeddingAvailable
          active <- async (provider.embed requestOne)
          takeMVar activePublished
          queued <- async (provider.embed requestOne)
          withinTestTimeout 1000000 $ waitUntilBlockedOnAdmission queued
          invalidate
          void (tryPutMVar releaseActive ())
          wait active `shouldReturn` Left (EmbeddingFailure ProviderCancelled False)
          wait queued `shouldReturn` Left (EmbeddingFailure ProviderUnavailable True)
          readIORef userCalls `shouldReturn` 1
          provider.availability `shouldReturn`
            EmbeddingUnavailable (EmbeddingFailure ProviderUnavailable True)
    withEndpoint app $ \policy endpointValue ->
      scenario policy endpointValue `finally` void (tryPutMVar releaseActive ())

  it "propagates active cancellation after availability and invalidates the generation" $ do
    invocationObserved <- newEmptyMVar
    releaseInvocation <- newEmptyMVar
    let app request respond = Wai.strictRequestBody request >>= \body -> serveCompatible request body respond
        afterAvailability = putMVar invocationObserved () >> takeMVar releaseInvocation
    withEndpoint app $ \policy endpointValue -> do
      (provider, _) <- expectRight =<<
        makeValidatedGpuEmbeddingProviderWithLifecycleHooks (pure ()) afterAvailability (pure ()) policy
          (gpuConfig endpointValue) Nothing
      provider.availability `shouldReturn` EmbeddingAvailable
      worker <- async (provider.embed requestOne)
      takeMVar invocationObserved
      cancel worker
      shouldBeCancelled worker
      provider.availability `shouldReturn`
        EmbeddingUnavailable (EmbeddingFailure ProviderCancelled False)

  it "publishes active cancellation before admitting a queued successor" $ do
    publicationObserved <- newEmptyMVar
    releasePublication <- newEmptyMVar
    let app request respond = Wai.strictRequestBody request >>= \body -> serveCompatible request body respond
        afterTransport = putMVar publicationObserved () >> takeMVar releasePublication
        scenario policy endpointValue = do
          (provider, _) <- expectRight =<<
            makeValidatedGpuEmbeddingProviderWithLifecycleHooks (pure ()) (pure ()) afterTransport policy
              (gpuConfig endpointValue) Nothing
          provider.availability `shouldReturn` EmbeddingAvailable
          worker <- async (provider.embed requestOne)
          takeMVar publicationObserved
          queued <- async (provider.embed requestOne)
          withinTestTimeout 1000000 $ waitUntilBlockedOnAdmission queued
          cancel worker
          shouldBeCancelled worker
          wait queued `shouldReturn` Left (EmbeddingFailure ProviderCancelled False)
          provider.availability `shouldReturn`
            EmbeddingUnavailable (EmbeddingFailure ProviderCancelled False)
    withEndpoint app $ \policy endpointValue ->
      scenario policy endpointValue `finally` void (tryPutMVar releasePublication ())

  it "fences a queued successor after custom asynchronous provider cancellation" $ do
    publicationObserved <- newEmptyMVar
    releasePublication <- newEmptyMVar
    userCalls <- newIORef (0 :: Int)
    let app request respond = do
          body <- Wai.strictRequestBody request
          count <- if Wai.rawPathInfo request == "/info" then pure 0 else expectInputCount body
          if count == 1 then modifyIORef' userCalls (+ 1) else pure ()
          serveCompatible request body respond
        afterTransport = putMVar publicationObserved () >> takeMVar releasePublication
        scenario policy endpointValue = do
          (provider, _) <- expectRight =<<
            makeValidatedGpuEmbeddingProviderWithLifecycleHooks
              (pure ()) (pure ()) afterTransport policy (gpuConfig endpointValue) Nothing
          provider.availability `shouldReturn` EmbeddingAvailable
          active <- async (provider.embed requestOne)
          takeMVar publicationObserved
          queued <- async (provider.embed requestOne)
          withinTestTimeout 1000000 $ waitUntilBlockedOnAdmission queued
          throwTo (asyncThreadId active) TestStopProvider
          waitCatch active >>= \case
            Left exception ->
              (fromException exception :: Maybe TestStopProvider) `shouldBe` Just TestStopProvider
            Right _ -> expectationFailure "custom provider cancellation was swallowed"
          wait queued `shouldReturn` Left (EmbeddingFailure ProviderCancelled False)
          readIORef userCalls `shouldReturn` 1
          provider.availability `shouldReturn`
            EmbeddingUnavailable (EmbeddingFailure ProviderCancelled False)
    withEndpoint app $ \policy endpointValue ->
      scenario policy endpointValue `finally` void (tryPutMVar releasePublication ())

  it "rejects an empirically prompted endpoint during provider acquisition" $ do
    let app request respond = do
          body <- Wai.strictRequestBody request
          if Wai.rawPathInfo request == "/info"
            then respond (Wai.responseLBS status200 [("Content-Type", "application/json")] validInfoResponse)
            else do
              count <- expectInputCount body
              let promptedVectors = if count == 4
                    then take 3 referenceVectors <> [wrongPromptQueryVector]
                    else replicate count unitVector
              respond (Wai.responseLBS status200 [("Content-Type", "application/json")] (Aeson.encode promptedVectors))
    withEndpoint app $ \policy endpointValue -> do
      provider <- expectRight =<< makeValidatedGpuEmbeddingProvider policy (gpuConfig endpointValue) Nothing
      provider.availability `shouldReturn`
        EmbeddingUnavailable (EmbeddingFailure ProviderProtocolError False)

  it "keeps decoded probe numerical validation inside the startup deadline" $ do
    numericalValidationStarted <- newEmptyMVar
    let app request respond = Wai.strictRequestBody request >>= \body -> serveCompatible request body respond
        slowNumericalValidation = putMVar numericalValidationStarted () >> threadDelay 3000000
    withEndpoint app $ \policy endpointValue -> do
      validating <- async $
        validateGpuEndpointCompatibilityWithValidationStep 1000 slowNumericalValidation policy endpointValue
      withinTestTimeout 2000000 (takeMVar numericalValidationStarted)
      wait validating `shouldReturn` Left (EmbeddingFailure ProviderTimedOut True)

  it "discards a user response when its generation is withdrawn in flight" $ do
    blockUser <- newIORef False
    userStarted <- newEmptyMVar
    releaseUser <- newEmptyMVar
    let app request respond = do
          body <- Wai.strictRequestBody request
          if Wai.rawPathInfo request == "/info"
            then respond (Wai.responseLBS status200 [("Content-Type", "application/json")] validInfoResponse)
            else do
              count <- expectInputCount body
              if count == 4
                then respond (Wai.responseLBS status200 [("Content-Type", "application/json")] referenceVectorResponse)
                else do
                  shouldBlock <- readIORef blockUser
                  if shouldBlock then putMVar userStarted () >> takeMVar releaseUser else pure ()
                  respond (Wai.responseLBS status200 [("Content-Type", "application/json")] (vectorResponse count))
    withEndpoint app $ \policy endpointValue -> do
      (provider, invalidate) <- expectRight =<<
        makeValidatedGpuEmbeddingProviderWithInvalidation policy (gpuConfig endpointValue) Nothing
      provider.availability `shouldReturn` EmbeddingAvailable
      modifyIORef' blockUser (const True)
      worker <- async (provider.embed requestOne)
      takeMVar userStarted
      invalidate
      putMVar releaseUser ()
      wait worker `shouldReturn` Left (EmbeddingFailure ProviderCancelled False)

  it "revalidates with non-user probes after a transient user transport failure" $ do
    failNextUser <- newIORef True
    validationCalls <- newIORef (0 :: Int)
    let app request respond = do
          body <- Wai.strictRequestBody request
          if Wai.rawPathInfo request == "/info"
            then do
              modifyIORef' validationCalls (+ 1)
              respond (Wai.responseLBS status200 [("Content-Type", "application/json")] validInfoResponse)
            else do
              count <- expectInputCount body
              if count == 4
                then do
                  modifyIORef' validationCalls (+ 1)
                  respond (Wai.responseLBS status200 [("Content-Type", "application/json")] referenceVectorResponse)
                else do
                  shouldFail <- readIORef failNextUser
                  if shouldFail
                    then modifyIORef' failNextUser (const False)
                      >> respond (Wai.responseLBS status500 [] "transient")
                    else respond (Wai.responseLBS status200 [] (vectorResponse count))
    withEndpoint app $ \policy endpointValue -> do
      provider <- expectRight =<< makeValidatedGpuEmbeddingProvider policy (gpuConfig endpointValue) Nothing
      provider.availability `shouldReturn` EmbeddingAvailable
      provider.embed requestOne `shouldReturn` Left (EmbeddingFailure ProviderUnavailable True)
      provider.availability `shouldReturn`
        EmbeddingUnavailable (EmbeddingFailure ProviderUnavailable True)
      threadDelay 1100000
      provider.availability `shouldReturn` EmbeddingAvailable
      readIORef validationCalls `shouldReturn` 4
      provider.embed requestOne `shouldReturn`
        Right (EmbeddingBatch [unitVector] managedTeiSpaceFingerprint)

  it "keeps retries inside one total request deadline" $ do
    calls <- newIORef (0 :: Int)
    let app _ respond = do
          modifyIORef' calls (+ 1)
          threadDelay 80000
          respond (Wai.responseLBS status500 [] "retry")
    withTransport app (httpConfig 5 150) $ \transport ->
      transport requestOne `shouldReturn` Left (EmbeddingFailure ProviderTimedOut True)
    callCount <- readIORef calls
    callCount `shouldSatisfy` (\count -> count > 0 && count <= 2)

  it "returns a protocol failure for a malformed successful response" $ do
    let app _ respond = respond (Wai.responseLBS status200 [] "not-json")
    withTransport app (httpConfig 0 1000) $ \transport ->
      transport requestOne `shouldReturn` Left (EmbeddingFailure ProviderProtocolError False)

  it "bounds chunked successful response bodies and releases the stream" $ do
    finished <- newEmptyMVar
    let oversized = BS.replicate (20 * 1024 * 1024) 120
        app _ respond =
          (respond (Wai.responseStream status200 [] $ \write _ -> write (byteString oversized)))
            `finally` putMVar finished ()
    withTransport app (httpConfig 0 5000) $ \transport ->
      transport requestOne `shouldReturn` Left (EmbeddingFailure ProviderProtocolError False)
    Timeout.timeout 1000000 (takeMVar finished) `shouldReturn` Just ()

  it "applies the same total deadline while a successful body streams slowly" $ do
    finished <- newEmptyMVar
    let app _ respond =
          (respond (Wai.responseStream status200 [] $ \write flush -> do
            write (byteString "[")
            flush
            threadDelay 250000
            write (byteString "]")))
          `finally` putMVar finished ()
    withTransport app (httpConfig 0 100) $ \transport ->
      transport requestOne `shouldReturn` Left (EmbeddingFailure ProviderTimedOut True)
    Timeout.timeout 1000000 (takeMVar finished) `shouldReturn` Just ()

  it "rejects an oversized declared response before consuming its body" $ do
    let app _ respond = respond (Wai.responseLBS status200 [("Content-Length", "999999999")] "[]")
    withTransport app (httpConfig 0 1000) $ \transport ->
      transport requestOne `shouldReturn` Left (EmbeddingFailure ProviderProtocolError False)

  it "retries only 408, 429, and 500 through 599 exactly to the configured bound" $ do
    withinTestTimeout 30000000 $
      mapM_ (\retryStatus -> do
        calls <- newIORef (0 :: Int)
        let app _ respond = do
              modifyIORef' calls (+ 1)
              current <- readIORef calls
              respond (Wai.responseLBS (if current < 3 then retryStatus else status200) [] (vectorResponse 1))
        withTransport app (httpConfig 2 1000) $ \transport -> do
          result <- transport requestOne
          result `shouldBe` Right (EmbeddingBatch [unitVector] managedTeiSpaceFingerprint)
        readIORef calls `shouldReturn` 3)
      [status408, status429, status500, mkStatus 599 "synthetic-server-error"]

  it "does not retry a nonretryable response status" $ do
    mapM_ (\nonRetryStatus -> do
      calls <- newIORef (0 :: Int)
      let app _ respond = do
            modifyIORef' calls (+ 1)
            respond (Wai.responseLBS nonRetryStatus [] "bad request")
      withTransport app (httpConfig 3 1000) $ \transport ->
        transport requestOne `shouldReturn` Left (EmbeddingFailure ProviderUnavailable False)
      readIORef calls `shouldReturn` 1)
      [status400]

  it "retries connection failures exactly to the configured bound" $ do
    calls <- newIORef (0 :: Int)
    let app _ respond = do
          modifyIORef' calls (+ 1)
          respond (Wai.responseRaw (\_ _ -> pure ()) (Wai.responseLBS status500 [] "unused"))
    withTransport app (httpConfig 2 1000) $ \transport ->
      transport requestOne `shouldReturn` Left (EmbeddingFailure ProviderUnavailable True)
    readIORef calls `shouldReturn` 3

  it "fails closed on redirects without sending the request body to the redirect route" $ do
    redirectCalls <- newIORef (0 :: Int)
    let app request respond
          | Wai.rawPathInfo request == "/embed" =
              respond (Wai.responseLBS status302 [("Location", "/redirect-target")] "")
          | otherwise = do
              modifyIORef' redirectCalls (+ 1)
              respond (Wai.responseLBS status200 [] (vectorResponse 1))
    withTransport app (httpConfig 0 1000) $ \transport ->
      transport requestOne `shouldReturn` Left (EmbeddingFailure ProviderUnavailable False)
    readIORef redirectCalls `shouldReturn` 0

  it "reports malformed endpoint failures without exposing endpoint text" $ do
    withTestPolicy $ \policy -> do
      let secretEndpoint = "http://127.0.0.1:1/?secret=do-not-log"
      result <- httpEmbeddingTransport policy (httpConfig 0 250) secretEndpoint requestOne
      result `shouldBe` Left (EmbeddingFailure ProviderConfigurationError False)
      show result `shouldNotSatisfy` isInfixOf "do-not-log"

  it "returns a structured timeout and releases the request" $ do
    let app _ respond = do
          threadDelay 250000
          respond (Wai.responseLBS status200 [] (vectorResponse 1))
    withTransport app (httpConfig 0 100) $ \transport ->
      transport requestOne `shouldReturn` Left (EmbeddingFailure ProviderTimedOut True)

  it "propagates transport cancellation after owned helper cleanup" $ do
    let app _ respond = do
          threadDelay 1000000
          respond (Wai.responseLBS status200 [] (vectorResponse 1))
    withTransport app (httpConfig 0 1000) $ \transport -> do
      worker <- async (transport requestOne)
      threadDelay 20000
      cancel worker
      shouldBeCancelled worker

  it "propagates unrelated asynchronous faults" $ do
    let app _ respond = do
          threadDelay 1000000
          respond (Wai.responseLBS status200 [] (vectorResponse 1))
    withTransport app (httpConfig 0 1000) $ \transport -> do
      worker <- async (transport requestOne)
      threadDelay 20000
      throwTo (asyncThreadId worker) StackOverflow
      result <- try @SomeException (wait worker)
      case result of
        Left caught -> (fromException caught :: Maybe AsyncException) `shouldBe` Just StackOverflow
        Right _ -> expectationFailure "unrelated asynchronous fault was swallowed"

  it "keeps the pinned model-space fingerprint independent of endpoint routing" $ do
    let config = defaultConfig.embeddingProvider
          { mode = EmbeddingProviderHttp, endpoint = Just "https://gpu.example" }
    config.spaceFingerprint `shouldBe` managedTeiSpaceFingerprint

requestOne :: EmbeddingRequest
requestOne = EmbeddingRequest [EmbeddingInput EmbeddingDocument "document"]

httpConfig :: Int -> Int -> EmbeddingProviderConfig
httpConfig retries timeoutMillis = defaultConfig.embeddingProvider
  { mode = EmbeddingProviderHttp
  , endpoint = Just "http://127.0.0.1:8080"
  , retryAttempts = retries
  , timeoutMs = timeoutMillis
  }

gpuConfig :: T.Text -> EmbeddingProviderConfig
gpuConfig endpointValue = (httpConfig 0 1000)
  { endpoint = Just endpointValue
  , gpuProfile = Just managedTeiGpuProfile
  }

validMetadata :: GpuEndpointMetadata
validMetadata = GpuEndpointMetadata
  { modelId = "/model"
  , servedModelName = "/model"
  , modelSha = Nothing
  , teiVersion = "1.9.3"
  , teiSourceSha = "06670157fb6c1523482219bdb2d1660277d38088"
  , modelDtype = "float16"
  , pooling = "last_token"
  , maxInputLength = 32768
  , maxBatchTokens = 32768
  , autoTruncate = False
  , defaultPrompt = Nothing
  , isCausal = Nothing
  }

validInfoResponse :: LBS.ByteString
validInfoResponse = Aeson.encode $ object
  [ "model_id" .= ("/model" :: T.Text)
  , "served_model_name" .= ("/model" :: T.Text)
  , "model_sha" .= Aeson.Null
  , "version" .= ("1.9.3" :: T.Text)
  , "sha" .= ("06670157fb6c1523482219bdb2d1660277d38088" :: T.Text)
  , "model_dtype" .= ("float16" :: T.Text)
  , "model_type" .= object ["embedding" .= object ["pooling" .= ("last_token" :: T.Text)]]
  , "max_input_length" .= (32768 :: Int)
  , "max_batch_tokens" .= (32768 :: Int)
  , "auto_truncate" .= False
  , "default_prompt" .= Aeson.Null
  ]

newtype ReferenceGolden = ReferenceGolden { cases :: [ReferenceCase] }

instance FromJSON ReferenceGolden where
  parseJSON = withObject "ReferenceGolden" $ \o -> ReferenceGolden <$> o .: "cases"

newtype ReferenceCase = ReferenceCase { vector :: [Double] }

instance FromJSON ReferenceCase where
  parseJSON = withObject "ReferenceCase" $ \o -> ReferenceCase <$> o .: "vector"

referenceVectors :: [[Double]]
referenceVectors = case Aeson.eitherDecodeStrict' embeddedReferenceGolden :: Either String ReferenceGolden of
  Left parseFailure -> error ("invalid embedded reference golden in test: " <> parseFailure)
  Right fixture -> map (.vector) fixture.cases


-- Captured by the committed GPU-foundation qualification from the raw query
-- probe with its required instruction omitted. Authority:
-- D:\hmem-artifacts\aa30a81c-2adf-4f54-b1eb-cfeaa39bac77\gpu-foundation-v1\validation\6ed32e069c3a41038f1a7e3c95da5693\foundation-validation-report-v1.json
-- 562758 bytes; SHA256 012eaaa5fbb98328e71210231e49c179d57a3fec8fa177e085a6c86cc02cc630.
-- Against the query_search golden: cosine 0.8937135074517196,
-- L2 0.46105639687483163, max-coordinate error 0.0562999629581871.
wrongPromptQueryVector :: [Double]
wrongPromptQueryVector =
  [ 0.0075329687, 0.039325897, -0.010450538, -0.00096384, 0.027312376, -0.028342105
  , 0.0029666044, -0.0366535, 0.019393258, 0.0077536255, 0.00016099116, -0.073846385
  , -0.02049654, 0.0046583046, 0.0041373097, 0.0022341474, -0.022408897, -0.016622793
  , 0.008017187, -0.0023735901, 0.0044928123, 0.020717196, 0.044597138, 0.005746264
  , -0.015740165, -0.035501186, -0.006319358, 0.005960791, 0.010266658, -0.007201984
  , 0.018106094, -0.03677609, -0.016463429, 0.058890775, -0.00883852, 0.008060093
  , -0.0381981, 0.006815835, -0.017419606, 0.024198666, -0.005623677, -0.033441722
  , -0.015825978, -8.27462e-05, -0.0038614892, -0.013214874, -0.017456383, -0.0005370841
  , 0.01805706, -0.017909955, 0.029273767, -0.031750023, -0.007931377, -0.016009858
  , 0.08061319, -0.01570339, -0.005105747, -0.0071713375, 0.019380998, -0.015936306
  , -0.013276168, 0.007839437, -0.018486114, 0.035844427, -0.0049218666, -0.033784967
  , -0.03197068, 0.014453003, -0.001459551, 0.00300951, -0.010652807, 0.0088691665
  , -0.046337873, 0.016304066, 0.010278917, 0.022690848, -0.028611796, -0.0011645762
  , 0.0056757764, -0.018866133, 0.009518878, -0.006870999, 0.025154844, -0.05021162
  , -0.022163723, -0.0112044485, -0.014796246, 0.013300685, 0.02669944, -0.022249533
  , 0.016046634, -0.033515275, -0.030695776, 0.008256231, 0.015225301, -0.043003507
  , -0.010757006, -0.0011316309, -0.035574738, -0.055115096, 0.035010837, -0.0018127547
  , -0.003536634, -0.0117683485, -0.007888471, -0.02679751, -0.07605295, 0.002977331
  , 0.010468926, -0.029322801, -0.030646741, 0.026086506, -0.0031443555, -0.047269534
  , 0.016426653, -0.040282074, 0.06222514, -0.0019751824, 0.008599475, -0.04741664
  , -0.016904742, -0.00072402926, 0.031088054, -0.035182457, -0.008574958, -0.030573187
  , 0.036579948, -0.008004929, -0.007863954, -0.0030064452, -0.052908532, -0.023745095
  , -0.003668415, -0.032044232, -0.021023665, -0.013680705, 0.009616947, -0.023916716
  , 0.013374237, 0.025694227, 0.010150201, 0.01852289, -0.013876844, 0.001904695
  , 0.0029574104, -0.012522258, -0.01805706, -0.00695681, 0.046779186, 0.014906575
  , 0.032485545, -0.016794413, 0.013361979, 0.0007201984, -0.017946731, 0.03290234
  , -0.01062829, 0.08375141, 0.013398755, 0.030131875, 0.014869799, -0.012080945
  , -0.03479018, -0.015727907, -0.017542195, -0.0013009541, 0.021587564, 0.009206281
  , 0.013766516, -0.027631102, -0.012528388, 0.008207197, -0.036212187, 0.03631026
  , 0.029249249, 0.032926857, -0.041507944, 0.05565448, 0.03528053, -0.044253893
  , 0.0061140247, 0.0015629837, 0.0117438305, 0.02444384, 0.00987438, -0.018167388
  , -0.022776658, -0.02981315, 0.09154794, 0.036040567, 0.012945184, 0.0005535568
  , 0.03807551, -0.010757006, -0.009659853, 0.009439196, 0.006270323, -0.022825692
  , 0.0227644, 0.003023301, 0.0018663865, -0.0014932625, 0.029690562, 0.035305046
  , 0.0026248933, 0.0072510187, -0.0122893425, -0.07061009, 0.019221636, 0.015360147
  , -0.011112508, -0.0042752204, -0.01928293, 0.033588827, -0.029102145, -0.028072415
  , -0.014833023, -0.024382547, -0.0051854285, 0.005473508, 0.001554556, -0.011394458
  , -0.019846829, -0.035721842, 0.005078165, -0.002986525, -0.010707971, 0.012932925
  , -0.008617863, -0.0065951785, -0.010260529, 0.0112044485, 0.0038890713, 0.011774478
  , -0.012191273, 0.0066809896, 0.0055133486, 0.008029446, 0.039620105, 0.015249818
  , -0.01824094, -0.007379735, 0.0039626234, -0.02248245, -0.009739534, -0.011057344
  , 0.03942397, 0.0005535568, 0.0017085557, -0.02321797, -0.016022116, -0.0077413665
  , -0.038100027, -0.0046583046, 0.0066809896, -0.021391425, 0.016402135, -0.036530916
  , 0.020422988, 0.028121449, 0.0042660264, -0.02256826, 0.017051846, 0.0016809737
  , -0.017309278, -0.0028486145, 0.0016334712, -0.015911788, 0.00029669877, -0.008060093
  , 0.03770775, -0.023340557, 0.005464314, 0.0042292504, 0.015678873, -0.006938422
  , -0.00010697628, -0.0026861867, -0.020288141, -0.009947932, -0.0024992416, -0.03638381
  , -0.01757897, 0.037266437, 0.040233042, -0.022923762, -0.01702733, 0.015789201
  , 0.006901646, 0.04013497, -0.0041894093, 0.027067201, -0.024308994, 0.020888818
  , 0.014869799, 0.0045387824, -0.018437078, -0.010352469, 0.0082868785, 0.0042261854
  , -0.018915169, 0.00036891014, -0.044915862, -0.0100460015, -0.025449052, 0.027165271
  , 0.03476566, -0.026871063, -0.0051854285, -0.02153853, 0.0015568545, 0.015825978
  , -0.04221895, 0.0015645161, 0.022323085, -0.0077720135, 0.023242489, -0.015323371
  , 0.012798078, 0.009163375, 0.01729702, 0.007943635, 0.03508439, 0.02406382
  , 0.0021253515, 0.008495277, 0.0077413665, -0.00038155192, 0.040306594, 0.017836403
  , -0.032608133, -0.07095333, 0.030818362, -0.052123975, 0.033834003, -0.00893659
  , 0.010150201, -0.0044836183, 0.04802957, -0.016966036, 0.02209017, -0.0011867951
  , 0.032313924, 0.071443684, 0.0028746643, 0.024517393, -0.0006696313, -0.004069887
  , 0.0349618, 0.020606868, -0.011492528, -0.0005240593, -0.0076678144, 0.0427093
  , -0.099589646, 0.0017116205, 0.010352469, -0.02313216, -0.009696629, -0.024027044
  , 0.014244605, -0.0007615715, 0.0048023444, -0.0033803354, 0.03800196, 0.006107895
  , 0.05236915, -0.02981315, 0.0013760387, 0.013999431, 0.018755805, -0.0025023064
  , 0.02256826, -0.0058228807, -0.020827524, 0.024946447, 0.0070181037, 0.0006792084
  , 0.006570661, 0.050603896, 0.03910524, -0.006083378, 0.0045847525, 0.020717196
  , -0.01993264, 0.030254463, -0.01306777, -0.004787021, 0.017088622, -0.042807367
  , -0.04400872, 0.0023735901, -0.026969131, -0.0088814255, 0.047612775, -0.03339269
  , 0.009224669, -0.03393207, -0.03047512, -0.009273703, -0.006227418, -0.01992038
  , 0.030867398, -0.022556001, 0.021305613, -0.010168589, -0.00639291, 0.030622223
  , -0.0017836402, -0.044866826, -0.011584468, -0.00902853, 0.0031244352, 0.006754542
  , 0.0062764524, -0.036163155, 0.019859089, -0.018363526, 0.016708603, 0.047024358
  , 0.013374237, -0.0037389023, -0.021599824, -0.001664118, -0.03787937, -0.0016656504
  , -0.008409466, 0.026037471, -0.018265458, 0.018755805, -0.028145967, -0.0034845343
  , -0.01965069, -0.009512749, 0.082525544, -0.023830906, -0.023377333, 0.013509084
  , -0.018228682, 0.033515275, 0.009715017, -0.011480269, -0.024137372, -0.0094637135
  , 0.0025191621, -0.039154276, -0.0146368835, 0.014563331, 0.017652523, -0.017162174
  , -0.029396353, -0.016696345, 0.045970112, 0.036408328, -0.014183312, -0.018547406
  , 0.00035167133, 0.003695997, 0.033907555, 0.0056573884, -0.0026969132, 0.036457364
  , 0.03770775, 0.011314777, 0.05820429, -0.045112003, 0.066932485, 0.029935736
  , -0.010707971, -0.013692964, 0.044400997, -0.0029788632, 0.01993264, -0.1079746
  , 0.028121449, -0.0044621653, 0.0048360555, -0.010573125, 0.007073268, 0.16358005
  , 0.009941802, -0.017885437, 0.0039166533, -0.0015783071, 0.042635746, -0.019993933
  , 0.0022035006, -0.029666046, -0.0072265016, -0.0035580865, -0.022102429, -0.016291806
  , -0.0073490883, 0.0013538197, 0.026037471, -0.0036040568, 0.024664497, -0.035035353
  , -0.01410976, 2.8755261e-05, 0.019577138, 0.009083694, 0.028464692, 0.031137088
  , -0.015004644, -0.003069271, -0.003282266, -0.0047287922, 0.0083175255, -0.0002419177
  , 0.0062580644, -0.009359514, 0.014992385, 0.010321822, 0.022739882, -0.002496177
  , -0.006889387, -0.0069874567, -0.041900225, 0.0075084516, 0.015078196, 0.024689015
  , -0.029592492, -0.0069261636, 0.022298569, 0.023475403, 0.026552336, 0.0037848724
  , -0.017431866, 0.0012932925, 0.083653346, -0.0015085858, 0.01862096, 0.0050046127
  , -0.0014089838, 0.010481185, -0.005847398, 0.039620105, 0.018939685, 0.012522258
  , 0.00064319844, -0.015335629, 0.012724526, 0.018106094, 0.09586301, -0.0018740481
  , -0.006319358, -0.015678873, -0.0003028281, -0.0010450538, 0.004324255, 0.02557164
  , -0.022298569, -0.02772917, -0.00940242, 0.0008918201, 0.0018418691, 0.00033309177
  , 0.037487093, -0.01945455, -0.022261793, 0.00020839783, 0.006901646, -0.0019476004
  , -0.0076862024, 0.01185416, -0.0134600485, -0.015850494, 0.02152627, 0.0043457076
  , -0.030377049, 0.036923192, 0.060312785, -0.0016120186, -0.017456383, 0.01795899
  , 0.011762219, -0.01504142, 0.024983224, 0.03959559, 0.0083297845, 0.0030845944
  , -0.061244447, -0.027336892, -0.008219455, -0.009181763, 0.05565448, 0.023279265
  , -0.0045020063, -0.0025038386, -0.033098478, 0.036040567, 0.025743263, -0.013680705
  , -0.015078196, 0.029690562, 0.01485754, -0.03753613, 0.013411013, -0.046386905
  , 0.005773846, 0.017125398, -0.014207829, 0.023806388, 0.03243651, 0.03574636
  , -0.030328015, 0.0070242328, 0.0015162475, 0.0011768348, 0.030793846, 0.03591798
  , 0.024566427, 0.0043119965, 0.0009079097, 0.04719598, 0.025914883, -0.026576852
  , -0.018008025, -0.014575589, 0.011106378, -0.0033435593, -0.010407633, -0.010475056
  , 0.012712268, 0.017346054, 0.021759186, -0.005856592, -0.042733815, -0.022298569
  , 0.017333796, 0.030426083, 0.0005006911, 0.0047042747, -0.029224731, -0.02031266
  , -0.00996632, -0.008771097, 0.0052038166, 0.017468642, 0.01832675, -0.010456668
  , 0.0038768128, 0.023046348, 9.423873e-05, 0.013754257, 0.046828218, 0.056242898
  , -0.011596726, 0.020852042, -0.016757637, 0.0023199583, 0.03856586, 0.022519225
  , 0.014440744, 0.014710436, 0.022028876, 0.006564532, 0.0055777067, 0.056831315
  , -0.02227405, -0.033024926, 0.019025497, -0.017897697, -0.021244321, 0.011578338
  , 0.04521007, 0.0017284761, -0.0050352593, -0.02387994, -0.01334972, 0.030499635
  , 0.037462577, -0.018682253, 0.035672806, 0.014330416, -0.010977662, -0.02321797
  , 0.05820429, -0.01334972, 0.005262045, -0.023916716, -0.0064848503, 0.012749044
  , 0.007091656, 0.025326466, 0.0032485544, -0.02021459, 0.04212088, 0.07659233
  , 0.010873464, -0.025914883, -0.011369941, -0.01024827, 0.029666046, 0.006993586
  , -0.0064358157, 0.029273767, 0.017615747, 0.0039626234, 0.020042969, -0.012209661
  , 0.0059485324, 0.033834003, 0.01429364, 0.0020502668, 0.019699724, 0.045798488
  , -0.015262077, -0.01467366, 0.040355626, -0.013582636, 0.042464122, -0.013901361
  , -0.00070793973, -0.018768065, -0.025154844, 0.05678228, -0.0012549841, -0.0021973713
  , 0.007539098, 0.0032332311, -0.0033650121, -0.021035923, 0.023732835, 0.001418178
  , -0.030867398, -0.0036224448, 0.0076862024, -0.012650974, 0.009862121, 0.019613914
  , -0.016794413, 0.04192474, -0.006307099, 0.028415658, -0.0021299485, -0.040355626
  , 0.029567976, 0.013594894, 0.027533032, -0.044621654, 0.005844333, -0.036898676
  , 0.02792531, 0.0074594165, 0.0043181255, -0.00441926, -0.012614198, -0.013386496
  , 0.040355626, 0.0021253515, -0.0016978295, 0.009292092, 0.006288711, 0.038884584
  , -0.024848377, -0.028047897, 0.0032117784, 0.047245014, -0.0009975514, 0.019761018
  , 0.0061753183, -0.0023781871, -0.008814002, -0.010236011, -0.02613554, -0.03760968
  , -0.013153581, 0.021379165, 0.018449338, -0.013496825, 0.015090455, 0.0040208525
  , -0.011222837, 0.042978987, 0.009770181, 0.011958358, -0.037756786, -0.010971533
  , -0.012369025, 0.004906543, 0.030303497, 0.05771394, 0.019233894, 0.020165555
  , -0.035427634, -0.024860635, -0.011241225, -0.012638716, 0.0014128147, 0.01683119
  , 0.054967992, -0.001554556, 0.0043487726, 0.007398123, 0.010977662, -0.0009416211
  , -0.10777846, -0.0028057091, -0.0022326151, -0.041777637, -0.079093106, -0.025350984
  , 0.017407348, -4.0750587e-05, 0.008562699, 0.0010994518, 0.03513342, -0.008133645
  , -0.008942719, -0.043224163, -0.021918548, -0.0012664766, 0.0059945025, 0.041042116
  , -0.02096237, -0.071149476, 0.0022739882, -0.017051846, -0.0020012322, 0.014943351
  , 0.022310827, 0.0033343653, -0.02679751, -0.035672806, -0.00597305, 0.011259613
  , -0.011664149, 0.052761428, -0.02217598, -0.008231714, 0.03513342, 0.013239392
  , -0.0021238192, 0.04334675, 0.05004, -0.0014848346, 0.010309564, 0.0033619474
  , 0.0021805156, 0.0010350937, -0.0152007835, -0.011327036, 0.013276168, -0.003032495
  , -0.01306777, 0.016720861, -0.0048299264, 0.015029161, -0.0030493506, 0.021195285
  , -0.0069629396, 0.012381284, 0.003846166, 0.020937853, -0.033221066, -0.0061508007
  , -0.014502037, 0.05104521, 0.014710436, -0.015078196, -0.003545828, 0.056340966
  , 0.009482102, 0.035427634, -0.017591229, -0.018633218, -0.024333512, 0.00012737552
  , -0.032681685, -0.008127515, -0.011455752, 0.005151717, 0.0039534294, -0.0040668226
  , 0.03601605, -0.007747496, -0.058155254, 0.04106663, 0.020790748, 0.024909671
  , -0.032387476, -0.0014051531, -0.0032179079, 0.016647309, -0.038958136, -0.024885153
  , -0.014489779, -0.047171462, -0.011314777, 0.038860068, -0.040110454, -0.036800604
  , -0.0019491327, -0.051780734, -0.015127231, -0.008391078, -0.06850159, -0.008256231
  , -0.0017499289, -0.011425105, -0.0072326306, -0.003300654, 0.016046634, -0.030033806
  , -0.019123565, 0.004333449, -0.018951945, 0.00723876, -0.0111799305, -0.038100027
  , -0.04221895, -0.017149916, -0.001936874, -0.004688951, -0.01674538, -0.024848377
  , 0.021599824, -0.0017361379, 0.041311808, -0.01391362, -0.00611096, -0.020165555
  , -0.049525134, 0.028047897, -0.018596442, -0.020177813, 0.027900793, -0.037462577
  , -0.014710436, 0.017799627, 0.00036144, 0.00085810875, 0.0088691665, -0.013925879
  , 0.014992385, -0.036874156, -0.009549525, -0.026258128, 0.060214717, 0.004682822
  , -0.022825692, -0.027214305, 0.02041073, 0.01757897, 0.029151179, 0.040845975
  , 0.0042445734, 0.032779753, 0.04501393, 0.019908123, -0.07786724, 0.022654071
  , -0.03008284, 0.0010013822, -0.003610186, -0.011032826, 0.022923762, -0.015666613
  , -0.0060404725, 0.020239107, -0.05423247, -0.023524437, -0.0040913397, -0.012626457
  , 0.036628984, -0.010861205, 0.0033803354, -0.013374237, 0.0009554121, -0.003714385
  , 0.02895504, 0.027214305, 2.2206916e-05, 0.027214305, -0.009224669, -0.009794698
  , 0.010481185, -0.009623077, 0.028538244, -0.008814002, -0.033049446, -0.019699724
  , 0.01052409, -0.015666613, 0.0030110423, 0.007220372, -0.01570339, 0.03476566
  , -0.011835772, -0.013938137, -0.04589656, 0.012626457, -0.005182364, 0.039987866
  , 0.012945184, 0.019687466, -0.024456099, -0.004204733, -0.0029022463, -0.027287858
  , -0.02444384, -0.0359425, 0.014269122, -0.0020977694, -0.0074778046, -0.019246154
  , -0.016978294, -0.010953145, -0.023009572, -0.005225269, 0.012945184, -0.015997598
  , 0.0047625033, 0.014734953, -0.0007355218, 0.00065086014, 0.045823008, -0.0066809896
  , -0.07620005, 0.035697322, -0.03505987, -0.06256839, 0.008115257, -0.01034634
  , 0.0020272818, -0.002736754, 0.03307396, -0.004397807, -0.011676408, -0.024505133
  , 0.00015696877, 0.004587817, -0.020190073, 0.0197365, 0.03751161, -0.010965404
  , -0.00041181556, 0.021746928, 0.011333165, 0.0327062, 0.019662948, -0.019197118
  , -0.009132729, 0.044523586, -0.025301948, 0.0050567123, 0.012160626, -0.003760355
  , -0.022985056, -0.018106094, -0.015568544, 0.024566427, 0.02971508, 0.007201984
  , -0.007342959, 0.14377, 0.0008098401, 0.012638716, -0.012994218, 0.0070058447
  , 0.015531768, -0.018853875, -0.008605605, -0.014318157, 9.331693e-06, 0.0094759725
  , -0.009108211, 0.035967015, 0.04521007, -0.017002812, 0.038835548, 0.0013951928
  , -0.012234178, 0.0100582605, -0.004578623, -0.022494707, 0.01702733, 0.014685918
  , 0.0041189217, -0.017346054, 0.009727275, -0.02041073, 0.025252914, -0.008814002
  , 0.035893463, 0.0051486525, 0.014649142, -0.053251777, -0.009954061, 0.011578338
  , -0.013656188, 0.008225585, 0.005924015, -0.010787653, 0.009574042, -0.033711415
  , -0.0024103662, -0.030278979, -0.003695997, -0.067913175, 0.020913336, 0.0047747623
  , -0.026356196, 0.025400018, 0.024112856, 0.019331964, 0.016132444, 0.0077168494
  , 0.014183312, 0.031995196, 0.03714385, -0.0123322485, 0.02217598, -0.0072265016
  , 0.0072816657, -0.034618556, 0.007937506, -0.0083052665, 0.025718745, -0.00639291
  , 0.0078087896, 0.01643891, -0.015298853, -0.02190629, -0.04484231, -0.013570377
  , -0.024027044, 0.012810337, 0.031946164, 0.016917001, -0.020888818, 0.0041618273
  , -0.017811885, 0.004998483, -0.021783704, 0.01185416, 0.004597011, 0.048103124
  , 0.0022847145, 0.018400302, 0.023058608, -9.8835735e-05, -0.026576852, 0.014453003
  , 0.07551357, -0.0021115604, -0.03513342, -0.007128432, 0.009157246, 0.021035923
  , 0.009984708, 0.0019813117, 0.024027044, 0.012087074, 0.01814287, 0.024210924
  , 0.02934732, -0.0034109822, -0.03564829, 0.15828429, 0.012736785, -0.019993933
  , -0.017946731, -0.022028876, -0.0025666645, -0.0070364918, 0.06291163, 0.0018648541
  , 0.018130612, 0.034177244, 0.019319706, 0.0018878392, 0.018265458, 0.012920666
  , 0.015409181, -0.011327036, 0.0074594165, -0.012277084, -0.0043395786, -0.010806041
  , -0.0055777067, 0.016316324, -0.005387697, -0.029004075, -0.024909671, -0.026061988
  , 0.048568953, 0.014649142, -0.003778743, -0.006337746, -0.0009523475, 0.026625888
  , 0.013423272, 0.00939629, 0.0123016015, 0.005688035, -0.03581991, -0.026307162
  , -0.0053294683, 0.01447752, 0.004106663, 0.011811254, -0.009390161, -0.016340842
  , 0.011130896, 0.0140607245, 0.033319138, 0.042169914, -0.058547534, 0.023757353
  , -0.082231335, 0.01739509, -0.0026785252, 0.0017131527, 0.0016656504, -0.0031474202
  , 0.035868946, -0.00062902435, -0.001791302, -0.0074778046, 0.016880225, -0.009843733
  , 0.0015875011, -0.019687466, -0.016475687, 0.0151762655, 0.0007401188, 0.021281097
  , 0.014171053, -0.0070303623, 0.04587204, -0.02623361, 0.017640265, 0.016279548
  , -0.015262077, -0.033024926, 0.01109412, -0.0057952986, -0.013411013, -0.02679751
  , 0.020827524, -0.05011355, 0.017652523, -0.052123975, 0.0028378882, -0.021697892
  , 0.09806957, 0.00030972363, 0.005632871, 0.008066222, 0.012675492, 0.048838645
  , -0.014354933, 0.011982876, 0.04062532, -0.0074410285, -0.023083124, -0.0075145806
  , 0.017321538, -0.013950396, 0.017284762, 0.023499921, 0.003254684, -0.012185144
  , 0.0066135665, 0.011627373, 0.0068403524, 0.023242489, 0.051486526, -0.034177244
  , -0.004998483, -0.015507251, -0.010162459, -0.00931048, -0.015617579, -0.015323371
  , 0.04042918, -0.025228396, -0.017272502, -0.015507251, -0.035403114, 0.03648188
  , 0.0051180054, 0.016880225, -0.00950049, 0.0010151733, 0.013092288, -0.017358314
  , -0.019699724, 0.04087049, 0.0055899653, -0.014244605, 0.02669944, -0.0050996174
  , -0.01824094, 0.03412821, -0.007355218, -0.015494992, -0.007079397, 0.00996632
  , -0.0009446858, 0.0070242328, 0.003423241, 0.041875705, 0.017505419, -0.015114972
  , -0.034446936, 0.036579948, -0.016892483, 0.023990268, 0.0017392025, -0.015029161
  , -0.045724936, -0.009261445, -0.0042813495, 0.06698152, 0.0037910019, 0.020570092
  , -0.029298283, 0.015740165, -0.006883258, -0.024860635, -0.019442292, -0.00987438
  , -0.0449649, -0.008942719, 0.003478405, -0.004051499, -0.007011974, 0.0076616853
  , -0.008930461, -0.0067361537, -0.004624593, 0.016806673, -0.012724526, 0.005191558
  , 0.030818362, 0.014281381, -0.035967015, -0.014955609, -0.0027674006, 0.02266633
  , -0.016365359, 0.011008309, -0.019957157, -0.015213042, 0.07899504, -0.0040852106
  , -0.035206977, -0.005464314, -0.007165208, 0.010113425, -0.011296389, -0.01588727
  , 0.0045265234, -0.015384664, 0.032755237, -0.021489494, 0.0017575906, -0.007655556
  , 0.011829642, -0.0128838895, 0.017811885, 0.036334775, -0.0026754604, 0.041826673
  , -0.005605289, 0.015004644, 0.009561783, -0.013607153, 0.0021360777, 0.032559097
  , 0.0445481, -0.016659569, 0.016058892, -0.010566996, -0.0032271019, 0.008066222
  , 0.032411993, -0.043665476, 0.009604689, -0.017750593, -0.026307162, -0.033588827
  , -0.029764114, -0.0068587405, 0.009843733, 0.024027044, 0.054820888, -0.043886133
  , -0.012650974, -0.026184576, -0.0066809896, -0.014661401, -0.023009572, -0.042905435
  , 0.014220088, -0.01918486, -0.022457931, -0.0016058892, 0.014649142, -0.012264825
  , 0.0123016015, -0.01739509, -0.043297715, -0.003968753, -0.052614324, 0.026037471
  , 0.032730717, 0.0027995796, -0.006644213, 0.0039074593, 0.054477647, 0.043469336
  , 0.01729702, -0.014207829, 0.023107642, -0.024394805, -0.014808505, -0.0002627958
  , -0.026160058, 0.015225301, 0.005709488, -0.035672806, -0.02200436, 0.0003196838
  , -0.019123565, -0.009929544, -0.021452717, -0.016733121, -0.01504142, 0.010389245
  , 0.03564829, 0.040944044, -0.0040576286, -0.026258128, 0.016659569, 0.013717481
  , -0.011921582, -0.026111024, 0.011596726, -0.01983457, -0.013619412, -0.016978294
  , -0.027287858, -0.033221066, 0.022102429, 0.0404537, 0.024051562, -0.055262204
  , 0.0013905958, -0.008237843, -0.023156676, -0.005206881, 0.026405232, 0.009745663
  , -0.030377049, -0.051682662, -0.0075329687, -0.033343654, -0.008660769, 0.025130328
  , 0.00061983033, 0.0057309405, 0.02679751, 0.01766478, -0.010855076, -0.029567976
  , -0.006828094, 0.015678873, -0.015850494, 0.0013423272, 0.009341126, -0.035231493
  , -0.0020088938, 0.014551072, -0.0012312328, -0.004005529, 0.0027612713, -0.031039018
  , -0.01598534, 0.053251777, 0.006319358, 0.029371835, 0.011878677, 0.04118922
  , -0.020901076, -0.032191336, -0.0016656504, 0.040380146, 0.0016947647, -0.0038982653
  , 0.0023000378, 0.012197402, -0.024664497, -0.05236915, -0.028709866, 0.031553883
  , 0.030989984, -0.039816245, 0.013423272, 0.003187261, -0.01475947, 0.0020364758
  , 0.0025544057, -0.0046491106, -0.006846482, 0.020251365, -0.00075505907, 0.008470759
  , -0.0061937063, 0.0076126503, 0.029273767, -0.014771729, -0.054477647, 0.011259613
  , 0.008746579, -0.010646678, -0.03111257, -0.01278582, -0.011633502, 0.021967584
  , -0.0019675207, 0.021955324, -0.019515844, 0.021121733, -0.029788632, -0.0073123123
  , -0.0027352215, 0.008035575, -0.045259107, -0.009567913, 0.012614198, 0.015519509
  ]

referenceVectorResponse :: LBS.ByteString
referenceVectorResponse = Aeson.encode referenceVectors

newtype InputEnvelope = InputEnvelope { inputValues :: [T.Text] }

instance FromJSON InputEnvelope where
  parseJSON = withObject "InputEnvelope" $ \o -> InputEnvelope <$> o .: "inputs"

expectInputCount :: LBS.ByteString -> IO Int
expectInputCount body = length <$> expectInputValues body

expectInputValues :: LBS.ByteString -> IO [T.Text]
expectInputValues body = case Aeson.eitherDecode body :: Either String InputEnvelope of
  Left parseFailure -> expectationFailure parseFailure >> fail "unreachable"
  Right envelope -> pure envelope.inputValues

serveCompatible
  :: Wai.Request
  -> LBS.ByteString
  -> (Wai.Response -> IO Wai.ResponseReceived)
  -> IO Wai.ResponseReceived
serveCompatible request body respond
  | Wai.rawPathInfo request == "/info" =
      respond (Wai.responseLBS status200 [("Content-Type", "application/json")] validInfoResponse)
  | Wai.rawPathInfo request == "/embed" = do
      count <- expectInputCount body
      let responseBody = if count == 4 then referenceVectorResponse else vectorResponse count
      respond (Wai.responseLBS status200 [("Content-Type", "application/json")] responseBody)
  | otherwise = respond (Wai.responseLBS status400 [] "unexpected route")

vectorResponse :: Int -> LBS.ByteString
vectorResponse count = Aeson.encode (replicate count unitVector)

unitVector :: [Double]
unitVector = 1 : replicate (observationEmbeddingDimensions - 1) 0

contains :: LBS.ByteString -> LBS.ByteString -> Bool
contains haystack needle = LBS8.unpack needle `isInfixOf` LBS8.unpack haystack

withinTestTimeout :: Int -> IO result -> IO result
withinTestTimeout microseconds action = do
  result <- Timeout.timeout microseconds action
  case result of
    Just value -> pure value
    Nothing -> expectationFailure "HTTP test exceeded its deterministic timeout" >> fail "unreachable"

shouldBeCancelled :: Async result -> IO ()
shouldBeCancelled worker = waitCatch worker >>= \case
  Left exception ->
    (fromException exception :: Maybe AsyncCancelled) `shouldBe` Just AsyncCancelled
  Right _ -> expectationFailure "expected asynchronous cancellation"

withTestPolicy :: (SessionPolicy -> IO result) -> IO result
withTestPolicy action = do
  path <- testHelperExecutable
  action SessionPolicy
    { helperExecutable = path
    , afterSpawnBeforeReady = const (pure ())
    , afterSuccessfulResponse = const (pure ())
    }

-- The test executable and its build-tool dependency use the same Stack dist
-- directory. An explicit path is retained for pinned external runtime checks.
testHelperExecutable :: IO FilePath
testHelperExecutable = do
  configured <- lookupEnv "HMEM_TEST_HTTP_HELPER"
  candidate <- case configured of
    Just value | not (null value) -> pure value
    _ -> do
      executable <- canonicalizePath =<< getExecutablePath
      let testBuild = takeDirectory executable
          build = takeDirectory testBuild
          distHash = takeFileName (takeDirectory build)
          dist = takeDirectory (takeDirectory build)
          stackWork = takeDirectory dist
          serverPackage = takeDirectory stackWork
          repo = takeDirectory serverPackage
          suffix = takeExtension executable
      unless (takeFileName executable == "hmem-server-test" <> suffix
          && takeFileName testBuild == "hmem-server-test"
          && takeFileName build == "build"
          && takeFileName dist == "dist"
          && takeFileName stackWork == ".stack-work"
          && takeFileName serverPackage == "hmem-server") $
        expectationFailure "test executable is outside its expected Stack build layout"
      pure $ repo </> "hmem-embedding-http" </> ".stack-work" </> "dist"
        </> distHash </> "build" </> "hmem-embedding-http-helper"
        </> ("hmem-embedding-http-helper" <> suffix)
  exists <- doesFileExist candidate
  unless exists $ expectationFailure "the built HTTP helper is unavailable"
  canonicalizePath candidate

withTransport
  :: Wai.Application
  -> EmbeddingProviderConfig
  -> ((EmbeddingRequest -> IO (Either EmbeddingFailure EmbeddingBatch)) -> IO result)
  -> IO result
withTransport app config action =
  testWithApplication (pure app) $ \port -> do
    withTestPolicy $ \policy -> do
      let endpointValue = "http://127.0.0.1:" <> T.pack (show port)
      action (httpEmbeddingTransport policy config endpointValue)

withEndpoint
  :: Wai.Application
  -> (SessionPolicy -> T.Text -> IO result)
  -> IO result
withEndpoint app action = testWithApplication (pure app) $ \port ->
  withTestPolicy $ \policy ->
    action policy ("http://127.0.0.1:" <> T.pack (show port))

expectRight :: Show left => Either left right -> IO right
expectRight = \case
  Left failure -> expectationFailure (show failure) >> fail "unreachable"
  Right value -> pure value

waitUntilBlockedOnAdmission :: Async value -> IO ()
waitUntilBlockedOnAdmission worker = do
  status <- threadStatus (asyncThreadId worker)
  case status of
    ThreadBlocked BlockedOnMVar -> pure ()
    ThreadBlocked BlockedOnSTM -> pure ()
    ThreadFinished -> expectationFailure "queued availability call finished before cancellation"
    ThreadDied -> expectationFailure "queued availability call died before cancellation"
    _ -> threadDelay 1000 >> waitUntilBlockedOnAdmission worker
