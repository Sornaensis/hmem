module HMem.Server.Embedding.HttpSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, asyncThreadId, cancel, wait)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (AsyncException(..), SomeException, finally, fromException, throwTo, try)
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.ByteString.Builder (byteString)
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.List (isInfixOf)
import Data.Text qualified as T
import Network.HTTP.Client
  ( ManagerSettings(..)
  , defaultManagerSettings
  , newManager
  , responseTimeoutMicro
  )
import Network.HTTP.Types (mkStatus, status200, status302, status400, status408, status429, status500)
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp (testWithApplication)
import System.Timeout qualified as Timeout
import Test.Hspec

import HMem.Config
import HMem.Server.Embedding.Http
import HMem.Server.Embedding.Provider
import HMem.Types (observationEmbeddingDimensions)

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
    embeddingEndpointPath `shouldBe` "/embed"

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
    decodeEmbeddingResponse 1 valid `shouldBe` Right [replicate observationEmbeddingDimensions 0.25]
    decodeEmbeddingResponse 2 valid
      `shouldBe` Left (EmbeddingFailure ProviderProtocolError False)
    decodeEmbeddingResponse 1 (Aeson.encode [replicate 2 (0.25 :: Double)])
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
      result `shouldBe` Right (EmbeddingBatch [replicate observationEmbeddingDimensions 0.25] managedTeiSpaceFingerprint)
    recorded <- readIORef seen
    recorded `shouldSatisfy` \case
      Just (method, path, body) -> method == "POST" && path == "/embed" && contains body "\"inputs\":[\"document\"]"
      Nothing -> False

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

  it "retries only 408, 429, and 500 through 599 exactly to the configured bound" $ do
    mapM_ (\retryStatus -> do
      calls <- newIORef (0 :: Int)
      let app _ respond = do
            modifyIORef' calls (+ 1)
            current <- readIORef calls
            respond (Wai.responseLBS (if current < 3 then retryStatus else status200) [] (vectorResponse 1))
      withTransport app (httpConfig 2 1000) $ \transport -> do
        result <- transport requestOne
        result `shouldBe` Right (EmbeddingBatch [replicate observationEmbeddingDimensions 0.25] managedTeiSpaceFingerprint)
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
      [status400, mkStatus 600 "synthetic-non-http"]

  it "retries connection failures exactly to the configured bound" $ do
    calls <- newIORef (0 :: Int)
    manager <- newManager defaultManagerSettings
      { managerRawConnection = pure $ \_ _ _ -> do
          modifyIORef' calls (+ 1)
          ioError (userError "connection closed")
      }
    result <- httpEmbeddingTransport manager (httpConfig 2 1000) "http://127.0.0.1:8080" requestOne
    result `shouldBe` Left (EmbeddingFailure ProviderUnavailable True)
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
    manager <- newManager defaultManagerSettings
    let secretEndpoint = "http://127.0.0.1:1/?secret=do-not-log"
    result <- httpEmbeddingTransport manager (httpConfig 0 250) secretEndpoint requestOne
    result `shouldBe` Left (EmbeddingFailure ProviderConfigurationError False)
    show result `shouldNotSatisfy` isInfixOf "do-not-log"

  it "returns a structured timeout and releases the request" $ do
    let app _ respond = do
          threadDelay 250000
          respond (Wai.responseLBS status200 [] (vectorResponse 1))
    withTransportWithSettings app (httpConfig 0 100)
      (defaultManagerSettings { managerResponseTimeout = responseTimeoutMicro 20000 }) $ \transport ->
      transport requestOne `shouldReturn` Left (EmbeddingFailure ProviderTimedOut True)

  it "returns structured cancellation rather than an untyped transport exception" $ do
    let app _ respond = do
          threadDelay 1000000
          respond (Wai.responseLBS status200 [] (vectorResponse 1))
    withTransport app (httpConfig 0 1000) $ \transport -> do
      worker <- async (transport requestOne)
      threadDelay 20000
      cancel worker
      wait worker `shouldReturn` Left (EmbeddingFailure ProviderCancelled False)

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

vectorResponse :: Int -> LBS.ByteString
vectorResponse count = Aeson.encode (replicate count (replicate observationEmbeddingDimensions (0.25 :: Double)))

contains :: LBS.ByteString -> LBS.ByteString -> Bool
contains haystack needle = LBS8.unpack needle `isInfixOf` LBS8.unpack haystack

withTransport
  :: Wai.Application
  -> EmbeddingProviderConfig
  -> ((EmbeddingRequest -> IO (Either EmbeddingFailure EmbeddingBatch)) -> IO result)
  -> IO result
withTransport app config action =
  withTransportWithSettings app config defaultManagerSettings action

withTransportWithSettings
  :: Wai.Application
  -> EmbeddingProviderConfig
  -> ManagerSettings
  -> ((EmbeddingRequest -> IO (Either EmbeddingFailure EmbeddingBatch)) -> IO result)
  -> IO result
withTransportWithSettings app config settings action =
  testWithApplication (pure app) $ \port -> do
    manager <- newManager settings
    let endpointValue = "http://127.0.0.1:" <> T.pack (show port)
    action (httpEmbeddingTransport manager config endpointValue)
