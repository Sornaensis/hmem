module HMem.Server.Embedding.ProviderSpec (spec) where

import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.List (isInfixOf)
import Test.Hspec

import HMem.Config
import HMem.Server.Embedding.Provider

spec :: Spec
spec = describe "embedding provider selection" $ do
  it "keeps disabled mode inert and does not invoke a transport" $ do
    calls <- newIORef (0 :: Int)
    let transport _ _ = modifyIORef' calls (+ 1) >> pure unavailable
        unavailable = Left (EmbeddingFailure ProviderUnavailable True)
    provider <- expectRight $ selectEmbeddingProvider defaultConfig.embeddingProvider Nothing transport
    provider.availability `shouldReturn` EmbeddingDisabled
    provider.embed (EmbeddingRequest [EmbeddingInput EmbeddingDocument "never sent"])
      `shouldReturn` Left (EmbeddingFailure ProviderDisabled False)
    readIORef calls `shouldReturn` 0

  it "routes HTTP mode to its fixed configured endpoint" $ do
    endpointSeen <- newIORef Nothing
    let config = defaultConfig.embeddingProvider
          { mode = EmbeddingProviderHttp, endpoint = Just "https://gpu.example" }
        transport endpointValue _ = do
          modifyIORef' endpointSeen (const (Just endpointValue))
          pure (Right (EmbeddingBatch [[1.0]] config.spaceFingerprint))
    provider <- expectRight $ selectEmbeddingProvider config Nothing transport
    provider.embed (EmbeddingRequest [EmbeddingInput EmbeddingQuery "query"])
      `shouldReturn` Right (EmbeddingBatch [[1.0]] config.spaceFingerprint)
    readIORef endpointSeen `shouldReturn` Just "https://gpu.example/embed"

  it "requires an exact loopback authority for managed-tei" $ do
    let config :: EmbeddingProviderConfig
        config = EmbeddingProviderConfig
          { mode = EmbeddingProviderManagedTei
          , endpoint = Nothing
          , batchSize = 32
          , timeoutMs = 30000
          , retryAttempts = 0
          , spaceFingerprint = managedTeiSpaceFingerprint
          }
        transport _ _ = pure unavailable
        unavailable = Left (EmbeddingFailure ProviderUnavailable True)
    mapM_ (\endpointValue -> case selectEmbeddingProvider config (Just endpointValue) transport of
      Left failure -> failure `shouldBe` EmbeddingFailure ProviderConfigurationError False
      Right _ -> expectationFailure "unexpected managed provider")
      [ "https://remote.example"
      , "http://localhost.evil:8080"
      , "http://127.evil:8080"
      , "http://user@127.0.0.1:8080"
      , "http://:8080"
      , "http://127.0.0.1.evil:8080"
      ]
    mapM_ (\endpointValue -> do
      provider <- expectRight $ selectEmbeddingProvider config (Just endpointValue) transport
      provider.availability `shouldReturn` EmbeddingAvailable)
      [ "http://127.0.0.1:8080"
      , "http://localhost:8080"
      , "http://[::1]:8080"
      ]

  it "rejects arbitrary and repeated routes during HTTP and managed selection" $ do
    let httpConfig = defaultConfig.embeddingProvider
          { mode = EmbeddingProviderHttp, endpoint = Just "https://gpu.example/admin" }
        managedConfig = defaultConfig.embeddingProvider
          { mode = EmbeddingProviderManagedTei, endpoint = Nothing }
        transport _ _ = pure unavailable
        unavailable = Left (EmbeddingFailure ProviderUnavailable True)
        rejected result = case result of
          Left failure -> failure `shouldBe` EmbeddingFailure ProviderConfigurationError False
          Right _ -> expectationFailure "unexpected provider"
    rejected (selectEmbeddingProvider httpConfig Nothing transport)
    mapM_ (\endpointValue -> rejected (selectEmbeddingProvider managedConfig (Just endpointValue) transport))
      [ "http://127.0.0.1:8080/admin"
      , "http://127.0.0.1:8080/embed/embed"
      ]

  it "redacts input text and vectors from provider Show instances" $ do
    let request = EmbeddingRequest [EmbeddingInput EmbeddingDocument "input-sentinel"]
        batch = EmbeddingBatch [[42.125]] "space-sentinel"
        rendered = show request <> show batch
    rendered `shouldNotSatisfy` isInfixOf "input-sentinel"
    rendered `shouldNotSatisfy` isInfixOf "42.125"

  it "rejects empty and oversized batches before calling a transport" $ do
    calls <- newIORef (0 :: Int)
    let config = defaultConfig.embeddingProvider
          { mode = EmbeddingProviderHttp, endpoint = Just "https://gpu.example", batchSize = 1 }
        transport _ _ = modifyIORef' calls (+ 1) >> pure unavailable
        unavailable = Left (EmbeddingFailure ProviderUnavailable True)
    provider <- expectRight $ selectEmbeddingProvider config Nothing transport
    provider.embed (EmbeddingRequest [])
      `shouldReturn` Left (EmbeddingFailure ProviderConfigurationError False)
    provider.embed (EmbeddingRequest
      [ EmbeddingInput EmbeddingDocument "one"
      , EmbeddingInput EmbeddingDocument "two"
      ]) `shouldReturn` Left (EmbeddingFailure ProviderConfigurationError False)
    readIORef calls `shouldReturn` 0

expectRight :: Show left => Either left right -> IO right
expectRight = \case
  Left err -> expectationFailure (show err) >> fail "unreachable"
  Right value -> pure value
