module HMem.Server.ObservationDiscoverySpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (withAsync, wait)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (throwIO)
import Data.Aeson (toJSON, object, (.=))
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.List (isInfixOf)
import Data.Text qualified as T
import Test.Hspec

import HMem.Config (managedTeiSpaceFingerprint)
import HMem.DB.Embedding qualified as Embedding
import HMem.DB.Observation qualified as Observation
import HMem.DB.TestHarness (TestEnv(..), createTestWorkspace, withTestEnv)
import HMem.Types
import HMem.Server.Embedding.GteQwen2 (gteQwen2QueryPrefix)
import HMem.Server.Embedding.Provider
import HMem.Server.ObservationDiscovery

spec :: Spec
spec = describe "Observation discovery query preparation" $ do
  it "keeps disabled discovery inert even with a published provider" $ do
    service <- newObservationDiscovery False
    _ <- publishDiscoveryProvider service forbiddenProvider
    prepareDiscoveryQuery service forbiddenStorage "query" `shouldReturn` DiscoveryFts DiscoveryDisabled
    discoveryCapability service forbiddenStorage `shouldReturn` DiscoveryCapability (Just DiscoveryDisabled)

  it "does not probe or start a provider when no validated generation is published" $ do
    service <- newObservationDiscovery True
    prepareDiscoveryQuery service forbiddenStorage "query" `shouldReturn` DiscoveryFts DiscoveryProviderUnavailable

  it "prepares a query once and normalizes its finite vector in the exact space" $ do
    requests <- newIORef []
    service <- newObservationDiscovery True
    _ <- publishDiscoveryProvider service goodProvider
      { embed = \request -> modifyIORef' requests (<> [request]) >> pure (Right (EmbeddingBatch [map (* 2) unitVector] managedTeiSpaceFingerprint)) }
    prepareDiscoveryQuery service readyStorage "body" `shouldReturn` DiscoveryHybrid unitVector managedTeiSpaceFingerprint
    readIORef requests `shouldReturn` [EmbeddingRequest [EmbeddingInput EmbeddingQuery (gteQwen2QueryPrefix <> "body")]]

  it "does not run provider work without storage and compatible vectors" $ do
    service <- newObservationDiscovery True
    _ <- publishDiscoveryProvider service forbiddenProvider
    prepareDiscoveryQuery service (pure Nothing) "query" `shouldReturn` DiscoveryFts DiscoveryStorageUnavailable
    prepareDiscoveryQuery service (pure (Just False)) "query" `shouldReturn` DiscoveryFts DiscoveryNoCompatibleVectors

  it "reports capability without producing a query vector or exposing configuration" $ do
    service <- newObservationDiscovery True
    _ <- publishDiscoveryProvider service goodProvider { embed = \_ -> expectationFailure "unexpected embedding" >> pure unavailable }
    result <- discoveryCapability service readyStorage
    toJSON result `shouldBe` object ["mode" .= ("hybrid" :: T.Text), "reason" .= (Nothing :: Maybe T.Text)]

  it "keeps empty and oversized queries in FTS without provider work" $ do
    service <- newObservationDiscovery True
    _ <- publishDiscoveryProvider service forbiddenProvider
    prepareDiscoveryQuery service forbiddenStorage " \n\t" `shouldReturn` DiscoveryFts DiscoveryEmptyQuery
    prepareDiscoveryQuery service forbiddenStorage (T.replicate 2048 "x") `shouldReturn` DiscoveryFts DiscoveryInputTooLong

  it "rejects mismatched spaces, malformed dimensions, nonfinite and zero vectors" $ do
    service <- newObservationDiscovery True
    mapM_ (\batch -> do
      _ <- publishDiscoveryProvider service goodProvider { embed = \_ -> pure (Right batch) }
      prepareDiscoveryQuery service readyStorage "query" `shouldReturn` DiscoveryFts DiscoveryInvalidResponse)
      [ EmbeddingBatch [unitVector] "foreign-space"
      , EmbeddingBatch [[1]] managedTeiSpaceFingerprint
      , EmbeddingBatch [(0 / 0) : drop 1 unitVector] managedTeiSpaceFingerprint
      , EmbeddingBatch [replicate 1536 0] managedTeiSpaceFingerprint
      , EmbeddingBatch [] managedTeiSpaceFingerprint
      ]

  it "uses fixed secret-safe failure codes and redacted Show output" $ do
    service <- newObservationDiscovery True
    _ <- publishDiscoveryProvider service goodProvider { embed = \_ -> throwIO (userError "secret-endpoint-input-sentinel") }
    result <- prepareDiscoveryQuery service readyStorage "secret-endpoint-input-sentinel"
    result `shouldBe` DiscoveryFts DiscoveryProviderUnavailable
    show result `shouldNotSatisfy` isInfixOf "sentinel"
    show (DiscoveryHybrid unitVector managedTeiSpaceFingerprint) `shouldBe` "DiscoveryHybrid <redacted>"

  it "preserves structured provider failure reasons" $ do
    service <- newObservationDiscovery True
    mapM_ (\(code, expected) -> do
      _ <- publishDiscoveryProvider service goodProvider { embed = \_ -> pure (Left (EmbeddingFailure code True)) }
      prepareDiscoveryQuery service readyStorage "query" `shouldReturn` DiscoveryFts expected)
      [(ProviderTimedOut, DiscoveryTimedOut), (ProviderProtocolError, DiscoveryInvalidResponse), (ProviderCancelled, DiscoveryRetired)]

  it "bounds the complete storage/availability/query operation" $ do
    service <- newObservationDiscovery True
    _ <- publishDiscoveryProvider service goodProvider { embed = \_ -> threadDelay (discoveryTimeoutMicros * 5) >> pure unavailable }
    prepareDiscoveryQuery service readyStorage "query" `shouldReturn` DiscoveryFts DiscoveryTimedOut
    -- Timeout cancellation releases the same caller: the next generation works.
    _ <- publishDiscoveryProvider service goodProvider
    prepareDiscoveryQuery service readyStorage "query" `shouldReturn` DiscoveryHybrid unitVector managedTeiSpaceFingerprint

  it "retires an in-flight query and prevents an old withdrawal clearing its replacement" $ do
    service <- newObservationDiscovery True
    started <- newEmptyMVar
    release <- newEmptyMVar
    withdraw <- publishDiscoveryProvider service goodProvider
      { embed = \_ -> putMVar started () >> takeMVar release >> pure (Right validBatch) }
    withAsync (prepareDiscoveryQuery service readyStorage "query") $ \query -> do
      takeMVar started
      withdraw
      _ <- publishDiscoveryProvider service goodProvider
      withdraw
      putMVar release ()
      wait query `shouldReturn` DiscoveryFts DiscoveryRetired
    prepareDiscoveryQuery service readyStorage "new query" `shouldReturn` DiscoveryHybrid unitVector managedTeiSpaceFingerprint

  it "probes optional storage safely and admits only workspace-local exact-space vectors" $
    withTestEnv $ \env -> do
      workspace <- createTestWorkspace env "discovery-vector-space"
      foreignWorkspace <- createTestWorkspace env "discovery-foreign-space"
      let Just space = parseEmbeddingSpaceFingerprint managedTeiSpaceFingerprint
          probe = Observation.discoveryVectorAvailability env.pool workspace.id space
          create workspaceId = Observation.createObservation env.pool
            (CreateObservation workspaceId [ObservationSubject SubjectFile "src/Main.hs"] (T.replicate 40 "a") "content")
      probe >>= \case
        Nothing -> probe `shouldReturn` Nothing
        Just available -> do
          available `shouldBe` False
          foreignObservation <- create foreignWorkspace.id
          Embedding.setObservationEmbeddingInSpace env.pool foreignWorkspace.id foreignObservation.id space unitVector
          probe `shouldReturn` Just False
          local <- create workspace.id
          Embedding.setObservationEmbeddingInSpace env.pool workspace.id local.id legacyManualEmbeddingSpace unitVector
          probe `shouldReturn` Just False
          Embedding.setObservationEmbeddingInSpace env.pool workspace.id local.id space unitVector
          probe `shouldReturn` Just True

unitVector :: [Double]
unitVector = 1 : replicate 1535 0

validBatch :: EmbeddingBatch
validBatch = EmbeddingBatch [unitVector] managedTeiSpaceFingerprint

unavailable :: Either EmbeddingFailure EmbeddingBatch
unavailable = Left (EmbeddingFailure ProviderUnavailable True)

goodProvider :: EmbeddingProvider
goodProvider = EmbeddingProvider (pure EmbeddingAvailable) (\_ -> pure (Right validBatch))

forbiddenProvider :: EmbeddingProvider
forbiddenProvider = EmbeddingProvider
  (expectationFailure "unexpected provider availability" >> pure EmbeddingDisabled)
  (\_ -> expectationFailure "unexpected provider embedding" >> pure unavailable)

readyStorage :: IO (Maybe Bool)
readyStorage = pure (Just True)

forbiddenStorage :: IO (Maybe Bool)
forbiddenStorage = expectationFailure "unexpected storage probe" >> pure Nothing
