module HMem.Server.ObservationDiscoverySpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (withAsync, wait, cancel)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (throwIO)
import Control.Monad (forM_)
import Data.Aeson (toJSON, object, (.=))
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.List (isInfixOf)
import Data.Text qualified as T
import Data.Time (UTCTime(..), addUTCTime, fromGregorian)
import Data.UUID qualified as UUID
import Data.Vector.Unboxed qualified as VU
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
  it "supersedes pending generations before the client knows a context token and keeps rapid edits in one slot" $ do
    service <- newObservationDiscovery False
    started <- newEmptyMVar
    release <- newEmptyMVar
    let session = UUID.fromWords 1 2 3 4
        create generation prepare = createDiscoveryContextInSession service "actor" UUID.nil contextFilters Nothing (Just (session,generation)) prepare
    withAsync (create 1 (putMVar started () >> takeMVar release >> pure (DiscoveryFts DiscoveryDisabled))) $ \pending -> do
      takeMVar started
      latest <- create 2 (pure (DiscoveryFts DiscoveryTimedOut)) >>= expectContext
      putMVar release ()
      wait pending `shouldReturn` Left DiscoveryContextUnavailable
      lookupDiscoveryContext service "actor" UUID.nil latest.token `shouldReturn` Right latest
    calls <- newIORef (0 :: Int)
    create 1 (modifyIORef' calls (+ 1) >> pure (DiscoveryFts DiscoveryDisabled)) `shouldReturn` Left DiscoveryContextUnavailable
    readIORef calls `shouldReturn` 0
    mapM_ (\generation -> create generation (pure (DiscoveryFts DiscoveryDisabled)) >>= expectContext) [3..30]
    -- Thirty edits occupy one slot; seven independent searches still fit.
    mapM_ (\_ -> createDiscoveryContext service "actor" UUID.nil contextFilters Nothing (pure (DiscoveryFts DiscoveryDisabled)) >>= expectContext) [1..7 :: Int]

  it "fences late requests after a keyed preparation is cancelled" $ do
    service <- newObservationDiscovery False
    started <- newEmptyMVar
    release <- newEmptyMVar
    let session = UUID.fromWords 5 6 7 8
        create generation prepare = createDiscoveryContextInSession service "actor" UUID.nil contextFilters Nothing (Just (session,generation)) prepare
    withAsync (create 2 (putMVar started () >> takeMVar release >> pure (DiscoveryFts DiscoveryDisabled))) $ \pending -> do
      takeMVar started
      cancel pending
    create 1 (pure (DiscoveryFts DiscoveryDisabled)) `shouldReturn` Left DiscoveryContextUnavailable
    _ <- create 3 (pure (DiscoveryFts DiscoveryDisabled)) >>= expectContext
    pure ()

  it "retains keyed generation fencing after explicit retirement for lost storage" $ do
    service <- newObservationDiscovery False
    let session = UUID.fromWords 9 10 11 12
        create generation = createDiscoveryContextInSession service "actor" UUID.nil contextFilters Nothing (Just (session,generation)) (pure (DiscoveryHybrid (VU.fromList unitVector) managedTeiSpaceFingerprint))
    current <- create 2 >>= expectContext
    retireDiscoveryContext service "actor" UUID.nil current.token
    lookupDiscoveryContext service "actor" UUID.nil current.token `shouldReturn` Left DiscoveryContextUnavailable
    create 1 `shouldReturn` Left DiscoveryContextUnavailable
    create 2 `shouldReturn` Left DiscoveryContextUnavailable
    next <- create 3 >>= expectContext
    lookupDiscoveryContext service "actor" UUID.nil next.token `shouldReturn` Right next

  it "rejects mixed-session replacements without erasing ownership or generation fences beyond eight contexts" $ do
    service <- newObservationDiscovery False
    calls <- newIORef (0 :: Int)
    let sessionA = UUID.fromWords 13 14 15 16
        sessionB = UUID.fromWords 17 18 19 20
        prepared = pure (DiscoveryFts DiscoveryDisabled)
        forbidden = modifyIORef' calls (+ 1) >> prepared
        createA generation predecessor = createDiscoveryContextInSession service "actor" UUID.nil contextFilters predecessor (Just (sessionA,generation)) prepared
    first <- createA 2 Nothing >>= expectContext
    mapM_ (\_ -> createDiscoveryContext service "actor" UUID.nil contextFilters Nothing prepared >>= expectContext) [1..7 :: Int]
    createDiscoveryContext service "actor" UUID.nil contextFilters (Just first.token) forbidden `shouldReturn` Left DiscoveryContextUnavailable
    createDiscoveryContextInSession service "actor" UUID.nil contextFilters (Just first.token) (Just (sessionB,1)) forbidden `shouldReturn` Left DiscoveryContextUnavailable
    createDiscoveryContextInSession service "foreign-actor" UUID.nil contextFilters (Just first.token) (Just (sessionA,3)) forbidden `shouldReturn` Left DiscoveryContextUnavailable
    readIORef calls `shouldReturn` 0
    lookupDiscoveryContext service "actor" UUID.nil first.token `shouldReturn` Right first
    createA 1 Nothing `shouldReturn` Left DiscoveryContextUnavailable
    replacement <- createA 3 (Just first.token) >>= expectContext
    lookupDiscoveryContext service "actor" UUID.nil replacement.token `shouldReturn` Right replacement
    -- A ninth context of the audience in another session is created.
    _ <- createDiscoveryContextInSession service "actor" UUID.nil contextFilters Nothing (Just (sessionB,1)) forbidden >>= expectContext
    readIORef calls `shouldReturn` 1

  it "pins complete filters and retrieval mode across provider changes and rejects foreign context audiences" $ do
    service <- newObservationDiscovery False
    let workspace = UUID.nil
        filters = ObservationDiscoveryFilter (Just "src") (Just "needle") Nothing Nothing Nothing Nothing Nothing
        prepared = DiscoveryFts DiscoveryTimedOut
    created <- createDiscoveryContext service "actor-a" workspace filters Nothing (pure prepared) >>= expectContext
    _ <- publishDiscoveryProvider service goodProvider
    looked <- lookupDiscoveryContext service "actor-a" workspace created.token >>= expectContext
    (looked.filterInputs,looked.preparedQuery) `shouldBe` (filters,Just prepared)
    lookupDiscoveryContext service "actor-b" workspace created.token `shouldReturn` Left DiscoveryContextUnavailable
    let foreignWorkspace = UUID.fromWords 1 2 3 4
    lookupDiscoveryContext service "actor-a" foreignWorkspace created.token `shouldReturn` Left DiscoveryContextUnavailable

  it "expires contexts at an absolute five-minute boundary without refreshing on use" $ do
    now <- newIORef (UTCTime (fromGregorian 2026 10 20) 0)
    service <- newObservationDiscoveryWithClock False (readIORef now)
    created <- createDiscoveryContext service "actor" UUID.nil contextFilters Nothing (pure (DiscoveryFts DiscoveryDisabled)) >>= expectContext
    modifyIORef' now (addUTCTime 299)
    lookupDiscoveryContext service "actor" UUID.nil created.token `shouldReturn` Right created
    modifyIORef' now (addUTCTime 1)
    lookupDiscoveryContext service "actor" UUID.nil created.token `shouldReturn` Left DiscoveryContextUnavailable

  it "creates a ninth context for one audience and a 65th globally without refusing" $ do
    service <- newObservationDiscovery False
    calls <- newIORef (0 :: Int)
    let prepare = modifyIORef' calls (+ 1) >> pure (DiscoveryFts DiscoveryDisabled)
        create actor = createDiscoveryContext service actor UUID.nil contextFilters Nothing prepare
    firsts <- mapM (\_ -> create "first" >>= expectContext) [1..9 :: Int]
    mapM_ (\n -> mapM_ (\_ -> create (T.pack (show n)) >>= expectContext) [1..8 :: Int]) [1..7 :: Int]
    _ <- create "sixty-fifth" >>= expectContext
    readIORef calls `shouldReturn` 66
    forM_ firsts $ \context -> lookupDiscoveryContext service "first" UUID.nil context.token `shouldReturn` Right context

  it "evicts the earliest-expiring context beyond the memory backstop instead of refusing" $ do
    now <- newIORef (UTCTime (fromGregorian 2026 10 20) 0)
    service <- newObservationDiscoveryWithClockAndBound False (readIORef now) 3
    let create actor = createDiscoveryContext service actor UUID.nil contextFilters Nothing (pure (DiscoveryFts DiscoveryDisabled))
    earliest <- create "a" >>= expectContext
    modifyIORef' now (addUTCTime 1)
    kept <- mapM (\actor -> create actor >>= expectContext) ["b", "c"]
    modifyIORef' now (addUTCTime 1)
    newest <- create "d" >>= expectContext
    -- The evicted context behaves as expired: restart is required.
    lookupDiscoveryContext service "a" UUID.nil earliest.token `shouldReturn` Left DiscoveryContextUnavailable
    forM_ (zip ["b", "c", "d"] (kept <> [newest])) $ \(actor, context) ->
      lookupDiscoveryContext service actor UUID.nil context.token `shouldReturn` Right context
    discoveryContextBound `shouldBe` 8192

  it "evicts a pending reservation at the bound so its creator must restart" $ do
    now <- newIORef (UTCTime (fromGregorian 2026 10 20) 0)
    service <- newObservationDiscoveryWithClockAndBound False (readIORef now) 2
    started <- newEmptyMVar
    release <- newEmptyMVar
    let prepared = pure (DiscoveryFts DiscoveryDisabled)
        create actor = createDiscoveryContext service actor UUID.nil contextFilters Nothing prepared
    withAsync (createDiscoveryContext service "slow" UUID.nil contextFilters Nothing (putMVar started () >> takeMVar release >> prepared)) $ \pending -> do
      takeMVar started
      modifyIORef' now (addUTCTime 1)
      kept <- mapM (\actor -> create actor >>= expectContext) ["b", "c"]
      discoveryContextCount service `shouldReturn` 2
      putMVar release ()
      -- The evicted in-flight reservation returns discovery_restart_required.
      wait pending `shouldReturn` Left DiscoveryContextUnavailable
      forM_ (zip ["b", "c"] kept) $ \(actor, context) ->
        lookupDiscoveryContext service actor UUID.nil context.token `shouldReturn` Right context

  it "expires and evicts through its expiry index across thousands of contexts" $ do
    now <- newIORef (UTCTime (fromGregorian 2026 10 20) 0)
    service <- newObservationDiscoveryWithClockAndBound False (readIORef now) 3000
    let create actor = createDiscoveryContext service actor UUID.nil contextFilters Nothing (pure (DiscoveryFts DiscoveryDisabled))
    first <- create "first" >>= expectContext
    modifyIORef' now (addUTCTime 1)
    -- Distinct creation times make expiry order equal creation order.
    contexts <- mapM (\index -> modifyIORef' now (addUTCTime 0.001) >> create (T.pack (show index)) >>= expectContext) [1..3999 :: Int]
    discoveryContextCount service `shouldReturn` 3000
    lookupDiscoveryContext service "first" UUID.nil first.token `shouldReturn` Left DiscoveryContextUnavailable
    lookupDiscoveryContext service "999" UUID.nil (contexts !! 998).token `shouldReturn` Left DiscoveryContextUnavailable
    lookupDiscoveryContext service "1000" UUID.nil (contexts !! 999).token `shouldReturn` Right (contexts !! 999)
    modifyIORef' now (addUTCTime 300)
    lookupDiscoveryContext service "3999" UUID.nil (last contexts).token `shouldReturn` Left DiscoveryContextUnavailable
    discoveryContextCount service `shouldReturn` 0

  it "atomically retires a predecessor and permits its replacement" $ do
    service <- newObservationDiscovery False
    first <- createDiscoveryContext service "actor" UUID.nil contextFilters Nothing (pure (DiscoveryFts DiscoveryDisabled)) >>= expectContext
    mapM_ (\_ -> createDiscoveryContext service "actor" UUID.nil contextFilters Nothing (pure (DiscoveryFts DiscoveryDisabled)) >>= expectContext) [1..7 :: Int]
    replacement <- createDiscoveryContext service "actor" UUID.nil contextFilters (Just first.token) (pure (DiscoveryFts DiscoveryNoCompatibleVectors)) >>= expectContext
    lookupDiscoveryContext service "actor" UUID.nil first.token `shouldReturn` Left DiscoveryContextUnavailable
    lookupDiscoveryContext service "actor" UUID.nil replacement.token `shouldReturn` Right replacement

  it "admits contexts beside pending preparation and releases cancelled reservations" $ do
    service <- newObservationDiscovery False
    started <- newEmptyMVar
    blocked <- newEmptyMVar
    let prepare = putMVar started () >> takeMVar blocked >> pure (DiscoveryFts DiscoveryDisabled)
    withAsync (createDiscoveryContext service "actor" UUID.nil contextFilters Nothing prepare) $ \pending -> do
      takeMVar started
      mapM_ (\_ -> createDiscoveryContext service "actor" UUID.nil contextFilters Nothing (pure (DiscoveryFts DiscoveryDisabled)) >>= expectContext) [1..8 :: Int]
      cancel pending
    _ <- createDiscoveryContext service "actor" UUID.nil contextFilters Nothing (pure (DiscoveryFts DiscoveryDisabled)) >>= expectContext
    pure ()

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
    prepareDiscoveryQuery service readyStorage "body" `shouldReturn` DiscoveryHybrid (VU.fromList unitVector) managedTeiSpaceFingerprint
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
    show (DiscoveryHybrid (VU.fromList unitVector) managedTeiSpaceFingerprint) `shouldBe` "DiscoveryHybrid <redacted>"

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
    prepareDiscoveryQuery service readyStorage "query" `shouldReturn` DiscoveryHybrid (VU.fromList unitVector) managedTeiSpaceFingerprint

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
    prepareDiscoveryQuery service readyStorage "new query" `shouldReturn` DiscoveryHybrid (VU.fromList unitVector) managedTeiSpaceFingerprint

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

contextFilters :: ObservationDiscoveryFilter
contextFilters = ObservationDiscoveryFilter Nothing Nothing Nothing Nothing Nothing Nothing Nothing

expectContext :: Either DiscoveryContextError StoredDiscoveryContext -> IO StoredDiscoveryContext
expectContext = either (\failure -> expectationFailure (show failure) >> fail "context unavailable") pure
