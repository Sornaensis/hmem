module HMem.Server.SessionPrefixSpec (spec) where

import Control.Concurrent (getNumCapabilities, threadDelay, newEmptyMVar, readMVar, putMVar, takeMVar, tryPutMVar)
import Control.Exception (SomeException, bracket, finally, throwIO, try, uninterruptibleMask_)
import Control.Concurrent.Async (withAsync, wait, poll)
import Control.Monad (forM, forM_, replicateM, void, when)
import Data.ByteString qualified as BS
import Data.IORef
import Data.List (nub,sort)
import Data.Map.Strict qualified as Map
import Data.Time
import Data.UUID.V4 qualified as UUID
import System.Timeout (timeout)
import Test.Hspec

import HMem.AgentSession (sessionSha256)
import HMem.AgentSession.Transport qualified as Wire
import HMem.Server.SessionPrefix
import HMem.Server.SessionTraffic (ComputeStats(..), MissReason(..), SessionTrafficSnapshot(..), readSessionTraffic)

spec :: Spec
spec = describe "PrefixProof service" $ do
  it "finishes reader cleanup after confirmed joins before reporting a worker failure" $ do
    entered <- newEmptyMVar
    blocked <- newEmptyMVar
    finalizing <- newEmptyMVar
    release <- newEmptyMVar
    cleaned <- newEmptyMVar
    order <- newIORef ([] :: [String])
    let record event=atomicModifyIORef' order (\events -> (events<>[event],()))
        hook index _
          | index == 0 = throwIO (userError "controlled first worker failure")
          | otherwise = (putMVar entered () >> readMVar blocked) `finally`
              uninterruptibleMask_ (putMVar finalizing () >> readMVar release >> record "worker-finalizer")
        cleanup _=record "reader-cleanup" >> void (tryPutMVar cleaned ())
        options=PrefixOptions getCurrentTime (const (pure True)) (\_ _ -> pure BS.empty)
    registry <- newPrefixRegistryWithWorkerHookAndCleanupHook hook cleanup options
    let drain=void (tryPutMVar release ()) >> void (try @SomeException (closePrefixRegistry registry))
    (do
      timeout 1000000 (readMVar entered) `shouldReturn` Just ()
      withAsync (try @SomeException (closePrefixRegistry registry)) $ \closing -> do
        (do
          timeout 1000000 (readMVar finalizing) `shouldReturn` Just ()
          timeout 10000 (readMVar cleaned) `shouldReturn` Nothing
          poll closing >>= (`shouldSatisfy` maybe True (const False))) `finally` putMVar release ()
        result <- wait closing
        result `shouldSatisfy` either (const True) (const False)
      readIORef order `shouldReturn` ["worker-finalizer","reader-cleanup"]) `finally` drain

  it "includes expired queued ownership in cleanup after the visible entries are pruned" $ do
    clock <- getCurrentTime >>= newIORef
    entered <- newEmptyMVar
    blocked <- newEmptyMVar
    cleaned <- newIORef []
    let hook _ _=putMVar entered () >> readMVar blocked
        options=PrefixOptions (readIORef clock) (const (pure True)) (\_ _ -> pure BS.empty)
    -- Both workers remain behind their startup hook while four admitted jobs
    -- expire out of the visible registry. The observer reports which job
    -- reader owners the confirmed-join cleanup actually visited.
    targets <- bracket (newPrefixRegistryWithWorkerHookAndCleanupHook hook (writeIORef cleaned) options) closePrefixRegistry $ \registry -> do
      timeout 1000000 (takeMVar entered) `shouldReturn` Just ()
      timeout 1000000 (takeMVar entered) `shouldReturn` Just ()
      targets <- replicateM 4 (targetFor BS.empty)
      forM_ targets $ \target -> submitPrefixProof registry target >>= (`shouldSatisfy` isRight)
      prefixRegistryCounts registry `shouldReturn` (4,4)
      modifyIORef' clock (addUTCTime 301)
      prefixRegistryCounts registry `shouldReturn` (0,4)
      modifyIORef' clock (addUTCTime 301)
      prefixRegistryCounts registry `shouldReturn` (0,0)
      -- Four unfinished owners no longer refuse a fifth submission.
      fresh <- targetFor BS.empty
      timeout 1000000 (submitPrefixProof registry fresh) >>= (`shouldSatisfy` maybe False isRight)
      pure (fresh:targets)
    sort . nub <$> readIORef cleaned `shouldReturn` sort (map (.request.operationId) targets)

  it "requeues active readers and accepts new work while expired queued owners await cleanup" $ do
    clock <- getCurrentTime >>= newIORef
    let bytes=BS.replicate (2*1048576) 97
    old <- replicateM 2 (targetFor bytes)
    newer <- replicateM 2 (targetFor bytes)
    fresh <- replicateM 2 (targetFor bytes)
    oldEntered <- newEmptyMVar; oldGate <- newEmptyMVar
    newEntered <- newEmptyMVar; newGate <- newEmptyMVar
    requeued <- newEmptyMVar; requeueGate <- newEmptyMVar
    let oldIds=map (.request.operationId) old; newIds=map (.request.operationId) newer
        readWindow target offset=do
          when (offset == 0 && target.request.operationId `elem` oldIds) (putMVar oldEntered () >> readMVar oldGate)
          when (offset == 0 && target.request.operationId `elem` newIds) (putMVar newEntered () >> readMVar newGate)
          pure (BS.take 1048576 (BS.drop (fromIntegral offset) bytes))
        afterRequeue target=when (target.request.operationId `elem` oldIds) (putMVar requeued () >> readMVar requeueGate)
        options=PrefixOptions (readIORef clock) (const (pure True)) readWindow
    bracket (newPrefixRegistryWithRequeueHook afterRequeue options) closePrefixRegistry $ \registry -> do
      forM_ old $ \target -> submitPrefixProof registry target >>= (`shouldSatisfy` isRight)
      replicateM 2 (timeout 1000000 (takeMVar oldEntered)) `shouldReturn` [Just (),Just ()]
      modifyIORef' clock (addUTCTime 250)
      forM_ newer $ \target -> submitPrefixProof registry target >>= (`shouldSatisfy` isRight)
      putMVar oldGate ()
      replicateM 2 (timeout 1000000 (takeMVar requeued)) `shouldReturn` [Just (),Just ()]
      -- Expire after the atomic handoff, before either old turn can finish.
      modifyIORef' clock (addUTCTime 51)
      prefixRegistryCounts registry `shouldReturn` (2,4)
      putMVar requeueGate ()
      replicateM 2 (timeout 1000000 (takeMVar newEntered)) `shouldReturn` [Just (),Just ()]
      forM_ fresh $ \target -> timeout 1000000 (submitPrefixProof registry target) >>= (`shouldSatisfy` maybe False isRight)
      putMVar newGate ()
      forM_ newer $ \target -> await registry target >>= (\value -> value.state `shouldBe` "verified")
      forM_ old $ \target -> await registry target >>= (\value -> value.state `shouldBe` "failed")
      forM_ fresh $ \target -> await registry target >>= (\value -> value.state `shouldBe` "verified")

  it "joins every worker finalizer before reporting an earlier worker failure" $ do
    secondEntered <- newEmptyMVar; blocked <- newEmptyMVar; finalizerEntered <- newEmptyMVar; release <- newEmptyMVar
    let hook index _action
          | index == 0 = throwIO (userError "controlled worker failure")
          | otherwise = (putMVar secondEntered () >> readMVar blocked)
              `finally` uninterruptibleMask_ (putMVar finalizerEntered () >> readMVar release)
        options = PrefixOptions getCurrentTime (const (pure True)) (\_ _ -> pure BS.empty)
    registry <- newPrefixRegistryWithWorkerHook hook options
    ready <- timeout 1000000 (readMVar secondEntered)
    ready `shouldBe` Just ()
    withAsync (try @SomeException (closePrefixRegistry registry)) $ \closing -> do
      (do
        entered <- timeout 1000000 (readMVar finalizerEntered)
        entered `shouldBe` Just ()
        poll closing >>= (`shouldSatisfy` maybe True (const False))) `finally` putMVar release ()
      outcome <- wait closing
      outcome `shouldSatisfy` either (const True) (const False)

  it "accepts the fifth and the fiftieth concurrent submission and fairly finishes them with two workers" $ do
    gate <- newEmptyMVar
    steps <- newIORef []; active <- newIORef (0 :: Int,0 :: Int)
    let bytes = BS.replicate (3*1048576) 97
        readWindow target offset = bracket
          (atomicModifyIORef' active (\(count,peak) -> ((count+1,max peak (count+1)),())))
          (\_ -> atomicModifyIORef' active (\(count,peak) -> ((count-1,peak),()))) $ \_ -> do
            atomicModifyIORef' steps (\values -> (values<>[target.request.operationId],()))
            readMVar gate
            pure (BS.take 1048576 (BS.drop (fromIntegral offset) bytes))
    bracket (newPrefixRegistryWith (PrefixOptions getCurrentTime (const (pure True)) readWindow)) closePrefixRegistry $ \registry -> do
      targets <- replicateM 50 (targetFor bytes)
      forM_ targets $ \target -> submitPrefixProof registry target >>= (`shouldSatisfy` isRight)
      prefixRegistryCounts registry `shouldReturn` (50,50)
      let first = head targets
      submitPrefixProof registry first >>= (`shouldSatisfy` isRight)
      submitPrefixProof registry (first { request = first.request { Wire.sha256 = sessionSha256 "different" } }) `shouldReturn` Left PrefixConflict
      lookupPrefixProof registry first.workspace first.session "foreign-audience" first.request.operationId `shouldReturn` Nothing
      putMVar gate ()
      forM_ targets $ \target -> await registry target >>= (\value -> value.state `shouldBe` "verified")
      prefixRegistryCounts registry `shouldReturn` (0,50)
      observed <- readIORef steps
      -- Fair turns: every job takes its first window before any takes a third.
      length (nub (take 50 observed)) `shouldBe` 50
      (_,peak) <- readIORef active
      peak `shouldSatisfy` (<=2)

  it "retains seventeen terminal results until their five-minute expiry without count eviction" $ do
    clock <- getCurrentTime >>= newIORef
    let options = PrefixOptions (readIORef clock) (const (pure True)) (\_ _ -> pure BS.empty)
    bracket (newPrefixRegistryWith options) closePrefixRegistry $ \registry -> do
      targets <- forM [1..17 :: Int] $ \_ -> do
        target <- targetFor BS.empty
        submitPrefixProof registry target >>= (`shouldSatisfy` isRight)
        _ <- await registry target
        modifyIORef' clock (addUTCTime 0.001)
        pure target
      prefixRegistryCounts registry `shouldReturn` (0,17)
      let oldest = head targets; latest = last targets
      forM_ [oldest, latest] $ \target ->
        lookupPrefixProof registry target.workspace target.session target.audience target.request.operationId >>= (`shouldSatisfy` maybe False ((== "verified") . (.state)))
      modifyIORef' clock (addUTCTime 299.98)
      lookupPrefixProof registry latest.workspace latest.session latest.audience latest.request.operationId >>= (`shouldSatisfy` maybe False ((== "verified") . (.state)))
      modifyIORef' clock (addUTCTime 0.02)
      forM_ [oldest, latest] $ \target ->
        lookupPrefixProof registry target.workspace target.session target.audience target.request.operationId `shouldReturn` Nothing
      prefixRegistryCounts registry `shouldReturn` (0,0)
    bracket (newPrefixRegistryWith options) closePrefixRegistry $ \registry -> prefixRegistryCounts registry `shouldReturn` (0,0)

  it "classifies each unknown operation internally while every lookup stays unknown" $ do
    clock <- getCurrentTime >>= newIORef
    let options = PrefixOptions (readIORef clock) (const (pure True)) (\_ _ -> pure BS.empty)
        classify registry target = lookupPrefixProofClassified registry target.workspace target.session target.audience target.request.operationId
        unknown registry target = lookupPrefixProof registry target.workspace target.session target.audience target.request.operationId `shouldReturn` Nothing
    (survivor, foreignTarget) <- bracket (newPrefixRegistryWith options) closePrefixRegistry $ \registry -> do
      never <- targetFor BS.empty
      classify registry never `shouldReturn` Left MissUnknownSinceStart
      unknown registry never
      first <- targetFor BS.empty
      submitPrefixProof registry first >>= (`shouldSatisfy` isRight)
      _ <- await registry first
      let foreignTarget = first {audience = "user:other-fixture"}
          otherSession = first {session = never.session}
      classify registry foreignTarget `shouldReturn` Left MissTargetMismatch
      classify registry otherSession `shouldReturn` Left MissTargetMismatch
      unknown registry foreignTarget
      later <- forM [1..16 :: Int] $ \_ -> do
        modifyIORef' clock (addUTCTime 0.001)
        target <- targetFor BS.empty
        submitPrefixProof registry target >>= (`shouldSatisfy` isRight)
        _ <- await registry target
        pure target
      -- Terminal results are not evicted by count.
      classify registry first >>= (`shouldSatisfy` either (const False) ((== "verified") . (.state)))
      classify registry foreignTarget `shouldReturn` Left MissTargetMismatch
      let survivor = last later
      classify registry survivor >>= (`shouldSatisfy` either (const False) ((== "verified") . (.state)))
      modifyIORef' clock (addUTCTime 300)
      classify registry first `shouldReturn` Left MissExpired
      unknown registry first
      classify registry survivor `shouldReturn` Left MissExpired
      unknown registry survivor
      modifyIORef' clock (addUTCTime 300)
      classify registry never `shouldReturn` Left MissUnknownOperation
      snapshot <- readSessionTraffic (prefixTraffic registry)
      fmap (.proofs) (Map.lookup ("verified","le_1mib") snapshot.computeStats) `shouldBe` Just 17
      pure (survivor, foreignTarget)
    -- A restarted process retains no operation or attribution from its predecessor.
    bracket (newPrefixRegistryWith options) closePrefixRegistry $ \registry -> do
      classify registry survivor `shouldReturn` Left MissUnknownSinceStart
      classify registry foreignTarget `shouldReturn` Left MissUnknownSinceStart

  it "admits more than sixteen sweeps from four followers and retains every result" $ do
    let bytes = BS.replicate (3*1048576) 97
    initialGate <- newEmptyMVar
    gateRef <- newIORef initialGate
    let readWindow _ offset = do
          gate <- readIORef gateRef
          readMVar gate
          pure (BS.take 1048576 (BS.drop (fromIntegral offset) bytes))
    bracket (newPrefixRegistryWith (PrefixOptions getCurrentTime (const (pure True)) readWindow)) closePrefixRegistry $ \registry -> do
      followers <- replicateM 4 (targetFor bytes)
      rounds <- forM [1..5 :: Int] $ \roundIndex -> do
        gate <- newEmptyMVar
        writeIORef gateRef gate
        targets <- forM followers $ \target -> do
          operation <- UUID.nextRandom
          pure target {request = target.request {Wire.operationId = operation}}
        forM_ targets $ \target -> submitPrefixProof registry target >>= (`shouldSatisfy` isRight)
        prefixRegistryCounts registry `shouldReturn` (4,roundIndex*4)
        forM_ targets $ \target -> lookupPrefixProof registry target.workspace target.session target.audience target.request.operationId >>= (`shouldSatisfy` maybe False ((== "pending") . (.state)))
        putMVar gate ()
        forM_ targets $ \target -> await registry target >>= (\value -> value.state `shouldBe` "verified")
        pure targets
      prefixRegistryCounts registry `shouldReturn` (0,20)
      let earliest = head (head rounds)
      lookupPrefixProof registry earliest.workspace earliest.session earliest.audience earliest.request.operationId
        >>= (`shouldSatisfy` maybe False ((== "verified") . (.state)))

  it "opens per-turn readers for twenty concurrent multi-window proofs without exceeding the worker count" $ do
    gate <- newEmptyMVar
    counters <- newIORef (0 :: Int, 0 :: Int, 0 :: Int)
    let bytes = BS.replicate (3*1048576+17) 98
        open _ = do
          atomicModifyIORef' counters (\(count,peak,opened) -> ((count+1,max peak (count+1),opened+1),()))
          pure PrefixReader
            { readPrefixWindow = \offset limit -> readMVar gate >> pure (BS.take limit (BS.drop (fromIntegral offset) bytes))
            , closePrefixReader = atomicModifyIORef' counters (\(count,peak,opened) -> ((count-1,peak,opened),())) }
        options = PrefixOptions getCurrentTime (const (pure True)) (\_ _ -> error "the reader seam serves every window")
    bracket (newPrefixRegistryWithReaders 3 open options) closePrefixRegistry $ \registry -> do
      targets <- replicateM 20 (targetFor bytes)
      forM_ targets $ \target -> submitPrefixProof registry target >>= (`shouldSatisfy` isRight)
      putMVar gate ()
      forM_ targets $ \target -> await registry target >>= (\value -> value.state `shouldBe` "verified")
      (count,peak,opened) <- readIORef counters
      count `shouldBe` 0
      peak `shouldSatisfy` (<= 3)
      -- Each of the four windows reopened its reader after yielding a turn.
      opened `shouldBe` 20*4

  it "evicts only the earliest-expiring terminal results above the ceiling and never live work" $ do
    clock <- getCurrentTime >>= newIORef
    gate <- newEmptyMVar
    blocked <- targetFor (BS.replicate 1024 99)
    let readWindow target _
          | target == blocked = readMVar gate >> pure (BS.replicate 1024 99)
          | otherwise = pure BS.empty
        options = PrefixOptions (readIORef clock) (const (pure True)) readWindow
        classify registry target = lookupPrefixProofClassified registry target.workspace target.session target.audience target.request.operationId
    bracket (newPrefixRegistryWithTerminalLimit 3 options) closePrefixRegistry $ \registry -> do
      submitPrefixProof registry blocked >>= (`shouldSatisfy` isRight)
      done <- forM [1..5 :: Int] $ \_ -> do
        modifyIORef' clock (addUTCTime 0.001)
        target <- targetFor BS.empty
        submitPrefixProof registry target >>= (`shouldSatisfy` isRight)
        _ <- await registry target
        pure target
      prefixRegistryCounts registry `shouldReturn` (1,4)
      forM_ (take 2 done) $ \target -> classify registry target `shouldReturn` Left MissEvicted
      forM_ (drop 2 done) $ \target -> classify registry target >>= (`shouldSatisfy` either (const False) ((== "verified") . (.state)))
      classify registry blocked >>= (`shouldSatisfy` either (const False) ((== "pending") . (.state)))
      putMVar gate ()
      _ <- await registry blocked
      -- The newly terminal result now displaces the earliest retained one.
      classify registry (done !! 2) `shouldReturn` Left MissEvicted
      classify registry blocked >>= (`shouldSatisfy` either (const False) ((== "verified") . (.state)))
      prefixRegistryCounts registry `shouldReturn` (0,3)

  it "keeps submit and lookup cost bounded with ten thousand retained results" $ do
    let options = PrefixOptions getCurrentTime (const (pure True)) (\_ _ -> pure BS.empty)
        settle registry expected = do
          let loop = prefixRegistryCounts registry >>= \counts -> if counts == (0,expected) then pure () else threadDelay 1000 >> loop
          timeout 20000000 loop `shouldReturn` Just ()
        batch registry count = do
          targets <- replicateM count (targetFor BS.empty)
          started <- getCurrentTime
          forM_ targets $ \target -> submitPrefixProof registry target >>= (`shouldSatisfy` isRight)
          forM_ targets $ \target -> lookupPrefixProof registry target.workspace target.session target.audience target.request.operationId
            >>= (`shouldSatisfy` maybe False (const True))
          ended <- getCurrentTime
          pure (realToFrac (diffUTCTime ended started) :: Double)
    bracket (newPrefixRegistryWith options) closePrefixRegistry $ \registry -> do
      early <- batch registry 2000
      settle registry 2000
      _ <- batch registry 6000
      settle registry 8000
      late <- batch registry 2000
      settle registry 10000
      -- Indexed pruning: cost per operation does not grow with the ~10k
      -- retained results, unlike a full scan of every entry per call.
      late `shouldSatisfy` (<= 3*early + 0.25)

  it "sizes production workers to the CPU and joins every worker on shutdown" $ do
    capabilities <- getNumCapabilities
    prefixWorkerCount `shouldReturn` max 2 (capabilities `div` 2)
    gate <- newEmptyMVar
    active <- newIORef (0 :: Int, 0 :: Int)
    let bytes = BS.replicate 1024 97
        readWindow _ _ = bracket
          (atomicModifyIORef' active (\(count,peak) -> ((count+1,max peak (count+1)),())))
          (\_ -> atomicModifyIORef' active (\(count,peak) -> ((count-1,peak),()))) $ \_ -> readMVar gate >> pure bytes
    registry <- newPrefixRegistryWithWorkers 6 (PrefixOptions getCurrentTime (const (pure True)) readWindow)
    (do
      targets <- replicateM 6 (targetFor bytes)
      forM_ targets $ \target -> submitPrefixProof registry target >>= (`shouldSatisfy` isRight)
      let busy = readIORef active >>= \(count,_) -> if count == 6 then pure () else threadDelay 1000 >> busy
      timeout 2000000 busy `shouldReturn` Just ()) `finally` putMVar gate ()
    closePrefixRegistry registry
    fst <$> readIORef active `shouldReturn` 0

  it "publishes deadline failure and joins an interrupted in-flight window" $ do
    clock <- getCurrentTime >>= newIORef
    let bytes = BS.replicate 1024 97
        options = PrefixOptions (readIORef clock) (const (pure True)) $ \_ _ -> modifyIORef' clock (addUTCTime 301) >> pure bytes
    bracket (newPrefixRegistryWith options) closePrefixRegistry $ \registry -> do
      target <- targetFor bytes
      submitPrefixProof registry target >>= (`shouldSatisfy` isRight)
      result <- await registry target
      result.state `shouldBe` "failed"
      result.error `shouldBe` Just "verification_deadline"
    entered <- newEmptyMVar; blocked <- newEmptyMVar; closed <- newIORef False
    let blockedOptions = PrefixOptions getCurrentTime (const (pure True)) $ \_ _ -> (putMVar entered () >> readMVar blocked) `finally` writeIORef closed True
    registry <- newPrefixRegistryWith blockedOptions
    target <- targetFor bytes
    _ <- submitPrefixProof registry target
    ready <- timeout 1000000 (readMVar entered)
    ready `shouldBe` Just ()
    closePrefixRegistry registry
    readIORef closed `shouldReturn` True
    submitPrefixProof registry target `shouldReturn` Left PrefixClosed

  it "fails mismatching digest and invalid source target rather than certifying absence" $ do
    let bytes = "canonical-prefix"
    bracket (newPrefixRegistryWith (PrefixOptions getCurrentTime (const (pure True)) (\_ _ -> pure bytes))) closePrefixRegistry $ \registry -> do
      target <- targetFor bytes
      let wrong = target { request = target.request { Wire.sha256 = sessionSha256 "wrong" } }
      _ <- submitPrefixProof registry wrong
      result <- await registry wrong
      result.state `shouldBe` "mismatch"
    bracket (newPrefixRegistryWith (PrefixOptions getCurrentTime (const (pure False)) (\_ _ -> error "invalid target must not be read"))) closePrefixRegistry $ \registry -> do
      target <- targetFor bytes
      submitPrefixProof registry target `shouldReturn` Left PrefixInvalid

targetFor :: BS.ByteString -> IO PrefixTarget
targetFor bytes = do
  workspace <- UUID.nextRandom; session <- UUID.nextRandom; operation <- UUID.nextRandom
  pure $ PrefixTarget workspace session "user:authenticated-fixture" (Wire.PrefixProofRequest operation (fromIntegral (BS.length bytes)) (if BS.null bytes then 0 else 1) (sessionSha256 bytes))

await :: PrefixRegistry -> PrefixTarget -> IO Wire.PrefixProofResponse
await registry target = do
  result <- timeout 2000000 loop
  maybe (expectationFailure "prefix operation did not finish" >> fail "prefix timeout") pure result
  where
    loop = do
      value <- lookupPrefixProof registry target.workspace target.session target.audience target.request.operationId
      case value of
        Just response | response.state /= "pending" -> pure response
        _ -> threadDelay 1000 >> loop

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False
