module HMem.Server.SessionPrefixSpec (spec) where

import Control.Concurrent (threadDelay, newEmptyMVar, readMVar, putMVar, takeMVar, tryPutMVar)
import Control.Exception (SomeException, bracket, finally, throwIO, try, uninterruptibleMask_)
import Control.Concurrent.Async (withAsync, wait, poll)
import Control.Monad (forM, forM_, replicateM, void, when)
import Data.ByteString qualified as BS
import Data.IORef
import Data.List (nub,sort)
import Data.Time
import Data.UUID.V4 qualified as UUID
import System.Timeout (timeout)
import Test.Hspec

import HMem.AgentSession (sessionSha256)
import HMem.AgentSession.Transport qualified as Wire
import HMem.Server.SessionPrefix

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
      fresh <- targetFor BS.empty
      timeout 1000000 (submitPrefixProof registry fresh) `shouldReturn` Just (Left PrefixBusy)
      pure targets
    sort . nub <$> readIORef cleaned `shouldReturn` sort (map (.request.operationId) targets)

  it "reserves requeue capacity for active readers while expired queued owners await cleanup" $ do
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
      forM_ fresh $ \target -> timeout 1000000 (submitPrefixProof registry target) `shouldReturn` Just (Left PrefixBusy)
      putMVar newGate ()
      forM_ newer $ \target -> await registry target >>= (\value -> value.state `shouldBe` "verified")
      forM_ old $ \target -> await registry target >>= (\value -> value.state `shouldBe` "failed")
      submitPrefixProof registry (head fresh) >>= (`shouldSatisfy` isRight)
      await registry (head fresh) >>= (\value -> value.state `shouldBe` "verified")

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

  it "binds all targets and fairly finishes four jobs with only two workers" $ do
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
      targets <- replicateM 4 (targetFor bytes)
      forM_ targets $ \target -> submitPrefixProof registry target >>= (`shouldSatisfy` isRight)
      prefixRegistryCounts registry `shouldReturn` (4,4)
      fifth <- targetFor bytes
      submitPrefixProof registry fifth `shouldReturn` Left PrefixBusy
      let first = head targets
      submitPrefixProof registry first >>= (`shouldSatisfy` isRight)
      submitPrefixProof registry (first { request = first.request { Wire.sha256 = sessionSha256 "different" } }) `shouldReturn` Left PrefixConflict
      lookupPrefixProof registry first.workspace first.session "foreign-audience" first.request.operationId `shouldReturn` Nothing
      putMVar gate ()
      forM_ targets $ \target -> await registry target >>= (\value -> value.state `shouldBe` "verified")
      prefixRegistryCounts registry `shouldReturn` (0,4)
      observed <- readIORef steps
      length (nub (take 4 observed)) `shouldBe` 4
      (_,peak) <- readIORef active
      peak `shouldSatisfy` (<=2)

  it "evicts oldest terminal entries on admission before the five-minute maximum" $ do
    clock <- getCurrentTime >>= newIORef
    let options = PrefixOptions (readIORef clock) (const (pure True)) (\_ _ -> pure BS.empty)
    bracket (newPrefixRegistryWith options) closePrefixRegistry $ \registry -> do
      targets <- forM [1..17 :: Int] $ \_ -> do
        target <- targetFor BS.empty
        submitPrefixProof registry target >>= (`shouldSatisfy` isRight)
        _ <- await registry target
        modifyIORef' clock (addUTCTime 0.001)
        pure target
      prefixRegistryCounts registry `shouldReturn` (0,16)
      let oldest = head targets; latest = last targets
      lookupPrefixProof registry oldest.workspace oldest.session oldest.audience oldest.request.operationId `shouldReturn` Nothing
      lookupPrefixProof registry latest.workspace latest.session latest.audience latest.request.operationId >>= (`shouldSatisfy` maybe False ((== "verified") . (.state)))
      modifyIORef' clock (addUTCTime 300)
      lookupPrefixProof registry latest.workspace latest.session latest.audience latest.request.operationId `shouldReturn` Nothing
    bracket (newPrefixRegistryWith options) closePrefixRegistry $ \registry -> prefixRegistryCounts registry `shouldReturn` (0,0)

  it "admits more than sixteen sweeps from four followers without evicting their live work" $ do
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
        prefixRegistryCounts registry `shouldReturn` (4,min 16 (roundIndex*4))
        forM_ targets $ \target -> lookupPrefixProof registry target.workspace target.session target.audience target.request.operationId >>= (`shouldSatisfy` maybe False ((== "pending") . (.state)))
        putMVar gate ()
        forM_ targets $ \target -> await registry target >>= (\value -> value.state `shouldBe` "verified")
        pure targets
      prefixRegistryCounts registry `shouldReturn` (0,16)
      let evicted = head (head rounds)
      lookupPrefixProof registry evicted.workspace evicted.session evicted.audience evicted.request.operationId `shouldReturn` Nothing

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
