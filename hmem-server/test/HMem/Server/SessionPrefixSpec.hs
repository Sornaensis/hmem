module HMem.Server.SessionPrefixSpec (spec) where

import Control.Concurrent (threadDelay, newEmptyMVar, readMVar, putMVar)
import Control.Exception (SomeException, bracket, finally, throwIO, try, uninterruptibleMask_)
import Control.Concurrent.Async (withAsync, wait, poll)
import Control.Monad (forM, forM_, replicateM)
import Data.ByteString qualified as BS
import Data.IORef
import Data.List (nub)
import Data.Time
import Data.UUID.V4 qualified as UUID
import System.Timeout (timeout)
import Test.Hspec

import HMem.AgentSession (sessionSha256)
import HMem.AgentSession.Transport qualified as Wire
import HMem.Server.SessionPrefix

spec :: Spec
spec = describe "PrefixProof service" $ do
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
