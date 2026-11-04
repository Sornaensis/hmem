module HMem.Server.Document.LauncherSpec (spec) where

import Control.Exception (SomeException,SomeAsyncException,Exception,try,throwIO,fromException,mask)
import Control.Concurrent.Async
import Control.Concurrent (throwTo,myThreadId)
import Control.Concurrent.MVar
import Data.IORef
import Data.Map.Strict qualified
import Data.List (isInfixOf)
import Control.Monad (forM_,void,unless)
import Data.Aeson
import Data.ByteString qualified as BS
import Data.Either (isLeft)
import Data.Text qualified
import Data.Time (getCurrentTime,addUTCTime)
import System.FilePath ((</>),takeDirectory)
import System.Directory (getTemporaryDirectory,canonicalizePath,createDirectory,removeDirectoryRecursive,pathIsSymbolicLink,doesDirectoryExist)
import System.IO.Unsafe (unsafePerformIO)
import System.Timeout (timeout)
import System.Process (getPid)
import System.Info (os)
import System.IO (hClose,hPutStrLn,stderr)
import Data.UUID.V4 qualified as UUID
import Test.Hspec
import HMem.Document.Codec (payloadSha256)
import HMem.Document.Preview
import HMem.Server.Document.Launcher qualified as Launcher

spec :: Spec
spec=describe "Bounded preview launcher protocol" $ do
  it "rejects duplicate escaped keys, nested duplicates, nonobjects and trailing JSON" $ do
    forM_ ["{\"a\":1,\"a\":2}","{\"a\":1,\"\\u0061\":2}","{\"x\":{\"a\":1,\"a\":2}}","[]","{} {}"]
      (\bytes->reject(Launcher.decodeHeader bytes))
    Launcher.decodeHeader "{\"a\":[{\"b\":1}],\"c\":true}" `shouldReturn`
      object["a" .= [object["b" .= (1::Int)]],"c" .= True]
  it "bounds nesting and preserves opaque floating geometry instead of hashing a reserialization" $ do
    reject(Launcher.decodeHeader(BS.replicate 66 91<>"0"<>BS.replicate 66 93))
    value<-Launcher.decodeHeader "{\"opaque\":1.0000000000000002}"
    case value of Object _->pure(); _->expectationFailure "Lost parsed geometry"
  it "maps and bounds image plans and rejects mismatched hash/shape targets before admission" $ do
    run<-sampleRun "image/png"
    let plan=object["original_sha256" .= run.rawSha256,"media_type" .= run.mediaType,
          "policy_version" .= run.policyVersion,"limits_sha256" .= payloadSha256(canonicalPreviewLimits run.limits),
          "output_count" .= (2::Int),"maximum_bytes" .= run.limits.outputBytes,
          "inspection_decoded_pixels" .= (0::Int),"inspection_raster_bytes" .= (0::Int),
          "info" .= object["width" .= (1::Int),"height" .= (1::Int)]]
    inspection<-Launcher.validatePlan run plan(payloadSha256 "opaque plan bytes")
    inspection.outputShape `shouldBe` [("image",0,"image/png"),("thumbnail",0,"image/png")]
    reject(Launcher.validatePlan run plan "invalid")
    reject(Launcher.validatePlan run{rawSha256=payloadSha256 "different"} plan(payloadSha256 "plan"))
  it "clamps monotonic deadlines and never treats an expired deadline as work time" $ do
    deadline<-Launcher.deadlineFromRemaining 0
    Launcher.remainingMicros deadline `shouldReturn` 0
    reject(Launcher.within deadline(pure()))
    deadline2<-Launcher.deadlineFromRemaining 1000000
    remaining<-Launcher.remainingMicros deadline2
    remaining `shouldSatisfy` (\n->n>0&&n<=1000000)
  it "captures a real PID for a live job-owned child and preserves it across immediate exit" $
    withNativeFixture $ \directory registry senders->do
      let livePath=directory </> "pid-live.py"
          fastPath=directory </> "pid-fast.py"
      BS.writeFile livePath "import sys\nsys.stdin.buffer.read()\n"
      BS.writeFile fastPath "pass\n"
      run<-sampleRun "image/png"
      child<-Launcher.spawnNative registry defaultPreviewConfig{launcherPath=Just livePath}run[]
      identity<-readIORef child.nativePid
      identity `shouldSatisfy` maybe False (>0)
      reaper<-Launcher.nativeWaiter child
      poll reaper >>= (`shouldSatisfy` \case Nothing->True;_->False)
      -- Public process.getPid omits OpenExtHandle even while this actual
      -- Windows job-owned child is alive. The immutable owned-handle snapshot
      -- must still be valid, without disabling tree ownership.
      if os=="mingw32" then getPid child.process `shouldReturn` Nothing else pure()
      hClose child.input
      timeout 3000000(waitCatch reaper) >>= (`shouldSatisfy` \case Just(Right _)->True;_->False)
      readIORef child.nativePid `shouldReturn` identity
      fast<-Launcher.spawnNative registry defaultPreviewConfig{launcherPath=Just fastPath}run[]
      readIORef fast.nativePid >>= (`shouldSatisfy` maybe False (>0))
      verifyOwned registry run
  it "cleans a live registry after an injected assertion failure without replacing its primary cause" $ do
    captured<-newIORef Nothing
    attempted<-try @SomeException $ withNativeFixture $ \directory registry _->do
      let path=directory </> "assertion-live.py"
      BS.writeFile path "import sys\nsys.stdin.buffer.read()\n"
      run<-sampleRun "image/png"
      _<-Launcher.spawnNative registry defaultPreviewConfig{launcherPath=Just path}run[]
      writeIORef captured(Just(directory,registry,run))
      expectationFailure "injected native fixture primary assertion"
    case attempted of
      Left primary->show primary `shouldSatisfy` isInfixOf "injected native fixture primary assertion"
      Right _->expectationFailure "Injected failure disappeared"
    saved<-readIORef captured
    case saved of
      Nothing->expectationFailure "Fixture did not install its native owner"
      Just(directory,registry,run)->do
        doesDirectoryExist directory `shouldReturn` False
        native<-Data.Map.Strict.findWithDefault [] run.id <$>readIORef registry
        forM_ native $ \child->do
          reaper<-Launcher.nativeWaiter child
          poll reaper >>= (`shouldSatisfy` \case Just(Right _)->True;_->False)
  it "retains and joins the real native handle when cancellation interrupts the masked handoff" $
    withNativeFixture $ \directory registry senders->do
      let path=directory </> "handoff.py"
      BS.writeFile path "import sys,time\nif '--cleanup' not in sys.argv: time.sleep(60)\n"
      run<-sampleRun "image/png"
      entered<-newIORef False
      release<-newEmptyMVar
      physical<-newIORef Nothing
      ownerThread<-myThreadId
      deadline<-Launcher.deadlineFromRemaining 6000000
      let hooks=noHooks{Launcher.registerNative = \_->do
              writeIORef entered True
              sender<-async(throwTo ownerThread AsyncCancelled)
              atomicModifyIORef' senders(\current->(sender:current,()))
              takeMVar release
            ,Launcher.settled = \outcome->writeIORef physical(Just outcome.physical)}
      result<-try @SomeException(Launcher.launchGeneration registry
        defaultPreviewConfig{launcherPath=Just path} run "raw" deadline hooks)
      readIORef entered `shouldReturn` True
      case result of
        Left failure->(fromException failure::Maybe AsyncCancelled) `shouldSatisfy` (/=Nothing)
        _->expectationFailure "Cancellation was swallowed"
      verifyOwned registry run
      receipt<-readIORef physical
      receipt `shouldBe` Just(Launcher.PhysicalDisposition True True False)
  it "bounds a stalled native header read and retains uncertainty without losing its pipes" $
    withNativeFixture $ \directory registry senders->do
      let path=directory </> "stalled-header.py"
      BS.writeFile path "import sys\nif '--cleanup' not in sys.argv: sys.stdin.buffer.read()\n"
      run<-sampleRun "image/png"
      deadline<-Launcher.deadlineFromRemaining 6000000
      result<-Launcher.launchGeneration registry defaultPreviewConfig{launcherPath=Just path}
        run "raw" deadline noHooks
      case result.result of Left "deadline_exceeded"->pure();_->expectationFailure "Stalled header did not retain its deadline failure"
      result.physical `shouldBe` Launcher.PhysicalDisposition True True True
      verifyOwned registry run
  it "includes a cleanup helper acquired after main exit in every closure proof and retry" $
    withNativeFixture $ \directory registry senders->do
      let path=directory </> "held-cleanup.py"
      BS.writeFile path "import sys,time,struct,json\nif '--cleanup' in sys.argv:\n time.sleep(60)\nelse:\n b=json.dumps({'version':1,'status':'error','code':'worker_crashed'}).encode()\n sys.stdout.buffer.write(struct.pack('>I',len(b))+b);sys.stdout.buffer.flush()\n"
      run<-sampleRun "image/png"
      deadline<-Launcher.deadlineFromRemaining 6000000
      result<-Launcher.launchGeneration registry defaultPreviewConfig{launcherPath=Just path}
        run "raw" deadline noHooks
      result.physical.nativeExited `shouldBe` False
      result.physical.cleanupVerified `shouldBe` False
      -- A fresh test-owner cleanup budget does not alter the production run's
      -- failed deadline/receipt. It proves retained handles can actually settle.
      verifyOwned registry run

-- A registry fixture owns its input directory independently of the harness's
-- automatically removed sandbox, so unresolved native ownership can retain both.
-- There are only finitely many native cases in this spec; retained failures are
-- kept in-process and the runner retains its contained runtime for diagnosis.
data RetainedFixture=RetainedFixture FilePath Launcher.NativeRegistry (IORef[Async ()])
{-# NOINLINE retainedFixtures #-}
retainedFixtures :: IORef[RetainedFixture]
retainedFixtures=unsafePerformIO(newIORef [])
newtype FixtureCleanupFailure=FixtureCleanupFailure String deriving stock(Show)
instance Exception FixtureCleanupFailure

withNativeFixture :: (FilePath->Launcher.NativeRegistry->IORef[Async ()]->IO a)->IO a
withNativeFixture action=mask $ \restore->do
  parent<-getTemporaryDirectory >>=canonicalizePath
  identifier<-UUID.nextRandom
  let directory=parent </> ("hmem-preview-native-"<>show identifier)
  createDirectory directory
  registry<-Launcher.newNativeRegistry
  senders<-newIORef []
  attempted<-try @SomeException(restore(action directory registry senders))
  cleaned<-try @SomeException $ restore $ do
    complete<-timeout 10000000 $ do
      deadline<-Launcher.deadlineFromRemaining 8000000
      native<-concat . Data.Map.Strict.elems <$>readIORef registry
      _<-mapM(Launcher.stopNative deadline)native
      signals<-readIORef senders
      signalClosure<-and <$>mapM(Launcher.joined deadline)signals
      -- No fixture producer survives this point. Revisit the full registry in
      -- case its cancellation path registered a helper during the first join.
      finalNative<-concat . Data.Map.Strict.elems <$>readIORef registry
      finalClosure<-and <$>mapM(Launcher.stopNative deadline)finalNative
      unless(signalClosure&&finalClosure)(throwIO(FixtureCleanupFailure "native_or_pipe_join_unverified"))
      resolved<-canonicalizePath directory
      symbolic<-pathIsSymbolicLink directory
      unless(resolved==directory&&takeDirectory resolved==parent&&not symbolic)
        (throwIO(FixtureCleanupFailure "fixture_containment_changed"))
      removeDirectoryRecursive directory
    unless(complete==Just())(throwIO(FixtureCleanupFailure "fixture_cleanup_deadline"))
  case cleaned of
    Left _->do
      atomicModifyIORef' retainedFixtures(\current->(RetainedFixture directory registry senders:current,()))
      hPutStrLn stderr("preview_native_fixture_cleanup_unverified: retained="<>directory)
    Right()->pure()
  -- A cleanup failure never replaces the original assertion or async cause.
  case attempted of
    Left primary->throwIO primary
    Right value->case cleaned of
      Left cleanup->case fromException cleanup::Maybe SomeAsyncException of
        Just _->throwIO cleanup
        Nothing->throwIO(FixtureCleanupFailure "fixture_cleanup_unverified")
      Right()->pure value

noHooks :: Launcher.LauncherHooks
noHooks=Launcher.LauncherHooks(\_->pure())(\_->pure())(\_->pure())(\_->pure())(pure())(\_->pure())
verifyOwned :: Launcher.NativeRegistry -> PreviewRun -> IO()
verifyOwned registry run=do
  owners<-readIORef registry
  let children=Data.Map.Strict.findWithDefault [] run.id owners
  length children `shouldSatisfy` (>0)
  deadline<-Launcher.deadlineFromRemaining 3000000
  joined<-mapM(Launcher.stopNative deadline)children
  joined `shouldSatisfy` and
  forM_ children $ \child->do
    reaper<-Launcher.nativeWaiter child
    poll reaper >>= (`shouldSatisfy` \case Just(Right _)->True;_->False)

sampleRun :: Data.Text.Text -> IO PreviewRun
sampleRun media=do
  owner<-UUID.nextRandom;workspace<-UUID.nextRandom;document<-UUID.nextRandom
  controller<-UUID.nextRandom;token<-UUID.nextRandom;now<-getCurrentTime
  pure(PreviewRun owner workspace document 1 controller token(addUTCTime 30 now)(addUTCTime 45 now)
    3(payloadSha256 "raw")media previewPolicyVersion qualifiedPreviewImage defaultPreviewLimits)
reject :: IO a -> IO()
reject action=try @SomeException action >>= \result->case result of
  Left _->pure();Right _->expectationFailure "Expected bounded rejection"
