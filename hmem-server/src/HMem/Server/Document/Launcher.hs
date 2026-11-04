{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
-- | Qualified launcher protocol and native ownership; no decoder runs in-server.
module HMem.Server.Document.Launcher where

import Control.Concurrent.Async
import Control.Concurrent (throwTo)
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad (unless,when,void,forM,forM_)
import Data.Aeson hiding(decode)
import Data.Aeson.Decoding.ByteString (bsToTokens)
import Data.Aeson.Decoding.Tokens
import Data.Aeson.Key (Key)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Char (toUpper)
import Data.IORef
import Data.Int (Int64)
import Data.List (sort)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.UUID qualified as UUID
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import System.Environment (getEnvironment)
import System.Exit (ExitCode(..))
import System.Info (os)
import System.IO
import System.Process
#if defined(mingw32_HOST_OS)
import System.Process.Internals (withProcessHandle,ProcessHandle__(..))
import System.Win32.Process qualified as Win32
#endif
import System.Timeout (timeout)
import HMem.DB.Document qualified as Document
import HMem.Document.Codec (payloadSha256)
import HMem.Document.Preview

newtype Deadline=Deadline Word64
deadlineFromRemaining :: Int -> IO Deadline
deadlineFromRemaining micros=do
  now<-getMonotonicTimeNSec
  pure(Deadline(now+fromIntegral(max 0(min 120000000 micros))*1000))
remainingMicros :: Deadline -> IO Int
remainingMicros(Deadline end)=do
  now<-getMonotonicTimeNSec
  pure(if now>=end then 0 else fromIntegral(div (end-now) 1000))
workDeadline :: Deadline -> Deadline
workDeadline(Deadline end)=Deadline(if end>5000000000 then end-5000000000 else 0)
within :: Deadline -> IO a -> IO a
within deadline action=do
  remaining<-remainingMicros deadline
  when(remaining<=0)(throwIO(ProtocolFailure "deadline_exceeded"))
  timeout remaining action >>=maybe(throwIO(ProtocolFailure "deadline_exceeded"))pure

newtype ProtocolFailure=ProtocolFailure Text deriving stock(Show)
instance Exception ProtocolFailure
newtype WorkerFailure=WorkerFailure Text deriving stock(Show)
instance Exception WorkerFailure
ensure :: Bool -> Text -> IO()
ensure condition code=unless condition(throwIO(ProtocolFailure code))
protocol :: Bool -> IO()
protocol condition=ensure condition "preview_protocol_invalid"

-- Aeson's public token stream retains every decoded key, unlike its Value map.
-- Bound nesting before constructing Value, and reject duplicate escaped keys.
uniqueTokens :: Int -> Tokens k String -> Either Text k
uniqueTokens depth tokens
  | depth>64=Left "preview_protocol_invalid"
  | otherwise=case tokens of
      TkLit _ rest->Right rest
      TkText _ rest->Right rest
      TkNumber _ rest->Right rest
      TkArrayOpen values->uniqueArray(depth+1)values
      TkRecordOpen values->uniqueRecord(depth+1)Set.empty values
      TkErr _->Left "preview_protocol_invalid"
uniqueArray :: Int -> TkArray k String -> Either Text k
uniqueArray depth values=case values of
  TkArrayEnd rest->Right rest
  TkArrayErr _->Left "preview_protocol_invalid"
  TkItem item->uniqueTokens depth item >>=uniqueArray depth
uniqueRecord :: Int -> Set.Set Key -> TkRecord k String -> Either Text k
uniqueRecord depth seen values=case values of
  TkRecordEnd rest->Right rest
  TkRecordErr _->Left "preview_protocol_invalid"
  TkPair key item | Set.member key seen->Left "preview_protocol_invalid"
                  | otherwise->uniqueTokens depth item >>=uniqueRecord depth(Set.insert key seen)
decodeHeader :: ByteString -> IO Value
decodeHeader bytes=do
  case uniqueTokens 0(bsToTokens bytes) of
    Left code->throwIO(ProtocolFailure code)
    Right rest->protocol(BS.all(\x->elem x [9,10,13,32])rest)
  value<-either(const(throwIO(ProtocolFailure "preview_protocol_invalid")))pure(eitherDecodeStrict' bytes)
  case value of Object _->pure value; _->throwIO(ProtocolFailure "preview_protocol_invalid")
objectFields :: [Text] -> Value -> IO Object
objectFields keys(Object fields)=do
  protocol(sort(map Key.toText(KM.keys fields))==sort keys)
  pure fields
objectFields _ _=throwIO(ProtocolFailure "preview_protocol_invalid")
field :: FromJSON a => Object -> Key -> IO a
field fields key=case KM.lookup key fields of
  Nothing->throwIO(ProtocolFailure "preview_protocol_invalid")
  Just value->case fromJSON value of Success decoded->pure decoded; Error _->throwIO(ProtocolFailure "preview_protocol_invalid")

readExactly :: Handle -> Int -> IO ByteString
readExactly handle count=go count []
  where
    go 0 parts=pure(BS.concat(reverse parts))
    go left parts=do
      bytes<-BS.hGetSome handle(min 65536 left)
      ensure(not(BS.null bytes))"preview_truncated_output"
      go(left-BS.length bytes)(bytes:parts)
decodeLength :: ByteString -> Integer
decodeLength=BS.foldl'(\value byte->value*256+fromIntegral byte)0
lengthBytes :: Int -> Integer -> ByteString
lengthBytes width value=BS.pack[fromIntegral(div value (256^index))|index<-reverse[0..width-1]]
readHeader :: Handle -> IO Value
readHeader handle=do
  count<-decodeLength <$>readExactly handle 4
  ensure(count>0 &&count<=262144)"preview_header_limit"
  readExactly handle(fromIntegral count) >>=decodeHeader
writeHeader :: Handle -> Value -> IO()
writeHeader handle value=do
  let bytes=LBS.toStrict(encode value)
  protocol(not(BS.null bytes)&&BS.length bytes<=65536)
  BS.hPut handle(lengthBytes 4(fromIntegral(BS.length bytes))<>bytes)
  hFlush handle

data Inspection=Inspection
  { planValue :: !Value, planSha256 :: !Text, outputShape :: ![(Text,Int,Text)], pageCount :: !Int
  } deriving stock(Show,Eq)
validatePlan :: PreviewRun -> Value -> Text -> IO Inspection
validatePlan run value planHash=do
  protocol(validHash planHash)
  plan<-objectFields["original_sha256","media_type","policy_version","limits_sha256","output_count","maximum_bytes",
    "inspection_decoded_pixels","inspection_raster_bytes","info"]value
  original<-field plan "original_sha256"; media<-field plan "media_type"; policy<-field plan "policy_version"
  limitsHash<-field plan "limits_sha256"; outputs<-field plan "output_count"::IO Integer
  maximumBytes<-field plan "maximum_bytes"::IO Integer
  decoded<-field plan "inspection_decoded_pixels"::IO Integer
  raster<-field plan "inspection_raster_bytes"::IO Integer
  protocol(original==run.rawSha256&&media==run.mediaType&&policy==run.policyVersion
    &&limitsHash==payloadSha256(canonicalPreviewLimits run.limits)&&maximumBytes==fromIntegral run.limits.outputBytes&&decoded==0&&raster==0)
  info<-field plan "info"::IO Value
  (shape,pages)<-case run.mediaType of
    "application/pdf"->do
      fields<-objectFields["pages","planned_decoded_bytes"]info
      descriptors<-field fields "pages"::IO[Value]
      protocol(not(null descriptors)&&length descriptors<=run.limits.pdfPages)
      work<-sum <$>forM descriptors(\descriptor->do
        page<-objectFields["width","height","scale"]descriptor
        width<-field page "width"::IO Integer; height<-field page "height"::IO Integer
        scale<-field page "scale"::IO Double
        protocol(width>0&&height>0&&width<=fromIntegral run.limits.longestSide&&height<=fromIntegral run.limits.longestSide
          &&width*height<=fromIntegral run.limits.pagePixels&&not(isNaN scale||isInfinite scale)&&scale>0&&scale<=2)
        pure(width*height*4))
      planned<-field fields "planned_decoded_bytes"::IO Integer
      protocol(planned==work&&work<=fromIntegral run.limits.pdfDecodedBytes)
      let count=length descriptors
      pure(("thumbnail",0,"image/png"):[(variant,page,"image/png")|page<-[1..count],variant<-["pdf_page","pdf_thumbnail"]],count)
    "text/html"->objectFields[]info >>pure([("styled_html_preview",0,"text/html")],0)
    media | elem media ["image/png","image/jpeg","image/webp","video/mp4"]->do
      image<-objectFields["width","height"]info
      width<-field image "width"::IO Integer; height<-field image "height"::IO Integer
      protocol(width>0&&height>0&&width*height<=fromIntegral run.limits.imagePixels)
      when(media=="video/mp4")(protocol(width<=4096&&height<=4096&&width*height<=8294400))
      pure([("image",0,"image/png"),("thumbnail",0,"image/png")],0)
    _->throwIO(ProtocolFailure "unsupported")
  protocol(outputs==fromIntegral(length shape)&&outputs<=401)
  pure(Inspection value planHash shape pages)

data ParsedResponse=ParsedResponse
  { inspection :: !Inspection, descriptors :: ![Value], metadata :: !PreviewMetadata }
validateResponse :: PreviewRun -> Text -> Maybe Inspection -> Value -> IO ParsedResponse
validateResponse run operation previous value=do
  case value of
    Object fields | KM.lookup "status" fields==Just(String "error")->do
      err<-objectFields["version","status","code"]value
      version<-field err "version"::IO Integer
      code<-field err "code"
      protocol(version==1&&T.length code<=80&&not(T.null code)&&T.all(\c->(c>='a'&&c<='z')||(c>='0'&&c<='9')||c=='_')code)
      throwIO(WorkerFailure code)
    _->pure()
  header<-objectFields["version","status","operation","media_type","original_bytes","original_sha256","policy_version",
    "plan","plan_sha256","work","outputs","runtime","elapsed_ms","launcher"]value
  version<-field header "version"::IO Integer; status<-field header "status"::IO Text
  actualOperation<-field header "operation"; media<-field header "media_type"
  originalBytes<-field header "original_bytes"::IO Integer
  originalHash<-field header "original_sha256"; policy<-field header "policy_version"
  workerElapsed<-field header "elapsed_ms"::IO Integer
  protocol(version==1&&status=="ok"&&actualOperation==operation&&media==run.mediaType&&originalBytes==fromIntegral run.rawBytes
    &&originalHash==run.rawSha256&&policy==run.policyVersion&&workerElapsed>=0&&workerElapsed<=120000)
  plan<-field header "plan"; planHash<-field header "plan_sha256"
  inspection<-validatePlan run plan planHash
  forM_ previous(\prior->protocol(prior==inspection))
  records<-field header "outputs"::IO[Value]
  protocol(length records==if operation=="inspect" then 0 else length inspection.outputShape)
  work<-field header "work" >>=objectFields["decoded_pixels","pdf_decoded_bytes","animated_first_frame"]
  pixels<-field work "decoded_pixels"::IO Int64; decodedBytes<-field work "pdf_decoded_bytes"::IO Int64
  animated<-field work "animated_first_frame"
  protocol(pixels>=0&&pixels<=run.limits.imagePixels&&decodedBytes>=0&&decodedBytes<=run.limits.pdfDecodedBytes)
  when(operation=="inspect")(protocol(pixels==0&&decodedBytes==0&&not animated))
  runtime<-field header "runtime" >>=objectFields["uid","cap_eff","cap_bnd","no_new_privs","seccomp","memory_max","pids_max","root_read_only","interfaces","pdfium_flags","pdfium_version"]
  uid<-field runtime "uid"::IO Integer; caps<-field runtime "cap_eff"::IO Integer; bound<-field runtime "cap_bnd"::IO Integer
  privileges<-field runtime "no_new_privs"::IO Integer; seccomp<-field runtime "seccomp"::IO Integer
  memory<-field runtime "memory_max"::IO Integer; pids<-field runtime "pids_max"::IO Integer
  readOnly<-field runtime "root_read_only"; interfaces<-field runtime "interfaces"::IO[Text]
  flags<-field runtime "pdfium_flags"::IO[Value]; pdfVersion<-field runtime "pdfium_version"::IO Text
  protocol(uid==65534&&caps==0&&bound==0&&privileges==1&&seccomp==2&&memory==536870912&&pids==64
    &&readOnly&&interfaces==["lo"]&&null flags&&not(T.null pdfVersion)&&T.length pdfVersion<=128)
  receipt<-field header "launcher" >>=objectFields["owner_id","elapsed_ms","remaining_ms"]
  owner<-field receipt "owner_id"; elapsed<-field receipt "elapsed_ms"::IO Integer
  remaining<-field receipt "remaining_ms"::IO Integer
  protocol(owner==UUID.toText run.id&&elapsed>=0&&remaining>0&&elapsed+remaining<=120001)
  let metadata=PreviewMetadata planHash run.rawSha256(payloadSha256(canonicalPreviewLimits run.limits))
        run.policyVersion run.imageId(length inspection.outputShape)inspection.pageCount pixels decodedBytes animated
  pure(ParsedResponse inspection records metadata)

readOutputs :: Handle -> ParsedResponse -> Int64 -> IO[Document.Derivative]
readOutputs handle response limit=go response.descriptors 0 Set.empty []
  where
    go [] _ seen outputs=do
      protocol(Set.size seen==length response.inspection.outputShape)
      end<-BS.hGetSome handle 1
      ensure(BS.null end)"preview_trailing_output"
      pure(reverse outputs)
    go(record:rest) used seen outputs=do
      fields<-objectFields["variant","page","media_type","bytes","sha256"]record
      variant<-field fields "variant"; pageNumber<-field fields "page"::IO Integer; media<-field fields "media_type"
      count<-field fields "bytes"::IO Integer; hash<-field fields "sha256"
      protocol(pageNumber>=0&&pageNumber<=200)
      let page=fromIntegral pageNumber
      protocol(elem(variant,page,media)response.inspection.outputShape&&not(Set.member(variant,page)seen)&&validHash hash)
      ensure(count>0&&count<=fromIntegral(limit-used))"preview_output_limit"
      actual<-decodeLength <$>readExactly handle 8
      protocol(actual==count)
      bytes<-readExactly handle(fromIntegral count)
      protocol(payloadSha256 bytes==hash)
      when(media=="image/png")(protocol(BS.take 8 bytes==BS.pack[137,80,78,71,13,10,26,10]))
      let mapped=case variant of "image"->"page";"pdf_page"->"page";"pdf_thumbnail"->"thumbnail";other->other
      go rest(used+fromIntegral count)(Set.insert(variant,page)seen)(Document.Derivative mapped page media bytes:outputs)

data NativeChild=NativeChild
  { process :: !ProcessHandle, input :: !Handle, output :: !Handle, errors :: !Handle
  , waiter :: !(IORef(Maybe(Async ExitCode))), stderrDrain :: !(IORef(Maybe(Async ())))
  , writer :: !(IORef(Maybe(Async ()))), auxiliary :: !(IORef[NativeChild])
  , cleanupState :: !(MVar Bool), initialization :: !(MVar ())
  , nativePid :: !(IORef(Maybe Int64))
  , protocolTask :: !(IORef(Maybe(Async ()))), signals :: !(IORef[Async ()])
  }

type NativeRegistry=IORef(Map.Map UUID.UUID [NativeChild])
newNativeRegistry :: IO NativeRegistry
newNativeRegistry=newIORef Map.empty
forgetNativeOwner :: NativeRegistry -> UUID.UUID -> IO()
forgetNativeOwner registry owner=atomicModifyIORef' registry(\current->(Map.delete owner current,()))

-- A task cannot touch a pipe or callback until its exact owner contains the
-- Async handle. Acquisition stays masked; the gate opens only after publication.
registeredAsync :: IORef(Maybe(Async a)) -> IO a -> IO(Async a)
registeredAsync slot action=mask $ \restore->do
  gate<-newEmptyMVar
  child<-async(takeMVar gate >>restore action)
  writeIORef slot(Just child)
  putMVar gate()
  pure child

-- Register the raw process and every pipe before any interruptible constructor,
-- callback or restored cancellation. Failed construction remains retryable using
-- these exact handles; a callback is never the sole ownership handoff.
initializeNative :: NativeChild -> IO()
initializeNative child=withMVar child.initialization $ \_->mask_ $ do
  identity<-readIORef child.nativePid
  case identity of
    Just _->pure()
    Nothing->do
      pid<-snapshotNativePid child.process
      ensure(maybe False(>0)pid)"preview_native_identity"
      writeIORef child.nativePid pid
  reaper<-readIORef child.waiter
  case reaper of
    Just _->pure()
    Nothing->void(registeredAsync child.waiter(waitForProcess child.process))
  drain<-readIORef child.stderrDrain
  case drain of
    Just _->pure()
    Nothing->void(registeredAsync child.stderrDrain(discard child.errors))
  where
    discard handle=let loop=BS.hGetSome handle 4096 >>= \bytes->unless(BS.null bytes)loop in loop
nativeWaiter :: NativeChild -> IO(Async ExitCode)
nativeWaiter child=initializeNative child >>readIORef child.waiter >>=maybe
  (throwIO(ProtocolFailure "preview_native_identity"))pure

-- process getPid omits OpenExtHandle on Windows. Keep the job object and read
-- the actual already-owned process handle while open, before its sole reaper.
-- This immutable identity is diagnostic only; termination always uses handles.
snapshotNativePid :: ProcessHandle -> IO(Maybe Int64)
#if defined(mingw32_HOST_OS)
snapshotNativePid process=withProcessHandle process $ \state->case state of
  OpenHandle handle->Just . fromIntegral <$>Win32.getProcessId handle
  OpenExtHandle handle _->Just . fromIntegral <$>Win32.getProcessId handle
  ClosedHandle _->pure Nothing
#else
snapshotNativePid process=fmap fromIntegral <$>getPid process
#endif
data PhysicalDisposition=PhysicalDisposition
  { nativeExited :: !Bool, cleanupVerified :: !Bool, creationUncertain :: !Bool }
  deriving stock(Show,Eq)
data VerifiedGeneration=VerifiedGeneration
  { outputs :: ![Document.Derivative], metadata :: !PreviewMetadata }
data LauncherOutcome=LauncherOutcome
  { result :: !(Either Text VerifiedGeneration), physical :: !PhysicalDisposition, cause :: !(Maybe SomeException) }
data LauncherHooks=LauncherHooks
  { registerNative :: NativeChild -> IO(), nativeStarted :: Int64 -> IO()
  , inspectedPlan :: Inspection -> IO()
  , admittedPlan :: Inspection -> IO()
  , startingRender :: IO()
  , settled :: LauncherOutcome -> IO()
  }

-- Retain precisely these handles until their single reapers and pipe users join.
-- A caller may keep an unclean child in its bounded owner registry and retry its
-- cleanup; neither a PID nor a timeout substitutes for this capability.
spawnNative :: NativeRegistry -> PreviewConfig -> PreviewRun -> [String] -> IO NativeChild
spawnNative registry config run extra=mask $ \_ ->do
  path<-maybe(throwIO(ProtocolFailure "renderer_unavailable"))pure config.launcherPath
  inherited<-getEnvironment
  let allowed=["PATH","SYSTEMROOT","WINDIR","COMSPEC","TEMP","TMP","TMPDIR","HOME","USERPROFILE","DOCKER_HOST","DOCKER_CONTEXT","DOCKER_CONFIG","PATHEXT"]
      environment=filter(\(key,_)->elem(map toUpper key)allowed)inherited
        <>[("PYTHONDONTWRITEBYTECODE","1"),("PYTHONUNBUFFERED","1")]
      command=(proc config.pythonExecutable(path:["--image",T.unpack run.imageId,"--owner-id",UUID.toString run.id]<>extra))
        {std_in=CreatePipe,std_out=CreatePipe,std_err=CreatePipe,env=Just environment
        ,create_group=os/="mingw32",use_process_jobs=os=="mingw32"}
  (Just stdinHandle,Just stdoutHandle,Just stderrHandle,handle)<-createProcess command
  reaper<-newIORef Nothing
  drain<-newIORef Nothing
  writer<-newIORef Nothing
  auxiliary<-newIORef []
  cleanupState<-newMVar False
  initialization<-newMVar ()
  identity<-newIORef Nothing
  protocolTask<-newIORef Nothing
  signals<-newIORef []
  let child=NativeChild handle stdinHandle stdoutHandle stderrHandle reaper drain writer auxiliary cleanupState initialization identity protocolTask signals
  atomicModifyIORef' registry(\current->(Map.insertWith (<>) run.id [child] current,()))
  initializeNative child
  pure child

ignoreSynchronous :: IO() -> IO()
ignoreSynchronous action=action `catch` \(_::IOException)->pure()
joined :: Deadline -> Async a -> IO Bool
joined deadline child=do
  left<-remainingMicros deadline
  if left<=0 then pure False else timeout left(waitCatch child) >>=pure . maybe False(const True)

stopNative :: Deadline -> NativeChild -> IO Bool
stopNative deadline child=do
  left<-remainingMicros deadline
  if left<=0 then pure False else timeout left(stopOwned child) >>=pure . maybe False id
  where
    stopOwned owned=modifyMVar owned.cleanupState $ \_->do
      -- Re-evaluate on every call: cleanup may have registered a helper after a
      -- previous successful main-process join.
      reaper<-nativeWaiter owned
      poll reaper >>= \case
        Just(Right _)->pure()
        _->do
          when(os/="mingw32")(ignoreSynchronous(interruptProcessGroupOf owned.process))
          ignoreSynchronous(terminateProcess owned.process)
      remaining<-remainingMicros deadline
      nativeResult<-if remaining<=0 then pure Nothing else timeout remaining(waitCatch reaper)
      let exited=case nativeResult of Just(Right _)->True; _->False
      if not exited then pure(False,False) else do
        -- Stop the actual native job BEFORE signalling any task that may hold
        -- a Windows pipe read/write. Synchronous cancel-and-wait here could
        -- otherwise prevent us from ever reaching terminateProcess.
        protocolReader<-readIORef owned.protocolTask
        forM_ protocolReader $ \task->poll task >>= \case
          Nothing->do
            gate<-newEmptyMVar
            signal<-async(takeMVar gate >>throwTo(asyncThreadId task)AsyncCancelled)
            atomicModifyIORef' owned.signals(\current->(signal:current,()))
            putMVar gate()
          Just _->pure()
        protocolJoined<-maybe(pure True)(joined deadline)protocolReader
        signals<-readIORef owned.signals
        signalsJoined<-and <$>mapM(joined deadline)signals
        writes<-readIORef owned.writer
        writerJoined<-maybe(pure True)(joined deadline)writes
        -- EOF must settle the real pipe consumer; no unbounded cancel-and-wait.
        drain<-readIORef owned.stderrDrain
        stderrJoined<-maybe(pure False)(joined deadline)drain
        auxiliary<-readIORef owned.auxiliary
        auxiliariesJoined<-and <$>mapM(stopNative deadline)auxiliary
        if not(protocolJoined&&signalsJoined&&writerJoined&&stderrJoined&&auxiliariesJoined) then pure(False,False) else do
          left<-remainingMicros deadline
          closed<-if left<=0 then pure Nothing else timeout left $mapM_(ignoreSynchronous . hClose)[owned.input,owned.output,owned.errors]
          let complete=closed/=Nothing
          pure(complete,complete)

requestHeader :: PreviewRun -> Value
requestHeader run=object["version" .= (1::Int),"operation" .= ("inspect"::Text),"media_type" .= run.mediaType,
  "original_bytes" .= run.rawBytes,"original_sha256" .= run.rawSha256,"policy_version" .= run.policyVersion,"limits" .= run.limits]

knownSettledError :: Text -> Bool
knownSettledError code=elem code ["worker_crashed","confinement_unavailable","invalid_or_exhausted_input","unsupported","cancelled","deadline_exceeded"]
  ||any(`T.isPrefixOf`code)["image_","video_","pdf_","html_","css_","dom_","output_","original_","plan_","policy_","limits_","embedded_","version_","operation_","request_","response_","header_","duplicate_","trailing_","truncated_","json_","work_receipt_"]

-- A separate cleanup helper is registered under the same actual native owner.
-- Its current-absence result never settles an unknown daemon create.
cleanupOwner :: NativeRegistry -> PreviewConfig -> PreviewRun -> Deadline -> NativeChild -> IO Bool
cleanupOwner registry config run deadline owner=mask $ \restore->do
  left<-remainingMicros deadline
  if left<=0 then pure False else do
    helper<-within deadline(spawnNative registry config run ["--cleanup"])
    modifyIORef' owner.auxiliary(helper:)
    stdoutDrain<-registeredAsync helper.writer
      (let loop=BS.hGetSome helper.output 4096 >>= \bytes->unless(BS.null bytes)loop in loop)
    reaper<-nativeWaiter helper
    outcome<-try @SomeException(restore(within deadline(waitCatch reaper)))
    finalStdoutJoined<-joined deadline stdoutDrain
    nativeJoined<-stopNative deadline helper
    pure(case outcome of Right(Right ExitSuccess)->nativeJoined&&finalStdoutJoined; _->False)

launchGeneration :: NativeRegistry -> PreviewConfig -> PreviewRun -> ByteString -> Deadline -> LauncherHooks -> IO LauncherOutcome
launchGeneration registry config run original deadline hooks=mask $ \restore->do
  protocol(validPreviewConfig config&&run.policyVersion==previewPolicyVersion&&BS.length original==fromIntegral run.rawBytes
    &&payloadSha256 original==run.rawSha256&&run.limits==config.limits)
  child<-within(workDeadline deadline)(spawnNative registry config run [])
  -- Before input or ACK can reach the child, registration owns every native handle.
  stageSettled<-newIORef True
  let observeFailure exception=case fromException exception of
        Just(ProtocolFailure code)->code
        Nothing->case fromException exception of Just(WorkerFailure code)->code; Nothing->"worker_failed"
      action=do
        pid<-readIORef child.nativePid >>=maybe(throwIO(ProtocolFailure "preview_native_identity"))pure
        hooks.nativeStarted pid
        writeIORef stageSettled False
        writing<-registeredAsync child.writer
          (writeHeader child.input(requestHeader run)>>BS.hPut child.input original>>hFlush child.input)
        first<-readHeader child.output >>=validateResponse run "inspect" Nothing
        wait writing
        writeIORef stageSettled True
        hooks.inspectedPlan first.inspection
        hooks.admittedPlan first.inspection
        hooks.startingRender
        writeIORef stageSettled False
        writeHeader child.input(object["render_plan_sha256" .= first.inspection.planSha256])
        hClose child.input
        final<-readHeader child.output >>=validateResponse run "render"(Just first.inspection)
        -- Qualified launcher adds this bound receipt only after physical cleanup.
        -- Payload integrity still has to pass before anything can be published.
        writeIORef stageSettled True
        outputs<-readOutputs child.output final run.limits.outputBytes
        reaper<-nativeWaiter child
        exit<-wait reaper
        ensure(exit==ExitSuccess)"worker_failed"
        pure(VerifiedGeneration outputs final.metadata)
  protocolResult<-newEmptyMVar
  operation<-registeredAsync child.protocolTask $ do
    result<-try @SomeException(hooks.registerNative child >>restore action)
    putMVar protocolResult result
  -- Only the owned operation task touches protocol pipes. The deadline caller
  -- waits on an interruptible Async/MVar and can always reach native teardown.
  observed<-try @SomeException $ restore $ within(workDeadline deadline) $ do
    wait operation
    takeMVar protocolResult
  let attempted=either Left id observed
  case attempted of
    Left exception | Just(WorkerFailure code)<-fromException exception->when(knownSettledError code)(writeIORef stageSettled True)
    _->pure()
  mainJoined<-stopNative deadline child
  quiescent<-readIORef stageSettled
  cleanup<-case attempted of
    Right _->pure mainJoined
    Left _->if mainJoined then cleanupOwner registry config run deadline child else pure False
  -- Include helpers acquired after the first join, and re-evaluate all pipes.
  owned<-Map.findWithDefault [] run.id <$>readIORef registry
  nativeJoined<-and <$>mapM(stopNative deadline)owned
  let physical=PhysicalDisposition nativeJoined cleanup(not quiescent)
      outcome=LauncherOutcome(either(Left . observeFailure)Right attempted)physical(either Just(const Nothing)attempted)
  settlement<-try @SomeException(hooks.settled outcome)
  case attempted of
    Left exception | Just _<-(fromException exception::Maybe SomeAsyncException)->throwIO exception
    _->either throwIO(const(pure outcome))settlement
