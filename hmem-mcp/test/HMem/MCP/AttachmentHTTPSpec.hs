{-# LANGUAGE ForeignFunctionInterface #-}
module HMem.MCP.AttachmentHTTPSpec (spec) where

import Control.Concurrent (threadDelay, throwTo)
import Control.Concurrent.Async (withAsync, wait, asyncThreadId)
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception hiding (handle)
import Control.Monad (unless, when, void, forM_)
import Data.Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as B8
import Data.ByteString.Lazy qualified as BL
import Data.IORef
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time (UTCTime)
import Data.UUID (UUID)
import Data.UUID.V4 qualified as UUID
import Foreign (Ptr, alloca, nullPtr, peek, poke)
import Foreign.C (CString,CInt(..))
import GHC.Clock (getMonotonicTimeNSec)
import Network.HTTP.Client qualified as H
import Network.HTTP.Types
import Network.Wai qualified as W
import Network.Wai.Handler.Warp (testWithApplication)
import System.Directory
import System.Environment (getExecutablePath)
import System.FilePath
import System.Info qualified as Info
import System.Timeout (timeout)
import Test.Hspec
import HMem.BotIdentity (BotIdentity,mkBotIdentity)
import HMem.Document.Codec (payloadSha256)
import HMem.Document.Types qualified as Doc
import HMem.MCP.Attachment
import HMem.MCP.AttachmentHTTP qualified as HTTP
import HMem.MCP.Server (MCPContext(..),handleStdioLineWithAttachmentScope)
import HMem.MCP.SessionFile qualified as File
import HMem.MCP.SessionProcess qualified as P

foreign import ccall safe "hmem_session_process_spawn_kind"
  spawnKind :: CString -> CInt -> Ptr (Ptr ()) -> IO CInt

spec :: Spec
spec = describe "Owned attachment HTTP" $ do
  it "kills and joins both native pumps before stalled headers, body or pipe backpressure can outlive the deadline" $
    forM_ ["headers","body","input","output" :: String] $ \mode -> withScope $ \scope -> do
      entered <- newIORef False
      released <- newIORef False
      release <- newEmptyMVar
      baseline <- P.nativeProcessCount
      let pause = do
            writeIORef entered True
            void (timeout 2000000 (readMVar release))
            writeIORef released True
          app _ respond = case mode of
            "body" -> respond (W.responseStream status200 [("Content-Length","4")] $ \send flush -> do
              send "go"; flush; pause; send "od")
            "output" -> respond (W.responseLBS status200 [] (BL.replicate (8*1048576) 120))
            _ -> pause >> respond (W.responseLBS status200 [] "good")
      withServer app $ \_ base -> flip finally (void (tryPutMVar release ())) $ do
        request <- H.parseRequest base
        deadline <- HTTP.newDeadline 500000 100000
        start <- getMonotonicTimeNSec
        result <- try @HTTP.TransportFault (HTTP.exchange scope deadline
          request {H.method=if mode=="input" then "POST" else "GET"}
          (if mode=="input" then replicate 25 (BS.replicate 1048576 120) else []) (100*1048576)
          (\_ _ -> pure ()) (\_ -> when (mode=="output") pause) (pure ()))
        end <- getMonotonicTimeNSec
        result `shouldBe` Left HTTP.TransportDeadline
        readIORef entered `shouldReturn` True
        readIORef released `shouldReturn` False
        (fromIntegral (end-start)/1000000 :: Double) `shouldSatisfy` (<1500)
        P.nativeProcessCount `shouldReturn` baseline
        P.processMetrics scope.processes `shouldReturn` (0,0,False)

  it "returns an unread early rejection while a large upload writer is backpressured" $ withScope $ \scope -> do
    baseline <- P.nativeProcessCount
    let bytes = encode (object ["error" .= ("forbidden" :: Text)])
        app request respond = do
          lookup "Expect" (W.requestHeaders request) `shouldBe` Just "100-continue"
          respond (W.responseLBS status403 [("Content-Type","application/json")] bytes)
    withServer app $ \_ base -> do
      request <- H.parseRequest base
      status <- newIORef 0
      body <- newIORef []
      completed <- newIORef False
      deadline <- HTTP.newDeadline 2000000 200000
      HTTP.exchange scope deadline request {H.method="POST"} (replicate 25 (BS.replicate 1048576 120)) 0
        (\code _ -> writeIORef status code) (\chunk -> modifyIORef' body (chunk:)) (writeIORef completed True)
      readIORef status `shouldReturn` 403
      (BL.fromStrict . BS.concat . reverse <$> readIORef body) `shouldReturn` bytes
      readIORef completed `shouldReturn` True
      P.nativeProcessCount `shouldReturn` baseline
      P.processMetrics scope.processes `shouldReturn` (0,0,False)

  it "keeps authoritative 403 and 409 error bodies independent of a zero or tiny successful representation bound" $
    withScope $ \scope -> forM_ [0,1] $ \maximum -> forM_ [status403,status409] $ \status -> do
      let bytes = encode (object ["error" .= (if status==status403 then "forbidden" else "document_request_conflict" :: Text)])
      withServer (\_ respond -> respond (W.responseLBS status [("Content-Type","application/json")] bytes)) $ \_ base -> do
        request <- H.parseRequest base
        seen <- newIORef []
        code <- newIORef 0
        deadline <- HTTP.newDeadline 2000000 200000
        HTTP.exchange scope deadline request [] maximum (\value _ -> writeIORef code value)
          (\chunk -> modifyIORef' seen (chunk:)) (pure ())
        readIORef code `shouldReturn` statusCode status
        (BL.fromStrict . BS.concat . reverse <$> readIORef seen) `shouldReturn` bytes
        P.processMetrics scope.processes `shouldReturn` (0,0,False)

  it "records a complete terminal acknowledgement before a blocked pre-EOF callback and still retires physical ownership" $
    withScope $ \scope -> do
      acknowledged <- newIORef False
      release <- newEmptyMVar
      baseline <- P.nativeProcessCount
      withServer (\_ respond -> respond (W.responseLBS status200 [] "{\"accepted\":true}")) $ \_ base ->
        flip finally (void (tryPutMVar release ())) $ do
          request <- H.parseRequest base
          deadline <- HTTP.newDeadline 500000 100000
          result <- try @HTTP.TransportFault (HTTP.exchange scope deadline request [] 1024
            (\_ _ -> pure ()) (\_ -> pure ()) (writeIORef acknowledged True >> readMVar release))
          result `shouldBe` Left HTTP.TransportDeadline
          readIORef acknowledged `shouldReturn` True
          P.nativeProcessCount `shouldReturn` baseline
          P.processMetrics scope.processes `shouldReturn` (0,0,False)

  it "preserves a validated flat upload receipt when retirement is held and refuses context replacement" $
    withFiles $ \directory -> withHeldScope $ \scope entered release -> do
      let source = directory </> "notes.txt"
      BS.writeFile source "good"
      fileBaseline <- File.openSourceCount
      withServer uploadApp $ \manager base ->
        withAsync (executeAttachmentWith scope defaultAttachmentOptions {operationMicros=500000}
          manager base Nothing identity [] (Upload workspace task source Doc.Source requestId Nothing Nothing Nothing)) $ \worker ->
          flip finally (void (tryPutMVar release ())) $ do
            await entered
            result <- bounded (wait worker)
            errorCode result `shouldSatisfy` (`elem` [Just "attachment_cleanup_timeout",Just "attachment_cleanup_failed"])
            field ["structuredContent","error","receipt","attachment_id"] result `shouldBe` Just (toJSON document)
            field ["structuredContent","error","receipt","publication_confirmed"] result `shouldBe` Just (Bool True)
            atomically (P.scopePoisoned scope.processes) `shouldReturn` True
            File.openSourceCount `shouldReturn` fileBaseline
            assertContextRefused scope manager base
            void (tryPutMVar release ())
            awaitEmpty scope

  it "poisons admission when a second cancellation interrupts cleanup, without discarding the retained lease" $
    withHeldScope $ \scope entered release ->
      withServer (\_ respond -> respond (W.responseLBS status200 [] "{\"accepted\":true}")) $ \manager base -> do
        request <- H.parseRequest base
        deadline <- HTTP.newDeadline 10000000 1000000
        known <- newIORef False
        withAsync (try @SomeException (HTTP.exchange scope deadline request [] 1024
          (\_ _ -> pure ()) (\_ -> pure ()) (writeIORef known True))) $ \worker ->
          flip finally (void (tryPutMVar release ())) $ do
            await entered
            readIORef known `shouldReturn` True
            throwTo (asyncThreadId worker) ThreadKilled
            bounded (atomically (P.attachmentCancellationRequested scope.processes >>= check))
            throwTo (asyncThreadId worker) UserInterrupt
            result <- bounded (wait worker)
            result `shouldSatisfy` either (const True) (const False)
            atomically (P.scopePoisoned scope.processes) `shouldReturn` True
            (leases,_,_) <- P.processMetrics scope.processes
            leases `shouldBe` 1
            assertContextRefused scope manager base
            refused <- try @HTTP.TransportFault (HTTP.exchange scope deadline request [] 1024
              (\_ _ -> pure ()) (\_ -> pure ()) (pure ()))
            refused `shouldBe` Left HTTP.TransportOwnership
            void (tryPutMVar release ())
            awaitEmpty scope

  it "never dispatches request input after a late native spawn crosses its cutoff" $ withScope $ \scope -> do
    spawned <- newEmptyMVar
    release <- newEmptyMVar
    dispatched <- newIORef False
    now <- getMonotonicTimeNSec
    withAsync (try @P.ProcessFault (P.runAttachmentExchange scope.processes scope.executable
      (now+100000000) (now+300000000) (\_ -> putMVar spawned () >> readMVar release)
      (\_ -> writeIORef dispatched True) (\channel -> channel.receiveAttachmentFrame))) $ \worker ->
        flip finally (void (tryPutMVar release ())) $ do
          await spawned
          result <- bounded (wait worker)
          result `shouldBe` Left P.ProcessOwnership
          readIORef dispatched `shouldReturn` False
          atomically (P.scopePoisoned scope.processes) `shouldReturn` True
          void (tryPutMVar release ())
          awaitEmpty scope
          readIORef dispatched `shouldReturn` False

  it "rejects malformed, oversized and truncated private request streams with joined pumps" $ withScope $ \scope ->
    forM_ [0,1,2 :: Int] $ \mode -> do
      now <- getMonotonicTimeNSec
      let input :: P.AttachmentPipes -> IO ()
          input channel = case mode of
            0 -> channel.sendAttachmentFrame (BS.cons 0 "not JSON")
            1 -> channel.sendAttachmentFrame (BS.cons 0 (BS.replicate 262145 120))
            _ -> pure ()
          output :: P.AttachmentPipes -> IO BS.ByteString
          output channel = do
            response <- channel.receiveAttachmentFrame
            BS.take 1 response `shouldBe` BS.singleton 6
            eitherDecodeStrict' (BS.drop 1 response) `shouldBe` Right (object ["code" .= ("protocol" :: Text)])
            channel.expectAttachmentEOF
            pure BS.empty
      result <- P.runAttachmentExchange scope.processes scope.executable (now+2000000000) (now+2500000000)
        (const (pure ())) input output
      result `shouldBe` Right BS.empty
      P.processMetrics scope.processes `shouldReturn` (0,0,False)

  it "rejects an unknown native helper kind before allocating a process" $ do
    executable <- helperExecutable
    baseline <- P.nativeProcessCount
    (code,pointer) <- BS.useAsCString (TE.encodeUtf8 (T.pack executable)) $ \path -> alloca $ \out -> do
      poke out nullPtr
      code <- spawnKind path 2 out
      pointer <- peek out
      pure (code,pointer)
    code `shouldBe` 0
    pointer `shouldBe` nullPtr
    P.nativeProcessCount `shouldReturn` baseline

helperExecutable :: IO FilePath
helperExecutable = do
  executable <- getExecutablePath
  canonicalizePath (takeDirectory (takeDirectory executable) </> "hmem-mcp" </>
    ("hmem-mcp" <> if Info.os=="mingw32" then ".exe" else ""))
withScope :: (HTTP.AttachmentScope -> IO a) -> IO a
withScope action = helperExecutable >>= \executable -> bracket (HTTP.newAttachmentScope executable) closeScope action
closeScope :: HTTP.AttachmentScope -> IO ()
closeScope scope = HTTP.closeAttachmentScope scope 5000000 >>= \closed ->
  unless closed (fail "Attachment test ownership did not retire")
withHeldScope :: (HTTP.AttachmentScope -> MVar () -> MVar () -> IO a) -> IO a
withHeldScope action = do
  entered <- newEmptyMVar
  release <- newEmptyMVar
  executable <- helperExecutable
  processes <- P.newProcessScopeWithReapHook (void (tryPutMVar entered ()) >> readMVar release)
  let scope = HTTP.AttachmentScope processes executable
  action scope entered release `finally` (void (tryPutMVar release ()) >> closeScope scope)
bounded :: IO a -> IO a
bounded action = timeout 5000000 action >>= maybe (fail "Attachment fixture exceeded five-second control bound") pure
await :: MVar a -> IO a
await = bounded . readMVar
awaitEmpty :: HTTP.AttachmentScope -> IO ()
awaitEmpty scope = bounded $ let loop = do
                                  (leases,pipes,_) <- P.processMetrics scope.processes
                                  if leases==0 && pipes==0 then pure () else threadDelay 1000 >> loop
                             in loop
withServer :: W.Application -> (H.Manager -> String -> IO a) -> IO a
withServer app action = testWithApplication (pure app) $ \port ->
  bracket (H.newManager H.defaultManagerSettings {H.managerIdleConnectionCount=0}) H.closeManager $ \manager ->
    action manager ("http://127.0.0.1:" <> show port)
withFiles :: (FilePath -> IO a) -> IO a
withFiles action = do
  root <- getTemporaryDirectory
  unique <- UUID.nextRandom
  bracket (let directory=root </> ("attachment-http-" <> show unique) in createDirectory directory >> canonicalizePath directory)
    removeDirectoryRecursive action

assertContextRefused :: HTTP.AttachmentScope -> H.Manager -> String -> IO ()
assertContextRefused scope manager base = do
  initialized <- newTVarIO True
  let previous = Just (MCPContext (Just workspace) identity)
  context <- newTVarIO previous
  response <- handleStdioLineWithAttachmentScope scope manager base Nothing initialized context
    (BL.toStrict (encode (object ["jsonrpc" .= ("2.0" :: Text),"id" .= (1::Int),"method" .= ("tools/call" :: Text),
      "params" .= object ["name" .= ("set_workspace" :: Text),"arguments" .= object ["workspace_id" .= task,"identity" .= identity]]])))
  fmap (field ["result","structuredContent","error","code"]) response `shouldBe` Just (Just (String "attachment_cleanup_pending"))
  readTVarIO context `shouldReturn` previous

uploadApp :: W.Application
uploadApp request respond = do
  body <- W.strictRequestBody request
  BL.toStrict body `shouldBe` "good"
  let metadata = maybe "" id (lookup "X-HMem-Document-Metadata" (W.requestHeaders request))
  value <- either fail pure (eitherDecodeStrict' metadata)
  manifest <- case field ["manifest"] value of
    Nothing -> fail "Missing fixture upload manifest"
    Just item -> case fromJSON item of Error message -> fail message; Success value -> pure (value :: Doc.OriginalManifest)
  let doc = Doc.Document document workspace payload manifest.filename manifest.title manifest.mediaType
        manifest.rawBytes manifest.rawSha256 (read "2026-01-01 00:00:00 UTC" :: UTCTime)
        (object ["kind" .= ("test" :: Text)]) "queued" Nothing
  respond (W.responseLBS status201 [("Content-Type","application/json")] (encode doc))

workspace,task,document,payload,requestId :: UUID
workspace=read "11111111-1111-4111-8111-111111111111"
task=read "33333333-3333-4333-8333-333333333333"
document=read "44444444-4444-4444-8444-444444444444"
payload=read "55555555-5555-4555-8555-555555555555"
requestId=read "88888888-8888-4888-8888-888888888888"
identity :: BotIdentity
identity=either (error . T.unpack) id (mkBotIdentity "Attachment protocol test" "test:fixture:default")
field :: [Text] -> Value -> Maybe Value
field [] value=Just value
field (key:rest) (Object fields)=KM.lookup (Key.fromText key) fields >>= field rest
field _ _=Nothing
errorCode :: Value -> Maybe Text
errorCode value=case field ["structuredContent","error","code"] value of
  Just (String code)->Just code
  _->Nothing
