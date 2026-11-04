module HMem.MCP.AttachmentSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (withAsync, wait, waitCatch, cancel)
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad (when, void, forM_, unless)
import Data.Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as B8
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Builder qualified as Builder
import Data.IORef
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time (UTCTime, getCurrentTime, diffUTCTime)
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUIDv4
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Types
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp (testWithApplication)
import System.Directory
import System.FilePath
import System.IO
import System.Environment (getExecutablePath, lookupEnv, setEnv, unsetEnv)
import System.Exit (ExitCode(..))
import System.Info qualified as Info
import System.Process
import System.Timeout (timeout)
import GHC.Clock (getMonotonicTimeNSec)
import Test.Hspec

import HMem.BotIdentity (BotIdentity, mkBotIdentity)
import HMem.DB.TestHarness (TestEnv(..), withEphemeralPg, withTestEnv, createTestWorkspace)
import HMem.DB.Task qualified as Tasks
import HMem.DB.Document qualified as Documents
import HMem.Config qualified as Config
import HMem.Types qualified as Types
import HMem.Document.Codec (payloadSha256)
import HMem.Document.Preview (previewPolicyVersion)
import HMem.Document.Types qualified as Doc
import HMem.MCP.Attachment
import HMem.MCP.AttachmentHTTP qualified as HTTP
import HMem.MCP.SessionProcess qualified as P
import HMem.MCP.SessionFile qualified as File
import HMem.MCP.Tools (handleToolCallWithAttachmentScope)
import HMem.Server.App (mkAppWithDocumentAuthority)
import HMem.Server.AccessTracker (newAccessTracker)
import HMem.Server.Document.Grant qualified as Grants
import HMem.Server.Document.PreviewOrigin (newPreviewApplication)
import HMem.Server.WebSocket (newWSState)

spec :: Spec
spec = aroundAll_ (withoutProvenance . withEphemeralPg) $ around withAttachmentScopeTest $ describe "Verified MCP attachments" $ do
  it "requires explicit styled associations/generations and rejects NUL before native paths" $ \scope -> do
    let common = ["workspace_id" .= workspace, "task_id" .= task, "attachment_id" .= documentId,
          "path" .= ("new.html" :: Text), "variant" .= ("styled_html_preview" :: Text), "generation_id" .= generation]
        parsed fields = parseAttachmentCall "attachment_download" (object fields)
    parsed common `shouldSatisfy` isLeft
    parsed (common <> ["role" .= ("source" :: Text)]) `shouldSatisfy` isLeft
    parsed (common <> ["role" .= ("source" :: Text), "association_id" .= association]) `shouldSatisfy` isRight
    parseAttachmentCall "attachment_download" (object ["workspace_id" .= workspace, "task_id" .= task,
      "attachment_id" .= documentId, "path" .= ("bad\0path" :: Text)]) `shouldSatisfy` isLeft
    parseAttachmentCall "attachment_download_link" (object ["workspace_id" .= workspace, "task_id" .= task,
      "attachment_id" .= documentId]) `shouldSatisfy` isLeft
    parseAttachmentCall "attachment_download" (object ["workspace_id" .= workspace, "task_id" .= task,
      "attachment_id" .= documentId, "path" .= ("page.png" :: Text), "variant" .= ("page" :: Text)]) `shouldSatisfy` isLeft
  it "encodes Unicode metadata as ASCII with non-BMP surrogate pairs and exact JSON identity" $ \scope -> do
    let value = object ["filename" .= ("é雪😀.txt" :: Text), "title" .= ("quote\"\\\r\n" :: Text)]
        bytes = asciiJSON value
    BS.all (<128) bytes `shouldBe` True
    BS.isInfixOf "\\ud83d\\ude00" bytes `shouldBe` True
    eitherDecodeStrict' bytes `shouldBe` Right value
  it "detects same-size source changes and replacement before HTTP without leaking native handles" $ \scope ->
    withFiles $ \directory -> do
      count <- newIORef (0 :: Int)
      let app _ respond = modifyIORef' count (+1) >> respond (json status500 Null)
          source = directory </> "source.txt"
          upload = Upload workspace task source Doc.Source requestId Nothing Nothing Nothing
      BS.writeFile source "first"
      before <- File.openSourceCount
      withServer app $ \manager base -> do
        changed <- executeAttachmentWith scope defaultAttachmentOptions
          {beforeUploadRecheck = BS.writeFile source "other"} manager base Nothing identity [] upload
        errorCode changed `shouldBe` Just "source_changed_or_unavailable"
        readIORef count `shouldReturn` 0
        BS.writeFile source "first"
        replaced <- executeAttachmentWith scope defaultAttachmentOptions
          {beforeUploadRecheck = renameFile source (directory </> "old.txt") >> BS.writeFile source "first"}
          manager base Nothing identity [] upload
        errorCode replaced `shouldBe` Just "source_changed_or_unavailable"
        readIORef count `shouldReturn` 0
      File.openSourceCount `shouldReturn` before
  it "rejects directories and over-limit regular files before dispatch" $ \scope ->
    withFiles $ \directory -> do
      count <- newIORef (0 :: Int)
      let app _ respond = modifyIORef' count (+1) >> respond (json status500 Null)
          sparse = directory </> "large.txt"
      withBinaryFile sparse WriteMode $ \handle -> hSetFileSize handle (toInteger Doc.maxOriginalBytes + 1)
      withServer app $ \manager base -> do
        invalid <- executeAttachment scope manager base Nothing identity [] (Upload workspace task directory Doc.Source requestId Nothing Nothing Nothing)
        errorCode invalid `shouldBe` Just "source_changed_or_unavailable"
        large <- executeAttachment scope manager base Nothing identity [] (Upload workspace task sparse Doc.Source requestId Nothing Nothing Nothing)
        errorCode large `shouldBe` Just "document_size_exceeded"
      readIORef count `shouldReturn` 0
  it "preserves the request, manifest and body across an uncertain response and caller retry" $ \scope ->
    withFiles $ \directory -> do
      calls <- newIORef ([] :: [(BS.ByteString,BL.ByteString,Maybe BS.ByteString)])
      let source = directory </> "notes.txt"
          bytes = TE.encodeUtf8 "same\r\nUnicode: 雪"
          app request respond
            | Wai.requestMethod request == methodPost = do
                body <- Wai.strictRequestBody request
                let metadata = maybe "" id (lookup "X-HMem-Document-Metadata" (Wai.requestHeaders request))
                modifyIORef' calls (<> [(metadata,body,lookup "X-Request-Id" (Wai.requestHeaders request))])
                seen <- readIORef calls
                if length seen == 1 then respond (json status503 (object ["error" .= ("document_unavailable" :: Text)]))
                else decodeMetadata metadata >>= respond . json status200 . toJSON . fromManifest
            | BS.isSuffixOf "/counts" (Wai.rawPathInfo request) = respond (json status200 (toJSON (Doc.DocumentCounts 1 0 0 0)))
            | otherwise = do
                seen <- readIORef calls
                let (metadata,_,_) = last seen
                manifest <- decodeMetadata metadata
                respond (json status200 (toJSON (Doc.DocumentPage [Doc.DocumentLink association task Doc.Source (fromManifest manifest) now] Nothing)))
      BS.writeFile source bytes
      withServer app $ \manager base -> do
        let upload = Upload workspace task source Doc.Source requestId (Just "notes-😀.txt") Nothing Nothing
        first <- executeAttachment scope manager base (Just "fixture-secret") identity [] upload
        errorCode first `shouldBe` Just "document_unavailable"
        field ["structuredContent","error","request_id"] first `shouldBe` Just (toJSON requestId)
        field ["structuredContent","error","manifest","raw_sha256"] first `shouldBe` Just (String (payloadSha256 bytes))
        second <- executeAttachment scope manager base (Just "fixture-secret") identity [] upload
        field ["structuredContent","attachment_id"] second `shouldBe` Just (toJSON documentId)
        field ["structuredContent","association_id"] second `shouldBe` Just (toJSON association)
      seen <- readIORef calls
      length seen `shouldBe` 2
      head seen `shouldBe` last seen
      let (metadata,body,request) = head seen
      BS.all (<128) metadata `shouldBe` True
      BL.toStrict body `shouldBe` bytes
      request `shouldBe` Just (TE.encodeUtf8 (UUID.toText requestId))
      BS.isInfixOf (TE.encodeUtf8 (T.pack directory)) metadata `shouldBe` False
  it "retains every acknowledged mutation when canonical counts fail or exceed the operation deadline" $ \scope ->
    withFiles $ \directory -> forM_ [False,True] $ \timed -> forM_ [0,1,2 :: Int] $ \operation -> do
      unblock <- newEmptyMVar
      released <- newIORef False
      countsEntered <- newIORef False
      sourceBaseline <- File.openSourceCount
      nativeBaseline <- P.nativeProcessCount
      putStrLn ("attachment counts case start: timed=" <> show timed <> ", mutation=" <> show operation)
      hFlush stdout
      let source = directory </> "notes.txt"
          call = case operation of
            0 -> Upload workspace task source Doc.Source requestId Nothing Nothing Nothing
            1 -> AddEvidence workspace task documentId requestId
            _ -> RemoveSource workspace task documentId
          app request respond
            | BS.isSuffixOf "/counts" (Wai.rawPathInfo request) = do
                writeIORef countsEntered True
                putStrLn "attachment counts fixture: waiting for count response"
                hFlush stdout
                when timed (void (timeout 2000000 (takeMVar unblock)))
                writeIORef released True
                putStrLn "attachment counts fixture: count response released"
                hFlush stdout
                respond (json status503 (object ["error" .= ("document_unavailable" :: Text)]))
            | Wai.requestMethod request == methodDelete = do
                putStrLn "attachment counts fixture: trusted removal acknowledgement"
                hFlush stdout
                respond (Wai.responseLBS status204 [] "")
            | Wai.requestMethod request == methodPost = do
                _ <- Wai.strictRequestBody request
                -- Both authoritative upload and promotion replies are flat Document.
                putStrLn "attachment counts fixture: trusted document acknowledgement"
                hFlush stdout
                respond (json status200 (toJSON (mockDocument "good")))
            | otherwise = do
                let role = if lookup "role" (Wai.queryString request) == Just (Just "evidence") then Doc.Evidence else Doc.Source
                respond (json status200 (toJSON (Doc.DocumentPage [Doc.DocumentLink association task role (mockDocument "good") now] Nothing)))
      BS.writeFile source "good"
      fixtureStart <- getMonotonicTimeNSec
      withServer app $ \manager base -> flip finally (void (tryPutMVar unblock ())) $ do
        clientStart <- getMonotonicTimeNSec
        result <- timeout 5000000 (executeAttachmentWith scope defaultAttachmentOptions {operationMicros=500000}
          manager base Nothing identity [] call) >>= maybe (fail "Mutation/counts call exceeded its five-second diagnostic guard") pure
        clientEnd <- getMonotonicTimeNSec
        let elapsedMs = fromIntegral (clientEnd-clientStart) / 1000000 :: Double
        putStrLn ("attachment counts result: timed=" <> show timed <> ", mutation=" <> show operation <> ", client_ms=" <> show elapsedMs)
        hFlush stdout
        File.openSourceCount `shouldReturn` sourceBaseline
        P.nativeProcessCount `shouldReturn` nativeBaseline
        P.processMetrics scope.processes `shouldReturn` (0,0,False)
        when timed $ do
          readIORef countsEntered `shouldReturn` True
          readIORef released `shouldReturn` False
          elapsedMs `shouldSatisfy` (<1500)
        field ["isError"] result `shouldBe` Nothing
        field ["structuredContent","attachment_id"] result `shouldBe` Just (toJSON documentId)
        field ["structuredContent","publication_confirmed"] result `shouldBe` Just (Bool True)
        field ["structuredContent","counts"] result `shouldBe` Just Null
        field ["structuredContent","counts_status"] result `shouldBe` Just (String "unavailable")
        field ["structuredContent","next_request","name"] result `shouldBe` Just (String "attachment_list")
        when (operation < 2) $ field ["structuredContent","request_id"] result `shouldBe` Just (toJSON requestId)
        when timed $ field ["structuredContent","follow_up_error","code"] result `shouldBe` Just (String "attachment_timeout")
      fixtureEnd <- getMonotonicTimeNSec
      putStrLn ("attachment counts fixture shutdown complete: timed=" <> show timed <> ", mutation=" <> show operation
        <> ", whole_fixture_ms=" <> show (fromIntegral (fixtureEnd-fixtureStart) / 1000000 :: Double))
      hFlush stdout
  it "reports late observed source change with the known durable receipt even when counts would fail" $ \scope ->
    withFiles $ \directory -> do
      let source = directory </> "notes.txt"
          app request respond
            | Wai.requestMethod request == methodPost = do
                _ <- Wai.strictRequestBody request
                manifest <- decodeMetadata (maybe "" id (lookup "X-HMem-Document-Metadata" (Wai.requestHeaders request)))
                BS.writeFile source "other"
                respond (json status201 (toJSON (fromManifest manifest)))
            | otherwise = respond (json status503 Null)
      BS.writeFile source "first"
      withServer app $ \manager base -> do
        result <- executeAttachment scope manager base Nothing identity [] (Upload workspace task source Doc.Source requestId Nothing Nothing Nothing)
        errorCode result `shouldBe` Just "source_changed_after_publication"
        field ["structuredContent","error","receipt","attachment_id"] result `shouldBe` Just (toJSON documentId)
        field ["structuredContent","error","manifest","raw_sha256"] result `shouldBe` Just (String (payloadSha256 "first"))
  it "verifies an original and refuses overwrite including a destination created during transfer" $ \scope ->
    withFiles $ \directory -> do
      let destination = directory </> "download.txt"
      withServer (documentApp "good" (pure ()) Nothing) $ \manager base -> do
        result <- executeAttachment scope manager base Nothing identity [] (original destination)
        field ["structuredContent","verified"] result `shouldBe` Just (Bool True)
        BS.readFile destination `shouldReturn` "good"
        repeated <- executeAttachment scope manager base Nothing identity [] (original destination)
        errorCode repeated `shouldBe` Just "destination_exists"
      removeFile destination
      withServer (documentApp "good" (BS.writeFile destination "racer") Nothing) $ \manager base -> do
        raced <- executeAttachment scope manager base Nothing identity [] (original destination)
        errorCode raced `shouldBe` Just "destination_exists"
      BS.readFile destination `shouldReturn` "racer"
      noPartials directory
  it "rejects corrupt, oversized, encoded and redirected bodies without publication or forwarding credentials" $ \scope ->
    withFiles $ \directory -> do
      redirected <- newIORef ([] :: [RequestHeaders])
      withServer (\request respond -> modifyIORef' redirected (<> [Wai.requestHeaders request]) >> respond (json status200 Null)) $ \_ outside ->
        forM_ [("corrupt",0::Int),("oversized",1),("encoded",2),("redirected",3)] $ \(tag,mode) -> do
          let destination = directory </> tag
              behavior = case mode of
                0 -> Just (status200,binaryHeaders "good","evil")
                1 -> Just (status200,("Content-Length","5"):filter ((/="Content-Length") . fst) (binaryHeaders "good"),"extra")
                2 -> Just (status200,("Content-Encoding","gzip"):binaryHeaders "good","good")
                _ -> Just (status302,[("Location",B8.pack outside)],"")
          withServer (documentApp "good" (pure ()) behavior) $ \manager base -> do
            result <- executeAttachment scope manager base (Just "must-not-leak") identity [] (original destination)
            errorCode result `shouldSatisfy` (/=Nothing)
          doesPathExist destination `shouldReturn` False
          noPartials directory
      readIORef redirected `shouldReturn` []
  it "removes partial output on the absolute operation deadline and asynchronous cancellation" $ \scope ->
    withFiles $ \directory -> forM_ [False,True] $ \cancelled -> do
      began <- newEmptyMVar
      finish <- newEmptyMVar
      let destination = directory </> if cancelled then "cancel.txt" else "timeout.txt"
          app request respond
            | BS.isSuffixOf "/original" (Wai.rawPathInfo request) =
                respond (Wai.responseStream status200 (binaryHeaders "good") $ \send flush -> do
                  send (Builder.byteString "go"); flush; putMVar began (); takeMVar finish; send (Builder.byteString "od"))
            | otherwise = respond (json status200 (toJSON (mockDocument "good")))
      withServer app $ \manager base -> flip finally (void (tryPutMVar finish ())) $
        withAsync (executeAttachmentWith scope defaultAttachmentOptions {operationMicros=500000}
          manager base Nothing identity [] (original destination)) $ \worker -> do
          await began
          if cancelled then cancel worker >> waitCatch worker >>= \outcome -> outcome `shouldSatisfy` isLeft
          else wait worker >>= \result -> errorCode result `shouldBe` Just "attachment_timeout"
      doesPathExist destination `shouldReturn` False
      noPartials directory
  it "rejects a foreign document response and duplicate preview descriptors before projection" $ \scope ->
    withFiles $ \directory -> do
      let wrong = case toJSON (mockDocument "good") of
            Object fields -> Object (KM.insert "workspace_id" (toJSON otherWorkspace) fields)
            _ -> Null
      withServer (\_ respond -> respond (json status200 wrong)) $ \manager base -> do
        result <- executeAttachment scope manager base Nothing identity [] (original (directory </> "wrong.txt"))
        errorCode result `shouldBe` Just "invalid_response"
      let output = object ["variant" .= ("page" :: Text),"page" .= (1::Int),"media_type" .= ("image/png" :: Text),
            "raw_bytes" .= (4::Int),"raw_sha256" .= payloadSha256 "good"]
          app request respond
            | BS.isSuffixOf "/previews/current" (Wai.rawPathInfo request) =
                respond (json status200 (object ["state" .= ("ready" :: Text),"generation_id" .= generation,
                  "policy_version" .= ("p1" :: Text),"outputs" .= [output,output]]))
            | otherwise = respond (json status200 (toJSON (Doc.DocumentPage [Doc.DocumentLink association task Doc.Source (mockDocument "good") now] Nothing)))
      withServer app $ \manager base -> do
        result <- executeAttachment scope manager base Nothing identity [] (List workspace task Doc.Source Nothing 50)
        errorCode result `shouldBe` Just "invalid_response"
  it "requires the effective isolated CSP and never sends private credentials or cookies to styled grant URLs" $ \scope ->
    withFiles $ \directory -> forM_ [0,1,2,3,4 :: Int] $ \mode -> do
      address <- newIORef ""
      captured <- newIORef []
      let bytes = "<p>inert styled transport fixture</p>"
          digest = payloadSha256 bytes
          output = object ["variant" .= ("styled_html_preview" :: Text),"page" .= (0::Int),
            "media_type" .= ("text/html" :: Text),"raw_bytes" .= BS.length bytes,"raw_sha256" .= digest]
          csp = case mode of
            1 -> "sandbox allow-scripts allow-same-origin; script-src 'unsafe-inline'; " <> isolatedCSP
            2 -> BS.intercalate ";" (map (\part -> if B8.words part == ["img-src","data:"] then "img-src https:" else part)
                  (B8.split ';' isolatedCSP))
            3 -> "SANDBOX allow-scripts; " <> isolatedCSP
            4 -> BS.intercalate ";" (map (\part -> if take 1 (B8.words part) == ["frame-ancestors"]
                  then "frame-ancestors https://*.evil.example" else part) (B8.split ';' isolatedCSP))
            _ -> isolatedCSP
          app request respond
            | BS.isPrefixOf "/document-grants/" (Wai.rawPathInfo request) = do
                modifyIORef' captured (<> [Wai.requestHeaders request])
                respond (Wai.responseLBS status200
                  ([("Content-Type","text/html; charset=utf-8"),("Content-Security-Policy",csp),
                    ("Cache-Control","no-store"),("Referrer-Policy","no-referrer"),("X-Content-Type-Options","nosniff")]
                    <> filter ((/="Content-Type") . fst) (binaryHeaders bytes)) (BL.fromStrict bytes))
            | BS.isSuffixOf "/download-grants" (Wai.rawPathInfo request) = do
                base <- readIORef address
                respond (json status201 (object ["grant_id" .= requestId,"workspace_id" .= workspace,
                  "task_id" .= task,"document_id" .= documentId,"role" .= Doc.Source,"association_id" .= association,
                  "variant" .= ("styled_html_preview" :: Text),"page" .= (0::Int),"generation_id" .= generation,
                  "policy_version" .= ("p1" :: Text),"expires_at" .= now,
                  "url" .= (base <> "/document-grants/" <> show requestId <> "/styled_html_preview/0?token=" <> replicate 43 'a'),
                  "manifest" .= object ["raw_bytes" .= BS.length bytes,"raw_sha256" .= digest,
                    "media_type" .= ("text/html" :: Text),"filename" .= ("notes.txt" :: Text),
                    "original_sha256" .= payloadSha256 "good"]]))
            | BS.isSuffixOf "/previews/current" (Wai.rawPathInfo request) =
                respond (json status200 (object ["state" .= ("ready" :: Text),"generation_id" .= generation,
                  "policy_version" .= ("p1" :: Text),"outputs" .= [output]]))
            | otherwise = respond (json status200 (toJSON (mockDocument "good")))
          destination = directory </> ("styled-" <> show mode <> ".html")
      withServer app $ \manager base -> do
        writeIORef address base
        result <- executeAttachment scope manager base (Just "private-service-credential") identity
          [("X-HMem-MCP-Provenance","private-provenance"),("Cookie","private-cookie")]
          (Download workspace task documentId destination "styled_html_preview" 0 (Just generation) (Just Doc.Source) (Just association))
        if mode == 0 then do
          field ["structuredContent","verified"] result `shouldBe` Just (Bool True)
          BS.readFile destination `shouldReturn` bytes
        else errorCode result `shouldBe` Just "invalid_response"
      headers <- readIORef captured
      length headers `shouldBe` 1
      forM_ headers $ \values -> forM_ ["Authorization","Cookie","X-HMem-MCP-Provenance","X-HMem-Bot-Identity"] $ \key ->
        lookup key values `shouldBe` Nothing
      when (mode /= 0) (doesPathExist destination `shouldReturn` False)
      noPartials directory
  it "streams a generation-pinned derivative above the upload cap and preserves additive compact preview metadata" $ \scope ->
    withFiles $ \directory -> do
      let bytes = BS.replicate (25*1048576+1) 120
          output = object ["variant" .= ("page" :: Text),"page" .= (0::Int),"media_type" .= ("image/png" :: Text),
            "raw_bytes" .= BS.length bytes,"raw_sha256" .= payloadSha256 bytes]
          cursor = Doc.DocumentCursor now association
          app request respond
            | BS.isSuffixOf "/page/0" (Wai.rawPathInfo request) =
                respond (Wai.responseLBS status200 (("Content-Type","image/png"):filter ((/="Content-Type") . fst) (binaryHeaders bytes)) (BL.fromStrict bytes))
            | BS.isSuffixOf "/previews/current" (Wai.rawPathInfo request) =
                respond (json status200 (object ["state" .= ("ready" :: Text),"generation_id" .= generation,
                  "policy_version" .= ("p1" :: Text),"outputs" .= [output],"retryable" .= False,
                  "generation_metadata" .= object ["verification" .= ("unknown" :: Text)],"future_field" .= True]))
            | BS.isSuffixOf "/counts" (Wai.rawPathInfo request) =
                respond (json status200 (toJSON (Doc.DocumentCounts 1 2 3 5)))
            | BS.isSuffixOf "/documents" (Wai.rawPathInfo request) =
                respond (json status200 (toJSON (Doc.DocumentPage [Doc.DocumentLink association task Doc.Source (mockDocument "good") now] (Just cursor))))
            | otherwise = respond (json status200 (toJSON (mockDocument "good")))
      withServer app $ \manager base -> do
        downloaded <- executeAttachment scope manager base Nothing identity []
          (Download workspace task documentId (directory </> "large.png") "page" 0 (Just generation) Nothing Nothing)
        field ["structuredContent","verified"] downloaded `shouldBe` Just (Bool True)
        withBinaryFile (directory </> "large.png") ReadMode hFileSize `shouldReturn` toInteger (BS.length bytes)
        payloadSha256 <$> BS.readFile (directory </> "large.png") `shouldReturn` payloadSha256 bytes
        listed <- executeAttachment scope manager base Nothing identity [] (List workspace task Doc.Source Nothing 50)
        field ["structuredContent","counts","evidence_total"] listed `shouldBe` Just (Number 5)
        field ["structuredContent","next_cursor"] listed `shouldBe` Just (toJSON cursor)
        field ["structuredContent","next_request","arguments","cursor"] listed `shouldBe` Just (toJSON cursor)
        case field ["structuredContent","items"] listed of
          Just (Array rows) -> case foldr (:) [] rows of
            [row] -> do
              field ["preview","generation_metadata","verification"] row `shouldBe` Just (String "unknown")
              field ["preview","animated_first_frame"] row `shouldBe` Nothing
              field ["preview","outputs"] row `shouldBe` Nothing
            _ -> expectationFailure "Expected exactly one compact preview row"
          _ -> expectationFailure "Expected canonical page"
      noPartials directory
  it "applies one finite deadline to the whole preview-enriched page and preserves actionable backend errors" $ \scope -> do
    let rows = [Doc.DocumentLink association task Doc.Source (mockDocument "good") now,
          Doc.DocumentLink requestId task Doc.Source (mockDocument "good") now]
        app request respond
          | BS.isSuffixOf "/previews/current" (Wai.rawPathInfo request) = do
              threadDelay 80000
              respond (json status200 (object ["state" .= ("queued" :: Text),"outputs" .= ([] :: [Value])]))
          | otherwise = respond (json status200 (toJSON (Doc.DocumentPage rows Nothing)))
    withServer app $ \manager base -> do
      result <- executeAttachmentWith scope defaultAttachmentOptions {operationMicros=100000}
        manager base Nothing identity [] (List workspace task Doc.Source Nothing 50)
      errorCode result `shouldBe` Just "attachment_timeout"
    forM_ ["document_preview_stale","document_preview_unavailable","styled_preview_unavailable","document_download_unavailable",
      "credential_authority_unavailable","unauthorized","forbidden"] $ \code ->
      withServer (\_ respond -> respond (json status409 (object ["error" .= (code :: Text)]))) $ \manager base -> do
        result <- executeAttachment scope manager base Nothing identity [] (original "never-written")
        errorCode result `shouldBe` Just code
  it "projects animation only from verified generation metadata and refuses conflicting original bindings" $ \scope ->
    forM_ [0,1,2,3 :: Int] $ \mode -> do
      let digest = payloadSha256 "good"
          output = object ["variant" .= ("page" :: Text),"page" .= (0::Int),"media_type" .= ("image/png" :: Text),
            "raw_bytes" .= (4::Int),"raw_sha256" .= digest]
          metadata = case mode of
            0 -> Null
            1 -> object ["verification" .= ("unknown" :: Text)]
            _ -> object ["verification" .= ("verified" :: Text),"plan_sha256" .= digest,"limits_sha256" .= digest,
              "original_sha256" .= (if mode == 3 then payloadSha256 "other" else digest),"policy_version" .= previewPolicyVersion,
              "image_id" .= ("sha256:" <> digest),"output_count" .= (1::Int),"page_count" .= (0::Int),
              "decoded_pixels" .= (1::Int),"pdf_decoded_bytes" .= (0::Int),"animated_first_frame" .= True]
          app request respond
            | BS.isSuffixOf "/previews/current" (Wai.rawPathInfo request) =
                respond (json status200 (object ["state" .= ("ready" :: Text),"generation_id" .= generation,
                  "policy_version" .= previewPolicyVersion,"outputs" .= [output],"generation_metadata" .= metadata]))
            | BS.isSuffixOf "/counts" (Wai.rawPathInfo request) =
                respond (json status200 (toJSON (Doc.DocumentCounts 1 0 0 0)))
            | otherwise = respond (json status200 (toJSON (Doc.DocumentPage
                [Doc.DocumentLink association task Doc.Source (mockDocument "good") now] Nothing)))
      withServer app $ \manager base -> do
        result <- executeAttachment scope manager base Nothing identity [] (List workspace task Doc.Source Nothing 50)
        if mode == 3 then errorCode result `shouldBe` Just "invalid_response"
        else case field ["structuredContent","items"] result of
          Just (Array rows) -> case foldr (:) [] rows of
            [row] -> field ["preview","animated_first_frame"] row `shouldBe` (if mode == 2 then Just (Bool True) else Nothing)
            _ -> expectationFailure "Expected one typed preview row"
          _ -> expectationFailure ("Missing typed preview page, mode/error " <> show (mode,errorCode result))
  it "uses the delivered native stdio executable for real upload, list, verified download and exact evidence promotion" $ \scope ->
    withRealAPI $ \env ws tid _ manager base -> withFiles $ \directory -> withNativeMCP base $ \rpc -> do
      _ <- rpc 1 "initialize" (object [])
      declared <- rpc 2 "tools/call" (object ["name" .= ("set_workspace" :: Text),
        "arguments" .= object ["workspace_id" .= ws,"identity" .= identity]])
      field ["isError"] declared `shouldBe` Nothing
      let source = directory </> "source.txt"
          destination = directory </> "download.txt"
          bytes = TE.encodeUtf8 "actual stdio source\r\n雪😀"
          uploadArgs = ["task_id" .= tid,"path" .= source,"role" .= ("source" :: Text),"request_id" .= requestId,
            "filename" .= ("original-😀.txt" :: Text)]
          tool n name args = rpc n "tools/call" (object ["name" .= (name :: Text),"arguments" .= object args])
      BS.writeFile source bytes
      uploaded <- tool 3 "attachment_upload" uploadArgs
      identifier <- required ["structuredContent","attachment_id"] uploaded
      sourceAssociation <- required ["structuredContent","association_id"] uploaded
      field ["structuredContent","filename"] uploaded `shouldBe` Just (String "original-😀.txt")
      field ["structuredContent","counts","evidence_total"] uploaded `shouldBe` Just (Number 0)
      repeated <- tool 4 "attachment_upload" uploadArgs
      field ["structuredContent","attachment_id"] repeated `shouldBe` Just (toJSON (identifier :: UUID))
      listed <- tool 5 "attachment_list" ["task_id" .= tid,"role" .= ("source" :: Text)]
      field ["structuredContent","counts","source_count"] listed `shouldBe` Just (Number 1)
      case field ["structuredContent","items"] listed of
        Just (Array rows) -> length rows `shouldBe` 1
        _ -> expectationFailure "Missing canonical source page"
      downloaded <- tool 6 "attachment_download" ["task_id" .= tid,"attachment_id" .= identifier,"path" .= destination]
      field ["structuredContent","verified"] downloaded `shouldBe` Just (Bool True)
      BS.readFile destination `shouldReturn` bytes
      linked <- tool 7 "attachment_download_link" ["task_id" .= tid,"attachment_id" .= identifier,
        "role" .= ("source" :: Text),"association_id" .= (sourceAssociation :: UUID),"ttl_seconds" .= (60::Int)]
      linkUrl <- required ["structuredContent","url"] linked
      expires <- required ["structuredContent","expires_at"] linked
      fetchedAt <- getCurrentTime
      public <- HTTP.parseRequest (T.unpack (linkUrl :: Text))
      putStrLn ("attachment native fixture: fetching original grant, seconds_remaining=" <> show (diffUTCTime expires fetchedAt))
      hFlush stdout
      fetched <- HTTP.httpLbs public {HTTP.redirectCount=0,HTTP.decompress=const False} manager
      putStrLn "attachment native fixture: original grant fetch complete"
      hFlush stdout
      HTTP.responseBody fetched `shouldBe` BL.fromStrict bytes
      short <- tool 13 "attachment_download_link" ["task_id" .= tid,"attachment_id" .= identifier,
        "role" .= ("source" :: Text),"association_id" .= sourceAssociation,"ttl_seconds" .= (1::Int)]
      shortUrl <- required ["structuredContent","url"] short
      shortRequest <- HTTP.parseRequest (T.unpack (shortUrl :: Text))
      threadDelay 1100000
      expired <- HTTP.httpLbs shortRequest {HTTP.checkResponse = \_ _ -> pure ()} manager
      statusCode (HTTP.responseStatus expired) `shouldSatisfy` (>=400)
      collision <- tool 12 "attachment_add_evidence" ["task_id" .= tid,"attachment_id" .= identifier,"request_id" .= requestId]
      errorCode collision `shouldBe` Just "document_request_conflict"
      promotionRequest <- UUIDv4.nextRandom
      promoted <- tool 8 "attachment_add_evidence" ["task_id" .= tid,"attachment_id" .= identifier,"request_id" .= promotionRequest]
      evidenceAssociation <- required ["structuredContent","association_id"] promoted
      field ["structuredContent","role"] promoted `shouldBe` Just (String "evidence")
      field ["structuredContent","counts","evidence_total"] promoted `shouldBe` Just (Number 1)
      promotedAgain <- tool 9 "attachment_add_evidence" ["task_id" .= tid,"attachment_id" .= identifier,"request_id" .= promotionRequest]
      field ["structuredContent","association_id"] promotedAgain `shouldBe` Just (toJSON (evidenceAssociation :: UUID))
      removed <- tool 10 "attachment_remove_source" ["task_id" .= tid,"attachment_id" .= identifier]
      field ["structuredContent","counts","source_count"] removed `shouldBe` Just (Number 0)
      field ["structuredContent","counts","evidence_total"] removed `shouldBe` Just (Number 1)
      directRequest <- UUIDv4.nextRandom
      direct <- tool 11 "attachment_upload" ["task_id" .= tid,"path" .= source,"role" .= ("evidence" :: Text),
        "request_id" .= directRequest]
      field ["structuredContent","counts","evidence_total"] direct `shouldBe` Just (Number 2)
      current <- Tasks.getTask env.pool tid
      fmap (.status) current `shouldBe` Just Types.Todo
      noPartials directory
  it "pins real raster/styled fixtures, uses credential-free isolated grants and refuses stale/cross-workspace scope" $ \scope ->
    withRealAPI $ \env ws tid authority manager base -> withFiles $ \directory -> do
      let source = directory </> "drawing.html"
          originalBytes = "<!doctype html><html><p>original source</p></html>"
          call name args = handleToolCallWithAttachmentScope (Just scope) manager base Nothing identity (object ["name" .= (name :: Text),
            "arguments" .= object (["workspace_id" .= ws,"task_id" .= tid] <> args)])
      BS.writeFile source originalBytes
      uploaded <- call "attachment_upload" ["path" .= source,"role" .= ("source" :: Text),"request_id" .= requestId]
      identifier <- required ["structuredContent","attachment_id"] uploaded
      sourceAssociation <- required ["structuredContent","association_id"] uploaded
      gen <- UUIDv4.nextRandom
      let raster = BS.pack [137,80,78,71,13,10,26,10] <> "verified transport raster fixture"
          styledBytes = "<html><body><p>sanitized fixture</p></body></html>"
          policy = previewPolicyVersion
      Documents.reservePreview env.pool ws identifier gen policy 2 1048576
      Documents.publishPreview env.pool ws identifier gen policy
        [Documents.Derivative "page" 0 "image/png" raster,Documents.Derivative "styled_html_preview" 0 "text/html" styledBytes]
      listed <- call "attachment_list" ["role" .= ("source" :: Text)]
      case field ["structuredContent","items"] listed of
        Just (Array rows) -> do
          length rows `shouldBe` 1
          field ["preview","generation_id"] (head (foldr (:) [] rows)) `shouldBe` Just (toJSON gen)
        _ -> expectationFailure "Missing preview metadata"
      rasterDownload <- call "attachment_download" ["attachment_id" .= identifier,"path" .= (directory </> "page.png"),
        "variant" .= ("page" :: Text),"page" .= (0::Int),"generation_id" .= gen]
      field ["structuredContent","verified"] rasterDownload `shouldBe` Just (Bool True)
      BS.readFile (directory </> "page.png") `shouldReturn` raster
      styledDownload <- call "attachment_download" ["attachment_id" .= identifier,"path" .= (directory </> "styled.html"),
        "variant" .= ("styled_html_preview" :: Text),"generation_id" .= gen,"role" .= ("source" :: Text),
        "association_id" .= (sourceAssociation :: UUID)]
      errorCode styledDownload `shouldBe` Nothing
      field ["structuredContent","grant","association_id"] styledDownload `shouldBe` Just (toJSON sourceAssociation)
      field ["structuredContent","verified"] styledDownload `shouldBe` Just (Bool True)
      BS.readFile (directory </> "styled.html") `shouldReturn` styledBytes
      stale <- call "attachment_download" ["attachment_id" .= identifier,"path" .= (directory </> "stale.png"),
        "variant" .= ("page" :: Text),"generation_id" .= generation]
      errorCode stale `shouldBe` Just "preview_stale"
      doesPathExist (directory </> "stale.png") `shouldReturn` False
      foreignResult <- executeAttachment scope manager base Nothing identity [] (Download otherWorkspace tid identifier
        (directory </> "foreign") "original" 0 Nothing Nothing Nothing)
      errorCode foreignResult `shouldSatisfy` (/=Nothing)
      doesPathExist (directory </> "foreign") `shouldReturn` False
      promotionRequest <- UUIDv4.nextRandom
      promoted <- call "attachment_add_evidence" ["attachment_id" .= identifier,"request_id" .= promotionRequest]
      evidenceAssociation <- required ["structuredContent","association_id"] promoted
      blocked <- call "attachment_download"
        ["attachment_id" .= identifier,"path" .= (directory </> "blocked.html"),"role" .= ("evidence" :: Text),
         "association_id" .= (evidenceAssociation :: UUID)]
      errorCode blocked `shouldBe` Just "document_evidence_task_not_terminal"
      doesPathExist (directory </> "blocked.html") `shouldReturn` False
      removed <- call "attachment_remove_source" ["attachment_id" .= identifier]
      field ["structuredContent","counts","source_count"] removed `shouldBe` Just (Number 0)
      field ["structuredContent","counts","evidence_total"] removed `shouldBe` Just (Number 1)
      evidence <- call "attachment_list" ["role" .= ("evidence" :: Text)]
      case field ["structuredContent","items"] evidence of
        Just (Array rows) -> do
          length rows `shouldBe` 1
          forM_ rows $ \row -> do
            field ["association_id"] row `shouldBe` Just (toJSON (evidenceAssociation :: UUID))
            field ["preview","generation_id"] row `shouldBe` Just (toJSON gen)
        _ -> expectationFailure "Missing Evidence preview metadata"
      _ <- Tasks.updateTask env.pool tid
        (Types.UpdateTask Nothing Types.Unchanged Types.Unchanged Types.Unchanged (Just Types.Done) Nothing Nothing)
      evidenceOriginal <- call "attachment_download"
        ["attachment_id" .= identifier,"path" .= (directory </> "evidence-original.html")]
      field ["structuredContent","verified"] evidenceOriginal `shouldBe` Just (Bool True)
      BS.readFile (directory </> "evidence-original.html") `shouldReturn` originalBytes
      evidenceStyled <- call "attachment_download" ["attachment_id" .= identifier,"path" .= (directory </> "evidence-styled.html"),
        "variant" .= ("styled_html_preview" :: Text),"generation_id" .= gen,"role" .= ("evidence" :: Text),
        "association_id" .= evidenceAssociation]
      errorCode evidenceStyled `shouldBe` Nothing
      field ["structuredContent","grant","association_id"] evidenceStyled `shouldBe` Just (toJSON evidenceAssociation)
      field ["structuredContent","verified"] evidenceStyled `shouldBe` Just (Bool True)
      BS.readFile (directory </> "evidence-styled.html") `shouldReturn` styledBytes
      pageGrant <- call "attachment_download_link" ["attachment_id" .= identifier,
        "role" .= ("evidence" :: Text),"association_id" .= evidenceAssociation,
        "variant" .= ("page" :: Text),"page" .= (0::Int),"generation_id" .= gen]
      field ["structuredContent","association_id"] pageGrant `shouldBe` Just (toJSON evidenceAssociation)
      field ["structuredContent","generation_id"] pageGrant `shouldBe` Just (toJSON gen)
      pageURL <- required ["structuredContent","url"] pageGrant
      pageRequest <- HTTP.parseRequest (T.unpack (pageURL :: Text))
      page <- HTTP.httpLbs pageRequest {HTTP.redirectCount=0,HTTP.decompress=const False} manager
      HTTP.responseBody page `shouldBe` BL.fromStrict raster
      -- Withdraw the exact local authority epoch and verify a previously issued
      -- bearer cannot regain access through another association or credential.
      grant <- call "attachment_download_link" ["attachment_id" .= identifier,"role" .= ("evidence" :: Text),
        "association_id" .= evidenceAssociation]
      url <- required ["structuredContent","url"] grant
      original <- HTTP.parseRequest (T.unpack (url :: Text))
      before <- HTTP.httpLbs original {HTTP.checkResponse = \_ _ -> pure ()} manager
      HTTP.responseStatus before `shouldBe` status200
      policyBefore <- readIORef authority.policy
      Grants.replaceDocumentAuthorityPolicy authority policyBefore.auth policyBefore.server policyBefore.delivery
      after <- HTTP.httpLbs original {HTTP.checkResponse = \_ _ -> pure ()} manager
      statusCode (HTTP.responseStatus after) `shouldSatisfy` (>=400)
      noPartials directory

withoutProvenance :: IO a -> IO a
withoutProvenance action = bracket (lookupEnv name) restore $ \_ -> unsetEnv name >> action
  where
    name = "HMEM_MCP_PROVENANCE_TOKEN"
    restore Nothing = unsetEnv name
    restore (Just value) = setEnv name value

attachmentExecutable :: IO FilePath
attachmentExecutable = do
  testExecutable <- getExecutablePath
  canonicalizePath (takeDirectory (takeDirectory testExecutable) </> "hmem-mcp" </>
    ("hmem-mcp" <> if Info.os == "mingw32" then ".exe" else ""))

withAttachmentScopeTest :: (HTTP.AttachmentScope -> IO a) -> IO a
withAttachmentScopeTest action = do
  executable <- attachmentExecutable
  bracket (HTTP.newAttachmentScope executable) close action
  where
    close scope = HTTP.closeAttachmentScope scope 5000000 >>= \closed ->
      unless closed (fail "Test attachment scope did not retire its native ownership")

await :: MVar a -> IO a
await signal = timeout 5000000 (takeMVar signal) >>= maybe (fail "Fixture signal exceeded five seconds") pure

required :: FromJSON a => [Text] -> Value -> IO a
required keys value = case field keys value of
  Nothing -> fail ("Missing response field " <> show keys <> ": " <> show value)
  Just item -> case fromJSON item of
    Error message -> fail ("Invalid response field " <> show keys <> ": " <> message)
    Success result -> pure result

withRealAPI :: (TestEnv -> UUID -> UUID -> Grants.DocumentAuthority -> HTTP.Manager -> String -> IO a) -> IO a
withRealAPI action = withTestEnv $ \env -> do
  ws <- (.id) <$> createTestWorkspace env "attachment-http"
  taskRow <- Tasks.createTask env.pool (Types.CreateTask ws Nothing Nothing "MCP attachment fixture" Nothing Nothing Nothing)
  let cfg = Config.defaultConfig
  authority <- Grants.newDocumentAuthority env.pool cfg.auth cfg.server Config.defaultDocumentDeliveryConfig
  tracker <- newAccessTracker env.pool 3600
  sockets <- newWSState
  app <- mkAppWithDocumentAuthority authority Nothing Nothing cfg.changeStream id cfg.auth cfg.cors cfg.rateLimit
    env.pool tracker sockets Nothing False
  preview <- newPreviewApplication authority cfg.rateLimit
  testWithApplication (pure app) $ \mainPort -> testWithApplication (pure preview) $ \previewPort -> do
    let origin = "http://localhost:" <> T.pack (show mainPort)
        delivery = Config.defaultDocumentDeliveryConfig
          {Config.applicationOrigin=Just origin,Config.downloadOrigin=Just origin,
           Config.previewOrigin=Just ("http://127.0.0.1:" <> T.pack (show previewPort)),
           Config.previewListenPort=Just previewPort,Config.proxyAuthCookieDomains=Just []}
    Grants.replaceDocumentAuthorityPolicy authority cfg.auth cfg.server delivery
    putStrLn "attachment fixture schema/HTTP authority ready"
    hFlush stdout
    bracket (HTTP.newManager HTTP.defaultManagerSettings {HTTP.managerIdleConnectionCount=0}) HTTP.closeManager $ \manager ->
      action env ws taskRow.id authority manager ("http://127.0.0.1:" <> show mainPort)

-- The actual delivered executable, adjacent to the test component's build
-- directory, exercises the real bounded queue and captured context.
withNativeMCP :: String -> ((Int -> Text -> Value -> IO Value) -> IO a) -> IO a
withNativeMCP base action = do
  executable <- attachmentExecutable
  exists <- doesFileExist executable
  unless exists (fail "Build hmem-mcp:exe:hmem-mcp before the native stdio fixture")
  bracket (createProcess (proc executable ["--server-url",base,"--no-auth"])
    {std_in=CreatePipe,std_out=CreatePipe,std_err=CreatePipe,create_group=True}) cleanup $ \created ->
      case created of
        (Just input,Just output,Just errors,processHandle) -> do
          hSetBuffering input LineBuffering
          childId <- getPid processHandle
          putStrLn ("attachment native fixture: child=" <> show childId)
          hFlush stdout
          -- A failed assertion must kill and join the child before withAsync
          -- attempts to join a potentially blocked Windows pipe reader.
          withAsync (drain errors) $ \stderrWorker -> flip onException (stop processHandle) $ do
            result <- action $ \identifier method params -> do
              putStrLn ("attachment native RPC start: " <> show identifier)
              hFlush stdout
              BL.hPutStr input (encode (object ["jsonrpc" .= ("2.0" :: Text),"id" .= identifier,
                "method" .= method,"params" .= params]) <> "\n")
              hFlush input
              bytes <- withAsync (threadDelay 15000000 >> stop processHandle) $ \_ -> B8.hGetLine output
              putStrLn ("attachment native RPC complete: " <> show identifier)
              hFlush stdout
              response <- either fail pure (eitherDecodeStrict' bytes)
              field ["id"] response `shouldBe` Just (toJSON identifier)
              required ["result"] response
            hClose input
            outcome <- timeout 5000000 (waitForProcess processHandle)
            outcome `shouldBe` Just ExitSuccess
            timeout 5000000 (wait stderrWorker) >>= maybe (fail "Native MCP stderr did not drain") pure
            pure result
        _ -> fail "Native MCP fixture did not obtain its three owned pipes"
  where
    drain handle = do
      bytes <- BS.hGetSome handle 4096
      unless (BS.null bytes) (drain handle)
    stop processHandle = do
      exited <- getProcessExitCode processHandle
      when (exited == Nothing) (terminateProcess processHandle)
      joined <- timeout 5000000 (waitForProcess processHandle)
      when (joined == Nothing) (fail "Owned native MCP process failed to join")
    cleanup (input,output,errors,processHandle) = do
      stop processHandle
      forM_ input $ \handle -> hClose handle `catch` \(_ :: IOException) -> pure ()
      forM_ (catMaybes [input,output,errors]) $ \handle -> hClose handle `catch` \(_ :: IOException) -> pure ()

workspace, otherWorkspace, task, documentId, payloadId, association, generation, requestId :: UUID
workspace = read "11111111-1111-4111-8111-111111111111"
otherWorkspace = read "22222222-2222-4222-8222-222222222222"
task = read "33333333-3333-4333-8333-333333333333"
documentId = read "44444444-4444-4444-8444-444444444444"
payloadId = read "55555555-5555-4555-8555-555555555555"
association = read "66666666-6666-4666-8666-666666666666"
generation = read "77777777-7777-4777-8777-777777777777"
requestId = read "88888888-8888-4888-8888-888888888888"
now :: UTCTime
now = read "2026-01-01 00:00:00 UTC"
identity :: BotIdentity
identity = either (error . T.unpack) id (mkBotIdentity "Attachment test" "test:fixture:default")
isolatedCSP :: BS.ByteString
isolatedCSP = "sandbox; default-src 'none'; script-src 'none'; style-src 'unsafe-inline'; img-src data:; font-src 'none'; connect-src 'none'; frame-src 'none'; child-src 'none'; worker-src 'none'; object-src 'none'; media-src 'none'; base-uri 'none'; form-action 'none'; frame-ancestors http://localhost:8420"
mockDocument :: BS.ByteString -> Doc.Document
mockDocument bytes = fromManifest (Doc.OriginalManifest (fromIntegral (BS.length bytes)) (payloadSha256 bytes) "notes.txt" "notes.txt" "text/plain")
fromManifest :: Doc.OriginalManifest -> Doc.Document
fromManifest m = Doc.Document documentId workspace payloadId m.filename m.title m.mediaType m.rawBytes m.rawSha256 now
  (object ["kind" .= ("test" :: Text)]) "queued" Nothing
original :: FilePath -> AttachmentCall
original destination = Download workspace task documentId destination "original" 0 Nothing Nothing Nothing
isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False
isRight :: Either a b -> Bool
isRight = not . isLeft
field :: [Text] -> Value -> Maybe Value
field [] value = Just value
field (key:rest) (Object value) = KM.lookup (Key.fromText key) value >>= field rest
field _ _ = Nothing
errorCode :: Value -> Maybe Text
errorCode value = case field ["structuredContent","error","code"] value of Just (String code) -> Just code; _ -> Nothing
json :: Status -> Value -> Wai.Response
json status value = Wai.responseLBS status [("Content-Type","application/json")] (encode value)
decodeMetadata :: BS.ByteString -> IO Doc.OriginalManifest
decodeMetadata bytes = case eitherDecodeStrict' bytes >>= \value -> maybe (Left "manifest") Right (field ["manifest"] value) of
  Left _ -> fail "Invalid fixture metadata"
  Right value -> case fromJSON value of Error _ -> fail "Invalid fixture manifest"; Success manifest -> pure manifest
binaryHeaders :: BS.ByteString -> ResponseHeaders
binaryHeaders bytes = [("Content-Type","application/octet-stream"),("Content-Length",B8.pack (show (BS.length bytes))),
  ("Content-Disposition","attachment; filename=\"notes.txt\""),("X-HMem-Document-SHA256",TE.encodeUtf8 (payloadSha256 bytes)),
  ("X-HMem-Document-Bytes",B8.pack (show (BS.length bytes)))]
documentApp :: BS.ByteString -> IO () -> Maybe (Status,ResponseHeaders,BL.ByteString) -> Wai.Application
documentApp bytes before override request respond
  | BS.isSuffixOf "/original" (Wai.rawPathInfo request) = do
      before
      let (status,headers,body) = maybe (status200,binaryHeaders bytes,BL.fromStrict bytes) id override
      respond (Wai.responseLBS status headers body)
  | otherwise = respond (json status200 (toJSON (mockDocument bytes)))
withServer :: Wai.Application -> (HTTP.Manager -> String -> IO a) -> IO a
withServer app action = testWithApplication (pure app) $ \port ->
  bracket (HTTP.newManager HTTP.defaultManagerSettings {HTTP.managerIdleConnectionCount=0}) HTTP.closeManager $ \manager ->
    action manager ("http://127.0.0.1:" <> show port)
withFiles :: (FilePath -> IO a) -> IO a
withFiles action = do
  parent <- getTemporaryDirectory
  unique <- UUIDv4.nextRandom
  let directory = parent </> ("attachment-" <> show unique)
  bracket (createDirectory directory >> canonicalizePath directory) removeDirectoryRecursive action
noPartials :: FilePath -> IO ()
noPartials directory = listDirectory directory >>= \names ->
  filter (T.isPrefixOf ".hmem-attachment-" . T.pack) names `shouldBe` []
