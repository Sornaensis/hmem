module HMem.Server.DocumentSpec (spec) where

import Control.Concurrent.Async (concurrently,withAsync,wait)
import Control.Concurrent.MVar
import Control.Exception (SomeException,try,throwIO,bracket_)
import Control.Monad (forM_)
import Data.Foldable (toList)
import Data.Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as B8
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as LBS
import Data.IORef
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.UUID (UUID)
import Data.UUID.V4 qualified as UUID
import Data.Time.Format.ISO8601 (iso8601Show)
import System.Timeout (timeout)
import Hasql.Decoders qualified as D
import Hasql.Encoders qualified as E
import Hasql.Session qualified as S
import Hasql.Statement qualified as St
import Network.HTTP.Types
import Network.Wai qualified as Wai
import Network.Wai.Internal (ResponseReceived(..))
import Network.Wai.Test (SRequest(..),SResponse(..),runSession,srequest)
import Test.Hspec
import HMem.AgentSession qualified as Sessions
import HMem.Config qualified as Config
import HMem.DB.AgentSession qualified as AgentDB
import HMem.DB.Auth qualified as Auth
import HMem.DB.Document qualified as Documents
import HMem.DB.Pool qualified as Pool
import HMem.DB.Task qualified as Task
import HMem.DB.TestHarness (TestEnv(..),createTestWorkspace)
import HMem.Document.Types qualified as Doc
import HMem.Document.Codec (payloadSha256)
import HMem.Server.AuthTokens (IssuedAccessToken(..))
import HMem.Server.Document.HTTP (metadataHeader)
import HMem.Server.TestHarness
import HMem.Types qualified as Types

spec :: Spec
spec=describe "Task document HTTP" $ do
  it "uploads a Source on a completed task, returns exact retries and byte-identical safe original downloads" $
    withLocalSandboxAppContext $ \ctx -> do
      (workspace,task)<-fixture ctx.localEnv
      execute ctx.localEnv("UPDATE tasks SET status='done' WHERE id='"<>uuid task<>"'")
      requestId<-UUID.nextRandom
      let bytes=TE.encodeUtf8 "notes\r\næ雪"
          headers=uploadHeaders requestId Doc.Source bytes "café.txt" "text/plain"
          root=documents workspace task
      first<-call ctx.localApplication methodPost root headers(LBS.fromStrict bytes) >>= document
      retry<-call ctx.localApplication methodPost root headers(LBS.fromStrict bytes) >>= document
      first.id `shouldBe` retry.id
      getCounts ctx.localApplication root [] `shouldReturn` Doc.DocumentCounts 1 0 0 0
      result<-call ctx.localApplication methodGet(root<>"/"<>uuid first.id<>"/original") [] ""
      simpleStatus result `shouldBe` status200
      LBS.toStrict result.simpleBody `shouldBe` bytes
      lookup hContentType result.simpleHeaders `shouldBe` Just "application/octet-stream"
      lookup "Content-Disposition" result.simpleHeaders `shouldSatisfy` maybe False(BS.isPrefixOf "attachment;")
      lookup "Content-Disposition" result.simpleHeaders `shouldSatisfy` maybe False(BS.isInfixOf "filename*=UTF-8''caf%C3%A9.txt")
      lookup "X-HMem-Document-SHA256" result.simpleHeaders `shouldBe` Just(TE.encodeUtf8(payloadSha256 bytes))
      lookup hCacheControl result.simpleHeaders `shouldBe` Just "no-store"
      lookup "Referrer-Policy" result.simpleHeaders `shouldBe` Just "no-referrer"
      lookup "X-Content-Type-Options" result.simpleHeaders `shouldBe` Just "nosniff"
      headResult<-call ctx.localApplication methodHead(root<>"/"<>uuid first.id<>"/original") [] ""
      simpleStatus headResult `shouldBe` status200
      headResult.simpleBody `shouldBe` ""
      quotedRequest<-UUID.nextRandom
      quoted<-call ctx.localApplication methodPost root(uploadHeaders quotedRequest Doc.Source bytes "odd\";name'.txt" "text/plain")(LBS.fromStrict bytes) >>= document
      quotedResult<-call ctx.localApplication methodGet(root<>"/"<>uuid quoted.id<>"/original") [] ""
      lookup "Content-Disposition" quotedResult.simpleHeaders `shouldSatisfy` maybe False(BS.isInfixOf "filename*=UTF-8''odd%22%3Bname%27.txt")
      unicodeRequest<-UUID.nextRandom
      let asciiMetadata=[(name,if name==metadataHeader then TE.encodeUtf8(T.replace "雪" "\\u96ea"(TE.decodeUtf8 value)) else value) | (name,value)<-uploadHeaders unicodeRequest Doc.Source bytes "雪.txt" "text/plain"]
      unicode<-call ctx.localApplication methodPost root asciiMetadata(LBS.fromStrict bytes) >>= document
      unicode.filename `shouldBe` "雪.txt"
      astralRequest<-UUID.nextRandom
      let astralName="notes-😀.txt"
          surrogateMetadata=[(name,if name==metadataHeader then TE.encodeUtf8(T.replace "😀" "\\ud83d\\ude00"(TE.decodeUtf8 value)) else value) | (name,value)<-uploadHeaders astralRequest Doc.Source bytes astralName "text/plain"]
      astral<-call ctx.localApplication methodPost root surrogateMetadata(LBS.fromStrict bytes) >>= document
      (astral.filename,astral.title) `shouldBe` (astralName,astralName)
      astralResult<-call ctx.localApplication methodGet(root<>"/"<>uuid astral.id<>"/original") [] ""
      lookup "Content-Disposition" astralResult.simpleHeaders `shouldSatisfy` maybe False(BS.isInfixOf "filename*=UTF-8''notes-%F0%9F%98%80.txt")
      Just current<-Task.getTask ctx.localEnv.pool task
      current.status `shouldBe` Types.Done
  it "queues an MP4 document for preview and preserves its exact attachment when the renderer is unavailable" $
    withLocalSandboxAppContext $ \ctx -> do
      (workspace,task)<-fixture ctx.localEnv
      requestId<-UUID.nextRandom
      let bytes=BS.pack [0,0,0,20]<>"ftypisom"<>BS.replicate 4 0<>"mp42"
          root=documents workspace task
      created<-call ctx.localApplication methodPost root(uploadHeaders requestId Doc.Source bytes "clip.mp4" "video/mp4")(LBS.fromStrict bytes) >>= document
      created.mediaType `shouldBe` "video/mp4"
      (created.previewState,created.previewError) `shouldBe` ("failed",Just "renderer_unavailable")
      boolean ctx.localEnv ("SELECT EXISTS(SELECT 1 FROM document_preview_jobs WHERE document_id='"<>uuid created.id<>"' AND retryable)") `shouldReturn` True
      result<-call ctx.localApplication methodGet(root<>"/"<>uuid created.id<>"/original") [] ""
      simpleStatus result `shouldBe` status200
      LBS.toStrict result.simpleBody `shouldBe` bytes
      lookup hContentType result.simpleHeaders `shouldBe` Just "application/octet-stream"
      lookup "Content-Disposition" result.simpleHeaders `shouldSatisfy` maybe False(BS.isPrefixOf "attachment;")
  it "separates Source/evidence counts, supports agent evidence upload and immutable promotion/removal" $
    withLocalSandboxAppContext $ \ctx -> do
      (workspace,task)<-fixture ctx.localEnv
      let root=documents workspace task; bytes="same original"
      sourceRequest<-UUID.nextRandom
      source<-call ctx.localApplication methodPost root(uploadHeaders sourceRequest Doc.Source bytes "notes.txt" "text/plain")(LBS.fromStrict bytes) >>= document
      getCounts ctx.localApplication root [] `shouldReturn` Doc.DocumentCounts 1 0 0 0
      evidenceRequest<-UUID.nextRandom
      _<-call ctx.localApplication methodPost root(uploadHeaders evidenceRequest Doc.Evidence bytes "notes.txt" "text/plain")(LBS.fromStrict bytes) >>= document
      getCounts ctx.localApplication root [] `shouldReturn` Doc.DocumentCounts 1 1 0 1
      promotion<-UUID.nextRandom
      let target=root<>"/"<>uuid source.id
      forM_ [1::Int,2] $ \_ -> do
        result<-postJSON ctx.localApplication(target<>"/evidence") [] (object["request_id" .= promotion])
        simpleStatus result `shouldBe` status200
      getCounts ctx.localApplication root [] `shouldReturn` Doc.DocumentCounts 1 2 0 2
      call ctx.localApplication methodDelete(target<>"/source") [] "" >>= status status204
      getCounts ctx.localApplication root [] `shouldReturn` Doc.DocumentCounts 0 2 0 2
      call ctx.localApplication methodGet(target<>"/original") [] "" >>= status status200
      boolean ctx.localEnv "SELECT count(*)=1 FROM document_payloads" `shouldReturn` True
  it "enforces deployed read/edit/admin and foreign workspace roles before any body read" $
    withDeployedSandboxAppContext $ \ctx -> do
      (workspace,task)<-fixture ctx.deployedEnv
      (_,reader)<-audience ctx workspace (Just Auth.WorkspaceRoleRead)
      (_,editor)<-audience ctx workspace (Just Auth.WorkspaceRoleEdit)
      (_,admin)<-audience ctx workspace (Just Auth.WorkspaceRoleAdmin)
      (_,outsider)<-audience ctx workspace Nothing
      requestId<-UUID.nextRandom
      let root=documents workspace task; bytes="body"; metadata=uploadHeaders requestId Doc.Source bytes "notes.txt" "text/plain"
      forM_ [([],status401),(reader,status403),(outsider,status403)] $ \(headers,expected) -> do
        (result,reads)<-unread ctx.deployedApplication(mkRequest methodPost root(metadata<>headers))
        result `shouldBe` expected; reads `shouldBe` 0
      foreignWorkspace<-(.id) <$> createTestWorkspace ctx.deployedEnv "foreign-task"
      foreignTask<-Task.createTask ctx.deployedEnv.pool(Types.CreateTask foreignWorkspace Nothing Nothing "foreign task" Nothing Nothing Nothing)
      (foreignResult,foreignReads)<-unread ctx.deployedApplication(mkRequest methodPost(documents workspace foreignTask.id)((hContentType,"application/octet-stream"):(metadataHeader,"{"):editor))
      foreignResult `shouldBe` status404; foreignReads `shouldBe` 0
      forM_ [editor,admin] $ \headers -> do
        req<-UUID.nextRandom
        call ctx.deployedApplication methodPost root(uploadHeaders req Doc.Evidence bytes "notes.txt" "text/plain"<>headers)(LBS.fromStrict bytes) >>= status status201
      call ctx.deployedApplication methodGet(root<>"/counts") reader "" >>= status status200
      call ctx.deployedApplication methodGet(root<>"?limit=invalid") outsider "" >>= status status403
      let forged=reader<>[("X-HMem-Client","mcp"),("X-HMem-Change-Cause","mcp")]
      call ctx.deployedApplication methodPost root(metadata<>forged)(LBS.fromStrict bytes) >>= status status403
  it "requires deployed cookie CSRF before upload/association body consumption" $
    withDeployedSandboxAppContext $ \ctx -> do
      (workspace,task)<-fixture ctx.deployedEnv
      user<-createDeployedSandboxUser ctx.deployedEnv False False
      _<-Auth.upsertWorkspaceMembership ctx.deployedEnv.pool workspace(Auth.UpsertWorkspaceMembership user Auth.WorkspaceRoleEdit) Nothing
      _<-createDeployedSandboxAuthSession ctx.deployedEnv user "doc-session" "doc-csrf"
      requestId<-UUID.nextRandom
      let cfg=ctx.deployedConfig.auth.deployed
          cookie=[("Cookie",TE.encodeUtf8(cfg.sessionCookieName<>"=doc-session; "<>cfg.csrfCookieName<>"=doc-csrf"))]
          root=documents workspace task; bytes="cookie"
          headers=uploadHeaders requestId Doc.Source bytes "notes.txt" "text/plain"
      (denied,reads)<-unread ctx.deployedApplication(mkRequest methodPost root(headers<>cookie))
      denied `shouldBe` status403; reads `shouldBe` 0
      created<-call ctx.deployedApplication methodPost root(headers<>cookie<>[("X-CSRF-Token","doc-csrf")])(LBS.fromStrict bytes) >>= document
      (promotionDenied,promotionReads)<-unread ctx.deployedApplication(mkRequest methodPost(root<>"/"<>uuid created.id<>"/evidence")((hContentType,"application/json"):cookie))
      promotionDenied `shouldBe` status403; promotionReads `shouldBe` 0
  it "rejects oversized/duplicate/unknown metadata and inconsistent lengths before reading or reserving" $
    withLocalSandboxAppContext $ \ctx -> do
      (workspace,task)<-fixture ctx.localEnv
      requestId<-UUID.nextRandom
      let root=documents workspace task
          headers=uploadHeaders requestId Doc.Source "body" "notes.txt" "text/plain"
          meta=snd(last headers)
          bads=[([(hContentType,"application/octet-stream"),(metadataHeader,BS.replicate 8193 32)],Wai.ChunkedBody,status431)
               ,(headers<>[(metadataHeader,meta)],Wai.ChunkedBody,status400)
               ,(headers<>[(hContentLength,"9")],Wai.KnownLength 4,status400)
               ,(headers<>[(hContentLength,"4"),(hContentLength,"4")],Wai.KnownLength 4,status400)
               ,(headers,Wai.KnownLength 9,status400)
               ,(uploadHeaders requestId Doc.Source "body" "evil\r\nX-Test: injected.txt" "text/plain",Wai.ChunkedBody,status400)
               ,([(hContentType,"application/octet-stream"),(metadataHeader,"{\"path\":\"C:/private\"}")],Wai.ChunkedBody,status400)]
      forM_ bads $ \(input,lengthMode,expected) -> do
        (result,reads)<-unread ctx.localApplication((mkRequest methodPost root input){Wai.requestBodyLength=lengthMode})
        result `shouldBe` expected; reads `shouldBe` 0
      boolean ctx.localEnv "SELECT NOT EXISTS(SELECT 1 FROM document_reservations) AND NOT EXISTS(SELECT 1 FROM documents)" `shouldReturn` True
  it "bounds chunked originals, rejects interrupted/hash/type failures and expires private failed reservations" $
    withLocalSandboxAppContext $ \ctx -> do
      (workspace,task)<-fixture ctx.localEnv
      requestId<-UUID.nextRandom
      let root=documents workspace task; headers=uploadHeaders requestId Doc.Source "original" "notes.txt" "text/plain"
      rawCall ctx.localApplication methodPost root headers "short" Wai.ChunkedBody >>= status status400
      rawCall ctx.localApplication methodPost root headers "original-extra" Wai.ChunkedBody >>= status status413
      rawCall ctx.localApplication methodPost root headers "different" Wai.ChunkedBody >>= status status413
      getCounts ctx.localApplication root [] `shouldReturn` Doc.DocumentCounts 0 0 0 0
      boolean ctx.localEnv "SELECT NOT EXISTS(SELECT 1 FROM document_payloads) AND count(*)=1 FROM document_reservations" `shouldReturn` True
      execute ctx.localEnv "UPDATE document_reservations SET expires_at=clock_timestamp()-interval '1 second'"
      Documents.expireReservations ctx.localEnv.pool workspace
      boolean ctx.localEnv "SELECT NOT EXISTS(SELECT 1 FROM document_reservations)" `shouldReturn` True
      _<-call ctx.localApplication methodPost root headers "original" >>= document
      call ctx.localApplication methodPost root headers "changed!" >>= status status400
      newRequest<-UUID.nextRandom
      call ctx.localApplication methodPost root(uploadHeaders newRequest Doc.Source "MZbad" "image.png" "image/png") "MZbad" >>= status status415
  it "binds request UUID to full manifest and role placement without duplicate publication" $
    withLocalSandboxAppContext $ \ctx -> do
      (workspace,task)<-fixture ctx.localEnv
      requestId<-UUID.nextRandom
      let root=documents workspace task; bytes="exact"
      first<-call ctx.localApplication methodPost root(uploadHeaders requestId Doc.Source bytes "notes.txt" "text/plain")(LBS.fromStrict bytes) >>= document
      call ctx.localApplication methodPost root(uploadHeaders requestId Doc.Evidence bytes "notes.txt" "text/plain")(LBS.fromStrict bytes) >>= status status409
      call ctx.localApplication methodPost root(uploadHeaders requestId Doc.Source bytes "different.txt" "text/plain")(LBS.fromStrict bytes) >>= status status409
      (a,b)<-concurrently
        (call ctx.localApplication methodPost root(uploadHeaders requestId Doc.Source bytes "notes.txt" "text/plain")(LBS.fromStrict bytes) >>= document)
        (call ctx.localApplication methodPost root(uploadHeaders requestId Doc.Source bytes "notes.txt" "text/plain")(LBS.fromStrict bytes) >>= document)
      (a.id,b.id) `shouldBe` (first.id,first.id)
      getCounts ctx.localApplication root [] `shouldReturn` Doc.DocumentCounts 1 0 0 0
  it "does not let a bad exact concurrent retry abort a valid in-flight upload reservation" $
    withLocalSandboxAppContext $ \ctx -> do
      (workspace,task)<-fixture ctx.localEnv
      requestId<-UUID.nextRandom
      entered<-newEmptyMVar; release<-newEmptyMVar; firstRead<-newIORef True
      result<-newIORef status500
      let root=documents workspace task; headers=uploadHeaders requestId Doc.Source "valid" "notes.txt" "text/plain"
          body=atomicModifyIORef' firstRead(\first -> (False,first)) >>= \first -> if first then putMVar entered() >> takeMVar release >> pure "valid" else pure ""
          request=(mkRequest methodPost root headers){Wai.requestBody=body,Wai.requestBodyLength=Wai.ChunkedBody}
          running=ctx.localApplication request(\response -> writeIORef result(Wai.responseStatus response) >> pure ResponseReceived)
      withAsync running $ \worker -> do
        ready<-timeout 3000000(takeMVar entered)
        ready `shouldBe` Just()
        call ctx.localApplication methodPost root headers "wrong" >>= status status400
        putMVar release()
        _<-wait worker
        readIORef result `shouldReturn` status201
      getCounts ctx.localApplication root [] `shouldReturn` Doc.DocumentCounts 1 0 0 0
  it "serializes simultaneous HTTP quota admission without partial publication" $
    withLocalSandboxAppContext $ \ctx -> do
      (workspace,task)<-fixture ctx.localEnv
      execute ctx.localEnv("INSERT INTO document_budgets(workspace_id,raw_limit) VALUES('"<>uuid workspace<>"',6)")
      a<-UUID.nextRandom; b<-UUID.nextRandom
      let root=documents workspace task
      (first,second)<-concurrently
        (call ctx.localApplication methodPost root(uploadHeaders a Doc.Source "first!" "a.txt" "text/plain") "first!")
        (call ctx.localApplication methodPost root(uploadHeaders b Doc.Source "second" "b.txt" "text/plain") "second")
      map simpleStatus[first,second] `shouldMatchList` [status201,status409]
      getCounts ctx.localApplication root [] `shouldReturn` Doc.DocumentCounts 1 0 0 0
  it "computes3document+27terminated evidence as30 independently of list cursors and failed preview state" $
    withLocalSandboxAppContext $ \ctx -> do
      (workspace,task)<-fixture ctx.localEnv
      let root=documents workspace task
      forM_ [1::Int .. 3] $ \_ -> do requestId<-UUID.nextRandom; _<-call ctx.localApplication methodPost root(uploadHeaders requestId Doc.Evidence "evidence" "notes.txt" "text/plain") "evidence" >>= document; pure()
      sourceRequest<-UUID.nextRandom
      _<-call ctx.localApplication methodPost root(uploadHeaders sourceRequest Doc.Source "source" "source.txt" "text/plain") "source" >>= document
      execute ctx.localEnv("UPDATE tasks SET status='in_progress' WHERE id='"<>uuid task<>"'")
      let createAt target=do identifier<-UUID.nextRandom; AgentDB.createSession ctx.localEnv.pool(Sessions.CreateSession identifier workspace "jsonl" "HTTP count fixture" Nothing Nothing target)
          seal row=AgentDB.terminateSession ctx.localEnv.pool workspace row.id(Sessions.TerminateSession row.committedBytes row.revision 0(payloadSha256 ""))
      forM_ [1::Int .. 27] $ \_ -> createAt task >>= seal
      _<-createAt task
      child<-Task.createTask ctx.localEnv.pool(Types.CreateTask workspace Nothing(Just task) "child" Nothing Nothing Nothing)
      execute ctx.localEnv("UPDATE tasks SET status='in_progress' WHERE id='"<>uuid child.id<>"'")
      _<-createAt child.id >>= seal
      execute ctx.localEnv "UPDATE document_previews SET state='failed',error_code='renderer_failed'"
      page<-call ctx.localApplication methodGet(root<>"?role=evidence&limit=1") [] "" >>= decoded @Doc.DocumentPage
      length page.items `shouldBe` 1
      case page.nextCursor of
        Nothing -> expectationFailure "Expected evidence continuation"
        Just cursor -> do
          let query=renderQuery True[("role",Just "evidence"),("limit",Just "1"),("before_created_at",Just(B8.pack(iso8601Show cursor.createdAt))),("before_id",Just(uuid cursor.id))]
          second<-call ctx.localApplication methodGet(root<>query) [] "" >>= decoded @Doc.DocumentPage
          length second.items `shouldBe` 1
          map (.id) second.items `shouldSatisfy` all (`notElem` map (.id) page.items)
      getCounts ctx.localApplication root [] `shouldReturn` Doc.DocumentCounts 1 3 27 30
      boolean ctx.localEnv "SELECT EXISTS(SELECT 1 FROM change_stream_outbox WHERE envelope::text LIKE '%task_documents:%') AND EXISTS(SELECT 1 FROM change_stream_outbox WHERE envelope::text LIKE '%sessions:%')" `shouldReturn` True
  it "denies removed/foreign/deleted task links and restores retained originals" $
    withLocalSandboxAppContext $ \ctx -> do
      (workspace,task)<-fixture ctx.localEnv
      requestId<-UUID.nextRandom
      let root=documents workspace task
      doc<-call ctx.localApplication methodPost root(uploadHeaders requestId Doc.Source "retained" "notes.txt" "text/plain") "retained" >>= document
      foreignWorkspace<-(.id) <$> createTestWorkspace ctx.localEnv "foreign"
      call ctx.localApplication methodGet(documents foreignWorkspace task<>"/"<>uuid doc.id<>"/original") [] "" >>= status status404
      execute ctx.localEnv("UPDATE tasks SET deleted_at=clock_timestamp() WHERE id='"<>uuid task<>"'")
      call ctx.localApplication methodGet(root<>"/counts") [] "" >>= status status404
      (deletedResult,deletedReads)<-unread ctx.localApplication(mkRequest methodPost root(uploadHeaders requestId Doc.Source "retained" "notes.txt" "text/plain"))
      deletedResult `shouldBe` status404; deletedReads `shouldBe` 0
      execute ctx.localEnv("UPDATE tasks SET deleted_at=NULL WHERE id='"<>uuid task<>"'")
      call ctx.localApplication methodGet(root<>"/"<>uuid doc.id<>"/original") [] "" >>= status status200
      call ctx.localApplication methodDelete(root<>"/"<>uuid doc.id<>"/source") [] "" >>= status status204
      call ctx.localApplication methodGet(root<>"/"<>uuid doc.id<>"/original") [] "" >>= status status404
  it "keeps active HTML inert as a forced original attachment and refuses corrupt stored payloads before200" $
    withLocalSandboxAppContext $ \ctx -> do
      (workspace,task)<-fixture ctx.localEnv
      requestId<-UUID.nextRandom
      let root=documents workspace task; bytes="<script>fetch('/api/v1/tasks')</script>"
      doc<-call ctx.localApplication methodPost root(uploadHeaders requestId Doc.Source bytes "active.html" "text/html")(LBS.fromStrict bytes) >>= document
      result<-call ctx.localApplication methodGet(root<>"/"<>uuid doc.id<>"/original") [] ""
      simpleStatus result `shouldBe` status200
      lookup hContentType result.simpleHeaders `shouldBe` Just "application/octet-stream"
      bracket_
        (execute ctx.localEnv "ALTER TABLE document_payload_parts DISABLE TRIGGER document_part_guard")
        (execute ctx.localEnv "ALTER TABLE document_payload_parts ENABLE TRIGGER document_part_guard") $ do
          execute ctx.localEnv "UPDATE document_payload_parts SET data='corrupt'"
          call ctx.localApplication methodGet(root<>"/"<>uuid doc.id<>"/original") [] "" >>= status status503
  it "reauthorizes between bounded original chunks and aborts after workspace role withdrawal" $
    withDeployedSandboxAppContext $ \ctx -> do
      (workspace,task)<-fixture ctx.deployedEnv
      (_,editor)<-audience ctx workspace(Just Auth.WorkspaceRoleEdit)
      (readerUser,reader)<-audience ctx workspace(Just Auth.WorkspaceRoleRead)
      requestId<-UUID.nextRandom
      let bytes=BS.replicate(2*1048576+1)120; root=documents workspace task
      doc<-call ctx.deployedApplication methodPost root(uploadHeaders requestId Doc.Source bytes "large.txt" "text/plain"<>editor)(LBS.fromStrict bytes) >>= document
      sent<-newIORef(0::Int)
      attempted<-try @SomeException $ ctx.deployedApplication(mkRequest methodGet(root<>"/"<>uuid doc.id<>"/original") reader) $ \response -> do
        let (_,_,withBody)=Wai.responseToStream response
        withBody $ \stream -> stream (\builder -> do
          modifyIORef' sent(+fromIntegral(LBS.length(Builder.toLazyByteString builder)))
          _<-Auth.deleteWorkspaceMembership ctx.deployedEnv.pool workspace readerUser
          pure()) (pure())
        pure ResponseReceived
      case attempted of Left _ -> pure(); Right _ -> expectationFailure "Revoked original transfer completed"
      readIORef sent `shouldReturn` 1048576
  it "publishes the document OpenAPI surface and browser metadata CORS contract" $
    withLocalSandboxAppContext $ \ctx -> do
      result<-call ctx.localApplication methodGet "/api/v1/openapi.json" [] ""
      simpleStatus result `shouldBe` status200
      value<-either fail pure(eitherDecode @Value result.simpleBody)
      field ["paths","/api/v1/workspaces/{workspaceId}/tasks/{taskId}/documents","post"] value `shouldSatisfy` (/=Nothing)
      let rootPath="/api/v1/workspaces/{workspaceId}/tasks/{taskId}/documents"
      field ["paths",rootPath,"post","responses","201","content","application/json","schema","$ref"] value `shouldBe` Just(String "#/components/schemas/TaskDocument")
      field ["paths",rootPath<>"/counts","get","responses","200","content","application/json","schema","$ref"] value `shouldBe` Just(String "#/components/schemas/TaskDocumentCounts")
      forM_ ["","/counts","/{documentId}","/{documentId}/original","/{documentId}/evidence","/{documentId}/source"] $ \suffix ->
        forM_ (maybe [] schemaReferences(field ["paths",rootPath<>suffix] value)) $ \reference -> case T.stripPrefix "#/components/schemas/" reference of
          Nothing -> expectationFailure "Document response has an unexpected reference namespace"
          Just component -> field ["components","schemas",component] value `shouldSatisfy` (/=Nothing)
      (workspace,task)<-fixture ctx.localEnv
      cors<-call ctx.localApplication methodOptions(documents workspace task)
        [("Origin","http://localhost"),("Access-Control-Request-Method","POST"),("Access-Control-Request-Headers","content-type,x-hmem-document-metadata")] ""
      simpleStatus cors `shouldBe` status200

fixture :: TestEnv -> IO(UUID,UUID)
fixture env=do
  workspace<-(.id) <$> createTestWorkspace env "document-http"
  task<-Task.createTask env.pool(Types.CreateTask workspace Nothing Nothing "document task" Nothing Nothing Nothing)
  pure(workspace,task.id)
documents :: UUID -> UUID -> ByteString
documents workspace task="/api/v1/workspaces/"<>uuid workspace<>"/tasks/"<>uuid task<>"/documents"
uuid :: UUID -> ByteString
uuid=TE.encodeUtf8 . T.pack . show
uploadHeaders :: UUID -> Doc.DocumentRole -> ByteString -> Text -> Text -> [Header]
uploadHeaders requestId role bytes filename mediaType=
  [(hContentType,"application/octet-stream"),(metadataHeader,LBS.toStrict(encode(object["request_id" .= requestId,"role" .= role,"manifest" .= Doc.OriginalManifest (fromIntegral(BS.length bytes))(payloadSha256 bytes) filename filename mediaType])))]
mkRequest :: Method -> ByteString -> [Header] -> Wai.Request
mkRequest method path headers=Wai.defaultRequest{Wai.requestMethod=method,Wai.rawPathInfo=raw,Wai.pathInfo=filter(not . T.null)(T.splitOn "/"(TE.decodeUtf8 raw)),Wai.rawQueryString=query,Wai.queryString=parseQuery query,Wai.requestHeaders=headers}
  where(raw,query)=BS.break(==63) path
call :: Wai.Application -> Method -> ByteString -> [Header] -> LBS.ByteString -> IO SResponse
call app method path headers body=rawCall app method path headers body(Wai.KnownLength(fromIntegral(LBS.length body)))
rawCall :: Wai.Application -> Method -> ByteString -> [Header] -> LBS.ByteString -> Wai.RequestBodyLength -> IO SResponse
rawCall app method path headers body lengthMode=runSession(srequest(SRequest((mkRequest method path headers){Wai.requestBodyLength=lengthMode}) body)) app
postJSON :: Wai.Application -> ByteString -> [Header] -> Value -> IO SResponse
postJSON app path headers value=call app methodPost path((hContentType,"application/json"):headers)(encode value)
status :: Status -> SResponse -> IO()
status expected response=simpleStatus response `shouldBe` expected
decoded :: FromJSON a => SResponse -> IO a
decoded response=do simpleStatus response `shouldBe` status200; either fail pure(eitherDecode response.simpleBody)
document :: SResponse -> IO Doc.Document
document response=do simpleStatus response `shouldSatisfy` (`elem` [status200,status201]); either fail pure(eitherDecode response.simpleBody)
getCounts :: Wai.Application -> ByteString -> [Header] -> IO Doc.DocumentCounts
getCounts app root headers=call app methodGet(root<>"/counts") headers "" >>= decoded
unread :: Wai.Application -> Wai.Request -> IO(Status,Int)
unread app request=do
  reads<-newIORef(0::Int); result<-newIORef status500
  let next=modifyIORef' reads(+1) >> throwIO(userError "Unexpected body consumption")
  _<-app request{Wai.requestBody=next}(\response -> writeIORef result(Wai.responseStatus response) >> pure ResponseReceived)
  (,) <$> readIORef result <*> readIORef reads
audience :: DeployedSandboxApp -> UUID -> Maybe Auth.WorkspaceRole -> IO(UUID,[Header])
audience ctx workspace role=do
  user<-createDeployedSandboxUser ctx.deployedEnv False False
  forM_ role $ \grant -> do _<-Auth.upsertWorkspaceMembership ctx.deployedEnv.pool workspace(Auth.UpsertWorkspaceMembership user grant) Nothing; pure()
  token<-issueDeployedSandboxPAT ctx.deployedEnv user "document audience"
  pure(user,[("Authorization","Bearer "<>TE.encodeUtf8 token.rawToken),("X-HMem-Bot-Identity","{\"role\":\"Document HTTP Test\",\"model-id\":\"test:fixture:default\"}")])
execute :: TestEnv -> ByteString -> IO()
execute env sql=Pool.runSession env.pool(S.sql sql)
boolean :: TestEnv -> ByteString -> IO Bool
boolean env sql=Pool.runSession env.pool(S.statement ()(St.Statement sql E.noParams(D.singleRow(D.column(D.nonNullable D.bool))) False))
field :: [Text] -> Value -> Maybe Value
field [] value=Just value
field(key:rest)(Object obj)=KM.lookup(Key.fromText key)obj >>= field rest
field _ _=Nothing
schemaReferences :: Value -> [Text]
schemaReferences(Object obj)=maybe [] (\case String reference -> [reference]; _ -> [])(KM.lookup "$ref" obj)<>concatMap schemaReferences(KM.elems obj)
schemaReferences(Array values)=concatMap schemaReferences(toList values)
schemaReferences _=[]
