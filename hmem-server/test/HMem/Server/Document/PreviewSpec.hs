module HMem.Server.Document.PreviewSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (mapConcurrently)
import Control.Exception (SomeException,try,onException)
import Control.Monad (forM_,void,unless)
import Data.Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as B8
import Data.ByteString.Lazy qualified as LBS
import Data.Foldable (toList)
import Data.List (sort)
import Data.Text (Text)
import Data.Text qualified
import Data.Text.Encoding qualified as TE
import Data.UUID (UUID)
import Data.UUID.V4 qualified as UUID
import Data.UUID qualified as UUIDText
import Hasql.Decoders qualified as D
import Hasql.Encoders qualified as E
import Hasql.Session qualified as S
import Hasql.Statement qualified as St
import Network.HTTP.Types
import Network.HTTP.Types.Header (hHost,hSetCookie)
import Network.Wai qualified as Wai
import Network.Wai.Test (SResponse(..),SRequest(..))
import Network.Wai.Test qualified as WaiTest
import System.FilePath ((</>))
import System.Timeout (timeout)
import Text.Printf (printf)
import System.IO (hFlush,stdout,hPutStrLn,stderr)
import Test.Hspec
import HMem.DB.Document qualified as Documents
import HMem.DB.Auth qualified as Auth
import HMem.DB.DocumentGrant qualified as Grants
import HMem.DB.DocumentPreview qualified as Jobs
import HMem.DB.Pool (runSession)
import HMem.DB.Task qualified as Task
import HMem.DB.TestHarness
import HMem.Config qualified as C
import HMem.Document.Codec (payloadSha256)
import HMem.Document.Grant (PreviewStatus(..),PreviewOutput(..))
import HMem.Document.Preview
import HMem.Document.Types qualified as Doc
import HMem.Server.CtlMigrations
import HMem.Server.AuthTokens qualified as Tokens
import HMem.Server.App (mkAppWithDocumentAuthority)
import HMem.Server.AccessTracker (newAccessTracker)
import HMem.Server.WebSocket (newWSState)
import HMem.Server.Document.Grant qualified as Delivery
import HMem.Server.Document.PreviewOrigin (newPreviewApplication)
import HMem.Server.TestHarness
import HMem.Server.Document.Worker
import HMem.Types qualified as Types

spec :: Spec
spec=describe "Document preview integration" $ do
  it "bundles and installs the current V060 schema in the native artifact" $
    withTestSandbox $ \sandbox->do
      let name="V060__document_preview_jobs.sql"
      case lookup name bundledMigrations of
        Nothing->expectationFailure "Native migration inventory omitted V060"
        Just bytes->do
          bytes `shouldSatisfy` BS.isInfixOf "CREATE FUNCTION hmem_document_preview_publish_run"
          source<-BS.readFile(sandbox.sandboxRepoRoot </> "hmem-server" </> "migrations" </> name)
          bytes `shouldBe` source
          (destination,names)<-installBundledMigrations(sandbox.sandboxTmpDir </> "native-install")
          names `shouldContain` [name]
          BS.readFile(destination </> name) `shouldReturn` source
          sort(map fst bundledMigrations) `shouldBe` sort names
  it "renders real PNG, active PDF and styled HTML through the qualified launcher and durable controller" $
    withTestEnv $ \env->do
      cfg<-configuration
      Jobs.heartbeat env.pool cfg
      fixtures<-mapM(\(bytes,name,media)->seed env bytes name media)
        [(png,"image.png","image/png"),(activePDF,"active.pdf","application/pdf"),
         ("<!doctype html><html><body><script>BAD_SCRIPT</script><h1 style=\"color:red\">Safe title</h1><iframe src=\"https://evil.test\"></iframe><p onclick=\"BAD_HANDLER\">Hello</p></body></html>","styled.html","text/html")]
      (withPreviewWorker env.pool cfg $ do
        statuses<-mapConcurrently(awaitReady env)fixtures
        -- Publish exact owner identity before any subsequent output assertion
        -- can fail and the borrowed database is torn down.
        forM_ statuses $ \status->case status.generationId of
          Just owner->putStrLn("preview_verified_owner="<>UUIDText.toString owner)>>hFlush stdout
          Nothing->expectationFailure "Ready output has no owner generation"
        (application,styled,headers)<-readApplications env fixtures
        forM_(zip fixtures statuses) $ \(f,status)->do
          status.state `shouldBe` "ready"
          status.retryable `shouldBe` False
          metadata<-maybe(fail "Missing verified work receipt")pure status.generationMetadata
          metadata.originalSha256 `shouldBe` payloadSha256 f.bytes
          metadata.policyVersion `shouldBe` previewPolicyVersion
          metadata.imageId `shouldBe` qualifiedPreviewImage
          metadata.outputCount `shouldBe` length status.outputs
          metadata.animatedFirstFrame `shouldBe` False
          Documents.readOriginal env.pool f.workspace f.task f.document.id `shouldReturn` f.bytes
          case f.document.mediaType of
            "text/html"->do
              length status.outputs `shouldBe` 1
              (_,html)<-Documents.readDerivative env.pool f.workspace f.task f.document.id "styled_html_preview" 0
              html `shouldSatisfy` BS.isInfixOf "Safe title"
              forM_["BAD_SCRIPT","BAD_HANDLER","<iframe","https://evil.test"] $ \unsafe->
                BS.isInfixOf unsafe html `shouldBe` False
            "application/pdf"->do
              metadata.pageCount `shouldBe` 1
              sort[(o.variant,o.page)|o<-status.outputs] `shouldBe`
                [("page",1),("thumbnail",0),("thumbnail",1)]
            _->do
              metadata.pageCount `shouldBe` 0
              metadata.decodedPixels `shouldBe` 1
              sort[(o.variant,o.page)|o<-status.outputs] `shouldBe` [("page",0),("thumbnail",0)]
          forM_ status.outputs $ \output->do
            (_,bytes)<-Documents.readDerivative env.pool f.workspace f.task f.document.id output.variant output.page
            payloadSha256 bytes `shouldBe` output.rawSha256
            fromIntegral(BS.length bytes) `shouldBe` output.rawBytes
          checkHTTPGeneration env application styled headers f status
        -- Independent links do not create another physical/render generation.
        forM_ fixtures $ \f->do
          request<-UUID.nextRandom
          _<-Documents.addEvidence env.pool f.workspace f.task f.document.id request
          Jobs.retry env.pool f.workspace f.task f.document.id `shouldReturn` False
        count env "SELECT count(*) FROM document_preview_runs" `shouldReturn` 3
        count env "SELECT count(*) FROM document_preview_runs WHERE released_at IS NULL" `shouldReturn` 0
        count env "SELECT count(*) FROM document_reservations WHERE kind='preview'" `shouldReturn` 0
        ) `onException` reportOwners env
      -- Restart preserves ready generations and does not replay completed jobs.
      (withPreviewWorker env.pool cfg $threadDelay 500000) `onException` reportOwners env
      count env "SELECT count(*) FROM document_preview_runs" `shouldReturn` 3

  it "reports quota exhaustion before render and retains the downloadable original" $
    withTestEnv $ \env->do
      cfg<-configuration
      Jobs.heartbeat env.pool cfg
      f<-seed env png "quota.png" "image/png"
      runSession env.pool(S.sql("UPDATE document_budgets SET binary_limit=1048576 WHERE workspace_id='"<>uuid f.workspace<>"'"))
      (withPreviewWorker env.pool cfg $ do
        let awaitFailure=Grants.previewStatus env.pool f.workspace f.task f.document.id >>= \case
              Just status | status.state=="failed"->pure status
              _->threadDelay 50000 >>awaitFailure
        terminal<-timeout 35000000 awaitFailure
        status<-maybe(fail "Quota rejection did not settle")pure terminal
        status.errorCode `shouldBe` Just "quota_exceeded"
        status.retryable `shouldBe` True
        status.generationId `shouldBe` Nothing
        count env "SELECT count(*) FROM document_preview_runs" `shouldReturn` 1
        count env "SELECT count(*) FROM document_preview_runs WHERE released_at IS NOT NULL AND native_exited AND cleanup_verified AND NOT creation_uncertain AND plan_sha256 IS NULL" `shouldReturn` 1
        count env "SELECT count(*) FROM document_reservations WHERE kind='preview'" `shouldReturn` 0
        count env "SELECT count(*) FROM document_generations" `shouldReturn` 0
        owner<-runSession env.pool(S.statement ()(St.Statement "SELECT id FROM document_preview_runs" E.noParams
          (D.singleRow(D.column(D.nonNullable D.uuid)))True))
        putStrLn("preview_quota_owner="<>UUIDText.toString owner)>>hFlush stdout
        (application,_,headers)<-readApplications env [f]
        original<-call application methodGet(documentBase f<>"/original")headers
        check status200 original
        LBS.toStrict original.simpleBody `shouldBe` png
        ) `onException` reportOwners env

  it "round-trips a verified True animation receipt through durable metadata and authorized HTTP" $
    withTestEnv $ \env->do
      -- This is a storage/transport receipt fixture. Actual animation decoding
      -- and first-frame work reuse the unchanged qualified worker's real cases.
      cfg<-configuration
      Jobs.heartbeat env.pool cfg
      f<-seed env png "animation-receipt.png" "image/png"
      owner<-UUID.nextRandom;controller<-UUID.nextRandom;token<-UUID.nextRandom
      Just run<-Jobs.claim env.pool cfg f.workspace f.document.id owner controller token f.document.mediaType
      Jobs.started env.pool run 42
      Jobs.inspected env.pool run
      let metadata=PreviewMetadata(payloadSha256 "animation receipt plan")run.rawSha256
            (payloadSha256(canonicalPreviewLimits run.limits))run.policyVersion run.imageId 2 0 600 0 True
      Jobs.reserve env.pool run metadata.planSha256 2 0 105906176
      Jobs.rendering env.pool run
      Jobs.closed env.pool run True False
      Jobs.publish env.pool run metadata
        [Documents.Derivative "page" 0 "image/png" png,Documents.Derivative "thumbnail" 0 "image/png" png]
      Just status<-Grants.previewStatus env.pool f.workspace f.task f.document.id
      status.generationMetadata `shouldBe` Just metadata
      (application,styled,headers)<-readApplications env [f]
      checkHTTPGeneration env application styled headers f status
      response<-call application methodGet(documentBase f<>"/previews/current")headers
      value<-decodeResponse status200 response
      at ["generation_metadata","animated_first_frame"] value `shouldBe` Just(Bool True)
      at ["generation_metadata","decoded_pixels"] value `shouldBe` Just(Number 600)
      at ["generation_metadata","verification"] value `shouldBe` Just(String "verified")

  it "authorizes preview retry with workspace roles and browser CSRF without starting the task" $
    withDeployedSandboxAppContext $ \ctx->do
      let env=ctx.deployedEnv
          app=ctx.deployedApplication
      f<-seed env png "retry.png" "image/png"
      let path="/api/v1/workspaces/"<>uuid f.workspace<>"/tasks/"<>uuid f.task<>"/documents/"<>uuid f.document.id<>"/previews/retry"
      call app methodPost path [] >>=check status401
      reader<-createDeployedSandboxUser env False False
      _<-Auth.upsertWorkspaceMembership env.pool f.workspace(Auth.UpsertWorkspaceMembership reader Auth.WorkspaceRoleRead)Nothing
      readToken<-issueDeployedSandboxPAT env reader "reader"
      call app methodPost path(bearer readToken) >>=check status403
      editor<-createDeployedSandboxUser env False False
      _<-Auth.upsertWorkspaceMembership env.pool f.workspace(Auth.UpsertWorkspaceMembership editor Auth.WorkspaceRoleEdit)Nothing
      _<-createDeployedSandboxAuthSession env editor "preview-retry-session" "preview-retry-csrf"
      let cookie=[(hCookie,"hmem_session=preview-retry-session; hmem_csrf=preview-retry-csrf")]
          headers=("X-CSRF-Token","preview-retry-csrf"):cookie
      call app methodPost path cookie >>=check status403
      runSession env.pool(S.sql("UPDATE tasks SET status='done' WHERE id='"<>uuid f.task<>"'"))
      cfg<-configuration
      Jobs.heartbeat env.pool cfg
      accepted<-call app methodPost path headers
      check status202 accepted
      eitherDecode @Value accepted.simpleBody `shouldBe` Right(object["accepted" .= True])
      again<-call app methodPost path headers
      check status200 again
      eitherDecode @Value again.simpleBody `shouldBe` Right(object["accepted" .= False])
      state<-runSession env.pool(S.statement ()(St.Statement("SELECT status FROM tasks WHERE id='"<>uuid f.task<>"'")E.noParams
        (D.singleRow(D.column(D.nonNullable D.text)))True))
      state `shouldBe` "done"
      count env "SELECT count(*) FROM document_preview_runs" `shouldReturn` 0
      metadata<-call app methodGet(BS.take(BS.length path-5)path<>"current")(bearer readToken)
      check status200 metadata
      value<-either fail pure(eitherDecode @Value metadata.simpleBody)
      at ["generation_metadata"] value `shouldBe` Just Null
      at ["retryable"] value `shouldBe` Just(Bool False)
      Documents.removeSource env.pool f.workspace f.task f.document.id
      call app methodPost path headers >>=check status404
      schema<-call app methodGet "/api/v1/openapi.json" headers
      check status200 schema
      wire<-either fail pure(eitherDecode @Value schema.simpleBody)
      at ["paths","/api/v1/workspaces/{workspaceId}/tasks/{taskId}/documents/{documentId}/previews/retry","post","responses","202"] wire `shouldSatisfy` (/=Nothing)
      let metadataPath=["components","schemas","TaskDocumentPreviewMetadata","properties","generation_metadata","oneOf"]
      case at metadataPath wire of
        Just(Array branches) | [absent,unknown,verified]<-toList branches->do
          absent `shouldBe` object["type" .= String "string","nullable" .= True,"enum" .= [Null]]
          generation<-UUID.nextRandom
          let legacy=PreviewStatus "ready" Nothing (Just generation) (Just previewPolicyVersion) [] Nothing False
          at ["generation_metadata"] (toJSON legacy) `shouldBe` Just(object["verification" .= String "unknown"])
          at ["type"] unknown `shouldBe` Just(String "object")
          at ["required"] unknown `shouldBe` Just(toJSON [String "verification"])
          at ["additionalProperties"] unknown `shouldBe` Just(Bool False)
          at ["properties","verification","enum"] unknown `shouldBe` Just(toJSON [String "unknown"])
          at ["type"] verified `shouldBe` Just(String "object")
          at ["properties","verification","enum"] verified `shouldBe` Just(toJSON [String "verified"])
          at ["required"] verified `shouldBe` Just(toJSON (["verification","plan_sha256","original_sha256"
            ,"limits_sha256","policy_version","image_id","output_count","page_count","decoded_pixels"
            ,"pdf_decoded_bytes","animated_first_frame"]::[Text]))
        branches->expectationFailure("Expected null, unknown and verified metadata schemas, got "<>show branches)

-- Only bounded owner/phase/closure metadata enters failure diagnostics. This
-- runs before borrowed fixture teardown and never replaces the primary error.
reportOwners :: TestEnv -> IO()
reportOwners env=void $ try @SomeException $ do
  receipt<-timeout 2000000 $ runSession env.pool(S.statement ()(St.Statement
    "SELECT coalesce(jsonb_agg(to_jsonb(r)),'[]'::jsonb) FROM (SELECT id,document_id,phase,native_pid,native_exited,cleanup_verified,creation_uncertain,released_at FROM document_preview_runs ORDER BY created_at DESC,id LIMIT 32) r" E.noParams
    (D.singleRow(D.column(D.nonNullable D.jsonb)))True))
  hPutStrLn stderr("preview_owner_failure_receipt="<>maybe "unavailable" show receipt)

at :: [Text] -> Value -> Maybe Value
at [] value=Just value
at (key:rest)(Object value)=KM.lookup(Key.fromText key)value >>=at rest
at _ _=Nothing
uuid :: UUID -> ByteString
uuid=TE.encodeUtf8 . Data.Text.pack . show
bearer :: Tokens.IssuedAccessToken -> [Header]
bearer token=[(hAuthorization,"Bearer "<>TE.encodeUtf8 token.rawToken),
  ("X-HMem-Bot-Identity","{\"role\":\"Preview test\",\"model-id\":\"test:preview:default\"}")]
call :: Wai.Application -> Method -> ByteString -> [Header] -> IO SResponse
call app method path headers=callBody app method path headers ""
callBody :: Wai.Application -> Method -> ByteString -> [Header] -> LBS.ByteString -> IO SResponse
callBody app method target headers body=WaiTest.runSession(WaiTest.srequest(SRequest request body))app
  where
    (path,query)=BS.break(==63)target
    request=Wai.defaultRequest{Wai.requestMethod=method,Wai.rawPathInfo=path,
      Wai.pathInfo=filter(not . Data.Text.null)(Data.Text.splitOn "/"(TE.decodeUtf8 path)),
      Wai.rawQueryString=query,Wai.queryString=parseQuery query,Wai.requestHeaders=headers,
      Wai.requestBodyLength=Wai.KnownLength(fromIntegral(LBS.length body))}
check :: Status -> SResponse -> IO()
check expected response=response.simpleStatus `shouldBe` expected
decodeResponse :: Status -> SResponse -> IO Value
decodeResponse expected response=check expected response >>either fail pure(eitherDecode response.simpleBody)
documentBase :: Fixture -> ByteString
documentBase f="/api/v1/workspaces/"<>uuid f.workspace<>"/tasks/"<>uuid f.task<>"/documents/"<>uuid f.document.id

readApplications :: TestEnv -> [Fixture] -> IO(Wai.Application,Wai.Application,[Header])
readApplications env fixtures=do
  let base=mkDeployedSandboxConfig env
      cfg=base{C.rateLimit=base.rateLimit{C.rlEnabled=False}}
      delivery=C.DocumentDeliveryConfig(Just "https://app.example.test")(Just "https://download.example.test")
        (Just "https://preview.example.net")(Just [])"127.0.0.1"(Just 8421)
  authority<-Delivery.newDocumentAuthority env.pool cfg.auth cfg.server delivery
  tracker<-newAccessTracker env.pool 3600
  state<-newWSState
  app<-mkAppWithDocumentAuthority authority Nothing Nothing cfg.changeStream id cfg.auth cfg.cors cfg.rateLimit env.pool tracker state Nothing False
  styled<-newPreviewApplication authority cfg.rateLimit
  reader<-createDeployedSandboxUser env False False
  forM_ fixtures $ \f->void(Auth.upsertWorkspaceMembership env.pool f.workspace(Auth.UpsertWorkspaceMembership reader Auth.WorkspaceRoleRead)Nothing)
  token<-issueDeployedSandboxPAT env reader "preview reader"
  pure(app,styled,bearer token)

checkHTTPGeneration :: TestEnv -> Wai.Application -> Wai.Application -> [Header] -> Fixture -> PreviewStatus -> IO()
checkHTTPGeneration env application styled headers f expected=do
  current<-call application methodGet(documentBase f<>"/previews/current")headers
  wire<-decodeResponse status200 current
  wire `shouldBe` toJSON expected
  original<-call application methodGet(documentBase f<>"/original")headers
  check status200 original
  LBS.toStrict original.simpleBody `shouldBe` f.bytes
  lookup hContentType original.simpleHeaders `shouldBe` Just "application/octet-stream"
  lookup "Content-Disposition" original.simpleHeaders `shouldSatisfy` maybe False(BS.isPrefixOf "attachment;")
  sources<-Documents.listDocuments env.pool f.workspace f.task Doc.Source Nothing 50
  association<-case sources.items of [source]->pure source.id;_->fail "Expected exact source association"
  generation<-maybe(fail "Ready generation missing")pure expected.generationId
  forM_ expected.outputs $ \output->do
    (_,bytes)<-Documents.readDerivative env.pool f.workspace f.task f.document.id output.variant output.page
    let pinned=documentBase f<>"/previews/"<>uuid generation<>"/"<>TE.encodeUtf8 output.variant<>"/"<>B8.pack(show output.page)
    if output.variant=="styled_html_preview"
      then call application methodGet pinned headers >>=check status400
      else do
        raster<-call application methodGet pinned headers
        check status200 raster
        LBS.toStrict raster.simpleBody `shouldBe` bytes
        lookup hContentType raster.simpleHeaders `shouldBe` Just(TE.encodeUtf8 output.mediaType)
    issued<-callBody application methodPost(documentBase f<>"/download-grants")((hContentType,"application/json"):headers)
      (encode(object["role" .= ("source"::Text),"association_id" .= association,"variant" .= output.variant,
        "page" .= output.page,"generation_id" .= generation,"ttl_seconds" .= (60::Int)]))
    grant<-decodeResponse status201 issued
    at ["association_id"] grant `shouldBe` Just(toJSON association)
    at ["generation_id"] grant `shouldBe` Just(toJSON generation)
    at ["manifest","raw_sha256"] grant `shouldBe` Just(String output.rawSha256)
    url<-case at ["url"] grant of Just(String value)->pure value;_->fail "Scoped grant URL missing"
    let suffix=snd(Data.Text.breakOn "/document-grants/" url)
    unless(not(Data.Text.null suffix))(fail "Scoped fixture URL has no representation route")
    served<-if output.variant=="styled_html_preview"
      then do
        call application methodGet(TE.encodeUtf8 suffix)[] >>=check status404
        call styled methodGet(TE.encodeUtf8 suffix)[(hHost,"preview.example.net"),("Sec-Fetch-Dest","document")]
      else call application methodGet(TE.encodeUtf8 suffix)[]
    check status200 served
    LBS.toStrict served.simpleBody `shouldBe` bytes
    lookup hCacheControl served.simpleHeaders `shouldBe` Just "no-store"
    lookup "Referrer-Policy" served.simpleHeaders `shouldBe` Just "no-referrer"
    lookup "X-Content-Type-Options" served.simpleHeaders `shouldBe` Just "nosniff"
    if output.variant=="styled_html_preview"
      then do
        lookup hContentType served.simpleHeaders `shouldBe` Just "text/html;charset=utf-8"
        lookup hSetCookie served.simpleHeaders `shouldBe` Nothing
        forM_ ["sandbox","script-src 'none'","connect-src 'none'","frame-ancestors https://app.example.test"] $ \policy->
          lookup "Content-Security-Policy" served.simpleHeaders `shouldSatisfy` maybe False(BS.isInfixOf policy)
      else lookup hContentType served.simpleHeaders `shouldBe` Just(TE.encodeUtf8 output.mediaType)
data Fixture=Fixture{workspace::UUID,task::UUID,document::Doc.Document,bytes::ByteString}
configuration :: IO PreviewConfig
configuration=do
  root<-resolveRepoRoot
  pure defaultPreviewConfig{enabled=True,launcherPath=Just(root </> "hmem-preview-worker" </> "launch.py")}
seed :: TestEnv -> ByteString -> Text -> Text -> IO Fixture
seed env bytes name media=do
  workspace<-createTestWorkspace env "real-previews"
  task<-Task.createTask env.pool(Types.CreateTask workspace.id Nothing Nothing "preview task" Nothing Nothing Nothing)
  request<-UUID.nextRandom
  document<-Documents.uploadDocument env.pool(Doc.UploadDocument workspace.id task.id request Doc.Source
    (Doc.OriginalManifest(fromIntegral(BS.length bytes))(payloadSha256 bytes)name name media))bytes
  pure(Fixture workspace.id task.id document bytes)
awaitReady :: TestEnv -> Fixture -> IO PreviewStatus
awaitReady env f=do
  status<-timeout 125000000 loop
  maybe(fail "Preview generation did not settle within its bounded total deadline")pure status
  where loop=Grants.previewStatus env.pool f.workspace f.task f.document.id >>= \case
          Just status | status.state=="ready"->pure status
          Just status | elem status.state ["failed","unsupported"]->do
            owners<-runSession env.pool(S.statement ()(St.Statement
              ("SELECT coalesce(jsonb_agg(jsonb_build_object('id',id,'phase',phase,'native_pid',native_pid,'native_exited',native_exited,'cleanup_verified',cleanup_verified,'creation_uncertain',creation_uncertain,'released_at',released_at)),'[]'::jsonb) FROM document_preview_runs WHERE document_id='"<>uuid f.document.id<>"'") E.noParams
              (D.singleRow(D.column(D.nonNullable D.jsonb)))True))
            fail("Preview failed: "<>show status.errorCode<>"; physical_owners="<>show owners)
          _->threadDelay 50000 >>loop
count :: TestEnv -> ByteString -> IO Int
count env sql=fromIntegral <$>runSession env.pool(S.statement ()(St.Statement sql E.noParams
  (D.singleRow(D.column(D.nonNullable D.int8)))True))
png :: ByteString
png=BS.pack[137,80,78,71,13,10,26,10,0,0,0,13,73,72,68,82,0,0,0,1,0,0,0,1,8,4,0,0,0,181,28,12,2,0,0,0,11,73,68,65,84,120,218,99,100,248,15,0,1,5,1,1,39,24,227,102,0,0,0,0,73,69,78,68,174,66,96,130]
activePDF :: ByteString
activePDF=
  let objects=["<< /Type /Catalog /Pages 2 0 R /OpenAction 5 0 R >>",
        "<< /Type /Pages /Kids [3 0 R] /Count 1 >>",
        "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 100 140] /Contents 4 0 R /Annots [6 0 R] >>",
        "<< /Length 0 >>\nstream\n\nendstream",
        "<< /S /JavaScript /JS (app.alert\\(EVIL\\)) >>",
        "<< /Type /Annot /Subtype /Link /Rect [0 0 10 10] /A << /S /URI /URI (https://evil.test) >> >>"]
      entries=[B8.pack(show index)<>" 0 obj\n"<>body<>"\nendobj\n"|(index,body)<-zip[1::Int ..]objects]
      offsets=scanl(\offset bytes->offset+BS.length bytes)9 entries
      body="%PDF-1.7\n"<>BS.concat entries
      xref="xref\n0 7\n0000000000 65535 f \n"<>BS.concat[B8.pack(printf "%010d 00000 n \n" offset)|offset<-take 6 offsets]
  in body<>xref<>"trailer\n<< /Size 7 /Root 1 0 R >>\nstartxref\n"<>B8.pack(show(BS.length body))<>"\n%%EOF\n"
