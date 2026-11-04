module HMem.Server.DocumentGrantSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException,IOException,try,throwIO,bracket)
import Control.Monad (forM_)
import Data.Aeson hiding(decode)
import Data.Foldable (toList)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Builder qualified as Builder
import Data.IORef
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time (UTCTime,getCurrentTime,addUTCTime)
import Data.UUID (UUID)
import Data.UUID.V4 qualified as UUID
import Hasql.Decoders qualified as D
import Hasql.Encoders qualified as E
import Hasql.Session qualified as S
import Hasql.Statement qualified as St
import Network.HTTP.Types
import Network.Socket (SockAddr(..),tupleToHostAddress)
import Network.Socket qualified as Socket
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Types.Header (hHost,hSetCookie)
import System.Timeout (timeout)
import Network.Wai qualified as Wai
import Network.Wai.Internal (ResponseReceived(..))
import Network.Wai.Test (SRequest(..),SResponse(..),runSession,srequest)
import Test.Hspec
import HMem.Config qualified as C
import HMem.DB.Auth qualified as Auth
import HMem.DB.Document qualified as Documents
import HMem.DB.Pool qualified as Pool
import HMem.DB.Task qualified as Task
import HMem.DB.TestHarness
import HMem.Document.Codec (payloadSha256)
import HMem.Document.Grant
import HMem.Document.Types qualified as Doc
import HMem.Server.AccessTracker (newAccessTracker)
import HMem.Server.App (mkAppWithDocumentAuthority)
import HMem.Server.AuthTokens qualified as Tokens
import HMem.Server.Document.Grant qualified as Grants
import HMem.Server.Document.PreviewOrigin (newPreviewApplication,withPreviewListener)
import HMem.Server.Logging (jsonRequestLogger)
import System.Log.FastLogger (fromLogStr)
import HMem.Server.TestHarness
import HMem.Server.WebSocket (newWSState)
import HMem.Types qualified as Types

spec :: Spec
spec=around withTestEnv $ describe "Document capabilities" $ do
  it "gates explicit bot Evidence bytes and grants without blocking the same document's Source" $ \env -> do
    fixture<-seed env "linked artifact" "artifact.txt" "text/plain"
    (_,app)<-application env (mkDeployedSandboxConfig env) deployedDelivery
    (_,_,headers)<-audience env fixture.workspace Auth.WorkspaceRoleRead
    let localDelivery=deployedDelivery
          {C.applicationOrigin=Just "http://127.0.0.1:8420",C.downloadOrigin=Just "http://127.0.0.1:8420"
          ,C.previewOrigin=Just "http://localhost:8421"}
    (_,localApp)<-application env (mkLocalSandboxConfig env) localDelivery
    promotion<-UUID.nextRandom
    _<-Documents.addEvidence env.pool fixture.workspace fixture.task fixture.document.id promotion
    page<-Documents.listDocuments env.pool fixture.workspace fixture.task Doc.Evidence Nothing 50
    evidence<-only page.items
    generation<-raster env fixture "policy1"
    let (declared,bot)=foldr (\header (identity,credentials) -> if fst header=="X-HMem-Bot-Identity"
          then (header:identity,credentials) else (identity,header:credentials)) ([],[]) headers
        path=base fixture
        evidenceScope="?role=evidence&association_id="<>uuid evidence.id
        sourceScope="?role=source&association_id="<>uuid fixture.source
        original=path<>"/original"
        preview=path<>"/previews/"<>uuid generation<>"/page/1"
        grantInput=GrantRequest Doc.Evidence evidence.id "original" 0 Nothing Nothing
        change state=execute env("UPDATE tasks SET status='"<>state<>"' WHERE id='"<>uuid fixture.task<>"'")
    forM_ [original,preview] $ \target -> do
      call app methodGet(target<>sourceScope) bot "" >>=status status200
      call app methodGet target bot "" >>=status status200
      call app methodGet(target<>evidenceScope) bot "" >>=status status403
      call app methodHead(target<>evidenceScope) bot "" >>=status status403
    postJSON app(path<>"/download-grants") headers (toJSON grantInput) >>=status status403
    call localApp methodGet(original<>evidenceScope) [] "" >>=status status200
    call localApp methodGet(original<>evidenceScope) declared "" >>=status status403
    change "done"
    forM_ [original,preview] $ \target -> call app methodGet(target<>evidenceScope) bot "" >>=status status200
    issued<-issue app fixture headers grantInput
    url<-required "url" issued
    call app methodGet(urlPath url) [] "" >>=status status200
    declaredGrant<-issue localApp fixture declared grantInput
    declaredUrl<-required "url" declaredGrant
    declaredId<-required "grant_id" declaredGrant
    storedIssuer<-json env("SELECT issuer FROM document_download_grants WHERE id='"<>uuid declaredId<>"'")
    field ["actor_type"] storedIssuer `shouldBe` Just (String "user")
    field ["bot_restricted"] storedIssuer `shouldBe` Just (Bool True)
    call localApp methodGet(urlPath declaredUrl) [] "" >>=status status200
    change "todo"
    call app methodGet(urlPath url) [] "" >>=status status403
    call localApp methodGet(urlPath declaredUrl) [] "" >>=status status403
    Documents.removeSource env.pool fixture.workspace fixture.task fixture.document.id
    call app methodGet original bot "" >>=status status403
    change "cancelled"
    call app methodGet original bot "" >>=status status200
  it "delegates a read-member PAT with exact role/association, hash-only storage and original fidelity" $ \env -> do
    fixture<-seed env "hello capability" "notes-😀.txt" "text/plain"
    let cfg=mkDeployedSandboxConfig env
    (_,app)<-application env cfg deployedDelivery
    (user,token,headers)<-audience env fixture.workspace Auth.WorkspaceRoleRead
    issued<-issue app fixture headers (originalRequest fixture.source)
    url<-required "url" issued
    shouldSatisfy url (T.isPrefixOf "https://download.example.test/document-grants/")
    shouldBe (field ["association_id"] issued) (Just(toJSON fixture.source))
    shouldBe (field ["role"] issued) (Just(String "source"))
    shouldBe (field ["document_id"] issued) (Just(toJSON fixture.document.id))
    result<-call app methodGet (urlPath url) [] ""
    shouldBe result.simpleStatus status200
    shouldBe result.simpleBody "hello capability"
    shouldBe (lookup hContentType result.simpleHeaders) (Just "application/octet-stream")
    shouldSatisfy (lookup "Content-Disposition" result.simpleHeaders) (maybe False(BS.isInfixOf "filename*=UTF-8''notes-%F0%9F%98%80.txt"))
    shouldBe (lookup hCacheControl result.simpleHeaders) (Just "no-store")
    shouldBe (lookup "Referrer-Policy" result.simpleHeaders) (Just "no-referrer")
    shouldBe (lookup "X-Content-Type-Options" result.simpleHeaders) (Just "nosniff")
    headResult<-call app methodHead(urlPath url) [] ""
    shouldBe headResult.simpleStatus status200
    shouldBe headResult.simpleBody ""
    shouldBe (lookup hContentLength headResult.simpleHeaders) (Just "16")
    let rawToken=tokenPart url
    shouldBe (BS.length rawToken) 43
    call app methodGet(urlPath url<>"&token="<>rawToken) [] "" >>=status status400
    let withoutToken=fst(BS.break(==63)(urlPath url))
    call app methodGet(withoutToken<>"?token="<>BS.replicate 43 65) [] "" >>=status status404
    call app methodGet(BS.concat[fst(BS.breakSubstring "/original/0" withoutToken),"/original/1?token=",rawToken]) [] "" >>=status status404
    unknownGrant<-UUID.nextRandom
    call app methodGet("/document-grants/"<>uuid unknownGrant<>"/original/0?token="<>rawToken) [] "" >>=status status404
    identifier<-required "grant_id" issued
    stored<-json env("SELECT jsonb_build_object('hash',token_sha256,'issuer',issuer) FROM document_download_grants WHERE id='"<>uuid identifier<>"'")
    shouldBe (field ["hash"] stored) (Just(String(payloadSha256 rawToken)))
    shouldBe (field ["issuer","actor_id"] stored) (Just(String(T.pack(show token.tokenId))))
    shouldBe (field ["issuer","authority","id"] stored) (Just(toJSON user))
    shouldBe (field ["issuer","credential_origin","id"] stored) (Just(toJSON token.tokenId))
    audit<-json env("SELECT jsonb_build_object('actor_type',actor_type,'actor_id',actor_id,'bot_identity',bot_identity) FROM audit_log WHERE entity_type='task_document_grant' AND entity_id='"<>uuid identifier<>"'")
    shouldBe (field ["actor_type"] audit)(Just(String "bot"))
    shouldBe (field ["actor_id"] audit)(Just(String(T.pack(show token.tokenId))))
    shouldBe (field ["bot_identity","role"] audit)(Just(String "Capability test"))
    shouldBe (T.isInfixOf(TE.decodeUtf8 rawToken)(TE.decodeUtf8(LBS.toStrict(encode stored)))) False
    poisoned<-postJSON app(base fixture<>"/download-grants")(("Host","attacker.invalid"):headers)(toJSON(originalRequest fixture.source))
    shouldBe poisoned.simpleStatus status201
    poisonedValue<-decode poisoned
    poisonedUrl<-required "url" poisonedValue
    shouldSatisfy poisonedUrl(T.isPrefixOf "https://download.example.test/")
  it "authorizes before body processing and rejects unbounded or ambiguous grant input without publication" $ \env -> do
    fixture<-seed env "bounded" "bounded.txt" "text/plain"
    (_,app)<-application env (mkDeployedSandboxConfig env) deployedDelivery
    (_,_,reader)<-audience env fixture.workspace Auth.WorkspaceRoleRead
    outsider<-createDeployedSandboxUser env False False
    outsiderToken<-issueDeployedSandboxPAT env outsider "outsider"
    let outsiderHeaders=bearer outsiderToken
        path=base fixture<>"/download-grants"
    shouldReturn (unread app(request methodPost path outsiderHeaders)) (status403,0)
    shouldReturn (unread app(request methodPost path [])) (status401,0)
    missing<-UUID.nextRandom
    shouldReturn (unread app(request methodPost
      ("/api/v1/workspaces/"<>uuid fixture.workspace<>"/tasks/"<>uuid fixture.task<>"/documents/"<>uuid missing<>"/download-grants") reader)) (status404,0)
    let valid=toJSON(originalRequest fixture.source)
        badPage=object["role" .= ("source"::Text),"association_id" .= fixture.source,"variant" .= ("original"::Text),"page" .= (4294967296::Integer)]
        badTtl=object["role" .= ("source"::Text),"association_id" .= fixture.source,"variant" .= ("original"::Text),"page" .= (0::Int),"ttl_seconds" .= (4294967296::Integer)]
        unknown=case valid of Object fields->Object(KM.insert "issuer"(String "forged")fields); _->Null
    forM_ [badPage,badTtl,unknown] $ \value->do
      result<-postJSON app path reader value
      shouldBe result.simpleStatus status400
    oversized<-call app methodPost path((hContentType,"application/json"):reader)(LBS.replicate 4097 120)
    shouldBe oversized.simpleStatus status413
    duplicate<-call app methodPost path((hContentType,"application/json"):(hContentType,"application/json"):reader)(encode valid)
    shouldBe duplicate.simpleStatus status415
    shouldReturn(json env "SELECT to_jsonb(count(*)) FROM document_download_grants")(Number 0)
  it "keeps Source and Evidence authority independent and prevents association ABA" $ \env -> do
    fixture<-seed env "retained" "retained.txt" "text/plain"
    (_,app)<-application env (mkDeployedSandboxConfig env) deployedDelivery
    (_,_,headers)<-audience env fixture.workspace Auth.WorkspaceRoleRead
    source<-issue app fixture headers(originalRequest fixture.source)
    sourceUrl<-required "url" source
    evidenceRequest<-UUID.nextRandom
    _<-Documents.addEvidence env.pool fixture.workspace fixture.task fixture.document.id evidenceRequest
    page<-Documents.listDocuments env.pool fixture.workspace fixture.task Doc.Evidence Nothing 50
    evidence<-only page.items
    execute env("UPDATE tasks SET status='done' WHERE id='"<>uuid fixture.task<>"'")
    evidenceGrant<-issue app fixture headers(GrantRequest Doc.Evidence evidence.id "original" 0 Nothing Nothing)
    evidenceUrl<-required "url" evidenceGrant
    Documents.removeSource env.pool fixture.workspace fixture.task fixture.document.id
    call app methodGet(urlPath sourceUrl) [] "" >>=status status404
    call app methodGet(urlPath evidenceUrl) [] "" >>=status status200
    replacement<-UUID.nextRandom
    execute env("INSERT INTO task_document_sources(id,workspace_id,task_id,document_id) VALUES('"<>uuid replacement<>"','"<>uuid fixture.workspace<>"','"<>uuid fixture.task<>"','"<>uuid fixture.document.id<>"')")
    call app methodGet(urlPath sourceUrl) [] "" >>=status status404
    replacementGrant<-issue app fixture headers(originalRequest replacement)
    required "association_id" replacementGrant >>= \identifier->shouldBe identifier replacement
    wrongRole<-postJSON app(base fixture<>"/download-grants")headers(toJSON(GrantRequest Doc.Evidence replacement "original" 0 Nothing Nothing))
    shouldBe wrongRole.simpleStatus status404
  it "rechecks PAT revocation, expiry, enabled user and current read membership" $ \env -> do
    fixture<-seed env "access" "access.txt" "text/plain"
    (_,app)<-application env (mkDeployedSandboxConfig env) deployedDelivery
    forM_ ["revoke","expire","disable","role"] $ \mode->do
      (user,token,headers)<-audience env fixture.workspace Auth.WorkspaceRoleRead
      grant<-issue app fixture headers(originalRequest fixture.source)
      url<-required "url" grant
      call app methodGet(urlPath url) [] "" >>=status status200
      case mode of
        "revoke"->Tokens.revokeAccessToken env.pool token.tokenId >>= \changed->shouldBe changed True
        "expire"->execute env("UPDATE access_tokens SET expires_at=created_at WHERE id='"<>uuid token.tokenId<>"'")
        "disable"->execute env("UPDATE users SET disabled_at=clock_timestamp() WHERE id='"<>uuid user<>"'")
        _->Auth.deleteWorkspaceMembership env.pool fixture.workspace user >>pure()
      call app methodGet(urlPath url) [] "" >>=status status403
  it "expires grants and permits explicit issuer/admin revocation without granting another reader authority" $ \env -> do
    fixture<-seed env "expiry" "expiry.txt" "text/plain"
    (_,app)<-application env (mkDeployedSandboxConfig env) deployedDelivery
    (_,_,headers)<-audience env fixture.workspace Auth.WorkspaceRoleRead
    short<-issue app fixture headers(GrantRequest Doc.Source fixture.source "original" 0 Nothing(Just 1))
    shortUrl<-required "url" short
    threadDelay 1200000
    call app methodGet(urlPath shortUrl) [] "" >>=status status404
    grant<-issue app fixture headers(originalRequest fixture.source)
    identifier<-required "grant_id" grant
    url<-required "url" grant
    (_,_,otherReader)<-audience env fixture.workspace Auth.WorkspaceRoleRead
    call app methodDelete(base fixture<>"/download-grants/"<>uuid identifier)otherReader "" >>=status status403
    call app methodDelete(base fixture<>"/download-grants/"<>uuid identifier)headers "" >>=status status204
    call app methodGet(urlPath url) [] "" >>=status status404
    another<-issue app fixture headers(originalRequest fixture.source)
    anotherId<-required "grant_id" another
    (_,_,admin)<-audience env fixture.workspace Auth.WorkspaceRoleAdmin
    call app methodDelete(base fixture<>"/download-grants/"<>uuid anotherId)admin "" >>=status status204
  it "binds browser-session identity, preserves CSRF and observes actual logout without per-part usage writes" $ \env -> do
    fixture<-seed env "browser" "browser.txt" "text/plain"
    (_,app)<-application env (mkDeployedSandboxConfig env) deployedDelivery
    user<-createDeployedSandboxUser env False False
    _<-Auth.upsertWorkspaceMembership env.pool fixture.workspace(Auth.UpsertWorkspaceMembership user Auth.WorkspaceRoleRead)Nothing
    identifier<-createDeployedSandboxAuthSession env user "browser-grant-session" "csrf-value"
    let cookies=[(hCookie,"hmem_session=browser-grant-session; hmem_csrf=csrf-value")]
        headers=("X-CSRF-Token","csrf-value"):cookies
    shouldReturn(unread app(request methodPost(base fixture<>"/download-grants")cookies))(status403,0)
    grant<-issue app fixture headers(originalRequest fixture.source)
    url<-required "url" grant
    before<-json env("SELECT to_jsonb(last_used_at) FROM auth_sessions WHERE id='"<>uuid identifier<>"'")
    call app methodGet(urlPath url) [] "" >>=status status200
    shouldReturn(json env("SELECT to_jsonb(last_used_at) FROM auth_sessions WHERE id='"<>uuid identifier<>"'"))before
    call app methodPost "/api/v1/auth/logout" headers "" >>=status status302
    call app methodGet(urlPath url) [] "" >>=status status403
  it "preserves ordinary JWT reads while refusing unsupported JWT delegation before consuming a body" $ \env -> do
    fixture<-seed env "jwt" "jwt.txt" "text/plain"
    (_,app)<-application env (mkDeployedSandboxConfig env) deployedDelivery
    user<-createDeployedSandboxUserWithSubject env "jwt-grant-subject" False False
    _<-Auth.upsertWorkspaceMembership env.pool fixture.workspace(Auth.UpsertWorkspaceMembership user Auth.WorkspaceRoleRead)Nothing
    token<-signDeployedSandboxJwt "jwt-grant-subject"
    let headers=[(hAuthorization,"Bearer "<>TE.encodeUtf8 token)]
    call app methodGet(base fixture)headers "" >>=status status200
    shouldReturn(unread app(request methodPost(base fixture<>"/download-grants")headers))(status403,0)
    result<-postJSON app(base fixture<>"/download-grants")headers(toJSON(originalRequest fixture.source))
    shouldBe result.simpleStatus status403
    value<-decode result
    shouldBe (field ["error"] value)(Just(String "credential_authority_unavailable"))

  it "caps delegation at the originating credential expiry" $ \env -> do
    fixture<-seed env "cap" "cap.txt" "text/plain"
    (_,app)<-application env (mkDeployedSandboxConfig env) deployedDelivery
    user<-createDeployedSandboxUser env False False
    _<-Auth.upsertWorkspaceMembership env.pool fixture.workspace(Auth.UpsertWorkspaceMembership user Auth.WorkspaceRoleRead)Nothing
    now<-getCurrentTime
    Right token<-Tokens.issueAccessTokenWithSecret env.pool(Just deployedSandboxTokenHashSecret)
      (Tokens.IssueAccessTokenInput user Tokens.AccessTokenActorBot "short credential"(Just(addUTCTime 50 now)))
    value<-issue app fixture(bearer token)(GrantRequest Doc.Source fixture.source "original" 0 Nothing(Just 900))
    expiry<-required "expires_at" value::IO UTCTime
    actual<-json env("SELECT to_jsonb(expires_at) FROM access_tokens WHERE id='"<>uuid token.tokenId<>"'")
    actualExpiry<-case fromJSON actual of Success instant->pure instant; Error message->fail message
    shouldBe expiry (actualExpiry::UTCTime)
  it "serves current preview metadata and exact inert raster generations without newer-byte fallback" $ \env -> do
    fixture<-seed env "preview" "preview.txt" "text/plain"
    (_,app)<-application env (mkDeployedSandboxConfig env) deployedDelivery
    (_,_,headers)<-audience env fixture.workspace Auth.WorkspaceRoleRead
    generation<-raster env fixture "policy1"
    metadata<-call app methodGet(base fixture<>"/previews/current")headers "" >>=decode
    shouldBe (field ["generation_id"] metadata)(Just(toJSON generation))
    shouldBe (field ["policy_version"] metadata)(Just(String "policy1"))
    case field ["outputs"] metadata of
      Just(Array values)->shouldBe (length values) 2
      _->expectationFailure "Missing bounded output descriptors"
    let path=base fixture<>"/previews/"<>uuid generation<>"/page/1"
    result<-call app methodGet path headers ""
    shouldBe result.simpleStatus status200
    shouldBe result.simpleBody(LBS.fromStrict png)
    shouldBe (lookup hContentType result.simpleHeaders)(Just "image/png")
    headResult<-call app methodHead path headers ""
    shouldBe headResult.simpleStatus status200
    shouldBe headResult.simpleBody ""
    grant<-issue app fixture headers(GrantRequest Doc.Source fixture.source "page" 1(Just generation)Nothing)
    url<-required "url" grant
    call app methodGet(urlPath url) [] "" >>=status status200
    call app methodGet(base fixture<>"/previews/"<>uuid generation<>"/styled_html_preview/0")headers "" >>=status status400
    next<-raster env fixture "policy2"
    shouldSatisfy next(/=generation)
    call app methodGet path headers "" >>=status status409
    call app methodGet(urlPath url) [] "" >>=status status404
    execute env("UPDATE tasks SET deleted_at=clock_timestamp() WHERE id='"<>uuid fixture.task<>"'")
    call app methodGet(base fixture<>"/previews/current")headers "" >>=status status404
    execute env("UPDATE tasks SET deleted_at=NULL WHERE id='"<>uuid fixture.task<>"'")
    call app methodGet(base fixture<>"/previews/current")headers "" >>=status status200
  it "serves styled snapshots only on the isolated grant app with direct-navigation enforcing CSP" $ \env -> do
    fixture<-seed env "<!doctype html><html><script>alert(1)</script></html>" "wireframe.html" "text/html"
    (authority,app)<-application env (mkDeployedSandboxConfig env) deployedDelivery
    (_,_,headers)<-audience env fixture.workspace Auth.WorkspaceRoleRead
    generation<-styled env fixture "sanitizer1" "<h1 style=\"color:red\">Snapshot &amp; preview</h1>"
    grant<-issue app fixture headers(GrantRequest Doc.Source fixture.source "styled_html_preview" 0(Just generation)Nothing)
    url<-required "url" grant
    shouldSatisfy url(T.isPrefixOf "https://preview.example.net/")
    preview<-previewFor authority
    result<-call preview methodGet(urlPath url)[("Sec-Fetch-Dest","document")] ""
    shouldBe result.simpleStatus status200
    shouldBe result.simpleBody "<h1 style=\"color:red\">Snapshot &amp; preview</h1>"
    shouldBe (lookup hContentType result.simpleHeaders)(Just "text/html;charset=utf-8")
    shouldBe (lookup "Content-Security-Policy" result.simpleHeaders)(Just styledCSP)
    shouldBe (lookup hSetCookie result.simpleHeaders) Nothing
    shouldBe (lookup hCacheControl result.simpleHeaders)(Just "no-store")
    shouldBe (lookup "Referrer-Policy" result.simpleHeaders)(Just "no-referrer")
    shouldBe (lookup "X-Content-Type-Options" result.simpleHeaders)(Just "nosniff")
    headResult<-call preview methodHead(urlPath url) [] ""
    shouldBe headResult.simpleBody ""
    shouldBe (lookup "Content-Security-Policy" headResult.simpleHeaders)(Just styledCSP)
    let framePath=urlPath url<>"&view=frame"
    framed<-call preview methodGet framePath [] ""
    shouldBe framed.simpleStatus status200
    shouldBe (lookup "Content-Security-Policy" framed.simpleHeaders)(Just styledCSP)
    shouldBe (lookup hContentType framed.simpleHeaders)(Just "text/html;charset=utf-8")
    shouldBe (lookup "X-HMem-Document-SHA256" framed.simpleHeaders) Nothing
    shouldBe (lookup "X-HMem-Document-Bytes" framed.simpleHeaders) Nothing
    shouldSatisfy (LBS.toStrict framed.simpleBody)
      (BS.isSuffixOf "srcdoc=\"&lt;h1 style=&quot;color:red&quot;&gt;Snapshot &amp;amp; preview&lt;/h1&gt;\"></iframe>")
    call preview methodGet(framePath<>"&view=frame") [] "" >>=status status400
    call preview methodGet(framePath<>"&unexpected=1") [] "" >>=status status400
    call app methodGet framePath [] "" >>=status status400
    finalCheckGrant<-issue app fixture headers(GrantRequest Doc.Source fixture.source "styled_html_preview" 0(Just generation)Nothing)
    finalCheckUrl<-required "url" finalCheckGrant
    finalCheckId<-required "grant_id" finalCheckGrant
    sent<-interruptedAfter 2 preview (request methodGet(urlPath finalCheckUrl<>"&view=frame")[]) $
      call app methodDelete(base fixture<>"/download-grants/"<>uuid finalCheckId)headers "" >>=status status204
    shouldBe sent (fromIntegral(LBS.length framed.simpleBody)-BS.length "\"></iframe>")
    call preview methodGet(urlPath url)[(hCookie,"hmem_session=leak")] "" >>=status status400
    call preview methodGet(urlPath url)[(hAuthorization,"Bearer must-not-be-resolved")] "" >>=status status400
    shouldReturn(unread preview(request methodPost(urlPath url)[]))(status405,0)
    call preview methodGet "/api/v1/session" [] "" >>=status status404
    call preview methodGet "/index.html" [] "" >>=status status404
    call app methodGet(urlPath url) [] "" >>=status status404
    let wrongVariant=BS.concat[fst(BS.breakSubstring "/styled_html_preview/0"(urlPath url)),"/original/0?token=",tokenPart url]
    call preview methodGet wrongVariant [] "" >>=status status404
    health<-call app methodGet "/api/v1/health" [] ""
    shouldBe (lookup "Content-Security-Policy" health.simpleHeaders)
      (Just "frame-src https://preview.example.net; child-src https://preview.example.net")
    evidenceRequest<-UUID.nextRandom
    _<-Documents.addEvidence env.pool fixture.workspace fixture.task fixture.document.id evidenceRequest
    page<-Documents.listDocuments env.pool fixture.workspace fixture.task Doc.Evidence Nothing 50
    evidence<-only page.items
    execute env("UPDATE tasks SET status='done' WHERE id='"<>uuid fixture.task<>"'")
    evidenceGrant<-issue app fixture headers(GrantRequest Doc.Evidence evidence.id "styled_html_preview" 0(Just generation)Nothing)
    evidenceUrl<-required "url" evidenceGrant
    Documents.removeSource env.pool fixture.workspace fixture.task fixture.document.id
    call preview methodGet(urlPath url) [] "" >>=status status404
    call preview methodGet framePath [] "" >>=status status404
    call preview methodGet(urlPath evidenceUrl) [] "" >>=status status200
  it "stops original streams after one bounded chunk on explicit grant or issuer authority withdrawal" $ \env -> do
    forM_ ["grant","credential","membership","link","expiry"] $ \mode->do
      fixture<-seed env(BS.replicate(2*1048576+1)120)"large.txt" "text/plain"
      (_,app)<-application env (mkDeployedSandboxConfig env) deployedDelivery
      (user,token,headers)<-audience env fixture.workspace Auth.WorkspaceRoleRead
      let input=if mode=="expiry" then GrantRequest Doc.Source fixture.source "original" 0 Nothing(Just 3) else originalRequest fixture.source
      grant<-issue app fixture headers input
      url<-required "url" grant
      identifier<-required "grant_id" grant
      let withdraw=case mode of
            "grant"->call app methodDelete(base fixture<>"/download-grants/"<>uuid identifier)headers "" >>=status status204
            "credential"->Tokens.revokeAccessToken env.pool token.tokenId >>pure()
            "membership"->Auth.deleteWorkspaceMembership env.pool fixture.workspace user >>pure()
            "expiry"->threadDelay 3200000
            _->Documents.removeSource env.pool fixture.workspace fixture.task fixture.document.id
      sent<-interruptedAfter 1 app(request methodGet(urlPath url)[])withdraw
      shouldBe sent 1048576
  it "invalidates an in-flight styled generation instead of sending a replacement generation" $ \env -> do
    fixture<-seed env "<!doctype html><html><p>wireframe</p></html>" "frame.html" "text/html"
    (authority,app)<-application env (mkDeployedSandboxConfig env) deployedDelivery
    (_,_,headers)<-audience env fixture.workspace Auth.WorkspaceRoleRead
    generation<-styled env fixture "policy1"(BS.concat["<p>",BS.replicate(2*1048576)32,"</p>"])
    grant<-issue app fixture headers(GrantRequest Doc.Source fixture.source "styled_html_preview" 0(Just generation)Nothing)
    url<-required "url" grant
    preview<-previewFor authority
    sent<-interruptedAfter 1 preview(request methodGet(urlPath url)[]) $ do
      _<-styled env fixture "policy2" "<p>replacement</p>"
      pure()
    shouldBe sent 1048576
  it "fails closed for unsafe origin/cookie configurations including numeric-host aliases" $ \env -> do
    let cfg=mkDeployedSandboxConfig env
        bad=
          [deployedDelivery{C.previewOrigin=Just "https://app.example.test:9443"}
          ,deployedDelivery{C.previewOrigin=Just "https://frame.app.example.test",C.proxyAuthCookieDomains=Just [".example.test"]}
          ,deployedDelivery{C.proxyAuthCookieDomains=Nothing}
          ,deployedDelivery{C.proxyAuthCookieDomains=Just ["example.test:443"]}
          ,deployedDelivery{C.previewListenHost="0.0.0.0"}
          ,deployedDelivery{C.previewOrigin=Just "http://preview.example.net"}
          ,deployedDelivery{C.previewOrigin=Just "https://user@preview.example.net"}
          ,deployedDelivery{C.previewOrigin=Just "https://preview.example.net/path"}
          ,deployedDelivery{C.previewOrigin=Just "https://preview.example.net/?token=forbidden"}
          ,deployedDelivery{C.previewOrigin=Just "https://preview.example.net:18446744073709552059"}
          ,deployedDelivery{C.previewOrigin=Just "https://127.1"}
          ,deployedDelivery{C.applicationOrigin=Just "https://[::1]:8443",C.downloadOrigin=Just "https://[::1]:8443",C.previewOrigin=Just "https://[0:0:0:0:0:0:0:1]:9443"}]
    forM_ bad $ \delivery->do
      authority<-Grants.newDocumentAuthority env.pool cfg.auth cfg.server delivery
      shouldReturn(Grants.previewListener authority) Nothing
    fixture<-seed env "original still works" "safe.txt" "text/plain"
    (_,app)<-application env cfg(deployedDelivery{C.downloadOrigin=Just "http://public.example.test"})
    (_,_,headers)<-audience env fixture.workspace Auth.WorkspaceRoleRead
    result<-postJSON app(base fixture<>"/download-grants")headers(toJSON(originalRequest fixture.source))
    shouldBe result.simpleStatus status503
    call app methodGet(base fixture<>"/original")headers "" >>=status status200
  it "binds local grants to loopback/current mode and invalidates all prior grants on runtime epoch replacement" $ \env -> do
    fixture<-seed env "local" "local.txt" "text/plain"
    let cfg=mkLocalSandboxConfig env
        delivery=C.DocumentDeliveryConfig(Just "http://127.0.0.1:8420")(Just "http://127.0.0.1:8420")
          (Just "http://localhost:8421")(Just [])"127.0.0.1"(Just 8421)
    (authority,app)<-application env cfg delivery
    grant<-issue app fixture [](originalRequest fixture.source)
    url<-required "url" grant
    call app methodGet(urlPath url) [] "" >>=status status200
    let remote=(request methodGet(urlPath url)[]){Wai.remoteHost=SockAddrInet 0(tupleToHostAddress(203,0,113,9))}
    runSession(srequest(SRequest remote ""))app >>=status status403
    let remoteIssue=(request methodPost(base fixture<>"/download-grants")[]){Wai.remoteHost=SockAddrInet 0(tupleToHostAddress(203,0,113,9))}
    shouldReturn(unread app remoteIssue)(status403,0)
    (_,restarted)<-application env cfg delivery
    call restarted methodGet(urlPath url) [] "" >>=status status403
    Grants.replaceDocumentAuthorityPolicy authority (cfg.auth{C.mode=C.AuthModeDeployed})cfg.server delivery
    call app methodGet(urlPath url) [] "" >>=status status403

  it "rejects missing, app/download, unknown, duplicate and wrong-port preview Host before grant lookup" $ \env -> do
    fixture<-seed env "<!doctype html><html><p>host</p></html>" "host.html" "text/html"
    (authority,app)<-application env (mkDeployedSandboxConfig env) deployedDelivery
    (_,_,headers)<-audience env fixture.workspace Auth.WorkspaceRoleRead
    generation<-styled env fixture "policy1" "<p>Host isolated</p>"
    grant<-issue app fixture headers(GrantRequest Doc.Source fixture.source "styled_html_preview" 0(Just generation)Nothing)
    url<-required "url" grant
    preview<-newPreviewApplication authority noRate
    let authorities=[[],[(hHost,"app.example.test")],[(hHost,"download.example.test")],[(hHost,"unknown.example.test")]
          ,[(hHost,"preview.example.net:8443")],[(hHost,"preview.example.net/")],[(hHost,"preview.example.net/path")],[(hHost,"preview.example.net"),(hHost,"preview.example.net")]
          ,[(hHost,"app.example.test"),("X-Forwarded-Host","preview.example.net")]]
    forM_ authorities $ \incoming->do
      result<-call preview methodGet(urlPath url)incoming ""
      shouldBe (statusCode result.simpleStatus) 421
      shouldBe (lookup "Content-Security-Policy" result.simpleHeaders)(Just styledCSP)
    shouldReturn(unread preview(request methodPost "/document-grants/not-a-uuid/x/0?token=bad"[(hHost,"app.example.test")]))(mkStatus 421 "Misdirected Request",0)
    call preview methodGet(urlPath url)[(hHost,"PREVIEW.EXAMPLE.NET:443"),("X-Forwarded-Host","attacker.example")] "" >>=status status200
  it "applies configured burst admission to public original/raster and dedicated styled transfers" $ \env -> do
    fixture<-seed env "limits" "limits.txt" "text/plain"
    let cfg=mkDeployedSandboxConfig env
    (authority,issuerApp)<-application env cfg deployedDelivery
    (_,_,headers)<-audience env fixture.workspace Auth.WorkspaceRoleRead
    generation<-raster env fixture "policy1"
    forM_ [originalRequest fixture.source,GrantRequest Doc.Source fixture.source "page" 1(Just generation)Nothing] $ \input->do
      grant<-issue issuerApp fixture headers input
      url<-required "url" grant
      limited<-applicationWith env(cfg{C.rateLimit=burstOne})authority id
      call limited methodGet(urlPath url) [] "" >>=status status200
      rejected<-call limited methodGet(urlPath url) [] ""
      shouldBe rejected.simpleStatus status429
      shouldBe (lookup hCacheControl rejected.simpleHeaders)(Just "no-store")
    html<-seed env "<!doctype html><html><p>limited</p></html>" "limited.html" "text/html"
    styledGeneration<-styled env html "policy1" "<p>Limited</p>"
    (_,_,htmlHeaders)<-audience env html.workspace Auth.WorkspaceRoleRead
    htmlGrant<-issue issuerApp html htmlHeaders(GrantRequest Doc.Source html.source "styled_html_preview" 0(Just styledGeneration)Nothing)
    url<-required "url" htmlGrant
    preview<-newPreviewApplication authority burstOne
    call preview methodGet(urlPath url)[(hHost,"preview.example.net")] "" >>=status status200
    rejected<-call preview methodGet(urlPath url)[(hHost,"preview.example.net")] ""
    shouldBe rejected.simpleStatus status429
    shouldBe (lookup "Content-Security-Policy" rejected.simpleHeaders)(Just styledCSP)
    shouldBe (lookup "X-Content-Type-Options" rejected.simpleHeaders)(Just "nosniff")

  it "keeps tokens and service credentials out of the actual JSON request logger and metadata projections" $ \env -> do
    fixture<-seed env "logged" "logged.txt" "text/plain"
    let cfg=mkDeployedSandboxConfig env
    authority<-Grants.newDocumentAuthority env.pool cfg.auth cfg.server deployedDelivery
    entries<-newIORef []
    logger<-jsonRequestLogger(\entry->modifyIORef' entries(<>[fromLogStr entry]))
    app<-applicationWith env cfg authority logger
    (_,token,headers)<-audience env fixture.workspace Auth.WorkspaceRoleRead
    grant<-issue app fixture headers(originalRequest fixture.source)
    url<-required "url" grant
    call app methodGet(urlPath url)[(hAuthorization,"Bearer "<>TE.encodeUtf8 token.rawToken)] "" >>=status status200
    metadata<-call app methodGet(base fixture<>"/previews/current")headers "" >>=decode
    output<-BS.concat <$>readIORef entries
    shouldSatisfy output(BS.isInfixOf "/document-grants/")
    shouldSatisfy output(BS.isInfixOf "download-grants")
    shouldBe (BS.isInfixOf(tokenPart url)output) False
    shouldBe (BS.isInfixOf(TE.encodeUtf8 url)output) False
    shouldBe (BS.isInfixOf(TE.encodeUtf8 token.rawToken)output) False
    public<-json env "SELECT jsonb_build_object('audit',(SELECT jsonb_agg(new_values) FROM audit_log WHERE entity_type='task_document_grant'),'outbox',(SELECT jsonb_agg(envelope) FROM change_stream_outbox))"
    let projected=LBS.toStrict(encode(object["previews" .= metadata,"events" .= public]))
    shouldBe (BS.isInfixOf(tokenPart url)projected) False
    shouldBe (BS.isInfixOf(TE.encodeUtf8(payloadSha256(tokenPart url)))projected) False
    shouldBe (BS.isInfixOf(TE.encodeUtf8 url)projected) False
    shouldBe (BS.isInfixOf(TE.encodeUtf8 token.rawToken)projected) False
  it "runs the owned dedicated listener with Host/CSP/credential/rate controls and joins it before return" $ \env -> do
    fixture<-seed env "<!doctype html><html><p>listener</p></html>" "listener.html" "text/html"
    port<-freePort
    let cfg=mkLocalSandboxConfig env
        delivery=C.DocumentDeliveryConfig(Just "http://localhost:8420")(Just "http://localhost:8420")
          (Just("http://127.0.0.1:"<>T.pack(show port)))(Just [])"127.0.0.1"(Just port)
    (authority,app)<-application env cfg delivery
    generation<-styled env fixture "policy1" "<p>Native listener</p>"
    grant<-issue app fixture [](GrantRequest Doc.Source fixture.source "styled_html_preview" 0(Just generation)Nothing)
    url<-required "url" grant
    let fetch manager headers=do
          original<-HTTP.parseRequest(T.unpack url)
          HTTP.httpLbs original{HTTP.requestHeaders=headers,HTTP.checkResponse = \_ _ -> pure (),HTTP.responseTimeout=HTTP.responseTimeoutMicro 2000000}manager
    -- httpLbs drains every response; explicit manager closure closes the idle
    -- client connections before the listener action returns and is joined.
    withPreviewListener authority noRate id $ bracket(HTTP.newManager HTTP.defaultManagerSettings)HTTP.closeManager $ \manager->do
      assertPortBound port
      response<-fetch manager []
      shouldBe (HTTP.responseStatus response) status200
      shouldBe (HTTP.responseBody response) "<p>Native listener</p>"
      shouldSatisfy (lookup "Content-Security-Policy"(HTTP.responseHeaders response))(maybe False(BS.isInfixOf "sandbox; frame-ancestors http://localhost:8420"))
      wrong<-fetch manager [(hHost,"localhost:8420")]
      shouldBe (statusCode(HTTP.responseStatus wrong)) 421
      cookie<-fetch manager [(hCookie,"app-cookie=forbidden")]
      shouldBe (HTTP.responseStatus cookie) status400
    assertPortClosed port
    withPreviewListener authority burstOne id $ bracket(HTTP.newManager HTTP.defaultManagerSettings)HTTP.closeManager $ \manager->do
      _<-fetch manager []
      rejected<-fetch manager []
      shouldBe (HTTP.responseStatus rejected) status429
      shouldBe (lookup hCacheControl(HTTP.responseHeaders rejected))(Just "no-store")
      shouldSatisfy (lookup "Content-Security-Policy"(HTTP.responseHeaders rejected))(maybe False(BS.isInfixOf "sandbox;"))
    assertPortClosed port
    bracket(Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol)Socket.close $ \occupied->do
      Socket.bind occupied(SockAddrInet 0(tupleToHostAddress(127,0,0,1)))
      Socket.listen occupied 1
      SockAddrInet occupiedPort _<-Socket.getSocketName occupied
      let unavailable=delivery{C.previewOrigin=Just("http://127.0.0.1:"<>T.pack(show occupiedPort)),C.previewListenPort=Just(fromIntegral occupiedPort)}
      blocked<-Grants.newDocumentAuthority env.pool cfg.auth cfg.server unavailable
      ran<-newIORef False
      outcome<-try @SomeException(withPreviewListener blocked noRate id(writeIORef ran True))
      case outcome of Left _->pure(); Right _->expectationFailure "Colliding listener startup unexpectedly succeeded"
      shouldReturn(readIORef ran) False

  it "serializes the actual preview/grant OpenAPI paths and resolving schema references" $ \env -> do
    (_,app)<-application env(mkLocalSandboxConfig env)C.defaultDocumentDeliveryConfig
    response<-call app methodGet "/api/v1/openapi.json" [] ""
    shouldBe response.simpleStatus status200
    value<-decode response
    let root="/api/v1/workspaces/{workspaceId}/tasks/{taskId}/documents/{documentId}"
        current=root<>"/previews/current"
        grants=root<>"/download-grants"
        paths=[current,root<>"/previews/{generationId}/{variant}/{page}",grants,grants<>"/{grantId}","/document-grants/{grantId}/{variant}/{page}"]
    shouldBe (field ["paths",current,"get","responses","200","content","application/json","schema","$ref"] value)
      (Just(String "#/components/schemas/TaskDocumentPreviewMetadata"))
    shouldBe (field ["paths",grants,"post","requestBody","content","application/json","schema","$ref"] value)
      (Just(String "#/components/schemas/DocumentDownloadGrantRequest"))
    shouldBe (field ["paths",grants,"post","responses","201","content","application/json","schema","$ref"] value)
      (Just(String "#/components/schemas/DocumentDownloadGrant"))
    forM_ paths $ \path->do
      shouldSatisfy (field ["paths",path] value)(/=Nothing)
      forM_ (maybe [] schemaReferences(field ["paths",path]value)) $ \reference->case T.stripPrefix "#/components/schemas/" reference of
        Nothing->expectationFailure "Unexpected document capability reference namespace"
        Just component->shouldSatisfy(field ["components","schemas",component]value)(/=Nothing)

data Fixture=Fixture {workspace :: UUID,task :: UUID,document :: Doc.Document,source :: UUID}
seed :: TestEnv -> ByteString -> Text -> Text -> IO Fixture
seed env bytes filename media=do
  workspace<-(.id) <$>createTestWorkspace env "document-grants"
  task<-Task.createTask env.pool(Types.CreateTask workspace Nothing Nothing "grant task" Nothing Nothing Nothing)
  identifier<-UUID.nextRandom
  document<-Documents.uploadDocument env.pool(Doc.UploadDocument workspace task.id identifier Doc.Source
    (Doc.OriginalManifest(fromIntegral(BS.length bytes))(payloadSha256 bytes)filename filename media))bytes
  page<-Documents.listDocuments env.pool workspace task.id Doc.Source Nothing 50
  source<-only page.items
  pure(Fixture workspace task.id document source.id)
only :: [a] -> IO a
only [value]=pure value
only _=fail "Expected one fixture item"
originalRequest :: UUID -> GrantRequest
originalRequest association=GrantRequest Doc.Source association "original" 0 Nothing Nothing
deployedDelivery :: C.DocumentDeliveryConfig
deployedDelivery=C.DocumentDeliveryConfig(Just "https://app.example.test")(Just "https://download.example.test")
  (Just "https://preview.example.net")(Just [])"127.0.0.1"(Just 8421)
application :: TestEnv -> C.HMemConfig -> C.DocumentDeliveryConfig -> IO(Grants.DocumentAuthority,Wai.Application)
application env cfg delivery=do
  authority<-Grants.newDocumentAuthority env.pool cfg.auth cfg.server delivery
  app<-applicationWith env cfg authority id
  pure(authority,app)
applicationWith :: TestEnv -> C.HMemConfig -> Grants.DocumentAuthority -> Wai.Middleware -> IO Wai.Application
applicationWith env cfg authority logger=do
  tracker<-newAccessTracker env.pool 3600
  ws<-newWSState
  mkAppWithDocumentAuthority authority Nothing Nothing cfg.changeStream logger cfg.auth cfg.cors cfg.rateLimit env.pool tracker ws Nothing False
audience :: TestEnv -> UUID -> Auth.WorkspaceRole -> IO(UUID,Tokens.IssuedAccessToken,[Header])
audience env workspace role=do
  user<-createDeployedSandboxUser env False False
  _<-Auth.upsertWorkspaceMembership env.pool workspace(Auth.UpsertWorkspaceMembership user role)Nothing
  token<-issueDeployedSandboxPAT env user "grant actor"
  pure(user,token,bearer token)
bearer :: Tokens.IssuedAccessToken -> [Header]
bearer token=[(hAuthorization,"Bearer "<>TE.encodeUtf8 token.rawToken),("X-HMem-Bot-Identity","{\"role\":\"Capability test\",\"model-id\":\"test:fixture:default\"}")]
base :: Fixture -> ByteString
base fixture="/api/v1/workspaces/"<>uuid fixture.workspace<>"/tasks/"<>uuid fixture.task<>"/documents/"<>uuid fixture.document.id
uuid :: UUID -> ByteString
uuid=TE.encodeUtf8 . T.pack . show
request :: Method -> ByteString -> [Header] -> Wai.Request
request method path headers=Wai.defaultRequest{Wai.requestMethod=method,Wai.rawPathInfo=raw,Wai.pathInfo=filter(not . T.null)(T.splitOn "/"(TE.decodeUtf8 raw)),
  Wai.rawQueryString=query,Wai.queryString=parseQuery query,Wai.requestHeaders=headers,Wai.remoteHost=SockAddrInet 0(tupleToHostAddress(127,0,0,1))}
  where(raw,query)=BS.break(==63)path
call :: Wai.Application -> Method -> ByteString -> [Header] -> LBS.ByteString -> IO SResponse
call app method path headers body=runSession(srequest(SRequest((request method path headers){Wai.requestBodyLength=Wai.KnownLength(fromIntegral(LBS.length body))})body))app
postJSON :: Wai.Application -> ByteString -> [Header] -> Value -> IO SResponse
postJSON app path headers value=call app methodPost path((hContentType,"application/json"):headers)(encode value)
issue :: Wai.Application -> Fixture -> [Header] -> GrantRequest -> IO Value
issue app fixture headers input=do
  response<-postJSON app(base fixture<>"/download-grants")headers(toJSON input)
  status status201 response
  decode response
status :: Status -> SResponse -> IO()
status expected response=shouldBe response.simpleStatus expected
decode :: SResponse -> IO Value
decode response=either fail pure(eitherDecode response.simpleBody)
required :: FromJSON a => Text -> Value -> IO a
required key value=case field [key] value of
  Nothing->fail("Missing field "<>T.unpack key)
  Just found->case fromJSON found of Success parsed->pure parsed; Error message->fail message
field :: [Text] -> Value -> Maybe Value
field [] value=Just value
field(key:rest)(Object fields)=KM.lookup(Key.fromText key)fields >>=field rest
field _ _=Nothing
urlPath :: Text -> ByteString
urlPath url=TE.encodeUtf8(T.dropWhile(/='/')(T.drop 3(snd(T.breakOn "://" url))))
tokenPart :: Text -> ByteString
tokenPart url=BS.drop 7(snd(BS.break(==63)(urlPath url)))
execute :: TestEnv -> ByteString -> IO()
execute env sql=Pool.runSession env.pool(S.sql sql)
json :: TestEnv -> ByteString -> IO Value
json env sql=Pool.runSession env.pool(S.statement ()(St.Statement sql E.noParams(D.singleRow(D.column(D.nonNullable D.jsonb)))False))
unread :: Wai.Application -> Wai.Request -> IO(Status,Int)
unread app original=do
  count<-newIORef(0::Int); responseStatus<-newIORef status500
  let next=modifyIORef' count(+1)>>throwIO(userError "Unexpected body read")
  _<-app original{Wai.requestBody=next}(\response->writeIORef responseStatus(Wai.responseStatus response)>>pure ResponseReceived)
  (,) <$>readIORef responseStatus <*>readIORef count

raster :: TestEnv -> Fixture -> Text -> IO UUID
raster env fixture policy=do
  generation<-UUID.nextRandom
  Documents.reservePreview env.pool fixture.workspace fixture.document.id generation policy 2 65536
  Documents.publishPreview env.pool fixture.workspace fixture.document.id generation policy
    [Documents.Derivative "page" 1 "image/png" png,Documents.Derivative "thumbnail" 1 "image/png" png]
  pure generation
styled :: TestEnv -> Fixture -> Text -> ByteString -> IO UUID
styled env fixture policy bytes=do
  generation<-UUID.nextRandom
  Documents.reservePreview env.pool fixture.workspace fixture.document.id generation policy 1 (Doc.uploadReservationBytes(fromIntegral(BS.length bytes)))
  Documents.publishPreview env.pool fixture.workspace fixture.document.id generation policy [Documents.Derivative "styled_html_preview" 0 "text/html" bytes]
  pure generation
png :: ByteString
png=BS.pack [137,80,78,71,13,10,26,10,0,0,0,13,73,72,68,82,0,0,0,1,0,0,0,1,8,4,0,0,0,181,28,12,2,0,0,0,11,73,68,65,84,120,218,99,100,248,15,0,1,5,1,1,39,24,227,102,0,0,0,0,73,69,78,68,174,66,96,130]
styledCSP :: ByteString
styledCSP="default-src 'none'; script-src 'none'; style-src 'unsafe-inline'; img-src data:; font-src 'none'; connect-src 'none'; frame-src 'none'; child-src 'none'; worker-src 'none'; object-src 'none'; media-src 'none'; base-uri 'none'; form-action 'none'; sandbox; frame-ancestors https://app.example.test"
interruptedAfter :: Int -> Wai.Application -> Wai.Request -> IO() -> IO Int
interruptedAfter sends app original withdraw=do
  sent<-newIORef(0::Int)
  remaining<-newIORef sends
  withdrawalCompleted<-newIORef False
  result<-try @SomeException $ app original $ \response->do
    shouldBe (Wai.responseStatus response) status200
    let(_,_,withBody)=Wai.responseToStream response
    withBody $ \stream->stream(\builder->do
      modifyIORef' sent(+fromIntegral(LBS.length(Builder.toLazyByteString builder)))
      modifyIORef' remaining (subtract 1)
      untilWithdrawal<-readIORef remaining
      if untilWithdrawal==0 then withdraw>>writeIORef withdrawalCompleted True else pure())(pure())
    pure ResponseReceived
  case result of Left _->pure(); Right _->expectationFailure "Revoked stream completed"
  shouldReturn(readIORef withdrawalCompleted) True
  readIORef sent

noRate :: C.RateLimitConfig
noRate=C.defaultConfig.rateLimit{C.rlEnabled=False}
burstOne :: C.RateLimitConfig
burstOne=C.defaultConfig.rateLimit{C.rlEnabled=True,C.rlBurst=1,C.rlRequestsPerSecond=0.0001}
previewFor :: Grants.DocumentAuthority -> IO Wai.Application
previewFor authority=do
  application<-newPreviewApplication authority noRate
  pure $ \original respond->let headers=Wai.requestHeaders original
    in application original{Wai.requestHeaders=if any((==hHost) . fst)headers then headers else (hHost,"preview.example.net"):headers}respond

freePort :: IO Int
freePort=bracket(Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol)Socket.close $ \socket->do
  Socket.bind socket(SockAddrInet 0(tupleToHostAddress(127,0,0,1)))
  SockAddrInet port _<-Socket.getSocketName socket
  pure(fromIntegral port)
assertPortClosed :: Int -> IO()
assertPortClosed port=do
  outcome<-timeout 2000000(try @IOException(bindProbe port))
  case outcome of
    Just(Right())->pure()
    Just(Left failure)->expectationFailure("Preview endpoint was not released after owned listener join: "<>show failure)
    Nothing->expectationFailure "Preview endpoint rebind proof exceeded its finite deadline"
assertPortBound :: Int -> IO()
assertPortBound port=do
  outcome<-timeout 2000000(try @IOException(bindProbe port))
  case outcome of
    Just(Left _)->pure()
    Just(Right())->expectationFailure "No-reuse proof socket unexpectedly acquired the live listener endpoint"
    Nothing->expectationFailure "Live preview endpoint negative control exceeded its finite deadline"
bindProbe :: Int -> IO()
bindProbe port=bracket(Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol)Socket.close $ \socket->do
  -- Never enable address/port reuse: success must prove the old listener released
  -- this exact endpoint. The same operation must fail while that listener is live.
  Socket.setSocketOption socket Socket.ReuseAddr 0
  Socket.bind socket(SockAddrInet(fromIntegral port)(tupleToHostAddress(127,0,0,1)))
  Socket.listen socket 1

schemaReferences :: Value -> [Text]
schemaReferences(Object fields)=maybe [] (\value->case value of String reference->[reference]; _->[])(KM.lookup "$ref" fields)
  <>concatMap schemaReferences(KM.elems fields)
schemaReferences(Array values)=concatMap schemaReferences(toList values)
schemaReferences _=[]
