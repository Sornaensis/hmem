module HMem.DB.DocumentGrantSpec (spec) where

import Control.Exception (SomeException,try)
import Control.Monad (void)
import Data.Aeson
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as B8
import Data.ByteString.Lazy qualified as LBS
import Data.Either (isLeft)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time (getCurrentTime,addUTCTime)
import Data.UUID (UUID)
import Data.UUID qualified as UUIDText
import Data.UUID.V4 qualified as UUID
import Hasql.Decoders qualified as D
import Hasql.Encoders qualified as E
import Hasql.Session qualified as S
import Hasql.Statement qualified as St
import Test.Hspec
import HMem.Config qualified as C
import HMem.Auth.CredentialOrigin
import HMem.DB.Auth qualified as Auth
import HMem.DB.Document qualified as Documents
import HMem.DB.DocumentGrant qualified as Grants
import HMem.DB.Pool (runSession)
import HMem.DB.RequestContext
import HMem.DB.Task qualified as Task
import HMem.DB.TestHarness
import HMem.Document.Codec (payloadSha256)
import HMem.Document.Grant
import HMem.Document.Types qualified as Doc
import HMem.Types qualified as Types

spec :: Spec
spec=around withTestEnv $ describe "Document grant storage" $ do
  it "keeps legacy configurations disabled and round-trips explicit delivery policy" $ \_ -> do
    shouldBe (eitherDecode "{}" :: Either String C.HMemConfig) (Right C.defaultConfig)
    let delivery=C.DocumentDeliveryConfig(Just "https://app.example.test")(Just "https://download.example.test")
          (Just "https://preview.example.net")(Just [])"127.0.0.1"(Just 8421)
        configured=C.defaultConfig{C.documentDelivery=delivery}
    shouldBe (eitherDecode(encode configured)) (Right configured)
  it "binds the exact role association through source removal/recreation while evidence independently survives" $ \env -> do
    (w,t,document,source)<-fixture env
    Just original<-Grants.resolveBinding env.pool w t document.id (request source)
    Grants.readBoundPayload env.pool original `shouldReturn` "original\r\nbytes"
    evidenceRequest<-UUID.nextRandom
    _<-Documents.addEvidence env.pool w t document.id evidenceRequest
    evidencePage<-Documents.listDocuments env.pool w t Doc.Evidence Nothing 50
    let evidence=head evidencePage.items
    Just evidenceBinding<-Grants.resolveBinding env.pool w t document.id (GrantRequest Doc.Evidence evidence.id "original" 0 Nothing Nothing)
    Documents.removeSource env.pool w t document.id
    Grants.bindingIsCurrent env.pool original `shouldReturn` False
    Grants.bindingIsCurrent env.pool evidenceBinding `shouldReturn` True
    replacement<-UUID.nextRandom
    execute env ("INSERT INTO task_document_sources(id,workspace_id,task_id,document_id) VALUES('"<>uuid replacement<>"','"<>uuid w<>"','"<>uuid t<>"','"<>uuid document.id<>"')")
    Grants.bindingIsCurrent env.pool original `shouldReturn` False
    Grants.resolveBinding env.pool w t document.id (request replacement) >>= (`shouldSatisfy` (/=Nothing))
    execute env ("UPDATE tasks SET deleted_at=clock_timestamp() WHERE id='"<>uuid t<>"'")
    Grants.bindingIsCurrent env.pool evidenceBinding `shouldReturn` False
    execute env ("UPDATE tasks SET deleted_at=NULL WHERE id='"<>uuid t<>"'")
    Grants.bindingIsCurrent env.pool evidenceBinding `shouldReturn` True
  it "pins current preview generation/policy/hash and refuses a stale generation or HTML-as-raster" $ \env -> do
    (w,t,document,source)<-fixture env
    first<-UUID.nextRandom
    Documents.reservePreview env.pool w document.id first "policy1" 1 65536
    Documents.publishPreview env.pool w document.id first "policy1" [Documents.Derivative "page" 1 "image/png" "inert-raster-one"]
    Just binding<-Grants.resolveBinding env.pool w t document.id (GrantRequest Doc.Source source "page" 1 (Just first) Nothing)
    Grants.resolveBinding env.pool w t document.id (GrantRequest Doc.Source source "page" 4294967297 (Just first) Nothing) `shouldReturn` Nothing
    Grants.readBoundPayload env.pool binding `shouldReturn` "inert-raster-one"
    Just status<-Grants.previewStatus env.pool w t document.id
    status.generationId `shouldBe` Just first
    status.outputs `shouldBe` [PreviewOutput "page" 1 "image/png" 16 (payloadSha256 "inert-raster-one")]
    next<-UUID.nextRandom
    Documents.reservePreview env.pool w document.id next "policy2" 1 65536
    Documents.publishPreview env.pool w document.id next "policy2" [Documents.Derivative "styled_html_preview" 0 "text/html" "<p>safe</p>"]
    Grants.bindingIsCurrent env.pool binding `shouldReturn` False
    Grants.resolveBinding env.pool w t document.id (GrantRequest Doc.Source source "page" 0 (Just next) Nothing) `shouldReturn` Nothing
    Just html<-Grants.resolveBinding env.pool w t document.id (GrantRequest Doc.Source source "styled_html_preview" 0 (Just next) Nothing)
    html.policyVersion `shouldBe` Just "policy2"
    ambiguous<-UUID.nextRandom
    Documents.reservePreview env.pool w document.id ambiguous "policy3" 2 65536
    -- The ordinary publisher already rejects duplicate representation keys.
    -- Model an imported malformed generation without disabling immutable guards:
    -- both descriptors refer to the existing verified immutable HTML payload.
    execute env("INSERT INTO document_generations(id,workspace_id,document_id,original_sha256,policy_version,outputs) SELECT '"<>uuid ambiguous<>"',workspace_id,document_id,original_sha256,'policy3',outputs||outputs FROM document_generations WHERE id='"<>uuid next<>"'")
    execute env("UPDATE document_previews SET state='ready',generation_id='"<>uuid ambiguous<>"' WHERE document_id='"<>uuid document.id<>"'")
    shouldReturn (Grants.resolveBinding env.pool w t document.id (GrantRequest Doc.Source source "styled_html_preview" 0 (Just ambiguous) Nothing)) Nothing
  it "publishes immutable hash-only grants and metadata-only create/revoke events without grant secrets" $ \env -> do
    (w,t,document,source)<-fixture env
    Just binding<-Grants.resolveBinding env.pool w t document.id (request source)
    Grants.resolveBinding env.pool w t document.id (GrantRequest Doc.Source source "original" 4294967296 Nothing Nothing) `shouldReturn` Nothing
    Grants.resolveBinding env.pool w t document.id (GrantRequest Doc.Source source "original" 0 Nothing (Just 4294967296)) `shouldReturn` Nothing
    json env "SELECT to_jsonb(count(*)) FROM document_download_grants" `shouldReturn` Number 0
    identifier<-UUID.nextRandom; epoch<-UUID.nextRandom; now<-getCurrentTime
    let tokenHash=payloadSha256 "secret-token-that-is-never-persisted"
    grant<-Grants.createGrant env.pool identifier tokenHash localIssuer epoch binding (addUTCTime 300 now)
    Grants.lookupGrant env.pool identifier tokenHash `shouldReturn` Just grant
    Grants.lookupGrant env.pool identifier (payloadSha256 "wrong") `shouldReturn` Nothing
    reject(execute env "UPDATE document_download_grants SET binding=jsonb_set(binding,'{page}','99')")
    reject(execute env "DELETE FROM document_download_grants")
    reject(execute env "TRUNCATE document_download_grants")
    value<-json env "SELECT jsonb_build_object('audit',(SELECT jsonb_agg(to_jsonb(a)) FROM audit_log a WHERE entity_type='task_document_grant'),'changes',(SELECT jsonb_agg(to_jsonb(c)) FROM change_stream_outbox c WHERE envelope->'entity'->>'type'='task_document_grant'))"
    let serialized=B8.unpack(LBS.toStrict(encode value))
    serialized `shouldNotContain` T.unpack tokenHash
    serialized `shouldNotContain` "secret-token"
    serialized `shouldNotContain` "?token="
    json env "SELECT to_jsonb(count(*)) FROM change_stream_outbox WHERE envelope->'entity'->>'type'='task_document_grant'" `shouldReturn` Number 0
    Grants.revokeGrant env.pool w t document.id identifier `shouldReturn` True
    Grants.revokeGrant env.pool w t document.id identifier `shouldReturn` False
    Grants.lookupGrant env.pool identifier tokenHash `shouldReturn` Nothing
    reject(execute env "UPDATE document_download_grants SET revoked_at=NULL")
  it "rejects cross-scope/manipulated manifests and lifetimes atomically" $ \env -> do
    (w,t,document,source)<-fixture env
    Just binding<-Grants.resolveBinding env.pool w t document.id (request source)
    identifier<-UUID.nextRandom; epoch<-UUID.nextRandom; now<-getCurrentTime
    reject(Grants.createGrant env.pool identifier (payloadSha256 "token") localIssuer epoch
      binding{rawSha256=T.replicate 64 "0"} (addUTCTime 300 now))
    reject(Grants.createGrant env.pool identifier (payloadSha256 "token") localIssuer epoch binding (addUTCTime 901 now))
    Grants.getGrant env.pool identifier `shouldReturn` Nothing
    json env "SELECT to_jsonb(count(*)) FROM audit_log WHERE entity_type='task_document_grant'" `shouldReturn` Number 0
  it "rechecks bot PAT authority by token ID without writing usage and notices revoke/expiry/disabled users" $ \env -> do
    user<-UUID.nextRandom; token<-UUID.nextRandom
    execute env ("INSERT INTO users(id,display_name) VALUES('"<>uuid user<>"','grant-user')")
    execute env ("INSERT INTO access_tokens(id,grant_user_id,actor_type,actor_label,token_hash) VALUES('"<>uuid token<>"','"<>uuid user<>"','bot','token-actor','unused-hash')")
    Just resolved<-Auth.revalidateCredentialOrigin env.pool (AccessTokenOrigin token)
    resolved.principal.actorId `shouldBe` UUIDText.toText token
    resolved.principal.authority `shouldBe` PrincipalGrantUser user
    json env "SELECT to_jsonb(last_used_at IS NULL) FROM access_tokens" `shouldReturn` Bool True
    execute env "UPDATE access_tokens SET expires_at=created_at"
    Auth.revalidateCredentialOrigin env.pool (AccessTokenOrigin token) `shouldReturn` Nothing
    execute env "UPDATE access_tokens SET expires_at=NULL,revoked_at=clock_timestamp()"
    Auth.revalidateCredentialOrigin env.pool (AccessTokenOrigin token) `shouldReturn` Nothing
    execute env "UPDATE access_tokens SET revoked_at=NULL"
    execute env "UPDATE users SET disabled_at=clock_timestamp()"
    Auth.revalidateCredentialOrigin env.pool (AccessTokenOrigin token) `shouldReturn` Nothing
  it "retains browser session identity and rechecks logout/expiry without updating last_used_at" $ \env -> do
    user<-UUID.nextRandom; now<-getCurrentTime
    execute env ("INSERT INTO users(id,display_name) VALUES('"<>uuid user<>"','browser')")
    identifier<-Auth.createAuthSession env.pool user "browser-session" "csrf" (addUTCTime 300 now)
    Just initial<-Auth.resolveAuthSessionAuthentication env.pool "browser-session"
    initial.credentialOrigin `shouldBe` BrowserSessionOrigin identifier
    Auth.revalidateCredentialOrigin env.pool initial.credentialOrigin `shouldReturn` Just initial
    json env "SELECT to_jsonb(last_used_at IS NULL) FROM auth_sessions" `shouldReturn` Bool True
    Auth.revokeAuthSession env.pool "browser-session" `shouldReturn` True
    Auth.revalidateCredentialOrigin env.pool initial.credentialOrigin `shouldReturn` Nothing
    expired<-Auth.createAuthSession env.pool user "expiring-browser-session" "csrf" (addUTCTime 300 now)
    execute env("UPDATE auth_sessions SET expires_at=created_at WHERE id='"<>uuid expired<>"'")
    shouldReturn (Auth.revalidateCredentialOrigin env.pool (BrowserSessionOrigin expired)) Nothing

request :: UUID -> GrantRequest
request source=GrantRequest Doc.Source source "original" 0 Nothing Nothing
localIssuer :: GrantIssuer
localIssuer=GrantIssuer (Principal ActorUser "local-user" "local" PrincipalSyntheticLocalSuperadmin) LocalOrigin False
fixture :: TestEnv -> IO(UUID,UUID,Doc.Document,UUID)
fixture env=do
  workspace<-createTestWorkspace env "grant-storage"
  task<-Task.createTask env.pool (Types.CreateTask workspace.id Nothing Nothing "grant task" Nothing Nothing Nothing)
  identifier<-UUID.nextRandom
  let bytes="original\r\nbytes"
  document<-Documents.uploadDocument env.pool (Doc.UploadDocument workspace.id task.id identifier Doc.Source
    (Doc.OriginalManifest 15 (payloadSha256 bytes) "original.txt" "Original" "text/plain")) bytes
  page<-Documents.listDocuments env.pool workspace.id task.id Doc.Source Nothing 50
  pure(workspace.id,task.id,document,(head page.items).id)
execute :: TestEnv -> ByteString -> IO ()
execute env sql=runSession env.pool (S.sql sql)
json :: TestEnv -> ByteString -> IO Value
json env sql=runSession env.pool $ S.statement () (St.Statement sql E.noParams (D.singleRow(D.column(D.nonNullable D.jsonb))) False)
uuid :: UUID -> ByteString
uuid=TE.encodeUtf8 . UUIDText.toText
reject :: IO a -> IO ()
reject action=try @SomeException (void action) >>= (`shouldSatisfy` isLeft)
