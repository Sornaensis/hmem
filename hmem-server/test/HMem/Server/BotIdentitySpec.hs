module HMem.Server.BotIdentitySpec (spec) where

import Data.Aeson (Result(..), decode, encode, fromJSON, object, (.=))
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.ByteString.Lazy qualified as LBS
import Data.UUID qualified as UUID
import HMem.Config qualified as Config
import HMem.DB.RequestContext
import HMem.Server.App (authMiddleware, botIdentityMiddleware, resolveRequestPrincipal)
import HMem.Server.WebSocket (resolveLocalWebSocketAccess, WorkspaceSubscription(..))
import HMem.DB.TestHarness (TestEnv(..), createTestWorkspace)
import HMem.DB.Pool qualified as Pool
import HMem.DB.ChangeStream (ChangeScope(..))
import HMem.DB.Auth qualified as Auth
import HMem.BotIdentity (mkBotIdentity)
import HMem.Server.Snapshot (materializeSnapshot)
import HMem.Server.TestHarness (LocalSandboxApp(..), DeployedSandboxApp(..), withLocalSandboxAppContext, withDeployedSandboxAppContext, createDeployedSandboxUser, issueDeployedSandboxPAT)
import HMem.Server.AuthTokens (IssuedAccessToken(..))
import HMem.Types (Workspace(..), Observation(..), ObservationProvenance(..), ObservationRevisionEvent(..), PaginatedResult(..), SessionContext(..), SessionPrincipal(..), SessionActor(..))
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson (Value(..))
import Network.HTTP.Types (Header, Method, methodGet, methodPost, status200, status400, status401, status403)
import Network.Wai (Application, defaultRequest, responseLBS)
import Network.Wai qualified as Wai
import Network.Wai.Test
import Test.Hspec

spec :: Spec
spec = describe "HTTP declared bot identity" $ do
  it "authenticates a distinct local human bearer without a bot declaration" $ do
    response <- callBearer humanAuth "human-secret" []
    simpleStatus response `shouldBe` status200
    simpleBody response `shouldBe` encode (object
      ["principal_id" .= ("local-user" :: String), "authority" .= show PrincipalSyntheticLocalSuperadmin
      , "actor_type" .= ("user" :: String), "has_identity" .= False])
    wrong <- callBearer humanAuth "human-secreu" []
    simpleStatus wrong `shouldBe` status401

  it "keeps API and configured-bot bearers as declared bots beside the human credential" $
    mapM_ (\token -> do
      missing <- callBearer humanAuth token []
      simpleStatus missing `shouldBe` status400
      declared <- callBearer humanAuth token [identityHeader]
      simpleStatus declared `shouldBe` status200
      fields <- case decode (simpleBody declared) of
        Just (Object value) -> pure value
        other -> fail ("expected bot attribution response, got " <> show other)
      KM.lookup "actor_type" fields `shouldBe` Just (String "bot"))
      ["machine-secret", "configured-bot-secret"]

  it "rejects dormant and colliding human credentials instead of falling back" $ do
    let req = bearerRequest "human-secret" []
    resolveRequestPrincipal (humanAuth { Config.enabled = False }) req `shouldBe` Nothing
    resolveRequestPrincipal (humanAuth { Config.mode = Config.AuthModeDeployed }) req `shouldBe` Nothing
    mapM_ (\cfg -> do
      resolveRequestPrincipal cfg req `shouldBe` Nothing
      denied <- callBearer cfg "human-secret" []
      simpleStatus denied `shouldBe` status401)
      [ humanAuth { Config.apiKey = Just "human-secret" }
      , humanAuth { Config.local = humanAuth.local { Config.botTokens = [Config.LocalBotTokenConfig "collision" "human-secret"] } }
      , humanAuth { Config.local = humanAuth.local { Config.userToken = Just "   " } }
      ]

  it "supports human-only local bearer gating and the same WebSocket identity" $ do
    let cfg = humanAuth { Config.apiKey = Nothing }
    response <- callBearer cfg "human-secret" []
    simpleStatus response `shouldBe` status200
    denied <- callBearer cfg "wrong" []
    simpleStatus denied `shouldBe` status401
    case resolveLocalWebSocketAccess cfg (Just "human-secret") of
      Just (Just original, SubscribeAllWorkspaces) -> original `shouldBe` principal
      other -> expectationFailure ("expected authenticated human WebSocket, got " <> show other)
    resolveLocalWebSocketAccess cfg Nothing `shouldBe` Nothing
    resolveLocalWebSocketAccess cfg (Just "wrong") `shouldBe` Nothing
    resolveLocalWebSocketAccess (cfg { Config.apiKey = Just "human-secret" }) (Just "human-secret") `shouldBe` Nothing

  it "attributes an implicit local declaration without changing its authority or audience ID" $ do
    response <- call local principal methodPost [identityHeader]
    simpleStatus response `shouldBe` status200
    simpleBody response `shouldBe` encode (object
      ["principal_id" .= ("local-user" :: String), "authority" .= show PrincipalSyntheticLocalSuperadmin
      , "actor_type" .= ("bot" :: String), "has_identity" .= True])

  it "keeps undeclared human requests human and authenticated bot IDs and grants unchanged" $ do
    human <- call local principal methodPost []
    simpleStatus human `shouldBe` status200
    simpleBody human `shouldBe` encode (object
      ["principal_id" .= ("local-user" :: String), "authority" .= show PrincipalSyntheticLocalSuperadmin
      , "actor_type" .= ("user" :: String), "has_identity" .= False])
    bot <- call deployed botPrincipal methodPost [identityHeader]
    simpleStatus bot `shouldBe` status200
    simpleBody bot `shouldBe` encode (object
      ["principal_id" .= ("credential-bot" :: String), "authority" .= show (PrincipalGrantUser UUID.nil)
      , "actor_type" .= ("bot" :: String), "has_identity" .= True])

  it "does not grant deployed callers an implicit local persona" $ do
    response <- call deployed principal methodPost [identityHeader]
    simpleStatus response `shouldBe` status200
    simpleBody response `shouldBe` encode (object
      ["principal_id" .= ("local-user" :: String), "authority" .= show PrincipalSyntheticLocalSuperadmin
      , "actor_type" .= ("user" :: String), "has_identity" .= True])

  it "requires direct API mutation declarations for both local and deployed known bots" $ do
    mapM_ (\cfg -> do
      missing <- call cfg botPrincipal methodPost []
      simpleStatus missing `shouldBe` status400
      readOnly <- call cfg botPrincipal methodGet []
      simpleStatus readOnly `shouldBe` status200) [local, deployed]

  it "rejects malformed declarations, duplicate headers, spoofed IDs and oversized input even on reads" $ do
    mapM_ (\headers -> do
      response <- call local principal methodGet headers
      simpleStatus response `shouldBe` status400)
      [[("X-HMem-Bot-Identity", "null")], [("X-HMem-Bot-Identity", "{}")]
      , [identityHeader, identityHeader]
      , [("X-HMem-Bot-Identity", "{\"role\":\"Reviewer\",\"model-id\":\"pi:m:r\",\"actor_id\":\"local-user\"}")]
      , [("X-HMem-Bot-Identity", LBS.toStrict (LBS.replicate 4097 32))]]

  it "restores declaration context between separate requests" $ do
    _ <- call local principal methodPost [identityHeader]
    currentEffectiveActor `shouldReturn` Nothing
    currentPrincipal `shouldReturn` Nothing

  it "exposes session attribution changes while retaining local authority permissions and cache identity" $
    withLocalSandboxAppContext $ \ctx -> do
      workspace <- createTestWorkspace ctx.localEnv "declared-local-session"
      plain <- sessionRequest ctx.localApplication [] workspace.id >>= sessionBody
      first <- sessionRequest ctx.localApplication [identityHeader] workspace.id >>= sessionBody
      second <- sessionRequest ctx.localApplication [otherIdentityHeader] workspace.id >>= sessionBody
      map (.principal) [first, second] `shouldBe` replicate 2 plain.principal
      map (.globalPermissions) [first, second] `shouldBe` replicate 2 plain.globalPermissions
      map (.workspace) [first, second] `shouldBe` replicate 2 plain.workspace
      let Just human = plain.effectiveActor
          Just actorA = first.effectiveActor
          Just actorB = second.effectiveActor
      human.actorType `shouldBe` "user"
      human.actorId `shouldBe` plain.principal.actorId
      actorA.actorType `shouldBe` "bot"
      actorA.actorId `shouldNotBe` actorB.actorId
      actorA.botIdentity `shouldBe` either (const Nothing) Just (mkBotIdentity "Reviewer" "pi:gpt-6.1-sol:extra-high")

  it "keeps deployed grant user and workspace access independent of declared role and model" $
    withDeployedSandboxAppContext $ \ctx -> do
      workspace <- createTestWorkspace ctx.deployedEnv "declared-deployed-session"
      forbidden <- createTestWorkspace ctx.deployedEnv "declared-forbidden-session"
      user <- createDeployedSandboxUser ctx.deployedEnv False False
      _ <- Auth.upsertWorkspaceMembership ctx.deployedEnv.pool workspace.id
        (Auth.UpsertWorkspaceMembership user Auth.WorkspaceRoleRead) Nothing
      credential <- issueDeployedSandboxPAT ctx.deployedEnv user "Declared PAT"
      let authorization = ("Authorization", "Bearer " <> TE.encodeUtf8 credential.rawToken)
      first <- sessionRequest ctx.deployedApplication [authorization, identityHeader] workspace.id >>= sessionBody
      second <- sessionRequest ctx.deployedApplication [authorization, otherIdentityHeader] workspace.id >>= sessionBody
      second.principal `shouldBe` first.principal
      second.principal.grantUserId `shouldBe` Just user
      second.globalPermissions `shouldBe` first.globalPermissions
      second.workspace `shouldBe` first.workspace
      fmap (.actorId) second.effectiveActor `shouldBe` Just first.principal.actorId
      fmap (.botIdentity) second.effectiveActor `shouldNotBe` fmap (.botIdentity) first.effectiveActor
      denied <- sessionRequest ctx.deployedApplication [authorization, otherIdentityHeader] forbidden.id
      simpleStatus denied `shouldBe` status403

  it "carries declared identity through REST history and the existing full snapshot projection" $
    withLocalSandboxAppContext $ \ctx -> do
      workspace <- createTestWorkspace ctx.localEnv "declared-rest-snapshot"
      let request method path headers body = runSession (srequest (SRequest defaultRequest
            { Wai.requestMethod = method, Wai.pathInfo = path, Wai.requestHeaders = ("Content-Type", "application/json") : headers } body)) ctx.localApplication
      created <- request methodPost ["api", "v1", "observations"] [identityHeader] $ encode $ object
        ["workspace_id" .= workspace.id, "subject_kind" .= ("file" :: T.Text)
        , "subject" .= ("src/Identity.hs" :: T.Text), "git_sha" .= T.replicate 40 "a", "content" .= ("identity" :: T.Text)]
      simpleStatus created `shouldBe` status200
      row <- maybe (fail "expected Observation") pure (decode (simpleBody created) :: Maybe Observation)
      let Just provenance = row.currentProvenance
      fmap (const True) provenance.botIdentity `shouldBe` Just True
      provenance.actorType `shouldBe` Just "bot"
      history <- request methodGet ["api", "v1", "observations", UUID.toText row.id, "history"] [] ""
      simpleStatus history `shouldBe` status200
      events <- maybe (fail "expected history page") pure (decode (simpleBody history) :: Maybe (PaginatedResult ObservationRevisionEvent))
      map (.provenance) events.items `shouldBe` [provenance]
      snapshots <- Pool.runSession ctx.localEnv.pool (materializeSnapshot (WorkspaceScope workspace.id))
      let observationItems = [fields | Object item <- snapshots, KM.lookup "kind" item == Just (String "observation"), Just (Object fields) <- [KM.lookup "data" item]]
      decoded <- mapM (\value -> case fromJSON value of
        Success result -> pure result
        Error message -> fail message)
        [value | fields <- observationItems, Just value <- [KM.lookup "current_provenance" fields]]
      decoded `shouldBe` [provenance]

local, deployed :: Config.AuthConfig
local = Config.defaultConfig.auth
deployed = local { Config.mode = Config.AuthModeDeployed }

humanAuth :: Config.AuthConfig
humanAuth = local { Config.enabled = True, Config.apiKey = Just "machine-secret"
  , Config.local = local.local { Config.bootstrapEnabled = False, Config.userToken = Just "human-secret"
    , Config.botTokens = [Config.LocalBotTokenConfig "agent" "configured-bot-secret"] } }

bearerRequest :: T.Text -> [Header] -> Wai.Request
bearerRequest token headers = defaultRequest
  { Wai.requestMethod = methodPost, Wai.pathInfo = ["api", "v1", "workspaces"]
  , Wai.requestHeaders = ("Authorization", "Bearer " <> TE.encodeUtf8 token) : headers }

callBearer :: Config.AuthConfig -> T.Text -> [Header] -> IO SResponse
callBearer cfg token headers =
  let req = bearerRequest token headers
  in withPrincipalContext (resolveRequestPrincipal cfg req) $
    runSession (srequest (SRequest req "")) (authMiddleware cfg (botIdentityMiddleware cfg inspect))

principal, botPrincipal :: Principal
principal = Principal ActorUser "local-user" "Local User" PrincipalSyntheticLocalSuperadmin
botPrincipal = Principal ActorBot "credential-bot" "Credential Bot" (PrincipalGrantUser UUID.nil)

identityHeader :: Header
identityHeader = ("X-HMem-Bot-Identity", "{\"role\":\"Reviewer\",\"model-id\":\"pi:gpt-6.1-sol:extra-high\"}")

otherIdentityHeader :: Header
otherIdentityHeader = ("X-HMem-Bot-Identity", "{\"role\":\"Superadmin\",\"model-id\":\"other:model:default\"}")

sessionRequest :: Application -> [Header] -> UUID.UUID -> IO SResponse
sessionRequest app headers workspace = runSession (srequest (SRequest defaultRequest
  { Wai.requestMethod = methodGet, Wai.pathInfo = ["api", "v1", "session"]
  , Wai.queryString = [("workspace_id", Just (TE.encodeUtf8 (UUID.toText workspace)))]
  , Wai.requestHeaders = headers } "")) app

sessionBody :: SResponse -> IO SessionContext
sessionBody response = do
  simpleStatus response `shouldBe` status200
  maybe (fail "expected session response") pure (decode (simpleBody response))

call :: Config.AuthConfig -> Principal -> Method -> [Header] -> IO SResponse
call cfg original method headers =
  withPrincipalContext (Just original) $ runSession
    (srequest (SRequest defaultRequest
      { Wai.requestMethod = method, Wai.pathInfo = ["api", "v1", "workspaces"]
      , Wai.requestHeaders = headers } ""))
    (botIdentityMiddleware cfg inspect)

inspect :: Application
inspect _ respond = do
  Just original <- currentPrincipal
  Just actor <- currentEffectiveActor
  respond $ responseLBS status200 [] $ encode $ object
    ["principal_id" .= original.actorId, "authority" .= show original.authority
    , "actor_type" .= actorTypeToText actor.actorType, "has_identity" .= maybe False (const True) actor.botIdentity]
