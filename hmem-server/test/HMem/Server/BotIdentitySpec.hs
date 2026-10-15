module HMem.Server.BotIdentitySpec (spec) where

import Data.Aeson (Result(..), decode, encode, fromJSON, object, (.=))
import Data.Text qualified as T
import Data.ByteString.Lazy qualified as LBS
import Data.UUID qualified as UUID
import HMem.Config qualified as Config
import HMem.DB.RequestContext
import HMem.Server.App (botIdentityMiddleware)
import HMem.DB.TestHarness (TestEnv(..), createTestWorkspace)
import HMem.DB.Pool qualified as Pool
import HMem.DB.ChangeStream (ChangeScope(..))
import HMem.Server.Snapshot (materializeSnapshot)
import HMem.Server.TestHarness (LocalSandboxApp(..), withLocalSandboxAppContext)
import HMem.Types (Workspace(..), Observation(..), ObservationProvenance(..), ObservationRevisionEvent(..), PaginatedResult(..))
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson (Value(..))
import Network.HTTP.Types (Header, Method, methodGet, methodPost, status200, status400)
import Network.Wai (Application, defaultRequest, responseLBS)
import Network.Wai qualified as Wai
import Network.Wai.Test
import Test.Hspec

spec :: Spec
spec = describe "HTTP declared bot identity" $ do
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

principal, botPrincipal :: Principal
principal = Principal ActorUser "local-user" "Local User" PrincipalSyntheticLocalSuperadmin
botPrincipal = Principal ActorBot "credential-bot" "Credential Bot" (PrincipalGrantUser UUID.nil)

identityHeader :: Header
identityHeader = ("X-HMem-Bot-Identity", "{\"role\":\"Reviewer\",\"model-id\":\"pi:gpt-6.1-sol:extra-high\"}")

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
