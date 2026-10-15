module HMem.Server.BotIdentitySpec (spec) where

import Data.Aeson (encode, object, (.=))
import Data.ByteString.Lazy qualified as LBS
import Data.UUID qualified as UUID
import HMem.Config qualified as Config
import HMem.DB.RequestContext
import HMem.Server.App (botIdentityMiddleware)
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
