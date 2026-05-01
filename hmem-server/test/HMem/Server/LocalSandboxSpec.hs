module HMem.Server.LocalSandboxSpec (spec) where

import Control.Exception (bracket)
import Data.Aeson (FromJSON, decode, encode, object, (.=))
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Network.HTTP.Types (HeaderName, Method, methodGet, methodPost, parseQuery, statusCode)
import Network.Wai (Application, defaultRequest)
import Network.Wai qualified as Wai
import Network.Wai.Test (SRequest(..), SResponse(..), runSession, srequest)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import Test.Hspec

import HMem.Config qualified as Config
import HMem.DB.RequestContext (ActorType(..), Principal(..), PrincipalAuthority(..))
import HMem.DB.TestHarness (AuditLogRow(..), TestDb(..), TestEnv(..), TestSandbox(..), createTestWorkspace, getAuditLogRows)
import HMem.Server.TestHarness
import HMem.Server.WebSocket (WorkspaceSubscription(..), resolveLocalWebSocketAccess)
import HMem.Types (SessionContext(..), SessionGlobalPermissions(..), SessionPrincipal(..), Workspace(..))

spec :: Spec
spec = describe "local-mode sandbox profile" $ do
  it "constructs explicit loopback local config under sandbox paths and ignores ambient credentials" $
    withEnvVar "HMEM_API_KEY" (Just "ambient-api-key") $
      withEnvVar "HMEM_DB_PASSWORD" (Just "ambient-db-password") $
        withEnvVar "HMEM_TEST_DB" (Just "host=real.example dbname=real_hmem") $
          withEnvVar "HMEM_TEST_EXTERNAL_DB" (Just "host=external.example dbname=external_hmem") $
            withEnvVar "HMEM_TEST_ALLOW_EXTERNAL_DB" (Just "1") $
              withLocalSandboxAppContext $ \ctx -> do
            let cfg = ctx.localConfig
                env = ctx.localEnv
            cfg.auth.mode `shouldBe` Config.AuthModeLocal
            cfg.auth.enabled `shouldBe` False
            cfg.auth.apiKey `shouldBe` Nothing
            cfg.auth.local.bootstrapEnabled `shouldBe` True
            cfg.auth.local.allowRemoteBootstrap `shouldBe` False
            cfg.auth.local.botTokens `shouldBe`
              [ Config.LocalBotTokenConfig
                  { Config.label = localSandboxBotLabel
                  , Config.token = localSandboxBotToken
                  }
              ]
            cfg.server.host `shouldBe` "127.0.0.1"
            Config.serverHostIsLoopback cfg.server.host `shouldBe` True
            Config.corsAllowsRemoteOrigins cfg.cors `shouldBe` False
            Config.localImplicitBootstrapStartupError cfg `shouldBe` Nothing
            cfg.database.port `shouldBe` env.testDb.testDbPort
            cfg.database.name `shouldBe` env.testDb.testDbName
            cfg.database.password `shouldBe` Nothing
            env.testDb.testDbUnsafeExternal `shouldBe` False
            cfg.web.webStaticDir `shouldBe` Just env.testSandbox.sandboxStaticDir
            lookupEnv "HMEM_API_KEY" >>= (`shouldBe` Nothing)
            lookupEnv "HMEM_DB_PASSWORD" >>= (`shouldBe` Nothing)
            lookupEnv "HMEM_TEST_DB" >>= (`shouldBe` Nothing)
            lookupEnv "HMEM_TEST_EXTERNAL_DB" >>= (`shouldBe` Nothing)
            lookupEnv "HMEM_TEST_ALLOW_EXTERNAL_DB" >>= (`shouldBe` Nothing)

  it "returns the implicit local superadmin session" $
    withLocalSandboxApp $ \app -> do
      resp <- get_ app "/api/v1/session"
      respStatus resp `shouldBe` 200
      session <- expectDecoded (respBody resp) :: IO SessionContext
      session.authMode `shouldBe` "local"
      session.principal.actorType `shouldBe` "user"
      session.principal.actorId `shouldBe` "local-user"
      session.principal.authority `shouldBe` "local_superadmin"
      session.globalPermissions.createWorkspace `shouldBe` True
      session.globalPermissions.superadmin `shouldBe` True

  it "records configured local bot attribution and not ambient static-bearer attribution" $
    withEnvVar "HMEM_API_KEY" (Just "ambient-api-key") $
      withLocalSandboxAppContext $ \ctx -> do
        let app = ctx.localApplication
            env = ctx.localEnv
        botResp <- runReqWithHeaders app methodPost "/api/v1/workspaces"
          [("Authorization", "Bearer " <> encodeUtf8Text localSandboxBotToken)]
          (encode (object ["name" .= ("local-sandbox-bot-ws" :: T.Text)]))
        respStatus botResp `shouldBe` 200
        botWs <- expectDecoded (respBody botResp) :: IO Workspace
        botRows <- getAuditLogRows env.pool "workspace" (T.pack (show botWs.id))
        botCreated <- expectSingle botRows
        botCreated.actorType `shouldBe` Just "bot"
        botCreated.actorId `shouldBe` Just ("local-bot:" <> localSandboxBotLabel)
        botCreated.actorLabel `shouldBe` Just localSandboxBotLabel

        ambientResp <- runReqWithHeaders app methodPost "/api/v1/workspaces"
          [("Authorization", "Bearer ambient-api-key")]
          (encode (object ["name" .= ("local-sandbox-ambient-ws" :: T.Text)]))
        respStatus ambientResp `shouldBe` 200
        ambientWs <- expectDecoded (respBody ambientResp) :: IO Workspace
        ambientRows <- getAuditLogRows env.pool "workspace" (T.pack (show ambientWs.id))
        ambientCreated <- expectSingle ambientRows
        ambientCreated.actorType `shouldBe` Just "user"
        ambientCreated.actorId `shouldBe` Just "local-user"
        ambientCreated.actorLabel `shouldBe` Just "Local User"

  it "uses safe local WebSocket defaults and explicit local bot tokens" $
    withLocalSandboxAppContext $ \ctx -> do
      let cfg = ctx.localConfig
      case resolveLocalWebSocketAccess cfg.auth Nothing of
        Just (Just principal, SubscribeAllWorkspaces) -> do
          principal.actorType `shouldBe` ActorUser
          principal.actorId `shouldBe` "local-user"
          principal.authority `shouldBe` PrincipalSyntheticLocalSuperadmin
        other -> expectationFailure $ "expected implicit local WebSocket access, got " <> show other

      case resolveLocalWebSocketAccess cfg.auth (Just localSandboxBotToken) of
        Just (Just principal, SubscribeAllWorkspaces) -> do
          principal.actorType `shouldBe` ActorBot
          principal.actorId `shouldBe` "local-bot:" <> localSandboxBotLabel
          principal.actorLabel `shouldBe` localSandboxBotLabel
          principal.authority `shouldBe` PrincipalSyntheticLocalSuperadmin
        other -> expectationFailure $ "expected local bot WebSocket access, got " <> show other

      let exposedLocalAuth = cfg.auth
            { Config.local = cfg.auth.local { Config.allowRemoteBootstrap = True }
            }
      resolveLocalWebSocketAccess exposedLocalAuth Nothing `shouldBe` Nothing

  it "keeps exposed-local validation gates active outside the safe profile" $
    withLocalSandboxAppContext $ \ctx -> do
      let cfg = ctx.localConfig
          unsafeHost = cfg { Config.server = Config.ServerConfig { Config.host = "0.0.0.0", Config.port = cfg.server.port } }
          unsafeCors = cfg { Config.cors = Config.CorsConfig { Config.allowedOrigins = ["*"] } }
          escapeHatch = unsafeHost
            { Config.auth = unsafeHost.auth
                { Config.local = unsafeHost.auth.local { Config.allowRemoteBootstrap = True }
                }
            }
      Config.localImplicitBootstrapStartupError cfg `shouldBe` Nothing
      Config.localImplicitBootstrapStartupError unsafeHost `shouldSatisfy` maybe False (T.isInfixOf "server.host")
      Config.localImplicitBootstrapStartupError unsafeCors `shouldSatisfy` maybe False (T.isInfixOf "implicit local superadmin")
      Config.localImplicitBootstrapStartupError escapeHatch `shouldBe` Nothing

  it "can still create normal local-mode fixtures through the shared sandbox DB" $
    withLocalSandboxAppContext $ \ctx -> do
      ws <- createTestWorkspace ctx.localEnv "local-profile-fixture-ws"
      resp <- get_ ctx.localApplication ("/api/v1/workspaces/" <> asciiBytes (show ws.id))
      respStatus resp `shouldBe` 200

runReqWithHeaders :: Application -> Method -> BS.ByteString -> [(HeaderName, BS.ByteString)] -> LBS.ByteString -> IO SResponse
runReqWithHeaders app method fullPath headers body =
  runSession (srequest $ SRequest req body) app
  where
    (rawPath, rawQS) = BS.break (== 0x3F) fullPath
    req = defaultRequest
      { Wai.requestMethod  = method
      , Wai.rawPathInfo    = rawPath
      , Wai.pathInfo       = filter (not . T.null) $ T.split (== '/') $ decodeUtf8 rawPath
      , Wai.rawQueryString = rawQS
      , Wai.queryString    = parseQuery rawQS
      , Wai.requestHeaders = [("Content-Type", "application/json")] <> headers
      }

get_ :: Application -> BS.ByteString -> IO SResponse
get_ app path = runReqWithHeaders app methodGet path [] ""

respStatus :: SResponse -> Int
respStatus = statusCode . simpleStatus

respBody :: SResponse -> LBS.ByteString
respBody = simpleBody

expectDecoded :: FromJSON a => LBS.ByteString -> IO a
expectDecoded body = case decode body of
  Just value -> pure value
  Nothing -> do
    let message = "failed to decode response body: " <> show body
    expectationFailure message
    fail message

expectSingle :: Show a => [a] -> IO a
expectSingle [value] = pure value
expectSingle values = do
  let message = "expected exactly one row, got: " <> show values
  expectationFailure message
  fail message

encodeUtf8Text :: T.Text -> BS.ByteString
encodeUtf8Text = encodeUtf8

asciiBytes :: String -> BS.ByteString
asciiBytes = BS8.pack

withEnvVar :: String -> Maybe String -> IO a -> IO a
withEnvVar name value = bracket setup restore . const
  where
    setup = do
      old <- lookupEnv name
      case value of
        Nothing -> unsetEnv name
        Just raw -> setEnv name raw
      pure old

    restore old = case old of
      Nothing -> unsetEnv name
      Just raw -> setEnv name raw
