module HMem.Server.DeployedSandboxSpec (spec) where

import Control.Exception (bracket)
import Data.Aeson (FromJSON, decode, encode, object, (.=))
import Data.ByteString qualified as BS
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
import HMem.DB.TestHarness (TestDb(..), TestEnv(..), TestSandbox(..))
import HMem.Server.AuthTokens (IssuedAccessToken(..))
import HMem.Server.TestHarness
import HMem.Types (SessionContext(..), SessionGlobalPermissions(..), SessionPrincipal(..))

spec :: Spec
spec = describe "deployed-mode sandbox profile" $ do
  it "constructs explicit deployed config under sandbox paths and ignores ambient local credentials" $
    withEnvVar "HMEM_API_KEY" (Just "ambient-local-api-key") $
      withEnvVar "HMEM_DB_PASSWORD" (Just "ambient-db-password") $
        withEnvVar "HMEM_TEST_DB" (Just "host=real.example dbname=real_hmem") $
          withEnvVar "HMEM_TEST_EXTERNAL_DB" (Just "host=external.example dbname=external_hmem") $
            withEnvVar "HMEM_TEST_ALLOW_EXTERNAL_DB" (Just "1") $
              withEnvVar "KEYCLOAK_URL" (Just "https://real-keycloak.example") $
                withDeployedSandboxAppContext $ \ctx -> do
                  let cfg = ctx.deployedConfig
                      env = ctx.deployedEnv
                  cfg.auth.mode `shouldBe` Config.AuthModeDeployed
                  cfg.auth.enabled `shouldBe` False
                  cfg.auth.apiKey `shouldBe` Nothing
                  cfg.auth.local.bootstrapEnabled `shouldBe` False
                  cfg.auth.local.allowRemoteBootstrap `shouldBe` False
                  cfg.auth.local.botTokens `shouldBe` []
                  cfg.auth.deployed.issuer `shouldBe` Just deployedSandboxIssuer
                  cfg.auth.deployed.audience `shouldBe` Just deployedSandboxAudience
                  cfg.auth.deployed.jwks `shouldSatisfy` maybe False (const True)
                  cfg.auth.deployed.tokenHashSecret `shouldBe` Just deployedSandboxTokenHashSecret
                  cfg.auth.deployed.clientId `shouldBe` Just deployedSandboxAudience
                  cfg.auth.deployed.cookieSecure `shouldBe` True
                  cfg.auth.deployed.cookieSameSite `shouldBe` "Lax"
                  cfg.server.host `shouldBe` "127.0.0.1"
                  Config.serverHostIsLoopback cfg.server.host `shouldBe` True
                  Config.corsAllowsRemoteOrigins cfg.cors `shouldBe` False
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
                  lookupEnv "KEYCLOAK_URL" >>= (`shouldBe` Nothing)

  it "provides a cross-origin cookie variant with secure SameSite=None settings" $
    withDeployedSandboxCrossOriginAppContext ["https://frontend.example"] $ \ctx -> do
      let cfg = ctx.deployedConfig
      cfg.auth.mode `shouldBe` Config.AuthModeDeployed
      cfg.cors.allowedOrigins `shouldBe` ["https://frontend.example"]
      Config.corsAllowsRemoteOrigins cfg.cors `shouldBe` True
      cfg.auth.deployed.cookieSecure `shouldBe` True
      cfg.auth.deployed.cookieSameSite `shouldBe` "None"

  it "fails closed when deployed provider and token-hash config is absent or mismatched" $
    withDeployedSandboxAppConfig misconfiguredDeployedConfig $ \ctx -> do
      _ <- createDeployedSandboxUserWithSubject ctx.deployedEnv "misconfigured-provider-user" True False
      grantUserId <- createDeployedSandboxUser ctx.deployedEnv True False
      issued <- issueDeployedSandboxPAT ctx.deployedEnv grantUserId "Misconfigured PAT Bot"
      jwt <- signDeployedSandboxJwt "misconfigured-provider-user"

      jwtResp <- runReqWithHeaders ctx.deployedApplication methodGet "/api/v1/session"
        [("Authorization", "Bearer " <> encodeUtf8 jwt)]
        ""
      respStatus jwtResp `shouldBe` 401

      patResp <- runReqWithHeaders ctx.deployedApplication methodGet "/api/v1/session"
        [("Authorization", "Bearer " <> encodeUtf8 issued.rawToken)]
        ""
      respStatus patResp `shouldBe` 401

  it "fails closed without a principal and ignores ambient local-mode bearer credentials" $
    withEnvVar "HMEM_API_KEY" (Just "ambient-local-api-key") $
      withDeployedSandboxAppContext $ \ctx -> do
        let app = ctx.deployedApplication
        sessionResp <- get_ app "/api/v1/session"
        respStatus sessionResp `shouldBe` 401

        createResp <- runReqWithHeaders app methodPost "/api/v1/workspaces" []
          (encode (object ["name" .= ("deployed-no-principal" :: T.Text)]))
        respStatus createResp `shouldBe` 401

        ambientBearerResp <- runReqWithHeaders app methodGet "/api/v1/session"
          [("Authorization", "Bearer ambient-local-api-key")]
          ""
        respStatus ambientBearerResp `shouldBe` 401

        malformedJwtResp <- runReqWithHeaders app methodGet "/api/v1/session"
          [("Authorization", "Bearer not-a-sandbox-token")]
          ""
        respStatus malformedJwtResp `shouldBe` 401

  it "resolves sandbox-created PAT users with the deterministic token hash secret" $
    withDeployedSandboxAppContext $ \ctx -> do
      userId <- createDeployedSandboxUser ctx.deployedEnv True False
      issued <- issueDeployedSandboxPAT ctx.deployedEnv userId "Sandbox PAT Bot"

      sessionResp <- runReqWithHeaders ctx.deployedApplication methodGet "/api/v1/session"
        [("Authorization", "Bearer " <> encodeUtf8 issued.rawToken)]
        ""
      respStatus sessionResp `shouldBe` 200
      session <- expectDecoded (respBody sessionResp) :: IO SessionContext
      session.authMode `shouldBe` "deployed"
      session.principal.actorType `shouldBe` "bot"
      session.principal.actorLabel `shouldBe` "Sandbox PAT Bot"
      session.principal.grantUserId `shouldBe` Just userId
      session.globalPermissions.createWorkspace `shouldBe` True

      createResp <- runReqWithHeaders ctx.deployedApplication methodPost "/api/v1/workspaces"
        [("Authorization", "Bearer " <> encodeUtf8 issued.rawToken)]
        (encode (object ["name" .= ("deployed-pat-created-ws" :: T.Text)]))
      respStatus createResp `shouldBe` 200

  it "resolves sandbox-signed JWT users through inline JWKS" $
    withDeployedSandboxAppContext $ \ctx -> do
      userId <- createDeployedSandboxUserWithSubject ctx.deployedEnv "sandbox-jwt-user" True False
      token <- signDeployedSandboxJwt "sandbox-jwt-user"

      sessionResp <- runReqWithHeaders ctx.deployedApplication methodGet "/api/v1/session"
        [("Authorization", "Bearer " <> encodeUtf8 token)]
        ""
      respStatus sessionResp `shouldBe` 200
      session <- expectDecoded (respBody sessionResp) :: IO SessionContext
      session.authMode `shouldBe` "deployed"
      session.principal.actorType `shouldBe` "user"
      session.principal.grantUserId `shouldBe` Just userId
      session.globalPermissions.createWorkspace `shouldBe` True

  it "resolves sandbox-created cookie sessions and enforces CSRF on unsafe requests" $
    withDeployedSandboxAppContext $ \ctx -> do
      userId <- createDeployedSandboxUser ctx.deployedEnv True False
      _ <- createDeployedSandboxAuthSession ctx.deployedEnv userId "sandbox-session-token" "sandbox-csrf-token"
      let sessionCookie = cookieHeader [("hmem_session", "sandbox-session-token")]
          csrfCookies = cookieHeader [("hmem_session", "sandbox-session-token"), ("hmem_csrf", "sandbox-csrf-token")]
          body = encode (object ["name" .= ("deployed-cookie-created-ws" :: T.Text)])

      sessionResp <- runReqWithHeaders ctx.deployedApplication methodGet "/api/v1/session" [sessionCookie] ""
      respStatus sessionResp `shouldBe` 200
      session <- expectDecoded (respBody sessionResp) :: IO SessionContext
      session.principal.grantUserId `shouldBe` Just userId

      missingCsrf <- runReqWithHeaders ctx.deployedApplication methodPost "/api/v1/workspaces" [sessionCookie] body
      respStatus missingCsrf `shouldBe` 403

      ok <- runReqWithHeaders ctx.deployedApplication methodPost "/api/v1/workspaces"
        [csrfCookies, ("X-CSRF-Token", "sandbox-csrf-token")]
        body
      respStatus ok `shouldBe` 200

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

cookieHeader :: [(T.Text, T.Text)] -> (HeaderName, BS.ByteString)
cookieHeader cookies =
  ("Cookie", BS.intercalate "; " [encodeUtf8 name <> "=" <> encodeUtf8 value | (name, value) <- cookies])

expectDecoded :: FromJSON a => LBS.ByteString -> IO a
expectDecoded body = case decode body of
  Just value -> pure value
  Nothing -> do
    let message = "failed to decode response body: " <> show body
    expectationFailure message
    fail message

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

misconfiguredDeployedConfig :: TestEnv -> Config.HMemConfig
misconfiguredDeployedConfig env =
  let cfg = mkDeployedSandboxConfig env
  in cfg
      { Config.auth = cfg.auth
          { Config.deployed = cfg.auth.deployed
              { Config.issuer = Nothing
              , Config.audience = Nothing
              , Config.jwks = Nothing
              , Config.tokenHashSecret = Just "wrong-token-hash-secret"
              }
          }
      }
