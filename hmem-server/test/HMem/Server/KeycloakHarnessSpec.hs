module HMem.Server.KeycloakHarnessSpec (spec) where

import Control.Exception (bracket)
import Data.Aeson (FromJSON, decode)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.List (isPrefixOf)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Network.HTTP.Types (HeaderName, Method, methodGet, parseQuery, statusCode)
import Network.Wai (Application, defaultRequest)
import Network.Wai qualified as Wai
import Network.Wai.Test (SRequest(..), SResponse(..), runSession, srequest)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import Test.Hspec

import HMem.Config qualified as Config
import HMem.DB.TestHarness (TestEnv(..), TestSandbox(..), withTestEnv)
import HMem.Server.AccessTracker (newAccessTracker)
import HMem.Server.App (mkApp)
import HMem.Server.KeycloakHarness
import HMem.Server.TestHarness (createDeployedSandboxUserWithSubject, mkDeployedSandboxConfig)
import HMem.Server.WebSocket (newWSState)
import HMem.Types (SessionContext(..), SessionGlobalPermissions(..), SessionPrincipal(..))

spec :: Spec
spec = describe "opt-in isolated Keycloak provider harness" $ do
  it "is skipped unless HMEM_TEST_KEYCLOAK=1 is set" $ do
    enabled <- keycloakEnabled
    if enabled
      then pendingWith "HMEM_TEST_KEYCLOAK=1 set; real Keycloak harness is exercised by the provider-token test"
      else pendingWith "Set HMEM_TEST_KEYCLOAK=1 to run isolated Docker Keycloak tests"

  it "starts disposable Keycloak and validates provider-issued ID tokens" $ do
    enabled <- keycloakEnabled
    if not enabled
      then pendingWith "Set HMEM_TEST_KEYCLOAK=1 to run isolated Docker Keycloak tests"
      else withEnvVar "KEYCLOAK_URL" (Just "https://real-keycloak.example") $
        withTestEnv $ \env ->
          withSandboxedKeycloak env $ \kc -> do
            lookupEnv "KEYCLOAK_URL" >>= (`shouldBe` Nothing)
            kc.keycloakBaseUrl `shouldSatisfy` T.isPrefixOf "http://127.0.0.1:"
            kc.keycloakRealm `shouldSatisfy` T.isPrefixOf "hmem-sandbox-"
            kc.keycloakContainerName `shouldSatisfy` T.isPrefixOf "hmem-keycloak-" . T.pack
            let TestEnv { testSandbox = sandbox } = env
                TestSandbox { sandboxLogDir = logDir } = sandbox
            kc.keycloakLogFile `shouldSatisfy` (logDir `isPrefixOfPath`)

            app <- mkKeycloakBackedApp env kc
            userId <- createDeployedSandboxUserWithSubject env kc.keycloakUserSubject True False

            sessionResp <- runReqWithHeaders app methodGet "/api/v1/session"
              [("Authorization", "Bearer " <> encodeUtf8 kc.keycloakIdToken)]
              ""
            respStatus sessionResp `shouldBe` 200
            session <- expectDecoded (respBody sessionResp) :: IO SessionContext
            session.authMode `shouldBe` "deployed"
            session.principal.actorType `shouldBe` "user"
            session.principal.grantUserId `shouldBe` Just userId
            session.globalPermissions.createWorkspace `shouldBe` True

mkKeycloakBackedApp :: TestEnv -> KeycloakHarness -> IO Application
mkKeycloakBackedApp env kc = do
  let baseCfg = mkDeployedSandboxConfig env
      cfg = baseCfg
        { Config.auth = baseCfg.auth
            { Config.deployed = baseCfg.auth.deployed
                { Config.issuer = Just kc.keycloakIssuer
                , Config.audience = Just kc.keycloakClientId
                , Config.jwks = Just kc.keycloakJwks
                , Config.clientId = Just kc.keycloakClientId
                , Config.authorizationEndpoint = Just kc.keycloakAuthorizationEndpoint
                , Config.tokenEndpoint = Just kc.keycloakTokenEndpoint
                }
            }
        }
  tracker <- newAccessTracker env.pool 3600
  wsState <- newWSState
  mkApp id cfg.auth cfg.cors cfg.rateLimit env.pool tracker wsState cfg.web.webStaticDir True

keycloakEnabled :: IO Bool
keycloakEnabled = do
  value <- lookupEnv keycloakEnvFlag
  pure $ value `elem` [Just "1", Just "true", Just "TRUE", Just "yes", Just "YES"]

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

isPrefixOfPath :: FilePath -> FilePath -> Bool
isPrefixOfPath root path = root `isPrefixOf` path
