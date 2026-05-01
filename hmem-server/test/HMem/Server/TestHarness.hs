module HMem.Server.TestHarness
  ( LocalSandboxApp(..)
  , DeployedSandboxApp(..)
  , localSandboxBotLabel
  , localSandboxBotToken
  , deployedSandboxIssuer
  , deployedSandboxAudience
  , deployedSandboxJwtSecret
  , deployedSandboxTokenHashSecret
  , mkLocalSandboxConfig
  , mkDeployedSandboxConfig
  , mkDeployedSandboxCrossOriginConfig
  , withLocalSandboxApp
  , withLocalSandboxAppEnv
  , withLocalSandboxAppContext
  , withDeployedSandboxApp
  , withDeployedSandboxAppEnv
  , withDeployedSandboxAppContext
  , withDeployedSandboxAppConfig
  , withDeployedSandboxCrossOriginAppContext
  , createDeployedSandboxUser
  , createDeployedSandboxUserWithSubject
  , createDeployedSandboxAuthSession
  , issueDeployedSandboxPAT
  , signDeployedSandboxJwt
  ) where

import Control.Exception (bracket)
import Control.Lens ((&), (?~))
import Data.Aeson (toJSON)
import Data.ByteString.Lazy qualified as LBS
import Data.Functor.Contravariant (contramap)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Time (addUTCTime, getCurrentTime)
import Data.UUID (UUID)
import Hasql.Decoders qualified as Dec
import Hasql.Encoders qualified as Enc
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement
import Network.Wai (Application)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import Data.String (fromString)

import Crypto.JOSE.JWK qualified as JWK
import Crypto.JOSE.JWS qualified as JWS
import Crypto.JWT qualified as JWT
import HMem.Config qualified as Config
import HMem.DB.Auth qualified as Auth
import HMem.DB.Pool qualified as DBPool
import HMem.DB.TestHarness (TestDb(..), TestEnv(..), TestSandbox(..), withTestEnv)
import HMem.Server.AccessTracker (newAccessTracker)
import HMem.Server.App (mkApp)
import HMem.Server.AuthTokens qualified as AuthTokens
import HMem.Server.WebSocket (WSState, newWSState)

data LocalSandboxApp = LocalSandboxApp
  { localEnv :: !TestEnv
  , localConfig :: !Config.HMemConfig
  , localApplication :: !Application
  , localWSState :: !WSState
  }

data DeployedSandboxApp = DeployedSandboxApp
  { deployedEnv :: !TestEnv
  , deployedConfig :: !Config.HMemConfig
  , deployedApplication :: !Application
  , deployedWSState :: !WSState
  }

localSandboxBotLabel :: Text
localSandboxBotLabel = "Sandbox Bot"

localSandboxBotToken :: Text
localSandboxBotToken = "sandbox-local-bot-token"

deployedSandboxIssuer :: Text
deployedSandboxIssuer = "https://sandbox-issuer.example"

deployedSandboxAudience :: Text
deployedSandboxAudience = "hmem-sandbox-web"

deployedSandboxJwtSecret :: Text
deployedSandboxJwtSecret = "0123456789abcdef0123456789abcdef"

deployedSandboxTokenHashSecret :: Text
deployedSandboxTokenHashSecret = "sandbox-token-hash-secret"

mkLocalSandboxConfig :: TestEnv -> Config.HMemConfig
mkLocalSandboxConfig env = Config.defaultConfig
  { Config.server = Config.ServerConfig
      { Config.host = "127.0.0.1"
      , Config.port = 0
      }
  , Config.database = Config.DatabaseConfig
      { Config.host = "127.0.0.1"
      , Config.port = env.testDb.testDbPort
      , Config.name = env.testDb.testDbName
      , Config.user = Nothing
      , Config.password = Nothing
      , Config.sslmode = Nothing
      }
  , Config.cors = Config.CorsConfig
      { Config.allowedOrigins = ["http://127.0.0.1", "http://localhost"]
      }
  , Config.auth = Config.defaultConfig.auth
      { Config.mode = Config.AuthModeLocal
      , Config.enabled = False
      , Config.apiKey = Nothing
      , Config.local = Config.LocalAuthConfig
          { Config.bootstrapEnabled = True
          , Config.allowRemoteBootstrap = False
          , Config.botTokens =
              [ Config.LocalBotTokenConfig
                  { Config.label = localSandboxBotLabel
                  , Config.token = localSandboxBotToken
                  }
              ]
          }
      }
  , Config.web = Config.defaultConfig.web
      { Config.webStaticDir = Just env.testSandbox.sandboxStaticDir
      }
  }

mkDeployedSandboxConfig :: TestEnv -> Config.HMemConfig
mkDeployedSandboxConfig env = Config.defaultConfig
  { Config.server = Config.ServerConfig
      { Config.host = "127.0.0.1"
      , Config.port = 0
      }
  , Config.database = Config.DatabaseConfig
      { Config.host = "127.0.0.1"
      , Config.port = env.testDb.testDbPort
      , Config.name = env.testDb.testDbName
      , Config.user = Nothing
      , Config.password = Nothing
      , Config.sslmode = Nothing
      }
  , Config.cors = Config.CorsConfig
      { Config.allowedOrigins = ["http://127.0.0.1", "http://localhost"]
      }
  , Config.auth = Config.defaultConfig.auth
      { Config.mode = Config.AuthModeDeployed
      , Config.enabled = False
      , Config.apiKey = Nothing
      , Config.local = Config.LocalAuthConfig
          { Config.bootstrapEnabled = False
          , Config.allowRemoteBootstrap = False
          , Config.botTokens = []
          }
      , Config.deployed = Config.defaultConfig.auth.deployed
          { Config.issuer = Just deployedSandboxIssuer
          , Config.audience = Just deployedSandboxAudience
          , Config.jwks = Just (toJSON deployedSandboxJwkSet)
          , Config.tokenLookup = Config.TokenLookupDatabase
          , Config.tokenHashSecret = Just deployedSandboxTokenHashSecret
          , Config.clientId = Just deployedSandboxAudience
          , Config.clientSecret = Just "sandbox-client-secret"
          , Config.redirectUri = Just "http://127.0.0.1:0/api/v1/auth/oidc/callback"
          , Config.authorizationEndpoint = Just "https://sandbox-issuer.example/authorize"
          , Config.tokenEndpoint = Just "https://sandbox-issuer.example/token"
          , Config.sessionCookieName = "hmem_session"
          , Config.csrfCookieName = "hmem_csrf"
          , Config.csrfHeaderName = "X-CSRF-Token"
          , Config.sessionTtlSeconds = 3600
          , Config.cookieSecure = True
          , Config.cookieSameSite = "Lax"
          }
      }
  , Config.web = Config.defaultConfig.web
      { Config.webStaticDir = Just env.testSandbox.sandboxStaticDir
      }
  }

mkDeployedSandboxCrossOriginConfig :: TestEnv -> [Text] -> Config.HMemConfig
mkDeployedSandboxCrossOriginConfig env origins =
  let cfg = mkDeployedSandboxConfig env
  in cfg
      { Config.cors = Config.CorsConfig { Config.allowedOrigins = origins }
      , Config.auth = cfg.auth
          { Config.deployed = cfg.auth.deployed
              { Config.cookieSameSite = "None"
              , Config.cookieSecure = True
              }
          }
      }

withLocalSandboxApp :: (Application -> IO a) -> IO a
withLocalSandboxApp action =
  withLocalSandboxAppContext (action . (.localApplication))

withLocalSandboxAppEnv :: (TestEnv -> Application -> IO a) -> IO a
withLocalSandboxAppEnv action =
  withLocalSandboxAppContext $ \ctx -> action ctx.localEnv ctx.localApplication

withLocalSandboxAppContext :: (LocalSandboxApp -> IO a) -> IO a
withLocalSandboxAppContext action = withClearedAmbientLocalProfileEnv $
  withTestEnv $ \env -> do
    tracker <- newAccessTracker env.pool 3600
    wsState <- newWSState
    let cfg = mkLocalSandboxConfig env
    if env.testDb.testDbUnsafeExternal
      then fail "Local sandbox profile refuses unsafe external DB mode"
      else pure ()
    app <- mkApp id cfg.auth cfg.cors cfg.rateLimit env.pool tracker wsState cfg.web.webStaticDir True
    action LocalSandboxApp
      { localEnv = env
      , localConfig = cfg
      , localApplication = app
      , localWSState = wsState
      }

withDeployedSandboxApp :: (Application -> IO a) -> IO a
withDeployedSandboxApp action =
  withDeployedSandboxAppContext (action . (.deployedApplication))

withDeployedSandboxAppEnv :: (TestEnv -> Application -> IO a) -> IO a
withDeployedSandboxAppEnv action =
  withDeployedSandboxAppContext $ \ctx -> action ctx.deployedEnv ctx.deployedApplication

withDeployedSandboxAppContext :: (DeployedSandboxApp -> IO a) -> IO a
withDeployedSandboxAppContext = withDeployedSandboxAppConfig mkDeployedSandboxConfig

withDeployedSandboxCrossOriginAppContext :: [Text] -> (DeployedSandboxApp -> IO a) -> IO a
withDeployedSandboxCrossOriginAppContext origins =
  withDeployedSandboxAppConfig (`mkDeployedSandboxCrossOriginConfig` origins)

withDeployedSandboxAppConfig :: (TestEnv -> Config.HMemConfig) -> (DeployedSandboxApp -> IO a) -> IO a
withDeployedSandboxAppConfig mkCfg action = withClearedAmbientSandboxProfileEnv $
  withTestEnv $ \env -> do
    tracker <- newAccessTracker env.pool 3600
    wsState <- newWSState
    let cfg = mkCfg env
    if env.testDb.testDbUnsafeExternal
      then fail "Deployed sandbox profile refuses unsafe external DB mode"
      else pure ()
    app <- mkApp id cfg.auth cfg.cors cfg.rateLimit env.pool tracker wsState cfg.web.webStaticDir True
    action DeployedSandboxApp
      { deployedEnv = env
      , deployedConfig = cfg
      , deployedApplication = app
      , deployedWSState = wsState
      }

createDeployedSandboxUser :: TestEnv -> Bool -> Bool -> IO UUID
createDeployedSandboxUser env = createDeployedSandboxUserWithMaybeSubject env Nothing

createDeployedSandboxUserWithSubject :: TestEnv -> Text -> Bool -> Bool -> IO UUID
createDeployedSandboxUserWithSubject env subject = createDeployedSandboxUserWithMaybeSubject env (Just subject)

createDeployedSandboxAuthSession :: TestEnv -> UUID -> Text -> Text -> IO UUID
createDeployedSandboxAuthSession env userId sessionToken csrfToken = do
  now <- getCurrentTime
  Auth.createAuthSession env.pool userId sessionToken csrfToken (addUTCTime 3600 now)

issueDeployedSandboxPAT :: TestEnv -> UUID -> Text -> IO AuthTokens.IssuedAccessToken
issueDeployedSandboxPAT env grantUserId actorLabel = do
  result <- AuthTokens.issueAccessTokenWithSecret env.pool (Just deployedSandboxTokenHashSecret) AuthTokens.IssueAccessTokenInput
    { AuthTokens.grantUserId = grantUserId
    , AuthTokens.actorType = AuthTokens.AccessTokenActorBot
    , AuthTokens.actorLabel = actorLabel
    , AuthTokens.expiresAt = Nothing
    }
  case result of
    Right issued -> pure issued
    Left err -> fail $ "Failed to issue deployed sandbox PAT: " <> show err

signDeployedSandboxJwt :: Text -> IO Text
signDeployedSandboxJwt subject = do
  let jwk = deployedSandboxJwk
      asStringOrUri = fromString . T.unpack
      claims = JWT.emptyClaimsSet
        & JWT.claimIss ?~ asStringOrUri deployedSandboxIssuer
        & JWT.claimAud ?~ JWT.Audience [asStringOrUri deployedSandboxAudience]
        & JWT.claimSub ?~ asStringOrUri subject
  signedResult <- (JWT.runJOSE $ do
    alg <- JWK.bestJWSAlg jwk
    JWT.signClaims jwk (JWS.newJWSHeader ((), alg)) claims) :: IO (Either JWT.JWTError JWT.SignedJWT)
  case signedResult of
    Left err -> fail $ "Failed to sign deployed sandbox JWT: " <> show err
    Right signed -> pure . decodeUtf8 . LBS.toStrict $ JWT.encodeCompact signed

withClearedAmbientLocalProfileEnv :: IO a -> IO a
withClearedAmbientLocalProfileEnv = withClearedAmbientSandboxProfileEnv

withClearedAmbientSandboxProfileEnv :: IO a -> IO a
withClearedAmbientSandboxProfileEnv = bracket snapshot restore . const
  where
    snapshot = do
      values <- mapM (\name -> do value <- lookupEnv name; pure (name, value)) ambientSandboxProfileVars
      mapM_ unsetEnv ambientSandboxProfileVars
      pure values

    restore values =
      mapM_ restoreOne values

    restoreOne (name, Nothing) = unsetEnv name
    restoreOne (name, Just value) = setEnv name value

ambientSandboxProfileVars :: [String]
ambientSandboxProfileVars =
  [ "HMEM_TEST_DB"
  , "HMEM_TEST_EXTERNAL_DB"
  , "HMEM_TEST_ALLOW_EXTERNAL_DB"
  , "HMEM_DB_PASSWORD"
  , "HMEM_DB_SSLMODE"
  , "HMEM_API_KEY"
  , "HMEM_AUTH_TOKEN"
  , "HMEM_MCP_AUTH_TOKEN"
  , "HMEM_SERVER_URL"
  , "KEYCLOAK_URL"
  , "KEYCLOAK_REALM"
  , "KEYCLOAK_CLIENT_SECRET"
  ]

deployedSandboxJwk :: JWK.JWK
deployedSandboxJwk = JWK.fromOctets (encodeUtf8 deployedSandboxJwtSecret)

deployedSandboxJwkSet :: JWK.JWKSet
deployedSandboxJwkSet = JWK.JWKSet [deployedSandboxJwk]

createDeployedSandboxUserWithMaybeSubject :: TestEnv -> Maybe Text -> Bool -> Bool -> IO UUID
createDeployedSandboxUserWithMaybeSubject env mSubject canCreateWorkspace isSuperadmin =
  DBPool.runSession env.pool $ Session.statement
    (mSubject, canCreateWorkspace, isSuperadmin)
    createDeployedSandboxUserStatement

createDeployedSandboxUserStatement :: Statement.Statement (Maybe Text, Bool, Bool) UUID
createDeployedSandboxUserStatement = Statement.Statement sql encoder decoder True
  where
    sql = "INSERT INTO users (auth_subject, display_name, can_create_workspace, is_superadmin) \
          \VALUES ($1, $1, $2, $3) RETURNING id"
    encoder =
      contramap (\(subject,_,_) -> subject) (Enc.param (Enc.nullable Enc.text)) <>
      contramap (\(_,canCreate,_) -> canCreate) (Enc.param (Enc.nonNullable Enc.bool)) <>
      contramap (\(_,_,superadmin) -> superadmin) (Enc.param (Enc.nonNullable Enc.bool))
    decoder = Dec.singleRow (Dec.column (Dec.nonNullable Dec.uuid))
