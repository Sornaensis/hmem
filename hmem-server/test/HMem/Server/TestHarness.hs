module HMem.Server.TestHarness
  ( LocalSandboxApp(..)
  , localSandboxBotLabel
  , localSandboxBotToken
  , mkLocalSandboxConfig
  , withLocalSandboxApp
  , withLocalSandboxAppEnv
  , withLocalSandboxAppContext
  ) where

import Control.Exception (bracket)
import Network.Wai (Application)
import Data.Text (Text)
import System.Environment (lookupEnv, setEnv, unsetEnv)

import HMem.Config qualified as Config
import HMem.DB.TestHarness (TestDb(..), TestEnv(..), TestSandbox(..), withTestEnv)
import HMem.Server.AccessTracker (newAccessTracker)
import HMem.Server.App (mkApp)
import HMem.Server.WebSocket (WSState, newWSState)

data LocalSandboxApp = LocalSandboxApp
  { localEnv :: !TestEnv
  , localConfig :: !Config.HMemConfig
  , localApplication :: !Application
  , localWSState :: !WSState
  }

localSandboxBotLabel :: Text
localSandboxBotLabel = "Sandbox Bot"

localSandboxBotToken :: Text
localSandboxBotToken = "sandbox-local-bot-token"

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

withClearedAmbientLocalProfileEnv :: IO a -> IO a
withClearedAmbientLocalProfileEnv = bracket snapshot restore . const
  where
    snapshot = do
      values <- mapM (\name -> do value <- lookupEnv name; pure (name, value)) ambientLocalProfileVars
      mapM_ unsetEnv ambientLocalProfileVars
      pure values

    restore values =
      mapM_ restoreOne values

    restoreOne (name, Nothing) = unsetEnv name
    restoreOne (name, Just value) = setEnv name value

ambientLocalProfileVars :: [String]
ambientLocalProfileVars =
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
