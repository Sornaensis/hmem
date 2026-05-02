module Main where

import Control.Concurrent (ThreadId, forkIO, killThread, threadDelay)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, tryPutMVar, tryTakeMVar)
import Control.Exception (Exception, SomeException, finally, throwIO, try, catch)
import Control.Monad (unless, when)
import Data.Aeson (encode, toJSON)
import Data.ByteString.Lazy qualified as LBS
import Data.Pool (destroyAllResources)
import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time (addUTCTime, getCurrentTime)
import Network.Wai.Handler.Warp
  ( defaultSettings
  , runSettings
  , setBeforeMainLoop
  , setGracefulShutdownTimeout
  , setHost
  , setPort
  , setTimeout
  )
import Options.Applicative
import System.Environment (setEnv)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.IO (BufferMode(..), hFlush, hPutStrLn, hSetBuffering, stderr, stdout)
import System.Log.FastLogger (FileLogSpec(..), LogType'(LogFile), defaultBufSize, newFastLogger)
import System.Random (randomRIO)

import Crypto.JOSE.JWK qualified as JWK
import HMem.Config qualified as Config
import HMem.DB.Auth qualified as Auth
import HMem.DB.Pool qualified as DBPool
import HMem.DB.TestHarness (TestDb(..), TestEnv(..), TestSandbox(..), createTestWorkspace, withSandboxedTestEnv)
import HMem.Server.AccessTracker (flushNow, newAccessTracker)
import HMem.Server.App (mkApp)
import HMem.Server.AuthBootstrap qualified as AuthBootstrap
import HMem.Server.AuthTokens qualified as AuthTokens
import HMem.Server.Logging (jsonRequestLogger, logInfo, logWarn, newLogger, parseLogLevel)
import HMem.Server.WebSocket (newWSState)

data HarnessMode
  = LocalMode
  | DeployedMode
  | KeycloakMode
  deriving stock (Show, Eq)

data Opts = Opts
  { optMode :: !HarnessMode
  , optInteractive :: !Bool
  , optPreserve :: !Bool
  , optPort :: !(Maybe Int)
  }

data PreserveSandboxExit = PreserveSandboxExit
  deriving stock (Show)

instance Exception PreserveSandboxExit

localBotLabel :: Text
localBotLabel = "Sandbox Bot"

localBotToken :: Text
localBotToken = "sandbox-local-bot-token"

deployedIssuer :: Text
deployedIssuer = "https://sandbox-issuer.example"

deployedAudience :: Text
deployedAudience = "hmem-sandbox-web"

deployedTokenHashSecret :: Text
deployedTokenHashSecret = "sandbox-token-hash-secret"

deployedJwtSecret :: Text
deployedJwtSecret = "0123456789abcdef0123456789abcdef"

deployedSessionToken :: Text
deployedSessionToken = "sandbox-session-token"

deployedCsrfToken :: Text
deployedCsrfToken = "sandbox-csrf-token"

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  opts <- execParser $ info (optsParser <**> helper)
    ( fullDesc
   <> progDesc "Start an isolated hmem sandbox server for manual testing"
   <> header "hmem-test-harness"
    )

  unless opts.optInteractive $ do
    hPutStrLn stderr "hmem-test-harness is interactive-only for now; pass --interactive."
    exitFailure

  case opts.optMode of
    KeycloakMode -> do
      hPutStrLn stderr "Keycloak sandbox mode is planned but not implemented yet. Use local or deployed."
      exitFailure
    _ -> pure ()

  when opts.optPreserve $
    setEnv "HMEM_TEST_PRESERVE_SANDBOX" "1"

  runHarness opts `catch` \PreserveSandboxExit -> do
    putStrLn ""
    putStrLn "[sandbox] Preserved sandbox root because --preserve was set."

optsParser :: Parser Opts
optsParser = Opts
  <$> argument modeReader
      ( metavar "MODE"
     <> help "Sandbox mode: local, deployed, or keycloak (future)"
      )
  <*> switch
      ( long "interactive"
     <> help "Run until Enter/Ctrl-C, then tear down the sandbox"
      )
  <*> switch
      ( long "preserve"
     <> help "Keep the sandbox root after exit for debugging"
      )
  <*> optional (option auto
      ( long "port"
     <> short 'p'
     <> metavar "PORT"
     <> help "Loopback HTTP port (default: random ephemeral port)"
      ))

modeReader :: ReadM HarnessMode
modeReader = eitherReader $ \case
  "local" -> Right LocalMode
  "deployed" -> Right DeployedMode
  "keycloak" -> Right KeycloakMode
  other -> Left $ "unknown mode " <> show other <> "; expected local, deployed, or keycloak"

runHarness :: Opts -> IO ()
runHarness opts = withSandboxedTestEnv $ \env -> do
  let TestEnv { testSandbox = sandbox } = env
      TestSandbox { sandboxConfigDir = configDir, sandboxLogDir = logDir } = sandbox
  port <- maybe (randomRIO (49152, 65535)) pure opts.optPort
  let cfg = case opts.optMode of
        LocalMode -> mkLocalConfig env port
        DeployedMode -> mkDeployedConfig env port
        KeycloakMode -> error "Keycloak mode is rejected before runHarness"

  case Config.localImplicitBootstrapStartupError cfg of
    Just err -> fail $ "Unsafe local auth config refused: " <> T.unpack err
    Nothing -> pure ()

  seed <- case opts.optMode of
    LocalMode -> seedLocal env
    DeployedMode -> seedDeployed env
    KeycloakMode -> error "Keycloak mode is rejected before runHarness"

  let configPath = configDir </> "hmem-test-harness-config.json"
  LBS.writeFile configPath (encode cfg)

  tracker <- newAccessTracker env.pool 5
  wsState <- newWSState
  let logPath = logDir </> "hmem-test-harness.log"
      fileSpec = FileLogSpec
        { log_file = logPath
        , log_file_size = 10 * 1024 * 1024
        , log_backup_number = 3
        }
  (logAction, cleanupLog) <- newFastLogger (LogFile fileSpec defaultBufSize)
  let logger = newLogger logAction (parseLogLevel "info")
  requestLogger <- jsonRequestLogger logAction

  pgvec <- DBPool.checkPgvector env.pool
  app <- mkApp requestLogger cfg.auth cfg.cors cfg.rateLimit env.pool tracker wsState cfg.web.webStaticDir pgvec

  ready <- newEmptyMVar
  serverDone <- newEmptyMVar
  inputDone <- newEmptyMVar
  let settings = setBeforeMainLoop (putMVar ready ())
               $ setHost (fromString "127.0.0.1")
               $ setPort port
               $ setTimeout 60
               $ setGracefulShutdownTimeout (Just 5)
               $ defaultSettings
      serverUrl = "http://127.0.0.1:" <> T.pack (show port)
      shutdown = do
        logInfo logger "[sandbox] Shutting down interactive harness..."
        flushNow env.pool tracker `catch` \(_ :: SomeException) ->
          logWarn logger "failed to flush access tracker"
        destroyAllResources env.pool
        cleanupLog

  serverThread <- forkIO $
    try (runSettings settings app) >>= putMVar serverDone
  inputThread <- forkIO $ do
    inputResult <- try getLine
    case inputResult of
      Right _ -> do
        _ <- tryPutMVar inputDone ()
        pure ()
      Left (_ :: SomeException) -> do
        -- Non-interactive runners used by smoke tests may not provide a valid
        -- stdin handle.  Treat that the same as an immediate Enter press so the
        -- sandbox still starts, prints its coordinates, and tears down cleanly.
        _ <- tryPutMVar inputDone ()
        pure ()

  let stopThreads = do
        killThread inputThread
        killThread serverThread
      runInteractive = do
        waitUntilReady ready serverDone
        printBanner opts env cfg serverUrl logPath configPath seed
        waitForStop inputDone serverDone serverThread

  (runInteractive `finally` stopThreads) `finally` shutdown
  when opts.optPreserve $
    throwIO PreserveSandboxExit

data SeedInfo = SeedInfo
  { seedWorkspaceName :: !Text
  , seedCredentialLines :: ![String]
  , seedCurlAuthHeader :: !(Maybe String)
  }

seedLocal :: TestEnv -> IO SeedInfo
seedLocal env = do
  _ <- createTestWorkspace env "Interactive Sandbox"
  pure SeedInfo
    { seedWorkspaceName = "Interactive Sandbox"
    , seedCredentialLines =
        [ "Auth mode: local implicit superadmin for loopback requests"
        , "Local bot bearer token: " <> T.unpack localBotToken
        ]
    , seedCurlAuthHeader = Just ("-H \"Authorization: Bearer " <> T.unpack localBotToken <> "\"")
    }

seedDeployed :: TestEnv -> IO SeedInfo
seedDeployed env = do
  _ <- createTestWorkspace env "Interactive Sandbox"
  boot <- AuthBootstrap.bootstrapSuperadmin env.pool AuthBootstrap.BootstrapSuperadminInput
    { AuthBootstrap.authSubject = "sandbox-superadmin"
    , AuthBootstrap.email = Just "sandbox-superadmin@example.invalid"
    , AuthBootstrap.displayName = Just "Sandbox Superadmin"
    , AuthBootstrap.force = False
    }
  userId <- case boot of
    Left err -> fail $ "failed to create deployed sandbox superadmin: " <> show err
    Right result -> pure result.userId
  issued <- AuthTokens.issueAccessTokenWithSecret env.pool (Just deployedTokenHashSecret) AuthTokens.IssueAccessTokenInput
    { AuthTokens.grantUserId = userId
    , AuthTokens.actorType = AuthTokens.AccessTokenActorBot
    , AuthTokens.actorLabel = "Sandbox PAT"
    , AuthTokens.expiresAt = Nothing
    }
  rawToken <- case issued of
    Left err -> fail $ "failed to issue deployed sandbox PAT: " <> show err
    Right token -> pure token.rawToken
  now <- getCurrentTime
  _ <- Auth.createAuthSession env.pool userId deployedSessionToken deployedCsrfToken (addUTCTime 3600 now)
  pure SeedInfo
    { seedWorkspaceName = "Interactive Sandbox"
    , seedCredentialLines =
        [ "Auth mode: deployed sandbox"
        , "Superadmin auth subject: sandbox-superadmin"
        , "Bearer PAT: " <> T.unpack rawToken
        , "Session cookie: hmem_session=" <> T.unpack deployedSessionToken
        , "CSRF cookie/header token: " <> T.unpack deployedCsrfToken
        ]
    , seedCurlAuthHeader = Just ("-H \"Authorization: Bearer " <> T.unpack rawToken <> "\"")
    }

mkLocalConfig :: TestEnv -> Int -> Config.HMemConfig
mkLocalConfig env port =
  let TestEnv { testSandbox = sandbox } = env
      TestSandbox { sandboxStaticDir = staticDir } = sandbox
  in Config.defaultConfig
  { Config.server = Config.ServerConfig
      { Config.host = "127.0.0.1"
      , Config.port = port
      }
  , Config.database = sandboxDatabaseConfig env
  , Config.cors = loopbackCors port
  , Config.auth = Config.defaultConfig.auth
      { Config.mode = Config.AuthModeLocal
      , Config.enabled = False
      , Config.apiKey = Nothing
      , Config.local = Config.LocalAuthConfig
          { Config.bootstrapEnabled = True
          , Config.allowRemoteBootstrap = False
          , Config.botTokens =
              [ Config.LocalBotTokenConfig
                  { Config.label = localBotLabel
                  , Config.token = localBotToken
                  }
              ]
          }
      }
  , Config.rateLimit = Config.RateLimitConfig False 100 200
  , Config.web = Config.defaultConfig.web
      { Config.webStaticDir = Just staticDir
      }
  }

mkDeployedConfig :: TestEnv -> Int -> Config.HMemConfig
mkDeployedConfig env port =
  let TestEnv { testSandbox = sandbox } = env
      TestSandbox { sandboxStaticDir = staticDir } = sandbox
  in Config.defaultConfig
  { Config.server = Config.ServerConfig
      { Config.host = "127.0.0.1"
      , Config.port = port
      }
  , Config.database = sandboxDatabaseConfig env
  , Config.cors = loopbackCors port
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
          { Config.issuer = Just deployedIssuer
          , Config.audience = Just deployedAudience
          , Config.jwks = Just (toJSON (JWK.JWKSet [JWK.fromOctets (encodeUtf8 deployedJwtSecret)]))
          , Config.tokenHashSecret = Just deployedTokenHashSecret
          , Config.clientId = Just deployedAudience
          , Config.clientSecret = Just "sandbox-client-secret"
          , Config.redirectUri = Just ("http://127.0.0.1:" <> T.pack (show port) <> "/api/v1/auth/oidc/callback")
          , Config.authorizationEndpoint = Just "https://sandbox-issuer.example/authorize"
          , Config.tokenEndpoint = Just "https://sandbox-issuer.example/token"
          , Config.cookieSecure = False
          , Config.cookieSameSite = "Lax"
          , Config.sessionTtlSeconds = 3600
          }
      }
  , Config.rateLimit = Config.RateLimitConfig False 100 200
  , Config.web = Config.defaultConfig.web
      { Config.webStaticDir = Just staticDir
      }
  }

sandboxDatabaseConfig :: TestEnv -> Config.DatabaseConfig
sandboxDatabaseConfig TestEnv { testDb = db } =
  let TestDb { testDbPort = dbPort, testDbName = dbName } = db
  in Config.DatabaseConfig
  { Config.host = "127.0.0.1"
  , Config.port = dbPort
  , Config.name = dbName
  , Config.user = Nothing
  , Config.password = Nothing
  , Config.sslmode = Nothing
  }

loopbackCors :: Int -> Config.CorsConfig
loopbackCors port = Config.CorsConfig
  { Config.allowedOrigins =
      [ "http://127.0.0.1"
      , "http://127.0.0.1:" <> T.pack (show port)
      , "http://localhost"
      , "http://localhost:" <> T.pack (show port)
      , "http://localhost:5173"
      , "http://127.0.0.1:5173"
      ]
  }

waitUntilReady :: MVar () -> MVar (Either SomeException ()) -> IO ()
waitUntilReady ready serverDone = do
  mReady <- tryTakeMVar ready
  case mReady of
    Just () -> pure ()
    Nothing -> do
      mDone <- tryTakeMVar serverDone
      case mDone of
        Just (Left err) -> throwIO err
        Just (Right ()) -> fail "hmem-test-harness server stopped before it became ready"
        Nothing -> threadDelay 100000 >> waitUntilReady ready serverDone

waitForStop :: MVar () -> MVar (Either SomeException ()) -> ThreadId -> IO ()
waitForStop inputDone serverDone serverThread = do
  mDone <- tryTakeMVar serverDone
  case mDone of
    Just (Left err) -> throwIO err
    Just (Right ()) -> fail "hmem-test-harness server stopped unexpectedly"
    Nothing -> do
      mInput <- tryTakeMVar inputDone
      case mInput of
        Just () -> killThread serverThread
        Nothing -> threadDelay 100000 >> waitForStop inputDone serverDone serverThread

printBanner :: Opts -> TestEnv -> Config.HMemConfig -> Text -> FilePath -> FilePath -> SeedInfo -> IO ()
printBanner opts env cfg serverUrl logPath configPath seed = do
  let TestEnv { testSandbox = sandbox, testDb = db } = env
      TestSandbox
        { sandboxRoot = root
        , sandboxConfigDir = configDir
        , sandboxStaticDir = staticDir
        } = sandbox
      TestDb
        { testDbLogFile = dbLogFile
        , testDbName = dbName
        , testDbPort = dbPort
        } = db
  putStrLn ""
  putStrLn "=== hmem interactive sandbox ==="
  putStrLn $ "Mode        : " <> modeLabel opts.optMode
  putStrLn $ "Server URL  : " <> T.unpack serverUrl
  putStrLn $ "Sandbox root: " <> root
  putStrLn $ "Config dir  : " <> configDir
  putStrLn $ "Config file : " <> configPath
  putStrLn $ "Static dir  : " <> staticDir
  putStrLn $ "Server log  : " <> logPath
  putStrLn $ "Postgres log: " <> dbLogFile
  putStrLn $ "Postgres DB : " <> T.unpack dbName <> " on port " <> show dbPort
  putStrLn $ "CORS origins: " <> show cfg.cors.allowedOrigins
  putStrLn $ "Seeded workspace: " <> T.unpack seed.seedWorkspaceName
  putStrLn ""
  mapM_ putStrLn seed.seedCredentialLines
  case seed.seedCurlAuthHeader of
    Just authHeader -> putStrLn $ "Example: curl " <> authHeader <> " " <> T.unpack serverUrl <> "/api/v1/session"
    Nothing -> pure ()
  putStrLn ""
  putStrLn "This runner used sandboxed PostgreSQL and scrubbed hmem-related ambient env vars."
  putStrLn $ if opts.optPreserve
    then "Press Enter or Ctrl-C to stop. Sandbox files will be preserved (--preserve)."
    else "Press Enter or Ctrl-C to stop and delete the sandbox."
  hFlush stdout

modeLabel :: HarnessMode -> String
modeLabel = \case
  LocalMode -> "local"
  DeployedMode -> "deployed"
  KeycloakMode -> "keycloak"
