{-# LANGUAGE ForeignFunctionInterface #-}
module Main where

import Control.Concurrent (ThreadId, forkIO, forkIOWithUnmask, killThread, threadDelay)
import Control.Concurrent.MVar (MVar, newEmptyMVar, newMVar, modifyMVar, modifyMVar_, readMVar, tryReadMVar, takeMVar, putMVar, tryPutMVar, tryTakeMVar)
import Control.Exception (Exception, SomeException, SomeAsyncException, IOException, bracket, finally, throwIO, try, catch, mask, onException, fromException, toException)
import Control.Monad (unless, when, forever, void)
import Data.Aeson (Value(..), FromJSON, encode, toJSON, object, (.=), eitherDecodeStrict', fromJSON)
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.IORef
import Data.Char (isAlphaNum)
import Data.List (isPrefixOf)
import Data.Pool (Pool, destroyAllResources)
import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time (addUTCTime, getCurrentTime)
import Data.Word (Word64)
import Foreign (Ptr, alloca, peek)
import Foreign.C.Types (CInt(..))
import GHC.Clock (getMonotonicTimeNSec)
import Hasql.Connection qualified as H
import Hasql.Decoders qualified as D
import Hasql.Encoders qualified as E
import Hasql.Session qualified as S
import Hasql.Statement qualified as St
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
import System.Environment (setEnv, lookupEnv, getArgs)
import System.Exit (exitFailure)
import System.Directory (canonicalizePath, doesDirectoryExist, doesFileExist, pathIsSymbolicLink, listDirectory)
import System.FilePath ((</>), equalFilePath, addTrailingPathSeparator, takeDirectory, takeFileName, normalise)
import System.IO (BufferMode(..), Handle, IOMode(ReadMode), SeekMode(AbsoluteSeek), openBinaryFile, hClose, hIsClosed, hIsSeekable, hFileSize, hSeek, hFlush, hPutStrLn, hSetBuffering, stderr, stdin, stdout)
import System.Log.FastLogger (FileLogSpec(..), LogType'(LogFile), defaultBufSize, newFastLogger)
import System.Random (randomRIO)
import System.Timeout (timeout)

import Crypto.JOSE.JWK qualified as JWK
import HMem.Config qualified as Config
import HMem.DB.Auth qualified as Auth
import HMem.DB.Pool qualified as DBPool
import HMem.DB.SessionArchive.Maintenance qualified as ArchiveMaintenance
import HMem.DB.TestHarness (TestDb(..), TestEnv(..), TestSandbox(..), SandboxLifetime, createTestWorkspace, newSandboxLifetime, newSandboxLifetimeWithReleaseHooks, withSandboxedTestEnvBorrowed, retrySandboxLifetime)
import HMem.Server.AccessTracker (flushNow, newAccessTracker)
import HMem.Server.App (mkAppWithPrefixProofs)
import HMem.Server.SessionPrefix (newPrefixRegistry, closePrefixRegistry)
import HMem.Server.AuthBootstrap qualified as AuthBootstrap
import HMem.Server.AuthTokens qualified as AuthTokens
import HMem.Server.ChangeStream (startChangeStreamWorker, stopChangeStreamWorker)
import HMem.Server.SessionReaper (startSessionReaper, stopSessionReaper)
import HMem.Server.Archive.Worker (ArchiveWorker, startArchiveWorker, stopArchiveWorker)
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
  , optArchiveQualificationRoot :: !(Maybe FilePath)
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
  arguments <- getArgs
  let qualificationRequested = "--archive-qualification-root" `elem` arguments
  when qualificationRequested (qualificationPhase "main-entry")
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  when qualificationRequested (qualificationPhase "stdio-configured")
  opts <- execParser $ info (optsParser <**> helper)
    ( fullDesc
   <> progDesc "Start an isolated hmem sandbox server for manual testing"
   <> header "hmem-test-harness"
    )
  when qualificationRequested (qualificationPhase "arguments-parsed")

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

  when (opts.optPreserve && opts.optArchiveQualificationRoot /= Nothing) $
    fail "Archive qualification refuses --preserve"

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
  <*> optional (strOption
      ( long "archive-qualification-root"
     <> metavar "DIRECTORY"
     <> help "Opt-in fixed Task8 scratch root and private stdin qualification controls (Windows only)"
      ))

modeReader :: ReadM HarnessMode
modeReader = eitherReader $ \case
  "local" -> Right LocalMode
  "deployed" -> Right DeployedMode
  "keycloak" -> Right KeycloakMode
  other -> Left $ "unknown mode " <> show other <> "; expected local, deployed, or keycloak"

runHarness :: Opts -> IO ()
runHarness opts = do
  lifetime <- newSandboxLifetime
  case opts.optArchiveQualificationRoot of
    Nothing -> withSandboxedTestEnvBorrowed lifetime (startArchiveWorker . (.pool)) stopArchiveWorker $ \env _archiveWorker -> runHarnessEnv opts env
    Just root -> do
      qualificationPhase "root-validation-begin"
      verifyQualificationRoot root
      qualificationPhase "root-validation-complete"
      privateTemp <- verifiedQualificationTemp root
      acquired <- newIORef False
      observeStartup <- qualificationStartupObserver privateTemp acquired
      qualifiedLifetime <- newSandboxLifetimeWithReleaseHooks (pure ()) observeStartup
      qualificationPhase "borrowed-sandbox-begin"
      outcome <- try @SomeException $ withSandboxedTestEnvBorrowed qualifiedLifetime (acquireQualification root) drainQualification $ \env qualified -> do
        writeIORef acquired True
        qualificationPhase "qualified-database-acquired"
        startQualification qualified
        runHarnessEnvWith (Just qualified) opts (env {pool=qualified.runtimePool})
      case outcome of
        Right () -> pure ()
        Left primary -> recoverQualification qualifiedLifetime primary

runHarnessEnv :: Opts -> TestEnv -> IO ()
runHarnessEnv = runHarnessEnvWith Nothing

runHarnessEnvWith :: Maybe Qualification -> Opts -> TestEnv -> IO ()
runHarnessEnvWith qualified opts env = do
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
  (bracket
      (startChangeStreamWorker env.pool cfg.changeStream wsState)
      stopChangeStreamWorker
      (\_streamWorker ->
        bracket
          (newFastLogger (LogFile fileSpec defaultBufSize))
          (\(_, cleanupLog) -> cleanupLog)
          (\(logAction, _) -> bracket (newPrefixRegistry env.pool) closePrefixRegistry $ \prefixes -> do
            let logger = newLogger logAction (parseLogLevel "info")
            requestLogger <- jsonRequestLogger logAction

            pgvec <- DBPool.checkPgvector env.pool
            app <- mkAppWithPrefixProofs prefixes Nothing cfg.changeStream requestLogger cfg.auth cfg.cors cfg.rateLimit env.pool tracker wsState cfg.web.webStaticDir pgvec

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
                flushForShutdown = do
                  logInfo logger "[sandbox] Shutting down interactive harness..."
                  flushNow env.pool tracker `catch` \(_ :: SomeException) ->
                    logWarn logger "failed to flush access tracker"
                runInput = do
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

            (bracket (startSessionReaper env.pool) stopSessionReaper $ \_ -> withHarnessThread qualified
                (try (runSettings settings app) >>= putMVar serverDone)
                (\serverThread -> case qualified of
                  Just controls -> do
                    -- The qualifier's caller owns stdin directly. A failed
                    -- input-child join therefore cannot compete with recovery
                    -- for this same pipe after the borrowing scope exits.
                    waitUntilReady ready serverDone
                    qualificationPhase "http-ready"
                    printBanner opts env cfg serverUrl logPath configPath seed
                    qualificationInput controls inputDone serverDone
                    killThread serverThread
                  Nothing -> withHarnessThread Nothing
                    runInput
                    (\_inputThread -> do
                      waitUntilReady ready serverDone
                      printBanner opts env cfg serverUrl logPath configPath seed
                      waitForStop inputDone serverDone serverThread)))
              `finally` flushForShutdown)))
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
          , Config.userToken = Nothing
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
          , Config.userToken = Nothing
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

-- Private qualification controls belong to this executable, not the HTTP API.
-- The root is fixed to the active task, and every mutation uses its owned DB.
foreign import ccall unsafe "hmem_archive_qualification_sample"
  nativeQualificationSample :: Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO CInt

data NativeSample = NativeSample
  { sampledAt :: !Word64, workingSet :: !Word64, kernelTicks :: !Word64, userTicks :: !Word64 }
data SamplePhase = SamplePhase
  { phaseName :: !Text, firstSample :: !NativeSample, lastSample :: !NativeSample
  , sampleCount :: !Word64, minimumWorkingSet :: !Word64, maximumWorkingSet :: !Word64
  , maximumGap :: !Word64 }
data QualificationThread = QualificationThread
  { nativeThread :: !ThreadId, completion :: !(MVar (Either SomeException ())) }
data Qualification = Qualification
  { adminEnv :: !TestEnv, runtimePool :: !(Pool H.Connection)
  , archiveWorker :: !(IORef (Maybe ArchiveWorker))
  , children :: !(MVar [QualificationThread]), sampling :: !(MVar SamplePhase) }

verifyQualificationRoot :: FilePath -> IO ()
verifyQualificationRoot root = do
  repo <- canonicalizePath "."
  actual <- canonicalizePath root
  expected <- canonicalizePath (repo </> ".scratch" </> "sealed-archives-4a40c671")
  exists <- doesDirectoryExist actual
  unless (exists && equalFilePath actual expected) $
    fail "Archive qualification requires the existing exact Task8 scratch root"
  -- Refuse ambient database/borrowing coordinates even though this entrypoint
  -- deliberately calls the standalone borrowed-sandbox constructor.
  mapM_ (\name -> lookupEnv name >>= \value -> unless (value == Nothing) $
    fail ("Archive qualification refuses ambient " <> name))
    ["HMEM_TEST_EXTERNAL_DB", "HMEM_TEST_ALLOW_EXTERNAL_DB", "HMEM_TEST_DB"
    ,"HMEM_TEST_SANDBOX_ACTIVE", "HMEM_TEST_SANDBOX_ROOT", "HMEM_TEST_PRESERVE_SANDBOX"]
  temporary <- lookupEnv "TEMP" >>= maybe (fail "Archive qualification requires child-only TEMP") canonicalizePath
  temporary2 <- lookupEnv "TMP" >>= maybe (fail "Archive qualification requires child-only TMP") canonicalizePath
  unless (contained actual temporary && contained actual temporary2) $
    fail "Archive qualification TEMP/TMP must be inside its Task8 root"
  qualificationPhase "native-sample-begin"
  void takeNativeSample
  qualificationPhase "native-sample-complete"

qualificationPhase :: Text -> IO ()
qualificationPhase name = do
  now <- getMonotonicTimeNSec
  LBS.putStr (encode (object ["qualification_phase" .= name,"monotonic_ns" .= show now]) <> "\n")
  hFlush stdout

-- The existing release hook has no root parameter. Bind this observer to the
-- verified Node-owned run's private TEMP before the sandbox changes env state.
verifiedQualificationTemp :: FilePath -> IO FilePath
verifiedQualificationTemp taskRoot = do
  root <- canonicalizePath taskRoot
  raw <- lookupEnv "TEMP" >>= maybe (fail "Qualification TEMP missing") pure
  temporary <- plainQualificationPath raw
  let runRoot=takeDirectory temporary
      marker=runRoot </> "owner.json"
  unless (takeFileName temporary == "tmp" && equalFilePath (takeDirectory runRoot) root) $
    fail "Qualification TEMP is not an immediate Node-owned run/tmp"
  void (plainQualificationPath runRoot)
  void (plainQualificationPath marker)
  markerIsFile <- doesFileExist marker
  unless markerIsFile (fail "Qualification owner marker is not a regular file")
  markerBytes <- bracket (openBinaryFile marker ReadMode) hClose (\handle -> do
    seekable <- hIsSeekable handle
    unless seekable (fail "Qualification owner marker is not seekable")
    size <- hFileSize handle
    unless (size <= 2048) (fail "Qualification owner marker exceeds bound")
    BS.hGet handle 2049)
  unless (BS.length markerBytes <= 2048) (fail "Qualification owner marker changed beyond bound")
  owner <- either (const (fail "Invalid qualification owner marker")) pure (eitherDecodeStrict' markerBytes)
  taskId <- controlField @Text "task" owner
  runId <- controlField @Text "id" owner
  unless (taskId == "4a40c671-2d77-4cca-be24-c7c25ef72337" && not (T.null runId)
    && takeFileName runRoot == "run-" <> T.unpack runId) $
    fail "Qualification TEMP owner/task mismatch"
  pure temporary

plainQualificationPath :: FilePath -> IO FilePath
plainQualificationPath path = do
  symbolic <- pathIsSymbolicLink path
  resolved <- canonicalizePath path
  unless (not symbolic && equalFilePath resolved (normalise path)) $
    fail "Qualification diagnostic path has a symbolic/reparse escape"
  pure resolved

data QualificationLogHandleRetained = QualificationLogHandleRetained Handle SomeException
  deriving stock (Show)
instance Exception QualificationLogHandleRetained

qualificationStartupObserver :: FilePath -> IORef Bool -> IO (IO ())
qualificationStartupObserver privateTemp acquired = do
  pending <- newIORef Nothing
  emitted <- newIORef False
  let emit message = void $ try @IOException $ timeout 100000 $
        BS.hPut stderr ("[qualification-pg-startup-log] " <> message <> "\n") >> hFlush stderr
      closePending = readIORef pending >>= mapM_ (\handle -> do
        closedResult <- try @SomeException (hClose handle >> hIsClosed handle)
        case closedResult of
          Right True -> writeIORef pending Nothing
          Right False -> throwIO (QualificationLogHandleRetained handle (toException (userError "log handle closure unconfirmed")))
          Left failure -> do
            -- IO errors with unknown handle state must escape the observer's
            -- IOException handler. Outer timeout/control exceptions stay exact.
            case fromException failure :: Maybe IOException of
              Just _ -> throwIO (QualificationLogHandleRetained handle failure)
              Nothing -> throwIO failure)
      readTail path = mask $ \restore -> do
        handle <- openBinaryFile path ReadMode
        writeIORef pending (Just handle)
        result <- try @SomeException $ restore $ do
          seekable <- hIsSeekable handle
          unless seekable (fail "PostgreSQL diagnostic is not a regular seekable file")
          size <- hFileSize handle
          hSeek handle AbsoluteSeek (max 0 (size-8192))
          bytes <- BS.hGet handle 8192
          void (plainQualificationPath path)
          pure bytes
        closePending
        either throwIO pure result
      capture = do
        -- Retrying release first closes the same retained handle. No detached
        -- observer, lifetime-MVar access, or path-derived cleanup authority.
        closePending
        void (plainQualificationPath privateTemp)
        entries <- listDirectory privateTemp
        unless (length (take 17 entries) <= 16) (fail "Qualification TEMP inventory exceeds sixteen entries")
        let candidates=filter ("hmem-sandbox-" `isPrefixOf`) entries
        child <- case candidates of
          [name] -> pure (privateTemp </> name)
          _ -> fail "PostgreSQL diagnostic unavailable: expected exactly one sandbox candidate"
        root <- plainQualificationPath child
        directory <- doesDirectoryExist root
        unless (directory && contained privateTemp root && not (equalFilePath privateTemp root)) $
          fail "PostgreSQL diagnostic candidate is not an immediate contained directory"
        let logDirectory=root </> "logs"
            logPath=logDirectory </> "postgresql.log"
        void (plainQualificationPath logDirectory)
        resolved <- plainQualificationPath logPath
        regular <- doesFileExist resolved
        unless (regular && contained root resolved) (fail "PostgreSQL startup log is unavailable or escapes its sandbox")
        bytes <- readTail resolved
        -- The actual read handle is closed before any output or root deletion.
        emit bytes
  pure $ do
    admitted <- readIORef acquired
    done <- readIORef emitted
    unless (admitted || done) $ do
      outcome <- try @IOException (timeout 1000000 capture)
      case outcome of
        Right (Just ()) -> writeIORef emitted True
        Right Nothing -> do
          -- timeout returns only after bracketed read cleanup; an unconfirmed
          -- close escapes as QualificationLogHandleRetained and retains root.
          closePending
          emit "unavailable: observer sub-deadline"
          writeIORef emitted True
        Left failure -> do
          closePending
          emit (encodeUtf8 ("unavailable: " <> T.take 512 (T.pack (show failure))))
          writeIORef emitted True

contained :: FilePath -> FilePath -> Bool
contained parent child = addTrailingPathSeparator parent `isPrefixOf` addTrailingPathSeparator child

takeNativeSample :: IO NativeSample
takeNativeSample = alloca $ \memory -> alloca $ \kernel -> alloca $ \user -> do
  result <- nativeQualificationSample memory kernel user
  unless (result == 1) (fail "Archive qualification current-process Windows memory/CPU API unavailable")
  NativeSample <$> getMonotonicTimeNSec <*> peek memory <*> peek kernel <*> peek user

newPhase :: Text -> NativeSample -> SamplePhase
newPhase name sample = SamplePhase name sample sample 1 sample.workingSet sample.workingSet 0

addSample :: SamplePhase -> NativeSample -> SamplePhase
addSample phase sample = phase
  { lastSample=sample, sampleCount=phase.sampleCount+1
  , minimumWorkingSet=min phase.minimumWorkingSet sample.workingSet
  , maximumWorkingSet=max phase.maximumWorkingSet sample.workingSet
  , maximumGap=max phase.maximumGap (sample.sampledAt-phase.lastSample.sampledAt) }

phaseValue :: SamplePhase -> Value
phaseValue phase = object
  [ "phase" .= phase.phaseName, "samples" .= phase.sampleCount
  , "requested_interval_ms" .= (20::Int)
  , "start_monotonic_ns" .= show phase.firstSample.sampledAt
  , "end_monotonic_ns" .= show phase.lastSample.sampledAt
  , "elapsed_ms" .= (fromIntegral (phase.lastSample.sampledAt-phase.firstSample.sampledAt)/1000000::Double)
  , "maximum_gap_ms" .= (fromIntegral phase.maximumGap/1000000::Double)
  , "minimum_working_set_bytes" .= phase.minimumWorkingSet
  , "maximum_working_set_bytes" .= phase.maximumWorkingSet
  , "kernel_cpu_seconds" .= (fromIntegral (phase.lastSample.kernelTicks-phase.firstSample.kernelTicks)/10000000::Double)
  , "user_cpu_seconds" .= (fromIntegral (phase.lastSample.userTicks-phase.firstSample.userTicks)/10000000::Double)
  , "scope" .= ("application current WorkingSetSize; sampled, not continuous peak; PostgreSQL excluded"::Text) ]

acquireQualification :: FilePath -> TestEnv -> IO Qualification
acquireQualification root env = mask $ \_ -> do
  actual <- canonicalizePath env.testSandbox.sandboxRoot
  target <- canonicalizePath root
  database <- canonicalizePath env.testDb.testDbDataDir
  unless (not env.testDb.testDbUnsafeExternal && contained target actual && contained actual database) $
    fail "Archive qualification requires its actually owned contained PostgreSQL sandbox"
  -- Same restricted-role contract as the reviewed maintenance qualifier. This
  -- role owns no evidence objects and cannot disable their guards.
  DBPool.runSession env.pool (S.sql
    "CREATE ROLE archive_qualification_runtime LOGIN NOINHERIT NOSUPERUSER NOCREATEDB NOCREATEROLE NOREPLICATION NOBYPASSRLS; GRANT USAGE ON SCHEMA public TO archive_qualification_runtime; GRANT SELECT,INSERT,UPDATE,DELETE ON ALL TABLES IN SCHEMA public TO archive_qualification_runtime; GRANT EXECUTE ON ALL FUNCTIONS IN SCHEMA public TO archive_qualification_runtime")
  pool <- DBPool.createPool (env.testDb.testDbConnStr <> " user=archive_qualification_runtime") 6 30 30000
    `onException` DBPool.runSession env.pool (S.sql "DROP OWNED BY archive_qualification_runtime; DROP ROLE archive_qualification_runtime")
  -- No native/thread child is started until the caller has registered this
  -- borrower and its actual pool with SandboxLifetime.
  (Qualification env pool <$> newIORef Nothing <*> newMVar [] <*> (takeNativeSample >>= newMVar . newPhase "setup"))
    `onException` destroyAllResources pool

startQualification :: Qualification -> IO ()
startQualification qualified = mask $ \_ -> do
  ArchiveMaintenance.configureMaintenance qualified.runtimePool ArchiveMaintenance.Pause
  worker <- startArchiveWorker qualified.runtimePool
  writeIORef qualified.archiveWorker (Just worker)
  void $ startQualificationThread qualified $ forever $ do
    threadDelay 20000
    modifyMVar_ qualified.sampling $ \phase -> addSample phase <$> takeNativeSample

startQualificationThread :: Qualification -> IO () -> IO QualificationThread
startQualificationThread qualified action = mask $ \_ -> do
  finished <- newEmptyMVar
  gate <- newEmptyMVar
  modifyMVar qualified.children $ \existing -> do
    unless (length existing < 3) (fail "Archive qualification thread admission full")
    thread <- forkIOWithUnmask $ \unmask -> do
      outcome <- try (takeMVar gate >> unmask action)
      putMVar finished outcome
    let owned = QualificationThread thread finished
    putMVar gate ()
    pure (owned:existing,owned)

stopQualificationThread :: Qualification -> QualificationThread -> IO ()
stopQualificationThread qualified owned = mask $ \restore -> do
  joined <- restore (timeout 4000000 (killThread owned.nativeThread >> readMVar owned.completion))
  case joined of
    Nothing -> fail "Archive qualification child did not join; actual handle and sandbox retained"
    Just outcome -> do
      modifyMVar_ qualified.children (pure . filter (\child -> child.nativeThread /= owned.nativeThread))
      case outcome of
        Right () -> pure ()
        Left failure -> case fromException failure :: Maybe SomeAsyncException of
          Just _ -> pure ()
          Nothing -> throwIO failure

withHarnessThread :: Maybe Qualification -> IO () -> (ThreadId -> IO a) -> IO a
withHarnessThread Nothing action use = bracket (forkIO action) killThread use
withHarnessThread (Just qualified) action use =
  bracket (startQualificationThread qualified action) (stopQualificationThread qualified) (use . (.nativeThread))

drainQualification :: Qualification -> IO ()
drainQualification qualified = do
  readMVar qualified.children >>= mapM_ (stopQualificationThread qualified)
  readIORef qualified.archiveWorker >>= mapM_ stopArchiveWorker
  writeIORef qualified.archiveWorker Nothing
  destroyAllResources qualified.runtimePool
  DBPool.runSession qualified.adminEnv.pool (S.sql "DROP OWNED BY archive_qualification_runtime; DROP ROLE archive_qualification_runtime")

controlField :: FromJSON a => Text -> Value -> IO a
controlField key (Object fields) = case KM.lookup (fromString (T.unpack key)) fields of
  Just value -> case fromJSON value of Aeson.Success result -> pure result; Aeson.Error _ -> fail "Invalid qualification control field"
  Nothing -> fail "Missing qualification control field"
controlField _ _ = fail "Invalid qualification control object"

qualificationInput :: Qualification -> MVar () -> MVar (Either SomeException ()) -> IO ()
qualificationInput qualified stopped serverDone = loop
  where
    loop = do
      line <- BS.hGetLine stdin
      tryReadMVar serverDone >>= \case
        Just (Left failure) -> throwIO failure
        Just (Right ()) -> fail "Qualification server stopped unexpectedly"
        Nothing -> pure ()
      unless (BS.length line <= 8192) (fail "Qualification control exceeds 8192 bytes")
      request <- either (const (fail "Invalid qualification control JSON")) pure (eitherDecodeStrict' line)
      identifier <- controlField @Int "id" request
      command <- controlField @Text "command" request
      result <- case command of
        "phase" -> do
          name <- controlField @Text "name" request
          unless (not (T.null name) && T.length name <= 64 && T.all (\c -> isAlphaNum c || c `elem` ("_-"::String)) name) $
            fail "Invalid qualification phase name"
          modifyMVar qualified.sampling $ \previous -> do
            sample <- takeNativeSample
            pure (newPhase name sample,phaseValue (addSample previous sample))
        "sample" -> modifyMVar qualified.sampling $ \previous -> do
          phase <- addSample previous <$> takeNativeSample
          pure (phase,phaseValue phase)
        "pause" -> control ArchiveMaintenance.Pause
        "resume" -> control ArchiveMaintenance.Resume
        "reclaim-disable" -> control ArchiveMaintenance.DisableReclamation
        "reclaim-enable" -> control ArchiveMaintenance.EnableReclamation
        "status" -> do
          current <- ArchiveMaintenance.maintenanceStatus qualified.runtimePool
          clock <- DBPool.runSession qualified.adminEnv.pool $ S.statement () $ St.Statement
            "SELECT floor(extract(epoch FROM clock_timestamp())*1000000)::bigint::text"
            E.noParams (D.singleRow (D.column (D.nonNullable D.text))) False
          case current of
            Object fields -> pure (Object (KM.insert "db_clock_epoch_us" (String clock) fields))
            _ -> fail "Qualification maintenance status is not an object"
        "snapshot" -> qualificationSnapshot qualified
        "stop" -> pure (object ["stopping" .= True])
        _ -> fail "Unknown qualification control command"
      LBS.putStr (encode (object ["qualification_control" .= True,"id" .= identifier,"result" .= result]) <> "\n")
      hFlush stdout
      if command == "stop" then putMVar stopped () else loop
    control action = ArchiveMaintenance.configureMaintenance qualified.runtimePool action >> ArchiveMaintenance.maintenanceStatus qualified.runtimePool

qualificationSnapshot :: Qualification -> IO Value
qualificationSnapshot qualified = DBPool.runSession qualified.adminEnv.pool $ S.statement () $
  St.Statement
    "SELECT jsonb_build_object('db_clock_epoch_us',floor(extract(epoch FROM clock_timestamp())*1000000)::bigint::text,'archive_generation',(SELECT jsonb_build_object('id',g.id,'session_id',g.session_id,'created_at',g.created_at,'published_at',g.published_at) FROM session_archive_generations g ORDER BY g.created_at DESC,g.id DESC LIMIT 1),'database_bytes',pg_database_size(current_database()),'raw_rows',(SELECT count(*) FROM session_chunks),'raw_stored_values',(SELECT coalesce(sum(pg_column_size(data)),0) FROM session_chunks),'wal_lsn',pg_current_wal_insert_lsn()::text,'retained_wal_bytes',(SELECT sum(size) FROM pg_ls_waldir()),'temporary_bytes',(SELECT temp_bytes FROM pg_stat_database WHERE datname=current_database()),'shared_buffers_setting',current_setting('shared_buffers'),'postgres_scope','configured/SQL observations; not included in application working set','relations',(SELECT jsonb_agg(jsonb_build_object('relation',c.relname,'heap_main',pg_relation_size(c.oid),'heap_other',pg_table_size(c.oid)-pg_relation_size(c.oid)-CASE WHEN c.reltoastrelid=0 THEN 0 ELSE pg_total_relation_size(c.reltoastrelid) END,'indexes',pg_indexes_size(c.oid),'toast_total',CASE WHEN c.reltoastrelid=0 THEN 0 ELSE pg_total_relation_size(c.reltoastrelid) END,'total',pg_total_relation_size(c.oid))) FROM pg_class c JOIN pg_namespace n ON n.oid=c.relnamespace WHERE n.nspname='public' AND c.relname IN ('session_chunks','session_archive_parts','session_archive_checkpoints','session_archive_ledger','session_archive_generations','session_archive_reclamation','session_archive_control','session_archive_requests','session_archive_jobs')))"
    E.noParams (D.singleRow (D.column (D.nonNullable D.jsonb))) False

-- Keep the exact lifetime reachable in this process after a failed drain.
-- Only its explicit supervisor may request another finite release attempt;
-- neither EOF, a timeout, nor a different handle releases retained resources.
data QualificationRecoveryFailure = QualificationRecoveryFailure SomeException [SomeException]
  deriving stock (Show)
instance Exception QualificationRecoveryFailure

recoverQualification :: SandboxLifetime -> SomeException -> IO ()
recoverQualification lifetime primary = mask $ \_ -> do
  secondary <- newIORef []
  released <- newIORef False
  deferredControl <- newIORef Nothing
  let remember failure = do
        modifyIORef' secondary (take 8 . (<> [failure]))
        case fromException failure :: Maybe SomeAsyncException of
          Nothing -> pure ()
          Just _ -> do
            previous <- readIORef deferredControl
            case previous of Nothing -> writeIORef deferredControl (Just failure); Just _ -> pure ()
      finish = do
        causes <- readIORef secondary
        deferred <- readIORef deferredControl
        let asynchronous [] = Nothing
            asynchronous (failure:rest) = case fromException failure :: Maybe SomeAsyncException of
              Just _ -> Just failure
              Nothing -> asynchronous rest
        case asynchronous (primary:maybe [] pure deferred) of
          Just control -> do
            diagnostic <- try @SomeException (hPutStrLn stderr ("[archive-qualification] original failure retained before deferred asynchronous control: " <> take 4096 (show primary)))
            either remember (const (pure ())) diagnostic
            throwIO control
          Nothing -> if null causes then throwIO primary else throwIO (QualificationRecoveryFailure primary causes)
      -- Output, input and the pacing delay are all fallible/interruptible.
      -- Keep each failure bounded and defer it while the lifetime is retained.
      pace = do
        end <- (+1000000000) <$> getMonotonicTimeNSec
        let wait = do
              now <- getMonotonicTimeNSec
              when (now < end) $ do
                attempt <- try @SomeException (threadDelay (fromIntegral ((end-now+999) `div` 1000)))
                either remember (const (pure ())) attempt
                wait
        wait
      turn = do
        emit (object ["qualification_recovery" .= True,"state" .= ("retained"::Text),"owner" .= ("Task8; same native supervisor and SandboxLifetime"::Text),"disposition" .= ("Explicit retry-cleanup; unresolved failure retained until issue closure"::Text)])
        line <- BS.hGetLine stdin
        unless (BS.length line <= 8192) (fail "Qualification recovery frame limit")
        value <- either (const (fail "Invalid recovery control")) pure (eitherDecodeStrict' line)
        identifier <- controlField @Int "id" value
        name <- controlField @Text "command" value
        unless (name == "retry-cleanup") (fail "Only same-owner retry-cleanup is admitted")
        result <- try @SomeException (retrySandboxLifetime lifetime)
        either remember (const (writeIORef released True)) result
        let completed = either (const False) (const True) result
        emit (object ["qualification_control" .= True,"id" .= identifier,"result" .= object ["cleanup_complete" .= completed,"original_failure_preserved" .= True]])
      emit value = do
        output <- try @SomeException (LBS.putStr (encode value <> "\n") >> hFlush stdout)
        either remember (const (pure ())) output
      loop = do
        attempt <- try @SomeException turn
        either remember (const (pure ())) attempt
        complete <- readIORef released
        if complete then finish else pace >> loop
  loop
