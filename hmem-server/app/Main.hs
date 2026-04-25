module Main where

import Data.Maybe (fromMaybe)
import Data.String (fromString)
import Data.Text qualified as T
import Control.Exception (SomeException, bracket, catch, finally)
import Control.Monad (when)
import Data.Pool (Pool, destroyAllResources)
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setHost, setPort, setTimeout, setGracefulShutdownTimeout)
import Network.Wai.Handler.WarpTLS (runTLS, tlsSettings)
import Options.Applicative
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO (BufferMode(..), hPutStrLn, hSetBuffering, stderr)
import System.Log.FastLogger (LogType'(LogFile, LogStderr), FileLogSpec(..), defaultBufSize, newFastLogger)

import Hasql.Connection qualified as Hasql
import Hasql.Session qualified as Session
import HMem.Config qualified as Config
import HMem.DB.Pool qualified as Pool
import HMem.DB.TestHarness (EphemeralPg(..), checkPgTools, startEphemeralPg, stopEphemeralPg, ensureSchema, TestEnv(..))
import HMem.Server.AccessTracker (newAccessTracker, flushNow)
import HMem.Server.App (mkApp)
import HMem.Server.Logging (newLogger, parseLogLevel, logInfo, logWarn, jsonRequestLogger)
import HMem.Server.Static (resolveStaticDir)
import HMem.Server.WebSocket (newWSState)

------------------------------------------------------------------------
-- CLI
------------------------------------------------------------------------

data Opts = Opts
  { optPort    :: Maybe Int
  , optDbConn  :: Maybe String
  , optPool    :: Maybe Int
  , optTlsCert :: Maybe FilePath
  , optTlsKey  :: Maybe FilePath
  , optDev     :: Bool
  }

optsParser :: Parser Opts
optsParser = Opts
  <$> optional (option auto
      ( long "port"
     <> short 'p'
     <> metavar "PORT"
     <> help "HTTP port (default: from ~/.hmem/config.yaml or 8420)"
      ))
  <*> optional (strOption
      ( long "db"
     <> short 'd'
     <> metavar "CONNSTR"
     <> help "PostgreSQL connection string (default: from config)"
      ))
  <*> optional (option auto
      ( long "pool-size"
     <> metavar "N"
     <> help "Maximum database connections (default: from config or 10)"
      ))
  <*> optional (strOption
      ( long "tls-cert"
     <> metavar "FILE"
     <> help "Path to TLS certificate file (enables HTTPS when used with --tls-key)"
      ))
  <*> optional (strOption
      ( long "tls-key"
     <> metavar "FILE"
     <> help "Path to TLS private key file (enables HTTPS when used with --tls-cert)"
      ))
  <*> switch
      ( long "dev"
     <> help "Dev mode: ephemeral PostgreSQL with seed data, logs to stderr"
      )

------------------------------------------------------------------------
-- Main
------------------------------------------------------------------------

main :: IO ()
main = do
  hSetBuffering stderr LineBuffering
  opts <- execParser $ info (optsParser <**> helper)
    ( fullDesc
   <> progDesc "hmem - LLM memory & task management server"
   <> header "hmem-server"
    )

  if opts.optDev
    then runDevMode opts
    else runNormalMode opts

-- | Dev mode: ephemeral PostgreSQL, logs to stderr, seed data.
runDevMode :: Opts -> IO ()
runDevMode opts = do
  hPutStrLn stderr "[dev] Checking for PostgreSQL tools..."
  checkPgTools
  bracket startEphemeralPg stopEphemeralPg $ \pg -> do
    let port   = fromMaybe 8420 opts.optPort
        connStr = pg.epConnStr

    pool <- Pool.createPool connStr 5 5.0 30000
    -- Apply schema
    ensureSchema (TestEnv { pool = pool })
    -- Seed demo data
    seedDevData pool

    tracker <- newAccessTracker pool 5
    pgvec <- Pool.checkPgvector pool
    wsState <- newWSState
    mStaticDir <- resolveStaticDir (Just "hmem-server/static")

    (logAction, cleanupLog) <- newFastLogger (LogStderr defaultBufSize)
    let logger = newLogger logAction (parseLogLevel "info")
    requestLogger <- jsonRequestLogger logAction

    logInfo logger $ "[dev] Ephemeral PostgreSQL on port " <> T.pack (show pg.epPort)
    logInfo logger $ "[dev] hmem-server listening on http://localhost:" <> T.pack (show port)
    logInfo logger $ "[dev] Web UI: " <> case mStaticDir of
      Just dir -> "serving from " <> T.pack dir
      Nothing  -> "no static/ found — run 'stack run build-frontend' first"
    logInfo logger "[dev] WebSocket: enabled at /api/v1/ws"
    logInfo logger "[dev] Auth: disabled"
    logInfo logger "[dev] Press Ctrl-C to stop (ephemeral DB will be destroyed)"

    let devAuth = Config.defaultConfig.auth { Config.enabled = False, Config.apiKey = Nothing }
        devCors = Config.CorsConfig { allowedOrigins = ["*"] }
        devRateLimit = Config.RateLimitConfig { rlEnabled = False, rlRequestsPerSecond = 100, rlBurst = 200 }
        settings = setHost (fromString "127.0.0.1") $ setPort port $ setTimeout 60 $ setGracefulShutdownTimeout (Just 5) $ defaultSettings
        shutdown = do
          logInfo logger "[dev] Shutting down..."
          flushNow pool tracker `catch` \(_ :: SomeException) -> pure ()
          destroyAllResources pool
          cleanupLog

    app <- mkApp requestLogger devAuth devCors devRateLimit pool tracker wsState mStaticDir pgvec
    runSettings settings app `finally` shutdown

-- | Seed some sample data for dev mode so the UI has something to show.
seedDevData :: Pool Hasql.Connection -> IO ()
seedDevData pool = do
  hPutStrLn stderr "[dev] Seeding demo data..."
  Pool.withConn pool $ \conn -> do
    let seedSql = "\
          \INSERT INTO workspaces (name, workspace_type) VALUES \
          \  ('Demo Workspace', 'repository'), \
          \  ('Research Notes', 'personal'); \
          \\
          \INSERT INTO projects (workspace_id, name, description, status) \
          \SELECT w.id, p.name, p.description, p.status::project_status_enum \
          \FROM workspaces w, \
          \     (VALUES ('Web Frontend', 'Build the hmem web UI with Elm and Vite', 'active'), \
          \            ('API Improvements', 'Enhance the REST API with new endpoints and validation', 'active'), \
          \            ('Documentation', 'Write user guides and API docs', 'paused') \
          \     ) AS p(name, description, status) \
          \WHERE w.name = 'Demo Workspace'; \
          \\
          \INSERT INTO projects (workspace_id, name, description, status, parent_id) \
          \SELECT w.id, 'Graph Visualization', 'Cytoscape.js knowledge graph component', 'active', p.id \
          \FROM workspaces w \
          \JOIN projects p ON p.workspace_id = w.id AND p.name = 'Web Frontend' \
          \WHERE w.name = 'Demo Workspace'; \
          \\
          \INSERT INTO tasks (workspace_id, project_id, title, description, status, priority) \
          \SELECT w.id, p.id, t.title, t.description, t.status::task_status_enum, t.priority \
          \FROM workspaces w \
          \JOIN projects p ON p.workspace_id = w.id AND p.name = 'Web Frontend', \
          \     (VALUES ('Set up Elm scaffold', 'Create Main.elm and ports', 'done', 8), \
          \            ('Implement WebSocket client', 'Connect to /api/v1/ws for real-time updates', 'in_progress', 7), \
          \            ('Build graph view', 'Cytoscape.js integration for memory visualization', 'todo', 6), \
          \            ('Add drag-and-drop', 'Entity reordering in sidebar via HTML5 drag API', 'todo', 5), \
          \            ('Style status badges', 'Color-code project and task status indicators', 'blocked', 4) \
          \     ) AS t(title, description, status, priority) \
          \WHERE w.name = 'Demo Workspace'; \
          \\
          \INSERT INTO tasks (workspace_id, project_id, title, description, status, priority) \
          \SELECT w.id, p.id, t.title, t.description, t.status::task_status_enum, t.priority \
          \FROM workspaces w \
          \JOIN projects p ON p.workspace_id = w.id AND p.name = 'API Improvements', \
          \     (VALUES ('Add pagination headers', 'Include X-Total-Count and Link headers', 'done', 7), \
          \            ('Rate limiting middleware', 'Implement token bucket rate limiter', 'in_progress', 6) \
          \     ) AS t(title, description, status, priority) \
          \WHERE w.name = 'Demo Workspace'; \
          \\
          \INSERT INTO tasks (workspace_id, project_id, title, description, status, priority, parent_id) \
          \SELECT w.id, t.project_id, 'Handle reconnection logic', 'Auto-reconnect with exponential backoff', 'todo'::task_status_enum, 6, t.id \
          \FROM workspaces w \
          \JOIN tasks t ON t.workspace_id = w.id AND t.title = 'Implement WebSocket client' \
          \WHERE w.name = 'Demo Workspace'; \
          \\
          \INSERT INTO task_dependencies (task_id, depends_on_id) \
          \SELECT t1.id, t2.id \
          \FROM tasks t1, tasks t2 \
          \WHERE t1.title = 'Build graph view' AND t2.title = 'Set up Elm scaffold'; \
          \\
          \INSERT INTO task_dependencies (task_id, depends_on_id) \
          \SELECT t1.id, t2.id \
          \FROM tasks t1, tasks t2 \
          \WHERE t1.title = 'Style status badges' AND t2.title = 'Set up Elm scaffold'; \
          \\
          \INSERT INTO task_dependencies (task_id, depends_on_id) \
          \SELECT t1.id, t2.id \
          \FROM tasks t1, tasks t2 \
          \WHERE t1.title = 'Add drag-and-drop' AND t2.title = 'Build graph view'; \
          \\
          \INSERT INTO memories (workspace_id, content, summary, memory_type, importance) \
          \SELECT w.id, m.content, m.summary, m.mtype::memory_type_enum, m.importance \
          \FROM workspaces w, \
          \     (VALUES ('The hmem server uses Servant for its REST API and Rel8 for database queries. The API follows RESTful conventions with JSON request/response bodies.', 'Server Architecture', 'long_term', 8), \
          \            ('Always run migrations before starting the server in a new environment. Use the setup executable or --dev flag for automatic schema management.', 'Migration Workflow', 'long_term', 7), \
          \            ('WebSocket events use the format: {type, entity_type, entity_id, timestamp, data}. Clients should reconnect with exponential backoff on disconnect.', 'WebSocket Protocol', 'long_term', 6), \
          \            ('Elm ports are used for JavaScript interop with Cytoscape.js and WebSocket. Port subscriptions are defined in main.js.', 'Elm Ports Pattern', 'short_term', 5), \
          \            ('The frontend dev server (Vite) proxies /api to localhost:8420. Use npm run dev for hot-reload during development.', 'Vite Dev Setup', 'short_term', 4) \
          \     ) AS m(content, summary, mtype, importance) \
          \WHERE w.name = 'Demo Workspace'; \
          \\
          \INSERT INTO project_memory_links (project_id, memory_id) \
          \SELECT p.id, m.id \
          \FROM projects p, memories m \
          \WHERE p.name = 'Web Frontend' AND m.summary = 'Server Architecture'; \
          \\
          \INSERT INTO project_memory_links (project_id, memory_id) \
          \SELECT p.id, m.id \
          \FROM projects p, memories m \
          \WHERE p.name = 'Web Frontend' AND m.summary = 'Elm Ports Pattern'; \
          \\
          \INSERT INTO project_memory_links (project_id, memory_id) \
          \SELECT p.id, m.id \
          \FROM projects p, memories m \
          \WHERE p.name = 'API Improvements' AND m.summary = 'Server Architecture'; \
          \\
          \INSERT INTO task_memory_links (task_id, memory_id) \
          \SELECT t.id, m.id \
          \FROM tasks t, memories m \
          \WHERE t.title = 'Implement WebSocket client' AND m.summary = 'WebSocket Protocol'; \
          \\
          \INSERT INTO task_memory_links (task_id, memory_id) \
          \SELECT t.id, m.id \
          \FROM tasks t, memories m \
          \WHERE t.title = 'Build graph view' AND m.summary = 'Elm Ports Pattern'; \
          \\
          \INSERT INTO task_memory_links (task_id, memory_id) \
          \SELECT t.id, m.id \
          \FROM tasks t, memories m \
          \WHERE t.title = 'Set up Elm scaffold' AND m.summary = 'Vite Dev Setup';"
    result <- Session.run (Session.sql seedSql) conn
    case result of
      Left err -> hPutStrLn stderr $ "[dev] seed warning: " ++ show err
      Right _  -> hPutStrLn stderr "[dev] Demo data seeded."

-- | Normal production mode.
runNormalMode :: Opts -> IO ()
runNormalMode opts = do
  cfg <- Config.loadConfig

  let port    = fromMaybe cfg.server.port opts.optPort
      connStr = maybe (Config.connectionString cfg.database) T.pack opts.optDbConn
      poolSz  = fromMaybe cfg.pool.size opts.optPool

  pool <- Pool.createPool connStr poolSz cfg.pool.idleTimeout cfg.pool.statementTimeoutMs
  tracker <- newAccessTracker pool 5  -- flush access counts every 5 seconds
  pgvec <- Pool.checkPgvector pool

  -- WebSocket state
  wsState <- newWSState

  -- Resolve static file directory for the web frontend
  mStaticDir <- if cfg.web.webEnabled
    then resolveStaticDir cfg.web.webStaticDir
    else pure Nothing

  -- Set up rotating file logger in ~/.hmem/logs/
  logDir <- (</> "logs") <$> Config.configDir
  createDirectoryIfMissing True logDir
  let logPath = logDir </> "hmem-server.log"
      fileSpec = FileLogSpec
        { log_file          = logPath
        , log_file_size     = fromIntegral cfg.logging.maxSizeMB * 1024 * 1024
        , log_backup_number = cfg.logging.backupCount
        }
  (logAction, cleanupLog) <- newFastLogger (LogFile fileSpec defaultBufSize)
  let logger = newLogger logAction (parseLogLevel cfg.logging.level)
  requestLogger <- jsonRequestLogger logAction

  logInfo logger $ "hmem-server listening on " <> cfg.server.host <> ":" <> T.pack (show port)
  logInfo logger $ "Logging to: " <> T.pack logPath
  logInfo logger $ "Rotation: " <> T.pack (show cfg.logging.maxSizeMB) <> " MB, "
                               <> T.pack (show cfg.logging.backupCount) <> " backups"
  logInfo logger $ "Log level: " <> cfg.logging.level
  logInfo logger $ "API auth (legacy static bearer path): " <> if Config.authStaticBearerEnabled cfg.auth then "enabled" else "disabled"
  logInfo logger $ "Rate limiting: " <> if cfg.rateLimit.rlEnabled then "enabled" else "disabled"
  logInfo logger $ "pgvector: " <> if pgvec then "available" else "not installed (similarity search disabled)"
  logInfo logger $ "Web UI: " <> case mStaticDir of
    Just dir -> "serving from " <> T.pack dir
    Nothing  -> "disabled (no static/ directory found)"
  logInfo logger $ "WebSocket: enabled at /api/v1/ws"

  -- Warn when CORS origins are localhost-only but server is network-accessible
  let isLoopback h = h `elem` ["127.0.0.1", "localhost", "::1"]
      originsAreLocal = not (null cfg.cors.allowedOrigins)
                     && "*" `notElem` cfg.cors.allowedOrigins
                     && all (\o -> any (`T.isInfixOf` o) ["localhost", "127.0.0.1", "::1"]) cfg.cors.allowedOrigins
  when (not (isLoopback cfg.server.host) && originsAreLocal) $
    logWarn logger $ "CORS allowedOrigins are localhost-only but server is bound to "
                  <> cfg.server.host <> " — remote clients will be rejected by CORS"

  let settings = setHost (fromString (T.unpack cfg.server.host))
               $ setPort port
               $ setTimeout 60
               $ setGracefulShutdownTimeout (Just 30)
               $ defaultSettings
  let shutdown = do
        logInfo logger "hmem-server: shutting down..."
        flushNow pool tracker
          `catch` \(_ :: SomeException) -> logWarn logger "failed to flush access tracker"
        destroyAllResources pool
        cleanupLog

  -- Resolve TLS config: CLI flags override config.yaml
  let mTlsCert = opts.optTlsCert <|> cfg.tls.tlsCertFile
      mTlsKey  = opts.optTlsKey  <|> cfg.tls.tlsKeyFile

  app <- mkApp requestLogger cfg.auth cfg.cors cfg.rateLimit pool tracker wsState mStaticDir pgvec
  case (mTlsCert, mTlsKey) of
    (Just cert, Just key) -> do
      logInfo logger $ "TLS enabled: cert=" <> T.pack cert <> " key=" <> T.pack key
      let tls = tlsSettings cert key
      runTLS tls settings app
        `finally` shutdown
    (Just _, Nothing) -> do
      logWarn logger "--tls-cert provided without --tls-key; running plain HTTP"
      runSettings settings app
        `finally` shutdown
    (Nothing, Just _) -> do
      logWarn logger "--tls-key provided without --tls-cert; running plain HTTP"
      runSettings settings app
        `finally` shutdown
    (Nothing, Nothing) -> do
      logInfo logger "TLS disabled (no cert/key configured)"
      runSettings settings app
        `finally` shutdown
