module HMem.DB.TestHarness
  ( -- * Test environment
    TestEnv(..)
  , withTestEnv
    -- * Ephemeral PostgreSQL
  , withEphemeralPg
    -- * DB utilities
  , cleanDB
  , ensureSchema
    -- * Fixture helpers
  , createTestWorkspace
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, bracket, try)
import Data.ByteString qualified as BS
import Data.List (sort)
import Data.Pool (Pool)
import Data.Text (Text)
import Data.Text qualified as T
import System.Directory (doesDirectoryExist, findExecutable, getCurrentDirectory,
                         listDirectory, removeDirectoryRecursive)
import System.Environment (lookupEnv, setEnv)
import System.FilePath ((</>))
import System.IO (hFlush, hPutStrLn, stderr)
import System.IO.Temp (createTempDirectory, getCanonicalTemporaryDirectory)
import System.Process (callProcess)
import System.Random (randomRIO)
import Hasql.Connection qualified as Hasql
import Hasql.Decoders qualified as D
import Hasql.Encoders qualified as E
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement
import Rel8

import HMem.DB.Pool (createPool, withConn)
import HMem.DB.Schema
import HMem.Types

-- | Test environment wrapping the connection pool.
data TestEnv = TestEnv
  { pool :: Pool Hasql.Connection
  }

-- | Default connection string when HMEM_TEST_DB is not set.
defaultTestConnStr :: Text
defaultTestConnStr = "host=localhost dbname=hmem_test"

-- | Read the test connection string from the HMEM_TEST_DB environment
-- variable, falling back to @host=localhost dbname=hmem_test@.
getTestConnStr :: IO Text
getTestConnStr = do
  env <- lookupEnv "HMEM_TEST_DB"
  pure $ maybe defaultTestConnStr T.pack env

-- | Set up a test environment: read config from env, create a pool,
-- ensure the schema is applied, and truncate all tables.
--
-- Usage with hspec:
--
-- @
-- spec :: Spec
-- spec = around withTestEnv $ do
--   it "does something" $ \\env -> do
--     ws <- createTestWorkspace env "my-ws"
--     ...
-- @
withTestEnv :: (TestEnv -> IO a) -> IO a
withTestEnv action = do
  connStr <- getTestConnStr
  p <- createPool connStr 10 5.0
  let env = TestEnv { pool = p }
  ensureSchema env
  cleanDB env
  action env

-- | Check whether the schema has been applied and apply it if not.
-- Looks for @sql/migrations/@ relative to the working directory.
ensureSchema :: TestEnv -> IO ()
ensureSchema env = withConn env.pool $ \conn -> do
  let checkStmt = Statement.Statement
        "SELECT EXISTS (SELECT 1 FROM information_schema.tables \
        \WHERE table_schema = 'public' AND table_name = 'workspaces')"
        E.noParams
        (D.singleRow (D.column (D.nonNullable D.bool)))
        True
  result <- Session.run (Session.statement () checkStmt) conn
  case result of
    Right True -> pure ()  -- schema already applied
    _ -> applyMigrations conn

-- | Find the migrations directory and apply all SQL files in order.
applyMigrations :: Hasql.Connection -> IO ()
applyMigrations conn = do
  dir <- findMigrationsDir
  files <- sort . Prelude.filter isMigration <$> listDirectory dir
  mapM_ (applyOne dir) files
  where
    isMigration f = take 1 f == "V" && drop (length f - 4) f == ".sql"
    applyOne dir f = do
      sql <- BS.readFile (dir </> f)
      result <- Session.run (Session.sql sql) conn
      case result of
        Left err -> fail $ "Failed to apply migration " <> f <> ": " <> show err
        Right _  -> pure ()

-- | Search a few candidate paths for @sql/migrations/@.
findMigrationsDir :: IO FilePath
findMigrationsDir = do
  let candidates =
        [ "sql/migrations"
        , "../sql/migrations"
        , "../../sql/migrations"
        ]
  found <- mapM (\p -> (,) <$> doesDirectoryExist p <*> pure p) candidates
  case [p | (True, p) <- found] of
    (p:_) -> pure p
    []    -> do
      cwd <- getCurrentDirectory
      fail $ "Could not find sql/migrations/ (cwd: " <> cwd <> ")"

-- | Truncate all tables in dependency order, resetting the database
-- to a clean state between tests.
cleanDB :: TestEnv -> IO ()
cleanDB env = withConn env.pool $ \conn -> do
  let stmt = Statement.Statement sql E.noParams D.noResult True
      sql = "TRUNCATE \
            \  workspace_group_members, \
            \  workspace_groups, \
            \  task_memory_links, \
            \  project_memory_links, \
            \  task_dependencies, \
            \  memory_links, \
            \  memory_tags, \
            \  memory_category_links, \
            \  tasks, \
            \  projects, \
            \  cleanup_policies, \
            \  memories, \
            \  memory_categories, \
            \  workspaces \
            \CASCADE"
  result <- Session.run (Session.statement () stmt) conn
  case result of
    Left err -> fail $ "Failed to clean test DB: " <> show err
    Right _  -> pure ()

-- | Insert a workspace with just a name and return its domain type.
-- Useful as a test fixture since most entities require a workspace.
createTestWorkspace :: TestEnv -> Text -> IO Workspace
createTestWorkspace env wsName = withConn env.pool $ \conn -> do
  let sess = Session.statement () $ run $
        insert Insert
          { into = workspaceSchema
          , rows = values
              [ WorkspaceT
                  { wsId        = unsafeDefault
                  , wsName      = lit wsName
                  , wsPath      = lit (Nothing :: Maybe Text)
                  , wsGhOwner   = lit (Nothing :: Maybe Text)
                  , wsGhRepo    = lit (Nothing :: Maybe Text)
                  , wsType      = lit WsRepository
                  , wsCreatedAt = unsafeDefault
                  , wsUpdatedAt = unsafeDefault
                  }
              ]
          , onConflict = Abort
          , returning  = Returning id
          }
  result <- Session.run sess conn
  case result of
    Left err -> fail $ "Failed to create test workspace: " <> show err
    Right (r:_) ->
      pure Workspace
        { id            = r.wsId
        , name          = r.wsName
        , path          = r.wsPath
        , ghOwner       = r.wsGhOwner
        , ghRepo        = r.wsGhRepo
        , workspaceType = r.wsType
        , createdAt     = r.wsCreatedAt
        , updatedAt     = r.wsUpdatedAt
        }
    Right [] -> fail "createTestWorkspace: INSERT returned no rows"

------------------------------------------------------------------------
-- Ephemeral PostgreSQL
------------------------------------------------------------------------

-- | State of an ephemeral PostgreSQL cluster started for testing.
data EphemeralPg = EphemeralPg
  { epTmpDir  :: FilePath
  , epDataDir :: FilePath
  , epPort    :: Int
  , epConnStr :: Text
  }

-- | Bracket that starts an ephemeral PostgreSQL instance when
-- @HMEM_TEST_DB@ is not set, runs the inner action, then guarantees
-- teardown.  If @HMEM_TEST_DB@ is already set, the action runs
-- directly against that database.
--
-- Designed for use with hspec's @aroundAll_@ in a @SpecHook@ module:
--
-- @
-- hook :: Spec -> Spec
-- hook = aroundAll_ withEphemeralPg
-- @
withEphemeralPg :: IO () -> IO ()
withEphemeralPg action = do
  env <- lookupEnv "HMEM_TEST_DB"
  case env of
    Just _  -> action
    Nothing -> do
      checkPgTools
      bracket startEphemeralPg stopEphemeralPg $ \pg -> do
        setEnv "HMEM_TEST_DB" (T.unpack pg.epConnStr)
        action

-- | Fail immediately when required PostgreSQL CLI tools are missing.
checkPgTools :: IO ()
checkPgTools = do
  let required = ["initdb", "pg_ctl", "createdb"]
  results <- mapM (\cmd -> (,) cmd <$> findExecutable cmd) required
  let missing = [cmd | (cmd, Nothing) <- results]
  case missing of
    [] -> pure ()
    _  -> fail $ unlines
            [ "HMEM_TEST_DB is not set and required PostgreSQL tools not on PATH: "
                ++ unwords missing
            , "Install PostgreSQL (ensure its bin/ is on PATH) or set HMEM_TEST_DB"
            , "to point at an existing test database."
            ]

-- | Start an ephemeral PostgreSQL cluster on a random port.
startEphemeralPg :: IO EphemeralPg
startEphemeralPg = do
  port <- randomRIO (49152, 65535) :: IO Int
  tmpBase <- getCanonicalTemporaryDirectory
  tmpDir  <- createTempDirectory tmpBase "hmem-test-pg"
  let dataDir = tmpDir </> "data"
      logFile = tmpDir </> "pg.log"
      connStr = "host=localhost port=" <> T.pack (show port) <> " dbname=hmem_test"

  hPutStrLn stderr $ "[test-pg] tmp dir : " ++ tmpDir
  hPutStrLn stderr $ "[test-pg] port    : " ++ show port
  hFlush stderr

  -- Initialise a fresh data directory
  callProcess "initdb"
    [ "-D", dataDir, "--auth=trust", "--no-instructions"
    , "--no-locale", "-E", "UTF8"
    ]

  -- Configure port / localhost only
  appendFile (dataDir </> "postgresql.conf") $ unlines
    [ "", "# hmem test overrides"
    , "port = " ++ show port
    , "listen_addresses = 'localhost'"
    ]

  -- Start
  callProcess "pg_ctl"
    [ "start", "-D", dataDir, "-l", logFile, "-w", "-t", "30" ]
  threadDelay 500000

  -- Create the test database
  callProcess "createdb"
    [ "-h", "localhost", "-p", show port, "hmem_test" ]

  hPutStrLn stderr "[test-pg] PostgreSQL ready."
  hFlush stderr

  pure EphemeralPg
    { epTmpDir  = tmpDir
    , epDataDir = dataDir
    , epPort    = port
    , epConnStr = connStr
    }

-- | Stop the ephemeral PostgreSQL cluster and remove its temp
-- directory.  Ignores errors so teardown always completes.
stopEphemeralPg :: EphemeralPg -> IO ()
stopEphemeralPg pg = do
  hPutStrLn stderr "[test-pg] Tearing down..."
  hFlush stderr
  _ <- try (callProcess "pg_ctl"
        [ "stop", "-D", pg.epDataDir, "-m", "fast" ]
        ) :: IO (Either SomeException ())
  -- Small delay so Windows releases file locks
  threadDelay 500000
  _ <- try (removeDirectoryRecursive pg.epTmpDir
        ) :: IO (Either SomeException ())
  pure ()
