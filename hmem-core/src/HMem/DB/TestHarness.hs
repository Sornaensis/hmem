module HMem.DB.TestHarness
  ( -- * Test environment
    TestEnv(..)
  , TestSandbox(..)
  , TestDb(..)
  , AuditLogRow(..)
  , withTestEnv
  , withTestSandbox
  , withSandboxedEnv
  , withSandboxedPostgres
  , withSandboxedTestEnv
  , assertInSandbox
    -- * Transaction-based test isolation
  , setupTestPool
  , withTestTransaction
    -- * Ephemeral PostgreSQL
  , EphemeralPg(..)
  , withEphemeralPg
  , startEphemeralPg
  , stopEphemeralPg
  , checkPgTools
  , resolveRepoRoot
  , resolveMigrationsDir
    -- * DB utilities
  , cleanDB
  , ensureSchema
  , getAuditLogRows
    -- * Fixture helpers
  , createTestWorkspace
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, bracket, bracketOnError, finally, throwIO, try)
import Control.Monad (forM_, unless, void, when)
import Data.Aeson (Value)
import Data.Functor.Contravariant (contramap)
import Data.Foldable qualified as Foldable
import Data.Maybe (catMaybes, isJust)
import Data.Pool (Pool)
import Data.Text (Text)
import Data.Text qualified as T
import Data.UUID (UUID)
import Data.UUID.V4 qualified as UUID
import System.Directory (canonicalizePath, createDirectoryIfMissing, doesDirectoryExist, doesFileExist,
                         findExecutable, getCurrentDirectory, removeDirectoryRecursive)
import System.Environment (getExecutablePath, lookupEnv, setEnv, unsetEnv)
import System.FilePath ((</>), isPathSeparator, normalise, takeDirectory, takeFileName)
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

import HMem.DB.Migration qualified as Migration
import HMem.DB.Pool (createPool, runSession, setTestTransactionMode, withConn)
import HMem.DB.Schema
import HMem.Types
import Paths_hmem_core (getDataDir)

-- | Test environment wrapping the connection pool.
data TestEnv = TestEnv
  { pool :: Pool Hasql.Connection
  , testSandbox :: TestSandbox
  , testDb :: TestDb
  }

data TestSandbox = TestSandbox
  { sandboxRoot :: FilePath
  , sandboxTmpDir :: FilePath
  , sandboxLogDir :: FilePath
  , sandboxConfigDir :: FilePath
  , sandboxStaticDir :: FilePath
  , sandboxCacheDir :: FilePath
  , sandboxHomeDir :: FilePath
  , sandboxRepoRoot :: FilePath
  , sandboxMigrationsDir :: FilePath
  , sandboxPreserveOnFailure :: Bool
  } deriving stock (Show, Eq)

data TestDb = TestDb
  { testDbDataDir :: FilePath
  , testDbLogFile :: FilePath
  , testDbPort :: Int
  , testDbName :: Text
  , testDbConnStr :: Text
  , testDbUnsafeExternal :: Bool
  } deriving stock (Show, Eq)

data AuditLogRow = AuditLogRow
  { entityType :: Text
  , entityId :: Text
  , action :: Text
  , workspaceId :: Maybe UUID
  , actorType :: Maybe Text
  , actorId :: Maybe Text
  , actorLabel :: Maybe Text
  , requestId :: Maybe Text
  , oldValues :: Maybe Value
  , newValues :: Maybe Value
  } deriving stock (Show, Eq)

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
  active <- currentSandboxFromEnv
  case active of
    Just (sandbox, db) -> withPooledTestEnv sandbox db 10 action
    Nothing -> withConfiguredExternalOrSandboxedTestEnv action

------------------------------------------------------------------------
-- Transaction-based test isolation
------------------------------------------------------------------------

-- | Create a shared test pool with a single connection.  Call once
-- per test suite via @beforeAll@, then pair with 'withTestTransaction'
-- via @aroundWith@ so each test runs inside a transaction that is
-- rolled back on completion.
--
-- @
-- spec :: Spec
-- spec = beforeAll setupTestPool $ aroundWith withTestTransaction $ do
--   it "does something" $ \\env -> …
-- @
setupTestPool :: IO TestEnv
setupTestPool = do
  (sandbox, db) <- requireCurrentSandbox
  withPooledTestEnv sandbox db 1 pure

-- | Wrap a single test case in a database transaction that is always
-- rolled back, regardless of success or failure.  Requires a pool of
-- size 1 (from 'setupTestPool') so that all operations within the
-- test share the same connection and transaction context.
withTestTransaction :: (TestEnv -> IO a) -> TestEnv -> IO a
withTestTransaction action env = do
  -- Open an outer transaction on the single pooled connection.
  withConn env.pool $ \conn -> do
    result <- Session.run (Session.sql "BEGIN") conn
    case result of
      Left err -> fail $ "Failed to BEGIN test transaction: " <> show err
      Right _  -> pure ()
  setTestTransactionMode True
  action env `finally` do
    setTestTransactionMode False
    -- Roll back – undoes all data written by the test.
    withConn env.pool $ \conn ->
      void $ Session.run (Session.sql "ROLLBACK") conn

-- | Check whether the schema has been applied and apply it if not.
ensureSchema :: TestEnv -> IO ()
ensureSchema env = do
  result <- Migration.runMigrations env.pool env.testSandbox.sandboxMigrationsDir
  case result.failed of
    Just (file, err) -> fail $ "Failed to apply migration " <> file <> ": " <> err
    Nothing          -> pure ()

resolveRepoRoot :: IO FilePath
resolveRepoRoot = do
  explicit <- lookupEnv repoRootOverrideVar
  cwd <- Just <$> (canonicalizePath =<< getCurrentDirectory)
  exeDir <- either (const Nothing) (Just . takeDirectory) <$> (try getExecutablePath :: IO (Either SomeException FilePath))
  dataDir <- either (const Nothing) Just <$> (try getDataDir :: IO (Either SomeException FilePath))
  let candidates = catMaybes [explicit, cwd, exeDir, dataDir]
  resolved <- firstExistingRepoRoot candidates
  case resolved of
    Just repoRoot -> pure repoRoot
    Nothing -> fail $ "Could not resolve hmem repository root from candidates: " <> show candidates
  where
    firstExistingRepoRoot [] = pure Nothing
    firstExistingRepoRoot (candidate:rest) = do
      found <- findRepoRootFrom candidate
      case found of
        Just repoRoot -> pure (Just repoRoot)
        Nothing -> firstExistingRepoRoot rest

    findRepoRootFrom anchor = do
      start <- canonicalizePath anchor
      go start

    go dir = do
      hasStack <- doesFileExist (dir </> "stack.yaml")
      hasMigrations <- doesDirectoryExist (dir </> "hmem-server" </> "migrations")
      if hasStack && hasMigrations
        then pure (Just dir)
        else do
          let parent = takeDirectory dir
          if parent == dir
            then pure Nothing
            else go parent

resolveMigrationsDir :: FilePath -> IO FilePath
resolveMigrationsDir repoRoot = do
  dir <- canonicalizePath (repoRoot </> "hmem-server" </> "migrations")
  migrationsExists <- doesDirectoryExist dir
  firstMigration <- doesFileExist (dir </> "V001__initial_schema.sql")
  if migrationsExists && firstMigration
    then pure dir
    else fail $ "Could not resolve hmem-server/migrations under repo root: " <> repoRoot

-- | Truncate all tables in dependency order, resetting the database
-- to a clean state between tests.
cleanDB :: TestEnv -> IO ()
cleanDB env = withConn env.pool $ \conn -> do
  let stmt = Statement.Statement sql E.noParams D.noResult True
      sql = "TRUNCATE \
             \  audit_log, \
             \  access_tokens, \
             \  workspace_memberships, \
             \  users, \
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

getAuditLogRows :: Pool Hasql.Connection -> Text -> Text -> IO [AuditLogRow]
getAuditLogRows pool entityType entityId =
  runSession pool $ Session.statement (entityType, entityId) auditLogRowsStatement

auditLogRowsStatement :: Statement.Statement (Text, Text) [AuditLogRow]
auditLogRowsStatement = Statement.Statement sql encoder decoder True
  where
    sql = "SELECT entity_type, entity_id, action::text, workspace_id, actor_type::text, actor_id, actor_label, request_id, old_values, new_values \
          \FROM audit_log \
          \WHERE entity_type = $1 AND entity_id = $2 \
          \ORDER BY changed_at ASC, id ASC"
    encoder =
      contramap fst (E.param (E.nonNullable E.text)) <>
      contramap snd (E.param (E.nonNullable E.text))
    decoder = D.rowList $ AuditLogRow
      <$> D.column (D.nonNullable D.text)
      <*> D.column (D.nonNullable D.text)
      <*> D.column (D.nonNullable D.text)
      <*> D.column (D.nullable D.uuid)
      <*> D.column (D.nullable D.text)
      <*> D.column (D.nullable D.text)
      <*> D.column (D.nullable D.text)
      <*> D.column (D.nullable D.text)
      <*> D.column (D.nullable D.jsonb)
      <*> D.column (D.nullable D.jsonb)

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
                  , wsGhOwner   = lit (Nothing :: Maybe Text)
                  , wsGhRepo    = lit (Nothing :: Maybe Text)
                  , wsType      = lit WsRepository
                  , wsDeletedAt = unsafeDefault
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
  , epLogFile :: FilePath
  , epDbName  :: Text
  }

-- | Bracket that starts an isolated sandboxed PostgreSQL instance, runs the
-- inner action, then guarantees teardown.  Legacy @HMEM_TEST_DB@ is scrubbed
-- from the test environment rather than accepted as a silent external DB
-- escape hatch.
--
-- Designed for use with hspec's @aroundAll_@ in a @SpecHook@ module:
--
-- @
-- hook :: Spec -> Spec
-- hook = aroundAll_ withEphemeralPg
-- @
withEphemeralPg :: IO () -> IO ()
withEphemeralPg action =
  withTestSandbox $ \sandbox ->
    withSandboxedEnv sandbox $ 
      withSandboxedPostgres sandbox $ \db ->
        withActiveSandboxEnv sandbox db action

withSandboxedTestEnv :: (TestEnv -> IO a) -> IO a
withSandboxedTestEnv action =
  withTestSandbox $ \sandbox ->
    withSandboxedEnv sandbox $
      withSandboxedPostgres sandbox $ \db ->
        withActiveSandboxEnv sandbox db $
          withPooledTestEnv sandbox db 10 action

withTestSandbox :: (TestSandbox -> IO a) -> IO a
withTestSandbox action = do
  preserve <- envFlag preserveSandboxVar
  tmpBase <- getCanonicalTemporaryDirectory
  root <- createTempDirectory tmpBase "hmem-sandbox"
  let tmpDir = root </> "tmp"
      logDir = root </> "logs"
      configDir = root </> "config"
      staticDir = root </> "static"
      cacheDir = root </> "cache"
      homeDir = root </> "home"
  result <- try $ do
    forM_ [tmpDir, logDir, configDir, staticDir, cacheDir, homeDir] $
      createDirectoryIfMissing True
    repoRoot <- resolveRepoRoot
    migrationsDir <- resolveMigrationsDir repoRoot
    let sandbox = TestSandbox
          { sandboxRoot = root
          , sandboxTmpDir = tmpDir
          , sandboxLogDir = logDir
          , sandboxConfigDir = configDir
          , sandboxStaticDir = staticDir
          , sandboxCacheDir = cacheDir
          , sandboxHomeDir = homeDir
          , sandboxRepoRoot = repoRoot
          , sandboxMigrationsDir = migrationsDir
          , sandboxPreserveOnFailure = preserve
          }
    value <- action sandbox
    pure (sandbox, value)
  case result of
    Right (sandbox, value) -> do
      cleanupSandbox sandbox True
      pure value
    Left (err :: SomeException) -> do
      cleanupSandboxRoot root preserve
      throwIO err

withSandboxedEnv :: TestSandbox -> IO a -> IO a
withSandboxedEnv sandbox action =
  bracket snapshotEnv restoreEnv $ \_ -> do
    legacy <- lookupEnv legacyTestDbVar
    when (isJust legacy) $
      hPutStrLn stderr $ "[test-sandbox] ignoring legacy " <> legacyTestDbVar <> " in favor of sandboxed PostgreSQL"
    forM_ scrubbedEnvVars unsetEnv
    setEnv sandboxRootVar sandbox.sandboxRoot
    setEnv sandboxTmpVar sandbox.sandboxTmpDir
    setEnv sandboxLogVar sandbox.sandboxLogDir
    setEnv sandboxConfigVar sandbox.sandboxConfigDir
    setEnv sandboxStaticVar sandbox.sandboxStaticDir
    setEnv sandboxCacheVar sandbox.sandboxCacheDir
    setEnv sandboxHomeVar sandbox.sandboxHomeDir
    setEnv sandboxRepoVar sandbox.sandboxRepoRoot
    setEnv sandboxMigrationsVar sandbox.sandboxMigrationsDir
    setEnv sandboxActiveVar "1"
    setEnv "HOME" sandbox.sandboxHomeDir
    setEnv "USERPROFILE" sandbox.sandboxHomeDir
    setEnv "APPDATA" (sandbox.sandboxHomeDir </> "AppData" </> "Roaming")
    setEnv "LOCALAPPDATA" (sandbox.sandboxHomeDir </> "AppData" </> "Local")
    setEnv "XDG_CONFIG_HOME" (sandbox.sandboxHomeDir </> ".config")
    action

withSandboxedPostgres :: TestSandbox -> (TestDb -> IO a) -> IO a
withSandboxedPostgres sandbox action = do
  checkPgTools
  bracket (startEphemeralPgInSandbox sandbox) stopEphemeralPgServer $ \pg -> do
    let db = TestDb
          { testDbDataDir = pg.epDataDir
          , testDbLogFile = pg.epLogFile
          , testDbPort = pg.epPort
          , testDbName = pg.epDbName
          , testDbConnStr = pg.epConnStr
          , testDbUnsafeExternal = False
          }
    action db

-- | Fail immediately when required PostgreSQL CLI tools are missing.
checkPgTools :: IO ()
checkPgTools = do
  let required = ["initdb", "pg_ctl", "createdb"]
  results <- mapM (\cmd -> (,) cmd <$> findExecutable cmd) required
  let missing = [cmd | (cmd, Nothing) <- results]
  case missing of
    [] -> pure ()
    _  -> fail $ unlines
            [ "Required PostgreSQL tools not on PATH for sandboxed tests: "
                ++ unwords missing
            , "Install PostgreSQL and ensure its bin/ is on PATH."
            ]

-- | Start an ephemeral PostgreSQL cluster on a random port.
startEphemeralPg :: IO EphemeralPg
startEphemeralPg = do
  sandbox <- createStandaloneSandbox
  startEphemeralPgInSandbox sandbox

startEphemeralPgInSandbox :: TestSandbox -> IO EphemeralPg
startEphemeralPgInSandbox sandbox = do
  port <- randomRIO (49152, 65535) :: IO Int
  suffix <- T.take 8 . T.filter (/= '-') . T.pack . show <$> UUID.nextRandom
  let dbName = "hmem_test_" <> suffix
      dataDir = sandbox.sandboxRoot </> "postgres" </> "data"
      logFile = sandbox.sandboxLogDir </> "postgresql.log"
      connStr = "host=localhost port=" <> T.pack (show port) <> " dbname=" <> dbName

  hPutStrLn stderr $ "[test-pg] sandbox : " ++ sandbox.sandboxRoot
  hPutStrLn stderr $ "[test-pg] port    : " ++ show port
  hPutStrLn stderr $ "[test-pg] db      : " ++ T.unpack dbName
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

  let pg = EphemeralPg
        { epTmpDir  = sandbox.sandboxRoot
        , epDataDir = dataDir
        , epPort    = port
        , epConnStr = connStr
        , epLogFile = logFile
        , epDbName  = dbName
        }

  bracketOnError
    (do
      callProcess "pg_ctl"
        [ "start", "-D", dataDir, "-l", logFile, "-w", "-t", "30" ]
      pure pg)
    stopEphemeralPgServer
    (\started -> do
      threadDelay 500000

      -- Create the test database
      callProcess "createdb"
        [ "-h", "localhost", "-p", show port, T.unpack dbName ]

      hPutStrLn stderr "[test-pg] PostgreSQL ready."
      hFlush stderr

      pure started)

-- | Stop the ephemeral PostgreSQL cluster and remove its temp
-- directory.  Ignores errors so teardown always completes.
stopEphemeralPg :: EphemeralPg -> IO ()
stopEphemeralPg pg = do
  stopEphemeralPgServer pg
  _ <- try (removeDirectoryRecursive pg.epTmpDir
        ) :: IO (Either SomeException ())
  pure ()

stopEphemeralPgServer :: EphemeralPg -> IO ()
stopEphemeralPgServer pg = do
  hPutStrLn stderr "[test-pg] Tearing down..."
  hFlush stderr
  _ <- try (callProcess "pg_ctl"
        [ "stop", "-D", pg.epDataDir, "-m", "fast" ]
        ) :: IO (Either SomeException ())
  -- Small delay so Windows releases file locks
  threadDelay 500000
  pure ()

assertInSandbox :: TestSandbox -> FilePath -> IO ()
assertInSandbox sandbox path = do
  root <- canonicalizePath sandbox.sandboxRoot
  target <- canonicalizePath path
  unless (isPathWithin root target) $
    fail $ "Path is outside sandbox. root=" <> root <> " path=" <> target

------------------------------------------------------------------------
-- Internal sandbox helpers
------------------------------------------------------------------------

withPooledTestEnv :: TestSandbox -> TestDb -> Int -> (TestEnv -> IO a) -> IO a
withPooledTestEnv sandbox db poolSize action = do
  p <- createPool db.testDbConnStr poolSize 5.0 30000
  let env = TestEnv { pool = p, testSandbox = sandbox, testDb = db }
  ensureSchema env
  cleanDB env
  action env

currentSandboxFromEnv :: IO (Maybe (TestSandbox, TestDb))
currentSandboxFromEnv = do
  active <- lookupEnv sandboxActiveVar
  mRoot <- lookupEnv sandboxRootVar
  mConn <- lookupEnv sandboxDbVar
  case (active, mRoot, mConn) of
    (Just "1", Just root, Just connStr) -> do
      sandbox <- TestSandbox root
        <$> envOrFail sandboxTmpVar
        <*> envOrFail sandboxLogVar
        <*> envOrFail sandboxConfigVar
        <*> envOrFail sandboxStaticVar
        <*> envOrFail sandboxCacheVar
        <*> envOrFail sandboxHomeVar
        <*> envOrFail sandboxRepoVar
        <*> envOrFail sandboxMigrationsVar
        <*> envFlag preserveSandboxVar
      db <- TestDb
        <$> envOrFail sandboxDbDataVar
        <*> envOrFail sandboxDbLogVar
        <*> (read <$> envOrFail sandboxDbPortVar)
        <*> (T.pack <$> envOrFail sandboxDbNameVar)
        <*> pure (T.pack connStr)
        <*> pure False
      validateActiveSandbox sandbox db
      pure $ Just (sandbox, db)
    (Nothing, _, _) -> pure Nothing
    _ -> fail "Incomplete or untrusted active sandbox metadata in environment"

requireCurrentSandbox :: IO (TestSandbox, TestDb)
requireCurrentSandbox = do
  active <- currentSandboxFromEnv
  case active of
    Just value -> pure value
    Nothing -> fail "No active sandbox metadata found in environment"

withActiveSandboxEnv :: TestSandbox -> TestDb -> IO a -> IO a
withActiveSandboxEnv _sandbox db action = do
  setEnv sandboxActiveVar "1"
  setEnv sandboxDbVar (T.unpack db.testDbConnStr)
  setEnv sandboxDbDataVar db.testDbDataDir
  setEnv sandboxDbLogVar db.testDbLogFile
  setEnv sandboxDbPortVar (show db.testDbPort)
  setEnv sandboxDbNameVar (T.unpack db.testDbName)
  action

withConfiguredExternalOrSandboxedTestEnv :: (TestEnv -> IO a) -> IO a
withConfiguredExternalOrSandboxedTestEnv action = do
  external <- lookupEnv externalTestDbVar
  allowExternal <- envFlag allowExternalTestDbVar
  ci <- envFlag "CI"
  case external of
    Nothing
      | allowExternal -> fail $ allowExternalTestDbVar <> " requires " <> externalTestDbVar
      | otherwise -> withSandboxedTestEnv action
    Just connStr
      | not allowExternal -> fail $ "Refusing " <> externalTestDbVar <> " without " <> allowExternalTestDbVar <> "=1"
      | ci -> fail "Refusing unsafe external test database mode in CI"
      | otherwise -> withUnsafeExternalTestEnv (T.pack connStr) action

withUnsafeExternalTestEnv :: Text -> (TestEnv -> IO a) -> IO a
withUnsafeExternalTestEnv connStr action =
  withTestSandbox $ \sandbox ->
    withSandboxedEnv sandbox $ do
      let externalDir = sandbox.sandboxRoot </> "external-db-not-managed"
          externalLog = sandbox.sandboxLogDir </> "external-db-not-managed.log"
          db = TestDb
            { testDbDataDir = externalDir
            , testDbLogFile = externalLog
            , testDbPort = 0
            , testDbName = "external"
            , testDbConnStr = connStr
            , testDbUnsafeExternal = True
            }
      createDirectoryIfMissing True externalDir
      writeFile externalLog "[test-sandbox] unsafe external database mode; PostgreSQL is not managed by harness\n"
      hPutStrLn stderr $ "[test-sandbox] UNSAFE external test database enabled by " <> allowExternalTestDbVar
      withActiveSandboxEnv sandbox db $
        withPooledTestEnv sandbox db 10 action

createStandaloneSandbox :: IO TestSandbox
createStandaloneSandbox = do
  preserve <- envFlag preserveSandboxVar
  tmpBase <- getCanonicalTemporaryDirectory
  root <- createTempDirectory tmpBase "hmem-sandbox"
  let tmpDir = root </> "tmp"
      logDir = root </> "logs"
      configDir = root </> "config"
      staticDir = root </> "static"
      cacheDir = root </> "cache"
      homeDir = root </> "home"
  forM_ [tmpDir, logDir, configDir, staticDir, cacheDir, homeDir] $
    createDirectoryIfMissing True
  repoRoot <- resolveRepoRoot
  migrationsDir <- resolveMigrationsDir repoRoot
  pure TestSandbox
    { sandboxRoot = root
    , sandboxTmpDir = tmpDir
    , sandboxLogDir = logDir
    , sandboxConfigDir = configDir
    , sandboxStaticDir = staticDir
    , sandboxCacheDir = cacheDir
    , sandboxHomeDir = homeDir
    , sandboxRepoRoot = repoRoot
    , sandboxMigrationsDir = migrationsDir
    , sandboxPreserveOnFailure = preserve
    }

cleanupSandbox :: TestSandbox -> Bool -> IO ()
cleanupSandbox sandbox success
  | sandbox.sandboxPreserveOnFailure && not success =
      hPutStrLn stderr $ "[test-sandbox] preserved after failure: " <> sandbox.sandboxRoot
  | otherwise = cleanupSandboxRoot sandbox.sandboxRoot False

cleanupSandboxRoot :: FilePath -> Bool -> IO ()
cleanupSandboxRoot root preserve
  | preserve = hPutStrLn stderr $ "[test-sandbox] preserved after failure: " <> root
  | otherwise = do
      _ <- try (removeDirectoryRecursive root) :: IO (Either SomeException ())
      pure ()

snapshotEnv :: IO [(String, Maybe String)]
snapshotEnv = mapM (\name -> do value <- lookupEnv name; pure (name, value)) sandboxManagedEnvVars

restoreEnv :: [(String, Maybe String)] -> IO ()
restoreEnv snapshot =
  forM_ snapshot $ \(name, value) -> case value of
    Nothing -> unsetEnv name
    Just raw -> setEnv name raw

envOrFail :: String -> IO String
envOrFail name = do
  value <- lookupEnv name
  case value of
    Just raw -> pure raw
    Nothing -> fail $ "Missing sandbox environment variable: " <> name

envFlag :: String -> IO Bool
envFlag name = do
  value <- lookupEnv name
  pure $ value `elem` [Just "1", Just "true", Just "TRUE", Just "yes", Just "YES"]

validateActiveSandbox :: TestSandbox -> TestDb -> IO ()
validateActiveSandbox sandbox db = do
  tempRoot <- getCanonicalTemporaryDirectory >>= canonicalizePath
  root <- canonicalizePath sandbox.sandboxRoot
  unless (isPathWithin tempRoot root && "hmem-sandbox" `prefixOfString` takeFileName root) $
    fail $ "Active sandbox root is not an hmem temp sandbox: " <> root
  forM_
    [ sandbox.sandboxTmpDir
    , sandbox.sandboxLogDir
    , sandbox.sandboxConfigDir
    , sandbox.sandboxStaticDir
    , sandbox.sandboxCacheDir
    , sandbox.sandboxHomeDir
    , db.testDbDataDir
    , db.testDbLogFile
    ] $
    assertInSandbox sandbox
  unless ("hmem_test_" `T.isPrefixOf` db.testDbName) $
    fail $ "Active sandbox DB name is not harness-generated: " <> T.unpack db.testDbName
  let expectedConnStr = "host=localhost port=" <> T.pack (show db.testDbPort) <> " dbname=" <> db.testDbName
  unless (db.testDbConnStr == expectedConnStr) $
    fail "Active sandbox DB connection string does not match sandbox metadata"

isPathWithin :: FilePath -> FilePath -> Bool
isPathWithin rawRoot rawTarget =
  let root = addTrailingSeparator (normalise rawRoot)
      target = normalise rawTarget
  in target == normalise rawRoot || root `prefixOfPath` target

addTrailingSeparator :: FilePath -> FilePath
addTrailingSeparator path
  | Foldable.null path = path
  | isPathSeparator (last path) = path
  | otherwise = path <> [pathSeparator]
  where
    pathSeparator = if any (== '\\') path then '\\' else '/'

prefixOfPath :: FilePath -> FilePath -> Bool
prefixOfPath [] _ = True
prefixOfPath _ [] = False
prefixOfPath (x:xs) (y:ys) = x == y && prefixOfPath xs ys

prefixOfString :: String -> String -> Bool
prefixOfString [] _ = True
prefixOfString _ [] = False
prefixOfString (x:xs) (y:ys) = x == y && prefixOfString xs ys

sandboxActiveVar, sandboxDbVar, legacyTestDbVar, preserveSandboxVar, externalTestDbVar, allowExternalTestDbVar, repoRootOverrideVar :: String
sandboxActiveVar = "HMEM_TEST_SANDBOX_ACTIVE"
sandboxDbVar = "HMEM_TEST_SANDBOX_DB"
legacyTestDbVar = "HMEM_TEST_DB"
preserveSandboxVar = "HMEM_TEST_PRESERVE_SANDBOX"
externalTestDbVar = "HMEM_TEST_EXTERNAL_DB"
allowExternalTestDbVar = "HMEM_TEST_ALLOW_EXTERNAL_DB"
repoRootOverrideVar = "HMEM_TEST_REPO_ROOT"

sandboxRootVar, sandboxTmpVar, sandboxLogVar, sandboxConfigVar, sandboxStaticVar, sandboxCacheVar, sandboxHomeVar, sandboxRepoVar, sandboxMigrationsVar :: String
sandboxRootVar = "HMEM_TEST_SANDBOX_ROOT"
sandboxTmpVar = "HMEM_TEST_SANDBOX_TMP"
sandboxLogVar = "HMEM_TEST_SANDBOX_LOGS"
sandboxConfigVar = "HMEM_TEST_SANDBOX_CONFIG"
sandboxStaticVar = "HMEM_TEST_SANDBOX_STATIC"
sandboxCacheVar = "HMEM_TEST_SANDBOX_CACHE"
sandboxHomeVar = "HMEM_TEST_SANDBOX_HOME"
sandboxRepoVar = "HMEM_TEST_SANDBOX_REPO_ROOT"
sandboxMigrationsVar = "HMEM_TEST_SANDBOX_MIGRATIONS"

sandboxDbDataVar, sandboxDbLogVar, sandboxDbPortVar, sandboxDbNameVar :: String
sandboxDbDataVar = "HMEM_TEST_SANDBOX_DB_DATA"
sandboxDbLogVar = "HMEM_TEST_SANDBOX_DB_LOG"
sandboxDbPortVar = "HMEM_TEST_SANDBOX_DB_PORT"
sandboxDbNameVar = "HMEM_TEST_SANDBOX_DB_NAME"

scrubbedEnvVars :: [String]
scrubbedEnvVars =
  [ legacyTestDbVar
  , externalTestDbVar
  , allowExternalTestDbVar
  , "HMEM_DB_PASSWORD"
  , "HMEM_API_KEY"
  , "HMEM_DB_SSLMODE"
  , "HMEM_AUTH_TOKEN"
  , "HMEM_MCP_AUTH_TOKEN"
  , "HMEM_SERVER_URL"
  , "KEYCLOAK_URL"
  , "KEYCLOAK_REALM"
  , "KEYCLOAK_CLIENT_SECRET"
  ]

sandboxManagedEnvVars :: [String]
sandboxManagedEnvVars = scrubbedEnvVars <>
  [ sandboxActiveVar
  , sandboxDbVar
  , sandboxRootVar
  , sandboxTmpVar
  , sandboxLogVar
  , sandboxConfigVar
  , sandboxStaticVar
  , sandboxCacheVar
  , sandboxHomeVar
  , sandboxRepoVar
  , sandboxMigrationsVar
  , sandboxDbDataVar
  , sandboxDbLogVar
  , sandboxDbPortVar
  , sandboxDbNameVar
  , "HOME"
  , "USERPROFILE"
  , "APPDATA"
  , "LOCALAPPDATA"
  , "XDG_CONFIG_HOME"
  ]
