{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module HMem.DB.TestHarness
  ( -- * Test environment
    TestEnv(..)
  , TestSandbox(..)
  , TestDb(..)
  , TestPostgresBackend(..)
  , readTestPostgresBackend
  , managedLinuxImageReference
  , managedLinuxIndexDigest
  , managedLinuxAmd64ManifestDigest
  , managedLinuxConfigDigest
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
import Control.Concurrent.Async (Async, wait, waitCatch, withAsync)
import Control.Exception (SomeException, bracket, bracketOnError, finally, mask, onException, throwIO, try)
import Control.Exception qualified as Exception
import Control.Monad (forM_, unless, void, when)
import Data.Char (isHexDigit)
import Data.Aeson (Value)
import Data.Functor.Contravariant (contramap)
import Data.Foldable qualified as Foldable
import Data.List (intercalate, isInfixOf)
import Data.List qualified as List
import Data.Maybe (catMaybes, isJust)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Pool (Pool)
import Data.Text (Text)
import Data.Text qualified as T
import Data.UUID (UUID)
import Data.UUID.V4 qualified as UUID
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import System.Directory (canonicalizePath, createDirectoryIfMissing, doesDirectoryExist, doesFileExist,
                         findExecutable, getCurrentDirectory, removeDirectoryRecursive)
import System.Environment (getEnvironment, getExecutablePath, lookupEnv, setEnv, unsetEnv)
import System.Exit (ExitCode(..))
import System.FilePath ((</>), isAbsolute, isPathSeparator, normalise, takeDirectory, takeFileName)
import System.IO (Handle, IOMode(AppendMode), hClose, hFlush, hGetContents, hPutStrLn, stderr, withFile)
import System.IO.Temp (createTempDirectory, getCanonicalTemporaryDirectory)
import System.Info (os)
import System.Process (CreateProcess(..), ProcessHandle, StdStream(CreatePipe, NoStream, UseHandle), callProcess, createProcess, getPid, getProcessExitCode, proc, terminateProcess, waitForProcess)
import System.Random (randomRIO)
import System.Timeout (timeout)
import Text.Read (readMaybe)
import Prelude hiding (min)
import Prelude qualified
#if !defined(mingw32_HOST_OS)
import Foreign.C.Error (throwErrnoIfMinus1_)
import Foreign.C.Types (CInt(..))
#endif
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

data TestPostgresBackend
  = TestPostgresNative
  | TestPostgresManagedLinux
  deriving stock (Show, Eq)

-- The repository digest identifies the immutable multi-platform index.  The
-- platform and config descriptors below were resolved from that index before
-- this managed test profile was admitted.
managedLinuxImageReference :: String
managedLinuxImageReference =
  "pgvector/pgvector@sha256:cf134a767f474095eeba57e0117be8e568e011a63f33fbf252f14c9b760f8e6f"

managedLinuxIndexDigest :: String
managedLinuxIndexDigest = "sha256:cf134a767f474095eeba57e0117be8e568e011a63f33fbf252f14c9b760f8e6f"

managedLinuxAmd64ManifestDigest :: String
managedLinuxAmd64ManifestDigest = "sha256:dca0d688bbb31d3f851502ffcb9c7791387b4fcc544ae434dab41761e5ece317"

managedLinuxConfigDigest :: String
managedLinuxConfigDigest = "sha256:17a06c0a60bf6fb548a8493b8f6057dc830b791bbbe1f1ab706ca4b9d97c8180"

data PreparedPostgresBackend
  = PreparedNative
  | PreparedManagedLinux ManagedDocker

data ManagedDocker = ManagedDocker
  { managedDockerExecutable :: FilePath
  , managedDockerEndpoint :: String
  , managedDockerImageId :: String
  , managedDockerDefaultRole :: String
  , managedDockerEnvironment :: [(String, String)]
  }

data CommandOutput = CommandOutput
  { commandStdout :: String
  , commandStderr :: String
  }

data ManagedLinuxPg = ManagedLinuxPg
  { managedPgDocker :: ManagedDocker
  , managedPgToken :: String
  , managedPgName :: String
  , managedPgContainerId :: String
  , managedPgMetadataDir :: FilePath
  , managedPgLogFile :: FilePath
  , managedPgPort :: Int
  , managedPgDbName :: Text
  }

readTestPostgresBackend :: IO TestPostgresBackend
readTestPostgresBackend = do
  selected <- lookupEnv postgresBackendVar
  case selected of
    Nothing -> pure TestPostgresNative
    Just "native" -> pure TestPostgresNative
    Just "managed-linux" -> pure TestPostgresManagedLinux
    Just other -> fail $ "Unknown " <> postgresBackendVar <> " value: " <> show other

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
      start <- try (canonicalizePath anchor) :: IO (Either SomeException FilePath)
      case start of
        Left _ -> pure Nothing
        Right dir -> go dir

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
      sql = mconcat
        [ "TRUNCATE audit_log, access_tokens, workspace_memberships, users, "
        , "workspace_group_members, workspace_groups, delete_cascade_migration_report, "
        , "task_dependencies, tasks, projects, observations, workspaces CASCADE"
        ]
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
    sql = mconcat
      [ "SELECT entity_type, entity_id, action::text, workspace_id, actor_type::text, "
      , "actor_id, actor_label, request_id, old_values, new_values FROM audit_log "
      , "WHERE entity_type = $1 AND entity_id = $2 ORDER BY changed_at ASC, id ASC"
      ]
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
  , epProcess :: Maybe ProcessHandle
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
withSandboxedEnv sandbox action = do
  prepared <- preparePostgresBackend sandbox
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
    setEnv repoRootOverrideVar sandbox.sandboxRepoRoot
    setEnv sandboxActiveVar "1"
    setEnv "HOME" sandbox.sandboxHomeDir
    setEnv "USERPROFILE" sandbox.sandboxHomeDir
    setEnv "APPDATA" (sandbox.sandboxHomeDir </> "AppData" </> "Roaming")
    setEnv "LOCALAPPDATA" (sandbox.sandboxHomeDir </> "AppData" </> "Local")
    setEnv "XDG_CONFIG_HOME" (sandbox.sandboxHomeDir </> ".config")
    installPreparedPostgresBackend sandbox prepared
    action

withSandboxedPostgres :: TestSandbox -> (TestDb -> IO a) -> IO a
withSandboxedPostgres sandbox action = do
  prepared <- preparedPostgresBackendFromEnv sandbox
  case prepared of
    PreparedNative -> do
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
    PreparedManagedLinux docker ->
      bracket (startManagedLinuxPg sandbox docker) stopManagedLinuxPg $ \pg ->
        action TestDb
          { testDbDataDir = pg.managedPgMetadataDir
          , testDbLogFile = pg.managedPgLogFile
          , testDbPort = pg.managedPgPort
          , testDbName = pg.managedPgDbName
          , testDbConnStr = "host=localhost port=" <> T.pack (show pg.managedPgPort)
              <> " dbname=" <> pg.managedPgDbName
          , testDbUnsafeExternal = False
          }

-- | Fail immediately when required PostgreSQL CLI tools are missing.
checkPgTools :: IO ()
checkPgTools = do
  let required = ["initdb", "pg_ctl", "createdb"] <> ["pg_isready" | os == "mingw32"]
  results <- mapM (\cmd -> (,) cmd <$> findExecutable cmd) required
  let missing = [cmd | (cmd, Nothing) <- results]
  case missing of
    [] -> pure ()
    _  -> fail $ unlines
            [ "Required PostgreSQL tools not on PATH for sandboxed tests: "
                ++ unwords missing
             , "Install PostgreSQL and ensure its bin/ is on PATH."
             ]

preparePostgresBackend :: TestSandbox -> IO PreparedPostgresBackend
preparePostgresBackend sandbox = do
  carried <- lookupEnv preparedBackendVar
  case carried of
    Just "native" -> validatePreparedSelection TestPostgresNative >> pure PreparedNative
    Just "managed-linux" -> do
      validatePreparedSelection TestPostgresManagedLinux
      PreparedManagedLinux <$> managedDockerFromEnv sandbox
    Just other -> fail $ "Untrusted prepared PostgreSQL backend: " <> show other
    Nothing -> do
      rejectOrphanedPreparedBackend
      selected <- readTestPostgresBackend
      case selected of
        TestPostgresNative -> pure PreparedNative
        TestPostgresManagedLinux -> PreparedManagedLinux <$> resolveManagedDocker sandbox

preparedPostgresBackendFromEnv :: TestSandbox -> IO PreparedPostgresBackend
preparedPostgresBackendFromEnv sandbox = do
  carried <- lookupEnv preparedBackendVar
  case carried of
    Nothing -> preparePostgresBackend sandbox
    Just "native" -> validatePreparedSelection TestPostgresNative >> pure PreparedNative
    Just "managed-linux" -> do
      validatePreparedSelection TestPostgresManagedLinux
      PreparedManagedLinux <$> managedDockerFromEnv sandbox
    Just other -> fail $ "Untrusted prepared PostgreSQL backend: " <> show other

installPreparedPostgresBackend :: TestSandbox -> PreparedPostgresBackend -> IO ()
installPreparedPostgresBackend sandbox = \case
  PreparedNative -> do
    setEnv postgresBackendVar "native"
    setEnv preparedBackendVar "native"
    setEnv preparedOwnerRootVar sandbox.sandboxRoot
  PreparedManagedLinux docker -> do
    forM_ managedLibpqEnvVars unsetEnv
    setEnv postgresBackendVar "managed-linux"
    setEnv preparedBackendVar "managed-linux"
    setEnv preparedOwnerRootVar sandbox.sandboxRoot
    setEnv preparedDockerExeVar docker.managedDockerExecutable
    setEnv preparedDockerEndpointVar docker.managedDockerEndpoint
    setEnv preparedDockerImageIdVar docker.managedDockerImageId
    setEnv preparedDockerRoleVar docker.managedDockerDefaultRole

validatePreparedSelection :: TestPostgresBackend -> IO ()
validatePreparedSelection expected = do
  active <- lookupEnv sandboxActiveVar
  unless (active == Just "1") $
    fail "Refusing prepared PostgreSQL backend outside an active sandbox"
  ownerRoot <- envOrFail preparedOwnerRootVar >>= canonicalizePath
  activeRoot <- envOrFail sandboxRootVar >>= canonicalizePath
  unless (ownerRoot == activeRoot) $
    fail "Prepared PostgreSQL backend owner does not match the active sandbox"
  tempRoot <- getCanonicalTemporaryDirectory >>= canonicalizePath
  unless (isPathWithin tempRoot activeRoot && "hmem-sandbox" `prefixOfString` takeFileName activeRoot) $
    fail "Prepared PostgreSQL backend owner is not an active hmem temp sandbox"
  ownedDirectories <- mapM envOrFail
    [ sandboxTmpVar, sandboxLogVar, sandboxConfigVar, sandboxStaticVar
    , sandboxCacheVar, sandboxHomeVar, sandboxRepoVar, sandboxMigrationsVar
    ]
  let privateDirectories = take 6 ownedDirectories
  privateDirectoryStates <- mapM doesDirectoryExist privateDirectories
  unless (Foldable.and privateDirectoryStates && Foldable.all (isPathWithin activeRoot . normalise) privateDirectories) $
    fail "Prepared PostgreSQL backend owner metadata is not a live sandbox"
  case drop 6 ownedDirectories of
    [repoRoot, migrationsRoot] -> do
      canonicalRepo <- canonicalizePath repoRoot
      canonicalMigrations <- canonicalizePath migrationsRoot
      expectedRepo <- resolveRepoRoot
      expectedMigrations <- resolveMigrationsDir expectedRepo
      unless (canonicalRepo == expectedRepo && canonicalMigrations == expectedMigrations) $
        fail "Prepared PostgreSQL backend repository metadata changed"
    _ -> fail "Prepared PostgreSQL backend owner metadata is incomplete"
  selected <- readTestPostgresBackend
  unless (selected == expected) $
    fail "Explicit PostgreSQL backend selection conflicts with prepared sandbox backend"

rejectOrphanedPreparedBackend :: IO ()
rejectOrphanedPreparedBackend = do
  values <- mapM lookupEnv
    [ preparedOwnerRootVar
    , preparedDockerExeVar
    , preparedDockerEndpointVar
    , preparedDockerImageIdVar
    , preparedDockerRoleVar
    ]
  when (any isJust values) $
    fail "Refusing orphaned prepared PostgreSQL backend metadata"

managedDockerFromEnv :: TestSandbox -> IO ManagedDocker
managedDockerFromEnv sandbox = do
  executable <- envOrFail preparedDockerExeVar
  endpoint <- envOrFail preparedDockerEndpointVar
  imageId <- envOrFail preparedDockerImageIdVar
  defaultRole <- envOrFail preparedDockerRoleVar
  unless (isAbsolute executable) $
    fail "Prepared managed Docker executable is not absolute"
  executableExists <- doesFileExist executable
  unless executableExists $ fail "Prepared managed Docker executable no longer exists"
  canonicalExecutable <- canonicalizePath executable
  unless (canonicalExecutable == executable) $
    fail "Prepared managed Docker executable is not canonical"
  validateLocalDockerEndpoint endpoint
  unless (imageId `elem` [managedLinuxIndexDigest, managedLinuxConfigDigest]) $
    fail "Prepared managed Docker image ID does not match the pinned authority"
  validateDatabaseRole defaultRole
  environment <- controlledDockerEnvironment sandbox endpoint
  pure $ ManagedDocker executable endpoint imageId defaultRole environment

resolveManagedDocker :: TestSandbox -> IO ManagedDocker
resolveManagedDocker sandbox = do
  executable <- findExecutable "docker" >>= \case
    Nothing -> fail $ postgresBackendVar <> "=managed-linux requires Docker on PATH"
    Just path -> canonicalizePath path
  discoveryEnvironment <- dockerDiscoveryEnvironment
  configuredContext <- lookupEnv "DOCKER_CONTEXT"
  configuredHost <- lookupEnv "DOCKER_HOST"
  endpoint <- case configuredContext of
    Just context | not (Foldable.null context) -> resolveContextEndpoint discoveryEnvironment executable context
    _ -> case configuredHost of
      Just host | not (Foldable.null host) -> validateLocalDockerEndpoint host >> pure host
      _ -> do
        context <- trim <$> runCommand discoveryEnvironment executable 10 ["context", "show"]
        when (Foldable.null context) $ fail "Docker reported an empty current context"
        resolveContextEndpoint discoveryEnvironment executable context
  defaultRole <- resolveDefaultDatabaseRole
  environment <- controlledDockerEnvironment sandbox endpoint
  let docker = ManagedDocker executable endpoint "" defaultRole environment
  imageDetails <- trim <$> runDocker docker 10
    ["image", "inspect", managedLinuxImageReference, "--format"
    , "{{.Id}}|{{.Os}}|{{.Architecture}}|{{json .RepoDigests}}"
    ]
  case splitOn '|' imageDetails of
    [imageId, imageOs, imageArchitecture, repoDigests] -> do
      unless (imageOs == "linux" && imageArchitecture == "amd64") $
        fail $ "Managed PostgreSQL image platform mismatch: " <> imageOs <> "/" <> imageArchitecture
      unless (("@" <> managedLinuxIndexDigest) `isInfixOf` repoDigests) $
        fail "Managed PostgreSQL image repository digest does not match the pinned index"
      unless (imageId `elem` [managedLinuxIndexDigest, managedLinuxConfigDigest]) $
        fail $ "Managed PostgreSQL image ID does not match the pinned image authority: " <> imageId
      pure docker { managedDockerImageId = imageId }
    _ -> fail $ "Unexpected Docker image inspection output: " <> imageDetails

validateLocalDockerEndpoint :: String -> IO ()
validateLocalDockerEndpoint endpoint =
  unless ("npipe:////./pipe/" `prefixOfString` endpoint || "unix://" `prefixOfString` endpoint) $
    fail $ "Refusing non-local Docker endpoint for managed tests: " <> endpoint

validateDatabaseRole :: String -> IO ()
validateDatabaseRole value =
  unless (not (Foldable.null value) && length value <= 63 && '\NUL' `notElem` value) $
    fail "Prepared local database role name is not valid for managed tests"

controlledDockerEnvironment :: TestSandbox -> String -> IO [(String, String)]
controlledDockerEnvironment sandbox endpoint = do
  let configDir = sandbox.sandboxConfigDir </> "docker"
  createDirectoryIfMissing True configDir
  ambient <- getEnvironment
  pure $
    [ ("DOCKER_HOST", endpoint)
    , ("DOCKER_CONFIG", configDir)
    , ("HOME", sandbox.sandboxHomeDir)
    , ("USERPROFILE", sandbox.sandboxHomeDir)
    ] <> List.filter (allowedManagedCommandEnv . fst) ambient

dockerDiscoveryEnvironment :: IO [(String, String)]
dockerDiscoveryEnvironment = do
  ambient <- getEnvironment
  pure $ List.filter (allowedDockerDiscoveryEnv . fst) ambient

allowedManagedCommandEnv :: String -> Bool
allowedManagedCommandEnv name = normalized `elem`
  [ "PATH", "PATHEXT", "SYSTEMROOT", "WINDIR", "COMSPEC", "TEMP", "TMP", "TMPDIR"
  , "LANG", "LC_ALL", "TERM"
  ]
  where
    normalized = map toUpperAscii name

allowedDockerDiscoveryEnv :: String -> Bool
allowedDockerDiscoveryEnv name =
  allowedManagedCommandEnv name || normalized `elem`
    [ "DOCKER_CONTEXT", "DOCKER_CONFIG", "HOME", "USERPROFILE" ]
  where
    normalized = map toUpperAscii name

toUpperAscii :: Char -> Char
toUpperAscii char
  | char >= 'a' && char <= 'z' = toEnum (fromEnum char - 32)
  | otherwise = char

resolveContextEndpoint :: [(String, String)] -> FilePath -> String -> IO String
resolveContextEndpoint environment executable context = do
  resolved <- trim <$> runCommand environment executable 10
    ["context", "inspect", context, "--format", "{{.Endpoints.docker.Host}}"]
  validateLocalDockerEndpoint resolved
  pure resolved

startManagedLinuxPg :: TestSandbox -> ManagedDocker -> IO ManagedLinuxPg
startManagedLinuxPg sandbox docker = do
  uuid <- T.unpack . T.filter (/= '-') . T.pack . show <$> UUID.nextRandom
  let suffix = take 12 uuid
      token = "hmem-test-" <> uuid
      containerName = "hmem-test-" <> suffix
      dbName = T.pack ("hmem_test_" <> suffix)
      metadataDir = sandbox.sandboxRoot </> ("managed-postgres-" <> suffix)
      logFile = sandbox.sandboxLogDir </> (containerName <> ".log")
      cidFile = metadataDir </> "container.id"
      partialCleanup = cleanupPartialManagedLinuxPg docker token containerName cidFile logFile
  createDirectoryIfMissing True metadataDir
  (do
      outputId <- trim <$> runDocker docker 20
        [ "run", "--detach", "--sig-proxy=false", "--pull", "never", "--platform", "linux/amd64"
        , "--name", containerName, "--cidfile", cidFile
        , "--label", "hmem.test.owner=" <> token
        , "--publish", "127.0.0.1::5432/tcp"
        , "--cpus", "2", "--memory", "768m"
        , "--mount", "type=tmpfs,destination=/var/lib/postgresql/data,tmpfs-size=536870912"
        , "--env", "POSTGRES_HOST_AUTH_METHOD=trust"
        , "--env", "POSTGRES_USER=postgres"
        , "--env", "POSTGRES_DB=" <> T.unpack dbName
        , "--env", "POSTGRES_INITDB_ARGS=--no-locale -E UTF8"
        , managedLinuxImageReference
        ]
      faultEnabled "empty-cid-after-create" >>= \enabled -> when enabled $ do
        writeFile cidFile ""
        fail "Injected managed PostgreSQL interruption after an empty cidfile"
      faultEnabled "truncated-cid-after-create" >>= \enabled -> when enabled $ do
        writeFile cidFile (take 12 outputId)
        fail "Injected managed PostgreSQL interruption after a truncated cidfile"
      cidFileId <- trim <$> readFile cidFile
      unless (validContainerId outputId && outputId == cidFileId) $
        fail "Docker did not return one exact managed test container ID"
      validateManagedContainer docker True token containerName outputId
      faultEnabled "exit-before-readiness" >>= \enabled -> when enabled $ do
        void $ runDocker docker 10 ["stop", "--timeout", "1", outputId]
        fail "Injected managed PostgreSQL failure before readiness"
      portOutput <- trim <$> runDocker docker 10 ["port", outputId, "5432/tcp"]
      port <- parseLoopbackDockerPort portOutput
      let pg = ManagedLinuxPg docker token containerName outputId metadataDir logFile port dbName
      activeCommandFault <- Foldable.or <$> mapM faultEnabled
        [ "active-command"
        , "active-command-grace-expiry"
        , "active-command-immediate-exit"
        , "active-command-repeated-cancel"
        , "active-command-trace-failure"
        , "active-command-cleanup-trace-failure"
        , "active-command-timeout-trace-cancel"
        ]
      when activeCommandFault $ do
        writeFile (metadataDir </> "active-command.pending") outputId
        shortTimeout <- faultEnabled "active-command-timeout-trace-cancel"
        void $ runDocker docker (if shortTimeout then 1 else 60) ["exec", outputId, "sleep", "30"]
        fail "Injected managed PostgreSQL failure after active Docker command"
      ready <- timeout 30000000 (waitForManagedPostgres pg 80)
      unless (ready == Just True) $ fail "Managed PostgreSQL did not become ready within 30 seconds"
      initializeManagedPostgres pg
      writeManagedMetadata pg
      hPutStrLn stderr $ "[test-pg] managed Linux PostgreSQL ready: " <> containerName
      pure pg
    ) `onException` partialCleanup

waitForManagedPostgres :: ManagedLinuxPg -> Int -> IO Bool
waitForManagedPostgres _ 0 = pure False
waitForManagedPostgres pg attempts = do
  outcome <- tryRunDocker pg.managedPgDocker 5
    [ "exec", pg.managedPgContainerId, "pg_isready"
    , "-h", "127.0.0.1", "-U", "postgres", "-d", T.unpack pg.managedPgDbName
    ]
  case outcome of
    Right _ -> pure True
    Left _ -> threadDelay 250000 >> waitForManagedPostgres pg (attempts - 1)

initializeManagedPostgres :: ManagedLinuxPg -> IO ()
initializeManagedPostgres pg = do
  let execPsql sql = runDocker pg.managedPgDocker 10
        [ "exec", pg.managedPgContainerId, "psql", "-X", "--quiet", "-v", "ON_ERROR_STOP=1"
        , "-h", "127.0.0.1", "-U", "postgres", "-d", T.unpack pg.managedPgDbName, "-At", "-c", sql
        ]
  void $ execPsql "CREATE EXTENSION IF NOT EXISTS vector"
  let defaultRole = pg.managedPgDocker.managedDockerDefaultRole
  when (defaultRole /= "postgres") $
    void $ execPsql $ "CREATE ROLE " <> quotePostgresIdentifier defaultRole <> " WITH LOGIN SUPERUSER"
  identity <- lines <$> execPsql
    "SHOW server_version; SELECT extversion FROM pg_extension WHERE extname='vector'; SELECT version(); SHOW dynamic_shared_memory_type"
  case identity of
    [serverVersion, vectorVersion, build, sharedMemory] -> do
      unless ("17." `prefixOfString` serverVersion) $
        fail $ "Managed PostgreSQL server version mismatch: " <> serverVersion
      unless (vectorVersion == "0.8.6") $
        fail $ "Managed pgvector version mismatch: " <> vectorVersion
      unless ("linux" `isInfixOf` build) $
        fail $ "Managed PostgreSQL build is not Linux: " <> build
      unless (sharedMemory == "posix") $
        fail $ "Managed PostgreSQL shared-memory type mismatch: " <> sharedMemory
    _ -> fail $ "Unexpected managed PostgreSQL identity output: " <> show identity

writeManagedMetadata :: ManagedLinuxPg -> IO ()
writeManagedMetadata pg =
  writeFile (pg.managedPgMetadataDir </> "authority.txt") $ unlines
    [ "backend=managed-linux"
    , "image-reference=" <> managedLinuxImageReference
    , "index-digest=" <> managedLinuxIndexDigest
    , "amd64-manifest-digest=" <> managedLinuxAmd64ManifestDigest
    , "config-digest=" <> managedLinuxConfigDigest
    , "docker-image-id=" <> pg.managedPgDocker.managedDockerImageId
    , "container-id=" <> pg.managedPgContainerId
    , "owner-token=" <> pg.managedPgToken
    , "loopback-port=" <> show pg.managedPgPort
    ]

stopManagedLinuxPg :: ManagedLinuxPg -> IO ()
stopManagedLinuxPg pg = do
  validateManagedContainer pg.managedPgDocker False pg.managedPgToken pg.managedPgName pg.managedPgContainerId
  logResult <- try (captureManagedLogs pg.managedPgDocker pg.managedPgContainerId pg.managedPgLogFile)
    :: IO (Either SomeException ())
  stopResult <- tryRunDocker pg.managedPgDocker 10 ["stop", "--timeout", "5", pg.managedPgContainerId]
  removeResult <- tryRunDocker pg.managedPgDocker 10 ["rm", "--force", pg.managedPgContainerId]
  absenceResult <- managedContainerExists pg.managedPgDocker pg.managedPgContainerId
  case absenceResult of
    Right False -> pure ()
    Right True -> fail "Managed PostgreSQL container remained after bounded cleanup"
    Left err -> fail $ "Could not verify managed PostgreSQL container removal: " <> err
  case (stopResult, removeResult) of
    (Right _, Right _) -> case logResult of
      Right () -> pure ()
      Left err -> throwIO err
    (Left stopErr, Right _) -> fail $ "Managed PostgreSQL stop failed before successful forced removal: " <> stopErr
    (_, Left removeErr) -> fail $ "Managed PostgreSQL removal failed: " <> removeErr

cleanupPartialManagedLinuxPg :: ManagedDocker -> String -> String -> FilePath -> FilePath -> IO ()
cleanupPartialManagedLinuxPg docker token containerName cidFile logFile = do
  cidExists <- doesFileExist cidFile
  cidResult <- if cidExists
    then try (trim <$> readFile cidFile) :: IO (Either SomeException String)
    else pure (Right "")
  let candidate = case cidResult of
        Right cid | validContainerId cid -> cid
        _ -> containerName
  details <- tryRunDocker docker 5
    ["inspect", candidate, "--format", "{{index .Config.Labels \"hmem.test.owner\"}}|{{.Id}}"]
  case details of
    Right rawDetails -> case splitOn '|' (trim rawDetails) of
      [owner, exactId] | owner == token && validContainerId exactId -> do
        logResult <- try (captureManagedLogs docker exactId logFile) :: IO (Either SomeException ())
        removal <- tryRunDocker docker 10 ["rm", "--force", exactId]
        absence <- managedContainerExists docker exactId
        case absence of
          Right False -> pure ()
          Right True -> fail "Partially started managed PostgreSQL container remained after cleanup"
          Left err -> fail $ "Could not verify partial managed PostgreSQL cleanup: " <> err
        case removal of
          Left err -> fail $ "Partially started managed PostgreSQL container removal failed: " <> err
          Right _ -> case logResult of
            Right () -> pure ()
            Left err -> throwIO err
      _ -> fail "Refusing to remove partially started container with mismatched ownership"
    Left err
      | candidate == containerName && confirmedMissingContainerError err -> pure ()
      | otherwise -> fail $ "Could not inspect partially started managed PostgreSQL container: " <> err

validateManagedContainer :: ManagedDocker -> Bool -> String -> String -> String -> IO ()
validateManagedContainer docker requireRunning token expectedName containerId = do
  details <- trim <$> runDocker docker 10
    [ "inspect", containerId, "--format"
    , "{{.Id}}|{{index .Config.Labels \"hmem.test.owner\"}}|{{.Name}}|{{.State.Status}}|{{.Image}}|{{.Config.Image}}"
    ]
  case splitOn '|' details of
    [actualId, owner, rawName, state, imageId, imageReference] -> do
      unless (actualId == containerId && owner == token && rawName == "/" <> expectedName) $
        fail "Managed PostgreSQL container ownership metadata mismatch"
      when (requireRunning && state /= "running") $
        fail $ "Managed PostgreSQL container is not running: " <> state
      unless (imageId `elem` [managedLinuxIndexDigest, managedLinuxConfigDigest]) $
        fail $ "Managed PostgreSQL container image ID mismatch: " <> imageId
      unless (imageReference == managedLinuxImageReference) $
        fail $ "Managed PostgreSQL container image reference mismatch: " <> imageReference
    _ -> fail $ "Unexpected managed PostgreSQL container inspection output: " <> details

captureManagedLogs :: ManagedDocker -> String -> FilePath -> IO ()
captureManagedLogs docker containerId logFile = do
  faultEnabled "log-write" >>= \enabled -> when enabled $
    ioError (userError "Injected managed PostgreSQL log write failure")
  result <- tryRunDockerOutput docker 10 ["logs", containerId]
  case result of
    Right output -> writeFile logFile $ unlines
      [ "[container stdout]"
      , output.commandStdout
      , "[container stderr]"
      , output.commandStderr
      ]
    Left err -> writeFile logFile $ "[test-harness] docker logs failed: " <> err <> "\n"

managedContainerExists :: ManagedDocker -> String -> IO (Either String Bool)
managedContainerExists docker containerId = do
  result <- tryRunDocker docker 5
    ["ps", "--all", "--no-trunc", "--filter", "id=" <> containerId, "--format", "{{.ID}}"]
  uncertain <- faultEnabled "verification-uncertain"
  pure $ if uncertain
    then Left "Injected managed PostgreSQL removal-verification uncertainty"
    else fmap (elem containerId . lines . trim) result

parseLoopbackDockerPort :: String -> IO Int
parseLoopbackDockerPort output = case lines output of
  [binding] -> case dropPrefix "127.0.0.1:" binding >>= readMaybe of
    Just port | port > 0 && port <= 65535 -> pure port
    _ -> fail $ "Docker did not publish PostgreSQL on one valid loopback port: " <> output
  _ -> fail $ "Docker returned ambiguous PostgreSQL port bindings: " <> output

runDocker :: ManagedDocker -> Int -> [String] -> IO String
runDocker docker seconds args =
  runCommand docker.managedDockerEnvironment docker.managedDockerExecutable seconds
    (dockerHostArgs docker <> args)

tryRunDocker :: ManagedDocker -> Int -> [String] -> IO (Either String String)
tryRunDocker docker seconds args = fmap (fmap (\output -> output.commandStdout)) $
  tryRunDockerOutput docker seconds args

tryRunDockerOutput :: ManagedDocker -> Int -> [String] -> IO (Either String CommandOutput)
tryRunDockerOutput docker seconds args =
  tryRunCommand docker.managedDockerEnvironment docker.managedDockerExecutable seconds
    (dockerHostArgs docker <> args)

dockerHostArgs :: ManagedDocker -> [String]
dockerHostArgs docker = ["--host", docker.managedDockerEndpoint]

runCommand :: [(String, String)] -> FilePath -> Int -> [String] -> IO String
runCommand environment executable seconds args =
  tryRunCommand environment executable seconds args >>= \case
    Right output -> pure output.commandStdout
    Left err -> fail err

tryRunCommand :: [(String, String)] -> FilePath -> Int -> [String] -> IO (Either String CommandOutput)
tryRunCommand environment executable seconds args = mask $ \restore -> do
  (_, Just stdoutHandle, Just stderrHandle, process) <- createProcess (proc executable args)
    { env = Just environment
    , std_in = NoStream
    , std_out = CreatePipe
    , std_err = CreatePipe
    }
  cleanupComplete <- newIORef False
  let cleanupBeforeReaders commandPid = do
        processCleanup <- try (terminateAndReapProcess environment args commandPid process Nothing)
          :: IO (Either SomeException (Maybe SomeException))
        stdoutCleanup <- try (hClose stdoutHandle) :: IO (Either SomeException ())
        stderrCleanup <- try (hClose stderrHandle) :: IO (Either SomeException ())
        case processCleanup of
          Right remembered -> do
            writeIORef cleanupComplete True
            throwRememberedOrSynchronous remembered [stdoutCleanup, stderrCleanup]
          Left processErr -> throwIO processErr
  commandPid <- (maybe "unavailable" show <$> getPid process)
    `onException` cleanupBeforeReaders "unavailable"
  (do
      traceManagedCommand args commandPid "started"
      immediateExit <- faultEnabled "active-command-immediate-exit"
      when (immediateExit && "sleep" `elem` args && "30" `elem` args) $ do
        terminateProcess process
        void $ waitForProcess process
        fail "Injected managed command failure after immediate exit"
      withAsync (strictReadHandle stdoutHandle) $ \stdoutTask ->
        withAsync (strictReadHandle stderrHandle) $ \stderrTask -> do
          waited <- try (restore (waitForCommandExit process (seconds * 50)))
            :: IO (Either SomeException (Maybe ExitCode))
          case waited of
            Left primary -> do
              cleanup <- try (terminateAndReapCommand environment args commandPid process stdoutTask stderrTask)
                :: IO (Either SomeException (Maybe SomeException))
              when (either (const False) (const True) cleanup) $ writeIORef cleanupComplete True
              case cleanup of
                Left cleanupErr -> hPutStrLn stderr $
                  "[test-harness] managed command cleanup failed after interruption: " <> show cleanupErr
                Right _ -> pure ()
              throwPreferredException primary cleanup
            Right Nothing -> do
              cleanup <- try (terminateAndReapCommand environment args commandPid process stdoutTask stderrTask)
                :: IO (Either SomeException (Maybe SomeException))
              when (either (const False) (const True) cleanup) $ writeIORef cleanupComplete True
              case cleanup of
                Right (Just cleanupAsync) -> throwIO cleanupAsync
                _ -> pure $ Left $ "Command timed out after " <> show seconds <> " seconds: "
                  <> executable <> " " <> intercalate " " args
                  <> either (("\nCommand cleanup failed: " <>) . show) (const "") cleanup
            Right (Just exitCode) -> do
              _ <- waitForProcess process
              traceManagedCommand args commandPid "reaped"
              stdout <- wait stdoutTask
              stderrOutput <- wait stderrTask
              pure $ case exitCode of
                ExitSuccess -> Right $ CommandOutput stdout stderrOutput
                ExitFailure code -> Left $
                  "Command failed (exit " <> show code <> "): " <> executable <> " " <> intercalate " " args
                    <> "\n" <> stdout <> stderrOutput
    ) `onException` do
      complete <- readIORef cleanupComplete
      unless complete $ do
        cleanup <- try (cleanupBeforeReaders commandPid) :: IO (Either SomeException ())
        case cleanup of
          Left cleanupErr
            | isAsyncException cleanupErr -> throwIO cleanupErr
            | otherwise -> hPutStrLn stderr $
                "[test-harness] managed command acquisition cleanup failed: " <> show cleanupErr
          Right () -> pure ()

strictReadHandle :: Handle -> IO String
strictReadHandle handle = do
  contents <- hGetContents handle
  void $ Exception.evaluate (length contents)
  hClose handle
  pure contents

waitForCommandExit :: ProcessHandle -> Int -> IO (Maybe ExitCode)
waitForCommandExit process attempts = getProcessExitCode process >>= \case
  exited@(Just _) -> pure exited
  Nothing
    | attempts <= 0 -> pure Nothing
    | otherwise -> threadDelay 20000 >> waitForCommandExit process (attempts - 1)

waitForCommandExitUntil :: Word64 -> IO () -> ProcessHandle -> Maybe SomeException -> IO (Maybe ExitCode, Maybe SomeException)
waitForCommandExitUntil deadline onAsync process remembered = do
  (status, remembered') <- completeRememberingAsyncWith onAsync remembered (getProcessExitCode process)
  case status of
    Just _ -> pure (status, remembered')
    Nothing -> do
      now <- getMonotonicTimeNSec
      if now >= deadline
        then pure (Nothing, remembered')
        else do
          let remainingMicros = fromIntegral ((deadline - now + 999) `div` 1000)
          remembered'' <- interruptibleDelayRememberingAsync onAsync remembered' $
            Prelude.min 20000 remainingMicros
          waitForCommandExitUntil deadline onAsync process remembered''

delayRememberingAsync :: Word64 -> IO () -> Maybe SomeException -> IO (Maybe SomeException)
delayRememberingAsync deadline onAsync remembered = do
  now <- getMonotonicTimeNSec
  if now >= deadline
    then pure remembered
    else do
      let remainingMicros = fromIntegral ((deadline - now + 999) `div` 1000)
      remembered' <- interruptibleDelayRememberingAsync onAsync remembered $
        Prelude.min 20000 remainingMicros
      delayRememberingAsync deadline onAsync remembered'

interruptibleDelayRememberingAsync :: IO () -> Maybe SomeException -> Int -> IO (Maybe SomeException)
interruptibleDelayRememberingAsync onAsync remembered microseconds = try (threadDelay microseconds) >>= \case
  Right () -> pure remembered
  Left err
    | isAsyncException err -> do
        let remembered' = preferExceptions remembered [Left err]
        rememberDiagnosticException remembered' onAsync
    | otherwise -> throwIO err

completeRememberingAsync :: Maybe SomeException -> IO a -> IO (a, Maybe SomeException)
completeRememberingAsync = completeRememberingAsyncWith (pure ())

completeRememberingAsyncWith :: IO () -> Maybe SomeException -> IO a -> IO (a, Maybe SomeException)
completeRememberingAsyncWith onAsync remembered action = try action >>= \case
  Right value -> pure (value, remembered)
  Left err
    | isAsyncException err -> do
        let remembered' = preferExceptions remembered [Left err]
        remembered'' <- rememberDiagnosticException remembered' onAsync
        completeRememberingAsyncWith onAsync remembered'' action
    | otherwise -> throwIO err

rememberDiagnosticException :: Maybe SomeException -> IO () -> IO (Maybe SomeException)
rememberDiagnosticException remembered diagnostic =
  try diagnostic >>= \outcome -> pure $ preferExceptions remembered [outcome]

waitCatchRememberingAsync :: Maybe SomeException -> Async a -> IO (Either SomeException a, Maybe SomeException)
waitCatchRememberingAsync remembered task = completeRememberingAsync remembered (waitCatch task)

isAsyncException :: SomeException -> Bool
isAsyncException err = isJust (Exception.fromException err :: Maybe Exception.SomeAsyncException)

preferExceptions :: Maybe SomeException -> [Either SomeException a] -> Maybe SomeException
preferExceptions = foldl prefer
  where
    prefer current (Right _) = current
    prefer current (Left candidate)
      | maybe False isAsyncException current = current
      | isAsyncException candidate = Just candidate
      | otherwise = case current of
          Just _ -> current
          Nothing -> Just candidate

throwRememberedOrSynchronous :: Maybe SomeException -> [Either SomeException a] -> IO ()
throwRememberedOrSynchronous remembered outcomes =
  maybe (pure ()) throwIO (preferExceptions remembered outcomes)

throwPreferredException :: SomeException -> Either SomeException (Maybe SomeException) -> IO a
throwPreferredException primary cleanup
  | isAsyncException primary = throwIO primary
  | otherwise = case cleanup of
      Left cleanupErr | isAsyncException cleanupErr -> throwIO cleanupErr
      Right (Just cleanupErr) | isAsyncException cleanupErr -> throwIO cleanupErr
      _ -> throwIO primary

terminateAndReapCommand :: [(String, String)] -> [String] -> String -> ProcessHandle -> Async String -> Async String -> IO (Maybe SomeException)
terminateAndReapCommand environment args commandPid process stdoutTask stderrTask = do
  remembered <- terminateAndReapProcess environment args commandPid process Nothing
  (stdoutResult, remembered') <- waitCatchRememberingAsync remembered stdoutTask
  (stderrResult, remembered'') <- waitCatchRememberingAsync remembered' stderrTask
  remembered''' <- rememberDiagnosticException remembered'' $
    traceManagedCommand args commandPid "readers-joined"
  pure $ preferExceptions remembered''' [stdoutResult, stderrResult]

terminateAndReapProcess :: [(String, String)] -> [String] -> String -> ProcessHandle -> Maybe SomeException -> IO (Maybe SomeException)
terminateAndReapProcess environment args commandPid process remembered0 = do
  let noteAsync = traceManagedCommand args commandPid "async-remembered"
  remembered <- rememberDiagnosticException remembered0 $
    traceManagedCommand args commandPid "cleanup-started"
  started <- getMonotonicTimeNSec
  let graceDeadline = started + 5000000000
      finalDeadline = started + 10000000000
  (status, remembered1) <- completeRememberingAsyncWith noteAsync remembered (getProcessExitCode process)
  case status of
    Just _ -> reap remembered1
    Nothing -> do
      skipGrace <- Foldable.or <$> mapM faultEnabled
        [ "active-command-grace-expiry"
        , "active-command-repeated-cancel"
        , "active-command-cleanup-trace-failure"
        , "active-command-timeout-trace-cancel"
        ]
      remembered2 <- if skipGrace
        then pure remembered1
        else snd <$> completeRememberingAsyncWith noteAsync remembered1 (terminateProcess process)
      waitForCommandExitUntil graceDeadline noteAsync process remembered2 >>= \case
        (Just _, remembered3) -> reap remembered3
        (Nothing, remembered3) -> do
          remembered4 <- rememberDiagnosticException remembered3 $
            traceManagedCommand args commandPid "escalated"
          pauseAtEscalation <- Foldable.or <$> mapM faultEnabled
            ["active-command-repeated-cancel", "active-command-timeout-trace-cancel"]
          remembered5 <- if pauseAtEscalation
            then do
              now <- getMonotonicTimeNSec
              delayRememberingAsync (Prelude.min finalDeadline (now + 1000000000)) noteAsync remembered4
            else pure remembered4
          remembered6 <- forceTerminateCommand environment commandPid process remembered5
          waitForCommandExitUntil finalDeadline noteAsync process remembered6 >>= \case
            (Nothing, _) -> fail "Managed command remained alive after bounded hard termination"
            (Just _, remembered7) -> reap remembered7
  where
    reap remembered = do
      let noteAsync = traceManagedCommand args commandPid "async-remembered"
      (_, remembered') <- completeRememberingAsyncWith noteAsync remembered (waitForProcess process)
      remembered'' <- rememberDiagnosticException remembered' $
        traceManagedCommand args commandPid "reaped"
      pure remembered''

forceTerminateCommand :: [(String, String)] -> String -> ProcessHandle -> Maybe SomeException -> IO (Maybe SomeException)
forceTerminateCommand _ "unavailable" _ _ =
  fail "Cannot hard-terminate managed command without its exact process ID"
#if defined(mingw32_HOST_OS)
forceTerminateCommand _ _ process remembered =
  -- System.Process uses the exact owned process handle and TerminateProcess on Windows.
  snd <$> completeRememberingAsync remembered (terminateProcess process)
#else
forceTerminateCommand _ commandPid _ remembered = do
  pid <- maybe (fail "Managed command process ID was not numeric") pure $
    readMaybe commandPid :: IO CInt
  snd <$> completeRememberingAsync remembered
    (throwErrnoIfMinus1_ "kill" $ c_kill pid 9)

foreign import ccall unsafe "kill" c_kill :: CInt -> CInt -> IO CInt
#endif

traceManagedCommand :: [String] -> String -> String -> IO ()
traceManagedCommand args commandPid state = do
  activeFault <- Foldable.or <$> mapM faultEnabled
    [ "active-command"
    , "active-command-grace-expiry"
    , "active-command-immediate-exit"
    , "active-command-repeated-cancel"
    , "active-command-trace-failure"
    , "active-command-cleanup-trace-failure"
    , "active-command-timeout-trace-cancel"
    ]
  traceFile <- lookupEnv managedCommandTraceVar
  when (activeFault && "sleep" `elem` args && "30" `elem` args) $
    case traceFile of
      Nothing -> pure ()
      Just path -> do
        sandbox <- envOrFail sandboxRootVar
        assertInSandboxRoot sandbox path
        persistentCleanupFailure <- Foldable.or <$> mapM faultEnabled
          ["active-command-cleanup-trace-failure", "active-command-timeout-trace-cancel"]
        if persistentCleanupFailure && state /= "started"
          then do
            appendFile (path <> ".failed") (state <> "=" <> commandPid <> "\n")
            fail "Injected persistent managed command cleanup trace failure"
          else do
            appendFile path (state <> "=" <> commandPid <> "\n")
            traceFailure <- faultEnabled "active-command-trace-failure"
            when (traceFailure && state == "started") $
              fail "Injected managed command trace failure after acquisition"

assertInSandboxRoot :: FilePath -> FilePath -> IO ()
assertInSandboxRoot rawRoot rawPath = do
  root <- canonicalizePath rawRoot
  path <- canonicalizePath rawPath
  unless (isPathWithin root path) $
    fail "Managed command trace path is outside the active sandbox"

validContainerId :: String -> Bool
validContainerId value = length value == 64 && all isHexDigit value

confirmedMissingContainerError :: String -> Bool
confirmedMissingContainerError raw =
  let lower = T.unpack (T.toLower (T.pack raw))
  in "no such object" `isInfixOf` lower || "no such container" `isInfixOf` lower

faultEnabled :: String -> IO Bool
faultEnabled expected = (== Just expected) <$> lookupEnv managedFaultVar

splitOn :: Char -> String -> [String]
splitOn separator input = case break (== separator) input of
  (part, []) -> [part]
  (part, _:rest) -> part : splitOn separator rest

dropPrefix :: String -> String -> Maybe String
dropPrefix [] rest = Just rest
dropPrefix _ [] = Nothing
dropPrefix (x:xs) (y:ys)
  | x == y = dropPrefix xs ys
  | otherwise = Nothing

trim :: String -> String
trim = reverse . dropWhile (`elem` [' ', '\t', '\r', '\n']) . reverse . dropWhile (`elem` [' ', '\t', '\r', '\n'])

resolveDefaultDatabaseRole :: IO String
resolveDefaultDatabaseRole = do
  candidates <- mapM lookupEnv ["USERNAME", "USER"]
  case [value | Just value <- candidates, not (Foldable.null value)] of
    value:_
      | length value <= 63 && '\NUL' `notElem` value -> pure value
      | otherwise -> fail "Local database role name is not valid for managed tests"
    [] -> fail "Could not determine the local default database role for managed tests"

quotePostgresIdentifier :: String -> String
quotePostgresIdentifier value = '"' : concatMap escape value <> "\""
  where
    escape '"' = "\"\""
    escape char = [char]

-- | Start an ephemeral PostgreSQL cluster on a random port.
startEphemeralPg :: IO EphemeralPg
startEphemeralPg =
  bracketOnError createStandaloneSandbox (`cleanupSandbox` False) startEphemeralPgInSandbox

startEphemeralPgInSandbox :: TestSandbox -> IO EphemeralPg
startEphemeralPgInSandbox sandbox = do
  port <- randomRIO (49152, 65535) :: IO Int
  suffix <- T.take 8 . T.filter (/= '-') . T.pack . show <$> UUID.nextRandom
  let dbName = "hmem_test_" <> suffix
      dataDir = sandbox.sandboxRoot </> "postgres" </> "data"
      logFile = sandbox.sandboxLogDir </> "postgresql.log"

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

  (startedPort, process) <- startPostgresWithRetries dataDir logFile port 5
  let pg = EphemeralPg
        { epTmpDir  = sandbox.sandboxRoot
        , epDataDir = dataDir
        , epPort    = startedPort
        , epConnStr = "host=localhost port=" <> T.pack (show startedPort) <> " dbname=" <> dbName
        , epLogFile = logFile
        , epDbName  = dbName
        , epProcess = process
        }

  bracketOnError
    (pure pg)
    stopEphemeralPgServer
    (\started -> do
      threadDelay 500000

      -- Create the test database
      callProcess "createdb"
        [ "-h", "localhost", "-p", show startedPort, T.unpack dbName ]

      hPutStrLn stderr "[test-pg] PostgreSQL ready."
      hFlush stderr

      pure started)

-- | Retry a random port when another process wins the race between port
-- selection and PostgreSQL binding. Windows frequently leaves just-closed
-- loopback ports unavailable briefly during the migration-heavy test suite.
startPostgresWithRetries :: FilePath -> FilePath -> Int -> Int -> IO (Int, Maybe ProcessHandle)
startPostgresWithRetries dataDir logFile port attempts = do
  started <- if os == "mingw32"
    then startWindowsPostgres dataDir logFile port
    else fmap (fmap (const Nothing)) $ try (callProcess "pg_ctl"
      [ "start", "-D", dataDir, "-l", logFile, "-w", "-t", "30" ])
  case started of
    Right process -> pure (port, process)
    Left err
      | attempts <= 1 -> throwIO err
      | otherwise -> do
          nextPort <- randomRIO (49152, 65535) :: IO Int
          hPutStrLn stderr $ "[test-pg] retrying port: " ++ show nextPort
          appendFile (dataDir </> "postgresql.conf") $ "port = " ++ show nextPort ++ "\n"
          startPostgresWithRetries dataDir logFile nextPort (attempts - 1)

-- | PostgreSQL's Windows @pg_ctl start@ asks Windows to create a restricted
-- child token.  That API is unavailable in some isolated test hosts even
-- though @postgres@ itself can run normally.  Start the foreground server
-- directly there and wait for its local readiness probe; other platforms keep
-- pg_ctl's normal daemonized, wait-for-ready behavior.
startWindowsPostgres :: FilePath -> FilePath -> Int -> IO (Either SomeException (Maybe ProcessHandle))
startWindowsPostgres dataDir logFile port = try $ do
  handle <- withFile logFile AppendMode $ \logHandle -> do
    (_, _, _, process) <- createProcess (proc "postgres" ["-D", dataDir])
      { std_out = UseHandle logHandle, std_err = UseHandle logHandle }
    pure process
  ready <- waitForLocalPostgres port handle 60
  if ready then pure (Just handle) else do
    terminateProcess handle
    void $ waitForProcess handle
    fail "postgres did not become ready"

waitForLocalPostgres :: Int -> ProcessHandle -> Int -> IO Bool
waitForLocalPostgres port process attempts = do
  exited <- getProcessExitCode process
  case exited of
    Just _ -> pure False
    Nothing -> do
      ready <- try (callProcess "pg_isready" ["-h", "localhost", "-p", show port]) :: IO (Either SomeException ())
      case ready of
        Right () -> pure True
        Left _
          | attempts <= 1 -> pure False
          | otherwise -> threadDelay 500000 >> waitForLocalPostgres port process (attempts - 1)

-- | Stop the ephemeral PostgreSQL cluster and remove its temp
-- directory.  Ignores errors so teardown always completes.
stopEphemeralPg :: EphemeralPg -> IO ()
stopEphemeralPg pg = do
  stopEphemeralPgServer pg
  removeSandboxDirectory pg.epTmpDir

stopEphemeralPgServer :: EphemeralPg -> IO ()
stopEphemeralPgServer pg = do
  hPutStrLn stderr "[test-pg] Tearing down..."
  hFlush stderr
  stopResult <- (try $ case pg.epProcess of
    Just process -> do
      terminateProcess process
      void $ waitForProcess process
    Nothing -> callProcess "pg_ctl" [ "stop", "-D", pg.epDataDir, "-m", "fast" ]) :: IO (Either SomeException ())
  case stopResult of
    Right () -> pure ()
    Left err -> hPutStrLn stderr $ "[test-pg] PostgreSQL teardown failed: " <> show err
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
  result <- try $ do
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
  case result of
    Right sandbox -> pure sandbox
    Left (err :: SomeException) -> do
      cleanupSandboxRoot root preserve
      throwIO err

cleanupSandbox :: TestSandbox -> Bool -> IO ()
cleanupSandbox sandbox success
  | sandbox.sandboxPreserveOnFailure && not success =
      hPutStrLn stderr $ "[test-sandbox] preserved after failure: " <> sandbox.sandboxRoot
  | otherwise = cleanupSandboxRoot sandbox.sandboxRoot False

cleanupSandboxRoot :: FilePath -> Bool -> IO ()
cleanupSandboxRoot root preserve
  | preserve = hPutStrLn stderr $ "[test-sandbox] preserved after failure: " <> root
  | otherwise = removeSandboxDirectory root

removeSandboxDirectory :: FilePath -> IO ()
removeSandboxDirectory root = do
  result <- try (removeDirectoryRecursive root) :: IO (Either SomeException ())
  case result of
    Right () -> pure ()
    Left err -> hPutStrLn stderr $ "[test-sandbox] failed to remove sandbox root " <> root <> ": " <> show err

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

postgresBackendVar, preparedBackendVar, preparedOwnerRootVar, preparedDockerExeVar, preparedDockerEndpointVar, preparedDockerImageIdVar, preparedDockerRoleVar, managedFaultVar, managedCommandTraceVar :: String
postgresBackendVar = "HMEM_TEST_POSTGRES_BACKEND"
preparedBackendVar = "HMEM_TEST_SANDBOX_POSTGRES_BACKEND"
preparedOwnerRootVar = "HMEM_TEST_SANDBOX_POSTGRES_OWNER_ROOT"
preparedDockerExeVar = "HMEM_TEST_SANDBOX_DOCKER_EXE"
preparedDockerEndpointVar = "HMEM_TEST_SANDBOX_DOCKER_ENDPOINT"
preparedDockerImageIdVar = "HMEM_TEST_SANDBOX_DOCKER_IMAGE_ID"
preparedDockerRoleVar = "HMEM_TEST_SANDBOX_DOCKER_ROLE"
managedFaultVar = "HMEM_TEST_SANDBOX_MANAGED_FAULT"
managedCommandTraceVar = "HMEM_TEST_SANDBOX_MANAGED_COMMAND_TRACE"

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
  [ postgresBackendVar
  , legacyTestDbVar
  , externalTestDbVar
  , allowExternalTestDbVar
  , "DOCKER_HOST"
  , "DOCKER_CONTEXT"
  , "DOCKER_TLS_VERIFY"
  , "DOCKER_CERT_PATH"
  , "DOCKER_CONFIG"
  , "DOCKER_API_VERSION"
  , "DOCKER_AUTH_CONFIG"
  , "HTTP_PROXY"
  , "HTTPS_PROXY"
  , "NO_PROXY"
  , "http_proxy"
  , "https_proxy"
  , "no_proxy"
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
  [ preparedBackendVar
  , preparedOwnerRootVar
  , preparedDockerExeVar
  , preparedDockerEndpointVar
  , preparedDockerImageIdVar
  , preparedDockerRoleVar
  , managedFaultVar
  , managedCommandTraceVar
  , sandboxActiveVar
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
  , repoRootOverrideVar
  , "HOME"
  , "USERPROFILE"
  , "APPDATA"
  , "LOCALAPPDATA"
  , "XDG_CONFIG_HOME"
  ] <> managedLibpqEnvVars

managedLibpqEnvVars :: [String]
managedLibpqEnvVars =
  [ "PGHOST"
  , "PGHOSTADDR"
  , "PGPORT"
  , "PGDATABASE"
  , "PGUSER"
  , "PGPASSWORD"
  , "PGPASSFILE"
  , "PGSERVICE"
  , "PGSERVICEFILE"
  , "PGOPTIONS"
  , "PGAPPNAME"
  , "PGCONNECT_TIMEOUT"
  , "PGCLIENTENCODING"
  , "PGSSLMODE"
  , "PGREQUIRESSL"
  , "PGSSLCERT"
  , "PGSSLKEY"
  , "PGSSLROOTCERT"
  , "PGSSLCRL"
  , "PGTARGETSESSIONATTRS"
  , "PGCHANNELBINDING"
  ]
