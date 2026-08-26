module HMem.Server.CtlMigrate
  ( defaultContainerMigrationsDir
  , MigrateOptions(..)
  , MigrateReport(..)
  , MigrateError(..)
  , renderMigrateError
  , validateMigrationsDirectory
  , runMigrate
  , runMigrateWithConfig
  ) where

import Control.Exception (SomeException, try)
import Data.Int (Int32)
import Data.List (isPrefixOf, isSuffixOf, sort)
import Data.Pool (Pool, destroyAllResources)
import Hasql.Connection qualified as Hasql
import Hasql.Decoders qualified as Dec
import Hasql.Encoders qualified as Enc
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (takeFileName)

import HMem.Config (HMemConfig(..), PoolConfig(..), connectionString, loadConfig)
import HMem.DB.Migration qualified as Migration
import HMem.DB.Pool qualified as Pool

-- | Container image location for bundled SQL migrations.
defaultContainerMigrationsDir :: FilePath
defaultContainerMigrationsDir = "/opt/hmem/migrations"

data MigrateOptions = MigrateOptions
  { migrationsDir :: !FilePath
  } deriving stock (Show, Eq)

data MigrateReport = MigrateReport
  { appliedMigrations :: ![FilePath]
  , skippedMigrations :: ![FilePath]
  } deriving stock (Show, Eq)

data MigrateError
  = MigrationsDirMissing !FilePath
  | MigrationsDirUnreadable !FilePath !String
  | MigrationsDirEmpty !FilePath
  | DatabaseUnavailable !String
  | MigrationFailed !FilePath !String
  | SchemaMigrationsUnreadable !String
  deriving stock (Show, Eq)

renderMigrateError :: MigrateError -> String
renderMigrateError = \case
  MigrationsDirMissing dir ->
    "migrations directory does not exist: " <> dir
  MigrationsDirUnreadable dir err ->
    "could not read migrations directory " <> dir <> ": " <> err
  MigrationsDirEmpty dir ->
    "migrations directory contains no migration files (expected V*.sql): " <> dir
  DatabaseUnavailable err ->
    "database connection or migration check failed: " <> err
  MigrationFailed file err ->
    "migration failed: " <> file <> "; error: " <> err
  SchemaMigrationsUnreadable err ->
    "schema_migrations table could not be read after migrations: " <> err

-- | Validate that a migrations directory exists and contains at least one
-- migration candidate before opening a database connection.
validateMigrationsDirectory :: FilePath -> IO (Either MigrateError [FilePath])
validateMigrationsDirectory dir = do
  exists <- doesDirectoryExist dir
  if not exists
    then pure $ Left $ MigrationsDirMissing dir
    else do
      listing <- try (listDirectory dir) :: IO (Either SomeException [FilePath])
      case listing of
        Left err -> pure $ Left $ MigrationsDirUnreadable dir (show err)
        Right entries -> do
          let migrations = sort $ filter isMigrationFile entries
          pure $ if null migrations
            then Left $ MigrationsDirEmpty dir
            else Right migrations
  where
    isMigrationFile name =
      "V" `isPrefixOf` takeFileName name && ".sql" `isSuffixOf` takeFileName name

-- | Load the normal hmem config and apply pending migrations to the configured
-- already-running PostgreSQL database.
runMigrate :: MigrateOptions -> IO (Either MigrateError MigrateReport)
runMigrate opts = do
  cfg <- loadConfig
  runMigrateWithConfig cfg opts

runMigrateWithConfig :: HMemConfig -> MigrateOptions -> IO (Either MigrateError MigrateReport)
runMigrateWithConfig cfg opts = do
  validated <- validateMigrationsDirectory opts.migrationsDir
  case validated of
    Left err -> pure $ Left err
    Right _ -> do
      pool <- Pool.createPool
        (connectionString cfg.database)
        cfg.pool.size
        cfg.pool.idleTimeout
        cfg.pool.statementTimeoutMs
      -- If a migration ledger already exists, fail with the explicit ledger
      -- diagnostic before attempting its first version lookup.  A fresh
      -- database has no ledger yet and is allowed to create one in V001.
      existingLedger <- runSchemaStatement pool schemaMigrationsExistsStatement
      preflight <- case existingLedger of
        Left err -> pure (Left err)
        Right False -> pure (Right ())
        Right True -> verifySchemaMigrationsReadable pool
      finalResult <- case preflight of
        Left err -> pure (Left err)
        Right () -> do
          result <- try (Migration.runMigrations pool opts.migrationsDir)
            :: IO (Either SomeException Migration.MigrationResult)
          case result of
            Left err -> pure $ Left $ DatabaseUnavailable (show err)
            Right migrationResult -> case migrationResult.failed of
              Just (file, err) -> pure $ Left $ MigrationFailed file err
              Nothing -> do
                schemaOk <- verifySchemaMigrationsReadable pool
                pure $ case schemaOk of
                  Left err -> Left err
                  Right () -> Right MigrateReport
                    { appliedMigrations = migrationResult.applied
                    , skippedMigrations = migrationResult.skipped
                    }
      destroyAllResources pool
      pure finalResult

verifySchemaMigrationsReadable :: Pool Hasql.Connection -> IO (Either MigrateError ())
verifySchemaMigrationsReadable pool = do
  shape <- runSchemaStatement pool schemaMigrationsShapeStatement
  case shape of
    Left err -> pure $ Left err
    Right False -> pure $ Left $ SchemaMigrationsUnreadable
      "schema_migrations is missing expected columns: version integer, name text, applied_at timestamptz"
    Right True -> do
      readable <- runSchemaStatement pool schemaMigrationsReadableStatement
      pure $ (() <$ readable)

runSchemaStatement
  :: Pool Hasql.Connection
  -> Statement.Statement () a
  -> IO (Either MigrateError a)
runSchemaStatement pool stmt = do
  result <- try @SomeException (Pool.withConn pool $ \conn ->
    Session.run (Session.statement () stmt) conn)
  pure $ case result of
    Left err -> Left $ DatabaseUnavailable (show err)
    Right (Left err) -> Left $ SchemaMigrationsUnreadable (show err)
    Right (Right value) -> Right value

schemaMigrationsExistsStatement :: Statement.Statement () Bool
schemaMigrationsExistsStatement = Statement.Statement
  "SELECT to_regclass('schema_migrations') IS NOT NULL"
  Enc.noParams
  (Dec.singleRow (Dec.column (Dec.nonNullable Dec.bool)))
  True

schemaMigrationsShapeStatement :: Statement.Statement () Bool
schemaMigrationsShapeStatement = Statement.Statement
  "SELECT EXISTS (\
  \ SELECT 1\
  \ FROM pg_class c\
  \ JOIN pg_attribute version_col\
  \   ON version_col.attrelid = c.oid\
  \  AND version_col.attname = 'version'\
  \  AND NOT version_col.attisdropped\
  \ JOIN pg_type version_type\
  \   ON version_type.oid = version_col.atttypid\
  \  AND version_type.typname = 'int4'\
  \ JOIN pg_attribute name_col\
  \   ON name_col.attrelid = c.oid\
  \  AND name_col.attname = 'name'\
  \  AND NOT name_col.attisdropped\
  \ JOIN pg_type name_type\
  \   ON name_type.oid = name_col.atttypid\
  \  AND name_type.typname = 'text'\
  \ JOIN pg_attribute applied_col\
  \   ON applied_col.attrelid = c.oid\
  \  AND applied_col.attname = 'applied_at'\
  \  AND NOT applied_col.attisdropped\
  \ JOIN pg_type applied_type\
  \   ON applied_type.oid = applied_col.atttypid\
  \  AND applied_type.typname = 'timestamptz'\
  \ WHERE c.oid = to_regclass('schema_migrations')\
  \   AND c.relkind IN ('r', 'p')\
  \)"
  Enc.noParams
  (Dec.singleRow (Dec.column (Dec.nonNullable Dec.bool)))
  True

schemaMigrationsReadableStatement :: Statement.Statement () Int32
schemaMigrationsReadableStatement = Statement.Statement
  "SELECT count(*)::int FROM (SELECT version, name, applied_at FROM schema_migrations LIMIT 1) readable_schema_migrations"
  Enc.noParams
  (Dec.singleRow (Dec.column (Dec.nonNullable Dec.int4)))
  True
