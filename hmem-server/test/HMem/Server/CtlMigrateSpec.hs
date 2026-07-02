module HMem.Server.CtlMigrateSpec (spec) where

import Control.Exception (bracket)
import Data.Pool (destroyAllResources)
import Hasql.Session qualified as Session
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import Test.Hspec

import HMem.Config qualified as Config
import HMem.DB.Pool qualified as DBPool
import HMem.DB.TestHarness
  ( TestDb(..)
  , TestSandbox(..)
  , withSandboxedEnv
  , withSandboxedPostgres
  , withTestSandbox
  )
import HMem.Server.CtlMigrate

spec :: Spec
spec = describe "hmem-ctl migrate support" $ do
  it "uses the container migrations directory by default" $
    defaultContainerMigrationsDir `shouldBe` "/opt/hmem/migrations"

  it "rejects a missing migrations directory before connecting" $
    withTestSandbox $ \sandbox -> do
      let missing = sandbox.sandboxTmpDir </> "missing-migrations"
      result <- runMigrateWithConfig unreachableConfig (MigrateOptions missing)
      result `shouldBe` Left (MigrationsDirMissing missing)

  it "rejects an empty migrations directory before connecting" $
    withTestSandbox $ \sandbox -> do
      let emptyDir = sandbox.sandboxTmpDir </> "empty-migrations"
      createDirectoryIfMissing True emptyDir
      result <- runMigrateWithConfig unreachableConfig (MigrateOptions emptyDir)
      result `shouldBe` Left (MigrationsDirEmpty emptyDir)

  it "reports database connection failures clearly" $
    withTestSandbox $ \sandbox -> do
      migrationsDir <- writeMinimalMigration sandbox "connection-failure-migrations"
      result <- runMigrateWithConfig unreachableConfig (MigrateOptions migrationsDir)
      case result of
        Left (DatabaseUnavailable msg) -> msg `shouldSatisfy` (not . null)
        other -> expectationFailure $ "Expected DatabaseUnavailable, got: " <> show other

  it "reports failed migration SQL" $
    withTestSandbox $ \sandbox ->
      withSandboxedEnv sandbox $
        withSandboxedPostgres sandbox $ \db -> do
          let migrationsDir = sandbox.sandboxTmpDir </> "broken-migrations"
              migrationName = "V001__broken.sql"
          createDirectoryIfMissing True migrationsDir
          writeFile (migrationsDir </> migrationName) "SELECT * FROM definitely_missing_ctl_migrate_table;\n"

          result <- runMigrateWithConfig (configForDb db) (MigrateOptions migrationsDir)
          case result of
            Left (MigrationFailed file msg) -> do
              file `shouldBe` migrationName
              msg `shouldSatisfy` (not . null)
            other -> expectationFailure $ "Expected MigrationFailed, got: " <> show other

  it "rejects malformed schema_migrations instead of silently skipping" $
    withTestSandbox $ \sandbox ->
      withSandboxedEnv sandbox $
        withSandboxedPostgres sandbox $ \db -> do
          migrationsDir <- writeMinimalMigration sandbox "malformed-schema-migrations"
          bracket (DBPool.createPool db.testDbConnStr 1 5 30000) destroyAllResources $ \pool ->
            DBPool.runSession pool $
              Session.sql "CREATE TABLE schema_migrations (version_text text PRIMARY KEY)"

          result <- runMigrateWithConfig (configForDb db) (MigrateOptions migrationsDir)
          case result of
            Left (SchemaMigrationsUnreadable msg) ->
              msg `shouldSatisfy` (not . null)
            other -> expectationFailure $ "Expected SchemaMigrationsUnreadable, got: " <> show other

  it "is idempotent after all migrations are applied" $
    withTestSandbox $ \sandbox ->
      withSandboxedEnv sandbox $
        withSandboxedPostgres sandbox $ \db -> do
          migrationsDir <- writeMinimalMigration sandbox "idempotent-migrations"
          let cfg = configForDb db
              opts = MigrateOptions migrationsDir

          first <- runMigrateWithConfig cfg opts
          first `shouldBe` Right (MigrateReport [minimalMigrationName] [])

          second <- runMigrateWithConfig cfg opts
          second `shouldBe` Right (MigrateReport [] [minimalMigrationName])

minimalMigrationName :: FilePath
minimalMigrationName = "V001__minimal_schema_migrations.sql"

writeMinimalMigration :: TestSandbox -> FilePath -> IO FilePath
writeMinimalMigration sandbox dirName = do
  let migrationsDir = sandbox.sandboxTmpDir </> dirName
  createDirectoryIfMissing True migrationsDir
  writeFile (migrationsDir </> minimalMigrationName) $ unlines
    [ "CREATE TABLE IF NOT EXISTS schema_migrations ("
    , "  version integer PRIMARY KEY,"
    , "  name text NOT NULL,"
    , "  applied_at timestamptz NOT NULL DEFAULT now()"
    , ");"
    , "CREATE TABLE IF NOT EXISTS ctl_migrate_spec (id integer PRIMARY KEY);"
    ]
  pure migrationsDir

configForDb :: TestDb -> Config.HMemConfig
configForDb db = Config.defaultConfig
  { Config.database = Config.DatabaseConfig
      { Config.host = "localhost"
      , Config.port = db.testDbPort
      , Config.name = db.testDbName
      , Config.user = Nothing
      , Config.password = Nothing
      , Config.sslmode = Nothing
      }
  }

unreachableConfig :: Config.HMemConfig
unreachableConfig = Config.defaultConfig
  { Config.database = Config.defaultConfig.database
      { Config.host = "127.0.0.1"
      , Config.port = 1
      , Config.name = "hmem_ctl_migrate_unreachable"
      , Config.user = Nothing
      , Config.password = Nothing
      , Config.sslmode = Nothing
      }
  , Config.pool = Config.defaultConfig.pool
      { Config.size = 1
      , Config.idleTimeout = 1
      , Config.statementTimeoutMs = 1000
      }
  }
