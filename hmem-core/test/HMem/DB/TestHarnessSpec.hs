module HMem.DB.TestHarnessSpec (spec) where

import Control.Exception (SomeException, bracket, try)
import Data.Functor.Contravariant (contramap)
import Data.List (sort)
import Data.Pool (destroyAllResources)
import Data.Text (Text)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Text qualified as T
import Data.UUID (UUID)
import Hasql.Decoders qualified as D
import Hasql.Encoders qualified as E
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement
import System.Directory (copyFile, createDirectoryIfMissing, doesDirectoryExist, listDirectory, removeDirectoryRecursive, withCurrentDirectory)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.FilePath ((</>), takeDirectory)
import System.IO.Temp (getCanonicalTemporaryDirectory)
import Test.Hspec

import HMem.DB.Migration qualified as Migration
import HMem.DB.Pool (DBException(..), createPool, runSession)
import HMem.DB.TestHarness

spec :: Spec
spec = do
  describe "sandboxed test harness metadata" $ do
    it "exposes sandbox and database paths for normal test environments" $
      withTestEnv $ \env -> do
        env.testDb.testDbUnsafeExternal `shouldBe` False
        env.testDb.testDbName `shouldSatisfy` T.isPrefixOf "hmem_test_"
        assertInSandbox env.testSandbox env.testDb.testDbDataDir
        assertInSandbox env.testSandbox env.testDb.testDbLogFile
        assertInSandbox env.testSandbox env.testSandbox.sandboxLogDir
        lookupEnv "HMEM_TEST_DB" >>= (`shouldBe` Nothing)

    it "generates unique database identities for independent sandboxes" $ do
      first <- withTestSandbox $ \sandbox ->
        withSandboxedEnv sandbox $
          withSandboxedPostgres sandbox $ \db ->
            pure db.testDbName
      second <- withTestSandbox $ \sandbox ->
        withSandboxedEnv sandbox $
          withSandboxedPostgres sandbox $ \db ->
            pure db.testDbName
      first `shouldNotBe` second

    it "creates and removes a standalone sandbox root" $ do
      root <- withTestSandbox $ \sandbox -> do
        exists <- doesDirectoryExist sandbox.sandboxRoot
        exists `shouldBe` True
        pure sandbox.sandboxRoot
      doesDirectoryExist root >>= (`shouldBe` False)

    it "checks sandbox path containment" $
      withTestSandbox $ \sandbox -> do
        assertInSandbox sandbox sandbox.sandboxTmpDir
        assertInSandbox sandbox (sandbox.sandboxRoot)
        assertInSandbox sandbox (takeDirectory sandbox.sandboxRoot) `shouldThrow` anyException

  describe "sandboxed environment" $ do
    it "scrubs ambient credentials inside the sandbox and restores them afterwards" $
      withEnvVar "HMEM_API_KEY" (Just "real-user-token") $ do
        withTestSandbox $ \sandbox ->
          withSandboxedEnv sandbox $ do
            lookupEnv "HMEM_API_KEY" >>= (`shouldBe` Nothing)
            lookupEnv "HOME" >>= (`shouldBe` Just sandbox.sandboxHomeDir)
        lookupEnv "HMEM_API_KEY" >>= (`shouldBe` Just "real-user-token")

    it "overrides and restores the explicit repo-root override inside the sandbox" $
      getCanonicalTemporaryDirectory >>= \staleRoot -> withEnvVar "HMEM_TEST_REPO_ROOT" (Just staleRoot) $ do
        withTestSandbox $ \sandbox ->
          withSandboxedEnv sandbox $
            lookupEnv "HMEM_TEST_REPO_ROOT" >>= (`shouldBe` Just sandbox.sandboxRepoRoot)
        lookupEnv "HMEM_TEST_REPO_ROOT" >>= (`shouldBe` Just staleRoot)

    it "ignores the legacy HMEM_TEST_DB variable inside sandboxed tests" $
      withEnvVar "HMEM_TEST_DB" (Just "host=real.example dbname=real_hmem") $ do
        withTestSandbox $ \sandbox ->
          withSandboxedEnv sandbox $
            lookupEnv "HMEM_TEST_DB" >>= (`shouldBe` Nothing)
        lookupEnv "HMEM_TEST_DB" >>= (`shouldBe` Just "host=real.example dbname=real_hmem")

    it "rejects unsafe external DB settings unless explicitly allowed" $
      withoutActiveSandbox $
        withEnvVar "HMEM_TEST_EXTERNAL_DB" (Just "host=real.example dbname=real_hmem") $
          withEnvVar "HMEM_TEST_ALLOW_EXTERNAL_DB" Nothing $
            withTestEnv (const (pure ())) `shouldThrow` anyException

    it "rejects unsafe external DB mode in CI" $
      withoutActiveSandbox $
        withEnvVar "HMEM_TEST_EXTERNAL_DB" (Just "host=real.example dbname=real_hmem") $
          withEnvVar "HMEM_TEST_ALLOW_EXTERNAL_DB" (Just "1") $
            withEnvVar "CI" (Just "true") $
              withTestEnv (const (pure ())) `shouldThrow` anyException

    it "labels explicitly allowed external DB mode as unsafe" $
      withTestSandbox $ \externalSandbox ->
        withSandboxedEnv externalSandbox $
          withSandboxedPostgres externalSandbox $ \externalDb ->
            withoutActiveSandbox $
              withEnvVar "HMEM_TEST_EXTERNAL_DB" (Just (T.unpack externalDb.testDbConnStr)) $
                withEnvVar "HMEM_TEST_ALLOW_EXTERNAL_DB" (Just "1") $
                  withEnvVar "CI" Nothing $
                    withTestEnv $ \env -> do
                      env.testDb.testDbUnsafeExternal `shouldBe` True
                      env.testDb.testDbConnStr `shouldBe` externalDb.testDbConnStr

    it "preserves the sandbox root on failure when requested" $ do
      rootRef <- newIORef Nothing
      withEnvVar "HMEM_TEST_PRESERVE_SANDBOX" (Just "1") $ do
        result <- try $ withTestSandbox $ \sandbox -> do
          writeIORef rootRef (Just sandbox.sandboxRoot)
          fail "intentional sandbox preservation test failure"
        (result :: Either SomeException ()) `shouldSatisfy` either (const True) (const False)
      mRoot <- readIORef rootRef
      case mRoot of
        Nothing -> expectationFailure "sandbox root was not captured"
        Just root -> do
          doesDirectoryExist root >>= (`shouldBe` True)
          removeDirectoryRecursive root

  describe "migration resolution" $ do
    it "uses the resolved repository root even when cwd changes" $
      withTestSandbox $ \sandbox ->
        withCurrentDirectory sandbox.sandboxTmpDir $ do
          migrations <- resolveMigrationsDir sandbox.sandboxRepoRoot
          migrations `shouldBe` sandbox.sandboxMigrationsDir

    it "legacyUnlinkedMigration preserves legacy unlinked memories when applying the explicit-link migration" $
      withTestSandbox $ \sandbox -> do
        migrations <- resolveMigrationsDir sandbox.sandboxRepoRoot
        withSandboxedEnv sandbox $
          withSandboxedPostgres sandbox $ \db ->
            bracket (createPool db.testDbConnStr 2 30 30000) destroyAllResources $ \pool -> do
              preV13Dir <- copyMigrationSubset sandbox migrations "pre-v013" (\name -> name < "V013")
              v13OnlyDir <- copyMigrationSubset sandbox migrations "v013-only" (== "V013__explicit_memory_creation_links.sql")

              preResult <- Migration.runMigrations pool preV13Dir
              preResult.failed `shouldBe` Nothing

              wsId <- runSession pool $
                Session.statement ("legacy-unlinked-migration-ws" :: Text) insertWorkspaceDirectStatement
              legacyMemoryId <- runSession pool $
                Session.statement (wsId, "legacy unlinked memory" :: Text) insertUnlinkedMemoryDirectStatement

              v13Result <- Migration.runMigrations pool v13OnlyDir
              v13Result.failed `shouldBe` Nothing

              legacyActive <- runSession pool $
                Session.statement legacyMemoryId activeMemoryExistsStatement
              legacyActive `shouldBe` True

              newUnlinked <- try (runSession pool $
                Session.statement (wsId, "new unlinked memory" :: Text) insertUnlinkedMemoryDirectStatement)
                :: IO (Either DBException UUID)
              case newUnlinked of
                Left (DBWorkflowViolation code _ _ _) -> code `shouldBe` "MEMORY_LINK_REQUIRED"
                Left other -> expectationFailure $ "Expected MEMORY_LINK_REQUIRED, got: " <> show other
                Right _ -> expectationFailure "Expected new unlinked memory insert to fail after V013"

    it "resolves the repository root from a non-repo cwd" $
      withTestSandbox $ \sandbox ->
        withCurrentDirectory sandbox.sandboxTmpDir $ do
          repoRoot <- resolveRepoRoot
          repoRoot `shouldBe` sandbox.sandboxRepoRoot

withEnvVar :: String -> Maybe String -> IO a -> IO a
withEnvVar name value = bracket setup restore . const
  where
    setup = do
      old <- lookupEnv name
      case value of
        Nothing -> unsetEnv name
        Just raw -> setEnv name raw
      pure old

    restore old = case old of
      Nothing -> unsetEnv name
      Just raw -> setEnv name raw

withoutActiveSandbox :: IO a -> IO a
withoutActiveSandbox = withEnvVar "HMEM_TEST_SANDBOX_ACTIVE" Nothing

copyMigrationSubset :: TestSandbox -> FilePath -> FilePath -> (FilePath -> Bool) -> IO FilePath
copyMigrationSubset sandbox sourceDir dirname keep = do
  let destDir = sandbox.sandboxTmpDir </> dirname
  createDirectoryIfMissing True destDir
  files <- sort . filter keep <$> listDirectory sourceDir
  mapM_ (\name -> copyFile (sourceDir </> name) (destDir </> name)) files
  pure destDir

insertWorkspaceDirectStatement :: Statement.Statement Text UUID
insertWorkspaceDirectStatement = Statement.Statement sql encoder decoder True
  where
    sql = "INSERT INTO workspaces (name) VALUES ($1) RETURNING id"
    encoder = E.param (E.nonNullable E.text)
    decoder = D.singleRow (D.column (D.nonNullable D.uuid))

insertUnlinkedMemoryDirectStatement :: Statement.Statement (UUID, Text) UUID
insertUnlinkedMemoryDirectStatement = Statement.Statement sql encoder decoder True
  where
    sql = "INSERT INTO memories (workspace_id, content, memory_type) VALUES ($1, $2, 'short_term') RETURNING id"
    encoder =
      contramap fst (E.param (E.nonNullable E.uuid)) <>
      contramap snd (E.param (E.nonNullable E.text))
    decoder = D.singleRow (D.column (D.nonNullable D.uuid))

activeMemoryExistsStatement :: Statement.Statement UUID Bool
activeMemoryExistsStatement = Statement.Statement sql encoder decoder True
  where
    sql = "SELECT EXISTS (SELECT 1 FROM memories WHERE id = $1 AND deleted_at IS NULL)"
    encoder = E.param (E.nonNullable E.uuid)
    decoder = D.singleRow (D.column (D.nonNullable D.bool))
