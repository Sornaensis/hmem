module HMem.DB.TestHarnessSpec (spec) where

import Control.Exception (SomeException, bracket, try)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Text qualified as T
import System.Directory (doesDirectoryExist, removeDirectoryRecursive, withCurrentDirectory)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.FilePath (takeDirectory)
import System.IO.Temp (getCanonicalTemporaryDirectory)
import Test.Hspec

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
