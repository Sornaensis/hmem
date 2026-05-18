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
import HMem.DB.Pool (DBException(..), createPool, runSession, runTransaction)
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

    it "autoBlockMigration backfills existing task blockers and preserves manual blocked tasks" $
      withTestSandbox $ \sandbox -> do
        migrations <- resolveMigrationsDir sandbox.sandboxRepoRoot
        withSandboxedEnv sandbox $
          withSandboxedPostgres sandbox $ \db ->
            bracket (createPool db.testDbConnStr 2 30 30000) destroyAllResources $ \pool -> do
              preV14Dir <- copyMigrationSubset sandbox migrations "pre-v014" (\name -> name < "V014")
              v14OnlyDir <- copyMigrationSubset sandbox migrations "v014-only" (== "V014__recursive_task_auto_blocking.sql")

              preResult <- Migration.runMigrations pool preV14Dir
              preResult.failed `shouldBe` Nothing

              wsId <- runSession pool $
                Session.statement ("auto-block-migration-ws" :: Text) insertWorkspaceDirectStatement
              projectId <- runSession pool $
                Session.statement (wsId, "Auto Block Migration Project" :: Text) insertProjectDirectStatement
              parentId <- runSession pool $
                Session.statement (wsId, projectId, Nothing, "parent" :: Text, "todo" :: Text) insertTaskDirectStatement
              _childId <- runSession pool $
                Session.statement (wsId, projectId, Just parentId, "child" :: Text, "todo" :: Text) insertTaskDirectStatement
              manualBlockedId <- runSession pool $
                Session.statement (wsId, projectId, Nothing, "manual blocked" :: Text, "blocked" :: Text) insertTaskDirectStatement

              v14Result <- Migration.runMigrations pool v14OnlyDir
              v14Result.failed `shouldBe` Nothing

              parentStatus <- runSession pool $
                Session.statement parentId taskStatusTextStatement
              parentStatus `shouldBe` "blocked"
              parentAutoBlocked <- runSession pool $
                Session.statement parentId taskAutoBlockedStatement
              parentAutoBlocked `shouldBe` True

              manualStatus <- runSession pool $
                Session.statement manualBlockedId taskStatusTextStatement
              manualStatus `shouldBe` "blocked"
              manualAutoBlocked <- runSession pool $
                Session.statement manualBlockedId taskAutoBlockedStatement
              manualAutoBlocked `shouldBe` False

    it "autoBlockMigration preserves legacy blockers inside closed lifecycle trees" $
      withTestSandbox $ \sandbox -> do
        migrations <- resolveMigrationsDir sandbox.sandboxRepoRoot
        withSandboxedEnv sandbox $
          withSandboxedPostgres sandbox $ \db ->
            bracket (createPool db.testDbConnStr 2 30 30000) destroyAllResources $ \pool -> do
              preV12Dir <- copyMigrationSubset sandbox migrations "pre-v012" (\name -> name < "V012")
              v12AndV13Dir <- copyMigrationSubset sandbox migrations "v012-v013" (\name -> name >= "V012" && name < "V014")
              v14OnlyDir <- copyMigrationSubset sandbox migrations "v014-only-closed-project" (== "V014__recursive_task_auto_blocking.sql")

              preResult <- Migration.runMigrations pool preV12Dir
              preResult.failed `shouldBe` Nothing

              wsId <- runSession pool $
                Session.statement ("legacy-closed-project-autoblock-ws" :: Text) insertWorkspaceDirectStatement
              projectId <- runSession pool $
                Session.statement (wsId, "Legacy Closed Project" :: Text) insertProjectDirectStatement
              dependencyId <- runSession pool $
                Session.statement (wsId, projectId, Nothing, "open dependency" :: Text, "todo" :: Text) insertTaskDirectStatement
              dependentId <- runSession pool $
                Session.statement (wsId, projectId, Nothing, "done dependent" :: Text, "done" :: Text) insertTaskDirectStatement
              runSession pool $
                Session.statement (dependentId, dependencyId) insertTaskDependencyDirectStatement
              runSession pool $
                Session.statement (projectId, "completed" :: Text) updateProjectStatusDirectStatement

              activeProjectId <- runSession pool $
                Session.statement (wsId, "Legacy Done Ancestor Project" :: Text) insertProjectDirectStatement
              doneParentId <- runSession pool $
                Session.statement (wsId, activeProjectId, Nothing, "done parent" :: Text, "done" :: Text) insertTaskDirectStatement
              childDependencyId <- runSession pool $
                Session.statement (wsId, activeProjectId, Nothing, "child dependency" :: Text, "todo" :: Text) insertTaskDirectStatement
              childUnderDoneId <- runSession pool $
                Session.statement (wsId, activeProjectId, Just doneParentId, "child under done parent" :: Text, "todo" :: Text) insertTaskDirectStatement
              runSession pool $
                Session.statement (childUnderDoneId, childDependencyId) insertTaskDependencyDirectStatement

              v12AndV13Result <- Migration.runMigrations pool v12AndV13Dir
              v12AndV13Result.failed `shouldBe` Nothing

              v14Result <- Migration.runMigrations pool v14OnlyDir
              v14Result.failed `shouldBe` Nothing

              dependentStatus <- runSession pool $
                Session.statement dependentId taskStatusTextStatement
              dependentStatus `shouldBe` "done"
              dependentAutoBlocked <- runSession pool $
                Session.statement dependentId taskAutoBlockedStatement
              dependentAutoBlocked `shouldBe` False
              projectStatus <- runSession pool $
                Session.statement projectId projectStatusTextStatement
              projectStatus `shouldBe` "completed"

              doneParentStatus <- runSession pool $
                Session.statement doneParentId taskStatusTextStatement
              doneParentStatus `shouldBe` "done"
              childUnderDoneStatus <- runSession pool $
                Session.statement childUnderDoneId taskStatusTextStatement
              childUnderDoneStatus `shouldBe` "todo"
              childUnderDoneAutoBlocked <- runSession pool $
                Session.statement childUnderDoneId taskAutoBlockedStatement
              childUnderDoneAutoBlocked `shouldBe` False

    it "explicitMemoryTypeMigration backfills null rows and rejects new missing types" $
      withTestSandbox $ \sandbox -> do
        migrations <- resolveMigrationsDir sandbox.sandboxRepoRoot
        withSandboxedEnv sandbox $
          withSandboxedPostgres sandbox $ \db ->
            bracket (createPool db.testDbConnStr 2 30 30000) destroyAllResources $ \pool -> do
              preV15Dir <- copyMigrationSubset sandbox migrations "pre-v015" (\name -> name < "V015")
              v15OnlyDir <- copyMigrationSubset sandbox migrations "v015-only" (== "V015__require_explicit_memory_type.sql")

              preResult <- Migration.runMigrations pool preV15Dir
              preResult.failed `shouldBe` Nothing

              wsId <- runSession pool $
                Session.statement ("explicit-type-migration-ws" :: Text) insertWorkspaceDirectStatement
              projectId <- runSession pool $
                Session.statement (wsId, "Explicit Type Migration Project" :: Text) insertProjectDirectStatement
              runSession pool $ Session.sql "ALTER TABLE memories ALTER COLUMN memory_type DROP NOT NULL"
              legacyMemoryId <- runTransaction pool $ do
                mid <- Session.statement (wsId, "legacy null type memory" :: Text) insertMemoryWithoutTypeDirectStatement
                Session.statement (projectId, mid) linkProjectMemoryDirectStatement
                forceDeferredConstraints
                pure mid

              v15Result <- Migration.runMigrations pool v15OnlyDir
              v15Result.failed `shouldBe` Nothing

              legacyType <- runSession pool $
                Session.statement legacyMemoryId memoryTypeTextStatement
              legacyType `shouldBe` "short_term"

              newMissingType <- try (runTransaction pool $ do
                mid <- Session.statement (wsId, "new missing type" :: Text) insertMemoryWithoutTypeDirectStatement
                Session.statement (projectId, mid) linkProjectMemoryDirectStatement
                forceDeferredConstraints
                pure mid)
                :: IO (Either DBException UUID)
              case newMissingType of
                Left (DBWorkflowViolation code _ _ _) -> code `shouldBe` "MEMORY_TYPE_REQUIRED"
                Left other -> expectationFailure $ "Expected MEMORY_TYPE_REQUIRED, got: " <> show other
                Right _ -> expectationFailure "Expected new missing-type memory insert to fail after V015"

    it "flatSubtaskMigration flattens nested tasks, preserves safe dependency edges, and repairs legacy orphans" $
      withTestSandbox $ \sandbox -> do
        migrations <- resolveMigrationsDir sandbox.sandboxRepoRoot
        withSandboxedEnv sandbox $
          withSandboxedPostgres sandbox $ \db ->
            bracket (createPool db.testDbConnStr 2 30 30000) destroyAllResources $ \pool -> do
              preV17Dir <- copyMigrationSubset sandbox migrations "pre-v017" (\name -> name < "V017")
              v17AndV18Dir <- copyMigrationSubset sandbox migrations "v017-v018" (\name -> name >= "V017" && name < "V019")

              preResult <- Migration.runMigrations pool preV17Dir
              preResult.failed `shouldBe` Nothing

              wsId <- runSession pool $
                Session.statement ("flat-subtask-migration-ws" :: Text) insertWorkspaceDirectStatement
              projectId <- runSession pool $
                Session.statement (wsId, "Flat Migration Project" :: Text) insertProjectDirectStatement

              rootId <- runSession pool $
                Session.statement (wsId, projectId, Nothing, "root" :: Text, "todo" :: Text) insertTaskDirectStatement
              childId <- runSession pool $
                Session.statement (wsId, projectId, Just rootId, "child" :: Text, "todo" :: Text) insertTaskDirectStatement
              grandchildId <- runSession pool $
                Session.statement (wsId, projectId, Just childId, "grandchild" :: Text, "todo" :: Text) insertTaskDirectStatement
              greatGrandchildId <- runSession pool $
                Session.statement (wsId, projectId, Just grandchildId, "great grandchild" :: Text, "todo" :: Text) insertTaskDirectStatement
              -- Duplicate edge candidate: migration should keep exactly one.
              runSession pool $
                Session.statement (grandchildId, childId) insertTaskDependencyDirectStatement

              cycleRootId <- runSession pool $
                Session.statement (wsId, projectId, Nothing, "cycle root" :: Text, "todo" :: Text) insertTaskDirectStatement
              cycleChildId <- runSession pool $
                Session.statement (wsId, projectId, Just cycleRootId, "cycle child" :: Text, "todo" :: Text) insertTaskDirectStatement
              cycleGrandchildId <- runSession pool $
                Session.statement (wsId, projectId, Just cycleChildId, "cycle grandchild" :: Text, "todo" :: Text) insertTaskDirectStatement
              -- The reverse dependency means migration must skip cycleGrandchild -> cycleChild.
              runSession pool $
                Session.statement (cycleChildId, cycleGrandchildId) insertTaskDependencyDirectStatement

              hierarchyCycleRootId <- runSession pool $
                Session.statement (wsId, projectId, Nothing, "hierarchy cycle root" :: Text, "todo" :: Text) insertTaskDirectStatement
              hierarchyCycleChildId <- runSession pool $
                Session.statement (wsId, projectId, Just hierarchyCycleRootId, "hierarchy cycle child" :: Text, "todo" :: Text) insertTaskDirectStatement
              hierarchyCycleGrandchildId <- runSession pool $
                Session.statement (wsId, projectId, Just hierarchyCycleChildId, "hierarchy cycle grandchild" :: Text, "todo" :: Text) insertTaskDirectStatement
              runSession pool $ do
                Session.sql "ALTER TABLE tasks DISABLE TRIGGER trg_task_auto_blocking_from_task"
                Session.sql "ALTER TABLE tasks DISABLE TRIGGER trg_task_lifecycle_invariants"
                Session.sql "ALTER TABLE tasks DISABLE TRIGGER trg_task_no_cycle"
                Session.statement (hierarchyCycleRootId, Just hierarchyCycleGrandchildId) setTaskParentDirectStatement
                Session.sql "ALTER TABLE tasks ENABLE TRIGGER trg_task_no_cycle"
                Session.sql "ALTER TABLE tasks ENABLE TRIGGER trg_task_lifecycle_invariants"
                Session.sql "ALTER TABLE tasks ENABLE TRIGGER trg_task_auto_blocking_from_task"

              batchCycleRootId <- runSession pool $
                Session.statement (wsId, projectId, Nothing, "batch cycle root" :: Text, "todo" :: Text) insertTaskDirectStatement
              batchCycleChildId <- runSession pool $
                Session.statement (wsId, projectId, Just batchCycleRootId, "batch cycle child" :: Text, "todo" :: Text) insertTaskDirectStatement
              batchCycleGrandchildId <- runSession pool $
                Session.statement (wsId, projectId, Just batchCycleChildId, "batch cycle grandchild" :: Text, "todo" :: Text) insertTaskDirectStatement
              batchCycleGreatGrandchildId <- runSession pool $
                Session.statement (wsId, projectId, Just batchCycleGrandchildId, "batch cycle great grandchild" :: Text, "todo" :: Text) insertTaskDirectStatement
              -- This existing edge is safe before flattening, but only one of
              -- the two migration-created edges can be added without making a
              -- dependency cycle across the accepted batch.
              runSession pool $
                Session.statement (batchCycleChildId, batchCycleGreatGrandchildId) insertTaskDependencyDirectStatement

              mixedRootId <- runSession pool $
                Session.statement (wsId, projectId, Nothing, "mixed root" :: Text, "todo" :: Text) insertTaskDirectStatement
              mixedChildId <- runSession pool $
                Session.statement (wsId, projectId, Just mixedRootId, "mixed child" :: Text, "todo" :: Text) insertTaskDirectStatement
              doneGrandchildId <- runSession pool $
                Session.statement (wsId, projectId, Just mixedChildId, "done grandchild" :: Text, "done" :: Text) insertTaskDirectStatement
              cancelledGrandchildId <- runSession pool $
                Session.statement (wsId, projectId, Just mixedChildId, "cancelled grandchild" :: Text, "cancelled" :: Text) insertTaskDirectStatement

              deletedParentId <- runSession pool $
                Session.statement (wsId, projectId, Nothing, "deleted parent" :: Text, "todo" :: Text) insertTaskDirectStatement
              orphanedChildId <- runSession pool $
                Session.statement (wsId, projectId, Just deletedParentId, "orphaned child" :: Text, "todo" :: Text) insertTaskDirectStatement
              runSession pool $
                Session.statement deletedParentId softDeleteTaskDirectStatement

              missingParentId <- runSession pool $
                Session.statement (wsId, projectId, Nothing, "missing parent" :: Text, "todo" :: Text) insertTaskDirectStatement
              missingParentChildId <- runSession pool $
                Session.statement (wsId, projectId, Just missingParentId, "missing parent child" :: Text, "todo" :: Text) insertTaskDirectStatement
              runSession pool $ do
                Session.sql "ALTER TABLE tasks DISABLE TRIGGER ALL"
                Session.statement missingParentId hardDeleteTaskDirectStatement
                Session.sql "ALTER TABLE tasks ENABLE TRIGGER ALL"

              v18Result <- Migration.runMigrations pool v17AndV18Dir
              v18Result.failed `shouldBe` Nothing

              activeNested <- runSession pool $ Session.statement () activeNestedSubtaskCountStatement
              activeNested `shouldBe` 0

              runSession pool (Session.statement grandchildId taskParentIdStatement) `shouldReturn` Just rootId
              runSession pool (Session.statement greatGrandchildId taskParentIdStatement) `shouldReturn` Just rootId
              runSession pool (Session.statement cycleGrandchildId taskParentIdStatement) `shouldReturn` Just cycleRootId
              runSession pool (Session.statement batchCycleGrandchildId taskParentIdStatement) `shouldReturn` Just batchCycleRootId
              runSession pool (Session.statement batchCycleGreatGrandchildId taskParentIdStatement) `shouldReturn` Just batchCycleRootId
              runSession pool (Session.statement hierarchyCycleRootId taskParentIdStatement) `shouldReturn` Nothing
              runSession pool (Session.statement hierarchyCycleChildId taskParentIdStatement) `shouldReturn` Nothing
              runSession pool (Session.statement hierarchyCycleGrandchildId taskParentIdStatement) `shouldReturn` Nothing
              runSession pool (Session.statement doneGrandchildId taskParentIdStatement) `shouldReturn` Just mixedRootId
              runSession pool (Session.statement cancelledGrandchildId taskParentIdStatement) `shouldReturn` Just mixedRootId
              runSession pool (Session.statement orphanedChildId taskParentIdStatement) `shouldReturn` Nothing
              runSession pool (Session.statement missingParentChildId taskParentIdStatement) `shouldReturn` Nothing

              runSession pool (Session.statement (grandchildId, childId) taskDependencyCountStatement) `shouldReturn` 1
              runSession pool (Session.statement (greatGrandchildId, grandchildId) taskDependencyCountStatement) `shouldReturn` 1
              runSession pool (Session.statement (doneGrandchildId, mixedChildId) taskDependencyCountStatement) `shouldReturn` 1
              runSession pool (Session.statement (cancelledGrandchildId, mixedChildId) taskDependencyCountStatement) `shouldReturn` 1
              runSession pool (Session.statement (cycleChildId, cycleGrandchildId) taskDependencyCountStatement) `shouldReturn` 1
              runSession pool (Session.statement (cycleGrandchildId, cycleChildId) taskDependencyCountStatement) `shouldReturn` 0
              runSession pool (Session.statement (batchCycleChildId, batchCycleGreatGrandchildId) taskDependencyCountStatement) `shouldReturn` 1
              runSession pool (Session.statement (batchCycleGrandchildId, batchCycleChildId) taskDependencyCountStatement) `shouldReturn` 1
              runSession pool (Session.statement (batchCycleGreatGrandchildId, batchCycleGrandchildId) taskDependencyCountStatement) `shouldReturn` 0
              -- V018 creates real dependency edges for all active moved tasks,
              -- so existing auto-blocking rules reopen done tasks whose new
              -- dependency is still open; cancelled tasks remain closed.
              runSession pool (Session.statement doneGrandchildId taskStatusTextStatement) `shouldReturn` "blocked"
              runSession pool (Session.statement cancelledGrandchildId taskStatusTextStatement) `shouldReturn` "cancelled"
              runSession pool (Session.statement orphanedChildId deletedParentReportCountStatement) `shouldReturn` 1
              runSession pool (Session.statement (missingParentChildId, missingParentId) missingParentReportDetailStatement) `shouldReturn` True
              runSession pool (Session.statement (batchCycleGreatGrandchildId, batchCycleGrandchildId) dependencyCycleSkippedReportDetailStatement) `shouldReturn` True
              runSession pool (Session.statement hierarchyCycleRootId hierarchyCycleReportCountStatement) `shouldReturn` 1

    it "deleteCascadeMigration backfills active descendants of deleted tasks and projects" $
      withTestSandbox $ \sandbox -> do
        migrations <- resolveMigrationsDir sandbox.sandboxRepoRoot
        withSandboxedEnv sandbox $
          withSandboxedPostgres sandbox $ \db ->
            bracket (createPool db.testDbConnStr 2 30 30000) destroyAllResources $ \pool -> do
              preV19Dir <- copyMigrationSubset sandbox migrations "pre-v019" (\name -> name < "V019")
              v19OnlyDir <- copyMigrationSubset sandbox migrations "v019-only" (== "V019__cascade_delete_task_project_subtrees.sql")

              preResult <- Migration.runMigrations pool preV19Dir
              preResult.failed `shouldBe` Nothing

              wsId <- runSession pool $
                Session.statement ("delete-cascade-migration-ws" :: Text) insertWorkspaceDirectStatement
              projectId <- runSession pool $
                Session.statement (wsId, "Delete Cascade Migration Project" :: Text) insertProjectDirectStatement

              deletedParentTaskId <- runSession pool $
                Session.statement (wsId, projectId, Nothing, "deleted parent task" :: Text, "todo" :: Text) insertTaskDirectStatement
              activeChildTaskId <- runSession pool $
                Session.statement (wsId, projectId, Just deletedParentTaskId, "active child task" :: Text, "todo" :: Text) insertTaskDirectStatement
              dependentTaskId <- runSession pool $
                Session.statement (wsId, projectId, Nothing, "dependent task" :: Text, "todo" :: Text) insertTaskDirectStatement
              runSession pool $
                Session.statement (dependentTaskId, activeChildTaskId) insertTaskDependencyDirectStatement
              runSession pool $
                Session.statement deletedParentTaskId softDeleteTaskDirectStatement

              deletedProjectId <- runSession pool $
                Session.statement (wsId, "deleted project" :: Text) insertProjectDirectStatement
              activeChildProjectId <- runSession pool $
                Session.statement (wsId, Just deletedProjectId, "active child project" :: Text) insertProjectWithParentDirectStatement
              activeProjectTaskId <- runSession pool $
                Session.statement (wsId, activeChildProjectId, Nothing, "active project task" :: Text, "todo" :: Text) insertTaskDirectStatement
              runSession pool $
                Session.statement deletedProjectId softDeleteProjectDirectStatement

              v19Result <- Migration.runMigrations pool v19OnlyDir
              v19Result.failed `shouldBe` Nothing

              runSession pool (Session.statement activeChildTaskId activeTaskExistsStatement) `shouldReturn` False
              runSession pool (Session.statement activeChildProjectId activeProjectExistsStatement) `shouldReturn` False
              runSession pool (Session.statement activeProjectTaskId activeTaskExistsStatement) `shouldReturn` False
              runSession pool (Session.statement (dependentTaskId, activeChildTaskId) taskDependencyCountStatement) `shouldReturn` 0
              runSession pool (Session.statement activeChildTaskId activeChildTaskCascadeReportCountStatement) `shouldReturn` 1
              runSession pool (Session.statement activeChildProjectId activeChildProjectCascadeReportCountStatement) `shouldReturn` 1
              runSession pool (Session.statement activeProjectTaskId projectTaskCascadeReportCountStatement) `shouldReturn` 1

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

insertMemoryWithoutTypeDirectStatement :: Statement.Statement (UUID, Text) UUID
insertMemoryWithoutTypeDirectStatement = Statement.Statement sql encoder decoder True
  where
    sql = "INSERT INTO memories (workspace_id, content) VALUES ($1, $2) RETURNING id"
    encoder =
      contramap fst (E.param (E.nonNullable E.uuid)) <>
      contramap snd (E.param (E.nonNullable E.text))
    decoder = D.singleRow (D.column (D.nonNullable D.uuid))

linkProjectMemoryDirectStatement :: Statement.Statement (UUID, UUID) ()
linkProjectMemoryDirectStatement = Statement.Statement sql encoder D.noResult True
  where
    sql = "INSERT INTO project_memory_links (project_id, memory_id) VALUES ($1, $2)"
    encoder =
      contramap fst (E.param (E.nonNullable E.uuid)) <>
      contramap snd (E.param (E.nonNullable E.uuid))

memoryTypeTextStatement :: Statement.Statement UUID Text
memoryTypeTextStatement = Statement.Statement sql encoder decoder True
  where
    sql = "SELECT memory_type::text FROM memories WHERE id = $1"
    encoder = E.param (E.nonNullable E.uuid)
    decoder = D.singleRow (D.column (D.nonNullable D.text))

forceDeferredConstraints :: Session.Session ()
forceDeferredConstraints = do
  Session.sql "SET CONSTRAINTS ALL IMMEDIATE"
  Session.sql "SET CONSTRAINTS ALL DEFERRED"

activeMemoryExistsStatement :: Statement.Statement UUID Bool
activeMemoryExistsStatement = Statement.Statement sql encoder decoder True
  where
    sql = "SELECT EXISTS (SELECT 1 FROM memories WHERE id = $1 AND deleted_at IS NULL)"
    encoder = E.param (E.nonNullable E.uuid)
    decoder = D.singleRow (D.column (D.nonNullable D.bool))

insertProjectDirectStatement :: Statement.Statement (UUID, Text) UUID
insertProjectDirectStatement = Statement.Statement sql encoder decoder True
  where
    sql = "INSERT INTO projects (workspace_id, name) VALUES ($1, $2) RETURNING id"
    encoder =
      contramap fst (E.param (E.nonNullable E.uuid)) <>
      contramap snd (E.param (E.nonNullable E.text))
    decoder = D.singleRow (D.column (D.nonNullable D.uuid))

insertProjectWithParentDirectStatement :: Statement.Statement (UUID, Maybe UUID, Text) UUID
insertProjectWithParentDirectStatement = Statement.Statement sql encoder decoder True
  where
    sql = "INSERT INTO projects (workspace_id, parent_id, name) VALUES ($1, $2, $3) RETURNING id"
    encoder =
      contramap (\(wsId, _, _) -> wsId) (E.param (E.nonNullable E.uuid)) <>
      contramap (\(_, parentId, _) -> parentId) (E.param (E.nullable E.uuid)) <>
      contramap (\(_, _, name) -> name) (E.param (E.nonNullable E.text))
    decoder = D.singleRow (D.column (D.nonNullable D.uuid))

insertTaskDirectStatement :: Statement.Statement (UUID, UUID, Maybe UUID, Text, Text) UUID
insertTaskDirectStatement = Statement.Statement sql encoder decoder True
  where
    sql = "INSERT INTO tasks (workspace_id, project_id, parent_id, title, status) VALUES ($1, $2, $3, $4, $5::task_status_enum) RETURNING id"
    encoder =
      contramap (\(wsId, _, _, _, _) -> wsId) (E.param (E.nonNullable E.uuid)) <>
      contramap (\(_, projectId, _, _, _) -> projectId) (E.param (E.nonNullable E.uuid)) <>
      contramap (\(_, _, parentId, _, _) -> parentId) (E.param (E.nullable E.uuid)) <>
      contramap (\(_, _, _, title, _) -> title) (E.param (E.nonNullable E.text)) <>
      contramap (\(_, _, _, _, status) -> status) (E.param (E.nonNullable E.text))
    decoder = D.singleRow (D.column (D.nonNullable D.uuid))

insertTaskDependencyDirectStatement :: Statement.Statement (UUID, UUID) ()
insertTaskDependencyDirectStatement = Statement.Statement sql encoder D.noResult True
  where
    sql = "INSERT INTO task_dependencies (task_id, depends_on_id) VALUES ($1, $2)"
    encoder =
      contramap fst (E.param (E.nonNullable E.uuid)) <>
      contramap snd (E.param (E.nonNullable E.uuid))

updateProjectStatusDirectStatement :: Statement.Statement (UUID, Text) ()
updateProjectStatusDirectStatement = Statement.Statement sql encoder D.noResult True
  where
    sql = "UPDATE projects SET status = $2::project_status_enum WHERE id = $1"
    encoder =
      contramap fst (E.param (E.nonNullable E.uuid)) <>
      contramap snd (E.param (E.nonNullable E.text))

projectStatusTextStatement :: Statement.Statement UUID Text
projectStatusTextStatement = Statement.Statement sql encoder decoder True
  where
    sql = "SELECT status::text FROM projects WHERE id = $1"
    encoder = E.param (E.nonNullable E.uuid)
    decoder = D.singleRow (D.column (D.nonNullable D.text))

taskStatusTextStatement :: Statement.Statement UUID Text
taskStatusTextStatement = Statement.Statement sql encoder decoder True
  where
    sql = "SELECT status::text FROM tasks WHERE id = $1"
    encoder = E.param (E.nonNullable E.uuid)
    decoder = D.singleRow (D.column (D.nonNullable D.text))

taskAutoBlockedStatement :: Statement.Statement UUID Bool
taskAutoBlockedStatement = Statement.Statement sql encoder decoder True
  where
    sql = "SELECT auto_blocked FROM tasks WHERE id = $1"
    encoder = E.param (E.nonNullable E.uuid)
    decoder = D.singleRow (D.column (D.nonNullable D.bool))

softDeleteTaskDirectStatement :: Statement.Statement UUID ()
softDeleteTaskDirectStatement = Statement.Statement sql encoder D.noResult True
  where
    sql = "UPDATE tasks SET deleted_at = now() WHERE id = $1"
    encoder = E.param (E.nonNullable E.uuid)

softDeleteProjectDirectStatement :: Statement.Statement UUID ()
softDeleteProjectDirectStatement = Statement.Statement sql encoder D.noResult True
  where
    sql = "UPDATE projects SET deleted_at = now() WHERE id = $1"
    encoder = E.param (E.nonNullable E.uuid)

hardDeleteTaskDirectStatement :: Statement.Statement UUID ()
hardDeleteTaskDirectStatement = Statement.Statement sql encoder D.noResult True
  where
    sql = "DELETE FROM tasks WHERE id = $1"
    encoder = E.param (E.nonNullable E.uuid)

setTaskParentDirectStatement :: Statement.Statement (UUID, Maybe UUID) ()
setTaskParentDirectStatement = Statement.Statement sql encoder D.noResult True
  where
    sql = "UPDATE tasks SET parent_id = $2 WHERE id = $1"
    encoder =
      contramap fst (E.param (E.nonNullable E.uuid)) <>
      contramap snd (E.param (E.nullable E.uuid))

taskParentIdStatement :: Statement.Statement UUID (Maybe UUID)
taskParentIdStatement = Statement.Statement sql encoder decoder True
  where
    sql = "SELECT parent_id FROM tasks WHERE id = $1"
    encoder = E.param (E.nonNullable E.uuid)
    decoder = D.singleRow (D.column (D.nullable D.uuid))

activeNestedSubtaskCountStatement :: Statement.Statement () Int
activeNestedSubtaskCountStatement = Statement.Statement sql E.noParams decoder True
  where
    sql = "SELECT count(*)::int FROM tasks child JOIN tasks parent ON parent.id = child.parent_id WHERE child.deleted_at IS NULL AND parent.deleted_at IS NULL AND parent.parent_id IS NOT NULL"
    decoder = D.singleRow (fromIntegral <$> D.column (D.nonNullable D.int4))

activeTaskExistsStatement :: Statement.Statement UUID Bool
activeTaskExistsStatement = Statement.Statement sql encoder decoder True
  where
    sql = "SELECT EXISTS (SELECT 1 FROM tasks WHERE id = $1 AND deleted_at IS NULL)"
    encoder = E.param (E.nonNullable E.uuid)
    decoder = D.singleRow (D.column (D.nonNullable D.bool))

activeProjectExistsStatement :: Statement.Statement UUID Bool
activeProjectExistsStatement = Statement.Statement sql encoder decoder True
  where
    sql = "SELECT EXISTS (SELECT 1 FROM projects WHERE id = $1 AND deleted_at IS NULL)"
    encoder = E.param (E.nonNullable E.uuid)
    decoder = D.singleRow (D.column (D.nonNullable D.bool))

taskDependencyCountStatement :: Statement.Statement (UUID, UUID) Int
taskDependencyCountStatement = Statement.Statement sql encoder decoder True
  where
    sql = "SELECT count(*)::int FROM task_dependencies WHERE task_id = $1 AND depends_on_id = $2"
    encoder =
      contramap fst (E.param (E.nonNullable E.uuid)) <>
      contramap snd (E.param (E.nonNullable E.uuid))
    decoder = D.singleRow (fromIntegral <$> D.column (D.nonNullable D.int4))

deletedParentReportCountStatement :: Statement.Statement UUID Int
deletedParentReportCountStatement = Statement.Statement sql encoder decoder True
  where
    sql = "SELECT count(*)::int FROM task_flatten_migration_report WHERE task_id = $1 AND issue = 'deleted_parent_detached'"
    encoder = E.param (E.nonNullable E.uuid)
    decoder = D.singleRow (fromIntegral <$> D.column (D.nonNullable D.int4))

hierarchyCycleReportCountStatement :: Statement.Statement UUID Int
hierarchyCycleReportCountStatement = Statement.Statement sql encoder decoder True
  where
    sql = "SELECT count(*)::int FROM task_flatten_migration_report WHERE task_id = $1 AND issue = 'hierarchy_cycle_detached'"
    encoder = E.param (E.nonNullable E.uuid)
    decoder = D.singleRow (fromIntegral <$> D.column (D.nonNullable D.int4))

missingParentReportDetailStatement :: Statement.Statement (UUID, UUID) Bool
missingParentReportDetailStatement = Statement.Statement sql encoder decoder True
  where
    sql = "SELECT EXISTS (SELECT 1 FROM task_flatten_migration_report WHERE task_id = $1 AND issue = 'missing_parent_detached' AND detail->>'parent_missing' = 'true' AND detail->>'legacy_parent_id' = $2::text)"
    encoder =
      contramap fst (E.param (E.nonNullable E.uuid)) <>
      contramap snd (E.param (E.nonNullable E.uuid))
    decoder = D.singleRow (D.column (D.nonNullable D.bool))

dependencyCycleSkippedReportDetailStatement :: Statement.Statement (UUID, UUID) Bool
dependencyCycleSkippedReportDetailStatement = Statement.Statement sql encoder decoder True
  where
    sql = "SELECT EXISTS (SELECT 1 FROM task_flatten_migration_report WHERE task_id = $1 AND issue = 'dependency_cycle_edge_skipped' AND detail->>'depends_on_id' = $2::text)"
    encoder =
      contramap fst (E.param (E.nonNullable E.uuid)) <>
      contramap snd (E.param (E.nonNullable E.uuid))
    decoder = D.singleRow (D.column (D.nonNullable D.bool))

activeChildTaskCascadeReportCountStatement :: Statement.Statement UUID Int
activeChildTaskCascadeReportCountStatement = Statement.Statement sql encoder decoder True
  where
    sql = "SELECT count(*)::int FROM delete_cascade_migration_report WHERE entity_type = 'task' AND entity_id = $1 AND issue = 'active_child_task_deleted_with_parent'"
    encoder = E.param (E.nonNullable E.uuid)
    decoder = D.singleRow (fromIntegral <$> D.column (D.nonNullable D.int4))

activeChildProjectCascadeReportCountStatement :: Statement.Statement UUID Int
activeChildProjectCascadeReportCountStatement = Statement.Statement sql encoder decoder True
  where
    sql = "SELECT count(*)::int FROM delete_cascade_migration_report WHERE entity_type = 'project' AND entity_id = $1 AND issue = 'active_child_project_deleted_with_parent'"
    encoder = E.param (E.nonNullable E.uuid)
    decoder = D.singleRow (fromIntegral <$> D.column (D.nonNullable D.int4))

projectTaskCascadeReportCountStatement :: Statement.Statement UUID Int
projectTaskCascadeReportCountStatement = Statement.Statement sql encoder decoder True
  where
    sql = "SELECT count(*)::int FROM delete_cascade_migration_report WHERE entity_type = 'task' AND entity_id = $1 AND issue = 'active_task_deleted_with_project'"
    encoder = E.param (E.nonNullable E.uuid)
    decoder = D.singleRow (fromIntegral <$> D.column (D.nonNullable D.int4))
