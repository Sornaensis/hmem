module HMem.DB.TestHarnessSpec (spec) where

import Control.Concurrent.Async (withAsync, wait)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (SomeException, bracket, try)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as B8
import Data.Functor.Contravariant (contramap)
import Data.List (isInfixOf, sort)
import Data.Pool (Pool, destroyAllResources)
import Data.Text (Text)
import Data.Int (Int64)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Text qualified as T
import Data.UUID (UUID)
import Hasql.Decoders qualified as D
import Hasql.Encoders qualified as E
import Hasql.Connection qualified as Hasql
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement
import System.Directory (copyFile, createDirectoryIfMissing, doesDirectoryExist, listDirectory, removeDirectoryRecursive, withCurrentDirectory)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.FilePath ((</>), takeDirectory)
import System.IO.Temp (getCanonicalTemporaryDirectory)
import Test.Hspec

import HMem.DB.Migration qualified as Migration
import HMem.DB.Pool (DBException(..), createPool, runSession, runTransaction, withConn)
import HMem.DB.TestHarness
import HMem.ObservationSubjectMatchCorpus (observationSubjectMatchCorpus)
import HMem.Types (SubjectKind, subjectKindToText)

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
                Left (DBOtherError _) -> pure ()
                Left other -> expectationFailure $ "Expected historical V013 rejection, got: " <> show other
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
                Left (DBOtherError _) -> pure ()
                Left other -> expectationFailure $ "Expected historical V015 rejection, got: " <> show other
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

    it "observationMigration destructively replaces legacy memories without affecting projects or tasks" $
      withTestSandbox $ \sandbox -> do
        migrations <- resolveMigrationsDir sandbox.sandboxRepoRoot
        withSandboxedEnv sandbox $
          withSandboxedPostgres sandbox $ \db ->
            bracket (createPool db.testDbConnStr 2 30 30000) destroyAllResources $ \pool -> do
              preV20Dir <- copyMigrationSubset sandbox migrations "pre-v020" (\name -> name < "V020")
              v20OnlyDir <- copyMigrationSubset sandbox migrations "v020-only" (== "V020__replace_memories_with_observations.sql")

              preResult <- Migration.runMigrations pool preV20Dir
              preResult.failed `shouldBe` Nothing
              runTransaction pool $ Session.sql $ B8.unlines
                [ "INSERT INTO workspaces (name) VALUES ('observation migration workspace');"
                , "INSERT INTO projects (workspace_id, name) SELECT id, 'preserved project' FROM workspaces WHERE name = 'observation migration workspace';"
                , "INSERT INTO projects (workspace_id, name) SELECT id, 'unrelated project' FROM workspaces WHERE name = 'observation migration workspace';"
                , "INSERT INTO tasks (workspace_id, project_id, title) SELECT w.id, p.id, 'preserved task' FROM workspaces w JOIN projects p ON p.workspace_id = w.id WHERE w.name = 'observation migration workspace' AND p.name = 'preserved project';"
                , "INSERT INTO tasks (workspace_id, project_id, title) SELECT w.id, p.id, 'unrelated task' FROM workspaces w JOIN projects p ON p.workspace_id = w.id WHERE w.name = 'observation migration workspace' AND p.name = 'unrelated project';"
                , "INSERT INTO memories (workspace_id, content, memory_type) SELECT id, 'legacy secret content', 'short_term' FROM workspaces WHERE name = 'observation migration workspace';"
                , "INSERT INTO memories (workspace_id, content, memory_type) SELECT id, 'another legacy secret', 'long_term' FROM workspaces WHERE name = 'observation migration workspace';"
                , "INSERT INTO project_memory_links (project_id, memory_id) SELECT p.id, m.id FROM projects p JOIN memories m ON m.workspace_id = p.workspace_id WHERE p.name = 'preserved project';"
                , "INSERT INTO task_memory_links (task_id, memory_id) SELECT t.id, m.id FROM tasks t JOIN memories m ON m.workspace_id = t.workspace_id WHERE t.title = 'preserved task' LIMIT 1;"
                , "INSERT INTO memory_categories (workspace_id, name) SELECT id, 'legacy category' FROM workspaces WHERE name = 'observation migration workspace';"
                , "INSERT INTO memory_category_links (memory_id, category_id) SELECT m.id, c.id FROM memories m CROSS JOIN memory_categories c LIMIT 1;"
                , "INSERT INTO memory_tags (memory_id, tag) SELECT id, 'legacy' FROM memories;"
                , "INSERT INTO memory_links (source_id, target_id, relation_type) SELECT source.id, target.id, 'related' FROM memories source JOIN memories target ON target.id <> source.id LIMIT 1;"
                , "INSERT INTO cleanup_policies (workspace_id, memory_type) SELECT id, 'short_term' FROM workspaces WHERE name = 'observation migration workspace';"
                , "INSERT INTO saved_views (workspace_id, name, entity_type) SELECT id, 'legacy memories', 'memory_search' FROM workspaces WHERE name = 'observation migration workspace';"
                , "INSERT INTO saved_views (workspace_id, name, entity_type) SELECT id, 'activity', 'activity' FROM workspaces WHERE name = 'observation migration workspace';"
                , "INSERT INTO workspace_groups (name, description) VALUES ('unrelated catalog', 'must survive');"
                , "INSERT INTO audit_log (entity_type, entity_id, action, old_values) VALUES ('project', 'unrelated-content-audit', 'update', '{\"content\": \"unrelated audit payload\"}'::jsonb);"
                ]

              -- A failure after destructive statements must roll back the whole
              -- V020 transaction, leave its ledger entry absent, and permit a
              -- retry on the same pool after the conflicting fixture is removed.
              runSession pool $ Session.sql "CREATE TABLE observations (id UUID)"
              failedV20 <- Migration.runMigrations pool v20OnlyDir
              failedV20.failed `shouldSatisfy` maybe False (const True)
              runSession pool (queryBool "SELECT to_regclass('public.memories') IS NOT NULL AND EXISTS (SELECT 1 FROM memories WHERE content = 'legacy secret content') AND NOT EXISTS (SELECT 1 FROM schema_migrations WHERE version = 20)") `shouldReturn` True
              runSession pool $ Session.sql "DROP TABLE observations"

              v20Result <- Migration.runMigrations pool v20OnlyDir
              v20Result.failed `shouldBe` Nothing
              rerunV20 <- Migration.runMigrations pool v20OnlyDir
              rerunV20.applied `shouldBe` []
              rerunV20.skipped `shouldBe` ["V020__replace_memories_with_observations.sql"]

              runSession pool (queryBool "SELECT NOT EXISTS (SELECT 1 FROM pg_class WHERE relnamespace = 'public'::regnamespace AND relname = ANY (ARRAY['memories', 'memory_categories', 'memory_tags', 'memory_category_links', 'memory_links', 'project_memory_links', 'task_memory_links', 'cleanup_policies']))") `shouldReturn` True
              runSession pool (queryBool "SELECT NOT EXISTS (SELECT 1 FROM pg_proc WHERE proname = ANY (ARRAY['hmem_validate_memory_creation_link', 'hmem_check_memory_creation_link_from_memory', 'hmem_check_memory_creation_link_from_project_link', 'hmem_check_memory_creation_link_from_task_link', 'hmem_check_memory_creation_link_from_project_target', 'hmem_check_memory_creation_link_from_task_target', 'hmem_require_explicit_memory_type', 'hmem_memories_search_vector', 'hmem_memory_tags_reindex', 'hmem_check_category_cycle', 'hmem_soft_delete_memories_for_deleted_links'])) AND NOT EXISTS (SELECT 1 FROM pg_type WHERE typname = ANY (ARRAY['memory_type_enum', 'relation_type_enum'])) AND NOT EXISTS (SELECT 1 FROM pg_class WHERE relnamespace = 'public'::regnamespace AND relname = ANY (ARRAY['uq_global_category_name', 'idx_memories_workspace', 'idx_memories_workspace_type', 'idx_memories_workspace_importance', 'idx_memories_expires', 'idx_memories_last_accessed', 'idx_memories_created', 'idx_memories_metadata', 'idx_memories_search', 'idx_memories_pinned', 'idx_memories_workspace_id', 'idx_memories_embedding', 'idx_memory_tags_tag', 'idx_memory_tags_covering', 'idx_memory_categories_ws', 'idx_memory_categories_parent', 'idx_memory_links_target', 'idx_memory_links_source_relation', 'idx_project_mem_links_memory', 'idx_task_mem_links_memory', 'idx_project_memory_links_memory', 'idx_task_memory_links_memory', 'idx_memories_workspace_pinned'])) AND NOT EXISTS (SELECT 1 FROM pg_trigger WHERE NOT tgisinternal AND tgname = ANY (ARRAY['trg_memories_search_vector', 'trg_memory_tags_reindex', 'trg_memories_updated_at', 'trg_memory_creation_link_required', 'trg_memory_type_required', 'trg_memories_audit', 'trg_memory_categories_audit', 'trg_memory_tags_audit', 'trg_memory_category_links_audit', 'trg_memory_links_audit', 'trg_project_memory_links_audit', 'trg_task_memory_links_audit', 'trg_cleanup_policies_audit', 'trg_project_memory_target_valid', 'trg_task_memory_target_valid'])) AND position('memories' IN pg_get_functiondef('hmem_audit_change'::regproc)) = 0 AND position('memories' IN pg_get_functiondef('hmem_cascade_task_soft_delete'::regproc)) = 0 AND position('memories' IN pg_get_functiondef('hmem_cascade_project_soft_delete'::regproc)) = 0") `shouldReturn` True
              runSession pool (queryBool "SELECT count(*) = 2 FROM projects WHERE name IN ('preserved project', 'unrelated project')") `shouldReturn` True
              runSession pool (queryBool "SELECT count(*) = 2 FROM tasks WHERE title IN ('preserved task', 'unrelated task')") `shouldReturn` True
              runSession pool (queryBool "SELECT count(*) = 1 FROM workspaces WHERE name = 'observation migration workspace'") `shouldReturn` True
              runSession pool (queryBool "SELECT EXISTS (SELECT 1 FROM schema_migrations WHERE version = 20 AND name = 'V020__replace_memories_with_observations.sql')") `shouldReturn` True
              runSession pool (queryBool "SELECT NOT EXISTS (SELECT 1 FROM audit_log WHERE entity_type IN ('memory', 'memory_tag', 'memory_category', 'memory_category_link', 'memory_link', 'project_memory_link', 'task_memory_link', 'cleanup_policy')) AND EXISTS (SELECT 1 FROM audit_log WHERE entity_id = 'unrelated-content-audit' AND old_values ->> 'content' = 'unrelated audit payload')") `shouldReturn` True
              runSession pool (queryBool "SELECT NOT EXISTS (SELECT 1 FROM saved_views WHERE entity_type IN ('memory_search', 'memory_list')) AND EXISTS (SELECT 1 FROM saved_views WHERE entity_type = 'activity') AND EXISTS (SELECT 1 FROM workspace_groups WHERE name = 'unrelated catalog')") `shouldReturn` True
              runSession pool (queryBool "SELECT pg_get_constraintdef(oid) = 'CHECK ((entity_type = ANY (ARRAY[''observation_search''::text, ''observation_list''::text, ''project_list''::text, ''task_list''::text, ''activity''::text])))' FROM pg_constraint WHERE conrelid = 'saved_views'::regclass AND conname = 'chk_saved_views_entity_type'") `shouldReturn` True
              runSession pool (queryBool "SELECT position('octet_length(subject) >= 1' IN pg_get_constraintdef(oid)) > 0 AND position('octet_length(subject) <= 4096' IN pg_get_constraintdef(oid)) > 0 FROM pg_constraint WHERE conrelid = 'observations'::regclass AND conname = 'chk_observations_subject_octet_length'") `shouldReturn` True
              runSession pool (queryBool "SELECT position('octet_length(content) >= 1' IN pg_get_constraintdef(oid)) > 0 AND position('octet_length(content) <= 524288' IN pg_get_constraintdef(oid)) > 0 FROM pg_constraint WHERE conrelid = 'observations'::regclass AND conname = 'chk_observations_content_octet_length'") `shouldReturn` True

              runSession pool $ Session.sql "INSERT INTO observations (workspace_id, subject_kind, subject, git_sha, content) SELECT id, 'file', 'src/Main.hs', '0123456789abcdef0123456789abcdef01234567', 'first needle' FROM workspaces WHERE name = 'observation migration workspace'"
              runSession pool (queryBool "SELECT search_vector @@ plainto_tsquery('simple', 'needle') FROM observations WHERE subject = 'src/Main.hs'") `shouldReturn` True
              runSession pool (queryBool "SELECT (SELECT count(*) FROM pg_trigger WHERE tgrelid = 'observations'::regclass AND NOT tgisinternal AND tgname IN ('trg_observations_search_vector', 'trg_observations_provenance_immutable', 'trg_observations_updated_at', 'trg_observations_audit')) = 4") `shouldReturn` True
              runSession pool (queryBool "SELECT EXISTS (SELECT 1 FROM audit_log WHERE entity_type = 'observation')") `shouldReturn` True
              runSession pool (queryBool "SELECT (SELECT count(*) FROM pg_indexes WHERE tablename = 'observations' AND indexname IN ('idx_observations_workspace_git_sha', 'idx_observations_workspace_subject_kind_subject', 'idx_observations_workspace_git_sha_subject', 'idx_observations_search')) = 4") `shouldReturn` True
              runSession pool (queryBool "SELECT pg_get_indexdef('idx_observations_workspace_git_sha'::regclass) LIKE '%(workspace_id, git_sha)%' AND pg_get_indexdef('idx_observations_workspace_subject_kind_subject'::regclass) LIKE '%(workspace_id, subject_kind, subject)%' AND pg_get_indexdef('idx_observations_workspace_git_sha_subject'::regclass) LIKE '%(workspace_id, git_sha, subject_kind, subject)%'") `shouldReturn` True
              runSession pool (queryBool "SELECT (SELECT count(*) FROM information_schema.columns WHERE table_name = 'observations') = 9 + CASE WHEN EXISTS (SELECT 1 FROM pg_extension WHERE extname = 'vector') THEN 1 ELSE 0 END") `shouldReturn` True
              runSession pool (queryBool "SELECT (EXISTS (SELECT 1 FROM pg_extension WHERE extname = 'vector')) = EXISTS (SELECT 1 FROM information_schema.columns WHERE table_name = 'observations' AND column_name = 'embedding') AND (NOT EXISTS (SELECT 1 FROM pg_extension WHERE extname = 'vector') OR to_regclass('public.idx_observations_embedding') IS NOT NULL)") `shouldReturn` True

              badSha <- try (runSession pool $ Session.sql "INSERT INTO observations (workspace_id, subject_kind, subject, git_sha, content) SELECT id, 'glob', '*.hs', 'ABCDEF', 'bad SHA' FROM workspaces LIMIT 1") :: IO (Either DBException ())
              badSha `shouldSatisfy` either (const True) (const False)
              emptySubject <- try (runSession pool $ Session.sql "INSERT INTO observations (workspace_id, subject_kind, subject, git_sha, content) SELECT id, 'glob', '', '0123456789abcdef0123456789abcdef01234567', 'empty subject' FROM workspaces LIMIT 1") :: IO (Either DBException ())
              emptySubject `shouldSatisfy` either (const True) (const False)
              emptyContent <- try (runSession pool $ Session.sql "INSERT INTO observations (workspace_id, subject_kind, subject, git_sha, content) SELECT id, 'glob', '*.hs', '0123456789abcdef0123456789abcdef01234567', '' FROM workspaces LIMIT 1") :: IO (Either DBException ())
              emptyContent `shouldSatisfy` either (const True) (const False)
              longSubject <- try (runSession pool $ Session.sql $ B8.concat ["INSERT INTO observations (workspace_id, subject_kind, subject, git_sha, content) SELECT id, 'file', '", B8.replicate 4097 's', "', '0123456789abcdef0123456789abcdef01234567', 'long subject' FROM workspaces LIMIT 1"]) :: IO (Either DBException ())
              longSubject `shouldSatisfy` either (const True) (const False)
              longContent <- try (runSession pool $ Session.sql $ B8.concat ["INSERT INTO observations (workspace_id, subject_kind, subject, git_sha, content) SELECT id, 'file', 'long-content', '0123456789abcdef0123456789abcdef01234567', '", B8.replicate 524289 'c', "' FROM workspaces LIMIT 1"]) :: IO (Either DBException ())
              longContent `shouldSatisfy` either (const True) (const False)
              runSession pool $ Session.sql "INSERT INTO workspaces (name) VALUES ('other observation workspace')"
              changedWorkspace <- try (runSession pool $ Session.sql "UPDATE observations SET workspace_id = (SELECT id FROM workspaces WHERE name = 'other observation workspace') WHERE subject = 'src/Main.hs'") :: IO (Either DBException ())
              changedWorkspace `shouldSatisfy` either (const True) (const False)
              changedSubjectKind <- try (runSession pool $ Session.sql "UPDATE observations SET subject_kind = 'glob' WHERE subject = 'src/Main.hs'") :: IO (Either DBException ())
              changedSubjectKind `shouldSatisfy` either (const True) (const False)
              changedSubject <- try (runSession pool $ Session.sql "UPDATE observations SET subject = 'src/Other.hs' WHERE subject = 'src/Main.hs'") :: IO (Either DBException ())
              changedSubject `shouldSatisfy` either (const True) (const False)
              changedProvenance <- try (runSession pool $ Session.sql "UPDATE observations SET git_sha = 'fedcba9876543210fedcba9876543210fedcba98' WHERE subject = 'src/Main.hs'") :: IO (Either DBException ())
              changedProvenance `shouldSatisfy` either (const True) (const False)
              runSession pool $ Session.sql "UPDATE observations SET content = 'second needle' WHERE subject = 'src/Main.hs'"
              runSession pool (queryBool "SELECT search_vector @@ plainto_tsquery('simple', 'second') FROM observations WHERE subject = 'src/Main.hs'") `shouldReturn` True

    it "observationMigration fresh chain omits pgvector artifacts when vector is not installed" $
      withTestSandbox $ \sandbox -> do
        migrations <- resolveMigrationsDir sandbox.sandboxRepoRoot
        withSandboxedEnv sandbox $
          withSandboxedPostgres sandbox $ \db ->
            bracket (createPool db.testDbConnStr 2 30 30000) destroyAllResources $ \pool -> do
              preV20Dir <- copyMigrationSubset sandbox migrations "pre-v020-without-vector" (\name -> name < "V020")
              v20OnlyDir <- copyMigrationSubset sandbox migrations "v020-only-without-vector" (== "V020__replace_memories_with_observations.sql")
              preResult <- Migration.runMigrations pool preV20Dir
              preResult.failed `shouldBe` Nothing
              -- A version entry with a different filename is a ledger error,
              -- not an idempotent skip; no destructive SQL may run in that case.
              runSession pool $ Session.sql "INSERT INTO schema_migrations (version, name) VALUES (20, 'V020__wrong_name.sql')"
              wrongLedger <- Migration.runMigrations pool v20OnlyDir
              case wrongLedger.failed of
                Just (file, message) -> do
                  file `shouldBe` "V020__replace_memories_with_observations.sql"
                  message `shouldSatisfy` isInfixOf "V020__wrong_name.sql"
                Nothing -> expectationFailure "Expected conflicting V020 migration ledger entry to fail"
              runSession pool (queryBool "SELECT to_regclass('public.memories') IS NOT NULL") `shouldReturn` True
              runSession pool $ Session.sql "DELETE FROM schema_migrations WHERE version = 20"
              runSession pool $ Session.sql "DROP INDEX IF EXISTS idx_memories_embedding; ALTER TABLE memories DROP COLUMN IF EXISTS embedding; DROP EXTENSION IF EXISTS vector"
              v20Result <- Migration.runMigrations pool v20OnlyDir
              v20Result.failed `shouldBe` Nothing
              runSession pool (queryBool "SELECT to_regclass('public.observations') IS NOT NULL AND EXISTS (SELECT 1 FROM pg_type WHERE typname = 'observation_subject_kind') AND NOT EXISTS (SELECT 1 FROM pg_type WHERE typname IN ('memory_type_enum', 'relation_type_enum'))") `shouldReturn` True
              runSession pool (queryBool "SELECT NOT EXISTS (SELECT 1 FROM pg_extension WHERE extname = 'vector') AND NOT EXISTS (SELECT 1 FROM information_schema.columns WHERE table_name = 'observations' AND column_name = 'embedding') AND to_regclass('public.idx_observations_embedding') IS NULL") `shouldReturn` True

    it "observationSubjectSetMigration upgrades populated V020 observations without losing recursive-glob provenance" $
      withTestSandbox $ \sandbox -> do
        migrations <- resolveMigrationsDir sandbox.sandboxRepoRoot
        withSandboxedEnv sandbox $
          withSandboxedPostgres sandbox $ \db ->
            bracket (createPool db.testDbConnStr 2 30 30000) destroyAllResources $ \pool -> do
              preV20 <- copyMigrationSubset sandbox migrations "pre-v020-subject-set" (\name -> name < "V020")
              v20 <- copyMigrationSubset sandbox migrations "v020-subject-set" (== "V020__replace_memories_with_observations.sql")
              v21 <- copyMigrationSubset sandbox migrations "v021-subject-set" (== "V021__observation_subject_sets.sql")
              Migration.runMigrations pool preV20 >>= (\result -> result.failed `shouldBe` Nothing)
              Migration.runMigrations pool v20 >>= (\result -> result.failed `shouldBe` Nothing)
              runSession pool $ Session.sql "INSERT INTO workspaces (name) VALUES ('subject-set-upgrade')"
              runSession pool $ Session.sql "INSERT INTO observations (id, workspace_id, subject_kind, subject, git_sha, content, created_at, updated_at) SELECT '00000000-0000-0000-0000-000000000021', id, 'glob', 'src/**/*.hs', '0123456789abcdef0123456789abcdef01234567', 'recursive provenance', '2001-02-03T04:05:06Z', '2001-02-03T04:05:06Z' FROM workspaces WHERE name = 'subject-set-upgrade'"
              runSession pool $ Session.sql "INSERT INTO observations (id, workspace_id, subject_kind, subject, git_sha, content) SELECT '00000000-0000-0000-0000-000000000023', id, 'file', 'src/Deleted.hs', '0123456789abcdef0123456789abcdef01234567', 'deleted provenance' FROM workspaces WHERE name = 'subject-set-upgrade'"
              runSession pool $ Session.sql "UPDATE observations SET content = 'deleted provenance revised' WHERE id = '00000000-0000-0000-0000-000000000023'"
              runSession pool $ Session.sql "DELETE FROM observations WHERE id = '00000000-0000-0000-0000-000000000023'"
              runSession pool $ Session.sql "DO $$ BEGIN IF EXISTS (SELECT 1 FROM pg_extension WHERE extname = 'vector') THEN EXECUTE format('UPDATE observations SET embedding = %L::vector WHERE id = %L', '[' || repeat('0,', 1535) || '1]', '00000000-0000-0000-0000-000000000021'); END IF; END $$"
              runSession pool $ Session.sql "UPDATE observations SET content = 'recursive provenance revised' WHERE id = '00000000-0000-0000-0000-000000000021'"
              runSession pool $ Session.sql "CREATE TABLE v021_fixture_before AS SELECT id, created_at, updated_at FROM observations WHERE id = '00000000-0000-0000-0000-000000000021'"
              Migration.runMigrations pool v21 >>= (\result -> result.failed `shouldBe` Nothing)
              runSession pool (queryBool "SELECT NOT EXISTS (SELECT 1 FROM information_schema.columns WHERE table_name = 'observations' AND column_name IN ('subject_kind', 'subject'))") `shouldReturn` True
              runSession pool (queryBool "SELECT EXISTS (SELECT 1 FROM observation_subjects s JOIN observations o ON o.id = s.observation_id WHERE o.id = '00000000-0000-0000-0000-000000000021' AND s.subject_kind = 'glob' AND s.subject = 'src/**/*.hs' AND s.ordinal = 0 AND o.content = 'recursive provenance revised')") `shouldReturn` True
              runSession pool (queryBool "SELECT o.created_at = f.created_at AND o.updated_at = f.updated_at FROM observations o JOIN v021_fixture_before f ON f.id = o.id") `shouldReturn` True
              runSession pool $ Session.sql "DROP TABLE v021_fixture_before"
              runSession pool (queryBool "SELECT (SELECT count(*) FROM audit_log WHERE entity_type = 'observation' AND entity_id = '00000000-0000-0000-0000-000000000021' AND action = 'create') = 1") `shouldReturn` True
              runSession pool (queryBool "SELECT (SELECT count(*) FROM audit_log WHERE entity_type = 'observation' AND entity_id = '00000000-0000-0000-0000-000000000021' AND action = 'update') = 1") `shouldReturn` True
              runSession pool (queryBool "SELECT (SELECT count(*) FROM audit_log WHERE entity_type = 'observation' AND entity_id = '00000000-0000-0000-0000-000000000021' AND action = 'create' AND new_values->'subjects' = '[{\"subject\": \"src/**/*.hs\", \"subject_kind\": \"glob\"}]'::jsonb AND new_values->>'subject_kind' = 'glob' AND new_values->>'subject' = 'src/**/*.hs') = 1") `shouldReturn` True
              runSession pool (queryBool "SELECT (SELECT count(*) FROM audit_log WHERE entity_type = 'observation' AND entity_id = '00000000-0000-0000-0000-000000000021' AND action = 'update' AND old_values->'subjects' = '[{\"subject\": \"src/**/*.hs\", \"subject_kind\": \"glob\"}]'::jsonb AND new_values->'subjects' = '[{\"subject\": \"src/**/*.hs\", \"subject_kind\": \"glob\"}]'::jsonb AND old_values->>'subject' = 'src/**/*.hs' AND new_values->>'subject' = 'src/**/*.hs') = 1") `shouldReturn` True
              runSession pool (queryBool "SELECT (SELECT count(*) FROM audit_log WHERE entity_type = 'observation' AND entity_id = '00000000-0000-0000-0000-000000000023') = 3 AND (SELECT count(*) FROM audit_log WHERE entity_type = 'observation' AND entity_id = '00000000-0000-0000-0000-000000000023' AND action = 'create' AND old_values IS NULL AND new_values->'subjects' = '[{\"subject\": \"src/Deleted.hs\", \"subject_kind\": \"file\"}]'::jsonb AND new_values->>'subject_kind' = 'file' AND new_values->>'subject' = 'src/Deleted.hs') = 1 AND (SELECT count(*) FROM audit_log WHERE entity_type = 'observation' AND entity_id = '00000000-0000-0000-0000-000000000023' AND action = 'update' AND old_values->'subjects' = '[{\"subject\": \"src/Deleted.hs\", \"subject_kind\": \"file\"}]'::jsonb AND new_values->'subjects' = '[{\"subject\": \"src/Deleted.hs\", \"subject_kind\": \"file\"}]'::jsonb AND old_values->>'subject_kind' = 'file' AND old_values->>'subject' = 'src/Deleted.hs' AND new_values->>'subject_kind' = 'file' AND new_values->>'subject' = 'src/Deleted.hs') = 1 AND (SELECT count(*) FROM audit_log WHERE entity_type = 'observation' AND entity_id = '00000000-0000-0000-0000-000000000023' AND action = 'delete' AND old_values->'subjects' = '[{\"subject\": \"src/Deleted.hs\", \"subject_kind\": \"file\"}]'::jsonb AND old_values->>'subject_kind' = 'file' AND old_values->>'subject' = 'src/Deleted.hs' AND new_values IS NULL) = 1") `shouldReturn` True
              runSession pool $ Session.sql "WITH inserted AS (INSERT INTO observations (id, workspace_id, git_sha, content, subject_set_open) SELECT '00000000-0000-0000-0000-000000000022', id, '0123456789abcdef0123456789abcdef01234567', 'multi-subject audit', TRUE FROM workspaces WHERE name = 'subject-set-upgrade' RETURNING id), inserted_subjects AS (INSERT INTO observation_subjects (observation_id, ordinal, subject_kind, subject) SELECT inserted.id, entries.ordinal, entries.subject_kind::observation_subject_kind, entries.subject FROM inserted CROSS JOIN (VALUES (0, 'glob', 'src/**/*.hs'), (1, 'file', 'README.md')) AS entries(ordinal, subject_kind, subject) RETURNING observation_id) SELECT 1 FROM inserted_subjects"
              runSession pool (queryBool "SELECT (SELECT count(*) FROM audit_log WHERE entity_type = 'observation' AND entity_id = '00000000-0000-0000-0000-000000000022' AND action = 'create' AND new_values->'subjects' = '[{\"subject\": \"src/**/*.hs\", \"subject_kind\": \"glob\"}, {\"subject\": \"README.md\", \"subject_kind\": \"file\"}]'::jsonb AND new_values->>'subject_kind' = 'glob' AND new_values->>'subject' = 'src/**/*.hs') = 1") `shouldReturn` True
              runSession pool $ Session.sql "UPDATE observations SET content = 'multi-subject audit revised' WHERE id = '00000000-0000-0000-0000-000000000022'"
              runSession pool (queryBool "SELECT (SELECT count(*) FROM audit_log WHERE entity_type = 'observation' AND entity_id = '00000000-0000-0000-0000-000000000022' AND action = 'update' AND old_values->'subjects' = '[{\"subject\": \"src/**/*.hs\", \"subject_kind\": \"glob\"}, {\"subject\": \"README.md\", \"subject_kind\": \"file\"}]'::jsonb AND new_values->'subjects' = '[{\"subject\": \"src/**/*.hs\", \"subject_kind\": \"glob\"}, {\"subject\": \"README.md\", \"subject_kind\": \"file\"}]'::jsonb AND old_values->>'subject' = 'src/**/*.hs' AND new_values->>'subject' = 'src/**/*.hs') = 1") `shouldReturn` True
              runSession pool $ Session.sql "DELETE FROM observations WHERE id = '00000000-0000-0000-0000-000000000022'"
              runSession pool (queryBool "SELECT (SELECT count(*) FROM audit_log WHERE entity_type = 'observation' AND entity_id = '00000000-0000-0000-0000-000000000022' AND action = 'delete' AND old_values->'subjects' = '[{\"subject\": \"src/**/*.hs\", \"subject_kind\": \"glob\"}, {\"subject\": \"README.md\", \"subject_kind\": \"file\"}]'::jsonb AND old_values->>'subject_kind' = 'glob' AND old_values->>'subject' = 'src/**/*.hs') = 1 AND (SELECT count(*) FROM audit_log WHERE entity_type = 'observation' AND entity_id = '00000000-0000-0000-0000-000000000022') = 3") `shouldReturn` True
              runSession pool $ Session.sql "DO $$ DECLARE preserved BOOLEAN; BEGIN IF EXISTS (SELECT 1 FROM pg_extension WHERE extname = 'vector') THEN EXECUTE format('SELECT embedding <-> %L::vector = 0 FROM observations WHERE id = %L', '[' || repeat('0,', 1535) || '1]', '00000000-0000-0000-0000-000000000021') INTO preserved; IF NOT preserved THEN RAISE EXCEPTION 'embedding was not preserved'; END IF; END IF; END $$"
              mapM_ (assertSqlSubjectMatch pool) observationSubjectMatchCorpus
              runSession pool (queryBool "SELECT to_regclass('idx_observation_subjects_exact') IS NOT NULL AND EXISTS (SELECT 1 FROM pg_constraint WHERE conrelid = 'observation_subjects'::regclass AND conname IN ('uq_observation_subjects_kind_subject', 'chk_observation_subjects_ordinal', 'chk_observation_subjects_length'))") `shouldReturn` True
              immutable <- try (runSession pool $ Session.sql "UPDATE observation_subjects SET subject = 'src/Other.hs'") :: IO (Either DBException ())
              immutable `shouldSatisfy` either (const True) (const False)
              inserted <- try (runSession pool $ Session.sql "INSERT INTO observation_subjects (observation_id, ordinal, subject_kind, subject) SELECT id, 1, 'file', 'README.md' FROM observations") :: IO (Either DBException ())
              inserted `shouldSatisfy` either (const True) (const False)
              removed <- try (runSession pool $ Session.sql "DELETE FROM observation_subjects") :: IO (Either DBException ())
              removed `shouldSatisfy` either (const True) (const False)
              runSession pool $ Session.sql "DELETE FROM observations"
              runSession pool (queryBool "SELECT NOT EXISTS (SELECT 1 FROM observation_subjects) AND (SELECT count(*) FROM audit_log WHERE entity_type = 'observation' AND entity_id = '00000000-0000-0000-0000-000000000021' AND action = 'delete') = 1") `shouldReturn` True

    it "observationMigration fresh chain creates pgvector artifacts when vector is available" $
      withTestSandbox $ \sandbox -> do
        migrations <- resolveMigrationsDir sandbox.sandboxRepoRoot
        withSandboxedEnv sandbox $
          withSandboxedPostgres sandbox $ \db ->
            bracket (createPool db.testDbConnStr 2 30 30000) destroyAllResources $ \pool -> do
              available <- runSession pool (queryBool "SELECT EXISTS (SELECT 1 FROM pg_available_extensions WHERE name = 'vector')")
              if not available
                then do
                  -- CI images without pgvector still verify the exact present
                  -- branch; pgvector-enabled images exercise it end-to-end.
                  migrationSql <- readFile (migrations </> "V020__replace_memories_with_observations.sql")
                  migrationSql `shouldSatisfy` isInfixOf "ALTER TABLE observations ADD COLUMN embedding vector(1536);"
                  migrationSql `shouldSatisfy` isInfixOf "ON observations USING hnsw (embedding vector_cosine_ops);"
                else do
                  result <- Migration.runMigrations pool migrations
                  result.failed `shouldBe` Nothing
                  runSession pool (queryBool "SELECT EXISTS (SELECT 1 FROM pg_extension WHERE extname = 'vector') AND EXISTS (SELECT 1 FROM information_schema.columns WHERE table_name = 'observations' AND column_name = 'embedding')") `shouldReturn` True
                  runSession pool (queryBool "SELECT format_type(a.atttypid, a.atttypmod) = 'vector(1536)' FROM pg_attribute a WHERE a.attrelid = 'observations'::regclass AND a.attname = 'embedding' AND NOT a.attisdropped") `shouldReturn` True
                  runSession pool (queryBool "SELECT pg_get_indexdef('idx_observations_embedding'::regclass) = 'CREATE INDEX idx_observations_embedding ON public.observations USING hnsw (embedding vector_cosine_ops)'") `shouldReturn` True

    it "upgrades a large reconvergent V021 dependency graph through V022 within the configured bound" $
      withTestSandbox $ \sandbox -> do
        migrations <- resolveMigrationsDir sandbox.sandboxRepoRoot
        withSandboxedEnv sandbox $
          withSandboxedPostgres sandbox $ \db ->
            bracket (createPool db.testDbConnStr 2 30 25000) destroyAllResources $ \pool -> do
              preV22Dir <- copyMigrationSubset sandbox migrations "pre-v022-bounded-cycle" (\name -> name < "V022")
              v22ThroughCurrentDir <- copyMigrationSubset sandbox migrations "v022-through-current-bounded-cycle" (\name -> name >= "V022")
              preResult <- Migration.runMigrations pool preV22Dir
              preResult.failed `shouldBe` Nothing
              runSession pool $ Session.sql reconvergentDependencyGraphSql
              runSession pool (queryInt "SELECT count(*) FROM task_dependencies") `shouldReturn` 2268
              runSession pool (queryInt "SELECT count(*) FROM audit_log WHERE entity_type = 'task_dependency' AND action = 'update'") `shouldReturn` 0

              upgradeResult <- Migration.runMigrations pool v22ThroughCurrentDir
              upgradeResult.failed `shouldBe` Nothing
              upgradeResult.applied `shouldSatisfy` elem "V022__change_stream_outbox.sql"
              upgradeResult.applied `shouldSatisfy` elem "V027__bounded_task_dependency_cycle_check.sql"
              runSession pool (queryBool "SELECT count(*) = 1 FROM schema_migrations WHERE version = 22 AND name = 'V022__change_stream_outbox.sql'") `shouldReturn` True
              runSession pool (queryBool "SELECT count(*) = 2268 FROM task_dependencies WHERE workspace_id = '10000000-0000-0000-0000-000000000001'") `shouldReturn` True
              runSession pool (queryInt "SELECT count(*) FROM audit_log WHERE entity_type = 'task_dependency' AND action = 'update'") `shouldReturn` 2268
              runSession pool (queryInt "SELECT count(*) FROM change_stream_outbox WHERE envelope->'entity'->>'type' = 'task_dependency'") `shouldReturn` 0
              assertBoundedTaskDependencyCycleDDL pool

              wsId <- runSession pool $ Session.statement ("v022-post-upgrade-workspace" :: Text) insertWorkspaceDirectStatement
              projectId <- runSession pool $ Session.statement (wsId, "v022-post-upgrade-project" :: Text) insertProjectDirectStatement
              firstId <- runSession pool $ Session.statement (wsId, projectId, Nothing, "first" :: Text, "todo" :: Text) insertTaskDirectStatement
              secondId <- runSession pool $ Session.statement (wsId, projectId, Nothing, "second" :: Text, "todo" :: Text) insertTaskDirectStatement
              thirdId <- runSession pool $ Session.statement (wsId, projectId, Nothing, "third" :: Text, "done" :: Text) insertTaskDirectStatement
              auditBefore <- runSession pool (queryInt "SELECT count(*) FROM audit_log WHERE entity_type = 'task_dependency'")
              outboxBefore <- runSession pool (queryInt "SELECT count(*) FROM change_stream_outbox WHERE envelope->'entity'->>'type' = 'task_dependency'")
              runSession pool $ Session.statement (firstId, secondId) insertTaskDependencyDirectStatement
              runSession pool (Session.statement firstId taskStatusTextStatement) `shouldReturn` "blocked"
              runSession pool (Session.statement firstId taskAutoBlockedStatement) `shouldReturn` True
              runSession pool $ Session.statement (secondId, thirdId) insertTaskDependencyDirectStatement
              cycleUpdateAuditBefore <- runSession pool (queryInt "SELECT count(*) FROM audit_log WHERE entity_type = 'task_dependency'")
              cycleUpdateOutboxBefore <- runSession pool (queryInt "SELECT count(*) FROM change_stream_outbox WHERE envelope->'entity'->>'type' = 'task_dependency'")
              cycleUpdate <- try (runSession pool $ Session.statement (secondId, thirdId, firstId) updateTaskDependencyEndpointStatement)
                :: IO (Either DBException ())
              cycleUpdate `shouldSatisfy` \case
                Left (DBTaskDependencyCycle _) -> True
                _ -> False
              runSession pool (Session.statement (secondId, thirdId) taskDependencyExistsStatement) `shouldReturn` True
              runSession pool (Session.statement (secondId, firstId) taskDependencyExistsStatement) `shouldReturn` False
              runSession pool (Session.statement firstId taskStatusTextStatement) `shouldReturn` "blocked"
              runSession pool (Session.statement firstId taskAutoBlockedStatement) `shouldReturn` True
              runSession pool (Session.statement secondId taskStatusTextStatement) `shouldReturn` "todo"
              runSession pool (Session.statement secondId taskAutoBlockedStatement) `shouldReturn` False
              runSession pool (queryInt "SELECT count(*) FROM audit_log WHERE entity_type = 'task_dependency'") `shouldReturn` cycleUpdateAuditBefore
              runSession pool (queryInt "SELECT count(*) FROM change_stream_outbox WHERE envelope->'entity'->>'type' = 'task_dependency'") `shouldReturn` cycleUpdateOutboxBefore
              runSession pool $ Session.statement (firstId, secondId, thirdId) updateTaskDependencyEndpointStatement
              runSession pool (Session.statement firstId taskStatusTextStatement) `shouldReturn` "todo"
              runSession pool (Session.statement firstId taskAutoBlockedStatement) `shouldReturn` False
              runSession pool $ Session.statement (firstId, thirdId, secondId) updateTaskDependencyEndpointStatement
              runSession pool (Session.statement firstId taskStatusTextStatement) `shouldReturn` "blocked"
              runSession pool (Session.statement firstId taskAutoBlockedStatement) `shouldReturn` True
              runSession pool $ Session.statement (firstId, wsId) updateTaskDependencyWorkspaceOnlyStatement
              runSession pool (Session.statement firstId taskStatusTextStatement) `shouldReturn` "blocked"
              cycleResult <- try (runSession pool $ Session.statement (secondId, firstId) insertTaskDependencyDirectStatement)
                :: IO (Either DBException ())
              cycleResult `shouldSatisfy` \case
                Left (DBTaskDependencyCycle _) -> True
                _ -> False
              runSession pool $ Session.statement (firstId, secondId) deleteTaskDependencyDirectStatement
              runSession pool (Session.statement firstId taskStatusTextStatement) `shouldReturn` "todo"
              runSession pool (Session.statement firstId taskAutoBlockedStatement) `shouldReturn` False
              runSession pool $ Session.statement (secondId, thirdId) deleteTaskDependencyDirectStatement
              auditAfter <- runSession pool (queryInt "SELECT count(*) FROM audit_log WHERE entity_type = 'task_dependency'")
              outboxAfter <- runSession pool (queryInt "SELECT count(*) FROM change_stream_outbox WHERE envelope->'entity'->>'type' = 'task_dependency'")
              auditAfter - auditBefore `shouldBe` 6
              outboxAfter - outboxBefore `shouldBe` 4
              runSession pool (queryBool "SELECT NOT EXISTS (SELECT 1 FROM task_dependencies WHERE task_id IN (SELECT id FROM tasks WHERE title IN ('first', 'second', 'third')))") `shouldReturn` True

              rerun <- Migration.runMigrations pool v22ThroughCurrentDir
              rerun.failed `shouldBe` Nothing
              rerun.applied `shouldBe` []

    it "rolls back a blocked V022 compatibility preflight and succeeds on retry" $
      withTestSandbox $ \sandbox -> do
        migrations <- resolveMigrationsDir sandbox.sandboxRepoRoot
        withSandboxedEnv sandbox $
          withSandboxedPostgres sandbox $ \db -> do
            preV22Dir <- copyMigrationSubset sandbox migrations "pre-v022-preflight-rollback" (\name -> name < "V022")
            v22OnlyDir <- copyMigrationSubset sandbox migrations "v022-only-preflight-rollback" (== "V022__change_stream_outbox.sql")
            bracket (createPool db.testDbConnStr 2 30 30000) destroyAllResources $ \setupPool -> do
              preResult <- Migration.runMigrations setupPool preV22Dir
              preResult.failed `shouldBe` Nothing
              beforeFunction <- runSession setupPool (queryText "SELECT pg_get_functiondef('hmem_check_task_dep_cycle'::regproc)")
              beforeTrigger <- runSession setupPool (queryText "SELECT pg_get_triggerdef(oid) FROM pg_trigger WHERE tgrelid = 'task_dependencies'::regclass AND tgname = 'trg_task_dep_no_cycle'")
              beforeAutoBlockTrigger <- runSession setupPool (queryText "SELECT pg_get_triggerdef(oid) FROM pg_trigger WHERE tgrelid = 'task_dependencies'::regclass AND tgname = 'trg_task_auto_blocking_from_dependency'")
              beforeFunction `shouldSatisfy` T.isInfixOf "UNION ALL"
              beforeTrigger `shouldSatisfy` T.isInfixOf "BEFORE INSERT OR UPDATE ON"

              runSession setupPool $ Session.sql "UPDATE schema_migrations SET name = 'renamed-v021.sql' WHERE version = 21"
              unsupportedLedger <- Migration.runMigrations setupPool v22OnlyDir
              unsupportedLedger.failed `shouldSatisfy` \case
                Just ("V022__change_stream_outbox.sql", message) ->
                  "refused a non-canonical ledger" `isInfixOf` message
                _ -> False
              runSession setupPool (queryText "SELECT pg_get_functiondef('hmem_check_task_dep_cycle'::regproc)") `shouldReturn` beforeFunction
              runSession setupPool (queryText "SELECT pg_get_triggerdef(oid) FROM pg_trigger WHERE tgrelid = 'task_dependencies'::regclass AND tgname = 'trg_task_dep_no_cycle'") `shouldReturn` beforeTrigger
              runSession setupPool (queryText "SELECT pg_get_triggerdef(oid) FROM pg_trigger WHERE tgrelid = 'task_dependencies'::regclass AND tgname = 'trg_task_auto_blocking_from_dependency'") `shouldReturn` beforeAutoBlockTrigger
              runSession setupPool $ Session.sql "UPDATE schema_migrations SET name = 'V021__observation_subject_sets.sql' WHERE version = 21"

              bracket (createPool db.testDbConnStr 1 30 30000) destroyAllResources $ \lockPool ->
                bracket (createPool db.testDbConnStr 1 30 1000) destroyAllResources $ \migrationPool -> do
                  ready <- newEmptyMVar
                  release <- newEmptyMVar
                  withAsync (holdDependencyTableLock lockPool ready release) $ \locker -> do
                    takeMVar ready >>= \case
                      Left err -> expectationFailure $ "failed to acquire test lock: " <> err
                      Right () -> pure ()
                    failedResult <- Migration.runMigrations migrationPool v22OnlyDir
                    failedResult.failed `shouldSatisfy` \case
                      Just ("V022__change_stream_outbox.sql", message) ->
                        "compatibility preflight failed and was rolled back" `isInfixOf` message
                      _ -> False
                    runSession setupPool (queryText "SELECT pg_get_functiondef('hmem_check_task_dep_cycle'::regproc)") `shouldReturn` beforeFunction
                    runSession setupPool (queryText "SELECT pg_get_triggerdef(oid) FROM pg_trigger WHERE tgrelid = 'task_dependencies'::regclass AND tgname = 'trg_task_dep_no_cycle'") `shouldReturn` beforeTrigger
                    runSession setupPool (queryText "SELECT pg_get_triggerdef(oid) FROM pg_trigger WHERE tgrelid = 'task_dependencies'::regclass AND tgname = 'trg_task_auto_blocking_from_dependency'") `shouldReturn` beforeAutoBlockTrigger
                    runSession setupPool (queryBool "SELECT NOT EXISTS (SELECT 1 FROM schema_migrations WHERE version = 22) AND to_regclass('public.change_stream_outbox') IS NULL") `shouldReturn` True
                    putMVar release ()
                    wait locker >>= \case
                      Left err -> expectationFailure $ "failed to release test lock: " <> err
                      Right () -> pure ()
                    retryResult <- Migration.runMigrations migrationPool v22OnlyDir
                    retryResult.failed `shouldBe` Nothing
                    retryResult.applied `shouldBe` ["V022__change_stream_outbox.sql"]

              runSession setupPool (queryBool "SELECT EXISTS (SELECT 1 FROM schema_migrations WHERE version = 22 AND name = 'V022__change_stream_outbox.sql')") `shouldReturn` True
              assertBoundedTaskDependencyCycleDDL setupPool

    it "converges a V026 database and a fresh database on the V027 cycle guard" $
      withTestSandbox $ \sandbox -> do
        migrations <- resolveMigrationsDir sandbox.sandboxRepoRoot
        withSandboxedEnv sandbox $
          withSandboxedPostgres sandbox $ \db ->
            bracket (createPool db.testDbConnStr 2 30 30000) destroyAllResources $ \pool -> do
              throughV26Dir <- copyMigrationSubset sandbox migrations "through-v026-cycle-convergence" (\name -> name < "V027")
              v27OnlyDir <- copyMigrationSubset sandbox migrations "v027-only-cycle-convergence" (== "V027__bounded_task_dependency_cycle_check.sql")
              throughV26 <- Migration.runMigrations pool throughV26Dir
              throughV26.failed `shouldBe` Nothing
              runSession pool (queryBool "SELECT position('UNION ALL' IN pg_get_functiondef('hmem_check_task_dep_cycle'::regproc)) > 0") `shouldReturn` True
              v27 <- Migration.runMigrations pool v27OnlyDir
              v27.failed `shouldBe` Nothing
              v27.applied `shouldBe` ["V027__bounded_task_dependency_cycle_check.sql"]
              assertBoundedTaskDependencyCycleDDL pool

    it "cleanDB truncates migration reports after the Observation schema reset" $
      withTestSandbox $ \sandbox -> do
        migrations <- resolveMigrationsDir sandbox.sandboxRepoRoot
        withSandboxedEnv sandbox $
          withSandboxedPostgres sandbox $ \db ->
            bracket (createPool db.testDbConnStr 2 30 30000) destroyAllResources $ \pool -> do
              result <- Migration.runMigrations pool migrations
              result.failed `shouldBe` Nothing
              assertBoundedTaskDependencyCycleDDL pool
              runSession pool $ Session.sql "INSERT INTO delete_cascade_migration_report (entity_type, entity_id, issue) VALUES ('task', gen_random_uuid(), 'reset-fixture')"
              cleanDB TestEnv { pool = pool, testSandbox = sandbox, testDb = db }
              runSession pool (queryBool "SELECT NOT EXISTS (SELECT 1 FROM delete_cascade_migration_report)") `shouldReturn` True

    it "resolves the repository root from a non-repo cwd" $
      withTestSandbox $ \sandbox ->
        withCurrentDirectory sandbox.sandboxTmpDir $ do
          repoRoot <- resolveRepoRoot
          repoRoot `shouldBe` sandbox.sandboxRepoRoot

queryBool :: BS.ByteString -> Session.Session Bool
queryBool sql = Session.statement () $ Statement.Statement sql E.noParams (D.singleRow (D.column (D.nonNullable D.bool))) True

queryInt :: BS.ByteString -> Session.Session Int64
queryInt sql = Session.statement () $ Statement.Statement sql E.noParams (D.singleRow (D.column (D.nonNullable D.int8))) True

queryText :: BS.ByteString -> Session.Session Text
queryText sql = Session.statement () $ Statement.Statement sql E.noParams (D.singleRow (D.column (D.nonNullable D.text))) True

assertBoundedTaskDependencyCycleDDL :: Pool Hasql.Connection -> IO ()
assertBoundedTaskDependencyCycleDDL pool = do
  runSession pool (queryBool "SELECT position('UNION ALL' IN pg_get_functiondef('hmem_check_task_dep_cycle'::regproc)) = 0 AND position(E'\\n      UNION\\n' IN pg_get_functiondef('hmem_check_task_dep_cycle'::regproc)) > 0 AND position('HD301' IN pg_get_functiondef('hmem_check_task_dep_cycle'::regproc)) > 0") `shouldReturn` True
  runSession pool (queryBool "SELECT pg_get_triggerdef(oid) LIKE '%BEFORE INSERT OR UPDATE OF task_id, depends_on_id ON public.task_dependencies%' FROM pg_trigger WHERE tgrelid = 'task_dependencies'::regclass AND tgname = 'trg_task_dep_no_cycle' AND NOT tgisinternal") `shouldReturn` True
  runSession pool (queryBool "SELECT pg_get_triggerdef(oid) LIKE '%AFTER INSERT OR DELETE OR UPDATE OF task_id, depends_on_id ON public.task_dependencies%' FROM pg_trigger WHERE tgrelid = 'task_dependencies'::regclass AND tgname = 'trg_task_auto_blocking_from_dependency' AND NOT tgisinternal") `shouldReturn` True

holdDependencyTableLock :: Pool Hasql.Connection -> MVar (Either String ()) -> MVar () -> IO (Either String ())
holdDependencyTableLock pool ready release = withConn pool $ \conn -> do
  locked <- Session.run (Session.sql "BEGIN; LOCK TABLE task_dependencies IN ACCESS SHARE MODE") conn
  case locked of
    Left err -> putMVar ready (Left (show err)) >> pure (Left (show err))
    Right () -> do
      putMVar ready (Right ())
      takeMVar release
      Session.run (Session.sql "ROLLBACK") conn >>= \case
        Left err -> pure (Left (show err))
        Right () -> pure (Right ())

reconvergentDependencyGraphSql :: BS.ByteString
reconvergentDependencyGraphSql =
  "INSERT INTO workspaces (id, name) VALUES ('10000000-0000-0000-0000-000000000001', 'v022 bounded graph');\n\
  \INSERT INTO projects (id, workspace_id, name) VALUES ('10000000-0000-0000-0000-000000000002', '10000000-0000-0000-0000-000000000001', 'v022 bounded graph');\n\
  \CREATE TEMP TABLE v022_nodes(layer integer NOT NULL, slot integer NOT NULL, id uuid PRIMARY KEY);\n\
  \INSERT INTO v022_nodes(layer, slot, id) SELECT layer, slot, md5('v022-node-' || layer || '-' || slot)::uuid FROM generate_series(0, 63) layer CROSS JOIN generate_series(0, 5) slot;\n\
  \INSERT INTO tasks(id, workspace_id, project_id, title, status) SELECT id, '10000000-0000-0000-0000-000000000001', '10000000-0000-0000-0000-000000000002', 'bounded node ' || layer || '-' || slot, 'todo' FROM v022_nodes;\n\
  \ALTER TABLE task_dependencies DISABLE TRIGGER trg_task_auto_blocking_from_dependency;\n\
  \DO $$ DECLARE layer_index integer; BEGIN FOR layer_index IN REVERSE 63..1 LOOP INSERT INTO task_dependencies(task_id, depends_on_id) SELECT upper_node.id, lower_node.id FROM v022_nodes upper_node CROSS JOIN v022_nodes lower_node WHERE upper_node.layer = layer_index AND lower_node.layer = layer_index - 1; END LOOP; END $$;\n\
  \ALTER TABLE task_dependencies ENABLE TRIGGER trg_task_auto_blocking_from_dependency"

assertSqlSubjectMatch :: Pool Hasql.Connection -> (SubjectKind, Text, Text, Bool) -> IO ()
assertSqlSubjectMatch pool (kind, pattern, path, expected) = do
  actual <- runSession pool (Session.statement (subjectKindToText kind, pattern, path) subjectMatchStatement)
  if actual == expected
    then pure ()
    else expectationFailure $ "SQL glob corpus mismatch for " <> show (kind, pattern, path) <> ": expected " <> show expected <> ", got " <> show actual

subjectMatchStatement :: Statement.Statement (Text, Text, Text) Bool
subjectMatchStatement = Statement.Statement
  "SELECT hmem_observation_subject_matches($1::observation_subject_kind, $2, $3)"
  ((contramap (\(kind, _, _) -> kind) (E.param (E.nonNullable E.text)))
    <> (contramap (\(_, pattern, _) -> pattern) (E.param (E.nonNullable E.text)))
    <> (contramap (\(_, _, path) -> path) (E.param (E.nonNullable E.text))))
  (D.singleRow (D.column (D.nonNullable D.bool)))
  True

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

taskDependencyExistsStatement :: Statement.Statement (UUID, UUID) Bool
taskDependencyExistsStatement = Statement.Statement sql encoder decoder True
  where
    sql = "SELECT EXISTS (SELECT 1 FROM task_dependencies WHERE task_id = $1 AND depends_on_id = $2)"
    encoder =
      contramap fst (E.param (E.nonNullable E.uuid)) <>
      contramap snd (E.param (E.nonNullable E.uuid))
    decoder = D.singleRow (D.column (D.nonNullable D.bool))

updateTaskDependencyEndpointStatement :: Statement.Statement (UUID, UUID, UUID) ()
updateTaskDependencyEndpointStatement = Statement.Statement sql encoder D.noResult True
  where
    sql = "UPDATE task_dependencies SET depends_on_id = $3 WHERE task_id = $1 AND depends_on_id = $2"
    encoder =
      contramap (\(taskId, _, _) -> taskId) (E.param (E.nonNullable E.uuid)) <>
      contramap (\(_, oldDependencyId, _) -> oldDependencyId) (E.param (E.nonNullable E.uuid)) <>
      contramap (\(_, _, newDependencyId) -> newDependencyId) (E.param (E.nonNullable E.uuid))

updateTaskDependencyWorkspaceOnlyStatement :: Statement.Statement (UUID, UUID) ()
updateTaskDependencyWorkspaceOnlyStatement = Statement.Statement sql encoder D.noResult True
  where
    sql = "UPDATE task_dependencies SET workspace_id = $2 WHERE task_id = $1"
    encoder =
      contramap fst (E.param (E.nonNullable E.uuid)) <>
      contramap snd (E.param (E.nonNullable E.uuid))

deleteTaskDependencyDirectStatement :: Statement.Statement (UUID, UUID) ()
deleteTaskDependencyDirectStatement = Statement.Statement sql encoder D.noResult True
  where
    sql = "DELETE FROM task_dependencies WHERE task_id = $1 AND depends_on_id = $2"
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
