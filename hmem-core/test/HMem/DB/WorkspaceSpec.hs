module HMem.DB.WorkspaceSpec (spec) where

import Control.Exception (try)
import Data.Functor.Contravariant (contramap)
import Data.List (find)
import Data.Text qualified as T
import Data.UUID (UUID)
import Data.UUID.V4 qualified as UUID
import Hasql.Decoders qualified as Dec
import Hasql.Encoders qualified as Enc
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement
import Test.Hspec

import HMem.DB.Pool (DBException(..), runSession)
import HMem.DB.Project qualified as Project
import HMem.DB.Task qualified as Task
import HMem.DB.TestHarness
import HMem.DB.Workspace
import HMem.Types

spec :: Spec
spec = beforeAll setupTestPool $ aroundWith withTestTransaction $ do
  describe "Workspace planning counts" $ do
    it "counts live mixed-status nested and unassigned rows once across the whole workspace" $ \env -> do
      workspace <- createTestWorkspace env "planning-counts-mixed"
      other <- createTestWorkspace env "planning-counts-other"
      let project parent name = Project.createProject env.pool (CreateProject workspace.id parent name Nothing Nothing Nothing)
          task projectId parent title = Task.createTask env.pool (CreateTask workspace.id projectId parent title Nothing Nothing Nothing Nothing)
          status table entityId value = runSession env.pool $ Session.statement (entityId, value) $
            Statement.Statement ("UPDATE " <> table <> " SET status = $2::" <> (if table == "projects" then "project_status_enum" else "task_status_enum") <> " WHERE id = $1")
              (contramap fst (Enc.param (Enc.nonNullable Enc.uuid)) <> contramap snd (Enc.param (Enc.nonNullable Enc.text))) Dec.noResult True
          deleted table entityId = runSession env.pool $ Session.statement entityId $
            Statement.Statement ("UPDATE " <> table <> " SET deleted_at = now() WHERE id = $1")
              (Enc.param (Enc.nonNullable Enc.uuid)) Dec.noResult True
      root <- project Nothing "root"
      child <- project (Just root.id) "child"
      paused <- project (Just root.id) "paused"
      completed <- project Nothing "completed"
      archived <- project Nothing "archived"
      removedProject <- project Nothing "deleted"
      status "projects" paused.id "paused"
      status "projects" completed.id "completed"
      status "projects" archived.id "archived"
      deleted "projects" removedProject.id
      parent <- task (Just root.id) Nothing "parent"
      _ <- task (Just root.id) (Just parent.id) "subtask"
      assigned <- task (Just child.id) Nothing "assigned"
      running <- task Nothing Nothing "running"
      manualBlocked <- task Nothing Nothing "manual blocked"
      done <- task Nothing Nothing "done"
      cancelled <- task Nothing Nothing "cancelled"
      removedTask <- task Nothing Nothing "deleted"
      status "tasks" running.id "in_progress"
      status "tasks" manualBlocked.id "blocked"
      status "tasks" done.id "done"
      status "tasks" cancelled.id "cancelled"
      deleted "tasks" removedTask.id
      Task.addDependency env.pool assigned.id manualBlocked.id
      Task.addDependency env.pool assigned.id running.id
      _ <- Project.createProject env.pool (CreateProject other.id Nothing "foreign" Nothing Nothing Nothing)
      _ <- Task.createTask env.pool (CreateTask other.id Nothing Nothing "foreign" Nothing Nothing Nothing Nothing)
      getWorkspacePlanningCounts env.pool workspace.id `shouldReturn` Just (WorkspacePlanningCounts 3 5)
      getWorkspacePlanningCounts env.pool other.id `shouldReturn` Just (WorkspacePlanningCounts 1 1)

    it "returns zeros for an empty workspace and no result for missing or deleted workspaces" $ \env -> do
      workspace <- createTestWorkspace env "planning-counts-empty"
      missing <- UUID.nextRandom
      getWorkspacePlanningCounts env.pool workspace.id `shouldReturn` Just (WorkspacePlanningCounts 0 0)
      getWorkspacePlanningCounts env.pool missing `shouldReturn` Nothing
      deleteWorkspace env.pool workspace.id `shouldReturn` True
      getWorkspacePlanningCounts env.pool workspace.id `shouldReturn` Nothing

  describe "Workspace rename persistence" $ do
    it "soft-deletes once and keeps the existing workspace audit lifecycle" $ \env -> do
      workspace <- createTestWorkspace env "workspace-delete-target"
      deleteWorkspace env.pool workspace.id `shouldReturn` True
      deleteWorkspace env.pool workspace.id `shouldReturn` False
      listActiveWorkspaces env.pool 100 0 >>= (`shouldNotContain` [workspace])
      audit <- getAuditLogRows env.pool "workspace" (T.pack (show workspace.id))
      audit `shouldSatisfy` (not . null)

    it "persists the canonical rename after reload while preserving every other workspace field" $ \env -> do
      original <- createTestWorkspace env "workspace-before-rename"

      renamed <- renameWorkspace env.pool original.id (UpdateWorkspace "workspace-after-rename")
      reloaded <- reloadWorkspace env original.id

      renamed `shouldBe` Just reloaded
      reloaded.name `shouldBe` "workspace-after-rename"
      reloaded.id `shouldBe` original.id
      reloaded.workspaceType `shouldBe` original.workspaceType
      reloaded.ghOwner `shouldBe` original.ghOwner
      reloaded.ghRepo `shouldBe` original.ghRepo
      reloaded.createdAt `shouldBe` original.createdAt
      reloaded.updatedAt `shouldBe` original.updatedAt

    it "rejects blank and overlong names before changing the active workspace" $ \env -> do
      workspace <- createTestWorkspace env "workspace-validation-target"
      assertRejectedRename env workspace (UpdateWorkspace "   ")
      assertRejectedRename env workspace (UpdateWorkspace (T.replicate 1025 "x"))

    it "reports missing and deleted workspaces without updating either" $ \env -> do
      missingId <- UUID.nextRandom
      renameWorkspace env.pool missingId (UpdateWorkspace "missing-rename") `shouldReturn` Nothing

      workspace <- createTestWorkspace env "workspace-deleted-target"
      runSession env.pool $ Session.statement workspace.id softDeleteWorkspaceStatement
      renameWorkspace env.pool workspace.id (UpdateWorkspace "deleted-rename") `shouldReturn` Nothing
      listActiveWorkspaces env.pool 100 0 >>= (`shouldNotContain` [workspace])

    it "allows duplicate display names while retaining each workspace identity" $ \env -> do
      first <- createTestWorkspace env "workspace-duplicate-source"
      second <- createTestWorkspace env "workspace-duplicate-target"

      renamed <- renameWorkspace env.pool first.id (UpdateWorkspace second.name)
      reloadedFirst <- reloadWorkspace env first.id
      reloadedSecond <- reloadWorkspace env second.id

      renamed `shouldBe` Just reloadedFirst
      reloadedFirst.name `shouldBe` reloadedSecond.name
      reloadedFirst.id `shouldBe` first.id
      reloadedSecond.id `shouldBe` second.id

    it "does not create an audit entry for a name-only no-op" $ \env -> do
      workspace <- createTestWorkspace env "workspace-rename-noop"
      beforeAudit <- getAuditLogRows env.pool "workspace" (T.pack (show workspace.id))

      renameWorkspace env.pool workspace.id (UpdateWorkspace workspace.name) `shouldReturn` Just workspace

      afterAudit <- getAuditLogRows env.pool "workspace" (T.pack (show workspace.id))
      afterAudit `shouldBe` beforeAudit

assertRejectedRename :: TestEnv -> Workspace -> UpdateWorkspace -> Expectation
assertRejectedRename env workspace input = do
  prior <- reloadWorkspace env workspace.id
  result <- try @DBException $ renameWorkspace env.pool workspace.id input
  result `shouldSatisfy` \case
    Left (DBCheckViolation _) -> True
    _ -> False
  reloadWorkspace env workspace.id `shouldReturn` prior

reloadWorkspace :: TestEnv -> UUID -> IO Workspace
reloadWorkspace env workspaceId = do
  active <- listActiveWorkspaces env.pool 100 0
  case find ((== workspaceId) . (.id)) active of
    Just workspace -> pure workspace
    Nothing -> expectationFailure ("workspace was not present after reload: " <> show workspaceId) >> fail "unreachable"

softDeleteWorkspaceStatement :: Statement.Statement UUID ()
softDeleteWorkspaceStatement = Statement.Statement
  "UPDATE workspaces SET deleted_at = now() WHERE id = $1"
  (Enc.param (Enc.nonNullable Enc.uuid))
  Dec.noResult
  True
