module HMem.DB.WorkspaceSpec (spec) where

import Control.Exception (try)
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
import HMem.DB.TestHarness
import HMem.DB.Workspace
import HMem.Types

spec :: Spec
spec = beforeAll setupTestPool $ aroundWith withTestTransaction $
  describe "Workspace rename persistence" $ do
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
