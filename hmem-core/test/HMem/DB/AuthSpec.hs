{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module HMem.DB.AuthSpec (spec) where

import Data.Aeson (object)
import Data.Functor.Contravariant (contramap)
import Data.Pool (Pool)
import Data.Text (Text)
import Data.Text qualified as T
import Data.UUID (UUID)
import Hasql.Connection qualified as Hasql
import Hasql.Decoders qualified as Dec
import Hasql.Encoders qualified as Enc
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement
import Test.Hspec

import HMem.DB.Auth
import HMem.DB.Category qualified as Cat
import HMem.DB.Memory qualified as Mem
import HMem.DB.Pool qualified as DBPool
import HMem.DB.Project qualified as Proj
import HMem.DB.RequestContext
import HMem.DB.SavedView qualified as SV
import HMem.DB.Task qualified as Task
import HMem.DB.TestHarness
import HMem.Types

spec :: Spec
spec = beforeAll setupTestPool $ aroundWith withTestTransaction $ do
  describe "workspace role ordering" $ do
    it "treats admin as edit/read and edit as read" $ \_env -> do
      WorkspaceRoleAdmin `roleSatisfies` WorkspaceRoleRead `shouldBe` True
      WorkspaceRoleAdmin `roleSatisfies` WorkspaceRoleEdit `shouldBe` True
      WorkspaceRoleEdit `roleSatisfies` WorkspaceRoleRead `shouldBe` True
      WorkspaceRoleRead `roleSatisfies` WorkspaceRoleEdit `shouldBe` False

  describe "authorization policy helpers" $ do
    it "allows local synthetic superadmin principals without stored grants" $ \env -> do
      ws <- createTestWorkspace env "authz-local"
      let localPrincipal = Principal
            { actorType = ActorUser
            , actorId = "local-user"
            , actorLabel = "Local User"
            , authority = PrincipalSyntheticLocalSuperadmin
            }

      hasGlobalPermission env.pool (Just localPrincipal) GlobalCreateWorkspace `shouldReturn` True
      hasGlobalPermission env.pool (Just localPrincipal) GlobalSuperadmin `shouldReturn` True
      hasWorkspaceRole env.pool (Just localPrincipal) ws.id WorkspaceRoleAdmin `shouldReturn` True
      authorizeScope env.pool (Just localPrincipal) EntityGlobalScope WorkspaceRoleAdmin `shouldReturn` Right ()

    it "evaluates stored user grants and workspace roles" $ \env -> do
      ws <- createTestWorkspace env "authz-membership"
      userId <- createUser env.pool True False
      addMembership env.pool ws.id userId WorkspaceRoleEdit
      let deployedPrincipal = Principal
            { actorType = ActorUser
            , actorId = textUuid userId
            , actorLabel = "Deployed User"
            , authority = PrincipalGrantUser userId
            }

      getUserGrants env.pool userId `shouldReturn` Just UserGrants
        { userCanCreateWorkspace = True
        , userIsSuperadmin = False
        }
      getWorkspaceRole env.pool ws.id userId `shouldReturn` Just WorkspaceRoleEdit
      hasGlobalPermission env.pool (Just deployedPrincipal) GlobalCreateWorkspace `shouldReturn` True
      hasGlobalPermission env.pool (Just deployedPrincipal) GlobalSuperadmin `shouldReturn` False
      hasWorkspaceRole env.pool (Just deployedPrincipal) ws.id WorkspaceRoleRead `shouldReturn` True
      hasWorkspaceRole env.pool (Just deployedPrincipal) ws.id WorkspaceRoleEdit `shouldReturn` True
      hasWorkspaceRole env.pool (Just deployedPrincipal) ws.id WorkspaceRoleAdmin `shouldReturn` False

    it "uses stored grant-bearing users for bot principals" $ \env -> do
      ws <- createTestWorkspace env "authz-bot-grants"
      userId <- createUser env.pool False False
      addMembership env.pool ws.id userId WorkspaceRoleRead
      let botPrincipal = Principal
            { actorType = ActorBot
            , actorId = "bot-token-1"
            , actorLabel = "Agent Bot"
            , authority = PrincipalGrantUser userId
            }

      hasGlobalPermission env.pool (Just botPrincipal) GlobalCreateWorkspace `shouldReturn` False
      hasWorkspaceRole env.pool (Just botPrincipal) ws.id WorkspaceRoleRead `shouldReturn` True
      hasWorkspaceRole env.pool (Just botPrincipal) ws.id WorkspaceRoleEdit `shouldReturn` False

    it "allows stored superadmins to bypass workspace membership" $ \env -> do
      ws <- createTestWorkspace env "authz-db-superadmin"
      userId <- createUser env.pool False True
      let superadminPrincipal = Principal
            { actorType = ActorUser
            , actorId = textUuid userId
            , actorLabel = "Stored Superadmin"
            , authority = PrincipalGrantUser userId
            }

      hasGlobalPermission env.pool (Just superadminPrincipal) GlobalSuperadmin `shouldReturn` True
      hasGlobalPermission env.pool (Just superadminPrincipal) GlobalCreateWorkspace `shouldReturn` True
      hasWorkspaceRole env.pool (Just superadminPrincipal) ws.id WorkspaceRoleAdmin `shouldReturn` True

    it "denies missing principals" $ \env -> do
      ws <- createTestWorkspace env "authz-missing"
      authorizeGlobal env.pool Nothing GlobalCreateWorkspace `shouldReturn` Left MissingPrincipal
      authorizeWorkspace env.pool Nothing ws.id WorkspaceRoleRead `shouldReturn` Left MissingPrincipal

    it "requires superadmin for global scopes" $ \env -> do
      userId <- createUser env.pool False False
      let principal = Principal
            { actorType = ActorUser
            , actorId = textUuid userId
            , actorLabel = "Plain User"
            , authority = PrincipalGrantUser userId
            }

      authorizeScope env.pool (Just principal) EntityGlobalScope WorkspaceRoleRead
        `shouldReturn` Left GlobalScopeRequiresSuperadmin

  describe "entity scope resolution" $ do
    it "resolves representative entity IDs to workspace or global scopes" $ \env -> do
      ws <- createTestWorkspace env "authz-resolution"
      mem <- Mem.createMemory env.pool CreateMemory
        { workspaceId = ws.id
        , content = "scoped memory"
        , summary = Nothing
        , memoryType = ShortTerm
        , importance = Nothing
        , metadata = Nothing
        , expiresAt = Nothing
        , source = Nothing
        , confidence = Nothing
        , pinned = Nothing
        , tags = Nothing
        , ftsLanguage = Nothing
        }
      project <- Proj.createProject env.pool CreateProject
        { workspaceId = ws.id
        , name = "scoped project"
        , description = Nothing
        , parentId = Nothing
        , priority = Nothing
        , metadata = Nothing
        }
      task <- Task.createTask env.pool CreateTask
        { workspaceId = ws.id
        , projectId = Just project.id
        , parentId = Nothing
        , title = "scoped task"
        , description = Nothing
        , priority = Nothing
        , metadata = Nothing
        , dueAt = Nothing
        }
      workspaceCategory <- Cat.createCategory env.pool CreateMemoryCategory
        { workspaceId = Just ws.id
        , name = "workspace category"
        , description = Nothing
        , parentId = Nothing
        }
      globalCategory <- Cat.createCategory env.pool CreateMemoryCategory
        { workspaceId = Nothing
        , name = "global category"
        , description = Nothing
        , parentId = Nothing
        }
      savedView <- SV.createSavedView env.pool CreateSavedView
        { workspaceId = ws.id
        , name = "scoped saved view"
        , description = Nothing
        , entityType = "memory_search"
        , queryParams = object []
        }
      policyId <- createCleanupPolicy env.pool ws.id

      resolveEntityScope env.pool EntityWorkspace ws.id `shouldReturn` Just (EntityWorkspaceScope ws.id)
      resolveEntityScope env.pool EntityMemory mem.id `shouldReturn` Just (EntityWorkspaceScope ws.id)
      resolveEntityScope env.pool EntityProject project.id `shouldReturn` Just (EntityWorkspaceScope ws.id)
      resolveEntityScope env.pool EntityTask task.id `shouldReturn` Just (EntityWorkspaceScope ws.id)
      resolveEntityScope env.pool EntityCategory workspaceCategory.id `shouldReturn` Just (EntityWorkspaceScope ws.id)
      resolveEntityScope env.pool EntityCategory globalCategory.id `shouldReturn` Just EntityGlobalScope
      resolveEntityScope env.pool EntitySavedView savedView.id `shouldReturn` Just (EntityWorkspaceScope ws.id)
      resolveEntityScope env.pool EntityCleanupPolicy policyId `shouldReturn` Just (EntityWorkspaceScope ws.id)

    it "resolves soft-deleted entities and reports missing entity scopes" $ \env -> do
      ws <- createTestWorkspace env "authz-deleted-resolution"
      mem <- Mem.createMemory env.pool CreateMemory
        { workspaceId = ws.id
        , content = "deleted but still scoped"
        , summary = Nothing
        , memoryType = ShortTerm
        , importance = Nothing
        , metadata = Nothing
        , expiresAt = Nothing
        , source = Nothing
        , confidence = Nothing
        , pinned = Nothing
        , tags = Nothing
        , ftsLanguage = Nothing
        }

      Mem.deleteMemory env.pool mem.id `shouldReturn` True
      resolveEntityScope env.pool EntityMemory mem.id `shouldReturn` Just (EntityWorkspaceScope ws.id)
      resolveEntityScopeRequired env.pool EntityMemory ws.id
        `shouldReturn` Left (EntityScopeNotFound EntityMemory ws.id)

createUser :: Pool Hasql.Connection -> Bool -> Bool -> IO UUID
createUser pool canCreateWorkspace isSuperadmin =
  runRaw pool $ Session.statement (canCreateWorkspace, isSuperadmin) createUserStatement

addMembership :: Pool Hasql.Connection -> UUID -> UUID -> WorkspaceRole -> IO ()
addMembership pool workspaceId userId role =
  runRaw pool $ Session.statement (workspaceId, userId, roleToText role) addMembershipStatement

createCleanupPolicy :: Pool Hasql.Connection -> UUID -> IO UUID
createCleanupPolicy pool workspaceId =
  runRaw pool $ Session.statement workspaceId createCleanupPolicyStatement

runRaw :: Pool Hasql.Connection -> Session.Session a -> IO a
runRaw pool sess = do
  -- The test harness uses a single outer transaction.  Use runSession so
  -- these fixture writes share the same request-context and savepoint path
  -- as production DB helpers.
  DBPool.runSession pool sess

createUserStatement :: Statement.Statement (Bool, Bool) UUID
createUserStatement = Statement.Statement sql encoder decoder True
  where
    sql = "INSERT INTO users (can_create_workspace, is_superadmin) VALUES ($1, $2) RETURNING id"
    encoder =
      contramap fst (Enc.param (Enc.nonNullable Enc.bool)) <>
      contramap snd (Enc.param (Enc.nonNullable Enc.bool))
    decoder = Dec.singleRow (Dec.column (Dec.nonNullable Dec.uuid))

addMembershipStatement :: Statement.Statement (UUID, UUID, Text) ()
addMembershipStatement = Statement.Statement sql encoder Dec.noResult True
  where
    sql = "INSERT INTO workspace_memberships (workspace_id, user_id, role) VALUES ($1, $2, $3::workspace_role_enum)"
    encoder =
      contramap (\(a,_,_) -> a) (Enc.param (Enc.nonNullable Enc.uuid)) <>
      contramap (\(_,b,_) -> b) (Enc.param (Enc.nonNullable Enc.uuid)) <>
      contramap (\(_,_,c) -> c) (Enc.param (Enc.nonNullable Enc.text))

createCleanupPolicyStatement :: Statement.Statement UUID UUID
createCleanupPolicyStatement = Statement.Statement sql encoder decoder True
  where
    sql = "INSERT INTO cleanup_policies (workspace_id, memory_type) VALUES ($1, 'short_term') RETURNING id"
    encoder = Enc.param (Enc.nonNullable Enc.uuid)
    decoder = Dec.singleRow (Dec.column (Dec.nonNullable Dec.uuid))

textUuid :: UUID -> Text
textUuid = T.pack . show
