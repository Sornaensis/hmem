module HMem.DB.Auth
  ( -- * Roles and permissions
    WorkspaceRole(..)
  , roleToText
  , roleFromText
  , roleSatisfies
  , GlobalPermission(..)
  , UserGrants(..)

    -- * Authorization decisions
  , AuthorizationError(..)
  , hasGlobalPermission
  , hasWorkspaceRole
  , authorizeGlobal
  , authorizeWorkspace
  , authorizeScope

    -- * Grant lookup
  , getUserGrants
  , getWorkspaceRole

    -- * Entity-to-workspace resolution
  , EntityKind(..)
  , EntityScope(..)
  , resolveEntityScope
  , resolveEntityScopeRequired
  ) where

import Data.ByteString (ByteString)
import Data.Functor.Contravariant (contramap)
import Data.Pool (Pool)
import Data.Text (Text)
import Data.UUID (UUID)
import Hasql.Connection qualified as Hasql
import Hasql.Decoders qualified as Dec
import Hasql.Encoders qualified as Enc
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement

import HMem.DB.Pool (runSession)
import HMem.DB.RequestContext (Principal(..), PrincipalAuthority(..))

------------------------------------------------------------------------
-- Roles and permissions
------------------------------------------------------------------------

data WorkspaceRole
  = WorkspaceRoleRead
  | WorkspaceRoleEdit
  | WorkspaceRoleAdmin
  deriving stock (Show, Eq, Ord, Enum, Bounded)

roleToText :: WorkspaceRole -> Text
roleToText WorkspaceRoleRead  = "read"
roleToText WorkspaceRoleEdit  = "edit"
roleToText WorkspaceRoleAdmin = "admin"

roleFromText :: Text -> Maybe WorkspaceRole
roleFromText "read"  = Just WorkspaceRoleRead
roleFromText "edit"  = Just WorkspaceRoleEdit
roleFromText "admin" = Just WorkspaceRoleAdmin
roleFromText _       = Nothing

roleSatisfies :: WorkspaceRole -> WorkspaceRole -> Bool
roleSatisfies actual required = fromEnum actual >= fromEnum required

data GlobalPermission
  = GlobalCreateWorkspace
  | GlobalSuperadmin
  deriving stock (Show, Eq)

data UserGrants = UserGrants
  { userCanCreateWorkspace :: !Bool
  , userIsSuperadmin       :: !Bool
  } deriving stock (Show, Eq)

------------------------------------------------------------------------
-- Authorization decisions
------------------------------------------------------------------------

data AuthorizationError
  = MissingPrincipal
  | MissingGlobalPermission !GlobalPermission
  | MissingWorkspaceRole !UUID !WorkspaceRole
  | GlobalScopeRequiresSuperadmin
  | EntityScopeNotFound !EntityKind !UUID
  deriving stock (Show, Eq)

hasGlobalPermission :: Pool Hasql.Connection -> Maybe Principal -> GlobalPermission -> IO Bool
hasGlobalPermission _pool Nothing _permission = pure False
hasGlobalPermission pool (Just principal) permission = principalHasGlobalPermission pool principal permission

hasWorkspaceRole :: Pool Hasql.Connection -> Maybe Principal -> UUID -> WorkspaceRole -> IO Bool
hasWorkspaceRole _pool Nothing _workspaceId _required = pure False
hasWorkspaceRole pool (Just principal) workspaceId required = do
  isAdmin <- principalHasGlobalPermission pool principal GlobalSuperadmin
  if isAdmin
    then pure True
    else case principal.authority of
      PrincipalGrantUser userId -> do
        mRole <- getWorkspaceRole pool workspaceId userId
        pure $ maybe False (`roleSatisfies` required) mRole
      _ -> pure False

authorizeGlobal :: Pool Hasql.Connection -> Maybe Principal -> GlobalPermission -> IO (Either AuthorizationError ())
authorizeGlobal pool mPrincipal permission = do
  allowed <- hasGlobalPermission pool mPrincipal permission
  pure $ if allowed
    then Right ()
    else Left $ case mPrincipal of
      Nothing -> MissingPrincipal
      Just _  -> MissingGlobalPermission permission

authorizeWorkspace :: Pool Hasql.Connection -> Maybe Principal -> UUID -> WorkspaceRole -> IO (Either AuthorizationError ())
authorizeWorkspace pool mPrincipal workspaceId required = do
  allowed <- hasWorkspaceRole pool mPrincipal workspaceId required
  pure $ if allowed
    then Right ()
    else Left $ case mPrincipal of
      Nothing -> MissingPrincipal
      Just _  -> MissingWorkspaceRole workspaceId required

authorizeScope :: Pool Hasql.Connection -> Maybe Principal -> EntityScope -> WorkspaceRole -> IO (Either AuthorizationError ())
authorizeScope pool mPrincipal scope required = case scope of
  EntityWorkspaceScope workspaceId -> authorizeWorkspace pool mPrincipal workspaceId required
  EntityGlobalScope -> do
    allowed <- hasGlobalPermission pool mPrincipal GlobalSuperadmin
    pure $ if allowed
      then Right ()
      else Left $ case mPrincipal of
        Nothing -> MissingPrincipal
        Just _  -> GlobalScopeRequiresSuperadmin

principalHasGlobalPermission :: Pool Hasql.Connection -> Principal -> GlobalPermission -> IO Bool
principalHasGlobalPermission pool principal permission = case principal.authority of
  PrincipalSyntheticLocalSuperadmin -> pure True
  PrincipalNoAuthority -> pure False
  PrincipalGrantUser userId -> do
    mGrants <- getUserGrants pool userId
    pure $ maybe False grantsAllowUser mGrants
  where
    grantsAllowUser UserGrants{..} = grantsAllow permission userCanCreateWorkspace userIsSuperadmin

grantsAllow :: GlobalPermission -> Bool -> Bool -> Bool
grantsAllow GlobalCreateWorkspace canCreate isSuper = isSuper || canCreate
grantsAllow GlobalSuperadmin _canCreate isSuper = isSuper

------------------------------------------------------------------------
-- Grant lookup
------------------------------------------------------------------------

getUserGrants :: Pool Hasql.Connection -> UUID -> IO (Maybe UserGrants)
getUserGrants pool userId = runSession pool $ Session.statement userId userGrantsStatement

getWorkspaceRole :: Pool Hasql.Connection -> UUID -> UUID -> IO (Maybe WorkspaceRole)
getWorkspaceRole pool workspaceId userId = do
  mRoleText <- runSession pool $ Session.statement (workspaceId, userId) workspaceRoleStatement
  pure (mRoleText >>= roleFromText)

userGrantsStatement :: Statement.Statement UUID (Maybe UserGrants)
userGrantsStatement = Statement.Statement sql encoder decoder True
  where
    sql = "SELECT can_create_workspace, is_superadmin FROM users WHERE id = $1"
    encoder = Enc.param (Enc.nonNullable Enc.uuid)
    decoder = Dec.rowMaybe $ UserGrants
      <$> Dec.column (Dec.nonNullable Dec.bool)
      <*> Dec.column (Dec.nonNullable Dec.bool)

workspaceRoleStatement :: Statement.Statement (UUID, UUID) (Maybe Text)
workspaceRoleStatement = Statement.Statement sql encoder decoder True
  where
    sql = "SELECT role::text FROM workspace_memberships WHERE workspace_id = $1 AND user_id = $2"
    encoder =
      contramap fst (Enc.param (Enc.nonNullable Enc.uuid)) <>
      contramap snd (Enc.param (Enc.nonNullable Enc.uuid))
    decoder = Dec.rowMaybe (Dec.column (Dec.nonNullable Dec.text))

------------------------------------------------------------------------
-- Entity-to-workspace resolution
------------------------------------------------------------------------

data EntityKind
  = EntityWorkspace
  | EntityMemory
  | EntityProject
  | EntityTask
  | EntityCategory
  | EntitySavedView
  | EntityCleanupPolicy
  deriving stock (Show, Eq)

-- | Scope returned for an entity lookup. Most hmem resources are workspace
-- scoped. Categories may be global (@memory_categories.workspace_id IS NULL@),
-- and the default policy treats global entity scopes as superadmin-only because
-- the public v1 permission model has no separate global read/edit role.
--
-- Relationship operations (memory links, project/task memory links, task
-- dependencies, category links) should authorize through their participating
-- owner entities before mutation. For example, task dependency writes should
-- resolve the owning task, and memory-link writes should resolve the source
-- memory and validate any peer-resource policy required by the endpoint.

data EntityScope
  = EntityWorkspaceScope !UUID
  | EntityGlobalScope
  deriving stock (Show, Eq)

resolveEntityScope :: Pool Hasql.Connection -> EntityKind -> UUID -> IO (Maybe EntityScope)
resolveEntityScope pool kind entityId = runSession pool $ Session.statement entityId (entityScopeStatement kind)

resolveEntityScopeRequired :: Pool Hasql.Connection -> EntityKind -> UUID -> IO (Either AuthorizationError EntityScope)
resolveEntityScopeRequired pool kind entityId = do
  mScope <- resolveEntityScope pool kind entityId
  pure $ maybe (Left $ EntityScopeNotFound kind entityId) Right mScope

entityScopeStatement :: EntityKind -> Statement.Statement UUID (Maybe EntityScope)
entityScopeStatement EntityWorkspace     = nonNullScopeStatement "SELECT id FROM workspaces WHERE id = $1"
entityScopeStatement EntityMemory        = nonNullScopeStatement "SELECT workspace_id FROM memories WHERE id = $1"
entityScopeStatement EntityProject       = nonNullScopeStatement "SELECT workspace_id FROM projects WHERE id = $1"
entityScopeStatement EntityTask          = nonNullScopeStatement "SELECT workspace_id FROM tasks WHERE id = $1"
entityScopeStatement EntitySavedView     = nonNullScopeStatement "SELECT workspace_id FROM saved_views WHERE id = $1"
entityScopeStatement EntityCleanupPolicy = nonNullScopeStatement "SELECT workspace_id FROM cleanup_policies WHERE id = $1"
entityScopeStatement EntityCategory      = nullableScopeStatement "SELECT workspace_id FROM memory_categories WHERE id = $1"

nonNullScopeStatement :: ByteString -> Statement.Statement UUID (Maybe EntityScope)
nonNullScopeStatement sql = Statement.Statement sql encoder decoder True
  where
    encoder = Enc.param (Enc.nonNullable Enc.uuid)
    decoder = Dec.rowMaybe (EntityWorkspaceScope <$> Dec.column (Dec.nonNullable Dec.uuid))

nullableScopeStatement :: ByteString -> Statement.Statement UUID (Maybe EntityScope)
nullableScopeStatement sql = Statement.Statement sql encoder decoder True
  where
    encoder = Enc.param (Enc.nonNullable Enc.uuid)
    decoder = Dec.rowMaybe $ do
      mWorkspaceId <- Dec.column (Dec.nullable Dec.uuid)
      pure $ maybe EntityGlobalScope EntityWorkspaceScope mWorkspaceId
