module HMem.DB.Auth
  ( -- * Roles and permissions
    WorkspaceRole(..)
  , roleToText
  , roleFromText
  , roleSatisfies
  , GlobalPermission(..)
  , UserGrants(..)
  , ResolvedAccessToken(..)
  , WorkspaceMembership(..)
  , UpsertWorkspaceMembership(..)

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
  , accessTokenHash
  , accessTokenHmacHash
  , accessTokenHashCandidates
  , resolveAccessTokenPrincipal
  , resolveAccessTokenPrincipalWithSecret
  , touchAccessTokenLastUsed
  , resolveUserPrincipalByAuthSubject
  , listWorkspaceMemberships
  , upsertWorkspaceMembership
  , deleteWorkspaceMembership
  , grantWorkspaceAdminToCreator
  , grantWorkspaceAdminToCreatorSession

    -- * Entity-to-workspace resolution
  , EntityKind(..)
  , EntityScope(..)
  , resolveEntityScope
  , resolveEntityScopeRequired
  ) where

import Data.Aeson (FromJSON(..), ToJSON(..), Value(String), object, withObject, withText, (.:), (.=))
import Data.ByteString (ByteString)
import Data.Functor.Contravariant (contramap)
import Data.Int (Int32, Int64)
import Data.Pool (Pool)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time (UTCTime)
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Control.Monad (void)
import Crypto.Hash (Digest, SHA256, hash)
import Crypto.MAC.HMAC (HMAC, hmac, hmacGetDigest)
import Hasql.Connection qualified as Hasql
import Hasql.Decoders qualified as Dec
import Hasql.Encoders qualified as Enc
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement
import GHC.Generics (Generic)

import HMem.DB.Pool (runSession)
import HMem.DB.RequestContext (ActorType(..), Principal(..), PrincipalAuthority(..))

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

instance ToJSON WorkspaceRole where
  toJSON = String . roleToText

instance FromJSON WorkspaceRole where
  parseJSON = withText "WorkspaceRole" $ \t ->
    maybe (fail "Invalid workspace role (expected read, edit, or admin)") pure (roleFromText t)

data GlobalPermission
  = GlobalCreateWorkspace
  | GlobalSuperadmin
  deriving stock (Show, Eq)

data UserGrants = UserGrants
  { userCanCreateWorkspace :: !Bool
  , userIsSuperadmin       :: !Bool
  } deriving stock (Show, Eq)

data ResolvedAccessToken = ResolvedAccessToken
  { tokenId   :: !UUID
  , principal :: !Principal
  } deriving stock (Show, Eq)

data WorkspaceMembership = WorkspaceMembership
  { membershipWorkspaceId :: !UUID
  , membershipUserId      :: !UUID
  , membershipRole        :: !WorkspaceRole
  , membershipGrantedBy   :: !(Maybe UUID)
  , membershipCreatedAt   :: !UTCTime
  , membershipUpdatedAt   :: !UTCTime
  } deriving stock (Show, Eq, Generic)

instance ToJSON WorkspaceMembership where
  toJSON membership = object
    [ "workspace_id" .= membership.membershipWorkspaceId
    , "user_id" .= membership.membershipUserId
    , "role" .= membership.membershipRole
    , "granted_by" .= membership.membershipGrantedBy
    , "created_at" .= membership.membershipCreatedAt
    , "updated_at" .= membership.membershipUpdatedAt
    ]

instance FromJSON WorkspaceMembership where
  parseJSON = withObject "WorkspaceMembership" $ \o ->
    WorkspaceMembership
      <$> o .: "workspace_id"
      <*> o .: "user_id"
      <*> o .: "role"
      <*> o .: "granted_by"
      <*> o .: "created_at"
      <*> o .: "updated_at"

data UpsertWorkspaceMembership = UpsertWorkspaceMembership
  { membershipUserIdInput :: !UUID
  , membershipRoleInput   :: !WorkspaceRole
  } deriving stock (Show, Eq, Generic)

instance FromJSON UpsertWorkspaceMembership where
  parseJSON = withObject "UpsertWorkspaceMembership" $ \o ->
    UpsertWorkspaceMembership
      <$> o .: "user_id"
      <*> o .: "role"

instance ToJSON UpsertWorkspaceMembership where
  toJSON req = object
    [ "user_id" .= req.membershipUserIdInput
    , "role" .= req.membershipRoleInput
    ]

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

-- | Canonical v1 access-token digest format for persisted PAT/bot tokens.
-- Operators should store this value in @access_tokens.token_hash@ rather than
-- raw bearer material.
accessTokenHash :: Text -> Text
accessTokenHash token = "sha256:" <> T.pack (show digest)
  where
    digest = hash (TE.encodeUtf8 token) :: Digest SHA256

-- | Versioned HMAC-SHA256 token digest for installations with a server-side
-- token hash secret/pepper.  The raw token is already high-entropy; the HMAC
-- protects against offline lookup if the database is exposed without the
-- server configuration secret.
accessTokenHmacHash :: Text -> Text -> Text
accessTokenHmacHash secret token = "hmac-sha256-v1:" <> T.pack (show digest)
  where
    digest = hmacGetDigest (hmac (TE.encodeUtf8 secret) (TE.encodeUtf8 token) :: HMAC SHA256)

accessTokenHashCandidates :: Maybe Text -> Text -> [Text]
accessTokenHashCandidates mSecret token = case T.strip <$> mSecret of
  Just secret | not (T.null secret) -> [accessTokenHmacHash secret token, accessTokenHash token]
  _ -> [accessTokenHash token]

resolveAccessTokenPrincipal :: Pool Hasql.Connection -> Text -> IO (Maybe ResolvedAccessToken)
resolveAccessTokenPrincipal pool token =
  resolveAccessTokenPrincipalWithSecret pool Nothing token

resolveAccessTokenPrincipalWithSecret :: Pool Hasql.Connection -> Maybe Text -> Text -> IO (Maybe ResolvedAccessToken)
resolveAccessTokenPrincipalWithSecret pool mSecret token =
  firstJustM (runSession pool . (`Session.statement` accessTokenPrincipalStatement))
    (accessTokenHashCandidates mSecret token)

firstJustM :: (a -> IO (Maybe b)) -> [a] -> IO (Maybe b)
firstJustM _ [] = pure Nothing
firstJustM f (x:xs) = do
  result <- f x
  case result of
    Just _ -> pure result
    Nothing -> firstJustM f xs

touchAccessTokenLastUsed :: Pool Hasql.Connection -> UUID -> IO ()
touchAccessTokenLastUsed pool tokenId =
  runSession pool $ Session.statement tokenId touchAccessTokenLastUsedStatement

resolveUserPrincipalByAuthSubject :: Pool Hasql.Connection -> Text -> IO (Maybe Principal)
resolveUserPrincipalByAuthSubject pool authSubject =
  runSession pool $ Session.statement authSubject userPrincipalByAuthSubjectStatement

listWorkspaceMemberships :: Pool Hasql.Connection -> UUID -> Maybe Int -> Maybe Int -> IO [WorkspaceMembership]
listWorkspaceMemberships pool workspaceId mlimit moffset = do
  let lim = fromIntegral (maybe 50 (max 1 . min 201) mlimit) :: Int32
      off = fromIntegral (maybe 0 (max 0) moffset) :: Int32
  runSession pool $ Session.statement (workspaceId, lim, off) listWorkspaceMembershipsStatement

upsertWorkspaceMembership
  :: Pool Hasql.Connection
  -> UUID
  -> UpsertWorkspaceMembership
  -> Maybe UUID
  -> IO WorkspaceMembership
upsertWorkspaceMembership pool workspaceId req grantedBy =
  runSession pool $ Session.statement
    (workspaceId, req.membershipUserIdInput, roleToText req.membershipRoleInput, grantedBy)
    upsertWorkspaceMembershipStatement

deleteWorkspaceMembership :: Pool Hasql.Connection -> UUID -> UUID -> IO Bool
deleteWorkspaceMembership pool workspaceId userId = do
  n <- runSession pool $ Session.statement (workspaceId, userId) deleteWorkspaceMembershipStatement
  pure (n > 0)

grantWorkspaceAdminToCreator :: Pool Hasql.Connection -> UUID -> Principal -> IO ()
grantWorkspaceAdminToCreator pool workspaceId principal =
  runSession pool $ grantWorkspaceAdminToCreatorSession workspaceId principal

grantWorkspaceAdminToCreatorSession :: UUID -> Principal -> Session.Session ()
grantWorkspaceAdminToCreatorSession workspaceId principal = case principal.authority of
  PrincipalGrantUser userId -> do
    mGrants <- Session.statement userId userGrantsStatement
    case mGrants of
      Just UserGrants { userIsSuperadmin = True } -> pure ()
      Nothing -> pure ()
      _ -> void $ Session.statement
        ( workspaceId
        , userId
        , roleToText WorkspaceRoleAdmin
        , Just userId
        )
        upsertWorkspaceMembershipStatement
  _ -> pure ()

userGrantsStatement :: Statement.Statement UUID (Maybe UserGrants)
userGrantsStatement = Statement.Statement sql encoder decoder True
  where
    sql = "SELECT can_create_workspace, is_superadmin FROM users WHERE id = $1 AND disabled_at IS NULL"
    encoder = Enc.param (Enc.nonNullable Enc.uuid)
    decoder = Dec.rowMaybe $ UserGrants
      <$> Dec.column (Dec.nonNullable Dec.bool)
      <*> Dec.column (Dec.nonNullable Dec.bool)

workspaceRoleStatement :: Statement.Statement (UUID, UUID) (Maybe Text)
workspaceRoleStatement = Statement.Statement sql encoder decoder True
  where
    sql = "SELECT wm.role::text \
          \FROM workspace_memberships wm \
          \JOIN users u ON u.id = wm.user_id \
          \WHERE wm.workspace_id = $1 AND wm.user_id = $2 AND u.disabled_at IS NULL"
    encoder =
      contramap fst (Enc.param (Enc.nonNullable Enc.uuid)) <>
      contramap snd (Enc.param (Enc.nonNullable Enc.uuid))
    decoder = Dec.rowMaybe (Dec.column (Dec.nonNullable Dec.text))

accessTokenPrincipalStatement :: Statement.Statement Text (Maybe ResolvedAccessToken)
accessTokenPrincipalStatement = Statement.Statement sql encoder decoder True
  where
    sql = "SELECT at.id, at.grant_user_id, at.actor_type::text, at.actor_label \
          \FROM access_tokens at \
          \JOIN users u ON u.id = at.grant_user_id \
          \WHERE at.token_hash = $1 \
          \  AND at.revoked_at IS NULL \
          \  AND (at.expires_at IS NULL OR at.expires_at > now()) \
          \  AND u.disabled_at IS NULL"
    encoder = Enc.param (Enc.nonNullable Enc.text)
    decoder = Dec.rowMaybe $ do
      tokenId <- Dec.column (Dec.nonNullable Dec.uuid)
      grantUserId <- Dec.column (Dec.nonNullable Dec.uuid)
      actorTypeText <- Dec.column (Dec.nonNullable Dec.text)
      label <- Dec.column (Dec.nonNullable Dec.text)
      actorType <- case actorTypeFromText actorTypeText of
        Just parsed -> pure parsed
        Nothing -> fail $ "Unexpected actor_type_enum value: " <> show actorTypeText
      let actorId = case actorType of
            ActorUser -> UUID.toText grantUserId
            ActorBot  -> UUID.toText tokenId
      pure ResolvedAccessToken
        { tokenId = tokenId
        , principal = Principal
            { actorType = actorType
            , actorId = actorId
            , actorLabel = label
            , authority = PrincipalGrantUser grantUserId
            }
        }

touchAccessTokenLastUsedStatement :: Statement.Statement UUID ()
touchAccessTokenLastUsedStatement = Statement.Statement sql encoder decoder True
  where
    sql = "UPDATE access_tokens SET last_used_at = now() WHERE id = $1"
    encoder = Enc.param (Enc.nonNullable Enc.uuid)
    decoder = Dec.noResult

userPrincipalByAuthSubjectStatement :: Statement.Statement Text (Maybe Principal)
userPrincipalByAuthSubjectStatement = Statement.Statement sql encoder decoder True
  where
    sql = "SELECT id, COALESCE(display_name, email, auth_subject, id::text) \
          \FROM users WHERE auth_subject = $1 AND disabled_at IS NULL"
    encoder = Enc.param (Enc.nonNullable Enc.text)
    decoder = Dec.rowMaybe $ do
      userId <- Dec.column (Dec.nonNullable Dec.uuid)
      label <- Dec.column (Dec.nonNullable Dec.text)
      pure Principal
        { actorType = ActorUser
        , actorId = UUID.toText userId
        , actorLabel = label
        , authority = PrincipalGrantUser userId
        }

actorTypeFromText :: Text -> Maybe ActorType
actorTypeFromText "user" = Just ActorUser
actorTypeFromText "bot"  = Just ActorBot
actorTypeFromText _      = Nothing

workspaceMembershipRowDecoder :: Dec.Row WorkspaceMembership
workspaceMembershipRowDecoder = do
  workspaceId <- Dec.column (Dec.nonNullable Dec.uuid)
  userId <- Dec.column (Dec.nonNullable Dec.uuid)
  roleText <- Dec.column (Dec.nonNullable Dec.text)
  grantedBy <- Dec.column (Dec.nullable Dec.uuid)
  createdAt <- Dec.column (Dec.nonNullable Dec.timestamptz)
  updatedAt <- Dec.column (Dec.nonNullable Dec.timestamptz)
  role <- case roleFromText roleText of
    Just parsed -> pure parsed
    Nothing -> fail $ "Unexpected workspace_role_enum value: " <> show roleText
  pure WorkspaceMembership
    { membershipWorkspaceId = workspaceId
    , membershipUserId = userId
    , membershipRole = role
    , membershipGrantedBy = grantedBy
    , membershipCreatedAt = createdAt
    , membershipUpdatedAt = updatedAt
    }

listWorkspaceMembershipsStatement :: Statement.Statement (UUID, Int32, Int32) [WorkspaceMembership]
listWorkspaceMembershipsStatement = Statement.Statement sql encoder decoder True
  where
    sql = "SELECT workspace_id, user_id, role::text, granted_by, created_at, updated_at \
          \FROM workspace_memberships WHERE workspace_id = $1 \
          \ORDER BY created_at ASC, user_id ASC LIMIT $2 OFFSET $3"
    encoder =
      contramap (\(a,_,_) -> a) (Enc.param (Enc.nonNullable Enc.uuid)) <>
      contramap (\(_,b,_) -> b) (Enc.param (Enc.nonNullable Enc.int4)) <>
      contramap (\(_,_,c) -> c) (Enc.param (Enc.nonNullable Enc.int4))
    decoder = Dec.rowList workspaceMembershipRowDecoder

upsertWorkspaceMembershipStatement :: Statement.Statement (UUID, UUID, Text, Maybe UUID) WorkspaceMembership
upsertWorkspaceMembershipStatement = Statement.Statement sql encoder decoder True
  where
    sql = "INSERT INTO workspace_memberships (workspace_id, user_id, role, granted_by) \
          \VALUES ($1, $2, $3::workspace_role_enum, $4) \
          \ON CONFLICT (workspace_id, user_id) DO UPDATE \
          \SET role = EXCLUDED.role, granted_by = EXCLUDED.granted_by \
          \RETURNING workspace_id, user_id, role::text, granted_by, created_at, updated_at"
    encoder =
      contramap (\(a,_,_,_) -> a) (Enc.param (Enc.nonNullable Enc.uuid)) <>
      contramap (\(_,b,_,_) -> b) (Enc.param (Enc.nonNullable Enc.uuid)) <>
      contramap (\(_,_,c,_) -> c) (Enc.param (Enc.nonNullable Enc.text)) <>
      contramap (\(_,_,_,d) -> d) (Enc.param (Enc.nullable Enc.uuid))
    decoder = Dec.singleRow workspaceMembershipRowDecoder

deleteWorkspaceMembershipStatement :: Statement.Statement (UUID, UUID) Int64
deleteWorkspaceMembershipStatement = Statement.Statement sql encoder decoder True
  where
    sql = "DELETE FROM workspace_memberships WHERE workspace_id = $1 AND user_id = $2"
    encoder =
      contramap fst (Enc.param (Enc.nonNullable Enc.uuid)) <>
      contramap snd (Enc.param (Enc.nonNullable Enc.uuid))
    decoder = Dec.rowsAffected

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
