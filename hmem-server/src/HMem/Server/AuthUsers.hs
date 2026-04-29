module HMem.Server.AuthUsers
  ( UpsertUserInput(..)
  , UpsertUserDecision(..)
  , UpsertUserResult(..)
  , UserAdminError(..)
  , upsertUser
  ) where

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

import HMem.DB.Pool (runTransaction)

data UpsertUserInput = UpsertUserInput
  { authSubject        :: !Text
  , email              :: !(Maybe Text)
  , displayName        :: !(Maybe Text)
  , canCreateWorkspace :: !(Maybe Bool)
  , isSuperadmin       :: !(Maybe Bool)
  , active             :: !(Maybe Bool)
  } deriving stock (Show, Eq)

data UpsertUserDecision
  = UserCreated
  | UserUpdated
  deriving stock (Show, Eq)

data UpsertUserResult = UpsertUserResult
  { userId             :: !UUID
  , decision           :: !UpsertUserDecision
  , canCreateWorkspace :: !Bool
  , isSuperadmin       :: !Bool
  , active             :: !Bool
  } deriving stock (Show, Eq)

data UserAdminError
  = EmptyAuthSubject
  deriving stock (Show, Eq)

data UserRow = UserRow
  { rowUserId             :: !UUID
  , rowCanCreateWorkspace :: !Bool
  , rowIsSuperadmin       :: !Bool
  , rowActive             :: !Bool
  } deriving stock (Show, Eq)

upsertUser :: Pool Hasql.Connection -> UpsertUserInput -> IO (Either UserAdminError UpsertUserResult)
upsertUser pool input = do
  let subject = T.strip input.authSubject
      normalized = input
        { authSubject = subject
        , email = T.strip <$> input.email
        , displayName = T.strip <$> input.displayName
        }
  if T.null subject
    then pure (Left EmptyAuthSubject)
    else runTransaction pool (upsertUserSession normalized)

upsertUserSession :: UpsertUserInput -> Session.Session (Either UserAdminError UpsertUserResult)
upsertUserSession input = do
  -- Serialize operator user creation so concurrent first-time upserts for the
  -- same auth_subject cannot race into the unique auth_subject index.
  Session.sql "LOCK TABLE users IN EXCLUSIVE MODE"
  mExisting <- Session.statement input.authSubject userRowBySubjectStatement
  case mExisting of
    Nothing -> do
      row <- Session.statement
        ( input.authSubject
        , input.email >>= nonEmptyText
        , input.displayName >>= nonEmptyText
        , maybe False id input.canCreateWorkspace
        , maybe False id input.isSuperadmin
        , maybe True id input.active
        )
        insertUserStatement
      pure (Right (rowToResult UserCreated row))
    Just existing -> do
      row <- Session.statement
        ( existing.rowUserId
        , input.email >>= nonEmptyText
        , input.displayName >>= nonEmptyText
        , input.canCreateWorkspace
        , input.isSuperadmin
        , input.active
        )
        updateUserStatement
      pure (Right (rowToResult UserUpdated row))

nonEmptyText :: Text -> Maybe Text
nonEmptyText value =
  let stripped = T.strip value
  in if T.null stripped then Nothing else Just stripped

rowToResult :: UpsertUserDecision -> UserRow -> UpsertUserResult
rowToResult decision row = UpsertUserResult
  { userId = row.rowUserId
  , decision = decision
  , canCreateWorkspace = row.rowCanCreateWorkspace
  , isSuperadmin = row.rowIsSuperadmin
  , active = row.rowActive
  }

userRowBySubjectStatement :: Statement.Statement Text (Maybe UserRow)
userRowBySubjectStatement = Statement.Statement sql encoder decoder True
  where
    sql = "SELECT id, can_create_workspace, is_superadmin, disabled_at IS NULL \
          \FROM users WHERE auth_subject = $1"
    encoder = Enc.param (Enc.nonNullable Enc.text)
    decoder = Dec.rowMaybe userRowDecoder

insertUserStatement :: Statement.Statement (Text, Maybe Text, Maybe Text, Bool, Bool, Bool) UserRow
insertUserStatement = Statement.Statement sql encoder decoder True
  where
    sql = "INSERT INTO users (auth_subject, email, display_name, can_create_workspace, is_superadmin, disabled_at) \
          \VALUES ($1, $2, $3, $4, $5, CASE WHEN $6 THEN NULL ELSE now() END) \
          \RETURNING id, can_create_workspace, is_superadmin, disabled_at IS NULL"
    encoder =
      contramap (\(a,_,_,_,_,_) -> a) (Enc.param (Enc.nonNullable Enc.text)) <>
      contramap (\(_,b,_,_,_,_) -> b) (Enc.param (Enc.nullable Enc.text)) <>
      contramap (\(_,_,c,_,_,_) -> c) (Enc.param (Enc.nullable Enc.text)) <>
      contramap (\(_,_,_,d,_,_) -> d) (Enc.param (Enc.nonNullable Enc.bool)) <>
      contramap (\(_,_,_,_,e,_) -> e) (Enc.param (Enc.nonNullable Enc.bool)) <>
      contramap (\(_,_,_,_,_,f) -> f) (Enc.param (Enc.nonNullable Enc.bool))
    decoder = Dec.singleRow userRowDecoder

updateUserStatement :: Statement.Statement (UUID, Maybe Text, Maybe Text, Maybe Bool, Maybe Bool, Maybe Bool) UserRow
updateUserStatement = Statement.Statement sql encoder decoder True
  where
    sql = "UPDATE users \
          \SET email = COALESCE($2, email), \
          \    display_name = COALESCE($3, display_name), \
          \    can_create_workspace = COALESCE($4, can_create_workspace), \
          \    is_superadmin = COALESCE($5, is_superadmin), \
          \    disabled_at = CASE $6 WHEN true THEN NULL WHEN false THEN COALESCE(disabled_at, now()) ELSE disabled_at END \
          \WHERE id = $1 \
          \RETURNING id, can_create_workspace, is_superadmin, disabled_at IS NULL"
    encoder =
      contramap (\(a,_,_,_,_,_) -> a) (Enc.param (Enc.nonNullable Enc.uuid)) <>
      contramap (\(_,b,_,_,_,_) -> b) (Enc.param (Enc.nullable Enc.text)) <>
      contramap (\(_,_,c,_,_,_) -> c) (Enc.param (Enc.nullable Enc.text)) <>
      contramap (\(_,_,_,d,_,_) -> d) (Enc.param (Enc.nullable Enc.bool)) <>
      contramap (\(_,_,_,_,e,_) -> e) (Enc.param (Enc.nullable Enc.bool)) <>
      contramap (\(_,_,_,_,_,f) -> f) (Enc.param (Enc.nullable Enc.bool))
    decoder = Dec.singleRow userRowDecoder

userRowDecoder :: Dec.Row UserRow
userRowDecoder = UserRow
  <$> Dec.column (Dec.nonNullable Dec.uuid)
  <*> Dec.column (Dec.nonNullable Dec.bool)
  <*> Dec.column (Dec.nonNullable Dec.bool)
  <*> Dec.column (Dec.nonNullable Dec.bool)
