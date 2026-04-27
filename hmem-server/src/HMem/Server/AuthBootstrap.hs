module HMem.Server.AuthBootstrap
  ( BootstrapSuperadminInput(..)
  , BootstrapSuperadminDecision(..)
  , BootstrapSuperadminResult(..)
  , ExistingSuperadmin(..)
  , BootstrapSuperadminError(..)
  , bootstrapSuperadmin
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

data BootstrapSuperadminInput = BootstrapSuperadminInput
  { authSubject :: !Text
  , email       :: !(Maybe Text)
  , displayName :: !(Maybe Text)
  , force       :: !Bool
  } deriving stock (Show, Eq)

data BootstrapSuperadminDecision
  = BootstrapCreated
  | BootstrapUpdated
  | BootstrapAlreadySatisfied
  deriving stock (Show, Eq)

data BootstrapSuperadminResult = BootstrapSuperadminResult
  { userId   :: !UUID
  , decision :: !BootstrapSuperadminDecision
  } deriving stock (Show, Eq)

data ExistingSuperadmin = ExistingSuperadmin
  { existingUserId :: !UUID
  , existingLabel  :: !Text
  } deriving stock (Show, Eq)

data BootstrapSuperadminError
  = EmptyAuthSubject
  | DifferentSuperadminExists !ExistingSuperadmin
  deriving stock (Show, Eq)

data BootstrapUserRow = BootstrapUserRow
  { rowUserId             :: !UUID
  , rowEmail              :: !(Maybe Text)
  , rowDisplayName        :: !(Maybe Text)
  , rowCanCreateWorkspace :: !Bool
  , rowIsSuperadmin       :: !Bool
  } deriving stock (Show, Eq)

bootstrapSuperadmin
  :: Pool Hasql.Connection
  -> BootstrapSuperadminInput
  -> IO (Either BootstrapSuperadminError BootstrapSuperadminResult)
bootstrapSuperadmin pool input = do
  let subject = T.strip input.authSubject
      normalizedInput = input { authSubject = subject }
  if T.null subject
    then pure (Left EmptyAuthSubject)
    else runTransaction pool (bootstrapSuperadminSession normalizedInput)

bootstrapSuperadminSession
  :: BootstrapSuperadminInput
  -> Session.Session (Either BootstrapSuperadminError BootstrapSuperadminResult)
bootstrapSuperadminSession input = do
  -- Serialize bootstrap decisions so two concurrent first-superadmin attempts
  -- cannot both observe an empty superadmin set and create different users.
  Session.sql "LOCK TABLE users IN EXCLUSIVE MODE"
  mExisting <- Session.statement input.authSubject userByAuthSubjectStatement
  case mExisting of
    Just row
      | bootstrapSatisfied input row ->
          pure (Right BootstrapSuperadminResult
            { userId = row.rowUserId
            , decision = BootstrapAlreadySatisfied
            })
    _ -> do
      mOther <- Session.statement input.authSubject findOtherSuperadminStatement
      case (mOther, input.force, mExisting) of
        (Just other, False, _) -> pure (Left (DifferentSuperadminExists other))
        (_, _, Nothing) -> do
          createdId <- Session.statement
            ( input.authSubject
            , input.email
            , input.displayName
            )
            insertBootstrapSuperadminStatement
          pure (Right BootstrapSuperadminResult
            { userId = createdId
            , decision = BootstrapCreated
            })
        (_, _, Just row) -> do
          updatedId <- Session.statement
            ( row.rowUserId
            , input.email
            , input.displayName
            )
            updateBootstrapSuperadminStatement
          pure (Right BootstrapSuperadminResult
            { userId = updatedId
            , decision = BootstrapUpdated
            })

bootstrapSatisfied :: BootstrapSuperadminInput -> BootstrapUserRow -> Bool
bootstrapSatisfied input row =
  row.rowCanCreateWorkspace &&
  row.rowIsSuperadmin &&
  maybe True (\expected -> row.rowEmail == Just expected) input.email &&
  maybe True (\expected -> row.rowDisplayName == Just expected) input.displayName

findOtherSuperadminStatement :: Statement.Statement Text (Maybe ExistingSuperadmin)
findOtherSuperadminStatement = Statement.Statement sql encoder decoder True
  where
    sql = "SELECT id, COALESCE(display_name, email, id::text) \
          \FROM users \
          \WHERE is_superadmin \
          \  AND auth_subject IS DISTINCT FROM $1 \
          \ORDER BY created_at ASC, id ASC \
          \LIMIT 1"
    encoder = Enc.param (Enc.nonNullable Enc.text)
    decoder = Dec.rowMaybe $ ExistingSuperadmin
      <$> Dec.column (Dec.nonNullable Dec.uuid)
      <*> Dec.column (Dec.nonNullable Dec.text)

userByAuthSubjectStatement :: Statement.Statement Text (Maybe BootstrapUserRow)
userByAuthSubjectStatement = Statement.Statement sql encoder decoder True
  where
    sql = "SELECT id, email, display_name, can_create_workspace, is_superadmin \
          \FROM users WHERE auth_subject = $1"
    encoder = Enc.param (Enc.nonNullable Enc.text)
    decoder = Dec.rowMaybe bootstrapUserRowDecoder

bootstrapUserRowDecoder :: Dec.Row BootstrapUserRow
bootstrapUserRowDecoder = BootstrapUserRow
  <$> Dec.column (Dec.nonNullable Dec.uuid)
  <*> Dec.column (Dec.nullable Dec.text)
  <*> Dec.column (Dec.nullable Dec.text)
  <*> Dec.column (Dec.nonNullable Dec.bool)
  <*> Dec.column (Dec.nonNullable Dec.bool)

insertBootstrapSuperadminStatement :: Statement.Statement (Text, Maybe Text, Maybe Text) UUID
insertBootstrapSuperadminStatement = Statement.Statement sql encoder decoder True
  where
    sql = "INSERT INTO users (auth_subject, email, display_name, can_create_workspace, is_superadmin) \
          \VALUES ($1, $2, $3, true, true) \
          \RETURNING id"
    encoder =
      contramap (\(a,_,_) -> a) (Enc.param (Enc.nonNullable Enc.text)) <>
      contramap (\(_,b,_) -> b) (Enc.param (Enc.nullable Enc.text)) <>
      contramap (\(_,_,c) -> c) (Enc.param (Enc.nullable Enc.text))
    decoder = Dec.singleRow (Dec.column (Dec.nonNullable Dec.uuid))

updateBootstrapSuperadminStatement :: Statement.Statement (UUID, Maybe Text, Maybe Text) UUID
updateBootstrapSuperadminStatement = Statement.Statement sql encoder decoder True
  where
    sql = "UPDATE users \
          \SET email = COALESCE($2, email), \
          \    display_name = COALESCE($3, display_name), \
          \    can_create_workspace = true, \
          \    is_superadmin = true \
          \WHERE id = $1 \
          \RETURNING id"
    encoder =
      contramap (\(a,_,_) -> a) (Enc.param (Enc.nonNullable Enc.uuid)) <>
      contramap (\(_,b,_) -> b) (Enc.param (Enc.nullable Enc.text)) <>
      contramap (\(_,_,c) -> c) (Enc.param (Enc.nullable Enc.text))
    decoder = Dec.singleRow (Dec.column (Dec.nonNullable Dec.uuid))
