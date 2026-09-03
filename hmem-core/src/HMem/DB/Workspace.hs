module HMem.DB.Workspace
  ( listActiveWorkspaces
  , listVisibleWorkspaces
  , renameWorkspace
  ) where

import Control.Exception (throwIO)
import Data.Functor.Contravariant (contramap)
import Data.Int (Int32)
import Data.Pool (Pool)
import Data.Text (Text)
import Data.Text qualified as T
import Data.UUID (UUID)
import Hasql.Connection qualified as Hasql
import Hasql.Decoders qualified as Dec
import Hasql.Encoders qualified as Enc
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement

import HMem.DB.Pool (DBException(..), runSession)
import HMem.Types (UpdateWorkspace(..), Workspace(..), validateUpdateWorkspaceInput, workspaceTypeFromText)

listActiveWorkspaces :: Pool Hasql.Connection -> Int -> Int -> IO [Workspace]
listActiveWorkspaces pool limitRows offsetRows =
  runSession pool $ Session.statement (toInt32 limitRows, toInt32 offsetRows) listActiveWorkspacesStatement

listVisibleWorkspaces :: Pool Hasql.Connection -> UUID -> Int -> Int -> IO [Workspace]
listVisibleWorkspaces pool userId limitRows offsetRows =
  runSession pool $ Session.statement (userId, toInt32 limitRows, toInt32 offsetRows) listVisibleWorkspacesStatement

-- | Rename an active workspace and return its canonical row.  Validation lives
-- here as well as at the HTTP boundary so alternate callers cannot bypass the
-- name-only invariant.
renameWorkspace :: Pool Hasql.Connection -> UUID -> UpdateWorkspace -> IO (Maybe Workspace)
renameWorkspace pool workspaceId input = do
  case validateUpdateWorkspaceInput input of
    [] -> pure ()
    errors -> throwIO $ DBCheckViolation (T.intercalate "; " errors)
  runSession pool $ Session.statement (workspaceId, input.name) renameWorkspaceStatement

toInt32 :: Int -> Int32
toInt32 = fromIntegral

workspaceRowDecoder :: Dec.Row Workspace
workspaceRowDecoder = do
  wsId <- Dec.column (Dec.nonNullable Dec.uuid)
  wsName <- Dec.column (Dec.nonNullable Dec.text)
  wsTypeText <- Dec.column (Dec.nonNullable Dec.text)
  wsGhOwner <- Dec.column (Dec.nullable Dec.text)
  wsGhRepo <- Dec.column (Dec.nullable Dec.text)
  wsCreatedAt <- Dec.column (Dec.nonNullable Dec.timestamptz)
  wsUpdatedAt <- Dec.column (Dec.nonNullable Dec.timestamptz)
  wsType <- case workspaceTypeFromText wsTypeText of
    Just parsed -> pure parsed
    Nothing -> fail $ "Unexpected workspace_type_enum value: " <> show wsTypeText
  pure Workspace
    { id = wsId
    , name = wsName
    , workspaceType = wsType
    , ghOwner = wsGhOwner
    , ghRepo = wsGhRepo
    , createdAt = wsCreatedAt
    , updatedAt = wsUpdatedAt
    }

listActiveWorkspacesStatement :: Statement.Statement (Int32, Int32) [Workspace]
listActiveWorkspacesStatement = Statement.Statement sql encoder decoder True
  where
    sql = "SELECT id, name, workspace_type::text, gh_owner, gh_repo, created_at, updated_at \
          \FROM workspaces \
          \WHERE deleted_at IS NULL \
          \ORDER BY name ASC, id ASC \
          \LIMIT $1 OFFSET $2"
    encoder =
      contramap fst (Enc.param (Enc.nonNullable Enc.int4)) <>
      contramap snd (Enc.param (Enc.nonNullable Enc.int4))
    decoder = Dec.rowList workspaceRowDecoder

listVisibleWorkspacesStatement :: Statement.Statement (UUID, Int32, Int32) [Workspace]
listVisibleWorkspacesStatement = Statement.Statement sql encoder decoder True
  where
    sql = "SELECT w.id, w.name, w.workspace_type::text, w.gh_owner, w.gh_repo, w.created_at, w.updated_at \
          \FROM workspaces w \
          \JOIN workspace_memberships wm ON wm.workspace_id = w.id \
          \WHERE wm.user_id = $1 \
          \  AND w.deleted_at IS NULL \
          \ORDER BY w.name ASC, w.id ASC \
          \LIMIT $2 OFFSET $3"
    encoder =
      contramap (\(a,_,_) -> a) (Enc.param (Enc.nonNullable Enc.uuid)) <>
      contramap (\(_,b,_) -> b) (Enc.param (Enc.nonNullable Enc.int4)) <>
      contramap (\(_,_,c) -> c) (Enc.param (Enc.nonNullable Enc.int4))
    decoder = Dec.rowList workspaceRowDecoder

renameWorkspaceStatement :: Statement.Statement (UUID, Text) (Maybe Workspace)
renameWorkspaceStatement = Statement.Statement sql encoder decoder True
  where
    sql = "UPDATE workspaces SET name = $2 WHERE id = $1 AND deleted_at IS NULL RETURNING id, name, workspace_type::text, gh_owner, gh_repo, created_at, updated_at"
    encoder =
      contramap fst (Enc.param (Enc.nonNullable Enc.uuid)) <>
      contramap snd (Enc.param (Enc.nonNullable Enc.text))
    decoder = Dec.rowMaybe workspaceRowDecoder
