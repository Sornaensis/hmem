module HMem.DB.Workspace
  ( listActiveWorkspaces
  , listVisibleWorkspaces
  ) where

import Data.Functor.Contravariant (contramap)
import Data.Int (Int32)
import Data.Pool (Pool)
import Data.UUID (UUID)
import Hasql.Connection qualified as Hasql
import Hasql.Decoders qualified as Dec
import Hasql.Encoders qualified as Enc
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement

import HMem.DB.Pool (runSession)
import HMem.Types (Workspace(..), workspaceTypeFromText)

listActiveWorkspaces :: Pool Hasql.Connection -> Int -> Int -> IO [Workspace]
listActiveWorkspaces pool limitRows offsetRows =
  runSession pool $ Session.statement (toInt32 limitRows, toInt32 offsetRows) listActiveWorkspacesStatement

listVisibleWorkspaces :: Pool Hasql.Connection -> UUID -> Int -> Int -> IO [Workspace]
listVisibleWorkspaces pool userId limitRows offsetRows =
  runSession pool $ Session.statement (userId, toInt32 limitRows, toInt32 offsetRows) listVisibleWorkspacesStatement

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
