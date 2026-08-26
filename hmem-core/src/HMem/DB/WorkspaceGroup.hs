module HMem.DB.WorkspaceGroup
  ( createGroup
  , getGroup
  , deleteGroup
  , listGroups
  , AddMemberResult(..)
  , addMember
  , removeMember
  , listGroupMembers
  ) where

import Control.Exception (throwIO)
import Data.Functor.Contravariant ((>$<), contramap)
import Data.Pool (Pool)
import Data.UUID (UUID)
import Hasql.Connection qualified as Hasql
import Hasql.Decoders qualified as Dec
import Hasql.Encoders qualified as Enc
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement
import Rel8

import HMem.DB.Pool (runSession, DBException(..))
import HMem.DB.Schema
import HMem.Types

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

rowToGroup :: WorkspaceGroupT Result -> WorkspaceGroup
rowToGroup r = WorkspaceGroup
  { id          = r.wgId
  , name        = r.wgName
  , description = r.wgDescription
  , createdAt   = r.wgCreatedAt
  , updatedAt   = r.wgUpdatedAt
  }

------------------------------------------------------------------------
-- Create
------------------------------------------------------------------------

createGroup :: Pool Hasql.Connection -> CreateWorkspaceGroup -> IO WorkspaceGroup
createGroup pool cg = do
  rows <- runSession pool $ Session.statement () $ run $
    insert Insert
      { into = workspaceGroupSchema
      , rows = values
          [ WorkspaceGroupT
              { wgId          = unsafeDefault
              , wgName        = lit cg.name
              , wgDescription = lit cg.description
              , wgCreatedAt   = unsafeDefault
              , wgUpdatedAt   = unsafeDefault
              }
          ]
      , onConflict = Abort
      , returning  = Returning id
      }
  case rows of
    (r:_) -> pure $ rowToGroup r
    []    -> throwIO $ DBOtherError "createGroup: INSERT returned no rows"

------------------------------------------------------------------------
-- Read
------------------------------------------------------------------------

getGroup :: Pool Hasql.Connection -> UUID -> IO (Maybe WorkspaceGroup)
getGroup pool gid = do
  rows <- runSession pool $ Session.statement () $ run $ select $ do
    row <- each workspaceGroupSchema
    where_ $ row.wgId ==. lit gid
    pure row
  case rows of
    []    -> pure Nothing
    (r:_) -> pure . Just $ rowToGroup r

------------------------------------------------------------------------
-- Delete
------------------------------------------------------------------------

deleteGroup :: Pool Hasql.Connection -> UUID -> IO Bool
deleteGroup pool gid = do
  n <- runSession pool $ Session.statement () $ runN $
    delete Delete
      { from = workspaceGroupSchema
      , using = pure ()
      , deleteWhere = \_ row -> row.wgId ==. lit gid
      , returning = NoReturning
      }
  pure (n > 0)

------------------------------------------------------------------------
-- List
------------------------------------------------------------------------

listGroups :: Pool Hasql.Connection -> Maybe Int -> Maybe Int -> IO [WorkspaceGroup]
listGroups pool mlimit moffset = do
  let (lim, off) = capPaginationOverfetch mlimit moffset
  rows <- runSession pool $ Session.statement () $ run $ select $
    limit (fromIntegral lim) $ offset (fromIntegral off) $
    orderBy ((\row -> row.wgName) >$< asc) $
    each workspaceGroupSchema
  pure $ map rowToGroup rows

------------------------------------------------------------------------
-- Members
------------------------------------------------------------------------

data AddMemberResult
  = MemberAdded
  | MemberAlreadyPresent
  | MemberWorkspaceInactive
  deriving stock (Eq, Show)

-- The workspace lock makes the active-state predicate and insert one atomic
-- operation with respect to a concurrent workspace soft-delete.
addMember :: Pool Hasql.Connection -> UUID -> UUID -> IO AddMemberResult
addMember pool gid wsId = do
  (inserted, active) <- runSession pool $ Session.statement (gid, wsId) addMemberStatement
  pure $ case (inserted, active) of
    (True, _) -> MemberAdded
    (False, True) -> MemberAlreadyPresent
    (False, False) -> MemberWorkspaceInactive

addMemberStatement :: Statement.Statement (UUID, UUID) (Bool, Bool)
addMemberStatement = Statement.Statement sql encoder decoder True where
  sql =
    "WITH active_workspace AS MATERIALIZED ("
      <> " SELECT id FROM workspaces WHERE id = $2 AND deleted_at IS NULL FOR SHARE"
      <> "), inserted AS ("
      <> " INSERT INTO workspace_group_members (group_id, workspace_id)"
      <> " SELECT $1, $2 FROM active_workspace"
      <> " ON CONFLICT (group_id, workspace_id) DO NOTHING"
      <> " RETURNING 1"
      <> ")"
      <> " SELECT EXISTS (SELECT 1 FROM inserted), EXISTS (SELECT 1 FROM active_workspace)"
  encoder =
       contramap fst (Enc.param (Enc.nonNullable Enc.uuid))
    <> contramap snd (Enc.param (Enc.nonNullable Enc.uuid))
  decoder = Dec.singleRow $
    (,) <$> Dec.column (Dec.nonNullable Dec.bool)
        <*> Dec.column (Dec.nonNullable Dec.bool)

removeMember :: Pool Hasql.Connection -> UUID -> UUID -> IO Bool
removeMember pool gid wsId = do
  n <- runSession pool $ Session.statement () $ runN $
    delete Delete
      { from = workspaceGroupMemberSchema
      , using = pure ()
      , deleteWhere = \_ row -> row.wgmGroupId ==. lit gid &&. row.wgmWorkspaceId ==. lit wsId
      , returning = NoReturning
      }
  pure (n > 0)

listGroupMembers :: Pool Hasql.Connection -> UUID -> IO [UUID]
listGroupMembers pool gid =
  runSession pool $ Session.statement () $ run $ select $ do
    row <- each workspaceGroupMemberSchema
    present $ do
      ws <- each workspaceSchema
      where_ $ ws.wsId ==. row.wgmWorkspaceId
      where_ $ activeWorkspace ws
    where_ $ row.wgmGroupId ==. lit gid
    pure row.wgmWorkspaceId
