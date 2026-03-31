module HMem.DB.WorkspaceGroup
  ( createGroup
  , getGroup
  , deleteGroup
  , listGroups
  , addMember
  , removeMember
  , listGroupMembers
  ) where

import Control.Exception (throwIO)
import Data.Functor.Contravariant ((>$<))
import Data.Pool (Pool)
import Data.UUID (UUID)
import Hasql.Connection qualified as Hasql
import Hasql.Session qualified as Session
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
  let (lim, off) = capPagination mlimit moffset
  rows <- runSession pool $ Session.statement () $ run $ select $
    limit (fromIntegral lim) $ offset (fromIntegral off) $
    orderBy ((\row -> row.wgName) >$< asc) $
    each workspaceGroupSchema
  pure $ map rowToGroup rows

------------------------------------------------------------------------
-- Members
------------------------------------------------------------------------

addMember :: Pool Hasql.Connection -> UUID -> UUID -> IO ()
addMember pool gid wsId =
  runSession pool $ Session.statement () $ run_ $
    insert Insert
      { into = workspaceGroupMemberSchema
      , rows = values
          [ WorkspaceGroupMemberT
              { wgmGroupId     = lit gid
              , wgmWorkspaceId = lit wsId
              , wgmJoinedAt    = unsafeDefault
              }
          ]
      , onConflict = DoNothing
      , returning = NoReturning
      }

removeMember :: Pool Hasql.Connection -> UUID -> UUID -> IO ()
removeMember pool gid wsId =
  runSession pool $ Session.statement () $ run_ $
    delete Delete
      { from = workspaceGroupMemberSchema
      , using = pure ()
      , deleteWhere = \_ row -> row.wgmGroupId ==. lit gid &&. row.wgmWorkspaceId ==. lit wsId
      , returning = NoReturning
      }

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
