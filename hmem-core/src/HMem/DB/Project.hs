module HMem.DB.Project
  ( createProject
  , getProject
  , updateProject
  , deleteProject
  , listProjects
  , linkProjectMemory
  , unlinkProjectMemory
  ) where

import Control.Exception (throwIO)
import Data.Aeson (Object, toJSON)
import Data.Functor.Contravariant ((>$<))
import Data.Int (Int16)
import Data.Maybe (fromMaybe)
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

rowToProject :: ProjectT Result -> Project
rowToProject r = Project
  { id          = r.projId
  , workspaceId = r.projWorkspaceId
  , parentId    = r.projParentId
  , name        = r.projName
  , description = r.projDescription
  , status      = r.projStatus
  , priority    = fromIntegral r.projPriority
  , metadata    = r.projMetadata
  , createdAt   = r.projCreatedAt
  , updatedAt   = r.projUpdatedAt
  }

------------------------------------------------------------------------
-- Create
------------------------------------------------------------------------

createProject :: Pool Hasql.Connection -> CreateProject -> IO Project
createProject pool cp = do
  let pri  = maybe 5 fromIntegral (cp.priority) :: Int16
      meta = fromMaybe (toJSON (mempty :: Object)) (cp.metadata)
  rows <- runSession pool $ Session.statement () $ run $
    insert Insert
      { into = projectSchema
      , rows = values
          [ ProjectT
              { projId          = unsafeDefault
              , projWorkspaceId = lit cp.workspaceId
              , projParentId    = lit cp.parentId
              , projName        = lit cp.name
              , projDescription = lit cp.description
              , projStatus      = unsafeDefault
              , projPriority    = lit pri
              , projMetadata    = lit meta
              , projCreatedAt   = unsafeDefault
              , projUpdatedAt   = unsafeDefault
              }
          ]
      , onConflict = Abort
      , returning  = Returning id
      }
  case rows of
    (r:_) -> pure $ rowToProject r
    []    -> throwIO $ DBOtherError "createProject: INSERT returned no rows"

------------------------------------------------------------------------
-- Read
------------------------------------------------------------------------

getProject :: Pool Hasql.Connection -> UUID -> IO (Maybe Project)
getProject pool pid = do
  rows <- runSession pool $ Session.statement () $ run $ select $ do
    row <- each projectSchema
    where_ $ row.projId ==. lit pid
    pure row
  case rows of
    []    -> pure Nothing
    (r:_) -> pure . Just $ rowToProject r

------------------------------------------------------------------------
-- Update
------------------------------------------------------------------------

updateProject :: Pool Hasql.Connection -> UUID -> UpdateProject -> IO (Maybe Project)
updateProject pool pid up = do
  rows <- runSession pool $ Session.statement () $ run $
    update Update
      { target = projectSchema
      , from = pure ()
      , set = \_ row -> row
          { projName        = maybe row.projName        lit up.name
          , projDescription = applyNullableUpdate row.projDescription up.description
          , projStatus      = maybe row.projStatus      lit up.status
          , projPriority    = maybe row.projPriority    (lit . fromIntegral) up.priority
          , projMetadata    = maybe row.projMetadata    lit up.metadata
          }
      , updateWhere = \_ row -> row.projId ==. lit pid
      , returning = Returning id
      }
  case rows of
    []    -> pure Nothing
    (r:_) -> pure . Just $ rowToProject r

------------------------------------------------------------------------
-- Delete
------------------------------------------------------------------------

deleteProject :: Pool Hasql.Connection -> UUID -> IO Bool
deleteProject pool pid = do
  n <- runSession pool $ Session.statement () $ runN $
    delete Delete
      { from = projectSchema
      , using = pure ()
      , deleteWhere = \_ row -> row.projId ==. lit pid
      , returning = NoReturning
      }
  pure (n > 0)

------------------------------------------------------------------------
-- List
------------------------------------------------------------------------

listProjects
  :: Pool Hasql.Connection
  -> UUID               -- ^ workspace_id
  -> Maybe ProjectStatus
  -> Maybe Int          -- ^ limit
  -> Maybe Int          -- ^ offset
  -> IO [Project]
listProjects pool wsId mstatus mlimit moffset = do
  let lim = fromMaybe 50 mlimit
      off = fromMaybe 0  moffset
  rows <- runSession pool $ Session.statement () $ run $ select $
    limit (fromIntegral lim) $ offset (fromIntegral off) $
    orderBy (((\row -> row.projPriority) >$< desc) <> ((\row -> row.projName) >$< asc)) $ do
      row <- each projectSchema
      where_ $ row.projWorkspaceId ==. lit wsId
      case mstatus of
        Nothing -> pure ()
        Just s  -> where_ $ row.projStatus ==. lit s
      pure row
  pure $ map rowToProject rows

------------------------------------------------------------------------
-- Memory links
------------------------------------------------------------------------

linkProjectMemory :: Pool Hasql.Connection -> UUID -> UUID -> IO ()
linkProjectMemory pool pid mid =
  runSession pool $ Session.statement () $ run_ $
    insert Insert
      { into = projectMemoryLinkSchema
      , rows = values
          [ ProjectMemoryLinkT
              { pmlProjectId = lit pid
              , pmlMemoryId  = lit mid
              }
          ]
      , onConflict = DoNothing
      , returning = NoReturning
      }

unlinkProjectMemory :: Pool Hasql.Connection -> UUID -> UUID -> IO ()
unlinkProjectMemory pool pid mid =
  runSession pool $ Session.statement () $ run_ $
    delete Delete
      { from = projectMemoryLinkSchema
      , using = pure ()
      , deleteWhere = \_ row -> row.pmlProjectId ==. lit pid &&. row.pmlMemoryId ==. lit mid
      , returning = NoReturning
      }
