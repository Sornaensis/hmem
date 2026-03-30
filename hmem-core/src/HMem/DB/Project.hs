module HMem.DB.Project
  ( createProject
  , getProject
  , updateProject
  , deleteProject
  , listProjects
  , listProjectsWithQuery
  , linkProjectMemory
  , unlinkProjectMemory
  , linkProjectMemoryBatch
  ) where

import Control.Exception (throwIO)
import Control.Monad (when)
import Data.Aeson (Object, toJSON)
import Data.ByteString.Char8 qualified as BS8
import Data.Functor.Contravariant ((>$<))
import Data.Int (Int16)
import Data.Maybe (fromMaybe)
import Data.Pool (Pool)
import Data.UUID (UUID)
import Hasql.Connection qualified as Hasql
import Hasql.Decoders qualified as Dec
import Hasql.Encoders qualified as Enc
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement
import Rel8

import HMem.DB.Pool (runSession, runTransaction, DBException(..))
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

projectSubtreeIdsStatement :: Statement.Statement UUID [UUID]
projectSubtreeIdsStatement = Statement.Statement sql encoder decoder True
  where
    sql = BS8.pack $ unlines
      [ "WITH RECURSIVE project_tree AS ("
      , "  SELECT id"
      , "  FROM projects"
      , "  WHERE id = $1 AND deleted_at IS NULL"
      , "  UNION ALL"
      , "  SELECT p.id"
      , "  FROM projects p"
      , "  JOIN project_tree pt ON p.parent_id = pt.id"
      , "  WHERE p.deleted_at IS NULL"
      , ")"
      , "SELECT id FROM project_tree"
      ]
    encoder = Enc.param (Enc.nonNullable Enc.uuid)
    decoder = Dec.rowList (Dec.column (Dec.nonNullable Dec.uuid))

ensureParentProject :: Pool Hasql.Connection -> UUID -> Maybe UUID -> IO ()
ensureParentProject _ _ Nothing = pure ()
ensureParentProject pool workspaceId (Just parentId) = do
  parent <- getProject pool parentId >>= maybe
    (throwIO $ DBForeignKeyViolation "Referenced parent project does not exist")
    pure
  when (parent.workspaceId /= workspaceId) $
    throwIO $ DBCheckViolation "Parent project must belong to the same workspace"

------------------------------------------------------------------------
-- Create
------------------------------------------------------------------------

createProject :: Pool Hasql.Connection -> CreateProject -> IO Project
createProject pool cp = do
  ensureParentProject pool cp.workspaceId cp.parentId
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
              , projDeletedAt   = unsafeDefault
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
    where_ $ activeProject row
    pure row
  case rows of
    []    -> pure Nothing
    (r:_) -> pure . Just $ rowToProject r

------------------------------------------------------------------------
-- Update
------------------------------------------------------------------------

updateProject :: Pool Hasql.Connection -> UUID -> UpdateProject -> IO (Maybe Project)
updateProject pool pid up = do
  current <- getProject pool pid
  case current of
    Nothing -> pure Nothing
    Just project -> do
      case up.parentId of
        Unchanged   -> pure ()
        SetNull     -> pure ()
        SetTo newId -> ensureParentProject pool project.workspaceId (Just newId)
      rows <- runSession pool $ Session.statement () $ run $
        update Update
          { target = projectSchema
          , from = pure ()
          , set = \_ row -> row
              { projName        = maybe row.projName        lit up.name
              , projDescription = applyNullableUpdate row.projDescription up.description
              , projParentId    = applyNullableUpdate row.projParentId up.parentId
              , projStatus      = maybe row.projStatus      lit up.status
              , projPriority    = maybe row.projPriority    (lit . fromIntegral) up.priority
              , projMetadata    = maybe row.projMetadata    lit up.metadata
              }
          , updateWhere = \_ row -> row.projId ==. lit pid &&. activeProject row
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
  runTransaction pool $ do
    ids <- Session.statement pid projectSubtreeIdsStatement
    case ids of
      [] -> pure False
      _ -> do
        Session.statement () $ run_ $
          update Update
            { target = projectSchema
            , from = pure ()
            , set = \_ row -> row { projDeletedAt = deletedNow }
            , updateWhere = \_ row -> in_ row.projId (map lit ids) &&. activeProject row
            , returning = NoReturning
            }
        Session.statement () $ run_ $
          update Update
            { target = taskSchema
            , from = pure ()
            , set = \_ row -> row { taskProjectId = lit (Nothing :: Maybe UUID) }
            , updateWhere = \_ row -> in_ row.taskProjectId (map lit (Just <$> ids)) &&. activeTask row
            , returning = NoReturning
            }
        Session.statement () $ run_ $
          delete Delete
            { from = projectMemoryLinkSchema
            , using = pure ()
            , deleteWhere = \_ row -> in_ row.pmlProjectId (map lit ids)
            , returning = NoReturning
            }
        pure True

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
listProjects pool wsId mstatus mlimit moffset =
  listProjectsWithQuery pool ProjectListQuery
    { workspaceId = Just wsId
    , status = mstatus
    , createdAfter = Nothing
    , createdBefore = Nothing
    , updatedAfter = Nothing
    , updatedBefore = Nothing
    , limit = mlimit
    , offset = moffset
    }

listProjectsWithQuery :: Pool Hasql.Connection -> ProjectListQuery -> IO [Project]
listProjectsWithQuery pool pq = do
  let lim = fromMaybe 50 pq.limit
      off = fromMaybe 0  pq.offset
  rows <- runSession pool $ Session.statement () $ run $ select $
    limit (fromIntegral lim) $ offset (fromIntegral off) $
    orderBy (((\row -> row.projPriority) >$< desc) <> ((\row -> row.projName) >$< asc)) $ do
      row <- each projectSchema
      case pq.workspaceId of
        Just wid -> where_ $ row.projWorkspaceId ==. lit wid
        Nothing  -> pure ()
      where_ $ activeProject row
      case pq.status of
        Nothing -> pure ()
        Just s  -> where_ $ row.projStatus ==. lit s
      case pq.createdAfter of
        Just createdAfter -> where_ $ row.projCreatedAt >=. lit createdAfter
        Nothing -> pure ()
      case pq.createdBefore of
        Just createdBefore -> where_ $ row.projCreatedAt <=. lit createdBefore
        Nothing -> pure ()
      case pq.updatedAfter of
        Just updatedAfter -> where_ $ row.projUpdatedAt >=. lit updatedAfter
        Nothing -> pure ()
      case pq.updatedBefore of
        Just updatedBefore -> where_ $ row.projUpdatedAt <=. lit updatedBefore
        Nothing -> pure ()
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

-- | Link multiple memories to a project in a single insert.
-- Idempotent: already-linked pairs are silently skipped.
-- Returns the number of memory IDs submitted.
linkProjectMemoryBatch :: Pool Hasql.Connection -> UUID -> [UUID] -> IO Int
linkProjectMemoryBatch _pool _ [] = pure 0
linkProjectMemoryBatch pool pid mids =
  runSession pool $ do
    Session.statement () $ run_ $
      insert Insert
        { into = projectMemoryLinkSchema
        , rows = values
            [ ProjectMemoryLinkT { pmlProjectId = lit pid, pmlMemoryId = lit mid }
            | mid <- mids
            ]
        , onConflict = DoNothing
        , returning = NoReturning
        }
    pure (length mids)
