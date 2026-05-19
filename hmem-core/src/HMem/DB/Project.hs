module HMem.DB.Project
  ( createProject
  , getProject
  , updateProject
  , deleteProject
  , deleteProjectCascade
  , deleteProjectBatch
  , updateProjectBatch
  , restoreProject
  , purgeProjectCascade
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
import Data.Functor.Contravariant ((>$<), contramap)
import Data.Int (Int16)
import Data.Maybe (fromMaybe)
import Data.Pool (Pool)
import Data.Time (UTCTime)
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

projectSubtreeIdsForUpdateStatement :: Statement.Statement UUID [UUID]
projectSubtreeIdsForUpdateStatement = Statement.Statement sql encoder decoder True
  where
    sql = BS8.pack $ unlines
      [ "WITH RECURSIVE project_tree AS ("
      , "  SELECT id"
      , "  FROM projects"
      , "  WHERE id = $1 AND deleted_at IS NULL"
      , "  UNION ALL"
      , "  SELECT child.id"
      , "  FROM projects child"
      , "  JOIN project_tree parent_tree ON child.parent_id = parent_tree.id"
      , "  WHERE child.deleted_at IS NULL"
      , ")"
      , "SELECT project_to_lock.id"
      , "  FROM projects project_to_lock"
      , "  JOIN project_tree ON project_tree.id = project_to_lock.id"
      , " WHERE project_to_lock.deleted_at IS NULL"
      , " ORDER BY project_to_lock.id"
      , " FOR UPDATE OF project_to_lock"
      ]
    encoder = Enc.param (Enc.nonNullable Enc.uuid)
    decoder = Dec.rowList (Dec.column (Dec.nonNullable Dec.uuid))

projectSubtreeIdsForRootsForUpdateStatement :: Statement.Statement [UUID] [UUID]
projectSubtreeIdsForRootsForUpdateStatement = Statement.Statement sql encoder decoder True
  where
    sql = BS8.pack $ unlines
      [ "WITH RECURSIVE project_tree AS ("
      , "  SELECT id"
      , "  FROM projects"
      , "  WHERE id = ANY($1) AND deleted_at IS NULL"
      , "  UNION"
      , "  SELECT child.id"
      , "  FROM projects child"
      , "  JOIN project_tree parent_tree ON child.parent_id = parent_tree.id"
      , "  WHERE child.deleted_at IS NULL"
      , ")"
      , "SELECT project_to_lock.id"
      , "  FROM projects project_to_lock"
      , "  JOIN project_tree ON project_tree.id = project_to_lock.id"
      , " WHERE project_to_lock.deleted_at IS NULL"
      , " ORDER BY project_to_lock.id"
      , " FOR UPDATE OF project_to_lock"
      ]
    encoder = Enc.param (Enc.nonNullable (Enc.foldableArray (Enc.nonNullable Enc.uuid)))
    decoder = Dec.rowList (Dec.column (Dec.nonNullable Dec.uuid))

deletedProjectSubtreeIdsStatement :: Statement.Statement (UUID, UTCTime) [UUID]
deletedProjectSubtreeIdsStatement = Statement.Statement sql encoder decoder True
  where
    sql = BS8.pack $ unlines
      [ "WITH RECURSIVE project_tree AS ("
      , "  SELECT id"
      , "  FROM projects"
      , "  WHERE id = $1 AND deleted_at = $2"
      , "  UNION ALL"
      , "  SELECT p.id"
      , "  FROM projects p"
      , "  JOIN project_tree pt ON p.parent_id = pt.id"
      , "  WHERE p.deleted_at = $2"
      , ")"
      , "SELECT id FROM project_tree"
      ]
    encoder =
      contramap fst (Enc.param (Enc.nonNullable Enc.uuid)) <>
      contramap snd (Enc.param (Enc.nonNullable Enc.timestamptz))
    decoder = Dec.rowList (Dec.column (Dec.nonNullable Dec.uuid))

allProjectSubtreeIdsStatement :: Statement.Statement UUID [UUID]
allProjectSubtreeIdsStatement = Statement.Statement sql encoder decoder True
  where
    sql = BS8.pack $ unlines
      [ "WITH RECURSIVE project_tree AS ("
      , "  SELECT id"
      , "  FROM projects"
      , "  WHERE id = $1"
      , "  UNION"
      , "  SELECT p.id"
      , "  FROM projects p"
      , "  JOIN project_tree pt ON p.parent_id = pt.id"
      , ")"
      , "SELECT id FROM project_tree"
      ]
    encoder = Enc.param (Enc.nonNullable Enc.uuid)
    decoder = Dec.rowList (Dec.column (Dec.nonNullable Dec.uuid))

activeProjectTaskIdsForUpdateStatement :: Statement.Statement [UUID] [UUID]
activeProjectTaskIdsForUpdateStatement = Statement.Statement sql encoder decoder True
  where
    sql = BS8.pack $ unlines
      [ "WITH RECURSIVE task_tree AS ("
      , "  SELECT id"
      , "    FROM tasks"
      , "   WHERE project_id = ANY($1)"
      , "     AND deleted_at IS NULL"
      , "  UNION"
      , "  SELECT child.id"
      , "    FROM tasks child"
      , "    JOIN task_tree parent ON child.parent_id = parent.id"
      , "   WHERE child.deleted_at IS NULL"
      , ")"
      , "SELECT task_to_lock.id"
      , "  FROM tasks task_to_lock"
      , "  JOIN task_tree ON task_tree.id = task_to_lock.id"
      , " WHERE task_to_lock.deleted_at IS NULL"
      , " ORDER BY task_to_lock.id"
      , " FOR UPDATE OF task_to_lock"
      ]
    encoder = Enc.param (Enc.nonNullable (Enc.foldableArray (Enc.nonNullable Enc.uuid)))
    decoder = Dec.rowList (Dec.column (Dec.nonNullable Dec.uuid))

deletedProjectTaskIdsStatement :: Statement.Statement ([UUID], UTCTime) [UUID]
deletedProjectTaskIdsStatement = Statement.Statement sql encoder decoder True
  where
    sql = BS8.pack $ unlines
      [ "WITH RECURSIVE task_tree AS ("
      , "  SELECT id"
      , "    FROM tasks"
      , "   WHERE project_id = ANY($1)"
      , "     AND deleted_at = $2"
      , "  UNION"
      , "  SELECT child.id"
      , "    FROM tasks child"
      , "    JOIN task_tree parent ON child.parent_id = parent.id"
      , "   WHERE child.deleted_at = $2"
      , ")"
      , "SELECT id FROM task_tree"
      ]
    uuidArrayEncoder = Enc.param (Enc.nonNullable (Enc.foldableArray (Enc.nonNullable Enc.uuid)))
    encoder =
      contramap fst uuidArrayEncoder <>
      contramap snd (Enc.param (Enc.nonNullable Enc.timestamptz))
    decoder = Dec.rowList (Dec.column (Dec.nonNullable Dec.uuid))

allProjectTaskIdsStatement :: Statement.Statement [UUID] [UUID]
allProjectTaskIdsStatement = Statement.Statement sql encoder decoder True
  where
    sql = BS8.pack $ unlines
      [ "WITH RECURSIVE task_tree AS ("
      , "  SELECT id"
      , "    FROM tasks"
      , "   WHERE project_id = ANY($1)"
      , "  UNION"
      , "  SELECT child.id"
      , "    FROM tasks child"
      , "    JOIN task_tree parent ON child.parent_id = parent.id"
      , ")"
      , "SELECT id FROM task_tree"
      ]
    encoder = Enc.param (Enc.nonNullable (Enc.foldableArray (Enc.nonNullable Enc.uuid)))
    decoder = Dec.rowList (Dec.column (Dec.nonNullable Dec.uuid))

softDeleteProjectsStatement :: Statement.Statement [UUID] Int
softDeleteProjectsStatement = Statement.Statement sql encoder decoder True
  where
    sql = BS8.pack $ unlines
      [ "WITH updated AS ("
      , "  UPDATE projects"
      , "     SET deleted_at = now()"
      , "   WHERE id = ANY($1)"
      , "     AND deleted_at IS NULL"
      , "  RETURNING id"
      , ")"
      , "SELECT count(*)::int FROM updated"
      ]
    encoder = Enc.param (Enc.nonNullable (Enc.foldableArray (Enc.nonNullable Enc.uuid)))
    decoder = Dec.singleRow (fromIntegral <$> Dec.column (Dec.nonNullable Dec.int4))

softDeleteProjectTasksStatement :: Statement.Statement [UUID] Int
softDeleteProjectTasksStatement = Statement.Statement sql encoder decoder True
  where
    sql = BS8.pack $ unlines
      [ "WITH updated AS ("
      , "  UPDATE tasks"
      , "     SET deleted_at = now()"
      , "   WHERE id = ANY($1)"
      , "     AND deleted_at IS NULL"
      , "  RETURNING id"
      , ")"
      , "SELECT count(*)::int FROM updated"
      ]
    encoder = Enc.param (Enc.nonNullable (Enc.foldableArray (Enc.nonNullable Enc.uuid)))
    decoder = Dec.singleRow (fromIntegral <$> Dec.column (Dec.nonNullable Dec.int4))

deleteProjectTaskDependenciesStatement :: Statement.Statement [UUID] Int
deleteProjectTaskDependenciesStatement = Statement.Statement sql encoder decoder True
  where
    sql = BS8.pack $ unlines
      [ "WITH deleted AS ("
      , "  DELETE FROM task_dependencies"
      , "   WHERE task_id = ANY($1)"
      , "      OR depends_on_id = ANY($1)"
      , "  RETURNING task_id"
      , ")"
      , "SELECT count(*)::int FROM deleted"
      ]
    encoder = Enc.param (Enc.nonNullable (Enc.foldableArray (Enc.nonNullable Enc.uuid)))
    decoder = Dec.singleRow (fromIntegral <$> Dec.column (Dec.nonNullable Dec.int4))

purgeProjectTasksStatement :: Statement.Statement [UUID] Int
purgeProjectTasksStatement = Statement.Statement sql encoder decoder True
  where
    sql = BS8.pack $ unlines
      [ "WITH deleted AS ("
      , "  DELETE FROM tasks"
      , "   WHERE id = ANY($1)"
      , "  RETURNING id"
      , ")"
      , "SELECT count(*)::int FROM deleted"
      ]
    encoder = Enc.param (Enc.nonNullable (Enc.foldableArray (Enc.nonNullable Enc.uuid)))
    decoder = Dec.singleRow (fromIntegral <$> Dec.column (Dec.nonNullable Dec.int4))

purgeProjectsStatement :: Statement.Statement [UUID] Int
purgeProjectsStatement = Statement.Statement sql encoder decoder True
  where
    sql = BS8.pack $ unlines
      [ "WITH deleted AS ("
      , "  DELETE FROM projects"
      , "   WHERE id = ANY($1)"
      , "  RETURNING id"
      , ")"
      , "SELECT count(*)::int FROM deleted"
      ]
    encoder = Enc.param (Enc.nonNullable (Enc.foldableArray (Enc.nonNullable Enc.uuid)))
    decoder = Dec.singleRow (fromIntegral <$> Dec.column (Dec.nonNullable Dec.int4))

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
              , projSearchVector = unsafeDefault
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
    Just currentProject -> do
      case up.parentId of
        Unchanged   -> pure ()
        SetNull     -> pure ()
        SetTo newId -> ensureParentProject pool currentProject.workspaceId (Just newId)
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
deleteProject pool pid = maybe False (const True) <$> deleteProjectCascade pool pid

deleteProjectCascade :: Pool Hasql.Connection -> UUID -> IO (Maybe CascadeResult)
deleteProjectCascade pool pid = do
  runTransaction pool $ do
    -- Lock projects before tasks so project deletion and task project-move
    -- operations acquire rows in the same coarse order: projects, then tasks.
    projectIds <- Session.statement pid projectSubtreeIdsForUpdateStatement
    deleteProjectIdsCascadeS projectIds

-- | Soft-delete multiple projects by ID in a single transaction, cascading to
-- project subtrees and active tasks within those project subtrees. Returns the
-- total number of project and task rows actually deleted.
deleteProjectBatch :: Pool Hasql.Connection -> [UUID] -> IO Int
deleteProjectBatch _pool [] = pure 0
deleteProjectBatch pool ids = do
  runTransaction pool $ do
    -- Lock projects before tasks for the same order used by task project moves.
    projectIds <- Session.statement ids projectSubtreeIdsForRootsForUpdateStatement
    mResult <- deleteProjectIdsCascadeS projectIds
    pure $ maybe 0 (.affected) mResult

deleteProjectIdsCascadeS :: [UUID] -> Session.Session (Maybe CascadeResult)
deleteProjectIdsCascadeS [] = pure Nothing
deleteProjectIdsCascadeS projectIds = do
  taskIds <- Session.statement projectIds activeProjectTaskIdsForUpdateStatement
  memoryCount <- softDeleteProjectAndTaskMemoriesS projectIds taskIds
  dependencyCount <- deleteProjectTaskDependenciesS taskIds
  taskCount <- softDeleteProjectTasksS taskIds
  projectCount <- Session.statement projectIds softDeleteProjectsStatement
  pure . Just $ CascadeResult
    { affected = projectCount + taskCount
    , projectCount = projectCount
    , taskCount = taskCount
    , memoryCount = memoryCount
    , dependencyCount = dependencyCount
    }

-- | Batch-update multiple projects. Each item is updated individually.
-- Returns the count of successfully updated projects.
updateProjectBatch :: Pool Hasql.Connection -> [(UUID, UpdateProject)] -> IO Int
updateProjectBatch _pool [] = pure 0
updateProjectBatch pool items = do
  results <- mapM (\(pid, up) -> updateProject pool pid up) items
  pure $ length [() | Just _ <- results]

-- | Restore a soft-deleted project by clearing its deleted_at timestamp.
-- Returns True if the project was restored, False if not found or not deleted.
restoreProject :: Pool Hasql.Connection -> UUID -> IO Bool
restoreProject pool pid = do
  runTransaction pool $ do
    rows <- Session.statement () $ run $ select $ do
      row <- each projectSchema
      where_ $ row.projId ==. lit pid
      pure row
    case rows of
      [] -> pure False
      (row:_)
        | Just deletedAt <- row.projDeletedAt -> do
            ids <- Session.statement (pid, deletedAt) deletedProjectSubtreeIdsStatement
            taskIds <- Session.statement (ids, deletedAt) deletedProjectTaskIdsStatement
            n <- Session.statement () $ runN $
              update Update
                { target = projectSchema
                , from = pure ()
                , set = \_ projectRow -> projectRow { projDeletedAt = lit (Nothing :: Maybe UTCTime) }
                , updateWhere = \_ projectRow -> in_ projectRow.projId (map lit ids) &&. not_ (isNull projectRow.projDeletedAt)
                , returning = NoReturning
                }
            _taskN <- Session.statement () $ runN $
              update Update
                { target = taskSchema
                , from = pure ()
                , set = \_ task -> task { taskDeletedAt = lit (Nothing :: Maybe UTCTime) }
                , updateWhere = \_ task -> in_ task.taskId (map lit taskIds) &&. task.taskDeletedAt ==. lit (Just deletedAt)
                , returning = NoReturning
                }
            _ <- restoreProjectAndTaskMemoriesS ids taskIds deletedAt
            pure (n > 0)
        | otherwise -> pure False

purgeProjectCascade :: Pool Hasql.Connection -> UUID -> IO (Maybe CascadeResult)
purgeProjectCascade pool pid =
  runTransaction pool $ do
    rows <- Session.statement () $ run $ select $ do
      row <- each projectSchema
      where_ $ row.projId ==. lit pid
      pure row
    case rows of
      [] -> pure Nothing
      (row:_) -> case row.projDeletedAt of
        Nothing -> pure Nothing
        Just _deletedAt -> do
          -- Purge hard-deletes the full project subtree regardless of individual
          -- descendant deleted_at timestamps.  Restore remains timestamp-scoped,
          -- but purge must delete older pre-deleted child projects and their
          -- tasks before project_id ON DELETE SET NULL can detach task rows.
          projectIds <- Session.statement pid allProjectSubtreeIdsStatement
          -- Purge hard-deletes every task row still associated with the project
          -- subtree, including tasks that were soft-deleted before the project
          -- delete timestamp.  Otherwise project_id ON DELETE SET NULL would
          -- detach those older deleted tasks when the project rows are purged.
          taskIds <- Session.statement projectIds allProjectTaskIdsStatement
          dependencyCount <- deleteProjectTaskDependenciesS taskIds
          taskCount <- purgeProjectTasksS taskIds
          projectCount <- Session.statement projectIds purgeProjectsStatement
          pure . Just $ CascadeResult
            { affected = projectCount + taskCount
            , projectCount = projectCount
            , taskCount = taskCount
            , memoryCount = 0
            , dependencyCount = dependencyCount
            }

softDeleteProjectAndTaskMemoriesS :: [UUID] -> [UUID] -> Session.Session Int
softDeleteProjectAndTaskMemoriesS [] [] = pure 0
softDeleteProjectAndTaskMemoriesS projectIds taskIds =
  Session.statement (projectIds, taskIds) softDeleteProjectAndTaskMemoriesStatement

softDeleteProjectAndTaskMemoriesStatement :: Statement.Statement ([UUID], [UUID]) Int
softDeleteProjectAndTaskMemoriesStatement = Statement.Statement sql encoder decoder True
  where
    sql = BS8.pack $ unlines
      [ "WITH updated AS ("
      , "  UPDATE memories m"
      , "  SET deleted_at = now()"
      , "  WHERE m.deleted_at IS NULL"
      , "    AND ("
      , "      EXISTS ("
      , "        SELECT 1 FROM project_memory_links pml"
      , "        WHERE pml.memory_id = m.id"
      , "          AND pml.project_id = ANY($1)"
      , "      )"
      , "      OR EXISTS ("
      , "        SELECT 1 FROM task_memory_links tml"
      , "        WHERE tml.memory_id = m.id"
      , "          AND tml.task_id = ANY($2)"
      , "      )"
      , "    )"
      , "    AND NOT EXISTS ("
      , "    SELECT 1"
      , "    FROM project_memory_links pml"
      , "    JOIN projects p ON p.id = pml.project_id"
      , "    WHERE pml.memory_id = m.id"
      , "      AND p.deleted_at IS NULL"
      , "      AND p.workspace_id = m.workspace_id"
      , "      AND p.id <> ALL($1)"
      , "    )"
      , "    AND NOT EXISTS ("
      , "    SELECT 1"
      , "    FROM task_memory_links tml"
      , "    JOIN tasks t ON t.id = tml.task_id"
      , "    WHERE tml.memory_id = m.id"
      , "      AND t.deleted_at IS NULL"
      , "      AND t.workspace_id = m.workspace_id"
      , "      AND t.id <> ALL($2)"
      , "    )"
      , "  RETURNING m.id"
      , ")"
      , "SELECT count(*)::int FROM updated"
      ]
    uuidArrayEncoder = Enc.param (Enc.nonNullable (Enc.foldableArray (Enc.nonNullable Enc.uuid)))
    encoder = contramap fst uuidArrayEncoder <> contramap snd uuidArrayEncoder
    decoder = Dec.singleRow (fromIntegral <$> Dec.column (Dec.nonNullable Dec.int4))

restoreProjectAndTaskMemoriesS :: [UUID] -> [UUID] -> UTCTime -> Session.Session Int
restoreProjectAndTaskMemoriesS [] [] _ = pure 0
restoreProjectAndTaskMemoriesS projectIds taskIds deletedAt =
  Session.statement (projectIds, taskIds, deletedAt) restoreProjectAndTaskMemoriesStatement

restoreProjectAndTaskMemoriesStatement :: Statement.Statement ([UUID], [UUID], UTCTime) Int
restoreProjectAndTaskMemoriesStatement = Statement.Statement sql encoder decoder True
  where
    sql = BS8.pack $ unlines
      [ "WITH updated AS ("
      , "  UPDATE memories m"
      , "  SET deleted_at = NULL"
      , "  WHERE m.deleted_at IS NOT NULL"
      , "    AND ("
      , "      EXISTS ("
      , "        SELECT 1 FROM project_memory_links pml"
      , "        WHERE pml.memory_id = m.id"
      , "          AND pml.project_id = ANY($1)"
      , "      )"
      , "      OR EXISTS ("
      , "        SELECT 1 FROM task_memory_links tml"
      , "        WHERE tml.memory_id = m.id"
      , "          AND tml.task_id = ANY($2)"
      , "      )"
      , "    )"
      , "    AND ("
      , "    m.deleted_at = $3"
      , "    OR EXISTS ("
      , "      SELECT 1"
      , "      FROM project_memory_links pml_deleted"
      , "      JOIN projects p_deleted ON p_deleted.id = pml_deleted.project_id"
      , "      WHERE pml_deleted.memory_id = m.id"
      , "        AND p_deleted.deleted_at = m.deleted_at"
      , "    )"
      , "    OR EXISTS ("
      , "      SELECT 1"
      , "      FROM task_memory_links tml_deleted"
      , "      JOIN tasks t_deleted ON t_deleted.id = tml_deleted.task_id"
      , "      WHERE tml_deleted.memory_id = m.id"
      , "        AND t_deleted.deleted_at = m.deleted_at"
      , "    )"
      , "    )"
      , "    AND ("
      , "    EXISTS ("
      , "      SELECT 1"
      , "      FROM project_memory_links pml_active"
      , "      JOIN projects p_active ON p_active.id = pml_active.project_id"
      , "      WHERE pml_active.memory_id = m.id"
      , "        AND p_active.deleted_at IS NULL"
      , "        AND p_active.workspace_id = m.workspace_id"
      , "    )"
      , "    OR EXISTS ("
      , "      SELECT 1"
      , "      FROM task_memory_links tml_active"
      , "      JOIN tasks t_active ON t_active.id = tml_active.task_id"
      , "      WHERE tml_active.memory_id = m.id"
      , "        AND t_active.deleted_at IS NULL"
      , "        AND t_active.workspace_id = m.workspace_id"
      , "    )"
      , "    )"
      , "  RETURNING m.id"
      , ")"
      , "SELECT count(*)::int FROM updated"
      ]
    uuidArrayEncoder = Enc.param (Enc.nonNullable (Enc.foldableArray (Enc.nonNullable Enc.uuid)))
    encoder =
      contramap (\(projectIds, _, _) -> projectIds) uuidArrayEncoder <>
      contramap (\(_, taskIds, _) -> taskIds) uuidArrayEncoder <>
      contramap (\(_, _, deletedAt) -> deletedAt) (Enc.param (Enc.nonNullable Enc.timestamptz))
    decoder = Dec.singleRow (fromIntegral <$> Dec.column (Dec.nonNullable Dec.int4))

softDeleteProjectTasksS :: [UUID] -> Session.Session Int
softDeleteProjectTasksS [] = pure 0
softDeleteProjectTasksS ids = Session.statement ids softDeleteProjectTasksStatement

deleteProjectTaskDependenciesS :: [UUID] -> Session.Session Int
deleteProjectTaskDependenciesS [] = pure 0
deleteProjectTaskDependenciesS ids = Session.statement ids deleteProjectTaskDependenciesStatement

purgeProjectTasksS :: [UUID] -> Session.Session Int
purgeProjectTasksS [] = pure 0
purgeProjectTasksS ids = Session.statement ids purgeProjectTasksStatement

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
    , query = Nothing
    , searchLanguage = Nothing
    , createdAfter = Nothing
    , createdBefore = Nothing
    , updatedAfter = Nothing
    , updatedBefore = Nothing
    , limit = mlimit
    , offset = moffset
    }

listProjectsWithQuery :: Pool Hasql.Connection -> ProjectListQuery -> IO [Project]
listProjectsWithQuery pool pq = do
  let (lim, off) = capPaginationOverfetch pq.limit pq.offset
      searchLang = fromMaybe "english" pq.searchLanguage
      applyFilters row = do
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
  case pq.query of
    Nothing -> do
      rows <- runSession pool $ Session.statement () $ run $ select $
        limit (fromIntegral lim) $ offset (fromIntegral off) $
        orderBy (((\row -> row.projPriority) >$< desc) <> ((\row -> row.projName) >$< asc)) $ do
          row <- each projectSchema
          applyFilters row
          pure row
      pure $ map rowToProject rows
    Just q -> do
      results <- runSession pool $ Session.statement () $ run $ select $
        limit (fromIntegral lim) $ offset (fromIntegral off) $
        orderBy (snd >$< desc) $ do
          row <- each projectSchema
          applyFilters row
          let config = unsafeCastExpr (lit searchLang) :: Expr PgRegConfig
          let tsq = function "plainto_tsquery" (config, lit q) :: Expr PgTSQuery
          let tsvec = row.projSearchVector :: Expr PgTSVector
          where_ $ rawBinaryOperator "@@" tsvec tsq
          let tsRank = function "ts_rank" (tsvec, tsq) :: Expr Double
          pure (row, tsRank)
      pure $ map (rowToProject . fst) results

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
