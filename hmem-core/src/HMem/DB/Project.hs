module HMem.DB.Project
  ( createProject
  , getProject
  , getProjectsByIds
  , getProjectAncestorIds
  , updateProject
  , deleteProject
  , deleteProjectCascade
  , deleteProjectBatch
  , updateProjectBatch
  , restoreProject
  , purgeProjectCascade
  , listProjects
  , listProjectsWithQuery
  , listProjectChildren
  , listFilteredProjectChildren
  ) where

import Control.Exception (throwIO)
import Control.Monad (when)
import Data.Aeson (Object, toJSON)
import Data.ByteString.Char8 qualified as BS8
import Data.Functor.Contravariant ((>$<), contramap)
import Data.Int (Int16, Int32)
import Data.Maybe (fromMaybe)
import Data.Pool (Pool)
import Data.Text (Text)
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

projectCardRowDecoder :: Dec.Row Project
projectCardRowDecoder = do
  projectId <- Dec.column (Dec.nonNullable Dec.uuid)
  projectWorkspaceId <- Dec.column (Dec.nonNullable Dec.uuid)
  projectParentId <- Dec.column (Dec.nullable Dec.uuid)
  projectName <- Dec.column (Dec.nonNullable Dec.text)
  projectDescription <- Dec.column (Dec.nullable Dec.text)
  statusText <- Dec.column (Dec.nonNullable Dec.text)
  projectStatus <- maybe (fail $ "Unexpected project_status_enum value: " <> show statusText) pure (projectStatusFromText statusText)
  projectPriority <- Dec.column (Dec.nonNullable Dec.int2)
  projectMetadata <- Dec.column (Dec.nonNullable Dec.jsonb)
  projectCreatedAt <- Dec.column (Dec.nonNullable Dec.timestamptz)
  projectUpdatedAt <- Dec.column (Dec.nonNullable Dec.timestamptz)
  pure Project
    { id = projectId, workspaceId = projectWorkspaceId, parentId = projectParentId
    , name = projectName, description = projectDescription, status = projectStatus
    , priority = fromIntegral projectPriority, metadata = projectMetadata
    , createdAt = projectCreatedAt, updatedAt = projectUpdatedAt }

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

-- | Fetch a bounded caller-supplied set in one statement.  Callers retain
-- request ordering at their boundary; this query deliberately returns DB order.
getProjectsByIds :: Pool Hasql.Connection -> [UUID] -> IO [Project]
getProjectsByIds pool ids
  | Prelude.null ids = pure []
  | otherwise = do
      rows <- runSession pool $ Session.statement () $ run $ select $ do
        row <- each projectSchema
        where_ $ in_ row.projId (map lit ids)
        where_ $ activeProject row
        pure row
      pure (map rowToProject rows)

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
  dependencyCount <- deleteProjectTaskDependenciesS taskIds
  taskCount <- softDeleteProjectTasksS taskIds
  projectCount <- Session.statement projectIds softDeleteProjectsStatement
  pure . Just $ CascadeResult
    { affected = projectCount + taskCount
    , projectCount = projectCount
    , taskCount = taskCount
    , dependencyLinkCount = dependencyCount
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
            , dependencyLinkCount = dependencyCount
            }

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
        -- Offset pagination requires a total order.  The UUID tie breaker keeps
        -- pages stable when projects share a priority and name.
        orderBy (((\row -> row.projPriority) >$< desc) <> ((\row -> row.projName) >$< asc) <> ((\row -> row.projId) >$< asc)) $ do
          row <- each projectSchema
          applyFilters row
          pure row
      pure $ map rowToProject rows

    Just q -> do
      results <- runSession pool $ Session.statement () $ run $ select $
        limit (fromIntegral lim) $ offset (fromIntegral off) $
        orderBy ((snd >$< desc) <> ((\(row, _) -> row.projId) >$< asc)) $ do
          row <- each projectSchema
          applyFilters row
          let config = unsafeCastExpr (lit searchLang) :: Expr PgRegConfig
          let tsq = function "plainto_tsquery" (config, lit q) :: Expr PgTSQuery
          let tsvec = row.projSearchVector :: Expr PgTSVector
          where_ $ rawBinaryOperator "@@" tsvec tsq
          let tsRank = function "ts_rank" (tsvec, tsq) :: Expr Double
          pure (row, tsRank)
      pure $ map (rowToProject . fst) results

-- | Return the root-to-parent chain for a project in one bounded recursive
-- query.  The focus handler decorates this set in one further batch, rather
-- than issuing a lookup and rollup query for every breadcrumb segment.
getProjectAncestorIds :: Pool Hasql.Connection -> UUID -> UUID -> Int -> IO [UUID]
getProjectAncestorIds pool wsId projectId takeN =
  runSession pool $ Session.statement (wsId, projectId, fromIntegral takeN :: Int32) projectAncestorIdsStatement

projectAncestorIdsStatement :: Statement.Statement (UUID, UUID, Int32) [UUID]
projectAncestorIdsStatement = Statement.Statement sql encoder decoder True
  where
    sql = BS8.pack $ unlines
      [ "WITH RECURSIVE ancestors(id, depth) AS ("
      , " SELECT parent_id, 0 FROM projects WHERE id=$2 AND workspace_id=$1 AND deleted_at IS NULL AND parent_id IS NOT NULL"
      , " UNION ALL"
      , " SELECT parent.parent_id, current.depth + 1 FROM projects parent JOIN ancestors current ON parent.id=current.id"
      , " WHERE parent.workspace_id=$1 AND parent.deleted_at IS NULL AND parent.parent_id IS NOT NULL"
      , ") SELECT id FROM ancestors ORDER BY depth DESC LIMIT $3"
      ]
    encoder =
      contramap (\(a,_,_) -> a) (Enc.param (Enc.nonNullable Enc.uuid)) <>
      contramap (\(_,b,_) -> b) (Enc.param (Enc.nonNullable Enc.uuid)) <>
      contramap (\(_,_,c) -> c) (Enc.param (Enc.nonNullable Enc.int4))
    decoder = Dec.rowList (Dec.column (Dec.nonNullable Dec.uuid))

-- | Bounded direct-child navigation.  This deliberately filters in SQL rather
-- than loading a workspace and pruning it in the caller.
listProjectChildren :: Pool Hasql.Connection -> UUID -> Maybe UUID -> Int -> Int -> IO [Project]
listProjectChildren pool wsId parent lim off = do
  rows <- runSession pool $ Session.statement () $ run $ select $
    limit (fromIntegral lim) $ offset (fromIntegral off) $
      orderBy (((\row -> unsafeCastExpr row.projStatus :: Expr Text) >$< asc) <> ((\row -> row.projPriority) >$< desc) <> ((\row -> row.projName) >$< asc) <> ((\row -> row.projId) >$< asc)) $ do
        row <- each projectSchema
        where_ $ activeProject row
        where_ $ row.projWorkspaceId ==. lit wsId
        where_ $ row.projParentId ==. lit parent
        pure row
  pure $ map rowToProject rows

-- | Tree filtering is evaluated by the database, not by an unbounded client
-- cache. A matching descendant retains its direct branch ancestor even when
-- that ancestor itself does not satisfy the status or priority predicate.
listFilteredProjectChildren :: Pool Hasql.Connection -> UUID -> Maybe UUID -> NavigationFilter -> Int -> Int -> IO [Project]
listFilteredProjectChildren pool workspace parent selector lim off =
  runSession pool $ Session.statement
    ( workspace, parent, map projectStatusToText selector.projectStatuses, map taskStatusToText selector.taskStatuses
    , selector.priorityMode, selector.priorityValue, selector.query, fromMaybe "all" selector.showOnly, fromIntegral lim :: Int32, fromIntegral off :: Int32 )
    filteredProjectChildrenStatement

filteredProjectChildrenStatement :: Statement.Statement (UUID, Maybe UUID, [Text], [Text], Maybe Text, Maybe Int, Maybe Text, Text, Int32, Int32) [Project]
filteredProjectChildrenStatement = Statement.Statement sql encoder (Dec.rowList projectCardRowDecoder) True
  where
    sql = BS8.pack $ unlines
      [ "WITH RECURSIVE project_tree(root_id,id) AS ("
      , " SELECT p.id,p.id FROM projects p WHERE p.workspace_id=$1 AND p.parent_id IS NOT DISTINCT FROM $2 AND p.deleted_at IS NULL"
      , " UNION SELECT tree.root_id,child.id FROM projects child JOIN project_tree tree ON child.parent_id=tree.id WHERE child.deleted_at IS NULL AND child.workspace_id=$1"
      , "), task_tree(root_id,id) AS ("
      , " SELECT tree.root_id,t.id FROM project_tree tree JOIN tasks t ON t.project_id=tree.id WHERE t.deleted_at IS NULL AND t.workspace_id=$1"
      , " UNION SELECT tree.root_id,child.id FROM tasks child JOIN task_tree tree ON child.parent_id=tree.id WHERE child.deleted_at IS NULL"
      , ") SELECT p.id,p.workspace_id,p.parent_id,p.name,p.description,p.status::text,p.priority,p.metadata,p.created_at,p.updated_at"
      , " FROM projects p WHERE p.id IN (SELECT DISTINCT root_id FROM project_tree)"
      -- A branch ancestor remains visible when a descendant matches.  In
      -- particular, do not apply p's own status/priority before the matching
      -- disjunction: that made an inactive-looking ancestor hide an otherwise
      -- visible descendant path.
      , " AND ((($8 <> 'tasks') AND (((cardinality($3::text[])=0 OR p.status::text=ANY($3)) AND ($5 IS NULL OR $5='any' OR ($5='exact' AND p.priority=$6) OR ($5='above' AND p.priority>$6) OR ($5='below' AND p.priority<$6)) AND ($7 IS NULL OR lower(p.name) LIKE '%' || replace(replace(replace(lower($7), E'\\\\', E'\\\\\\\\'), '%', E'\\\\%'), '_', E'\\\\_') || '%' ESCAPE E'\\\\' OR lower(coalesce(p.description,'')) LIKE '%' || replace(replace(replace(lower($7), E'\\\\', E'\\\\\\\\'), '%', E'\\\\%'), '_', E'\\\\_') || '%' ESCAPE E'\\\\'))"
      , " OR EXISTS (SELECT 1 FROM projects descendant WHERE descendant.id IN (SELECT id FROM project_tree WHERE root_id=p.id) AND descendant.deleted_at IS NULL AND (cardinality($3::text[])=0 OR descendant.status::text=ANY($3)) AND ($5 IS NULL OR $5='any' OR ($5='exact' AND descendant.priority=$6) OR ($5='above' AND descendant.priority>$6) OR ($5='below' AND descendant.priority<$6)) AND ($7 IS NULL OR lower(descendant.name) LIKE '%' || replace(replace(replace(lower($7), E'\\\\', E'\\\\\\\\'), '%', E'\\\\%'), '_', E'\\\\_') || '%' ESCAPE E'\\\\' OR lower(coalesce(descendant.description,'')) LIKE '%' || replace(replace(replace(lower($7), E'\\\\', E'\\\\\\\\'), '%', E'\\\\%'), '_', E'\\\\_') || '%' ESCAPE E'\\\\'))))"
      , " OR (($8 <> 'projects') AND EXISTS (SELECT 1 FROM tasks descendant WHERE descendant.id IN (SELECT id FROM task_tree WHERE root_id=p.id) AND descendant.deleted_at IS NULL AND (cardinality($4::text[])=0 OR descendant.status::text=ANY($4)) AND ($5 IS NULL OR $5='any' OR ($5='exact' AND descendant.priority=$6) OR ($5='above' AND descendant.priority>$6) OR ($5='below' AND descendant.priority<$6)) AND ($7 IS NULL OR lower(descendant.title) LIKE '%' || replace(replace(replace(lower($7), E'\\\\', E'\\\\\\\\'), '%', E'\\\\%'), '_', E'\\\\_') || '%' ESCAPE E'\\\\' OR lower(coalesce(descendant.description,'')) LIKE '%' || replace(replace(replace(lower($7), E'\\\\', E'\\\\\\\\'), '%', E'\\\\%'), '_', E'\\\\_') || '%' ESCAPE E'\\\\'))))"
      , " ORDER BY CASE p.status WHEN 'active'::project_status_enum THEN 0 WHEN 'paused'::project_status_enum THEN 1 WHEN 'completed'::project_status_enum THEN 2 ELSE 3 END, p.priority DESC, lower(p.name),p.id LIMIT $9 OFFSET $10"
      ]
    encoder =
      contramap (\(a,_,_,_,_,_,_,_,_,_) -> a) (Enc.param (Enc.nonNullable Enc.uuid)) <>
      contramap (\(_,b,_,_,_,_,_,_,_,_) -> b) (Enc.param (Enc.nullable Enc.uuid)) <>
      contramap (\(_,_,c,_,_,_,_,_,_,_) -> c) (Enc.param (Enc.nonNullable (Enc.foldableArray (Enc.nonNullable Enc.text)))) <>
      contramap (\(_,_,_,d,_,_,_,_,_,_) -> d) (Enc.param (Enc.nonNullable (Enc.foldableArray (Enc.nonNullable Enc.text)))) <>
      contramap (\(_,_,_,_,e,_,_,_,_,_) -> e) (Enc.param (Enc.nullable Enc.text)) <>
      contramap (\(_,_,_,_,_,f,_,_,_,_) -> fmap fromIntegral f) (Enc.param (Enc.nullable Enc.int2)) <>
      contramap (\(_,_,_,_,_,_,g,_,_,_) -> g) (Enc.param (Enc.nullable Enc.text)) <>
      contramap (\(_,_,_,_,_,_,_,h,_,_) -> h) (Enc.param (Enc.nonNullable Enc.text)) <>
      contramap (\(_,_,_,_,_,_,_,_,i,_) -> i) (Enc.param (Enc.nonNullable Enc.int4)) <>
      contramap (\(_,_,_,_,_,_,_,_,_,j) -> j) (Enc.param (Enc.nonNullable Enc.int4))
