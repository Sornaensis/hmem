module HMem.DB.Overview
  ( getTaskOverview
  , getProjectOverview
  , projectCardSummary
  , taskCardSummary
  , projectCardSummaries
  , taskCardSummaries
  , listTaskDependencyPage
  ) where

import Data.ByteString.Char8 qualified as BS8
import Data.Functor.Contravariant ((>$<), contramap)
import Data.Int (Int32)
import Data.Map.Strict qualified as Map
import Data.Pool (Pool)
import Data.UUID (UUID)
import Hasql.Connection qualified as Hasql
import Hasql.Decoders qualified as Dec
import Hasql.Encoders qualified as Enc
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement
import Rel8 hiding (filter, null)

import HMem.DB.Pool (runSession)
import HMem.DB.Project qualified as Proj
import HMem.DB.Schema
import HMem.DB.Task qualified as Task
import HMem.Types

getTaskOverview :: Pool Hasql.Connection -> UUID -> IO (Maybe TaskOverview)
getTaskOverview pool taskId = do
  mTask <- Task.getTask pool taskId
  case mTask of
    Nothing -> pure Nothing
    Just task -> do
      dependencies <- listTaskDependencySummaries pool taskId
      readinessRollup <- getTaskReadinessRollup pool taskId
      pure $ Just TaskOverview { task = task, dependencies = dependencies, readinessRollup = readinessRollup }

getProjectOverview :: Pool Hasql.Connection -> UUID -> IO (Maybe ProjectOverview)
getProjectOverview pool projectId = do
  mProject <- Proj.getProject pool projectId
  case mProject of
    Nothing -> pure Nothing
    Just project -> do
      tasks <- Task.listTasksWithQuery pool TaskListQuery
        { workspaceId = Nothing, projectId = Just projectId, status = Nothing
        , priority = Nothing, query = Nothing, searchLanguage = Nothing
        , createdAfter = Nothing, createdBefore = Nothing, updatedAfter = Nothing, updatedBefore = Nothing
        , limit = Just 200, offset = Just 0
        }
      allProjects <- Proj.listProjectsWithQuery pool ProjectListQuery
        { workspaceId = Just project.workspaceId, status = Nothing, query = Nothing, searchLanguage = Nothing
        , createdAfter = Nothing, createdBefore = Nothing, updatedAfter = Nothing, updatedBefore = Nothing
        , limit = Just 200, offset = Just 0
        }
      readinessRollup <- getProjectReadinessRollup pool projectId
      pure $ Just ProjectOverview
        { project = project
        , tasks = tasks
        , subprojects = filter (\child -> child.parentId == Just projectId) allProjects
        , readinessRollup = readinessRollup
        }

-- | Produce the card payload without serialising a Project's description or
-- metadata.  Counts are direct-only; the readiness rollup keeps the existing
-- recursive lifecycle semantics.
projectCardSummary :: Pool Hasql.Connection -> Project -> IO ProjectCardSummary
projectCardSummary pool value = head <$> projectCardSummaries pool [value]

taskCardSummary :: Pool Hasql.Connection -> Task -> IO TaskCardSummary
taskCardSummary pool value = head <$> taskCardSummaries pool [value]

-- | All card decoration for one response is read in four set-based queries.
-- Keeping the input order is important for the navigation and summary-batch
-- contracts, while avoiding a direct-count and recursive-rollup round trip for
-- every card in a page.
projectCardSummaries :: Pool Hasql.Connection -> [Project] -> IO [ProjectCardSummary]
projectCardSummaries _ [] = pure []
projectCardSummaries pool values = do
  let ids = map (.id) values
  counts <- Map.fromList <$> runSession pool (Session.statement ids directProjectCountsBatchStatement)
  rollups <- Map.fromList <$> runSession pool (Session.statement ids projectReadinessRollupsBatchStatement)
  pure
    [ let (childProjects, childTasks) = Map.findWithDefault (0, 0) value.id counts
          rollup = Map.findWithDefault emptyProjectReadinessRollup value.id rollups
      in ProjectCardSummary
          { id = value.id, workspaceId = value.workspaceId, parentId = value.parentId, name = value.name
          , status = value.status, priority = value.priority, createdAt = value.createdAt, updatedAt = value.updatedAt
          , directProjectCount = childProjects, directTaskCount = childTasks, hasChildren = childProjects + childTasks > 0
          , readinessRollup = rollup }
    | value <- values
    ]

taskCardSummaries :: Pool Hasql.Connection -> [Task] -> IO [TaskCardSummary]
taskCardSummaries _ [] = pure []
taskCardSummaries pool values = do
  let ids = map (.id) values
  counts <- Map.fromList <$> runSession pool (Session.statement ids directSubtaskCountsBatchStatement)
  rollups <- Map.fromList <$> runSession pool (Session.statement ids taskReadinessRollupsBatchStatement)
  pure
    [ let childTasks = Map.findWithDefault 0 value.id counts
          rollup = Map.findWithDefault emptyTaskReadinessRollup value.id rollups
      in TaskCardSummary
          { id = value.id, workspaceId = value.workspaceId, projectId = value.projectId, parentId = value.parentId
          , title = value.title, status = value.status, priority = value.priority, dueAt = value.dueAt, completedAt = value.completedAt
          , dependencyCount = value.dependencyCount, createdAt = value.createdAt, updatedAt = value.updatedAt
          , directSubtaskCount = childTasks, hasChildren = childTasks > 0, readinessRollup = rollup }
    | value <- values
    ]

emptyProjectReadinessRollup :: ProjectReadinessRollup
emptyProjectReadinessRollup = ProjectReadinessRollup 0 0 0 0 0 0 0 0 True

emptyTaskReadinessRollup :: TaskReadinessRollup
emptyTaskReadinessRollup = TaskReadinessRollup 0 0 0 0 0 0 True

listTaskDependencyPage :: Pool Hasql.Connection -> UUID -> Int -> Int -> IO TaskDependencyPage
listTaskDependencyPage pool taskId requestedLimit requestedOffset = do
  let takeN = Prelude.min maxNavigationPageSize (Prelude.max 1 requestedLimit)
      skipN = Prelude.min maxNavigationOffset (Prelude.max 0 requestedOffset)
  rows <- runSession pool $ Session.statement (taskId, fromIntegral (takeN + 1) :: Int32, fromIntegral skipN :: Int32) taskDependencyPageStatement
  pure TaskDependencyPage { items = take takeN rows, hasMore = length rows > takeN }

directProjectCountsStatement :: Statement.Statement UUID (Int, Int)
directProjectCountsStatement = Statement.Statement
  "SELECT (SELECT count(*)::bigint FROM projects p WHERE p.parent_id=$1 AND p.deleted_at IS NULL), (SELECT count(*)::bigint FROM tasks t WHERE t.project_id=$1 AND t.parent_id IS NULL AND t.deleted_at IS NULL)"
  (Enc.param (Enc.nonNullable Enc.uuid))
  (Dec.singleRow ((,) <$> (fromIntegral <$> Dec.column (Dec.nonNullable Dec.int8)) <*> (fromIntegral <$> Dec.column (Dec.nonNullable Dec.int8)))) True

directProjectCountsBatchStatement :: Statement.Statement [UUID] [(UUID, (Int, Int))]
directProjectCountsBatchStatement = Statement.Statement
  "SELECT root_id, (SELECT count(*)::bigint FROM projects p WHERE p.parent_id=root_id AND p.deleted_at IS NULL), (SELECT count(*)::bigint FROM tasks t WHERE t.project_id=root_id AND t.parent_id IS NULL AND t.deleted_at IS NULL) FROM unnest($1::uuid[]) AS roots(root_id)"
  (Enc.param (Enc.nonNullable (Enc.foldableArray (Enc.nonNullable Enc.uuid))))
  (Dec.rowList ((,) <$> Dec.column (Dec.nonNullable Dec.uuid) <*> ((,) <$> (fromIntegral <$> Dec.column (Dec.nonNullable Dec.int8)) <*> (fromIntegral <$> Dec.column (Dec.nonNullable Dec.int8))))) True

directSubtaskCountStatement :: Statement.Statement UUID Int
directSubtaskCountStatement = Statement.Statement
  "SELECT count(*)::bigint FROM tasks WHERE parent_id=$1 AND deleted_at IS NULL"
  (Enc.param (Enc.nonNullable Enc.uuid))
  (Dec.singleRow (fromIntegral <$> Dec.column (Dec.nonNullable Dec.int8))) True

directSubtaskCountsBatchStatement :: Statement.Statement [UUID] [(UUID, Int)]
directSubtaskCountsBatchStatement = Statement.Statement
  "SELECT root_id, (SELECT count(*)::bigint FROM tasks t WHERE t.parent_id=root_id AND t.deleted_at IS NULL) FROM unnest($1::uuid[]) AS roots(root_id)"
  (Enc.param (Enc.nonNullable (Enc.foldableArray (Enc.nonNullable Enc.uuid))))
  (Dec.rowList ((,) <$> Dec.column (Dec.nonNullable Dec.uuid) <*> (fromIntegral <$> Dec.column (Dec.nonNullable Dec.int8)))) True

taskDependencyPageStatement :: Statement.Statement (UUID, Int32, Int32) [TaskDependencySummary]
taskDependencyPageStatement = Statement.Statement
  "SELECT t.id,t.title FROM task_dependencies d JOIN tasks t ON t.id=d.depends_on_id WHERE d.task_id=$1 AND t.deleted_at IS NULL ORDER BY lower(t.title),t.id LIMIT $2 OFFSET $3"
  ( contramap (\(a, _, _) -> a) (Enc.param (Enc.nonNullable Enc.uuid))
 <> contramap (\(_, b, _) -> b) (Enc.param (Enc.nonNullable Enc.int4))
 <> contramap (\(_, _, c) -> c) (Enc.param (Enc.nonNullable Enc.int4)) )
  (Dec.rowList (TaskDependencySummary <$> Dec.column (Dec.nonNullable Dec.uuid) <*> Dec.column (Dec.nonNullable Dec.text))) True

getTaskReadinessRollup :: Pool Hasql.Connection -> UUID -> IO TaskReadinessRollup
getTaskReadinessRollup pool taskId = runSession pool $ Session.statement taskId taskReadinessRollupStatement

taskReadinessRollupStatement :: Statement.Statement UUID TaskReadinessRollup
taskReadinessRollupStatement = Statement.Statement sql encoder decoder True
  where
    sql = BS8.pack $ unlines
      [ "WITH RECURSIVE task_tree(id, parent_id, status) AS ("
      , "  SELECT id, parent_id, status FROM tasks WHERE id = $1 AND deleted_at IS NULL"
      , "  UNION SELECT child.id, child.parent_id, child.status FROM tasks child JOIN task_tree parent ON child.parent_id = parent.id WHERE child.deleted_at IS NULL"
      , "), desc_tasks AS (SELECT * FROM task_tree WHERE id <> $1),"
      , "open_dependency_edges AS ("
      , " SELECT DISTINCT tree.id AS task_id, dep.id AS dep_id FROM task_tree tree"
      , " JOIN task_dependencies dep_link ON dep_link.task_id = tree.id JOIN tasks dep ON dep.id = dep_link.depends_on_id"
      , " WHERE dep.deleted_at IS NULL AND hmem_is_open_task_status(tree.status) AND hmem_is_open_task_status(dep.status))"
      , "SELECT (SELECT count(*)::bigint FROM desc_tasks WHERE hmem_is_open_task_status(status)),"
      , " (SELECT count(*)::bigint FROM desc_tasks WHERE status = 'done'::task_status_enum),"
      , " (SELECT count(*)::bigint FROM desc_tasks WHERE status = 'cancelled'::task_status_enum),"
      , " (SELECT count(*)::bigint FROM desc_tasks WHERE status = 'blocked'::task_status_enum),"
      , " (SELECT count(DISTINCT task_id)::bigint FROM open_dependency_edges),"
      , " (SELECT count(*)::bigint FROM open_dependency_edges),"
      , " NOT EXISTS (SELECT 1 FROM desc_tasks WHERE hmem_is_open_task_status(status))"
      ]
    encoder = Enc.param (Enc.nonNullable Enc.uuid)
    decoder = taskReadinessRollupRowDecoder

taskReadinessRollupRowDecoder :: Dec.Result TaskReadinessRollup
taskReadinessRollupRowDecoder = Dec.singleRow $ TaskReadinessRollup
  <$> (fromIntegral <$> Dec.column (Dec.nonNullable Dec.int8))
  <*> (fromIntegral <$> Dec.column (Dec.nonNullable Dec.int8))
  <*> (fromIntegral <$> Dec.column (Dec.nonNullable Dec.int8))
  <*> (fromIntegral <$> Dec.column (Dec.nonNullable Dec.int8))
  <*> (fromIntegral <$> Dec.column (Dec.nonNullable Dec.int8))
  <*> (fromIntegral <$> Dec.column (Dec.nonNullable Dec.int8))
  <*> Dec.column (Dec.nonNullable Dec.bool)

-- The recursive CTE is the single-task lifecycle definition above with a
-- root_id carried through each row.  That keeps card rollups bit-for-bit
-- aligned with the legacy overview while evaluating a whole response at once.
taskReadinessRollupsBatchStatement :: Statement.Statement [UUID] [(UUID, TaskReadinessRollup)]
taskReadinessRollupsBatchStatement = Statement.Statement sql encoder decoder True
  where
    sql = BS8.pack $ unlines
      [ "WITH RECURSIVE roots(root_id) AS (SELECT unnest($1::uuid[])),"
      , "task_tree(root_id,id,parent_id,status) AS ("
      , " SELECT roots.root_id,t.id,t.parent_id,t.status FROM roots JOIN tasks t ON t.id=roots.root_id WHERE t.deleted_at IS NULL"
      , " UNION SELECT tree.root_id,child.id,child.parent_id,child.status FROM tasks child JOIN task_tree tree ON child.parent_id=tree.id WHERE child.deleted_at IS NULL"
      , "), desc_tasks AS (SELECT * FROM task_tree WHERE id <> root_id),"
      , "open_dependency_edges AS ("
      , " SELECT DISTINCT tree.root_id,tree.id AS task_id,dep.id AS dep_id FROM task_tree tree JOIN task_dependencies link ON link.task_id=tree.id JOIN tasks dep ON dep.id=link.depends_on_id WHERE dep.deleted_at IS NULL AND hmem_is_open_task_status(tree.status) AND hmem_is_open_task_status(dep.status))"
      , "SELECT roots.root_id,"
      , " (SELECT count(*)::bigint FROM desc_tasks t WHERE t.root_id=roots.root_id AND hmem_is_open_task_status(t.status)),"
      , " (SELECT count(*)::bigint FROM desc_tasks t WHERE t.root_id=roots.root_id AND t.status='done'::task_status_enum),"
      , " (SELECT count(*)::bigint FROM desc_tasks t WHERE t.root_id=roots.root_id AND t.status='cancelled'::task_status_enum),"
      , " (SELECT count(*)::bigint FROM desc_tasks t WHERE t.root_id=roots.root_id AND t.status='blocked'::task_status_enum),"
      , " (SELECT count(DISTINCT task_id)::bigint FROM open_dependency_edges e WHERE e.root_id=roots.root_id),"
      , " (SELECT count(*)::bigint FROM open_dependency_edges e WHERE e.root_id=roots.root_id),"
      , " NOT EXISTS (SELECT 1 FROM desc_tasks t WHERE t.root_id=roots.root_id AND hmem_is_open_task_status(t.status))"
      , "FROM roots"
      ]
    encoder = Enc.param (Enc.nonNullable (Enc.foldableArray (Enc.nonNullable Enc.uuid)))
    decoder = Dec.rowList $ (,) <$> Dec.column (Dec.nonNullable Dec.uuid) <*> taskReadinessRollupFields

taskReadinessRollupFields :: Dec.Row TaskReadinessRollup
taskReadinessRollupFields = TaskReadinessRollup
  <$> (fromIntegral <$> Dec.column (Dec.nonNullable Dec.int8))
  <*> (fromIntegral <$> Dec.column (Dec.nonNullable Dec.int8))
  <*> (fromIntegral <$> Dec.column (Dec.nonNullable Dec.int8))
  <*> (fromIntegral <$> Dec.column (Dec.nonNullable Dec.int8))
  <*> (fromIntegral <$> Dec.column (Dec.nonNullable Dec.int8))
  <*> (fromIntegral <$> Dec.column (Dec.nonNullable Dec.int8))
  <*> Dec.column (Dec.nonNullable Dec.bool)

getProjectReadinessRollup :: Pool Hasql.Connection -> UUID -> IO ProjectReadinessRollup
getProjectReadinessRollup pool projectId = runSession pool $ Session.statement projectId projectReadinessRollupStatement

projectReadinessRollupStatement :: Statement.Statement UUID ProjectReadinessRollup
projectReadinessRollupStatement = Statement.Statement sql encoder decoder True
  where
    sql = BS8.pack $ unlines
      [ "WITH RECURSIVE project_tree(id, parent_id, status, workspace_id) AS ("
      , " SELECT id, parent_id, status, workspace_id FROM projects WHERE id = $1 AND deleted_at IS NULL"
      , " UNION SELECT child.id, child.parent_id, child.status, child.workspace_id FROM projects child JOIN project_tree parent ON child.parent_id = parent.id WHERE child.deleted_at IS NULL AND child.workspace_id = parent.workspace_id"
      , "), desc_projects AS (SELECT * FROM project_tree WHERE id <> $1),"
      , "task_tree(id, parent_id, status) AS ("
      , " SELECT t.id, t.parent_id, t.status FROM tasks t JOIN project_tree project ON t.project_id = project.id WHERE t.deleted_at IS NULL AND t.workspace_id = project.workspace_id"
      , " UNION SELECT child.id, child.parent_id, child.status FROM tasks child JOIN task_tree parent ON child.parent_id = parent.id WHERE child.deleted_at IS NULL"
      , "), open_dependency_edges AS ("
      , " SELECT DISTINCT tree.id AS task_id, dep.id AS dep_id FROM task_tree tree JOIN task_dependencies dep_link ON dep_link.task_id = tree.id JOIN tasks dep ON dep.id = dep_link.depends_on_id WHERE dep.deleted_at IS NULL AND hmem_is_open_task_status(tree.status) AND hmem_is_open_task_status(dep.status))"
      , "SELECT (SELECT count(*)::bigint FROM desc_projects WHERE status IN ('active'::project_status_enum, 'paused'::project_status_enum)),"
      , " (SELECT count(*)::bigint FROM desc_projects WHERE status IN ('completed'::project_status_enum, 'archived'::project_status_enum)),"
      , " (SELECT count(*)::bigint FROM task_tree WHERE hmem_is_open_task_status(status)),"
      , " (SELECT count(*)::bigint FROM task_tree WHERE status = 'done'::task_status_enum),"
      , " (SELECT count(*)::bigint FROM task_tree WHERE status = 'cancelled'::task_status_enum),"
      , " (SELECT count(*)::bigint FROM task_tree WHERE status = 'blocked'::task_status_enum),"
      , " (SELECT count(DISTINCT task_id)::bigint FROM open_dependency_edges),"
      , " (SELECT count(*)::bigint FROM open_dependency_edges),"
      , " NOT EXISTS (SELECT 1 FROM desc_projects WHERE status IN ('active'::project_status_enum, 'paused'::project_status_enum)) AND NOT EXISTS (SELECT 1 FROM task_tree WHERE hmem_is_open_task_status(status))"
      ]
    encoder = Enc.param (Enc.nonNullable Enc.uuid)
    decoder = projectReadinessRollupRowDecoder

projectReadinessRollupRowDecoder :: Dec.Result ProjectReadinessRollup
projectReadinessRollupRowDecoder = Dec.singleRow $ ProjectReadinessRollup
  <$> (fromIntegral <$> Dec.column (Dec.nonNullable Dec.int8))
  <*> (fromIntegral <$> Dec.column (Dec.nonNullable Dec.int8))
  <*> (fromIntegral <$> Dec.column (Dec.nonNullable Dec.int8))
  <*> (fromIntegral <$> Dec.column (Dec.nonNullable Dec.int8))
  <*> (fromIntegral <$> Dec.column (Dec.nonNullable Dec.int8))
  <*> (fromIntegral <$> Dec.column (Dec.nonNullable Dec.int8))
  <*> (fromIntegral <$> Dec.column (Dec.nonNullable Dec.int8))
  <*> (fromIntegral <$> Dec.column (Dec.nonNullable Dec.int8))
  <*> Dec.column (Dec.nonNullable Dec.bool)

projectReadinessRollupsBatchStatement :: Statement.Statement [UUID] [(UUID, ProjectReadinessRollup)]
projectReadinessRollupsBatchStatement = Statement.Statement sql encoder decoder True
  where
    sql = BS8.pack $ unlines
      [ "WITH RECURSIVE roots(root_id) AS (SELECT unnest($1::uuid[])),"
      , "project_tree(root_id,id,parent_id,status,workspace_id) AS ("
      , " SELECT roots.root_id,p.id,p.parent_id,p.status,p.workspace_id FROM roots JOIN projects p ON p.id=roots.root_id WHERE p.deleted_at IS NULL"
      , " UNION SELECT tree.root_id,child.id,child.parent_id,child.status,child.workspace_id FROM projects child JOIN project_tree tree ON child.parent_id=tree.id WHERE child.deleted_at IS NULL AND child.workspace_id=tree.workspace_id"
      , "), desc_projects AS (SELECT * FROM project_tree WHERE id <> root_id),"
      , "task_tree(root_id,id,parent_id,status) AS ("
      , " SELECT tree.root_id,t.id,t.parent_id,t.status FROM project_tree tree JOIN tasks t ON t.project_id=tree.id WHERE t.deleted_at IS NULL AND t.workspace_id=tree.workspace_id"
      , " UNION SELECT tree.root_id,child.id,child.parent_id,child.status FROM tasks child JOIN task_tree tree ON child.parent_id=tree.id WHERE child.deleted_at IS NULL"
      , "), open_dependency_edges AS ("
      , " SELECT DISTINCT tree.root_id,tree.id AS task_id,dep.id AS dep_id FROM task_tree tree JOIN task_dependencies link ON link.task_id=tree.id JOIN tasks dep ON dep.id=link.depends_on_id WHERE dep.deleted_at IS NULL AND hmem_is_open_task_status(tree.status) AND hmem_is_open_task_status(dep.status))"
      , "SELECT roots.root_id,"
      , " (SELECT count(*)::bigint FROM desc_projects p WHERE p.root_id=roots.root_id AND p.status IN ('active'::project_status_enum,'paused'::project_status_enum)),"
      , " (SELECT count(*)::bigint FROM desc_projects p WHERE p.root_id=roots.root_id AND p.status IN ('completed'::project_status_enum,'archived'::project_status_enum)),"
      , " (SELECT count(*)::bigint FROM task_tree t WHERE t.root_id=roots.root_id AND hmem_is_open_task_status(t.status)),"
      , " (SELECT count(*)::bigint FROM task_tree t WHERE t.root_id=roots.root_id AND t.status='done'::task_status_enum),"
      , " (SELECT count(*)::bigint FROM task_tree t WHERE t.root_id=roots.root_id AND t.status='cancelled'::task_status_enum),"
      , " (SELECT count(*)::bigint FROM task_tree t WHERE t.root_id=roots.root_id AND t.status='blocked'::task_status_enum),"
      , " (SELECT count(DISTINCT task_id)::bigint FROM open_dependency_edges e WHERE e.root_id=roots.root_id),"
      , " (SELECT count(*)::bigint FROM open_dependency_edges e WHERE e.root_id=roots.root_id),"
      , " NOT EXISTS (SELECT 1 FROM desc_projects p WHERE p.root_id=roots.root_id AND p.status IN ('active'::project_status_enum,'paused'::project_status_enum)) AND NOT EXISTS (SELECT 1 FROM task_tree t WHERE t.root_id=roots.root_id AND hmem_is_open_task_status(t.status))"
      , "FROM roots"
      ]
    encoder = Enc.param (Enc.nonNullable (Enc.foldableArray (Enc.nonNullable Enc.uuid)))
    decoder = Dec.rowList $ (,) <$> Dec.column (Dec.nonNullable Dec.uuid) <*> projectReadinessRollupFields

projectReadinessRollupFields :: Dec.Row ProjectReadinessRollup
projectReadinessRollupFields = ProjectReadinessRollup
  <$> (fromIntegral <$> Dec.column (Dec.nonNullable Dec.int8))
  <*> (fromIntegral <$> Dec.column (Dec.nonNullable Dec.int8))
  <*> (fromIntegral <$> Dec.column (Dec.nonNullable Dec.int8))
  <*> (fromIntegral <$> Dec.column (Dec.nonNullable Dec.int8))
  <*> (fromIntegral <$> Dec.column (Dec.nonNullable Dec.int8))
  <*> (fromIntegral <$> Dec.column (Dec.nonNullable Dec.int8))
  <*> (fromIntegral <$> Dec.column (Dec.nonNullable Dec.int8))
  <*> (fromIntegral <$> Dec.column (Dec.nonNullable Dec.int8))
  <*> Dec.column (Dec.nonNullable Dec.bool)

listTaskDependencySummaries :: Pool Hasql.Connection -> UUID -> IO [TaskDependencySummary]
listTaskDependencySummaries pool taskId = do
  rows <- runSession pool $ Session.statement () $ run $ select $ orderBy ((\(_, name) -> name) >$< asc) $ do
    dependency <- each taskDependencySchema
    where_ $ dependency.tdTaskId ==. lit taskId
    task <- each taskSchema
    where_ $ task.taskId ==. dependency.tdDependsOnId
    where_ $ activeTask task
    pure (task.taskId, task.taskTitle)
  pure [TaskDependencySummary { id = dependencyId, name = dependencyName } | (dependencyId, dependencyName) <- rows]
