module HMem.DB.Overview
  ( getTaskOverview
  , getProjectOverview
  ) where

import Data.ByteString.Char8 qualified as BS8
import Data.Functor.Contravariant ((>$<))
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
