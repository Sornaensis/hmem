module HMem.DB.Task
  ( createTask
  , getTask
  , updateTask
  , updateTaskWithDependencySnapshots
  , updateTaskBatch
  , deleteTask
  , deleteTaskCascade
  , deleteTaskBatch
  , restoreTask
  , purgeTaskCascade
  , moveTasksBatch
  , listTasks
  , listTasksWithQuery
  , listTasksByWorkspace
  , listNextTasks
  , enrichTaskCounts
  , dependencyAutoBlockSnapshots
  , enrichDependencyAutoBlockSnapshots
  , addDependencyWithSnapshots
  , removeDependencyWithSnapshots
  , addDependency
  , removeDependency
  ) where

import Control.Exception (throwIO)
import Control.Monad (void, when)
import Data.Aeson (Object, object, (.=), toJSON)
import Data.Aeson qualified as Aeson
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as BL
import Data.Functor.Contravariant ((>$<), contramap)
import Data.Int (Int16, Int32, Int64)
import Data.List (intercalate, nub)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Pool (Pool)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time (UTCTime)
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Hasql.Connection qualified as Hasql
import Hasql.Decoders qualified as Dec
import Hasql.Encoders qualified as Enc
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement
import Rel8

import HMem.DB.Pool (runSession, runTransaction, DBException(..))
import HMem.DB.Project qualified as Proj
import HMem.DB.Schema
import HMem.Types

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

rowToTask :: TaskT Result -> Task
rowToTask r = Task
  { id              = r.taskId
  , workspaceId     = r.taskWorkspaceId
  , projectId       = r.taskProjectId
  , parentId        = r.taskParentId
  , title           = r.taskTitle
  , description     = r.taskDescription
  , status          = r.taskStatus
  , priority        = fromIntegral r.taskPriority
  , metadata        = r.taskMetadata
  , dueAt           = r.taskDueAt
  , completedAt     = r.taskCompletedAt
  , dependencyCount = 0
  , createdAt       = r.taskCreatedAt
  , updatedAt       = r.taskUpdatedAt
  }

rawTaskRowDecoder :: Dec.Row Task
rawTaskRowDecoder = do
  taskId <- Dec.column (Dec.nonNullable Dec.uuid)
  taskWorkspaceId <- Dec.column (Dec.nonNullable Dec.uuid)
  taskProjectId <- Dec.column (Dec.nullable Dec.uuid)
  taskParentId <- Dec.column (Dec.nullable Dec.uuid)
  taskTitle <- Dec.column (Dec.nonNullable Dec.text)
  taskDescription <- Dec.column (Dec.nullable Dec.text)
  taskStatusText <- Dec.column (Dec.nonNullable Dec.text)
  taskStatus <- case taskStatusFromText taskStatusText of
    Just parsed -> pure parsed
    Nothing -> fail $ "Unexpected task_status_enum value: " <> show taskStatusText
  taskPriority <- Dec.column (Dec.nonNullable Dec.int2)
  taskMetadata <- Dec.column (Dec.nonNullable Dec.jsonb)
  taskDueAt <- Dec.column (Dec.nullable Dec.timestamptz)
  taskCompletedAt <- Dec.column (Dec.nullable Dec.timestamptz)
  taskCreatedAt <- Dec.column (Dec.nonNullable Dec.timestamptz)
  taskUpdatedAt <- Dec.column (Dec.nonNullable Dec.timestamptz)
  pure Task
    { id = taskId
    , workspaceId = taskWorkspaceId
    , projectId = taskProjectId
    , parentId = taskParentId
    , title = taskTitle
    , description = taskDescription
    , status = taskStatus
    , priority = fromIntegral taskPriority
    , metadata = taskMetadata
    , dueAt = taskDueAt
    , completedAt = taskCompletedAt
    , dependencyCount = 0
      , createdAt = taskCreatedAt
    , updatedAt = taskUpdatedAt
    }

-- | Enrich a list of tasks with dependency counts.
enrichTaskCounts :: Pool Hasql.Connection -> [Task] -> IO [Task]
enrichTaskCounts _ [] = pure []
enrichTaskCounts pool tasks = do
  let taskIds = map (.id) tasks
  depCounts <- getCountMap pool depCountStatement taskIds
  pure $ map (setDependencyCount depCounts) tasks

setDependencyCount :: Map.Map UUID Int -> Task -> Task
setDependencyCount counts task = task { dependencyCount = Map.findWithDefault 0 task.id counts }

getCountMap :: Pool Hasql.Connection -> Statement.Statement [UUID] [(UUID, Int64)] -> [UUID] -> IO (Map.Map UUID Int)
getCountMap pool stmt uuids = do
  rows <- runSession pool $ Session.statement uuids stmt
  pure $ Map.fromList [ (uid, fromIntegral cnt) | (uid, cnt) <- rows ]

depCountStatement :: Statement.Statement [UUID] [(UUID, Int64)]
depCountStatement = Statement.Statement sql encoder decoder True
  where
    sql = BS8.pack $ unlines
      [ "SELECT task_id, COUNT(*)"
      , "FROM task_dependencies"
      , "WHERE task_id = ANY($1)"
      , "GROUP BY task_id"
      ]
    encoder = Enc.param (Enc.nonNullable (Enc.foldableArray (Enc.nonNullable Enc.uuid)))
    decoder = Dec.rowList $
      (,) <$> Dec.column (Dec.nonNullable Dec.uuid)
          <*> Dec.column (Dec.nonNullable Dec.int8)


taskSubtreeIdsStatement :: Statement.Statement UUID [UUID]
taskSubtreeIdsStatement = Statement.Statement sql encoder decoder True
  where
    sql = BS8.pack $ unlines
      [ "WITH RECURSIVE task_tree AS ("
      , "  SELECT id"
      , "  FROM tasks"
      , "  WHERE id = $1 AND deleted_at IS NULL"
      , "  UNION ALL"
      , "  SELECT t.id"
      , "  FROM tasks t"
      , "  JOIN task_tree tt ON t.parent_id = tt.id"
      , "  WHERE t.deleted_at IS NULL"
      , ")"
      , "SELECT id FROM task_tree"
      ]
    encoder = Enc.param (Enc.nonNullable Enc.uuid)
    decoder = Dec.rowList (Dec.column (Dec.nonNullable Dec.uuid))

taskSubtreeIdsForUpdateStatement :: Statement.Statement UUID [UUID]
taskSubtreeIdsForUpdateStatement = Statement.Statement sql encoder decoder True
  where
    sql = BS8.pack $ unlines
      [ "WITH RECURSIVE task_tree AS ("
      , "  SELECT id"
      , "  FROM tasks"
      , "  WHERE id = $1 AND deleted_at IS NULL"
      , "  UNION ALL"
      , "  SELECT child.id"
      , "  FROM tasks child"
      , "  JOIN task_tree parent_tree ON child.parent_id = parent_tree.id"
      , "  WHERE child.deleted_at IS NULL"
      , ")"
      , "SELECT task_to_lock.id"
      , "  FROM tasks task_to_lock"
      , "  JOIN task_tree ON task_tree.id = task_to_lock.id"
      , " WHERE task_to_lock.deleted_at IS NULL"
      , " ORDER BY task_to_lock.id"
      , " FOR UPDATE OF task_to_lock"
      ]
    encoder = Enc.param (Enc.nonNullable Enc.uuid)
    decoder = Dec.rowList (Dec.column (Dec.nonNullable Dec.uuid))

taskSubtreeIdsForRootsStatement :: Statement.Statement [UUID] [UUID]
taskSubtreeIdsForRootsStatement = Statement.Statement sql encoder decoder True
  where
    sql = BS8.pack $ unlines
      [ "WITH RECURSIVE task_tree AS ("
      , "  SELECT id"
      , "  FROM tasks"
      , "  WHERE id = ANY($1) AND deleted_at IS NULL"
      , "  UNION"
      , "  SELECT t.id"
      , "  FROM tasks t"
      , "  JOIN task_tree tt ON t.parent_id = tt.id"
      , "  WHERE t.deleted_at IS NULL"
      , ")"
      , "SELECT id FROM task_tree"
      ]
    encoder = Enc.param (Enc.nonNullable (Enc.foldableArray (Enc.nonNullable Enc.uuid)))
    decoder = Dec.rowList (Dec.column (Dec.nonNullable Dec.uuid))

taskSubtreeIdsForRootsForUpdateStatement :: Statement.Statement [UUID] [UUID]
taskSubtreeIdsForRootsForUpdateStatement = Statement.Statement sql encoder decoder True
  where
    sql = BS8.pack $ unlines
      [ "WITH RECURSIVE task_tree AS ("
      , "  SELECT id"
      , "  FROM tasks"
      , "  WHERE id = ANY($1) AND deleted_at IS NULL"
      , "  UNION"
      , "  SELECT child.id"
      , "  FROM tasks child"
      , "  JOIN task_tree parent_tree ON child.parent_id = parent_tree.id"
      , "  WHERE child.deleted_at IS NULL"
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

deletedTaskSubtreeIdsStatement :: Statement.Statement (UUID, UTCTime) [UUID]
deletedTaskSubtreeIdsStatement = Statement.Statement sql encoder decoder True
  where
    sql = BS8.pack $ unlines
      [ "WITH RECURSIVE task_tree AS ("
      , "  SELECT id"
      , "  FROM tasks"
      , "  WHERE id = $1 AND deleted_at = $2"
      , "  UNION ALL"
      , "  SELECT t.id"
      , "  FROM tasks t"
      , "  JOIN task_tree tt ON t.parent_id = tt.id"
      , "  WHERE t.deleted_at = $2"
      , ")"
      , "SELECT id FROM task_tree"
      ]
    encoder =
      contramap fst (Enc.param (Enc.nonNullable Enc.uuid)) <>
      contramap snd (Enc.param (Enc.nonNullable Enc.timestamptz))
    decoder = Dec.rowList (Dec.column (Dec.nonNullable Dec.uuid))

allTaskSubtreeIdsStatement :: Statement.Statement UUID [UUID]
allTaskSubtreeIdsStatement = Statement.Statement sql encoder decoder True
  where
    sql = BS8.pack $ unlines
      [ "WITH RECURSIVE task_tree AS ("
      , "  SELECT id"
      , "  FROM tasks"
      , "  WHERE id = $1"
      , "  UNION"
      , "  SELECT t.id"
      , "  FROM tasks t"
      , "  JOIN task_tree tt ON t.parent_id = tt.id"
      , ")"
      , "SELECT id FROM task_tree"
      ]
    encoder = Enc.param (Enc.nonNullable Enc.uuid)
    decoder = Dec.rowList (Dec.column (Dec.nonNullable Dec.uuid))

softDeleteTasksStatement :: Statement.Statement [UUID] Int
softDeleteTasksStatement = Statement.Statement sql encoder decoder True
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

deleteTaskDependenciesStatement :: Statement.Statement [UUID] Int
deleteTaskDependenciesStatement = Statement.Statement sql encoder decoder True
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

deleteTaskDependenciesS :: [UUID] -> Session.Session Int
deleteTaskDependenciesS [] = pure 0
deleteTaskDependenciesS ids = Session.statement ids deleteTaskDependenciesStatement

purgeTasksStatement :: Statement.Statement [UUID] Int
purgeTasksStatement = Statement.Statement sql encoder decoder True
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

applyFieldUpdateMaybe :: Maybe a -> FieldUpdate a -> Maybe a
applyFieldUpdateMaybe oldValue = \case
  Unchanged -> oldValue
  SetNull -> Nothing
  SetTo value -> Just value

ensureTaskProject :: Pool Hasql.Connection -> UUID -> Maybe UUID -> IO ()
ensureTaskProject _ _ Nothing = pure ()
ensureTaskProject pool workspaceId (Just projectId) = do
  foundProject <- Proj.getProject pool projectId >>= maybe
    (throwIO $ DBForeignKeyViolation "Referenced project does not exist")
    pure
  when (foundProject.workspaceId /= workspaceId) $
    throwIO $ DBCheckViolation "Task project must belong to the same workspace"

ensureTaskParent :: Pool Hasql.Connection -> UUID -> Maybe UUID -> IO (Maybe Task)
ensureTaskParent _ _ Nothing = pure Nothing
ensureTaskParent pool workspaceId (Just parentId) = do
  parent <- getTask pool parentId >>= maybe
    (throwIO $ DBForeignKeyViolation "Referenced parent task does not exist")
    pure
  when (parent.workspaceId /= workspaceId) $
    throwIO $ DBCheckViolation "Parent task must belong to the same workspace"
  pure (Just parent)

ensureTaskPlacement :: Pool Hasql.Connection -> UUID -> Maybe UUID -> Maybe UUID -> IO (Maybe Task)
ensureTaskPlacement pool workspaceId projectId parentId = do
  ensureTaskProject pool workspaceId projectId
  mParent <- ensureTaskParent pool workspaceId parentId
  case mParent of
    Nothing -> pure Nothing
    Just parent -> do
      when (parent.projectId /= projectId) $
        throwIO $ DBCheckViolation "Task and parent task must belong to the same project"
      when (parent.parentId /= Nothing) $
        throwIO $ lifecycleViolation
          "TASK_SUBTASK_DEPTH_EXCEEDED"
          "Cannot create or move a task under a subtask."
          (Just $ blockerDetail parent.id)
          (Just "Move the target parent to the top level before adding subtasks, or attach this task to a top-level task.")
      pure (Just parent)

ensureTaskCanBecomeSubtask :: Pool Hasql.Connection -> UUID -> Maybe UUID -> IO ()
ensureTaskCanBecomeSubtask _ _ Nothing = pure ()
ensureTaskCanBecomeSubtask pool taskId (Just _) = do
  mFirstChild <- taskFirstActiveChild pool taskId
  case mFirstChild of
    Nothing -> pure ()
    Just childId ->
      throwIO $ lifecycleViolation
        "TASK_SUBTASK_DEPTH_EXCEEDED"
        "Cannot move a task with subtasks under another task."
        (Just $ blockerDetail childId)
        (Just "Move, delete, or detach existing subtasks before making this task a subtask.")

ensureSubtaskStartAllowed :: Bool -> Maybe Task -> TaskStatus -> IO ()
ensureSubtaskStartAllowed shouldEnforce mParent targetStatus =
  case (shouldEnforce, mParent, targetStatus) of
    (True, Just parent, InProgress) | parent.status /= InProgress ->
      throwIO $ lifecycleViolation
        "TASK_SUBTASK_START_BLOCKED"
        "Cannot start a subtask while its parent task is not in progress."
        (Just $ blockerDetail parent.id)
        (Just "Start the parent task before moving a subtask to in_progress.")
    _ -> pure ()

taskFirstActiveChild :: Pool Hasql.Connection -> UUID -> IO (Maybe UUID)
taskFirstActiveChild pool taskId =
  runSession pool $ Session.statement taskId taskFirstActiveChildStatement

taskFirstActiveChildStatement :: Statement.Statement UUID (Maybe UUID)
taskFirstActiveChildStatement = Statement.Statement sql encoder decoder True
  where
    sql = "SELECT id FROM tasks WHERE parent_id = $1 AND deleted_at IS NULL ORDER BY id LIMIT 1"
    encoder = Enc.param (Enc.nonNullable Enc.uuid)
    decoder = Dec.rowMaybe (Dec.column (Dec.nonNullable Dec.uuid))

lifecycleViolation :: Text -> Text -> Maybe Text -> Maybe Text -> DBException
lifecycleViolation = DBLifecycleViolation

blockerDetail :: UUID -> Text
blockerDetail blockerId = blockersDetail 1 [blockerId]

blockersDetail :: Int -> [UUID] -> Text
blockersDetail blockerCount blockerIds = TE.decodeUtf8 . BL.toStrict . Aeson.encode $ object
  [ "blocker_count" .= blockerCount
  , "blocker_ids" .= map UUID.toText blockerIds
  ]

data MoveViolation = MoveViolation Int UUID UUID

data ProjectPlacement = ProjectPlacement UUID UUID

data ParentPlacement = ParentPlacement UUID UUID (Maybe UUID) (Maybe UUID)

moveViolationDecoder :: Dec.Row MoveViolation
moveViolationDecoder = MoveViolation
  <$> (fromIntegral <$> Dec.column (Dec.nonNullable Dec.int8))
  <*> Dec.column (Dec.nonNullable Dec.uuid)
  <*> Dec.column (Dec.nonNullable Dec.uuid)

projectPlacementDecoder :: Dec.Row ProjectPlacement
projectPlacementDecoder = ProjectPlacement
  <$> Dec.column (Dec.nonNullable Dec.uuid)
  <*> Dec.column (Dec.nonNullable Dec.uuid)

parentPlacementDecoder :: Dec.Row ParentPlacement
parentPlacementDecoder = ParentPlacement
  <$> Dec.column (Dec.nonNullable Dec.uuid)
  <*> Dec.column (Dec.nonNullable Dec.uuid)
  <*> Dec.column (Dec.nullable Dec.uuid)
  <*> Dec.column (Dec.nullable Dec.uuid)

moveViolationException :: Text -> Text -> MoveViolation -> Text -> DBException
moveViolationException code message (MoveViolation blockerCount firstId secondId) hint =
  lifecycleViolation code message (Just detail) (Just hint)
  where
    detail = blockersDetail blockerCount [firstId, secondId]

lockTasksS :: [UUID] -> Session.Session ()
lockTasksS [] = pure ()
lockTasksS taskIds = void $ Session.statement (nub taskIds) lockTasksStatement

lockTasksStatement :: Statement.Statement [UUID] [UUID]
lockTasksStatement = Statement.Statement sql encoder decoder True
  where
    sql = BS8.pack $ unlines
      [ "SELECT id"
      , "  FROM tasks"
      , " WHERE id = ANY($1)"
      , "   AND deleted_at IS NULL"
      , " ORDER BY id"
      , " FOR UPDATE"
      ]
    encoder = Enc.param (Enc.nonNullable (Enc.foldableArray (Enc.nonNullable Enc.uuid)))
    decoder = Dec.rowList (Dec.column (Dec.nonNullable Dec.uuid))

lockIncidentDependencyTasksS :: [UUID] -> Session.Session ()
lockIncidentDependencyTasksS [] = pure ()
lockIncidentDependencyTasksS movedIds = void $ Session.statement (nub movedIds) lockIncidentDependencyTasksStatement

lockIncidentDependencyTasksStatement :: Statement.Statement [UUID] [UUID]
lockIncidentDependencyTasksStatement = Statement.Statement sql encoder decoder True
  where
    sql = BS8.pack $ unlines
      [ "WITH moved(id) AS ("
      , "  SELECT DISTINCT moved_id"
      , "    FROM unnest($1::uuid[]) AS moved_id"
      , "   WHERE moved_id IS NOT NULL"
      , "),"
      , "incident_task_ids(id) AS ("
      , "  SELECT td.task_id"
      , "    FROM task_dependencies td"
      , "   WHERE td.task_id IN (SELECT id FROM moved)"
      , "      OR td.depends_on_id IN (SELECT id FROM moved)"
      , "  UNION"
      , "  SELECT td.depends_on_id"
      , "    FROM task_dependencies td"
      , "   WHERE td.task_id IN (SELECT id FROM moved)"
      , "      OR td.depends_on_id IN (SELECT id FROM moved)"
      , ")"
      , "SELECT task_to_lock.id"
      , "  FROM tasks task_to_lock"
      , "  JOIN incident_task_ids incident ON incident.id = task_to_lock.id"
      , " WHERE task_to_lock.deleted_at IS NULL"
      , " ORDER BY task_to_lock.id"
      , " FOR UPDATE OF task_to_lock"
      ]
    encoder = Enc.param (Enc.nonNullable (Enc.foldableArray (Enc.nonNullable Enc.uuid)))
    decoder = Dec.rowList (Dec.column (Dec.nonNullable Dec.uuid))

lockBatchMoveExternalParentsS :: [UUID] -> Session.Session ()
lockBatchMoveExternalParentsS [] = pure ()
lockBatchMoveExternalParentsS movedIds = void $ Session.statement (nub movedIds) lockBatchMoveExternalParentsStatement

lockBatchMoveExternalParentsStatement :: Statement.Statement [UUID] [UUID]
lockBatchMoveExternalParentsStatement = Statement.Statement sql encoder decoder True
  where
    sql = BS8.pack $ unlines
      [ "WITH moved(id) AS ("
      , "  SELECT DISTINCT moved_id"
      , "    FROM unnest($1::uuid[]) AS moved_id"
      , "   WHERE moved_id IS NOT NULL"
      , "),"
      , "external_parent_ids(id) AS ("
      , "  SELECT DISTINCT parent.id"
      , "  FROM tasks child"
      , "  JOIN moved moved_child ON moved_child.id = child.id"
      , "  JOIN tasks parent ON parent.id = child.parent_id AND parent.deleted_at IS NULL"
      , "  LEFT JOIN moved moved_parent ON moved_parent.id = parent.id"
      , " WHERE child.deleted_at IS NULL"
      , "   AND moved_parent.id IS NULL"
      , ")"
      , "SELECT parent_to_lock.id"
      , "  FROM tasks parent_to_lock"
      , "  JOIN external_parent_ids external_parent ON external_parent.id = parent_to_lock.id"
      , " ORDER BY parent_to_lock.id"
      , " FOR UPDATE OF parent_to_lock"
      ]
    encoder = Enc.param (Enc.nonNullable (Enc.foldableArray (Enc.nonNullable Enc.uuid)))
    decoder = Dec.rowList (Dec.column (Dec.nonNullable Dec.uuid))

validateTaskProjectLockedS :: UUID -> Maybe UUID -> Session.Session (Maybe DBException)
validateTaskProjectLockedS _ Nothing = pure Nothing
validateTaskProjectLockedS workspaceId (Just projectId) = do
  mProject <- Session.statement projectId taskProjectPlacementStatement
  case mProject of
    Nothing -> pure $ Just $ DBForeignKeyViolation "Referenced project does not exist"
    Just (ProjectPlacement _ projectWorkspaceId)
      | projectWorkspaceId /= workspaceId -> pure $ Just $ DBCheckViolation "Task project must belong to the same workspace"
      | otherwise -> pure Nothing

taskProjectPlacementStatement :: Statement.Statement UUID (Maybe ProjectPlacement)
taskProjectPlacementStatement = Statement.Statement sql encoder decoder True
  where
    sql = BS8.pack $ unlines
      [ "SELECT id, workspace_id"
      , "  FROM projects"
      , " WHERE id = $1"
      , "   AND deleted_at IS NULL"
      , " FOR UPDATE"
      ]
    encoder = Enc.param (Enc.nonNullable Enc.uuid)
    decoder = Dec.rowMaybe projectPlacementDecoder

validateTaskPlacementLockedS :: UUID -> Maybe UUID -> Maybe UUID -> Session.Session (Maybe DBException)
validateTaskPlacementLockedS _ _ Nothing = pure Nothing
validateTaskPlacementLockedS workspaceId projectId (Just parentId) = do
  mParent <- Session.statement parentId taskParentPlacementStatement
  case mParent of
    Nothing -> pure $ Just $ DBForeignKeyViolation "Referenced parent task does not exist"
    Just (ParentPlacement parentTaskId parentWorkspaceId parentProjectId parentParentId)
      | parentWorkspaceId /= workspaceId -> pure $ Just $ DBCheckViolation "Parent task must belong to the same workspace"
      | parentProjectId /= projectId -> pure $ Just $ DBCheckViolation "Task and parent task must belong to the same project"
      | parentParentId /= Nothing -> pure $ Just $ lifecycleViolation
          "TASK_SUBTASK_DEPTH_EXCEEDED"
          "Cannot create or move a task under a subtask."
          (Just $ blockerDetail parentTaskId)
          (Just "Move the target parent to the top level before adding subtasks, or attach this task to a top-level task.")
      | otherwise -> pure Nothing

taskParentPlacementStatement :: Statement.Statement UUID (Maybe ParentPlacement)
taskParentPlacementStatement = Statement.Statement sql encoder decoder True
  where
    sql = BS8.pack $ unlines
      [ "SELECT id, workspace_id, project_id, parent_id"
      , "  FROM tasks"
      , " WHERE id = $1"
      , "   AND deleted_at IS NULL"
      ]
    encoder = Enc.param (Enc.nonNullable Enc.uuid)
    decoder = Dec.rowMaybe parentPlacementDecoder

validateTaskMoveDependencyConsistencyS :: [UUID] -> Maybe UUID -> Session.Session (Maybe DBException)
validateTaskMoveDependencyConsistencyS [] _ = pure Nothing
validateTaskMoveDependencyConsistencyS movedIds targetProjectId = do
  workspaceMismatch <- Session.statement normalizedMovedIds taskDependencyWorkspaceMismatchStatement
  case workspaceMismatch of
    Just violation -> pure $ Just $ moveViolationException
      "TASK_DEPENDENCY_CROSS_WORKSPACE"
      "Cannot move tasks while dependency endpoints span multiple workspaces."
      violation
      "Remove or repair cross-workspace dependency links before moving these tasks."
    Nothing -> do
      projectMismatch <- Session.statement (normalizedMovedIds, targetProjectId) taskDependencyProjectMismatchStatement
      case projectMismatch of
        Just violation -> pure $ Just $ moveViolationException
          "TASK_DEPENDENCY_CROSS_PROJECT"
          "Cannot move tasks because dependency endpoints would span projects."
          violation
          "Move dependent tasks together, move them to the dependency project, or remove the cross-project dependency first."
        Nothing -> do
          subtreeCycle <- Session.statement normalizedMovedIds taskSubtreeDependencyCycleStatement
          case subtreeCycle of
            Just violation -> pure $ Just $ moveViolationException
              "TASK_DEPENDENCY_HIERARCHY_CYCLE"
              "Cannot move a task subtree with a subtask that depends on its parent."
              violation
              "Remove the dependency from the subtask to its parent before moving this subtree."
            Nothing -> pure Nothing
  where
    normalizedMovedIds = nub movedIds

validateTaskParentDependencyConsistencyS :: UUID -> Maybe UUID -> Session.Session (Maybe DBException)
validateTaskParentDependencyConsistencyS _ Nothing = pure Nothing
validateTaskParentDependencyConsistencyS taskId (Just parentId) = do
  hierarchyCycle <- Session.statement (taskId, parentId) taskParentDependencyCycleStatement
  case hierarchyCycle of
    Just violation -> pure $ Just $ moveViolationException
      "TASK_DEPENDENCY_HIERARCHY_CYCLE"
      "Cannot make a task a subtask of one of its dependencies."
      violation
      "Remove the dependency chain from the subtask to the target parent before reparenting."
    Nothing -> pure Nothing

validateBatchMoveParentPlacementS :: [UUID] -> Maybe UUID -> Session.Session (Maybe DBException)
validateBatchMoveParentPlacementS [] _ = pure Nothing
validateBatchMoveParentPlacementS movedIds targetProjectId = do
  parentMismatch <- Session.statement (nub movedIds, targetProjectId) taskBatchMoveParentProjectMismatchStatement
  case parentMismatch of
    Just violation -> pure $ Just $ moveViolationException
      "TASK_PARENT_PROJECT_MISMATCH"
      "Cannot move a subtask away from its parent project."
      violation
      "Move the parent task with the subtask, detach the subtask first, or choose the parent task's project."
    Nothing -> pure Nothing

recomputeTaskAutoBlockingS :: [UUID] -> Session.Session ()
recomputeTaskAutoBlockingS [] = pure ()
recomputeTaskAutoBlockingS seedIds = Session.sql $ BS8.pack $
  "DO $$ BEGIN PERFORM hmem_recompute_task_auto_blocking(ARRAY["
    <> intercalate "," (map formatUUID (nub seedIds))
    <> "]::uuid[]); END $$;"
  where
    formatUUID uuid = "'" <> T.unpack (UUID.toText uuid) <> "'::uuid"

taskDependencyWorkspaceMismatchStatement :: Statement.Statement [UUID] (Maybe MoveViolation)
taskDependencyWorkspaceMismatchStatement = Statement.Statement sql encoder decoder True
  where
    sql = BS8.pack $ unlines
      [ "WITH moved(id) AS ("
      , "  SELECT DISTINCT moved_id"
      , "    FROM unnest($1::uuid[]) AS moved_id"
      , "   WHERE moved_id IS NOT NULL"
      , "),"
      , "incident_dependencies AS ("
      , "  SELECT td.task_id, td.depends_on_id, task.workspace_id AS task_workspace_id, dep.workspace_id AS dep_workspace_id"
      , "    FROM task_dependencies td"
      , "    JOIN tasks task ON task.id = td.task_id AND task.deleted_at IS NULL"
      , "    JOIN tasks dep ON dep.id = td.depends_on_id AND dep.deleted_at IS NULL"
      , "   WHERE td.task_id IN (SELECT id FROM moved)"
      , "      OR td.depends_on_id IN (SELECT id FROM moved)"
      , "),"
      , "violations AS ("
      , "  SELECT task_id, depends_on_id"
      , "    FROM incident_dependencies"
      , "   WHERE task_workspace_id IS DISTINCT FROM dep_workspace_id"
      , ")"
      , "SELECT count(*) OVER ()::bigint, task_id, depends_on_id"
      , "  FROM violations"
      , " ORDER BY task_id, depends_on_id"
      , " LIMIT 1"
      ]
    encoder = Enc.param (Enc.nonNullable (Enc.foldableArray (Enc.nonNullable Enc.uuid)))
    decoder = Dec.rowMaybe moveViolationDecoder

taskDependencyProjectMismatchStatement :: Statement.Statement ([UUID], Maybe UUID) (Maybe MoveViolation)
taskDependencyProjectMismatchStatement = Statement.Statement sql encoder decoder True
  where
    sql = BS8.pack $ unlines
      [ "WITH moved(id) AS ("
      , "  SELECT DISTINCT moved_id"
      , "    FROM unnest($1::uuid[]) AS moved_id"
      , "   WHERE moved_id IS NOT NULL"
      , "),"
      , "incident_dependencies AS ("
      , "  SELECT td.task_id, td.depends_on_id,"
      , "         CASE WHEN moved_task.id IS NOT NULL THEN $2::uuid ELSE task.project_id END AS task_project_after,"
      , "         CASE WHEN moved_dep.id IS NOT NULL THEN $2::uuid ELSE dep.project_id END AS dep_project_after"
      , "    FROM task_dependencies td"
      , "    JOIN tasks task ON task.id = td.task_id AND task.deleted_at IS NULL"
      , "    JOIN tasks dep ON dep.id = td.depends_on_id AND dep.deleted_at IS NULL"
      , "    LEFT JOIN moved moved_task ON moved_task.id = td.task_id"
      , "    LEFT JOIN moved moved_dep ON moved_dep.id = td.depends_on_id"
      , "   WHERE moved_task.id IS NOT NULL"
      , "      OR moved_dep.id IS NOT NULL"
      , "),"
      , "violations AS ("
      , "  SELECT task_id, depends_on_id"
      , "    FROM incident_dependencies"
      , "   WHERE task_project_after IS DISTINCT FROM dep_project_after"
      , ")"
      , "SELECT count(*) OVER ()::bigint, task_id, depends_on_id"
      , "  FROM violations"
      , " ORDER BY task_id, depends_on_id"
      , " LIMIT 1"
      ]
    encoder =
      contramap fst (Enc.param (Enc.nonNullable (Enc.foldableArray (Enc.nonNullable Enc.uuid)))) <>
      contramap snd (Enc.param (Enc.nullable Enc.uuid))
    decoder = Dec.rowMaybe moveViolationDecoder

taskSubtreeDependencyCycleStatement :: Statement.Statement [UUID] (Maybe MoveViolation)
taskSubtreeDependencyCycleStatement = Statement.Statement sql encoder decoder True
  where
    sql = BS8.pack $ unlines
      [ "WITH RECURSIVE moved(id) AS ("
      , "  SELECT DISTINCT moved_id"
      , "    FROM unnest($1::uuid[]) AS moved_id"
      , "   WHERE moved_id IS NOT NULL"
      , "),"
      , "child_parent AS ("
      , "  SELECT child.id AS child_id, parent.id AS parent_id"
      , "    FROM tasks child"
      , "    JOIN tasks parent ON parent.id = child.parent_id AND parent.deleted_at IS NULL"
      , "    JOIN moved moved_child ON moved_child.id = child.id"
      , "    JOIN moved moved_parent ON moved_parent.id = parent.id"
      , "   WHERE child.deleted_at IS NULL"
      , "),"
      , "dependency_reachable(origin_id, dependency_id, path) AS ("
      , "  SELECT cp.child_id, td.depends_on_id, ARRAY[td.depends_on_id]::uuid[]"
      , "    FROM child_parent cp"
      , "    JOIN task_dependencies td ON td.task_id = cp.child_id"
      , "  UNION ALL"
      , "  SELECT dr.origin_id, td.depends_on_id, dr.path || td.depends_on_id"
      , "    FROM dependency_reachable dr"
      , "    JOIN task_dependencies td ON td.task_id = dr.dependency_id"
      , "   WHERE td.depends_on_id <> ALL(dr.path)"
      , "),"
      , "violations AS ("
      , "  SELECT cp.child_id, cp.parent_id"
      , "    FROM child_parent cp"
      , "    JOIN dependency_reachable dr ON dr.origin_id = cp.child_id AND dr.dependency_id = cp.parent_id"
      , ")"
      , "SELECT count(*) OVER ()::bigint, child_id, parent_id"
      , "  FROM violations"
      , " ORDER BY child_id, parent_id"
      , " LIMIT 1"
      ]
    encoder = Enc.param (Enc.nonNullable (Enc.foldableArray (Enc.nonNullable Enc.uuid)))
    decoder = Dec.rowMaybe moveViolationDecoder

taskParentDependencyCycleStatement :: Statement.Statement (UUID, UUID) (Maybe MoveViolation)
taskParentDependencyCycleStatement = Statement.Statement sql encoder decoder True
  where
    sql = BS8.pack $ unlines
      [ "WITH RECURSIVE dependency_reachable(dependency_id, path) AS ("
      , "  SELECT td.depends_on_id, ARRAY[td.depends_on_id]::uuid[]"
      , "    FROM task_dependencies td"
      , "   WHERE td.task_id = $1"
      , "  UNION ALL"
      , "  SELECT td.depends_on_id, dr.path || td.depends_on_id"
      , "    FROM dependency_reachable dr"
      , "    JOIN task_dependencies td ON td.task_id = dr.dependency_id"
      , "   WHERE td.depends_on_id <> ALL(dr.path)"
      , "),"
      , "violations AS ("
      , "  SELECT $1::uuid AS task_id, $2::uuid AS parent_id"
      , "   WHERE EXISTS (SELECT 1 FROM dependency_reachable WHERE dependency_id = $2)"
      , ")"
      , "SELECT count(*) OVER ()::bigint, task_id, parent_id"
      , "  FROM violations"
      , " LIMIT 1"
      ]
    encoder =
      contramap fst (Enc.param (Enc.nonNullable Enc.uuid)) <>
      contramap snd (Enc.param (Enc.nonNullable Enc.uuid))
    decoder = Dec.rowMaybe moveViolationDecoder

taskBatchMoveParentProjectMismatchStatement :: Statement.Statement ([UUID], Maybe UUID) (Maybe MoveViolation)
taskBatchMoveParentProjectMismatchStatement = Statement.Statement sql encoder decoder True
  where
    sql = BS8.pack $ unlines
      [ "WITH moved(id) AS ("
      , "  SELECT DISTINCT moved_id"
      , "    FROM unnest($1::uuid[]) AS moved_id"
      , "   WHERE moved_id IS NOT NULL"
      , "),"
      , "violations AS ("
      , "  SELECT child.id AS task_id, parent.id AS parent_id"
      , "    FROM tasks child"
      , "    JOIN moved moved_child ON moved_child.id = child.id"
      , "    JOIN tasks parent ON parent.id = child.parent_id AND parent.deleted_at IS NULL"
      , "    LEFT JOIN moved moved_parent ON moved_parent.id = parent.id"
      , "   WHERE child.deleted_at IS NULL"
      , "     AND moved_parent.id IS NULL"
      , "     AND parent.project_id IS DISTINCT FROM $2::uuid"
      , ")"
      , "SELECT count(*) OVER ()::bigint, task_id, parent_id"
      , "  FROM violations"
      , " ORDER BY task_id, parent_id"
      , " LIMIT 1"
      ]
    encoder =
      contramap fst (Enc.param (Enc.nonNullable (Enc.foldableArray (Enc.nonNullable Enc.uuid)))) <>
      contramap snd (Enc.param (Enc.nullable Enc.uuid))
    decoder = Dec.rowMaybe moveViolationDecoder

------------------------------------------------------------------------
-- Create
------------------------------------------------------------------------

createTask :: Pool Hasql.Connection -> CreateTask -> IO Task
createTask pool ct = do
  _ <- ensureTaskPlacement pool ct.workspaceId ct.projectId ct.parentId
  let pri  = maybe 5 fromIntegral (ct.priority) :: Int16
      meta = fromMaybe (toJSON (mempty :: Object)) (ct.metadata)
  rows <- runSession pool $ Session.statement () $ run $
    insert Insert
      { into = taskSchema
      , rows = values
          [ TaskT
              { taskId          = unsafeDefault
              , taskWorkspaceId = lit ct.workspaceId
              , taskProjectId   = lit ct.projectId
              , taskParentId    = lit ct.parentId
              , taskTitle       = lit ct.title
              , taskDescription = lit ct.description
              , taskStatus      = unsafeDefault
              , taskPriority    = lit pri
              , taskMetadata    = lit meta
              , taskDueAt       = lit ct.dueAt
              , taskCompletedAt = lit (Nothing :: Maybe UTCTime)
              , taskSearchVector = unsafeDefault
              , taskDeletedAt   = unsafeDefault
              , taskCreatedAt   = unsafeDefault
              , taskUpdatedAt   = unsafeDefault
              }
          ]
      , onConflict = Abort
      , returning  = Returning id
      }
  case rows of
    (r:_) -> pure $ rowToTask r
    []    -> throwIO $ DBOtherError "createTask: INSERT returned no rows"

------------------------------------------------------------------------
-- Read
------------------------------------------------------------------------

getTask :: Pool Hasql.Connection -> UUID -> IO (Maybe Task)
getTask pool tid = do
  rows <- runSession pool $ Session.statement () $ run $ select $ do
    row <- each taskSchema
    where_ $ row.taskId ==. lit tid
    where_ $ activeTask row
    pure row
  case rows of
    []    -> pure Nothing
    (r:_) -> do
      enriched <- enrichTaskCounts pool [rowToTask r]
      pure $ listToMaybe enriched

------------------------------------------------------------------------
-- Update
------------------------------------------------------------------------

updateTask :: Pool Hasql.Connection -> UUID -> UpdateTask -> IO (Maybe Task)
updateTask pool tid ut = fmap (\(task, _, _) -> task) <$> updateTaskWithDependencySnapshots pool tid ut

updateTaskWithDependencySnapshots :: Pool Hasql.Connection -> UUID -> UpdateTask -> IO (Maybe (Task, [TaskDependencyAutoBlockSnapshot], [TaskDependencyAutoBlockSnapshot]))
updateTaskWithDependencySnapshots pool tid ut = do
  current <- getTask pool tid
  case current of
    Nothing -> pure Nothing
    Just task -> do
      let targetProjectId = applyFieldUpdateMaybe task.projectId ut.projectId
          targetParentId = applyFieldUpdateMaybe task.parentId ut.parentId
          targetStatus = fromMaybe task.status ut.status
          enforcingStartGate =
            targetParentId /= Nothing &&
              ( (ut.status == Just InProgress && task.status /= InProgress)
                || (targetParentId /= task.parentId && targetStatus == InProgress)
              )
      mParent <- ensureTaskPlacement pool task.workspaceId targetProjectId targetParentId
      ensureTaskCanBecomeSubtask pool tid targetParentId
      ensureSubtaskStartAllowed enforcingStartGate mParent targetStatus
      let projectChanged = task.projectId /= targetProjectId
          placementChanged = projectChanged || task.parentId /= targetParentId
          parentSeedIds = [parentId | Just parentId <- [task.parentId, targetParentId]]
      transactionResult <- runTransaction pool $ do
        projectValidation <- validateTaskProjectLockedS task.workspaceId targetProjectId
        case projectValidation of
          Just err -> pure (Left err)
          Nothing -> do
            lockTasksS (tid : parentSeedIds)
            movedIds <- if projectChanged
              then Session.statement tid taskSubtreeIdsForUpdateStatement
              else pure [tid]
            lockTasksS movedIds
            lockIncidentDependencyTasksS movedIds
            let snapshotSeedIds = nub $ movedIds <> [tid] <> parentSeedIds
            placementValidation <- validateTaskPlacementLockedS task.workspaceId targetProjectId targetParentId
            validationFailure <- case placementValidation of
              Just err -> pure (Just err)
              Nothing -> if placementChanged
                then do
                  dependencyProjectValidation <- if projectChanged
                    then validateTaskMoveDependencyConsistencyS movedIds targetProjectId
                    else pure Nothing
                  case dependencyProjectValidation of
                    Just err -> pure (Just err)
                    Nothing -> if task.parentId /= targetParentId
                      then validateTaskParentDependencyConsistencyS tid targetParentId
                      else pure Nothing
                else pure Nothing
            case validationFailure of
              Just err -> pure (Left err)
              Nothing -> do
                before <- dependencyAutoBlockSnapshotsS snapshotSeedIds
                when (task.projectId /= targetProjectId) $ do
                  Session.statement () $ run_ $
                    update Update
                      { target = taskSchema
                      , from = pure ()
                      , set = \_ row -> row { taskProjectId = lit targetProjectId }
                      , updateWhere = \_ row -> in_ row.taskId (map lit movedIds) &&. activeTask row
                      , returning = NoReturning
                      }

                rows <- Session.statement () $ run $
                  update Update
                    { target = taskSchema
                    , from = pure ()
                    , set = \_ row -> row
                        { taskTitle       = maybe row.taskTitle       lit ut.title
                        , taskDescription = applyNullableUpdate row.taskDescription ut.description
                        , taskProjectId   = applyNullableUpdate row.taskProjectId ut.projectId
                        , taskParentId    = applyNullableUpdate row.taskParentId ut.parentId
                        , taskStatus      = maybe row.taskStatus      lit ut.status
                        , taskPriority    = maybe row.taskPriority    (lit . fromIntegral) ut.priority
                        , taskMetadata    = maybe row.taskMetadata    lit ut.metadata
                        , taskDueAt       = applyNullableUpdate row.taskDueAt ut.dueAt
                        -- completed_at is managed by the hmem_task_completion trigger
                        , taskCompletedAt = row.taskCompletedAt
                        }
                    , updateWhere = \_ row -> row.taskId ==. lit tid &&. activeTask row
                    , returning = Returning id
                    }
                case rows of
                  []    -> do
                    recomputeTaskAutoBlockingS snapshotSeedIds
                    after <- dependencyAutoBlockSnapshotsS snapshotSeedIds
                    pure $ Right (Nothing, before, after)
                  (r:_) -> do
                    recomputeTaskAutoBlockingS snapshotSeedIds
                    after <- dependencyAutoBlockSnapshotsS snapshotSeedIds
                    pure $ Right (Just $ rowToTask r, before, after)
      (mTask, beforeRaw, afterRaw) <- case transactionResult of
        Left err -> throwIO err
        Right result -> pure result
      case mTask of
        Nothing -> pure Nothing
        Just t -> do
          mEnrichedTask <- getTask pool t.id
          case mEnrichedTask of
            Nothing -> pure Nothing
            Just enrichedTask -> do
              before <- enrichDependencyAutoBlockSnapshots pool beforeRaw
              after <- enrichDependencyAutoBlockSnapshots pool afterRaw
              pure $ Just (enrichedTask, before, after)

------------------------------------------------------------------------
-- Delete
------------------------------------------------------------------------

-- | Batch-update multiple tasks. Each item is updated individually.
-- Returns the count of successfully updated tasks.
updateTaskBatch :: Pool Hasql.Connection -> [(UUID, UpdateTask)] -> IO Int
updateTaskBatch _pool [] = pure 0
updateTaskBatch pool items = do
  results <- mapM (\(tid, ut) -> updateTask pool tid ut) items
  pure $ length [() | Just _ <- results]

deleteTask :: Pool Hasql.Connection -> UUID -> IO Bool
deleteTask pool tid = isJustCascade <$> deleteTaskCascade pool tid
  where
    isJustCascade = maybe False (const True)

deleteTaskCascade :: Pool Hasql.Connection -> UUID -> IO (Maybe CascadeResult)
deleteTaskCascade pool tid = do
  runTransaction pool $ do
    ids <- Session.statement tid taskSubtreeIdsStatement
    deleteTaskIdsCascadeS ids

-- | Soft-delete multiple tasks by ID in a single transaction, cascading to
-- active descendants. Returns the number of task rows actually deleted.
deleteTaskBatch :: Pool Hasql.Connection -> [UUID] -> IO Int
deleteTaskBatch _pool [] = pure 0
deleteTaskBatch pool ids = do
  runTransaction pool $ do
    cascadeIds <- Session.statement ids taskSubtreeIdsForRootsStatement
    mResult <- deleteTaskIdsCascadeS cascadeIds
    pure $ maybe 0 (.taskCount) mResult

deleteTaskIdsCascadeS :: [UUID] -> Session.Session (Maybe CascadeResult)
deleteTaskIdsCascadeS [] = pure Nothing
deleteTaskIdsCascadeS ids = do
  dependencyCount <- deleteTaskDependenciesS ids
  taskCount <- Session.statement ids softDeleteTasksStatement
  pure . Just $ CascadeResult
    { affected = taskCount
    , projectCount = 0
    , taskCount = taskCount
    , dependencyLinkCount = dependencyCount
    }

-- | Restore a soft-deleted task by clearing its deleted_at timestamp.
-- Returns True if the task was restored, False if not found or not deleted.
restoreTask :: Pool Hasql.Connection -> UUID -> IO Bool
restoreTask pool tid = do
  runTransaction pool $ do
    rows <- Session.statement () $ run $ select $ do
      row <- each taskSchema
      where_ $ row.taskId ==. lit tid
      pure row
    case rows of
      [] -> pure False
      (row:_)
        | Just deletedAt <- row.taskDeletedAt -> do
            ids <- Session.statement (tid, deletedAt) deletedTaskSubtreeIdsStatement
            n <- Session.statement () $ runN $
              update Update
                { target = taskSchema
                , from = pure ()
                , set = \_ task -> task { taskDeletedAt = lit (Nothing :: Maybe UTCTime) }
                , updateWhere = \_ task -> in_ task.taskId (map lit ids) &&. not_ (isNull task.taskDeletedAt)
                , returning = NoReturning
                }
            pure (n > 0)
        | otherwise -> pure False

purgeTaskCascade :: Pool Hasql.Connection -> UUID -> IO (Maybe CascadeResult)
purgeTaskCascade pool tid =
  runTransaction pool $ do
    rows <- Session.statement () $ run $ select $ do
      row <- each taskSchema
      where_ $ row.taskId ==. lit tid
      pure row
    case rows of
      [] -> pure Nothing
      (row:_) -> case row.taskDeletedAt of
        Nothing -> pure Nothing
        Just _deletedAt -> do
          -- Purge hard-deletes every descendant row, including tasks that were
          -- soft-deleted before the root task.  Timestamp-scoped restore remains
          -- separate; purge must not rely on implicit FK cascades for counts or
          -- dependency cleanup.
          ids <- Session.statement tid allTaskSubtreeIdsStatement
          dependencyCount <- deleteTaskDependenciesS ids
          taskCount <- Session.statement ids purgeTasksStatement
          pure . Just $ CascadeResult
            { affected = taskCount
            , projectCount = 0
            , taskCount = taskCount
            , dependencyLinkCount = dependencyCount
            }

-- | Move multiple tasks to a new project (or detach from all projects
-- when projectId is Nothing). Returns the number of tasks actually moved.
moveTasksBatch :: Pool Hasql.Connection -> [UUID] -> Maybe UUID -> IO Int
moveTasksBatch _pool [] _ = pure 0
moveTasksBatch pool ids projectId = do
  let rootIds = nub ids
  roots <- mapM (getTask pool) rootIds
  case [task | Just task <- roots] of
    [] -> pure 0
    firstTask:activeRoots -> do
      let rootTasks = firstTask : activeRoots
          rootWorkspaceId = firstTask.workspaceId
      case [task.id | task <- rootTasks, task.workspaceId /= rootWorkspaceId] of
        otherTaskId:_ -> throwIO $ lifecycleViolation
          "TASK_BATCH_MOVE_CROSS_WORKSPACE"
          "Cannot batch-move tasks from multiple workspaces."
          (Just $ blockersDetail 2 [firstTask.id, otherTaskId])
          (Just "Move tasks from one workspace at a time.")
        [] -> pure ()
      ensureTaskProject pool rootWorkspaceId projectId
      transactionResult <- runTransaction pool $ do
        projectValidation <- validateTaskProjectLockedS rootWorkspaceId projectId
        case projectValidation of
          Just err -> pure (Left err)
          Nothing -> do
            movedIds <- Session.statement rootIds taskSubtreeIdsForRootsForUpdateStatement
            lockBatchMoveExternalParentsS movedIds
            lockIncidentDependencyTasksS movedIds
            parentValidation <- validateBatchMoveParentPlacementS movedIds projectId
            validationFailure <- case parentValidation of
              Just err -> pure (Just err)
              Nothing -> validateTaskMoveDependencyConsistencyS movedIds projectId
            case validationFailure of
              Just err -> pure (Left err)
              Nothing -> do
                _before <- dependencyAutoBlockSnapshotsS movedIds
                movedCount <- Session.statement () $ runN $
                  update Update
                    { target = taskSchema
                    , from = pure ()
                    , set = \_ row -> row { taskProjectId = lit projectId }
                    , updateWhere = \_ row -> in_ row.taskId (map lit movedIds) &&. activeTask row
                    , returning = NoReturning
                    }
                recomputeTaskAutoBlockingS movedIds
                _after <- dependencyAutoBlockSnapshotsS movedIds
                pure (Right movedCount)
      n <- case transactionResult of
        Left err -> throwIO err
        Right movedCount -> pure movedCount
      pure (fromIntegral n)

------------------------------------------------------------------------
-- List
------------------------------------------------------------------------

listTasks
  :: Pool Hasql.Connection
  -> UUID           -- ^ project_id
  -> Maybe TaskStatus
  -> Maybe Int      -- ^ limit
  -> Maybe Int      -- ^ offset
  -> IO [Task]
listTasks pool projId mstatus mlimit moffset =
  listTasksWithQuery pool TaskListQuery
    { workspaceId = Nothing
    , projectId = Just projId
    , status = mstatus
    , priority = Nothing
    , query = Nothing
    , searchLanguage = Nothing
    , createdAfter = Nothing
    , createdBefore = Nothing
    , updatedAfter = Nothing
    , updatedBefore = Nothing
    , limit = mlimit
    , offset = moffset
    }

listTasksWithQuery :: Pool Hasql.Connection -> TaskListQuery -> IO [Task]
listTasksWithQuery pool tq = do
  let (lim, off) = capPaginationOverfetch tq.limit tq.offset
      searchLang = fromMaybe "english" tq.searchLanguage
      applyFilters row = do
        where_ $ activeTask row
        case tq.workspaceId of
          Just wsId -> where_ $ row.taskWorkspaceId ==. lit wsId
          Nothing -> pure ()
        case tq.projectId of
          Just projId -> where_ $ row.taskProjectId ==. lit (Just projId)
          Nothing -> pure ()
        case tq.status of
          Nothing -> pure ()
          Just s  -> where_ $ row.taskStatus ==. lit s
        case tq.priority of
          Just priority -> where_ $ row.taskPriority ==. lit (fromIntegral priority :: Int16)
          Nothing -> pure ()
        case tq.createdAfter of
          Just createdAfter -> where_ $ row.taskCreatedAt >=. lit createdAfter
          Nothing -> pure ()
        case tq.createdBefore of
          Just createdBefore -> where_ $ row.taskCreatedAt <=. lit createdBefore
          Nothing -> pure ()
        case tq.updatedAfter of
          Just updatedAfter -> where_ $ row.taskUpdatedAt >=. lit updatedAfter
          Nothing -> pure ()
        case tq.updatedBefore of
          Just updatedBefore -> where_ $ row.taskUpdatedAt <=. lit updatedBefore
          Nothing -> pure ()
  case tq.query of
    Nothing -> do
      rows <- runSession pool $ Session.statement () $ run $ select $
        limit (fromIntegral lim) $ offset (fromIntegral off) $
        orderBy (((\row -> row.taskPriority) >$< desc) <> ((\row -> row.taskCreatedAt) >$< asc)) $ do
          row <- each taskSchema
          applyFilters row
          pure row
      enrichTaskCounts pool $ map rowToTask rows
    Just q -> do
      results <- runSession pool $ Session.statement () $ run $ select $
        limit (fromIntegral lim) $ offset (fromIntegral off) $
        orderBy (snd >$< desc) $ do
          row <- each taskSchema
          applyFilters row
          let config = unsafeCastExpr (lit searchLang) :: Expr PgRegConfig
          let tsq = function "plainto_tsquery" (config, lit q) :: Expr PgTSQuery
          let tsvec = row.taskSearchVector :: Expr PgTSVector
          where_ $ rawBinaryOperator "@@" tsvec tsq
          let tsRank = function "ts_rank" (tsvec, tsq) :: Expr Double
          pure (row, tsRank)
      enrichTaskCounts pool $ map (rowToTask . fst) results

-- | List tasks by workspace (including workspace-level tasks without a project).
listTasksByWorkspace
  :: Pool Hasql.Connection
  -> UUID           -- ^ workspace_id
  -> Maybe TaskStatus
  -> Maybe UUID     -- ^ project_id filter (Nothing = all tasks in workspace)
  -> Maybe Int      -- ^ limit
  -> Maybe Int      -- ^ offset
  -> IO [Task]
listTasksByWorkspace pool wsId mstatus mprojId mlimit moffset =
  listTasksWithQuery pool TaskListQuery
    { workspaceId = Just wsId
    , projectId = mprojId
    , status = mstatus
    , priority = Nothing
    , query = Nothing
    , searchLanguage = Nothing
    , createdAfter = Nothing
    , createdBefore = Nothing
    , updatedAfter = Nothing
    , updatedBefore = Nothing
    , limit = mlimit
    , offset = moffset
    }

-- | Return the next actionable tasks for a project subtree.
--
-- A task is actionable when it is still open and either has no open
-- dependencies or dependency-blocked diagnostics were explicitly requested.
-- Project subprojects are included recursively, and descendants of in-scope
-- tasks are included for migrated task trees.  Subtasks are only startable
-- when their immediate parent is in_progress.  Parents with open descendants
-- are returned with completionGated/openDescendantCount annotations rather
-- than being filtered out.  Results are ordered by scoped project priority,
-- then task priority, then stable task creation/id tie-breakers.
listNextTasks :: Pool Hasql.Connection -> UUID -> Bool -> Int -> IO [NextTaskCandidate]
listNextTasks pool projectId includeBlocked limitRows = do
  rows <- runSession pool $ Session.statement (projectId, includeBlocked, toInt32 sanitizedLimit) listNextTasksStatement
  enrichedTasks <- enrichTaskCounts pool (map (.task) rows)
  let enrichedById = Map.fromList [(task.id, task) | task <- enrichedTasks]
      enrichCandidate candidate = NextTaskCandidate
        { task = fromMaybe candidate.task (Map.lookup candidate.task.id enrichedById)
        , completionGated = candidate.completionGated
        , openDescendantCount = candidate.openDescendantCount
        , dependencyBlocked = candidate.dependencyBlocked
        , openDependencyCount = candidate.openDependencyCount
        }
  pure (map enrichCandidate rows)
  where
    sanitizedLimit = Prelude.min 200 . Prelude.max 1 $ limitRows

    toInt32 :: Int -> Int32
    toInt32 = fromIntegral

listNextTasksStatement :: Statement.Statement (UUID, Bool, Int32) [NextTaskCandidate]
listNextTasksStatement = Statement.Statement sql encoder decoder True
  where
    sql = BS8.pack $ unlines
      [ "WITH RECURSIVE project_tree(id, workspace_id, priority) AS ("
      , "  SELECT id, workspace_id, priority"
      , "    FROM projects"
      , "   WHERE id = $1"
      , "     AND deleted_at IS NULL"
      , "     AND status IN ('active'::project_status_enum, 'paused'::project_status_enum)"
      , "  UNION ALL"
      , "  SELECT child.id, child.workspace_id, child.priority"
      , "    FROM projects child"
      , "    JOIN project_tree pt ON child.parent_id = pt.id"
      , "   WHERE child.deleted_at IS NULL"
      , "     AND child.status IN ('active'::project_status_enum, 'paused'::project_status_enum)"
      , "     AND child.workspace_id = pt.workspace_id"
      , "),"
      , "scoped_tasks(id, workspace_id, project_id, parent_id, title, description, status, priority, metadata, due_at, completed_at, created_at, updated_at, scope_project_id, scope_project_priority) AS ("
      , "  SELECT t.id, t.workspace_id, t.project_id, t.parent_id, t.title, t.description,"
      , "         t.status, t.priority, t.metadata, t.due_at, t.completed_at, t.created_at, t.updated_at,"
      , "         pt.id, pt.priority"
      , "    FROM tasks t"
      , "    JOIN project_tree pt ON t.project_id = pt.id"
      , "   WHERE t.deleted_at IS NULL"
      , "     AND t.workspace_id = pt.workspace_id"
      , "  UNION"
      , "  SELECT child.id, child.workspace_id, child.project_id, child.parent_id, child.title, child.description,"
      , "         child.status, child.priority, child.metadata, child.due_at, child.completed_at, child.created_at, child.updated_at,"
      , "         coalesce(child_pt.id, parent.scope_project_id), coalesce(child_pt.priority, parent.scope_project_priority)"
      , "    FROM tasks child"
      , "    JOIN scoped_tasks parent ON child.parent_id = parent.id"
      , "    LEFT JOIN project_tree child_pt ON child.project_id = child_pt.id AND child.workspace_id = child_pt.workspace_id"
      , "   WHERE child.deleted_at IS NULL"
      , "     AND child.workspace_id = parent.workspace_id"
      , "),"
      , "candidate_tasks AS ("
      , "  SELECT task.*"
      , "    FROM scoped_tasks task"
      , "    LEFT JOIN scoped_tasks parent ON task.parent_id = parent.id"
      , "   WHERE task.status IN ('todo'::task_status_enum, 'in_progress'::task_status_enum, 'blocked'::task_status_enum)"
      , "     AND ($2 OR task.status <> 'blocked'::task_status_enum)"
      , "     AND (task.parent_id IS NULL OR parent.status = 'in_progress'::task_status_enum)"
      , "),"
      , "open_descendant_ancestors(descendant_id, ancestor_id) AS ("
      , "  SELECT child.id, parent.id"
      , "    FROM scoped_tasks child"
      , "    JOIN scoped_tasks parent ON child.parent_id = parent.id"
      , "   WHERE child.status IN ('todo'::task_status_enum, 'in_progress'::task_status_enum, 'blocked'::task_status_enum)"
      , "  UNION"
      , "  SELECT current.descendant_id, parent.id"
      , "    FROM open_descendant_ancestors current"
      , "    JOIN scoped_tasks ancestor ON ancestor.id = current.ancestor_id"
      , "    JOIN scoped_tasks parent ON ancestor.parent_id = parent.id"
      , "),"
      , "open_descendant_counts AS ("
      , "  SELECT ancestor_id AS id, count(DISTINCT descendant_id)::bigint AS open_descendant_count"
      , "    FROM open_descendant_ancestors"
      , "   GROUP BY ancestor_id"
      , "),"
      , "open_dependency_counts AS ("
      , "  SELECT candidate.id, count(DISTINCT dep.id)::bigint AS open_dependency_count"
      , "    FROM candidate_tasks candidate"
      , "    JOIN task_dependencies dep_link ON dep_link.task_id = candidate.id"
      , "    JOIN tasks dep ON dep.id = dep_link.depends_on_id"
      , "   WHERE dep.deleted_at IS NULL"
      , "     AND dep.status IN ('todo'::task_status_enum, 'in_progress'::task_status_enum, 'blocked'::task_status_enum)"
      , "   GROUP BY candidate.id"
      , ")"
      , "SELECT candidate.id, candidate.workspace_id, candidate.project_id, candidate.parent_id,"
      , "       candidate.title, candidate.description, candidate.status::text, candidate.priority,"
      , "       candidate.metadata, candidate.due_at, candidate.completed_at,"
      , "       candidate.created_at, candidate.updated_at,"
      , "       coalesce(open_desc.open_descendant_count, 0) > 0 AS completion_gated,"
      , "       coalesce(open_desc.open_descendant_count, 0)::bigint AS open_descendant_count,"
      , "       coalesce(open_dep.open_dependency_count, 0) > 0 AS dependency_blocked,"
      , "       coalesce(open_dep.open_dependency_count, 0)::bigint AS open_dependency_count"
      , "  FROM candidate_tasks candidate"
      , "  LEFT JOIN open_descendant_counts open_desc ON open_desc.id = candidate.id"
      , "  LEFT JOIN open_dependency_counts open_dep ON open_dep.id = candidate.id"
      , " WHERE ($2 OR coalesce(open_dep.open_dependency_count, 0) = 0)"
      , " ORDER BY candidate.scope_project_priority DESC, candidate.priority DESC, candidate.created_at ASC, candidate.id ASC"
      , " LIMIT $3"
      ]
    encoder =
      contramap (\(projectId, _, _) -> projectId) (Enc.param (Enc.nonNullable Enc.uuid)) <>
      contramap (\(_, includeBlocked, _) -> includeBlocked) (Enc.param (Enc.nonNullable Enc.bool)) <>
      contramap (\(_, _, limitRows) -> limitRows) (Enc.param (Enc.nonNullable Enc.int4))
    decoder = Dec.rowList nextTaskCandidateRowDecoder

nextTaskCandidateRowDecoder :: Dec.Row NextTaskCandidate
nextTaskCandidateRowDecoder = do
  task <- rawTaskRowDecoder
  completionGated <- Dec.column (Dec.nonNullable Dec.bool)
  openDescendantCount <- Dec.column (Dec.nonNullable Dec.int8)
  dependencyBlocked <- Dec.column (Dec.nonNullable Dec.bool)
  openDependencyCount <- Dec.column (Dec.nonNullable Dec.int8)
  pure NextTaskCandidate
    { task = task
    , completionGated = completionGated
    , openDescendantCount = fromIntegral openDescendantCount
    , dependencyBlocked = dependencyBlocked
    , openDependencyCount = fromIntegral openDependencyCount
    }

dependencyAutoBlockSnapshots :: Pool Hasql.Connection -> [UUID] -> IO [TaskDependencyAutoBlockSnapshot]
dependencyAutoBlockSnapshots _ [] = pure []
dependencyAutoBlockSnapshots pool seedIds = do
  snapshots <- runSession pool $ dependencyAutoBlockSnapshotsS seedIds
  enrichDependencyAutoBlockSnapshots pool snapshots

enrichDependencyAutoBlockSnapshots :: Pool Hasql.Connection -> [TaskDependencyAutoBlockSnapshot] -> IO [TaskDependencyAutoBlockSnapshot]
enrichDependencyAutoBlockSnapshots _ [] = pure []
enrichDependencyAutoBlockSnapshots pool snapshots = do
  enrichedTasks <- enrichTaskCounts pool (map (.task) snapshots)
  let enrichedById = Map.fromList [(task.id, task) | task <- enrichedTasks]
      enrichSnapshot :: TaskDependencyAutoBlockSnapshot -> TaskDependencyAutoBlockSnapshot
      enrichSnapshot snapshot = TaskDependencyAutoBlockSnapshot
        { task = Map.findWithDefault snapshot.task snapshot.task.id enrichedById
        , autoBlocked = snapshot.autoBlocked
        , openDependencyCount = snapshot.openDependencyCount
        }
  pure $ map enrichSnapshot snapshots

dependencyAutoBlockSnapshotsS :: [UUID] -> Session.Session [TaskDependencyAutoBlockSnapshot]
dependencyAutoBlockSnapshotsS seedIds = Session.statement seedIds dependencyAutoBlockSnapshotsStatement

dependencyAutoBlockSnapshotsStatement :: Statement.Statement [UUID] [TaskDependencyAutoBlockSnapshot]
dependencyAutoBlockSnapshotsStatement = Statement.Statement sql encoder decoder True
  where
    sql = BS8.pack $ unlines
      [ "WITH RECURSIVE seeds(id) AS ("
      , "  SELECT DISTINCT seed_id"
      , "    FROM unnest($1::uuid[]) AS seed_id"
      , "   WHERE seed_id IS NOT NULL"
      , "),"
      , "dependency_dependents(id) AS ("
      , "  SELECT td.task_id"
      , "    FROM task_dependencies td"
      , "    JOIN seeds s ON s.id = td.depends_on_id"
      , "    JOIN tasks dependent ON dependent.id = td.task_id"
      , "   WHERE dependent.deleted_at IS NULL"
      , "  UNION"
      , "  SELECT td.task_id"
      , "    FROM task_dependencies td"
      , "    JOIN dependency_dependents dd ON dd.id = td.depends_on_id"
      , "    JOIN tasks dependent ON dependent.id = td.task_id"
      , "   WHERE dependent.deleted_at IS NULL"
      , "),"
      , "direct_targets(id) AS ("
      , "  SELECT t.id"
      , "    FROM tasks t"
      , "    JOIN seeds s ON s.id = t.id"
      , "   WHERE t.deleted_at IS NULL"
      , "  UNION"
      , "  SELECT id FROM dependency_dependents"
      , "),"
      , "affected_tasks(id, parent_id) AS ("
      , "  SELECT t.id, t.parent_id"
      , "    FROM tasks t"
      , "    JOIN direct_targets dt ON dt.id = t.id"
      , "   WHERE t.deleted_at IS NULL"
      , "  UNION"
      , "  SELECT parent.id, parent.parent_id"
      , "    FROM tasks parent"
      , "    JOIN affected_tasks child ON child.parent_id = parent.id"
      , "   WHERE parent.deleted_at IS NULL"
      , "),"
      , "targets(id) AS ("
      , "  SELECT DISTINCT id FROM affected_tasks"
      , ")"
      , "SELECT t.id, t.workspace_id, t.project_id, t.parent_id, t.title, t.description,"
      , "       t.status::text, t.priority, t.metadata, t.due_at, t.completed_at,"
      , "       t.created_at, t.updated_at,"
      , "       t.auto_blocked,"
      , "       coalesce(open_dep.open_dependency_count, 0)::bigint"
      , "  FROM tasks t"
      , "  JOIN targets target ON target.id = t.id"
      , "  LEFT JOIN LATERAL ("
      , "    SELECT count(DISTINCT dep.id)::bigint AS open_dependency_count"
      , "      FROM task_dependencies dep_link"
      , "      JOIN tasks dep ON dep.id = dep_link.depends_on_id"
      , "     WHERE dep_link.task_id = t.id"
      , "       AND dep.deleted_at IS NULL"
      , "       AND hmem_is_open_task_status(dep.status)"
      , "  ) open_dep ON true"
      , " WHERE t.deleted_at IS NULL"
      , "   AND NOT hmem_task_is_inside_closed_project(t.id)"
      , "   AND NOT hmem_task_has_done_ancestor(t.id)"
      , " ORDER BY t.created_at ASC, t.id ASC"
      ]
    encoder = Enc.param (Enc.nonNullable (Enc.foldableArray (Enc.nonNullable Enc.uuid)))
    decoder = Dec.rowList dependencyAutoBlockSnapshotRowDecoder

dependencyAutoBlockSnapshotRowDecoder :: Dec.Row TaskDependencyAutoBlockSnapshot
dependencyAutoBlockSnapshotRowDecoder = do
  task <- rawTaskRowDecoder
  autoBlocked <- Dec.column (Dec.nonNullable Dec.bool)
  openDependencyCount <- Dec.column (Dec.nonNullable Dec.int8)
  pure TaskDependencyAutoBlockSnapshot
    { task = task
    , autoBlocked = autoBlocked
    , openDependencyCount = fromIntegral openDependencyCount
    }

------------------------------------------------------------------------
-- Dependencies
------------------------------------------------------------------------

addDependency :: Pool Hasql.Connection -> UUID -> UUID -> IO ()
addDependency pool tid depId = void $ addDependencyWithSnapshots pool tid depId

addDependencyWithSnapshots :: Pool Hasql.Connection -> UUID -> UUID -> IO ([TaskDependencyAutoBlockSnapshot], [TaskDependencyAutoBlockSnapshot])
addDependencyWithSnapshots pool tid depId =
  runTransaction pool $ do
    before <- dependencyAutoBlockSnapshotsS [tid, depId]
    addDependencyS tid depId
    after <- dependencyAutoBlockSnapshotsS [tid, depId]
    pure (before, after)

addDependencyS :: UUID -> UUID -> Session.Session ()
addDependencyS tid depId =
  Session.statement () $ run_ $
    insert Insert
      { into = taskDependencySchema
      , rows = values
          [ TaskDependencyT
              { tdTaskId      = lit tid
              , tdDependsOnId = lit depId
              }
          ]
      , onConflict = DoNothing
      , returning = NoReturning
      }

removeDependency :: Pool Hasql.Connection -> UUID -> UUID -> IO ()
removeDependency pool tid depId = void $ removeDependencyWithSnapshots pool tid depId

removeDependencyWithSnapshots :: Pool Hasql.Connection -> UUID -> UUID -> IO ([TaskDependencyAutoBlockSnapshot], [TaskDependencyAutoBlockSnapshot])
removeDependencyWithSnapshots pool tid depId =
  runTransaction pool $ do
    before <- dependencyAutoBlockSnapshotsS [tid, depId]
    removeDependencyS tid depId
    after <- dependencyAutoBlockSnapshotsS [tid, depId]
    pure (before, after)

removeDependencyS :: UUID -> UUID -> Session.Session ()
removeDependencyS tid depId =
  Session.statement () $ run_ $
    delete Delete
      { from = taskDependencySchema
      , using = pure ()
      , deleteWhere = \_ row -> row.tdTaskId ==. lit tid &&. row.tdDependsOnId ==. lit depId
      , returning = NoReturning
      }
