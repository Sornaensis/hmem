module HMem.DB.Task
  ( createTask
  , getTask
  , updateTask
  , deleteTask
  , deleteTaskBatch
  , moveTasksBatch
  , listTasks
  , listTasksWithQuery
  , listTasksByWorkspace
  , addDependency
  , removeDependency
  , linkTaskMemory
  , unlinkTaskMemory
  , linkTaskMemoryBatch
  ) where

import Control.Exception (throwIO)
import Control.Monad (when)
import Data.Aeson (Object, toJSON)
import Data.ByteString.Char8 qualified as BS8
import Data.Functor.Contravariant ((>$<))
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
import HMem.DB.Project qualified as Proj
import HMem.DB.Schema
import HMem.Types

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

rowToTask :: TaskT Result -> Task
rowToTask r = Task
  { id          = r.taskId
  , workspaceId = r.taskWorkspaceId
  , projectId   = r.taskProjectId
  , parentId    = r.taskParentId
  , title       = r.taskTitle
  , description = r.taskDescription
  , status      = r.taskStatus
  , priority    = fromIntegral r.taskPriority
  , metadata    = r.taskMetadata
  , dueAt       = r.taskDueAt
  , completedAt = r.taskCompletedAt
  , createdAt   = r.taskCreatedAt
  , updatedAt   = r.taskUpdatedAt
  }

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

applyFieldUpdateMaybe :: Maybe a -> FieldUpdate a -> Maybe a
applyFieldUpdateMaybe oldValue = \case
  Unchanged -> oldValue
  SetNull -> Nothing
  SetTo value -> Just value

ensureTaskProject :: Pool Hasql.Connection -> UUID -> Maybe UUID -> IO ()
ensureTaskProject _ _ Nothing = pure ()
ensureTaskProject pool workspaceId (Just projectId) = do
  project <- Proj.getProject pool projectId >>= maybe
    (throwIO $ DBForeignKeyViolation "Referenced project does not exist")
    pure
  when (project.workspaceId /= workspaceId) $
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

ensureTaskPlacement :: Pool Hasql.Connection -> UUID -> Maybe UUID -> Maybe UUID -> IO ()
ensureTaskPlacement pool workspaceId projectId parentId = do
  ensureTaskProject pool workspaceId projectId
  mParent <- ensureTaskParent pool workspaceId parentId
  case mParent of
    Nothing -> pure ()
    Just parent ->
      when (parent.projectId /= projectId) $
        throwIO $ DBCheckViolation "Task and parent task must belong to the same project"

------------------------------------------------------------------------
-- Create
------------------------------------------------------------------------

createTask :: Pool Hasql.Connection -> CreateTask -> IO Task
createTask pool ct = do
  ensureTaskPlacement pool ct.workspaceId ct.projectId ct.parentId
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
    (r:_) -> pure . Just $ rowToTask r

------------------------------------------------------------------------
-- Update
------------------------------------------------------------------------

updateTask :: Pool Hasql.Connection -> UUID -> UpdateTask -> IO (Maybe Task)
updateTask pool tid ut = do
  current <- getTask pool tid
  case current of
    Nothing -> pure Nothing
    Just task -> do
      let targetProjectId = applyFieldUpdateMaybe task.projectId ut.projectId
          targetParentId = applyFieldUpdateMaybe task.parentId ut.parentId
      ensureTaskPlacement pool task.workspaceId targetProjectId targetParentId
      runTransaction pool $ do
        when (task.projectId /= targetProjectId) $ do
          ids <- Session.statement tid taskSubtreeIdsStatement
          Session.statement () $ run_ $
            update Update
              { target = taskSchema
              , from = pure ()
              , set = \_ row -> row { taskProjectId = lit targetProjectId }
              , updateWhere = \_ row -> in_ row.taskId (map lit ids) &&. activeTask row
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
          []    -> pure Nothing
          (r:_) -> pure . Just $ rowToTask r

------------------------------------------------------------------------
-- Delete
------------------------------------------------------------------------

deleteTask :: Pool Hasql.Connection -> UUID -> IO Bool
deleteTask pool tid = do
  runTransaction pool $ do
    ids <- Session.statement tid taskSubtreeIdsStatement
    case ids of
      [] -> pure False
      _ -> do
        Session.statement () $ run_ $
          update Update
            { target = taskSchema
            , from = pure ()
            , set = \_ row -> row { taskDeletedAt = deletedNow }
            , updateWhere = \_ row -> in_ row.taskId (map lit ids) &&. activeTask row
            , returning = NoReturning
            }
        Session.statement () $ run_ $
          delete Delete
            { from = taskDependencySchema
            , using = pure ()
            , deleteWhere = \_ row -> in_ row.tdTaskId (map lit ids) ||. in_ row.tdDependsOnId (map lit ids)
            , returning = NoReturning
            }
        Session.statement () $ run_ $
          delete Delete
            { from = taskMemoryLinkSchema
            , using = pure ()
            , deleteWhere = \_ row -> in_ row.tmlTaskId (map lit ids)
            , returning = NoReturning
            }
        pure True

-- | Soft-delete multiple tasks by ID in a single transaction.
-- Does NOT cascade to subtasks (unlike deleteTask).
-- Returns the number of tasks actually deleted.
deleteTaskBatch :: Pool Hasql.Connection -> [UUID] -> IO Int
deleteTaskBatch _pool [] = pure 0
deleteTaskBatch pool ids = do
  runTransaction pool $ do
    n <- Session.statement () $ runN $
      update Update
        { target = taskSchema
        , from = pure ()
        , set = \_ row -> row { taskDeletedAt = deletedNow }
        , updateWhere = \_ row -> in_ row.taskId (map lit ids) &&. activeTask row
        , returning = NoReturning
        }
    -- Cascade dependency and link cleanup
    Session.statement () $ run_ $
      delete Delete
        { from = taskDependencySchema
        , using = pure ()
        , deleteWhere = \_ row -> in_ row.tdTaskId (map lit ids) ||. in_ row.tdDependsOnId (map lit ids)
        , returning = NoReturning
        }
    Session.statement () $ run_ $
      delete Delete
        { from = taskMemoryLinkSchema
        , using = pure ()
        , deleteWhere = \_ row -> in_ row.tmlTaskId (map lit ids)
        , returning = NoReturning
        }
    pure (fromIntegral n)

-- | Move multiple tasks to a new project (or detach from all projects
-- when projectId is Nothing). Returns the number of tasks actually moved.
moveTasksBatch :: Pool Hasql.Connection -> [UUID] -> Maybe UUID -> IO Int
moveTasksBatch _pool [] _ = pure 0
moveTasksBatch pool ids projectId = do
  n <- runSession pool $ Session.statement () $ runN $
    update Update
      { target = taskSchema
      , from = pure ()
      , set = \_ row -> row { taskProjectId = lit projectId }
      , updateWhere = \_ row -> in_ row.taskId (map lit ids) &&. activeTask row
      , returning = NoReturning
      }
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
    , createdAfter = Nothing
    , createdBefore = Nothing
    , updatedAfter = Nothing
    , updatedBefore = Nothing
    , limit = mlimit
    , offset = moffset
    }

listTasksWithQuery :: Pool Hasql.Connection -> TaskListQuery -> IO [Task]
listTasksWithQuery pool tq = do
  let (lim, off) = capPagination tq.limit tq.offset
  rows <- runSession pool $ Session.statement () $ run $ select $
    limit (fromIntegral lim) $ offset (fromIntegral off) $
    orderBy (((\row -> row.taskPriority) >$< desc) <> ((\row -> row.taskCreatedAt) >$< asc)) $ do
      row <- each taskSchema
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
      pure row
  pure $ map rowToTask rows

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
    , createdAfter = Nothing
    , createdBefore = Nothing
    , updatedAfter = Nothing
    , updatedBefore = Nothing
    , limit = mlimit
    , offset = moffset
    }

------------------------------------------------------------------------
-- Dependencies
------------------------------------------------------------------------

addDependency :: Pool Hasql.Connection -> UUID -> UUID -> IO ()
addDependency pool tid depId =
  runSession pool $ Session.statement () $ run_ $
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
removeDependency pool tid depId =
  runSession pool $ Session.statement () $ run_ $
    delete Delete
      { from = taskDependencySchema
      , using = pure ()
      , deleteWhere = \_ row -> row.tdTaskId ==. lit tid &&. row.tdDependsOnId ==. lit depId
      , returning = NoReturning
      }

------------------------------------------------------------------------
-- Memory links
------------------------------------------------------------------------

linkTaskMemory :: Pool Hasql.Connection -> UUID -> UUID -> IO ()
linkTaskMemory pool tid mid =
  runSession pool $ Session.statement () $ run_ $
    insert Insert
      { into = taskMemoryLinkSchema
      , rows = values
          [ TaskMemoryLinkT
              { tmlTaskId   = lit tid
              , tmlMemoryId = lit mid
              }
          ]
      , onConflict = DoNothing
      , returning = NoReturning
      }

unlinkTaskMemory :: Pool Hasql.Connection -> UUID -> UUID -> IO ()
unlinkTaskMemory pool tid mid =
  runSession pool $ Session.statement () $ run_ $
    delete Delete
      { from = taskMemoryLinkSchema
      , using = pure ()
      , deleteWhere = \_ row -> row.tmlTaskId ==. lit tid &&. row.tmlMemoryId ==. lit mid
      , returning = NoReturning
      }

-- | Link multiple memories to a task in a single insert.
-- Idempotent: already-linked pairs are silently skipped.
-- Returns the number of memory IDs submitted.
linkTaskMemoryBatch :: Pool Hasql.Connection -> UUID -> [UUID] -> IO Int
linkTaskMemoryBatch _pool _ [] = pure 0
linkTaskMemoryBatch pool tid mids =
  runSession pool $ do
    Session.statement () $ run_ $
      insert Insert
        { into = taskMemoryLinkSchema
        , rows = values
            [ TaskMemoryLinkT { tmlTaskId = lit tid, tmlMemoryId = lit mid }
            | mid <- mids
            ]
        , onConflict = DoNothing
        , returning = NoReturning
        }
    pure (length mids)
