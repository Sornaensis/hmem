module HMem.DB.Task
  ( createTask
  , getTask
  , updateTask
  , updateTaskBatch
  , deleteTask
  , deleteTaskBatch
  , restoreTask
  , moveTasksBatch
  , listTasks
  , listTasksWithQuery
  , listTasksByWorkspace
  , enrichTaskCounts
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
import Data.Functor.Contravariant ((>$<), contramap)
import Data.Int (Int16, Int64)
import Data.Map.Strict qualified as Map
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
  , memoryLinkCount = 0
  , createdAt       = r.taskCreatedAt
  , updatedAt       = r.taskUpdatedAt
  }

-- | Enrich a list of tasks with dependency and memory-link counts.
-- Uses two GROUP BY queries across all task IDs in a single round-trip each.
enrichTaskCounts :: Pool Hasql.Connection -> [Task] -> IO [Task]
enrichTaskCounts _ [] = pure []
enrichTaskCounts pool tasks = do
  let taskIds = map (.id) tasks
  depCounts <- getCountMap pool depCountStatement taskIds
  memCounts <- getCountMap pool memLinkCountStatement taskIds
  pure $ map (\t -> t { dependencyCount = Map.findWithDefault 0 t.id depCounts
                      , memoryLinkCount = Map.findWithDefault 0 t.id memCounts
                      }) tasks

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

memLinkCountStatement :: Statement.Statement [UUID] [(UUID, Int64)]
memLinkCountStatement = Statement.Statement sql encoder decoder True
  where
    sql = BS8.pack $ unlines
      [ "SELECT tml.task_id, COUNT(*)"
      , "FROM task_memory_links tml"
      , "JOIN memories m ON m.id = tml.memory_id AND m.deleted_at IS NULL"
      , "WHERE tml.task_id = ANY($1)"
      , "GROUP BY tml.task_id"
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
      pure $ Just (head enriched)

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
      mTask <- runTransaction pool $ do
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
      case mTask of
        Nothing -> pure Nothing
        Just t -> do
          enriched <- enrichTaskCounts pool [t]
          pure $ Just (head enriched)

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
  let (lim, off) = capPagination tq.limit tq.offset
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
