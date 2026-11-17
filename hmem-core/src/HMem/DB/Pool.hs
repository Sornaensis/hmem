module HMem.DB.Pool
  ( createPool
  , createPoolWithCheckoutTimeout
  , createDeliveryPool
  , createDeliveryPoolWithCheckoutTimeout
  , defaultCheckoutTimeoutMs
  , destroyDeliveryPool
  , withConn
  , runSession
  , runTransaction
  , runSerializableTransaction
  , DBException(..)
  , checkPgvector
  , PoolMetrics(..)
  , setTestTransactionMode
  , getPoolMetrics
  , getPoolMetricsFor
  , getPoolCheckoutMetricsFor
  , runSessionWithRollbackForTest
  ) where

import Control.Exception (Exception, SomeAsyncException, SomeException, bracket_, finally, fromException, mask, onException, throwIO, try)
import Control.Monad (void, when)
import Data.ByteString (ByteString)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.Pool (Pool, destroyAllResources, destroyResource, newPool, defaultPoolConfig, putResource, setNumStripes, takeResource)
import Data.Pool.Internal qualified as PoolInternal
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import Hasql.Connection qualified as Hasql
import Hasql.Connection.Setting qualified as Setting
import Hasql.Connection.Setting.Connection qualified as ConnSetting
import Hasql.Decoders qualified as D
import Hasql.Encoders qualified as E
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement
import System.IO (hPutStrLn, stderr)
import System.IO.Unsafe (unsafePerformIO)
import System.Timeout (timeout)

import HMem.DB.RequestContext (EffectiveActor(..), RequestContext(..), actorTypeToText, changeCauseToText, currentRequestContext, resolveEffectiveActor)
import HMem.DB.RequestTiming
  ( CheckoutCounters, PoolCheckoutMetrics, RequestTimer, connectCountOn, currentRequestTimer, mainCheckoutCounters, newCheckoutCounters
  , readCheckoutCounters, recordCheckoutOn, recordCheckoutTimeoutOn, recordConnectOn, recordHoldOn, timerConnectNs )
import Data.Aeson (encode)
import Data.ByteString.Lazy qualified as LBS
import Data.Text.Encoding qualified as TE

------------------------------------------------------------------------
-- Pool metrics
------------------------------------------------------------------------

-- | Snapshot of connection pool utilisation.
data PoolMetrics = PoolMetrics
  { activeConnections :: !Int
  , maxConnections    :: !Int
  , pendingCheckouts  :: !Int  -- ^ Checkouts waiting for a connection.
  } deriving (Show, Eq)

-- | Gauges and checkout counters for one set of pools. Pools from
-- 'createPool' share the process-wide main handle; each delivery pool has its
-- own, so its demand neither appears in the main gauges nor changes the main
-- capacity-wait classification.
data PoolHandle = PoolHandle
  { handleActive     :: !(IORef Int)  -- ^ Connections currently checked out.
  , handlePending    :: !(IORef Int)  -- ^ Checkouts waiting for a connection.
  , handleMax        :: !(IORef Int)  -- ^ Configured maximum connections.
  , handleCheckoutTimeoutMs :: !(IORef Int) -- ^ Longest checkout wait before 'DBPoolCheckoutTimeout'.
  , handleLastWarnNs :: !(IORef Word64) -- ^ When the utilisation warning was last printed.
  , handleCounters   :: !CheckoutCounters
  , handleAttributed :: !Bool         -- ^ Whether checkouts count toward request timers.
  , handleLabel      :: !String
  }

-- | Process-wide counter of connections currently checked out of the pool.
{-# NOINLINE activeConnectionsRef #-}
activeConnectionsRef :: IORef Int
activeConnectionsRef = unsafePerformIO (newIORef 0)

-- | Process-wide count of checkouts waiting for a connection.
{-# NOINLINE pendingCheckoutsRef #-}
pendingCheckoutsRef :: IORef Int
pendingCheckoutsRef = unsafePerformIO (newIORef 0)

-- | Configured maximum number of connections (set once in 'createPool').
{-# NOINLINE maxConnectionsRef #-}
maxConnectionsRef :: IORef Int
maxConnectionsRef = unsafePerformIO (newIORef 0)

-- | Main-pool checkout timeout (set in 'createPoolWithCheckoutTimeout').
{-# NOINLINE checkoutTimeoutMsRef #-}
checkoutTimeoutMsRef :: IORef Int
checkoutTimeoutMsRef = unsafePerformIO (newIORef defaultCheckoutTimeoutMs)

{-# NOINLINE mainLastWarnRef #-}
mainLastWarnRef :: IORef Word64
mainLastWarnRef = unsafePerformIO (newIORef 0)

mainHandle :: PoolHandle
mainHandle = PoolHandle activeConnectionsRef pendingCheckoutsRef maxConnectionsRef checkoutTimeoutMsRef mainLastWarnRef
  mainCheckoutCounters True "pool"

-- | Default pool checkout timeout: 60 seconds. It is a safety net against a
-- hung database, not load shedding, and should exceed the statement timeout.
defaultCheckoutTimeoutMs :: Int
defaultCheckoutTimeoutMs = 60000

-- | Delivery pools keyed by their pool identity (resource-pool's reaper
-- reference). Unregistered pools use 'mainHandle'.
{-# NOINLINE deliveryHandlesRef #-}
deliveryHandlesRef :: IORef [(IORef (), PoolHandle)]
deliveryHandlesRef = unsafePerformIO (newIORef [])

poolIdentity :: Pool a -> IORef ()
poolIdentity = PoolInternal.reaperRef

handleFor :: Pool a -> IO PoolHandle
handleFor pool = do
  registered <- readIORef deliveryHandlesRef
  pure $ case registered of
    [] -> mainHandle
    _ -> maybe mainHandle id (lookup (poolIdentity pool) registered)

-- | Read the current main pool metrics.
getPoolMetrics :: IO PoolMetrics
getPoolMetrics = readHandleMetrics mainHandle

-- | Read the metrics of the handle that owns this pool: its own for a delivery
-- pool, otherwise the main pool metrics.
getPoolMetricsFor :: Pool a -> IO PoolMetrics
getPoolMetricsFor pool = handleFor pool >>= readHandleMetrics

getPoolCheckoutMetricsFor :: Pool a -> IO PoolCheckoutMetrics
getPoolCheckoutMetricsFor pool = handleFor pool >>= readCheckoutCounters . (.handleCounters)

readHandleMetrics :: PoolHandle -> IO PoolMetrics
readHandleMetrics handle = PoolMetrics <$> readIORef handle.handleActive <*> readIORef handle.handleMax <*> readIORef handle.handlePending

------------------------------------------------------------------------
-- Test transaction mode
------------------------------------------------------------------------

-- | When enabled, 'runManagedSession' uses SAVEPOINT\/RELEASE instead
-- of BEGIN\/COMMIT so that an outer transaction can wrap an entire
-- test case and roll back all changes at the end.
{-# NOINLINE testTransactionModeRef #-}
testTransactionModeRef :: IORef Bool
testTransactionModeRef = unsafePerformIO (newIORef False)

-- | Counter for generating unique savepoint names.
{-# NOINLINE savepointCounterRef #-}
savepointCounterRef :: IORef Int
savepointCounterRef = unsafePerformIO (newIORef 0)

-- | Enable or disable test transaction mode.
setTestTransactionMode :: Bool -> IO ()
setTestTransactionMode = writeIORef testTransactionModeRef

------------------------------------------------------------------------
-- Structured database exceptions
------------------------------------------------------------------------

-- | Typed database exceptions for proper HTTP status mapping.
data DBException
  = DBUniqueViolation Text
  | DBForeignKeyViolation Text
  | DBCheckViolation Text
  | DBTaskDependencyCycle Text
  | DBLifecycleViolation Text Text (Maybe Text) (Maybe Text)
  | DBStatementTimeout
  | DBPoolCheckoutTimeout  -- ^ No pool connection became free within the checkout timeout.
  | DBSerializationFailure  -- ^ SQLSTATE 40001 or deadlock 40P01; the transaction was rolled back.
  | DBResyncUnauthorized
  | DBCapabilityUnavailable Text
  | DBOtherError Text
  deriving (Show, Eq)

instance Exception DBException

-- | Classify a Hasql SessionError into a structured DBException by
-- extracting the SQLSTATE code directly from the ServerError constructor
-- instead of string-matching on Show output.
classifyError :: Session.SessionError -> DBException
classifyError sessErr = classifyCmd cmdErr
  where
    cmdErr = case sessErr of
      Session.QueryError _ _ ce -> ce
      Session.PipelineError ce  -> ce
    classifyCmd (Session.ResultError (Session.ServerError sqlstate msg detail hint _))
      | Just lifecycleCode <- lifecycleCodeFor sqlstate =
          DBLifecycleViolation lifecycleCode (decode msg) (decode <$> detail) (decode <$> hint)
      | sqlstate == "23505" = DBUniqueViolation (decode msg)
      | sqlstate == "23503" = DBForeignKeyViolation (decode msg)
      | sqlstate == "23514" = DBCheckViolation (decode msg)
      | sqlstate == "HD301" = DBTaskDependencyCycle (decode msg)
       | sqlstate == "57014" = DBStatementTimeout
       -- A deadlock also rolls the transaction back and is retryable.
       | sqlstate == "40001" || sqlstate == "40P01" = DBSerializationFailure
       | sqlstate == "HM501" = DBResyncUnauthorized
      | otherwise           = DBOtherError (decode msg)
    classifyCmd other = DBOtherError (T.pack (show other))

    decode = TE.decodeUtf8Lenient

    lifecycleCodeFor "HM101" = Just "TASK_COMPLETION_BLOCKED"
    lifecycleCodeFor "HM102" = Just "TASK_OPEN_UNDER_DONE_TASK"
    lifecycleCodeFor "HM104" = Just "TASK_SUBTASK_DEPTH_EXCEEDED"
    lifecycleCodeFor "HM105" = Just "TASK_SUBTASK_START_BLOCKED"
    lifecycleCodeFor "HM106" = Just "TASK_OPEN_UNDER_CANCELLED_TASK"
    lifecycleCodeFor "HM107" = Just "TASK_START_BLOCKED_BY_SUBTASKS"
    lifecycleCodeFor "HM108" = Just "TASK_RUNNING_UNDER_PAUSED_PROJECT"
    lifecycleCodeFor "HM109" = Just "TASK_DEPENDENCY_SCOPE_MISMATCH"
    lifecycleCodeFor "HM201" = Just "PROJECT_COMPLETION_BLOCKED"
    lifecycleCodeFor "HM202" = Just "PROJECT_OPEN_UNDER_CLOSED_PROJECT"
    lifecycleCodeFor "HM203" = Just "TASK_OPEN_UNDER_CLOSED_PROJECT"
    lifecycleCodeFor "HM204" = Just "PROJECT_PAUSE_BLOCKED_BY_RUNNING_TASKS"
    lifecycleCodeFor "HM205" = Just "PROJECT_PAUSE_REQUIRES_REOPEN"
    lifecycleCodeFor _       = Nothing


------------------------------------------------------------------------
-- Pool creation
------------------------------------------------------------------------

-- | Create a connection pool to PostgreSQL using Hasql.
--
-- The pool has one stripe, so any idle connection serves any thread and
-- waiting checkouts are served in FIFO order. A stripe per capability would
-- make a checkout wait behind its own stripe's busy connection while others
-- sat idle, and would round each stripe's share up, opening more connections
-- than configured. The process opens at most the configured maximum here.
createPool
  :: Text            -- ^ Connection string, e.g. @"host=localhost dbname=hmem"@
  -> Int             -- ^ Maximum number of connections
  -> Double          -- ^ Idle timeout in seconds
  -> Int             -- ^ Statement timeout in milliseconds
  -> IO (Pool Hasql.Connection)
createPool connStr maxConns idleTimeout stmtTimeoutMs =
  createPoolWithCheckoutTimeout connStr maxConns idleTimeout stmtTimeoutMs defaultCheckoutTimeoutMs

-- | 'createPool' with an explicit checkout timeout in milliseconds.
createPoolWithCheckoutTimeout :: Text -> Int -> Double -> Int -> Int -> IO (Pool Hasql.Connection)
createPoolWithCheckoutTimeout connStr maxConns idleTimeout stmtTimeoutMs checkoutTimeoutMs = do
  atomicModifyIORef' maxConnectionsRef $ \_ -> (maxConns, ())
  atomicModifyIORef' checkoutTimeoutMsRef $ \_ -> (checkoutTimeoutMs, ())
  newPool $ setNumStripes (Just 1) $ defaultPoolConfig
    (acquireConnection mainHandle connStr stmtTimeoutMs)
    Hasql.release
    idleTimeout
    maxConns

-- | Create a single-stripe pool for change-stream delivery, separate from the
-- main pool so delivery never competes with HTTP checkouts. Its gauges and
-- checkout counters are separate from the main pools ('getPoolMetricsFor',
-- 'getPoolCheckoutMetricsFor') and its checkouts and connections are never
-- attributed to request timers. Release it with 'destroyDeliveryPool'.
createDeliveryPool
  :: Text            -- ^ Connection string
  -> Int             -- ^ Maximum number of connections
  -> Double          -- ^ Idle timeout in seconds
  -> Int             -- ^ Statement timeout in milliseconds
  -> IO (Pool Hasql.Connection)
createDeliveryPool connStr maxConns idleTimeout stmtTimeoutMs =
  createDeliveryPoolWithCheckoutTimeout connStr maxConns idleTimeout stmtTimeoutMs defaultCheckoutTimeoutMs

-- | 'createDeliveryPool' with an explicit checkout timeout in milliseconds.
createDeliveryPoolWithCheckoutTimeout :: Text -> Int -> Double -> Int -> Int -> IO (Pool Hasql.Connection)
createDeliveryPoolWithCheckoutTimeout connStr maxConns idleTimeout stmtTimeoutMs checkoutTimeoutMs = do
  handle <- PoolHandle <$> newIORef 0 <*> newIORef 0 <*> newIORef maxConns <*> newIORef checkoutTimeoutMs
    <*> newIORef 0 <*> newCheckoutCounters <*> pure False <*> pure "delivery pool"
  pool <- newPool $ setNumStripes (Just 1) $ defaultPoolConfig
    (acquireConnection handle connStr stmtTimeoutMs)
    Hasql.release
    idleTimeout
    maxConns
  atomicModifyIORef' deliveryHandlesRef $ \registered -> ((poolIdentity pool, handle) : registered, ())
  pure pool

-- | Destroy a delivery pool's idle connections and forget its metrics handle.
-- Call only after every user of the pool has finished.
destroyDeliveryPool :: Pool Hasql.Connection -> IO ()
destroyDeliveryPool pool =
  destroyAllResources pool `finally`
    atomicModifyIORef' deliveryHandlesRef (\registered -> (filter ((/= poolIdentity pool) . fst) registered, ()))

-- resource-pool creates connections on the requesting thread, so a main-pool
-- connection time is attributed to that thread's request.
acquireConnection :: PoolHandle -> Text -> Int -> IO Hasql.Connection
acquireConnection handle connStr stmtTimeoutMs = do
  started <- getMonotonicTimeNSec
  result <- Hasql.acquire [Setting.connection (ConnSetting.string connStr)]
  case result of
    Left err   -> fail $ "Failed to connect to PostgreSQL: " <> show err
    Right conn -> do
      let timeoutSql = "SET statement_timeout = '" <> T.pack (show stmtTimeoutMs) <> "ms'"
      r <- Session.run (Session.sql (TE.encodeUtf8 timeoutSql)) conn
      case r of
        Left _  -> do
          Hasql.release conn
          fail "Failed to set statement timeout"
        Right _ -> do
          finished <- getMonotonicTimeNSec
          timer <- handleTimer handle
          recordConnectOn handle.handleCounters timer (elapsedNs started finished)
          pure conn

handleTimer :: PoolHandle -> IO (Maybe RequestTimer)
handleTimer handle
  | handle.handleAttributed = currentRequestTimer
  | otherwise = pure Nothing

-- | Run an action with a connection from the pool.
-- The pool already validates connections on checkout (via the
-- @SET statement_timeout@ in 'acquire'); stale connections throw
-- on first use and are automatically discarded by resource-pool.
-- Active-connection metrics are updated atomically on checkout/return.
-- Checkout wait (request to hand-over) and hold (hand-over to return) are
-- recorded in the pool's handle (process-wide for main pools) and, for main
-- pools, the calling thread's request timer.
--
-- A checkout that waits longer than the pool's checkout timeout leaves the
-- FIFO queue and throws 'DBPoolCheckoutTimeout'. resource-pool passes a
-- connection released to a waiter that is giving up on to the next waiter,
-- so no connection is leaked.
withConn :: Pool Hasql.Connection -> (Hasql.Connection -> IO a) -> IO a
withConn pool action = do
  handle <- handleFor pool
  timer <- handleTimer handle
  maxAtRequest <- readIORef handle.handleMax
  timeoutMs <- readIORef handle.handleCheckoutTimeoutMs
  connectsBefore <- connectCountOn handle.handleCounters
  connectNsBefore <- timerConnectNs timer
  -- Earlier waiters, including one being handed a released connection, count
  -- as demand so that only a wait below the configured maximum is a capacity
  -- wait.
  queued <- newIORef True
  let leaveQueue = do
        waiting <- atomicModifyIORef' queued (\q -> (False, q))
        when waiting $ atomicModifyIORef' handle.handlePending (\n -> (max 0 (n - 1), ()))
  mask $ \restore -> do
    pendingAtRequest <- atomicModifyIORef' handle.handlePending (\n -> (n + 1, n))
    activeAtRequest <- readIORef handle.handleActive
    requested <- getMonotonicTimeNSec
    flip finally leaveQueue $ do
      -- Still masked: the timeout can interrupt only the blocking wait, never
      -- the hand-over of a connection.
      taken <- timeout (max 1 timeoutMs * 1000) (takeResource pool)
      case taken of
        Nothing -> do
          recordCheckoutTimeoutOn handle.handleCounters
          throwIO DBPoolCheckoutTimeout
        Just (conn, localPool) -> do
          result <- restore (do
              acquired <- getMonotonicTimeNSec
              connectsAfter <- connectCountOn handle.handleCounters
              connectNsAfter <- timerConnectNs timer
              recordCheckoutOn handle.handleCounters timer (elapsedNs requested acquired) (connectNsAfter - connectNsBefore)
                (activeAtRequest + pendingAtRequest < maxAtRequest && connectsAfter == connectsBefore)
              checkedOut handle timer leaveQueue acquired conn)
            `onException` destroyResource pool localPool conn
          putResource localPool conn
          pure result
  where
    -- The checkout becomes active before it stops waiting, so it is always
    -- counted as demand by concurrent requests.
    checkedOut handle timer leaveQueue acquired conn = bracket_
      (do active <- atomicModifyIORef' handle.handleActive $ \n ->
                      let n' = n + 1 in (n', n')
          leaveQueue
          maxC <- readIORef handle.handleMax
          when (maxC > 1 && active * 100 >= maxC * 80) $ do
            now <- getMonotonicTimeNSec
            due <- atomicModifyIORef' handle.handleLastWarnNs $ \lastNs ->
              if lastNs == 0 || elapsedNs lastNs now >= utilisationWarningIntervalNs then (now, True) else (lastNs, False)
            when due $
              hPutStrLn stderr $ "Warning: " <> handle.handleLabel <> " utilisation at "
                <> show (active * 100 `div` maxC) <> "% ("
                <> show active <> "/" <> show maxC <> ")")
      (do atomicModifyIORef' handle.handleActive $ \n -> (max 0 (n - 1), ())
          released <- getMonotonicTimeNSec
          recordHoldOn handle.handleCounters timer (elapsedNs acquired released))
      (action conn)

-- | At most one utilisation warning per pool every ten seconds.
utilisationWarningIntervalNs :: Word64
utilisationWarningIntervalNs = 10000000000

elapsedNs :: Word64 -> Word64 -> Word64
elapsedNs start end
  | end <= start = 0
  | otherwise = end - start

-- | Run a Hasql Session via a connection pool, throwing a structured
-- 'DBException' on error.
runSession :: Pool Hasql.Connection -> Session.Session a -> IO a
runSession = runManagedSession "BEGIN"

-- | Deterministic test seam for the rollback operation.  Both production
-- transaction branches call the same implementation with 'runRollback'.
runSessionWithRollbackForTest
  :: (Hasql.Connection -> ByteString -> IO (Either Session.SessionError ()))
  -> Pool Hasql.Connection
  -> Session.Session a
  -> IO a
runSessionWithRollbackForTest rollback = runManagedSessionWithRollback rollback "BEGIN"

-- | Run a Hasql 'Session' inside a database transaction (BEGIN/COMMIT/ROLLBACK).
-- Throws a structured 'DBException' on error.
runTransaction :: Pool Hasql.Connection -> Session.Session a -> IO a
runTransaction = runManagedSession "BEGIN"

-- | Run a transaction whose isolation level is selected by its BEGIN command,
-- before request-context SET statements issue any query.
runSerializableTransaction :: Pool Hasql.Connection -> Session.Session a -> IO a
runSerializableTransaction = runManagedSession "BEGIN ISOLATION LEVEL SERIALIZABLE"

runManagedSession :: ByteString -> Pool Hasql.Connection -> Session.Session a -> IO a
runManagedSession = runManagedSessionWithRollback runRollback

runManagedSessionWithRollback
  :: (Hasql.Connection -> ByteString -> IO (Either Session.SessionError ()))
  -> ByteString
  -> Pool Hasql.Connection
  -> Session.Session a
  -> IO a
runManagedSessionWithRollback rollback beginSql pool sess = withConn pool $ \conn -> do
  testMode <- readIORef testTransactionModeRef
  reqCtx <- currentRequestContext
  if testMode
    then runWithSavepoint rollback conn reqCtx sess
    else runWithTransaction rollback beginSql conn reqCtx sess

runWithTransaction
  :: (Hasql.Connection -> ByteString -> IO (Either Session.SessionError ()))
  -> ByteString
  -> Hasql.Connection
  -> RequestContext
  -> Session.Session a
  -> IO a
runWithTransaction rollback beginSql conn reqCtx sess = do
  let txn = do
        Session.sql beginSql
        applyRequestContext reqCtx
        a <- sess
        Session.sql "COMMIT"
        pure a
  result <- Session.run txn conn
  case result of
    Right a  -> pure a
    Left err -> rollbackAndThrow rollback conn "ROLLBACK" err

runWithSavepoint
  :: (Hasql.Connection -> ByteString -> IO (Either Session.SessionError ()))
  -> Hasql.Connection
  -> RequestContext
  -> Session.Session a
  -> IO a
runWithSavepoint rollback conn reqCtx sess = do
  spId <- atomicModifyIORef' savepointCounterRef $ \n -> let n' = n + 1 in (n', n')
  let spName = "sp_" <> TE.encodeUtf8 (T.pack (show spId))
      txn = do
        Session.sql $ "SAVEPOINT " <> spName
        applyRequestContext reqCtx
        a <- sess
        Session.sql $ "RELEASE SAVEPOINT " <> spName
        pure a
  result <- Session.run txn conn
  case result of
    Right a  -> pure a
    Left err -> rollbackAndThrow rollback conn ("ROLLBACK TO SAVEPOINT " <> spName) err

runRollback :: Hasql.Connection -> ByteString -> IO (Either Session.SessionError ())
runRollback conn rollbackSql = Session.run (Session.sql rollbackSql) conn

rollbackAndThrow
  :: (Hasql.Connection -> ByteString -> IO (Either Session.SessionError ()))
  -> Hasql.Connection
  -> ByteString
  -> Session.SessionError
  -> IO a
rollbackAndThrow rollback conn rollbackSql originalError = do
  rollbackResult <- try @SomeException (rollback conn rollbackSql)
  case rollbackResult of
    Left rollbackException
      | isAsyncException rollbackException -> throwIO rollbackException
    _ -> throwIO (classifyError originalError)

isAsyncException :: SomeException -> Bool
isAsyncException exceptionValue =
  case fromException exceptionValue :: Maybe SomeAsyncException of
    Just _ -> True
    Nothing -> False

applyRequestContext :: RequestContext -> Session.Session ()
applyRequestContext reqCtx = do
  let mRequestId = case reqCtx of
        RequestContext { requestId = rid } -> rid
      mActor = case reqCtx.effectiveActor of
        Just actor -> Just actor
        Nothing -> fmap (\p -> resolveEffectiveActor False p Nothing) reqCtx.principal
      mWorkspaceId = case reqCtx of
        RequestContext { workspaceId = wsId } -> wsId
      mChangeCause = case reqCtx of
        RequestContext { changeCause = cause } -> cause
  void $ Session.statement (maybe "" id mRequestId) setRequestIdStatement
  maybe clearPrincipalContext applyActorContext mActor
  void $ Session.statement (maybe "" (T.pack . show) mWorkspaceId) setWorkspaceIdStatement
  void $ Session.statement (maybe "core" changeCauseToText mChangeCause) setChangeCauseStatement

applyActorContext :: EffectiveActor -> Session.Session ()
applyActorContext actor = do
  void $ Session.statement (actorTypeToText actor.actorType) setActorTypeStatement
  void $ Session.statement actor.actorId setActorIdStatement
  void $ Session.statement actor.actorLabel setActorLabelStatement
  void $ Session.statement (maybe "" (TE.decodeUtf8 . LBS.toStrict . encode) actor.botIdentity) setBotIdentityStatement

clearPrincipalContext :: Session.Session ()
clearPrincipalContext = do
  void $ Session.statement "" setActorTypeStatement
  void $ Session.statement "" setActorIdStatement
  void $ Session.statement "" setActorLabelStatement
  void $ Session.statement "" setBotIdentityStatement

setBotIdentityStatement :: Statement.Statement Text Text
setBotIdentityStatement = Statement.Statement
  "SELECT set_config('hmem.bot_identity', $1, true)"
  (E.param (E.nonNullable E.text))
  (D.singleRow (D.column (D.nonNullable D.text)))
  True

setRequestIdStatement :: Statement.Statement Text Text
setRequestIdStatement = Statement.Statement
  "SELECT set_config('hmem.request_id', $1, true)"
  (E.param (E.nonNullable E.text))
  (D.singleRow (D.column (D.nonNullable D.text)))
  True

setActorTypeStatement :: Statement.Statement Text Text
setActorTypeStatement = Statement.Statement
  "SELECT set_config('hmem.actor_type', $1, true)"
  (E.param (E.nonNullable E.text))
  (D.singleRow (D.column (D.nonNullable D.text)))
  True

setActorIdStatement :: Statement.Statement Text Text
setActorIdStatement = Statement.Statement
  "SELECT set_config('hmem.actor_id', $1, true)"
  (E.param (E.nonNullable E.text))
  (D.singleRow (D.column (D.nonNullable D.text)))
  True

setActorLabelStatement :: Statement.Statement Text Text
setActorLabelStatement = Statement.Statement
  "SELECT set_config('hmem.actor_label', $1, true)"
  (E.param (E.nonNullable E.text))
  (D.singleRow (D.column (D.nonNullable D.text)))
  True

setWorkspaceIdStatement :: Statement.Statement Text Text
setWorkspaceIdStatement = Statement.Statement
  "SELECT set_config('hmem.workspace_id', $1, true)"
  (E.param (E.nonNullable E.text))
  (D.singleRow (D.column (D.nonNullable D.text)))
  True

setChangeCauseStatement :: Statement.Statement Text Text
setChangeCauseStatement = Statement.Statement
  "SELECT set_config('hmem.change_cause', $1, true)"
  (E.param (E.nonNullable E.text))
  (D.singleRow (D.column (D.nonNullable D.text)))
  True

-- | Check whether the pgvector extension is installed in the database.
checkPgvector :: Pool Hasql.Connection -> IO Bool
checkPgvector pool = withConn pool $ \conn -> do
  let stmt = Statement.Statement
        "SELECT EXISTS (SELECT 1 FROM pg_extension WHERE extname = 'vector')"
        E.noParams
        (D.singleRow (D.column (D.nonNullable D.bool)))
        True
  result <- Session.run (Session.statement () stmt) conn
  pure $ case result of
    Right True -> True
    _          -> False
