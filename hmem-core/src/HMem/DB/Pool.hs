module HMem.DB.Pool
  ( createPool
  , withConn
  , runSession
  , runTransaction
  , DBException(..)
  , checkPgvector
  ) where

import Control.Exception (Exception, SomeException, throwIO, try)
import Control.Monad (void)
import Data.Pool (Pool, newPool, defaultPoolConfig, setNumStripes, withResource)
import Data.Text.Encoding qualified as TE
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Conc (getNumCapabilities)
import Hasql.Connection qualified as Hasql
import Hasql.Connection.Setting qualified as Setting
import Hasql.Connection.Setting.Connection qualified as ConnSetting
import Hasql.Decoders qualified as D
import Hasql.Encoders qualified as E
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement

import HMem.DB.RequestContext (currentRequestId)

------------------------------------------------------------------------
-- Structured database exceptions
------------------------------------------------------------------------

-- | Typed database exceptions for proper HTTP status mapping.
data DBException
  = DBUniqueViolation Text
  | DBForeignKeyViolation Text
  | DBCheckViolation Text
  | DBCycleDetected Text
  | DBStatementTimeout
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
    classifyCmd (Session.ResultError (Session.ServerError sqlstate msg _ _ _))
      | sqlstate == "23505" = DBUniqueViolation (TE.decodeUtf8Lenient msg)
      | sqlstate == "23503" = DBForeignKeyViolation (TE.decodeUtf8Lenient msg)
      | sqlstate == "23514" = DBCheckViolation (TE.decodeUtf8Lenient msg)
      | sqlstate == "P0001" = DBCycleDetected (TE.decodeUtf8Lenient msg)
      | sqlstate == "57014" = DBStatementTimeout
      | otherwise           = DBOtherError (TE.decodeUtf8Lenient msg)
    classifyCmd other = DBOtherError (T.pack (show other))

------------------------------------------------------------------------
-- Pool creation
------------------------------------------------------------------------

-- | Create a connection pool to PostgreSQL using Hasql.
createPool
  :: Text            -- ^ Connection string, e.g. @"host=localhost dbname=hmem"@
  -> Int             -- ^ Maximum number of connections
  -> Double          -- ^ Idle timeout in seconds
  -> Int             -- ^ Statement timeout in milliseconds
  -> IO (Pool Hasql.Connection)
createPool connStr maxConns idleTimeout stmtTimeoutMs = do
  caps <- getNumCapabilities
  let stripes = max 1 (min maxConns caps)
  newPool $ setNumStripes (Just stripes) $ defaultPoolConfig
    acquire
    Hasql.release
    idleTimeout
    maxConns
  where
    acquire = do
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
            Right _ -> pure conn

-- | Run an action with a connection from the pool.
-- The pool already validates connections on checkout (via the
-- @SET statement_timeout@ in 'acquire'); stale connections throw
-- on first use and are automatically discarded by resource-pool.
withConn :: Pool Hasql.Connection -> (Hasql.Connection -> IO a) -> IO a
withConn pool action = withResource pool action

-- | Run a Hasql Session via a connection pool, throwing a structured
-- 'DBException' on error.
runSession :: Pool Hasql.Connection -> Session.Session a -> IO a
runSession = runManagedSession

-- | Run a Hasql 'Session' inside a database transaction (BEGIN/COMMIT/ROLLBACK).
-- Throws a structured 'DBException' on error.
runTransaction :: Pool Hasql.Connection -> Session.Session a -> IO a
runTransaction = runManagedSession

runManagedSession :: Pool Hasql.Connection -> Session.Session a -> IO a
runManagedSession pool sess = withConn pool $ \conn -> do
  mRequestId <- currentRequestId
  let txn = do
        Session.sql "BEGIN"
        applyRequestIdContext mRequestId
        a <- sess
        Session.sql "COMMIT"
        pure a
  result <- Session.run txn conn
  case result of
    Right a  -> pure a
    Left err -> do
      -- Wrap ROLLBACK in try so that a broken connection doesn't mask
      -- the original error.  withResource will destroy the connection
      -- when throwIO propagates out.
      _ <- try @SomeException $ Session.run (Session.sql "ROLLBACK") conn
      throwIO (classifyError err)

applyRequestIdContext :: Maybe Text -> Session.Session ()
applyRequestIdContext Nothing = pure ()
applyRequestIdContext (Just requestId) =
  void $ Session.statement requestId setRequestIdStatement

setRequestIdStatement :: Statement.Statement Text Text
setRequestIdStatement = Statement.Statement
  "SELECT set_config('hmem.request_id', $1, true)"
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
