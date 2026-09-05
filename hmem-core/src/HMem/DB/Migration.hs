module HMem.DB.Migration
  ( runMigrations
  , rollbackMigration
  , MigrationResult(..)
  ) where

import Control.Exception (SomeException, try)
import Data.ByteString qualified as BS
import Data.Functor.Contravariant (contramap)
import Data.Int (Int32)
import Data.List (isPrefixOf, sort)
import Data.Pool (Pool)
import Data.Text qualified as T
import Hasql.Connection qualified as Hasql
import Hasql.Decoders qualified as Dec
import Hasql.Encoders qualified as Enc
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>), takeFileName)

import HMem.DB.Pool (withConn)

data MigrationResult = MigrationResult
  { applied   :: [FilePath]
  , skipped   :: [FilePath]
  , failed    :: Maybe (FilePath, String)
  } deriving (Show)

-- | Run all pending migrations from the given directory.
--
-- Migration files must be named @V\<NNN\>__\<description\>.sql@ where
-- @\<NNN\>@ is a zero-padded integer version number.  The function
-- checks the @schema_migrations@ table for already-applied versions
-- and applies any new ones in order, each inside its own transaction.
--
-- Returns a 'MigrationResult' summarising what happened.
runMigrations :: Pool Hasql.Connection -> FilePath -> IO MigrationResult
runMigrations pool migrationsDir = do
  exists <- doesDirectoryExist migrationsDir
  if not exists
    then pure MigrationResult { applied = [], skipped = [], failed = Nothing }
    else do
      files <- sort . filter isMigration <$> listDirectory migrationsDir
      go files [] []
  where
    isMigration f = "V" `isPrefixOf` f && ".sql" `isSuffixOf'` f

    isSuffixOf' suffix s = drop (length s - length suffix) s == suffix

    go [] acc skAcc = pure MigrationResult
      { applied = reverse acc, skipped = reverse skAcc, failed = Nothing }
    go (f:fs) acc skAcc = do
      let ver  = parseVersion f
          name = takeFileName f
          path = migrationsDir </> f
      case ver of
        Nothing -> go fs acc (f : skAcc)
        Just v  -> do
          appliedStatus <- checkApplied pool v name
          case appliedStatus of
            Left err -> pure MigrationResult
              { applied = reverse acc
              , skipped = reverse skAcc
              , failed = Just (f, err)
              }
            Right True -> go fs acc (f : skAcc)
            Right False -> do
              preflight <-
                if v == 22 && name == canonicalV022Name
                  then runV022CompatibilityPreflight pool
                  else pure (Right ())
              result <- case preflight of
                Left err -> pure $ Left err
                Right () -> applyMigration pool path v name
              case result of
                Left err -> pure MigrationResult
                  { applied = reverse acc
                  , skipped = reverse skAcc
                  , failed  = Just (f, err)
                  }
                Right () -> go fs (f : acc) skAcc

-- | Parse the version number from a filename like @V001__initial.sql@.
parseVersion :: String -> Maybe Int
parseVersion ('V':rest) =
  case span (`elem` ("0123456789" :: String)) rest of
    (digits, '_':'_':_) | not (null digits) -> Just (read digits)
    _ -> Nothing
parseVersion _ = Nothing

-- | V022 adds and backfills a non-endpoint column on every dependency row.
-- The legacy cycle and auto-block triggers nevertheless ran graph traversal
-- for those updates, while the cycle check's UNION ALL enumerated every
-- reconvergent path.  Install bounded, endpoint-scoped definitions before
-- V022 starts, but only for the exact canonical pre-V022 ledger.  The
-- compatibility DDL commits separately so a later V022 failure is safely
-- retryable without reintroducing either slow trigger path.
runV022CompatibilityPreflight :: Pool Hasql.Connection -> IO (Either String ())
runV022CompatibilityPreflight pool = do
  ledger <- readMigrationLedger pool
  case ledger of
    Left err -> pure $ Left $
      "V022 compatibility preflight could not read schema_migrations: " <> err
    Right observed
      | observed /= canonicalPreV022Ledger -> pure $ Left $
          "V022 compatibility preflight refused a non-canonical ledger; "
          <> "expected exactly V001 through V021 with canonical names, observed "
          <> show observed
      | otherwise -> applyV022CompatibilityPreflight pool

readMigrationLedger :: Pool Hasql.Connection -> IO (Either String [(Int32, T.Text)])
readMigrationLedger pool = withConn pool $ \conn -> do
  result <- Session.run (Session.statement () migrationLedgerStatement) conn
  pure $ case result of
    Left err -> Left (show err)
    Right rows -> Right rows

applyV022CompatibilityPreflight :: Pool Hasql.Connection -> IO (Either String ())
applyV022CompatibilityPreflight pool = withConn pool $ \conn -> do
  result <- try (Session.run (Session.sql v022CompatibilitySql) conn)
    :: IO (Either SomeException (Either Session.SessionError ()))
  case result of
    Left ex -> do
      rollbackAbortedTransaction conn
      pure $ Left $ compatibilityFailure (show ex)
    Right (Left err) -> do
      rollbackAbortedTransaction conn
      pure $ Left $ compatibilityFailure (show err)
    Right (Right ()) -> pure (Right ())
  where
    compatibilityFailure err =
      "V022 compatibility preflight failed and was rolled back; "
      <> "the database is unchanged and safe to retry: " <> err

canonicalV022Name :: String
canonicalV022Name = "V022__change_stream_outbox.sql"

canonicalPreV022Ledger :: [(Int32, T.Text)]
canonicalPreV022Ledger =
  [ (1, "V001__initial_schema.sql")
  , (2, "V002__soft_deletes_audit_and_integrity.sql")
  , (3, "V003__saved_views.sql")
  , (4, "V004__project_task_fts.sql")
  , (5, "V005__drop_workspace_path.sql")
  , (6, "V006__auth_schema.sql")
  , (7, "V007__redact_access_token_hash_from_audit.sql")
  , (8, "V008__user_disabled_at.sql")
  , (9, "V009__ignore_access_token_last_used_audit.sql")
  , (10, "V010__audit_log_workspace_query_index.sql")
  , (11, "V011__auth_sessions.sql")
  , (12, "V012__recursive_lifecycle_invariants.sql")
  , (13, "V013__explicit_memory_creation_links.sql")
  , (14, "V014__recursive_task_auto_blocking.sql")
  , (15, "V015__require_explicit_memory_type.sql")
  , (16, "V016__subtasks_completion_gated_not_auto_blocking.sql")
  , (17, "V017__flat_subtask_lifecycle_rules.sql")
  , (18, "V018__flatten_nested_subtasks.sql")
  , (19, "V019__cascade_delete_task_project_subtrees.sql")
  , (20, "V020__replace_memories_with_observations.sql")
  , (21, "V021__observation_subject_sets.sql")
  ]

migrationLedgerStatement :: Statement.Statement () [(Int32, T.Text)]
migrationLedgerStatement = Statement.Statement
  "SELECT version, name FROM schema_migrations ORDER BY version"
  Enc.noParams
  (Dec.rowList ((,) <$> Dec.column (Dec.nonNullable Dec.int4) <*> Dec.column (Dec.nonNullable Dec.text)))
  True

v022CompatibilitySql :: BS.ByteString
v022CompatibilitySql =
  "BEGIN;\n\
  \CREATE OR REPLACE FUNCTION hmem_check_task_dep_cycle()\n\
  \RETURNS TRIGGER AS $$\n\
  \BEGIN\n\
  \  IF EXISTS (\n\
  \    WITH RECURSIVE chain AS (\n\
  \      SELECT depends_on_id AS id\n\
  \        FROM task_dependencies\n\
  \       WHERE task_id = NEW.depends_on_id\n\
  \      UNION\n\
  \      SELECT td.depends_on_id\n\
  \        FROM task_dependencies td\n\
  \        JOIN chain c ON td.task_id = c.id\n\
  \    )\n\
  \    SELECT 1 FROM chain WHERE id = NEW.task_id\n\
  \  ) THEN\n\
  \    RAISE EXCEPTION 'Cycle detected in task dependencies' USING ERRCODE = 'HD301';\n\
  \  END IF;\n\
  \  RETURN NEW;\n\
  \END;\n\
  \$$ LANGUAGE plpgsql;\n\
  \DROP TRIGGER IF EXISTS trg_task_dep_no_cycle ON task_dependencies;\n\
  \CREATE TRIGGER trg_task_dep_no_cycle\n\
  \  BEFORE INSERT OR UPDATE OF task_id, depends_on_id ON task_dependencies\n\
  \  FOR EACH ROW EXECUTE FUNCTION hmem_check_task_dep_cycle();\n\
  \DROP TRIGGER IF EXISTS trg_task_auto_blocking_from_dependency ON task_dependencies;\n\
  \CREATE TRIGGER trg_task_auto_blocking_from_dependency\n\
  \  AFTER INSERT OR DELETE OR UPDATE OF task_id, depends_on_id ON task_dependencies\n\
  \  FOR EACH ROW EXECUTE FUNCTION hmem_recompute_task_auto_blocking_from_dependency();\n\
  \COMMIT;\n"

-- | Check whether a migration version has already been applied.
checkApplied :: Pool Hasql.Connection -> Int -> String -> IO (Either String Bool)
checkApplied pool ver expectedName = withConn pool $ \conn -> do
  tableResult <- Session.run (Session.statement () schemaMigrationsExistsStatement) conn
  case tableResult of
    Left err -> pure $ Left $ "Could not inspect schema_migrations: " <> show err
    Right False -> pure $ Right False
    Right True -> do
      let version = fromIntegral ver :: Int32
      result <- Session.run (Session.statement version schemaMigrationNameStatement) conn
      pure $ case result of
        Left err -> Left $ "Could not read schema_migrations: " <> show err
        Right Nothing -> Right False
        Right (Just recordedName)
          | recordedName == T.pack expectedName -> Right True
          | otherwise -> Left $
              "Migration version " <> show ver <> " is already recorded as "
                <> show recordedName <> ", not " <> show expectedName

-- | Apply a single migration file, then record it in @schema_migrations@.
-- Hasql wraps each 'Session.run' in its own transaction, so we combine the
-- migration SQL and the bookkeeping INSERT into a single session to get
-- atomic application.
applyMigration :: Pool Hasql.Connection -> FilePath -> Int -> String -> IO (Either String ())
applyMigration pool path ver name = withConn pool $ \conn -> do
  sqlBytes <- BS.readFile path
  let version = fromIntegral ver :: Int32
      txn = do
        Session.sql sqlBytes
        Session.statement (version, T.pack name) registerMigrationStatement
  result <- try (Session.run txn conn) :: IO (Either SomeException (Either Session.SessionError ()))
  case result of
    Left ex          -> rollbackAbortedTransaction conn >> pure (Left (show ex))
    Right (Left err) -> rollbackAbortedTransaction conn >> pure (Left (show err))
    Right (Right ()) -> pure $ Right ()

-- | Roll back a single migration version by running its rollback script
-- from the given rollbacks directory, then removing the version from
-- @schema_migrations@.
--
-- Rollback files must be named @R\<NNN\>__\<description\>.sql@ where
-- @\<NNN\>@ matches the forward migration version.
--
-- Returns @Left err@ on failure, @Right ()@ on success.
rollbackMigration :: Pool Hasql.Connection -> FilePath -> Int -> IO (Either String ())
rollbackMigration pool rollbacksDir ver = do
  exists <- doesDirectoryExist rollbacksDir
  if not exists
    then pure $ Left $ "Rollback directory not found: " <> rollbacksDir
    else do
      files <- listDirectory rollbacksDir
      let prefix = "R" <> padVersion ver
          match  = filter (\f -> take (length prefix) f == prefix && ".sql" `isSuffix` f) files
      case match of
        []    -> pure $ Left $ "No rollback script found for version " <> show ver
        (f:_) -> do
          let path = rollbacksDir </> f
          withConn pool $ \conn -> do
            sqlBytes <- BS.readFile path
            let version = fromIntegral ver :: Int32
                txn = do
                  Session.sql sqlBytes
                  Session.statement version deregisterMigrationStatement
            result <- try (Session.run txn conn) :: IO (Either SomeException (Either Session.SessionError ()))
            case result of
              Left ex          -> rollbackAbortedTransaction conn >> pure (Left (show ex))
              Right (Left err) -> rollbackAbortedTransaction conn >> pure (Left (show err))
              Right (Right ()) -> pure $ Right ()
  where
    isSuffix suffix s = drop (length s - length suffix) s == suffix
    padVersion v
      | v < 10    = "00" <> show v
      | v < 100   = "0" <> show v
      | otherwise = show v

schemaMigrationsExistsStatement :: Statement.Statement () Bool
schemaMigrationsExistsStatement = Statement.Statement
  "SELECT EXISTS (SELECT 1 FROM information_schema.tables WHERE table_name = 'schema_migrations')"
  Enc.noParams
  (Dec.singleRow (Dec.column (Dec.nonNullable Dec.bool)))
  True

schemaMigrationNameStatement :: Statement.Statement Int32 (Maybe T.Text)
schemaMigrationNameStatement = Statement.Statement
  "SELECT name FROM schema_migrations WHERE version = $1"
  (Enc.param (Enc.nonNullable Enc.int4))
  (Dec.rowMaybe (Dec.column (Dec.nonNullable Dec.text)))
  True

-- | A migration file may start an explicit transaction.  When it fails,
-- PostgreSQL leaves that transaction aborted, so clear it before returning
-- the connection to the pool; otherwise a corrected migration cannot retry.
rollbackAbortedTransaction :: Hasql.Connection -> IO ()
rollbackAbortedTransaction conn = do
  _ <- try (Session.run (Session.sql "ROLLBACK") conn)
    :: IO (Either SomeException (Either Session.SessionError ()))
  pure ()

registerMigrationStatement :: Statement.Statement (Int32, T.Text) ()
registerMigrationStatement = Statement.Statement
  "INSERT INTO schema_migrations (version, name) VALUES ($1, $2) ON CONFLICT (version) DO NOTHING"
  encoder
  Dec.noResult
  True
  where
    encoder =
      contramap fst (Enc.param (Enc.nonNullable Enc.int4))
      <> contramap snd (Enc.param (Enc.nonNullable Enc.text))

deregisterMigrationStatement :: Statement.Statement Int32 ()
deregisterMigrationStatement = Statement.Statement
  "DELETE FROM schema_migrations WHERE version = $1"
  (Enc.param (Enc.nonNullable Enc.int4))
  Dec.noResult
  True
