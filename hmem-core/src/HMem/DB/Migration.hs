module HMem.DB.Migration
  ( runMigrations
  , rollbackMigration
  , MigrationResult(..)
  ) where

import Control.Exception (SomeException, try)
import Data.ByteString qualified as BS
import Data.List (isPrefixOf, sort)
import Data.Pool (Pool)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Hasql.Connection qualified as Hasql
import Hasql.Session qualified as Session
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
          alreadyApplied <- checkApplied pool v
          if alreadyApplied
            then go fs acc (f : skAcc)
            else do
              result <- applyMigration pool path v name
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

-- | Check whether a migration version has already been applied.
checkApplied :: Pool Hasql.Connection -> Int -> IO Bool
checkApplied pool ver = withConn pool $ \conn -> do
  let sql = Session.sql $ "DO $$ BEGIN "
         <> "IF NOT EXISTS (SELECT 1 FROM information_schema.tables "
         <> "WHERE table_name = 'schema_migrations') THEN RETURN; END IF; "
         <> "IF EXISTS (SELECT 1 FROM schema_migrations WHERE version = "
         <> TE.encodeUtf8 (T.pack (show ver))
         <> ") THEN RAISE EXCEPTION 'EXISTS'; END IF; END $$"
  result <- Session.run sql conn
  case result of
    Left _  -> pure True   -- RAISE means it exists (or table check triggered)
    Right _ -> pure False  -- No raise means it doesn't exist

-- | Apply a single migration file, then record it in @schema_migrations@.
-- Hasql wraps each 'Session.run' in its own transaction, so we combine the
-- migration SQL and the bookkeeping INSERT into a single session to get
-- atomic application.
applyMigration :: Pool Hasql.Connection -> FilePath -> Int -> String -> IO (Either String ())
applyMigration pool path ver name = withConn pool $ \conn -> do
  sqlBytes <- BS.readFile path
  let record = "INSERT INTO schema_migrations (version, name) VALUES ("
        <> TE.encodeUtf8 (T.pack (show ver))
        <> ", '"
        <> TE.encodeUtf8 (T.pack (escapeSql name))
        <> "')"
      txn = do
        Session.sql sqlBytes
        Session.sql record
  result <- try (Session.run txn conn) :: IO (Either SomeException (Either Session.SessionError ()))
  case result of
    Left ex          -> pure $ Left (show ex)
    Right (Left err) -> pure $ Left (show err)
    Right (Right ()) -> pure $ Right ()

-- | Escape single quotes in a string for SQL literals.
escapeSql :: String -> String
escapeSql = concatMap (\c -> if c == '\'' then "''" else [c])

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
            let deregister = "DELETE FROM schema_migrations WHERE version = "
                  <> TE.encodeUtf8 (T.pack (show ver))
                txn = do
                  Session.sql sqlBytes
                  Session.sql deregister
            result <- try (Session.run txn conn) :: IO (Either SomeException (Either Session.SessionError ()))
            case result of
              Left ex          -> pure $ Left (show ex)
              Right (Left err) -> pure $ Left (show err)
              Right (Right ()) -> pure $ Right ()
  where
    isSuffix suffix s = drop (length s - length suffix) s == suffix
    padVersion v
      | v < 10    = "00" <> show v
      | v < 100   = "0" <> show v
      | otherwise = show v
