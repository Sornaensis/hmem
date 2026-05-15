module HMem.Server.LogRotation
  ( preRotateLogFileIfNeeded
  ) where

import Control.Monad (forM_, when)
import System.Directory (doesFileExist, getFileSize, removeFile, renameFile)

-- | Rotate an already-oversized log before opening it with the runtime logger.
--
-- This is intentionally performed before hmem-server creates its rotating file
-- logger. On Windows, attempting to rename an already-open file fails with a
-- sharing violation, so a pre-existing oversized log can otherwise make the
-- process die during startup.
preRotateLogFileIfNeeded :: FilePath -> Integer -> Int -> IO Bool
preRotateLogFileIfNeeded logPath maxBytes backupCount
  | maxBytes <= 0 = pure False
  | otherwise = do
      exists <- doesFileExist logPath
      if not exists
        then pure False
        else do
          size <- getFileSize logPath
          if size < maxBytes
            then pure False
            else do
              rotateExistingLog logPath backupCount
              pure True

rotateExistingLog :: FilePath -> Int -> IO ()
rotateExistingLog logPath backupCount
  | backupCount <= 0 = removeIfExists logPath
  | otherwise = do
      removeIfExists (backupPath (backupCount - 1))
      forM_ (reverse [0 .. backupCount - 2]) $ \idx ->
        moveIfExists (backupPath idx) (backupPath (idx + 1))
      moveIfExists logPath (backupPath 0)
  where
    backupPath idx = logPath <> "." <> show idx

moveIfExists :: FilePath -> FilePath -> IO ()
moveIfExists src dst = do
  exists <- doesFileExist src
  when exists $ do
    removeIfExists dst
    renameFile src dst

removeIfExists :: FilePath -> IO ()
removeIfExists path = do
  exists <- doesFileExist path
  when exists (removeFile path)
