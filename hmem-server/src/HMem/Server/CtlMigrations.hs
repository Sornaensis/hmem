{-# LANGUAGE TemplateHaskell #-}

-- | Native migration installation uses the build's source bundle, rather than
-- a mutable Cabal data directory which can retain files removed by an upgrade.
module HMem.Server.CtlMigrations
  ( bundledMigrations
  , installBundledMigrations
  , installMigrationBundle
  ) where

import Control.Exception (bracket)
import Control.Monad (forM_, unless, when)
import Data.ByteString qualified as BS
import Data.Char (isDigit)
import Data.FileEmbed (embedDir, makeRelativeToProject)
import Data.List (nub, sortOn)
import System.Directory
  ( createDirectoryIfMissing, doesDirectoryExist, doesFileExist
  , listDirectory, removeFile, renameFile
  )
import System.FilePath ((</>), takeExtension, takeFileName)
import System.IO (hClose, openBinaryTempFile)
import Text.Read (readMaybe)

bundledMigrations :: [(FilePath, BS.ByteString)]
bundledMigrations = $(makeRelativeToProject "migrations" >>= embedDir)

installBundledMigrations :: FilePath -> IO (FilePath, [FilePath])
installBundledMigrations hmemDir = do
  let destination = hmemDir </> "migrations"
  files <- installMigrationBundle bundledMigrations destination
  pure (destination, files)

-- | Validate the complete inventory before touching the installation. Only
-- versioned migration SQL belongs to this installer; other entries survive.
installMigrationBundle :: [(FilePath, BS.ByteString)] -> FilePath -> IO [FilePath]
installMigrationBundle bundle destination = do
  let ordered = sortOn fst bundle
      names = map fst ordered
      versions = traverse migrationVersion names
      invalid = ioError (userError "Invalid bundled migration inventory")
  case versions of
    Just values | not (null values), length (nub values) == length values
      , all (not . BS.null . snd) ordered -> pure ()
    _ -> invalid
  -- Refuse directory collisions before publishing even the first file.
  forM_ names $ \name -> do
    directory <- doesDirectoryExist (destination </> name)
    when directory invalid
  createDirectoryIfMissing True destination
  existing <- listDirectory destination
  forM_ ordered $ \(name, bytes) ->
    bracket (openBinaryTempFile destination ".hmem-migration-")
      (\(temporary, handle) -> do
        hClose handle
        exists <- doesFileExist temporary
        when exists (removeFile temporary)) $ \(temporary, handle) -> do
          BS.hPut handle bytes
          hClose handle
          renameFile temporary (destination </> name)
  -- Retire obsolete names only after every canonical file was published.
  forM_ existing $ \name ->
    when (migrationVersion name /= Nothing && name `notElem` names) $ do
      let path = destination </> name
      regular <- doesFileExist path
      unless regular (ioError (userError "Obsolete migration is not a regular file"))
      removeFile path
  pure names

migrationVersion :: FilePath -> Maybe Integer
migrationVersion name
  | takeFileName name /= name || takeExtension name /= ".sql" = Nothing
migrationVersion ('V':rest) = case span isDigit rest of
  (digits, '_':'_':description)
    | not (null digits), length description > 4 -> readMaybe digits
  _ -> Nothing
migrationVersion _ = Nothing
