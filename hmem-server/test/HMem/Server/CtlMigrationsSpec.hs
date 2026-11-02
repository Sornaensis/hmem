module HMem.Server.CtlMigrationsSpec (spec) where

import Control.Exception (bracket)
import Data.ByteString qualified as BS
import Data.List (sort)
import System.Directory (createDirectoryIfMissing, listDirectory, withCurrentDirectory)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.FilePath ((</>))
import Test.Hspec

import HMem.DB.TestHarness (TestSandbox(..), withTestSandbox)
import HMem.Server.CtlMigrations

spec :: Spec
spec = describe "native migration bundle installation" $ do
  it "ignores stale mutable package files and removes the obsolete installed V053" $
    withTestSandbox $ \sandbox -> do
      let home = sandbox.sandboxTmpDir </> "home"
          destination = home </> "migrations"
          stalePackage = sandbox.sandboxTmpDir </> "package"
          staleName = "V053__bounded_outbox_retention.sql"
      createDirectoryIfMissing True destination
      createDirectoryIfMissing True (stalePackage </> "migrations")
      BS.writeFile (destination </> staleName) "stale installed SQL"
      BS.writeFile (stalePackage </> "migrations" </> staleName) "stale packaged SQL"
      BS.writeFile (destination </> "operator-notes.txt") "keep notes"
      BS.writeFile (destination </> "Vnotes.sql") "keep unversioned SQL"
      (_, installed) <- withPackageDataDirectory stalePackage $
        withCurrentDirectory stalePackage (installBundledMigrations home)
      mapM_ (\name -> installed `shouldContain` [name])
        ["V053__session_runtime_harness.sql", "V055__bounded_outbox_retention.sql"]
      installed `shouldNotContain` [staleName]
      actual <- listDirectory destination
      sort actual `shouldBe` sort (installed <> ["operator-notes.txt", "Vnotes.sql"])
      BS.readFile (destination </> "operator-notes.txt") `shouldReturn` "keep notes"
      BS.readFile (destination </> "Vnotes.sql") `shouldReturn` "keep unversioned SQL"
      mapM_ (\(name, bytes) -> BS.readFile (destination </> name) `shouldReturn` bytes) bundledMigrations
      -- A repeated refresh, including from the destination itself, never
      -- deletes the source of a migration before trying to read it.
      withPackageDataDirectory destination $
        withCurrentDirectory destination (installBundledMigrations home)
        `shouldReturn` (destination, installed)
      mapM_ (\(name, bytes) -> BS.readFile (destination </> name) `shouldReturn` bytes) bundledMigrations

  it "rejects duplicate versions and unsafe names without changing existing files" $
    withTestSandbox $ \sandbox -> do
      let destination = sandbox.sandboxTmpDir </> "migrations"
          original = "V001__original.sql"
      createDirectoryIfMissing True destination
      BS.writeFile (destination </> original) "original bytes"
      mapM_ (\bundle -> do
        installMigrationBundle bundle destination `shouldThrow` anyIOException
        listDirectory destination `shouldReturn` [original]
        BS.readFile (destination </> original) `shouldReturn` "original bytes")
        [ []
        , [("V001__first.sql", "one"), ("V001__second.sql", "two")]
        , [("../V002__escape.sql", "escape")]
        , [("V002__empty.sql", "")]
        , [("V002__valid.sql", "valid"), ("notes.txt", "notes")]
        ]

  it "preflights destination directory collisions before replacing existing SQL" $
    withTestSandbox $ \sandbox -> do
      let destination = sandbox.sandboxTmpDir </> "migrations"
      createDirectoryIfMissing True (destination </> "V002__directory.sql")
      BS.writeFile (destination </> "V001__first.sql") "original bytes"
      installMigrationBundle
        [("V001__first.sql", "replacement"), ("V002__directory.sql", "two")]
        destination `shouldThrow` anyIOException
      BS.readFile (destination </> "V001__first.sql") `shouldReturn` "original bytes"

-- Paths_hmem_server would use this mutable directory in the old installer.
-- Keep the override local to the example and restore its exact prior value.
withPackageDataDirectory :: FilePath -> IO a -> IO a
withPackageDataDirectory path action =
  bracket (lookupEnv key) (maybe (unsetEnv key) (setEnv key)) $ \_ -> do
    setEnv key path
    action
  where key = "hmem_server_datadir"
