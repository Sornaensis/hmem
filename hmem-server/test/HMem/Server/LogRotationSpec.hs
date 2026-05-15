module HMem.Server.LogRotationSpec (spec) where

import Control.Exception (finally)
import HMem.Server.LogRotation
import System.Directory
  ( createDirectory
  , doesFileExist
  , getTemporaryDirectory
  , removeDirectoryRecursive
  , removeFile
  )
import System.FilePath ((</>))
import System.IO (hClose, openTempFile)
import Test.Hspec

spec :: Spec
spec = describe "startup log pre-rotation" $ do
  it "rotates a pre-existing oversized log before the runtime logger opens it" $
    withTempDir $ \dir -> do
      let logPath = dir </> "hmem-server.log"
      writeFile logPath (replicate 20 'x')

      rotated <- preRotateLogFileIfNeeded logPath 10 3

      rotated `shouldBe` True
      doesFileExist logPath `shouldReturn` False
      readFile (logPath <> ".0") `shouldReturn` replicate 20 'x'

  it "shifts existing backups and respects the configured backup count" $
    withTempDir $ \dir -> do
      let logPath = dir </> "hmem-server.log"
      writeFile logPath "current-current"
      writeFile (logPath <> ".0") "previous-zero"
      writeFile (logPath <> ".1") "previous-one"

      rotated <- preRotateLogFileIfNeeded logPath 10 2

      rotated `shouldBe` True
      readFile (logPath <> ".0") `shouldReturn` "current-current"
      readFile (logPath <> ".1") `shouldReturn` "previous-zero"

withTempDir :: (FilePath -> IO a) -> IO a
withTempDir action = do
  base <- getTemporaryDirectory
  (path, h) <- openTempFile base "hmem-log-rotation"
  hClose h
  removeFile path
  createDirectory path
  action path `finally` removeDirectoryRecursive path
