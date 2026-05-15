module HMem.Server.CtlPathsSpec (spec) where

import HMem.Server.CtlPaths
import System.FilePath (takeFileName)
import Test.Hspec

spec :: Spec
spec = describe "hmem-ctl log paths" $ do
  it "keeps child stdio separate from the rotating server log" $ do
    let logDir = "logs"
    takeFileName (serverRotatingLogPath logDir) `shouldBe` "hmem-server.log"
    takeFileName (serverStdioLogPath logDir) `shouldBe` "hmem-server-stdio.log"
    serverStdioLogPath logDir `shouldNotBe` serverRotatingLogPath logDir
    logPathsAreDistinct logDir `shouldBe` True

  it "uses the real API health endpoint for startup readiness" $ do
    serverHealthPath `shouldBe` "/api/v1/health"
    serverStartupHealthUrl "127.0.0.1" 8420 `shouldBe` "http://127.0.0.1:8420/api/v1/health"
    serverStartupHealthUrl "0.0.0.0" 8420 `shouldBe` "http://127.0.0.1:8420/api/v1/health"
    serverStartupHealthUrl "::" 8420 `shouldBe` "http://[::1]:8420/api/v1/health"
    serverStartupHealthUrl "2001:db8::1" 8420 `shouldBe` "http://[2001:db8::1]:8420/api/v1/health"
    serverStartupHealthUrl "[::1]" 8420 `shouldBe` "http://[::1]:8420/api/v1/health"
