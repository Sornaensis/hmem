module HMem.Server.CtlArchivesCliSpec (spec) where

import Control.Exception (throwIO)
import Data.Aeson (object,(.=))
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.ByteString qualified as BS
import Data.IORef
import Options.Applicative
import System.Exit (ExitCode(..))
import Test.Hspec

import HMem.DB.SessionArchive.Maintenance qualified as M
import HMem.Server.CtlArchives
import HMem.Server.CtlArchivesCli

spec :: Spec
spec=describe "Archive maintenance CLI" $ do
  it "requires explicit qualification and validates capacity arithmetic" $ do
    parsed ["status","--json"] `shouldBe` Right (ArchivesCommand ArchivesStatus True)
    parsed ["pause"] `shouldBe` Right (ArchivesCommand (ArchivesConfigure M.Pause) False)
    parsed ["backfill-enable","--qualified"] `shouldBe` Right (ArchivesCommand (ArchivesConfigure M.EnableBackfill) False)
    parsed ["reclaim-enable"] `shouldSatisfy` isLeft
    parsed ["backfill-enable"] `shouldSatisfy` isLeft
    parsed ["capacity","--staging-bytes","-1","--wal-bytes","0"] `shouldSatisfy` isLeft
    parsed ["capacity","--staging-bytes","9223372036854775808","--wal-bytes","0"] `shouldSatisfy` isLeft
  it "dispatches once and emits machine-readable status or a bounded secret-safe error" $ do
    calls<-newIORef []
    let invoke op=modifyIORef' calls (op:) >> pure (object ["state".= ("paused"::String)])
    (code,out,err)<-runArchivesCommandWith invoke (ArchivesCommand (ArchivesConfigure M.Pause) True)
    code `shouldBe` ExitSuccess
    out `shouldBe` "{\"state\":\"paused\"}\n"
    err `shouldBe` ""
    readIORef calls `shouldReturn` [ArchivesConfigure M.Pause]
    (failed,_,message)<-runArchivesCommandWith (const (throwIO (userError "private database password"))) (ArchivesCommand ArchivesStatus False)
    failed `shouldBe` ExitFailure 1
    message `shouldSatisfy` (not . BS.isInfixOf "password" . BL.toStrict)
  where
    parsed args=case execParserPure defaultPrefs (archivesCommandInfo id) args of
      Success command -> Right command
      Failure _ -> Left ()
      CompletionInvoked _ -> Left ()
    isLeft Left{}=True
    isLeft Right{}=False
