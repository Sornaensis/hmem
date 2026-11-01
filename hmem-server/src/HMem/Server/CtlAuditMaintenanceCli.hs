module HMem.Server.CtlAuditMaintenanceCli
  ( AuditMaintenanceCommand(..),auditMaintenanceCommandParser,auditMaintenanceCommandInfo
  ,runAuditMaintenanceCommand,runAuditMaintenanceCommandWith ) where

import Data.Aeson (Value,encode)
import Data.ByteString.Lazy.Char8 qualified as BL
import Options.Applicative
import System.Exit
import System.IO (stderr)
import Control.Exception (throwIO)
import HMem.Config (loadConfig)
import HMem.Server.CtlAuditMaintenance
import HMem.Server.Exception (trySynchronous)
import HMem.DB.AuditMaintenance qualified as DB
import HMem.Server.AuditMaintenance.Operation qualified as Op

data AuditMaintenanceCommand=AuditMaintenanceCommand {operation :: !AuditControl,json :: !Bool} deriving stock (Show,Eq)
auditMaintenanceCommandParser :: Parser AuditMaintenanceCommand
auditMaintenanceCommandParser=AuditMaintenanceCommand <$> subparser
  (command "status" (info (pure AuditStatus) (progDesc "Show bounded audit maintenance controls and last assessment"))
   <>command "pause" (info (pure AuditPause) (progDesc "Pause new audit maintenance and request bounded drain"))
   <>command "resume" (info (pure AuditResume) (progDesc "Resume eligibility under configured safety gates"))
   <>command "run-once" (info (pure AuditRunOnce) (progDesc "Run one guarded audit maintenance cycle; physical work retains its window and safety gates")))
  <*>switch(long "json"<>help "Emit JSON")
auditMaintenanceCommandInfo :: (AuditMaintenanceCommand -> a) -> ParserInfo a
auditMaintenanceCommandInfo wrap=info ((wrap <$>auditMaintenanceCommandParser)<**>helper)
  (fullDesc<>progDesc "Manage automatic audit maintenance using its separate restricted database login")
runAuditMaintenanceCommandWith :: (AuditControl -> IO Value) -> AuditMaintenanceCommand -> IO (ExitCode,BL.ByteString,BL.ByteString)
runAuditMaintenanceCommandWith run command=trySynchronous (run command.operation) >>= \case
  Left _ -> pure (ExitFailure 1,"","Audit maintenance control failed: verify migration, separate credentials and narrow capability grants.\n")
  Right value -> do
    let failed=command.operation `elem` [AuditRunOnce,AuditPause] && maybe False (\result->DB.valueField "failed" result==Just True)
          (DB.valueField "result" value :: Maybe Value)
    pure (if failed then ExitFailure 1 else ExitSuccess,(if command.json then "" else "Audit maintenance: ")<>encode value<>"\n","")
runAuditMaintenanceCommand :: AuditMaintenanceCommand -> IO ExitCode
runAuditMaintenanceCommand command=do
  cfg<-loadConfig
  -- A one-shot's pending owner is handled before generic failure-to-exit code.
  -- Output failure/repeated cancellation cannot abandon its database children.
  let consume (Op.AuditRunJoined value)=either throwIO pure value
      consume (Op.AuditRunPending owner)=Op.quarantineAuditRun (\value->BL.hPutStr stderr (encode value<>"\n")) owner
  (code,out,err)<-runAuditMaintenanceCommandWith (runAuditControlWithOwner cfg consume) command
  BL.putStr out;BL.hPutStr stderr err;pure code
