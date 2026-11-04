module HMem.Server.CtlArchivesCli
  ( ArchivesCommand(..), archivesCommandParser, archivesCommandInfo
  , runArchivesCommand, runArchivesCommandWith
  ) where

import Data.Aeson (Value,encode)
import Control.Exception (fromException)
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.Int (Int64)
import Data.UUID qualified as UUID
import Options.Applicative
import System.Exit (ExitCode(..))
import System.IO (stderr)
import Text.Read (readMaybe)

import HMem.DB.SessionArchive.Maintenance qualified as M
import HMem.SessionArchive.Replacement qualified as Replacement
import HMem.Server.CtlArchives
import HMem.Server.Exception (trySynchronous)

data ArchivesCommand = ArchivesCommand { operation :: !ArchivesOperation, json :: !Bool }
  deriving stock (Show,Eq)

archivesCommandParser :: Parser ArchivesCommand
archivesCommandParser=ArchivesCommand <$> subparser
  (command "status" (info (pure ArchivesStatus) (progDesc "Show durable controls, progress, leases and configured capacity"))
   <> command "pause" (configured M.Pause "Stop new claims; drain the current bounded attempt")
   <> command "resume" (configured M.Resume "Resume eligible claims")
   <> command "backfill-enable" (qualified M.EnableBackfill "Enable historical discovery after qualification")
   <> command "backfill-disable" (configured M.DisableBackfill "Disable historical discovery")
   <> command "reclaim-enable" (qualified M.EnableReclamation "Enable verified raw-row reclamation after qualification")
   <> command "reclaim-disable" (configured M.DisableReclamation "Disable exact and sanitized raw-row reclamation")
   <> command "sanitized-reclaim-enable" (qualified M.EnableSanitizedReclamation "Enable verified sanitized raw-row reclamation after separate qualification")
   <> command "sanitized-reclaim-disable" (configured M.DisableSanitizedReclamation "Disable sanitized raw-row reclamation")
   <> command "replacement-install" (info (ArchivesInstallReplacement
      <$> strOption (long "policy-file"<>metavar "PATH") <* qualification)
      (progDesc "Install a validated replacement policy after sanitized lifecycle qualification"))
   <> command "replacement-disable" (configured M.DisableReplacement "Disable replacement eligibility for new archive jobs")
   <> command "capacity" (info (ArchivesConfigure <$> (M.Capacity
      <$> option capacityReader (long "staging-bytes"<>metavar "BYTES")
      <*> option capacityReader (long "wal-bytes"<>metavar "BYTES")))
      (progDesc "Set configured logical staging and WAL headroom allowances"))
   <> command "retry" (info (ArchivesConfigure <$> (M.Retry
      <$> option uuidReader (long "workspace"<>metavar "UUID")
      <*> option uuidReader (long "session"<>metavar "UUID")))
      (progDesc "Retry a failed compression or maintenance verification")))
  <*> switch (long "json"<>help "Emit JSON controls and progress")
  where
    configured action description=info (pure (ArchivesConfigure action)) (progDesc description)
    qualified action description=info (ArchivesConfigure action <$ qualification) (progDesc description)
    qualification=flag' ()
      (long "qualified"<>help "Acknowledge compatible readers, full-object qualification and verified backups")
    capacityReader=eitherReader $ \raw -> case readMaybe raw :: Maybe Integer of
      Just n | n>=0 && n<=toInteger (maxBound::Int64) -> Right (fromInteger n)
      _ -> Left "expected a nonnegative signed 64-bit byte allowance"
    uuidReader=eitherReader $ maybe (Left "expected an exact UUID") Right . UUID.fromString

archivesCommandInfo :: (ArchivesCommand -> a) -> ParserInfo a
archivesCommandInfo wrap=info ((wrap <$> archivesCommandParser)<**>helper)
  (fullDesc<>progDesc "Manage sealed archive maintenance; historical backfill and raw reclamation default off")

runArchivesCommandWith :: (ArchivesOperation -> IO Value) -> ArchivesCommand -> IO (ExitCode,BL.ByteString,BL.ByteString)
runArchivesCommandWith run command=do
  result<-trySynchronous (run command.operation)
  pure $ case result of
    Left failure | Just Replacement.InvalidPolicy <- fromException failure ->
      (ExitFailure 1,"","Invalid replacement policy. Use a valid UTF-8 version 1 policy of at most 64 KiB.\n")
    Left _ -> (ExitFailure 1,"","Archive control failed: verify migration, role/capability grants and configured database.\n")
    Right value -> (ExitSuccess,(if command.json then "" else "Archive maintenance: ")<>encode value<>"\n","")

runArchivesCommand :: ArchivesCommand -> IO ExitCode
runArchivesCommand command=do
  (code,out,err)<-runArchivesCommandWith runArchivesOperation command
  BL.putStr out
  BL.hPutStr stderr err
  pure code
