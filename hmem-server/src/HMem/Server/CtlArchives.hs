module HMem.Server.CtlArchives
  ( ArchivesOperation(..), runArchivesOperation, runArchivesOperationWithPool ) where

import Control.Exception (bracket,throwIO)
import Data.Aeson (Value)
import Data.ByteString qualified as BS
import Data.Pool (Pool,destroyAllResources)
import Hasql.Connection qualified as H
import Hasql.Decoders qualified as D
import Hasql.Encoders qualified as E
import Hasql.Session qualified as S
import Hasql.Statement qualified as St
import System.IO (IOMode(ReadMode),withBinaryFile)

import HMem.Config (HMemConfig(..),PoolConfig(..),connectionString,loadConfig)
import HMem.DB.Pool qualified as P
import HMem.DB.SessionArchive.Maintenance qualified as M
import HMem.SessionArchive.Replacement qualified as Replacement

data ArchivesOperation = ArchivesStatus | ArchivesConfigure M.MaintenanceAction | ArchivesInstallReplacement FilePath
  deriving stock (Show,Eq)

runArchivesOperation :: ArchivesOperation -> IO Value
runArchivesOperation operation=do
  cfg<-loadConfig
  bracket (P.createPool (connectionString cfg.database) 2 cfg.pool.idleTimeout cfg.pool.statementTimeoutMs)
    destroyAllResources (\pool -> runArchivesOperationWithPool pool operation)

-- A bootstrap superuser is useful for migration, but cannot establish the
-- non-assumable-owner runtime boundary required before enabling raw deletion.
runArchivesOperationWithPool :: Pool H.Connection -> ArchivesOperation -> IO Value
runArchivesOperationWithPool pool operation=do
  case operation of
    ArchivesStatus -> pure ()
    ArchivesConfigure M.EnableReclamation -> enable M.EnableReclamation "SELECT public.hmem_archive_runtime_safe()"
    ArchivesConfigure M.EnableSanitizedReclamation -> enable M.EnableSanitizedReclamation "SELECT public.hmem_archive_sanitized_runtime_safe()"
    ArchivesConfigure action -> M.configureMaintenance pool action
    ArchivesInstallReplacement path -> do
      bytes<-withBinaryFile path ReadMode (\handle -> BS.hGet handle 65537)
      policy<-either throwIO pure (Replacement.decodePolicy bytes)
      M.configureMaintenance pool (M.InstallReplacement policy)
  M.maintenanceStatus pool
  where
    enable action sql = do
      safe<-P.runSession pool $ S.statement () $ St.Statement
        sql
        E.noParams (D.singleRow (D.column (D.nonNullable D.bool))) True
      if safe then M.configureMaintenance pool action
        else fail "Archive reclamation requires a restricted runtime role with the explicit maintenance capability grant"
