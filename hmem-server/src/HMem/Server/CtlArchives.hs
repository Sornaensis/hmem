module HMem.Server.CtlArchives
  ( ArchivesOperation(..), runArchivesOperation, runArchivesOperationWithPool ) where

import Control.Exception (bracket)
import Data.Aeson (Value)
import Data.Pool (Pool,destroyAllResources)
import Hasql.Connection qualified as H
import Hasql.Decoders qualified as D
import Hasql.Encoders qualified as E
import Hasql.Session qualified as S
import Hasql.Statement qualified as St

import HMem.Config (HMemConfig(..),PoolConfig(..),connectionString,loadConfig)
import HMem.DB.Pool qualified as P
import HMem.DB.SessionArchive.Maintenance qualified as M

data ArchivesOperation = ArchivesStatus | ArchivesConfigure M.MaintenanceAction deriving stock (Show,Eq)

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
    ArchivesConfigure M.EnableReclamation -> do
      safe<-P.runSession pool $ S.statement () $ St.Statement
        "SELECT public.hmem_archive_runtime_safe()"
        E.noParams (D.singleRow (D.column (D.nonNullable D.bool))) True
      if safe then M.configureMaintenance pool M.EnableReclamation
        else fail "Archive reclamation requires a restricted runtime role with the explicit maintenance capability grant"
    ArchivesConfigure action -> M.configureMaintenance pool action
  M.maintenanceStatus pool
