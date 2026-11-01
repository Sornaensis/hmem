module HMem.Server.CtlAuditMaintenance
  ( AuditControl(..),runAuditControl,runAuditControlWithConfig,publicAuditSettings ) where

import Data.Aeson
import HMem.Config
import HMem.DB.AuditMaintenance qualified as DB
import HMem.Server.AuditMaintenance

data AuditControl = AuditStatus | AuditPause | AuditResume | AuditRunOnce deriving stock (Show,Eq)

publicAuditSettings :: AuditMaintenanceConfig -> Value
publicAuditSettings cfg=object
  ["enabled".=cfg.enabled,"heap_compaction_enabled".=cfg.heapCompactionEnabled
  ,"interval_days".=cfg.intervalDays,"window_start".=cfg.windowStart,"window_end".=cfg.windowEnd,"timezone".=cfg.timezone
  ,"assessment_interval_seconds".=cfg.assessmentIntervalSeconds,"cooldown_seconds".=cfg.cooldownSeconds
  ,"minimum_reclaim_bytes".=cfg.minimumReclaimBytes,"minimum_reclaim_fraction".=cfg.minimumReclaimFraction
  ,"lock_timeout_ms".=cfg.lockTimeoutMs,"maximum_operation_seconds".=cfg.maximumOperationSeconds
  ,"assessment_timeout_ms".=cfg.assessmentTimeoutMs,"minimum_free_bytes".=cfg.minimumFreeBytes]

runAuditControl :: AuditControl -> IO Value
runAuditControl operation=loadConfig >>= \cfg -> runAuditControlWithConfig cfg operation

runAuditControlWithConfig :: HMemConfig -> AuditControl -> IO Value
runAuditControlWithConfig cfg operation=do
  result<-case operation of
    AuditRunOnce -> runAuditCycle cfg ManualAudit
    _ -> case DB.maintenanceDatabase cfg of
      Left reason -> pure (DB.reasonValue reason)
      Right _ -> DB.withAuditConnection cfg $ \connection -> case operation of
        AuditStatus -> DB.auditStatus connection
        AuditPause -> DB.setAuditPaused connection True
        AuditResume -> DB.setAuditPaused connection False
  pure (object ["settings".=publicAuditSettings cfg.auditMaintenance,"result".=result])
