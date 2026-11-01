module HMem.Server.CtlAuditMaintenance
  ( AuditControl(..),runAuditControl,runAuditControlWithConfig,runAuditControlWithOwner,publicAuditSettings ) where

import Data.Aeson
import Data.Text (Text)
import Control.Concurrent (threadDelay)
import GHC.Clock (getMonotonicTimeNSec)
import HMem.Config
import HMem.DB.AuditMaintenance qualified as DB
import HMem.Server.AuditMaintenance
import HMem.Server.AuditMaintenance.Operation qualified as Op

data AuditControl = AuditStatus | AuditPause | AuditResume | AuditRunOnce deriving stock (Show,Eq)

publicAuditSettings :: AuditMaintenanceConfig -> Value
publicAuditSettings cfg=object
  ["enabled".=cfg.enabled,"heap_compaction_enabled".=cfg.heapCompactionEnabled
  ,"interval_days".=cfg.intervalDays,"window_start".=cfg.windowStart,"window_end".=cfg.windowEnd,"timezone".=cfg.timezone
  ,"assessment_interval_seconds".=cfg.assessmentIntervalSeconds,"cooldown_seconds".=cfg.cooldownSeconds
  ,"minimum_reclaim_bytes".=cfg.minimumReclaimBytes,"minimum_reclaim_fraction".=cfg.minimumReclaimFraction
  ,"lock_timeout_ms".=cfg.lockTimeoutMs,"maximum_operation_seconds".=cfg.maximumOperationSeconds
  ,"assessment_timeout_ms".=cfg.assessmentTimeoutMs,"minimum_free_bytes".=cfg.minimumFreeBytes
  ,"maximum_wal_bytes".=cfg.maximumWalBytes,"maximum_slot_retention_bytes".=cfg.maximumSlotRetentionBytes
  ,"maximum_archive_backlog_bytes".=cfg.maximumArchiveBacklogBytes,"backup_maximum_age_seconds".=cfg.backupMaximumAgeSeconds]

runAuditControl :: AuditControl -> IO Value
runAuditControl operation=loadConfig >>= \cfg -> runAuditControlWithConfig cfg operation

runAuditControlWithConfig :: HMemConfig -> AuditControl -> IO Value
runAuditControlWithConfig cfg=runControl cfg (runAuditCycle cfg ManualAudit)

runAuditControlWithOwner :: HMemConfig -> (Op.AuditRunResult -> IO Value) -> AuditControl -> IO Value
runAuditControlWithOwner cfg consume=runControl cfg (runAuditCycleWithOwner cfg ManualAudit consume)

runControl :: HMemConfig -> IO Value -> AuditControl -> IO Value
runControl cfg oneShot operation=do
  result<-case operation of
    AuditRunOnce -> oneShot
    _ -> case DB.maintenanceDatabase cfg of
      Left reason -> pure (DB.reasonValue reason)
      Right _ -> DB.withAuditConnection cfg $ \connection -> case operation of
        AuditStatus -> DB.auditStatus connection
        AuditPause -> pauseAndDrain connection
        AuditResume -> DB.setAuditPaused connection False
  pure (object ["settings".=publicAuditSettings cfg.auditMaintenance,"result".=result])

pauseAndDrain :: DB.AuditConnection -> IO Value
pauseAndDrain connection=do
  _<-DB.setAuditPaused connection True
  started<-getMonotonicTimeNSec
  let reply drain failed status=object["paused".=True,"drain".=(drain::Text),"failed".=failed
        ,"reason".=((if failed then "cleanup_pending" else "paused")::Text)
        ,"operation_id".=((DB.valueField "operation" status::Maybe Value)>>=DB.valueField @Text "id")]
      wait=do
        acquired<-DB.tryAuditLock connection
        if acquired then do
          status<-DB.auditStatus connection
          let state=(DB.valueField "operation" status::Maybe Value)>>=DB.valueField "state"
              settled=maybe True (`elem`(["succeeded","reconciled","cancelled","failed","partial","deferred"]::[Text])) state
          -- Never retain this reconciliation lock while waiting on the old
          -- owner to reacquire it. Return an honest pending acknowledgement.
          pure(reply (if settled then "joined" else "uncertain") (not settled) status)
        else do
          now<-getMonotonicTimeNSec
          if now-started>=5000000000 then DB.auditStatus connection >>=pure . reply "pending" True
            else threadDelay 10000>>wait
  wait
