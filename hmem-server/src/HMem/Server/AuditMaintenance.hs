module HMem.Server.AuditMaintenance
  ( AuditWorker, AuditDrainFailure(..), AuditRunMode(..), startAuditWorker,startAuditWorkerForApplication,startAuditWorkerWith
  , stopAuditWorker,readAuditWorkerStatus,runAuditCycle,runAuditCycleAt,runAuditCycleAtWith
  , capacityRequirements, pressureReason, backupReady ) where

import Control.Concurrent (ThreadId,forkIOWithUnmask)
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception (Exception,SomeException,SomeAsyncException,fromException,mask,throwIO,try)
import Control.Monad (unless)
import Data.Aeson
import Data.IORef
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time
import System.IO (hPutStrLn,stderr)
import System.Timeout (timeout)

import HMem.Config
import HMem.DB.AuditMaintenance qualified as DB
import HMem.DB.AuditMaintenance.Host qualified as Host

data AuditRunMode = ScheduledAudit | ManualAudit deriving stock (Show,Eq)
data AuditWorker = AuditWorker ThreadId (TVar Bool) (MVar (Either SomeException ())) (TVar Value)
instance Show AuditWorker where show (AuditWorker thread _ _ _)="audit worker "<>show thread
newtype AuditDrainFailure = AuditDrainFailure AuditWorker deriving stock (Show)
instance Exception AuditDrainFailure

startAuditWorker :: HMemConfig -> IO AuditWorker
startAuditWorker cfg = startAuditWorkerWith cfg (runAuditCycle cfg ScheduledAudit)

-- The application's raw connection override has not been proven equivalent to
-- the configured maintenance target. Refuse before acquiring either configured
-- connection; ordinary application service may still use its effective target.
startAuditWorkerForApplication :: HMemConfig -> Maybe Text -> IO AuditWorker
startAuditWorkerForApplication cfg override = startAuditWorkerWith cfg $ case override of
  Nothing -> runAuditCycle cfg ScheduledAudit
  Just _ -> pure (DB.reasonValue "application_target_override_unresolved")

-- A failed stop retains this actual worker, its dedicated connection and any
-- native call/handles. No asynchronous exception discards a half-read Hasql
-- protocol or releases native evidence beneath unfinished I/O.
startAuditWorkerWith :: HMemConfig -> IO Value -> IO AuditWorker
startAuditWorkerWith cfg action = mask $ \_ -> do
  stopped<-newTVarIO False
  finished<-newEmptyMVar
  status<-newTVarIO (DB.reasonValue "starting")
  thread<-forkIOWithUnmask $ \unmask -> try (unmask (loop stopped status Nothing)) >>= putMVar finished
  pure (AuditWorker thread stopped finished status)
  where
    loop stopped status previous=do
      quitting<-readTVarIO stopped
      unless quitting $ do
        result<-trySynchronous action
        let value=either (\failure -> object ["reason".=safeFailureReason failure,"failed".=True,"failure_persistence".=("unconfirmed"::Text)]) id result
            reason=DB.valueField "reason" value :: Maybe Text
        atomically (writeTVar status value)
        unless (reason==previous) $ hPutStrLn stderr ("audit-maintenance: "<>T.unpack (fromMaybe "unknown" reason))
        wake<-registerDelay (cfg.auditMaintenance.assessmentIntervalSeconds*1000000)
        atomically $ (readTVar stopped >>= check) `orElse` (readTVar wake >>= check)
        loop stopped status reason

stopAuditWorker :: AuditWorker -> IO ()
stopAuditWorker owner@(AuditWorker _ stopped finished _) = do
  atomically (writeTVar stopped True)
  timeout 5000000 (readMVar finished) >>= \case
    Nothing -> throwIO (AuditDrainFailure owner)
    Just (Left failure) -> throwIO failure
    Just (Right ()) -> pure ()

readAuditWorkerStatus :: AuditWorker -> IO Value
readAuditWorkerStatus (AuditWorker _ _ _ status)=readTVarIO status

trySynchronous :: IO a -> IO (Either SomeException a)
trySynchronous action=try action >>= \case
  Left failure | Just (_::SomeAsyncException)<-fromException failure -> throwIO failure
  result -> pure result

safeFailureReason :: SomeException -> Text
safeFailureReason failure=case fromException failure of
  Just (DB.AuditFailure reason) -> reason
  Nothing -> "maintenance_unexpected_failure"

withFailureRecording :: DB.AuditConnection -> (IORef Bool -> IO Value) -> IO Value
withFailureRecording connection action=do
  claimed<-newIORef False
  trySynchronous (action claimed) >>= \case
    Right value -> pure value
    Left failure -> do
      cycleClaimed<-readIORef claimed
      now<-getCurrentTime
      let outcome persistence=object ["reason".=safeFailureReason failure,"operation".=("assessment"::Text)
            ,"failed".=True,"executed".=False,"at".=now,"failure_persistence".=(persistence::Text)]
      recorded<-trySynchronous (DB.recordAuditOutcome connection (outcome "recorded") cycleClaimed)
      pure (outcome (either (const "unconfirmed") (const "recorded") recorded))

runAuditCycle :: HMemConfig -> AuditRunMode -> IO Value
runAuditCycle cfg mode=runAuditCycleAt cfg mode Nothing

runAuditCycleAt :: HMemConfig -> AuditRunMode -> Maybe UTCTime -> IO Value
runAuditCycleAt=runAuditCycleAtWith (pure ())

runAuditCycleAtWith :: IO () -> HMemConfig -> AuditRunMode -> Maybe UTCTime -> IO Value
runAuditCycleAtWith beforeLock cfg mode suppliedNow
  | not cfg.auditMaintenance.enabled = pure (DB.reasonValue "disabled")
  | Left reason<-DB.maintenanceDatabase cfg = pure (DB.reasonValue reason)
  | otherwise = DB.withAuditConnection cfg $ \connection -> do
      capability<-DB.auditStatus connection
      if DB.valueField "runtime_safe" capability/=Just True then pure (DB.reasonValue "unsupported_server_or_maintenance_role") else do
        beforeLock
        locked<-DB.tryAuditLock connection
        if not locked then pure (DB.reasonValue "maintenance_busy") else withFailureRecording connection $ \claimState -> do
          status<-DB.auditStatus connection
          schedule<-DB.auditSchedule connection cfg.auditMaintenance suppliedNow (mode==ScheduledAudit)
          let claimed=DB.valueField "claimed" schedule==Just True
          writeIORef claimState claimed
          if DB.valueField "paused" status==Just True then record connection claimed (DB.reasonValue "paused") else do
            assessment<-DB.auditAssessment connection
            if DB.valueField "reason" assessment/=Just ("ok"::Text) then
              record connection claimed (object ["reason".=fromMaybe ("assessment_unavailable"::Text) (DB.valueField "reason" assessment),"schedule".=schedule])
            else do
              identityValue<-DB.auditIdentity connection
              case DB.parseValue identityValue of
                Left reason -> record connection claimed (DB.reasonValue reason)
                Right identity -> do
                  nativeResult<-Host.withAuditHost identity $ \owned -> do
                    sampled<-Host.sampleAuditHost owned
                    now<-maybe getCurrentTime pure suppliedNow
                    let capacity=sampled >>= capacityRequirements cfg.auditMaintenance identity.relations
                        pressure=pressureReason cfg.auditMaintenance assessment
                        scheduleReason=fromMaybe "schedule_unknown" (DB.valueField "reason" schedule :: Maybe Text)
                        reclaim=fromMaybe 0 (DB.valueField "heap_toast_reclaimable" assessment :: Maybe Integer)
                        total=fromMaybe 0 (DB.valueField "total_allocated" assessment :: Maybe Integer)
                        heapBloat=threshold cfg.auditMaintenance reclaim total
                        indexBloat=any (\entry -> threshold cfg.auditMaintenance
                          (fromMaybe 0 (DB.valueField "reclaimable" entry)) (fromMaybe 0 (DB.valueField "allocated" entry)))
                          (fromMaybe [] (DB.valueField "indexes" assessment :: Maybe [Value]))
                        recent=any (\lastTime -> diffUTCTime now lastTime<fromIntegral cfg.auditMaintenance.cooldownSeconds)
                          ([t | Just t<-[DB.valueField "last_heap_at" status :: Maybe UTCTime,DB.valueField "last_index_at" status]])
                        backup=backupReady cfg.auditMaintenance identity now (DB.valueField "backup_receipt" status)
                        baseReason=case capacity of
                          Left reason -> reason
                          Right _ -> fromMaybe
                            (if DB.valueField "eligible" schedule/=Just True then scheduleReason
                             else if recent then "cooldown" else if not(heapBloat||indexBloat) then "below_reclaim_threshold" else "eligible") pressure
                        heapEligible=baseReason=="eligible" && heapBloat && cfg.auditMaintenance.heapCompactionEnabled && backup
                        indexEligible=baseReason=="eligible" && indexBloat
                        reason=if baseReason=="eligible" && heapBloat && cfg.auditMaintenance.heapCompactionEnabled && not backup && not indexEligible
                          then "verified_backup_receipt_required" else baseReason
                        value=object ["reason".=reason,"operation".=("assessment"::Text),"executed".=False,"at".=now
                          ,"schedule".=schedule,"assessment".=assessment,"before_bytes".=total,"after_bytes".=total
                          ,"heap_eligible".=heapEligible,"index_eligible".=indexEligible
                          ,"headroom".=either (const ([]::[Value])) id capacity
                          ,"supported_host".=("native_windows_pg18_same_sid_and_logon"::Text)]
                    record connection claimed value
                  case nativeResult of
                    Left reason -> record connection claimed (object ["reason".=reason,"schedule".=schedule,"assessment".=assessment])
                    Right value -> pure value
  where
    record connection claimed value=DB.recordAuditOutcome connection value claimed >> pure value

threshold :: AuditMaintenanceConfig -> Integer -> Integer -> Bool
threshold cfg reclaim allocated = allocated>0 && reclaim>=toInteger cfg.minimumReclaimBytes
  && fromInteger reclaim/fromInteger allocated>=cfg.minimumReclaimFraction

capacityRequirements :: AuditMaintenanceConfig -> [Host.AuditRelation] -> Host.HostCapacity -> Either Text [Value]
capacityRequirements cfg relations capacity
  | length mapping/=length relations+2 || null volumes || any (\i->i<0||i>=length volumes) mapping = Left "native_host_capacity_unknown"
  | any (\relation -> relation.allocated<0) relations = Left "relation_size_unknown"
  | otherwise = case mapping of
      dataVolume:walVolume:relationVolumes ->
        let total=sum (map (toInteger . (.allocated)) relations)
            initial=Map.fromListWith (+) [(dataVolume,0),(walVolume,3*total)]
            requirements=foldr (\(relation,volume)->Map.insertWith (+) volume (2*toInteger relation.allocated)) initial (zip relations relationVolumes)
            rows=[(volumes!!volume,required+toInteger cfg.minimumFreeBytes) | (volume,required)<-Map.toList requirements]
        in if any (\(volume,required)->volume.available<required) rows then Left "insufficient_physical_headroom"
          else Right [object ["volume".=volume.key,"available".=volume.available,"total_free".=volume.totalFree,"required".=required] | (volume,required)<-rows]
      _ -> Left "native_host_capacity_unknown"
  where mapping=capacity.directoryVolumes; volumes=capacity.volumes

pressureReason :: AuditMaintenanceConfig -> Value -> Maybe Text
pressureReason cfg assessment = do
  let wal=DB.valueField "wal_bytes" assessment :: Maybe Integer
      slots=DB.valueField "slot_retained_bytes" assessment :: Maybe Integer
      archive=DB.valueField "archive_pending_bytes" assessment :: Maybe Integer
      failed=DB.valueField "archive_failed" assessment :: Maybe Bool
  case (wal,slots,archive,failed) of
    (Just w,Just s,Just a,Just f)
      | any (<0) [w,s,a] -> Just "wal_pressure_unknown"
      | w>toInteger cfg.maximumWalBytes -> Just "wal_pressure"
      | s>toInteger cfg.maximumSlotRetentionBytes -> Just "slot_pressure"
      | a>toInteger cfg.maximumArchiveBacklogBytes || f -> Just "archive_pressure"
      | otherwise -> Nothing
    _ -> Just "wal_pressure_unknown"

backupReady :: AuditMaintenanceConfig -> Host.HostIdentity -> UTCTime -> Maybe Value -> Bool
backupReady cfg identity now receipt = case receipt of
  Nothing -> False
  Just value -> DB.valueField "system_identifier" value==Just identity.systemIdentifier
    && DB.valueField "database_oid" value==Just identity.databaseOid
    && DB.valueField "restore_verified" value==Just True
    && maybe False (\stamp->stamp<=now && diffUTCTime now stamp<=fromIntegral cfg.backupMaximumAgeSeconds)
      (DB.valueField "verified_at" value :: Maybe UTCTime)
    && all (\field->maybe False validHash (DB.valueField field value :: Maybe Text)) ["backup_sha256","restore_evidence_sha256"]
  where validHash text=T.length text==64&&T.all (\c->c>='0'&&c<='9'||c>='a'&&c<='f') text
