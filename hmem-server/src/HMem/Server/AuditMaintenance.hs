module HMem.Server.AuditMaintenance
  ( AuditWorker, AuditDrainFailure(..), AuditRunMode(..), startAuditWorker,startAuditWorkerForApplication,startAuditWorkerWith
  , stopAuditWorker,drainAuditWorker,readAuditWorkerStatus,runAuditCycle,runAuditCycleAt,runAuditCycleAtWith
  , runAuditCycleWithOwner,runAuditCycleAtWithHooks
  , capacityRequirements, pressureReason, backupReady ) where

import Control.Concurrent (ThreadId,forkIOWithUnmask)
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception (Exception,SomeException,SomeAsyncException,fromException,mask,throwIO,try)
import Control.Monad (unless)
import Data.Aeson
import Data.IORef
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.UUID (UUID)
import Data.Int (Int64)
import Data.List (sortOn)
import Data.Ord (Down(..))
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
import HMem.Server.AuditMaintenance.Operation qualified as Op

data AuditRunMode = ScheduledAudit | ManualAudit deriving stock (Show,Eq)
data AuditWorker = AuditWorker ThreadId (TVar Bool) (MVar (Either SomeException ())) (TVar Value)
instance Show AuditWorker where show (AuditWorker thread _ _ _)="audit worker "<>show thread
newtype AuditDrainFailure = AuditDrainFailure AuditWorker deriving stock (Show)
instance Exception AuditDrainFailure

startAuditWorker :: HMemConfig -> IO AuditWorker
startAuditWorker cfg = startAuditWorkerUsing cfg $ \stopped ->
  runAuditCycleOwned Op.defaultExecutionHooks (pure ()) cfg ScheduledAudit Nothing
    (readTVarIO stopped >>= \closed -> pure(if closed then Just "shutdown" else Nothing)) defaultConsume

-- The application's raw connection override has not been proven equivalent to
-- the configured maintenance target. Refuse before acquiring either configured
-- connection; ordinary application service may still use its effective target.
startAuditWorkerForApplication :: HMemConfig -> Maybe Text -> IO AuditWorker
startAuditWorkerForApplication cfg override = case override of
  Nothing -> startAuditWorker cfg
  Just _ -> startAuditWorkerWith cfg (pure(DB.reasonValue "application_target_override_unresolved"))

-- A failed stop retains this actual worker, its dedicated connection and any
-- native call/handles. No asynchronous exception discards a half-read Hasql
-- protocol or releases native evidence beneath unfinished I/O.
startAuditWorkerWith :: HMemConfig -> IO Value -> IO AuditWorker
startAuditWorkerWith cfg action=startAuditWorkerUsing cfg (const action)

startAuditWorkerUsing :: HMemConfig -> (TVar Bool -> IO Value) -> IO AuditWorker
startAuditWorkerUsing cfg action = mask $ \_ -> do
  stopped<-newTVarIO False
  finished<-newEmptyMVar
  status<-newTVarIO (DB.reasonValue "starting")
  thread<-forkIOWithUnmask $ \unmask -> try (unmask (loop stopped status Nothing)) >>= putMVar finished
  pure (AuditWorker thread stopped finished status)
  where
    loop stopped status previous=do
      quitting<-readTVarIO stopped
      unless quitting $ do
        result<-trySynchronous (action stopped)
        let value=either (\failure -> object ["reason".=safeFailureReason failure,"failed".=True,"failure_persistence".=("unconfirmed"::Text)]) id result
            reason=DB.valueField "reason" value :: Maybe Text
        atomically (writeTVar status value)
        unless (reason==previous) $ hPutStrLn stderr ("audit-maintenance: "<>T.unpack (fromMaybe "unknown" reason))
        wake<-registerDelay (cfg.auditMaintenance.assessmentIntervalSeconds*1000000)
        atomically $ (readTVar stopped >>= check) `orElse` (readTVar wake >>= check)
        loop stopped status reason

stopAuditWorker :: AuditWorker -> IO ()
stopAuditWorker owner = drainAuditWorker owner >>= \case
  Nothing -> throwIO (AuditDrainFailure owner)
  Just (Left failure) -> throwIO failure
  Just (Right ()) -> pure ()

-- Completion and the completed root's exception are separate facts. An async
-- exception stored in the completion cell is still a joined worker.
drainAuditWorker :: AuditWorker -> IO (Maybe(Either SomeException ()))
drainAuditWorker (AuditWorker _ stopped finished _) = do
  atomically (writeTVar stopped True)
  timeout 5000000 (readMVar finished)

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

withFailureRecording :: Op.ExecutionHooks -> HMemConfig -> UUID -> DB.AuditConnection -> (IORef Bool -> IORef(Maybe Value) -> IO Value) -> IO Value
withFailureRecording hooks cfg rootId connection action=do
  claimed<-newIORef False
  physical<-newIORef Nothing
  trySynchronous (action claimed physical) >>= \case
    Right value -> pure value
    Left failure -> do
      cycleClaimed<-readIORef claimed
      now<-getCurrentTime
      admitted<-readIORef physical
      case admitted of
        Nothing -> do
          let outcome persistence=object ["reason".=safeFailureReason failure,"operation".=("assessment"::Text)
                ,"failed".=True,"executed".=False,"at".=now,"failure_persistence".=(persistence::Text)]
          recorded<-trySynchronous (DB.recordAuditOutcome connection (outcome "recorded") cycleClaimed)
          pure(outcome(either(const "unconfirmed")(const "recorded") recorded))
        Just attempt -> do
          let operationId=fromMaybe rootId(DB.valueField "id" attempt)
              outcome persistence=object["reason".=safeFailureReason failure
                ,"operation".=fromMaybe("physical"::Text)(DB.valueField "kind" attempt),"operation_id".=operationId
                ,"failed".=True,"executed".=Null,"uncertain".=True,"unclean".=True,"at".=now
                ,"target_oid".=(DB.valueField "target_oid" attempt::Maybe Int64),"failure_persistence".=(persistence::Text)]
          -- Every admitted-operation exception remains physical/uncertain.
          -- No follow-up SQL is sent on a possibly cancelled/retired protocol.
          recorded<-trySynchronous $ do
            DB.retireAuditConnection connection
            hooks.beforeFailureRecording
            DB.withAuditConnection cfg $ \fresh -> do
              acquired<-DB.tryAuditLock fresh
              if not acquired then do
                DB.markAuditOperationUncertain fresh operationId
                throwIO(DB.AuditFailure "physical_failure_record_pending")
              else do
                current<-DB.auditStatus fresh
                let currentAttempt=DB.valueField "operation" current::Maybe Value
                    currentId=currentAttempt>>=DB.valueField "id"
                    state=currentAttempt>>=DB.valueField "state"
                if currentId/=Just operationId then pure "superseded"
                else if maybe True (`notElem`(["prepared","running","uncertain"]::[Text])) state then pure "already_settled"
                else do
                  DB.markAuditOperationUncertain fresh operationId
                  DB.recordAuditOutcome fresh (outcome "recorded")
                    (cycleClaimed||DB.valueField "scheduled" attempt==Just True)
                  pure "recorded"
          pure(outcome(either(const "unconfirmed") id recorded))

runAuditCycle :: HMemConfig -> AuditRunMode -> IO Value
runAuditCycle cfg mode=runAuditCycleAt cfg mode Nothing

runAuditCycleAt :: HMemConfig -> AuditRunMode -> Maybe UTCTime -> IO Value
runAuditCycleAt=runAuditCycleAtWith (pure ())

runAuditCycleAtWith :: IO () -> HMemConfig -> AuditRunMode -> Maybe UTCTime -> IO Value
runAuditCycleAtWith=runAuditCycleAtWithHooks Op.defaultExecutionHooks

runAuditCycleAtWithHooks :: Op.ExecutionHooks -> IO () -> HMemConfig -> AuditRunMode -> Maybe UTCTime -> IO Value
runAuditCycleAtWithHooks hooks beforeLock cfg mode suppliedNow=
  runAuditCycleOwned hooks beforeLock cfg mode suppliedNow (pure Nothing) defaultConsume

runAuditCycleWithOwner :: HMemConfig -> AuditRunMode -> (Op.AuditRunResult -> IO Value) -> IO Value
runAuditCycleWithOwner cfg mode=runAuditCycleOwned Op.defaultExecutionHooks (pure ()) cfg mode Nothing (pure Nothing)

defaultConsume :: Op.AuditRunResult -> IO Value
defaultConsume (Op.AuditRunJoined value)=either throwIO pure value
defaultConsume (Op.AuditRunPending owner)=Op.quarantineAuditRun (BL.hPutStrLn stderr . encode) owner

runAuditCycleOwned :: Op.ExecutionHooks -> IO () -> HMemConfig -> AuditRunMode -> Maybe UTCTime -> IO (Maybe Text) -> (Op.AuditRunResult -> IO Value) -> IO Value
runAuditCycleOwned hooks beforeLock cfg mode suppliedNow outerStop consume=Op.runOwnedAuditRun (\identity stop ->
  let requested=Op.stopRequested stop >>=maybe outerStop (pure . Just)
  in runAuditCycleDirect hooks beforeLock cfg mode suppliedNow identity requested) consume

runAuditCycleDirect :: Op.ExecutionHooks -> IO () -> HMemConfig -> AuditRunMode -> Maybe UTCTime -> UUID -> IO (Maybe Text) -> IO Value
runAuditCycleDirect hooks beforeLock cfg mode suppliedNow operationId requested
  | not cfg.auditMaintenance.enabled = pure (DB.reasonValue "disabled")
  | Left reason<-DB.maintenanceDatabase cfg = pure (DB.reasonValue reason)
  | otherwise = DB.withAuditConnection cfg $ \connection -> do
      capability<-DB.auditStatus connection
      if DB.valueField "runtime_safe" capability/=Just True then pure (DB.reasonValue "unsupported_server_or_maintenance_role") else do
        beforeLock
        locked<-DB.tryAuditLock connection
        if not locked then pure (DB.reasonValue "maintenance_busy") else withFailureRecording hooks cfg operationId connection $ \claimState physicalState -> do
          status<-DB.auditStatus connection
          let previousAttempt=DB.valueField "operation" status::Maybe Value
          writeIORef physicalState $ case previousAttempt of
            Just attempt | maybe False (`elem`(["prepared","running","uncertain"]::[Text])) (DB.valueField "state" attempt) -> Just attempt
            _ -> Nothing
          previous<-Op.reconcileAuditOperation connection status
          maybe (assess connection status claimState physicalState) pure previous
  where
    record connection claimed value=DB.recordAuditOutcome connection value claimed >> pure value
    assess connection status claimState physicalState=do
      schedule<-DB.auditSchedule connection cfg.auditMaintenance suppliedNow (mode==ScheduledAudit)
      let claimed=DB.valueField "claimed" schedule==Just True
      writeIORef claimState claimed
      stop<-requested
      if DB.valueField "paused" status==Just True then record connection claimed (DB.reasonValue "paused")
      else case stop of
        Just reason -> record connection claimed (DB.reasonValue reason)
        Nothing -> do
          assessment<-DB.auditAssessment connection
          if DB.valueField "reason" assessment/=Just ("ok"::Text) then
            record connection claimed (object ["reason".=fromMaybe ("assessment_unavailable"::Text) (DB.valueField "reason" assessment),"schedule".=schedule])
          else do
            identityValue<-DB.auditIdentity connection
            case DB.parseValue identityValue of
              Left reason -> record connection claimed (DB.reasonValue reason)
              Right identity -> do
                nativeResult<-Host.withAuditHost identity $ \owned -> do
                  stopBeforeSample<-requested
                  case stopBeforeSample of
                    Just reason -> record connection claimed (DB.reasonValue reason)
                    Nothing -> do
                      sampled<-Host.sampleAuditHost owned
                      now<-maybe getCurrentTime pure suppliedNow
                      let settings=cfg.auditMaintenance
                          capacity=sampled >>= capacityRequirements settings identity.relations
                          pressure=pressureReason settings assessment
                          total=fromMaybe 0 (DB.valueField "total_allocated" assessment::Maybe Integer)
                          heapBytes=sum [toInteger relation.allocated|relation<-identity.relations,relation.kind=="heap"||relation.kind=="toast"]
                          heapBloat=threshold settings (fromMaybe 0(DB.valueField "heap_toast_reclaimable" assessment)) heapBytes
                          indexes=fromMaybe [] (DB.valueField "indexes" assessment::Maybe[Value])
                          qualifies entry=DB.valueField "valid" entry==Just True && threshold settings
                            (fromMaybe 0(DB.valueField "reclaimable" entry)) (fromMaybe 0(DB.valueField "allocated" entry))
                          recent stamp=diffUTCTime now stamp<fromIntegral settings.cooldownSeconds
                          heapRecent=maybe False recent (DB.valueField "last_heap_at" status)
                          indexRecent oid=heapRecent||maybe False recent
                            ((DB.valueField "index_cooldowns" status::Maybe Value)>>=DB.valueField (T.pack(show oid)))
                          publicQualifiers=[entry|entry<-indexes,qualifies entry,maybe False (T.isPrefixOf "public.") (DB.valueField "name" entry)]
                          candidates=sortOn (Down . (\(_,entry)->fromMaybe 0(DB.valueField "reclaimable" entry::Maybe Integer)))
                            [(oid,entry)|entry<-publicQualifiers,Just oid<-[DB.valueField "oid" entry::Maybe Int64],not(indexRecent oid)]
                          invalid=any (\relation->relation.kind=="index"&&not relation.valid) identity.relations
                          baseReason=case capacity of
                            Left reason -> reason
                            Right _ -> fromMaybe
                              (if DB.valueField "eligible" schedule/=Just True then fromMaybe "schedule_unknown"(DB.valueField "reason" schedule)
                               else if invalid then "invalid_bound_indexes_operator_remediation" else "eligible") pressure
                          backup=backupReady settings identity now (DB.valueField "backup_receipt" status)
                          heapEligible=baseReason=="eligible"&&heapBloat&&settings.heapCompactionEnabled&&backup&&not heapRecent
                          indexEligible=baseReason=="eligible"&&not(null candidates)
                          reason | baseReason/="eligible"=baseReason
                                 | heapEligible||indexEligible="eligible"
                                 | heapBloat&&settings.heapCompactionEnabled&&not backup="verified_backup_receipt_required"
                                 | heapRecent||not(null publicQualifiers)="cooldown"
                                 | any qualifies indexes="toast_index_requires_heap_rewrite"
                                 | heapBloat&&not settings.heapCompactionEnabled="heap_compaction_disabled"
                                 | otherwise="below_reclaim_threshold"
                          value=object ["reason".=reason,"operation".=("assessment"::Text),"executed".=False,"at".=now
                            ,"schedule".=schedule,"assessment".=assessment,"before_bytes".=total,"after_bytes".=total
                            ,"heap_eligible".=heapEligible,"index_eligible".=indexEligible
                            ,"headroom".=either (const([]::[Value])) id capacity
                            ,"supported_host".=("native_windows_pg18_same_sid_and_logon"::Text)]
                          selected=if heapEligible then case [relation.oid|relation<-identity.relations,relation.kind=="heap"] of
                              [oid] -> Just("heap",oid)
                              _ -> Nothing
                            else case candidates of
                              (oid,_):_ | indexEligible -> Just("index",oid)
                              _ -> Nothing
                      case selected of
                        Nothing -> record connection claimed value
                        Just(kind,target) -> Op.executeAuditOperation hooks Op.AuditExecution
                          {Op.config=cfg,Op.operationId=operationId,Op.requestStop=requested,Op.connection=connection
                          ,Op.native=owned,Op.identity=identity,Op.kind=kind,Op.target=target,Op.scheduled=mode==ScheduledAudit
                          ,Op.schedule=schedule,Op.assessment=assessment,Op.control=status
                          ,Op.admitted=writeIORef physicalState . Just
                          ,Op.checkCapacity=Host.sampleAuditHost owned >>=pure . (\result->case result >>=capacityRequirements settings identity.relations of Left reason->Left reason;Right _->Right())}
                case nativeResult of
                  Left reason -> record connection claimed (object ["reason".=reason,"schedule".=schedule,"assessment".=assessment])
                  Right value -> pure value

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
backupReady=Op.backupReady
