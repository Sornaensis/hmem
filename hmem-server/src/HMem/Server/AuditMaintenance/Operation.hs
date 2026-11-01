module HMem.Server.AuditMaintenance.Operation
  ( OwnedAuditRun,AuditStop,AuditRunResult(..),startOwnedAuditRun,requestAuditStop,stopRequested
  , awaitAuditRun,runOwnedAuditRun,quarantineAuditRun,ownedRunId,withJoinedAuditQuery
  , AuditExecution(..),ExecutionHooks(..),defaultExecutionHooks,executeAuditOperation,reconcileAuditOperation,backupReady ) where

import Control.Concurrent (ThreadId,forkIOWithUnmask)
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Concurrent.Async (Async,asyncWithUnmask,poll,waitCatch)
import Control.Exception (SomeException,SomeAsyncException,mask,try,throwIO,fromException)
import Control.Concurrent (threadDelay)
import Data.Aeson
import Data.Text (Text)
import Data.IORef
import Data.Int (Int64)
import Data.Maybe (fromMaybe,isNothing)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time
import Data.UUID (UUID)
import Data.UUID.V4 qualified as UUID
import Data.Word (Word64)
import System.Timeout (timeout)
import GHC.Clock (getMonotonicTimeNSec)
import HMem.Config
import HMem.DB.AuditMaintenance qualified as DB
import HMem.DB.AuditMaintenance.Host qualified as Host
import HMem.DB.AuditMaintenance.Cancel qualified as Cancel

data AuditExecution=AuditExecution
  { config :: HMemConfig, operationId :: UUID, requestStop :: IO (Maybe Text)
  , connection :: DB.AuditConnection, native :: Host.AuditHost, identity :: Host.HostIdentity
  , checkCapacity :: IO (Either Text ()), kind :: Text, target :: Int64, scheduled :: Bool
  , schedule :: Value, assessment :: Value, control :: Value, admitted :: Value -> IO () }
data ExecutionHooks=ExecutionHooks
  { beforeDispatch :: IO (), beforeCommand :: IO (), beforeCancel :: IO ()
  , monitorResult :: Value -> IO Value, afterCommand :: IO ()
  , beforeSettlement :: IO (), beforeFailureRecording :: IO () }
defaultExecutionHooks :: ExecutionHooks
defaultExecutionHooks=ExecutionHooks (pure ()) (pure ()) (pure ()) pure (pure ()) (pure ()) (pure ())

data ObservationSnapshot=ObservationSnapshot Word64 (Maybe(Word64,Value)) (Maybe(Word64,Either Text ()))
data OperationReadiness=Ready | Waiting | Stop Text

-- One child owns all observer protocol/native samples. It publishes metadata
-- before starting capacity I/O, so a blocked native sample cannot prevent the
-- root from fencing/cancelling the command. No sample child is ever killed;
-- nested query cleanup finishes first, then this child must join before Host use.
withObservationMonitor :: DB.AuditConnection -> UUID -> IO(Maybe Text) -> IO(Either Text ()) -> (Value -> IO Value)
  -> (IO ObservationSnapshot -> IO () -> IO a) -> IO a
withObservationMonitor observer operationId requested capacity transform use=do
  started<-getMonotonicTimeNSec
  metadata<-newTVarIO Nothing
  samples<-newTVarIO Nothing
  stopped<-newTVarIO False
  cause<-newIORef (Nothing::Maybe SomeException)
  let stop=atomically(writeTVar stopped True)
      quitting=readTVarIO stopped
      loop=do
        closed<-quitting
        if closed then pure () else do
          observed<-try @SomeException (DB.monitorAuditOperation observer operationId >>=transform)
          now<-getMonotonicTimeNSec
          let value=either (DB.reasonValue . failureReason) id observed
          atomically(writeTVar metadata (Just(now,value)))
          case observed of
            Left failure -> case fromException failure::Maybe SomeAsyncException of
              Just _ -> writeIORef cause (Just failure)
              Nothing -> pure ()
            Right _ -> do
              ending<-quitting
              stopRequest<-requested
              if ending then pure () else if not(isNothing stopRequest) then threadDelay 20000>>loop else do
                measured<-try @SomeException capacity
                stamp<-getMonotonicTimeNSec
                atomically(writeTVar samples (Just(stamp,either (const(Left "native_capacity_sample_failed")) id measured)))
                case measured of
                  Left failure -> case fromException failure::Maybe SomeAsyncException of
                    Just _ -> writeIORef cause (Just failure)
                    Nothing -> pure ()
                  Right(Left _) -> pure ()
                  Right(Right ()) -> threadDelay 20000>>loop
      snapshot=ObservationSnapshot started <$>readTVarIO metadata <*>readTVarIO samples
  value<-withJoinedAuditQuery loop stop (const(use snapshot stop))
  failure<-readIORef cause
  maybe (pure value) throwIO failure

-- Runs only inside the actual owned root, retaining the singleton and native
-- brackets through every protocol child, cancel transport and reconciliation.
executeAuditOperation :: ExecutionHooks -> AuditExecution -> IO Value
executeAuditOperation hooks execution=mask $ \_ -> do
  let cfg=execution.config
      settings=cfg.auditMaintenance
      publish reason=do
        let value=object ["reason".=reason,"executed".=False,"operation".=execution.kind,"operation_id".=execution.operationId]
        DB.recordAuditOutcome execution.connection value execution.scheduled
        pure value
  initialStop<-execution.requestStop
  if not(isNothing initialStop) then publish(fromMaybe "stopped" initialStop)
    else if not settings.enabled || execution.kind=="heap" && not settings.heapCompactionEnabled
    then publish ("disabled"::Text)
    else if DB.valueField "eligible" execution.schedule/=Just True then publish "window_closed"
    else do
      permitted<-DB.operationPrivileges execution.connection
      if not permitted then publish "maintenance_operation_grants_required" else DB.withAuditConnection cfg $ \observer -> do
        DB.configureOperationBudget observer (min 200 settings.assessmentTimeoutMs)
        capability<-Cancel.capabilityVersion
        case capability of
          Left reason -> publish reason
          Right _ -> do
            now<-getCurrentTime
            if execution.kind=="heap" && not(backupReady settings execution.identity now (DB.valueField "backup_receipt" execution.control))
              then publish "verified_backup_receipt_required"
              else do
                operation<-DB.beginAuditOperation execution.connection execution.operationId execution.kind execution.target execution.scheduled
                let attempt=DB.auditOperationValue operation
                    settle connection result stopReason=hooks.beforeSettlement>>settleOperation connection attempt result stopReason
                execution.admitted attempt
                opened<-DB.withAuditCancel execution.connection $ \capsule -> do
                  initial<-DB.monitorAuditOperation observer execution.operationId
                  case (DB.valueField "at" initial::Maybe UTCTime,DB.valueField "window_end" execution.schedule::Maybe UTCTime) of
                    (Just serverNow,Just closing) -> do
                      let available=floor(diffUTCTime closing serverNow*1000)-1000
                          budget=min (settings.maximumOperationSeconds*1000) available
                      if budget<1000 then do
                        Cancel.finishCancel capsule
                        settle execution.connection (Just(Right False)) (Just "insufficient_window_budget")
                      else do
                        started<-getMonotonicTimeNSec
                        let deadline=started+fromIntegral budget*1000000
                        DB.configureOperationBudget execution.connection budget
                        DB.markAuditOperationRunning execution.connection execution.operationId
                        gate<-newTVarIO Nothing
                        stopped<-newTVarIO Nothing
                        cancelled<-newIORef False
                        nextCancel<-newIORef (0::Word64)
                        commandResult<-newIORef Nothing
                        firstCause<-newIORef (Nothing::Maybe SomeException)
                        let remember failure=modifyIORef' firstCause $ \old -> case old of Nothing -> Just failure; Just _ -> old
                            request reason=atomically $ readTVar stopped >>= \case Nothing -> writeTVar stopped (Just reason); Just _ -> pure ()
                            requested=do
                              own<-readTVarIO stopped
                              outer<-execution.requestStop
                              pure (case own of Just reason -> Just reason; Nothing -> outer)
                            dispatchCancel=do
                              clock<-getMonotonicTimeNSec
                              next<-readIORef nextCancel
                              if clock>=next then do
                                writeIORef cancelled True
                                writeIORef nextCancel (clock+if clock<deadline then 250000000 else 1000000000)
                                hooks.beforeCancel
                                dispatched<-Cancel.dispatchBefore (clock+250000000) capsule
                                case dispatched of Left _ -> request "cancel_transport_unconfirmed"; Right () -> pure ()
                              else pure ()
                            stopQuery=do
                              unstarted<-atomically $ readTVar gate >>= \case
                                Nothing -> writeTVar gate (Just False)>>pure True
                                Just False -> pure True
                                Just True -> pure False
                              if unstarted then pure () else dispatchCancel
                            command=do
                              permitted<-atomically (readTVar gate >>=maybe retry pure)
                              result<-try $ do
                                if permitted then hooks.beforeCommand else pure ()
                                stop<-requested
                                if not permitted || not(isNothing stop) then pure False
                                  else DB.runAuditCommand execution.connection operation>>hooks.afterCommand>>pure True
                              writeIORef commandResult (Just result)
                              either throwIO pure result
                            readiness snapshot=do
                              stop<-requested
                              ObservationSnapshot began metadata capacity<-snapshot
                              clock<-getMonotonicTimeNSec
                              let freshness=fromIntegral settings.assessmentTimeoutMs*1000000
                                  value=maybe (DB.reasonValue "observer_pending") snd metadata
                                  result=case stop of
                                    Just reason -> Stop reason
                                    Nothing | clock>=deadline -> Stop "operation_deadline"
                                    Nothing -> case metadata of
                                      Nothing | clock-began>=freshness -> Stop "observer_sample_deadline"
                                              | otherwise -> Waiting
                                      Just(stamp,_) | clock-stamp>=freshness -> Stop "observer_sample_deadline"
                                      Just _ | DB.valueField "reason" value/=Just ("ok"::Text) -> Stop "observer_unavailable"
                                      Just _ | DB.valueField "paused" value==Just True -> Stop "paused"
                                      Just _ | maybe True (>=closing) (DB.valueField "at" value::Maybe UTCTime) -> Stop "window_exhausted"
                                      Just _ | DB.valueField "foreground_waiters" value/=Just (0::Int) && execution.kind=="heap" -> Stop "foreground_pressure"
                                      Just _ -> case monitorPressure settings execution.assessment value of
                                        Just reason -> Stop reason
                                        Nothing -> case capacity of
                                          Nothing | clock-began>=freshness -> Stop "native_capacity_sample_deadline"
                                                  | otherwise -> Waiting
                                          Just(stamp,_) | clock-stamp>=freshness -> Stop "native_capacity_sample_deadline"
                                          Just(_,Left reason) -> Stop reason
                                          Just(_,Right ()) -> Ready
                              pure result
                            use snapshot child=do
                              hooks.beforeDispatch
                              let prepare=do
                                    state<-readiness snapshot
                                    case state of
                                      Waiting -> threadDelay 10000>>prepare
                                      Stop reason -> request reason>>atomically(writeTVar gate (Just False))
                                      Ready -> do
                                        -- Fresh nonblocking state is checked at
                                        -- the final gate, after all preflight I/O.
                                        again<-requested
                                        now<-getMonotonicTimeNSec
                                        if not(isNothing again)||now>=deadline then do
                                          request(fromMaybe "operation_deadline" again)
                                          atomically(writeTVar gate (Just False))
                                        else atomically(writeTVar gate (Just True))
                              prepare
                              let loop=do
                                    finished<-poll child
                                    case finished of
                                      Just _ -> pure ()
                                      Nothing -> do
                                        state<-readiness snapshot
                                        case state of
                                          Ready -> pure ()
                                          Waiting -> pure ()
                                          Stop reason -> do
                                            request reason
                                            -- Retry serial transports for this
                                            -- same query while start is unknown;
                                            -- an ignored idle cancel never
                                            -- consumes the outstanding stop.
                                            stopQuery
                                        threadDelay 10000
                                        loop
                              loop
                        controlled<-try $ withObservationMonitor observer execution.operationId requested execution.checkCapacity hooks.monitorResult $ \snapshot _ ->
                          withJoinedAuditQuery command stopQuery (use snapshot)
                        case controlled of Left failure -> remember failure; Right () -> pure ()
                        Cancel.finishCancel capsule
                        result<-readIORef commandResult
                        stoppedFor<-readTVarIO stopped
                        cancellationAttempted<-readIORef cancelled
                        fresh<-try @SomeException (DB.monitorAuditOperation observer execution.operationId)
                        value<-case fresh of
                          Right status | not cancellationAttempted && reusableProtocol result && DB.valueField "backend_state" status==Just ("idle"::Text) && DB.valueField "reason" status==Just ("ok"::Text) ->
                            settle execution.connection result stoppedFor
                          _ -> do
                          -- Any cancel attempt may race normal completion and
                          -- strike later SQL on this backend. Never reuse that
                          -- protocol, even if the observer currently sees idle.
                          -- Client retirement is explicitly not a backend join.
                            _<-try @SomeException (DB.markAuditOperationUncertain observer execution.operationId)
                            DB.retireAuditConnection execution.connection
                            let awaitBackend=do
                                  observed<-try @SomeException $ do
                                    exited<-Host.backendHasExited execution.native
                                    case exited of Right True -> pure True; _ -> threadDelay 100000>>pure False
                                  case observed of
                                    Right True -> pure ()
                                    Left failure -> remember failure>>awaitBackend
                                    _ -> awaitBackend
                            awaitBackend
                            DB.withAuditConnection cfg $ \reconciler -> do
                              acquired<-DB.tryAuditLock reconciler
                              if not acquired then pure(object ["reason".=("reconciliation_busy"::Text),"operation_id".=execution.operationId,"executed".=Null,"failed".=True])
                                else settle reconciler result stoppedFor
                        cause<-readIORef firstCause
                        maybe (pure value) throwIO cause
                    _ -> do
                      Cancel.finishCancel capsule
                      settle execution.connection (Just(Right False)) (Just "window_unknown")
                case opened of
                  Left reason -> settle execution.connection (Just(Right False)) (Just reason)
                  Right value -> pure value

data AuditStop=AuditStop (TVar (Maybe Text))
data OwnedAuditRun=OwnedAuditRun UUID ThreadId AuditStop (MVar (Either SomeException Value)) (TVar (Maybe SomeException))
data AuditRunResult=AuditRunJoined (Either SomeException Value) | AuditRunPending OwnedAuditRun

ownedRunId :: OwnedAuditRun -> UUID
ownedRunId (OwnedAuditRun identity _ _ _ _)=identity

-- Acquisition must be composed under the caller's masked ownership handoff
-- (runOwnedAuditRun does this). Only this root enters database/native brackets;
-- callers own stop/status/completion handles, never those brackets themselves.
startOwnedAuditRun :: (UUID -> AuditStop -> IO Value) -> IO OwnedAuditRun
startOwnedAuditRun action=mask $ \_ -> do
  identity<-UUID.nextRandom
  requested<-AuditStop <$>newTVarIO Nothing
  finished<-newEmptyMVar
  cause<-newTVarIO Nothing
  start<-newEmptyMVar
  thread<-forkIOWithUnmask $ \unmask -> do
    takeMVar start
    result<-try (unmask (action identity requested))
    putMVar finished result
  let owner=OwnedAuditRun identity thread requested finished cause
  putMVar start ()
  pure owner

stopRequested :: AuditStop -> IO (Maybe Text)
stopRequested (AuditStop request)=readTVarIO request

requestAuditStop :: OwnedAuditRun -> Text -> IO ()
requestAuditStop (OwnedAuditRun _ _ (AuditStop requested) _ _) reason=atomically $ do
  current<-readTVar requested
  case current of Nothing -> writeTVar requested (Just reason); Just _ -> pure ()

rememberCause :: OwnedAuditRun -> SomeException -> IO ()
rememberCause (OwnedAuditRun _ _ _ _ cause) failure=atomically $ do
  current<-readTVar cause
  case current of Nothing -> writeTVar cause (Just failure); Just _ -> pure ()

awaitAuditRun :: Int -> OwnedAuditRun -> IO AuditRunResult
awaitAuditRun microseconds owner@(OwnedAuditRun _ _ _ finished _)=
  timeout microseconds (readMVar finished) >>=pure . maybe (AuditRunPending owner) AuditRunJoined

-- Restore caller cancellation only while waiting. A cancellation requests stop
-- and gets one bounded drain. A pending result always carries the same actual
-- root owner; it is not an exception for a generic CLI-to-exit handler to catch.
runOwnedAuditRun :: (UUID -> AuditStop -> IO Value) -> (AuditRunResult -> IO a) -> IO a
runOwnedAuditRun action consume=mask $ \restore -> do
  owner<-startOwnedAuditRun action
  let wait=case owner of OwnedAuditRun _ _ _ finished _ -> readMVar finished
  result<-try (restore wait)
  disposition<-case result of
    Right value -> pure (AuditRunJoined value)
    Left failure -> do
      rememberCause owner failure
      requestAuditStop owner "caller_cancelled"
      drained<-try (restore (awaitAuditRun 5000000 owner))
      case drained of
        Right value -> pure value
        Left next -> rememberCause owner next>>pure (AuditRunPending owner)
  -- The continuation owns publication while masked. If pending, it must enter
  -- quarantine rather than return/throw and abandon the actual root thread.
  case disposition of
    AuditRunJoined value -> do
      first<-case owner of OwnedAuditRun _ _ _ _ cause -> readTVarIO cause
      consume (AuditRunJoined (maybe value Left first))
    pending -> do
      consumed<-try (consume pending)
      case consumed of
        Left failure -> rememberCause owner failure>>requestAuditStop owner "caller_cancelled"
        Right _ -> pure ()
      -- Even a faulty handler/output failure cannot escape ownership. A handler
      -- that returned early is quarantined here until that same root joins.
      _<-quarantineAuditRun (const (pure ())) owner
      either throwIO pure consumed

-- Emit once, then observe in bounded turns. Repeated handled cancellation and
-- output failure retain the first cause and cannot abandon the root's brackets.
-- Only a confirmed join permits returning or rethrowing the original cause.
quarantineAuditRun :: (Value -> IO ()) -> OwnedAuditRun -> IO Value
quarantineAuditRun emit owner@(OwnedAuditRun identity _ _ finished cause)=mask $ \restore -> do
  emitted<-try (restore (emit (object ["reason".=("cleanup_pending"::Text),"unclean".=True,"operation_id".=identity])))
  case emitted of Left failure -> rememberCause owner failure; Right () -> pure ()
  let loop=do
        observed<-try (restore (timeout 100000 (readMVar finished)))
        case observed of
          Left failure -> rememberCause owner failure>>requestAuditStop owner "caller_cancelled">>loop
          Right Nothing -> loop
          Right (Just value) -> do
            first<-readTVarIO cause
            either throwIO pure (maybe value Left first)
  loop

-- Install the actual child and cleanup while masked. Neither assertion failure
-- nor repeated control exceptions can unwind the surrounding connection/native
-- brackets before the query joins. stopQuery owns/finalizes its cancel transport
-- synchronously; it never asynchronously kills the Hasql child. The configured
-- finite server statement budget is the fallback if dispatch cannot succeed.
withJoinedAuditQuery :: IO a -> IO () -> (Async a -> IO b) -> IO b
withJoinedAuditQuery command stopQuery use=mask $ \restore -> do
  first<-newIORef (Nothing :: Maybe SomeException)
  let remember failure=modifyIORef' first $ \old -> case old of Nothing -> Just failure; Just _ -> old
  child<-asyncWithUnmask (\unmask -> unmask command)
  result<-try (restore (use child))
  case result of Left failure -> remember failure; Right _ -> pure ()
  pending<-poll child
  case pending of
    Just _ -> pure ()
    Nothing -> do
      stopped<-try stopQuery
      case stopped of Left failure -> remember failure; Right () -> pure ()
  let join=do
        joined<-try (restore (waitCatch child))
        case joined of Left failure -> remember failure>>join; Right _ -> pure ()
  join
  cause<-readIORef first
  case cause of Just failure -> throwIO failure; Nothing -> either throwIO pure result

failureReason :: SomeException -> Text
failureReason failure=case fromException failure of
  Just (DB.AuditFailure reason) -> reason
  Nothing -> "maintenance_unexpected_failure"

-- Only an acknowledged normal completion/no-dispatch result is reusable.
-- Every error path retires and joins the original backend conservatively.
reusableProtocol :: Maybe(Either SomeException Bool) -> Bool
reusableProtocol (Just(Right _))=True
reusableProtocol _=False

backupReady :: AuditMaintenanceConfig -> Host.HostIdentity -> UTCTime -> Maybe Value -> Bool
backupReady cfg proof now receipt=case receipt of
  Nothing -> False
  Just value -> DB.valueField "system_identifier" value==Just proof.systemIdentifier
    && DB.valueField "database_oid" value==Just proof.databaseOid && DB.valueField "restore_verified" value==Just True
    && maybe False (\stamp->stamp<=now&&diffUTCTime now stamp<=fromIntegral cfg.backupMaximumAgeSeconds)
      (DB.valueField "verified_at" value::Maybe UTCTime)
    && all (\field->maybe False validHash (DB.valueField field value::Maybe Text)) ["backup_sha256","restore_evidence_sha256"]
    && maybe False (\point->not(T.null point)&&T.length point<=32) (DB.valueField "recovery_point" value::Maybe Text)
  where validHash value=T.length value==64&&T.all (\c->c>='0'&&c<='9'||c>='a'&&c<='f') value

monitorPressure :: AuditMaintenanceConfig -> Value -> Value -> Maybe Text
monitorPressure cfg baseline observed=case
  (DB.valueField "wal_bytes" baseline,DB.valueField "wal_growth_bytes" observed,
   DB.valueField "slot_retained_bytes" observed,DB.valueField "slot_count" observed,DB.valueField "archive_failed" observed,
   DB.valueField "archive_pending_bytes" observed) of
  (Just wal,Just growth,Just slots,Just count,Just failed,Just archive)
    | any (<0) [wal,growth,slots,archive] || (count::Int)>256 -> Just "wal_pressure_unknown"
    | wal+growth>toInteger cfg.maximumWalBytes -> Just "wal_pressure"
    | slots>toInteger cfg.maximumSlotRetentionBytes -> Just "slot_pressure"
    | failed || archive>toInteger cfg.maximumArchiveBacklogBytes -> Just "archive_pressure"
    | otherwise -> Nothing
  _ -> Just "wal_pressure_unknown"

fromJSONList :: Value -> Maybe [Value]
fromJSONList value=case fromJSON value of Success values->Just values; Error _->Nothing

catalogBytes :: Value -> Integer
catalogBytes value=sum [fromMaybe 0(DB.valueField "bytes" entry)|entry<-fromMaybe [] (fromJSONList value)]

sizeEvidence :: Value -> Value -> [Value]
sizeEvidence before after=
  [object["name".=name,"before_bytes".=(Map.lookup name old >>=DB.valueField @Integer "bytes")
    ,"after_bytes".=(Map.lookup name new >>=DB.valueField @Integer "bytes")
    ,"before_filenode".=(Map.lookup name old >>=DB.valueField @Integer "filenode")
    ,"after_filenode".=(Map.lookup name new >>=DB.valueField @Integer "filenode")]
  |name<-take 64(Map.keys(Map.union old new))]
  where
    table value=Map.fromList[(name,entry)|entry<-fromMaybe [] (fromJSONList value),Just name<-[DB.valueField "name" entry::Maybe Text]]
    old=table before;new=table after

catalogChanged :: Value -> Value -> Either Text Bool
catalogChanged attempt after=do
  before<-maybe(Left "operation_catalog_unknown") Right (DB.valueField "before" attempt >>=fromJSONList)
  current<-maybe(Left "operation_catalog_unknown") Right (fromJSONList after)
  targetName<-maybe(Left "operation_target_unknown") Right (DB.valueField "target_name" attempt::Maybe Text)
  old<-unique [entry|entry<-before,DB.valueField "name" entry==Just targetName]
  new<-unique [entry|entry<-current,DB.valueField "name" entry==Just targetName]
  heap<-unique [entry|entry<-before,DB.valueField "kind" entry==Just("heap"::Text)]
  heapId<-maybe(Left "operation_catalog_unknown") Right (DB.valueField "oid" heap::Maybe Int64)
  let indexes entries=Map.fromList[(name,(DB.valueField "definition" entry::Maybe Text,DB.valueField "constraint_oids" entry::Maybe Value))
        |entry<-entries,DB.valueField "parent_oid" entry==Just heapId,Just name<-[DB.valueField "name" entry::Maybe Text]]
      valid entry=DB.valueField "kind" entry/=Just("index"::Text)
        || all(\field->DB.valueField field entry==Just True)["valid","ready","live"]
  if not(all valid current) then Left "invalid_bound_indexes_operator_remediation"
    else if indexes before/=indexes current then Left "audit_index_catalog_changed_operator_remediation"
    else case (DB.valueField "oid" old::Maybe Int64,DB.valueField "filenode" old::Maybe Int64,
               DB.valueField "oid" new::Maybe Int64,DB.valueField "filenode" new::Maybe Int64) of
      (Just oldOid,Just oldFile,Just newOid,Just newFile)->Right(oldOid/=newOid||oldFile/=newFile)
      _ -> Left "operation_catalog_unknown"
  where unique [value]=Right value;unique _=Left "operation_target_changed_operator_remediation"

settleOperation :: DB.AuditConnection -> Value -> Maybe (Either SomeException Bool) -> Maybe Text -> IO Value
settleOperation connection attempt result stopped=do
  status<-DB.auditStatus connection
  let operationId=DB.valueField "id" attempt::Maybe UUID
      current=DB.valueField "operation" status::Maybe Value
  if (current >>=DB.valueField "id")/=operationId then pure(object["reason".=("operation_generation_changed"::Text),"executed".=Null,"failed".=True])
  else if maybe False (\state->state `notElem`(["prepared","running","uncertain"]::[Text])) (current >>=DB.valueField "state")
    then pure(fromMaybe(DB.reasonValue "operation_already_settled")(DB.valueField "last_assessment" status))
  else do
    after<-DB.operationCatalog connection
    let changed=catalogChanged attempt after
        acknowledged=case result of Just(Right True)->True; _->False
        errorReason=case result of Just(Left failure)->Just(failureReason failure); _->Nothing
        kind=fromMaybe "unknown" (DB.valueField "kind" attempt::Maybe Text)
        notDispatched=case result of Just(Right False)->True; _->False
        (state,reason)=case changed of
          Left reason -> ("failed",reason)
          Right True | acknowledged -> ("succeeded","completed")
          Right True | kind=="index" -> ("reconciled","index_postcondition_observed_without_ack")
          Right True -> ("partial","heap_rewrite_observed_analysis_unconfirmed")
          Right False | errorReason==Just "maintenance_sqlstate_55P03" -> ("deferred","lock_pressure")
          Right False | notDispatched -> ("cancelled",fromMaybe "not_dispatched" stopped)
          Right False | errorReason==Just "maintenance_sqlstate_57014" -> ("cancelled",fromMaybe "operation_timeout" stopped)
          Right False | isNothing result -> ("deferred","no_rewrite_observed_after_lost_ack")
          Right False -> ("failed",fromMaybe "maintenance_postcondition_unconfirmed" errorReason)
        before=fromMaybe (Array mempty) (DB.valueField "before" attempt)
        value=object ["operation_id".=operationId,"operation".=kind,"state".=(state::Text),"reason".=(reason::Text)
          ,"acknowledged".=acknowledged,"rewrite_observed".=either (const Nothing) Just changed
          ,"executed".=either (const(if acknowledged then Just True else Nothing)) Just changed
          ,"failed".=(state `elem`(["failed","partial"]::[Text]))
          ,"before_bytes".=catalogBytes before,"after_bytes".=catalogBytes after
          ,"measurement_confidence".=("postgres_relation_forks_non_atomic"::Text)
          ,"relations".=sizeEvidence before after,"stop_reason".=stopped,"server_error".=errorReason]
    case operationId of
      Nothing -> pure(DB.reasonValue "operation_metadata_invalid")
      Just identity -> DB.finishAuditOperation connection identity state value after>>pure value

-- Recovery does not invent an old native join. Current exclusive authority,
-- absence of the exact old backend and actual catalog postconditions are needed.
reconcileAuditOperation :: DB.AuditConnection -> Value -> IO (Maybe Value)
reconcileAuditOperation connection status=case DB.valueField "operation" status of
  Just attempt | maybe False (`elem`(["prepared","running","uncertain"]::[Text])) (DB.valueField "state" attempt) ->
    case DB.valueField "id" attempt of
      Nothing -> pure(Just(DB.reasonValue "operation_metadata_invalid"))
      Just identity -> do
        observed<-DB.monitorAuditOperation connection identity
        if DB.valueField "reason" observed/=Just ("ok"::Text) || DB.valueField "backend_present" observed/=Just False
          then pure(Just(object["reason".=("previous_operation_unsettled"::Text),"operation_id".=identity,"executed".=Null]))
          else Just <$>settleOperation connection attempt Nothing (Just "lost_acknowledgement")
  _ -> pure Nothing
