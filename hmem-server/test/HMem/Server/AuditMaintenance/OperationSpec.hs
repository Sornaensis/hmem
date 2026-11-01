{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module HMem.Server.AuditMaintenance.OperationSpec (spec) where

import Control.Concurrent (threadDelay,throwTo,runInBoundThread)
import Control.Concurrent.Async (async,asyncThreadId,waitCatch,withAsync,poll)
import Control.Concurrent.MVar
import Control.Exception (AsyncException(..),SomeAsyncException,SomeException,IOException,bracket,bracket_,finally,fromException,throwIO,try,mask,mask_)
import Control.Monad (void,forM_,forM,unless,when)
import Crypto.Hash (Digest,SHA256,hash)
import Data.Aeson
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Int (Int64,Int32)
import Data.Bits ((.&.))
import Data.IORef
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time (getCurrentTime)
import Data.Pool (destroyAllResources)
import Data.UUID.V4 qualified as UUID
import Data.UUID qualified as UUIDCore
import GHC.Clock (getMonotonicTimeNSec)
import Hasql.Session qualified as S
import Hasql.Connection qualified as H
import Hasql.Decoders qualified as D
import Hasql.Encoders qualified as E
import Hasql.Statement qualified as St
import HMem.Config qualified as C
import HMem.DB.AuditMaintenance qualified as DB
import HMem.DB.AuditMaintenance.Cancel qualified as Cancel
import HMem.DB.AuditMaintenance.Host qualified as Host
import HMem.DB.Pool (withConn,runSession,createPool)
import HMem.DB.TestHarness
import HMem.Server.AuditMaintenance.Operation
import HMem.Server.AuditMaintenance qualified as Worker
import HMem.Server.CtlAuditMaintenance qualified as Ctl
import HMem.Server.AuditMaintenanceSpec (withRuntime)
import HMem.Server.TestHarness (withLocalSandboxAppEnv,mkLocalSandboxConfig)
import HMem.Server.App (mkApp)
import HMem.Server.AccessTracker (newAccessTracker)
import HMem.Server.WebSocket (newWSState)
import HMem.Types (Workspace(..))
import Network.Wai qualified as Wai
import Network.Wai.Test qualified as WaiTest
import Network.HTTP.Types (statusCode)
import System.Timeout (timeout)
import System.Directory (removeFile,removeDirectory,doesFileExist,createDirectoryIfMissing,canonicalizePath,listDirectory,pathIsSymbolicLink,doesDirectoryExist,getFileSize)
import System.Environment (getEnvironment)
import System.Exit (ExitCode(..))
import System.FilePath ((</>),isAbsolute,splitDirectories,takeDirectory,normalise,makeRelative)
import System.Process
import System.IO (withFile,withBinaryFile,IOMode(AppendMode,ReadMode),hFileSize,hSeek,SeekMode(AbsoluteSeek))
import Foreign hiding (void)
import Foreign.C.String (CWString,withCWString)
import Test.Hspec

spec :: Spec
spec=do
  describe "Audit operation owners" $ do
    it "publishes completion only after the actual root's resource finalizer" $ do
      released<-newIORef False
      value<-runOwnedAuditRun (\_ _ -> bracket_ (pure ()) (writeIORef released True) (pure (DB.reasonValue "finished"))) consume
      DB.valueField "reason" value `shouldBe` Just ("finished"::Text)
      readIORef released `shouldReturn` True
    it "requests cooperative stop and rethrows caller cancellation after the root joins" $ do
      entered<-newEmptyMVar; released<-newIORef False
      let action _ stop=bracket_ (putMVar entered ()) (writeIORef released True) $ do
            let wait=stopRequested stop >>= maybe (threadDelay 1000>>wait) (const (pure (DB.reasonValue "stopped")))
            wait
      withAsync (runOwnedAuditRun action consume) $ \caller -> do
        timeout 1000000 (takeMVar entered) `shouldReturn` Just ()
        throwTo (asyncThreadId caller) UserInterrupt
        result<-timeout 2000000 (waitCatch caller)
        case result of
          Just (Left failure) -> fromException failure `shouldBe` Just UserInterrupt
          _ -> expectationFailure "caller cancellation was not preserved after join"
      readIORef released `shouldReturn` True
    it "quarantines the same pending owner across repeated cancellation and output failure" $ do
      entered<-newEmptyMVar; release<-newEmptyMVar; pending<-newEmptyMVar
      released<-newIORef False; emissions<-newIORef (0::Int); roots<-newIORef (0::Int)
      let action _ _=bracket_ (modifyIORef' roots (+1)>>putMVar entered ()) (writeIORef released True) $ do
            -- A finite fixture guard is independent of test assertions/cleanup.
            _<-timeout 15000000 (takeMVar release)
            pure (DB.reasonValue "finished")
          emit _=modifyIORef' emissions (+1)>>void(tryPutMVar pending ())>>throwIO(userError "fixture_output_failure")
          handle (AuditRunJoined result)=either throwIO pure result
          handle (AuditRunPending owner)=quarantineAuditRun emit owner
      withAsync (runOwnedAuditRun action handle) $ \caller ->
        (do
          timeout 1000000 (takeMVar entered) `shouldReturn` Just ()
          throwTo (asyncThreadId caller) UserInterrupt
          timeout 6500000 (takeMVar pending) `shouldReturn` Just ()
          readIORef released `shouldReturn` False
          poll caller >>= \case Nothing -> pure (); _ -> expectationFailure "pending caller escaped before root join"
          throwTo (asyncThreadId caller) ThreadKilled
          putMVar release ()
          result<-timeout 2000000 (waitCatch caller)
          case result of
            Just (Left failure) -> fromException failure `shouldBe` Just UserInterrupt
            _ -> expectationFailure "pending owner/cause was lost"
          readIORef emissions `shouldReturn` 1
          readIORef roots `shouldReturn` 1
          readIORef released `shouldReturn` True)
        `finally` (void(tryPutMVar release ())>>void(waitCatch caller))
    it "fences a pending handler that throws or returns before its root joins" $
      forM_ [False,True] $ \throws -> do
        entered<-newEmptyMVar; release<-newEmptyMVar; pending<-newEmptyMVar
        released<-newIORef False
        let action _ _=bracket_ (putMVar entered ()) (writeIORef released True) $ do
              _<-timeout 15000000 (takeMVar release)
              pure (DB.reasonValue "finished")
            handle (AuditRunJoined result)=either throwIO pure result
            handle (AuditRunPending _)=do
              putMVar pending ()
              if throws then throwIO(userError "fixture_handler_failure") else pure (DB.reasonValue "early_return")
        withAsync (runOwnedAuditRun action handle) $ \caller ->
          (do
            timeout 1000000 (takeMVar entered) `shouldReturn` Just ()
            throwTo (asyncThreadId caller) UserInterrupt
            timeout 6500000 (takeMVar pending) `shouldReturn` Just ()
            threadDelay 10000
            poll caller >>= \case Nothing -> pure (); _ -> expectationFailure "faulty handler abandoned actual root"
            readIORef released `shouldReturn` False
            putMVar release ()
            timeout 2000000 (waitCatch caller) >>= \case
              Just (Left failure) -> fromException failure `shouldBe` Just UserInterrupt
              _ -> expectationFailure "faulty handler lost first cause"
            readIORef released `shouldReturn` True)
          `finally` (void(tryPutMVar release ())>>void(waitCatch caller))
  around withTestEnv $ describe "Audit operation primitives" $ do
    it "returns metadata under a real AccessExclusive audit lock within the observer budget" $ \env -> withRuntime env $ \cfg _ ->
      DB.withAuditConnection cfg $ \owner -> do
        DB.tryAuditLock owner `shouldReturn` True
        identity<-DB.auditIdentity owner
        target<-heapOid identity
        operationId<-UUID.nextRandom
        _<-DB.beginAuditOperation owner operationId "heap" target False
        DB.markAuditOperationRunning owner operationId
        withConn env.pool $ \locker -> bracket_
          (S.run (S.sql "BEGIN; LOCK TABLE public.audit_log IN ACCESS EXCLUSIVE MODE") locker >>=either (fail . show) pure)
          (void(S.run (S.sql "ROLLBACK") locker)) $
          DB.withAuditConnection cfg $ \observer -> do
            DB.configureOperationBudget observer 100
            result<-timeout 500000 (DB.monitorAuditOperation observer operationId)
            case result of
              Just value -> DB.valueField "reason" value `shouldBe` Just ("ok"::Text)
              Nothing -> expectationFailure "metadata observer blocked on audit relation lock"
        after<-DB.operationCatalog owner
        DB.finishAuditOperation owner operationId "cancelled" (DB.reasonValue "fixture_monitor_only") after
    it "dispatches a secret-bound real cancel, joins query, then proves retained backend exit before lock handoff" $ \env -> void $
      runOwnedAuditRun (\operationId _ -> withRuntime env $ \cfg _ ->
      DB.withAuditConnection cfg $ \owner -> do
        DB.tryAuditLock owner `shouldReturn` True
        identity<-DB.auditIdentity owner
        proof<-either (fail . show) pure (DB.parseValue identity)
        target<-heapOid identity
        _<-DB.beginAuditOperation owner operationId "heap" target False
        DB.markAuditOperationRunning owner operationId
        DB.configureOperationBudget owner 10000
        native<-Host.withAuditHost proof $ \host -> DB.withAuditConnection cfg $ \observer -> do
          DB.configureOperationBudget observer 200
          capable<-DB.withAuditCancel owner $ \capsule -> do
            let dispatch=do
                  now<-getMonotonicTimeNSec
                  Cancel.dispatchBefore (now+500000000) capsule
            releaseQuery<-newEmptyMVar
            withJoinedAuditQuery (takeMVar releaseQuery>>DB.session owner (S.sql "SELECT pg_sleep(30)"))
              (void(tryPutMVar releaseQuery ())>>void dispatch) $ \query -> do
              -- A successful signal while idle does not consume the obligation:
              -- the same operation's reset capsule must cancel its later query.
              dispatch `shouldReturn` Right ()
              putMVar releaseQuery ()
              let awaitActive=do
                    value<-DB.monitorAuditOperation observer operationId
                    if DB.valueField "backend_state" value==Just ("active"::Text) then pure () else threadDelay 1000>>awaitActive
              timeout 500000 awaitActive `shouldReturn` Just ()
              started<-getMonotonicTimeNSec
              dispatch `shouldReturn` Right ()
              completed<-timeout 1000000 (waitCatch query)
              ended<-getMonotonicTimeNSec
              (ended-started) `shouldSatisfy` (<2000000000)
              case completed of
                Just (Left failure) -> fromException failure `shouldBe` Just (DB.AuditFailure "maintenance_sqlstate_57014")
                _ -> expectationFailure "explicit cancellation did not join before the separate 10s server guard"
              pure True
          capable `shouldBe` Right True
          status<-DB.monitorAuditOperation observer operationId
          DB.valueField "backend_state" status `shouldBe` Just ("idle"::Text)
          Host.backendHasExited host `shouldReturn` Right False
          DB.retireAuditConnection owner
          let awaitExit=Host.backendHasExited host >>= \case
                Right True -> pure ()
                Right False -> threadDelay 1000>>awaitExit
                Left failure -> fail(show failure)
          timeout 2000000 awaitExit `shouldReturn` Just ()
          DB.tryAuditLock observer `shouldReturn` True
          after<-DB.operationCatalog observer
          DB.finishAuditOperation observer operationId "cancelled" (DB.reasonValue "fixture_cancel_and_join") after
          pure True
        native `shouldBe` Right True
        pure (DB.reasonValue "fixture_cancel_and_join")) consume
    it "denies runtime backup self-attestation at both the ACL and independent owner check" $ \env -> withRuntime env $ \cfg role ->
      DB.withAuditConnection cfg $ \connection -> do
        result<-try @DB.AuditFailure (DB.session connection (S.sql "SELECT public.hmem_audit_backup_attest('{}'::jsonb)"))
        result `shouldBe` Left (DB.AuditFailure "maintenance_sqlstate_42501")
        let execute sql=withConn env.pool $ \admin -> S.run (S.sql sql) admin >>=either (fail . show) pure
        bracket_ (execute ("GRANT EXECUTE ON FUNCTION public.hmem_audit_backup_attest(jsonb) TO "<>role))
          (execute ("REVOKE EXECUTE ON FUNCTION public.hmem_audit_backup_attest(jsonb) FROM "<>role)) $ do
          granted<-try @DB.AuditFailure (DB.session connection (S.sql "SELECT public.hmem_audit_backup_attest('{}'::jsonb)"))
          granted `shouldBe` Left (DB.AuditFailure "maintenance_sqlstate_42501")
  around (\action->withLocalSandboxAppEnv(\env app->action(env,app))) $ describe "Audit physical smoke" $ do
    it "automatically rewrites only the audit heap after an actual verified backup restore" $ \(env,app) -> void $
      runOwnedAuditRun (\_ _ -> withRuntime env $ \base role -> do
        let cfg=physicalConfig base True
        requireFixtureCapacity cfg
        (taskPath,revertPath)<-prepareApiHistory env app
        prepareBloat env
        adminSql env ("CREATE TABLE public.audit_scope_child () INHERITS(public.audit_log); GRANT MAINTAIN ON public.audit_scope_child TO "<>role<>"; INSERT INTO public.audit_scope_child(entity_type,entity_id,action,new_values) VALUES('scope_child','unchanged','create','{\"value\":1}')")
        childBefore<-adminJson env "SELECT jsonb_build_object('filenode',pg_relation_filenode('public.audit_scope_child'),'rows',(SELECT jsonb_agg(to_jsonb(c)) FROM ONLY public.audit_scope_child c))"
        beforeRows<-auditRows env
        before<-allocatedCatalog env cfg
        verifyBackupRestore env cfg beforeRows
        outcome<-awaitPhysicalWorker cfg "heap"
        DB.valueField "executed" outcome `shouldBe` Just True
        auditRows env `shouldReturn` beforeRows
        adminJson env "SELECT jsonb_build_object('filenode',pg_relation_filenode('public.audit_scope_child'),'rows',(SELECT jsonb_agg(to_jsonb(c)) FROM ONLY public.audit_scope_child c))" `shouldReturn` childBefore
        after<-allocatedCatalog env cfg
        let bytes kind entries=sum [size| (entry,size)<-entries, DB.valueField "kind" entry==Just(kind::Text)]
        bytes "heap" after+bytes "toast" after `shouldSatisfy` (<bytes "heap" before+bytes "toast" before)
        sum(map snd after) `shouldSatisfy` (<sum(map snd before))
        assertIndexesValid env
        assertTaskSnapshot app taskPath "physical_after" afterDescription
        _<-apiJson app "POST" revertPath Null
        assertTaskSnapshot app taskPath "physical_before" beforeDescription
        putStrLn("physical_heap_allocated_before="<>show(sum(map snd before))<>" after="<>show(sum(map snd after)))
        pure outcome) consume
  around withTestEnv $ describe "Audit operation safety" $ do
    it "measures a required native main file and rejects its absence while allowing missing optional forks" $ \env -> do
      let present=env.testSandbox.sandboxTmpDir</>"allocation-fixture"
          missing=present<>"-missing"
      bracket_ (BS.writeFile present(BS.replicate 8192 1)) (removeFile present) $ do
        allocatedFile True present >>= (`shouldSatisfy` (>0))
        allocatedFile False missing `shouldReturn` 0
        absent<-try @IOException(allocatedFile True missing)
        case absent of Left _->pure();Right _->expectationFailure "missing required main was reported as zero allocation"
    it "retains a nested worker after bounded drain failure and repeated cancellation until its actual finalizer joins" $ \_ -> do
      entered<-newEmptyMVar; released<-newIORef False
      let action=putMVar entered()>>threadDelay 6000000>>writeIORef released True>>pure(DB.reasonValue "fixture_finished")
          acquire=Worker.startAuditWorkerWith C.defaultConfig action
          use _=takeMVar entered>>fail "fixture_worker_assertion"
      withAsync (withRetainedWorker acquire use) $ \caller -> do
        threadDelay 200000
        throwTo (asyncThreadId caller) UserInterrupt
        result<-timeout 8000000(waitCatch caller)
        case result of
          Just(Left failure) -> show failure `shouldContain` "fixture_worker_assertion"
          _ -> expectationFailure "nested worker owner/cause escaped its join"
      readIORef released `shouldReturn` True
    it "joins the same helper handle even when the cancellation signal itself throws" $ \_ -> do
      retained<-newIORef Nothing
      let signal handle=writeIORef retained(Just handle)>>fail "fixture_signal_failure"
          command=(proc "powershell" ["-NoProfile","-Command","Start-Sleep -Milliseconds 200"]) {std_in=NoStream,std_out=NoStream,std_err=NoStream}
      outcome<-try @IOException(ownedHelperWith 10000 signal command)
      case outcome of Left _->pure();Right _->expectationFailure "helper timeout was lost"
      readIORef retained >>=maybe (expectationFailure "helper handle not retained") (\handle->getProcessExitCode handle `shouldReturn` Just ExitSuccess)
    it "treats an async worker failure as joined while retaining the first repeated caller interruption" $ \_ ->
      forM_ [False,True] $ \interrupt -> do
        entered<-newEmptyMVar; release<-newEmptyMVar; finalized<-newIORef False
        let action=(putMVar entered()>>void(timeout 3000000(readMVar release))>>throwIO UserInterrupt)
              `finally` writeIORef finalized True
            acquire=Worker.startAuditWorkerWith C.defaultConfig action
        withAsync (withRetainedWorker acquire (const(readMVar entered))) $ \caller -> do
          readMVar entered
          when interrupt $ do
            throwTo(asyncThreadId caller) ThreadKilled
            throwTo(asyncThreadId caller) UserInterrupt
          putMVar release()
          timeout 2000000(waitCatch caller) >>= \case
            Just(Left failure)->fromException failure `shouldBe` Just(if interrupt then ThreadKilled else UserInterrupt)
            _->expectationFailure "joined async worker failure was mistaken for pending work"
        readIORef finalized `shouldReturn` True
    it "honors an existing stop before entering capacity I/O or admitting a generation" $ \env -> ownedIndex env $ \execution -> do
      count<-newIORef (0::Int)
      outcome<-executeAuditOperation defaultExecutionHooks execution
        {requestStop=pure(Just "fixture_stop"),checkCapacity=modifyIORef' count (+1)>>pure(Right())}
      readIORef count `shouldReturn` 0
      DB.valueField "reason" outcome `shouldBe` Just("fixture_stop"::Text)
      DB.auditStatus execution.connection >>= \value -> (DB.valueField "operation" value::Maybe Value) `shouldBe` Just Null
      pure outcome
    it "fences a generation when stop arrives at the pre-dispatch barrier" $ \env -> ownedIndex env $ \execution -> do
      stopping<-newIORef Nothing; commands<-newIORef(0::Int)
      let hooks=defaultExecutionHooks {beforeDispatch=writeIORef stopping(Just "fixture_pause"),beforeCommand=modifyIORef' commands (+1)}
      before<-DB.operationCatalog execution.connection
      outcome<-executeAuditOperation hooks execution {requestStop=readIORef stopping}
      readIORef commands `shouldReturn` 0
      DB.operationCatalog execution.connection `shouldReturn` before
      DB.valueField "executed" outcome `shouldBe` Just False
      pure outcome
    it "does not dispatch when the first capacity sample consumes the absolute deadline" $ \env -> ownedIndex env $ \execution -> do
      commands<-newIORef(0::Int)
      let cfg=execution.config {C.auditMaintenance=execution.config.auditMaintenance {C.maximumOperationSeconds=1}}
          hooks=defaultExecutionHooks {beforeCommand=modifyIORef' commands (+1)}
      outcome<-executeAuditOperation hooks execution {config=cfg,checkCapacity=threadDelay 1200000>>pure(Right())}
      readIORef commands `shouldReturn` 0
      DB.valueField "executed" outcome `shouldBe` Just False
      pure outcome
    it "cancels a real blocked command while a native sample remains owned and unfinished" $ \env -> ownedIndex env $ \execution ->
      withConn env.pool $ \locker -> do
        DB.session execution.connection (S.sql "SET lock_timeout='5s'")
        stopping<-newIORef Nothing; samples<-newIORef(0::Int)
        sampleEntered<-newEmptyMVar; releaseSample<-newEmptyMVar; cancelEntered<-newEmptyMVar; locked<-newEmptyMVar
        sampleJoined<-newIORef False
        let sql value=S.run(S.sql value) locker >>=either (fail . show) pure
            capacity=do
              n<-atomicModifyIORef' samples (\n->(n+1,n+1))
              if n/=2 then pure(Right()) else do
                putMVar sampleEntered ()
                _<-timeout 5000000(takeMVar releaseSample)
                writeIORef sampleJoined True
                pure(Right())
            hooks=defaultExecutionHooks
              {beforeCommand=do
                result<-try @SomeException(sql "BEGIN; LOCK TABLE public.audit_log IN ACCESS EXCLUSIVE MODE")
                putMVar locked result
                either throwIO pure result
              ,beforeCancel=void(tryPutMVar cancelEntered ())}
            control=DB.withAuditConnection execution.config $ \observer ->
              (do
                readMVar locked >>=either throwIO pure
                timeout 2000000(takeMVar sampleEntered) `shouldReturn` Just ()
                writeIORef stopping(Just "fixture_pause")
                timeout 1000000(takeMVar cancelEntered) `shouldReturn` Just ()
                readIORef sampleJoined `shouldReturn` False
                let idle=DB.monitorAuditOperation observer execution.operationId >>= \value ->
                      if DB.valueField "backend_state" value==Just("idle"::Text) then pure() else threadDelay 1000>>idle
                timeout 1000000 idle `shouldReturn` Just ())
              `finally` (readMVar locked>>void(try @SomeException(sql "ROLLBACK"))>>void(tryPutMVar releaseSample ()))
            cfg=execution.config {C.auditMaintenance=execution.config.auditMaintenance {C.lockTimeoutMs=5000}}
        bracket_ (pure()) (void(S.run(S.sql "ROLLBACK") locker)) $
          withJoinedAuditQuery control (writeIORef stopping(Just "fixture_cleanup")>>void(tryPutMVar releaseSample ())) $ \controller -> do
            outcome<-executeAuditOperation hooks execution {config=cfg,requestStop=readIORef stopping,checkCapacity=capacity}
            waitCatch controller >>=either throwIO pure
            readIORef sampleJoined `shouldReturn` True
            Host.backendHasExited execution.native `shouldReturn` Right True
            DB.valueField "executed" outcome `shouldBe` Just False
            DB.valueField "server_error" outcome `shouldBe` Just("maintenance_sqlstate_57014"::Text)
            DB.valueField "stop_reason" outcome `shouldBe` Just("fixture_pause"::Text)
            pure outcome
    it "retires the original backend when a delayed cancel races acknowledged command completion" $ \env -> ownedIndex env $ \execution -> do
      stopping<-newIORef Nothing; finished<-newEmptyMVar; release<-newEmptyMVar; cancelled<-newEmptyMVar
      let hooks=defaultExecutionHooks
            {afterCommand=putMVar finished()>>void(timeout 5000000(takeMVar release))
            ,beforeCancel=void(tryPutMVar cancelled())}
          control=(do
            timeout 2000000(takeMVar finished) `shouldReturn` Just ()
            writeIORef stopping(Just "fixture_completion_race")
            timeout 1000000(takeMVar cancelled) `shouldReturn` Just ()) `finally` void(tryPutMVar release())
      withJoinedAuditQuery control (writeIORef stopping(Just "fixture_cleanup")>>void(tryPutMVar release())) $ \controller -> do
        outcome<-executeAuditOperation hooks execution {requestStop=readIORef stopping}
        waitCatch controller >>=either throwIO pure
        Host.backendHasExited execution.native `shouldReturn` Right True
        DB.valueField "acknowledged" outcome `shouldBe` Just True
        DB.valueField "executed" outcome `shouldBe` Just True
        pure outcome
    it "keeps a stop obligation when an observer fails before a delayed SQL dispatch" $ \env -> ownedIndex env $ \execution -> do
      entered<-newIORef False; release<-newEmptyMVar; cancelled<-newEmptyMVar; commands<-newIORef(0::Int)
      let hooks=defaultExecutionHooks
            {beforeCommand=writeIORef entered True>>void(timeout 5000000(takeMVar release))>>modifyIORef' commands (+1)
            ,monitorResult= \value -> readIORef entered >>= \active -> pure(if active then DB.reasonValue "fixture_observer_failure" else value)
            ,beforeCancel=void(tryPutMVar cancelled())}
          control=(timeout 2000000(takeMVar cancelled) `shouldReturn` Just ()) `finally` void(tryPutMVar release())
      before<-DB.operationCatalog execution.connection
      withJoinedAuditQuery control (void(tryPutMVar release())) $ \controller -> do
        outcome<-executeAuditOperation hooks execution
        waitCatch controller >>=either throwIO pure
        readIORef commands `shouldReturn` 1
        DB.valueField "executed" outcome `shouldBe` Just False
        DB.withAuditConnection execution.config $ \fresh -> do
          DB.tryAuditLock fresh `shouldReturn` True
          DB.operationCatalog fresh `shouldReturn` before
        pure outcome
    it "measures pending ready archive segments even without an archiver failure" $ \env -> ownedIndex env $ \execution -> do
      _<-DB.beginAuditOperation execution.connection execution.operationId "index" execution.target False
      let ready=env.testDb.testDbDataDir</>"pg_wal"</>"archive_status"</>"0000000100000000000000FE.ready"
      bracket_ (BS.writeFile ready BS.empty) (removeFile ready) $ do
        value<-DB.monitorAuditOperation execution.connection execution.operationId
        DB.valueField "archive_pending_bytes" value `shouldBe` Just(16777216::Integer)
        DB.valueField "archive_failed" value `shouldBe` Just False
      catalog<-DB.operationCatalog execution.connection
      DB.finishAuditOperation execution.connection execution.operationId "cancelled" (DB.reasonValue "fixture_archive_only") catalog
      pure(DB.reasonValue "fixture_archive_only")
    it "withdraws dispatch when a live archive backlog crosses its bound without an archiver failure" $ \env -> ownedIndex env $ \execution -> do
      release<-newEmptyMVar; entered<-newIORef False
      let ready=env.testDb.testDbDataDir</>"pg_wal"</>"archive_status"</>"0000000100000000000000FD.ready"
          hooks=defaultExecutionHooks
            {beforeCommand=BS.writeFile ready BS.empty>>writeIORef entered True>>void(timeout 5000000(takeMVar release))
            ,beforeCancel=void(tryPutMVar release())}
          cfg=execution.config {C.auditMaintenance=execution.config.auditMaintenance {C.maximumArchiveBacklogBytes=0}}
      outcome<-executeAuditOperation hooks execution {config=cfg}
        `finally` (doesFileExist ready >>= \exists->when exists(removeFile ready))
      readIORef entered `shouldReturn` True
      DB.valueField "stop_reason" outcome `shouldBe` Just("archive_pressure"::Text)
      DB.valueField "executed" outcome `shouldBe` Just False
      pure outcome
    it "does not fall through to index work outside the configured window, including manual restart" $ \env -> withRuntime env $ \base _ -> do
      prepareSmallBloat env
      let cfg=smallConfig base
          outside=Just(read "2026-11-01 01:00:00 UTC")
      forM_ [Worker.ScheduledAudit,Worker.ManualAudit] $ \mode -> do
        outcome<-Worker.runAuditCycleAt (cfg {C.auditMaintenance=cfg.auditMaintenance {C.windowStart="03:00",C.windowEnd="04:00"}}) mode outside
        DB.valueField "executed" outcome `shouldBe` Just False
        DB.valueField "reason" outcome `shouldBe` Just("window_closed"::Text)
      adminJson env "SELECT coalesce(operation,'null'::jsonb) FROM public.audit_maintenance_control" `shouldReturn` Null
    it "preserves physical uncertainty after a post-command settlement failure and reconciles without another rewrite" $ \env -> withRuntime env $ \base _ -> do
      prepareSmallBloat env
      let cfg=smallConfig base
          hooks=defaultExecutionHooks {beforeSettlement=throwIO(DB.AuditFailure "fixture_settlement_failure")}
      outcome<-Worker.runAuditCycleAtWithHooks hooks (pure()) cfg Worker.ManualAudit Nothing
      DB.valueField "operation" outcome `shouldBe` Just("index"::Text)
      DB.valueField "executed" outcome `shouldBe` Just Null
      DB.valueField "uncertain" outcome `shouldBe` Just True
      DB.valueField "failure_persistence" outcome `shouldBe` Just("recorded"::Text)
      settled<-Worker.runAuditCycle cfg Worker.ManualAudit
      DB.valueField "reason" settled `shouldBe` Just("index_postcondition_observed_without_ack"::Text)
      DB.valueField "executed" settled `shouldBe` Just True
    it "does not overwrite a settled or replaced generation during failure-recorder lock handoff" $ \env ->
      forM_ [False,True] $ \replace -> withRuntime env $ \base _ -> do
        prepareSmallBloat env
        let cfg=smallConfig base
            newer=DB.reasonValue "fixture_other_owner_settled"
            handoff=DB.withAuditConnection cfg $ \other -> do
              DB.tryAuditLock other `shouldReturn` True
              status<-DB.auditStatus other
              attempt<-maybe(fail "attempt missing") pure(DB.valueField "operation" status::Maybe Value)
              identifier<-maybe(fail "id missing") pure(DB.valueField "id" attempt)
              catalog<-DB.operationCatalog other
              DB.finishAuditOperation other identifier "reconciled" newer catalog
              when replace $ do
                next<-UUID.nextRandom
                entries<-either(fail . show) pure(DB.parseValue catalog::Either Text [Value])
                target<-case [oid|entry<-entries,DB.valueField "name" entry==(DB.valueField "target_name" attempt::Maybe Text)
                  ,Just oid<-[DB.valueField "oid" entry::Maybe Int64]] of
                  [oid]->pure oid
                  _->fail "current validated target missing"
                _<-DB.beginAuditOperation other next "index" target False
                DB.finishAuditOperation other next "cancelled" newer catalog
            hooks=defaultExecutionHooks {beforeSettlement=throwIO(DB.AuditFailure "fixture_settlement_failure"),beforeFailureRecording=handoff}
        outcome<-Worker.runAuditCycleAtWithHooks hooks (pure()) cfg Worker.ManualAudit Nothing
        DB.valueField "failure_persistence" outcome `shouldBe` Just(if replace then "superseded" else "already_settled"::Text)
        adminJson env "SELECT last_assessment FROM public.audit_maintenance_control" `shouldReturn` newer
  around withTestEnv $ describe "Audit physical interruption" $ do
    it "defers a real FULL rewrite at the short exclusive-lock budget without changing audit files or values" $ \env -> ownedHeap env $ \execution ->
      withAuditReadLock execution.config $ do
        before<-DB.operationCatalog execution.connection
        began<-getMonotonicTimeNSec
        outcome<-executeAuditOperation defaultExecutionHooks execution
        ended<-getMonotonicTimeNSec
        (ended-began) `shouldSatisfy` (<2000000000)
        DB.valueField "server_error" outcome `shouldBe` Just("maintenance_sqlstate_55P03"::Text)
        DB.valueField "executed" outcome `shouldBe` Just False
        DB.withAuditConnection execution.config $ \fresh -> do
          DB.tryAuditLock fresh `shouldReturn` True
          DB.operationCatalog fresh `shouldReturn` before
        pure outcome
    it "FULL_timeout bounds the real command and joins its cancelled backend" $ \env -> ownedHeap env $ \execution ->
      withAuditReadLock execution.config $ do
        DB.session execution.connection(S.sql "SET lock_timeout='5s'")
        let cfg=execution.config {C.auditMaintenance=execution.config.auditMaintenance {C.maximumOperationSeconds=1}}
        began<-getMonotonicTimeNSec
        attempted<-try @SomeException(executeAuditOperation defaultExecutionHooks execution {config=cfg})
        outcome<-case attempted of
          Right value->pure value
          Left failure->do
            void(try @SomeException(diagnoseTimeoutFailure env execution failure))
            throwIO failure
        ended<-getMonotonicTimeNSec
        (ended-began) `shouldSatisfy` (<2500000000)
        DB.valueField "server_error" outcome `shouldBe` Just("maintenance_sqlstate_57014"::Text)
        DB.valueField "executed" outcome `shouldBe` Just False
        Host.backendHasExited execution.native `shouldReturn` Right True
        pure outcome
    it "persists pause and cancels/joins an actual waiting FULL command through the CLI control route" $ \env -> ownedHeap env $ \execution ->
      withAuditReadLock execution.config $ do
        DB.session execution.connection(S.sql "SET lock_timeout='5s'")
        let control=do
              awaitActualCommand env execution "VACUUM (FULL"
              response<-Ctl.runAuditControlWithConfig execution.config Ctl.AuditPause
              let result=DB.valueField "result" response::Maybe Value
              (result >>=DB.valueField "paused") `shouldBe` Just True
              (result >>=DB.valueField "drain") `shouldSatisfy` (`elem`[Just("joined"::Text),Just "uncertain"])
        withJoinedAuditQuery control (DB.withAuditConnection execution.config(\fresh->void(DB.setAuditPaused fresh True))) $ \controller -> do
          outcome<-executeAuditOperation defaultExecutionHooks execution
          waitCatch controller >>=either throwIO pure
          DB.valueField "server_error" outcome `shouldBe` Just("maintenance_sqlstate_57014"::Text)
          DB.valueField "stop_reason" outcome `shouldBe` Just("paused"::Text)
          DB.valueField "executed" outcome `shouldBe` Just False
          Host.backendHasExited execution.native `shouldReturn` Right True
          pure outcome
    it "retains native ownership through actual connection loss during a FULL command" $ \env -> ownedHeap env $ \execution ->
      withAuditReadLock execution.config $ do
        DB.session execution.connection(S.sql "SET lock_timeout='5s'")
        let control=do
              awaitActualCommand env execution "VACUUM (FULL"
              let proof=execution.identity
              killed<-adminJson env(TE.encodeUtf8("SELECT to_jsonb(pg_terminate_backend(pid)) FROM pg_stat_activity WHERE pid="<>T.pack(show proof.backendPid)
                <>" AND floor(extract(epoch FROM backend_start)*1000000)::bigint="<>T.pack(show proof.backendStartUs)
                <>" AND datname=current_database() AND application_name='hmem-audit-maintenance'"))
              killed `shouldBe` Bool True
        withJoinedAuditQuery control (pure()) $ \controller -> do
          outcome<-executeAuditOperation defaultExecutionHooks execution
          waitCatch controller >>=either throwIO pure
          Host.backendHasExited execution.native `shouldReturn` Right True
          DB.valueField "executed" outcome `shouldBe` Just False
          DB.valueField "failed" outcome `shouldBe` Just True
          pure outcome
    it "joins an automatic worker shutdown while an actual FULL command is waiting" $ \env -> void $
      runOwnedAuditRun (\_ _ -> withRuntime env $ \base _ -> do
        prepareSmallBloat env
        rows<-auditRows env
        let cfg=(smallConfig base) {C.auditMaintenance=(smallConfig base).auditMaintenance {C.heapCompactionEnabled=True,C.lockTimeoutMs=5000}}
        verifyBackupRestore env cfg rows
        withAuditReadLock cfg $
          withRetainedWorker (Worker.startAuditWorker cfg) $ \worker -> do
            awaitActualKind env "VACUUM (FULL"
            joined<-Worker.drainAuditWorker worker
            case joined of Just(Right())->pure();_->fail "automatic FULL shutdown did not join"
            outcome<-Worker.readAuditWorkerStatus worker
            DB.valueField "server_error" outcome `shouldBe` Just("maintenance_sqlstate_57014"::Text)
            DB.valueField "stop_reason" outcome `shouldBe` Just("shutdown"::Text)
            auditRows env `shouldReturn` rows
            pure outcome) consume
    it "preserves and reports real partial concurrent-index artifacts after cancellation or connection loss" $ \env ->
      forM_ ["cancel","connection_loss"] (partialIndexCase env)
    it "index_timeout expires during the real concurrent phase and preserves live indexes and history" $ \env ->
      partialIndexCase env "timeout"
  around (\action->withLocalSandboxAppEnv(\env app->action(env,app))) $ describe "Audit in-rewrite interruption" $ do
    it "cancels during actual heap index rebuild to bound a foreground audit read and application write" $ \(env,app) -> do
      (taskPath,revertPath)<-prepareApiHistory env app
      withRewriteBarrier env $ ownedHeapWithAppends True env $ \execution ->
        withRewriteBarrierLock execution.config $ do
          DB.session execution.connection(S.sql "SET lock_timeout='5s'")
          before<-DB.operationCatalog execution.connection
          beforeAllocation<-measureCatalog env before
          let control=do
                proof<-awaitRewritePhase env execution
                putStrLn("heap_interruption_progress="<>show proof)
                began<-getMonotonicTimeNSec
                withJoinedAuditQuery (apiJson app "PUT" taskPath(object["title".=("interference_after"::Text)])) (pure()) $ \writer -> do
                  bracket (createPool(C.connectionString execution.config.database) 1 30 500) destroyAllResources $ \readerPool -> do
                    readValue<-runSession readerPool(S.statement () (St.Statement "SELECT to_jsonb(count(*)) FROM ONLY public.audit_log" E.noParams(D.singleRow(D.column(D.nonNullable D.jsonb))) False))
                    case fromJSON readValue of Success(rows::Integer)->rows `shouldSatisfy` (>0);Error message->fail message
                  waitCatch writer >>=either throwIO (const(pure()))
                ended<-getMonotonicTimeNSec
                (ended-began) `shouldSatisfy` (<500000000)
                apiJson app "GET" taskPath Null >>= \task -> DB.valueField "title" task `shouldBe` Just("interference_after"::Text)
          withJoinedAuditQuery control (pure()) $ \controller -> do
            outcome<-executeAuditOperation defaultExecutionHooks execution
            waitCatch controller >>=either throwIO pure
            DB.valueField "server_error" outcome `shouldBe` Just("maintenance_sqlstate_57014"::Text)
            DB.valueField "stop_reason" outcome `shouldBe` Just("foreground_pressure"::Text)
            DB.valueField "executed" outcome `shouldBe` Just False
            Host.backendHasExited execution.native `shouldReturn` Right True
            after<-allocatedCatalog env execution.config
            map (\(entry,_)->(DB.valueField "name" entry::Maybe Text,DB.valueField "filenode" entry::Maybe Int64)) after
              `shouldBe` map (\(entry,_)->(DB.valueField "name" entry::Maybe Text,DB.valueField "filenode" entry::Maybe Int64)) beforeAllocation
            pure outcome
      _<-apiJson app "POST" revertPath Null
      assertTaskSnapshot app taskPath "physical_before" beforeDescription
    it "settles actual in-rewrite connection loss without losing audit values or revert capability" $ \(env,app) -> do
      (taskPath,revertPath)<-prepareApiHistory env app
      withRewriteBarrier env $ ownedHeap env $ \execution ->
        withRewriteBarrierLock execution.config $ do
          DB.session execution.connection(S.sql "SET lock_timeout='5s'")
          before<-DB.operationCatalog execution.connection
          beforeAllocation<-measureCatalog env before
          let control=awaitRewritePhase env execution>>terminateFixtureBackend env execution.identity
          withJoinedAuditQuery control (pure()) $ \controller -> do
            outcome<-executeAuditOperation defaultExecutionHooks execution
            waitCatch controller >>=either throwIO pure
            Host.backendHasExited execution.native `shouldReturn` Right True
            DB.valueField "executed" outcome `shouldBe` Just False
            DB.valueField "failed" outcome `shouldBe` Just True
            after<-allocatedCatalog env execution.config
            after `shouldBe` beforeAllocation
            pure outcome
      _<-apiJson app "POST" revertPath Null
      assertTaskSnapshot app taskPath "physical_before" beforeDescription
  around withTestEnv $ describe "AuditServerRestart" $ do
    it "reconciles actual in-rewrite heap and partial concurrent-index restarts without duplicate scheduled work" $ \ambient -> void $
      runOwnedAuditRun (\_ _ -> do
        admin<-adminJson ambient "SELECT to_jsonb(session_user::text)" >>=either (fail . show) pure . DB.parseValue
        withTestSandbox $ \sandbox -> forM_ ["heap","index"] $ \kind ->
          withRestartFixture sandbox admin $ \env stopServer startServer -> withRuntime env $ \base _ -> do
            app<-privateApplication env
            (taskPath,revertPath)<-prepareApiHistory env app
            prepareSmallBloat env
            let cfg=(smallConfig base) {C.auditMaintenance=(smallConfig base).auditMaintenance
                  {C.heapCompactionEnabled=kind=="heap",C.lockTimeoutMs=5000,C.maximumWalBytes=268435456}}
                qualify=do
                  requireFixtureCapacity cfg
                  rows<-auditRows env
                  when(kind=="heap")(verifyBackupRestore env cfg rows)
                  assertRestartFootprint env
                  before<-allocatedCatalog env cfg
                  attemptCell<-newEmptyMVar
                  let control=do
                        attempt<-awaitRunningAttempt env kind
                        putMVar attemptCell attempt
                        if kind=="heap" then do
                          pid<-field "backend_pid" attempt
                          started<-field "backend_start_us" attempt
                          target<-field "target_oid" attempt
                          progress<-awaitRewriteBound env pid started target
                          putStrLn("heap_restart_progress="<>show progress)
                        else awaitPartialIndexPhase env attempt
                        stopServer
                  operationResult<-try @SomeException $ withRestartBlocker cfg kind $
                    withJoinedAuditQuery control stopServer $ \controller -> do
                      value<-Worker.runAuditCycle cfg Worker.ScheduledAudit
                      waitCatch controller >>=either throwIO pure
                      pure value
                  stopServer
                  destroyAllResources env.pool
                  startServer
                  outcome<-either throwIO pure operationResult
                  DB.valueField "operation" outcome `shouldBe` Just kind
                  DB.valueField "executed" outcome `shouldBe` Just Null
                  DB.valueField "uncertain" outcome `shouldBe` Just True
                  attempt<-readMVar attemptCell
                  freshIdentity<-adminJson env "SELECT jsonb_build_object('system_identifier',(SELECT system_identifier::text FROM pg_control_system()),'database_oid',(SELECT oid::bigint FROM pg_database WHERE datname=current_database()),'postmaster_start',pg_postmaster_start_time())"
                  (DB.valueField "system_identifier" freshIdentity::Maybe Text) `shouldBe` DB.valueField "system_identifier" attempt
                  (DB.valueField "database_oid" freshIdentity::Maybe Int64) `shouldBe` DB.valueField "database_oid" attempt
                  (DB.valueField "postmaster_start" freshIdentity::Maybe Value) `shouldNotBe` DB.valueField "postmaster_start" attempt
                  recovered<-Worker.runAuditCycle cfg Worker.ScheduledAudit
                  DB.valueField "operation_id" recovered `shouldBe` (DB.valueField "id" attempt::Maybe UUIDCore.UUID)
                  DB.valueField "reason" recovered `shouldBe` Just(if kind=="heap" then "no_rewrite_observed_after_lost_ack" else "invalid_bound_indexes_operator_remediation"::Text)
                  auditRows env `shouldReturn` rows
                  after<-allocatedCatalog env cfg
                  assertOriginalRelations before after
                  assertAuditPrimaryRead env rows
                  firstStatus<-adminJson env "SELECT jsonb_build_object('operation_id',operation->>'id','claimed_date',last_scheduled_date) FROM public.audit_maintenance_control"
                  _<-Worker.runAuditCycle cfg Worker.ScheduledAudit
                  adminJson env "SELECT jsonb_build_object('operation_id',operation->>'id','claimed_date',last_scheduled_date) FROM public.audit_maintenance_control" `shouldReturn` firstStatus
                  when(kind=="index") $ do
                    any(\(entry,_)->DB.valueField "kind" entry==Just("index"::Text)&&DB.valueField "valid" entry==Just False) after `shouldBe` True
                  _<-apiJson app "POST" revertPath Null
                  assertTaskSnapshot app taskPath "physical_before" beforeDescription
                  assertRestartFootprint env
                  putStrLn("restart_reconciliation_kind="<>T.unpack kind<>" same_date_no_duplicate=PASS")
            if kind=="heap" then withRewriteBarrier env qualify else qualify
        pure(DB.reasonValue "restart_qualification_complete")) consume
  around (\action->withLocalSandboxAppEnv(\env app->action(env,app))) $ describe "Audit concurrent index smoke" $ do
    it "automatically rebuilds a bloated bound index concurrently and preserves audit values" $ \(env,app) -> void $
      runOwnedAuditRun (\_ _ -> withRuntime env $ \base _ -> do
        let cfg=(physicalConfig base False) {C.auditMaintenance=(physicalConfig base False).auditMaintenance {C.lockTimeoutMs=5000}}
        requireFixtureCapacity cfg
        (taskPath,_)<-prepareApiHistory env app
        prepareBloat env
        beforeRows<-auditRows env
        before<-allocatedCatalog env cfg
        ready<-newEmptyMVar
        outcome<-withJoinedAuditQuery (foregroundIndexProof env cfg app taskPath ready) (pure()) $ \foreground -> do
          timeout 2000000(readMVar ready) `shouldReturn` Just ()
          completed<-awaitPhysicalWorker cfg "index"
          waitCatch foreground >>=either throwIO pure
          pure completed
        DB.valueField "executed" outcome `shouldBe` Just True
        -- Concurrent audited writes append history; every earlier complete row
        -- (including attribution/old/new values) remains byte-for-value equal.
        afterRows<-auditRows env
        case (fromJSON beforeRows::Result[Value],fromJSON afterRows::Result[Value]) of
          (Success old,Success new)->mapM_ (\entry->new `shouldContain` [entry]) old
          _->fail "audit history shape unknown"
        after<-allocatedCatalog env cfg
        let indexBytes entries=sum [size|(entry,size)<-entries,DB.valueField "kind" entry==Just("index"::Text)]
        indexBytes after `shouldSatisfy` (<indexBytes before)
        assertIndexesValid env
        putStrLn("physical_index_allocated_before="<>show(indexBytes before)<>" after="<>show(indexBytes after))
        pure outcome) consume

consume :: AuditRunResult -> IO Value
consume (AuditRunJoined value)=either throwIO pure value
consume (AuditRunPending owner)=quarantineAuditRun (const(pure())) owner

heapOid :: Value -> IO Int64
heapOid identity=case [oid | relation<-maybe [] id (DB.valueField "relations" identity :: Maybe [Value])
  , DB.valueField "kind" relation==Just ("heap"::Text), Just oid<-[DB.valueField "oid" relation]] of
  [oid] -> pure oid
  _ -> fail "fixed audit heap missing"

-- These smokes use an explicit all-day UTC window and fixture-only reserve.
-- Production default/local-window and supervised process qualification are
-- separate gates. At most 256 MiB relations plus 512 MiB WAL are admitted here.
physicalConfig :: C.HMemConfig -> Bool -> C.HMemConfig
physicalConfig cfg heap=cfg {C.auditMaintenance=cfg.auditMaintenance
  {C.heapCompactionEnabled=heap,C.timezone="UTC",C.windowStart="00:00",C.windowEnd="23:59"
  ,C.assessmentIntervalSeconds=1,C.maximumOperationSeconds=30,C.maximumWalBytes=536870912}}

requireFixtureCapacity :: C.HMemConfig -> IO ()
requireFixtureCapacity cfg=DB.withAuditConnection cfg $ \connection -> do
  DB.tryAuditLock connection `shouldReturn` True
  identity<-DB.auditIdentity connection
  proof<-either (fail . show) pure (DB.parseValue identity)
  result<-Host.withAuditHost proof $ \host -> do
    capacity<-Host.sampleAuditHost host >>=either (fail . T.unpack) pure
    mapM_ (\volume -> volume.available `shouldSatisfy` (>=2147483648)) capacity.volumes
  result `shouldBe` Right ()

adminSql :: TestEnv -> BS.ByteString -> IO ()
adminSql env sql=withConn env.pool $ \connection -> S.run (S.sql sql) connection >>=either (fail . show) pure
adminJson :: TestEnv -> BS.ByteString -> IO Value
adminJson env sql=withConn env.pool $ \connection -> rawJson connection sql
rawJson :: H.Connection -> BS.ByteString -> IO Value
rawJson connection sql=S.run (S.statement () (St.Statement sql E.noParams (D.singleRow(D.column(D.nonNullable D.jsonb))) False)) connection >>=either (fail . show) pure
auditRows :: TestEnv -> IO Value
auditRows env=adminJson env "SELECT coalesce(jsonb_agg(to_jsonb(a) ORDER BY id),'[]') FROM ONLY public.audit_log a"

prepareBloat :: TestEnv -> IO ()
prepareBloat env=do
  adminSql env "CREATE EXTENSION IF NOT EXISTS pgstattuple"
  adminSql env "ALTER TABLE public.audit_log SET(autovacuum_enabled=false,toast.autovacuum_enabled=false)"
  withConn env.pool $ \connection -> bracket_
    (S.run (S.sql "BEGIN") connection >>=either (fail . show) pure)
    (S.run (S.sql "ROLLBACK") connection >>=either (fail . show) pure) $
      forM_ [0..9::Int] $ \batch -> do
        let sql=TE.encodeUtf8(T.pack("INSERT INTO public.audit_log(entity_type,entity_id,action,new_values) SELECT 'physical_fixture',(SELECT string_agg(md5((g*1000+s)::text),'') FROM generate_series(1,64) s),'create',jsonb_build_object('payload',(SELECT string_agg(md5((g*1000+s)::text),'') FROM generate_series(1,192) s)) FROM generate_series("<>show(batch*1000+1)<>","<>show((batch+1)*1000)<>") g"))
        S.run (S.sql sql) connection >>=either (fail . show) pure
        size<-rawJson connection "SELECT to_jsonb(pg_database_size(current_database()))"
        case fromJSON size of
          Success (bytes::Integer) -> bytes `shouldSatisfy` (<268435456)
          Error message -> fail message
        wal<-rawJson connection "SELECT to_jsonb(coalesce(sum(size),0)::bigint) FROM pg_ls_waldir()"
        case fromJSON wal of
          Success (bytes::Integer) -> bytes `shouldSatisfy` (<536870912)
          Error message -> fail message
  adminSql env "INSERT INTO public.audit_log(entity_type,entity_id,action,new_values) VALUES('physical_fixture','committed_tail','create','{\"value\":42}')"
  adminSql env "VACUUM(INDEX_CLEANUP ON,TRUNCATE FALSE) ONLY public.audit_log"

awaitPhysicalWorker :: C.HMemConfig -> Text -> IO Value
awaitPhysicalWorker cfg expected=withRetainedWorker (Worker.startAuditWorker cfg) $ \worker -> do
  let wait=do
        value<-Worker.readAuditWorkerStatus worker
        if DB.valueField "executed" value==Just True then pure value else
          if DB.valueField "reason" value/=Just ("starting"::Text)
            then fail("physical smoke outcome: "<>show value)
            else threadDelay 20000>>wait
  result<-timeout 45000000 wait
  value<-case result of Just value->pure value;Nothing->Worker.readAuditWorkerStatus worker >>=fail . ("physical smoke timed out: "<>) . show
  DB.valueField "operation" value `shouldBe` Just expected
  pure value

-- A failed bounded stop is a pending handle to this exact worker. Retrying it
-- retains its credentials/fixture until the actual completion cell is joined.
withRetainedWorker :: IO Worker.AuditWorker -> (Worker.AuditWorker -> IO a) -> IO a
withRetainedWorker acquire use=mask $ \restore -> do
  worker<-acquire
  result<-try @SomeException(restore(use worker))
  first<-newIORef(either Just (const Nothing) result)
  let remember failure=modifyIORef' first (\old->case old of Just _->old;Nothing->Just failure)
      drain=try @SomeException(Worker.drainAuditWorker worker)>>= \case
        Right Nothing -> drain
        Right(Just(Right())) -> pure()
        Right(Just(Left failure)) -> remember failure
        -- This exception interrupted the wait, not a completed worker result.
        Left failure -> remember failure>>drain
  drain
  readIORef first >>=maybe (either throwIO pure result) throwIO

assertIndexesValid :: TestEnv -> IO ()
assertIndexesValid env=adminJson env "SELECT to_jsonb(bool_and(indisvalid AND indisready AND indislive)) FROM pg_index WHERE indrelid='public.audit_log'::regclass" >>= (`shouldBe` Bool True)

verifyBackupRestore :: TestEnv -> C.HMemConfig -> Value -> IO ()
verifyBackupRestore env cfg expected=do
  suffix<-filter(/='-') . show <$>UUID.nextRandom
  let clone="audit_restore_"<>T.pack suffix
      backup=env.testSandbox.sandboxTmpDir</>(suffix<>".dump")
      executable name="C:\\msys64\\mingw64\\bin"</>name<>".exe"
      cloneDb=cfg.database {C.name=clone}
  environment<-filter (\(key,_)->not("PG" `T.isPrefixOf` T.pack key||"HMEM" `T.isPrefixOf` T.pack key)) <$>getEnvironment
  let helper name arguments=ownedHelper (proc (executable name) arguments) {env=Just environment, std_in=NoStream,std_out=NoStream,std_err=NoStream}
  helper "pg_dump" ["--dbname",T.unpack(C.connectionString cfg.database),"--format=custom","--file",backup]
  bracket_ (adminSql env (TE.encodeUtf8("CREATE DATABASE "<>clone)))
    (adminSql env (TE.encodeUtf8("DROP DATABASE "<>clone))) $ do
      helper "pg_restore" ["--dbname",T.unpack(C.connectionString cloneDb),"--exit-on-error","--no-owner","--no-privileges",backup]
      restored<-bracket (createPool(C.connectionString cloneDb) 1 30 30000) destroyAllResources $ \pool ->
        runSession pool (S.statement () (St.Statement "SELECT coalesce(jsonb_agg(to_jsonb(a) ORDER BY id),'[]') FROM ONLY public.audit_log a" E.noParams (D.singleRow(D.column(D.nonNullable D.jsonb))) False))
      restored `shouldBe` expected
      backupHash<-T.pack . show . (hash::BS.ByteString->Digest SHA256) <$>BS.readFile backup
      let restoreHash=T.pack(show(hash(BL.toStrict(encode restored))::Digest SHA256))
      identity<-adminJson env "SELECT jsonb_build_object('system_identifier',(SELECT system_identifier::text FROM pg_control_system()),'database_oid',(SELECT oid::bigint FROM pg_database WHERE datname=current_database()),'recovery_point',pg_current_wal_lsn()::text)"
      now<-getCurrentTime
      let receipt=object ["system_identifier".=(DB.valueField "system_identifier" identity::Maybe Text),"database_oid".=(DB.valueField "database_oid" identity::Maybe Int64)
            ,"recovery_point".=(DB.valueField "recovery_point" identity::Maybe Text),"verified_at".=now,"restore_verified".=True
            ,"backup_sha256".=backupHash,"restore_evidence_sha256".=restoreHash]
      runSession env.pool (S.statement receipt (St.Statement "SELECT public.hmem_audit_backup_attest($1)" (E.param(E.nonNullable E.jsonb)) D.noResult False))
  removeFile backup

-- The exact ProcessHandle is retained through helper cancellation and exit.
-- A helper that cannot join keeps this owned root alive instead of releasing PG.
ownedHelper :: CreateProcess -> IO ()
ownedHelper=ownedHelperWith 30000000 terminateProcess

ownedHelperWith :: Int -> (ProcessHandle -> IO ()) -> CreateProcess -> IO ()
ownedHelperWith budget signal command=mask $ \restore -> do
  (_,_,_,process)<-createProcess command
  result<-try @SomeException (restore(timeout budget(waitForProcess process)))
  case result of
    Right(Just ExitSuccess) -> pure ()
    Right(Just _) -> fail "owned backup/restore helper exited unsuccessfully"
    _ -> do
      -- Signaling failure cannot skip joining the same actual process handle.
      _<-try @SomeException(signal process)
      let join=try @SomeException(waitForProcess process)>>=either(const join) pure
      _<-join
      case result of Left failure->throwIO failure;_ -> fail "owned helper failed or exceeded its deadline"

allocatedCatalog :: TestEnv -> C.HMemConfig -> IO [(Value,Integer)]
allocatedCatalog env cfg=do
  value<-DB.withAuditConnection cfg $ \connection -> do
    DB.tryAuditLock connection `shouldReturn` True
    DB.operationCatalog connection
  measureCatalog env value

measureCatalog :: TestEnv -> Value -> IO [(Value,Integer)]
measureCatalog env value=do
  entries<-either (fail . show) pure (DB.parseValue value::Either Text [Value])
  forM entries $ \entry -> do
    relative<-maybe (fail "catalog path missing") (pure . T.unpack) (DB.valueField "path" entry::Maybe Text)
    when (isAbsolute relative||any (`elem` ["..","."]) (splitDirectories relative)) (fail "non-fixture relation path")
    let file=env.testDb.testDbDataDir</>relative
    doesFileExist (file<>".1") >>= (`shouldBe` False)
    main<-allocatedFile True file
    auxiliary<-mapM (allocatedFile False) [file<>"_fsm",file<>"_vm",file<>"_init"]
    pure(entry,main+sum auxiliary)

#ifdef mingw32_HOST_OS
foreign import ccall safe "CreateFileW" createFile :: CWString -> Word32 -> Word32 -> Ptr () -> Word32 -> Word32 -> Ptr () -> IO (Ptr ())
foreign import ccall safe "GetFileInformationByHandleEx" fileInformation :: Ptr () -> Int32 -> Ptr () -> Word32 -> IO Int32
foreign import ccall unsafe "CloseHandle" closeHandle :: Ptr () -> IO Int32
foreign import ccall unsafe "GetLastError" lastError :: IO Word32
foreign import ccall safe "GetExtendedTcpTable" listenerTable :: Ptr () -> Ptr Word32 -> Int32 -> Word32 -> Int32 -> Word32 -> IO Word32

fixtureOwnsListener :: ProcessHandle -> Int -> IO Bool
fixtureOwnsListener process port=do
  identifier<-getPid process
  state<-getProcessExitCode process
  case (identifier,state) of
    (Just owner,Nothing)->allocaBytes 1048576 $ \buffer -> alloca $ \bytes -> do
      poke bytes 1048576
      status<-listenerTable buffer bytes 0 2 3 0
      unless(status==0)(fail "private fixture native listener proof unavailable")
      count<-peek(castPtr buffer::Ptr Word32)
      when(count>4096)(fail "private fixture listener-count bound")
      rows<-forM [0..fromIntegral count-1::Int] $ \number -> do
        let row=buffer `plusPtr` (4+number*24)
        tcpState<-peekByteOff row 0::IO Word32
        address<-peekByteOff row 4::IO Word32
        rawPort<-peekByteOff row 8::IO Word32
        pid<-peekByteOff row 20::IO Word32
        let hostPort=fromIntegral((rawPort .&. 255)*256+((rawPort `div` 256) .&. 255))::Int
        pure(tcpState,address,hostPort,pid)
      pure([ (address,pid)|(tcpState,address,local,pid)<-rows,tcpState==2,local==port,address==0||address==0x0100007f]
        ==[(0x0100007f,fromIntegral owner)])
    _->pure False
allocatedFile :: Bool -> FilePath -> IO Integer
allocatedFile required path=runInBoundThread $ withCWString path $ \wide -> bracket
  (createFile wide 0x80 7 nullPtr 3 0x200000 nullPtr)
  (\handle->when (ptrToIntPtr handle/=(-1)) (void(closeHandle handle))) $ \handle ->
    if ptrToIntPtr handle==(-1) then lastError >>= \code ->
      if not required&&(code==2||code==3) then pure 0 else fail("fixture file open error "<>show code)
    else allocaBytes 24 $ \buffer -> do
      tags<-fileInformation handle 9 buffer 8
      unless(tags/=0) (fail "fixture attribute query failed")
      attributes<-peek(castPtr buffer::Ptr Word32)
      when(attributes .&. 0xe10/=0) (fail "fixture allocation requires an ordinary uncompressed, non-sparse file")
      ok<-fileInformation handle 1 buffer 24
      unless(ok/=0) (fail "fixture allocated-size query failed")
      size<-peek(castPtr buffer::Ptr Int64)
      when(size<0) (fail "negative allocation")
      pure(toInteger size)
#else
fixtureOwnsListener :: ProcessHandle -> Int -> IO Bool
fixtureOwnsListener _ _=fail "private restart fixture requires qualified Windows listener ownership"
allocatedFile :: Bool -> FilePath -> IO Integer
allocatedFile _ _=fail "native allocated-file smoke requires qualified Windows host"
#endif

smallConfig :: C.HMemConfig -> C.HMemConfig
smallConfig cfg=(physicalConfig cfg False) {C.auditMaintenance=(physicalConfig cfg False).auditMaintenance
  {C.minimumReclaimBytes=0,C.minimumReclaimFraction=0.01}}

prepareSmallBloat :: TestEnv -> IO ()
prepareSmallBloat env=do
  adminSql env "CREATE EXTENSION IF NOT EXISTS pgstattuple; ALTER TABLE public.audit_log SET(autovacuum_enabled=false,toast.autovacuum_enabled=false)"
  adminSql env "BEGIN; INSERT INTO public.audit_log(entity_type,entity_id,action,new_values) SELECT 'small_fixture',g::text,'create',jsonb_build_object('value',g) FROM generate_series(1,5000) g; ROLLBACK"
  adminSql env "INSERT INTO public.audit_log(entity_type,entity_id,action,new_values) VALUES('small_fixture','tail','create','{\"value\":42}')"
  adminSql env "VACUUM(INDEX_CLEANUP ON,TRUNCATE FALSE) ONLY public.audit_log"

ownedIndex :: TestEnv -> (AuditExecution -> IO Value) -> IO ()
ownedIndex env use=void $ runOwnedAuditRun (\operationId _ -> withRuntime env $ \base _ -> do
  adminSql env "CREATE EXTENSION IF NOT EXISTS pgstattuple; INSERT INTO public.audit_log(entity_type,entity_id,action,old_values,new_values,request_id) VALUES('index_fixture','retained','update','{\"value\":1}','{\"value\":2}','index-fixture-request')"
  withExecution env (physicalConfig base False) operationId "index" use) consume

ownedHeap :: TestEnv -> (AuditExecution -> IO Value) -> IO ()
ownedHeap=ownedHeapWithAppends False

ownedHeapWithAppends :: Bool -> TestEnv -> (AuditExecution -> IO Value) -> IO ()
ownedHeapWithAppends allowAppends env use=void $ runOwnedAuditRun (\operationId _ -> withRuntime env $ \base _ -> do
  prepareSmallBloat env
  rows<-auditRows env
  let cfg=(smallConfig base) {C.auditMaintenance=(smallConfig base).auditMaintenance {C.heapCompactionEnabled=True}}
  verifyBackupRestore env cfg rows
  result<-withExecution env cfg operationId "heap" use
  if not allowAppends then auditRows env `shouldReturn` rows else do
    after<-auditRows env
    case (fromJSON rows::Result[Value],fromJSON after::Result[Value]) of
      (Success old,Success new)->mapM_(\entry->new `shouldContain` [entry]) old
      _->fail "audit history shape unknown"
  assertIndexesValid env
  pure result) consume

withExecution :: TestEnv -> C.HMemConfig -> UUIDCore.UUID -> Text -> (AuditExecution -> IO Value) -> IO Value
withExecution _ cfg operationId kind use=
  DB.withAuditConnection cfg $ \connection -> do
    DB.tryAuditLock connection `shouldReturn` True
    identity<-DB.auditIdentity connection >>=either (fail . show) pure . DB.parseValue
    target<-case [relation.oid|relation<-identity.relations,relation.kind==kind,T.isPrefixOf "public." relation.name] of
      oid:_ -> pure oid
      _ -> fail "fixed audit index missing"
    assessment<-DB.auditAssessment connection
    schedule<-DB.auditSchedule connection cfg.auditMaintenance Nothing False
    control<-DB.auditStatus connection
    result<-Host.withAuditHost identity $ \host -> use AuditExecution
      {config=cfg,operationId=operationId,requestStop=pure Nothing,connection=connection,native=host,identity=identity
      ,checkCapacity=Host.sampleAuditHost host >>=pure . either Left (const(Right()))
      ,kind=kind,target=target,scheduled=False,schedule=schedule,assessment=assessment,control=control,admitted=const(pure())}
    either (fail . T.unpack) pure result

withAuditReadLock :: C.HMemConfig -> IO a -> IO a
withAuditReadLock cfg use=bracket (createPool(C.connectionString cfg.database) 1 30 10000) destroyAllResources $ \lockPool ->
  withConn lockPool $ \locker -> do
    let raw statement=S.run(S.sql statement) locker >>=either (fail . show) pure
    bracket_ (raw "BEGIN; LOCK TABLE public.audit_log IN ACCESS SHARE MODE") (raw "ROLLBACK") use

withOldAuditSnapshot :: C.HMemConfig -> IO a -> IO a
withOldAuditSnapshot cfg use=bracket (createPool(C.connectionString cfg.database) 1 30 10000) destroyAllResources $ \lockPool ->
  withConn lockPool $ \locker -> do
    let raw statement=S.run(S.sql statement) locker >>=either (fail . show) pure
    bracket_ (raw "BEGIN ISOLATION LEVEL REPEATABLE READ; SELECT count(*) FROM public.audit_log") (raw "ROLLBACK") use

withRewriteBarrier :: TestEnv -> IO a -> IO a
withRewriteBarrier env=bracket_
  (adminSql env "CREATE FUNCTION public.audit_fixture_rewrite_barrier(text) RETURNS text LANGUAGE plpgsql IMMUTABLE AS $$ BEGIN IF current_setting('application_name')='hmem-audit-maintenance' THEN PERFORM pg_advisory_xact_lock(717784,1); END IF; RETURN $1; END $$; CREATE INDEX audit_fixture_rewrite_barrier ON public.audit_log(public.audit_fixture_rewrite_barrier(entity_id))")
  (adminSql env "DROP INDEX public.audit_fixture_rewrite_barrier; DROP FUNCTION public.audit_fixture_rewrite_barrier(text)")

withRewriteBarrierLock :: C.HMemConfig -> IO a -> IO a
withRewriteBarrierLock cfg use=bracket (createPool(C.connectionString cfg.database) 1 30 10000) destroyAllResources $ \lockPool ->
  withConn lockPool $ \locker -> do
    let raw statement=S.run(S.sql statement) locker >>=either (fail . show) pure
    bracket_ (raw "SELECT pg_advisory_lock(717784,1)") (raw "SELECT pg_advisory_unlock(717784,1)") use

awaitRewritePhase :: TestEnv -> AuditExecution -> IO Value
awaitRewritePhase env execution=awaitRewriteBound env (fromIntegral execution.identity.backendPid) execution.identity.backendStartUs execution.target

awaitRewriteBound :: TestEnv -> Int64 -> Int64 -> Int64 -> IO Value
awaitRewriteBound env pid started target=do
  let query="SELECT coalesce((SELECT jsonb_build_object('observed_at',clock_timestamp(),'backend_pid',p.pid,'backend_start_us',"<>T.pack(show started)<>",'target_oid',p.relid,'phase',p.phase,'heap_tuples_written',p.heap_tuples_written,'access_exclusive',EXISTS(SELECT 1 FROM pg_locks l WHERE l.pid=p.pid AND l.locktype='relation' AND l.relation=p.relid AND l.mode='AccessExclusiveLock' AND l.granted),'advisory_wait',EXISTS(SELECT 1 FROM pg_locks l WHERE l.pid=p.pid AND l.locktype='advisory' AND l.classid=717784 AND l.objid=1 AND l.objsubid=2 AND NOT l.granted)) FROM pg_stat_progress_cluster p WHERE p.pid="<>T.pack(show pid)
        <>" AND EXISTS(SELECT 1 FROM pg_stat_activity a WHERE a.pid=p.pid AND floor(extract(epoch FROM a.backend_start)*1000000)::bigint="<>T.pack(show started)<>") AND p.relid="<>T.pack(show target)<>" AND p.phase='rebuilding index' AND p.heap_tuples_written>0),'null'::jsonb)"
      loop=adminJson env(TE.encodeUtf8 query)>>= \value ->
        if DB.valueField "access_exclusive" value==Just True&&DB.valueField "advisory_wait" value==Just True
          then pure value else threadDelay 1000>>loop
  timeout 2000000 loop >>= \case
    Nothing->fail "real FULL rewrite/index-rebuild phase was not observed"
    Just value->do
      DB.valueField "access_exclusive" value `shouldBe` Just True
      DB.valueField "advisory_wait" value `shouldBe` Just True
      pure value

terminateFixtureBackend :: TestEnv -> Host.HostIdentity -> IO ()
terminateFixtureBackend env proof=do
  killed<-adminJson env(TE.encodeUtf8("SELECT to_jsonb(pg_terminate_backend(pid)) FROM pg_stat_activity WHERE pid="<>T.pack(show proof.backendPid)
    <>" AND floor(extract(epoch FROM backend_start)*1000000)::bigint="<>T.pack(show proof.backendStartUs)
    <>" AND datname=current_database() AND application_name='hmem-audit-maintenance'"))
  killed `shouldBe` Bool True

-- Fixture administrator cleanup only after the actual operation/backend joined
-- and the old snapshot closed. Authority comes from this isolated cluster and
-- exact before-OID set, bound parent, invalid state and absence of constraints;
-- runtime maintenance has no DROP capability and never performs this cleanup.
cleanupNewInvalidIndexes :: TestEnv -> Value -> IO ()
cleanupNewInvalidIndexes env before=do
  entries<-either(fail . show) pure(DB.parseValue before::Either Text [Value])
  let original=[T.pack(show oid)|entry<-entries,Just oid<-[DB.valueField "oid" entry::Maybe Int64]]
  when(null original)(fail "fixture before-index ownership missing")
  adminSql env(TE.encodeUtf8("DO $$ DECLARE target oid; BEGIN FOR target IN SELECT indexrelid FROM pg_index WHERE indrelid='public.audit_log'::regclass AND NOT indisvalid AND indexrelid NOT IN("<>T.intercalate "," original<>") AND NOT EXISTS(SELECT 1 FROM pg_constraint WHERE conindid=indexrelid) LOOP EXECUTE format('DROP INDEX %s',target::regclass); END LOOP; END $$"))

assertAuditPrimaryRead :: TestEnv -> Value -> IO ()
assertAuditPrimaryRead env rows=do
  entries<-either(fail . show) pure(DB.parseValue rows::Either Text [Value])
  entry<-case entries of first:_->pure first;_->fail "nonempty audit history required"
  identifier<-maybe(fail "audit id missing") pure(DB.valueField "id" entry::Maybe UUIDCore.UUID)
  withConn env.pool $ \reader -> do
    let raw statement=S.run(S.sql statement) reader >>=either (fail . show) pure
        query="SELECT to_jsonb(a) FROM ONLY public.audit_log a WHERE id='"<>UUIDCore.toText identifier<>"'"
        planQuery="EXPLAIN(FORMAT JSON) SELECT id FROM ONLY public.audit_log WHERE id='"<>UUIDCore.toText identifier<>"'"
    bracket_ (raw "BEGIN; SET LOCAL enable_seqscan=off; SET LOCAL statement_timeout='200ms'") (raw "ROLLBACK") $ do
      plan<-S.run(S.statement ()(St.Statement (TE.encodeUtf8 planQuery) E.noParams (D.singleRow(D.column(D.nonNullable D.json))) False)) reader >>=either (fail . show) pure
      show (plan::Value) `shouldContain` "audit_log_pkey"
      rawJson reader(TE.encodeUtf8 query) `shouldReturn` entry

diagnoseTimeoutFailure :: TestEnv -> AuditExecution -> SomeException -> IO ()
diagnoseTimeoutFailure env execution failure=do
  native<-Host.backendHasExited execution.native
  metadata<-try @SomeException(adminJson env "SELECT jsonb_build_object('state',operation->>'state','id',operation->>'id','backend_present',EXISTS(SELECT 1 FROM pg_stat_activity WHERE pid=(operation->>'backend_pid')::int AND floor(extract(epoch FROM backend_start)*1000000)::bigint=(operation->>'backend_start_us')::bigint)) FROM public.audit_maintenance_control")
  putStrLn("FULL_timeout_failure="<>show failure<>" native_exit="<>show native<>" operation="<>either(const "metadata_unavailable") show metadata)
  diagnostic<-try @SomeException $ withBinaryFile env.testDb.testDbLogFile ReadMode $ \handle -> do
    bytes<-hFileSize handle
    hSeek handle AbsoluteSeek(max 0(bytes-16384))
    content<-TE.decodeUtf8 <$>BS.hGet handle 16384
    let relevant=filter(\line->any (`T.isInfixOf` line)["FATAL","could not","connection","terminat","canceling statement"]) (T.lines content)
    pure(T.take 4096(T.unlines relevant))
  either (const(pure())) (putStrLn . T.unpack) diagnostic

field :: FromJSON a => Text -> Value -> IO a
field name value=maybe(fail("fixture field missing: "<>T.unpack name)) pure(DB.valueField name value)

privateApplication :: TestEnv -> IO Wai.Application
privateApplication env=do
  tracker<-newAccessTracker env.pool 3600
  websocket<-newWSState
  let cfg=mkLocalSandboxConfig env
  mkApp id cfg.auth cfg.cors cfg.rateLimit env.pool tracker websocket cfg.web.webStaticDir True

awaitRunningAttempt :: TestEnv -> Text -> IO Value
awaitRunningAttempt env kind=do
  let loop=adminJson env "SELECT coalesce(operation,'null'::jsonb) FROM public.audit_maintenance_control" >>= \value ->
        if DB.valueField "kind" value==Just kind&&DB.valueField "state" value==Just("running"::Text)
          then pure value else threadDelay 1000>>loop
  timeout 3000000 loop >>=maybe(fail "scheduled physical attempt was not observed") pure

awaitPartialIndexPhase :: TestEnv -> Value -> IO ()
awaitPartialIndexPhase env attempt=do
  pid<-field @Int64 "backend_pid" attempt
  started<-field @Int64 "backend_start_us" attempt
  relations<-field @[Value] "before" attempt
  heap<-case [oid|entry<-relations,DB.valueField "kind" entry==Just("heap"::Text),Just oid<-[DB.valueField "oid" entry::Maybe Int64]] of
    [oid]->pure oid
    _->fail "partial index parent binding missing"
  let query="SELECT to_jsonb(EXISTS(SELECT 1 FROM pg_stat_progress_create_index p JOIN pg_stat_activity a ON a.pid=p.pid WHERE p.pid="<>T.pack(show pid)
        <>" AND floor(extract(epoch FROM a.backend_start)*1000000)::bigint="<>T.pack(show started)
        <>" AND p.relid="<>T.pack(show heap)<>" AND EXISTS(SELECT 1 FROM pg_index i WHERE i.indexrelid=p.index_relid AND i.indrelid=p.relid)"
        <>" AND a.state='active' AND a.query LIKE 'REINDEX INDEX CONCURRENTLY %' AND p.command='REINDEX CONCURRENTLY' AND p.phase='waiting for old snapshots'))"
      loop=adminJson env(TE.encodeUtf8 query)>>= \value ->if value==Bool True then pure() else threadDelay 1000>>loop
  timeout 3000000 loop `shouldReturn` Just ()

withRestartBlocker :: C.HMemConfig -> Text -> IO a -> IO a
withRestartBlocker cfg kind use=bracket (createPool(C.connectionString cfg.database) 1 30 10000) destroyAllResources $ \lockPool ->
  withConn lockPool $ \locker -> do
    let raw statement=S.run(S.sql statement) locker >>=either (fail . show) pure
        acquire=if kind=="heap" then raw "SELECT pg_advisory_lock(717784,1)"
          else raw "BEGIN ISOLATION LEVEL REPEATABLE READ; SELECT count(*) FROM public.audit_log"
        release=void(try @SomeException(raw(if kind=="heap" then "SELECT pg_advisory_unlock(717784,1)" else "ROLLBACK")))
    -- The expected server stop has already joined the postmaster/backends
    -- before stale client release. A failed unlock/rollback is not a join proof.
    bracket_ acquire release use

assertOriginalRelations :: [(Value,Integer)] -> [(Value,Integer)] -> IO ()
assertOriginalRelations before after=forM_ before $ \(original,size) -> do
  (current,currentSize)<-case [(entry,allocated)|(entry,allocated)<-after,DB.valueField "name" entry==(DB.valueField "name" original::Maybe Text)] of
    [entry]->pure entry
    _->fail "authoritative relation missing after restart"
  currentSize `shouldBe` size
  forM_ ["oid","name","filenode","definition","constraint_oids","valid","ready","live","parent_oid"] $ \name ->
    (DB.valueField name current::Maybe Value) `shouldBe` DB.valueField name original

assertRestartFootprint :: TestEnv -> IO ()
assertRestartFootprint env=do
  walBytes<-adminJson env "SELECT to_jsonb(coalesce(sum(size),0)::bigint) FROM pg_ls_waldir()"
  count<-newIORef(0::Int)
  let walk path=do
        pathIsSymbolicLink path >>= \linked->when linked(fail "restart footprint reparse refused")
        if normalise path==normalise(env.testDb.testDbDataDir</>"pg_wal") then pure 0 else do
          folder<-doesDirectoryExist path
          if folder then listDirectory path >>=fmap sum . mapM(walk . (path</>)) else do
            n<-atomicModifyIORef' count(\n->(n+1,n+1))
            when(n>4096)(fail "restart footprint file-count bound")
            getFileSize path
  size<-walk env.testDb.testDbDataDir
  wal<-either(fail . show) pure(DB.parseValue walBytes::Either Text Integer)
  size `shouldSatisfy` (<134217728)
  wal `shouldSatisfy` (<268435456)
  putStrLn("restart_fixture_data_bytes="<>show size<>" wal_bytes="<>show wal)

-- One private subtree is reused sequentially for heap/index restart. Only this
-- owner starts/replaces its foreground postmaster; the ambient suite PG is idle
-- and untouched. The original ProcessHandle remains retained until normal exit
-- and PID-file absence; no PID rediscovery or detached process is used.
withRestartFixture :: TestSandbox -> Text -> (TestEnv -> IO () -> IO () -> IO a) -> IO a
withRestartFixture sandbox admin use=mask $ \restore -> do
  allowed<-canonicalizePath "D:\\Projects\\hmem\\.tmp\\audit-operations-1a291d34-c8b81100"
  root<-canonicalizePath sandbox.sandboxRoot
  let relative=makeRelative allowed root
  when(isAbsolute relative||any(=="..")(splitDirectories relative))(fail "restart fixture outside owned scratch")
  pathIsSymbolicLink root >>= \linked->when linked(fail "restart root reparse refused")
  let directory=root</>"postgres"</>"data"
      pidFile=directory</>"postmaster.pid"
      logPath=sandbox.sandboxLogDir</>"postgres-native.log"
      executable name="C:\\msys64\\mingw64\\bin"</>name<>".exe"
  token<-UUID.nextRandom
  let (word,_,_,_)=UUIDCore.toWords token
      initialPort=49152+fromIntegral(word .&. 16383)
      database="audit_restart_"<>T.filter(/='-')(UUIDCore.toText token)
      settings port name=C.defaultConfig.database {C.host="127.0.0.1",C.port=port,C.name=name,C.user=Just admin,C.password=Nothing,C.sslmode=Just "disable"}
      connection port name=C.connectionString(settings port name)<>" connect_timeout=1"
  selectedPort<-newIORef initialPort
  admittedEndpoint<-newIORef False
  current<-newIORef(Nothing::Maybe(ProcessHandle,Bool))
  lifecycle<-newMVar()
  cleanupCause<-newIORef(Nothing::Maybe SomeException)
  pendingReported<-newIORef False
  let remember failure=modifyIORef' cleanupCause(\old->case old of Nothing->Just failure;Just _->old)
      pending=do
        old<-atomicModifyIORef' pendingReported(\value->(True,value))
        emitted<-try @SomeException(unless old(putStrLn "restart_fixture_cleanup_pending retaining_actual_postmaster"))
        either remember pure emitted
        waited<-try @SomeException(threadDelay 100000)
        either remember pure waited
      stopServer=mask_ $ do
        let acquire=try @SomeException(withMVar lifecycle(const stopLoop))>>= \case
              Right()->pure()
              Left failure->remember failure>>acquire
        acquire
      stopLoop=do
        step<-try @SomeException stopStep
        case step of
          Right True->pure()
          Right False->pending>>stopLoop
          Left failure->remember failure>>pending>>stopLoop
      stopStep=readIORef current >>= \case
          Nothing->pure True
          Just(process,wasReady)->do
            state<-getProcessExitCode process
            exists<-doesFileExist pidFile
            case state of
              Just status | not exists && (status==ExitSuccess||not wasReady) -> do
                _<-waitForProcess process
                writeIORef current Nothing
                putStrLn("restart_fixture_postmaster_join="<>show status<>" pid_absent=True")
                pure True
              Just _ -> pure False
              Nothing | not exists -> pure False
              Nothing -> do
                contents<-T.lines . TE.decodeUtf8 <$>BS.readFile pidFile
                identifier<-getPid process
                port<-readIORef selectedPort
                let bound=case (contents,identifier) of
                      (pid:path:_:actualPort:_,Just actualPid)->T.strip pid==T.pack(show actualPid)
                        &&normalise(T.unpack(T.strip path))==normalise directory&&T.strip actualPort==T.pack(show port)
                      _->False
                if not bound then pure False else do
                  stopped<-try @SomeException(ownedHelperWith 7000000 terminateProcess
                    ((proc(executable "pg_ctl")["stop","-D",directory,"-m","fast","-w","-t","5"])
                      {std_in=NoStream,std_out=NoStream,std_err=NoStream}))
                  either remember pure stopped
                  pure False
      startServer=withMVar lifecycle $ \_ -> do
        firstSetup<-newIORef(Nothing::Maybe SomeException)
        let attempt remaining=mask $ \restoreStart -> do
              existing<-readIORef current
              case existing of Just _->fail "old postmaster not joined before replacement";Nothing->pure()
              port<-readIORef selectedPort
              appendFile(directory</>"postgresql.conf")("\nport="<>show port<>"\n")
              BS.writeFile logPath BS.empty
              putStrLn("restart_fixture_start_attempt port="<>show port<>" remaining="<>show remaining)
              process<-withFile logPath AppendMode $ \logHandle -> do
                (_,_,_,handle)<-createProcess((proc(executable "postgres")["-D",directory])
                  {std_in=NoStream,std_out=UseHandle logHandle,std_err=UseHandle logHandle})
                writeIORef current(Just(handle,False))
                pure handle
              started<-try @SomeException $ restoreStart $ bracket (createPool(connection port "postgres") 1 30 1000) destroyAllResources $ \probe -> do
                now<-getMonotonicTimeNSec
                let deadline=now+10000000000
                    waitAgain=do
                      clock<-getMonotonicTimeNSec
                      if clock>=deadline then fail "restart postmaster readiness deadline" else threadDelay 50000>>ready
                    ready=do
                      state<-getProcessExitCode process
                      case state of
                        Just status->do
                          diagnostic<-BS.readFile logPath
                          fail("restart postmaster setup exit "<>show status<>" "<>T.unpack(TE.decodeUtf8(BS.drop(max 0(BS.length diagnostic-2048)) diagnostic)))
                        Nothing->pure()
                      owns<-fixtureOwnsListener process port
                      if not owns then waitAgain else do
                        outcome<-try @SomeException(runSession probe(S.statement () (St.Statement "SELECT current_setting('data_directory')::text" E.noParams(D.singleRow(D.column(D.nonNullable D.text))) False)))
                        case outcome of
                          Right actual->do
                            when(normalise(T.unpack actual)/=normalise directory)(fail "private postmaster readiness data-directory mismatch")
                            stillOwns<-fixtureOwnsListener process port
                            unless stillOwns(fail "private postmaster lost listener ownership during readiness")
                            writeIORef current(Just(process,True))
                          Left _->waitAgain
                ready
              case started of
                Right()->writeIORef admittedEndpoint True>>putStrLn("restart_fixture_started root="<>root<>" port="<>show port)
                Left failure->case fromException failure::Maybe SomeAsyncException of
                  Just _->throwIO failure
                  Nothing->do
                    modifyIORef' firstSetup(\old->case old of Nothing->Just failure;Just _->old)
                    status<-getProcessExitCode process
                    exists<-doesFileExist pidFile
                    diagnostic<-BS.readFile logPath
                    let retryable=case status of Just(ExitFailure _)->not exists&&"could not create any TCP/IP sockets" `BS.isInfixOf` diagnostic;_->False
                    if remaining>1&&retryable then do
                      stopLoop
                      fixed<-readIORef admittedEndpoint
                      unless fixed $ do
                        next<-UUID.nextRandom
                        let (choice,_,_,_)=UUIDCore.toWords next
                        writeIORef selectedPort(49152+fromIntegral(choice .&. 16383))
                      threadDelay 200000
                      attempt(remaining-1)
                    else readIORef firstSetup >>=maybe(throwIO failure) throwIO
        attempt (5::Int)
  result<-try @SomeException $ restore $ do
    createDirectoryIfMissing True(takeDirectory directory)
    ownedHelper((proc(executable "initdb")["-D",directory,"--username",T.unpack admin,"--auth=trust","--locale=C","--encoding=UTF8"])
      {std_in=NoStream,std_out=NoStream,std_err=NoStream})
    appendFile(directory</>"postgresql.conf")("\nlisten_addresses='127.0.0.1'\nmax_wal_size='64MB'\nmin_wal_size='32MB'\ntimezone='UTC'\n")
    startServer
    port<-readIORef selectedPort
    bracket (createPool(connection port "postgres") 1 30 10000) destroyAllResources $ \initial ->
      withConn initial $ \conn -> S.run(S.sql(TE.encodeUtf8("CREATE DATABASE "<>database))) conn >>=either (fail . show) pure
    bracket (createPool(connection port database) 10 5 30000) destroyAllResources $ \pool -> do
      let db=TestDb directory logPath port database (connection port database) False
          env=TestEnv pool sandbox db
      ensureSchema env
      use env stopServer startServer
  stopServer
  -- Exact current-task-owned target is checked before recursive removal, after
  -- all clients/helpers/postmasters joined. Reparse entries are never followed.
  let target=root</>"postgres"
  resolved<-canonicalizePath target
  when(resolved/=normalise target||isAbsolute(makeRelative root resolved)||any(=="..")(splitDirectories(makeRelative root resolved)))
    (fail "restart cleanup target mismatch")
  let check path=do
        pathIsSymbolicLink path >>= \linked->when linked(fail "restart cleanup reparse refused")
        folder<-doesDirectoryExist path
        when folder(listDirectory path >>=mapM_(check . (path</>)))
  check target
  removeDirectoryRecursiveOwned target
  putStrLn "restart_fixture_data_removed_after_join"
  case result of
    Left failure->throwIO failure
    Right value->readIORef cleanupCause >>=maybe(pure value) throwIO

removeDirectoryRecursiveOwned :: FilePath -> IO ()
removeDirectoryRecursiveOwned path=do
  entries<-listDirectory path
  forM_ entries $ \entry -> do
    let child=path</>entry
    folder<-doesDirectoryExist child
    if folder then removeDirectoryRecursiveOwned child else removeFile child
  removeDirectory path

awaitActualCommand :: TestEnv -> AuditExecution -> Text -> IO ()
awaitActualCommand env execution prefix=do
  let proof=execution.identity
      query="SELECT to_jsonb(EXISTS(SELECT 1 FROM pg_stat_activity WHERE pid="<>T.pack(show proof.backendPid)
        <>" AND floor(extract(epoch FROM backend_start)*1000000)::bigint="<>T.pack(show proof.backendStartUs)
        <>" AND datname=current_database() AND state='active' AND query LIKE '"<>prefix<>"%'))"
      loop=adminJson env(TE.encodeUtf8 query)>>= \value->if value==Bool True then pure() else threadDelay 1000>>loop
  timeout 2000000 loop `shouldReturn` Just ()

awaitActualKind :: TestEnv -> Text -> IO ()
awaitActualKind env prefix=do
  let loop=adminJson env(TE.encodeUtf8("SELECT to_jsonb(EXISTS(SELECT 1 FROM pg_stat_activity WHERE datname=current_database() AND application_name='hmem-audit-maintenance' AND state='active' AND query LIKE '"<>prefix<>"%'))"))>>= \value->
        if value==Bool True then pure() else threadDelay 1000>>loop
  timeout 5000000 loop `shouldReturn` Just ()

apiJson :: Wai.Application -> BS.ByteString -> BS.ByteString -> Value -> IO Value
apiJson app method path body=do
  let request=WaiTest.defaultRequest {Wai.requestMethod=method,Wai.rawPathInfo=path
        ,Wai.pathInfo=filter(not . T.null)(T.splitOn "/"(TE.decodeUtf8 path)),Wai.requestHeaders=[("Content-Type","application/json")]}
  response<-WaiTest.runSession (WaiTest.srequest(WaiTest.SRequest request (if body==Null then "" else encode body))) app
  statusCode response.simpleStatus `shouldSatisfy` (\code->code>=200&&code<300)
  either (fail . ("fixture API response: "<>)) pure (eitherDecode response.simpleBody)

prepareApiHistory :: TestEnv -> Wai.Application -> IO(BS.ByteString,BS.ByteString)
prepareApiHistory env app=do
  workspace<-createTestWorkspace env "physical-audit-equivalence"
  created<-apiJson app "POST" "/api/v1/tasks" (object["workspace_id".=workspace.id,"title".=("physical_before"::Text),"description".=beforeDescription])
  taskId<-maybe(fail "created task id missing") pure(DB.valueField "id" created::Maybe UUIDCore.UUID)
  let taskPath="/api/v1/tasks/"<>TE.encodeUtf8(UUIDCore.toText taskId)
  _<-apiJson app "PUT" taskPath (object["title".=("physical_after"::Text),"description".=afterDescription])
  identifier<-adminJson env (TE.encodeUtf8("SELECT to_jsonb(id::text) FROM public.audit_log WHERE entity_type='task' AND entity_id='"<>UUIDCore.toText taskId<>"' AND action='update' ORDER BY changed_at DESC LIMIT 1"))
  auditId<-either(fail . show) pure(DB.parseValue identifier::Either Text UUIDCore.UUID)
  adminJson env "SELECT to_jsonb(pg_relation_size(reltoastrelid)>0) FROM pg_class WHERE oid='public.audit_log'::regclass" `shouldReturn` Bool True
  pure(taskPath,"/api/v1/audit/"<>TE.encodeUtf8(UUIDCore.toText auditId)<>"/revert")

beforeDescription,afterDescription :: Text
beforeDescription=descriptionPayload "before"
afterDescription=descriptionPayload "after"
descriptionPayload :: Text -> Text
descriptionPayload tag=T.concat[T.pack(show(hash(TE.encodeUtf8(tag<>T.pack(show number)))::Digest SHA256))|number<-[1..256::Int]]

assertTaskSnapshot :: Wai.Application -> BS.ByteString -> Text -> Text -> IO ()
assertTaskSnapshot app path title description=do
  task<-apiJson app "GET" path Null
  DB.valueField "title" task `shouldBe` Just title
  DB.valueField "description" task `shouldBe` Just description

-- An actual old MVCC snapshot keeps REINDEX CONCURRENTLY in its supported
-- multi-phase protocol while ordinary API updates/reads continue. The snapshot
-- has its own pool, so no nested checkout can deadlock a one-resource stripe.
foregroundIndexProof :: TestEnv -> C.HMemConfig -> Wai.Application -> BS.ByteString -> MVar () -> IO ()
foregroundIndexProof env cfg app path ready=
  bracket (createPool(C.connectionString cfg.database) 1 30 10000) destroyAllResources $ \snapshotPool ->
    withConn snapshotPool $ \snapshot -> do
      let sql statement=S.run(S.sql statement) snapshot >>=either (fail . show) pure
      bracket_ (sql "BEGIN ISOLATION LEVEL REPEATABLE READ; SELECT count(*) FROM public.audit_log") (sql "ROLLBACK") $ do
        taskId<-maybe(fail "foreground task path invalid") pure(UUIDCore.fromText(last(T.splitOn "/"(TE.decodeUtf8 path))))
        let predicate="entity_type='task' AND entity_id='"<>UUIDCore.toText taskId<>"' AND action='update'"
            attribution="jsonb_build_object('workspace_id',workspace_id,'actor_type',actor_type,'actor_id',actor_id,'actor_label',actor_label,'bot_identity',bot_identity)"
        initial<-adminJson env(TE.encodeUtf8("SELECT "<>attribution<>" FROM public.audit_log WHERE "<>predicate<>" ORDER BY changed_at DESC LIMIT 1"))
        putMVar ready ()
        let active=adminJson env "SELECT to_jsonb(EXISTS(SELECT 1 FROM pg_stat_progress_create_index WHERE relid='public.audit_log'::regclass))" >>= \value ->
              if value==Bool True then pure() else threadDelay 1000>>active
        timeout 5000000 active `shouldReturn` Just ()
        forM_ [1..5::Int] $ \number -> do
          began<-getMonotonicTimeNSec
          let title=T.pack("concurrent_"<>show number)
              oldTitle=if number==1 then "physical_after" else T.pack("concurrent_"<>show(number-1))
          _<-apiJson app "PUT" path (object["title".=title])
          readback<-apiJson app "GET" path Null
          DB.valueField "title" readback `shouldBe` Just title
          DB.valueField "description" readback `shouldBe` Just afterDescription
          ended<-getMonotonicTimeNSec
          (ended-began) `shouldSatisfy` (<500000000)
          transition<-adminJson env(TE.encodeUtf8("SELECT jsonb_build_object('old',old_values->>'title','new',new_values->>'title','old_description',old_values->>'description','new_description',new_values->>'description','attribution',"<>attribution<>",'request_id',request_id) FROM ONLY public.audit_log WHERE "<>predicate<>" AND new_values->>'title'='"<>title<>"'"))
          DB.valueField "old" transition `shouldBe` Just oldTitle
          DB.valueField "new" transition `shouldBe` Just title
          DB.valueField "old_description" transition `shouldBe` Just afterDescription
          DB.valueField "new_description" transition `shouldBe` Just afterDescription
          DB.valueField "attribution" transition `shouldBe` Just initial
          (DB.valueField "request_id" transition::Maybe Text) `shouldSatisfy` maybe False(not . T.null)
        withConn env.pool $ \reader -> do
          let raw statement=S.run(S.sql statement) reader >>=either (fail . show) pure
          bracket_ (raw "BEGIN; SET LOCAL enable_seqscan=off; SET LOCAL statement_timeout='200ms'") (raw "ROLLBACK") $ do
            plan<-S.run (S.statement () (St.Statement (TE.encodeUtf8("EXPLAIN(FORMAT JSON) SELECT id,old_values,new_values FROM ONLY public.audit_log WHERE "<>predicate<>" ORDER BY changed_at DESC LIMIT 10"))
              E.noParams (D.singleRow(D.column(D.nonNullable D.json))) False)) reader >>=either (fail . show) pure
            let nodes value@(Object fields)=value:concatMap nodes(KM.elems fields)
                nodes (Array values)=foldMap nodes values
                nodes _=[]
                names=[name|node<-nodes plan,DB.valueField "Node Type" node==Just("Index Scan"::Text)
                  ,DB.valueField "Relation Name" node==Just("audit_log"::Text),Just name<-[DB.valueField "Index Name" node::Maybe Text]]
                quoted value="'"<>T.replace "'" "''" value<>"'"
            names `shouldSatisfy` (not . null)
            live<-rawJson reader(TE.encodeUtf8("SELECT to_jsonb(count(*)="<>T.pack(show(length names))<>" AND bool_and(i.indisvalid AND i.indisready AND i.indislive)) FROM pg_index i JOIN pg_class c ON c.oid=i.indexrelid WHERE i.indrelid='public.audit_log'::regclass AND c.relname IN("<>T.intercalate ","(map quoted names)<>")"))
            live `shouldBe` Bool True
            titles<-rawJson reader(TE.encodeUtf8("SELECT coalesce(jsonb_agg(new_values->>'title' ORDER BY new_values->>'title'),'[]') FROM ONLY public.audit_log WHERE "<>predicate<>" AND new_values->>'title' LIKE 'concurrent_%'"))
            titles `shouldBe` toJSON[T.pack("concurrent_"<>show number)|number<-[1..5::Int]]
        putStrLn "index_foreground_audited_write_read_pairs=5 each_under_500ms"

partialIndexCase :: TestEnv -> Text -> IO ()
partialIndexCase env mode=ownedIndex env $ \original -> do
  let execution=if mode=="timeout" then original {config=original.config {C.auditMaintenance=original.config.auditMaintenance {C.maximumOperationSeconds=1}}} else original
  DB.session execution.connection(S.sql "SET lock_timeout='5s'")
  before<-DB.operationCatalog execution.connection
  beforeRows<-auditRows env
  stopping<-newIORef Nothing
  let control=do
        result<-try @SomeException $ do
          attempt<-awaitRunningAttempt env "index"
          DB.valueField "id" attempt `shouldBe` Just execution.operationId
          awaitPartialIndexPhase env attempt
          case mode of
            "connection_loss"->terminateFixtureBackend env execution.identity
            "cancel"->writeIORef stopping(Just "fixture_partial_cancel")
            _->pure()
        case result of
          Right()->pure()
          Left failure->writeIORef stopping(Just "fixture_partial_cleanup")>>throwIO failure
  bracket_ (pure()) (cleanupNewInvalidIndexes env before) $ do
    began<-getMonotonicTimeNSec
    outcome<-withOldAuditSnapshot execution.config $
      withJoinedAuditQuery control (writeIORef stopping(Just "fixture_partial_cleanup")) $ \controller -> do
        value<-executeAuditOperation defaultExecutionHooks execution {requestStop=readIORef stopping}
        waitCatch controller >>=either throwIO pure
        pure value
    ended<-getMonotonicTimeNSec
    when(mode=="timeout") $ do
      (ended-began) `shouldSatisfy` (<2500000000)
      readIORef stopping `shouldReturn` Nothing
      DB.valueField "server_error" outcome `shouldBe` Just("maintenance_sqlstate_57014"::Text)
    Host.backendHasExited execution.native `shouldReturn` Right True
    DB.valueField "reason" outcome `shouldBe` Just("invalid_bound_indexes_operator_remediation"::Text)
    DB.valueField "failed" outcome `shouldBe` Just True
    after<-DB.withAuditConnection execution.config $ \fresh -> do
      DB.tryAuditLock fresh `shouldReturn` True
      DB.operationCatalog fresh
    entries<-either(fail . show) pure(DB.parseValue after::Either Text [Value])
    originals<-either(fail . show) pure(DB.parseValue before::Either Text [Value])
    forM_ [entry|entry<-originals,DB.valueField "kind" entry==Just("index"::Text)] $ \original -> do
      current<-case [entry|entry<-entries,DB.valueField "name" entry==(DB.valueField "name" original::Maybe Text)] of
        [entry]->pure entry
        _->fail "authoritative original index disappeared"
      forM_ ["oid","name","definition","constraint_oids","valid","ready","live","parent_oid"] $ \field ->
        (DB.valueField field current::Maybe Value) `shouldBe` DB.valueField field original
      forM_ ["valid","ready","live"] $ \field -> DB.valueField field current `shouldBe` Just True
    auditRows env `shouldReturn` beforeRows
    assertAuditPrimaryRead env beforeRows
    any (\entry->DB.valueField "kind" entry==Just("index"::Text)&&DB.valueField "valid" entry==Just False) entries `shouldBe` True
    forM_ [1..2::Int] $ \_ -> do
      deferred<-Worker.runAuditCycle execution.config Worker.ManualAudit
      DB.valueField "reason" deferred `shouldBe` Just("invalid_bound_indexes_operator_remediation"::Text)
    DB.withAuditConnection execution.config $ \fresh -> do
      DB.tryAuditLock fresh `shouldReturn` True
      DB.operationCatalog fresh `shouldReturn` after
    pure outcome
