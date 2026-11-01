module HMem.Server.AuditMaintenanceSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (cancel,wait,waitCatch,withAsync)
import Control.Concurrent.MVar
import Control.Exception (IOException,bracket,bracket_,try)
import Control.Monad (void,replicateM_)
import Data.Aeson
import Data.ByteString (ByteString)
import Data.Either (isLeft)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time
import Data.UUID.V4 qualified as UUID
import Hasql.Decoders qualified as D
import Hasql.Encoders qualified as E
import Hasql.Session qualified as S
import Hasql.Statement qualified as St
import System.Directory (createDirectory,renameDirectory,removeDirectory)
import System.FilePath ((</>))
import System.Info (os)
import System.Exit (ExitCode(..))
import System.Timeout (timeout)
import Test.Hspec

import HMem.Config qualified as C
import HMem.DB.AuditMaintenance qualified as DB
import HMem.DB.AuditMaintenance.Host qualified as Host
import HMem.DB.Pool (runSession,withConn)
import HMem.DB.Pool qualified as Pool
import Data.Pool (destroyAllResources)
import HMem.DB.TestHarness
import HMem.Server.AuditMaintenance
import HMem.Server.CtlAuditMaintenance qualified as Ctl
import HMem.Server.CtlAuditMaintenanceCli qualified as Cli

spec :: Spec
spec=around withTestEnv $ describe "Audit maintenance controls" $ do
  it "migrates without pgstattuple and reports missing diagnostics without enabling/installing them" $ \env -> withRuntime env $ \cfg _ -> do
    execute env "DROP EXTENSION IF EXISTS pgstattuple"
    outcome<-runAuditCycleAt cfg ScheduledAudit (Just (instant "2026-11-01 02:30:00 UTC"))
    DB.valueField "reason" outcome `shouldBe` Just ("diagnostic_missing"::Text)
    number env "SELECT count(*) FROM pg_extension WHERE extname='pgstattuple'" `shouldReturn` 0
  it "persists one date claim across new connections, respects cadence and reports the actual next opening" $ \env -> withRuntime env $ \cfg _ -> do
    let weekly=cfg.auditMaintenance {C.intervalDays=7,C.timezone="Europe/Copenhagen"}
        schedule now=DB.withAuditConnection cfg $ \conn -> do
          DB.tryAuditLock conn `shouldReturn` True
          DB.auditSchedule conn weekly (Just (instant now)) True
    first<-schedule "2026-10-31 02:30:00 UTC"
    DB.valueField "claimed" first `shouldBe` Just True
    DB.valueField "next_eligibility" first `shouldBe` Just (instant "2026-11-07 02:00:00 UTC")
    before<-schedule "2026-11-01 01:00:00 UTC"
    DB.valueField "next_eligibility" before `shouldBe` Just (instant "2026-11-07 02:00:00 UTC")
    again<-schedule "2026-10-31 02:40:00 UTC"
    DB.valueField "reason" again `shouldBe` Just ("date_already_claimed_or_cadence"::Text)
    later<-schedule "2026-11-07 02:30:00 UTC"
    DB.valueField "claimed" later `shouldBe` Just True
  it "claims the actual execution date across an overnight window and restart, never twice in that date" $ \env -> withRuntime env $ \cfg _ -> do
    let settings=cfg.auditMaintenance {C.windowStart="23:00",C.windowEnd="01:00"}
        schedule now=DB.withAuditConnection cfg $ \conn -> do
          DB.tryAuditLock conn `shouldReturn` True
          DB.auditSchedule conn settings (Just (instant now)) True
    first<-schedule "2026-11-01 23:30:00 UTC"
    DB.valueField "claimed" first `shouldBe` Just True
    DB.valueField "date" first `shouldBe` Just (fromGregorian 2026 11 2)
    DB.valueField "window_opening_date" first `shouldBe` Just (fromGregorian 2026 11 1)
    DB.valueField "next_eligibility" first `shouldBe` Just (instant "2026-11-02 23:00:00 UTC")
    afterRestart<-schedule "2026-11-02 22:30:00 UTC"
    DB.valueField "reason" afterRestart `shouldBe` Just ("date_already_claimed_or_cadence"::Text)
    DB.valueField "next_eligibility" afterRestart `shouldBe` Just (instant "2026-11-02 23:00:00 UTC")
    nextDate<-schedule "2026-11-02 23:00:00 UTC"
    DB.valueField "claimed" nextDate `shouldBe` Just True
    missed<-schedule "2026-11-03 00:30:00 UTC"
    DB.valueField "reason" missed `shouldBe` Just ("window_closed"::Text)
  it "reports post-claim weekly overnight eligibility at the next actual date's midnight" $ \env -> withRuntime env $ \cfg _ -> do
    let settings=cfg.auditMaintenance {C.windowStart="23:00",C.windowEnd="01:00",C.intervalDays=7}
        schedule now=DB.withAuditConnection cfg $ \conn -> do
          DB.tryAuditLock conn `shouldReturn` True
          DB.auditSchedule conn settings (Just (instant now)) True
    first<-schedule "2026-11-01 23:30:00 UTC"
    DB.valueField "next_eligibility" first `shouldBe` Just (instant "2026-11-08 23:00:00 UTC")
    early<-schedule "2026-11-08 22:30:00 UTC"
    DB.valueField "reason" early `shouldBe` Just ("date_already_claimed_or_cadence"::Text)
    DB.valueField "next_eligibility" early `shouldBe` Just (instant "2026-11-08 23:00:00 UTC")
    nextDate<-schedule "2026-11-08 23:00:00 UTC"
    DB.valueField "claimed" nextDate `shouldBe` Just True
  it "resolves overnight next-date midnight gaps forward and midnight folds to their earliest occurrence" $ \env -> withRuntime env $ \cfg _ -> do
    let schedule zone now=DB.withAuditConnection cfg $ \conn -> do
          DB.tryAuditLock conn `shouldReturn` True
          DB.auditSchedule conn (cfg.auditMaintenance {C.windowStart="23:00",C.windowEnd="02:00",C.timezone=zone}) (Just (instant now)) True
    gap<-schedule "America/Sao_Paulo" "2018-11-04 02:30:00 UTC"
    DB.valueField "claimed" gap `shouldBe` Just True
    DB.valueField "next_eligibility" gap `shouldBe` Just (instant "2018-11-04 03:00:00 UTC")
    execute env "UPDATE public.audit_maintenance_control SET last_scheduled_date=NULL,last_scheduled_zone=NULL,last_claimed_at=NULL"
    fold<-schedule "America/Havana" "2018-11-04 03:30:00 UTC"
    DB.valueField "claimed" fold `shouldBe` Just True
    DB.valueField "next_eligibility" fold `shouldBe` Just (instant "2018-11-04 04:00:00 UTC")
  it "resolves gap/fold boundaries, skips collapsed windows and never catches up outside them" $ \env -> withRuntime env $ \cfg _ -> do
    let resolve start end now=DB.withAuditConnection cfg $ \conn -> do
          DB.tryAuditLock conn `shouldReturn` True
          DB.auditSchedule conn (cfg.auditMaintenance {C.windowStart=start,C.windowEnd=end,C.timezone="Europe/Copenhagen"}) (Just (instant now)) False
    gap<-resolve "02:30" "04:00" "2026-03-29 01:15:00 UTC"
    DB.valueField "window_start" gap `shouldBe` Just (instant "2026-03-29 01:00:00 UTC")
    fold<-resolve "02:30" "04:00" "2026-10-25 00:45:00 UTC"
    DB.valueField "window_start" fold `shouldBe` Just (instant "2026-10-25 00:30:00 UTC")
    collapsed<-resolve "02:10" "02:50" "2026-03-29 01:15:00 UTC"
    DB.valueField "reason" collapsed `shouldBe` Just ("collapsed_window"::Text)
    missed<-resolve "03:00" "04:00" "2026-11-01 03:01:00 UTC"
    DB.valueField "eligible" missed `shouldBe` Just False
  it "defers an unproven role-overridden local timezone while an explicit zone stays deterministic" $ \env -> withRuntime env $ \cfg role -> do
    execute env ("ALTER ROLE "<>role<>" SET TimeZone='UTC'")
    DB.withAuditConnection cfg $ \conn -> do
      DB.tryAuditLock conn `shouldReturn` True
      local<-DB.auditSchedule conn (cfg.auditMaintenance {C.timezone="local"}) (Just (instant "2026-11-01 02:30:00 UTC")) False
      DB.valueField "reason" local `shouldBe` Just ("unproven_server_local_timezone"::Text)
      explicit<-DB.auditSchedule conn cfg.auditMaintenance (Just (instant "2026-11-01 02:30:00 UTC")) False
      DB.valueField "window_start" explicit `shouldBe` Just (instant "2026-11-01 02:00:00 UTC")
  it "rejects missing MAINTAIN, direct broad membership and nested assumable privileged roles" $ \env -> withRuntime env $ \cfg role -> do
    execute env ("REVOKE MAINTAIN ON public.audit_log FROM "<>role)
    DB.withAuditConnection cfg (fmap (DB.valueField "runtime_safe") . DB.auditStatus) `shouldReturn` Just False
    execute env ("GRANT MAINTAIN ON public.audit_log TO "<>role<>"; GRANT pg_maintain TO "<>role)
    DB.withAuditConnection cfg (fmap (DB.valueField "runtime_safe") . DB.auditStatus) `shouldReturn` Just False
    execute env ("REVOKE pg_maintain FROM "<>role)
    let holder=role<>"_priv"; middle=role<>"_middle"
    bracket (execute env ("CREATE ROLE "<>holder<>" NOLOGIN CREATEROLE; CREATE ROLE "<>middle<>" NOLOGIN NOINHERIT; GRANT "<>holder<>" TO "<>middle<>"; GRANT "<>middle<>" TO "<>role))
      (const (execute env ("REVOKE "<>middle<>" FROM "<>role<>"; DROP ROLE "<>middle<>"; DROP ROLE "<>holder))) $ \_ ->
        DB.withAuditConnection cfg (fmap (DB.valueField "runtime_safe") . DB.auditStatus) `shouldReturn` Just False
  it "compares actual app/maintenance identities when the ordinary libpq user is omitted" $ \env -> withRuntime env $ \cfg _ -> do
    admin<-textValue env "SELECT session_user::text"
    let same=cfg {C.database=cfg.database {C.user=Nothing},C.auditMaintenance=cfg.auditMaintenance {C.maintenanceUser=Just admin}}
    result<-try @DB.AuditFailure (DB.withAuditConnection same (const (pure ())))
    result `shouldBe` Left (DB.AuditFailure "separate_maintenance_credentials_required")
  it "keeps ordinary target B active while a raw application override prevents any configured A assessment" $ \env -> withRuntime env $ \cfg _ -> do
    let targetB=cfg.database {C.name="postgres"}
        override=C.connectionString targetB
        settings=cfg {C.auditMaintenance=cfg.auditMaintenance {C.assessmentIntervalSeconds=1}}
    bracket (Pool.createPool override 1 30 30000) destroyAllResources $ \ordinary -> do
      Pool.runSession ordinary (S.statement () (St.Statement "SELECT current_database()::text" E.noParams (D.singleRow (D.column (D.nonNullable D.text))) False)) `shouldReturn` "postgres"
      cfg.database.name `shouldNotBe` "postgres"
      bracket (startAuditWorkerForApplication settings (Just override)) stopAuditWorker $ \worker -> do
        let awaitRefusal=do
              value<-readAuditWorkerStatus worker
              if DB.valueField "reason" value==Just ("application_target_override_unresolved"::Text)
                then pure () else threadDelay 10000>>awaitRefusal
        timeout 2000000 awaitRefusal `shouldReturn` Just ()
        number env "SELECT count(*) FROM public.audit_maintenance_control WHERE last_scheduled_date IS NOT NULL OR last_assessment IS NOT NULL OR last_cycle IS NOT NULL" `shouldReturn` 0
        number env "SELECT count(*) FROM pg_stat_activity WHERE datname=current_database() AND application_name='hmem-audit-maintenance'" `shouldReturn` 0
        Pool.runSession ordinary (S.statement () (St.Statement "SELECT 1::bigint" E.noParams (D.singleRow (D.column (D.nonNullable D.int8))) False)) `shouldReturn` 1
  it "keeps one operation lock through the assessment and obeys durable pause/restart" $ \env -> withRuntime env $ \cfg _ -> do
    DB.withAuditConnection cfg $ \conn -> do
      DB.tryAuditLock conn `shouldReturn` True
      busy<-runAuditCycle cfg ManualAudit
      DB.valueField "reason" busy `shouldBe` Just ("maintenance_busy"::Text)
    DB.withAuditConnection cfg (\conn -> void (DB.setAuditPaused conn True))
    paused<-runAuditCycle cfg ManualAudit
    DB.valueField "reason" paused `shouldBe` Just ("paused"::Text)
    DB.withAuditConnection cfg (fmap (DB.valueField "paused") . DB.auditStatus) `shouldReturn` Just True
    DB.withAuditConnection cfg (\conn -> void (DB.setAuditPaused conn False))
  it "refreshes mutable controls after acquiring the lock at an owner handoff" $ \env -> withRuntime env $ \cfg _ -> do
    execute env "CREATE EXTENSION IF NOT EXISTS pgstattuple"
    ready<-newEmptyMVar; allowLock<-newEmptyMVar; startSecond<-newEmptyMVar
    withAsync (takeMVar startSecond >> runAuditCycleAtWith (putMVar ready ()>>takeMVar allowLock) cfg ManualAudit
      (Just (instant "2026-11-01 02:30:00 UTC"))) $ \second -> do
      DB.withAuditConnection cfg $ \first -> do
        DB.tryAuditLock first `shouldReturn` True
        putMVar startSecond ()
        timeout 5000000 (takeMVar ready) `shouldReturn` Just ()
        execute env "UPDATE public.audit_maintenance_control SET last_heap_at='2026-11-01 02:30:00+00'"
      putMVar allowLock ()
      outcome<-wait second
      DB.valueField "reason" outcome `shouldBe` Just ("cooldown"::Text)
  it "measures physical headroom with the real restricted native connection and preserves audit contents" $ \env -> withRuntime env $ \cfg _ -> do
    execute env "CREATE EXTENSION IF NOT EXISTS pgstattuple"
    before<-number env "SELECT count(*) FROM audit_log"
    outcome<-runAuditCycleAt cfg ManualAudit (Just (instant "2026-11-01 02:30:00 UTC"))
    DB.valueField "supported_host" outcome `shouldBe` Just ("native_windows_pg18_same_sid_and_logon"::Text)
    DB.valueField "executed" outcome `shouldBe` Just False
    DB.valueField "reason" outcome `shouldBe` Just ("below_reclaim_threshold"::Text)
    number env "SELECT count(*) FROM audit_log" `shouldReturn` before
    let unsafe=cfg {C.auditMaintenance=cfg.auditMaintenance {C.minimumFreeBytes=1125899906842624}}
    deferred<-runAuditCycleAt unsafe ManualAudit (Just (instant "2026-11-01 02:30:00 UTC"))
    DB.valueField "reason" deferred `shouldBe` Just ("insufficient_physical_headroom"::Text)
  it "retains known empty/deleted index pages when only the zero-leaf density is undefined" $ \env -> withRuntime env $ \cfg _ -> do
    execute env "CREATE EXTENSION IF NOT EXISTS pgstattuple; INSERT INTO workspaces(id,name,workspace_type) SELECT gen_random_uuid(),'density-fixture-'||n::text,'repository'::workspace_type_enum FROM generate_series(1,500) n"
    withIndexDiagnostic env "LANGUAGE sql AS $$ SELECT 4,0,49152::bigint,0::bigint,0::bigint,0::bigint,2::bigint,3::bigint,'NaN'::float8,0::float8 $$" $
      DB.withAuditConnection cfg $ \conn -> do
        DB.tryAuditLock conn `shouldReturn` True
        assessed<-DB.auditAssessment conn
        DB.valueField "reason" assessed `shouldBe` Just ("ok"::Text)
        let indexes=maybe [] id (DB.valueField "indexes" assessed :: Maybe [Value])
        any (\index->maybe False (>0) (DB.valueField "reclaimable" index :: Maybe Integer)) indexes `shouldBe` True
        mapM_ (\index->case (DB.valueField "reclaimable" index :: Maybe Integer,DB.valueField "allocated" index :: Maybe Integer) of
          (Just reclaim,Just allocated) -> reclaim `shouldSatisfy` (\n->n>=0&&n<=allocated)
          _ -> pure ()) indexes
  it "defers unknown nonempty-leaf density rather than treating it as known zero" $ \env -> withRuntime env $ \cfg _ -> do
    execute env "CREATE EXTENSION IF NOT EXISTS pgstattuple"
    withIndexDiagnostic env "LANGUAGE sql AS $$ SELECT 4,0,16384::bigint,0::bigint,0::bigint,1::bigint,0::bigint,0::bigint,'NaN'::float8,0::float8 $$" $
      DB.withAuditConnection cfg $ \conn -> do
        DB.tryAuditLock conn `shouldReturn` True
        assessed<-DB.auditAssessment conn
        DB.valueField "reason" assessed `shouldBe` Just ("diagnostic_invalid_index_density"::Text)
  it "persists a post-claim diagnostic failure and exposes it through the actual CLI status path" $ \env -> withRuntime env $ \cfg _ -> do
    execute env "CREATE EXTENSION IF NOT EXISTS pgstattuple"
    withIndexDiagnostic env "LANGUAGE plpgsql AS $$ BEGIN RAISE EXCEPTION USING ERRCODE='XX000',MESSAGE='fixture_diagnostic_failure'; END $$" $ do
      failed<-runAuditCycleAt cfg ScheduledAudit (Just (instant "2026-11-01 02:30:00 UTC"))
      DB.valueField "reason" failed `shouldBe` Just ("maintenance_sqlstate_XX000"::Text)
      DB.valueField "failure_persistence" failed `shouldBe` Just ("recorded"::Text)
      status<-Ctl.runAuditControlWithConfig cfg Ctl.AuditStatus
      let cycleReceipt=DB.valueField "result" status >>= DB.valueField "last_cycle" :: Maybe Value
      (cycleReceipt >>= DB.valueField "reason") `shouldBe` Just ("maintenance_sqlstate_XX000"::Text)
      (code,_,_)<-Cli.runAuditMaintenanceCommandWith (Ctl.runAuditControlWithConfig cfg) (Cli.AuditMaintenanceCommand Ctl.AuditRunOnce True)
      code `shouldBe` ExitFailure 1
  it "releases a published native acquisition when cancellation arrives before use" $ \env -> withRuntime env $ \cfg _ ->
    if os/="mingw32" then pendingWith "native Windows proof" else DB.withAuditConnection cfg $ \conn -> do
      DB.tryAuditLock conn `shouldReturn` True
      identityValue<-DB.auditIdentity conn
      proof<-either (fail . T.unpack) pure (DB.parseValue identityValue)
      let directory=env.testSandbox.sandboxTmpDir</>"audit-publication-owner"
          moved=directory<>"-moved"
          scoped=proof {Host.relations=[Host.AuditRelation 0 "test" "publication" 0 (T.pack directory) True]}
      createDirectory directory
      ready<-newEmptyMVar; hold<-newEmptyMVar
      withAsync (Host.withAuditHostWith (putMVar ready ()>>takeMVar hold) scoped (const (pure ()))) $ \child -> do
        timeout 5000000 (takeMVar ready) `shouldReturn` Just ()
        attempted<-try @IOException (renameDirectory directory moved)
        attempted `shouldSatisfy` isLeft
        cancel child
        void (waitCatch child)
      renameDirectory directory moved
      removeDirectory moved
  it "does not obstruct a real CHECKPOINT while repeatedly reading the immutable control prefix" $ \env -> withRuntime env $ \cfg _ -> do
    DB.withAuditConnection cfg $ \conn -> do
      DB.tryAuditLock conn `shouldReturn` True
      identityValue<-DB.auditIdentity conn
      proof<-either (fail . T.unpack) pure (DB.parseValue identityValue)
      result<-Host.withAuditHost proof $ \host -> withAsync (replicateM_ 20 (do
          sample<-Host.sampleAuditHost host
          case sample of Left reason -> fail (T.unpack reason); Right _ -> pure ()
          threadDelay 1000)) $ \samples -> do
        withConn env.pool $ \admin -> S.run (S.sql "CHECKPOINT") admin >>= either (fail . show) pure
        wait samples
      result `shouldSatisfy` (not . isLeft)
  it "runs and cleanly drains the integrated supervised worker on the restricted native connection" $ \env -> withRuntime env $ \cfg _ -> do
    execute env "CREATE EXTENSION IF NOT EXISTS pgstattuple"
    bracket (startAuditWorker (cfg {C.auditMaintenance=cfg.auditMaintenance {C.assessmentIntervalSeconds=1}})) stopAuditWorker $ \worker -> do
      let awaitProof=do
            outcome<-readAuditWorkerStatus worker
            if DB.valueField "supported_host" outcome==Just ("native_windows_pg18_same_sid_and_logon"::Text)
              then pure () else threadDelay 10000>>awaitProof
      timeout 10000000 awaitProof `shouldReturn` Just ()
  it "groups transient copy/WAL requirements once per actual volume and fails unknown pressure closed" $ \_ -> do
    let cfg=C.defaultAuditMaintenanceConfig {C.minimumFreeBytes=1048576}
        relation=Host.AuditRelation 1 "heap" "audit" 100 "unused" True
        capacity=Host.HostCapacity [Host.VolumeCapacity "volume" 1049076 1049076] [0,0,0]
    fmap length (capacityRequirements cfg [relation] capacity) `shouldBe` Right 1
    capacityRequirements cfg [relation] (Host.HostCapacity [Host.VolumeCapacity "volume" 1049075 1049075] [0,0,0]) `shouldBe` Left "insufficient_physical_headroom"
    pressureReason cfg (object []) `shouldBe` Just "wal_pressure_unknown"
  it "retains the actual worker on failed stop and joins it on retry" $ \_ -> do
    ready<-newEmptyMVar; release<-newEmptyMVar
    bracket (startAuditWorkerWith C.defaultConfig (putMVar ready ()>>takeMVar release>>pure (DB.reasonValue "test_done")))
      (\worker -> void (tryPutMVar release ()) >> stopAuditWorker worker) $ \worker -> do
        timeout 5000000 (takeMVar ready) `shouldReturn` Just ()
        stopped<-try @AuditDrainFailure (stopAuditWorker worker)
        stopped `shouldSatisfy` isLeft
        putMVar release ()
        stopAuditWorker worker

withRuntime :: TestEnv -> (C.HMemConfig -> ByteString -> IO a) -> IO a
withRuntime env action=do
  suffix<-T.filter (/='-') . T.pack . show <$> UUID.nextRandom
  admin<-textValue env "SELECT session_user::text"
  let role="audit_runtime_"<>TE.encodeUtf8 suffix
      settings=C.defaultAuditMaintenanceConfig {C.maintenanceUser=Just (TE.decodeUtf8 role),C.timezone="Europe/Copenhagen",C.minimumFreeBytes=1048576}
      cfg=C.defaultConfig {C.database=C.defaultConfig.database {C.host="127.0.0.1",C.port=env.testDb.testDbPort,C.name=env.testDb.testDbName,C.user=Just admin,C.password=Nothing,C.sslmode=Just "disable"},C.auditMaintenance=settings}
      setup=execute env ("CREATE ROLE "<>role<>" LOGIN NOINHERIT NOSUPERUSER NOCREATEDB NOCREATEROLE NOREPLICATION NOBYPASSRLS; GRANT USAGE ON SCHEMA public TO "<>role<>"; GRANT MAINTAIN ON public.audit_log TO "<>role<>"; GRANT EXECUTE ON FUNCTION public.hmem_audit_runtime_safe(),public.hmem_audit_status(),public.hmem_audit_pause(boolean),public.hmem_audit_schedule(timestamptz,text,text,text,int,boolean),public.hmem_audit_identity(),public.hmem_audit_assess(),public.hmem_audit_record(jsonb,boolean) TO "<>role<>"; UPDATE public.audit_maintenance_control SET paused=FALSE,last_scheduled_date=NULL,last_scheduled_zone=NULL,last_claimed_at=NULL,last_heap_at=NULL,last_index_at=NULL,last_assessment=NULL,last_cycle=NULL,backup_receipt=NULL")
  bracket setup (const (execute env ("DROP OWNED BY "<>role<>"; DROP ROLE "<>role))) $ \_ -> action cfg role

execute :: TestEnv -> ByteString -> IO ()
execute env sql=runSession env.pool (S.sql sql)
withIndexDiagnostic :: TestEnv -> ByteString -> IO a -> IO a
withIndexDiagnostic env body action=do
  original<-textValue env "SELECT pg_get_functiondef('public.pgstatindex(regclass)'::regprocedure)"
  let signature="CREATE OR REPLACE FUNCTION public.pgstatindex(IN relname regclass, OUT version int, OUT tree_level int, OUT index_size bigint, OUT root_block_no bigint, OUT internal_pages bigint, OUT leaf_pages bigint, OUT empty_pages bigint, OUT deleted_pages bigint, OUT avg_leaf_density float8, OUT leaf_fragmentation float8) "
  bracket_ (execute env (signature<>body)) (execute env (TE.encodeUtf8 original)) action
number :: TestEnv -> ByteString -> IO Int
number env sql=fromIntegral <$>runSession env.pool (S.statement () (St.Statement sql E.noParams (D.singleRow (D.column (D.nonNullable D.int8))) False))
textValue :: TestEnv -> ByteString -> IO Text
textValue env sql=runSession env.pool (S.statement () (St.Statement sql E.noParams (D.singleRow (D.column (D.nonNullable D.text))) False))
instant :: String -> UTCTime
instant=read
