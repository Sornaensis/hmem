{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import Control.Concurrent (threadDelay, rtsSupportsBoundThreads)
import Control.Exception
import Control.Monad
import Data.IORef
import Data.List (isPrefixOf)
import Data.Char (toUpper)
import Control.Concurrent.Async (async,waitCatch)
import Crypto.Hash (hash,Digest,SHA256)
import Data.Aeson
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Int
import Data.Bits ((.&.))
import Data.Pool (Pool,destroyAllResources)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time
import Data.UUID qualified as UUID
import Data.Word
import Foreign hiding (void,Pool)
import Foreign.C.String (withCWString,CWString)
import Hasql.Connection qualified as H
import Hasql.Session qualified as S
import Hasql.Decoders qualified as D
import Hasql.Encoders qualified as E
import Hasql.Statement qualified as St
import HMem.DB.Pool qualified as Pool
import HMem.DB.Migration qualified as Migration
import HMem.DB.AuditMaintenance qualified as Audit
import HMem.DB.ChangeStream qualified as Stream
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Types.Status (statusCode)
import Network.WebSockets qualified as WS
import Network.Socket qualified as Socket
import Foreign.Ptr
import Foreign.Marshal.Array (allocaArray)
import GHC.Clock
import HMem.Config qualified as Config
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.Info (os)
import System.Process
import System.Timeout (timeout)

-- This runner owns only children it creates. It never uses ctl stop, image
-- matching, PID rediscovery, or a production configuration/database.
main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  getArgs >>= \args -> case args of
    ["--config-probe",expected] -> do
      ambient<-getEnvironment
      let private=[(nativeKey key,value)|(key,value)<-ambient,"HMEM" `isPrefixOf` nativeKey key]
          pgKeys=[key|(key,_)<-ambient,"PG" `isPrefixOf` nativeKey key]
      unless(private==[("HMEM_CONFIG_DIR",takeDirectory expected)]&&null pgKeys)(fail "child_environment_isolation_not_proven_before_config_IO")
      actual <- Config.configFilePath
      putStrLn ("observed_config_path=" <> actual <> " expected=" <> expected)
      unless (normalise actual == normalise expected) (fail "private_config_path_mismatch")
      putStrLn ("private_config_path=" <> actual)
    ["--signal-child",root] -> do
      (writeFile (root </> "signal-ready") "ready" >> forever (threadDelay 100000))
        `catch` \UserInterrupt -> writeFile (root </> "signal-joined") "UserInterrupt"
    ["--effective-config",expectedRoot,dbPort,httpPort] -> do
      actualRoot<-Config.configDir
      unless(normalise actualRoot==normalise expectedRoot)(fail "effective_private_root_mismatch")
      cfg<-Config.loadConfig
      let expected=Config.defaultAuditMaintenanceConfig {Config.maintenanceUser=Just "qualification_maint",Config.heapCompactionEnabled=cfg.auditMaintenance.heapCompactionEnabled}
      unless(cfg.database.host=="127.0.0.1"&&cfg.database.port==read dbPort&&cfg.database.name=="postgres"&&cfg.database.user==Just "qualification_admin"&&cfg.database.sslmode==Just "disable"&&cfg.server.host=="127.0.0.1"&&cfg.server.port==read httpPort)(fail "effective_fixture_target_mismatch")
      unless(cfg.embeddingProvider.mode==Config.EmbeddingProviderDisabled&&not cfg.web.webEnabled&&cfg.auth.local.userToken==Nothing&&cfg.auditMaintenance==expected&&cfg.changeStream==Config.defaultConfig.changeStream)(fail "effective_worker_or_default_policy_mismatch")
      emit "effective_private_config" (object["root".=actualRoot,"database_host".=cfg.database.host,"database_port".=cfg.database.port,"database".=cfg.database.name,"http_port".=cfg.server.port,"embedding_disabled".=True,"web_enabled".=False,"local_token_absent".=True,"separate_maintenance_identity".=True,"heap_enabled".=cfg.auditMaintenance.heapCompactionEnabled,"default_schedule".=("03:00-04:00/local/daily"::Text)])
    ["--preflight",root] -> preflight root
    ["--signal-preflight",root] -> signalPreflight root
    ["--qualify",root] -> preflight root >> qualify root
    _ -> fail "usage: AuditSpace --preflight ABSOLUTE_OWNED_ROOT"

preflight :: FilePath -> IO ()
preflight root = do
  unless rtsSupportsBoundThreads (fail "threaded_runtime_required")
  canonical <- canonicalizePath root
  unless (normalise canonical == normalise "D:\\Projects\\hmem\\.tmp\\audit-qualification-df58e815") (fail "qualification_root_mismatch")
  linked <- pathIsSymbolicLink root
  when linked (fail "qualification_root_reparse_refused")
  let home = root </> "home"
      config = home </> ".hmem" </> "config.yaml"
  createDirectoryIfMissing True (takeDirectory config)
  environment <- isolatedEnvironment home root
  ambient<-getEnvironment
  let injected=[("hMeM_CoNfIg_DiR","D:\\never-open-decoy-config"),("pGhOsT","203.0.113.1"),("hmem_db_password","never-use-decoy"),("HmEm_EmBeDdInG_EnDpOiNt","http://203.0.113.1/"),("hMeM_eMbEdDiNg_PrOvIdEr","http"),("hMeM_AuTh_LoCaL_UsEr_ToKeN","never-use-decoy-token")]<>ambient
      hostile=makeIsolatedEnvironment home root injected
  executable <- getExecutablePath
  ownedHelper 5000000 ((proc executable ["--config-probe",config]) {env=Just environment})
  ownedHelper 5000000 ((proc executable ["--config-probe",config]) {env=Just hostile})
  putStrLn "mixed_case_decoy_roots_targets_removed_before_config_IO=True"
  let effectiveHome=root </> "preflight-effective" </> "home"
      effectiveRoot=effectiveHome </> ".hmem"
      effectiveEnvironment=makeIsolatedEnvironment effectiveHome root injected<>[("HMEM_AUDIT_MAINTENANCE_DB_USER","qualification_maint")]
  createDirectoryIfMissing True effectiveRoot
  writeFile(effectiveRoot </> "config.yaml") "server:\n  host: 127.0.0.1\n  port: 49198\ndatabase:\n  host: 127.0.0.1\n  port: 49199\n  name: postgres\n  user: qualification_admin\n  sslmode: disable\nembedding:\n  mode: disabled\nweb:\n  enabled: false\n"
  ownedHelper 5000000 ((proc executable ["--effective-config",effectiveRoot,"49199","49198"]){env=Just effectiveEnvironment})
  selectedPort<-availableLoopbackPort
  putStrLn("os_selected_loopback_port_probe="<>show selectedPort<>" temporary_socket_closed=True")
  signalPreflight root

signalPreflight :: FilePath -> IO ()
signalPreflight root = do
  executable <- getExecutablePath
  environment <- isolatedEnvironment (root </> "home") root
  ensureConsole
  forM_ ["signal-ready","signal-joined"] $ \name -> do
    exists <- doesFileExist (root </> name)
    when exists (removeFile (root </> name))
  mask $ \restore -> do
    (_,_,_,child) <- createProcess ((proc executable ["--signal-child",root])
      {env=Just environment,create_group=True,std_in=NoStream,std_out=NoStream,std_err=NoStream})
    result <- try @SomeException $ restore $ do
      await 3000000 (doesFileExist (root </> "signal-ready")) "signal_child_readiness"
      identifier <- getPid child
      putStrLn ("owned_signal_group=" <> show identifier)
      interruptProcessGroupOf child
      await 3000000 (doesFileExist (root </> "signal-joined")) "signal_child_graceful_marker"
      joined <- waitForProcess child
      unless (joined == ExitSuccess) (fail ("signal_child_exit=" <> show joined))
      putStrLn "owned_signal_graceful_join=True"
    case result of
      Right () -> pure ()
      Left failure -> do
        state<-getProcessExitCode child
        putStrLn("signal_fixture_failure_child_state_before_retirement="<>show state)
        retireChild child
        throwIO failure
  putStrLn "preflight_pass=True"

isolatedEnvironment :: FilePath -> FilePath -> IO [(String,String)]
isolatedEnvironment home root = do
  ambient <- getEnvironment
  pure(makeIsolatedEnvironment home root ambient)

nativeKey :: String -> String
nativeKey key=if os=="mingw32" then map toUpper key else key

makeIsolatedEnvironment :: FilePath -> FilePath -> [(String,String)] -> [(String,String)]
makeIsolatedEnvironment home root ambient =
  let clear original = let key=nativeKey original in not ("PG" `isPrefixOf` key || "HMEM" `isPrefixOf` key || key `elem`
        ["HOME","USERPROFILE","HOMEDRIVE","HOMEPATH","APPDATA","LOCALAPPDATA","XDG_CONFIG_HOME","TEMP","TMP"])
  in filter (clear . fst) ambient <>
    [("HMEM_CONFIG_DIR",home </> ".hmem"),("HOME",home),("USERPROFILE",home),("HOMEDRIVE",takeDrive home),("HOMEPATH",dropDrive home)
    ,("APPDATA",home </> "AppData"),("LOCALAPPDATA",home </> "AppData"),("TEMP",root),("TMP",root)]

await :: Int -> IO Bool -> String -> IO ()
await microseconds condition label = do
  start <- getMonotonicTimeNSec
  let deadline = start + fromIntegral microseconds * 1000
      loop = do
        done <- condition
        unless done $ do
          now <- getMonotonicTimeNSec
          when (now >= deadline) (fail (label <> "_deadline"))
          threadDelay 10000
          loop
  loop

-- Publication is masked. Interrupted/failed helpers retain their original
-- ProcessHandle through termination and actual join; a failed signal cannot
-- skip the join. Pending cleanup retains this controller rather than unwinding.
ownedHelper :: Int -> CreateProcess -> IO ()
ownedHelper budget command = mask $ \restore -> do
  (_,_,_,child) <- createProcess command
  result <- try @SomeException (restore (boundedWait budget child))
  case result of
    Right (Just ExitSuccess) -> pure ()
    Right (Just status) -> fail ("owned_helper_exit=" <> show status)
    _ -> do
      retireChild child
      either throwIO (const (fail "owned_helper_deadline")) result

boundedWait :: Int -> ProcessHandle -> IO (Maybe ExitCode)
boundedWait budget child = do
  start <- getMonotonicTimeNSec
  let deadline = start + fromIntegral budget * 1000
      loop = getProcessExitCode child >>= \state -> case state of
        Just _ -> Just <$> waitForProcess child
        Nothing -> do
          now <- getMonotonicTimeNSec
          if now >= deadline then pure Nothing else threadDelay 10000 >> loop
  loop

retireChild :: ProcessHandle -> IO ()
retireChild child = mask_ $ do
  _ <- try @SomeException (terminateProcess child)
  let join = try @SomeException (boundedWait 1000000 child) >>= \state -> case state of
        Right (Just _) -> pure ()
        _ -> do
          _ <- try @SomeException (putStrLn "cleanup_pending retaining_actual_child")
          join
  join

#ifdef mingw32_HOST_OS
foreign import ccall unsafe "GetLastError" getLastError :: IO Word32
foreign import ccall unsafe "AllocConsole" allocConsole :: IO Int
foreign import ccall unsafe "GetConsoleProcessList" getConsoleProcessList :: Ptr Word32 -> Word32 -> IO Word32
foreign import ccall unsafe "GetConsoleWindow" getConsoleWindow :: IO (Ptr ())
foreign import ccall unsafe "ShowWindow" showWindow :: Ptr () -> Int -> IO Int
ensureConsole :: IO ()
ensureConsole = do
  count <- allocaArray 1 (\buffer -> getConsoleProcessList buffer 1)
  putStrLn ("owned_console_process_count=" <> show count)
  when (count == 0) $ do
    allocated <- allocConsole
    unless (allocated /= 0) (getLastError >>= \code -> fail ("owned_console_unavailable_error=" <> show code))
    window <- getConsoleWindow
    _ <- showWindow window 0
    pure ()
#else
ensureConsole :: IO ()
ensureConsole = fail "native_windows_qualification_only"
#endif

repo :: FilePath
repo = "D:\\Projects\\hmem"
pg :: String -> FilePath
pg name = "C:\\msys64\\mingw64\\bin" </> name <> ".exe"
serverExe,ctlExe :: FilePath
serverExe = repo </> ".stack-work\\install\\c66173c6\\bin\\hmem-server.exe"
ctlExe = repo </> ".stack-work\\install\\c66173c6\\bin\\hmem-ctl.exe"

data Fixture = Fixture { directory :: FilePath, settings :: Config.DatabaseConfig
  , pool :: Pool H.Connection, environment :: [(String,String)], home :: FilePath }

qualify :: FilePath -> IO ()
qualify root = forM_ ["heap","index"] $ \kind -> withFixture root kind $ \fixture -> do
  putStrLn ("qualification_begin kind="<>kind)
  migrations <- Migration.runMigrations fixture.pool (repo </> "hmem-server\\migrations")
  case migrations.failed of Just failure -> fail (show failure); Nothing -> pure ()
  sql fixture "CREATE EXTENSION pgstattuple; ALTER TABLE public.audit_log SET(autovacuum_enabled=false,toast.autovacuum_enabled=false); UPDATE public.audit_maintenance_control SET paused=true"
  provision fixture
  zone <- json fixture "SELECT to_jsonb(name) FROM pg_timezone_names WHERE (clock_timestamp() AT TIME ZONE name)::time >= time '03:00' AND (clock_timestamp() AT TIME ZONE name)::time < time '03:45' AND name NOT LIKE 'posix/%' ORDER BY extract(minute FROM clock_timestamp() AT TIME ZONE name),name LIMIT 1" >>= parsed
  appendFile (fixture.directory </> "postgresql.conf") ("\ntimezone='"<>T.unpack zone<>"'\n")
  sql fixture "SELECT pg_reload_conf()"
  await 2000000 (do value<-json fixture "SELECT to_jsonb(current_setting('TimeZone'))"; pure (value==String zone)) "server_timezone_reload"
  clock <- json fixture "SELECT jsonb_build_object('utc',clock_timestamp(),'local',clock_timestamp() AT TIME ZONE current_setting('TimeZone'),'zone',current_setting('TimeZone'),'source',(SELECT source FROM pg_settings WHERE name='TimeZone'))"
  emit "actual_server_clock" clock
  port <- availableLoopbackPort
  let suffix = if kind=="heap" then "" else "audit_maintenance:\n  heap_compaction_enabled: false\n"
  writeConfig fixture port suffix
  configBytes <- BS.readFile (fixture.home </> ".hmem\\config.yaml")
  unless (kind/="heap" || not ("audit_maintenance" `BS.isInfixOf` configBytes)) (fail "default_trial_contains_audit_block")
  manager <- HTTP.newManager HTTP.defaultManagerSettings
  -- Preparation uses the actual Main with durable pause already set. It cannot
  -- consume the eligible date; no application --db override or --dev is used.
  (workspace,taskPath,revertPath,history) <- withServer fixture port $ \_ -> do
    workspace <- request manager port "POST" "/api/v1/workspaces" (object ["name".= ("qualification"::Text)]) >>= field "id"
    task <- request manager port "POST" "/api/v1/tasks" (object ["workspace_id".=(workspace::Text),"title".= ("before"::Text),"description".= description "before"])
    taskId <- field "id" task
    let taskPath="/api/v1/tasks/"<>taskId
    _ <- request manager port "PUT" taskPath (object ["title".= ("after"::Text),"description".= description "after"])
    auditId <- json fixture ("SELECT to_jsonb(id::text) FROM public.audit_log WHERE entity_type='task' AND entity_id='"<>taskId<>"' AND action='update' ORDER BY changed_at DESC LIMIT 1") >>= parsed
    history <- auditRows fixture
    assertTask manager port taskPath "after" (description "after")
    pure (workspace,taskPath,"/api/v1/audit/"<>auditId<>"/revert",history)
  seedBloat fixture
  -- A real token is minted before the later stale outbox prefix. Its opaque
  -- bearer belongs to the same synthetic local audience used by actual Main.
  workspaceId <- maybe (fail "workspace UUID") pure (UUID.fromText workspace)
  begin <- Stream.beginResync fixture.pool 300 (Stream.WorkspaceScope workspaceId) (Stream.TrustedAudience "local:local-user") (pure []) >>= either (fail . show) pure
  page <- Stream.readSnapshotPageWithTtls fixture.pool 300 82800 (Stream.WorkspaceScope workspaceId) (Stream.TrustedAudience "local:local-user") begin.snapshotToken 100 >>= either (fail . show) pure
  oldToken <- maybe (fail "terminal token missing") pure page.snapshotResumeToken
  seedOutbox fixture workspace
  validBeforePrune<-Stream.replayUnacknowledgedResumeToken fixture.pool (Stream.WorkspaceScope workspaceId) (Stream.TrustedAudience "local:local-user") oldToken 100
  case validBeforePrune of Right _->pure();Left failure->fail("old_bearer_invalid_before_prune="<>show failure)
  oldWatermark<-json fixture "SELECT to_jsonb(watermark) FROM change_stream_resume_tokens ORDER BY created_at DESC LIMIT 1" >>=parsed :: IO Integer
  tokenTtl <- json fixture "SELECT to_jsonb(round(extract(epoch FROM expires_at-created_at))::bigint) FROM change_stream_resume_tokens ORDER BY created_at DESC LIMIT 1"
  unless (tokenTtl==Number 82800) (fail "default_resume_ttl_not_23h")
  verifyBackup fixture history
  baseline <- auditRows fixture
  before <- catalog fixture
  emit "before_relations" (toJSON before)
  immutable <- auditRows fixture
  unless (immutable==baseline) (fail "seed_or_backup_changed_audit_history")
  sql fixture "UPDATE public.audit_maintenance_control SET paused=false"
  (outcome,peak) <- withSamples fixture $ \checkSamples -> withServerChecked checkSamples fixture port $ \process -> do
    await 30000000 (do checkSamples;result<-json fixture "SELECT to_jsonb(last_cycle IS NOT NULL AND operation->>'state' IN ('succeeded','reconciled')) FROM public.audit_maintenance_control"; checkSamples;state<-getProcessExitCode process; when (state/=Nothing)(fail "actual_server_exited_before_maintenance"); pure (result==Bool True)) "automatic_physical_completion"
    result <- json fixture "SELECT jsonb_build_object('date',last_scheduled_date,'zone',last_scheduled_zone,'cycle',last_cycle,'operation',operation,'assessment',last_assessment) FROM public.audit_maintenance_control"
    emit "actual_main_automatic_outcome" result
    await 15000000 (do checkSamples;value<-json fixture ("SELECT to_jsonb(count(*)=0) FROM change_stream_outbox WHERE workspace_id='"<>workspace<>"' AND occurred_at < clock_timestamp()-interval '1 day'"); checkSamples;pure(value==Bool True)) "one_day_automatic_pruning"
    remaining <- json fixture ("SELECT jsonb_build_object('rows',count(*),'age_hours',round(extract(epoch FROM clock_timestamp()-min(occurred_at))/3600)) FROM change_stream_outbox WHERE workspace_id='"<>workspace<>"'")
    emit "retained_replay" remaining
    crossed<-json fixture("SELECT to_jsonb(retained_from_cursor>"<>T.pack(show(oldWatermark+1))<>") FROM change_stream_scope_counters WHERE scope='workspace' AND workspace_id='"<>workspace<>"'")
    unless(crossed==Bool True)(fail "retention_floor_did_not_cross_old_watermark")
    staleReplay<-Stream.replayUnacknowledgedResumeToken fixture.pool (Stream.WorkspaceScope workspaceId) (Stream.TrustedAudience "local:local-user") oldToken 100
    unless(staleReplay==Left Stream.ReplayRetentionPruned)(fail "stale_replay_not_rejected_specifically_by_retention")
    staleTicket<-request manager port "POST" "/api/v1/change-stream/ticket" (object ["scope".=object["scope".=("workspace"::Text),"workspace_id".=workspace],"resume_token".=oldToken.unResumeToken])
    ticket<-field "ticket" staleTicket
    terminal<-timeout 2000000 $ WS.runClient "127.0.0.1" port ("/api/v1/ws?ticket="<>T.unpack ticket) $ \connection->do
      frame<-WS.receiveData connection::IO BS.ByteString
      value<-either (fail . ("stale_WS_frame_decode="<>)) pure(eitherDecodeStrict frame)
      unless((Audit.valueField "type" value::Maybe Text)==Just "resync_required")(fail "actual_stale_WS_missing_resync_required")
    unless(terminal==Just())(fail "actual_stale_WS_retention_response_deadline")
    putStrLn "old_bearer_preprune_valid_floor_crossed_typed_retention_error_actual_WS_resync_required=True"
    snapshot <- request manager port "POST" "/api/v1/change-stream/resync" (object ["scope".=object["scope".=("workspace"::Text),"workspace_id".=workspace],"page_size".=(100::Int),"start_idempotency_key".=("qualification-snapshot-recovery-00001"::Text)])
    recovered <- collectSnapshot checkSamples manager port workspace snapshot
    let recoveredTasks=[item | item<-recovered, Audit.valueField "kind" item==Just ("task"::Text)]
    unless(any (\item -> (Audit.valueField "data" item >>= Audit.valueField "description")==Just(description "after")) recoveredTasks)(fail "canonical_snapshot_missing_complete_current_task")
    emit "snapshot_recovery" (object["items".=length recovered,"current_task_complete".=True])
    actualResumeTtl<-json fixture "SELECT to_jsonb(round(extract(epoch FROM expires_at-created_at))::bigint) FROM change_stream_resume_tokens ORDER BY created_at DESC LIMIT 1"
    unless(actualResumeTtl==Number 82800)(fail "actual_HTTP_snapshot_resume_budget_not_23h")
    forM_ [1..5::Int] $ \_ -> checkSamples>>assertTask manager port taskPath "after" (description "after")>>checkSamples
    preserved <- auditRows fixture
    unless (preserved==baseline) (fail "automatic_maintenance_changed_history")
    pure result
  emit "sampled_peak" peak
  after <- catalog fixture
  emit "after_relations" (toJSON after)
  unless (sum (map snd after) < sum (map snd before)) (fail "native_relation_allocation_did_not_reduce")
  let expected = if kind=="heap" then "heap" else "index"
  operation <- field "operation" outcome :: IO Value
  actualKind <- field "kind" operation
  unless ((actualKind::Text)==expected) (fail "wrong_automatic_physical_kind")
  -- Restart actual Main on the same database and real local date. Neither a
  -- new physical UUID nor a replacement date may be admitted.
  withServer fixture port $ \_ -> do
    threadDelay 300000
    repeated <- json fixture "SELECT jsonb_build_object('date',last_scheduled_date,'zone',last_scheduled_zone,'cycle',last_cycle,'operation',operation,'assessment',last_assessment) FROM public.audit_maintenance_control"
    unless ((Audit.valueField "date" repeated::Maybe Text)==Audit.valueField "date" outcome && (Audit.valueField "operation" repeated >>= Audit.valueField "id" ::Maybe Text)==(Audit.valueField "operation" outcome >>= Audit.valueField "id")) (fail "same_date_restart_repeated_claim_or_operation")
    _ <- request manager port "POST" revertPath Null
    assertTask manager port taskPath "before" (description "before")
  putStrLn ("qualification_trial_pass="<>kind)

availableLoopbackPort :: IO Int
availableLoopbackPort=Socket.withSocketsDo $ bracket
  (Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol)
  Socket.close $ \socket->do
    let address=Socket.tupleToHostAddress(127,0,0,1)
    Socket.bind socket(Socket.SockAddrInet 0 address)
    endpoint<-Socket.getSocketName socket
    case endpoint of
      Socket.SockAddrInet port host | port/=0&&host==address ->pure(fromIntegral port)
      _->fail "OS_loopback_port_assignment_not_proven"

withFixture :: FilePath -> String -> (Fixture -> IO a) -> IO a
withFixture root kind use = mask $ \restore -> do
  free<-effectiveFree root
  unless(free>=3221225472)(fail "qualification_requires_3GiB_effective_free_before_initialization")
  putStrLn("pre_initialization_effective_free="<>show free)
  let owned=root </> kind
      directory=owned </> "data"
      home=owned </> "home"
      logPath=owned </> "postgres.log"
  exists <- doesPathExist owned
  when exists (fail "fixture_subtree_already_exists_requiring_owned_disposition")
  createDirectoryIfMissing True owned
  environment <- isolatedEnvironment home owned
  let helper name arguments=ownedHelper 30000000 ((proc(pg name) arguments) {env=Just environment,std_in=NoStream,std_out=NoStream,std_err=NoStream})
  helper "initdb" ["-D",directory,"--username","qualification_admin","--auth=trust","--locale=C","--encoding=UTF8"]
  port <- availableLoopbackPort
  appendFile (directory </> "postgresql.conf") ("\nlisten_addresses='127.0.0.1'\nport="<>show port<>"\nmax_wal_size='64MB'\nmin_wal_size='32MB'\nlogging_collector=off\ntimezone='UTC'\n")
  let settings=Config.defaultConfig.database {Config.host="127.0.0.1",Config.port=port,Config.name="postgres",Config.user=Just "qualification_admin",Config.password=Nothing,Config.sslmode=Just "disable"}
      conn=Config.connectionString settings<>" connect_timeout=1"
  complete <- try @SomeException $ withPreservingLog logPath $ \logHandle -> do
    -- The parent log handle stays inside the same lifetime as the process.
    (_,_,_,process)<-createProcess ((proc(pg "postgres")["-D",directory]) {env=Just environment,std_in=NoStream,std_out=UseHandle logHandle,std_err=UseHandle logHandle,create_group=True})
    stopIssued<-newIORef False
    wasReady<-newIORef False
    result <- try @SomeException $ restore $ do
      identifier<-getPid process
      putStrLn("owned_postmaster_created pid="<>show identifier<>" port="<>show port<>" data="<>directory)
      bracket (Pool.createPool conn 4 30 2000) destroyAllResources $ \pool -> do
        await 10000000 (do
          state<-getProcessExitCode process
          when(state/=Nothing)(fail("owned_postmaster_setup_exit="<>show state))
          owns<-ownsListener process port
          if not owns then pure False else do
            answer<-try @SomeException(Pool.runSession pool(S.statement () (St.Statement "SELECT current_setting('data_directory')::text" E.noParams(D.singleRow(D.column(D.nonNullable D.text))) False)))
            case answer of Right value->do unless(normalise(T.unpack value)==normalise directory)(fail "server_path_mismatch"); ownsListener process port
                           Left _->pure False) "owned_postmaster_readiness"
        writeIORef wasReady True
        use (Fixture directory settings pool environment home)
    (status,cleanupCause)<-retainedCleanup "postmaster" $ do
      exited<-getProcessExitCode process
      case exited of
        Just status -> do
          exists<-doesFileExist(directory </> "postmaster.pid")
          when exists(fail "joined_postmaster_pid_file_remains")
          _<-waitForProcess process
          pure status
        Nothing -> do
          issued<-readIORef stopIssued
          unless issued $ do
            pid<-getPid process
            contents<-T.lines . TE.decodeUtf8 <$>BS.readFile(directory </> "postmaster.pid")
            let bound=case contents of p:path:_:actualPort:_->T.strip p==maybe "" (T.pack . show) pid && normalise(T.unpack(T.strip path))==normalise directory && T.strip actualPort==T.pack(show port);_->False
            unless bound(fail "postmaster_pid_root_port_binding_mismatch")
            writeIORef stopIssued True
            ownedHelper 7000000 ((proc(pg "pg_ctl")["stop","-D",directory,"-m","fast","-w","-t","5"]){env=Just environment,std_in=NoStream,std_out=NoStream,std_err=NoStream})
          status<-boundedWait 1000000 process >>=maybe(fail "postmaster_exit_pending") pure
          exists<-doesFileExist(directory </> "postmaster.pid")
          when exists(fail "joined_postmaster_pid_file_remains")
          pure status
    validation<-try @SomeException $ do
      putStrLn("owned_postmaster_join="<>show status<>" pid_absent=True")
      ready<-readIORef wasReady
      when(ready&&status/=ExitSuccess)(fail "postmaster_joined_unclean_exit")
    finishOwned result(firstCause cleanupCause(either Just(const Nothing) validation))
  case complete of
    Left _->forM_ [("postmaster",logPath),("main_stderr",owned </> "server.log"),("main_app",home </> ".hmem\\logs\\hmem-server.log")] $ \(label,path)->void(try @SomeException $ do
      exists<-doesFileExist path
      when exists $ do
        diagnostic<-withBinaryFile path ReadMode $ \handle->do
          size<-hFileSize handle
          hSeek handle AbsoluteSeek(max 0(size-4096))
          BS.hGet handle 4096
        emit ("failed_owned_"<>label<>"_tail") (String(either(const "non_utf8_fixture_diagnostic") id(TE.decodeUtf8' diagnostic))))
    Right _->pure()
  removed<-try @SomeException(safeRemoveTree owned)
  finishOwned complete (either Just (const Nothing) removed)

sql :: Fixture -> Text -> IO ()
sql fixture statement = Pool.withConn fixture.pool $ \connection -> S.run (S.sql (TE.encodeUtf8 statement)) connection >>= either (fail . show) pure
json :: Fixture -> Text -> IO Value
json fixture statement = Pool.runSession fixture.pool (S.statement () (St.Statement (TE.encodeUtf8 statement) E.noParams (D.singleRow(D.column(D.nonNullable D.jsonb))) False))
parsed :: FromJSON a => Value -> IO a
parsed value = case fromJSON value of Success answer->pure answer;Error failure->fail failure
field :: FromJSON a => Text -> Value -> IO a
field name value = maybe (fail("missing_qualification_field="<>T.unpack name)) pure (Audit.valueField name value)
emit :: String -> Value -> IO ()
emit name value = putStrLn (name<>"="<>T.unpack(TE.decodeUtf8(BL.toStrict(encode value))))

auditRows :: Fixture -> IO Value
auditRows fixture=json fixture "SELECT coalesce(jsonb_agg(to_jsonb(a) ORDER BY id),'[]') FROM ONLY public.audit_log a"

description :: Text -> Text
description tag = T.concat[T.pack(show(hash(TE.encodeUtf8(tag<>T.pack(show number)))::Digest SHA256))|number<-[1..256::Int]]

writeConfig :: Fixture -> Int -> String -> IO ()
writeConfig fixture port extra = do
  let path=fixture.home </> ".hmem\\config.yaml"
  createDirectoryIfMissing True (takeDirectory path)
  writeFile path ("server:\n  host: 127.0.0.1\n  port: "<>show port<>"\ndatabase:\n  host: 127.0.0.1\n  port: "<>show fixture.settings.port<>"\n  name: postgres\n  user: qualification_admin\n  sslmode: disable\nembedding:\n  mode: disabled\nweb:\n  enabled: false\nlogging:\n  max_size_mb: 1\n  backup_count: 0\n"<>extra)

withServer :: Fixture -> Int -> (ProcessHandle -> IO a) -> IO a
withServer fixture=withServerChecked(void(checkFootprint fixture)) fixture

withServerChecked :: IO () -> Fixture -> Int -> (ProcessHandle -> IO a) -> IO a
withServerChecked checkSamples fixture port action=mask $ \restore -> do
  let environment=fixture.environment<>[("HMEM_AUDIT_MAINTENANCE_DB_USER","qualification_maint")]
      logPath=takeDirectory fixture.directory </> "server.log"
  executable<-getExecutablePath
  ownedHelper 5000000 ((proc executable ["--effective-config",fixture.home </> ".hmem",show fixture.settings.port,show port]){env=Just environment})
  checkSamples
  let shutdownLog=fixture.home </> ".hmem\\logs\\hmem-server.log"
  oldMarkers<-shutdownMarkerCount shutdownLog
  withPreservingLog logPath $ \handle -> do
    (_,_,_,child)<-createProcess ((proc serverExe []) {env=Just environment,create_group=True,std_in=NoStream,std_out=UseHandle handle,std_err=UseHandle handle})
    signaled<-newIORef False
    stopDeadline<-newIORef Nothing
    result <- try @SomeException $ restore $ do
      await 10000000 (checkSamples>>ownsListener child port) "actual_main_http_listen"
      action child
    ((status,graceful),cleanupCause)<-retainedCleanup "actual_main" $ do
      previousDeadline<-readIORef stopDeadline
      deadline<-case previousDeadline of
        Just end->pure end
        Nothing->do
          now<-getMonotonicTimeNSec
          let end=now+10000000000
          writeIORef stopDeadline(Just end)
          pure end
      issued<-readIORef signaled
      unless issued $ do
        writeIORef signaled True
        interruptProcessGroupOf child
        threadDelay 100000
        listening<-ownsListener child port
        childState<-getProcessExitCode child
        void(try @SomeException(putStrLn("actual_main_after_group_signal owns_exact_listener="<>show listening<>" listener_closure=unknown process="<>show childState)))
      now<-getMonotonicTimeNSec
      boundedWait (fromIntegral(if now>=deadline then 0 else(deadline-now)`div`1000)) child >>= \case
        Just status->pure(status,True)
        Nothing->do retireChild child;status<-waitForProcess child;pure(status,False)
    validation<-try @SomeException $ do
      putStrLn("actual_main_signal_join="<>show status<>" forced="<>show(not graceful))
      unless graceful(fail "actual_main_shutdown_unclean_forced")
      markers<-shutdownMarkerCount shutdownLog
      unless(markers>oldMarkers)(fail "actual_main_shutdown_entry_not_observed")
      await 2000000 (do value<-json fixture "SELECT to_jsonb(count(*)=0) FROM pg_stat_activity WHERE datname=current_database() AND application_name IN ('hmem-audit-maintenance','hmem-audit-observer')";pure(value==Bool True)) "actual_main_maintenance_backend_exit"
      disposition<-json fixture "SELECT jsonb_build_object('operation_id',operation->>'id','state',operation->>'state','settled',coalesce(operation->>'state' NOT IN('prepared','running','uncertain'),true)) FROM public.audit_maintenance_control"
      unless((Audit.valueField "settled" disposition::Maybe Bool)==Just True)(fail "actual_main_operation_not_settled_at_shutdown")
      emit "actual_main_shutdown_audit_disposition" (object["exit".=show status,"forced".=False,"shutdown_entry_marker_added".=True,"maintenance_backends_absent".=True,"operation".=disposition])
    finishOwned result(firstCause cleanupCause(either Just(const Nothing) validation))

shutdownMarkerCount :: FilePath -> IO Int
shutdownMarkerCount path=do
  exists<-doesFileExist path
  if exists then length . filter(BS.isInfixOf "hmem-server: shutting down") . BS.split 10 <$>BS.readFile path else pure 0

-- Repeated caller interruption cannot discard a created child or close its
-- inherited log capability. Each bounded retry retains the same actual owner.
retainedCleanup :: String -> IO a -> IO (a,Maybe SomeException)
retainedCleanup label action=mask_ $ do
  first<-newIORef Nothing
  let remember failure=modifyIORef' first(\old->case old of Nothing->Just failure;_->old)
      loop=try @SomeException action >>= \case
        Right value->do cause<-readIORef first;pure(value,cause)
        Left failure->do
          old<-readIORef first
          remember failure
          when(case old of Nothing->True;_->False)(void(try @SomeException(putStrLn("cleanup_pending="<>label<>" cause="<>take 512(displayException failure)))))
          waited<-try @SomeException(threadDelay 100000)
          either remember pure waited
          loop
  loop

finishOwned :: Either SomeException a -> Maybe SomeException -> IO a
finishOwned result cleanupCause=case result of
  Left failure->do
    forM_ cleanupCause(\secondary->void(try @SomeException(putStrLn("secondary_cleanup_failure="<>take 512(displayException secondary)))))
    throwIO failure
  Right value->maybe(pure value) throwIO cleanupCause

firstCause :: Maybe a -> Maybe a -> Maybe a
firstCause first later=case first of Just _->first;Nothing->later

withPreservingLog :: FilePath -> (Handle -> IO a) -> IO a
withPreservingLog path use=mask $ \restore->do
  handle<-openFile path AppendMode
  result<-try @SomeException(restore(use handle))
  closed<-try @SomeException(hClose handle)
  finishOwned result(either Just(const Nothing) closed)

requestStatus :: HTTP.Manager -> Int -> BS.ByteString -> Text -> Value -> IO (Int,Value)
requestStatus manager port method path body = do
  base<-HTTP.parseRequest("http://127.0.0.1:"<>show port<>T.unpack path)
  let query=base {HTTP.method=method,HTTP.requestHeaders=[("Content-Type","application/json"),("Connection","close")],HTTP.requestBody=HTTP.RequestBodyLBS(if body==Null then "" else encode body),HTTP.responseTimeout=HTTP.responseTimeoutMicro 500000}
  response<-HTTP.httpLbs query manager
  value<-either (fail . ("HTTP_JSON_decode="<>)) pure(eitherDecode response.responseBody)
  pure(statusCode response.responseStatus,value)
request :: HTTP.Manager -> Int -> BS.ByteString -> Text -> Value -> IO Value
request manager port method path body = do
  (status,value)<-requestStatus manager port method path body
  unless(status>=200&&status<300)(fail("HTTP_status="<>show status<>" path="<>T.unpack path<>" body="<>show value))
  pure value
assertTask :: HTTP.Manager -> Int -> Text -> Text -> Text -> IO ()
assertTask manager port path title payload=do
  start<-getMonotonicTimeNSec
  value<-request manager port "GET" path Null
  actualTitle<-field "title" value
  actualPayload<-field "description" value
  unless(actualTitle==title&&actualPayload==payload)(fail "task_value_mismatch")
  stop<-getMonotonicTimeNSec
  unless(stop-start<500000000)(fail "foreground_read_over_500ms")
  putStrLn("foreground_read_us="<>show((stop-start)`div`1000))

provision :: Fixture -> IO ()
provision fixture=sql fixture "CREATE ROLE qualification_maint LOGIN NOINHERIT NOSUPERUSER NOCREATEDB NOCREATEROLE NOREPLICATION NOBYPASSRLS; GRANT USAGE ON SCHEMA public TO qualification_maint; GRANT MAINTAIN ON public.audit_log TO qualification_maint; GRANT EXECUTE ON FUNCTION public.hmem_audit_runtime_safe(),public.hmem_audit_status(),public.hmem_audit_pause(boolean),public.hmem_audit_schedule(timestamptz,text,text,text,int,boolean),public.hmem_audit_identity(),public.hmem_audit_assess(),public.hmem_audit_record(jsonb,boolean),public.hmem_audit_operation_catalog(),public.hmem_audit_operation_begin(uuid,text,oid,boolean),public.hmem_audit_operation_running(uuid),public.hmem_audit_operation_monitor(uuid),public.hmem_audit_operation_uncertain(uuid),public.hmem_audit_operation_finish(uuid,text,jsonb,jsonb) TO qualification_maint"

seedBloat :: Fixture -> IO ()
seedBloat fixture = do
  Pool.withConn fixture.pool $ \connection -> bracket_
    (S.run(S.sql "BEGIN") connection >>=either(fail . show) pure)
    (S.run(S.sql "ROLLBACK") connection >>=either(fail . show) pure) $
    forM_ [0..9::Int] $ \batch -> do
      let statement="INSERT INTO public.audit_log(entity_type,entity_id,action,new_values) SELECT 'qualification_fixture',(SELECT string_agg(md5((g*1000+s)::text),'') FROM generate_series(1,64) s),'create',jsonb_build_object('payload',(SELECT string_agg(md5((g*1000+s)::text),'') FROM generate_series(1,192) s)) FROM generate_series("<>T.pack(show(batch*1000+1))<>","<>T.pack(show((batch+1)*1000))<>") g"
      S.run(S.sql(TE.encodeUtf8 statement)) connection >>=either(fail . show) pure
      void(checkFootprint fixture)
  sql fixture "VACUUM(INDEX_CLEANUP ON,TRUNCATE FALSE) ONLY public.audit_log"

seedOutbox :: Fixture -> Text -> IO ()
seedOutbox fixture workspace = do
  -- Seven daily batches retain six obsolete days plus one current day. These
  -- are ordinary canonical project writes; only fixture timestamps are aged.
  sql fixture ("UPDATE public.change_stream_outbox SET occurred_at=clock_timestamp()-interval '7 days' WHERE workspace_id='"<>workspace<>"'")
  forM_ [7,6..1::Int] $ \day -> do
    previous<-json fixture("SELECT to_jsonb(coalesce(max(cursor),0)::bigint) FROM public.change_stream_outbox WHERE scope='workspace' AND workspace_id='"<>workspace<>"'") >>=parsed :: IO Integer
    sql fixture ("INSERT INTO public.projects(workspace_id,name) SELECT '"<>workspace<>"','backlog-"<>T.pack(show day)<>"-'||g FROM generate_series(1,100) g; UPDATE public.change_stream_outbox SET occurred_at=clock_timestamp()-interval '"<>(if day==1 then "23 hours" else T.pack(show day)<>" days")<>"' WHERE scope='workspace' AND workspace_id='"<>workspace<>"' AND cursor>"<>T.pack(show previous))
  proof<-json fixture("SELECT jsonb_build_object('expired_rows',count(*) FILTER(WHERE occurred_at<clock_timestamp()-interval '1 day'),'retained_rows',count(*) FILTER(WHERE occurred_at>=clock_timestamp()-interval '1 day'),'oldest_age_seconds',floor(extract(epoch FROM clock_timestamp()-min(occurred_at)))::bigint,'retained_age_seconds',floor(extract(epoch FROM clock_timestamp()-max(occurred_at)))::bigint) FROM public.change_stream_outbox WHERE scope='workspace' AND workspace_id='"<>workspace<>"'")
  expired<-field "expired_rows" proof::IO Integer
  retained<-field "retained_rows" proof::IO Integer
  oldest<-field "oldest_age_seconds" proof::IO Integer
  recent<-field "retained_age_seconds" proof::IO Integer
  unless(expired>=600&&retained==100&&oldest>=604800&&recent>=82800&&recent<82900)(fail "seven_day_outbox_fixture_ages_not_proven_before_main")
  emit "seven_day_outbox_fixture" proof

verifyBackup :: Fixture -> Value -> IO ()
verifyBackup fixture expected = do
  let backup=takeDirectory fixture.directory </> "backup.dump"
      helper name arguments=ownedHelper 30000000 ((proc(pg name) arguments){env=Just fixture.environment,std_in=NoStream,std_out=NoStream,std_err=NoStream})
      clone=fixture.settings {Config.name="qualification_restore"}
  helper "pg_dump" ["--dbname",T.unpack(Config.connectionString fixture.settings),"--format=custom","--file",backup]
  void(checkFootprint fixture)
  backupBytes<-allocatedFile True backup
  unless(backupBytes<=134217728)(fail "backup_restore_128MiB_bound")
  restoredPath<-newIORef Nothing
  bracket_ (sql fixture "CREATE DATABASE qualification_restore") (sql fixture "DROP DATABASE qualification_restore") $ do
    helper "pg_restore" ["--dbname",T.unpack(Config.connectionString clone),"--exit-on-error","--no-owner","--no-privileges",backup]
    void(checkFootprint fixture)
    cloneOid<-json fixture "SELECT to_jsonb(oid::bigint) FROM pg_database WHERE datname='qualification_restore'" >>=parsed :: IO Integer
    let clonePath=fixture.directory </> "base" </> show cloneOid
    writeIORef restoredPath(Just clonePath)
    restoredBytes<-nativeTree clonePath
    unless(backupBytes+restoredBytes<=134217728)(fail "backup_restore_128MiB_bound")
    emit "backup_restore_native_allocation" (object["dump_bytes".=backupBytes,"restored_database_bytes".=restoredBytes,"aggregate_limit_bytes".=(134217728::Integer)])
    restored<-bracket (Pool.createPool(Config.connectionString clone) 1 30 2000) destroyAllResources $ \pool -> Pool.runSession pool(S.statement () (St.Statement "SELECT coalesce(jsonb_agg(to_jsonb(a) ORDER BY id),'[]') FROM ONLY public.audit_log a" E.noParams(D.singleRow(D.column(D.nonNullable D.jsonb))) False))
    -- Project backlog can add audit rows. The pre-seed API history must be an
    -- exact preserved subset; receipt hashes bind the complete restored set.
    original<-auditRows fixture
    unless(restored==original)(fail "backup_restore_audit_mismatch")
    entries<-parsed original :: IO [Value]
    old<-parsed expected :: IO [Value]
    unless(all (`elem` entries) old)(fail "backup_lost_original_api_history")
    identity<-json fixture "SELECT jsonb_build_object('system_identifier',(SELECT system_identifier::text FROM pg_control_system()),'database_oid',(SELECT oid::bigint FROM pg_database WHERE datname=current_database()),'recovery_point',pg_current_wal_lsn()::text)"
    now<-getCurrentTime
    digest<-shaFile backup
    let receipt=object["system_identifier".=(Audit.valueField "system_identifier" identity::Maybe Text),"database_oid".=(Audit.valueField "database_oid" identity::Maybe Int64),"recovery_point".=(Audit.valueField "recovery_point" identity::Maybe Text),"verified_at".=now,"restore_verified".=True,"backup_sha256".=digest,"restore_evidence_sha256".=T.pack(show(hash(BL.toStrict(encode restored))::Digest SHA256))]
    Pool.runSession fixture.pool(S.statement receipt(St.Statement "SELECT public.hmem_audit_backup_attest($1)" (E.param(E.nonNullable E.jsonb)) D.noResult False))
  removeFile backup
  readIORef restoredPath >>=mapM_(\path->await 2000000 (not <$>doesDirectoryExist path) "restored_database_directory_removed")
  putStrLn "backup_restored_and_fixture_clone_removed=True"

nativeTree :: FilePath -> IO Integer
nativeTree path=do
  linked<-pathIsSymbolicLink path
  when linked(fail "owned_native_tree_reparse_refused")
  directory<-doesDirectoryExist path
  if directory then listDirectory path >>=mapM(nativeTree . (path</>)) >>=pure . sum else allocatedFile True path

shaFile :: FilePath -> IO Text
shaFile path = T.pack . show . (hash :: BS.ByteString -> Digest SHA256) <$> BS.readFile path

-- Relation catalog already deduplicates heap, TOAST, their indexes and bound
-- audit indexes. Each entry is measured once with mandatory main, optional
-- forks, and no unsupported >1GiB segment assumptions hidden in the sum.
catalog :: Fixture -> IO [(Value,Integer)]
catalog fixture = do
  let cfg=Config.defaultConfig {Config.database=fixture.settings,Config.auditMaintenance=Config.defaultAuditMaintenanceConfig {Config.maintenanceUser=Just "qualification_maint"}}
  value<-Audit.withAuditConnection cfg $ \connection->do
    locked<-Audit.tryAuditLock connection
    unless locked(fail "qualification_catalog_lock_busy")
    Audit.operationCatalog connection
  entries<-parsed value :: IO [Value]
  forM entries $ \entry -> do
    relative<-field "path" entry
    when(isAbsolute relative||any (`elem` ["..","."])(splitDirectories relative))(fail "catalog_path_outside_fixture")
    let path=fixture.directory </> relative
    doesFileExist(path<>".1") >>= \present->when present(fail "qualification_segment_bound_exceeded")
    allocated<-allocatedFile True path
    forks<-mapM(allocatedFile False)[path<>"_fsm",path<>"_vm",path<>"_init"]
    pure(entry,allocated+sum forks)

-- 25ms sampling reports the largest actually observed native AllocationSize,
-- not an inferred exact transient maximum. Data excludes pg_wal. Backup and
-- restore are gone before sampling; no file appears in both aggregates.
withSamples :: Fixture -> (IO () -> IO a) -> IO (a,Value)
withSamples fixture action=mask $ \restore -> do
  stop<-newIORef False
  failure<-newIORef Nothing
  latest<-newIORef Nothing
  started<-getMonotonicTimeNSec
  stats<-newIORef(0::Integer,0::Integer,0::Integer,0::Int,0::Word64)
  let check=do
        readIORef failure >>=maybe(pure()) throwIO
        now<-getMonotonicTimeNSec
        previous<-readIORef latest
        when(now-maybe started id previous>1000000000)(fail "qualification_sampler_stale")
      loop=do
        (dataBytes,walBytes,rootBytes)<-checkFootprint fixture
        now<-getMonotonicTimeNSec
        previous<-readIORef latest
        modifyIORef' stats(\(oldData,oldWal,oldRoot,count,gap)->(max oldData dataBytes,max oldWal walBytes,max oldRoot rootBytes,count+1,max gap(now-maybe started id previous)))
        writeIORef latest(Just now)
        done<-readIORef stop
        unless done(threadDelay 25000>>loop)
  sample<-async(try @SomeException loop >>=either(\cause->writeIORef failure(Just cause)) pure)
  result<-try @SomeException $ restore $ do
    await 1000000 (check>>readIORef latest >>=pure . maybe False(const True)) "first_native_sample_before_main_admission"
    value<-action check
    check
    pure value
  writeIORef stop True
  -- A native sample is not asynchronously discarded; retain the actual owner
  -- until that child joins even when the assertion/caller is interrupted.
  let join=try @SomeException(waitCatch sample)>>=either(const join) pure
  sampled<-join
  sampledCause<-readIORef failure
  value<-finishOwned result(case sampled of Left cause->Just cause;Right()->sampledCause)
  (dataPeak,walPeak,rootPeak,count,gap)<-readIORef stats
  pure(value,object["data_peak_allocated_bytes".=dataPeak,"wal_peak_allocated_bytes".=walPeak,"owned_root_peak_allocated_bytes".=rootPeak,"samples".=count,"sleep_interval_ms".=(25::Int),"maximum_sample_gap_us".=(gap`div`1000),"freshness_limit_ms".=(1000::Int),"confidence".=("observed_non_atomic_sample_maxima"::Text)])

checkFootprint :: Fixture -> IO (Integer,Integer,Integer)
checkFootprint fixture=do
  (dataBytes,walBytes)<-sampleAllocation fixture
  let root=takeDirectory(takeDirectory fixture.directory)
      walk path
        | normalise path==normalise fixture.directory=pure(0,0)
        | otherwise=do
            linked<-pathIsSymbolicLink path
            when linked(fail "qualification_root_reparse_refused")
            directory<-doesDirectoryExist path
            if directory then do
              names<-listDirectory path
              sizes<-mapM(walk . (path</>)) names
              pure(sum(map fst sizes),sum(map snd sizes))
            else do
              allocated<-allocatedFile False path
              logSize<-if ".log" `isPrefixOf` takeExtension path || "hmem-server.log" `isPrefixOf` takeFileName path then getFileSize path else pure 0
              pure(allocated,logSize)
  (otherBytes,logBytes)<-walk root
  let total=dataBytes+walBytes+otherBytes
  unless(dataBytes<=268435456&&walBytes<=536870912&&total<=1073741824&&logBytes<=2097152)(fail "qualification_declared_data_wal_root_or_log_cap_exceeded")
  pure(dataBytes,walBytes,total)

sampleAllocation :: Fixture -> IO(Integer,Integer)
sampleAllocation fixture=do
  let walk path=do
        linked<-pathIsSymbolicLink path
        when linked(fail "qualification_sampler_reparse_refused")
        names<-listDirectory path
        sizes<-forM names $ \name->do
          let child=path </> name
          directory<-doesDirectoryExist child
          if directory then walk child else allocatedFile False child
        pure(sum sizes)
  children<-listDirectory fixture.directory
  dataSizes<-forM(filter(/="pg_wal") children) $ \name->do
    let child=fixture.directory </> name
    directory<-doesDirectoryExist child
    if directory then walk child else allocatedFile False child
  walBytes<-walk(fixture.directory </> "pg_wal")
  pure(sum dataSizes,walBytes)

#ifdef mingw32_HOST_OS
foreign import ccall safe "CreateFileW" createFile :: CWString -> Word32 -> Word32 -> Ptr () -> Word32 -> Word32 -> Ptr () -> IO (Ptr ())
foreign import ccall safe "GetFileInformationByHandleEx" fileInformation :: Ptr () -> Int32 -> Ptr () -> Word32 -> IO Int32
foreign import ccall unsafe "CloseHandle" closeHandle :: Ptr () -> IO Int32
foreign import ccall safe "GetExtendedTcpTable" listenerTable :: Ptr () -> Ptr Word32 -> Int32 -> Word32 -> Int32 -> Word32 -> IO Word32
foreign import ccall safe "GetDiskFreeSpaceExW" diskFree :: CWString -> Ptr Word64 -> Ptr Word64 -> Ptr Word64 -> IO Int32

effectiveFree :: FilePath -> IO Integer
effectiveFree root=withCWString root $ \wide -> alloca $ \available -> alloca $ \total -> alloca $ \free -> do
  ok<-diskFree wide available total free
  unless(ok/=0)(fail "effective_capacity_unknown")
  toInteger <$>peek available

ownsListener :: ProcessHandle -> Int -> IO Bool
ownsListener process port=do
  identifier<-getPid process
  state<-getProcessExitCode process
  case(identifier,state)of
    (Just owner,Nothing)->allocaBytes 1048576 $ \buffer->alloca $ \bytes->do
      poke bytes 1048576
      status<-listenerTable buffer bytes 0 2 3 0
      unless(status==0)(fail "native_listener_proof_unavailable")
      count<-peek(castPtr buffer::Ptr Word32)
      when(count>4096)(fail "native_listener_count_bound")
      rows<-forM[0..fromIntegral count-1::Int] $ \number->do
        let row=buffer `plusPtr` (4+number*24)
        tcpState<-peekByteOff row 0::IO Word32
        address<-peekByteOff row 4::IO Word32
        rawPort<-peekByteOff row 8::IO Word32
        pid<-peekByteOff row 20::IO Word32
        let hostPort=fromIntegral((rawPort .&. 255)*256+((rawPort `div`256).&.255))::Int
        pure(tcpState,address,hostPort,pid)
      pure([ (address,pid)|(tcpState,address,local,pid)<-rows,tcpState==2,local==port,address==0||address==0x0100007f]==[(0x0100007f,fromIntegral owner)])
    _->pure False
allocatedFile :: Bool -> FilePath -> IO Integer
allocatedFile required path=withCWString path $ \wide->bracket
  (createFile wide 0x80 7 nullPtr 3 0x200000 nullPtr)
  (\handle->when(ptrToIntPtr handle/=(-1))(void(closeHandle handle))) $ \handle->
    if ptrToIntPtr handle==(-1) then getLastError >>= \code->
      if not required&&(code==2||code==3) then pure 0 else fail("allocation_open_error="<>show code)
    else allocaBytes 24 $ \buffer->do
      tags<-fileInformation handle 9 buffer 8
      unless(tags/=0)(fail "allocation_attribute_query_failed")
      attributes<-peek(castPtr buffer::Ptr Word32)
      when(attributes .&. 0xe10/=0)(fail "qualification_requires_ordinary_non_sparse_uncompressed_files")
      ok<-fileInformation handle 1 buffer 24
      unless(ok/=0)(fail "allocation_query_failed")
      size<-peek(castPtr buffer::Ptr Int64)
      when(size<0)(fail "allocation_negative")
      pure(toInteger size)
#else
effectiveFree :: FilePath -> IO Integer
effectiveFree _=fail "native_windows_qualification_only"
ownsListener :: ProcessHandle -> Int -> IO Bool
ownsListener _ _=fail "native_windows_qualification_only"
allocatedFile :: Bool -> FilePath -> IO Integer
allocatedFile _ _=fail "native_windows_qualification_only"
#endif

collectSnapshot :: IO () -> HTTP.Manager -> Int -> Text -> Value -> IO [Value]
collectSnapshot checkSamples manager port workspace first=go (0::Int) [] first
  where
    go pages accumulated value=do
      when(pages>=10)(fail "qualification_snapshot_page_bound")
      checkSamples
      items<-field "items" value
      more<-field "has_more" value
      if more then do
        token<-field "next_page_token" value :: IO Text
        next<-request manager port "POST" "/api/v1/change-stream/resync" (object["scope".=object["scope".=("workspace"::Text),"workspace_id".=workspace],"page_token".=token,"page_size".=(100::Int)])
        go (pages+1) (accumulated<>items) next
      else pure(accumulated<>items)

safeRemoveTree :: FilePath -> IO ()
safeRemoveTree path=do
  let allowed=repo </> ".tmp\\audit-qualification-df58e815"
  actual<-canonicalizePath path
  unless(normalise actual==normalise path&&takeDirectory actual==allowed)(fail "qualification_cleanup_target_mismatch")
  start<-getMonotonicTimeNSec
  let deadline=start+5000000000
      remove current=do
        now<-getMonotonicTimeNSec
        when(now>=deadline)(fail "qualification_cleanup_deadline_retaining_owned_tree")
        linked<-pathIsSymbolicLink current
        when linked(fail "qualification_cleanup_reparse_refused")
        folder<-doesDirectoryExist current
        if folder then listDirectory current >>=mapM_(remove . (current</>))>>removeDirectory current else removeFile current
  remove actual
  remaining<-doesPathExist path
  when remaining(fail "qualification_cleanup_not_absent")
  putStrLn("owned_fixture_removed="<>path)
