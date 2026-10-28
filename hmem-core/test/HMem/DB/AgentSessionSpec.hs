module HMem.DB.AgentSessionSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (cancel, concurrently, waitCatch, withAsync)
import Control.Exception (SomeException, AsyncException(..), bracket, bracket_, try, finally, onException, catch, throwIO, displayException)
import Control.Monad (forM_, forM, foldM, replicateM)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as B8
import Data.Aeson qualified as Aeson
import Data.Aeson.Types (parseEither)
import Data.Pool (Pool, destroyAllResources)
import Data.IORef (newIORef, atomicModifyIORef')
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.UUID (UUID)
import Data.UUID.V4 qualified as UUID
import Data.Time (addUTCTime)
import Hasql.Decoders qualified as D
import Hasql.Connection qualified as Hasql
import Hasql.Encoders qualified as E
import Hasql.Session qualified as S
import Hasql.Statement qualified as St
import System.Directory (copyFile, createDirectory, listDirectory)
import System.FilePath ((</>))
import System.Timeout (timeout)
import System.Environment (lookupEnv)
import System.Process
import System.IO (hGetLine, hPutStrLn, hFlush, hClose)
import System.Exit (ExitCode(..))
import Test.Hspec

import HMem.AgentSession
import HMem.BotIdentity (mkBotIdentity)
import HMem.DB.AgentSession
import HMem.DB.Migration qualified as Migration
import HMem.DB.Pool (DBException(..), PoolMetrics(..), getPoolMetrics, createPool, runSession, runTransaction, withConn)
import HMem.DB.RequestContext
import HMem.DB.Task qualified as Task
import HMem.DB.Project qualified as Project
import HMem.DB.TestHarness
import HMem.Types qualified as Types

spec :: Spec
spec = do
  around withTestEnv $ describe "Agent sessions" $ do

    it "HistoricalImport links completed tasks without changing live rules or exposing partial evidence" $ \env -> do
      workspace <- (.id) <$> createTestWorkspace env "historical-import"
      forM_ ["done","cancelled"] $ \status -> do
        task <- makeTask env workspace "Historical work"
        execute env ("UPDATE tasks SET status='"<>status<>"' WHERE id='"<>uuid task.id<>"'")
        identifier <- UUID.nextRandom
        let body="{\"message\":\"historical\"}\r\n[1,2]"
            metadata=CreateSession identifier workspace "historical-jsonl" "Historical completed work" Nothing Nothing task.id
            manifest=ImportSession metadata (fromIntegral (BS.length body)) (sessionSha256 body)
        expectCode (SessionTaskNotInProgress (TE.decodeUtf8 status)) $ createSession env.pool metadata
        row <- importSession env.pool manifest
        importSession env.pool manifest `shouldReturn` row
        expectCode SessionConflict $ importSession env.pool (manifest {finalSha256=sessionSha256 "different"})
        expectCode SessionConflict $ createSession env.pool metadata
        (row.importTaskId,row.importBytes,row.importSha256) `shouldBe` (Just task.id,Just (fromIntegral (BS.length body)),Just (sessionSha256 body))
        page <- listTaskSessions env.pool workspace task.id Nothing 50
        page.items `shouldBe` []
        workspaceSessionActivity env.pool [workspace] `shouldReturn` [(workspace,False)]
        expectCode SessionConflict $ setRuntimeIdentity env.pool workspace identifier (SetRuntimeIdentity 0 Nothing)
        expectCode SessionConflict $ terminateSession env.pool workspace identifier (receipt row "")
        sealIdleSessionWithClock env.pool (pure (addUTCTime 7200 row.lastSeenAt)) (pure ()) row `shouldReturn` Nothing
        rejectDb $ execute env ("UPDATE sessions SET state='terminated',terminated_at=clock_timestamp(),final_bytes=0,final_sha256='"<>TE.encodeUtf8 (sessionSha256 "")<>"',revision=revision+1 WHERE id='"<>uuid identifier<>"'")
        rejectDb $ execute env ("UPDATE sessions SET import_bytes=import_bytes+1 WHERE id='"<>uuid identifier<>"'")
        appended <- (.session) <$> append env workspace row body True
        sealed <- terminateSession env.pool workspace identifier (receipt appended body)
        allBytes env workspace identifier `shouldReturn` body
        pageDone <- listTaskSessions env.pool workspace task.id Nothing 50
        map (.id) pageDone.items `shouldBe` [identifier]
        importSession env.pool manifest `shouldReturn` sealed
        Just unchanged <- Task.getTask env.pool task.id
        Aeson.toJSON unchanged.status `shouldBe` Aeson.String (TE.decodeUtf8 status)
    it "HistoricalImport upgrades V048 atomically without rewriting legacy evidence" $ \_ ->
      withTestSandbox $ \sandbox -> withSandboxedEnv sandbox $ withSandboxedPostgres sandbox $ \db ->
        bracket (createPool db.testDbConnStr 2 30 30000) destroyAllResources $ \pool -> do
          source <- resolveMigrationsDir sandbox.sandboxRepoRoot
          let beforeDir=sandbox.sandboxRoot </> "before-v049"
              upgradeDir=sandbox.sandboxRoot </> "only-v049"
              filename="V049__historical_session_import.sql"
              env=TestEnv pool sandbox db
          createDirectory beforeDir; createDirectory upgradeDir
          files <- listDirectory source
          forM_ (filter (\name -> name < "V049" && ".sql" `T.isSuffixOf` T.pack name) files) $ \name -> copyFile (source </> name) (beforeDir </> name)
          copyFile (source </> filename) (upgradeDir </> filename)
          Migration.runMigrations pool beforeDir >>= (\result -> result.failed `shouldBe` Nothing)
          (workspace,row) <- legacyFreshFixture env
          uploaded <- (.session) <$> append env workspace row "{}\r\n" False
          sealed <- terminateSession pool workspace row.id (receipt uploaded "{}\r\n")
          execute env "CREATE TABLE pre_v049_sessions AS SELECT to_jsonb(s) AS value FROM sessions s; CREATE TABLE pre_v049_counts AS SELECT (SELECT count(*) FROM audit_log) AS audits,(SELECT count(*) FROM change_stream_outbox) AS events"
          bracket_
            (execute env "CREATE FUNCTION import_ledger_failure() RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN IF NEW.version=49 THEN RAISE EXCEPTION 'injected import ledger failure'; END IF; RETURN NEW; END; $$; CREATE TRIGGER import_ledger_failure BEFORE INSERT ON schema_migrations FOR EACH ROW EXECUTE FUNCTION import_ledger_failure()")
            (execute env "DROP TRIGGER import_ledger_failure ON schema_migrations; DROP FUNCTION import_ledger_failure()") $ do
              Migration.runMigrations pool upgradeDir >>= (\result -> result.failed `shouldSatisfy` (/=Nothing))
              boolean env "SELECT NOT EXISTS(SELECT 1 FROM information_schema.columns WHERE table_schema='public' AND table_name='sessions' AND column_name='import_task_id') AND NOT EXISTS(SELECT 1 FROM schema_migrations WHERE version=49)" `shouldReturn` True
          Migration.runMigrations pool upgradeDir >>= (\result -> result.applied `shouldBe` [filename])
          boolean env "SELECT NOT EXISTS(SELECT 1 FROM sessions s WHERE (to_jsonb(s)-ARRAY['import_task_id','import_bytes','import_sha256']) NOT IN (SELECT value FROM pre_v049_sessions) OR s.import_task_id IS NOT NULL OR s.import_bytes IS NOT NULL OR s.import_sha256 IS NOT NULL) AND (SELECT count(*) FROM audit_log)=(SELECT audits FROM pre_v049_counts) AND (SELECT count(*) FROM change_stream_outbox)=(SELECT events FROM pre_v049_counts)" `shouldReturn` True
          getSession pool workspace row.id `shouldReturn` Just sealed
          allBytes env workspace row.id `shouldReturn` "{}\r\n"
          Migration.runMigrations pool upgradeDir >>= (\result -> result.applied `shouldBe` [])

    it "SealCAS releases each bounded group lease and refuses an append winner" $ \env -> do
      (workspace,_,row) <- reaperFixture env
      let record = BS.replicate 1022 32 <> "0\n"; chunk = BS.concat (replicate 1024 record)
      current <- foldM (\prior _ -> (.session) <$> append env workspace prior chunk False) row [1..5 :: Int]
      calls <- newIORef (0 :: Int)
      let during = do
            metrics <- getPoolMetrics
            metrics.activeConnections `shouldBe` 0
            index <- atomicModifyIORef' calls (\n -> (n+1,n))
            if index /= 0 then pure () else do
              result <- timeout 1000000 (append env workspace current "[]\n" False)
              result `shouldSatisfy` maybe False (const True)
      expectCode SessionConflict $ terminateSessionWithHook env.pool during workspace row.id (receipt current (BS.concat (replicate 5 chunk)))
      Just after <- getSession env.pool workspace row.id
      after.state `shouldBe` Active
      after.committedBytes `shouldBe` current.committedBytes+3

    it "SealCAS preserves an exact concurrent seal and the latest independent runtime metadata" $ \env -> do
      (workspace,_,row) <- reaperFixture env
      uploaded <- append env workspace row "{}\r\n" False
      let input = receipt uploaded.session "{}\r\n"
          during = do
            _ <- setRuntimeIdentity env.pool workspace row.id (SetRuntimeIdentity 0 (Just (RuntimeIdentity "actual" Nothing Nothing (Just "Developer"))))
            _ <- terminateSession env.pool workspace row.id input
            pure ()
      sealed <- terminateSessionWithHook env.pool during workspace row.id input
      sealed.state `shouldBe` Terminated
      sealed.runtimeRevision `shouldBe` 1
      sealed.finalSha256 `shouldBe` Just (sessionSha256 "{}\r\n")
      terminateSession env.pool workspace row.id input `shouldReturn` sealed
      expectCode SessionTerminated $ append env workspace sealed "[]\n" False

    it "SealCAS rolls back a late publication failure and retains the verified active prefix" $ \env -> do
      (workspace,_,row) <- reaperFixture env
      uploaded <- append env workspace row "{}\n" False
      execute env "CREATE TABLE seal_failure_counts AS SELECT (SELECT count(*) FROM audit_log) AS audits,(SELECT count(*) FROM change_stream_outbox) AS events"
      bracket_
        (execute env "CREATE FUNCTION session_seal_failure() RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN IF NEW.state='terminated' THEN RAISE EXCEPTION USING ERRCODE='23514',MESSAGE='injected seal publication failure'; END IF; RETURN NEW; END $$; CREATE TRIGGER zz_session_seal_failure AFTER UPDATE ON sessions FOR EACH ROW EXECUTE FUNCTION session_seal_failure()")
        (execute env "DROP TRIGGER zz_session_seal_failure ON sessions; DROP FUNCTION session_seal_failure()") $ do
          rejectDb $ terminateSession env.pool workspace row.id (receipt uploaded.session "{}\n")
          getSession env.pool workspace row.id `shouldReturn` Just uploaded.session
          boolean env "SELECT (SELECT count(*) FROM audit_log)=(SELECT audits FROM seal_failure_counts) AND (SELECT count(*) FROM change_stream_outbox)=(SELECT events FROM seal_failure_counts)" `shouldReturn` True
      terminateSession env.pool workspace row.id (receipt uploaded.session "{}\n") >>= (\sealed -> sealed.state `shouldBe` Terminated)

    it "SealCAS releases a cancelled group and fails a missing prefix window without publishing" $ \env -> do
      (workspace,_,row) <- reaperFixture env
      uploaded <- append env workspace row "{}\n" False
      result <- try @AsyncException $ terminateSessionWithHook env.pool (throwIO ThreadKilled) workspace row.id (receipt uploaded.session "{}\n")
      result `shouldSatisfy` either (==ThreadKilled) (const False)
      getPoolMetrics >>= (\metrics -> metrics.activeConnections `shouldBe` 0)
      getSession env.pool workspace row.id `shouldReturn` Just uploaded.session
      bracket_ (execute env "ALTER TABLE session_chunks DISABLE TRIGGER session_chunk_guard") (execute env "ALTER TABLE session_chunks ENABLE TRIGGER session_chunk_guard") $
        execute env ("DELETE FROM session_chunks WHERE session_id='"<>uuid row.id<>"'")
      expectCode SessionInvalidInput $ terminateSession env.pool workspace row.id (receipt uploaded.session "{}\n")
      expectCode SessionInvalidInput $ readSessionRange env.pool workspace row.id 0 maxSessionRangeBytes
      getSession env.pool workspace row.id `shouldReturn` Just uploaded.session

    it "SealCAS reads the immutable committed window without waiting on an uncommitted append" $ \env -> do
      (workspace,_,row) <- reaperFixture env
      uploaded <- append env workspace row "{}\n" False
      bracket (createPool env.testDb.testDbConnStr 1 5 30000) destroyAllResources $ \pool -> withConn pool $ \connection -> do
        let sql command = S.run (S.sql command) connection >>= either (fail . show) pure
        sql ("BEGIN; INSERT INTO session_chunks(session_id,start_offset,revision,data,allow_final_record,record_count,ends_with_newline) VALUES ('"<>uuid row.id<>"',3,2,convert_to(E'[]\\n','UTF8'),false,0,false)")
        (do
          value <- timeout 1000000 (readSessionRange env.pool workspace row.id 0 maxSessionRangeBytes)
          fmap (.bytes) value `shouldBe` Just "{}\n"
          fmap ((.committedBytes) . (.session)) value `shouldBe` Just uploaded.session.committedBytes
          sql "COMMIT") `onException` sql "ROLLBACK"
      allBytes env workspace row.id `shouldReturn` "{}\n[]\n"

    it "SealCAS rolls back a later pipeline SQL error and keeps the lease reusable" $ \env -> do
      (workspace,_,row) <- reaperFixture env
      let chunk = BS.concat (replicate 1024 (BS.replicate 1022 32 <> "0\n"))
          body = BS.concat (replicate 5 chunk)
      current <- foldM (\prior _ -> (.session) <$> append env workspace prior chunk False) row [1..5 :: Int]
      once <- newIORef False
      let during = do
            changed <- atomicModifyIORef' once (\old -> (True,old))
            if changed then pure () else execute env "ALTER TABLE session_chunks RENAME COLUMN data TO pipeline_fixture_data"
          restore = execute env "DO $$ BEGIN IF EXISTS(SELECT 1 FROM information_schema.columns WHERE table_schema='public' AND table_name='session_chunks' AND column_name='pipeline_fixture_data') THEN ALTER TABLE session_chunks RENAME COLUMN pipeline_fixture_data TO data; END IF; END $$"
      outcome <- try @SomeException $ do
        result <- try @DBException $ terminateSessionWithHook env.pool during workspace row.id (receipt current body)
        result `shouldSatisfy` either (\case DBOtherError _ -> True; _ -> False) (const False)
        getPoolMetrics >>= (\metrics -> metrics.activeConnections `shouldBe` 0)
        getSession env.pool workspace row.id `shouldReturn` Just current
      restored <- try @SomeException restore
      case (outcome,restored) of
        (Left primary,Left cleanup) -> fail ("Pipeline assertion: "<>displayException primary<>"; restoration: "<>displayException cleanup)
        (Left primary,_) -> throwIO primary
        (_,Left cleanup) -> throwIO cleanup
        _ -> pure ()
      timeout 1000000 (getSession env.pool workspace row.id) `shouldReturn` Just (Just current)
      terminateSession env.pool workspace row.id (receipt current body) >>= (\sealed -> sealed.finalSha256 `shouldBe` Just (sessionSha256 body))

    it "DBCycleMeasurement reports optional native driver protocol cycles without retaining payloads" $ \env -> do
      enabled <- lookupEnv "HMEM_SESSION_DB_CYCLES"
      if enabled /= Just "1" then pendingWith "opt-in PostgreSQL protocol measurement; Node is not a core CI prerequisite" else
        withCycleCounter env $ \pool snapshot -> do
          forM_ [1048576,1024] $ \chunkSize -> do
            (workspace,_,row) <- reaperFixture env
            let total = 1048576; record = BS.replicate 1022 32 <> "0\n"
                body = BS.concat (replicate 1024 record)
                count = total `div` chunkSize
                expression = "convert_to(repeat(repeat(' ',1022)||E'0\\n',"<>B8.pack (show (chunkSize `div` 1024))<>"),'UTF8')"
            execute env ("DO $$ DECLARE n integer; BEGIN FOR n IN 0.."<>B8.pack(show(count-1))<>" LOOP INSERT INTO session_chunks(session_id,start_offset,revision,data,allow_final_record,record_count,ends_with_newline) VALUES ('"<>uuid row.id<>"',n*"<>B8.pack(show chunkSize)<>",n+1,"<>expression<>",false,0,false); END LOOP; END $$")
            Just current <- getSession env.pool workspace row.id
            current.committedBytes `shouldBe` fromIntegral total
            _ <- getSession pool workspace row.id -- prime pool/startup outside the measured operation
            beforeCycles <- snapshot
            sealed <- terminateSession pool workspace row.id (receipt current body)
            afterCycles <- snapshot
            sealed.finalSha256 `shouldBe` Just (sessionSha256 body)
            putStrLn ("[session-db-cycles] "<>show chunkSize<>" "<>show (zipWith (-) afterCycles beforeCycles))
            last afterCycles `shouldBe` 0

    it "PrefixProof accepts only scoped exact append boundaries through later growth" $ \env -> do
      (workspace,_,row) <- reaperFixture env
      let first = "{\"text\":\"first\"}\r\n"; second = "{\"text\":\"second\"}\r\n"
      a <- appendSession env.pool workspace row.id (AppendSession 0 0 first False)
      _ <- appendSession env.pool workspace row.id (AppendSession a.session.committedBytes a.session.revision second False)
      validateSessionPrefixBoundary env.pool workspace row.id 0 0 `shouldReturn` True
      validateSessionPrefixBoundary env.pool workspace row.id a.session.committedBytes a.session.revision `shouldReturn` True
      validateSessionPrefixBoundary env.pool workspace row.id (a.session.committedBytes-1) a.session.revision `shouldReturn` False
      validateSessionPrefixBoundary env.pool workspace row.id a.session.committedBytes (a.session.revision+1) `shouldReturn` False
      other <- createTestWorkspace env "prefix-foreign"
      validateSessionPrefixBoundary env.pool other.id row.id a.session.committedBytes a.session.revision `shouldReturn` False
      readSessionPrefixWindow env.pool workspace row.id a.session.committedBytes 0 `shouldReturn` first

    it "PrefixProof hashes bounded ordered raw windows across the one MiB boundary" $ \env -> do
      (workspace,_,row) <- reaperFixture env
      let record = "{\"text\":\"" <> BS.replicate (65536-13) 97 <> "\"}\r\n"
          first = BS.concat (replicate 16 record); tailBytes = "{\"text\":\"tail\"}\r\n"
      BS.length first `shouldBe` 1048576
      a <- appendSession env.pool workspace row.id (AppendSession 0 0 first False)
      b <- appendSession env.pool workspace row.id (AppendSession a.session.committedBytes a.session.revision tailBytes False)
      one <- readSessionPrefixWindow env.pool workspace row.id b.session.committedBytes 0
      two <- readSessionPrefixWindow env.pool workspace row.id b.session.committedBytes 1048576
      BS.length one `shouldBe` 1048576
      one `shouldBe` first
      two `shouldBe` tailBytes
      sessionSha256 (one<>two) `shouldBe` sessionSha256 (first<>tailBytes)

    it "automatically seals only strictly idle terminal-task sessions with the canonical empty receipt" $ \env -> do
      (workspace,task,row) <- reaperFixture env
      execute env ("UPDATE tasks SET status='done' WHERE id='"<>uuid task.id<>"'")
      let boundary = addUTCTime 3600 row.lastSeenAt
      listIdleSessionCandidatesAt env.pool boundary Nothing `shouldReturn` []
      sealIdleSessionWithClock env.pool (pure boundary) (pure ()) row `shouldReturn` Nothing
      let after = addUTCTime 0.001 boundary
      candidates <- listIdleSessionCandidatesAt env.pool after Nothing
      map (.id) candidates `shouldBe` [row.id]
      Just sealed <- sealIdleSessionWithClock env.pool (pure after) (pure ()) row
      sealed.state `shouldBe` Terminated
      sealed.finalBytes `shouldBe` Just 0
      sealed.finalSha256 `shouldBe` Just (sessionSha256 "")
      sealed.lastSeenAt `shouldBe` row.lastSeenAt
      sealIdleSessionWithClock env.pool (pure after) (pure ()) row `shouldReturn` Nothing
      workspaceSessionActivity env.pool [workspace] `shouldReturn` [(workspace,False)]
      getSession env.pool workspace row.id `shouldReturn` Just sealed

    it "uses last activity rather than creation age and keeps unfinished deleted task links conservative" $ \env -> do
      (workspace,task,row) <- reaperFixture env
      second <- makeTask env workspace "Unfinished deleted task"
      associateSessionTask env.pool workspace row.id second.id
      execute env ("UPDATE tasks SET status='done' WHERE id='"<>uuid task.id<>"'; UPDATE tasks SET deleted_at=clock_timestamp() WHERE id='"<>uuid second.id<>"'")
      let clock = pure (addUTCTime 7200 row.lastSeenAt)
      sealIdleSessionWithClock env.pool clock (pure ()) row `shouldReturn` Nothing
      execute env ("UPDATE tasks SET status='cancelled' WHERE id='"<>uuid second.id<>"'")
      Just sealed <- sealIdleSessionWithClock env.pool clock (pure ()) row
      sealed.state `shouldBe` Terminated
      -- An old creation time cannot make recent heartbeat activity eligible.
      identifier <- UUID.nextRandom
      initial <- makeTask env workspace "Recent old session task"
      runTransaction env.pool $ S.sql ("INSERT INTO sessions(id,workspace_id,format_label,title,created_at,last_seen_at) VALUES ('"<>uuid identifier<>"','"<>uuid workspace<>"','jsonl','Recently active old session',clock_timestamp()-interval '2 days',clock_timestamp()); INSERT INTO task_sessions(workspace_id,session_id,task_id) VALUES ('"<>uuid workspace<>"','"<>uuid identifier<>"','"<>uuid initial.id<>"')")
      execute env ("UPDATE tasks SET status='done' WHERE id='"<>uuid initial.id<>"'")
      Just recent <- getSession env.pool workspace identifier
      sealIdleSession env.pool recent `shouldReturn` Nothing

    it "hashes every immutable chunk and preserves the latest independent runtime identity" $ \env -> do
      (workspace,task,initial) <- reaperFixture env
      first <- append env workspace initial "{\"one\":1}\n" False
      second <- append env workspace first.session "{\"two\":2}\r\n" False
      execute env ("UPDATE tasks SET status='done' WHERE id='"<>uuid task.id<>"'")
      once <- newIORef False
      let row = second.session
          during = do
            seen <- atomicModifyIORef' once (\old -> (True,old))
            if seen then pure () else do
              _ <- setRuntimeIdentity env.pool workspace row.id (SetRuntimeIdentity 0 (Just (RuntimeIdentity "actual-model" Nothing Nothing (Just "Developer"))))
              pure ()
      result <- timeout 5000000 (sealIdleSessionWithClock env.pool (pure (addUTCTime 7200 row.lastSeenAt)) during row)
      Just (Just sealed) <- pure result
      sealed.finalSha256 `shouldBe` Just (sessionSha256 "{\"one\":1}\n{\"two\":2}\r\n")
      sealed.finalBytes `shouldBe` Just 21
      sealed.runtimeRevision `shouldBe` 1
      (fmap (.role) sealed.runtimeIdentity) `shouldBe` Just (Just "Developer")
      allBytes env workspace row.id `shouldReturn` "{\"one\":1}\n{\"two\":2}\r\n"

    it "does not seal after heartbeat, append, new task link or task reopening during hashing" $ \env -> do
      forM_ ["heartbeat","append","link","reopen"] $ \operation -> do
        (workspace,task,initial) <- reaperFixture env
        uploaded <- append env workspace initial "{}\n" False
        execute env ("UPDATE tasks SET status='done' WHERE id='"<>uuid task.id<>"'")
        let row = uploaded.session
            during = case operation of
              "heartbeat" -> threadDelay 1000 >> heartbeatSession env.pool workspace row.id >> pure ()
              "append" -> append env workspace row "{\"later\":true}\n" False >> pure ()
              "link" -> makeTask env workspace "New linked running task" >>= \new -> associateSessionTask env.pool workspace row.id new.id
              _ -> execute env ("UPDATE tasks SET status='in_progress' WHERE id='"<>uuid task.id<>"'")
        timeout 5000000 (sealIdleSessionWithClock env.pool (pure (addUTCTime 7200 row.lastSeenAt)) during row) `shouldReturn` Just Nothing
        Just current <- getSession env.pool workspace row.id
        current.state `shouldBe` Active

    it "leaves a concurrently explicitly sealed receipt unchanged" $ \env -> do
      (workspace,task,initial) <- reaperFixture env
      uploaded <- append env workspace initial "{}\n" False
      execute env ("UPDATE tasks SET status='done' WHERE id='"<>uuid task.id<>"'")
      let row = uploaded.session
          during = terminateSession env.pool workspace row.id (receipt row "{}\n") >> pure ()
      timeout 5000000 (sealIdleSessionWithClock env.pool (pure (addUTCTime 7200 row.lastSeenAt)) during row) `shouldReturn` Just Nothing
      Just sealed <- getSession env.pool workspace row.id
      sealed.finalSha256 `shouldBe` Just (sessionSha256 "{}\n")
      sealed.revision `shouldBe` row.revision + 1

    it "installs the idle-session scan index and ledger atomically without rewriting populated sessions" $ \_ ->
      withTestSandbox $ \sandbox -> withSandboxedEnv sandbox $ withSandboxedPostgres sandbox $ \db ->
        bracket (createPool db.testDbConnStr 2 30 30000) destroyAllResources $ \pool -> do
          source <- resolveMigrationsDir sandbox.sandboxRepoRoot
          let beforeDir = sandbox.sandboxRoot </> "before-v047"
              upgradeDir = sandbox.sandboxRoot </> "only-v047"
              filename = "V047__idle_session_scan.sql"
              env = TestEnv pool sandbox db
          createDirectory beforeDir
          createDirectory upgradeDir
          files <- listDirectory source
          forM_ (filter (\name -> name < "V047" && ".sql" `T.isSuffixOf` T.pack name) files) $ \name -> copyFile (source </> name) (beforeDir </> name)
          copyFile (source </> filename) (upgradeDir </> filename)
          Migration.runMigrations pool beforeDir >>= (\result -> result.failed `shouldBe` Nothing)
          workspace <- (.id) <$> createTestWorkspace env "idle-session-reaper"
          task <- makeTask env workspace "Session reaper fixture"
          row <- legacySessionFixture env workspace task.id
          bracket_
            (execute env "CREATE FUNCTION idle_index_ledger_failure() RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN IF NEW.version=47 THEN RAISE EXCEPTION 'injected idle index ledger failure'; END IF; RETURN NEW; END; $$; CREATE TRIGGER idle_index_ledger_failure BEFORE INSERT ON schema_migrations FOR EACH ROW EXECUTE FUNCTION idle_index_ledger_failure()")
            (execute env "DROP TRIGGER idle_index_ledger_failure ON schema_migrations; DROP FUNCTION idle_index_ledger_failure()") $ do
              Migration.runMigrations pool upgradeDir >>= (\result -> result.failed `shouldSatisfy` (/=Nothing))
              boolean env "SELECT to_regclass('sessions_active_last_seen_id_idx') IS NULL AND NOT EXISTS(SELECT 1 FROM schema_migrations WHERE version=47)" `shouldReturn` True
          upgraded <- Migration.runMigrations pool upgradeDir
          upgraded.failed `shouldBe` Nothing
          upgraded.applied `shouldBe` [filename]
          boolean env "SELECT indisvalid AND indpred IS NOT NULL FROM pg_index WHERE indexrelid='sessions_active_last_seen_id_idx'::regclass" `shouldReturn` True
          getSession pool workspace row.id `shouldReturn` Just row
          Migration.runMigrations pool upgradeDir >>= (\result -> result.applied `shouldBe` [])

    it "creates the initial running-task link atomically and preserves later completion, retry and sealing" $ \env -> do
      workspace <- createTestWorkspace env "initial-session-task"
      task <- makeTask env workspace.id "Initial running task"
      identifier <- UUID.nextRandom
      let input=CreateSession identifier workspace.id "jsonl" "Capture running task" Nothing Nothing task.id
      row <- createSession env.pool input
      page <- listSessionTasks env.pool workspace.id identifier Nothing 50
      map (.id) page.items `shouldBe` [task.id]
      execute env ("UPDATE tasks SET status='done' WHERE id='"<>uuid task.id<>"'")
      createSession env.pool input `shouldReturn` row
      -- Creation-only task input is not an instruction to add another link.
      createSession env.pool input {taskId=workspace.id} `shouldReturn` row
      uploaded <- append env workspace.id row "{}\n" False
      sealed <- terminateSession env.pool workspace.id identifier (receipt uploaded.session "{}\n")
      sealed.state `shouldBe` Terminated
      createSession env.pool input `shouldReturn` sealed
      allBytes env workspace.id identifier `shouldReturn` "{}\n"
      execute env ("UPDATE tasks SET deleted_at=clock_timestamp() WHERE id='"<>uuid task.id<>"'")
      createSession env.pool input `shouldReturn` sealed

    it "rejects missing, foreign, deleted and non-running initial tasks without publishing session effects" $ \env -> do
      workspace <- createTestWorkspace env "refused-session-task"
      other <- createTestWorkspace env "foreign-session-task"
      foreignTask <- makeTask env other.id "Foreign running task"
      deleted <- makeTask env workspace.id "Deleted running task"
      execute env ("UPDATE tasks SET deleted_at=clock_timestamp() WHERE id='"<>uuid deleted.id<>"'")
      missing <- UUID.nextRandom
      let refused code target = do
            identifier <- UUID.nextRandom
            expectCode code $ createSession env.pool (CreateSession identifier workspace.id "jsonl" "Rejected task capture" Nothing Nothing target)
            boolean env ("SELECT NOT EXISTS(SELECT 1 FROM sessions WHERE id='"<>uuid identifier<>"') AND NOT EXISTS(SELECT 1 FROM task_sessions WHERE session_id='"<>uuid identifier<>"') AND NOT EXISTS(SELECT 1 FROM audit_log WHERE entity_type='agent_session' AND entity_id='"<>uuid identifier<>"') AND NOT EXISTS(SELECT 1 FROM change_stream_outbox WHERE envelope#>>'{entity,id}'='"<>uuid identifier<>"')") `shouldReturn` True
      forM_ [missing,foreignTask.id,deleted.id] (refused SessionTaskNotFound)
      forM_ ["todo","blocked","done","cancelled"] $ \status -> do
        task <- Task.createTask env.pool (Types.CreateTask workspace.id Nothing Nothing "Non-running initial task" Nothing Nothing Nothing)
        execute env ("UPDATE tasks SET status='"<>TE.encodeUtf8 status<>"' WHERE id='"<>uuid task.id<>"'")
        refused (SessionTaskNotInProgress status) task.id

    it "rejects raw orphan commits and task completion inside the creation transaction while preserving rollback" $ \env -> do
      workspace <- createTestWorkspace env "raw-session-initial-task"
      task <- makeTask env workspace.id "Raw running task"
      orphan <- UUID.nextRandom
      closed <- UUID.nextRandom
      valid <- UUID.nextRandom
      let insert identifier="INSERT INTO sessions(id,workspace_id,format_label,title) VALUES ('"<>uuid identifier<>"','"<>uuid workspace.id<>"','jsonl','Raw task capture');"
          link identifier="INSERT INTO task_sessions(workspace_id,session_id,task_id) VALUES ('"<>uuid workspace.id<>"','"<>uuid identifier<>"','"<>uuid task.id<>"');"
      rejectDb $ runTransaction env.pool $ S.sql (insert orphan)
      rejectDb $ runTransaction env.pool $ S.sql (insert closed<>link closed<>"UPDATE tasks SET status='done' WHERE id='"<>uuid task.id<>"'")
      boolean env ("SELECT NOT EXISTS(SELECT 1 FROM sessions WHERE id IN ('"<>uuid orphan<>"','"<>uuid closed<>"')) AND NOT EXISTS(SELECT 1 FROM task_sessions WHERE session_id IN ('"<>uuid orphan<>"','"<>uuid closed<>"')) AND (SELECT status='in_progress' FROM tasks WHERE id='"<>uuid task.id<>"') AND NOT EXISTS(SELECT 1 FROM audit_log WHERE entity_type='agent_session' AND entity_id IN ('"<>uuid orphan<>"','"<>uuid closed<>"'))") `shouldReturn` True
      runTransaction env.pool $ S.sql (insert valid<>link valid)
      getSession env.pool workspace.id valid >>= (`shouldSatisfy` (/=Nothing))
      execute env ("UPDATE tasks SET status='done' WHERE id='"<>uuid task.id<>"'")
      Just row <- getSession env.pool workspace.id valid
      terminateSession env.pool workspace.id valid (receipt row "") >>= (\sealed -> sealed.state `shouldBe` Terminated)

    it "honors explicit immediate creation validation without allowing missing or non-running initial links" $ \env -> do
      workspace <- createTestWorkspace env "immediate-session-initial-task"
      running <- makeTask env workspace.id "Immediate initial task"
      waiting <- Task.createTask env.pool (Types.CreateTask workspace.id Nothing Nothing "Waiting initial task" Nothing Nothing Nothing)
      orphan <- UUID.nextRandom
      eagerOrphan <- UUID.nextRandom
      invalid <- UUID.nextRandom
      missing <- UUID.nextRandom
      valid <- UUID.nextRandom
      let insert identifier="INSERT INTO sessions(id,workspace_id,format_label,title) VALUES ('"<>uuid identifier<>"','"<>uuid workspace.id<>"','jsonl','Immediate task capture');"
          link identifier task="INSERT INTO task_sessions(workspace_id,session_id,task_id) VALUES ('"<>uuid workspace.id<>"','"<>uuid identifier<>"','"<>uuid task<>"');"
          immediate="SET CONSTRAINTS session_initial_task_guard IMMEDIATE;"
      rejectDb $ runTransaction env.pool $ S.sql (insert orphan<>immediate)
      rejectDb $ runTransaction env.pool $ S.sql (immediate<>insert eagerOrphan)
      forM_ [waiting.id,missing] $ \target -> do
        identifier <- UUID.nextRandom
        rejectDb $ runTransaction env.pool $ S.sql (immediate<>"WITH inserted AS (INSERT INTO sessions(id,workspace_id,format_label,title) VALUES ('"<>uuid identifier<>"','"<>uuid workspace.id<>"','jsonl','Immediate task capture') RETURNING id,workspace_id) INSERT INTO task_sessions(workspace_id,session_id,task_id) SELECT workspace_id,id,'"<>uuid target<>"' FROM inserted")
        getSession env.pool workspace.id identifier `shouldReturn` Nothing
      rejectDb $ runTransaction env.pool $ S.sql (insert invalid<>link invalid waiting.id<>immediate)
      rejectDb $ runTransaction env.pool $ S.sql (insert missing<>link missing missing<>immediate)
      boolean env ("SELECT NOT EXISTS(SELECT 1 FROM sessions WHERE id IN ('"<>uuid orphan<>"','"<>uuid invalid<>"','"<>uuid missing<>"'))") `shouldReturn` True
      -- An explicit immediate check establishes valid creation before this
      -- deliberately later lifecycle operation in the same transaction.
      runTransaction env.pool $ S.sql (insert valid<>link valid running.id<>immediate<>"UPDATE tasks SET status='done' WHERE id='"<>uuid running.id<>"'")
      Just row <- getSession env.pool workspace.id valid
      page <- listSessionTasks env.pool workspace.id valid Nothing 50
      map (.id) page.items `shouldBe` [running.id]
      uploaded <- append env workspace.id row "{}\n" False
      terminateSession env.pool workspace.id valid (receipt uploaded.session "{}\n") >>= (\sealed -> sealed.state `shouldBe` Terminated)

    it "serializes creation against both orders of task completion" $ \env -> do
      workspace <- createTestWorkspace env "creation-task-race"
      task <- makeTask env workspace.id "Racing initial task"
      identifier <- UUID.nextRandom
      withSessionRacePools env $ \peer probePool -> do
        -- The fixture connection is held by the blocker. The third pool keeps
        -- both observation and the independent winner outside that lease.
        let probe=env {pool=probePool}
            input=CreateSession identifier workspace.id "jsonl" "Race task capture" Nothing Nothing task.id
        withConn env.pool $ \connection -> do
          let sql command=S.run (S.sql command) connection >>= either (fail . show) pure
          sql ("BEGIN; UPDATE tasks SET status='done' WHERE id='"<>uuid task.id<>"'")
          withAsync (try @SessionException $ createSession peer input) $ \worker ->
            (do
              withinSessionRace (waitForLock probe "SELECT t.status%") `shouldReturn` True
              sql "COMMIT"
              result <- withinSessionRace (waitCatch worker) >>= either (fail . show) pure
              fmap (.code) (either Just (const Nothing) result) `shouldBe` Just (SessionTaskNotInProgress "done")
              getSession peer workspace.id identifier `shouldReturn` Nothing) `finally` sql "ROLLBACK"
        execute env ("UPDATE tasks SET status='in_progress' WHERE id='"<>uuid task.id<>"'")
        withConn env.pool $ \connection -> do
          let sql command=S.run (S.sql command) connection >>= either (fail . show) pure
          sql ("BEGIN; INSERT INTO sessions(id,workspace_id,format_label,title) VALUES ('"<>uuid identifier<>"','"<>uuid workspace.id<>"','jsonl','Race task capture'); INSERT INTO task_sessions(workspace_id,session_id,task_id) VALUES ('"<>uuid workspace.id<>"','"<>uuid identifier<>"','"<>uuid task.id<>"')")
          withAsync (execute (env {pool=peer}) ("UPDATE tasks SET status='done' WHERE id='"<>uuid task.id<>"'")) $ \worker ->
            (do
              withinSessionRace (waitForLock probe "UPDATE tasks SET status=%") `shouldReturn` True
              sql "COMMIT"
              withinSessionRace (waitCatch worker) >>= either (fail . show) pure) `finally` sql "ROLLBACK"
        Just row <- getSession peer workspace.id identifier
        createSession peer input `shouldReturn` row

    it "rereads a concurrently created canonical session when its creation-only task validation loses a race" $ \env -> do
      workspace <- createTestWorkspace env "session-retry-task-race"
      waiting <- makeTask env workspace.id "Waiting retry task"
      winning <- makeTask env workspace.id "Winning creation task"
      identifier <- UUID.nextRandom
      withSessionRacePools env $ \peer probePool -> do
        let probe=env {pool=probePool}
            input=CreateSession identifier workspace.id "jsonl" "Retry racing creation" Nothing Nothing waiting.id
        withConn env.pool $ \connection -> do
          let sql command=S.run (S.sql command) connection >>= either (fail . show) pure
          -- A read lock holds the losing lookup without owning the workspace
          -- outbox sequence needed by the concurrently winning creation.
          sql ("BEGIN; SELECT 1 FROM tasks WHERE id='"<>uuid waiting.id<>"' FOR UPDATE")
          withAsync (createSession peer input) $ \worker ->
            (do
              withinSessionRace (waitForLock probe "SELECT t.status%") `shouldReturn` True
              canonical <- withinSessionRace (createSession probePool input {taskId=winning.id})
              sql ("UPDATE tasks SET status='done' WHERE id='"<>uuid waiting.id<>"'")
              sql "COMMIT"
              withinSessionRace (waitCatch worker) >>= either (fail . show) (`shouldBe` canonical)
              links <- listSessionTasks peer workspace.id identifier Nothing 50
              map (.id) links.items `shouldBe` [winning.id]) `finally` sql "ROLLBACK"

    it "upgrades V044 legacy orphans without rewriting evidence and rolls back failed V045 installation" $ \_ ->
      withTestSandbox $ \sandbox -> withSandboxedEnv sandbox $ withSandboxedPostgres sandbox $ \db ->
        bracket (createPool db.testDbConnStr 2 30 30000) destroyAllResources $ \pool -> do
          source <- resolveMigrationsDir sandbox.sandboxRepoRoot
          let beforeDir=sandbox.sandboxRoot </> "before-v045"
              upgradeDir=sandbox.sandboxRoot </> "only-v045"
              filename="V045__session_initial_task.sql"
              env=TestEnv pool sandbox db
          createDirectory beforeDir
          createDirectory upgradeDir
          files <- listDirectory source
          forM_ (filter (\name -> name < "V045" && ".sql" `T.isSuffixOf` T.pack name) files) $ \name -> copyFile (source </> name) (beforeDir </> name)
          copyFile (source </> filename) (upgradeDir </> filename)
          Migration.runMigrations pool beforeDir >>= (\result -> result.failed `shouldBe` Nothing)
          workspace <- createTestWorkspace env "legacy-orphan-session-upgrade"
          active <- UUID.nextRandom
          sealed <- UUID.nextRandom
          abandoned <- UUID.nextRandom
          forM_ [active,sealed] $ \identifier -> execute env ("INSERT INTO sessions(id,workspace_id,format_label,title) VALUES ('"<>uuid identifier<>"','"<>uuid workspace.id<>"','jsonl','Legacy orphan capture')")
          execute env ("INSERT INTO sessions(id,workspace_id,format_label,title,last_seen_at) VALUES ('"<>uuid abandoned<>"','"<>uuid workspace.id<>"','jsonl','Idle legacy orphan',clock_timestamp()-interval '2 hours')")
          execute env ("UPDATE sessions SET state='terminated',terminated_at=clock_timestamp(),final_bytes=0,final_sha256='"<>TE.encodeUtf8 (sessionSha256 "")<>"',revision=1 WHERE id='"<>uuid sealed<>"'")
          execute env "CREATE TABLE pre_v045_sessions AS SELECT * FROM sessions; CREATE TABLE pre_v045_counts AS SELECT (SELECT count(*) FROM audit_log) AS audits,(SELECT count(*) FROM change_stream_outbox) AS events"
          bracket_
            (execute env "CREATE FUNCTION initial_task_ledger_failure() RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN IF NEW.version=45 THEN RAISE EXCEPTION 'injected initial task ledger failure'; END IF; RETURN NEW; END; $$; CREATE TRIGGER initial_task_ledger_failure BEFORE INSERT ON schema_migrations FOR EACH ROW EXECUTE FUNCTION initial_task_ledger_failure()")
            (execute env "DROP TRIGGER initial_task_ledger_failure ON schema_migrations; DROP FUNCTION initial_task_ledger_failure()") $ do
              Migration.runMigrations pool upgradeDir >>= (\result -> result.failed `shouldSatisfy` (/=Nothing))
              boolean env "SELECT to_regprocedure('hmem_session_initial_task_guard()') IS NULL AND NOT EXISTS(SELECT 1 FROM schema_migrations WHERE version=45)" `shouldReturn` True
          upgraded <- Migration.runMigrations pool upgradeDir
          upgraded.failed `shouldBe` Nothing
          upgraded.applied `shouldBe` [filename]
          boolean env "SELECT NOT EXISTS((SELECT to_jsonb(s) FROM sessions s EXCEPT SELECT to_jsonb(s) FROM pre_v045_sessions s) UNION ALL (SELECT to_jsonb(s) FROM pre_v045_sessions s EXCEPT SELECT to_jsonb(s) FROM sessions s)) AND (SELECT count(*) FROM audit_log)=(SELECT audits FROM pre_v045_counts) AND (SELECT count(*) FROM change_stream_outbox)=(SELECT events FROM pre_v045_counts)" `shouldReturn` True
          Just legacy <- getSession pool workspace.id active
          createSession pool (CreateSession active workspace.id "jsonl" "Legacy orphan capture" Nothing Nothing workspace.id) `shouldReturn` legacy
          terminateSession pool workspace.id active (receipt legacy "") >>= (\row -> row.state `shouldBe` Terminated)
          newer <- UUID.nextRandom
          rejectDb $ execute env ("INSERT INTO sessions(id,workspace_id,format_label,title) VALUES ('"<>uuid newer<>"','"<>uuid workspace.id<>"','jsonl','Rejected orphan capture')")
          Migration.runMigrations pool upgradeDir >>= (\result -> result.applied `shouldBe` [])
          -- The V045 assertions above retain their historical schema boundary.
          -- Exercise today's reaper only after installing its complete schema.
          Migration.runMigrations pool source >>= (\result -> result.failed `shouldBe` Nothing)
          Just oldOrphan <- getSession pool workspace.id abandoned
          Just savedOrphan <- sealIdleSession pool oldOrphan
          savedOrphan.finalSha256 `shouldBe` Just (sessionSha256 "")
          savedOrphan.state `shouldBe` Terminated

    it "keeps runtime declarations separate from immutable caller and transcript receipts, including retry and clear" $ \env -> do
      workspace <- createTestWorkspace env "runtime-identities"
      initial <- makeTask env workspace.id "Runtime session task"
      identifier <- UUID.nextRandom
      caller <- either (fail . T.unpack) pure (mkBotIdentity "Connector" "codex:connection-model:high")
      updater <- either (fail . T.unpack) pure (mkBotIdentity "Manager" "codex:another-connection-model:low")
      let actor = EffectiveActor ActorBot "connector" "Connector" (Just caller)
          manager = EffectiveActor ActorBot "manager" "Manager" (Just updater)
          first = RuntimeIdentity "actual-model-a" Nothing Nothing (Just "developer")
          second = RuntimeIdentity "actual-model-b" (Just "Known provider") (Just "high") (Just "reviewer")
          input = CreateSession identifier workspace.id "jsonl" "Separate runtime identity" Nothing (Just first) initial.id
          body = "{\"text\":\"original bytes\"}\r\n"
          stable row = ((row.creatorActorType,row.creatorActorId,row.creatorActorLabel,row.creatorBotIdentity),
            (row.createdAt,row.lastSeenAt,row.committedBytes,row.committedRecords,row.revision,row.endsWithNewline),
            (row.state,row.terminatedAt,row.finalBytes,row.finalSha256,row.title,row.parentSessionId))
      created <- withEffectiveActorContext (Just actor) $ createSession env.pool input
      created.creatorBotIdentity `shouldBe` Just caller
      created.runtimeIdentity `shouldBe` Just first
      created.runtimeRevision `shouldBe` 0
      before <- (.session) <$> append env workspace.id created body False
      changed <- withEffectiveActorContext (Just manager) $ setRuntimeIdentity env.pool workspace.id identifier (SetRuntimeIdentity 0 (Just second))
      stable changed `shouldBe` stable before
      changed.runtimeRevision `shouldBe` 1
      changed.runtimeIdentity `shouldBe` Just second
      replay <- setRuntimeIdentity env.pool workspace.id identifier (SetRuntimeIdentity 0 (Just second))
      replay `shouldBe` changed
      setRuntimeIdentity env.pool workspace.id identifier (SetRuntimeIdentity 1 (Just second)) `shouldReturn` changed
      boolean env ("SELECT count(*)=3 AND bool_or(actor_id='manager' AND new_values->'runtime_identity'->>'model'='actual-model-b') FROM audit_log WHERE entity_type='agent_session' AND entity_id='"<>uuid identifier<>"'") `shouldReturn` True
      boolean env ("SELECT NOT EXISTS(SELECT 1 FROM change_stream_outbox WHERE envelope#>>'{entity,type}'='agent_session' AND envelope#>>'{entity,id}'='"<>uuid identifier<>"' AND envelope#>>'{entity,action}'='updated' AND (envelope->'invalidations')::text LIKE '%active_sessions:%')") `shouldReturn` True
      retried <- withEffectiveActorContext (Just actor) $ createSession env.pool input
      retried `shouldBe` changed
      expectCode SessionRuntimeConflict $ setRuntimeIdentity env.pool workspace.id identifier (SetRuntimeIdentity 0 (Just first))
      cleared <- setRuntimeIdentity env.pool workspace.id identifier (SetRuntimeIdentity 1 Nothing)
      stable cleared `shouldBe` stable before
      cleared.runtimeIdentity `shouldBe` Nothing
      cleared.runtimeRevision `shouldBe` 2
      setRuntimeIdentity env.pool workspace.id identifier (SetRuntimeIdentity 1 Nothing) `shouldReturn` cleared
      forM_ ["runtime_revision=runtime_revision+1", "runtime_identity='{\"model\":\"Other\"}',runtime_revision=runtime_revision+2", "runtime_identity='{\"model\":\"Other\"}',runtime_revision=runtime_revision+1,last_seen_at=clock_timestamp()", "runtime_identity='{\"model\":\"Other\"}',runtime_revision=runtime_revision+1,revision=revision+1", "runtime_identity='{\"model\":\"Other\"}',runtime_revision=runtime_revision+1,creator_actor_label='Impersonated'"] $ \mutation ->
        rejectDb $ execute env ("UPDATE sessions SET "<>mutation<>" WHERE id='"<>uuid identifier<>"'")
      forM_ ["'{}'", "'{\"model\":\"\"}'", "'{\"model\":\" valid \"}'", "'{\"model\":\"A\",\"invented\":\"B\"}'", "'{\"model\":\"A\",\"provider\":null}'"] $ \value ->
        rejectDb $ execute env ("UPDATE sessions SET runtime_identity="<>value<>"::jsonb,runtime_revision=runtime_revision+1 WHERE id='"<>uuid identifier<>"'")
      sealed <- terminateSession env.pool workspace.id identifier (receipt cleared body)
      sealed.runtimeIdentity `shouldBe` Nothing
      sealed.runtimeRevision `shouldBe` 2
      allBytes env workspace.id identifier `shouldReturn` body
      expectCode SessionTerminated $ setRuntimeIdentity env.pool workspace.id identifier (SetRuntimeIdentity 2 (Just first))

    it "serializes competing runtime changes with append and sealing without sharing transcript revisions" $ \env -> do
      (workspace,row) <- fresh env
      let a=RuntimeIdentity "Model A" Nothing Nothing (Just "developer"); b=RuntimeIdentity "Model B" Nothing Nothing (Just "developer")
      bracket (createPool env.testDb.testDbConnStr 1 30 30000) destroyAllResources $ \firstPool ->
        bracket (createPool env.testDb.testDbConnStr 1 30 30000) destroyAllResources $ \secondPool ->
          bracket (createPool env.testDb.testDbConnStr 1 30 30000) destroyAllResources $ \probePool -> do
        let probe=env {pool=probePool}
        (left,right) <- withConn env.pool $ \connection -> do
          let sql command=S.run (S.sql command) connection >>= either (fail . show) pure
              blocked 0=do
                phase <- runSession probePool $ S.statement () $ St.Statement
                  "SELECT coalesce(string_agg(state||':'||coalesce(wait_event_type,'-')||':'||left(query,240),E'\\n'),'') FROM pg_stat_activity WHERE datname=current_database() AND pid<>pg_backend_pid()"
                  E.noParams (D.singleRow (D.column (D.nonNullable D.text))) False
                expectationFailure ("Runtime contender phase: "<>T.unpack phase)
                pure False
              blocked attempts=do
                waiting <- boolean probe "SELECT count(*)>=2 FROM pg_stat_activity WHERE datname=current_database() AND pid<>pg_backend_pid() AND wait_event_type='Lock' AND query LIKE 'SELECT to_jsonb(s)%'"
                if waiting then pure True else threadDelay 10000 >> blocked (attempts-1)
          sql ("BEGIN; SELECT 1 FROM sessions WHERE id='"<>uuid row.id<>"' FOR UPDATE")
          withAsync (try @SessionException $ setRuntimeIdentity firstPool workspace row.id (SetRuntimeIdentity 0 (Just a))) $ \first ->
            withAsync (try @SessionException $ setRuntimeIdentity secondPool workspace row.id (SetRuntimeIdentity 0 (Just b))) $ \second ->
              (do
                blocked (500 :: Int) `shouldReturn` True
                sql "COMMIT"
                l <- waitCatch first >>= either (fail . show) pure
                r <- waitCatch second >>= either (fail . show) pure
                pure (l,r)) `onException` sql "ROLLBACK"
        length [() | Right _ <- [left,right]] `shouldBe` 1
        [e.code | Left e <- [left,right]] `shouldBe` [SessionRuntimeConflict]
        Just winner <- getSession probePool workspace row.id
        let body="{\"text\":\"parallel append\"}\n"
        (_,changed) <- concurrently (append (env {pool=firstPool}) workspace row body False)
          (setRuntimeIdentity secondPool workspace row.id (SetRuntimeIdentity winner.runtimeRevision Nothing))
        changed.runtimeIdentity `shouldBe` Nothing
        Just combined <- getSession probePool workspace row.id
        combined.committedBytes `shouldBe` fromIntegral (BS.length body)
        combined.revision `shouldBe` 1
        combined.runtimeRevision `shouldBe` 2
        (runtimeResult,sealed) <- concurrently (try @SessionException $ setRuntimeIdentity firstPool workspace row.id (SetRuntimeIdentity 2 (Just a)))
          (terminateSession secondPool workspace row.id (receipt combined body))
        sealed.state `shouldBe` Terminated
        case runtimeResult of
          Left e -> e.code `shouldBe` SessionTerminated
          Right updated -> updated.runtimeRevision `shouldBe` 3
        Just final <- getSession probePool workspace row.id
        final.runtimeRevision `shouldBe` maybe 2 (.runtimeRevision) (either (const Nothing) Just runtimeResult)
        final.finalSha256 `shouldBe` Just (sessionSha256 body)
        final.revision `shouldBe` 2

    it "requires a runtime role prospectively while preserving populated roleless lifecycle and evidence" $ \_ ->
      withTestSandbox $ \sandbox -> withSandboxedEnv sandbox $ withSandboxedPostgres sandbox $ \db ->
        bracket (createPool db.testDbConnStr 2 30 30000) destroyAllResources $ \pool -> do
          source <- resolveMigrationsDir sandbox.sandboxRepoRoot
          let beforeDir=sandbox.sandboxRoot </> "before-v046"
              upgradeDir=sandbox.sandboxRoot </> "only-v046"
              filename="V046__session_runtime_roles.sql"
              env=TestEnv pool sandbox db
              legacy=RuntimeIdentity "Legacy runtime" Nothing Nothing Nothing
              declared=legacy {role=Just "developer"}
              body="{\"message\":\"legacy bytes\"}\r\n"
          createDirectory beforeDir
          createDirectory upgradeDir
          files <- listDirectory source
          forM_ (filter (\name -> name < "V046" && ".sql" `T.isSuffixOf` T.pack name) files) $ \name -> copyFile (source </> name) (beforeDir </> name)
          copyFile (source </> filename) (upgradeDir </> filename)
          Migration.runMigrations pool beforeDir >>= (\result -> result.failed `shouldBe` Nothing)
          workspace <- createTestWorkspace env "legacy-role-upgrade"
          task <- makeTask env workspace.id "Legacy running agent"
          activeId <- UUID.nextRandom
          sealedId <- UUID.nextRandom
          forM_ [activeId,sealedId] $ \identifier ->
            execute env ("INSERT INTO sessions(id,workspace_id,format_label,title,runtime_identity) VALUES ('"<>uuid identifier<>"','"<>uuid workspace.id<>"','jsonl','Legacy agent role','{\"model\":\"Legacy runtime\"}'); INSERT INTO task_sessions(workspace_id,task_id,session_id) VALUES ('"<>uuid workspace.id<>"','"<>uuid task.id<>"','"<>uuid identifier<>"')")
          Just activeBefore <- getSession pool workspace.id activeId
          Just sealedEmpty <- getSession pool workspace.id sealedId
          sealedBefore <- terminateSession pool workspace.id sealedId (receipt sealedEmpty "")
          execute env "CREATE TABLE role_upgrade_audit_expected AS SELECT count(*) AS n FROM audit_log"
          Migration.runMigrations pool upgradeDir >>= (\result -> result.applied `shouldBe` [filename])
          getSession pool workspace.id activeId `shouldReturn` Just activeBefore
          getSession pool workspace.id sealedId `shouldReturn` Just sealedBefore
          boolean env "SELECT (SELECT count(*) FROM audit_log)=(SELECT n FROM role_upgrade_audit_expected)" `shouldReturn` True
          execute env ("UPDATE tasks SET status='done' WHERE id='"<>uuid task.id<>"'")
          createSession pool (CreateSession activeId workspace.id "jsonl" "Legacy agent role" Nothing (Just legacy) task.id) `shouldReturn` activeBefore
          setRuntimeIdentity pool workspace.id activeId (SetRuntimeIdentity 0 (Just legacy)) `shouldReturn` activeBefore
          appended <- (.session) <$> append env workspace.id activeBefore body False
          beat <- heartbeatSession pool workspace.id activeId
          beat.runtimeIdentity `shouldBe` Just legacy
          rejected <- UUID.nextRandom
          expectCode SessionInvalidInput $ createSession pool (CreateSession rejected workspace.id "jsonl" "Missing runtime role" Nothing (Just legacy) task.id)
          expectCode SessionInvalidInput $ setRuntimeIdentity pool workspace.id activeId (SetRuntimeIdentity 0 (Just (legacy {model="Other"})))
          rejectDb $ execute env ("INSERT INTO sessions(id,workspace_id,format_label,title,runtime_identity) VALUES ('"<>uuid rejected<>"','"<>uuid workspace.id<>"','jsonl','Missing runtime role','{\"model\":\"Other\"}')")
          forM_ ["null","\"\"","\" developer \"","123","\""<>B8.replicate 129 'x'<>"\""] $ \roleValue ->
            rejectDb $ execute env ("UPDATE sessions SET runtime_identity='{\"model\":\"Other\",\"role\":"<>roleValue<>"}',runtime_revision=runtime_revision+1 WHERE id='"<>uuid activeId<>"'")
          updated <- setRuntimeIdentity pool workspace.id activeId (SetRuntimeIdentity 0 (Just declared))
          updated.runtimeIdentity `shouldBe` Just declared
          (updated.committedBytes,updated.revision,updated.lastSeenAt) `shouldBe` (appended.committedBytes,beat.revision,beat.lastSeenAt)
          sealed <- terminateSession pool workspace.id activeId (receipt updated body)
          allBytes env workspace.id activeId `shouldReturn` body
          sealed.runtimeIdentity `shouldBe` Just declared
          expectCode SessionTerminated $ setRuntimeIdentity pool workspace.id sealedId (SetRuntimeIdentity 0 (Just declared))
          createSession pool (CreateSession sealedId workspace.id "jsonl" "Legacy agent role" Nothing (Just declared) task.id) `shouldReturn` sealedBefore

    it "upgrades populated V043 runtime to unknown without altering caller, active bytes or sealed receipts" $ \_ ->
      withTestSandbox $ \sandbox -> withSandboxedEnv sandbox $ withSandboxedPostgres sandbox $ \db ->
        bracket (createPool db.testDbConnStr 2 30 30000) destroyAllResources $ \pool -> do
          source <- resolveMigrationsDir sandbox.sandboxRepoRoot
          let beforeDir=sandbox.sandboxRoot </> "before-v044"
              upgradeDir=sandbox.sandboxRoot </> "only-v044"
              filename="V044__session_runtime_identity.sql"
              env=TestEnv pool sandbox db
              body="{\"text\":\"legacy runtime evidence\"}\r\n"
          createDirectory beforeDir
          createDirectory upgradeDir
          files <- listDirectory source
          forM_ (filter (\name -> name < "V044" && ".sql" `T.isSuffixOf` T.pack name) files) $ \name -> copyFile (source </> name) (beforeDir </> name)
          copyFile (source </> filename) (upgradeDir </> filename)
          before <- Migration.runMigrations pool beforeDir
          before.failed `shouldBe` Nothing
          workspace <- createTestWorkspace env "legacy-runtime-upgrade"
          activeId <- UUID.nextRandom
          sealedId <- UUID.nextRandom
          caller <- either (fail . T.unpack) pure (mkBotIdentity "Connector" "codex:legacy-connector:high")
          let actor=EffectiveActor ActorBot "legacy-connector" "Legacy Connector" (Just caller)
          forM_ [activeId,sealedId] $ \identifier -> do
            execute env ("INSERT INTO sessions(id,workspace_id,format_label,title,creator_actor_type,creator_actor_id,creator_actor_label,creator_bot_identity) VALUES ('"<>uuid identifier<>"','"<>uuid workspace.id<>"','jsonl','Legacy runtime capture','bot','legacy-connector','Legacy Connector','{\"role\":\"Connector\",\"model-id\":\"codex:legacy-connector:high\"}'); INSERT INTO session_chunks(session_id,start_offset,revision,data,allow_final_record,record_count,ends_with_newline) VALUES ('"<>uuid identifier<>"',0,1,convert_to(E'{\"text\":\"legacy runtime evidence\"}\\r\\n','UTF8'),false,0,false)")
          execute env ("UPDATE sessions SET state='terminated',terminated_at=clock_timestamp(),final_bytes=committed_bytes,final_sha256='"<>TE.encodeUtf8 (sessionSha256 body)<>"',revision=revision+1 WHERE id='"<>uuid sealedId<>"'")
          execute env "CREATE TABLE runtime_legacy_expected AS SELECT id,to_jsonb(s) AS value FROM sessions s; CREATE TABLE runtime_legacy_audit_expected AS SELECT count(*) AS n FROM audit_log"
          bracket_ (execute env "CREATE FUNCTION runtime_ledger_failure() RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN IF NEW.version=44 THEN RAISE EXCEPTION 'injected runtime ledger failure'; END IF; RETURN NEW; END; $$; CREATE TRIGGER runtime_ledger_failure BEFORE INSERT ON schema_migrations FOR EACH ROW EXECUTE FUNCTION runtime_ledger_failure()")
            (execute env "DROP TRIGGER runtime_ledger_failure ON schema_migrations; DROP FUNCTION runtime_ledger_failure()") $ do
              refused <- Migration.runMigrations pool upgradeDir
              refused.failed `shouldSatisfy` (/=Nothing)
              boolean env "SELECT NOT EXISTS(SELECT 1 FROM information_schema.columns WHERE table_schema='public' AND table_name='sessions' AND column_name='runtime_identity') AND NOT EXISTS(SELECT 1 FROM schema_migrations WHERE version=44)" `shouldReturn` True
          upgraded <- Migration.runMigrations pool upgradeDir
          upgraded.failed `shouldBe` Nothing
          upgraded.applied `shouldBe` [filename]
          boolean env "SELECT NOT EXISTS(SELECT 1 FROM sessions s JOIN runtime_legacy_expected e USING(id) WHERE (to_jsonb(s)-ARRAY['runtime_identity','runtime_revision']) IS DISTINCT FROM e.value OR s.runtime_identity IS NOT NULL OR s.runtime_revision<>0) AND (SELECT count(*) FROM audit_log)=(SELECT n FROM runtime_legacy_audit_expected)" `shouldReturn` True
          -- Continue the lifecycle against the current prospective contracts;
          -- the preservation assertion above still isolates the V044 upgrade.
          Migration.runMigrations pool source >>= (\result -> result.failed `shouldBe` Nothing)
          Just original <- getSession pool workspace.id activeId
          let declaration=RuntimeIdentity "Known actual runtime" Nothing Nothing (Just "developer")
          resumed <- withEffectiveActorContext (Just actor) $ createSession pool (CreateSession activeId workspace.id "jsonl" "Legacy runtime capture" Nothing (Just declaration) workspace.id)
          resumed `shouldBe` original
          resumed.runtimeIdentity `shouldBe` Nothing
          boolean env "SELECT (SELECT count(*) FROM audit_log)=(SELECT n FROM runtime_legacy_audit_expected)" `shouldReturn` True
          updated <- setRuntimeIdentity pool workspace.id activeId (SetRuntimeIdentity 0 (Just declaration))
          updated.lastSeenAt `shouldBe` original.lastSeenAt
          updated.revision `shouldBe` original.revision
          forM_ [activeId,sealedId] $ \identifier -> do
            range <- readSessionRange pool workspace.id identifier 0 maxSessionRangeBytes
            range.bytes `shouldBe` body
          Just sealed <- getSession pool workspace.id sealedId
          sealed.runtimeIdentity `shouldBe` Nothing
          sealed.finalSha256 `shouldBe` Just (sessionSha256 body)
          expectCode SessionTerminated $ setRuntimeIdentity pool workspace.id sealedId (SetRuntimeIdentity 0 (Just declaration))
          repeated <- Migration.runMigrations pool upgradeDir
          repeated.applied `shouldBe` []

    it "requires short titles and preserves a same-workspace immutable session tree, including raw SQL guards" $ \env -> do
      (workspace,parent) <- fresh env
      initial <- makeTask env workspace "Session tree task"
      childId <- UUID.nextRandom
      child <- createSession env.pool (CreateSession childId workspace "jsonl" "Repair session filters" (Just parent.id) Nothing initial.id)
      child.title `shouldBe` Just "Repair session filters"
      child.parentSessionId `shouldBe` Just parent.id
      Just viewed <- getSession env.pool workspace child.id
      map (.id) viewed.ancestors `shouldBe` [parent.id]
      rejectDb $ execute env ("UPDATE sessions SET parent_session_id=NULL WHERE id='"<>uuid child.id<>"'")
      rejectDb $ execute env ("UPDATE sessions SET title='Changed title' WHERE id='"<>uuid child.id<>"'")
      another <- UUID.nextRandom
      rejectDb $ createSession env.pool (CreateSession another workspace "jsonl" "Self parent test" (Just another) Nothing initial.id)
      other <- createTestWorkspace env "foreign-session-parent"
      otherTask <- makeTask env other.id "Other session tree task"
      rejectDb $ createSession env.pool (CreateSession another other.id "jsonl" "Foreign parent test" (Just parent.id) Nothing otherTask.id)
      missing <- UUID.nextRandom
      rejectDb $ createSession env.pool (CreateSession another workspace "jsonl" "Missing parent test" (Just missing) Nothing initial.id)
      forM_ ["NULL","''","E'\\t'","E'\\n'","U&'\\00a0'"] $ \title -> do
        identifier <- UUID.nextRandom
        rejectDb $ execute env ("INSERT INTO sessions(id,workspace_id,format_label,title) VALUES ('"<>uuid identifier<>"','"<>uuid workspace<>"','jsonl',"<>title<>")")
      expectCode SessionInvalidInput $ createSession env.pool (CreateSession another workspace "jsonl" (T.replicate 27 "雪") Nothing Nothing initial.id)
      rejectDb $ execute env ("INSERT INTO sessions(id,workspace_id,format_label,title,parent_session_id) VALUES ('"<>uuid another<>"','"<>uuid workspace<>"','jsonl','Cycle first','"<>uuid missing<>"'),('"<>uuid missing<>"','"<>uuid workspace<>"','jsonl','Cycle second','"<>uuid another<>"')")
      deep <- foldM (\previous _ -> do identifier <- UUID.nextRandom; createSession env.pool (CreateSession identifier workspace "jsonl" "Nested session work" (Just previous.id) Nothing initial.id)) child [1..20::Int]
      Just context <- getSession env.pool workspace deep.id
      length context.ancestors `shouldBe` 16
      context.ancestorsTruncated `shouldBe` True

    it "only adds new links to in_progress tasks while preserving completed link retries and serializing status races" $ \env -> do
      (workspace,row) <- fresh env
      task <- makeTask env workspace "Linked running task"
      associateSessionTask env.pool workspace row.id task.id
      execute env ("UPDATE tasks SET status='done' WHERE id='"<>uuid task.id<>"'")
      associateSessionTask env.pool workspace row.id task.id
      execute env ("INSERT INTO task_sessions(workspace_id,session_id,task_id) VALUES ('"<>uuid workspace<>"','"<>uuid row.id<>"','"<>uuid task.id<>"')")
      page <- listSessionTasks env.pool workspace row.id Nothing 50
      map (.id) page.items `shouldBe` [task.id]
      forM_ ["todo","blocked","done","cancelled"] $ \status -> do
        target <- Task.createTask env.pool (Types.CreateTask workspace Nothing Nothing "Not running" Nothing Nothing Nothing)
        execute env ("UPDATE tasks SET status='"<>TE.encodeUtf8 status<>"' WHERE id='"<>uuid target.id<>"'")
        expectCode (SessionTaskNotInProgress status) $ associateSessionTask env.pool workspace row.id target.id
        refusal <- try @DBException $ execute env ("INSERT INTO task_sessions(workspace_id,session_id,task_id) VALUES ('"<>uuid workspace<>"','"<>uuid row.id<>"','"<>uuid target.id<>"')")
        case refusal of
          Left (DBCheckViolation message) -> message `shouldSatisfy` T.isInfixOf status
          _ -> expectationFailure "Raw association did not explain the required task status"
      racing <- makeTask env workspace "Racing task status"
      bracket (createPool env.testDb.testDbConnStr 2 30 30000) destroyAllResources $ \peer ->
        withConn env.pool $ \connection -> do
          let sql command = S.run (S.sql command) connection >>= either (fail . show) pure
              probe = env {pool=peer}
              blocked 0 = pure False
              blocked attempts = do
                waiting <- boolean probe "SELECT EXISTS(SELECT 1 FROM pg_stat_activity WHERE datname=current_database() AND pid<>pg_backend_pid() AND wait_event_type='Lock' AND query LIKE 'SELECT t.status::text,EXISTS%')"
                if waiting then pure True else threadDelay 10000 >> blocked (attempts-1)
          sql ("BEGIN; UPDATE tasks SET status='done' WHERE id='"<>uuid racing.id<>"'")
          withAsync (try @SessionException $ associateSessionTask env.pool workspace row.id racing.id) $ \worker ->
            (do
              blocked (500::Int) `shouldReturn` True
              sql "COMMIT"
              result <- waitCatch worker
              case result of
                Right (Left e) -> e.code `shouldBe` SessionTaskNotInProgress "done"
                _ -> expectationFailure "Association did not wait for and observe the committed done status"
              boolean probe ("SELECT NOT EXISTS(SELECT 1 FROM task_sessions WHERE session_id='"<>uuid row.id<>"' AND task_id='"<>uuid racing.id<>"')") `shouldReturn` True
            ) `finally` sql "ROLLBACK"

    it "pages complete active facets and intersects project/task scopes through unloaded and null-project descendants" $ \env -> do
      (workspace,row) <- fresh env
      root <- Project.createProject env.pool (Types.CreateProject workspace Nothing "Root scope" Nothing Nothing Nothing)
      leaf <- Project.createProject env.pool (Types.CreateProject workspace (Just root.id) "Deep scope" Nothing Nothing Nothing)
      other <- Project.createProject env.pool (Types.CreateProject workspace Nothing "Other scope" Nothing Nothing Nothing)
      tasks <- replicateM 51 $ do
        task <- makeTask env workspace "Active facet task"
        execute env ("UPDATE tasks SET project_id='"<>uuid leaf.id<>"' WHERE id='"<>uuid task.id<>"'")
        associateSessionTask env.pool workspace row.id task.id
        pure task
      first <- listSessionFacets env.pool workspace False (Just root.id) Nothing (Just (last tasks).id) 50
      length first.items `shouldBe` 50
      fmap (.id) first.selected `shouldBe` Just (last tasks).id
      second <- listSessionFacets env.pool workspace False (Just root.id) first.nextId Nothing 50
      length second.items `shouldBe` 1
      second.nextId `shouldBe` Nothing
      foreignScope <- makeTask env workspace "Other linked task"
      execute env ("UPDATE tasks SET project_id='"<>uuid other.id<>"' WHERE id='"<>uuid foreignScope.id<>"'")
      associateSessionTask env.pool workspace row.id foreignScope.id
      mismatched <- listWorkspaceSessionsFiltered env.pool workspace (SessionFilter (Just Active) Nothing (Just root.id) (Just foreignScope.id)) Nothing 50
      mismatched.items `shouldBe` []
      child <- Task.createTask env.pool (Types.CreateTask workspace (Just leaf.id) (Just (head tasks).id) "Null project active child" Nothing Nothing Nothing)
      execute env ("UPDATE tasks SET status='in_progress',project_id=NULL WHERE id='"<>uuid child.id<>"'")
      associateSessionTask env.pool workspace row.id child.id
      filtered <- listWorkspaceSessionsFiltered env.pool workspace (SessionFilter (Just Active) Nothing (Just root.id) (Just child.id)) Nothing 50
      map (.id) filtered.items `shouldBe` [row.id]
      let selector = Types.NavigationFilter (Just "tasks") [] [] Nothing Nothing (Just "Null project") (Just True) (Just True)
      visible <- Project.listFilteredProjectChildren env.pool workspace Nothing selector 50 0
      map (.id) visible `shouldBe` [root.id]
      matching <- Task.listFilteredTaskChildren env.pool workspace (Just leaf.id) Nothing selector 50 0
      map (.id) matching `shouldBe` [(head tasks).id]
      _ <- terminateSession env.pool workspace row.id (receipt row "")
      empty <- listSessionFacets env.pool workspace True Nothing Nothing (Just root.id) 50
      empty.items `shouldBe` []
      empty.selected `shouldBe` Nothing

    it "creates an empty active session with server attribution and idempotent immutable metadata" $ \env -> do
      workspace <- createTestWorkspace env "session-create"
      initial <- makeTask env workspace.id "Initial session task"
      identifier <- UUID.nextRandom
      identity <- either (fail . T.unpack) pure (mkBotIdentity "developer" "codex:model:high")
      let actor = EffectiveActor ActorBot "credential-bot" "Developer" (Just identity)
          input = CreateSession identifier workspace.id "provider-neutral-jsonl" "Test session work" Nothing Nothing initial.id
      row <- withEffectiveActorContext (Just actor) $ createSession env.pool input
      row.creatorActorId `shouldBe` Just "credential-bot"
      row.creatorBotIdentity `shouldBe` Just identity
      row.state `shouldBe` Active
      row.committedBytes `shouldBe` 0
      row.committedRecords `shouldBe` 0
      row.revision `shouldBe` 0
      withEffectiveActorContext (Just actor) (createSession env.pool input) `shouldReturn` row
      expectCode SessionConflict $ createSession env.pool input
      expectCode SessionConflict $ withEffectiveActorContext (Just actor) $ createSession env.pool input {formatLabel="other"}
      other <- createTestWorkspace env "session-create-other"
      otherTask <- makeTask env other.id "Other creation task"
      expectCode SessionConflict $ createSession env.pool input {workspaceId=other.id,taskId=otherTask.id}
      getSession env.pool other.id identifier `shouldReturn` Nothing

    it "preserves UTF-8, CRLF, arbitrary JSON values, escaped NUL and large JSON numbers byte for byte" $ \env -> do
      (workspace,row) <- fresh env
      let body = TE.encodeUtf8 "{\"message\":\"æ雪\"}\r\n[true,null,1]\n\"\\u0000\"\n1e999999\n"
      result <- append env workspace row body False
      result.session.committedBytes `shouldBe` fromIntegral (BS.length body)
      result.session.committedRecords `shouldBe` 4
      ranges <- mapM (\offset -> readSessionRange env.pool workspace row.id offset 3) [0,3..fromIntegral (BS.length body)-1]
      BS.concat (map (.bytes) ranges) `shouldBe` body
      let current = result.session
      sealed <- terminateSession env.pool workspace row.id (receipt current body)
      sealed.state `shouldBe` Terminated
      sealed.finalSha256 `shouldBe` Just (sessionSha256 body)
      downloaded <- readSessionRange env.pool workspace row.id 0 maxSessionRangeBytes
      downloaded.bytes `shouldBe` body
      downloaded.hasMore `shouldBe` False

    it "streams a multichunk transcript hash and reads bounded windows across chunk boundaries" $ \env -> do
      (workspace,row) <- fresh env
      let chunk = "\"" <> BS.replicate (maxSessionChunkBytes-3) 97 <> "\"\n"
          body = chunk <> chunk <> "true\n"
      first <- append env workspace row chunk False
      second <- append env workspace first.session chunk False
      third <- append env workspace second.session "true\n" False
      window <- readSessionRange env.pool workspace row.id (fromIntegral maxSessionChunkBytes-2) 5
      window.bytes `shouldBe` BS.take 5 (BS.drop (maxSessionChunkBytes-2) body)
      window.hasMore `shouldBe` True
      window.nextOffset `shouldBe` fromIntegral maxSessionChunkBytes+3
      sealed <- terminateSession env.pool workspace row.id (receipt third.session body)
      sealed.finalSha256 `shouldBe` Just (sessionSha256 body)
      lastWindow <- readSessionRange env.pool workspace row.id (fromIntegral (2*maxSessionChunkBytes)) maxSessionRangeBytes
      lastWindow.bytes `shouldBe` "true\n"
      lastWindow.hasMore `shouldBe` False

    it "keeps a flushed newline-free record active and requires a real separator before any later bytes" $ \env -> do
      (workspace,row) <- fresh env
      first <- append env workspace row "{\"a\":1}" True
      first.session.state `shouldBe` Active
      first.session.committedRecords `shouldBe` 1
      forM_ [" ","true\n","\rtrue\n","\t\n"] $ \bytes -> do
        rejectDb $ append env workspace first.session bytes True
        getSession env.pool workspace row.id `shouldReturn` Just first.session
      separator <- append env workspace first.session "\r\n" False
      separator.session.committedRecords `shouldBe` 1
      second <- append env workspace separator.session "true\n" False
      second.session.committedRecords `shouldBe` 2
      allBytes env workspace row.id `shouldReturn` "{\"a\":1}\r\ntrue\n"

    it "also accepts a separator and new complete record in one chunk" $ \env -> do
      (workspace,row) <- fresh env
      first <- append env workspace row "1" True
      second <- append env workspace first.session "\n2\r\n3" True
      second.session.committedRecords `shouldBe` 3
      second.session.state `shouldBe` Active
      allBytes env workspace row.id `shouldReturn` "1\n2\r\n3"

    it "rejects invalid UTF-8, empty/whitespace records, malformed JSON and pending tails atomically" $ \env -> do
      (workspace,row) <- fresh env
      forM_ [("{}",False),("{\n",True),("{}\n{",True),("\n",True),(" \r\n",False)
            ,("{}\n\n",False),(BS.pack [255,10],False),("true false\n",False),("{}\n  ",True)] $ \(body,flush) -> do
        rejectDb $ append env workspace row body flush
        getSession env.pool workspace row.id `shouldReturn` Just row
        allBytes env workspace row.id `shouldReturn` ""
      expectCode SessionInvalidInput $ append env workspace row "" False
      expectCode SessionInvalidInput $ append env workspace row (BS.replicate (maxSessionChunkBytes+1) 32) False
      let large = "\"" <> BS.replicate (maxSessionChunkBytes-3) 97 <> "\"\n"
      accepted <- append env workspace row large False
      accepted.session.committedRecords `shouldBe` 1

    it "acknowledges exact replay at old offsets, rejects gaps/overlap, and never mutates on replay" $ \env -> do
      (workspace,row) <- fresh env
      first <- append env workspace row "1\n" False
      second <- append env workspace first.session "2\n" False
      replay <- append env workspace row "1\n" False
      replay.replayed `shouldBe` True
      replay.session `shouldBe` second.session
      expectCode SessionConflict $ append env workspace row "3\n" False
      expectCode SessionConflict $ appendSession env.pool workspace row.id (AppendSession 100 2 "3\n" False)
      expectCode SessionConflict $ appendSession env.pool workspace row.id (AppendSession 1 0 "3\n" False)
      expectCode SessionConflict $ append env workspace row "1\n" True
      sealed <- terminateSession env.pool workspace row.id (receipt second.session "1\n2\n")
      replaySealed <- append env workspace row "1\n" False
      replaySealed `shouldBe` SessionAppendResult sealed True

    it "seals only the committed prefix, validates the receipt, and retries the exact seal" $ \env -> do
      (workspace,row) <- fresh env
      uploaded <- append env workspace row "{}" True
      let current = uploaded.session
          exact = receipt current "{}"
      expectCode SessionConflict $ terminateSession env.pool workspace row.id exact {expectedOffset=0}
      expectCode SessionConflict $ terminateSession env.pool workspace row.id exact {expectedRevision=0}
      expectCode SessionConflict $ terminateSession env.pool workspace row.id exact {finalBytes=99}
      expectCode SessionConflict $ terminateSession env.pool workspace row.id exact {finalSha256=T.replicate 64 "0"}
      getSession env.pool workspace row.id `shouldReturn` Just current
      sealed <- terminateSession env.pool workspace row.id exact
      sealed.committedBytes `shouldBe` current.committedBytes
      sealed.revision `shouldBe` current.revision+1
      terminateSession env.pool workspace row.id exact `shouldReturn` sealed
      expectCode SessionConflict $ terminateSession env.pool workspace row.id exact {expectedRevision=sealed.revision}
      expectCode SessionTerminated $ heartbeatSession env.pool workspace row.id
      expectCode SessionTerminated $ append env workspace sealed "\ntrue\n" False

    it "can seal an empty session with no uploader and leaves task status unchanged" $ \env -> do
      (workspace,row) <- fresh env
      task <- makeTask env workspace "still todo"
      associateSessionTask env.pool workspace row.id task.id
      seen <- heartbeatSession env.pool workspace row.id
      seen.state `shouldBe` Active
      seen.revision `shouldBe` 0
      sealed <- terminateSession env.pool workspace row.id (receipt seen "")
      sealed.finalBytes `shouldBe` Just 0
      Just after <- Task.getTask env.pool task.id
      after.status `shouldBe` Types.InProgress

    it "seals a stale active prefix without requiring a heartbeat or uploader" $ \env -> do
      workspace <- createTestWorkspace env "stale-session"
      identifier <- UUID.nextRandom
      insertRaw env workspace.id [identifier] ("INSERT INTO sessions(id,workspace_id,format_label,title,created_at,last_seen_at) VALUES ('"
        <> uuid identifier <> "','" <> uuid workspace.id <> "','jsonl','Test session work',clock_timestamp()-interval '1 hour',clock_timestamp()-interval '1 hour')")
      Just stale <- getSession env.pool workspace.id identifier
      stale.state `shouldBe` Active
      sealed <- terminateSession env.pool workspace.id identifier (receipt stale "")
      sealed.lastSeenAt `shouldBe` stale.lastSeenAt
      sealed.state `shouldBe` Terminated

    it "omits heartbeat-only audit entries while preserving live events and meaningful session changes" $ \env -> do
      (workspace,row) <- fresh env
      let audits count = boolean env ("SELECT count(*)="<>count<>" FROM audit_log WHERE entity_type='agent_session' AND entity_id='"<>uuid row.id<>"'") `shouldReturn` True
          events count = boolean env ("SELECT count(*)="<>count<>" FROM change_stream_outbox WHERE scope='workspace' AND envelope#>>'{entity,type}'='agent_session' AND envelope#>>'{entity,id}'='"<>uuid row.id<>"'") `shouldReturn` True
      audits "1"
      events "1"
      seen <- heartbeatSession env.pool workspace row.id
      seen.lastSeenAt `shouldSatisfy` (>row.lastSeenAt)
      audits "1"
      events "2"
      boolean env ("SELECT NOT EXISTS(SELECT 1 FROM change_stream_outbox e CROSS JOIN LATERAL jsonb_array_elements(e.envelope->'invalidations') target WHERE e.envelope#>>'{entity,id}'='"<>uuid row.id<>"' AND e.envelope#>>'{entity,action}'='updated' AND target->>'target' LIKE 'active_sessions:%')") `shouldReturn` True
      uploaded <- append env workspace seen "{}\n" False
      audits "2"
      events "3"
      _ <- setRuntimeIdentity env.pool workspace row.id (SetRuntimeIdentity 0 (Just (RuntimeIdentity "audit-model" Nothing Nothing (Just "developer"))))
      audits "3"
      events "4"
      sealed <- terminateSession env.pool workspace row.id (receipt uploaded.session "{}\n")
      sealed.state `shouldBe` Terminated
      audits "4"
      events "5"
      boolean env ("SELECT count(*)=1 FROM audit_log WHERE entity_type='agent_session' AND entity_id='"<>uuid row.id<>"' AND old_values->>'state'='active' AND new_values->>'state'='terminated'") `shouldReturn` True

    it "does not store raw bytes in audit or change events" $ \env -> do
      (workspace,row) <- fresh env
      _ <- append env workspace row "{\"secret\":\"session-only-needle\"}\n" False
      boolean env "SELECT NOT EXISTS (SELECT 1 FROM audit_log WHERE old_values::text LIKE '%session-only-needle%' OR new_values::text LIKE '%session-only-needle%') AND NOT EXISTS (SELECT 1 FROM change_stream_outbox WHERE envelope::text LIKE '%session-only-needle%')" `shouldReturn` True

    it "guards raw SQL chunk/order/counters, active identity, sealed metadata and additive task links" $ \env -> do
      (workspace,row) <- fresh env
      task <- makeTask env workspace "guarded"
      associateSessionTask env.pool workspace row.id task.id
      forM_ ["UPDATE sessions SET format_label='changed'","UPDATE sessions SET committed_bytes=5"
            ,"INSERT INTO session_chunks(session_id,start_offset,revision,data,allow_final_record,record_count,ends_with_newline) SELECT id,1,1,convert_to('1','UTF8'),true,1,false FROM sessions"
            ,"INSERT INTO session_chunks(session_id,start_offset,revision,data,allow_final_record,record_count,ends_with_newline) SELECT id,0,1,convert_to('{','UTF8'),true,1,false FROM sessions"
            ,"DELETE FROM task_sessions","UPDATE task_sessions SET created_at=clock_timestamp()"] $ \sql -> rejectDb (execute env sql)
      first <- append env workspace row "1\n" False
      forM_ ["UPDATE session_chunks SET data=convert_to('2','UTF8')","DELETE FROM session_chunks"] $ \sql -> rejectDb (execute env sql)
      sealed <- terminateSession env.pool workspace row.id (receipt first.session "1\n")
      forM_ ["UPDATE sessions SET state='active',terminated_at=NULL,final_bytes=NULL,final_sha256=NULL"
            ,"UPDATE sessions SET last_seen_at=clock_timestamp()","UPDATE sessions SET final_sha256=repeat('0',64)"
            ,"UPDATE sessions SET revision=revision+1","UPDATE sessions SET format_label='changed'","DELETE FROM sessions"
            ,"INSERT INTO session_chunks(session_id,start_offset,revision,data,allow_final_record,record_count,ends_with_newline) SELECT id,committed_bytes,revision+1,convert_to('2','UTF8'),true,1,false FROM sessions"] $ \sql -> rejectDb (execute env sql)
      getSession env.pool workspace row.id `shouldReturn` Just sealed
      allBytes env workspace row.id `shouldReturn` "1\n"
      forM_ ["TRUNCATE session_chunks","TRUNCATE task_sessions","TRUNCATE sessions CASCADE"
            ,"TRUNCATE tasks CASCADE","TRUNCATE workspaces CASCADE"] $ \sql -> do
        rejectDb (execute env sql)
        getSession env.pool workspace row.id `shouldReturn` Just sealed
        allBytes env workspace row.id `shouldReturn` "1\n"

    it "restores exactly the evidence truncate guards after successful and failed test cleanup" $ \env -> do
      (workspace,row) <- fresh env
      task <- makeTask env workspace "cleanup-retained"
      associateSessionTask env.pool workspace row.id task.id
      first <- append env workspace row "1\n" False
      sealed <- terminateSession env.pool workspace row.id (receipt first.session "1\n")
      bracket_
        (execute env "CREATE FUNCTION session_cleanup_failure() RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN RAISE EXCEPTION 'injected cleanup failure'; END; $$; CREATE TRIGGER zz_session_cleanup_failure BEFORE TRUNCATE ON workspaces EXECUTE FUNCTION session_cleanup_failure()")
        (execute env "DROP TRIGGER zz_session_cleanup_failure ON workspaces; DROP FUNCTION session_cleanup_failure()") $ do
          result <- try @SomeException (cleanDB env)
          result `shouldSatisfy` either (const True) (const False)
          getSession env.pool workspace row.id `shouldReturn` Just sealed
          allBytes env workspace row.id `shouldReturn` "1\n"
          assertTruncateGuards env
      cleanDB env
      getSession env.pool workspace row.id `shouldReturn` Nothing
      assertTruncateGuards env

    it "rolls back suspended truncate guards and sealed evidence when test cleanup is cancelled" $ \env -> do
      (workspace,row) <- fresh env
      first <- append env workspace row "1\n" False
      sealed <- terminateSession env.pool workspace row.id (receipt first.session "1\n")
      bracket_
        (execute env "CREATE FUNCTION session_cleanup_wait() RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN PERFORM pg_sleep(5); RETURN NULL; END; $$; CREATE TRIGGER zz_session_cleanup_wait BEFORE TRUNCATE ON workspaces EXECUTE FUNCTION session_cleanup_wait()")
        (execute env "DROP TRIGGER zz_session_cleanup_wait ON workspaces; DROP FUNCTION session_cleanup_wait()") $
          withAsync (cleanDB env) $ \cleanup -> do
            awaitCleanupSleep env 200
            cancel cleanup
            waitCatch cleanup >>= (`shouldSatisfy` either (const True) (const False))
            getSession env.pool workspace row.id `shouldReturn` Just sealed
            allBytes env workspace row.id `shouldReturn` "1\n"
            assertTruncateGuards env

    it "keeps an enclosing fixture transaction open while cleaning under a savepoint" $ \env -> do
      (workspace,row) <- fresh env
      sealed <- terminateSession env.pool workspace row.id (receipt row "")
      bracket (createPool env.testDb.testDbConnStr 1 30 30000) destroyAllResources $ \pool ->
        withTestTransaction (\transactionEnv -> do
          cleanDB transactionEnv
          getSession pool workspace row.id `shouldReturn` Nothing
          boolean transactionEnv "SELECT count(*)=3 AND bool_and(tgenabled='O') FROM pg_trigger WHERE tgname='session_evidence_no_truncate' AND tgrelid IN ('public.sessions'::regclass,'public.session_chunks'::regclass,'public.task_sessions'::regclass)" `shouldReturn` True) env {pool=pool}
      -- The enclosing fixture rollback must restore the earlier sealed row.
      getSession env.pool workspace row.id `shouldReturn` Just sealed
      assertTruncateGuards env

    it "links idempotently before/after sealing, rejects other workspaces, hides soft-deleted tasks and restores links" $ \env -> do
      (workspace,row) <- fresh env
      task <- makeTask env workspace "linked"
      otherWorkspace <- createTestWorkspace env "other"
      other <- makeTask env otherWorkspace.id "other"
      associateSessionTask env.pool workspace row.id task.id
      associateSessionTask env.pool workspace row.id task.id
      expectCode SessionNotFound $ associateSessionTask env.pool workspace row.id other.id
      rejectDb $ execute env ("INSERT INTO task_sessions(workspace_id,task_id,session_id) VALUES ('" <> uuid otherWorkspace.id <> "','" <> uuid other.id <> "','" <> uuid row.id <> "')")
      linked <- listSessionTasks env.pool workspace row.id Nothing 100
      map (.id) linked.items `shouldBe` [task.id]
      _ <- terminateSession env.pool workspace row.id (receipt row "")
      later <- makeTask env workspace "later"
      associateSessionTask env.pool workspace row.id later.id
      execute env ("UPDATE tasks SET deleted_at=clock_timestamp() WHERE id='" <> uuid task.id <> "'")
      hidden <- listTaskSessions env.pool workspace task.id Nothing 100
      hidden.items `shouldBe` []
      visible <- listSessionTasks env.pool workspace row.id Nothing 100
      map (.id) visible.items `shouldBe` [later.id]
      execute env ("UPDATE tasks SET deleted_at=NULL WHERE id='" <> uuid task.id <> "'")
      restored <- listTaskSessions env.pool workspace task.id Nothing 100
      map (.id) restored.items `shouldBe` [row.id]

    it "uses bounded keyset lists and strict range limits with workspace isolation" $ \env -> do
      (workspace,row) <- fresh env
      _ <- freshIn env workspace
      _ <- freshIn env workspace
      first <- listWorkspaceSessions env.pool workspace Nothing 1
      length first.items `shouldBe` 1
      second <- listWorkspaceSessions env.pool workspace first.nextCursor 1
      length second.items `shouldBe` 1
      map (.id) second.items `shouldNotBe` map (.id) first.items
      _ <- freshIn env workspace
      third <- listWorkspaceSessions env.pool workspace second.nextCursor 1
      map (.id) third.items `shouldBe` [row.id]
      third.nextCursor `shouldBe` Nothing
      forM_ [0,101] $ \limit -> expectCode SessionInvalidInput $ listWorkspaceSessions env.pool workspace Nothing limit
      expectCode SessionInvalidInput $ readSessionRange env.pool workspace row.id 0 (maxSessionRangeBytes+1)
      expectCode SessionConflict $ readSessionRange env.pool workspace row.id 1 1
      other <- createTestWorkspace env "range-isolation"
      expectCode SessionNotFound $ readSessionRange env.pool other.id row.id 0 1
      listWorkspaceSessions env.pool other.id Nothing 1 `shouldReturn` SessionPage [] Nothing

    it "filters lifecycle and heartbeat freshness before keyset pagination independently" $ \env -> do
      (workspace,current) <- fresh env
      old <- UUID.nextRandom
      older <- UUID.nextRandom
      insertRaw env workspace [old,older] ("INSERT INTO sessions(id,workspace_id,format_label,title,created_at,last_seen_at) VALUES ('" <> uuid old <> "','" <> uuid workspace <> "','jsonl','Test session work',clock_timestamp()-interval '60 seconds',clock_timestamp()-interval '60 seconds'),('" <> uuid older <> "','" <> uuid workspace <> "','jsonl','Test session work',clock_timestamp()-interval '90 seconds',clock_timestamp()-interval '90 seconds')")
      oldRow <- getSession env.pool workspace old >>= maybe (fail "Missing seeded session") pure
      sealed <- terminateSession env.pool workspace old (receipt oldRow "")
      let stale = SessionFilter Nothing (Just Stale) Nothing Nothing
          freshOnly = SessionFilter Nothing (Just Fresh) Nothing Nothing
      first <- listWorkspaceSessionsFiltered env.pool workspace stale Nothing 1
      map (.id) first.items `shouldBe` [old]
      second <- listWorkspaceSessionsFiltered env.pool workspace stale first.nextCursor 1
      map (.id) second.items `shouldBe` [older]
      second.nextCursor `shouldBe` Nothing
      recent <- listWorkspaceSessionsFiltered env.pool workspace freshOnly Nothing 50
      map (.id) recent.items `shouldBe` [current.id]
      closed <- listWorkspaceSessionsFiltered env.pool workspace (SessionFilter (Just Terminated) (Just Stale) Nothing Nothing) Nothing 50
      closed.items `shouldBe` [sealed]
      active <- listWorkspaceSessionsFiltered env.pool workspace (SessionFilter (Just Active) (Just Stale) Nothing Nothing) Nothing 50
      map (.id) active.items `shouldBe` [older]
      task <- makeTask env workspace "filtered evidence"
      forM_ [current.id,old,older] $ \id -> associateSessionTask env.pool workspace id task.id
      linked <- listTaskSessionsFiltered env.pool workspace task.id (SessionFilter (Just Terminated) Nothing Nothing Nothing) Nothing 1
      linked.items `shouldBe` [sealed]

    it "serializes concurrent appends and append/termination races without losing a committed prefix" $ \env -> do
      (workspace,row) <- fresh env
      (one,two) <- concurrently
        (try @SessionException $ append env workspace row "1\n" False)
        (try @SessionException $ append env workspace row "2\n" False)
      length [() | Right _ <- [one,two]] `shouldBe` 1
      length [() | Left e <- [one,two], e.code == SessionConflict] `shouldBe` 1
      Just current <- getSession env.pool workspace row.id
      body <- allBytes env workspace row.id
      (appended,sealed) <- concurrently
        (try @SessionException $ append env workspace current "3\n" False)
        (try @SessionException $ terminateSession env.pool workspace row.id (receipt current body))
      case (appended,sealed) of
        (Right _,Left e) -> e.code `shouldBe` SessionConflict
        (Left e,Right _) -> e.code `shouldBe` SessionTerminated
        _ -> expectationFailure "Exactly one racing mutation must succeed"
      Just after <- getSession env.pool workspace row.id
      after.committedBytes `shouldBe` if after.state == Terminated then current.committedBytes else current.committedBytes+2

    it "rolls back a late chunk failure including all parent counters" $ \env -> do
      (workspace,row) <- fresh env
      bracket_
        (execute env "CREATE FUNCTION session_test_failure() RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN RAISE EXCEPTION USING ERRCODE='23514', MESSAGE='injected late failure'; END; $$; CREATE TRIGGER zz_session_test_failure AFTER INSERT ON session_chunks FOR EACH ROW EXECUTE FUNCTION session_test_failure()")
        (execute env "DROP TRIGGER zz_session_test_failure ON session_chunks; DROP FUNCTION session_test_failure()") $ do
          rejectDb $ append env workspace row "1\n" False
          getSession env.pool workspace row.id `shouldReturn` Just row
          allBytes env workspace row.id `shouldReturn` ""

  describe "Agent sessions migration" $ do
    it "removes only historical heartbeat audits and installs the audit guard atomically" $
      withTestSandbox $ \sandbox -> withSandboxedEnv sandbox $ withSandboxedPostgres sandbox $ \db ->
        bracket (createPool db.testDbConnStr 2 30 30000) destroyAllResources $ \pool -> do
          source <- resolveMigrationsDir sandbox.sandboxRepoRoot
          let beforeDir = sandbox.sandboxRoot </> "before-v048"
              upgradeDir = sandbox.sandboxRoot </> "only-v048"
              filename = "V048__session_timestamp_audit_noise.sql"
              env = TestEnv pool sandbox db
          createDirectory beforeDir
          createDirectory upgradeDir
          files <- listDirectory source
          forM_ (filter (\name -> name < "V048" && ".sql" `T.isSuffixOf` T.pack name) files) $ \name -> copyFile (source </> name) (beforeDir </> name)
          copyFile (source </> filename) (upgradeDir </> filename)
          Migration.runMigrations pool beforeDir >>= (\result -> result.failed `shouldBe` Nothing)
          (workspace,row) <- legacyFreshFixture env
          _ <- heartbeatSession pool workspace row.id
          execute env "CREATE TABLE heartbeat_audit_ids AS SELECT id FROM audit_log WHERE entity_type='agent_session' AND action='update'"
          boolean env "SELECT count(*)=1 FROM heartbeat_audit_ids" `shouldReturn` True
          uploaded <- append env workspace row "{}\n" False
          _ <- setRuntimeIdentity pool workspace row.id (SetRuntimeIdentity 0 (Just (RuntimeIdentity "upgrade-model" Nothing Nothing (Just "developer"))))
          sealed <- terminateSession pool workspace row.id (receipt uploaded.session "{}\n")
          -- Minimal legacy snapshots exercise cleanup without newer runtime keys.
          execute env "INSERT INTO audit_log(entity_type,entity_id,action,old_values,new_values,request_id) VALUES ('agent_session','legacy-heartbeat','update','{\"state\":\"active\",\"last_seen_at\":\"old\"}','{\"state\":\"active\",\"last_seen_at\":\"new\"}','remove-heartbeat')"
          let retain entity action old new = execute env ("INSERT INTO audit_log(entity_type,entity_id,action,old_values,new_values,request_id) VALUES ('"<>entity<>"','retained-fixture','"<>action<>"',"<>old<>","<>new<>",'retain-heartbeat-fixture')")
          forM_
            [ ("agent_session","create","NULL","'{\"last_seen_at\":\"new\"}'")
            , ("task","update","'{\"last_seen_at\":\"old\"}'","'{\"last_seen_at\":\"new\"}'")
            , ("agent_session","update","NULL","'{\"last_seen_at\":\"new\"}'")
            , ("agent_session","update","'[]'","'[\"last_seen_at\"]'")
            , ("agent_session","update","'1'","'2'")
            , ("agent_session","update","'\"old\"'","'\"new\"'")
            , ("agent_session","update","'null'","'true'")
            , ("agent_session","update","'{}'","'{\"last_seen_at\":\"new\"}'")
            , ("agent_session","update","'{\"last_seen_at\":null}'","'{\"last_seen_at\":\"new\"}'")
            , ("agent_session","update","'{\"last_seen_at\":\"same\"}'","'{\"last_seen_at\":\"same\"}'")
            , ("agent_session","update","'{\"last_seen_at\":\"old\",\"future_field\":1}'","'{\"last_seen_at\":\"new\",\"future_field\":2}'")
            ] $ \(entity,action,old,new) -> retain entity action old new
          execute env "CREATE TABLE pre_v048_audits AS TABLE audit_log; CREATE TABLE pre_v048_events AS TABLE change_stream_outbox; CREATE TABLE pre_v048_sessions AS SELECT to_jsonb(s) AS value FROM sessions s; CREATE TABLE pre_v048_function AS SELECT pg_get_functiondef('hmem_agent_session_change()'::regprocedure) AS value; CREATE TABLE expected_v048_audits AS SELECT * FROM audit_log WHERE id NOT IN (SELECT id FROM heartbeat_audit_ids) AND request_id IS DISTINCT FROM 'remove-heartbeat'"
          bracket_
            (execute env "CREATE FUNCTION heartbeat_ledger_failure() RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN IF NEW.version=48 THEN RAISE EXCEPTION 'injected heartbeat ledger failure'; END IF; RETURN NEW; END; $$; CREATE TRIGGER heartbeat_ledger_failure BEFORE INSERT ON schema_migrations FOR EACH ROW EXECUTE FUNCTION heartbeat_ledger_failure()")
            (execute env "DROP TRIGGER heartbeat_ledger_failure ON schema_migrations; DROP FUNCTION heartbeat_ledger_failure()") $ do
              Migration.runMigrations pool upgradeDir >>= (\result -> result.failed `shouldSatisfy` (/=Nothing))
              boolean env "SELECT NOT EXISTS((TABLE audit_log EXCEPT TABLE pre_v048_audits) UNION ALL (TABLE pre_v048_audits EXCEPT TABLE audit_log)) AND NOT EXISTS(SELECT 1 FROM schema_migrations WHERE version=48)" `shouldReturn` True
              boolean env "SELECT pg_get_functiondef('hmem_agent_session_change()'::regprocedure)=(SELECT value FROM pre_v048_function)" `shouldReturn` True
          upgraded <- Migration.runMigrations pool upgradeDir
          upgraded.failed `shouldBe` Nothing
          upgraded.applied `shouldBe` [filename]
          boolean env "SELECT (SELECT pg_get_functiondef('hmem_agent_session_change()'::regprocedure))<>(SELECT value FROM pre_v048_function) AND NOT EXISTS((TABLE audit_log EXCEPT TABLE expected_v048_audits) UNION ALL (TABLE expected_v048_audits EXCEPT TABLE audit_log))" `shouldReturn` True
          boolean env "SELECT NOT EXISTS((TABLE change_stream_outbox EXCEPT TABLE pre_v048_events) UNION ALL (TABLE pre_v048_events EXCEPT TABLE change_stream_outbox)) AND NOT EXISTS((SELECT to_jsonb(s) FROM sessions s EXCEPT SELECT value FROM pre_v048_sessions) UNION ALL (SELECT value FROM pre_v048_sessions EXCEPT SELECT to_jsonb(s) FROM sessions s))" `shouldReturn` True
          getSession pool workspace row.id `shouldReturn` Just sealed
          allBytes env workspace row.id `shouldReturn` "{}\n"
          Migration.runMigrations pool upgradeDir >>= (\result -> result.applied `shouldBe` [])
          (activeWorkspace,active) <- legacyFreshFixture env
          _ <- heartbeatSession pool activeWorkspace active.id
          boolean env ("SELECT count(*)=1 FROM audit_log WHERE entity_type='agent_session' AND entity_id='"<>uuid active.id<>"'") `shouldReturn` True

    it "upgrades populated V042 evidence atomically, keeps legacy metadata unchanged and resumes an untitled active session" $
      withTestSandbox $ \sandbox -> withSandboxedEnv sandbox $ withSandboxedPostgres sandbox $ \db ->
        bracket (createPool db.testDbConnStr 2 30 30000) destroyAllResources $ \pool -> do
          source <- resolveMigrationsDir sandbox.sandboxRepoRoot
          let beforeDir=sandbox.sandboxRoot </> "before-v043"
              upgradeDir=sandbox.sandboxRoot </> "only-v043"
              filename="V043__session_hierarchy_and_titles.sql"
              env=TestEnv pool sandbox db
              body="{\"text\":\"legacy evidence\"}\r\n"
          createDirectory beforeDir
          createDirectory upgradeDir
          files <- listDirectory source
          forM_ (filter (\name -> name < "V043" && ".sql" `T.isSuffixOf` T.pack name) files) $ \name -> copyFile (source </> name) (beforeDir </> name)
          copyFile (source </> filename) (upgradeDir </> filename)
          before <- Migration.runMigrations pool beforeDir
          before.failed `shouldBe` Nothing
          workspace <- createTestWorkspace env "legacy-session-upgrade"
          activeId <- UUID.nextRandom
          sealedId <- UUID.nextRandom
          forM_ [activeId,sealedId] $ \identifier -> do
            execute env ("INSERT INTO sessions(id,workspace_id,format_label) VALUES ('"<>uuid identifier<>"','"<>uuid workspace.id<>"','legacy-jsonl')")
            execute env ("INSERT INTO session_chunks(session_id,start_offset,revision,data,allow_final_record,record_count,ends_with_newline) VALUES ('"<>uuid identifier<>"',0,1,convert_to(E'{\"text\":\"legacy evidence\"}\\r\\n','UTF8'),false,0,false)")
          execute env ("UPDATE sessions SET state='terminated',terminated_at=clock_timestamp(),final_bytes=committed_bytes,final_sha256='"<>TE.encodeUtf8 (sessionSha256 body)<>"',revision=revision+1 WHERE id='"<>uuid sealedId<>"'")
          execute env "CREATE TABLE legacy_session_expected AS SELECT id,state,created_at,last_seen_at,committed_bytes,committed_records,revision,terminated_at,final_bytes,final_sha256 FROM sessions; CREATE TABLE legacy_audit_expected AS SELECT count(*) AS n FROM audit_log"
          bracket_
            (execute env "CREATE FUNCTION title_ledger_failure() RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN IF NEW.version=43 THEN RAISE EXCEPTION 'injected title ledger failure'; END IF; RETURN NEW; END; $$; CREATE TRIGGER title_ledger_failure BEFORE INSERT ON schema_migrations FOR EACH ROW EXECUTE FUNCTION title_ledger_failure()")
            (execute env "DROP TRIGGER title_ledger_failure ON schema_migrations; DROP FUNCTION title_ledger_failure()") $ do
              refused <- Migration.runMigrations pool upgradeDir
              refused.failed `shouldSatisfy` (/=Nothing)
              boolean env "SELECT NOT EXISTS(SELECT 1 FROM information_schema.columns WHERE table_schema='public' AND table_name='sessions' AND column_name='title') AND NOT EXISTS(SELECT 1 FROM schema_migrations WHERE version=43)" `shouldReturn` True
          upgraded <- Migration.runMigrations pool upgradeDir
          upgraded.failed `shouldBe` Nothing
          upgraded.applied `shouldBe` [filename]
          boolean env "SELECT NOT EXISTS(SELECT 1 FROM sessions s JOIN legacy_session_expected e USING(id) WHERE (s.state,s.created_at,s.last_seen_at,s.committed_bytes,s.committed_records,s.revision,s.terminated_at,s.final_bytes,s.final_sha256) IS DISTINCT FROM (e.state,e.created_at,e.last_seen_at,e.committed_bytes,e.committed_records,e.revision,e.terminated_at,e.final_bytes,e.final_sha256) OR s.title IS NOT NULL OR s.parent_session_id IS NOT NULL) AND (SELECT count(*) FROM audit_log)=(SELECT n FROM legacy_audit_expected)" `shouldReturn` True
          Just legacy <- getSession pool workspace.id activeId
          resumed <- createSession pool (CreateSession activeId workspace.id "legacy-jsonl" "Resume legacy session work" Nothing Nothing workspace.id)
          resumed `shouldBe` legacy
          resumed.title `shouldBe` Nothing
          sealed <- readSessionRange pool workspace.id sealedId 0 maxSessionRangeBytes
          sealed.bytes `shouldBe` body
          sealed.session.finalSha256 `shouldBe` Just (sessionSha256 body)
          boolean env "SELECT (SELECT count(*) FROM audit_log)=(SELECT n FROM legacy_audit_expected)" `shouldReturn` True
          repeated <- Migration.runMigrations pool upgradeDir
          repeated.applied `shouldBe` []

    it "upgrades populated V040, atomically rolls back a late DDL failure, and reruns idempotently" $
      withTestSandbox $ \sandbox -> withSandboxedEnv sandbox $ withSandboxedPostgres sandbox $ \db ->
        bracket (createPool db.testDbConnStr 2 30 30000) destroyAllResources $ \pool -> do
          source <- resolveMigrationsDir sandbox.sandboxRepoRoot
          let beforeDir = sandbox.sandboxRoot </> "before-v041"
              upgradeDir = sandbox.sandboxRoot </> "only-v041"
              filename = "V041__agent_sessions.sql"
          createDirectory beforeDir
          createDirectory upgradeDir
          files <- listDirectory source
          forM_ (filter (\name -> name < "V041" && ".sql" `T.isSuffixOf` T.pack name) files) $ \name -> copyFile (source </> name) (beforeDir </> name)
          copyFile (source </> filename) (upgradeDir </> filename)
          before <- Migration.runMigrations pool beforeDir
          before.failed `shouldBe` Nothing
          let env = TestEnv pool sandbox db
          workspace <- createTestWorkspace env "session-upgrade"
          task <- makeTask env workspace.id "survives"
          execute env "CREATE TABLE task_sessions(conflict boolean)"
          failed <- Migration.runMigrations pool upgradeDir
          failed.failed `shouldSatisfy` (/=Nothing)
          boolean env "SELECT to_regclass('sessions') IS NULL AND to_regclass('session_chunks') IS NULL AND NOT EXISTS(SELECT 1 FROM schema_migrations WHERE version=41)" `shouldReturn` True
          execute env "DROP TABLE task_sessions"
          upgraded <- Migration.runMigrations pool upgradeDir
          upgraded.failed `shouldBe` Nothing
          upgraded.applied `shouldBe` [filename]
          Just after <- Task.getTask pool task.id
          after `shouldBe` task
          repeated <- Migration.runMigrations pool upgradeDir
          repeated.failed `shouldBe` Nothing
          repeated.applied `shouldBe` []
          _ <- Migration.runMigrations pool source
          row <- freshIn env workspace.id
          associateSessionTask pool workspace.id row.id task.id

reaperFixture :: TestEnv -> IO (UUID,Types.Task,AgentSession)
reaperFixture env = do
  workspace <- createTestWorkspace env "idle-session-reaper"
  task <- makeTask env workspace.id "Session reaper fixture"
  identifier <- UUID.nextRandom
  row <- createSession env.pool (CreateSession identifier workspace.id "jsonl" "Idle task capture" Nothing Nothing task.id)
  pure (workspace.id,task,row)

legacySessionFixture :: TestEnv -> UUID -> UUID -> IO AgentSession
legacySessionFixture env workspace task = do
  identifier <- UUID.nextRandom
  runTransaction env.pool $ S.sql ("INSERT INTO sessions(id,workspace_id,format_label,title) VALUES ('"<>uuid identifier<>"','"<>uuid workspace<>"','jsonl','Historical schema fixture'); INSERT INTO task_sessions(workspace_id,session_id,task_id) VALUES ('"<>uuid workspace<>"','"<>uuid identifier<>"','"<>uuid task<>"')")
  getSession env.pool workspace identifier >>= maybe (fail "Missing historical fixture session") pure

legacyFreshFixture :: TestEnv -> IO (UUID,AgentSession)
legacyFreshFixture env = do
  workspace <- (.id) <$> createTestWorkspace env "historical-schema-session"
  task <- makeTask env workspace "Historical schema task"
  row <- legacySessionFixture env workspace task.id
  execute env ("UPDATE tasks SET deleted_at=clock_timestamp() WHERE id='"<>uuid task.id<>"'")
  pure (workspace,row)

fresh :: TestEnv -> IO (UUID,AgentSession)
fresh env = do
  workspace <- createTestWorkspace env "session-test"
  row <- freshIn env workspace.id
  pure (workspace.id,row)

freshIn :: TestEnv -> UUID -> IO AgentSession
freshIn env workspace = do
  identifier <- UUID.nextRandom
  initial <- makeTask env workspace "Session fixture creation"
  row <- createSession env.pool (CreateSession identifier workspace "jsonl" "Test session work" Nothing Nothing initial.id)
  -- General fixtures have no visible task demand after valid creation. Keep
  -- their historical initial link while exercising ordinary task deletion.
  execute env ("UPDATE tasks SET deleted_at=clock_timestamp() WHERE id='"<>uuid initial.id<>"'")
  pure row

insertRaw :: TestEnv -> UUID -> [UUID] -> ByteString -> IO ()
insertRaw env workspace identifiers statement = do
  initial <- makeTask env workspace "Raw session fixture creation"
  runTransaction env.pool $ S.sql (statement <> ";" <> BS.concat
    ["INSERT INTO task_sessions(workspace_id,session_id,task_id) VALUES ('"<>uuid workspace<>"','"<>uuid identifier<>"','"<>uuid initial.id<>"');" | identifier <- identifiers])
  execute env ("UPDATE tasks SET deleted_at=clock_timestamp() WHERE id='"<>uuid initial.id<>"'")

waitForLock :: TestEnv -> ByteString -> IO Bool
waitForLock env pattern = loop (500::Int)
  where
    loop 0=do
      phase <- runSession env.pool $ S.statement () $ St.Statement
        "SELECT coalesce(string_agg(state||':'||coalesce(wait_event_type,'-')||':'||left(query,300),E'\\n'),'') FROM pg_stat_activity WHERE datname=current_database() AND pid<>pg_backend_pid()"
        E.noParams (D.singleRow (D.column (D.nonNullable D.text))) False
      expectationFailure ("Expected blocked phase "<>B8.unpack pattern<>"; observed "<>T.unpack phase)
      pure False
    loop remaining=do
      blocked <- boolean env ("SELECT EXISTS(SELECT 1 FROM pg_stat_activity WHERE datname=current_database() AND pid<>pg_backend_pid() AND wait_event_type='Lock' AND query LIKE '"<>pattern<>"')")
      if blocked then pure True else threadDelay 10000 >> loop (remaining-1)

append :: TestEnv -> UUID -> AgentSession -> ByteString -> Bool -> IO SessionAppendResult
append env workspace row bytes flush = appendSession env.pool workspace row.id (AppendSession row.committedBytes row.revision bytes flush)

receipt :: AgentSession -> ByteString -> TerminateSession
receipt row body = TerminateSession row.committedBytes row.revision (fromIntegral (BS.length body)) (sessionSha256 body)

allBytes :: TestEnv -> UUID -> UUID -> IO ByteString
allBytes env workspace identifier = (.bytes) <$> readSessionRange env.pool workspace identifier 0 maxSessionRangeBytes

makeTask :: TestEnv -> UUID -> T.Text -> IO Types.Task
makeTask env workspace title = do
  task <- Task.createTask env.pool (Types.CreateTask workspace Nothing Nothing title Nothing Nothing Nothing)
  execute env ("UPDATE tasks SET status='in_progress' WHERE id='" <> uuid task.id <> "'")
  Task.getTask env.pool task.id >>= maybe (fail "Missing test task") pure

expectCode :: SessionErrorCode -> IO a -> IO ()
expectCode code action = do
  result <- try @SessionException action
  case result of
    Left e -> e.code `shouldBe` code
    Right _ -> expectationFailure ("Expected " <> show code)

rejectDb :: IO a -> IO ()
rejectDb action = do
  result <- try @DBException action
  case result of
    Left (DBCheckViolation _) -> pure ()
    Left (DBForeignKeyViolation _) -> pure ()
    other -> expectationFailure $ "Expected guarded SQL rejection, got " <> either show (const "success") other

execute :: TestEnv -> ByteString -> IO ()
execute env sql = runSession env.pool (S.sql sql)

boolean :: TestEnv -> ByteString -> IO Bool
boolean env sql = runSession env.pool $ S.statement () $ St.Statement sql E.noParams (D.singleRow (D.column (D.nonNullable D.bool))) False

uuid :: UUID -> ByteString
uuid = B8.pack . show

assertTruncateGuards :: TestEnv -> IO ()
assertTruncateGuards env = do
  boolean env "SELECT count(*)=3 AND bool_and(tgenabled='O') FROM pg_trigger WHERE tgname='session_evidence_no_truncate' AND tgrelid IN ('public.sessions'::regclass,'public.session_chunks'::regclass,'public.task_sessions'::regclass)" `shouldReturn` True
  forM_ ["TRUNCATE session_chunks","TRUNCATE task_sessions","TRUNCATE sessions CASCADE"] $ \sql -> rejectDb (execute env sql)

awaitCleanupSleep :: TestEnv -> Int -> IO ()
awaitCleanupSleep env attempts = do
  sleeping <- boolean env "SELECT EXISTS(SELECT 1 FROM pg_stat_activity WHERE datname=current_database() AND query LIKE 'TRUNCATE change_stream_snapshot_page_tokens%' AND wait_event='PgSleep')"
  if sleeping then pure ()
  else if attempts <= 0 then expectationFailure "Cleanup did not reach the synchronized trigger wait"
  else threadDelay 10000 >> awaitCleanupSleep env (attempts-1)

withSessionRacePools :: TestEnv -> (Pool Hasql.Connection -> Pool Hasql.Connection -> IO a) -> IO a
withSessionRacePools env action =
  bracket (createPool env.testDb.testDbConnStr 2 30 30000) destroyAllResources $ \peer ->
    bracket (createPool env.testDb.testDbConnStr 1 30 30000) destroyAllResources $ \probe -> action peer probe

withinSessionRace :: IO a -> IO a
withinSessionRace action = timeout 6000000 action >>= maybe (fail "Session creation race phase exceeded six seconds") pure

-- Optional measurement only. Every owner is joined independently; the observer
-- emits numeric frame counts, never PostgreSQL packet contents or credentials.
withCycleCounter :: TestEnv -> (Pool Hasql.Connection -> IO [Int] -> IO a) -> IO a
withCycleCounter env action = bracket acquire cleanup $ \(input,output,process,port) ->
  bracket (createPool ("host=127.0.0.1 port="<>T.pack(show port)<>" dbname="<>env.testDb.testDbName) 1 5 30000) destroyAllResources $ \pool ->
    (action pool $ do
      hPutStrLn input "snapshot"; hFlush input
      line <- timeout 5000000 (hGetLine output) >>= maybe (fail "PostgreSQL counter snapshot deadline") pure
      value <- either fail pure (Aeson.eitherDecodeStrict' (B8.pack line))
      either fail pure $ parseEither (Aeson.withObject "cycle counts" $ \o -> sequence [o Aeson..: "frontendSync",o Aeson..: "frontendSimpleQuery",o Aeson..: "frontendExecute",o Aeson..: "backendReady",o Aeson..: "unsupported"]) value)
      `catch` \(failure :: SomeException) -> putStrLn ("[session-db-counter-primary] "<>take 1200 (displayException failure)) >> throwIO failure
  where
    acquire = do
      let path = env.testSandbox.sandboxRepoRoot </> "hmem-server" </> "frontend" </> "perf" </> "session-sync.mjs"
      (Just input,Just output,_,process) <- createProcess (proc "node" [path,"pg-counter",show env.testDb.testDbPort]) {std_in=CreatePipe,std_out=CreatePipe,std_err=Inherit,create_group=True}
      let readPort :: IO Int
          readPort = do
            line <- timeout 10000000 (hGetLine output) >>= maybe (fail "PostgreSQL counter startup deadline") pure
            value <- either fail pure (Aeson.eitherDecodeStrict' (B8.pack line))
            either fail pure $ parseEither (Aeson.withObject "counter startup" (Aeson..: "port")) value
      port <- readPort `onException` (try @SomeException (cleanup (input,output,process,0)) >> pure ())
      pure (input,output,process,port)
    cleanup (input,output,process,_) = do
      _ <- try @SomeException (hPutStrLn input "close" >> hFlush input >> hClose input)
      joined <- timeout 7000000 (waitForProcess process)
      case joined of
        Just ExitSuccess -> hClose output
        _ -> do
          terminateProcess process
          final <- timeout 5000000 (waitForProcess process)
          hClose output
          case final of Nothing -> fail "PostgreSQL counter process did not join"; _ -> fail "PostgreSQL counter cleanup failed"
