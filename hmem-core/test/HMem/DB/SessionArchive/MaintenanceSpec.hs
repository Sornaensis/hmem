module HMem.DB.SessionArchive.MaintenanceSpec (spec) where

import Control.Exception (SomeException,try,bracket,bracket_)
import Control.Monad (forM_,replicateM_)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as B8
import Data.Pool (destroyAllResources)
import Data.Text qualified as T
import Data.UUID.V4 qualified as UUID
import Hasql.Decoders qualified as D
import Hasql.Encoders qualified as E
import Hasql.Session qualified as S
import Hasql.Statement qualified as St
import Test.Hspec
import System.Directory (createDirectory,listDirectory,copyFile)
import System.FilePath ((</>))

import HMem.AgentSession
import HMem.DB.AgentSession
import HMem.DB.Pool (runSession,createPool)
import HMem.DB.Migration qualified as Migration
import HMem.DB.SessionArchive.Maintenance
import HMem.DB.Task qualified as Task
import HMem.DB.TestHarness
import HMem.Types qualified as Types

spec :: Spec
spec=around withTestEnv $ describe "Archive maintenance storage" $ do
  it "defaults to new-seal work only and provisions an unassumable restricted capability" $ \env -> do
    boolean env "SELECT NOT paused AND NOT backfill_enabled AND NOT reclaim_enabled AND NOT qualified AND staging_budget=8589934592 AND wal_budget=34359738368 FROM session_archive_control" `shouldReturn` True
    boolean env "SELECT NOT rolcanlogin AND NOT rolinherit AND NOT rolsuper AND NOT rolcreatedb AND NOT rolcreaterole AND NOT rolreplication AND NOT rolbypassrls FROM pg_roles WHERE rolname='hmem_archive_maintenance_owner'" `shouldReturn` True
    boolean env "SELECT p.prosecdef AND r.rolname='hmem_archive_maintenance_owner' AND p.proconfig=ARRAY['search_path=pg_catalog'] AND NOT EXISTS(SELECT 1 FROM aclexplode(p.proacl) WHERE grantee=0 AND privilege_type='EXECUTE') FROM pg_proc p JOIN pg_roles r ON r.oid=p.proowner WHERE p.oid='public.hmem_archive_reclaim(uuid,uuid,uuid,text,uuid)'::regprocedure" `shouldReturn` True
    rejected (execute env "SELECT public.hmem_archive_reclaim(gen_random_uuid(),gen_random_uuid(),gen_random_uuid(),repeat('0',64),gen_random_uuid())")
  it "requires qualification for backfill/reclamation and persists pause/capacity controls" $ \env -> do
    rejected (execute env "UPDATE session_archive_control SET reclaim_enabled=true")
    rejected (execute env "UPDATE session_archive_control SET backfill_enabled=true")
    configureMaintenance env.pool EnableBackfill
    rejected (configureMaintenance env.pool EnableReclamation)
    configureMaintenance env.pool Pause
    configureMaintenance env.pool (Capacity 104857600 419430400)
    boolean env "SELECT paused AND qualified AND backfill_enabled AND NOT reclaim_enabled AND staging_budget=104857600 FROM session_archive_control" `shouldReturn` True
    configureMaintenance env.pool Resume
    boolean env "SELECT NOT paused FROM session_archive_control" `shouldReturn` True
  it "commits empty discovery and its cursor together" $ \env -> do
    discoverArchives env.pool `shouldReturn` 0
    boolean env "SELECT cursor_time IS NULL AND cursor_id IS NULL FROM session_archive_control" `shouldReturn` True
  it "validates session identities under pg_restore's empty search path without changing the grammar" $ \env ->
    runSession env.pool (do
      S.sql "SET LOCAL search_path=''"
      S.statement () (St.Statement "SELECT public.hmem_session_runtime_identity_valid(NULL) AND public.hmem_session_runtime_identity_valid('{\"model\":\"fixture\",\"role\":\"developer\"}'::jsonb) AND public.hmem_session_runtime_identity_valid('{\"model\":\"legacy\"}'::jsonb) AND NOT public.hmem_session_runtime_identity_valid('{\"model\":\" invalid \"}'::jsonb)" E.noParams (D.singleRow (D.column (D.nonNullable D.bool))) False)) `shouldReturn` True
  it "commits discovery candidates and the durable cursor together across interruption and restart" $ \env -> do
    workspace<-(.id) <$> createTestWorkspace env "interrupted-discovery"
    task<-Task.createTask env.pool (Types.CreateTask workspace Nothing Nothing "Discovery fixture" Nothing Nothing Nothing)
    execute env ("UPDATE tasks SET status='in_progress' WHERE id='"<>B8.pack (show task.id)<>"'")
    replicateM_ 65 $ do
      identifier<-UUID.nextRandom
      _<-createSession env.pool (CreateSession identifier workspace "jsonl" "Discovery fixture" Nothing Nothing task.id)
      _<-terminateSession env.pool workspace identifier (TerminateSession 0 0 0 (sessionSha256 ""))
      pure ()
    bracket_
      (execute env "CREATE FUNCTION fail_archive_cursor() RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN IF NEW.cursor_id IS NOT NULL THEN RAISE EXCEPTION 'injected cursor failure'; END IF; RETURN NEW; END $$; CREATE TRIGGER fail_archive_cursor BEFORE UPDATE ON session_archive_control FOR EACH ROW EXECUTE FUNCTION fail_archive_cursor()")
      (execute env "DROP TRIGGER fail_archive_cursor ON session_archive_control; DROP FUNCTION fail_archive_cursor()") $ do
        rejected (discoverArchives env.pool)
        boolean env "SELECT NOT EXISTS(SELECT 1 FROM session_archive_jobs) AND cursor_id IS NULL FROM session_archive_control" `shouldReturn` True
    discoverArchives env.pool `shouldReturn` 32
    boolean env "SELECT (SELECT count(*) FROM session_archive_jobs)=32 AND cursor_id IS NOT NULL FROM session_archive_control" `shouldReturn` True
    bracket (createPool env.testDb.testDbConnStr 2 30 30000) destroyAllResources $ \restarted -> do
      discoverArchives restarted `shouldReturn` 32
      discoverArchives restarted `shouldReturn` 1
      discoverArchives restarted `shouldReturn` 0
    boolean env "SELECT (SELECT count(*) FROM session_archive_jobs)=65 AND cursor_id IS NULL FROM session_archive_control" `shouldReturn` True
  it "upgrades pre-archive and V050 databases atomically and preserves sealed evidence on rollback/retry" $ \_ ->
    forM_ ["V050","V051"] $ \cutoff -> withTestSandbox $ \sandbox -> withSandboxedEnv sandbox $
      withSandboxedPostgres sandbox $ \db -> bracket (createPool db.testDbConnStr 3 30 30000) destroyAllResources $ \pool -> do
        let env=TestEnv pool sandbox db
            before=sandbox.sandboxRoot </> "before-maintenance"
            after=sandbox.sandboxRoot </> "maintenance-upgrade"
        createDirectory before; createDirectory after
        files<-listDirectory sandbox.sandboxMigrationsDir
        forM_ (filter (T.isSuffixOf ".sql" . T.pack) files) $ \file ->
          copyFile (sandbox.sandboxMigrationsDir </> file) ((if file<cutoff then before else after) </> file)
        Migration.runMigrations pool before >>= (\r -> r.failed `shouldBe` Nothing)
        w<-(.id) <$> createTestWorkspace env "maintenance-upgrade"
        task<-Task.createTask pool (Types.CreateTask w Nothing Nothing "Upgrade evidence" Nothing Nothing Nothing)
        execute env ("UPDATE tasks SET status='in_progress' WHERE id='"<>B8.pack (show task.id)<>"'")
        identifier<-UUID.nextRandom
        initial<-createSession pool (CreateSession identifier w "jsonl" "Upgrade evidence" Nothing Nothing task.id)
        uploaded<-(.session) <$> appendSession pool w identifier (AppendSession initial.committedBytes initial.revision "{}\r\n" True)
        sealed<-terminateSession pool w identifier (TerminateSession uploaded.committedBytes uploaded.revision uploaded.committedBytes (sessionSha256 "{}\r\n"))
        bracket_
          (execute env "CREATE FUNCTION fail_maintenance_ledger() RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN IF NEW.version=51 THEN RAISE EXCEPTION 'injected V051 ledger failure'; END IF; RETURN NEW; END $$; CREATE TRIGGER fail_maintenance_ledger BEFORE INSERT ON schema_migrations FOR EACH ROW EXECUTE FUNCTION fail_maintenance_ledger()")
          (execute env "DROP TRIGGER fail_maintenance_ledger ON schema_migrations; DROP FUNCTION fail_maintenance_ledger()") $ do
            Migration.runMigrations pool after >>= (\r -> r.failed `shouldSatisfy` (/=Nothing))
            boolean env "SELECT to_regclass('session_archive_control') IS NULL AND NOT EXISTS(SELECT 1 FROM schema_migrations WHERE version=51)" `shouldReturn` True
            rejected (execute env "DELETE FROM session_chunks")
        Migration.runMigrations pool after >>= (\r -> r.failed `shouldBe` Nothing)
        Migration.runMigrations pool after >>= (\r -> r.applied `shouldBe` [])
        getSession pool w identifier `shouldReturn` Just sealed
        readSessionRange pool w identifier 0 10 >>= (\r -> r.bytes `shouldBe` "{}\r\n")

execute :: TestEnv -> ByteString -> IO ()
execute env sql=runSession env.pool (S.sql sql)
boolean :: TestEnv -> ByteString -> IO Bool
boolean env sql=runSession env.pool $ S.statement () (St.Statement sql E.noParams (D.singleRow (D.column (D.nonNullable D.bool))) False)
rejected :: IO a -> IO ()
rejected action=try @SomeException action >>= \case Left _ -> pure (); Right _ -> expectationFailure "Expected rejection"
