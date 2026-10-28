module HMem.Server.Archive.MaintenanceSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (concurrently,withAsync,wait)
import Control.Concurrent.MVar (newEmptyMVar,putMVar,takeMVar)
import Control.Exception (SomeException,bracket,try)
import Control.Monad (foldM,forM_,void)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as B8
import Data.Aeson (encode)
import Data.ByteString.Lazy qualified as LBS
import Data.Int (Int64)
import Data.Pool (Pool,destroyAllResources)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.UUID (UUID)
import Data.UUID.V4 qualified as UUID
import Hasql.Connection qualified as H
import Hasql.Decoders qualified as D
import Hasql.Encoders qualified as E
import Hasql.Session qualified as S
import Hasql.Statement qualified as St
import System.Timeout (timeout)
import Test.Hspec

import HMem.AgentSession
import HMem.DB.AgentSession
import HMem.DB.Pool (createPool,runSession,runTransaction)
import HMem.DB.SessionArchive qualified as A
import HMem.DB.SessionArchive.Maintenance qualified as M
import HMem.DB.SessionArchive.Reader qualified as R
import HMem.DB.Task qualified as Task
import HMem.DB.TestHarness
import HMem.Server.Archive.Maintenance
import HMem.Server.Archive.Worker
import HMem.Server.CtlArchives
import HMem.SessionArchive.Codec qualified as C
import HMem.Types qualified as Types

spec :: Spec
spec=around withTestEnv $ describe "Archive maintenance worker" $ do
  it "verifies and reclaims empty, UTF-8/CRLF/final-no-LF and large evidence in bounded atomic turns" $ \admin -> bounded $
    withRuntime admin $ \pool -> do
      let env=admin {pool=pool}
      status<-runArchivesOperationWithPool pool ArchivesStatus
      LBS.toStrict (encode status) `shouldSatisfy` BS.isInfixOf "\"runtime_safe\":true"
      forM_ [[],["{}\r\n[\"\195\166\"]"],["{}","\n[]\n"],["{}","\r\n[]\r\n"],replicate 19 ("\""<>BS.replicate 180000 120<>"\"\n")<>["[true,null]"]] $ \chunks -> do
        (w,sealed,_)<-fixture env chunks
        manifest<-publish env w sealed
        M.configureMaintenance pool M.EnableReclamation
        Just lease<-M.claimReclamation pool
        verifyPublishedArchive pool manifest
        M.recordVerification pool lease Nothing
        let drain=do
              before<-number env "SELECT count(*) FROM session_chunks"
              deleted<-M.reclaimArchiveBatch pool lease
              deleted `shouldSatisfy` (\n -> n>=0 && n<=8)
              after<-number env "SELECT count(*) FROM session_chunks"
              before-after `shouldBe` deleted
              forM_ [0,1,1048575,fromIntegral (BS.length (BS.concat chunks))] $ \offset ->
                if offset>sealed.committedBytes then pure () else do
                  bytes<-readSessionRange pool w sealed.id offset 1048576
                  bytes.bytes `shouldBe` BS.take 1048576 (BS.drop (fromIntegral offset) (BS.concat chunks))
              if deleted==0 then pure () else drain
        drain
        M.reclaimArchiveBatch pool lease `shouldReturn` 0
        M.releaseReclamation pool lease
        getSession pool w sealed.id `shouldReturn` Just sealed
        if null chunks then pure () else do
          replay<-appendSession pool w sealed.id (AppendSession 0 0 (head chunks) True)
          replay.replayed `shouldBe` True
        number env ("SELECT count(*) FROM session_archive_ledger WHERE generation_id='"<>uuid manifest.id<>"'") `shouldReturn` fromIntegral (length chunks)

  forM_ ["direct","assumable","preinstalled"] $ \mode ->
    it ("rejects "<>mode<>" trigger authority before privileged reclamation") $ \admin -> bounded $
      withRuntime admin $ \pool -> do
        let env=admin {pool=pool}
        runtime<-runSession pool $ S.statement () (St.Statement "SELECT session_user::text" E.noParams (D.singleRow (D.column (D.nonNullable D.text))) False)
        let role=TE.encodeUtf8 runtime
            holder=if mode=="direct" then role else role<>"_hooks"
            schema=role<>"_private"
            targets=["session_chunks","session_archive_control","session_archive_jobs","session_archive_generations","session_archive_reclamation"]
            setup=execute admin ((if mode=="direct" then "" else "CREATE ROLE "<>holder<>" NOLOGIN NOINHERIT; GRANT "<>holder<>" TO "<>role<>"; ")<>"CREATE SCHEMA "<>schema<>" AUTHORIZATION "<>holder)
            cleanup=execute admin ("DROP SCHEMA "<>schema<>" CASCADE; "<>(if mode=="direct" then "" else "REVOKE "<>holder<>" FROM "<>role<>"; DROP OWNED BY "<>holder<>"; DROP ROLE "<>holder))
            asHolder sql=execute env ("SET LOCAL ROLE "<>holder<>"; "<>sql)
            grantTrigger table=execute admin ("GRANT TRIGGER ON public."<>table<>" TO "<>holder)
            revokeTrigger table=execute admin ("REVOKE TRIGGER ON public."<>table<>" FROM "<>holder)
        bracket setup (const cleanup) $ \_ -> do
          boolean env "SELECT NOT has_schema_privilege(session_user,'public','CREATE')" `shouldReturn` True
          (workspace,sealed,_)<-fixture env ["{}\n"]
          manifest<-publish env workspace sealed
          (victimWorkspace,victim,_)<-fixture env (replicate 9 "[]\n")
          M.configureMaintenance pool M.EnableReclamation
          Just lease<-M.claimReclamation pool
          verifyPublishedArchive pool manifest
          M.recordVerification pool lease Nothing
          let denied=do
                boolean env "SELECT NOT public.hmem_archive_runtime_safe()" `shouldReturn` True
                rejected (M.reclaimArchiveBatch pool lease)
                number env "SELECT count(*) FROM session_chunks" `shouldReturn` 10
                number env "SELECT count(*) FROM session_archive_reclamation WHERE verified_at IS NOT NULL AND error_code IS NULL AND deleted_revision=0 AND deleted_bytes=0 AND NOT complete" `shouldReturn` 1
                getSession pool victimWorkspace victim.id `shouldReturn` Just victim
                M.configureMaintenance pool M.DisableReclamation
                rejected (M.configureMaintenance pool M.EnableReclamation)
                boolean env "SELECT NOT reclaim_enabled FROM session_archive_control" `shouldReturn` True
              restored=do
                boolean env "SELECT public.hmem_archive_runtime_safe()" `shouldReturn` True
                M.configureMaintenance pool M.EnableReclamation
          if mode=="preinstalled" then do
            -- This invoker callback would delete nine unverified rows from
            -- another workspace as the definer, outside its bound batch.
            asHolder ("CREATE FUNCTION "<>schema<>".nested_delete() RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN IF (TG_OP='DELETE' AND OLD.session_id='"<>uuid sealed.id<>"'::uuid) OR (TG_OP='UPDATE' AND NEW.session_id='"<>uuid sealed.id<>"'::uuid) THEN DELETE FROM public.session_chunks WHERE session_id='"<>uuid victim.id<>"'::uuid; END IF; RETURN coalesce(NEW,OLD); END $$")
            execute admin ("GRANT USAGE ON SCHEMA "<>schema<>" TO hmem_archive_maintenance_owner")
            forM_ [("session_chunks","DELETE"),("session_archive_reclamation","UPDATE")] $ \(table,event) -> do
              grantTrigger table
              asHolder ("CREATE TRIGGER archive_callback AFTER "<>event<>" ON public."<>table<>" FOR EACH ROW EXECUTE FUNCTION "<>schema<>".nested_delete()")
              revokeTrigger table
              boolean env ("SELECT NOT has_table_privilege('"<>TE.encodeUtf8 runtime<>"', 'public."<>table<>"','TRIGGER') AND NOT has_table_privilege('"<>holder<>"','public."<>table<>"','TRIGGER')") `shouldReturn` True
              denied
              execute admin ("DROP TRIGGER archive_callback ON public."<>table)
              restored
          else forM_ targets $ \table -> do
            grantTrigger table
            denied
            revokeTrigger table
            restored
          if mode=="assumable" then do
            execute admin ("GRANT CREATE ON SCHEMA public TO "<>holder)
            denied
            execute admin ("REVOKE CREATE ON SCHEMA public FROM "<>holder)
            restored
          else pure ()
          M.releaseReclamation pool lease

  it "rejects direct DML, wrong bindings, unverified deletion, pause and unsafe administrative callers" $ \admin -> bounded $
    withRuntime admin $ \pool -> do
      let env=admin {pool=pool}
      (w,sealed,_)<-fixture env ["{}\n"]
      manifest<-publish env w sealed
      M.configureMaintenance pool M.EnableReclamation
      Just lease<-M.claimReclamation pool
      forM_ ["UPDATE session_chunks SET allow_final_record=false","DELETE FROM session_chunks","TRUNCATE session_chunks"] $ \sql -> rejected (execute env sql)
      rejected (M.reclaimArchiveBatch pool lease)
      verifyPublishedArchive pool manifest
      M.recordVerification pool lease Nothing
      foreignWorkspace<-UUID.nextRandom
      foreignGeneration<-UUID.nextRandom
      rejected (M.reclaimArchiveBatch pool (lease {M.manifest=manifest {A.workspaceId=foreignWorkspace}}))
      rejected (M.reclaimArchiveBatch pool (lease {M.manifest=manifest {A.id=foreignGeneration}}))
      rejected (M.reclaimArchiveBatch pool (lease {M.manifest=manifest {A.metadataSha256=T.replicate 64 "0"}}))
      rejected (M.reclaimArchiveBatch admin.pool lease)
      M.configureMaintenance pool M.Pause
      M.reclaimArchiveBatch pool lease `shouldReturn` 0
      number env "SELECT count(*) FROM session_chunks" `shouldReturn` 1
      M.configureMaintenance pool M.Resume
      M.reclaimArchiveBatch pool lease `shouldReturn` 1
      rejected (execute env ("DELETE FROM session_archive_generations WHERE id='"<>uuid manifest.id<>"'"))

  it "rolls back the whole delete batch when a later raw row differs from its immutable ledger" $ \admin -> bounded $
    withRuntime admin $ \pool -> do
      let env=admin {pool=pool}
      (w,sealed,_)<-fixture env (replicate 3 "{}\n")
      manifest<-publish env w sealed
      M.configureMaintenance pool M.EnableReclamation
      Just lease<-M.claimReclamation pool
      verifyPublishedArchive pool manifest
      M.recordVerification pool lease Nothing
      M.configureMaintenance pool M.EnableReclamation
      runTransaction admin.pool $ S.sql "ALTER TABLE session_chunks DISABLE TRIGGER session_chunk_guard; UPDATE session_chunks SET data=convert_to('[]'||chr(10),'UTF8') WHERE revision=2; ALTER TABLE session_chunks ENABLE TRIGGER session_chunk_guard"
      rejected (M.reclaimArchiveBatch pool lease)
      number env "SELECT count(*) FROM session_chunks" `shouldReturn` 3
      number env "SELECT deleted_revision FROM session_archive_reclamation" `shouldReturn` 0

  it "fails stored gzip verification without creating a deletion capability" $ \admin -> bounded $
    withRuntime admin $ \pool -> do
      let env=admin {pool=pool}
      (w,sealed,_)<-fixture env ["{}\n"]
      manifest<-publish env w sealed
      M.configureMaintenance pool M.EnableReclamation
      runTransaction admin.pool $ S.sql "ALTER TABLE session_archive_parts DISABLE TRIGGER session_archive_stage_guard; UPDATE session_archive_parts SET data=set_byte(data,0,get_byte(data,0)#1) WHERE ordinal=1; ALTER TABLE session_archive_parts ENABLE TRIGGER session_archive_stage_guard"
      rejected (verifyPublishedArchive pool manifest)
      maintainArchive pool
      M.verificationRecorded pool manifest `shouldReturn` False
      number env "SELECT count(*) FROM session_chunks" `shouldReturn` 1
      number env "SELECT count(*) FROM session_archive_reclamation WHERE error_code='archive_verification_failed' AND verified_at IS NULL" `shouldReturn` 1

  it "keeps old queued history dormant but honors an explicit bounded same-workspace request" $ \env -> bounded $ do
    (w,sealed,_)<-fixture env ["{}\n"]
    execute env "UPDATE session_archive_control SET cutover_at=clock_timestamp()+interval '1 hour'"
    M.discoverArchives env.pool `shouldReturn` 0
    _<-A.enqueueArchives env.pool Nothing 100
    implementation<-C.codecImplementation
    A.claimArchive env.pool implementation `shouldReturn` Nothing
    M.maintenanceStatus env.pool >>= (\status -> LBS.toStrict (encode status) `shouldSatisfy` BS.isInfixOf "resource_blocked_jobs")
    foreignWorkspace<-UUID.nextRandom
    R.ensureArchive env.pool foreignWorkspace sealed.id
    A.claimArchive env.pool implementation `shouldReturn` Nothing
    R.ensureArchive env.pool w sealed.id
    Just lease<-A.claimArchive env.pool implementation
    runArchiveLease env.pool lease
    A.getPublishedArchive env.pool w sealed.id >>= (`shouldSatisfy` (/=Nothing))

  it "fences a global attempt, renews during pause and drains detached staging before replacement" $ \env -> bounded $ do
    (w,sealed,_)<-fixture env ["{}\n"]
    _<-fixture env ["[]\n"]
    _<-M.discoverArchives env.pool
    implementation<-C.codecImplementation
    (one,two)<-concurrently (A.claimArchive env.pool implementation) (A.claimArchive env.pool implementation)
    let leases=[l | Just l<-[one,two]]
    length leases `shouldBe` 1
    let lease=head leases
    _<-compressArchive env.pool lease
    M.configureMaintenance env.pool M.Pause
    A.renewArchive env.pool lease `shouldReturn` True
    A.claimArchive env.pool implementation `shouldReturn` Nothing
    A.releaseArchive env.pool lease False "test_pause" `shouldReturn` True
    M.configureMaintenance env.pool M.Resume
    A.claimArchive env.pool implementation `shouldReturn` Nothing
    let drain=do n<-A.cleanupNextArchiveGeneration env.pool; if n==0 then pure () else drain
    drain
    A.claimArchive env.pool implementation >>= (`shouldSatisfy` (/=Nothing))
    getSession env.pool w sealed.id `shouldReturn` Just sealed

  it "reports capacity blocking without claiming unsupported session sizes and admits raised capacity" $ \env -> bounded $ do
    _<-fixture env ["{}\n"]
    _<-M.discoverArchives env.pool
    M.configureMaintenance env.pool (M.Capacity 0 0)
    implementation<-C.codecImplementation
    A.claimArchive env.pool implementation `shouldReturn` Nothing
    number env "SELECT count(*) FROM session_archive_control WHERE blocked_reason='resource_blocked' AND permit_generation IS NULL" `shouldReturn` 1
    M.configureMaintenance env.pool (M.Capacity 8589934592 34359738368)
    A.claimArchive env.pool implementation >>= (`shouldSatisfy` (/=Nothing))

  it "admits fitting new seals while a larger supported requested session waits for capacity" $ \env -> bounded $ do
    (largeWorkspace,large,_)<-fixture env ["\""<>BS.replicate 200000 120<>"\"\n"]
    R.ensureArchive env.pool largeWorkspace large.id
    (smallWorkspace,small,_)<-fixture env ["{}\n"]
    _<-M.discoverArchives env.pool
    M.configureMaintenance env.pool (M.Capacity 8500000 34000000)
    implementation<-C.codecImplementation
    Just fitting<-A.claimArchive env.pool implementation
    fitting.sessionId `shouldBe` small.id
    runArchiveLease env.pool fitting
    A.getPublishedArchive env.pool smallWorkspace small.id >>= (`shouldSatisfy` (/=Nothing))
    A.claimArchive env.pool implementation `shouldReturn` Nothing
    M.configureMaintenance env.pool (M.Capacity 8589934592 34359738368)
    Just raised<-A.claimArchive env.pool implementation
    raised.sessionId `shouldBe` large.id

  it "shares one durable permit across two verifiers and compression and drains a paused verification" $ \admin -> bounded $
    withRuntime admin $ \pool -> do
      let env=admin {pool=pool}
      (w,sealed,_)<-fixture env ["{}\n"]
      _<-publish env w sealed
      M.configureMaintenance pool M.EnableReclamation
      Just lease<-M.claimReclamation pool
      entered<-newEmptyMVar
      continue<-newEmptyMVar
      withAsync (runReclamationLeaseWith (putMVar entered () >> takeMVar continue) pool lease) $ \worker -> do
        timeout 1000000 (takeMVar entered) `shouldReturn` Just ()
        M.claimReclamation pool `shouldReturn` Nothing
        _<-fixture env ["[]\n"]
        _<-M.discoverArchives pool
        implementation<-C.codecImplementation
        A.claimArchive pool implementation `shouldReturn` Nothing
        M.configureMaintenance pool M.Pause
        number env "SELECT count(*) FROM session_archive_control WHERE paused AND permit_kind='maintenance' AND permit_expires_at>clock_timestamp()" `shouldReturn` 1
        M.renewReclamation pool lease `shouldReturn` True
        putMVar continue ()
        timeout 5000000 (wait worker) `shouldReturn` Just ()
      number env "SELECT count(*) FROM session_archive_control WHERE paused AND permit_kind IS NULL" `shouldReturn` 1
      M.verificationRecorded pool lease.manifest `shouldReturn` True
      number env "SELECT count(*) FROM session_archive_reclamation WHERE error_code IS NOT NULL" `shouldReturn` 0
      number env "SELECT count(*) FROM session_archive_reclamation WHERE deleted_revision=0 AND deleted_bytes=0 AND NOT complete" `shouldReturn` 1
      number env ("SELECT count(*) FROM session_chunks WHERE session_id='"<>uuid sealed.id<>"'") `shouldReturn` 1

  it "fences expired verification results and stale release from a replacement maintenance permit" $ \admin -> bounded $
    withRuntime admin $ \pool -> do
      let env=admin {pool=pool}
      (w,sealed,_)<-fixture env ["{}\n"]
      _<-publish env w sealed
      M.configureMaintenance pool M.EnableReclamation
      Just old<-M.claimReclamation pool
      execute admin "UPDATE session_archive_control SET permit_expires_at=clock_timestamp()+interval '20 milliseconds' WHERE singleton"
      threadDelay 40000
      M.renewReclamation pool old `shouldReturn` False
      Just fresh<-M.claimReclamation pool
      fresh.leaseToken `shouldNotBe` old.leaseToken
      M.recordVerification pool old Nothing
      M.verificationRecorded pool old.manifest `shouldReturn` False
      rejected (M.reclaimArchiveBatch pool old)
      M.releaseReclamation pool old
      M.renewReclamation pool fresh `shouldReturn` True
      M.releaseReclamation pool fresh

bounded :: IO () -> IO ()
bounded action=timeout 60000000 action >>= (`shouldBe` Just ())
fixture :: TestEnv -> [ByteString] -> IO (UUID,AgentSession,UUID)
fixture env chunks=do
  w<-(.id) <$> createTestWorkspace env "archive-maintenance"
  task<-Task.createTask env.pool (Types.CreateTask w Nothing Nothing "Maintenance fixture" Nothing Nothing Nothing)
  execute env ("UPDATE tasks SET status='in_progress' WHERE id='"<>uuid task.id<>"'")
  identifier<-UUID.nextRandom
  initial<-createSession env.pool (CreateSession identifier w "jsonl" "Maintenance fixture" Nothing Nothing task.id)
  uploaded<-foldM (\row bytes -> (.session) <$> appendSession env.pool w identifier (AppendSession row.committedBytes row.revision bytes True)) initial chunks
  sealed<-terminateSession env.pool w identifier (TerminateSession uploaded.committedBytes uploaded.revision uploaded.committedBytes (sessionSha256 (BS.concat chunks)))
  pure(w,sealed,task.id)
publish :: TestEnv -> UUID -> AgentSession -> IO A.ArchiveManifest
publish env workspace sealed=do
  R.ensureArchive env.pool workspace sealed.id
  implementation<-C.codecImplementation
  Just lease<-A.claimArchive env.pool implementation
  runArchiveLease env.pool lease
  A.getPublishedArchive env.pool workspace sealed.id >>= \case
    Just manifest -> pure manifest
    Nothing -> do
      job<-A.getArchiveJob env.pool workspace sealed.id
      fail ("Archive was not published: "<>show job)

-- This is a real authenticated PostgreSQL connection, not SET ROLE under an
-- administrative session. The role cannot assume the protected NOLOGIN owner.
withRuntime :: TestEnv -> (Pool H.Connection -> IO a) -> IO a
withRuntime admin use=do
  identifier<-UUID.nextRandom
  let role="archive_runtime_"<>B8.filter (/='-') (uuid identifier)
      create=execute admin ("CREATE ROLE "<>role<>" LOGIN NOINHERIT NOSUPERUSER NOCREATEDB NOCREATEROLE NOREPLICATION NOBYPASSRLS; GRANT USAGE ON SCHEMA public TO "<>role<>"; GRANT SELECT,INSERT,UPDATE,DELETE ON ALL TABLES IN SCHEMA public TO "<>role<>"; GRANT EXECUTE ON ALL FUNCTIONS IN SCHEMA public TO "<>role)
      dropRole=execute admin ("DROP OWNED BY "<>role<>"; DROP ROLE "<>role)
  bracket create (const dropRole) $ \_ -> bracket
    (createPool (admin.testDb.testDbConnStr<>" user="<>T.pack (B8.unpack role)) 4 30 30000)
    destroyAllResources use
execute :: TestEnv -> ByteString -> IO ()
execute env sql=runSession env.pool (S.sql sql)
number :: TestEnv -> ByteString -> IO Int64
number env sql=runSession env.pool $ S.statement () (St.Statement sql E.noParams (D.singleRow (D.column (D.nonNullable D.int8))) False)
boolean :: TestEnv -> ByteString -> IO Bool
boolean env sql=runSession env.pool $ S.statement () (St.Statement sql E.noParams (D.singleRow (D.column (D.nonNullable D.bool))) False)
rejected :: IO a -> IO ()
rejected action=try @SomeException action >>= \case Left _ -> pure (); Right _ -> expectationFailure "Expected maintenance rejection"
uuid :: UUID -> ByteString
uuid=B8.pack . show
