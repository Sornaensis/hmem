module HMem.DB.SessionArchiveSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (withAsync, wait, concurrently)
import Control.Exception (bracket, bracket_, finally, try)
import Control.Monad (forM_)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as B8
import Data.Int (Int32, Int64)
import Data.Pool (destroyAllResources)
import Data.Text qualified as T
import Data.UUID (UUID)
import Data.UUID.V4 qualified as UUID
import Hasql.Decoders qualified as D
import Hasql.Encoders qualified as E
import Hasql.Session qualified as S
import Hasql.Statement qualified as St
import System.Directory (createDirectory, copyFile, listDirectory)
import System.FilePath ((</>))
import System.Timeout (timeout)
import Test.Hspec

import HMem.AgentSession
import HMem.DB.AgentSession
import HMem.DB.Migration qualified as Migration
import HMem.DB.Pool (DBException(..), createPool, runSession, withConn)
import HMem.DB.SessionArchive
import HMem.DB.Task qualified as Task
import HMem.DB.TestHarness
import HMem.Types qualified as Types

spec :: Spec
spec=around withTestEnv $ describe "Session archives" $ do
  it "keeps staged data invisible, publishes a bound complete generation, and retries exactly" $ \env -> do
    (w,s,l)<-claimed env "{}\r\n[]"
    getPublishedArchive env.pool w s.id `shouldReturn` Nothing
    stageComplete env l
    readArchivePart env.pool w s.id l.generationId 0 `shouldReturn` Nothing
    publishArchive env.pool l verification `shouldReturn` True
    publishArchive env.pool l verification `shouldReturn` True
    publishArchive env.pool l (verification {compressedSha256=sessionSha256 "wrong"}) `shouldReturn` False
    Just m<-getPublishedArchive env.pool w s.id
    (m.rawBytes,m.rawSha256,m.ledgerCount,m.partCount,m.codec) `shouldBe` (6,sessionSha256 "{}\r\n[]",1,2,"gzip-v1")
    Just p<-readArchivePart env.pool w s.id l.generationId 1
    p.bytes `shouldBe` BS.replicate 10 2
    Just c<-findArchiveCheckpoint env.pool w s.id l.generationId 3
    (c.rawOffset,c.compressedOffset,c.dictionary) `shouldBe` (0,10,"")
    Just original<-readArchiveLedger env.pool w s.id l.generationId 5
    (original.startOffset,original.endOffset,original.allowFinalRecord,original.rawSha256) `shouldBe` (0,6,True,sessionSha256 "{}\r\n[]")
    readSessionRange env.pool w s.id 0 10 >>= (\r->r.bytes `shouldBe` "{}\r\n[]")
    getSession env.pool w s.id `shouldReturn` Just s

  it "archives empty evidence with no ledger and preserves 64-bit counters" $ \env -> do
    (w,s,l)<-claimed env ""
    stageComplete env l
    publishArchive env.pool l (verification {ledgerCount=0}) `shouldReturn` True
    Just m<-getPublishedArchive env.pool w s.id
    (m.rawBytes,m.rawRecords,m.rawRevision,m.ledgerCount) `shouldBe` (0,0,1,0)
    boolean env "SELECT pg_typeof(raw_bytes)='bigint'::regtype AND pg_typeof(compressed_bytes)='bigint'::regtype AND pg_typeof(raw_revision)='bigint'::regtype FROM session_archives" `shouldReturn` True
    boolean env "SELECT (SELECT attstorage='e' FROM pg_attribute WHERE attrelid='session_archive_parts'::regclass AND attname='data') AND (SELECT attstorage='x' FROM pg_attribute WHERE attrelid='session_archive_checkpoints'::regclass AND attname='dictionary')" `shouldReturn` True

  it "requires sealed eligibility and rejects foreign workspace binding" $ \env -> do
    (w,s)<-fixture env "{}\n" False
    enqueueArchives env.pool Nothing 100 `shouldReturn` []
    reject $ execute env ("INSERT INTO session_archive_jobs(session_id,workspace_id) VALUES('"<>uuid s.id<>"','"<>uuid w<>"')")
    sealed<-terminateSession env.pool w s.id (TerminateSession s.committedBytes s.revision s.committedBytes (sessionSha256 "{}\n"))
    other<-(.id) <$> createTestWorkspace env "foreign-archive"
    reject $ execute env ("INSERT INTO session_archive_jobs(session_id,workspace_id) VALUES('"<>uuid sealed.id<>"','"<>uuid other<>"')")
    rows<-enqueueArchives env.pool Nothing 1
    length rows `shouldBe` 1
    enqueueArchives env.pool (Just (head rows)) 1 `shouldReturn` []
    _<-enqueueArchives env.pool Nothing 1
    claimArchive env.pool "test-storage-only" >>= (\x->x `shouldSatisfy` (/=Nothing))
    claimArchive env.pool "test-storage-only" `shouldReturn` Nothing

  it "rejects gaps, reordered and corrupt parts, checkpoints and ledger, then accepts intact data" $ \env -> do
    (_,_,l)<-claimed env "{}\n"
    reject $ stageArchivePart env.pool l 1 0 "bad"
    reject $ stageArchivePart env.pool l 0 1 "bad"
    reject $ execute env ("INSERT INTO session_archive_parts(generation_id,session_id,workspace_id,ordinal,start_offset,end_offset,data,data_sha256,binding_sha256) VALUES("<>binding l<>"0,0,3,decode('010203','hex'),repeat('0',64),'')")
    reject $ stageArchiveCheckpoint env.pool l 0 0 10 1 ""
    reject $ execute env ("INSERT INTO session_archive_ledger(generation_id,session_id,workspace_id,revision,start_offset,end_offset,allow_final_record,record_count,ends_with_newline,raw_sha256,binding_sha256) VALUES("<>binding l<>"1,0,3,false,1,true,repeat('0',64),'')")
    stageComplete env l
    publishArchive env.pool l verification `shouldReturn` True

  it "reads and stages one validated original append under the exact live ledger fence" $ \env -> do
    (w,active)<-fixture env "{}\r\n" False
    appended<-appendSession env.pool w active.id (AppendSession active.committedBytes active.revision "[]\n" True)
    let raw="{}\r\n[]\n"
        final=appended.session
    sealed<-terminateSession env.pool w final.id (TerminateSession final.committedBytes final.revision final.committedBytes (sessionSha256 raw))
    _<-enqueueArchives env.pool Nothing 100
    Just lease<-claimArchive env.pool "combined-source-ledger"
    wrongToken<-UUID.nextRandom
    let forged=ArchiveLease lease.workspaceId lease.sessionId lease.generationId wrongToken lease.attempt
        accept :: ArchiveSourceChunk -> Either T.Text Int64
        accept row=Right row.recordCount
        rejectGap row
          | row.startOffset==4 && row.revision==2 = Left "source gap"
          | otherwise = accept row
        readStage offset revision=readAndStageArchiveSourceChunk env.pool lease offset revision
    readAndStageArchiveSourceChunk env.pool forged 0 0 accept `shouldReturn` ArchiveSourceEnd
    readStage 1 0 rejectGap `shouldReturn` ArchiveSourceRejected "source gap"
    readStage 0 0 (const (Left "invalid metadata" :: Either T.Text Int64)) `shouldReturn` ArchiveSourceRejected "invalid metadata"
    readStage 0 1 accept `shouldReturn` ArchiveSourceLedgerRejected
    readStagedLedger env.pool lease 1 `shouldReturn` Nothing
    boolean env "SELECT staged_bytes=0 FROM session_archive_control" `shouldReturn` True

    readStage 0 0 accept `shouldReturn` ArchiveSourceReady (ArchiveSourceChunk 1 0 "{}\r\n" True 1 True) 1
    Just first<-readStagedLedger env.pool lease 1
    (first.revision,first.startOffset,first.endOffset,first.rawSha256) `shouldBe` (1,0,4,sessionSha256 "{}\r\n")
    T.length first.bindingSha256 `shouldBe` 64
    reject $ readStage 0 0 accept
    readStagedLedger env.pool lease 1 `shouldReturn` Just first
    boolean env "SELECT staged_bytes=1024 FROM session_archive_control" `shouldReturn` True

    readStage 4 1 accept `shouldReturn` ArchiveSourceReady (ArchiveSourceChunk 2 4 "[]\n" True 1 True) 1
    Just second<-readStagedLedger env.pool lease 2
    (second.revision,second.startOffset,second.endOffset,second.rawSha256) `shouldBe` (2,4,7,sessionSha256 "[]\n")
    second.bindingSha256 `shouldNotBe` first.bindingSha256
    readStage 7 2 accept `shouldReturn` ArchiveSourceEnd
    getPublishedArchive env.pool w sealed.id `shouldReturn` Nothing
    expire env sealed.id
    readStage 0 0 accept `shouldReturn` ArchiveSourceEnd
    boolean env "SELECT staged_bytes=2048 FROM session_archive_control" `shouldReturn` True
    readSessionRange env.pool w sealed.id 0 10 >>= (\result -> result.bytes `shouldBe` raw)

  it "rechecks global permit expiry after the initial source lease fence" $ \env -> do
    (_,sealed,lease)<-claimed env "{}\n"
    let isolated = createPool env.testDb.testDbConnStr 1 30 30000
        accept _=Right () :: Either T.Text ()
        waitFor observation = do
          let loop = do
                seen<-observation
                if seen then pure () else threadDelay 20000 >> loop
          timeout 6000000 loop `shouldReturn` Just ()
    bracket isolated destroyAllResources $ \blockerPool ->
      bracket isolated destroyAllResources $ \observerPool ->
        withConn blockerPool $ \blockerConnection -> do
          let run sql=S.run (S.sql sql) blockerConnection >>= either (fail . show) pure
              unblock=run "ROLLBACK"
              observer=env {pool=observerPool}
          blocker <- S.run (S.statement () (St.Statement "SELECT pg_backend_pid()" E.noParams (D.singleRow (D.column (D.nonNullable D.int4))) True)) blockerConnection
            >>= either (fail . show) pure
          let sourceBlockedWithLivePermit = runSession observerPool $ S.statement blocker $ St.Statement
                "SELECT EXISTS(SELECT 1 FROM pg_stat_activity a WHERE a.wait_event_type='Lock' AND pg_blocking_pids(a.pid) @> ARRAY[$1::int4] AND a.query LIKE 'SELECT c.revision,c.start_offset,c.data,%') AND EXISTS(SELECT 1 FROM session_archive_control WHERE singleton AND permit_kind='compression' AND permit_expires_at>clock_timestamp())"
                (E.param (E.nonNullable E.int4)) (D.singleRow (D.column (D.nonNullable D.bool))) False
              permitExpiredWithLiveJob = boolean observer
                ("SELECT c.permit_expires_at<=clock_timestamp() AND j.lease_expires_at>clock_timestamp() FROM session_archive_control c CROSS JOIN session_archive_jobs j WHERE c.singleton AND j.session_id='"<>uuid sealed.id<>"'")
          bracket_ (run "BEGIN; LOCK TABLE session_chunks IN ACCESS EXCLUSIVE MODE") unblock $ do
            execute observer "UPDATE session_archive_control SET permit_expires_at=clock_timestamp()+interval '3 seconds' WHERE singleton"
            withAsync (readAndStageArchiveSourceChunk env.pool lease 0 0 accept) $ \source -> do
              -- Release the real blocker before waiting or cancelling the source.
              (waitFor sourceBlockedWithLivePermit >> waitFor permitExpiredWithLiveJob) `finally` unblock
              timeout 5000000 (wait source) `shouldReturn` Just ArchiveSourceLedgerRejected
    boolean env "SELECT staged_bytes=0 AND NOT EXISTS(SELECT 1 FROM session_archive_ledger) FROM session_archive_control" `shouldReturn` True

  it "refuses incomplete output or manifest counts and retains raw evidence" $ \env -> do
    (w,s,l)<-claimed env "{}\n"
    reject $ publishArchive env.pool l verification
    stageArchivePart env.pool l 0 0 (BS.replicate 10 1) `shouldReturn` True
    stageArchiveCheckpoint env.pool l 0 0 10 0 "" `shouldReturn` True
    _<-stageArchiveLedger env.pool l 0 8
    reject $ publishArchive env.pool l verification
    stageArchivePart env.pool l 1 10 (BS.replicate 10 2) `shouldReturn` True
    reject $ publishArchive env.pool l (verification {checkpointCount=2})
    getPublishedArchive env.pool w s.id `shouldReturn` Nothing
    readSessionRange env.pool w s.id 0 10 >>= (\r->r.bytes `shouldBe` "{}\n")
    publishArchive env.pool l verification `shouldReturn` True

  it "fences expired renewal, stale publication and concurrent replacement attempts" $ \env -> do
    (w,s,old)<-claimed env "{}\n"
    stageComplete env old
    expire env s.id
    reject $ execute env ("UPDATE session_archive_jobs SET lease_expires_at=clock_timestamp()+interval '60 seconds' WHERE session_id='"<>uuid s.id<>"'")
    renewArchive env.pool old `shouldReturn` False
    publishArchive env.pool old verification `shouldReturn` False
    claimArchive env.pool "new-attempt" `shouldReturn` Nothing
    cleanupArchiveGeneration env.pool w s.id old.generationId `shouldReturn` 4
    cleanupArchiveGeneration env.pool w s.id old.generationId `shouldReturn` 1
    (a,b)<-concurrently (claimArchive env.pool "new-attempt") (claimArchive env.pool "new-attempt")
    let leases=[l | Just l<-[a,b]]
    length leases `shouldBe` 1
    let current=head leases
    (current.generationId/=old.generationId,current.leaseToken/=old.leaseToken,current.attempt) `shouldBe` (True,True,2)
    stageArchivePart env.pool old 2 20 "x" `shouldReturn` False
    cleanupArchiveGeneration env.pool w s.id current.generationId `shouldReturn` 0
    cleanupArchiveGeneration env.pool w s.id old.generationId `shouldReturn` 0
    stageComplete env current
    publishArchive env.pool current verification `shouldReturn` True
    cleanupArchiveGeneration env.pool w s.id current.generationId `shouldReturn` 0
    publishArchive env.pool old verification `shouldReturn` False

  it "supports renewable progress and retry backoff without exposing stale attempt progress" $ \env -> do
    (w,s,l)<-claimed env "{}\n"
    renewArchive env.pool l `shouldReturn` True
    archiveProgress env.pool l "verifying" 3 20 `shouldReturn` True
    releaseArchive env.pool l False "io_failure" `shouldReturn` True
    renewArchive env.pool l `shouldReturn` False
    Just j<-getArchiveJob env.pool w s.id
    (j.state,j.phase,j.rawProgress,j.compressedProgress,j.errorCode) `shouldBe` ("retry","queued",0,0,Just "io_failure")
    claimArchive env.pool "retry" `shouldReturn` Nothing
    -- Wait only the specified first backoff, then acquire a fresh generation.
    threadDelay 1100000
    cleanupArchiveGeneration env.pool w s.id l.generationId `shouldReturn` 1
    Just next<-claimArchive env.pool "retry"
    next.attempt `shouldBe` 2
    stageComplete env next
    releaseArchive env.pool next True "codec_failure" `shouldReturn` True
    Just failed<-getArchiveJob env.pool w s.id
    (failed.state,failed.generationId,failed.leaseToken) `shouldBe` ("failed",Nothing,Nothing)
    cleanupArchiveGeneration env.pool w s.id next.generationId `shouldReturn` 4
    cleanupArchiveGeneration env.pool w s.id next.generationId `shouldReturn` 1
    cleanupArchiveGeneration env.pool w s.id next.generationId `shouldReturn` 0
    getSession env.pool w s.id `shouldReturn` Just s
    readSessionRange env.pool w s.id 0 10 >>= (\r->r.bytes `shouldBe` "{}\n")
    threadDelay 10000
    claimArchive env.pool "retry" `shouldReturn` Nothing

  it "rejects ordinary mutation/deletion/truncation of published data and raw evidence" $ \env -> do
    (_,_,l)<-claimed env "{}\n"
    stageComplete env l
    publishArchive env.pool l verification `shouldReturn` True
    forM_ ["session_archive_jobs","session_archive_generations","session_archive_parts","session_archive_checkpoints","session_archive_ledger"] $ \table -> do
      reject $ execute env ("DELETE FROM "<>table)
      reject $ execute env ("TRUNCATE "<>table<>" CASCADE")
    forM_ ["UPDATE session_archive_jobs SET state='retry',phase='queued'","UPDATE session_archive_generations SET codec_implementation='other'","UPDATE session_archive_parts SET data=decode('ff','hex')","UPDATE session_archive_checkpoints SET unused_bits=1","UPDATE session_archive_ledger SET allow_final_record=false","DELETE FROM session_chunks","UPDATE sessions SET final_bytes=0"] $ \sql -> reject (execute env sql)
    -- The separately authorized fixture reset still works with all guards restored.
    cleanDB env
    boolean env "SELECT NOT EXISTS(SELECT 1 FROM session_archive_jobs) AND NOT EXISTS(SELECT 1 FROM pg_trigger WHERE tgname='session_evidence_no_truncate' AND tgenabled='D')" `shouldReturn` True

  it "publishes atomically for readers racing a blocked publisher" $ \env -> do
    (w,s,l)<-claimed env "{}\n"
    stageComplete env l
    -- A separate pool avoids recursively borrowing the held capability stripe.
    finished <- timeout 10000000 $ bracket (createPool env.testDb.testDbConnStr 1 30 30000) destroyAllResources $ \blockerPool -> withConn blockerPool $ \connection -> do
      let run sql=S.run (S.sql sql) connection >>= either (fail . show) pure
      blocker <- S.run (S.statement () (St.Statement "SELECT pg_backend_pid()" E.noParams (D.singleRow (D.column (D.nonNullable D.int4))) True)) connection >>= either (fail . show) pure
      bracket_
        (run ("BEGIN; SELECT session_id FROM session_archive_jobs WHERE session_id='"<>uuid s.id<>"' FOR UPDATE"))
        (run "ROLLBACK") $
        withAsync (publishArchive env.pool l verification) $ \publisher -> do
          waitForPublisher env blocker
          getPublishedArchive env.pool w s.id `shouldReturn` Nothing
          readArchivePart env.pool w s.id l.generationId 0 `shouldReturn` Nothing
          run "COMMIT"
          wait publisher `shouldReturn` True
    finished `shouldBe` Just ()
    getPublishedArchive env.pool w s.id >>= (\x->x `shouldSatisfy` (/=Nothing))
    readArchivePart env.pool w s.id l.generationId 0 >>= (\x->x `shouldSatisfy` (/=Nothing))

  it "upgrades V049 atomically, rolls back an injected ledger failure, and reruns without rewriting evidence" $ \_ ->
    withTestSandbox $ \sandbox -> withSandboxedEnv sandbox $ withSandboxedPostgres sandbox $ \db ->
      bracket (createPool db.testDbConnStr 3 30 30000) destroyAllResources $ \pool -> do
        let before=sandbox.sandboxRoot </> "before-v050"; only=sandbox.sandboxRoot </> "only-v050"
            filename="V050__sealed_session_archives.sql"; env=TestEnv pool sandbox db
        createDirectory before; createDirectory only
        files<-listDirectory sandbox.sandboxMigrationsDir
        forM_ (filter (\name->name<"V050" && ".sql" `T.isSuffixOf` T.pack name) files) $ \name->copyFile (sandbox.sandboxMigrationsDir </> name) (before </> name)
        copyFile (sandbox.sandboxMigrationsDir </> filename) (only </> filename)
        Migration.runMigrations pool before >>= (\r->r.failed `shouldBe` Nothing)
        (w,s)<-fixture env "{}\r\n" True
        execute env "CREATE TABLE before_archive_sessions AS SELECT to_jsonb(s) value FROM sessions s; CREATE TABLE before_archive_chunks AS SELECT to_jsonb(c) value FROM session_chunks c"
        bracket_
          (execute env "CREATE FUNCTION archive_ledger_failure() RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN IF NEW.version=50 THEN RAISE EXCEPTION 'injected archive ledger failure'; END IF; RETURN NEW; END; $$; CREATE TRIGGER archive_ledger_failure BEFORE INSERT ON schema_migrations FOR EACH ROW EXECUTE FUNCTION archive_ledger_failure()")
          (execute env "DROP TRIGGER archive_ledger_failure ON schema_migrations; DROP FUNCTION archive_ledger_failure()") $ do
            Migration.runMigrations pool only >>= (\r->r.failed `shouldSatisfy` (/=Nothing))
            boolean env "SELECT to_regclass('session_archive_jobs') IS NULL AND to_regclass('session_archives') IS NULL AND NOT EXISTS(SELECT 1 FROM schema_migrations WHERE version=50) AND NOT EXISTS(SELECT 1 FROM pg_proc WHERE proname='hmem_archive_binding')" `shouldReturn` True
        Migration.runMigrations pool only >>= (\r->r.applied `shouldBe` [filename])
        Migration.runMigrations pool only >>= (\r->r.applied `shouldBe` [])
        getSession pool w s.id `shouldReturn` Just s
        boolean env "SELECT NOT EXISTS(SELECT 1 FROM sessions s WHERE to_jsonb(s) NOT IN(SELECT value FROM before_archive_sessions)) AND NOT EXISTS(SELECT 1 FROM session_chunks c WHERE to_jsonb(c) NOT IN(SELECT value FROM before_archive_chunks))" `shouldReturn` True

-- Synthetic opaque output deliberately tests storage only; no test below claims
-- that this fixture is a valid gzip or substitutes for the Task3 decoder gate.
verification :: ArchiveVerification
verification=ArchiveVerification 20 (sessionSha256 (BS.replicate 10 1<>BS.replicate 10 2)) 2 1 1
stageComplete :: TestEnv -> ArchiveLease -> IO ()
stageComplete env l=do
  stageArchivePart env.pool l 0 0 (BS.replicate 10 1) `shouldReturn` True
  stageArchivePart env.pool l 1 10 (BS.replicate 10 2) `shouldReturn` True
  stageArchiveCheckpoint env.pool l 0 0 10 0 "" `shouldReturn` True
  stageArchiveLedger env.pool l 0 8 >>= (\x->x `shouldSatisfy` (/=Nothing))
claimed :: TestEnv -> ByteString -> IO (UUID,AgentSession,ArchiveLease)
claimed env body=do
  (w,s)<-fixture env body True
  _<-enqueueArchives env.pool Nothing 100
  Just l<-claimArchive env.pool "storage-test-v1"
  pure(w,s,l)
fixture :: TestEnv -> ByteString -> Bool -> IO (UUID,AgentSession)
fixture env body seal=do
  w<-(.id) <$> createTestWorkspace env "archive-test"
  task<-Task.createTask env.pool (Types.CreateTask w Nothing Nothing "Archive fixture" Nothing Nothing Nothing)
  execute env ("UPDATE tasks SET status='in_progress' WHERE id='"<>uuid task.id<>"'")
  identifier<-UUID.nextRandom
  initial<-createSession env.pool (CreateSession identifier w "jsonl" "Archive fixture" Nothing Nothing task.id)
  row<-if BS.null body then pure initial else (.session) <$> appendSession env.pool w identifier (AppendSession 0 0 body True)
  final<-if seal then terminateSession env.pool w identifier (TerminateSession row.committedBytes row.revision row.committedBytes (sessionSha256 body)) else pure row
  pure(w,final)
expire :: TestEnv -> UUID -> IO ()
expire env identifier=do
  execute env ("UPDATE session_archive_jobs SET lease_expires_at=clock_timestamp()+interval '50 milliseconds' WHERE session_id='"<>uuid identifier<>"'")
  threadDelay 80000
waitForPublisher :: TestEnv -> Int32 -> IO ()
waitForPublisher env blocker=do
  let blocked=runSession env.pool $ S.statement blocker $ St.Statement
        "SELECT EXISTS(SELECT 1 FROM pg_stat_activity a WHERE a.wait_event_type='Lock' AND pg_blocking_pids(a.pid) @> ARRAY[$1::int4] AND a.query LIKE 'SELECT session_id FROM session_archive_jobs WHERE workspace_id=% FOR UPDATE')"
        (E.param (E.nonNullable E.int4)) (D.singleRow (D.column (D.nonNullable D.bool))) False
      loop=do observed<-blocked; if observed then pure () else threadDelay 10000 >> loop
  timeout 5000000 loop `shouldReturn` Just ()
binding :: ArchiveLease -> ByteString
binding l="'"<>uuid l.generationId<>"','"<>uuid l.sessionId<>"','"<>uuid l.workspaceId<>"',"
uuid :: UUID -> ByteString
uuid=B8.pack . show
execute :: TestEnv -> ByteString -> IO ()
execute env sql=runSession env.pool (S.sql sql)
boolean :: TestEnv -> ByteString -> IO Bool
boolean env sql=runSession env.pool $ S.statement () (St.Statement sql E.noParams (D.singleRow (D.column (D.nonNullable D.bool))) False)
reject :: IO a -> IO ()
reject action=do
  result<-try @DBException action
  case result of
    Left (DBCheckViolation _) -> pure ()
    Left (DBForeignKeyViolation _) -> pure ()
    other->expectationFailure ("Expected guarded SQL rejection, got "<>either show (const "success") other)
