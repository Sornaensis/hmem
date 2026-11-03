module HMem.DB.DocumentSpec (spec) where

import Control.Concurrent.Async (concurrently)
import Control.Exception (SomeException,try,bracket,bracket_,mask,finally)
import Control.Concurrent (threadDelay)
import Control.Monad (forM_,unless,when)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as B8
import Data.Either (isRight)
import Data.Pool (Pool,destroyAllResources)
import Data.IORef
import Data.Text qualified as T
import Data.UUID (UUID)
import Data.UUID.V4 qualified as UUID
import Hasql.Decoders qualified as D
import Hasql.Connection qualified as H
import Hasql.Encoders qualified as E
import Hasql.Session qualified as S
import Hasql.Statement qualified as St
import Test.Hspec
import System.Directory (findExecutable,createDirectory,createDirectoryIfMissing,listDirectory,copyFile,doesFileExist,canonicalizePath,removeDirectoryRecursive)
import System.FilePath ((</>),takeDirectory)
import System.IO.Temp (withTempDirectory,createTempDirectory)
import System.Process (callProcess,createProcess,proc,CreateProcess(..),StdStream(..),ProcessHandle,waitForProcess,getProcessExitCode,readProcessWithExitCode)
import System.Exit (ExitCode(..))
import System.Random (randomRIO)
import System.Timeout (timeout)
import HMem.AgentSession
import HMem.DB.AgentSession qualified as Sessions
import HMem.DB.Migration qualified as Migration
import HMem.DB.Document
import HMem.DB.Pool (runSession,createPool,withConn)
import HMem.DB.Task qualified as Task
import HMem.DB.TestHarness
import HMem.Document.Types
import HMem.Document.Codec
import HMem.Types qualified as Types

spec :: Spec
spec = around withTestEnv $ describe "Task documents" $ do
  it "publishes originals atomically with independent document attribution and workspace-only dedup" $ \env -> do
    (w,t,input)<-fixture env "exact\r\noriginal"
    first<-uploadDocument env.pool input "exact\r\noriginal"
    readOriginal env.pool w t first.id `shouldReturn` "exact\r\noriginal"
    again<-uploadDocument env.pool input "exact\r\noriginal"
    again `shouldBe` first
    request<-UUID.nextRandom
    second<-uploadDocument env.pool (input{manifest=input.manifest,requestId=request}) "exact\r\noriginal"
    second.id `shouldNotBe` first.id
    second.payloadId `shouldBe` first.payloadId
    getTaskDocumentCounts env.pool w t `shouldReturn` Just(DocumentCounts 2 0 0 0)
    boolean env "SELECT count(*)=1 FROM document_payloads" `shouldReturn` True
    boolean env "SELECT NOT EXISTS(SELECT 1 FROM document_reservations)" `shouldReturn` True
    other<-(.id) <$> createTestWorkspace env "other"
    task<-Task.createTask env.pool (Types.CreateTask other Nothing Nothing "other" Nothing Nothing Nothing)
    foreignRequest<-UUID.nextRandom
    foreignDoc<-uploadDocument env.pool (input{workspaceId=other,taskId=task.id,requestId=foreignRequest}) "exact\r\noriginal"
    foreignDoc.payloadId `shouldNotBe` first.payloadId
  it "binds exact retries to the full manifest and placement and rejects mismatched input without publication" $ \env -> do
    (w,t,input)<-fixture env "bytes"
    _<-uploadDocument env.pool input "bytes"
    reject $ uploadDocument env.pool (input{manifest=input.manifest,role=Evidence}) "bytes"
    reject $ uploadDocument env.pool (input{manifest=OriginalManifest input.manifest.rawBytes input.manifest.rawSha256 input.manifest.filename "changed" input.manifest.mediaType}) "bytes"
    another<-UUID.nextRandom
    reject $ uploadDocument env.pool (input{manifest=input.manifest,requestId=another}) "wrong"
    getTaskDocumentCounts env.pool w t `shouldReturn` Just(DocumentCounts 1 0 0 0)
  it "keeps an exact reservation retry stable after a different request publishes the same payload" $ \env -> do
    (w,t,input)<-fixture env "deduplicated"
    reserveUpload env.pool input `shouldReturn` Nothing
    other<-UUID.nextRandom
    _<-uploadDocument env.pool (input{manifest=input.manifest,requestId=other}) "deduplicated"
    reserveUpload env.pool input `shouldReturn` Nothing
    _<-publishUpload env.pool input "deduplicated"
    getTaskDocumentCounts env.pool w t `shouldReturn` Just(DocumentCounts 2 0 0 0)
  it "retains immutable evidence after source removal and task soft-delete/restore, including completed tasks" $ \env -> do
    (w,t,input)<-fixture env "durable"
    execute env ("UPDATE tasks SET status='done' WHERE id='"<>uuid t<>"'")
    document<-uploadDocument env.pool input "durable"
    request<-UUID.nextRandom
    _<-addEvidence env.pool w t document.id request
    _<-addEvidence env.pool w t document.id request
    removeSource env.pool w t document.id
    getTaskDocumentCounts env.pool w t `shouldReturn` Just(DocumentCounts 0 1 0 1)
    readOriginal env.pool w t document.id `shouldReturn` "durable"
    reject $ execute env "UPDATE task_document_evidence SET snapshot='{}'"
    reject $ execute env "DELETE FROM documents"
    reject $ execute env "UPDATE document_payload_parts SET data='corrupt'"
    execute env ("INSERT INTO document_payloads(id,workspace_id,codec,raw_bytes,raw_sha256,compressed_bytes,compressed_sha256,part_count,original) VALUES(gen_random_uuid(),'"<>uuid w<>"','gzip-v1',0,repeat('0',64),20,repeat('0',64),1,false)")
    reject $ execute env "UPDATE document_payload_parts SET payload_id=(SELECT id FROM document_payloads WHERE published_at IS NULL) WHERE payload_id=(SELECT payload_id FROM documents LIMIT 1)"
    readOriginal env.pool w t document.id `shouldReturn` "durable"
    execute env ("UPDATE tasks SET deleted_at=clock_timestamp() WHERE id='"<>uuid t<>"'")
    getDocument env.pool w t document.id `shouldReturn` Nothing
    getTaskDocumentCounts env.pool w t `shouldReturn` Nothing
    execute env ("UPDATE tasks SET deleted_at=NULL WHERE id='"<>uuid t<>"'")
    readOriginal env.pool w t document.id `shouldReturn` "durable"
  it "reserves before consuming input, accounts concurrent quotas and expires only unpublished work" $ \env -> do
    (w,_,input)<-fixture env "budget"
    _<-reserveUpload env.pool input
    execute env ("UPDATE document_budgets SET staging_limit="<>B8.pack(show(uploadReservationBytes 6))<>" WHERE workspace_id='"<>uuid w<>"'")
    req<-UUID.nextRandom
    reject $ reserveUpload env.pool (input{manifest=input.manifest,requestId=req})
    boolean env "SELECT NOT EXISTS(SELECT 1 FROM documents)" `shouldReturn` True
    execute env "UPDATE document_reservations SET expires_at=clock_timestamp()-interval '1 second'"
    expireReservations env.pool w
    boolean env "SELECT NOT EXISTS(SELECT 1 FROM document_reservations)" `shouldReturn` True
    _<-reserveUpload env.pool (input{manifest=input.manifest,requestId=req})
    abortReservation env.pool w req
    boolean env "SELECT NOT EXISTS(SELECT 1 FROM document_reservations)" `shouldReturn` True
  it "serializes competing quota reservations and exact concurrent publication" $ \env -> do
    (w,_,input)<-fixture env "racing"
    _<-reserveUpload env.pool input
    abortReservation env.pool w input.requestId
    execute env ("UPDATE document_budgets SET raw_limit=6 WHERE workspace_id='"<>uuid w<>"'")
    req<-UUID.nextRandom
    let reserve :: UploadDocument -> IO(Either SomeException(Maybe Document))
        reserve i=try(reserveUpload env.pool i)
    (a,b)<-concurrently (reserve input) (reserve(input{manifest=input.manifest,requestId=req}))
    length(filter isRight[a,b]) `shouldBe` 1
    execute env "DELETE FROM document_reservations"
    (first,second)<-concurrently (uploadDocument env.pool input "racing") (uploadDocument env.pool input "racing")
    first.id `shouldBe` second.id
    boolean env "SELECT count(*)=1 FROM documents" `shouldReturn` True
  it "rolls back payloads, documents, links and events when publication fails" $ \env -> do
    (_,_,input)<-fixture env "atomic"
    _<-reserveUpload env.pool input
    execute env "CREATE FUNCTION document_test_failure() RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN RAISE EXCEPTION 'injected publication failure'; END; $$; CREATE TRIGGER document_test_failure BEFORE INSERT ON document_requests FOR EACH ROW EXECUTE FUNCTION document_test_failure()"
    reject $ publishUpload env.pool input "atomic"
    boolean env "SELECT NOT EXISTS(SELECT 1 FROM documents) AND NOT EXISTS(SELECT 1 FROM document_payloads) AND NOT EXISTS(SELECT 1 FROM task_document_sources) AND NOT EXISTS(SELECT 1 FROM audit_log WHERE entity_type='task_document')" `shouldReturn` True
    execute env "DROP TRIGGER document_test_failure ON document_requests; DROP FUNCTION document_test_failure()"
    _<-publishUpload env.pool input "atomic"
    pure()
  it "paginates metadata separately from canonical totals and isolates workspace/task links" $ \env -> do
    (w,t,input)<-fixture env "same payload"
    forM_ [1::Int .. 3] $ \_ -> do req<-UUID.nextRandom; _<-uploadDocument env.pool (input{manifest=input.manifest,requestId=req,role=Evidence}) "same payload"; pure()
    first<-listDocuments env.pool w t Evidence Nothing 2
    length first.items `shouldBe` 2
    second<-listDocuments env.pool w t Evidence first.nextCursor 2
    length second.items `shouldBe` 1
    getTaskDocumentCounts env.pool w t `shouldReturn` Just(DocumentCounts 0 3 0 3)
    other<-(.id) <$> createTestWorkspace env "foreign"
    reject $ reserveUpload env.pool (input{manifest=input.manifest,workspaceId=other})
    case first.items of
      item:_ -> getDocument env.pool other t item.document.id `shouldReturn` Nothing
      [] -> expectationFailure "Expected an evidence page"
  it "counts3documents+27terminated sessions as30 across pages, excluding active/source-only/descendant evidence" $ \env -> do
    (w,t,input)<-fixture env "evidence"
    forM_ [1::Int .. 3] $ \_ -> do request<-UUID.nextRandom; _<-uploadDocument env.pool (input{manifest=input.manifest,requestId=request,role=Evidence}) "evidence"; pure()
    request<-UUID.nextRandom
    _<-uploadDocument env.pool (input{manifest=input.manifest,requestId=request}) "evidence"
    execute env ("UPDATE tasks SET status='in_progress' WHERE id='"<>uuid t<>"'")
    let createAt target = do identifier<-UUID.nextRandom; Sessions.createSession env.pool(CreateSession identifier w "jsonl" "Count fixture" Nothing Nothing target)
        terminate row = Sessions.terminateSession env.pool w row.id(TerminateSession row.committedBytes row.revision 0(payloadSha256 ""))
    forM_ [1::Int .. 27] $ \_ -> createAt t >>= terminate
    _<-createAt t
    child<-Task.createTask env.pool(Types.CreateTask w Nothing (Just t) "child" Nothing Nothing Nothing)
    execute env("UPDATE tasks SET status='in_progress' WHERE id='"<>uuid child.id<>"'")
    _<-createAt child.id >>= terminate
    getTaskDocumentCounts env.pool w t `shouldReturn` Just(DocumentCounts 1 3 27 30)
  it "keeps originals when preview fails and publishes a whole verified generation only" $ \env -> do
    (w,t,input)<-fixture env "original"
    document<-uploadDocument env.pool input "original"
    generation<-UUID.nextRandom
    reservePreview env.pool w document.id generation "test-policy-v1" 2 4096
    reject $ publishPreview env.pool w document.id generation "test-policy-v1" [Derivative "page" 1 "image/png" "page"]
    reject $ readDerivative env.pool w t document.id "page" 1
    publishPreview env.pool w document.id generation "test-policy-v1" [Derivative "page" 1 "image/png" "page",Derivative "thumbnail" 1 "image/png" "thumb"]
    (_,bytes)<-readDerivative env.pool w t document.id "page" 1
    bytes `shouldBe` "page"
    readOriginal env.pool w t document.id `shouldReturn` "original"
    failPreview env.pool w document.id (Just generation) "renderer_failed" `shouldReturn` False
    readOriginal env.pool w t document.id `shouldReturn` "original"
    reject $ execute env "DELETE FROM document_generations"
  it "fences older preview publication, exact re-reservation and failure after a newer generation wins" $ \env -> do
    (w,t,input)<-fixture env "original"
    document<-uploadDocument env.pool input "original"
    old<-UUID.nextRandom; newer<-UUID.nextRandom
    reservePreview env.pool w document.id old "old-policy" 1 4096
    reservePreview env.pool w document.id newer "new-policy" 1 4096
    publishPreview env.pool w document.id newer "new-policy" [Derivative "page" 1 "image/png" "new page"]
    reject $ publishPreview env.pool w document.id old "old-policy" [Derivative "page" 1 "image/png" "old page"]
    reject $ reservePreview env.pool w document.id old "old-policy" 1 4096
    failPreview env.pool w document.id (Just old) "old_failure" `shouldReturn` False
    (_,bytes)<-readDerivative env.pool w t document.id "page" 1
    bytes `shouldBe` "new page"
    abortReservation env.pool w old
    reject $ reservePreview env.pool w document.id old "old-policy" 1 4096
    (_,stillNew)<-readDerivative env.pool w t document.id "page" 1
    stillNew `shouldBe` "new page"
    expired<-UUID.nextRandom
    reservePreview env.pool w document.id expired "retry-policy" 1 4096
    execute env("UPDATE document_reservations SET expires_at=clock_timestamp()-interval '1 second' WHERE id='"<>uuid expired<>"'")
    reject $ reservePreview env.pool w document.id expired "retry-policy" 1 4096
    expireReservations env.pool w
    reject $ reservePreview env.pool w document.id expired "retry-policy" 1 4096
    winner<-UUID.nextRandom
    reservePreview env.pool w document.id winner "winning-policy" 1 4096
    publishPreview env.pool w document.id winner "winning-policy" [Derivative "page" 1 "image/png" "winner page"]
    reject $ reservePreview env.pool w document.id expired "retry-policy" 1 4096
    (_,afterExpiry)<-readDerivative env.pool w t document.id "page" 1
    afterExpiry `shouldBe` "winner page"
  it "preserves originals through independent service pools and a real PostgreSQL backup/restore" $ \env -> do
    tools<-mapM findExecutable ["pg_dump","pg_restore","createdb","dropdb"]
    if any (==Nothing) tools then pendingWith "PostgreSQL backup tools unavailable" else do
      (w,t,input)<-fixture env "restored\r\nexact"
      document<-uploadDocument env.pool input "restored\r\nexact"
      bracket(createPool env.testDb.testDbConnStr 1 30 30000) destroyAllResources $ \peer ->
        readOriginal peer w t document.id `shouldReturn` "restored\r\nexact"
      withTempDirectory env.testSandbox.sandboxTmpDir "document-recovery-" $ \scratch -> do
        identifier<-UUID.nextRandom
        let dbName="doc_"<>filter (/='-')(show identifier)
            conn=T.unpack env.testDb.testDbConnStr
            restored=env.testDb.testDbConnStr<>" dbname="<>T.pack dbName
            archive=scratch </> "documents.dump"
        callProcess "pg_dump" ["--dbname",conn,"--format=custom","--file",archive]
        bracket_
          (callProcess "createdb" ["--maintenance-db",conn,dbName])
          (callProcess "dropdb" ["--maintenance-db",conn,dbName]) $ do
            callProcess "pg_restore" ["--dbname",T.unpack restored,"--no-owner","--no-acl",archive]
            bracket(createPool restored 1 30 30000) destroyAllResources $ \recovered -> do
              readOriginal recovered w t document.id `shouldReturn` "restored\r\nexact"
              getTaskDocumentCounts recovered w t `shouldReturn` Just(DocumentCounts 1 0 0 0)
  it "rolls V058 DDL back with a failed ledger write and safely retries the ordered migration" $ \_ ->
    withTestSandbox $ \sandbox -> withSandboxedPostgres sandbox $ \db ->
      bracket(createPool db.testDbConnStr 2 30 30000) destroyAllResources $ \pool -> do
        source<-resolveMigrationsDir sandbox.sandboxRepoRoot
        let beforeDir=sandbox.sandboxTmpDir </> "before-v058"
            upgrade=sandbox.sandboxTmpDir </> "only-v058"
            filename="V058__task_documents.sql"
            env=TestEnv pool sandbox db
        createDirectory beforeDir; createDirectory upgrade
        files<-listDirectory source
        forM_ (filter(\name -> name<"V058" && ".sql" `T.isSuffixOf` T.pack name) files) $ \name -> copyFile(source </> name)(beforeDir </> name)
        copyFile(source </> filename)(upgrade </> filename)
        Migration.runMigrations pool beforeDir >>= (\result -> result.failed `shouldBe` Nothing)
        execute env "CREATE FUNCTION document_ledger_failure() RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN IF NEW.version=58 THEN RAISE EXCEPTION 'injected ledger failure'; END IF; RETURN NEW; END; $$; CREATE TRIGGER document_ledger_failure BEFORE INSERT ON schema_migrations FOR EACH ROW EXECUTE FUNCTION document_ledger_failure()"
        Migration.runMigrations pool upgrade >>= (\result -> result.failed `shouldSatisfy` (/=Nothing))
        boolean env "SELECT to_regclass('documents') IS NULL AND NOT EXISTS(SELECT 1 FROM schema_migrations WHERE version=58)" `shouldReturn` True
        execute env "DROP TRIGGER document_ledger_failure ON schema_migrations; DROP FUNCTION document_ledger_failure()"
        Migration.runMigrations pool upgrade >>= (\result -> result.applied `shouldBe` [filename])
        Migration.runMigrations pool upgrade >>= (\result -> result.applied `shouldBe` [])
  it "retains immutable originals and links across an actual owned PostgreSQL process restart" $ \env ->
    withRestartCluster env $ \pg active start -> do
      ownedProcess<-readIORef active >>= maybe(fail "Missing owned PostgreSQL handle") pure
      -- Exercise the same read-only mismatch gate used before DB creation.
      createDirectory(pg.epTmpDir </> "different-initialization")
      mutationReached<-newIORef False
      reject $ withVerifiedRestartConnection pg (pg.epTmpDir </> "different-initialization") ownedProcess (\_ -> writeIORef mutationReached True)
      readIORef mutationReached `shouldReturn` False
      source<-resolveMigrationsDir env.testSandbox.sandboxRepoRoot
      (w,t,identifier)<-withVerifiedRestartPool pg active $ \pool -> do
        Migration.runMigrations pool source >>= (\result -> result.failed `shouldBe` Nothing)
        let db=TestDb pg.epDataDir pg.epLogFile pg.epPort pg.epDbName pg.epConnStr False
            local=TestEnv pool env.testSandbox db
        (workspace,task,input)<-fixture local "restart\r\noriginal"
        document<-uploadDocument pool input "restart\r\noriginal"
        pure(workspace,task,document.id)
      stopRestartProcess pg active
      start
      withVerifiedRestartPool pg active $ \pool -> do
        readOriginal pool w t identifier `shouldReturn` "restart\r\noriginal"
        getTaskDocumentCounts pool w t `shouldReturn` Just(DocumentCounts 1 0 0 0)
  it "enforces per-task links and emits only metadata in audit/change projections" $ \env -> do
    (w,_,input)<-fixture env "secret-original-bytes"
    _<-uploadDocument env.pool input "secret-original-bytes"
    execute env ("UPDATE document_budgets SET task_link_limit=1 WHERE workspace_id='"<>uuid w<>"'")
    req<-UUID.nextRandom
    reject $ uploadDocument env.pool (input{manifest=input.manifest,requestId=req}) "secret-original-bytes"
    boolean env "SELECT NOT EXISTS(SELECT 1 FROM audit_log WHERE entity_type='task_document' AND (new_values::text LIKE '%secret-original-bytes%' OR new_values::text LIKE '%payload%'))" `shouldReturn` True

fixture :: TestEnv -> ByteString -> IO(UUID,UUID,UploadDocument)
fixture env bytes = do
  workspace<-(.id) <$> createTestWorkspace env "documents"
  task<-Task.createTask env.pool (Types.CreateTask workspace Nothing Nothing "task" Nothing Nothing Nothing)
  request<-UUID.nextRandom
  pure(workspace,task.id,UploadDocument workspace task.id request Source (OriginalManifest (fromIntegral(BS.length bytes)) (payloadSha256 bytes) "notes.txt" "Notes" "text/plain"))
execute :: TestEnv -> ByteString -> IO()
execute env sql=runSession env.pool (S.sql sql)
boolean :: TestEnv -> ByteString -> IO Bool
boolean env sql=runSession env.pool(S.statement () (St.Statement sql E.noParams (D.singleRow(D.column(D.nonNullable D.bool))) False))
uuid :: UUID -> ByteString
uuid=B8.pack . show
reject :: IO a -> IO()
reject action=do result<-try(action>>pure())::IO(Either SomeException()); result `shouldSatisfy` either (const True) (const False)

-- The ordinary suite database is never stopped. This one test owns a separate
-- contained native cluster, both exact postmaster handles and its control
-- helpers. A failed join retains the cluster instead of deleting live data.
withRestartCluster :: TestEnv -> (EphemeralPg -> IORef(Maybe ProcessHandle) -> IO() -> IO a) -> IO a
withRestartCluster env action=mask $ \restore -> do
  let ownerRoot=env.testSandbox.sandboxRepoRoot </> ".tmp" </> "task-documents-41ca722d"
  createDirectoryIfMissing True ownerRoot
  ownedParent<-canonicalizePath ownerRoot
  root<-createTempDirectory ownerRoot "restart-"
  active<-newIORef Nothing
  port<-randomRIO(49152,65535)
  databaseIdentifier<-UUID.nextRandom
  let dataDir=root </> "data"
      databaseName="hmem_document_"<>T.filter(/='-')(T.pack(show databaseIdentifier))
      conn="host=localhost port="<>T.pack(show port)<>" dbname="<>databaseName<>" connect_timeout=2"
      pg=EphemeralPg root dataDir port conn (root </> "postgres.log") databaseName Nothing
      start=mask $ \unmask -> do
        (_,_,_,process)<-createProcess(proc "postgres" ["-D",dataDir,"-c","logging_collector=off"]){std_in=NoStream,std_out=NoStream,std_err=NoStream}
        writeIORef active(Just process)
        let ready 0=fail "Owned restart PostgreSQL did not become ready"
            ready attempts=do
              ended<-getProcessExitCode process
              when(ended/=Nothing)(fail "Owned restart PostgreSQL exited before readiness")
              (code,_,_)<-readProcessWithExitCode "pg_isready" ["-h","localhost","-p",show port] ""
              unless(code==ExitSuccess)(threadDelay 100000 >> ready(attempts-1))
        outcome<-unmask(timeout 10000000(ready(100::Int)))
        when(outcome==Nothing)(fail "Owned restart PostgreSQL readiness exceeded10seconds")
        -- A colliding port can answer pg_isready while our child is still
        -- failing startup. Prove the server identity before createdb/migrations.
        unmask(verifyRestartOwnership pg dataDir process)
      release=do
        stopRestartProcess pg active
        resolved<-canonicalizePath root
        unless(takeDirectory resolved==ownedParent)(fail "Refusing restart scratch cleanup outside owned task directory")
        removeDirectoryRecursive resolved
      run=do
        callProcess "initdb" ["-D",dataDir,"--auth=trust","--no-instructions","--no-locale","-E","UTF8"]
        appendFile(dataDir </> "postgresql.conf")(unlines["port = "<>show port,"listen_addresses = 'localhost'","logging_collector = off"])
        start
        process<-readIORef active >>= maybe(fail "Missing owned PostgreSQL handle") pure
        -- CREATE DATABASE cannot run inside a transaction. Use the same native
        -- maintenance connection whose identity was just checked, not a fresh
        -- external createdb connection to an unverified port.
        withVerifiedRestartConnection pg dataDir process $ \connection -> do
          result<-S.run(S.sql(B8.pack("CREATE DATABASE "<>T.unpack databaseName))) connection
          either (fail . show) pure result
        action pg active start
  restore run `finally` release

verifyRestartOwnership :: EphemeralPg -> FilePath -> ProcessHandle -> IO()
verifyRestartOwnership pg expectedData process=withVerifiedRestartConnection pg expectedData process(const(pure()))

withVerifiedRestartConnection :: EphemeralPg -> FilePath -> ProcessHandle -> (H.Connection -> IO a) -> IO a
withVerifiedRestartConnection pg expectedData process action=do
  let maintenance="host=localhost port="<>T.pack(show pg.epPort)<>" dbname=postgres connect_timeout=2"
  bracket(createPool maintenance 1 5 1000) destroyAllResources $ \probe -> withConn probe $ \connection -> do
    verifyRestartConnection expectedData process connection
    action connection

verifyRestartConnection :: FilePath -> ProcessHandle -> H.Connection -> IO()
verifyRestartConnection expectedData process connection=do
  getProcessExitCode process >>= \ended -> when(ended/=Nothing)(fail "Owned PostgreSQL is not live")
  identity<-S.run(S.statement ()(St.Statement "SHOW data_directory" E.noParams(D.singleRow(D.column(D.nonNullable D.text))) False)) connection
  actual<-either (fail . show) pure identity
  expected<-canonicalizePath expectedData
  observed<-canonicalizePath(T.unpack actual)
  unless(expected==observed)(fail "Readiness belongs to a different PostgreSQL initialization; refusing all database writes")
  getProcessExitCode process >>= \ended -> when(ended/=Nothing)(fail "Owned PostgreSQL exited during identity verification")

withVerifiedRestartPool :: EphemeralPg -> IORef(Maybe ProcessHandle) -> (Pool H.Connection -> IO a) -> IO a
withVerifiedRestartPool pg active action=bracket(createPool pg.epConnStr 1 30 30000) destroyAllResources $ \pool -> do
  process<-readIORef active >>= maybe(fail "Missing owned PostgreSQL handle") pure
  -- A UUID-unique database prevents a later fresh connection from attaching an
  -- unrelated same-name database. Verify the initial physical connection too.
  withConn pool (verifyRestartConnection pg.epDataDir process)
  action pool

stopRestartProcess :: EphemeralPg -> IORef(Maybe ProcessHandle) -> IO()
stopRestartProcess pg active=mask $ \restore -> readIORef active >>= \case
  Nothing -> pure()
  Just process -> do
    ended<-getProcessExitCode process
    case ended of
      Nothing -> restore(callProcess "pg_ctl" ["stop","-D",pg.epDataDir,"-m","fast","-w","-t","5"])
      Just _ -> pure()
    outcome<-restore(timeout 5000000(waitForProcess process))
    unless(outcome==Just ExitSuccess)(fail("Owned PostgreSQL join unproven; retained "<>pg.epTmpDir))
    remains<-doesFileExist(pg.epDataDir </> "postmaster.pid")
    when remains(fail("Owned PostgreSQL PID file remained; retained "<>pg.epTmpDir))
    writeIORef active Nothing
