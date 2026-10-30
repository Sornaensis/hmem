module HMem.DB.SessionOccurrenceSpec (spec) where

import Control.Concurrent.Async (concurrently)
import Control.Exception (bracket)
import Control.Monad (forM_)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as B8
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
import Test.Hspec
import HMem.AgentSession
import HMem.AgentSession.Occurrence qualified as O
import HMem.DB.AgentSession
import HMem.DB.SessionOccurrence qualified as Projection
import HMem.DB.Migration qualified as Migration
import HMem.DB.Pool (createPool, runSession)
import HMem.DB.Task qualified as Task
import HMem.DB.TestHarness
import HMem.Types qualified as Types

spec :: Spec
spec = around withTestEnv $ describe "Session occurrence projection" $ do
  it "publishes append maxima in list/detail and keeps replay, heartbeat, runtime and seal independent" $ \env -> do
    (w,row,task) <- fixture env
    let newer=event "2026-10-30T12:00:00Z" "newer"
        older=event "2026-10-30T11:00:00Z" "older"
        input=AppendSession 0 0 newer True
    first <- (.session) <$> appendSession env.pool w row.id input
    first.lastActivityAt `shouldBe` O.parseOccurrenceTime "2026-10-30T12:00:00Z"
    second <- (.session) <$> appendSession env.pool w row.id (AppendSession first.committedBytes first.revision ("\n"<>older) True)
    second.lastActivityAt `shouldBe` first.lastActivityAt
    replay <- appendSession env.pool w row.id input
    replay.session.lastActivityAt `shouldBe` first.lastActivityAt
    heart <- heartbeatSession env.pool w row.id
    heart.lastActivityAt `shouldBe` first.lastActivityAt
    runtime <- setRuntimeIdentity env.pool w row.id (SetRuntimeIdentity 0 (Just (RuntimeIdentity "model" Nothing Nothing (Just "developer") Nothing)))
    runtime.lastActivityAt `shouldBe` first.lastActivityAt
    page <- listTaskSessions env.pool w task Nothing 50
    map (.lastActivityAt) page.items `shouldBe` [first.lastActivityAt]
    sealed <- terminateSession env.pool w row.id (TerminateSession second.committedBytes second.revision second.committedBytes (sessionSha256 (newer<>"\n"<>older)))
    sealed.lastActivityAt `shouldBe` first.lastActivityAt
    getSession env.pool w row.id `shouldReturn` Just sealed
    foreignWorkspace <- (.id) <$> createTestWorkspace env "occurrence-foreign"
    getSession env.pool foreignWorkspace row.id `shouldReturn` Nothing
    Projection.checkpoint env.pool foreignWorkspace row.id `shouldReturn` Nothing
    execute env ("UPDATE session_occurrences SET derivation_version=2 WHERE session_id='"<>uuid row.id<>"'")
    getSession env.pool w row.id >>= (\value -> fmap (.lastActivityAt) value `shouldBe` Just Nothing)
    deriveSessionOccurrence env.pool w row.id
    getSession env.pool w row.id `shouldReturn` Just sealed

  it "keeps partial backfills unavailable and resumes split records without audit or canonical-row writes" $ \env -> do
    (w,row,_) <- fixture env
    let pad="{\"type\":\"metadata\",\"padding\":\""<>BS.replicate 600000 120<>"\"}\n"
        activity=event "2026-10-30T12:00:00Z" "backfilled"
    first <- (.session) <$> appendSession env.pool w row.id (AppendSession 0 0 pad False)
    second <- (.session) <$> appendSession env.pool w row.id (AppendSession first.committedBytes first.revision (pad<>activity) True)
    execute env ("DELETE FROM session_occurrences WHERE session_id='"<>uuid row.id<>"'")
    execute env "CREATE TABLE occurrence_before AS SELECT to_jsonb(s) value FROM sessions s; CREATE TABLE occurrence_audit_before AS SELECT count(*) n FROM audit_log; CREATE TABLE occurrence_stream_before AS SELECT count(*) n FROM change_stream_outbox"
    getSession env.pool w row.id >>= (\value -> fmap (.lastActivityAt) value `shouldBe` Just Nothing)
    boolean env "SELECT (SELECT count(*) FROM change_stream_outbox)=(SELECT n FROM occurrence_stream_before)" `shouldReturn` True
    deriveSessionOccurrence env.pool w row.id
    Just pending <- Projection.checkpoint env.pool w row.id
    pending.processedBytes `shouldBe` 1048576
    BS.length pending.pending `shouldSatisfy` (>0)
    getSession env.pool w row.id >>= (\value -> fmap (.lastActivityAt) value `shouldBe` Just Nothing)
    boolean env "SELECT (SELECT count(*) FROM change_stream_outbox)=(SELECT n FROM occurrence_stream_before)" `shouldReturn` True
    deriveSessionOccurrence env.pool w row.id
    Just done <- getSession env.pool w row.id
    done.lastActivityAt `shouldBe` O.parseOccurrenceTime "2026-10-30T12:00:00Z"
    done.committedBytes `shouldBe` second.committedBytes
    boolean env "SELECT (SELECT count(*) FROM change_stream_outbox)=(SELECT n+1 FROM occurrence_stream_before) AND NOT EXISTS(SELECT 1 FROM change_stream_outbox WHERE envelope::text LIKE '%backfilled%')" `shouldReturn` True
    boolean env "SELECT (SELECT count(*) FROM audit_log)=(SELECT n FROM occurrence_audit_before) AND NOT EXISTS((SELECT to_jsonb(s) FROM sessions s EXCEPT TABLE occurrence_before) UNION ALL (TABLE occurrence_before EXCEPT SELECT to_jsonb(s) FROM sessions s))" `shouldReturn` True
    deriveSessionOccurrence env.pool w row.id
    boolean env "SELECT (SELECT count(*) FROM change_stream_outbox)=(SELECT n+1 FROM occurrence_stream_before)" `shouldReturn` True

  it "fences concurrent append/backfill/seal and stale checkpoint retries" $ \env -> do
    (w,row,_) <- fixture env
    let a=event "2026-10-30T11:00:00Z" "a"<>"\n"
        b=event "2026-10-30T12:00:00Z" "b"<>"\n"
    first <- (.session) <$> appendSession env.pool w row.id (AppendSession 0 0 a False)
    execute env ("DELETE FROM session_occurrences WHERE session_id='"<>uuid row.id<>"'")
    (_,appended) <- concurrently (deriveSessionOccurrence env.pool w row.id) (appendSession env.pool w row.id (AppendSession first.committedBytes first.revision b False))
    _ <- concurrently (deriveSessionOccurrence env.pool w row.id)
      (terminateSession env.pool w row.id (TerminateSession appended.session.committedBytes appended.session.revision appended.session.committedBytes (sessionSha256 (a<>b))))
    deriveSessionOccurrence env.pool w row.id
    Just done <- getSession env.pool w row.id
    done.lastActivityAt `shouldBe` O.parseOccurrenceTime "2026-10-30T12:00:00Z"
    done.state `shouldBe` Terminated
    -- A stale zero-cursor writer cannot overwrite the completed prefix.
    runSession env.pool (Projection.saveCheckpoint w row.id 0 O.emptyOccurrence) `shouldReturn` False
    getSession env.pool w row.id `shouldReturn` Just done

  it "upgrades populated active/imported/sealed evidence without rewriting canonical rows and is idempotent" $ \_ ->
    withTestSandbox $ \sandbox -> withSandboxedEnv sandbox $ withSandboxedPostgres sandbox $ \db ->
      bracket (createPool db.testDbConnStr 2 30 30000) destroyAllResources $ \pool -> do
        source <- resolveMigrationsDir sandbox.sandboxRepoRoot
        let beforeDir=sandbox.sandboxRoot </> "before-v054"
            upgradeDir=sandbox.sandboxRoot </> "only-v054"
            filename="V054__session_occurrences.sql"
            env=TestEnv pool sandbox db
            body=event "2026-10-30T12:00:00Z" "historical"<>"\n"
        createDirectory beforeDir
        createDirectory upgradeDir
        files <- listDirectory source
        forM_ (filter (\name -> name<"V054" && ".sql" `T.isSuffixOf` T.pack name) files) $ \name -> copyFile (source </> name) (beforeDir </> name)
        copyFile (source </> filename) (upgradeDir </> filename)
        Migration.runMigrations pool beforeDir >>= (\r -> r.failed `shouldBe` Nothing)
        (w,row,_) <- fixture env
        first <- (.session) <$> appendSession pool w row.id (AppendSession 0 0 body False)
        _ <- terminateSession pool w row.id (TerminateSession first.committedBytes first.revision first.committedBytes (sessionSha256 body))
        (activeWorkspace,active,_) <- fixture env
        activeContent <- (.session) <$> appendSession pool activeWorkspace active.id (AppendSession 0 0 body False)
        (importWorkspace,_,importTask) <- fixture env
        importedId <- UUID.nextRandom
        imported <- importSession pool (ImportSession (CreateSession importedId importWorkspace "codex-jsonl" "Imported occurrence" Nothing Nothing importTask) (fromIntegral (BS.length body)) (sessionSha256 body))
        importedContent <- (.session) <$> appendSession pool importWorkspace imported.id (AppendSession 0 0 body False)
        _ <- terminateSession pool importWorkspace imported.id (TerminateSession importedContent.committedBytes importedContent.revision importedContent.committedBytes (sessionSha256 body))
        execute env "CREATE TABLE upgrade_occurrence_rows AS SELECT to_jsonb(s) value FROM sessions s; CREATE TABLE upgrade_occurrence_chunks AS TABLE session_chunks"
        execute env "CREATE FUNCTION occurrence_ledger_failure() RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN IF NEW.version=54 THEN RAISE EXCEPTION 'injected occurrence ledger failure'; END IF; RETURN NEW; END; $$; CREATE TRIGGER occurrence_ledger_failure BEFORE INSERT ON schema_migrations FOR EACH ROW EXECUTE FUNCTION occurrence_ledger_failure()"
        Migration.runMigrations pool upgradeDir >>= (\r -> r.failed `shouldSatisfy` (/=Nothing))
        boolean env "SELECT to_regclass('session_occurrences') IS NULL AND NOT EXISTS(SELECT 1 FROM schema_migrations WHERE version=54)" `shouldReturn` True
        execute env "DROP TRIGGER occurrence_ledger_failure ON schema_migrations; DROP FUNCTION occurrence_ledger_failure()"
        Migration.runMigrations pool upgradeDir >>= (\r -> r.applied `shouldBe` [filename])
        Migration.runMigrations pool upgradeDir >>= (\r -> r.applied `shouldBe` [])
        forM_ [(w,row.id),(activeWorkspace,activeContent.id),(importWorkspace,importedId)] $ \(workspace,identifier) -> do
          deriveSessionOccurrence pool workspace identifier
          Just result <- getSession pool workspace identifier
          result.lastActivityAt `shouldBe` O.parseOccurrenceTime "2026-10-30T12:00:00Z"
        Just done <- getSession pool w row.id
        done.lastActivityAt `shouldBe` O.parseOccurrenceTime "2026-10-30T12:00:00Z"
        boolean env "SELECT NOT EXISTS((SELECT to_jsonb(s) FROM sessions s EXCEPT TABLE upgrade_occurrence_rows) UNION ALL (TABLE upgrade_occurrence_rows EXCEPT SELECT to_jsonb(s) FROM sessions s)) AND NOT EXISTS((TABLE session_chunks EXCEPT TABLE upgrade_occurrence_chunks) UNION ALL (TABLE upgrade_occurrence_chunks EXCEPT TABLE session_chunks))" `shouldReturn` True

fixture :: TestEnv -> IO (UUID,AgentSession,UUID)
fixture env = do
  workspace <- createTestWorkspace env "occurrence"
  task <- Task.createTask env.pool (Types.CreateTask workspace.id Nothing Nothing "Occurrence fixture" Nothing Nothing Nothing)
  execute env ("UPDATE tasks SET status='in_progress' WHERE id='"<>uuid task.id<>"'")
  identifier <- UUID.nextRandom
  row <- createSession env.pool (CreateSession identifier workspace.id "codex-jsonl" "Occurrence fixture" Nothing Nothing task.id)
  pure (workspace.id,row,task.id)

event :: ByteString -> ByteString -> ByteString
event time detail = "{\"timestamp\":\""<>time<>"\",\"type\":\"event_msg\",\"payload\":{\"type\":\"custom_event\",\"detail\":\""<>detail<>"\"}}"
uuid :: UUID -> ByteString
uuid = B8.pack . show
execute :: TestEnv -> ByteString -> IO ()
execute env sql = runSession env.pool (S.sql sql)
boolean :: TestEnv -> ByteString -> IO Bool
boolean env sql = runSession env.pool $ S.statement () $ St.Statement sql E.noParams (D.singleRow (D.column (D.nonNullable D.bool))) False
