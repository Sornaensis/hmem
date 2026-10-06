module HMem.DB.ObservationVersionSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (withAsync, wait)
import Control.Exception (bracket, try)
import Control.Monad (when)
import Data.Aeson (Value)
import Data.ByteString.Char8 qualified as B8
import Data.Text qualified as T
import Data.Pool (destroyAllResources)
import Data.UUID (UUID)
import Hasql.Decoders qualified as Dec
import Hasql.Encoders qualified as Enc
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement
import System.Timeout (timeout)
import Test.Hspec

import HMem.DB.ChangeStream (ChangeScope(..), listOutboxAfter)
import HMem.DB.Embedding (enableEmbeddingTarget)
import HMem.DB.Migration qualified as Migration
import HMem.DB.Observation
import HMem.DB.Pool (DBException, checkPgvector, createPool, runSession, withConn)
import HMem.DB.TestHarness
import HMem.Types

-- Uses the existing suite sandbox and a real ten-connection pool, without the
-- single-connection outer transaction used by the ordinary CRUD examples.
spec :: Spec
spec = around withTestEnv $ describe "Observation content versions" $ do
  it "allows exactly one competing writer, then a conscious rebase" $ \env -> do
    workspace <- createTestWorkspace env "observation-content-race"
    original <- create env workspace.id "original"
    let write pool body = updateObservationConditional pool workspace.id original.id original.contentVersion (UpdateObservation body)
        -- Pool stripes can serialize checkouts on one capability. Separate
        -- pools guarantee three distinct sessions while observing contention.
        withWriterPool = bracket (createPool env.testDb.testDbConnStr 1 60 10000) destroyAllResources
    results <- withWriterPool $ \firstPool -> withWriterPool $ \secondPool -> withWriterPool $ \lockPool -> withConn lockPool $ \conn -> do
      let run sql = Session.run (Session.sql sql) conn >>= either (fail . show) pure
      run "BEGIN"
      run ("SELECT id FROM observations WHERE id = '" <> B8.pack (show original.id) <> "' FOR UPDATE")
      withAsync (write firstPool "first contender") $ \first ->
        withAsync (write secondPool "second contender") $ \second -> do
          blocked <- timeout 5000000 $ waitForBlockedWriters env
          run "COMMIT"
          blocked `shouldBe` Just ()
          completion <- timeout 5000000 $ (,) <$> wait first <*> wait second
          maybe (expectationFailure "conditional writers timed out" >> fail "unreachable") pure completion
    let outcomes = [fst results, snd results]
        accepted = [row | ObservationUpdated row <- outcomes]
        rejected = [row | ObservationVersionMismatch row <- outcomes]
    length accepted `shouldBe` 1
    rejected `shouldBe` accepted
    winner <- case accepted of
      [row] -> pure row
      _ -> expectationFailure "expected one accepted writer" >> fail "unreachable"
    winner.contentVersion `shouldNotBe` original.contentVersion
    winner.subjects `shouldBe` original.subjects
    winner.gitSha `shouldBe` original.gitSha
    rebased <- updateObservationConditional env.pool workspace.id original.id winner.contentVersion (UpdateObservation "conscious rebase")
    case rebased of
      ObservationUpdated row -> do
        row.content `shouldBe` "conscious rebase"
        row.contentVersion `shouldNotBe` winner.contentVersion
      _ -> expectationFailure (show rebased)
    audit <- getAuditLogRows env.pool "observation" (T.pack (show original.id))
    map (.action) audit `shouldBe` ["create", "update", "update"]

  it "leaves content, search, jobs, audit, outbox and embedding untouched on mismatch" checkConflictEffects

  it "applies and rejects conditional writes with unchanged side effects without pgvector" $ \env ->
    -- Keep capability-changing DDL in one rollback-only pooled connection;
    -- restore the suite's optional vector column/extension before later tests.
    bracket (createPool env.testDb.testDbConnStr 1 60 10000) destroyAllResources $ \pool ->
      withTestTransaction (\transactionEnv -> do
        runSession transactionEnv.pool $ Session.sql "DROP INDEX IF EXISTS idx_observations_embedding; ALTER TABLE observations DROP COLUMN IF EXISTS embedding; DROP EXTENSION IF EXISTS vector"
        checkPgvector transactionEnv.pool `shouldReturn` False
        checkConflictEffects transactionEnv
      ) env { pool = pool }

  it "advances equal-content conditional, unconditional and SQL writes but rejects token replacement" $ \env -> do
    workspace <- createTestWorkspace env "observation-version-all-writers"
    original <- create env workspace.id "same content"
    Just unconditional <- updateObservation env.pool workspace.id original.id (UpdateObservation original.content)
    unconditional.contentVersion `shouldNotBe` original.contentVersion
    conditional <- updateObservationConditional env.pool workspace.id original.id unconditional.contentVersion (UpdateObservation original.content)
    latest <- case conditional of
      ObservationUpdated row -> pure row
      _ -> expectationFailure (show conditional) >> fail "unreachable"
    latest.contentVersion `shouldNotBe` unconditional.contentVersion
    runSession env.pool $ Session.sql "UPDATE observations SET content = content"
    Just sqlWrite <- getObservation env.pool workspace.id original.id
    sqlWrite.contentVersion `shouldNotBe` latest.contentVersion
    replacement <- try @DBException $ runSession env.pool $ Session.sql "UPDATE observations SET content_version = gen_random_uuid()"
    replacement `shouldSatisfy` either (const True) (const False)
    getObservation env.pool workspace.id original.id `shouldReturn` Just sqlWrite

  it "returns missing for foreign, deleted and nonexistent IDs without leaking canonical content" $ \env -> do
    workspace <- createTestWorkspace env "observation-version-owner"
    foreignWorkspace <- createTestWorkspace env "observation-version-foreign"
    original <- create env workspace.id "private content"
    updateObservationConditional env.pool foreignWorkspace.id original.id original.contentVersion (UpdateObservation "foreign") `shouldReturn` ObservationNotFound
    updateObservationConditional env.pool workspace.id foreignWorkspace.id original.contentVersion (UpdateObservation "missing") `shouldReturn` ObservationNotFound
    deleteObservation env.pool workspace.id original.id `shouldReturn` True
    updateObservationConditional env.pool workspace.id original.id original.contentVersion (UpdateObservation "deleted") `shouldReturn` ObservationNotFound

  it "backfills populated V029 rows without data effects and records V030 atomically" $ \env -> do
    workspace <- createTestWorkspace env "observation-version-migration"
    first <- create env workspace.id "first existing"
    second <- create env workspace.id "second existing"
    auditBefore <- getAuditLogRows env.pool "observation" (T.pack (show first.id))
    outboxBefore <- listOutboxAfter env.pool (WorkspaceScope workspace.id) 0 100
    -- Recreate the exact pre-V030 table shape in this contained test database.
    runSession env.pool $ Session.sql "DROP TRIGGER trg_observations_content_version ON observations; ALTER TABLE observations DROP COLUMN content_version CASCADE; DROP FUNCTION hmem_advance_observation_content_version(); DROP FUNCTION hmem_guard_observation_content_version(); DELETE FROM schema_migrations WHERE version = 30"
    upgraded <- Migration.runMigrations env.pool env.testSandbox.sandboxMigrationsDir
    upgraded.failed `shouldBe` Nothing
    upgraded.applied `shouldBe` ["V030__observation_content_versions.sql"]
    Just migratedFirst <- getObservation env.pool workspace.id first.id
    Just migratedSecond <- getObservation env.pool workspace.id second.id
    map (.content) [migratedFirst, migratedSecond] `shouldBe` [first.content, second.content]
    migratedFirst.contentVersion `shouldNotBe` migratedSecond.contentVersion
    migratedFirst.subjects `shouldBe` first.subjects
    getAuditLogRows env.pool "observation" (T.pack (show first.id)) `shouldReturn` auditBefore
    listOutboxAfter env.pool (WorkspaceScope workspace.id) 0 100 `shouldReturn` outboxBefore
    queryBool env "SELECT EXISTS (SELECT 1 FROM schema_migrations WHERE version = 30 AND name = 'V030__observation_content_versions.sql')" `shouldReturn` True
    repeated <- Migration.runMigrations env.pool env.testSandbox.sandboxMigrationsDir
    repeated.failed `shouldBe` Nothing
    repeated.applied `shouldBe` []

checkConflictEffects :: TestEnv -> IO ()
checkConflictEffects env = do
  workspace <- createTestWorkspace env "observation-content-conflict-effects"
  vectorAvailable <- checkPgvector env.pool
  when vectorAvailable $
    enableEmbeddingTarget env.pool (maybe (error "invalid test embedding space") id (parseEmbeddingSpaceFingerprint "hmem:version-test:v1"))
  original <- create env workspace.id "original"
  Just latest <- updateObservation env.pool workspace.id original.id (UpdateObservation "latest searchable")
  when vectorAvailable $
    setObservationEmbedding env.pool workspace.id original.id (replicate observationEmbeddingDimensions 0.25)
  Just canonical <- getObservation env.pool workspace.id original.id
  canonical.contentVersion `shouldBe` latest.contentVersion
  before <- effects env original.id
  auditBefore <- getAuditLogRows env.pool "observation" (T.pack (show original.id))
  outboxBefore <- listOutboxAfter env.pool (WorkspaceScope workspace.id) 0 100
  updateObservationConditional env.pool workspace.id original.id original.contentVersion (UpdateObservation "rejected content")
    `shouldReturn` ObservationVersionMismatch canonical
  effects env original.id `shouldReturn` before
  getAuditLogRows env.pool "observation" (T.pack (show original.id)) `shouldReturn` auditBefore
  listOutboxAfter env.pool (WorkspaceScope workspace.id) 0 100 `shouldReturn` outboxBefore
  getObservation env.pool workspace.id original.id `shouldReturn` Just canonical
  applied <- updateObservationConditional env.pool workspace.id original.id canonical.contentVersion (UpdateObservation "replacement searchable")
  case applied of
    ObservationUpdated row -> do
      row.contentVersion `shouldNotBe` canonical.contentVersion
      row.subjects `shouldBe` canonical.subjects
      row.gitSha `shouldBe` canonical.gitSha
    _ -> expectationFailure (show applied)
  queryBool env "SELECT EXISTS (SELECT 1 FROM observations WHERE search_vector @@ plainto_tsquery('simple', 'replacement'))" `shouldReturn` True
  when vectorAvailable $ do
    queryBool env "SELECT NOT EXISTS (SELECT 1 FROM observations WHERE embedding_space_fingerprint IS NOT NULL)" `shouldReturn` True
    queryBool env "SELECT EXISTS (SELECT 1 FROM embedding_jobs WHERE state = 'pending')" `shouldReturn` True

create :: TestEnv -> UUID -> T.Text -> IO Observation
create env workspace body = createObservation env.pool (CreateObservation workspace [ObservationSubject SubjectFile "src/Version.hs"] (T.replicate 40 "a") body)

queryBool :: TestEnv -> B8.ByteString -> IO Bool
queryBool env sql = runSession env.pool $ Session.statement () $
  Statement.Statement sql Enc.noParams (Dec.singleRow (Dec.column (Dec.nonNullable Dec.bool))) False

waitForBlockedWriters :: TestEnv -> IO ()
waitForBlockedWriters env = do
  blocked <- queryBool env "SELECT count(*) >= 2 FROM pg_stat_activity WHERE datname = current_database() AND wait_event_type = 'Lock' AND query LIKE '%UPDATE observations SET content = $3%'"
  if blocked then pure () else threadDelay 10000 >> waitForBlockedWriters env

-- The full row includes the optional vector when present, search vector and
-- token; the job includes lease/fingerprint/state metadata.
effects :: TestEnv -> UUID -> IO [Value]
effects env observationId = runSession env.pool $ Session.statement () $
  Statement.Statement
    ("SELECT to_jsonb(o) FROM observations o WHERE id = '" <> B8.pack (show observationId) <> "' UNION ALL SELECT to_jsonb(j) FROM embedding_jobs j WHERE observation_id = '" <> B8.pack (show observationId) <> "'")
    Enc.noParams (Dec.rowList (Dec.column (Dec.nonNullable Dec.jsonb))) False
