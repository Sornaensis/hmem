module HMem.DB.ObservationVersionSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (withAsync, wait)
import Control.Exception (bracket, try)
import Control.Monad (when)
import Data.Aeson (Value)
import Data.ByteString.Char8 qualified as B8
import Data.List (isInfixOf, sort)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Pool (destroyAllResources)
import Data.UUID (UUID)
import Hasql.Decoders qualified as Dec
import Hasql.Encoders qualified as Enc
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement
import System.Directory (copyFile, createDirectoryIfMissing, listDirectory)
import System.FilePath ((</>))
import System.Timeout (timeout)
import Test.Hspec

import HMem.DB.ChangeStream (ChangeScope(..), listOutboxAfter)
import HMem.DB.Embedding (enableEmbeddingTarget)
import HMem.DB.Migration qualified as Migration
import HMem.DB.Observation
import HMem.DB.ObservationFixture (reviewed, writeObservation)
import HMem.DB.Pool (DBException, checkPgvector, createPool, runSession, withConn)
import HMem.DB.TestHarness
import HMem.Types

-- Uses the existing suite sandbox and a real ten-connection pool, without the
-- single-connection outer transaction used by the ordinary CRUD examples.
spec :: Spec
spec = describe "Observation content versions" $ do
  around withTestEnv currentSchemaSpec
  it "backfills populated V029 rows without data effects and records V030 atomically" historicalVersionMigration

currentSchemaSpec :: SpecWith TestEnv
currentSchemaSpec = do
  it "allows exactly one competing writer, then a conscious rebase" $ \env -> do
    workspace <- createTestWorkspace env "observation-content-race"
    original <- create env workspace.id "original"
    let write pool body = updateObservationReviewed pool workspace.id original.id original.contentVersion (reviewed body)
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
    winner.latestSequence `shouldBe` 2
    fmap (.contentVersion) winner.currentProvenance `shouldBe` Just (Just winner.contentVersion)
    historyAfterRace <- listObservationHistory env.pool workspace.id original.id Nothing Nothing
    map (.provenance.sequence) historyAfterRace `shouldBe` [2,1]
    map (.provenance.contentVersion) historyAfterRace `shouldBe` [Just winner.contentVersion, Just original.contentVersion]
    rebased <- updateObservationReviewed env.pool workspace.id original.id winner.contentVersion (reviewed "conscious rebase")
    case rebased of
      ObservationUpdated row -> do
        row.content `shouldBe` "conscious rebase"
        row.contentVersion `shouldNotBe` winner.contentVersion
        row.latestSequence `shouldBe` 3
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

  it "advances equal-content reviewed assertions but rejects unbound SQL and token replacement" $ \env -> do
    workspace <- createTestWorkspace env "observation-version-all-writers"
    original <- create env workspace.id "same content"
    Just unconditional <- writeObservation env.pool workspace.id original.id (reviewed original.content)
    unconditional.contentVersion `shouldNotBe` original.contentVersion
    conditional <- updateObservationReviewed env.pool workspace.id original.id unconditional.contentVersion (reviewed original.content)
    latest <- case conditional of
      ObservationUpdated row -> pure row
      _ -> expectationFailure (show conditional) >> fail "unreachable"
    latest.contentVersion `shouldNotBe` unconditional.contentVersion
    unbound <- try @DBException $ runSession env.pool $ Session.sql "UPDATE observations SET content = content"
    unbound `shouldSatisfy` either (const True) (const False)
    Just sqlWrite <- getObservation env.pool workspace.id original.id
    sqlWrite.contentVersion `shouldBe` latest.contentVersion
    replacement <- try @DBException $ runSession env.pool $ Session.sql "UPDATE observations SET content_version = gen_random_uuid()"
    replacement `shouldSatisfy` either (const True) (const False)
    getObservation env.pool workspace.id original.id `shouldReturn` Just sqlWrite

  it "returns missing for foreign, deleted and nonexistent IDs without leaking canonical content" $ \env -> do
    workspace <- createTestWorkspace env "observation-version-owner"
    foreignWorkspace <- createTestWorkspace env "observation-version-foreign"
    original <- create env workspace.id "private content"
    updateObservationReviewed env.pool foreignWorkspace.id original.id original.contentVersion (reviewed "foreign") `shouldReturn` ObservationNotFound
    updateObservationReviewed env.pool workspace.id foreignWorkspace.id original.contentVersion (reviewed "missing") `shouldReturn` ObservationNotFound
    deleteObservation env.pool workspace.id original.id `shouldReturn` True
    updateObservationReviewed env.pool workspace.id original.id original.contentVersion (reviewed "deleted") `shouldReturn` ObservationNotFound

-- Historical schemas need their own database, not a downgrade of the current
-- suite database: later migrations may legitimately depend on the new objects.
historicalVersionMigration :: Expectation
historicalVersionMigration = withTestSandbox $ \sandbox ->
  withSandboxedEnv sandbox $ withSandboxedPostgres sandbox $ \db ->
    bracket (createPool db.testDbConnStr 2 30 30000) destroyAllResources $ \pool -> do
      let env = TestEnv pool sandbox db
          migrationName = "V030__observation_content_versions.sql"
      beforeDir <- copyVersionMigrations sandbox "through-v029" (< "V030")
      upgradeDir <- copyVersionMigrations sandbox "v030-only" (== migrationName)
      failureDir <- copyVersionMigrations sandbox "v030-failure" (== migrationName)
      preVersion <- Migration.runMigrations pool beforeDir
      preVersion.failed `shouldBe` Nothing
      queryBool env "SELECT count(*) = 29 AND max(version) = 29 FROM schema_migrations" `shouldReturn` True
      let assertPreVersion = queryBool env "SELECT NOT EXISTS (SELECT 1 FROM information_schema.columns WHERE table_schema = 'public' AND table_name = 'observations' AND column_name IN ('content_version', 'latest_sequence', 'current_provenance')) AND to_regclass('public.observation_revision_events') IS NULL AND NOT EXISTS (SELECT 1 FROM schema_migrations WHERE version >= 30) AND to_regprocedure('public.hmem_advance_observation_content_version()') IS NULL AND to_regprocedure('public.hmem_guard_observation_content_version()') IS NULL" `shouldReturn` True
      assertPreVersion
      -- Seed only columns present at V029, including ordered subject sets.
      runSession pool $ Session.sql "INSERT INTO workspaces (id, name) VALUES ('00000000-0000-0000-0000-000000000010', 'observation-version-migration'); WITH inserted AS (INSERT INTO observations (id, workspace_id, git_sha, content, subject_set_open) VALUES ('00000000-0000-0000-0000-000000000001', '00000000-0000-0000-0000-000000000010', repeat('a',40), 'first existing', TRUE), ('00000000-0000-0000-0000-000000000002', '00000000-0000-0000-0000-000000000010', repeat('b',40), 'second existing', TRUE) RETURNING id) INSERT INTO observation_subjects (observation_id, ordinal, subject_kind, subject) SELECT i.id, s.ordinal, s.kind::observation_subject_kind, s.subject FROM inserted i JOIN (VALUES ('00000000-0000-0000-0000-000000000001'::uuid, 0, 'file', 'src/Version.hs'), ('00000000-0000-0000-0000-000000000002'::uuid, 0, 'glob', 'src/**/*.hs'), ('00000000-0000-0000-0000-000000000002'::uuid, 1, 'file', 'README.md')) s(id, ordinal, kind, subject) ON s.id = i.id"
      [workspace] <- queryUUIDs env "SELECT id FROM workspaces"
      [firstId, secondId] <- queryUUIDs env "SELECT id FROM observations ORDER BY id"
      rowsBefore <- queryValues env "SELECT to_jsonb(o) FROM observations o ORDER BY id"
      subjectsBefore <- queryValues env "SELECT to_jsonb(s) FROM observation_subjects s ORDER BY observation_id, ordinal"
      auditBefore <- queryValues env "SELECT to_jsonb(a) FROM audit_log a ORDER BY id"
      outboxBefore <- listOutboxAfter pool (WorkspaceScope workspace) 0 100
      queryBool env "SELECT count(*) = 2 FROM audit_log WHERE entity_type = 'observation' AND action = 'create'" `shouldReturn` True
      length outboxBefore `shouldSatisfy` (>= 2)
      let assertEffects = do
            queryValues env "SELECT to_jsonb(s) FROM observation_subjects s ORDER BY observation_id, ordinal" `shouldReturn` subjectsBefore
            queryValues env "SELECT to_jsonb(a) FROM audit_log a ORDER BY id" `shouldReturn` auditBefore
            listOutboxAfter pool (WorkspaceScope workspace) 0 100 `shouldReturn` outboxBefore
      -- Fail after V030's DDL, backfill and ledger INSERT, but before COMMIT.
      -- Only the sandbox copy is modified; retry uses the canonical SQL bytes.
      canonicalSql <- TIO.readFile (upgradeDir </> migrationName)
      T.count "COMMIT;" canonicalSql `shouldBe` 1
      TIO.writeFile (failureDir </> migrationName) (T.replace "COMMIT;" "SELECT 1 / 0;\nCOMMIT;" canonicalSql)
      failed <- Migration.runMigrations pool failureDir
      failed.applied `shouldBe` []
      failed.failed `shouldSatisfy` \case
        Just (name, message) -> name == migrationName && "division by zero" `isInfixOf` message
        _ -> False
      assertPreVersion
      queryValues env "SELECT to_jsonb(o) FROM observations o ORDER BY id" `shouldReturn` rowsBefore
      assertEffects
      upgraded <- Migration.runMigrations pool upgradeDir
      upgraded.failed `shouldBe` Nothing
      upgraded.applied `shouldBe` [migrationName]
      queryValues env "SELECT to_jsonb(o) - 'content_version' FROM observations o ORDER BY id" `shouldReturn` rowsBefore
      versions <- queryUUIDs env "SELECT content_version FROM observations ORDER BY id"
      case versions of
        [firstVersion, secondVersion] -> firstVersion `shouldNotBe` secondVersion
        _ -> expectationFailure "expected exactly two non-null Observation versions"
      assertEffects
      queryBool env "SELECT count(*) = 1 FROM schema_migrations WHERE version = 30 AND name = 'V030__observation_content_versions.sql'" `shouldReturn` True
      repeated <- Migration.runMigrations pool upgradeDir
      repeated.failed `shouldBe` Nothing
      repeated.applied `shouldBe` []
      queryUUIDs env "SELECT content_version FROM observations ORDER BY id" `shouldReturn` versions
      assertEffects
      current <- Migration.runMigrations pool sandbox.sandboxMigrationsDir
      current.failed `shouldBe` Nothing
      current.applied `shouldSatisfy` elem "V039__declared_bot_identity.sql"
      Just migratedFirst <- getObservation pool workspace firstId
      Just migratedSecond <- getObservation pool workspace secondId
      map (.content) [migratedFirst, migratedSecond] `shouldBe` ["first existing", "second existing"]
      map (.contentVersion) [migratedFirst, migratedSecond] `shouldBe` versions
      migratedFirst.subjects `shouldBe` [ObservationSubject SubjectFile "src/Version.hs"]
      migratedSecond.subjects `shouldBe` [ObservationSubject SubjectGlob "src/**/*.hs", ObservationSubject SubjectFile "README.md"]
      queryBool env "SELECT count(*) = 2 FROM observation_revision_events WHERE event_kind = 'legacy_creation' AND content_version IS NULL AND content_digest IS NULL AND bot_identity IS NULL" `shouldReturn` True
      Just revised <- writeObservation pool workspace firstId (reviewed "reviewed after upgrade")
      revised.contentVersion `shouldNotBe` migratedFirst.contentVersion
      revised.latestSequence `shouldBe` 2
      queryBool env "SELECT o.current_provenance = hmem_observation_event_projection(e) AND e.event_kind = 'update' AND e.content_version = o.content_version AND NOT (o.current_provenance ? 'bot_identity') FROM observations o JOIN observation_revision_events e ON e.observation_id = o.id AND e.sequence = o.latest_sequence WHERE o.id = '00000000-0000-0000-0000-000000000001'" `shouldReturn` True
      finalRepeat <- Migration.runMigrations pool sandbox.sandboxMigrationsDir
      finalRepeat.failed `shouldBe` Nothing
      finalRepeat.applied `shouldBe` []

copyVersionMigrations :: TestSandbox -> FilePath -> (FilePath -> Bool) -> IO FilePath
copyVersionMigrations sandbox name keep = do
  let target = sandbox.sandboxTmpDir </> name
  createDirectoryIfMissing True target
  files <- sort . filter keep <$> listDirectory sandbox.sandboxMigrationsDir
  mapM_ (\file -> copyFile (sandbox.sandboxMigrationsDir </> file) (target </> file)) files
  pure target

queryValues :: TestEnv -> B8.ByteString -> IO [Value]
queryValues env sql = runSession env.pool $ Session.statement () $
  Statement.Statement sql Enc.noParams (Dec.rowList (Dec.column (Dec.nonNullable Dec.jsonb))) False

queryUUIDs :: TestEnv -> B8.ByteString -> IO [UUID]
queryUUIDs env sql = runSession env.pool $ Session.statement () $
  Statement.Statement sql Enc.noParams (Dec.rowList (Dec.column (Dec.nonNullable Dec.uuid))) False

checkConflictEffects :: TestEnv -> IO ()
checkConflictEffects env = do
  workspace <- createTestWorkspace env "observation-content-conflict-effects"
  vectorAvailable <- checkPgvector env.pool
  when vectorAvailable $
    enableEmbeddingTarget env.pool (maybe (error "invalid test embedding space") id (parseEmbeddingSpaceFingerprint "hmem:version-test:v1"))
  original <- create env workspace.id "original"
  Just latest <- writeObservation env.pool workspace.id original.id (reviewed "latest searchable")
  when vectorAvailable $
    setObservationEmbedding env.pool workspace.id original.id (replicate observationEmbeddingDimensions 0.25)
  Just canonical <- getObservation env.pool workspace.id original.id
  canonical.contentVersion `shouldBe` latest.contentVersion
  before <- effects env original.id
  historyBefore <- listObservationHistory env.pool workspace.id original.id Nothing Nothing
  auditBefore <- getAuditLogRows env.pool "observation" (T.pack (show original.id))
  outboxBefore <- listOutboxAfter env.pool (WorkspaceScope workspace.id) 0 100
  updateObservationReviewed env.pool workspace.id original.id original.contentVersion (reviewed "rejected content")
    `shouldReturn` ObservationVersionMismatch canonical
  effects env original.id `shouldReturn` before
  listObservationHistory env.pool workspace.id original.id Nothing Nothing `shouldReturn` historyBefore
  getAuditLogRows env.pool "observation" (T.pack (show original.id)) `shouldReturn` auditBefore
  listOutboxAfter env.pool (WorkspaceScope workspace.id) 0 100 `shouldReturn` outboxBefore
  getObservation env.pool workspace.id original.id `shouldReturn` Just canonical
  applied <- updateObservationReviewed env.pool workspace.id original.id canonical.contentVersion (reviewed "replacement searchable")
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
