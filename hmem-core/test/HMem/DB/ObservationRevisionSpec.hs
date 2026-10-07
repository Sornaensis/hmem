module HMem.DB.ObservationRevisionSpec (spec) where

import Control.Exception (bracket, bracket_, try)
import Control.Monad (forM_)
import Data.Aeson (Value, object, (.=))
import Data.ByteString.Char8 qualified as B8
import Data.Pool (destroyAllResources)
import Data.Text qualified as T
import Data.UUID (UUID)
import Hasql.Decoders qualified as Dec
import Hasql.Encoders qualified as Enc
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement
import System.Directory (copyFile, createDirectory, listDirectory)
import System.FilePath ((</>))
import Test.Hspec

import HMem.DB.ChangeStream (ChangeScope(..), listOutboxAfter)
import HMem.DB.Embedding
import HMem.DB.Migration qualified as Migration
import HMem.DB.Observation
import HMem.DB.Pool (DBException(..), checkPgvector, createPool, runSession)
import HMem.DB.RequestContext
import HMem.DB.Search (searchAll)
import HMem.DB.TestHarness
import HMem.Types

spec :: Spec
spec = do
  around withTestEnv $ describe "Observation revision assertions" $ do
    it "composes original/current/history filters without duplicating current rows or their facets and search hits" $ \env -> do
      workspace <- createTestWorkspace env "revision-filter-parity"
      original <- create env workspace.id "needle old"
      ObservationUpdated second <- updateObservationReviewed env.pool workspace.id original.id original.contentVersion
        (ReviewedObservationUpdate "needle middle" reviewSha)
      ObservationUpdated repeated <- updateObservationReviewed env.pool workspace.id original.id second.contentVersion
        (ReviewedObservationUpdate second.content reviewSha)
      let currentSha = T.replicate 40 "c"
          context = Just (ObservationProvenanceMatch (Just original.gitSha) (Just currentSha) (Just reviewSha))
      ObservationUpdated current <- updateObservationReviewed env.pool workspace.id original.id repeated.contentVersion
        (ReviewedObservationUpdate "needle current" currentSha)
      let input = ObservationQuery workspace.id Nothing Nothing (Just original.gitSha) (Just "needle") (Just 1) Nothing (Just currentSha) (Just reviewSha)
      rows <- listObservationsOverfetch env.pool input
      map (.id) rows `shouldBe` [current.id]
      map (.content) rows `shouldBe` [current.content]
      map (.provenanceMatch) rows `shouldBe` [context]
      listObservations env.pool input { currentGitSha = Just reviewSha } `shouldReturn` []
      listObservations env.pool input { historyGitSha = Just (T.replicate 40 "d") } `shouldReturn` []
      countObservations env.pool (ObservationCountQuery workspace.id Nothing Nothing (Just original.gitSha) (Just "needle") Nothing (Just currentSha) (Just reviewSha))
        `shouldReturn` ObservationCounts workspace.id 1 1
      facets <- listObservationSubjectFacets env.pool (ObservationSubjectFacetQuery workspace.id Nothing (Just original.gitSha) (Just "needle") Nothing Nothing (Just currentSha) (Just reviewSha))
      map (.observationCount) facets `shouldBe` [1]
      matches <- matchObservations env.pool (ObservationMatchQuery workspace.id ["src/Revision.hs", "src/Revision.hs"] Nothing (Just original.gitSha) (Just "needle") Nothing Nothing (Just currentSha) (Just reviewSha))
      map (\(m :: ObservationMatch) -> m.observation.id) matches `shouldBe` [current.id]
      map (\(m :: ObservationMatch) -> m.observation.provenanceMatch) matches `shouldBe` [context]
      hits <- searchAll env.pool UnifiedSearchQuery
        { workspaceId = Just workspace.id, query = Just "needle", entityTypes = Just [SearchObservation], searchLanguage = Nothing
        , limit = Just 1, offset = Nothing, subjectKind = Nothing, subject = Nothing, gitSha = Just original.gitSha
        , projectStatus = Nothing, taskStatus = Nothing, taskPriority = Nothing, projectId = Nothing
        , currentGitSha = Just currentSha, historyGitSha = Just reviewSha }
      map (.id) hits.observations `shouldBe` [current.id]
      map (.contentVersion) hits.observations `shouldBe` [current.contentVersion]
      map (.currentProvenance) hits.observations `shouldBe` [current.currentProvenance]
      map (.provenanceMatch) hits.observations `shouldBe` [context]
      validateObservationQuery input { currentGitSha = Just "BAD" } `shouldSatisfy` (not . null)
      validateObservationQuery input { historyGitSha = Just "BAD" } `shouldSatisfy` (not . null)
      available <- checkPgvector env.pool
      putStrLn ("[revision-filter-parity] pgvector_present=" <> show available)
      if not available then pure () else do
        let vector = 1 : replicate (observationEmbeddingDimensions - 1) 0
            similar = SimilarObservationQuery workspace.id Nothing Nothing (Just original.gitSha) vector Nothing Nothing Nothing Nothing (Just currentSha) (Just reviewSha)
        setObservationEmbedding env.pool workspace.id current.id vector
        neighbors <- similarObservations env.pool similar
        map (\(m :: SimilarObservation) -> m.observation.id) neighbors `shouldBe` [current.id]
        map (\(m :: SimilarObservation) -> m.observation.provenanceMatch) neighbors `shouldBe` [context]
        similarObservations env.pool similar { currentGitSha = Just reviewSha } `shouldReturn` []

    it "binds exact UTF-8 and advances repeated SHA/text assertions with bounded ordered history" $ \env -> do
      workspace <- createTestWorkspace env "revision-events"
      original <- create env workspace.id "é\r\n"
      let Just initial = original.currentProvenance
      original.latestSequence `shouldBe` 1
      initial.eventKind `shouldBe` "creation"
      initial.contentDigest `shouldBe` Just (observationContentDigest original.content)
      initial.contentVersion `shouldBe` Just original.contentVersion
      initial.reviewedGitSha `shouldBe` original.gitSha
      ObservationUpdated second <- updateObservationReviewed env.pool workspace.id original.id original.contentVersion
        (ReviewedObservationUpdate original.content reviewSha)
      ObservationUpdated third <- updateObservationReviewed env.pool workspace.id original.id second.contentVersion
        (ReviewedObservationUpdate original.content reviewSha)
      third.latestSequence `shouldBe` 3
      third.contentVersion `shouldNotBe` second.contentVersion
      third.gitSha `shouldBe` original.gitSha
      third.subjects `shouldBe` original.subjects
      let Just current = third.currentProvenance
      current.sequence `shouldBe` third.latestSequence
      current.reviewedGitSha `shouldBe` reviewSha
      current.contentDigest `shouldBe` Just (observationContentDigest original.content)
      current.contentVersion `shouldBe` Just third.contentVersion
      history <- listObservationHistory env.pool workspace.id original.id Nothing Nothing
      map (.provenance.sequence) history `shouldBe` [3,2,1]
      map (.observationId) history `shouldBe` replicate 3 original.id
      listObservationHistory env.pool workspace.id original.id (Just 1) (Just 1) `shouldReturn` [history !! 1]
      listObservationHistoryOverfetch env.pool workspace.id original.id (Just 1) Nothing `shouldReturn` take 2 history
      listObservationHistory env.pool workspace.id original.id (Just 200) (Just 3) `shouldReturn` []
      expectRejected $ listObservationHistory env.pool workspace.id original.id (Just 201) Nothing
      expectRejected $ listObservationHistory env.pool workspace.id original.id Nothing (Just (-1))

    it "preserves trusted local, user and token-bot actor identity rather than the grant holder" $ \env -> do
      workspace <- createTestWorkspace env "revision-actors"
      let grantUser = read "00000000-0000-0000-0000-000000000077"
          actors = [ Principal ActorUser "local-user" "Local user" PrincipalSyntheticLocalSuperadmin
                   , Principal ActorBot "local-bot:test" "Local bot" PrincipalSyntheticLocalSuperadmin
                   , Principal ActorUser (T.pack (show grantUser)) "User" (PrincipalGrantUser grantUser)
                   , Principal ActorBot "token-actor-42" "Bot token" (PrincipalGrantUser grantUser) ]
      forM_ actors $ \actor -> withPrincipalContext (Just actor) $ do
        row <- create env workspace.id "actor"
        ObservationUpdated updated <- updateObservationReviewed env.pool workspace.id row.id row.contentVersion
          (ReviewedObservationUpdate "actor" reviewSha)
        forM_ [row, updated] $ \observation -> do
          let Just p = observation.currentProvenance
          (p.actorType, p.actorId, p.actorLabel) `shouldBe`
            (Just (actorTypeToText actor.actorType), Just actor.actorId, Just actor.actorLabel)
        recorded <- runSession env.pool $ Session.statement () $ Statement.Statement
          ("SELECT jsonb_build_object('actor_type',actor_type,'actor_id',actor_id,'actor_label',actor_label) FROM audit_log WHERE entity_type = 'observation' AND entity_id = '" <> B8.pack (show row.id) <> "'")
          Enc.noParams (Dec.rowList (Dec.column (Dec.nonNullable Dec.jsonb))) False
        recorded `shouldBe` replicate 2 (object ["actor_type" .= actorTypeToText actor.actorType, "actor_id" .= actor.actorId, "actor_label" .= actor.actorLabel])

    it "rejects invalid/projection/event mutation without effects" $ \env -> do
      workspace <- createTestWorkspace env "revision-guards"
      row <- create env workspace.id "guarded"
      before <- allEffects env row.id
      expectRejected $ updateObservationReviewed env.pool workspace.id row.id row.contentVersion (ReviewedObservationUpdate "invalid" "BAD")
      forM_ [ "UPDATE observations SET current_provenance = NULL"
            , "UPDATE observations SET latest_sequence = latest_sequence + 1"
            , "UPDATE observation_revision_events SET reviewed_git_sha = repeat('b',40)"
            , "DELETE FROM observation_revision_events"
            , "INSERT INTO observation_revision_events SELECT observation_id,sequence+1,event_kind,reviewed_git_sha,content_version,content_digest,recorded_at,actor_type,actor_id,actor_label FROM observation_revision_events" ] $ \sql ->
        expectRejected $ runSession env.pool (Session.sql sql)
      allEffects env row.id `shouldReturn` before
      deleteObservation env.pool workspace.id row.id `shouldReturn` True
      bool env "SELECT NOT EXISTS (SELECT 1 FROM observation_revision_events)" `shouldReturn` True

    it "rolls back event/version/vector/job/audit/outbox effects on a late mutation failure" $ \env -> do
      workspace <- createTestWorkspace env "revision-rollback"
      let Just space = parseEmbeddingSpaceFingerprint "hmem:revision-rollback:v1"
      enableEmbeddingTarget env.pool space
      row <- create env workspace.id "before"
      before <- allEffects env row.id
      auditBefore <- getAuditLogRows env.pool "observation" (T.pack (show row.id))
      outboxBefore <- listOutboxAfter env.pool (WorkspaceScope workspace.id) 0 100
      bracket_
        (runSession env.pool $ Session.sql "CREATE FUNCTION revision_test_failure() RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN RAISE EXCEPTION USING ERRCODE = '23514', MESSAGE = 'injected late failure'; END; $$; CREATE TRIGGER zz_revision_test_failure AFTER UPDATE OF content ON observations FOR EACH ROW EXECUTE FUNCTION revision_test_failure()")
        (runSession env.pool $ Session.sql "DROP TRIGGER zz_revision_test_failure ON observations; DROP FUNCTION revision_test_failure()") $ do
          expectRejected $ updateObservationReviewed env.pool workspace.id row.id row.contentVersion (ReviewedObservationUpdate "rollback" reviewSha)
          allEffects env row.id `shouldReturn` before
          getAuditLogRows env.pool "observation" (T.pack (show row.id)) `shouldReturn` auditBefore
          listOutboxAfter env.pool (WorkspaceScope workspace.id) 0 100 `shouldReturn` outboxBefore

  describe "Observation revision migration" $ do
    it "rolls back partial DDL and honestly migrates populated creation claims with atomic ledger and rerun" $
      withTestSandbox $ \sandbox -> withSandboxedEnv sandbox $ withSandboxedPostgres sandbox $ \db ->
        bracket (createPool db.testDbConnStr 2 30 30000) destroyAllResources $ \pool -> do
          source <- resolveMigrationsDir sandbox.sandboxRepoRoot
          let beforeDir = sandbox.sandboxRoot </> "before-v032"
              upgradeDir = sandbox.sandboxRoot </> "only-v032"
              filename = "V032__observation_revision_events.sql"
          createDirectory beforeDir
          createDirectory upgradeDir
          files <- listDirectory source
          forM_ (filter (\name -> name < "V032" && ".sql" `T.isSuffixOf` T.pack name) files) $ \name -> copyFile (source </> name) (beforeDir </> name)
          copyFile (source </> filename) (upgradeDir </> filename)
          beforeMigration <- Migration.runMigrations pool beforeDir
          beforeMigration.failed `shouldBe` Nothing
          let env = TestEnv pool sandbox db
          workspace <- createTestWorkspace env "revision-legacy"
          runSession pool $ Session.sql $ B8.pack $
            "WITH inserted AS (INSERT INTO observations (id,workspace_id,git_sha,content,subject_set_open) VALUES ('00000000-0000-0000-0000-000000000031','" <> show workspace.id <> "',repeat('a',40),'legacy creation',true), ('00000000-0000-0000-0000-000000000032','" <> show workspace.id <> "',repeat('a',40),'corrected legacy',true) RETURNING id) INSERT INTO observation_subjects (observation_id,ordinal,subject_kind,subject) SELECT id,0,'file','src/Legacy.hs' FROM inserted; UPDATE observations SET content = 'corrected without reviewed SHA' WHERE id = '00000000-0000-0000-0000-000000000032'"
          -- A deliberately conflicting object fails after ALTER TABLE, proving
          -- the migration owns rollback and ledger registration together.
          runSession pool $ Session.sql "CREATE TABLE observation_revision_events (conflict boolean)"
          failed <- Migration.runMigrations pool upgradeDir
          failed.failed `shouldSatisfy` (/= Nothing)
          bool env "SELECT NOT EXISTS (SELECT 1 FROM schema_migrations WHERE version = 32) AND NOT EXISTS (SELECT 1 FROM information_schema.columns WHERE table_name = 'observations' AND column_name = 'latest_sequence')" `shouldReturn` True
          runSession pool $ Session.sql "DROP TABLE observation_revision_events"
          auditBefore <- getAuditLogRows pool "observation" "00000000-0000-0000-0000-000000000032"
          outboxBefore <- listOutboxAfter pool (WorkspaceScope workspace.id) 0 100
          upgraded <- Migration.runMigrations pool upgradeDir
          upgraded.failed `shouldBe` Nothing
          upgraded.applied `shouldBe` [filename]
          forM_ ["00000000-0000-0000-0000-000000000031", "00000000-0000-0000-0000-000000000032"] $ \identifier -> do
            let observationId = read identifier
            Just row <- getObservation pool workspace.id observationId
            row.latestSequence `shouldBe` 1
            row.currentProvenance `shouldBe` Nothing
            [event] <- listObservationHistory pool workspace.id observationId Nothing Nothing
            event.provenance.eventKind `shouldBe` "legacy_creation"
            event.provenance.reviewedGitSha `shouldBe` row.gitSha
            event.provenance.contentVersion `shouldBe` Nothing
            event.provenance.contentDigest `shouldBe` Nothing
            event.provenance.recordedAt `shouldBe` row.createdAt
            event.provenance.actorId `shouldBe` Nothing
          getAuditLogRows pool "observation" "00000000-0000-0000-0000-000000000032" `shouldReturn` auditBefore
          listOutboxAfter pool (WorkspaceScope workspace.id) 0 100 `shouldReturn` outboxBefore
          bool env "SELECT EXISTS (SELECT 1 FROM schema_migrations WHERE version = 32 AND name = 'V032__observation_revision_events.sql')" `shouldReturn` True
          repeated <- Migration.runMigrations pool upgradeDir
          repeated.failed `shouldBe` Nothing
          repeated.applied `shouldBe` []
          let correctedId = read "00000000-0000-0000-0000-000000000032"
          Just legacy <- getObservation pool workspace.id correctedId
          ObservationUpdated bound <- updateObservationReviewed pool workspace.id correctedId legacy.contentVersion (ReviewedObservationUpdate legacy.content reviewSha)
          bound.latestSequence `shouldBe` 2
          fmap (.reviewedGitSha) bound.currentProvenance `shouldBe` Just reviewSha

create :: TestEnv -> UUID -> T.Text -> IO Observation
create env workspace body = createObservation env.pool (CreateObservation workspace [ObservationSubject SubjectFile "src/Revision.hs"] (T.replicate 40 "a") body)

reviewSha :: T.Text
reviewSha = T.replicate 40 "b"

expectRejected :: IO a -> IO ()
expectRejected action = do
  result <- try @DBException action
  case result of
    Left (DBCheckViolation _) -> pure ()
    other -> expectationFailure $ "Expected DBCheckViolation, got " <> either show (const "success") other

bool :: TestEnv -> B8.ByteString -> IO Bool
bool env sql = runSession env.pool $ Session.statement () $ Statement.Statement sql Enc.noParams (Dec.singleRow (Dec.column (Dec.nonNullable Dec.bool))) False

allEffects :: TestEnv -> UUID -> IO [Value]
allEffects env identifier = runSession env.pool $ Session.statement () $ Statement.Statement
  ("SELECT to_jsonb(o) FROM observations o WHERE id='" <> B8.pack (show identifier) <> "' UNION ALL SELECT to_jsonb(e) FROM observation_revision_events e WHERE observation_id='" <> B8.pack (show identifier) <> "' UNION ALL SELECT to_jsonb(j) FROM embedding_jobs j WHERE observation_id='" <> B8.pack (show identifier) <> "'")
  Enc.noParams (Dec.rowList (Dec.column (Dec.nonNullable Dec.jsonb))) False
