module HMem.DB.BotIdentitySpec (spec) where

import Control.Exception (SomeException, bracket, try)
import Control.Monad (forM_)
import Data.Aeson (Value(..), object, toJSON, (.=))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Char8 qualified as B8
import Data.Pool (destroyAllResources)
import Data.Text qualified as T
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Hasql.Decoders qualified as Dec
import Hasql.Encoders qualified as Enc
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement
import System.Directory (copyFile, createDirectory, listDirectory)
import System.FilePath ((</>))
import Test.Hspec

import HMem.BotIdentity
import HMem.DB.Audit
import HMem.DB.ChangeStream
import HMem.DB.Migration qualified as Migration
import HMem.DB.Observation
import HMem.DB.Pool
import HMem.DB.Project qualified as Project
import HMem.DB.RequestContext
import HMem.DB.Task qualified as Task
import HMem.DB.TestHarness
import HMem.DB.Timeline
import HMem.Types

spec :: Spec
spec = do
  around withTestEnv $ describe "declared identity persistence" $ do
    it "keeps immutable audit revision current-provenance and outbox snapshots in parity" $ \env -> do
      workspace <- createTestWorkspace env "identity-parity"
      let human = Principal ActorUser "local-user" "Local User" PrincipalSyntheticLocalSuperadmin
          credential = Principal ActorBot "token-distinct-from-grant" "Token Bot" (PrincipalGrantUser UUID.nil)
          cases = [(human, Nothing), (human, Just identity), (credential, Just identity)]
      forM_ cases $ \(original, declaration) -> do
        let actor = resolveEffectiveActor True original declaration
        row <- withPrincipalContext (Just original) $ withEffectiveActorContext (Just actor) $
          create env workspace.id "first"
        updated <- withPrincipalContext (Just original) $ withEffectiveActorContext (Just actor) $ do
          ObservationUpdated result <- updateObservationReviewed env.pool workspace.id row.id row.contentVersion
            (ReviewedObservationUpdate "second" (T.replicate 40 "b"))
          pure result
        [latest, first] <- listObservationHistory env.pool workspace.id row.id Nothing Nothing
        updated.currentProvenance `shouldBe` Just latest.provenance
        row.currentProvenance `shouldBe` Just first.provenance
        forM_ [first, latest] $ \event -> do
          event.provenance.actorId `shouldBe` Just actor.actorId
          event.provenance.actorType `shouldBe` Just (actorTypeToText actor.actorType)
          event.provenance.botIdentity `shouldBe` declaration
        audits <- getAuditByEntity env.pool "observation" (UUID.toText row.id) Nothing
        length audits `shouldBe` 2
        map (.actorId) audits `shouldBe` replicate 2 (Just actor.actorId)
        map (.botIdentity) audits `shouldBe` replicate 2 declaration
        events <- listOutboxAfter env.pool (WorkspaceScope workspace.id) 0 100
        let ours = filter (\record -> field ["entity", "id"] record.outboxEnvelope == Just (toJSON row.id)) events
        length ours `shouldBe` 2
        forM_ ours $ \record -> do
          field ["actor", "id"] record.outboxEnvelope `shouldBe` Just (toJSON actor.actorId)
          field ["actor", "type"] record.outboxEnvelope `shouldBe` Just (String (if actor.actorType == ActorBot then "service" else "user"))
          field ["actor", "bot_identity"] record.outboxEnvelope `shouldBe` fmap toJSON declaration
        raw <- json env ("SELECT to_jsonb(current_provenance = hmem_observation_event_projection(e)) AS equal FROM observations o JOIN observation_revision_events e ON e.observation_id=o.id AND e.sequence=o.latest_sequence WHERE o.id='" <> show row.id <> "'")
        raw `shouldBe` [Bool True]

    it "attributes custom project audits cascade task writes and timeline actors to one declared operation" $ \env -> do
      workspace <- createTestWorkspace env "identity-cascade"
      root <- Project.createProject env.pool (CreateProject workspace.id Nothing "root" Nothing Nothing Nothing)
      child <- Project.createProject env.pool (CreateProject workspace.id (Just root.id) "child" Nothing Nothing Nothing)
      task <- Task.createTask env.pool (CreateTask workspace.id (Just child.id) Nothing "task" Nothing Nothing Nothing)
      let original = Principal ActorUser "local-user" "Local User" PrincipalSyntheticLocalSuperadmin
          actor = resolveEffectiveActor True original (Just identity)
      withPrincipalContext (Just original) $ withEffectiveActorContext (Just actor) $
        Project.updateProject env.pool root.id (UpdateProject Nothing Unchanged Unchanged (Just ProjArchived) Nothing Nothing) >> pure ()
      forM_ [("project", root.id), ("project", child.id), ("task", task.id)] $ \(kind, identifier) -> do
        audits <- getAuditByEntity env.pool kind (UUID.toText identifier) Nothing
        let changed = filter ((== AuditUpdate) . (.action)) audits
        changed `shouldSatisfy` (not . null)
        map (.actorId) changed `shouldBe` replicate (length changed) (Just actor.actorId)
        map (.botIdentity) changed `shouldBe` replicate (length changed) (Just identity)
      timeline <- listWorkspaceTimeline env.pool workspace.id Nothing Nothing Nothing Nothing Nothing Nothing
      let changed = filter (\event -> event.eventType `elem` ["project_archived", "task_cancelled"]) timeline
      length changed `shouldBe` 3
      map (fmap (.botIdentity) . (.actor)) changed `shouldBe` replicate 3 (Just (Just identity))

    it "clears pooled actor and declaration settings after success failure and absent context" $ \env ->
      bracket (createPool env.testDb.testDbConnStr 1 30 30000) destroyAllResources $ \pool -> do
        let original = Principal ActorUser "local-user" "Local User" PrincipalSyntheticLocalSuperadmin
            actor = resolveEffectiveActor True original (Just identity)
            settings = Session.statement () (Statement.Statement
              "SELECT jsonb_build_object('actor_id',nullif(current_setting('hmem.actor_id',true),''),'identity',nullif(current_setting('hmem.bot_identity',true),'')::jsonb)"
              Enc.noParams (Dec.singleRow (Dec.column (Dec.nonNullable Dec.jsonb))) True)
        withPrincipalContext (Just original) $ withEffectiveActorContext (Just actor) $ do
          runSession pool settings `shouldReturn` object ["actor_id" .= actor.actorId, "identity" .= identity]
          failure <- try (runSession pool (Session.sql "SELECT 1/0")) :: IO (Either SomeException ())
          failure `shouldSatisfy` either (const True) (const False)
        withPrincipalContext (Just original) $ runSession pool settings
          `shouldReturn` object ["actor_id" .= original.actorId, "identity" .= Null]
        runSession pool settings `shouldReturn` object ["actor_id" .= Null, "identity" .= Null]

  describe "declared identity migration" $
    it "upgrades V038 without rewriting history and atomically rolls back failed ledger registration before retry" $
      withTestSandbox $ \sandbox -> withSandboxedEnv sandbox $ withSandboxedPostgres sandbox $ \db ->
        bracket (createPool db.testDbConnStr 2 30 30000) destroyAllResources $ \pool -> do
          source <- resolveMigrationsDir sandbox.sandboxRepoRoot
          let beforeDir = sandbox.sandboxRoot </> "before-identity"
              upgradeDir = sandbox.sandboxRoot </> "identity-only"
              filename = "V039__declared_bot_identity.sql"
              env = TestEnv pool sandbox db
          createDirectory beforeDir
          createDirectory upgradeDir
          files <- listDirectory source
          forM_ (filter (\name -> name < "V039" && ".sql" `T.isSuffixOf` T.pack name) files) $ \name -> copyFile (source </> name) (beforeDir </> name)
          copyFile (source </> filename) (upgradeDir </> filename)
          before <- Migration.runMigrations pool beforeDir
          before.failed `shouldBe` Nothing
          workspace <- createTestWorkspace env "before-identity"
          row <- create env workspace.id "historical"
          oldEvents <- json env "SELECT to_jsonb(e) FROM observation_revision_events e ORDER BY observation_id,sequence"
          oldAudits <- json env "SELECT to_jsonb(a) FROM audit_log a ORDER BY id"
          oldProjection <- json env "SELECT current_provenance FROM observations"
          runSession pool $ Session.sql "CREATE FUNCTION reject_identity_ledger() RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN IF NEW.version=39 THEN RAISE EXCEPTION 'late identity ledger failure'; END IF; RETURN NEW; END; $$; CREATE TRIGGER reject_identity_ledger BEFORE INSERT ON schema_migrations FOR EACH ROW EXECUTE FUNCTION reject_identity_ledger()"
          failed <- Migration.runMigrations pool upgradeDir
          failed.failed `shouldSatisfy` (/= Nothing)
          json env "SELECT to_jsonb(NOT EXISTS(SELECT 1 FROM schema_migrations WHERE version=39) AND NOT EXISTS(SELECT 1 FROM information_schema.columns WHERE table_name='audit_log' AND column_name='bot_identity') AND to_regprocedure('hmem_observation_event_projection(observation_revision_events)') IS NULL)" `shouldReturn` [Bool True]
          runSession pool $ Session.sql "DROP TRIGGER reject_identity_ledger ON schema_migrations; DROP FUNCTION reject_identity_ledger()"
          upgraded <- Migration.runMigrations pool upgradeDir
          upgraded.failed `shouldBe` Nothing
          upgraded.applied `shouldBe` [filename]
          json env "SELECT to_jsonb(e)-'bot_identity' FROM observation_revision_events e ORDER BY observation_id,sequence" `shouldReturn` oldEvents
          json env "SELECT to_jsonb(a)-'bot_identity' FROM audit_log a ORDER BY id" `shouldReturn` oldAudits
          json env "SELECT current_provenance FROM observations" `shouldReturn` oldProjection
          [historical] <- listObservationHistory pool workspace.id row.id Nothing Nothing
          historical.provenance.botIdentity `shouldBe` Nothing
          row.currentProvenance `shouldBe` Just historical.provenance
          let original = Principal ActorUser "local-user" "Local User" PrincipalSyntheticLocalSuperadmin
          withPrincipalContext (Just original) $ withEffectiveActorContext (Just (resolveEffectiveActor True original (Just identity))) $ do
            ObservationUpdated updated <- updateObservationReviewed pool workspace.id row.id row.contentVersion
              (ReviewedObservationUpdate row.content (T.replicate 40 "b"))
            fmap (.botIdentity) updated.currentProvenance `shouldBe` Just (Just identity)
          [newest, preserved] <- listObservationHistory pool workspace.id row.id Nothing Nothing
          newest.provenance.sequence `shouldBe` 2
          preserved `shouldBe` historical
          repeated <- Migration.runMigrations pool upgradeDir
          repeated.failed `shouldBe` Nothing
          repeated.applied `shouldBe` []

identity :: BotIdentity
identity = either (error . T.unpack) id (mkBotIdentity "Developer" "pi:gpt-6.1-sol:high")

create :: TestEnv -> UUID -> T.Text -> IO Observation
create env workspace content = createObservation env.pool
  (CreateObservation workspace [ObservationSubject SubjectFile "src/Identity.hs"] (T.replicate 40 "a") content)

json :: TestEnv -> String -> IO [Value]
json env sql = runSession env.pool $ Session.statement () $
  Statement.Statement (B8.pack sql) Enc.noParams (Dec.rowList (Dec.column (Dec.nonNullable Dec.jsonb))) False

field :: [T.Text] -> Value -> Maybe Value
field [] value = Just value
field (key:rest) (Object fields) = KM.lookup (Key.fromText key) fields >>= field rest
field _ _ = Nothing
