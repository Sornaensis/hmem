module HMem.DB.ProjectSpec (spec) where

import Data.Aeson (Value(..), object, (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as AesonKey
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Maybe (isJust)
import Data.Functor.Contravariant (contramap)
import Control.Exception (bracket_, try)
import Data.Text (Text)
import Data.Text qualified as T
import Data.UUID (UUID)
import Hasql.Decoders qualified as Dec
import Hasql.Encoders qualified as Enc
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement
import Test.Hspec

import HMem.DB.ChangeStream
import HMem.DB.Observation
import HMem.DB.Overview (getTaskOverview, getProjectOverview, projectCardSummary, projectCardSummaries)
import HMem.DB.Pool (runSession, runTransaction, DBException(..))
import HMem.DB.Project
import HMem.DB.Task qualified as Task
import HMem.DB.TestHarness
import HMem.DB.WorkspaceGroup qualified as WorkspaceGroup
import HMem.Types

spec :: Spec
spec = do
  around withTestEnv $ describe "Project pause audit provenance" $ do
    it "captures per-row marker removal, no-ops and later writes in the same transaction" $ \env -> do
      workspace <- createTestWorkspace env "pause-audit-multirow"
      root <- pauseProject env workspace.id Nothing "root"
      _ <- updateProject env.pool root.id (archiveStatusUpdate ProjPaused)
      first <- pauseProject env workspace.id (Just root.id) "first"
      second <- pauseProject env workspace.id (Just root.id) "second"
      runSession env.pool $ Session.statement [first.id, second.id] $ Statement.Statement
        "INSERT INTO project_retained_pause_intents SELECT unnest($1::uuid[])"
        (Enc.param (Enc.nonNullable (Enc.foldableArray (Enc.nonNullable Enc.uuid)))) Dec.noResult True
      beforeFirst <- getAuditLogRows env.pool "project" (T.pack (show first.id))
      beforeSecond <- getAuditLogRows env.pool "project" (T.pack (show second.id))
      runTransaction env.pool $ do
        Session.statement [first.id, second.id] $ Statement.Statement
          "UPDATE projects SET status='active' WHERE id=ANY($1)"
          (Enc.param (Enc.nonNullable (Enc.foldableArray (Enc.nonNullable Enc.uuid)))) Dec.noResult True
        Session.statement first.id touchProjectStatement
        Session.statement second.id $ Statement.Statement
          "UPDATE projects SET name='later write' WHERE id=$1"
          (Enc.param (Enc.nonNullable Enc.uuid)) Dec.noResult True
        Session.sql "DO $$ BEGIN IF coalesce(current_setting('hmem.project_pause_audit_capture',true),'{}')::jsonb <> '{}'::jsonb THEN RAISE EXCEPTION 'capture leaked'; END IF; END $$;"
      firstRows <- runSession env.pool $ Session.statement first.id pauseAuditSnapshotsStatement
      secondRows <- runSession env.pool $ Session.statement second.id pauseAuditSnapshotsStatement
      let transitions rows = [ row | row <- rows
            , (field "old" row >>= field "retained_paused") == Just (Bool True)
            , (field "new" row >>= field "retained_paused") == Just (Bool False) ]
      length (transitions firstRows) `shouldBe` 1
      length (transitions secondRows) `shouldBe` 1
      length firstRows `shouldBe` length beforeFirst + 2 -- explicit requested row + effective inherited reconciliation; no-op suppressed
      length secondRows `shouldBe` length beforeSecond + 3 -- additionally the independent later metadata write
      runSession env.pool (Session.statement () pauseCaptureEmptyStatement) `shouldReturn` True

    it "clears conflict-only INSERT captures before later writes in the same transaction" $ \env -> do
      workspace <- createTestWorkspace env "pause-audit-conflict"
      root <- pauseProject env workspace.id Nothing "root"
      before <- getAuditLogRows env.pool "project" (T.pack (show root.id))
      runTransaction env.pool $ do
        Session.statement root.id $ Statement.Statement
          "INSERT INTO projects(id,workspace_id,name) SELECT id,workspace_id,name FROM projects WHERE id=$1 ON CONFLICT(id) DO NOTHING"
          (Enc.param (Enc.nonNullable Enc.uuid)) Dec.noResult True
        Session.sql "DO $$ BEGIN IF coalesce(current_setting('hmem.project_pause_audit_capture',true),'{}')::jsonb <> '{}'::jsonb THEN RAISE EXCEPTION 'capture leaked'; END IF; END $$;"
        Session.statement root.id touchProjectStatement
      getAuditLogRows env.pool "project" (T.pack (show root.id)) `shouldReturn` before
      runSession env.pool (Session.statement () pauseCaptureEmptyStatement) `shouldReturn` True

    it "clears upsert and zero-row statement captures before subsequent writes" $ \env -> do
      workspace <- createTestWorkspace env "pause-audit-upsert"
      root <- pauseProject env workspace.id Nothing "root"
      before <- getAuditLogRows env.pool "project" (T.pack (show root.id))
      runTransaction env.pool $ do
        Session.statement root.id $ Statement.Statement
          "INSERT INTO projects(id,workspace_id,name) SELECT id,workspace_id,'upserted' FROM projects WHERE id=$1 ON CONFLICT(id) DO UPDATE SET name=excluded.name"
          (Enc.param (Enc.nonNullable Enc.uuid)) Dec.noResult True
        Session.statement () pauseCaptureEmptyStatement >>= \empty -> if empty then pure () else error "upsert capture leaked"
        Session.sql "UPDATE projects SET name=name WHERE false; INSERT INTO projects(workspace_id,name) SELECT workspace_id,name FROM projects WHERE false; DELETE FROM projects WHERE false"
        Session.statement () pauseCaptureEmptyStatement >>= \empty -> if empty then pure () else error "zero-row capture leaked"
        Session.statement root.id touchProjectStatement
      after <- getAuditLogRows env.pool "project" (T.pack (show root.id))
      length after `shouldBe` length before + 1
      runSession env.pool (Session.statement () pauseCaptureEmptyStatement) `shouldReturn` True

    it "clears a suppressed row capture without producing an audit or affecting later writes" $ \env -> do
      workspace <- createTestWorkspace env "pause-audit-suppressed"
      root <- pauseProject env workspace.id Nothing "root"
      before <- getAuditLogRows env.pool "project" (T.pack (show root.id))
      runTransaction env.pool $ do
        Session.sql "CREATE FUNCTION suppress_pause_audit_fixture() RETURNS trigger AS $$ BEGIN RETURN NULL; END $$ LANGUAGE plpgsql; CREATE TRIGGER zzzzz_suppress_pause_audit_fixture BEFORE UPDATE ON projects FOR EACH ROW EXECUTE FUNCTION suppress_pause_audit_fixture()"
        Session.statement root.id touchProjectStatement
        Session.sql "DROP TRIGGER zzzzz_suppress_pause_audit_fixture ON projects; DROP FUNCTION suppress_pause_audit_fixture()"
        Session.statement () pauseCaptureEmptyStatement >>= \empty -> if empty then pure () else error "suppressed capture leaked"
        Session.statement root.id touchProjectStatement
      getAuditLogRows env.pool "project" (T.pack (show root.id)) `shouldReturn` before
      runSession env.pool (Session.statement () pauseCaptureEmptyStatement) `shouldReturn` True

    it "restores capture scope after a rejected pause and allows a later same-transaction write" $ \env -> do
      workspace <- createTestWorkspace env "pause-audit-rejected"
      root <- pauseProject env workspace.id Nothing "root"
      task <- pauseTask env workspace.id (Just root.id) Nothing "running"
      _ <- Task.updateTask env.pool task.id (pauseTaskStatus InProgress)
      before <- getAuditLogRows env.pool "project" (T.pack (show root.id))
      runTransaction env.pool $ do
        _ <- Session.statement (T.pack (show root.id)) $ Statement.Statement
          "SELECT set_config('hmem.test_pause_target',$1,true)" (Enc.param (Enc.nonNullable Enc.text))
          (Dec.singleRow (Dec.column (Dec.nonNullable Dec.text))) True
        Session.sql "DO $$ BEGIN BEGIN UPDATE projects SET status='paused' WHERE id=current_setting('hmem.test_pause_target')::uuid; RAISE EXCEPTION 'expected running refusal'; EXCEPTION WHEN SQLSTATE 'HM204' THEN NULL; END; END $$"
        Session.statement () pauseCaptureEmptyStatement >>= \empty -> if empty then pure () else error "rejected capture leaked"
        Session.statement root.id $ Statement.Statement "UPDATE projects SET name='after rejection' WHERE id=$1"
          (Enc.param (Enc.nonNullable Enc.uuid)) Dec.noResult True
      after <- getAuditLogRows env.pool "project" (T.pack (show root.id))
      length after `shouldBe` length before + 1
      fmap (fmap (.status)) (getProject env.pool root.id) `shouldReturn` Just ProjActive
      runSession env.pool (Session.statement () pauseCaptureEmptyStatement) `shouldReturn` True

    it "preserves pending outer marker snapshots during parent-before-child materialization" $ \env -> do
      workspace <- createTestWorkspace env "pause-audit-pending"
      closed <- pauseProject env workspace.id Nothing "closed"
      parent <- pauseProject env workspace.id (Just closed.id) "moving parent"
      child <- pauseProject env workspace.id (Just parent.id) "pending child"
      runTransaction env.pool $ do
        Session.sql "ALTER TABLE projects DISABLE TRIGGER USER"
        Session.statement closed.id archiveProjectStatement
        Session.statement [child.id] $ Statement.Statement
          "UPDATE projects SET status='paused',own_paused=false WHERE id=ANY($1)"
          (Enc.param (Enc.nonNullable (Enc.foldableArray (Enc.nonNullable Enc.uuid)))) Dec.noResult True
        Session.sql "ALTER TABLE projects ENABLE TRIGGER USER"
        Session.statement [child.id] $ Statement.Statement
          "INSERT INTO project_retained_pause_intents SELECT unnest($1::uuid[])"
          (Enc.param (Enc.nonNullable (Enc.foldableArray (Enc.nonNullable Enc.uuid)))) Dec.noResult True
      beforeRows <- runSession env.pool $ Session.statement child.id pauseAuditSnapshotsStatement
      runTransaction env.pool $ do
        Session.sql "CREATE FUNCTION assert_pending_pause_capture() RETURNS trigger AS $$ BEGIN IF current_setting('hmem.retained_pause_materializing',true)='true' AND NOT EXISTS (SELECT 1 FROM jsonb_each(coalesce(nullif(current_setting('hmem.project_pause_audit_capture',true),''),'{}')::jsonb) entry WHERE entry.value->>'project_id'=NEW.id::text AND entry.key <> ('UPDATE:' || TG_RELID::text || ':' || OLD.ctid::text)) THEN RAISE EXCEPTION 'parent-before-child fixture did not preserve pending outer row'; END IF; RETURN NEW; END $$ LANGUAGE plpgsql; CREATE TRIGGER zz_assert_pending_pause_capture BEFORE UPDATE OF own_paused ON projects FOR EACH ROW EXECUTE FUNCTION assert_pending_pause_capture(); SET LOCAL enable_indexscan=off; SET LOCAL enable_bitmapscan=off;"
        Session.statement [parent.id,child.id] $ Statement.Statement
          "UPDATE projects SET parent_id=CASE WHEN id=($1::uuid[])[1] THEN NULL ELSE parent_id END,name=name WHERE id=ANY($1)"
          (Enc.param (Enc.nonNullable (Enc.foldableArray (Enc.nonNullable Enc.uuid)))) Dec.noResult True
        Session.sql "DROP TRIGGER zz_assert_pending_pause_capture ON projects; DROP FUNCTION assert_pending_pause_capture();"
      rows <- runSession env.pool $ Session.statement child.id pauseAuditSnapshotsStatement
      length rows `shouldBe` length beforeRows + 1
      let materialized row = (field "old" row >>= field "own_paused") == Just (Bool False)
            && (field "old" row >>= field "retained_paused") == Just (Bool True)
            && (field "new" row >>= field "own_paused") == Just (Bool True)
            && (field "new" row >>= field "retained_paused") == Just (Bool False)
      length (filter materialized rows) `shouldBe` 1
      runSession env.pool (Session.statement () pauseCaptureEmptyStatement) `shouldReturn` True

    it "correlates deferred FK hard-delete audits with their captured tuple versions" $ \env -> do
      workspace <- createTestWorkspace env "pause-audit-fk-purge"
      parent <- pauseProject env workspace.id Nothing "parent"
      child <- pauseProject env workspace.id (Just parent.id) "child"
      runSession env.pool $ Session.statement child.id $ Statement.Statement
        "INSERT INTO project_retained_pause_intents(project_id) VALUES ($1)"
        (Enc.param (Enc.nonNullable Enc.uuid)) Dec.noResult True
      runTransaction env.pool $ Session.statement parent.id $ Statement.Statement
        "DELETE FROM projects WHERE id=$1" (Enc.param (Enc.nonNullable Enc.uuid)) Dec.noResult True
      rows <- runSession env.pool $ Session.statement child.id pauseAuditSnapshotsStatement
      let deleted row = field "new" row == Just Null
            && (field "old" row >>= field "retained_paused") == Just (Bool True)
      length (filter deleted rows) `shouldBe` 1
      runSession env.pool (Session.statement () pauseCaptureEmptyStatement) `shouldReturn` True

    it "records settled marker consumption during nested materialization" $ \env -> do
      workspace <- createTestWorkspace env "pause-audit-materialization"
      root <- pauseProject env workspace.id Nothing "root"
      _ <- updateProject env.pool root.id (archiveStatusUpdate ProjPaused)
      child <- pauseProject env workspace.id (Just root.id) "child"
      runSession env.pool $ Session.statement [child.id] $ Statement.Statement
        "INSERT INTO project_retained_pause_intents SELECT unnest($1::uuid[])"
        (Enc.param (Enc.nonNullable (Enc.foldableArray (Enc.nonNullable Enc.uuid)))) Dec.noResult True
      _ <- updateProject env.pool root.id (archiveStatusUpdate ProjActive)
      rows <- runSession env.pool $ Session.statement child.id pauseAuditSnapshotsStatement
      let settled row = (field "old" row >>= field "own_paused") == Just (Bool False)
            && (field "old" row >>= field "retained_paused") == Just (Bool True)
            && (field "new" row >>= field "own_paused") == Just (Bool True)
            && (field "new" row >>= field "retained_paused") == Just (Bool False)
      length (filter settled rows) `shouldBe` 1
      getProject env.pool child.id >>= \value -> fmap (.status) value `shouldBe` Just ProjPaused
      runSession env.pool (Session.statement () pauseCaptureEmptyStatement) `shouldReturn` True

    it "rejects foreign or deleted parents in ordinary and private mutations atomically" $ \env -> do
      workspace <- createTestWorkspace env "pause-parent-scope"
      foreignWorkspace <- createTestWorkspace env "pause-parent-foreign"
      root <- pauseProject env workspace.id Nothing "root"
      foreignParent <- pauseProject env foreignWorkspace.id Nothing "foreign"
      deletedParent <- pauseProject env workspace.id Nothing "deleted"
      _ <- deleteProject env.pool deletedParent.id
      before <- getProject env.pool root.id
      audits <- getAuditLogRows env.pool "project" (T.pack (show root.id))
      events <- listOutboxAfter env.pool (WorkspaceScope workspace.id) 0 100
      let move :: UUID -> UpdateProject
          move parentId = (archiveStatusUpdate ProjActive) { parentId = SetTo parentId }
      mapM_ (\parent -> do
        updateProject env.pool root.id (move parent) `shouldThrow` anyException
        updateProjectForAudit env.pool workspace.id root.id (move parent)
          (Just (ProjectAuditPauseIntent False False)) `shouldThrow` anyException
        getProject env.pool root.id `shouldReturn` before
        getAuditLogRows env.pool "project" (T.pack (show root.id)) `shouldReturn` audits
        listOutboxAfter env.pool (WorkspaceScope workspace.id) 0 100 `shouldReturn` events)
        [foreignParent.id, deletedParent.id]

    it "allows pure resume of legacy running work while genuine pause still refuses frozen descendants" $ \env -> do
      workspace <- createTestWorkspace env "pause-legacy-running-resume"
      mapM_ (\frozenBranch -> do
        root <- pauseProject env workspace.id Nothing "root"
        branch <- pauseProject env workspace.id (Just root.id) "branch"
        leaf <- pauseProject env workspace.id (Just branch.id) "leaf"
        running <- pauseTask env workspace.id (Just leaf.id) Nothing "historical running"
        runTransaction env.pool $ do
          Session.sql "ALTER TABLE projects DISABLE TRIGGER USER; ALTER TABLE tasks DISABLE TRIGGER USER"
          Session.statement root.id $ Statement.Statement "UPDATE projects SET status='paused',own_paused=true WHERE id=$1"
            (Enc.param (Enc.nonNullable Enc.uuid)) Dec.noResult True
          Session.statement leaf.id $ Statement.Statement "UPDATE projects SET status='paused',own_paused=false WHERE id=$1"
            (Enc.param (Enc.nonNullable Enc.uuid)) Dec.noResult True
          if frozenBranch then Session.statement branch.id archiveProjectStatement else
            Session.statement branch.id $ Statement.Statement "UPDATE projects SET status='paused',own_paused=false WHERE id=$1"
              (Enc.param (Enc.nonNullable Enc.uuid)) Dec.noResult True
          Session.statement running.id $ Statement.Statement "UPDATE tasks SET status='in_progress',auto_blocked=false WHERE id=$1"
            (Enc.param (Enc.nonNullable Enc.uuid)) Dec.noResult True
          Session.sql "ALTER TABLE tasks ENABLE TRIGGER USER; ALTER TABLE projects ENABLE TRIGGER USER"
          if frozenBranch then Session.statement leaf.id $ Statement.Statement
            "INSERT INTO project_retained_pause_intents(project_id) VALUES ($1)"
            (Enc.param (Enc.nonNullable Enc.uuid)) Dec.noResult True else pure ()
        taskBefore <- Task.getTask env.pool running.id
        leafBefore <- getProject env.pool leaf.id
        resumed <- updateProject env.pool root.id (archiveStatusUpdate ProjActive)
        fmap (.status) resumed `shouldBe` Just ProjActive
        Task.getTask env.pool running.id `shouldReturn` taskBefore
        if frozenBranch then getProject env.pool leaf.id `shouldReturn` leafBefore else
          fmap (fmap (.status)) (getProject env.pool leaf.id) `shouldReturn` Just ProjActive
        rejected <- try @DBException $ updateProject env.pool root.id (archiveStatusUpdate ProjPaused)
        rejected `shouldSatisfy` \case Left (DBLifecycleViolation "PROJECT_PAUSE_BLOCKED_BY_RUNNING_TASKS" _ _ _) -> True; _ -> False
        fmap (fmap (.status)) (getProject env.pool root.id) `shouldReturn` Just ProjActive
        Task.getTask env.pool running.id `shouldReturn` taskBefore) [False,True]

    it "isolates mixed resume/pause rows and prevents same-transaction resume capability reuse" $ \env -> do
      workspace <- createTestWorkspace env "pause-resume-capability"
      resumed <- pauseProject env workspace.id Nothing "resume"
      paused <- pauseProject env workspace.id Nothing "pause"
      firstTask <- pauseTask env workspace.id (Just resumed.id) Nothing "legacy running"
      secondTask <- pauseTask env workspace.id (Just paused.id) Nothing "running"
      runTransaction env.pool $ do
        Session.sql "ALTER TABLE projects DISABLE TRIGGER USER; ALTER TABLE tasks DISABLE TRIGGER USER"
        Session.statement resumed.id $ Statement.Statement "UPDATE projects SET status='paused',own_paused=true WHERE id=$1"
          (Enc.param (Enc.nonNullable Enc.uuid)) Dec.noResult True
        Session.statement [firstTask.id,secondTask.id] $ Statement.Statement "UPDATE tasks SET status='in_progress',auto_blocked=false WHERE id=ANY($1)"
          (Enc.param (Enc.nonNullable (Enc.foldableArray (Enc.nonNullable Enc.uuid)))) Dec.noResult True
        Session.sql "ALTER TABLE tasks ENABLE TRIGGER USER; ALTER TABLE projects ENABLE TRIGGER USER"
      before <- traverse (getProject env.pool) [resumed.id,paused.id]
      events <- listOutboxAfter env.pool (WorkspaceScope workspace.id) 0 100
      rejected <- try @DBException $ runTransaction env.pool $ Session.statement [resumed.id,paused.id] $ Statement.Statement
        "UPDATE projects SET status=CASE WHEN id=($1::uuid[])[1] THEN 'active'::project_status_enum ELSE 'paused'::project_status_enum END WHERE id=ANY($1)"
        (Enc.param (Enc.nonNullable (Enc.foldableArray (Enc.nonNullable Enc.uuid)))) Dec.noResult True
      rejected `shouldSatisfy` \case Left (DBLifecycleViolation "PROJECT_PAUSE_BLOCKED_BY_RUNNING_TASKS" _ _ _) -> True; _ -> False
      traverse (getProject env.pool) [resumed.id,paused.id] `shouldReturn` before
      listOutboxAfter env.pool (WorkspaceScope workspace.id) 0 100 `shouldReturn` events
      runTransaction env.pool $ do
        Session.statement resumed.id $ Statement.Statement "UPDATE projects SET status='active' WHERE id=$1"
          (Enc.param (Enc.nonNullable Enc.uuid)) Dec.noResult True
        Session.statement () pauseCaptureEmptyStatement >>= \empty -> if empty then pure () else error "resume candidate leaked"
        _ <- Session.statement (T.pack (show resumed.id)) $ Statement.Statement
          "SELECT set_config('hmem.test_pause_target',$1,true)" (Enc.param (Enc.nonNullable Enc.text))
          (Dec.singleRow (Dec.column (Dec.nonNullable Dec.text))) True
        Session.sql "DO $$ BEGIN BEGIN UPDATE projects SET own_paused=true WHERE id=current_setting('hmem.test_pause_target')::uuid; RAISE EXCEPTION 'resume authority leaked into later pause'; EXCEPTION WHEN SQLSTATE 'HM204' THEN NULL; END; END $$"
        Session.statement () pauseCaptureEmptyStatement >>= \empty -> if empty then pure () else error "later rejected candidate leaked"
        Session.statement resumed.id $ Statement.Statement "UPDATE projects SET status='active' WHERE id=$1"
          (Enc.param (Enc.nonNullable Enc.uuid)) Dec.noResult True
        Session.statement resumed.id $ Statement.Statement "UPDATE projects SET own_paused=own_paused,name=name WHERE id=$1"
          (Enc.param (Enc.nonNullable Enc.uuid)) Dec.noResult True
        Session.statement () pauseCaptureEmptyStatement >>= \empty -> if empty then pure () else error "repeated/no-op candidate leaked"
      fmap (fmap (.status)) (getProject env.pool resumed.id) `shouldReturn` Just ProjActive
      fmap (fmap (.status)) (Task.getTask env.pool firstTask.id) `shouldReturn` Just InProgress

    it "refuses a descendant pause before its parent's same-statement resume can mask original running work" $ \env -> do
      workspace <- createTestWorkspace env "pause-hierarchical-mixed"
      parent <- pauseProject env workspace.id Nothing "parent"
      _ <- updateProject env.pool parent.id (archiveStatusUpdate ProjPaused)
      child <- pauseProject env workspace.id (Just parent.id) "child"
      running <- pauseTask env workspace.id (Just child.id) Nothing "original running"
      runTransaction env.pool $ do
        Session.sql "ALTER TABLE tasks DISABLE TRIGGER USER"
        Session.statement running.id $ Statement.Statement "UPDATE tasks SET status='in_progress',auto_blocked=false WHERE id=$1"
          (Enc.param (Enc.nonNullable Enc.uuid)) Dec.noResult True
        Session.sql "ALTER TABLE tasks ENABLE TRIGGER USER"
      before <- traverse (getProject env.pool) [parent.id,child.id]
      taskBefore <- Task.getTask env.pool running.id
      audits <- traverse (\id -> getAuditLogRows env.pool "project" (T.pack (show id))) [parent.id,child.id]
      taskAudit <- getAuditLogRows env.pool "task" (T.pack (show running.id))
      events <- listOutboxAfter env.pool (WorkspaceScope workspace.id) 0 100
      rejected <- try @DBException $ runTransaction env.pool $ do
        _ <- Session.statement (T.pack (show child.id)) $ Statement.Statement
          "SELECT set_config('hmem.test_pending_pause_child',$1,true)" (Enc.param (Enc.nonNullable Enc.text))
          (Dec.singleRow (Dec.column (Dec.nonNullable Dec.text))) True
        Session.sql "CREATE FUNCTION witness_parent_resume_mask() RETURNS trigger AS $$ BEGIN IF OLD.status='in_progress' AND NEW.status='blocked' THEN IF NOT EXISTS (SELECT 1 FROM jsonb_each(coalesce(nullif(current_setting('hmem.project_pause_audit_capture',true),''),'{}')::jsonb) entry WHERE entry.value->'resume'->>'id'=current_setting('hmem.test_pending_pause_child') AND entry.value->'resume'->'requested'='true'::jsonb AND entry.value->'resume'->'active'='false'::jsonb) THEN RAISE EXCEPTION 'parent-before-descendant fixture not established'; END IF; PERFORM set_config('hmem.test_parent_mask_witness','true',true); END IF; RETURN NEW; END $$ LANGUAGE plpgsql; CREATE TRIGGER zz_witness_parent_resume_mask BEFORE UPDATE OF status ON tasks FOR EACH ROW EXECUTE FUNCTION witness_parent_resume_mask(); SET LOCAL enable_indexscan=off; SET LOCAL enable_bitmapscan=off;"
        Session.statement [parent.id,child.id] $ Statement.Statement
          "UPDATE projects SET status=CASE WHEN id=($1::uuid[])[1] THEN 'active'::project_status_enum ELSE 'paused'::project_status_enum END WHERE id=ANY($1)"
          (Enc.param (Enc.nonNullable (Enc.foldableArray (Enc.nonNullable Enc.uuid)))) Dec.noResult True
        witnessed <- Session.statement () $ Statement.Statement
          "SELECT current_setting('hmem.test_parent_mask_witness',true)='true'" Enc.noParams
          (Dec.singleRow (Dec.column (Dec.nonNullable Dec.bool))) True
        if witnessed then pure () else error "controlled parent-first path did not execute"
        Session.sql "DROP TRIGGER zz_witness_parent_resume_mask ON tasks; DROP FUNCTION witness_parent_resume_mask()"
      rejected `shouldSatisfy` \case Left (DBLifecycleViolation "PROJECT_PAUSE_BLOCKED_BY_RUNNING_TASKS" _ _ _) -> True; _ -> False
      traverse (getProject env.pool) [parent.id,child.id] `shouldReturn` before
      Task.getTask env.pool running.id `shouldReturn` taskBefore
      traverse (\id -> getAuditLogRows env.pool "project" (T.pack (show id))) [parent.id,child.id] `shouldReturn` audits
      getAuditLogRows env.pool "task" (T.pack (show running.id)) `shouldReturn` taskAudit
      listOutboxAfter env.pool (WorkspaceScope workspace.id) 0 100 `shouldReturn` events
      runSession env.pool (Session.statement () pauseCaptureEmptyStatement) `shouldReturn` True

    it "refuses repeated true own pause intent with original running rows and no events" $ \env -> do
      workspace <- createTestWorkspace env "pause-repeated-own-true"
      root <- pauseProject env workspace.id Nothing "root"
      _ <- updateProject env.pool root.id (archiveStatusUpdate ProjPaused)
      running <- pauseTask env workspace.id (Just root.id) Nothing "original running"
      runTransaction env.pool $ do
        Session.sql "ALTER TABLE tasks DISABLE TRIGGER USER"
        Session.statement running.id $ Statement.Statement "UPDATE tasks SET status='in_progress',auto_blocked=false WHERE id=$1"
          (Enc.param (Enc.nonNullable Enc.uuid)) Dec.noResult True
        Session.sql "ALTER TABLE tasks ENABLE TRIGGER USER"
      before <- getProject env.pool root.id
      taskBefore <- Task.getTask env.pool running.id
      audits <- getAuditLogRows env.pool "project" (T.pack (show root.id))
      events <- listOutboxAfter env.pool (WorkspaceScope workspace.id) 0 100
      rejected <- try @DBException $ runSession env.pool $ Session.statement (root.id,True) ownPauseProjectStatement
      rejected `shouldSatisfy` \case Left (DBLifecycleViolation "PROJECT_PAUSE_BLOCKED_BY_RUNNING_TASKS" _ _ _) -> True; _ -> False
      getProject env.pool root.id `shouldReturn` before
      Task.getTask env.pool running.id `shouldReturn` taskBefore
      getAuditLogRows env.pool "project" (T.pack (show root.id)) `shouldReturn` audits
      listOutboxAfter env.pool (WorkspaceScope workspace.id) 0 100 `shouldReturn` events
      runSession env.pool (Session.statement () pauseCaptureEmptyStatement) `shouldReturn` True

    it "keeps harmless and suppressed descendant captures from refusing a same-statement parent resume" $ \env -> do
      workspace <- createTestWorkspace env "pause-harmless-mixed"
      mapM_ (\suppressChild -> do
        parent <- pauseProject env workspace.id Nothing "parent"
        _ <- updateProject env.pool parent.id (archiveStatusUpdate ProjPaused)
        child <- pauseProject env workspace.id (Just parent.id) "child"
        running <- pauseTask env workspace.id (Just child.id) Nothing "legacy running"
        runTransaction env.pool $ do
          Session.sql "ALTER TABLE tasks DISABLE TRIGGER USER"
          Session.statement running.id $ Statement.Statement "UPDATE tasks SET status='in_progress',auto_blocked=false WHERE id=$1"
            (Enc.param (Enc.nonNullable Enc.uuid)) Dec.noResult True
          Session.sql "ALTER TABLE tasks ENABLE TRIGGER USER"
        taskBefore <- Task.getTask env.pool running.id
        taskAudits <- getAuditLogRows env.pool "task" (T.pack (show running.id))
        runTransaction env.pool $ do
          if suppressChild then Session.sql "CREATE FUNCTION suppress_descendant_pause_fixture() RETURNS trigger AS $$ BEGIN IF NEW.name='child' AND NEW.own_paused THEN RETURN NULL; END IF; RETURN NEW; END $$ LANGUAGE plpgsql; CREATE TRIGGER zzzzz_suppress_descendant_pause_fixture BEFORE UPDATE ON projects FOR EACH ROW EXECUTE FUNCTION suppress_descendant_pause_fixture()" else pure ()
          Session.sql "SET LOCAL enable_indexscan=off; SET LOCAL enable_bitmapscan=off"
          Session.statement ([parent.id,child.id],suppressChild) $ Statement.Statement
            "UPDATE projects SET own_paused=CASE WHEN id=($1::uuid[])[1] THEN false WHEN $2 THEN true ELSE own_paused END,name=name WHERE id=ANY($1)"
            (contramap fst (Enc.param (Enc.nonNullable (Enc.foldableArray (Enc.nonNullable Enc.uuid)))) <> contramap snd (Enc.param (Enc.nonNullable Enc.bool))) Dec.noResult True
          if suppressChild then Session.sql "DROP TRIGGER zzzzz_suppress_descendant_pause_fixture ON projects; DROP FUNCTION suppress_descendant_pause_fixture()" else pure ()
          Session.statement () pauseCaptureEmptyStatement >>= \empty -> if empty then pure () else error "mixed metadata/suppressed capture leaked"
        fmap (fmap (.status)) (getProject env.pool parent.id) `shouldReturn` Just ProjActive
        fmap (fmap (.status)) (getProject env.pool child.id) `shouldReturn` Just ProjActive
        Task.getTask env.pool running.id `shouldReturn` taskBefore
        getAuditLogRows env.pool "task" (T.pack (show running.id)) `shouldReturn` taskAudits
        childAudits <- runSession env.pool $ Session.statement child.id pauseAuditSnapshotsStatement
        filter (\row -> (field "new" row >>= field "own_paused") == Just (Bool True)) childAudits `shouldBe` []) [False,True]

    it "rolls back deferred replay intent and events when a frozen descendant is running" $ \env -> do
      workspace <- createTestWorkspace env "pause-deferred-replay-running"
      ancestor <- pauseProject env workspace.id Nothing "deleted ancestor"
      child <- pauseProject env workspace.id (Just ancestor.id) "legacy child"
      running <- pauseTask env workspace.id (Just child.id) Nothing "running"
      _ <- Task.updateTask env.pool running.id (pauseTaskStatus InProgress)
      runTransaction env.pool $ do
        Session.sql "ALTER TABLE projects DISABLE TRIGGER USER"
        Session.statement ancestor.id $ Statement.Statement "UPDATE projects SET deleted_at=now() WHERE id=$1"
          (Enc.param (Enc.nonNullable Enc.uuid)) Dec.noResult True
        Session.sql "ALTER TABLE projects ENABLE TRIGGER USER"
      before <- getProject env.pool child.id
      taskBefore <- Task.getTask env.pool running.id
      audits <- getAuditLogRows env.pool "project" (T.pack (show child.id))
      events <- listOutboxAfter env.pool (WorkspaceScope workspace.id) 0 100
      rejected <- try @DBException $ updateProjectForAudit env.pool workspace.id child.id
        (archiveStatusUpdate ProjPaused) (Just (ProjectAuditPauseIntent False True))
      rejected `shouldSatisfy` \case Left (DBLifecycleViolation "PROJECT_PAUSE_BLOCKED_BY_RUNNING_TASKS" _ _ _) -> True; _ -> False
      getProject env.pool child.id `shouldReturn` before
      Task.getTask env.pool running.id `shouldReturn` taskBefore
      getAuditLogRows env.pool "project" (T.pack (show child.id)) `shouldReturn` audits
      listOutboxAfter env.pool (WorkspaceScope workspace.id) 0 100 `shouldReturn` events
      runSession env.pool (Session.statement child.id $ Statement.Statement
        "SELECT NOT EXISTS(SELECT 1 FROM project_retained_pause_intents WHERE project_id=$1)"
        (Enc.param (Enc.nonNullable Enc.uuid)) (Dec.singleRow (Dec.column (Dec.nonNullable Dec.bool))) True) `shouldReturn` True
      runSession env.pool (Session.statement () pauseCaptureEmptyStatement) `shouldReturn` True

    it "binds private replay to the workspace and restores inherited false intent" $ \env -> do
      workspace <- createTestWorkspace env "pause-audit-private"
      foreignWorkspace <- createTestWorkspace env "pause-audit-foreign"
      root <- pauseProject env workspace.id Nothing "root"
      _ <- updateProject env.pool root.id (archiveStatusUpdate ProjPaused)
      child <- pauseProject env workspace.id (Just root.id) "child"
      before <- getProject env.pool child.id
      updateProjectForAudit env.pool foreignWorkspace.id child.id (archiveStatusUpdate ProjPaused)
        (Just (ProjectAuditPauseIntent True False)) `shouldReturn` Nothing
      getProject env.pool child.id `shouldReturn` before
      _ <- updateProject env.pool child.id (archiveStatusUpdate ProjPaused)
      _ <- updateProjectForAudit env.pool workspace.id child.id (archiveStatusUpdate ProjPaused)
        (Just (ProjectAuditPauseIntent False False))
      _ <- updateProject env.pool root.id (archiveStatusUpdate ProjActive)
      fmap (fmap (.status)) (getProject env.pool child.id) `shouldReturn` Just ProjActive

  around withTestEnv $ describe "Project pause lifecycle" $ do
    it "pauses deep open descendants and hierarchy-only subtasks while preserving closed and terminal rows" $ \env -> do
      workspace <- createTestWorkspace env "pause-deep"
      root <- pauseProject env workspace.id Nothing "root"
      child <- pauseProject env workspace.id (Just root.id) "child"
      grandchild <- pauseProject env workspace.id (Just child.id) "grandchild"
      closed <- pauseProject env workspace.id (Just child.id) "closed"
      _ <- updateProject env.pool closed.id (archiveStatusUpdate ProjCompleted)
      parent <- pauseTask env workspace.id (Just grandchild.id) Nothing "parent"
      subtask <- runSession env.pool $ Session.statement (workspace.id, parent.id) unassignedArchiveChildStatement
      doneTask <- pauseTask env workspace.id (Just child.id) Nothing "done"
      cancelled <- pauseTask env workspace.id (Just child.id) Nothing "cancelled"
      _ <- Task.updateTask env.pool doneTask.id (pauseTaskStatus Done)
      _ <- Task.updateTask env.pool cancelled.id (pauseTaskStatus Cancelled)
      beforeDone <- Task.getTask env.pool doneTask.id
      beforeCancelled <- Task.getTask env.pool cancelled.id
      beforeClosed <- getProject env.pool closed.id
      updated <- updateProject env.pool root.id (archiveStatusUpdate ProjPaused)
      fmap (.status) updated `shouldBe` Just ProjPaused
      mapM (fmap (fmap (.status)) . getProject env.pool) [root.id, child.id, grandchild.id]
        `shouldReturn` replicate 3 (Just ProjPaused)
      mapM (fmap (fmap (.status)) . Task.getTask env.pool) [parent.id, subtask]
        `shouldReturn` replicate 2 (Just Blocked)
      Just overview <- getTaskOverview env.pool parent.id
      overview.readinessRollup.canStartSubtasks `shouldBe` False
      Task.getTask env.pool doneTask.id `shouldReturn` beforeDone
      Task.getTask env.pool cancelled.id `shouldReturn` beforeCancelled
      getProject env.pool closed.id `shouldReturn` beforeClosed
      _ <- updateProject env.pool root.id (archiveStatusUpdate ProjActive)
      fmap (fmap (.status)) (Task.getTask env.pool subtask) `shouldReturn` Just Todo
      fmap (fmap (.status)) (Task.getTask env.pool parent.id) `shouldReturn` Just Blocked
      Task.getTask env.pool doneTask.id `shouldReturn` beforeDone
      Task.getTask env.pool cancelled.id `shouldReturn` beforeCancelled

    it "preserves frozen historical project and task branches while refusing their running descendants" $ \env -> do
      workspace <- createTestWorkspace env "pause-frozen-history"
      root <- pauseProject env workspace.id Nothing "root"
      closed <- pauseProject env workspace.id (Just root.id) "closed branch"
      historical <- pauseProject env workspace.id (Just closed.id) "historical open grandchild"
      waiting <- pauseTask env workspace.id (Just historical.id) Nothing "frozen waiting"
      terminalParent <- pauseTask env workspace.id (Just root.id) Nothing "terminal parent"
      child <- pauseTask env workspace.id (Just root.id) (Just terminalParent.id) "historical open child"
      bracket_
        (runSession env.pool $ Session.sql "ALTER TABLE projects DISABLE TRIGGER USER; ALTER TABLE tasks DISABLE TRIGGER USER")
        (runSession env.pool $ Session.sql "ALTER TABLE projects ENABLE TRIGGER USER; ALTER TABLE tasks ENABLE TRIGGER USER") $ do
          _ <- updateProject env.pool closed.id (archiveStatusUpdate ProjCompleted)
          runSession env.pool $ Session.statement terminalParent.id freezePauseTaskStatement
      projectsBefore <- mapM (getProject env.pool) [closed.id, historical.id]
      tasksBefore <- mapM (Task.getTask env.pool) [waiting.id, terminalParent.id, child.id]
      auditsBefore <- mapM (getAuditLogRows env.pool "project" . T.pack . show) [closed.id, historical.id]
      taskAuditsBefore <- mapM (getAuditLogRows env.pool "task" . T.pack . show) [waiting.id, terminalParent.id, child.id]
      let preserve = do
            mapM (getProject env.pool) [closed.id, historical.id] `shouldReturn` projectsBefore
            mapM (Task.getTask env.pool) [waiting.id, terminalParent.id, child.id] `shouldReturn` tasksBefore
            mapM (getAuditLogRows env.pool "project" . T.pack . show) [closed.id, historical.id] `shouldReturn` auditsBefore
            mapM (getAuditLogRows env.pool "task" . T.pack . show) [waiting.id, terminalParent.id, child.id] `shouldReturn` taskAuditsBefore
      before <- listOutboxAfter env.pool (WorkspaceScope workspace.id) 0 100
      _ <- updateProject env.pool root.id (archiveStatusUpdate ProjPaused)
      preserve
      _ <- updateProject env.pool root.id (archiveStatusUpdate ProjActive)
      preserve
      after <- listOutboxAfter env.pool (WorkspaceScope workspace.id) 0 100
      length after `shouldBe` length before + 2 -- only the explicit root pause/resume
      bracket_
        (runSession env.pool $ Session.sql "ALTER TABLE tasks DISABLE TRIGGER USER")
        (runSession env.pool $ Session.sql "ALTER TABLE tasks ENABLE TRIGGER USER") $
        runSession env.pool $ Session.statement waiting.id startFrozenPauseTaskStatement
      refused <- try @DBException $ updateProject env.pool root.id (archiveStatusUpdate ProjPaused)
      refused `shouldSatisfy` \case Left (DBLifecycleViolation "PROJECT_PAUSE_BLOCKED_BY_RUNNING_TASKS" _ _ _) -> True; _ -> False
      fmap (fmap (.status)) (getProject env.pool root.id) `shouldReturn` Just ProjActive
      listOutboxAfter env.pool (WorkspaceScope workspace.id) 0 100 `shouldReturn` after

    it "retains independent child pauses and manual/dependency/subtask blockers on resume" $ \env -> do
      workspace <- createTestWorkspace env "pause-causes"
      root <- pauseProject env workspace.id Nothing "root"
      own <- pauseProject env workspace.id (Just root.id) "own"
      inherited <- pauseProject env workspace.id (Just root.id) "inherited"
      ready <- pauseTask env workspace.id (Just inherited.id) Nothing "ready"
      manual <- pauseTask env workspace.id (Just inherited.id) Nothing "manual"
      dependent <- pauseTask env workspace.id (Just inherited.id) Nothing "dependent"
      prerequisite <- pauseTask env workspace.id (Just inherited.id) Nothing "prerequisite"
      ownTask <- pauseTask env workspace.id (Just own.id) Nothing "own task"
      _ <- Task.updateTask env.pool manual.id (pauseTaskStatus Blocked)
      Task.addDependency env.pool dependent.id prerequisite.id
      _ <- updateProject env.pool own.id (archiveStatusUpdate ProjPaused)
      _ <- updateProject env.pool root.id (archiveStatusUpdate ProjPaused)
      -- Metadata-only updates of an inherited paused project must not own it.
      _ <- updateProject env.pool inherited.id (UpdateProject (Just "renamed") Unchanged Unchanged Nothing Nothing Nothing)
      _ <- updateProject env.pool root.id (archiveStatusUpdate ProjActive)
      mapM (fmap (fmap (.status)) . getProject env.pool) [root.id, inherited.id, own.id]
        `shouldReturn` [Just ProjActive, Just ProjActive, Just ProjPaused]
      mapM (fmap (fmap (.status)) . Task.getTask env.pool) [ready.id, manual.id, dependent.id, ownTask.id]
        `shouldReturn` [Just Todo, Just Blocked, Just Blocked, Just Blocked]
      _ <- updateProject env.pool own.id (archiveStatusUpdate ProjActive)
      fmap (fmap (.status)) (Task.getTask env.pool ownTask.id) `shouldReturn` Just Todo
      _ <- Task.updateTask env.pool prerequisite.id (pauseTaskStatus Done)
      fmap (fmap (.status)) (Task.getTask env.pool dependent.id) `shouldReturn` Just Todo
      fmap (fmap (.status)) (Task.getTask env.pool manual.id) `shouldReturn` Just Blocked

    it "honors repeated own intent, overlapping SQL targets and remaining paused ancestry" $ \env -> do
      workspace <- createTestWorkspace env "pause-overlap"
      root <- pauseProject env workspace.id Nothing "root"
      child <- pauseProject env workspace.id (Just root.id) "child"
      runSession env.pool $ Session.statement [root.id, child.id] pauseProjectsStatement
      _ <- updateProject env.pool root.id (archiveStatusUpdate ProjActive)
      fmap (fmap (.status)) (getProject env.pool child.id) `shouldReturn` Just ProjPaused
      _ <- updateProject env.pool root.id (archiveStatusUpdate ProjPaused)
      updated <- updateProject env.pool child.id (archiveStatusUpdate ProjActive)
      fmap (.status) updated `shouldBe` Just ProjPaused
      _ <- updateProject env.pool root.id (archiveStatusUpdate ProjActive)
      fmap (fmap (.status)) (getProject env.pool child.id) `shouldReturn` Just ProjActive
      _ <- updateProject env.pool root.id (archiveStatusUpdate ProjPaused)
      _ <- updateProject env.pool child.id (archiveStatusUpdate ProjPaused)
      _ <- updateProject env.pool root.id (archiveStatusUpdate ProjActive)
      fmap (fmap (.status)) (getProject env.pool child.id) `shouldReturn` Just ProjPaused

    it "counts running frozen descendants equally in single and batch pause readiness" $ \env -> do
      workspace <- createTestWorkspace env "pause-frozen-running-rollup"
      root <- pauseProject env workspace.id Nothing "root"
      frozen <- pauseProject env workspace.id (Just root.id) "frozen"
      parent <- pauseTask env workspace.id (Just frozen.id) Nothing "parent"
      child <- pauseTask env workspace.id (Just frozen.id) (Just parent.id) "running subtask"
      _ <- Task.updateTask env.pool child.id (pauseTaskStatus InProgress)
      runTransaction env.pool $ do
        Session.sql "ALTER TABLE projects DISABLE TRIGGER USER"
        Session.statement frozen.id archiveProjectStatement
        Session.sql "ALTER TABLE projects ENABLE TRIGGER USER"
      single <- projectCardSummary env.pool root
      batch <- projectCardSummaries env.pool [root]
      batch `shouldBe` [single]
      single.readinessRollup.inProgressTaskCount `shouldBe` 1
      overview <- getProjectOverview env.pool root.id
      fmap (\value -> value.readinessRollup.inProgressTaskCount) overview `shouldBe` Just 1
      rejected <- try @DBException $ updateProject env.pool root.id (archiveStatusUpdate ProjPaused)
      rejected `shouldSatisfy` \case Left (DBLifecycleViolation "PROJECT_PAUSE_BLOCKED_BY_RUNNING_TASKS" _ _ _) -> True; _ -> False

    it "rejects pause atomically for a running descendant subtask with unchanged audit/outbox" $ \env -> do
      workspace <- createTestWorkspace env "pause-running"
      root <- pauseProject env workspace.id Nothing "root"
      child <- pauseProject env workspace.id (Just root.id) "child"
      parent <- pauseTask env workspace.id (Just child.id) Nothing "parent"
      subtask <- pauseTask env workspace.id (Just child.id) (Just parent.id) "running child"
      _ <- Task.updateTask env.pool subtask.id (pauseTaskStatus InProgress)
      beforeEvents <- listOutboxAfter env.pool (WorkspaceScope workspace.id) 0 100
      beforeAudit <- getAuditLogRows env.pool "project" (T.pack (show root.id))
      pause <- try @DBException $ updateProject env.pool root.id (archiveStatusUpdate ProjPaused)
      pause `shouldSatisfy` \case Left (DBLifecycleViolation "PROJECT_PAUSE_BLOCKED_BY_RUNNING_TASKS" _ _ _) -> True; _ -> False
      mapM (fmap (fmap (.status)) . getProject env.pool) [root.id, child.id]
        `shouldReturn` replicate 2 (Just ProjActive)
      fmap (fmap (.status)) (Task.getTask env.pool subtask.id) `shouldReturn` Just InProgress
      listOutboxAfter env.pool (WorkspaceScope workspace.id) 0 100 `shouldReturn` beforeEvents
      getAuditLogRows env.pool "project" (T.pack (show root.id)) `shouldReturn` beforeAudit

    it "requires reopening a closed root before pausing" $ \env -> do
      workspace <- createTestWorkspace env "pause-closed"
      root <- pauseProject env workspace.id Nothing "root"
      _ <- updateProject env.pool root.id (archiveStatusUpdate ProjCompleted)
      rejected <- try @DBException $ updateProject env.pool root.id (archiveStatusUpdate ProjPaused)
      rejected `shouldSatisfy` \case Left (DBLifecycleViolation "PROJECT_PAUSE_REQUIRES_REOPEN" _ _ _) -> True; _ -> False
      _ <- updateProject env.pool root.id (archiveStatusUpdate ProjActive)
      fmap (fmap (.status)) (updateProject env.pool root.id (archiveStatusUpdate ProjPaused)) `shouldReturn` Just ProjPaused

    it "guards own-intent-only SQL writes and restores false intent on an already inherited paused row" $ \env -> do
      workspace <- createTestWorkspace env "pause-own-authority"
      root <- pauseProject env workspace.id Nothing "root"
      child <- pauseProject env workspace.id (Just root.id) "child"
      task <- pauseTask env workspace.id (Just child.id) Nothing "task"
      _ <- Task.updateTask env.pool task.id (pauseTaskStatus InProgress)
      rejected <- try @DBException $ runSession env.pool $ Session.statement (root.id, True) ownPauseProjectStatement
      rejected `shouldSatisfy` \case Left (DBLifecycleViolation "PROJECT_PAUSE_BLOCKED_BY_RUNNING_TASKS" _ _ _) -> True; _ -> False
      fmap (fmap (.status)) (getProject env.pool root.id) `shouldReturn` Just ProjActive
      _ <- Task.updateTask env.pool task.id (pauseTaskStatus Todo)
      runSession env.pool $ Session.statement (root.id, True) ownPauseProjectStatement
      fmap (fmap (.status)) (getProject env.pool child.id) `shouldReturn` Just ProjPaused
      runSession env.pool $ Session.statement (child.id, False) ownPauseProjectStatement
      -- Replaying effective paused plus own=false must retain inherited intent,
      -- including false->false, rather than treating it as a repeated own pause.
      runSession env.pool $ do
        Session.statement child.id setRestoredPauseIntentStatement
        Session.statement [child.id] pauseProjectsStatement
        Session.sql "SELECT set_config('hmem.project_pause_intent', '', true)"
      runSession env.pool $ Session.statement (root.id, False) ownPauseProjectStatement
      mapM (fmap (fmap (.status)) . getProject env.pool) [root.id, child.id]
        `shouldReturn` replicate 2 (Just ProjActive)
      fmap (fmap (.status)) (Task.getTask env.pool task.id) `shouldReturn` Just Todo
      _ <- Task.updateTask env.pool task.id (pauseTaskStatus Done)
      _ <- updateProject env.pool child.id (archiveStatusUpdate ProjCompleted)
      closed <- try @DBException $ runSession env.pool $ Session.statement (child.id, True) ownPauseProjectStatement
      closed `shouldSatisfy` \case Left (DBLifecycleViolation "PROJECT_PAUSE_REQUIRES_REOPEN" _ _ _) -> True; _ -> False

    it "reconciles canonical creation, project moves, batch task moves and inherited restores" $ \env -> do
      workspace <- createTestWorkspace env "pause-placement"
      paused <- pauseProject env workspace.id Nothing "paused"
      active <- pauseProject env workspace.id Nothing "active"
      moving <- pauseProject env workspace.id (Just active.id) "moving"
      task <- pauseTask env workspace.id (Just moving.id) Nothing "moving task"
      _ <- updateProject env.pool paused.id (archiveStatusUpdate ProjPaused)
      inherited <- pauseProject env workspace.id (Just paused.id) "inherited"
      inherited.status `shouldBe` ProjPaused
      created <- pauseTask env workspace.id (Just inherited.id) Nothing "created blocked"
      created.status `shouldBe` Blocked
      _ <- updateProject env.pool moving.id (UpdateProject Nothing Unchanged (SetTo paused.id) Nothing Nothing Nothing)
      fmap (fmap (.status)) (Task.getTask env.pool task.id) `shouldReturn` Just Blocked
      _ <- updateProject env.pool moving.id (UpdateProject Nothing Unchanged (SetTo active.id) Nothing Nothing Nothing)
      fmap (fmap (.status)) (Task.getTask env.pool task.id) `shouldReturn` Just Todo
      _ <- Task.moveTasksBatch env.pool [task.id] (Just inherited.id)
      fmap (fmap (.status)) (Task.getTask env.pool task.id) `shouldReturn` Just Blocked
      _ <- Task.moveTasksBatch env.pool [task.id] (Just active.id)
      fmap (fmap (.status)) (Task.getTask env.pool task.id) `shouldReturn` Just Todo
      _ <- deleteProject env.pool inherited.id
      restoreProject env.pool inherited.id `shouldReturn` True
      _ <- updateProject env.pool paused.id (archiveStatusUpdate ProjActive)
      fmap (fmap (.status)) (getProject env.pool inherited.id) `shouldReturn` Just ProjActive
      fmap (fmap (.status)) (Task.getTask env.pool created.id) `shouldReturn` Just Todo

    it "refuses task starts, running subtree moves and restores into paused ancestry" $ \env -> do
      workspace <- createTestWorkspace env "pause-running-placement"
      paused <- pauseProject env workspace.id Nothing "paused"
      active <- pauseProject env workspace.id Nothing "active"
      waiting <- pauseTask env workspace.id (Just paused.id) Nothing "waiting"
      moving <- pauseTask env workspace.id (Just active.id) Nothing "moving"
      _ <- Task.updateTask env.pool moving.id (pauseTaskStatus InProgress)
      _ <- updateProject env.pool paused.id (archiveStatusUpdate ProjPaused)
      let rejected action = do
            result <- try @DBException action
            result `shouldSatisfy` \case Left (DBLifecycleViolation "TASK_RUNNING_UNDER_PAUSED_PROJECT" _ _ _) -> True; _ -> False
      rejected $ Task.updateTask env.pool waiting.id (pauseTaskStatus InProgress)
      rejected $ Task.moveTasksBatch env.pool [moving.id] (Just paused.id)
      _ <- Task.deleteTask env.pool moving.id
      -- Restore uses the current project's ancestry, including a moved project.
      _ <- updateProject env.pool active.id (UpdateProject Nothing Unchanged (SetTo paused.id) Nothing Nothing Nothing)
      rejected $ Task.restoreTask env.pool moving.id
      fmap (fmap (.status)) (Task.getTask env.pool waiting.id) `shouldReturn` Just Blocked

    it "protects indirect done dependents during pause/resume but resets origin before unrelated dependency edits" $ \env -> do
      workspace <- createTestWorkspace env "pause-terminal-propagation"
      root <- pauseProject env workspace.id Nothing "root"
      prerequisite <- pauseTask env workspace.id (Just root.id) Nothing "prerequisite"
      doneTask <- pauseTask env workspace.id (Just root.id) Nothing "done dependent"
      indirect <- pauseTask env workspace.id (Just root.id) Nothing "indirect done"
      _ <- Task.updateTask env.pool prerequisite.id (pauseTaskStatus Done)
      Task.addDependency env.pool doneTask.id prerequisite.id
      _ <- Task.updateTask env.pool doneTask.id (pauseTaskStatus Done)
      Task.addDependency env.pool indirect.id doneTask.id
      _ <- Task.updateTask env.pool indirect.id (pauseTaskStatus Done)
      -- Legacy done dependents of an open prerequisite, impossible to create
      -- through normal readiness recomputation, expose V029's transitive branch.
      bracket_
        (runSession env.pool $ Session.sql "ALTER TABLE tasks DISABLE TRIGGER USER")
        (runSession env.pool $ Session.sql "ALTER TABLE tasks ENABLE TRIGGER USER") $
        runSession env.pool $ Session.statement prerequisite.id pauseReopenTaskStatement
      beforeDone <- Task.getTask env.pool doneTask.id
      beforeIndirect <- Task.getTask env.pool indirect.id
      _ <- updateProject env.pool root.id (archiveStatusUpdate ProjPaused)
      Task.getTask env.pool doneTask.id `shouldReturn` beforeDone
      Task.getTask env.pool indirect.id `shouldReturn` beforeIndirect
      runSession env.pool $ do
        Session.statement root.id resumeProjectStatement
        -- The very next unrelated dependency trigger in this transaction must
        -- retain the existing ability to reopen done dependents.
        Session.statement (doneTask.id, prerequisite.id) pauseTouchDependencyStatement
      mapM (fmap (fmap (.status)) . Task.getTask env.pool) [doneTask.id, indirect.id]
        `shouldReturn` replicate 2 (Just Blocked)

    it "retains ordinary done-dependent reopening and dependency insertion inside paused ancestry" $ \env -> do
      workspace <- createTestWorkspace env "pause-unrelated-dependencies"
      root <- pauseProject env workspace.id Nothing "root"
      prerequisite <- pauseTask env workspace.id (Just root.id) Nothing "prerequisite"
      dependent <- pauseTask env workspace.id (Just root.id) Nothing "dependent"
      newDependent <- pauseTask env workspace.id (Just root.id) Nothing "new dependent"
      _ <- Task.updateTask env.pool prerequisite.id (pauseTaskStatus Done)
      Task.addDependency env.pool dependent.id prerequisite.id
      _ <- Task.updateTask env.pool dependent.id (pauseTaskStatus Done)
      _ <- Task.updateTask env.pool newDependent.id (pauseTaskStatus Done)
      _ <- updateProject env.pool root.id (archiveStatusUpdate ProjPaused)
      mapM (fmap (fmap (.status)) . Task.getTask env.pool) [dependent.id, newDependent.id]
        `shouldReturn` replicate 2 (Just Done)
      _ <- Task.updateTask env.pool prerequisite.id (pauseTaskStatus Todo)
      fmap (fmap (.status)) (Task.getTask env.pool dependent.id) `shouldReturn` Just Blocked
      Task.addDependency env.pool newDependent.id prerequisite.id
      fmap (fmap (.status)) (Task.getTask env.pool newDependent.id) `shouldReturn` Just Blocked

  beforeAll setupTestPool $ aroundWith withTestTransaction $ describe "Project lifecycle" $ do
    it "cascade archival archives completed descendant projects and cancels unfinished tasks without changing done rows" $ \env -> do
      workspace <- createTestWorkspace env "project-cascade-archive"
      let create parent title = createProject env.pool CreateProject
            { workspaceId = workspace.id, parentId = parent, name = title, description = Nothing, priority = Nothing, metadata = Nothing }
          task project parent title = Task.createTask env.pool CreateTask
            { workspaceId = workspace.id, projectId = Just project, parentId = parent, title = title, description = Nothing, priority = Nothing, metadata = Nothing }
          projectStatus status = UpdateProject Nothing Unchanged Unchanged (Just status) Nothing Nothing
          taskStatus status = UpdateTask Nothing Unchanged Unchanged Unchanged (Just status) Nothing Nothing
      root <- create Nothing "root"
      active <- create (Just root.id) "active child"
      completed <- create (Just active.id) "completed grandchild"
      _ <- updateProject env.pool completed.id (projectStatus ProjCompleted)
      parent <- task active.id Nothing "parent task"
      child <- task active.id (Just parent.id) "subtask"
      -- The DB permits a legacy/direct-SQL child without its parent's project.
      -- Archive must still follow the task hierarchy beyond project seed rows.
      unassignedChild <- runSession env.pool $ Session.statement (workspace.id, parent.id) unassignedArchiveChildStatement
      doneTask <- task active.id Nothing "done task"
      _ <- Task.updateTask env.pool doneTask.id (taskStatus Done)
      beforeDone <- Task.getTask env.pool doneTask.id
      _ <- updateProject env.pool root.id (projectStatus ProjArchived)
      mapM (fmap (fmap (.status)) . getProject env.pool) [root.id, active.id, completed.id]
        `shouldReturn` replicate 3 (Just ProjArchived)
      mapM (fmap (fmap (.status)) . Task.getTask env.pool) [parent.id, child.id, unassignedChild]
        `shouldReturn` replicate 3 (Just Cancelled)
      Task.getTask env.pool doneTask.id `shouldReturn` beforeDone
    it "cascade archival normalizes a historical root only on explicit status intent" $ \env -> do
      workspace <- createTestWorkspace env "project-cascade-repeat"
      root <- createProject env.pool (CreateProject workspace.id Nothing "root" Nothing Nothing Nothing)
      child <- createProject env.pool (CreateProject workspace.id (Just root.id) "completed" Nothing Nothing Nothing)
      _ <- updateProject env.pool child.id (archiveStatusUpdate ProjCompleted)
      -- Simulate already-archived historical state without sweeping it.
      bracket_
        (runSession env.pool $ Session.sql "ALTER TABLE projects DISABLE TRIGGER USER")
        (runSession env.pool $ Session.sql "ALTER TABLE projects ENABLE TRIGGER USER") $
        runSession env.pool $ Session.statement root.id archiveProjectStatement
      _ <- updateProject env.pool root.id ((archiveStatusUpdate ProjArchived) { status = Nothing, name = Just "renamed" })
      fmap (fmap (.status)) (getProject env.pool child.id) `shouldReturn` Just ProjCompleted
      _ <- updateProject env.pool root.id (archiveStatusUpdate ProjArchived)
      fmap (fmap (.status)) (getProject env.pool child.id) `shouldReturn` Just ProjArchived

    it "cascade archival from direct SQL preserves strict completion refusal" $ \env -> do
      workspace <- createTestWorkspace env "project-cascade-sql"
      root <- createProject env.pool (CreateProject workspace.id Nothing "root" Nothing Nothing Nothing)
      child <- createProject env.pool (CreateProject workspace.id (Just root.id) "child" Nothing Nothing Nothing)
      runSession env.pool $ Session.statement root.id archiveProjectStatement
      mapM (fmap (fmap (.status)) . getProject env.pool) [root.id, child.id] `shouldReturn` replicate 2 (Just ProjArchived)
      strict <- createProject env.pool (CreateProject workspace.id Nothing "strict completion" Nothing Nothing Nothing)
      _ <- createProject env.pool (CreateProject workspace.id (Just strict.id) "still open" Nothing Nothing Nothing)
      -- Expected SQL failures end this rollback-wrapped example: resource-pool
      -- discards the connection on exceptions, including its outer transaction.
      completion <- try (updateProject env.pool strict.id (archiveStatusUpdate ProjCompleted))
      completion `shouldSatisfy` (\case Left (DBLifecycleViolation "PROJECT_COMPLETION_BLOCKED" _ _ _) -> True; _ -> False)

    it "creates every initial task in order with project and task outbox events" $ \env -> do
      workspace <- createTestWorkspace env "project-spec-success"
      let input = CreateProjectSpec workspace.id "spec project" (Just "durable project description") (Just 7)
            [ ProjectSpecTask "first" (Just "first description") (Just 8)
            , ProjectSpecTask "second" Nothing Nothing
            , ProjectSpecTask "third" Nothing (Just 2) ]
      validateCreateProjectSpecInput input `shouldBe` []
      created <- createProjectSpec env.pool input
      project <- getProject env.pool created.projectId
      fmap (.name) project `shouldBe` Just "spec project"
      tasks <- mapM (Task.getTask env.pool) created.taskIds
      map (fmap (.title)) tasks `shouldBe` map Just ["first", "second", "third"]
      map (fmap (.projectId)) tasks `shouldBe` replicate 3 (Just (Just created.projectId))
      records <- listOutboxAfter env.pool (WorkspaceScope workspace.id) 0 10
      length records `shouldBe` 4
      let transactionId record = field "transaction" record.outboxEnvelope >>= field "id"
      map transactionId records `shouldSatisfy` \case [Just a, Just b, Just c, Just d] -> a == b && b == c && c == d; _ -> False

    it "does not mutate independent observations" $ \env -> do
      workspace <- createTestWorkspace env "project-observation-isolation"
      observation <- createObservation env.pool (CreateObservation workspace.id [ObservationSubject SubjectFile "src/Project.hs"] "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" "independent")
      project <- createProject env.pool CreateProject
        { workspaceId = workspace.id, parentId = Nothing, name = "project", description = Nothing, priority = Nothing, metadata = Nothing }
      deleteProjectCascade env.pool project.id >>= (`shouldSatisfy` isJust)
      getObservation env.pool workspace.id observation.id >>= (`shouldSatisfy` isJust)
    it "captures committed project mutations in the workspace outbox" $ \env -> do
      workspace <- createTestWorkspace env "project-change-stream"
      _ <- createProject env.pool CreateProject
        { workspaceId = workspace.id, parentId = Nothing, name = "project", description = Nothing, priority = Nothing, metadata = Nothing }
      records <- listOutboxAfter env.pool (WorkspaceScope workspace.id) 0 10
      case records of
        [record] -> record.outboxCursor `shouldBe` 1
        _ -> expectationFailure "expected exactly one project outbox record"
    it "publishes the exact sanitized project invalidations" $ \env -> do
      workspace <- createTestWorkspace env "project-change-stream-invalidations"
      project <- createProject env.pool CreateProject
        { workspaceId = workspace.id, parentId = Nothing, name = "project", description = Nothing, priority = Nothing, metadata = Nothing }
      records <- listOutboxAfter env.pool (WorkspaceScope workspace.id) 0 10
      case records of
        [record] -> case record.outboxEnvelope of
          Object envelope -> do
            KeyMap.lookup "invalidations" envelope `shouldBe` Just (toInvalidations workspace.id project.id)
            KeyMap.member "old_values" envelope `shouldBe` False
            KeyMap.member "new_values" envelope `shouldBe` False
            KeyMap.member "embedding" envelope `shouldBe` False
          _ -> expectationFailure "outbox envelope must be an object"
        _ -> expectationFailure "expected exactly one project outbox record"
    it "does not emit an outbox record for an ignored no-op update" $ \env -> do
      workspace <- createTestWorkspace env "project-change-stream-noop"
      project <- createProject env.pool CreateProject
        { workspaceId = workspace.id, parentId = Nothing, name = "project", description = Nothing, priority = Nothing, metadata = Nothing }
      runSession env.pool $ Session.statement project.id touchProjectStatement
      listOutboxAfter env.pool (WorkspaceScope workspace.id) 1 10 `shouldReturn` []
    it "records every project cascade with contiguous cursors and one transaction id" $ \env -> do
      workspace <- createTestWorkspace env "project-change-stream-cascade"
      root <- createProject env.pool CreateProject
        { workspaceId = workspace.id, parentId = Nothing, name = "root", description = Nothing, priority = Nothing, metadata = Nothing }
      _child <- createProject env.pool CreateProject
        { workspaceId = workspace.id, parentId = Just root.id, name = "child", description = Nothing, priority = Nothing, metadata = Nothing }
      deleteProjectCascade env.pool root.id >>= (`shouldSatisfy` isJust)
      records <- listOutboxAfter env.pool (WorkspaceScope workspace.id) 0 10
      map (.outboxCursor) records `shouldBe` [1, 2, 3, 4]
      let deleted = drop 2 records
          action record = Just record.outboxEnvelope >>= field "entity" >>= field "action"
          transactionId record = Just record.outboxEnvelope >>= field "transaction" >>= field "id"
      map action deleted `shouldBe` [Just (String "deleted"), Just (String "deleted")]
      map transactionId deleted `shouldSatisfy` \case [Just first, Just second] -> first == second; _ -> False
    it "captures task, observation, dependency, global group, and group-member writes with exact targets" $ \env -> do
      workspace <- createTestWorkspace env "change-stream-direct-matrix"
      project <- createProject env.pool CreateProject
        { workspaceId = workspace.id, parentId = Nothing, name = "project", description = Nothing, priority = Nothing, metadata = Nothing }
      prerequisite <- Task.createTask env.pool CreateTask
        { workspaceId = workspace.id, projectId = Just project.id, parentId = Nothing, title = "prerequisite", description = Nothing, priority = Nothing, metadata = Nothing }
      dependent <- Task.createTask env.pool CreateTask
        { workspaceId = workspace.id, projectId = Just project.id, parentId = Just prerequisite.id, title = "dependent", description = Nothing, priority = Nothing, metadata = Nothing }
      Task.addDependency env.pool dependent.id prerequisite.id
      _ <- createObservation env.pool (CreateObservation workspace.id [ObservationSubject SubjectFile "src/ChangeStream.hs"] "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" "captured")
      group <- WorkspaceGroup.createGroup env.pool (CreateWorkspaceGroup "change-stream-group" Nothing)
      _ <- WorkspaceGroup.addMember env.pool group.id workspace.id
      workspaceEvents <- listOutboxAfter env.pool (WorkspaceScope workspace.id) 0 50
      globalEvents <- listOutboxAfter env.pool GlobalScope 0 50
      let matching kind entityId = [record | record <- workspaceEvents, (field "entity" record.outboxEnvelope >>= field "type") == Just (String kind), (field "entity" record.outboxEnvelope >>= field "id") == Just (String entityId)]
          taskExpected = Aeson.toJSON
            [ object ["kind" .= ("entity" :: Text), "target" .= ("task:" <> T.pack (show prerequisite.id))]
            , object ["kind" .= ("collection" :: Text), "target" .= ("tasks:" <> T.pack (show workspace.id))]
            , object ["kind" .= ("tree" :: Text), "target" .= ("workspace:" <> T.pack (show workspace.id))]
            , object ["kind" .= ("readiness" :: Text), "target" .= ("task:" <> T.pack (show prerequisite.id))]
            , object ["kind" .= ("next_task" :: Text), "target" .= ("workspace:" <> T.pack (show workspace.id))]
            , object ["kind" .= ("search" :: Text), "target" .= ("workspace:" <> T.pack (show workspace.id))]
            , object ["kind" .= ("readiness" :: Text), "target" .= ("project:" <> T.pack (show project.id))]
            ]
          memberExpected = Aeson.toJSON
            [ object ["kind" .= ("entity" :: Text), "target" .= ("group_membership:" <> T.pack (show group.id) <> ":" <> T.pack (show workspace.id))]
            , object ["kind" .= ("collection" :: Text), "target" .= ("group:" <> T.pack (show group.id) <> ":members")]
            , object ["kind" .= ("collection" :: Text), "target" .= ("workspace:" <> T.pack (show workspace.id) <> ":groups")]
            ]
          globalInvalidations entityId = Aeson.toJSON
            [ object ["kind" .= ("entity" :: Text), "target" .= ("group:" <> entityId)]
            , object ["kind" .= ("collection" :: Text), "target" .= ("workspace-groups" :: Text)]
            ]
      case matching "task" (T.pack (show prerequisite.id)) of
        (record:_) -> field "invalidations" record.outboxEnvelope `shouldBe` Just taskExpected
        _ -> expectationFailure "missing direct task outbox record"
      case matching "workspace_group_membership" (T.pack (show group.id) <> ":" <> T.pack (show workspace.id)) of
        (record:_) -> field "invalidations" record.outboxEnvelope `shouldBe` Just memberExpected
        _ -> expectationFailure "missing direct group-member outbox record"
      let groupEvents =
            [ record
            | record <- globalEvents
            , (field "entity" record.outboxEnvelope >>= field "type") == Just (String "workspace_group")
            , (field "entity" record.outboxEnvelope >>= field "id") == Just (String (T.pack (show group.id)))
            ]
      case groupEvents of
        (record:_) -> field "invalidations" record.outboxEnvelope `shouldBe` Just (globalInvalidations (T.pack (show group.id)))
        _ -> expectationFailure "missing global workspace-group outbox record"
      let workspaceEventsGlobal = [record | record <- globalEvents, (field "entity" record.outboxEnvelope >>= field "type") == Just (String "workspace"), (field "entity" record.outboxEnvelope >>= field "id") == Just (String (T.pack (show workspace.id)))]
          workspaceExpected = Aeson.toJSON
            [ object ["kind" .= ("entity" :: Text), "target" .= ("workspace:" <> T.pack (show workspace.id))]
            , object ["kind" .= ("catalogue" :: Text), "target" .= ("workspace-catalog" :: Text)]
            , object ["kind" .= ("collection" :: Text), "target" .= ("workspace-groups" :: Text)]
            ]
      case workspaceEventsGlobal of
        (record:_) -> field "invalidations" record.outboxEnvelope `shouldBe` Just workspaceExpected
        _ -> expectationFailure "missing global workspace-catalogue outbox record"
      let workspaceCaptured = any (\record -> (field "entity" record.outboxEnvelope >>= field "type") == Just (String "observation")) workspaceEvents
          dependencyCaptured = any (\record -> (field "entity" record.outboxEnvelope >>= field "type") == Just (String "task_dependency")) workspaceEvents
      workspaceCaptured `shouldBe` True
      dependencyCaptured `shouldBe` True
    it "captures every task cascade in one transaction" $ \env -> do
      workspace <- createTestWorkspace env "task-change-stream-cascade"
      root <- Task.createTask env.pool CreateTask
        { workspaceId = workspace.id, projectId = Nothing, parentId = Nothing, title = "root", description = Nothing, priority = Nothing, metadata = Nothing }
      _child <- Task.createTask env.pool CreateTask
        { workspaceId = workspace.id, projectId = Nothing, parentId = Just root.id, title = "child", description = Nothing, priority = Nothing, metadata = Nothing }
      Task.deleteTaskCascade env.pool root.id >>= (`shouldSatisfy` isJust)
      records <- listOutboxAfter env.pool (WorkspaceScope workspace.id) 2 20
      let deleted = [record | record <- records, (field "entity" record.outboxEnvelope >>= field "action") == Just (String "deleted")]
          transactionId record = field "transaction" record.outboxEnvelope >>= field "id"
      length deleted `shouldBe` 2
      map transactionId deleted `shouldSatisfy` \case [Just first, Just second] -> first == second; _ -> False

  beforeAll setupTestPool $ describe "Project spec transaction rollback" $
    it "rolls back the project and all tasks when the middle insert fails" $ \env -> do
      workspace <- createTestWorkspace env "project-spec-rollback"
      let addConstraint = runSession env.pool $ Session.sql "ALTER TABLE tasks ADD CONSTRAINT project_spec_reject_middle CHECK (title <> 'reject-middle')"
          dropConstraint = runSession env.pool $ Session.sql "ALTER TABLE tasks DROP CONSTRAINT project_spec_reject_middle"
      bracket_ addConstraint dropConstraint $ do
        let input = CreateProjectSpec workspace.id "rolled-back project" Nothing Nothing
              [ ProjectSpecTask "first" Nothing Nothing
              , ProjectSpecTask "reject-middle" Nothing Nothing
              , ProjectSpecTask "third" Nothing Nothing ]
        result <- try @DBException (createProjectSpec env.pool input)
        result `shouldSatisfy` \case Left (DBCheckViolation _) -> True; _ -> False
        projects <- listProjects env.pool workspace.id Nothing Nothing Nothing
        projects `shouldBe` []
        tasks <- Task.listTasksByWorkspace env.pool workspace.id Nothing Nothing Nothing Nothing
        tasks `shouldBe` []
        listOutboxAfter env.pool (WorkspaceScope workspace.id) 0 10 `shouldReturn` []

toInvalidations :: UUID -> UUID -> Value
toInvalidations workspaceId projectId =
  let workspace = show workspaceId
      project = T.pack (show projectId)
  in Aeson.toJSON
    [ object ["kind" .= ("entity" :: Text), "target" .= ("project:" <> project)]
    , object ["kind" .= ("collection" :: Text), "target" .= ("projects:" <> T.pack workspace)]
    , object ["kind" .= ("tree" :: Text), "target" .= ("workspace:" <> T.pack workspace)]
    , object ["kind" .= ("readiness" :: Text), "target" .= ("project:" <> project)]
    , object ["kind" .= ("next_task" :: Text), "target" .= ("workspace:" <> T.pack workspace)]
    , object ["kind" .= ("search" :: Text), "target" .= ("workspace:" <> T.pack workspace)]
    ]

touchProjectStatement :: Statement.Statement UUID ()
touchProjectStatement = Statement.Statement
  "UPDATE projects SET updated_at = updated_at WHERE id = $1"
  (Enc.param (Enc.nonNullable Enc.uuid)) Dec.noResult True

field :: Text -> Value -> Maybe Value
field key = \case
  Object objectValue -> KeyMap.lookup (AesonKey.fromText key) objectValue
  _ -> Nothing

archiveStatusUpdate :: ProjectStatus -> UpdateProject
archiveStatusUpdate status = UpdateProject Nothing Unchanged Unchanged (Just status) Nothing Nothing

archiveProjectStatement :: Statement.Statement UUID ()
archiveProjectStatement = Statement.Statement
  "UPDATE projects SET status = 'archived' WHERE id = $1"
  (Enc.param (Enc.nonNullable Enc.uuid)) Dec.noResult True

unassignedArchiveChildStatement :: Statement.Statement (UUID, UUID) UUID
unassignedArchiveChildStatement = Statement.Statement
  "INSERT INTO tasks (workspace_id, parent_id, title) VALUES ($1, $2, 'unassigned hierarchy child') RETURNING id"
  (contramap fst (Enc.param (Enc.nonNullable Enc.uuid)) <> contramap snd (Enc.param (Enc.nonNullable Enc.uuid)))
  (Dec.singleRow (Dec.column (Dec.nonNullable Dec.uuid))) True

pauseProject :: TestEnv -> UUID -> Maybe UUID -> Text -> IO Project
pauseProject env workspace parent name = createProject env.pool (CreateProject workspace parent name Nothing Nothing Nothing)

pauseTask :: TestEnv -> UUID -> Maybe UUID -> Maybe UUID -> Text -> IO Task
pauseTask env workspace project parent title = Task.createTask env.pool (CreateTask workspace project parent title Nothing Nothing Nothing)

pauseTaskStatus :: TaskStatus -> UpdateTask
pauseTaskStatus status = UpdateTask Nothing Unchanged Unchanged Unchanged (Just status) Nothing Nothing

pauseProjectsStatement :: Statement.Statement [UUID] ()
pauseProjectsStatement = Statement.Statement
  "UPDATE projects SET status = 'paused' WHERE id = ANY($1)"
  (Enc.param (Enc.nonNullable (Enc.foldableArray (Enc.nonNullable Enc.uuid)))) Dec.noResult True

resumeProjectStatement :: Statement.Statement UUID ()
resumeProjectStatement = Statement.Statement "UPDATE projects SET status = 'active' WHERE id = $1"
  (Enc.param (Enc.nonNullable Enc.uuid)) Dec.noResult True

pauseReopenTaskStatement :: Statement.Statement UUID ()
pauseReopenTaskStatement = Statement.Statement "UPDATE tasks SET status = 'todo' WHERE id = $1"
  (Enc.param (Enc.nonNullable Enc.uuid)) Dec.noResult True

freezePauseTaskStatement :: Statement.Statement UUID ()
freezePauseTaskStatement = Statement.Statement "UPDATE tasks SET status = 'done' WHERE id = $1"
  (Enc.param (Enc.nonNullable Enc.uuid)) Dec.noResult True

startFrozenPauseTaskStatement :: Statement.Statement UUID ()
startFrozenPauseTaskStatement = Statement.Statement "UPDATE tasks SET status = 'in_progress' WHERE id = $1"
  (Enc.param (Enc.nonNullable Enc.uuid)) Dec.noResult True

pauseTouchDependencyStatement :: Statement.Statement (UUID, UUID) ()
pauseTouchDependencyStatement = Statement.Statement
  "UPDATE task_dependencies SET task_id = task_id WHERE task_id = $1 AND depends_on_id = $2"
  (contramap fst (Enc.param (Enc.nonNullable Enc.uuid)) <> contramap snd (Enc.param (Enc.nonNullable Enc.uuid))) Dec.noResult True

ownPauseProjectStatement :: Statement.Statement (UUID, Bool) ()
ownPauseProjectStatement = Statement.Statement "UPDATE projects SET own_paused = $2 WHERE id = $1"
  (contramap fst (Enc.param (Enc.nonNullable Enc.uuid)) <> contramap snd (Enc.param (Enc.nonNullable Enc.bool))) Dec.noResult True

setRestoredPauseIntentStatement :: Statement.Statement UUID ()
setRestoredPauseIntentStatement = Statement.Statement
  "SELECT set_config('hmem.project_pause_intent', jsonb_build_object('id', $1::uuid, 'own_paused', false)::text, true)"
  (Enc.param (Enc.nonNullable Enc.uuid)) Dec.noResult True

pauseAuditSnapshotsStatement :: Statement.Statement UUID [Value]
pauseAuditSnapshotsStatement = Statement.Statement
  "SELECT jsonb_build_object('old',old_values,'new',new_values) FROM audit_log WHERE entity_type='project' AND entity_id=$1::uuid::text ORDER BY changed_at,id"
  (Enc.param (Enc.nonNullable Enc.uuid)) (Dec.rowList (Dec.column (Dec.nonNullable Dec.jsonb))) True

pauseCaptureEmptyStatement :: Statement.Statement () Bool
pauseCaptureEmptyStatement = Statement.Statement
  "SELECT coalesce(nullif(current_setting('hmem.project_pause_audit_capture',true),''),'{}')::jsonb='{}'::jsonb AND coalesce(nullif(current_setting('hmem.project_pause_audit_scopes',true),''),'[]')::jsonb='[]'::jsonb"
  Enc.noParams (Dec.singleRow (Dec.column (Dec.nonNullable Dec.bool))) True

