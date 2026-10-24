module HMem.DB.AgentSessionSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (cancel, concurrently, waitCatch, withAsync)
import Control.Exception (SomeException, bracket, bracket_, try, finally)
import Control.Monad (forM_, forM, foldM, replicateM)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as B8
import Data.Pool (destroyAllResources)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.UUID (UUID)
import Data.UUID.V4 qualified as UUID
import Hasql.Decoders qualified as D
import Hasql.Encoders qualified as E
import Hasql.Session qualified as S
import Hasql.Statement qualified as St
import System.Directory (copyFile, createDirectory, listDirectory)
import System.FilePath ((</>))
import Test.Hspec

import HMem.AgentSession
import HMem.BotIdentity (mkBotIdentity)
import HMem.DB.AgentSession
import HMem.DB.Migration qualified as Migration
import HMem.DB.Pool (DBException(..), createPool, runSession, withConn)
import HMem.DB.RequestContext
import HMem.DB.Task qualified as Task
import HMem.DB.Project qualified as Project
import HMem.DB.TestHarness
import HMem.Types qualified as Types

spec :: Spec
spec = do
  around withTestEnv $ describe "Agent sessions" $ do
    it "requires short titles and preserves a same-workspace immutable session tree, including raw SQL guards" $ \env -> do
      (workspace,parent) <- fresh env
      childId <- UUID.nextRandom
      child <- createSession env.pool (CreateSession childId workspace "jsonl" "Repair session filters" (Just parent.id))
      child.title `shouldBe` Just "Repair session filters"
      child.parentSessionId `shouldBe` Just parent.id
      Just viewed <- getSession env.pool workspace child.id
      map (.id) viewed.ancestors `shouldBe` [parent.id]
      rejectDb $ execute env ("UPDATE sessions SET parent_session_id=NULL WHERE id='"<>uuid child.id<>"'")
      rejectDb $ execute env ("UPDATE sessions SET title='Changed title' WHERE id='"<>uuid child.id<>"'")
      another <- UUID.nextRandom
      rejectDb $ createSession env.pool (CreateSession another workspace "jsonl" "Self parent test" (Just another))
      other <- createTestWorkspace env "foreign-session-parent"
      rejectDb $ createSession env.pool (CreateSession another other.id "jsonl" "Foreign parent test" (Just parent.id))
      missing <- UUID.nextRandom
      rejectDb $ createSession env.pool (CreateSession another workspace "jsonl" "Missing parent test" (Just missing))
      forM_ ["NULL","''","E'\\t'","E'\\n'","U&'\\00a0'"] $ \title -> do
        identifier <- UUID.nextRandom
        rejectDb $ execute env ("INSERT INTO sessions(id,workspace_id,format_label,title) VALUES ('"<>uuid identifier<>"','"<>uuid workspace<>"','jsonl',"<>title<>")")
      expectCode SessionInvalidInput $ createSession env.pool (CreateSession another workspace "jsonl" (T.replicate 27 "雪") Nothing)
      rejectDb $ execute env ("INSERT INTO sessions(id,workspace_id,format_label,title,parent_session_id) VALUES ('"<>uuid another<>"','"<>uuid workspace<>"','jsonl','Cycle first','"<>uuid missing<>"'),('"<>uuid missing<>"','"<>uuid workspace<>"','jsonl','Cycle second','"<>uuid another<>"')")
      deep <- foldM (\previous _ -> do identifier <- UUID.nextRandom; createSession env.pool (CreateSession identifier workspace "jsonl" "Nested session work" (Just previous.id))) child [1..20::Int]
      Just context <- getSession env.pool workspace deep.id
      length context.ancestors `shouldBe` 16
      context.ancestorsTruncated `shouldBe` True

    it "only adds new links to in_progress tasks while preserving completed link retries and serializing status races" $ \env -> do
      (workspace,row) <- fresh env
      task <- makeTask env workspace "Linked running task"
      associateSessionTask env.pool workspace row.id task.id
      execute env ("UPDATE tasks SET status='done' WHERE id='"<>uuid task.id<>"'")
      associateSessionTask env.pool workspace row.id task.id
      execute env ("INSERT INTO task_sessions(workspace_id,session_id,task_id) VALUES ('"<>uuid workspace<>"','"<>uuid row.id<>"','"<>uuid task.id<>"')")
      page <- listSessionTasks env.pool workspace row.id Nothing 50
      map (.id) page.items `shouldBe` [task.id]
      forM_ ["todo","blocked","done","cancelled"] $ \status -> do
        target <- Task.createTask env.pool (Types.CreateTask workspace Nothing Nothing "Not running" Nothing Nothing Nothing)
        execute env ("UPDATE tasks SET status='"<>TE.encodeUtf8 status<>"' WHERE id='"<>uuid target.id<>"'")
        expectCode (SessionTaskNotInProgress status) $ associateSessionTask env.pool workspace row.id target.id
        refusal <- try @DBException $ execute env ("INSERT INTO task_sessions(workspace_id,session_id,task_id) VALUES ('"<>uuid workspace<>"','"<>uuid row.id<>"','"<>uuid target.id<>"')")
        case refusal of
          Left (DBCheckViolation message) -> message `shouldSatisfy` T.isInfixOf status
          _ -> expectationFailure "Raw association did not explain the required task status"
      racing <- makeTask env workspace "Racing task status"
      bracket (createPool env.testDb.testDbConnStr 2 30 30000) destroyAllResources $ \peer ->
        withConn env.pool $ \connection -> do
          let sql command = S.run (S.sql command) connection >>= either (fail . show) pure
              probe = env {pool=peer}
              blocked 0 = pure False
              blocked attempts = do
                waiting <- boolean probe "SELECT EXISTS(SELECT 1 FROM pg_stat_activity WHERE datname=current_database() AND pid<>pg_backend_pid() AND wait_event_type='Lock' AND query LIKE 'SELECT t.status::text,EXISTS%')"
                if waiting then pure True else threadDelay 10000 >> blocked (attempts-1)
          sql ("BEGIN; UPDATE tasks SET status='done' WHERE id='"<>uuid racing.id<>"'")
          withAsync (try @SessionException $ associateSessionTask env.pool workspace row.id racing.id) $ \worker ->
            (do
              blocked (500::Int) `shouldReturn` True
              sql "COMMIT"
              result <- waitCatch worker
              case result of
                Right (Left e) -> e.code `shouldBe` SessionTaskNotInProgress "done"
                _ -> expectationFailure "Association did not wait for and observe the committed done status"
              boolean probe ("SELECT NOT EXISTS(SELECT 1 FROM task_sessions WHERE session_id='"<>uuid row.id<>"' AND task_id='"<>uuid racing.id<>"')") `shouldReturn` True
            ) `finally` sql "ROLLBACK"

    it "pages complete active facets and intersects project/task scopes through unloaded and null-project descendants" $ \env -> do
      (workspace,row) <- fresh env
      root <- Project.createProject env.pool (Types.CreateProject workspace Nothing "Root scope" Nothing Nothing Nothing)
      leaf <- Project.createProject env.pool (Types.CreateProject workspace (Just root.id) "Deep scope" Nothing Nothing Nothing)
      other <- Project.createProject env.pool (Types.CreateProject workspace Nothing "Other scope" Nothing Nothing Nothing)
      tasks <- replicateM 51 $ do
        task <- makeTask env workspace "Active facet task"
        execute env ("UPDATE tasks SET project_id='"<>uuid leaf.id<>"' WHERE id='"<>uuid task.id<>"'")
        associateSessionTask env.pool workspace row.id task.id
        pure task
      first <- listSessionFacets env.pool workspace False (Just root.id) Nothing (Just (last tasks).id) 50
      length first.items `shouldBe` 50
      fmap (.id) first.selected `shouldBe` Just (last tasks).id
      second <- listSessionFacets env.pool workspace False (Just root.id) first.nextId Nothing 50
      length second.items `shouldBe` 1
      second.nextId `shouldBe` Nothing
      foreignScope <- makeTask env workspace "Other linked task"
      execute env ("UPDATE tasks SET project_id='"<>uuid other.id<>"' WHERE id='"<>uuid foreignScope.id<>"'")
      associateSessionTask env.pool workspace row.id foreignScope.id
      mismatched <- listWorkspaceSessionsFiltered env.pool workspace (SessionFilter (Just Active) Nothing (Just root.id) (Just foreignScope.id)) Nothing 50
      mismatched.items `shouldBe` []
      child <- Task.createTask env.pool (Types.CreateTask workspace (Just leaf.id) (Just (head tasks).id) "Null project active child" Nothing Nothing Nothing)
      execute env ("UPDATE tasks SET status='in_progress',project_id=NULL WHERE id='"<>uuid child.id<>"'")
      associateSessionTask env.pool workspace row.id child.id
      filtered <- listWorkspaceSessionsFiltered env.pool workspace (SessionFilter (Just Active) Nothing (Just root.id) (Just child.id)) Nothing 50
      map (.id) filtered.items `shouldBe` [row.id]
      let selector = Types.NavigationFilter (Just "tasks") [] [] Nothing Nothing (Just "Null project") (Just True) (Just True)
      visible <- Project.listFilteredProjectChildren env.pool workspace Nothing selector 50 0
      map (.id) visible `shouldBe` [root.id]
      matching <- Task.listFilteredTaskChildren env.pool workspace (Just leaf.id) Nothing selector 50 0
      map (.id) matching `shouldBe` [(head tasks).id]
      _ <- terminateSession env.pool workspace row.id (receipt row "")
      empty <- listSessionFacets env.pool workspace True Nothing Nothing (Just root.id) 50
      empty.items `shouldBe` []
      empty.selected `shouldBe` Nothing

    it "creates an empty active session with server attribution and idempotent immutable metadata" $ \env -> do
      workspace <- createTestWorkspace env "session-create"
      identifier <- UUID.nextRandom
      identity <- either (fail . T.unpack) pure (mkBotIdentity "developer" "codex:model:high")
      let actor = EffectiveActor ActorBot "credential-bot" "Developer" (Just identity)
          input = CreateSession identifier workspace.id "provider-neutral-jsonl" "Test session work" Nothing
      row <- withEffectiveActorContext (Just actor) $ createSession env.pool input
      row.creatorActorId `shouldBe` Just "credential-bot"
      row.creatorBotIdentity `shouldBe` Just identity
      row.state `shouldBe` Active
      row.committedBytes `shouldBe` 0
      row.committedRecords `shouldBe` 0
      row.revision `shouldBe` 0
      withEffectiveActorContext (Just actor) (createSession env.pool input) `shouldReturn` row
      expectCode SessionConflict $ createSession env.pool input
      expectCode SessionConflict $ withEffectiveActorContext (Just actor) $ createSession env.pool input {formatLabel="other"}
      other <- createTestWorkspace env "session-create-other"
      expectCode SessionConflict $ createSession env.pool input {workspaceId=other.id}
      getSession env.pool other.id identifier `shouldReturn` Nothing

    it "preserves UTF-8, CRLF, arbitrary JSON values, escaped NUL and large JSON numbers byte for byte" $ \env -> do
      (workspace,row) <- fresh env
      let body = TE.encodeUtf8 "{\"message\":\"æ雪\"}\r\n[true,null,1]\n\"\\u0000\"\n1e999999\n"
      result <- append env workspace row body False
      result.session.committedBytes `shouldBe` fromIntegral (BS.length body)
      result.session.committedRecords `shouldBe` 4
      ranges <- mapM (\offset -> readSessionRange env.pool workspace row.id offset 3) [0,3..fromIntegral (BS.length body)-1]
      BS.concat (map (.bytes) ranges) `shouldBe` body
      let current = result.session
      sealed <- terminateSession env.pool workspace row.id (receipt current body)
      sealed.state `shouldBe` Terminated
      sealed.finalSha256 `shouldBe` Just (sessionSha256 body)
      downloaded <- readSessionRange env.pool workspace row.id 0 maxSessionRangeBytes
      downloaded.bytes `shouldBe` body
      downloaded.hasMore `shouldBe` False

    it "streams a multichunk transcript hash and reads bounded windows across chunk boundaries" $ \env -> do
      (workspace,row) <- fresh env
      let chunk = "\"" <> BS.replicate (maxSessionChunkBytes-3) 97 <> "\"\n"
          body = chunk <> chunk <> "true\n"
      first <- append env workspace row chunk False
      second <- append env workspace first.session chunk False
      third <- append env workspace second.session "true\n" False
      window <- readSessionRange env.pool workspace row.id (fromIntegral maxSessionChunkBytes-2) 5
      window.bytes `shouldBe` BS.take 5 (BS.drop (maxSessionChunkBytes-2) body)
      window.hasMore `shouldBe` True
      window.nextOffset `shouldBe` fromIntegral maxSessionChunkBytes+3
      sealed <- terminateSession env.pool workspace row.id (receipt third.session body)
      sealed.finalSha256 `shouldBe` Just (sessionSha256 body)
      lastWindow <- readSessionRange env.pool workspace row.id (fromIntegral (2*maxSessionChunkBytes)) maxSessionRangeBytes
      lastWindow.bytes `shouldBe` "true\n"
      lastWindow.hasMore `shouldBe` False

    it "keeps a flushed newline-free record active and requires a real separator before any later bytes" $ \env -> do
      (workspace,row) <- fresh env
      first <- append env workspace row "{\"a\":1}" True
      first.session.state `shouldBe` Active
      first.session.committedRecords `shouldBe` 1
      forM_ [" ","true\n","\rtrue\n","\t\n"] $ \bytes -> do
        rejectDb $ append env workspace first.session bytes True
        getSession env.pool workspace row.id `shouldReturn` Just first.session
      separator <- append env workspace first.session "\r\n" False
      separator.session.committedRecords `shouldBe` 1
      second <- append env workspace separator.session "true\n" False
      second.session.committedRecords `shouldBe` 2
      allBytes env workspace row.id `shouldReturn` "{\"a\":1}\r\ntrue\n"

    it "also accepts a separator and new complete record in one chunk" $ \env -> do
      (workspace,row) <- fresh env
      first <- append env workspace row "1" True
      second <- append env workspace first.session "\n2\r\n3" True
      second.session.committedRecords `shouldBe` 3
      second.session.state `shouldBe` Active
      allBytes env workspace row.id `shouldReturn` "1\n2\r\n3"

    it "rejects invalid UTF-8, empty/whitespace records, malformed JSON and pending tails atomically" $ \env -> do
      (workspace,row) <- fresh env
      forM_ [("{}",False),("{\n",True),("{}\n{",True),("\n",True),(" \r\n",False)
            ,("{}\n\n",False),(BS.pack [255,10],False),("true false\n",False),("{}\n  ",True)] $ \(body,flush) -> do
        rejectDb $ append env workspace row body flush
        getSession env.pool workspace row.id `shouldReturn` Just row
        allBytes env workspace row.id `shouldReturn` ""
      expectCode SessionInvalidInput $ append env workspace row "" False
      expectCode SessionInvalidInput $ append env workspace row (BS.replicate (maxSessionChunkBytes+1) 32) False
      let large = "\"" <> BS.replicate (maxSessionChunkBytes-3) 97 <> "\"\n"
      accepted <- append env workspace row large False
      accepted.session.committedRecords `shouldBe` 1

    it "acknowledges exact replay at old offsets, rejects gaps/overlap, and never mutates on replay" $ \env -> do
      (workspace,row) <- fresh env
      first <- append env workspace row "1\n" False
      second <- append env workspace first.session "2\n" False
      replay <- append env workspace row "1\n" False
      replay.replayed `shouldBe` True
      replay.session `shouldBe` second.session
      expectCode SessionConflict $ append env workspace row "3\n" False
      expectCode SessionConflict $ appendSession env.pool workspace row.id (AppendSession 100 2 "3\n" False)
      expectCode SessionConflict $ appendSession env.pool workspace row.id (AppendSession 1 0 "3\n" False)
      expectCode SessionConflict $ append env workspace row "1\n" True
      sealed <- terminateSession env.pool workspace row.id (receipt second.session "1\n2\n")
      replaySealed <- append env workspace row "1\n" False
      replaySealed `shouldBe` SessionAppendResult sealed True

    it "seals only the committed prefix, validates the receipt, and retries the exact seal" $ \env -> do
      (workspace,row) <- fresh env
      uploaded <- append env workspace row "{}" True
      let current = uploaded.session
          exact = receipt current "{}"
      expectCode SessionConflict $ terminateSession env.pool workspace row.id exact {expectedOffset=0}
      expectCode SessionConflict $ terminateSession env.pool workspace row.id exact {expectedRevision=0}
      expectCode SessionConflict $ terminateSession env.pool workspace row.id exact {finalBytes=99}
      expectCode SessionConflict $ terminateSession env.pool workspace row.id exact {finalSha256=T.replicate 64 "0"}
      getSession env.pool workspace row.id `shouldReturn` Just current
      sealed <- terminateSession env.pool workspace row.id exact
      sealed.committedBytes `shouldBe` current.committedBytes
      sealed.revision `shouldBe` current.revision+1
      terminateSession env.pool workspace row.id exact `shouldReturn` sealed
      expectCode SessionConflict $ terminateSession env.pool workspace row.id exact {expectedRevision=sealed.revision}
      expectCode SessionTerminated $ heartbeatSession env.pool workspace row.id
      expectCode SessionTerminated $ append env workspace sealed "\ntrue\n" False

    it "can seal an empty session with no uploader and leaves task status unchanged" $ \env -> do
      (workspace,row) <- fresh env
      task <- makeTask env workspace "still todo"
      associateSessionTask env.pool workspace row.id task.id
      seen <- heartbeatSession env.pool workspace row.id
      seen.state `shouldBe` Active
      seen.revision `shouldBe` 0
      sealed <- terminateSession env.pool workspace row.id (receipt seen "")
      sealed.finalBytes `shouldBe` Just 0
      Just after <- Task.getTask env.pool task.id
      after.status `shouldBe` Types.InProgress

    it "seals a stale active prefix without requiring a heartbeat or uploader" $ \env -> do
      workspace <- createTestWorkspace env "stale-session"
      identifier <- UUID.nextRandom
      execute env ("INSERT INTO sessions(id,workspace_id,format_label,title,created_at,last_seen_at) VALUES ('"
        <> uuid identifier <> "','" <> uuid workspace.id <> "','jsonl','Test session work',clock_timestamp()-interval '1 hour',clock_timestamp()-interval '1 hour')")
      Just stale <- getSession env.pool workspace.id identifier
      stale.state `shouldBe` Active
      sealed <- terminateSession env.pool workspace.id identifier (receipt stale "")
      sealed.lastSeenAt `shouldBe` stale.lastSeenAt
      sealed.state `shouldBe` Terminated

    it "does not store raw bytes in audit or change events" $ \env -> do
      (workspace,row) <- fresh env
      _ <- append env workspace row "{\"secret\":\"session-only-needle\"}\n" False
      boolean env "SELECT NOT EXISTS (SELECT 1 FROM audit_log WHERE old_values::text LIKE '%session-only-needle%' OR new_values::text LIKE '%session-only-needle%') AND NOT EXISTS (SELECT 1 FROM change_stream_outbox WHERE envelope::text LIKE '%session-only-needle%')" `shouldReturn` True

    it "guards raw SQL chunk/order/counters, active identity, sealed metadata and additive task links" $ \env -> do
      (workspace,row) <- fresh env
      task <- makeTask env workspace "guarded"
      associateSessionTask env.pool workspace row.id task.id
      forM_ ["UPDATE sessions SET format_label='changed'","UPDATE sessions SET committed_bytes=5"
            ,"INSERT INTO session_chunks(session_id,start_offset,revision,data,allow_final_record,record_count,ends_with_newline) SELECT id,1,1,convert_to('1','UTF8'),true,1,false FROM sessions"
            ,"INSERT INTO session_chunks(session_id,start_offset,revision,data,allow_final_record,record_count,ends_with_newline) SELECT id,0,1,convert_to('{','UTF8'),true,1,false FROM sessions"
            ,"DELETE FROM task_sessions","UPDATE task_sessions SET created_at=clock_timestamp()"] $ \sql -> rejectDb (execute env sql)
      first <- append env workspace row "1\n" False
      forM_ ["UPDATE session_chunks SET data=convert_to('2','UTF8')","DELETE FROM session_chunks"] $ \sql -> rejectDb (execute env sql)
      sealed <- terminateSession env.pool workspace row.id (receipt first.session "1\n")
      forM_ ["UPDATE sessions SET state='active',terminated_at=NULL,final_bytes=NULL,final_sha256=NULL"
            ,"UPDATE sessions SET last_seen_at=clock_timestamp()","UPDATE sessions SET final_sha256=repeat('0',64)"
            ,"UPDATE sessions SET revision=revision+1","UPDATE sessions SET format_label='changed'","DELETE FROM sessions"
            ,"INSERT INTO session_chunks(session_id,start_offset,revision,data,allow_final_record,record_count,ends_with_newline) SELECT id,committed_bytes,revision+1,convert_to('2','UTF8'),true,1,false FROM sessions"] $ \sql -> rejectDb (execute env sql)
      getSession env.pool workspace row.id `shouldReturn` Just sealed
      allBytes env workspace row.id `shouldReturn` "1\n"
      forM_ ["TRUNCATE session_chunks","TRUNCATE task_sessions","TRUNCATE sessions CASCADE"
            ,"TRUNCATE tasks CASCADE","TRUNCATE workspaces CASCADE"] $ \sql -> do
        rejectDb (execute env sql)
        getSession env.pool workspace row.id `shouldReturn` Just sealed
        allBytes env workspace row.id `shouldReturn` "1\n"

    it "restores exactly the evidence truncate guards after successful and failed test cleanup" $ \env -> do
      (workspace,row) <- fresh env
      task <- makeTask env workspace "cleanup-retained"
      associateSessionTask env.pool workspace row.id task.id
      first <- append env workspace row "1\n" False
      sealed <- terminateSession env.pool workspace row.id (receipt first.session "1\n")
      bracket_
        (execute env "CREATE FUNCTION session_cleanup_failure() RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN RAISE EXCEPTION 'injected cleanup failure'; END; $$; CREATE TRIGGER zz_session_cleanup_failure BEFORE TRUNCATE ON workspaces EXECUTE FUNCTION session_cleanup_failure()")
        (execute env "DROP TRIGGER zz_session_cleanup_failure ON workspaces; DROP FUNCTION session_cleanup_failure()") $ do
          result <- try @SomeException (cleanDB env)
          result `shouldSatisfy` either (const True) (const False)
          getSession env.pool workspace row.id `shouldReturn` Just sealed
          allBytes env workspace row.id `shouldReturn` "1\n"
          assertTruncateGuards env
      cleanDB env
      getSession env.pool workspace row.id `shouldReturn` Nothing
      assertTruncateGuards env

    it "rolls back suspended truncate guards and sealed evidence when test cleanup is cancelled" $ \env -> do
      (workspace,row) <- fresh env
      first <- append env workspace row "1\n" False
      sealed <- terminateSession env.pool workspace row.id (receipt first.session "1\n")
      bracket_
        (execute env "CREATE FUNCTION session_cleanup_wait() RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN PERFORM pg_sleep(5); RETURN NULL; END; $$; CREATE TRIGGER zz_session_cleanup_wait BEFORE TRUNCATE ON workspaces EXECUTE FUNCTION session_cleanup_wait()")
        (execute env "DROP TRIGGER zz_session_cleanup_wait ON workspaces; DROP FUNCTION session_cleanup_wait()") $
          withAsync (cleanDB env) $ \cleanup -> do
            awaitCleanupSleep env 200
            cancel cleanup
            waitCatch cleanup >>= (`shouldSatisfy` either (const True) (const False))
            getSession env.pool workspace row.id `shouldReturn` Just sealed
            allBytes env workspace row.id `shouldReturn` "1\n"
            assertTruncateGuards env

    it "keeps an enclosing fixture transaction open while cleaning under a savepoint" $ \env -> do
      (workspace,row) <- fresh env
      sealed <- terminateSession env.pool workspace row.id (receipt row "")
      bracket (createPool env.testDb.testDbConnStr 1 30 30000) destroyAllResources $ \pool ->
        withTestTransaction (\transactionEnv -> do
          cleanDB transactionEnv
          getSession pool workspace row.id `shouldReturn` Nothing
          boolean transactionEnv "SELECT count(*)=3 AND bool_and(tgenabled='O') FROM pg_trigger WHERE tgname='session_evidence_no_truncate' AND tgrelid IN ('public.sessions'::regclass,'public.session_chunks'::regclass,'public.task_sessions'::regclass)" `shouldReturn` True) env {pool=pool}
      -- The enclosing fixture rollback must restore the earlier sealed row.
      getSession env.pool workspace row.id `shouldReturn` Just sealed
      assertTruncateGuards env

    it "links idempotently before/after sealing, rejects other workspaces, hides soft-deleted tasks and restores links" $ \env -> do
      (workspace,row) <- fresh env
      task <- makeTask env workspace "linked"
      otherWorkspace <- createTestWorkspace env "other"
      other <- makeTask env otherWorkspace.id "other"
      associateSessionTask env.pool workspace row.id task.id
      associateSessionTask env.pool workspace row.id task.id
      expectCode SessionNotFound $ associateSessionTask env.pool workspace row.id other.id
      rejectDb $ execute env ("INSERT INTO task_sessions(workspace_id,task_id,session_id) VALUES ('" <> uuid otherWorkspace.id <> "','" <> uuid other.id <> "','" <> uuid row.id <> "')")
      linked <- listSessionTasks env.pool workspace row.id Nothing 100
      map (.id) linked.items `shouldBe` [task.id]
      _ <- terminateSession env.pool workspace row.id (receipt row "")
      later <- makeTask env workspace "later"
      associateSessionTask env.pool workspace row.id later.id
      execute env ("UPDATE tasks SET deleted_at=clock_timestamp() WHERE id='" <> uuid task.id <> "'")
      hidden <- listTaskSessions env.pool workspace task.id Nothing 100
      hidden.items `shouldBe` []
      visible <- listSessionTasks env.pool workspace row.id Nothing 100
      map (.id) visible.items `shouldBe` [later.id]
      execute env ("UPDATE tasks SET deleted_at=NULL WHERE id='" <> uuid task.id <> "'")
      restored <- listTaskSessions env.pool workspace task.id Nothing 100
      map (.id) restored.items `shouldBe` [row.id]

    it "uses bounded keyset lists and strict range limits with workspace isolation" $ \env -> do
      (workspace,row) <- fresh env
      _ <- freshIn env workspace
      _ <- freshIn env workspace
      first <- listWorkspaceSessions env.pool workspace Nothing 1
      length first.items `shouldBe` 1
      second <- listWorkspaceSessions env.pool workspace first.nextCursor 1
      length second.items `shouldBe` 1
      map (.id) second.items `shouldNotBe` map (.id) first.items
      _ <- freshIn env workspace
      third <- listWorkspaceSessions env.pool workspace second.nextCursor 1
      map (.id) third.items `shouldBe` [row.id]
      third.nextCursor `shouldBe` Nothing
      forM_ [0,101] $ \limit -> expectCode SessionInvalidInput $ listWorkspaceSessions env.pool workspace Nothing limit
      expectCode SessionInvalidInput $ readSessionRange env.pool workspace row.id 0 (maxSessionRangeBytes+1)
      expectCode SessionConflict $ readSessionRange env.pool workspace row.id 1 1
      other <- createTestWorkspace env "range-isolation"
      expectCode SessionNotFound $ readSessionRange env.pool other.id row.id 0 1
      listWorkspaceSessions env.pool other.id Nothing 1 `shouldReturn` SessionPage [] Nothing

    it "filters lifecycle and heartbeat freshness before keyset pagination independently" $ \env -> do
      (workspace,current) <- fresh env
      old <- UUID.nextRandom
      older <- UUID.nextRandom
      execute env ("INSERT INTO sessions(id,workspace_id,format_label,title,created_at,last_seen_at) VALUES ('" <> uuid old <> "','" <> uuid workspace <> "','jsonl','Test session work',clock_timestamp()-interval '60 seconds',clock_timestamp()-interval '60 seconds'),('" <> uuid older <> "','" <> uuid workspace <> "','jsonl','Test session work',clock_timestamp()-interval '90 seconds',clock_timestamp()-interval '90 seconds')")
      oldRow <- getSession env.pool workspace old >>= maybe (fail "Missing seeded session") pure
      sealed <- terminateSession env.pool workspace old (receipt oldRow "")
      let stale = SessionFilter Nothing (Just Stale) Nothing Nothing
          freshOnly = SessionFilter Nothing (Just Fresh) Nothing Nothing
      first <- listWorkspaceSessionsFiltered env.pool workspace stale Nothing 1
      map (.id) first.items `shouldBe` [old]
      second <- listWorkspaceSessionsFiltered env.pool workspace stale first.nextCursor 1
      map (.id) second.items `shouldBe` [older]
      second.nextCursor `shouldBe` Nothing
      recent <- listWorkspaceSessionsFiltered env.pool workspace freshOnly Nothing 50
      map (.id) recent.items `shouldBe` [current.id]
      closed <- listWorkspaceSessionsFiltered env.pool workspace (SessionFilter (Just Terminated) (Just Stale) Nothing Nothing) Nothing 50
      closed.items `shouldBe` [sealed]
      active <- listWorkspaceSessionsFiltered env.pool workspace (SessionFilter (Just Active) (Just Stale) Nothing Nothing) Nothing 50
      map (.id) active.items `shouldBe` [older]
      task <- makeTask env workspace "filtered evidence"
      forM_ [current.id,old,older] $ \id -> associateSessionTask env.pool workspace id task.id
      linked <- listTaskSessionsFiltered env.pool workspace task.id (SessionFilter (Just Terminated) Nothing Nothing Nothing) Nothing 1
      linked.items `shouldBe` [sealed]

    it "serializes concurrent appends and append/termination races without losing a committed prefix" $ \env -> do
      (workspace,row) <- fresh env
      (one,two) <- concurrently
        (try @SessionException $ append env workspace row "1\n" False)
        (try @SessionException $ append env workspace row "2\n" False)
      length [() | Right _ <- [one,two]] `shouldBe` 1
      length [() | Left e <- [one,two], e.code == SessionConflict] `shouldBe` 1
      Just current <- getSession env.pool workspace row.id
      body <- allBytes env workspace row.id
      (appended,sealed) <- concurrently
        (try @SessionException $ append env workspace current "3\n" False)
        (try @SessionException $ terminateSession env.pool workspace row.id (receipt current body))
      case (appended,sealed) of
        (Right _,Left e) -> e.code `shouldBe` SessionConflict
        (Left e,Right _) -> e.code `shouldBe` SessionTerminated
        _ -> expectationFailure "Exactly one racing mutation must succeed"
      Just after <- getSession env.pool workspace row.id
      after.committedBytes `shouldBe` if after.state == Terminated then current.committedBytes else current.committedBytes+2

    it "rolls back a late chunk failure including all parent counters" $ \env -> do
      (workspace,row) <- fresh env
      bracket_
        (execute env "CREATE FUNCTION session_test_failure() RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN RAISE EXCEPTION USING ERRCODE='23514', MESSAGE='injected late failure'; END; $$; CREATE TRIGGER zz_session_test_failure AFTER INSERT ON session_chunks FOR EACH ROW EXECUTE FUNCTION session_test_failure()")
        (execute env "DROP TRIGGER zz_session_test_failure ON session_chunks; DROP FUNCTION session_test_failure()") $ do
          rejectDb $ append env workspace row "1\n" False
          getSession env.pool workspace row.id `shouldReturn` Just row
          allBytes env workspace row.id `shouldReturn` ""

  describe "Agent sessions migration" $ do
    it "upgrades populated V042 evidence atomically, keeps legacy metadata unchanged and resumes an untitled active session" $
      withTestSandbox $ \sandbox -> withSandboxedEnv sandbox $ withSandboxedPostgres sandbox $ \db ->
        bracket (createPool db.testDbConnStr 2 30 30000) destroyAllResources $ \pool -> do
          source <- resolveMigrationsDir sandbox.sandboxRepoRoot
          let beforeDir=sandbox.sandboxRoot </> "before-v043"
              upgradeDir=sandbox.sandboxRoot </> "only-v043"
              filename="V043__session_hierarchy_and_titles.sql"
              env=TestEnv pool sandbox db
              body="{\"text\":\"legacy evidence\"}\r\n"
          createDirectory beforeDir
          createDirectory upgradeDir
          files <- listDirectory source
          forM_ (filter (\name -> name < "V043" && ".sql" `T.isSuffixOf` T.pack name) files) $ \name -> copyFile (source </> name) (beforeDir </> name)
          copyFile (source </> filename) (upgradeDir </> filename)
          before <- Migration.runMigrations pool beforeDir
          before.failed `shouldBe` Nothing
          workspace <- createTestWorkspace env "legacy-session-upgrade"
          activeId <- UUID.nextRandom
          sealedId <- UUID.nextRandom
          forM_ [activeId,sealedId] $ \identifier -> do
            execute env ("INSERT INTO sessions(id,workspace_id,format_label) VALUES ('"<>uuid identifier<>"','"<>uuid workspace.id<>"','legacy-jsonl')")
            execute env ("INSERT INTO session_chunks(session_id,start_offset,revision,data,allow_final_record,record_count,ends_with_newline) VALUES ('"<>uuid identifier<>"',0,1,convert_to(E'{\"text\":\"legacy evidence\"}\\r\\n','UTF8'),false,0,false)")
          execute env ("UPDATE sessions SET state='terminated',terminated_at=clock_timestamp(),final_bytes=committed_bytes,final_sha256='"<>TE.encodeUtf8 (sessionSha256 body)<>"',revision=revision+1 WHERE id='"<>uuid sealedId<>"'")
          execute env "CREATE TABLE legacy_session_expected AS SELECT id,state,created_at,last_seen_at,committed_bytes,committed_records,revision,terminated_at,final_bytes,final_sha256 FROM sessions; CREATE TABLE legacy_audit_expected AS SELECT count(*) AS n FROM audit_log"
          bracket_
            (execute env "CREATE FUNCTION title_ledger_failure() RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN IF NEW.version=43 THEN RAISE EXCEPTION 'injected title ledger failure'; END IF; RETURN NEW; END; $$; CREATE TRIGGER title_ledger_failure BEFORE INSERT ON schema_migrations FOR EACH ROW EXECUTE FUNCTION title_ledger_failure()")
            (execute env "DROP TRIGGER title_ledger_failure ON schema_migrations; DROP FUNCTION title_ledger_failure()") $ do
              refused <- Migration.runMigrations pool upgradeDir
              refused.failed `shouldSatisfy` (/=Nothing)
              boolean env "SELECT NOT EXISTS(SELECT 1 FROM information_schema.columns WHERE table_schema='public' AND table_name='sessions' AND column_name='title') AND NOT EXISTS(SELECT 1 FROM schema_migrations WHERE version=43)" `shouldReturn` True
          upgraded <- Migration.runMigrations pool upgradeDir
          upgraded.failed `shouldBe` Nothing
          upgraded.applied `shouldBe` [filename]
          boolean env "SELECT NOT EXISTS(SELECT 1 FROM sessions s JOIN legacy_session_expected e USING(id) WHERE (s.state,s.created_at,s.last_seen_at,s.committed_bytes,s.committed_records,s.revision,s.terminated_at,s.final_bytes,s.final_sha256) IS DISTINCT FROM (e.state,e.created_at,e.last_seen_at,e.committed_bytes,e.committed_records,e.revision,e.terminated_at,e.final_bytes,e.final_sha256) OR s.title IS NOT NULL OR s.parent_session_id IS NOT NULL) AND (SELECT count(*) FROM audit_log)=(SELECT n FROM legacy_audit_expected)" `shouldReturn` True
          Just legacy <- getSession pool workspace.id activeId
          resumed <- createSession pool (CreateSession activeId workspace.id "legacy-jsonl" "Resume legacy session work" Nothing)
          resumed `shouldBe` legacy
          resumed.title `shouldBe` Nothing
          sealed <- readSessionRange pool workspace.id sealedId 0 maxSessionRangeBytes
          sealed.bytes `shouldBe` body
          sealed.session.finalSha256 `shouldBe` Just (sessionSha256 body)
          boolean env "SELECT (SELECT count(*) FROM audit_log)=(SELECT n FROM legacy_audit_expected)" `shouldReturn` True
          repeated <- Migration.runMigrations pool upgradeDir
          repeated.applied `shouldBe` []

    it "upgrades populated V040, atomically rolls back a late DDL failure, and reruns idempotently" $
      withTestSandbox $ \sandbox -> withSandboxedEnv sandbox $ withSandboxedPostgres sandbox $ \db ->
        bracket (createPool db.testDbConnStr 2 30 30000) destroyAllResources $ \pool -> do
          source <- resolveMigrationsDir sandbox.sandboxRepoRoot
          let beforeDir = sandbox.sandboxRoot </> "before-v041"
              upgradeDir = sandbox.sandboxRoot </> "only-v041"
              filename = "V041__agent_sessions.sql"
          createDirectory beforeDir
          createDirectory upgradeDir
          files <- listDirectory source
          forM_ (filter (\name -> name < "V041" && ".sql" `T.isSuffixOf` T.pack name) files) $ \name -> copyFile (source </> name) (beforeDir </> name)
          copyFile (source </> filename) (upgradeDir </> filename)
          before <- Migration.runMigrations pool beforeDir
          before.failed `shouldBe` Nothing
          let env = TestEnv pool sandbox db
          workspace <- createTestWorkspace env "session-upgrade"
          task <- makeTask env workspace.id "survives"
          execute env "CREATE TABLE task_sessions(conflict boolean)"
          failed <- Migration.runMigrations pool upgradeDir
          failed.failed `shouldSatisfy` (/=Nothing)
          boolean env "SELECT to_regclass('sessions') IS NULL AND to_regclass('session_chunks') IS NULL AND NOT EXISTS(SELECT 1 FROM schema_migrations WHERE version=41)" `shouldReturn` True
          execute env "DROP TABLE task_sessions"
          upgraded <- Migration.runMigrations pool upgradeDir
          upgraded.failed `shouldBe` Nothing
          upgraded.applied `shouldBe` [filename]
          Just after <- Task.getTask pool task.id
          after `shouldBe` task
          repeated <- Migration.runMigrations pool upgradeDir
          repeated.failed `shouldBe` Nothing
          repeated.applied `shouldBe` []
          _ <- Migration.runMigrations pool source
          row <- freshIn env workspace.id
          associateSessionTask pool workspace.id row.id task.id

fresh :: TestEnv -> IO (UUID,AgentSession)
fresh env = do
  workspace <- createTestWorkspace env "session-test"
  row <- freshIn env workspace.id
  pure (workspace.id,row)

freshIn :: TestEnv -> UUID -> IO AgentSession
freshIn env workspace = do
  identifier <- UUID.nextRandom
  createSession env.pool (CreateSession identifier workspace "jsonl" "Test session work" Nothing)

append :: TestEnv -> UUID -> AgentSession -> ByteString -> Bool -> IO SessionAppendResult
append env workspace row bytes flush = appendSession env.pool workspace row.id (AppendSession row.committedBytes row.revision bytes flush)

receipt :: AgentSession -> ByteString -> TerminateSession
receipt row body = TerminateSession row.committedBytes row.revision (fromIntegral (BS.length body)) (sessionSha256 body)

allBytes :: TestEnv -> UUID -> UUID -> IO ByteString
allBytes env workspace identifier = (.bytes) <$> readSessionRange env.pool workspace identifier 0 maxSessionRangeBytes

makeTask :: TestEnv -> UUID -> T.Text -> IO Types.Task
makeTask env workspace title = do
  task <- Task.createTask env.pool (Types.CreateTask workspace Nothing Nothing title Nothing Nothing Nothing)
  execute env ("UPDATE tasks SET status='in_progress' WHERE id='" <> uuid task.id <> "'")
  Task.getTask env.pool task.id >>= maybe (fail "Missing test task") pure

expectCode :: SessionErrorCode -> IO a -> IO ()
expectCode code action = do
  result <- try @SessionException action
  case result of
    Left e -> e.code `shouldBe` code
    Right _ -> expectationFailure ("Expected " <> show code)

rejectDb :: IO a -> IO ()
rejectDb action = do
  result <- try @DBException action
  case result of
    Left (DBCheckViolation _) -> pure ()
    Left (DBForeignKeyViolation _) -> pure ()
    other -> expectationFailure $ "Expected guarded SQL rejection, got " <> either show (const "success") other

execute :: TestEnv -> ByteString -> IO ()
execute env sql = runSession env.pool (S.sql sql)

boolean :: TestEnv -> ByteString -> IO Bool
boolean env sql = runSession env.pool $ S.statement () $ St.Statement sql E.noParams (D.singleRow (D.column (D.nonNullable D.bool))) False

uuid :: UUID -> ByteString
uuid = B8.pack . show

assertTruncateGuards :: TestEnv -> IO ()
assertTruncateGuards env = do
  boolean env "SELECT count(*)=3 AND bool_and(tgenabled='O') FROM pg_trigger WHERE tgname='session_evidence_no_truncate' AND tgrelid IN ('public.sessions'::regclass,'public.session_chunks'::regclass,'public.task_sessions'::regclass)" `shouldReturn` True
  forM_ ["TRUNCATE session_chunks","TRUNCATE task_sessions","TRUNCATE sessions CASCADE"] $ \sql -> rejectDb (execute env sql)

awaitCleanupSleep :: TestEnv -> Int -> IO ()
awaitCleanupSleep env attempts = do
  sleeping <- boolean env "SELECT EXISTS(SELECT 1 FROM pg_stat_activity WHERE datname=current_database() AND query LIKE 'TRUNCATE change_stream_snapshot_page_tokens%' AND wait_event='PgSleep')"
  if sleeping then pure ()
  else if attempts <= 0 then expectationFailure "Cleanup did not reach the synchronized trigger wait"
  else threadDelay 10000 >> awaitCleanupSleep env (attempts-1)
