module HMem.Server.SessionReaperSpec (spec) where

import Control.Concurrent (Chan, newChan, readChan, writeChan, threadDelay)
import Control.Exception (bracket, bracket_)
import Control.Monad (forM_, replicateM)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as B8
import Data.UUID (UUID)
import Data.UUID.V4 qualified as UUID
import Hasql.Decoders qualified as D
import Hasql.Encoders qualified as E
import Hasql.Session qualified as S
import Hasql.Statement qualified as St
import System.Timeout (timeout)
import Test.Hspec

import HMem.AgentSession (AgentSession(..), SessionState(..), sessionSha256)
import HMem.DB.AgentSession (getSession, workspaceSessionActivity, projectSessionActivity)
import HMem.DB.Pool (runSession, runTransaction)
import HMem.DB.Project qualified as Project
import HMem.DB.Task qualified as Task
import HMem.DB.TestHarness
import HMem.Server.SessionReaper
import HMem.Types qualified as Types

spec :: Spec
spec = around withTestEnv $ describe "abandoned session worker" $ do
  it "reconciles at startup and periodically, emits canonical termination and clears the last workspace/project light" $ \env -> do
    workspace <- createTestWorkspace env "worker-session-policy"
    project <- Project.createProject env.pool (Types.CreateProject workspace.id Nothing "Session project" Nothing Nothing Nothing)
    first <- oldSession env workspace.id (Just project.id) True
    workspaceSessionActivity env.pool [workspace.id] `shouldReturn` [(workspace.id,True)]
    projectSessionActivity env.pool workspace.id [project.id] `shouldReturn` [(project.id,True)]
    batches <- newChan
    bracket (startSessionReaperWithDelay 10000 (writeChan batches ()) env.pool) stopSessionReaper $ \_ -> do
      timeout 5000000 (readChan batches) `shouldReturn` Just ()
      Just sealed <- getSession env.pool workspace.id first
      sealed.state `shouldBe` Terminated
      sealed.finalSha256 `shouldBe` Just (sessionSha256 "")
      workspaceSessionActivity env.pool [workspace.id] `shouldReturn` [(workspace.id,False)]
      projectSessionActivity env.pool workspace.id [project.id] `shouldReturn` [(project.id,False)]
      queryBool env ("SELECT EXISTS(SELECT 1 FROM change_stream_outbox WHERE envelope#>>'{entity,type}'='agent_session' AND envelope#>>'{entity,id}'='"<>uuid first<>"' AND envelope#>>'{entity,action}'='updated' AND (envelope->'invalidations')::text LIKE '%active_sessions:%')") `shouldReturn` True
      queryBool env ("SELECT EXISTS(SELECT 1 FROM audit_log WHERE entity_type='agent_session' AND entity_id='"<>uuid first<>"' AND action='update' AND actor_id IS NULL AND new_values->>'state'='terminated')") `shouldReturn` True
      second <- oldSession env workspace.id (Just project.id) True
      -- The cursor may need one empty page to wrap before discovering a row
      -- whose activity sorts before the previous cursor.
      awaitSealed env batches workspace.id second 8
    afterStop <- oldSession env workspace.id Nothing True
    threadDelay 50000
    Just row <- getSession env.pool workspace.id afterStop
    row.state `shouldBe` Active

  it "advances its bounded keyset beyond unfinished and failed candidates without starvation" $ \env -> do
    workspace <- createTestWorkspace env "worker-keyset-fairness"
    blocked <- replicateM 31 (oldSession env workspace.id Nothing False)
    failed <- oldSession env workspace.id Nothing True
    eligible <- oldSession env workspace.id Nothing True
    bracket_
      (runSession env.pool $ S.sql ("CREATE FUNCTION reaper_injected_failure() RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN IF NEW.id='"<>uuid failed<>"' AND NEW.state='terminated' THEN RAISE EXCEPTION 'injected seal failure'; END IF; RETURN NEW; END; $$; CREATE TRIGGER reaper_injected_failure BEFORE UPDATE ON sessions FOR EACH ROW EXECUTE FUNCTION reaper_injected_failure()"))
      (runSession env.pool $ S.sql "DROP TRIGGER reaper_injected_failure ON sessions; DROP FUNCTION reaper_injected_failure()") $ do
        first <- reapSessionBatch env.pool Nothing
        first `shouldSatisfy` (/=Nothing)
        Just waiting <- getSession env.pool workspace.id eligible
        waiting.state `shouldBe` Active
        second <- reapSessionBatch env.pool first
        Just sealed <- getSession env.pool workspace.id eligible
        sealed.state `shouldBe` Terminated
        forM_ (failed:blocked) $ \identifier -> do
          Just row <- getSession env.pool workspace.id identifier
          row.state `shouldBe` Active
        reapSessionBatch env.pool second `shouldReturn` Nothing

oldSession :: TestEnv -> UUID -> Maybe UUID -> Bool -> IO UUID
oldSession env workspace project terminal = do
  task <- Task.createTask env.pool (Types.CreateTask workspace project Nothing "Idle session task" Nothing Nothing Nothing)
  runSession env.pool $ S.sql ("UPDATE tasks SET status='in_progress' WHERE id='"<>uuid task.id<>"'")
  identifier <- UUID.nextRandom
  runTransaction env.pool $ S.sql
    ("INSERT INTO sessions(id,workspace_id,format_label,title,created_at,last_seen_at) VALUES ('"<>uuid identifier<>"','"<>uuid workspace<>"','jsonl','Abandoned task capture',clock_timestamp()-interval '2 hours',clock_timestamp()-interval '2 hours'); INSERT INTO task_sessions(workspace_id,session_id,task_id) VALUES ('"<>uuid workspace<>"','"<>uuid identifier<>"','"<>uuid task.id<>"')")
  if terminal then runSession env.pool (S.sql ("UPDATE tasks SET status='done' WHERE id='"<>uuid task.id<>"'")) else pure ()
  pure identifier

awaitSealed :: TestEnv -> Chan () -> UUID -> UUID -> Int -> IO ()
awaitSealed env batches workspace identifier remaining = do
  timeout 5000000 (readChan batches) `shouldReturn` Just ()
  Just row <- getSession env.pool workspace identifier
  if row.state == Terminated then pure ()
  else if remaining <= (0 :: Int) then expectationFailure "periodic reconciliation did not seal eligible session"
  else awaitSealed env batches workspace identifier (remaining-1)

queryBool :: TestEnv -> BS.ByteString -> IO Bool
queryBool env sql = runSession env.pool $ S.statement () $ St.Statement sql E.noParams (D.singleRow (D.column (D.nonNullable D.bool))) False

uuid :: UUID -> BS.ByteString
uuid = B8.pack . show
