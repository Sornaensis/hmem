module HMem.Server.AgentSessionSpec (spec) where

import Control.Concurrent.Async (concurrently, withAsync, cancel, waitCatch)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (bracket, throwIO, try, SomeException, displayException)
import System.Directory (getTemporaryDirectory, removeFile, listDirectory)
import System.IO (openBinaryTempFile, hClose)
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))
import Network.Wai.Internal (ResponseReceived(..))
import Control.Monad (forM, forM_, replicateM)
import Data.Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.UUID (UUID)
import Data.UUID.V4 qualified as UUID
import Hasql.Session qualified as SQL
import Network.HTTP.Types
import Network.Wai qualified as Wai
import Network.Wai.Test (SRequest(..), SResponse(..), runSession, srequest)
import Test.Hspec

import HMem.AgentSession qualified as Core
import HMem.AgentSession.Transport
import HMem.Config qualified as Config
import HMem.DB.AgentSession qualified as DB
import HMem.DB.Auth qualified as Auth
import HMem.DB.Pool qualified as Pool
import HMem.DB.Task qualified as Task
import HMem.DB.Project qualified as Project
import HMem.DB.TestHarness (TestEnv(..), createTestWorkspace)
import HMem.Server.AuthTokens (IssuedAccessToken(..))
import HMem.Server.TestHarness
import HMem.Types qualified as Types

spec :: Spec
spec = describe "Agent session HTTP" $ do
  it "activity projects include unloaded nested and null-project tasks across moves, status and deletion" $
    withLocalSandboxAppContext $ \ctx -> do
      let env=ctx.localEnv
      workspace <- createTestWorkspace env "activity-tree"
      root <- Project.createProject env.pool (Types.CreateProject workspace.id Nothing "Root" Nothing Nothing Nothing)
      middle <- Project.createProject env.pool (Types.CreateProject workspace.id (Just root.id) "Middle" Nothing Nothing Nothing)
      leaf <- Project.createProject env.pool (Types.CreateProject workspace.id (Just middle.id) "Unloaded leaf" Nothing Nothing Nothing)
      destination <- Project.createProject env.pool (Types.CreateProject workspace.id Nothing "Destination" Nothing Nothing Nothing)
      parent <- Task.createTask env.pool (Types.CreateTask workspace.id (Just leaf.id) Nothing "Parent" Nothing Nothing Nothing)
      child <- Task.createTask env.pool (Types.CreateTask workspace.id (Just leaf.id) (Just parent.id) "Null-project subtask" Nothing Nothing Nothing)
      -- Legacy/direct-SQL rows can omit a subtask project. Keep every database
      -- guard enabled; the projection must follow its canonical parent link.
      Pool.runSession env.pool $ SQL.sql ("UPDATE tasks SET project_id=NULL WHERE id='"<>uuid child.id<>"'")
      fmap (.projectId) <$> Task.getTask env.pool child.id `shouldReturn` Just Nothing
      identifier <- UUID.nextRandom
      Pool.runSession env.pool $ SQL.sql ("INSERT INTO sessions(id,workspace_id,format_label,created_at,last_seen_at) VALUES ('"<>uuid identifier<>"','"<>uuid workspace.id<>"','quiet activity',clock_timestamp()-interval '1 hour',clock_timestamp()-interval '1 hour')")
      DB.associateSessionTask env.pool workspace.id identifier child.id
      DB.projectSessionActivity env.pool workspace.id [root.id,middle.id,leaf.id] >>= (`shouldMatchList` [(root.id,True),(middle.id,True),(leaf.id,True)])
      _ <- Task.updateTask env.pool parent.id (Types.UpdateTask Nothing Types.Unchanged (Types.SetTo destination.id) Types.Unchanged Nothing Nothing Nothing)
      DB.projectSessionActivity env.pool workspace.id [root.id,destination.id] >>= (`shouldMatchList` [(root.id,False),(destination.id,True)])
      _ <- Task.updateTask env.pool parent.id (Types.UpdateTask Nothing Types.Unchanged Types.Unchanged Types.Unchanged (Just Types.Cancelled) Nothing Nothing)
      DB.projectSessionActivity env.pool workspace.id [destination.id] `shouldReturn` [(destination.id,True)]
      _ <- DB.terminateSession env.pool workspace.id identifier (Core.TerminateSession 0 0 0 (Core.sessionSha256 ""))
      DB.projectSessionActivity env.pool workspace.id [destination.id] `shouldReturn` [(destination.id,False)]
      unlinked <- seed env workspace.id
      DB.workspaceSessionActivity env.pool [workspace.id] `shouldReturn` [(workspace.id,True)]
      DB.associateSessionTask env.pool workspace.id unlinked.id child.id
      _ <- Task.deleteTask env.pool child.id
      DB.projectSessionActivity env.pool workspace.id [destination.id] `shouldReturn` [(destination.id,False)]
      _ <- Project.deleteProject env.pool destination.id
      DB.projectSessionActivity env.pool workspace.id [destination.id] `shouldReturn` []

  it "activity batches authorize each scope, omit unknown targets and cap unique targets before work" $
    withDeployedSandboxAppContext $ \ctx -> do
      let env=ctx.deployedEnv; app=ctx.deployedApplication
      workspace <- createTestWorkspace env "activity-reader"
      foreignWorkspace <- createTestWorkspace env "activity-foreign"
      project <- Project.createProject env.pool (Types.CreateProject workspace.id Nothing "Visible" Nothing Nothing Nothing)
      foreignProject <- Project.createProject env.pool (Types.CreateProject foreignWorkspace.id Nothing "Hidden" Nothing Nothing Nothing)
      _ <- seed env foreignWorkspace.id
      reader <- audience ctx workspace.id (Just Auth.WorkspaceRoleRead) False
      let path="/api/v1/session-activity?workspace_id="<>uuid workspace.id<>"&workspace_id="<>uuid foreignWorkspace.id<>"&project_workspace_id="<>uuid workspace.id<>"&project_id="<>uuid project.id<>"&project_id="<>uuid foreignProject.id
      call app methodGet path [] "" >>= status status401
      response <- call app methodGet path reader "" >>= decoded @Value
      field ["workspaces"] response `shouldBe` Just (toJSON [object ["id" .= workspace.id,"active" .= False]])
      field ["projects"] response `shouldBe` Just (toJSON [object ["id" .= project.id,"active" .= False]])
      call app methodGet ("/api/v1/session-activity?project_workspace_id="<>uuid foreignWorkspace.id<>"&project_id="<>uuid foreignProject.id) reader "" >>= status status403
      let over="/api/v1/session-activity?"<>BS.intercalate "&" ["workspace_id=invalid-"<>TE.encodeUtf8 (T.pack $ show n) | n<-[1..51::Int]]
      call app methodGet over reader "" >>= status status400
      let duplicates="/api/v1/session-activity?"<>BS.intercalate "&" (replicate 51 ("workspace_id="<>uuid workspace.id))
      call app methodGet duplicates reader "" >>= ok

  it "streams every task-associated terminated session as genuine DEFLATE ZIP64 without crossing task or auth scope" $
    withDeployedSandboxAppContext $ \ctx -> do
      let env=ctx.deployedEnv; app=ctx.deployedApplication
      workspace <- createTestWorkspace env "compressed-task-evidence"
      task <- makeTask env workspace.id
      other <- makeTask env workspace.id
      foreignWorkspace <- createTestWorkspace env "other-evidence-workspace"
      foreignTask <- makeTask env foreignWorkspace.id
      rows <- forM [0..52 :: Int] $ \n -> do
        let bytes=case n `mod` 4 of
              0 -> ""
              1 -> TE.encodeUtf8 "{\"text\":\"æ雪 café\"}\r\n"
              2 -> "[true,null]"
              _ -> TE.encodeUtf8 ("\"" <> T.replicate 140000 "x" <> "\"\n")
        sealed <- seedEvidence env workspace.id task.id "Review / café ../capture" bytes
        pure (sealed,bytes)
      otherRow <- seedEvidence env workspace.id other.id "Excluded other task" "true\n"
      active <- seed env workspace.id
      DB.associateSessionTask env.pool workspace.id active.id task.id
      reader <- audience ctx workspace.id (Just Auth.WorkspaceRoleRead) False
      outsider <- audience ctx workspace.id Nothing False
      let path=taskArchive workspace.id task.id
      call app methodGet path [] "" >>= status status401
      call app methodGet path outsider "" >>= status status403
      call app methodGet (taskArchive workspace.id foreignTask.id) reader "" >>= status status404
      result <- call app methodGet path reader ""
      ok result
      lookup hContentType result.simpleHeaders `shouldBe` Just "application/zip"
      lookup "Cache-Control" result.simpleHeaders `shouldBe` Just "no-store"
      lookup "Content-Disposition" result.simpleHeaders `shouldSatisfy` maybe False (BS.isPrefixOf "attachment;")
      verifyArchive result.simpleBody rows
      one <- call app methodGet (taskArchive workspace.id other.id) reader ""
      ok one
      verifyArchive one.simpleBody [(otherRow,"true\n")]
      empty <- makeTask env workspace.id
      call app methodGet (taskArchive workspace.id empty.id) reader "" >>= (\response -> ok response >> verifyArchive response.simpleBody [])
  it "holds only two export admissions and releases disconnects before the first stream write" $
    withLocalSandboxAppContext $ \ctx -> do
      let env=ctx.localEnv; app=ctx.localApplication
      workspace <- createTestWorkspace env "archive-admission"
      task <- makeTask env workspace.id
      entered <- newEmptyMVar
      hold <- newEmptyMVar
      let req=request methodGet (taskArchive workspace.id task.id) []
          blocked=app req (\_ -> putMVar entered () >> takeMVar hold >> pure ResponseReceived)
      withAsync blocked $ \first -> do
        takeMVar entered
        withAsync blocked $ \second -> do
          takeMVar entered
          overloaded <- call app methodGet (taskArchive workspace.id task.id) [] ""
          status status503 overloaded
          lookup "Retry-After" overloaded.simpleHeaders `shouldBe` Just "1"
          cancel first; cancel second
          _ <- waitCatch first; _ <- waitCatch second
          recovered <- call app methodGet (taskArchive workspace.id task.id) [] ""
          ok recovered
          verifyArchive recovered.simpleBody []

  it "aborts failed stream callbacks without leaking details, releases admission and removes the index" $
    withLocalSandboxAppContext $ \ctx -> do
      let env=ctx.localEnv; app=ctx.localApplication
      workspace <- createTestWorkspace env "archive-stream-failure"
      task <- makeTask env workspace.id
      _ <- seedEvidence env workspace.id task.id "Private capture" "true\n"
      temporary <- getTemporaryDirectory
      let indexes = filter (T.isPrefixOf "hmem-task-evidence-index-" . T.pack) <$> listDirectory temporary
      before <- indexes
      result <- try @SomeException $ app (request methodGet (taskArchive workspace.id task.id) []) $ \response -> do
        let (_,_,withBody)=Wai.responseToStream response
        withBody $ \body -> body (\_ -> throwIO (userError "private storage details")) (pure ())
        pure ResponseReceived
      case result of
        Left failure -> do
          let message=T.pack (displayException failure)
          message `shouldSatisfy` T.isPrefixOf "user error (Task evidence archive failed)"
          message `shouldSatisfy` (not . T.isInfixOf "private storage details")
        Right _ -> expectationFailure "stream failure was reported as success"
      indexes >>= (`shouldMatchList` before)
      call app methodGet (taskArchive workspace.id task.id) [] "" >>= ok

  it "applies lifecycle/freshness predicates before paging and authorizes before invalid filters" $
    withDeployedSandboxAppContext $ \ctx -> do
      let env=ctx.deployedEnv; app=ctx.deployedApplication
      workspace <- createTestWorkspace env "http-session-filters"
      row <- seed env workspace.id
      newer <- seed env workspace.id
      sealed <- DB.terminateSession env.pool workspace.id row.id (Core.TerminateSession 0 0 0 (Core.sessionSha256 ""))
      reader <- audience ctx workspace.id (Just Auth.WorkspaceRoleRead) False
      outsider <- audience ctx workspace.id Nothing False
      let root=sessions workspace.id
      closed <- call app methodGet (root<>"?limit=1&state=terminated&freshness=fresh") reader "" >>= decoded @SessionPageResponse
      closed.items `shouldBe` [sealed]
      closed.nextCursor `shouldBe` Nothing
      active <- call app methodGet (root<>"?limit=1&state=active") reader "" >>= decoded @SessionPageResponse
      active.items `shouldBe` [newer]
      stale <- call app methodGet (root<>"?freshness=stale") reader "" >>= decoded @SessionPageResponse
      stale.items `shouldBe` []
      forM_ ["?state=invalid","?freshness=invalid"] $ \query -> do
        call app methodGet (root<>query) reader "" >>= status status400
        call app methodGet (root<>query) outsider "" >>= status status403
        call app methodGet (root<>query) [] "" >>= status status401
  it "captures live JSONL, replays lost acknowledgments, links tasks and reconstructs sealed bytes/hash" $
    withLocalSandboxAppContext $ \ctx -> do
      let env = ctx.localEnv; app = ctx.localApplication
      workspace <- createTestWorkspace env "http-session-workflow"
      identifier <- UUID.nextRandom
      task <- makeTask env workspace.id
      let root = sessions workspace.id
          path = root <> "/" <> uuid identifier
          input = CreateSessionRequest identifier "neutral-jsonl"
          body = TE.encodeUtf8 "{\"message\":\"æ雪\"}\r\n[true,null]\n{}"
      created <- post app root [] (toJSON input) >>= decoded @Core.AgentSession
      retried <- post app root [] (toJSON input) >>= decoded @Core.AgentSession
      retried `shouldBe` created
      uploaded <- post app (path<>"/append") [] (toJSON $ AppendSessionRequest 0 0 (encodeSessionBytes body) True) >>= decoded @AppendSessionResponse
      uploaded.session.state `shouldBe` Core.Active
      repeated <- post app (path<>"/append") [] (toJSON $ AppendSessionRequest 0 0 (encodeSessionBytes body) True) >>= decoded @AppendSessionResponse
      repeated.replayed `shouldBe` True
      repeated.session `shouldBe` uploaded.session
      _ <- call app methodPut (path<>"/tasks/"<>uuid task.id) [] "" >>= ok
      _ <- call app methodPut (path<>"/tasks/"<>uuid task.id) [] "" >>= ok
      linked <- call app methodGet (path<>"/tasks") [] "" >>= decoded @SessionTaskPageResponse
      map (.id) linked.items `shouldBe` [task.id]
      active <- call app methodGet (path<>"/transcript?offset=1&limit=3") [] "" >>= decoded @SessionRangeResponse
      decodeSessionBytes active.dataBase64 `shouldBe` Right (BS.take 3 (BS.drop 1 body))
      call app methodGet (path<>"/evidence") [] "" >>= status status409
      let receipt = TerminateSessionRequest uploaded.session.committedBytes uploaded.session.revision (fromIntegral $ BS.length body) (Core.sessionSha256 body)
      sealed <- post app (path<>"/terminate") [] (toJSON receipt) >>= decoded @Core.AgentSession
      post app (path<>"/terminate") [] (toJSON receipt) >>= decoded @Core.AgentSession >>= (`shouldBe` sealed)
      sealed.state `shouldBe` Core.Terminated
      rebuilt <- download app path 0
      rebuilt `shouldBe` body
      Just (Core.sessionSha256 rebuilt) `shouldBe` sealed.finalSha256
      Just (fromIntegral $ BS.length rebuilt) `shouldBe` sealed.finalBytes
      call app methodGet (path<>"/transcript") [] "" >>= status status409
      post app (path<>"/append") [] (toJSON $ AppendSessionRequest sealed.committedBytes sealed.revision "Cg==" False) >>= status status409
      call app methodPost (path<>"/heartbeat") [] "" >>= status status409
      later <- makeTask env workspace.id
      call app methodPut (path<>"/tasks/"<>uuid later.id) [] "" >>= ok
      taskPage <- call app methodGet (taskSessions workspace.id later.id) [] "" >>= decoded @SessionPageResponse
      map (.id) taskPage.items `shouldBe` [identifier]
      Just unchanged <- Task.getTask env.pool task.id
      unchanged.status `shouldBe` Types.Todo
      forM_ [methodDelete,methodPut] $ \method -> call app method path [] "{}" >>= (\r -> simpleStatus r `shouldSatisfy` (`elem` [status404,status405]))

  it "uses strict JSON/base64, rejects malformed tails and oversize chunks without changing the cursor or leaking records" $
    withLocalSandboxAppContext $ \ctx -> do
      workspace <- createTestWorkspace ctx.localEnv "http-session-invalid"
      row <- seed ctx.localEnv workspace.id
      let app=ctx.localApplication; path=sessions workspace.id<>"/"<>uuid row.id
      forM_ ["YQ","YQ=","YQ===","YR==","YQ==\n","____","雪"] $ \encoded ->
        post app (path<>"/append") [] (toJSON $ AppendSessionRequest 0 0 encoded True) >>= status status400
      forM_ ["{\"private\":\"uploaded-secret-needle\"","{}\n{","\n",BS.pack [255,10]] $ \bytes -> do
        response <- post app (path<>"/append") [] (toJSON $ AppendSessionRequest 0 0 (encodeSessionBytes bytes) True)
        status status400 response
        BS.isInfixOf "uploaded-secret-needle" (LBS.toStrict $ simpleBody response) `shouldBe` False
      post app (path<>"/append") [] (object ["expected_offset" .= (0::Int),"expected_revision" .= (0::Int),"data_base64" .= ("e30="::Text),"local_path" .= ("private-path"::Text)]) >>= status status400
      post app (path<>"/append") [] (toJSON $ AppendSessionRequest 0 0 (T.replicate (maxSessionBase64Chars+1) "A") True) >>= status status413
      post app (path<>"/append") [] (toJSON $ AppendSessionRequest 0 0 (encodeSessionBytes $ BS.replicate (Core.maxSessionChunkBytes+1) 32) True) >>= status status413
      DB.getSession ctx.localEnv.pool workspace.id row.id `shouldReturn` Just row
      call app methodPost (sessions workspace.id) [] "{" >>= status status400
      let boundary = "\""<>BS.replicate (Core.maxSessionChunkBytes-3) 97<>"\"\n"
      accepted <- post app (path<>"/append") [] (toJSON $ AppendSessionRequest 0 0 (encodeSessionBytes boundary) False) >>= decoded @AppendSessionResponse
      accepted.session.committedBytes `shouldBe` fromIntegral Core.maxSessionChunkBytes
      defaultWindow <- call app methodGet (path<>"/transcript") [] "" >>= decoded @SessionRangeResponse
      defaultBytes <- either (fail . show) pure (decodeSessionBytes defaultWindow.dataBase64)
      BS.length defaultBytes `shouldBe` defaultSessionRangeBytes
      fullWindow <- call app methodGet (path<>"/transcript?limit=1048576") [] "" >>= decoded @SessionRangeResponse
      exactBytes <- either (fail . show) pure (decodeSessionBytes fullWindow.dataBase64)
      BS.length exactBytes `shouldBe` Core.maxSessionChunkBytes
      Core.sessionSha256 exactBytes `shouldBe` Core.sessionSha256 boundary

  it "bounds both known-length and chunked bodies before JSON decode and preserves authorization-first denial" $
    withDeployedSandboxAppContext $ \ctx -> do
      workspace <- createTestWorkspace ctx.deployedEnv "http-body-limit"
      reader <- audience ctx workspace.id (Just Auth.WorkspaceRoleRead) False
      editor <- audience ctx workspace.id (Just Auth.WorkspaceRoleEdit) False
      let huge = LBS.replicate (fromIntegral maxSessionRequestBytes+1) 32
          target = sessions workspace.id
      forM_ [Wai.KnownLength (fromIntegral maxSessionRequestBytes+1),Wai.ChunkedBody] $ \lengthMode -> do
        rawWithLength ctx.deployedApplication target editor huge lengthMode >>= status status413
        rawWithLength ctx.deployedApplication target reader huge lengthMode >>= status status403
        rawWithLength ctx.deployedApplication target [] huge lengthMode >>= status status401
      row <- seed ctx.deployedEnv workspace.id
      forM_ [target<>"/",target<>"/"<>uuid row.id<>"/append/",target<>"/"<>uuid row.id<>"/terminate/"] $ \trailing ->
        forM_ [Wai.KnownLength (fromIntegral maxSessionRequestBytes+1),Wai.ChunkedBody] $ \lengthMode -> do
          rawWithLength ctx.deployedApplication trailing editor huge lengthMode >>= status status413
          rawWithLength ctx.deployedApplication trailing reader huge lengthMode >>= status status403
      rawWithLength ctx.deployedApplication target editor (LBS.replicate (fromIntegral maxSessionRequestBytes) 32) Wai.ChunkedBody >>= status status400
      page <- call ctx.deployedApplication methodGet target editor "" >>= decoded @SessionPageResponse
      page.items `shouldBe` [row]

  it "authorizes every read audience and every mutation role without exposing foreign or deleted tasks" $
    withDeployedSandboxAppContext $ \ctx -> do
      let env=ctx.deployedEnv; app=ctx.deployedApplication
      workspace <- createTestWorkspace env "http-roles"
      other <- createTestWorkspace env "http-other"
      row <- seed env workspace.id
      foreignRow <- seed env other.id
      task <- makeTask env workspace.id
      foreignTask <- makeTask env other.id
      DB.associateSessionTask env.pool workspace.id row.id task.id
      reader <- audience ctx workspace.id (Just Auth.WorkspaceRoleRead) False
      editor <- audience ctx workspace.id (Just Auth.WorkspaceRoleEdit) False
      admin <- audience ctx workspace.id (Just Auth.WorkspaceRoleAdmin) False
      superadmin <- audience ctx workspace.id Nothing True
      outsider <- audience ctx workspace.id Nothing False
      let root=sessions workspace.id; path=root<>"/"<>uuid row.id
          reads=[root,path,path<>"/tasks",path<>"/transcript",taskSessions workspace.id task.id]
      forM_ reads $ \target -> do
        forM_ [reader,editor,admin,superadmin] $ \headers -> call app methodGet target headers "" >>= ok
        call app methodGet target outsider "" >>= status status403
        call app methodGet target [] "" >>= status status401
      forM_ [root<>"?limit=invalid",path<>"/transcript?offset=invalid"] $ \target -> call app methodGet target outsider "" >>= status status403
      forM_ [(root,methodPost),(path<>"/append",methodPost),(path<>"/heartbeat",methodPost),(path<>"/terminate",methodPost),(path<>"/tasks/"<>uuid task.id,methodPut)] $ \(target,method) -> do
        call app method target reader "{" >>= status status403
        call app method target outsider "{" >>= status status403
        call app method target [] "{" >>= status status401
      forM_ [editor,admin,superadmin] $ \headers -> do
        identifier <- UUID.nextRandom
        created <- post app root headers (toJSON $ CreateSessionRequest identifier "role-jsonl") >>= decoded @Core.AgentSession
        created.creatorActorType `shouldBe` Just "bot"
        let target=root<>"/"<>uuid identifier
        call app methodPost (target<>"/heartbeat") headers "" >>= ok
        uploaded <- post app (target<>"/append") headers (toJSON $ AppendSessionRequest 0 0 "e30=" True) >>= decoded @AppendSessionResponse
        _ <- post app (target<>"/terminate") headers (toJSON $ TerminateSessionRequest 2 uploaded.session.revision 2 (Core.sessionSha256 "{}")) >>= ok
        call app methodPut (target<>"/tasks/"<>uuid task.id) headers "" >>= ok
        call app methodGet (target<>"/evidence") reader "" >>= ok
        call app methodGet (target<>"/evidence") outsider "" >>= status status403
      call app methodGet (root<>"/"<>uuid foreignRow.id) editor "" >>= status status404
      call app methodPut (path<>"/tasks/"<>uuid foreignTask.id) editor "" >>= status status404
      call app methodGet (taskSessions workspace.id foreignTask.id) editor "" >>= status status404
      Pool.runSession env.pool $ SQL.sql ("UPDATE tasks SET deleted_at=clock_timestamp() WHERE id='"<>uuid task.id<>"'")
      call app methodGet (taskSessions workspace.id task.id) reader "" >>= status status404
      hidden <- call app methodGet (path<>"/tasks") reader "" >>= decoded @SessionTaskPageResponse
      hidden.items `shouldBe` []
      Pool.runSession env.pool $ SQL.sql ("UPDATE tasks SET deleted_at=NULL WHERE id='"<>uuid task.id<>"'")
      restored <- call app methodGet (path<>"/tasks") reader "" >>= decoded @SessionTaskPageResponse
      map (.id) restored.items `shouldBe` [task.id]

  it "requires cookie CSRF and credential-bot declaration while preserving creator-independent edit authority" $
    withDeployedSandboxAppContext $ \ctx -> do
      workspace <- createTestWorkspace ctx.deployedEnv "http-session-csrf"
      user <- createDeployedSandboxUser ctx.deployedEnv False False
      _ <- Auth.upsertWorkspaceMembership ctx.deployedEnv.pool workspace.id (Auth.UpsertWorkspaceMembership user Auth.WorkspaceRoleEdit) Nothing
      _ <- createDeployedSandboxAuthSession ctx.deployedEnv user "session-token" "csrf-value"
      let cfg=ctx.deployedConfig.auth.deployed
          cookies=[("Cookie",TE.encodeUtf8 (cfg.sessionCookieName<>"=session-token; "<>cfg.csrfCookieName<>"=csrf-value"))]
          valid=cookies<>[("X-CSRF-Token","csrf-value")]
      identifier <- UUID.nextRandom
      call ctx.deployedApplication methodPost (sessions workspace.id) cookies "{" >>= status status403
      created <- post ctx.deployedApplication (sessions workspace.id) valid (toJSON $ CreateSessionRequest identifier "cookie-jsonl") >>= decoded @Core.AgentSession
      created.creatorActorType `shouldBe` Just "user"
      token <- issueDeployedSandboxPAT ctx.deployedEnv user "later-editor"
      let bearer=[("Authorization","Bearer "<>TE.encodeUtf8 token.rawToken)]
          path=sessions workspace.id<>"/"<>uuid identifier
      call ctx.deployedApplication methodPost (path<>"/heartbeat") bearer "" >>= status status400
      sealed <- post ctx.deployedApplication (path<>"/terminate") (bearer<>[identityHeader]) (toJSON $ TerminateSessionRequest 0 0 0 (Core.sessionSha256 "")) >>= decoded @Core.AgentSession
      sealed.state `shouldBe` Core.Terminated
      sealed.creatorActorId `shouldBe` created.creatorActorId

  it "keeps finite EOF flush active, enforces its separator, rejects receipt races and needs no uploader" $
    withLocalSandboxAppContext $ \ctx -> do
      workspace <- createTestWorkspace ctx.localEnv "http-session-race"
      row <- seed ctx.localEnv workspace.id
      let app=ctx.localApplication; path=sessions workspace.id<>"/"<>uuid row.id
      first <- post app (path<>"/append") [] (toJSON $ AppendSessionRequest 0 0 "e30=" True) >>= decoded @AppendSessionResponse
      first.session.state `shouldBe` Core.Active
      call app methodPost (path<>"/heartbeat") [] "" >>= decoded @Core.AgentSession >>= (\r -> r.state `shouldBe` Core.Active)
      post app (path<>"/append") [] (toJSON $ AppendSessionRequest 2 1 "dHJ1ZQo=" False) >>= status status400
      (appended,sealed) <- concurrently
        (post app (path<>"/append") [] (toJSON $ AppendSessionRequest 2 1 "CnRydWUK" False))
        (post app (path<>"/terminate") [] (toJSON $ TerminateSessionRequest 2 1 2 (Core.sessionSha256 "{}")))
      length [() | r<-[appended,sealed],simpleStatus r==status200] `shouldBe` 1
      length [() | r<-[appended,sealed],simpleStatus r==status409] `shouldBe` 1
      -- Heartbeating, stale or never-followed sessions all use the same seal API.
      forM_ [False,True] $ \heartbeat -> do
        independent <- seed ctx.localEnv workspace.id
        let target=sessions workspace.id<>"/"<>uuid independent.id
        if heartbeat then call app methodPost (target<>"/heartbeat") [] "" >> pure () else pure ()
        post app (target<>"/terminate") [] (toJSON $ TerminateSessionRequest 0 0 0 (Core.sessionSha256 "")) >>= ok
      staleId <- UUID.nextRandom
      Pool.runSession ctx.localEnv.pool $ SQL.sql ("INSERT INTO sessions(id,workspace_id,format_label,created_at,last_seen_at) VALUES ('"<>uuid staleId<>"','"<>uuid workspace.id<>"','jsonl',clock_timestamp()-interval '1 hour',clock_timestamp()-interval '1 hour')")
      post app (sessions workspace.id<>"/"<>uuid staleId<>"/terminate") [] (toJSON $ TerminateSessionRequest 0 0 0 (Core.sessionSha256 "")) >>= ok

  it "preserves keyset cursors, bounded task pages/ranges and rejects partial or invalid continuation inputs" $
    withLocalSandboxAppContext $ \ctx -> do
      workspace <- createTestWorkspace ctx.localEnv "http-session-pages"
      rows <- replicateM 3 (seed ctx.localEnv workspace.id)
      let app=ctx.localApplication; root=sessions workspace.id
      first <- call app methodGet (root<>"?limit=1") [] "" >>= decoded @SessionPageResponse
      cursor <- maybe (fail "missing cursor") pure first.nextCursor
      _ <- seed ctx.localEnv workspace.id
      second <- call app methodGet (root<>cursorQuery cursor) [] "" >>= decoded @SessionPageResponse
      map (.id) second.items `shouldBe` [ (rows!!1).id ]
      forM_ ["?limit=0","?limit=101","?limit=bad","?before_id=bad","?before_created_at=bad&before_id=bad"] $ \query -> call app methodGet (root<>query) [] "" >>= status status400
      let row=head rows; path=root<>"/"<>uuid row.id
      forM_ ["?offset=-1","?limit=0","?limit=1048577"] $ \query -> call app methodGet (path<>"/transcript"<>query) [] "" >>= status status400
      call app methodGet (path<>"/transcript?offset=1") [] "" >>= status status409
      tasks <- replicateM 3 (makeTask ctx.localEnv workspace.id)
      forM_ tasks $ \task -> call app methodPut (path<>"/tasks/"<>uuid task.id) [] "" >>= ok
      taskPage <- call app methodGet (path<>"/tasks?limit=1") [] "" >>= decoded @SessionTaskPageResponse
      after <- maybe (fail "missing task cursor") pure taskPage.nextTaskId
      rest <- call app methodGet (path<>"/tasks?limit=100&after_task_id="<>uuid after) [] "" >>= decoded @SessionTaskPageResponse
      length rest.items `shouldBe` 2

  it "publishes typed OpenAPI session routes, base64 limits, immutable operations and structured errors" $
    withLocalSandboxAppContext $ \ctx -> do
      document <- call ctx.localApplication methodGet "/api/v1/openapi.json" [] "" >>= decoded @Value
      let prefix="/api/v1/workspaces/{workspaceId}/sessions"
      forM_ [(prefix,"post"),(prefix,"get"),(prefix<>"/{sessionId}/append","post"),(prefix<>"/{sessionId}/terminate","post"),(prefix<>"/{sessionId}/transcript","get"),(prefix<>"/{sessionId}/evidence","get")] $ \(path,method) -> do
        field ["paths",path,method] document `shouldSatisfy` (/=Nothing)
        forM_ ["400","401","403","404","409","413"] $ \code -> field ["paths",path,method,"responses",code] document `shouldSatisfy` (/=Nothing)
      field ["paths",prefix<>"/{sessionId}","delete"] document `shouldBe` Nothing
      field ["paths",prefix<>"/{sessionId}","put"] document `shouldBe` Nothing
      field ["components","schemas","AppendSessionRequest","properties","data_base64","maxLength"] document `shouldBe` Just (Number 1398104)

sessions :: UUID -> BS.ByteString
sessions workspace = "/api/v1/workspaces/"<>uuid workspace<>"/sessions"
taskSessions :: UUID -> UUID -> BS.ByteString
taskSessions workspace task = "/api/v1/workspaces/"<>uuid workspace<>"/tasks/"<>uuid task<>"/sessions"
uuid :: UUID -> BS.ByteString
uuid = TE.encodeUtf8 . T.pack . show

call :: Wai.Application -> Method -> BS.ByteString -> [Header] -> LBS.ByteString -> IO SResponse
call app method path headers body = runSession (srequest (SRequest (request method path headers) body)) app
request :: Method -> BS.ByteString -> [Header] -> Wai.Request
request method path headers = Wai.defaultRequest
  { Wai.requestMethod=method, Wai.rawPathInfo=raw, Wai.pathInfo=filter (not . T.null) (T.splitOn "/" (TE.decodeUtf8 raw))
  , Wai.rawQueryString=query, Wai.queryString=parseQuery query, Wai.requestHeaders=("Content-Type","application/json"):headers }
  where (raw,query)=BS.break (==63) path
post :: Wai.Application -> BS.ByteString -> [Header] -> Value -> IO SResponse
post app path headers = call app methodPost path headers . encode
status :: Status -> SResponse -> IO ()
status expected response = simpleStatus response `shouldBe` expected
ok :: SResponse -> IO ()
ok = status status200
decoded :: FromJSON a => SResponse -> IO a
decoded response = do
  ok response
  either fail pure (eitherDecode $ simpleBody response)

seed :: TestEnv -> UUID -> IO Core.AgentSession
seed env workspace = do
  identifier <- UUID.nextRandom
  DB.createSession env.pool (Core.CreateSession identifier workspace "jsonl")
makeTask :: TestEnv -> UUID -> IO Types.Task
makeTask env workspace = Task.createTask env.pool (Types.CreateTask workspace Nothing Nothing "session-task" Nothing Nothing Nothing)
identityHeader :: Header
identityHeader=("X-HMem-Bot-Identity","{\"role\":\"Session API Test\",\"model-id\":\"test:fixture:default\"}")
audience :: DeployedSandboxApp -> UUID -> Maybe Auth.WorkspaceRole -> Bool -> IO [Header]
audience ctx workspace role superadmin = do
  user <- createDeployedSandboxUser ctx.deployedEnv False superadmin
  forM_ role $ \grant -> do
    _ <- Auth.upsertWorkspaceMembership ctx.deployedEnv.pool workspace (Auth.UpsertWorkspaceMembership user grant) Nothing
    pure ()
  token <- issueDeployedSandboxPAT ctx.deployedEnv user "session-audience"
  pure [identityHeader,("Authorization","Bearer "<>TE.encodeUtf8 token.rawToken)]
rawWithLength :: Wai.Application -> BS.ByteString -> [Header] -> LBS.ByteString -> Wai.RequestBodyLength -> IO SResponse
rawWithLength app path headers body lengthMode = runSession (srequest (SRequest rawRequest body)) app
  where
    base = request methodPost path headers
    -- Preserve Warp-style empty trailing path segments; the ordinary helper's
    -- filtering would hide Servant's accepted trailing-slash spelling.
    rawRequest = base {Wai.requestBodyLength=lengthMode,Wai.pathInfo=drop 1 (T.splitOn "/" (TE.decodeUtf8 $ Wai.rawPathInfo base))}
download :: Wai.Application -> BS.ByteString -> Int -> IO BS.ByteString
download app path offset = do
  part <- call app methodGet (path<>"/evidence?limit=3&offset="<>TE.encodeUtf8 (T.pack $ show offset)) [] "" >>= decoded @SessionRangeResponse
  bytes <- either (fail . show) pure (decodeSessionBytes part.dataBase64)
  rest <- if part.hasMore then download app path (fromIntegral part.nextOffset) else pure BS.empty
  pure (bytes<>rest)
cursorQuery :: Core.SessionPageCursor -> BS.ByteString
cursorQuery cursor = renderQuery True [("limit",Just "1"),("before_created_at",Just $ TE.encodeUtf8 $ T.pack $ iso8601Show cursor.createdAt),("before_id",Just $ uuid cursor.id)]
field :: [Text] -> Value -> Maybe Value
field [] value=Just value
field (key:rest) (Object objectValue)=KM.lookup (Key.fromText key) objectValue >>= field rest
field _ _=Nothing


taskArchive :: UUID -> UUID -> BS.ByteString
taskArchive workspace task = "/api/v1/workspaces/"<>uuid workspace<>"/tasks/"<>uuid task<>"/evidence.zip"

seedEvidence :: TestEnv -> UUID -> UUID -> Text -> BS.ByteString -> IO Core.AgentSession
seedEvidence env workspace task label bytes = do
  identifier <- UUID.nextRandom
  created <- DB.createSession env.pool (Core.CreateSession identifier workspace label)
  uploaded <- if BS.null bytes then pure created else (.session) <$> DB.appendSession env.pool workspace identifier (Core.AppendSession 0 0 bytes True)
  sealed <- DB.terminateSession env.pool workspace identifier (Core.TerminateSession uploaded.committedBytes uploaded.revision uploaded.committedBytes (Core.sessionSha256 bytes))
  DB.associateSessionTask env.pool workspace identifier task
  pure sealed

verifyArchive :: LBS.ByteString -> [(Core.AgentSession,BS.ByteString)] -> IO ()
verifyArchive body rows = do
  temporary <- getTemporaryDirectory
  bracket (openBinaryTempFile temporary "hmem-zip-reader-") (\(name,handle) -> hClose handle >> removeFile name) $ \(name,handle) -> do
    LBS.hPut handle body
    hClose handle
    let expected=encode $ object [Key.fromText (T.pack $ show session.id) .= Core.sessionSha256 bytes | (session,bytes)<-rows]
        program="import sys,json,zipfile,hashlib\nexpected=json.loads(sys.stdin.read())\nwith zipfile.ZipFile(sys.argv[1]) as z:\n assert z.testzip() is None\n assert len(z.infolist())==len(expected)\n seen=set()\n for info in z.infolist():\n  assert info.compress_type==8\n  key=next((key for key in expected if info.filename.endswith(key+'.jsonl')),None)\n  assert key is not None and key not in seen\n  seen.add(key)\n  assert '/' not in info.filename and not info.filename.startswith('.')\n  assert hashlib.sha256(z.read(info)).hexdigest()==expected[key]\n if len(expected)>50: assert sum(i.compress_size for i in z.infolist()) < sum(i.file_size for i in z.infolist())\n"
    (code,_,_) <- readProcessWithExitCode "python" ["-c",program,name] (map (toEnum . fromEnum) $ BS.unpack $ LBS.toStrict expected)
    code `shouldBe` ExitSuccess
