module HMem.Server.ChangeStreamSpec (spec) where

import Control.Concurrent (Chan, MVar, newChan, newEmptyMVar, putMVar, readChan, takeMVar, threadDelay, throwTo, tryTakeMVar, writeChan)
import Control.Concurrent.Async (Async, async, asyncWithUnmask, asyncThreadId, cancel, wait, waitCatch)
import Control.Exception (AsyncException(..), MaskingState(..), SomeException, bracket, catch, fromException, getMaskingState, mask, throwIO, try)
import Control.Monad (forM_, replicateM_)
import Data.Aeson (FromJSON, Value(..), decode, encode, object, toJSON, (.=))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Maybe (isJust)
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import GHC.Clock (getMonotonicTimeNSec)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as Text
import Data.Time (getCurrentTime)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUIDV4
import Hasql.Decoders qualified as Dec
import Hasql.Encoders qualified as Enc
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement
import Network.HTTP.Types (Header, methodGet, methodPost, parseQuery, status200)
import Network.HTTP.Types qualified as HTTP
import Network.Wai (Application, defaultRequest)
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp (testWithApplication)
import Network.Wai.Test qualified as WaiTest
import Network.WebSockets qualified as WS
import System.Timeout (timeout)
import System.IO (hPutStrLn, stderr)
import Test.Hspec

import HMem.Config qualified as Config
import HMem.BotIdentity (BotIdentity, mkBotIdentity, localDeclaredBotId)
import HMem.DB.Auth qualified as Auth
import HMem.AgentSession qualified as Agent
import HMem.AgentSession.Transport qualified as AgentWire
import HMem.DB.Task qualified as TaskDB
import HMem.DB.ChangeStream
import HMem.DB.Pool (runSession)
import HMem.DB.Pool qualified as DBPool
import HMem.DB.Project qualified as Project
import HMem.DB.Workspace qualified as WorkspaceDB
import HMem.DB.RequestContext (ActorType(..), Principal(..), PrincipalAuthority(..), withRequestContext, emptyRequestContext)
import HMem.DB.TestHarness (TestEnv(..), createTestWorkspace)
import HMem.DB.WorkspaceGroup qualified as WorkspaceGroup
import HMem.Server.TestHarness (DeployedSandboxApp(..), LocalSandboxApp(..), createDeployedSandboxUser, issueDeployedSandboxPAT, withDeployedSandboxAppContext, withLocalSandboxAppContext)
import HMem.Server.AuthTokens (IssuedAccessToken(..))
import HMem.Server.ChangeStream (ChangeStreamWorker, startChangeStreamWorker, startChangeStreamWorkerWithStartupHook, stopChangeStreamWorker)
import HMem.Server.Snapshot (materializeSnapshot, materializeSnapshotWithProfile)
import HMem.Server.WebSocket (createCanonicalTicket, createCanonicalTicketWithExpiry, createCanonicalTicketWithTtl, dispatchCanonicalOutbox, handleCanonicalDispatchFailure)
import HMem.Types (ChangeStreamResyncResponse(..), ChangeStreamSnapshotItem(..), CreateProject(..), CreateTask(..), CreateWorkspaceGroup(..), Project(..), Task(..), SnapshotProfile(..), UpdateWorkspace(..), WebSocketTicketResponse(..), Workspace(..), WorkspaceGroup(..))

spec :: Spec
spec = describe "canonical change-stream loopback" $ do
  it "delivers session metadata through replay and live HTTP without evidence bytes, duplicate effects or cross-workspace leakage" $
    withDeployedSandboxAppContext $ \ctx -> do
      workspace <- createTestWorkspace ctx.deployedEnv "session-stream"
      other <- createTestWorkspace ctx.deployedEnv "session-stream-other"
      user <- createDeployedSandboxUser ctx.deployedEnv False False
      outsider <- createDeployedSandboxUser ctx.deployedEnv False False
      forM_ [workspace.id,other.id] $ \id -> do
        _ <- Auth.upsertWorkspaceMembership ctx.deployedEnv.pool id (Auth.UpsertWorkspaceMembership user Auth.WorkspaceRoleEdit) Nothing
        pure ()
      _ <- Auth.upsertWorkspaceMembership ctx.deployedEnv.pool other.id (Auth.UpsertWorkspaceMembership outsider Auth.WorkspaceRoleRead) Nothing
      token <- issueDeployedSandboxPAT ctx.deployedEnv user "session writer"
      outsideToken <- issueDeployedSandboxPAT ctx.deployedEnv outsider "other scope"
      identifier <- UUIDV4.nextRandom
      let scope = WorkspaceScope workspace.id
          audience = AuthenticatedAudience (UUID.toText user) user
          headers = authHeaders token.rawToken
          root = "/api/v1/workspaces/" <> Text.encodeUtf8 (UUID.toText workspace.id) <> "/sessions"
          path = root <> "/" <> Text.encodeUtf8 (UUID.toText identifier)
          sentinel = "SESSION_WEBSOCKET_PRIVATE_SENTINEL"
          bytes = Text.encodeUtf8 ("{\"message\":\"" <> sentinel <> " æ雪\"}\r\n")
          post target value = apiRequest ctx.deployedApplication methodPost target headers (encode value)
          sessionRecords = filter (\record -> (jsonField "entity" record.outboxEnvelope >>= jsonField "type") `elem` [Just (String "agent_session"),Just (String "task_session")])
          noEvidence value = T.isInfixOf sentinel (Text.decodeUtf8 (LBS.toStrict (encode value))) `shouldBe` False
      resume <- terminalResume ctx scope audience
      issued <- createCanonicalTicket ctx.deployedWSState (grantPrincipal user) scope resume
      otherTicket <- canonicalTicketFor ctx (grantPrincipal outsider) (WorkspaceScope other.id) (AuthenticatedAudience (UUID.toText outsider) outsider)
      testWithApplication (pure ctx.deployedApplication) $ \port ->
        bracket (startOneFrameClient port otherTicket.ticket) (\client -> joinClient "session other-scope client" client.clientWorker) $ \otherClient -> do
          frameType <$> awaitFrame otherClient.clientInitial `shouldReturn` Just "checkpoint"
          response <- post root (toJSON (AgentWire.CreateSessionRequest identifier "jsonl" "Test session work" Nothing Nothing))
          responseStatus response `shouldBe` status200
          created <- decodeResponse response :: IO Agent.AgentSession
          responseStatus <$> post root (toJSON (AgentWire.CreateSessionRequest identifier "jsonl" "Test session work" Nothing Nothing)) `shouldReturn` status200
          uploadedResponse <- post (path<>"/append") (toJSON (AgentWire.AppendSessionRequest 0 0 (AgentWire.encodeSessionBytes bytes) False))
          responseStatus uploadedResponse `shouldBe` status200
          uploaded <- decodeResponse uploadedResponse :: IO AgentWire.AppendSessionResponse
          responseStatus <$> post (path<>"/append") (toJSON (AgentWire.AppendSessionRequest 0 0 (AgentWire.encodeSessionBytes bytes) False)) `shouldReturn` status200
          before <- sessionRecords <$> listOutboxAfter ctx.deployedEnv.pool scope 0 100
          length before `shouldBe` 2
          forM_ [FullV1,WorkspaceShellV1] $ \profile -> do
            items <- runSession ctx.deployedEnv.pool (materializeSnapshotWithProfile profile scope)
            map (jsonField "kind") items `shouldSatisfy` all (`notElem` [Just (String "agent_session"),Just (String "task_session"),Just (String "session_chunks")])
            noEvidence (toJSON items)
          frames <- newChan
          let receive = declaredPhase "session frame" (readChan frames >>= decodeFrame)
              client count ticket = asyncWithUnmask $ \unmask -> unmask $
                (WS.runClient "127.0.0.1" port ("/api/v1/ws?ticket=" <> showTicket ticket) $ \conn -> do
                  replicateM_ count (WS.receiveData conn >>= writeChan frames)
                  WS.sendClose conn ("session scenario complete" :: Text)) `catch` \(_ :: WS.ConnectionException) -> pure ()
          beat <- withDeclaredClient (client 4 issued.ticket) $ \_ -> do
            first <- receive; second <- receive; checkpoint <- receive
            map frameType [first,second,checkpoint] `shouldBe` [Just "change",Just "change",Just "checkpoint"]
            mapM_ noEvidence [first,second,checkpoint]
            map (\frame -> jsonField "event" frame >>= jsonField "entity" >>= jsonField "type") [first,second] `shouldBe` replicate 2 (Just (String "agent_session"))
            responseBeat <- apiRequest ctx.deployedApplication methodPost (path<>"/heartbeat") headers ""
            responseStatus responseBeat `shouldBe` status200
            canonical <- decodeResponse responseBeat :: IO Agent.AgentSession
            records <- listOutboxAfter ctx.deployedEnv.pool scope 0 100
            dispatchCanonicalOutbox ctx.deployedEnv.pool ctx.deployedWSState records
            live <- receive
            frameType live `shouldBe` Just "change"
            noEvidence live
            LBS.length (encode live) `shouldSatisfy` (< 2048)
            pure canonical
          task <- TaskDB.createTask ctx.deployedEnv.pool (CreateTask workspace.id Nothing Nothing "late association" Nothing Nothing Nothing)
          reconnect <- terminalResume ctx scope audience
          let receipt = AgentWire.TerminateSessionRequest uploaded.session.committedBytes beat.revision uploaded.session.committedBytes (Agent.sessionSha256 bytes)
          sealedResponse <- post (path<>"/terminate") (toJSON receipt)
          responseStatus sealedResponse `shouldBe` status200
          sealed <- decodeResponse sealedResponse :: IO Agent.AgentSession
          sealed.state `shouldBe` Agent.Terminated
          sealed.lastSeenAt `shouldBe` beat.lastSeenAt
          sealed.creatorActorId `shouldBe` created.creatorActorId
          responseStatus <$> post (path<>"/terminate") (toJSON receipt) `shouldReturn` status200
          let linkPath = path<>"/tasks/"<>Text.encodeUtf8 (UUID.toText task.id)
          forM_ [1,2 :: Int] $ \_ -> responseStatus <$> apiRequest ctx.deployedApplication HTTP.methodPut linkPath headers "" `shouldReturn` status200
          lateTicket <- createCanonicalTicket ctx.deployedWSState (grantPrincipal user) scope reconnect
          withDeclaredClient (client 3 lateTicket.ticket) $ \_ -> do
            seal <- receive; association <- receive; checkpoint <- receive
            map frameType [seal,association,checkpoint] `shouldBe` [Just "change",Just "change",Just "checkpoint"]
            (jsonField "event" association >>= jsonField "entity" >>= jsonField "id") `shouldBe` Just (String (UUID.toText identifier<>":"<>UUID.toText task.id))
            mapM_ noEvidence [seal,association,checkpoint]
          effects <- sessionRecords <$> listOutboxAfter ctx.deployedEnv.pool scope 0 100
          length effects `shouldBe` 5
          mapM_ (noEvidence . (.outboxEnvelope)) effects
          metadata <- apiRequest ctx.deployedApplication methodGet path headers ""
          responseStatus metadata `shouldBe` status200
          value <- decodeResponse metadata :: IO Value
          jsonField "committed_bytes" value `shouldBe` Just (toJSON sealed.committedBytes)
          noEvidence value
          responseStatus <$> apiRequest ctx.deployedApplication methodGet path (authHeaders outsideToken.rawToken) "" `shouldReturn` HTTP.status403
          let foreignPath = "/api/v1/workspaces/"<>Text.encodeUtf8 (UUID.toText other.id)<>"/sessions/"<>Text.encodeUtf8 (UUID.toText identifier)
          responseStatus <$> apiRequest ctx.deployedApplication methodGet foreignPath headers "" `shouldReturn` HTTP.status404
          revokeTicket <- canonicalTicketFor ctx (grantPrincipal user) scope audience
          initial <- newEmptyMVar; revoked <- newEmptyMVar; closed <- newEmptyMVar
          let revokeClient = asyncWithUnmask $ \unmask -> unmask $
                (WS.runClient "127.0.0.1" port ("/api/v1/ws?ticket=" <> showTicket revokeTicket.ticket) $ \conn -> do
                  WS.receiveData conn >>= putMVar initial
                  WS.receiveData conn >>= putMVar revoked
                  ended <- try @WS.ConnectionException (WS.receiveData conn :: IO Text)
                  putMVar closed (either (const True) (const False) ended)) `catch` \(_ :: WS.ConnectionException) -> pure ()
          withDeclaredClient revokeClient $ \_ -> do
            frameType <$> awaitFrame initial `shouldReturn` Just "checkpoint"
            Auth.deleteWorkspaceMembership ctx.deployedEnv.pool workspace.id user `shouldReturn` True
            revokedRecord <- latestOutbox ctx scope
            dispatchCanonicalOutbox ctx.deployedEnv.pool ctx.deployedWSState [revokedRecord]
            frameType <$> awaitFrame revoked `shouldReturn` Just "access_revoked"
            declaredPhase "session revoked socket close" (takeMVar closed) `shouldReturn` True
          responseStatus <$> apiRequest ctx.deployedApplication methodGet path headers "" `shouldReturn` HTTP.status403
          foreignProject <- Project.createProject ctx.deployedEnv.pool (CreateProject other.id Nothing "other client wake" Nothing Nothing Nothing)
          foreignRecord <- latestOutbox ctx (WorkspaceScope other.id)
          dispatchCanonicalOutbox ctx.deployedEnv.pool ctx.deployedWSState [foreignRecord]
          observed <- awaitFrame otherClient.clientObserved
          assertChangeIdentity foreignRecord foreignProject.id observed
          wait otherClient.clientWorker

  it "keeps declared HTTP and WebSocket audiences stable while replaying bot metadata and genuine system work" $
    withLocalSandboxAppContext $ \ctx -> boundedDeclaredScenario $ do
      workspace <- declaredPhase "workspace setup" (createTestWorkspace ctx.localEnv "declared-stream-audience")
      let identityA = declaredIdentity "Reviewer" "pi:gpt-6.1-sol:extra-high"
          identityB = declaredIdentity "Developer" "other:model:default"
          header identity = ("X-HMem-Bot-Identity", LBS.toStrict (encode identity))
          scope = object ["scope" .= ("workspace" :: Text), "workspace_id" .= workspace.id]
          createWith identity title = declaredPhase (T.unpack title) $ do
            response <- apiRequest ctx.localApplication methodPost "/api/v1/projects" [header identity]
              (encode (object ["workspace_id" .= workspace.id, "name" .= (title :: Text)]))
            responseStatus response `shouldBe` status200
            decodeResponse response :: IO Project
          actor identity = object ["type" .= ("service" :: Text), "id" .= localDeclaredBotId identity, "bot_identity" .= identity]
          eventActor frame = jsonField "event" frame >>= jsonField "actor"
          persistedActor identifier records =
            [jsonField "actor" record.outboxEnvelope | record <- records
            , (jsonField "entity" record.outboxEnvelope >>= jsonField "id") == Just (toJSON identifier)]
      started <- declaredPhase "snapshot setup" $ apiRequest ctx.localApplication methodPost "/api/v1/change-stream/resync" [header identityA]
        (encode (object ["scope" .= scope, "page_size" .= (100 :: Int), "start_idempotency_key" .= ("declared-stream-audience-snapshot" :: Text)]))
      responseStatus started `shouldBe` status200
      snapshot <- decodeResponse started :: IO ChangeStreamResyncResponse
      resume <- maybe (fail "expected terminal local snapshot") pure snapshot.resumeToken
      issued <- declaredPhase "ticket setup" $ apiRequest ctx.localApplication methodPost "/api/v1/change-stream/ticket" [header identityB]
        (encode (object ["scope" .= scope, "resume_token" .= resume]))
      responseStatus issued `shouldBe` status200
      ticket <- decodeResponse issued :: IO WebSocketTicketResponse
      backlog <- createWith identityA "declared backlog"
      system <- declaredPhase "system write" $ withRequestContext emptyRequestContext $
        Project.createProject ctx.localEnv.pool (CreateProject workspace.id Nothing "genuine system work" Nothing Nothing Nothing)
      backlogRecords <- declaredPhase "backlog metadata read" $ listOutboxAfter ctx.localEnv.pool (WorkspaceScope workspace.id) 0 100
      let path = "/api/v1/ws?ticket=" <> Text.encodeUtf8 ticket.ticket
          upgrade = [("Connection", "Upgrade"), ("Upgrade", "websocket"), ("Sec-WebSocket-Version", "13"), ("Sec-WebSocket-Key", "dGhlIHNhbXBsZSBub25jZQ==")]
      forM_ [[header identityA, header identityA], [("X-HMem-Bot-Identity", "null")]] $ \invalid -> do
        rejected <- declaredPhase "ticket declaration rejection" $ apiRequest ctx.localApplication methodGet path (upgrade <> invalid) ""
        responseStatus rejected `shouldBe` HTTP.status400
        body <- decodeResponse rejected :: IO Value
        jsonField "error" body `shouldBe` Just (String "invalid_bot_identity")
      testWithApplication (pure ctx.localApplication) $ \port -> do
        frames <- newChan
        let receive phase = declaredPhase phase (readChan frames >>= decodeFrame)
            start = asyncWithUnmask $ \unmask -> unmask $
              WS.runClientWith "127.0.0.1" port ("/api/v1/ws?ticket=" <> showTicket ticket.ticket)
                WS.defaultConnectionOptions [header identityA] $ \conn -> do
                  replicateM_ 5 (WS.receiveData conn >>= writeChan frames)
                  WS.sendClose conn ("declared scenario complete" :: Text)
        withDeclaredClient start $ \_client -> do
          replayed <- receive "backlog receive"
          frameType replayed `shouldBe` Just "change"
          (jsonField "event" replayed >>= jsonField "entity" >>= jsonField "id") `shouldBe` Just (toJSON backlog.id)
          eventActor replayed `shouldBe` Just (actor identityA)
          [eventActor replayed] `shouldBe` persistedActor backlog.id backlogRecords
          systemFrame <- receive "system receive"
          frameType systemFrame `shouldBe` Just "change"
          (jsonField "event" systemFrame >>= jsonField "entity" >>= jsonField "id") `shouldBe` Just (toJSON system.id)
          eventActor systemFrame `shouldBe` Just (object ["type" .= ("system" :: Text), "id" .= Null])
          [eventActor systemFrame] `shouldBe` persistedActor system.id backlogRecords
          frameType <$> receive "backlog checkpoint" `shouldReturn` Just "checkpoint"
          live <- createWith identityB "declared live"
          records <- declaredPhase "live metadata read" $ listOutboxAfter ctx.localEnv.pool (WorkspaceScope workspace.id) 0 100
          declaredPhase "live dispatch" (dispatchCanonicalOutbox ctx.localEnv.pool ctx.localWSState records)
          delivered <- receive "live receive"
          frameType delivered `shouldBe` Just "change"
          (jsonField "event" delivered >>= jsonField "entity" >>= jsonField "id") `shouldBe` Just (toJSON live.id)
          eventActor delivered `shouldBe` Just (actor identityB)
          [eventActor delivered] `shouldBe` persistedActor live.id records
          frameType <$> receive "live checkpoint" `shouldReturn` Just "checkpoint"

  it "stops an acquired dispatcher when subsequent harness setup fails" $
    withLocalSandboxAppContext $ \ctx -> do
      stopped <- newEmptyMVar
      result <- try @SomeException $
        bracket
          (startChangeStreamWorker ctx.localEnv.pool testChangeStreamConfig ctx.localWSState)
          (\worker -> timeout 2000000 (stopChangeStreamWorker worker) >>= putMVar stopped)
          (\_worker -> ioError $ userError "injected harness setup failure")
      case result of
        Left _ -> pure ()
        Right () -> expectationFailure "injected harness setup failure unexpectedly succeeded"
      timeout 2000000 (takeMVar stopped) `shouldReturn` Just (Just ())

  it "starts a bracket-acquired dispatcher unmasked and joins cancellation" $
    withLocalSandboxAppContext $ \ctx -> do
      entered <- newEmptyMVar
      released <- newIORef False
      let startup = do
            masking <- getMaskingState
            putMVar entered masking
            started <- getMonotonicTimeNSec
            let spin = do
                  release <- atomicModifyIORef' released (\value -> (value, value))
                  now <- getMonotonicTimeNSec
                  if release || now - started >= 6000000000
                    then ioError (userError "fixture startup gate finished before database work")
                    else spin
            spin
          cleanup worker = do
            atomicModifyIORef' released (const (True, ()))
            timeout 2000000 (stopChangeStreamWorker worker) `shouldReturn` Just ()
      observed <- bracket
        (startChangeStreamWorkerWithStartupHook startup ctx.localEnv.pool testChangeStreamConfig ctx.localWSState)
        cleanup $ \worker -> do
          masking <- timeout 2000000 (takeMVar entered)
          stopped <- timeout 2000000 (stopChangeStreamWorker worker)
          pure (masking, stopped)
      observed `shouldBe` (Just Unmasked, Just ())

  it "publishes dispatcher completion when its startup hook fails" $
    withLocalSandboxAppContext $ \ctx -> do
      entered <- newEmptyMVar
      let startup = putMVar entered () >> ioError (userError "fixture startup failure before SQL")
          joined worker = timeout 2000000 (stopChangeStreamWorker worker) `shouldReturn` Just ()
      bracket
        (startChangeStreamWorkerWithStartupHook startup ctx.localEnv.pool testChangeStreamConfig ctx.localWSState)
        joined $ \worker -> do
          timeout 2000000 (takeMVar entered) `shouldReturn` Just ()
          joined worker

  it "rethrows cancellation from a deterministically blocked canonical dispatch" $ do
    entered <- newEmptyMVar
    cleanup <- newEmptyMVar
    blocker <- newEmptyMVar
    worker <- async $ handleCanonicalDispatchFailure (putMVar cleanup ()) $ do
      putMVar entered ()
      takeMVar blocker
    takeMVar entered
    throwTo (asyncThreadId worker) ThreadKilled
    result <- waitCatch worker
    case result of
      Left err -> case fromException err :: Maybe AsyncException of
        Just _ -> pure ()
        Nothing -> expectationFailure "blocked canonical dispatch raised a synchronous exception"
      Right () -> expectationFailure "blocked canonical dispatch swallowed cancellation"
    tryTakeMVar cleanup `shouldReturn` Nothing

  it "never materializes global workspace-group membership rows" $
    withDeployedSandboxAppContext $ \ctx -> do
      workspace <- createTestWorkspace ctx.deployedEnv "global-snapshot-redaction"
      group <- WorkspaceGroup.createGroup ctx.deployedEnv.pool (CreateWorkspaceGroup "redacted members" Nothing)
      WorkspaceGroup.addMember ctx.deployedEnv.pool group.id workspace.id `shouldReturn` WorkspaceGroup.MemberAdded
      items <- runSession ctx.deployedEnv.pool (materializeSnapshot GlobalScope)
      (Just (String "workspace_group_membership") `elem` map (jsonField "kind") items) `shouldBe` False

  it "consumes a local canonical ticket before any legacy fallback" $
    withLocalSandboxAppContext $ \ctx -> do
      workspace <- createTestWorkspace ctx.localEnv "local-canonical-ticket"
      let scope = WorkspaceScope workspace.id
          audience = TrustedAudience "local:canonical-ticket"
          principal = Principal
            { actorType = ActorUser, actorId = "canonical-ticket"
            , actorLabel = "canonical ticket", authority = PrincipalSyntheticLocalSuperadmin }
      resume <- terminalLocalResume ctx scope audience
      issued <- createCanonicalTicket ctx.localWSState principal scope resume
      received <- newEmptyMVar
      testWithApplication (pure ctx.localApplication) $ \port -> do
        client <- async $ (WS.runClient "127.0.0.1" port ("/api/v1/ws?ticket=" <> showTicket issued.ticket) $ \conn -> do
          WS.receiveData conn >>= putMVar received
          WS.sendClose conn ("done" :: Text)) `catch` \(_ :: WS.ConnectionException) -> pure ()
        frame <- awaitFrame received
        frameType frame `shouldBe` Just "checkpoint"
        wait client

  it "publishes one committed rename to workspace then global scope and suppresses no-ops" $
    withDeployedSandboxAppContext $ \ctx -> do
      workspace <- createTestWorkspace ctx.deployedEnv "canonical-workspace-rename"
      let workspaceScope = WorkspaceScope workspace.id
          cursorFor scope = do
            existing <- listOutboxAfter ctx.deployedEnv.pool scope 0 100
            pure $ maybe 0 (.outboxCursor) (safeLast existing)
      workspaceBefore <- cursorFor workspaceScope
      globalBefore <- cursorFor GlobalScope
      renamed <- WorkspaceDB.renameWorkspace ctx.deployedEnv.pool workspace.id (UpdateWorkspace "canonical-workspace-renamed")
      renamed `shouldSatisfy` isJust
      workspaceRecords <- listOutboxAfter ctx.deployedEnv.pool workspaceScope workspaceBefore 10
      globalRecords <- listOutboxAfter ctx.deployedEnv.pool GlobalScope globalBefore 10
      length workspaceRecords `shouldBe` 1
      length globalRecords `shouldBe` 1
      let [workspaceRecord] = workspaceRecords
          [globalRecord] = globalRecords
      workspaceRecord.outboxCursor `shouldSatisfy` (< globalRecord.outboxCursor)
      let transactionId record = jsonField "transaction" record.outboxEnvelope >>= jsonField "id"
      transactionId workspaceRecord `shouldSatisfy` isJust
      transactionId workspaceRecord `shouldBe` transactionId globalRecord
      mapM_ (\record -> do
        (jsonField "entity" record.outboxEnvelope >>= jsonField "type") `shouldBe` Just (String "workspace")
        (jsonField "entity" record.outboxEnvelope >>= jsonField "id") `shouldBe` Just (String $ UUID.toText workspace.id)
        (jsonField "entity" record.outboxEnvelope >>= jsonField "action") `shouldBe` Just (String "updated")) [workspaceRecord, globalRecord]
      workspaceNoopCursor <- cursorFor workspaceScope
      globalNoopCursor <- cursorFor GlobalScope
      _ <- WorkspaceDB.renameWorkspace ctx.deployedEnv.pool workspace.id (UpdateWorkspace "canonical-workspace-renamed")
      listOutboxAfter ctx.deployedEnv.pool workspaceScope workspaceNoopCursor 10 `shouldReturn` []
      listOutboxAfter ctx.deployedEnv.pool GlobalScope globalNoopCursor 10 `shouldReturn` []
      workspaceRollbackCursor <- cursorFor workspaceScope
      globalRollbackCursor <- cursorFor GlobalScope
      rollbackResult <- DBPool.withConn ctx.deployedEnv.pool $ \connection ->
        Session.run
          (Session.sql "BEGIN" >> Session.statement workspace.id rollbackRenameStatement >> Session.sql "ROLLBACK")
          connection
      case rollbackResult of
        Left err -> expectationFailure $ "Failed to roll back workspace rename: " <> show err
        Right () -> pure ()
      listOutboxAfter ctx.deployedEnv.pool workspaceScope workspaceRollbackCursor 10 `shouldReturn` []
      listOutboxAfter ctx.deployedEnv.pool GlobalScope globalRollbackCursor 10 `shouldReturn` []

  it "replays a committed rename and converges two edit-role clients without leaking to another workspace" $
    withDeployedSandboxAppContext $ \ctx -> do
      workspace <- createTestWorkspace ctx.deployedEnv "rename-convergence"
      otherWorkspace <- createTestWorkspace ctx.deployedEnv "rename-convergence-other"
      editorOne <- createDeployedSandboxUser ctx.deployedEnv False False
      editorTwo <- createDeployedSandboxUser ctx.deployedEnv False False
      unrelated <- createDeployedSandboxUser ctx.deployedEnv False False
      let scope = WorkspaceScope workspace.id
          audience userId = AuthenticatedAudience (UUID.toText userId) userId
          grantEdit userId = Auth.upsertWorkspaceMembership ctx.deployedEnv.pool workspace.id
            (Auth.UpsertWorkspaceMembership userId Auth.WorkspaceRoleEdit) Nothing
          principal userId = Principal
            { actorType = ActorUser, actorId = UUID.toText userId, actorLabel = "rename editor"
            , authority = PrincipalGrantUser userId }
      _ <- grantEdit editorOne
      _ <- grantEdit editorTwo
      _ <- Auth.upsertWorkspaceMembership ctx.deployedEnv.pool otherWorkspace.id
        (Auth.UpsertWorkspaceMembership unrelated Auth.WorkspaceRoleEdit) Nothing
      replayResume <- terminalResume ctx scope (audience editorOne)
      firstTicket <- canonicalTicketFor ctx (principal editorOne) scope (audience editorOne)
      secondTicket <- canonicalTicketFor ctx (principal editorTwo) scope (audience editorTwo)
      unrelatedTicket <- canonicalTicketFor ctx (principal unrelated) (WorkspaceScope otherWorkspace.id) (audience unrelated)
      before <- do
        records <- listOutboxAfter ctx.deployedEnv.pool scope 0 100
        pure $ maybe 0 (.outboxCursor) (safeLast records)
      testWithApplication (pure ctx.deployedApplication) $ \port -> do
        firstClient <- startOneFrameClient port firstTicket.ticket
        secondClient <- startOneFrameClient port secondTicket.ticket
        unrelatedClient <- startOneFrameClient port unrelatedTicket.ticket
        mapM_ (awaitFrame . (.clientInitial)) [firstClient, secondClient, unrelatedClient]
        _ <- WorkspaceDB.renameWorkspace ctx.deployedEnv.pool workspace.id (UpdateWorkspace "rename converged")
        records <- listOutboxAfter ctx.deployedEnv.pool scope before 10
        record <- case records of
          [value] -> pure value
          _ -> fail "expected exactly one workspace-scoped rename event"
        replayed <- replayAndRotateResumeToken ctx.deployedEnv.pool 60 scope (audience editorOne) replayResume 10
        case replayed of
          Right ReplayPage { replayPageRecords = [replayedRecord] } ->
            replayedRecord.outboxEventId `shouldBe` record.outboxEventId
          other -> expectationFailure $ "expected the committed rename in replay: " <> show other
        dispatchCanonicalOutbox ctx.deployedEnv.pool ctx.deployedWSState [record]
        firstFrame <- awaitFrame firstClient.clientObserved
        secondFrame <- awaitFrame secondClient.clientObserved
        mapM_ (assertChangeIdentity record workspace.id) [firstFrame, secondFrame]
        threadDelay 100000
        tryTakeMVar unrelatedClient.clientObserved `shouldReturn` Nothing
        cancel unrelatedClient.clientWorker
        mapM_ (wait . (.clientWorker)) [firstClient, secondClient]

  it "replays then remains live with ordered cursor-free frames" $
    withDeployedSandboxAppContext $ \ctx -> do
      workspace <- createTestWorkspace ctx.deployedEnv "canonical-loopback"
      userId <- createDeployedSandboxUser ctx.deployedEnv False False
      _ <- Auth.upsertWorkspaceMembership ctx.deployedEnv.pool workspace.id
        (Auth.UpsertWorkspaceMembership userId Auth.WorkspaceRoleRead) Nothing
      let principal = Principal
            { actorType = ActorUser, actorId = UUID.toText userId, actorLabel = "loopback user"
            , authority = PrincipalGrantUser userId }
          scope = WorkspaceScope workspace.id
          audience = AuthenticatedAudience (UUID.toText userId) userId
      begun <- beginResync ctx.deployedEnv.pool 60 scope audience (pure [])
      begin <- either (fail . show) pure begun
      terminal <- readSnapshotPageWithTtl ctx.deployedEnv.pool 60 scope audience begin.snapshotToken 20
      resume <- case terminal of
        Right SnapshotPage { snapshotResumeToken = Just token } -> pure token
        other -> fail (show other)
      -- Prove the exact bearer/principal pairing before it crosses the
      -- loopback transport; this also yields the one-use ticket's seed.
      checked <- replayAndRotateResumeToken ctx.deployedEnv.pool 60 scope audience resume 20
      replayResume <- case checked of
        Right ReplayPage { replayPageResumeToken = token } -> pure token
        other -> fail (show other)
      issued <- createCanonicalTicket ctx.deployedWSState principal scope replayResume
      initial <- newEmptyMVar
      live <- newEmptyMVar
      testWithApplication (pure ctx.deployedApplication) $ \port -> do
        client <- async $ (WS.runClient "127.0.0.1" port ("/api/v1/ws?ticket=" <> showTicket issued.ticket) $ \conn -> do
          first <- WS.receiveData conn :: IO Text
          putMVar initial first
          second <- WS.receiveData conn :: IO Text
          putMVar live second
          WS.sendClose conn ("done" :: Text)) `catch` \(_ :: WS.ConnectionException) -> pure ()
        initialFrame <- awaitFrame initial
        frameType initialFrame `shouldBe` Just "checkpoint"
        _ <- Project.createProject ctx.deployedEnv.pool CreateProject
          { workspaceId = workspace.id, parentId = Nothing, name = "after-checkpoint"
          , description = Nothing, priority = Nothing, metadata = Nothing }
        records <- listOutboxAfter ctx.deployedEnv.pool scope 0 20
        case reverse records of
          record:_ -> dispatchCanonicalOutbox ctx.deployedEnv.pool ctx.deployedWSState [record]
          [] -> fail "project mutation did not commit an outbox record"
        changeFrame <- awaitFrame live
        frameType changeFrame `shouldBe` Just "change"
        let event = jsonField "event" changeFrame
        (event >>= jsonField "cursor") `shouldBe` Nothing
        -- The client remains connected after its checkpoint, proving live
        -- dispatch rather than a replay-only one-shot socket.
        wait client

  it "holds a paginated HTTP snapshot at H and replays a concurrent direct-core commit" $
    withDeployedSandboxAppContext $ \ctx -> do
      workspace <- createTestWorkspace ctx.deployedEnv "canonical-snapshot-h"
      userId <- createDeployedSandboxUser ctx.deployedEnv False False
      _ <- Auth.upsertWorkspaceMembership ctx.deployedEnv.pool workspace.id
        (Auth.UpsertWorkspaceMembership userId Auth.WorkspaceRoleAdmin) Nothing
      accessToken <- issueDeployedSandboxPAT ctx.deployedEnv userId "snapshot H client"
      let headers = authHeaders accessToken.rawToken
          scopeValue = object ["scope" .= ("workspace" :: Text), "workspace_id" .= workspace.id]
      preResponse <- apiRequest ctx.deployedApplication methodPost "/api/v1/projects" headers
        (encode (object ["workspace_id" .= workspace.id, "name" .= ("before snapshot" :: Text)]))
      responseStatus preResponse `shouldBe` status200
      preSnapshot <- decodeResponse preResponse :: IO Project

      firstResponse <- apiRequest ctx.deployedApplication methodPost "/api/v1/change-stream/resync" headers
        (encode (object
          [ "scope" .= scopeValue
          , "page_size" .= (1 :: Int)
          , "start_idempotency_key" .= ("snapshot-h-release-gate-key-000001" :: Text)
          ]))
      responseStatus firstResponse `shouldBe` status200
      firstPage <- decodeResponse firstResponse :: IO ChangeStreamResyncResponse
      firstPage.hasMore `shouldBe` True
      firstPageToken <- case firstPage.nextPageToken of
        Just token -> pure token
        Nothing -> expectationFailure "nonterminal snapshot page omitted continuation token" >> fail "unreachable"

      mismatchResponse <- apiRequest ctx.deployedApplication methodPost "/api/v1/change-stream/resync" headers
        (encode (object
          [ "scope" .= scopeValue
          , "page_token" .= firstPageToken
          , "page_size" .= (2 :: Int)
          ]))
      responseStatus mismatchResponse `shouldBe` HTTP.status409
      mismatchBody <- decodeResponse mismatchResponse :: IO Value
      jsonField "error" mismatchBody `shouldBe` Just (String "resync_required")
      let streamConfig = ctx.deployedConfig.changeStream
          audience = AuthenticatedAudience (UUID.toText userId) userId
      readSnapshotPageWithTtls ctx.deployedEnv.pool
          (fromIntegral streamConfig.snapshotSessionTtlSeconds)
          (fromIntegral streamConfig.resumeTokenTtlSeconds)
          (WorkspaceScope workspace.id) audience (SnapshotToken firstPageToken) 2
        `shouldReturn` Left SnapshotOutOfOrder

      concurrent <- Project.createProject ctx.deployedEnv.pool CreateProject
        { workspaceId = workspace.id, parentId = Nothing, name = "during snapshot"
        , description = Nothing, priority = Nothing, metadata = Nothing }
      (snapshotItems, resumeToken) <- collectSnapshotPages
        ctx.deployedApplication headers scopeValue firstPage
      let projectIds =
            [ projectId
            | item <- snapshotItems
            , item.kind == "project"
            , Just (String projectId) <- [jsonField "id" item.data_]
            ]
      UUID.toText preSnapshot.id `shouldSatisfy` (`elem` projectIds)
      UUID.toText concurrent.id `shouldSatisfy` (`notElem` projectIds)

      currentResponse <- apiRequest ctx.deployedApplication methodGet
        ("/api/v1/projects/" <> Text.encodeUtf8 (UUID.toText concurrent.id)) headers ""
      responseStatus currentResponse `shouldBe` status200
      current <- decodeResponse currentResponse :: IO Project
      current.id `shouldBe` concurrent.id

      ticketResponse <- apiRequest ctx.deployedApplication methodPost "/api/v1/change-stream/ticket" headers
        (encode (object ["scope" .= scopeValue, "resume_token" .= resumeToken]))
      responseStatus ticketResponse `shouldBe` status200
      ticket <- decodeResponse ticketResponse :: IO WebSocketTicketResponse
      durableRecord <- latestOutbox ctx (WorkspaceScope workspace.id)
      replayed <- newEmptyMVar
      checkpointed <- newEmptyMVar
      testWithApplication (pure ctx.deployedApplication) $ \port -> do
        client <- async $ (WS.runClient "127.0.0.1" port ("/api/v1/ws?ticket=" <> showTicket ticket.ticket) $ \conn -> do
          WS.receiveData conn >>= putMVar replayed
          WS.receiveData conn >>= putMVar checkpointed
          WS.sendClose conn ("done" :: Text)) `catch` \(_ :: WS.ConnectionException) -> pure ()
        replayFrame <- awaitFrame replayed
        frameType replayFrame `shouldBe` Just "change"
        let replayEvent = jsonField "event" replayFrame
        (replayEvent >>= jsonField "event_id") `shouldBe` Just (String $ UUID.toText durableRecord.outboxEventId)
        (replayEvent >>= jsonField "entity" >>= jsonField "id") `shouldBe` Just (String $ UUID.toText concurrent.id)
        (replayEvent >>= jsonField "transaction" >>= jsonField "cause") `shouldBe` Just (String "core")
        (replayEvent >>= jsonField "cursor") `shouldBe` Nothing
        frameType <$> awaitFrame checkpointed `shouldReturn` Just "checkpoint"
        wait client

  it "delivers only to the eligible workspace and global audiences without hidden cadence frames" $
    withDeployedSandboxAppContext $ \ctx -> do
      workspaceA <- createTestWorkspace ctx.deployedEnv "canonical-audience-a"
      workspaceB <- createTestWorkspace ctx.deployedEnv "canonical-audience-b"
      memberId <- createDeployedSandboxUser ctx.deployedEnv False False
      adminId <- createDeployedSandboxUser ctx.deployedEnv False False
      unrelatedId <- createDeployedSandboxUser ctx.deployedEnv False False
      superadminId <- createDeployedSandboxUser ctx.deployedEnv False True
      _ <- Auth.upsertWorkspaceMembership ctx.deployedEnv.pool workspaceA.id
        (Auth.UpsertWorkspaceMembership memberId Auth.WorkspaceRoleRead) Nothing
      _ <- Auth.upsertWorkspaceMembership ctx.deployedEnv.pool workspaceA.id
        (Auth.UpsertWorkspaceMembership adminId Auth.WorkspaceRoleAdmin) Nothing
      _ <- Auth.upsertWorkspaceMembership ctx.deployedEnv.pool workspaceB.id
        (Auth.UpsertWorkspaceMembership unrelatedId Auth.WorkspaceRoleRead) Nothing
      let workspaceAudience userId = AuthenticatedAudience (UUID.toText userId) userId
      beginResync ctx.deployedEnv.pool 60 GlobalScope (workspaceAudience memberId) (pure [])
        `shouldReturn` Left ResyncUnauthorized
      memberTicket <- canonicalTicketFor ctx (grantPrincipal memberId) (WorkspaceScope workspaceA.id) (workspaceAudience memberId)
      adminTicket <- canonicalTicketFor ctx (grantPrincipal adminId) (WorkspaceScope workspaceA.id) (workspaceAudience adminId)
      unrelatedTicket <- canonicalTicketFor ctx (grantPrincipal unrelatedId) (WorkspaceScope workspaceB.id) (workspaceAudience unrelatedId)
      globalTicket <- canonicalTicketFor ctx (grantPrincipal superadminId) GlobalScope (workspaceAudience superadminId)
      testWithApplication (pure ctx.deployedApplication) $ \port -> do
        memberClient <- startOneFrameClient port memberTicket.ticket
        adminClient <- startOneFrameClient port adminTicket.ticket
        unrelatedClient <- startOneFrameClient port unrelatedTicket.ticket
        globalClient <- startOneFrameClient port globalTicket.ticket
        mapM_ (awaitFrame . (.clientInitial)) [memberClient, adminClient, unrelatedClient, globalClient]

        projectA <- Project.createProject ctx.deployedEnv.pool CreateProject
          { workspaceId = workspaceA.id, parentId = Nothing, name = "audience A"
          , description = Nothing, priority = Nothing, metadata = Nothing }
        recordA <- latestOutbox ctx (WorkspaceScope workspaceA.id)
        dispatchCanonicalOutbox ctx.deployedEnv.pool ctx.deployedWSState [recordA]
        memberFrame <- awaitFrame memberClient.clientObserved
        adminFrame <- awaitFrame adminClient.clientObserved
        mapM_ (assertChangeIdentity recordA projectA.id) [memberFrame, adminFrame]

        projectB <- Project.createProject ctx.deployedEnv.pool CreateProject
          { workspaceId = workspaceB.id, parentId = Nothing, name = "audience B"
          , description = Nothing, priority = Nothing, metadata = Nothing }
        recordB <- latestOutbox ctx (WorkspaceScope workspaceB.id)
        dispatchCanonicalOutbox ctx.deployedEnv.pool ctx.deployedWSState [recordB]
        unrelatedFrame <- awaitFrame unrelatedClient.clientObserved
        assertChangeIdentity recordB projectB.id unrelatedFrame

        group <- WorkspaceGroup.createGroup ctx.deployedEnv.pool (CreateWorkspaceGroup "global audience" Nothing)
        globalRecord <- latestOutbox ctx GlobalScope
        dispatchCanonicalOutbox ctx.deployedEnv.pool ctx.deployedWSState [globalRecord]
        globalFrame <- awaitFrame globalClient.clientObserved
        frameType globalFrame `shouldBe` Just "change"
        (jsonField "event" globalFrame >>= jsonField "entity" >>= jsonField "type")
          `shouldBe` Just (String "workspace_group")
        (jsonField "event" globalFrame >>= jsonField "entity" >>= jsonField "id")
          `shouldBe` Just (String $ UUID.toText group.id)
        mapM_ (wait . (.clientWorker)) [memberClient, adminClient, unrelatedClient, globalClient]

  it "suppresses hidden-only membership activity without a frame or checkpoint" $
    withDeployedSandboxAppContext $ \ctx -> do
      workspace <- createTestWorkspace ctx.deployedEnv "canonical-hidden"
      subscriberId <- createDeployedSandboxUser ctx.deployedEnv False False
      hiddenUserId <- createDeployedSandboxUser ctx.deployedEnv False False
      _ <- Auth.upsertWorkspaceMembership ctx.deployedEnv.pool workspace.id
        (Auth.UpsertWorkspaceMembership subscriberId Auth.WorkspaceRoleRead) Nothing
      let principal = grantPrincipal subscriberId
          scope = WorkspaceScope workspace.id
          audience = AuthenticatedAudience (UUID.toText subscriberId) subscriberId
      resume <- terminalResume ctx scope audience
      issued <- createCanonicalTicket ctx.deployedWSState principal scope resume
      initial <- newEmptyMVar
      observed <- newEmptyMVar
      testWithApplication (pure ctx.deployedApplication) $ \port -> do
        client <- async $ (WS.runClient "127.0.0.1" port ("/api/v1/ws?ticket=" <> showTicket issued.ticket) $ \conn -> do
          WS.receiveData conn >>= putMVar initial
          WS.receiveData conn >>= putMVar observed
          WS.sendClose conn ("done" :: Text)) `catch` \(_ :: WS.ConnectionException) -> pure ()
        initialFrame <- awaitFrame initial
        frameType initialFrame `shouldBe` Just "checkpoint"
        _ <- Auth.upsertWorkspaceMembership ctx.deployedEnv.pool workspace.id
          (Auth.UpsertWorkspaceMembership hiddenUserId Auth.WorkspaceRoleRead) Nothing
        record <- latestOutbox ctx scope
        dispatchCanonicalOutbox ctx.deployedEnv.pool ctx.deployedWSState [record]
        -- The next observable frame is the later visible mutation.  A frame
        -- from the hidden membership (including a checkpoint) would arrive
        -- first and fail this assertion.
        _ <- Project.createProject ctx.deployedEnv.pool CreateProject
          { workspaceId = workspace.id, parentId = Nothing, name = "visible-after-hidden"
          , description = Nothing, priority = Nothing, metadata = Nothing }
        visibleRecord <- latestOutbox ctx scope
        dispatchCanonicalOutbox ctx.deployedEnv.pool ctx.deployedWSState [visibleRecord]
        visibleFrame <- awaitFrame observed
        frameType visibleFrame `shouldBe` Just "change"
        wait client

  it "sends a targeted revoke and closes the canonical workspace scope" $
    withDeployedSandboxAppContext $ \ctx -> do
      workspace <- createTestWorkspace ctx.deployedEnv "canonical-revoke"
      userId <- createDeployedSandboxUser ctx.deployedEnv False False
      _ <- Auth.upsertWorkspaceMembership ctx.deployedEnv.pool workspace.id
        (Auth.UpsertWorkspaceMembership userId Auth.WorkspaceRoleRead) Nothing
      let principal = grantPrincipal userId
          scope = WorkspaceScope workspace.id
          audience = AuthenticatedAudience (UUID.toText userId) userId
      resume <- terminalResume ctx scope audience
      issued <- createCanonicalTicket ctx.deployedWSState principal scope resume
      initial <- newEmptyMVar
      permitRead <- newEmptyMVar
      revoked <- newEmptyMVar
      testWithApplication (pure ctx.deployedApplication) $ \port -> do
        client <- async $ (WS.runClient "127.0.0.1" port ("/api/v1/ws?ticket=" <> showTicket issued.ticket) $ \conn -> do
          WS.receiveData conn >>= putMVar initial
          takeMVar permitRead
          WS.receiveData conn >>= putMVar revoked
          _ <- (WS.receiveData conn :: IO Text) `catch` \(_ :: WS.ConnectionException) -> pure ""
          pure ()) `catch` \(_ :: WS.ConnectionException) -> pure ()
        _ <- awaitFrame initial
        Auth.deleteWorkspaceMembership ctx.deployedEnv.pool workspace.id userId `shouldReturn` True
        record <- latestOutbox ctx scope
        dispatchCanonicalOutbox ctx.deployedEnv.pool ctx.deployedWSState [record]
        putMVar permitRead ()
        revokeFrame <- awaitFrame revoked
        frameType revokeFrame `shouldBe` Just "access_revoked"
        wait client

  it "expires a connected canonical bearer on its fixed deadline despite hidden activity" $
    withDeployedSandboxAppContext $ \ctx -> do
      workspace <- createTestWorkspace ctx.deployedEnv "canonical-fixed-expiry"
      subscriberId <- createDeployedSandboxUser ctx.deployedEnv False False
      hiddenUserId <- createDeployedSandboxUser ctx.deployedEnv False False
      _ <- Auth.upsertWorkspaceMembership ctx.deployedEnv.pool workspace.id
        (Auth.UpsertWorkspaceMembership subscriberId Auth.WorkspaceRoleRead) Nothing
      let principal = grantPrincipal subscriberId
          scope = WorkspaceScope workspace.id
          audience = AuthenticatedAudience (UUID.toText subscriberId) subscriberId
      testWithApplication (pure ctx.deployedApplication) $ \port -> do
        resume <- terminalResume ctx scope audience
        issued <- createCanonicalTicketWithTtl ctx.deployedWSState 0.3 principal scope resume
        bracket (startOneFrameClient port issued.ticket) (\client -> joinClient "expiry client" client.clientWorker) $ \client -> do
          frameType <$> awaitFrame client.clientInitial `shouldReturn` Just "checkpoint"
          _ <- Auth.upsertWorkspaceMembership ctx.deployedEnv.pool workspace.id
            (Auth.UpsertWorkspaceMembership hiddenUserId Auth.WorkspaceRoleRead) Nothing
          hiddenRecord <- latestOutbox ctx scope
          dispatchCanonicalOutbox ctx.deployedEnv.pool ctx.deployedWSState [hiddenRecord]
          frameType <$> awaitFrame client.clientObserved `shouldReturn` Just "resync_required"
          getCurrentTime >>= (`shouldSatisfy` (>= issued.expiresAt))
          timeout 2000000 (wait client.clientWorker) `shouldReturn` Just ()

  it "keeps an aged durable reconnect on its actual expiry despite hidden activity" $
    withDeployedSandboxAppContext $ \ctx -> do
      workspace <- createTestWorkspace ctx.deployedEnv "canonical-aged-reconnect-expiry"
      subscriberId <- createDeployedSandboxUser ctx.deployedEnv False False
      hiddenUserId <- createDeployedSandboxUser ctx.deployedEnv False False
      _ <- Auth.upsertWorkspaceMembership ctx.deployedEnv.pool workspace.id
        (Auth.UpsertWorkspaceMembership subscriberId Auth.WorkspaceRoleRead) Nothing
      let principal = grantPrincipal subscriberId
          scope = WorkspaceScope workspace.id
          audience = AuthenticatedAudience (UUID.toText subscriberId) subscriberId
      testWithApplication (pure ctx.deployedApplication) $ \port -> do
        -- Start listening before the original durable lifetime begins. Real
        -- validation ages the bearer; ticket exchange preserves its expiry.
        resume <- terminalResumeWithTtl ctx 2.0 scope audience
        validated <- validateCanonicalResumeToken ctx.deployedEnv.pool scope audience resume
        expiresAt <- either (fail . show) pure validated
        issued <- createCanonicalTicketWithExpiry ctx.deployedWSState expiresAt principal scope resume
        issued.expiresAt `shouldBe` expiresAt
        bracket (startOneFrameClient port issued.ticket)
          (\client -> joinClient "aged expiry client" client.clientWorker) $ \client -> do
            frameType <$> awaitFrame client.clientInitial `shouldReturn` Just "checkpoint"
            _ <- Auth.upsertWorkspaceMembership ctx.deployedEnv.pool workspace.id
              (Auth.UpsertWorkspaceMembership hiddenUserId Auth.WorkspaceRoleRead) Nothing
            hiddenRecord <- latestOutbox ctx scope
            dispatchCanonicalOutbox ctx.deployedEnv.pool ctx.deployedWSState [hiddenRecord]
            frameType <$> awaitFrame client.clientObserved `shouldReturn` Just "resync_required"
            getCurrentTime >>= (`shouldSatisfy` (>= expiresAt))
            timeout 2000000 (wait client.clientWorker) `shouldReturn` Just ()

  it "announces a targeted grant for a separately scoped workspace" $
    withDeployedSandboxAppContext $ \ctx -> do
      currentWorkspace <- createTestWorkspace ctx.deployedEnv "canonical-grant-current"
      grantedWorkspace <- createTestWorkspace ctx.deployedEnv "canonical-grant-target"
      userId <- createDeployedSandboxUser ctx.deployedEnv False False
      _ <- Auth.upsertWorkspaceMembership ctx.deployedEnv.pool currentWorkspace.id
        (Auth.UpsertWorkspaceMembership userId Auth.WorkspaceRoleRead) Nothing
      let principal = grantPrincipal userId
          scope = WorkspaceScope currentWorkspace.id
          audience = AuthenticatedAudience (UUID.toText userId) userId
      resume <- terminalResume ctx scope audience
      issued <- createCanonicalTicket ctx.deployedWSState principal scope resume
      initial <- newEmptyMVar
      permitGrant <- newEmptyMVar
      granted <- newEmptyMVar
      testWithApplication (pure ctx.deployedApplication) $ \port -> do
        client <- async $ (WS.runClient "127.0.0.1" port ("/api/v1/ws?ticket=" <> showTicket issued.ticket) $ \conn -> do
          WS.receiveData conn >>= putMVar initial
          takeMVar permitGrant
          WS.receiveData conn >>= putMVar granted
          WS.sendClose conn ("done" :: Text)) `catch` \(_ :: WS.ConnectionException) -> pure ()
        _ <- awaitFrame initial
        _ <- Auth.upsertWorkspaceMembership ctx.deployedEnv.pool grantedWorkspace.id
          (Auth.UpsertWorkspaceMembership userId Auth.WorkspaceRoleRead) Nothing
        grantRecord <- latestOutbox ctx (WorkspaceScope grantedWorkspace.id)
        dispatchCanonicalOutbox ctx.deployedEnv.pool ctx.deployedWSState [grantRecord]
        putMVar permitGrant ()
        grantFrame <- awaitFrame granted
        frameType grantFrame `shouldBe` Just "access_granted"
        jsonField "workspace_id" grantFrame `shouldBe` Just (String $ UUID.toText grantedWorkspace.id)
        wait client

  it "sends a cross-scope revoke while the retained scope remains live" $
    withDeployedSandboxAppContext $ \ctx -> do
      revokedWorkspace <- createTestWorkspace ctx.deployedEnv "canonical-cross-scope-revoke-target"
      retainedWorkspace <- createTestWorkspace ctx.deployedEnv "canonical-cross-scope-revoke-retained"
      userId <- createDeployedSandboxUser ctx.deployedEnv False False
      _ <- Auth.upsertWorkspaceMembership ctx.deployedEnv.pool revokedWorkspace.id
        (Auth.UpsertWorkspaceMembership userId Auth.WorkspaceRoleRead) Nothing
      _ <- Auth.upsertWorkspaceMembership ctx.deployedEnv.pool retainedWorkspace.id
        (Auth.UpsertWorkspaceMembership userId Auth.WorkspaceRoleRead) Nothing
      let principal = grantPrincipal userId
          revokedScope = WorkspaceScope revokedWorkspace.id
          retainedScope = WorkspaceScope retainedWorkspace.id
          audience = AuthenticatedAudience (UUID.toText userId) userId
      retainedResume <- terminalResume ctx retainedScope audience
      retainedTicket <- createCanonicalTicket ctx.deployedWSState principal retainedScope retainedResume
      retainedInitial <- newEmptyMVar
      retainedRevoke <- newEmptyMVar
      retainedLive <- newEmptyMVar
      testWithApplication (pure ctx.deployedApplication) $ \port -> do
        retainedClient <- async $ (WS.runClient "127.0.0.1" port ("/api/v1/ws?ticket=" <> showTicket retainedTicket.ticket) $ \conn -> do
          WS.receiveData conn >>= putMVar retainedInitial
          WS.receiveData conn >>= putMVar retainedRevoke
          WS.receiveData conn >>= putMVar retainedLive
          WS.sendClose conn ("done" :: Text)) `catch` \(_ :: SomeException) -> pure ()
        _ <- awaitFrame retainedInitial
        Auth.deleteWorkspaceMembership ctx.deployedEnv.pool revokedWorkspace.id userId `shouldReturn` True
        revokeRecord <- latestOutbox ctx revokedScope
        dispatchCanonicalOutbox ctx.deployedEnv.pool ctx.deployedWSState [revokeRecord]
        retainedControl <- awaitFrame retainedRevoke
        frameType retainedControl `shouldBe` Just "access_revoked"
        jsonField "workspace_id" retainedControl `shouldBe` Just (String $ UUID.toText revokedWorkspace.id)
        _ <- Project.createProject ctx.deployedEnv.pool CreateProject
          { workspaceId = retainedWorkspace.id, parentId = Nothing, name = "other-scope-remains-live"
          , description = Nothing, priority = Nothing, metadata = Nothing }
        retainedRecord <- latestOutbox ctx retainedScope
        dispatchCanonicalOutbox ctx.deployedEnv.pool ctx.deployedWSState [retainedRecord]
        frameType <$> awaitFrame retainedLive `shouldReturn` Just "change"
        wait retainedClient

  it "consumes each canonical ticket once and reconnects from its replacement token" $
    withDeployedSandboxAppContext $ \ctx -> do
      workspace <- createTestWorkspace ctx.deployedEnv "canonical-reconnect"
      userId <- createDeployedSandboxUser ctx.deployedEnv False False
      _ <- Auth.upsertWorkspaceMembership ctx.deployedEnv.pool workspace.id
        (Auth.UpsertWorkspaceMembership userId Auth.WorkspaceRoleRead) Nothing
      let principal = grantPrincipal userId
          scope = WorkspaceScope workspace.id
          audience = AuthenticatedAudience (UUID.toText userId) userId
      resume <- terminalResume ctx scope audience
      firstTicket <- createCanonicalTicket ctx.deployedWSState principal scope resume
      firstFrame <- newEmptyMVar
      testWithApplication (pure ctx.deployedApplication) $ \port -> do
        first <- async $ (WS.runClient "127.0.0.1" port ("/api/v1/ws?ticket=" <> showTicket firstTicket.ticket) $ \conn -> do
          WS.receiveData conn >>= putMVar firstFrame
          WS.sendClose conn ("done" :: Text)) `catch` \(_ :: WS.ConnectionException) -> pure ()
        checkpoint <- awaitFrame firstFrame
        frameType checkpoint `shouldBe` Just "checkpoint"
        replacement <- checkpointResume checkpoint
        wait first
        -- The old ticket was consumed by the successful upgrade and cannot
        -- open a second delivery identity.
        admitted <- newEmptyMVar
        secondAttempt <- timeout 2000000 $
          (WS.runClient "127.0.0.1" port ("/api/v1/ws?ticket=" <> showTicket firstTicket.ticket) (\_ -> putMVar admitted ())
            `catch` \(_ :: SomeException) -> pure ())
        secondAttempt `shouldSatisfy` isJust
        tryTakeMVar admitted `shouldReturn` Nothing
        _ <- Project.createProject ctx.deployedEnv.pool CreateProject
          { workspaceId = workspace.id, parentId = Nothing, name = "after-reconnect"
          , description = Nothing, priority = Nothing, metadata = Nothing }
        reconnectTicket <- createCanonicalTicket ctx.deployedWSState principal scope replacement
        change <- newEmptyMVar
        terminal <- newEmptyMVar
        reconnect <- async $ (WS.runClient "127.0.0.1" port ("/api/v1/ws?ticket=" <> showTicket reconnectTicket.ticket) $ \conn -> do
          WS.receiveData conn >>= putMVar change
          WS.receiveData conn >>= putMVar terminal
          WS.sendClose conn ("done" :: Text)) `catch` \(_ :: WS.ConnectionException) -> pure ()
        changeFrame <- awaitFrame change
        frameType changeFrame `shouldBe` Just "change"
        (jsonField "event" changeFrame >>= jsonField "event_id") `shouldSatisfy` maybe False isText
        terminalFrame <- awaitFrame terminal
        frameType terminalFrame `shouldBe` Just "checkpoint"
        wait reconnect

  it "replays retained commits after dispatcher restart and cancels workers cleanly" $
    withDeployedSandboxAppContext $ \ctx -> do
      workspace <- createTestWorkspace ctx.deployedEnv "canonical-worker-restart"
      userId <- createDeployedSandboxUser ctx.deployedEnv False False
      _ <- Auth.upsertWorkspaceMembership ctx.deployedEnv.pool workspace.id
        (Auth.UpsertWorkspaceMembership userId Auth.WorkspaceRoleRead) Nothing
      let principal = grantPrincipal userId
          scope = WorkspaceScope workspace.id
          audience = AuthenticatedAudience (UUID.toText userId) userId
      resume <- terminalResume ctx scope audience
      ticket <- createCanonicalTicket ctx.deployedWSState principal scope resume
      initial <- newEmptyMVar
      liveFrames <- newChan
      expectedReplay <- newIORef Nothing
      testWithApplication (pure ctx.deployedApplication) $ \port -> do
        bracket (asyncWithUnmask $ \unmask -> unmask $ (WS.runClient "127.0.0.1" port ("/api/v1/ws?ticket=" <> showTicket ticket.ticket) $ \conn -> do
          WS.receiveData conn >>= putMVar initial
          let receiveReplay = do
                frame <- WS.receiveData conn :: IO Text
                expected <- readIORef expectedReplay
                let entityId = (decode (LBS.fromStrict (Text.encodeUtf8 frame)) :: Maybe Value)
                      >>= jsonField "event" >>= jsonField "entity" >>= jsonField "id"
                if expected /= Nothing && entityId == expected
                  then WS.sendClose conn ("replay received" :: Text) >> writeChan liveFrames frame
                  else writeChan liveFrames frame >> receiveReplay
          receiveReplay) `catch` \(_ :: WS.ConnectionException) -> pure ())
          (joinClient "restart client") $ \_client -> do
            _ <- awaitFrame initial
            withDispatcherFixture ctx "first dispatcher" $ \fixture -> do
              beforeRestart <- Project.createProject ctx.deployedEnv.pool CreateProject
                { workspaceId = workspace.id, parentId = Nothing, name = "before-restart"
                , description = Nothing, priority = Nothing, metadata = Nothing }
              awaitChangeChan liveFrames beforeRestart.id
              stopDispatcherFixture ctx "first explicit stop" fixture
              duringRestart <- Project.createProject ctx.deployedEnv.pool CreateProject
                { workspaceId = workspace.id, parentId = Nothing, name = "during-restart"
                , description = Nothing, priority = Nothing, metadata = Nothing }
              atomicModifyIORef' expectedReplay (const (Just (String (UUID.toText duringRestart.id)), ()))
              withDispatcherFixture ctx "replacement dispatcher" $ \_ -> do
                awaitChangeChan liveFrames duringRestart.id
      withDispatcherFixture ctx "fresh dispatcher" $ \fixture ->
        stopDispatcherFixture ctx "fresh explicit stop" fixture

-- Joined cleanup is bounded even when a fixture assertion throws.
newtype DispatcherFixture = DispatcherFixture ChangeStreamWorker

withDispatcherFixture :: DeployedSandboxApp -> String -> (DispatcherFixture -> IO value) -> IO value
withDispatcherFixture ctx phase = bracket acquire (stopDispatcherFixture ctx (phase <> " cleanup"))
  where
    acquire = DispatcherFixture <$> startChangeStreamWorker ctx.deployedEnv.pool testChangeStreamConfig ctx.deployedWSState

stopDispatcherFixture :: DeployedSandboxApp -> String -> DispatcherFixture -> IO ()
stopDispatcherFixture _ctx phase (DispatcherFixture worker) = do
  stopped <- timeout 2000000 (stopChangeStreamWorker worker)
  case stopped of
    Just () -> pure ()
    Nothing -> expectationFailure (phase <> " did not join within two seconds")

joinClient :: String -> Async value -> IO ()
joinClient phase client = do
  joined <- timeout 2000000 (cancel client)
  case joined of
    Just () -> pure ()
    Nothing -> expectationFailure (phase <> " did not join within two seconds")

grantPrincipal :: UUID.UUID -> Principal
grantPrincipal userId = Principal
  { actorType = ActorUser, actorId = UUID.toText userId, actorLabel = "loopback user"
  , authority = PrincipalGrantUser userId }

terminalResume :: DeployedSandboxApp -> ChangeScope -> ChangeAudience -> IO ResumeToken
terminalResume ctx scope audience = do
  begun <- beginResync ctx.deployedEnv.pool 60 scope audience (pure [])
  begin <- either (fail . show) pure begun
  terminal <- readSnapshotPageWithTtl ctx.deployedEnv.pool 60 scope audience begin.snapshotToken 20
  resume <- case terminal of
    Right SnapshotPage { snapshotResumeToken = Just token } -> pure token
    other -> fail (show other)
  replayed <- replayAndRotateResumeToken ctx.deployedEnv.pool 60 scope audience resume 20
  case replayed of
    Right ReplayPage { replayPageResumeToken = token } -> pure token
    other -> fail (show other)

terminalResumeWithTtl :: DeployedSandboxApp -> Double -> ChangeScope -> ChangeAudience -> IO ResumeToken
terminalResumeWithTtl ctx ttl scope audience = do
  begun <- beginResync ctx.deployedEnv.pool 60 scope audience (pure [])
  begin <- either (fail . show) pure begun
  terminal <- readSnapshotPageWithTtls ctx.deployedEnv.pool 60 (realToFrac ttl) scope audience begin.snapshotToken 20
  case terminal of
    Right SnapshotPage { snapshotResumeToken = Just token } -> pure token
    other -> fail (show other)

terminalLocalResume :: LocalSandboxApp -> ChangeScope -> ChangeAudience -> IO ResumeToken
terminalLocalResume ctx scope audience = do
  begun <- beginResync ctx.localEnv.pool 60 scope audience (pure [])
  begin <- either (fail . show) pure begun
  terminal <- readSnapshotPageWithTtl ctx.localEnv.pool 60 scope audience begin.snapshotToken 20
  case terminal of
    Right SnapshotPage { snapshotResumeToken = Just token } -> pure token
    other -> fail (show other)

latestOutbox :: DeployedSandboxApp -> ChangeScope -> IO OutboxRecord
latestOutbox ctx scope = do
  records <- listOutboxAfter ctx.deployedEnv.pool scope 0 100
  case reverse records of
    record:_ -> pure record
    [] -> fail "expected a committed outbox record"

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast xs = Just (last xs)

rollbackRenameStatement :: Statement.Statement UUID.UUID ()
rollbackRenameStatement = Statement.Statement
  "UPDATE workspaces SET name = 'rolled-back-workspace-rename' WHERE id = $1"
  (Enc.param (Enc.nonNullable Enc.uuid)) Dec.noResult True

canonicalTicketFor :: DeployedSandboxApp -> Principal -> ChangeScope -> ChangeAudience -> IO WebSocketTicketResponse
canonicalTicketFor ctx principal scope audience = do
  resume <- terminalResume ctx scope audience
  createCanonicalTicket ctx.deployedWSState principal scope resume

data OneFrameClient = OneFrameClient
  { clientWorker :: !(Async ())
  , clientInitial :: !(MVar Text)
  , clientObserved :: !(MVar Text)
  }

startOneFrameClient :: Int -> Text -> IO OneFrameClient
startOneFrameClient port ticket = do
  initial <- newEmptyMVar
  observed <- newEmptyMVar
  worker <- asyncWithUnmask $ \unmask -> unmask $ (WS.runClient "127.0.0.1" port ("/api/v1/ws?ticket=" <> showTicket ticket) $ \conn -> do
    WS.receiveData conn >>= putMVar initial
    WS.receiveData conn >>= putMVar observed
    WS.sendClose conn ("done" :: Text)) `catch` \(_ :: WS.ConnectionException) -> pure ()
  pure OneFrameClient { clientWorker = worker, clientInitial = initial, clientObserved = observed }

assertChangeIdentity :: OutboxRecord -> UUID.UUID -> Value -> Expectation
assertChangeIdentity record entityId frame = do
  frameType frame `shouldBe` Just "change"
  (jsonField "event" frame >>= jsonField "event_id")
    `shouldBe` Just (String $ UUID.toText record.outboxEventId)
  (jsonField "event" frame >>= jsonField "entity" >>= jsonField "id")
    `shouldBe` Just (String $ UUID.toText entityId)

authHeaders :: Text -> [Header]
authHeaders token =
  [("Authorization", "Bearer " <> Text.encodeUtf8 token)
  , ("X-HMem-Bot-Identity", "{\"role\":\"Stream Test Bot\",\"model-id\":\"test:fixture:default\"}")]

declaredIdentity :: Text -> Text -> BotIdentity
declaredIdentity role model = either (error . T.unpack) id (mkBotIdentity role model)

-- These guards belong only to this integration scenario. Preserve the original
-- assertion/timeout exception if bounded client cleanup also fails.
boundedDeclaredScenario :: IO a -> IO a
boundedDeclaredScenario action = timeout 20000000 action >>=
  maybe (fail "declared stream overall scenario deadline (setup/ticket/backlog/system/live)") pure

declaredPhase :: String -> IO a -> IO a
declaredPhase phase action = timeout 5000000 action >>=
  maybe (fail ("declared stream " <> phase <> " deadline")) pure

withDeclaredClient :: IO (Async a) -> (Async a -> IO b) -> IO b
withDeclaredClient acquire use = mask $ \restore -> do
  client <- acquire
  primary <- try @SomeException (restore (use client))
  cleanup <- try @SomeException $ timeout 1000000 $ case primary of
    Right _ -> fmap (fmap (const ())) (waitCatch client)
    Left _ -> cancel client >> pure (Right ())
  let diagnostic disposition = Just ("declared stream cleanup failure: ownedThreadId=" <> show (asyncThreadId client)
          <> "; " <> disposition <> "; Warp fixture teardown follows")
      cleanupProblem = case cleanup of
        Right (Just (Right ())) -> Nothing
        Right (Just (Left childError)) -> diagnostic ("client joined with exception: " <> show childError)
        Right Nothing -> diagnostic "client unjoined after deadline"
        Left cleanupError -> diagnostic ("join disposition unknown; cleanup exception: " <> show cleanupError)
  case primary of
    Left original -> do
      maybe (pure ()) (hPutStrLn stderr) cleanupProblem
      throwIO original
    Right result -> maybe (pure result) fail cleanupProblem

apiRequest :: Application -> BS.ByteString -> BS.ByteString -> [Header] -> LBS.ByteString -> IO WaiTest.SResponse
apiRequest app method path headers body = WaiTest.runSession (WaiTest.srequest (WaiTest.SRequest request body)) app
  where
    (rawPath, rawQuery) = BS.break (== 63) path
    request = defaultRequest
      { Wai.requestMethod = method
      , Wai.rawPathInfo = rawPath
      , Wai.rawQueryString = rawQuery
      , Wai.queryString = parseQuery rawQuery
      , Wai.pathInfo = filter (not . T.null) (T.splitOn "/" (Text.decodeUtf8 rawPath))
      , Wai.requestHeaders = ("Content-Type", "application/json") : headers
      }

responseStatus :: WaiTest.SResponse -> HTTP.Status
responseStatus = WaiTest.simpleStatus

decodeResponse :: FromJSON a => WaiTest.SResponse -> IO a
decodeResponse response =
  case decode (WaiTest.simpleBody response) of
    Just value -> pure value
    Nothing -> expectationFailure "canonical API returned invalid JSON" >> fail "unreachable"

collectSnapshotPages :: Application -> [Header] -> Value -> ChangeStreamResyncResponse -> IO ([ChangeStreamSnapshotItem], Text)
collectSnapshotPages app headers scopeValue = go []
  where
    go accumulated page =
      let items = accumulated <> page.items
      in case page.nextPageToken of
        Just pageToken -> do
          response <- apiRequest app methodPost "/api/v1/change-stream/resync" headers
            (encode (object ["scope" .= scopeValue, "page_token" .= pageToken]))
          responseStatus response `shouldBe` status200
          next <- decodeResponse response
          go items next
        Nothing -> case page.resumeToken of
          Just resume -> pure (items, resume)
          Nothing -> expectationFailure "terminal snapshot page omitted resume token" >> fail "unreachable"

checkpointResume :: Value -> IO ResumeToken
checkpointResume value = case jsonField "resume_token" value of
  Just (String token) -> pure (ResumeToken token)
  _ -> fail "canonical checkpoint omitted its opaque resume token"

isText :: Value -> Bool
isText (String _) = True
isText _ = False

awaitChangeChan :: Chan Text -> UUID.UUID -> IO ()
awaitChangeChan frames projectId = timeout 5000000 consume >>= \case
  Just () -> pure ()
  Nothing -> fail "timed out waiting for dispatcher change delivery"
  where
    consume = do
      frame <- readChan frames >>= decodeFrame
      case frameType frame of
        Just "change"
          | (jsonField "event" frame >>= jsonField "entity" >>= jsonField "id") == Just (String (UUID.toText projectId)) -> pure ()
          | otherwise -> consume
        Just "checkpoint" -> consume
        other -> fail $ "expected canonical change frame, got " <> show other

decodeFrame :: Text -> IO Value
decodeFrame text = case decode (LBS.fromStrict $ Text.encodeUtf8 text) of
  Just frame -> pure frame
  Nothing -> fail "received a non-JSON canonical websocket frame"

testChangeStreamConfig :: Config.ChangeStreamConfig
testChangeStreamConfig = Config.ChangeStreamConfig
  { Config.retentionSeconds = 604800
  , Config.resumeTokenTtlSeconds = 86400
  , Config.snapshotSessionTtlSeconds = 300
  }

awaitFrame :: MVar Text -> IO Value
awaitFrame slot = do
  received <- timeout 5000000 (takeMVar slot)
  case received >>= decode . LBS.fromStrict . Text.encodeUtf8 of
    Just frame -> pure frame
    Nothing -> fail "timed out waiting for canonical websocket frame"

showTicket :: Text -> String
showTicket = T.unpack

frameType :: Value -> Maybe Text
frameType value = jsonField "type" value >>= \case String result -> Just result; _ -> Nothing

jsonField :: Text -> Value -> Maybe Value
jsonField name = \case
  Object fields -> KeyMap.lookup (Key.fromText name) fields
  _ -> Nothing
