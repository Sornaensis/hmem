module HMem.Server.ChangeStreamSpec (spec) where

import Control.Concurrent (Chan, MVar, newChan, newEmptyMVar, putMVar, readChan, takeMVar, threadDelay, throwTo, tryTakeMVar, writeChan)
import Control.Concurrent.Async (async, asyncThreadId, cancel, wait, waitCatch)
import Control.Exception (AsyncException(..), SomeException, bracket, catch, fromException)
import Control.Monad (forever)
import Data.Aeson (Value(..), decode)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Lazy qualified as LBS
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as Text
import Data.UUID qualified as UUID
import Network.Wai.Handler.Warp (testWithApplication)
import Network.WebSockets qualified as WS
import System.Timeout (timeout)
import Test.Hspec

import HMem.Config qualified as Config
import HMem.DB.Auth qualified as Auth
import HMem.DB.ChangeStream
import HMem.DB.Pool (runSession)
import HMem.DB.Project qualified as Project
import HMem.DB.RequestContext (ActorType(..), Principal(..), PrincipalAuthority(..))
import HMem.DB.TestHarness (TestEnv(..), createTestWorkspace)
import HMem.DB.WorkspaceGroup qualified as WorkspaceGroup
import HMem.Server.TestHarness (DeployedSandboxApp(..), LocalSandboxApp(..), createDeployedSandboxUser, withDeployedSandboxAppContext, withLocalSandboxAppContext)
import HMem.Server.ChangeStream (startChangeStreamWorker, stopChangeStreamWorker)
import HMem.Server.Snapshot (materializeSnapshot)
import HMem.Server.WebSocket (createCanonicalTicket, createCanonicalTicketWithExpiry, createCanonicalTicketWithTtl, dispatchCanonicalOutbox, handleCanonicalDispatchFailure)
import HMem.Types (CreateProject(..), CreateWorkspaceGroup(..), WebSocketTicketResponse(..), Workspace(..), WorkspaceGroup(..))

spec :: Spec
spec = describe "canonical change-stream loopback" $ do
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
      resume <- terminalResume ctx scope audience
      issued <- createCanonicalTicketWithTtl ctx.deployedWSState 0.3 principal scope resume
      initial <- newEmptyMVar
      expired <- newEmptyMVar
      testWithApplication (pure ctx.deployedApplication) $ \port -> do
        client <- async $ (WS.runClient "127.0.0.1" port ("/api/v1/ws?ticket=" <> showTicket issued.ticket) $ \conn -> do
          WS.receiveData conn >>= putMVar initial
          WS.receiveData conn >>= putMVar expired
          WS.sendClose conn ("done" :: Text)) `catch` \(_ :: WS.ConnectionException) -> pure ()
        frameType <$> awaitFrame initial `shouldReturn` Just "checkpoint"
        _ <- Auth.upsertWorkspaceMembership ctx.deployedEnv.pool workspace.id
          (Auth.UpsertWorkspaceMembership hiddenUserId Auth.WorkspaceRoleRead) Nothing
        hiddenRecord <- latestOutbox ctx scope
        dispatchCanonicalOutbox ctx.deployedEnv.pool ctx.deployedWSState [hiddenRecord]
        frameType <$> awaitFrame expired `shouldReturn` Just "resync_required"
        wait client

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
      -- The harness configuration permits a 24-hour bearer, but this resume
      -- bearer was minted earlier with a sub-second lifetime. Its reconnect
      -- ticket must retain that stored expiry rather than a fresh config TTL.
      resume <- terminalResumeWithTtl ctx 0.8 scope audience
      threadDelay 350000
      validated <- validateCanonicalResumeToken ctx.deployedEnv.pool scope audience resume
      expiresAt <- either (fail . show) pure validated
      issued <- createCanonicalTicketWithExpiry ctx.deployedWSState expiresAt principal scope resume
      initial <- newEmptyMVar
      expired <- newEmptyMVar
      testWithApplication (pure ctx.deployedApplication) $ \port -> do
        client <- async $ (WS.runClient "127.0.0.1" port ("/api/v1/ws?ticket=" <> showTicket issued.ticket) $ \conn -> do
          WS.receiveData conn >>= putMVar initial
          WS.receiveData conn >>= putMVar expired
          WS.sendClose conn ("done" :: Text)) `catch` \(_ :: WS.ConnectionException) -> pure ()
        frameType <$> awaitFrame initial `shouldReturn` Just "checkpoint"
        _ <- Auth.upsertWorkspaceMembership ctx.deployedEnv.pool workspace.id
          (Auth.UpsertWorkspaceMembership hiddenUserId Auth.WorkspaceRoleRead) Nothing
        hiddenRecord <- latestOutbox ctx scope
        dispatchCanonicalOutbox ctx.deployedEnv.pool ctx.deployedWSState [hiddenRecord]
        frameType <$> awaitFrame expired `shouldReturn` Just "resync_required"
        wait client

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
      testWithApplication (pure ctx.deployedApplication) $ \port -> do
        client <- async $ (WS.runClient "127.0.0.1" port ("/api/v1/ws?ticket=" <> showTicket ticket.ticket) $ \conn -> do
          WS.receiveData conn >>= putMVar initial
          forever $ (WS.receiveData conn :: IO Text) >>= writeChan liveFrames) `catch` \(_ :: WS.ConnectionException) -> pure ()
        _ <- awaitFrame initial
        bracket (startChangeStreamWorker ctx.deployedEnv.pool testChangeStreamConfig ctx.deployedWSState) stopChangeStreamWorker $ \worker -> do
          _ <- Project.createProject ctx.deployedEnv.pool CreateProject
            { workspaceId = workspace.id, parentId = Nothing, name = "before-restart"
            , description = Nothing, priority = Nothing, metadata = Nothing }
          awaitChangeChan liveFrames
          stopChangeStreamWorker worker
          _ <- Project.createProject ctx.deployedEnv.pool CreateProject
            { workspaceId = workspace.id, parentId = Nothing, name = "during-restart"
            , description = Nothing, priority = Nothing, metadata = Nothing }
          bracket (startChangeStreamWorker ctx.deployedEnv.pool testChangeStreamConfig ctx.deployedWSState) stopChangeStreamWorker $ \_ -> do
            awaitChangeChan liveFrames
        cancel client
      bracket (startChangeStreamWorker ctx.deployedEnv.pool testChangeStreamConfig ctx.deployedWSState) stopChangeStreamWorker $ \worker -> do
        timeout 2000000 (stopChangeStreamWorker worker) `shouldReturn` Just ()

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

checkpointResume :: Value -> IO ResumeToken
checkpointResume value = case jsonField "resume_token" value of
  Just (String token) -> pure (ResumeToken token)
  _ -> fail "canonical checkpoint omitted its opaque resume token"

isText :: Value -> Bool
isText (String _) = True
isText _ = False

awaitChangeChan :: Chan Text -> IO ()
awaitChangeChan frames = do
  frame <- readChan frames >>= decodeFrame
  case frameType frame of
    Just "change" -> pure ()
    Just "checkpoint" -> awaitChangeChan frames
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
