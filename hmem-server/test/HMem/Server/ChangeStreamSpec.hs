module HMem.Server.ChangeStreamSpec (spec) where

import Control.Concurrent (Chan, MVar, newChan, newEmptyMVar, putMVar, readChan, takeMVar, threadDelay, throwTo, tryTakeMVar, writeChan)
import Control.Concurrent.Async (Async, async, asyncThreadId, cancel, wait, waitCatch)
import Control.Exception (AsyncException(..), SomeException, bracket, catch, finally, fromException, try)
import Control.Monad (forever)
import Data.Aeson (FromJSON, Value(..), decode, encode, object, (.=))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as Text
import Data.UUID qualified as UUID
import Network.HTTP.Types (Header, methodGet, methodPost, parseQuery, status200)
import Network.HTTP.Types qualified as HTTP
import Network.Wai (Application, defaultRequest)
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp (testWithApplication)
import Network.Wai.Test qualified as WaiTest
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
import HMem.Server.TestHarness (DeployedSandboxApp(..), LocalSandboxApp(..), createDeployedSandboxUser, issueDeployedSandboxPAT, withDeployedSandboxAppContext, withLocalSandboxAppContext)
import HMem.Server.AuthTokens (IssuedAccessToken(..))
import HMem.Server.ChangeStream (startChangeStreamWorker, stopChangeStreamWorker)
import HMem.Server.Snapshot (materializeSnapshot)
import HMem.Server.WebSocket (createCanonicalTicket, createCanonicalTicketWithExpiry, createCanonicalTicketWithTtl, dispatchCanonicalOutbox, handleCanonicalDispatchFailure)
import HMem.Types (ChangeStreamResyncResponse(..), ChangeStreamSnapshotItem(..), CreateProject(..), CreateWorkspaceGroup(..), Project(..), WebSocketTicketResponse(..), Workspace(..), WorkspaceGroup(..))

spec :: Spec
spec = describe "canonical change-stream loopback" $ do
  it "stops an acquired dispatcher when subsequent harness setup fails" $
    withLocalSandboxAppContext $ \ctx -> do
      stopped <- newEmptyMVar
      result <- try @SomeException $
        bracket
          (startChangeStreamWorker ctx.localEnv.pool testChangeStreamConfig ctx.localWSState)
          (\worker -> stopChangeStreamWorker worker `finally` putMVar stopped ())
          (\_worker -> ioError $ userError "injected harness setup failure")
      case result of
        Left _ -> pure ()
        Right () -> expectationFailure "injected harness setup failure unexpectedly succeeded"
      timeout 2000000 (takeMVar stopped) `shouldReturn` Just ()

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
  worker <- async $ (WS.runClient "127.0.0.1" port ("/api/v1/ws?ticket=" <> showTicket ticket) $ \conn -> do
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
authHeaders token = [("Authorization", "Bearer " <> Text.encodeUtf8 token)]

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
