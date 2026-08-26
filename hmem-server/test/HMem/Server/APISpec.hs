module HMem.Server.APISpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (poll, wait, withAsync)
import Control.Exception (onException)
import Control.Monad (void)
import Data.Aeson (Value(..), decode, encode, object, (.=))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Functor.Contravariant ((>$<))
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Maybe (isJust, isNothing)
import Data.Text qualified as T
import Data.Text.Encoding qualified as Text
import Data.Time (addUTCTime, getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.UUID (UUID)
import Hasql.Decoders qualified as Dec
import Hasql.Encoders qualified as Enc
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement
import Network.HTTP.Types (Header, methodDelete, methodGet, methodPost, methodPut, parseQuery, status200, status400, status401, status403, status404, status409)
import Network.HTTP.Types qualified
import Network.Wai (Application, defaultRequest)
import Network.Wai qualified as Wai
import Network.Wai.Test (SRequest(..), SResponse(..), runSession, srequest)
import Servant (Proxy(..), serve)
import Test.Hspec

import HMem.Config qualified as Config
import HMem.DB.Auth qualified as Auth
import HMem.DB.Pool qualified as DBPool
import HMem.DB.WorkspaceGroup qualified as WorkspaceGroup
import HMem.DB.RequestContext (ActorType(..), Principal(..), PrincipalAuthority(..), withPrincipalContext)
import HMem.DB.TestHarness (TestEnv(..), createTestWorkspace)
import HMem.Server.AccessTracker (newAccessTracker)
import HMem.Server.API (HMemAPI, server)
import HMem.Server.AuthTokens (IssuedAccessToken(..))
import HMem.Server.Event (ChangeEvent(..), ChangeType(..), EntityType(..), entityTypeToText)
import HMem.Server.TestHarness (DeployedSandboxApp(..), createDeployedSandboxUser, issueDeployedSandboxPAT, withDeployedSandboxAppContext, withLocalSandboxAppEnv)
import HMem.Server.WebSocket (WorkspaceSubscription(..), consumeTicket, eventVisibleToSubscription, newWSState, ticketEventVisible)
import HMem.Types

request :: Application -> BS.ByteString -> BS.ByteString -> LBS.ByteString -> IO SResponse
request app method path = requestWithHeaders app method path []

requestWithHeaders :: Application -> BS.ByteString -> BS.ByteString -> [Header] -> LBS.ByteString -> IO SResponse
requestWithHeaders app method path headers body = runSession (srequest (SRequest req body)) app where
  (rawPath, rawQuery) = BS.break (== 63) path
  req = defaultRequest
    { Wai.requestMethod = method, Wai.rawPathInfo = rawPath, Wai.rawQueryString = rawQuery, Wai.queryString = parseQuery rawQuery
    , Wai.pathInfo = filter (not . T.null) (T.splitOn "/" (Text.decodeUtf8 rawPath))
    , Wai.requestHeaders = [("Content-Type", "application/json")] <> headers }

postJson :: Application -> BS.ByteString -> Value -> IO SResponse
postJson app path = request app methodPost path . encode

responseStatus :: SResponse -> Network.HTTP.Types.Status
responseStatus SResponse { simpleStatus = status } = status

responseBody :: SResponse -> LBS.ByteString
responseBody SResponse { simpleBody = body } = body

jsonField :: T.Text -> Value -> Maybe Value
jsonField fieldName (Object fields) = KeyMap.lookup (Key.fromText fieldName) fields
jsonField _ _ = Nothing

jsonPath :: [T.Text] -> Value -> Maybe Value
jsonPath fieldNames value = foldl (\current fieldName -> current >>= jsonField fieldName) (Just value) fieldNames

jsonStrings :: Maybe Value -> Maybe [T.Text]
jsonStrings (Just (Array values)) = foldr collect (Just []) values
  where
    collect (String value) rest = (value :) <$> rest
    collect _ _ = Nothing
jsonStrings _ = Nothing

softDeleteWorkspaceStatement :: Statement.Statement UUID ()
softDeleteWorkspaceStatement = Statement.Statement
  "UPDATE workspaces SET deleted_at = now() WHERE id = $1"
  (Enc.param (Enc.nonNullable Enc.uuid)) Dec.noResult True

markWorkspaceDeleted :: TestEnv -> UUID -> IO ()
markWorkspaceDeleted env workspaceId = DBPool.runSession env.pool $
  Session.statement workspaceId softDeleteWorkspaceStatement

groupMembershipExists :: TestEnv -> UUID -> UUID -> IO Bool
groupMembershipExists env groupId workspaceId = DBPool.runSession env.pool $
  Session.statement (groupId, workspaceId) (Statement.Statement
    "SELECT EXISTS (SELECT 1 FROM workspace_group_members WHERE group_id = $1 AND workspace_id = $2)"
    ((fst >$< Enc.param (Enc.nonNullable Enc.uuid)) <> (snd >$< Enc.param (Enc.nonNullable Enc.uuid)))
    (Dec.singleRow (Dec.column (Dec.nonNullable Dec.bool))) True)

recordingGroupApp :: TestEnv -> IO (Application, IO [ChangeEvent])
recordingGroupApp env = do
  tracker <- newAccessTracker env.pool 3600
  wsState <- newWSState
  events <- newIORef []
  let broadcast event = modifyIORef' events (event :)
      app = serve (Proxy @HMemAPI) (server Config.defaultConfig.auth env.pool tracker broadcast wsState True)
      localSuperadmin = Principal
        { actorType = ActorUser, actorId = "group-event-test", actorLabel = "Group Event Test"
        , authority = PrincipalSyntheticLocalSuperadmin }
  pure (\req respond -> withPrincipalContext (Just localSuperadmin) (app req respond), reverse <$> readIORef events)

spec :: Spec
spec = around (\example -> withLocalSandboxAppEnv (\env app -> example (env, app))) $ do
  describe "Workspace Groups HTTP contract" $ do
    it "creates, lists, views, deletes, and manages active workspace members" $ \(env, app) -> do
      workspace <- createTestWorkspace env "group-member"
      createdResponse <- postJson app "/api/v1/groups" (object ["name" .= ("API group" :: T.Text), "description" .= ("group description" :: T.Text)])
      responseStatus createdResponse `shouldBe` status200
      let Just created = decode (responseBody createdResponse) :: Maybe WorkspaceGroup
          groupPath = "/api/v1/groups/" <> Text.encodeUtf8 (T.pack (show created.id))
      fetched <- request app methodGet groupPath ""
      responseStatus fetched `shouldBe` status200
      decode (responseBody fetched) `shouldBe` Just created
      secondResponse <- postJson app "/api/v1/groups" (object ["name" .= ("ZZ API group" :: T.Text)])
      let Just second = decode (responseBody secondResponse) :: Maybe WorkspaceGroup
      listed <- request app methodGet "/api/v1/groups?limit=1" ""
      responseStatus listed `shouldBe` status200
      let Just groupPage = decode (responseBody listed) :: Maybe (PaginatedResult WorkspaceGroup)
      groupPage.items `shouldBe` [created]
      groupPage.hasMore `shouldBe` True
      finalPage <- request app methodGet "/api/v1/groups?limit=1&offset=1" ""
      let Just finalGroupPage = decode (responseBody finalPage) :: Maybe (PaginatedResult WorkspaceGroup)
      finalGroupPage.items `shouldBe` [second]
      finalGroupPage.hasMore `shouldBe` False
      added <- postJson app (groupPath <> "/members") (object ["workspace_id" .= workspace.id])
      responseStatus added `shouldBe` status200
      members <- request app methodGet (groupPath <> "/members") ""
      responseStatus members `shouldBe` status200
      decode (responseBody members) `shouldBe` Just [workspace.id]
      removed <- request app methodDelete (groupPath <> "/members/" <> Text.encodeUtf8 (T.pack (show workspace.id))) ""
      responseStatus removed `shouldBe` status200
      emptyMembers <- request app methodGet (groupPath <> "/members") ""
      decode (responseBody emptyMembers) `shouldBe` Just ([] :: [UUID])
      deleted <- request app methodDelete groupPath ""
      responseStatus deleted `shouldBe` status200
      request app methodGet groupPath "" >>= (\response -> responseStatus response `shouldBe` status404)
      request app methodGet (groupPath <> "/members") "" >>= (\response -> responseStatus response `shouldBe` status404)
      entityTypeToText ETWorkspaceGroup `shouldBe` "workspace_group"

    it "emits global group events and workspace-scoped membership events" $ \(env, _app) -> do
      workspace <- createTestWorkspace env "group-event-member"
      (app, readEvents) <- recordingGroupApp env
      createdResponse <- postJson app "/api/v1/groups" (object ["name" .= ("event group" :: T.Text)])
      let Just created = decode (responseBody createdResponse) :: Maybe WorkspaceGroup
          groupPath = "/api/v1/groups/" <> Text.encodeUtf8 (T.pack (show created.id))
      postJson app (groupPath <> "/members") (object ["workspace_id" .= workspace.id]) >>= (\response -> responseStatus response `shouldBe` status200)
      postJson app (groupPath <> "/members") (object ["workspace_id" .= workspace.id]) >>= (\response -> responseStatus response `shouldBe` status200)
      inactiveWorkspace <- createTestWorkspace env "inactive-group-event-member"
      markWorkspaceDeleted env inactiveWorkspace.id
      postJson app (groupPath <> "/members") (object ["workspace_id" .= inactiveWorkspace.id]) >>= (\response -> responseStatus response `shouldBe` status404)
      request app methodDelete (groupPath <> "/members/" <> Text.encodeUtf8 (T.pack (show workspace.id))) "" >>= (\response -> responseStatus response `shouldBe` status200)
      request app methodDelete (groupPath <> "/members/" <> Text.encodeUtf8 (T.pack (show workspace.id))) "" >>= (\response -> responseStatus response `shouldBe` status200)
      request app methodDelete groupPath "" >>= (\response -> responseStatus response `shouldBe` status200)
      events <- readEvents
      map (\event -> (event.changeType, event.entityType, event.entityId, event.workspaceId)) events
        `shouldBe`
          [ (Created, ETWorkspaceGroup, created.id, Nothing)
          , (Updated, ETWorkspaceGroup, created.id, Just workspace.id)
          , (Updated, ETWorkspaceGroup, created.id, Just workspace.id)
          , (Deleted, ETWorkspaceGroup, created.id, Nothing)
          ]
      let createdEvent = head events
      eventVisibleToSubscription True (SubscribeWorkspace workspace.id) createdEvent `shouldBe` True
      eventVisibleToSubscription False (SubscribeWorkspace workspace.id) createdEvent `shouldBe` False

    it "validates create input and atomically rejects inactive member workspaces" $ \(env, app) -> do
      postJson app "/api/v1/groups" (object ["name" .= ("" :: T.Text)]) >>= (\response -> responseStatus response `shouldBe` status400)
      postJson app "/api/v1/groups" (object ["name" .= T.replicate 1025 "x"]) >>= (\response -> responseStatus response `shouldBe` status400)
      let missingGroup = "00000000-0000-0000-0000-000000000001"
      request app methodGet ("/api/v1/groups/" <> missingGroup) "" >>= (\response -> responseStatus response `shouldBe` status404)
      request app methodGet ("/api/v1/groups/" <> missingGroup <> "/members") "" >>= (\response -> responseStatus response `shouldBe` status404)
      createdResponse <- postJson app "/api/v1/groups" (object ["name" .= ("inactive member" :: T.Text)])
      let Just created = decode (responseBody createdResponse) :: Maybe WorkspaceGroup
          groupPath = "/api/v1/groups/" <> Text.encodeUtf8 (T.pack (show created.id))
      let missingWorkspace = "00000000-0000-0000-0000-000000000002" :: T.Text
      postJson app (groupPath <> "/members") (object ["workspace_id" .= missingWorkspace]) >>= (\response -> responseStatus response `shouldBe` status404)
      workspace <- createTestWorkspace env "inactive-group-member"
      markWorkspaceDeleted env workspace.id
      postJson app (groupPath <> "/members") (object ["workspace_id" .= workspace.id]) >>= (\response -> responseStatus response `shouldBe` status404)
      WorkspaceGroup.addMember env.pool created.id workspace.id
        `shouldReturn` WorkspaceGroup.MemberWorkspaceInactive
      groupMembershipExists env created.id workspace.id `shouldReturn` False

    it "does not insert when a concurrent soft-delete wins the workspace lock" $ \(env, app) -> do
      createdResponse <- postJson app "/api/v1/groups" (object ["name" .= ("concurrent inactive member" :: T.Text)])
      let Just created = decode (responseBody createdResponse) :: Maybe WorkspaceGroup
      workspace <- createTestWorkspace env "concurrent-inactive-group-member"
      DBPool.withConn env.pool $ \deleteConn ->
        (do
          deleteResult <- Session.run
            (Session.sql "BEGIN" >> Session.statement workspace.id softDeleteWorkspaceStatement)
            deleteConn
          case deleteResult of
            Left err -> expectationFailure $ "Failed to lock and soft-delete workspace: " <> show err
            Right () -> pure ()
          withAsync (WorkspaceGroup.addMember env.pool created.id workspace.id) $ \pendingAdd -> do
            -- The add sees the pre-update row but must wait for its FOR SHARE lock.
            -- Once the delete commits, PostgreSQL rechecks deleted_at and the
            -- INSERT ... SELECT receives no active workspace row.
            threadDelay 100000
            poll pendingAdd >>= (`shouldSatisfy` isNothing)
            commitResult <- Session.run (Session.sql "COMMIT") deleteConn
            case commitResult of
              Left err -> expectationFailure $ "Failed to commit concurrent workspace delete: " <> show err
              Right () -> pure ()
            wait pendingAdd `shouldReturn` WorkspaceGroup.MemberWorkspaceInactive
        ) `onException` void (Session.run (Session.sql "ROLLBACK") deleteConn)
      groupMembershipExists env created.id workspace.id `shouldReturn` False

  describe "Workspace Groups authorization" $ do
    it "requires global superadmin, not merely workspace-creation permission" $ \_ ->
      withDeployedSandboxAppContext $ \ctx -> do
        operatorId <- createDeployedSandboxUser ctx.deployedEnv True False
        operatorToken <- issueDeployedSandboxPAT ctx.deployedEnv operatorId "Group operator"
        denied <- requestWithHeaders ctx.deployedApplication methodGet "/api/v1/groups" [("Authorization", "Bearer " <> Text.encodeUtf8 operatorToken.rawToken)] ""
        responseStatus denied `shouldBe` status403
        superadminId <- createDeployedSandboxUser ctx.deployedEnv False True
        superadminToken <- issueDeployedSandboxPAT ctx.deployedEnv superadminId "Group superadmin"
        allowed <- requestWithHeaders ctx.deployedApplication methodGet "/api/v1/groups" [("Authorization", "Bearer " <> Text.encodeUtf8 superadminToken.rawToken)] ""
        responseStatus allowed `shouldBe` status200

    it "delivers global group events through deployed superadmin tickets only" $ \_ ->
      withDeployedSandboxAppContext $ \ctx -> do
        workspace <- createTestWorkspace ctx.deployedEnv "deployed-group-events"
        superadminId <- createDeployedSandboxUser ctx.deployedEnv False True
        memberId <- createDeployedSandboxUser ctx.deployedEnv False False
        _ <- Auth.upsertWorkspaceMembership ctx.deployedEnv.pool workspace.id
          (Auth.UpsertWorkspaceMembership memberId Auth.WorkspaceRoleRead) Nothing
        superadminToken <- issueDeployedSandboxPAT ctx.deployedEnv superadminId "Group event superadmin"
        memberToken <- issueDeployedSandboxPAT ctx.deployedEnv memberId "Group event member"
        let authHeader token = [("Authorization", "Bearer " <> Text.encodeUtf8 token.rawToken)]
            ticketRequest = encode (object ["workspace_id" .= workspace.id])
        superTicketResponse <- requestWithHeaders ctx.deployedApplication methodPost "/api/v1/ws-ticket" (authHeader superadminToken) ticketRequest
        memberTicketResponse <- requestWithHeaders ctx.deployedApplication methodPost "/api/v1/ws-ticket" (authHeader memberToken) ticketRequest
        responseStatus superTicketResponse `shouldBe` status200
        responseStatus memberTicketResponse `shouldBe` status200
        let Just superTicket = decode (responseBody superTicketResponse) :: Maybe WebSocketTicketResponse
            Just memberTicket = decode (responseBody memberTicketResponse) :: Maybe WebSocketTicketResponse
        now <- getCurrentTime
        let globalGroupEvent = ChangeEvent
              { changeType = Created, entityType = ETWorkspaceGroup, entityId = workspace.id, workspaceId = Nothing
              , timestamp = now, requestId = Nothing, actorType = Nothing, actorId = Nothing, actorLabel = Nothing, payload = Nothing }
        superTicketState <- consumeTicket ctx.deployedWSState superTicket.ticket
        memberTicketState <- consumeTicket ctx.deployedWSState memberTicket.ticket
        fmap (`ticketEventVisible` globalGroupEvent) superTicketState `shouldBe` Just True
        fmap (`ticketEventVisible` globalGroupEvent) memberTicketState `shouldBe` Just False

  describe "Global Audit Log authorization" $ do
    it "allows only deployed global superadmins to list across workspaces while preserving scoped admins and filters" $ \_ ->
      withDeployedSandboxAppContext $ \ctx -> do
        firstWorkspace <- createTestWorkspace ctx.deployedEnv "global-audit-first"
        secondWorkspace <- createTestWorkspace ctx.deployedEnv "global-audit-second"
        superadminId <- createDeployedSandboxUser ctx.deployedEnv False True
        superadminToken <- issueDeployedSandboxPAT ctx.deployedEnv superadminId "Audit superadmin"
        workspaceAdminId <- createDeployedSandboxUser ctx.deployedEnv False False
        _ <- Auth.upsertWorkspaceMembership ctx.deployedEnv.pool firstWorkspace.id
          (Auth.UpsertWorkspaceMembership workspaceAdminId Auth.WorkspaceRoleAdmin) Nothing
        workspaceAdminToken <- issueDeployedSandboxPAT ctx.deployedEnv workspaceAdminId "Audit workspace admin"
        let authHeader token = [("Authorization", "Bearer " <> Text.encodeUtf8 token.rawToken)]
            projectInput workspace name = object
              [ "workspace_id" .= workspace.id, "name" .= (name :: T.Text) ]
            projectPath = "/api/v1/projects"
            auditPath = "/api/v1/audit?entity_type=project"
        firstCreated <- requestWithHeaders ctx.deployedApplication methodPost projectPath (authHeader superadminToken) (encode (projectInput firstWorkspace "First audit project"))
        secondCreated <- requestWithHeaders ctx.deployedApplication methodPost projectPath (authHeader superadminToken) (encode (projectInput secondWorkspace "Second audit project"))
        responseStatus firstCreated `shouldBe` status200
        responseStatus secondCreated `shouldBe` status200
        let Just firstProject = decode (responseBody firstCreated) :: Maybe Project

        unauthenticated <- request ctx.deployedApplication methodGet auditPath ""
        responseStatus unauthenticated `shouldBe` status401
        denied <- requestWithHeaders ctx.deployedApplication methodGet auditPath (authHeader workspaceAdminToken) ""
        responseStatus denied `shouldBe` status403
        scoped <- requestWithHeaders ctx.deployedApplication methodGet
          (auditPath <> "&workspace_id=" <> Text.encodeUtf8 (T.pack (show firstWorkspace.id)))
          (authHeader workspaceAdminToken) ""
        responseStatus scoped `shouldBe` status200
        let Just scopedPage = decode (responseBody scoped) :: Maybe (PaginatedResult AuditLogEntry)
        scopedPage.items `shouldSatisfy` (not . null)
        all (\entry -> entry.workspaceId == Just firstWorkspace.id) scopedPage.items `shouldBe` True

        firstGlobalPage <- requestWithHeaders ctx.deployedApplication methodGet (auditPath <> "&limit=1") (authHeader superadminToken) ""
        responseStatus firstGlobalPage `shouldBe` status200
        let Just firstPage = decode (responseBody firstGlobalPage) :: Maybe (PaginatedResult AuditLogEntry)
        firstPage.hasMore `shouldBe` True
        global <- requestWithHeaders ctx.deployedApplication methodGet (auditPath <> "&limit=10") (authHeader superadminToken) ""
        responseStatus global `shouldBe` status200
        let Just globalPage = decode (responseBody global) :: Maybe (PaginatedResult AuditLogEntry)
            globalWorkspaces = map (.workspaceId) globalPage.items
        globalWorkspaces `shouldSatisfy` (elem (Just firstWorkspace.id))
        globalWorkspaces `shouldSatisfy` (elem (Just secondWorkspace.id))
        filtered <- requestWithHeaders ctx.deployedApplication methodGet
          (auditPath <> "&entity_id=" <> Text.encodeUtf8 (T.pack (show firstProject.id)))
          (authHeader superadminToken) ""
        responseStatus filtered `shouldBe` status200
        let Just filteredPage = decode (responseBody filtered) :: Maybe (PaginatedResult AuditLogEntry)
        filteredPage.items `shouldSatisfy` (not . null)
        all (\entry -> entry.entityId == T.pack (show firstProject.id)) filteredPage.items `shouldBe` True

  describe "Workspace Timeline HTTP contract" $ do
    it "lists, buckets, validates queries, and rejects missing or deleted workspaces" $ \(env, app) -> do
      workspace <- createTestWorkspace env "timeline-api"
      let workspacePath = "/api/v1/workspaces/" <> Text.encodeUtf8 (T.pack (show workspace.id))
          bucketPath = workspacePath <> "/timeline/buckets?since=2020-01-01T00:00:00Z&until=2020-01-02T00:00:00Z&bucket=day"
      timeline <- request app methodGet (workspacePath <> "/timeline?limit=1") ""
      responseStatus timeline `shouldBe` status200
      let Just page = decode (responseBody timeline) :: Maybe (PaginatedResult WorkspaceTimelineEvent)
      page.items `shouldBe` []
      page.hasMore `shouldBe` False
      buckets <- request app methodGet bucketPath ""
      responseStatus buckets `shouldBe` status200
      let Just bucketResponse = decode (responseBody buckets) :: Maybe WorkspaceTimelineBucketsResponse
      bucketResponse.timelineBucketsWorkspaceId `shouldBe` workspace.id
      bucketResponse.timelineBucketsBucket `shouldBe` "day"
      bucketResponse.timelineBucketsBuckets `shouldSatisfy` (not . null)
      mapM_ (\suffix -> request app methodGet (workspacePath <> suffix) "" >>= (\response -> responseStatus response `shouldBe` status400))
        [ "/timeline?limit=0", "/timeline?limit=201", "/timeline?offset=-1"
        , "/timeline?since=2021-01-02T00:00:00Z&until=2021-01-01T00:00:00Z"
        , "/timeline/buckets?until=2021-01-02T00:00:00Z&bucket=day"
        , "/timeline/buckets?since=2021-01-01T00:00:00Z&until=2021-01-02T00:00:00Z&bucket=year"
        ]
      request app methodGet "/api/v1/workspaces/00000000-0000-0000-0000-000000000099/timeline" "" >>= (\response -> responseStatus response `shouldBe` status404)
      markWorkspaceDeleted env workspace.id
      request app methodGet (workspacePath <> "/timeline") "" >>= (\response -> responseStatus response `shouldBe` status404)

    it "preserves lifecycle filters, half-open ranges, ordering, pagination, and bucket totals" $ \(env, app) -> do
      workspace <- createTestWorkspace env "timeline-lifecycle"
      rangeStart <- addUTCTime (-1) <$> getCurrentTime
      let workspacePath = "/api/v1/workspaces/" <> Text.encodeUtf8 (T.pack (show workspace.id))
          projectInput name = object ["workspace_id" .= workspace.id, "name" .= (name :: T.Text)]
          taskInput title = object ["workspace_id" .= workspace.id, "title" .= (title :: T.Text)]
          projectPath project = "/api/v1/projects/" <> Text.encodeUtf8 (T.pack (show project.id))
          taskPath task = "/api/v1/tasks/" <> Text.encodeUtf8 (T.pack (show task.id))
          getTimeline suffix = request app methodGet (workspacePath <> "/timeline" <> suffix) ""
          timestamp value = Text.encodeUtf8 (T.pack (iso8601Show value))

      rootResponse <- postJson app "/api/v1/projects" (projectInput "Timeline root")
      responseStatus rootResponse `shouldBe` status200
      let Just root = decode (responseBody rootResponse) :: Maybe Project
      subprojectResponse <- postJson app "/api/v1/projects" (object
        [ "workspace_id" .= workspace.id, "parent_id" .= root.id, "name" .= ("Timeline child project" :: T.Text) ])
      responseStatus subprojectResponse `shouldBe` status200
      let Just subproject = decode (responseBody subprojectResponse) :: Maybe Project
      request app methodPut (projectPath subproject) (encode (object ["status" .= ("archived" :: T.Text)])) >>= (\response -> responseStatus response `shouldBe` status200)

      completedTaskResponse <- postJson app "/api/v1/tasks" (taskInput "Timeline completed task")
      responseStatus completedTaskResponse `shouldBe` status200
      let Just completedTask = decode (responseBody completedTaskResponse) :: Maybe Task
      request app methodPut (taskPath completedTask) (encode (object ["status" .= ("done" :: T.Text)])) >>= (\response -> responseStatus response `shouldBe` status200)

      cancelledTaskResponse <- postJson app "/api/v1/tasks" (taskInput "Timeline cancelled task")
      responseStatus cancelledTaskResponse `shouldBe` status200
      let Just cancelledTask = decode (responseBody cancelledTaskResponse) :: Maybe Task
      request app methodPut (taskPath cancelledTask) (encode (object ["status" .= ("cancelled" :: T.Text)])) >>= (\response -> responseStatus response `shouldBe` status200)

      parentTaskResponse <- postJson app "/api/v1/tasks" (taskInput "Timeline parent task")
      responseStatus parentTaskResponse `shouldBe` status200
      let Just parentTask = decode (responseBody parentTaskResponse) :: Maybe Task
      childTaskResponse <- postJson app "/api/v1/tasks" (object
        [ "workspace_id" .= workspace.id, "parent_id" .= parentTask.id, "title" .= ("Timeline subtask" :: T.Text) ])
      responseStatus childTaskResponse `shouldBe` status200
      let Just childTask = decode (responseBody childTaskResponse) :: Maybe Task
      request app methodPut (taskPath childTask) (encode (object ["status" .= ("done" :: T.Text)])) >>= (\response -> responseStatus response `shouldBe` status200)
      rangeEnd <- addUTCTime 1 <$> getCurrentTime

      allResponse <- getTimeline "?limit=50"
      responseStatus allResponse `shouldBe` status200
      let Just allEvents = decode (responseBody allResponse) :: Maybe (PaginatedResult WorkspaceTimelineEvent)
          events = allEvents.items
          eventIds = map (.id) events
          hasEvent eventType entityId = any (\event -> event.eventType == eventType && event.entityId == entityId) events
      hasEvent "project_created" root.id `shouldBe` True
      hasEvent "subtask_completed" childTask.id `shouldBe` True
      hasEvent "task_completed" completedTask.id `shouldBe` True
      hasEvent "task_cancelled" cancelledTask.id `shouldBe` True

      taskOnlyResponse <- getTimeline "?entity_type=task&limit=50"
      let Just taskOnly = decode (responseBody taskOnlyResponse) :: Maybe (PaginatedResult WorkspaceTimelineEvent)
      responseStatus taskOnlyResponse `shouldBe` status200
      taskOnly.items `shouldSatisfy` (not . null)
      taskOnly.items `shouldSatisfy` all (\event -> event.entityType == "task")
      subtaskOnlyResponse <- getTimeline "?entity_type=subtask&limit=50"
      let Just subtaskOnly = decode (responseBody subtaskOnlyResponse) :: Maybe (PaginatedResult WorkspaceTimelineEvent)
      responseStatus subtaskOnlyResponse `shouldBe` status200
      subtaskOnly.items `shouldSatisfy` all (\event -> event.entityType == "subtask")
      completedOnlyResponse <- getTimeline "?event_type=task_completed&limit=50"
      let Just completedOnly = decode (responseBody completedOnlyResponse) :: Maybe (PaginatedResult WorkspaceTimelineEvent)
      responseStatus completedOnlyResponse `shouldBe` status200
      completedOnly.items `shouldBe` filter (\event -> event.eventType == "task_completed") events

      let boundary = head events
      untilBoundaryResponse <- getTimeline ("?until=" <> timestamp boundary.occurredAt <> "&limit=50")
      let Just untilBoundary = decode (responseBody untilBoundaryResponse) :: Maybe (PaginatedResult WorkspaceTimelineEvent)
      responseStatus untilBoundaryResponse `shouldBe` status200
      map (.id) untilBoundary.items `shouldSatisfy` (notElem boundary.id)
      sinceBoundaryResponse <- getTimeline ("?since=" <> timestamp boundary.occurredAt <> "&limit=50")
      let Just sinceBoundary = decode (responseBody sinceBoundaryResponse) :: Maybe (PaginatedResult WorkspaceTimelineEvent)
      responseStatus sinceBoundaryResponse `shouldBe` status200
      boundary.id `shouldBe` head (map (.id) sinceBoundary.items)

      firstPageResponse <- getTimeline "?limit=1"
      secondPageResponse <- getTimeline "?limit=1&offset=1"
      let Just firstPage = decode (responseBody firstPageResponse) :: Maybe (PaginatedResult WorkspaceTimelineEvent)
          Just secondPage = decode (responseBody secondPageResponse) :: Maybe (PaginatedResult WorkspaceTimelineEvent)
      responseStatus firstPageResponse `shouldBe` status200
      responseStatus secondPageResponse `shouldBe` status200
      map (.id) firstPage.items `shouldBe` take 1 eventIds
      map (.id) secondPage.items `shouldBe` take 1 (drop 1 eventIds)
      firstPage.hasMore `shouldBe` True
      secondPage.hasMore `shouldBe` True

      let bucketQuery = "/timeline/buckets?since=" <> timestamp rangeStart <> "&until=" <> timestamp rangeEnd <> "&bucket=day"
      bucketsResponse <- request app methodGet (workspacePath <> bucketQuery) ""
      responseStatus bucketsResponse `shouldBe` status200
      let Just buckets = decode (responseBody bucketsResponse) :: Maybe WorkspaceTimelineBucketsResponse
          totals select = sum (map select buckets.timelineBucketsBuckets)
      totals (\bucket -> bucket.timelineBucketCounts.projectCounts.created) `shouldBe` 1
      totals (\bucket -> bucket.timelineBucketCounts.subprojectCounts.created) `shouldBe` 1
      totals (\bucket -> bucket.timelineBucketCounts.subprojectCounts.completed) `shouldBe` 1
      totals (\bucket -> bucket.timelineBucketCounts.taskCounts.created) `shouldBe` 3
      totals (\bucket -> bucket.timelineBucketCounts.taskCounts.completed) `shouldBe` 1
      totals (\bucket -> bucket.timelineBucketCounts.taskCounts.cancelled) `shouldBe` 1
      totals (\bucket -> bucket.timelineBucketCounts.subtaskCounts.created) `shouldBe` 1
      totals (\bucket -> bucket.timelineBucketCounts.subtaskCounts.completed) `shouldBe` 1
      totals (\bucket -> bucket.timelineBucketTotals.created) `shouldBe` 6
      totals (\bucket -> bucket.timelineBucketTotals.completed) `shouldBe` 3
      totals (\bucket -> bucket.timelineBucketTotals.cancelled) `shouldBe` 1
      futureBucketsResponse <- request app methodGet (workspacePath <> "/timeline/buckets?since=" <> timestamp rangeEnd <> "&until=" <> timestamp (addUTCTime 86400 rangeEnd) <> "&bucket=day") ""
      responseStatus futureBucketsResponse `shouldBe` status200
      let Just futureBuckets = decode (responseBody futureBucketsResponse) :: Maybe WorkspaceTimelineBucketsResponse
          totalsFuture response = sum (map (\bucket -> bucket.timelineBucketTotals.created + bucket.timelineBucketTotals.completed + bucket.timelineBucketTotals.cancelled) response.timelineBucketsBuckets)
      totalsFuture futureBuckets `shouldBe` 0

    it "allows readers and superadmins, but denies non-members" $ \_ ->
      withDeployedSandboxAppContext $ \ctx -> do
        workspace <- createTestWorkspace ctx.deployedEnv "timeline-auth"
        readerId <- createDeployedSandboxUser ctx.deployedEnv False False
        outsiderId <- createDeployedSandboxUser ctx.deployedEnv False False
        superadminId <- createDeployedSandboxUser ctx.deployedEnv False True
        _ <- Auth.upsertWorkspaceMembership ctx.deployedEnv.pool workspace.id
          (Auth.UpsertWorkspaceMembership readerId Auth.WorkspaceRoleRead) Nothing
        readerToken <- issueDeployedSandboxPAT ctx.deployedEnv readerId "Timeline reader"
        outsiderToken <- issueDeployedSandboxPAT ctx.deployedEnv outsiderId "Timeline outsider"
        superadminToken <- issueDeployedSandboxPAT ctx.deployedEnv superadminId "Timeline superadmin"
        let timelinePath = "/api/v1/workspaces/" <> Text.encodeUtf8 (T.pack (show workspace.id)) <> "/timeline"
            bucketPath = timelinePath <> "/buckets?since=2020-01-01T00:00:00Z&until=2020-01-02T00:00:00Z&bucket=day"
            authHeader token = [("Authorization", "Bearer " <> Text.encodeUtf8 token.rawToken)]
        requestWithHeaders ctx.deployedApplication methodGet timelinePath (authHeader readerToken) "" >>= (\response -> responseStatus response `shouldBe` status200)
        requestWithHeaders ctx.deployedApplication methodGet timelinePath (authHeader outsiderToken) "" >>= (\response -> responseStatus response `shouldBe` status403)
        requestWithHeaders ctx.deployedApplication methodGet timelinePath (authHeader superadminToken) "" >>= (\response -> responseStatus response `shouldBe` status200)
        requestWithHeaders ctx.deployedApplication methodGet bucketPath (authHeader readerToken) "" >>= (\response -> responseStatus response `shouldBe` status200)
        requestWithHeaders ctx.deployedApplication methodGet bucketPath (authHeader outsiderToken) "" >>= (\response -> responseStatus response `shouldBe` status403)
        requestWithHeaders ctx.deployedApplication methodGet bucketPath (authHeader superadminToken) "" >>= (\response -> responseStatus response `shouldBe` status200)

  describe "Observation HTTP contract" $ do
    it "creates, lists, updates content only, and hard deletes repository observations" $ \(env, app) -> do
      workspace <- createTestWorkspace env "observation-api"
      createdResponse <- postJson app "/api/v1/observations" (object
        [ "workspace_id" .= workspace.id, "subject_kind" .= ("file" :: T.Text)
        , "subject" .= ("src/Main.hs" :: T.Text), "git_sha" .= ("0123456789abcdef0123456789abcdef01234567" :: T.Text)
        , "content" .= ("initial observation" :: T.Text) ])
      responseStatus createdResponse `shouldBe` status200
      let Just created = decode (responseBody createdResponse) :: Maybe Observation
      listed <- request app methodGet ("/api/v1/observations?workspace_id=" <> Text.encodeUtf8 (T.pack (show workspace.id)) <> "&subject_kind=file&subject=src/Main.hs") ""
      responseStatus listed `shouldBe` status200
      let Just page = decode (responseBody listed) :: Maybe (PaginatedResult Observation)
      page.items `shouldBe` [created]
      maximumPage <- request app methodGet ("/api/v1/observations?workspace_id=" <> Text.encodeUtf8 (T.pack (show workspace.id)) <> "&limit=200") ""
      responseStatus maximumPage `shouldBe` status200
      mapM_ (\suffix -> request app methodGet ("/api/v1/observations?workspace_id=" <> Text.encodeUtf8 (T.pack (show workspace.id)) <> suffix) "" >>= (\response -> responseStatus response `shouldBe` status400))
        ["&limit=0", "&limit=201", "&offset=-1", "&offset=100001"]
      updated <- request app methodPut ("/api/v1/observations/" <> Text.encodeUtf8 (T.pack (show created.id))) (encode (object ["content" .= ("revised observation" :: T.Text)]))
      responseStatus updated `shouldBe` status200
      let Just revised = decode (responseBody updated) :: Maybe Observation
      revised.content `shouldBe` "revised observation"
      revised.subject `shouldBe` created.subject
      immutable <- request app methodPut ("/api/v1/observations/" <> Text.encodeUtf8 (T.pack (show created.id))) (encode (object ["content" .= ("x" :: T.Text), "git_sha" .= ("different" :: T.Text)]))
      responseStatus immutable `shouldBe` status400 -- Servant rejects non-contract request bodies before routing the handler.
      deleted <- request app methodDelete ("/api/v1/observations/" <> Text.encodeUtf8 (T.pack (show created.id))) ""
      responseStatus deleted `shouldBe` status200
      missing <- request app methodGet ("/api/v1/observations/" <> Text.encodeUtf8 (T.pack (show created.id))) ""
      responseStatus missing `shouldBe` status404

    it "rejects planning and deleted workspaces through every Observation-bearing HTTP surface" $ \(env, app) -> do
      planningResponse <- postJson app "/api/v1/workspaces" (object
        [ "name" .= ("observation-planning" :: T.Text), "workspace_type" .= ("planning" :: T.Text) ])
      responseStatus planningResponse `shouldBe` status200
      let Just planning = decode (responseBody planningResponse) :: Maybe Workspace
          planningId = Text.encodeUtf8 (T.pack (show planning.id))
          observationInput workspaceId = object
            [ "workspace_id" .= workspaceId, "subject_kind" .= ("file" :: T.Text)
            , "subject" .= ("src/Scope.hs" :: T.Text), "git_sha" .= ("0123456789abcdef0123456789abcdef01234567" :: T.Text)
            , "content" .= ("scope" :: T.Text) ]
          rejected response = responseStatus response `shouldBe` status404
      postJson app "/api/v1/observations" (observationInput planning.id) >>= rejected
      request app methodGet ("/api/v1/observations?workspace_id=" <> planningId) "" >>= rejected
      postJson app "/api/v1/observations/similar" (object ["workspace_id" .= planning.id, "embedding" .= ([] :: [Double])]) >>= rejected
      postJson app "/api/v1/search" (object ["workspace_id" .= planning.id, "entity_types" .= ["observation" :: T.Text]]) >>= rejected
      request app methodGet ("/api/v1/audit?workspace_id=" <> planningId <> "&entity_type=observation") "" >>= rejected

      repository <- createTestWorkspace env "observation-deleted"
      createdResponse <- postJson app "/api/v1/observations" (observationInput repository.id)
      responseStatus createdResponse `shouldBe` status200
      let Just created = decode (responseBody createdResponse) :: Maybe Observation
          observationPath = "/api/v1/observations/" <> Text.encodeUtf8 (T.pack (show created.id))
          repositoryId = Text.encodeUtf8 (T.pack (show repository.id))
      markWorkspaceDeleted env repository.id
      request app methodGet ("/api/v1/observations?workspace_id=" <> repositoryId) "" >>= rejected
      postJson app "/api/v1/observations" (observationInput repository.id) >>= rejected
      postJson app "/api/v1/observations/similar" (object ["workspace_id" .= repository.id, "embedding" .= ([] :: [Double])]) >>= rejected
      request app methodGet observationPath "" >>= rejected
      request app methodPut observationPath (encode (object ["content" .= ("rejected" :: T.Text)])) >>= rejected
      request app methodDelete observationPath "" >>= rejected
      request app methodPut (observationPath <> "/embedding") (encode ([] :: [Double])) >>= rejected
      postJson app "/api/v1/search" (object ["workspace_id" .= repository.id, "entity_types" .= ["observation" :: T.Text]]) >>= rejected
      request app methodGet ("/api/v1/audit?workspace_id=" <> repositoryId <> "&entity_type=observation") "" >>= rejected

    it "keeps the removed Memory, category, cleanup, link, and context routes absent" $ \(_env, app) -> do
      mapM_ (\path -> request app methodGet path "" >>= (\response -> responseStatus response `shouldBe` status404))
        [ "/api/v1/memories", "/api/v1/categories", "/api/v1/cleanup/policies"
        , "/api/v1/projects/00000000-0000-0000-0000-000000000001/memories"
        , "/api/v1/tasks/00000000-0000-0000-0000-000000000001/context" ]

  describe "Observation audit and events" $ do
    it "lists observation audit entries and explicitly rejects audit reverts" $ \(env, app) -> do
      workspace <- createTestWorkspace env "observation-audit"
      response <- postJson app "/api/v1/observations" (object
        [ "workspace_id" .= workspace.id, "subject_kind" .= ("glob" :: T.Text)
        , "subject" .= ("src/**/*.hs" :: T.Text), "git_sha" .= ("0123456789abcdef0123456789abcdef01234567" :: T.Text)
        , "content" .= ("audit me" :: T.Text) ])
      responseStatus response `shouldBe` status200
      let Just observation = decode (responseBody response) :: Maybe Observation
      audits <- request app methodGet ("/api/v1/audit?workspace_id=" <> Text.encodeUtf8 (T.pack (show workspace.id)) <> "&entity_type=observation") ""
      responseStatus audits `shouldBe` status200
      let Just auditPage = decode (responseBody audits) :: Maybe (PaginatedResult AuditLogEntry)
      auditPage.items `shouldSatisfy` (not . null)
      let entry = head auditPage.items
      entry.entityType `shouldBe` "observation"
      revert <- request app methodPost ("/api/v1/audit/" <> Text.encodeUtf8 (T.pack (show entry.id)) <> "/revert") ""
      responseStatus revert `shouldBe` status409
      deleted <- request app methodDelete ("/api/v1/observations/" <> Text.encodeUtf8 (T.pack (show observation.id))) ""
      responseStatus deleted `shouldBe` status200
      deletedAudits <- request app methodGet ("/api/v1/audit?workspace_id=" <> Text.encodeUtf8 (T.pack (show workspace.id)) <> "&entity_type=observation&action=delete") ""
      responseStatus deletedAudits `shouldBe` status200
      let Just deletedPage = decode (responseBody deletedAudits) :: Maybe (PaginatedResult AuditLogEntry)
          deleteEntry = head deletedPage.items
      fetchedDelete <- request app methodGet ("/api/v1/audit/" <> Text.encodeUtf8 (T.pack (show deleteEntry.id))) ""
      responseStatus fetchedDelete `shouldBe` status200
      deletedRevert <- request app methodPost ("/api/v1/audit/" <> Text.encodeUtf8 (T.pack (show deleteEntry.id)) <> "/revert") ""
      responseStatus deletedRevert `shouldBe` status409
      let Just revertError = decode (responseBody deletedRevert) :: Maybe Value
      jsonField "error" revertError `shouldBe` Just (String "unsupported_entity")
      stillDeleted <- request app methodGet ("/api/v1/observations/" <> Text.encodeUtf8 (T.pack (show observation.id))) ""
      responseStatus stillDeleted `shouldBe` status404
      entityTypeToText ETObservation `shouldBe` "observation"

  describe "OpenAPI" $ do
    it "documents Observation and Timeline contracts while excluding legacy paths" $ \(_env, app) -> do
      -- Check the served document too, not merely the value used by middleware.
      served <- request app methodGet "/api/v1/openapi.json" ""
      responseStatus served `shouldBe` status200
      let Just document = decode (responseBody served) :: Maybe Value
          paths = jsonPath ["paths"] document
          schema name = jsonPath ["components", "schemas", name] document
          hasPath path = isJust (paths >>= jsonField path)
          operationTags path method = paths >>= jsonField path >>= jsonField method >>= jsonField "tags"
          hasObservationTag path method = jsonStrings (operationTags path method) == Just ["Observations"]
          enum name = jsonStrings (schema name >>= jsonField "enum")
          embeddingSchema = schema "SimilarObservationQuery" >>= jsonField "properties" >>= jsonField "embedding"
          fixedEmbedding = embeddingSchema >>= \embedding -> do
            minimum <- jsonField "minItems" embedding
            maximum <- jsonField "maxItems" embedding
            pure (minimum, maximum)
          hasOptionalAuditWorkspace = case jsonPath ["paths", "/api/v1/audit", "get", "parameters"] document of
            Just (Array parameters) -> any (\parameter ->
              jsonField "name" parameter == Just (String "workspace_id")
                && jsonField "required" parameter == Just (Bool False)) parameters
            _ -> False
      mapM_ (\path -> hasPath path `shouldBe` True)
        [ "/api/v1/groups"
        , "/api/v1/groups/{groupId}"
        , "/api/v1/groups/{groupId}/members"
        , "/api/v1/groups/{groupId}/members/{workspaceId}"
        , "/api/v1/observations"
        , "/api/v1/observations/similar"
        , "/api/v1/observations/{observationId}"
        , "/api/v1/observations/{observationId}/embedding"
        , "/api/v1/workspaces/{workspaceId}/timeline"
        , "/api/v1/workspaces/{workspaceId}/timeline/buckets" ]
      let hasWorkspaceGroupTag path method = jsonStrings (operationTags path method) == Just ["Workspace Groups"]
      mapM_ (\(path, method) -> hasWorkspaceGroupTag path method `shouldBe` True)
        [ ("/api/v1/groups", "get"), ("/api/v1/groups", "post")
        , ("/api/v1/groups/{groupId}", "get"), ("/api/v1/groups/{groupId}", "delete")
        , ("/api/v1/groups/{groupId}/members", "get"), ("/api/v1/groups/{groupId}/members", "post")
        , ("/api/v1/groups/{groupId}/members/{workspaceId}", "delete") ]
      mapM_ (\(path, method) -> hasObservationTag path method `shouldBe` True)
        [ ("/api/v1/observations", "get"), ("/api/v1/observations", "post")
        , ("/api/v1/observations/similar", "post")
        , ("/api/v1/observations/{observationId}", "get")
        , ("/api/v1/observations/{observationId}", "put")
        , ("/api/v1/observations/{observationId}", "delete")
        , ("/api/v1/observations/{observationId}/embedding", "put") ]
      enum "SubjectKind" `shouldBe` Just ["file", "glob"]
      enum "EntitySearchType" `shouldBe` Just ["observation", "project", "task"]
      schema "WorkspaceTimelineEvent" `shouldSatisfy` isJust
      schema "WorkspaceTimelineBucketsResponse" `shouldSatisfy` isJust
      fixedEmbedding `shouldBe` Just (Number 1536, Number 1536)
      hasOptionalAuditWorkspace `shouldBe` True
      mapM_ (\legacyPath -> (paths >>= jsonField legacyPath) `shouldBe` Nothing)
        [ "/api/v1/memories", "/api/v1/categories", "/api/v1/cleanup/policies"
        , "/api/v1/projects/{projectId}/memories", "/api/v1/tasks/{taskId}/context" ]
