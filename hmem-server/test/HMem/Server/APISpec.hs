module HMem.Server.APISpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (poll, wait, withAsync)
import Control.Exception (onException)
import Control.Monad (forM_, void)
import Data.Aeson (Value(..), decode, encode, object, toJSON, (.=))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Foldable (toList)
import Data.Functor.Contravariant ((>$<))
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.List (find)
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
import Network.HTTP.Types (Header, methodDelete, methodGet, methodPost, methodPut, parseQuery, status200, status400, status401, status403, status404, status409, status503)
import Network.HTTP.Types qualified
import Network.Wai (Application, defaultRequest)
import Network.Wai qualified as Wai
import Network.Wai.Test (SRequest(..), SResponse(..), runSession, srequest)
import Servant (Proxy(..), serve)
import Test.Hspec

import HMem.Config qualified as Config
import HMem.DB.Auth qualified as Auth
import HMem.DB.ChangeStream (ChangeScope(..), ChangeAudience(..), ResumeToken(..), ReplayPage(..), OutboxRecord(..), listOutboxAfter, replayAndRotateResumeToken)
import HMem.DB.Pool qualified as DBPool
import HMem.DB.WorkspaceGroup qualified as WorkspaceGroup
import HMem.DB.RequestContext (ActorType(..), Principal(..), PrincipalAuthority(..), withPrincipalContext)
import HMem.DB.TestHarness (TestEnv(..), createTestWorkspace)
import HMem.Server.AccessTracker (newAccessTracker)
import HMem.Server.API (HMemAPI, server)
import HMem.Server.AuthTokens (IssuedAccessToken(..))
import HMem.Server.Event (ChangeEvent(..), ChangeType(..), EntityType(..), entityTypeToText)
import HMem.Server.App (mkApp)
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

firstJsonArrayValue :: Maybe Value -> Maybe Value
firstJsonArrayValue (Just (Array values)) = case toList values of
  value : _ -> Just value
  [] -> Nothing
firstJsonArrayValue _ = Nothing

softDeleteWorkspaceStatement :: Statement.Statement UUID ()
softDeleteWorkspaceStatement = Statement.Statement
  "UPDATE workspaces SET deleted_at = now() WHERE id = $1"
  (Enc.param (Enc.nonNullable Enc.uuid)) Dec.noResult True

markWorkspaceDeleted :: TestEnv -> UUID -> IO ()
markWorkspaceDeleted env workspaceId = DBPool.runSession env.pool $
  Session.statement workspaceId softDeleteWorkspaceStatement

backdateObservationStatement :: Statement.Statement UUID ()
backdateObservationStatement = Statement.Statement
  "UPDATE observations SET updated_at = '2000-01-01T00:00:00Z' WHERE id = $1"
  (Enc.param (Enc.nonNullable Enc.uuid)) Dec.noResult True

-- now() is fixed for this suite's outer rollback transaction. Backdating one
-- setup row makes the production update trigger's recency effect observable.
backdateObservation :: TestEnv -> UUID -> IO ()
backdateObservation env observationId = DBPool.runSession env.pool $ do
  Session.sql "ALTER TABLE observations DISABLE TRIGGER trg_observations_updated_at"
  Session.statement observationId backdateObservationStatement
  Session.sql "ALTER TABLE observations ENABLE TRIGGER trg_observations_updated_at"

groupMembershipExists :: TestEnv -> UUID -> UUID -> IO Bool
groupMembershipExists env groupId workspaceId = DBPool.runSession env.pool $
  Session.statement (groupId, workspaceId) (Statement.Statement
    "SELECT EXISTS (SELECT 1 FROM workspace_group_members WHERE group_id = $1 AND workspace_id = $2)"
    ((fst >$< Enc.param (Enc.nonNullable Enc.uuid)) <> (snd >$< Enc.param (Enc.nonNullable Enc.uuid)))
    (Dec.singleRow (Dec.column (Dec.nonNullable Dec.bool))) True)

recordingGroupApp :: TestEnv -> IO Application
recordingGroupApp env = do
  tracker <- newAccessTracker env.pool 3600
  wsState <- newWSState
  let app = serve (Proxy @HMemAPI) (server Config.defaultConfig.auth env.pool tracker wsState True)
      localSuperadmin = Principal
        { actorType = ActorUser, actorId = "group-event-test", actorLabel = "Group Event Test"
        , authority = PrincipalSyntheticLocalSuperadmin }
  pure $ \req respond -> withPrincipalContext (Just localSuperadmin) (app req respond)

recordingObservationApp :: TestEnv -> IO Application
recordingObservationApp env = do
  tracker <- newAccessTracker env.pool 3600
  wsState <- newWSState
  let app = serve (Proxy @HMemAPI) (server Config.defaultConfig.auth env.pool tracker wsState True)
      localSuperadmin = Principal
        { actorType = ActorUser, actorId = "observation-event-test", actorLabel = "Observation Event Test"
        , authority = PrincipalSyntheticLocalSuperadmin }
  pure $ \req respond -> withPrincipalContext (Just localSuperadmin) (app req respond)

spec :: Spec
spec = around (\example -> withLocalSandboxAppEnv (\env app -> example (env, app))) $ do
  describe "Task dependency HTTP contract" $ do
    it "adds and removes same-workspace dependencies with the frontend paths and payloads" $ \(env, app) -> do
      workspace <- createTestWorkspace env "task-dependency-contract"
      let taskInput title = object ["workspace_id" .= workspace.id, "title" .= (title :: T.Text)]
          taskPath taskId = "/api/v1/tasks/" <> Text.encodeUtf8 (T.pack (show taskId))
          requestHeaders = [("X-Request-Id", "task-dependency-contract")]
      dependentResponse <- postJson app "/api/v1/tasks" (taskInput "dependent")
      dependencyResponse <- postJson app "/api/v1/tasks" (taskInput "dependency")
      let Just dependent = decode (responseBody dependentResponse) :: Maybe Task
          Just dependency = decode (responseBody dependencyResponse) :: Maybe Task
          addPath = taskPath dependent.id <> "/dependencies"
          removePath = addPath <> "/" <> Text.encodeUtf8 (T.pack (show dependency.id))
          requestBody = encode (object ["depends_on_id" .= dependency.id, "request_id" .= ("task-dependency-contract" :: T.Text)])
      added <- requestWithHeaders app methodPost addPath requestHeaders requestBody
      responseStatus added `shouldBe` status200
      let Just addResult = decode (responseBody added) :: Maybe DependencyMutationResult
      addResult.action `shouldBe` "add"
      addResult.taskId `shouldBe` dependent.id
      addResult.dependsOnId `shouldBe` dependency.id
      overview <- request app methodGet (taskPath dependent.id <> "/overview") ""
      let Just dependencyOverview = decode (responseBody overview) :: Maybe TaskOverview
      map (.id) dependencyOverview.dependencies `shouldBe` [dependency.id]
      removed <- requestWithHeaders app methodDelete removePath requestHeaders requestBody
      responseStatus removed `shouldBe` status200
      let Just removeResult = decode (responseBody removed) :: Maybe DependencyMutationResult
      removeResult.action `shouldBe` "remove"
      removeResult.taskId `shouldBe` dependent.id
      removeResult.dependsOnId `shouldBe` dependency.id
      afterRemoval <- request app methodGet (taskPath dependent.id <> "/overview") ""
      let Just removedOverview = decode (responseBody afterRemoval) :: Maybe TaskOverview
      removedOverview.dependencies `shouldBe` []

    it "rejects self, missing, and cross-workspace task dependencies before mutation" $ \(env, app) -> do
      workspace <- createTestWorkspace env "task-dependency-validation"
      otherWorkspace <- createTestWorkspace env "task-dependency-other-workspace"
      let create workspaceId title = postJson app "/api/v1/tasks" (object ["workspace_id" .= workspaceId, "title" .= (title :: T.Text)])
          taskPath taskId = "/api/v1/tasks/" <> Text.encodeUtf8 (T.pack (show taskId))
      dependentResponse <- create workspace.id "dependent"
      otherDependencyResponse <- create otherWorkspace.id "foreign dependency"
      let Just dependent = decode (responseBody dependentResponse) :: Maybe Task
          Just otherDependency = decode (responseBody otherDependencyResponse) :: Maybe Task
          add dependencyId = postJson app (taskPath dependent.id <> "/dependencies") (object ["depends_on_id" .= dependencyId])
      add dependent.id >>= (\response -> responseStatus response `shouldBe` status400)
      add otherDependency.id >>= (\response -> responseStatus response `shouldBe` status400)
      add (read "00000000-0000-0000-0000-000000000001" :: UUID) >>= (\response -> responseStatus response `shouldBe` status404)

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

    it "records group and membership changes in the durable outbox" $ \(env, _app) -> do
      workspace <- createTestWorkspace env "group-event-member"
      app <- recordingGroupApp env
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
      global <- listOutboxAfter env.pool GlobalScope 0 100
      workspaceRecords <- listOutboxAfter env.pool (WorkspaceScope workspace.id) 0 100
      let globalActions = [ jsonPath ["entity", "action"] record.outboxEnvelope
                          | record <- global
                          , jsonPath ["entity", "type"] record.outboxEnvelope == Just (String "workspace_group")
                          , jsonPath ["entity", "id"] record.outboxEnvelope == Just (String (T.pack (show created.id))) ]
          membershipActions = [ jsonPath ["entity", "action"] record.outboxEnvelope
                              | record <- workspaceRecords
                              , jsonPath ["entity", "type"] record.outboxEnvelope == Just (String "workspace_group_membership")
                              , maybe False (T.isPrefixOf (T.pack (show created.id))) (jsonPath ["entity", "id"] record.outboxEnvelope >>= \case String value -> Just value; _ -> Nothing) ]
      globalActions `shouldBe` [Just (String "created"), Just (String "deleted")]
      membershipActions `shouldBe` [Just (String "created"), Just (String "deleted")]

  describe "Canonical change-stream resync" $ do
    it "returns allowlisted snapshot items and an opaque terminal token without a cursor" $ \(env, app) -> do
      workspace <- createTestWorkspace env "canonical-resync"
      let start = object
            [ "scope" .= object ["scope" .= ("workspace" :: T.Text), "workspace_id" .= workspace.id]
            , "page_size" .= (20 :: Int), "start_idempotency_key" .= ("canonical-resync-response-loss-key-0001" :: T.Text) ]
      response <- postJson app "/api/v1/change-stream/resync" start
      responseStatus response `shouldBe` status200
      let Just body = decode (responseBody response) :: Maybe Value
      jsonField "resume_token" body `shouldSatisfy` (/= Nothing)
      jsonField "next_page_token" body `shouldBe` Nothing
      jsonField "cursor" body `shouldBe` Nothing
      -- Retrying a lost start response returns the same materialized first
      -- page/token; the client cannot silently create another snapshot.
      retry <- postJson app "/api/v1/change-stream/resync" start
      responseStatus retry `shouldBe` status200
      responseBody retry `shouldBe` responseBody response
      pageSizeMismatch <- postJson app "/api/v1/change-stream/resync" (object
        [ "scope" .= object ["scope" .= ("workspace" :: T.Text), "workspace_id" .= workspace.id]
        , "page_size" .= (21 :: Int), "start_idempotency_key" .= ("canonical-resync-response-loss-key-0001" :: T.Text) ])
      responseStatus pageSizeMismatch `shouldBe` status409
      weakStartKey <- postJson app "/api/v1/change-stream/resync" (object
        [ "scope" .= object ["scope" .= ("workspace" :: T.Text), "workspace_id" .= workspace.id]
        , "page_size" .= (20 :: Int), "start_idempotency_key" .= ("predictable" :: T.Text) ])
      responseStatus weakStartKey `shouldBe` status400
      -- The public ticket endpoint must accept a successor token, not only
      -- the terminal snapshot token from which it descended. This is the
      -- reconnect path after the server has replayed/rotated a prior page.
      resume <- case jsonField "resume_token" body of
        Just (String value) -> pure (ResumeToken value)
        _ -> expectationFailure "terminal resync omitted resume_token" >> fail "unreachable"
      rotated <- replayAndRotateResumeToken env.pool 60 (WorkspaceScope workspace.id) (TrustedAudience "local:local-user") resume 20
      successor <- case rotated of
        Right ReplayPage { replayPageResumeToken = value } -> pure value
        Left err -> expectationFailure (show err) >> fail "unreachable"
      ticketResponse <- postJson app "/api/v1/change-stream/ticket" (object
        [ "scope" .= object ["scope" .= ("workspace" :: T.Text), "workspace_id" .= workspace.id]
        , "resume_token" .= successor.unResumeToken ])
      responseStatus ticketResponse `shouldBe` status200
      case jsonField "items" body of
        Just (Array items) -> do
          not (null items) `shouldBe` True
          all (\item -> jsonField "schema_version" item == Just (Number 1) && jsonField "data" item /= Nothing) items `shouldBe` True
        _ -> expectationFailure "expected snapshot items"

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
    it "keeps a spoofed MCP header from an ordinary bot PAT as REST in the durable outbox" $ \_ ->
      withDeployedSandboxAppContext $ \ctx -> do
        workspace <- createTestWorkspace ctx.deployedEnv "spoofed-mcp-cause"
        botUserId <- createDeployedSandboxUser ctx.deployedEnv False True
        botToken <- issueDeployedSandboxPAT ctx.deployedEnv botUserId "mcp"
        response <- requestWithHeaders ctx.deployedApplication methodPost "/api/v1/projects"
          [ ("Authorization", "Bearer " <> Text.encodeUtf8 botToken.rawToken)
          , ("X-HMem-Change-Cause", "mcp")
          ]
          (encode (object ["workspace_id" .= workspace.id, "name" .= ("spoofed MCP cause" :: T.Text)]))
        responseStatus response `shouldBe` status200
        records <- listOutboxAfter ctx.deployedEnv.pool (WorkspaceScope workspace.id) 0 10
        case records of
          [record] -> jsonPath ["transaction", "cause"] record.outboxEnvelope `shouldBe` Just (String "rest")
          _ -> expectationFailure "expected exactly one durable project outbox record"
    it "rejects an empty configured MCP provenance secret" $ \_ ->
      withDeployedSandboxAppContext $ \ctx -> do
        tracker <- newAccessTracker ctx.deployedEnv.pool 3600
        wsState <- newWSState
        let cfg = ctx.deployedConfig
            blankAuth = cfg.auth { Config.mcpProvenanceToken = Just " \t " }
        blankApp <- mkApp id blankAuth cfg.cors cfg.rateLimit ctx.deployedEnv.pool tracker wsState cfg.web.webStaticDir True
        workspace <- createTestWorkspace ctx.deployedEnv "blank-mcp-cause"
        botUserId <- createDeployedSandboxUser ctx.deployedEnv False True
        botToken <- issueDeployedSandboxPAT ctx.deployedEnv botUserId "mcp-blank"
        response <- requestWithHeaders blankApp methodPost "/api/v1/projects"
          [ ("Authorization", "Bearer " <> Text.encodeUtf8 botToken.rawToken)
          , ("X-HMem-Change-Cause", "mcp")
          , ("X-HMem-MCP-Provenance", " \t ")
          ]
          (encode (object ["workspace_id" .= workspace.id, "name" .= ("blank MCP cause" :: T.Text)]))
        responseStatus response `shouldBe` status200
        records <- listOutboxAfter ctx.deployedEnv.pool (WorkspaceScope workspace.id) 0 10
        case records of
          [record] -> jsonPath ["transaction", "cause"] record.outboxEnvelope `shouldBe` Just (String "rest")
          _ -> expectationFailure "expected exactly one durable project outbox record"
    it "attributes a server-authenticated MCP proxy mutation as MCP" $ \_ ->
      withDeployedSandboxAppContext $ \ctx -> do
        tracker <- newAccessTracker ctx.deployedEnv.pool 3600
        wsState <- newWSState
        let cfg = ctx.deployedConfig
            trustedAuth = cfg.auth { Config.mcpProvenanceToken = Just "test-mcp-provenance" }
        trustedApp <- mkApp id trustedAuth cfg.cors cfg.rateLimit ctx.deployedEnv.pool tracker wsState cfg.web.webStaticDir True
        workspace <- createTestWorkspace ctx.deployedEnv "trusted-mcp-cause"
        botUserId <- createDeployedSandboxUser ctx.deployedEnv False True
        botToken <- issueDeployedSandboxPAT ctx.deployedEnv botUserId "mcp-proxy"
        response <- requestWithHeaders trustedApp methodPost "/api/v1/projects"
          [ ("Authorization", "Bearer " <> Text.encodeUtf8 botToken.rawToken)
          , ("X-HMem-Change-Cause", "mcp")
          , ("X-HMem-MCP-Provenance", "test-mcp-provenance")
          ]
          (encode (object ["workspace_id" .= workspace.id, "name" .= ("trusted MCP cause" :: T.Text)]))
        responseStatus response `shouldBe` status200
        records <- listOutboxAfter ctx.deployedEnv.pool (WorkspaceScope workspace.id) 0 10
        case records of
          [record] -> jsonPath ["transaction", "cause"] record.outboxEnvelope `shouldBe` Just (String "mcp")
          _ -> expectationFailure "expected exactly one durable MCP project outbox record"
    it "records a project audit revert with the audit_revert cause" $ \(env, app) -> do
      workspace <- createTestWorkspace env "audit-revert-change-cause"
      created <- postJson app "/api/v1/projects" (object ["workspace_id" .= workspace.id, "name" .= ("revert me" :: T.Text)])
      responseStatus created `shouldBe` status200
      audits <- request app methodGet ("/api/v1/audit?workspace_id=" <> Text.encodeUtf8 (T.pack (show workspace.id)) <> "&entity_type=project") ""
      let Just page = decode (responseBody audits) :: Maybe (PaginatedResult AuditLogEntry)
          entry = head page.items
      reverted <- request app methodPost ("/api/v1/audit/" <> Text.encodeUtf8 (T.pack (show entry.id)) <> "/revert") ""
      responseStatus reverted `shouldBe` status200
      records <- listOutboxAfter env.pool (WorkspaceScope workspace.id) 1 10
      case records of
        (record:_) -> jsonPath ["transaction", "cause"] record.outboxEnvelope `shouldBe` Just (String "audit_revert")
        _ -> expectationFailure "expected a durable audit-revert outbox record"
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
      request app methodGet (workspacePath <> "/timeline/buckets?since=2020-01-01T00:00:00Z&until=2021-01-02T00:00:00Z&bucket=day") "" >>= (\response -> responseStatus response `shouldBe` status400)
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
          Just rawBuckets = decode (responseBody bucketsResponse) :: Maybe Value
          seriesActions entity = firstJsonArrayValue (jsonField "buckets" rawBuckets) >>= jsonPath ["series", entity]
          hasActions entity = all (\action -> isJust (seriesActions entity >>= jsonField action)) ["created", "completed", "deleted"]
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
      totals (\bucket -> bucket.timelineBucketSeries.seriesProject.created) `shouldBe` 2
      totals (\bucket -> bucket.timelineBucketSeries.seriesProject.completed) `shouldBe` 0
      totals (\bucket -> bucket.timelineBucketSeries.seriesTask.created) `shouldBe` 3
      totals (\bucket -> bucket.timelineBucketSeries.seriesTask.completed) `shouldBe` 1
      totals (\bucket -> bucket.timelineBucketSeries.seriesSubtask.created) `shouldBe` 1
      totals (\bucket -> bucket.timelineBucketSeries.seriesSubtask.completed) `shouldBe` 1
      totals (\bucket -> bucket.timelineBucketSeries.seriesObservation.completed) `shouldBe` 0
      totals (\bucket -> bucket.timelineBucketSeriesTotals.created) `shouldBe` 6
      totals (\bucket -> bucket.timelineBucketSeriesTotals.completed) `shouldBe` 2
      mapM_ (\entity -> hasActions entity `shouldBe` True) ["project", "task", "subtask", "observation"]
      (firstJsonArrayValue (jsonField "buckets" rawBuckets) >>= jsonField "counts") `shouldSatisfy` isJust
      (firstJsonArrayValue (jsonField "buckets" rawBuckets) >>= jsonField "totals") `shouldSatisfy` isJust
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
    it "preserves legacy singleton creates and exposes canonical multi-subject observations" $ \(env, app) -> do
      workspace <- createTestWorkspace env "observation-api"
      legacyResponse <- postJson app "/api/v1/observations" (object
        [ "workspace_id" .= workspace.id, "subject_kind" .= ("file" :: T.Text)
        , "subject" .= ("src/Legacy.hs" :: T.Text), "git_sha" .= ("0123456789abcdef0123456789abcdef01234567" :: T.Text)
        , "content" .= ("legacy observation" :: T.Text) ])
      responseStatus legacyResponse `shouldBe` status200
      let Just legacy = decode (responseBody legacyResponse) :: Maybe Observation
      legacy.subjects `shouldBe` [ObservationSubject SubjectFile "src/Legacy.hs"]
      createdResponse <- postJson app "/api/v1/observations" (object
        [ "workspace_id" .= workspace.id
        , "subjects" .= [ ObservationSubject SubjectFile "src/Main.hs"
                          , ObservationSubject SubjectGlob "src/**/*.hs"
                          , ObservationSubject SubjectFile "src/Main.hs" ]
        , "git_sha" .= ("0123456789abcdef0123456789abcdef01234567" :: T.Text)
        , "content" .= ("initial observation" :: T.Text) ])
      responseStatus createdResponse `shouldBe` status200
      let Just created = decode (responseBody createdResponse) :: Maybe Observation
      created.subjects `shouldBe`
        [ ObservationSubject SubjectFile "src/Main.hs", ObservationSubject SubjectGlob "src/**/*.hs" ]
      let Just createdJson = decode (responseBody createdResponse) :: Maybe Value
      jsonField "subject_kind" createdJson `shouldBe` Just (String "file")
      jsonField "subject" createdJson `shouldBe` Just (String "src/Main.hs")
      listed <- request app methodGet ("/api/v1/observations?workspace_id=" <> Text.encodeUtf8 (T.pack (show workspace.id)) <> "&subject_kind=file&subject=src/Main.hs") ""
      responseStatus listed `shouldBe` status200
      let Just page = decode (responseBody listed) :: Maybe (PaginatedResult Observation)
      page.items `shouldBe` [created]
      byGlob <- request app methodGet ("/api/v1/observations?workspace_id=" <> Text.encodeUtf8 (T.pack (show workspace.id)) <> "&subject_kind=glob&subject=src/**/*.hs") ""
      let Just globPage = decode (responseBody byGlob) :: Maybe (PaginatedResult Observation)
      globPage.items `shouldBe` [created]
      maximumPage <- request app methodGet ("/api/v1/observations?workspace_id=" <> Text.encodeUtf8 (T.pack (show workspace.id)) <> "&limit=200") ""
      responseStatus maximumPage `shouldBe` status200
      mapM_ (\suffix -> request app methodGet ("/api/v1/observations?workspace_id=" <> Text.encodeUtf8 (T.pack (show workspace.id)) <> suffix) "" >>= (\response -> responseStatus response `shouldBe` status400))
        ["&limit=0", "&limit=201", "&offset=-1", "&offset=100001"]
      let sha = ("0123456789abcdef0123456789abcdef01234567" :: T.Text)
          subjectCountOverflow = [ObservationSubject SubjectFile ("src/Count" <> T.pack (show index) <> ".hs") | index <- [1 .. 257 :: Int]]
          subjectByteOverflow = [ObservationSubject SubjectFile ("src/" <> T.replicate 4090 "a" <> T.pack (show index)) | index <- [1 .. 65 :: Int]]
          invalidCreates =
            [ object [ "workspace_id" .= workspace.id, "git_sha" .= sha, "content" .= ("missing subjects" :: T.Text) ]
            , object [ "workspace_id" .= workspace.id, "subject_kind" .= ("file" :: T.Text), "git_sha" .= sha, "content" .= ("half legacy kind" :: T.Text) ]
            , object [ "workspace_id" .= workspace.id, "subject" .= ("src/Half.hs" :: T.Text), "git_sha" .= sha, "content" .= ("half legacy subject" :: T.Text) ]
            , object [ "workspace_id" .= workspace.id, "subjects" .= ([] :: [ObservationSubject]), "git_sha" .= sha, "content" .= ("empty" :: T.Text) ]
            , object [ "workspace_id" .= workspace.id, "subjects" .= [ObservationSubject SubjectFile "src/Mixed.hs"]
                     , "subject_kind" .= ("file" :: T.Text), "subject" .= ("src/Other.hs" :: T.Text), "git_sha" .= sha, "content" .= ("mixed" :: T.Text) ]
            , object [ "workspace_id" .= workspace.id, "subjects" .= [ObservationSubject SubjectFile "../outside.hs"], "git_sha" .= sha, "content" .= ("bad path" :: T.Text) ]
            , object [ "workspace_id" .= workspace.id, "subjects" .= [ObservationSubject SubjectGlob "src/**bad/*.hs"], "git_sha" .= sha, "content" .= ("bad glob" :: T.Text) ]
            , object [ "workspace_id" .= workspace.id, "subjects" .= subjectCountOverflow, "git_sha" .= sha, "content" .= ("too many subjects" :: T.Text) ]
            , object [ "workspace_id" .= workspace.id, "subjects" .= subjectByteOverflow, "git_sha" .= sha, "content" .= ("too many subject bytes" :: T.Text) ]
            ]
      forM_ invalidCreates $ \input -> do
        rejected <- postJson app "/api/v1/observations" input
        responseStatus rejected `shouldBe` status400
        let Just rejection = decode (responseBody rejected) :: Maybe Value
        jsonField "error" rejection `shouldBe` Just (String "validation_error")
        jsonField "message" rejection `shouldSatisfy` maybe False (\value -> case value of String message -> not (T.null message); _ -> False)
      searchResponse <- postJson app "/api/v1/search" (object
        [ "workspace_id" .= workspace.id, "query" .= ("initial" :: T.Text), "entity_types" .= ["observation" :: T.Text] ])
      responseStatus searchResponse `shouldBe` status200
      let Just unified = decode (responseBody searchResponse) :: Maybe UnifiedSearchResults
      map (.id) unified.observations `shouldBe` [created.id]
      let Just searchJson = decode (responseBody searchResponse) :: Maybe Value
          Just searchHit = firstJsonArrayValue (jsonField "observations" searchJson)
      jsonField "subjects" searchHit `shouldBe` Just (toJSON created.subjects)
      jsonField "subject_kind" searchHit `shouldBe` Just (String "file")
      jsonField "subject" searchHit `shouldBe` Just (String "src/Main.hs")
      let vector = (1 : replicate (observationEmbeddingDimensions - 1) 0) :: [Double]
          observationPath = "/api/v1/observations/" <> Text.encodeUtf8 (T.pack (show created.id))
      embeddingResponse <- request app methodPut (observationPath <> "/embedding") (encode vector)
      case responseStatus embeddingResponse of
        status | status == status503 -> pure ()
        status -> do
          status `shouldBe` status200
          similarResponse <- postJson app "/api/v1/observations/similar" (object
            [ "workspace_id" .= workspace.id, "embedding" .= vector ])
          responseStatus similarResponse `shouldBe` status200
          let Just similar = decode (responseBody similarResponse) :: Maybe [SimilarObservation]
          map (.observation.id) similar `shouldBe` [created.id]
          let Just similarJson = decode (responseBody similarResponse) :: Maybe Value
              Just similarHit = firstJsonArrayValue (Just similarJson)
              Just similarObservationJson = jsonField "observation" similarHit
          jsonField "subjects" similarObservationJson `shouldBe` Just (toJSON created.subjects)
          jsonField "subject_kind" similarObservationJson `shouldBe` Just (String "file")
          jsonField "subject" similarObservationJson `shouldBe` Just (String "src/Main.hs")
      updated <- request app methodPut ("/api/v1/observations/" <> Text.encodeUtf8 (T.pack (show created.id))) (encode (object ["content" .= ("revised observation" :: T.Text)]))
      responseStatus updated `shouldBe` status200
      let Just revised = decode (responseBody updated) :: Maybe Observation
      revised.content `shouldBe` "revised observation"
      revised.subjects `shouldBe` created.subjects
      immutable <- request app methodPut ("/api/v1/observations/" <> Text.encodeUtf8 (T.pack (show created.id))) (encode (object ["content" .= ("x" :: T.Text), "git_sha" .= ("different" :: T.Text)]))
      responseStatus immutable `shouldBe` status400 -- Servant rejects non-contract request bodies before routing the handler.
      deleted <- request app methodDelete ("/api/v1/observations/" <> Text.encodeUtf8 (T.pack (show created.id))) ""
      responseStatus deleted `shouldBe` status200
      missing <- request app methodGet ("/api/v1/observations/" <> Text.encodeUtf8 (T.pack (show created.id))) ""
      responseStatus missing `shouldBe` status404

    it "records Observation changes without projecting handler-local payloads" $ \(env, _app) -> do
      workspace <- createTestWorkspace env "observation-event"
      app <- recordingObservationApp env
      let subjectsValue = [ObservationSubject SubjectFile "src/Event.hs", ObservationSubject SubjectGlob "src/**/*.hs"]
      response <- postJson app "/api/v1/observations" (object
        [ "workspace_id" .= workspace.id, "subjects" .= subjectsValue
        , "git_sha" .= ("0123456789abcdef0123456789abcdef01234567" :: T.Text), "content" .= ("event observation" :: T.Text) ])
      responseStatus response `shouldBe` status200
      let Just observation = decode (responseBody response) :: Maybe Observation
      records <- listOutboxAfter env.pool (WorkspaceScope workspace.id) 0 100
      let observed = [ record | record <- records
                      , jsonPath ["entity", "type"] record.outboxEnvelope == Just (String "observation")
                      , jsonPath ["entity", "id"] record.outboxEnvelope == Just (String (T.pack (show observation.id))) ]
      length observed `shouldBe` 1
      jsonPath ["payload"] (head observed).outboxEnvelope `shouldBe` Nothing

    it "lists deterministic workspace-isolated subject facets with full filtered counts" $ \(env, app) -> do
      workspace <- createTestWorkspace env "observation-facets-api"
      otherWorkspace <- createTestWorkspace env "observation-facets-api-other"
      let sha = ("0123456789abcdef0123456789abcdef01234567" :: T.Text)
          alternate = ("fedcba9876543210fedcba9876543210fedcba98" :: T.Text)
          input workspaceId shaValue body subjectsValue = object
            [ "workspace_id" .= workspaceId, "subjects" .= subjectsValue
            , "git_sha" .= shaValue, "content" .= (body :: T.Text)
            ]
      _ <- postJson app "/api/v1/observations" (input workspace.id sha "selected first"
        [ObservationSubject SubjectFile "src/Shared.hs", ObservationSubject SubjectGlob "src/**/*.hs"])
      _ <- postJson app "/api/v1/observations" (input workspace.id alternate "selected second"
        [ObservationSubject SubjectFile "src/Shared.hs", ObservationSubject SubjectFile "src/Unique.hs"])
      _ <- postJson app "/api/v1/observations" (input otherWorkspace.id sha "isolated"
        [ObservationSubject SubjectFile "src/Shared.hs"])
      let base = "/api/v1/observations/subject-facets?workspace_id=" <> Text.encodeUtf8 (T.pack (show workspace.id))
      firstPageResponse <- request app methodGet (base <> "&limit=1") ""
      responseStatus firstPageResponse `shouldBe` status200
      let Just firstPage = decode (responseBody firstPageResponse) :: Maybe (PaginatedResult ObservationSubjectFacet)
      firstPage.hasMore `shouldBe` True
      map (\facet -> (facet.subjectKind, facet.subject, facet.observationCount)) firstPage.items `shouldBe`
        [(SubjectFile, "src/Shared.hs", 2)]
      secondPageResponse <- request app methodGet (base <> "&limit=1&offset=1") ""
      let Just secondPage = decode (responseBody secondPageResponse) :: Maybe (PaginatedResult ObservationSubjectFacet)
      length secondPage.items `shouldBe` 1
      globResponse <- request app methodGet (base <> "&subject_kind=glob") ""
      let Just globPage = decode (responseBody globResponse) :: Maybe (PaginatedResult ObservationSubjectFacet)
      map (\facet -> (facet.subjectKind, facet.subject, facet.observationCount)) globPage.items `shouldBe`
        [(SubjectGlob, "src/**/*.hs", 1)]
      shaResponse <- request app methodGet (base <> "&git_sha=" <> Text.encodeUtf8 alternate) ""
      let Just shaPage = decode (responseBody shaResponse) :: Maybe (PaginatedResult ObservationSubjectFacet)
      map (.observationCount) shaPage.items `shouldBe` [1, 1]
      queryResponse <- request app methodGet (base <> "&query=selected") ""
      let Just queryPage = decode (responseBody queryResponse) :: Maybe (PaginatedResult ObservationSubjectFacet)
      map (\facet -> (facet.subject, facet.observationCount)) queryPage.items `shouldContain` [("src/Shared.hs", 2)]
      request app methodGet "/api/v1/observations/subject-facets" "" >>= (\response -> responseStatus response `shouldBe` status400)
      mapM_ (\suffix -> request app methodGet (base <> suffix) "" >>= (\response -> responseStatus response `shouldBe` status400))
        ["&limit=0", "&limit=201", "&offset=-1", "&offset=100001", "&git_sha=not-a-sha"]

    it "updates facet query membership and recency, then removes hard-deleted groups" $ \(env, app) -> do
      workspace <- createTestWorkspace env "observation-facets-mutations-api"
      let sha = ("0123456789abcdef0123456789abcdef01234567" :: T.Text)
          input body subjectsValue = object
            [ "workspace_id" .= workspace.id, "subjects" .= subjectsValue
            , "git_sha" .= sha, "content" .= (body :: T.Text)
            ]
          base = "/api/v1/observations/subject-facets?workspace_id=" <> Text.encodeUtf8 (T.pack (show workspace.id))
          observationPath observationId = "/api/v1/observations/" <> Text.encodeUtf8 (T.pack (show observationId))
      stableResponse <- postJson app "/api/v1/observations" (input "needle stable"
        [ ObservationSubject SubjectFile "src/Shared.hs"
        , ObservationSubject SubjectFile "aaa-stable-only.hs"
        ])
      movingResponse <- postJson app "/api/v1/observations" (input "ordinary"
        [ ObservationSubject SubjectFile "src/Shared.hs"
        , ObservationSubject SubjectFile "zzz-moving-only.hs"
        ])
      let Just stable = decode (responseBody stableResponse) :: Maybe Observation
          Just moving = decode (responseBody movingResponse) :: Maybe Observation
      backdateObservation env stable.id
      backdatedResponse <- request app methodGet (observationPath stable.id) ""
      let Just backdatedStable = decode (responseBody backdatedResponse) :: Maybe Observation
      beforeResponse <- request app methodGet (base <> "&query=needle") ""
      let Just facetsBefore = decode (responseBody beforeResponse) :: Maybe (PaginatedResult ObservationSubjectFacet)
      map (.subject) facetsBefore.items `shouldBe` ["aaa-stable-only.hs", "src/Shared.hs"]
      updatedResponse <- request app methodPut (observationPath moving.id)
        (encode (object ["content" .= ("needle revised" :: T.Text)]))
      responseStatus updatedResponse `shouldBe` status200
      let Just updated = decode (responseBody updatedResponse) :: Maybe Observation
      updated.updatedAt `shouldSatisfy` (> backdatedStable.updatedAt)
      afterUpdateResponse <- request app methodGet (base <> "&query=needle") ""
      let Just afterUpdate = decode (responseBody afterUpdateResponse) :: Maybe (PaginatedResult ObservationSubjectFacet)
      map (\facet -> (facet.subject, facet.observationCount)) afterUpdate.items `shouldBe`
        [("src/Shared.hs", 2), ("zzz-moving-only.hs", 1), ("aaa-stable-only.hs", 1)]
      fmap (.latestUpdatedAt) (find ((== "src/Shared.hs") . (.subject)) afterUpdate.items) `shouldBe` Just updated.updatedAt
      fmap (.latestUpdatedAt) (find ((== "zzz-moving-only.hs") . (.subject)) afterUpdate.items) `shouldBe` Just updated.updatedAt
      fmap (.latestUpdatedAt) (find ((== "aaa-stable-only.hs") . (.subject)) afterUpdate.items) `shouldBe` Just backdatedStable.updatedAt
      request app methodDelete (observationPath moving.id) "" >>= (\response -> responseStatus response `shouldBe` status200)
      afterFirstDeleteResponse <- request app methodGet (base <> "&query=needle") ""
      let Just afterFirstDelete = decode (responseBody afterFirstDeleteResponse) :: Maybe (PaginatedResult ObservationSubjectFacet)
      map (\facet -> (facet.subject, facet.observationCount)) afterFirstDelete.items `shouldBe`
        [("aaa-stable-only.hs", 1), ("src/Shared.hs", 1)]
      request app methodDelete (observationPath stable.id) "" >>= (\response -> responseStatus response `shouldBe` status200)
      afterFinalDeleteResponse <- request app methodGet (base <> "&query=needle") ""
      let Just afterFinalDelete = decode (responseBody afterFinalDeleteResponse) :: Maybe (PaginatedResult ObservationSubjectFacet)
      afterFinalDelete.items `shouldBe` []

    it "matches concrete paths with subject evidence, filters, pagination, and workspace isolation" $ \(env, app) -> do
      workspace <- createTestWorkspace env "observation-match"
      otherWorkspace <- createTestWorkspace env "observation-match-other"
      let sha = ("0123456789abcdef0123456789abcdef01234567" :: T.Text)
          observationInput workspaceId value subjectsValue = object
            [ "workspace_id" .= workspaceId, "subjects" .= subjectsValue, "git_sha" .= sha, "content" .= (value :: T.Text) ]
          matchInput workspaceId requested = object ["workspace_id" .= workspaceId, "paths" .= (requested :: [T.Text])]
      firstResponse <- postJson app "/api/v1/observations" (observationInput workspace.id "first matching observation"
        [ObservationSubject SubjectFile "src/Main.hs", ObservationSubject SubjectGlob "src/**/*.hs"])
      secondResponse <- postJson app "/api/v1/observations" (observationInput workspace.id "second matching observation"
        [ObservationSubject SubjectGlob "lib/**/*.hs"])
      _ <- postJson app "/api/v1/observations" (observationInput otherWorkspace.id "isolated observation"
        [ObservationSubject SubjectGlob "src/**/*.hs"])
      let Just first = decode (responseBody firstResponse) :: Maybe Observation
          Just second = decode (responseBody secondResponse) :: Maybe Observation
      matchedResponse <- postJson app "/api/v1/observations/match" (matchInput workspace.id ["src/Main.hs", "src/Other.hs", "lib/HMem/Thing.hs"])
      responseStatus matchedResponse `shouldBe` status200
      let Just matched = decode (responseBody matchedResponse) :: Maybe (PaginatedResult ObservationMatch)
          findMatch observationId = filter (\item -> item.observation.id == observationId) matched.items
      length matched.items `shouldBe` 2
      let [firstMatch] = findMatch first.id
      firstMatch.pathMatches `shouldBe`
        [ ObservationPathMatch "src/Main.hs"
            [ObservationSubject SubjectFile "src/Main.hs", ObservationSubject SubjectGlob "src/**/*.hs"]
        , ObservationPathMatch "src/Other.hs" [ObservationSubject SubjectGlob "src/**/*.hs"]
        ]
      firstMatch.matchedPaths `shouldBe` ["src/Main.hs", "src/Other.hs"]
      firstMatch.matchedSubjects `shouldBe`
        [ObservationSubject SubjectFile "src/Main.hs", ObservationSubject SubjectGlob "src/**/*.hs"]
      let [secondMatch] = findMatch second.id
      secondMatch.pathMatches `shouldBe`
        [ObservationPathMatch "lib/HMem/Thing.hs" [ObservationSubject SubjectGlob "lib/**/*.hs"]]
      secondMatch.matchedPaths `shouldBe` ["lib/HMem/Thing.hs"]
      secondMatch.matchedSubjects `shouldBe` [ObservationSubject SubjectGlob "lib/**/*.hs"]
      globOnly <- postJson app "/api/v1/observations/match" (object
        [ "workspace_id" .= workspace.id, "paths" .= (["src/Main.hs"] :: [T.Text]), "subject_kind" .= ("glob" :: T.Text) ])
      let Just globMatches = decode (responseBody globOnly) :: Maybe (PaginatedResult ObservationMatch)
      map (.observation.id) globMatches.items `shouldBe` [first.id]
      filtered <- postJson app "/api/v1/observations/match" (object
        [ "workspace_id" .= workspace.id, "paths" .= (["src/Main.hs"] :: [T.Text]), "git_sha" .= sha, "query" .= ("first" :: T.Text) ])
      let Just filteredMatches = decode (responseBody filtered) :: Maybe (PaginatedResult ObservationMatch)
      map (.observation.id) filteredMatches.items `shouldBe` [first.id]
      nonmatchingSha <- postJson app "/api/v1/observations/match" (object
        [ "workspace_id" .= workspace.id, "paths" .= (["src/Main.hs"] :: [T.Text]), "git_sha" .= ("fedcba9876543210fedcba9876543210fedcba98" :: T.Text) ])
      responseStatus nonmatchingSha `shouldBe` status200
      let Just nonmatchingShaMatches = decode (responseBody nonmatchingSha) :: Maybe (PaginatedResult ObservationMatch)
      nonmatchingShaMatches.items `shouldBe` []
      firstPage <- postJson app "/api/v1/observations/match" (object
        [ "workspace_id" .= workspace.id, "paths" .= (["src/Main.hs", "lib/HMem/Thing.hs"] :: [T.Text]), "limit" .= (1 :: Int) ])
      let Just paged = decode (responseBody firstPage) :: Maybe (PaginatedResult ObservationMatch)
      length paged.items `shouldBe` 1
      paged.hasMore `shouldBe` True
      secondPage <- postJson app "/api/v1/observations/match" (object
        [ "workspace_id" .= workspace.id, "paths" .= (["src/Main.hs", "lib/HMem/Thing.hs"] :: [T.Text]), "limit" .= (1 :: Int), "offset" .= (1 :: Int) ])
      let Just secondPaged = decode (responseBody secondPage) :: Maybe (PaginatedResult ObservationMatch)
      secondPaged.hasMore `shouldBe` False
      map (.observation.id) (paged.items <> secondPaged.items) `shouldBe` map (.observation.id) matched.items
      let pathCountOverflow = ["src/Path" <> T.pack (show index) <> ".hs" | index <- [1 .. 257 :: Int]]
          pathByteOverflow = ["src/" <> T.replicate 4090 "a" <> T.pack (show index) | index <- [1 .. 65 :: Int]]
      let invalidMatches =
            [ object ["workspace_id" .= workspace.id]
            , matchInput workspace.id ([] :: [T.Text])
            , matchInput workspace.id ["src/*.hs"]
            , object ["workspace_id" .= workspace.id, "paths" .= (["src/Main.hs"] :: [T.Text]), "limit" .= (201 :: Int)]
            , object ["workspace_id" .= workspace.id, "paths" .= pathCountOverflow]
            , object ["workspace_id" .= workspace.id, "paths" .= pathByteOverflow] ]
      forM_ invalidMatches $ \input -> do
        rejected <- postJson app "/api/v1/observations/match" input
        responseStatus rejected `shouldBe` status400
        let Just rejection = decode (responseBody rejected) :: Maybe Value
        jsonField "error" rejection `shouldBe` Just (String "validation_error")
        jsonField "message" rejection `shouldSatisfy` maybe False (\value -> case value of String message -> not (T.null message); _ -> False)

    it "authorizes Observation matching and subject facets as repository read operations" $ \_ ->
      withDeployedSandboxAppContext $ \ctx -> do
        workspace <- createTestWorkspace ctx.deployedEnv "observation-match-auth"
        readerId <- createDeployedSandboxUser ctx.deployedEnv False False
        outsiderId <- createDeployedSandboxUser ctx.deployedEnv False False
        superadminId <- createDeployedSandboxUser ctx.deployedEnv False True
        _ <- Auth.upsertWorkspaceMembership ctx.deployedEnv.pool workspace.id
          (Auth.UpsertWorkspaceMembership readerId Auth.WorkspaceRoleRead) Nothing
        readerToken <- issueDeployedSandboxPAT ctx.deployedEnv readerId "Observation match reader"
        outsiderToken <- issueDeployedSandboxPAT ctx.deployedEnv outsiderId "Observation match outsider"
        superadminToken <- issueDeployedSandboxPAT ctx.deployedEnv superadminId "Observation match superadmin"
        let authHeader token = [("Authorization", "Bearer " <> Text.encodeUtf8 token.rawToken)]
            input = object
              [ "workspace_id" .= workspace.id, "subjects" .= [ObservationSubject SubjectGlob "src/**/*.hs"]
              , "git_sha" .= ("0123456789abcdef0123456789abcdef01234567" :: T.Text), "content" .= ("auth observation" :: T.Text) ]
            matchInput = encode (object ["workspace_id" .= workspace.id, "paths" .= (["src/Main.hs"] :: [T.Text])])
            facetsPath = "/api/v1/observations/subject-facets?workspace_id=" <> Text.encodeUtf8 (T.pack (show workspace.id))
        created <- requestWithHeaders ctx.deployedApplication methodPost "/api/v1/observations" (authHeader superadminToken) (encode input)
        responseStatus created `shouldBe` status200
        request ctx.deployedApplication methodPost "/api/v1/observations/match" matchInput >>= (\response -> responseStatus response `shouldBe` status401)
        requestWithHeaders ctx.deployedApplication methodPost "/api/v1/observations/match" (authHeader outsiderToken) matchInput >>= (\response -> responseStatus response `shouldBe` status403)
        requestWithHeaders ctx.deployedApplication methodPost "/api/v1/observations/match" (authHeader readerToken) matchInput >>= (\response -> responseStatus response `shouldBe` status200)
        request ctx.deployedApplication methodGet facetsPath "" >>= (\response -> responseStatus response `shouldBe` status401)
        requestWithHeaders ctx.deployedApplication methodGet facetsPath (authHeader outsiderToken) "" >>= (\response -> responseStatus response `shouldBe` status403)
        requestWithHeaders ctx.deployedApplication methodGet facetsPath (authHeader readerToken) "" >>= (\response -> responseStatus response `shouldBe` status200)

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
      request app methodGet ("/api/v1/observations/subject-facets?workspace_id=" <> planningId) "" >>= rejected
      postJson app "/api/v1/observations/match" (object ["workspace_id" .= planning.id, "paths" .= (["src/Scope.hs"] :: [T.Text])]) >>= rejected
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
      request app methodGet ("/api/v1/observations/subject-facets?workspace_id=" <> repositoryId) "" >>= rejected
      postJson app "/api/v1/observations" (observationInput repository.id) >>= rejected
      postJson app "/api/v1/observations/match" (object ["workspace_id" .= repository.id, "paths" .= (["src/Scope.hs"] :: [T.Text])]) >>= rejected
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
        [ "workspace_id" .= workspace.id
        , "subjects" .= [ObservationSubject SubjectFile "src/Audit.hs", ObservationSubject SubjectGlob "src/**/*.hs"]
        , "git_sha" .= ("0123456789abcdef0123456789abcdef01234567" :: T.Text)
        , "content" .= ("audit me" :: T.Text) ])
      responseStatus response `shouldBe` status200
      let Just observation = decode (responseBody response) :: Maybe Observation
      audits <- request app methodGet ("/api/v1/audit?workspace_id=" <> Text.encodeUtf8 (T.pack (show workspace.id)) <> "&entity_type=observation") ""
      responseStatus audits `shouldBe` status200
      let Just auditPage = decode (responseBody audits) :: Maybe (PaginatedResult AuditLogEntry)
      auditPage.items `shouldSatisfy` (not . null)
      let entry = head auditPage.items
      entry.entityType `shouldBe` "observation"
      let Just createdSnapshot = entry.newValues
      jsonField "subjects" createdSnapshot `shouldBe` Just (toJSON
        [ObservationSubject SubjectFile "src/Audit.hs", ObservationSubject SubjectGlob "src/**/*.hs"])
      jsonField "subject_kind" createdSnapshot `shouldBe` Just (String "file")
      jsonField "subject" createdSnapshot `shouldBe` Just (String "src/Audit.hs")
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
          operationDescription path method = paths >>= jsonField path >>= jsonField method >>= jsonField "description"
          hasObservationTag path method = jsonStrings (operationTags path method) == Just ["Observations"]
          hasSchemaProperty schemaName propertyName = isJust (schema schemaName >>= jsonField "properties" >>= jsonField propertyName)
          requiredSchemaFields schemaName = jsonStrings (schema schemaName >>= jsonField "required")
          hasCreateVariantProperty propertyName = case schema "CreateObservation" >>= jsonField "oneOf" of
            Just (Array variants) -> any (\variant -> isJust (jsonField "properties" variant >>= jsonField propertyName)) variants
            _ -> False
          createVariantRequired propertyName = case schema "CreateObservation" >>= jsonField "oneOf" of
            Just (Array variants) -> any (\variant -> maybe False (elem propertyName) (jsonStrings (jsonField "required" variant))) variants
            _ -> False
          deprecatedProperty schemaName propertyName = schema schemaName >>= jsonField "properties" >>= jsonField propertyName >>= jsonField "deprecated"
          schemaDescription schemaName = schema schemaName >>= jsonField "description"
          matchPathsSchema = schema "ObservationMatchQuery" >>= jsonField "properties" >>= jsonField "paths"
          pathParameter path method parameterName = do
            Array parameters <- paths >>= jsonField path >>= jsonField method >>= jsonField "parameters"
            find (\parameter -> jsonField "name" parameter == Just (String parameterName)) (toList parameters)
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
        , "/api/v1/observations/subject-facets"
        , "/api/v1/observations/match"
        , "/api/v1/observations/similar"
        , "/api/v1/observations/{observationId}"
         , "/api/v1/observations/{observationId}/embedding"
         , "/api/v1/tasks/{taskId}/dependencies"
         , "/api/v1/tasks/{taskId}/dependencies/{dependsOnId}"
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
        , ("/api/v1/observations/subject-facets", "get")
        , ("/api/v1/observations/match", "post")
        , ("/api/v1/observations/similar", "post")
        , ("/api/v1/observations/{observationId}", "get")
        , ("/api/v1/observations/{observationId}", "put")
        , ("/api/v1/observations/{observationId}", "delete")
        , ("/api/v1/observations/{observationId}/embedding", "put") ]
      enum "SubjectKind" `shouldBe` Just ["file", "glob"]
      enum "EntitySearchType" `shouldBe` Just ["observation", "project", "task"]
      schema "ObservationSubject" `shouldSatisfy` isJust
      schema "ObservationSubjectFacet" `shouldSatisfy` isJust
      schema "ObservationMatchQuery" `shouldSatisfy` isJust
      schema "ObservationPathMatch" `shouldSatisfy` isJust
      schema "ObservationMatch" `shouldSatisfy` isJust
      schema "LinkDependency" `shouldSatisfy` isJust
      schema "DependencyMutationResult" `shouldSatisfy` isJust
      hasSchemaProperty "Observation" "subjects" `shouldBe` True
      hasSchemaProperty "Observation" "subject_kind" `shouldBe` True
      hasSchemaProperty "Observation" "subject" `shouldBe` True
      hasSchemaProperty "ObservationSearchHit" "subjects" `shouldBe` True
      hasSchemaProperty "ObservationSearchHit" "subject_kind" `shouldBe` True
      hasSchemaProperty "ObservationSearchHit" "subject" `shouldBe` True
      hasCreateVariantProperty "subjects" `shouldBe` True
      hasCreateVariantProperty "subject_kind" `shouldBe` True
      hasCreateVariantProperty "subject" `shouldBe` True
      createVariantRequired "subjects" `shouldBe` True
      createVariantRequired "subject_kind" `shouldBe` True
      createVariantRequired "subject" `shouldBe` True
      case schema "CreateObservation" >>= jsonField "oneOf" of
        Just (Array variants) -> do
          let branches = toList variants
              branchRequired branch = jsonStrings (jsonField "required" branch)
              canonical = filter (maybe False (elem "subjects") . branchRequired) branches
              legacy = filter (maybe False (elem "subject_kind") . branchRequired) branches
              hasProperty branch propertyName = isJust (jsonField "properties" branch >>= jsonField propertyName)
              isClosed branch = jsonField "additionalProperties" branch == Just (Bool False)
          length branches `shouldBe` 2
          length canonical `shouldBe` 1
          length legacy `shouldBe` 1
          let [canonicalBranch] = canonical
              [legacyBranch] = legacy
          branchRequired canonicalBranch `shouldBe` Just ["workspace_id", "subjects", "git_sha", "content"]
          branchRequired legacyBranch `shouldBe` Just ["workspace_id", "subject_kind", "subject", "git_sha", "content"]
          hasProperty canonicalBranch "subjects" `shouldBe` True
          hasProperty canonicalBranch "subject_kind" `shouldBe` False
          hasProperty canonicalBranch "subject" `shouldBe` False
          isClosed canonicalBranch `shouldBe` True
          hasProperty legacyBranch "subjects" `shouldBe` False
          hasProperty legacyBranch "subject_kind" `shouldBe` True
          hasProperty legacyBranch "subject" `shouldBe` True
          (jsonField "properties" legacyBranch >>= jsonField "subject_kind" >>= jsonField "deprecated") `shouldBe` Just (Bool True)
          (jsonField "properties" legacyBranch >>= jsonField "subject" >>= jsonField "deprecated") `shouldBe` Just (Bool True)
          isClosed legacyBranch `shouldBe` True
        _ -> expectationFailure "CreateObservation must use a two-form oneOf"
      deprecatedProperty "Observation" "subject_kind" `shouldBe` Just (Bool True)
      deprecatedProperty "Observation" "subject" `shouldBe` Just (Bool True)
      deprecatedProperty "ObservationSearchHit" "subject_kind" `shouldBe` Just (Bool True)
      deprecatedProperty "ObservationSearchHit" "subject" `shouldBe` Just (Bool True)
      mapM_ (\propertyName -> hasSchemaProperty "ObservationSubjectFacet" propertyName `shouldBe` True)
        ["subject_kind", "subject", "observation_count", "latest_updated_at"]
      hasSchemaProperty "ObservationMatch" "path_matches" `shouldBe` True
      deprecatedProperty "ObservationMatch" "matched_paths" `shouldBe` Just (Bool True)
      deprecatedProperty "ObservationMatch" "matched_subjects" `shouldBe` Just (Bool True)
      let facetParameter = pathParameter "/api/v1/observations/subject-facets" "get"
          observationParameter = pathParameter "/api/v1/observations" "get"
      mapM_ (\parameterName -> facetParameter parameterName `shouldSatisfy` isJust)
        ["workspace_id", "subject_kind", "git_sha", "query", "limit", "offset"]
      (facetParameter "workspace_id" >>= jsonField "required") `shouldBe` Just (Bool True)
      observationParameter "query" `shouldSatisfy` isJust
      operationDescription "/api/v1/observations" "get" `shouldBe`
        Just (String "Lists Observations from an active repository workspace after repository read authorization. The optional full-text query searches Observation content and all stored subject text. Exact subject_kind and subject filters must match the same stored subject row. Results rank by text relevance when query is set, then updated_at DESC and id DESC. Pagination defaults to limit 50 and offset 0; limit is 1..200 and offset is 0..100000.")
      operationDescription "/api/v1/observations/subject-facets" "get" `shouldBe`
        Just (String "Requires repository read authorization for the requested active repository workspace. Each facet is identified by the exact (subject_kind, subject) tuple. Groups stored subjects after workspace, subject_kind, git_sha, and query filtering. The optional full-text query searches Observation content and all stored subject text before grouping. observation_count is COUNT DISTINCT over the full filtered Observation set before subject-group pagination. Results order by observation_count DESC, latest_updated_at DESC, subject_kind, then subject. Pagination defaults to limit 50 and offset 0; limit is 1..200 and offset is 0..100000.")
      operationDescription "/api/v1/observations/match" "post" `shouldBe`
        Just (String "Requires repository read authorization for the requested active repository workspace. Matches concrete canonical repository-relative paths against stored file and glob subjects. The optional full-text query searches Observation content and all stored subject text. Canonical path_matches correlates each caller path with its ordered matched stored subjects.")
      hasSchemaProperty "ObservationMatchQuery" "paths" `shouldBe` True
      requiredSchemaFields "ObservationMatchQuery" `shouldSatisfy` maybe False (elem "paths")
      (matchPathsSchema >>= jsonField "minItems") `shouldBe` Just (Number 1)
      (matchPathsSchema >>= jsonField "maxItems") `shouldBe` Just (Number 256)
      schemaDescription "CreateObservation" `shouldSatisfy` maybe False (== String "Create with exactly one non-empty canonical `subjects` array or the complete deprecated legacy `subject_kind` plus `subject` pair. The forms are mutually exclusive; missing or half legacy pairs are rejected. Duplicate canonical subjects are de-duplicated in first-occurrence order. Subjects and Git provenance are immutable after creation.")
      schemaDescription "ObservationSubjectFacet" `shouldBe`
        Just (String "One exact stored subject group identified by the (subject_kind, subject) tuple. observation_count is the distinct Observation count over the complete filtered set before subject-group pagination; latest_updated_at is the newest matching Observation update time.")
      schemaDescription "ObservationMatchQuery" `shouldBe`
        Just (String "Match concrete canonical repository-relative paths against stored file and glob subjects after repository read authorization for the requested active repository workspace. Paths are ORed; optional subject_kind, git_sha, and query filters compose with the match. The optional full-text query searches Observation content and all stored subject text. Input paths never accept globs or touch the repository filesystem. Each Observation appears once; canonical path_matches follow deduplicated caller path order and each group's matched_subjects follow stored subject order. Results rank by text relevance when query is set, then updated_at DESC and id DESC. Pagination defaults to limit 50 and offset 0; limit is 1..200 and offset is 0..100000. git_sha is an exact immutable lowercase 40-character hexadecimal SHA.")
      schema "WorkspaceTimelineEvent" `shouldSatisfy` isJust
      schema "WorkspaceTimelineBucketsResponse" `shouldSatisfy` isJust
      hasSchemaProperty "WorkspaceTimelineBucket" "series" `shouldBe` True
      hasSchemaProperty "WorkspaceTimelineBucket" "series_totals" `shouldBe` True
      deprecatedProperty "WorkspaceTimelineBucket" "counts" `shouldBe` Just (Bool True)
      deprecatedProperty "WorkspaceTimelineBucket" "totals" `shouldBe` Just (Bool True)
      -- Change-stream requests are intentionally a disjoint start versus
      -- continuation contract, and scope itself is a global/workspace oneOf.
      -- Assert the served document so generated clients cannot combine bearer
      -- and start-key fields despite the runtime parser rejecting that form.
      case schema "ChangeStreamScopeRequest" >>= jsonField "oneOf" of
        Just (Array branches) -> length (toList branches) `shouldBe` 2
        _ -> expectationFailure "ChangeStreamScopeRequest must use a two-form oneOf"
      case schema "ChangeStreamResyncRequest" >>= jsonField "oneOf" of
        Just (Array branches) -> do
          let forms = toList branches
              required branch = jsonStrings (jsonField "required" branch)
              startForms = filter (maybe False (elem "start_idempotency_key") . required) forms
              continuationForms = filter (maybe False (elem "page_token") . required) forms
          length forms `shouldBe` 2
          length startForms `shouldBe` 1
          length continuationForms `shouldBe` 1
        _ -> expectationFailure "ChangeStreamResyncRequest must use start/continuation oneOf"
      fixedEmbedding `shouldBe` Just (Number 1536, Number 1536)
      hasOptionalAuditWorkspace `shouldBe` True
      mapM_ (\legacyPath -> (paths >>= jsonField legacyPath) `shouldBe` Nothing)
        [ "/api/v1/memories", "/api/v1/categories", "/api/v1/cleanup/policies"
        , "/api/v1/projects/{projectId}/memories", "/api/v1/tasks/{taskId}/context" ]
