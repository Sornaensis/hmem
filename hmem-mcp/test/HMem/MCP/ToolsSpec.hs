module HMem.MCP.ToolsSpec (spec) where

import Control.Concurrent.STM
import Control.Exception (bracket)
import Control.Monad (filterM, forM_)
import Data.Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Foldable (toList)
import Data.List (sort)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.UUID (UUID)
import Network.HTTP.Client (Manager, closeManager, defaultManagerSettings, newManager)
import Network.HTTP.Types (Status, methodDelete, methodGet, methodPost, methodPut, status200, status204, status400, status401, status403, status404, status409, status500, statusCode)
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp (testWithApplication)
import System.Directory (doesFileExist)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import Test.Hspec

import HMem.Config qualified as Config
import HMem.DB.ChangeStream (ChangeScope(..), OutboxRecord(..), listOutboxAfter)
import HMem.DB.Pool qualified as DBPool
import HMem.DB.RequestContext (ActorType(..), Principal(..), PrincipalAuthority(..), withPrincipalContext)
import HMem.DB.TestHarness (TestDb(..), TestEnv(..), createTestWorkspace, withTestEnv)
import HMem.MCP.Server (handleStdioLine)
import HMem.MCP.Tools
import HMem.Server.AccessTracker (newAccessTracker)
import HMem.Server.App (mkAppWithChangeStream)
import HMem.Server.Snapshot (materializeSnapshot)
import HMem.Server.WebSocket (newWSState)
import HMem.Types (CreateObservation(..), ObservationSubject(..), SubjectKind(..), UpdateObservation(..), Workspace(..), maxObservationSubjects)

spec :: Spec
spec = do
  describe "Workspace rename MCP registry" $ do
    it "advertises a strict name-only workspace_update call" $ do
      toolNames `shouldContain` ["workspace_update"]
      schemaProperties "workspace_update" `shouldBe` ["name", "workspace_id"]
      schemaRequired "workspace_update" `shouldBe` ["workspace_id", "name"]
      schemaAdditionalProperties "workspace_update" `shouldBe` Just (Bool False)
      parseToolCall "workspace_update" (object ["workspace_id" .= workspaceId, "name" .= ("Renamed" :: Text)]) `shouldSatisfy` isRight
      parseToolCall "workspace_update" (object ["workspace_id" .= workspaceId, "name" .= ("Renamed" :: Text), "workspace_type" .= ("repository" :: Text)]) `shouldSatisfy` isLeft
    it "forwards a strict PUT body and correlated request id" $ do
      requests <- newTVarIO []
      withMock requests $ \manager base -> do
        result <- call manager base "workspace_update" (object ["workspace_id" .= workspaceId, "name" .= ("Renamed" :: Text)])
        jsonField "action" result `shouldBe` Just (String "updated")
        jsonField "summary" result `shouldSatisfy` maybe False (hasFields ["id", "name"])
      [request] <- readTVarIO requests
      request.requestMethod `shouldBe` methodPut
      request.requestPath `shouldBe` "/api/v1/workspaces/11111111-2222-3333-4444-555555555555"
      decode request.requestBody `shouldBe` Just (object ["name" .= ("Renamed" :: Text)])
      request.requestId `shouldSatisfy` maybe False (not . BS.null)
    it "defers workspace name validation to the authorized REST boundary" $ do
      requests <- newTVarIO []
      withMock requests $ \manager base ->
        forM_ ["   ", T.replicate 1025 "x"] $ \workspaceName -> do
          response <- handleToolCall manager base Nothing (object ["name" .= ("workspace_update" :: Text), "arguments" .= object ["workspace_id" .= workspaceId, "name" .= workspaceName]])
          response `shouldNotSatisfy` isMcpError
      readTVarIO requests >>= \observed -> length observed `shouldBe` 2
    it "surfaces structured workspace REST 400, 403, and 404 messages as MCP errors" $
      forM_ [(status400, "Workspace name cannot be blank."), (status403, "Workspace edit permission is required."), (status404, "Workspace not found.")] $ \(status, message) ->
        withWorkspaceStructuredStatusMock status message $ \manager base -> do
          response <- handleToolCall manager base Nothing (object ["name" .= ("workspace_update" :: Text), "arguments" .= object ["workspace_id" .= workspaceId, "name" .= ("Valid workspace name" :: Text)]])
          response `shouldSatisfy` isMcpError
          response `shouldSatisfy` contains ("[HTTP_" <> T.pack (show (statusCode status)) <> "] " <> message)
    it "returns stable errors through the real deployed-mode application proxy" $
      forM_ renameProxyErrorCases $ \(principal, useMissingWorkspace, workspaceName, expected) ->
        withRenameProxyApp principal $ \workspaceId' manager base -> do
          let targetWorkspaceId = if useMissingWorkspace then missingWorkspaceId else workspaceId'
          response <- handleToolCall manager base Nothing (object
            [ "name" .= ("workspace_update" :: Text)
            , "arguments" .= object ["workspace_id" .= targetWorkspaceId, "name" .= workspaceName]
            ])
          response `shouldSatisfy` isMcpError
          response `shouldSatisfy` contains expected
  describe "Observation MCP registry" $ do
    it "advertises and parses every Observation capability, with no removed memory, link, or context tools" $ do
      toolNames `shouldContain` ["observation_create", "observation_get", "observation_update", "observation_list", "observation_match", "observation_delete", "observation_set_embedding", "observation_similar"]
      mapM_ (\(name, arguments) -> parseToolCall name arguments `shouldSatisfy` isRight)
        [ ("observation_create", observationArguments)
        , ("observation_get", object ["observation_id" .= observationId])
        , ("observation_update", object ["observation_id" .= observationId, "content" .= ("replacement" :: Text)])
        , ("observation_list", observationListArguments)
        , ("observation_match", observationMatchArguments)
        , ("observation_delete", object ["observation_id" .= observationId])
        , ("observation_set_embedding", object ["observation_id" .= observationId, "embedding" .= embedding])
        , ("observation_similar", similarArguments)
        ]
      toolNames `shouldNotSatisfy` any (`elem` ["memory_create", "memory_get", "memory_update", "memory_link", "link_memory", "context_get"])
      mapM_ (\name -> parseToolCall name (object []) `shouldSatisfy` isUnknown name)
        ["memory_create", "memory_get", "memory_update", "memory_link", "link_memory", "context_get"]

    it "advertises only provenance fields on observation tools and search" $ do
      all (`elem` schemaProperties "observation_create") ["subjects", "git_sha", "content"] `shouldBe` True
      schemaProperties "observation_create" `shouldNotContain` ["subject_kind", "subject", "workspace_id"]
      schemaRequired "observation_create" `shouldBe` ["subjects", "git_sha", "content"]
      (schemaProperty "observation_create" "subjects" >>= jsonField "maxItems") `shouldBe` Just (Number (fromIntegral maxObservationSubjects))
      all (`elem` schemaProperties "observation_update") ["observation_id", "content"] `shouldBe` True
      length (schemaProperties "observation_update") `shouldBe` 2
      all (`elem` schemaProperties "search") ["subject_kind", "subject", "git_sha"] `shouldBe` True
      schemaProperties "search" `shouldContain` ["offset"]
      all (`elem` schemaProperties "observation_list") ["subject_kind", "subject", "git_sha", "query", "limit", "offset"] `shouldBe` True
      schemaProperties "observation_list" `shouldNotContain` ["workspace_id"]
      all (`elem` schemaProperties "observation_similar") ["embedding", "min_similarity", "limit", "offset"] `shouldBe` True
      schemaProperties "observation_similar" `shouldNotContain` ["workspace_id"]
      all (`elem` schemaProperties "observation_match") ["paths", "subject_kind", "git_sha", "query", "limit", "offset"] `shouldBe` True
      schemaProperties "observation_match" `shouldNotContain` ["workspace_id", "subject"]
      schemaRequired "observation_match" `shouldBe` ["paths"]
      toolDescription "observation_match" `shouldSatisfy` maybe False (\description -> all (\needle -> needle `T.isInfixOf` description) ["concrete", "OR", "glob", "next_offset"])
      schemaProperties "search" `shouldNotContain` ["memory_type", "tags", "pinned_only", "min_importance"]
      (schemaProperty "set_workspace" "workspace_id" >>= jsonField "anyOf")
        `shouldSatisfy` maybe False nullableWorkspaceTypes
      toolDescription "task_finish" `shouldSatisfy` maybe False ("does not create an observation" `T.isInfixOf`)
      toolDescription "project_archive" `shouldSatisfy` maybe False (not . ("summary" `T.isInfixOf`))
      sort (schemaProperties "task_dependency") `shouldBe` sort ["task_id", "depends_on_id", "action"]
      schemaRequired "task_dependency" `shouldBe` ["task_id", "depends_on_id", "action"]

    it "forwards both trusted-MCP headers only when the bridge is configured" $ do
      mcpProvenanceHeadersFor (Just "test-private-provenance")
        `shouldBe` [("X-HMem-Change-Cause", "mcp"), ("X-HMem-MCP-Provenance", "test-private-provenance")]
      mcpProvenanceHeadersFor (Just "  trimmed-private-provenance  ")
        `shouldBe` [("X-HMem-Change-Cause", "mcp"), ("X-HMem-MCP-Provenance", "trimmed-private-provenance")]
      mcpProvenanceHeadersFor (Just "  provenance-\955  ")
        `shouldBe` [("X-HMem-Change-Cause", "mcp"), ("X-HMem-MCP-Provenance", TE.encodeUtf8 "provenance-\955")]
      mcpProvenanceHeadersFor Nothing
        `shouldBe` [("X-HMem-Change-Cause", "mcp")]
      mcpProvenanceHeadersFor (Just " \t ")
        `shouldBe` [("X-HMem-Change-Cause", "mcp")]

    it "guides durable project and task descriptions, status, and atomic subtasks" $ do
      toolDescriptionIs "project_create" "Create a project in the active workspace. Its description is a durable specification; status records execution state."
      toolDescriptionIs "project_update" "Update a project's durable specification, hierarchy, or execution state. Descriptions are not logs of progress or updates."
      toolDescriptionIs "project_spec" "Create a project and its initial atomic tasks in one call. Descriptions are durable specifications; status records execution state. Create later-discovered atomic work as subtasks."
      toolDescriptionIs "task_create" "Create a task in the active workspace. Its description is a durable specification; status records execution state. Create later-discovered atomic work as subtasks."
      toolDescriptionIs "task_update" "Update a task's durable specification, hierarchy, or execution state. Descriptions are not logs of progress or updates; create later-discovered atomic work as subtasks."
      toolDescriptionIs "task_dependency" "Add or remove a prerequisite edge: task_id cannot proceed until depends_on_id is complete. Use dependencies for ordering, not logs of progress or updates."
      toolDescriptionIs "task_start" "Set a task's execution state to in_progress. Preserve its description as a durable specification; status records progress."
      toolDescriptionIs "task_finish" "Set a task's execution state to done, blocked, or cancelled. Status records progress; this does not create an observation."
      schemaPropertyDescriptionIs "project_create" "description" "Optional durable project specification: aims, scope, constraints, approach, and acceptance intent; not a log of progress or updates"
      schemaPropertyDescriptionIs "project_update" "description" "Durable project specification: aims, scope, constraints, approach, and acceptance intent, or null; not a log of progress or updates"
      schemaPropertyDescriptionIs "project_update" "status" "Execution state; record progress here, not in the description"
      schemaPropertyDescriptionIs "project_spec" "description" "Optional durable project specification: aims, scope, constraints, approach, and acceptance intent; not a log of progress or updates"
      schemaPropertyDescriptionIs "task_create" "description" "Optional durable task specification: scope, constraints, approach, and acceptance intent; not a log of progress or updates"
      schemaPropertyDescriptionIs "task_create" "parent_id" "Optional parent task UUID; use it to create a subtask for later-discovered atomic work"
      schemaPropertyDescriptionIs "task_update" "description" "Durable task specification: scope, constraints, approach, and acceptance intent, or null; not a log of progress or updates"
      schemaPropertyDescriptionIs "task_update" "parent_id" "Parent task UUID to make this an atomic subtask for later-discovered work, or null"
      schemaPropertyDescriptionIs "task_update" "status" "Execution state; record progress here, not in the description"
      schemaPropertyDescriptionIs "project_spec" "tasks" "Initial atomic tasks; later-discovered atomic work must be created as subtasks, not appended to a parent description"
      projectSpecTaskPropertyDescriptionIs "title" "Atomic task title"
      projectSpecTaskPropertyDescriptionIs "description" "Durable task specification: scope, constraints, approach, and acceptance intent; not a log of progress or updates"
      projectSpecTaskPropertyDescriptionIs "priority" "Task priority"
      (schemaProperty "project_spec" "tasks" >>= jsonField "items" >>= jsonField "required") `shouldBe` Just (toJSON (["title"] :: [Text]))
      planningGuidance `shouldSatisfy` planningGuidanceContract
      T.replace "durable specification" "progress log" planningGuidance `shouldNotSatisfy` planningGuidanceContract
      T.replace "atomic work as subtasks" "parent description" planningGuidance `shouldNotSatisfy` planningGuidanceContract

    it "guides Observations as durable repository insights and treats Git SHAs as staleness sentinels" $ do
      toolDescriptionIs "search" "Search observations, projects, and tasks. An Observation is a durable, non-obvious repository insight tied to file or glob subjects; subject_kind, subject, and git_sha are exact provenance filters."
      toolDescriptionIs "observation_create" "Create an Observation: a durable, non-obvious repository insight tied to one or more repository-relative file or glob subjects. git_sha records the repository state where the insight was established; use it as a sentinel to decide whether the insight needs re-audit, not as timeless proof. Pass subjects as an ordered array; they are OR alternatives and, with git_sha, immutable after creation. File subjects must be concrete paths. Glob subjects may use only *, ?, and ** path components (for example my/src/proj/**/*.java)."
      toolDescriptionIs "observation_update" "Replace only the content of a durable, non-obvious repository insight. Subjects and git_sha are immutable provenance; git_sha remains the state where the insight was established and a staleness-audit sentinel, not timeless proof."
      toolDescriptionIs "observation_list" "List durable, non-obvious repository insights using exact subject and git_sha provenance filters and optional text search. git_sha is a sentinel for deciding when an insight needs re-audit, not timeless proof. When has_more is true, pass next_offset to retrieve the next page."
      toolDescriptionIs "observation_match" "Find durable, non-obvious repository insights whose stored file subjects or safe glob subjects match any concrete repository-relative path supplied in paths. Paths are ORed; do not pass globs here and no repository filesystem is read. Optional filters compose with matching. git_sha is a staleness-audit sentinel, not timeless proof. Continue with next_offset until has_more is false."
      toolDescriptionIs "observation_similar" "Find semantically similar durable, non-obvious repository insights. Subject and git_sha filters are exact provenance filters; git_sha is a staleness-audit sentinel, not timeless proof. To continue, add returned_count to offset and repeat until returned_count is less than limit or zero."
      schemaPropertyDescriptionIs "observation_create" "content" "Durable, non-obvious repository insight about its subjects; not a progress update or routine fact"
      schemaPropertyDescriptionIs "observation_create" "git_sha" "Lowercase 40-character Git SHA for the repository state where this insight was established; a staleness-audit sentinel, not timeless proof"
      schemaPropertyDescriptionIs "observation_update" "content" "Replacement durable, non-obvious repository insight about the existing subjects; not a progress update or routine fact"
      schemaPropertyDescriptionIs "observation_match" "paths" "One to 256 concrete repository-relative files to match against stored Observation subjects; globs are rejected"
      observationGuidance `shouldSatisfy` observationGuidanceContract
      T.replace "durable, non-obvious" "routine, obvious" observationGuidance `shouldNotSatisfy` observationGuidanceContract
      T.replace "not timeless proof" "timeless proof" observationGuidance `shouldNotSatisfy` observationGuidanceContract

    it "parses, validates, dispatches, and JSON-RPC-routes task dependency mutations" $ do
      let addArguments = object ["task_id" .= observationId, "depends_on_id" .= workspaceId, "action" .= ("add" :: Text)]
          removeArguments = object ["task_id" .= observationId, "depends_on_id" .= workspaceId, "action" .= ("remove" :: Text)]
      parseToolCall "task_dependency" addArguments `shouldSatisfy` isRight
      parseToolCall "task_dependency" (object ["task_id" .= observationId, "depends_on_id" .= workspaceId, "action" .= ("replace" :: Text)])
        `shouldSatisfy` isRight
      case parseToolCall "task_dependency" (object ["task_id" .= observationId, "depends_on_id" .= workspaceId, "action" .= ("replace" :: Text)]) of
        Right parsed -> validateToolCall parsed `shouldSatisfy` isLeft
        Left err -> expectationFailure err
      requests <- newTVarIO []
      withMock requests $ \manager base -> do
        added <- call manager base "task_dependency" addArguments
        added `shouldSatisfy` hasFields ["ok", "action", "entity_type", "task_id", "depends_on_id", "affected_tasks"]
        removed <- call manager base "task_dependency" removeArguments
        removed `shouldSatisfy` hasFields ["ok", "action", "entity_type", "task_id", "depends_on_id", "affected_tasks"]
        initialized <- newTVarIO True
        workspaceContext <- newTVarIO Nothing
        jsonRpcToolCall manager base initialized workspaceContext "task_dependency" addArguments
          >>= (`shouldSatisfy` maybe False (not . isJsonRpcMcpError))
      [addRequest, removeRequest, jsonRpcRequest] <- readTVarIO requests
      addRequest.requestMethod `shouldBe` methodPost
      addRequest.requestPath `shouldBe` "/api/v1/tasks/aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee/dependencies"
      decode addRequest.requestBody `shouldBe` Just (object ["depends_on_id" .= workspaceId])
      removeRequest.requestMethod `shouldBe` methodDelete
      removeRequest.requestPath `shouldBe` "/api/v1/tasks/aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee/dependencies/11111111-2222-3333-4444-555555555555"
      jsonRpcRequest.requestPath `shouldBe` "/api/v1/tasks/aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee/dependencies"

    it "preserves structured dependency-cycle errors through the MCP and JSON-RPC bridges" $
      withDependencyCycleMock $ \manager base -> do
        let arguments = object ["task_id" .= observationId, "depends_on_id" .= workspaceId, "action" .= ("add" :: Text)]
        response <- handleToolCall manager base Nothing (object ["name" .= ("task_dependency" :: Text), "arguments" .= arguments])
        response `shouldSatisfy` isMcpError
        response `shouldSatisfy` contains "[HTTP_400] Task dependency would create a cycle"
        response `shouldSatisfy` contains "dependency_cycle"
        initialized <- newTVarIO True
        workspaceContext <- newTVarIO Nothing
        jsonRpcToolCall manager base initialized workspaceContext "task_dependency" arguments
          >>= (`shouldSatisfy` maybe False (\value -> isJsonRpcMcpError value && contains "dependency_cycle" value))

    it "keeps the advertised registry parser and dispatch reachability in lockstep" $ do
      sort toolNames `shouldBe` sort (serverOwnedTools <> map fst toolSamples)
      mapM_ (\(name, arguments) -> parseToolCall name arguments `shouldSatisfy` isRight) toolSamples
      requests <- newTVarIO []
      withMock requests $ \manager base ->
        forM_ toolSamples $ \(name, arguments) -> do
          response <- handleToolCall manager base Nothing (object ["name" .= name, "arguments" .= arguments])
          response `shouldSatisfy` not . isMcpError
      dispatched <- readTVarIO requests
      all (`elem` map (.requestPath) dispatched)
        [ "/api/v1/workspaces", "/api/v1/search", "/api/v1/observations"
        , "/api/v1/observations/aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee"
         , "/api/v1/observations/aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee/embedding"
         , "/api/v1/observations/match", "/api/v1/observations/similar", "/api/v1/projects", "/api/v1/tasks"
        ] `shouldBe` True
      manager <- newManager defaultManagerSettings
      initialized <- newTVarIO True
      workspaceContext <- newTVarIO Nothing
      setResponse <- serverToolCall manager initialized workspaceContext "set_workspace" (object ["workspace_id" .= workspaceId])
      setResponse `shouldSatisfy` maybe False (contains "workspace_context")
      getResponse <- serverToolCall manager initialized workspaceContext "get_workspace" (object [])
      getResponse `shouldSatisfy` maybe False (contains workspaceId)

    it "routes every live tool through JSON-RPC, including server-owned workspace context" $ do
      requests <- newTVarIO []
      withMock requests $ \manager base -> do
        initialized <- newTVarIO True
        workspaceContext <- newTVarIO Nothing
        forM_ toolSamples $ \(name, arguments) -> do
          response <- jsonRpcToolCall manager base initialized workspaceContext name arguments
          response `shouldSatisfy` maybe False (not . isJsonRpcMcpError)
        setResponse <- jsonRpcToolCall manager base initialized workspaceContext "set_workspace" (object ["workspace_id" .= workspaceId])
        setResponse `shouldSatisfy` maybe False (not . isJsonRpcMcpError)
        getResponse <- jsonRpcToolCall manager base initialized workspaceContext "get_workspace" (object [])
        getResponse `shouldSatisfy` maybe False (contains workspaceId)

    it "injects the session workspace into observation_match when agents omit it" $ do
      requests <- newTVarIO []
      withMock requests $ \manager base -> do
        initialized <- newTVarIO True
        workspaceContext <- newTVarIO Nothing
        _ <- serverToolCall manager initialized workspaceContext "set_workspace" (object ["workspace_id" .= workspaceId])
        response <- jsonRpcToolCall manager base initialized workspaceContext "observation_match"
          (object ["paths" .= (["src/HMem/Types.hs"] :: [Text])])
        response `shouldSatisfy` maybe False (not . isJsonRpcMcpError)
      [request] <- readTVarIO requests
      decode request.requestBody `shouldBe` Just (object ["workspace_id" .= workspaceId, "paths" .= (["src/HMem/Types.hs"] :: [Text])])

  describe "Observation compact response shaping" $ do
    it "derives bounded content_preview from a create/get response when the server supplies full content" $ do
      let summary = compactObservationSummary (object ["id" .= observationId, "subjects" .= observationSubjects, "subject_kind" .= ("file" :: Text), "subject" .= ("src/HMem/Types.hs" :: Text), "git_sha" .= gitSha, "content" .= (T.replicate 600 "x")])
      jsonField "content_preview" summary `shouldBe` Just (String (T.replicate 500 "x"))
      summary `shouldSatisfy` hasFields ["id", "subjects", "subject_kind", "subject", "git_sha", "content_preview"]

    it "matches fixture-backed compact golden outputs for new Observation tools" $ do
      golden <- compactFixtures
      requests <- newTVarIO []
      withMock requests $ \manager base -> do
        created <- call manager base "observation_create" observationArguments
        created `shouldBe` fixturePayload golden "observation_create"
        searched <- call manager base "search" (object ["workspace_id" .= workspaceId, "entity_types" .= (["observation"] :: [Text])])
        searched `shouldBe` fixturePayload golden "unified_search"
        listed <- call manager base "observation_list" observationListArguments
        listed `shouldBe` fixturePayload golden "observation_list"
        deleted <- call manager base "observation_delete" (object ["observation_id" .= observationId])
        deleted `shouldBe` fixturePayload golden "observation_delete"
        embedded <- call manager base "observation_set_embedding" (object ["observation_id" .= observationId, "embedding" .= embedding])
        embedded `shouldBe` fixturePayload golden "observation_set_embedding"
        similar <- call manager base "observation_similar" similarArguments
        similar `shouldBe` fixturePayload golden "observation_similar"
        matched <- call manager base "observation_match" observationMatchArguments
        matched `shouldBe` fixturePayload golden "observation_match"

  describe "Observation parsing and validation" $ do
    it "parses provenance-bound creates and content-only updates" $ do
      case parseToolCall "observation_create" observationArguments of
        Right (ObservationCreate (CreateObservation _ values sha _)) -> do
          values `shouldBe` observationSubjects
          sha `shouldBe` gitSha
        result -> expectationFailure (show result)
      case parseToolCall "observation_update" (object ["workspace_id" .= workspaceId, "observation_id" .= observationId, "content" .= ("replacement" :: Text)]) of
        Right (ObservationUpdate _ (UpdateObservation content)) -> content `shouldBe` "replacement"
        result -> expectationFailure (show result)

    it "rejects mutable provenance fields on update and malformed provenance on create" $ do
      parseToolCall "observation_update" (object ["observation_id" .= observationId, "content" .= ("replacement" :: Text), "git_sha" .= gitSha]) `shouldSatisfy` isLeft
      parseToolCall "observation_create" (object ["workspace_id" .= workspaceId, "subjects" .= observationSubjects, "subject_kind" .= ("file" :: Text), "subject" .= ("src/HMem/Types.hs" :: Text), "git_sha" .= gitSha, "content" .= ("content" :: Text)]) `shouldSatisfy` isLeft
      case parseToolCall "observation_create" (object ["workspace_id" .= workspaceId, "subjects" .= [object ["subject_kind" .= ("file" :: Text), "subject" .= ("/absolute" :: Text)]], "git_sha" .= ("bad" :: Text), "content" .= ("content" :: Text)]) of
        Right parsed -> validateToolCall parsed `shouldSatisfy` isLeft
        result -> expectationFailure (show result)

    it "accepts observation search provenance filters and rejects legacy entity types" $ do
      parseToolCall "search" (object ["workspace_id" .= workspaceId, "entity_types" .= (["observation"] :: [Text]), "subject_kind" .= ("file" :: Text), "subject" .= ("src/HMem/Types.hs" :: Text), "git_sha" .= gitSha]) `shouldSatisfy` isRight
      parseToolCall "search" (object ["workspace_id" .= workspaceId, "entity_types" .= (["memory"] :: [Text])]) `shouldSatisfy` isLeft

    it "validates Observation pagination, similarity bounds, and fixed embeddings before dispatch" $ do
      case parseToolCall "observation_list" (object ["workspace_id" .= workspaceId, "limit" .= (201 :: Int)]) of
        Right parsed -> validateToolCall parsed `shouldSatisfy` isLeft
        result -> expectationFailure (show result)
      case parseToolCall "observation_similar" (object ["workspace_id" .= workspaceId, "embedding" .= ([0 :: Double] :: [Double]), "min_similarity" .= (1.1 :: Double)]) of
        Right parsed -> validateToolCall parsed `shouldSatisfy` isLeft
        result -> expectationFailure (show result)
      case parseToolCall "observation_set_embedding" (object ["observation_id" .= observationId, "embedding" .= ([0 :: Double] :: [Double])]) of
        Right parsed -> validateToolCall parsed `shouldSatisfy` isLeft
        result -> expectationFailure (show result)
      case parseToolCall "observation_match" (object ["workspace_id" .= workspaceId, "paths" .= (["src/**/*.hs"] :: [Text])]) of
        Right parsed -> validateToolCall parsed `shouldSatisfy` isLeft
        result -> expectationFailure (show result)

    it "supports canonical multi-subject creates and unadvertised legacy singleton compatibility" $ do
      case parseToolCall "observation_create" legacyObservationArguments of
        Right (ObservationCreate (CreateObservation _ values _ _)) -> values `shouldBe` [ObservationSubject SubjectFile "src/HMem/Types.hs"]
        result -> expectationFailure (show result)
      parseToolCall "observation_create" (object ["workspace_id" .= workspaceId, "subject_kind" .= ("file" :: Text), "git_sha" .= gitSha, "content" .= ("content" :: Text)]) `shouldSatisfy` isLeft

    it "rejects incomplete legacy create forms before any HTTP request" $ do
      requests <- newTVarIO []
      withMock requests $ \manager base -> do
        let malformed fields = handleToolCall manager base Nothing
              (object ["name" .= ("observation_create" :: Text), "arguments" .= object (["workspace_id" .= workspaceId, "git_sha" .= gitSha, "content" .= ("content" :: Text)] <> fields)])
        subjectOnly <- malformed ["subject" .= ("src/HMem/Types.hs" :: Text)]
        kindOnly <- malformed ["subject_kind" .= ("file" :: Text)]
        subjectOnly `shouldSatisfy` isMcpError
        kindOnly `shouldSatisfy` isMcpError
      requestsAfter <- readTVarIO requests
      length requestsAfter `shouldBe` 0

    it "rejects 257 unique subjects and concrete match paths before HTTP dispatch" $ do
      requests <- newTVarIO []
      let tooManySubjects = [ObservationSubject SubjectFile ("src/File" <> T.pack (show index) <> ".hs") | index <- [1 .. maxObservationSubjects + 1]]
          tooManyPaths = ["src/File" <> T.pack (show index) <> ".hs" | index <- [1 .. 257 :: Int]]
      withMock requests $ \manager base -> do
        tooManyCreate <- handleToolCall manager base Nothing (object
          [ "name" .= ("observation_create" :: Text)
          , "arguments" .= object ["workspace_id" .= workspaceId, "subjects" .= tooManySubjects, "git_sha" .= gitSha, "content" .= ("content" :: Text)]
          ])
        tooManyMatch <- handleToolCall manager base Nothing (object
          [ "name" .= ("observation_match" :: Text)
          , "arguments" .= object ["workspace_id" .= workspaceId, "paths" .= tooManyPaths]
          ])
        tooManyCreate `shouldSatisfy` isMcpError
        tooManyMatch `shouldSatisfy` isMcpError
      requestsAfter <- readTVarIO requests
      length requestsAfter `shouldBe` 0

  describe "Observation dispatch and status-only workflows" $ do
    it "uses Observation paths and compact provenance shapes" $ do
      requests <- newTVarIO []
      withMock requests $ \manager base -> do
        created <- call manager base "observation_create" observationArguments
        jsonField "entity_type" created `shouldBe` Just (String "observation")
        jsonField "summary" created `shouldSatisfy` maybe False (hasFields ["id", "subject_kind", "subject", "git_sha", "content_preview"])
        jsonField "workspace_id" created `shouldBe` Nothing
        detail <- call manager base "observation_get" (object ["observation_id" .= observationId])
        jsonField "content" detail `shouldBe` Just (String "complete observation content")
        detail `shouldSatisfy` hasFields ["subjects", "subject_kind", "subject"]
        detail `shouldSatisfy` not . contains "workspace_id"
        update <- call manager base "observation_update" (object ["observation_id" .= observationId, "content" .= ("replacement" :: Text)])
        jsonField "action" update `shouldBe` Just (String "updated")
      observed <- readTVarIO requests
      map (.requestPath) observed `shouldBe` ["/api/v1/observations", "/api/v1/observations/aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee", "/api/v1/observations/aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee"]
      map (.requestMethod) observed `shouldBe` [methodPost, methodGet, methodPut]

    it "passes exact provenance filters through unified search and compacts observations" $ do
      requests <- newTVarIO []
      withMock requests $ \manager base -> do
        result <- call manager base "search" (object ["workspace_id" .= workspaceId, "entity_types" .= (["observation"] :: [Text]), "subject_kind" .= ("file" :: Text), "subject" .= ("src/HMem/Types.hs" :: Text), "git_sha" .= gitSha])
        jsonField "observations" result `shouldSatisfy` maybe False (arrayFirst (hasFields ["id", "subject_kind", "subject", "git_sha", "content_preview"]))
        result `shouldSatisfy` not . contains "linked_memories"
      [RequestInfo _ _ _ body _ _ _ _] <- readTVarIO requests
      decode body `shouldSatisfy` maybe False (\value -> hasFields ["subject_kind", "subject", "git_sha"] value)

    it "forwards list filters and gives an exact next offset only when the server reports another page" $ do
      requests <- newTVarIO []
      withMock requests $ \manager base -> do
        first <- call manager base "observation_list" observationListArguments
        jsonField "has_more" first `shouldBe` Just (Bool True)
        jsonField "next_offset" first `shouldBe` Just (Number 2)
        jsonField "items" first `shouldSatisfy` maybe False (arrayFirst (hasFields ["id", "subjects", "subject_kind", "subject", "git_sha", "content_preview"]))
        final <- call manager base "observation_list" (object ["workspace_id" .= workspaceId, "limit" .= (2 :: Int), "offset" .= (2 :: Int)])
        jsonField "has_more" final `shouldBe` Just (Bool False)
        jsonField "next_offset" final `shouldBe` Nothing
      [firstRequest, finalRequest] <- readTVarIO requests
      firstRequest.requestPath `shouldBe` "/api/v1/observations"
      firstRequest.requestQuery `shouldBe` "?workspace_id=11111111-2222-3333-4444-555555555555&subject_kind=file&subject=src%2FHMem%2FTypes.hs&git_sha=0123456789abcdef0123456789abcdef01234567&query=types&limit=2&offset=0"
      finalRequest.requestQuery `shouldBe` "?workspace_id=11111111-2222-3333-4444-555555555555&limit=2&offset=2"

    it "sends canonical subject arrays for both advertised and legacy singleton creation" $ do
      requests <- newTVarIO []
      withMock requests $ \manager base -> do
        _ <- call manager base "observation_create" observationArguments
        _ <- call manager base "observation_create" legacyObservationArguments
        pure ()
      [canonicalRequest, legacyRequest] <- readTVarIO requests
      let expectedCanonical = object ["workspace_id" .= workspaceId, "subjects" .= observationSubjects, "git_sha" .= gitSha, "content" .= ("complete observation content" :: Text)]
          expectedLegacy = object ["workspace_id" .= workspaceId, "subjects" .= [ObservationSubject SubjectFile "src/HMem/Types.hs"], "git_sha" .= gitSha, "content" .= ("complete observation content" :: Text)]
      canonicalRequest.requestMethod `shouldBe` methodPost
      legacyRequest.requestMethod `shouldBe` methodPost
      canonicalRequest.requestPath `shouldBe` "/api/v1/observations"
      legacyRequest.requestPath `shouldBe` "/api/v1/observations"
      decode canonicalRequest.requestBody `shouldBe` Just expectedCanonical
      decode legacyRequest.requestBody `shouldBe` Just expectedLegacy

    it "matches concrete paths with deterministic evidence, bearer forwarding, and continuation metadata" $ do
      requests <- newTVarIO []
      withMock requests $ \manager base -> do
        result <- callWithKey manager base (Just "test-token") "observation_match" observationMatchArguments
        jsonField "has_more" result `shouldBe` Just (Bool True)
        jsonField "returned_count" result `shouldBe` Just (Number 2)
        jsonField "next_offset" result `shouldBe` Just (Number 2)
        jsonField "items" result `shouldSatisfy` maybe False (arrayFirst (hasFields ["observation", "path_matches", "matched_paths", "matched_subjects"]))
      [request] <- readTVarIO requests
      request.requestMethod `shouldBe` methodPost
      request.requestPath `shouldBe` "/api/v1/observations/match"
      request.authorization `shouldBe` Just "Bearer test-token"
      decode request.requestBody `shouldBe` Just (object ["workspace_id" .= workspaceId, "paths" .= (["src/HMem/Types.hs", "my/src/proj/Main.java"] :: [Text]), "subject_kind" .= ("glob" :: Text), "git_sha" .= gitSha, "query" .= ("types" :: Text), "limit" .= (2 :: Int), "offset" .= (0 :: Int)])

    it "continues observation_match without a final or empty cursor" $ do
      requests <- newTVarIO []
      withMock requests $ \manager base -> do
        first <- call manager base "observation_match" observationMatchArguments
        jsonField "has_more" first `shouldBe` Just (Bool True)
        jsonField "next_offset" first `shouldBe` Just (Number 2)
        final <- call manager base "observation_match" (withOffset observationMatchArguments 2)
        jsonField "has_more" final `shouldBe` Just (Bool False)
        jsonField "returned_count" final `shouldBe` Just (Number 0)
        jsonField "next_offset" final `shouldBe` Nothing
      observed <- readTVarIO requests
      map (.requestMethod) observed `shouldBe` [methodPost, methodPost]
      map (decode . (.requestBody)) observed `shouldBe`
        [ Just (object ["workspace_id" .= workspaceId, "paths" .= (["src/HMem/Types.hs", "my/src/proj/Main.java"] :: [Text]), "subject_kind" .= ("glob" :: Text), "git_sha" .= gitSha, "query" .= ("types" :: Text), "limit" .= (2 :: Int), "offset" .= (0 :: Int)])
        , Just (object ["workspace_id" .= workspaceId, "paths" .= (["src/HMem/Types.hs", "my/src/proj/Main.java"] :: [Text]), "subject_kind" .= ("glob" :: Text), "git_sha" .= gitSha, "query" .= ("types" :: Text), "limit" .= (2 :: Int), "offset" .= (2 :: Int)])
        ]

    it "deduplicates match rows and emits no cursor for nonempty-final or empty pages" $ do
      let matchedObservation = object ["id" .= observationId, "subjects" .= observationSubjects, "subject_kind" .= ("file" :: Text), "subject" .= ("src/HMem/Types.hs" :: Text), "git_sha" .= gitSha, "content_preview" .= ("preview" :: Text)]
          pathEvidence = [object ["path" .= ("src/HMem/Types.hs" :: Text), "matched_subjects" .= observationSubjects]]
          row = object ["observation" .= matchedObservation, "path_matches" .= pathEvidence, "matched_paths" .= (["src/HMem/Types.hs"] :: [Text]), "matched_subjects" .= observationSubjects]
          page = compactObservationMatches 4 (object ["items" .= [row, row], "has_more" .= True])
          nonemptyFinalPage = compactObservationMatches 6 (object ["items" .= [row], "has_more" .= False])
          finalPage = compactObservationMatches 6 (object ["items" .= ([] :: [Value]), "has_more" .= False])
      jsonField "returned_count" page `shouldBe` Just (Number 2)
      jsonField "next_offset" page `shouldBe` Just (Number 6)
      case jsonField "items" page of
        Just (Array values) -> length values `shouldBe` 1
        _ -> expectationFailure "match page did not contain items"
      jsonField "returned_count" nonemptyFinalPage `shouldBe` Just (Number 1)
      jsonField "next_offset" nonemptyFinalPage `shouldBe` Nothing
      jsonField "returned_count" finalPage `shouldBe` Just (Number 0)
      jsonField "next_offset" finalPage `shouldBe` Nothing

    it "uses raw embeddings, forwards authorization, and returns signal-only acknowledgements for 204 mutations" $ do
      requests <- newTVarIO []
      withMock requests $ \manager base -> do
        deleted <- callWithKey manager base (Just "test-token") "observation_delete" (object ["observation_id" .= observationId])
        deleted `shouldBe` object ["ok" .= True, "action" .= ("deleted" :: Text), "entity_type" .= ("observation" :: Text), "id" .= observationId]
        embedded <- callWithKey manager base (Just "test-token") "observation_set_embedding" (object ["observation_id" .= observationId, "embedding" .= embedding])
        embedded `shouldBe` object ["ok" .= True, "action" .= ("embedding_set" :: Text), "entity_type" .= ("observation" :: Text), "id" .= observationId]
      [deleteRequest, embeddingRequest] <- readTVarIO requests
      map (.requestMethod) [deleteRequest, embeddingRequest] `shouldBe` [methodDelete, methodPut]
      embeddingRequest.requestPath `shouldBe` "/api/v1/observations/aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee/embedding"
      decode embeddingRequest.requestBody `shouldBe` Just (toJSON embedding)
      map (.authorization) [deleteRequest, embeddingRequest] `shouldBe` [Just "Bearer test-token", Just "Bearer test-token"]

    it "compacts similar results and provides only repeat-until-short continuation metadata" $ do
      requests <- newTVarIO []
      withMock requests $ \manager base -> do
        result <- call manager base "observation_similar" similarArguments
        jsonField "items" result `shouldSatisfy` maybe False (arrayFirst (hasFields ["observation", "similarity"]))
        jsonField "offset" result `shouldBe` Just (Number 4)
        jsonField "limit" result `shouldBe` Just (Number 2)
        jsonField "returned_count" result `shouldBe` Just (Number 1)
        jsonField "has_more" result `shouldBe` Nothing
      [request] <- readTVarIO requests
      request.requestPath `shouldBe` "/api/v1/observations/similar"
      request.requestMethod `shouldBe` methodPost
      decode request.requestBody `shouldBe` Just (object ["workspace_id" .= workspaceId, "subject_kind" .= ("file" :: Text), "subject" .= ("src/HMem/Types.hs" :: Text), "git_sha" .= gitSha, "embedding" .= embedding, "min_similarity" .= (0.5 :: Double), "limit" .= (2 :: Int), "offset" .= (4 :: Int)])

    it "propagates REST failures, including pgvector capability errors" $ do
      withErrorMock $ \manager base -> do
        result <- handleToolCall manager base Nothing (object ["name" .= ("observation_set_embedding" :: Text), "arguments" .= object ["observation_id" .= observationId, "embedding" .= embedding]])
        result `shouldSatisfy` contains "[HTTP_400] pgvector extension is required"

    it "propagates HTTP status failures and forwards Bearer authorization on Observation reads" $ do
      requests <- newTVarIO []
      withStatusMock requests $ \manager base -> do
        let run :: Text -> Value -> Text -> IO ()
            run name arguments expected = do
              response <- handleToolCall manager base (Just "test-token") (object ["name" .= name, "arguments" .= arguments])
              response `shouldSatisfy` contains expected
        run "observation_get" (object ["observation_id" .= observationId]) "[HTTP_401] status 401"
        run "observation_list" observationListArguments "[HTTP_403] status 403"
        run "observation_delete" (object ["observation_id" .= observationId]) "[HTTP_404] status 404"
        run "observation_set_embedding" (object ["observation_id" .= observationId, "embedding" .= embedding]) "[HTTP_409] status 409"
        run "observation_similar" similarArguments "[HTTP_400] status 400"
        run "observation_match" observationMatchArguments "[HTTP_500] status 500"
      observed <- readTVarIO requests
      map (.authorization) observed `shouldBe` replicate 6 (Just "Bearer test-token")

    it "makes task_finish and project_archive single status mutations without observation side effects" $ do
      requests <- newTVarIO []
      withMock requests $ \manager base -> do
        finished <- call manager base "task_finish" (object ["task_id" .= observationId, "status" .= ("done" :: Text)])
        jsonField "action" finished `shouldBe` Just (String "finished")
        archived <- call manager base "project_archive" (object ["project_id" .= observationId])
        jsonField "action" archived `shouldBe` Just (String "archived")
      observed <- readTVarIO requests
      map (.requestPath) observed `shouldBe` ["/api/v1/tasks/aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee", "/api/v1/projects/aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee"]
      map (.requestMethod) observed `shouldBe` [methodPut, methodPut]
      mapM_ (\request -> decode request.requestBody `shouldBe` Just (object ["status" .= if request.requestPath == "/api/v1/tasks/aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee" then ("done" :: Text) else "archived"])) observed

  describe "Canonical change-stream integration" $
    it "carries a real MCP proxy mutation through REST into the durable outbox and snapshot" $
      withTestEnv $ \env -> do
        if env.testDb.testDbUnsafeExternal
          then fail "MCP release gate refuses unsafe external DB mode"
          else pure ()
        tracker <- newAccessTracker env.pool 3600
        wsState <- newWSState
        let cfg = trustedMcpConfig
        app <- mkAppWithChangeStream cfg.changeStream id cfg.auth cfg.cors cfg.rateLimit
          env.pool tracker wsState Nothing True
        workspace <- createTestWorkspace env "mcp-change-stream-release-gate"
        withEnvironment "HMEM_MCP_PROVENANCE_TOKEN" trustedMcpProvenance $
          testWithApplication (pure app) $ \port ->
            bracket (newManager defaultManagerSettings) closeManager $ \manager -> do
              let base = "http://127.0.0.1:" <> show port
              created <- callWithKey manager base (Just localMcpBotToken) "project_create"
                (object ["workspace_id" .= workspace.id, "name" .= ("MCP canonical project" :: Text)])
              projectId <- case jsonField "summary" created >>= jsonField "id" of
                Just (String value) -> pure value
                other -> expectationFailure ("MCP create omitted project identity: " <> show other) >> fail "unreachable"

              records <- listOutboxAfter env.pool (WorkspaceScope workspace.id) 0 20
              let matching =
                    [ record
                    | record <- records
                    , jsonPath ["entity", "type"] record.outboxEnvelope == Just (String "project")
                    , jsonPath ["entity", "id"] record.outboxEnvelope == Just (String projectId)
                    ]
              case matching of
                [record] -> do
                  jsonPath ["transaction", "cause"] record.outboxEnvelope `shouldBe` Just (String "mcp")
                  jsonField "invalidations" record.outboxEnvelope `shouldSatisfy` maybe False (contains ("project:" <> projectId))
                  mapM_ (\forbidden -> jsonField forbidden record.outboxEnvelope `shouldBe` Nothing)
                    ["old_values", "new_values", "embedding"]
                _ -> expectationFailure "expected exactly one canonical MCP project record"

              snapshot <- DBPool.runSession env.pool (materializeSnapshot (WorkspaceScope workspace.id))
              snapshot `shouldSatisfy` any (\item ->
                jsonField "kind" item == Just (String "project")
                  && jsonPath ["data", "id"] item == Just (String projectId))
              fetched <- callWithKey manager base (Just localMcpBotToken) "project_detail"
                (object ["project_id" .= projectId])
              jsonField "id" fetched `shouldBe` Just (String projectId)

workspaceId, observationId, gitSha :: Text
workspaceId = "11111111-2222-3333-4444-555555555555"
observationId = "aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee"
gitSha = "0123456789abcdef0123456789abcdef01234567"

observationArguments :: Value
observationArguments = object ["workspace_id" .= workspaceId, "subjects" .= observationSubjects, "git_sha" .= gitSha, "content" .= ("complete observation content" :: Text)]

legacyObservationArguments :: Value
legacyObservationArguments = object ["workspace_id" .= workspaceId, "subject_kind" .= ("file" :: Text), "subject" .= ("src/HMem/Types.hs" :: Text), "git_sha" .= gitSha, "content" .= ("complete observation content" :: Text)]

observationSubjects :: [ObservationSubject]
observationSubjects =
  [ ObservationSubject SubjectFile "src/HMem/Types.hs"
  , ObservationSubject SubjectGlob "my/src/proj/**/*.java"
  ]

observationMatchArguments :: Value
observationMatchArguments = object
  [ "workspace_id" .= workspaceId
  , "paths" .= (["src/HMem/Types.hs", "my/src/proj/Main.java"] :: [Text])
  , "subject_kind" .= ("glob" :: Text)
  , "git_sha" .= gitSha
  , "query" .= ("types" :: Text)
  , "limit" .= (2 :: Int)
  , "offset" .= (0 :: Int)
  ]

withOffset :: Value -> Int -> Value
withOffset (Object values) value = Object (KM.insert "offset" (toJSON value) values)
withOffset arguments _ = arguments

observationListArguments :: Value
observationListArguments = object
  [ "workspace_id" .= workspaceId
  , "subject_kind" .= ("file" :: Text)
  , "subject" .= ("src/HMem/Types.hs" :: Text)
  , "git_sha" .= gitSha
  , "query" .= ("types" :: Text)
  , "limit" .= (2 :: Int)
  , "offset" .= (0 :: Int)
  ]

embedding :: [Double]
embedding = replicate 1536 0.25

similarArguments :: Value
similarArguments = object
  [ "workspace_id" .= workspaceId
  , "subject_kind" .= ("file" :: Text)
  , "subject" .= ("src/HMem/Types.hs" :: Text)
  , "git_sha" .= gitSha
  , "embedding" .= embedding
  , "min_similarity" .= (0.5 :: Double)
  , "limit" .= (2 :: Int)
  , "offset" .= (4 :: Int)
  ]

serverOwnedTools :: [Text]
serverOwnedTools = ["set_workspace", "get_workspace"]

toolSamples :: [(Text, Value)]
toolSamples =
  [ ("workspace_list", object [])
  , ("workspace_register", object ["name" .= ("Repository" :: Text)])
  , ("workspace_update", object ["workspace_id" .= workspaceId, "name" .= ("Renamed workspace" :: Text)])
  , ("search", object ["workspace_id" .= workspaceId, "entity_types" .= (["observation"] :: [Text]), "offset" .= (0 :: Int)])
  , ("observation_create", observationArguments)
  , ("observation_get", object ["observation_id" .= observationId])
  , ("observation_update", object ["observation_id" .= observationId, "content" .= ("replacement" :: Text)])
  , ("observation_list", observationListArguments)
  , ("observation_match", observationMatchArguments)
  , ("observation_delete", object ["observation_id" .= observationId])
  , ("observation_set_embedding", object ["observation_id" .= observationId, "embedding" .= embedding])
  , ("observation_similar", similarArguments)
  , ("project_create", object ["workspace_id" .= workspaceId, "name" .= ("Project" :: Text)])
  , ("project_update", object ["project_id" .= observationId, "name" .= ("Updated project" :: Text)])
  , ("project_detail", object ["project_id" .= observationId])
  , ("project_overview", object ["project_id" .= observationId])
  , ("project_next_tasks", object ["project_id" .= observationId])
  , ( "project_spec"
    , object
        [ "workspace_id" .= workspaceId
        , "name" .= ("Project specification" :: Text)
        , "tasks" .= [object ["title" .= ("Initial task" :: Text)]]
        ]
    )
  , ("project_archive", object ["project_id" .= observationId])
  , ("task_create", object ["workspace_id" .= workspaceId, "title" .= ("Task" :: Text)])
  , ("task_update", object ["task_id" .= observationId, "title" .= ("Updated task" :: Text)])
  , ("task_detail", object ["task_id" .= observationId])
  , ("task_overview", object ["task_id" .= observationId])
  , ("task_dependency", object ["task_id" .= observationId, "depends_on_id" .= workspaceId, "action" .= ("add" :: Text)])
  , ("task_start", object ["task_id" .= observationId])
  , ("task_finish", object ["task_id" .= observationId, "status" .= ("done" :: Text)])
  ]

toolNames :: [Text]
toolNames = [name | Object tool <- toolDefinitions, Just (String name) <- [KM.lookup "name" tool]]

schemaProperties :: Text -> [Text]
schemaProperties name = case [schema | Object tool <- toolDefinitions, KM.lookup "name" tool == Just (String name), Just schema <- [KM.lookup "inputSchema" tool]] of
  Object schema : _ -> case KM.lookup "properties" schema of Just (Object properties) -> Key.toText <$> KM.keys properties; _ -> []
  _ -> []

schemaRequired :: Text -> [Text]
schemaRequired name = case [schema | Object tool <- toolDefinitions, KM.lookup "name" tool == Just (String name), Just schema <- [KM.lookup "inputSchema" tool]] of
  Object schema : _ -> case KM.lookup "required" schema of Just (Array fields) -> [field | String field <- toList fields]; _ -> []
  _ -> []

schemaAdditionalProperties :: Text -> Maybe Value
schemaAdditionalProperties name = case [schema | Object tool <- toolDefinitions, KM.lookup "name" tool == Just (String name), Just schema <- [KM.lookup "inputSchema" tool]] of
  Object schema : _ -> KM.lookup "additionalProperties" schema
  _ -> Nothing

toolDescription :: Text -> Maybe Text
toolDescription name = case [description | Object tool <- toolDefinitions, KM.lookup "name" tool == Just (String name), Just (String description) <- [KM.lookup "description" tool]] of description : _ -> Just description; [] -> Nothing

toolDescriptionIs :: Text -> Text -> Expectation
toolDescriptionIs name expected = toolDescription name `shouldBe` Just expected

planningGuidance :: Text
planningGuidance = T.intercalate "\n" $
  [ toolDescription name
  | name <- ["project_create", "project_update", "project_spec", "task_create", "task_update", "task_start", "task_finish"]
  ] >>= maybe [] pure

planningGuidanceContract :: Text -> Bool
planningGuidanceContract text =
  all (`T.isInfixOf` text)
    [ "durable specification"
    , "not logs of progress or updates"
    , "status records"
    , "atomic work as subtasks"
    ]

observationGuidance :: Text
observationGuidance = T.intercalate "\n" $
  [ toolDescription name
  | name <- ["search", "observation_create", "observation_update", "observation_list", "observation_match", "observation_similar"]
  ] >>= maybe [] pure

observationGuidanceContract :: Text -> Bool
observationGuidanceContract text =
  all (`T.isInfixOf` text)
    [ "durable, non-obvious repository insight"
    , "file or glob subjects"
    , "repository-relative"
    , "immutable provenance"
    , "staleness-audit sentinel"
    , "not timeless proof"
    ]

schemaProperty :: Text -> Text -> Maybe Value
schemaProperty toolName propertyName = case
  [ property
  | Object tool <- toolDefinitions
  , KM.lookup "name" tool == Just (String toolName)
  , Just (Object inputSchema) <- [KM.lookup "inputSchema" tool]
  , Just (Object properties) <- [KM.lookup "properties" inputSchema]
  , Just property <- [KM.lookup (Key.fromText propertyName) properties]
  ] of
    property : _ -> Just property
    [] -> Nothing

schemaPropertyDescriptionIs :: Text -> Text -> Text -> Expectation
schemaPropertyDescriptionIs toolName propertyName expected =
  (schemaProperty toolName propertyName >>= jsonField "description")
    `shouldBe` Just (String expected)

projectSpecTaskPropertyDescriptionIs :: Text -> Text -> Expectation
projectSpecTaskPropertyDescriptionIs propertyName expected =
  (schemaProperty "project_spec" "tasks" >>= jsonField "items" >>= jsonField "properties" >>= objectField propertyName >>= jsonField "description")
    `shouldBe` Just (String expected)

objectField :: Text -> Value -> Maybe Value
objectField name (Object fields) = KM.lookup (Key.fromText name) fields
objectField _ _ = Nothing

nullableWorkspaceTypes :: Value -> Bool
nullableWorkspaceTypes (Array values) = map (jsonField "type") (toList values) == [Just (String "string"), Just (String "null")]
nullableWorkspaceTypes _ = False

isUnknown :: Text -> Either String ToolCall -> Bool
isUnknown name = \case Left message -> ("Unknown tool: " <> T.unpack name) == message; Right _ -> False
isLeft :: Either a b -> Bool
isLeft = \case Left _ -> True; Right _ -> False
isRight :: Either a b -> Bool
isRight = not . isLeft

isMcpError :: Value -> Bool
isMcpError value = jsonField "isError" value == Just (Bool True)

isJsonRpcMcpError :: Value -> Bool
isJsonRpcMcpError value = maybe True isMcpError (jsonField "result" value)

jsonField :: Text -> Value -> Maybe Value
jsonField key (Object objectValue) = KM.lookup (Key.fromText key) objectValue
jsonField _ _ = Nothing
jsonPath :: [Text] -> Value -> Maybe Value
jsonPath keys value = foldl (\current key -> current >>= jsonField key) (Just value) keys
hasFields :: [Text] -> Value -> Bool
hasFields keys value = all (\key -> jsonField key value /= Nothing) keys
contains :: Text -> Value -> Bool
contains needle = T.isInfixOf needle . TE.decodeUtf8 . BL.toStrict . encode
arrayFirst :: (Value -> Bool) -> Value -> Bool
arrayFirst predicate (Array values) = case toList values of value : _ -> predicate value; [] -> False
arrayFirst _ _ = False

call :: Manager -> String -> Text -> Value -> IO Value
call manager base = callWithKey manager base Nothing

callWithKey :: Manager -> String -> Maybe Text -> Text -> Value -> IO Value
callWithKey manager base apiKey name arguments = do
  response <- handleToolCall manager base apiKey (object ["name" .= name, "arguments" .= arguments])
  case jsonField "content" response >>= arrayText >>= (either (const Nothing) Just . eitherDecodeStrict' . TE.encodeUtf8) of
    Just value -> pure value
    Nothing -> expectationFailure ("Expected MCP JSON response, got " <> show response) >> pure Null
  where
    arrayText (Array values) = case toList values of
      Object first : _ -> case KM.lookup "text" first of Just (String text) -> Just text; _ -> Nothing
      _ -> Nothing
    arrayText _ = Nothing

serverToolCall :: Manager -> TVar Bool -> TVar (Maybe UUID) -> Text -> Value -> IO (Maybe Value)
serverToolCall manager initialized workspaceContext name arguments =
  handleStdioLine manager "http://unused.invalid" Nothing initialized workspaceContext
    (BL.toStrict (encode (object
      [ "jsonrpc" .= ("2.0" :: Text)
      , "id" .= ("workspace-context" :: Text)
      , "method" .= ("tools/call" :: Text)
      , "params" .= object ["name" .= name, "arguments" .= arguments]
      ])))

jsonRpcToolCall :: Manager -> String -> TVar Bool -> TVar (Maybe UUID) -> Text -> Value -> IO (Maybe Value)
jsonRpcToolCall manager base initialized workspaceContext name arguments =
  handleStdioLine manager base Nothing initialized workspaceContext
    (BL.toStrict (encode (object
      [ "jsonrpc" .= ("2.0" :: Text)
      , "id" .= ("jsonrpc-tool-call" :: Text)
      , "method" .= ("tools/call" :: Text)
      , "params" .= object ["name" .= name, "arguments" .= arguments]
      ])))

compactFixtures :: IO Value
compactFixtures = do
  let candidates = ["test/fixtures/mcp-compact-responses.json", "hmem-mcp/test/fixtures/mcp-compact-responses.json"]
  available <- filterM doesFileExist candidates
  case available of
    path : _ -> do
      decoded <- eitherDecode <$> BL.readFile path
      case decoded of Right fixture -> pure fixture; Left err -> expectationFailure err >> pure Null
    [] -> expectationFailure "mcp compact response fixture not found" >> pure Null

fixturePayload :: Value -> Text -> Value
fixturePayload fixtures name = case jsonField name fixtures >>= jsonField "payload" of
  Just payload -> payload
  Nothing -> error ("missing fixture payload: " <> T.unpack name)

data RequestInfo = RequestInfo
  { requestMethod :: ByteString
  , requestPath :: String
  , requestQuery :: ByteString
  , requestBody :: BL.ByteString
  , authorization :: Maybe ByteString
  , requestChangeCause :: Maybe ByteString
  , requestMcpProvenance :: Maybe ByteString
  , requestId :: Maybe ByteString
  }

withMock :: TVar [RequestInfo] -> (Manager -> String -> IO a) -> IO a
withMock requests action = testWithApplication (pure (mockApp requests)) $ \port ->
  bracket (newManager defaultManagerSettings) closeManager $ \manager ->
    action manager ("http://127.0.0.1:" <> show port)

trustedMcpProvenance :: String
trustedMcpProvenance = "release-gate-mcp-provenance"

localMcpBotToken :: Text
localMcpBotToken = "release-gate-local-mcp-bot"

trustedMcpConfig :: Config.HMemConfig
trustedMcpConfig =
  let cfg = Config.defaultConfig
  in cfg
    { Config.auth = cfg.auth
        { Config.mode = Config.AuthModeLocal
        , Config.enabled = False
        , Config.apiKey = Nothing
        , Config.local = Config.LocalAuthConfig
            { Config.bootstrapEnabled = False
            , Config.allowRemoteBootstrap = False
            , Config.botTokens =
                [ Config.LocalBotTokenConfig
                    { Config.label = "Release gate MCP bot"
                    , Config.token = localMcpBotToken
                    }
                ]
            }
        , Config.mcpProvenanceToken = Just (T.pack trustedMcpProvenance)
        }
    }

-- These cases use the complete WAI application in deployed mode, not a
-- hand-written JSON mock.  The injected principals model the three distinct
-- authorization outcomes while keeping the MCP HTTP proxy as the client
-- under test.
renameProxyErrorCases :: [(Principal, Bool, Text, Text)]
renameProxyErrorCases =
  [ (renameSuperadmin, False, "   ", "[HTTP_400] name must not be empty")
  , (renameUnprivileged, False, "authorized-shape", "[HTTP_403] Workspace edit permission is required.")
  , (renameSuperadmin, True, "authorized-shape", "[HTTP_404] Workspace not found.")
  ]

renameSuperadmin, renameUnprivileged :: Principal
renameSuperadmin = Principal
  { actorType = ActorBot
  , actorId = "rename-proxy-superadmin"
  , actorLabel = "Rename proxy superadmin"
  , authority = PrincipalSyntheticLocalSuperadmin
  }
renameUnprivileged = Principal
  { actorType = ActorBot
  , actorId = "rename-proxy-unprivileged"
  , actorLabel = "Rename proxy unprivileged"
  , authority = PrincipalNoAuthority
  }

missingWorkspaceId :: UUID
missingWorkspaceId = read "00000000-0000-0000-0000-000000000001"

withRenameProxyApp :: Principal -> (UUID -> Manager -> String -> IO a) -> IO a
withRenameProxyApp principal action =
  withTestEnv $ \env -> do
    tracker <- newAccessTracker env.pool 3600
    wsState <- newWSState
    let cfg = Config.defaultConfig
          { Config.auth = Config.defaultConfig.auth
              { Config.mode = Config.AuthModeDeployed
              , Config.enabled = False
              }
          }
        asPrincipal application request respond =
          withPrincipalContext (Just principal) (application request respond)
    app <- mkAppWithChangeStream cfg.changeStream asPrincipal cfg.auth cfg.cors cfg.rateLimit
      env.pool tracker wsState Nothing True
    workspace <- createTestWorkspace env "workspace-rename-mcp-proxy-errors"
    testWithApplication (pure app) $ \port ->
      bracket (newManager defaultManagerSettings) closeManager $ \manager ->
        action workspace.id manager ("http://127.0.0.1:" <> show port)

withEnvironment :: String -> String -> IO a -> IO a
withEnvironment name value = bracket acquire restore . const
  where
    acquire = do
      previous <- lookupEnv name
      setEnv name value
      pure previous
    restore Nothing = unsetEnv name
    restore (Just previous) = setEnv name previous

mockApp :: TVar [RequestInfo] -> Wai.Application
mockApp requests request respond = do
  body <- Wai.strictRequestBody request
  atomically $ modifyTVar' requests (<> [RequestInfo request.requestMethod (T.unpack (TE.decodeUtf8 request.rawPathInfo)) request.rawQueryString body (lookup "Authorization" request.requestHeaders) (lookup "X-HMem-Change-Cause" request.requestHeaders) (lookup "X-HMem-MCP-Provenance" request.requestHeaders) (lookup "X-Request-Id" request.requestHeaders)])
  case request.requestMethod of
    method | method == methodDelete && "/dependencies/" `T.isInfixOf` TE.decodeUtf8 request.rawPathInfo -> respond $ Wai.responseLBS status200 [("Content-Type", "application/json")] (encode (responseFor request.requestMethod request.rawPathInfo request.rawQueryString body))
    method | method == methodDelete -> respond $ Wai.responseLBS status204 [] ""
    method | method == methodPut && "/embedding" `T.isSuffixOf` TE.decodeUtf8 request.rawPathInfo -> respond $ Wai.responseLBS status204 [] ""
    _ -> respond $ Wai.responseLBS status200 [("Content-Type", "application/json")] (encode (responseFor request.requestMethod request.rawPathInfo request.rawQueryString body))

withErrorMock :: (Manager -> String -> IO a) -> IO a
withErrorMock action = testWithApplication (pure errorApp) $ \port ->
  bracket (newManager defaultManagerSettings) closeManager $ \manager ->
    action manager ("http://127.0.0.1:" <> show port)

errorApp :: Wai.Application
errorApp _ respond = respond $ Wai.responseLBS status400 [("Content-Type", "text/plain")] "pgvector extension is required"

withStatusMock :: TVar [RequestInfo] -> (Manager -> String -> IO a) -> IO a
withStatusMock requests action = testWithApplication (pure (statusApp requests)) $ \port ->
  bracket (newManager defaultManagerSettings) closeManager $ \manager ->
    action manager ("http://127.0.0.1:" <> show port)

withWorkspaceStructuredStatusMock :: Status -> Text -> (Manager -> String -> IO a) -> IO a
withWorkspaceStructuredStatusMock status message action =
  testWithApplication (pure (workspaceStructuredStatusApp status message)) $ \port ->
    bracket (newManager defaultManagerSettings) closeManager $ \manager ->
      action manager ("http://127.0.0.1:" <> show port)

withDependencyCycleMock :: (Manager -> String -> IO a) -> IO a
withDependencyCycleMock action =
  testWithApplication (pure dependencyCycleApp) $ \port ->
    bracket (newManager defaultManagerSettings) closeManager $ \manager ->
      action manager ("http://127.0.0.1:" <> show port)

dependencyCycleApp :: Wai.Application
dependencyCycleApp _ respond = respond $ Wai.responseLBS status400 [("Content-Type", "application/json")]
  (encode (object ["error" .= ("dependency_cycle" :: Text), "message" .= ("Task dependency would create a cycle" :: Text)]))

workspaceStructuredStatusApp :: Status -> Text -> Wai.Application
workspaceStructuredStatusApp status message _ respond =
  respond $ Wai.responseLBS status [("Content-Type", "application/json")] (encode (object ["error" .= ("workspace_error" :: Text), "message" .= message]))

statusApp :: TVar [RequestInfo] -> Wai.Application
statusApp requests request respond = do
  body <- Wai.strictRequestBody request
  atomically $ modifyTVar' requests (<> [RequestInfo request.requestMethod (T.unpack (TE.decodeUtf8 request.rawPathInfo)) request.rawQueryString body (lookup "Authorization" request.requestHeaders) (lookup "X-HMem-Change-Cause" request.requestHeaders) (lookup "X-HMem-MCP-Provenance" request.requestHeaders) (lookup "X-Request-Id" request.requestHeaders)])
  let status
        | request.rawPathInfo == "/api/v1/observations/match" = status500
        | request.rawPathInfo == "/api/v1/observations/similar" = status400
        | "/embedding" `T.isSuffixOf` TE.decodeUtf8 request.rawPathInfo = status409
        | request.requestMethod == methodDelete = status404
        | request.rawPathInfo == "/api/v1/observations" = status403
        | otherwise = status401
      message = "status " <> T.pack (show (statusCode status))
  respond $ Wai.responseLBS status [("Content-Type", "text/plain")] (BL.fromStrict (TE.encodeUtf8 message))

responseFor :: ByteString -> ByteString -> ByteString -> BL.ByteString -> Value
responseFor method path rawQuery body
  | path == "/api/v1/search" = object ["observations" .= [observation], "projects" .= ([] :: [Value]), "tasks" .= ([] :: [Value])]
  | path == "/api/v1/observations/match" && "\"offset\":2" `T.isInfixOf` TE.decodeUtf8 (BL.toStrict body) = object ["items" .= ([] :: [Value]), "has_more" .= False]
  | path == "/api/v1/observations/match" = object ["items" .= [match, match], "has_more" .= True]
  | path == "/api/v1/observations/similar" = toJSON [object ["observation" .= observation, "similarity" .= (0.75 :: Double)]]
  | method == methodPost && path == "/api/v1/tasks/aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee/dependencies" = dependencyMutation "add"
  | method == methodDelete && path == "/api/v1/tasks/aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee/dependencies/11111111-2222-3333-4444-555555555555" = dependencyMutation "remove"
  | method == methodPost && path == "/api/v1/observations" = observation
  | method == methodPost && path == "/api/v1/projects" = project
  | method == methodPost && path == "/api/v1/tasks" = task
  | method == methodPut && path == "/api/v1/workspaces/11111111-2222-3333-4444-555555555555" = object ["id" .= workspaceId, "name" .= ("Renamed" :: Text), "workspace_type" .= ("repository" :: Text)]
  | path == "/api/v1/observations" && "offset=2" `T.isInfixOf` TE.decodeUtf8 rawQuery = object ["items" .= [observation], "has_more" .= False]
  | path == "/api/v1/observations" = object ["items" .= [observation, observation], "has_more" .= True]
  | path == "/api/v1/observations/aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee" = observation
  | path == "/api/v1/tasks/aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee" = task
  | path == "/api/v1/projects/aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee" = project
  | otherwise = object []
  where
    observation = object ["id" .= observationId, "workspace_id" .= workspaceId, "subjects" .= observationSubjects, "subject_kind" .= ("file" :: Text), "subject" .= ("src/HMem/Types.hs" :: Text), "git_sha" .= gitSha, "content" .= ("complete observation content" :: Text), "content_preview" .= ("complete observation content" :: Text)]
    match = object
      [ "observation" .= observation
      , "path_matches" .=
          [ object ["path" .= ("my/src/proj/Main.java" :: Text), "matched_subjects" .= [ObservationSubject SubjectGlob "my/src/proj/**/*.java"]]
          , object ["path" .= ("src/HMem/Types.hs" :: Text), "matched_subjects" .= [ObservationSubject SubjectFile "src/HMem/Types.hs"]]
          ]
      , "matched_paths" .= (["my/src/proj/Main.java", "src/HMem/Types.hs"] :: [Text])
      , "matched_subjects" .= observationSubjects
      ]
    task = object ["id" .= observationId, "workspace_id" .= workspaceId, "title" .= ("Task" :: Text), "status" .= ("done" :: Text), "priority" .= (5 :: Int)]
    project = object ["id" .= observationId, "workspace_id" .= workspaceId, "name" .= ("Project" :: Text), "status" .= ("archived" :: Text), "priority" .= (5 :: Int)]
    dependencyMutation :: Text -> Value
    dependencyMutation action = object ["action" .= action, "task_id" .= observationId, "depends_on_id" .= workspaceId, "affected_tasks" .= [object ["task" .= task, "previous_status" .= ("todo" :: Text), "current_status" .= ("blocked" :: Text), "auto_blocked" .= True, "open_dependency_count" .= (1 :: Int), "reason" .= ("blocked_by_open_dependencies" :: Text)]]]
