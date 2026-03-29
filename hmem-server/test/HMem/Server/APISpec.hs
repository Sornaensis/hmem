{-# OPTIONS_GHC -Wno-x-partial -Wno-incomplete-uni-patterns #-}

module HMem.Server.APISpec (spec) where

import Data.Aeson (decode, encode, object, (.=), toJSON, Value(..))
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Maybe (isJust)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.UUID (UUID)
import Network.HTTP.Types
import Network.Wai (Application, defaultRequest)
import Network.Wai qualified as Wai
import Network.Wai.Test (SResponse(..), runSession, srequest, SRequest(..))
import Test.Hspec

import HMem.Config (CorsConfig(..))
import HMem.Config qualified as Config
import HMem.DB.TestHarness
import HMem.Server.AccessTracker (newAccessTracker)
import HMem.Server.App (mkApp)
import HMem.Types

------------------------------------------------------------------------
-- WAI test helpers
------------------------------------------------------------------------

withApp :: (Application -> IO a) -> IO a
withApp action = withAppConfig Config.defaultConfig action

withAppConfig :: Config.HMemConfig -> (Application -> IO a) -> IO a
withAppConfig cfg action = withAppEnvConfig cfg (\_ app -> action app)

withAppEnv :: (TestEnv -> Application -> IO a) -> IO a
withAppEnv = withAppEnvConfig Config.defaultConfig

withAppEnvConfig :: Config.HMemConfig -> (TestEnv -> Application -> IO a) -> IO a
withAppEnvConfig cfg action = withTestEnv $ \env -> do
  tracker <- newAccessTracker env.pool 3600
  let cfg' = cfg { Config.cors = CorsConfig { allowedOrigins = ["*"] } }
  app <- mkApp id cfg'.auth cfg'.cors cfg'.rateLimit env.pool tracker True
  action env app

testAuthCfg :: Config.HMemConfig
testAuthCfg = Config.defaultConfig
  { Config.auth = Config.AuthConfig
      { Config.enabled = True
      , Config.apiKey = Just "test-secret"
      }
  }

testRateLimitCfg :: Config.HMemConfig
testRateLimitCfg = Config.defaultConfig
  { Config.rateLimit = Config.RateLimitConfig
      { Config.rlEnabled = True
      , Config.rlRequestsPerSecond = 1.0
      , Config.rlBurst = 1
      }
  }

runReqWithHeaders :: Application -> Method -> BS.ByteString -> [(HeaderName, BS.ByteString)] -> LBS.ByteString -> IO SResponse
runReqWithHeaders app method fullPath headers body =
  runSession (srequest $ SRequest req body) app
  where
    (rawPath, rawQS) = BS.break (== 0x3F) fullPath  -- split on '?'
    req = defaultRequest
      { Wai.requestMethod  = method
      , Wai.rawPathInfo    = rawPath
      , Wai.pathInfo       = filter (not . T.null) $ T.split (== '/')
                               $ decodeUtf8 rawPath
      , Wai.rawQueryString = rawQS
      , Wai.queryString    = parseQuery rawQS
      , Wai.requestHeaders = [("Content-Type", "application/json")] <> headers
      }

runReq :: Application -> Method -> BS.ByteString -> LBS.ByteString -> IO SResponse
runReq app method fullPath body =
  runReqWithHeaders app method fullPath [] body

get_ :: Application -> BS.ByteString -> IO SResponse
get_ app path = runReq app methodGet path ""

postJSON :: Application -> BS.ByteString -> Value -> IO SResponse
postJSON app path body = runReq app methodPost path (encode body)

putJSON :: Application -> BS.ByteString -> Value -> IO SResponse
putJSON app path body = runReq app methodPut path (encode body)

del :: Application -> BS.ByteString -> IO SResponse
del app path = runReq app methodDelete path ""

respStatus :: SResponse -> Int
respStatus = statusCode . simpleStatus

respBody :: SResponse -> LBS.ByteString
respBody = simpleBody

uuidPath :: BS.ByteString -> UUID -> BS.ByteString
uuidPath prefix uid = prefix <> "/" <> encodeUtf8 (T.pack (show uid))

------------------------------------------------------------------------
-- Tests
------------------------------------------------------------------------

spec :: Spec
spec = around withApp $ do

  describe "GET /api/v1/workspaces" $ do
    it "returns empty list initially" $ \app -> do
      resp <- get_ app "/api/v1/workspaces"
      respStatus resp `shouldBe` 200
      decode (respBody resp) `shouldBe` Just
        (object ["items" .= ([] :: [Value]), "has_more" .= False])

  describe "POST /api/v1/workspaces" $ do
    it "creates a workspace" $ \app -> do
      resp <- postJSON app "/api/v1/workspaces"
        (object ["name" .= ("test-ws" :: T.Text)])
      respStatus resp `shouldBe` 200
      let Just ws = decode (respBody resp) :: Maybe Workspace
      ws.name `shouldBe` "test-ws"

    it "rejects oversized workspace names" $ \app -> do
      resp <- postJSON app "/api/v1/workspaces"
        (object ["name" .= T.replicate (maxNameBytes + 1) "a"])
      respStatus resp `shouldBe` 400

  describe "workspace + memory flow" $ do
    it "creates workspace, memory, retrieves, updates, deletes" $ \app -> do
      -- Create workspace
      wsResp <- postJSON app "/api/v1/workspaces"
        (object ["name" .= ("flow-ws" :: T.Text)])
      respStatus wsResp `shouldBe` 200
      let Just ws = decode (respBody wsResp) :: Maybe Workspace

      -- Create memory
      memResp <- postJSON app "/api/v1/memories"
        (object
          [ "workspace_id" .= ws.id
          , "content" .= ("Test memory content" :: T.Text)
          , "memory_type" .= ("short_term" :: T.Text)
          , "importance" .= (7 :: Int)
          , "tags" .= (["test", "flow"] :: [T.Text])
          ])
      respStatus memResp `shouldBe` 200
      let Just mem = decode (respBody memResp) :: Maybe Memory
      mem.content `shouldBe` "Test memory content"
      mem.importance `shouldBe` 7

      -- Get memory
      getResp <- get_ app (uuidPath "/api/v1/memories" mem.id)
      respStatus getResp `shouldBe` 200

      -- Update memory
      upResp <- putJSON app (uuidPath "/api/v1/memories" mem.id)
        (object ["content" .= ("Updated content" :: T.Text)])
      respStatus upResp `shouldBe` 200
      let Just updated = decode (respBody upResp) :: Maybe Memory
      updated.content `shouldBe` "Updated content"

      -- Delete memory
      delResp <- del app (uuidPath "/api/v1/memories" mem.id)
      respStatus delResp `shouldBe` 200

      -- Confirm gone
      getResp2 <- get_ app (uuidPath "/api/v1/memories" mem.id)
      respStatus getResp2 `shouldBe` 404

      purgeResp <- del app (uuidPath "/api/v1/memories" mem.id <> "/purge")
      respStatus purgeResp `shouldBe` 200

  describe "project flow" $ do
    it "creates workspace, project, lists, updates status" $ \app -> do
      wsResp <- postJSON app "/api/v1/workspaces"
        (object ["name" .= ("proj-flow-ws" :: T.Text)])
      let Just ws = decode (respBody wsResp) :: Maybe Workspace

      -- Create project
      projResp <- postJSON app "/api/v1/projects"
        (object
          [ "workspace_id" .= ws.id
          , "name" .= ("Test Project" :: T.Text)
          , "priority" .= (8 :: Int)
          ])
      respStatus projResp `shouldBe` 200
      let Just proj = decode (respBody projResp) :: Maybe Project
      proj.name `shouldBe` "Test Project"
      proj.status `shouldBe` ProjActive

      -- List projects
      listResp <- get_ app ("/api/v1/projects?workspace_id=" <> encodeUtf8 (T.pack (show ws.id)))
      respStatus listResp `shouldBe` 200
      let Just projs = decode (respBody listResp) :: Maybe (PaginatedResult Project)
      length projs.items `shouldBe` 1

      -- Update project
      upResp <- putJSON app (uuidPath "/api/v1/projects" proj.id)
        (object ["status" .= ("completed" :: T.Text)])
      respStatus upResp `shouldBe` 200
      let Just updated = decode (respBody upResp) :: Maybe Project
      updated.status `shouldBe` ProjCompleted

  describe "task flow" $ do
    it "creates project, task, marks done" $ \app -> do
      wsResp <- postJSON app "/api/v1/workspaces"
        (object ["name" .= ("task-flow-ws" :: T.Text)])
      let Just ws = decode (respBody wsResp) :: Maybe Workspace
      projResp <- postJSON app "/api/v1/projects"
        (object ["workspace_id" .= ws.id, "name" .= ("TProj" :: T.Text)])
      let Just proj = decode (respBody projResp) :: Maybe Project

      -- Create task
      taskResp <- postJSON app "/api/v1/tasks"
        (object
          [ "workspace_id" .= ws.id
          , "project_id" .= proj.id
          , "title" .= ("Do something" :: T.Text)
          , "priority" .= (6 :: Int)
          ])
      respStatus taskResp `shouldBe` 200
      let Just task = decode (respBody taskResp) :: Maybe Task
      task.title `shouldBe` "Do something"
      task.status `shouldBe` Todo

      -- Mark done
      doneResp <- putJSON app (uuidPath "/api/v1/tasks" task.id)
        (object ["status" .= ("done" :: T.Text)])
      respStatus doneResp `shouldBe` 200
      let Just done = decode (respBody doneResp) :: Maybe Task
      done.status `shouldBe` Done
      done.completedAt `shouldSatisfy` isJust

  describe "search" $ do
    it "full-text search finds matching memories" $ \app -> do
      wsResp <- postJSON app "/api/v1/workspaces"
        (object ["name" .= ("search-ws" :: T.Text)])
      let Just ws = decode (respBody wsResp) :: Maybe Workspace
      _ <- postJSON app "/api/v1/memories"
        (object
          [ "workspace_id" .= ws.id
          , "content" .= ("Haskell is a purely functional programming language" :: T.Text)
          , "memory_type" .= ("short_term" :: T.Text)
          ])
      _ <- postJSON app "/api/v1/memories"
        (object
          [ "workspace_id" .= ws.id
          , "content" .= ("Python is a dynamically typed scripting language" :: T.Text)
          , "memory_type" .= ("short_term" :: T.Text)
          ])
      searchResp <- postJSON app "/api/v1/memories/search"
        (object
          [ "workspace_id" .= ws.id
          , "query" .= ("haskell functional" :: T.Text)
          ])
      respStatus searchResp `shouldBe` 200
      let Just results = decode (respBody searchResp) :: Maybe [Memory]
      length results `shouldSatisfy` (>= 1)

  describe "error handling" $ do
    it "returns 404 for nonexistent memory" $ \app -> do
      resp <- get_ app "/api/v1/memories/00000000-0000-0000-0000-000000000099"
      respStatus resp `shouldBe` 404

    it "returns 404 for nonexistent project" $ \app -> do
      resp <- get_ app "/api/v1/projects/00000000-0000-0000-0000-000000000099"
      respStatus resp `shouldBe` 404

    it "returns 404 for nonexistent task" $ \app -> do
      resp <- get_ app "/api/v1/tasks/00000000-0000-0000-0000-000000000099"
      respStatus resp `shouldBe` 404

    it "returns all memories when workspace_id is omitted" $ \app -> do
      resp <- get_ app "/api/v1/memories"
      respStatus resp `shouldBe` 200

    it "sanitizes unique-violation details" $ \app -> do
      firstResp <- postJSON app "/api/v1/workspaces"
        (object
          [ "name" .= ("safe-errors-a" :: T.Text)
          , "path" .= ("/tmp/same-workspace-path" :: T.Text)
          ])
      respStatus firstResp `shouldBe` 200

      secondResp <- postJSON app "/api/v1/workspaces"
        (object
          [ "name" .= ("safe-errors-b" :: T.Text)
          , "path" .= ("/tmp/same-workspace-path" :: T.Text)
          ])
      respStatus secondResp `shouldBe` 409

      let bodyText = decodeUtf8 (LBS.toStrict (respBody secondResp))
      bodyText `shouldSatisfy` T.isInfixOf "Resource already exists"
      bodyText `shouldSatisfy` T.isInfixOf "\"error\":\"conflict\""
      bodyText `shouldSatisfy` (not . T.isInfixOf "duplicate key value violates unique constraint")
      bodyText `shouldSatisfy` (not . T.isInfixOf "uq_workspace")

  describe "GET /health" $ do
    it "returns 200 with status ok" $ \app -> do
      resp <- get_ app "/api/v1/health"
      respStatus resp `shouldBe` 200
      let Just body = decode (respBody resp) :: Maybe Value
      body `shouldSatisfy` \v -> case v of
        Object _ -> True
        _        -> False

  describe "optional bearer auth" $ do
    it "returns 401 when auth is enabled and the bearer token is missing" $ \_ ->
      withAppConfig testAuthCfg $ \app -> do
        resp <- get_ app "/api/v1/health"
        respStatus resp `shouldBe` 401

    it "allows requests when auth is enabled and the bearer token matches" $ \_ ->
      withAppConfig testAuthCfg $ \app -> do
        resp <- runReqWithHeaders app methodGet "/api/v1/health"
          [("Authorization", "Bearer test-secret")]
          ""
        respStatus resp `shouldBe` 200

  describe "rate limiting" $ do
    it "returns 429 when the configured burst is exceeded" $ \_ ->
      withAppConfig testRateLimitCfg $ \app -> do
        firstResp <- get_ app "/api/v1/health"
        secondResp <- get_ app "/api/v1/health"
        respStatus firstResp `shouldBe` 200
        respStatus secondResp `shouldBe` 429

  describe "list filtering" $ do
    it "filters memories by created_after" $ \app -> do
      wsResp <- postJSON app "/api/v1/workspaces"
        (object ["name" .= ("memory-filter-ws" :: T.Text)])
      let Just ws = decode (respBody wsResp) :: Maybe Workspace

      firstMemResp <- postJSON app "/api/v1/memories"
        (object
          [ "workspace_id" .= ws.id
          , "content" .= ("older memory" :: T.Text)
          , "memory_type" .= ("short_term" :: T.Text)
          ])
      secondMemResp <- postJSON app "/api/v1/memories"
        (object
          [ "workspace_id" .= ws.id
          , "content" .= ("newer memory" :: T.Text)
          , "memory_type" .= ("short_term" :: T.Text)
          ])
      let Just firstMem = decode (respBody firstMemResp) :: Maybe Memory
      let Just secondMem = decode (respBody secondMemResp) :: Maybe Memory

      resp <- get_ app
        ( "/api/v1/memories?workspace_id=" <> encodeUtf8 (T.pack (show ws.id))
       <> "&created_after=" <> encodeUtf8 (T.pack (iso8601Show secondMem.createdAt))
        )
      respStatus resp `shouldBe` 200
      let Just page = decode (respBody resp) :: Maybe (PaginatedResult Memory)
      map (\item -> item.id) page.items `shouldBe` [secondMem.id]
      page.items `shouldNotSatisfy` any ((== firstMem.id) . (\item -> item.id))

    it "filters projects by updated_after" $ \app -> do
      wsResp <- postJSON app "/api/v1/workspaces"
        (object ["name" .= ("project-filter-ws" :: T.Text)])
      let Just ws = decode (respBody wsResp) :: Maybe Workspace

      oldProjResp <- postJSON app "/api/v1/projects"
        (object ["workspace_id" .= ws.id, "name" .= ("Old Project" :: T.Text)])
      newProjResp <- postJSON app "/api/v1/projects"
        (object ["workspace_id" .= ws.id, "name" .= ("Updated Project" :: T.Text)])
      let Just oldProj = decode (respBody oldProjResp) :: Maybe Project
      let Just newProj = decode (respBody newProjResp) :: Maybe Project
      updatedProjResp <- putJSON app (uuidPath "/api/v1/projects" newProj.id)
        (object ["status" .= ("paused" :: T.Text)])
      let Just updatedProj = decode (respBody updatedProjResp) :: Maybe Project

      resp <- get_ app
        ( "/api/v1/projects?workspace_id=" <> encodeUtf8 (T.pack (show ws.id))
       <> "&updated_after=" <> encodeUtf8 (T.pack (iso8601Show updatedProj.updatedAt))
        )
      respStatus resp `shouldBe` 200
      let Just page = decode (respBody resp) :: Maybe (PaginatedResult Project)
      map (\item -> item.id) page.items `shouldBe` [updatedProj.id]
      page.items `shouldNotSatisfy` any ((== oldProj.id) . (\item -> item.id))

    it "filters tasks by priority" $ \app -> do
      wsResp <- postJSON app "/api/v1/workspaces"
        (object ["name" .= ("task-filter-ws" :: T.Text)])
      let Just ws = decode (respBody wsResp) :: Maybe Workspace
      projResp <- postJSON app "/api/v1/projects"
        (object ["workspace_id" .= ws.id, "name" .= ("Task Filter Project" :: T.Text)])
      let Just proj = decode (respBody projResp) :: Maybe Project

      lowTaskResp <- postJSON app "/api/v1/tasks"
        (object
          [ "workspace_id" .= ws.id
          , "project_id" .= proj.id
          , "title" .= ("Low priority" :: T.Text)
          , "priority" .= (2 :: Int)
          ])
      highTaskResp <- postJSON app "/api/v1/tasks"
        (object
          [ "workspace_id" .= ws.id
          , "project_id" .= proj.id
          , "title" .= ("High priority" :: T.Text)
          , "priority" .= (8 :: Int)
          ])
      let Just lowTask = decode (respBody lowTaskResp) :: Maybe Task
      let Just highTask = decode (respBody highTaskResp) :: Maybe Task

      resp <- get_ app
        ( "/api/v1/tasks?workspace_id=" <> encodeUtf8 (T.pack (show ws.id))
       <> "&priority=8"
        )
      respStatus resp `shouldBe` 200
      let Just page = decode (respBody resp) :: Maybe (PaginatedResult Task)
      map (\item -> item.id) page.items `shouldBe` [highTask.id]
      page.items `shouldNotSatisfy` any ((== lowTask.id) . (\item -> item.id))

  describe "memory links" $ do
    it "creates and retrieves memory links" $ \app -> do
      wsResp <- postJSON app "/api/v1/workspaces"
        (object ["name" .= ("link-ws" :: T.Text)])
      let Just ws = decode (respBody wsResp) :: Maybe Workspace
      m1Resp <- postJSON app "/api/v1/memories"
        (object ["workspace_id" .= ws.id, "content" .= ("mem A" :: T.Text)
                , "memory_type" .= ("short_term" :: T.Text)])
      let Just m1 = decode (respBody m1Resp) :: Maybe Memory
      m2Resp <- postJSON app "/api/v1/memories"
        (object ["workspace_id" .= ws.id, "content" .= ("mem B" :: T.Text)
                , "memory_type" .= ("short_term" :: T.Text)])
      let Just m2 = decode (respBody m2Resp) :: Maybe Memory

      -- Link m1 -> m2
      linkResp <- postJSON app (uuidPath "/api/v1/memories" m1.id <> "/links")
        (object ["target_id" .= m2.id, "relation_type" .= ("related" :: T.Text)])
      respStatus linkResp `shouldBe` 200

      -- List links
      listResp <- get_ app (uuidPath "/api/v1/memories" m1.id <> "/links")
      respStatus listResp `shouldBe` 200

  describe "memory tags" $ do
    it "sets and retrieves tags" $ \app -> do
      wsResp <- postJSON app "/api/v1/workspaces"
        (object ["name" .= ("tag-ws" :: T.Text)])
      let Just ws = decode (respBody wsResp) :: Maybe Workspace
      memResp <- postJSON app "/api/v1/memories"
        (object ["workspace_id" .= ws.id, "content" .= ("tagged" :: T.Text)
                , "memory_type" .= ("short_term" :: T.Text)
                , "tags" .= (["alpha", "beta"] :: [T.Text])])
      let Just mem = decode (respBody memResp) :: Maybe Memory
      respStatus memResp `shouldBe` 200

      -- Get tags
      tagResp <- get_ app (uuidPath "/api/v1/memories" mem.id <> "/tags")
      respStatus tagResp `shouldBe` 200

  describe "batch create" $ do
    it "creates multiple memories in one request" $ \app -> do
      wsResp <- postJSON app "/api/v1/workspaces"
        (object ["name" .= ("batch-ws" :: T.Text)])
      let Just ws = decode (respBody wsResp) :: Maybe Workspace
      let items =
            [ object ["workspace_id" .= ws.id, "content" .= ("batch 1" :: T.Text)
                     , "memory_type" .= ("short_term" :: T.Text)]
            , object ["workspace_id" .= ws.id, "content" .= ("batch 2" :: T.Text)
                     , "memory_type" .= ("long_term" :: T.Text)]
            ]
      batchResp <- postJSON app "/api/v1/memories/batch" (toJSON items)
      respStatus batchResp `shouldBe` 200
      let Just mems = decode (respBody batchResp) :: Maybe [Memory]
      length mems `shouldBe` 2

    it "rejects batch items with empty content" $ \app -> do
      wsResp <- postJSON app "/api/v1/workspaces"
        (object ["name" .= ("batch-invalid-ws" :: T.Text)])
      let Just ws = decode (respBody wsResp) :: Maybe Workspace
      let items =
            [ object ["workspace_id" .= ws.id, "content" .= ("   " :: T.Text)
                     , "memory_type" .= ("short_term" :: T.Text)]
            ]
      batchResp <- postJSON app "/api/v1/memories/batch" (toJSON items)
      respStatus batchResp `shouldBe` 400

  describe "task dependencies via API" $ do
    it "adds dependency and rejects cycle" $ \app -> do
      wsResp <- postJSON app "/api/v1/workspaces"
        (object ["name" .= ("dep-ws" :: T.Text)])
      let Just ws = decode (respBody wsResp) :: Maybe Workspace
      projResp <- postJSON app "/api/v1/projects"
        (object ["workspace_id" .= ws.id, "name" .= ("Dep" :: T.Text)])
      let Just proj = decode (respBody projResp) :: Maybe Project
      t1Resp <- postJSON app "/api/v1/tasks"
        (object ["workspace_id" .= ws.id, "project_id" .= proj.id, "title" .= ("A" :: T.Text)])
      let Just t1 = decode (respBody t1Resp) :: Maybe Task
      t2Resp <- postJSON app "/api/v1/tasks"
        (object ["workspace_id" .= ws.id, "project_id" .= proj.id, "title" .= ("B" :: T.Text)])
      let Just t2 = decode (respBody t2Resp) :: Maybe Task

      -- Add B depends on A (OK)
      depResp <- postJSON app (uuidPath "/api/v1/tasks" t2.id <> "/dependencies")
        (object ["depends_on_id" .= t1.id])
      respStatus depResp `shouldBe` 200

      -- Add A depends on B (cycle -- should be 409)
      cycleResp <- postJSON app (uuidPath "/api/v1/tasks" t1.id <> "/dependencies")
        (object ["depends_on_id" .= t2.id])
      respStatus cycleResp `shouldBe` 409

      -- Remove dependency
      delResp <- del app (uuidPath "/api/v1/tasks" t2.id <> "/dependencies/"
                          <> encodeUtf8 (T.pack (show t1.id)))
      respStatus delResp `shouldBe` 200

  describe "workspace update and delete" $ do
    it "updates and deletes a workspace" $ \app -> do
      wsResp <- postJSON app "/api/v1/workspaces"
        (object ["name" .= ("upd-ws" :: T.Text)])
      let Just ws = decode (respBody wsResp) :: Maybe Workspace

      upResp <- putJSON app (uuidPath "/api/v1/workspaces" ws.id)
        (object ["name" .= ("renamed-ws" :: T.Text)])
      respStatus upResp `shouldBe` 200
      let Just updated = decode (respBody upResp) :: Maybe Workspace
      updated.name `shouldBe` "renamed-ws"

      delResp <- del app (uuidPath "/api/v1/workspaces" ws.id)
      respStatus delResp `shouldBe` 200

      getResp <- get_ app (uuidPath "/api/v1/workspaces" ws.id)
      respStatus getResp `shouldBe` 404

    it "soft-deletes workspace children and allows recreation with the same path" $ \app -> do
      wsResp <- postJSON app "/api/v1/workspaces"
        (object
          [ "name" .= ("cascade-ws" :: T.Text)
          , "path" .= ("C:/tmp/cascade-ws" :: T.Text)
          ])
      let Just ws = decode (respBody wsResp) :: Maybe Workspace

      memResp <- postJSON app "/api/v1/memories"
        (object ["workspace_id" .= ws.id, "content" .= ("hidden memory" :: T.Text)
                , "memory_type" .= ("short_term" :: T.Text)])
      let Just mem = decode (respBody memResp) :: Maybe Memory

      projResp <- postJSON app "/api/v1/projects"
        (object ["workspace_id" .= ws.id, "name" .= ("Hidden Project" :: T.Text)])
      let Just proj = decode (respBody projResp) :: Maybe Project

      taskResp <- postJSON app "/api/v1/tasks"
        (object ["workspace_id" .= ws.id, "project_id" .= proj.id, "title" .= ("Hidden Task" :: T.Text)])
      let Just task = decode (respBody taskResp) :: Maybe Task

      catResp <- postJSON app "/api/v1/categories"
        (object ["workspace_id" .= ws.id, "name" .= ("hidden-category" :: T.Text)])
      let Just cat = decode (respBody catResp) :: Maybe MemoryCategory

      delResp <- del app (uuidPath "/api/v1/workspaces" ws.id)
      respStatus delResp `shouldBe` 200

      getMemResp <- get_ app (uuidPath "/api/v1/memories" mem.id)
      respStatus getMemResp `shouldBe` 404
      getProjResp <- get_ app (uuidPath "/api/v1/projects" proj.id)
      respStatus getProjResp `shouldBe` 404
      getTaskResp <- get_ app (uuidPath "/api/v1/tasks" task.id)
      respStatus getTaskResp `shouldBe` 404
      getCatResp <- get_ app (uuidPath "/api/v1/categories" cat.id)
      respStatus getCatResp `shouldBe` 404

      recreateResp <- postJSON app "/api/v1/workspaces"
        (object
          [ "name" .= ("cascade-ws-recreated" :: T.Text)
          , "path" .= ("C:/tmp/cascade-ws" :: T.Text)
          ])
      respStatus recreateResp `shouldBe` 200

    it "requires soft-delete before purge" $ \app -> do
      wsResp <- postJSON app "/api/v1/workspaces"
        (object ["name" .= ("purge-ws" :: T.Text)])
      let Just ws = decode (respBody wsResp) :: Maybe Workspace

      purgeActiveResp <- del app (uuidPath "/api/v1/workspaces" ws.id <> "/purge")
      respStatus purgeActiveResp `shouldBe` 409

      delResp <- del app (uuidPath "/api/v1/workspaces" ws.id)
      respStatus delResp `shouldBe` 200

      purgeDeletedResp <- del app (uuidPath "/api/v1/workspaces" ws.id <> "/purge")
      respStatus purgeDeletedResp `shouldBe` 200

  --------------------------------------------------------------------------
  -- Categories
  --------------------------------------------------------------------------

  describe "categories" $ do
    it "creates, lists, updates, and deletes a category" $ \app -> do
      wsResp <- postJSON app "/api/v1/workspaces"
        (object ["name" .= ("cat-ws" :: T.Text)])
      let Just ws = decode (respBody wsResp) :: Maybe Workspace

      -- Create category
      catResp <- postJSON app "/api/v1/categories"
        (object
          [ "workspace_id" .= ws.id
          , "name" .= ("test-category" :: T.Text)
          , "description" .= ("A test category" :: T.Text)
          ])
      respStatus catResp `shouldBe` 200
      let Just cat = decode (respBody catResp) :: Maybe MemoryCategory
      cat.name `shouldBe` "test-category"

      -- List categories for workspace
      listResp <- get_ app ("/api/v1/categories?workspace_id="
                            <> encodeUtf8 (T.pack (show ws.id)))
      respStatus listResp `shouldBe` 200
      let Just cats = decode (respBody listResp) :: Maybe (PaginatedResult MemoryCategory)
      length cats.items `shouldSatisfy` (>= 1)

      -- Get category
      getResp <- get_ app (uuidPath "/api/v1/categories" cat.id)
      respStatus getResp `shouldBe` 200

      -- Update category
      upResp <- putJSON app (uuidPath "/api/v1/categories" cat.id)
        (object ["name" .= ("renamed-category" :: T.Text)])
      respStatus upResp `shouldBe` 200
      let Just updated = decode (respBody upResp) :: Maybe MemoryCategory
      updated.name `shouldBe` "renamed-category"

      -- Delete category
      delResp <- del app (uuidPath "/api/v1/categories" cat.id)
      respStatus delResp `shouldBe` 200

      -- Confirm 404
      getResp2 <- get_ app (uuidPath "/api/v1/categories" cat.id)
      respStatus getResp2 `shouldBe` 404

    it "detaches active child categories when deleting a parent" $ \app -> do
      wsResp <- postJSON app "/api/v1/workspaces"
        (object ["name" .= ("cat-tree-ws" :: T.Text)])
      let Just ws = decode (respBody wsResp) :: Maybe Workspace

      parentResp <- postJSON app "/api/v1/categories"
        (object ["workspace_id" .= ws.id, "name" .= ("parent" :: T.Text)])
      let Just parent = decode (respBody parentResp) :: Maybe MemoryCategory

      childResp <- postJSON app "/api/v1/categories"
        (object
          [ "workspace_id" .= ws.id
          , "name" .= ("child" :: T.Text)
          , "parent_id" .= parent.id
          ])
      let Just child = decode (respBody childResp) :: Maybe MemoryCategory

      delResp <- del app (uuidPath "/api/v1/categories" parent.id)
      respStatus delResp `shouldBe` 200

      getChildResp <- get_ app (uuidPath "/api/v1/categories" child.id)
      respStatus getChildResp `shouldBe` 200
      let Just updatedChild = decode (respBody getChildResp) :: Maybe MemoryCategory
      updatedChild.parentId `shouldBe` Nothing

    it "lists global categories" $ \app -> do
      -- Create a global category (no workspace_id)
      catResp <- postJSON app "/api/v1/categories"
        (object ["name" .= ("global-cat" :: T.Text)])
      respStatus catResp `shouldBe` 200

      listResp <- get_ app "/api/v1/categories/global"
      respStatus listResp `shouldBe` 200

    it "links and unlinks a memory to a category" $ \app -> do
      wsResp <- postJSON app "/api/v1/workspaces"
        (object ["name" .= ("catlink-ws" :: T.Text)])
      let Just ws = decode (respBody wsResp) :: Maybe Workspace

      catResp <- postJSON app "/api/v1/categories"
        (object ["workspace_id" .= ws.id, "name" .= ("linkable" :: T.Text)])
      let Just cat = decode (respBody catResp) :: Maybe MemoryCategory

      memResp <- postJSON app "/api/v1/memories"
        (object ["workspace_id" .= ws.id, "content" .= ("to categorize" :: T.Text)
                , "memory_type" .= ("short_term" :: T.Text)])
      let Just mem = decode (respBody memResp) :: Maybe Memory

      -- Link
      linkResp <- postJSON app "/api/v1/categories/link"
        (object ["memory_id" .= mem.id, "category_id" .= cat.id])
      respStatus linkResp `shouldBe` 200

      -- Unlink
      unlinkResp <- postJSON app "/api/v1/categories/unlink"
        (object ["memory_id" .= mem.id, "category_id" .= cat.id])
      respStatus unlinkResp `shouldBe` 200

  --------------------------------------------------------------------------
  -- Workspace groups
  --------------------------------------------------------------------------

  describe "workspace groups" $ do
    it "creates, gets, lists, and deletes a group" $ \app -> do
      grpResp <- postJSON app "/api/v1/groups"
        (object
          [ "name" .= ("test-group" :: T.Text)
          , "description" .= ("A test group" :: T.Text)
          ])
      respStatus grpResp `shouldBe` 200
      let Just grp = decode (respBody grpResp) :: Maybe WorkspaceGroup
      grp.name `shouldBe` "test-group"

      -- Get group
      getResp <- get_ app (uuidPath "/api/v1/groups" grp.id)
      respStatus getResp `shouldBe` 200

      -- List groups
      listResp <- get_ app "/api/v1/groups"
      respStatus listResp `shouldBe` 200
      let Just groups = decode (respBody listResp) :: Maybe (PaginatedResult WorkspaceGroup)
      length groups.items `shouldSatisfy` (>= 1)

      -- Delete group
      delResp <- del app (uuidPath "/api/v1/groups" grp.id)
      respStatus delResp `shouldBe` 200

      -- Confirm 404
      getResp2 <- get_ app (uuidPath "/api/v1/groups" grp.id)
      respStatus getResp2 `shouldBe` 404

    it "adds and removes group members" $ \app -> do
      grpResp <- postJSON app "/api/v1/groups"
        (object ["name" .= ("member-group" :: T.Text)])
      let Just grp = decode (respBody grpResp) :: Maybe WorkspaceGroup

      wsResp <- postJSON app "/api/v1/workspaces"
        (object ["name" .= ("grp-member-ws" :: T.Text)])
      let Just ws = decode (respBody wsResp) :: Maybe Workspace

      -- Add member
      addResp <- postJSON app (uuidPath "/api/v1/groups" grp.id <> "/members")
        (object ["workspace_id" .= ws.id])
      respStatus addResp `shouldBe` 200

      -- List members
      memResp <- get_ app (uuidPath "/api/v1/groups" grp.id <> "/members")
      respStatus memResp `shouldBe` 200
      let Just members = decode (respBody memResp) :: Maybe [UUID]
      members `shouldBe` [ws.id]

      -- Remove member
      delResp <- del app (uuidPath "/api/v1/groups" grp.id <> "/members/"
                          <> encodeUtf8 (T.pack (show ws.id)))
      respStatus delResp `shouldBe` 200

  --------------------------------------------------------------------------
  -- Cleanup
  --------------------------------------------------------------------------

  describe "cleanup" $ do
    it "upserts a policy and lists policies" $ \app -> do
      wsResp <- postJSON app "/api/v1/workspaces"
        (object ["name" .= ("cleanup-ws" :: T.Text)])
      let Just ws = decode (respBody wsResp) :: Maybe Workspace

      -- Upsert a cleanup policy
      polResp <- postJSON app "/api/v1/cleanup/policies"
        (object
          [ "workspace_id" .= ws.id
          , "memory_type" .= ("short_term" :: T.Text)
          , "max_age_hours" .= (72 :: Int)
          , "min_importance" .= (3 :: Int)
          , "enabled" .= True
          ])
      respStatus polResp `shouldBe` 200
      let Just pol = decode (respBody polResp) :: Maybe CleanupPolicy
      pol.enabled `shouldBe` True

      -- List policies
      listResp <- get_ app ("/api/v1/cleanup/policies?workspace_id="
                            <> encodeUtf8 (T.pack (show ws.id)))
      respStatus listResp `shouldBe` 200
      let Just pols = decode (respBody listResp) :: Maybe (PaginatedResult CleanupPolicy)
      length pols.items `shouldSatisfy` (>= 1)

    it "runs cleanup on a workspace" $ \app -> do
      wsResp <- postJSON app "/api/v1/workspaces"
        (object ["name" .= ("cleanup-run-ws" :: T.Text)])
      let Just ws = decode (respBody wsResp) :: Maybe Workspace

      runResp <- postJSON app "/api/v1/cleanup/run"
        (object ["workspace_id" .= ws.id])
      respStatus runResp `shouldBe` 200
      let Just result = decode (respBody runResp) :: Maybe CleanupResult
      result.deletedCount `shouldBe` 0

  --------------------------------------------------------------------------
  -- Activity timeline
  --------------------------------------------------------------------------

  describe "activity timeline" $ do
    it "returns activity events after creating entities" $ \app -> do
      wsResp <- postJSON app "/api/v1/workspaces"
        (object ["name" .= ("activity-ws" :: T.Text)])
      let Just ws = decode (respBody wsResp) :: Maybe Workspace

      -- Create a memory to generate activity
      _ <- postJSON app "/api/v1/memories"
        (object ["workspace_id" .= ws.id, "content" .= ("activity test" :: T.Text)
                , "memory_type" .= ("short_term" :: T.Text)])

      actResp <- get_ app ("/api/v1/activity?workspace_id="
                           <> encodeUtf8 (T.pack (show ws.id)))
      respStatus actResp `shouldBe` 200
      let Just events = decode (respBody actResp) :: Maybe (PaginatedResult ActivityEvent)
      length events.items `shouldSatisfy` (>= 1)

    it "returns all activity when workspace_id is omitted" $ \app -> do
      actResp <- get_ app "/api/v1/activity"
      respStatus actResp `shouldBe` 200

  --------------------------------------------------------------------------
  -- Error paths
  --------------------------------------------------------------------------

  describe "error paths" $ do
    it "returns 400 for invalid UUID in path" $ \app -> do
      resp <- get_ app "/api/v1/memories/not-a-uuid"
      respStatus resp `shouldBe` 400

    it "returns 400 for malformed JSON body" $ \app -> do
      resp <- runReq app methodPost "/api/v1/workspaces" "{invalid json"
      respStatus resp `shouldBe` 400

    it "returns 404 for nonexistent group" $ \app -> do
      resp <- get_ app "/api/v1/groups/00000000-0000-0000-0000-000000000099"
      respStatus resp `shouldBe` 404

    it "returns 404 for nonexistent category" $ \app -> do
      resp <- get_ app "/api/v1/categories/00000000-0000-0000-0000-000000000099"
      respStatus resp `shouldBe` 404

  describe "audit log" $ do
    it "persists the current request id for mutations" $ \_ -> do
      withAppEnv $ \env app -> do
        let requestIdHeader = "audit-req-1"
        resp <- runReqWithHeaders app methodPost "/api/v1/workspaces"
          [("X-Request-Id", requestIdHeader)]
          (encode (object ["name" .= ("audit-http-ws" :: T.Text)]))
        respStatus resp `shouldBe` 200
        lookup "X-Request-Id" resp.simpleHeaders `shouldBe` Just requestIdHeader

        let Just ws = decode (respBody resp) :: Maybe Workspace
        rows <- getAuditLogRows env.pool "workspace" (T.pack (show ws.id))
        length rows `shouldBe` 1
        let [created] = rows
        created.action `shouldBe` "create"
        created.requestId `shouldBe` Just "audit-req-1"
