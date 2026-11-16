-- | Opt-in isolated request latency baseline. It seeds a fixed-seed workspace
-- through the real application, then measures normalized routes with the
-- request logger's phase fields, pool checkout counters and sampled
-- PostgreSQL statement and lock activity. See
-- docs/reference/request-performance.md for the method and recorded result.
module HMem.Server.RequestLatencyMeasurementSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (forConcurrently_, wait, withAsync)
import Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVar, readTVarIO, writeTVar)
import Control.Exception (bracket)
import Control.Monad (forM, forM_, replicateM, replicateM_, unless, void, when)
import Data.Aeson (Value(..), object, (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Bits (shiftR, xor)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as LBS
import Data.Foldable (toList)
import Data.Int (Int64)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.List (sortOn)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Pool (Pool, defaultPoolConfig, destroyAllResources, newPool, setNumStripes)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time (UTCTime, addUTCTime, defaultTimeLocale, formatTime, getCurrentTime)
import Data.UUID qualified as UUID
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Conc (getNumCapabilities)
import Hasql.Connection qualified as Hasql
import Hasql.Connection.Setting qualified as Setting
import Hasql.Connection.Setting.Connection qualified as ConnSetting
import Hasql.Decoders qualified as D
import Hasql.Encoders qualified as E
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Types (Method, statusCode)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (defaultSettings, setHost, setOnException, withApplicationSettings)
import System.Directory (createDirectoryIfMissing)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import System.Log.FastLogger (fromLogStr)
import Test.Hspec

import HMem.Config qualified as Config
import HMem.DB.Pool qualified as Pool
import HMem.DB.RequestTiming (PoolCheckoutMetrics(..), checkoutWaitBoundsNs, getPoolCheckoutMetrics, recordConnect)
import HMem.DB.TestHarness (TestDb(..), TestEnv(..), withTestEnv)
import HMem.Server.AccessTracker (newAccessTracker)
import HMem.Server.App (mkApp)
import HMem.Server.Logging (jsonRequestLogger)
import HMem.Server.TestHarness (mkLocalSandboxConfig)
import HMem.Server.WebSocket (newWSState)

spec :: Spec
spec = describe "request latency attribution" $ do
  it "RequestLatencyMeasurement records an isolated phase baseline (opt-in)" $ do
    enabled <- lookupEnv "HMEM_REQUEST_LATENCY_MEASUREMENT"
    if enabled /= Just "1" then pendingWith "opt-in request latency measurement" else measure
  it "RequestPlanModes compares prepared statement plan modes for tail routes (opt-in)" $ do
    enabled <- lookupEnv "HMEM_REQUEST_PLAN_MODES"
    if enabled /= Just "1" then pendingWith "opt-in prepared plan mode probe" else measurePlanModes

------------------------------------------------------------------------
-- Profile
------------------------------------------------------------------------

fixtureSeed :: Word64
fixtureSeed = 0x9bb3fedd

data Scale = Scale { projects, topTasks, subtasks, dependencies, observations :: !Int }

-- The large shape of hmem-server/frontend/perf/fixtures.mjs.
scale :: Scale
scale = Scale { projects = 250, topTasks = 1000, subtasks = 500, dependencies = 1200, observations = 2000 }

sequentialRounds :: Int
sequentialRounds = 200

-- (phase, clients, rounds per client)
concurrentProfiles :: [(Text, Int, Int)]
concurrentProfiles = [("c4", 4, 25), ("c16", 16, 8)]

------------------------------------------------------------------------
-- Measurement
------------------------------------------------------------------------

measure :: IO ()
measure = withTestEnv $ \env -> do
  outDir <- fromMaybe ".tmp/request-latency" <$> lookupEnv "HMEM_REQUEST_LATENCY_OUTPUT"
  createDirectoryIfMissing True outDir
  caps <- getNumCapabilities
  let db = env.testDb
      ipv4 = "host=127.0.0.1 port=" <> T.pack (show db.testDbPort) <> " dbname=" <> db.testDbName
      named = "host=localhost port=" <> T.pack (show db.testDbPort) <> " dbname=" <> db.testDbName
  connectProbe <- forM [("localhost", named), ("127.0.0.1", ipv4)] $ \(label, connStr) -> do
    times <- replicateM 10 (timeConnect connStr)
    pure (label, summary times)
  sink <- newIORef []
  logger <- jsonRequestLogger (\entry -> atomicModifyIORef' sink (\xs -> (fromLogStr entry : xs, ())))
  -- Production pool defaults: 10 connections, 60 s idle, 30 s statements.
  pool <- Pool.createPool ipv4 10 60 30000
  withRaw ipv4 $ \raw -> do
    statements <- statementsAvailable raw
    withServer logger env pool $ \base -> do
      manager <- HTTP.newManager HTTP.defaultManagerSettings { HTTP.managerConnCount = 64 }
      counter <- newIORef (0 :: Int)
      samplesRef <- newIORef []
      let client phase index = Client manager base phase index counter samplesRef
      seedStarted <- getMonotonicTimeNSec
      fixture <- seedFixture (client "seed" 0)
      seedFinished <- getMonotonicTimeNSec
      spreadHistory raw fixture
      sizes <- fixtureSizes raw fixture.workspace
      now <- getCurrentTime
      -- Pool-cold sequential pass: the first sample per route variant creates
      -- connections; PostgreSQL buffers are warm from seeding.
      destroyAllResources pool
      sequentialPhase <- runPhase raw statements "seq" $ do
        let c = client "seq" 0
        forM_ [0 .. sequentialRounds - 1] $ \r -> forM_ (variants fixture now 0 1) $ \v -> v.run c r
      concurrent <- forM concurrentProfiles $ \(phase, clients, rounds) ->
        runConcurrent raw statements phase clients rounds (client phase) fixture now
      -- Controlled stripe probe: the same 16-client profile against a pool with
      -- one stripe, between two runs of the production striping.
      probePool <- singleStripePool ipv4
      probe <- withServer logger env probePool $ \probeBase -> do
        let probeClient phase index = Client manager probeBase phase index counter samplesRef
        runConcurrent raw statements "c16_one_stripe" 16 8 (probeClient "c16_one_stripe") fixture now
      repeated <- runConcurrent raw statements "c16_repeat" 16 8 (client "c16_repeat") fixture now
      destroyAllResources probePool
      -- Matched plans for the slowest sequential variants.
      clientSamples <- readIORef samplesRef
      entries <- mapMaybe decodeEntry <$> readIORef sink
      let rows = joinRows clientSamples entries
          seqSummaries = summarizePhase "seq" rows
          tails = take 6 [label | (label, p99) <- sortOn (negate . snd) [(s.label, s.p99) | s <- seqSummaries], label /= "health", p99 > 0]
      plans <- planProbe raw db pool outDir fixture now tails (client "plan" 0)
      metricsEnd <- getPoolCheckoutMetrics
      let result = object
            [ "seed" .= fixtureSeed, "capabilities" .= caps, "pool" .= object ["size" .= (10 :: Int), "stripes" .= min 10 caps, "idle_s" .= (60 :: Int)]
            , "pg_stat_statements" .= statements
            , "connect_probe" .= object [Key.fromText label .= value | (label, value) <- connectProbe]
            , "fixture" .= object (sizes <> ["seed_ms" .= ms seedStarted seedFinished])
            , "phases" .= ([sequentialPhase] <> concurrent <> [probe, repeated])
            , "routes" .= map summaryJson (concatMap (`summarizePhase` rows) (["seed", "seq", "c4", "c16", "c16_one_stripe", "c16_repeat", "plan"] :: [Text]))
            , "plan_probe" .= plans
            , "pool_totals" .= metricsJson metricsEnd
            ]
      LBS.writeFile (outDir </> "request-latency.json") (Aeson.encode result)
      putStrLn ("request latency summary written to " <> (outDir </> "request-latency.json"))
  destroyAllResources pool

-- Spread audit history uniformly over 90 days, then refresh statistics.
spreadHistory :: Hasql.Connection -> Fixture -> IO ()
spreadHistory raw fixture = do
  exec raw "SET session_replication_role = replica"
  exec raw $ "UPDATE audit_log SET changed_at = now() - make_interval(secs => abs(hashtext(id::text)) % 7776000) WHERE workspace_id = '"
    <> TE.encodeUtf8 fixture.workspace <> "'"
  exec raw "SET session_replication_role = origin"
  exec raw "VACUUM ANALYZE"

-- Eight consecutive requests per variant on one client thread reuse one pooled
-- connection, so PostgreSQL may switch a prepared statement to a generic plan
-- after its fifth execution. The same sequence then runs with custom plans.
measurePlanModes :: IO ()
measurePlanModes = withTestEnv $ \env -> do
  outDir <- fromMaybe ".tmp/request-latency" <$> lookupEnv "HMEM_REQUEST_LATENCY_OUTPUT"
  createDirectoryIfMissing True outDir
  let db = env.testDb
      ipv4 = "host=127.0.0.1 port=" <> T.pack (show db.testDbPort) <> " dbname=" <> db.testDbName
      database = "\"" <> TE.encodeUtf8 db.testDbName <> "\""
      labels = [ "timeline_events", "timeline_buckets_week", "timeline_buckets_day", "timeline_buckets_all_time", "discovery_content"
               , "discovery_subject_tree", "search_unified", "task_update_fields", "observation_list_query" ]
      explain = [ ("session_preload_libraries", "'auto_explain'"), ("auto_explain.log_min_duration", "0")
                , ("auto_explain.log_analyze", "on"), ("auto_explain.log_parameter_max_length", "0") ]
  sink <- newIORef []
  logger <- jsonRequestLogger (\entry -> atomicModifyIORef' sink (\xs -> (fromLogStr entry : xs, ())))
  pool <- Pool.createPool ipv4 10 60 30000
  withRaw ipv4 $ \raw -> withServer logger env pool $ \base -> do
    manager <- HTTP.newManager HTTP.defaultManagerSettings
    counter <- newIORef (0 :: Int)
    samplesRef <- newIORef []
    let client phase = Client manager base phase 0 counter samplesRef
    fixture <- seedFixture (client "seed")
    spreadHistory raw fixture
    now <- getCurrentTime
    forM_ explain $ \(key, value) -> exec raw ("ALTER DATABASE " <> database <> " SET " <> key <> " = " <> value)
    exec raw "DO $$ BEGIN RAISE LOG 'hmem-latency-plan-modes-begin'; END $$"
    forM_ [("plan_auto", "auto"), ("plan_custom", "force_custom_plan")] $ \(phase, mode) -> do
      exec raw ("ALTER DATABASE " <> database <> " SET plan_cache_mode = " <> mode)
      destroyAllResources pool
      exec raw ("DO $$ BEGIN RAISE LOG 'hmem-latency-plan-mode " <> TE.encodeUtf8 phase <> "'; END $$")
      forM_ labels $ \label -> forM_ [v | v <- variants fixture now 0 1, v.label == label] $ \v ->
        forM_ [0 .. 7] $ \r -> v.run (client phase) r
    exec raw "DO $$ BEGIN RAISE LOG 'hmem-latency-plan-modes-end'; END $$"
    forM_ (("plan_cache_mode", "") : explain) $ \(key, _) -> exec raw ("ALTER DATABASE " <> database <> " RESET " <> key)
    destroyAllResources pool
    threadDelay 500000
    logBytes <- BS.readFile db.testDbLogFile
    let (_, fromBegin) = BS.breakSubstring "hmem-latency-plan-modes-begin" logBytes
        (segment, _) = BS.breakSubstring "hmem-latency-plan-modes-end" fromBegin
    BS.writeFile (outDir </> "plan-modes.log") segment
    clientSamples <- readIORef samplesRef
    entries <- mapMaybe decodeEntry <$> readIORef sink
    let rows = joinRows clientSamples entries
        series phase = object
          [ Key.fromText label .= [r3 r.entry.complete | r <- sortOn (.order) group]
          | (label, group) <- Map.toList (Map.fromListWith (<>) [(r.sample.label, [r]) | r <- rows, r.sample.phase == phase]) ]
    LBS.writeFile (outDir </> "plan-modes.json") $ Aeson.encode $ object
      [ "seed" .= fixtureSeed, "iterations" .= (8 :: Int), "log_bytes" .= BS.length segment
      , "complete_ms" .= object [Key.fromText phase .= series phase | phase <- ["plan_auto", "plan_custom" :: Text]] ]
  destroyAllResources pool

withServer :: (Application -> Application) -> TestEnv -> Pool Hasql.Connection -> (String -> IO a) -> IO a
withServer logger env pool action = do
  tracker <- newAccessTracker pool 3600
  wsState <- newWSState
  pgvec <- Pool.checkPgvector pool
  let cfg = mkLocalSandboxConfig env
      settings = setHost "127.0.0.1" (setOnException (\_ _ -> pure ()) defaultSettings)
  app <- mkApp logger cfg.auth cfg.cors cfg.rateLimit pool tracker wsState Nothing pgvec
  withApplicationSettings settings (pure app) $ \port -> action ("http://127.0.0.1:" <> show port)

singleStripePool :: Text -> IO (Pool Hasql.Connection)
singleStripePool connStr = newPool $ setNumStripes (Just 1) $ defaultPoolConfig acquire Hasql.release 60 10
  where
    acquire = do
      started <- getMonotonicTimeNSec
      conn <- Hasql.acquire [Setting.connection (ConnSetting.string connStr)] >>= either (fail . show) pure
      Session.run (Session.sql "SET statement_timeout = '30000ms'") conn >>= either (fail . show) pure
      finished <- getMonotonicTimeNSec
      recordConnect (finished - started)
      pure conn

------------------------------------------------------------------------
-- HTTP client
------------------------------------------------------------------------

data Client = Client
  { manager :: HTTP.Manager, base :: String, phase :: Text, index :: Int
  , counter :: IORef Int, samples :: IORef [ClientSample] }

data ClientSample = ClientSample
  { requestId :: Text, phase :: Text, label :: Text, status :: Int
  , requestBytes :: Int, responseBytes :: Int, clientMs :: Double }

call :: Client -> Text -> Method -> String -> Maybe Value -> IO (Int, LBS.ByteString)
call client label method path body = do
  n <- atomicModifyIORef' client.counter (\i -> (i + 1, i))
  let rid = client.phase <> "." <> label <> "." <> T.pack (show n)
      bytes = maybe "" Aeson.encode body
  initial <- HTTP.parseRequest (client.base <> path)
  let request = initial
        { HTTP.method = method
        , HTTP.requestHeaders =
            [ ("Content-Type", "application/json")
            , ("X-Request-Id", TE.encodeUtf8 rid)
            -- One declared identity per client gives each its own discovery audience.
            , ("X-HMem-Bot-Identity", "{\"role\":\"Latency Fixture " <> BS8.pack (show client.index) <> "\",\"model-id\":\"test:fixture:default\"}")
            ]
        , HTTP.requestBody = HTTP.RequestBodyLBS bytes
        }
  started <- getMonotonicTimeNSec
  response <- HTTP.httpLbs request client.manager
  finished <- getMonotonicTimeNSec
  let status = statusCode (HTTP.responseStatus response)
      payload = HTTP.responseBody response
  atomicModifyIORef' client.samples $ \xs ->
    (ClientSample rid client.phase label status (fromIntegral (LBS.length bytes)) (fromIntegral (LBS.length payload)) (ms started finished) : xs, ())
  pure (status, payload)

call_ :: Client -> Text -> Method -> String -> Maybe Value -> IO ()
call_ client label method path body = void (call client label method path body)

idOf :: Text -> LBS.ByteString -> IO Text
idOf field payload = case Aeson.decode payload of
  Just (Object o) | Just (String value) <- KM.lookup (Key.fromText field) o -> pure value
  _ -> fail ("response has no " <> T.unpack field <> ": " <> show (LBS.take 300 payload))

------------------------------------------------------------------------
-- Deterministic fixture
------------------------------------------------------------------------

data Fixture = Fixture
  { workspace :: Text
  , projectIds :: Seq Text
  , taskIds :: Seq Text          -- top-level and subtasks
  , statusTargets :: Seq Text    -- top-level leaf tasks without prerequisites, left in todo
  , vocabulary :: Seq Text
  }

newtype Gen = Gen (IORef Word64)

-- SplitMix64 output over a counter: reproducible and independent of the RTS.
draw :: Gen -> Int -> IO Int
draw (Gen ref) n = do
  x <- atomicModifyIORef' ref (\s -> let s' = s + 0x9e3779b97f4a7c15 in (s', s'))
  let z1 = (x `xor` (x `shiftR` 30)) * 0xbf58476d1ce4e5b9
      z2 = (z1 `xor` (z1 `shiftR` 27)) * 0x94d049bb133111eb
      z = z2 `xor` (z2 `shiftR` 31)
  pure (fromIntegral (z `mod` fromIntegral (max 1 n)))

vocabularyWords :: Seq Text
vocabularyWords = Seq.fromList (take 300 [a <> b <> c | a <- syllables, b <- syllables, c <- syllables])
  where syllables = ["ka", "lo", "mi", "re", "tu", "sa", "vi", "no", "pe", "da", "fi", "gu"]

-- Squared uniform draw: a few common words and a long tail.
word :: Gen -> IO Text
word g = do
  u <- draw g 1000
  pure (Seq.index vocabularyWords ((u * u * Seq.length vocabularyWords) `div` 1000000))

sentence :: Gen -> Int -> IO Text
sentence g n = T.unwords <$> replicateM n (word g)

seedFixture :: Client -> IO Fixture
seedFixture c = do
  g <- Gen <$> newIORef fixtureSeed
  (_, wsBody) <- call c "workspace_create" "POST" "/api/v1/workspaces"
    (Just (object ["name" .= ("latency-fixture" :: Text), "workspace_type" .= ("repository" :: Text)]))
  ws <- idOf "id" wsBody
  -- Projects: 25 roots, then children of earlier projects up to depth 4.
  projectsRef <- newIORef (Seq.empty :: Seq (Text, Int))
  forM_ [0 .. scale.projects - 1] $ \i -> do
    existing <- readIORef projectsRef
    parent <- if i < 25 then pure Nothing else do
      candidate <- Seq.index existing <$> draw g (Seq.length existing)
      pure (if snd candidate < 3 then Just candidate else Nothing)
    name <- sentence g 3
    priority <- (+ 1) <$> draw g 10
    (_, body) <- call c "project_create" "POST" "/api/v1/projects" $ Just $ object $
      [ "workspace_id" .= ws, "name" .= (name <> " " <> T.pack (show i)), "priority" .= priority ]
      <> [ "parent_id" .= fst p | Just p <- [parent] ]
    pid <- idOf "id" body
    atomicModifyIORef' projectsRef (\xs -> (xs Seq.|> (pid, maybe 0 ((+ 1) . snd) parent), ()))
  projectSeq <- fmap fst <$> readIORef projectsRef
  let task label extra = do
        title <- sentence g 4
        description <- sentence g 24
        priority <- (+ 1) <$> draw g 10
        (_, body) <- call c label "POST" "/api/v1/tasks" $ Just $ object $
          [ "workspace_id" .= ws, "title" .= title, "description" .= description, "priority" .= priority ] <> extra
        idOf "id" body
  tops <- forM [1 .. scale.topTasks] $ \_ -> do
    p <- Seq.index projectSeq <$> draw g (Seq.length projectSeq)
    t <- task "task_create" ["project_id" .= p]
    pure (t, p)
  let topSeq = Seq.fromList tops
  subs <- forM [1 .. scale.subtasks] $ \_ -> do
    (parent, p) <- Seq.index topSeq <$> draw g (Seq.length topSeq)
    t <- task "subtask_create" ["project_id" .= p, "parent_id" .= parent]
    pure (t, parent)
  -- Prerequisites stay inside one project and point to earlier tasks.
  let byProject = Map.fromListWith (flip (<>)) [(p, [t]) | (t, p) <- tops]
      eligible = Seq.fromList [Seq.fromList ts | ts <- Map.elems byProject, length ts >= 2]
  dependents <- fmap (Map.fromList . concat) $ replicateM scale.dependencies $ do
    members <- Seq.index eligible <$> draw g (Seq.length eligible)
    later <- (+ 1) <$> draw g (Seq.length members - 1)
    earlier <- draw g later
    let dependent = Seq.index members later
    (status, _) <- call c "dependency_link" "POST" ("/api/v1/tasks/" <> T.unpack dependent <> "/dependencies")
      (Just (object ["depends_on_id" .= Seq.index members earlier]))
    pure [(dependent, ()) | status < 300]
  -- Observations with file and glob subjects below src/.
  replicateM_ scale.observations $ do
    count <- (+ 1) <$> draw g 3
    subjects <- replicateM count $ do
      m <- draw g 20
      glob <- (< 1) <$> draw g 10
      if glob then pure (object ["subject_kind" .= ("glob" :: Text), "subject" .= ("src/m" <> pad m <> "/**/*.hs")])
      else do
        s <- draw g 10
        f <- draw g 20
        pure (object ["subject_kind" .= ("file" :: Text), "subject" .= ("src/m" <> pad m <> "/s" <> T.pack (show s) <> "/File" <> T.pack (show f) <> ".hs")])
    sha <- T.concat <$> replicateM 40 (T.singleton . ("0123456789abcdef" `T.index`) <$> draw g 16)
    len <- (+ 30) <$> draw g 90
    content <- sentence g len
    call_ c "observation_create" "POST" "/api/v1/observations" $ Just $ object
      ["workspace_id" .= ws, "subjects" .= subjects, "git_sha" .= sha, "content" .= content]
  -- Close or start a share of leaf tasks to populate lifecycle history.
  let parents = Map.fromList [(parent, ()) | (_, parent) <- subs]
      topLeaves = [t | (t, _) <- tops, not (Map.member t parents)]
  todo <- fmap concat $ forM (topLeaves <> map fst subs) $ \t -> do
    r <- draw g 100
    let target | r < 20 = Just ("done" :: Text) | r < 30 = Just "in_progress" | r < 35 = Just "cancelled" | otherwise = Nothing
    case target of
      Nothing -> pure [t]
      Just status -> [] <$ call_ c "task_status_seed" "PUT" ("/api/v1/tasks/" <> T.unpack t) (Just (object ["status" .= status]))
  let todoSet = Map.fromList [(t, ()) | t <- todo]
  pure Fixture
    { workspace = ws, projectIds = projectSeq, taskIds = Seq.fromList (map fst tops <> map fst subs)
    , statusTargets = Seq.fromList [t | t <- topLeaves, Map.member t todoSet, not (Map.member t dependents)]
    , vocabulary = vocabularyWords }
  where
    pad n = (if n < 10 then "0" else "") <> T.pack (show n)

fixtureSizes :: Hasql.Connection -> Text -> IO [(Key.Key, Value)]
fixtureSizes raw ws = do
  let count sql = queryInt raw (BS8.pack sql)
      scoped table = "SELECT count(*)::int8 FROM " <> table <> " WHERE workspace_id = '" <> T.unpack ws <> "'"
  projects <- count (scoped "projects")
  tasks <- count (scoped "tasks")
  observations <- count (scoped "observations")
  dependencies <- count ("SELECT count(*)::int8 FROM task_dependencies d JOIN tasks t ON t.id = d.task_id WHERE t.workspace_id = '" <> T.unpack ws <> "'")
  audit <- count (scoped "audit_log")
  bytes <- count "SELECT pg_database_size(current_database())::int8"
  pure [ "projects" .= projects, "tasks" .= tasks, "observations" .= observations, "dependencies" .= dependencies
       , "audit_rows" .= audit, "audit_span_days" .= (90 :: Int), "database_bytes" .= bytes ]

------------------------------------------------------------------------
-- Route variants
------------------------------------------------------------------------

data Variant = Variant { label :: Text, run :: Client -> Int -> IO () }

-- Each client of a profile owns disjoint tasks for writes.
variants :: Fixture -> UTCTime -> Int -> Int -> [Variant]
variants f now clientIndex clients =
  [ Variant "health" $ \c _ -> call_ c "health" "GET" "/api/v1/health" Nothing
  , Variant "task_get" $ \c r -> call_ c "task_get" "GET" ("/api/v1/tasks/" <> task r) Nothing
  , Variant "task_update_fields" $ \c r -> call_ c "task_update_fields" "PUT" ("/api/v1/tasks/" <> task (r + 7))
      (Just (object ["title" .= ("Measured update " <> T.pack (show r)), "priority" .= (1 + r `mod` 10)]))
  , Variant "task_update_status" $ \c r ->
      call_ c "task_update_status" "PUT" ("/api/v1/tasks/" <> owned (r `div` 2))
        (Just (object ["status" .= (if even r then "in_progress" else "todo" :: Text)]))
  , Variant "task_list_query" $ \c r -> call_ c "task_list_query" "GET"
      ("/api/v1/tasks?workspace_id=" <> ws <> "&query=" <> vocab r <> "&limit=50") Nothing
  , Variant "search_unified" $ \c r -> call_ c "search_unified" "POST" "/api/v1/search"
      (Just (object ["workspace_id" .= f.workspace, "query" .= vocabText (r + 3), "limit" .= (10 :: Int)]))
  , Variant "nav_root" $ \c _ -> call_ c "nav_root" "GET" ("/api/v1/workspaces/" <> ws <> "/navigation?parent_kind=workspace_root") Nothing
  , Variant "nav_project" $ \c r -> call_ c "nav_project" "GET"
      ("/api/v1/workspaces/" <> ws <> "/navigation?parent_kind=project&parent_id=" <> project r) Nothing
  -- A batch reads up to four distinct non-root owners.
  , Variant "nav_batch" $ \c r -> call_ c "nav_batch" "POST" ("/api/v1/workspaces/" <> ws <> "/navigation/batch")
      (Just (object ["requests" .=
        [ object ["request_id" .= ("p" <> T.pack (show i)), "parent" .= object ["kind" .= ("project" :: Text), "parent_id" .= T.pack (project (r + i))]]
        | i <- [0 .. 3 :: Int] ]]))
  , Variant "nav_focus" $ \c r -> call_ c "nav_focus" "GET" ("/api/v1/workspaces/" <> ws <> "/navigation/focus/task/" <> task (r + 11)) Nothing
  , Variant "planning_counts" $ \c _ -> call_ c "planning_counts" "GET" ("/api/v1/workspaces/" <> ws <> "/planning-counts") Nothing
  , Variant "project_overview" $ \c r -> call_ c "project_overview" "GET" ("/api/v1/projects/" <> project r <> "/overview") Nothing
  , Variant "observation_list_query" $ \c r -> call_ c "observation_list_query" "GET"
      ("/api/v1/observations?workspace_id=" <> ws <> "&query=" <> vocab (r + 5)) Nothing
  , Variant "observation_subject_tree" $ \c _ -> call_ c "observation_subject_tree" "GET"
      ("/api/v1/observations/subject-tree?workspace_id=" <> ws <> "&prefix=src") Nothing
  , Variant "discovery_content" $ \c r -> do
      (session, generation) <- discoverySession c 1
      (_, body) <- call c "discovery_content" "POST" "/api/v1/observations/discovery" $ Just $ object
        [ "workspace_id" .= f.workspace, "search_session" .= session, "generation" .= generation
        , "filters" .= object ["content_query" .= vocabText (r + 9)], "view" .= ("list" :: Text), "limit" .= (50 :: Int) ]
      case Aeson.decode body of
        Just (Object o) | Just (String token) <- KM.lookup "context_token" o ->
          call_ c "discovery_continue" "POST" "/api/v1/observations/discovery" $ Just $ object
            [ "workspace_id" .= f.workspace, "context_token" .= token, "view" .= ("list" :: Text), "offset" .= (50 :: Int), "limit" .= (50 :: Int) ]
        _ -> pure ()
  , Variant "discovery_subject_tree" $ \c r -> do
      (session, generation) <- discoverySession c 2
      call_ c "discovery_subject_tree" "POST" "/api/v1/observations/discovery" $ Just $ object
        [ "workspace_id" .= f.workspace, "search_session" .= session, "generation" .= generation
        , "filters" .= object ["subject_query" .= ("src/m" <> T.pack (show (r `mod` 10)))], "view" .= ("tree" :: Text), "prefix" .= ("src" :: Text) ]
  , Variant "timeline_buckets_week" $ \c _ -> call_ c "timeline_buckets_week" "GET"
      ("/api/v1/workspaces/" <> ws <> "/timeline/buckets?since=" <> iso (-91) <> "&until=" <> iso 1 <> "&bucket=week") Nothing
  , Variant "timeline_buckets_day" $ \c _ -> call_ c "timeline_buckets_day" "GET"
      ("/api/v1/workspaces/" <> ws <> "/timeline/buckets?since=" <> iso (-91) <> "&until=" <> iso 1 <> "&bucket=day") Nothing
  , Variant "timeline_buckets_all_time" $ \c _ -> call_ c "timeline_buckets_all_time" "GET"
      ("/api/v1/workspaces/" <> ws <> "/timeline/buckets?until=" <> iso 1 <> "&bucket=week&paged=true") Nothing
  , Variant "timeline_events" $ \c _ -> call_ c "timeline_events" "GET" ("/api/v1/workspaces/" <> ws <> "/timeline?limit=50") Nothing
  ]
  where
    ws = T.unpack f.workspace
    pickFrom xs i = Seq.index xs (i `mod` Seq.length xs)
    task r = T.unpack (pickFrom f.taskIds (clientIndex + r * clients * 13))
    project r = T.unpack (pickFrom f.projectIds (clientIndex * 31 + r * 17))
    ownedTasks = Seq.fromList [t | (i, t) <- zip [0 :: Int ..] (toList f.statusTargets), i `mod` clients == clientIndex]
    owned r = T.unpack (pickFrom ownedTasks r)
    vocabText r = pickFrom f.vocabulary ((clientIndex * 7 + r * 37) `mod` 60)
    vocab = T.unpack . vocabText
    iso :: Int -> String
    iso days = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" (addUTCTime (fromIntegral days * 86400) now)

-- Sessions are reused across phases so contexts are replaced, never
-- accumulated; the shared request counter keeps generations increasing.
discoverySession :: Client -> Int -> IO (Text, Int)
discoverySession c tag = do
  generation <- atomicModifyIORef' c.counter (\i -> (i + 1, i + 1))
  pure (UUID.toText (UUID.fromWords 0x9bb3fedd (fromIntegral c.index) (fromIntegral tag) 0), generation)

------------------------------------------------------------------------
-- Phases
------------------------------------------------------------------------

runConcurrent :: Hasql.Connection -> Bool -> Text -> Int -> Int -> (Int -> Client) -> Fixture -> UTCTime -> IO Value
runConcurrent raw statements phase clients rounds mkClient fixture now =
  runPhase raw statements phase $ forConcurrently_ [0 .. clients - 1] $ \i -> do
    let vs = variants fixture now i clients
        rotated = drop (i `mod` length vs) vs <> take (i `mod` length vs) vs
    forM_ [0 .. rounds - 1] $ \r -> forM_ rotated $ \v -> v.run (mkClient i) r

-- Pool counters, sampled PostgreSQL activity and statement totals around one phase.
runPhase :: Hasql.Connection -> Bool -> Text -> IO () -> IO Value
runPhase raw statements phase action = do
  when statements $ exec raw "SELECT pg_stat_statements_reset()"
  startMetrics <- getPoolCheckoutMetrics
  databaseBefore <- databaseCounters raw
  samplesRef <- newTVarIO []
  stop <- newTVarIO False
  started <- getMonotonicTimeNSec
  withAsync (sampler raw stop samplesRef) $ \sampling -> do
    action
    atomically (writeTVar stop True)
    wait sampling
  finished <- getMonotonicTimeNSec
  endMetrics <- getPoolCheckoutMetrics
  activity <- readTVarIO samplesRef
  -- Backends publish cumulative statistics at most once per second.
  threadDelay 1100000
  databaseAfter <- databaseCounters raw
  top <- if statements then topStatements raw else pure []
  let waitLabels = Map.toList (Map.fromListWith (+) [(l, 1 :: Int) | (_, _, _, ls) <- activity, l <- ls])
  pure $ object
    [ "phase" .= phase, "elapsed_ms" .= ms started finished
    , "pool" .= object
        [ "checkouts" .= (endMetrics.checkouts - startMetrics.checkouts)
        , "unattributed" .= (endMetrics.unattributedCheckouts - startMetrics.unattributedCheckouts)
        , "connects" .= (endMetrics.connects - startMetrics.connects)
        , "capacity_waits" .= (endMetrics.capacityWaits - startMetrics.capacityWaits)
        , "wait_total_ms" .= nsMs (endMetrics.waitTotalNs - startMetrics.waitTotalNs)
        , "connect_total_ms" .= nsMs (endMetrics.connectTotalNs - startMetrics.connectTotalNs)
        , "hold_total_ms" .= nsMs (endMetrics.holdTotalNs - startMetrics.holdTotalNs)
        , "wait_buckets" .= bucketsJson (zipWith (-) endMetrics.waitBuckets startMetrics.waitBuckets) ]
    , "activity" .= object
        [ "samples" .= length activity
        , "max_active" .= maximum0 [a | (a, _, _, _) <- activity]
        , "samples_with_lock_wait" .= length [() | (_, l, _, _) <- activity, l > 0]
        , "max_ungranted_locks" .= maximum0 [u | (_, _, u, _) <- activity]
        , "active_wait_events" .= object [Key.fromText l .= n | (l, n) <- waitLabels] ]
    , "database" .= object [Key.fromText k .= (b - a) | (k, a, b) <- zip3 databaseCounterNames databaseBefore databaseAfter]
    , "statements" .= top ]
  where
    maximum0 xs = if null xs then 0 else maximum xs

databaseCounterNames :: [Text]
databaseCounterNames = ["xact_commit", "xact_rollback", "blks_read", "blks_hit", "temp_files", "temp_bytes", "deadlocks"]

databaseCounters :: Hasql.Connection -> IO [Int64]
databaseCounters raw = do
  exec raw "SELECT pg_stat_clear_snapshot()"
  Session.run (Session.statement () statement) raw >>= either (fail . show) pure
  where
    statement = Statement.Statement sql E.noParams (D.singleRow (replicateM (length databaseCounterNames) (D.column (D.nonNullable D.int8)))) False
    sql = "SELECT xact_commit, xact_rollback, blks_read, blks_hit, temp_files, temp_bytes, deadlocks FROM pg_stat_database WHERE datname = current_database()"

sampler :: Hasql.Connection -> TVar Bool -> TVar [(Int, Int, Int, [Text])] -> IO ()
sampler raw stop samplesRef = loop
  where
    loop = do
      done <- readTVarIO stop
      unless done $ do
        row <- Session.run (Session.statement () activityStatement) raw >>= either (fail . show) pure
        atomically $ readTVar samplesRef >>= writeTVar samplesRef . (row :)
        threadDelay 25000
        loop

activityStatement :: Statement.Statement () (Int, Int, Int, [Text])
activityStatement = Statement.Statement sql E.noParams decoder False
  where
    sql = "SELECT count(*) FILTER (WHERE a.state = 'active')::int4, count(*) FILTER (WHERE a.wait_event_type = 'Lock')::int4, \
          \(SELECT count(*) FROM pg_locks WHERE NOT granted)::int4, \
          \coalesce(string_agg(coalesce(a.wait_event_type, 'CPU') || ':' || coalesce(a.wait_event, '-'), ',') FILTER (WHERE a.state = 'active'), '') \
          \FROM pg_stat_activity a WHERE a.datname = current_database() AND a.pid <> pg_backend_pid() AND a.backend_type = 'client backend'"
    decoder = D.singleRow $ (,,,)
      <$> (fromIntegral <$> D.column (D.nonNullable D.int4))
      <*> (fromIntegral <$> D.column (D.nonNullable D.int4))
      <*> (fromIntegral <$> D.column (D.nonNullable D.int4))
      <*> (filter (not . T.null) . T.splitOn "," <$> D.column (D.nonNullable D.text))

statementsAvailable :: Hasql.Connection -> IO Bool
statementsAvailable raw = do
  preload <- Session.run (Session.statement () (Statement.Statement "SELECT current_setting('shared_preload_libraries')" E.noParams (D.singleRow (D.column (D.nonNullable D.text))) False)) raw
  case preload of
    Right libs | "pg_stat_statements" `T.isInfixOf` libs -> do
      exec raw "CREATE EXTENSION IF NOT EXISTS pg_stat_statements"
      pure True
    _ -> pure False

topStatements :: Hasql.Connection -> IO [Value]
topStatements raw = do
  rows <- Session.run (Session.statement () statement) raw >>= either (fail . show) pure
  pure [ object [ "query" .= q, "calls" .= calls, "total_ms" .= r3 total, "mean_ms" .= r3 mean, "max_ms" .= r3 maxMs
                , "rows" .= rowsN, "shared_hit" .= hit, "shared_read" .= readN ]
       | (q, calls, total, mean, maxMs, rowsN, hit, readN) <- rows ]
  where
    statement = Statement.Statement sql E.noParams (D.rowList row) False
    sql = "SELECT left(regexp_replace(query, '\\s+', ' ', 'g'), 160), calls, total_exec_time, mean_exec_time, max_exec_time, rows, shared_blks_hit, shared_blks_read \
          \FROM pg_stat_statements WHERE dbid = (SELECT oid FROM pg_database WHERE datname = current_database()) \
          \AND query NOT LIKE '%pg_stat_%' AND query NOT LIKE '%pg_locks%' ORDER BY total_exec_time DESC LIMIT 12"
    row = (,,,,,,,) <$> D.column (D.nonNullable D.text) <*> int8 <*> float8 <*> float8 <*> float8 <*> int8 <*> int8 <*> int8
    int8 = D.column (D.nonNullable D.int8) :: D.Row Int64
    float8 = D.column (D.nonNullable D.float8)

-- auto_explain for a few requests of each tail variant on fresh connections.
planProbe :: Hasql.Connection -> TestDb -> Pool Hasql.Connection -> FilePath -> Fixture -> UTCTime -> [Text] -> Client -> IO Value
planProbe raw db pool outDir fixture now labels c = do
  let database = "\"" <> TE.encodeUtf8 db.testDbName <> "\""
      settings = [ ("session_preload_libraries", "'auto_explain'"), ("auto_explain.log_min_duration", "0")
                 , ("auto_explain.log_analyze", "on"), ("auto_explain.log_buffers", "on")
                 , ("auto_explain.log_nested_statements", "on"), ("auto_explain.log_parameter_max_length", "0") ]
  forM_ settings $ \(key, value) -> exec raw ("ALTER DATABASE " <> database <> " SET " <> key <> " = " <> value)
  destroyAllResources pool
  exec raw "DO $$ BEGIN RAISE LOG 'hmem-latency-plan-probe-begin'; END $$"
  forM_ labels $ \label -> forM_ [v | v <- variants fixture now 0 1, v.label == label] $ \v ->
    forM_ [0 .. 2] $ \r -> v.run c r
  exec raw "DO $$ BEGIN RAISE LOG 'hmem-latency-plan-probe-end'; END $$"
  forM_ settings $ \(key, _) -> exec raw ("ALTER DATABASE " <> database <> " RESET " <> key)
  destroyAllResources pool
  threadDelay 500000
  logBytes <- BS.readFile db.testDbLogFile
  let (_, fromBegin) = BS.breakSubstring "hmem-latency-plan-probe-begin" logBytes
      (segment, _) = BS.breakSubstring "hmem-latency-plan-probe-end" fromBegin
  BS.writeFile (outDir </> "plans.log") segment
  pure (object ["variants" .= labels, "log_bytes" .= BS.length segment, "file" .= ("plans.log" :: Text)])

------------------------------------------------------------------------
-- Aggregation
------------------------------------------------------------------------

data Entry = Entry
  { requestId :: Text, route :: Text, outcome :: Text, complete, wait, connect, hold :: Double
  , provider :: Maybe Double, checkouts :: Double }

decodeEntry :: BS.ByteString -> Maybe Entry
decodeEntry line = case Aeson.decodeStrict line of
  Just (Object o) -> do
    rid <- text o "request_id"
    Entry rid <$> text o "route" <*> text o "outcome" <*> number o "complete_ms" <*> number o "db_wait_ms"
      <*> number o "db_connect_ms" <*> number o "db_hold_ms" <*> pure (number o "provider_ms") <*> number o "db_checkouts"
  _ -> Nothing
  where
    text o k = case KM.lookup k o of Just (String v) -> Just v; _ -> Nothing
    number o k = case KM.lookup k o of Just (Number v) -> Just (realToFrac v); _ -> Nothing

data Row = Row { sample :: ClientSample, entry :: Entry, order :: Int }

joinRows :: [ClientSample] -> [Entry] -> [Row]
joinRows samples entries =
  [ Row s e n | s <- samples, Just e <- [Map.lookup s.requestId byId], let n = sequenceOf s.requestId ]
  where
    byId = Map.fromList [(e.requestId, e) | e <- entries]
    sequenceOf rid = maybe 0 fst (BS8.readInt (TE.encodeUtf8 (last (T.splitOn "." rid))))

data Summary = Summary { label :: Text, p99 :: Double, json :: Value }

summarizePhase :: Text -> [Row] -> [Summary]
summarizePhase phase rows =
  [ summarize phase label group | (label, group) <- Map.toList grouped ]
  where
    grouped = Map.fromListWith (<>) [(r.sample.label, [r]) | r <- rows, r.sample.phase == phase]

summarize :: Text -> Text -> [Row] -> Summary
summarize phase label group = Summary label (pct 0.99 completes) $ object $
  [ "phase" .= phase, "variant" .= label, "route" .= (case ordered of r : _ -> r.entry.route; [] -> "")
  , "n" .= length warm, "status" .= counts (statusClass . (.sample.status)) group
  , "outcome" .= counts (.entry.outcome) group
  , "complete_ms" .= percentiles completes
  , "client_ms" .= percentiles (map (.sample.clientMs) warm)
  , "mean_ms" .= object
      [ "wait" .= meanOf (.entry.wait), "connect" .= meanOf (.entry.connect), "hold" .= meanOf (.entry.hold)
      , "provider" .= meanOf (fromMaybe 0 . (.entry.provider)), "app" .= meanOf appMs, "checkouts" .= meanOf (.entry.checkouts) ]
  , "tail" .= object
      [ "n" .= length tailRows, "complete" .= meanIn tailRows (.entry.complete), "wait" .= meanIn tailRows (.entry.wait)
      , "connect" .= meanIn tailRows (.entry.connect), "hold" .= meanIn tailRows (.entry.hold), "app" .= meanIn tailRows appMs ]
  , "request_bytes" .= meanIn warm (fromIntegral . (.sample.requestBytes))
  , "response_bytes" .= meanIn warm (fromIntegral . (.sample.responseBytes)) ]
  <> [ "cold" .= object ["complete" .= r3 r.entry.complete, "wait" .= r3 r.entry.wait, "connect" .= r3 r.entry.connect, "hold" .= r3 r.entry.hold]
     | phase == "seq", r : _ <- [ordered] ]
  where
    ordered = sortOn (.order) group
    -- The sequential phase starts pool-cold; its first sample is reported separately.
    warm = if phase == "seq" then drop 1 ordered else ordered
    completes = map (.entry.complete) warm
    threshold = pct 0.95 completes
    tailRows = [r | r <- warm, r.entry.complete >= threshold]
    appMs r = max 0 (r.entry.complete - r.entry.wait - r.entry.hold - fromMaybe 0 r.entry.provider)
    meanOf f = meanIn warm f
    statusClass s = T.pack (show (s `div` 100)) <> "xx"

summaryJson :: Summary -> Value
summaryJson s = s.json

meanIn :: [a] -> (a -> Double) -> Double
meanIn [] _ = 0
meanIn xs f = r3 (sum (map f xs) / fromIntegral (length xs))

counts :: (a -> Text) -> [a] -> Value
counts f xs = object [Key.fromText k .= n | (k, n) <- Map.toList (Map.fromListWith (+) [(f x, 1 :: Int) | x <- xs])]

percentiles :: [Double] -> Value
percentiles xs = object ["p50" .= r3 (pct 0.5 xs), "p95" .= r3 (pct 0.95 xs), "p99" .= r3 (pct 0.99 xs), "max" .= r3 (pct 1 xs)]

-- Nearest rank.
pct :: Double -> [Double] -> Double
pct _ [] = 0
pct p xs = sorted !! max 0 (min (length xs - 1) (ceiling (p * fromIntegral (length xs)) - 1))
  where sorted = sortOn id xs

summary :: [Double] -> Value
summary xs = object ["n" .= length xs, "mean_ms" .= meanIn xs id, "max_ms" .= r3 (pct 1 xs)]

bucketsJson :: [Word64] -> Value
bucketsJson countsList = object
  [ Key.fromText (maybe "overflow" (\b -> "le_" <> T.pack (show (nsMs b)) <> "ms") bound) .= n
  | (bound, n) <- zip (map Just checkoutWaitBoundsNs <> [Nothing]) countsList ]

metricsJson :: PoolCheckoutMetrics -> Value
metricsJson m = object
  [ "checkouts" .= m.checkouts, "unattributed" .= m.unattributedCheckouts, "connects" .= m.connects
  , "capacity_waits" .= m.capacityWaits, "wait_max_ms" .= nsMs m.waitMaxNs, "wait_buckets" .= bucketsJson m.waitBuckets ]

------------------------------------------------------------------------
-- Raw connection helpers
------------------------------------------------------------------------

withRaw :: Text -> (Hasql.Connection -> IO a) -> IO a
withRaw connStr = bracket
  (Hasql.acquire [Setting.connection (ConnSetting.string connStr)] >>= either (fail . show) pure)
  Hasql.release

timeConnect :: Text -> IO Double
timeConnect connStr = do
  started <- getMonotonicTimeNSec
  withRaw connStr $ \conn -> Session.run (Session.sql "SELECT 1") conn >>= either (fail . show) pure
  finished <- getMonotonicTimeNSec
  pure (ms started finished)

exec :: Hasql.Connection -> BS.ByteString -> IO ()
exec conn sql = Session.run (Session.sql sql) conn >>= either (fail . show) pure

queryInt :: Hasql.Connection -> BS.ByteString -> IO Int64
queryInt conn sql = Session.run (Session.statement () (Statement.Statement sql E.noParams (D.singleRow (D.column (D.nonNullable D.int8))) False)) conn
  >>= either (fail . show) pure

ms :: Word64 -> Word64 -> Double
ms started finished = r3 (fromIntegral (finished - started) / 1e6)

nsMs :: Word64 -> Double
nsMs ns = r3 (fromIntegral ns / 1e6)

r3 :: Double -> Double
r3 x = fromIntegral (round (x * 1000) :: Integer) / 1000
