{-# LANGUAGE RankNTypes #-}
module HMem.MCP.SessionFollower
  ( Registry, FollowerOptions(..), defaultFollowerOptions, newRegistry, newRegistryWith
  , closeRegistry, uploadToolDefinitions, isUploadTool, handleUploadTool, registryMetrics
  ) where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception
import Control.Monad (forM, forM_, unless, when, (>=>))
import Crypto.Hash (Context, SHA256, Digest, hashInit, hashUpdate, hashFinalize)
import Data.Aeson
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types (parseEither)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Int (Int64)
import Data.List (sortOn)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time
import Data.UUID (UUID, nil)
import Data.UUID.V4 qualified as UUID
import HMem.AgentSession qualified as Core
import HMem.AgentSession.Transport qualified as Wire
import HMem.BotIdentity (BotIdentity)
import HMem.MCP.SessionFile
import HMem.MCP.SessionHTTP
import HMem.MCP.SessionProcess
import System.Environment (getExecutablePath)
import Network.HTTP.Client (Manager)
import System.Timeout (timeout)

data FollowerOptions = FollowerOptions
  { now :: IO UTCTime, intervalMicros :: Int, controlWaitMicros :: Int
  , shutdownMicros :: Int, requestOverride :: Maybe (ByteString -> String -> Maybe Value -> IO (Either HTTPFault Value))
  -- The child starts masked; only the supplied unmask function may restore
  -- cancellation after the completion boundary has been installed.
  , forkWorker :: ((forall a. IO a -> IO a) -> IO ()) -> IO ThreadId
  , transition :: Text -> IO ()
  , helperExecutable :: IO FilePath
  , processScopeFactory :: IO ProcessScope
  }
defaultFollowerOptions :: FollowerOptions
defaultFollowerOptions = FollowerOptions getCurrentTime 1000000 1000000 2000000 Nothing forkIOWithUnmask (const (pure ())) getExecutablePath newProcessScope

data Registry = Registry
  { jobs :: TVar (Map.Map UUID Job), order :: TVar [UUID], closed :: TVar Bool
  , slots :: TVar Int, peak :: TVar Int, workers :: [(ThreadId, MVar ())]
  , options :: FollowerOptions
  , processScope :: ProcessScope, helperPath :: FilePath
  }
data Job = Job
  { sid :: UUID, workspace :: UUID, sourcePath :: FilePath, formatLabel :: Text, title :: Text, parentSessionId :: Maybe UUID, initialRuntimeIdentity :: Maybe Core.RuntimeIdentity, taskId :: UUID
  , http :: CapturedHTTP, state :: TVar JobState, control :: TVar (Maybe Control)
  , busy :: TVar Bool
  }
data Control = Control
  { operationId :: UUID, stop :: Bool, boundary :: Maybe Int64, result :: Maybe Value, completion :: TMVar Value }
data SweepPurpose = Resume | Periodic | Receipt deriving (Eq)
data Sweep = Sweep !SweepPurpose !Int64 !Int64 !(Context SHA256)
data JobState = JobState
  { phase :: Text, source :: Maybe Source, remote :: Maybe Core.AgentSession
  , createNeeded :: Bool, sweep :: Maybe Sweep, verifyNext :: Bool
  , pending :: Maybe Wire.AppendSessionRequest, reconcile :: Bool
  , due :: UTCTime, lastAck :: Maybe UTCTime, failures :: Int, failure :: Maybe Text
  , completedAt :: Maybe UTCTime, receipt :: Maybe Value
  , observedSize :: Int64
  , lastHeartbeat :: Maybe UTCTime
  , failureDetails :: Maybe Value
  }

isUploadTool :: Text -> Bool
isUploadTool name = name `elem` ["session_upload_start", "session_upload_status", "session_upload_flush", "session_upload_stop"]

uploadToolDefinitions :: [Value]
uploadToolDefinitions =
  [ define "session_upload_start" "Start/resume capture with REQUIRED task_id and a VERY SHORT DESCRIPTIVE TITLE, normally 3–8 words such as 'Repair session filters'. NEW sessions are atomically linked to this same-workspace in_progress task; start the task first. Resume preserves existing links and does not require the task to remain in_progress. Registration returns validating while remote creation is pending; a rejected task fails and closes the follower without creating a session. Accepted identity attributes the connection caller, NOT the actual target session model. Supply initial_runtime_identity for that model when known; omit/null means unknown. This initializes NEW sessions only; resume preserves canonical runtime and never reasserts startup identity after model switches. Use session_set_runtime for changes. An optional parent_session_id fixes one parent at creation. Captured workspace/caller/credentials never retarget. Never terminates a session."
      ["path" .= str "Explicit local JSONL file path; never sent to the HTTP server or echoed", "format_label" .= str "Required provider-neutral format label for create/resume", "title" .= str "Required very short descriptive title (normally 3–8 words; 1..80 UTF-8 bytes)", "task_id" .= str "Required initial same-workspace task UUID; NEW sessions require status in_progress", "parent_session_id" .= str "Optional existing same-workspace parent session UUID, fixed at creation", "initial_runtime_identity" .= runtimeIdentitySchema] ["path", "format_label", "title", "task_id"]
  , define "session_upload_status" "Inspect bounded process-local metadata, pending control receipt and independent 30-second HTTP acknowledgement staleness. No paths or transcript data." [] []
  , define "session_upload_flush" "Capture one finite source boundary after current job work; may send a complete final JSON record without LF. Returns a fully verified canonical prefix receipt or explicit pending operation within one second. Does not seal." [] []
  , define "session_upload_stop" "Capture and flush one finite source boundary, verify receipt and close the source. Partial/invalid final tail remains pending in the receipt. Returns complete or pending within one second; never seals." [] []
  ]
  where
    runtimeIdentitySchema = object ["description" .= ("Actual target model declaration for NEW sessions only; never inferred from the connection caller. Null/omitted is unknown. Resume preserves canonical runtime, reported by uploader status." :: Text),"anyOf" .= [object ["type" .= ("object" :: Text),"additionalProperties" .= False,"required" .= (["model"] :: [Text]),"properties" .= object ["model" .= str "Actual target model name (1..256 UTF-8 bytes)","provider" .= str "Optional known provider (1..128 UTF-8 bytes)","reasoning" .= str "Optional known reasoning setting (1..128 UTF-8 bytes)"]],object ["type" .= ("null" :: Text)]]]
    str description = object ["type" .= ("string" :: Text), "description" .= (description :: Text)]
    define name description extra required = object ["name" .= (name :: Text), "description" .= (description :: Text), "inputSchema" .= object
      ["type" .= ("object" :: Text), "properties" .= object
        (["workspace_id" .= str "Optional workspace override; accepted identity still required", "session_id" .= str "Explicit session UUID"] <> extra)
      , "required" .= (["session_id"] <> required :: [Text]), "additionalProperties" .= False]]

newRegistry :: IO Registry
newRegistry = newRegistryWith defaultFollowerOptions

newRegistryWith :: FollowerOptions -> IO Registry
newRegistryWith options = mask_ $ do
  jobs <- newTVarIO Map.empty; order <- newTVarIO []; closed <- newTVarIO False
  slots <- newTVarIO 0; peak <- newTVarIO 0
  scope <- options.processScopeFactory; helper <- options.helperExecutable
  let FollowerOptions {forkWorker = forkOwned} = options
      shell = Registry jobs order closed slots peak [] options scope helper
      cancelJoin (tid,done) = do
        _ <- forkIO (killThread tid)
        joined <- timeout options.shutdownMicros (readMVar done)
        unless (isJust joined) $ throwIO (userError "Follower startup cleanup exceeded its budget")
      spawn = do
        ready <- newEmptyMVar; done <- newEmptyMVar
        tid <- forkOwned $ \unmask ->
          (putMVar ready () >> unmask (workerLoop shell)) `finally` putMVar done ()
        -- Retain ownership if startup is interrupted while awaiting readiness.
        started <- timeout options.shutdownMicros (readMVar ready) `onException` cancelJoin (tid,done)
        unless (isJust started) $ do
          cancelJoin (tid,done)
          throwIO (userError "Follower worker startup exceeded its budget")
        pure (tid,done)
  first <- spawn
  second <- spawn `onException` cancelJoin first
  pure shell {workers = [first,second]}

terminal :: Text -> Bool
terminal phase = phase `elem` ["stopped", "failed", "terminated"]

completeControl :: Job -> Value -> STM ()
completeControl job resultValue = do
  current <- readTVar job.control
  case current of
    Just c | c.result == Nothing -> do
      _ <- tryPutTMVar c.completion resultValue
      writeTVar job.control (Just c {result = Just resultValue})
    _ -> pure ()

registryMetrics :: Registry -> IO (Int, Int, Int)
registryMetrics registry = do
  prune registry
  atomically $ (,,) <$> readTVar registry.slots <*> readTVar registry.peak <*> (Map.size <$> readTVar registry.jobs)

prune :: Registry -> IO ()
prune registry = do
  time <- registry.options.now
  atomically $ do
    jobs <- readTVar registry.jobs
    states <- mapM (readTVar . (.state)) jobs
    let old = [key | (key,s) <- Map.toList states, Just ended <- [s.completedAt], diffUTCTime time ended > 300]
        terminalRows = sortOn (\(_,s) -> s.completedAt) [(key,s) | (key,s) <- Map.toList states, terminal s.phase, key `notElem` old]
        remove = old <> map fst (take (max 0 (length terminalRows-16)) terminalRows)
        kept = foldr Map.delete jobs remove
    writeTVar registry.jobs kept
    modifyTVar' registry.order (filter (`Map.member` kept))

-- Bounded cancellation/join. A stuck OS operation is an explicit cleanup
-- failure; never race a native close against a still-running file read.
closeRegistry :: Registry -> IO Bool
closeRegistry registry = mask $ \restore -> do
  atomically $ writeTVar registry.closed True >> cancelProcessScope registry.processScope
  forM_ registry.workers $ \(tid,_) -> do _ <- forkIO (killThread tid); pure ()
  -- Some platform socket reads defer cancellation until their finite HTTP
  -- deadline. Allow that deadline plus retirement/join overhead before deciding
  -- whether it is safe to close a job's source handle.
  joined <- timeout (followerHTTPDeadlineMicros + registry.options.shutdownMicros) $ restore $ mapM_ (readMVar . snd) registry.workers
  allJobs <- Map.elems <$> readTVarIO registry.jobs
  closers <- forM allJobs $ \job -> do
    done <- newEmptyMVar
    _ <- forkIO $ do
      active <- readTVarIO job.busy
      if active && not (isJust joined) then putMVar done False else do
        state <- readTVarIO job.state
        outcome <- try @SomeException $ forM_ state.source closeSource
        time <- registry.options.now
        atomically $ do
          completeControl job (object ["error" .= ("process_shutdown" :: Text), "source_verified" .= False])
          modifyTVar' job.state (\s -> s {source = Nothing, phase = if terminal s.phase then s.phase else "stopped", completedAt = if terminal s.phase then s.completedAt else Just time, failure = if terminal s.phase then s.failure else Just "process_shutdown"})
        putMVar done (either (const False) (const True) outcome)
    pure done
  closed <- timeout registry.options.shutdownMicros $ restore $ do
    processesClosed <- closeProcessScope registry.processScope registry.options.shutdownMicros
    sourcesClosed <- mapM readMVar closers
    pure (processesClosed && and sourcesClosed)
  pure (isJust joined && maybe False id closed)

handleUploadTool :: Registry -> Manager -> String -> Maybe Text -> BotIdentity -> Text -> Value -> IO Value
handleUploadTool registry _ordinaryManager base token identity name arguments = case parseEither parse arguments of
  Left _ -> pure $ toolError "invalid_arguments" (if name == "session_upload_start" then "session_upload_start requires task_id (a same-workspace in_progress task UUID), a very short descriptive title (1 to 80 UTF-8 bytes), a format_label and readable path; parent_session_id is optional and must differ from session_id." else "Invalid uploader arguments")
  Right (workspace,sid,path,label,title,parentSessionId,initialRuntimeIdentity,taskId) -> do
    prune registry
    found <- Map.lookup sid <$> readTVarIO registry.jobs
    case name of
      "session_upload_start" -> do
        captured <- captureHTTP registry.processScope registry.helperPath base token identity
        time <- registry.options.now
        state <- newTVarIO (JobState "validating" Nothing Nothing False Nothing False Nothing False time Nothing 0 Nothing Nothing Nothing 0 Nothing Nothing)
        control <- newTVarIO Nothing; busy <- newTVarIO False
        let job = Job sid workspace path label title parentSessionId initialRuntimeIdentity taskId captured state control busy
        result <- atomically $ do
          closing <- readTVar registry.closed
          existing <- readTVar registry.jobs
          snapshots <- mapM (readTVar . (.state)) existing
          let keep = Map.filterWithKey (\key _ -> case Map.lookup key snapshots >>= (.completedAt) of
                Just ended -> diffUTCTime time ended <= 300
                Nothing -> True) existing
              terminals = sortOn (\(_,s) -> s.completedAt) [(key,s) | (key,s) <- Map.toList snapshots, terminal s.phase, Map.member key keep]
              trimmed = foldr Map.delete keep (map fst (take (max 0 (length terminals-15)) terminals))
              activeCount = length [() | s <- Map.elems snapshots, not (terminal s.phase)]
          if closing then pure (Left "registry_closed")
          else if maybe False (not . terminal . (.phase)) (Map.lookup sid snapshots) then pure (Left "already_following")
          else if activeCount >= 4 then pure (Left "capacity")
          else do
            writeTVar registry.jobs (Map.insert sid job trimmed)
            modifyTVar' registry.order (\ids -> filter (`Map.member` trimmed) (filter (/= sid) ids) <> [sid])
            pure (Right ())
        case result of
          Left code -> pure $ toolError code "Uploader registration refused"
          Right () -> statusValue registry job
      _ -> case found of
        Just job | job.workspace == workspace -> case name of
          "session_upload_status" -> statusValue registry job
          _ -> do
            time <- registry.options.now
            op <- UUID.nextRandom
            completion <- newEmptyTMVarIO
            registry.options.transition "before_control_admission"
            admitted <- atomically $ do
              state <- readTVar job.state
              closing <- readTVar registry.closed
              if terminal state.phase || state.phase == "finalizing" || closing then pure Nothing else do
                current <- readTVar job.control
                selected <- case current of
                  Just c | c.result == Nothing -> pure c {stop = c.stop || name == "session_upload_stop"}
                  _ -> pure (Control op (name == "session_upload_stop") Nothing Nothing completion)
                writeTVar job.control (Just selected)
                modifyTVar' job.state (\s -> s {due = min time s.due})
                pure (Just selected)
            case admitted of
              Nothing -> statusValue registry job
              Just selected -> do
                resultValue <- timeout registry.options.controlWaitMicros $ atomically $ readTMVar selected.completion
                status <- statusValue registry job
                pure $ addRequestedOperation selected resultValue status
        _ -> pure $ toolError "uploader_not_found" "Uploader not found in resolved workspace"
  where
    parse = withObject "uploader arguments" $ \o -> do
      let allowed = ["workspace_id", "session_id"]
          extra = if name == "session_upload_start" then ["path", "format_label", "title", "parent_session_id", "initial_runtime_identity", "task_id"] else []
      unless (all (`elem` (allowed <> extra)) (KM.keys o)) $ fail "Unsupported fields"
      workspace <- o .: "workspace_id"; sid <- o .: "session_id"
      path <- if name == "session_upload_start" then o .: "path" else pure ""
      label <- if name == "session_upload_start" then o .: "format_label" else pure ""
      title <- if name == "session_upload_start" then T.strip <$> o .: "title" else pure ""
      parentSessionId <- if name == "session_upload_start" then o .:? "parent_session_id" else pure Nothing
      initialRuntimeIdentity <- if name == "session_upload_start" then o .:? "initial_runtime_identity" else pure Nothing
      taskId <- if name == "session_upload_start" then o .: "task_id" else pure nil
      when (name == "session_upload_start" && (null path || '\0' `elem` path || T.null (T.strip label) || BS.length (TE.encodeUtf8 label) > 128)) $ fail "Invalid source"
      when (name == "session_upload_start" && (T.null title || BS.length (TE.encodeUtf8 title)>80 || parentSessionId==Just sid)) $ fail "Invalid title or optional parent"
      pure (workspace,sid,path,label,title,parentSessionId,initialRuntimeIdentity,taskId)

toolError :: Text -> Text -> Value
toolError code message = object ["isError" .= True, "content" .= [object ["type" .= ("text" :: Text), "text" .= message]], "structuredContent" .= object ["error" .= object ["code" .= code, "message" .= message]]]

addRequestedOperation :: Control -> Maybe Value -> Value -> Value
addRequestedOperation selected resultValue (Object envelope) = case KM.lookup "structuredContent" envelope of
  Just (Object fields) ->
    let value = Object (KM.insert "requested_operation" (object ["id" .= selected.operationId, "pending" .= not (isJust resultValue), "receipt" .= resultValue]) fields)
    in object ["structuredContent" .= value, "content" .= [object ["type" .= ("text" :: Text), "text" .= TE.decodeUtf8 (BL.toStrict (encode value))]]]
  _ -> Object envelope
addRequestedOperation _ _ value = value

statusValue :: Registry -> Job -> IO Value
statusValue registry job = do
  (state,control) <- atomically $ (,) <$> readTVar job.state <*> readTVar job.control
  time <- registry.options.now
  let value = object
        ["session_id" .= job.sid, "workspace_id" .= job.workspace, "status" .= state.phase, "error" .= state.failure, "error_details" .= state.failureDetails
        , "committed_offset" .= fmap (.committedBytes) state.remote, "revision" .= fmap (.revision) state.remote
        , "runtime_identity" .= (state.remote >>= (.runtimeIdentity)), "runtime_revision" .= fmap (.runtimeRevision) state.remote
        , "last_ack_at" .= state.lastAck, "stale" .= maybe True (\lastSeen -> diffUTCTime time lastSeen >= 30) state.lastAck
        , "receipt" .= state.receipt, "operation" .= fmap (\c -> object ["id" .= c.operationId, "pending" .= not (isJust c.result), "capture_boundary" .= c.boundary, "receipt" .= c.result]) control]
  pure $ object ["content" .= [object ["type" .= ("text" :: Text), "text" .= TE.decodeUtf8 (BL.toStrict (encode value))]], "structuredContent" .= value]

workerLoop :: Registry -> IO ()
workerLoop registry = do
  prune registry
  time <- registry.options.now
  selected <- atomically $ do
    closing <- readTVar registry.closed
    ids <- readTVar registry.order
    jobs <- readTVar registry.jobs
    let choose [] = pure Nothing
        choose (sid:rest) = case Map.lookup sid jobs of
          Nothing -> choose rest
          Just job -> do
            state <- readTVar job.state; busy <- readTVar job.busy
            if terminal state.phase || state.phase == "finalizing" || busy || state.due > time then choose rest else do
              writeTVar job.busy True
              writeTVar registry.order (filter (/= sid) ids <> [sid])
              pure (Just job)
    if closing then pure Nothing else choose ids
  case selected of
    Nothing -> threadDelay 10000
    Just job -> (turn registry job `catch` \(exception :: SomeException) -> do
        let reason = case fromException exception :: Maybe SourceFault of
              Just SourceChanged -> "source_identity_or_prefix_changed"
              Just SourceTruncated -> "source_truncated"
              Just SourceClosed -> "source_closed"
              _ -> "source_unavailable_or_invalid_response"
        retire registry job "failed" reason
        case fromException exception :: Maybe SomeAsyncException of Just _ -> throwIO exception; Nothing -> pure ())
      `finally` atomically (writeTVar job.busy False)
  closing <- readTVarIO registry.closed
  unless closing $ workerLoop registry

retire :: Registry -> Job -> Text -> Text -> IO ()
retire registry job phase reason = retireWithDetails registry job phase reason Nothing

retireWithDetails :: Registry -> Job -> Text -> Text -> Maybe Value -> IO ()
retireWithDetails registry job phase reason details = do
  state <- readTVarIO job.state
  closed <- try @SomeException $ forM_ state.source closeSource
  registry.options.transition "before_terminal_publication"
  time <- registry.options.now
  atomically $ do
    completeControl job (object ["error" .= reason, "source_verified" .= False])
    modifyTVar' job.state (\s -> s {phase = phase, source = Nothing, failure = Just (either (const "source_close_failed") (const reason) closed), failureDetails = either (const Nothing) (const details) closed, completedAt = Just time})

request :: Registry -> Job -> ByteString -> String -> Maybe Value -> IO (Either HTTPFault Value)
request registry job verb suffix body = bracket_ acquire release $ do
  let path = "/api/v1/workspaces/" <> show job.workspace <> "/sessions/" <> show job.sid <> suffix
  case registry.options.requestOverride of
    Nothing -> followerRequest job.http verb path body
    Just action -> action verb path body
  where
    acquire = atomically $ do
      n <- readTVar registry.slots
      check (n < 2)
      writeTVar registry.slots (n+1)
      modifyTVar' registry.peak (max (n+1))
    release = atomically $ do
      poisoned <- scopePoisoned registry.processScope
      if poisoned then writeTVar registry.closed True else modifyTVar' registry.slots (subtract 1)

-- Each fair turn performs at most one HTTP request and at most one 1 MiB
-- source read. Verification pages are 64 KiB and freeze their end cursor.
turn :: Registry -> Job -> IO ()
turn registry job = do
  state <- readTVarIO job.state
  time <- registry.options.now
  let settled :: UTCTime -> JobState -> JobState -> JobState
      settled completed updated current = updated
        { observedSize = max current.observedSize updated.observedSize
        , due = addUTCTime (fromIntegral registry.options.intervalMicros / 1000000) completed }
      save updated = do
        completed <- registry.options.now
        atomically $ modifyTVar' job.state (settled completed updated)
      acknowledge updated = do
        completed <- registry.options.now
        atomically $ modifyTVar' job.state (settled completed updated {lastAck = Just completed, failures = 0, failure = Nothing})
      mutated updated = do
        completed <- registry.options.now
        atomically $ modifyTVar' job.state (settled completed updated {lastAck = Just completed, lastHeartbeat = Just completed, failures = 0, failure = Nothing})
      failHTTP fault = do
        failedAt <- registry.options.now
        case fault of
          HTTPTaskRejected code kind currentStatus -> do
            retireWithDetails registry job "failed" kind (Just $ object
              (["code" .= kind, "field" .= ("task_id" :: Text), "http_status" .= code
               , "message" .= ("Start a live same-workspace task before creating a session; its status must be in_progress." :: Text)
               , "required_status" .= ("in_progress" :: Text)] <> maybe [] (\value -> ["current_status" .= value]) currentStatus))
          HTTPStatus code | code == 401 || code == 403 -> retire registry job "failed" "permission_revoked"
          HTTPStatus 409 -> save state {remote = Nothing, reconcile = True, sweep = Nothing, phase = "validating"}
          HTTPStatus 400 -> retire registry job "failed" "invalid_record_or_boundary"
          HTTPStatus 413 -> retire registry job "failed" "record_too_large"
          _ | state.failures >= 4 -> retire registry job "failed" "retry_exhausted"
            | otherwise -> atomically $ modifyTVar' job.state (\current -> state {observedSize = current.observedSize, phase = "retry", failures = state.failures+1, failure = Just "transient_http", due = addUTCTime (fromIntegral (min 16 (2 ^ state.failures) :: Int)) failedAt})
      decodeValue :: FromJSON a => Value -> IO a
      decodeValue value = either (const $ throwIO SourceUnavailable) pure (parseEither parseJSON value)
      acceptSession value = do
        session <- decodeValue value
        unless ((session :: Core.AgentSession).id == job.sid && session.workspaceId == job.workspace && session.formatLabel == job.formatLabel && session.parentSessionId == job.parentSessionId && maybe True (==job.title) session.title) $ throwIO SourceChanged
        pure session
      begin session = if session.state == Core.Terminated then retire registry job "terminated" "session_terminated"
        else acknowledge state {remote = Just session, createNeeded = False, sweep = Just (Sweep Resume session.committedBytes 0 hashInit), phase = "validating"}
  case state.source of
    Nothing -> mask_ $ do
      source <- openSource job.sourcePath
      save state {source = Just source}
    Just source -> do
      size <- checkSource source (max state.observedSize (maybe 0 (.committedBytes) state.remote))
      atomically $ modifyTVar' job.state (\s -> s {observedSize = max size s.observedSize})
      rawControl <- readTVarIO job.control
      let control = case rawControl of Just c | c.result == Nothing -> Just c; _ -> Nothing
      case control of
        Just c | not (isJust c.boundary) -> atomically $ modifyTVar' job.control (fmap (\current -> if current.operationId == c.operationId then current {boundary = Just size} else current))
        _ -> pure ()
      case state.remote of
        Nothing | state.createNeeded -> do
          result <- createRemote registry job
          either failHTTP (acceptSession >=> begin) result
        Nothing -> do
          result <- request registry job "GET" "" Nothing
          case result of
            Left (HTTPStatus 404) | not state.reconcile -> save state {createNeeded = True}
            Left fault -> failHTTP fault
            Right value -> acceptSession value >>= begin
        Just session | not (isJust state.pending) && maybe True (\lastBeat -> diffUTCTime time lastBeat >= 10) state.lastHeartbeat -> do
          result <- request registry job "POST" "/heartbeat" Nothing
          either failHTTP (\value -> do
            canonical <- acceptSession value
            if canonical.state == Core.Terminated then retire registry job "terminated" "session_terminated"
            else mutated state {remote = Just canonical
              , phase = if canonical.committedBytes /= session.committedBytes then "validating" else state.phase
              , sweep = if canonical.committedBytes /= session.committedBytes then Just (Sweep Resume canonical.committedBytes 0 hashInit) else state.sweep}) result
        Just session -> case state.pending of
          Just append | not state.reconcile -> do
            result <- request registry job "POST" "/append" (Just (toJSON append))
            case result of
              Left fault -> failHTTP fault
              Right value -> do
                response <- decodeValue value :: IO Wire.AppendSessionResponse
                canonical <- acceptSession (toJSON response.session)
                if canonical.state == Core.Terminated then retire registry job "terminated" "session_terminated" else
                  mutated state {remote = Just canonical, pending = Nothing, phase = "following"
                    , sweep = if response.replayed || canonical.committedBytes /= append.expectedOffset + fromIntegral (either (const 0) BS.length (Wire.decodeSessionBytes append.dataBase64))
                        then Just (Sweep Resume canonical.committedBytes 0 hashInit)
                        else case state.sweep of Nothing -> Just (Sweep Periodic canonical.committedBytes 0 hashInit); existing -> existing}
          _ -> case state.sweep of
            Just (Sweep purpose end offset context) | purpose /= Periodic || state.verifyNext -> do
              if offset == end then do
                _ <- checkSource source (max end (maybe end id (control >>= (.boundary))))
                let digest = T.pack (show (hashFinalize context :: Digest SHA256))
                if purpose == Receipt then do
                  result <- request registry job "GET" "" Nothing
                  case result of
                    Left fault -> failHTTP fault
                    Right value -> do
                      canonical <- acceptSession value
                      if canonical.state == Core.Terminated then retire registry job "terminated" "session_terminated"
                      else if canonical.committedBytes /= end || canonical.revision /= session.revision
                        then case control >>= (.boundary) of
                          Just boundary | canonical.committedBytes > boundary -> retire registry job "failed" "capture_boundary_conflict"
                          _ -> acknowledge state {remote = Just canonical, sweep = Just (Sweep Receipt canonical.committedBytes 0 hashInit)}
                        else do
                          let resultValue = object ["committed_offset" .= end, "revision" .= canonical.revision, "sha256" .= digest
                                , "capture_boundary" .= (control >>= (.boundary)), "pending_tail" .= maybe False (> end) (control >>= (.boundary))
                                , "source_verified" .= True]
                          completed <- registry.options.now
                          stopNow <- atomically $ do
                            latest <- readTVar job.control
                            let stopping = maybe False (.stop) latest
                            if stopping then modifyTVar' job.state (\s -> s {phase = "finalizing"})
                            else do
                              completeControl job resultValue
                              modifyTVar' job.state (settled completed state {remote = Just canonical, receipt = Just resultValue, sweep = Nothing, phase = "caught_up", lastAck = Just completed, failures = 0, failure = Nothing})
                            pure stopping
                          if stopNow then do
                            registry.options.transition "stop_finalizing"
                            closeSource source
                            stoppedAt <- registry.options.now
                            atomically $ do
                              completeControl job resultValue
                              modifyTVar' job.state (settled stoppedAt state {phase = "stopped", remote = Just canonical, source = Nothing, receipt = Just resultValue, completedAt = Just stoppedAt, sweep = Nothing, pending = Nothing, lastAck = Just completed, failures = 0, failure = Nothing})
                          else pure ()
                else save state {sweep = Nothing, pending = if state.reconcile then Nothing else state.pending, reconcile = False, phase = "following", verifyNext = False}
              else do
                let count = fromIntegral (min 65536 (end-offset))
                result <- request registry job "GET" ("/transcript?offset=" <> show offset <> "&limit=" <> show count) Nothing
                case result of
                  Left fault -> failHTTP fault
                  Right value -> do
                    range <- decodeValue value :: IO Wire.SessionRangeResponse
                    canonical <- acceptSession (toJSON range.session)
                    bytes <- either (const $ throwIO SourceUnavailable) pure (Wire.decodeSessionBytes range.dataBase64)
                    unless (range.offset == offset && not (BS.null bytes) && BS.length bytes <= count && range.nextOffset == offset+fromIntegral (BS.length bytes)) $ throwIO SourceChanged
                    local <- readSource source offset (BS.length bytes)
                    unless (bytes == local) $ throwIO SourceChanged
                    if canonical.state == Core.Terminated then retire registry job "terminated" "session_terminated"
                    else acknowledge state {sweep = Just (Sweep purpose end range.nextOffset (hashUpdate context bytes)), verifyNext = False}
            _ -> do
              let boundary = maybe size id (control >>= (.boundary))
                  activeControl = maybe False (not . isJust . (.result)) control
                  available = max 0 (boundary-session.committedBytes)
              bytes <- readSource source session.committedBytes (fromIntegral (min 1048576 available))
              let complete = maybe BS.empty (\lastLF -> BS.take (lastLF+1) bytes) (BS.elemIndexEnd 10 bytes)
                  finalJSON = activeControl && available <= 1048576 && validFinal bytes
                  upload = if not (BS.null complete) then complete else if finalJSON then bytes else BS.empty
              if not (BS.null upload) then
                save state {pending = Just (Wire.AppendSessionRequest session.committedBytes session.revision (Wire.encodeSessionBytes upload) (BS.last upload /= 10)), verifyNext = True, phase = "following"}
              else if BS.length bytes == 1048576 && available > 1048576 then retire registry job "failed" "record_too_large"
              else if activeControl then save state {sweep = Just (Sweep Receipt session.committedBytes 0 hashInit), phase = "validating"}
              else save state {phase = if BS.null bytes then "caught_up" else "pending_tail", verifyNext = True
                , sweep = case state.sweep of Nothing -> Just (Sweep Periodic session.committedBytes 0 hashInit); existing -> existing}

validFinal :: ByteString -> Bool
validFinal bytes = not (BS.null bytes) && case TE.decodeUtf8' bytes of
  Left _ -> False
  Right _ -> case eitherDecodeStrict' bytes :: Either String Value of Right _ -> True; Left _ -> False

createRemote :: Registry -> Job -> IO (Either HTTPFault Value)
createRemote registry job = bracket_ acquire release $ do
  let path = "/api/v1/workspaces/" <> show job.workspace <> "/sessions"
      body = Just (toJSON (Wire.CreateSessionRequest job.sid job.formatLabel job.title job.parentSessionId job.initialRuntimeIdentity job.taskId))
  case registry.options.requestOverride of Nothing -> followerRequest job.http "POST" path body; Just action -> action "POST" path body
  where
    acquire = atomically $ do n <- readTVar registry.slots; check (n < 2); writeTVar registry.slots (n+1); modifyTVar' registry.peak (max (n+1))
    release = atomically $ do
      poisoned <- scopePoisoned registry.processScope
      if poisoned then writeTVar registry.closed True else modifyTVar' registry.slots (subtract 1)
