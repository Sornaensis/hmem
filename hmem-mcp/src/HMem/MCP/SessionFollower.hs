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
defaultFollowerOptions = FollowerOptions getCurrentTime 1000000 900000 2000000 Nothing forkIOWithUnmask (const (pure ())) getExecutablePath newProcessScope

data Registry = Registry
  { jobs :: TVar (Map.Map UUID Job), order :: TVar [UUID], closed :: TVar Bool
  , slots :: TVar Int, peak :: TVar Int, workers :: [(ThreadId, MVar ())]
  , options :: FollowerOptions
  , processScope :: ProcessScope, helperPath :: FilePath
  }
data Job = Job
  { sid :: UUID, workspace :: UUID, sourcePath :: FilePath, formatLabel :: Text, title :: Text, parentSessionId :: Maybe UUID, initialRuntimeIdentity :: Maybe Core.RuntimeIdentity, taskId :: UUID, imported :: Bool
  , http :: CapturedHTTP, state :: TVar JobState, control :: TVar (Maybe Control)
  , busy :: TVar Bool
  }
data Control = Control
  { operationId :: UUID, stop :: Bool, boundary :: Maybe Int64, result :: Maybe Value, completion :: TMVar Value }
data SweepPurpose = Resume | Periodic | Receipt deriving (Eq)
-- A sweep belongs to this job's one stable opened Source generation. No digest
-- or proof survives source closure/reopen. Server bytes are immutable at N/R.
data Sweep = Sweep
  { purpose :: !SweepPurpose, end :: !Int64, revision :: !Int64, offset :: !Int64
  , context :: !(Context SHA256), proof :: !(Maybe Wire.PrefixProofRequest)
  , audience :: !(Maybe Text), submitted :: !Bool, proven :: !Bool, nextPoll :: !(Maybe UTCTime) }

newSweep :: SweepPurpose -> Core.AgentSession -> Sweep
newSweep purpose session = Sweep purpose session.committedBytes session.revision 0 hashInit Nothing Nothing False False Nothing
data ImportScan = ImportScan !Int64 !Int64 !(Context SHA256) !ByteString

data ImportFault = InvalidImport | ImportDeadline deriving (Show,Eq)
instance Exception ImportFault

data JobState = JobState
  { phase :: Text, source :: Maybe Source, remote :: Maybe Core.AgentSession
  , createNeeded :: Bool, sweep :: Maybe Sweep, verifyNext :: Bool
  , pending :: Maybe Wire.AppendSessionRequest, reconcile :: Bool
  , due :: UTCTime, lastAck :: Maybe UTCTime, failures :: Int, failure :: Maybe Text
  , completedAt :: Maybe UTCTime, receipt :: Maybe Value
  , observedSize :: Int64
  , lastHeartbeat :: Maybe UTCTime
  , failureDetails :: Maybe Value
  , nextVerification :: UTCTime, lastProof :: Maybe (Wire.PrefixProofRequest,Text)
  , importScan :: Maybe ImportScan, importManifest :: Maybe (Int64,Text), importDeadline :: Maybe UTCTime, importCompleted :: Maybe Core.AgentSession
  }

isUploadTool :: Text -> Bool
isUploadTool name = name `elem` ["session_import", "session_upload_start", "session_upload_status", "session_upload_flush", "session_upload_stop"]

uploadToolDefinitions :: [Value]
uploadToolDefinitions =
  [ define "session_upload_start" "Start/resume capture with REQUIRED task_id and a VERY SHORT DESCRIPTIVE TITLE, normally 3–8 words such as 'Repair session filters'. NEW sessions are atomically linked to this same-workspace in_progress task; start the task first. Resume preserves existing links and does not require the task to remain in_progress. Registration returns validating while remote creation is pending; a rejected task fails and closes the follower without creating a session. Accepted identity attributes the connection caller, NOT the actual target session model. Supply initial_runtime_identity with that model and the actual running agent role when known; omit/null means unknown. This initializes NEW sessions only; resume preserves canonical runtime and never reasserts startup identity after model switches. Use session_set_runtime for changes. An optional parent_session_id fixes one parent at creation. Captured workspace/caller/credentials never retarget. Never terminates a session."
      ["path" .= str "Explicit local JSONL file path; never sent to the HTTP server or echoed", "format_label" .= str "Required provider-neutral format label for create/resume", "title" .= str "Required very short descriptive title (normally 3–8 words; 1..80 UTF-8 bytes)", "task_id" .= str "Required initial same-workspace task UUID; NEW sessions require status in_progress", "parent_session_id" .= str "Optional existing same-workspace parent session UUID, fixed at creation", "initial_runtime_identity" .= runtimeIdentitySchema] ["path", "format_label", "title", "task_id"]
  , define "session_import" "Import a completed local JSONL as historical task evidence. Validates the entire finite regular file before registration, preserves exact bytes, and seals only the full verified import. Accepts any visible same-workspace task status without changing it. Returns promptly; inspect session_upload_status for completed status and receipt. Retry the same UUID, source and immutable metadata to resume. Invalid tails or observed source changes fail; no transcript or path is returned. Explicit historical runtime identity is separate from importer attribution."
      ["path" .= str "Explicit completed local JSONL regular-file path; remains local", "format_label" .= str "Provider-neutral transcript format", "title" .= str "Very short descriptive title (1..80 UTF-8 bytes)", "task_id" .= str "Visible same-workspace historical task UUID; any status", "parent_session_id" .= str "Optional existing same-workspace parent UUID", "initial_runtime_identity" .= runtimeIdentitySchema] ["path", "format_label", "title", "task_id"]
  , define "session_upload_status" "Inspect bounded process-local metadata, pending control receipt and independent 30-second HTTP acknowledgement staleness. No paths or transcript data." [] []
  , define "session_upload_flush" "Capture one finite source boundary after current job work; may send a complete final JSON record without LF. Returns a fully verified canonical prefix receipt or explicit pending operation within one second. Does not seal." [] []
  , define "session_upload_stop" "Capture and flush one finite source boundary, verify receipt and close the source. Partial/invalid final tail remains pending in the receipt. Returns complete or pending within one second; never seals." [] []
  ]
  where
    runtimeIdentitySchema = object ["description" .= ("Actual running agent role and model declaration for NEW sessions only; never inferred from the connection caller. Null/omitted is unknown. Resume preserves canonical runtime, reported by uploader status." :: Text),"anyOf" .= [object ["type" .= ("object" :: Text),"additionalProperties" .= False,"required" .= (["model","role"] :: [Text]),"properties" .= object ["role" .= str "Actual running agent role (1..128 UTF-8 bytes); never infer from the connection caller","model" .= str "Actual target model name (1..256 UTF-8 bytes)","provider" .= str "Optional known provider (1..128 UTF-8 bytes)","reasoning" .= str "Optional known reasoning setting (1..128 UTF-8 bytes)"]],object ["type" .= ("null" :: Text)]]]
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
terminal phase = phase `elem` ["stopped", "failed", "terminated", "imported"]

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
  Left _ -> pure $ toolError "invalid_arguments" (if name == "session_import" then "session_import requires a completed regular-file path, task_id (a visible same-workspace task UUID of any status), session_id, a very short descriptive title and format_label; optional parent must differ from session_id." else if name == "session_upload_start" then "session_upload_start requires task_id (a same-workspace in_progress task UUID), a very short descriptive title (1 to 80 UTF-8 bytes), a format_label and readable path; parent_session_id is optional and must differ from session_id." else "Invalid uploader arguments")
  Right (workspace,sid,path,label,title,parentSessionId,initialRuntimeIdentity,taskId) -> do
    prune registry
    found <- Map.lookup sid <$> readTVarIO registry.jobs
    case name of
      startName | startName `elem` ["session_upload_start","session_import"] -> do
        captured <- captureHTTP registry.processScope registry.helperPath base token identity
        time <- registry.options.now
        state <- newTVarIO JobState
          { phase = "validating", source = Nothing, remote = Nothing, createNeeded = False
          , sweep = Nothing, verifyNext = False, pending = Nothing, reconcile = False, due = time
          , lastAck = Nothing, failures = 0, failure = Nothing, completedAt = Nothing, receipt = Nothing
          , observedSize = 0, lastHeartbeat = Nothing, failureDetails = Nothing
          , nextVerification = time, lastProof = Nothing
          , importScan = Nothing, importManifest = Nothing, importDeadline = if name == "session_import" then Just (addUTCTime 900 time) else Nothing, importCompleted = Nothing }
        control <- newTVarIO Nothing; busy <- newTVarIO False
        let job = Job sid workspace path label title parentSessionId initialRuntimeIdentity taskId (name == "session_import") captured state control busy
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
          Left "already_following" | Just existing <- found, job.imported && existing.imported
            && job.workspace == existing.workspace && job.sourcePath == existing.sourcePath
            && job.taskId == existing.taskId && job.formatLabel == existing.formatLabel && job.title == existing.title
            && job.parentSessionId == existing.parentSessionId && job.initialRuntimeIdentity == existing.initialRuntimeIdentity -> statusValue registry existing
          Left code -> pure $ toolError code "Uploader registration refused"
          Right () -> statusValue registry job
      _ -> case found of
        Just job | job.workspace == workspace -> case name of
          "session_upload_status" -> statusValue registry job
          _ | job.imported -> pure $ toolError "import_control_unsupported" "Historical imports accept status reads and exact session_import retries; live flush/stop controls are not supported"
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
          extra = if name `elem` ["session_upload_start","session_import"] then ["path", "format_label", "title", "parent_session_id", "initial_runtime_identity", "task_id"] else []
      unless (all (`elem` (allowed <> extra)) (KM.keys o)) $ fail "Unsupported fields"
      workspace <- o .: "workspace_id"; sid <- o .: "session_id"
      path <- if name `elem` ["session_upload_start","session_import"] then o .: "path" else pure ""
      label <- if name `elem` ["session_upload_start","session_import"] then o .: "format_label" else pure ""
      title <- if name `elem` ["session_upload_start","session_import"] then T.strip <$> o .: "title" else pure ""
      parentSessionId <- if name `elem` ["session_upload_start","session_import"] then o .:? "parent_session_id" else pure Nothing
      initialRuntimeIdentity <- if name `elem` ["session_upload_start","session_import"] then o .:? "initial_runtime_identity" else pure Nothing
      taskId <- if name `elem` ["session_upload_start","session_import"] then o .: "task_id" else pure nil
      when (name `elem` ["session_upload_start","session_import"] && (null path || '\0' `elem` path || T.null (T.strip label) || BS.length (TE.encodeUtf8 label) > 128)) $ fail "Invalid source"
      when (name `elem` ["session_upload_start","session_import"] && (T.null title || BS.length (TE.encodeUtf8 title)>80 || parentSessionId==Just sid)) $ fail "Invalid title or optional parent"
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
        let reason = case fromException exception :: Maybe ImportFault of
              Just InvalidImport -> "invalid_jsonl"
              Just ImportDeadline -> "import_timeout"
              Nothing -> case fromException exception :: Maybe SourceFault of
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
-- source read. Hash sweeps use local 1 MiB windows and freeze their N/R target.
turn :: Registry -> Job -> IO ()
turn registry job = do
  state <- readTVarIO job.state
  time <- registry.options.now
  when (maybe False (time >) state.importDeadline) $ throwIO ImportDeadline
  let verificationDelay = fromIntegral registry.options.intervalMicros / 100000
      pollDelay = min 0.05 (fromIntegral registry.options.intervalMicros / 1000000)
      saveAt due updated = do
        completed <- registry.options.now
        atomically $ do
          control <- readTVar job.control
          let uncaptured = maybe False (\c -> c.result == Nothing && c.boundary == Nothing) control
          modifyTVar' job.state (\current -> (settled completed updated current) {due = if uncaptured then completed else due})
      save updated = do
        completed <- registry.options.now
        atomically $ modifyTVar' job.state (settled completed updated)
      acknowledge updated = do
        completed <- registry.options.now
        atomically $ modifyTVar' job.state (settled completed updated {lastAck = Just completed, failures = 0, failure = Nothing})
      acknowledgeAt due updated = do
        completed <- registry.options.now
        saveAt due updated {lastAck = Just completed, failures = 0, failure = Nothing}
      mutated updated = do
        completed <- registry.options.now
        atomically $ modifyTVar' job.state (settled completed updated {lastAck = Just completed, lastHeartbeat = Just completed, failures = 0, failure = Nothing})
      failHTTP fault = do
        failedAt <- registry.options.now
        case fault of
          HTTPTaskRejected code kind currentStatus -> do
            retireWithDetails registry job "failed" kind (Just $ object
              (["code" .= kind, "field" .= ("task_id" :: Text), "http_status" .= code
               , "message" .= (if job.imported then "Import requires a visible nondeleted task in the same workspace; any task status is accepted." else "Start a live same-workspace task before creating a session; its status must be in_progress." :: Text)]
               <> if job.imported then [] else ["required_status" .= ("in_progress" :: Text)] <> maybe [] (\value -> ["current_status" .= value]) currentStatus))
          HTTPStatus code | code == 401 || code == 403 -> retire registry job "failed" "permission_revoked"
          HTTPStatus 409 -> save state {remote = Nothing, reconcile = True, sweep = Nothing, phase = "validating"}
          HTTPStatus 400 -> retire registry job "failed" "invalid_record_or_boundary"
          HTTPStatus 413 -> retire registry job "failed" "record_too_large"
          _ | state.failures >= 4 -> retire registry job "failed" "retry_exhausted"
            | otherwise -> atomically $ modifyTVar' job.state (\current -> state {observedSize = current.observedSize, phase = "retry", failures = state.failures+1, failure = Just "transient_http", due = addUTCTime (fromIntegral (min 16 (2 ^ state.failures) :: Int)) failedAt})
      acceptSession value = do
        session <- decodeValue value
        unless ((session :: Core.AgentSession).id == job.sid && session.workspaceId == job.workspace && session.formatLabel == job.formatLabel && session.parentSessionId == job.parentSessionId && maybe True (==job.title) session.title) $ throwIO SourceChanged
        unless (if job.imported then session.importTaskId == Just job.taskId
          && session.importBytes == fmap fst state.importManifest && session.importSha256 == fmap snd state.importManifest
          && session.runtimeIdentity == job.initialRuntimeIdentity else session.importTaskId == Nothing) $ throwIO SourceChanged
        pure session
      completeImport source session = do
        (total,digest) <- maybe (throwIO SourceUnavailable) pure state.importManifest
        size <- checkSource source total
        unless (size == total && session.state == Core.Terminated && session.finalBytes == Just total && session.finalSha256 == Just digest) $ throwIO SourceChanged
        closeSource source
        completed <- registry.options.now
        let receiptValue = object ["committed_offset" .= total, "revision" .= session.revision, "sha256" .= digest,
              "capture_boundary" .= total, "pending_tail" .= False, "source_verified" .= True, "state" .= ("terminated" :: Text), "historical" .= True]
        atomically $ do
          completeControl job receiptValue
          modifyTVar' job.state (settled completed state {phase="imported",source=Nothing,remote=Just session,receipt=Just receiptValue,completedAt=Just completed,lastAck=Just completed,failure=Nothing})
      begin source session = if session.state == Core.Terminated then
          if job.imported then save state {remote=Just session,importCompleted=Just session,importScan=Just (ImportScan session.committedBytes 0 hashInit BS.empty)} else retire registry job "terminated" "session_terminated"
        else acknowledge state {remote = Just session, createNeeded = False, sweep = Just (newSweep Resume session), phase = "validating"}
  case state.source of
    Nothing -> mask_ $ do
      source <- openSource job.sourcePath
      boundary <- checkSource source 0 `onException` closeSource source
      save state {source = Just source, importScan = if job.imported then Just (ImportScan boundary 0 hashInit BS.empty) else Nothing}
    Just source -> do
      size <- checkSource source (max state.observedSize (maybe 0 (.committedBytes) state.remote))
      when (job.imported && maybe False ((/=size) . fst) state.importManifest) $ throwIO SourceChanged
      atomically $ modifyTVar' job.state (\s -> s {observedSize = max size s.observedSize})
      rawControl <- readTVarIO job.control
      let control = case rawControl of Just c | c.result == Nothing -> Just c; _ -> Nothing
      case control of
        Just c | not (isJust c.boundary) -> atomically $ modifyTVar' job.control (fmap (\current -> if current.operationId == c.operationId then current {boundary = Just size} else current))
        _ -> pure ()
      case state.importScan of
        Just (ImportScan boundary offset context tailBytes) -> do
          unless (size == boundary) $ throwIO SourceChanged
          if offset < boundary then do
            let count = fromIntegral (min 1048576 (boundary-offset))
            bytes <- readSource source offset count
            unless (BS.length bytes == count) $ throwIO SourceTruncated
            let records = BS.split 10 (tailBytes <> bytes)
                complete = init records
                tailNext = last records
            unless (all (\line -> BS.length line < 1048576 && validFinal line) complete && BS.length tailNext <= 1048576) $ throwIO InvalidImport
            let updated = hashUpdate context bytes
            updated `seq` registry.options.transition "import_validation_page"
            save state {importScan=Just (ImportScan boundary (offset+fromIntegral count) updated tailNext)}
          else do
            unless (BS.null tailBytes || validFinal tailBytes) $ throwIO InvalidImport
            let manifest = (boundary,T.pack (show (hashFinalize context :: Digest SHA256)))
            case state.importCompleted of
              Just completed -> do
                unless (state.importManifest == Just manifest) $ throwIO SourceChanged
                completeImport source completed
              Nothing -> do
                op <- UUID.nextRandom
                completion <- newEmptyTMVarIO
                atomically $ writeTVar job.control (Just (Control op True (Just boundary) Nothing completion))
                save state {importScan=Nothing,importManifest=Just manifest}
        Nothing -> advance source size control acceptSession begin completeImport save saveAt acknowledge acknowledgeAt mutated failHTTP verificationDelay pollDelay
  where
    advance source size (control :: Maybe Control) acceptSession begin completeImport save saveAt acknowledge acknowledgeAt mutated failHTTP verificationDelay pollDelay = do
      state <- readTVarIO job.state
      time <- registry.options.now
      case state.remote of
        Nothing | state.createNeeded -> do
          result <- createRemote registry job
          either failHTTP (acceptSession >=> begin source) result
        Nothing -> do
          result <- request registry job "GET" "" Nothing
          case result of
            Left (HTTPStatus 404) | not state.reconcile -> save state {createNeeded = True}
            Left fault -> failHTTP fault
            Right value -> acceptSession value >>= begin source
        Just session | not (isJust state.pending) && maybe True (\lastBeat -> diffUTCTime time lastBeat >= 10) state.lastHeartbeat -> do
          result <- request registry job "POST" "/heartbeat" Nothing
          either failHTTP (\value -> do
            canonical <- acceptSession value
            if canonical.state == Core.Terminated then if job.imported then begin source canonical else retire registry job "terminated" "session_terminated"
            else mutated state {remote = Just canonical
              , phase = if canonical.committedBytes /= session.committedBytes then "validating" else state.phase
              , sweep = if canonical.committedBytes /= session.committedBytes || canonical.revision /= session.revision then Just (newSweep Resume canonical) else state.sweep}) result
        Just session -> case state.pending of
          Just append | not state.reconcile -> do
            result <- request registry job "POST" "/append" (Just (toJSON append))
            case result of
              Left fault -> failHTTP fault
              Right value -> do
                response <- decodeValue value :: IO Wire.AppendSessionResponse
                canonical <- acceptSession (toJSON response.session)
                if canonical.state == Core.Terminated then if job.imported then begin source canonical else retire registry job "terminated" "session_terminated" else
                  mutated state {remote = Just canonical, pending = Nothing, phase = "following"
                    , sweep = if response.replayed || canonical.committedBytes /= append.expectedOffset + fromIntegral (either (const 0) BS.length (Wire.decodeSessionBytes append.dataBase64))
                        then Just (newSweep Resume canonical)
                        else case state.sweep of Nothing | time >= state.nextVerification -> Just (newSweep Periodic canonical); existing -> existing}
          _ -> case state.sweep of
            Just verify | verify.purpose /= Periodic || state.verifyNext && maybe True (<=time) verify.nextPoll -> do
              case verify.nextPoll of
                Just due | due > time -> saveAt due state
                _ | verify.offset < verify.end -> do
                  let count = fromIntegral (min 1048576 (verify.end-verify.offset))
                  bytes <- readSource source verify.offset count
                  unless (BS.length bytes == count) $ throwIO SourceTruncated
                  let updated = hashUpdate verify.context bytes
                  updated `seq` registry.options.transition "local_prefix_page"
                  save state {sweep = Just verify {offset = verify.offset+fromIntegral count, context = updated}, verifyNext = False}
                _ | verify.proven -> do
                  let digest = T.pack (show (hashFinalize verify.context :: Digest SHA256))
                  proof <- maybe (throwIO SourceUnavailable) pure verify.proof
                  unless (proof.sha256 == digest) $ throwIO SourceChanged
                  _ <- checkSource source (max verify.end (maybe verify.end id (control >>= (.boundary))))
                  if verify.purpose == Periodic then
                    save state {sweep = Nothing, verifyNext = False, nextVerification = addUTCTime verificationDelay time}
                  else do
                    -- This existing write-authorized endpoint supplies one latest
                    -- cursor checkpoint after the final local pass. A downgraded
                    -- read-only credential cannot complete a finite receipt.
                    result <- request registry job "POST" "/heartbeat" Nothing
                    case result of
                      Left fault -> failHTTP fault
                      Right value -> do
                        canonical <- acceptSession value
                        if canonical.state == Core.Terminated then if job.imported then begin source canonical else retire registry job "terminated" "session_terminated"
                        else if maybe False (canonical.committedBytes >) (control >>= (.boundary)) then retire registry job "failed" "capture_boundary_conflict"
                        else if canonical.committedBytes /= verify.end || canonical.revision /= verify.revision then
                          mutated state {remote = Just canonical, sweep = Just (newSweep verify.purpose canonical)}
                        else if verify.purpose == Resume then
                          mutated state {remote = Just canonical, sweep = Nothing, pending = if state.reconcile then Nothing else state.pending
                            , reconcile = False, phase = "following", verifyNext = False, nextVerification = addUTCTime verificationDelay time}
                        else if job.imported then do
                          (total,expected) <- maybe (throwIO SourceUnavailable) pure state.importManifest
                          unless (verify.end == total && digest == expected) $ throwIO SourceChanged
                          sealed <- request registry job "POST" "/terminate" (Just (toJSON (Wire.TerminateSessionRequest total canonical.revision total expected)))
                          either failHTTP (acceptSession >=> completeImport source) sealed
                        else do
                          let resultValue = object ["committed_offset" .= verify.end, "revision" .= canonical.revision, "sha256" .= digest
                                , "capture_boundary" .= (control >>= (.boundary)), "pending_tail" .= maybe False (> verify.end) (control >>= (.boundary)), "source_verified" .= True]
                          completed <- registry.options.now
                          stopNow <- atomically $ do
                            latest <- readTVar job.control
                            let stopping = maybe False (.stop) latest
                            if stopping then modifyTVar' job.state (\s -> s {phase = "finalizing"}) else do
                              completeControl job resultValue
                              modifyTVar' job.state (settled completed state {remote = Just canonical, receipt = Just resultValue, sweep = Nothing, phase = "caught_up"
                                , lastAck = Just completed, lastHeartbeat = Just completed, failures = 0, failure = Nothing, nextVerification = addUTCTime verificationDelay completed})
                            pure stopping
                          when stopNow $ do
                            registry.options.transition "stop_finalizing"
                            closeSource source
                            stoppedAt <- registry.options.now
                            atomically $ do
                              completeControl job resultValue
                              modifyTVar' job.state (settled stoppedAt state {phase = "stopped", remote = Just canonical, source = Nothing, receipt = Just resultValue
                                , completedAt = Just stoppedAt, sweep = Nothing, pending = Nothing, lastAck = Just completed, failures = 0, failure = Nothing})
                _ -> case verify.proof of
                  Nothing -> do
                    let digest = T.pack (show (hashFinalize verify.context :: Digest SHA256))
                        reusable = case state.lastProof of
                          Just (proof,audience) | proof.expectedOffset == verify.end && proof.expectedRevision == verify.revision && proof.sha256 == digest -> Just (proof,audience)
                          _ -> Nothing
                    case reusable of
                      Just (proof,audience) -> save state {sweep = Just verify {proof = Just proof, audience = Just audience, submitted = True}}
                      Nothing -> do
                        operation <- UUID.nextRandom
                        save state {sweep = Just verify {proof = Just (Wire.PrefixProofRequest operation verify.end verify.revision digest)}}
                  Just proof -> do
                    result <- if verify.submitted then request registry job "GET" ("/prefix-verifications/" <> show proof.operationId) Nothing
                      else request registry job "POST" "/prefix-verifications" (Just (toJSON proof))
                    case result of
                      Left (HTTPStatus 404) | verify.submitted ->
                        save state {sweep = Just verify {offset = 0, context = hashInit, proof = Nothing, audience = Nothing, submitted = False, nextPoll = Nothing}, lastProof = Nothing}
                      Left (HTTPStatus 409) -> retire registry job "failed" "verification_operation_conflict"
                      Left (HTTPStatus 400) -> retire registry job "failed" "verification_target_invalid"
                      Left fault -> failHTTP fault
                      Right value -> do
                        reply <- decodeValue value :: IO Wire.PrefixProofResponse
                        unless (reply.workspaceId == job.workspace && reply.sessionId == job.sid && reply.operationId == proof.operationId
                          && reply.expectedOffset == verify.end && reply.expectedRevision == verify.revision && reply.sha256 == proof.sha256
                          && not (T.null reply.audience) && BS.length (TE.encodeUtf8 reply.audience) <= 512
                          && maybe True (==reply.audience) verify.audience) $ throwIO SourceChanged
                        case reply.state of
                          "verified" -> do
                            unless (reply.error == Nothing) $ throwIO SourceChanged
                            -- A second bounded local pass catches persistent mutation
                            -- during remote verification before resume/finite receipt.
                            let checked = verify {audience = Just reply.audience, submitted = True, proven = True, nextPoll = Nothing
                                  , offset = if verify.purpose == Periodic then verify.offset else 0
                                  , context = if verify.purpose == Periodic then verify.context else hashInit}
                            acknowledge state {sweep = Just checked, lastProof = Just (proof,reply.audience)}
                          "pending" -> do
                            let due = addUTCTime pollDelay time
                                updated = state {sweep = Just verify {audience = Just reply.audience, submitted = True, nextPoll = Just due}, verifyNext = False}
                            if verify.purpose == Periodic then acknowledge updated else acknowledgeAt due updated
                          "mismatch" -> throwIO SourceChanged
                          "failed" -> retire registry job "failed" "verification_unavailable"
                          _ -> throwIO SourceUnavailable
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
              else if activeControl then save state {sweep = Just (newSweep Receipt session), phase = "validating"}
              else do
                let next = case state.sweep of Nothing | time >= state.nextVerification -> Just (newSweep Periodic session); existing -> existing
                    due = case next of Just verify -> maybe time id verify.nextPoll; Nothing -> addUTCTime (fromIntegral registry.options.intervalMicros / 1000000) time
                saveAt due state {phase = if BS.null bytes then "caught_up" else "pending_tail", verifyNext = True, sweep = next}

validFinal :: ByteString -> Bool
validFinal bytes = not (BS.null bytes) && case TE.decodeUtf8' bytes of
  Left _ -> False
  Right _ -> case eitherDecodeStrict' bytes :: Either String Value of Right _ -> True; Left _ -> False

createRemote :: Registry -> Job -> IO (Either HTTPFault Value)
createRemote registry job = bracket_ acquire release $ do
  state <- readTVarIO job.state
  let path = "/api/v1/workspaces/" <> show job.workspace <> "/sessions" <> if job.imported then "/imports" else ""
      metadata = Wire.CreateSessionRequest job.sid job.formatLabel job.title job.parentSessionId job.initialRuntimeIdentity job.taskId
      body = Just $ case state.importManifest of
        Just (total,digest) | job.imported -> toJSON (Wire.ImportSessionRequest metadata total digest)
        _ -> toJSON metadata
  case registry.options.requestOverride of Nothing -> followerRequest job.http "POST" path body; Just action -> action "POST" path body
  where
    acquire = atomically $ do n <- readTVar registry.slots; check (n < 2); writeTVar registry.slots (n+1); modifyTVar' registry.peak (max (n+1))
    release = atomically $ do
      poisoned <- scopePoisoned registry.processScope
      if poisoned then writeTVar registry.closed True else modifyTVar' registry.slots (subtract 1)

-- Fixed diagnostics: never expose raw response/parser or transcript content.
decodeValue :: FromJSON a => Value -> IO a
decodeValue value = either (const $ throwIO SourceUnavailable) pure (parseEither parseJSON value)

settled :: UTCTime -> JobState -> JobState -> JobState
settled completed updated current = updated
  { observedSize = max current.observedSize updated.observedSize, due = completed }
