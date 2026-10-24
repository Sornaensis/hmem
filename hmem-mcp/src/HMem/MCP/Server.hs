module HMem.MCP.Server
  ( runMCPServer
  , MCPContext(..)
  , injectWorkspaceContext
  -- * Testing
  , JsonRpcRequest(..)
  , handleRequest
  , handleStdioLineWithRegistry
  , handleStdioLine
  , runMCPServerWithHandles
  , runMCPServerWithHandlesObserved
  , runMCPServerWithHandlesObservedWithFork
  , runMCPServerWithHandlesObservedWithFollowerOptions
  , encodeStdioResponse
  , sendResponseToHandle
  ) where

import Control.Concurrent (ThreadId, forkIO, forkIOWithUnmask, killThread)
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Concurrent.STM
  ( atomically, TBQueue, newTBQueueIO, readTBQueue, writeTBQueue
  , isEmptyTBQueue, isFullTBQueue, TVar, newTVarIO, readTVar, modifyTVar', retry )
import Control.Exception (bracket, SomeAsyncException, SomeException, catch, finally, fromException, mask, onException, throwIO, try)
import Data.Aeson
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types (parseEither)
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Network.HTTP.Client (Manager, newManager, managerResponseTimeout, responseTimeoutMicro)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.IO (Handle, hFlush, hSetBuffering, stdin, stdout, stderr, hPutStrLn, BufferMode (..), hIsEOF)
import HMem.BotIdentity (BotIdentity)
import HMem.MCP.SessionFollower (Registry, FollowerOptions, defaultFollowerOptions, newRegistryWith, closeRegistry, handleUploadTool, isUploadTool)
import HMem.MCP.Tools (handleToolCall, searchContextError, toolDefinitions)

------------------------------------------------------------------------
-- JSON-RPC types
------------------------------------------------------------------------

data JsonRpcRequest = JsonRpcRequest
  { reqId     :: Maybe Value
  , reqMethod :: Text
  , reqParams :: Maybe Value
  } deriving (Show)

-- | Per-process accepted declaration. Null workspace permits global operations.
data MCPContext = MCPContext
  { workspaceId :: !(Maybe UUID)
  , identity :: !BotIdentity
  } deriving (Show, Eq)

-- | Capture the whole context, including an absent declaration, at acceptance.
-- Context controls run in the reader after draining the preceding epoch.
data QueuedLine = QueuedLine BS8.ByteString (Maybe MCPContext)

instance FromJSON JsonRpcRequest where
  parseJSON = withObject "JsonRpcRequest" $ \o -> JsonRpcRequest
    <$> o .:? "id"
    <*> o .:  "method"
    <*> o .:? "params"

jsonRpcResponse :: Maybe Value -> Value -> Value
jsonRpcResponse rid result = object
  [ "jsonrpc" .= ("2.0" :: Text)
  , "id"      .= rid
  , "result"  .= result
  ]

jsonRpcError :: Maybe Value -> Int -> Text -> Value
jsonRpcError rid code msg = object
  [ "jsonrpc" .= ("2.0" :: Text)
  , "id"      .= rid
  , "error"   .= object
      [ "code"    .= code
      , "message" .= msg
      ]
  ]

------------------------------------------------------------------------
-- MCP server loop
------------------------------------------------------------------------

-- | Run the MCP server, reading line-delimited JSON-RPC from stdin.
-- Creates a single HTTP manager for the lifetime of the process.
-- A fixed pool of worker threads reads from a bounded queue to provide
-- backpressure without unbounded thread creation.

-- | Maximum number of concurrent worker threads.
maxConcurrency :: Int
maxConcurrency = 16

-- | Maximum number of pending requests in the queue before rejecting.
maxQueueDepth :: Int
maxQueueDepth = 64

runMCPServer :: String -> Maybe Text -> IO ()
runMCPServer serverUrl mApiKey = do
  hSetBuffering stdin  LineBuffering
  hSetBuffering stdout LineBuffering
  mgr    <- newManager tlsManagerSettings
              { managerResponseTimeout = responseTimeoutMicro (30 * 1000000) }
  runMCPServerWithHandles maxConcurrency maxQueueDepth stdin stdout stderr mgr serverUrl mApiKey

-- | Run the MCP stdio loop against explicit handles.
--
-- This is the same read → queue → worker → write pipeline used by
-- 'runMCPServer', but parameterized for handle-level tests. The explicit
-- error handle receives loop drain/shutdown messages; protocol-level
-- diagnostics inside request handlers still use the process stderr.
runMCPServerWithHandles :: Int -> Int -> Handle -> Handle -> Handle -> Manager -> String -> Maybe Text -> IO ()
runMCPServerWithHandles = runMCPServerWithHandlesObserved (const $ pure ())

-- | Like 'runMCPServerWithHandles', but reports worker thread IDs after
-- input has been read and before drain/shutdown logging. This is a test seam
-- for asserting shutdown cleanup.
runMCPServerWithHandlesObserved :: ([ThreadId] -> IO ()) -> Int -> Int -> Handle -> Handle -> Handle -> Manager -> String -> Maybe Text -> IO ()
runMCPServerWithHandlesObserved = runMCPServerWithHandlesObservedWithFork forkWorkerUnmasked

-- | Like 'runMCPServerWithHandlesObserved', but allows tests to inject worker
-- startup behavior. The fork action must run the supplied worker action
-- unmasked so queue and request processing remain interruptible.
runMCPServerWithHandlesObservedWithFork :: (IO () -> IO ThreadId) -> ([ThreadId] -> IO ()) -> Int -> Int -> Handle -> Handle -> Handle -> Manager -> String -> Maybe Text -> IO ()
runMCPServerWithHandlesObservedWithFork = runMCPServerWithHandlesObservedWithFollowerOptions defaultFollowerOptions

-- | Exercise the actual registry/stdio lifetime with injected follower scheduling
-- and observations. Ordinary request admission and worker ownership are unchanged.
runMCPServerWithHandlesObservedWithFollowerOptions :: FollowerOptions -> (IO () -> IO ThreadId) -> ([ThreadId] -> IO ()) -> Int -> Int -> Handle -> Handle -> Handle -> Manager -> String -> Maybe Text -> IO ()
runMCPServerWithHandlesObservedWithFollowerOptions options forkWorker observeWorkers workerCount queueDepth input output errHandle mgr serverUrl mApiKey = bracket (newRegistryWith options) cleanupFollower $ \registry -> mask $ \restore -> do
  lock   <- newMVar ()
  queue  <- newTBQueueIO (fromIntegral $ max 1 queueDepth)
  active <- newTVarIO (0 :: Int)
  initialized <- newTVarIO False
  wsContext <- newTVarIO (Nothing :: Maybe MCPContext)
  -- Spawn fixed worker pool with async exceptions masked in the parent so a
  -- cancellation cannot leak a partially-started pool. Already-started workers
  -- are cleaned up if a later startup step fails. Workers are explicitly
  -- unmasked by the fork action so request processing remains interruptible.
  workerIds <- spawnWorkers forkWorker (max 1 workerCount) $
    worker (Just registry) output mgr lock serverUrl mApiKey queue active initialized wsContext
  let cleanupWorkers = mapM_ killThread workerIds
      requestWorkerCleanup = mapM_ (forkIO . killThread) workerIds
      runLoop = restore $ do
        -- Read stdin → queue
        readLoop (Just registry) input output lock queue active mgr serverUrl mApiKey initialized wsContext
        observeWorkers workerIds
        -- Drain: wait for queue to empty and all workers to finish
        hPutStrLn errHandle "MCP server: stdin closed, draining in-flight requests..."
        atomically $ do
          empty <- isEmptyTBQueue queue
          n     <- readTVar active
          if empty && n == 0 then pure () else retry
        hPutStrLn errHandle "MCP server: shutdown complete."
  runLoop `onException` requestWorkerCleanup
  cleanupWorkers

forkWorkerUnmasked :: IO () -> IO ThreadId
forkWorkerUnmasked action = forkIOWithUnmask $ \unmask -> unmask action

cleanupFollower :: Registry -> IO ()
cleanupFollower registry = do
  ok <- closeRegistry registry
  if ok then pure () else throwIO (userError "MCP follower cleanup did not complete within its budget")

spawnWorkers :: (IO () -> IO ThreadId) -> Int -> IO () -> IO [ThreadId]
spawnWorkers forkWorker workerCount action = go workerCount []
  where
    go remaining started
      | remaining <= 0 = pure $ reverse started
      | otherwise = do
          workerId <- forkWorker action
          go (remaining - 1) (workerId : started) `onException` killThread workerId

-- | Worker thread: reads from the queue and processes each line.
-- Atomically dequeues + increments active counter to prevent drain races.
worker :: Maybe Registry -> Handle -> Manager -> MVar () -> String -> Maybe Text -> TBQueue QueuedLine -> TVar Int -> TVar Bool -> TVar (Maybe MCPContext) -> IO ()
worker follower output mgr lock url mApiKey queue active initialized wsContext = go
  where
    go = do
      mline <- try @SomeException $ atomically $ do
        l <- readTBQueue queue
        modifyTVar' active (+ 1)
        pure l
      case mline of
        Left e -> rethrowAsync e  -- Worker exits cleanly for non-async queue errors
        Right (QueuedLine line workspaceSnapshot) -> do
          (processLineWithWorkspaceSnapshot follower output mgr lock url mApiKey initialized wsContext (Just workspaceSnapshot) line `catch` \(e :: SomeException) -> rethrowAsync e)
            `finally` atomically (modifyTVar' active (subtract 1))
          go

-- | Main read loop: reads lines from an input handle and enqueues them.
-- If the queue is full, sends an overload error immediately.
readLoop :: Maybe Registry -> Handle -> Handle -> MVar () -> TBQueue QueuedLine -> TVar Int -> Manager -> String -> Maybe Text -> TVar Bool -> TVar (Maybe MCPContext) -> IO ()
readLoop follower input output lock queue active mgr serverUrl mApiKey initialized wsContext = do
  eof <- hIsEOF input `catch` \(e :: SomeException) -> rethrowAsync e >> pure True
  if eof
    then pure ()
    else do
      line <- BS8.hGetLine input
      if BS8.null line
        then continue
        else do
          if isOrderedContextControl line
            then do
              -- A context transition starts a new epoch.  Let all ordinary
              -- requests accepted in the preceding epoch finish before
              -- committing it, so their externally-visible effects cannot
              -- overtake later context controls.
              waitForDrain queue active
              processLine follower output mgr lock serverUrl mApiKey initialized wsContext line
            else do
              workspaceSnapshot <- atomically $ readTVar wsContext
              full <- atomically $ do
                f <- isFullTBQueue queue
                if f then pure True
                else do
                  writeTBQueue queue (QueuedLine line workspaceSnapshot)
                  pure False
              if full
                then sendResponseToHandle output lock $ jsonRpcError Nothing (-32000) "Server overloaded"
                else pure ()
          continue
  where
    continue = readLoop follower input output lock queue active mgr serverUrl mApiKey initialized wsContext

-- | Wait until no accepted ordinary request can still be running.  This is
-- used only at workspace-context epoch boundaries; workers remain concurrent
-- for ordinary requests within an epoch.
waitForDrain :: TBQueue QueuedLine -> TVar Int -> IO ()
waitForDrain queue active = atomically $ do
  empty <- isEmptyTBQueue queue
  n <- readTVar active
  if empty && n == 0 then pure () else retry

-- | Initialization and workspace/identity declarations are local transitions. Run
-- them in the input reader so their effects are committed before accepting the
-- next request.  Notifications retain their existing ignored semantics.
isOrderedContextControl :: BS8.ByteString -> Bool
isOrderedContextControl line = case eitherDecodeStrict @JsonRpcRequest line of
  Right req
    | not (isNotification req)
    , req.reqMethod == "initialize" -> True
    | not (isNotification req)
    , req.reqMethod == "tools/call" -> case req.reqParams of
        Just (Object o) -> case KM.lookup "name" o of
          Just (String "set_workspace") -> True
          _ -> False
        _ -> False
  _ -> False

processLine :: Maybe Registry -> Handle -> Manager -> MVar () -> String -> Maybe Text -> TVar Bool -> TVar (Maybe MCPContext) -> BS8.ByteString -> IO ()
processLine follower output mgr lock serverUrl mApiKey initialized wsContext =
  processLineWithWorkspaceSnapshot follower output mgr lock serverUrl mApiKey initialized wsContext Nothing

processLineWithWorkspaceSnapshot :: Maybe Registry -> Handle -> Manager -> MVar () -> String -> Maybe Text -> TVar Bool -> TVar (Maybe MCPContext) -> Maybe (Maybe MCPContext) -> BS8.ByteString -> IO ()
processLineWithWorkspaceSnapshot follower output mgr lock serverUrl mApiKey initialized wsContext workspaceSnapshot line = do
  mresp <- handleStdioLineWithWorkspaceSnapshot follower mgr serverUrl mApiKey initialized wsContext workspaceSnapshot line
  case mresp of
    Nothing   -> pure ()
    Just resp -> sendResponseToHandle output lock resp

-- | Handle one line-delimited JSON-RPC message read from stdin.
-- Returns 'Nothing' for blank lines and notifications, matching the
-- stdio loop's no-response behavior.
handleStdioLine :: Manager -> String -> Maybe Text -> TVar Bool -> TVar (Maybe MCPContext) -> BS8.ByteString -> IO (Maybe Value)
handleStdioLine mgr serverUrl mApiKey initialized wsContext line
  = handleStdioLineWithWorkspaceSnapshot Nothing mgr serverUrl mApiKey initialized wsContext Nothing line

handleStdioLineWithRegistry :: Registry -> Manager -> String -> Maybe Text -> TVar Bool -> TVar (Maybe MCPContext) -> BS8.ByteString -> IO (Maybe Value)
handleStdioLineWithRegistry registry mgr serverUrl mApiKey initialized wsContext =
  handleStdioLineWithWorkspaceSnapshot (Just registry) mgr serverUrl mApiKey initialized wsContext Nothing

handleStdioLineWithWorkspaceSnapshot :: Maybe Registry -> Manager -> String -> Maybe Text -> TVar Bool -> TVar (Maybe MCPContext) -> Maybe (Maybe MCPContext) -> BS8.ByteString -> IO (Maybe Value)
handleStdioLineWithWorkspaceSnapshot follower mgr serverUrl mApiKey initialized wsContext workspaceSnapshot line
  | BS8.null line = pure Nothing
  | otherwise = case eitherDecodeStrict @Value line of
      Left _err ->
        -- Invalid JSON → -32700 Parse error (JSON-RPC 2.0 §5.1)
        pure $ Just $ jsonRpcError Nothing (-32700) "Parse error"
      Right val -> case eitherDecodeStrict @JsonRpcRequest line of
        Left _err ->
          -- Valid JSON but missing required fields (e.g. "method") → -32600
          pure $ Just $ jsonRpcError (extractId val) (-32600)
            "Invalid Request: missing required 'method' field"
        Right req ->
          handleRequestWithWorkspaceSnapshot follower mgr serverUrl mApiKey initialized wsContext workspaceSnapshot req
            `catch` \(e :: SomeException) -> do
              rethrowAsync e
              pure $ Just $ jsonRpcError req.reqId (-32603)
                ("Internal error: " <> T.pack (show e))

rethrowAsync :: SomeException -> IO ()
rethrowAsync e = case fromException e :: Maybe SomeAsyncException of
  Just _  -> throwIO e
  Nothing -> pure ()

-- | Try to extract the "id" from arbitrary JSON for error responses.
extractId :: Value -> Maybe Value
extractId (Object o) = KM.lookup "id" o
extractId _          = Nothing

-- | Handle a JSON-RPC request.  Returns 'Nothing' for notifications
-- (requests without an @id@ field), per JSON-RPC 2.0 §4.1.
--
-- Enforces the MCP initialization handshake: only @initialize@ and
-- @notifications/initialized@ are accepted before the handshake
-- completes.  All other methods receive @-32002@ ("Server not
-- initialized").
handleRequest :: Manager -> String -> Maybe Text -> TVar Bool -> TVar (Maybe MCPContext) -> JsonRpcRequest -> IO (Maybe Value)
handleRequest mgr serverUrl mApiKey initialized wsContext =
  handleRequestWithWorkspaceSnapshot Nothing mgr serverUrl mApiKey initialized wsContext Nothing

-- | Handle a request, optionally using the whole context captured by the
-- input reader.  Direct callers use 'handleRequest' and retain live-state
-- behavior; queued stdio requests use a stable snapshot.
handleRequestWithWorkspaceSnapshot :: Maybe Registry -> Manager -> String -> Maybe Text -> TVar Bool -> TVar (Maybe MCPContext) -> Maybe (Maybe MCPContext) -> JsonRpcRequest -> IO (Maybe Value)
handleRequestWithWorkspaceSnapshot follower mgr serverUrl mApiKey initialized wsContext workspaceSnapshot req = case req.reqMethod of
  "initialize" -> do
    atomically $ modifyTVar' initialized (const True)
    pure $ Just $ jsonRpcResponse req.reqId $ object
      [ "protocolVersion" .= ("2024-11-05" :: Text)
      , "capabilities"    .= object
          [ "experimental" .= object ["hmem/structured-tool-output" .= object ["version" .= (1 :: Int)]]
          , "tools" .= object
              [ "listChanged" .= False
              ]
          ]
      , "serverInfo"      .= object
          [ "name"    .= ("hmem-mcp" :: Text)
          , "version" .= ("0.1.0" :: Text)
          ]
      ]

  "notifications/initialized" -> do
    hPutStrLn stderr "MCP server: initialization handshake complete."
    pure Nothing

  -- Any other notification (no id) → silently drop per JSON-RPC 2.0 §4.1.
  -- This covers notifications/cancelled, custom notifications, etc.
  _ | isNotification req -> do
        hPutStrLn stderr $ "MCP notification (ignored): " <> T.unpack req.reqMethod
        pure Nothing

  _ -> do
    ready <- atomically $ readTVar initialized
    if not ready
      then pure $ Just $ jsonRpcError req.reqId (-32002) "Server not initialized"
      else handleMethod follower mgr serverUrl mApiKey wsContext workspaceSnapshot req

-- | Dispatch initialized requests to the appropriate handler.
handleMethod :: Maybe Registry -> Manager -> String -> Maybe Text -> TVar (Maybe MCPContext) -> Maybe (Maybe MCPContext) -> JsonRpcRequest -> IO (Maybe Value)
handleMethod follower mgr serverUrl mApiKey wsContext workspaceSnapshot req = case req.reqMethod of
  "tools/list" -> do
    pure $ Just $ jsonRpcResponse req.reqId $ object
      [ "tools" .= toolDefinitions ]

  "tools/call" -> do
    case req.reqParams of
      Nothing -> pure $ Just $ jsonRpcError req.reqId (-32602)
          "Invalid params: tools/call requires 'name' and 'arguments'"
      Just params -> do
        -- Handle workspace context tools directly (they mutate server state)
        let mToolName = case params of
              Object o -> case KM.lookup "name" o of
                Just (String n) -> Just n
                _               -> Nothing
              _ -> Nothing
        case mToolName of
          Just "set_workspace" -> handleSetWorkspace wsContext req params
          Just "get_workspace" -> handleGetWorkspaceSnapshot wsContext workspaceSnapshot req
          _ -> do
            capturedWorkspace <- case workspaceSnapshot of
              Just captured -> pure captured
              Nothing -> atomically $ readTVar wsContext
            case capturedWorkspace of
              Nothing -> pure $ Just $ jsonRpcResponse req.reqId $
                if maybe False isUploadTool mToolName || mToolName `elem` (map Just ["search", "observation_discovery", "session_create", "session_list", "session_detail", "session_tasks", "session_associate", "session_terminate", "session_read", "session_evidence", "session_set_runtime"]) then searchContextError else object
                  [ "content" .= [object ["type" .= ("text" :: Text), "text" .= ("Call set_workspace with workspace_id and identity before using ordinary tools" :: Text)]]
                  , "isError" .= True
                  ]
              Just accepted -> do
                let params' = injectWorkspaceContextValue capturedWorkspace params
                result <- case (follower, mToolName, params') of
                  (Just registry, Just name, Object fields) | isUploadTool name ->
                    handleUploadTool registry mgr serverUrl mApiKey accepted.identity name (maybe (object []) id (KM.lookup "arguments" fields))
                  (_, Just name, _) | isUploadTool name -> pure $ object ["isError" .= True, "content" .= [object ["type" .= ("text" :: Text), "text" .= ("Uploader requires a process-owned registry" :: Text)]]]
                  _ -> handleToolCall mgr serverUrl mApiKey accepted.identity params'
                pure $ Just $ jsonRpcResponse req.reqId result

  method ->
    pure $ Just $ jsonRpcError req.reqId (-32601)
      ("Method not found: " <> method)

-- | JSON-RPC 2.0 notifications have no @id@ field.
isNotification :: JsonRpcRequest -> Bool
isNotification req = case req.reqId of
  Nothing -> True
  _       -> False

sendResponseToHandle :: Handle -> MVar () -> Value -> IO ()
sendResponseToHandle handle lock v = withMVar lock $ \_ -> do
  BL.hPut handle (encodeStdioResponse v)
  hFlush handle

encodeStdioResponse :: Value -> BL.ByteString
encodeStdioResponse = (`BL8.snoc` '\n') . encode

------------------------------------------------------------------------
-- Workspace context helpers
------------------------------------------------------------------------

-- | Validate the complete declaration before atomically replacing context.
-- Explicit null clears workspace and accepts identity; initialize preserves both.
-- Identity is forgotten only when this process exits.
handleSetWorkspace :: TVar (Maybe MCPContext) -> JsonRpcRequest -> Value -> IO (Maybe Value)
handleSetWorkspace wsContext req params = case parseEither parseDeclaration params of
  Left err -> pure $ Just $ jsonRpcResponse req.reqId $ object
    [ "content" .= [object ["type" .= ("text" :: Text), "text" .= T.pack err]]
    , "isError" .= True
    ]
  Right context -> do
    atomically $ modifyTVar' wsContext (const (Just context))
    pure $ Just $ jsonRpcResponse req.reqId $ mcpJSON $ object
      [ "ok" .= True
      , "action" .= (if maybe True (const False) context.workspaceId then "cleared" else "set" :: Text)
      , "entity_type" .= ("workspace_context" :: Text)
      , "workspace_id" .= context.workspaceId
      , "identity" .= context.identity
      ]
  where
    parseDeclaration = withObject "set_workspace params" $ \o -> do
      args <- o .: "arguments"
      withObject "set_workspace arguments" (\fields -> do
        if all (`elem` ["workspace_id", "identity"]) (KM.keys fields)
          then pure () else fail "set_workspace accepts only workspace_id and identity"
        declared <- fields .: "identity"
        workspaceValue <- fields .: "workspace_id"
        case workspaceValue of
          Null -> pure (MCPContext Nothing declared)
          String ws -> case UUID.fromText ws of
            Nothing -> fail "invalid workspace_id format: expected UUID or null"
            Just uuid -> pure (MCPContext (Just uuid) declared)
          _ -> fail "invalid workspace_id format: expected UUID or null"
        ) args

-- | Handle the get_workspace tool: returns the current workspace context.
handleGetWorkspace :: TVar (Maybe MCPContext) -> JsonRpcRequest -> IO (Maybe Value)
handleGetWorkspace wsContext req = do
  mws <- atomically $ readTVar wsContext
  pure $ workspaceResponse req mws

handleGetWorkspaceSnapshot :: TVar (Maybe MCPContext) -> Maybe (Maybe MCPContext) -> JsonRpcRequest -> IO (Maybe Value)
handleGetWorkspaceSnapshot wsContext workspaceSnapshot req = case workspaceSnapshot of
  Just capturedWorkspace -> pure $ workspaceResponse req capturedWorkspace
  Nothing -> handleGetWorkspace wsContext req

workspaceResponse :: JsonRpcRequest -> Maybe MCPContext -> Maybe Value
workspaceResponse req mws =
  Just $ jsonRpcResponse req.reqId $ mcpJSON $ object
    [ "workspace_id" .= (mws >>= (.workspaceId)), "identity" .= fmap (.identity) mws ]


mcpJSON :: Value -> Value
mcpJSON value = object
  [ "content" .= [ object [ "type" .= ("text" :: Text), "text" .= decodeUtf8 (encode value) ] ] ]


decodeUtf8 :: BL.ByteString -> Text
decodeUtf8 = TE.decodeUtf8 . BL.toStrict

-- | Inject the stored workspace_id into tool call arguments when the
-- arguments object does not already contain a workspace_id field.
-- Explicit workspace_id in the call always takes precedence.
injectWorkspaceContext :: TVar (Maybe MCPContext) -> Value -> IO Value
injectWorkspaceContext wsContext params = do
  mws <- atomically $ readTVar wsContext
  pure $ injectWorkspaceContextValue mws params

-- | Apply a captured workspace value only when the call does not explicitly
-- provide workspace_id.  Explicit values, including null and invalid values,
-- deliberately remain visible to tool validation.
injectWorkspaceContextValue :: Maybe MCPContext -> Value -> Value
injectWorkspaceContextValue Nothing params = params
injectWorkspaceContextValue (Just (MCPContext Nothing _)) params = params
injectWorkspaceContextValue (Just (MCPContext (Just wsId) _)) params = case params of
  Object o -> case KM.lookup "arguments" o of
    Just (Object args)
      | not (KM.member "workspace_id" args) ->
          Object $ KM.insert "arguments"
            (Object $ KM.insert "workspace_id" (toJSON wsId) args) o
    Nothing ->
      Object $ KM.insert "arguments"
        (object ["workspace_id" .= wsId]) o
    Just Null ->
      Object $ KM.insert "arguments"
        (object ["workspace_id" .= wsId]) o
    _ -> params
  _ -> params
