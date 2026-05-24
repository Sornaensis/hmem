module HMem.MCP.Server
  ( runMCPServer
  , injectWorkspaceContext
  -- * Testing
  , JsonRpcRequest(..)
  , handleRequest
  , handleStdioLine
  , runMCPServerWithHandles
  , runMCPServerWithHandlesObserved
  , encodeStdioResponse
  , sendResponseToHandle
  ) where

import Control.Monad (replicateM)
import Control.Concurrent (ThreadId, forkIO, forkIOWithUnmask, killThread)
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Concurrent.STM
  ( atomically, TBQueue, newTBQueueIO, readTBQueue, writeTBQueue
  , isEmptyTBQueue, isFullTBQueue, TVar, newTVarIO, readTVar, modifyTVar', retry )
import Control.Exception (SomeAsyncException, SomeException, catch, finally, fromException, mask, onException, throwIO, try)
import Data.Aeson
import Data.Aeson.KeyMap qualified as KM
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
import HMem.MCP.Tools (handleToolCall, toolDefinitions)

------------------------------------------------------------------------
-- JSON-RPC types
------------------------------------------------------------------------

data JsonRpcRequest = JsonRpcRequest
  { reqId     :: Maybe Value
  , reqMethod :: Text
  , reqParams :: Maybe Value
  } deriving (Show)

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
runMCPServerWithHandlesObserved observeWorkers workerCount queueDepth input output errHandle mgr serverUrl mApiKey = mask $ \restore -> do
  lock   <- newMVar ()
  queue  <- newTBQueueIO (fromIntegral $ max 1 queueDepth)
  active <- newTVarIO (0 :: Int)
  initialized <- newTVarIO False
  wsContext <- newTVarIO (Nothing :: Maybe UUID)
  -- Spawn fixed worker pool with async exceptions masked in the parent so a
  -- cancellation cannot leak a partially-started pool. Workers are explicitly
  -- unmasked so request processing remains interruptible.
  workerIds <- replicateM (max 1 workerCount) $
    forkIOWithUnmask $ \unmask -> unmask $ worker output mgr lock serverUrl mApiKey queue active initialized wsContext
  let cleanupWorkers = mapM_ killThread workerIds
      requestWorkerCleanup = mapM_ (forkIO . killThread) workerIds
      runLoop = restore $ do
        -- Read stdin → queue
        readLoop input output lock queue
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

-- | Worker thread: reads from the queue and processes each line.
-- Atomically dequeues + increments active counter to prevent drain races.
worker :: Handle -> Manager -> MVar () -> String -> Maybe Text -> TBQueue BS8.ByteString -> TVar Int -> TVar Bool -> TVar (Maybe UUID) -> IO ()
worker output mgr lock url mApiKey queue active initialized wsContext = go
  where
    go = do
      mline <- try @SomeException $ atomically $ do
        l <- readTBQueue queue
        modifyTVar' active (+ 1)
        pure l
      case mline of
        Left e -> rethrowAsync e  -- Worker exits cleanly for non-async queue errors
        Right line -> do
          (processLine output mgr lock url mApiKey initialized wsContext line `catch` \(e :: SomeException) -> rethrowAsync e)
            `finally` atomically (modifyTVar' active (subtract 1))
          go

-- | Main read loop: reads lines from an input handle and enqueues them.
-- If the queue is full, sends an overload error immediately.
readLoop :: Handle -> Handle -> MVar () -> TBQueue BS8.ByteString -> IO ()
readLoop input output lock queue = do
  eof <- hIsEOF input `catch` \(e :: SomeException) -> rethrowAsync e >> pure True
  if eof
    then pure ()
    else do
      line <- BS8.hGetLine input
      if BS8.null line
        then readLoop input output lock queue
        else do
          full <- atomically $ do
            f <- isFullTBQueue queue
            if f then pure True
            else do
              writeTBQueue queue line
              pure False
          if full
            then sendResponseToHandle output lock $ jsonRpcError Nothing (-32000) "Server overloaded"
            else pure ()
          readLoop input output lock queue

processLine :: Handle -> Manager -> MVar () -> String -> Maybe Text -> TVar Bool -> TVar (Maybe UUID) -> BS8.ByteString -> IO ()
processLine output mgr lock serverUrl mApiKey initialized wsContext line = do
  mresp <- handleStdioLine mgr serverUrl mApiKey initialized wsContext line
  case mresp of
    Nothing   -> pure ()
    Just resp -> sendResponseToHandle output lock resp

-- | Handle one line-delimited JSON-RPC message read from stdin.
-- Returns 'Nothing' for blank lines and notifications, matching the
-- stdio loop's no-response behavior.
handleStdioLine :: Manager -> String -> Maybe Text -> TVar Bool -> TVar (Maybe UUID) -> BS8.ByteString -> IO (Maybe Value)
handleStdioLine mgr serverUrl mApiKey initialized wsContext line
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
          handleRequest mgr serverUrl mApiKey initialized wsContext req
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
handleRequest :: Manager -> String -> Maybe Text -> TVar Bool -> TVar (Maybe UUID) -> JsonRpcRequest -> IO (Maybe Value)
handleRequest mgr serverUrl mApiKey initialized wsContext req = case req.reqMethod of
  "initialize" -> do
    atomically $ modifyTVar' initialized (const True)
    pure $ Just $ jsonRpcResponse req.reqId $ object
      [ "protocolVersion" .= ("2024-11-05" :: Text)
      , "capabilities"    .= object
          [ "tools" .= object
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
      else handleMethod mgr serverUrl mApiKey wsContext req

-- | Dispatch initialized requests to the appropriate handler.
handleMethod :: Manager -> String -> Maybe Text -> TVar (Maybe UUID) -> JsonRpcRequest -> IO (Maybe Value)
handleMethod mgr serverUrl mApiKey wsContext req = case req.reqMethod of
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
          Just "get_workspace" -> handleGetWorkspace wsContext req
          _ -> do
            -- Inject workspace_id from context when absent
            params' <- injectWorkspaceContext wsContext params
            result <- handleToolCall mgr serverUrl mApiKey params'
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

-- | Handle the set_workspace tool: stores a workspace UUID in the
-- server's session state so subsequent tool calls can omit workspace_id.
-- Passing null or omitting workspace_id clears the context.
handleSetWorkspace :: TVar (Maybe UUID) -> JsonRpcRequest -> Value -> IO (Maybe Value)
handleSetWorkspace wsContext req params = do
  let mWsIdText = case params of
        Object o -> case KM.lookup "arguments" o of
          Just (Object args) -> case KM.lookup "workspace_id" args of
            Just (String s) -> Just (Just s)
            Just Null       -> Just Nothing   -- explicit clear
            Nothing         -> Just Nothing   -- omitted = clear
            _               -> Nothing        -- invalid type
          _ -> Just Nothing   -- no arguments = clear
        _ -> Nothing
  case mWsIdText of
    Nothing -> pure $ Just $ jsonRpcResponse req.reqId $ object
      [ "content" .= [ object [ "type" .= t "text", "text" .= t "Error: invalid workspace_id format" ] ]
      , "isError" .= True
      ]
    Just Nothing -> do
      atomically $ modifyTVar' wsContext (const Nothing)
      hPutStrLn stderr "MCP server: workspace context cleared."
      pure $ Just $ jsonRpcResponse req.reqId $ mcpJSON $ object
        [ "ok" .= True
        , "action" .= ("cleared" :: Text)
        , "entity_type" .= ("workspace_context" :: Text)
        ]
    Just (Just wsText) -> case UUID.fromText wsText of
      Nothing -> pure $ Just $ jsonRpcResponse req.reqId $ object
        [ "content" .= [ object [ "type" .= t "text", "text" .= ("Invalid UUID: " <> wsText) ] ]
        , "isError" .= True
        ]
      Just uuid -> do
        atomically $ modifyTVar' wsContext (const (Just uuid))
        hPutStrLn stderr $ "MCP server: workspace context set to " <> T.unpack wsText
        pure $ Just $ jsonRpcResponse req.reqId $ mcpJSON $ object
          [ "ok" .= True
          , "action" .= ("set" :: Text)
          , "entity_type" .= ("workspace_context" :: Text)
          , "workspace_id" .= uuid
          ]
  where
    t :: Text -> Text
    t = id

-- | Handle the get_workspace tool: returns the current workspace context.
handleGetWorkspace :: TVar (Maybe UUID) -> JsonRpcRequest -> IO (Maybe Value)
handleGetWorkspace wsContext req = do
  mws <- atomically $ readTVar wsContext
  pure $ Just $ jsonRpcResponse req.reqId $ mcpJSON $ object
    [ "workspace_id" .= mws ]


mcpJSON :: Value -> Value
mcpJSON value = object
  [ "content" .= [ object [ "type" .= ("text" :: Text), "text" .= decodeUtf8 (encode value) ] ] ]


decodeUtf8 :: BL.ByteString -> Text
decodeUtf8 = TE.decodeUtf8 . BL.toStrict

-- | Inject the stored workspace_id into tool call arguments when the
-- arguments object does not already contain a workspace_id field.
-- Explicit workspace_id in the call always takes precedence.
injectWorkspaceContext :: TVar (Maybe UUID) -> Value -> IO Value
injectWorkspaceContext wsContext params = do
  mws <- atomically $ readTVar wsContext
  case mws of
    Nothing -> pure params
    Just wsId -> pure $ case params of
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
