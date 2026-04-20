module HMem.MCP.Server
  ( runMCPServer
  ) where

import Control.Monad (replicateM_)
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Concurrent.STM
  ( atomically, TBQueue, newTBQueueIO, readTBQueue, writeTBQueue
  , isEmptyTBQueue, isFullTBQueue, TVar, newTVarIO, readTVar, modifyTVar', retry )
import Control.Exception (SomeException, catch, finally, try)
import Data.Aeson
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.Text (Text)
import Data.Text qualified as T
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Network.HTTP.Client (Manager, newManager, managerResponseTimeout, responseTimeoutMicro)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.IO (hFlush, hSetBuffering, stdin, stdout, stderr, hPutStrLn, BufferMode (..), hIsEOF)
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
  lock   <- newMVar ()
  queue  <- newTBQueueIO (fromIntegral maxQueueDepth)
  active <- newTVarIO (0 :: Int)
  initialized <- newTVarIO False
  wsContext <- newTVarIO (Nothing :: Maybe UUID)
  -- Spawn fixed worker pool
  replicateM_ maxConcurrency $ forkIO $ worker mgr lock serverUrl mApiKey queue active initialized wsContext
  -- Read stdin → queue
  readLoop lock queue
  -- Drain: wait for queue to empty and all workers to finish
  hPutStrLn stderr "MCP server: stdin closed, draining in-flight requests..."
  atomically $ do
    empty <- isEmptyTBQueue queue
    n     <- readTVar active
    if empty && n == 0 then pure () else retry
  hPutStrLn stderr "MCP server: shutdown complete."

-- | Worker thread: reads from the queue and processes each line.
-- Atomically dequeues + increments active counter to prevent drain races.
worker :: Manager -> MVar () -> String -> Maybe Text -> TBQueue BS8.ByteString -> TVar Int -> TVar Bool -> TVar (Maybe UUID) -> IO ()
worker mgr lock url mApiKey queue active initialized wsContext = go
  where
    go = do
      mline <- try @SomeException $ atomically $ do
        l <- readTBQueue queue
        modifyTVar' active (+ 1)
        pure l
      case mline of
        Left _ -> pure ()  -- Worker exits cleanly
        Right line -> do
          (processLine mgr lock url mApiKey initialized wsContext line `catch` \(_ :: SomeException) -> pure ())
            `finally` atomically (modifyTVar' active (subtract 1))
          go

-- | Main read loop: reads lines from stdin and enqueues them.
-- If the queue is full, sends an overload error immediately.
readLoop :: MVar () -> TBQueue BS8.ByteString -> IO ()
readLoop lock queue = do
  eof <- hIsEOF stdin `catch` \(_ :: SomeException) -> pure True
  if eof
    then pure ()
    else do
      line <- BS8.hGetLine stdin
      if BS8.null line
        then readLoop lock queue
        else do
          full <- atomically $ do
            f <- isFullTBQueue queue
            if f then pure True
            else do
              writeTBQueue queue line
              pure False
          if full
            then sendResponse lock $ jsonRpcError Nothing (-32000) "Server overloaded"
            else pure ()
          readLoop lock queue

processLine :: Manager -> MVar () -> String -> Maybe Text -> TVar Bool -> TVar (Maybe UUID) -> BS8.ByteString -> IO ()
processLine mgr lock serverUrl mApiKey initialized wsContext line
  | BS8.null line = pure ()
  | otherwise = case eitherDecodeStrict @Value line of
      Left _err ->
        -- Invalid JSON → -32700 Parse error (JSON-RPC 2.0 §5.1)
        sendResponse lock $ jsonRpcError Nothing (-32700) "Parse error"
      Right val -> case eitherDecodeStrict @JsonRpcRequest line of
        Left _err ->
          -- Valid JSON but missing required fields (e.g. "method") → -32600
          sendResponse lock $ jsonRpcError (extractId val) (-32600)
            "Invalid Request: missing required 'method' field"
        Right req -> do
          mresp <- handleRequest mgr serverUrl mApiKey initialized wsContext req
            `catch` \(e :: SomeException) ->
              pure $ Just $ jsonRpcError req.reqId (-32603)
                ("Internal error: " <> T.pack (show e))
          case mresp of
            Nothing   -> pure ()  -- notification: no response
            Just resp -> sendResponse lock resp

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

sendResponse :: MVar () -> Value -> IO ()
sendResponse lock v = withMVar lock $ \_ -> do
  BL8.putStrLn (encode v)
  hFlush stdout

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
      pure $ Just $ jsonRpcResponse req.reqId $ object
        [ "content" .= [ object [ "type" .= t "text", "text" .= t "Workspace context cleared." ] ] ]
    Just (Just wsText) -> case UUID.fromText wsText of
      Nothing -> pure $ Just $ jsonRpcResponse req.reqId $ object
        [ "content" .= [ object [ "type" .= t "text", "text" .= ("Invalid UUID: " <> wsText) ] ]
        , "isError" .= True
        ]
      Just uuid -> do
        atomically $ modifyTVar' wsContext (const (Just uuid))
        hPutStrLn stderr $ "MCP server: workspace context set to " <> T.unpack wsText
        pure $ Just $ jsonRpcResponse req.reqId $ object
          [ "content" .= [ object [ "type" .= t "text", "text" .= ("Workspace context set to " <> wsText) ] ] ]
  where
    t :: Text -> Text
    t = id

-- | Handle the get_workspace tool: returns the current workspace context.
handleGetWorkspace :: TVar (Maybe UUID) -> JsonRpcRequest -> IO (Maybe Value)
handleGetWorkspace wsContext req = do
  mws <- atomically $ readTVar wsContext
  let msg = case mws of
        Nothing   -> "No workspace context set."
        Just uuid -> "Current workspace: " <> UUID.toText uuid
  pure $ Just $ jsonRpcResponse req.reqId $ object
    [ "content" .= [ object [ "type" .= ("text" :: Text), "text" .= msg ] ] ]

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
        _ -> params
      _ -> params

