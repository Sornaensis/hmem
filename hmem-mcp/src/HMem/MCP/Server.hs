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
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Network.HTTP.Client (Manager, newManager, managerResponseTimeout, responseTimeoutMicro)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.IO (hFlush, hSetBuffering, stdin, stdout, stderr, hPutStrLn, BufferMode (..), hIsEOF)
import Text.Read (readMaybe)

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

-- | Page size for tools/list pagination.
toolsPageSize :: Int
toolsPageSize = 15

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
  -- Spawn fixed worker pool
  replicateM_ maxConcurrency $ forkIO $ worker mgr lock serverUrl mApiKey queue active initialized
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
worker :: Manager -> MVar () -> String -> Maybe Text -> TBQueue BS8.ByteString -> TVar Int -> TVar Bool -> IO ()
worker mgr lock url mApiKey queue active initialized = go
  where
    go = do
      mline <- try @SomeException $ atomically $ do
        l <- readTBQueue queue
        modifyTVar' active (+ 1)
        pure l
      case mline of
        Left _ -> pure ()  -- Worker exits cleanly
        Right line -> do
          (processLine mgr lock url mApiKey initialized line `catch` \(_ :: SomeException) -> pure ())
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

processLine :: Manager -> MVar () -> String -> Maybe Text -> TVar Bool -> BS8.ByteString -> IO ()
processLine mgr lock serverUrl mApiKey initialized line
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
          mresp <- handleRequest mgr serverUrl mApiKey initialized req
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
handleRequest :: Manager -> String -> Maybe Text -> TVar Bool -> JsonRpcRequest -> IO (Maybe Value)
handleRequest mgr serverUrl mApiKey initialized req = case req.reqMethod of
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
      else handleMethod mgr serverUrl mApiKey req

-- | Dispatch initialized requests to the appropriate handler.
handleMethod :: Manager -> String -> Maybe Text -> JsonRpcRequest -> IO (Maybe Value)
handleMethod mgr serverUrl mApiKey req = case req.reqMethod of
  "tools/list" -> do
    let allTools = toolDefinitions
        cursorIdx = parseCursor req.reqParams
        page = take toolsPageSize (drop cursorIdx allTools)
        nextIdx = cursorIdx + toolsPageSize
    pure $ Just $ jsonRpcResponse req.reqId $ object $
      [ "tools" .= page ] ++
      [ "nextCursor" .= show nextIdx | nextIdx < length allTools ]

  "tools/call" -> do
    case req.reqParams of
      Nothing -> pure $ Just $ jsonRpcError req.reqId (-32602)
          "Invalid params: tools/call requires 'name' and 'arguments'"
      Just params -> do
        result <- handleToolCall mgr serverUrl mApiKey params
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

-- | Parse the cursor parameter from tools/list params.
-- Accepts both string and numeric cursors; defaults to 0.
parseCursor :: Maybe Value -> Int
parseCursor (Just (Object o)) = case KM.lookup "cursor" o of
  Just (String s) -> fromMaybe 0 (readMaybe (T.unpack s))
  Just (Number n) -> round n
  _               -> 0
parseCursor _ = 0
