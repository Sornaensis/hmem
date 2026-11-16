{-# LANGUAGE BangPatterns #-}

module HMem.Server.Logging
  ( Logger
  , LogLevel(..)
  , newLogger
  , logDebug
  , logInfo
  , logWarn
  , logError
  , parseLogLevel
  , jsonRequestLogger
  , jsonRequestLoggerWith
  , requestRouteLabel
  ) where

import Control.Exception (IOException, SomeAsyncException, SomeException, evaluate, fromException, mask, throwIO, try)
import Control.Lens ((^.))
import Control.Monad (when)
import Data.Aeson qualified as Aeson
import Data.Aeson ((.=))
import Data.ByteString qualified as BS
import Data.HashMap.Strict.InsOrd qualified as InsOrdMap
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.List (dropWhileEnd)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.OpenApi (paths)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time (getCurrentTime, formatTime, defaultTimeLocale)
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import Network.HTTP.Types (statusCode)
import Network.Wai (Middleware)
import Network.Wai qualified as Wai
import Network.Wai.Internal (Response(..))
import System.Log.FastLogger (toLogStr, FastLogger)

import HMem.DB.RequestTiming (RequestTiming(..), readRequestTimer, withRequestTimer)
import HMem.Server.OpenAPI (openApiSpec)

------------------------------------------------------------------------
-- Log level
------------------------------------------------------------------------

data LogLevel = Debug | Info | Warn | Error
  deriving stock (Show, Eq, Ord)

parseLogLevel :: Text -> LogLevel
parseLogLevel t = case T.toLower t of
  "debug" -> Debug
  "warn"  -> Warn
  "error" -> Error
  _       -> Info

levelText :: LogLevel -> Text
levelText Debug = "debug"
levelText Info  = "info"
levelText Warn  = "warn"
levelText Error = "error"

------------------------------------------------------------------------
-- Logger
------------------------------------------------------------------------

data Logger = Logger
  { lgAction   :: !FastLogger
  , lgMinLevel :: !LogLevel
  }

newLogger :: FastLogger -> LogLevel -> Logger
newLogger = Logger

logDebug, logInfo, logWarn, logError :: Logger -> Text -> IO ()
logDebug lg = logMsg lg Debug
logInfo  lg = logMsg lg Info
logWarn  lg = logMsg lg Warn
logError lg = logMsg lg Error

logMsg :: Logger -> LogLevel -> Text -> IO ()
logMsg lg lvl msg
  | lvl < lg.lgMinLevel = pure ()
  | otherwise = do
      ts <- isoTimestamp
      let line = Aeson.object
            [ "timestamp" .= ts
            , "level"     .= levelText lvl
            , "message"   .= msg
            ]
      lg.lgAction (toLogStr (Aeson.encode line) <> "\n")

isoTimestamp :: IO Text
isoTimestamp = T.pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" <$> getCurrentTime

------------------------------------------------------------------------
-- Metadata-only JSON request logger
------------------------------------------------------------------------

-- | Emit one JSON line per request without reading request bodies or
-- retaining response bodies. See docs/reference/request-logging.md.
jsonRequestLogger :: FastLogger -> IO Middleware
jsonRequestLogger logAction = do
  -- Build the route table now so the first request does not pay for it.
  _ <- evaluate (routeWeight routeTable)
  pure (jsonRequestLoggerWith getMonotonicTimeNSec logAction)

data ResponseShape = Buffered | Streamed | FileBody | Upgrade

data Outcome
  = Completed
  | UpgradeClosed
  | UpgradeFallback
  | ApplicationFailed
  | ResponseFailed
  | StreamFailed
  | TransmissionFailed
  | UpgradeFailed
  | Interrupted
  | NoResponse

outcomeLabel :: Outcome -> Text
outcomeLabel = \case
  Completed -> "completed"
  UpgradeClosed -> "upgrade_closed"
  UpgradeFallback -> "upgrade_fallback"
  ApplicationFailed -> "application_failed"
  ResponseFailed -> "response_failed"
  StreamFailed -> "stream_failed"
  TransmissionFailed -> "transmission_failed"
  UpgradeFailed -> "upgrade_failed"
  Interrupted -> "interrupted"
  NoResponse -> "no_response"

outcomeSucceeded :: Outcome -> Bool
outcomeSucceeded = \case
  Completed -> True
  UpgradeClosed -> True
  UpgradeFallback -> True
  _ -> False

-- | The request logger with an injectable monotonic nanosecond clock.
--
-- Admission is entry into this middleware. @prepare_ms@ ends when the
-- downstream application hands a response to @respond@. @complete_ms@ ends
-- when that @respond@ call returns or throws (the transport has written the
-- response, finished the stream, or closed the raw upgrade), or when the
-- application fails without responding. Database pool and provider phases
-- accumulate on this thread's request timer until emission.
jsonRequestLoggerWith :: IO Word64 -> FastLogger -> Middleware
jsonRequestLoggerWith clock logAction app req respond = withRequestTimer $ \timer -> do
  admitted <- clock
  let !method = methodLabel (Wai.requestMethod req)
      !route = requestRouteLabel (Wai.pathInfo req)
      !requestId = boundedRequestId (lookup "X-Request-Id" (Wai.requestHeaders req))
  emittedRef <- newIORef False
  preparedRef <- newIORef Nothing
  let emit prepared finished status shape outcome = do
        first <- atomicModifyIORef' emittedRef (\done -> (True, not done))
        when first $ do
          ts <- isoTimestamp
          phases <- readRequestTimer timer
          let level = if outcomeSucceeded outcome then "info" else "warn" :: Text
              line = Aeson.object $
                [ "timestamp"   .= ts
                , "level"       .= level
                , "method"      .= method
                , "route"       .= route
                , "status"      .= status
                , "response"    .= fmap shapeLabel shape
                , "outcome"     .= outcomeLabel outcome
                , "prepare_ms"  .= fmap (elapsedMs admitted) prepared
                , "complete_ms" .= elapsedMs admitted finished
                , "db_checkouts" .= phases.dbCheckouts
                , "db_wait_ms" .= nsToMs phases.dbWaitNs
                , "db_connect_ms" .= nsToMs phases.dbConnectNs
                , "db_hold_ms" .= nsToMs phases.dbHoldNs
                , "provider_ms" .= (if phases.providerCalls == 0 then Nothing else Just (nsToMs phases.providerNs))
                ] ++ [ "request_id" .= rid | Just rid <- [requestId] ]
          logAction (toLogStr (Aeson.encode line) <> "\n")
      loggedRespond response = mask $ \restore -> do
        prepared <- clock
        writeIORef preparedRef (Just prepared)
        sendFailed <- newIORef False
        producerFailed <- newIORef False
        rawStarted <- newIORef False
        let (shape, instrumented) = instrument sendFailed producerFailed rawStarted response
            status = statusCode (Wai.responseStatus response)
        result <- try (restore (respond instrumented))
        finished <- clock
        started <- readIORef rawStarted
        transmission <- readIORef sendFailed
        case result of
          Right received -> do
            case shape of
              Upgrade
                | started -> emit (Just prepared) finished Nothing (Just Upgrade) UpgradeClosed
                | otherwise -> emit (Just prepared) finished (Just status) (Just Upgrade) UpgradeFallback
              -- A producer that swallowed a failed write still lost data.
              Streamed | transmission -> emit (Just prepared) finished (Just status) (Just shape) TransmissionFailed
              _ -> emit (Just prepared) finished (Just status) (Just shape) Completed
            pure received
          Left failure -> do
            producer <- readIORef producerFailed
            let outcome
                  | isAsync failure = Interrupted
                  | otherwise = case shape of
                      Upgrade -> UpgradeFailed
                      Streamed
                        | producer -> StreamFailed
                        | transmission -> TransmissionFailed
                      -- Anything else failed outside the producer: the header,
                      -- chunked terminator or final buffered write of a stream,
                      -- or a buffered or file response.
                      _
                        | isJust (fromException failure :: Maybe IOException) -> TransmissionFailed
                        | otherwise -> ResponseFailed
                -- The fallback status is reported only when it was sent.
                reportedStatus = case shape of
                  Upgrade -> Nothing
                  _ -> Just status
            emit (Just prepared) finished reportedStatus (Just shape) outcome
            throwIO failure
  -- Post-processing runs masked, as in bracket, so a late asynchronous
  -- exception cannot replace the original failure before it is rethrown.
  mask $ \restore -> do
    result <- try (restore (app req loggedRespond))
    finished <- clock
    prepared <- readIORef preparedRef
    case result of
      Right received -> do
        emit prepared finished Nothing Nothing NoResponse
        pure received
      Left failure -> do
        emit prepared finished Nothing Nothing (if isAsync failure then Interrupted else ApplicationFailed)
        throwIO failure

isAsync :: SomeException -> Bool
isAsync failure = isJust (fromException failure :: Maybe SomeAsyncException)

-- Forward every streamed chunk directly to the transport. Only flags are
-- recorded so a failed write can be distinguished from a failed producer.
-- The transport also writes outside the producer (Warp sends the header
-- before it and the chunked terminator and final buffer after it), so a
-- stream failure is the producer's only when it escapes the producer
-- without a failed write.
instrument :: IORef Bool -> IORef Bool -> IORef Bool -> Wai.Response -> (ResponseShape, Wai.Response)
instrument sendFailed producerFailed rawStarted = \case
  response@ResponseBuilder{} -> (Buffered, response)
  response@ResponseFile{} -> (FileBody, response)
  ResponseStream status headers body ->
    (Streamed, ResponseStream status headers $ \send flush -> do
      result <- try (body (guarded . send) (guarded flush))
      case result of
        Right () -> pure ()
        Left (failure :: SomeException) -> do
          transmission <- readIORef sendFailed
          when (not transmission) (writeIORef producerFailed True)
          throwIO failure)
  ResponseRaw raw fallback ->
    (Upgrade, ResponseRaw (\source sink -> writeIORef rawStarted True >> raw source sink) fallback)
  where
    guarded action = do
      result <- try action
      case result of
        Right () -> pure ()
        Left (failure :: SomeException) -> do
          writeIORef sendFailed True
          throwIO failure

shapeLabel :: ResponseShape -> Text
shapeLabel = \case
  Buffered -> "buffered"
  Streamed -> "stream"
  FileBody -> "file"
  Upgrade -> "upgrade"

elapsedMs :: Word64 -> Word64 -> Double
elapsedMs start end
  | end <= start = 0
  | otherwise = nsToMs (end - start)

nsToMs :: Word64 -> Double
nsToMs ns = fromIntegral ns / 1e6

methodLabel :: BS.ByteString -> Text
methodLabel = \case
  "GET" -> "GET"
  "HEAD" -> "HEAD"
  "POST" -> "POST"
  "PUT" -> "PUT"
  "PATCH" -> "PATCH"
  "DELETE" -> "DELETE"
  "OPTIONS" -> "OPTIONS"
  "CONNECT" -> "CONNECT"
  "TRACE" -> "TRACE"
  _ -> "OTHER"

-- The request ID is echoed from clients when present; only log a short
-- visible ASCII token (no spaces) so the log cannot carry arbitrary client
-- payloads.
boundedRequestId :: Maybe BS.ByteString -> Maybe Text
boundedRequestId = \case
  Just rid
    | not (BS.null rid) && BS.length rid <= 128 && BS.all (\b -> b > 0x20 && b < 0x7f) rid ->
        Just (TE.decodeLatin1 rid)
  _ -> Nothing

------------------------------------------------------------------------
-- Fixed route labels
------------------------------------------------------------------------

data RouteNode = RouteNode
  { literal :: !(Map.Map Text RouteNode)
  , parameter :: !(Maybe RouteNode)
  , label :: !(Maybe Text)
  }

emptyRoute :: RouteNode
emptyRoute = RouteNode Map.empty Nothing Nothing

-- | Routes not described by the OpenAPI document but served by middleware.
extraRouteTemplates :: [Text]
extraRouteTemplates =
  [ "/api/v1/ws"
  , "/api/v1/openapi.json"
  , "/api/v1/auth/login"
  , "/api/v1/auth/callback"
  , "/api/v1/auth/logout"
  ]

routeTable :: RouteNode
routeTable = foldl' insertTemplate emptyRoute
  (extraRouteTemplates ++ map T.pack (InsOrdMap.keys (openApiSpec ^. paths)))
{-# NOINLINE routeTable #-}

-- Traverses every node and label, fully forcing the table.
routeWeight :: RouteNode -> Int
routeWeight node =
  1 + maybe 0 T.length node.label
    + sum (map routeWeight (Map.elems node.literal))
    + maybe 0 routeWeight node.parameter

insertTemplate :: RouteNode -> Text -> RouteNode
insertTemplate root template = go root (filter (not . T.null) (T.splitOn "/" template))
  where
    go node = \case
      [] -> node { label = Just (maybe template id node.label) }
      segment : rest
        | "{" `T.isPrefixOf` segment && "}" `T.isSuffixOf` segment ->
            node { parameter = Just (go (maybe emptyRoute id node.parameter) rest) }
        | otherwise ->
            node { literal = Map.alter (Just . flip go rest . maybe emptyRoute id) segment node.literal }

-- | Map request path segments to a fixed label: a documented route template,
-- @unmatched@ for other API paths, or @static@. Raw identifiers, tokens and
-- query strings never appear in the label.
requestRouteLabel :: [Text] -> Text
requestRouteLabel segments = case match routeTable trimmed of
  Just template -> template
  Nothing -> case trimmed of
    "api" : _ -> "unmatched"
    "document-grants" : _ -> "unmatched"
    _ -> "static"
  where
    trimmed = dropWhileEnd T.null segments
    match node = \case
      [] -> node.label
      segment : rest ->
        let viaLiteral = Map.lookup segment node.literal >>= \next -> match next rest
            viaParameter
              | T.null segment = Nothing
              | otherwise = node.parameter >>= \next -> match next rest
        in case viaLiteral of
             Just found -> Just found
             Nothing -> viaParameter
