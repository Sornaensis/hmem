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
  ) where

import Data.Aeson qualified as Aeson
import Data.Aeson ((.=))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time (getCurrentTime, formatTime, defaultTimeLocale)
import Network.Wai (Middleware, rawPathInfo, requestMethod)
import Network.Wai qualified as Wai
import Network.Wai.Middleware.RequestLogger
    ( mkRequestLogger, defaultRequestLoggerSettings
    , outputFormat, destination
    , OutputFormat(..), Destination(Callback), OutputFormatterWithDetails
    )
import Network.HTTP.Types (statusCode)
import System.Log.FastLogger (toLogStr, FastLogger)

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
      now <- getCurrentTime
      let ts = T.pack $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" now
          line = Aeson.object
            [ "timestamp" .= ts
            , "level"     .= levelText lvl
            , "message"   .= msg
            ]
      lg.lgAction (toLogStr (Aeson.encode line) <> "\n")

------------------------------------------------------------------------
-- JSON request logger middleware
------------------------------------------------------------------------

-- | WAI request logger that emits one JSON line per request.
jsonRequestLogger :: FastLogger -> IO Middleware
jsonRequestLogger logAction = mkRequestLogger defaultRequestLoggerSettings
  { outputFormat = CustomOutputFormatWithDetails jsonFormatter
  , destination  = Callback logAction
  }

-- | OutputFormatterWithDetails:
--   ZonedDate -> Request -> Status -> Maybe Integer -> NominalDiffTime
--   -> [ByteString] -> Builder -> LogStr
jsonFormatter :: OutputFormatterWithDetails
jsonFormatter zonedDate req status mSize dur _reqBody _respBody = toLogStr jsonLine <> "\n"
  where
    mRequestId = TE.decodeUtf8 <$> lookup "X-Request-Id" (Wai.requestHeaders req)
    jsonLine = Aeson.encode $ Aeson.object $
      [ "timestamp"   .= TE.decodeUtf8 zonedDate
      , "level"       .= ("info" :: Text)
      , "method"      .= TE.decodeUtf8 (requestMethod req)
      , "path"        .= TE.decodeUtf8 (rawPathInfo req)
      , "status"      .= statusCode status
      , "duration_ms" .= (fromRational (toRational dur * 1000) :: Double)
      , "size"        .= mSize
      ] ++ [ "request_id" .= rid | Just rid <- [mRequestId] ]
