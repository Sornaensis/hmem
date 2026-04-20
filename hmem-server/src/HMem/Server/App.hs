module HMem.Server.App
  ( mkApp
  , requestIdMiddleware
  , authMiddleware
  ) where

import Control.Applicative ((<|>))
import Data.IORef (atomicModifyIORef', newIORef)
import Data.Map.Strict qualified as Map
import Data.Aeson (encode, object, (.=))
import Data.ByteString.Char8 qualified as BS8
import Data.Pool (Pool)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time (UTCTime, NominalDiffTime, diffUTCTime, getCurrentTime)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID
import Hasql.Connection qualified as Hasql
import Network.Wai (Application, Middleware)
import Network.Wai qualified as Wai
import Data.ByteString (isPrefixOf)
import Network.HTTP.Types (status200, status401, status429, hContentType, methodOptions)
import Network.Wai.Middleware.Cors
    (cors, simpleCorsResourcePolicy, corsRequestHeaders, corsMethods, corsOrigins, CorsResourcePolicy(..))
import Servant (Proxy (..), serve)

import HMem.Config (AuthConfig(..), CorsConfig(..), RateLimitConfig(..))
import HMem.DB.RequestContext (withRequestIdContext)
import HMem.Server.AccessTracker (AccessTracker)
import HMem.Server.API (HMemAPI, server)
import HMem.Server.OpenAPI (openApiSpec)
import HMem.Server.Static (staticMiddleware)
import HMem.Server.WebSocket (WSState, broadcast, wsMiddleware)

-- | Build the WAI Application.
mkApp :: Middleware -> AuthConfig -> CorsConfig -> RateLimitConfig -> Pool Hasql.Connection -> AccessTracker -> WSState -> Maybe FilePath -> Bool -> IO Application
mkApp logger authCfg corsCfg rateLimitCfg pool tracker wsState mStaticDir pgvec = do
  rateLimit <- rateLimitMiddleware rateLimitCfg
  let bc = broadcast wsState
  pure $ requestIdMiddleware
       $ logger
       $ wsMiddleware authCfg wsState
       $ staticMiddleware mStaticDir
       $ corsMiddleware corsCfg
       $ rateLimit
       $ authMiddleware authCfg
       $ openApiMiddleware
       $ serve (Proxy @HMemAPI) (server pool tracker bc pgvec)

-- | Middleware that assigns a unique X-Request-Id to every request.
-- If the incoming request already has an X-Request-Id header, it is
-- preserved; otherwise a new UUID v4 is generated.  The chosen id is
-- echoed back in the response headers so callers can correlate logs.
requestIdMiddleware :: Middleware
requestIdMiddleware app req respond = do
  rid <- case lookup "X-Request-Id" (Wai.requestHeaders req) <|> lookup "X-Request-ID" (Wai.requestHeaders req) of
    Just existing -> pure existing
    Nothing       -> TE.encodeUtf8 . UUID.toText <$> UUID.nextRandom
  let req' = req { Wai.requestHeaders = ("X-Request-Id", rid) : Wai.requestHeaders req }
  withRequestIdContext (Just (TE.decodeUtf8Lenient rid)) $
    app req' $ \resp -> respond $ Wai.mapResponseHeaders (("X-Request-Id", rid) :) resp

-- | Optional Bearer-token auth. When disabled, requests pass through.
-- When enabled, all non-OPTIONS requests must supply a matching
-- Authorization header of the form @Bearer <token>@.
authMiddleware :: AuthConfig -> Middleware
authMiddleware authCfg app req respond
  | not authCfg.enabled = app req respond
  | Wai.requestMethod req == methodOptions = app req respond
  | authorized = app req respond
  | otherwise = respond $ Wai.responseLBS status401
      [ (hContentType, "application/json")
      , ("WWW-Authenticate", "Bearer")
      ]
      (encode unauthorizedBody)
  where
    authorized = case (authCfg.apiKey, bearerToken req) of
      (Just expected, Just actual) -> actual == expected
      _                            -> False

    unauthorizedBody = object
      [ "error" .= ("unauthorized" :: Text)
      , "message" .= ("Missing or invalid bearer token" :: Text)
      ]

    bearerToken request = do
      raw <- lookup "Authorization" (Wai.requestHeaders request)
      BS8.stripPrefix "Bearer " raw >>= Just . TE.decodeUtf8

-- | Serve /api/v1/openapi.json directly at the WAI level to avoid
-- a circular module dependency with the Servant API type.
openApiMiddleware :: Middleware
openApiMiddleware app req respond
  | Wai.pathInfo req == ["api", "v1", "openapi.json"] =
      respond $ Wai.responseLBS status200
        [(hContentType, "application/json")]
        (encode openApiSpec)
  | otherwise = app req respond

corsMiddleware :: CorsConfig -> Application -> Application
corsMiddleware corsCfg = cors $ \req ->
    let mOrigin = lookup "Origin" (Wai.requestHeaders req)
        allowed = case mOrigin of
          Just o  -> any (matchOrigin o) corsCfg.allowedOrigins
          Nothing -> False
    in if allowed
       then Just policy { corsOrigins = fmap (\o -> ([o], True)) mOrigin }
       else Nothing
  where
    matchOrigin o pattern
      -- Exact match
      | o == TE.encodeUtf8 pattern = True
      -- Pattern with port wildcard: "http://localhost" matches "http://localhost:8080"
      | (TE.encodeUtf8 pattern <> ":") `isPrefixOf` o = True
      -- Wildcard: allow all origins
      | pattern == "*" = True
      | otherwise = False
    policy = simpleCorsResourcePolicy
      { corsMethods        = ["GET", "POST", "PUT", "DELETE", "OPTIONS"]
      , corsRequestHeaders = ["Content-Type", "Authorization", "X-Request-Id", "X-Request-ID"]
      , corsExposedHeaders = Just ["X-Request-Id"]
      }

data RateLimitBucket = RateLimitBucket
  { tokens    :: !Double
  , updatedAt :: !UTCTime
  }

rateLimitMiddleware :: RateLimitConfig -> IO Middleware
rateLimitMiddleware rateCfg
  | not rateCfg.rlEnabled = pure id
  | otherwise = do
      buckets <- newIORef Map.empty
      pure $ \app req respond ->
        if Wai.requestMethod req == methodOptions
          then app req respond
          else do
            now <- getCurrentTime
            allowed <- atomicModifyIORef' buckets $ \existing ->
              let pruned = Map.filter (isRecentBucket now rateCfg) existing
                  key = clientKey req
                  (bucket, isAllowed) = consumeBucket rateCfg now (Map.lookup key pruned)
              in (Map.insert key bucket pruned, isAllowed)
            if allowed
              then app req respond
              else respond $ Wai.responseLBS status429
                     [ (hContentType, "application/json")
                     , ("Retry-After", "1")
                     ]
                     (encode tooManyRequestsBody)
  where
    tooManyRequestsBody = object
      [ "error" .= ("too_many_requests" :: Text)
      , "message" .= ("Rate limit exceeded" :: Text)
      ]

clientKey :: Wai.Request -> Text
clientKey req =
  case lookup "X-Forwarded-For" (Wai.requestHeaders req) of
    Just raw -> T.strip . TE.decodeUtf8Lenient . BS8.takeWhile (/= ',') $ raw
    Nothing  -> T.pack (show (Wai.remoteHost req))

consumeBucket :: RateLimitConfig -> UTCTime -> Maybe RateLimitBucket -> (RateLimitBucket, Bool)
consumeBucket rateCfg now mBucket =
  if available >= 1.0
    then (RateLimitBucket (available - 1.0) now, True)
    else (RateLimitBucket available now, False)
  where
    capacity = fromIntegral rateCfg.rlBurst
    elapsedSeconds = maybe 0.0 (\bucket -> realToFrac (diffUTCTime now bucket.updatedAt)) mBucket
    currentTokens = maybe capacity (\bucket -> bucket.tokens) mBucket
    available = min capacity (currentTokens + elapsedSeconds * rateCfg.rlRequestsPerSecond)

isRecentBucket :: UTCTime -> RateLimitConfig -> RateLimitBucket -> Bool
isRecentBucket now rateCfg bucket =
  diffUTCTime now bucket.updatedAt <= staleWindow rateCfg

staleWindow :: RateLimitConfig -> NominalDiffTime
staleWindow rateCfg = realToFrac (max (60 :: Double) ((fromIntegral rateCfg.rlBurst / rateCfg.rlRequestsPerSecond) * 10.0))
