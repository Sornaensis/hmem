module HMem.Server.App
  ( mkApp
  , requestIdMiddleware
  ) where

import Data.Aeson (encode)
import Data.Pool (Pool)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID
import Hasql.Connection qualified as Hasql
import Network.Wai (Application, Middleware)
import Network.Wai qualified as Wai
import Data.ByteString (isPrefixOf)
import Data.Text.Encoding qualified as TE
import Network.HTTP.Types (status200, hContentType)
import Network.Wai.Middleware.Cors
    (cors, simpleCorsResourcePolicy, corsRequestHeaders, corsMethods, corsOrigins, CorsResourcePolicy(..))
import Servant (Proxy (..), serve)

import HMem.Config (CorsConfig(..))
import HMem.Server.AccessTracker (AccessTracker)
import HMem.Server.API (HMemAPI, server)
import HMem.Server.OpenAPI (openApiSpec)

-- | Build the WAI Application.
mkApp :: Middleware -> CorsConfig -> Pool Hasql.Connection -> AccessTracker -> Bool -> Application
mkApp logger corsCfg pool tracker pgvec =
  requestIdMiddleware $ logger $ corsMiddleware corsCfg $ openApiMiddleware $ serve (Proxy @HMemAPI) (server pool tracker pgvec)

-- | Middleware that assigns a unique X-Request-Id to every request.
-- If the incoming request already has an X-Request-Id header, it is
-- preserved; otherwise a new UUID v4 is generated.  The chosen id is
-- echoed back in the response headers so callers can correlate logs.
requestIdMiddleware :: Middleware
requestIdMiddleware app req respond = do
  rid <- case lookup "X-Request-Id" (Wai.requestHeaders req) of
    Just existing -> pure existing
    Nothing       -> TE.encodeUtf8 . UUID.toText <$> UUID.nextRandom
  let req' = req { Wai.requestHeaders = ("X-Request-Id", rid) : Wai.requestHeaders req }
  app req' $ \resp -> respond $ Wai.mapResponseHeaders (("X-Request-Id", rid) :) resp

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
      , corsRequestHeaders = ["Content-Type", "Authorization"]
      }
