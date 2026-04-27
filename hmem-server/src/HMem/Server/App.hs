module HMem.Server.App
  ( mkApp
  , requestIdMiddleware
  , resolveRequestPrincipal
  , resolveRequestPrincipalIO
  , authMiddleware
  ) where

import Control.Applicative ((<|>))
import Control.Exception (SomeException, try)
import Control.Lens ((&), (.~), (^.), preview)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.Maybe (isJust)
import Data.Map.Strict qualified as Map
import Data.Aeson (FromJSON, Value(..), decode, encode, fromJSON, object, Result(..), (.:), (.=))
import Data.Aeson qualified as Aeson
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as LBS
import Data.List (find)
import Data.Pool (Pool)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time (UTCTime, NominalDiffTime, diffUTCTime, getCurrentTime)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID
import Hasql.Connection qualified as Hasql
import Network.HTTP.Client (httpLbs, newManager, parseRequest, responseBody)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.Wai (Application, Middleware)
import Network.Wai qualified as Wai
import Data.ByteString (isPrefixOf)
import Network.HTTP.Types (status200, status401, status429, hContentType, methodOptions)
import Network.Wai.Middleware.Cors
    (cors, simpleCorsResourcePolicy, corsRequestHeaders, corsMethods, corsOrigins, CorsResourcePolicy(..))
import Servant (Proxy (..), serve)

import Crypto.JWT qualified as JWT
import Crypto.JOSE.JWK qualified as JWK
import HMem.Config (AuthConfig(..), AuthMode(..), DeployedAuthConfig(..), LocalAuthConfig(..), LocalBotTokenConfig(..), CorsConfig(..), RateLimitConfig(..), TokenLookupMode(..), authStaticBearerEnabled, authStaticBearerToken)
import HMem.DB.Auth qualified as Auth
import HMem.DB.RequestContext (ActorType(..), Principal(..), PrincipalAuthority(..), RequestContext(..), currentPrincipal, withPrincipalContext, withRequestContext, emptyRequestContext)
import HMem.Server.AccessTracker (AccessTracker)
import HMem.Server.API (HMemAPI, server)
import HMem.Server.OpenAPI (openApiSpec)
import HMem.Server.Static (staticMiddleware)
import HMem.Server.WebSocket (WSState, broadcast, wsMiddleware)

-- | Build the WAI Application.
mkApp :: Middleware -> AuthConfig -> CorsConfig -> RateLimitConfig -> Pool Hasql.Connection -> AccessTracker -> WSState -> Maybe FilePath -> Bool -> IO Application
mkApp logger authCfg corsCfg rateLimitCfg pool tracker wsState mStaticDir pgvec = do
  rateLimit <- rateLimitMiddleware rateLimitCfg
  jwksCache <- newIORef Nothing
  let bc = broadcast wsState
  pure $ requestIdMiddleware
       $ logger
       $ staticMiddleware mStaticDir
       $ corsMiddleware corsCfg
       $ rateLimit
       $ wsMiddleware authCfg wsState
       $ principalContextMiddleware jwksCache authCfg pool
       $ authMiddleware authCfg
       $ openApiMiddleware
       $ serve (Proxy @HMemAPI) (server pool tracker bc wsState pgvec)

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
      reqCtx = emptyRequestContext
        { requestId = Just (TE.decodeUtf8Lenient rid)
        }
  withRequestContext reqCtx $
    app req' $ \resp -> respond $ Wai.mapResponseHeaders (("X-Request-Id", rid) :) resp

type JWKSetCache = IORef (Maybe (UTCTime, JWK.JWKSet))

principalContextMiddleware :: JWKSetCache -> AuthConfig -> Pool Hasql.Connection -> Middleware
principalContextMiddleware jwksCache authCfg pool app req respond = do
  existingPrincipal <- currentPrincipal
  resolvedPrincipal <- case existingPrincipal of
    Just principal -> pure (Just principal)
    Nothing -> resolveRequestPrincipalWithCache jwksCache pool authCfg req
  withPrincipalContext resolvedPrincipal (app req respond)

resolveRequestPrincipal :: AuthConfig -> Wai.Request -> Maybe Principal
resolveRequestPrincipal authCfg req =
  resolveBearerPrincipal authCfg req <|> defaultPrincipal authCfg
  where
    defaultPrincipal cfg
      | cfg.mode == AuthModeLocal
      , localBootstrapEnabled cfg = Just localUserPrincipal
      | otherwise = Nothing

resolveRequestPrincipalIO :: Pool Hasql.Connection -> AuthConfig -> Wai.Request -> IO (Maybe Principal)
resolveRequestPrincipalIO pool authCfg req = do
  jwksCache <- newIORef Nothing
  resolveRequestPrincipalWithCache jwksCache pool authCfg req

resolveRequestPrincipalWithCache :: JWKSetCache -> Pool Hasql.Connection -> AuthConfig -> Wai.Request -> IO (Maybe Principal)
resolveRequestPrincipalWithCache jwksCache pool authCfg req = case authCfg.mode of
  AuthModeLocal -> pure (resolveRequestPrincipal authCfg req)
  AuthModeDeployed -> do
    mBearerPrincipal <- case bearerToken req of
      Nothing -> pure Nothing
      Just token -> resolveDeployedBearerPrincipal jwksCache pool authCfg.deployed token
    pure mBearerPrincipal

resolveDeployedBearerPrincipal :: JWKSetCache -> Pool Hasql.Connection -> DeployedAuthConfig -> Text -> IO (Maybe Principal)
resolveDeployedBearerPrincipal jwksCache pool deployedCfg token = do
  mPatPrincipal <- case deployedCfg.tokenLookup of
    TokenLookupDatabase -> Auth.resolveAccessTokenPrincipal pool token
  case mPatPrincipal of
    Just resolved -> do
      withPrincipalContext (Just resolved.principal) $
        Auth.touchAccessTokenLastUsed pool resolved.tokenId
      pure (Just resolved.principal)
    Nothing        -> resolveJwtPrincipal jwksCache pool deployedCfg token

resolveJwtPrincipal :: JWKSetCache -> Pool Hasql.Connection -> DeployedAuthConfig -> Text -> IO (Maybe Principal)
resolveJwtPrincipal jwksCache pool deployedCfg token = case (deployedCfg.issuer, deployedCfg.audience) of
  (Just expectedIssuer, Just expectedAudience) -> do
    decoded <- JWT.runJOSE (JWT.decodeCompact (LBS.fromStrict $ TE.encodeUtf8 token)) :: IO (Either JWT.JWTError JWT.SignedJWT)
    case decoded of
      Left _err -> pure Nothing
      Right signedJwt -> do
        mJwkSet <- loadJWKSet jwksCache deployedCfg
        case mJwkSet of
          Nothing -> pure Nothing
          Just jwkSet -> do
            let settings = jwtValidationSettings expectedIssuer expectedAudience
            verified <- JWT.runJOSE (JWT.verifyClaims settings jwkSet signedJwt) :: IO (Either JWT.JWTError JWT.ClaimsSet)
            case verified of
              Left _err -> pure Nothing
              Right claims -> case preview JWT.string =<< (claims ^. JWT.claimSub) of
                Nothing -> pure Nothing
                Just subject -> Auth.resolveUserPrincipalByAuthSubject pool subject
  _ -> pure Nothing

jwtValidationSettings :: Text -> Text -> JWT.JWTValidationSettings
jwtValidationSettings expectedIssuer expectedAudience =
  JWT.defaultJWTValidationSettings audienceMatches
    & JWT.issuerPredicate .~ issuerMatches
  where
    audienceMatches value = stringOrUriText value == Just expectedAudience

    issuerMatches value = stringOrUriText value == Just expectedIssuer

    stringOrUriText value =
      preview JWT.string value <|> fmap (T.pack . show) (preview JWT.uri value)

loadJWKSet :: JWKSetCache -> DeployedAuthConfig -> IO (Maybe JWK.JWKSet)
loadJWKSet jwksCache deployedCfg = case deployedCfg.jwks of
  Just inline -> pure (parseJWKSet inline)
  Nothing -> do
    now <- getCurrentTime
    cached <- readIORef jwksCache
    case cached of
      Just (loadedAt, jwkSet)
        | diffUTCTime now loadedAt < jwksCacheTtl -> pure (Just jwkSet)
      _ -> do
        mBody <- loadJWKSetBody deployedCfg
        let mJwkSet = mBody >>= decode
        maybe (pure ()) (\jwkSet -> writeIORef jwksCache (Just (now, jwkSet))) mJwkSet
        pure mJwkSet

jwksCacheTtl :: NominalDiffTime
jwksCacheTtl = 300

parseJWKSet :: Value -> Maybe JWK.JWKSet
parseJWKSet value = case fromJSON value of
  Success jwkSet -> Just jwkSet
  Error _ -> Nothing

loadJWKSetBody :: DeployedAuthConfig -> IO (Maybe LBS.ByteString)
loadJWKSetBody deployedCfg = case deployedCfg.jwksUrl of
  Just url -> fetchJson url
  Nothing -> case deployedCfg.discoveryUrl of
    Nothing -> pure Nothing
    Just discovery -> do
      mDiscoveryBody <- fetchJson discovery
      case mDiscoveryBody >>= decode of
        Just (OidcDiscovery discoveredIssuer jwksUri)
          | Just discoveredIssuer == deployedCfg.issuer -> fetchJson jwksUri
        _ -> pure Nothing

data OidcDiscovery = OidcDiscovery Text Text

instance FromJSON OidcDiscovery where
  parseJSON = Aeson.withObject "OidcDiscovery" $ \o -> OidcDiscovery
    <$> o .: "issuer"
    <*> o .: "jwks_uri"

fetchJson :: Text -> IO (Maybe LBS.ByteString)
fetchJson url
  | not ("https://" `T.isPrefixOf` url) = pure Nothing
  | otherwise = do
      result <- try $ do
        parsed <- parseRequest (T.unpack url)
        manager <- newManager tlsManagerSettings
        responseBody <$> httpLbs parsed manager
      case result of
        Left (_ :: SomeException) -> pure Nothing
        Right body -> pure (Just body)

localBootstrapEnabled :: AuthConfig -> Bool
localBootstrapEnabled cfg = case cfg of
  AuthConfig { local = LocalAuthConfig { bootstrapEnabled = enabled } } -> enabled

localUserPrincipal :: Principal
localUserPrincipal = Principal
  { actorType = ActorUser
  , actorId = "local-user"
  , actorLabel = "Local User"
  , authority = PrincipalSyntheticLocalSuperadmin
  }

resolveBearerPrincipal :: AuthConfig -> Wai.Request -> Maybe Principal
resolveBearerPrincipal authCfg req = do
  token <- bearerToken req
  localBotPrincipal authCfg token <|> legacyPrincipal authCfg token

localBotPrincipal :: AuthConfig -> Text -> Maybe Principal
localBotPrincipal authCfg token
  | authCfg.mode /= AuthModeLocal = Nothing
  | otherwise = do
  let configuredBotTokens = case authCfg of
        AuthConfig { local = LocalAuthConfig { botTokens = tokens } } -> tokens
  cfg <- find matchesToken configuredBotTokens
  -- Transitional local compatibility: configured local bot tokens keep the
  -- same broad local authority as the implicit local user while preserving
  -- bot attribution. Deployed bot/PAT resolution should use PrincipalGrantUser
  -- so the DB user/membership grant model remains authoritative.
  pure Principal
    { actorType = ActorBot
    , actorId = "local-bot:" <> botLabel cfg
    , actorLabel = botLabel cfg
    , authority = PrincipalSyntheticLocalSuperadmin
    }
  where
    matchesToken bot = case bot of
      LocalBotTokenConfig { token = botToken } -> botToken == token

    botLabel bot = case bot of
      LocalBotTokenConfig { label = lbl } -> lbl

localBotTokenAuthorized :: AuthConfig -> Text -> Bool
localBotTokenAuthorized authCfg token = isJust (localBotPrincipal authCfg token)

legacyPrincipal :: AuthConfig -> Text -> Maybe Principal
legacyPrincipal authCfg token = do
  expected <- authStaticBearerToken authCfg
  if token == expected
    -- Transitional local compatibility for the legacy static-bearer path.
    -- This path is config-gated to AuthModeLocal by authStaticBearerToken.
    then Just Principal
      { actorType = ActorBot
      , actorId = "legacy-static-bearer"
      , actorLabel = "Legacy Static Bearer"
      , authority = PrincipalSyntheticLocalSuperadmin
      }
    else Nothing

bearerToken :: Wai.Request -> Maybe Text
bearerToken request = do
  raw <- lookup "Authorization" (Wai.requestHeaders request)
  BS8.stripPrefix "Bearer " raw >>= Just . TE.decodeUtf8

-- | Current legacy static bearer auth path. When inactive, requests pass
-- through. When active, all non-OPTIONS requests must supply a matching
-- Authorization header of the form @Bearer <token>@.
authMiddleware :: AuthConfig -> Middleware
authMiddleware authCfg app req respond
  | not (authStaticBearerEnabled authCfg) = app req respond
  | Wai.requestMethod req == methodOptions = app req respond
  | authorized = app req respond
  | otherwise = respond $ Wai.responseLBS status401
      [ (hContentType, "application/json")
      , ("WWW-Authenticate", "Bearer")
      ]
      (encode unauthorizedBody)
  where
    unauthorizedBody = object
      [ "error" .= ("unauthorized" :: Text)
      , "message" .= ("Missing or invalid bearer token" :: Text)
      ]

    authorizedToken token = localBotTokenAuthorized authCfg token || maybe False (== token) (authStaticBearerToken authCfg)

    authorized = case bearerToken req of
      Just actual -> authorizedToken actual
      Nothing     -> False

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
