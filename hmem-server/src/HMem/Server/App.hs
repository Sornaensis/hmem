module HMem.Server.App
  ( mkApp
  , mkAppWithOidcCodeExchange -- ^ Test-only seam; production callers should use 'mkApp'.
  , OidcCodeExchange -- ^ Test-only injected OIDC code exchange type.
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
import Data.ByteString (ByteString, isPrefixOf)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as LBS
import Data.List (find)
import Data.Pool (Pool)
import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time (UTCTime, NominalDiffTime, addUTCTime, diffUTCTime, getCurrentTime)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID
import Hasql.Connection qualified as Hasql
import Network.HTTP.Client (httpLbs, method, newManager, parseRequest, requestHeaders, responseBody, responseStatus, urlEncodedBody)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.Wai (Application, Middleware)
import Network.Wai qualified as Wai
import Network.HTTP.Types (HeaderName, Status, status200, status302, status401, status403, status404, status429, status503, hContentType, hLocation, methodGet, methodHead, methodOptions, methodPost, statusCode)
import Network.HTTP.Types.URI (renderQuery, urlDecode, urlEncode)
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
mkApp = mkAppWithOidcCodeExchange defaultOidcCodeExchange

type OidcCodeExchange = DeployedAuthConfig -> Text -> IO (Maybe Text)

-- | Build the WAI Application with an injected OIDC code exchange.
--
-- This constructor exists for deterministic tests of the browser OIDC callback
-- flow. Production entry points should call 'mkApp', which always uses
-- 'defaultOidcCodeExchange' and the configured HTTPS provider token endpoint.
mkAppWithOidcCodeExchange :: OidcCodeExchange -> Middleware -> AuthConfig -> CorsConfig -> RateLimitConfig -> Pool Hasql.Connection -> AccessTracker -> WSState -> Maybe FilePath -> Bool -> IO Application
mkAppWithOidcCodeExchange oidcCodeExchange logger authCfg corsCfg rateLimitCfg pool tracker wsState mStaticDir pgvec = do
  rateLimit <- rateLimitMiddleware rateLimitCfg
  jwksCache <- newIORef Nothing
  let bc = broadcast wsState
  pure $ requestIdMiddleware
       $ logger
       $ staticMiddleware mStaticDir
        $ corsMiddleware authCfg corsCfg
        $ rateLimit
        $ wsMiddleware authCfg wsState
        $ oidcAuthRoutesMiddleware oidcCodeExchange jwksCache authCfg pool
        $ csrfMiddleware authCfg pool
        $ principalContextMiddleware jwksCache authCfg pool
        $ authMiddleware authCfg
        $ openApiMiddleware
       $ serve (Proxy @HMemAPI) (server authCfg pool tracker bc wsState pgvec)

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
  AuthModeDeployed -> case bearerToken req of
    Just token -> resolveDeployedBearerPrincipal jwksCache pool authCfg.deployed token
    Nothing -> resolveCookieSessionPrincipal pool authCfg.deployed req

resolveCookieSessionPrincipal :: Pool Hasql.Connection -> DeployedAuthConfig -> Wai.Request -> IO (Maybe Principal)
resolveCookieSessionPrincipal pool deployedCfg req = case lookupCookie deployedCfg.sessionCookieName req of
  Nothing -> pure Nothing
  Just sessionToken -> Auth.resolveAuthSessionPrincipal pool sessionToken

resolveDeployedBearerPrincipal :: JWKSetCache -> Pool Hasql.Connection -> DeployedAuthConfig -> Text -> IO (Maybe Principal)
resolveDeployedBearerPrincipal jwksCache pool deployedCfg token = do
  mPatPrincipal <- case deployedCfg.tokenLookup of
    TokenLookupDatabase -> Auth.resolveAccessTokenPrincipalWithSecret pool deployedCfg.tokenHashSecret token
  case mPatPrincipal of
    Just resolved -> do
      withPrincipalContext (Just resolved.principal) $
        Auth.touchAccessTokenLastUsed pool resolved.tokenId
      pure (Just resolved.principal)
    Nothing        -> resolveJwtPrincipal jwksCache pool deployedCfg token

resolveJwtPrincipal :: JWKSetCache -> Pool Hasql.Connection -> DeployedAuthConfig -> Text -> IO (Maybe Principal)
resolveJwtPrincipal jwksCache pool deployedCfg token = case (deployedCfg.issuer, jwtExpectedAudience deployedCfg) of
  (Just expectedIssuer, Just expectedAudience) -> resolveJwtPrincipalForAudience jwksCache pool deployedCfg expectedIssuer expectedAudience token
  _ -> pure Nothing

resolveJwtPrincipalForAudience :: JWKSetCache -> Pool Hasql.Connection -> DeployedAuthConfig -> Text -> Text -> Text -> IO (Maybe Principal)
resolveJwtPrincipalForAudience jwksCache pool deployedCfg expectedIssuer expectedAudience token = do
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

jwtExpectedAudience :: DeployedAuthConfig -> Maybe Text
jwtExpectedAudience deployedCfg = deployedCfg.audience <|> deployedCfg.clientId

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
        Just (OidcDiscovery discoveredIssuer _authorizationEndpoint _tokenEndpoint jwksUri)
          | Just discoveredIssuer == deployedCfg.issuer -> fetchJson jwksUri
        _ -> pure Nothing

data OidcDiscovery = OidcDiscovery Text Text Text Text

instance FromJSON OidcDiscovery where
  parseJSON = Aeson.withObject "OidcDiscovery" $ \o -> OidcDiscovery
    <$> o .: "issuer"
    <*> o .: "authorization_endpoint"
    <*> o .: "token_endpoint"
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

data OidcEndpoints = OidcEndpoints
  { oidcAuthorizationEndpoint :: !Text
  , oidcTokenEndpoint :: !Text
  }

resolveOidcEndpoints :: DeployedAuthConfig -> IO (Maybe OidcEndpoints)
resolveOidcEndpoints deployedCfg = case (deployedCfg.authorizationEndpoint, deployedCfg.tokenEndpoint) of
  (Just authEndpoint, Just tokenEndpoint) -> pure $ mkOidcEndpoints authEndpoint tokenEndpoint
  _ -> case deployedCfg.discoveryUrl of
    Nothing -> pure Nothing
    Just discovery -> do
      mDiscoveryBody <- fetchJson discovery
      pure $ case mDiscoveryBody >>= decode of
        Just (OidcDiscovery discoveredIssuer authEndpoint tokenEndpoint _jwksUri)
          | Just discoveredIssuer == deployedCfg.issuer ->
              mkOidcEndpoints
                (fromMaybeText deployedCfg.authorizationEndpoint authEndpoint)
                (fromMaybeText deployedCfg.tokenEndpoint tokenEndpoint)
        _ -> Nothing
  where
    fromMaybeText mConfigured discovered = maybe discovered id mConfigured

    mkOidcEndpoints authEndpoint tokenEndpoint
      | httpsUrl authEndpoint && httpsUrl tokenEndpoint = Just OidcEndpoints
          { oidcAuthorizationEndpoint = authEndpoint
          , oidcTokenEndpoint = tokenEndpoint
          }
      | otherwise = Nothing

httpsUrl :: Text -> Bool
httpsUrl = T.isPrefixOf "https://"

data OidcTokenResponse = OidcTokenResponse
  { oidcIdToken :: !Text
  }

instance FromJSON OidcTokenResponse where
  parseJSON = Aeson.withObject "OidcTokenResponse" $ \o -> OidcTokenResponse
    <$> o .: "id_token"

oidcAuthRoutesMiddleware :: OidcCodeExchange -> JWKSetCache -> AuthConfig -> Pool Hasql.Connection -> Middleware
oidcAuthRoutesMiddleware oidcCodeExchange jwksCache authCfg pool app req respond = case Wai.pathInfo req of
  ["api", "v1", "auth", "login"]
    | Wai.requestMethod req == methodGet -> handleOidcLogin authCfg req respond
  ["api", "v1", "auth", "callback"]
    | Wai.requestMethod req == methodGet -> handleOidcCallback oidcCodeExchange jwksCache authCfg pool req respond
  ["api", "v1", "auth", "logout"]
    | Wai.requestMethod req == methodPost -> handleOidcLogout authCfg pool req respond
  _ -> app req respond

handleOidcLogin :: AuthConfig -> Wai.Request -> (Wai.Response -> IO Wai.ResponseReceived) -> IO Wai.ResponseReceived
handleOidcLogin authCfg req respond = case authCfg.mode of
  AuthModeLocal -> respond $ jsonResponse status404 "not_found" "OIDC login is only available in deployed auth mode"
  AuthModeDeployed -> case (authCfg.deployed.clientId, authCfg.deployed.redirectUri) of
    (Just clientId, Just redirectUri) -> do
      mEndpoints <- resolveOidcEndpoints authCfg.deployed
      case mEndpoints of
        Nothing -> respond $ jsonResponse status503 "oidc_unavailable" "OIDC authorization endpoint is not configured"
        Just endpoints -> do
          stateToken <- randomSecret "hmem_oidc_state_v1_"
          let returnTo = sanitizeReturnTo $ queryParamText "return_to" req
              authUrl = appendQuery endpoints.oidcAuthorizationEndpoint
                [ ("client_id", Just clientId)
                , ("redirect_uri", Just redirectUri)
                , ("response_type", Just "code")
                , ("scope", Just $ T.unwords authCfg.deployed.scopes)
                , ("state", Just stateToken)
                ]
              headers =
                [ (hLocation, TE.encodeUtf8 authUrl)
                , setCookieHeader (stateCookieName authCfg.deployed) stateToken (Just 300) True authCfg.deployed
                , setCookieHeader (returnCookieName authCfg.deployed) (encodeCookieValue returnTo) (Just 300) True authCfg.deployed
                ]
          respond $ Wai.responseLBS status302 headers ""
    _ -> respond $ jsonResponse status503 "oidc_unavailable" "OIDC client_id and redirect_uri must be configured"

handleOidcCallback :: OidcCodeExchange -> JWKSetCache -> AuthConfig -> Pool Hasql.Connection -> Wai.Request -> (Wai.Response -> IO Wai.ResponseReceived) -> IO Wai.ResponseReceived
handleOidcCallback oidcCodeExchange jwksCache authCfg pool req respond = case authCfg.mode of
  AuthModeLocal -> respond $ jsonResponse status404 "not_found" "OIDC callback is only available in deployed auth mode"
  AuthModeDeployed -> do
    let deployedCfg = authCfg.deployed
        mCode = queryParamText "code" req
        mReturnedState = queryParamText "state" req
        mStoredState = lookupCookie (stateCookieName deployedCfg) req
        returnTo = sanitizeReturnTo $ decodeCookieValue <$> lookupCookie (returnCookieName deployedCfg) req
    case (mCode, mReturnedState, mStoredState) of
      (Just code, Just returnedState, Just storedState)
        | returnedState == storedState -> do
            result <- completeOidcCallback oidcCodeExchange jwksCache pool deployedCfg code
            case result of
              Left msg -> respond $ jsonResponseWithHeaders status401 (callbackClearHeaders deployedCfg) "oidc_login_failed" msg
              Right (sessionToken, csrfToken) -> do
                let headers =
                      [ (hLocation, TE.encodeUtf8 returnTo)
                      , setCookieHeader deployedCfg.sessionCookieName sessionToken (Just deployedCfg.sessionTtlSeconds) True deployedCfg
                      , setCookieHeader deployedCfg.csrfCookieName csrfToken (Just deployedCfg.sessionTtlSeconds) False deployedCfg
                      , clearCookieHeader (stateCookieName deployedCfg) deployedCfg
                      , clearCookieHeader (returnCookieName deployedCfg) deployedCfg
                      ]
                respond $ Wai.responseLBS status302 headers ""
      _ -> respond $ jsonResponseWithHeaders status401 (callbackClearHeaders deployedCfg) "oidc_state_mismatch" "OIDC callback state did not match the login state cookie"

completeOidcCallback :: OidcCodeExchange -> JWKSetCache -> Pool Hasql.Connection -> DeployedAuthConfig -> Text -> IO (Either Text (Text, Text))
completeOidcCallback oidcCodeExchange jwksCache pool deployedCfg code
  | deployedCfg.sessionTtlSeconds <= 0 = pure $ Left "OIDC session_ttl_seconds must be positive"
  | otherwise = case (deployedCfg.clientId, deployedCfg.clientSecret, deployedCfg.redirectUri) of
    (Just clientId, Just _clientSecret, Just _redirectUri) -> do
      mIdToken <- oidcCodeExchange deployedCfg code
      case mIdToken of
        Nothing -> pure $ Left "OIDC provider token exchange failed"
        Just idToken -> do
          mPrincipal <- case deployedCfg.issuer of
            Nothing -> pure Nothing
            Just expectedIssuer -> resolveJwtPrincipalForAudience jwksCache pool deployedCfg expectedIssuer clientId idToken
          case mPrincipal >>= principalGrantUser of
            Nothing -> pure $ Left "OIDC subject is not linked to an active hmem user"
            Just userId -> do
              now <- getCurrentTime
              let expiresAt = addUTCTime (fromIntegral deployedCfg.sessionTtlSeconds) now
              sessionToken <- randomSecret "hmem_sess_v1_"
              csrfToken <- randomSecret "hmem_csrf_v1_"
              _ <- Auth.createAuthSession pool userId sessionToken csrfToken expiresAt
              pure $ Right (sessionToken, csrfToken)
    _ -> pure $ Left "OIDC client_id, client_secret, and redirect_uri must be configured"

defaultOidcCodeExchange :: OidcCodeExchange
defaultOidcCodeExchange deployedCfg code = case (deployedCfg.clientId, deployedCfg.clientSecret, deployedCfg.redirectUri) of
  (Just clientId, Just clientSecret, Just redirectUri) -> do
    mEndpoints <- resolveOidcEndpoints deployedCfg
    case mEndpoints of
      Nothing -> pure Nothing
      Just endpoints -> fmap (.oidcIdToken) <$> exchangeAuthorizationCode endpoints.oidcTokenEndpoint clientId clientSecret redirectUri code
  _ -> pure Nothing

callbackClearHeaders :: DeployedAuthConfig -> [(HeaderName, ByteString)]
callbackClearHeaders deployedCfg =
  [ clearCookieHeader (stateCookieName deployedCfg) deployedCfg
  , clearCookieHeader (returnCookieName deployedCfg) deployedCfg
  ]

exchangeAuthorizationCode :: Text -> Text -> Text -> Text -> Text -> IO (Maybe OidcTokenResponse)
exchangeAuthorizationCode tokenEndpoint clientId clientSecret redirectUri code
  | not (httpsUrl tokenEndpoint) = pure Nothing
  | otherwise = do
      result <- try $ do
        parsed <- parseRequest (T.unpack tokenEndpoint)
        manager <- newManager tlsManagerSettings
        let request = urlEncodedBody
              [ ("grant_type", "authorization_code")
              , ("code", TE.encodeUtf8 code)
              , ("redirect_uri", TE.encodeUtf8 redirectUri)
              , ("client_id", TE.encodeUtf8 clientId)
              , ("client_secret", TE.encodeUtf8 clientSecret)
              ]
              parsed
        response <- httpLbs request { method = "POST", requestHeaders = ("Accept", "application/json") : requestHeaders request } manager
        pure $ if statusCode (responseStatus response) >= 200 && statusCode (responseStatus response) < 300
          then Just (responseBody response)
          else Nothing
      case result of
        Left (_ :: SomeException) -> pure Nothing
        Right mBody -> pure (mBody >>= decode)

handleOidcLogout :: AuthConfig -> Pool Hasql.Connection -> Wai.Request -> (Wai.Response -> IO Wai.ResponseReceived) -> IO Wai.ResponseReceived
handleOidcLogout authCfg pool req respond = do
  logoutAllowed <- case authCfg.mode of
    AuthModeLocal -> pure True
    AuthModeDeployed -> case lookupCookie authCfg.deployed.sessionCookieName req of
      Nothing -> pure True
      Just sessionToken -> case (lookupCookie authCfg.deployed.csrfCookieName req, csrfHeader authCfg.deployed req) of
        (Just csrfCookie, Just csrfHeaderValue)
          | csrfCookie == csrfHeaderValue -> Auth.authSessionCsrfMatches pool sessionToken csrfHeaderValue
        _ -> pure False
  if not logoutAllowed
    then respond $ jsonResponse status403 "csrf_required" "Logout requires a matching CSRF token"
    else do
      case authCfg.mode of
        AuthModeDeployed -> case lookupCookie authCfg.deployed.sessionCookieName req of
          Just sessionToken -> do
            _ <- Auth.revokeAuthSession pool sessionToken
            pure ()
          Nothing -> pure ()
        AuthModeLocal -> pure ()
      let deployedCfg = authCfg.deployed
          headers =
            [ (hLocation, "/")
            , clearCookieHeader deployedCfg.sessionCookieName deployedCfg
            , clearCookieHeader deployedCfg.csrfCookieName deployedCfg
            , clearCookieHeader (stateCookieName deployedCfg) deployedCfg
            , clearCookieHeader (returnCookieName deployedCfg) deployedCfg
            ]
      respond $ Wai.responseLBS status302 headers ""

csrfMiddleware :: AuthConfig -> Pool Hasql.Connection -> Middleware
csrfMiddleware authCfg pool app req respond
  | authCfg.mode /= AuthModeDeployed = app req respond
  | Wai.requestMethod req `elem` [methodGet, methodHead, methodOptions] = app req respond
  | isJust (bearerToken req) = app req respond
  | otherwise = case lookupCookie authCfg.deployed.sessionCookieName req of
      Nothing -> app req respond
      Just sessionToken -> case (lookupCookie authCfg.deployed.csrfCookieName req, csrfHeader authCfg.deployed req) of
        (Just csrfCookie, Just csrfHeaderValue)
          | csrfCookie == csrfHeaderValue -> do
              ok <- Auth.authSessionCsrfMatches pool sessionToken csrfHeaderValue
              if ok
                then app req respond
                else respond csrfDenied
        _ -> respond csrfDenied
  where
    csrfDenied = jsonResponse status403 "csrf_required" "Cookie-authenticated unsafe requests require a matching CSRF token"

principalGrantUser :: Principal -> Maybe UUID.UUID
principalGrantUser principal = case principal.authority of
  PrincipalGrantUser userId -> Just userId
  _ -> Nothing

randomSecret :: Text -> IO Text
randomSecret prefix = do
  a <- UUID.nextRandom
  b <- UUID.nextRandom
  c <- UUID.nextRandom
  pure $ prefix <> UUID.toText a <> UUID.toText b <> UUID.toText c

appendQuery :: Text -> [(Text, Maybe Text)] -> Text
appendQuery url params
  | T.null rendered = url
  | "?" `T.isInfixOf` url = url <> "&" <> rendered
  | otherwise = url <> "?" <> rendered
  where
    rendered = TE.decodeUtf8 $ BS.drop 1 $ renderQuery True
      [ (TE.encodeUtf8 key, TE.encodeUtf8 <$> value)
      | (key, value) <- params
      ]

queryParamText :: ByteString -> Wai.Request -> Maybe Text
queryParamText name req = do
  raw <- lookup name (Wai.queryString req) >>= id
  case TE.decodeUtf8' raw of
    Left _ -> Nothing
    Right txt -> Just (T.strip txt)

lookupCookie :: Text -> Wai.Request -> Maybe Text
lookupCookie cookieName req = do
  rawCookie <- lookup "Cookie" (Wai.requestHeaders req)
  let wanted = TE.encodeUtf8 cookieName
  value <- findCookie wanted rawCookie
  case TE.decodeUtf8' value of
    Left _ -> Nothing
    Right txt | T.null txt -> Nothing
    Right txt -> Just txt

findCookie :: ByteString -> ByteString -> Maybe ByteString
findCookie wanted rawCookie = go (BS.split 59 rawCookie)
  where
    go [] = Nothing
    go (part:rest) =
      let trimmed = BS.dropWhile (== 32) part
          (name, valueWithEquals) = BS.break (== 61) trimmed
      in if name == wanted && not (BS.null valueWithEquals)
          then Just (BS.drop 1 valueWithEquals)
          else go rest

csrfHeader :: DeployedAuthConfig -> Wai.Request -> Maybe Text
csrfHeader deployedCfg req = do
  raw <- lookup (fromString $ T.unpack deployedCfg.csrfHeaderName) (Wai.requestHeaders req)
  case TE.decodeUtf8' raw of
    Left _ -> Nothing
    Right txt | T.null txt -> Nothing
    Right txt -> Just txt

setCookieHeader :: Text -> Text -> Maybe Int -> Bool -> DeployedAuthConfig -> (HeaderName, ByteString)
setCookieHeader name value mMaxAge httpOnly deployedCfg =
  ("Set-Cookie", TE.encodeUtf8 $ T.intercalate "; " $ concat
    [ [ name <> "=" <> value
      , "Path=/"
      ]
    , maybe [] (\seconds -> ["Max-Age=" <> T.pack (show seconds)]) mMaxAge
    , ["SameSite=" <> cookieSameSiteValue deployedCfg.cookieSameSite]
    , ["Secure" | deployedCfg.cookieSecure]
    , ["HttpOnly" | httpOnly]
    ])

cookieSameSiteValue :: Text -> Text
cookieSameSiteValue raw = case T.toLower (T.strip raw) of
  "strict" -> "Strict"
  "none" -> "None"
  _ -> "Lax"

clearCookieHeader :: Text -> DeployedAuthConfig -> (HeaderName, ByteString)
clearCookieHeader name = setCookieHeader name "" (Just 0) True

stateCookieName :: DeployedAuthConfig -> Text
stateCookieName deployedCfg = deployedCfg.sessionCookieName <> "_oidc_state"

returnCookieName :: DeployedAuthConfig -> Text
returnCookieName deployedCfg = deployedCfg.sessionCookieName <> "_return_to"

sanitizeReturnTo :: Maybe Text -> Text
sanitizeReturnTo (Just value)
  | "/" `T.isPrefixOf` value
  , not ("//" `T.isPrefixOf` value) = value
sanitizeReturnTo _ = "/"

encodeCookieValue :: Text -> Text
encodeCookieValue = TE.decodeUtf8 . urlEncode True . TE.encodeUtf8

decodeCookieValue :: Text -> Text
decodeCookieValue = TE.decodeUtf8Lenient . urlDecode True . TE.encodeUtf8

jsonResponse :: Status -> Text -> Text -> Wai.Response
jsonResponse status = jsonResponseWithHeaders status []

jsonResponseWithHeaders :: Status -> [(HeaderName, ByteString)] -> Text -> Text -> Wai.Response
jsonResponseWithHeaders status headers err msg = Wai.responseLBS status
  ((hContentType, "application/json") : headers)
  (encode $ object ["error" .= err, "message" .= msg])

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
  tokenBytes <- BS8.stripPrefix "Bearer " raw
  case TE.decodeUtf8' tokenBytes of
    Left _ -> Nothing
    Right token
      | T.null token -> Nothing
      | otherwise -> Just token

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

corsMiddleware :: AuthConfig -> CorsConfig -> Application -> Application
corsMiddleware authCfg corsCfg = cors $ \req ->
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
      , corsRequestHeaders = ["Content-Type", "Authorization", fromString (T.unpack authCfg.deployed.csrfHeaderName), "X-CSRF-Token", "X-Request-Id", "X-Request-ID"]
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
