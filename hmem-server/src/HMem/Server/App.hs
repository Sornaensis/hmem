module HMem.Server.App
  ( mkApp
  , mkAppWithChangeStream
  , mkAppWithDiscovery
  , mkAppWithPrefixProofs
  , mkAppWithOidcCodeExchange -- ^ Test-only seam; production callers should use 'mkApp'.
  , mkAppWithOidcCodeExchangeAndClock -- ^ Test-only clock for pending-login expiry.
  , OidcCodeExchange -- ^ Test-only injected OIDC code exchange type.
  , requestIdMiddleware
  , resolveRequestPrincipal
  , resolveRequestPrincipalIO
  , authMiddleware
  , botIdentityMiddleware
  ) where

import Control.Applicative ((<|>))
import Control.Exception (SomeException, try)
import Control.Lens ((&), (.~), (^.), preview)
import Control.Monad (guard)
import Data.Bits (xor, (.|.))
import Data.Char (isControl)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.Maybe (isJust)
import Data.Map.Strict qualified as Map
import Data.Aeson (FromJSON, Value(..), decode, encode, fromJSON, object, Result(..), (.:), (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as AesonKeyMap
import Data.ByteArray.Encoding (Base(..), convertToBase)
import Data.ByteString (ByteString, isPrefixOf)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as LBS
import Data.List (dropWhileEnd, find, foldl')
import Data.Pool (Pool)
import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time (UTCTime, NominalDiffTime, addUTCTime, diffUTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID
import Hasql.Connection qualified as Hasql
import Network.HTTP.Client (httpLbs, method, newManager, parseRequest, requestHeaders, responseBody, responseStatus, urlEncodedBody)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.Wai (Application, Middleware)
import Network.Wai qualified as Wai
import Network.HTTP.Types (HeaderName, Status, mkStatus, status200, status302, status400, status401, status403, status404, status413, status429, status503, hContentType, hLocation, methodGet, methodHead, methodOptions, methodPost, statusCode)
import Network.HTTP.Types.URI (renderQuery)
import Network.Wai.Middleware.Cors
    (cors, simpleCorsResourcePolicy, corsRequestHeaders, corsMethods, corsOrigins, CorsResourcePolicy(..))
import Servant (Proxy (..), ServerError(..), runHandler, serve)

import Crypto.JWT qualified as JWT
import Crypto.JOSE.JWK qualified as JWK
import Crypto.Hash (Digest, SHA256, hash)
import Crypto.Random (getRandomBytes)
import HMem.Config (AuthConfig(..), AuthMode(..), DeployedAuthConfig(..), LocalAuthConfig(..), LocalBotTokenConfig(..), CorsConfig(..), RateLimitConfig(..), TokenLookupMode(..), authMcpProvenanceToken, authStaticBearerEnabled, authStaticBearerToken)
import HMem.DB.Auth qualified as Auth
import HMem.Server.BotIdentity (requestBotIdentity)
import HMem.DB.RequestContext (ActorType(..), ChangeCause(..), Principal(..), PrincipalAuthority(..), RequestContext(..), currentPrincipal, resolveEffectiveActor, withEffectiveActorContext, withChangeCauseContext, withPrincipalContext, withRequestContext, emptyRequestContext)
import HMem.Server.AccessTracker (AccessTracker)
import HMem.Config qualified as Config
import HMem.Server.API (HMemAPI, serverWithPrefixProofs, authorizeAgentSessionWrite)
import HMem.Server.SessionPrefix (PrefixRegistry)
import HMem.AgentSession.Transport (maxSessionRequestBytes)
import HMem.Server.ObservationDiscovery (ObservationDiscovery, newObservationDiscovery)
import HMem.Server.OpenAPI (openApiSpec)
import HMem.Server.Static (staticMiddleware)
import HMem.Server.SessionArchive (newArchiveMiddleware)
import HMem.Server.WebSocket (WSState, wsMiddleware)

-- | Build the WAI Application.
mkApp :: Middleware -> AuthConfig -> CorsConfig -> RateLimitConfig -> Pool Hasql.Connection -> AccessTracker -> WSState -> Maybe FilePath -> Bool -> IO Application
mkApp = mkAppWithOidcCodeExchange defaultOidcCodeExchange

-- | Production entry points provide the configured change-stream lifetimes;
-- the compatibility constructor above retains the default for focused tests.
mkAppWithChangeStream :: Config.ChangeStreamConfig -> Middleware -> AuthConfig -> CorsConfig -> RateLimitConfig -> Pool Hasql.Connection -> AccessTracker -> WSState -> Maybe FilePath -> Bool -> IO Application
mkAppWithChangeStream = mkAppWithOidcCodeExchangeAndChangeStream defaultOidcCodeExchange

-- | Production shares the lifecycle-owned validated provider with discovery.
mkAppWithDiscovery :: ObservationDiscovery -> Config.ChangeStreamConfig -> Middleware -> AuthConfig -> CorsConfig -> RateLimitConfig -> Pool Hasql.Connection -> AccessTracker -> WSState -> Maybe FilePath -> Bool -> IO Application
mkAppWithDiscovery discovery = mkAppWithServices Nothing (Just discovery) defaultOidcCodeExchange

-- The caller owns and joins proof workers before releasing the database pool.
mkAppWithPrefixProofs :: PrefixRegistry -> Maybe ObservationDiscovery -> Config.ChangeStreamConfig -> Middleware -> AuthConfig -> CorsConfig -> RateLimitConfig -> Pool Hasql.Connection -> AccessTracker -> WSState -> Maybe FilePath -> Bool -> IO Application
mkAppWithPrefixProofs prefixes discovery = mkAppWithServices (Just prefixes) discovery defaultOidcCodeExchange

-- | Provider configuration, authorization code, and its PKCE verifier.
type OidcCodeExchange = DeployedAuthConfig -> Text -> Text -> IO (Maybe Text)

-- | Build the WAI Application with an injected OIDC code exchange.
--
-- This constructor exists for deterministic tests of the browser OIDC callback
-- flow. Production entry points should call 'mkApp', which always uses
-- 'defaultOidcCodeExchange' and the configured HTTPS provider token endpoint.
mkAppWithOidcCodeExchange :: OidcCodeExchange -> Middleware -> AuthConfig -> CorsConfig -> RateLimitConfig -> Pool Hasql.Connection -> AccessTracker -> WSState -> Maybe FilePath -> Bool -> IO Application
mkAppWithOidcCodeExchange oidcCodeExchange = mkAppWithOidcCodeExchangeAndChangeStream oidcCodeExchange Config.defaultConfig.changeStream

mkAppWithOidcCodeExchangeAndClock :: IO UTCTime -> OidcCodeExchange -> Middleware -> AuthConfig -> CorsConfig -> RateLimitConfig -> Pool Hasql.Connection -> AccessTracker -> WSState -> Maybe FilePath -> Bool -> IO Application
mkAppWithOidcCodeExchangeAndClock clock oidcCodeExchange =
  mkAppWithServicesAt clock Nothing Nothing oidcCodeExchange Config.defaultConfig.changeStream

mkAppWithOidcCodeExchangeAndChangeStream :: OidcCodeExchange -> Config.ChangeStreamConfig -> Middleware -> AuthConfig -> CorsConfig -> RateLimitConfig -> Pool Hasql.Connection -> AccessTracker -> WSState -> Maybe FilePath -> Bool -> IO Application
mkAppWithOidcCodeExchangeAndChangeStream = mkAppWithServices Nothing Nothing

mkAppWithServices :: Maybe PrefixRegistry -> Maybe ObservationDiscovery -> OidcCodeExchange -> Config.ChangeStreamConfig -> Middleware -> AuthConfig -> CorsConfig -> RateLimitConfig -> Pool Hasql.Connection -> AccessTracker -> WSState -> Maybe FilePath -> Bool -> IO Application
mkAppWithServices = mkAppWithServicesAt getCurrentTime

mkAppWithServicesAt :: IO UTCTime -> Maybe PrefixRegistry -> Maybe ObservationDiscovery -> OidcCodeExchange -> Config.ChangeStreamConfig -> Middleware -> AuthConfig -> CorsConfig -> RateLimitConfig -> Pool Hasql.Connection -> AccessTracker -> WSState -> Maybe FilePath -> Bool -> IO Application
mkAppWithServicesAt oidcNow prefixes discovery oidcCodeExchange changeStreamConfig logger authCfg corsCfg rateLimitCfg pool tracker wsState mStaticDir pgvec = do
  discoveryService <- maybe (newObservationDiscovery False) pure discovery
  rateLimit <- rateLimitMiddleware rateLimitCfg
  archive <- newArchiveMiddleware pool
  jwksCache <- newIORef Nothing
  oidcLogins <- newIORef Map.empty
  pure $ requestIdMiddleware
       $ logger
       $ staticMiddleware mStaticDir
        $ corsMiddleware authCfg corsCfg
        $ rateLimit
         $ wsMiddleware authCfg pool wsState
        $ oidcAuthRoutesMiddleware oidcNow oidcLogins oidcCodeExchange jwksCache authCfg pool
        $ csrfMiddleware authCfg pool
        $ principalContextMiddleware jwksCache authCfg pool
        $ authMiddleware authCfg
        $ archive
        $ agentSessionBodyLimit pool
        $ openApiMiddleware
        $ normalizeObservationIfMatch
         $ serve (Proxy @HMemAPI) (serverWithPrefixProofs prefixes authCfg changeStreamConfig (Just discoveryService) pool tracker wsState pgvec)

-- Authorize before inspecting either declared size or streamed body content.
-- Read at most 2 MiB before JSON/base64 decoding, including chunked requests.
agentSessionBodyLimit :: Pool Hasql.Connection -> Middleware
agentSessionBodyLimit pool app req respond = case sessionMutationWorkspace req of
  Nothing -> app req respond
  Just workspace -> do
    authority <- runHandler (authorizeAgentSessionWrite pool workspace)
    case authority of
      Left failure -> respond $ Wai.responseLBS
        (mkStatus failure.errHTTPCode (BS8.pack failure.errReasonPhrase)) failure.errHeaders failure.errBody
      Right () -> case Wai.requestBodyLength req of
        Wai.KnownLength count | count > fromIntegral maxSessionRequestBytes -> tooLarge
        _ -> do
          bounded <- collect 0 []
          case bounded of
            Nothing -> tooLarge
            Just chunks -> do
              remaining <- newIORef chunks
              let next = atomicModifyIORef' remaining $ \case [] -> ([], BS.empty); chunk:rest -> (rest,chunk)
              app req { Wai.requestBody = next, Wai.requestBodyLength = Wai.KnownLength (fromIntegral (sum (map BS.length chunks))) } respond
  where
    tooLarge = respond $ jsonResponse status413 "session_too_large" "Session request body exceeds 2 MiB"
    collect total chunks = do
      chunk <- Wai.getRequestBodyChunk req
      let nextTotal = total + BS.length chunk
      if nextTotal > maxSessionRequestBytes then pure Nothing
      else if BS.null chunk then pure (Just (reverse chunks))
      else collect nextTotal (chunk:chunks)

sessionMutationWorkspace :: Wai.Request -> Maybe UUID.UUID
sessionMutationWorkspace req = case (Wai.requestMethod req, dropWhileEnd T.null (Wai.pathInfo req)) of
  ("POST", ["api","v1","workspaces",workspace,"sessions"]) -> UUID.fromText workspace
  ("POST", ["api","v1","workspaces",workspace,"sessions","imports"]) -> UUID.fromText workspace
  ("POST", ["api","v1","workspaces",workspace,"sessions",_,operation])
    | operation `elem` ["append","heartbeat","terminate","prefix-verifications"] -> UUID.fromText workspace
  ("PUT", ["api","v1","workspaces",workspace,"sessions",_,"tasks",_]) -> UUID.fromText workspace
  ("PUT", ["api","v1","workspaces",workspace,"sessions",_,"runtime"]) -> UUID.fromText workspace
  _ -> Nothing

-- Servant's Header combinator selects the first physical header line. Join
-- repeated content preconditions so the authorized handler rejects a tag list,
-- rather than silently selecting one token before a competing write.
normalizeObservationIfMatch :: Middleware
normalizeObservationIfMatch app req respond = case (Wai.requestMethod req, Wai.pathInfo req) of
  ("PUT", ["api", "v1", "observations", _]) ->
    case [value | (name, value) <- Wai.requestHeaders req, name == "If-Match"] of
      values@(_:_:_) -> app req
        { Wai.requestHeaders = ("If-Match", BS.intercalate "," values)
            : filter ((/= "If-Match") . fst) (Wai.requestHeaders req)
        } respond
      _ -> app req respond
  _ -> app req respond

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
        { requestId = Just (TE.decodeUtf8Lenient rid) }
  withRequestContext reqCtx $
    app req' $ \resp -> respond $ Wai.mapResponseHeaders (("X-Request-Id", rid) :) resp

-- A MCP cause needs both the ordinary marker and a private provenance token.
-- A user bearer/PAT can therefore never select it by adding a request header.
changeCauseFromRequest :: AuthConfig -> Wai.Request -> ChangeCause
changeCauseFromRequest authCfg req
  | trustedMcpProvenance authCfg req = ChangeCauseMcp
  | isAuditRevertPath (Wai.pathInfo req) = ChangeCauseAuditRevert
  | otherwise = ChangeCauseRest

trustedMcpProvenance :: AuthConfig -> Wai.Request -> Bool
trustedMcpProvenance authCfg req =
  lookup "X-HMem-Change-Cause" (Wai.requestHeaders req) == Just "mcp"
    && maybe False (\secret -> maybe False (secureByteStringEqual (TE.encodeUtf8 secret)) (lookup "X-HMem-MCP-Provenance" (Wai.requestHeaders req))) (authMcpProvenanceToken authCfg)

secureByteStringEqual :: ByteString -> ByteString -> Bool
secureByteStringEqual expected actual =
  not (BS.null expected)
    && BS.length expected == BS.length actual
    && foldl' (.|.) 0 (BS.zipWith xor expected actual) == 0

isAuditRevertPath :: [Text] -> Bool
isAuditRevertPath path = case path of
  ["api", "v1", "audit", _, "revert"] -> True
  _ -> False

type JWKSetCache = IORef (Maybe (UTCTime, JWK.JWKSet))

principalContextMiddleware :: JWKSetCache -> AuthConfig -> Pool Hasql.Connection -> Middleware
principalContextMiddleware jwksCache authCfg pool app req respond = do
  existingPrincipal <- currentPrincipal
  resolvedPrincipal <- case existingPrincipal of
    Just principal -> pure (Just principal)
    Nothing -> resolveRequestPrincipalWithCache jwksCache pool authCfg req
  withPrincipalContext resolvedPrincipal $
    withChangeCauseContext (Just (changeCauseFromRequest authCfg req)) $
      botIdentityMiddleware authCfg app req respond

-- | One JSON declaration header; clients cannot supply actor IDs. Validate even
-- on reads and injected-principal paths so malformed metadata never falls back
-- to the implicit Local User. Unknown no-auth clients cannot be identified as bots.
botIdentityMiddleware :: AuthConfig -> Middleware
botIdentityMiddleware authCfg app req respond = case requestBotIdentity (Wai.requestHeaders req) of
  Left message -> respond (jsonResponse status400 "invalid_bot_identity" message)
  Right identity -> do
    original <- currentPrincipal
    if isBot original && isMutation && identity == Nothing
      then respond (jsonResponse status400 "bot_identity_required" "Bot mutations require X-HMem-Bot-Identity with role and model-id")
      else withEffectiveActorContext (fmap (\p -> resolveEffectiveActor (authCfg.mode == AuthModeLocal) p identity) original) (app req respond)
  where
    isBot = maybe False (\p -> p.actorType == ActorBot)
    isMutation = take 2 (Wai.pathInfo req) == ["api", "v1"]
      && Wai.requestMethod req `notElem` [methodGet, methodHead, methodOptions]

resolveRequestPrincipal :: AuthConfig -> Wai.Request -> Maybe Principal
resolveRequestPrincipal authCfg req =
  if isJust (Config.localUserTokenConfigError authCfg) then Nothing
  else if isJust authCfg.local.userToken && isJust (lookup "Authorization" (Wai.requestHeaders req))
    then resolveBearerPrincipal authCfg req
    else resolveBearerPrincipal authCfg req <|> defaultPrincipal authCfg
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
  mClaims <- verifiedJwtClaims jwksCache deployedCfg expectedIssuer expectedAudience token
  case mClaims >>= (\claims -> do
    _ <- claims ^. JWT.claimIss
    _ <- claims ^. JWT.claimAud
    preview JWT.string =<< (claims ^. JWT.claimSub)) of
    Nothing -> pure Nothing
    Just subject -> Auth.resolveUserPrincipalByAuthSubject pool subject

verifiedJwtClaims :: JWKSetCache -> DeployedAuthConfig -> Text -> Text -> Text -> IO (Maybe JWT.ClaimsSet)
verifiedJwtClaims jwksCache deployedCfg expectedIssuer expectedAudience token = do
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
              Right claims -> pure (Just claims)

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

data OidcLoginTransaction = OidcLoginTransaction
  { oidcCodeVerifier :: !Text
  , oidcNonce :: !Text
  , oidcBrowserBinding :: !Text
  , oidcReturnTo :: !Text
  , oidcExpiresAt :: !UTCTime
  }

type OidcLoginRegistry = IORef (Map.Map Text OidcLoginTransaction)

oidcLoginTtlSeconds :: Int
oidcLoginTtlSeconds = 300

oidcLoginCapacity :: Int
oidcLoginCapacity = 1024

-- Pending transactions are private, bounded, and consumed before provider I/O.
-- Established login sessions remain durable in PostgreSQL.
admitOidcLogin :: OidcLoginRegistry -> UTCTime -> Maybe Text -> Maybe Text -> Text -> OidcLoginTransaction -> IO Bool
admitOidcLogin registry now previousState previousBinding state transaction =
  atomicModifyIORef' registry $ \entries ->
    let live = discardBoundLogin previousState previousBinding (liveOidcLogins now entries)
    in if Map.size live >= oidcLoginCapacity
      then (live, False)
      else (Map.insert state transaction live, True)

consumeOidcLogin :: OidcLoginRegistry -> UTCTime -> Text -> Text -> IO (Maybe OidcLoginTransaction)
consumeOidcLogin registry now state binding = atomicModifyIORef' registry $ \entries ->
  let live = liveOidcLogins now entries
  in case Map.lookup state live of
    Just transaction | secretTextEqual transaction.oidcBrowserBinding binding ->
      (Map.delete state live, Just transaction)
    _ -> (live, Nothing)

discardOidcLogin :: OidcLoginRegistry -> UTCTime -> Maybe Text -> Maybe Text -> IO ()
discardOidcLogin registry now state binding = atomicModifyIORef' registry $ \entries ->
  (discardBoundLogin state binding (liveOidcLogins now entries), ())

liveOidcLogins :: UTCTime -> Map.Map Text OidcLoginTransaction -> Map.Map Text OidcLoginTransaction
liveOidcLogins now = Map.filter (\transaction -> transaction.oidcExpiresAt > now)

discardBoundLogin :: Maybe Text -> Maybe Text -> Map.Map Text OidcLoginTransaction -> Map.Map Text OidcLoginTransaction
discardBoundLogin (Just state) (Just binding) entries = case Map.lookup state entries of
  Just transaction | secretTextEqual transaction.oidcBrowserBinding binding -> Map.delete state entries
  _ -> entries
discardBoundLogin _ _ entries = entries

secretTextEqual :: Text -> Text -> Bool
secretTextEqual expected actual = secureByteStringEqual (TE.encodeUtf8 expected) (TE.encodeUtf8 actual)

newOidcSecret :: IO Text
newOidcSecret = do
  bytes <- getRandomBytes 32 :: IO ByteString
  pure $ TE.decodeUtf8 (convertToBase Base64URLUnpadded bytes)

pkceChallenge :: Text -> Text
pkceChallenge verifier = TE.decodeUtf8 $ convertToBase Base64URLUnpadded
  (hash (TE.encodeUtf8 verifier) :: Digest SHA256)

oidcAuthRoutesMiddleware :: IO UTCTime -> OidcLoginRegistry -> OidcCodeExchange -> JWKSetCache -> AuthConfig -> Pool Hasql.Connection -> Middleware
oidcAuthRoutesMiddleware clock logins oidcCodeExchange jwksCache authCfg pool app req respond = case Wai.pathInfo req of
  ["api", "v1", "auth", "login"]
    | Wai.requestMethod req == methodGet -> handleOidcLogin clock logins authCfg req respond
  ["api", "v1", "auth", "callback"]
    | Wai.requestMethod req == methodGet -> handleOidcCallback clock logins oidcCodeExchange jwksCache authCfg pool req respond
  ["api", "v1", "auth", "logout"]
    | Wai.requestMethod req == methodPost -> handleOidcLogout clock logins authCfg pool req respond
  _ -> app req respond

handleOidcLogin :: IO UTCTime -> OidcLoginRegistry -> AuthConfig -> Wai.Request -> (Wai.Response -> IO Wai.ResponseReceived) -> IO Wai.ResponseReceived
handleOidcLogin clock logins authCfg req respond = case authCfg.mode of
  AuthModeLocal -> respond $ jsonResponse status404 "not_found" "OIDC login is only available in deployed auth mode"
  AuthModeDeployed -> case (authCfg.deployed.clientId, authCfg.deployed.redirectUri) of
    (Just clientId, Just redirectUri) -> do
      mEndpoints <- resolveOidcEndpoints authCfg.deployed
      case mEndpoints of
        Nothing -> respond $ jsonResponse status503 "oidc_unavailable" "OIDC authorization endpoint is not configured"
        Just endpoints -> do
          stateToken <- randomSecret "hmem_oidc_state_v1_"
          verifier <- newOidcSecret
          nonce <- newOidcSecret
          binding <- newOidcSecret
          now <- clock
          let returnTo = sanitizeReturnTo $ queryParamText "return_to" req
          admitted <- admitOidcLogin logins now
            (lookupCookie (stateCookieName authCfg.deployed) req)
            (lookupCookie (bindingCookieName authCfg.deployed) req)
            stateToken OidcLoginTransaction
              { oidcCodeVerifier = verifier
              , oidcNonce = nonce
              , oidcBrowserBinding = binding
              , oidcReturnTo = returnTo
              , oidcExpiresAt = addUTCTime (fromIntegral oidcLoginTtlSeconds) now
              }
          let authUrl = appendQuery endpoints.oidcAuthorizationEndpoint
                [ ("client_id", Just clientId)
                , ("redirect_uri", Just redirectUri)
                , ("response_type", Just "code")
                , ("scope", Just $ T.unwords authCfg.deployed.scopes)
                , ("state", Just stateToken)
                , ("code_challenge", Just (pkceChallenge verifier))
                , ("code_challenge_method", Just "S256")
                , ("nonce", Just nonce)
                ]
              headers =
                [ (hLocation, TE.encodeUtf8 authUrl)
                , setOidcCookie (stateCookieName authCfg.deployed) stateToken (Just oidcLoginTtlSeconds) authCfg.deployed
                , setOidcCookie (bindingCookieName authCfg.deployed) binding (Just oidcLoginTtlSeconds) authCfg.deployed
                ]
          if admitted then respond $ Wai.responseLBS status302 headers ""
            else respond $ jsonResponse status503 "oidc_login_capacity" "Too many pending sign-ins; please try again later"
    _ -> respond $ jsonResponse status503 "oidc_unavailable" "OIDC client_id and redirect_uri must be configured"

handleOidcCallback :: IO UTCTime -> OidcLoginRegistry -> OidcCodeExchange -> JWKSetCache -> AuthConfig -> Pool Hasql.Connection -> Wai.Request -> (Wai.Response -> IO Wai.ResponseReceived) -> IO Wai.ResponseReceived
handleOidcCallback clock logins oidcCodeExchange jwksCache authCfg pool req respond = case authCfg.mode of
  AuthModeLocal -> respond $ jsonResponse status404 "not_found" "OIDC callback is only available in deployed auth mode"
  AuthModeDeployed -> do
    let deployedCfg = authCfg.deployed
        mCode = queryParamText "code" req
        mReturnedState = queryParamText "state" req
        mStoredState = lookupCookie (stateCookieName deployedCfg) req
        mBinding = lookupCookie (bindingCookieName deployedCfg) req
        invalidState = respond $ jsonResponseWithHeaders status401 (callbackClearHeaders deployedCfg) "oidc_state_mismatch" "OIDC callback did not match an active browser login transaction"
    now <- clock
    case (mCode, mReturnedState, mStoredState, mBinding) of
      (Just code, Just returnedState, Just storedState, Just binding)
        | secretTextEqual storedState returnedState -> do
          mTransaction <- consumeOidcLogin logins now storedState binding
          case mTransaction of
            Nothing -> invalidState
            Just transaction -> do
              result <- completeOidcCallback oidcCodeExchange jwksCache pool deployedCfg code transaction
              case result of
                Left msg -> respond $ jsonResponseWithHeaders status401 (callbackClearHeaders deployedCfg) "oidc_login_failed" msg
                Right (sessionToken, csrfToken) -> do
                  let headers =
                        [ (hLocation, TE.encodeUtf8 transaction.oidcReturnTo)
                        , setCookieHeader deployedCfg.sessionCookieName sessionToken (Just deployedCfg.sessionTtlSeconds) True deployedCfg
                        , setCookieHeader deployedCfg.csrfCookieName csrfToken (Just deployedCfg.sessionTtlSeconds) False deployedCfg
                        , clearOidcCookie (stateCookieName deployedCfg) deployedCfg
                        , clearOidcCookie (bindingCookieName deployedCfg) deployedCfg
                        , clearCookieHeader (returnCookieName deployedCfg) deployedCfg
                        ]
                  respond $ Wai.responseLBS status302 headers ""
      _ -> do
        discardOidcLogin logins now mStoredState mBinding
        invalidState

completeOidcCallback :: OidcCodeExchange -> JWKSetCache -> Pool Hasql.Connection -> DeployedAuthConfig -> Text -> OidcLoginTransaction -> IO (Either Text (Text, Text))
completeOidcCallback oidcCodeExchange jwksCache pool deployedCfg code transaction
  | deployedCfg.sessionTtlSeconds <= 0 = pure $ Left "OIDC session_ttl_seconds must be positive"
  | otherwise = case (deployedCfg.clientId, deployedCfg.clientSecret, deployedCfg.redirectUri) of
    (Just clientId, Just _clientSecret, Just _redirectUri) -> do
      mIdToken <- oidcCodeExchange deployedCfg code transaction.oidcCodeVerifier
      case mIdToken of
        Nothing -> pure $ Left "OIDC provider token exchange failed"
        Just idToken -> do
          now <- getCurrentTime
          mClaims <- case deployedCfg.issuer of
            Nothing -> pure Nothing
            Just expectedIssuer -> verifiedJwtClaims jwksCache deployedCfg expectedIssuer clientId idToken
          mPrincipal <- case mClaims of
            Just claims ->
              case deployedCfg.issuer >>= \issuer -> oidcTokenSubject now issuer clientId transaction.oidcNonce claims of
                Just subject -> Auth.resolveUserPrincipalByAuthSubject pool subject
                Nothing -> pure Nothing
            _ -> pure Nothing
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
defaultOidcCodeExchange deployedCfg code verifier = case (deployedCfg.clientId, deployedCfg.clientSecret, deployedCfg.redirectUri) of
  (Just clientId, Just clientSecret, Just redirectUri) -> do
    mEndpoints <- resolveOidcEndpoints deployedCfg
    case mEndpoints of
      Nothing -> pure Nothing
      Just endpoints -> fmap (.oidcIdToken) <$> exchangeAuthorizationCode endpoints.oidcTokenEndpoint clientId clientSecret redirectUri code verifier
  _ -> pure Nothing

callbackClearHeaders :: DeployedAuthConfig -> [(HeaderName, ByteString)]
callbackClearHeaders deployedCfg =
  [ clearOidcCookie (stateCookieName deployedCfg) deployedCfg
  , clearOidcCookie (bindingCookieName deployedCfg) deployedCfg
  , clearCookieHeader (returnCookieName deployedCfg) deployedCfg
  ]

-- This is the browser ID-token contract, not the more general API JWT policy.
-- Validate only verified claims and preserve opaque (including URI-shaped) sub.
oidcTokenSubject :: UTCTime -> Text -> Text -> Text -> JWT.ClaimsSet -> Maybe Text
oidcTokenSubject now issuer clientId expectedNonce claims = do
  Object values <- pure (Aeson.toJSON claims)
  String actualIssuer <- AesonKeyMap.lookup "iss" values
  guard (actualIssuer == issuer)
  audiences <- case AesonKeyMap.lookup "aud" values of
    Just (String audience) -> Just [audience]
    Just (Array entries) -> traverse (\case String audience -> Just audience; _ -> Nothing) (foldr (:) [] entries)
    _ -> Nothing
  guard (not (null audiences) && all (== clientId) audiences)
  expires <- numericClaim "exp" values
  issued <- numericClaim "iat" values
  let seconds = realToFrac (utcTimeToPOSIXSeconds now) :: Double
  guard (expires > seconds && issued <= seconds)
  case AesonKeyMap.lookup "azp" values of
    Nothing -> pure ()
    Just (String party) -> guard (party == clientId)
    _ -> Nothing
  String actualNonce <- AesonKeyMap.lookup "nonce" values
  guard (secretTextEqual expectedNonce actualNonce)
  String subject <- AesonKeyMap.lookup "sub" values
  guard (not (T.null subject))
  pure subject
  where
    numericClaim key values = case AesonKeyMap.lookup key values of
      Just value@(Number _) -> case fromJSON value of
        Success number | not (isNaN number || isInfinite number) -> Just number
        _ -> Nothing
      _ -> Nothing

exchangeAuthorizationCode :: Text -> Text -> Text -> Text -> Text -> Text -> IO (Maybe OidcTokenResponse)
exchangeAuthorizationCode tokenEndpoint clientId clientSecret redirectUri code verifier
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
              , ("code_verifier", TE.encodeUtf8 verifier)
              ]
              parsed
        response <- httpLbs request { method = "POST", requestHeaders = ("Accept", "application/json") : requestHeaders request } manager
        pure $ if statusCode (responseStatus response) >= 200 && statusCode (responseStatus response) < 300
          then Just (responseBody response)
          else Nothing
      case result of
        Left (_ :: SomeException) -> pure Nothing
        Right mBody -> pure (mBody >>= decode)

handleOidcLogout :: IO UTCTime -> OidcLoginRegistry -> AuthConfig -> Pool Hasql.Connection -> Wai.Request -> (Wai.Response -> IO Wai.ResponseReceived) -> IO Wai.ResponseReceived
handleOidcLogout clock logins authCfg pool req respond = do
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
            , clearOidcCookie (stateCookieName deployedCfg) deployedCfg
            , clearOidcCookie (bindingCookieName deployedCfg) deployedCfg
            , clearCookieHeader (returnCookieName deployedCfg) deployedCfg
            ]
      now <- clock
      discardOidcLogin logins now (lookupCookie (stateCookieName deployedCfg) req) (lookupCookie (bindingCookieName deployedCfg) req)
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

-- A provider's top-level GET callback must receive the short-lived transaction
-- cookies even when the established session uses the stricter SameSite policy.
setOidcCookie :: Text -> Text -> Maybe Int -> DeployedAuthConfig -> (HeaderName, ByteString)
setOidcCookie name value maxAge deployedCfg =
  setCookieHeader name value maxAge True (deployedCfg { cookieSameSite = "Lax" })

clearOidcCookie :: Text -> DeployedAuthConfig -> (HeaderName, ByteString)
clearOidcCookie name = setOidcCookie name "" (Just 0)

stateCookieName :: DeployedAuthConfig -> Text
stateCookieName deployedCfg = deployedCfg.sessionCookieName <> "_oidc_state"

bindingCookieName :: DeployedAuthConfig -> Text
bindingCookieName deployedCfg = deployedCfg.sessionCookieName <> "_oidc_binding"

returnCookieName :: DeployedAuthConfig -> Text
returnCookieName deployedCfg = deployedCfg.sessionCookieName <> "_return_to"

sanitizeReturnTo :: Maybe Text -> Text
sanitizeReturnTo (Just value)
  | "/" `T.isPrefixOf` value
  , not ("//" `T.isPrefixOf` value)
  , not (T.any (\character -> character == '\\' || isControl character) value) = value
sanitizeReturnTo _ = "/"

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
  localHumanPrincipal authCfg token <|> localBotPrincipal authCfg token <|> legacyPrincipal authCfg token

localHumanPrincipal :: AuthConfig -> Text -> Maybe Principal
localHumanPrincipal authCfg token = do
  guard (Config.authLocalUserTokenMatches authCfg token)
  pure localUserPrincipal

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
  | isJust (Config.localUserTokenConfigError authCfg) = unauthorized
  | not (authStaticBearerEnabled authCfg || isJust (Config.authLocalUserToken authCfg)) = app req respond
  | Wai.requestMethod req == methodOptions = app req respond
  | authorized = app req respond
  | otherwise = unauthorized
  where
    unauthorized = respond $ Wai.responseLBS status401
      [ (hContentType, "application/json")
      , ("WWW-Authenticate", "Bearer")
      ]
      (encode unauthorizedBody)
    unauthorizedBody = object
      [ "error" .= ("unauthorized" :: Text)
      , "message" .= ("Missing or invalid bearer token" :: Text)
      ]

    authorizedToken token = isJust (localHumanPrincipal authCfg token)
      || localBotTokenAuthorized authCfg token || maybe False (== token) (authStaticBearerToken authCfg)

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
      , corsRequestHeaders = ["Content-Type", "Authorization", fromString (T.unpack authCfg.deployed.csrfHeaderName), "X-CSRF-Token", "X-Request-Id", "X-Request-ID", "If-Match", "X-HMem-Bot-Identity"]
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
