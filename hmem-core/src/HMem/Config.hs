module HMem.Config
  ( -- * Config types
    HMemConfig(..)
  , ServerConfig(..)
  , DatabaseConfig(..)
  , PoolConfig(..)
  , LogConfig(..)
  , CorsConfig(..)
  , AuthConfig(..)
  , AuthMode(..)
  , LocalAuthConfig(..)
  , LocalBotTokenConfig(..)
  , DeployedAuthConfig(..)
  , TokenLookupMode(..)
  , RateLimitConfig(..)
  , ChangeStreamConfig(..)
  , EmbeddingProviderMode(..)
  , EmbeddingProviderConfig(..)
  , EmbeddingEndpointAuthority(..)
  , parseEmbeddingEndpointAuthority
  , embeddingEndpointPathAllowed
  , normalizeEmbeddingEndpointRoute
  , managedTeiModelId
  , managedTeiSpaceFingerprint
  , TlsConfig(..)
  , WebConfig(..)
    -- * Defaults
  , defaultConfig
    -- * Load / Save
  , loadConfig
  , loadConfigFile
  , saveConfig
    -- * Validation
  , validateConfig
    -- * Paths
  , configDir
  , configFilePath
    -- * Overrides
  , applyEnvOverrides
  , applyChangeStreamEnvOverrides
  , applyMcpProvenanceEnvOverride
  , applyEmbeddingProviderEnvOverrides
    -- * Derived helpers
  , connectionString
  , serverUrl
  , serverHostIsLoopback
  , corsAllowsRemoteOrigins
  , localImplicitBootstrapActive
  , localImplicitBootstrapAllowed
  , localImplicitBootstrapExposesRemote
  , localImplicitBootstrapStartupError
  , authStaticBearerEnabled
  , authStaticBearerToken
  , authMcpProvenanceToken
  ) where

import Control.Applicative ((<|>))
import Control.Exception (throwIO)
import Control.Monad (guard)
import Data.Aeson (FromJSON(..), ToJSON(..), (.:), (.:?), (.!=), (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.Char (isAlphaNum, isDigit, isHexDigit)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Yaml qualified as Yaml
import System.Environment (lookupEnv)
import System.Directory (getHomeDirectory, doesFileExist, createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)
import Text.Read (readMaybe)

------------------------------------------------------------------------
-- Config types
------------------------------------------------------------------------

data ServerConfig = ServerConfig
  { port :: !Int
  , host :: !Text
  } deriving stock (Show, Eq)

data DatabaseConfig = DatabaseConfig
  { host     :: !Text
  , port     :: !Int
  , name     :: !Text
  , user     :: !(Maybe Text)
  , password :: !(Maybe Text)
  , sslmode  :: !(Maybe Text)
  } deriving stock (Show, Eq)

data PoolConfig = PoolConfig
  { size               :: !Int
  , idleTimeout        :: !Double
  , statementTimeoutMs :: !Int
  } deriving stock (Show, Eq)

data LogConfig = LogConfig
  { level       :: !Text
  , maxSizeMB   :: !Int
  , backupCount :: !Int
  } deriving stock (Show, Eq)

data CorsConfig = CorsConfig
  { allowedOrigins :: ![Text]
  } deriving stock (Show, Eq)

data AuthMode
  = AuthModeLocal
  | AuthModeDeployed
  deriving stock (Show, Eq)

data TokenLookupMode
  = TokenLookupDatabase
  deriving stock (Show, Eq)

data LocalBotTokenConfig = LocalBotTokenConfig
  { label :: !Text
  , token :: !Text
  } deriving stock (Show, Eq)

data LocalAuthConfig = LocalAuthConfig
  { bootstrapEnabled      :: !Bool
  , allowRemoteBootstrap :: !Bool
  , botTokens            :: ![LocalBotTokenConfig]
  } deriving stock (Show, Eq)

data DeployedAuthConfig = DeployedAuthConfig
  { issuer      :: !(Maybe Text)
  , audience    :: !(Maybe Text)
  , discoveryUrl :: !(Maybe Text)
  , jwksUrl     :: !(Maybe Text)
  , jwks        :: !(Maybe Aeson.Value)
  , tokenLookup :: !TokenLookupMode
  , tokenHashSecret :: !(Maybe Text)
  , clientId    :: !(Maybe Text)
  , clientSecret :: !(Maybe Text)
  , redirectUri :: !(Maybe Text)
  , scopes      :: ![Text]
  , authorizationEndpoint :: !(Maybe Text)
  , tokenEndpoint :: !(Maybe Text)
  , sessionCookieName :: !Text
  , csrfCookieName :: !Text
  , csrfHeaderName :: !Text
  , sessionTtlSeconds :: !Int
  , cookieSecure :: !Bool
  , cookieSameSite :: !Text
  } deriving stock (Show, Eq)

data AuthConfig = AuthConfig
  { mode     :: !AuthMode
  , enabled  :: !Bool
  , apiKey   :: !(Maybe Text)
  -- | Private credential for the MCP-to-server hop, distinct from user bearers.
  , mcpProvenanceToken :: !(Maybe Text)
  , local    :: !LocalAuthConfig
  , deployed :: !DeployedAuthConfig
  } deriving stock (Show, Eq)

data RateLimitConfig = RateLimitConfig
  { rlEnabled           :: !Bool
  , rlRequestsPerSecond :: !Double
  , rlBurst             :: !Int
  } deriving stock (Show, Eq)

-- | Retention and hand-off lifetimes for the database-backed change stream.
-- These values are intentionally expressed in seconds because they are also
-- available as environment overrides in container deployments.
data ChangeStreamConfig = ChangeStreamConfig
  { retentionSeconds       :: !Int
  , resumeTokenTtlSeconds  :: !Int
  , snapshotSessionTtlSeconds :: !Int
  } deriving stock (Show, Eq)

-- | A deliberately closed set of embedding providers.  The managed profile
-- is pinned by @config/managed-embedding-provenance.yaml@; HTTP exists only
-- for an operator-managed compatible TEI endpoint in that same vector space.
data EmbeddingProviderMode
  = EmbeddingProviderDisabled
  | EmbeddingProviderManagedTei
  | EmbeddingProviderHttp
  deriving stock (Show, Eq)

-- | Connection policy for the provider-neutral embedding boundary.  The
-- custom 'Show' instance never renders a configured endpoint, preventing a
-- malformed endpoint containing a secret from being emitted in logs.
data EmbeddingProviderConfig = EmbeddingProviderConfig
  { mode             :: !EmbeddingProviderMode
  , endpoint         :: !(Maybe Text)
  , batchSize        :: !Int
  , timeoutMs        :: !Int
  , retryAttempts    :: !Int
  , spaceFingerprint :: !Text
  } deriving stock (Eq)

-- | The structural authority extracted from an embedding endpoint.  Keeping
-- this separate from the raw URL lets the managed provider make an exact host
-- decision instead of relying on a string prefix.
data EmbeddingEndpointAuthority = EmbeddingEndpointAuthority
  { scheme :: !Text
  , host :: !Text
  , endpointPath :: !Text
  } deriving stock (Show, Eq)

instance Show EmbeddingProviderConfig where
  show provider =
    "EmbeddingProviderConfig {mode = " <> show provider.mode
      <> ", endpoint = " <> if isJust provider.endpoint then "<configured>" else "<none>"
      <> ", batchSize = " <> show provider.batchSize
      <> ", timeoutMs = " <> show provider.timeoutMs
      <> ", retryAttempts = " <> show provider.retryAttempts
      <> ", spaceFingerprint = " <> show provider.spaceFingerprint
      <> "}"

managedTeiModelId :: Text
managedTeiModelId = "Alibaba-NLP/gte-Qwen2-1.5B-instruct"

-- | The one supported vector space: immutable model revision, output
-- dimension, and the explicitly selected noncausal attention semantics.
managedTeiSpaceFingerprint :: Text
managedTeiSpaceFingerprint =
  "Alibaba-NLP/gte-Qwen2-1.5B-instruct@1cad2ab3ff41c2671f34e135d29831368ee26b68:1536:attention=noncausal:v1"

-- Historical vectors using this unqualified identity have unknown attention
-- provenance.  Keep the value recognizable for an actionable rejection, but
-- never reinterpret it as the selected noncausal space.
historicalManagedTeiSpaceFingerprint :: Text
historicalManagedTeiSpaceFingerprint =
  "Alibaba-NLP/gte-Qwen2-1.5B-instruct@1cad2ab3ff41c2671f34e135d29831368ee26b68:1536"

containsHistoricalManagedTeiSpaceFingerprint :: Aeson.Value -> Bool
containsHistoricalManagedTeiSpaceFingerprint (Aeson.Object root) =
  case KeyMap.lookup "embedding" root of
    Just (Aeson.Object embedding) ->
      KeyMap.lookup "space_fingerprint" embedding
        == Just (Aeson.String historicalManagedTeiSpaceFingerprint)
    _ -> False
containsHistoricalManagedTeiSpaceFingerprint _ = False

data TlsConfig = TlsConfig
  { tlsCertFile :: !(Maybe FilePath)
  , tlsKeyFile  :: !(Maybe FilePath)
  } deriving stock (Show, Eq)

data WebConfig = WebConfig
  { webEnabled  :: !Bool
  , webStaticDir :: !(Maybe FilePath)
  } deriving stock (Show, Eq)

data HMemConfig = HMemConfig
  { server   :: !ServerConfig
  , database :: !DatabaseConfig
  , pool     :: !PoolConfig
  , logging  :: !LogConfig
  , cors     :: !CorsConfig
  , auth     :: !AuthConfig
  , rateLimit :: !RateLimitConfig
  , changeStream :: !ChangeStreamConfig
  , embeddingProvider :: !EmbeddingProviderConfig
  , tls      :: !TlsConfig
  , web      :: !WebConfig
  } deriving stock (Show, Eq)

------------------------------------------------------------------------
-- JSON / YAML instances
------------------------------------------------------------------------

instance FromJSON ServerConfig where
  parseJSON = Aeson.withObject "ServerConfig" $ \o -> ServerConfig
    <$> o .:? "port" .!= 8420
    <*> o .:? "host" .!= "127.0.0.1"

instance ToJSON ServerConfig where
  toJSON sc = Aeson.object ["port" .= sc.port, "host" .= sc.host]

instance FromJSON DatabaseConfig where
  parseJSON = Aeson.withObject "DatabaseConfig" $ \o -> DatabaseConfig
    <$> o .:? "host" .!= "127.0.0.1"
    <*> o .:? "port" .!= 54320
    <*> o .:? "name" .!= "hmem"
    <*> o .:? "user"
    <*> o .:? "password"
    <*> o .:? "sslmode"

instance ToJSON DatabaseConfig where
  toJSON dc = Aeson.object $ concat
    [ [ "host" .= dc.host
      , "port" .= dc.port
      , "name" .= dc.name
      ]
    , maybe [] (\u -> ["user" .= u]) dc.user
    , maybe [] (\p -> ["password" .= p]) dc.password
    , maybe [] (\s -> ["sslmode" .= s]) dc.sslmode
    ]

instance FromJSON PoolConfig where
  parseJSON = Aeson.withObject "PoolConfig" $ \o -> PoolConfig
    <$> o .:? "size" .!= 10
    <*> o .:? "idle_timeout" .!= 60
    <*> o .:? "statement_timeout_ms" .!= 30000

instance ToJSON PoolConfig where
  toJSON pc = Aeson.object
    [ "size" .= pc.size
    , "idle_timeout" .= pc.idleTimeout
    , "statement_timeout_ms" .= pc.statementTimeoutMs
    ]

instance FromJSON LogConfig where
  parseJSON = Aeson.withObject "LogConfig" $ \o -> LogConfig
    <$> o .:? "level"          .!= "info"
    <*> o .:? "max_size_mb"   .!= 10
    <*> o .:? "backup_count"  .!= 5

instance ToJSON LogConfig where
  toJSON lc = Aeson.object
    [ "level"        .= lc.level
    , "max_size_mb"   .= lc.maxSizeMB
    , "backup_count"  .= lc.backupCount
    ]

instance FromJSON CorsConfig where
  parseJSON = Aeson.withObject "CorsConfig" $ \o -> CorsConfig
    <$> o .:? "allowed_origins" .!= defCorsOrigins

instance ToJSON CorsConfig where
  toJSON cc = Aeson.object
    [ "allowed_origins" .= cc.allowedOrigins
    ]

instance FromJSON AuthMode where
  parseJSON = Aeson.withText "AuthMode" $ \case
    "local"    -> pure AuthModeLocal
    "deployed" -> pure AuthModeDeployed
    other       -> fail $ "invalid auth mode: " <> T.unpack other

instance ToJSON AuthMode where
  toJSON = \case
    AuthModeLocal    -> Aeson.String "local"
    AuthModeDeployed -> Aeson.String "deployed"

instance FromJSON TokenLookupMode where
  parseJSON = Aeson.withText "TokenLookupMode" $ \case
    "database" -> pure TokenLookupDatabase
    other       -> fail $ "invalid token lookup mode: " <> T.unpack other

instance ToJSON TokenLookupMode where
  toJSON TokenLookupDatabase = Aeson.String "database"

instance FromJSON LocalBotTokenConfig where
  parseJSON = Aeson.withObject "LocalBotTokenConfig" $ \o -> LocalBotTokenConfig
    <$> o .: "label"
    <*> o .: "token"

instance ToJSON LocalBotTokenConfig where
  toJSON bot = Aeson.object
    [ "label" .= bot.label
    , "token" .= bot.token
    ]

instance FromJSON LocalAuthConfig where
  parseJSON = Aeson.withObject "LocalAuthConfig" $ \o -> LocalAuthConfig
    <$> o .:? "bootstrap_enabled" .!= True
    <*> o .:? "allow_remote_bootstrap" .!= False
    <*> o .:? "bot_tokens" .!= []

instance ToJSON LocalAuthConfig where
  toJSON localCfg = Aeson.object
    [ "bootstrap_enabled" .= localCfg.bootstrapEnabled
    , "allow_remote_bootstrap" .= localCfg.allowRemoteBootstrap
    , "bot_tokens" .= localCfg.botTokens
    ]

instance FromJSON DeployedAuthConfig where
  parseJSON = Aeson.withObject "DeployedAuthConfig" $ \o -> DeployedAuthConfig
    <$> o .:? "issuer"
    <*> o .:? "audience"
    <*> o .:? "discovery_url"
    <*> o .:? "jwks_url"
    <*> o .:? "jwks"
    <*> o .:? "token_lookup" .!= TokenLookupDatabase
    <*> o .:? "token_hash_secret"
    <*> o .:? "client_id"
    <*> o .:? "client_secret"
    <*> o .:? "redirect_uri"
    <*> o .:? "scopes" .!= ["openid", "profile", "email"]
    <*> o .:? "authorization_endpoint"
    <*> o .:? "token_endpoint"
    <*> o .:? "session_cookie_name" .!= "hmem_session"
    <*> o .:? "csrf_cookie_name" .!= "hmem_csrf"
    <*> o .:? "csrf_header_name" .!= "X-CSRF-Token"
    <*> o .:? "session_ttl_seconds" .!= 28800
    <*> o .:? "cookie_secure" .!= True
    <*> o .:? "cookie_same_site" .!= "Lax"

instance ToJSON DeployedAuthConfig where
  toJSON deployedCfg = Aeson.object $ concat
    [ maybe [] (\v -> ["issuer" .= v]) deployedCfg.issuer
    , maybe [] (\v -> ["audience" .= v]) deployedCfg.audience
    , maybe [] (\v -> ["discovery_url" .= v]) deployedCfg.discoveryUrl
    , maybe [] (\v -> ["jwks_url" .= v]) deployedCfg.jwksUrl
    , maybe [] (\v -> ["jwks" .= v]) deployedCfg.jwks
    , ["token_lookup" .= deployedCfg.tokenLookup]
    , maybe [] (\v -> ["token_hash_secret" .= v]) deployedCfg.tokenHashSecret
    , maybe [] (\v -> ["client_id" .= v]) deployedCfg.clientId
    , maybe [] (\v -> ["client_secret" .= v]) deployedCfg.clientSecret
    , maybe [] (\v -> ["redirect_uri" .= v]) deployedCfg.redirectUri
    , ["scopes" .= deployedCfg.scopes]
    , maybe [] (\v -> ["authorization_endpoint" .= v]) deployedCfg.authorizationEndpoint
    , maybe [] (\v -> ["token_endpoint" .= v]) deployedCfg.tokenEndpoint
    , [ "session_cookie_name" .= deployedCfg.sessionCookieName
      , "csrf_cookie_name" .= deployedCfg.csrfCookieName
      , "csrf_header_name" .= deployedCfg.csrfHeaderName
      , "session_ttl_seconds" .= deployedCfg.sessionTtlSeconds
      , "cookie_secure" .= deployedCfg.cookieSecure
      , "cookie_same_site" .= deployedCfg.cookieSameSite
      ]
    ]

instance FromJSON AuthConfig where
  parseJSON = Aeson.withObject "AuthConfig" $ \o -> AuthConfig
    <$> o .:? "mode" .!= AuthModeLocal
    <*> o .:? "enabled" .!= False
    <*> o .:? "api_key"
    <*> (normalizeMcpProvenanceToken <$> o .:? "mcp_provenance_token")
    <*> o .:? "local" .!= defLocalAuth
    <*> o .:? "deployed" .!= defDeployedAuth

instance ToJSON AuthConfig where
  toJSON ac = Aeson.object $
    [ "mode" .= ac.mode
    , "enabled" .= ac.enabled
    , "local" .= ac.local
    , "deployed" .= ac.deployed
    ]
    <> maybe [] (\k -> ["api_key" .= k]) ac.apiKey
    <> maybe [] (\k -> ["mcp_provenance_token" .= k]) ac.mcpProvenanceToken

instance FromJSON RateLimitConfig where
  parseJSON = Aeson.withObject "RateLimitConfig" $ \o -> RateLimitConfig
    <$> o .:? "enabled" .!= False
    <*> o .:? "requests_per_second" .!= 10.0
    <*> o .:? "burst" .!= 20

instance ToJSON RateLimitConfig where
  toJSON rl = Aeson.object
    [ "enabled" .= rl.rlEnabled
    , "requests_per_second" .= rl.rlRequestsPerSecond
    , "burst" .= rl.rlBurst
    ]

instance FromJSON ChangeStreamConfig where
  parseJSON = Aeson.withObject "ChangeStreamConfig" $ \o -> ChangeStreamConfig
    <$> o .:? "retention_seconds" .!= 604800
    <*> o .:? "resume_token_ttl_seconds" .!= 86400
    <*> o .:? "snapshot_session_ttl_seconds" .!= 300

instance ToJSON ChangeStreamConfig where
  toJSON cs = Aeson.object
    [ "retention_seconds" .= cs.retentionSeconds
    , "resume_token_ttl_seconds" .= cs.resumeTokenTtlSeconds
    , "snapshot_session_ttl_seconds" .= cs.snapshotSessionTtlSeconds
    ]

instance FromJSON EmbeddingProviderMode where
  parseJSON = Aeson.withText "EmbeddingProviderMode" $ \case
    "disabled" -> pure EmbeddingProviderDisabled
    "managed-tei" -> pure EmbeddingProviderManagedTei
    "http" -> pure EmbeddingProviderHttp
    other -> fail $ "invalid embedding provider mode: " <> T.unpack other

instance ToJSON EmbeddingProviderMode where
  toJSON = \case
    EmbeddingProviderDisabled -> Aeson.String "disabled"
    EmbeddingProviderManagedTei -> Aeson.String "managed-tei"
    EmbeddingProviderHttp -> Aeson.String "http"

instance FromJSON EmbeddingProviderConfig where
  parseJSON = Aeson.withObject "EmbeddingProviderConfig" $ \o -> do
    provider <- EmbeddingProviderConfig
      <$> o .:? "mode" .!= EmbeddingProviderDisabled
      <*> o .:? "endpoint"
      <*> o .:? "batch_size" .!= 32
      <*> o .:? "timeout_ms" .!= 30000
      <*> o .:? "retry_attempts" .!= 0
      <*> o .:? "space_fingerprint" .!= managedTeiSpaceFingerprint
    either fail pure (validateEmbeddingProviderConfig provider)

instance ToJSON EmbeddingProviderConfig where
  toJSON provider = Aeson.object $
    [ "mode" .= provider.mode
    , "batch_size" .= provider.batchSize
    , "timeout_ms" .= provider.timeoutMs
    , "retry_attempts" .= provider.retryAttempts
    , "space_fingerprint" .= provider.spaceFingerprint
    ] <> maybe [] (\value -> ["endpoint" .= value]) provider.endpoint

instance FromJSON TlsConfig where
  parseJSON = Aeson.withObject "TlsConfig" $ \o -> TlsConfig
    <$> o .:? "cert_file"
    <*> o .:? "key_file"

instance ToJSON TlsConfig where
  toJSON tc = Aeson.object $ concat
    [ maybe [] (\c -> ["cert_file" .= c]) tc.tlsCertFile
    , maybe [] (\k -> ["key_file" .= k]) tc.tlsKeyFile
    ]

instance FromJSON WebConfig where
  parseJSON = Aeson.withObject "WebConfig" $ \o -> WebConfig
    <$> o .:? "enabled"    .!= True
    <*> o .:? "static_dir"

instance ToJSON WebConfig where
  toJSON wc = Aeson.object $
    [ "enabled" .= wc.webEnabled ]
    <> maybe [] (\d -> ["static_dir" .= d]) wc.webStaticDir

instance FromJSON HMemConfig where
  parseJSON = Aeson.withObject "HMemConfig" $ \o -> HMemConfig
    <$> o .:? "server"   .!= defServer
    <*> o .:? "database" .!= defDatabase
    <*> o .:? "pool"     .!= defPool
    <*> o .:? "logging"  .!= defLogging
    <*> o .:? "cors"     .!= defCors
    <*> o .:? "auth"     .!= defAuth
    <*> o .:? "rate_limit" .!= defRateLimit
    <*> o .:? "change_stream" .!= defChangeStream
    <*> o .:? "embedding" .!= defEmbeddingProvider
    <*> o .:? "tls"      .!= defTls
    <*> o .:? "web"      .!= defWeb

instance ToJSON HMemConfig where
  toJSON cfg = Aeson.object
    [ "server"   .= cfg.server
    , "database" .= cfg.database
    , "pool"     .= cfg.pool
    , "logging"  .= cfg.logging
    , "cors"     .= cfg.cors
    , "auth"     .= cfg.auth
    , "rate_limit" .= cfg.rateLimit
    , "change_stream" .= cfg.changeStream
    , "embedding" .= cfg.embeddingProvider
    , "tls"      .= cfg.tls
    , "web"      .= cfg.web
    ]

------------------------------------------------------------------------
-- Defaults
------------------------------------------------------------------------

defServer :: ServerConfig
defServer = ServerConfig { port = 8420, host = "127.0.0.1" }

defDatabase :: DatabaseConfig
defDatabase = DatabaseConfig { host = "127.0.0.1", port = 54320, name = "hmem", user = Nothing, password = Nothing, sslmode = Nothing }

defPool :: PoolConfig
defPool = PoolConfig { size = 10, idleTimeout = 60, statementTimeoutMs = 30000 }

defLogging :: LogConfig
defLogging = LogConfig { level = "info", maxSizeMB = 10, backupCount = 5 }

defCorsOrigins :: [Text]
defCorsOrigins = ["http://localhost", "http://127.0.0.1"]

defCors :: CorsConfig
defCors = CorsConfig { allowedOrigins = defCorsOrigins }

defLocalAuth :: LocalAuthConfig
defLocalAuth = LocalAuthConfig
  { bootstrapEnabled = True
  , allowRemoteBootstrap = False
  , botTokens = []
  }

defDeployedAuth :: DeployedAuthConfig
defDeployedAuth = DeployedAuthConfig
  { issuer = Nothing
  , audience = Nothing
  , discoveryUrl = Nothing
  , jwksUrl = Nothing
  , jwks = Nothing
  , tokenLookup = TokenLookupDatabase
  , tokenHashSecret = Nothing
  , clientId = Nothing
  , clientSecret = Nothing
  , redirectUri = Nothing
  , scopes = ["openid", "profile", "email"]
  , authorizationEndpoint = Nothing
  , tokenEndpoint = Nothing
  , sessionCookieName = "hmem_session"
  , csrfCookieName = "hmem_csrf"
  , csrfHeaderName = "X-CSRF-Token"
  , sessionTtlSeconds = 28800
  , cookieSecure = True
  , cookieSameSite = "Lax"
  }

defAuth :: AuthConfig
defAuth = AuthConfig
  { mode = AuthModeLocal
  , enabled = False
  , apiKey = Nothing
  , mcpProvenanceToken = Nothing
  , local = defLocalAuth
  , deployed = defDeployedAuth
  }

defRateLimit :: RateLimitConfig
defRateLimit = RateLimitConfig
  { rlEnabled = False
  , rlRequestsPerSecond = 10.0
  , rlBurst = 20
  }

defChangeStream :: ChangeStreamConfig
defChangeStream = ChangeStreamConfig
  { retentionSeconds = 604800
  , resumeTokenTtlSeconds = 86400
  , snapshotSessionTtlSeconds = 300
  }

defEmbeddingProvider :: EmbeddingProviderConfig
defEmbeddingProvider = EmbeddingProviderConfig
  { mode = EmbeddingProviderDisabled
  , endpoint = Nothing
  , batchSize = 32
  , timeoutMs = 30000
  , retryAttempts = 0
  , spaceFingerprint = managedTeiSpaceFingerprint
  }

defTls :: TlsConfig
defTls = TlsConfig { tlsCertFile = Nothing, tlsKeyFile = Nothing }

defWeb :: WebConfig
defWeb = WebConfig { webEnabled = True, webStaticDir = Nothing }

defaultConfig :: HMemConfig
defaultConfig = HMemConfig
  { server   = defServer
  , database = defDatabase
  , pool     = defPool
  , logging  = defLogging
  , cors     = defCors
  , auth     = defAuth
  , rateLimit = defRateLimit
  , changeStream = defChangeStream
  , embeddingProvider = defEmbeddingProvider
  , tls      = defTls
  , web      = defWeb
  }

------------------------------------------------------------------------
-- Paths
------------------------------------------------------------------------

-- | The hmem config directory: @~\/.hmem\/@
configDir :: IO FilePath
configDir = do
  home <- getHomeDirectory
  pure (home </> ".hmem")

-- | Path to the config file: @~\/.hmem\/config.yaml@
configFilePath :: IO FilePath
configFilePath = (</> "config.yaml") <$> configDir

------------------------------------------------------------------------
-- Load / Save
------------------------------------------------------------------------

-- | Load config from @~\/.hmem\/config.yaml@.
loadConfig :: IO HMemConfig
loadConfig = configFilePath >>= loadConfigFile

-- | Load config from an explicit path.  A missing or generally malformed file
-- uses 'defaultConfig'.  An explicitly configured historical embedding identity
-- is rejected before environment overrides can activate a rewritten provider.
-- Prints warnings for validation issues and clamps values to valid ranges.
loadConfigFile :: FilePath -> IO HMemConfig
loadConfigFile path = do
  exists <- doesFileExist path
  cfg <- if exists
    then do
      contents <- BS.readFile path
      let result = Yaml.decodeEither' contents
      case result of
        Left err -> do
          let untyped = Yaml.decodeEither' contents :: Either Yaml.ParseException Aeson.Value
          case untyped of
            Right value | containsHistoricalManagedTeiSpaceFingerprint value -> throwIO err
            _ -> do
              hPutStrLn stderr $
                "Warning: failed to parse " <> path <> ": " <> show err
              hPutStrLn stderr "Using default configuration."
              pure defaultConfig
        Right c -> pure c
    else pure defaultConfig
  envPassword <- fmap T.pack <$> lookupEnv "HMEM_DB_PASSWORD"
  envApiKey   <- fmap T.pack <$> lookupEnv "HMEM_API_KEY"
  envMcpProvenanceToken <- fmap T.pack <$> lookupEnv "HMEM_MCP_PROVENANCE_TOKEN"
  envSslMode  <- fmap T.pack <$> lookupEnv "HMEM_DB_SSLMODE"
  envRetention <- lookupEnv "HMEM_CHANGE_STREAM_OUTBOX_RETENTION_SECONDS"
  envResumeTtl <- lookupEnv "HMEM_CHANGE_STREAM_RESUME_TOKEN_TTL_SECONDS"
  envSessionTtl <- lookupEnv "HMEM_CHANGE_STREAM_SNAPSHOT_SESSION_TTL_SECONDS"
  envEmbeddingMode <- fmap T.pack <$> lookupEnv "HMEM_EMBEDDING_PROVIDER"
  envEmbeddingEndpoint <- fmap T.pack <$> lookupEnv "HMEM_EMBEDDING_ENDPOINT"
  let cfg' = applyChangeStreamEnvOverrides envRetention envResumeTtl envSessionTtl
           $ applyEnvOverrides envPassword envApiKey envSslMode cfg
      cfg'' = applyMcpProvenanceEnvOverride envMcpProvenanceToken cfg'
  cfg''' <- case applyEmbeddingProviderEnvOverrides envEmbeddingMode envEmbeddingEndpoint cfg'' of
    Left err -> hPutStrLn stderr ("Config warning: " <> err <> "; retaining file configuration") >> pure cfg''
    Right overridden -> pure overridden
  let (warnings, validated) = validateConfig cfg'''
  mapM_ (\w -> hPutStrLn stderr $ "Config warning: " <> w) warnings
  pure validated

-- | Write config to @~\/.hmem\/config.yaml@.
saveConfig :: HMemConfig -> IO ()
saveConfig cfg = do
  dir <- configDir
  createDirectoryIfMissing True dir
  path <- configFilePath
  Yaml.encodeFile path cfg

-- | Apply environment-driven overrides after loading the file config.
-- Supports @HMEM_DB_PASSWORD@, @HMEM_API_KEY@, and @HMEM_DB_SSLMODE@,
-- each taking precedence over the corresponding value in @config.yaml@
-- when set.
applyEnvOverrides :: Maybe Text -> Maybe Text -> Maybe Text -> HMemConfig -> HMemConfig
applyEnvOverrides mDbPassword mApiKey mDbSslMode cfg =
  cfg
    { database = cfg.database
        { password = mDbPassword <|> cfg.database.password
        , sslmode  = mDbSslMode  <|> cfg.database.sslmode
        }
    , auth = cfg.auth
        { apiKey = mApiKey <|> cfg.auth.apiKey
        }
    }

applyChangeStreamEnvOverrides :: Maybe String -> Maybe String -> Maybe String -> HMemConfig -> HMemConfig
applyChangeStreamEnvOverrides mRetention mResumeTtl mSessionTtl cfg = cfg
  { changeStream = cfg.changeStream
      { retentionSeconds = fromMaybe cfg.changeStream.retentionSeconds (mRetention >>= readMaybe)
      , resumeTokenTtlSeconds = fromMaybe cfg.changeStream.resumeTokenTtlSeconds (mResumeTtl >>= readMaybe)
      , snapshotSessionTtlSeconds = fromMaybe cfg.changeStream.snapshotSessionTtlSeconds (mSessionTtl >>= readMaybe)
      }
  }

-- | Blank private credentials are not credentials.  Strip YAML/environment
-- values once so every consumer fails closed on whitespace-only configuration.
applyMcpProvenanceEnvOverride :: Maybe Text -> HMemConfig -> HMemConfig
applyMcpProvenanceEnvOverride envToken cfg = cfg
  { auth = cfg.auth
      { mcpProvenanceToken = normalizeMcpProvenanceToken envToken <|> normalizeMcpProvenanceToken cfg.auth.mcpProvenanceToken
      }
  }

normalizeMcpProvenanceToken :: Maybe Text -> Maybe Text
normalizeMcpProvenanceToken = (>>= nonEmpty . T.strip)
  where
    nonEmpty value
      | T.null value = Nothing
      | otherwise = Just value

-- | Environment overrides are parsed before use.  An invalid override cannot
-- silently select another provider: callers receive 'Left' and retain the
-- previously validated file configuration.
applyEmbeddingProviderEnvOverrides
  :: Maybe Text
  -> Maybe Text
  -> HMemConfig
  -> Either String HMemConfig
applyEmbeddingProviderEnvOverrides envMode envEndpoint cfg = do
  selectedMode <- case envMode of
    Nothing -> Right cfg.embeddingProvider.mode
    Just value -> parseEmbeddingProviderMode value
  let fileEndpoint
        | envMode /= Nothing && selectedMode /= EmbeddingProviderHttp = Nothing
        | otherwise = cfg.embeddingProvider.endpoint
      selectedEndpoint = envEndpoint <|> fileEndpoint
      provider = cfg.embeddingProvider { mode = selectedMode, endpoint = selectedEndpoint }
  _ <- validateEmbeddingProviderConfig provider
  pure cfg { embeddingProvider = provider }

parseEmbeddingProviderMode :: Text -> Either String EmbeddingProviderMode
parseEmbeddingProviderMode value = case T.toLower (T.strip value) of
  "disabled" -> Right EmbeddingProviderDisabled
  "managed-tei" -> Right EmbeddingProviderManagedTei
  "http" -> Right EmbeddingProviderHttp
  _ -> Left "HMEM_EMBEDDING_PROVIDER must be one of disabled, managed-tei, or http"

validateEmbeddingProviderConfig :: EmbeddingProviderConfig -> Either String EmbeddingProviderConfig
validateEmbeddingProviderConfig provider
  | provider.spaceFingerprint == historicalManagedTeiSpaceFingerprint =
      Left $ "embedding.space_fingerprint identifies the historical GTE-Qwen2 space with unknown attention semantics; configure "
        <> T.unpack managedTeiSpaceFingerprint <> " and recompute embeddings"
  | provider.spaceFingerprint /= managedTeiSpaceFingerprint =
      Left "embedding.space_fingerprint must identify the pinned noncausal Alibaba-NLP/gte-Qwen2-1.5B-instruct 1536-dimensional space"
  | provider.batchSize < 1 || provider.batchSize > 256 =
      Left "embedding.batch_size must be between 1 and 256"
  | provider.timeoutMs < 100 || provider.timeoutMs > 300000 =
      Left "embedding.timeout_ms must be between 100 and 300000"
  | provider.retryAttempts < 0 || provider.retryAttempts > 5 =
      Left "embedding.retry_attempts must be between 0 and 5"
  | provider.mode == EmbeddingProviderDisabled && provider.endpoint /= Nothing =
      Left "embedding.endpoint is not permitted when embedding.mode is disabled"
  | provider.mode == EmbeddingProviderManagedTei && provider.endpoint /= Nothing =
      Left "embedding.endpoint is supplied by the managed supervisor and must not be configured"
  | provider.mode == EmbeddingProviderHttp && provider.endpoint == Nothing =
      Left "embedding.endpoint is required when embedding.mode is http"
  | Just value <- provider.endpoint, not (validEmbeddingEndpoint value) =
      Left "embedding.endpoint must be an absolute http or https URL with a host and without credentials, query, or fragment"
  | otherwise = Right provider

validEmbeddingEndpoint :: Text -> Bool
validEmbeddingEndpoint raw = case parseEmbeddingEndpointAuthority raw of
  Just authority -> embeddingEndpointPathAllowed authority.endpointPath
  Nothing -> False

-- | The embedding provider exposes one fixed TEI route. Keep this rule in the
-- dependency-neutral configuration module because both provider selection and
-- the HTTP adapter must apply the identical restriction.
embeddingEndpointPathAllowed :: Text -> Bool
embeddingEndpointPathAllowed path = path `elem` ["", "/", "/embed", "/embed/"]

-- | Validate the restricted endpoint shape and canonicalize it to the fixed
-- @/embed@ route. Nothing is returned for malformed URLs or arbitrary paths.
normalizeEmbeddingEndpointRoute :: Text -> Maybe Text
normalizeEmbeddingEndpointRoute raw = do
  authority <- parseEmbeddingEndpointAuthority raw
  guard (embeddingEndpointPathAllowed authority.endpointPath)
  let endpoint = T.strip raw
      base = T.dropEnd (T.length authority.endpointPath) endpoint
  pure (T.dropWhileEnd (== '/') base <> "/embed")

-- | Parse the restricted absolute URL form accepted by embedding providers.
-- Queries, fragments and user-info are excluded because they are not part of
-- the fixed TEI route.  The host is returned without IPv6 brackets and is
-- lower-cased for exact authority comparisons.
parseEmbeddingEndpointAuthority :: Text -> Maybe EmbeddingEndpointAuthority
parseEmbeddingEndpointAuthority raw = do
  let value = T.strip raw
      (rawScheme, afterScheme) = T.breakOn "://" value
  guard (not (T.null rawScheme) && afterScheme /= value)
  let normalizedScheme = T.toLower rawScheme
  guard (normalizedScheme `elem` ["http", "https"])
  let afterPrefix = T.drop 3 afterScheme
      (authority, suffix) = T.break (`elem` ['/', '?', '#']) afterPrefix
  guard (not (T.null authority))
  guard (T.null suffix || T.head suffix == '/')
  guard (not (T.any (`elem` ['@', '?', '#']) value))
  guard (not (T.any (`elem` [' ', '\t', '\r', '\n']) value))
  parsedHost <- parseAuthority authority
  pure EmbeddingEndpointAuthority
    { scheme = normalizedScheme
    , host = T.toLower parsedHost
    , endpointPath = suffix
    }
  where
    parseAuthority authority
      | T.head authority == '[' = do
          let (inside, remainder) = T.breakOn "]" (T.tail authority)
          guard (not (T.null inside) && not (T.null remainder))
          validPortSuffix (T.drop 1 remainder)
          guard (validIpv6 inside)
          pure inside
      | otherwise = do
          let (candidateHost, portSuffix) = T.breakOn ":" authority
          guard (not (T.null candidateHost))
          guard (T.count ":" authority <= 1)
          validPortSuffix portSuffix
          guard (validHost candidateHost)
          pure candidateHost

    validHost candidate
      | T.any (== '.') candidate && T.all (\c -> isDigit c || c == '.') candidate = validIpv4 candidate
      | otherwise = validDnsHostname candidate

    validIpv4 candidate =
      let octets = T.splitOn "." candidate
      in length octets == 4 && all validOctet octets

    validOctet octet =
      not (T.null octet)
        && T.all isDigit octet
        && (T.length octet == 1 || T.head octet /= '0')
        && (read (T.unpack octet) :: Integer) <= 255

    validDnsHostname candidate =
      not (T.null candidate)
        && T.length candidate <= 253
        && all validLabel (T.splitOn "." candidate)

    validLabel label =
      not (T.null label)
        && T.length label <= 63
        && T.head label /= '-'
        && T.last label /= '-'
        && T.all (\c -> isAlphaNum c || c == '-') label

    validIpv6 candidate = case T.splitOn "::" candidate of
      [part] -> validIpv6Parts (splitIpv6Part part) && ipv6Units (splitIpv6Part part) == 8
      [leftPart, rightPart] ->
        let segments = splitIpv6Part leftPart <> splitIpv6Part rightPart
        in validIpv6Parts segments && ipv6Units segments < 8
      _ -> False

    splitIpv6Part part
      | T.null part = []
      | otherwise = T.splitOn ":" part

    validIpv6Parts segments = case reverse segments of
      [] -> True
      finalSegment : reversedInitial ->
        let initial = reverse reversedInitial
            embeddedIpv4 = T.any (== '.') finalSegment
        in all validHexGroup initial
            && (if embeddedIpv4 then validIpv4 finalSegment else validHexGroup finalSegment)
            && not (any (T.any (== '.')) initial)

    ipv6Units segments = case reverse segments of
      [] -> 0
      finalSegment : reversedInitial ->
        length reversedInitial + if T.any (== '.') finalSegment then 2 else 1

    validHexGroup group =
      not (T.null group) && T.length group <= 4 && T.all isHexDigit group

    validPortSuffix suffix
      | T.null suffix = Just ()
      | otherwise = do
          portText <- T.stripPrefix ":" suffix
          guard (not (T.null portText) && T.all isDigit portText)
          let port = read (T.unpack portText) :: Integer
          guard (port >= 1 && port <= 65535)

------------------------------------------------------------------------
-- Validation
------------------------------------------------------------------------

-- | Validate and normalise a config.  Returns a list of warnings
-- (empty if everything is fine) and the corrected config.
validateConfig :: HMemConfig -> ([String], HMemConfig)
validateConfig cfg = (warnings, corrected)
  where
    (srvWarns, srv) = validateServer cfg.server
    (dbWarns,  db)  = validateDatabase cfg.database
    (plWarns,  pl)  = validatePool cfg.pool
    (lgWarns,  lg)  = validateLog cfg.logging
    (auWarns,  au)  = validateAuth srv cfg.auth
    (rlWarns,  rl)  = validateRateLimit cfg.rateLimit
    (csWarns, cs) = validateChangeStream cfg.changeStream
    warnings  = srvWarns <> dbWarns <> plWarns <> lgWarns <> auWarns <> rlWarns <> csWarns
    corrected = cfg { server = srv, database = db, pool = pl, logging = lg, auth = au, rateLimit = rl, changeStream = cs }

    validateServer s =
      let (ws, p) = clampField "server.port" 1 65535 s.port
      in  (ws, ServerConfig { port = p, host = s.host })

    validateDatabase d =
      let (ws1, p)  = clampField "database.port" 1 65535 d.port
          ws2 = ["database.name is empty; using default 'hmem'" | T.null d.name]
          ws3 = ["database.host is empty; using default '127.0.0.1'" | T.null d.host]
          d'  = DatabaseConfig
                  { host     = if T.null d.host then "127.0.0.1" else d.host
                  , port     = p
                  , name     = if T.null d.name then "hmem" else d.name
                  , user     = d.user
                  , password = d.password
                  , sslmode  = d.sslmode
                  }
      in  (ws1 <> ws2 <> ws3, d')

    validatePool p =
      let (ws1, sz) = clampField "pool.size" 1 1000 p.size
          (ws2, it) = clampFieldD "pool.idle_timeout" 1.0 3600.0 p.idleTimeout
          (ws3, st) = clampField "pool.statement_timeout_ms" 1000 300000 p.statementTimeoutMs
      in  (ws1 <> ws2 <> ws3, PoolConfig { size = sz, idleTimeout = it, statementTimeoutMs = st })

    validateLog l =
      let (ws1, ms) = clampField "logging.max_size_mb" 1 10000 l.maxSizeMB
          (ws2, bc) = clampField "logging.backup_count" 0 100 l.backupCount
      in  (ws1 <> ws2, LogConfig { level = l.level, maxSizeMB = ms, backupCount = bc })

    validateAuth s a = (missingLegacyWarn <> blankMcpProvenanceWarn <> localExposureWarns <> deployedWarns, a { mcpProvenanceToken = normalizeMcpProvenanceToken a.mcpProvenanceToken })
      where
        blankMcpProvenanceWarn =
          ["auth.mcp_provenance_token is empty or whitespace-only; ignoring it"
          | maybe False (T.null . T.strip) a.mcpProvenanceToken]
        missingLegacyWarn
          | a.mode == AuthModeLocal
          , a.enabled
          , Nothing <- a.apiKey =
              ["auth.enabled is true but no auth.api_key or HMEM_API_KEY is configured; current runtime will not enable legacy static bearer auth until richer mode-specific auth is implemented"]
          | otherwise = []

        localExposureWarns
          | a.mode /= AuthModeLocal = []
          | not a.local.bootstrapEnabled = []
          | null exposureReasons = []
          | a.local.allowRemoteBootstrap = map escapeHatchWarning exposureReasons
          | otherwise =
              [ "auth.local.bootstrap_enabled is true in local mode with " <> joinedReasons <> "; implicit local superadmin is loopback/CORS-local only by default and startup validation will refuse this unless auth.local.allow_remote_bootstrap is true" ]
          where
            exposureReasons = localExposureReasons s cfg.cors
            joinedReasons = joinReasons exposureReasons

            escapeHatchWarning reason =
              "auth.local.allow_remote_bootstrap is true with " <> reason <> "; implicit local superadmin may be reachable by remote clients; only use this on trusted private networks"

        deployedWarns
          | a.mode /= AuthModeDeployed = []
          | otherwise = concat
              [ ["auth.mode is deployed but JWT issuer is not configured; provider JWTs will not resolve" | a.deployed.issuer == Nothing]
              , ["auth.mode is deployed but JWT audience is not configured; provider JWTs will not resolve" | a.deployed.audience == Nothing]
              , ["auth.mode is deployed but no discovery_url, jwks_url, or inline jwks is configured; provider JWTs will not resolve" | noJwksSource]
              , ["auth.deployed.discovery_url should use https" | maybe False (not . T.isPrefixOf "https://") a.deployed.discoveryUrl]
              , ["auth.deployed.jwks_url should use https" | maybe False (not . T.isPrefixOf "https://") a.deployed.jwksUrl]
              , ["auth.deployed.client_id is not configured; OIDC browser login will not start" | a.deployed.clientId == Nothing]
              , ["auth.deployed.client_secret is not configured; OIDC code exchange will fail" | a.deployed.clientSecret == Nothing]
              , ["auth.deployed.redirect_uri is not configured; OIDC browser login will not start" | a.deployed.redirectUri == Nothing]
              , ["auth.deployed.authorization_endpoint should use https" | maybe False (not . T.isPrefixOf "https://") a.deployed.authorizationEndpoint]
              , ["auth.deployed.token_endpoint should use https" | maybe False (not . T.isPrefixOf "https://") a.deployed.tokenEndpoint]
              , ["auth.deployed.cookie_same_site should be one of Lax, Strict, or None; runtime cookie emission will fall back to Lax" | not validSameSite]
              , ["auth.deployed.cookie_same_site=None requires auth.deployed.cookie_secure=true" | T.toLower a.deployed.cookieSameSite == "none" && not a.deployed.cookieSecure]
              , ["auth.deployed.session_ttl_seconds is below 60; browser sessions will expire quickly" | a.deployed.sessionTtlSeconds < 60]
              , ["auth.mode is deployed with credentialed CORS remote/wildcard origins; restrict cors.allowed_origins to trusted frontend origins before enabling browser cookie sessions" | corsAllowsRemoteOrigins cfg.cors]
              ]
          where
            noJwksSource = a.deployed.discoveryUrl == Nothing && a.deployed.jwksUrl == Nothing && a.deployed.jwks == Nothing
            validSameSite = T.toLower (T.strip a.deployed.cookieSameSite) `elem` ["lax", "strict", "none"]

    validateRateLimit rateLimitCfg =
      let (ws1, rps) = clampFieldD "rate_limit.requests_per_second" 0.1 10000.0 rateLimitCfg.rlRequestsPerSecond
          (ws2, burst) = clampField "rate_limit.burst" 1 100000 rateLimitCfg.rlBurst
      in (ws1 <> ws2, RateLimitConfig
        { rlEnabled = rateLimitCfg.rlEnabled
        , rlRequestsPerSecond = rps
        , rlBurst = burst
        })

    validateChangeStream streamCfg =
      let -- Three seconds is the smallest retention that can contain two
          -- strictly positive hand-off lifetimes and still leave one second
          -- of replay budget.  Clamp the session first, then derive the
          -- resume maximum from it, which makes the invariant constructive.
          (wRetention, retention) = clampField "change_stream.retention_seconds" 3 31536000 streamCfg.retentionSeconds
          sessionMax = retention - 2
          (wSession, session) = clampField "change_stream.snapshot_session_ttl_seconds" 1 sessionMax streamCfg.snapshotSessionTtlSeconds
          resumeMax = retention - session - 1
          (wResume, resume) = clampField "change_stream.resume_token_ttl_seconds" 1 resumeMax streamCfg.resumeTokenTtlSeconds
      in (wRetention <> wSession <> wResume, ChangeStreamConfig retention resume session)

    clampField :: String -> Int -> Int -> Int -> ([String], Int)
    clampField name lo hi val
      | val < lo  = ([name <> ": " <> show val <> " is below minimum " <> show lo <> "; using " <> show lo], lo)
      | val > hi  = ([name <> ": " <> show val <> " exceeds maximum " <> show hi <> "; using " <> show hi], hi)
      | otherwise = ([], val)

    clampFieldD :: String -> Double -> Double -> Double -> ([String], Double)
    clampFieldD name lo hi val
      | val < lo  = ([name <> ": " <> show val <> " is below minimum " <> show lo <> "; using " <> show lo], lo)
      | val > hi  = ([name <> ": " <> show val <> " exceeds maximum " <> show hi <> "; using " <> show hi], hi)
      | otherwise = ([], val)

    showText txt = "'" <> T.unpack txt <> "'"

    joinReasons = \case
      []       -> ""
      [x]      -> x
      x : rest -> x <> concatMap (", " <>) rest

    localExposureReasons s corsCfg = concat
      [ ["server.host is " <> showText s.host | not (serverHostIsLoopback s.host)]
      , ["cors.allowed_origins permits remote origins" | corsAllowsRemoteOrigins corsCfg]
      ]

------------------------------------------------------------------------
-- Derived helpers
------------------------------------------------------------------------

-- | Build a libpq connection string from the database config.
-- Values are single-quoted and internal single quotes are escaped
-- per libpq convention ('' → \') to handle special characters.
connectionString :: DatabaseConfig -> Text
connectionString dc = T.intercalate " " $ concat
  [ [ "host='" <> escPq dc.host <> "'"
    , "port=" <> T.pack (show dc.port)
    , "dbname='" <> escPq dc.name <> "'"
    ]
  , maybe [] (\u -> ["user='" <> escPq u <> "'"]) dc.user
  , maybe [] (\p -> ["password='" <> escPq p <> "'"]) dc.password
  , maybe [] (\s -> ["sslmode='" <> escPq s <> "'"]) dc.sslmode
  ]
  where
    escPq = T.replace "\\" "\\\\" . T.replace "'" "\\'"

-- | Build the server base URL, e.g. @http:\/\/127.0.0.1:8420@.
serverUrl :: ServerConfig -> Text
serverUrl sc = "http://" <> sc.host <> ":" <> T.pack (show sc.port)

-- | Whether a configured host is loopback-only for the local-mode implicit
-- superadmin safety policy.
serverHostIsLoopback :: Text -> Bool
serverHostIsLoopback host =
  let h = T.toLower (T.strip host)
  in h == "localhost"
    || h == "::1"
    || h == "[::1]"
    || maybe False isIPv4Loopback (parseIPv4 h)
  where
    isIPv4Loopback octets = case octets of
      127 : _ -> True
      _       -> False

    parseIPv4 txt = case traverse parseOctet (T.splitOn "." txt) of
      Just [a, b, c, d] -> Just [a, b, c, d]
      _                 -> Nothing

    parseOctet part
      | T.null part = Nothing
      | not (T.all isDigit part) = Nothing
      | otherwise = do
          value <- readMaybe (T.unpack part)
          if value >= (0 :: Int) && value <= 255
            then Just value
            else Nothing

-- | Whether CORS config permits browser requests from non-loopback origins.
corsAllowsRemoteOrigins :: CorsConfig -> Bool
corsAllowsRemoteOrigins corsCfg = any allowsRemote corsCfg.allowedOrigins
  where
    allowsRemote origin
      | T.strip origin == "*" = True
      | otherwise = not (maybe False serverHostIsLoopback (originHost origin))

    originHost origin =
      let trimmed = T.strip origin
          withoutScheme = case T.splitOn "://" trimmed of
            [_scheme, rest] -> rest
            _               -> trimmed
          authority = T.takeWhile (/= '/') withoutScheme
      in if T.null authority
          then Nothing
          else if "[" `T.isPrefixOf` authority
            then case T.breakOn "]" (T.drop 1 authority) of
              (ipv6Host, suffix)
                | "]" `T.isPrefixOf` suffix -> Just ipv6Host
              _ -> Nothing
            else Just (T.takeWhile (/= ':') authority)

-- | Whether local mode would synthesize the implicit local superadmin.
localImplicitBootstrapActive :: HMemConfig -> Bool
localImplicitBootstrapActive cfg =
  cfg.auth.mode == AuthModeLocal && cfg.auth.local.bootstrapEnabled

-- | Config gate for the implicit local superadmin policy.
localImplicitBootstrapAllowed :: HMemConfig -> Bool
localImplicitBootstrapAllowed cfg =
  not (localImplicitBootstrapActive cfg)
    || cfg.auth.local.allowRemoteBootstrap
    || (serverHostIsLoopback cfg.server.host && not (corsAllowsRemoteOrigins cfg.cors))

-- | Whether the implicit local superadmin would be reachable from non-loopback
-- clients under the current config.
localImplicitBootstrapExposesRemote :: HMemConfig -> Bool
localImplicitBootstrapExposesRemote cfg =
  localImplicitBootstrapActive cfg
    && (not (serverHostIsLoopback cfg.server.host) || corsAllowsRemoteOrigins cfg.cors)

-- | Startup refusal message for unsafe implicit local superadmin exposure.
localImplicitBootstrapStartupError :: HMemConfig -> Maybe Text
localImplicitBootstrapStartupError cfg
  | localImplicitBootstrapAllowed cfg = Nothing
  | otherwise = Just $
      "Unsafe local auth configuration: auth.mode=local and auth.local.bootstrap_enabled=true would expose the implicit local superadmin. "
      <> "Remediation: bind server.host to localhost/127.0.0.1 with local-only CORS, set auth.local.bootstrap_enabled=false, switch auth.mode=deployed, "
      <> "or set auth.local.allow_remote_bootstrap=true only on trusted private networks."

-- | Whether the currently implemented legacy static bearer auth path can
-- actually be enforced with the loaded config.
authStaticBearerEnabled :: AuthConfig -> Bool
authStaticBearerEnabled authCfg = authCfg.mode == AuthModeLocal && authCfg.enabled && isJust authCfg.apiKey

-- | The bearer token used by the currently implemented legacy static
-- bearer auth path. Returns 'Nothing' when that path is not active.
authStaticBearerToken :: AuthConfig -> Maybe Text
authStaticBearerToken authCfg
  | authStaticBearerEnabled authCfg = authCfg.apiKey
  | otherwise = Nothing

-- | Private bridge credential used solely to authenticate MCP provenance.
authMcpProvenanceToken :: AuthConfig -> Maybe Text
authMcpProvenanceToken = normalizeMcpProvenanceToken . (.mcpProvenanceToken)
