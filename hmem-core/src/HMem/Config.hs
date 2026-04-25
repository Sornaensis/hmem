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
  , TlsConfig(..)
  , WebConfig(..)
    -- * Defaults
  , defaultConfig
    -- * Load / Save
  , loadConfig
  , saveConfig
    -- * Validation
  , validateConfig
    -- * Paths
  , configDir
  , configFilePath
    -- * Overrides
  , applyEnvOverrides
    -- * Derived helpers
  , connectionString
  , serverUrl
  , authStaticBearerEnabled
  , authStaticBearerToken
  ) where

import Control.Applicative ((<|>))
import Data.Aeson (FromJSON(..), ToJSON(..), (.:), (.:?), (.!=), (.=))
import Data.Aeson qualified as Aeson
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Yaml qualified as Yaml
import System.Environment (lookupEnv)
import System.Directory (getHomeDirectory, doesFileExist, createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)

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
  { bootstrapEnabled :: !Bool
  , botTokens        :: ![LocalBotTokenConfig]
  } deriving stock (Show, Eq)

data DeployedAuthConfig = DeployedAuthConfig
  { issuer      :: !(Maybe Text)
  , audience    :: !(Maybe Text)
  , discoveryUrl :: !(Maybe Text)
  , jwksUrl     :: !(Maybe Text)
  , tokenLookup :: !TokenLookupMode
  } deriving stock (Show, Eq)

data AuthConfig = AuthConfig
  { mode     :: !AuthMode
  , enabled  :: !Bool
  , apiKey   :: !(Maybe Text)
  , local    :: !LocalAuthConfig
  , deployed :: !DeployedAuthConfig
  } deriving stock (Show, Eq)

data RateLimitConfig = RateLimitConfig
  { rlEnabled           :: !Bool
  , rlRequestsPerSecond :: !Double
  , rlBurst             :: !Int
  } deriving stock (Show, Eq)

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
    <*> o .:? "bot_tokens" .!= []

instance ToJSON LocalAuthConfig where
  toJSON localCfg = Aeson.object
    [ "bootstrap_enabled" .= localCfg.bootstrapEnabled
    , "bot_tokens" .= localCfg.botTokens
    ]

instance FromJSON DeployedAuthConfig where
  parseJSON = Aeson.withObject "DeployedAuthConfig" $ \o -> DeployedAuthConfig
    <$> o .:? "issuer"
    <*> o .:? "audience"
    <*> o .:? "discovery_url"
    <*> o .:? "jwks_url"
    <*> o .:? "token_lookup" .!= TokenLookupDatabase

instance ToJSON DeployedAuthConfig where
  toJSON deployedCfg = Aeson.object $ concat
    [ maybe [] (\v -> ["issuer" .= v]) deployedCfg.issuer
    , maybe [] (\v -> ["audience" .= v]) deployedCfg.audience
    , maybe [] (\v -> ["discovery_url" .= v]) deployedCfg.discoveryUrl
    , maybe [] (\v -> ["jwks_url" .= v]) deployedCfg.jwksUrl
    , ["token_lookup" .= deployedCfg.tokenLookup]
    ]

instance FromJSON AuthConfig where
  parseJSON = Aeson.withObject "AuthConfig" $ \o -> AuthConfig
    <$> o .:? "mode" .!= AuthModeLocal
    <*> o .:? "enabled" .!= False
    <*> o .:? "api_key"
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
  , botTokens = []
  }

defDeployedAuth :: DeployedAuthConfig
defDeployedAuth = DeployedAuthConfig
  { issuer = Nothing
  , audience = Nothing
  , discoveryUrl = Nothing
  , jwksUrl = Nothing
  , tokenLookup = TokenLookupDatabase
  }

defAuth :: AuthConfig
defAuth = AuthConfig
  { mode = AuthModeLocal
  , enabled = False
  , apiKey = Nothing
  , local = defLocalAuth
  , deployed = defDeployedAuth
  }

defRateLimit :: RateLimitConfig
defRateLimit = RateLimitConfig
  { rlEnabled = False
  , rlRequestsPerSecond = 10.0
  , rlBurst = 20
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

-- | Load config from @~\/.hmem\/config.yaml@.  Returns 'defaultConfig'
-- if the file does not exist or cannot be parsed.  Prints warnings for
-- any validation issues and clamps values to valid ranges.
loadConfig :: IO HMemConfig
loadConfig = do
  path   <- configFilePath
  exists <- doesFileExist path
  cfg <- if exists
    then do
      result <- Yaml.decodeFileEither path
      case result of
        Left err -> do
          hPutStrLn stderr $
            "Warning: failed to parse " <> path <> ": " <> show err
          hPutStrLn stderr "Using default configuration."
          pure defaultConfig
        Right c -> pure c
    else pure defaultConfig
  envPassword <- fmap T.pack <$> lookupEnv "HMEM_DB_PASSWORD"
  envApiKey   <- fmap T.pack <$> lookupEnv "HMEM_API_KEY"
  envSslMode  <- fmap T.pack <$> lookupEnv "HMEM_DB_SSLMODE"
  let cfg' = applyEnvOverrides envPassword envApiKey envSslMode cfg
      (warnings, validated) = validateConfig cfg'
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
    (auWarns,  au)  = validateAuth cfg.auth
    (rlWarns,  rl)  = validateRateLimit cfg.rateLimit
    warnings  = srvWarns <> dbWarns <> plWarns <> lgWarns <> auWarns <> rlWarns
    corrected = cfg { server = srv, database = db, pool = pl, logging = lg, auth = au, rateLimit = rl }

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

    validateAuth a = (missingLegacyWarn <> deployedWarns, a)
      where
        missingLegacyWarn
          | a.mode == AuthModeLocal
          , a.enabled
          , Nothing <- a.apiKey =
              ["auth.enabled is true but no auth.api_key or HMEM_API_KEY is configured; current runtime will not enable legacy static bearer auth until richer mode-specific auth is implemented"]
          | otherwise = []

        deployedWarns
          | a.mode == AuthModeDeployed
          , a.deployed.issuer == Nothing
          , a.deployed.discoveryUrl == Nothing
          , a.deployed.jwksUrl == Nothing =
              ["auth.mode is deployed but no issuer, discovery_url, or jwks_url is configured"]
          | otherwise = []

    validateRateLimit rateLimitCfg =
      let (ws1, rps) = clampFieldD "rate_limit.requests_per_second" 0.1 10000.0 rateLimitCfg.rlRequestsPerSecond
          (ws2, burst) = clampField "rate_limit.burst" 1 100000 rateLimitCfg.rlBurst
      in (ws1 <> ws2, RateLimitConfig
        { rlEnabled = rateLimitCfg.rlEnabled
        , rlRequestsPerSecond = rps
        , rlBurst = burst
        })

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
