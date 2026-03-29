module HMem.Config
  ( -- * Config types
    HMemConfig(..)
  , ServerConfig(..)
  , DatabaseConfig(..)
  , PoolConfig(..)
  , LogConfig(..)
  , CorsConfig(..)
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
    -- * Derived helpers
  , connectionString
  , serverUrl
  ) where

import Data.Aeson (FromJSON(..), ToJSON(..), (.:?), (.!=), (.=))
import Data.Aeson qualified as Aeson
import Data.Text (Text)
import Data.Text qualified as T
import Data.Yaml qualified as Yaml
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
  } deriving stock (Show, Eq)

data PoolConfig = PoolConfig
  { size        :: !Int
  , idleTimeout :: !Double
  } deriving stock (Show, Eq)

data LogConfig = LogConfig
  { level       :: !Text
  , maxSizeMB   :: !Int
  , backupCount :: !Int
  } deriving stock (Show, Eq)

data CorsConfig = CorsConfig
  { allowedOrigins :: ![Text]
  } deriving stock (Show, Eq)

data HMemConfig = HMemConfig
  { server   :: !ServerConfig
  , database :: !DatabaseConfig
  , pool     :: !PoolConfig
  , logging  :: !LogConfig
  , cors     :: !CorsConfig
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

instance ToJSON DatabaseConfig where
  toJSON dc = Aeson.object $ concat
    [ [ "host" .= dc.host
      , "port" .= dc.port
      , "name" .= dc.name
      ]
    , maybe [] (\u -> ["user" .= u]) dc.user
    , maybe [] (\p -> ["password" .= p]) dc.password
    ]

instance FromJSON PoolConfig where
  parseJSON = Aeson.withObject "PoolConfig" $ \o -> PoolConfig
    <$> o .:? "size" .!= 10
    <*> o .:? "idle_timeout" .!= 60

instance ToJSON PoolConfig where
  toJSON pc = Aeson.object
    [ "size" .= pc.size
    , "idle_timeout" .= pc.idleTimeout
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

instance FromJSON HMemConfig where
  parseJSON = Aeson.withObject "HMemConfig" $ \o -> HMemConfig
    <$> o .:? "server"   .!= defServer
    <*> o .:? "database" .!= defDatabase
    <*> o .:? "pool"     .!= defPool
    <*> o .:? "logging"  .!= defLogging
    <*> o .:? "cors"     .!= defCors

instance ToJSON HMemConfig where
  toJSON cfg = Aeson.object
    [ "server"   .= cfg.server
    , "database" .= cfg.database
    , "pool"     .= cfg.pool
    , "logging"  .= cfg.logging
    , "cors"     .= cfg.cors
    ]

------------------------------------------------------------------------
-- Defaults
------------------------------------------------------------------------

defServer :: ServerConfig
defServer = ServerConfig { port = 8420, host = "127.0.0.1" }

defDatabase :: DatabaseConfig
defDatabase = DatabaseConfig { host = "127.0.0.1", port = 54320, name = "hmem", user = Nothing, password = Nothing }

defPool :: PoolConfig
defPool = PoolConfig { size = 10, idleTimeout = 60 }

defLogging :: LogConfig
defLogging = LogConfig { level = "info", maxSizeMB = 10, backupCount = 5 }

defCorsOrigins :: [Text]
defCorsOrigins = ["http://localhost", "http://127.0.0.1"]

defCors :: CorsConfig
defCors = CorsConfig { allowedOrigins = defCorsOrigins }

defaultConfig :: HMemConfig
defaultConfig = HMemConfig
  { server   = defServer
  , database = defDatabase
  , pool     = defPool
  , logging  = defLogging
  , cors     = defCors
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
  let (warnings, validated) = validateConfig cfg
  mapM_ (\w -> hPutStrLn stderr $ "Config warning: " <> w) warnings
  pure validated

-- | Write config to @~\/.hmem\/config.yaml@.
saveConfig :: HMemConfig -> IO ()
saveConfig cfg = do
  dir <- configDir
  createDirectoryIfMissing True dir
  path <- configFilePath
  Yaml.encodeFile path cfg

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
    warnings  = srvWarns <> dbWarns <> plWarns <> lgWarns
    corrected = cfg { server = srv, database = db, pool = pl, logging = lg }

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
                  }
      in  (ws1 <> ws2 <> ws3, d')

    validatePool p =
      let (ws1, sz) = clampField "pool.size" 1 1000 p.size
          (ws2, it) = clampFieldD "pool.idle_timeout" 1.0 3600.0 p.idleTimeout
      in  (ws1 <> ws2, PoolConfig { size = sz, idleTimeout = it })

    validateLog l =
      let (ws1, ms) = clampField "logging.max_size_mb" 1 10000 l.maxSizeMB
          (ws2, bc) = clampField "logging.backup_count" 0 100 l.backupCount
      in  (ws1 <> ws2, LogConfig { level = l.level, maxSizeMB = ms, backupCount = bc })

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
  ]
  where
    escPq = T.replace "\\" "\\\\" . T.replace "'" "\\'"

-- | Build the server base URL, e.g. @http:\/\/127.0.0.1:8420@.
serverUrl :: ServerConfig -> Text
serverUrl sc = "http://" <> sc.host <> ":" <> T.pack (show sc.port)
