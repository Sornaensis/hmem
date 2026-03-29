module Main where

import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Control.Exception (SomeException, catch, finally)
import Data.Pool (destroyAllResources)
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setPort, setTimeout, setGracefulShutdownTimeout)
import Options.Applicative
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.Log.FastLogger (LogType'(LogFile), FileLogSpec(..), defaultBufSize, newFastLogger)

import HMem.Config qualified as Config
import HMem.DB.Pool qualified as Pool
import HMem.Server.AccessTracker (newAccessTracker, flushNow)
import HMem.Server.App (mkApp)
import HMem.Server.Logging (newLogger, parseLogLevel, logInfo, logWarn, jsonRequestLogger)

------------------------------------------------------------------------
-- CLI
------------------------------------------------------------------------

data Opts = Opts
  { optPort   :: Maybe Int
  , optDbConn :: Maybe String
  , optPool   :: Maybe Int
  }

optsParser :: Parser Opts
optsParser = Opts
  <$> optional (option auto
      ( long "port"
     <> short 'p'
     <> metavar "PORT"
     <> help "HTTP port (default: from ~/.hmem/config.yaml or 8420)"
      ))
  <*> optional (strOption
      ( long "db"
     <> short 'd'
     <> metavar "CONNSTR"
     <> help "PostgreSQL connection string (default: from config)"
      ))
  <*> optional (option auto
      ( long "pool-size"
     <> metavar "N"
     <> help "Maximum database connections (default: from config or 10)"
      ))

------------------------------------------------------------------------
-- Main
------------------------------------------------------------------------

main :: IO ()
main = do
  opts <- execParser $ info (optsParser <**> helper)
    ( fullDesc
   <> progDesc "hmem - LLM memory & task management server"
   <> header "hmem-server"
    )

  cfg <- Config.loadConfig

  let port    = fromMaybe cfg.server.port opts.optPort
      connStr = maybe (Config.connectionString cfg.database) T.pack opts.optDbConn
      poolSz  = fromMaybe cfg.pool.size opts.optPool

  pool <- Pool.createPool connStr poolSz cfg.pool.idleTimeout
  tracker <- newAccessTracker pool 5  -- flush access counts every 5 seconds
  pgvec <- Pool.checkPgvector pool

  -- Set up rotating file logger in ~/.hmem/logs/
  logDir <- (</> "logs") <$> Config.configDir
  createDirectoryIfMissing True logDir
  let logPath = logDir </> "hmem-server.log"
      fileSpec = FileLogSpec
        { log_file          = logPath
        , log_file_size     = fromIntegral cfg.logging.maxSizeMB * 1024 * 1024
        , log_backup_number = cfg.logging.backupCount
        }
  (logAction, cleanupLog) <- newFastLogger (LogFile fileSpec defaultBufSize)
  let logger = newLogger logAction (parseLogLevel cfg.logging.level)
  requestLogger <- jsonRequestLogger logAction

  logInfo logger $ "hmem-server listening on port " <> T.pack (show port)
  logInfo logger $ "Logging to: " <> T.pack logPath
  logInfo logger $ "Rotation: " <> T.pack (show cfg.logging.maxSizeMB) <> " MB, "
                               <> T.pack (show cfg.logging.backupCount) <> " backups"
  logInfo logger $ "Log level: " <> cfg.logging.level
  logInfo logger $ "API auth: " <> if cfg.auth.enabled then "enabled" else "disabled"
  logInfo logger $ "Rate limiting: " <> if cfg.rateLimit.rlEnabled then "enabled" else "disabled"
  logInfo logger $ "pgvector: " <> if pgvec then "available" else "not installed (similarity search disabled)"
  let settings = setPort port
               $ setTimeout 60
               $ setGracefulShutdownTimeout (Just 30)
               $ defaultSettings
  let shutdown = do
        logInfo logger "hmem-server: shutting down..."
        flushNow pool tracker
          `catch` \(_ :: SomeException) -> logWarn logger "failed to flush access tracker"
        destroyAllResources pool
        cleanupLog
  app <- mkApp requestLogger cfg.auth cfg.cors cfg.rateLimit pool tracker pgvec
  runSettings settings app
    `finally` shutdown
