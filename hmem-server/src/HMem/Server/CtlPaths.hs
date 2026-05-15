module HMem.Server.CtlPaths
  ( serverRotatingLogPath
  , serverStdioLogPath
  , logPathsAreDistinct
  , serverHealthPath
  , serverStartupHealthUrl
  ) where

import Data.Text qualified as T
import System.FilePath ((</>), normalise)

-- | The log file owned by hmem-server's rotating logger.
serverRotatingLogPath :: FilePath -> FilePath
serverRotatingLogPath logDir = logDir </> "hmem-server.log"

-- | Non-rotated stdout/stderr capture used by hmem-ctl when spawning
-- hmem-server. This must remain distinct from 'serverRotatingLogPath': on
-- Windows, holding the rotating log open from hmem-ctl prevents the server
-- from renaming it during log rotation and makes startup fail.
serverStdioLogPath :: FilePath -> FilePath
serverStdioLogPath logDir = logDir </> "hmem-server-stdio.log"

logPathsAreDistinct :: FilePath -> Bool
logPathsAreDistinct logDir =
  normalise (serverRotatingLogPath logDir) /= normalise (serverStdioLogPath logDir)

serverHealthPath :: String
serverHealthPath = "/api/v1/health"

serverStartupHealthUrl :: T.Text -> Int -> String
serverStartupHealthUrl host port =
  "http://" <> startupHealthHost host <> ":" <> show port <> serverHealthPath
  where
    startupHealthHost h
      | h == "0.0.0.0" = "127.0.0.1"
      | h == "::" = "[::1]"
      | T.isPrefixOf "[" h && T.isSuffixOf "]" h = T.unpack h
      | T.any (== ':') h = "[" <> T.unpack h <> "]"
      | otherwise = T.unpack h
