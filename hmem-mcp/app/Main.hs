module Main where

import Data.Text qualified as T
import Options.Applicative

import HMem.Config qualified as Config
import HMem.MCP.Server (runMCPServer)

data Opts = Opts
  { optServerUrl :: Maybe String
  }

optsParser :: Parser Opts
optsParser = Opts
  <$> optional (strOption
      ( long "server-url"
     <> short 's'
     <> metavar "URL"
     <> help "URL of the hmem-server instance (default: from ~/.hmem/config.yaml)"
      ))

main :: IO ()
main = do
  opts <- execParser $ info (optsParser <**> helper)
    ( fullDesc
   <> progDesc "hmem-mcp - MCP bridge for LLM memory management"
   <> header "hmem-mcp"
    )
  cfg <- Config.loadConfig
  let url = maybe (T.unpack (Config.serverUrl cfg.server)) id opts.optServerUrl
  runMCPServer url cfg.auth.apiKey
