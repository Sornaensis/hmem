module Main where

import Data.Text qualified as T
import Options.Applicative
import System.Environment (lookupEnv)
import System.IO (hPutStrLn, stderr)

import HMem.Config qualified as Config
import HMem.MCP.Config (resolveForwardedToken, resolveServerUrl, safeServerUrlForLog)
import HMem.MCP.Server (runMCPServer)

data Opts = Opts
  { optServerUrl :: Maybe String
  , optAuthToken :: Maybe String
  , optNoAuth    :: Bool
  }

optsParser :: Parser Opts
optsParser = Opts
  <$> optional (strOption
      ( long "server-url"
     <> short 's'
     <> metavar "URL"
     <> help "URL of the hmem-server instance (default: HMEM_SERVER_URL or ~/.hmem/config.yaml)"
      ))
  <*> optional (strOption
      ( long "auth-token"
     <> metavar "TOKEN"
     <> help "Token to forward as a Bearer Authorization header; accepts raw tokens or 'Bearer ...' values (default: HMEM_MCP_AUTH_TOKEN, HMEM_AUTH_TOKEN, or loopback-only local legacy auth.api_key when active)"
      ))
  <*> switch
      ( long "no-auth"
     <> help "Do not forward any Authorization header, even if config or env has a token"
      )

main :: IO ()
main = do
  opts <- execParser $ info (optsParser <**> helper)
    ( fullDesc
   <> progDesc "hmem-mcp - MCP bridge for LLM memory management"
   <> header "hmem-mcp"
     )
  cfg <- Config.loadConfig
  envServerUrl <- lookupEnv "HMEM_SERVER_URL"
  envMcpToken <- lookupEnv "HMEM_MCP_AUTH_TOKEN"
  envAuthToken <- lookupEnv "HMEM_AUTH_TOKEN"
  let url = resolveServerUrl opts.optServerUrl envServerUrl (Config.serverUrl cfg.server)
      mToken = resolveForwardedToken cfg url opts.optNoAuth opts.optAuthToken envMcpToken envAuthToken
  logForwardingSummary cfg url mToken opts.optNoAuth
  runMCPServer url mToken


logForwardingSummary :: Config.HMemConfig -> String -> Maybe T.Text -> Bool -> IO ()
logForwardingSummary cfg url mToken noAuth = do
  hPutStrLn stderr $ "hmem-mcp: forwarding to " <> safeServerUrlForLog url <> " (auth.mode=" <> modeLabel <> ")"
  case (noAuth, cfg.auth.mode, mToken) of
    (True, _, _) -> hPutStrLn stderr "hmem-mcp: auth forwarding disabled by --no-auth"
    (_, Config.AuthModeDeployed, Nothing) ->
      hPutStrLn stderr "hmem-mcp: warning: deployed mode has no bearer token; protected tool calls will fail until HMEM_MCP_AUTH_TOKEN/HMEM_AUTH_TOKEN or --auth-token is provided"
    (_, Config.AuthModeLocal, Nothing) ->
      hPutStrLn stderr "hmem-mcp: no bearer token configured; relying on local server bootstrap if enabled"
    (_, _, Just _) ->
      hPutStrLn stderr "hmem-mcp: forwarding bearer token from CLI/env/config without interpreting permissions"
  where
    modeLabel = case cfg.auth.mode of
      Config.AuthModeLocal -> "local"
      Config.AuthModeDeployed -> "deployed"
