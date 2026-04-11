-- | hmem-ctl — control tool for the hmem environment.
--
--   @hmem-ctl@               – full setup (init + install auto-run)
--   @hmem-ctl init@          – create ~/.hmem/, PostgreSQL data dir, database, schema
--   @hmem-ctl install@       – register auto-run services (requires init first)
--   @hmem-ctl start@         – start PostgreSQL, apply pending migrations, then start hmem-server
--   @hmem-ctl stop@          – stop hmem-server + PostgreSQL
--   @hmem-ctl status@        – show service status
--   @hmem-ctl uninstall@     – stop services, remove auto-run, delete ~/.hmem/
--   @hmem-ctl reinstall@     – uninstall + full setup
--   @hmem-ctl workspaces@    – list workspaces (with optional name search)
--   @hmem-ctl projects@      – list projects (with optional name/workspace filter)
--   @hmem-ctl visualize@     – generate SVG workspace visualization
--   @hmem-ctl workspace@    – interactively select/create workspace, write .hmem.workspace
--
--   Requires: initdb, pg_ctl, createdb, psql on PATH (ships with PostgreSQL).

module Main where

import Control.Concurrent (threadDelay)
import Control.Exception  (SomeException, try)
import Control.Monad      (filterM, when)
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.Char          (isSpace)
import Data.List          (isInfixOf, dropWhileEnd, isPrefixOf)
import Data.Maybe         (catMaybes, fromMaybe)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.UUID          (UUID)
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Types.Status (statusCode)
import Options.Applicative
import System.Directory   (createDirectoryIfMissing, copyFile,
                           doesDirectoryExist, doesFileExist,
                           findExecutable, getHomeDirectory,
                           listDirectory,
                           removeDirectoryRecursive, removeFile)
import System.Exit        (ExitCode(..), exitFailure)
import System.FilePath    ((</>))
import System.IO          (IOMode(..), hFlush, hPutStrLn,
                           openFile, stderr, stdout)
import System.Info        (os)
import System.Process     (CreateProcess(..), StdStream(..), callProcess,
                           createProcess, proc, readProcess, waitForProcess)

import qualified Brick
import qualified Brick.Widgets.List as BL
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Border as B
import qualified Brick.AttrMap as A
import qualified Graphics.Vty as Vty
import qualified Graphics.Vty.CrossPlatform as VtyCross
import qualified Data.Vector as Vec
import HMem.Config
import HMem.DB.Migration qualified as Migration
import HMem.DB.Pool qualified as Pool
import HMem.Server.VisualizationSvg (renderWorkspaceVisualizationSvg, SvgDocument(..))
import HMem.Types
import Paths_hmem_server qualified as Paths

------------------------------------------------------------------------
-- CLI
------------------------------------------------------------------------

data Command
  = CmdSetup      -- default: init + install
  | CmdInit
  | CmdInstall
  | CmdStart
  | CmdStop
  | CmdStatus
  | CmdUninstall
  | CmdReinstall
  | CmdWorkspaces WorkspacesOpts
  | CmdProjects   ProjectsOpts
  | CmdVisualize  VisualizeOpts
  | CmdWorkspace

data WorkspacesOpts = WorkspacesOpts
  { wsName :: Maybe String
  }

data ProjectsOpts = ProjectsOpts
  { projName      :: Maybe String
  , projWorkspace :: Maybe String
  , projStatus    :: Maybe String
  }

data VisualizeOpts = VisualizeOpts
  { vizWorkspace  :: String
  , vizShowTasks  :: Bool
  , vizShowSummary :: Bool
  , vizShowDescs  :: Bool
  , vizOutput     :: Maybe FilePath
  }

commandParser :: Parser Command
commandParser = subparser
  (  command "init"      (info (pure CmdInit)
      (progDesc "Initialize ~/.hmem/, PostgreSQL, and config"))
  <> command "install"   (info (pure CmdInstall)
    (progDesc "Set up auto-run services (requires init first)"))
  <> command "start"     (info (pure CmdStart)
    (progDesc "Start PostgreSQL, apply pending migrations, and start hmem-server"))
  <> command "stop"      (info (pure CmdStop)
      (progDesc "Stop hmem-server and PostgreSQL"))
  <> command "status"    (info (pure CmdStatus)
      (progDesc "Show service status"))
  <> command "uninstall" (info (pure CmdUninstall)
      (progDesc "Stop services, remove auto-run, and delete ~/.hmem/"))
  <> command "reinstall" (info (pure CmdReinstall)
      (progDesc "Uninstall everything, then re-setup from scratch"))
  <> command "workspaces" (info (CmdWorkspaces <$> workspacesParser)
      (progDesc "List workspaces (optionally filter by name)"))
  <> command "projects"   (info (CmdProjects <$> projectsParser)
      (progDesc "List projects (optionally filter by name or workspace)"))
  <> command "visualize"  (info (CmdVisualize <$> visualizeParser)
      (progDesc "Generate SVG workspace visualization"))
  <> command "workspace" (info (pure CmdWorkspace)
      (progDesc "Interactively select or create a workspace, write .hmem.workspace"))
  )
  <|> pure CmdSetup

workspacesParser :: Parser WorkspacesOpts
workspacesParser = WorkspacesOpts
  <$> optional (strOption (long "name" <> short 'n' <> metavar "PATTERN" <> help "Filter workspaces by name (case-insensitive substring)"))

projectsParser :: Parser ProjectsOpts
projectsParser = ProjectsOpts
  <$> optional (strOption (long "name" <> short 'n' <> metavar "PATTERN" <> help "Filter projects by name (case-insensitive substring)"))
  <*> optional (strOption (long "workspace" <> short 'w' <> metavar "NAME" <> help "Filter by workspace name (case-insensitive substring)"))
  <*> optional (strOption (long "status" <> short 's' <> metavar "STATUS" <> help "Filter by status (active, paused, completed, archived)"))

visualizeParser :: Parser VisualizeOpts
visualizeParser = VisualizeOpts
  <$> strOption (long "workspace" <> short 'w' <> metavar "NAME" <> help "Workspace name (case-insensitive substring match)")
  <*> switch (long "tasks" <> short 't' <> help "Include task nodes in SVG")
  <*> (not <$> switch (long "no-summary" <> help "Suppress task status summaries"))
  <*> switch (long "descriptions" <> short 'd' <> help "Show project/task descriptions")
  <*> optional (strOption (long "output" <> short 'o' <> metavar "FILE" <> help "Output file path (default: stdout)"))

main :: IO ()
main = do
  cmd <- execParser $ info (commandParser <**> helper)
    ( fullDesc
   <> progDesc "hmem-ctl - Initialize and manage the hmem environment"
   <> header "hmem-ctl"
    )
  case cmd of
    CmdSetup     -> doInit >> doInstall
    CmdInit      -> doInit
    CmdInstall   -> doInstall
    CmdStart     -> doStart
    CmdStop      -> doStop
    CmdStatus    -> doStatus
    CmdUninstall -> doUninstall
    CmdReinstall -> doUninstall >> doInit >> doInstall
    CmdWorkspaces opts -> doWorkspaces opts
    CmdProjects opts   -> doProjects opts
    CmdVisualize opts  -> doVisualize opts
    CmdWorkspace       -> doWorkspace

------------------------------------------------------------------------
-- Init
------------------------------------------------------------------------

doInit :: IO ()
doInit = do
  putStrLn "=== hmem-ctl init ==="
  putStrLn ""

  dir <- configDir
  let dataDir = dir </> "data" </> "postgresql"
      logDir  = dir </> "logs"

  -- 1. Create directory structure
  step "Creating directory structure"
  createDirectoryIfMissing True dataDir
  createDirectoryIfMissing True logDir

  -- 2. Check for PostgreSQL tools
  step "Checking PostgreSQL tools"
  requireCommand "initdb"
  requireCommand "pg_ctl"
  requireCommand "createdb"
  requireCommand "psql"
  putStrLn "  All required tools found."

  -- 3. Initialize PostgreSQL if needed
  let pgVersionFile = dataDir </> "PG_VERSION"
  alreadyInit <- doesFileExist pgVersionFile

  cfg <- if alreadyInit
    then do
      step "PostgreSQL data directory already initialized"
      loadConfig
    else do
      step "Initializing PostgreSQL data directory"
      callProcess "initdb"
        [ "-D", dataDir
        , "--auth=trust"
        , "--no-instructions"
        , "--no-locale"
        , "-E", "UTF8"
        ]

      let cfg = defaultConfig
      appendFile (dataDir </> "postgresql.conf") $ unlines
        [ ""
        , "# hmem configuration"
        , "port = " <> show cfg.database.port
        , "listen_addresses = 'localhost'"
        ]
      pure cfg

  -- 4. Start PostgreSQL temporarily
  step "Starting PostgreSQL"
  callProcess "pg_ctl"
    [ "start"
    , "-D", dataDir
    , "-l", logDir </> "postgresql.log"
    , "-w"
    , "-t", "30"
    ]
  threadDelay 500000

  let pgPort = show cfg.database.port
      dbName = T.unpack cfg.database.name

  -- 5. Create database (idempotent)
  step "Creating database"
  dbResult <- try (callProcess "createdb"
    ["-h", "localhost", "-p", pgPort, dbName])
    :: IO (Either SomeException ())
  case dbResult of
    Right () -> putStrLn $ "  Created database '" <> dbName <> "'"
    Left _   -> putStrLn $ "  Database '" <> dbName <> "' already exists"

  -- 6. Install bundled migrations into ~/.hmem/
  step "Installing migrations"
  (migrationsDir, migrationFiles) <- installMigrations dir
  putStrLn $ "  Installed " <> show (length migrationFiles)
    <> " migration(s) to " <> migrationsDir

  -- 7. Run migrations
  step "Running database migrations"
  let connStr = connectionString cfg.database
  pool <- Pool.createPool connStr 2 60 30000
  migResult <- Migration.runMigrations pool migrationsDir
  case migResult.applied of
    [] -> putStrLn "  No new migrations to apply."
    ms -> do
      mapM_ (\m -> putStrLn $ "  Applied: " <> m) ms
      putStrLn $ "  " <> show (length ms) <> " migration(s) applied."
  case migResult.failed of
    Nothing -> pure ()
    Just (f, err) -> do
      hPutStrLn stderr $ "  Migration failed: " <> f
      hPutStrLn stderr $ "  Error: " <> err
      exitFailure

  -- 8. Stop PostgreSQL
  putStrLn ""
  step "Stopping PostgreSQL"
  _ <- try (callProcess "pg_ctl" ["stop", "-D", dataDir, "-m", "fast"])
        :: IO (Either SomeException ())
  pure ()

  -- 9. Write config
  step "Writing configuration"
  configPath <- configFilePath
  configExists <- doesFileExist configPath
  if configExists
    then putStrLn $ "  Config already exists: " <> configPath
    else do
      saveConfig cfg
      putStrLn $ "  Wrote: " <> configPath

  putStrLn ""
  putStrLn "hmem initialized successfully."
  putStrLn $ "  Config:   " <> dir </> "config.yaml"
  putStrLn $ "  Data:     " <> dataDir
  putStrLn $ "  Logs:     " <> logDir

------------------------------------------------------------------------
-- Install (auto-run services)
------------------------------------------------------------------------

doInstall :: IO ()
doInstall = do
  putStrLn ""
  putStrLn "=== hmem-ctl install ==="
  putStrLn ""

  dir <- ensureInitialized "install"

  step "Refreshing installed migrations"
  (migrationsDir, migrationFiles) <- installMigrations dir
  putStrLn $ "  Installed " <> show (length migrationFiles)
    <> " migration(s) to " <> migrationsDir

  if isWindows
    then installWindows
    else installLinux

  -- Install agent configs and MCP server definitions
  installAgentConfigs
  installMCPConfigs

installLinux :: IO ()
installLinux = do
  dir  <- configDir
  home <- getHomeDirectory

  let systemdDir = home </> ".config" </> "systemd" </> "user"
      dataDir    = dir </> "data" </> "postgresql"
      logDir     = dir </> "logs"

  createDirectoryIfMissing True systemdDir

  -- Find binaries
  pgBin     <- findBinary "postgres" "postgres"
  serverBin <- findBinary "hmem-server" "hmem-server"

  -- Write PostgreSQL service
  step "Writing hmem-postgres.service"
  writeFile (systemdDir </> "hmem-postgres.service") $ unlines
    [ "[Unit]"
    , "Description=hmem PostgreSQL Database"
    , ""
    , "[Service]"
    , "Type=simple"
    , "ExecStart=" <> pgBin <> " -D " <> dataDir
    , "ExecReload=/bin/kill -HUP $MAINPID"
    , "StandardOutput=append:" <> logDir </> "postgresql.log"
    , "StandardError=append:" <> logDir </> "postgresql.log"
    , "Restart=on-failure"
    , "RestartSec=5"
    , ""
    , "[Install]"
    , "WantedBy=default.target"
    ]

  -- Write hmem-server service
  step "Writing hmem-server.service"
  writeFile (systemdDir </> "hmem-server.service") $ unlines
    [ "[Unit]"
    , "Description=hmem Memory Server"
    , "After=hmem-postgres.service"
    , "Requires=hmem-postgres.service"
    , ""
    , "[Service]"
    , "Type=simple"
    , "ExecStart=" <> serverBin
    , "StandardOutput=append:" <> logDir </> "hmem-server.log"
    , "StandardError=append:" <> logDir </> "hmem-server.log"
    , "Restart=on-failure"
    , "RestartSec=5"
    , ""
    , "[Install]"
    , "WantedBy=default.target"
    ]

  -- Reload and enable
  step "Enabling services"
  callProcess "systemctl" ["--user", "daemon-reload"]
  callProcess "systemctl" ["--user", "enable", "hmem-postgres", "hmem-server"]

  putStrLn ""
  putStrLn "Systemd user services installed and enabled."
  putStrLn "Start now with:  systemctl --user start hmem-postgres hmem-server"
  putStrLn "Or reboot to start automatically."

installWindows :: IO ()
installWindows = do
  dir <- configDir
  let dataDir = dir </> "data" </> "postgresql"

  pgCtlBin <- findBinary "pg_ctl" "pg_ctl"
  setupBin <- findBinary "hmem-ctl" "hmem-ctl"

  -- Write startup script
  -- `hmem-ctl start` handles: pg_ctl start (graceful if already
  -- running), migration refresh, and launching hmem-server as a
  -- detached process (detach_console + create_group).  The bat
  -- therefore exits quickly and Task Scheduler marks the task complete.
  step "Writing startup scripts"
  let startScript = dir </> "start-hmem.bat"
  writeFile startScript $ unlines
    [ "@echo off"
    , "\"" <> setupBin <> "\" start"
    ]

  let stopScript = dir </> "stop-hmem.bat"
  writeFile stopScript $ unlines
    [ "@echo off"
    , "taskkill /IM hmem-server.exe /F 2>nul"
    , "\"" <> pgCtlBin <> "\" stop -D \"" <> dataDir <> "\" -m fast"
    ]

  -- Create scheduled task (requires elevation on some Windows configurations)
  step "Creating scheduled task"
  schtasksResult <- try (readProcess "schtasks"
    [ "/create"
    , "/tn", "hmem"
    , "/tr", "cmd /c \"" <> startScript <> "\""
    , "/sc", "onlogon"
    , "/f"
    , "/rl", "LIMITED"
    ] "") :: IO (Either SomeException String)
  case schtasksResult of
    Right _ -> pure ()
    Left err -> do
      let errMsg = show err
      if any (`isInfixOf` errMsg) ["Access is denied", "ERROR: Access"]
        then do
          putStrLn ""
          putStrLn "  NOTE: Could not create scheduled task (requires administrator privileges)."
          putStrLn "  Startup scripts have been written — you can run them manually:"
          putStrLn $ "    Start: " <> startScript
          putStrLn $ "    Stop:  " <> stopScript
          putStrLn "  To register the auto-start task, re-run this command as Administrator."
        else do
          putStrLn $ "  Warning: schtasks failed: " <> errMsg
          putStrLn "  You can run the start/stop scripts manually."

  putStrLn ""
  putStrLn "Windows startup configured."
  putStrLn $ "  Start: " <> startScript
  putStrLn $ "  Stop:  " <> stopScript
  putStrLn "A scheduled task 'hmem' will run at logon."

------------------------------------------------------------------------
-- Agent config installation
------------------------------------------------------------------------

installAgentConfigs :: IO ()
installAgentConfigs = do
  putStrLn ""
  step "Installing agent configurations"
  dir <- configDir
  let agentDir = dir </> "agents"
  createDirectoryIfMissing True (agentDir </> "copilot")
  createDirectoryIfMissing True (agentDir </> "claude")

  -- Write Copilot agent definitions to ~/.hmem/agents/
  writeAgentFile (agentDir </> "copilot" </> "hmem-memory.agent.md")
    copilotMemoryAgent
  writeAgentFile (agentDir </> "copilot" </> "hmem-task.agent.md")
    copilotTaskAgent
  writeAgentFile (agentDir </> "copilot" </> "hmem-hmem.agent.md")
    copilotHmemAgent

  -- Write Claude instructions
  writeAgentFile (agentDir </> "claude" </> "hmem-instructions.md")
    claudeInstructions

  putStrLn $ "  Agents installed to: " <> agentDir

  -- Also install into VS Code global prompts directory
  step "Installing agents to VS Code prompts"
  home <- getHomeDirectory
  let vsPromptsDir = if isWindows
        then home </> "AppData" </> "Roaming" </> "Code" </> "User" </> "prompts"
        else home </> ".config" </> "Code" </> "User" </> "prompts"
  createDirectoryIfMissing True vsPromptsDir

  let agentFiles =
        [ ("hmem-memory.agent.md", copilotMemoryAgent)
        , ("hmem-task.agent.md",   copilotTaskAgent)
        , ("hmem-hmem.agent.md",   copilotHmemAgent)
        ]
  mapM_ (\(name, content) ->
    writeAgentFile (vsPromptsDir </> name) content) agentFiles

  putStrLn $ "  VS Code prompts dir: " <> vsPromptsDir

writeAgentFile :: FilePath -> String -> IO ()
writeAgentFile path content = do
  writeFile path content
  putStrLn $ "  Wrote: " <> path

------------------------------------------------------------------------
-- MCP config installation
------------------------------------------------------------------------

installMCPConfigs :: IO ()
installMCPConfigs = do
  putStrLn ""
  step "Installing MCP server configurations"

  mcpBin <- findBinary "hmem-mcp" "hmem-mcp"

  -- VS Code global settings
  installVSCodeMCP mcpBin

  -- Claude Desktop
  installClaudeMCP mcpBin

-- | Merge hmem into VS Code's user-level mcp.json
installVSCodeMCP :: FilePath -> IO ()
installVSCodeMCP mcpBin = do
  home <- getHomeDirectory
  let mcpDir = if isWindows
        then home </> "AppData" </> "Roaming" </> "Code" </> "User"
        else home </> ".config" </> "Code" </> "User"
      mcpFile = mcpDir </> "mcp.json"

  createDirectoryIfMissing True mcpDir
  mergeMCPEntry mcpFile "servers" mcpBin "VS Code"

-- | Merge hmem into Claude Desktop's config
installClaudeMCP :: FilePath -> IO ()
installClaudeMCP mcpBin = do
  configPath <- claudeConfigPath
  case configPath of
    Nothing -> putStrLn "  Claude Desktop: skipped (config path not found)"
    Just path -> do
      let dir = takeDirectory path
      createDirectoryIfMissing True dir
      mergeMCPEntry path "mcpServers" mcpBin "Claude Desktop"
  where
    takeDirectory = reverse . dropWhile (/= pathSep) . reverse
    pathSep = if isWindows then '\\' else '/'

-- | Get Claude Desktop config file path
claudeConfigPath :: IO (Maybe FilePath)
claudeConfigPath
  | isWindows = do
      home <- getHomeDirectory
      let path = home </> "AppData" </> "Roaming" </> "Claude" </> "claude_desktop_config.json"
      pure (Just path)
  | isMacOS = do
      home <- getHomeDirectory
      let path = home </> "Library" </> "Application Support" </> "Claude" </> "claude_desktop_config.json"
      pure (Just path)
  | otherwise = do
      home <- getHomeDirectory
      let path = home </> ".config" </> "claude" </> "claude_desktop_config.json"
      pure (Just path)

isMacOS :: Bool
isMacOS = os == "darwin"

-- | Merge the hmem MCP server entry into an existing JSON config file.
-- Creates the file if it doesn't exist. Uses the given key for the
-- servers object ("servers" for VS Code, "mcpServers" for Claude).
mergeMCPEntry :: FilePath -> String -> FilePath -> String -> IO ()
mergeMCPEntry path serversKey mcpBin label = do
  exists <- doesFileExist path
  existing <- if exists
    then do
      bs <- BL.readFile path
      case Aeson.decode bs of
        Just obj -> pure obj
        Nothing  -> do
          putStrLn $ "  " <> label <> ": warning — could not parse " <> path
          putStrLn $ "  " <> label <> ": creating backup and writing new config"
          _ <- try (copyFile path (path <> ".bak")) :: IO (Either SomeException ())
          pure (Aeson.Object KM.empty)
    else pure (Aeson.Object KM.empty)

  let hmemEntry = Aeson.object
        [ "command" Aeson..= mcpBin
        , "args"    Aeson..= ([] :: [String])
        ]
      -- For VS Code, also include "type": "stdio"
      hmemEntryFull = if serversKey == "servers"
        then Aeson.object
          [ "type"    Aeson..= ("stdio" :: String)
          , "command" Aeson..= mcpBin
          , "args"    Aeson..= ([] :: [String])
          ]
        else hmemEntry

  case existing of
    Aeson.Object topObj -> do
      let sKey = Key.fromString serversKey
          serversObj = case KM.lookup sKey topObj of
            Just (Aeson.Object s) -> s
            _                     -> KM.empty
          hmemKey = Key.fromString "hmem"
          updated = KM.insert sKey
            (Aeson.Object (KM.insert hmemKey hmemEntryFull serversObj))
            topObj
      BL.writeFile path (Aeson.encode (Aeson.Object updated))
      putStrLn $ "  " <> label <> ": installed hmem MCP server in " <> path
    _ -> do
      -- Not an object, write fresh
      let fresh = Aeson.object
            [ Key.fromString serversKey Aeson..= Aeson.object
                [ "hmem" Aeson..= hmemEntryFull ]
            ]
      BL.writeFile path (Aeson.encode fresh)
      putStrLn $ "  " <> label <> ": wrote new config to " <> path

------------------------------------------------------------------------
-- Agent content (embedded)
------------------------------------------------------------------------

copilotMemoryAgent :: String
copilotMemoryAgent = unlines
  [ "---"
  , "description: \"Memory management agent \\u2014 stores, searches, links, and organizes long-term and short-term memories via hmem MCP tools.\""
  , "tools:"
  , "  - hmem"
  , "---"
  , ""
  , "# Memory Agent"
  , ""
  , "You are the hmem memory management agent. Your role is to help the user store, retrieve, search, connect, and organize memories using the hmem MCP server."
  , ""
  , "## Core Capabilities"
  , ""
  , "### Storage"
  , "- **memory_create** / **memory_create_batch** \\u2014 Store new memories (short_term or long_term). Always include meaningful tags and set importance (1-10)."
  , "- **memory_update** \\u2014 Modify existing memory content, importance, type, or metadata."
  , "- **memory_delete** \\u2014 Remove memories that are no longer relevant."
  , ""
  , "### Retrieval"
  , "- **memory_get** \\u2014 Fetch a specific memory by ID."
  , "- **memory_list** \\u2014 Browse memories, optionally filtered by workspace or type."
  , "- **memory_search** \\u2014 Full-text search with filters. Primary retrieval method."
  , ""
  , "### Organization"
  , "- **memory_set_tags** / **memory_get_tags** \\u2014 Manage tags for categorization."
  , "- **memory_pin** / **memory_unpin** \\u2014 Pin important memories for quick access."
  , "- **memory_adjust_importance** \\u2014 Re-score importance (1-10)."
  , "- **memory_link** / **memory_unlink** \\u2014 Create typed relationships between memories."
  , "- **memory_links_list** \\u2014 View all relationships for a memory."
  , "- **memory_graph** \\u2014 Explore the relationship graph from a starting memory."
  , "- Categories: **category_create**, **category_list**, **category_link_memory**, **category_unlink_memory**."
  , ""
  , "## Guidelines"
  , ""
  , "1. **Always search before creating** \\u2014 Check if a similar memory already exists."
  , "2. **Use meaningful tags** \\u2014 Apply 2-5 descriptive tags per memory (lowercase, hyphenated)."
  , "3. **Set importance accurately** \\u2014 1-3: background, 4-6: useful, 7-8: important, 9-10: critical."
  , "4. **Link related memories** \\u2014 Use supersedes/contradicts/elaborates as appropriate."
  , "5. **Use long_term for durable knowledge** \\u2014 Patterns, preferences, decisions, architecture."
  , "6. **Batch when possible** \\u2014 Use memory_create_batch for multiple related memories."
  ]

copilotTaskAgent :: String
copilotTaskAgent = unlines
  [ "---"
  , "description: \"Task and project management agent \\u2014 creates, tracks, and organizes tasks and projects via hmem MCP tools.\""
  , "tools:"
  , "  - hmem"
  , "---"
  , ""
  , "# Task Management Agent"
  , ""
  , "You are the hmem task management agent. Your role is to help the user plan, track, and organize projects and tasks using the hmem MCP server."
  , ""
  , "## Projects"
  , "- **project_create** \\u2014 Create projects with minimal fields first; supports sub-projects via parent_id."
  , "- **project_list** \\u2014 Filter by status (active, paused, completed, archived)."
  , "- **project_update** / **project_delete** \\u2014 Modify, reparent, detach, or remove projects. Use parent_id=null to move a project back to top level."
  , "- **project_link_memory** \\u2014 Attach relevant memories to a project."
  , ""
  , "## Tasks"
  , "- **task_create** \\u2014 Create with title plus optional project_id, parent_id, due_at, and metadata."
  , "- **task_list** \\u2014 Filter by workspace, project, or status (todo, in_progress, blocked, done, cancelled)."
  , "- **task_update** \\u2014 Move through statuses, reparent tasks, or move them between projects. Setting done auto-records completion time."
  , "- **task_dependency_add** / **task_dependency_remove** \\u2014 Define task ordering."
  , "- **task_link_memory** \\u2014 Attach context to tasks."
  , ""
  , "## Guidelines"
  , ""
  , "1. **Structure hierarchically** \\u2014 Projects > tasks > sub-tasks."
  , "2. **Create minimal, refine later** \\u2014 Use create tools for first capture, then update tools to reorganize or enrich."
  , "3. **Set clear statuses** \\u2014 todo \\u2192 in_progress \\u2192 done. Use blocked/cancelled as needed."
  , "4. **Use dependencies** \\u2014 For ordering only; use parent_id for hierarchy."
  , "5. **Link memories to tasks** \\u2014 For traceability."
  , "6. **Priority conventions** \\u2014 1-3: low, 4-6: normal, 7-8: high, 9-10: urgent."
  ]

copilotHmemAgent :: String
copilotHmemAgent = unlines
  [ "---"
  , "description: \"Full hmem agent \\u2014 combines memory management and task tracking. Use for general-purpose interactions with the hmem system.\""
  , "tools:"
  , "  - hmem"
  , "---"
  , ""
  , "# hmem Agent"
  , ""
  , "You are the hmem agent, a combined memory management and task tracking system."
  , ""
  , "## Available Tool Groups"
  , ""
  , "### Workspaces"
  , "workspace_register, workspace_list, workspace_get, workspace_update, workspace_delete"
  , "workspace_group_create, workspace_group_list, workspace_group_add_member, workspace_group_remove_member"
  , ""
  , "### Memories"
  , "CRUD: memory_create, memory_create_batch, memory_get, memory_list, memory_update, memory_delete"
  , "Search: memory_search (FTS with filters)"
  , "Tags: memory_set_tags, memory_get_tags"
  , "Links: memory_link, memory_unlink, memory_links_list, memory_graph, memory_find_by_relation"
  , "Organization: memory_pin, memory_unpin, memory_adjust_importance"
  , "Categories: category_create, category_list, category_link_memory, category_unlink_memory"
  , ""
  , "### Projects & Tasks"
  , "Projects: project_create, project_get, project_list, project_update, project_delete (including reparenting and top-level detaching)"
  , "Tasks: task_create, task_get, task_list, task_update, task_delete (including project moves and reparenting)"
  , "Dependencies: task_dependency_add, task_dependency_remove"
  , "Links: project_link_memory, task_link_memory"
  , ""
  , "### Cleanup & Activity"
  , "cleanup_run, cleanup_policies_list, cleanup_policy_upsert"
  , "activity_timeline"
  , ""
  , "## Workflow Patterns"
  , ""
  , "### Starting a new project"
  , "1. Ensure workspace exists (workspace_list / workspace_register)"
  , "2. Create project (project_create)"
  , "3. Break work into tasks (task_create with project_id)"
  , "4. Link relevant memories (project_link_memory)"
  , "5. Reorganize as the plan sharpens (project_update / task_update) instead of recreating structure"
  , ""
  , "### Capturing knowledge"
  , "1. Search for existing related memories (memory_search)"
  , "2. Create or update as appropriate"
  , "3. Link to relevant task/project"
  , "4. Tag and categorize for future retrieval"
  ]

claudeInstructions :: String
claudeInstructions = unlines
  [ "# hmem \\u2014 Memory & Task Management System"
  , ""
  , "You have access to the hmem MCP server which provides persistent memory storage and task/project management."
  , ""
  , "## Memory Management"
  , "- memory_create / memory_create_batch \\u2014 Store memories (short_term or long_term) with tags, importance (1-10), source."
  , "- memory_search \\u2014 Full-text search with filters (primary retrieval method)."
  , "- memory_get / memory_list \\u2014 Fetch by ID or browse."
  , "- memory_update / memory_delete \\u2014 Modify or remove."
  , "- memory_link / memory_unlink \\u2014 Typed relationships (related, supersedes, contradicts, elaborates, etc)."
  , "- memory_set_tags / memory_pin / memory_adjust_importance \\u2014 Organization."
  , "- memory_graph \\u2014 Explore relationship networks."
  , "- category_create / category_link_memory \\u2014 Hierarchical categories."
  , ""
  , "## Task & Project Management"
  , "- project_create / project_list / project_update \\u2014 Manage projects, including hierarchy changes and top-level detaching."
  , "- task_create / task_list / task_update \\u2014 Manage tasks, including project moves and reparenting."
  , "- task_dependency_add / task_dependency_remove \\u2014 Task ordering."
  , "- project_link_memory / task_link_memory \\u2014 Attach knowledge to work items."
  , ""
  , "## Workspaces"
  , "- workspace_register / workspace_list / workspace_get \\u2014 All data is scoped to workspaces."
  , "- workspace_group_create / workspace_group_add_member \\u2014 Organize workspaces."
  , ""
  , "## Best Practices"
  , "1. Search or list before creating to avoid duplicates and to collect IDs for follow-up actions."
  , "2. Use supersedes when newer info replaces older; contradicts for conflicts."
  , "3. Set importance: 1-3 background, 4-6 useful, 7-8 important, 9-10 critical."
  , "4. Always include 2-5 descriptive tags."
  , "5. Use long_term for durable knowledge, short_term for transient context."
  , "6. Prefer creating minimal records first, then reorganize or enrich with update tools as the structure becomes clearer."
  ]

------------------------------------------------------------------------
-- Start / Stop / Status
------------------------------------------------------------------------

doStart :: IO ()
doStart = do
  dir <- ensureInitialized "start"
  cfg <- loadConfig
  let dataDir = dir </> "data" </> "postgresql"
      logDir  = dir </> "logs"

  pgCtlBin <- findBinary "pg_ctl" "pg_ctl"

  putStrLn "Starting PostgreSQL..."
  pgResult <- try (runHidden pgCtlBin
    [ "start", "-D", dataDir
    , "-l", logDir </> "postgresql.log"
    , "-w", "-t", "30"
    ]) :: IO (Either SomeException ())
  case pgResult of
    Right () -> pure ()
    Left _ -> do
      status <- try (readProcess pgCtlBin ["status", "-D", dataDir] "")
        :: IO (Either SomeException String)
      case status of
        Right s | "server is running" `isInfixOf` s ->
          putStrLn "  PostgreSQL already running."
        _ -> do
          hPutStrLn stderr "  Failed to start PostgreSQL."
          exitFailure
  threadDelay 1000000

  step "Refreshing installed migrations"
  (migrationsDir, migrationFiles) <- installMigrations dir
  putStrLn $ "  Installed " <> show (length migrationFiles)
    <> " migration(s) to " <> migrationsDir

  step "Applying pending migrations"
  let connStr = connectionString cfg.database
  pool <- Pool.createPool connStr 2 60 30000
  migResult <- Migration.runMigrations pool migrationsDir
  case migResult.applied of
    [] -> putStrLn "  No new migrations to apply."
    ms -> do
      mapM_ (\m -> putStrLn $ "  Applied: " <> m) ms
      putStrLn $ "  " <> show (length ms) <> " migration(s) applied."
  case migResult.failed of
    Nothing -> pure ()
    Just (f, err) -> do
      hPutStrLn stderr $ "  Migration failed: " <> f
      hPutStrLn stderr $ "  Error: " <> err
      exitFailure

  putStrLn "Starting hmem-server..."
  serverBin <- findBinary "hmem-server" "hmem-server"
  let serverLog = logDir </> "hmem-server.log"
  logH <- openFile serverLog AppendMode
  _ <- createProcess (proc serverBin [])
    { std_in           = NoStream
    , std_out          = UseHandle logH
    , std_err          = UseHandle logH
    , detach_console   = True
    , use_process_jobs = False
    }
  threadDelay 500000
  putStrLn $ "hmem-server running on port " <> show cfg.server.port
  putStrLn $ "  Log: " <> serverLog

doStop :: IO ()
doStop = do
  dir <- configDir
  let dataDir = dir </> "data" </> "postgresql"

  putStrLn "Stopping hmem-server..."
  _ <- try stopServer :: IO (Either SomeException ())

  putStrLn "Stopping hmem-mcp processes..."
  _ <- try stopMcp :: IO (Either SomeException ())

  putStrLn "Stopping PostgreSQL..."
  _ <- try (callProcess "pg_ctl" ["stop", "-D", dataDir, "-m", "fast"])
        :: IO (Either SomeException ())
  putStrLn "Stopped."
  where
    stopServer
      | isWindows = callProcess "taskkill" ["/IM", "hmem-server.exe", "/F"]
      | otherwise = callProcess "pkill" ["-f", "hmem-server"]
    stopMcp
      | isWindows = callProcess "taskkill" ["/IM", "hmem-mcp.exe", "/F"]
      | otherwise = callProcess "pkill" ["-f", "hmem-mcp"]

doStatus :: IO ()
doStatus = do
  dir <- configDir
  let dataDir = dir </> "data" </> "postgresql"

  -- PostgreSQL status
  putStr "PostgreSQL:  "
  hFlush stdout
  pgResult <- try (readProcess "pg_ctl" ["status", "-D", dataDir] "")
    :: IO (Either SomeException String)
  case pgResult of
    Right s | "server is running" `isInfixOf` s -> putStrLn "running"
    _ -> putStrLn "stopped"

  -- hmem-server status
  putStr "hmem-server: "
  hFlush stdout
  running <- checkHmemServer
  putStrLn $ if running then "running" else "stopped"
  where
    checkHmemServer
      | isWindows = do
          result <- try (readProcess "tasklist"
            ["/FI", "IMAGENAME eq hmem-server.exe", "/NH"] "")
            :: IO (Either SomeException String)
          pure $ case result of
            Right s -> "hmem-server.exe" `isInfixOf` s
            Left _  -> False
      | otherwise = do
          result <- try (readProcess "pgrep" ["-x", "hmem-server"] "")
            :: IO (Either SomeException String)
          pure $ case result of
            Right s -> not (null (strip s))
            Left _  -> False

------------------------------------------------------------------------
-- Uninstall
------------------------------------------------------------------------

doUninstall :: IO ()
doUninstall = do
  putStrLn "=== hmem-ctl uninstall ==="
  putStrLn ""

  dir <- configDir
  let dataDir = dir </> "data" </> "postgresql"

  -- 1. Stop running services
  step "Stopping services"
  _ <- try stopServer :: IO (Either SomeException ())
  _ <- try (callProcess "pg_ctl" ["stop", "-D", dataDir, "-m", "fast"])
        :: IO (Either SomeException ())
  threadDelay 500000

  -- 2. Remove auto-run configuration
  step "Removing auto-run configuration"
  if isWindows
    then uninstallWindows dir
    else uninstallLinux

  -- 3. Remove ~/.hmem/ directory
  step "Removing ~/.hmem/"
  exists <- doesDirectoryExist dir
  if exists
    then do
      removeDirectoryRecursive dir
      putStrLn $ "  Removed: " <> dir
    else
      putStrLn $ "  Not found: " <> dir

  putStrLn ""
  putStrLn "hmem uninstalled."
  where
    stopServer
      | isWindows = callProcess "taskkill" ["/IM", "hmem-server.exe", "/F"]
      | otherwise = callProcess "pkill" ["-f", "hmem-server"]

uninstallLinux :: IO ()
uninstallLinux = do
  home <- getHomeDirectory
  let systemdDir = home </> ".config" </> "systemd" </> "user"
      pgService  = systemdDir </> "hmem-postgres.service"
      srvService = systemdDir </> "hmem-server.service"

  -- Disable and remove services
  _ <- try (callProcess "systemctl"
    ["--user", "disable", "hmem-postgres", "hmem-server"])
    :: IO (Either SomeException ())

  removeIfExists pgService
  removeIfExists srvService

  _ <- try (callProcess "systemctl" ["--user", "daemon-reload"])
    :: IO (Either SomeException ())
  putStrLn "  Removed systemd user services."

uninstallWindows :: FilePath -> IO ()
uninstallWindows dir = do
  -- Remove scheduled task (may require elevation)
  schtasksResult <- try (readProcess "schtasks" ["/delete", "/tn", "hmem", "/f"] "")
    :: IO (Either SomeException String)
  case schtasksResult of
    Right _ -> pure ()
    Left err -> do
      let errMsg = show err
      if any (`isInfixOf` errMsg) ["Access is denied", "ERROR: Access"]
        then putStrLn "  NOTE: Could not remove scheduled task (requires administrator privileges)."
        else putStrLn $ "  Warning: schtasks delete failed: " <> errMsg

  -- Remove bat scripts
  removeIfExists (dir </> "start-hmem.bat")
  removeIfExists (dir </> "stop-hmem.bat")
  putStrLn "  Removed scheduled task and scripts."

removeIfExists :: FilePath -> IO ()
removeIfExists path = do
  exists <- doesFileExist path
  when exists $ removeFile path

------------------------------------------------------------------------
-- Query commands (workspaces, projects, visualize)
------------------------------------------------------------------------

-- | Create an HTTP manager and build the base URL from config.
withApiClient :: (HTTP.Manager -> String -> IO a) -> IO a
withApiClient action = do
  cfg <- loadConfig
  mgr <- HTTP.newManager HTTP.defaultManagerSettings
  let base = "http://" <> T.unpack cfg.server.host <> ":" <> show cfg.server.port
  action mgr base

apiGet :: HTTP.Manager -> String -> String -> IO BL.ByteString
apiGet mgr base path = do
  req <- HTTP.parseRequest (base <> path)
  resp <- HTTP.httpLbs req mgr
  let code = statusCode (HTTP.responseStatus resp)
  when (code >= 400) $ do
    hPutStrLn stderr $ "Error: API returned status " <> show code
    hPutStrLn stderr $ BL8.unpack (HTTP.responseBody resp)
    exitFailure
  pure (HTTP.responseBody resp)

apiPostJson :: Aeson.ToJSON a => HTTP.Manager -> String -> String -> a -> IO BL.ByteString
apiPostJson mgr base path body = do
  initReq <- HTTP.parseRequest (base <> path)
  let req = initReq
        { HTTP.method = "POST"
        , HTTP.requestBody = HTTP.RequestBodyLBS (Aeson.encode body)
        , HTTP.requestHeaders = [("Content-Type", "application/json"), ("Accept", "application/json")]
        }
  resp <- HTTP.httpLbs req mgr
  let code = statusCode (HTTP.responseStatus resp)
  when (code >= 400) $ do
    hPutStrLn stderr $ "Error: API returned status " <> show code
    hPutStrLn stderr $ BL8.unpack (HTTP.responseBody resp)
    exitFailure
  pure (HTTP.responseBody resp)

doWorkspaces :: WorkspacesOpts -> IO ()
doWorkspaces opts = withApiClient $ \mgr base -> do
  body <- apiGet mgr base "/api/v1/workspaces?limit=100"
  case Aeson.decode body :: Maybe (PaginatedResult Workspace) of
    Nothing -> do
      hPutStrLn stderr "Error: could not parse workspace list"
      exitFailure
    Just result -> do
      let filtered = case opts.wsName of
            Nothing -> result.items
            Just pat -> filter (matchName pat . T.unpack . (.name)) result.items
      if null filtered
        then putStrLn "No matching workspaces found."
        else do
          putStrLn $ padRight 38 "ID" <> padRight 30 "NAME" <> "TYPE"
          putStrLn $ replicate 82 '-'
          mapM_ printWorkspace filtered
  where
    printWorkspace ws =
      putStrLn $ padRight 38 (show ws.id) <> padRight 30 (T.unpack ws.name) <> T.unpack (workspaceTypeToText ws.workspaceType)

doProjects :: ProjectsOpts -> IO ()
doProjects opts = withApiClient $ \mgr base -> do
  -- First get workspaces to map IDs to names and optionally filter
  wsBody <- apiGet mgr base "/api/v1/workspaces?limit=100"
  let allWorkspaces = case Aeson.decode wsBody :: Maybe (PaginatedResult Workspace) of
        Nothing -> []
        Just r  -> r.items
      targetWorkspaces = case opts.projWorkspace of
        Nothing -> allWorkspaces
        Just pat -> filter (matchName pat . T.unpack . (.name)) allWorkspaces
      wsNames = [(show ws.id, T.unpack ws.name) | ws <- allWorkspaces]
      wsNameLookup wsId = fromMaybe (show wsId) (lookup (show wsId) wsNames)
  when (null targetWorkspaces) $ do
    putStrLn "No matching workspaces found."
    exitFailure
  -- Gather projects from each workspace
  allProjects <- concat <$> mapM (\ws -> do
    let statusParam = maybe "" (\s -> "&status=" <> s) opts.projStatus
        url = "/api/v1/projects?limit=100&workspace_id=" <> show ws.id <> statusParam
    projBody <- apiGet mgr base url
    case Aeson.decode projBody :: Maybe (PaginatedResult Project) of
      Nothing -> pure []
      Just r  -> pure r.items
    ) targetWorkspaces
  let filtered = case opts.projName of
        Nothing -> allProjects
        Just pat -> filter (matchName pat . T.unpack . (.name)) allProjects
  if null filtered
    then putStrLn "No matching projects found."
    else do
      putStrLn $ padRight 38 "ID" <> padRight 28 "NAME" <> padRight 12 "STATUS" <> padRight 5 "PRI" <> "WORKSPACE"
      putStrLn $ replicate 100 '-'
      mapM_ (\p ->
        putStrLn $ padRight 38 (show p.id) <> padRight 28 (ellipsis 26 $ T.unpack p.name) <> padRight 12 (T.unpack (projectStatusToText p.status)) <> padRight 5 (show p.priority) <> wsNameLookup p.workspaceId
        ) filtered

doVisualize :: VisualizeOpts -> IO ()
doVisualize opts = withApiClient $ \mgr base -> do
  -- Find workspace by name
  wsBody <- apiGet mgr base "/api/v1/workspaces?limit=100"
  case Aeson.decode wsBody :: Maybe (PaginatedResult Workspace) of
    Nothing -> do
      hPutStrLn stderr "Error: could not parse workspace list"
      exitFailure
    Just result -> do
      let matching = filter (matchName opts.vizWorkspace . T.unpack . (.name)) result.items
      case matching of
        [] -> do
          hPutStrLn stderr $ "No workspace matching '" <> opts.vizWorkspace <> "' found."
          exitFailure
        [ws] -> generateVisualization mgr base ws opts
        multiple -> do
          hPutStrLn stderr $ "Multiple workspaces match '" <> opts.vizWorkspace <> "':"
          mapM_ (\ws -> hPutStrLn stderr $ "  " <> show ws.id <> "  " <> T.unpack ws.name) multiple
          hPutStrLn stderr "Please refine your search."
          exitFailure

generateVisualization :: HTTP.Manager -> String -> Workspace -> VisualizeOpts -> IO ()
generateVisualization mgr base ws opts = do
  let vizQuery = WorkspaceVisualizationQuery
        { includeProjectIds = Nothing
        , excludeProjectIds = Nothing
        , taskStatuses = Nothing
        , memoryFilter = Nothing
        , showTasks = Just opts.vizShowTasks
        , showTaskStatusSummary = Just opts.vizShowSummary
        , showDescriptions = Just opts.vizShowDescs
        }
  body <- apiPostJson mgr base ("/api/v1/workspaces/" <> show ws.id <> "/visualization") vizQuery
  case Aeson.decode body :: Maybe WorkspaceVisualization of
    Nothing -> do
      hPutStrLn stderr "Error: could not parse visualization response"
      exitFailure
    Just viz -> do
      let SvgDocument svgText = renderWorkspaceVisualizationSvg vizQuery viz
      case opts.vizOutput of
        Nothing -> TIO.putStr svgText
        Just path -> do
          TIO.writeFile path svgText
          putStrLn $ "Wrote SVG to: " <> path

------------------------------------------------------------------------
-- workspace: interactive workspace selector
------------------------------------------------------------------------

doWorkspace :: IO ()
doWorkspace = withApiClient $ \mgr base -> do
  -- Check if .hmem.workspace already exists
  existingFile <- doesFileExist ".hmem.workspace"
  when existingFile $ do
    existingId <- strip <$> readFile ".hmem.workspace"
    putStrLn $ "Note: .hmem.workspace already exists (ID: " <> existingId <> ")"
    putStrLn ""

  -- Fetch existing workspaces
  body <- apiGet mgr base "/api/v1/workspaces?limit=200"
  case Aeson.decode body :: Maybe (PaginatedResult Workspace) of
    Nothing -> do
      hPutStrLn stderr "Error: could not parse workspace list"
      exitFailure
    Just result -> do
      let workspaces = result.items
          options = map (\ws ->
            padRight 30 (ellipsis 28 $ T.unpack ws.name)
              <> T.unpack (workspaceTypeToText ws.workspaceType)
            ) workspaces
            ++ ["Create new workspace"]

      selected <- arrowMenu "Select a workspace:" options

      wsId <- if selected == length workspaces
        then createNewWorkspace mgr base
        else pure $ (.id) (workspaces !! selected)

      -- Write the file
      writeFile ".hmem.workspace" (show wsId <> "\n")
      putStrLn $ "Wrote .hmem.workspace (" <> show wsId <> ")"

createNewWorkspace :: HTTP.Manager -> String -> IO UUID
createNewWorkspace mgr base = do
  -- Ask for workspace type
  let types = [WsRepository, WsPlanning, WsPersonal, WsOrganization]
      typeLabels = map (T.unpack . workspaceTypeToText) types
  typeIdx <- arrowMenu "Workspace type:" typeLabels
  let wsType = types !! typeIdx

  -- Ask for name
  wsName <- promptLine "Workspace name: "
  when (null (strip wsName)) $ do
    hPutStrLn stderr "Error: name cannot be empty"
    exitFailure

  let createReq = CreateWorkspace
        { name = T.pack (strip wsName)
        , workspaceType = Just wsType
        , ghOwner = Nothing
        , ghRepo = Nothing
        }
  respBody <- apiPostJson mgr base "/api/v1/workspaces" createReq
  case Aeson.decode respBody :: Maybe Workspace of
    Nothing -> do
      hPutStrLn stderr "Error: could not parse created workspace"
      exitFailure
    Just ws -> do
      putStrLn $ "Created workspace: " <> T.unpack ws.name
        <> " (" <> T.unpack (workspaceTypeToText ws.workspaceType) <> ")"
      pure ws.id

------------------------------------------------------------------------
-- Interactive menu (brick/vty)
------------------------------------------------------------------------

-- | Display an interactive menu using brick TUI.
-- Returns the 0-based index of the selected item.
arrowMenu :: String -> [String] -> IO Int
arrowMenu title options = do
  let items = BL.list () (Vec.fromList (zip [0..] options)) 1
      app = Brick.App
        { Brick.appDraw         = drawMenu title
        , Brick.appChooseCursor = Brick.neverShowCursor
        , Brick.appHandleEvent  = handleMenuEvent
        , Brick.appStartEvent   = pure ()
        , Brick.appAttrMap      = const menuAttrMap
        }
  vtyBuilder <- VtyCross.mkVty Vty.defaultConfig
  result <- Brick.customMain vtyBuilder (VtyCross.mkVty Vty.defaultConfig) Nothing app items
  case BL.listSelectedElement result of
    Just (_, (idx, _)) -> pure idx
    Nothing            -> exitFailure

drawMenu :: String -> BL.List () (Int, String) -> [Brick.Widget ()]
drawMenu title l =
  [ Brick.padAll 1 $
    Brick.vBox
      [ Brick.withAttr (A.attrName "title") $ Brick.str title
      , Brick.str " "
      , BL.renderList renderItem True l
      , Brick.str " "
      , Brick.withAttr (A.attrName "help") $
          Brick.str "  ↑/↓/j/k: move  Enter/Space: select  q/Esc: quit"
      ]
  ]

renderItem :: Bool -> (Int, String) -> Brick.Widget ()
renderItem sel (_, label)
  | sel       = Brick.withAttr (A.attrName "selected") $
                  Brick.str $ "  > " <> label
  | otherwise = Brick.str $ "    " <> label

handleMenuEvent :: Brick.BrickEvent () e -> Brick.EventM () (BL.List () (Int, String)) ()
handleMenuEvent (Brick.VtyEvent (Vty.EvKey Vty.KEnter []))      = Brick.halt
handleMenuEvent (Brick.VtyEvent (Vty.EvKey (Vty.KChar ' ') [])) = Brick.halt
handleMenuEvent (Brick.VtyEvent (Vty.EvKey Vty.KEsc []))         = Brick.halt >> error "quit"
handleMenuEvent (Brick.VtyEvent (Vty.EvKey (Vty.KChar 'q') [])) = Brick.halt >> error "quit"
handleMenuEvent (Brick.VtyEvent (Vty.EvKey (Vty.KChar 'j') [])) =
  handleMenuEvent (Brick.VtyEvent (Vty.EvKey Vty.KDown []))
handleMenuEvent (Brick.VtyEvent (Vty.EvKey (Vty.KChar 'k') [])) =
  handleMenuEvent (Brick.VtyEvent (Vty.EvKey Vty.KUp []))
handleMenuEvent (Brick.VtyEvent ev) = BL.handleListEvent ev
handleMenuEvent _ = pure ()

menuAttrMap :: A.AttrMap
menuAttrMap = A.attrMap Vty.defAttr
  [ (A.attrName "title",    Vty.withStyle Vty.defAttr Vty.bold)
  , (A.attrName "selected", Vty.withForeColor (Vty.withStyle Vty.defAttr Vty.bold) Vty.cyan)
  , (A.attrName "help",     Vty.withForeColor Vty.defAttr (Vty.rgbColor 128 128 128))
  , (BL.listSelectedFocusedAttr, Vty.withForeColor (Vty.withStyle Vty.defAttr Vty.bold) Vty.cyan)
  ]

promptLine :: String -> IO String
promptLine prompt = do
  putStr prompt
  hFlush stdout
  getLine

toLowerAscii :: Char -> Char
toLowerAscii c
  | c >= 'A' && c <= 'Z' = toEnum (fromEnum c + 32)
  | otherwise = c

-- Helpers for the query commands

matchName :: String -> String -> Bool
matchName pat str = map toLowerAscii pat `isInfixOf` map toLowerAscii str

padRight :: Int -> String -> String
padRight n s = take n (s <> replicate n ' ')

ellipsis :: Int -> String -> String
ellipsis n s
  | length s <= n = s
  | n <= 3 = take n s
  | otherwise = take (n - 2) s <> ".."

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

isWindows :: Bool
isWindows = os == "mingw32"

-- | On Windows, run a command with a hidden console window via a
-- temporary VBScript stub.  @WshShell.Run@ with window style 0
-- (@SW_HIDE@) creates the process with @STARTF_USESHOWWINDOW@ so
-- the console window is never visible.  Child processes (e.g.
-- postgres spawned by pg_ctl) inherit the hidden console rather
-- than allocating a new visible one.
--
-- Waits for the command to finish and propagates the exit code.
-- On non-Windows, falls back to 'callProcess'.
runHidden :: FilePath -> [String] -> IO ()
runHidden bin args
  | isWindows = do
      dir <- configDir
      let vbsFile = dir </> ".run-hidden.vbs"
          quote s = "\"" <> s <> "\""
          cmdLine = unwords $ map quote (bin : args)
          -- In VBS string literals, \" is escaped as \"\"
          vbsEsc  = concatMap (\c -> if c == '"' then "\"\"" else [c]) cmdLine
      writeFile vbsFile $ unlines
        [ "Set ws = CreateObject(\"WScript.Shell\")"
        , "rc = ws.Run(\"" <> vbsEsc <> "\", 0, True)"
        , "WScript.Quit rc"
        ]
      (_, _, _, ph) <- createProcess (proc "wscript" ["//B", "//nologo", vbsFile])
        { use_process_jobs = False }
      ec <- waitForProcess ph
      removeIfExists vbsFile
      case ec of
        ExitSuccess   -> pure ()
        ExitFailure c -> fail $ bin <> " exited with code " <> show c
  | otherwise = callProcess bin args

step :: String -> IO ()
step msg = do
  putStrLn $ "[*] " <> msg
  hFlush stdout

strip :: String -> String
strip = dropWhileEnd isSpace . dropWhile isSpace

requireCommand :: String -> IO ()
requireCommand name = do
  result <- findExecutable name
  case result of
    Just _  -> pure ()
    Nothing -> do
      hPutStrLn stderr $ "Error: '" <> name <> "' not found on PATH."
      hPutStrLn stderr
        "Please install PostgreSQL and ensure its bin directory is on your PATH."
      exitFailure

-- | Find an executable on PATH; fall back to the bare name with a warning.
findBinary :: String -> FilePath -> IO FilePath
findBinary name fallback = do
  result <- findExecutable name
  case result of
    Just p  -> pure p
    Nothing -> do
      hPutStrLn stderr $
        "Warning: '" <> name <> "' not found on PATH, using '" <> fallback <> "'"
      pure fallback

ensureInitialized :: String -> IO FilePath
ensureInitialized commandName = do
  dir <- configDir
  configPath <- configFilePath
  let dataDir = dir </> "data" </> "postgresql"
      pgVersionFile = dataDir </> "PG_VERSION"
  configExists <- doesFileExist configPath
  pgInitialized <- doesFileExist pgVersionFile
  when (not configExists || not pgInitialized) $ do
    hPutStrLn stderr "Error: hmem is not initialized."
    hPutStrLn stderr $
      "Run 'hmem-ctl init' or plain 'hmem-ctl' before 'hmem-ctl "
        <> commandName <> "'."
    exitFailure
  pure dir

isMigrationFile :: FilePath -> Bool
isMigrationFile f = not (null f) && head f == 'V'

installMigrations :: FilePath -> IO (FilePath, [FilePath])
installMigrations hmemDir = do
  srcMigrations <- findMigrationsDir hmemDir
  let destMigrations = hmemDir </> "migrations"
  createDirectoryIfMissing True destMigrations
  files <- filter isMigrationFile <$> listDirectory srcMigrations
  mapM_ (copyReplacing srcMigrations destMigrations) files
  pure (destMigrations, files)

copyReplacing :: FilePath -> FilePath -> FilePath -> IO ()
copyReplacing srcDir destDir fileName = do
  let destPath = destDir </> fileName
  destExists <- doesFileExist destPath
  when destExists $ removeFile destPath
  copyFile (srcDir </> fileName) destPath

-- | Search candidate paths for the migrations directory.
findMigrationsDir :: FilePath -> IO FilePath
findMigrationsDir hmemDir = do
  bundledMigrations <- Paths.getDataFileName "migrations"
  let candidates =
        [ bundledMigrations
        , hmemDir </> "migrations"
        , "sql" </> "migrations"
        , ".." </> "sql" </> "migrations"
        , ".." </> ".." </> "sql" </> "migrations"
        ]
  found <- filterM doesDirectoryExist candidates
  case found of
    (p:_) -> pure p
    []    -> do
      hPutStrLn stderr "Error: migrations directory not found."
      hPutStrLn stderr $ "Searched: " <> show candidates
      hPutStrLn stderr
        "Reinstall hmem-server or run from the project root so the bundled migrations are available."
      exitFailure
