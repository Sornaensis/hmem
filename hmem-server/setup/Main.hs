-- | One-shot setup tool for the hmem environment.
--
--   @hmem-setup@            – full setup (init + install auto-run)
--   @hmem-setup init@       – create ~/.hmem/, PostgreSQL data dir, database, schema
--   @hmem-setup install@    – register auto-run services (requires init first)
--   @hmem-setup start@      – start PostgreSQL, apply pending migrations, then start hmem-server
--   @hmem-setup stop@       – stop hmem-server + PostgreSQL
--   @hmem-setup status@     – show service status
--   @hmem-setup uninstall@  – stop services, remove auto-run, delete ~/.hmem/
--   @hmem-setup reinstall@  – uninstall + full setup
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
import Data.Char          (isSpace)
import Data.List          (isInfixOf, dropWhileEnd)
import Data.Text qualified as T
import Options.Applicative
import System.Directory   (createDirectoryIfMissing, copyFile,
                           doesDirectoryExist, doesFileExist,
                           findExecutable, getHomeDirectory,
                           listDirectory,
                           removeDirectoryRecursive, removeFile)
import System.Exit        (exitFailure)
import System.FilePath    ((</>))
import System.IO          (hFlush, hPutStrLn, stderr, stdout)
import System.Info        (os)
import System.Process     (callProcess, readProcess, spawnProcess)

import HMem.Config
import HMem.DB.Migration qualified as Migration
import HMem.DB.Pool qualified as Pool
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
  )
  <|> pure CmdSetup

main :: IO ()
main = do
  cmd <- execParser $ info (commandParser <**> helper)
    ( fullDesc
   <> progDesc "hmem-setup - Initialize and manage the hmem environment"
   <> header "hmem-setup"
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

------------------------------------------------------------------------
-- Init
------------------------------------------------------------------------

doInit :: IO ()
doInit = do
  putStrLn "=== hmem-setup init ==="
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
  pool <- Pool.createPool connStr 2 60
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
  putStrLn "=== hmem-setup install ==="
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
      logDir  = dir </> "logs"

  pgCtlBin  <- findBinary "pg_ctl" "pg_ctl"
  serverBin <- findBinary "hmem-server" "hmem-server"

  -- Write startup script
  step "Writing startup scripts"
  let startScript = dir </> "start-hmem.bat"
  writeFile startScript $ unlines
    [ "@echo off"
    , "\"" <> pgCtlBin <> "\" start -D \"" <> dataDir
        <> "\" -l \"" <> logDir </> "postgresql.log" <> "\" -w"
    , "timeout /t 2 /nobreak >nul"
    , "start /b \"hmem-server\" \"" <> serverBin <> "\""
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
  exists <- doesFileExist path
  if exists
    then putStrLn $ "  Skipped (exists): " <> path
    else do
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
  , "- **project_create** \\u2014 Create projects with name, description, priority. Supports sub-projects."
  , "- **project_list** \\u2014 Filter by status (active, paused, completed, archived)."
  , "- **project_update** / **project_delete** \\u2014 Modify or remove projects."
  , "- **project_link_memory** \\u2014 Attach relevant memories to a project."
  , ""
  , "## Tasks"
  , "- **task_create** \\u2014 Create with title, description, priority, optional due_at and project_id."
  , "- **task_list** \\u2014 Filter by workspace, project, or status (todo, in_progress, blocked, done, cancelled)."
  , "- **task_update** \\u2014 Move through statuses. Setting done auto-records completion time."
  , "- **task_dependency_add** / **task_dependency_remove** \\u2014 Define task ordering."
  , "- **task_link_memory** \\u2014 Attach context to tasks."
  , ""
  , "## Guidelines"
  , ""
  , "1. **Structure hierarchically** \\u2014 Projects > tasks > sub-tasks."
  , "2. **Set clear statuses** \\u2014 todo \\u2192 in_progress \\u2192 done. Use blocked/cancelled as needed."
  , "3. **Use dependencies** \\u2014 When tasks have natural ordering."
  , "4. **Link memories to tasks** \\u2014 For traceability."
  , "5. **Priority conventions** \\u2014 1-3: low, 4-6: normal, 7-8: high, 9-10: urgent."
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
  , "Projects: project_create, project_get, project_list, project_update, project_delete"
  , "Tasks: task_create, task_get, task_list, task_update, task_delete"
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
  , "- project_create / project_list / project_update \\u2014 Manage projects (active, paused, completed, archived)."
  , "- task_create / task_list / task_update \\u2014 Manage tasks (todo, in_progress, blocked, done, cancelled)."
  , "- task_dependency_add / task_dependency_remove \\u2014 Task ordering."
  , "- project_link_memory / task_link_memory \\u2014 Attach knowledge to work items."
  , ""
  , "## Workspaces"
  , "- workspace_register / workspace_list / workspace_get \\u2014 All data is scoped to workspaces."
  , "- workspace_group_create / workspace_group_add_member \\u2014 Organize workspaces."
  , ""
  , "## Best Practices"
  , "1. Search before creating to avoid duplicates."
  , "2. Use supersedes when newer info replaces older; contradicts for conflicts."
  , "3. Set importance: 1-3 background, 4-6 useful, 7-8 important, 9-10 critical."
  , "4. Always include 2-5 descriptive tags."
  , "5. Use long_term for durable knowledge, short_term for transient context."
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

  putStrLn "Starting PostgreSQL..."
  callProcess "pg_ctl"
    [ "start", "-D", dataDir
    , "-l", logDir </> "postgresql.log"
    , "-w", "-t", "30"
    ]
  threadDelay 1000000

  step "Refreshing installed migrations"
  (migrationsDir, migrationFiles) <- installMigrations dir
  putStrLn $ "  Installed " <> show (length migrationFiles)
    <> " migration(s) to " <> migrationsDir

  step "Applying pending migrations"
  let connStr = connectionString cfg.database
  pool <- Pool.createPool connStr 2 60
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
  _ <- spawnProcess "hmem-server" []
  putStrLn $ "hmem-server running on port " <> show cfg.server.port

doStop :: IO ()
doStop = do
  dir <- configDir
  let dataDir = dir </> "data" </> "postgresql"

  putStrLn "Stopping hmem-server..."
  _ <- try stopServer :: IO (Either SomeException ())

  putStrLn "Stopping PostgreSQL..."
  _ <- try (callProcess "pg_ctl" ["stop", "-D", dataDir, "-m", "fast"])
        :: IO (Either SomeException ())
  putStrLn "Stopped."
  where
    stopServer
      | isWindows = callProcess "taskkill" ["/IM", "hmem-server.exe", "/F"]
      | otherwise = callProcess "pkill" ["-f", "hmem-server"]

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
  putStrLn "=== hmem-setup uninstall ==="
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
-- Helpers
------------------------------------------------------------------------

isWindows :: Bool
isWindows = os == "mingw32"

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
      "Run 'hmem-setup init' or plain 'hmem-setup' before 'hmem-setup "
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
