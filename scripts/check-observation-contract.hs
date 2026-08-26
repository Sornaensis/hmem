#!/usr/bin/env stack
-- stack script --resolver lts-24.2 --package directory

-- | Guard the live Observation MCP contract.  The inventory below is the
-- supported surface: Memory APIs were removed with the Observation migration,
-- so their historical names are deliberately excluded rather than retained as
-- compatibility aliases.
module Main where

import Control.Monad (unless)
import Data.List (intercalate, isInfixOf, isPrefixOf, nub, sort)
import Data.Maybe (mapMaybe)
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath ((</>))

data Tool = Tool { toolName :: String, toolConstructor :: String } deriving (Eq, Show)

-- Server-owned tools mutate/read session state.  Every other tool is parsed
-- and dispatched by HMem.MCP.Tools.
serverOwned :: [String]
serverOwned = ["set_workspace", "get_workspace"]

toolsDispatched :: [Tool]
toolsDispatched =
  [ Tool "workspace_list" "WorkspaceList", Tool "workspace_register" "WorkspaceRegister", Tool "search" "UnifiedSearch"
  , Tool "observation_create" "ObservationCreate", Tool "observation_get" "ObservationGet", Tool "observation_update" "ObservationUpdate"
  , Tool "observation_list" "ObservationList", Tool "observation_delete" "ObservationDelete", Tool "observation_set_embedding" "ObservationSetEmbedding", Tool "observation_similar" "ObservationSimilar"
  , Tool "project_create" "ProjectCreate", Tool "project_update" "ProjectUpdate", Tool "project_detail" "ProjectDetail", Tool "project_overview" "ProjectOverviewCall", Tool "project_next_tasks" "ProjectNextTasks", Tool "project_spec" "ProjectSpec", Tool "project_archive" "ProjectArchive"
  , Tool "task_create" "TaskCreate", Tool "task_update" "TaskUpdate", Tool "task_detail" "TaskDetail", Tool "task_overview" "TaskOverviewCall", Tool "task_start" "TaskStart", Tool "task_finish" "TaskFinish"
  ]

liveTools :: [String]
liveTools = serverOwned <> map toolName toolsDispatched

-- These are intentionally forbidden only in live MCP implementation files.
-- Tests and this guard mention them to prove the migration boundary.
legacyAliases :: [String]
legacyAliases = ["memory_create", "memory_get", "memory_update", "memory_link", "link_memory", "context_get", "/api/v1/memories"]

sourceFiles :: [(FilePath, FilePath)]
sourceFiles =
  [ ("Tools.hs", "hmem-mcp/src/HMem/MCP/Tools.hs")
  , ("Server.hs", "hmem-mcp/src/HMem/MCP/Server.hs")
  , ("ToolsSpec.hs", "hmem-mcp/test/HMem/MCP/ToolsSpec.hs")
  , ("ServerSpec.hs", "hmem-mcp/test/HMem/MCP/ServerSpec.hs")
  , ("compact fixture", "hmem-mcp/test/fixtures/mcp-compact-responses.json")
  ]

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> runAt "."
    ["--self-test"] -> selfTest
    ["--root", root] -> runAt root
    _ -> failContract ["usage: check-observation-contract.hs [--root PATH | --self-test]"]

runAt :: FilePath -> IO ()
runAt root = do
  loaded <- mapM (load root) sourceFiles
  let missing = [label | (label, Nothing) <- loaded]
  unless (null missing) $ failContract ["missing scanned file(s): " <> intercalate ", " missing]
  let sources = [(label, content) | (label, Just content) <- loaded]
      violations = guardSources sources
  if null violations
    then putStrLn $ "Observation MCP contract scan passed (" <> show (length liveTools) <> " live tools: " <> intercalate ", " liveTools <> ")."
    else failContract violations

load :: FilePath -> (FilePath, FilePath) -> IO (FilePath, Maybe String)
load root (label, path) = do
  let fullPath = root </> path
  exists <- doesFileExist fullPath
  content <- if exists then Just <$> readFile fullPath else pure Nothing
  pure (label, content)

guardSources :: [(FilePath, String)] -> [String]
guardSources sources = concat
  [ registryChecks toolsSource
  , parserAndDispatchChecks toolsSource
  , sessionChecks serverSource
  , legacyChecks [("Tools.hs", toolsSource), ("Server.hs", serverSource)]
  , testCoverageChecks toolsSpec serverSpec fixture
  ]
  where
    source label = maybe "" id (lookup label sources)
    toolsSource = source "Tools.hs"
    serverSource = source "Server.hs"
    toolsSpec = source "ToolsSpec.hs"
    serverSpec = source "ServerSpec.hs"
    fixture = source "compact fixture"

registryChecks :: String -> [String]
registryChecks source =
  mismatch "registry" liveTools (registeredTools source)
  <> require "set_workspace schema must allow string|null" ("nullableProp" `isInfixOf` source && "\"null\"" `isInfixOf` source)

parserAndDispatchChecks :: String -> [String]
parserAndDispatchChecks source = concatMap check toolsDispatched
  where
    dispatchBody = dropWhileNot ("execute manager base apiKey" `isPrefixOf`) (lines source)
    check tool =
      require ("parser mapping missing: " <> toolName tool <> " -> " <> toolConstructor tool)
        (("\"" <> toolName tool <> "\" -> " <> toolConstructor tool) `isInfixOf` source)
      <> require ("dispatcher case missing constructor: " <> toolConstructor tool)
        (any (isPrefixOf ("  " <> toolConstructor tool <> " ")) dispatchBody)

sessionChecks :: String -> [String]
sessionChecks source = concat
  [ require "Server-owned set_workspace dispatch is missing" ("Just \"set_workspace\" -> handleSetWorkspace" `isInfixOf` source)
  , require "Server-owned get_workspace dispatch is missing" ("Just \"get_workspace\" -> handleGetWorkspaceSnapshot" `isInfixOf` source)
  , require "ordered input control barrier is missing" (all (`isInfixOf` source) ["QueuedLine", "isOrderedContextControl", "workspaceSnapshot"])
  , require "queued scoped calls do not use a captured workspace snapshot" ("injectWorkspaceContextValue capturedWorkspace" `isInfixOf` source)
  , require "explicit workspace_id precedence is missing" ("not (KM.member \"workspace_id\" args)" `isInfixOf` source)
  ]

legacyChecks :: [(FilePath, String)] -> [String]
legacyChecks files =
  [ path <> ": forbidden removed-Memory alias `" <> alias <> "` in live MCP implementation"
  | (path, source) <- files, alias <- legacyAliases, alias `isInfixOf` lower source
  ]

testCoverageChecks :: String -> String -> String -> [String]
testCoverageChecks toolsSpec serverSpec fixture = concat
  [ concatMap (\name -> require ("ToolsSpec lacks current-tool coverage: " <> name) (quoted name `isInfixOf` toolsSpec)) liveTools
  , require "ToolsSpec lacks JSON-RPC current-tool coverage" ("JSON-RPC" `isInfixOf` toolsSpec && "jsonRpcToolCall" `isInfixOf` toolsSpec)
  , require "ServerSpec lacks workspace ordering coverage" (all (`isInfixOf` serverSpec) ["workspace context ordering", "call-a", "call-after-clear"])
  , concatMap (\name -> require ("compact fixture missing current Observation payload: " <> name) (quoted name `isInfixOf` fixture)) compactFixtureTools
  ]

compactFixtureTools :: [String]
compactFixtureTools = ["observation_create", "unified_search", "observation_list", "observation_delete", "observation_set_embedding", "observation_similar"]

registeredTools :: String -> [String]
registeredTools = mapMaybe registration . lines
  where
    registration line = case breakOn "tool \"" line of
      Just (_, candidate) -> Just $ takeWhile (/= '"') (drop 6 candidate)
      _ -> Nothing

dropWhileNot :: (a -> Bool) -> [a] -> [a]
dropWhileNot _ [] = []
dropWhileNot predicate values@(value:rest)
  | predicate value = values
  | otherwise = dropWhileNot predicate rest

quoted :: String -> String
quoted value = "\"" <> value <> "\""

mismatch :: String -> [String] -> [String] -> [String]
mismatch label expected actual =
  [label <> " missing live tool(s): " <> intercalate ", " missing | not (null missing)]
  <> [label <> " has unexpected tool(s): " <> intercalate ", " unexpected | not (null unexpected)]
  <> [label <> " contains duplicate tool(s): " <> intercalate ", " duplicates | not (null duplicates)]
  where
    missing = filter (`notElem` actual) expected
    unexpected = filter (`notElem` expected) actual
    duplicates = nub [value | value <- values, length (filter (== value) values) > 1]
    values = actual

require :: String -> Bool -> [String]
require message condition = [message | not condition]

selfTest :: IO ()
selfTest = do
  loaded <- mapM (load ".") sourceFiles
  let sources = [(label, content) | (label, Just content) <- loaded]
      mutateTools f = [if label == "Tools.hs" then (label, f content) else (label, content) | (label, content) <- sources]
      staleAlias = guardSources (mutateTools (<> "\nlegacyAlias = \"memory_create\"\n"))
      missingTool = guardSources (mutateTools (replaceFirst "tool \"observation_get\"" "tool \"observation_missing\""))
      dispatcherMismatch = guardSources (mutateTools (replaceFirst "ObservationGet oid" "MissingObservationGet oid"))
  unless (null (guardSources sources)) $ failContract ["self-test baseline unexpectedly failed"]
  requireSelfTest "stale legacy alias" "forbidden removed-Memory alias `memory_create`" staleAlias
  requireSelfTest "missing accepted tool" "registry missing live tool(s): observation_get" missingTool
  requireSelfTest "registry/dispatcher mismatch" "dispatcher case missing constructor: ObservationGet" dispatcherMismatch
  putStrLn "Observation MCP contract self-test passed (baseline, stale alias, missing tool, and dispatcher mismatch fixtures)."

requireSelfTest :: String -> String -> [String] -> IO ()
requireSelfTest label expected diagnostics =
  unless (any (expected `isInfixOf`) diagnostics) $
    failContract ["self-test " <> label <> " fixture did not report `" <> expected <> "`"]

replaceFirst :: String -> String -> String -> String
replaceFirst needle replacement haystack = case breakOn needle haystack of
  Nothing -> haystack
  Just (before, after) -> before <> replacement <> drop (length needle) after

breakOn :: String -> String -> Maybe (String, String)
breakOn needle = go []
  where
    go _ [] = Nothing
    go before rest@(value:remaining)
      | needle `isPrefixOf` rest = Just (reverse before, rest)
      | otherwise = go (value : before) remaining

lower :: String -> String
lower = map lowerAscii
  where
    lowerAscii c | 'A' <= c && c <= 'Z' = toEnum (fromEnum c + 32) | otherwise = c

failContract :: [String] -> IO a
failContract messages = do
  putStrLn "Observation MCP contract scan failed:"
  mapM_ (putStrLn . ("  - " <>)) messages
  exitFailure
