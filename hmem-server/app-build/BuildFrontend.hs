-- | Build the Elm/Vite frontend, outputting to hmem-server\/static\/.
--
-- Usage:
--   stack run build-frontend            -- install deps + build
--   stack run build-frontend -- --clean  -- remove node_modules & elm-stuff first
--
-- Requires: npm on PATH (ships with Node.js).
-- If npm is not found the tool prints a warning and exits successfully
-- so that a full @stack build@ never fails due to a missing frontend
-- toolchain.

module Main where

import Control.Exception (SomeException, try)
import Control.Monad    (when)
import System.Directory (doesDirectoryExist, getCurrentDirectory,
                         removeDirectoryRecursive)
import System.Environment (getArgs)
import System.Exit      (ExitCode (..), exitWith)
import System.FilePath  ((</>))
import System.IO        (hFlush, hPutStrLn, stderr, stdout)
import System.Process   (CreateProcess(..), shell, readCreateProcessWithExitCode,
                         readProcessWithExitCode)

main :: IO ()
main = do
  args <- getArgs
  let clean = "--clean" `elem` args

  -- Locate the frontend directory relative to the working directory.
  -- We expect to be invoked from the project root (stack default).
  cwd <- getCurrentDirectory
  let frontendDir = cwd </> "hmem-server" </> "frontend"

  hasFrontend <- doesDirectoryExist frontendDir
  if not hasFrontend
    then do
      hPutStrLn stderr $
        "WARNING: frontend directory not found at " ++ frontendDir
      hPutStrLn stderr "  Skipping frontend build."
    else do
      -- Check that npm is available
      npmOk <- hasExecutable "npm"
      if not npmOk
        then do
          hPutStrLn stderr "WARNING: npm not found on PATH — skipping frontend build."
          hPutStrLn stderr "  Install Node.js/npm to build the web frontend."
          hPutStrLn stderr "  The server will run without the web UI."
        else do
          when clean $ do
            putStrLn "Cleaning frontend build artifacts..."
            removeIfExists (frontendDir </> "node_modules")
            removeIfExists (frontendDir </> "elm-stuff")
            removeIfExists (cwd </> "hmem-server" </> "static")

          -- Install dependencies if needed
          hasNodeModules <- doesDirectoryExist (frontendDir </> "node_modules")
          when (not hasNodeModules) $ do
            putStrLn "Installing frontend dependencies..."
            hFlush stdout
            runInDir frontendDir "npm" ["install"]

          -- Build
          putStrLn "Building frontend..."
          hFlush stdout
          runInDir frontendDir "npm" ["run", "build"]
          putStrLn "Frontend built -> hmem-server/static/"

-- | Check whether an executable is on PATH.
hasExecutable :: String -> IO Bool
hasExecutable name = do
  result <- try @SomeException $ readProcessWithExitCode "where" [name] ""
  case result of
    Right (ExitSuccess, _, _) -> pure True
    _ -> do
      -- Fall back to 'which' for non-Windows systems
      result2 <- try @SomeException $ readProcessWithExitCode "which" [name] ""
      case result2 of
        Right (ExitSuccess, _, _) -> pure True
        _                         -> pure False

-- | Run a command in a specific working directory.
-- Uses shell so that .cmd files (like npm on Windows) are resolved
-- through the system shell, which also handles PATHEXT correctly.
runInDir :: FilePath -> String -> [String] -> IO ()
runInDir dir cmd args = do
  let cmdLine = unwords (cmd : map quoteArg args)
      cp = (shell cmdLine) { cwd = Just dir }
  (ec, out, err) <- readCreateProcessWithExitCode cp ""
  when (not (null out)) $ putStr out
  when (not (null err)) $ hPutStrLn stderr err
  case ec of
    ExitSuccess   -> pure ()
    ExitFailure c -> do
      hPutStrLn stderr $ cmd ++ " failed with exit code " ++ show c
      exitWith ec

-- | Remove a directory if it exists.
removeIfExists :: FilePath -> IO ()
removeIfExists path = do
  exists <- doesDirectoryExist path
  when exists $ do
    putStrLn $ "  Removing " ++ path
    removeDirectoryRecursive path

-- | Quote an argument if it contains spaces.
quoteArg :: String -> String
quoteArg s
  | any (== ' ') s = "\"" ++ s ++ "\""
  | otherwise      = s
