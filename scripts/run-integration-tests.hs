#!/usr/bin/env stack
-- stack script --resolver lts-24.2 --package process --package directory --package temporary --package random

-- | Spins up an ephemeral PostgreSQL instance, runs the integration
--   tests against it, then tears everything down.
--
--   Usage:
--     stack scripts/run-integration-tests.hs              -- run all tests
--     stack scripts/run-integration-tests.hs hmem-core    -- run only core tests
--
--   Requires: initdb, pg_ctl, createdb on PATH (ships with PostgreSQL).

module Main where

import Control.Concurrent (threadDelay)
import Control.Exception  (SomeException, finally, try)
import Data.List          (isPrefixOf)
import System.Directory   (removeDirectoryRecursive)
import System.Environment (getArgs, setEnv)
import System.Exit        (ExitCode (..), exitFailure, exitWith)
import System.FilePath    ((</>))
import System.IO          (hFlush, hPutStrLn, stderr, stdout)
import System.IO.Temp     (createTempDirectory, getCanonicalTemporaryDirectory)
import System.Process     (callProcess, createProcess, proc, readProcess,
                           waitForProcess)
import System.Random      (randomRIO)

main :: IO ()
main = do
  args <- getArgs

  -- Pick a random ephemeral port so parallel runs don't collide.
  port <- randomRIO (49152, 65535) :: IO Int

  tmpBase <- getCanonicalTemporaryDirectory
  tmpDir  <- createTempDirectory tmpBase "hmem-test-pg"

  let dataDir = tmpDir </> "data"
      logFile = tmpDir </> "pg.log"
      connStr = "host=localhost port=" ++ show port ++ " dbname=hmem_test"

  putStrLn "=== hmem integration tests ==="
  putStrLn $ "  tmp dir : " ++ tmpDir
  putStrLn $ "  port    : " ++ show port
  putStrLn $ "  conn    : " ++ connStr
  putStrLn ""
  hFlush stdout

  let run = do
        -- 1. Initialise a fresh data directory
        putStrLn "[1/5] initdb..."
        hFlush stdout
        callProcess "initdb"
          [ "-D", dataDir
          , "--auth=trust"
          , "--no-instructions"
          , "--no-locale"
          , "-E", "UTF8"
          ]

        -- 2. Configure to use our port / localhost only
        appendFile (dataDir </> "postgresql.conf") $ unlines
          [ ""
          , "# hmem test overrides"
          , "port = " ++ show port
          , "listen_addresses = 'localhost'"
          ]

        -- 3. Start PostgreSQL
        putStrLn "[2/5] pg_ctl start..."
        hFlush stdout
        callProcess "pg_ctl"
          [ "start"
          , "-D", dataDir
          , "-l", logFile
          , "-w"                 -- wait until ready
          , "-t", "30"           -- timeout 30 s
          ]

        -- Give the TCP listener an extra moment
        threadDelay 500_000

        -- 4. Create the test database
        putStrLn "[3/5] createdb hmem_test..."
        hFlush stdout
        callProcess "createdb"
          [ "-h", "localhost"
          , "-p", show port
          , "hmem_test"
          ]

        -- 5. Run the test suite
        putStrLn "[4/5] stack test..."
        putStrLn ""
        hFlush stdout
        setEnv "HMEM_TEST_DB" connStr

        let stackArgs = "test" : args   -- pass through any extra args
        (_, _, _, ph) <- createProcess (proc "stack" stackArgs)
        waitForProcess ph

      teardown = do
        putStrLn ""
        putStrLn "[5/5] Tearing down..."
        hFlush stdout
        _ <- try (callProcess "pg_ctl"
              [ "stop", "-D", dataDir, "-m", "fast" ]
              ) :: IO (Either SomeException ())
        -- Small delay so Windows releases file locks
        threadDelay 500_000
        _ <- try (removeDirectoryRecursive tmpDir
              ) :: IO (Either SomeException ())
        pure ()

  exitCode <- run `finally` teardown

  putStrLn ""
  case exitCode of
    ExitSuccess -> putStrLn "All tests passed."
    code -> do
      hPutStrLn stderr $ "Tests failed (" ++ show code ++ ")."
      hPutStrLn stderr $ "Postgres log: " ++ logFile
      exitWith code
