-- | Spins up an ephemeral PostgreSQL instance, runs the integration
--   tests against it, then tears everything down.
--
--   Usage:
--     stack run run-integration-tests                    -- run all tests
--     stack run run-integration-tests -- hmem-core       -- run only core tests
--
--   Requires: initdb, pg_ctl, createdb on PATH (ships with PostgreSQL).

module Main where

import Control.Concurrent (threadDelay)
import Control.Exception  (SomeException, finally, try)
import System.Directory   (findExecutable, removeDirectoryRecursive)
import System.Environment (getArgs, setEnv)
import System.Exit        (ExitCode (..), exitWith, exitFailure)
import System.FilePath    ((</>), (<.>))
import System.IO          (hFlush, hPutStrLn, stderr, stdout)
import System.Info        (os)
import System.IO.Temp     (createTempDirectory, getCanonicalTemporaryDirectory)
import System.Process     (callProcess, createProcess, proc, readProcess,
                           waitForProcess)
import System.Random      (randomRIO)

main :: IO ()
main = do
  -- Check that PostgreSQL tools are on PATH before doing anything else.
  missing <- fmap (map fst . filter (null . snd)) $
    mapM (\cmd -> findExecutable cmd >>= \r -> pure (cmd, maybe [] pure r))
      ["initdb", "pg_ctl", "createdb"]
  case missing of
    [] -> pure ()
    _  -> do
      hPutStrLn stderr $ "ERROR: Required PostgreSQL tools not found on PATH: "
        ++ unwords missing
      hPutStrLn stderr ""
      hPutStrLn stderr "Install PostgreSQL and ensure its bin directory is on your PATH."
      hPutStrLn stderr "  MSYS2:   pacman -S mingw-w64-x86_64-postgresql"
      hPutStrLn stderr "  Ubuntu:  sudo apt install postgresql"
      hPutStrLn stderr "  macOS:   brew install postgresql"
      exitFailure

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
        threadDelay 500000

        -- 4. Create the test database
        putStrLn "[3/5] createdb hmem_test..."
        hFlush stdout
        callProcess "createdb"
          [ "-h", "localhost"
          , "-p", show port
          , "hmem_test"
          ]

        -- 5. Build the test suite.  On Windows the copy/register step may
        --    fail because this very executable is locked, so we catch and
        --    ignore that error — the test binary is still linked in
        --    .stack-work and we can run it directly.
        putStrLn "[4/6] Building test suite..."
        hFlush stdout
        _ <- try (callProcess "stack" ("build" : "--test" : "--no-run-tests" : args))
               :: IO (Either SomeException ())

        -- 6. Locate the test binary in the build directory and run it.
        putStrLn "[5/6] Running tests..."
        putStrLn ""
        hFlush stdout
        setEnv "HMEM_TEST_DB" connStr

        distDir <- fmap (Prelude.takeWhile (/= '\n')) $
                     readProcess "stack" ["path", "--dist-dir"] ""
        let testExe = "hmem-core" </> distDir </> "build" </> "hmem-core-test"
                               </> "hmem-core-test" <.> exeExt
        (_, _, _, ph) <- createProcess (proc testExe [])
        waitForProcess ph

      teardown = do
        putStrLn ""
        putStrLn "[6/6] Tearing down..."
        hFlush stdout
        _ <- try (callProcess "pg_ctl"
              [ "stop", "-D", dataDir, "-m", "fast" ]
              ) :: IO (Either SomeException ())
        -- Small delay so Windows releases file locks
        threadDelay 500000
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

-- | Platform-appropriate executable file extension.
exeExt :: String
exeExt
  | os == "mingw32" = "exe"
  | otherwise       = ""
