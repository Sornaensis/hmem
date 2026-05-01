#!/usr/bin/env stack
-- stack script --resolver lts-24.2 --package process

-- | Compatibility wrapper for the old integration-test script.
--
-- The duplicated manual PostgreSQL lifecycle and HMEM_TEST_DB override have
-- been retired. Run tests through Stack normally; SpecHook installs the
-- sandboxed hmem test harness, which starts isolated PostgreSQL and scrubs
-- ambient credentials/config paths.
--
-- Usage:
--   stack scripts/run-integration-tests.hs [stack test targets/options]
--
-- Examples:
--   stack scripts/run-integration-tests.hs
--   stack scripts/run-integration-tests.hs hmem-server:test:hmem-server-test --fast

module Main where

import System.Environment (getArgs)
import System.Exit (exitWith)
import System.IO (hFlush, hPutStrLn, stderr, stdout)
import System.Process (createProcess, proc, waitForProcess)

main :: IO ()
main = do
  args <- getArgs
  hPutStrLn stderr "[run-integration-tests] compatibility wrapper: using `stack test` with the sandboxed hmem test harness."
  hPutStrLn stderr "[run-integration-tests] the legacy HMEM_TEST_DB/manual PostgreSQL path has been retired."
  hFlush stderr
  hFlush stdout
  (_, _, _, ph) <- createProcess (proc "stack" ("test" : args))
  waitForProcess ph >>= exitWith
