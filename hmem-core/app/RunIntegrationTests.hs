-- | Compatibility entry point for the old integration-test runner.
--
-- The previous implementation duplicated PostgreSQL lifecycle management and
-- routed tests through HMEM_TEST_DB. That is intentionally retired: default
-- test execution now goes through hspec SpecHook + HMem.DB.TestHarness, which
-- creates a fail-closed sandbox PostgreSQL instance and scrubs ambient user
-- configuration.
--
-- Usage:
--   stack run run-integration-tests -- [stack test targets/options]
--
-- Examples:
--   stack run run-integration-tests                    -- hmem-core tests
--   stack run run-integration-tests -- hmem-core:test:hmem-core-test --fast

module Main where

import System.Environment (getArgs)
import System.Exit (exitWith)
import System.IO (hFlush, hPutStrLn, stderr, stdout)
import System.Process (createProcess, proc, waitForProcess)

main :: IO ()
main = do
  args <- getArgs
  let stackArgs = if null args then ["hmem-core:test:hmem-core-test"] else args
  hPutStrLn stderr "[run-integration-tests] compatibility wrapper: using `stack test` with the sandboxed hmem test harness."
  hPutStrLn stderr "[run-integration-tests] the legacy HMEM_TEST_DB/manual PostgreSQL path has been retired."
  hPutStrLn stderr "[run-integration-tests] no args defaults to hmem-core:test:hmem-core-test; pass Stack test targets/options after `--`."
  hPutStrLn stderr "[run-integration-tests] no stack install or global hmem binary is required."
  hFlush stderr
  hFlush stdout
  (_, _, _, ph) <- createProcess (proc "stack" ("test" : stackArgs))
  waitForProcess ph >>= exitWith
