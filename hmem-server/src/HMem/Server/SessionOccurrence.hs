-- | Independent fair reconciliation of derived transcript occurrence metadata.
module HMem.Server.SessionOccurrence
  ( runOccurrenceWorker ) where

import Control.Concurrent
import Control.Exception
import Control.Monad (when)
import Data.Pool (Pool)
import Hasql.Connection qualified as H
import System.IO (hPutStrLn, stderr)
import HMem.DB.AgentSession (deriveSessionOccurrence)
import HMem.DB.SessionOccurrence (nextCandidate)
import HMem.DB.RequestContext

-- The archive service owns this independent loop through its existing actual
-- thread capability and finite join/failure mechanism.
runOccurrenceWorker :: Pool H.Connection -> IO ()
runOccurrenceWorker pool = loop Nothing
  where
    loop cursor = do
      next <- synchronous (nextCandidate pool cursor) (pure Nothing)
      case next of
        Nothing -> threadDelay 1000000 >> loop Nothing
        Just (workspace,identifier,needed) -> do
          when needed $ synchronous
            (withRequestContext (emptyRequestContext {workspaceId=Just workspace,changeCause=Just ChangeCauseCore}) $
              deriveSessionOccurrence pool workspace identifier)
            (hPutStrLn stderr "session-occurrence: derivation failed; retrying next sweep")
          threadDelay 10000
          loop (Just identifier)

synchronous :: IO a -> IO a -> IO a
synchronous action fallback = action `catch` \(failure :: SomeException) ->
  case fromException failure :: Maybe SomeAsyncException of
    Just _ -> throwIO failure
    Nothing -> fallback
