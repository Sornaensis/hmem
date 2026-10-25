-- | Independently owned cleanup of abandoned, task-complete session evidence.
module HMem.Server.SessionReaper
  ( SessionReaper, startSessionReaper, startSessionReaperWithDelay, stopSessionReaper
  , reapSessionBatch
  ) where

import Control.Concurrent (MVar, ThreadId, forkIOWithUnmask, killThread, newEmptyMVar, putMVar, readMVar, threadDelay)
import Control.Exception (AsyncException(..), SomeException, catch, fromException, mask_, throwIO, try)
import Control.Monad (forM_)
import Data.Pool (Pool)
import Hasql.Connection qualified as Hasql
import System.IO (hPutStrLn, stderr)
import System.Timeout (timeout)

import HMem.AgentSession (AgentSession(..))
import HMem.DB.AgentSession (IdleSessionCursor, listIdleSessionCandidates, sealIdleSession)
import HMem.DB.RequestContext (ChangeCause(..), RequestContext(..), emptyRequestContext, withRequestContext)

data SessionReaper = SessionReaper ThreadId (MVar (Either SomeException ()))

startSessionReaper :: Pool Hasql.Connection -> IO SessionReaper
startSessionReaper = startSessionReaperWithDelay 60000000 (pure ())

-- | Scan immediately at startup, then at the supplied interval. A small fixed
-- page and one hash at a time bound connections, memory and concurrent work.
startSessionReaperWithDelay :: Int -> IO () -> Pool Hasql.Connection -> IO SessionReaper
startSessionReaperWithDelay delay afterBatch pool = mask_ $ do
  finished <- newEmptyMVar
  worker <- forkIOWithUnmask $ \unmask -> do
    result <- try (unmask (loop Nothing))
    putMVar finished result
  pure (SessionReaper worker finished)
  where
    loop cursor = do
      next <- synchronous (reapSessionBatch pool cursor)
        (hPutStrLn stderr "session-reaper: metadata scan failed; retrying" >> pure cursor)
      afterBatch
      threadDelay (max 1 delay)
      loop next

-- | Advance past ineligible or failed records, so neither can starve later
-- candidates. An empty page wraps the keyset for the next reconciliation.
reapSessionBatch :: Pool Hasql.Connection -> Maybe IdleSessionCursor -> IO (Maybe IdleSessionCursor)
reapSessionBatch pool cursor = withRequestContext
  (emptyRequestContext { changeCause = Just ChangeCauseCore }) $ do
    rows <- listIdleSessionCandidates pool cursor
    forM_ rows $ \row -> synchronous
      (withRequestContext (emptyRequestContext
        { workspaceId = Just row.workspaceId, changeCause = Just ChangeCauseCore }) $
        sealIdleSession pool row >> pure ())
      (hPutStrLn stderr "session-reaper: candidate failed; retrying on a later sweep")
    pure $ case reverse rows of
      [] -> Nothing
      row:_ -> Just (row.lastSeenAt,row.id)

stopSessionReaper :: SessionReaper -> IO ()
stopSessionReaper (SessionReaper worker finished) = do
  stopped <- timeout 5000000 (killThread worker >> readMVar finished)
  case stopped of
    Nothing -> throwIO (userError "session-reaper: shutdown exceeded five seconds")
    Just (Right ()) -> pure ()
    Just (Left failure) -> case fromException failure :: Maybe AsyncException of
      Just ThreadKilled -> pure ()
      _ -> throwIO failure

synchronous :: IO a -> IO a -> IO a
synchronous action fallback = action `catch` \(failure :: SomeException) ->
  case fromException failure :: Maybe AsyncException of
    Just _ -> throwIO failure
    Nothing -> fallback
