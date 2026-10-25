-- | Lifecycle-owned, bounded verification of immutable committed prefixes.
module HMem.Server.SessionPrefix
  ( PrefixRegistry, PrefixTarget(..), PrefixOptions(..), PrefixFault(..)
  , newPrefixRegistry, newPrefixRegistryWith, newPrefixRegistryWithWorkerHook, closePrefixRegistry
  , submitPrefixProof, lookupPrefixProof, prefixRegistryCounts
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad (forM_, unless, when)
import Crypto.Hash (Context, Digest, SHA256, hashInit, hashUpdate, hashFinalize)
import Data.ByteString qualified as BS
import Data.Int (Int64)
import Data.List (sortOn)
import Data.Map.Strict qualified as Map
import Data.Pool (Pool)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Hasql.Connection qualified as Hasql
import System.Timeout (timeout)

import HMem.AgentSession.Transport qualified as Wire
import HMem.DB.AgentSession qualified as DB

data PrefixTarget = PrefixTarget
  { workspace :: !UUID, session :: !UUID, audience :: !Text, request :: !Wire.PrefixProofRequest }
  deriving (Show, Eq)
data PrefixOptions = PrefixOptions
  { currentTime :: IO UTCTime
  , validateTarget :: PrefixTarget -> IO Bool
  , readWindow :: PrefixTarget -> Int64 -> IO BS.ByteString
  }
data PrefixFault = PrefixInvalid | PrefixConflict | PrefixBusy | PrefixClosed deriving (Show, Eq)
data Progress = Progress
  { offset :: !Int64, context :: !(Context SHA256), phase :: !Text
  , failure :: !(Maybe Text), completedAt :: !(Maybe UTCTime) }
data Job = Job { target :: !PrefixTarget, startedAt :: !UTCTime, progress :: !(TVar Progress) }
data PrefixRegistry = PrefixRegistry
  { entries :: TVar (Map.Map UUID Job), ready :: TBQueue Job, closed :: TVar Bool
  , workers :: [(ThreadId, MVar (Either SomeException ()))], options :: PrefixOptions }

newPrefixRegistry :: Pool Hasql.Connection -> IO PrefixRegistry
newPrefixRegistry pool = newPrefixRegistryWith $ PrefixOptions getCurrentTime
  (\target -> DB.validateSessionPrefixBoundary pool target.workspace target.session target.request.expectedOffset target.request.expectedRevision)
  (\target -> DB.readSessionPrefixWindow pool target.workspace target.session target.request.expectedOffset)

newPrefixRegistryWith :: PrefixOptions -> IO PrefixRegistry
newPrefixRegistryWith = newPrefixRegistryWithWorkerHook (\_ action -> action)

-- A worker-lifetime seam permits causal cancellation/finalization tests without
-- changing production worker or queue limits.
newPrefixRegistryWithWorkerHook :: (Int -> IO () -> IO ()) -> PrefixOptions -> IO PrefixRegistry
newPrefixRegistryWithWorkerHook hook options = mask_ $ do
  entries <- newTVarIO Map.empty; ready <- newTBQueueIO 4; closed <- newTVarIO False
  let registry = PrefixRegistry entries ready closed [] options
      spawn index = do
        done <- newEmptyMVar
        tid <- forkIOWithUnmask $ \unmask -> do
          result <- try (unmask (hook index (worker registry)))
          atomically $ writeTVar closed True
          putMVar done result
        pure (tid,done)
  first <- spawn 0
  second <- spawn 1 `onException` stopWorkers [first]
  pure registry { workers = [first,second] }

stopWorkers :: [(ThreadId, MVar (Either SomeException ()))] -> IO ()
stopWorkers workers = do
  stopped <- timeout 5000000 $ do
    forM_ workers (killThread . fst)
    mapM (readMVar . snd) workers
  case stopped of
    Nothing -> throwIO (userError "prefix verification shutdown exceeded five seconds")
    Just results -> do
      let failures = [failure | Left failure <- results, Nothing <- [fromException failure :: Maybe SomeAsyncException]]
      unless (null failures) $ throwIO (userError ("prefix verification workers failed after all joins: " <> show (length failures)))

closePrefixRegistry :: PrefixRegistry -> IO ()
closePrefixRegistry registry = do
  atomically $ writeTVar registry.closed True
  stopWorkers registry.workers

validRequest :: Wire.PrefixProofRequest -> Bool
validRequest request = request.operationId /= UUID.nil && request.expectedOffset >= 0 && request.expectedRevision >= 0
  && T.length request.sha256 == 64 && T.all (`elem` ("0123456789abcdef" :: String)) request.sha256

-- Expired terminal results are unknown. Deadline publication is fail-closed;
-- an in-flight worker checks the same state before publishing or requeuing.
prune :: PrefixRegistry -> UTCTime -> STM ()
prune registry time = do
  entries <- readTVar registry.entries
  forM_ (Map.elems entries) $ \job -> do
    progress <- readTVar job.progress
    when (progress.phase == "pending" && diffUTCTime time job.startedAt >= 300) $
      writeTVar job.progress progress { phase = "failed", failure = Just "verification_deadline", completedAt = Just time }
  states <- mapM (readTVar . (.progress)) entries
  let expired = [key | (key,value) <- Map.toList states, Just ended <- [value.completedAt], diffUTCTime time ended >= 300]
  writeTVar registry.entries (foldr Map.delete entries expired)
  queued <- flushTBQueue registry.ready
  forM_ queued $ \job -> do
    state <- readTVar job.progress
    when (state.phase == "pending") $ writeTBQueue registry.ready job

submitPrefixProof :: PrefixRegistry -> PrefixTarget -> IO (Either PrefixFault Wire.PrefixProofResponse)
submitPrefixProof registry target
  | not (validRequest target.request) = pure (Left PrefixInvalid)
  | otherwise = do
      valid <- registry.options.validateTarget target
      if not valid then pure (Left PrefixInvalid) else do
        time <- registry.options.currentTime
        atomically $ do
          prune registry time
          entries <- readTVar registry.entries
          closing <- readTVar registry.closed
          if closing then pure (Left PrefixClosed) else case Map.lookup target.request.operationId entries of
            Just job | job.target == target -> Right <$> response job
                     | otherwise -> pure (Left PrefixConflict)
            Nothing -> do
              states <- mapM (readTVar . (.progress)) entries
              if length (filter ((== "pending") . (.phase)) (Map.elems states)) >= 4 then pure (Left PrefixBusy) else do
                let terminal = sortOn (\(key,state) -> (state.completedAt,key)) [(key,state) | (key,state) <- Map.toList states, state.phase /= "pending"]
                    remove = map fst (take (max 0 (Map.size entries-15)) terminal)
                    retained = foldr Map.delete entries remove
                progress <- newTVar (Progress 0 hashInit "pending" Nothing Nothing)
                let job = Job target time progress
                writeTVar registry.entries (Map.insert target.request.operationId job retained)
                writeTBQueue registry.ready job
                Right <$> response job

lookupPrefixProof :: PrefixRegistry -> UUID -> UUID -> Text -> UUID -> IO (Maybe Wire.PrefixProofResponse)
lookupPrefixProof registry workspace session audience operation = do
  time <- registry.options.currentTime
  atomically $ do
    prune registry time
    entries <- readTVar registry.entries
    case Map.lookup operation entries of
      Just job | job.target.workspace == workspace && job.target.session == session && job.target.audience == audience -> Just <$> response job
      _ -> pure Nothing

prefixRegistryCounts :: PrefixRegistry -> IO (Int,Int)
prefixRegistryCounts registry = do
  time <- registry.options.currentTime
  atomically $ do
    prune registry time
    entries <- readTVar registry.entries
    states <- mapM (readTVar . (.progress)) entries
    pure (length (filter ((== "pending") . (.phase)) (Map.elems states)),Map.size entries)

response :: Job -> STM Wire.PrefixProofResponse
response job = do
  progress <- readTVar job.progress
  let target = job.target; request = target.request
  pure $ Wire.PrefixProofResponse target.workspace target.session target.audience request.operationId request.expectedOffset request.expectedRevision request.sha256 progress.phase progress.failure

worker :: PrefixRegistry -> IO ()
worker registry = do
  next <- atomically $ do
    closing <- readTVar registry.closed
    if closing then pure Nothing else Just <$> readTBQueue registry.ready
  case next of
    Nothing -> pure ()
    Just job -> do
      progress <- readTVarIO job.progress
      when (progress.phase == "pending") $ do
        time <- registry.options.currentTime
        let left = max 0 (300 - realToFrac (diffUTCTime time job.startedAt) :: Double)
            step = do
              if progress.offset == job.target.request.expectedOffset then pure progress else do
                bytes <- registry.options.readWindow job.target progress.offset
                let count = fromIntegral (BS.length bytes)
                unless (count == min 1048576 (job.target.request.expectedOffset-progress.offset)) $ throwIO (userError "invalid prefix window")
                let updated = hashUpdate progress.context bytes
                updated `seq` pure progress { offset = progress.offset+count, context = updated }
        result <- try @SomeException $ timeout (min 5000000 (ceiling (left*1000000))) step
        updatedAt <- registry.options.currentTime
        case result of
          Left failure | Just (_ :: SomeAsyncException) <- fromException failure -> throwIO failure
          _ -> atomically $ do
            current <- readTVar job.progress
            closing <- readTVar registry.closed
            when (current.phase == "pending" && not closing) $ do
              let failed code = current { phase = "failed", failure = Just code, completedAt = Just updatedAt }
              case result of
                _ | diffUTCTime updatedAt job.startedAt >= 300 -> writeTVar job.progress (failed "verification_deadline")
                Right (Just updated)
                  | updated.offset == job.target.request.expectedOffset -> do
                    let digest = T.pack (show (hashFinalize updated.context :: Digest SHA256))
                    writeTVar job.progress updated { phase = if digest == job.target.request.sha256 then "verified" else "mismatch", completedAt = Just updatedAt }
                  | otherwise -> writeTVar job.progress updated >> writeTBQueue registry.ready job
                _ -> writeTVar job.progress (failed "verification_unavailable")
      worker registry
