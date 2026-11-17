-- | Lifecycle-owned verification of immutable committed prefixes. Every valid
-- submission is accepted (2026-11-17 user decision); CPU-sized workers take
-- fair turns, live jobs keep a five-minute deadline and terminal results are
-- retained for five minutes under a memory ceiling that never evicts live
-- work. A job opens its source reader for one turn only, so open readers and
-- archive decoders never exceed the worker count.
module HMem.Server.SessionPrefix
  ( PrefixRegistry, PrefixTarget(..), PrefixOptions(..), PrefixFault(..), PrefixReader(..)
  , newPrefixRegistry, newPrefixRegistryWith, newPrefixRegistryWithWorkerHook, closePrefixRegistry
  , newPrefixRegistryWithWorkerHookAndCleanupHook
  , newPrefixRegistryWithRequeueHook, newPrefixRegistryAt, newPrefixRegistryLogging
  , submitPrefixProof, lookupPrefixProof, lookupPrefixProofClassified, prefixRegistryCounts
  , prefixTraffic, prefixWorkerCount, newPrefixRegistryWithWorkers
  , newPrefixRegistryWithReaders, newPrefixRegistryWithTerminalLimit, sessionPrefixReader
  , terminalResultLimit
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad (forM_, unless, when)
import Crypto.Hash (Context, Digest, SHA256, hashInit, hashUpdate, hashFinalize)
import Data.ByteString qualified as BS
import Data.Int (Int64)
import Data.Foldable (find)
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict qualified as Map
import Data.Pool (Pool)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Hasql.Connection qualified as Hasql
import System.Log.FastLogger (FastLogger)
import System.Timeout (timeout)

import HMem.AgentSession qualified as Session
import HMem.AgentSession.Transport qualified as Wire
import HMem.DB.AgentSession qualified as DB
import HMem.Server.SessionTraffic (MissReason(..), ProofResult(..), SessionTraffic, attachTrafficLog, detachTrafficLog, flushTrafficLog, newSessionTraffic, recordComputeSTM)

data PrefixTarget = PrefixTarget
  { workspace :: !UUID, session :: !UUID, audience :: !Text, request :: !Wire.PrefixProofRequest }
  deriving (Show, Eq)
data PrefixOptions = PrefixOptions
  { currentTime :: IO UTCTime
  , validateTarget :: PrefixTarget -> IO Bool
  , readWindow :: PrefixTarget -> Int64 -> IO BS.ByteString
  }
data PrefixFault = PrefixInvalid | PrefixConflict | PrefixClosed deriving (Show, Eq)
data Progress = Progress
  { offset :: !Int64, context :: !(Context SHA256), phase :: !Text
  , failure :: !(Maybe Text), completedAt :: !(Maybe UTCTime) }
-- | One turn's source: an ordered window read and its release.
data PrefixReader = PrefixReader
  { readPrefixWindow :: Int64 -> Int -> IO BS.ByteString, closePrefixReader :: IO () }
-- 'serial' is unique per registry, even if an operation UUID is later reused.
data Job = Job { serial :: !Int, target :: !PrefixTarget, startedAt :: !UTCTime, progress :: !(TVar Progress)
  , reader :: !(MVar (Maybe PrefixReader)), admissionHeld :: !(TVar Bool) }
-- 'live' holds pending jobs by start time (deadline order); 'terminal' holds
-- retained results by completion time (expiry and eviction order). Both are
-- also reachable by operation through 'entries'.
data PrefixRegistry = PrefixRegistry
  { entries :: TVar (Map.Map UUID Job), ready :: TQueue Job, closed :: TVar Bool
  , live :: TVar (Map.Map (UTCTime,Int) Job), terminal :: TVar (Map.Map (UTCTime,Int) Job)
  , nextSerial :: TVar Int, terminalLimit :: !Int
  , unfinished :: TVar (IntMap.IntMap Job)
  , workers :: [(ThreadId, MVar (Either SomeException ()))], options :: PrefixOptions
  , openReader :: !(Maybe (PrefixTarget -> IO PrefixReader)), readerCleanupHook :: !([UUID] -> IO ())
  , afterRequeue :: !(PrefixTarget -> IO ())
  , traffic :: !SessionTraffic, startedAt :: !UTCTime, tombstones :: !(TVar (Seq Tombstone)) }

-- Internal attribution of removed operations only. Lookups of every missing,
-- expired, evicted, restarted or foreign operation still return unknown.
data Tombstone = Tombstone { operation :: !UUID, target :: !PrefixTarget, reason :: !MissReason }

tombstoneLimit :: Int
tombstoneLimit = 256

-- Five-minute live deadline plus five-minute terminal retention.
operationLifetime :: NominalDiffTime
operationLifetime = 600

-- | Memory ceiling on retained terminal results only. A retained result costs
-- well under 1 KiB (target binding, final hash context and index nodes), so
-- the ceiling holds results to roughly 64 MiB. Beyond it the earliest-expiring
-- results are evicted ('MissEvicted'); live work is never evicted.
terminalResultLimit :: Int
terminalResultLimit = 65536

newPrefixRegistry :: Pool Hasql.Connection -> IO PrefixRegistry
newPrefixRegistry = newPrefixRegistryAt getCurrentTime

-- | The production registry, writing its traffic events to the server log
-- until 'closePrefixRegistry' detaches them.
newPrefixRegistryLogging :: FastLogger -> Pool Hasql.Connection -> IO PrefixRegistry
newPrefixRegistryLogging logAction pool = mask_ $ do
  registry <- newPrefixRegistry pool
  attachTrafficLog registry.traffic logAction `onException` closePrefixRegistry registry
  pure registry

-- | The production registry with an injectable deadline/retention clock.
newPrefixRegistryAt :: IO UTCTime -> Pool Hasql.Connection -> IO PrefixRegistry
newPrefixRegistryAt clock pool = do
  count <- prefixWorkerCount
  newPrefixRegistryWithReaders count (sessionPrefixReader pool) $
    PrefixOptions clock
      (\target -> DB.validateSessionPrefixBoundary pool target.workspace target.session target.request.expectedOffset target.request.expectedRevision)
      (\target -> DB.readSessionPrefixWindow pool target.workspace target.session target.request.expectedOffset)

-- | The production per-turn reader: a database session reader pinned to the
-- frozen target, which opens an archive decoder only when raw bytes are gone.
sessionPrefixReader :: Pool Hasql.Connection -> PrefixTarget -> IO PrefixReader
sessionPrefixReader pool target = do
  source <- DB.newSessionReader pool target.workspace target.session target.request.expectedOffset
  pure (PrefixReader (DB.readSessionReader source) (DB.closeSessionReader source))

-- | Seam with an explicit worker count and per-turn reader source.
newPrefixRegistryWithReaders :: Int -> (PrefixTarget -> IO PrefixReader) -> PrefixOptions -> IO PrefixRegistry
newPrefixRegistryWithReaders count open = newRegistryWithHooks count terminalResultLimit (\_ action -> action) (Just open) (const (pure ())) (const (pure ()))

newPrefixRegistryWith :: PrefixOptions -> IO PrefixRegistry
newPrefixRegistryWith = newPrefixRegistryWithWorkerHook (\_ action -> action)

-- | Test seam with an explicit worker count. The other seams keep two workers
-- so their causal schedules stay deterministic.
newPrefixRegistryWithWorkers :: Int -> PrefixOptions -> IO PrefixRegistry
newPrefixRegistryWithWorkers count = newRegistryWithHooks count terminalResultLimit (\_ action -> action) Nothing (const (pure ())) (const (pure ()))

-- | Test seam with a small terminal-result ceiling and two workers.
newPrefixRegistryWithTerminalLimit :: Int -> PrefixOptions -> IO PrefixRegistry
newPrefixRegistryWithTerminalLimit limit = newRegistryWithHooks 2 limit (\_ action -> action) Nothing (const (pure ())) (const (pure ()))

-- A worker-lifetime seam permits causal cancellation/finalization tests without
-- changing production worker limits.
newPrefixRegistryWithWorkerHook :: (Int -> IO () -> IO ()) -> PrefixOptions -> IO PrefixRegistry
newPrefixRegistryWithWorkerHook hook = newRegistry hook Nothing

-- Causal shutdown seam: observe completed reader cleanup, after every worker
-- join and before any previously observed worker failure is reported.
newPrefixRegistryWithWorkerHookAndCleanupHook :: (Int -> IO () -> IO ()) -> ([UUID] -> IO ()) -> PrefixOptions -> IO PrefixRegistry
newPrefixRegistryWithWorkerHookAndCleanupHook hook = newRegistryWithCleanup hook Nothing

-- Pause at the atomic queue handoff for causal expiry/ownership tests.
newPrefixRegistryWithRequeueHook :: (PrefixTarget -> IO ()) -> PrefixOptions -> IO PrefixRegistry
newPrefixRegistryWithRequeueHook = newRegistryWithHooks 2 terminalResultLimit (\_ action -> action) Nothing (const (pure ()))

newRegistry :: (Int -> IO () -> IO ()) -> Maybe (PrefixTarget -> IO PrefixReader) -> PrefixOptions -> IO PrefixRegistry
newRegistry hook openReader = newRegistryWithCleanup hook openReader (const (pure ()))

newRegistryWithCleanup :: (Int -> IO () -> IO ()) -> Maybe (PrefixTarget -> IO PrefixReader) -> ([UUID] -> IO ()) -> PrefixOptions -> IO PrefixRegistry
newRegistryWithCleanup hook openReader readerCleanupHook = newRegistryWithHooks 2 terminalResultLimit hook openReader readerCleanupHook (const (pure ()))

newRegistryWithHooks :: Int -> Int -> (Int -> IO () -> IO ()) -> Maybe (PrefixTarget -> IO PrefixReader) -> ([UUID] -> IO ()) -> (PrefixTarget -> IO ()) -> PrefixOptions -> IO PrefixRegistry
newRegistryWithHooks count limit hook openReader readerCleanupHook afterRequeue options = mask_ $ do
  entries <- newTVarIO Map.empty; ready <- newTQueueIO; closed <- newTVarIO False
  live <- newTVarIO Map.empty; terminal <- newTVarIO Map.empty; nextSerial <- newTVarIO 0
  unfinished <- newTVarIO IntMap.empty
  traffic <- newSessionTraffic; started <- options.currentTime; tombstones <- newTVarIO Seq.empty
  let registry = PrefixRegistry entries ready closed live terminal nextSerial (max 1 limit) unfinished [] options openReader readerCleanupHook afterRequeue traffic started tombstones
      spawn index = do
        done <- newEmptyMVar
        tid <- forkIOWithUnmask $ \unmask -> do
          result <- try (unmask (hook index (worker registry)))
          atomically $ writeTVar closed True
          putMVar done result
        pure (tid,done)
  let spawnAll index spawned
        | index >= max 1 count = pure (reverse spawned)
        | otherwise = do
            next <- spawn index `onException` stopWorkers spawned
            spawnAll (index + 1) (next : spawned)
  spawned <- spawnAll 0 []
  pure registry { workers = spawned }

-- | CPU-bound hashing is the real resource: half the capabilities, at least two.
prefixWorkerCount :: IO Int
prefixWorkerCount = (\capabilities -> max 2 (capabilities `div` 2)) <$> getNumCapabilities

stopWorkers :: [(ThreadId, MVar (Either SomeException ()))] -> IO ()
stopWorkers = stopWorkersWith (pure ())

stopWorkersWith :: IO () -> [(ThreadId, MVar (Either SomeException ()))] -> IO ()
stopWorkersWith afterJoined workers = do
  stopped <- timeout 5000000 $ do
    forM_ workers (killThread . fst)
    mapM (readMVar . snd) workers
  case stopped of
    Nothing -> throwIO (userError "prefix verification shutdown exceeded five seconds")
    Just results -> do
      afterJoined
      let failures = [failure | Left failure <- results, Nothing <- [fromException failure :: Maybe SomeAsyncException]]
      unless (null failures) $ throwIO (userError ("prefix verification workers failed after all joins: " <> show (length failures)))

closePrefixRegistry :: PrefixRegistry -> IO ()
closePrefixRegistry registry = flip finally (detachTrafficLog registry.traffic) $ do
  atomically $ writeTVar registry.closed True
  stopWorkersWith (do
    jobs <- atomically $ do
      entries <- readTVar registry.entries
      queued <- flushTQueue registry.ready
      owners <- readTVar registry.unfinished
      pure (Map.elems entries<>queued<>IntMap.elems owners)
    mapM_ (closeJobReader registry) jobs
    registry.readerCleanupHook (map (.target.request.operationId) jobs)) registry.workers

-- Release this turn's reader, if any. Called before a job yields its turn, so
-- a queued job never holds a reader or archive decoder.
releaseJobReader :: Job -> IO ()
releaseJobReader job = modifyMVar_ job.reader $ \reader -> do
  mapM_ (.closePrefixReader) reader
  pure Nothing

closeJobReader :: PrefixRegistry -> Job -> IO ()
closeJobReader registry job = mask_ $ do
  releaseJobReader job
  -- A terminal/pruned job still owns admission until its actual reader has
  -- closed. Retain the owner reference even while it is in flight or no
  -- longer visible, so confirmed shutdown can always finish that cleanup.
  atomically $ do
    held <- readTVar job.admissionHeld
    when held $ do
      owners <- readTVar registry.unfinished
      unless (IntMap.member job.serial owners) $
        throwSTM (userError "prefix reader admission owner missing")
      writeTVar registry.unfinished (IntMap.delete job.serial owners)
      writeTVar job.admissionHeld False

readJobWindow :: PrefixRegistry -> Job -> Int64 -> IO BS.ByteString
readJobWindow registry job offset = case registry.openReader of
  Nothing -> registry.options.readWindow job.target offset
  Just open -> mask $ \restore -> do
    -- Register ownership before allowing cancellation of the first read.
    reader <- modifyMVar job.reader $ \existing -> case existing of
      Just source -> pure (existing,source)
      Nothing -> do source <- open job.target; pure (Just source,source)
    restore (reader.readPrefixWindow offset (fromIntegral (min 1048576 (job.target.request.expectedOffset-offset))))

validRequest :: Wire.PrefixProofRequest -> Bool
validRequest request = request.operationId /= UUID.nil && request.expectedOffset >= 0 && request.expectedRevision >= 0
  && T.length request.sha256 == 64 && T.all (`elem` ("0123456789abcdef" :: String)) request.sha256

-- Expired terminal results are unknown. Deadline publication is fail-closed;
-- an in-flight worker checks the same state before publishing or requeuing.
-- Incremental: only due deadlines and expired results are visited, and no
-- TVar is written when nothing is due. A queued job made terminal here keeps
-- its admission until its next dequeuing worker cleans it up.
prune :: PrefixRegistry -> UTCTime -> STM ()
prune registry time = do
  live <- readTVar registry.live
  let (due,_) = Map.spanAntitone (\(started,_) -> diffUTCTime time started >= 300) live
  forM_ (Map.elems due) $ \job -> do
    progress <- readTVar job.progress
    when (progress.phase == "pending") $ do
      writeTVar job.progress progress { phase = "failed", failure = Just "verification_deadline", completedAt = Just time }
      recordResult registry job time ResultFailedDeadline
    retire registry job time
  terminal <- readTVar registry.terminal
  let (expired,kept) = Map.spanAntitone (\(ended,_) -> diffUTCTime time ended >= 300) terminal
  unless (Map.null expired) $ do
    writeTVar registry.terminal kept
    forget registry MissExpired (Map.elems expired)

-- Move a job that just became terminal from the live to the terminal index,
-- then evict the earliest-expiring terminal results above the ceiling.
retire :: PrefixRegistry -> Job -> UTCTime -> STM ()
retire registry job ended = do
  modifyTVar' registry.live (Map.delete (job.startedAt,job.serial))
  terminal <- Map.insert (ended,job.serial) job <$> readTVar registry.terminal
  let excess = Map.size terminal - registry.terminalLimit
  if excess <= 0 then writeTVar registry.terminal terminal else do
    let (evicted,kept) = Map.splitAt excess terminal
    writeTVar registry.terminal kept
    forget registry MissEvicted (Map.elems evicted)

-- Remove retained results from operation lookup and record why.
forget :: PrefixRegistry -> MissReason -> [Job] -> STM ()
forget registry reason jobs = do
  bury registry reason jobs
  modifyTVar' registry.entries $ \entries -> foldr
    (\job -> Map.update (\current -> if current.serial == job.serial then Nothing else Just current) job.target.request.operationId)
    entries jobs

submitPrefixProof :: PrefixRegistry -> PrefixTarget -> IO (Either PrefixFault Wire.PrefixProofResponse)
submitPrefixProof registry target
  | not (validRequest target.request) = pure (Left PrefixInvalid)
  | otherwise = do
      valid <- registry.options.validateTarget target
      if not valid then pure (Left PrefixInvalid) else do
        time <- registry.options.currentTime
        reader <- newMVar Nothing
        atomically $ do
          prune registry time
          entries <- readTVar registry.entries
          closing <- readTVar registry.closed
          if closing then pure (Left PrefixClosed) else case Map.lookup target.request.operationId entries of
            Just job | job.target == target -> Right <$> response job
                     | otherwise -> pure (Left PrefixConflict)
            Nothing -> do
              -- Accept every valid operation; live work is never evicted.
              serial <- readTVar registry.nextSerial
              writeTVar registry.nextSerial (serial + 1)
              progress <- newTVar (Progress 0 hashInit "pending" Nothing Nothing)
              admissionHeld <- newTVar True
              let job = Job serial target time progress reader admissionHeld
              modifyTVar' registry.unfinished (IntMap.insert serial job)
              modifyTVar' registry.live (Map.insert (time,serial) job)
              writeTVar registry.entries (Map.insert target.request.operationId job entries)
              writeTQueue registry.ready job
              Right <$> response job

lookupPrefixProof :: PrefixRegistry -> UUID -> UUID -> Text -> UUID -> IO (Maybe Wire.PrefixProofResponse)
lookupPrefixProof registry workspace session audience operation =
  either (const Nothing) Just <$> lookupPrefixProofClassified registry workspace session audience operation

-- | Like 'lookupPrefixProof', with an internal reason for an unknown result.
-- Callers see every reason as the same unknown operation.
lookupPrefixProofClassified :: PrefixRegistry -> UUID -> UUID -> Text -> UUID -> IO (Either MissReason Wire.PrefixProofResponse)
lookupPrefixProofClassified registry workspace session audience operation = do
  time <- registry.options.currentTime
  result <- atomically $ do
    prune registry time
    entries <- readTVar registry.entries
    let matches target = target.workspace == workspace && target.session == session && target.audience == audience
    case Map.lookup operation entries of
      Just job | matches job.target -> Right <$> response job
               | otherwise -> pure (Left MissTargetMismatch)
      Nothing -> do
        buried <- find (\tombstone -> tombstone.operation == operation) <$> readTVar registry.tombstones
        pure $ Left $ case buried of
          Just tombstone | matches tombstone.target -> tombstone.reason
                         | otherwise -> MissTargetMismatch
          Nothing | diffUTCTime time registry.startedAt < operationLifetime -> MissUnknownSinceStart
                  | otherwise -> MissUnknownOperation
  flushTrafficLog registry.traffic
  pure result

-- | Request and proof-compute telemetry owned by this registry.
prefixTraffic :: PrefixRegistry -> SessionTraffic
prefixTraffic registry = registry.traffic

-- Newest first; the bounded ring forgets the oldest attribution.
bury :: PrefixRegistry -> MissReason -> [Job] -> STM ()
bury registry reason jobs = unless (null jobs) $
  modifyTVar' registry.tombstones $ \existing ->
    Seq.take tombstoneLimit (Seq.fromList [Tombstone job.target.request.operationId job.target reason | job <- jobs] <> existing)

recordResult :: PrefixRegistry -> Job -> UTCTime -> ProofResult -> STM ()
recordResult registry job time result =
  recordComputeSTM registry.traffic time result job.target.request.expectedOffset (diffUTCTime time job.startedAt)

prefixRegistryCounts :: PrefixRegistry -> IO (Int,Int)
prefixRegistryCounts registry = do
  time <- registry.options.currentTime
  atomically $ do
    prune registry time
    (,) <$> (Map.size <$> readTVar registry.live) <*> (Map.size <$> readTVar registry.entries)

response :: Job -> STM Wire.PrefixProofResponse
response job = do
  progress <- readTVar job.progress
  let target = job.target; request = target.request
  pure $ Wire.PrefixProofResponse target.workspace target.session target.audience request.operationId request.expectedOffset request.expectedRevision request.sha256 progress.phase progress.failure

worker :: PrefixRegistry -> IO ()
worker registry = do
  next <- atomically $ do
    closing <- readTVar registry.closed
    if closing then pure Nothing else Just <$> readTQueue registry.ready
  case next of
    Nothing -> pure ()
    Just job -> do
      turn registry job `onException` closeJobReader registry job
      worker registry

turn :: PrefixRegistry -> Job -> IO ()
turn registry job = mask $ \restore -> do
  progress <- readTVarIO job.progress
  queued <- if progress.phase /= "pending" then pure False else do
    time <- registry.options.currentTime
    let left = max 0 (300 - realToFrac (diffUTCTime time job.startedAt) :: Double)
        step = do
          if progress.offset == job.target.request.expectedOffset then pure progress else do
            bytes <- readJobWindow registry job progress.offset
            let count = fromIntegral (BS.length bytes)
            unless (count == min 1048576 (job.target.request.expectedOffset-progress.offset)) $ throwIO (userError "invalid prefix window")
            let updated = hashUpdate progress.context bytes
            updated `seq` pure progress { offset = progress.offset+count, context = updated }
    result <- try @SomeException $ restore $ timeout (min 5000000 (ceiling (left*1000000))) step
    case result of
      Left failure | Just (_ :: SomeAsyncException) <- fromException failure -> throwIO failure
      _ -> pure ()
    -- The reader lives for this turn only: a yielding job holds no reader or
    -- archive decoder, so open readers never exceed the worker count. The next
    -- turn reopens at its window, an archive checkpoint boundary.
    releaseJobReader job
    updatedAt <- registry.options.currentTime
    atomically $ do
      current <- readTVar job.progress
      closing <- readTVar registry.closed
      if current.phase == "pending" && not closing then do
        let failed code outcome = do
              writeTVar job.progress current { phase = "failed", failure = Just code, completedAt = Just updatedAt }
              recordResult registry job updatedAt outcome
              retire registry job updatedAt
              pure False
        case result of
          _ | diffUTCTime updatedAt job.startedAt >= 300 -> failed "verification_deadline" ResultFailedDeadline
          Right (Just updated)
            | updated.offset == job.target.request.expectedOffset -> do
              let digest = T.pack (show (hashFinalize updated.context :: Digest SHA256))
                  matched = digest == job.target.request.sha256
              writeTVar job.progress updated { phase = if matched then "verified" else "mismatch", completedAt = Just updatedAt }
              recordResult registry job updatedAt (if matched then ResultVerified else ResultMismatch)
              retire registry job updatedAt
              pure False
            | otherwise -> writeTVar job.progress updated >> writeTQueue registry.ready job >> pure True
          Left failure | Just (Session.SessionException Session.SessionOriginalUnavailable _) <- fromException failure ->
            failed "original_evidence_unavailable" ResultFailedEvidence
          _ -> failed "verification_unavailable" ResultFailedUnavailable
      else pure False
  flushTrafficLog registry.traffic
  -- A successful handoff transfers this turn's ownership to the queue. Later
  -- expiry must be cleaned by its next dequeuing worker, never by this turn.
  if queued then registry.afterRequeue job.target else closeJobReader registry job
