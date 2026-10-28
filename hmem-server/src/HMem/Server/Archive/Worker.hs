-- | One application-owned post-seal worker. No connection survives a DB call.
module HMem.Server.Archive.Worker
  ( ArchiveWorker, ArchiveFailure(..), PreparedArchive(..)
  , startArchiveWorker, stopArchiveWorker, runArchiveLease
  , startArchiveWorkerWith
  , runArchiveLeaseWith
  , compressArchive, verifyAndPublishArchive, archiveAttemptMicros
  , verifyAndPublishArchiveWith
  ) where

import Control.Concurrent (ThreadId, forkIOWithUnmask, killThread, myThreadId, threadDelay, throwTo)
import Control.Concurrent.MVar
import Control.Exception (Exception, SomeException, SomeAsyncException, bracket, catch, finally, fromException, mask, throwIO, try)
import Control.Monad (unless, void, forever)
import Crypto.Hash (Context, SHA256, Digest, hashInit, hashUpdate, hashFinalize)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Int (Int64)
import Data.IORef
import Data.Pool (Pool)
import Data.Text (Text)
import Data.Text qualified as T
import Hasql.Connection qualified as H
import System.IO (hPutStrLn, stderr)
import System.Timeout (timeout)

import HMem.AgentSession (sessionSha256)
import HMem.DB.SessionArchive qualified as DB
import HMem.SessionArchive.Codec qualified as Codec

-- A failed join carries the exact unfinished capability, including when its
-- parent has exited. A later stop retries that child rather than a bare error.
data ArchiveFailure = ArchiveFailure Text | ArchiveJoinFailure OwnedThread deriving stock (Show, Eq)
instance Exception ArchiveFailure
data OwnedThread = OwnedThread ThreadId (MVar (Either SomeException ())) deriving stock (Eq)
instance Show OwnedThread where show (OwnedThread tid _) = "pending archive thread " <> show tid
newtype ArchiveWorker = ArchiveWorker OwnedThread
data PreparedArchive = PreparedArchive
  { receipt :: !DB.ArchiveReceipt, encoded :: !Codec.CodecResult
  , partCount :: !Int64, ledgerCount :: !Int64
  } deriving stock (Show, Eq)

check :: Bool -> Text -> IO ()
check ok code = unless ok (throwIO (ArchiveFailure code))
need :: Text -> Maybe a -> IO a
need code = maybe (throwIO (ArchiveFailure code)) pure
live :: IO Bool -> IO ()
live action = action >>= \ok -> check ok "archive_lease_lost"

spawnOwned :: IO () -> IO OwnedThread
spawnOwned action = mask $ \_ -> do
  finished <- newEmptyMVar
  tid <- forkIOWithUnmask $ \unmask -> try (unmask action) >>= putMVar finished
  pure (OwnedThread tid finished)

-- Completion is retained. A timed-out join is an explicit failed drain; callers
-- must keep dependent pools and sandbox resources until this handle joins.
stopOwned :: OwnedThread -> IO ()
stopOwned owned@(OwnedThread tid finished) = do
  result <- timeout 5000000 (killThread tid >> readMVar finished)
  case result of
    Nothing -> throwIO (ArchiveJoinFailure owned)
    Just (Right ()) -> pure ()
    Just (Left failure) -> case fromException failure :: Maybe SomeAsyncException of
      Just _ -> pure ()
      Nothing -> case fromException failure :: Maybe ArchiveFailure of
        Just (ArchiveJoinFailure child) -> stopOwned child
        _ -> throwIO failure

startArchiveWorker :: Pool H.Connection -> IO ArchiveWorker
startArchiveWorker = startArchiveWorkerWith (pure ()) (pure ())

-- | Test-only seams for an active attempt and the owned renewal finalizer.
startArchiveWorkerWith :: IO () -> IO () -> Pool H.Connection -> IO ArchiveWorker
startArchiveWorkerWith afterCompression renewalCleanup pool = ArchiveWorker <$> spawnOwned (loop Nothing)
  where
    loop cursor = do
      next <- synchronous (do
        _ <- DB.cleanupNextArchiveGeneration pool
        rows <- DB.enqueueArchives pool cursor 32
        implementation <- Codec.codecImplementation
        lease <- DB.claimArchive pool implementation
        mapM_ (runArchiveLeaseInternal afterCompression renewalCleanup pool) lease
        -- A sweep advances even when one candidate fails. Claim ordering is
        -- retry-time/session, independent of this eligibility keyset.
        pure $ case reverse rows of [] -> Nothing; row:_ -> Just row)
        (hPutStrLn stderr "session-archive: scan failed; retrying" >> threadDelay 1000000 >> pure cursor)
      threadDelay 100000
      loop next

stopArchiveWorker :: ArchiveWorker -> IO ()
stopArchiveWorker (ArchiveWorker thread) = stopOwned thread

synchronous :: IO a -> IO a -> IO a
synchronous action fallback = action `catch` \(failure :: SomeException) ->
  case fromException failure :: Maybe SomeAsyncException of
    Just _ -> throwIO failure
    Nothing -> case fromException failure :: Maybe ArchiveFailure of
      Just (ArchiveJoinFailure _) -> throwIO failure
      _ -> fallback

-- Checked integer arithmetic before conversion to the platform timeout type.
archiveAttemptMicros :: Int64 -> Int
archiveAttemptMicros raw = fromInteger $ min (toInteger (maxBound :: Int)) $
  1000000 * max 30 (120 * ((toInteger raw + 1073741823) `div` 1073741824))

runArchiveLease :: Pool H.Connection -> DB.ArchiveLease -> IO ()
runArchiveLease = runArchiveLeaseWith (pure ())

-- | Test-only cancellation seam with the renewal child already owned.
runArchiveLeaseWith :: IO () -> Pool H.Connection -> DB.ArchiveLease -> IO ()
runArchiveLeaseWith afterCompression = runArchiveLeaseInternal afterCompression (pure ())

runArchiveLeaseInternal :: IO () -> IO () -> Pool H.Connection -> DB.ArchiveLease -> IO ()
runArchiveLeaseInternal afterCompression renewalCleanup pool lease = mask $ \restore -> do
  receipt <- DB.readArchiveReceipt pool lease >>= need "archive_receipt_unavailable"
  parent <- myThreadId
  let renewal = forever $ do
        threadDelay 10000000
        renewed <- timeout 5000000 (DB.renewArchive pool lease)
        check (renewed == Just True) "archive_lease_lost"
      monitor = renewal `catch` \(failure :: SomeException) ->
        case fromException failure :: Maybe SomeAsyncException of
          Just _ -> throwIO failure
          Nothing -> throwTo parent (ArchiveFailure "archive_lease_lost")
  outcome <- try @SomeException $ bracket (spawnOwned (monitor `finally` renewalCleanup)) stopOwned $ \_ -> restore $ do
    result <- timeout (archiveAttemptMicros receipt.rawBytes) $ do
      prepared <- compressArchive pool lease
      afterCompression
      verifyAndPublishArchive pool lease prepared
    check (result == Just ()) "archive_attempt_timeout"
  case outcome of
    Right () -> pure ()
    Left failure -> do
      -- Do not attempt any dependent DB cleanup after unconfirmed child drain.
      case fromException failure :: Maybe ArchiveFailure of
        Just (ArchiveJoinFailure _) -> throwIO failure
        _ -> do
          let code = case fromException failure :: Maybe SomeAsyncException of
                Just _ -> "archive_cancelled"
                Nothing -> case fromException failure :: Maybe ArchiveFailure of
                  Just (ArchiveFailure value) -> value
                  _ -> "archive_verification_failed"
          -- One finite release; an expired owner stays fenced and is reclaimed
          -- by the next claim. Raw chunks and sealed receipts are never changed.
          _ <- timeout 5000000 $ synchronous (void (DB.releaseArchive pool lease False code)) (pure ())
          case fromException failure :: Maybe SomeAsyncException of
            Just _ -> throwIO failure
            Nothing -> pure ()

compressArchive :: Pool H.Connection -> DB.ArchiveLease -> IO PreparedArchive
compressArchive pool lease = do
  receipt <- DB.readArchiveReceipt pool lease >>= need "archive_receipt_unavailable"
  sourcePosition <- newIORef (0,0,0 :: Int64)
  partPosition <- newIORef (0,0 :: Int64)
  let source = do
        (offset,revision,records) <- readIORef sourcePosition
        row <- DB.readArchiveSourceChunk pool lease offset
        case row of
          Nothing -> do
            check (offset == receipt.rawBytes && revision == receipt.rawRevision-1 && records == receipt.rawRecords) "archive_source_incomplete"
            pure Nothing
          Just chunk -> do
            check (chunk.startOffset == offset && chunk.revision == revision+1 &&
              not (BS.null chunk.bytes) && BS.length chunk.bytes <= 1048576) "archive_source_order"
            end <- Codec.checkedAdd offset (fromIntegral (BS.length chunk.bytes))
            let newline = BS.last chunk.bytes == 10
                count = fromIntegral (BS.count 10 chunk.bytes) + if newline then 0 else 1
            check (end <= receipt.rawBytes && chunk.endsWithNewline == newline && chunk.recordCount == count &&
              (newline || chunk.allowFinalRecord && end == receipt.rawBytes)) "archive_source_metadata"
            copied <- DB.stageArchiveLedger pool lease revision 1
            check (copied == Just chunk.revision) "archive_ledger_stage_failed"
            totalRecords <- Codec.checkedAdd records count
            writeIORef sourcePosition (end,chunk.revision,totalRecords)
            pure (Just chunk.bytes)
      sink bytes = do
        (ordinal,offset) <- readIORef partPosition
        live (DB.stageArchivePart pool lease ordinal offset bytes)
        end <- Codec.checkedAdd offset (fromIntegral (BS.length bytes))
        writeIORef partPosition (ordinal+1,end)
        (raw,_,_) <- readIORef sourcePosition
        live (DB.archiveProgress pool lease "compressing" raw end)
  encoded <- Codec.compress source sink
  check (encoded.raw.byteCount == receipt.rawBytes && encoded.raw.sha256 == receipt.rawSha256) "archive_source_receipt_mismatch"
  (parts,_) <- readIORef partPosition
  (_,ledger,_) <- readIORef sourcePosition
  pure (PreparedArchive receipt encoded parts ledger)

-- Each pass fetches persisted parts individually and validates the position,
-- payload digest and SQL's versioned binding chain before giving bytes to zlib.
partSource :: Pool H.Connection -> DB.ArchiveLease -> PreparedArchive -> IO (IO (Maybe ByteString))
partSource pool lease prepared = do
  position <- newIORef (0,0 :: Int64)
  pure $ do
    (ordinal,offset) <- readIORef position
    part <- DB.readStagedPart pool lease ordinal
    if ordinal == prepared.partCount then do
      check (part == Nothing && offset == prepared.encoded.compressed.byteCount) "archive_extra_part"
      pure Nothing
    else do
      p <- need "archive_part_missing_or_binding" part
      end <- Codec.checkedAdd offset (fromIntegral (BS.length p.bytes))
      check (p.ordinal == ordinal && p.startOffset == offset && p.endOffset == end &&
        not (BS.null p.bytes) && BS.length p.bytes <= 1048576 && sessionSha256 p.bytes == p.dataSha256) "archive_part_integrity"
      writeIORef position (ordinal+1,end)
      pure (Just p.bytes)

verifyAndPublishArchive :: Pool H.Connection -> DB.ArchiveLease -> PreparedArchive -> IO ()
verifyAndPublishArchive = verifyAndPublishArchiveWith (pure ())

-- | Test-only fault seam after index persistence and before its independent
-- comparison pass. Production never supplies a callback here.
verifyAndPublishArchiveWith :: IO () -> Pool H.Connection -> DB.ArchiveLease -> PreparedArchive -> IO ()
verifyAndPublishArchiveWith afterIndex pool lease prepared = do
  let receipt = prepared.receipt
      verifyResult result = check (result.raw == prepared.encoded.raw && result.compressed == prepared.encoded.compressed) "archive_persisted_receipt_mismatch"
  live (DB.archiveProgress pool lease "verifying" 0 prepared.encoded.compressed.byteCount)
  firstSource <- partSource pool lease prepared
  index <- newIORef (0 :: Int64)
  first <- Codec.verify receipt.rawBytes firstSource (const (pure ())) $ \point -> do
    ordinal <- readIORef index
    live (DB.stageArchiveCheckpoint pool lease ordinal point.rawOffset point.compressedOffset (fromIntegral point.unusedBits) point.dictionary)
    writeIORef index (ordinal+1)
  verifyResult first
  points <- readIORef index
  afterIndex
  -- A second persisted readback derives the index anew, comparing exact bytes
  -- and bit positions. Even a self-consistent wrong dictionary must fail.
  secondSource <- partSource pool lease prepared
  indexCheck <- newIORef (0 :: Int64)
  ledgerSink <- verifyLedgerSink pool lease prepared
  second <- Codec.verify receipt.rawBytes secondSource (fst ledgerSink) $ \point -> do
    ordinal <- readIORef indexCheck
    c <- DB.readStagedCheckpoint pool lease ordinal >>= need "archive_checkpoint_missing_or_binding"
    check (c.ordinal == ordinal && c.rawOffset == point.rawOffset && c.compressedOffset == point.compressedOffset &&
      fromIntegral c.unusedBits == point.unusedBits && c.dictionary == point.dictionary &&
      c.dictionarySha256 == sessionSha256 point.dictionary) "archive_checkpoint_integrity"
    writeIORef indexCheck (ordinal+1)
  snd ledgerSink
  verifyResult second
  checkedPoints <- readIORef indexCheck
  extra <- DB.readStagedCheckpoint pool lease checkedPoints
  check (checkedPoints == points && second.checkpoints == points && extra == Nothing) "archive_checkpoint_count"
  currentReceipt <- DB.readArchiveReceipt pool lease >>= need "archive_receipt_unavailable"
  check (currentReceipt == receipt) "archive_receipt_changed"
  live (DB.publishArchive pool lease (DB.ArchiveVerification second.compressed.byteCount second.compressed.sha256 prepared.partCount points prepared.ledgerCount))

-- No original chunk is reconstructed in RAM: one hash context follows its
-- immutable ledger end, and each decoder buffer may cross several boundaries.
verifyLedgerSink :: Pool H.Connection -> DB.ArchiveLease -> PreparedArchive -> IO (ByteString -> IO (), IO ())
verifyLedgerSink pool lease prepared = do
  position <- newIORef (0,0,hashInit :: Context SHA256)
  current <- newIORef Nothing
  let consume bytes = unless (BS.null bytes) $ do
        (offset,revision,context) <- readIORef position
        cached <- readIORef current
        ledger <- case cached of
          Just value -> pure value
          Nothing -> do
            l <- DB.readStagedLedger pool lease (revision+1) >>= need "archive_ledger_missing_or_binding"
            original <- DB.readArchiveSourceChunk pool lease offset >>= need "archive_original_missing"
            let end = offset + fromIntegral (BS.length original.bytes)
            check (l.revision == revision+1 && original.revision == l.revision && original.startOffset == offset &&
              l.startOffset == offset && l.endOffset == end && l.allowFinalRecord == original.allowFinalRecord &&
              l.recordCount == original.recordCount && l.endsWithNewline == original.endsWithNewline &&
              l.rawSha256 == sessionSha256 original.bytes) "archive_ledger_metadata"
            writeIORef current (Just l)
            pure l
        let amount = fromIntegral (min (fromIntegral (BS.length bytes)) (ledger.endOffset-offset))
            (piece,rest) = BS.splitAt amount bytes
            nextContext = hashUpdate context piece
            end = offset + fromIntegral amount
        check (amount > 0) "archive_ledger_nonprogress"
        if end == ledger.endOffset then do
          check (T.pack (show (hashFinalize nextContext :: Digest SHA256)) == ledger.rawSha256) "archive_ledger_raw_hash"
          writeIORef position (end,ledger.revision,hashInit)
          writeIORef current Nothing
        else nextContext `seq` writeIORef position (end,revision,nextContext)
        consume rest
      finish = do
        (offset,revision,_) <- readIORef position
        pending <- readIORef current
        check (offset == prepared.receipt.rawBytes && revision == prepared.ledgerCount && pending == Nothing) "archive_ledger_incomplete"
        extra <- DB.readStagedLedger pool lease (revision+1)
        check (extra == Nothing) "archive_extra_ledger"
  pure (consume,finish)
