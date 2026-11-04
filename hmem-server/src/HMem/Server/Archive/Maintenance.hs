{-# LANGUAGE BangPatterns #-}
-- | Independent bounded readback before the first destructive maintenance turn.
module HMem.Server.Archive.Maintenance
  ( verifyPublishedArchive, runReclamation, contentBlockSink ) where

import Control.Exception (throwIO)
import Control.Monad (unless,when)
import Crypto.Hash (Context,Digest,SHA256,hashInit,hashUpdate,hashFinalize)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Int (Int64)
import Data.IORef
import Data.Pool (Pool)
import Data.Text (Text)
import Data.Text qualified as T
import Hasql.Connection qualified as H

import HMem.AgentSession (sessionSha256)
import HMem.DB.SessionArchive qualified as A
import HMem.DB.SessionArchive.Maintenance qualified as M
import HMem.DB.SessionArchive.Reader qualified as R
import HMem.SessionArchive.Codec qualified as C
import HMem.SessionArchive.Replacement qualified as Replacement

require :: Bool -> IO ()
require ok=unless ok (throwIO (C.CodecFailure "archive_maintenance_verification_failed"))
need :: Maybe a -> IO a
need=maybe (throwIO (C.CodecFailure "archive_maintenance_data_missing")) pure

-- No transaction or pool lease spans decompression. One stored part, one
-- original chunk and one checkpoint suffice regardless of transcript length.
verifyPublishedArchive :: Pool H.Connection -> A.ArchiveManifest -> IO ()
verifyPublishedArchive pool expected=do
  manifest<-R.verifiedManifest pool expected.workspaceId expected.sessionId >>= need
  require (manifest==expected)
  (consume,finish)<-ledgerSink pool manifest
  verifyReadback False pool manifest manifest.rawBytes manifest.rawSha256 consume
  finish
  R.verifiedManifest pool manifest.workspaceId manifest.sessionId >>= \current -> require (current==Just manifest)

verifyReadback :: Bool -> Pool H.Connection -> A.ArchiveManifest -> Int64 -> Text -> (ByteString -> IO ()) -> IO ()
verifyReadback sanitized pool manifest contentBytes contentHash consume=do
  position<-newIORef (0,0::Int64)
  let partAt=if sanitized then R.verifiedSanitizedPart else R.verifiedPart
      checkpointAt=if sanitized then M.verifiedSanitizedCheckpoint else M.verifiedCheckpoint
      source=do
        (ordinal,offset)<-readIORef position
        next<-partAt pool manifest.workspaceId manifest.sessionId manifest.id ordinal
        if ordinal==manifest.partCount then do
          require (next==Nothing && offset==manifest.compressedBytes)
          pure Nothing
        else do
          part<-need next
          require (part.ordinal==ordinal && part.startOffset==offset && part.endOffset<=manifest.compressedBytes)
          writeIORef position (ordinal+1,part.endOffset)
          pure (Just part.bytes)
  checkpoint<-newIORef (0::Int64)
  verified<-C.verify contentBytes source consume $ \point -> do
    ordinal<-readIORef checkpoint
    stored<-checkpointAt pool manifest ordinal >>= need
    require (stored.ordinal==ordinal && stored.rawOffset==point.rawOffset && stored.compressedOffset==point.compressedOffset
      && fromIntegral stored.unusedBits==point.unusedBits && stored.dictionary==point.dictionary
      && stored.dictionarySha256==sessionSha256 point.dictionary)
    writeIORef checkpoint (ordinal+1)
  count<-readIORef checkpoint
  extra<-checkpointAt pool manifest count
  require (count==manifest.checkpointCount && verified.checkpoints==count && extra==Nothing
    && verified.raw.byteCount==contentBytes && verified.raw.sha256==contentHash
    && verified.compressed.byteCount==manifest.compressedBytes && verified.compressed.sha256==manifest.compressedSha256)

verifyPublishedSanitized :: Pool H.Connection -> M.ReclamationLease -> IO ()
verifyPublishedSanitized pool lease=do
  let manifest=lease.manifest
      select=R.verifiedSanitizedManifest pool manifest.workspaceId manifest.sessionId (Just manifest.id)
  selected<-select >>= need
  require (selected.archive==manifest && selected.originalAvailable)
  snapshot<-M.readReclamationSnapshot pool lease >>= need
  require (sessionSha256 snapshot.snapshotBytes==selected.content.policySha256)
  rules<-either throwIO pure (Replacement.decodeSnapshot snapshot.formatLabel snapshot.eligibleFrom snapshot.snapshotBytes)
  source<-verifiedOriginalSource pool lease
  (transformed,count)<-Replacement.newReplacementSource rules source
  (compareBytes,finishComparison)<-compareSource transformed
  let blockAt=R.verifiedSanitizedBlock pool manifest.workspaceId manifest.sessionId manifest.id
  (verifyBlock,finishBlocks)<-contentBlockSink selected.content.contentBytes $ \block ->
    blockAt block.ordinal >>= require . (==Just block)
  verifyReadback True pool manifest selected.content.contentBytes selected.content.contentSha256
    (\bytes -> compareBytes bytes >> verifyBlock bytes)
  finishComparison
  count >>= require . (==selected.content.replacementCount)
  finishBlocks >>= blockAt >>= require . (==Nothing)
  select >>= require . (==Just selected)
  M.readReclamationSnapshot pool lease >>= require . (==Just snapshot)

-- Re-read the actual sealed source, authenticating each original row in its
-- own coordinate space. Only this stream feeds the independent transform.
verifiedOriginalSource :: Pool H.Connection -> M.ReclamationLease -> IO (IO (Maybe ByteString))
verifiedOriginalSource pool lease=do
  state<-newIORef (0,0,0,True,hashInit :: Context SHA256)
  let manifest=lease.manifest
      ledgerAt=R.verifiedSanitizedLedger pool manifest.workspaceId manifest.sessionId manifest.id
  pure $ do
    (offset,revision,records,previousNewline,digest)<-readIORef state
    row<-M.readReclamationSourceChunk pool lease offset
    case row of
      Nothing -> do
        require (offset==manifest.rawBytes && revision==manifest.ledgerCount && revision==manifest.rawRevision-1
          && records==manifest.rawRecords && digestText digest==manifest.rawSha256)
        pure Nothing
      Just original -> do
        let bytes=original.bytes
        require (not (BS.null bytes) && BS.length bytes<=1048576 && original.revision==revision+1
          && original.startOffset==offset)
        end<-C.checkedAdd offset (fromIntegral (BS.length bytes))
        ledger<-ledgerAt offset >>= need
        let newline=BS.last bytes==10
            count=fromIntegral (BS.count 10 bytes)+(if newline then 0 else 1)-(if previousNewline then 0 else 1)
        require (ledger.revision==original.revision && ledger.startOffset==offset && ledger.endOffset==end
          && end<=manifest.rawBytes && ledger.rawSha256==sessionSha256 bytes
          && ledger.allowFinalRecord==original.allowFinalRecord && (newline || original.allowFinalRecord)
          && ledger.endsWithNewline==newline && original.endsWithNewline==newline
          && count>=0 && original.recordCount==count && ledger.recordCount==count)
        total<-C.checkedAdd records count
        let !next=hashUpdate digest bytes
        writeIORef state (end,original.revision,total,newline,next)
        pure (Just bytes)

-- Codec and transformer windows need not share boundaries. Hold at most one
-- unconsumed transformed window and compare every byte before attesting.
compareSource :: IO (Maybe ByteString) -> IO (ByteString -> IO (),IO ())
compareSource source=do
  pending<-newIORef BS.empty
  let compareBytes input=unless (BS.null input) $ do
        saved<-readIORef pending
        expected<-if BS.null saved then source >>= need else pure saved
        let amount=min (BS.length input) (BS.length expected)
        require (amount>0 && BS.take amount input==BS.take amount expected)
        writeIORef pending (BS.drop amount expected)
        compareBytes (BS.drop amount input)
      finish=do
        readIORef pending >>= require . BS.null
        source >>= require . (==Nothing)
  pure (compareBytes,finish)

digestText :: Context SHA256 -> Text
digestText context=T.pack (show (hashFinalize context :: Digest SHA256))

-- Hash bounded content blocks independently of codec callback boundaries.
contentBlockSink :: Int64 -> (A.SanitizedBlock -> IO ()) -> IO (ByteString -> IO (),IO Int64)
contentBlockSink bytes emit=do
  position<-newIORef (0,0,hashInit :: Context SHA256)
  let consume input=unless (BS.null input) $ do
        (ordinal,offset,context)<-readIORef position
        let boundary=min (bytes-offset) (1048576-offset `mod` 1048576)
            amount=min (BS.length input) (fromIntegral boundary)
            (piece,rest)=BS.splitAt amount input
            next=offset+fromIntegral amount
            !updated=hashUpdate context piece
        require (amount>0)
        if fromIntegral amount==boundary then do
          emit (A.SanitizedBlock ordinal (ordinal*1048576) next (digestText updated))
          writeIORef position (ordinal+1,next,hashInit)
        else writeIORef position (ordinal,next,updated)
        consume rest
      finish=do
        (count,offset,_)<-readIORef position
        require (offset==bytes)
        pure count
  pure (consume,finish)

ledgerSink :: Pool H.Connection -> A.ArchiveManifest -> IO (ByteString -> IO (),IO ())
ledgerSink pool manifest=do
  position<-newIORef (0,0,0,True::Bool)
  current<-newIORef Nothing
  pieces<-newIORef ([]::[ByteString])
  let consume bytes=unless (BS.null bytes) $ do
        (offset,revision,records,previousNewline)<-readIORef position
        cached<-readIORef current
        ledger<-case cached of
          Just value -> pure value
          Nothing -> do
            row<-R.verifiedLedger pool manifest.workspaceId manifest.sessionId manifest.id offset >>= need
            require (row.startOffset==offset && row.revision==revision+1 && row.endOffset>offset
              && row.endOffset-offset<=1048576 && row.endOffset<=manifest.rawBytes)
            writeIORef current (Just row)
            pure row
        let amount=fromIntegral (min (fromIntegral (BS.length bytes)) (ledger.endOffset-offset))
            (piece,rest)=BS.splitAt amount bytes
            end=offset+fromIntegral amount
        require (amount>0)
        modifyIORef' pieces (piece:)
        if end==ledger.endOffset then do
          original<-BS.concat . reverse <$> readIORef pieces
          let newline=BS.last original==10
              separators=if previousNewline then 0 else 1
              count=fromIntegral (BS.count 10 original)+(if newline then 0 else 1)-separators
          require (fromIntegral (BS.length original)==ledger.endOffset-ledger.startOffset
            && sessionSha256 original==ledger.rawSha256 && newline==ledger.endsWithNewline
            && count==ledger.recordCount && count>=0
            && (previousNewline || BS.isPrefixOf "\n" original || BS.isPrefixOf "\r\n" original)
            && (newline || ledger.allowFinalRecord))
          total<-C.checkedAdd records count
          writeIORef position (end,ledger.revision,total,newline)
          writeIORef current Nothing
          writeIORef pieces []
        else writeIORef position (end,revision,records,previousNewline)
        consume rest
      finish=do
        (offset,revision,records,_)<-readIORef position
        pending<-readIORef current
        require (offset==manifest.rawBytes && revision==manifest.ledgerCount && records==manifest.rawRecords
          && manifest.rawRevision==revision+1 && pending==Nothing)
  pure (consume,finish)

-- The worker owns the shared admission and renewal child through its confirmed
-- join. A pause racing verification is rechecked by DELETE and removes none.
runReclamation :: IO () -> Pool H.Connection -> M.ReclamationLease -> IO ()
runReclamation beforeVerification pool lease=do
  let manifest=lease.manifest
  verified<-M.verificationRecorded pool manifest
  unless verified $ do
    beforeVerification
    if lease.sanitized then verifyPublishedSanitized pool lease else verifyPublishedArchive pool manifest
    M.recordVerification pool lease Nothing
  ready<-M.verificationRecorded pool manifest
  when ready $ do
    _<-M.reclaimArchiveBatch pool lease
    pure ()
