-- | Independent bounded readback before the first destructive maintenance turn.
module HMem.Server.Archive.Maintenance
  ( verifyPublishedArchive, runReclamation ) where

import Control.Exception (throwIO)
import Control.Monad (unless,when)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Int (Int64)
import Data.IORef
import Data.Pool (Pool)
import Hasql.Connection qualified as H

import HMem.AgentSession (sessionSha256)
import HMem.DB.SessionArchive qualified as A
import HMem.DB.SessionArchive.Maintenance qualified as M
import HMem.DB.SessionArchive.Reader qualified as R
import HMem.SessionArchive.Codec qualified as C

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
  position<-newIORef (0,0::Int64)
  let source=do
        (ordinal,offset)<-readIORef position
        next<-R.verifiedPart pool manifest.workspaceId manifest.sessionId manifest.id ordinal
        if ordinal==manifest.partCount then do
          require (next==Nothing && offset==manifest.compressedBytes)
          pure Nothing
        else do
          part<-need next
          require (part.ordinal==ordinal && part.startOffset==offset && part.endOffset<=manifest.compressedBytes)
          writeIORef position (ordinal+1,part.endOffset)
          pure (Just part.bytes)
  checkpoint<-newIORef (0::Int64)
  sink<-ledgerSink pool manifest
  verified<-C.verify manifest.rawBytes source (fst sink) $ \point -> do
    ordinal<-readIORef checkpoint
    stored<-M.verifiedCheckpoint pool manifest ordinal >>= need
    require (stored.ordinal==ordinal && stored.rawOffset==point.rawOffset && stored.compressedOffset==point.compressedOffset
      && fromIntegral stored.unusedBits==point.unusedBits && stored.dictionary==point.dictionary
      && stored.dictionarySha256==sessionSha256 point.dictionary)
    writeIORef checkpoint (ordinal+1)
  snd sink
  count<-readIORef checkpoint
  extra<-M.verifiedCheckpoint pool manifest count
  require (count==manifest.checkpointCount && verified.checkpoints==count && extra==Nothing
    && verified.raw.byteCount==manifest.rawBytes && verified.raw.sha256==manifest.rawSha256
    && verified.compressed.byteCount==manifest.compressedBytes && verified.compressed.sha256==manifest.compressedSha256)
  R.verifiedManifest pool manifest.workspaceId manifest.sessionId >>= \current -> require (current==Just manifest)

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
    verifyPublishedArchive pool manifest
    M.recordVerification pool lease Nothing
  ready<-M.verificationRecorded pool manifest
  when ready $ do
    _<-M.reclaimArchiveBatch pool lease
    pure ()
