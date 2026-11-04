{-# LANGUAGE NumericUnderscores #-}
module HMem.Server.SessionArchive (newArchiveMiddleware, newArchiveMiddlewareWithResolver, streamArchive, archiveEntryName) where

import Codec.Compression.Zlib.Internal qualified as Z
import Control.Concurrent.STM
import Control.Exception (bracket, finally, mask, throwIO)
import Control.Monad (forM_, unless, when)
import Crypto.Hash (Context, Digest, SHA256, hashInit, hashUpdate, hashFinalize)
import Data.Bits (xor, shiftR, (.&.))
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as B
import Data.ByteString.Lazy qualified as LBS
import Data.Char (isAlphaNum)
import Data.IORef
import Data.Pool (Pool)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Data.Word (Word8, Word32, Word64)
import Data.Int (Int64)
import Data.ByteString.Char8 qualified as BS8
import Hasql.Connection qualified as Hasql
import Network.HTTP.Types
import Network.Wai qualified as Wai
import Servant (runHandler, ServerError(..))
import System.Directory (getTemporaryDirectory, removeFile)
import System.IO
import System.Timeout (timeout)

import HMem.AgentSession qualified as S
import HMem.DB.AgentSession qualified as DB
import HMem.DB.RequestContext (Principal, currentRequestContext, withRequestContext, RequestContext(..))
import HMem.DB.SessionArchive qualified as Archive
import HMem.DB.SessionArchive.Reader qualified as Reader
import HMem.Server.API (authorizeTaskEvidenceRead,authorizeTaskEvidenceExport,requireBotSessionEvidence)
import HMem.Server.Exception (trySynchronous)

-- There is one gate per application, held through network backpressure and cleanup.
newArchiveMiddleware :: Pool Hasql.Connection -> IO Wai.Middleware
newArchiveMiddleware pool = newArchiveMiddlewareWithResolver pool Nothing

newArchiveMiddlewareWithResolver :: Pool Hasql.Connection -> Maybe (Wai.Request -> IO (Maybe Principal)) -> IO Wai.Middleware
newArchiveMiddlewareWithResolver pool resolve = do
  active <- newTVarIO (0 :: Int)
  pure $ \app request respond -> case Wai.pathInfo request of
    ["api","v1","workspaces",workspaceText,"tasks",taskText,"evidence.zip"]
      | Wai.requestMethod request == methodGet
      , Just workspace <- UUID.fromText workspaceText
      , Just task <- UUID.fromText taskText -> do
          authorized <- runHandler (authorizeTaskEvidenceExport pool workspace task)
          case authorized of
            Left failure -> respond (Wai.responseLBS (mkStatus failure.errHTTPCode (BS8.pack failure.errReasonPhrase)) failure.errHeaders failure.errBody)
            Right () -> case representation request of
              Nothing -> respond $ archiveError status400 "session_invalid_input" "Invalid archive representation"
              Just mixed -> do
                available <- trySynchronous (if mixed then pure True else not <$> DB.taskOriginalEvidenceUnavailable pool workspace task)
                case available of
                  Left _ -> respond $ archiveError status503 "archive_unavailable" "Complete task evidence is unavailable"
                  Right False -> respond $ archiveError status409 "original_evidence_unavailable" "Complete original evidence is unavailable"
                  Right True -> streamResponse active mixed request workspace task taskText respond
    _ -> app request respond
  where
    streamResponse active mixed request workspace task taskText respond = mask $ \restore -> do
              admitted <- atomically $ do
                count <- readTVar active
                if count >= 2 then pure False else writeTVar active (count+1) >> pure True
              if not admitted then respond $ Wai.responseLBS status503 [(hContentType,"application/json"),("Retry-After","1")] "{\"error\":\"archive_busy\",\"message\":\"Evidence exports are busy. Retry shortly.\"}"
              else do
                context <- currentRequestContext
                let check = do
                      when mixed $ forM_ resolve $ \freshPrincipal -> do
                        fresh <- freshPrincipal request
                        unless (fresh /= Nothing && fresh == context.principal) failed
                      runHandler (authorizeTaskEvidenceRead pool workspace task) >>= either (const failed) pure
                    checkEntry identifier = check >> (runHandler (requireBotSessionEvidence pool workspace identifier) >>= either (const failed) pure)
                    body send flush = withRequestContext context{workspaceId=Just workspace} $ do
                      attempted <- trySynchronous $ timeout (15 * 60 * 1_000_000) $
                        streamArchiveWith mixed pool workspace task check checkEntry (\bytes -> send (B.byteString bytes) >> flush)
                      case attempted of Right (Just ()) -> pure (); _ -> failed
                    suffix = if mixed then "-representations.zip" else "-evidence.zip"
                    headers = [(hContentType,"application/zip"),("Content-Disposition","attachment; filename=task-"<>TE.encodeUtf8 taskText<>suffix),("Cache-Control","no-store"),("X-Content-Type-Options","nosniff")]
                      <> [("X-HMem-Evidence-Zip","representations-v2") | mixed]
                restore (respond (Wai.responseStream status200 headers body)) `finally`
                  atomically (modifyTVar' active (subtract 1))
    failed = throwIO (userError "Task evidence archive failed")
    representation request = case [value | (key,value) <- Wai.queryString request, key=="representation"] of
      [] -> Just False
      [Just "available-v1"] -> Just True
      _ -> Nothing
    archiveError status code message = Wai.responseLBS status [(hContentType,"application/json"),("Cache-Control","no-store")]
      (LBS.fromStrict $ "{\"error\":\""<>code<>"\",\"message\":\""<>message<>"\"}")

archiveEntryName :: S.AgentSession -> BS.ByteString
archiveEntryName session = TE.encodeUtf8 $ label <> "-" <> UUID.toText session.id <> ".jsonl"
  where
    clean = T.map (\c -> if isAlphaNum c || c `elem` ("._ -" :: String) then c else '-') (T.take 72 session.formatLabel)
    label = case T.dropAround (`elem` (". -" :: String)) (T.replace ".." "-" clean) of "" -> "session"; value -> value

-- Only the current page, one raw range, zlib state and one index record are live.
-- The original evidence never enters an unbounded lazy ByteString.
streamArchive :: Pool Hasql.Connection -> UUID -> UUID -> IO () -> (UUID -> IO ()) -> (BS.ByteString -> IO ()) -> IO ()
streamArchive = streamArchiveWith False

streamArchiveWith :: Bool -> Pool Hasql.Connection -> UUID -> UUID -> IO () -> (UUID -> IO ()) -> (BS.ByteString -> IO ()) -> IO ()
streamArchiveWith mixed pool workspace task check checkEntry send = do
  temporary <- getTemporaryDirectory
  bracket (openBinaryTempFile temporary "hmem-task-evidence-index-")
    (\(name,handle) -> hClose handle `finally` removeFile name) $ \(_,index) -> do
      offset <- newIORef (0 :: Word64)
      count <- newIORef (0 :: Word64)
      let emit bytes = do
            before <- readIORef offset
            let size = fromIntegral (BS.length bytes)
            when (maxBound-before < size) failArchive
            send bytes
            writeIORef offset (before+size)
          builder = mapM_ emit . LBS.toChunks . B.toLazyByteString
          entry session = do
            checkEntry session.id
            size <- maybe failArchive pure session.finalBytes
            expected <- maybe failArchive pure session.finalSha256
            unless (session.state == S.Terminated && size >= 0 && size == session.committedBytes) failArchive
            sanitized <- if mixed then Reader.verifiedSanitizedManifest pool workspace session.id Nothing else pure Nothing
            forM_ sanitized $ \selected -> unless
              (selected.archive.rawBytes==size && selected.archive.rawSha256==expected &&
               selected.archive.rawRevision==session.revision && selected.archive.rawRecords==session.committedRecords) failArchive
            let originalName=archiveEntryName session
                name=maybe originalName (const (BS.take (BS.length originalName-6) originalName<>"-sanitized.jsonl")) sanitized
                contentSize=maybe size ((.contentBytes) . (.content)) sanitized
                contentHash=maybe expected ((.contentSha256) . (.content)) sanitized
                rawSize=fromIntegral contentSize
                receipt=if mixed then zipReceipt (fmap ((.id) . (.archive)) sanitized) contentHash else BS.empty
            start <- readIORef offset
            builder (local name rawSize receipt)
            compressedStart <- readIORef offset
            cursor <- newIORef (0 :: Int64)
            crc <- newIORef (0xffffffff :: Word32)
            hash <- newIORef (hashInit :: Context SHA256)
            let readOriginal use = bracket (DB.newSessionReader pool workspace session.id size) DB.closeSessionReader (use . DB.readSessionReader)
                readSelected use = case sanitized of
                  Nothing -> readOriginal use
                  Just selected -> bracket (Reader.openSanitizedReader pool selected) Reader.closeArchiveReader (use . Reader.readArchiveRange)
            readSelected $ \readWindow -> do
              let input = do
                    at <- readIORef cursor
                    if at == contentSize then pure BS.empty else do
                      checkEntry session.id
                      bytes <- readWindow at 65536
                      unless (not (BS.null bytes) && fromIntegral (BS.length bytes) == min 65536 (contentSize-at)) failArchive
                      writeIORef cursor (at+fromIntegral (BS.length bytes))
                      modifyIORef' crc (\previous -> BS.foldl' crcByte previous bytes)
                      modifyIORef' hash (`hashUpdate` bytes)
                      pure bytes
                  compress (Z.CompressInputRequired next) = input >>= next >>= compress
                  compress (Z.CompressOutputAvailable bytes next) = checkEntry session.id >> emit bytes >> next >>= compress
                  compress Z.CompressStreamEnd = pure ()
              compress (Z.compressIO Z.rawFormat Z.defaultCompressParams{Z.compressBufferSize=65536})
            actual <- readIORef cursor
            digest <- readIORef hash
            unless (actual == contentSize && T.pack (show (hashFinalize digest :: Digest SHA256)) == contentHash) failArchive
            checksum <- xor 0xffffffff <$> readIORef crc
            end <- readIORef offset
            let compressed=end-compressedStart
            builder (descriptor checksum compressed rawSize)
            B.hPutBuilder index (central name checksum compressed rawSize start receipt)
            modifyIORef' count (+1)
          pages before = do
            check
            page <- DB.listTaskSessionsFiltered pool workspace task (S.SessionFilter (Just S.Terminated) Nothing Nothing Nothing Nothing) before 50
            let keys=map (\s -> (s.createdAt,s.id)) page.items
                bound=fmap (\c -> (c.createdAt,c.id)) before
                ordered previous []=True
                ordered previous (key:rest)=(maybe True (key <) previous) && ordered (Just key) rest
            unless (ordered bound keys && (case page.nextCursor of Nothing -> True; Just next -> not (null keys) && Just (next.createdAt,next.id) == (if null keys then Nothing else Just (last keys)))) failArchive
            mapM_ entry page.items
            maybe (pure ()) (pages . Just) page.nextCursor
      pages Nothing
      check
      centralStart <- readIORef offset
      hFlush index
      hSeek index AbsoluteSeek 0
      let copy = do bytes <- BS.hGet index 65536; unless (BS.null bytes) (emit bytes >> copy)
      copy
      end <- readIORef offset
      entries <- readIORef count
      builder (footer entries (end-centralStart) centralStart end)
  where failArchive = throwIO (userError "Task evidence archive failed")

crcByte :: Word32 -> Word8 -> Word32
crcByte previous byte = go (previous `xor` fromIntegral byte) (8 :: Int)
  where go value 0=value
        go value n=go (if value .&. 1 == 1 then 0xedb88320 `xor` (value `shiftR` 1) else value `shiftR` 1) (n-1)

-- ZIP extra field 0x484d carries a versioned per-entry content receipt in both
-- headers. ZIP64 already provides the exact uncompressed byte count.
zipReceipt :: Maybe UUID -> Text -> BS.ByteString
zipReceipt generation digest = LBS.toStrict $ B.toLazyByteString $
  B.word16LE 0x484d <> B.word16LE 102 <> B.word8 2 <> B.word8 (maybe 0 (const 1) generation)
  <> B.byteString (TE.encodeUtf8 (UUID.toText (maybe UUID.nil id generation)))
  <> B.byteString (TE.encodeUtf8 digest)

local :: BS.ByteString -> Word64 -> BS.ByteString -> B.Builder
local name size receipt = B.word32LE 0x04034b50 <> B.word16LE 45 <> B.word16LE 0x808 <> B.word16LE 8 <> B.word16LE 0 <> B.word16LE 0x21 <> B.word32LE 0 <> B.word32LE 0xffffffff <> B.word32LE 0xffffffff <> B.word16LE (fromIntegral $ BS.length name) <> B.word16LE (fromIntegral $ 20+BS.length receipt) <> B.byteString name <> B.word16LE 1 <> B.word16LE 16 <> B.word64LE size <> B.word64LE 0 <> B.byteString receipt

descriptor :: Word32 -> Word64 -> Word64 -> B.Builder
descriptor crc compressed size = B.word32LE 0x08074b50 <> B.word32LE crc <> B.word64LE compressed <> B.word64LE size

central :: BS.ByteString -> Word32 -> Word64 -> Word64 -> Word64 -> BS.ByteString -> B.Builder
central name crc compressed size start receipt = B.word32LE 0x02014b50 <> B.word16LE 45 <> B.word16LE 45 <> B.word16LE 0x808 <> B.word16LE 8 <> B.word16LE 0 <> B.word16LE 0x21 <> B.word32LE crc <> B.word32LE 0xffffffff <> B.word32LE 0xffffffff <> B.word16LE (fromIntegral $ BS.length name) <> B.word16LE (fromIntegral $ 28+BS.length receipt) <> B.word16LE 0 <> B.word16LE 0 <> B.word16LE 0 <> B.word32LE 0 <> B.word32LE 0xffffffff <> B.byteString name <> B.word16LE 1 <> B.word16LE 24 <> B.word64LE size <> B.word64LE compressed <> B.word64LE start <> B.byteString receipt

footer :: Word64 -> Word64 -> Word64 -> Word64 -> B.Builder
footer count size start end = B.word32LE 0x06064b50 <> B.word64LE 44 <> B.word16LE 45 <> B.word16LE 45 <> B.word32LE 0 <> B.word32LE 0 <> B.word64LE count <> B.word64LE count <> B.word64LE size <> B.word64LE start <> B.word32LE 0x07064b50 <> B.word32LE 0 <> B.word64LE end <> B.word32LE 1 <> B.word32LE 0x06054b50 <> B.word16LE 0 <> B.word16LE 0 <> B.word16LE 0xffff <> B.word16LE 0xffff <> B.word32LE 0xffffffff <> B.word32LE 0xffffffff <> B.word16LE 0
