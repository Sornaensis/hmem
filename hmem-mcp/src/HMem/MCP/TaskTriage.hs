-- Download the server's streaming ZIP64 archive, validate it on disk, then
-- publish a complete new file. No transcript bytes enter the MCP response.
module HMem.MCP.TaskTriage (downloadEvidence, validateArchive) where

import Codec.Compression.Zlib.Internal qualified as Z
import Control.Exception hiding (handle)
import Control.Monad (unless, when)
import Crypto.Hash (Context, Digest, SHA256, hashInit, hashUpdate, hashFinalize)
import Data.Aeson
import Data.Bits (xor, shiftR, shiftL, (.|.), (.&.))
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.UUID (UUID)
import Data.Word (Word8, Word32)
import Network.HTTP.Client
import Network.HTTP.Types (Header, statusCode)
import System.Directory (doesPathExist, removeFile)
import System.FilePath (takeDirectory)
import System.IO
import System.Timeout (timeout)

import HMem.MCP.SessionFile (publishNewFile)

data ArchiveFault = ArchiveInvalid | DestinationExists | ArchiveHTTP Int | ArchiveTimeout | PublishFailed
  deriving (Show)
instance Exception ArchiveFault

downloadEvidence :: Manager -> String -> [Header] -> UUID -> UUID -> FilePath -> IO Value
downloadEvidence manager base headers workspace task destination = do
  attempted <- try @SomeException $ do
    exists <- doesPathExist destination
    when exists $ throwIO DestinationExists
    let directory = takeDirectory destination
        cleanup (path,handle) = do
          hClose handle `catch` (\(_ :: IOException) -> pure ())
          existsNow <- doesPathExist path
          when existsNow (removeFile path)
    bracket (openBinaryTempFile directory ".hmem-evidence-") cleanup $ \(temporary,handle) -> do
      completed <- timeout (900*1000000) $ do
        initial <- parseRequest (base <> "/api/v1/workspaces/" <> show workspace <> "/tasks/" <> show task <> "/evidence.zip")
        let request = initial {requestHeaders=headers,responseTimeout=responseTimeoutMicro (30*1000000),decompress=const False}
        (total,digest) <- withResponse request manager $ \response -> do
          let status = statusCode (responseStatus response)
          unless (status == 200) $ throwIO (ArchiveHTTP status)
          let copy total context = do
                bytes <- brRead (responseBody response)
                if BS.null bytes then pure (total,T.pack (show (hashFinalize context :: Digest SHA256))) else do
                  let next = total + fromIntegral (BS.length bytes)
                  when (next < total) $ throwIO ArchiveInvalid
                  BS.hPut handle bytes
                  copy next (hashUpdate context bytes)
          copy (0 :: Int64) (hashInit :: Context SHA256)
        hFlush handle
        count <- validateArchive handle
        pure (total,digest,count)
      (total,digest,count) <- maybe (throwIO ArchiveTimeout) pure completed
      mask_ $ do
        hClose handle
        published <- publishNewFile temporary destination
        unless published $ do
          existsNow <- doesPathExist destination
          throwIO (if existsNow then DestinationExists else PublishFailed)
        pure $ object ["workspace_id" .= workspace,"task_id" .= task,"archive_bytes" .= total,"sha256" .= digest,"session_count" .= count,"verified" .= True]
  case attempted of
    Right value -> pure $ envelope value
    Left exception | Just (_ :: SomeAsyncException) <- fromException exception -> throwIO exception
    Left exception -> pure $ failure $ case fromException exception of
      Just DestinationExists -> "destination_exists"
      Just (ArchiveHTTP status) | status == 401 -> "unauthorized"
      Just (ArchiveHTTP status) | status == 403 -> "forbidden"
      Just (ArchiveHTTP status) | status == 404 -> "task_not_found"
      Just ArchiveTimeout -> "archive_timeout"
      Just ArchiveInvalid -> "archive_invalid"
      Just PublishFailed -> "publication_failed"
      _ -> "archive_download_failed"
  where
    envelope value = object ["structuredContent" .= value,"content" .= [object ["type" .= ("text" :: Text),"text" .= TE.decodeUtf8 (BL.toStrict (encode value))]]]
    failure code = object ["isError" .= True,"structuredContent" .= object ["error" .= object ["code" .= (code :: Text),"message" .= ("Task evidence archive was not published." :: Text)]],"content" .= [object ["type" .= ("text" :: Text),"text" .= ("Task evidence archive was not published." :: Text)]]]

-- Deliberately accepts the bounded ZIP64 profile produced by SessionArchive,
-- rather than extracting arbitrary ZIPs or materializing their central index.
validateArchive :: Handle -> IO Int64
validateArchive handle = do
  lengthValue <- hFileSize handle
  ensure (lengthValue >= 98)
  footer <- at (lengthValue-98) 98
  ensure (u32 footer 0 == 0x06064b50 && u64 footer 4 == 44 && u32 footer 16 == 0 && u32 footer 20 == 0
    && u64 footer 24 == u64 footer 32 && u32 footer 56 == 0x07064b50 && u32 footer 60 == 0
    && u64 footer 64 == lengthValue-98 && u32 footer 72 == 1 && u32 footer 76 == 0x06054b50
    && u16 footer 80 == 0 && u16 footer 82 == 0 && u16 footer 84 == 0xffff && u16 footer 86 == 0xffff
    && u32 footer 88 == 0xffffffff && u32 footer 92 == 0xffffffff && u16 footer 96 == 0)
  let count = u64 footer 32; centralSize = u64 footer 40; centralStart = u64 footer 48
  ensure (centralStart+centralSize == lengthValue-98 && count <= centralSize `div` 74 && count <= fromIntegral (maxBound :: Int64))
  let entries remaining cursor localEnd
        | remaining == 0 = ensure (cursor == centralStart+centralSize && localEnd == centralStart)
        | otherwise = do
            ensure (cursor+46 <= centralStart+centralSize)
            central <- at cursor 46
            let nameLength=u16 central 28; extraLength=u16 central 30; commentLength=u16 central 32
            ensure (u32 central 0 == 0x02014b50 && u16 central 8 == 0x808 && u16 central 10 == 8
              && nameLength>0 && nameLength<=1024 && extraLength==28 && commentLength==0
              && u16 central 34==0 && u32 central 20==0xffffffff && u32 central 24==0xffffffff && u32 central 42==0xffffffff)
            name <- at (cursor+46) (fromInteger nameLength)
            extra <- at (cursor+46+nameLength) 28
            ensure (u16 extra 0==1 && u16 extra 2==24)
            let rawSize=u64 extra 4; compressed=u64 extra 12; start=u64 extra 20; expected=u32 central 16
            ensure (start==localEnd && start+30+nameLength+20+compressed+24<=centralStart)
            local <- at start 30
            ensure (u32 local 0==0x04034b50 && u16 local 6==0x808 && u16 local 8==8
              && u16 local 26==nameLength && u16 local 28==20 && u32 local 18==0xffffffff && u32 local 22==0xffffffff)
            localName <- at (start+30) (fromInteger nameLength)
            localExtra <- at (start+30+nameLength) 20
            ensure (localName==name && u16 localExtra 0==1 && u16 localExtra 2==16 && u64 localExtra 4==rawSize && u64 localExtra 12==0)
            let dataStart=start+30+nameLength+20; descriptorStart=dataStart+compressed
            hSeek handle AbsoluteSeek dataStart
            checksum <- inflate compressed rawSize
            descriptor <- at descriptorStart 24
            ensure (checksum==expected && u32 descriptor 0==0x08074b50 && u32 descriptor 4==expected
              && u64 descriptor 8==compressed && u64 descriptor 16==rawSize)
            entries (remaining-1) (cursor+46+nameLength+28) (descriptorStart+24)
  entries count centralStart 0
  pure (fromInteger count)
  where
    ensure condition = unless condition (throwIO ArchiveInvalid)
    at offset count = do
      hSeek handle AbsoluteSeek offset
      bytes <- BS.hGet handle count
      ensure (BS.length bytes==count)
      pure bytes
    inflate compressed rawSize = go compressed 0 (0xffffffff :: Word32) (Z.decompressIO Z.rawFormat Z.defaultDecompressParams{Z.decompressBufferSize=65536})
      where
        go remaining produced crc (Z.DecompressInputRequired next) = do
          ensure (remaining>0)
          let count=fromInteger (min 65536 remaining)
          bytes <- BS.hGet handle count
          ensure (BS.length bytes==count)
          next bytes >>= go (remaining-fromIntegral count) produced crc
        go remaining produced crc (Z.DecompressOutputAvailable bytes next) = do
          let total=produced+fromIntegral (BS.length bytes)
          ensure (total<=rawSize)
          next >>= go remaining total (BS.foldl' crcByte crc bytes)
        go remaining produced crc (Z.DecompressStreamEnd leftover) = do
          ensure (remaining==0 && BS.null leftover && produced==rawSize)
          pure (toInteger (crc `xor` 0xffffffff))
        go _ _ _ (Z.DecompressStreamError _) = throwIO ArchiveInvalid

u16, u32, u64 :: BS.ByteString -> Int -> Integer
u16 bytes offset = little bytes offset 2
u32 bytes offset = little bytes offset 4
u64 bytes offset = little bytes offset 8
little :: BS.ByteString -> Int -> Int -> Integer
little bytes offset count = foldr (\index value -> (value `shiftL` 8) .|. fromIntegral (BS.index bytes (offset+index))) 0 [0..count-1]

crcByte :: Word32 -> Word8 -> Word32
crcByte previous byte = go (previous `xor` fromIntegral byte) (8 :: Int)
  where
    go value 0 = value
    go value n = go (if value .&. 1==1 then 0xedb88320 `xor` (value `shiftR` 1) else value `shiftR` 1) (n-1)
