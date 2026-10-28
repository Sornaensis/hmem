{-# LANGUAGE ForeignFunctionInterface #-}
-- | Bounded single-member gzip-v1. Callbacks run with no native buffer borrowed.
module HMem.SessionArchive.Codec
  ( CodecFailure(..), StreamDigest(..), CodecResult(..), Checkpoint(..)
  , codecImplementation, compress, verify, checkedAdd
  ) where

import Control.Exception (Exception, bracket, throwIO)
import Control.Monad (unless, when)
import Crypto.Hash (Context, SHA256, Digest, hashInit, hashUpdate, hashFinalize)
import Data.Bits ((.&.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Int (Int64)
import Data.IORef
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word8, Word32)
import Foreign (Ptr, nullPtr, castPtr, alloca, allocaBytes, peek)
import Foreign.C

data NativeCodec
foreign import ccall unsafe "hmem_archive_codec_new" codecNew :: CInt -> IO (Ptr NativeCodec)
foreign import ccall unsafe "hmem_archive_codec_free" codecFree :: Ptr NativeCodec -> IO ()
foreign import ccall unsafe "hmem_archive_codec_step" codecStep :: Ptr NativeCodec -> Ptr Word8 -> Word32 -> Ptr Word8 -> Word32 -> CInt -> Ptr Word32 -> Ptr Word32 -> Ptr Word32 -> IO CInt
foreign import ccall unsafe "hmem_archive_dictionary" codecDictionary :: Ptr NativeCodec -> Ptr Word8 -> Ptr Word32 -> IO CInt
foreign import ccall unsafe "hmem_archive_zlib_version" codecVersion :: IO CString

data CodecFailure = CodecFailure Text deriving stock (Show, Eq)
instance Exception CodecFailure
data StreamDigest = StreamDigest { byteCount :: !Int64, sha256 :: !Text } deriving stock (Show, Eq)
data CodecResult = CodecResult { raw :: !StreamDigest, compressed :: !StreamDigest, checkpoints :: !Int64 } deriving stock (Show, Eq)
data Checkpoint = Checkpoint
  { rawOffset :: !Int64, compressedOffset :: !Int64, unusedBits :: !Int, dictionary :: !ByteString
  } deriving stock (Show, Eq)

codecImplementation :: IO Text
codecImplementation = ("zlib-" <>) . T.pack <$> (codecVersion >>= peekCString)

require :: Bool -> Text -> IO ()
require ok why = unless ok (throwIO (CodecFailure why))

checkedAdd :: Int64 -> Int64 -> IO Int64
checkedAdd a b = require (a >= 0 && b >= 0 && a <= maxBound - b) "archive_counter_overflow" >> pure (a+b)

data HashState = HashState !Int64 !(Context SHA256)
newHash :: IO (IORef HashState)
newHash = newIORef (HashState 0 hashInit)
feedHash :: IORef HashState -> ByteString -> IO ()
feedHash ref bytes = do
  HashState n h <- readIORef ref
  total <- checkedAdd n (fromIntegral (BS.length bytes))
  writeIORef ref $! HashState total (hashUpdate h bytes)
finishHash :: IORef HashState -> IO StreamDigest
finishHash ref = do
  HashState n h <- readIORef ref
  pure (StreamDigest n (T.pack (show (hashFinalize h :: Digest SHA256))))

withCodec :: Bool -> (Ptr NativeCodec -> IO a) -> IO a
withCodec encode = bracket acquire codecFree
  where acquire = do
          p <- codecNew (if encode then 1 else 0)
          require (p /= nullPtr) "archive_codec_initialization"
          pure p

-- One copied output buffer; the C state never retains input or output pointers.
step :: Ptr NativeCodec -> ByteString -> CInt -> IO (CInt, Int, ByteString, Word32)
step codec input mode = BS.useAsCStringLen input $ \(source,n) ->
  allocaBytes 65536 $ \output -> alloca $ \used -> alloca $ \made -> alloca $ \flags -> do
    require (n <= 65536) "archive_input_buffer_bound"
    result <- codecStep codec (castPtr source) (fromIntegral n) output 65536 mode used made flags
    consumed <- fromIntegral <$> peek used
    produced <- fromIntegral <$> peek made
    boundary <- peek flags
    require (consumed <= n && produced <= 65536) "archive_native_buffer_bound"
    bytes <- BS.packCStringLen (castPtr output, produced)
    pure (result,consumed,bytes,boundary)

dictionaryAt :: Ptr NativeCodec -> IO ByteString
dictionaryAt codec = allocaBytes 32768 $ \output -> alloca $ \size -> do
  result <- codecDictionary codec output size
  count <- fromIntegral <$> peek size
  require (result == 0 && count <= 32768) "archive_dictionary_failure"
  BS.packCStringLen (castPtr output,count)

-- | The source yields original chunks of at most 1 MiB. Physical source and
-- output cuts do not reset zlib. Exactly one finalization follows source EOF.
compress :: IO (Maybe ByteString) -> (ByteString -> IO ()) -> IO CodecResult
compress source sink = withCodec True $ \codec -> do
  rawHash <- newHash
  compressedHash <- newHash
  let emit bytes = unless (BS.null bytes) (feedHash compressedHash bytes >> sink bytes)
      drain mode bytes = do
        (result,used,output,_) <- step codec bytes mode
        require (result == 0 || result == 1 || result == -5 && BS.null bytes) "archive_deflate_failure"
        emit output
        let remaining = BS.drop used bytes
        if result == 1 then require (BS.null remaining && mode == 2) "archive_deflate_end"
        else if not (BS.null remaining) || BS.length output == 65536 || mode == 2
          then do
            require (used > 0 || not (BS.null output)) "archive_deflate_stalled"
            drain mode remaining
          else pure ()
      input bytes = unless (BS.null bytes) $ do
        HashState offset _ <- readIORef rawHash
        let count = min 65536 (min (BS.length bytes) (fromIntegral (1048576 - offset `mod` 1048576)))
            (piece,rest) = BS.splitAt count bytes
        feedHash rawHash piece
        let boundary = (offset + fromIntegral count) `mod` 1048576 == 0
        drain (if boundary then 1 else 0) piece
        input rest
      loop = source >>= \case
        Nothing -> drain 2 BS.empty
        Just bytes -> do
          require (not (BS.null bytes) && BS.length bytes <= 1048576) "archive_source_chunk_bound"
          input bytes
          loop
  loop
  CodecResult <$> finishHash rawHash <*> finishHash compressedHash <*> pure 0

-- | Decode exactly one complete member, rejecting truncated streams, extra
-- members and trailing bytes. Restart points are emitted individually from the
-- actual decoder state, never collected into a session-sized index.
verify :: Int64 -> IO (Maybe ByteString) -> (ByteString -> IO ()) -> (Checkpoint -> IO ()) -> IO CodecResult
verify expected source sink checkpoint = withCodec False $ \codec -> do
  require (expected >= 0) "archive_negative_receipt"
  rawHash <- newHash
  compressedHash <- newHash
  header <- newIORef BS.empty
  previous <- newIORef Nothing
  pointCount <- newIORef 0
  consumedTotal <- newIORef 0
  let readInput = source >>= \case
        Nothing -> pure Nothing
        Just bytes -> do
          require (not (BS.null bytes) && BS.length bytes <= 1048576) "archive_source_part_bound"
          feedHash compressedHash bytes
          prefix <- readIORef header
          when (BS.length prefix < 10) $ do
            let next = prefix <> BS.take (10 - BS.length prefix) bytes
            require (next == BS.take (BS.length next) (BS.pack [31,139,8,0,0,0,0,0,0,255])) "archive_gzip_header"
            writeIORef header next
          pure (Just bytes)
      -- A full output buffer can consume the last byte of a physical part
      -- before reaching the stream end. Refill before the next native step;
      -- empty-input Z_BUF_ERROR is a request for input, not corrupt gzip.
      inflateInput bytes | BS.null bytes = readInput >>= maybe (throwIO (CodecFailure "archive_truncated")) inflateInput
      inflateInput bytes = do
        let piece = BS.take 65536 bytes
        (result,used,output,flags) <- step codec piece 0
        require (result == 0 || result == 1) "archive_inflate_failure"
        modifyIORef' consumedTotal (+ fromIntegral used)
        feedHash rawHash output
        HashState rawCount _ <- readIORef rawHash
        require (rawCount <= expected) "archive_raw_length_exceeded"
        unless (BS.null output) (sink output)
        lastPoint <- readIORef previous
        when (flags .&. 128 /= 0 && flags .&. 64 == 0 &&
              (lastPoint == Nothing || rawCount < expected && maybe True (\r -> rawCount-r >= 1048576) lastPoint)) $ do
          require (maybe (rawCount == 0) (\r -> rawCount > r && rawCount-r <= 1048576) lastPoint) "archive_checkpoint_gap"
          compressedCount <- readIORef consumedTotal
          dict <- dictionaryAt codec
          let point = Checkpoint rawCount compressedCount (fromIntegral (flags .&. 7)) dict
          require (BS.length dict == fromIntegral (min rawCount 32768)) "archive_dictionary_length"
          checkpoint point
          modifyIORef' pointCount (+1)
          writeIORef previous (Just rawCount)
        let remaining = BS.drop used bytes
        if result == 1 then do
          require (BS.null remaining) "archive_trailing_bytes"
          readInput >>= \case Nothing -> pure (); Just _ -> throwIO (CodecFailure "archive_extra_member")
        else do
          require (used > 0 || not (BS.null output) || flags .&. 128 /= 0) "archive_inflate_stalled"
          if BS.null remaining && BS.length output < 65536
            then readInput >>= maybe (throwIO (CodecFailure "archive_truncated")) inflateInput
            else inflateInput remaining
  readInput >>= maybe (throwIO (CodecFailure "archive_truncated")) inflateInput
  rawResult <- finishHash rawHash
  compressedResult <- finishHash compressedHash
  lastPoint <- readIORef previous
  require (rawResult.byteCount == expected && maybe False (\r -> expected-r <= 1048576) lastPoint) "archive_final_length_or_index"
  CodecResult rawResult compressedResult <$> readIORef pointCount
