{-# LANGUAGE ForeignFunctionInterface #-}
-- Stable identity belongs to the same native handle that supplies the bytes.
module HMem.MCP.SessionFile
  ( Source, SourceFault(..), openSource, closeSource, checkSource, readSource, openSourceCount, publishNewFile ) where

import Control.Concurrent.MVar
import Control.Exception
import Control.Monad (unless, when)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Int (Int64)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Word (Word64, Word8)
import Foreign (Ptr, nullPtr, alloca, allocaBytes, castPtr, peek)
import Foreign.C

data Source = Source !FilePath !(MVar (Maybe (Ptr ()))) !(Word64, ByteString)
data SourceFault = SourceUnavailable | SourceChanged | SourceTruncated | SourceClosed
  deriving (Show, Eq)
instance Exception SourceFault

foreign import ccall safe "hmem_session_open" nativeOpen :: CString -> IO (Ptr ())
foreign import ccall safe "hmem_session_close" nativeClose :: Ptr () -> IO CInt
foreign import ccall safe "hmem_session_info" nativeInfo :: Ptr () -> Ptr Word64 -> Ptr Word8 -> Ptr Int64 -> IO CInt
foreign import ccall safe "hmem_session_read" nativeRead :: Ptr () -> Int64 -> Ptr Word8 -> CInt -> IO CInt
foreign import ccall unsafe "hmem_session_open_count" nativeCount :: IO CInt
foreign import ccall safe "hmem_session_publish" nativePublish :: CString -> CString -> IO CInt

-- Same-directory atomic publication. The native operation never replaces an
-- existing destination, including a destination created during the download.
publishNewFile :: FilePath -> FilePath -> IO Bool
publishNewFile source target =
  BS.useAsCString (TE.encodeUtf8 (T.pack source)) $ \from ->
    BS.useAsCString (TE.encodeUtf8 (T.pack target)) $ \to -> (/=0) <$> nativePublish from to

openSourceCount :: IO Int
openSourceCount = fromIntegral <$> nativeCount

openNative :: FilePath -> IO (Ptr ())
openNative path = do
  when ('\0' `elem` path) $ throwIO SourceUnavailable
  pointer <- BS.useAsCString (TE.encodeUtf8 (T.pack path)) nativeOpen
  when (pointer == nullPtr) $ throwIO SourceUnavailable
  pure pointer

info :: Ptr () -> IO ((Word64, ByteString), Int64)
info pointer = alloca $ \volume -> allocaBytes 16 $ \identifier -> alloca $ \size -> do
  ok <- nativeInfo pointer volume identifier size
  unless (ok /= 0) $ throwIO SourceUnavailable
  identity <- (,) <$> peek volume <*> BS.packCStringLen (castPtr identifier, 16)
  lengthValue <- peek size
  pure (identity, lengthValue)

openSource :: FilePath -> IO Source
openSource path = mask_ $ do
  pointer <- openNative path
  (identity, _) <- info pointer `onException` nativeClose pointer
  lock <- newMVar (Just pointer) `onException` nativeClose pointer
  pure (Source path lock identity)

closeSource :: Source -> IO ()
closeSource (Source _ lock _) = mask_ $ do
  ok <- modifyMVar lock $ \case
    Nothing -> pure (Nothing, 1)
    Just pointer -> do
      result <- nativeClose pointer
      pure (Nothing, result)
  unless (ok /= 0) $ throwIO SourceUnavailable

withSource :: Source -> (Ptr () -> IO a) -> IO a
withSource (Source _ lock _) action = withMVar lock $ maybe (throwIO SourceClosed) action

checkSource :: Source -> Int64 -> IO Int64
checkSource source@(Source path _ expected) minimumSize = withSource source $ \pointer -> do
  (identity, size) <- info pointer
  current <- bracket (openNative path) (\p -> nativeClose p >> pure ()) info
  unless (identity == expected && fst current == expected) $ throwIO SourceChanged
  when (size < minimumSize || snd current < minimumSize) $ throwIO SourceTruncated
  pure (min size (snd current))

readSource :: Source -> Int64 -> Int -> IO ByteString
readSource source offset count
  | offset < 0 || count < 0 || count > 1048576 = throwIO SourceUnavailable
  | otherwise = withSource source $ \pointer -> allocaBytes (max 1 count) $ \buffer -> do
      n <- nativeRead pointer offset buffer (fromIntegral count)
      when (n < 0) $ throwIO SourceUnavailable
      BS.packCStringLen (castPtr buffer, fromIntegral n)
