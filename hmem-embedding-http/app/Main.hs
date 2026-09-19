{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Exception (SomeException, try)
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Foreign.C.Types (CInt (..))
import System.Exit (exitFailure)
import System.IO (BufferMode (NoBuffering), hFlush, hSetBinaryMode, hSetBuffering,
                  stdin, stdout)

import HMem.Embedding.HttpProtocol
  ( TransportCode (InvalidFrame), WireReply (WireFailure), decodeCall
  , encodeWireReply, frameHeader, parseFrameHeader, requestFrameCap
  )
import HMem.Embedding.HttpRequest (performHttpCall)

foreign import ccall safe "hmem_child_watch_parent"
  watchParent :: IO CInt

main :: IO ()
main = do
  hSetBinaryMode stdin True
  hSetBinaryMode stdout True
  hSetBuffering stdout NoBuffering
  watched <- watchParent
  if watched /= 1 then exitFailure else do
    sendFrame "HMEM1"
    loop

loop :: IO ()
loop = do
  next <- receiveFrame
  case next of
    Nothing -> pure ()
    Just (Left ()) -> sendFrame (encodeWireReply (WireFailure InvalidFrame))
    Just (Right payload) -> do
      response <- case decodeCall payload of
        Left code -> pure (WireFailure code)
        Right call -> do
          result <- try (performHttpCall call) :: IO (Either SomeException WireReply)
          pure (either (const (WireFailure InvalidFrame)) id result)
      sendFrame (encodeWireReply response)
      loop

receiveFrame :: IO (Maybe (Either () ByteString))
receiveFrame = do
  header <- readExactly 4
  if BS.null header then pure Nothing
  else if BS.length header /= 4 then pure (Just (Left ()))
  else case parseFrameHeader requestFrameCap header of
    Nothing -> pure (Just (Left ()))
    Just size -> do
      payload <- readExactly size
      pure $ Just $ if BS.length payload == size
        then Right payload else Left ()

readExactly :: Int -> IO ByteString
readExactly needed = go needed []
  where
    go remaining pieces
      | remaining <= 0 = pure (BS.concat (reverse pieces))
      | otherwise = do
          piece <- BS.hGet stdin remaining
          if BS.null piece
            then pure (BS.concat (reverse pieces))
            else go (remaining - BS.length piece) (piece : pieces)

sendFrame :: ByteString -> IO ()
sendFrame payload = case frameHeader (BS.length payload) of
  Nothing -> exitFailure
  Just header -> BS.hPut stdout header >> BS.hPut stdout payload >> hFlush stdout
