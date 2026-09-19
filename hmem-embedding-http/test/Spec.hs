{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Concurrent (threadDelay)
import qualified Data.ByteString as BS
import Foreign.C.Types (CInt (..))
import System.Environment (getArgs, lookupEnv)
import System.IO (BufferMode (NoBuffering), hFlush, hSetBinaryMode,
                  hSetBuffering, stderr, stdin, stdout)
import Test.Hspec (hspec)
import qualified HttpProcessSpec
import HMem.Embedding.HttpProtocol (parseFrameHeader, requestFrameCap)

foreign import ccall safe "hmem_child_watch_parent"
  watchParent :: IO CInt

main :: IO ()
main = do
  fake <- lookupEnv "HMEM_HTTP_FAKE_HELPER_MODE"
  args <- getArgs
  case fake of
    Just mode -> fakeHelper mode
    Nothing -> case args of
      ["--parent-crash-driver", helper, endpoint, mode] ->
        HttpProcessSpec.crashDriver helper endpoint mode
      [] -> hspec HttpProcessSpec.spec
      _ -> fail "invalid standalone test driver arguments"

fakeHelper :: String -> IO ()
fakeHelper mode = do
  hSetBinaryMode stdin True
  hSetBinaryMode stdout True
  hSetBinaryMode stderr True
  hSetBuffering stdout NoBuffering
  watched <- watchParent
  if watched /= 1 then fail "fake helper watchdog failed" else do
    if mode == "stderr-flood" then do
      BS.hPut stderr (BS.replicate 70000 88)
      hFlush stderr
    else pure ()
    BS.hPut stdout "\0\0\0\5HMEM1"
    hFlush stdout
    case mode of
      "no-read" -> threadDelay 5000000
      "malformed-frame" -> receiveRequest >>
        BS.hPut stdout "\0\0\0\3\1\0\0" >> hFlush stdout
      "oversized-frame" -> receiveRequest >>
        BS.hPut stdout "\255\255\255\255" >> hFlush stdout
      "stderr-flood" -> pure ()
      _ -> fail "unknown fake helper mode"

receiveRequest :: IO ()
receiveRequest = do
  header <- BS.hGet stdin 4
  case parseFrameHeader requestFrameCap header of
    Nothing -> fail "invalid fake-helper request header"
    Just size -> do
      payload <- BS.hGet stdin size
      if BS.length payload == size then pure ()
      else fail "truncated fake-helper request"
