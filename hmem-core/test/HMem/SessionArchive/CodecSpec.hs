module HMem.SessionArchive.CodecSpec (spec) where

import Control.Exception (try, bracket)
import Control.Monad (forM_, when)
import Crypto.Hash (Context, Digest, SHA256, hashInit, hashUpdate, hashFinalize)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.IORef
import Data.List (foldl')
import Data.Text qualified as T
import Data.Bits (xor, shiftL, shiftR)
import Data.Word (Word64)
import GHC.Stats
import System.Mem (performGC)
import Test.Hspec
import HMem.AgentSession (sessionSha256)
import HMem.SessionArchive.Codec

spec :: Spec
spec = describe "Single-member archive codec" $ do
  it "round-trips empty, CRLF, UTF-8, final-no-LF and a maximum record across arbitrary source/output cuts" $ do
    forM_ ["", "{}\r\n[\"\195\166\"]", "\"" <> BS.replicate 1048574 120 <> "\"", BS.concat (replicate 700 "{\"message\":\"\195\166\195\184\195\165\"}\r\n")] $ \raw -> do
      (result,compressed) <- encodeChunks (cuts 37 raw)
      result.raw `shouldBe` StreamDigest (fromIntegral (BS.length raw)) (sessionSha256 raw)
      BS.take 10 compressed `shouldBe` BS.pack [31,139,8,0,0,0,0,0,0,255]
      forM_ [1,17,65521] $ \cut -> do
        source <- sourceOf (cuts cut compressed)
        decoded <- newIORef []
        checked <- verify (fromIntegral (BS.length raw)) source (\b -> modifyIORef' decoded (b:)) (const (pure ()))
        BS.concat . reverse <$> readIORef decoded `shouldReturn` raw
        checked.raw `shouldBe` result.raw
        checked.compressed `shouldBe` result.compressed
  it "retains one dictionary across chunks and emits bounded exact-interval restart points" $ do
    let raw = BS.concat (replicate 90000 "{\"payload\":\"repeated phrase and varied offsets\"}\r\n")
    (_,a) <- encodeChunks (cuts 1048573 raw)
    (_,b) <- encodeChunks (cuts 1021 raw)
    a `shouldBe` b
    source <- sourceOf (cuts 13 a)
    points <- newIORef []
    result <- verify (fromIntegral (BS.length raw)) source (const (pure ())) (\p -> modifyIORef' points (p:))
    checkpoints' <- reverse <$> readIORef points
    map (.rawOffset) checkpoints' `shouldBe` [0,1048576..fromIntegral (BS.length raw)-1]
    map (BS.length . (.dictionary)) checkpoints' `shouldBe` (0:replicate (length checkpoints'-1) 32768)
    forM_ checkpoints' $ \point -> point.dictionary `shouldBe`
      BS.take (fromIntegral (min 32768 point.rawOffset)) (BS.drop (fromIntegral (max 0 (point.rawOffset-32768))) raw)
    any ((/=0) . (.unusedBits)) checkpoints' `shouldBe` True
    result.checkpoints `shouldBe` fromIntegral (length checkpoints')
  it "rejects corruption, truncation, extra members, trailing bytes and wrong lengths" $ do
    (_,compressed) <- encodeChunks ["{}\r\n[]"]
    forM_ [BS.init compressed, BS.take 12 compressed, BS.take 10 compressed <> BS.singleton 255 <> BS.drop 11 compressed, compressed<>compressed, compressed<>"x"] $ \bad -> do
      source <- sourceOf (cuts 7 bad)
      result <- try @CodecFailure $ verify 6 source (const (pure ())) (const (pure ()))
      result `shouldSatisfy` either (const True) (const False)
    source <- sourceOf [compressed]
    verify 5 source (const (pure ())) (const (pure ())) `shouldThrow` anyException
  it "drains full native output buffers for deterministic high-entropy multipart input" $ do
    let chunks = [entropy seed 1048576 | seed <- [1,123456789,987654321]]
        raw = BS.concat chunks
    source <- sourceOf chunks
    parts <- newIORef []
    result <- compress source (\bytes -> modifyIORef' parts (bytes:))
    output <- reverse <$> readIORef parts
    any ((==65536) . BS.length) output `shouldBe` True
    let compressed = BS.concat output
    BS.length compressed `shouldSatisfy` (>1048576)
    input <- sourceOf (cuts 65521 compressed)
    decoded <- newIORef []
    verified <- verify (fromIntegral (BS.length raw)) input (\bytes -> modifyIORef' decoded (bytes:)) $ \point ->
      point.dictionary `shouldBe` BS.take (fromIntegral (min 32768 point.rawOffset))
        (BS.drop (fromIntegral (max 0 (point.rawOffset-32768))) raw)
    BS.concat . reverse <$> readIORef decoded `shouldReturn` raw
    verified.raw `shouldBe` result.raw
    verified.compressed `shouldBe` result.compressed
  it "keeps authoritative arithmetic above 4 GiB and rejects signed overflow" $ do
    checkedAdd 4294967296 1048593 `shouldReturn` 4296015889
    checkedAdd maxBound 1 `shouldThrow` anyException
  it "restores non-byte-aligned checkpoints and keeps one decoder across tiny sequential reads" $ do
    let raw = BS.concat (replicate 80000 "{\"payload\":\"shared dictionary across checkpoint boundaries\"}\r\n")
    (_,compressed) <- encodeChunks (cuts 65521 raw)
    source <- sourceOf (cuts 1021 compressed)
    points <- newIORef []
    _ <- verify (fromIntegral (BS.length raw)) source (const (pure ())) (\p -> modifyIORef' points (p:))
    checkpoints' <- reverse <$> readIORef points
    any ((/=0) . (.unusedBits)) checkpoints' `shouldBe` True
    forM_ checkpoints' $ \point -> do
      let start=fromIntegral point.rawOffset
          compressedStart=fromIntegral point.compressedOffset-if point.unusedBits == 0 then 0 else 1
          count=min 1048576 (BS.length raw-start)
      input <- sourceOf (cuts 17 (BS.drop compressedStart compressed))
      bracket (newDecoder point input) closeDecoder $ \decoder -> do
        pieces <- sequence [readDecoder decoder n | n <- map BS.length (cuts 1021 (BS.take count (BS.drop start raw)))]
        BS.concat pieces `shouldBe` BS.take count (BS.drop start raw)
    -- A wrong dictionary can successfully inflate plausible bytes. Integrity
    -- belongs to the complete-original-chunk hash in the compatible reader.
    case filter ((>0) . (.rawOffset)) checkpoints' of
      [] -> expectationFailure "missing dictionary checkpoint"
      point:_ -> do
        let bad=point {dictionary=BS.map (`xor` 1) point.dictionary}
            start=fromIntegral point.compressedOffset-if point.unusedBits == 0 then 0 else 1
        input <- sourceOf (cuts 65536 (BS.drop start compressed))
        result <- try @CodecFailure $ bracket (newDecoder bad input) closeDecoder (\decoder -> readDecoder decoder 65536)
        result `shouldSatisfy` either (const True) (/=BS.take 65536 (BS.drop (fromIntegral point.rawOffset) raw))
  it "hashes a generated 64 MiB stream with bounded live memory and no retained source/output list" $ do
    enabled <- getRTSStatsEnabled
    if not enabled then pendingWith "Run this gate with +RTS -T" else do
      performGC
      before <- getRTSStats
      let chunk = BS.concat (replicate 65536 "{\"message\":123}\n")
          expected = T.pack (show (hashFinalize (foldl' hashUpdate (hashInit :: Context SHA256) (replicate 64 chunk)) :: Digest SHA256))
      remaining <- newIORef (64 :: Int)
      livePeak <- newIORef 0
      let source = do
            n <- readIORef remaining
            when (n `mod` 8 == 0) $ do
              performGC
              stats <- getRTSStats
              modifyIORef' livePeak (max (gcdetails_live_bytes (gc stats)))
            if n == 0 then pure Nothing else do
              writeIORef remaining (n-1)
              -- An IO allocation cannot be floated into one shared pure copy.
              fresh <- BS.useAsCStringLen chunk BS.packCStringLen
              pure (Just fresh)
      result <- compress source (const (pure ()))
      result.raw.sha256 `shouldBe` expected
      result.raw.byteCount `shouldBe` 67108864
      readIORef livePeak >>= (`shouldSatisfy` (<= gcdetails_live_bytes (gc before) + 33554432))
      performGC
      after <- getRTSStats
      max_live_bytes after `shouldSatisfy` (<= max (max_live_bytes before) (gcdetails_live_bytes (gc before) + 33554432))

sourceOf :: [ByteString] -> IO (IO (Maybe ByteString))
sourceOf bytes = do
  ref <- newIORef bytes
  pure $ atomicModifyIORef' ref $ \case [] -> ([],Nothing); x:xs -> (xs,Just x)
cuts :: Int -> ByteString -> [ByteString]
cuts n bytes | BS.null bytes = []
             | otherwise = let (a,b)=BS.splitAt n bytes in a:cuts n b
encodeChunks :: [ByteString] -> IO (CodecResult,ByteString)
encodeChunks chunks = do
  source <- sourceOf chunks
  output <- newIORef []
  result <- compress source (\b -> modifyIORef' output (b:))
  bytes <- BS.concat . reverse <$> readIORef output
  pure (result,bytes)

entropy :: Word64 -> Int -> ByteString
entropy seed count = fst $ BS.unfoldrN count (\state ->
  let a = state `xor` (state `shiftL` 13)
      b = a `xor` (a `shiftR` 7)
      next = b `xor` (b `shiftL` 17)
  in Just (fromIntegral (next `shiftR` 24),next)) seed
