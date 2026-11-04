module HMem.SessionArchive.ReplacementSpec (spec) where

import Control.Exception (try)
import Control.Monad (forM_, when)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.IORef
import Data.Time (UTCTime)
import GHC.Stats (gc, gcdetails_live_bytes, getRTSStats, getRTSStatsEnabled)
import System.Mem (performGC)
import Test.Hspec

import HMem.SessionArchive.Replacement

spec :: Spec
spec = describe "Sealed archive replacement" $ do
  it "freezes normalized rules and uses the exact format label" $ do
    policy <- either (fail . show) pure (decodePolicy config)
    let active = read "2026-12-02 00:00:00 UTC"
    (_,frozen) <- frozenFixture active policy
    freezePolicy active "Codex_JSONL" policy `shouldBe` Right Nothing
    frozen `shouldBe` "{\"eligible_from\":\"2026-12-02T00:00:00Z\",\"format_label\":\"codex_jsonl\",\"rules\":[{\"match\":{\"/payload/type\":\"function_call\",\"/type\":\"response_item\"},\"path\":\"/payload/encrypted_content\",\"replacement\":\"Encrypted Payload\"}],\"version\":1}"
    decodeSnapshot "codex_jsonl" active (frozen <> " ") `shouldBe` Left InvalidPolicy
    decodeSnapshot "Codex_JSONL" active frozen `shouldBe` Left InvalidPolicy
    decodePolicy ("{\"version\":1," <> BS.drop 1 config) `shouldBe` Left InvalidPolicy

  it "preserves all bytes outside the matching nonempty string token" $ do
    rules <- fixtureRules
    let prefix = " \t{ \"payload\" : {\"type\":\"function_call\", \"encrypted_content\": "
        suffix = " , \"summary\":\"\\u0061\", \"score\":-1.2300E+000} , \"type\":\"response_item\" }\r\n"
        run token = replaceRecord rules (prefix <> token <> suffix)
    run "\"opaque\\u002fsecret\"" `shouldBe` Right (prefix <> "\"Encrypted Payload\"" <> suffix,1)
    mapM_ (\token -> run token `shouldBe` Right (prefix <> token <> suffix,0))
      ["\"\"", "null", "42", "false", "{}", "[]"]
    let unmatched = "{\"type\":\"response_item\",\"payload\":{\"type\":\"other\",\"encrypted_content\":\"opaque\"}}"
    replaceRecord rules unmatched `shouldBe` Right (unmatched,0)

  it "streams UTF-8, escapes, CRLF and a finite final record across one-byte fragments" $ do
    rules <- fixtureRules
    let input = "{\"type\":\"response_item\",\"payload\":{\"type\":\"function_call\",\"encrypted_content\":\"\195\169\\tsecret\"}}\r\n [] "
        expected = "{\"type\":\"response_item\",\"payload\":{\"type\":\"function_call\",\"encrypted_content\":\"Encrypted Payload\"}}\r\n [] "
    (source,count) <- newReplacementSource rules =<< fromChunks (map BS.singleton (BS.unpack input))
    drain source `shouldReturn` expected
    count `shouldReturn` 1

  it "batches short unchanged records next to shrinking records from distinct maximum chunks" $ do
    rules <- fixtureRules
    let prefix = "{\"type\":\"response_item\",\"payload\":{\"type\":\"function_call\",\"encrypted_content\":\""
        suffix = "\"}}\n[]\n"
        chunk = prefix <> BS.replicate (1048576 - BS.length prefix - BS.length suffix) 120 <> suffix
        expected = prefix <> "Encrypted Payload" <> suffix
        chunks = 130
        liveBytes = gcdetails_live_bytes . gc <$> getRTSStats
    measure <- getRTSStatsEnabled
    baseline <- if measure then performGC >> (Just <$> liveBytes) else pure Nothing
    remaining <- newIORef chunks
    liveDuringBatch <- newIORef Nothing
    let input = do
          n <- readIORef remaining
          -- At 127 source chunks, the 256-piece batch is almost full.
          when (measure && n == 3) $ do
            performGC
            liveBytes >>= writeIORef liveDuringBatch . Just
          if n == 0 then pure Nothing else do
            writeIORef remaining (n-1)
            -- Allocate in IO so each source chunk really owns distinct bytes.
            Just <$> BS.useAsCStringLen chunk BS.packCStringLen
    (source,count) <- newReplacementSource rules input
    drain source `shouldReturn` BS.concat (replicate chunks expected)
    count `shouldReturn` fromIntegral chunks
    forM_ baseline $ \baselineBytes ->
      readIORef liveDuringBatch >>= (`shouldSatisfy` maybe False (<= baselineBytes + 33554432))

  it "rejects duplicate decoded keys, ambiguous rules, excessive nesting and incomplete tails" $ do
    rules <- fixtureRules
    replaceRecord rules "{\"x\":1,\"\\u0078\":2}" `shouldBe` Left AmbiguousRecord
    replaceRecord rules (BS.replicate 65 91 <> BS.replicate 65 93) `shouldBe` Left RecordLimit
    replaceRecord rules (BS.replicate 1048577 32) `shouldBe` Left RecordLimit
    let matching = "{\"type\":\"response_item\",\"payload\":{\"type\":\"function_call\",\"encrypted_content\":\"opaque\"}}"
    replaceRecord (rules <> rules) matching `shouldBe` Left AmbiguousRecord
    (source,_) <- newReplacementSource rules =<< fromChunks ["{}\n{\"x\":","1"]
    try (drain source) `shouldReturn` Left InvalidRecord

config :: ByteString
config = "{\"version\":1,\"eligible_from\":\"2026-12-01T00:00:00Z\",\"formats\":{\"codex_jsonl\":[{\"path\":\"/payload/encrypted_content\",\"match\":{\"/type\":\"response_item\",\"/payload/type\":\"function_call\"}}]}}"

fixtureRules :: IO [Rule]
fixtureRules = do
  policy <- either (fail . show) pure (decodePolicy config)
  (cutoff,snapshot) <- frozenFixture (read "2026-12-02 00:00:00 UTC") policy
  either (fail . show) pure (decodeSnapshot "codex_jsonl" cutoff snapshot)

frozenFixture :: UTCTime -> Policy -> IO (UTCTime,ByteString)
frozenFixture active policy = do
  frozen <- either (fail . show) pure (freezePolicy active "codex_jsonl" policy)
  maybe (fail "missing frozen policy") pure frozen

fromChunks :: [ByteString] -> IO (IO (Maybe ByteString))
fromChunks input = do
  ref <- newIORef input
  pure $ atomicModifyIORef' ref $ \case
    [] -> ([],Nothing)
    bytes:rest -> (rest,Just bytes)

drain :: IO (Maybe ByteString) -> IO ByteString
drain source = BS.concat <$> go
  where
    go = source >>= \case
      Nothing -> pure []
      Just bytes -> (bytes :) <$> go
