module HMem.Document.CodecSpec (spec) where

import Control.Exception (SomeException,try)
import Control.Monad (forM_)
import Data.ByteString qualified as BS
import Test.Hspec
import HMem.Document.Codec
import HMem.Document.Types

spec :: Spec
spec = describe "Document codec" $ do
  it "roundtrips empty, binary and multipart originals byte-for-byte" $
    forM_ ["",BS.pack [0..255],BS.concat(replicate 5000 (BS.pack[0..255]))] $ \bytes -> do
      (manifest,parts)<-encodePayload maxOriginalBytes bytes
      manifest.rawSha256 `shouldBe` payloadSha256 bytes
      all (\p -> BS.length p<=1048576) parts `shouldBe` True
      decodePayload maxOriginalBytes manifest parts `shouldReturn` bytes
  it "rejects raw/compressed identity changes, truncation, concatenation and trailing bytes" $ do
    (m,parts)<-encodePayload maxOriginalBytes "original\r\n\NULbytes"
    reject $ decodePayload maxOriginalBytes (m{partCount=m.partCount,rawSha256=payloadSha256 "wrong"}) parts
    reject $ decodePayload maxOriginalBytes (m{partCount=m.partCount,rawBytes=m.rawBytes-1}) parts
    reject $ decodePayload maxOriginalBytes m [BS.init(BS.concat parts)]
    forM_ [BS.concat parts<>"tail",BS.concat parts<>BS.concat parts] $ \malformed ->
      reject $ decodePayload maxOriginalBytes (m{compressedBytes=fromIntegral(BS.length malformed),compressedSha256=payloadSha256 malformed,partCount=1}) [malformed]
  it "bounds expanded work before decompression and rejects oversized parts" $ do
    (m,parts)<-encodePayload maxOriginalBytes (BS.replicate 2000000 0)
    reject $ decodePayload 1024 m parts
    reject $ decodePayload maxOriginalBytes m [BS.replicate 1048577 0]
    reject $ encodePayload 10 "elevenbytes!"
  it "accepts only the original allowlist with matching extension and signature" $ do
    let input bytes name mime=OriginalManifest (fromIntegral(BS.length bytes)) (payloadSha256 bytes) name name mime
    validateOriginal (input "<html>static</html>" "layout.html" "text/html") "<html>static</html>"
    validateOriginal (input "text\r\n" "notes.md" "text/markdown") "text\r\n"
    reject $ validateOriginal (input "MZbad" "image.png" "image/png") "MZbad"
    reject $ validateOriginal (input "text" "script.exe" "text/plain") "text"
    reject $ validateOriginal (input "text" "../notes.txt" "text/plain") "text"
    reject $ validateOriginal (input "text" "notes.txt" "application/octet-stream") "text"

reject :: IO a -> IO ()
reject action = do result<-try (action>>pure()) :: IO(Either SomeException ()); result `shouldSatisfy` either (const True) (const False)
