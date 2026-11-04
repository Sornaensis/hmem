-- | Bounded, single-member gzip payloads. No session storage or seal semantics.
module HMem.Document.Codec
  ( encodePayload, decodePayload, payloadSha256, validateManifest, validateOriginal, chunks ) where

import Control.Exception (throwIO)
import Control.Monad (unless)
import Crypto.Hash (Digest, SHA256, hash)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Char (isControl)
import Data.IORef
import Data.Int (Int64)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import HMem.Document.Types
import HMem.SessionArchive.Codec qualified as Gzip

payloadSha256 :: ByteString -> T.Text
payloadSha256 bytes = T.pack (show (hash bytes :: Digest SHA256))

chunks :: ByteString -> [ByteString]
chunks bytes | BS.null bytes = []
             | otherwise = let (a,b) = BS.splitAt (fromIntegral maxPartBytes) bytes in a : chunks b

require :: Bool -> T.Text -> IO ()
require ok why = unless ok (throwIO (DocumentException why))

source :: [ByteString] -> IO (IO (Maybe ByteString))
source parts = do
  ref <- newIORef parts
  pure $ atomicModifyIORef' ref $ \case [] -> ([],Nothing); a:b -> (b,Just a)

encodePayload :: Int64 -> ByteString -> IO (PayloadManifest, [ByteString])
encodePayload limit bytes = do
  require (limit >= 0 && fromIntegral (BS.length bytes) <= limit && limit <= maxDerivativeBytes) "document_size_exceeded"
  input <- source (chunks bytes)
  output <- newIORef []
  result <- Gzip.compress input (\piece -> modifyIORef' output (piece:))
  compressed <- BS.concat . reverse <$> readIORef output
  require (fromIntegral (BS.length compressed) <= uploadReservationBytes limit) "document_compressed_size_exceeded"
  let parts = chunks compressed
      manifest = PayloadManifest "gzip-v1" result.raw.byteCount result.raw.sha256 result.compressed.byteCount result.compressed.sha256 (length parts)
  -- Verify both identities and framing before persistence.
  decoded <- decodePayload limit manifest parts
  require (decoded == bytes) "document_codec_roundtrip"
  pure (manifest,parts)

decodePayload :: Int64 -> PayloadManifest -> [ByteString] -> IO ByteString
decodePayload limit manifest parts = do
  require (manifest.codec == "gzip-v1" && manifest.rawBytes >= 0 && manifest.rawBytes <= limit && limit <= maxDerivativeBytes) "document_manifest_bound"
  require (length parts == manifest.partCount && all (\p -> not (BS.null p) && fromIntegral (BS.length p) <= maxPartBytes) parts) "document_part_bound"
  let compressed = BS.concat parts
  require (fromIntegral (BS.length compressed) == manifest.compressedBytes && payloadSha256 compressed == manifest.compressedSha256 && manifest.compressedBytes <= uploadReservationBytes limit) "document_compressed_manifest_mismatch"
  input <- source parts
  output <- newIORef []
  result <- Gzip.verify manifest.rawBytes input (\piece -> modifyIORef' output (piece:)) (const (pure ()))
  require (result.raw.sha256 == manifest.rawSha256 && result.compressed.sha256 == manifest.compressedSha256) "document_raw_manifest_mismatch"
  BS.concat . reverse <$> readIORef output

-- Content checks select a durable original type; they do not make an original
-- safe to embed. Office/design archives are never extracted here.
validateOriginal :: OriginalManifest -> ByteString -> IO ()
validateOriginal m bytes = do
  validateManifest m
  require (m.rawBytes >= 0 && m.rawBytes <= maxOriginalBytes && fromIntegral (BS.length bytes) == m.rawBytes && payloadSha256 bytes == m.rawSha256) "document_original_manifest_mismatch"
  let name = T.toLower m.filename
      ext suffix = suffix `T.isSuffixOf` name
      prefix = (`BS.isPrefixOf` bytes)
      textual = case TE.decodeUtf8' bytes of Right _ -> not (BS.elem 0 bytes); Left _ -> False
      accepted = case m.mediaType of
        "image/png" -> ext ".png" && prefix (BS.pack [137,80,78,71,13,10,26,10])
        "image/jpeg" -> (ext ".jpg" || ext ".jpeg") && prefix (BS.pack [255,216,255])
        "image/webp" -> ext ".webp" && prefix "RIFF" && BS.take 4 (BS.drop 8 bytes) == "WEBP"
        "video/mp4" -> ext ".mp4" && hasMp4FileType bytes
        "application/pdf" -> ext ".pdf" && prefix "%PDF-"
        "text/html" -> (ext ".html" || ext ".htm") && textual
        "image/svg+xml" -> ext ".svg" && textual
        "text/plain" -> ext ".txt" && textual
        "text/markdown" -> (ext ".md" || ext ".markdown") && textual
        "application/vnd.openxmlformats-officedocument.wordprocessingml.document" -> ext ".docx" && prefix "PK\ETX\EOT"
        "application/vnd.openxmlformats-officedocument.presentationml.presentation" -> ext ".pptx" && prefix "PK\ETX\EOT"
        "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet" -> ext ".xlsx" && prefix "PK\ETX\EOT"
        "application/x-figma" -> ext ".fig" && (prefix "fig-kiwi" || prefix "PK\ETX\EOT")
        "application/x-sketch" -> ext ".sketch" && prefix "PK\ETX\EOT"
        _ -> False
  require accepted "document_unsupported_or_mismatched_type"

validateManifest :: OriginalManifest -> IO ()
validateManifest m = do
  require (m.rawBytes>=0 && m.rawBytes<=maxOriginalBytes && T.length m.rawSha256==64 && T.all (`elem` ("0123456789abcdef"::String)) m.rawSha256) "document_invalid_manifest"
  require (validName m.filename && validText 1024 m.title) "document_invalid_name"
  let ext= snd (T.breakOnEnd "." (T.toLower m.filename))
      allowed=case m.mediaType of
        "image/png"->["png"]; "image/jpeg"->["jpg","jpeg"]; "image/webp"->["webp"]; "video/mp4"->["mp4"]; "application/pdf"->["pdf"]
        "text/html"->["html","htm"]; "image/svg+xml"->["svg"]; "text/plain"->["txt"]; "text/markdown"->["md","markdown"]
        "application/vnd.openxmlformats-officedocument.wordprocessingml.document"->["docx"]
        "application/vnd.openxmlformats-officedocument.presentationml.presentation"->["pptx"]
        "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"->["xlsx"]
        "application/x-figma"->["fig"]; "application/x-sketch"->["sketch"]; _->[]
  require (ext `elem` allowed) "document_unsupported_or_mismatched_type"
  where
    validText n value = not (T.null (T.strip value)) && BS.length (TE.encodeUtf8 value) <= n && not (T.any isControl value)
    validName value = validText 255 value && value /= "." && value /= ".." && not (T.any (`elem` ("/\\" :: String)) value)

-- Restrict the original signature to a bounded leading ISO MP4 file-type box.
-- The confined decoder verifies the tracks and codec before making derivatives.
hasMp4FileType :: ByteString -> Bool
hasMp4FileType bytes =
  BS.take 4 (BS.drop 4 bytes) == "ftyp"
  && boxBytes >= 16 && boxBytes <= min 4096 (BS.length bytes)
  && boxBytes `mod` 4 == 0
  && BS.take 4 (BS.drop 8 bytes) `elem` ["isom", "iso2", "mp41", "mp42", "avc1"]
  where
    boxBytes = BS.foldl' (\size byte -> size * 256 + fromIntegral byte) 0 (BS.take 4 bytes)
