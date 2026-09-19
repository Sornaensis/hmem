module HMem.Embedding.HttpProtocol
  ( HttpMethod (..)
  , HttpCall (..)
  , HttpReply (..)
  , TransportCode (..)
  , WireReply (..)
  , requestFrameCap
  , responseFrameCap
  , responseBodyCap
  , encodeCall
  , decodeCall
  , encodeWireReply
  , decodeWireReply
  , frameHeader
  , parseFrameHeader
  , retryableStatus
  ) where

import Data.Bits (shiftL, shiftR, (.|.))
import qualified Data.ByteString as BS
import Data.Word (Word8)

data HttpMethod = HttpGet | HttpPost deriving (Eq, Show)

data HttpCall = HttpCall
  { callMethod :: !HttpMethod
  , callUrl :: !BS.ByteString
  , callBody :: !BS.ByteString
  , callResponseCap :: !Int
  } deriving (Eq, Show)

data HttpReply = HttpReply
  { replyStatus :: !Int
  , replyBody :: !BS.ByteString
  } deriving (Eq, Show)

data TransportCode
  = InvalidRequest
  | NetworkFailure
  | InvalidResponse
  | BodyTooLarge
  | InvalidFrame
  | CertificateRejected
  deriving (Eq, Enum, Bounded, Show)

data WireReply = WireHttp !HttpReply | WireFailure !TransportCode
  deriving (Eq, Show)

requestFrameCap, responseFrameCap, responseBodyCap :: Int
requestFrameCap = 1024 * 1024
responseFrameCap = 2 * 1024 * 1024
responseBodyCap = 1024 * 1024

word16 :: Int -> BS.ByteString
word16 n = BS.pack [fromIntegral (n `shiftR` 8), fromIntegral n]

word32 :: Int -> BS.ByteString
word32 n = BS.pack [ fromIntegral (n `shiftR` 24)
                   , fromIntegral (n `shiftR` 16)
                   , fromIntegral (n `shiftR` 8)
                   , fromIntegral n
                   ]

read16 :: BS.ByteString -> Int
read16 b = fromIntegral (BS.index b 0) `shiftL` 8 .|. fromIntegral (BS.index b 1)

read32 :: BS.ByteString -> Int
read32 b = foldl (\n octet -> n `shiftL` 8 .|. fromIntegral octet) 0 (BS.unpack b)

frameHeader :: Int -> Maybe BS.ByteString
frameHeader n
  | n < 1 || n > responseFrameCap = Nothing
  | otherwise = Just (word32 n)

parseFrameHeader :: Int -> BS.ByteString -> Maybe Int
parseFrameHeader cap header
  | BS.length header /= 4 = Nothing
  | n < 1 || n > cap = Nothing
  | otherwise = Just n
  where n = read32 header

encodeCall :: HttpCall -> Maybe BS.ByteString
encodeCall call
  | BS.null url || BS.length url > 8192 = Nothing
  | BS.length body > 512 * 1024 = Nothing
  | callResponseCap call < 1 || callResponseCap call > responseBodyCap = Nothing
  | method == HttpGet && not (BS.null body) = Nothing
  | BS.length payload > requestFrameCap = Nothing
  | otherwise = Just payload
  where
    method = callMethod call
    url = callUrl call
    body = callBody call
    payload = BS.concat
      [ BS.pack [1, if method == HttpGet then 0 else 1]
      , word32 (BS.length url), word32 (BS.length body)
      , word32 (callResponseCap call), url, body
      ]

decodeCall :: BS.ByteString -> Either TransportCode HttpCall
decodeCall bytes
  | BS.length bytes < 14 || BS.length bytes > requestFrameCap = Left InvalidFrame
  | BS.index bytes 0 /= 1 = Left InvalidFrame
  | methodByte /= 0 && methodByte /= 1 = Left InvalidFrame
  | urlLength < 1 || urlLength > 8192 || bodyLength > 512 * 1024 = Left InvalidFrame
  | responseCap < 1 || responseCap > responseBodyCap = Left InvalidFrame
  | 14 + urlLength + bodyLength /= BS.length bytes = Left InvalidFrame
  | methodByte == 0 && bodyLength /= 0 = Left InvalidFrame
  | otherwise = Right $ HttpCall
      (if methodByte == 0 then HttpGet else HttpPost)
      (BS.take urlLength (BS.drop 14 bytes))
      (BS.drop (14 + urlLength) bytes)
      responseCap
  where
    methodByte = BS.index bytes 1
    urlLength = read32 (BS.take 4 (BS.drop 2 bytes))
    bodyLength = read32 (BS.take 4 (BS.drop 6 bytes))
    responseCap = read32 (BS.take 4 (BS.drop 10 bytes))

encodeWireReply :: WireReply -> BS.ByteString
encodeWireReply (WireFailure code) = BS.pack [1, 1, fromIntegral (fromEnum code)]
encodeWireReply (WireHttp reply) = BS.concat
  [ BS.pack [1, 0]
  , word16 (replyStatus reply)
  , word32 (BS.length body)
  , body
  ]
  where body = replyBody reply

decodeWireReply :: BS.ByteString -> Either TransportCode WireReply
decodeWireReply bytes
  | BS.length bytes < 3 || BS.length bytes > responseFrameCap = Left InvalidFrame
  | BS.index bytes 0 /= 1 = Left InvalidFrame
  | BS.index bytes 1 == 1 && BS.length bytes == 3 =
      case toCode (BS.index bytes 2) of
        Nothing -> Left InvalidFrame
        Just code -> Right (WireFailure code)
  | BS.index bytes 1 == 0 && BS.length bytes >= 8 =
      let status = read16 (BS.take 2 (BS.drop 2 bytes))
          bodyLength = read32 (BS.take 4 (BS.drop 4 bytes))
      in if status < 100 || status > 599 || bodyLength > responseBodyCap ||
            BS.length bytes /= 8 + bodyLength
         then Left InvalidFrame
         else Right (WireHttp (HttpReply status (BS.drop 8 bytes)))
  | otherwise = Left InvalidFrame

toCode :: Word8 -> Maybe TransportCode
toCode n
  | fromIntegral n <= fromEnum (maxBound :: TransportCode) =
      Just (toEnum (fromIntegral n))
  | otherwise = Nothing

retryableStatus :: Int -> Bool
retryableStatus status = status == 408 || status == 429 || status >= 500 && status <= 599
