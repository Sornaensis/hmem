-- Private, finite binary HTTP exchanges. Domain authority remains in the caller.
module HMem.MCP.AttachmentHTTP
  ( AttachmentScope(..), Deadline(..), TransportFault(..)
  , newAttachmentScope, closeAttachmentScope, attachmentScopeReady
  , newDeadline, exchange, runAttachmentHTTPHelper
  ) where

import Control.Exception hiding (handle)
import Control.Monad (unless, when, void, forM_)
import Control.Concurrent.STM (STM)
import Crypto.Hash (Context, Digest, SHA256, hashInit, hashUpdate, hashFinalize)
import Data.Aeson
import Data.Aeson.Types (Parser, parseEither)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as B8
import Data.ByteString.Lazy qualified as BL
import Data.CaseInsensitive qualified as CI
import Data.Int (Int64)
import Data.IORef
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import Network.HTTP.Client qualified as H
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (ResponseHeaders, statusCode)
import System.Directory (canonicalizePath, doesFileExist)
import System.Exit (exitFailure)
import System.FilePath (isAbsolute)
import System.IO
import HMem.MCP.SessionProcess qualified as P

data TransportFault = TransportUnavailable | TransportDeadline | TransportProtocol | TransportOwnership
  deriving (Eq, Show)
instance Exception TransportFault

data AttachmentScope = AttachmentScope
  { processes :: !P.ProcessScope, executable :: !FilePath }
data Deadline = Deadline
  { workUntil :: !Word64, cleanupUntil :: !Word64, cleanupMicros :: !Int }

newAttachmentScope :: FilePath -> IO AttachmentScope
newAttachmentScope executable = do
  exists <- if isAbsolute executable then doesFileExist executable else pure False
  canonical <- if exists then canonicalizePath executable else pure ""
  unless (exists && canonical == executable) (throwIO TransportUnavailable)
  AttachmentScope <$> P.newProcessScope <*> pure executable

closeAttachmentScope :: AttachmentScope -> Int -> IO Bool
closeAttachmentScope scope = P.closeProcessScope scope.processes

attachmentScopeReady :: AttachmentScope -> STM Bool
attachmentScopeReady scope = P.attachmentContextReady scope.processes

newDeadline :: Int -> Int -> IO Deadline
newDeadline total reserve = do
  now <- getMonotonicTimeNSec
  pure (Deadline (now + fromIntegral (total-reserve)*1000)
    (now + fromIntegral total*1000) reserve)

check :: Bool -> IO ()
check valid = unless valid (throwIO TransportProtocol)

digest :: [BS.ByteString] -> Text
digest chunks = T.pack (show (hashFinalize (foldl hashUpdate (hashInit :: Context SHA256) chunks) :: Digest SHA256))
finishedHash :: Context SHA256 -> Text
finishedHash context = T.pack (show (hashFinalize context :: Digest SHA256))

controlFrame :: Word -> Value -> IO BS.ByteString
controlFrame tag value = do
  let bytes = BL.toStrict (encode value)
  check (BS.length bytes <= 262144)
  pure (BS.cons (fromIntegral tag) bytes)
decodeControl :: Word -> BS.ByteString -> IO Value
decodeControl tag frame = do
  check (BS.length frame <= 262145)
  case BS.uncons frame of
    Just (actual,bytes) | fromIntegral actual == tag ->
      either (const (throwIO TransportProtocol)) pure (eitherDecodeStrict' bytes)
    _ -> throwIO TransportProtocol
parseValue :: (Value -> Parser a) -> Value -> IO a
parseValue parser value = either (const (throwIO TransportProtocol)) pure (parseEither parser value)

headerValues :: ResponseHeaders -> [(Text,Text)]
headerValues = map (\(key,value) -> (TE.decodeLatin1 (CI.original key),TE.decodeLatin1 value))
decodeHeaders :: [(Text,Text)] -> IO ResponseHeaders
decodeHeaders pairs = do
  check (length pairs <= 128 && all (\(key,value) -> T.all (<= '\255') key && T.all (<= '\255') value) pairs)
  pure [(CI.mk (B8.pack (T.unpack key)),B8.pack (T.unpack value)) | (key,value) <- pairs]

-- The IO pumps own only bytes and native pipes. Header/body/completion callbacks
-- run in the admitted caller, keeping its file handles out of retained pumps.
exchange :: AttachmentScope -> Deadline -> H.Request -> [BS.ByteString] -> Int64
  -> (Int -> ResponseHeaders -> IO ()) -> (BS.ByteString -> IO ()) -> IO () -> IO ()
exchange scope deadline request body maximum onHeaders onChunk onComplete = do
  now <- getMonotonicTimeNSec
  let cutoff = min deadline.workUntil (now + 30000000000)
      retirement = min deadline.cleanupUntil (cutoff + fromIntegral deadline.cleanupMicros*1000)
      bytes = sum (map (toInteger . BS.length) body)
  check (bytes <= 25*1048576 && maximum >= 0 && maximum <= 100*1048576)
  when (now >= cutoff) (throwIO TransportDeadline)
  control <- controlFrame 0 (object ["version" .= (1::Int),"url" .= show (H.getUri request),
    "method" .= TE.decodeLatin1 (H.method request),"headers" .= headerValues (H.requestHeaders request),
    "body_bytes" .= bytes,"body_sha256" .= digest body,"maximum_response_bytes" .= maximum,"cutoff_ns" .= cutoff])
  ending <- controlFrame 2 (object ["bytes" .= bytes,"sha256" .= digest body])
  let input :: P.AttachmentPipes -> IO ()
      input channel = do
        channel.sendAttachmentFrame control
        forM_ body $ \part -> forM_ (split part) $ \chunk ->
          channel.sendAttachmentFrame (BS.cons 1 chunk)
        channel.sendAttachmentFrame ending
      output :: P.AttachmentPipes -> IO BS.ByteString
      output channel = do
        first <- channel.receiveAttachmentFrame
        case BS.uncons first of
          Just (6,_) -> helperFailure first
          _ -> pure ()
        metadata <- decodeControl 3 first
        (status,headersValue) <- parseValue (withObject "HTTP response" $ \fields ->
          (,) <$> fields .: "status" <*> fields .: "headers") metadata
        check (status >= (100::Int) && status <= 599)
        headers <- decodeHeaders headersValue
        onHeaders status headers
        let limit = if status >= 200 && status < 300 then maximum else 8192
            receive count context = do
              frame <- channel.receiveAttachmentFrame
              case BS.uncons frame of
                Just (4,chunk) -> do
                  check (not (BS.null chunk) && BS.length chunk <= 1048576)
                  let next = count + fromIntegral (BS.length chunk)
                  check (next <= limit)
                  onChunk chunk
                  receive next (hashUpdate context chunk)
                Just (5,_) -> do
                  terminal <- decodeControl 5 frame
                  (actual,hash) <- parseValue (withObject "HTTP terminal" $ \fields ->
                    (,) <$> fields .: "bytes" <*> fields .: "sha256") terminal
                  check (actual == count && hash == finishedHash context)
                  onComplete
                  channel.expectAttachmentEOF
                Just (6,_) -> helperFailure frame
                _ -> throwIO TransportProtocol
        receive 0 (hashInit :: Context SHA256)
        pure BS.empty
  outcome <- P.runAttachmentExchange scope.processes scope.executable cutoff retirement (const (pure ())) input output
    `catch` \(fault :: P.ProcessFault) -> throwIO (mapFault fault)
  either (throwIO . mapFault) (const (pure ())) outcome

mapFault :: P.ProcessFault -> TransportFault
mapFault P.ProcessDeadline = TransportDeadline
mapFault P.ProcessOwnership = TransportOwnership
mapFault P.ProcessUnavailable = TransportUnavailable
mapFault _ = TransportProtocol

helperFailure :: BS.ByteString -> IO a
helperFailure frame = do
  value <- decodeControl 6 frame
  code <- parseValue (withObject "helper failure" (.: "code")) value
  throwIO (if code == ("deadline" :: Text) then TransportDeadline
    else if code == "protocol" then TransportProtocol else TransportUnavailable)

split :: BS.ByteString -> [BS.ByteString]
split bytes | BS.null bytes = []
            | otherwise = let (part,rest) = BS.splitAt 1048576 bytes in part:split rest

-- Hidden fixed mode, dispatched before configuration, logging or MCP startup.
runAttachmentHTTPHelper :: IO ()
runAttachmentHTTPHelper = do
  hSetBinaryMode stdin True
  hSetBinaryMode stdout True
  hSetBuffering stdout NoBuffering
  watched <- P.watchParent
  unless (watched == 1) exitFailure
  outcome <- try @SomeException $ do
    writeFrame "HMATTACH1"
    control <- readFrame 262145 >>= decodeControl 0
    (version,url,verb,rawHeaders,bodyBytes,bodyHash,maximum,cutoff) <- parseValue
      (withObject "HTTP request" $ \fields -> (,,,,,,,)
        <$> fields .: "version" <*> fields .: "url" <*> fields .: "method" <*> fields .: "headers"
        <*> fields .: "body_bytes" <*> fields .: "body_sha256" <*> fields .: "maximum_response_bytes" <*> fields .: "cutoff_ns") control
    check (version == (1::Int) && bodyBytes >= (0::Int64) && bodyBytes <= 25*1048576
      && maximum >= (0::Int64) && maximum <= 100*1048576
      && elem (verb :: Text) ["GET","HEAD","POST","PUT","PATCH","DELETE"])
    headers <- decodeHeaders rawHeaders
    initial <- H.parseRequest url
    cache <- newIORef ([] :: [BS.ByteString])
    inputDone <- newIORef False
    inputCount <- newIORef (0::Int64)
    inputHash <- newIORef (hashInit :: Context SHA256)
    let deadlineCheck = do
          time <- getMonotonicTimeNSec
          when (time >= (cutoff :: Word64)) (throwIO TransportDeadline)
        next = do
          deadlineCheck
          frame <- readFrame 1048577
          case BS.uncons frame of
            Just (1,chunk) -> do
              check (not (BS.null chunk) && BS.length chunk <= 1048576)
              count <- readIORef inputCount
              let total = count + fromIntegral (BS.length chunk)
              check (total <= bodyBytes)
              writeIORef inputCount total
              modifyIORef' inputHash (`hashUpdate` chunk)
              modifyIORef' cache (<> [chunk])
              pure chunk
            Just (2,_) -> do
              ending <- decodeControl 2 frame
              (count,hash) <- parseValue (withObject "request terminal" $ \fields ->
                (,) <$> fields .: "bytes" <*> fields .: "sha256") ending
              actual <- readIORef inputCount
              actualHash <- finishedHash <$> readIORef inputHash
              check (count == bodyBytes && actual == bodyBytes && hash == bodyHash && actualHash == bodyHash)
              extra <- BS.hGet stdin 1
              check (BS.null extra)
              writeIORef inputDone True
              pure BS.empty
            _ -> throwIO TransportProtocol
        body = H.RequestBodyStream bodyBytes $ \consume -> do
          position <- newIORef 0
          consume $ do
            index <- readIORef position
            cached <- readIORef cache
            done <- readIORef inputDone
            chunk <- if index < length cached then pure (cached !! index)
              else if done then pure BS.empty else next
            unless (BS.null chunk) (modifyIORef' position (+1))
            pure chunk
        request = initial {H.method=TE.encodeUtf8 verb,H.requestHeaders=
          (if bodyBytes > 0 then [("Expect","100-continue")] else []) <> filter ((/="Expect") . fst) headers,
          H.requestBody=body,H.redirectCount=0,H.decompress=const False,H.cookieJar=Nothing,
          H.checkResponse= \_ _ -> pure (),H.responseTimeout=H.responseTimeoutMicro 30000000}
    deadlineCheck
    manager <- H.newManager tlsManagerSettings {H.managerIdleConnectionCount=0,H.managerConnCount=1}
    H.withResponse request manager $ \response -> do
      let status = statusCode (H.responseStatus response)
      when (status >= 200 && status < 300) $ do
        -- Exact idempotent retries may receive a final success before the
        -- service consumes a duplicate body. Still verify the entire private
        -- request stream and terminal binding before trusting that response.
        let finishInput = do
              done <- readIORef inputDone
              unless done (next >> finishInput)
        finishInput
      controlFrame 3 (object ["status" .= status,"headers" .= headerValues (H.responseHeaders response)]) >>= writeFrame
      let limit = if status >= 200 && status < 300 then maximum else 8192
          copy count context = do
            deadlineCheck
            chunk <- H.brRead (H.responseBody response)
            let total = count + fromIntegral (BS.length chunk)
            check (total <= limit)
            if BS.null chunk then controlFrame 5 (object ["bytes" .= total,"sha256" .= finishedHash context]) >>= writeFrame
            else do
              mapM_ (writeFrame . BS.cons 4) (split chunk)
              copy total (hashUpdate context chunk)
      copy 0 (hashInit :: Context SHA256)
  case outcome of
    Right () -> pure ()
    Left exception -> do
      let code = case fromException exception of
            Just TransportDeadline -> "deadline" :: Text
            Just TransportProtocol -> "protocol"
            _ -> "unavailable"
      void (try @SomeException (controlFrame 6 (object ["code" .= code]) >>= writeFrame))
      exitFailure

readFrame :: Int -> IO BS.ByteString
readFrame cap = do
  header <- exact 4
  size <- maybe (throwIO TransportProtocol) pure (P.parseFrameHeader header)
  check (size > 0 && size <= cap)
  exact size
  where
    exact count = do
      bytes <- BS.hGet stdin count
      check (BS.length bytes == count)
      pure bytes
writeFrame :: BS.ByteString -> IO ()
writeFrame frame = do
  check (not (BS.null frame) && BS.length frame <= 1048577)
  BS.hPut stdout (P.frameHeader (BS.length frame) <> frame)
  hFlush stdout
