module HMem.MCP.SessionHTTP
  ( CapturedHTTP, HTTPFault(..), captureHTTP, followerRequest, followerHTTPDeadlineMicros, runSessionHTTPHelper ) where

import Control.Exception
import Control.Monad (unless, when)
import Data.Aeson
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types (parseEither)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.CaseInsensitive qualified as CI
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import HMem.BotIdentity (BotIdentity)
import HMem.MCP.SessionProcess
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (statusCode)
import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import System.IO
import System.Timeout (timeout)

-- Private requests, credentials and helper paths have no Show instance.
data CapturedHTTP = CapturedHTTP !ProcessScope !FilePath !String ![(Text,Text)]
data HTTPFault = HTTPStatus !Int | HTTPTaskRejected !Int !Text !(Maybe Text) | HTTPUnavailable | HTTPInvalid | HTTPTooLarge deriving (Show,Eq)

captureHTTP :: ProcessScope -> FilePath -> String -> Maybe Text -> BotIdentity -> IO CapturedHTTP
captureHTTP scope helper base credential identity = do
  provenance <- lookupEnv "HMEM_MCP_PROVENANCE_TOKEN"
  pure $ CapturedHTTP scope helper base $
    [("Content-Type","application/json"),("X-HMem-Bot-Identity",TE.decodeUtf8 (BL.toStrict (encode identity))),("X-HMem-Change-Cause","mcp")]
    <> maybe [] (\token -> [("Authorization","Bearer " <> token)]) credential
    <> maybe [] (\value -> if T.null (T.strip (T.pack value)) then [] else [("X-HMem-MCP-Provenance",T.strip (T.pack value))]) provenance

followerHTTPDeadlineMicros :: Int
followerHTTPDeadlineMicros = 5000000

-- The global follower slot includes child death/reap and all pipe-owner joins.
followerRequest :: CapturedHTTP -> ByteString -> String -> Maybe Value -> IO (Either HTTPFault Value)
followerRequest (CapturedHTTP scope helper base headers) verb path body = do
  let payload = BL.toStrict (encode (object ["url" .= (base <> path),"method" .= TE.decodeUtf8 verb,"headers" .= headers,"body" .= body]))
  response <- runHelper scope helper (const (pure ())) (const (pure ())) payload
  pure $ case response of
    Left _ -> Left HTTPUnavailable
    Right bytes -> case BS.uncons bytes of
      Just (0,rest) | BS.length rest >= 2 ->
        let status = fromIntegral (BS.index rest 0)*256 + fromIntegral (BS.index rest 1)
            content = BS.drop 2 rest
        in if BS.length content > 2097152 then Left HTTPTooLarge
           else if status < 200 || status >= 300 then Left (taskRejection status content)
           else if BS.null content then Right Null
           else either (const (Left HTTPInvalid)) Right (eitherDecodeStrict' content)
      Just (3,rest) | BS.null rest -> Left HTTPTooLarge
      Just (1,rest) | BS.null rest -> Left HTTPUnavailable
      _ -> Left HTTPInvalid

-- Preserve only fixed, task-specific server refusals. Never retain arbitrary
-- response bodies, parser text or caller-controlled values in follower status.
taskRejection :: Int -> ByteString -> HTTPFault
taskRejection status content = case eitherDecodeStrict' content of
  Right (Object fields)
    | KM.lookup "error" fields == Just (String "session_task_not_found") && status == 404 ->
        HTTPTaskRejected status "session_task_not_found" Nothing
    | KM.lookup "error" fields == Just (String "session_task_not_in_progress") && status == 409 ->
        let current = case KM.lookup "current_status" fields of
              Just (String value) | value `elem` ["todo","blocked","done","cancelled"] -> Just value
              _ -> Nothing
        in HTTPTaskRejected status "session_task_not_in_progress" current
  _ -> HTTPStatus status

-- Fixed hidden mode: before normal configuration, logging or MCP initialization.
-- Replies carry raw bounded HTTP bytes, avoiding Aeson re-encoding expansion.
runSessionHTTPHelper :: IO ()
runSessionHTTPHelper = do
  hSetBinaryMode stdin True; hSetBinaryMode stdout True; hSetBuffering stdout NoBuffering
  owned <- watchParent
  unless (owned == 1) exitFailure
  writeFrame "HMSESSION1"
  outcome <- try @SomeException $ do
    input <- readFrame
    value <- either (const $ throwIO ProcessProtocol) pure (eitherDecodeStrict' input)
    request <- either (const $ throwIO ProcessProtocol) pure (parseEither parseCall value)
    perform request
  writeFrame (either (const (BS.singleton 2)) id outcome)
  where
    parseCall = withObject "helper call" $ \fields -> do
      url <- fields .: "url"; verb <- fields .: "method"; headers <- fields .: "headers"; body <- fields .:? "body"; probe <- fields .:? "probe"
      pure (url,verb,headers,body,probe)
    perform (url,verb,headers,body,probe) = do
      -- Internal native blocking seam, never accepted by uploader tools or HTTP.
      case probe :: Maybe Text of
        Just phase | phase `elem` ["dns","connect","tls","body"] && url == "http://127.0.0.1:0/" -> do
          writeFrame ("PHASE:" <> TE.encodeUtf8 phase)
          blockChildProbe
        Just _ -> throwIO ProcessProtocol
        Nothing -> pure ()
      initial <- parseRequest url
      let request = initial {method=TE.encodeUtf8 verb, requestHeaders=[(CI.mk (TE.encodeUtf8 name),TE.encodeUtf8 value) | (name,value) <- headers]
            , requestBody=maybe (RequestBodyBS mempty) (RequestBodyLBS . encode) (body :: Maybe Value)
            , responseTimeout=responseTimeoutMicro followerHTTPDeadlineMicros}
      manager <- newManager tlsManagerSettings {managerIdleConnectionCount=0}
      response <- timeout followerHTTPDeadlineMicros $ withResponse request manager $ \reply -> do
        let status = statusCode (responseStatus reply)
        bytes <- readBody (responseBody reply) 2097152 []
        pure $ case bytes of
          Nothing -> BS.singleton 3
          Just content -> BS.pack [0,fromIntegral (status `div` 256),fromIntegral status] <> BL.toStrict content
      pure $ maybe (BS.singleton 1) id response
    readBody reader left pieces = do
      chunk <- brReadSome reader (min 32768 (left+1))
      if BL.null chunk then pure (Just (BL.concat (reverse pieces)))
      else if BL.length chunk > fromIntegral left then pure Nothing
      else readBody reader (left-fromIntegral (BL.length chunk)) (chunk:pieces)

readFrame :: IO ByteString
readFrame = do
  header <- readExactly 4
  count <- maybe (throwIO ProcessProtocol) pure (parseFrameHeader header)
  readExactly count
  where
    readExactly count = do
      bytes <- BS.hGet stdin count
      when (BS.length bytes /= count) $ throwIO ProcessProtocol
      pure bytes
writeFrame :: ByteString -> IO ()
writeFrame bytes = do
  when (BS.length bytes > frameCap) exitFailure
  BS.hPut stdout (frameHeader (BS.length bytes) <> bytes)
  hFlush stdout
