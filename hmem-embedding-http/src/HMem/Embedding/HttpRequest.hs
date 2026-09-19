{-# LANGUAGE OverloadedStrings #-}
module HMem.Embedding.HttpRequest
  ( performHttpCall
  ) where

import Control.Exception (SomeException, fromException, try)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Data.Char (ord)
import Network.HTTP.Client
  ( BodyReader, HttpException (..), HttpExceptionContent (..)
  , RequestBody (RequestBodyBS), brRead, checkResponse
  , defaultProxy, managerSetMaxHeaderLength, managerSetMaxNumberHeaders
  , managerSetProxy, method, newManager, parseRequest, redirectCount
  , requestBody, requestHeaders, responseBody, responseHeaders, responseStatus
  , responseTimeout, responseTimeoutNone, withResponse
  )
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Header (HeaderName, hAccept, hContentLength, hContentType)
import Network.HTTP.Types.Status (statusCode)
import Network.TLS (TLSException (..), TLSError (..), fromAlertDescription)

import HMem.Embedding.HttpProtocol
  ( HttpCall (..), HttpMethod (..), HttpReply (..), TransportCode (..)
  , WireReply (..)
  )

performHttpCall :: HttpCall -> IO WireReply
performHttpCall call
  | not ("http://" `BS.isPrefixOf` url || "https://" `BS.isPrefixOf` url) =
      pure (WireFailure InvalidRequest)
  | BS.any (\b -> b < 33 || b > 126) url = pure (WireFailure InvalidRequest)
  | otherwise = do
      result <- try execute :: IO (Either SomeException WireReply)
      pure (either (WireFailure . classifyFailure) id result)
  where
    url = callUrl call
    execute = do
      let settings = managerSetMaxNumberHeaders 64
                   $ managerSetMaxHeaderLength (32 * 1024)
                   $ managerSetProxy defaultProxy tlsManagerSettings
      manager <- newManager settings
      parsed <- parseRequest (C8.unpack url)
      let request = parsed
            { method = if callMethod call == HttpGet then "GET" else "POST"
            , requestBody = RequestBodyBS (callBody call)
            , requestHeaders = filter (\(name, _) -> name /= hAccept && name /= hContentType)
                                   (requestHeaders parsed) ++ [(hAccept, "application/json")]
                ++ if callMethod call == HttpPost
                   then [(hContentType, "application/json")]
                   else []
            , redirectCount = 0
            , responseTimeout = responseTimeoutNone
            , checkResponse = \_ _ -> pure ()
            }
      withResponse request manager $ \response -> do
        let status = statusCode (responseStatus response)
        if status < 200 || status >= 300
          then pure (WireHttp (HttpReply status BS.empty))
          else case contentLength (callResponseCap call) (responseHeaders response) of
            Left code -> pure (WireFailure code)
            Right () -> do
              bodyResult <- readBounded (callResponseCap call) (responseBody response)
              pure $ case bodyResult of
                Left code -> WireFailure code
                Right body -> WireHttp (HttpReply status body)

-- Keep exception details inside the helper. A caller sees only a finite code;
-- certificate failure remains retryable under its original parent deadline.
classifyFailure :: SomeException -> TransportCode
classifyFailure exception =
  case fromException exception of
    Just (HttpExceptionRequest _ (InternalException nested)) ->
      classifyTls nested
    _ -> classifyTls exception
  where
    classifyTls nested = case fromException nested of
      Just (HandshakeFailed problem) | certificateError problem -> CertificateRejected
      Just (Terminated _ _ problem) | certificateError problem -> CertificateRejected
      Just (Uncontextualized problem) | certificateError problem -> CertificateRejected
      _ -> NetworkFailure

    certificateError (Error_Certificate _) = True
    certificateError (Error_Protocol _ alert) =
      fromAlertDescription alert `elem` [42, 43, 44, 45, 46, 48, 49, 50, 113, 116]
    certificateError _ = False

contentLength :: Int -> [(HeaderName, BS.ByteString)] -> Either TransportCode ()
contentLength cap headers = case [value | (name, value) <- headers, name == hContentLength] of
  [] -> Right ()
  [value]
    | BS.null value || BS.any (\b -> b < fromIntegral (ord '0') ||
                                  b > fromIntegral (ord '9')) value ->
        Left InvalidResponse
    | BS.length value > 8 -> Left InvalidResponse
    | decimal value > cap -> Left BodyTooLarge
    | otherwise -> Right ()
  _ -> Left InvalidResponse
  where decimal = BS.foldl' (\n b -> n * 10 + fromIntegral (b - 48)) 0

readBounded :: Int -> BodyReader -> IO (Either TransportCode BS.ByteString)
readBounded cap reader = go 0 []
  where
    go total chunks = do
      chunk <- brRead reader
      if BS.null chunk
        then pure (Right (BS.concat (reverse chunks)))
        else let next = total + BS.length chunk in
          if next > cap
          then pure (Left BodyTooLarge)
          else go next (chunk : chunks)
