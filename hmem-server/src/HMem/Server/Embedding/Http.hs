-- | Minimal TEI-compatible HTTP adapter for the provider-neutral contract.
-- There is deliberately no auth or credential configuration in this adapter.
module HMem.Server.Embedding.Http
  ( httpEmbeddingTransport
  , embeddingRequestJson
  , decodeEmbeddingResponse
  , embeddingEndpointPath
  , normalizeEmbeddingEndpoint
  ) where

import Control.Concurrent.Async (AsyncCancelled(..))
import Control.Exception (AsyncException(..), SomeAsyncException, catch, fromException, throwIO, toException, try)
import Data.Aeson (Value, object, (.=))
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as LBS
import Data.Text (Text)
import Data.Text qualified as T
import Network.HTTP.Client
  ( Manager
  , Request(..)
  , RequestBody(..)
  , HttpException
  , BodyReader
  , brReadSome
  , parseRequest
  , requestHeaders
  , responseHeaders
  , responseTimeoutMicro
  , responseBody
  , responseStatus
  , withResponse
  )
import Network.HTTP.Types (statusCode)
import Network.HTTP.Types.Header (ResponseHeaders, hContentLength)
import System.Timeout (timeout)

import HMem.Config (EmbeddingProviderConfig(..), normalizeEmbeddingEndpointRoute)
import HMem.Types (observationEmbeddingDimensions)
import HMem.Server.Embedding.Provider

-- | The TEI embedding route is fixed.  Endpoints are normalized by appending
-- it once, so callers cannot smuggle a routing choice into the contract.
embeddingEndpointPath :: Text
embeddingEndpointPath = "/embed"

-- | The configured endpoint names a TEI service, not a route.  Preserve an
-- explicit existing /embed route and remove trailing slashes before adding
-- the fixed path so it is present exactly once.
normalizeEmbeddingEndpoint :: Text -> Either EmbeddingFailure Text
normalizeEmbeddingEndpoint endpointValue =
  maybe (Left (EmbeddingFailure ProviderConfigurationError False)) Right
    (normalizeEmbeddingEndpointRoute endpointValue)

embeddingRequestJson :: EmbeddingRequest -> Value
embeddingRequestJson request = object
  [ "inputs" .= map (.inputText) request.inputs
  ]

-- | Decode the only TEI response shape accepted by this boundary: one finite,
-- 1536-dimensional vector per submitted input.
decodeEmbeddingResponse
  :: Int
  -> LBS.ByteString
  -> Either EmbeddingFailure [[Double]]
decodeEmbeddingResponse expectedCount body = case Aeson.eitherDecode body of
  Left _ -> Left (EmbeddingFailure ProviderProtocolError False)
  Right decoded
    | length decoded /= expectedCount -> Left (EmbeddingFailure ProviderProtocolError False)
    | any invalidVector decoded -> Left (EmbeddingFailure ProviderProtocolError False)
    | otherwise -> Right decoded
  where
    invalidVector vector =
      length vector /= observationEmbeddingDimensions
        || any (\value -> isNaN value || isInfinite value) vector

-- | Build a transport for the configured mode.  It does not issue a request
-- until the returned function is called.  Caller cancellation is converted to
-- the provider contract's structured cancellation result; unrelated runtime
-- exceptions still propagate.
httpEmbeddingTransport
  :: Manager
  -> EmbeddingProviderConfig
  -> Text
  -> EmbeddingRequest
  -> IO (Either EmbeddingFailure EmbeddingBatch)
httpEmbeddingTransport manager cfg endpointValue request =
  catchCancelled $ case normalizeEmbeddingEndpoint endpointValue of
    Left failure -> pure (Left failure)
    Right routedEndpoint -> go routedEndpoint (cfg.retryAttempts + 1)
  where
    catchCancelled action = action `catch` \(exception :: SomeAsyncException) ->
      case fromException (toException exception) :: Maybe AsyncCancelled of
        Just AsyncCancelled -> pure (Left (EmbeddingFailure ProviderCancelled False))
        Nothing -> case fromException (toException exception) :: Maybe AsyncException of
          Just ThreadKilled -> pure (Left (EmbeddingFailure ProviderCancelled False))
          Just UserInterrupt -> pure (Left (EmbeddingFailure ProviderCancelled False))
          _ -> throwIO exception

    go routedEndpoint attempts = do
      result <- oneAttempt routedEndpoint
      case result of
        Left failure | failure.retryable && attempts > 1 -> go routedEndpoint (attempts - 1)
        _ -> pure result

    oneAttempt routedEndpoint = do
      parsed <- try @HttpException (parseRequest (T.unpack routedEndpoint))
      case parsed of
        Left _ -> pure (Left (EmbeddingFailure ProviderConfigurationError False))
        Right baseRequest -> do
          let httpRequest = baseRequest
                { method = "POST"
                , requestHeaders = ("Content-Type", "application/json") : baseRequest.requestHeaders
                , requestBody = RequestBodyLBS (Aeson.encode (embeddingRequestJson request))
                , redirectCount = 0
                , responseTimeout = responseTimeoutMicro (cfg.timeoutMs * 1000)
                }
          attempted <- timeout (cfg.timeoutMs * 1000) (try @HttpException (withResponse httpRequest manager $ \response ->
            if statusCode (responseStatus response) < 200 || statusCode (responseStatus response) >= 300
              then pure (Left (EmbeddingFailure ProviderUnavailable (retryableStatus (statusCode (responseStatus response)))))
              else do
                boundedBody <- readBoundedResponseBody (responseHeaders response) (responseBody response)
                pure $ boundedBody >>= decodeEmbeddingResponse (length request.inputs)))
          case attempted of
            Nothing -> pure (Left (EmbeddingFailure ProviderTimedOut True))
            Just (Left _) -> pure (Left (EmbeddingFailure ProviderUnavailable True))
            Just (Right (Left failure)) -> pure (Left failure)
            Just (Right (Right vectors)) -> pure (Right EmbeddingBatch
              { vectors = vectors
              , spaceFingerprint = cfg.spaceFingerprint
              })

    retryableStatus status = status == 408 || status == 429 || (status >= 500 && status <= 599)

-- | A maximum valid response is 256 vectors × 1536 dimensions. Forty bytes
-- per encoded number covers finite JSON number text plus separators, while
-- the reader also enforces this ceiling for chunked peers without a length.
maximumEmbeddingResponseBytes :: Int
maximumEmbeddingResponseBytes = 256 * observationEmbeddingDimensions * 40

responseReadChunkBytes :: Int
responseReadChunkBytes = 32 * 1024

readBoundedResponseBody
  :: ResponseHeaders
  -> BodyReader
  -> IO (Either EmbeddingFailure LBS.ByteString)
readBoundedResponseBody headers reader = case lookup hContentLength headers of
  Just rawLength | Just declared <- parseContentLength rawLength
                 , declared > maximumEmbeddingResponseBytes -> protocolFailure
                 | Nothing <- parseContentLength rawLength -> protocolFailure
  _ -> go 0 []
  where
    protocolFailure = pure (Left (EmbeddingFailure ProviderProtocolError False))

    go bytesRead chunks = do
      let requested = min responseReadChunkBytes (maximumEmbeddingResponseBytes - bytesRead + 1)
      chunk <- brReadSome reader requested
      let chunkBytes = fromIntegral (LBS.length chunk)
          total = bytesRead + chunkBytes
      if total > maximumEmbeddingResponseBytes
        then protocolFailure
        else if LBS.null chunk
          then pure (Right (mconcat (reverse chunks)))
          else go total (chunk : chunks)

    parseContentLength :: BS.ByteString -> Maybe Int
    parseContentLength raw = case BS8.readInteger raw of
      Just (declared, rest)
        | BS.null rest && declared >= 0 && declared <= fromIntegral (maxBound :: Int) ->
            Just (fromInteger declared)
      _ -> Nothing
