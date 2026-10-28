-- | Complete, immutable gzip objects. No database lease spans a network write.
module HMem.Server.Archive.HTTP
  ( archiveMiddleware, streamArchiveParts, archiveAudience, archiveETag
  ) where

import Control.Exception (Exception, fromException, throwIO)
import Control.Monad (unless)
import Crypto.Hash (Context, Digest, SHA256, hashInit, hashUpdate, hashFinalize)
import Data.Aeson (Value, encode, object, (.=))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Char8 qualified as B8
import Data.Int (Int64)
import Data.Pool (Pool)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time (UTCTime, diffUTCTime, getCurrentTime)
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Hasql.Connection qualified as H
import Network.HTTP.Types
import Network.HTTP.Types.Header (hIfNoneMatch, hRetryAfter, hVary, hETag)
import Network.Wai qualified as Wai
import Servant (ServerError(..), runHandler)
import System.Timeout (timeout)

import HMem.AgentSession qualified as Session
import HMem.DB.RequestContext
import HMem.DB.SessionArchive qualified as DB
import HMem.DB.SessionArchive.Reader qualified as Reader
import HMem.Server.API (authorizeSessionRead, archiveAudience)
import HMem.Server.Exception (trySynchronous)

archiveETag :: DB.ArchiveManifest -> Text -> ByteString
archiveETag manifest audience = "\"hmem-archive-" <> TE.encodeUtf8 (UUID.toText manifest.id)
  <> "-" <> TE.encodeUtf8 manifest.compressedSha256 <> "-" <> TE.encodeUtf8 audience <> "\""

newtype ArchiveAccessFailure = ArchiveAccessFailure ServerError deriving stock (Show)
instance Exception ArchiveAccessFailure

-- Resolve credentials again on every bounded turn, including the preflight.
-- Capturing the request preserves its exact authentication context; the fresh
-- resolver checks cookie logout, token revocation/expiry and disabled accounts.
archiveMiddleware :: Pool H.Connection -> (Wai.Request -> IO (Maybe Principal)) -> Wai.Middleware
archiveMiddleware pool resolve app request respond = case route of
  Nothing -> app request respond
  Just (workspace, session, generation) -> do
    original <- currentRequestContext
    let authorize = do
          fresh <- resolve request
          if fresh == Nothing || fresh /= original.principal
            then pure $ Left unauthorized
            else withPrincipalContext fresh $ runHandler (authorizeSessionRead pool workspace session)
        check = authorize >>= either (throwIO . ArchiveAccessFailure) (const $ pure ())
        prepare = do
          access <- authorize
          case access of
            Left failure -> pure $ failureResponse failure
            Right sealed -> withWorkspaceIdContext (Just workspace) $ do
              if sealed.state /= Session.Terminated
                then pure $ case generation of
                  Nothing -> json status409 [] "session_state_conflict" "Only terminated sessions have an archive"
                  Just _ -> json status404 [] "archive_not_found" "Published archive generation not found"
                else case generation of
                  Nothing -> do
                    Reader.ensureArchive pool workspace session
                    initial <- Reader.verifiedManifest pool workspace session
                    job <- DB.getArchiveJob pool workspace session
                    -- Publication may commit between these two independent
                    -- statements. A ready job must resolve its complete object.
                    ready <- case (initial, fmap (.state) job) of
                      (Nothing, Just "ready") -> do
                        published <- Reader.verifiedManifest pool workspace session
                        maybe unavailable (pure . Just) published
                      _ -> pure initial
                    now <- getCurrentTime
                    check
                    case ready of
                      Just manifest -> do
                        validateReceipt sealed manifest
                        pure $ Wai.responseLBS status200 manifestHeaders (encode $ readyManifest manifest)
                      Nothing -> pure $ pendingManifest now sealed job
                  Just requested -> do
                    ready <- Reader.verifiedManifest pool workspace session
                    case ready of
                      Just manifest | manifest.id == requested -> do
                        validateReceipt sealed manifest
                        -- Detect an unavailable/corrupt object before 200/304.
                        -- This deliberately reads storage twice, with fixed RAM.
                        let readPart = Reader.verifiedPart pool workspace session manifest.id
                        streamArchiveParts manifest readPart check (const $ pure ())
                        check
                        principal <- maybe unavailable pure original.principal
                        let audience = archiveAudience principal
                              (headerText hAuthorization) (headerText hCookie)
                            etag = archiveETag manifest audience
                            headers = contentHeaders manifest etag
                            matches = maybe False (etagMatches etag) (lookup hIfNoneMatch $ Wai.requestHeaders request)
                            body send flush = withRequestContext original{workspaceId=Just workspace} $ do
                              finished <- timeout transferMicros $
                                streamArchiveParts manifest readPart check (\bytes -> send (Builder.byteString bytes) >> flush)
                              unless (finished == Just ()) unavailable
                        pure $ if matches then Wai.responseLBS status304 headers ""
                          else Wai.responseStream status200 headers body
                      _ -> pure $ json status404 [] "archive_not_found" "Published archive generation not found"
    attempted <- trySynchronous $ timeout transferMicros prepare
    -- Streaming happens in respond, outside this handler. A post-header error
    -- aborts the response rather than replacing it with a misleading JSON tail.
    respond $ case attempted of
      Right (Just response) -> response
      Left failure | Just (ArchiveAccessFailure denied) <- fromException failure -> failureResponse denied
      _ -> json status503 [(hRetryAfter,"1")] "archive_unavailable" "Complete archive unavailable; retry shortly"
  where
    headerText name = TE.decodeUtf8With (\_ _ -> Just '\xfffd') <$> lookup name (Wai.requestHeaders request)
    route
      | Wai.requestMethod request /= methodGet = Nothing
      | otherwise = case Wai.pathInfo request of
          ["api","v1","workspaces",w,"sessions",s,"archive"] -> (,,) <$> UUID.fromText w <*> UUID.fromText s <*> pure Nothing
          ["api","v1","workspaces",w,"sessions",s,"archive",g,"content"] -> (,,) <$> UUID.fromText w <*> UUID.fromText s <*> (Just <$> UUID.fromText g)
          _ -> Nothing

transferMicros :: Int
transferMicros = 15 * 60 * 1000000

unavailable :: IO a
unavailable = throwIO (userError "Complete session archive unavailable")

unauthorized :: ServerError
unauthorized = ServerError 401 "Unauthorized" (encode $ object
  ["error" .= ("unauthorized" :: Text), "message" .= ("Authentication required" :: Text)]) [(hContentType,"application/json")]

failureResponse :: ServerError -> Wai.Response
failureResponse failure = Wai.responseLBS (mkStatus failure.errHTTPCode $ B8.pack failure.errReasonPhrase)
  (manifestHeaders <> failure.errHeaders) failure.errBody

manifestHeaders :: [Header]
manifestHeaders = [(hContentType,"application/json"),(hCacheControl,"no-store"),(hVary,"Authorization, Cookie"),("X-Content-Type-Options","nosniff")]

json :: Status -> [Header] -> Text -> Text -> Wai.Response
json status headers code message = Wai.responseLBS status (headers <> manifestHeaders)
  (encode $ object ["error" .= code,"message" .= message])

validateReceipt :: Session.AgentSession -> DB.ArchiveManifest -> IO ()
validateReceipt session manifest = unless
  (session.workspaceId == manifest.workspaceId && session.id == manifest.sessionId
   && session.finalBytes == Just manifest.rawBytes && session.finalSha256 == Just manifest.rawSha256
   && session.committedBytes == manifest.rawBytes && session.committedRecords == manifest.rawRecords
   && session.revision == manifest.rawRevision && manifest.codec == "gzip-v1"
   && manifest.compressedBytes >= 20 && manifest.partCount > 0) unavailable

readyManifest :: DB.ArchiveManifest -> Value
readyManifest m = object
  [ "workspace_id" .= m.workspaceId, "session_id" .= m.sessionId, "state" .= ("ready" :: Text)
  , "generation_id" .= m.id, "codec" .= m.codec, "codec_implementation" .= m.codecImplementation
  , "raw_bytes" .= decimal m.rawBytes, "raw_records" .= decimal m.rawRecords, "raw_sha256" .= m.rawSha256
  , "compressed_bytes" .= decimal m.compressedBytes, "compressed_sha256" .= m.compressedSha256
  , "content_path" .= ("/api/v1/workspaces/" <> UUID.toText m.workspaceId <> "/sessions/" <> UUID.toText m.sessionId <> "/archive/" <> UUID.toText m.id <> "/content")
  , "progress" .= object ["phase" .= ("ready" :: Text), "raw_bytes" .= decimal m.rawBytes,"compressed_bytes" .= decimal m.compressedBytes]
  , "retry_after_ms" .= (Nothing :: Maybe Int), "error" .= (Nothing :: Maybe Text)
  , "message" .= (Nothing :: Maybe Text), "retryable" .= False
  ]

pendingManifest :: UTCTime -> Session.AgentSession -> Maybe DB.ArchiveJob -> Wai.Response
pendingManifest now session job = Wai.responseLBS status headers $ encode $ object
  [ "workspace_id" .= session.workspaceId,"session_id" .= session.id,"state" .= state
  , "generation_id" .= (Nothing :: Maybe UUID),"codec" .= (Nothing :: Maybe Text),"codec_implementation" .= (Nothing :: Maybe Text)
  , "raw_bytes" .= fmap decimal session.finalBytes,"raw_records" .= decimal session.committedRecords,"raw_sha256" .= session.finalSha256
  , "compressed_bytes" .= (Nothing :: Maybe Text),"compressed_sha256" .= (Nothing :: Maybe Text),"content_path" .= (Nothing :: Maybe Text)
  , "progress" .= object ["phase" .= phase,"raw_bytes" .= decimal raw,"compressed_bytes" .= decimal compressed]
  , "retry_after_ms" .= (if failed then Nothing else Just delay),"error" .= (if failed then Just ("archive_failed" :: Text) else Nothing)
  , "message" .= (if failed then Just ("Archive preparation failed; operator repair required" :: Text) else Nothing),"retryable" .= not failed
  ]
  where
    failed = maybe False ((=="failed") . (.state)) job
    retry = maybe False ((=="retry") . (.state)) job
    state :: Text
    state | failed = "failed" | retry = "retrying" | otherwise = "preparing"
    status = if failed then status503 else status202
    delay :: Int
    delay = maybe 1000 (\j -> max 1000 $ min 60000 $ ceiling (1000 * diffUTCTime j.retryAt now)) job
    headers = manifestHeaders <> if failed then [] else [(hRetryAfter,B8.pack $ show ((delay+999) `div` 1000))]
    phase = maybe "queued" (\j -> if j.phase `elem` ["queued","compressing","verifying"] then j.phase else "queued") job
    raw = maybe 0 (\j -> max 0 $ min session.committedBytes j.rawProgress) job
    compressed = maybe 0 (max 0 . (.compressedProgress)) job

decimal :: Int64 -> Text
decimal = T.pack . show

contentHeaders :: DB.ArchiveManifest -> ByteString -> [Header]
contentHeaders m etag =
  [ (hContentType,"application/gzip"),(hContentLength,B8.pack $ show m.compressedBytes)
  , (hETag,etag),(hCacheControl,"private, no-cache, no-transform"),(hVary,"Authorization, Cookie")
  , ("X-HMem-Archive-Generation",TE.encodeUtf8 $ UUID.toText m.id)
  , ("X-HMem-Archive-SHA256",TE.encodeUtf8 m.compressedSha256)
  , ("X-HMem-Archive-Raw-Bytes",TE.encodeUtf8 $ decimal m.rawBytes)
  , ("X-HMem-Archive-Raw-SHA256",TE.encodeUtf8 m.rawSha256),("X-Content-Type-Options","nosniff")
  ]

etagMatches :: ByteString -> ByteString -> Bool
etagMatches tag = any (\value -> value == "*" || value == tag || value == "W/" <> tag)
  . map (B8.dropWhileEnd (==' ') . B8.dropWhile (==' ')) . B8.split ','

-- | One verified part (<=1 MiB), one incremental hash and counters. The reader
-- validates the DB position/payload binding before returning. A second check
-- immediately before send catches authority withdrawal during a part fetch.
streamArchiveParts :: DB.ArchiveManifest -> (Int64 -> IO (Maybe DB.ArchivePart))
  -> IO () -> (ByteString -> IO ()) -> IO ()
streamArchiveParts manifest readPart check send = do
  unless (manifest.compressedBytes >= 20 && manifest.partCount > 0) unavailable
  go 0 0 (hashInit :: Context SHA256)
  where
    go ordinal offset digest
      | ordinal == manifest.partCount = do
          check
          extra <- readPart ordinal
          unless (extra == Nothing && offset == manifest.compressedBytes
            && T.pack (show (hashFinalize digest :: Digest SHA256)) == manifest.compressedSha256) unavailable
      | otherwise = do
          check
          part <- readPart ordinal >>= maybe unavailable pure
          let size = fromIntegral (BS.length part.bytes) :: Int64
          unless (part.ordinal == ordinal && part.startOffset == offset && size > 0 && size <= 1048576
            && offset <= manifest.compressedBytes-size && part.endOffset == offset+size
            && part.dataSha256 == Session.sessionSha256 part.bytes) unavailable
          check
          let next = hashUpdate digest part.bytes
          next `seq` send part.bytes
          go (ordinal+1) (offset+size) next
