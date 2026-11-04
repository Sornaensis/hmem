-- | Authorized bounded document transport. Original bytes never enter JSON.
module HMem.Server.Document.HTTP
  (documentMiddleware, metadataHeader, safeHeaders, originalTransferHeaders,
   downloadAssociationQuery, authorizeDocumentDownload) where

import Control.Exception (Exception,fromException,throwIO)
import Control.Monad (unless,when)
import Control.Applicative ((<|>))
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types qualified as JSON
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as B8
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as LBS
import Data.Char (toLower,isAscii,isPrint)
import Data.Int (Int64)
import Data.List (dropWhileEnd)
import Data.Maybe (isNothing,listToMaybe)
import Data.Pool (Pool)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time (UTCTime)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Hasql.Connection qualified as H
import Network.HTTP.Types
import Network.HTTP.Types.Header (hContentDisposition,hContentLength,hETag,hVary)
import Network.HTTP.Types.URI (urlEncode)
import Network.Wai qualified as Wai
import Servant (ServerError(..),runHandler,throwError,err404)
import System.Timeout (timeout)
import Text.Read (readMaybe)

import HMem.DB.Document qualified as DB
import HMem.DB.DocumentPreview qualified as Preview
import HMem.DB.Task qualified as Task
import HMem.DB.Pool (DBException(..))
import HMem.DB.RequestContext
import HMem.Document.Types qualified as Doc
import HMem.Document.Codec (validateManifest,validateOriginal,chunks)
import HMem.Server.API (authorizeAgentSessionWrite,authorizeTaskEvidenceRead)
import HMem.Server.Exception (trySynchronous)
import HMem.SessionArchive.Codec qualified as Gzip
import HMem.Types qualified as Types

metadataHeader :: HeaderName
metadataHeader="X-HMem-Document-Metadata"

data Route=Collection | Counts | Detail UUID | Original UUID | Evidence UUID | Source UUID | RetryPreview UUID
data HTTPFailure=HTTPFailure Status Text deriving stock(Show)
instance Exception HTTPFailure
newtype AccessFailure=AccessFailure ServerError deriving stock(Show)
instance Exception AccessFailure

data UploadMetadata=UploadMetadata UUID Doc.DocumentRole Doc.OriginalManifest
instance FromJSON UploadMetadata where
  parseJSON=withObject "document upload metadata" $ \obj -> do
    exactKeys ["request_id","role","manifest"] obj
    original<-obj .: "manifest"
    withObject "original manifest" (exactKeys ["raw_bytes","raw_sha256","filename","title","media_type"]) original
    UploadMetadata <$> obj .: "request_id" <*> obj .: "role" <*> parseJSON original

exactKeys :: [Text] -> Object -> JSON.Parser ()
exactKeys expected obj=unless(all ((`elem` expected) . Key.toText)(KM.keys obj))(fail "Unknown document metadata field")

problem :: Status -> Text -> IO a
problem status code=throwIO(HTTPFailure status code)

documentMiddleware :: Pool H.Connection -> (Wai.Request -> IO(Maybe Principal)) -> Wai.Middleware
documentMiddleware pool resolve app request respond=case documentRoute request of
  Nothing -> app request respond
  Just(workspace,task,route) -> do
    original<-currentRequestContext
    let writing=Wai.requestMethod request `elem` [methodPost,methodDelete,methodPut,methodPatch]
        authorize=do
          fresh<-resolve request
          if isNothing fresh || fresh/=original.principal then problem status401 "unauthorized" else
            withPrincipalContext fresh $ do
              result<-runHandler $ if writing then do
                authorizeAgentSessionWrite pool workspace
                target<-liftIO(Task.getTask pool task)
                case target of Just row | row.workspaceId==workspace -> pure(); _ -> throwError err404
                else authorizeTaskEvidenceRead pool workspace task
              either (throwIO . AccessFailure) pure result
        lookupDocument identifier=DB.getDocument pool workspace task identifier >>= maybe(problem status404 "document_not_found") pure
        prepare=withWorkspaceIdContext(Just workspace) $ do
          -- No body/header-manifest inspection or reservation precedes this.
          authorize
          case (Wai.requestMethod request,route) of
            ("POST",Collection) -> do
              UploadMetadata requestId role manifest<-uploadMetadata request
              validateUploadHeaders request manifest.rawBytes
              validateManifest manifest
              let input=Doc.UploadDocument workspace task requestId role manifest
              previous<-DB.reserveUpload pool input
              bytes<-collectBody request manifest.rawBytes
              validateOriginal manifest bytes
              authorize
              document<-maybe(DB.publishUpload pool input bytes) pure previous
              pure(json (if isNothing previous then status201 else status200) document)
            ("GET",Collection) -> do
              (role,cursor,limit)<-listQuery request
              json status200 <$> DB.listDocuments pool workspace task role cursor limit
            ("GET",Counts) -> DB.getTaskDocumentCounts pool workspace task >>= maybe(problem status404 "task_not_found") (pure . json status200)
            ("GET",Detail identifier) -> json status200 <$> lookupDocument identifier
            (method,Original identifier) | method `elem` [methodGet,methodHead] -> do
              selection<-either (problem status400) pure (downloadAssociationQuery request)
              let authorizeBytes selected=authorizeDocumentDownload pool (isBotRequest original) workspace task identifier selected
                    >>= either (uncurry problem) pure
              association<-authorizeBytes selection
              document<-lookupDocument identifier
              let headers=originalHeaders document
              if method==methodHead then pure(Wai.responseLBS status200 headers "") else do
                bytes<-DB.readOriginal pool workspace task identifier
                let check=authorize >> authorizeBytes association >> lookupDocument identifier >> pure()
                    stream send flush=withRequestContext original{workspaceId=Just workspace} $ do
                      complete<-timeout transferMicros $ mapM_ (\part -> check >> send(Builder.byteString part) >> flush)(chunks bytes)
                      unless(complete==Just())(problem status503 "document_transfer_timeout")
                check
                pure(Wai.responseStream status200 headers stream)
            ("POST",Evidence identifier) -> do
              requireContentType request "application/json"
              bytes<-collectBounded request 4096
              requestId<-case eitherDecodeStrict' bytes >>= parseEvidence of
                Left _ -> problem status400 "document_invalid_request"
                Right value -> pure value
              authorize
              json status200 <$> DB.addEvidence pool workspace task identifier requestId
            ("DELETE",Source identifier) -> do
              DB.removeSource pool workspace task identifier
              pure(Wai.responseLBS status204 safeHeaders "")
            ("POST",RetryPreview identifier) -> do
              _<-lookupDocument identifier
              accepted<-Preview.retry pool workspace task identifier
              pure(json (if accepted then status202 else status200)(object["accepted" .= accepted]))
            _ -> problem status405 "method_not_allowed"
    attempted<-trySynchronous(timeout transferMicros prepare)
    -- Streaming is deliberately outside the error-to-JSON handler: a failure
    -- after headers aborts the transfer rather than appending an error body.
    respond $ case attempted of
      Right(Just response) -> response
      Right Nothing -> jsonError status408 "document_request_timeout"
      Left failure
        | Just(AccessFailure denied)<-fromException failure -> Wai.responseLBS(mkStatus denied.errHTTPCode(B8.pack denied.errReasonPhrase))(safeHeaders<>denied.errHeaders) denied.errBody
        | Just(HTTPFailure status code)<-fromException failure -> jsonError status code
        | Just(Doc.DocumentException code)<-fromException failure -> documentError code
        | Just(DBCheckViolation code)<-fromException failure -> documentError code
        | Just(DBForeignKeyViolation _)<-fromException failure -> jsonError status404 "document_not_found"
        | Just DBStatementTimeout<-fromException failure -> jsonError status504 "document_database_timeout"
        | Just(Gzip.CodecFailure _)<-fromException failure -> jsonError status503 "document_unavailable"
        | otherwise -> jsonError status503 "document_unavailable"

documentRoute :: Wai.Request -> Maybe(UUID,UUID,Route)
documentRoute req=case dropWhileEnd T.null(Wai.pathInfo req) of
  "api":"v1":"workspaces":w:"tasks":t:"documents":rest -> do
    workspace<-UUID.fromText w; task<-UUID.fromText t
    route<-case rest of
      [] -> Just Collection
      ["counts"] -> Just Counts
      [identifier] -> Detail <$> UUID.fromText identifier
      [identifier,"original"] -> Original <$> UUID.fromText identifier
      [identifier,"evidence"] -> Evidence <$> UUID.fromText identifier
      [identifier,"source"] -> Source <$> UUID.fromText identifier
      [identifier,"previews","retry"] -> RetryPreview <$> UUID.fromText identifier
      _ -> Nothing
    pure(workspace,task,route)
  _ -> Nothing

uploadMetadata :: Wai.Request -> IO UploadMetadata
uploadMetadata request=case [value | (name,value)<-Wai.requestHeaders request,name==metadataHeader] of
  [value] | BS.length value<=8192 -> either (const(problem status400 "document_invalid_metadata")) pure(eitherDecodeStrict' value)
  [_] -> problem status431 "document_metadata_too_large"
  _ -> problem status400 "document_metadata_required"

validateUploadHeaders :: Wai.Request -> Int64 -> IO()
validateUploadHeaders request expected=do
  when(expected<0 || expected>Doc.maxOriginalBytes)(problem status413 "document_size_exceeded")
  requireContentType request "application/octet-stream"
  case [value | (name,value)<-Wai.requestHeaders request,name==hContentLength] of
    [] -> pure()
    [value] | BS.length value<=20, B8.all (`elem` ['0'..'9']) value, Just count<-(readMaybe(B8.unpack value)::Maybe Integer) ->
      unless(count==fromIntegral expected)(problem status400 "document_length_mismatch")
    _ -> problem status400 "document_invalid_content_length"
  case Wai.requestBodyLength request of
    Wai.KnownLength count -> unless(count==fromIntegral expected)(problem status400 "document_length_mismatch")
    _ -> pure()

collectBody :: Wai.Request -> Int64 -> IO ByteString
collectBody request expected=do
  bytes<-collectBounded request expected
  unless(fromIntegral(BS.length bytes)==expected)(problem status400 "document_incomplete_body")
  pure bytes

collectBounded :: Wai.Request -> Int64 -> IO ByteString
collectBounded request bound=do
  case Wai.requestBodyLength request of
    Wai.KnownLength count | count>fromIntegral bound -> problem status413 "document_size_exceeded"
    _ -> pure()
  loop 0 []
  where
    loop total parts=do
      bytes<-Wai.getRequestBodyChunk request
      let count=fromIntegral(BS.length bytes)
          next=total+count
      if count>bound-total then problem status413 "document_size_exceeded"
      else if BS.null bytes then pure(BS.concat(reverse parts)) else loop next(bytes:parts)

requireContentType :: Wai.Request -> ByteString -> IO()
requireContentType request expected=case [value | (name,value)<-Wai.requestHeaders request,name==hContentType] of
  [value] | B8.map toLower(B8.takeWhile(/=';') value)==expected -> pure()
  _ -> problem status415 "document_content_type_required"

parseEvidence :: Value -> Either String UUID
parseEvidence=JSON.parseEither $ withObject "evidence association" $ \obj -> exactKeys ["request_id"] obj >> obj .: "request_id"

listQuery :: Wai.Request -> IO(Doc.DocumentRole,Maybe Doc.DocumentCursor,Int)
listQuery request=do
  let value key=case [raw | (name,raw)<-Wai.queryString request,name==key] of
        [] -> pure Nothing
        [Just raw] -> pure(Just raw)
        _ -> problem status400 "document_invalid_query"
  selected<-value "role" >>= \case Nothing -> pure Doc.Source; Just "source" -> pure Doc.Source; Just "evidence" -> pure Doc.Evidence; _ -> problem status400 "document_invalid_role"
  limit<-value "limit" >>= \case
    Nothing -> pure 50
    Just raw -> case readMaybe(B8.unpack raw) of Just n | n>=1 && n<=50 -> pure n; _ -> problem status400 "document_invalid_limit"
  beforeTime<-value "before_created_at"; beforeId<-value "before_id"
  cursor<-case(beforeTime,beforeId) of
    (Nothing,Nothing) -> pure Nothing
    (Just time,Just identifier) | Just parsedTime<-(iso8601ParseM(B8.unpack time)::Maybe UTCTime), Just parsedId<-UUID.fromASCIIBytes identifier -> pure(Just(Doc.DocumentCursor parsedTime parsedId))
    _ -> problem status400 "document_invalid_cursor"
  pure(selected,cursor,limit)

downloadAssociationQuery :: Wai.Request -> Either Text (Maybe (Doc.DocumentRole,UUID))
downloadAssociationQuery request = case (values "role",values "association_id") of
  ([],[]) -> Right Nothing
  ([Just role],[Just identifier])
    | Just selected <- lookup role [("source",Doc.Source),("evidence",Doc.Evidence)]
    , Just association <- UUID.fromASCIIBytes identifier -> Right (Just (selected,association))
  _ -> Left "document_invalid_association"
  where values key = [value | (name,value) <- Wai.queryString request, name==key]

-- | Pin the association authorizing bytes. Legacy unscoped human reads keep
-- their original behavior. A bot without a selection uses a Source if one
-- exists, then falls back to the Evidence status gate.
authorizeDocumentDownload :: Pool H.Connection -> Bool -> UUID -> UUID -> UUID
  -> Maybe (Doc.DocumentRole,UUID) -> IO (Either (Status,Text) (Maybe (Doc.DocumentRole,UUID)))
authorizeDocumentDownload pool bot workspace task document selected
  | not bot && isNothing selected = pure (Right Nothing)
  | otherwise = do
      scope <- DB.getDownloadScope pool workspace task document
      pure $ do
        (terminal,source,evidence) <- maybe (Left missing) Right scope
        let links = [(Doc.Source,identifier) | identifier <- maybe [] pure source]
                 <> [(Doc.Evidence,identifier) | identifier <- maybe [] pure evidence]
        association@(role,_) <- maybe (Left missing) Right (selected <|> listToMaybe links)
        unless (association `elem` links) (Left missing)
        when (bot && role==Doc.Evidence && not terminal) $
          Left (status403,"document_evidence_task_not_terminal")
        pure (Just association)
  where missing = (status404,"document_not_found")

safeHeaders :: [Header]
safeHeaders=[(hCacheControl,"no-store"),(hVary,"Authorization, Cookie"),("Referrer-Policy","no-referrer"),("X-Content-Type-Options","nosniff")]
json :: ToJSON a => Status -> a -> Wai.Response
json status value=Wai.responseLBS status ((hContentType,"application/json"):safeHeaders)(encode value)
jsonError :: Status -> Text -> Wai.Response
jsonError status code=json status(object["error" .= code,"message" .= code])

documentError :: Text -> Wai.Response
documentError code
  | code `elem` ["document_task_not_found","document_workspace_not_found","document_not_found"] = jsonError status404 "document_not_found"
  | code `elem` ["document_request_conflict","document_reservation_missing_or_conflicting"] = jsonError status409 "document_request_conflict"
  | code=="document_source_task_not_ready" = jsonError status403 code
  | code `elem` ["document_quota_exceeded","document_task_link_limit"] = jsonError status409 code
  | code=="document_unsupported_or_mismatched_type" = jsonError status415 code
  | code=="document_size_exceeded" = jsonError status413 code
  | code `elem` ["document_compressed_manifest_mismatch","document_raw_manifest_mismatch","document_manifest_bound","document_part_bound","document_codec_roundtrip"] = jsonError status503 "document_unavailable"
  | otherwise = jsonError status400 "document_invalid_input"

originalHeaders :: Doc.Document -> [Header]
originalHeaders document=originalTransferHeaders document.filename document.rawBytes document.rawSha256

originalTransferHeaders :: Text -> Int64 -> Text -> [Header]
originalTransferHeaders filename rawBytes rawSha256=safeHeaders <>
  [(hContentType,"application/octet-stream"),(hContentLength,B8.pack(show rawBytes))
  ,(hContentDisposition,"attachment; filename=\""<>TE.encodeUtf8 fallback<>"\"; filename*=UTF-8''"<>urlEncode False(TE.encodeUtf8 filename))
  ,(hETag,"\""<>TE.encodeUtf8 rawSha256<>"\"")
  ,("X-HMem-Document-SHA256",TE.encodeUtf8 rawSha256)
  ,("X-HMem-Document-Bytes",B8.pack(show rawBytes))
  ,("Content-Security-Policy","default-src 'none'; sandbox")]
  where fallback=T.map(\c -> if isAscii c && isPrint c && c `notElem` ['"','\\','/'] then c else '_') filename

transferMicros :: Int
transferMicros=5*60*1000000
