-- Finite local file operations over the authorized document HTTP contract.
module HMem.MCP.Attachment
  ( AttachmentCall(..), AttachmentOptions(..), defaultAttachmentOptions
  , isAttachmentTool, attachmentToolDefinitions, parseAttachmentCall
  , executeAttachment, executeAttachmentWith, asciiJSON
  ) where

import Control.Exception hiding (handle)
import Control.Monad (forM, unless, when)
import Crypto.Hash (Context, Digest, SHA256, hashInit, hashUpdate, hashFinalize)
import Data.Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types (Pair, parseEither)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as B8
import Data.ByteString.Lazy qualified as BL
import Data.Char (ord, toLower)
import Data.Int (Int64)
import Data.IORef
import Data.List (nub)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time (UTCTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Network.HTTP.Client
import Network.HTTP.Types (Header, HeaderName, statusCode)
import Network.HTTP.Types.URI (renderQuery, parseQuery)
import Numeric (showHex)
import System.Directory (doesPathExist, removeFile)
import System.FilePath (takeDirectory, takeExtension, takeFileName)
import System.IO
import System.Timeout (timeout)
import Text.Read (readMaybe)

import HMem.BotIdentity (BotIdentity)
import HMem.Document.Types qualified as Doc
import HMem.Document.Codec (validateManifest)
import HMem.Document.Grant qualified as Grant
import HMem.MCP.SessionFile qualified as File
import HMem.MCP.AttachmentHTTP qualified as HTTP

data HTTPContext = HTTPContext
  { scope :: !HTTP.AttachmentScope, deadline :: !HTTP.Deadline, ackPending :: !(IORef Bool) }

data AttachmentCall
  = Upload UUID UUID FilePath Doc.DocumentRole UUID (Maybe Text) (Maybe Text) (Maybe Text)
  | List UUID UUID Doc.DocumentRole (Maybe Doc.DocumentCursor) Int
  | Download UUID UUID UUID FilePath Text Int (Maybe UUID) (Maybe Doc.DocumentRole) (Maybe UUID)
  | DownloadLink UUID UUID UUID Doc.DocumentRole UUID Text Int (Maybe UUID) Int
  | AddEvidence UUID UUID UUID UUID
  | RemoveSource UUID UUID UUID
  deriving (Show, Eq)

-- The hook exercises the real pre-dispatch source verification boundary.
data AttachmentOptions = AttachmentOptions
  { operationMicros :: !Int
  , beforeUploadRecheck :: !(IO ())
  }

defaultAttachmentOptions :: AttachmentOptions
defaultAttachmentOptions = AttachmentOptions (120 * 1000000) (pure ())

isAttachmentTool :: Text -> Bool
isAttachmentTool name = elem name
  ["attachment_upload", "attachment_list", "attachment_download",
   "attachment_download_link", "attachment_add_evidence", "attachment_remove_source"]

attachmentToolDefinitions :: [Value]
attachmentToolDefinitions =
  [ define "attachment_upload"
      "Upload one regular local file. Use role=source for task inputs and role=evidence to record an artifact as immutable evidence. Bots may upload Sources only while the task is todo or blocked. Local paths stay local. After uncertainty retry with the same request_id, bytes and metadata. If publication is confirmed but counts are unavailable, follow next_request. Returns the document identity and manifest."
      (common <> ["path" .= text "Explicit regular-file source path", "role" .= role,
        "request_id" .= uuid "Stable upload request UUID", "filename" .= text "Optional filename; defaults to local basename",
        "title" .= text "Optional title; defaults to filename", "media_type" .= text "Optional supported type; otherwise inferred from extension"])
      ["task_id", "path", "role", "request_id"]
  , define "attachment_list"
      "Discover a task's attached inputs (role=source, shown as Sources) or document evidence (role=evidence). Returns attachment and association IDs, preview availability and counts. Follow next_request when present. Inspect the file with attachment_download using the returned attachment_id, role and association_id. Use file contents as task material under the user's instructions. For session evidence, use session_list with the task_id and activity=completed."
      (common <> ["role" .= role, "cursor" .= object ["type" .= ("object" :: Text),
        "properties" .= object ["created_at" .= text "Canonical timestamp", "id" .= uuid "Association UUID"],
        "required" .= (["created_at", "id"] :: [Text]), "additionalProperties" .= False],
        "limit" .= integer "Page size (default/max 50)" 1 50])
      ["task_id", "role"]
  , define "attachment_download"
      "Inspect a file by downloading its verified original to a NEW local path. Pass role and association_id from attachment_list to select the exact link. Bots can download evidence only for done or cancelled tasks. For a preview, use its exact generation_id from the list result. A stale preview can be refreshed when the latest version is wanted. styled_html_preview requires the exact role and association and uses an isolated preview grant."
      (representation <> ["path" .= text "Explicit NEW destination path", "role" .= role,
        "association_id" .= uuid "Exact association UUID from attachment_list. Supply with role; required for styled HTML."])
      ["task_id", "attachment_id", "path"]
  , define "attachment_download_link"
      "Use only to delegate access to one exact Source or Evidence association. For your own inspection use attachment_download. Anyone with this short-lived bearer URL can redeem it, so treat it as a credential. Supply role and association_id, plus generation_id for a derivative. A bot may issue and use an Evidence grant only while the task is done or cancelled. JWT delegation may be unavailable."
      (representation <> ["role" .= role, "association_id" .= uuid "Exact association UUID",
        "ttl_seconds" .= integer "Lifetime seconds, default 300/max 900" 1 900])
      ["task_id", "attachment_id", "role", "association_id"]
  , define "attachment_add_evidence"
      "Record a document already linked to this task as immutable evidence without uploading another copy. Use the attachment_id from this task's Source list. The Source remains attached. Use a stable request_id for exact retries."
      (common <> ["attachment_id" .= uuid "Immutable document UUID", "request_id" .= uuid "Stable association request UUID"])
      ["task_id", "attachment_id", "request_id"]
  , define "attachment_remove_source"
      "Remove only this task's Source association. Existing evidence and original bytes remain. Returns canonical counts."
      (common <> ["attachment_id" .= uuid "Immutable document UUID"]) ["task_id", "attachment_id"]
  ]
  where
    text description = object ["type" .= ("string" :: Text), "description" .= (description :: Text)]
    uuid = text
    integer description low high = object ["type" .= ("integer" :: Text), "description" .= (description :: Text),
      "minimum" .= (low :: Int), "maximum" .= (high :: Int)]
    role = object ["type" .= ("string" :: Text), "enum" .= (["source", "evidence"] :: [Text])]
    common = ["workspace_id" .= uuid "Optional explicit workspace; accepted declaration remains required", "task_id" .= uuid "Task UUID"]
    representation = common <> ["attachment_id" .= uuid "Immutable document UUID, not association UUID",
      "variant" .= object ["type" .= ("string" :: Text), "enum" .= (["original","page","thumbnail","styled_html_preview"] :: [Text]),
        "default" .= ("original" :: Text)], "page" .= integer "Page, default 0" 0 200,
      "generation_id" .= uuid "Required exact generation for derivatives"]
    define name description properties required = object ["name" .= (name :: Text), "description" .= (description :: Text),
      "inputSchema" .= object ["type" .= ("object" :: Text), "properties" .= object properties,
        "required" .= (required :: [Text]), "additionalProperties" .= False]]

parseAttachmentCall :: Text -> Value -> Either String AttachmentCall
parseAttachmentCall name = parseEither $ withObject "attachment arguments" $ \o -> do
  let check allowed = unless (all (flip elem allowed . Key.toText) (KM.keys o)) (fail "Unsupported attachment fields")
      path = do
        value <- o .: "path"
        when (null value || elem '\0' value) (fail "An explicit filesystem path without NUL is required")
        pure value
      representation = do
        variant <- o .:? "variant" .!= "original"
        page <- o .:? "page" .!= 0
        generation <- o .:? "generation_id"
        unless (Grant.validRepresentation variant page generation) (fail "Invalid representation; derivatives require generation_id")
        pure (variant, page, generation)
      baseKeys = ["workspace_id", "task_id"]
      representationKeys = baseKeys <> ["attachment_id", "variant", "page", "generation_id"]
  workspace <- o .: "workspace_id"
  task <- o .: "task_id"
  case name of
    "attachment_upload" -> do
      check (baseKeys <> ["path", "role", "request_id", "filename", "title", "media_type"])
      Upload workspace task <$> path <*> o .: "role" <*> o .: "request_id"
        <*> o .:? "filename" <*> o .:? "title" <*> o .:? "media_type"
    "attachment_list" -> do
      check (baseKeys <> ["role", "cursor", "limit"])
      cursor <- o .:? "cursor"
      limit <- o .:? "limit" .!= 50
      unless (limit >= 1 && limit <= 50) (fail "limit must be between 1 and 50")
      List workspace task <$> o .: "role" <*> pure cursor <*> pure limit
    "attachment_download" -> do
      check (representationKeys <> ["path", "role", "association_id"])
      (variant, page, generation) <- representation
      chosenRole <- o .:? "role"
      association <- o .:? "association_id"
      unless ((chosenRole == Nothing) == (association == Nothing))
        (fail "role and association_id must be supplied together")
      when (variant == "styled_html_preview" && (chosenRole == Nothing || association == Nothing))
        (fail "styled_html_preview requires explicit role and association_id")
      Download workspace task <$> o .: "attachment_id" <*> path <*> pure variant <*> pure page
        <*> pure generation <*> pure chosenRole <*> pure association
    "attachment_download_link" -> do
      check (representationKeys <> ["role", "association_id", "ttl_seconds"])
      (variant, page, generation) <- representation
      lifetime <- o .:? "ttl_seconds" .!= 300
      unless (lifetime >= 1 && lifetime <= 900) (fail "ttl_seconds must be between 1 and 900")
      DownloadLink workspace task <$> o .: "attachment_id" <*> o .: "role" <*> o .: "association_id"
        <*> pure variant <*> pure page <*> pure generation <*> pure lifetime
    "attachment_add_evidence" -> do
      check (baseKeys <> ["attachment_id", "request_id"])
      AddEvidence workspace task <$> o .: "attachment_id" <*> o .: "request_id"
    "attachment_remove_source" -> do
      check (baseKeys <> ["attachment_id"])
      RemoveSource workspace task <$> o .: "attachment_id"
    _ -> fail "Unknown attachment tool"

data Fault = Fault !Text !(Maybe Int) ![Pair] deriving (Show)
instance Exception Fault
fault :: Text -> IO a
fault code = throwIO (Fault code Nothing [])
ensure :: Bool -> IO ()
ensure ok = unless ok (fault "invalid_response")

envelope :: Value -> Value
envelope value = object ["structuredContent" .= value, "content" .=
  [object ["type" .= ("text" :: Text), "text" .= TE.decodeUtf8 (BL.toStrict (encode value))]]]
failure :: Text -> Maybe Int -> [Pair] -> Value
failure code status extra = object ["isError" .= True, "structuredContent" .= object
  ["error" .= object (["code" .= code, "message" .= message] <> maybe [] (\n -> ["http_status" .= n]) status <> extra)],
  "content" .= [object ["type" .= ("text" :: Text), "text" .= message]]]
  where message = "Attachment operation did not complete: " <> code

executeAttachment :: HTTP.AttachmentScope -> Manager -> String -> Maybe Text -> BotIdentity -> [Header] -> AttachmentCall -> IO Value
executeAttachment scope = executeAttachmentWith scope defaultAttachmentOptions
executeAttachmentWith :: HTTP.AttachmentScope -> AttachmentOptions -> Manager -> String -> Maybe Text -> BotIdentity -> [Header] -> AttachmentCall -> IO Value
executeAttachmentWith scope options _manager base credential identity provenance call = do
  progress <- newIORef []
  let total = max 10000 options.operationMicros
      reserve = min 5000000 (max 1000 (div total 10))
      headers = [("X-HMem-Bot-Identity", BL.toStrict (encode identity)), ("Accept-Encoding", "identity")]
        <> provenance <> maybe [] (\token -> [("Authorization", TE.encodeUtf8 ("Bearer " <> token))]) credential
  deadline <- HTTP.newDeadline total reserve
  ackPending <- newIORef False
  let transport = HTTPContext scope deadline ackPending
  -- The transport owns the earlier work cutoff and its retirement reserve.
  -- A second timer at that cutoff could interrupt the native retirement wait.
  attempted <- try @SomeException $ timeout total $ perform options transport base headers progress call
  retry <- readIORef progress
  pendingAck <- readIORef ackPending
  let failed code status extra
        | not pendingAck
        , not (elem code ["source_changed_after_publication","attachment_cleanup_timeout","attachment_cleanup_failed","attachment_transport_protocol"])
        , Just (Object receipt) <- KM.lookup "receipt" (KM.fromList retry) =
            envelope $ Object $ KM.insert "counts" Null $ KM.insert "counts_status" (String "unavailable") $
              KM.insert "follow_up_error" (object (["code" .= code] <> maybe [] (\n -> ["http_status" .= n]) status)) $
              maybe id (KM.insert "next_request") (KM.lookup "next_request" (KM.fromList retry)) receipt
        | otherwise = failure code status (retry <> extra)
  case attempted of
    Right (Just value) -> pure (envelope value)
    Right Nothing -> pure (failed "attachment_cleanup_timeout" Nothing [])
    Left exception | Just (_ :: SomeAsyncException) <- fromException exception -> throwIO exception
    Left exception | Just HTTP.TransportOwnership <- fromException exception -> pure (failed "attachment_cleanup_failed" Nothing [])
    Left exception | Just HTTP.TransportDeadline <- fromException exception -> pure (failed "attachment_timeout" Nothing [])
    Left exception | Just HTTP.TransportProtocol <- fromException exception -> pure (failed "attachment_transport_protocol" Nothing [])
    Left exception | Just HTTP.TransportUnavailable <- fromException exception -> pure (failed "attachment_transport_unavailable" Nothing [])
    Left exception | Just (Fault code status extra) <- fromException exception -> pure (failed code status extra)
    Left exception | Just (_ :: File.SourceFault) <- fromException exception -> pure (failed "source_changed_or_unavailable" Nothing [])
    Left exception | Just (Doc.DocumentException code) <- fromException exception -> pure (failed code Nothing [])
    Left _ -> pure (failed "attachment_transport_or_file_error" Nothing [])

perform :: AttachmentOptions -> HTTPContext -> String -> [Header] -> IORef [Pair] -> AttachmentCall -> IO Value
perform options manager base headers progress = \case
  Upload workspace task path role requestId filename title media ->
    bracket (File.openSource path) closeSourceChecked $ \source -> do
      size <- File.checkSource source 0
      when (size < 0 || size > Doc.maxOriginalBytes) (fault "document_size_exceeded")
      (parts, digest) <- snapshot source size
      let name = fromMaybe (T.pack (takeFileName path)) filename
          manifest = Doc.OriginalManifest size digest name (fromMaybe name title) (fromMaybe (inferMediaType name) media)
      validateManifest manifest
      options.beforeUploadRecheck
      verifySource source size digest
      writeIORef progress ["request_id" .= requestId, "manifest" .= manifest,
        "retry" .= ("Reuse this request_id with the unchanged file and metadata; do not replace the UUID after uncertainty" :: Text)]
      let metadata = asciiJSON (object ["request_id" .= requestId, "role" .= role, "manifest" .= manifest])
      when (BS.length metadata > 8192) (fault "document_metadata_too_large")
      initial <- privateRequest base headers "POST" (collection workspace task)
      let request = initial {requestHeaders = requestHeaders initial <> [("Content-Type", "application/octet-stream"),
            ("X-HMem-Document-Metadata", metadata), ("X-Request-Id", TE.encodeUtf8 (UUID.toText requestId))]}
          acknowledge value = do
            document <- decodeValue value
            validateDocument workspace document
            ensure (document.filename == manifest.filename && document.title == manifest.title
              && document.mediaType == manifest.mediaType && document.rawBytes == size && document.rawSha256 == digest)
            rememberReceipt progress workspace task role
              (documentReceipt workspace task document <> ["request_id" .= requestId,"role" .= role,"verified" .= True,
                "publication_confirmed" .= True,"source_recheck" .= ("pending" :: Text)])
            writeIORef manager.ackPending True
      document <- jsonResponseParts manager request parts acknowledge >>= decodeValue
      writeIORef manager.ackPending False
      let confirmed = documentReceipt workspace task document <>
            ["request_id" .= requestId,"role" .= role,"verified" .= True,
             "publication_confirmed" .= True,"source_recheck" .= ("pending" :: Text)]
      stable <- try @File.SourceFault (verifySource source size digest)
      case stable of
        Left _ -> throwIO (Fault "source_changed_after_publication" Nothing [])
        Right () -> pure ()
      let checked = filter ((/="source_recheck") . fst) confirmed <> ["source_recheck" .= ("complete" :: Text)]
      rememberReceipt progress workspace task role checked
      association <- findAssociation manager base headers workspace task document.id role
      let linked = checked <> ["association_id" .= association]
      rememberReceipt progress workspace task role linked
      counts <- readCounts manager base headers workspace task
      pure (object (linked <> ["counts" .= counts, "counts_status" .= ("current" :: Text)]))
  List workspace task role cursor limit -> do
    let parameters = [("role", Just (TE.encodeUtf8 (Doc.roleText role))), ("limit", Just (B8.pack (show limit)))]
          <> maybe [] (\c -> [("before_created_at", Just (B8.pack (iso8601Show c.createdAt))),
               ("before_id", Just (TE.encodeUtf8 (UUID.toText c.id)))]) cursor
    request <- privateRequest base headers "GET" (collection workspace task <> B8.unpack (renderQuery True parameters))
    result <- jsonResponse manager request >>= decodeValue @Doc.DocumentPage
    ensure (length result.items <= limit && length (nub (map (.id) result.items)) == length result.items)
    items <- forM result.items $ \link -> do
      ensure (link.taskId == task && link.role == role)
      validateDocument workspace link.document
      preview <- readPreview manager base headers workspace task link.document
      pure $ object (documentReceipt workspace task link.document <>
        ["association_id" .= link.id, "role" .= link.role, "associated_at" .= link.createdAt,
          "uploader" .= link.document.uploader, "preview" .= previewSummary preview])
    counts <- readCounts manager base headers workspace task
    let next = fmap (\c -> object ["name" .= ("attachment_list" :: Text), "arguments" .= object
          ["workspace_id" .= workspace, "task_id" .= task, "role" .= role, "cursor" .= c, "limit" .= limit]]) result.nextCursor
    pure $ object ["workspace_id" .= workspace, "task_id" .= task, "role" .= role, "items" .= items,
      "counts" .= counts, "next_cursor" .= result.nextCursor, "next_request" .= next]
  Download workspace task identifier destination variant page generation chosenRole association -> do
    document <- readDocument manager base headers workspace task identifier
    representation <- resolveRepresentation manager base headers workspace task document variant page generation
    (request, delegated) <- if variant == "styled_html_preview" then do
      role <- maybe (fault "explicit_association_required") pure chosenRole
      link <- maybe (fault "explicit_association_required") pure association
      grant <- issueGrant manager base headers workspace task document representation role link 300
      url <- jsonField "url" grant >>= decodeValue
      fresh <- grantRequest url
      pure (fresh, Just grant)
      else do
        let scope = case (chosenRole,association) of
              (Just role,Just link) -> [("role",Just (TE.encodeUtf8 (Doc.roleText role))),("association_id",Just (UUID.toASCIIBytes link))]
              _ -> []
        fresh <- privateRequest base headers "GET"
          (representationPath workspace task identifier representation <> B8.unpack (renderQuery True scope))
        pure (fresh, Nothing)
    downloadTo manager request representation destination
    pure $ object (documentReceipt workspace task document <>
      ["variant" .= variant, "page" .= page, "generation_id" .= representation.generation,
       "policy_version" .= representation.policy, "manifest" .= representationManifest representation,
       "path" .= destination, "verified" .= True] <> maybe [] (\g -> ["grant" .= g]) delegated)
  DownloadLink workspace task identifier role association variant page generation lifetime -> do
    document <- readDocument manager base headers workspace task identifier
    representation <- resolveRepresentation manager base headers workspace task document variant page generation
    issueGrant manager base headers workspace task document representation role association lifetime
  AddEvidence workspace task identifier requestId -> do
    writeIORef progress ["request_id" .= requestId,"attachment_id" .= identifier,
      "retry" .= ("Reuse this request_id and attachment_id after uncertainty" :: Text)]
    initial <- privateRequest base headers "POST" (documentPath workspace task identifier <> "/evidence")
    let request = initial {requestHeaders = requestHeaders initial <>
      [("Content-Type", "application/json"), ("X-Request-Id", TE.encodeUtf8 (UUID.toText requestId))],
          requestBody = RequestBodyLBS (encode (object ["request_id" .= requestId]))}
        acknowledge value = do
          document <- decodeValue @Doc.Document value
          ensure (document.id == identifier)
          validateDocument workspace document
          rememberReceipt progress workspace task Doc.Evidence
            (documentReceipt workspace task document <> ["role" .= Doc.Evidence,
              "request_id" .= requestId,"publication_confirmed" .= True])
          writeIORef manager.ackPending True
    value <- jsonResponseAcknowledged manager request acknowledge
    writeIORef manager.ackPending False
    -- The authoritative acknowledgement is a flat Document, not DocumentLink.
    document <- decodeValue @Doc.Document value
    ensure (document.id == identifier)
    validateDocument workspace document
    let confirmed = documentReceipt workspace task document <>
          ["role" .= Doc.Evidence, "request_id" .= requestId, "publication_confirmed" .= True]
    rememberReceipt progress workspace task Doc.Evidence confirmed
    association <- findAssociation manager base headers workspace task identifier Doc.Evidence
    let linked = confirmed <> ["association_id" .= association]
    rememberReceipt progress workspace task Doc.Evidence linked
    counts <- readCounts manager base headers workspace task
    pure $ object (linked <> ["counts" .= counts, "counts_status" .= ("current" :: Text)])
  RemoveSource workspace task identifier -> do
    request <- privateRequest base headers "DELETE" (documentPath workspace task identifier <> "/source")
    _ <- requestBytes manager request [] $ \status _ bytes -> do
      checkStatusBytes status bytes
      ensure (status == 204 && BS.null bytes)
      rememberReceipt progress workspace task Doc.Source
        ["workspace_id" .= workspace, "task_id" .= task, "attachment_id" .= identifier,
         "action" .= ("source_removed" :: Text), "publication_confirmed" .= True]
      writeIORef manager.ackPending True
    writeIORef manager.ackPending False
    counts <- readCounts manager base headers workspace task
    pure $ object ["workspace_id" .= workspace, "task_id" .= task, "attachment_id" .= identifier,
      "action" .= ("source_removed" :: Text), "publication_confirmed" .= True,
      "counts" .= counts, "counts_status" .= ("current" :: Text)]

closeSourceChecked :: File.Source -> IO ()
closeSourceChecked source = File.closeSource source `catch` \(_ :: File.SourceFault) ->
  fault "attachment_cleanup_failed"

rememberReceipt :: IORef [Pair] -> UUID -> UUID -> Doc.DocumentRole -> [Pair] -> IO ()
rememberReceipt progress workspace task role receipt = modifyIORef' progress $ \fields ->
  filter (\(key,_) -> key /= "receipt" && key /= "next_request") fields <>
    ["receipt" .= object receipt, "next_request" .= object ["name" .= ("attachment_list" :: Text),
      "arguments" .= object ["workspace_id" .= workspace, "task_id" .= task, "role" .= role]]]

findAssociation :: HTTPContext -> String -> [Header] -> UUID -> UUID -> UUID -> Doc.DocumentRole -> IO UUID
findAssociation manager base headers workspace task identifier role = go 0 Nothing
  where
    go pages cursor = do
      when (pages >= (20 :: Int)) (fault "association_refresh_required")
      let parameters = [("role",Just (TE.encodeUtf8 (Doc.roleText role))),("limit",Just "50")] <>
            maybe [] (\c -> [("before_created_at",Just (B8.pack (iso8601Show c.createdAt))),
              ("before_id",Just (TE.encodeUtf8 (UUID.toText c.id)))]) cursor
      request <- privateRequest base headers "GET" (collection workspace task <> B8.unpack (renderQuery True parameters))
      page <- jsonResponse manager request >>= decodeValue @Doc.DocumentPage
      ensure (length page.items <= 50)
      mapM_ (\link -> do
        ensure (link.taskId == task && link.role == role)
        validateDocument workspace link.document) page.items
      case filter (\link -> link.document.id == identifier) page.items of
        [link] -> pure link.id
        [] -> case page.nextCursor of
          Nothing -> fault "association_refresh_required"
          Just next -> do
            ensure (Just next /= cursor)
            go (pages+1) (Just next)
        _ -> fault "invalid_response"

snapshot :: File.Source -> Int64 -> IO ([ByteString], Text)
snapshot source expected = go 0 [] (hashInit :: Context SHA256)
  where
    go offset parts context
      | offset == expected = do
          File.checkSource source expected >>= \size -> unless (size == expected) (throwIO File.SourceChanged)
          pure (reverse parts, T.pack (show (hashFinalize context :: Digest SHA256)))
      | otherwise = do
          bytes <- File.readSource source offset (fromIntegral (min Doc.maxPartBytes (expected - offset)))
          when (BS.null bytes) (throwIO File.SourceTruncated)
          go (offset + fromIntegral (BS.length bytes)) (bytes:parts) (hashUpdate context bytes)

verifySource :: File.Source -> Int64 -> Text -> IO ()
verifySource source expected digest = do
  File.checkSource source expected >>= \size -> unless (size == expected) (throwIO File.SourceChanged)
  let loop offset context
        | offset == expected = do
            File.checkSource source expected >>= \size -> unless (size == expected) (throwIO File.SourceChanged)
            unless (T.pack (show (hashFinalize context :: Digest SHA256)) == digest) (throwIO File.SourceChanged)
        | otherwise = do
            bytes <- File.readSource source offset (fromIntegral (min Doc.maxPartBytes (expected - offset)))
            when (BS.null bytes) (throwIO File.SourceTruncated)
            loop (offset + fromIntegral (BS.length bytes)) (hashUpdate context bytes)
  loop 0 (hashInit :: Context SHA256)

asciiJSON :: Value -> ByteString
asciiJSON = TE.encodeUtf8 . T.concatMap escape . TE.decodeUtf8 . BL.toStrict . encode
  where
    unit n = T.pack ("\\u" <> replicate (4 - length digits) '0' <> digits) where digits = showHex n ""
    escape c
      | ord c < 128 = T.singleton c
      | ord c <= 65535 = unit (ord c)
      | otherwise = let n = ord c - 65536 in unit (55296 + div n 1024) <> unit (56320 + mod n 1024)

inferMediaType :: Text -> Text
inferMediaType name = case map toLower (takeExtension (T.unpack name)) of
  ".png" -> "image/png"; ".jpg" -> "image/jpeg"; ".jpeg" -> "image/jpeg"; ".webp" -> "image/webp"
  ".mp4" -> "video/mp4"; ".pdf" -> "application/pdf"; ".html" -> "text/html"; ".htm" -> "text/html"; ".svg" -> "image/svg+xml"
  ".txt" -> "text/plain"; ".md" -> "text/markdown"; ".markdown" -> "text/markdown"
  ".docx" -> "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
  ".pptx" -> "application/vnd.openxmlformats-officedocument.presentationml.presentation"
  ".xlsx" -> "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  ".fig" -> "application/x-figma"; ".sketch" -> "application/x-sketch"
  _ -> ""

collection :: UUID -> UUID -> String
collection workspace task = "/api/v1/workspaces/" <> show workspace <> "/tasks/" <> show task <> "/documents"
documentPath :: UUID -> UUID -> UUID -> String
documentPath workspace task identifier = collection workspace task <> "/" <> show identifier

privateRequest :: String -> [Header] -> ByteString -> String -> IO Request
privateRequest base headers verb suffix = do
  initial <- parseRequest (base <> suffix)
  pure initial {method = verb, requestHeaders = headers, redirectCount = 0,
    decompress = const False, cookieJar = Nothing, checkResponse = \_ _ -> pure (),
    responseTimeout = responseTimeoutMicro (30 * 1000000)}

jsonResponse :: HTTPContext -> Request -> IO Value
jsonResponse manager request = jsonResponseAcknowledged manager request (const (pure ()))

jsonResponseAcknowledged :: HTTPContext -> Request -> (Value -> IO ()) -> IO Value
jsonResponseAcknowledged manager request acknowledge = do
  parts <- requestBodyParts (requestBody request)
  jsonResponseParts manager request parts acknowledge

jsonResponseParts :: HTTPContext -> Request -> [ByteString] -> (Value -> IO ()) -> IO Value
jsonResponseParts manager request parts acknowledge = do
  (_,_,bytes) <- requestBytes manager request parts $ \status _ body -> do
    checkStatusBytes status body
    value <- either (const (fault "invalid_response")) pure (eitherDecodeStrict' body)
    acknowledge value
  either (const (fault "invalid_response")) pure (eitherDecodeStrict' bytes)

requestBodyParts :: RequestBody -> IO [ByteString]
requestBodyParts (RequestBodyBS bytes) = pure [bytes]
requestBodyParts (RequestBodyLBS bytes) = pure (BL.toChunks bytes)
requestBodyParts _ = fault "attachment_transport_protocol"

requestBytes :: HTTPContext -> Request -> [ByteString]
  -> (Int -> [(HeaderName,ByteString)] -> ByteString -> IO ())
  -> IO (Int,[(HeaderName,ByteString)],ByteString)
requestBytes manager request parts acknowledge = do
  response <- newIORef (0,[])
  pieces <- newIORef []
  complete <- newIORef Nothing
  HTTP.exchange manager.scope manager.deadline request parts (256*1024)
    (\status headers -> ensureUnencodedHeaders headers >> writeIORef response (status,headers))
    (\chunk -> modifyIORef' pieces (chunk:))
    (do
      (status,headers) <- readIORef response
      bytes <- BS.concat . reverse <$> readIORef pieces
      acknowledge status headers bytes
      writeIORef complete (Just (status,headers,bytes)))
  readIORef complete >>= maybe (fault "attachment_transport_protocol") pure

checkStatusBytes :: Int -> ByteString -> IO ()
checkStatusBytes status bytes = when (status < 200 || status >= 300) $ do
  let reported = either (const Nothing) (fieldText "error") (eitherDecodeStrict' bytes)
      allowed = ["unauthorized", "forbidden", "workspace_not_found", "task_not_found", "document_not_found",
        "document_request_conflict", "document_quota_exceeded", "document_task_link_limit", "document_size_exceeded",
        "document_request_timeout", "document_database_timeout", "document_transfer_timeout", "document_unavailable",
        "document_unsupported_or_mismatched_type", "preview_stale", "preview_unavailable",
        "credential_authority_unavailable", "grant_unavailable", "grant_not_found", "grant_revoked",
        "grant_expired", "preview_origin_unavailable", "local_credential_unavailable",
        "document_preview_stale", "document_preview_unavailable", "styled_preview_unavailable",
        "document_download_unavailable", "document_source_task_not_ready", "document_evidence_task_not_terminal",
        "document_invalid_association"]
      code = maybe "attachment_http_error" (\value -> if elem value allowed then value else "attachment_http_error") reported
  throwIO (Fault code (Just status) [])

ensureUnencodedHeaders :: [(HeaderName,ByteString)] -> IO ()
ensureUnencodedHeaders headers = ensure (all (\(name,value) -> name /= "Content-Encoding" || B8.map toLower value == "identity") headers)

decodeValue :: FromJSON a => Value -> IO a
decodeValue value = case fromJSON value of Error _ -> fault "invalid_response"; Success parsed -> pure parsed

jsonField :: Key.Key -> Value -> IO Value
jsonField key (Object value) = maybe (fault "invalid_response") pure (KM.lookup key value)
jsonField _ _ = fault "invalid_response"

fieldText :: Key.Key -> Value -> Maybe Text
fieldText key (Object value) = case KM.lookup key value of Just (String text) -> Just text; _ -> Nothing
fieldText _ _ = Nothing

validateDocument :: UUID -> Doc.Document -> IO ()
validateDocument workspace document = do
  ensure (document.workspaceId == workspace)
  validateManifest (Doc.OriginalManifest document.rawBytes document.rawSha256 document.filename document.title document.mediaType)

readDocument :: HTTPContext -> String -> [Header] -> UUID -> UUID -> UUID -> IO Doc.Document
readDocument manager base headers workspace task identifier = do
  request <- privateRequest base headers "GET" (documentPath workspace task identifier)
  document <- jsonResponse manager request >>= decodeValue
  validateDocument workspace document
  ensure (document.id == identifier)
  pure document

documentReceipt :: UUID -> UUID -> Doc.Document -> [Pair]
documentReceipt workspace task document =
  ["workspace_id" .= workspace, "task_id" .= task, "attachment_id" .= document.id,
   "filename" .= document.filename, "title" .= document.title, "media_type" .= document.mediaType,
   "raw_bytes" .= document.rawBytes, "raw_sha256" .= document.rawSha256,
   "created_at" .= document.createdAt, "preview_state" .= document.previewState, "preview_error" .= document.previewError]

readCounts :: HTTPContext -> String -> [Header] -> UUID -> UUID -> IO Doc.DocumentCounts
readCounts manager base headers workspace task = do
  request <- privateRequest base headers "GET" (collection workspace task <> "/counts")
  counts <- jsonResponse manager request >>= decodeValue
  ensure (counts.sourceCount >= 0 && counts.documentEvidenceCount >= 0 && counts.agentSessionEvidenceCount >= 0
    && toInteger counts.evidenceTotal == toInteger counts.documentEvidenceCount + toInteger counts.agentSessionEvidenceCount)
  pure counts

data Preview = Preview !Grant.PreviewStatus ![Pair]

readPreview :: HTTPContext -> String -> [Header] -> UUID -> UUID -> Doc.Document -> IO Preview
readPreview manager base headers workspace task document = do
  request <- privateRequest base headers "GET" (documentPath workspace task document.id <> "/previews/current")
  value <- jsonResponse manager request
  status <- decodeValue value
  ensure (elem status.state ["queued","processing","ready","unsupported","failed"] && length status.outputs <= 401)
  let keys = map (\output -> (output.variant,output.page)) status.outputs
  ensure (length (nub keys) == length keys)
  mapM_ validateOutput status.outputs
  ensure (sum (map (toInteger . (.rawBytes)) status.outputs) <= toInteger Doc.maxDerivativeBytes)
  when (status.state == "ready") (ensure (status.generationId /= Nothing && status.policyVersion /= Nothing))
  extras <- previewExtras document status value
  pure (Preview status extras)

previewExtras :: Doc.Document -> Grant.PreviewStatus -> Value -> IO [Pair]
previewExtras document status value = do
  fields <- case value of Object objectValue -> pure objectValue; _ -> fault "invalid_response"
  retryable <- case KM.lookup "retryable" fields of
    Nothing -> pure []
    Just (Bool flag) -> pure ["retryable" .= flag]
    _ -> fault "invalid_response"
  metadata <- case KM.lookup "generation_metadata" fields of
    Nothing -> pure []
    Just Null -> pure ["generation_metadata" .= Null]
    Just original@(Object _) -> case fieldText "verification" original of
      Just "unknown" -> pure ["generation_metadata" .= object ["verification" .= ("unknown" :: Text)]]
      Just "verified" -> do
        ensure (status.state == "ready" && status.generationId /= Nothing)
        hashes <- forM ["plan_sha256","original_sha256","limits_sha256"] $ \key -> do
          item <- jsonField key original >>= decodeValue @Text
          ensure (validHash item)
          pure (key,item)
        ensure (lookup "original_sha256" hashes == Just document.rawSha256)
        policy <- jsonField "policy_version" original >>= decodeValue @Text
        ensure (Just policy == status.policyVersion)
        image <- jsonField "image_id" original >>= decodeValue @Text
        ensure (maybe False validHash (T.stripPrefix "sha256:" image))
        outputCount <- jsonField "output_count" original >>= decodeValue @Int
        pageCount <- jsonField "page_count" original >>= decodeValue @Int
        pixels <- jsonField "decoded_pixels" original >>= decodeValue @Int64
        pdfBytes <- jsonField "pdf_decoded_bytes" original >>= decodeValue @Int64
        animated <- jsonField "animated_first_frame" original >>= decodeValue @Bool
        ensure (outputCount >= 1 && outputCount == length status.outputs && outputCount <= 401
          && pageCount >= 0 && pageCount <= 200
          && pageCount == (if document.mediaType == "application/pdf" then length (filter (\o -> o.variant == "page") status.outputs) else 0)
          && pixels >= 0 && pixels <= 40000000 && pdfBytes >= 0 && pdfBytes <= 2147483648)
        let safe = object (["verification" .= ("verified" :: Text),"policy_version" .= policy,"image_id" .= image,
              "output_count" .= outputCount,"page_count" .= pageCount,"decoded_pixels" .= pixels,
              "pdf_decoded_bytes" .= pdfBytes,"animated_first_frame" .= animated] <> map (\(key,item) -> key .= item) hashes)
        pure ["generation_metadata" .= safe,"animated_first_frame" .= animated]
      _ -> fault "invalid_response"
    _ -> fault "invalid_response"
  pure (retryable <> metadata)

validateOutput :: Grant.PreviewOutput -> IO ()
validateOutput output = ensure (output.page >= 0 && output.page <= 200
  && output.rawBytes >= 0 && output.rawBytes <= Doc.maxDerivativeBytes && validHash output.rawSha256
  && case output.variant of
    "styled_html_preview" -> output.page == 0 && output.mediaType == "text/html"
    "page" -> elem output.mediaType ["image/png","image/jpeg","image/webp"]
    "thumbnail" -> elem output.mediaType ["image/png","image/jpeg","image/webp"]
    _ -> False)

validHash :: Text -> Bool
validHash value = T.length value == 64 && T.all (\c -> elem c ("0123456789abcdef" :: String)) value

previewSummary :: Preview -> Value
previewSummary (Preview status extras) = object
  (["state" .= status.state, "error" .= status.errorCode, "generation_id" .= status.generationId,
    "policy_version" .= status.policyVersion, "page_count" .= length (filter (\o -> o.variant == "page") status.outputs),
    "variants" .= nub (map (.variant) status.outputs)] <> extras)

data Representation = Representation
  { variant :: !Text, page :: !Int, generation :: !(Maybe UUID), policy :: !(Maybe Text)
  , bytes :: !Int64, digest :: !Text, media :: !Text, filename :: !Text, originalDigest :: !Text
  } deriving (Show)

resolveRepresentation :: HTTPContext -> String -> [Header] -> UUID -> UUID -> Doc.Document -> Text -> Int -> Maybe UUID -> IO Representation
resolveRepresentation manager base headers workspace task document variant page generation
  | variant == "original" = pure (Representation variant page Nothing Nothing document.rawBytes document.rawSha256
      document.mediaType document.filename document.rawSha256)
  | otherwise = do
      preview@(Preview status _) <- readPreview manager base headers workspace task document
      unless (status.generationId == generation) (fault "preview_stale")
      unless (status.state == "ready") (throwIO (Fault "preview_unavailable" Nothing
        ["preview" .= previewSummary preview]))
      case filter (\output -> output.variant == variant && output.page == page) status.outputs of
        [output] -> pure (Representation variant page generation status.policyVersion output.rawBytes output.rawSha256
          output.mediaType document.filename document.rawSha256)
        _ -> fault "preview_representation_unavailable"

representationManifest :: Representation -> Value
representationManifest r = object ["raw_bytes" .= r.bytes, "raw_sha256" .= r.digest, "media_type" .= r.media,
  "filename" .= r.filename, "original_sha256" .= r.originalDigest]

representationPath :: UUID -> UUID -> UUID -> Representation -> String
representationPath workspace task identifier r = documentPath workspace task identifier <> case r.generation of
  Nothing -> "/original"
  Just generation -> "/previews/" <> show generation <> "/" <> T.unpack r.variant <> "/" <> show r.page

issueGrant :: HTTPContext -> String -> [Header] -> UUID -> UUID -> Doc.Document -> Representation -> Doc.DocumentRole -> UUID -> Int -> IO Value
issueGrant manager base headers workspace task document r role association lifetime = do
  initial <- privateRequest base headers "POST" (documentPath workspace task document.id <> "/download-grants")
  grant <- jsonResponse manager initial {requestHeaders = requestHeaders initial <> [("Content-Type", "application/json")],
    requestBody = RequestBodyLBS (encode (Grant.GrantRequest role association r.variant r.page r.generation (Just lifetime)))}
  let expected =
        [("workspace_id",toJSON workspace),("task_id",toJSON task),("document_id",toJSON document.id),
         ("role",toJSON role),("association_id",toJSON association),("variant",toJSON r.variant),("page",toJSON r.page),
         ("generation_id",toJSON r.generation),("policy_version",toJSON r.policy)]
  mapM_ (\(key,value) -> jsonField key grant >>= ensure . (==value)) expected
  manifest <- jsonField "manifest" grant
  mapM_ (\(key,value) -> jsonField key manifest >>= ensure . (==value))
    [("raw_bytes",toJSON r.bytes),("raw_sha256",toJSON r.digest),("media_type",toJSON r.media),
     ("filename",toJSON r.filename),("original_sha256",toJSON r.originalDigest)]
  identifier <- jsonField "grant_id" grant >>= decodeValue @UUID
  _ <- (jsonField "expires_at" grant >>= decodeValue) :: IO UTCTime
  url <- jsonField "url" grant >>= decodeValue
  request <- grantRequest url
  ensure (path request == B8.pack ("/document-grants/" <> show identifier <> "/" <> T.unpack r.variant <> "/" <> show r.page))
  pure $ case grant of
    Object fields -> Object (KM.insert "attachment_id" (toJSON document.id) (KM.delete "document_id" fields))
    _ -> grant

grantRequest :: Text -> IO Request
grantRequest url = do
  ensure (not (T.any (\c -> elem c ("\\\r\n#" :: String)) url))
  let authority = T.takeWhile (/='/') (snd (T.breakOnEnd "://" url))
  ensure (not (T.null authority) && not (T.any (=='@') authority))
  request <- parseRequest (T.unpack url)
  let local = elem (B8.map toLower (host request)) ["localhost","127.0.0.1","::1","[::1]"]
      query = parseQuery (queryString request)
      validToken token = BS.length token == 43 && B8.all (\c -> elem c ("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_" :: String)) token
  ensure (secure request || local)
  ensure (port request > 0 && port request <= 65535)
  ensure (case query of [("token",Just token)] -> validToken token; _ -> False)
  pure request {method = "GET", requestHeaders = [("Accept-Encoding","identity")], cookieJar = Nothing,
    redirectCount = 0, decompress = const False, checkResponse = \_ _ -> pure (),
    responseTimeout = responseTimeoutMicro (30 * 1000000)}

downloadTo :: HTTPContext -> Request -> Representation -> FilePath -> IO ()
downloadTo manager request r destination = do
  when (null destination || elem '\0' destination) (fault "invalid_destination")
  existing <- doesPathExist destination
  when existing (fault "destination_exists")
  let cleanup (temporary,handle) = (do
        closed <- hIsClosed handle
        unless closed (hClose handle)
        remains <- doesPathExist temporary
        when remains (removeFile temporary)) `catch` \(_ :: IOException) ->
          throwIO (Fault "attachment_cleanup_failed" Nothing ["partial_path" .= temporary])
  bracket (openBinaryTempFile (takeDirectory destination) ".hmem-attachment-") cleanup $ \(temporary,handle) -> do
    statusRef <- newIORef 0
    errors <- newIORef []
    total <- newIORef (0 :: Int64)
    context <- newIORef (hashInit :: Context SHA256)
    HTTP.exchange manager.scope manager.deadline request [] Doc.maxDerivativeBytes
      (\status headers -> do
        writeIORef statusRef status
        ensureUnencodedHeaders headers
        when (status >= 200 && status < 300) $ do
          ensure (status == 200)
          validateDownloadHeaders headers r)
      (\chunk -> do
        status <- readIORef statusRef
        if status < 200 || status >= 300 then modifyIORef' errors (chunk:)
        else do
          count <- readIORef total
          let next = count + fromIntegral (BS.length chunk)
          when (next > r.bytes) (fault "download_manifest_mismatch")
          mapM_ (BS.hPut handle) (splitBytes chunk)
          writeIORef total next
          modifyIORef' context (`hashUpdate` chunk))
      (do
        status <- readIORef statusRef
        if status < 200 || status >= 300 then BS.concat . reverse <$> readIORef errors >>= checkStatusBytes status
        else do
          count <- readIORef total
          digest <- T.pack . show . (hashFinalize :: Context SHA256 -> Digest SHA256) <$> readIORef context
          ensure (count == r.bytes && digest == r.digest))
    hFlush handle
    mask_ $ do
      hClose handle
      published <- File.publishNewFile temporary destination
      unless published $ do
        nowExists <- doesPathExist destination
        fault (if nowExists then "destination_exists" else "publication_failed")

splitBytes :: ByteString -> [ByteString]
splitBytes bytes
  | BS.null bytes = []
  | otherwise = let (part,rest) = BS.splitAt (fromIntegral Doc.maxPartBytes) bytes in part:splitBytes rest

oneHeader :: HeaderName -> [(HeaderName,ByteString)] -> IO ByteString
oneHeader name response = case [value | (key,value) <- response,key == name] of
  [value] -> pure value
  _ -> fault "invalid_response"

validateDownloadHeaders :: [(HeaderName,ByteString)] -> Representation -> IO ()
validateDownloadHeaders response r = do
  count <- oneHeader "Content-Length" response
  ensure (readMaybe (B8.unpack count) == Just r.bytes)
  digest <- oneHeader "X-HMem-Document-SHA256" response
  bytes <- oneHeader "X-HMem-Document-Bytes" response
  ensure (digest == TE.encodeUtf8 r.digest && readMaybe (B8.unpack bytes) == Just r.bytes)
  media <- oneHeader "Content-Type" response
  ensure (B8.map toLower (B8.takeWhile (/=';') media) ==
    (if r.variant == "original" then "application/octet-stream" else TE.encodeUtf8 r.media))
  when (r.variant == "original") $ do
    disposition <- oneHeader "Content-Disposition" response
    ensure (BS.isPrefixOf "attachment;" disposition)
  when (r.variant == "styled_html_preview") $ do
    ensure (all (\(key,_) -> key /= "Set-Cookie") (response))
    csp <- oneHeader "Content-Security-Policy" response
    let directives = [(B8.map toLower name,values) | name:values <- map B8.words (B8.split ';' csp)]
        fixed = [("sandbox",[]),("default-src",["'none'"]),("script-src",["'none'"]),
          ("style-src",["'unsafe-inline'"]),("img-src",["data:"]),("font-src",["'none'"]),
          ("connect-src",["'none'"]),("frame-src",["'none'"]),("child-src",["'none'"]),
          ("worker-src",["'none'"]),("object-src",["'none'"]),("media-src",["'none'"]),
          ("base-uri",["'none'"]),("form-action",["'none'"])]
    ensure (length (nub (map fst directives)) == length directives
      && length directives == length fixed + 1
      && all (\(name,values) -> lookup name directives == Just values) fixed)
    ancestor <- case lookup "frame-ancestors" directives of
      Just [origin] -> pure origin
      _ -> fault "invalid_response"
    ensure (not (B8.any (\c -> elem c ("@\\#*\r\n" :: String)) ancestor))
    origin <- parseRequest (B8.unpack ancestor)
    ensure ((secure origin || elem (B8.map toLower (host origin)) ["localhost","127.0.0.1","::1","[::1]"])
      && path origin == "/" && BS.null (queryString origin))
    ensure (B8.filter (/=' ') (B8.map toLower media) == "text/html;charset=utf-8")
    oneHeader "X-Content-Type-Options" response >>= ensure . (=="nosniff")
    oneHeader "Referrer-Policy" response >>= ensure . (=="no-referrer")
    cache <- oneHeader "Cache-Control" response
    ensure (elem "no-store" (map (B8.dropWhile (==' ') . B8.reverse . B8.dropWhile (==' ') . B8.reverse . B8.map toLower) (B8.split ',' cache)))
