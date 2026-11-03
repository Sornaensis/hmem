-- | PostgreSQL document authority. Callers must perform access checks before
-- body consumption; this layer also enforces workspace/task/link integrity.
module HMem.DB.Document
  ( reserveUpload, publishUpload, uploadDocument, abortReservation, expireReservations
  , getDocument, listDocuments, getTaskDocumentCounts, readOriginal
  , addEvidence, removeSource, reservePreview, publishPreview, failPreview
  , Derivative(..), readDerivative
  ) where

import Control.Exception (throwIO)
import Control.Monad (forM, forM_, unless, when)
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Functor.Contravariant (contramap)
import Data.Int (Int32, Int64)
import Data.Pool (Pool)
import Data.Text (Text)
import Data.Text qualified as T
import Data.UUID (UUID)
import Data.UUID.V4 qualified as UUID
import Hasql.Connection qualified as H
import Hasql.Decoders qualified as D
import Hasql.Encoders qualified as E
import Hasql.Session qualified as S
import Hasql.Statement qualified as St
import HMem.DB.Pool (runSession,runTransaction)
import HMem.Document.Codec
import HMem.Document.Types

param :: (a -> b) -> E.Value b -> E.Params a
param f v = contramap f (E.param (E.nonNullable v))
optional :: (a -> Maybe b) -> E.Value b -> E.Params a
optional f v = contramap f (E.param (E.nullable v))
pair :: E.Params (UUID,UUID)
pair = param fst E.uuid <> param snd E.uuid
triple :: E.Params (UUID,UUID,UUID)
triple = param (\(a,_,_)->a) E.uuid <> param (\(_,b,_)->b) E.uuid <> param (\(_,_,c)->c) E.uuid
jsonRow :: FromJSON a => D.Row a
jsonRow = D.column (D.nonNullable (D.refine parse D.jsonb))
  where parse v = case fromJSON v of Success a -> Right a; Error e -> Left (T.pack e)
voidStatement :: ByteString -> E.Params a -> St.Statement a ()
voidStatement sql encoder = St.Statement sql encoder D.noResult True
problem :: Text -> IO a
problem = throwIO . DocumentException

binding :: UploadDocument -> Value
binding = toJSON

checkTask :: UUID -> UUID -> S.Session ()
checkTask ws task = S.statement (ws,task) (voidStatement "SELECT hmem_document_check_task($1,$2)" pair)
lockBudget :: UUID -> S.Session ()
lockBudget ws = S.statement ws (voidStatement "SELECT workspace_id FROM document_budgets WHERE workspace_id=$1 FOR UPDATE" (param id E.uuid))

-- | Returns an exact already-published retry. Otherwise reserves before body
-- consumption; no bytes, document or task link is visible yet.
reserveUpload :: Pool H.Connection -> UploadDocument -> IO (Maybe Document)
reserveUpload pool input = do
  validateManifest input.manifest
  runTransaction pool $ do
    checkTask input.workspaceId input.taskId
    S.statement input.workspaceId (voidStatement "INSERT INTO document_budgets(workspace_id) VALUES($1) ON CONFLICT DO NOTHING" (param id E.uuid))
    lockBudget input.workspaceId
    previous <- existingRequest input.workspaceId input.requestId (binding input)
    case previous of
      Just row -> pure (Just row)
      Nothing -> do
        S.statement input (voidStatement
          "SELECT hmem_document_reserve($1,$2,'upload',$3,CASE WHEN EXISTS(SELECT 1 FROM document_payloads WHERE workspace_id=$1 AND original AND published_at IS NOT NULL AND raw_sha256=$4 AND raw_bytes=$5) THEN 0 ELSE $5 END,$6)"
          (param (.workspaceId) E.uuid <> param (.requestId) E.uuid <> param binding E.jsonb <> param (\i -> i.manifest.rawSha256) E.text <> param (\i -> i.manifest.rawBytes) E.int8 <> param (\i -> uploadReservationBytes i.manifest.rawBytes) E.int8))
        pure Nothing

existingRequest :: UUID -> UUID -> Value -> S.Session (Maybe Document)
existingRequest ws request expected = S.statement (ws,request,expected) $ St.Statement
  "SELECT CASE WHEN r.binding=$3 THEN hmem_document_json(d) ELSE hmem_document_request_conflict() END FROM document_requests r JOIN documents d ON d.id=r.document_id WHERE r.workspace_id=$1 AND r.request_id=$2"
  (param (\(a,_,_)->a) E.uuid <> param (\(_,b,_)->b) E.uuid <> param (\(_,_,c)->c) E.jsonb) (D.rowMaybe jsonRow) True

uploadDocument :: Pool H.Connection -> UploadDocument -> ByteString -> IO Document
uploadDocument pool input bytes = do
  validateOriginal input.manifest bytes
  reserveUpload pool input >>= \case
    Just row -> pure row
    Nothing -> publishUpload pool input bytes

-- | Publication verifies the bounded whole file and both gzip manifests. The
-- workspace lock makes payload dedup, actual accounting and link publication
-- one transaction. An exception rolls back every payload part and link.
publishUpload :: Pool H.Connection -> UploadDocument -> ByteString -> IO Document
publishUpload pool input bytes = do
  validateOriginal input.manifest bytes
  (manifest,parts) <- encodePayload maxOriginalBytes bytes
  payload <- UUID.nextRandom
  identifier <- UUID.nextRandom
  association <- UUID.nextRandom
  runTransaction pool $ do
    lockBudget input.workspaceId
    checkTask input.workspaceId input.taskId
    previous <- existingRequest input.workspaceId input.requestId (binding input)
    case previous of
      Just row -> pure row
      Nothing -> do
        S.statement (input.workspaceId,input.requestId,binding input) (voidStatement "SELECT hmem_document_require_reservation($1,$2,$3)"
          (param (\(a,_,_)->a) E.uuid <> param (\(_,b,_)->b) E.uuid <> param (\(_,_,c)->c) E.jsonb))
        stored <- storePayload input.workspaceId payload True manifest parts
        S.statement (input.workspaceId,input.requestId,stored,identifier,association) $ St.Statement
          "SELECT hmem_document_publish($1,$2,$3,$4,$5)"
          (param (\(a,_,_,_,_)->a) E.uuid <> param (\(_,b,_,_,_)->b) E.uuid <> param (\(_,_,c,_,_)->c) E.uuid <> param (\(_,_,_,d,_)->d) E.uuid <> param (\(_,_,_,_,e)->e) E.uuid)
          (D.singleRow jsonRow) True

storePayload :: UUID -> UUID -> Bool -> PayloadManifest -> [ByteString] -> S.Session UUID
storePayload ws identifier original manifest parts = do
  inserted <- S.statement (ws,identifier,original,toJSON manifest) $ St.Statement
    "INSERT INTO document_payloads(id,workspace_id,original,codec,raw_bytes,raw_sha256,compressed_bytes,compressed_sha256,part_count) VALUES($2,$1,$3,$4->>'codec',($4->>'raw_bytes')::bigint,$4->>'raw_sha256',($4->>'compressed_bytes')::bigint,$4->>'compressed_sha256',($4->>'part_count')::int) ON CONFLICT(workspace_id,raw_sha256,raw_bytes,original) DO NOTHING RETURNING id"
    (param (\(a,_,_,_)->a) E.uuid <> param (\(_,b,_,_)->b) E.uuid <> param (\(_,_,c,_)->c) E.bool <> param (\(_,_,_,d)->d) E.jsonb)
    (D.rowMaybe (D.column (D.nonNullable D.uuid))) True
  case inserted of
    Just value -> do
      forM_ (zip [0::Int32 ..] parts) $ \(ordinal,bytes) -> S.statement (ws,value,ordinal,bytes) (voidStatement
        "INSERT INTO document_payload_parts(workspace_id,payload_id,ordinal,data) VALUES($1,$2,$3,$4)"
        (param (\(a,_,_,_)->a) E.uuid <> param (\(_,b,_,_)->b) E.uuid <> param (\(_,_,c,_)->c) E.int4 <> param (\(_,_,_,d)->d) E.bytea))
      pure value
    Nothing -> S.statement (ws,manifest.rawSha256,manifest.rawBytes,original) $ St.Statement
      "SELECT id FROM document_payloads WHERE workspace_id=$1 AND raw_sha256=$2 AND raw_bytes=$3 AND original=$4 AND published_at IS NOT NULL"
      (param (\(a,_,_,_)->a) E.uuid <> param (\(_,b,_,_)->b) E.text <> param (\(_,_,c,_)->c) E.int8 <> param (\(_,_,_,d)->d) E.bool)
      (D.singleRow (D.column (D.nonNullable D.uuid))) True

abortReservation :: Pool H.Connection -> UUID -> UUID -> IO ()
abortReservation pool ws identifier = runTransaction pool $ do
  lockBudget ws
  S.statement (ws,identifier) (voidStatement "DELETE FROM document_reservations WHERE workspace_id=$1 AND id=$2" pair)

expireReservations :: Pool H.Connection -> UUID -> IO ()
expireReservations pool ws = runTransaction pool $ do
  lockBudget ws
  S.statement ws (voidStatement "DELETE FROM document_reservations WHERE workspace_id=$1 AND expires_at<=clock_timestamp()" (param id E.uuid))

getDocument :: Pool H.Connection -> UUID -> UUID -> UUID -> IO (Maybe Document)
getDocument pool ws task identifier = runSession pool $ S.statement (ws,task,identifier) $ St.Statement
  ("SELECT hmem_document_json(d) FROM documents d WHERE d.workspace_id=$1 AND d.id=$3 AND "<>visibleLink)
  triple (D.rowMaybe jsonRow) True

visibleLink :: ByteString
visibleLink = "EXISTS(SELECT 1 FROM tasks t JOIN workspaces w ON w.id=t.workspace_id WHERE t.id=$2 AND t.workspace_id=$1 AND t.deleted_at IS NULL AND w.deleted_at IS NULL AND (EXISTS(SELECT 1 FROM task_document_sources l WHERE l.task_id=t.id AND l.document_id=d.id) OR EXISTS(SELECT 1 FROM task_document_evidence l WHERE l.task_id=t.id AND l.document_id=d.id)))"

listDocuments :: Pool H.Connection -> UUID -> UUID -> DocumentRole -> Maybe DocumentCursor -> Int -> IO DocumentPage
listDocuments pool ws task selected cursor limit = do
  when (limit<1 || limit>50) (problem "document_page_limit")
  rows <- runSession pool $ do
    checkTask ws task
    S.statement (ws,task,roleText selected,fmap (.createdAt) cursor,fmap (.id) cursor,fromIntegral(limit+1)::Int32) $ St.Statement
      "WITH links AS (SELECT id,workspace_id,task_id,document_id,created_at,'source'::text AS role FROM task_document_sources WHERE $3='source' UNION ALL SELECT id,workspace_id,task_id,document_id,created_at,'evidence' FROM task_document_evidence WHERE $3='evidence') SELECT jsonb_build_object('id',l.id,'task_id',l.task_id,'role',l.role,'created_at',l.created_at,'document',hmem_document_json(d)) FROM links l JOIN documents d ON d.id=l.document_id WHERE l.workspace_id=$1 AND l.task_id=$2 AND ($4::timestamptz IS NULL OR (l.created_at,l.id)<($4,$5::uuid)) ORDER BY l.created_at DESC,l.id DESC LIMIT $6"
      (param (\(a,_,_,_,_,_)->a) E.uuid <> param (\(_,b,_,_,_,_)->b) E.uuid <> param (\(_,_,c,_,_,_)->c) E.text <> optional (\(_,_,_,d,_,_)->d) E.timestamptz <> optional (\(_,_,_,_,e,_)->e) E.uuid <> param (\(_,_,_,_,_,f)->f) E.int4)
      (D.rowList jsonRow) True
  let items=take limit rows
      nextCursor=if length rows>limit then case reverse items of a:_ -> Just(DocumentCursor a.createdAt a.id); _ -> Nothing else Nothing
  pure DocumentPage{..}

getTaskDocumentCounts :: Pool H.Connection -> UUID -> UUID -> IO (Maybe DocumentCounts)
getTaskDocumentCounts pool ws task = runSession pool $ S.statement (ws,task) $ St.Statement
  "SELECT jsonb_build_object('source_count',s.n,'document_evidence_count',d.n,'agent_session_evidence_count',a.n,'evidence_total',d.n+a.n) FROM tasks t JOIN workspaces w ON w.id=t.workspace_id CROSS JOIN LATERAL(SELECT count(*) n FROM task_document_sources WHERE task_id=t.id AND workspace_id=t.workspace_id) s CROSS JOIN LATERAL(SELECT count(DISTINCT id) n FROM task_document_evidence WHERE task_id=t.id AND workspace_id=t.workspace_id) d CROSS JOIN LATERAL(SELECT count(DISTINCT x.session_id) n FROM task_sessions x JOIN sessions z ON z.id=x.session_id AND z.workspace_id=x.workspace_id WHERE x.task_id=t.id AND x.workspace_id=t.workspace_id AND z.state='terminated') a WHERE t.workspace_id=$1 AND t.id=$2 AND t.deleted_at IS NULL AND w.deleted_at IS NULL"
  pair (D.rowMaybe jsonRow) True

readPayload :: Pool H.Connection -> UUID -> UUID -> Int64 -> IO ByteString
readPayload pool ws identifier limit = do
  (manifest,parts) <- runSession pool $ do
    manifest <- S.statement (ws,identifier) $ St.Statement
      "SELECT jsonb_build_object('codec',codec,'raw_bytes',raw_bytes,'raw_sha256',raw_sha256,'compressed_bytes',compressed_bytes,'compressed_sha256',compressed_sha256,'part_count',part_count) FROM document_payloads WHERE workspace_id=$1 AND id=$2 AND published_at IS NOT NULL"
      pair (D.singleRow jsonRow) True
    parts <- S.statement (ws,identifier) $ St.Statement "SELECT data FROM document_payload_parts WHERE workspace_id=$1 AND payload_id=$2 ORDER BY ordinal" pair (D.rowList(D.column(D.nonNullable D.bytea))) True
    pure(manifest,parts)
  decodePayload limit manifest parts

readOriginal :: Pool H.Connection -> UUID -> UUID -> UUID -> IO ByteString
readOriginal pool ws task identifier = getDocument pool ws task identifier >>= \case
  Nothing -> problem "document_not_found"
  Just document -> readPayload pool ws document.payloadId maxOriginalBytes

addEvidence :: Pool H.Connection -> UUID -> UUID -> UUID -> UUID -> IO Document
addEvidence pool ws task identifier request = do
  association <- UUID.nextRandom
  runTransaction pool $ do
    lockBudget ws
    checkTask ws task
    S.statement (ws,task,identifier,request,association) $ St.Statement "SELECT hmem_document_add_evidence($1,$2,$3,$4,$5)"
      (param (\(a,_,_,_,_)->a) E.uuid <> param (\(_,b,_,_,_)->b) E.uuid <> param (\(_,_,c,_,_)->c) E.uuid <> param (\(_,_,_,d,_)->d) E.uuid <> param (\(_,_,_,_,e)->e) E.uuid)
      (D.singleRow jsonRow) True

removeSource :: Pool H.Connection -> UUID -> UUID -> UUID -> IO ()
removeSource pool ws task identifier = runTransaction pool $ do
  lockBudget ws
  checkTask ws task
  S.statement (ws,task,identifier) (voidStatement "SELECT hmem_document_remove_source($1,$2,$3)" triple)

-- One reservation covers a complete bounded renderer generation, before work
-- starts. Failed/expired work owns no published derivative bytes.
reservePreview :: Pool H.Connection -> UUID -> UUID -> UUID -> Text -> Int -> Int64 -> IO ()
reservePreview pool ws document generation policy outputs budget = do
  when (T.null policy || T.length policy>128 || outputs<1 || outputs>401 || budget<0 || budget>maxDerivativeBytes+1048576) (problem "document_preview_bound")
  runTransaction pool $ S.statement (ws,document,generation,policy,outputs::Int,budget) (voidStatement
    "SELECT hmem_document_reserve_preview($1,$2,$3,$4,$5,$6)"
    (param (\(a,_,_,_,_,_)->a) E.uuid <> param (\(_,b,_,_,_,_)->b) E.uuid <> param (\(_,_,c,_,_,_)->c) E.uuid <> param (\(_,_,_,d,_,_)->d) E.text <> param (\(_,_,_,_,e,_)->fromIntegral e::Int32) E.int4 <> param (\(_,_,_,_,_,f)->f) E.int8))

data Derivative = Derivative { variant :: !Text, page :: !Int, mediaType :: !Text, bytes :: !ByteString }

publishPreview :: Pool H.Connection -> UUID -> UUID -> UUID -> Text -> [Derivative] -> IO ()
publishPreview pool ws document generation policy outputs = do
  when (null outputs || length outputs>401 || sum(map (fromIntegral . BS.length . (.bytes)) outputs)>maxDerivativeBytes) (problem "document_preview_bound")
  encoded <- forM outputs $ \output -> do
    unless (output.variant `elem` ["thumbnail","page","styled_html_preview"] && output.page>=0 && output.page<=200 && output.mediaType `elem` ["image/png","image/jpeg","image/webp","text/html"]) (problem "document_preview_representation")
    identifier <- UUID.nextRandom
    (manifest,parts) <- encodePayload maxDerivativeBytes output.bytes
    pure(output,identifier,manifest,parts)
  runTransaction pool $ do
    lockBudget ws
    descriptors <- forM encoded $ \(output,identifier,manifest,parts) -> do
      stored <- storePayload ws identifier False manifest parts
      S.statement (stored,ws) (voidStatement "SELECT hmem_document_validate_payload($1,$2)" pair)
      pure(object["variant" .= output.variant,"page" .= output.page,"media_type" .= output.mediaType,"payload_id" .= stored,"manifest" .= manifest])
    S.statement (ws,document,generation,policy,toJSON descriptors) (voidStatement "SELECT hmem_document_publish_preview($1,$2,$3,$4,$5)"
      (param (\(a,_,_,_,_)->a) E.uuid <> param (\(_,b,_,_,_)->b) E.uuid <> param (\(_,_,c,_,_)->c) E.uuid <> param (\(_,_,_,d,_)->d) E.text <> param (\(_,_,_,_,e)->e) E.jsonb))

-- Nothing is the still-unclaimed queued state (e.g. admission saturation).
-- A delayed worker may only fail its own active processing attempt.
failPreview :: Pool H.Connection -> UUID -> UUID -> Maybe UUID -> Text -> IO Bool
failPreview pool ws document attempt code = do
  when (T.length code>128) (problem "document_preview_error_bound")
  runTransaction pool $ do
    lockBudget ws
    updated <- S.statement (ws,document,attempt,code) $ St.Statement
      "UPDATE document_previews SET state='failed',error_code=$4 WHERE workspace_id=$1 AND document_id=$2 AND attempt_id IS NOT DISTINCT FROM $3::uuid AND ((state='queued' AND $3::uuid IS NULL) OR (state='processing' AND $3::uuid IS NOT NULL)) RETURNING true"
      (param (\(a,_,_,_)->a) E.uuid <> param (\(_,b,_,_)->b) E.uuid <> optional (\(_,_,c,_)->c) E.uuid <> param (\(_,_,_,d)->d) E.text)
      (D.rowMaybe(D.column(D.nonNullable D.bool))) True
    when (updated==Just True) $ forM_ attempt $ \identifier ->
      S.statement (ws,identifier) (voidStatement "DELETE FROM document_reservations WHERE workspace_id=$1 AND id=$2 AND kind='preview'" pair)
    pure(updated==Just True)

readDerivative :: Pool H.Connection -> UUID -> UUID -> UUID -> Text -> Int -> IO (Value,ByteString)
readDerivative pool ws task document variant page = do
  getDocument pool ws task document >>= \case Nothing -> problem "document_not_found"; Just _ -> pure ()
  descriptor <- runSession pool $ S.statement (ws,document,variant,fromIntegral page::Int32) $ St.Statement
    "SELECT value FROM document_previews p JOIN document_generations g ON g.id=p.generation_id CROSS JOIN LATERAL jsonb_array_elements(g.outputs) WHERE p.workspace_id=$1 AND p.document_id=$2 AND p.state='ready' AND value->>'variant'=$3 AND (value->>'page')::int=$4"
    (param (\(a,_,_,_)->a) E.uuid <> param (\(_,b,_,_)->b) E.uuid <> param (\(_,_,c,_)->c) E.text <> param (\(_,_,_,d)->d) E.int4) (D.rowMaybe jsonRow) True
  case descriptor of
    Nothing -> problem "document_preview_not_ready"
    Just value -> case parseMaybe (withObject "derivative" (.: "payload_id")) value of
      Nothing -> problem "document_invalid_derivative"
      Just payload -> do bytes <- readPayload pool ws payload maxDerivativeBytes; pure(value,bytes)
