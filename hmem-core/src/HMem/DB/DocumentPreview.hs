-- | Durable document jobs; authorization remains in the shared HTTP boundary.
module HMem.DB.DocumentPreview
  ( heartbeat,nextJob,claim,started,inspected,reserve,rendering,closed,failRun,publish
  , retry,unavailable,recoverDue,readOriginal ) where

import Control.Exception (throwIO)
import Control.Monad (forM_,unless,when)
import Data.Aeson
import Data.ByteString (ByteString)
import Data.Functor.Contravariant (contramap)
import Data.Int (Int32,Int64)
import Data.Pool (Pool)
import Data.Text (Text)
import Data.Time (getCurrentTime,diffUTCTime)
import Data.UUID (UUID)
import Hasql.Connection qualified as H
import Hasql.Decoders qualified as D
import Hasql.Encoders qualified as E
import Hasql.Session qualified as S
import Hasql.Statement qualified as St
import System.Timeout (timeout)
import HMem.DB.Document qualified as Document
import HMem.DB.Pool (runSession,runTransaction)
import HMem.Document.Codec (payloadSha256)
import HMem.Document.Preview
import HMem.Document.Types (DocumentException(..))

param :: (a->b) -> E.Value b -> E.Params a
param project value=contramap project(E.param(E.nonNullable value))
pair :: E.Params(UUID,UUID)
pair=param fst E.uuid<>param snd E.uuid
voidStatement :: ByteString -> E.Params a -> St.Statement a ()
voidStatement sql encode=St.Statement sql encode D.noResult True
failure :: Text -> IO a
failure=throwIO . DocumentException
jsonValue :: FromJSON a => Value -> IO a
jsonValue value=case fromJSON value of Success decoded->pure decoded; Error _->failure "document_preview_record_invalid"

heartbeat :: Pool H.Connection -> PreviewConfig -> IO()
heartbeat pool config=do
  unless(validPreviewConfig config &&config.enabled &&config.launcherPath/=Nothing)(failure "renderer_unavailable")
  runTransaction pool $ S.statement config(voidStatement
    "SELECT hmem_document_preview_heartbeat($1,$2,$3,$4,$5)"
    (param (\c->fromIntegral c.workers::Int32)E.int4<>param(\c->fromIntegral c.queueLimit::Int32)E.int4
      <>param(const previewPolicyVersion)E.text<>param(.imageId)E.text
      <>param(payloadSha256 . canonicalPreviewLimits . (.limits))E.text))

nextJob :: Pool H.Connection -> IO(Maybe(UUID,UUID,Text))
nextJob pool=runSession pool $ S.statement ()(St.Statement
  "SELECT j.workspace_id,j.document_id,d.media_type FROM document_preview_jobs j JOIN document_previews p USING(document_id) JOIN documents d ON d.id=j.document_id WHERE j.state='queued' AND j.available_at<=clock_timestamp() AND p.state='queued' AND p.attempt_id IS NULL ORDER BY j.available_at,j.document_id LIMIT 1"
  E.noParams(D.rowMaybe((,,) <$>D.column(D.nonNullable D.uuid)<*>D.column(D.nonNullable D.uuid)<*>D.column(D.nonNullable D.text)))True)

claim :: Pool H.Connection -> PreviewConfig -> UUID -> UUID -> UUID -> UUID -> UUID -> Text -> IO(Maybe PreviewRun)
claim pool config workspace document owner controller token media=do
  unless(validPreviewConfig config)(failure "document_preview_config_invalid")
  value<-runTransaction pool $ S.statement (workspace,document,owner,controller,token,config,media)(St.Statement
    "SELECT hmem_document_preview_claim($1,$2,$3,$4,$5,$6,$7,$8,$9,$10,$11)"
    (param(\(a,_,_,_,_,_,_)->a)E.uuid<>param(\(_,b,_,_,_,_,_)->b)E.uuid
      <>param(\(_,_,c,_,_,_,_)->c)E.uuid<>param(\(_,_,_,d,_,_,_)->d)E.uuid<>param(\(_,_,_,_,e,_,_)->e)E.uuid
      <>param(const previewPolicyVersion)E.text<>param(\(_,_,_,_,_,c,_)->c.imageId)E.text
      <>param(\(_,_,_,_,_,c,_)->toJSON c.limits)E.jsonb
      <>param(\(_,_,_,_,_,c,_)->payloadSha256(canonicalPreviewLimits c.limits))E.text
      <>param(\(_,_,_,_,_,c,_)->fromIntegral c.maxAttempts::Int32)E.int4
      <>param(\(_,_,_,_,_,c,m)->fromIntegral(if m=="application/pdf" then c.pdfSeconds else c.imageSeconds)::Int32)E.int4)
    (D.singleRow(D.column(D.nullable D.jsonb)))True)
  traverse jsonValue value

runPair :: PreviewRun -> (UUID,UUID)
runPair run=(run.id,run.leaseToken)
started :: Pool H.Connection -> PreviewRun -> Int64 -> IO()
started pool run pid=do
  when(pid<=0)(failure "document_preview_native_identity")
  runTransaction pool $ S.statement(run.id,run.leaseToken,pid)(voidStatement
    "SELECT hmem_document_preview_started($1,$2,$3)"
    (param(\(a,_,_)->a)E.uuid<>param(\(_,b,_)->b)E.uuid<>param(\(_,_,c)->c)E.int8))
inspected :: Pool H.Connection -> PreviewRun -> IO()
inspected pool run=runTransaction pool $ S.statement(runPair run)(voidStatement "SELECT hmem_document_preview_inspected($1,$2)" pair)
reserve :: Pool H.Connection -> PreviewRun -> Text -> Int -> Int -> Int64 -> IO()
reserve pool run plan outputs pages budget=do
  unless(validHash plan &&outputs>=1&&outputs<=401&&pages>=0&&pages<=200&&budget>=0&&budget<=105906176)
    (failure "document_preview_admission_bound")
  runTransaction pool $ S.statement(run.id,run.leaseToken,plan,outputs,pages,budget)(voidStatement
    "SELECT hmem_document_preview_reserve_run($1,$2,$3,$4,$5,$6)"
    (param(\(a,_,_,_,_,_)->a)E.uuid<>param(\(_,b,_,_,_,_)->b)E.uuid<>param(\(_,_,c,_,_,_)->c)E.text
      <>param(\(_,_,_,d,_,_)->fromIntegral d::Int32)E.int4<>param(\(_,_,_,_,e,_)->fromIntegral e::Int32)E.int4
      <>param(\(_,_,_,_,_,f)->f)E.int8))
rendering :: Pool H.Connection -> PreviewRun -> IO()
rendering pool run=runTransaction pool $ S.statement(runPair run)(voidStatement "SELECT hmem_document_preview_rendering($1,$2)" pair)

-- The caller owns an actual joined native process before attesting its exit.
-- Unjoined producers use failRun directly, preserving unknown ownership.
closed :: Pool H.Connection -> PreviewRun -> Bool -> Bool -> IO()
closed pool run verified uncertain=runTransaction pool $ S.statement(run.id,run.leaseToken,verified,uncertain)(voidStatement
  "SELECT hmem_document_preview_closed($1,$2,$3,$4)"
  (param(\(a,_,_,_)->a)E.uuid<>param(\(_,b,_,_)->b)E.uuid<>param(\(_,_,c,_)->c)E.bool<>param(\(_,_,_,d)->d)E.bool))

failRun :: Pool H.Connection -> PreviewRun -> Text -> Bool -> IO Bool
failRun pool run code automatic=runTransaction pool $ S.statement(run.id,run.leaseToken,code,automatic)(St.Statement
  "SELECT hmem_document_preview_fail($1,$2,$3,$4)"
  (param(\(a,_,_,_)->a)E.uuid<>param(\(_,b,_,_)->b)E.uuid<>param(\(_,_,c,_)->c)E.text<>param(\(_,_,_,d)->d)E.bool)
  (D.singleRow(D.column(D.nonNullable D.bool)))True)

publish :: Pool H.Connection -> PreviewRun -> PreviewMetadata -> [Document.Derivative] -> IO()
publish pool run metadata outputs=do
  unless(validPreviewMetadata metadata &&metadata.originalSha256==run.rawSha256
    &&metadata.policyVersion==run.policyVersion &&metadata.imageId==run.imageId
    &&metadata.limitsSha256==payloadSha256(canonicalPreviewLimits run.limits)
    &&metadata.outputCount==length outputs)(failure "document_preview_publication_receipt")
  now<-getCurrentTime
  let remainingInteger=floor(diffUTCTime run.deadlineAt now*1000000)::Integer
      remaining=fromIntegral(min 120000000(max 0 remainingInteger))::Int
  when(remaining<=0)(failure "document_preview_deadline")
  outcome<-timeout remaining $ do
    prepared<-Document.preparePreview outputs
    runTransaction pool $ do
      -- Bound every database statement by the remaining generation time while
      -- preserving a stricter role/session timeout. The SQL publication function
      -- also rechecks the absolute deadline after all output/event transitions.
      S.statement (max 1(fromIntegral remaining `div` 1000)::Int64)(voidStatement
        "SELECT set_config('statement_timeout',(CASE WHEN current_setting('statement_timeout')::interval=interval '0' THEN $1 ELSE least($1,extract(epoch FROM current_setting('statement_timeout')::interval)*1000)::bigint END)::text||'ms',TRUE)"
        (param id E.int8))
    -- Workspace budget -> runtime -> job/run -> preview locks precede all parts.
      S.statement(runPair run)(voidStatement "SELECT hmem_document_preview_require_run($1,$2,TRUE)" pair)
      descriptors<-Document.storePreparedPreview run.workspaceId prepared
      S.statement(run.id,run.leaseToken,descriptors,toJSON metadata)(voidStatement
        "SELECT hmem_document_preview_publish_run($1,$2,$3,$4)"
        (param(\(a,_,_,_)->a)E.uuid<>param(\(_,b,_,_)->b)E.uuid<>param(\(_,_,c,_)->c)E.jsonb<>param(\(_,_,_,d)->d)E.jsonb))
  maybe(failure "document_preview_deadline")pure outcome

retry :: Pool H.Connection -> UUID -> UUID -> UUID -> IO Bool
retry pool workspace task document=runTransaction pool $ S.statement(workspace,task,document)(St.Statement
  "SELECT hmem_document_preview_retry($1,$2,$3)"
  (param(\(a,_,_)->a)E.uuid<>param(\(_,b,_)->b)E.uuid<>param(\(_,_,c)->c)E.uuid)
  (D.singleRow(D.column(D.nonNullable D.bool)))True)
unavailable :: Pool H.Connection -> UUID -> UUID -> IO Bool
unavailable pool workspace document=runTransaction pool $ S.statement(workspace,document)(St.Statement
  "SELECT hmem_document_preview_unavailable($1,$2)" pair
  (D.singleRow(D.column(D.nonNullable D.bool)))True)
recoverDue :: Pool H.Connection -> IO()
recoverDue pool=do
  owners<-runSession pool $ S.statement ()(St.Statement
    "SELECT id FROM document_preview_runs WHERE released_at IS NULL AND lease_until<=clock_timestamp() ORDER BY lease_until,id LIMIT 32"
    E.noParams(D.rowList(D.column(D.nonNullable D.uuid)))True)
  forM_ owners $ \owner->runTransaction pool $ S.statement owner(voidStatement
    "SELECT hmem_document_preview_recover($1)"(param id E.uuid))

readOriginal :: Pool H.Connection -> PreviewRun -> IO ByteString
readOriginal pool run=Document.readPreviewOriginal pool run.workspaceId run.documentId run.rawSha256
