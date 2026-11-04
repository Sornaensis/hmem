-- | Exact document-link and immutable-representation authority for both servers.
module HMem.DB.DocumentGrant
  ( resolveBinding, resolveVisibleBinding, bindingIsCurrent, previewStatus, createGrant, lookupGrant, getGrant, revokeGrant, readBoundPayload ) where

import Control.Exception (throwIO)
import Data.Aeson
import Data.ByteString (ByteString)
import Data.Functor.Contravariant (contramap)
import Data.Int (Int32)
import Data.Pool (Pool)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.UUID (UUID)
import Hasql.Connection qualified as H
import Hasql.Decoders qualified as D
import Hasql.Encoders qualified as E
import Hasql.Session qualified as S
import Hasql.Statement qualified as St
import HMem.Auth.CredentialOrigin (GrantIssuer)
import HMem.DB.Pool (runSession,runTransaction)
import HMem.Document.Codec (decodePayload)
import HMem.Document.Grant
import HMem.Document.Types (DocumentException(..),PayloadManifest(..),maxDerivativeBytes,roleText)

param :: (a -> b) -> E.Value b -> E.Params a
param f value=contramap f (E.param(E.nonNullable value))
optional :: (a -> Maybe b) -> E.Value b -> E.Params a
optional f value=contramap f (E.param(E.nullable value))
pair :: E.Params(UUID,UUID)
pair=param fst E.uuid<>param snd E.uuid
parseValue :: FromJSON a => Value -> IO a
parseValue value=case fromJSON value of Success parsed->pure parsed; Error _->throwIO(DocumentException "document_grant_record_invalid")

resolveBinding :: Pool H.Connection -> UUID -> UUID -> UUID -> GrantRequest -> IO(Maybe GrantBinding)
resolveBinding _ _ _ _ input | not(validGrantRequest input)=pure Nothing
resolveBinding pool workspace task document input=do
  value<-runSession pool $ S.statement (workspace,task,document,input) $ St.Statement
    "SELECT hmem_document_grant_binding($1,$2,$3,$4,$5,$6,$7,$8)"
    (param (\(a,_,_,_)->a) E.uuid<>param (\(_,b,_,_)->b) E.uuid<>param (\(_,_,c,_)->c) E.uuid
      <>param (\(_,_,_,r)->roleText r.role) E.text<>param (\(_,_,_,r)->r.associationId) E.uuid
      <>param (\(_,_,_,r)->r.variant) E.text<>param (\(_,_,_,r)->fromIntegral r.page::Int32) E.int4
      <>optional (\(_,_,_,r)->r.generationId) E.uuid)
    (D.singleRow(D.column(D.nullable D.jsonb))) True
  traverse parseValue value

bindingIsCurrent :: Pool H.Connection -> GrantBinding -> IO Bool
bindingIsCurrent pool binding=(==Just binding) <$> resolveBinding pool binding.workspaceId binding.taskId binding.documentId
  (GrantRequest binding.role binding.associationId binding.variant binding.page binding.generationId Nothing)

-- Ordinary authenticated reads may use either retained role. Grant issuance
-- deliberately never uses this helper: its caller must supply an exact link.
resolveVisibleBinding :: Pool H.Connection -> UUID -> UUID -> UUID -> Text -> Int -> Maybe UUID -> IO(Maybe GrantBinding)
resolveVisibleBinding _ _ _ _ variant page generation | not(validRepresentation variant page generation)=pure Nothing
resolveVisibleBinding pool workspace task document variant page generation=do
  value<-runSession pool $ S.statement (workspace,task,document,variant,page,generation) $ St.Statement
    "SELECT binding FROM (SELECT hmem_document_grant_binding($1,$2,$3,role,id,$4,$5,$6) binding FROM (SELECT id,'source'::text role FROM task_document_sources WHERE workspace_id=$1 AND task_id=$2 AND document_id=$3 UNION ALL SELECT id,'evidence'::text FROM task_document_evidence WHERE workspace_id=$1 AND task_id=$2 AND document_id=$3) links) candidates WHERE binding IS NOT NULL LIMIT 1"
    (param (\(a,_,_,_,_,_)->a) E.uuid<>param (\(_,b,_,_,_,_)->b) E.uuid<>param (\(_,_,c,_,_,_)->c) E.uuid
      <>param (\(_,_,_,d,_,_)->d) E.text<>param (\(_,_,_,_,e,_)->fromIntegral e::Int32) E.int4
      <>optional (\(_,_,_,_,_,f)->f) E.uuid)
    (D.rowMaybe(D.column(D.nonNullable D.jsonb))) True
  traverse parseValue value

previewStatus :: Pool H.Connection -> UUID -> UUID -> UUID -> IO(Maybe PreviewStatus)
previewStatus pool workspace task document=do
  value<-runSession pool $ S.statement (workspace,task,document) $ St.Statement
    "SELECT jsonb_build_object('state',coalesce(p.state,'unsupported'),'error',p.error_code,'generation_id',CASE WHEN p.state='ready' THEN g.id END,'policy_version',CASE WHEN p.state='ready' THEN g.policy_version END,'retryable',coalesce(j.retryable AND p.state='failed' AND NOT hmem_document_preview_poisoned(),FALSE),'generation_metadata',CASE WHEN p.state='ready' THEN coalesce(g.preview_metadata,jsonb_build_object('verification','unknown')) END,'outputs',CASE WHEN p.state='ready' THEN coalesce((SELECT jsonb_agg(jsonb_build_object('variant',v->>'variant','page',(v->>'page')::int,'media_type',v->>'media_type','raw_bytes',(v->'manifest'->>'raw_bytes')::bigint,'raw_sha256',v->'manifest'->>'raw_sha256')) FROM jsonb_array_elements(g.outputs) v),'[]') ELSE '[]'::jsonb END) FROM documents d JOIN tasks t ON t.id=$2 AND t.workspace_id=$1 AND t.deleted_at IS NULL JOIN workspaces w ON w.id=$1 AND w.deleted_at IS NULL LEFT JOIN document_preview_jobs j ON j.document_id=d.id AND j.workspace_id=d.workspace_id LEFT JOIN document_previews p ON p.document_id=d.id AND p.workspace_id=d.workspace_id LEFT JOIN document_generations g ON g.id=p.generation_id AND g.workspace_id=d.workspace_id AND g.document_id=d.id AND g.original_sha256=d.raw_sha256 WHERE d.id=$3 AND d.workspace_id=$1 AND (EXISTS(SELECT 1 FROM task_document_sources s WHERE s.workspace_id=$1 AND s.task_id=$2 AND s.document_id=d.id) OR EXISTS(SELECT 1 FROM task_document_evidence e WHERE e.workspace_id=$1 AND e.task_id=$2 AND e.document_id=d.id))"
    (param (\(a,_,_)->a) E.uuid<>param (\(_,b,_)->b) E.uuid<>param (\(_,_,c)->c) E.uuid)
    (D.rowMaybe(D.column(D.nonNullable D.jsonb))) True
  traverse parseValue value

createGrant :: Pool H.Connection -> UUID -> Text -> GrantIssuer -> UUID -> GrantBinding -> UTCTime -> IO StoredGrant
createGrant pool identifier tokenHash issuer epoch binding expiry=do
  value<-runTransaction pool $ S.statement (identifier,tokenHash,issuer,epoch,binding,expiry) $ St.Statement
    "SELECT hmem_document_grant_create($1,$2,$3,$4,$5,$6)"
    (param (\(a,_,_,_,_,_)->a) E.uuid<>param (\(_,b,_,_,_,_)->b) E.text
      <>param (\(_,_,c,_,_,_)->toJSON c) E.jsonb<>param (\(_,_,_,d,_,_)->d) E.uuid
      <>param (\(_,_,_,_,e,_)->toJSON e) E.jsonb<>param (\(_,_,_,_,_,f)->f) E.timestamptz)
    (D.singleRow(D.column(D.nonNullable D.jsonb))) True
  parseValue value

-- Hashes are accepted only by this private lookup, never returned in a record.
lookupGrant :: Pool H.Connection -> UUID -> Text -> IO(Maybe StoredGrant)
lookupGrant pool identifier tokenHash=do
  value<-runSession pool $ S.statement (identifier,tokenHash) $ St.Statement
    (grantProjection<>" WHERE id=$1 AND token_sha256=$2 AND revoked_at IS NULL AND expires_at>clock_timestamp()")
    (param fst E.uuid<>param snd E.text) (D.rowMaybe(D.column(D.nonNullable D.jsonb))) True
  traverse parseValue value

getGrant :: Pool H.Connection -> UUID -> IO(Maybe StoredGrant)
getGrant pool identifier=do
  value<-runSession pool $ S.statement identifier $ St.Statement (grantProjection<>" WHERE id=$1")
    (E.param(E.nonNullable E.uuid)) (D.rowMaybe(D.column(D.nonNullable D.jsonb))) True
  traverse parseValue value

grantProjection :: ByteString
grantProjection="SELECT jsonb_build_object('id',id,'binding',binding,'issuer',issuer,'authority_epoch',authority_epoch,'created_at',created_at,'expires_at',expires_at,'revoked_at',revoked_at) FROM document_download_grants"

revokeGrant :: Pool H.Connection -> UUID -> UUID -> UUID -> UUID -> IO Bool
revokeGrant pool workspace task document identifier=runTransaction pool $ S.statement (workspace,task,document,identifier) $ St.Statement
  "SELECT hmem_document_grant_revoke($1,$2,$3,$4)"
  (param (\(a,_,_,_)->a) E.uuid<>param (\(_,b,_,_)->b) E.uuid<>param (\(_,_,c,_)->c) E.uuid<>param (\(_,_,_,d)->d) E.uuid)
  (D.singleRow(D.column(D.nonNullable D.bool))) True

readBoundPayload :: Pool H.Connection -> GrantBinding -> IO ByteString
readBoundPayload pool binding=do
  (metadata,parts)<-runTransaction pool $ do
    metadata<-S.statement (binding.workspaceId,binding.payloadId) $ St.Statement
      "SELECT jsonb_build_object('codec',codec,'raw_bytes',raw_bytes,'raw_sha256',raw_sha256,'compressed_bytes',compressed_bytes,'compressed_sha256',compressed_sha256,'part_count',part_count) FROM document_payloads WHERE workspace_id=$1 AND id=$2 AND published_at IS NOT NULL"
      pair (D.rowMaybe(D.column(D.nonNullable D.jsonb))) True
    bytes<-S.statement (binding.workspaceId,binding.payloadId) $ St.Statement
      "SELECT data FROM document_payload_parts WHERE workspace_id=$1 AND payload_id=$2 ORDER BY ordinal"
      pair (D.rowList(D.column(D.nonNullable D.bytea))) True
    pure(metadata,bytes)
  manifest<-maybe (throwIO(DocumentException "document_payload_not_found")) pure metadata
  parsed<-parseValue manifest::IO PayloadManifest
  -- The immutable receipt remains bound even if storage was corrupted outside guards.
  if parsed.rawBytes/=binding.rawBytes || parsed.rawSha256/=binding.rawSha256
    then throwIO(DocumentException "document_grant_payload_mismatch")
    else decodePayload maxDerivativeBytes parsed parts
