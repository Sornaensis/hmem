module HMem.DB.Audit
  ( -- * Single entry
    getAuditEntry
    -- * Per-entity history
  , getAuditByEntity
    -- * Filtered global log
  , getAuditLog
  ) where

import Data.ByteString.Char8 qualified as BS8
import Data.Functor.Contravariant (contramap)
import Data.Int (Int32)
import Data.Pool (Pool)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.UUID (UUID)
import Hasql.Connection qualified as Hasql
import Hasql.Decoders qualified as Dec
import Hasql.Encoders qualified as Enc
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement

import HMem.DB.Pool (runSession)
import HMem.Types (AuditLogEntry(..), AuditLogQuery(..), auditActionFromText, auditActionToText, capPagination)

------------------------------------------------------------------------
-- Row decoder (shared)
------------------------------------------------------------------------

auditLogRowDecoder :: Dec.Row AuditLogEntry
auditLogRowDecoder = do
  alId         <- Dec.column (Dec.nonNullable Dec.uuid)
  alEntityType <- Dec.column (Dec.nonNullable Dec.text)
  alEntityId   <- Dec.column (Dec.nonNullable Dec.text)
  alActionText <- Dec.column (Dec.nonNullable Dec.text)
  alOldValues  <- Dec.column (Dec.nullable Dec.jsonb)
  alNewValues  <- Dec.column (Dec.nullable Dec.jsonb)
  alRequestId  <- Dec.column (Dec.nullable Dec.text)
  alChangedAt  <- Dec.column (Dec.nonNullable Dec.timestamptz)
  let alAction = case auditActionFromText alActionText of
        Just a  -> a
        Nothing -> error $ "HMem.DB.Audit: unexpected audit_action_enum value: " <> show alActionText
  pure AuditLogEntry
    { id         = alId
    , entityType = alEntityType
    , entityId   = alEntityId
    , action     = alAction
    , oldValues  = alOldValues
    , newValues  = alNewValues
    , requestId  = alRequestId
    , changedAt  = alChangedAt
    }

------------------------------------------------------------------------
-- getAuditEntry
------------------------------------------------------------------------

getAuditEntry :: Pool Hasql.Connection -> UUID -> IO (Maybe AuditLogEntry)
getAuditEntry pool auditId =
  runSession pool $ Session.statement auditId getAuditEntryStatement

getAuditEntryStatement :: Statement.Statement UUID (Maybe AuditLogEntry)
getAuditEntryStatement = Statement.Statement sql encoder decoder True
  where
    sql = "SELECT id, entity_type, entity_id, action::text, old_values, new_values, \
          \request_id, changed_at \
          \FROM audit_log WHERE id = $1"
    encoder = Enc.param (Enc.nonNullable Enc.uuid)
    decoder = Dec.rowMaybe auditLogRowDecoder

------------------------------------------------------------------------
-- getAuditByEntity
------------------------------------------------------------------------

getAuditByEntity
  :: Pool Hasql.Connection
  -> Text     -- ^ entity_type
  -> Text     -- ^ entity_id
  -> Maybe Int -- ^ limit (default 50)
  -> IO [AuditLogEntry]
getAuditByEntity pool entityType entityId mlimit = do
  let (lim, _) = capPagination mlimit Nothing
  runSession pool $ Session.statement (entityType, entityId, fromIntegral lim :: Int32) getAuditByEntityStatement

getAuditByEntityStatement :: Statement.Statement (Text, Text, Int32) [AuditLogEntry]
getAuditByEntityStatement = Statement.Statement sql encoder decoder True
  where
    sql = "SELECT id, entity_type, entity_id, action::text, old_values, new_values, \
          \request_id, changed_at \
          \FROM audit_log \
          \WHERE entity_type = $1 AND entity_id = $2 \
          \ORDER BY changed_at DESC, id DESC \
          \LIMIT $3"
    encoder =
      contramap (\(a,_,_) -> a) (Enc.param (Enc.nonNullable Enc.text)) <>
      contramap (\(_,b,_) -> b) (Enc.param (Enc.nonNullable Enc.text)) <>
      contramap (\(_,_,c) -> c) (Enc.param (Enc.nonNullable Enc.int4))
    decoder = Dec.rowList auditLogRowDecoder

------------------------------------------------------------------------
-- getAuditLog
------------------------------------------------------------------------

getAuditLog :: Pool Hasql.Connection -> AuditLogQuery -> IO [AuditLogEntry]
getAuditLog pool q = do
  let (lim, off) = capPagination q.limit q.offset
      params = ( q.entityType
               , q.entityId
               , auditActionToText <$> q.action
               , q.since
               , q.until
               , fromIntegral lim :: Int32
               , fromIntegral off :: Int32
               )
  runSession pool $ Session.statement params getAuditLogStatement

type AuditLogParams = (Maybe Text, Maybe Text, Maybe Text, Maybe UTCTime, Maybe UTCTime, Int32, Int32)

getAuditLogStatement :: Statement.Statement AuditLogParams [AuditLogEntry]
getAuditLogStatement = Statement.Statement sql encoder decoder True
  where
    sql = BS8.pack $ unlines
      [ "SELECT id, entity_type, entity_id, action::text, old_values, new_values,"
      , "       request_id, changed_at"
      , "FROM audit_log"
      , "WHERE ($1::text IS NULL OR entity_type = $1)"
      , "  AND ($2::text IS NULL OR entity_id = $2)"
      , "  AND ($3::text IS NULL OR action::text = $3)"
      , "  AND ($4::timestamptz IS NULL OR changed_at >= $4)"
      , "  AND ($5::timestamptz IS NULL OR changed_at <= $5)"
      , "ORDER BY changed_at DESC, id DESC"
      , "LIMIT $6 OFFSET $7"
      ]
    encoder =
      contramap (\(a,_,_,_,_,_,_) -> a) (Enc.param (Enc.nullable Enc.text)) <>
      contramap (\(_,b,_,_,_,_,_) -> b) (Enc.param (Enc.nullable Enc.text)) <>
      contramap (\(_,_,c,_,_,_,_) -> c) (Enc.param (Enc.nullable Enc.text)) <>
      contramap (\(_,_,_,d,_,_,_) -> d) (Enc.param (Enc.nullable Enc.timestamptz)) <>
      contramap (\(_,_,_,_,e,_,_) -> e) (Enc.param (Enc.nullable Enc.timestamptz)) <>
      contramap (\(_,_,_,_,_,f,_) -> f) (Enc.param (Enc.nonNullable Enc.int4)) <>
      contramap (\(_,_,_,_,_,_,g) -> g) (Enc.param (Enc.nonNullable Enc.int4))
    decoder = Dec.rowList auditLogRowDecoder
