module HMem.DB.AuditMaintenance
  ( AuditConnection, AuditFailure(..), withAuditConnection, session, tryAuditLock
  , auditStatus, setAuditPaused, auditSchedule, auditIdentity, auditAssessment, recordAuditOutcome
  , maintenanceDatabase, reasonValue, valueField, parseValue ) where

import Control.Exception (Exception,bracket,throwIO)
import Data.Aeson
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Key qualified as Key
import Data.Aeson.Types (parseEither)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Functor.Contravariant (contramap)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time (UTCTime)
import Hasql.Connection qualified as H
import Hasql.Connection.Setting qualified as Setting
import Hasql.Connection.Setting.Connection qualified as Connection
import Hasql.Decoders qualified as D
import Hasql.Encoders qualified as E
import Hasql.Session qualified as S
import Hasql.Statement qualified as St

import HMem.Config

newtype AuditConnection = AuditConnection H.Connection
newtype AuditFailure = AuditFailure Text deriving stock (Show,Eq)
instance Exception AuditFailure

maintenanceDatabase :: HMemConfig -> Either Text DatabaseConfig
maintenanceDatabase cfg = case cfg.auditMaintenance.maintenanceUser of
  Just user | not (T.null user) && Just user /= cfg.database.user -> Right cfg.database
    { user=Just user,password=cfg.auditMaintenance.maintenancePassword }
  _ -> Left "separate_maintenance_credentials_required"

withAuditConnection :: HMemConfig -> (AuditConnection -> IO a) -> IO a
withAuditConnection cfg action = do
  database <- either (throwIO . AuditFailure) pure (maintenanceDatabase cfg)
  let acquire db = H.acquire [Setting.connection (Connection.string
        (connectionString db <> " connect_timeout=2 application_name='hmem-audit-maintenance'"))] >>= \case
        Left _ -> throwIO (AuditFailure "maintenance_connection_failed")
        Right conn -> pure conn
      actualUser conn = session (AuditConnection conn) $ S.statement () $ St.Statement
        "SELECT session_user::text" E.noParams (D.singleRow (D.column (D.nonNullable D.text))) False
  appUser<-bracket (acquire cfg.database) H.release actualUser
  bracket (acquire database) H.release $ \conn -> do
    user<-actualUser conn
    if user==appUser then throwIO (AuditFailure "separate_maintenance_credentials_required") else pure ()
    let owned=AuditConnection conn
    session owned $ S.sql $ TE.encodeUtf8 $ "SET statement_timeout='" <> T.pack (show cfg.auditMaintenance.assessmentTimeoutMs)
      <> "ms'; SET lock_timeout='" <> T.pack (show cfg.auditMaintenance.lockTimeoutMs) <> "ms'"
    action owned

session :: AuditConnection -> S.Session a -> IO a
session (AuditConnection connection) action = S.run action connection >>= \case
  Left failure -> throwIO (AuditFailure (queryFailureReason failure))
  Right value -> pure value

queryFailureReason :: S.SessionError -> Text
queryFailureReason failure = case failure of
  S.QueryError _ _ (S.ResultError (S.ServerError state _ _ _ _)) -> code state
  S.PipelineError (S.ResultError (S.ServerError state _ _ _ _)) -> code state
  _ -> "maintenance_transport_or_decode_failed"
  where
    code state | BS.length state==5 && BS.all (\c->c>=48&&c<=57||c>=65&&c<=90) state = "maintenance_sqlstate_"<>TE.decodeUtf8 state
               | otherwise = "maintenance_query_failed"

tryAuditLock :: AuditConnection -> IO Bool
tryAuditLock connection = session connection $ S.statement () $ St.Statement
  "SELECT pg_try_advisory_lock(717783,1)" E.noParams (D.singleRow (D.column (D.nonNullable D.bool))) False

jsonStatement :: ByteString -> St.Statement () Value
jsonStatement sql = St.Statement sql E.noParams (D.singleRow (D.column (D.nonNullable D.jsonb))) False

auditStatus :: AuditConnection -> IO Value
auditStatus connection = session connection $ S.statement () (jsonStatement "SELECT public.hmem_audit_status()")
setAuditPaused :: AuditConnection -> Bool -> IO Value
setAuditPaused connection paused = session connection $ S.statement paused $ St.Statement
  "SELECT public.hmem_audit_pause($1)" (E.param (E.nonNullable E.bool)) (D.singleRow (D.column (D.nonNullable D.jsonb))) False

auditSchedule :: AuditConnection -> AuditMaintenanceConfig -> Maybe UTCTime -> Bool -> IO Value
auditSchedule connection cfg now claim = session connection $ S.statement
  (now,cfg.windowStart,cfg.windowEnd,cfg.timezone,fromIntegral cfg.intervalDays,claim) $ St.Statement
    "SELECT public.hmem_audit_schedule($1,$2,$3,$4,$5,$6)"
    (contramap (\(a,_,_,_,_,_) -> a) (E.param (E.nullable E.timestamptz))
      <> contramap (\(_,a,_,_,_,_) -> a) (E.param (E.nonNullable E.text))
      <> contramap (\(_,_,a,_,_,_) -> a) (E.param (E.nonNullable E.text))
      <> contramap (\(_,_,_,a,_,_) -> a) (E.param (E.nonNullable E.text))
      <> contramap (\(_,_,_,_,a,_) -> a) (E.param (E.nonNullable E.int4))
      <> contramap (\(_,_,_,_,_,a) -> a) (E.param (E.nonNullable E.bool)))
    (D.singleRow (D.column (D.nonNullable D.jsonb))) False

auditIdentity :: AuditConnection -> IO Value
auditIdentity connection = session connection $ S.statement () (jsonStatement "SELECT public.hmem_audit_identity()")
auditAssessment :: AuditConnection -> IO Value
auditAssessment connection = session connection $ S.statement () (jsonStatement "SELECT public.hmem_audit_assess()")
recordAuditOutcome :: AuditConnection -> Value -> Bool -> IO ()
recordAuditOutcome connection value cycleClaimed = session connection $ S.statement (value,cycleClaimed) $ St.Statement
  "SELECT public.hmem_audit_record($1,$2)" ((fst >$< E.param (E.nonNullable E.jsonb)) <> (snd >$< E.param (E.nonNullable E.bool))) D.noResult False
  where (>$<)=contramap

valueField :: FromJSON a => Text -> Value -> Maybe a
valueField field (Object value) = KM.lookup (fromStringKey field) value >>= \x -> case fromJSON x of Success a -> Just a; Error _ -> Nothing
  where fromStringKey=Key.fromText
valueField _ _ = Nothing
reasonValue :: Text -> Value
reasonValue reason = object ["reason" .= reason]
parseValue :: FromJSON a => Value -> Either Text a
parseValue = either (const (Left "maintenance_invalid_database_response")) Right . parseEither parseJSON
