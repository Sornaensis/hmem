module HMem.DB.AuditMaintenance
  ( AuditConnection, AuditOperation,auditOperationValue,AuditFailure(..), withAuditConnection, session, tryAuditLock
  , auditStatus, setAuditPaused, auditSchedule, auditIdentity, auditAssessment, recordAuditOutcome
  , withAuditCancel,retireAuditConnection,operationCatalog,beginAuditOperation,markAuditOperationRunning
  , monitorAuditOperation,markAuditOperationUncertain,finishAuditOperation,configureOperationBudget,runAuditCommand
  , operationPrivileges
  , maintenanceDatabase, reasonValue, valueField, parseValue ) where

import Control.Exception (Exception,bracket,throwIO,mask_)
import Control.Concurrent.MVar
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
import Data.Int (Int64)
import Data.UUID (UUID)
import Hasql.Connection qualified as H
import Hasql.Connection.Setting qualified as Setting
import Hasql.Connection.Setting.Connection qualified as Connection
import Hasql.Decoders qualified as D
import Hasql.Encoders qualified as E
import Hasql.Session qualified as S
import Hasql.Statement qualified as St

import HMem.Config
import HMem.DB.AuditMaintenance.Cancel qualified as Cancel

data AuditConnection = AuditConnection H.Connection (MVar Bool)
-- Only the fixed, privilege-checked database begin function creates this value.
-- A caller cannot turn a JSON payload or a CLI identifier into a SQL target.
data AuditOperation = AuditOperation UUID Text Text Value
auditOperationValue :: AuditOperation -> Value
auditOperationValue (AuditOperation _ _ _ value)=value
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
      actualUser conn = runRaw conn $ S.statement () $ St.Statement
        "SELECT session_user::text" E.noParams (D.singleRow (D.column (D.nonNullable D.text))) False
  appUser<-bracket (acquire cfg.database) H.release actualUser
  bracket (AuditConnection <$>acquire database <*>newMVar False) retireAuditConnection $ \owned@(AuditConnection conn _) -> do
    user<-actualUser conn
    if user==appUser then throwIO (AuditFailure "separate_maintenance_credentials_required") else pure ()
    session owned $ S.sql $ TE.encodeUtf8 $ "SET statement_timeout='" <> T.pack (show cfg.auditMaintenance.assessmentTimeoutMs)
      <> "ms'; SET lock_timeout='" <> T.pack (show cfg.auditMaintenance.lockTimeoutMs) <> "ms'"
    action owned

session :: AuditConnection -> S.Session a -> IO a
session (AuditConnection connection retired) action = do
  closed<-readMVar retired
  if closed then throwIO (AuditFailure "maintenance_connection_retired") else runRaw connection action

runRaw :: H.Connection -> S.Session a -> IO a
runRaw connection action = S.run action connection >>= \case
  Left failure -> throwIO (AuditFailure (queryFailureReason failure))
  Right value -> pure value

-- Sole-owner retirement is permitted only after query/cancel children join.
-- Closing the client is not a backend join. On lost acknowledgement the caller
-- retains the native backend handle and uncertainty until that actual backend
-- exits, then reacquires the singleton before catalog reconciliation.
retireAuditConnection :: AuditConnection -> IO ()
retireAuditConnection (AuditConnection connection retired)=mask_ $
  modifyMVar_ retired $ \closed -> if closed then pure True else H.release connection>>pure True

withAuditCancel :: AuditConnection -> (Cancel.Cancel -> IO a) -> IO (Either Text a)
withAuditCancel (AuditConnection connection retired) action=do
  closed<-readMVar retired
  if closed then pure (Left "maintenance_connection_retired") else Cancel.withCancel connection action

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

operationCatalog :: AuditConnection -> IO Value
operationCatalog connection=session connection $ S.statement () (jsonStatement "SELECT public.hmem_audit_operation_catalog()")

operationPrivileges :: AuditConnection -> IO Bool
operationPrivileges connection=session connection $ S.statement () $ St.Statement
  "SELECT bool_and(coalesce(has_function_privilege(session_user,to_regprocedure(signature),'EXECUTE'),FALSE)) FROM unnest(ARRAY['public.hmem_audit_operation_catalog()','public.hmem_audit_operation_begin(uuid,text,oid,boolean)','public.hmem_audit_operation_running(uuid)','public.hmem_audit_operation_monitor(uuid)','public.hmem_audit_operation_uncertain(uuid)','public.hmem_audit_operation_finish(uuid,text,jsonb,jsonb)']) signature"
  E.noParams (D.singleRow(D.column(D.nonNullable D.bool))) False

beginAuditOperation :: AuditConnection -> UUID -> Text -> Int64 -> Bool -> IO AuditOperation
beginAuditOperation connection operationId kind target scheduled=do
  value<-session connection $ S.statement (operationId,kind,target,scheduled) $ St.Statement
    "SELECT public.hmem_audit_operation_begin($1,$2,$3::oid,$4)"
    (contramap (\(a,_,_,_)->a) (E.param (E.nonNullable E.uuid))
      <>contramap (\(_,a,_,_)->a) (E.param (E.nonNullable E.text))
      <>contramap (\(_,_,a,_)->a) (E.param (E.nonNullable E.int8))
      <>contramap (\(_,_,_,a)->a) (E.param (E.nonNullable E.bool)))
    (D.singleRow (D.column (D.nonNullable D.jsonb))) False
  case valueField "target_name" value of
    Just name | not(T.null name) && T.length name<=512
      && valueField "id" value==Just operationId && valueField "kind" value==Just kind
      && valueField "target_oid" value==Just target -> pure (AuditOperation operationId kind name value)
    _ -> throwIO (AuditFailure "maintenance_invalid_operation_target")

operationIdStatement :: ByteString -> St.Statement UUID ()
operationIdStatement sql=St.Statement sql (E.param (E.nonNullable E.uuid)) D.noResult False

markAuditOperationRunning :: AuditConnection -> UUID -> IO ()
markAuditOperationRunning connection operationId=session connection $ S.statement operationId
  (operationIdStatement "SELECT public.hmem_audit_operation_running($1)")
markAuditOperationUncertain :: AuditConnection -> UUID -> IO ()
markAuditOperationUncertain connection operationId=session connection $ S.statement operationId
  (operationIdStatement "SELECT public.hmem_audit_operation_uncertain($1)")
monitorAuditOperation :: AuditConnection -> UUID -> IO Value
monitorAuditOperation connection operationId=session connection $ S.statement operationId $ St.Statement
  "SELECT public.hmem_audit_operation_monitor($1)" (E.param (E.nonNullable E.uuid))
  (D.singleRow (D.column (D.nonNullable D.jsonb))) False

finishAuditOperation :: AuditConnection -> UUID -> Text -> Value -> Value -> IO ()
finishAuditOperation connection operationId state outcome after=session connection $ S.statement (operationId,state,outcome,after) $ St.Statement
  "SELECT public.hmem_audit_operation_finish($1,$2,$3,$4)"
  (contramap (\(a,_,_,_)->a) (E.param (E.nonNullable E.uuid))
    <>contramap (\(_,a,_,_)->a) (E.param (E.nonNullable E.text))
    <>contramap (\(_,_,a,_)->a) (E.param (E.nonNullable E.jsonb))
    <>contramap (\(_,_,_,a)->a) (E.param (E.nonNullable E.jsonb))) D.noResult False

configureOperationBudget :: AuditConnection -> Int -> IO ()
configureOperationBudget connection milliseconds
  | milliseconds<1 || milliseconds>86400000=throwIO (AuditFailure "maintenance_invalid_operation_budget")
  | otherwise=session connection $ S.sql $ TE.encodeUtf8
      ("SET statement_timeout='"<>T.pack(show milliseconds)<>"ms'")

-- Input is the fixed audit-only begin wrapper's validated result, never a CLI
-- identifier or caller-supplied SQL. Heap SQL is entirely constant. The index
-- name is PostgreSQL format('%I.%I',...) for the validated bound index OID.
runAuditCommand :: AuditConnection -> AuditOperation -> IO ()
runAuditCommand connection (AuditOperation _ kind name _)=case (kind,name) of
  ("heap","public.audit_log") -> session connection (S.sql "VACUUM (FULL, ANALYZE) ONLY public.audit_log")
  ("index",_) ->
    session connection (S.sql ("REINDEX INDEX CONCURRENTLY "<>TE.encodeUtf8 name))
  _ -> throwIO (AuditFailure "maintenance_invalid_operation_target")

valueField :: FromJSON a => Text -> Value -> Maybe a
valueField field (Object value) = KM.lookup (fromStringKey field) value >>= \x -> case fromJSON x of Success a -> Just a; Error _ -> Nothing
  where fromStringKey=Key.fromText
valueField _ _ = Nothing
reasonValue :: Text -> Value
reasonValue reason = object ["reason" .= reason]
parseValue :: FromJSON a => Value -> Either Text a
parseValue = either (const (Left "maintenance_invalid_database_response")) Right . parseEither parseJSON
