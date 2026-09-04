module HMem.Server.CtlEmbeddings
  ( EmbeddingExportScope(..)
  , EmbeddingExportOptions(..)
  , EmbeddingExportRecord(..)
  , EmbeddingImportRecord(..)
  , EmbeddingRecordError(..)
  , EmbeddingImportOutcome(..)
  , EmbeddingImportResult(..)
  , EmbeddingImportSummary(..)
  , EmbeddingsError(..)
  , embeddingExchangeVersion
  , defaultEmbeddingExportPageSize
  , maxEmbeddingImportRecords
  , maxEmbeddingImportLineBytes
  , observationContentFingerprint
  , encodeEmbeddingExportRecord
  , decodeEmbeddingImportRecord
  , validateEmbeddingImportRecord
  , validateEmbeddingImportRecordNumber
  , exportEmbeddingsWithConfig
  , exportEmbeddingsWithPool
  , importEmbeddingsWithConfig
  , importEmbeddingsWithPool
  ) where

import Control.Exception (bracket)
import Crypto.Hash (Digest, SHA256, hash)
import Data.Aeson (Value(..), object, (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types qualified as AesonTypes
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as LBS
import Data.Functor.Contravariant (contramap)
import Data.Int (Int32)
import Data.List (sort)
import Data.Pool (Pool, destroyAllResources)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.UUID (UUID)
import Hasql.Connection qualified as Hasql
import Hasql.Decoders qualified as Dec
import Hasql.Encoders qualified as Enc
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement

import HMem.Config (HMemConfig(..), PoolConfig(..), connectionString)
import HMem.DB.Pool qualified as Pool
import HMem.Server.CtlPgvector
import HMem.Server.Exception (trySynchronous)
import HMem.Types

data EmbeddingExportScope
  = ExportMissingEmbeddings
  | ExportAllEmbeddings
  deriving stock (Show, Eq)

data EmbeddingExportOptions = EmbeddingExportOptions
  { scope :: !EmbeddingExportScope
  , workspaceId :: !(Maybe UUID)
  , pageSize :: !Int
  } deriving stock (Show, Eq)

data EmbeddingExportRecord = EmbeddingExportRecord
  { formatVersion :: !Int
  , observationId :: !UUID
  , workspaceId :: !UUID
  , gitSha :: !Text
  , subjects :: ![ObservationSubject]
  , content :: !Text
  , contentFingerprint :: !Text
  } deriving stock (Show, Eq)

data EmbeddingImportRecord = EmbeddingImportRecord
  { formatVersion :: !Int
  , observationId :: !UUID
  , workspaceId :: !UUID
  , contentFingerprint :: !Text
  , embedding :: ![Double]
  } deriving stock (Show, Eq)

data EmbeddingRecordError
  = EmbeddingRecordTooLarge
  | EmbeddingRecordLimitExceeded
  | EmbeddingRecordMalformed
  | EmbeddingRecordInvalidShape
  | EmbeddingRecordUnsupportedVersion !Int
  | EmbeddingRecordInvalidFingerprint
  | EmbeddingRecordInvalidVector
  | EmbeddingRecordDuplicateObservation !UUID
  deriving stock (Show, Eq)

data EmbeddingImportOutcome
  = EmbeddingImportApplied
  | EmbeddingImportAlreadySatisfied
  | EmbeddingImportStale
  | EmbeddingImportNotFound
  | EmbeddingImportRejected !EmbeddingRecordError
  | EmbeddingImportDatabaseError
  deriving stock (Show, Eq)

data EmbeddingImportResult = EmbeddingImportResult
  { lineNumber :: !Int
  , observationId :: !(Maybe UUID)
  , workspaceId :: !(Maybe UUID)
  , outcome :: !EmbeddingImportOutcome
  } deriving stock (Show, Eq)

data EmbeddingImportSummary = EmbeddingImportSummary
  { processed :: !Int
  , applied :: !Int
  , alreadySatisfied :: !Int
  , stale :: !Int
  , notFound :: !Int
  , rejected :: !Int
  , databaseErrors :: !Int
  } deriving stock (Show, Eq)

data EmbeddingsError
  = EmbeddingsDatabaseUnavailable
  | EmbeddingsPgvectorInspectionFailed
  | EmbeddingsPgvectorNotReady !PgvectorReadiness
  | EmbeddingsInvalidExportPageSize
  | EmbeddingsOperationFailed
  deriving stock (Show, Eq)

embeddingExchangeVersion :: Int
embeddingExchangeVersion = 1

defaultEmbeddingExportPageSize :: Int
defaultEmbeddingExportPageSize = 200

maxEmbeddingImportRecords :: Int
maxEmbeddingImportRecords = 100000

maxEmbeddingImportLineBytes :: Int
maxEmbeddingImportLineBytes = 1024 * 1024

-- | Fingerprint the exact provider-neutral input represented by an export
-- record. Arrays are used deliberately so the encoded bytes do not depend on
-- JSON object key ordering.
observationContentFingerprint :: Text -> [ObservationSubject] -> Text -> Text
observationContentFingerprint gitShaValue subjectValues contentValue =
  T.pack $ show (hash payload :: Digest SHA256)
  where
    canonicalSubjects =
      [ (subjectKindToText subjectValue.subjectKind, subjectValue.subject)
      | subjectValue <- subjectValues
      ]
    payload = LBS.toStrict $ Aeson.encode
      ("hmem-observation-embedding-v1" :: Text, gitShaValue, canonicalSubjects, contentValue)

encodeEmbeddingExportRecord :: EmbeddingExportRecord -> ByteString
encodeEmbeddingExportRecord = LBS.toStrict . Aeson.encode . exportRecordJson

exportRecordJson :: EmbeddingExportRecord -> Value
exportRecordJson record = object
  [ "format_version" .= record.formatVersion
  , "observation_id" .= record.observationId
  , "workspace_id" .= record.workspaceId
  , "git_sha" .= record.gitSha
  , "subjects" .= record.subjects
  , "content" .= record.content
  , "content_fingerprint" .= record.contentFingerprint
  ]

decodeEmbeddingImportRecord :: ByteString -> Either EmbeddingRecordError EmbeddingImportRecord
decodeEmbeddingImportRecord bytes
  | BS.length bytes > maxEmbeddingImportLineBytes = Left EmbeddingRecordTooLarge
  | otherwise = case Aeson.eitherDecodeStrict' bytes of
      Left _ -> Left EmbeddingRecordMalformed
      Right value@(Object fields)
        | exactImportKeys fields ->
            case AesonTypes.parseEither parseImportRecord value of
              Left _ -> Left EmbeddingRecordMalformed
              Right record -> validateEmbeddingImportRecord record
        | otherwise -> Left EmbeddingRecordInvalidShape
      Right _ -> Left EmbeddingRecordInvalidShape

exactImportKeys :: KeyMap.KeyMap Value -> Bool
exactImportKeys fields =
  sort (map Key.toText (KeyMap.keys fields)) == sort importKeys
  where
    importKeys =
      [ "format_version"
      , "observation_id"
      , "workspace_id"
      , "content_fingerprint"
      , "embedding"
      ]

parseImportRecord :: Value -> AesonTypes.Parser EmbeddingImportRecord
parseImportRecord = AesonTypes.withObject "embedding import record" $ \fields ->
  EmbeddingImportRecord
    <$> fields AesonTypes..: "format_version"
    <*> fields AesonTypes..: "observation_id"
    <*> fields AesonTypes..: "workspace_id"
    <*> fields AesonTypes..: "content_fingerprint"
    <*> fields AesonTypes..: "embedding"

validateEmbeddingImportRecord
  :: EmbeddingImportRecord
  -> Either EmbeddingRecordError EmbeddingImportRecord
validateEmbeddingImportRecord record
  | record.formatVersion /= embeddingExchangeVersion =
      Left $ EmbeddingRecordUnsupportedVersion record.formatVersion
  | not (validFingerprint record.contentFingerprint) =
      Left EmbeddingRecordInvalidFingerprint
  | length record.embedding /= observationEmbeddingDimensions
      || any (\value -> isNaN value || isInfinite value) record.embedding =
      Left EmbeddingRecordInvalidVector
  | otherwise = Right record

validateEmbeddingImportRecordNumber :: Int -> Either EmbeddingRecordError ()
validateEmbeddingImportRecordNumber lineNumber
  | lineNumber > maxEmbeddingImportRecords = Left EmbeddingRecordLimitExceeded
  | otherwise = Right ()

validFingerprint :: Text -> Bool
validFingerprint value = T.length value == 64
  && T.all (\char -> char >= '0' && char <= '9' || char >= 'a' && char <= 'f') value

exportEmbeddingsWithConfig
  :: HMemConfig
  -> EmbeddingExportOptions
  -> (EmbeddingExportRecord -> IO ())
  -> IO (Either EmbeddingsError Int)
exportEmbeddingsWithConfig cfg options emit =
  withConfiguredPool cfg $ \pool -> exportEmbeddingsWithPool pool options emit

exportEmbeddingsWithPool
  :: Pool Hasql.Connection
  -> EmbeddingExportOptions
  -> (EmbeddingExportRecord -> IO ())
  -> IO (Either EmbeddingsError Int)
exportEmbeddingsWithPool pool options emit
  | options.pageSize < 1 || options.pageSize > 1000 =
      pure $ Left EmbeddingsInvalidExportPageSize
  | otherwise = do
      ready <- requirePgvectorReadiness pool
      case ready of
        Left err -> pure $ Left err
        Right () -> do
          attempted <- trySynchronous $ loop Nothing 0
          pure $ either (const $ Left EmbeddingsOperationFailed) Right attempted
  where
    loop cursor total = do
      rawPage <- Pool.runSession pool $ Session.statement
        ( options.workspaceId
        , options.scope == ExportAllEmbeddings
        , cursor
        , fromIntegral options.pageSize :: Int32
        ) exportPageStatement
      records <- traverse exportRecordFromRaw rawPage
      mapM_ emit records
      case reverse records of
        [] -> pure total
        lastRecord:_
          | length records < options.pageSize -> pure (total + length records)
          | otherwise -> loop (Just lastRecord.observationId) (total + length records)

data RawExportRecord = RawExportRecord
  { observationId :: !UUID
  , workspaceId :: !UUID
  , gitSha :: !Text
  , subjectsJson :: !Text
  , content :: !Text
  }

exportRecordFromRaw :: RawExportRecord -> IO EmbeddingExportRecord
exportRecordFromRaw raw = case Aeson.eitherDecodeStrict' (TE.encodeUtf8 raw.subjectsJson) of
  Left _ -> fail "invalid observation subjects in database"
  Right subjectValues -> pure EmbeddingExportRecord
    { formatVersion = embeddingExchangeVersion
    , observationId = raw.observationId
    , workspaceId = raw.workspaceId
    , gitSha = raw.gitSha
    , subjects = subjectValues
    , content = raw.content
    , contentFingerprint = observationContentFingerprint raw.gitSha subjectValues raw.content
    }

exportPageStatement
  :: Statement.Statement (Maybe UUID, Bool, Maybe UUID, Int32) [RawExportRecord]
exportPageStatement = Statement.Statement sql encoder decoder True
  where
    sql = BS8.pack $ unlines
      [ "SELECT o.id, o.workspace_id, o.git_sha,"
      , "       jsonb_agg(jsonb_build_object('subject_kind', s.subject_kind::text, 'subject', s.subject) ORDER BY s.ordinal)::text,"
      , "       o.content"
      , "FROM public.observations o"
      , "JOIN public.observation_subjects s ON s.observation_id = o.id"
      , "WHERE ($1::uuid IS NULL OR o.workspace_id = $1)"
      , "  AND ($2 OR o.embedding IS NULL)"
      , "  AND ($3::uuid IS NULL OR o.id > $3)"
      , "GROUP BY o.id, o.workspace_id, o.git_sha, o.content"
      , "ORDER BY o.id ASC"
      , "LIMIT $4"
      ]
    encoder =
         contramap (\(a,_,_,_) -> a) (Enc.param (Enc.nullable Enc.uuid))
      <> contramap (\(_,b,_,_) -> b) (Enc.param (Enc.nonNullable Enc.bool))
      <> contramap (\(_,_,c,_) -> c) (Enc.param (Enc.nullable Enc.uuid))
      <> contramap (\(_,_,_,d) -> d) (Enc.param (Enc.nonNullable Enc.int4))
    decoder = Dec.rowList $ RawExportRecord
      <$> Dec.column (Dec.nonNullable Dec.uuid)
      <*> Dec.column (Dec.nonNullable Dec.uuid)
      <*> Dec.column (Dec.nonNullable Dec.text)
      <*> Dec.column (Dec.nonNullable Dec.text)
      <*> Dec.column (Dec.nonNullable Dec.text)

importEmbeddingsWithConfig
  :: HMemConfig
  -> IO (Maybe ByteString)
  -> (EmbeddingImportResult -> IO ())
  -> IO (Either EmbeddingsError EmbeddingImportSummary)
importEmbeddingsWithConfig cfg nextLine emit =
  withConfiguredPool cfg $ \pool -> importEmbeddingsWithPool pool nextLine emit

importEmbeddingsWithPool
  :: Pool Hasql.Connection
  -> IO (Maybe ByteString)
  -> (EmbeddingImportResult -> IO ())
  -> IO (Either EmbeddingsError EmbeddingImportSummary)
importEmbeddingsWithPool pool nextLine emit = do
  ready <- requirePgvectorReadiness pool
  case ready of
    Left err -> pure $ Left err
    Right () -> do
      attempted <- trySynchronous $ loop 1 Set.empty emptySummary
      pure $ either (const $ Left EmbeddingsOperationFailed) Right attempted
  where
    loop lineNumber seen summary = nextLine >>= \case
      Nothing -> pure summary
      Just _ | Left recordError <- validateEmbeddingImportRecordNumber lineNumber -> do
        let result = EmbeddingImportResult
              { lineNumber = lineNumber
              , observationId = Nothing
              , workspaceId = Nothing
              , outcome = EmbeddingImportRejected recordError
              }
        emit result
        pure $ addOutcome result.outcome summary
      Just bytes -> case decodeEmbeddingImportRecord bytes of
            Left recordError -> do
              let result = EmbeddingImportResult
                    { lineNumber = lineNumber
                    , observationId = Nothing
                    , workspaceId = Nothing
                    , outcome = EmbeddingImportRejected recordError
                    }
              emit result
              loop (lineNumber + 1) seen (addOutcome result.outcome summary)
            Right record
              | Set.member record.observationId seen -> do
                  let result = resultFor lineNumber record $
                        EmbeddingImportRejected $
                          EmbeddingRecordDuplicateObservation record.observationId
                  emit result
                  loop (lineNumber + 1) seen (addOutcome result.outcome summary)
              | otherwise -> do
                  attempted <- trySynchronous $ applyImportRecord pool record
                  let outcome = either (const EmbeddingImportDatabaseError) id attempted
                      result = resultFor lineNumber record outcome
                  emit result
                  loop (lineNumber + 1)
                    (Set.insert record.observationId seen)
                    (addOutcome outcome summary)

resultFor :: Int -> EmbeddingImportRecord -> EmbeddingImportOutcome -> EmbeddingImportResult
resultFor lineNumber record outcome = EmbeddingImportResult
  { lineNumber = lineNumber
  , observationId = Just record.observationId
  , workspaceId = Just record.workspaceId
  , outcome = outcome
  }

emptySummary :: EmbeddingImportSummary
emptySummary = EmbeddingImportSummary 0 0 0 0 0 0 0

addOutcome :: EmbeddingImportOutcome -> EmbeddingImportSummary -> EmbeddingImportSummary
addOutcome outcome summary = case outcome of
  EmbeddingImportApplied -> summary { processed = summary.processed + 1, applied = summary.applied + 1 }
  EmbeddingImportAlreadySatisfied -> summary { processed = summary.processed + 1, alreadySatisfied = summary.alreadySatisfied + 1 }
  EmbeddingImportStale -> summary { processed = summary.processed + 1, stale = summary.stale + 1 }
  EmbeddingImportNotFound -> summary { processed = summary.processed + 1, notFound = summary.notFound + 1 }
  EmbeddingImportRejected {} -> summary { processed = summary.processed + 1, rejected = summary.rejected + 1 }
  EmbeddingImportDatabaseError -> summary { processed = summary.processed + 1, databaseErrors = summary.databaseErrors + 1 }

applyImportRecord
  :: Pool Hasql.Connection
  -> EmbeddingImportRecord
  -> IO EmbeddingImportOutcome
applyImportRecord pool record = Pool.runTransaction pool $ do
  currentRaw <- Session.statement
    (record.workspaceId, record.observationId)
    currentObservationStatement
  case currentRaw of
    Nothing -> pure EmbeddingImportNotFound
    Just (gitShaValue, subjectsJson, contentValue) ->
      case Aeson.eitherDecodeStrict' (TE.encodeUtf8 subjectsJson) of
        Left _ -> pure EmbeddingImportDatabaseError
        Right subjectValues ->
          if observationContentFingerprint gitShaValue subjectValues contentValue
                /= record.contentFingerprint
            then pure EmbeddingImportStale
            else do
              changed <- Session.statement
                (record.workspaceId, record.observationId, vectorText record.embedding)
                compareAndSetEmbeddingStatement
              pure $ if changed
                then EmbeddingImportApplied
                else EmbeddingImportAlreadySatisfied

currentObservationStatement
  :: Statement.Statement (UUID, UUID) (Maybe (Text, Text, Text))
currentObservationStatement = Statement.Statement sql encoder decoder True
  where
    sql = BS8.pack $ unlines
      [ "SELECT o.git_sha,"
      , "       (SELECT jsonb_agg(jsonb_build_object('subject_kind', s.subject_kind::text, 'subject', s.subject) ORDER BY s.ordinal)::text"
      , "          FROM public.observation_subjects s WHERE s.observation_id = o.id),"
      , "       o.content"
      , "FROM public.observations o"
      , "WHERE o.workspace_id = $1 AND o.id = $2"
      , "FOR UPDATE OF o"
      ]
    encoder =
         contramap fst (Enc.param (Enc.nonNullable Enc.uuid))
      <> contramap snd (Enc.param (Enc.nonNullable Enc.uuid))
    decoder = Dec.rowMaybe $
      (,,) <$> Dec.column (Dec.nonNullable Dec.text)
           <*> Dec.column (Dec.nonNullable Dec.text)
           <*> Dec.column (Dec.nonNullable Dec.text)

compareAndSetEmbeddingStatement :: Statement.Statement (UUID, UUID, Text) Bool
compareAndSetEmbeddingStatement = Statement.Statement
  "WITH changed AS (\
  \ UPDATE public.observations SET embedding = $3::vector\
  \ WHERE workspace_id = $1 AND id = $2 AND embedding IS DISTINCT FROM $3::vector\
  \ RETURNING 1)\
  \ SELECT EXISTS (SELECT 1 FROM changed)"
  ( contramap (\(a,_,_) -> a) (Enc.param (Enc.nonNullable Enc.uuid))
 <> contramap (\(_,b,_) -> b) (Enc.param (Enc.nonNullable Enc.uuid))
 <> contramap (\(_,_,c) -> c) (Enc.param (Enc.nonNullable Enc.text)))
  (Dec.singleRow (Dec.column (Dec.nonNullable Dec.bool)))
  True

vectorText :: [Double] -> Text
vectorText values = "[" <> T.intercalate "," (map (T.pack . show) values) <> "]"

requirePgvectorReadiness
  :: Pool Hasql.Connection
  -> IO (Either EmbeddingsError ())
requirePgvectorReadiness pool = inspectPgvectorWithPool pool >>= \case
  Left PgvectorDatabaseUnavailable {} -> pure $ Left EmbeddingsDatabaseUnavailable
  Left _ -> pure $ Left EmbeddingsPgvectorInspectionFailed
  Right statusValue -> pure $ case statusValue.readiness of
    PgvectorReady -> Right ()
    notReady -> Left $ EmbeddingsPgvectorNotReady notReady

withConfiguredPool
  :: HMemConfig
  -> (Pool Hasql.Connection -> IO (Either EmbeddingsError a))
  -> IO (Either EmbeddingsError a)
withConfiguredPool cfg action = do
  attempted <- trySynchronous $ bracket
    (Pool.createPool
      (connectionString cfg.database)
      cfg.pool.size
      cfg.pool.idleTimeout
      cfg.pool.statementTimeoutMs)
    destroyAllResources
    action
  pure $ either (const $ Left EmbeddingsDatabaseUnavailable) id attempted
