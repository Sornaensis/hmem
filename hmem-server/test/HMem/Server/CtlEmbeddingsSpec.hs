module HMem.Server.CtlEmbeddingsSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, poll, wait)
import Control.Exception (onException)
import Data.Aeson (object, (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as LBS
import Data.IORef (atomicModifyIORef', modifyIORef', newIORef, readIORef)
import Data.List (sort)
import Data.Text (Text)
import Data.Text qualified as T
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Hasql.Connection qualified as Hasql
import Hasql.Session qualified as Session
import Test.Hspec

import HMem.DB.Observation
import HMem.DB.Pool qualified as DBPool
import HMem.DB.TestHarness
import HMem.Server.CtlEmbeddings
import HMem.Server.CtlPgvector
import HMem.Server.TestHarness qualified as ServerHarness
import HMem.Types

spec :: Spec
spec = do
  describe "provider-neutral embedding exchange codecs" $ do
    it "fingerprints exact ordered content deterministically" $ do
      let subjects =
            [ ObservationSubject SubjectFile "src/Main.hs"
            , ObservationSubject SubjectGlob "docs/**"
            ]
          original = observationContentFingerprint canonicalSha subjects "content"
      original `shouldBe` observationContentFingerprint canonicalSha subjects "content"
      original `shouldSatisfy` isLowerHexDigest
      original `shouldNotBe` observationContentFingerprint canonicalSha (reverse subjects) "content"
      original `shouldNotBe` observationContentFingerprint canonicalSha subjects "changed"
      original `shouldNotBe` observationContentFingerprint alternateSha subjects "content"

    it "encodes the stable export shape without a vector" $ do
      let record = exportRecord observationId workspaceId "content"
      case Aeson.eitherDecodeStrict' (encodeEmbeddingExportRecord record) of
        Left err -> expectationFailure err
        Right (Aeson.Object fields) -> do
          KeyMap.keys fields `shouldMatchList` map Key.fromText
            [ "format_version", "observation_id", "workspace_id", "git_sha"
            , "subjects", "content", "content_fingerprint"
            ]
          show fields `shouldNotContain` "embedding"
        Right other -> expectationFailure $ "expected object, got: " <> show other

    it "accepts only the exact version-1 import shape and a 1536-finite vector" $ do
      decodeEmbeddingImportRecord (encodeImportRecord $ importRecord observationId workspaceId validFingerprint unitX)
        `shouldBe` Right (importRecord observationId workspaceId validFingerprint unitX)
      decodeEmbeddingImportRecord (LBS.toStrict $ Aeson.encode $ object
        [ "format_version" .= embeddingExchangeVersion
        , "observation_id" .= observationId
        , "workspace_id" .= workspaceId
        , "content_fingerprint" .= validFingerprint
        , "embedding" .= unitX
        , "unexpected" .= True
        ]) `shouldBe` Left EmbeddingRecordInvalidShape
      validateEmbeddingImportRecord (importRecord observationId workspaceId validFingerprint (replicate 1535 0))
        `shouldBe` Left EmbeddingRecordInvalidVector
      validateEmbeddingImportRecord (importRecord observationId workspaceId validFingerprint (0 / 0 : replicate 1535 0))
        `shouldBe` Left EmbeddingRecordInvalidVector
      validateEmbeddingImportRecord (importRecord observationId workspaceId "ABC" unitX)
        `shouldBe` Left EmbeddingRecordInvalidFingerprint
      validateEmbeddingImportRecord (EmbeddingImportRecord 2 observationId workspaceId validFingerprint unitX)
        `shouldBe` Left (EmbeddingRecordUnsupportedVersion 2)
      decodeEmbeddingImportRecord (BS.replicate (maxEmbeddingImportLineBytes + 1) 32)
        `shouldBe` Left EmbeddingRecordTooLarge
      validateEmbeddingImportRecordNumber maxEmbeddingImportRecords `shouldBe` Right ()
      validateEmbeddingImportRecordNumber (maxEmbeddingImportRecords + 1)
        `shouldBe` Left EmbeddingRecordLimitExceeded

  around withTestEnv $ do
    describe "provider-neutral embedding exchange database operations" $ do
      it "exports missing/all rows in stable pages and isolates an optional workspace" $ \env -> do
        requireReady env
        owner <- createTestWorkspace env "embedding-export-owner"
        outsider <- createTestWorkspace env "embedding-export-outsider"
        missing <- createObservation env.pool $ newObservation owner.id "missing"
        embedded <- createObservation env.pool $ newObservation owner.id "embedded"
        other <- createObservation env.pool $ newObservation outsider.id "other"
        setObservationEmbedding env.pool owner.id embedded.id unitX

        missingRows <- collectExport env EmbeddingExportOptions
          { scope = ExportMissingEmbeddings, workspaceId = Just owner.id, pageSize = 1 }
        map (.observationId) missingRows `shouldBe` [missing.id]

        allRows <- collectExport env EmbeddingExportOptions
          { scope = ExportAllEmbeddings, workspaceId = Just owner.id, pageSize = 1 }
        map (.observationId) allRows `shouldBe` sort [missing.id, embedded.id]
        map (.observationId) allRows `shouldNotContain` [other.id]
        map (.contentFingerprint) allRows `shouldBe`
          map (\record -> observationContentFingerprint record.gitSha record.subjects record.content) allRows

        repeated <- collectExport env EmbeddingExportOptions
          { scope = ExportAllEmbeddings, workspaceId = Just owner.id, pageSize = 2 }
        repeated `shouldBe` allRows

        configuredRef <- newIORef []
        configuredResult <- exportEmbeddingsWithConfig
          (ServerHarness.mkLocalSandboxConfig env)
          EmbeddingExportOptions
            { scope = ExportAllEmbeddings, workspaceId = Just owner.id, pageSize = 1 }
          (\record -> modifyIORef' configuredRef (record:))
        configuredResult `shouldBe` Right 2
        reverse <$> readIORef configuredRef `shouldReturn` allRows

      it "imports with stale-safe idempotent CAS and deterministic partial retry outcomes" $ \env -> do
        requireReady env
        owner <- createTestWorkspace env "embedding-import-owner"
        outsider <- createTestWorkspace env "embedding-import-outsider"
        appliedObservation <- createObservation env.pool $ newObservation owner.id "apply"
        staleObservation <- createObservation env.pool $ newObservation owner.id "before edit"
        mixedObservation <- createObservation env.pool $ newObservation owner.id "mixed"

        let appliedRecord = recordFor appliedObservation unitX
        (firstSummary, firstResults) <- importLines env [encodeImportRecord appliedRecord]
        firstSummary.applied `shouldBe` 1
        map (.outcome) firstResults `shouldBe` [EmbeddingImportApplied]
        similarIds env owner.id unitX `shouldReturn` [appliedObservation.id]

        (repeatSummary, repeatResults) <- importLines env [encodeImportRecord appliedRecord]
        repeatSummary.alreadySatisfied `shouldBe` 1
        map (.outcome) repeatResults `shouldBe` [EmbeddingImportAlreadySatisfied]

        let staleRecord = recordFor staleObservation unitY
        setObservationEmbedding env.pool owner.id staleObservation.id unitX
        _ <- updateObservation env.pool owner.id staleObservation.id (UpdateObservation "after edit")
        (staleSummary, staleResults) <- importLines env [encodeImportRecord staleRecord]
        staleSummary.stale `shouldBe` 1
        map (.outcome) staleResults `shouldBe` [EmbeddingImportStale]
        similarIds env owner.id unitX `shouldReturn` [appliedObservation.id]

        let wrongWorkspace = importRecord appliedRecord.observationId outsider.id
              appliedRecord.contentFingerprint appliedRecord.embedding
            absent = importRecord missingObservationId owner.id validFingerprint unitX
        (notFoundSummary, notFoundResults) <- importLines env
          [encodeImportRecord wrongWorkspace, encodeImportRecord absent]
        notFoundSummary.notFound `shouldBe` 2
        map (.outcome) notFoundResults `shouldBe`
          [EmbeddingImportNotFound, EmbeddingImportNotFound]

        let mixedRecord = recordFor mixedObservation unitY
        (mixedSummary, mixedResults) <- importLines env
          [ encodeImportRecord mixedRecord
          , "not-json"
          , encodeImportRecord mixedRecord
          ]
        mixedSummary.applied `shouldBe` 1
        mixedSummary.rejected `shouldBe` 2
        map (.outcome) mixedResults `shouldBe`
          [ EmbeddingImportApplied
          , EmbeddingImportRejected EmbeddingRecordMalformed
          , EmbeddingImportRejected (EmbeddingRecordDuplicateObservation mixedObservation.id)
          ]
        similarIds env owner.id unitY `shouldReturn` [mixedObservation.id]

  describe "embedding import race" $ do
    around withTestEnv $
      it "rejects an import that was waiting while content changed" $ \env -> do
        requireReady env
        owner <- createTestWorkspace env "embedding-import-race"
        target <- createObservation env.pool $ newObservation owner.id "before race"
        let pendingRecord = recordFor target unitX
        DBPool.withConn env.pool $ \connection -> do
          runConnectionSql connection "BEGIN"
          runConnectionSql connection $
            "UPDATE public.observations SET content = 'after race', embedding = NULL WHERE id = '"
              <> BS8.pack (show target.id) <> "'"
          worker <- async $ importLines env [encodeImportRecord pendingRecord]
          (do
              threadDelay 100000
              poll worker >>= \case
                Nothing -> pure ()
                Just _ -> expectationFailure "import did not wait for the concurrent row mutation"
              runConnectionSql connection "COMMIT"
            ) `onException` runConnectionSql connection "ROLLBACK"
          (summary, results) <- wait worker
          summary.stale `shouldBe` 1
          map (.outcome) results `shouldBe` [EmbeddingImportStale]

requireReady :: TestEnv -> IO ()
requireReady env = inspectPgvectorWithPool env.pool >>= \case
  Right statusValue | statusValue.readiness == PgvectorReady -> pure ()
  Right _ -> pendingWith "sandbox PostgreSQL does not have the exact pgvector schema ready"
  Left err -> expectationFailure (renderPgvectorError err)

collectExport :: TestEnv -> EmbeddingExportOptions -> IO [EmbeddingExportRecord]
collectExport env options = do
  recordsRef <- newIORef []
  result <- exportEmbeddingsWithPool env.pool options $ \record ->
    modifyIORef' recordsRef (record:)
  case result of
    Left err -> expectationFailure (show err) >> pure []
    Right count -> do
      records <- reverse <$> readIORef recordsRef
      count `shouldBe` length records
      pure records

importLines
  :: TestEnv
  -> [ByteString]
  -> IO (EmbeddingImportSummary, [EmbeddingImportResult])
importLines env inputLines = do
  sourceRef <- newIORef inputLines
  resultsRef <- newIORef []
  let nextLine = atomicModifyIORef' sourceRef $ \case
        [] -> ([], Nothing)
        line:rest -> (rest, Just line)
  result <- importEmbeddingsWithPool env.pool nextLine $ \record ->
    modifyIORef' resultsRef (record:)
  results <- reverse <$> readIORef resultsRef
  case result of
    Left err -> expectationFailure (show err) >> pure (EmbeddingImportSummary 0 0 0 0 0 0 0, results)
    Right summary -> pure (summary, results)

recordFor :: Observation -> [Double] -> EmbeddingImportRecord
recordFor observation vector = importRecord
  observation.id
  observation.workspaceId
  (observationContentFingerprint observation.gitSha observation.subjects observation.content)
  vector

exportRecord :: UUID -> UUID -> Text -> EmbeddingExportRecord
exportRecord observation workspace contentValue = EmbeddingExportRecord
  { formatVersion = embeddingExchangeVersion
  , observationId = observation
  , workspaceId = workspace
  , gitSha = canonicalSha
  , subjects = [ObservationSubject SubjectFile "src/Main.hs"]
  , content = contentValue
  , contentFingerprint = observationContentFingerprint canonicalSha
      [ObservationSubject SubjectFile "src/Main.hs"] contentValue
  }

importRecord :: UUID -> UUID -> Text -> [Double] -> EmbeddingImportRecord
importRecord observation workspace fingerprint vector = EmbeddingImportRecord
  { formatVersion = embeddingExchangeVersion
  , observationId = observation
  , workspaceId = workspace
  , contentFingerprint = fingerprint
  , embedding = vector
  }

encodeImportRecord :: EmbeddingImportRecord -> ByteString
encodeImportRecord record = LBS.toStrict $ Aeson.encode $ object
  [ "format_version" .= record.formatVersion
  , "observation_id" .= record.observationId
  , "workspace_id" .= record.workspaceId
  , "content_fingerprint" .= record.contentFingerprint
  , "embedding" .= record.embedding
  ]

newObservation :: UUID -> Text -> CreateObservation
newObservation workspace contentValue = CreateObservation
  { workspaceId = workspace
  , subjects = [ObservationSubject SubjectFile "src/Main.hs"]
  , gitSha = canonicalSha
  , content = contentValue
  }

similarIds :: TestEnv -> UUID -> [Double] -> IO [UUID]
similarIds env workspace vector = fmap (map (.observation.id)) $
  similarObservations env.pool SimilarObservationQuery
    { workspaceId = workspace
    , subjectKind = Nothing
    , subject = Nothing
    , gitSha = Nothing
    , embedding = vector
    , minSimilarity = Just 1
    , limit = Nothing
    , offset = Nothing
    }

runConnectionSql :: Hasql.Connection -> ByteString -> IO ()
runConnectionSql connection sql = Session.run (Session.sql sql) connection >>= \case
  Left err -> expectationFailure $ "SQL failed: " <> show err
  Right () -> pure ()

isLowerHexDigest :: Text -> Bool
isLowerHexDigest value = T.length value == 64
  && T.all (\char -> char >= '0' && char <= '9' || char >= 'a' && char <= 'f') value

validFingerprint :: Text
validFingerprint = "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"

unitX, unitY :: [Double]
unitX = 1 : replicate (observationEmbeddingDimensions - 1) 0
unitY = 0 : 1 : replicate (observationEmbeddingDimensions - 2) 0

canonicalSha, alternateSha :: Text
canonicalSha = "0123456789abcdef0123456789abcdef01234567"
alternateSha = "fedcba9876543210fedcba9876543210fedcba98"

observationId, workspaceId, missingObservationId :: UUID
observationId = uuid "aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee"
workspaceId = uuid "11111111-2222-3333-4444-555555555555"
missingObservationId = uuid "99999999-8888-7777-6666-555555555555"

uuid :: String -> UUID
uuid raw = case UUID.fromString raw of
  Just parsed -> parsed
  Nothing -> error "invalid UUID fixture"
