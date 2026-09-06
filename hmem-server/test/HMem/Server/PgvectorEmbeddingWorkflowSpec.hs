module HMem.Server.PgvectorEmbeddingWorkflowSpec (spec) where

import Control.Exception (bracket, finally, try)
import Data.Aeson (object, (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as AesonTypes
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as LBS
import Data.List (find, sort)
import Data.Pool (destroyAllResources)
import Data.Text (Text)
import Data.UUID (UUID)
import Hasql.Decoders qualified as Dec
import Hasql.Encoders qualified as Enc
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement
import System.Exit (ExitCode(..))
import System.Directory (removeFile)
import System.FilePath ((</>))
import System.IO
  ( Handle
  , SeekMode(..)
  , hClose
  , hFlush
  , hSeek
  , openBinaryTempFile
  )
import Test.Hspec

import HMem.DB.Observation
import HMem.DB.Pool (DBException(..))
import HMem.DB.Pool qualified as DBPool
import HMem.DB.TestHarness
import HMem.Server.CtlEmbeddings
import HMem.Server.CtlEmbeddingsCli
import HMem.Server.CtlPgvector
import HMem.Server.CtlPgvectorCli
import HMem.Server.TestHarness qualified as ServerHarness
import HMem.Types

spec :: Spec
spec = describe "pgvector and external embedding operator workflow" $
  it "runs status, enable, NDJSON backfill, refresh, and stale protection in a disposable configured database" $
    withSandboxedTestEnv $ \env -> workflow env `finally` destroyAllResources env.pool

workflow :: TestEnv -> IO ()
workflow env = do
      let cfg = ServerHarness.mkLocalSandboxConfig env
          embeddingOps = EmbeddingsCliOperations
            { exportOperation = exportEmbeddingsWithConfig cfg
            , importOperation = importEmbeddingsWithConfig cfg
            }

      resetOptionalVectorArtifacts env
      workspace <- createTestWorkspace env "pgvector-embedding-e2e"
      first <- createObservation env.pool $ newObservation workspace.id "first content"
      second <- createObservation env.pool $ newObservation workspace.id "second content"

      -- The same configured PostgreSQL remains fully usable for non-vector
      -- Observation work while the optional artifacts are absent.
      fmap (fmap (.content))
        (updateObservation env.pool workspace.id first.id (UpdateObservation "first content before enable"))
        `shouldReturn` Just "first content before enable"
      unavailable <- try @DBException $
        setObservationEmbedding env.pool workspace.id first.id unitX
      unavailable `shouldSatisfy` isCapabilityUnavailable

      initialStatus <- runPgvectorCommandWith
        (inspectPgvectorWithConfig cfg)
        (provisionPgvectorWithConfig cfg)
        (PgvectorCliCommand PgvectorStatusOperation PgvectorJson)
      initialStatus.exitCode `shouldBe` ExitFailure 2
      initialStatus.standardError `shouldBe` mempty
      initialStatus.standardOutput `shouldSatisfy` lazyContains "\"ready\":false"

      inspected <- expectRight "initial pgvector inspection" =<< inspectPgvectorWithConfig cfg
      case inspected.packageStatus of
        PgvectorPackageUnavailable -> do
          inspected.readiness `shouldBe` PgvectorNotReady PgvectorIssuePackageMissing
          initialStatus.standardOutput `shouldSatisfy` lazyContains "\"issue\":\"package_missing\""
          pendingWith "sandbox PostgreSQL does not provide the pgvector package; unavailable-package behavior passed and enabled workflow was skipped"
        PgvectorPackageAvailable {} -> pure ()

      inspected.readiness `shouldBe` PgvectorNotReady PgvectorIssueExtensionMissing
      initialStatus.standardOutput `shouldSatisfy` lazyContains "\"issue\":\"extension_missing\""
      ledgerBefore <- queryMigrationLedger env

      enabled <- runPgvectorCommandWith
        (inspectPgvectorWithConfig cfg)
        (provisionPgvectorWithConfig cfg)
        (PgvectorCliCommand PgvectorEnableOperation PgvectorHuman)
      enabled.exitCode `shouldBe` ExitSuccess
      enabled.standardOutput `shouldSatisfy` lazyContains "provisioned and verified"
      enabled.standardOutput `shouldSatisfy` lazyContains "maintenance warning"

      ledgerAfter <- queryMigrationLedger env
      ledgerAfter `shouldBe` ledgerBefore

      ready <- runPgvectorCommandWith
        (inspectPgvectorWithConfig cfg)
        (provisionPgvectorWithConfig cfg)
        (PgvectorCliCommand PgvectorStatusOperation PgvectorJson)
      ready.exitCode `shouldBe` ExitSuccess
      ready.standardOutput `shouldSatisfy` lazyContains "\"ready\":true"
      ready.standardOutput `shouldSatisfy` lazyContains "\"dimensions\":1536"
      ready.standardOutput `shouldSatisfy` lazyContains "\"access_method\":\"hnsw\""
      ready.standardOutput `shouldSatisfy` lazyContains "\"operator_class\":\"vector_cosine_ops\""
      ready.standardOutput `shouldSatisfy` lazyContains "\"total\":2"
      ready.standardOutput `shouldSatisfy` lazyContains "\"embedded\":0"
      ready.standardOutput `shouldSatisfy` lazyContains "\"missing\":2"

      noOp <- runPgvectorCommandWith
        (inspectPgvectorWithConfig cfg)
        (provisionPgvectorWithConfig cfg)
        (PgvectorCliCommand PgvectorEnableOperation PgvectorJson)
      noOp.exitCode `shouldBe` ExitSuccess
      noOp.standardOutput `shouldSatisfy` lazyContains "\"result\":\"already_ready\""

      let exportPath = env.testSandbox.sandboxTmpDir </> "embedding-input.ndjson"
          importPath = env.testSandbox.sandboxTmpDir </> "embedding-results.ndjson"
          stalePath = env.testSandbox.sandboxTmpDir </> "stale-result.ndjson"
      mapM_ (\path -> BS.writeFile path BS.empty) [exportPath, importPath, stalePath]
      mapM_ (assertInSandbox env.testSandbox) [exportPath, importPath, stalePath]

      (exportExit, exportOutput, exportError) <- runEmbeddingCli env embeddingOps $
        EmbeddingsExportCommand EmbeddingsExportCliOptions
          { exportAll = False
          , workspaceId = Just workspace.id
          , outputPath = Just exportPath
          , pageSize = 1
          , targetSpace = legacyManualEmbeddingSpace
          }
      exportExit `shouldBe` ExitSuccess
      exportError `shouldBe` mempty
      exportOutput `shouldSatisfy` BS8.isInfixOf "exported 2 observation"
      exported <- traverse decodeExportRecord . filter (not . BS.null) . BS8.lines
        =<< BS.readFile exportPath
      map (.formatVersion) exported `shouldBe` replicate 2 embeddingExchangeVersion
      sort (map (.observationId) exported) `shouldBe` sort [first.id, second.id]
      map (.observationId) exported `shouldBe` sort (map (.observationId) exported)
      map (.contentFingerprint) exported `shouldBe`
        map (\record -> observationContentFingerprint record.gitSha record.subjects record.content) exported

      firstRecord <- requireRecord first.id exported
      secondRecord <- requireRecord second.id exported
      BS.writeFile importPath $ ndjson
        [ encodeImportRecord firstRecord unitX
        , encodeImportRecord secondRecord unitY
        ]

      (importExit, importOutput, importError) <- runEmbeddingCli env embeddingOps $
        EmbeddingsImportCommand $ EmbeddingsImportCliOptions (Just importPath) EmbeddingsJson
      importExit `shouldBe` ExitSuccess
      importError `shouldBe` mempty
      importOutput `shouldSatisfy` BS8.isInfixOf "\"applied\":2"
      importOutput `shouldSatisfy` (not . BS8.isInfixOf "\"embedding\"")

      (retryExit, retryOutput, retryError) <- runEmbeddingCli env embeddingOps $
        EmbeddingsImportCommand $ EmbeddingsImportCliOptions (Just importPath) EmbeddingsHuman
      retryExit `shouldBe` ExitSuccess
      retryError `shouldBe` mempty
      retryOutput `shouldSatisfy` BS8.isInfixOf "already_satisfied=2"

      similarIds env workspace.id unitX `shouldReturn` [first.id]
      fmap (fmap (.content))
        (updateObservation env.pool workspace.id first.id (UpdateObservation "first content after embedding"))
        `shouldReturn` Just "first content after embedding"
      similarIds env workspace.id unitX `shouldReturn` []

      BS.writeFile stalePath $ ndjson [encodeImportRecord firstRecord unitX]
      (staleExit, staleOutput, staleError) <- runEmbeddingCli env embeddingOps $
        EmbeddingsImportCommand $ EmbeddingsImportCliOptions (Just stalePath) EmbeddingsHuman
      staleExit `shouldBe` ExitFailure 2
      staleError `shouldBe` mempty
      staleOutput `shouldSatisfy` BS8.isInfixOf "outcome=stale"
      staleOutput `shouldSatisfy` BS8.isInfixOf "stale=1"

      missingAfterEdit <- exportIds env embeddingOps workspace.id False "missing-after-edit.ndjson"
      missingAfterEdit `shouldBe` [first.id]
      allAfterEdit <- exportIds env embeddingOps workspace.id True "all-after-edit.ndjson"
      sort allAfterEdit `shouldBe` sort [first.id, second.id]

resetOptionalVectorArtifacts :: TestEnv -> IO ()
resetOptionalVectorArtifacts env = do
  execSql env "DROP INDEX IF EXISTS public.idx_observations_embedding"
  execSql env "ALTER TABLE public.observations DROP COLUMN IF EXISTS embedding"
  execSql env "DROP EXTENSION IF EXISTS vector"

execSql :: TestEnv -> ByteString -> IO ()
execSql env sql = DBPool.runSession env.pool $ Session.sql sql

queryMigrationLedger :: TestEnv -> IO Text
queryMigrationLedger env = DBPool.runSession env.pool $ Session.statement () statement
  where
    statement = Statement.Statement
      "SELECT coalesce(jsonb_agg(jsonb_build_array(version, name, applied_at) ORDER BY version)::text, '[]') FROM schema_migrations"
      Enc.noParams
      (Dec.singleRow $ Dec.column $ Dec.nonNullable Dec.text)
      True

runEmbeddingCli
  :: TestEnv
  -> EmbeddingsCliOperations
  -> EmbeddingsCliCommand
  -> IO (ExitCode, ByteString, ByteString)
runEmbeddingCli env operations commandValue =
  withCliHandles env $ \input output errors -> do
    exitCode <- runEmbeddingsCommandWith operations input output errors commandValue
    outputBytes <- readHandle output
    errorBytes <- readHandle errors
    pure (exitCode, outputBytes, errorBytes)

withCliHandles :: TestEnv -> (Handle -> Handle -> Handle -> IO a) -> IO a
withCliHandles env action =
  withTempHandle "e2e-stdin" $ \input ->
    withTempHandle "e2e-stdout" $ \output ->
      withTempHandle "e2e-stderr" $ \errors ->
        action input output errors
  where
    withTempHandle template use = bracket
      (openBinaryTempFile env.testSandbox.sandboxTmpDir template)
      (\(path, handle) -> hClose handle >> removeFile path)
      (use . snd)

readHandle :: Handle -> IO ByteString
readHandle handle = do
  hFlush handle
  hSeek handle AbsoluteSeek 0
  BS.hGetContents handle

decodeExportRecord :: ByteString -> IO EmbeddingExportRecord
decodeExportRecord bytes = case Aeson.eitherDecodeStrict' bytes >>= AesonTypes.parseEither parser of
  Left err -> expectationFailure err >> fail err
  Right record -> pure record
  where
    parser = AesonTypes.withObject "embedding export record" $ \fields ->
      EmbeddingExportRecord
        <$> fields AesonTypes..: "format_version"
        <*> fields AesonTypes..: "observation_id"
        <*> fields AesonTypes..: "workspace_id"
        <*> fields AesonTypes..: "git_sha"
        <*> fields AesonTypes..: "subjects"
        <*> fields AesonTypes..: "content"
        <*> fields AesonTypes..: "content_fingerprint"
        <*> fields AesonTypes..: "space_fingerprint"

encodeImportRecord :: EmbeddingExportRecord -> [Double] -> ByteString
encodeImportRecord record vector = LBS.toStrict $ Aeson.encode $ object
  [ "format_version" .= record.formatVersion
  , "observation_id" .= record.observationId
  , "workspace_id" .= record.workspaceId
  , "content_fingerprint" .= record.contentFingerprint
  , "space_fingerprint" .= record.spaceFingerprint
  , "embedding" .= vector
  ]

ndjson :: [ByteString] -> ByteString
ndjson records = BS.intercalate "\n" records <> "\n"

requireRecord :: UUID -> [EmbeddingExportRecord] -> IO EmbeddingExportRecord
requireRecord observationId records = case find ((== observationId) . (.observationId)) records of
  Just record -> pure record
  Nothing -> expectationFailure "expected exported observation record" >> fail "missing export record"

exportIds
  :: TestEnv
  -> EmbeddingsCliOperations
  -> UUID
  -> Bool
  -> FilePath
  -> IO [UUID]
exportIds env operations workspaceId exportAll fileName = do
  let path = env.testSandbox.sandboxTmpDir </> fileName
  BS.writeFile path BS.empty
  (exitCode, _, errors) <- runEmbeddingCli env operations $
    EmbeddingsExportCommand EmbeddingsExportCliOptions
      { exportAll = exportAll
      , workspaceId = Just workspaceId
      , outputPath = Just path
      , pageSize = 1
      , targetSpace = legacyManualEmbeddingSpace
      }
  exitCode `shouldBe` ExitSuccess
  errors `shouldBe` mempty
  records <- traverse decodeExportRecord . filter (not . BS.null) . BS8.lines =<< BS.readFile path
  pure $ map (.observationId) records

similarIds :: TestEnv -> UUID -> [Double] -> IO [UUID]
similarIds env workspaceId vector = fmap (map (.observation.id)) $
  similarObservations env.pool SimilarObservationQuery
    { workspaceId = workspaceId
    , subjectKind = Nothing
    , subject = Nothing
    , gitSha = Nothing
    , embedding = vector
    , spaceFingerprint = Nothing
    , minSimilarity = Just 1
    , limit = Nothing
    , offset = Nothing
    }

newObservation :: UUID -> Text -> CreateObservation
newObservation workspaceId content = CreateObservation
  { workspaceId = workspaceId
  , subjects = [ObservationSubject SubjectFile "src/Main.hs"]
  , gitSha = "0123456789abcdef0123456789abcdef01234567"
  , content = content
  }

isCapabilityUnavailable :: Either DBException () -> Bool
isCapabilityUnavailable (Left (DBCapabilityUnavailable _)) = True
isCapabilityUnavailable _ = False

expectRight :: Show error => String -> Either error value -> IO value
expectRight label = \case
  Right value -> pure value
  Left err -> expectationFailure (label <> " failed: " <> show err) >> fail label

unitX, unitY :: [Double]
unitX = 1 : replicate (observationEmbeddingDimensions - 1) 0
unitY = 0 : 1 : replicate (observationEmbeddingDimensions - 2) 0

lazyContains :: ByteString -> LBS.ByteString -> Bool
lazyContains needle = BS8.isInfixOf needle . LBS.toStrict
