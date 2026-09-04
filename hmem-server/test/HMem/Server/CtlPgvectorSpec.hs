module HMem.Server.CtlPgvectorSpec (spec) where

import Control.Concurrent.Async (concurrently)
import Control.Exception (bracket)
import Data.ByteString qualified as BS
import Data.Either (isRight)
import Data.List (isInfixOf, sort)
import Data.Pool (Pool, destroyAllResources)
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import Hasql.Connection qualified as Hasql
import Hasql.Decoders qualified as Dec
import Hasql.Encoders qualified as Enc
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement
import Test.Hspec

import HMem.Config qualified as Config
import HMem.DB.Pool qualified as DBPool
import HMem.DB.TestHarness
  ( TestDb(..)
  , withSandboxedEnv
  , withSandboxedPostgres
  , withTestSandbox
  )
import HMem.Server.CtlPgvector

spec :: Spec
spec = describe "hmem-ctl pgvector database operations" $ do
  describe "readiness classification" $ do
    it "classifies every required readiness boundary" $ do
      let available = PgvectorPackageAvailable "0.8.0"
          installed = PgvectorExtensionInstalled "0.8.0" "public"
          table = PgvectorObservationsTablePresent
          column = PgvectorEmbeddingColumnReady
          index = PgvectorEmbeddingIndexReady
      classifyPgvectorReadiness PgvectorPackageUnavailable installed table column index
        `shouldBe` PgvectorNotReady PgvectorIssuePackageMissing
      classifyPgvectorReadiness available PgvectorExtensionNotInstalled table column index
        `shouldBe` PgvectorNotReady PgvectorIssueExtensionMissing
      classifyPgvectorReadiness available incompatibleExtension table column index
        `shouldBe` PgvectorNotReady PgvectorIssueExtensionUnusable
      classifyPgvectorReadiness available installed PgvectorObservationsTableMissing column index
        `shouldBe` PgvectorNotReady PgvectorIssueObservationsTableMissing
      classifyPgvectorReadiness available installed table PgvectorEmbeddingColumnAbsent index
        `shouldBe` PgvectorNotReady PgvectorIssueEmbeddingColumnMissing
      classifyPgvectorReadiness available installed table incompatibleColumn index
        `shouldBe` PgvectorNotReady PgvectorIssueEmbeddingColumnDrift
      classifyPgvectorReadiness available installed table column PgvectorEmbeddingIndexAbsent
        `shouldBe` PgvectorNotReady PgvectorIssueEmbeddingIndexMissing
      classifyPgvectorReadiness available installed table column (PgvectorEmbeddingIndexInvalid invalidIndexDetails)
        `shouldBe` PgvectorNotReady PgvectorIssueEmbeddingIndexInvalid
      classifyPgvectorReadiness available installed table column (PgvectorEmbeddingIndexIncompatible invalidIndexDetails)
        `shouldBe` PgvectorNotReady PgvectorIssueEmbeddingIndexDrift
      classifyPgvectorReadiness available installed table column index
        `shouldBe` PgvectorReady

    it "renders actionable drift and exact counts" $ do
      let rendered = renderPgvectorStatus readyStatus
      rendered `shouldSatisfy` isInfixOf "readiness: ready"
      rendered `shouldSatisfy` isInfixOf "nullable vector(1536), no default"
      rendered `shouldSatisfy` isInfixOf "total=5, embedded=3, missing=2"
      renderPgvectorError (PgvectorProvisionRefused PgvectorIssueEmbeddingColumnDrift
        readyStatus { readiness = PgvectorNotReady PgvectorIssueEmbeddingColumnDrift })
        `shouldSatisfy` isInfixOf "no schema changes were committed"

  it "reports an unavailable configured database" $ do
    result <- inspectPgvectorWithConfig unreachableConfig
    case result of
      Left (PgvectorDatabaseUnavailable message) -> message `shouldSatisfy` (not . null)
      other -> expectationFailure $ "Expected PgvectorDatabaseUnavailable, got: " <> show other

  it "refuses a missing observations schema without installing the extension" $
    withPgvectorSandbox $ \db -> do
      package <- packageFor db
      case package of
        PgvectorPackageUnavailable -> pendingWith "sandbox PostgreSQL does not provide the pgvector package"
        PgvectorPackageAvailable {} -> do
          let cfg = configForDb db
          result <- provisionPgvectorWithConfig cfg
          case result of
            Left (PgvectorProvisionRefused PgvectorIssueObservationsTableMissing statusValue) -> do
              statusValue.observationsTableStatus `shouldBe` PgvectorObservationsTableMissing
              statusValue.extensionStatus `shouldBe` PgvectorExtensionNotInstalled
            other -> expectationFailure $ "Expected missing-table refusal, got: " <> show other
          withDbPool db $ \pool ->
            queryBool pool "SELECT NOT EXISTS (SELECT 1 FROM pg_extension WHERE extname = 'vector')"
              `shouldReturn` True

  it "provisions the exact schema, reports counts, and is idempotent and concurrency-safe" $
    withPgvectorSandbox $ \db -> do
      requirePgvectorPackage db
      withDbPool db $ \pool -> createObservationsTable pool
      let cfg = configForDb db

      initialInspection <- inspectPgvectorWithConfig cfg
      case initialInspection of
        Right statusValue -> do
          statusValue.extensionStatus `shouldBe` PgvectorExtensionNotInstalled
          statusValue.embeddingColumnStatus `shouldBe` PgvectorEmbeddingColumnAbsent
          statusValue.embeddingIndexStatus `shouldBe` PgvectorEmbeddingIndexAbsent
        other -> expectationFailure $ "Expected inspectable legacy state, got: " <> show other

      first <- provisionPgvectorWithConfig cfg
      case first of
        Right report -> do
          report.outcome `shouldBe` PgvectorProvisioned
            [ InstallPgvectorExtension
            , AddObservationsEmbeddingColumn
            , CreateObservationsEmbeddingIndex
            ]
          report.status.readiness `shouldBe` PgvectorReady
          report.maintenanceImplication `shouldSatisfy` (not . nullText)
        other -> expectationFailure $ "Expected successful provisioning, got: " <> show other

      withDbPool db $ \pool -> insertCountFixtures pool
      counted <- inspectPgvectorWithConfig cfg
      case counted of
        Right statusValue -> do
          statusValue.embeddingColumnStatus `shouldBe` PgvectorEmbeddingColumnReady
          statusValue.embeddingIndexStatus `shouldBe` PgvectorEmbeddingIndexReady
          statusValue.observationCounts `shouldBe` Just PgvectorObservationCounts
            { totalObservations = 2
            , embeddedObservations = 1
            , missingEmbeddings = 1
            }
        other -> expectationFailure $ "Expected ready counted status, got: " <> show other
      withDbPool db $ \pool ->
        queryBool pool exactPgvectorArtifactsSql `shouldReturn` True

      second <- provisionPgvectorWithConfig cfg
      fmap (.outcome) second `shouldBe` Right PgvectorAlreadyReady

      withDbPool db $ \pool -> resetPgvectorArtifacts pool
      (concurrentA, concurrentB) <- concurrently
        (provisionPgvectorWithConfig cfg)
        (provisionPgvectorWithConfig cfg)
      let outcomes = sort $ map outcomeTag [concurrentA, concurrentB]
      outcomes `shouldBe` ["already-ready", "provisioned"]
      fmap (fmap (.readiness)) (inspectPgvectorWithConfig cfg)
        `shouldReturn` Right PgvectorReady

  it "completes an extension-only legacy state without replaying migrations" $
    withPgvectorSandbox $ \db -> do
      requirePgvectorPackage db
      withDbPool db $ \pool -> do
        createObservationsTable pool
        execSql pool "CREATE EXTENSION vector WITH SCHEMA public"
      result <- provisionPgvectorWithConfig (configForDb db)
      fmap (.outcome) result `shouldBe` Right (PgvectorProvisioned
        [AddObservationsEmbeddingColumn, CreateObservationsEmbeddingIndex])

  it "refuses an installed extension whose schema is not visible" $
    withPgvectorSandbox $ \db -> do
      requirePgvectorPackage db
      withDbPool db $ \pool -> do
        createObservationsTable pool
        execSql pool "CREATE SCHEMA private_vector"
        execSql pool "CREATE EXTENSION vector WITH SCHEMA private_vector"
      let cfg = configForDb db
      inspected <- inspectPgvectorWithConfig cfg
      case inspected of
        Right statusValue -> case statusValue.extensionStatus of
          PgvectorExtensionIncompatible
              { vectorTypePresent = True
              , vectorTypeVisible = False
              , hnswAccessMethodPresent = True
              , hnswAccessMethodExtensionOwned = True
              , cosineOperatorClassPresent = True
              , cosineOperatorClassVisible = False
              , cosineOperatorClassExtensionOwned = True
              , cosineOperatorClassMatchesVectorHnsw = True
              } -> pure ()
          other -> expectationFailure $ "Expected invisible extension state, got: " <> show other
        other -> expectationFailure $ "Expected inspectable invisible extension, got: " <> show other
      provisioned <- provisionPgvectorWithConfig cfg
      case provisioned of
        Left (PgvectorProvisionRefused PgvectorIssueExtensionUnusable _) -> pure ()
        other -> expectationFailure $ "Expected incompatible-extension refusal, got: " <> show other
      withDbPool db $ \pool ->
        queryBool pool "SELECT NOT EXISTS (SELECT 1 FROM information_schema.columns WHERE table_schema = 'public' AND table_name = 'observations' AND column_name = 'embedding')"
          `shouldReturn` True

  it "preflights an incomplete extension before changing observations" $
    withPgvectorSandbox $ \db -> do
      requirePgvectorPackage db
      withDbPool db $ \pool -> do
        createObservationsTable pool
        execSql pool "CREATE EXTENSION vector WITH SCHEMA public"
        execSql pool "ALTER EXTENSION vector DROP OPERATOR CLASS public.vector_cosine_ops USING hnsw"
        execSql pool "DROP OPERATOR CLASS public.vector_cosine_ops USING hnsw"
      let cfg = configForDb db
      inspected <- inspectPgvectorWithConfig cfg
      case inspected of
        Right statusValue -> case statusValue.extensionStatus of
          PgvectorExtensionIncompatible
              { vectorTypePresent = True
              , vectorTypeVisible = True
              , hnswAccessMethodPresent = True
              , hnswAccessMethodExtensionOwned = True
              , cosineOperatorClassPresent = False
              } -> pure ()
          other -> expectationFailure $ "Expected incomplete extension state, got: " <> show other
        other -> expectationFailure $ "Expected inspectable incomplete extension, got: " <> show other
      provisioned <- provisionPgvectorWithConfig cfg
      case provisioned of
        Left (PgvectorProvisionRefused PgvectorIssueExtensionUnusable _) -> pure ()
        other -> expectationFailure $ "Expected incomplete-extension refusal, got: " <> show other
      withDbPool db $ \pool ->
        queryBool pool "SELECT NOT EXISTS (SELECT 1 FROM information_schema.columns WHERE table_schema = 'public' AND table_name = 'observations' AND column_name = 'embedding')"
          `shouldReturn` True

  it "classifies an incomplete index and succeeds after explicit operator removal" $
    withPgvectorSandbox $ \db -> do
      requirePgvectorPackage db
      withDbPool db $ \pool -> do
        createPartitionedObservationsTable pool
        execSql pool "CREATE EXTENSION vector WITH SCHEMA public"
        execSql pool "ALTER TABLE public.observations ADD COLUMN embedding vector(1536)"
        execSql pool "CREATE INDEX idx_observations_embedding ON ONLY public.observations USING hnsw (embedding vector_cosine_ops)"
      let cfg = configForDb db
      inspected <- inspectPgvectorWithConfig cfg
      case inspected of
        Right statusValue -> case statusValue.embeddingIndexStatus of
          PgvectorEmbeddingIndexInvalid details -> do
            details.valid `shouldBe` False
            details.ready `shouldBe` True
          other -> expectationFailure $ "Expected invalid/incomplete index, got: " <> show other
        other -> expectationFailure $ "Expected inspectable incomplete index, got: " <> show other
      refused <- provisionPgvectorWithConfig cfg
      case refused of
        Left (PgvectorProvisionRefused PgvectorIssueEmbeddingIndexInvalid _) -> pure ()
        other -> expectationFailure $ "Expected invalid-index refusal, got: " <> show other
      withDbPool db $ \pool -> execSql pool "DROP INDEX public.idx_observations_embedding"
      retried <- provisionPgvectorWithConfig cfg
      fmap (.outcome) retried `shouldBe`
        Right (PgvectorProvisioned [CreateObservationsEmbeddingIndex])
      withDbPool db $ \pool ->
        queryBool pool exactPgvectorArtifactsSql `shouldReturn` True

  it "refuses incompatible columns and indexes without repairing them" $
    withPgvectorSandbox $ \db -> do
      requirePgvectorPackage db
      let cfg = configForDb db
      withDbPool db $ \pool -> do
        createObservationsTable pool
        execSql pool "ALTER TABLE public.observations ADD COLUMN embedding text"
      wrongColumn <- provisionPgvectorWithConfig cfg
      case wrongColumn of
        Left (PgvectorProvisionRefused PgvectorIssueEmbeddingColumnDrift _) -> pure ()
        other -> expectationFailure $ "Expected column-drift refusal, got: " <> show other
      withDbPool db $ \pool -> do
        queryBool pool "SELECT NOT EXISTS (SELECT 1 FROM pg_extension WHERE extname = 'vector')"
          `shouldReturn` True
        execSql pool "ALTER TABLE public.observations DROP COLUMN embedding"
      provisionPgvectorWithConfig cfg >>= (`shouldSatisfy` isRight)
      withDbPool db $ \pool -> do
        execSql pool "DROP INDEX public.idx_observations_embedding"
        execSql pool "CREATE INDEX idx_observations_embedding ON public.observations (id)"
      wrongIndex <- provisionPgvectorWithConfig cfg
      case wrongIndex of
        Left (PgvectorProvisionRefused PgvectorIssueEmbeddingIndexDrift _) -> pure ()
        other -> expectationFailure $ "Expected index-drift refusal, got: " <> show other
      withDbPool db $ \pool ->
        queryBool pool "SELECT (SELECT amname FROM pg_am WHERE oid = (SELECT relam FROM pg_class WHERE oid = 'public.idx_observations_embedding'::regclass)) = 'btree'"
          `shouldReturn` True

  it "classifies permission failures without changing the schema" $
    withPgvectorSandbox $ \db -> do
      requirePgvectorPackage db
      withDbPool db $ \pool -> do
        createObservationsTable pool
        execSql pool "CREATE EXTENSION vector WITH SCHEMA public"
        execSql pool "CREATE ROLE hmem_pgvector_limited LOGIN"
        execSql pool $ "GRANT CONNECT ON DATABASE " <> db.testDbName
          <> " TO hmem_pgvector_limited"
        execSql pool "GRANT USAGE ON SCHEMA public TO hmem_pgvector_limited"
        execSql pool "GRANT SELECT ON public.observations TO hmem_pgvector_limited"
      result <- provisionPgvectorWithConfig
        (configForDb db) { Config.database = (configForDb db).database
          { Config.user = Just "hmem_pgvector_limited" } }
      case result of
        Left (PgvectorPermissionDenied message) -> message `shouldSatisfy` (not . null)
        other -> expectationFailure $ "Expected permission denial, got: " <> show other
      withDbPool db $ \pool ->
        queryBool pool "SELECT NOT EXISTS (SELECT 1 FROM information_schema.columns WHERE table_schema = 'public' AND table_name = 'observations' AND column_name = 'embedding')"
          `shouldReturn` True

  it "releases configured-pool connections after inspection" $
    withPgvectorSandbox $ \db -> do
      withDbPool db $ \pool ->
        execSql pool "CREATE ROLE hmem_pgvector_cleanup LOGIN"
      let cfg = (configForDb db)
            { Config.database = (configForDb db).database
                { Config.user = Just "hmem_pgvector_cleanup" }
            }
      inspected <- inspectPgvectorWithConfig cfg
      inspected `shouldSatisfy` isRight
      withDbPool db $ \pool ->
        queryBool pool "SELECT NOT EXISTS (SELECT 1 FROM pg_stat_activity WHERE datname = current_database() AND usename = 'hmem_pgvector_cleanup')"
          `shouldReturn` True

incompatibleExtension :: PgvectorExtensionStatus
incompatibleExtension = PgvectorExtensionIncompatible
  { installedVersion = "0.8.0"
  , incompatibleExtensionSchema = Just "private_vector"
  , vectorTypePresent = True
  , vectorTypeVisible = False
  , hnswAccessMethodPresent = True
  , hnswAccessMethodExtensionOwned = True
  , cosineOperatorClassPresent = True
  , cosineOperatorClassVisible = False
  , cosineOperatorClassExtensionOwned = True
  , cosineOperatorClassMatchesVectorHnsw = True
  }

incompatibleColumn :: PgvectorEmbeddingColumnStatus
incompatibleColumn = PgvectorEmbeddingColumnIncompatible PgvectorEmbeddingColumnDetails
  { typeSchema = Just "pg_catalog"
  , typeName = Just "text"
  , dimensions = Nothing
  , nullable = True
  , hasDefault = False
  }

invalidIndexDetails :: PgvectorEmbeddingIndexDetails
invalidIndexDetails = PgvectorEmbeddingIndexDetails
  { relationKind = Just "i"
  , valid = False
  , ready = False
  , live = True
  , accessMethod = Just "hnsw"
  , operatorClassSchema = Just "public"
  , operatorClassName = Just "vector_cosine_ops"
  , keyColumns = Just 1
  , totalColumns = Just 1
  , targetsEmbeddingColumn = True
  , hasPredicate = False
  , hasExpressions = False
  , unique = False
  }

readyStatus :: PgvectorStatus
readyStatus = PgvectorStatus
  { packageStatus = PgvectorPackageAvailable "0.8.0"
  , extensionStatus = PgvectorExtensionInstalled "0.8.0" "public"
  , observationsTableStatus = PgvectorObservationsTablePresent
  , embeddingColumnStatus = PgvectorEmbeddingColumnReady
  , embeddingIndexStatus = PgvectorEmbeddingIndexReady
  , observationCounts = Just PgvectorObservationCounts
      { totalObservations = 5, embeddedObservations = 3, missingEmbeddings = 2 }
  , readiness = PgvectorReady
  }

withPgvectorSandbox :: (TestDb -> IO a) -> IO a
withPgvectorSandbox action =
  withTestSandbox $ \sandbox ->
    withSandboxedEnv sandbox $
      withSandboxedPostgres sandbox action

withDbPool :: TestDb -> (Pool Hasql.Connection -> IO a) -> IO a
withDbPool db = bracket
  (DBPool.createPool db.testDbConnStr 2 5 30000)
  destroyAllResources

createObservationsTable :: Pool Hasql.Connection -> IO ()
createObservationsTable pool =
  execSql pool "CREATE TABLE public.observations (id bigint PRIMARY KEY, content text NOT NULL)"

createPartitionedObservationsTable :: Pool Hasql.Connection -> IO ()
createPartitionedObservationsTable pool =
  execSql pool "CREATE TABLE public.observations (id bigint, content text NOT NULL) PARTITION BY RANGE (id); CREATE TABLE public.observations_p0 PARTITION OF public.observations FOR VALUES FROM (0) TO (1000)"

exactPgvectorArtifactsSql :: Text
exactPgvectorArtifactsSql =
  "WITH extension_info AS ( \
  \  SELECT e.oid AS extension_oid, e.extnamespace \
  \  FROM pg_extension e WHERE e.extname = 'vector' \
  \), vector_type AS ( \
  \  SELECT t.oid \
  \  FROM pg_type t JOIN extension_info e ON e.extnamespace = t.typnamespace \
  \  JOIN pg_depend d ON d.classid = 'pg_type'::regclass AND d.objid = t.oid \
  \    AND d.refclassid = 'pg_extension'::regclass AND d.refobjid = e.extension_oid AND d.deptype = 'e' \
  \  WHERE t.typname = 'vector' \
  \), expected_index AS ( \
  \  SELECT i.*, ic.relam, oc.opcname, oc.opcnamespace, oc.opcintype, am.amname, a.attnum \
  \  FROM pg_index i \
  \  JOIN pg_class ic ON ic.oid = i.indexrelid \
  \  JOIN pg_namespace icn ON icn.oid = ic.relnamespace \
  \  JOIN pg_class tbl ON tbl.oid = i.indrelid \
  \  JOIN pg_namespace tn ON tn.oid = tbl.relnamespace \
  \  JOIN pg_attribute a ON a.attrelid = tbl.oid AND a.attname = 'embedding' AND NOT a.attisdropped \
  \  JOIN pg_opclass oc ON oc.oid = i.indclass[0] \
  \  JOIN pg_am am ON am.oid = ic.relam \
  \  WHERE icn.nspname = 'public' AND ic.relname = 'idx_observations_embedding' \
  \    AND tn.nspname = 'public' AND tbl.relname = 'observations' \
  \) \
  \SELECT EXISTS ( \
  \  SELECT 1 FROM pg_attribute col CROSS JOIN extension_info e CROSS JOIN vector_type vt CROSS JOIN expected_index i \
  \  WHERE col.attrelid = 'public.observations'::regclass AND col.attname = 'embedding' AND NOT col.attisdropped \
  \    AND col.atttypid = vt.oid AND col.atttypmod = 1536 AND NOT col.attnotnull AND NOT col.atthasdef \
  \    AND i.indisvalid AND i.indisready AND i.indislive AND NOT i.indisunique \
  \    AND i.indnkeyatts = 1 AND i.indnatts = 1 AND i.indkey[0] = col.attnum \
  \    AND i.amname = 'hnsw' AND i.opcname = 'vector_cosine_ops' \
  \    AND i.opcnamespace = e.extnamespace AND i.opcintype = vt.oid \
  \    AND i.indpred IS NULL AND i.indexprs IS NULL \
  \    AND EXISTS (SELECT 1 FROM pg_depend d WHERE d.classid = 'pg_am'::regclass AND d.objid = i.relam \
  \      AND d.refclassid = 'pg_extension'::regclass AND d.refobjid = e.extension_oid AND d.deptype = 'e') \
  \    AND EXISTS (SELECT 1 FROM pg_depend d WHERE d.classid = 'pg_opclass'::regclass AND d.objid = i.indclass[0] \
  \      AND d.refclassid = 'pg_extension'::regclass AND d.refobjid = e.extension_oid AND d.deptype = 'e') \
  \)"

insertCountFixtures :: Pool Hasql.Connection -> IO ()
insertCountFixtures pool = do
  execSql pool "INSERT INTO public.observations (id, content) VALUES (1, 'missing')"
  execSql pool "INSERT INTO public.observations (id, content, embedding) SELECT 2, 'embedded', ('[' || array_to_string(array_fill(0::float8, ARRAY[1536]), ',') || ']')::vector"

resetPgvectorArtifacts :: Pool Hasql.Connection -> IO ()
resetPgvectorArtifacts pool = do
  execSql pool "TRUNCATE public.observations"
  execSql pool "DROP INDEX public.idx_observations_embedding"
  execSql pool "ALTER TABLE public.observations DROP COLUMN embedding"
  execSql pool "DROP EXTENSION vector"

packageFor :: TestDb -> IO PgvectorPackageStatus
packageFor db = do
  inspected <- inspectPgvectorWithConfig (configForDb db)
  case inspected of
    Right statusValue -> pure statusValue.packageStatus
    Left err -> expectationFailure (renderPgvectorError err) >> pure PgvectorPackageUnavailable

requirePgvectorPackage :: TestDb -> IO ()
requirePgvectorPackage db = do
  packageFor db >>= \case
    PgvectorPackageUnavailable -> pendingWith "sandbox PostgreSQL does not provide the pgvector package"
    PgvectorPackageAvailable {} -> pure ()

outcomeTag :: Either PgvectorError PgvectorProvisionReport -> String
outcomeTag = \case
  Right report -> case report.outcome of
    PgvectorAlreadyReady -> "already-ready"
    PgvectorProvisioned {} -> "provisioned"
  Left err -> "error:" <> renderPgvectorError err

execSql :: Pool Hasql.Connection -> Text -> IO ()
execSql pool sqlText = DBPool.withConn pool $ \connection -> do
  result <- Session.run (Session.sql (encodeUtf8 sqlText)) connection
  case result of
    Left err -> expectationFailure $ "SQL failed: " <> show err
    Right () -> pure ()

queryBool :: Pool Hasql.Connection -> Text -> IO Bool
queryBool pool sqlText = DBPool.withConn pool $ \connection -> do
  result <- Session.run (Session.statement () statement) connection
  case result of
    Left err -> expectationFailure ("SQL query failed: " <> show err) >> pure False
    Right value -> pure value
  where
    statement = Statement.Statement
      (encodeUtf8 sqlText)
      Enc.noParams
      (Dec.singleRow (Dec.column (Dec.nonNullable Dec.bool)))
      True

nullText :: Text -> Bool
nullText = (== "")

encodeUtf8 :: Text -> BS.ByteString
encodeUtf8 = Text.encodeUtf8

configForDb :: TestDb -> Config.HMemConfig
configForDb db = Config.defaultConfig
  { Config.database = Config.DatabaseConfig
      { Config.host = "localhost"
      , Config.port = db.testDbPort
      , Config.name = db.testDbName
      , Config.user = Nothing
      , Config.password = Nothing
      , Config.sslmode = Nothing
      }
  , Config.pool = Config.defaultConfig.pool
      { Config.size = 2
      , Config.idleTimeout = 5
      , Config.statementTimeoutMs = 30000
      }
  }

unreachableConfig :: Config.HMemConfig
unreachableConfig = Config.defaultConfig
  { Config.database = Config.defaultConfig.database
      { Config.host = "127.0.0.1"
      , Config.port = 1
      , Config.name = "hmem_ctl_pgvector_unreachable"
      , Config.user = Nothing
      , Config.password = Nothing
      , Config.sslmode = Nothing
      }
  , Config.pool = Config.defaultConfig.pool
      { Config.size = 1
      , Config.idleTimeout = 1
      , Config.statementTimeoutMs = 1000
      }
  }
