module HMem.Server.CtlPgvectorCliSpec (spec) where

import Control.Exception (bracket)
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as AesonKey
import Data.Aeson.KeyMap qualified as AesonKeyMap
import Data.ByteString qualified as BS
import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.List (isInfixOf)
import Data.Pool (Pool, destroyAllResources)
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import Hasql.Connection qualified as Hasql
import Hasql.Decoders qualified as Dec
import Hasql.Encoders qualified as Enc
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement
import Options.Applicative
  ( ParserResult(..)
  , defaultPrefs
  , execParserPure
  , renderFailure
  )
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.Exit (ExitCode(..))
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
import HMem.Server.CtlPgvectorCli

spec :: Spec
spec = describe "hmem-ctl pgvector CLI" $ do
  describe "parser and help" $ do
    it "parses status and enable in human mode" $ do
      parseCommand ["status"] `shouldBe` Right
        (PgvectorCliCommand PgvectorStatusOperation PgvectorHuman)
      parseCommand ["enable"] `shouldBe` Right
        (PgvectorCliCommand PgvectorEnableOperation PgvectorHuman)

    it "accepts --json before or after either operation" $ do
      parseCommand ["--json", "status"] `shouldBe` Right
        (PgvectorCliCommand PgvectorStatusOperation PgvectorJson)
      parseCommand ["status", "--json"] `shouldBe` Right
        (PgvectorCliCommand PgvectorStatusOperation PgvectorJson)
      parseCommand ["--json", "enable"] `shouldBe` Right
        (PgvectorCliCommand PgvectorEnableOperation PgvectorJson)
      parseCommand ["enable", "--json"] `shouldBe` Right
        (PgvectorCliCommand PgvectorEnableOperation PgvectorJson)

    it "rejects unknown, conflicting, and duplicate arguments" $ do
      parseCommand ["disable"] `shouldSatisfy` isLeft
      parseCommand ["status", "enable"] `shouldSatisfy` isLeft
      parseCommand ["status", "--json", "--json"] `shouldSatisfy` isLeft
      parseCommand ["status", "--yaml"] `shouldSatisfy` isLeft

    it "publishes both operations, JSON mode, and exit semantics in help" $ do
      let helpText = renderHelp
      helpText `shouldSatisfy` isInfixOf "status or enable"
      helpText `shouldSatisfy` isInfixOf "--json"
      helpText `shouldSatisfy` isInfixOf "COMMAND"
      helpText `shouldSatisfy` isInfixOf "0 = ready/success"
      helpText `shouldSatisfy` isInfixOf "1 = connection"
      helpText `shouldSatisfy` isInfixOf "2 = status completed successfully"
      helpText `shouldSatisfy` isInfixOf "status is read-only"
      helpText `shouldSatisfy` isInfixOf "cannot install the PostgreSQL pgvector package"
      helpText `shouldSatisfy` isInfixOf "replay hmem migrations"
      helpText `shouldSatisfy` isInfixOf "schedule a change window"

  describe "dispatch and rendering" $ do
    it "dispatches status without invoking enable and reports exact human fields" $ do
      inspectCalls <- newIORef (0 :: Int)
      enableCalls <- newIORef (0 :: Int)
      result <- runPgvectorCommandWith
        (modifyIORef' inspectCalls (+ 1) >> pure (Right readyStatus))
        (modifyIORef' enableCalls (+ 1) >> pure (Right readyReport))
        (PgvectorCliCommand PgvectorStatusOperation PgvectorHuman)

      result.exitCode `shouldBe` ExitSuccess
      result.standardError `shouldBe` mempty
      let output = BL8.unpack result.standardOutput
      output `shouldSatisfy` isInfixOf "database: reachable"
      output `shouldSatisfy` isInfixOf "package: available (default version 0.8.0)"
      output `shouldSatisfy` isInfixOf "extension: installed (version 0.8.0, schema public)"
      output `shouldSatisfy` isInfixOf "observations table: ready"
      output `shouldSatisfy` isInfixOf "nullable vector(1536), no default"
      output `shouldSatisfy` isInfixOf "valid HNSW vector_cosine_ops"
      output `shouldSatisfy` isInfixOf "total=5, embedded=3, missing=2"
      output `shouldSatisfy` isInfixOf "next action: none"
      readIORef inspectCalls `shouldReturn` 1
      readIORef enableCalls `shouldReturn` 0

    it "uses exit 2 for every successfully diagnosed not-ready state" $ do
      mapM_ assertNotReady allNotReadyStatuses

    it "emits stable structured JSON status fields" $ do
      result <- runPgvectorCommandWith
        (pure $ Right readyStatus)
        (pure $ Right readyReport)
        (PgvectorCliCommand PgvectorStatusOperation PgvectorJson)
      result.exitCode `shouldBe` ExitSuccess
      json <- decodeOutput result
      lookupPath ["kind"] json `shouldBe` Just (Aeson.String "pgvector_status")
      lookupPath ["ok"] json `shouldBe` Just (Aeson.Bool True)
      lookupPath ["ready"] json `shouldBe` Just (Aeson.Bool True)
      lookupPath ["package", "default_version"] json `shouldBe` Just (Aeson.String "0.8.0")
      lookupPath ["embedding_column", "expected", "dimensions"] json
        `shouldBe` Just (Aeson.Number 1536)
      lookupPath ["embedding_index", "expected", "access_method"] json
        `shouldBe` Just (Aeson.String "hnsw")
      lookupPath ["embedding_index", "expected", "operator_class"] json
        `shouldBe` Just (Aeson.String "vector_cosine_ops")
      lookupPath ["counts", "embedded"] json `shouldBe` Just (Aeson.Number 3)

    it "dispatches enable and distinguishes changes from a verified no-op" $ do
      inspectCalls <- newIORef (0 :: Int)
      provisioned <- runPgvectorCommandWith
        (modifyIORef' inspectCalls (+ 1) >> pure (Right readyStatus))
        (pure $ Right provisionedReport)
        (PgvectorCliCommand PgvectorEnableOperation PgvectorHuman)
      provisioned.exitCode `shouldBe` ExitSuccess
      provisioned.standardError `shouldBe` mempty
      let changedOutput = BL8.unpack provisioned.standardOutput
      changedOutput `shouldSatisfy` isInfixOf "result: provisioned and verified"
      changedOutput `shouldSatisfy` isInfixOf "installed vector extension"
      changedOutput `shouldSatisfy` isInfixOf "added nullable vector(1536)"
      changedOutput `shouldSatisfy` isInfixOf "created HNSW vector_cosine_ops index"
      changedOutput `shouldSatisfy` isInfixOf "maintenance warning:"
      readIORef inspectCalls `shouldReturn` 0

      noOp <- runPgvectorCommandWith
        (pure $ Right readyStatus)
        (pure $ Right readyReport)
        (PgvectorCliCommand PgvectorEnableOperation PgvectorJson)
      noOp.exitCode `shouldBe` ExitSuccess
      noOpJson <- decodeOutput noOp
      lookupPath ["result"] noOpJson `shouldBe` Just (Aeson.String "already_ready")
      lookupPath ["changes"] noOpJson `shouldBe` Just (Aeson.Array mempty)
      lookupPath ["maintenance_warning"] noOpJson `shouldSatisfy` maybe False (/= Aeson.Null)

    it "maps every typed failure to exit 1 and never prints raw driver details" $ do
      mapM_ assertSafeError allTypedErrors

    it "renders actionable human permission and drift failures" $ do
      permission <- runPgvectorCommandWith
        (pure $ Right readyStatus)
        (pure $ Left $ PgvectorPermissionDenied "password=super-secret")
        (PgvectorCliCommand PgvectorEnableOperation PgvectorHuman)
      permission.exitCode `shouldBe` ExitFailure 1
      permission.standardOutput `shouldBe` mempty
      let permissionError = BL8.unpack permission.standardError
      permissionError `shouldSatisfy` isInfixOf "CREATE EXTENSION"
      permissionError `shouldSatisfy` isInfixOf "public.observations"
      permissionError `shouldNotSatisfy` isInfixOf "super-secret"

      refusal <- runPgvectorCommandWith
        (pure $ Right readyStatus)
        (pure $ Left $ PgvectorProvisionRefused
          PgvectorIssueEmbeddingColumnDrift sampleNotReadyStatus)
        (PgvectorCliCommand PgvectorEnableOperation PgvectorHuman)
      let refusalError = BL8.unpack refusal.standardError
      refusalError `shouldSatisfy` isInfixOf "no schema changes were committed"
      refusalError `shouldSatisfy` isInfixOf "current status:"
      refusalError `shouldSatisfy` isInfixOf "embedding column: incompatible"
      refusalError `shouldSatisfy` isInfixOf "next action:"

    it "maps unexpected config/runtime exceptions to a redacted exit 1" $ do
      let secret = "postgres://operator:super-secret@database/hmem"
      result <- runPgvectorCommandWith
        (ioError $ userError secret)
        (pure $ Right readyReport)
        (PgvectorCliCommand PgvectorStatusOperation PgvectorJson)
      result.exitCode `shouldBe` ExitFailure 1
      BL8.unpack result.standardOutput `shouldNotSatisfy` isInfixOf "super-secret"
      json <- decodeOutput result
      lookupPath ["error", "code"] json
        `shouldBe` Just (Aeson.String "database_unavailable")

  describe "configured database integration" $ do
    it "keeps status read-only and enables only the external configured target" $
      withPgvectorSandbox $ \db -> do
        withDbPool db $ \pool ->
          execSql pool "CREATE TABLE public.observations (id bigint PRIMARY KEY, content text NOT NULL)"
        let cfg = configForDb db
            inspectAction = inspectPgvectorWithConfig cfg
            enableAction = provisionPgvectorWithConfig cfg

        -- Once the sandbox server is running, remove PATH while the CLI layer
        -- operates.  A passing test proves status/enable connect directly via
        -- HMemConfig and do not shell out to psql or native service utilities.
        withoutExecutablePath $ do
          statusResult <- runPgvectorCommandWith inspectAction enableAction
            (PgvectorCliCommand PgvectorStatusOperation PgvectorJson)
          statusResult.exitCode `shouldBe` ExitFailure 2
          withDbPool db $ \pool ->
            queryArtifactCount pool `shouldReturn` 0

          initial <- inspectAction
          case initial of
            Right statusValue -> case statusValue.packageStatus of
              PgvectorPackageUnavailable ->
                pendingWith "sandbox PostgreSQL does not provide the pgvector package"
              PgvectorPackageAvailable {} -> pure ()
            Left err -> expectationFailure $ "configured inspection failed: " <> show err

          enabled <- runPgvectorCommandWith inspectAction enableAction
            (PgvectorCliCommand PgvectorEnableOperation PgvectorJson)
          enabled.exitCode `shouldBe` ExitSuccess
          lookupPath ["result"] <$> decodeOutput enabled
            `shouldReturn` Just (Aeson.String "provisioned")
          fmap (fmap (.readiness)) inspectAction `shouldReturn` Right PgvectorReady

          repeated <- runPgvectorCommandWith inspectAction enableAction
            (PgvectorCliCommand PgvectorEnableOperation PgvectorHuman)
          repeated.exitCode `shouldBe` ExitSuccess
          BL8.unpack repeated.standardOutput
            `shouldSatisfy` isInfixOf "already ready (no changes)"

parseCommand :: [String] -> Either String PgvectorCliCommand
parseCommand arguments = case execParserPure defaultPrefs (pgvectorCommandInfo id) arguments of
  Success commandValue -> Right commandValue
  Failure failure -> Left $ fst $ renderFailure failure "hmem-ctl pgvector"
  CompletionInvoked _ -> Left "completion invoked"

renderHelp :: String
renderHelp = case execParserPure defaultPrefs (pgvectorCommandInfo id) ["--help"] of
  Failure failure -> fst $ renderFailure failure "hmem-ctl pgvector"
  result -> "unexpected parser result: " <> showResult result
  where
    showResult = \case
      Success commandValue -> show commandValue
      Failure _ -> "failure"
      CompletionInvoked _ -> "completion"

assertNotReady :: PgvectorStatus -> Expectation
assertNotReady statusValue = do
  human <- runPgvectorCommandWith
    (pure $ Right statusValue)
    (pure $ Right readyReport)
    (PgvectorCliCommand PgvectorStatusOperation PgvectorHuman)
  human.exitCode `shouldBe` ExitFailure 2
  human.standardError `shouldBe` mempty
  BL8.unpack human.standardOutput `shouldSatisfy` isInfixOf "next action:"

  jsonResult <- runPgvectorCommandWith
    (pure $ Right statusValue)
    (pure $ Right readyReport)
    (PgvectorCliCommand PgvectorStatusOperation PgvectorJson)
  jsonResult.exitCode `shouldBe` ExitFailure 2
  json <- decodeOutput jsonResult
  lookupPath ["ok"] json `shouldBe` Just (Aeson.Bool True)
  lookupPath ["ready"] json `shouldBe` Just (Aeson.Bool False)
  lookupPath ["readiness", "state"] json `shouldBe` Just (Aeson.String "not_ready")

assertSafeError :: PgvectorError -> Expectation
assertSafeError typedError = do
  let secret = "postgres://operator:super-secret@database/hmem password=super-secret"
      commandValue = PgvectorCliCommand PgvectorEnableOperation PgvectorJson
  result <- runPgvectorCommandWith
    (pure $ Left typedError)
    (pure $ Left typedError)
    commandValue
  result.exitCode `shouldBe` ExitFailure 1
  result.standardError `shouldBe` mempty
  BL8.unpack result.standardOutput `shouldNotSatisfy` isInfixOf secret
  BL8.unpack result.standardOutput `shouldNotSatisfy` isInfixOf "super-secret"
  json <- decodeOutput result
  lookupPath ["kind"] json `shouldBe` Just (Aeson.String "pgvector_error")
  lookupPath ["ok"] json `shouldBe` Just (Aeson.Bool False)
  lookupPath ["error", "code"] json `shouldSatisfy` maybe False (/= Aeson.Null)

decodeOutput :: PgvectorCliResult -> IO Aeson.Value
decodeOutput result = case Aeson.eitherDecode result.standardOutput of
  Left err -> expectationFailure ("invalid JSON output: " <> err) >> pure Aeson.Null
  Right value -> pure value

lookupPath :: [Text] -> Aeson.Value -> Maybe Aeson.Value
lookupPath [] value = Just value
lookupPath (key : keys) (Aeson.Object objectValue) =
  AesonKeyMap.lookup (AesonKey.fromText key) objectValue >>= lookupPath keys
lookupPath _ _ = Nothing

allNotReadyStatuses :: [PgvectorStatus]
allNotReadyStatuses =
  [ notReady PgvectorIssuePackageMissing $ readyStatus
      { packageStatus = PgvectorPackageUnavailable }
  , notReady PgvectorIssueExtensionMissing $ readyStatus
      { extensionStatus = PgvectorExtensionNotInstalled }
  , notReady PgvectorIssueExtensionUnusable $ readyStatus
      { extensionStatus = incompatibleExtension }
  , notReady PgvectorIssueObservationsTableMissing $ readyStatus
      { observationsTableStatus = PgvectorObservationsTableMissing }
  , notReady PgvectorIssueObservationsTableDrift $ readyStatus
      { observationsTableStatus = PgvectorObservationsTableIncompatible "v" }
  , notReady PgvectorIssueEmbeddingColumnMissing $ readyStatus
      { embeddingColumnStatus = PgvectorEmbeddingColumnAbsent }
  , notReady PgvectorIssueEmbeddingColumnDrift $ readyStatus
      { embeddingColumnStatus = PgvectorEmbeddingColumnIncompatible incompatibleColumn }
  , notReady PgvectorIssueEmbeddingIndexMissing $ readyStatus
      { embeddingIndexStatus = PgvectorEmbeddingIndexAbsent }
  , notReady PgvectorIssueEmbeddingIndexInvalid $ readyStatus
      { embeddingIndexStatus = PgvectorEmbeddingIndexInvalid incompatibleIndex }
  , notReady PgvectorIssueEmbeddingIndexDrift $ readyStatus
      { embeddingIndexStatus = PgvectorEmbeddingIndexIncompatible incompatibleIndex }
  ]
  where
    notReady issue statusValue = statusValue
      { readiness = PgvectorNotReady issue
      , observationCounts = Nothing
      }

allTypedErrors :: [PgvectorError]
allTypedErrors =
  [ PgvectorDatabaseUnavailable secret
  , PgvectorInspectionFailed secret
  , PgvectorProvisionRefused PgvectorIssueEmbeddingColumnDrift
      sampleNotReadyStatus
  , PgvectorPermissionDenied secret
  , PgvectorProvisionFailed secret
  , PgvectorPostconditionFailed sampleNotReadyStatus
  ]
  where
    secret = "postgres://operator:super-secret@database/hmem password=super-secret"

sampleNotReadyStatus :: PgvectorStatus
sampleNotReadyStatus = readyStatus
  { embeddingColumnStatus = PgvectorEmbeddingColumnIncompatible incompatibleColumn
  , observationCounts = Nothing
  , readiness = PgvectorNotReady PgvectorIssueEmbeddingColumnDrift
  }

readyStatus :: PgvectorStatus
readyStatus = PgvectorStatus
  { packageStatus = PgvectorPackageAvailable "0.8.0"
  , extensionStatus = PgvectorExtensionInstalled "0.8.0" "public"
  , observationsTableStatus = PgvectorObservationsTablePresent
  , embeddingColumnStatus = PgvectorEmbeddingColumnReady
  , embeddingIndexStatus = PgvectorEmbeddingIndexReady
  , observationCounts = Just PgvectorObservationCounts
      { totalObservations = 5
      , embeddedObservations = 3
      , missingEmbeddings = 2
      }
  , readiness = PgvectorReady
  }

readyReport :: PgvectorProvisionReport
readyReport = PgvectorProvisionReport
  { outcome = PgvectorAlreadyReady
  , status = readyStatus
  , maintenanceImplication = pgvectorProvisioningMaintenanceNote
  }

provisionedReport :: PgvectorProvisionReport
provisionedReport = readyReport
  { outcome = PgvectorProvisioned
      [ InstallPgvectorExtension
      , AddObservationsEmbeddingColumn
      , CreateObservationsEmbeddingIndex
      ]
  }

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

incompatibleColumn :: PgvectorEmbeddingColumnDetails
incompatibleColumn = PgvectorEmbeddingColumnDetails
  { typeSchema = Just "pg_catalog"
  , typeName = Just "text"
  , dimensions = Nothing
  , nullable = False
  , hasDefault = True
  }

incompatibleIndex :: PgvectorEmbeddingIndexDetails
incompatibleIndex = PgvectorEmbeddingIndexDetails
  { relationKind = Just "i"
  , valid = False
  , ready = False
  , live = False
  , accessMethod = Just "btree"
  , operatorClassSchema = Just "pg_catalog"
  , operatorClassName = Just "int8_ops"
  , keyColumns = Just 1
  , totalColumns = Just 1
  , targetsEmbeddingColumn = False
  , hasPredicate = False
  , hasExpressions = False
  , unique = False
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

execSql :: Pool Hasql.Connection -> Text -> IO ()
execSql pool sqlText = DBPool.withConn pool $ \connection -> do
  result <- Session.run (Session.sql (encodeUtf8 sqlText)) connection
  case result of
    Left err -> expectationFailure $ "SQL failed: " <> show err
    Right () -> pure ()

queryArtifactCount :: Pool Hasql.Connection -> IO Int
queryArtifactCount pool = DBPool.withConn pool $ \connection -> do
  result <- Session.run (Session.statement () statement) connection
  case result of
    Left err -> expectationFailure ("artifact query failed: " <> show err) >> pure (-1)
    Right value -> pure value
  where
    statement = Statement.Statement
      "SELECT (SELECT count(*)::int FROM pg_extension WHERE extname = 'vector') + \
      \(SELECT count(*)::int FROM information_schema.columns WHERE table_schema = 'public' AND table_name = 'observations' AND column_name = 'embedding') + \
      \(SELECT count(*)::int FROM pg_class c JOIN pg_namespace n ON n.oid = c.relnamespace WHERE n.nspname = 'public' AND c.relname = 'idx_observations_embedding')"
      Enc.noParams
      (Dec.singleRow $ fromIntegral <$> Dec.column (Dec.nonNullable Dec.int4))
      True

encodeUtf8 :: Text -> BS.ByteString
encodeUtf8 = Text.encodeUtf8

withoutExecutablePath :: IO a -> IO a
withoutExecutablePath action = bracket
  (lookupEnv "PATH" <* setEnv "PATH" "")
  restorePath
  (const action)
  where
    restorePath = \case
      Nothing -> unsetEnv "PATH"
      Just previous -> setEnv "PATH" previous

isLeft :: Either a b -> Bool
isLeft Left {} = True
isLeft Right {} = False
