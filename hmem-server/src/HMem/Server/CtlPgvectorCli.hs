module HMem.Server.CtlPgvectorCli
  ( PgvectorCliOperation(..)
  , PgvectorCliFormat(..)
  , PgvectorCliCommand(..)
  , PgvectorCliResult(..)
  , pgvectorCommandParser
  , pgvectorCommandInfo
  , runPgvectorCommand
  , runPgvectorCommandWith
  , renderPgvectorStatusHuman
  , renderPgvectorEnableHuman
  , renderPgvectorStatusJson
  , renderPgvectorEnableJson
  , pgvectorNextAction
  ) where

import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.Text (Text)
import Data.Text qualified as T
import Options.Applicative
  ( Parser
  , ParserInfo
  , ReadM
  , argument
  , eitherReader
  , flag
  , footer
  , fullDesc
  , help
  , helper
  , info
  , long
  , metavar
  , progDesc
  , (<**>)
  )
import System.Exit (ExitCode(..))

import HMem.Server.CtlPgvector
import HMem.Server.Exception (trySynchronous)

-- | The pgvector operation requested from hmem-ctl.
data PgvectorCliOperation
  = PgvectorStatusOperation
  | PgvectorEnableOperation
  deriving stock (Show, Eq)

data PgvectorCliFormat
  = PgvectorHuman
  | PgvectorJson
  deriving stock (Show, Eq)

data PgvectorCliCommand = PgvectorCliCommand
  { operation :: !PgvectorCliOperation
  , format :: !PgvectorCliFormat
  } deriving stock (Show, Eq)

-- | Fully testable process output and exit semantics.  A successfully
-- diagnosed but not-ready database uses exit code 2; operational errors use
-- exit code 1.
data PgvectorCliResult = PgvectorCliResult
  { exitCode :: !ExitCode
  , standardOutput :: !ByteString
  , standardError :: !ByteString
  } deriving stock (Show, Eq)

-- | Parse the pgvector verb as an argument so @--json@ is accepted both
-- before and after it.  A duplicate flag is rejected by optparse-applicative.
pgvectorCommandParser :: Parser PgvectorCliCommand
pgvectorCommandParser = PgvectorCliCommand
  <$> argument pgvectorOperationReader
      ( metavar "COMMAND"
     <> help "Operation to run: status or enable"
      )
  <*> flag PgvectorHuman PgvectorJson
      ( long "json"
     <> help "Emit stable machine-readable JSON"
      )

pgvectorCommandInfo :: (PgvectorCliCommand -> a) -> ParserInfo a
pgvectorCommandInfo wrap = info
  ((wrap <$> pgvectorCommandParser) <**> helper)
  ( fullDesc
 <> progDesc "Inspect or enable pgvector Observation embedding support"
 <> footer (unwords
      [ "Exit codes: 0 = ready/success;"
      , "1 = connection, inspection, refusal, permission, or provisioning error;"
      , "2 = status completed successfully but the configured database is not ready."
      , "status is read-only; enable performs only the reported pgvector schema changes."
      , "enable cannot install the PostgreSQL pgvector package or replay hmem migrations."
      , "Provisioning is atomic, but schema/index work can block Observation writes;"
      , "take a backup and schedule a change window before enabling a production database."
      ])
  )

pgvectorOperationReader :: ReadM PgvectorCliOperation
pgvectorOperationReader = eitherReader $ \case
  "status" -> Right PgvectorStatusOperation
  "enable" -> Right PgvectorEnableOperation
  value -> Left $ "unknown pgvector command '" <> value
    <> "' (expected status or enable)"

runPgvectorCommand :: PgvectorCliCommand -> IO PgvectorCliResult
runPgvectorCommand = runPgvectorCommandWith inspectPgvector provisionPgvector

runPgvectorCommandWith
  :: IO (Either PgvectorError PgvectorStatus)
  -> IO (Either PgvectorError PgvectorProvisionReport)
  -> PgvectorCliCommand
  -> IO PgvectorCliResult
runPgvectorCommandWith inspectAction enableAction commandValue =
  case commandValue.operation of
    PgvectorStatusOperation -> do
      inspected <- catchOperationError inspectAction
      pure $ case inspected of
        Left err -> errorResult commandValue err
        Right statusValue -> PgvectorCliResult
          { exitCode = statusExitCode statusValue
          , standardOutput = case commandValue.format of
              PgvectorHuman -> textLine $ renderPgvectorStatusHuman statusValue
              PgvectorJson -> jsonLine $ renderPgvectorStatusJson statusValue
          , standardError = mempty
          }
    PgvectorEnableOperation -> do
      enabled <- catchOperationError enableAction
      pure $ case enabled of
        Left err -> errorResult commandValue err
        Right report -> PgvectorCliResult
          { exitCode = ExitSuccess
          , standardOutput = case commandValue.format of
              PgvectorHuman -> textLine $ renderPgvectorEnableHuman report
              PgvectorJson -> jsonLine $ renderPgvectorEnableJson report
          , standardError = mempty
          }

catchOperationError
  :: IO (Either PgvectorError a)
  -> IO (Either PgvectorError a)
catchOperationError action = do
  attempted <- trySynchronous action
  case attempted of
    Left err -> pure $ Left $ PgvectorDatabaseUnavailable (show err)
    Right result -> pure result

statusExitCode :: PgvectorStatus -> ExitCode
statusExitCode statusValue = case statusValue.readiness of
  PgvectorReady -> ExitSuccess
  PgvectorNotReady {} -> ExitFailure 2

errorResult :: PgvectorCliCommand -> PgvectorError -> PgvectorCliResult
errorResult commandValue err = case commandValue.format of
  PgvectorHuman -> PgvectorCliResult
    { exitCode = ExitFailure 1
    , standardOutput = mempty
    , standardError = textLine $ renderPgvectorErrorHuman err
    }
  PgvectorJson -> PgvectorCliResult
    { exitCode = ExitFailure 1
    , standardOutput = jsonLine $ renderPgvectorErrorJson commandValue.operation err
    , standardError = mempty
    }

renderPgvectorErrorHuman :: PgvectorError -> String
renderPgvectorErrorHuman err = unlines $
  ["Error: " <> safePgvectorErrorMessage err]
  <> case errorStatus err of
    Nothing -> []
    Just statusValue ->
      [ "current status:"
      , renderPgvectorStatus statusValue
      , "next action: " <> T.unpack (pgvectorNextAction statusValue.readiness)
      ]

-- | Human output deliberately builds on the core renderer, keeping the
-- catalog interpretation in one place while adding CLI exit/remediation
-- context.
renderPgvectorStatusHuman :: PgvectorStatus -> String
renderPgvectorStatusHuman statusValue = unlines
  [ "=== hmem-ctl pgvector status ==="
  , "database: reachable"
  , "required contract: nullable vector(1536), no default; valid HNSW vector_cosine_ops index"
  , renderPgvectorStatus statusValue
  , "next action: " <> T.unpack (pgvectorNextAction statusValue.readiness)
  ]

renderPgvectorEnableHuman :: PgvectorProvisionReport -> String
renderPgvectorEnableHuman report = unlines
  [ "=== hmem-ctl pgvector enable ==="
  , "result: " <> outcomeText report.outcome
  , "changes: " <> changesText report.outcome
  , "maintenance warning: " <> T.unpack report.maintenanceImplication
  , "verified status:"
  , renderPgvectorStatus report.status
  , "next action: " <> T.unpack (pgvectorNextAction report.status.readiness)
  ]
  where
    outcomeText = \case
      PgvectorAlreadyReady -> "already ready (no changes)"
      PgvectorProvisioned {} -> "provisioned and verified"

    changesText = \case
      PgvectorAlreadyReady -> "none"
      PgvectorProvisioned actions -> commaSeparated $ map actionHuman actions

renderPgvectorStatusJson :: PgvectorStatus -> Aeson.Value
renderPgvectorStatusJson statusValue = Aeson.object
  [ "kind" Aeson..= ("pgvector_status" :: Text)
  , "ok" Aeson..= True
  , "database" Aeson..= Aeson.object ["state" Aeson..= ("reachable" :: Text)]
  , "ready" Aeson..= isReady statusValue.readiness
  , "package" Aeson..= packageJson statusValue.packageStatus
  , "extension" Aeson..= extensionJson statusValue.extensionStatus
  , "observations_table" Aeson..= tableJson statusValue.observationsTableStatus
  , "embedding_column" Aeson..= columnJson statusValue.embeddingColumnStatus
  , "embedding_index" Aeson..= indexJson statusValue.embeddingIndexStatus
  , "counts" Aeson..= maybe Aeson.Null countsJson statusValue.observationCounts
  , "readiness" Aeson..= readinessJson statusValue.readiness
  , "next_action" Aeson..= pgvectorNextAction statusValue.readiness
  ]

renderPgvectorEnableJson :: PgvectorProvisionReport -> Aeson.Value
renderPgvectorEnableJson report = Aeson.object
  [ "kind" Aeson..= ("pgvector_enable" :: Text)
  , "ok" Aeson..= True
  , "result" Aeson..= case report.outcome of
      PgvectorAlreadyReady -> ("already_ready" :: Text)
      PgvectorProvisioned {} -> "provisioned"
  , "changes" Aeson..= case report.outcome of
      PgvectorAlreadyReady -> ([] :: [Text])
      PgvectorProvisioned actions -> map actionCode actions
  , "maintenance_warning" Aeson..= report.maintenanceImplication
  , "status" Aeson..= renderPgvectorStatusJson report.status
  ]

renderPgvectorErrorJson :: PgvectorCliOperation -> PgvectorError -> Aeson.Value
renderPgvectorErrorJson operationValue err = Aeson.object $
  [ "kind" Aeson..= ("pgvector_error" :: Text)
  , "ok" Aeson..= False
  , "command" Aeson..= operationCode operationValue
  , "error" Aeson..= Aeson.object
      [ "code" Aeson..= errorCode err
      , "message" Aeson..= safePgvectorErrorMessage err
      ]
  ] <> maybe [] (\statusValue -> ["status" Aeson..= renderPgvectorStatusJson statusValue])
    (errorStatus err)

-- | Raw driver/session messages are intentionally not included.  They can
-- contain connection settings supplied by an external operator.  The typed
-- category and safe remediation remain available in both output formats.
safePgvectorErrorMessage :: PgvectorError -> String
safePgvectorErrorMessage = \case
  PgvectorDatabaseUnavailable {} ->
    "could not connect to the configured PostgreSQL database; verify it is reachable and the configured credentials are valid"
  PgvectorInspectionFailed {} ->
    "could not inspect pgvector readiness; verify the configured role can read PostgreSQL catalogs and public.observations"
  PgvectorProvisionRefused issue _ ->
    "provisioning was refused and no schema changes were committed; "
      <> T.unpack (issueRemediation issue)
  PgvectorPermissionDenied {} ->
    "PostgreSQL denied permission; use a configured role that may CREATE EXTENSION and owns or may alter public.observations, then retry"
  PgvectorProvisionFailed {} ->
    "provisioning failed and was rolled back; check PostgreSQL logs, statement_timeout, and server pgvector availability, then retry"
  PgvectorPostconditionFailed {} ->
    "provisioning did not reach the exact required state and was rolled back; review the reported schema state before retrying"

errorCode :: PgvectorError -> Text
errorCode = \case
  PgvectorDatabaseUnavailable {} -> "database_unavailable"
  PgvectorInspectionFailed {} -> "inspection_failed"
  PgvectorProvisionRefused {} -> "provision_refused"
  PgvectorPermissionDenied {} -> "permission_denied"
  PgvectorProvisionFailed {} -> "provision_failed"
  PgvectorPostconditionFailed {} -> "postcondition_failed"

errorStatus :: PgvectorError -> Maybe PgvectorStatus
errorStatus = \case
  PgvectorProvisionRefused _ statusValue -> Just statusValue
  PgvectorPostconditionFailed statusValue -> Just statusValue
  _ -> Nothing

pgvectorNextAction :: PgvectorReadiness -> Text
pgvectorNextAction = \case
  PgvectorReady -> "none; pgvector storage and cosine search are ready"
  PgvectorNotReady issue -> issueRemediation issue

issueRemediation :: PgvectorReadinessIssue -> Text
issueRemediation = \case
  PgvectorIssuePackageMissing ->
    "install the pgvector package on the configured PostgreSQL server, then rerun hmem-ctl pgvector enable"
  PgvectorIssueExtensionMissing ->
    "run hmem-ctl pgvector enable to install the vector extension in this database"
  PgvectorIssueExtensionUnusable ->
    "repair the installed vector extension so its vector type, hnsw method, and vector_cosine_ops are visible and extension-owned"
  PgvectorIssueObservationsTableMissing ->
    "apply normal hmem migrations, then rerun hmem-ctl pgvector enable"
  PgvectorIssueObservationsTableDrift ->
    "repair the incompatible public.observations relation manually; hmem-ctl will not replace it"
  PgvectorIssueEmbeddingColumnMissing ->
    "run hmem-ctl pgvector enable to add the nullable vector(1536) embedding column"
  PgvectorIssueEmbeddingColumnDrift ->
    "repair the incompatible embedding column manually; hmem-ctl will not rewrite or drop it"
  PgvectorIssueEmbeddingIndexMissing ->
    "run hmem-ctl pgvector enable to create the HNSW vector_cosine_ops index"
  PgvectorIssueEmbeddingIndexInvalid ->
    "review and remove or repair the invalid/incomplete index before rerunning hmem-ctl pgvector enable"
  PgvectorIssueEmbeddingIndexDrift ->
    "repair the incompatible index manually; hmem-ctl will not rewrite or drop it"

packageJson :: PgvectorPackageStatus -> Aeson.Value
packageJson = \case
  PgvectorPackageUnavailable -> Aeson.object
    [ "state" Aeson..= ("unavailable" :: Text)
    , "available" Aeson..= False
    , "default_version" Aeson..= Aeson.Null
    ]
  PgvectorPackageAvailable version -> Aeson.object
    [ "state" Aeson..= ("available" :: Text)
    , "available" Aeson..= True
    , "default_version" Aeson..= version
    ]

extensionJson :: PgvectorExtensionStatus -> Aeson.Value
extensionJson = \case
  PgvectorExtensionNotInstalled -> Aeson.object
    [ "state" Aeson..= ("not_installed" :: Text)
    , "installed_version" Aeson..= Aeson.Null
    , "schema" Aeson..= Aeson.Null
    ]
  PgvectorExtensionInstalled version schemaName -> Aeson.object
    [ "state" Aeson..= ("installed" :: Text)
    , "installed_version" Aeson..= version
    , "schema" Aeson..= schemaName
    ]
  PgvectorExtensionIncompatible version schemaName typePresent typeVisible
      hnswPresent hnswOwned cosinePresent cosineVisible cosineOwned cosineMatches ->
    Aeson.object
      [ "state" Aeson..= ("incompatible" :: Text)
      , "installed_version" Aeson..= version
      , "schema" Aeson..= schemaName
      , "capabilities" Aeson..= Aeson.object
          [ "vector_type_present" Aeson..= typePresent
          , "vector_type_visible" Aeson..= typeVisible
          , "hnsw_present" Aeson..= hnswPresent
          , "hnsw_extension_owned" Aeson..= hnswOwned
          , "vector_cosine_ops_present" Aeson..= cosinePresent
          , "vector_cosine_ops_visible" Aeson..= cosineVisible
          , "vector_cosine_ops_extension_owned" Aeson..= cosineOwned
          , "vector_cosine_ops_matches_vector_hnsw" Aeson..= cosineMatches
          ]
      ]

tableJson :: PgvectorObservationsTableStatus -> Aeson.Value
tableJson = \case
  PgvectorObservationsTableMissing -> Aeson.object
    ["state" Aeson..= ("missing" :: Text)]
  PgvectorObservationsTablePresent -> Aeson.object
    ["state" Aeson..= ("ready" :: Text)]
  PgvectorObservationsTableIncompatible kind -> Aeson.object
    [ "state" Aeson..= ("incompatible" :: Text)
    , "relation_kind" Aeson..= kind
    ]

columnJson :: PgvectorEmbeddingColumnStatus -> Aeson.Value
columnJson columnStatus = Aeson.object $
  [ "state" Aeson..= columnState columnStatus
  , "expected" Aeson..= Aeson.object
      [ "type" Aeson..= ("vector" :: Text)
      , "dimensions" Aeson..= (1536 :: Int)
      , "nullable" Aeson..= True
      , "has_default" Aeson..= False
      ]
  ] <> case columnStatus of
    PgvectorEmbeddingColumnIncompatible details ->
      [ "actual" Aeson..= columnDetailsJson details ]
    _ -> []
  where
    columnState = \case
      PgvectorEmbeddingColumnUnavailable -> ("unavailable" :: Text)
      PgvectorEmbeddingColumnAbsent -> "missing"
      PgvectorEmbeddingColumnReady -> "ready"
      PgvectorEmbeddingColumnIncompatible {} -> "incompatible"

columnDetailsJson :: PgvectorEmbeddingColumnDetails -> Aeson.Value
columnDetailsJson details = Aeson.object
  [ "type_schema" Aeson..= details.typeSchema
  , "type_name" Aeson..= details.typeName
  , "dimensions" Aeson..= details.dimensions
  , "nullable" Aeson..= details.nullable
  , "has_default" Aeson..= details.hasDefault
  ]

indexJson :: PgvectorEmbeddingIndexStatus -> Aeson.Value
indexJson indexStatus = Aeson.object $
  [ "state" Aeson..= indexState indexStatus
  , "expected" Aeson..= Aeson.object
      [ "access_method" Aeson..= ("hnsw" :: Text)
      , "operator_class" Aeson..= ("vector_cosine_ops" :: Text)
      , "valid" Aeson..= True
      , "ready" Aeson..= True
      , "live" Aeson..= True
      , "targets_embedding_only" Aeson..= True
      ]
  ] <> case indexStatus of
    PgvectorEmbeddingIndexInvalid details ->
      [ "actual" Aeson..= indexDetailsJson details ]
    PgvectorEmbeddingIndexIncompatible details ->
      [ "actual" Aeson..= indexDetailsJson details ]
    _ -> []
  where
    indexState = \case
      PgvectorEmbeddingIndexUnavailable -> ("unavailable" :: Text)
      PgvectorEmbeddingIndexAbsent -> "missing"
      PgvectorEmbeddingIndexReady -> "ready"
      PgvectorEmbeddingIndexInvalid {} -> "invalid"
      PgvectorEmbeddingIndexIncompatible {} -> "incompatible"

indexDetailsJson :: PgvectorEmbeddingIndexDetails -> Aeson.Value
indexDetailsJson details = Aeson.object
  [ "relation_kind" Aeson..= details.relationKind
  , "valid" Aeson..= details.valid
  , "ready" Aeson..= details.ready
  , "live" Aeson..= details.live
  , "access_method" Aeson..= details.accessMethod
  , "operator_class_schema" Aeson..= details.operatorClassSchema
  , "operator_class_name" Aeson..= details.operatorClassName
  , "key_columns" Aeson..= details.keyColumns
  , "total_columns" Aeson..= details.totalColumns
  , "targets_embedding_column" Aeson..= details.targetsEmbeddingColumn
  , "has_predicate" Aeson..= details.hasPredicate
  , "has_expressions" Aeson..= details.hasExpressions
  , "unique" Aeson..= details.unique
  ]

countsJson :: PgvectorObservationCounts -> Aeson.Value
countsJson counts = Aeson.object
  [ "total" Aeson..= counts.totalObservations
  , "embedded" Aeson..= counts.embeddedObservations
  , "missing" Aeson..= counts.missingEmbeddings
  ]

readinessJson :: PgvectorReadiness -> Aeson.Value
readinessJson = \case
  PgvectorReady -> Aeson.object
    [ "state" Aeson..= ("ready" :: Text)
    , "issue" Aeson..= Aeson.Null
    ]
  PgvectorNotReady issue -> Aeson.object
    [ "state" Aeson..= ("not_ready" :: Text)
    , "issue" Aeson..= issueCode issue
    ]

issueCode :: PgvectorReadinessIssue -> Text
issueCode = \case
  PgvectorIssuePackageMissing -> "package_missing"
  PgvectorIssueExtensionMissing -> "extension_missing"
  PgvectorIssueExtensionUnusable -> "extension_unusable"
  PgvectorIssueObservationsTableMissing -> "observations_table_missing"
  PgvectorIssueObservationsTableDrift -> "observations_table_drift"
  PgvectorIssueEmbeddingColumnMissing -> "embedding_column_missing"
  PgvectorIssueEmbeddingColumnDrift -> "embedding_column_drift"
  PgvectorIssueEmbeddingIndexMissing -> "embedding_index_missing"
  PgvectorIssueEmbeddingIndexInvalid -> "embedding_index_invalid"
  PgvectorIssueEmbeddingIndexDrift -> "embedding_index_drift"

actionCode :: PgvectorProvisionAction -> Text
actionCode = \case
  InstallPgvectorExtension -> "install_vector_extension"
  AddObservationsEmbeddingColumn -> "add_embedding_column"
  CreateObservationsEmbeddingIndex -> "create_embedding_index"

actionHuman :: PgvectorProvisionAction -> String
actionHuman = \case
  InstallPgvectorExtension -> "installed vector extension"
  AddObservationsEmbeddingColumn -> "added nullable vector(1536) embedding column"
  CreateObservationsEmbeddingIndex -> "created HNSW vector_cosine_ops index"

operationCode :: PgvectorCliOperation -> Text
operationCode = \case
  PgvectorStatusOperation -> "status"
  PgvectorEnableOperation -> "enable"

isReady :: PgvectorReadiness -> Bool
isReady PgvectorReady = True
isReady PgvectorNotReady {} = False

commaSeparated :: [String] -> String
commaSeparated = \case
  [] -> "none"
  [value] -> value
  value : values -> value <> ", " <> commaSeparated values

textLine :: String -> ByteString
textLine value = BL8.pack value <> "\n"

jsonLine :: Aeson.Value -> ByteString
jsonLine value = Aeson.encode value <> "\n"
