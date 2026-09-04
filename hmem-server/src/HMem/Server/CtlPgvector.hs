module HMem.Server.CtlPgvector
  ( PgvectorPackageStatus(..)
  , PgvectorExtensionStatus(..)
  , PgvectorObservationsTableStatus(..)
  , PgvectorEmbeddingColumnDetails(..)
  , PgvectorEmbeddingColumnStatus(..)
  , PgvectorEmbeddingIndexDetails(..)
  , PgvectorEmbeddingIndexStatus(..)
  , PgvectorObservationCounts(..)
  , PgvectorReadinessIssue(..)
  , PgvectorReadiness(..)
  , PgvectorStatus(..)
  , PgvectorProvisionAction(..)
  , PgvectorProvisionOutcome(..)
  , PgvectorProvisionReport(..)
  , PgvectorError(..)
  , pgvectorProvisioningMaintenanceNote
  , classifyPgvectorReadiness
  , renderPgvectorStatus
  , renderPgvectorError
  , inspectPgvector
  , inspectPgvectorWithConfig
  , inspectPgvectorWithPool
  , provisionPgvector
  , provisionPgvectorWithConfig
  , provisionPgvectorWithPool
  ) where

import Control.Exception (SomeException, bracket, onException, try)
import Control.Monad (void)
import Data.ByteString (ByteString)
import Data.Int (Int32, Int64)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Pool (Pool, destroyAllResources)
import Data.Text (Text)
import Data.Text qualified as T
import Hasql.Connection qualified as Hasql
import Hasql.Decoders qualified as Dec
import Hasql.Encoders qualified as Enc
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement

import HMem.Config (HMemConfig(..), PoolConfig(..), connectionString, loadConfig)
import HMem.DB.Pool qualified as Pool
import HMem.Types (observationEmbeddingDimensions)

data PgvectorPackageStatus
  = PgvectorPackageUnavailable
  | PgvectorPackageAvailable
      { defaultVersion :: !Text
      }
  deriving stock (Show, Eq)

data PgvectorExtensionStatus
  = PgvectorExtensionNotInstalled
  | PgvectorExtensionInstalled
      { installedVersion :: !Text
      , installedExtensionSchema :: !Text
      }
  | PgvectorExtensionIncompatible
      { installedVersion :: !Text
      , incompatibleExtensionSchema :: !(Maybe Text)
      , vectorTypePresent :: !Bool
      , vectorTypeVisible :: !Bool
      , hnswAccessMethodPresent :: !Bool
      , hnswAccessMethodExtensionOwned :: !Bool
      , cosineOperatorClassPresent :: !Bool
      , cosineOperatorClassVisible :: !Bool
      , cosineOperatorClassExtensionOwned :: !Bool
      , cosineOperatorClassMatchesVectorHnsw :: !Bool
      }
  deriving stock (Show, Eq)

data PgvectorObservationsTableStatus
  = PgvectorObservationsTableMissing
  | PgvectorObservationsTablePresent
  | PgvectorObservationsTableIncompatible
      { relationKind :: !Text
      }
  deriving stock (Show, Eq)

data PgvectorEmbeddingColumnDetails = PgvectorEmbeddingColumnDetails
  { typeSchema :: !(Maybe Text)
  , typeName :: !(Maybe Text)
  , dimensions :: !(Maybe Int)
  , nullable :: !Bool
  , hasDefault :: !Bool
  } deriving stock (Show, Eq)

data PgvectorEmbeddingColumnStatus
  = PgvectorEmbeddingColumnUnavailable
  | PgvectorEmbeddingColumnAbsent
  | PgvectorEmbeddingColumnReady
  | PgvectorEmbeddingColumnIncompatible !PgvectorEmbeddingColumnDetails
  deriving stock (Show, Eq)

data PgvectorEmbeddingIndexDetails = PgvectorEmbeddingIndexDetails
  { relationKind :: !(Maybe Text)
  , valid :: !Bool
  , ready :: !Bool
  , live :: !Bool
  , accessMethod :: !(Maybe Text)
  , operatorClassSchema :: !(Maybe Text)
  , operatorClassName :: !(Maybe Text)
  , keyColumns :: !(Maybe Int)
  , totalColumns :: !(Maybe Int)
  , targetsEmbeddingColumn :: !Bool
  , hasPredicate :: !Bool
  , hasExpressions :: !Bool
  , unique :: !Bool
  } deriving stock (Show, Eq)

data PgvectorEmbeddingIndexStatus
  = PgvectorEmbeddingIndexUnavailable
  | PgvectorEmbeddingIndexAbsent
  | PgvectorEmbeddingIndexReady
  | PgvectorEmbeddingIndexInvalid !PgvectorEmbeddingIndexDetails
  | PgvectorEmbeddingIndexIncompatible !PgvectorEmbeddingIndexDetails
  deriving stock (Show, Eq)

data PgvectorObservationCounts = PgvectorObservationCounts
  { totalObservations :: !Int64
  , embeddedObservations :: !Int64
  , missingEmbeddings :: !Int64
  } deriving stock (Show, Eq)

data PgvectorReadinessIssue
  = PgvectorIssuePackageMissing
  | PgvectorIssueExtensionMissing
  | PgvectorIssueExtensionUnusable
  | PgvectorIssueObservationsTableMissing
  | PgvectorIssueObservationsTableDrift
  | PgvectorIssueEmbeddingColumnMissing
  | PgvectorIssueEmbeddingColumnDrift
  | PgvectorIssueEmbeddingIndexMissing
  | PgvectorIssueEmbeddingIndexInvalid
  | PgvectorIssueEmbeddingIndexDrift
  deriving stock (Show, Eq)

data PgvectorReadiness
  = PgvectorReady
  | PgvectorNotReady !PgvectorReadinessIssue
  deriving stock (Show, Eq)

data PgvectorStatus = PgvectorStatus
  { packageStatus :: !PgvectorPackageStatus
  , extensionStatus :: !PgvectorExtensionStatus
  , observationsTableStatus :: !PgvectorObservationsTableStatus
  , embeddingColumnStatus :: !PgvectorEmbeddingColumnStatus
  , embeddingIndexStatus :: !PgvectorEmbeddingIndexStatus
  , observationCounts :: !(Maybe PgvectorObservationCounts)
  , readiness :: !PgvectorReadiness
  } deriving stock (Show, Eq)

data PgvectorProvisionAction
  = InstallPgvectorExtension
  | AddObservationsEmbeddingColumn
  | CreateObservationsEmbeddingIndex
  deriving stock (Show, Eq)

data PgvectorProvisionOutcome
  = PgvectorAlreadyReady
  | PgvectorProvisioned ![PgvectorProvisionAction]
  deriving stock (Show, Eq)

data PgvectorProvisionReport = PgvectorProvisionReport
  { outcome :: !PgvectorProvisionOutcome
  , status :: !PgvectorStatus
  , maintenanceImplication :: !Text
  } deriving stock (Show, Eq)

data PgvectorError
  = PgvectorDatabaseUnavailable !String
  | PgvectorInspectionFailed !String
  | PgvectorProvisionRefused !PgvectorReadinessIssue !PgvectorStatus
  | PgvectorPermissionDenied !String
  | PgvectorProvisionFailed !String
  | PgvectorPostconditionFailed !PgvectorStatus
  deriving stock (Show, Eq)

-- | Provisioning deliberately uses an ordinary transactional HNSW build.  It
-- keeps extension, column, and index creation atomic, at the cost of holding
-- the table locks required by ALTER TABLE and CREATE INDEX until commit.
pgvectorProvisioningMaintenanceNote :: Text
pgvectorProvisioningMaintenanceNote =
  "Provisioning is atomic and uses a regular HNSW index build; ALTER TABLE and CREATE INDEX can block writes to public.observations until the transaction commits."

classifyPgvectorReadiness
  :: PgvectorPackageStatus
  -> PgvectorExtensionStatus
  -> PgvectorObservationsTableStatus
  -> PgvectorEmbeddingColumnStatus
  -> PgvectorEmbeddingIndexStatus
  -> PgvectorReadiness
classifyPgvectorReadiness package extension table column index
  | PgvectorPackageUnavailable <- package = PgvectorNotReady PgvectorIssuePackageMissing
  | PgvectorExtensionNotInstalled <- extension = PgvectorNotReady PgvectorIssueExtensionMissing
  | PgvectorExtensionIncompatible {} <- extension = PgvectorNotReady PgvectorIssueExtensionUnusable
  | PgvectorObservationsTableMissing <- table = PgvectorNotReady PgvectorIssueObservationsTableMissing
  | PgvectorObservationsTableIncompatible {} <- table = PgvectorNotReady PgvectorIssueObservationsTableDrift
  | PgvectorEmbeddingColumnUnavailable <- column = PgvectorNotReady PgvectorIssueEmbeddingColumnMissing
  | PgvectorEmbeddingColumnAbsent <- column = PgvectorNotReady PgvectorIssueEmbeddingColumnMissing
  | PgvectorEmbeddingColumnIncompatible {} <- column = PgvectorNotReady PgvectorIssueEmbeddingColumnDrift
  | PgvectorEmbeddingIndexUnavailable <- index = PgvectorNotReady PgvectorIssueEmbeddingIndexMissing
  | PgvectorEmbeddingIndexAbsent <- index = PgvectorNotReady PgvectorIssueEmbeddingIndexMissing
  | PgvectorEmbeddingIndexInvalid {} <- index = PgvectorNotReady PgvectorIssueEmbeddingIndexInvalid
  | PgvectorEmbeddingIndexIncompatible {} <- index = PgvectorNotReady PgvectorIssueEmbeddingIndexDrift
  | otherwise = PgvectorReady

renderPgvectorStatus :: PgvectorStatus -> String
renderPgvectorStatus statusValue = intercalate "\n"
  [ "readiness: " <> renderReadiness statusValue.readiness
  , "package: " <> renderPackage statusValue.packageStatus
  , "extension: " <> renderExtension statusValue.extensionStatus
  , "observations table: " <> renderTable statusValue.observationsTableStatus
  , "embedding column: " <> renderColumn statusValue.embeddingColumnStatus
  , "embedding index: " <> renderIndex statusValue.embeddingIndexStatus
  , "observation counts: " <> renderCounts statusValue.observationCounts
  ]
  where
    renderPackage = \case
      PgvectorPackageUnavailable -> "unavailable (install the pgvector package on the PostgreSQL server)"
      PgvectorPackageAvailable version -> "available (default version " <> T.unpack version <> ")"

    renderExtension = \case
      PgvectorExtensionNotInstalled -> "not installed in this database"
      PgvectorExtensionInstalled version schemaName ->
        "installed (version " <> T.unpack version <> ", schema " <> T.unpack schemaName <> ")"
      PgvectorExtensionIncompatible
          version schemaName typePresent typeVisible hnswPresent hnswOwned
          cosinePresent cosineVisible cosineOwned cosineMatches ->
        "incompatible (version " <> T.unpack version
          <> ", schema " <> maybe "unknown" T.unpack schemaName
          <> ", vector type present=" <> show typePresent
          <> ", vector type visible=" <> show typeVisible
          <> ", hnsw present=" <> show hnswPresent
          <> ", hnsw extension-owned=" <> show hnswOwned
          <> ", vector_cosine_ops present=" <> show cosinePresent
          <> ", vector_cosine_ops visible=" <> show cosineVisible
          <> ", vector_cosine_ops extension-owned=" <> show cosineOwned
          <> ", vector_cosine_ops matches vector/hnsw=" <> show cosineMatches
          <> ")"

    renderTable = \case
      PgvectorObservationsTableMissing -> "missing"
      PgvectorObservationsTablePresent -> "ready"
      PgvectorObservationsTableIncompatible kind ->
        "incompatible relation kind " <> T.unpack kind

    renderColumn = \case
      PgvectorEmbeddingColumnUnavailable -> "unavailable because public.observations is not a table"
      PgvectorEmbeddingColumnAbsent -> "missing"
      PgvectorEmbeddingColumnReady -> "ready (nullable vector(1536), no default)"
      PgvectorEmbeddingColumnIncompatible details ->
        "incompatible (type=" <> qualifiedType details
          <> maybe "" (\value -> "(" <> show value <> ")") details.dimensions
          <> ", nullable=" <> show details.nullable
          <> ", has default=" <> show details.hasDefault <> ")"

    qualifiedType details = case (details.typeSchema, details.typeName) of
      (Just schemaName, Just name) -> T.unpack schemaName <> "." <> T.unpack name
      (_, Just name) -> T.unpack name
      _ -> "unknown"

    renderIndex = \case
      PgvectorEmbeddingIndexUnavailable -> "unavailable because public.observations is not a table"
      PgvectorEmbeddingIndexAbsent -> "missing"
      PgvectorEmbeddingIndexReady -> "ready (valid HNSW vector_cosine_ops)"
      PgvectorEmbeddingIndexInvalid details -> "present but invalid/incomplete (" <> renderIndexDetails details <> ")"
      PgvectorEmbeddingIndexIncompatible details -> "incompatible (" <> renderIndexDetails details <> ")"

    renderIndexDetails details = intercalate ", "
      [ "relation kind=" <> maybe "unknown" T.unpack details.relationKind
      , "valid=" <> show details.valid
      , "ready=" <> show details.ready
      , "live=" <> show details.live
      , "access method=" <> maybe "unknown" T.unpack details.accessMethod
      , "operator class=" <> case (details.operatorClassSchema, details.operatorClassName) of
          (Just schemaName, Just name) -> T.unpack schemaName <> "." <> T.unpack name
          (_, Just name) -> T.unpack name
          _ -> "unknown"
      , "key columns=" <> maybe "unknown" show details.keyColumns
      , "total columns=" <> maybe "unknown" show details.totalColumns
      , "targets embedding=" <> show details.targetsEmbeddingColumn
      , "predicate=" <> show details.hasPredicate
      , "expressions=" <> show details.hasExpressions
      , "unique=" <> show details.unique
      ]

    renderCounts = \case
      Nothing -> "unavailable until the embedding column has the expected shape"
      Just counts -> intercalate ", "
        [ "total=" <> show counts.totalObservations
        , "embedded=" <> show counts.embeddedObservations
        , "missing=" <> show counts.missingEmbeddings
        ]

renderPgvectorError :: PgvectorError -> String
renderPgvectorError = \case
  PgvectorDatabaseUnavailable err ->
    "could not connect to the configured PostgreSQL database: " <> err
  PgvectorInspectionFailed err ->
    "could not inspect pgvector readiness in the configured database: " <> err
  PgvectorProvisionRefused issue current ->
    "refusing to provision pgvector because " <> renderIssue issue
      <> "; no schema changes were committed\n" <> renderPgvectorStatus current
  PgvectorPermissionDenied err ->
    "PostgreSQL denied permission while provisioning pgvector; use a configured database role that may CREATE EXTENSION and owns (or may alter) public.observations, then retry: " <> err
  PgvectorProvisionFailed err ->
    "pgvector provisioning failed and was rolled back: " <> err
  PgvectorPostconditionFailed current ->
    "pgvector provisioning did not reach the exact required state and was rolled back\n"
      <> renderPgvectorStatus current

renderReadiness :: PgvectorReadiness -> String
renderReadiness = \case
  PgvectorReady -> "ready"
  PgvectorNotReady issue -> "not ready: " <> renderIssue issue

renderIssue :: PgvectorReadinessIssue -> String
renderIssue = \case
  PgvectorIssuePackageMissing -> "the PostgreSQL server does not provide the pgvector package"
  PgvectorIssueExtensionMissing -> "the vector extension is not installed in this database"
  PgvectorIssueExtensionUnusable -> "the installed vector extension does not expose the required visible, extension-owned vector type, hnsw access method, and vector_cosine_ops operator class"
  PgvectorIssueObservationsTableMissing -> "public.observations is missing; apply normal hmem migrations first"
  PgvectorIssueObservationsTableDrift -> "public.observations exists but is not an ordinary or partitioned table"
  PgvectorIssueEmbeddingColumnMissing -> "public.observations.embedding is missing"
  PgvectorIssueEmbeddingColumnDrift -> "public.observations.embedding is not a nullable extension-owned vector(1536) column without a default"
  PgvectorIssueEmbeddingIndexMissing -> "public.idx_observations_embedding is missing"
  PgvectorIssueEmbeddingIndexInvalid -> "public.idx_observations_embedding is invalid or incomplete and requires operator review before retrying"
  PgvectorIssueEmbeddingIndexDrift -> "public.idx_observations_embedding does not exactly target embedding with HNSW vector_cosine_ops"

inspectPgvector :: IO (Either PgvectorError PgvectorStatus)
inspectPgvector = loadConfig >>= inspectPgvectorWithConfig

inspectPgvectorWithConfig :: HMemConfig -> IO (Either PgvectorError PgvectorStatus)
inspectPgvectorWithConfig cfg = withConfiguredPool cfg inspectPgvectorWithPool

inspectPgvectorWithPool
  :: Pool Hasql.Connection
  -> IO (Either PgvectorError PgvectorStatus)
inspectPgvectorWithPool pool = do
  result <- try @SomeException $ Pool.withConn pool $ \connection -> do
    inspected <- Session.run inspectPgvectorTransaction connection
      `onException` rollbackQuietly connection
    case inspected of
      Left err -> rollbackQuietly connection >> pure (Left err)
      Right statusValue -> pure (Right statusValue)
  pure $ case result of
    Left err -> Left $ PgvectorDatabaseUnavailable (show err)
    Right (Left err) -> Left $ PgvectorInspectionFailed (show err)
    Right (Right statusValue) -> Right statusValue

inspectPgvectorTransaction :: Session.Session PgvectorStatus
inspectPgvectorTransaction = do
  Session.sql "BEGIN ISOLATION LEVEL REPEATABLE READ READ ONLY"
  statusValue <- inspectPgvectorSession
  Session.sql "COMMIT"
  pure statusValue

provisionPgvector :: IO (Either PgvectorError PgvectorProvisionReport)
provisionPgvector = loadConfig >>= provisionPgvectorWithConfig

provisionPgvectorWithConfig
  :: HMemConfig
  -> IO (Either PgvectorError PgvectorProvisionReport)
provisionPgvectorWithConfig cfg = withConfiguredPool cfg provisionPgvectorWithPool

provisionPgvectorWithPool
  :: Pool Hasql.Connection
  -> IO (Either PgvectorError PgvectorProvisionReport)
provisionPgvectorWithPool pool = do
  attempted <- try @SomeException $ Pool.withConn pool $ \connection -> do
    result <- Session.run provisionPgvectorSession connection
      `onException` rollbackQuietly connection
    case result of
      Left err -> do
        rollbackQuietly connection
        pure $ Left $ classifyProvisionSessionError err
      Right report -> pure report
  pure $ case attempted of
    Left err -> Left $ PgvectorDatabaseUnavailable (show err)
    Right result -> result

withConfiguredPool
  :: HMemConfig
  -> (Pool Hasql.Connection -> IO (Either PgvectorError a))
  -> IO (Either PgvectorError a)
withConfiguredPool cfg action = do
  result <- try @SomeException $ bracket
    (Pool.createPool
      (connectionString cfg.database)
      cfg.pool.size
      cfg.pool.idleTimeout
      cfg.pool.statementTimeoutMs)
    destroyAllResources
    action
  pure $ case result of
    Left err -> Left $ PgvectorDatabaseUnavailable (show err)
    Right value -> value

provisionPgvectorSession
  :: Session.Session (Either PgvectorError PgvectorProvisionReport)
provisionPgvectorSession = do
  Session.sql "BEGIN"
  void $ Session.statement pgvectorProvisionLockKey pgvectorProvisionLockStatement
  before <- inspectPgvectorSession
  case provisionPlan before of
    Left issue -> do
      Session.sql "ROLLBACK"
      pure $ Left $ PgvectorProvisionRefused issue before
    Right [] -> do
      Session.sql "COMMIT"
      pure $ Right PgvectorProvisionReport
        { outcome = PgvectorAlreadyReady
        , status = before
        , maintenanceImplication = pgvectorProvisioningMaintenanceNote
        }
    Right actions -> do
      remaining <- case actions of
        InstallPgvectorExtension : rest -> do
          Session.sql $ provisionActionSql InstallPgvectorExtension
          afterInstall <- inspectPgvectorSession
          case afterInstall.extensionStatus of
            PgvectorExtensionInstalled {} -> pure $ Right rest
            _ -> do
              -- An installed package can still expose an old or incomplete
              -- extension.  Refuse it before touching the observations table;
              -- rolling back also removes the just-created extension.
              Session.sql "ROLLBACK"
              pure $ Left $ PgvectorProvisionRefused
                PgvectorIssueExtensionUnusable afterInstall
        _ -> pure $ Right actions
      case remaining of
        Left err -> pure $ Left err
        Right schemaActions -> do
          mapM_ (Session.sql . provisionActionSql) schemaActions
          after <- inspectPgvectorSession
          if after.readiness == PgvectorReady
            then do
              Session.sql "COMMIT"
              pure $ Right PgvectorProvisionReport
                { outcome = PgvectorProvisioned actions
                , status = after
                , maintenanceImplication = pgvectorProvisioningMaintenanceNote
                }
            else do
              Session.sql "ROLLBACK"
              pure $ Left $ PgvectorPostconditionFailed after

provisionPlan
  :: PgvectorStatus
  -> Either PgvectorReadinessIssue [PgvectorProvisionAction]
provisionPlan statusValue = do
  requirePackage statusValue.packageStatus
  requireTable statusValue.observationsTableStatus
  requireCompatibleExtension statusValue.extensionStatus
  requireCompatibleColumn statusValue.embeddingColumnStatus
  requireCompatibleIndex statusValue.embeddingIndexStatus
  pure $ concat
    [ [InstallPgvectorExtension | PgvectorExtensionNotInstalled <- [statusValue.extensionStatus]]
    , [AddObservationsEmbeddingColumn | PgvectorEmbeddingColumnAbsent <- [statusValue.embeddingColumnStatus]]
    , [CreateObservationsEmbeddingIndex | PgvectorEmbeddingIndexAbsent <- [statusValue.embeddingIndexStatus]]
    ]
  where
    requirePackage = \case
      PgvectorPackageUnavailable -> Left PgvectorIssuePackageMissing
      PgvectorPackageAvailable {} -> Right ()

    requireTable = \case
      PgvectorObservationsTableMissing -> Left PgvectorIssueObservationsTableMissing
      PgvectorObservationsTableIncompatible {} -> Left PgvectorIssueObservationsTableDrift
      PgvectorObservationsTablePresent -> Right ()

    requireCompatibleExtension = \case
      PgvectorExtensionIncompatible {} -> Left PgvectorIssueExtensionUnusable
      _ -> Right ()

    requireCompatibleColumn = \case
      PgvectorEmbeddingColumnUnavailable -> Left PgvectorIssueEmbeddingColumnMissing
      PgvectorEmbeddingColumnIncompatible {} -> Left PgvectorIssueEmbeddingColumnDrift
      _ -> Right ()

    requireCompatibleIndex = \case
      PgvectorEmbeddingIndexUnavailable -> Left PgvectorIssueEmbeddingIndexMissing
      PgvectorEmbeddingIndexInvalid {} -> Left PgvectorIssueEmbeddingIndexInvalid
      PgvectorEmbeddingIndexIncompatible {} -> Left PgvectorIssueEmbeddingIndexDrift
      _ -> Right ()

provisionActionSql :: PgvectorProvisionAction -> ByteString
provisionActionSql = \case
  InstallPgvectorExtension ->
    "CREATE EXTENSION IF NOT EXISTS vector WITH SCHEMA public"
  AddObservationsEmbeddingColumn ->
    "ALTER TABLE public.observations ADD COLUMN IF NOT EXISTS embedding vector(1536)"
  CreateObservationsEmbeddingIndex ->
    "CREATE INDEX IF NOT EXISTS idx_observations_embedding ON public.observations USING hnsw (embedding vector_cosine_ops)"

classifyProvisionSessionError :: Session.SessionError -> PgvectorError
classifyProvisionSessionError err
  | sessionSqlState err == Just "42501" = PgvectorPermissionDenied (show err)
  | otherwise = PgvectorProvisionFailed (show err)

sessionSqlState :: Session.SessionError -> Maybe ByteString
sessionSqlState = \case
  Session.QueryError _ _ commandError -> commandSqlState commandError
  Session.PipelineError commandError -> commandSqlState commandError
  where
    commandSqlState = \case
      Session.ResultError (Session.ServerError sqlState _ _ _ _) -> Just sqlState
      _ -> Nothing

rollbackQuietly :: Hasql.Connection -> IO ()
rollbackQuietly connection = void $ try @SomeException $ Session.run (Session.sql "ROLLBACK") connection

pgvectorProvisionLockKey :: Int64
pgvectorProvisionLockKey = 5211589505520083310

pgvectorProvisionLockStatement :: Statement.Statement Int64 Int32
pgvectorProvisionLockStatement = Statement.Statement
  "SELECT 1::int FROM pg_advisory_xact_lock($1)"
  (Enc.param (Enc.nonNullable Enc.int8))
  (Dec.singleRow (Dec.column (Dec.nonNullable Dec.int4)))
  True

inspectPgvectorSession :: Session.Session PgvectorStatus
inspectPgvectorSession = do
  raw <- Session.statement () pgvectorCatalogStatement
  let withoutCounts = statusFromRawCatalog raw Nothing
  counts <- case withoutCounts.embeddingColumnStatus of
    PgvectorEmbeddingColumnReady -> Just <$> Session.statement () observationCountsStatement
    _ -> pure Nothing
  pure $ statusFromRawCatalog raw counts

data RawPgvectorCatalog = RawPgvectorCatalog
  { packageDefaultVersion :: !(Maybe Text)
  , extensionVersion :: !(Maybe Text)
  , extensionSchemaName :: !(Maybe Text)
  , vectorTypeOid :: !(Maybe Int64)
  , vectorTypeIsVisible :: !(Maybe Bool)
  , hnswMethodOid :: !(Maybe Int64)
  , hnswMethodIsExtensionOwned :: !(Maybe Bool)
  , cosineOpclassOid :: !(Maybe Int64)
  , cosineOpclassIsVisible :: !(Maybe Bool)
  , cosineOpclassIsExtensionOwned :: !(Maybe Bool)
  , cosineOpclassMatchesVectorHnsw :: !(Maybe Bool)
  , observationsOid :: !(Maybe Int64)
  , observationsRelkind :: !(Maybe Text)
  , embeddingAttnum :: !(Maybe Int64)
  , embeddingTypeOid :: !(Maybe Int64)
  , embeddingTypeSchema :: !(Maybe Text)
  , embeddingTypeName :: !(Maybe Text)
  , embeddingTypmod :: !(Maybe Int32)
  , embeddingNotNull :: !(Maybe Bool)
  , embeddingHasDefault :: !(Maybe Bool)
  , indexOid :: !(Maybe Int64)
  , indexRelkind :: !(Maybe Text)
  , indexValid :: !(Maybe Bool)
  , indexReady :: !(Maybe Bool)
  , indexLive :: !(Maybe Bool)
  , indexTableOid :: !(Maybe Int64)
  , indexAccessMethod :: !(Maybe Text)
  , indexKeyColumns :: !(Maybe Int32)
  , indexTotalColumns :: !(Maybe Int32)
  , indexKeyAttnum :: !(Maybe Int64)
  , indexOperatorClassSchema :: !(Maybe Text)
  , indexOperatorClassName :: !(Maybe Text)
  , indexOperatorClassInputType :: !(Maybe Int64)
  , indexHasPredicate :: !(Maybe Bool)
  , indexHasExpressions :: !(Maybe Bool)
  , indexUnique :: !(Maybe Bool)
  } deriving stock (Show, Eq)

statusFromRawCatalog
  :: RawPgvectorCatalog
  -> Maybe PgvectorObservationCounts
  -> PgvectorStatus
statusFromRawCatalog raw counts = PgvectorStatus
  { packageStatus = package
  , extensionStatus = extension
  , observationsTableStatus = table
  , embeddingColumnStatus = column
  , embeddingIndexStatus = index
  , observationCounts = counts
  , readiness = classifyPgvectorReadiness package extension table column index
  }
  where
    package = case raw.packageDefaultVersion of
      Nothing -> PgvectorPackageUnavailable
      Just version -> PgvectorPackageAvailable version

    extension = case raw.extensionVersion of
      Nothing -> PgvectorExtensionNotInstalled
      Just version -> case
          ( raw.extensionSchemaName
          , raw.vectorTypeOid
          , raw.vectorTypeIsVisible
          , raw.hnswMethodOid
          , raw.hnswMethodIsExtensionOwned
          , raw.cosineOpclassOid
          , raw.cosineOpclassIsVisible
          , raw.cosineOpclassIsExtensionOwned
          , raw.cosineOpclassMatchesVectorHnsw
          ) of
        ( Just schemaName, Just _, Just True
          , Just _, Just True, Just _, Just True, Just True, Just True
          ) ->
          PgvectorExtensionInstalled version schemaName
        ( schemaName, vectorOid, visible
          , hnswOid, hnswOwned, cosineOid, cosineVisible, cosineOwned, cosineMatches
          ) -> PgvectorExtensionIncompatible
          { installedVersion = version
          , incompatibleExtensionSchema = schemaName
          , vectorTypePresent = maybe False (const True) vectorOid
          , vectorTypeVisible = fromMaybe False visible
          , hnswAccessMethodPresent = maybe False (const True) hnswOid
          , hnswAccessMethodExtensionOwned = fromMaybe False hnswOwned
          , cosineOperatorClassPresent = maybe False (const True) cosineOid
          , cosineOperatorClassVisible = fromMaybe False cosineVisible
          , cosineOperatorClassExtensionOwned = fromMaybe False cosineOwned
          , cosineOperatorClassMatchesVectorHnsw = fromMaybe False cosineMatches
          }

    table = case (raw.observationsOid, raw.observationsRelkind) of
      (Nothing, _) -> PgvectorObservationsTableMissing
      (Just _, Just kind) | kind `elem` ["r", "p"] -> PgvectorObservationsTablePresent
      (Just _, Just kind) -> PgvectorObservationsTableIncompatible kind
      (Just _, Nothing) -> PgvectorObservationsTableIncompatible "unknown"

    column = case table of
      PgvectorObservationsTablePresent -> case raw.embeddingAttnum of
        Nothing -> PgvectorEmbeddingColumnAbsent
        Just _
          | raw.embeddingTypeOid == raw.vectorTypeOid
          , raw.vectorTypeOid /= Nothing
          , raw.embeddingTypmod == Just (fromIntegral observationEmbeddingDimensions)
          , raw.embeddingNotNull == Just False
          , raw.embeddingHasDefault == Just False -> PgvectorEmbeddingColumnReady
          | otherwise -> PgvectorEmbeddingColumnIncompatible columnDetails
      _ -> PgvectorEmbeddingColumnUnavailable

    columnDetails = PgvectorEmbeddingColumnDetails
      { typeSchema = raw.embeddingTypeSchema
      , typeName = raw.embeddingTypeName
      , dimensions = fromIntegral <$> raw.embeddingTypmod
      , nullable = raw.embeddingNotNull == Just False
      , hasDefault = raw.embeddingHasDefault /= Just False
      }

    index = case table of
      PgvectorObservationsTablePresent -> case raw.indexOid of
        Nothing -> PgvectorEmbeddingIndexAbsent
        Just _
          | not indexIsLive -> PgvectorEmbeddingIndexInvalid indexDetails
          | raw.indexRelkind `notElem` [Just "i", Just "I"] -> PgvectorEmbeddingIndexIncompatible indexDetails
          | indexHasExpectedShape -> PgvectorEmbeddingIndexReady
          | otherwise -> PgvectorEmbeddingIndexIncompatible indexDetails
      _ -> PgvectorEmbeddingIndexUnavailable

    indexIsLive =
      raw.indexValid == Just True
      && raw.indexReady == Just True
      && raw.indexLive == Just True

    indexHasExpectedShape =
      raw.indexTableOid == raw.observationsOid
      && raw.indexAccessMethod == Just "hnsw"
      && raw.indexKeyColumns == Just 1
      && raw.indexTotalColumns == Just 1
      && raw.indexKeyAttnum == raw.embeddingAttnum
      && raw.indexOperatorClassName == Just "vector_cosine_ops"
      && raw.indexOperatorClassSchema == raw.extensionSchemaName
      && raw.indexOperatorClassInputType == raw.vectorTypeOid
      && raw.indexHasPredicate == Just False
      && raw.indexHasExpressions == Just False
      && raw.indexUnique == Just False

    indexDetails = PgvectorEmbeddingIndexDetails
      { relationKind = raw.indexRelkind
      , valid = raw.indexValid == Just True
      , ready = raw.indexReady == Just True
      , live = raw.indexLive == Just True
      , accessMethod = raw.indexAccessMethod
      , operatorClassSchema = raw.indexOperatorClassSchema
      , operatorClassName = raw.indexOperatorClassName
      , keyColumns = fromIntegral <$> raw.indexKeyColumns
      , totalColumns = fromIntegral <$> raw.indexTotalColumns
      , targetsEmbeddingColumn = raw.indexKeyAttnum == raw.embeddingAttnum
          && raw.embeddingAttnum /= Nothing
      , hasPredicate = raw.indexHasPredicate == Just True
      , hasExpressions = raw.indexHasExpressions == Just True
      , unique = raw.indexUnique == Just True
      }

pgvectorCatalogStatement :: Statement.Statement () RawPgvectorCatalog
pgvectorCatalogStatement = Statement.Statement catalogSql Enc.noParams decoder True
  where
    decoder = Dec.singleRow $ RawPgvectorCatalog
      <$> nullableText
      <*> nullableText
      <*> nullableText
      <*> nullableInt8
      <*> nullableBool
      <*> nullableInt8
      <*> nullableBool
      <*> nullableInt8
      <*> nullableBool
      <*> nullableBool
      <*> nullableBool
      <*> nullableInt8
      <*> nullableText
      <*> nullableInt8
      <*> nullableInt8
      <*> nullableText
      <*> nullableText
      <*> nullableInt4
      <*> nullableBool
      <*> nullableBool
      <*> nullableInt8
      <*> nullableText
      <*> nullableBool
      <*> nullableBool
      <*> nullableBool
      <*> nullableInt8
      <*> nullableText
      <*> nullableInt4
      <*> nullableInt4
      <*> nullableInt8
      <*> nullableText
      <*> nullableText
      <*> nullableInt8
      <*> nullableBool
      <*> nullableBool
      <*> nullableBool

    nullableText = Dec.column (Dec.nullable Dec.text)
    nullableInt8 = Dec.column (Dec.nullable Dec.int8)
    nullableInt4 = Dec.column (Dec.nullable Dec.int4)
    nullableBool = Dec.column (Dec.nullable Dec.bool)

catalogSql :: ByteString
catalogSql =
  "WITH package AS (\
  \  SELECT default_version FROM pg_available_extensions WHERE name = 'vector'\
  \), extension_info AS (\
  \  SELECT e.oid AS extension_oid, e.extversion, e.extnamespace, n.nspname AS extension_schema,\
  \         vt.oid AS vector_type_oid, pg_type_is_visible(vt.oid) AS vector_type_visible\
  \  FROM pg_extension e\
  \  JOIN pg_namespace n ON n.oid = e.extnamespace\
  \  LEFT JOIN LATERAL (\
  \    SELECT t.oid\
  \    FROM pg_type t\
  \    JOIN pg_depend d\
  \      ON d.classid = 'pg_type'::regclass\
  \     AND d.objid = t.oid\
  \     AND d.refclassid = 'pg_extension'::regclass\
  \     AND d.refobjid = e.oid\
  \     AND d.deptype = 'e'\
  \    WHERE t.typname = 'vector'\
  \    LIMIT 1\
  \  ) vt ON true\
  \  WHERE e.extname = 'vector'\
  \), hnsw_method AS (\
  \  SELECT am.oid, EXISTS (\
  \    SELECT 1 FROM pg_depend d\
  \    JOIN extension_info e ON e.extension_oid = d.refobjid\
  \    WHERE d.classid = 'pg_am'::regclass\
  \      AND d.objid = am.oid\
  \      AND d.refclassid = 'pg_extension'::regclass\
  \      AND d.deptype = 'e'\
  \  ) AS extension_owned\
  \  FROM pg_am am\
  \  WHERE am.amname = 'hnsw'\
  \), cosine_opclass AS (\
  \  SELECT oc.oid, pg_opclass_is_visible(oc.oid) AS is_visible,\
  \         EXISTS (\
  \           SELECT 1 FROM pg_depend d\
  \           WHERE d.classid = 'pg_opclass'::regclass\
  \             AND d.objid = oc.oid\
  \             AND d.refclassid = 'pg_extension'::regclass\
  \             AND d.refobjid = e.extension_oid\
  \             AND d.deptype = 'e'\
  \         ) AS extension_owned,\
  \         (oc.opcintype = e.vector_type_oid AND oc.opcmethod = hm.oid) AS matches_vector_hnsw\
  \  FROM pg_opclass oc\
  \  JOIN extension_info e ON e.extnamespace = oc.opcnamespace\
  \  JOIN hnsw_method hm ON hm.oid = oc.opcmethod\
  \  WHERE oc.opcname = 'vector_cosine_ops'\
  \  LIMIT 1\
  \), observations AS (\
  \  SELECT c.oid, c.relkind::text AS relkind\
  \  FROM pg_class c\
  \  JOIN pg_namespace n ON n.oid = c.relnamespace\
  \  WHERE n.nspname = 'public' AND c.relname = 'observations'\
  \  LIMIT 1\
  \), embedding_column AS (\
  \  SELECT a.attnum::bigint AS attnum, a.atttypid::bigint AS type_oid,\
  \         tn.nspname AS type_schema, t.typname AS type_name, a.atttypmod,\
  \         a.attnotnull, a.atthasdef\
  \  FROM observations o\
  \  JOIN pg_attribute a ON a.attrelid = o.oid\
  \  JOIN pg_type t ON t.oid = a.atttypid\
  \  JOIN pg_namespace tn ON tn.oid = t.typnamespace\
  \  WHERE a.attname = 'embedding' AND a.attnum > 0 AND NOT a.attisdropped\
  \), embedding_index AS (\
  \  SELECT ic.oid, ic.relkind::text AS relkind, i.indisvalid, i.indisready, i.indislive,\
  \         i.indrelid::bigint AS table_oid, iam.amname AS access_method,\
  \         i.indnkeyatts::int AS key_columns, i.indnatts::int AS total_columns,\
  \         i.indkey[0]::bigint AS key_attnum, ocn.nspname AS opclass_schema,\
  \         oc.opcname AS opclass_name, oc.opcintype::bigint AS opclass_input_type,\
  \         (i.indpred IS NOT NULL) AS has_predicate,\
  \         (i.indexprs IS NOT NULL) AS has_expressions, i.indisunique\
  \  FROM pg_class ic\
  \  JOIN pg_namespace icn ON icn.oid = ic.relnamespace\
  \  LEFT JOIN pg_index i ON i.indexrelid = ic.oid\
  \  LEFT JOIN pg_am iam ON iam.oid = ic.relam\
  \  LEFT JOIN pg_opclass oc ON oc.oid = i.indclass[0]\
  \  LEFT JOIN pg_namespace ocn ON ocn.oid = oc.opcnamespace\
  \  WHERE icn.nspname = 'public' AND ic.relname = 'idx_observations_embedding'\
  \  LIMIT 1\
  \)\
  \ SELECT package.default_version, extension_info.extversion, extension_info.extension_schema,\
  \        extension_info.vector_type_oid::bigint, extension_info.vector_type_visible,\
  \        hnsw_method.oid::bigint, hnsw_method.extension_owned,\
  \        cosine_opclass.oid::bigint, cosine_opclass.is_visible,\
  \        cosine_opclass.extension_owned, cosine_opclass.matches_vector_hnsw,\
  \        observations.oid::bigint, observations.relkind,\
  \        embedding_column.attnum, embedding_column.type_oid, embedding_column.type_schema,\
  \        embedding_column.type_name, embedding_column.atttypmod,\
  \        embedding_column.attnotnull, embedding_column.atthasdef,\
  \        embedding_index.oid::bigint, embedding_index.relkind, embedding_index.indisvalid,\
  \        embedding_index.indisready, embedding_index.indislive, embedding_index.table_oid,\
  \        embedding_index.access_method, embedding_index.key_columns, embedding_index.total_columns,\
  \        embedding_index.key_attnum, embedding_index.opclass_schema, embedding_index.opclass_name,\
  \        embedding_index.opclass_input_type, embedding_index.has_predicate,\
  \        embedding_index.has_expressions, embedding_index.indisunique\
  \ FROM (SELECT 1) singleton\
  \ LEFT JOIN package ON true\
  \ LEFT JOIN extension_info ON true\
  \ LEFT JOIN hnsw_method ON true\
  \ LEFT JOIN cosine_opclass ON true\
  \ LEFT JOIN observations ON true\
  \ LEFT JOIN embedding_column ON true\
  \ LEFT JOIN embedding_index ON true"

observationCountsStatement :: Statement.Statement () PgvectorObservationCounts
observationCountsStatement = Statement.Statement
  "SELECT count(*)::bigint, count(embedding)::bigint FROM public.observations"
  Enc.noParams
  (Dec.singleRow $ toCounts
    <$> Dec.column (Dec.nonNullable Dec.int8)
    <*> Dec.column (Dec.nonNullable Dec.int8))
  True
  where
    toCounts total embedded = PgvectorObservationCounts
      { totalObservations = total
      , embeddedObservations = embedded
      , missingEmbeddings = total - embedded
      }
