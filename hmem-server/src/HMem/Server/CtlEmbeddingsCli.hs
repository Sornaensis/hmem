module HMem.Server.CtlEmbeddingsCli
  ( EmbeddingsCliCommand(..)
  , EmbeddingsExportCliOptions(..)
  , EmbeddingsImportCliOptions(..)
  , EmbeddingsCliFormat(..)
  , EmbeddingsCliOperations(..)
  , embeddingsCommandParser
  , embeddingsCommandInfo
  , runEmbeddingsCommand
  , runEmbeddingsCommandWith
  , renderEmbeddingImportResultHuman
  , renderEmbeddingImportResultJson
  , renderEmbeddingImportSummaryHuman
  , renderEmbeddingImportSummaryJson
  , renderEmbeddingsError
  ) where

import Data.Aeson (Value, object, (.=))
import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as LBS
import Data.IORef (IORef, atomicModifyIORef', newIORef, writeIORef)
import Data.Text (Text)
import Data.Text qualified as T
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Options.Applicative
import System.Exit (ExitCode(..))
import System.IO
  ( Handle
  , IOMode(..)
  , hSetBinaryMode
  , stderr
  , stdin
  , stdout
  , withBinaryFile
  )

import HMem.Config (loadConfig)
import HMem.Server.CtlEmbeddings
import HMem.Server.Exception (trySynchronous)
import HMem.Types (EmbeddingSpaceFingerprint, legacyManualEmbeddingSpace, parseEmbeddingSpaceFingerprint)

data EmbeddingsCliCommand
  = EmbeddingsExportCommand !EmbeddingsExportCliOptions
  | EmbeddingsImportCommand !EmbeddingsImportCliOptions
  deriving stock (Show, Eq)

data EmbeddingsExportCliOptions = EmbeddingsExportCliOptions
  { exportAll :: !Bool
  , workspaceId :: !(Maybe UUID)
  , outputPath :: !(Maybe FilePath)
  , pageSize :: !Int
  , targetSpace :: !EmbeddingSpaceFingerprint
  } deriving stock (Show, Eq)

data EmbeddingsCliFormat
  = EmbeddingsHuman
  | EmbeddingsJson
  deriving stock (Show, Eq)

data EmbeddingsImportCliOptions = EmbeddingsImportCliOptions
  { inputPath :: !(Maybe FilePath)
  , format :: !EmbeddingsCliFormat
  } deriving stock (Show, Eq)

data EmbeddingsCliOperations = EmbeddingsCliOperations
  { exportOperation
      :: EmbeddingExportOptions
      -> (EmbeddingExportRecord -> IO ())
      -> IO (Either EmbeddingsError Int)
  , importOperation
      :: IO (Maybe ByteString)
      -> (EmbeddingImportResult -> IO ())
      -> IO (Either EmbeddingsError EmbeddingImportSummary)
  }

embeddingsCommandParser :: Parser EmbeddingsCliCommand
embeddingsCommandParser = subparser
  ( command "export" (info ((EmbeddingsExportCommand <$> exportParser) <**> helper)
      (progDesc "Write version-2 provider-neutral Observation work as NDJSON"))
 <> command "import" (info ((EmbeddingsImportCommand <$> importParser) <**> helper)
      (progDesc "Validate and compare-and-set versioned embedding result NDJSON"))
  )

embeddingsCommandInfo :: (EmbeddingsCliCommand -> a) -> ParserInfo a
embeddingsCommandInfo wrap = info
  ((wrap <$> embeddingsCommandParser) <**> helper)
  ( fullDesc
 <> progDesc "Export Observation inputs or import externally produced 1536-dimensional embeddings"
 <> footer (unwords
      [ "Export writes NDJSON to stdout by default; import reads NDJSON from stdin by default."
      , "Use --output/--input for Windows-safe binary files."
      , "Import commits each valid record independently and is safe to retry:"
      , "exit 0 means every record applied or was already satisfied;"
      , "exit 1 means setup/I/O/capability failure; exit 2 means one or more records were rejected."
      , "hmem never invokes an embedding provider."
      , "The external producer must return exactly 1536 finite numbers from one consistent"
      , "model revision, preprocessing, and vector space for both stored and query vectors."
      , "Content edits clear embeddings; export missing rows to retry, or use export --all"
      , "when replacing the embedding model for the complete corpus."
      ])
  )

exportParser :: Parser EmbeddingsExportCliOptions
exportParser = EmbeddingsExportCliOptions
  <$> switch
      ( long "all"
     <> help "Export all observations instead of only rows with NULL embedding"
      )
  <*> optional (option uuidReader
      ( long "workspace"
     <> metavar "UUID"
     <> help "Restrict export to one exact workspace UUID"
      ))
  <*> optional (strOption
      ( long "output"
     <> short 'o'
     <> metavar "FILE"
     <> help "Write NDJSON to FILE instead of stdout"
      ))
  <*> option auto
      ( long "page-size"
     <> metavar "N"
     <> value defaultEmbeddingExportPageSize
     <> showDefault
     <> help "Bounded database page size (1-1000)"
      )
  <*> option embeddingSpaceReader
      ( long "space-fingerprint"
     <> metavar "FINGERPRINT"
     <> value legacyManualEmbeddingSpace
     <> showDefaultWith (const "hmem:legacy-manual:v1")
     <> help "Exact vector space to export (default: legacy manual space)"
      )

embeddingSpaceReader :: ReadM EmbeddingSpaceFingerprint
embeddingSpaceReader = eitherReader $ \raw ->
  maybe (Left "expected a nonblank embedding space fingerprint without whitespace") Right
    (parseEmbeddingSpaceFingerprint (T.pack raw))

importParser :: Parser EmbeddingsImportCliOptions
importParser = EmbeddingsImportCliOptions
  <$> optional (strOption
      ( long "input"
     <> short 'i'
     <> metavar "FILE"
     <> help "Read NDJSON from FILE instead of stdin"
      ))
  <*> flag EmbeddingsHuman EmbeddingsJson
      ( long "json"
     <> help "Emit per-record and summary results as NDJSON"
      )

uuidReader :: ReadM UUID
uuidReader = eitherReader $ \raw -> case UUID.fromString raw of
  Nothing -> Left "expected an exact UUID"
  Just parsedUuid -> Right parsedUuid

runEmbeddingsCommand :: EmbeddingsCliCommand -> IO ExitCode
runEmbeddingsCommand commandValue = do
  attempted <- trySynchronous $ do
    cfg <- loadConfig
    runEmbeddingsCommandWith EmbeddingsCliOperations
      { exportOperation = exportEmbeddingsWithConfig cfg
      , importOperation = importEmbeddingsWithConfig cfg
      }
      stdin stdout stderr commandValue
  case attempted of
    Left _ -> do
      BS8.hPutStrLn stderr "Error: could not load configuration or access the selected NDJSON stream."
      pure $ ExitFailure 1
    Right exitCode -> pure exitCode

runEmbeddingsCommandWith
  :: EmbeddingsCliOperations
  -> Handle
  -> Handle
  -> Handle
  -> EmbeddingsCliCommand
  -> IO ExitCode
runEmbeddingsCommandWith operations standardInput standardOutput standardError = \case
  EmbeddingsExportCommand options ->
    withOutputHandle standardOutput options.outputPath $ \destination -> do
      result <- operations.exportOperation EmbeddingExportOptions
        { scope = if options.exportAll then ExportAllEmbeddings else ExportMissingEmbeddings
        , workspaceId = options.workspaceId
        , pageSize = options.pageSize
        , targetSpace = options.targetSpace
        }
        (writeExportRecord destination)
      case result of
        Left err -> writeError standardError err
        Right count -> do
          let summary = BS8.pack $ "exported " <> show count <> " observation(s) as version-2 NDJSON\n"
          case options.outputPath of
            Nothing -> BS.hPut standardError summary
            Just _ -> BS.hPut standardOutput summary
          pure ExitSuccess
  EmbeddingsImportCommand options ->
    withInputHandle standardInput options.inputPath $ \source -> do
      nextLine <- newBoundedNdjsonLineReader source
      result <- operations.importOperation
        nextLine
        (writeImportResult standardOutput options.format)
      case result of
        Left err -> writeError standardError err
        Right summary -> do
          writeImportSummary standardOutput options.format summary
          pure $ if importSummarySucceeded summary
            then ExitSuccess
            else ExitFailure 2

withOutputHandle :: Handle -> Maybe FilePath -> (Handle -> IO a) -> IO a
withOutputHandle fallback = \case
  Nothing -> \runWithHandle -> hSetBinaryMode fallback True >> runWithHandle fallback
  Just path -> withBinaryFile path WriteMode

withInputHandle :: Handle -> Maybe FilePath -> (Handle -> IO a) -> IO a
withInputHandle fallback = \case
  Nothing -> \runWithHandle -> hSetBinaryMode fallback True >> runWithHandle fallback
  Just path -> withBinaryFile path ReadMode

writeExportRecord :: Handle -> EmbeddingExportRecord -> IO ()
writeExportRecord handle record = do
  BS.hPut handle $ encodeEmbeddingExportRecord record
  BS8.hPutStrLn handle ""

-- | Build a physical-line reader that never retains more than the configured
-- input limit plus one byte. Once that sentinel is present, later chunks from
-- the same line are discarded until LF so the next call starts at the next
-- record. The fixed-size carry is needed only when a chunk contains both LF
-- and bytes from the following line.
newBoundedNdjsonLineReader :: Handle -> IO (IO (Maybe ByteString))
newBoundedNdjsonLineReader handle = do
  carry <- newIORef BS.empty
  pure $ readBoundedNdjsonLine handle carry

readBoundedNdjsonLine :: Handle -> IORef ByteString -> IO (Maybe ByteString)
readBoundedNdjsonLine handle carry = do
  initial <- atomicModifyIORef' carry $ \bytes -> (BS.empty, bytes)
  consume [] 0 False False initial
  where
    retainLimit = maxEmbeddingImportLineBytes + 1
    chunkSize = 32 * 1024

    consume retained retainedLength truncated sawBytes bytes =
      case BS.elemIndex 10 bytes of
        Just newlineOffset -> do
          let lineFragment = BS.take newlineOffset bytes
              remainder = BS.drop (newlineOffset + 1) bytes
              (retained', _, truncated') =
                retainFragment retained retainedLength truncated lineFragment
          writeIORef carry remainder
          pure $ Just $ finishLine retained' truncated'
        Nothing -> do
          let (retained', retainedLength', truncated') =
                retainFragment retained retainedLength truncated bytes
              sawBytes' = sawBytes || not (BS.null bytes)
          nextChunk <- BS.hGetSome handle chunkSize
          if BS.null nextChunk
            then if sawBytes'
              then pure $ Just $ finishLine retained' truncated'
              else pure Nothing
            else consume retained' retainedLength' truncated' sawBytes' nextChunk

    retainFragment retained retainedLength truncated fragment =
      let available = max 0 (retainLimit - retainedLength)
          kept = BS.take available fragment
          keptLength = BS.length kept
          retained' = if BS.null kept then retained else kept : retained
          truncated' = truncated || BS.length fragment > available
      in (retained', retainedLength + keptLength, truncated')

    finishLine retained truncated =
      let bytes = BS.concat $ reverse retained
      in if truncated
        then bytes
        else BS8.dropWhileEnd (== '\r') bytes

writeImportResult :: Handle -> EmbeddingsCliFormat -> EmbeddingImportResult -> IO ()
writeImportResult handle formatValue result = case formatValue of
  EmbeddingsHuman -> BS8.hPutStrLn handle $ BS8.pack $ renderEmbeddingImportResultHuman result
  EmbeddingsJson -> BS8.hPutStrLn handle $ LBS.toStrict $ Aeson.encode $ renderEmbeddingImportResultJson result

writeImportSummary :: Handle -> EmbeddingsCliFormat -> EmbeddingImportSummary -> IO ()
writeImportSummary handle formatValue summary = case formatValue of
  EmbeddingsHuman -> BS8.hPutStrLn handle $ BS8.pack $ renderEmbeddingImportSummaryHuman summary
  EmbeddingsJson -> BS8.hPutStrLn handle $ LBS.toStrict $ Aeson.encode $ renderEmbeddingImportSummaryJson summary

writeError :: Handle -> EmbeddingsError -> IO ExitCode
writeError handle err = do
  BS8.hPutStrLn handle $ BS8.pack $ "Error: " <> renderEmbeddingsError err
  pure $ ExitFailure 1

renderEmbeddingImportResultHuman :: EmbeddingImportResult -> String
renderEmbeddingImportResultHuman result =
  "line " <> show result.lineNumber
    <> maybe "" (\observationUuid -> ": observation=" <> show observationUuid) result.observationId
    <> maybe "" (\workspaceUuid -> " workspace=" <> show workspaceUuid) result.workspaceId
    <> " outcome=" <> T.unpack (outcomeCode result.outcome)
    <> maybe "" (\detail -> " reason=" <> T.unpack detail) (outcomeDetail result.outcome)

renderEmbeddingImportResultJson :: EmbeddingImportResult -> Value
renderEmbeddingImportResultJson result = object $
  [ "kind" .= ("embedding_import_result" :: Text)
  , "format_version" .= embeddingExchangeVersion
  , "line" .= result.lineNumber
  , "observation_id" .= result.observationId
  , "workspace_id" .= result.workspaceId
  , "outcome" .= outcomeCode result.outcome
  ] <> maybe [] (\detail -> ["reason" .= detail]) (outcomeDetail result.outcome)

renderEmbeddingImportSummaryHuman :: EmbeddingImportSummary -> String
renderEmbeddingImportSummaryHuman summary = unwords
  [ "summary: processed=" <> show summary.processed
  , "applied=" <> show summary.applied
  , "already_satisfied=" <> show summary.alreadySatisfied
  , "stale=" <> show summary.stale
  , "not_found=" <> show summary.notFound
  , "rejected=" <> show summary.rejected
  , "database_errors=" <> show summary.databaseErrors
  ]

renderEmbeddingImportSummaryJson :: EmbeddingImportSummary -> Value
renderEmbeddingImportSummaryJson summary = object
  [ "kind" .= ("embedding_import_summary" :: Text)
  , "format_version" .= embeddingExchangeVersion
  , "processed" .= summary.processed
  , "applied" .= summary.applied
  , "already_satisfied" .= summary.alreadySatisfied
  , "stale" .= summary.stale
  , "not_found" .= summary.notFound
  , "rejected" .= summary.rejected
  , "database_errors" .= summary.databaseErrors
  , "ok" .= importSummarySucceeded summary
  ]

importSummarySucceeded :: EmbeddingImportSummary -> Bool
importSummarySucceeded summary =
  summary.stale == 0
    && summary.notFound == 0
    && summary.rejected == 0
    && summary.databaseErrors == 0

outcomeCode :: EmbeddingImportOutcome -> Text
outcomeCode = \case
  EmbeddingImportApplied -> "applied"
  EmbeddingImportAlreadySatisfied -> "already_satisfied"
  EmbeddingImportStale -> "stale"
  EmbeddingImportNotFound -> "not_found"
  EmbeddingImportRejected {} -> "rejected"
  EmbeddingImportDatabaseError -> "database_error"

outcomeDetail :: EmbeddingImportOutcome -> Maybe Text
outcomeDetail = \case
  EmbeddingImportRejected recordError -> Just $ case recordError of
    EmbeddingRecordTooLarge -> "line exceeds the 1 MiB input limit"
    EmbeddingRecordLimitExceeded -> "input exceeds the 100000-record limit"
    EmbeddingRecordMalformed -> "malformed JSON or invalid field type"
    EmbeddingRecordInvalidShape -> "record fields do not exactly match a supported versioned import shape"
    EmbeddingRecordUnsupportedVersion version -> "unsupported format_version " <> T.pack (show version)
    EmbeddingRecordInvalidFingerprint -> "content_fingerprint must be 64 lowercase hexadecimal characters"
    EmbeddingRecordInvalidVector -> "embedding must contain exactly 1536 finite numbers"
    EmbeddingRecordDuplicateObservation observationId -> "duplicate observation_id " <> T.pack (show observationId)
  _ -> Nothing

renderEmbeddingsError :: EmbeddingsError -> String
renderEmbeddingsError = \case
  EmbeddingsDatabaseUnavailable ->
    "could not connect to the configured PostgreSQL database"
  EmbeddingsPgvectorInspectionFailed ->
    "could not verify pgvector readiness in the configured database"
  EmbeddingsPgvectorNotReady _ ->
    "pgvector is not ready; run 'hmem-ctl pgvector status' and then 'hmem-ctl pgvector enable' if the reported state is safely provisionable"
  EmbeddingsInvalidExportPageSize ->
    "--page-size must be between 1 and 1000"
  EmbeddingsOperationFailed ->
    "the embedding exchange stopped because a database or NDJSON stream operation failed; fix the cause and retry"
