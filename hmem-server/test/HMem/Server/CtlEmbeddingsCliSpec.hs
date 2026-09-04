module HMem.Server.CtlEmbeddingsCliSpec (spec) where

import Control.Exception (AsyncException(ThreadKilled), bracket, throwIO)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List (isInfixOf)
import Data.Text (Text)
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Options.Applicative
import System.Directory (getTemporaryDirectory, removeFile)
import System.Exit (ExitCode(..))
import System.IO
  ( Handle
  , SeekMode(..)
  , hClose
  , hFlush
  , hSeek
  , openBinaryTempFile
  )
import Test.Hspec

import HMem.Server.CtlEmbeddings
import HMem.Server.CtlEmbeddingsCli
import HMem.Server.CtlPgvector
import HMem.Types

spec :: Spec
spec = describe "hmem-ctl embeddings CLI" $ do
  describe "parser and help" $ do
    it "parses export defaults and explicit all/workspace/Windows output options" $ do
      parseCommand ["export"] `shouldBe` Right (EmbeddingsExportCommand EmbeddingsExportCliOptions
        { exportAll = False, workspaceId = Nothing, outputPath = Nothing
        , pageSize = defaultEmbeddingExportPageSize })
      parseCommand
        [ "export", "--all", "--workspace", show workspaceId
        , "--output", "C:\\temp\\embedding work.ndjson", "--page-size", "17"
        ] `shouldBe` Right (EmbeddingsExportCommand EmbeddingsExportCliOptions
          { exportAll = True, workspaceId = Just workspaceId
          , outputPath = Just "C:\\temp\\embedding work.ndjson", pageSize = 17 })

    it "parses import stdin/file and JSON modes and rejects operation-specific flags" $ do
      parseCommand ["import"] `shouldBe` Right
        (EmbeddingsImportCommand $ EmbeddingsImportCliOptions Nothing EmbeddingsHuman)
      parseCommand ["import", "--input", "C:\\temp\\results.ndjson", "--json"]
        `shouldBe` Right (EmbeddingsImportCommand $
          EmbeddingsImportCliOptions (Just "C:\\temp\\results.ndjson") EmbeddingsJson)
      parseCommand ["export", "--json"] `shouldSatisfy` isLeft
      parseCommand ["import", "--all"] `shouldSatisfy` isLeft
      parseCommand ["export", "--workspace", "not-a-uuid"] `shouldSatisfy` isLeft

    it "documents provider neutrality, streams, retry atomicity, and exits" $ do
      let helpText = renderHelp
      helpText `shouldSatisfy` isInfixOf "Available commands:"
      helpText `shouldSatisfy` isInfixOf "export"
      helpText `shouldSatisfy` isInfixOf "import"
      helpText `shouldSatisfy` isInfixOf "commits each"
      helpText `shouldSatisfy` isInfixOf "record independently"
      helpText `shouldSatisfy` isInfixOf "never invokes an embedding"
      helpText `shouldSatisfy` isInfixOf "exit 2"

  describe "stream dispatch, rendering, and exit behavior" $ do
    it "writes export NDJSON to stdout and keeps its summary on stderr" $
      withHandles "" $ \input output errors -> do
        exitCode <- runEmbeddingsCommandWith successfulOperations input output errors $
          EmbeddingsExportCommand $ EmbeddingsExportCliOptions
            False Nothing Nothing defaultEmbeddingExportPageSize
        exitCode `shouldBe` ExitSuccess
        standardOutput <- readHandle output
        standardError <- readHandle errors
        BS8.lines standardOutput `shouldSatisfy` ((== 1) . length)
        standardOutput `shouldSatisfy` BS8.isInfixOf "\"content_fingerprint\""
        standardOutput `shouldSatisfy` (not . BS8.isInfixOf "\"embedding\"")
        standardError `shouldSatisfy` BS8.isInfixOf "exported 1 observation"

    it "uses an explicit output file without contaminating it with human text" $
      withHandles "" $ \input output errors ->
        withTempPath "hmem-embedding-export.ndjson" $ \path -> do
          exitCode <- runEmbeddingsCommandWith successfulOperations input output errors $
            EmbeddingsExportCommand $ EmbeddingsExportCliOptions
              True (Just workspaceId) (Just path) 1
          exitCode `shouldBe` ExitSuccess
          fileBytes <- BS.readFile path
          BS8.lines fileBytes `shouldSatisfy` ((== 1) . length)
          fileBytes `shouldSatisfy` (not . BS8.isInfixOf "exported")
          readHandle output `shouldReturnContains` "exported 1 observation"

    it "reads import stdin, emits redacted result NDJSON, and exits 2 on any rejection" $
      withHandles "first\r\nsecond\n" $ \input output errors -> do
        seenRef <- newIORef ([] :: [ByteString])
        let operations = successfulOperations
              { importOperation = \nextLine emit -> do
                  first <- nextLine
                  second <- nextLine
                  third <- nextLine
                  writeIORef seenRef [lineBytes | Just lineBytes <- [first, second]]
                  third `shouldBe` Nothing
                  emit appliedResult
                  emit staleResult
                  pure $ Right $ EmbeddingImportSummary 2 1 0 1 0 0 0
              }
        exitCode <- runEmbeddingsCommandWith operations input output errors $
          EmbeddingsImportCommand $ EmbeddingsImportCliOptions Nothing EmbeddingsJson
        exitCode `shouldBe` ExitFailure 2
        readIORef seenRef `shouldReturn` ["first", "second"]
        standardOutput <- readHandle output
        length (BS8.lines standardOutput) `shouldBe` 3
        standardOutput `shouldSatisfy` BS8.isInfixOf "\"outcome\":\"stale\""
        standardOutput `shouldSatisfy` BS8.isInfixOf "\"ok\":false"
        standardOutput `shouldSatisfy` (not . BS8.isInfixOf "0.25")

    it "reads an explicit path and returns exit 0 for applied/already-satisfied retries" $
      withHandles "ignored" $ \input output errors ->
        withTempPath "hmem-embedding-import.ndjson" $ \path -> do
          BS.writeFile path "from-file\n"
          seenRef <- newIORef Nothing
          let operations = successfulOperations
                { importOperation = \nextLine emit -> do
                    nextLine >>= writeIORef seenRef
                    emit appliedResult
                    emit alreadyResult
                    pure $ Right $ EmbeddingImportSummary 2 1 1 0 0 0 0
                }
          exitCode <- runEmbeddingsCommandWith operations input output errors $
            EmbeddingsImportCommand $ EmbeddingsImportCliOptions (Just path) EmbeddingsHuman
          exitCode `shouldBe` ExitSuccess
          readIORef seenRef `shouldReturn` Just "from-file"
          readHandle output `shouldReturnContains` "already_satisfied"

    it "bounds and drains an oversized stdin line before continuing at final EOF" $
      withHandles (oversizedInput "after-stdin") $ \input output errors -> do
        let operations = oversizedImportOperations "after-stdin"
        exitCode <- runEmbeddingsCommandWith operations input output errors $
          EmbeddingsImportCommand $ EmbeddingsImportCliOptions Nothing EmbeddingsJson
        exitCode `shouldBe` ExitFailure 2
        standardOutput <- readHandle output
        BS8.lines standardOutput `shouldSatisfy` ((== 3) . length)
        standardOutput `shouldSatisfy` BS8.isInfixOf "line exceeds the 1 MiB input limit"
        standardOutput `shouldSatisfy` BS8.isInfixOf "\"ok\":false"
        BS.length standardOutput `shouldSatisfy` (< 4096)

    it "bounds and drains an oversized file line before continuing after CRLF" $
      withHandles "ignored" $ \input output errors ->
        withTempPath "hmem-embedding-oversized.ndjson" $ \path -> do
          BS.writeFile path $ oversizedInput "after-file\r\n"
          let operations = oversizedImportOperations "after-file"
          exitCode <- runEmbeddingsCommandWith operations input output errors $
            EmbeddingsImportCommand $ EmbeddingsImportCliOptions (Just path) EmbeddingsHuman
          exitCode `shouldBe` ExitFailure 2
          standardOutput <- readHandle output
          standardOutput `shouldSatisfy` BS8.isInfixOf "outcome=rejected"
          standardOutput `shouldSatisfy` BS8.isInfixOf "line exceeds the 1 MiB input limit"
          standardOutput `shouldSatisfy` BS8.isInfixOf "rejected=1"
          BS.length standardOutput `shouldSatisfy` (< 4096)

    it "preserves the exact 1 MiB boundary with CRLF and a final unterminated line" $
      withHandles
        (BS.replicate maxEmbeddingImportLineBytes 121 <> "\r\nafter-boundary") $
        \input output errors -> do
          let operations = successfulOperations
                { importOperation = \nextLine _ -> do
                    first <- nextLine
                    second <- nextLine
                    third <- nextLine
                    fmap BS.length first `shouldBe` Just maxEmbeddingImportLineBytes
                    fmap (BS.all (== 121)) first `shouldBe` Just True
                    second `shouldBe` Just "after-boundary"
                    third `shouldBe` Nothing
                    pure $ Right $ EmbeddingImportSummary 0 0 0 0 0 0 0
                }
          runEmbeddingsCommandWith operations input output errors
            (EmbeddingsImportCommand $ EmbeddingsImportCliOptions Nothing EmbeddingsJson)
            `shouldReturn` ExitSuccess

    it "rethrows asynchronous cancellation after bounded input reads" $
      withHandles (oversizedInput "after-cancel") $ \input output errors -> do
        let operations = successfulOperations
              { importOperation = \nextLine _ -> do
                  first <- nextLine
                  fmap BS.length first `shouldBe` Just (maxEmbeddingImportLineBytes + 1)
                  throwIO ThreadKilled
              }
        runEmbeddingsCommandWith operations input output errors
          (EmbeddingsImportCommand $ EmbeddingsImportCliOptions Nothing EmbeddingsJson)
          `shouldThrow` (== ThreadKilled)

    it "maps unavailable capability to redacted exit 1" $
      withHandles "" $ \input output errors -> do
        let operations = successfulOperations
              { exportOperation = \_ _ -> pure $ Left $
                  EmbeddingsPgvectorNotReady (PgvectorNotReady PgvectorIssueEmbeddingColumnMissing)
              }
        exitCode <- runEmbeddingsCommandWith operations input output errors $
          EmbeddingsExportCommand $ EmbeddingsExportCliOptions
            False Nothing Nothing defaultEmbeddingExportPageSize
        exitCode `shouldBe` ExitFailure 1
        errorOutput <- readHandle errors
        errorOutput `shouldSatisfy` BS8.isInfixOf "hmem-ctl pgvector status"
        errorOutput `shouldSatisfy` (not . BS8.isInfixOf "password")
        errorOutput `shouldSatisfy` (not . BS8.isInfixOf "\"embedding\":[")

successfulOperations :: EmbeddingsCliOperations
successfulOperations = EmbeddingsCliOperations
  { exportOperation = \_ emit -> emit exportRecord >> pure (Right 1)
  , importOperation = \_ emit -> do
      emit appliedResult
      pure $ Right $ EmbeddingImportSummary 1 1 0 0 0 0 0
  }

oversizedImportOperations :: ByteString -> EmbeddingsCliOperations
oversizedImportOperations expectedNext = successfulOperations
  { importOperation = \nextLine emit -> do
      first <- nextLine
      second <- nextLine
      third <- nextLine
      fmap BS.length first `shouldBe` Just (maxEmbeddingImportLineBytes + 1)
      fmap (BS.all (== 120)) first `shouldBe` Just True
      second `shouldBe` Just expectedNext
      third `shouldBe` Nothing
      emit oversizedResult
      emit appliedLineTwoResult
      pure $ Right $ EmbeddingImportSummary 2 1 0 0 0 1 0
  }

oversizedInput :: ByteString -> ByteString
oversizedInput nextRecord =
  BS.replicate (maxEmbeddingImportLineBytes + 128 * 1024) 120
    <> "\r\n"
    <> nextRecord

exportRecord :: EmbeddingExportRecord
exportRecord = EmbeddingExportRecord
  { formatVersion = embeddingExchangeVersion
  , observationId = observationId
  , workspaceId = workspaceId
  , gitSha = canonicalSha
  , subjects = [ObservationSubject SubjectFile "src/Main.hs"]
  , content = "content"
  , contentFingerprint = fingerprint
  }

appliedResult, alreadyResult, staleResult :: EmbeddingImportResult
appliedResult = importResult EmbeddingImportApplied
alreadyResult = importResult EmbeddingImportAlreadySatisfied
staleResult = importResult EmbeddingImportStale

oversizedResult, appliedLineTwoResult :: EmbeddingImportResult
oversizedResult = EmbeddingImportResult
  { lineNumber = 1
  , observationId = Nothing
  , workspaceId = Nothing
  , outcome = EmbeddingImportRejected EmbeddingRecordTooLarge
  }
appliedLineTwoResult = EmbeddingImportResult
  { lineNumber = 2
  , observationId = Just observationId
  , workspaceId = Just workspaceId
  , outcome = EmbeddingImportApplied
  }

importResult :: EmbeddingImportOutcome -> EmbeddingImportResult
importResult resultOutcome = EmbeddingImportResult
  { lineNumber = 1
  , observationId = Just observationId
  , workspaceId = Just workspaceId
  , outcome = resultOutcome
  }

parseCommand :: [String] -> Either String EmbeddingsCliCommand
parseCommand arguments = case execParserPure defaultPrefs (embeddingsCommandInfo id) arguments of
  Success commandValue -> Right commandValue
  Failure failure -> Left $ fst $ renderFailure failure "hmem-ctl embeddings"
  CompletionInvoked _ -> Left "completion invoked"

renderHelp :: String
renderHelp = case execParserPure defaultPrefs (embeddingsCommandInfo id) ["--help"] of
  Failure failure -> fst $ renderFailure failure "hmem-ctl embeddings"
  _ -> ""

isLeft :: Either a b -> Bool
isLeft Left {} = True
isLeft _ = False

withHandles :: ByteString -> (Handle -> Handle -> Handle -> IO a) -> IO a
withHandles inputBytes runTest = do
  tempDir <- getTemporaryDirectory
  bracket
    (do
      (inputPath, input) <- openBinaryTempFile tempDir "hmem-embedding-stdin"
      (outputPath, output) <- openBinaryTempFile tempDir "hmem-embedding-stdout"
      (errorPath, errors) <- openBinaryTempFile tempDir "hmem-embedding-stderr"
      BS.hPut input inputBytes
      hSeek input AbsoluteSeek 0
      pure ((inputPath, input), (outputPath, output), (errorPath, errors)))
    (\((inputPath, input), (outputPath, output), (errorPath, errors)) -> do
      mapM_ hClose [input, output, errors]
      mapM_ removeFile [inputPath, outputPath, errorPath])
    (\((_, input), (_, output), (_, errors)) -> runTest input output errors)

withTempPath :: String -> (FilePath -> IO a) -> IO a
withTempPath template runTest = do
  tempDir <- getTemporaryDirectory
  bracket
    (openBinaryTempFile tempDir template)
    (\(path, handle) -> hClose handle >> removeFile path)
    (\(path, handle) -> hClose handle >> runTest path)

readHandle :: Handle -> IO ByteString
readHandle handle = do
  hFlush handle
  hSeek handle AbsoluteSeek 0
  BS.hGetContents handle

shouldReturnContains :: IO ByteString -> ByteString -> Expectation
shouldReturnContains ioAction needle = ioAction >>= (`shouldSatisfy` BS8.isInfixOf needle)

fingerprint :: Text
fingerprint = "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"

canonicalSha :: Text
canonicalSha = "0123456789abcdef0123456789abcdef01234567"

observationId, workspaceId :: UUID
observationId = uuid "aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee"
workspaceId = uuid "11111111-2222-3333-4444-555555555555"

uuid :: String -> UUID
uuid raw = case UUID.fromString raw of
  Just parsed -> parsed
  Nothing -> error "invalid UUID fixture"
