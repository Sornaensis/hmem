#!/usr/bin/env stack
-- stack script --resolver lts-24.2 --package aeson --package yaml --package crypton --package directory --package filepath --package bytestring --package text --package containers --package scientific --package temporary --package process

-- | Offline verifier for the managed TEI provenance lockfile.  It never
-- contacts a registry or model hub: '--artifact-root' must be a pre-fetched
-- snapshot whose exact inventory is checked against the lockfile.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
module Main where

import Control.Monad (forM, forM_, unless, when)
import Crypto.Hash (Context, hashFinalize, hashInit, hashUpdate)
import Crypto.Hash.Algorithms (SHA256)
import Data.Aeson (Object, Value (..))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.Char (isHexDigit)
import Data.List (intercalate, nub, sort)
import Data.Scientific (floatingOrInteger)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Yaml qualified as Yaml
import Control.Exception (SomeException, displayException, try)
import System.Directory (canonicalizePath, createDirectoryLink, createDirectoryIfMissing, doesDirectoryExist, doesFileExist, doesPathExist, listDirectory, pathIsSymbolicLink, removePathForcibly)
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitFailure)
import System.FilePath ((</>), isAbsolute, makeRelative, normalise, splitDirectories, takeDirectory, takeDrive)
import System.Info (os)
import System.IO (IOMode (ReadMode, WriteMode), hSetFileSize, withBinaryFile)
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcessWithExitCode)

data Artifact = Artifact { artifactPath :: FilePath, artifactHash :: String } deriving (Eq, Ord, Show)
data LicenseRecord = LicenseRecord { licenseComponent :: String, licensePath :: FilePath, licenseHash :: String } deriving (Eq, Show)
data Lock = Lock
  { lockModelRoot :: FilePath
  , lockArtifacts :: [Artifact]
  , lockRuntimeRoot :: FilePath
  , lockRuntimeArtifacts :: [Artifact]
  , lockLicenses :: [LicenseRecord]
  } deriving (Eq, Show)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> validate "." Nothing
    ["--root", root] -> validate root Nothing
    ["--root", root, "--artifact-root", artifactRoot] -> validate root (Just artifactRoot)
    ["--self-test"] -> selfTest
    _ -> die ["usage: check-managed-embedding-provenance.hs [--root ROOT [--artifact-root BUNDLE] | --self-test]"]

validate :: FilePath -> Maybe FilePath -> IO ()
validate root artifactRoot = do
  decoded <- Yaml.decodeFileEither (root </> "config/managed-embedding-provenance.yaml")
  case decoded of
    Left err -> die ["invalid YAML: " <> Yaml.prettyPrintParseException err]
    Right value -> do
      case parseLock value of
        Left errors -> die errors
        Right lock -> do
          licenseErrors <- validateLicenses root lock
          artifactErrors <- maybe (pure []) (validateArtifacts lock) artifactRoot
          let errors = licenseErrors <> artifactErrors
          if null errors
            then putStrLn "Managed embedding provenance check passed."
            else die errors

parseLock :: Value -> Either [String] Lock
parseLock = parseLockAgainst expectedRuntimeArtifacts

parseLockAgainst :: [Artifact] -> Value -> Either [String] Lock
parseLockAgainst expectedRuntime value = do
  root <- objectAt "root" value
  requiredEq "schema_version" (Number 1) root
  requiredEq "profile" (String "managed-tei-gte-qwen2-1.5b-instruct") root
  expectedDimensions <- integerAt "expected_embedding_dimensions" root
  whenE (expectedDimensions /= 1536) "expected_embedding_dimensions must be 1536"
  tei <- requiredObject "tei" root
  release <- requiredText "release" tei
  whenE (not ("v" `T.isPrefixOf` release) || T.any (not . validReleaseChar) release) "tei.release must be a release label such as v1.9.3"
  sourceCommit <- requiredText "source_commit" tei
  whenE (not (isSha1 sourceCommit)) "tei.source_commit must be a 40-character immutable commit"
  archive <- requiredObject "source_archive" tei
  archiveUrl <- requiredText "url" archive
  archiveHash <- requiredText "sha256" archive
  whenE (not (T.unpack sourceCommit `isInfix` T.unpack archiveUrl)) "tei.source_archive.url must address the immutable source_commit, not a tag or branch"
  whenE (not (isSha256 archiveHash)) "tei.source_archive.sha256 must be a SHA-256"
  image <- requiredObject "cpu_image" tei
  imageRef <- requiredText "reference" image
  platform <- requiredText "platform" image
  whenE (platform /= "linux/amd64") "tei.cpu_image.platform must be linux/amd64"
  whenE (not (isDigestReference imageRef)) "tei.cpu_image.reference must be a registry digest, not a tag"
  registryUrl <- requiredText "registry_manifest_url" image
  whenE (not ("https://" `T.isPrefixOf` registryUrl)) "tei.cpu_image.registry_manifest_url must be an HTTPS upstream URL"
  whenE (not (registryEvidenceMatches imageRef registryUrl)) "tei.cpu_image.registry_manifest_url must address the cpu_image reference digest, not a floating tag"
  indexDigest <- requiredDigest "index_digest" image
  platformManifestDigest <- requiredDigest "platform_manifest_digest" image
  configDigest <- requiredDigest "config_digest" image
  executableLayerDigest <- requiredDigest "executable_layer_digest" image
  let expectedIndex = "sha256:ad950d30878eceb72aaf32024d26fa2b1d04a75304fa0b4776b49aa1941fea07"
      expectedPlatform = "sha256:c26a226262ad4ff3330fb30b76653c1bb65da2fcf413b92284545a010e0a8a48"
      expectedConfig = "sha256:f50bc806338120e138d774236098580f7e5840dbe8bc001ef64615847c1685e8"
      expectedLayer = "sha256:51345600aa8d1cd784c97a2bc6ba969a535841224986d40ac91770c504e472a9"
  whenE (indexDigest /= expectedIndex || imageRef /= "ghcr.io/huggingface/text-embeddings-inference@" <> expectedIndex) "tei.cpu_image must use the pinned TEI index digest"
  whenE (platformManifestDigest /= expectedPlatform) "tei.cpu_image.platform_manifest_digest must be the pinned linux/amd64 manifest"
  whenE (configDigest /= expectedConfig) "tei.cpu_image.config_digest must be the pinned platform config"
  whenE (executableLayerDigest /= expectedLayer) "tei.cpu_image.executable_layer_digest must be the pinned executable layer"
  runtime <- requiredObject "runtime_bundle" tei
  runtimeRoot <- T.unpack <$> requiredText "artifact_root" runtime
  whenE (not (isSafeRelative runtimeRoot)) "tei.runtime_bundle.artifact_root must be a safe relative path"
  runtimePlatform <- requiredDigest "source_platform_manifest_digest" runtime
  runtimeConfig <- requiredDigest "source_config_digest" runtime
  runtimeLayer <- requiredDigest "source_layer_digest" runtime
  whenE (runtimePlatform /= platformManifestDigest || runtimeConfig /= configDigest || runtimeLayer /= executableLayerDigest) "tei.runtime_bundle derivation must match the pinned platform manifest, config, and executable layer"
  systemAbi <- requiredText "system_abi_source" runtime
  retrievalRuntime <- requiredText "retrieval_phase" runtime
  runtimeUseBundle <- requiredText "runtime_use" runtime
  whenE (systemAbi /= "exact-pinned-platform-image") "tei.runtime_bundle.system_abi_source must require the exact pinned platform image"
  whenE (retrievalRuntime /= "build-time-only" || runtimeUseBundle /= "verified-local-bundle") "tei.runtime_bundle must forbid runtime retrieval and require a verified local bundle"
  loader <- requiredObject "loader" runtime
  inherited <- requiredBool "inherit_environment" loader
  libraryPath <- requiredText "ld_library_path" loader
  preload <- requiredText "ld_preload" loader
  whenE (inherited || libraryPath /= "/usr/local/lib" || preload /= "/usr/local/lib/libfakeintel.so") "tei.runtime_bundle.loader must use the controlled TEI loader environment"
  runtimeArtifacts <- requiredArray "artifacts" runtime >>= traverse parseRuntimeArtifact
  let runtimePaths = map canonicalArtifactPath runtimeArtifacts
  whenE (length runtimePaths /= length (nub runtimePaths)) "tei.runtime_bundle.artifacts contains duplicate paths"
  whenE (sort runtimeArtifacts /= sort expectedRuntime) "tei.runtime_bundle.artifacts must be the complete pinned TEI runtime closure"
  model <- requiredObject "model" root
  modelId <- requiredText "id" model
  whenE (modelId /= "Alibaba-NLP/gte-Qwen2-1.5B-instruct") "model.id must be the sole managed GTE-Qwen2 profile"
  revision <- requiredText "revision" model
  whenE (not (isSha1 revision)) "model.revision must be a 40-character immutable revision, not a tag or branch"
  revisionUrl <- requiredText "revision_url" model
  whenE (not (T.unpack revision `isInfix` T.unpack revisionUrl)) "model.revision_url must contain the immutable revision"
  snapshotUrl <- requiredText "snapshot_url" model
  whenE (not (T.unpack revision `isInfix` T.unpack snapshotUrl)) "model.snapshot_url must contain the immutable revision"
  retrieval <- requiredText "retrieval_phase" model
  runtimeUse <- requiredText "runtime_use" model
  modelRoot <- T.unpack <$> requiredText "artifact_root" model
  whenE (not (isSafeRelative modelRoot)) "model.artifact_root must be a safe relative path"
  whenE (retrieval /= "build-time-only" || runtimeUse /= "local-read-only-snapshot") "manifest must separate build-time retrieval from read-only runtime use"
  artifacts <- requiredArray "artifacts" model >>= traverse parseArtifact
  whenE (null artifacts) "model.artifacts must not be empty"
  let artifactPaths = map canonicalArtifactPath artifacts
  whenE (length artifactPaths /= length (nub artifactPaths)) "model.artifacts contains duplicate paths"
  licenses <- requiredArray "licenses" root >>= traverse parseLicense
  let expectedComponents = ["text-embeddings-inference", "Alibaba-NLP/gte-Qwen2-1.5B-instruct"]
      actualComponents = map licenseComponent licenses
      missingComponents = filter (`notElem` actualComponents) expectedComponents
      unknownComponents = filter (`notElem` expectedComponents) actualComponents
      duplicateComponents = nub [component | component <- actualComponents, length (filter (== component) actualComponents) > 1]
      componentErrors =
        ["licenses is missing required component record(s): " <> intercalate ", " missingComponents | not (null missingComponents)]
        <> ["licenses has unknown component record(s): " <> intercalate ", " unknownComponents | not (null unknownComponents)]
        <> ["licenses has duplicate component record(s): " <> intercalate ", " duplicateComponents | not (null duplicateComponents)]
  whenE (not (null componentErrors)) (intercalate "; " componentErrors)
  whenE (modelRoot == runtimeRoot) "model.artifact_root and tei.runtime_bundle.artifact_root must differ"
  pure (Lock modelRoot artifacts runtimeRoot runtimeArtifacts licenses)

parseArtifact :: Value -> Either [String] Artifact
parseArtifact value = do
  object <- objectAt "model artifact" value
  path <- T.unpack <$> requiredText "path" object
  hash <- T.unpack <$> requiredText "sha256" object
  whenE (not (isSafeRelative path)) ("artifact path is unsafe: " <> path)
  whenE (not (isSha256 (T.pack hash))) ("artifact checksum is not SHA-256: " <> path)
  pure (Artifact path hash)

parseRuntimeArtifact :: Value -> Either [String] Artifact
parseRuntimeArtifact value = do
  artifact <- parseArtifact value
  whenE (not ("usr/local/" `isPrefixOf` artifactPath artifact)) ("runtime artifact path must be rooted at usr/local: " <> artifactPath artifact)
  pure artifact

expectedRuntimeArtifacts :: [Artifact]
expectedRuntimeArtifacts =
  [ Artifact "usr/local/bin/text-embeddings-router" "26299644125584e504f281faa732ba4e30012a955d08707215c592247b23df07"
  , Artifact "usr/local/lib/libfakeintel.so" "aebd0088156960223e1b1a8eab5e2f6e0cd681410ae711a4b21d5c9c13b98af5"
  , Artifact "usr/local/lib/libmkl_avx2.so.2" "92a9d14aabae9258feb1f5eb196d1b3f23c80c84bd9c8a2ff60c59ad4f48031e"
  , Artifact "usr/local/lib/libmkl_avx512.so.2" "27aa2387bf18efffcba4b330ad2df23512dadbb60f47d58c04caa9bcfcf2f69c"
  , Artifact "usr/local/lib/libmkl_core.so.2" "1f6e3f9a162efc9eaffa63dff361495ce17c1d371646d7318a83f8931f82a969"
  , Artifact "usr/local/lib/libmkl_def.so.2" "c21e78a449012f1e3c90e3a68752135bb656dc774af178004b4eb05fba45311c"
  , Artifact "usr/local/lib/libmkl_intel_lp64.so.2" "9c88e7e3f26c7c5739aa8ea1caefd2a0999208072458abc6b1c112698dd6c355"
  , Artifact "usr/local/lib/libmkl_intel_thread.so.2" "652fc69f34eb8c752dcfff8e4395f4dd35c94dd63926e8013d3b52d38aefeee0"
  , Artifact "usr/local/lib/libmkl_vml_avx2.so.2" "78457313d3af7be623a3f495d94c1e952d70eefff2623645186584e1de439ec7"
  , Artifact "usr/local/lib/libmkl_vml_avx512.so.2" "d7c5ed56e8dce5e6b7a32a9cbbae2293c1ae17317fafc8dbdb50b70915cc5b1a"
  , Artifact "usr/local/lib/libmkl_vml_def.so.2" "388187aa59614107392a58bb307d19f413c1bfa645bd1dad49992f448598aa77"
  ]

parseLicense :: Value -> Either [String] LicenseRecord
parseLicense value = do
  object <- objectAt "license record" value
  component <- T.unpack <$> requiredText "component" object
  spdx <- requiredText "spdx" object
  whenE (spdx /= "Apache-2.0") "managed embedding license must be Apache-2.0"
  path <- T.unpack <$> requiredText "notice_path" object
  hash <- T.unpack <$> requiredText "notice_sha256" object
  source <- requiredText "source_url" object
  whenE (not ("https://" `T.isPrefixOf` source)) "license source_url must be an HTTPS upstream URL"
  whenE (not (isSafeRelative path)) ("license notice path is unsafe: " <> path)
  whenE (not (isSha256 (T.pack hash))) ("license notice checksum is not SHA-256: " <> path)
  pure (LicenseRecord component path hash)

validateLicenses :: FilePath -> Lock -> IO [String]
validateLicenses root lock = fmap concat . forM (lockLicenses lock) $ \record -> do
  let path = root </> licensePath record
  exists <- doesFileExist path
  if not exists
    then pure ["missing license notice: " <> licensePath record]
    else do
      actual <- sha256File path
      pure ["license checksum drift: " <> licensePath record | actual /= licenseHash record]

validateArtifacts :: Lock -> FilePath -> IO [String]
validateArtifacts lock bundleRoot = do
  modelErrors <- validateArtifactTree "prefetched model artifact" (lockModelRoot lock) (lockArtifacts lock) bundleRoot
  runtimeErrors <- validateArtifactTree "TEI runtime artifact" (lockRuntimeRoot lock) (lockRuntimeArtifacts lock) bundleRoot
  pure (modelErrors <> runtimeErrors)

validateArtifactTree :: String -> FilePath -> [Artifact] -> FilePath -> IO [String]
validateArtifactTree label relativeRoot expected bundleRoot = do
  let root = bundleRoot </> relativeRoot
  rootErrors <- validateDirectoryPath bundleRoot relativeRoot
  containmentErrors <- canonicalContainmentErrors bundleRoot root
  (actualPaths, safetyErrors) <- if null (rootErrors <> containmentErrors) then filesUnder root else pure ([], [])
  let expectedPaths = sort (map (normalise . artifactPath) expected)
      unexpected = filter (`notElem` expectedPaths) actualPaths
      absent = filter (`notElem` actualPaths) expectedPaths
  expectedErrors <- fmap concat . forM expected $ \artifact -> do
    let path = root </> artifactPath artifact
    pathErrors <- validateExpectedFilePath root (artifactPath artifact)
    if not (null pathErrors) || normalise (artifactPath artifact) `notElem` actualPaths
      then pure []
      else do
        regular <- isRegularFile path
        if not regular
          then pure []
          else do
            actual <- sha256File path
            pure [label <> " checksum drift: " <> artifactPath artifact | actual /= artifactHash artifact]
  expectedPathErrors <- fmap concat $ forM expected (validateExpectedFilePath root . artifactPath)
  pure $ rootErrors <> containmentErrors <> safetyErrors <> expectedPathErrors <> expectedErrors
    <> ["absent " <> label <> ": " <> path | path <- absent]
    <> ["unlisted " <> label <> ": " <> path | path <- unexpected]

validateDirectoryPath :: FilePath -> FilePath -> IO [String]
validateDirectoryPath bundleRoot relativeRoot = validateComponents bundleRoot (splitDirectories relativeRoot) True

validateExpectedFilePath :: FilePath -> FilePath -> IO [String]
validateExpectedFilePath root relative = validateComponents root (splitDirectories relative) False

validateComponents :: FilePath -> [FilePath] -> Bool -> IO [String]
validateComponents base components finalDirectory = do
  baseLink <- pathIsSymbolicLink base
  if baseLink
    then pure ["unsafe symlink bundle ancestor: " <> base]
    else go base components
  where
    go _ [] = pure []
    go current (component:rest) = do
      let next = current </> component
          isFinal = null rest
      exists <- doesPathExist next
      linked <- if exists then pathIsSymbolicLink next else pure False
      if not exists
        then pure []
        else if linked
          then pure ["unsafe symlink artifact ancestor: " <> next]
          else if isFinal && finalDirectory
            then do
              directory <- doesDirectoryExist next
              pure ["unsafe non-directory artifact root: " <> next | not directory]
            else if isFinal
              then do
                regular <- isRegularFile next
                pure ["unsafe non-regular expected artifact: " <> next | not regular]
              else do
                directory <- doesDirectoryExist next
                if directory
                  then go next rest
                  else pure ["unsafe non-directory artifact ancestor: " <> next]

canonicalContainmentErrors :: FilePath -> FilePath -> IO [String]
canonicalContainmentErrors bundleRoot child = do
  bundleExists <- doesPathExist bundleRoot
  childExists <- doesPathExist child
  if not bundleExists || not childExists
    then pure []
    else do
      canonicalBundle <- canonicalizePath bundleRoot
      canonicalChild <- canonicalizePath child
      let relative = makeRelative canonicalBundle canonicalChild
      pure ["artifact root escapes bundle root: " <> child | not (isContainedRelative relative)]

isContainedRelative :: FilePath -> Bool
isContainedRelative relative =
  not (isAbsolute relative) && null (takeDrive relative)
    && all (/= "..") (splitDirectories relative)

isRegularFile :: FilePath -> IO Bool
isRegularFile path
  | os == "mingw32" = do
      linked <- pathIsSymbolicLink path
      directory <- doesDirectoryExist path
      file <- doesFileExist path
      pure (file && not linked && not directory)
  | otherwise = do
      (status, _, _) <- readProcessWithExitCode "test" ["-f", path] ""
      pure (status == ExitSuccess)

filesUnder :: FilePath -> IO ([FilePath], [String])
filesUnder root = do
  rootExists <- doesPathExist root
  rootLink <- if rootExists then pathIsSymbolicLink root else pure False
  rootDirectory <- if rootExists && not rootLink then doesDirectoryExist root else pure False
  if not rootExists
    then pure ([], ["missing artifact root: " <> root])
    else if rootLink
      then pure ([], ["unsafe symlink artifact root: " <> root])
      else if not rootDirectory
        then pure ([], ["unsafe non-directory artifact root: " <> root])
        else go ""
  where
    go relative = do
      entries <- listDirectory (root </> relative)
      pairs <- forM entries $ \entry -> do
        let next = if null relative then entry else relative </> entry
            full = root </> next
        linked <- pathIsSymbolicLink full
        directory <- if linked then pure False else doesDirectoryExist full
        regular <- if linked || directory then pure False else isRegularFile full
        if linked
          then pure ([], ["unsafe symlink artifact: " <> normalise next])
          else if directory
            then go next
            else if regular
              then pure ([normalise next], [])
              else pure ([], ["unsafe non-regular artifact: " <> normalise next])
      pure (concatMap fst pairs, concatMap snd pairs)

sha256File :: FilePath -> IO String
sha256File path = withBinaryFile path ReadMode (go (hashInit :: Context SHA256))
  where
    go !context handle = do
      chunk <- BS.hGetSome handle (1024 * 1024)
      if BS.null chunk
        then pure (show (hashFinalize context))
        else do
          let next = hashUpdate context chunk
          -- Force the updated context before recursing so its thunk cannot
          -- retain this chunk (and every previous chunk) at -O0.
          next `seq` go next handle

objectAt :: String -> Value -> Either [String] Object
objectAt label (Object object) = Right object
objectAt label _ = Left [label <> " must be an object"]

requiredObject :: Text -> Object -> Either [String] Object
requiredObject key object = requiredValue key object >>= objectAt (T.unpack key)

requiredArray :: Text -> Object -> Either [String] [Value]
requiredArray key object = do
  value <- requiredValue key object
  case value of
    Array values -> Right (foldr (:) [] values)
    _ -> Left [T.unpack key <> " must be an array"]

requiredText :: Text -> Object -> Either [String] Text
requiredText key object = do
  value <- requiredValue key object
  case value of
    String text -> Right text
    _ -> Left [T.unpack key <> " must be a string"]

requiredBool :: Text -> Object -> Either [String] Bool
requiredBool key object = do
  value <- requiredValue key object
  case value of
    Bool boolean -> Right boolean
    _ -> Left [T.unpack key <> " must be a boolean"]

requiredDigest :: Text -> Object -> Either [String] Text
requiredDigest key object = do
  digest <- requiredText key object
  whenE (not ("sha256:" `T.isPrefixOf` digest && isSha256 (T.drop 7 digest))) (T.unpack key <> " must be a sha256 digest")
  pure digest

integerAt :: Text -> Object -> Either [String] Int
integerAt key object = do
  value <- requiredValue key object
  case value of
    Number scientific -> case floatingOrInteger scientific :: Either Double Int of
      Right integer -> Right integer
      Left _ -> Left [T.unpack key <> " must be an integer"]
    _ -> Left [T.unpack key <> " must be an integer"]

requiredValue :: Text -> Object -> Either [String] Value
requiredValue key object =
  maybe (Left [T.unpack key <> " is required"]) Right (KM.lookup (Key.fromText key) object)

requiredEq :: Text -> Value -> Object -> Either [String] ()
requiredEq key expected object = do
  actual <- requiredValue key object
  whenE (actual /= expected) (T.unpack key <> " has an unexpected value")

whenE :: Bool -> String -> Either [String] ()
whenE condition message = if condition then Left [message] else Right ()

isSha1 :: Text -> Bool
isSha1 value = T.length value == 40 && T.all isHexDigit value

isSha256 :: Text -> Bool
isSha256 value = T.length value == 64 && T.all isHexDigit value

isDigestReference :: Text -> Bool
isDigestReference ref =
  case T.splitOn "@sha256:" ref of
    [name, digest] ->
      not (T.null name) && isSha256 digest
        && let leaf = last (T.splitOn "/" name) in not (":" `T.isInfixOf` leaf)
    _ -> False

registryEvidenceMatches :: Text -> Text -> Bool
registryEvidenceMatches reference evidence =
  case T.splitOn "@" reference of
    [imageName, digest]
      | isDigestReference reference -> case T.splitOn "/" imageName of
          registry:repositoryParts@(_:_) -> evidence == "https://" <> registry <> "/v2/" <> T.intercalate "/" repositoryParts <> "/manifests/" <> digest
          _ -> False
    _ -> False

isSafeRelative :: FilePath -> Bool
isSafeRelative path =
  not (null path) && not (isAbsolute path) && null (takeDrive path)
    && not ('\\' `elem` path)
    && all (`notElem` ["", ".", ".."] ) (slashSegments path)

canonicalArtifactPath :: Artifact -> FilePath
canonicalArtifactPath = normalise . artifactPath

slashSegments :: FilePath -> [FilePath]
slashSegments = go [] []
  where
    go current segments [] = reverse (reverse current : segments)
    go current segments ('/':rest) = go [] (reverse current : segments) rest
    go current segments (character:rest) = go (character:current) segments rest

validReleaseChar :: Char -> Bool
validReleaseChar c = c == 'v' || c == '.' || c == '-' || c == '_' || c >= '0' && c <= '9'

isInfix :: Eq a => [a] -> [a] -> Bool
isInfix needle = any (needle `isPrefixOf`) . tails

isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys

tails :: [a] -> [[a]]
tails [] = [[]]
tails values@(_:rest) = values : tails rest

selfTest :: IO ()
selfTest = withSystemTempDirectory "hmem-managed-embedding-provenance" $ \root -> do
  let
      snapshot = root </> "snapshot"
      modelSnapshot = snapshot </> "model"
      runtimeSnapshot = snapshot </> "tei-runtime"
      license = root </> "licenses" </> "managed-embedding" </> "notice.txt"
      artifact = modelSnapshot </> "config.json"
      largeArtifact = modelSnapshot </> "large.sparse"
      outside = root </> "outside"
      manifest = root </> "config" </> "managed-embedding-provenance.yaml"
  createDirectoryIfMissing True (takeDirectory artifact)
  createDirectoryIfMissing True (takeDirectory license)
  createDirectoryIfMissing True (takeDirectory manifest)
  BS.writeFile artifact "fixture artifact"
  withBinaryFile largeArtifact WriteMode $ \handle -> do
    BS.hPut handle "x"
    hSetFileSize handle (256 * 1024 * 1024)
  runtimeArtifacts <- forM (zip [1 :: Int ..] expectedRuntimeArtifacts) $ \(index, lockedArtifact) -> do
    let path = runtimeSnapshot </> artifactPath lockedArtifact
        bytes = BSC.pack ("fixture TEI runtime artifact " <> show index)
    createDirectoryIfMissing True (takeDirectory path)
    BS.writeFile path bytes
    hash <- sha256File path
    pure (Artifact (artifactPath lockedArtifact) hash)
  runtimeArtifact <- case runtimeArtifacts of
    firstRuntimeArtifact:_ -> pure (runtimeSnapshot </> artifactPath firstRuntimeArtifact)
    [] -> die ["self-test runtime fixture is empty"]
  let runtimeArtifactBytes = "fixture TEI runtime artifact 1"
  BS.writeFile license "fixture Apache notice"
  artifactHash <- sha256File artifact
  largeArtifactHash <- sha256File largeArtifact
  licenseHash <- sha256File license
  let fixtureLock = Lock "model" [Artifact "config.json" artifactHash, Artifact "large.sparse" largeArtifactHash] "tei-runtime" runtimeArtifacts [LicenseRecord "fixture" "licenses/managed-embedding/notice.txt" licenseHash]
  baseline <- (<>) <$> validateLicenses root fixtureLock <*> validateArtifacts fixtureLock snapshot
  requireFixture "complete offline bundle" (null baseline)
  writeFile manifest (fullFixture (lockArtifacts fixtureLock) runtimeArtifacts licenseHash)
  parsedFixture <- parseFixtureLock root runtimeArtifacts
  parsedBaseline <- (<>) <$> validateLicenses root parsedFixture <*> validateArtifacts parsedFixture snapshot
  requireFixture "parsed complete offline bundle" (null parsedBaseline)
  BS.writeFile artifact "drift"
  driftErrors <- validateArtifacts fixtureLock snapshot
  requireFixture "model checksum drift" (contains "checksum drift" driftErrors)
  BS.writeFile artifact "fixture artifact"
  removePathForcibly runtimeArtifact
  missingErrors <- validateArtifacts fixtureLock snapshot
  requireFixture "missing runtime artifact" (contains "absent TEI runtime artifact" missingErrors)
  BS.writeFile runtimeArtifact runtimeArtifactBytes
  BS.writeFile (runtimeSnapshot </> "unexpected") "unexpected"
  extraErrors <- validateArtifacts fixtureLock snapshot
  requireFixture "extra runtime artifact" (contains "unlisted TEI runtime artifact" extraErrors)
  removePathForcibly (runtimeSnapshot </> "unexpected")
  let nonregular = modelSnapshot </> "not-a-file"
      nonregularLock = fixtureLock { lockArtifacts = lockArtifacts fixtureLock <> [Artifact "not-a-file" artifactHash] }
  createDirectoryIfMissing True nonregular
  nonregularErrors <- validateArtifacts nonregularLock snapshot
  requireFixture "non-regular expected artifact" (contains "unsafe non-regular expected artifact" nonregularErrors)
  removePathForcibly nonregular
  when (os /= "mingw32") $ do
    let fifo = modelSnapshot </> "fixture.fifo"
        fifoLock = fixtureLock { lockArtifacts = lockArtifacts fixtureLock <> [Artifact "fixture.fifo" artifactHash] }
    (fifoStatus, _, _) <- readProcessWithExitCode "mkfifo" [fifo] ""
    requireFixture "FIFO setup" (fifoStatus == ExitSuccess)
    fifoErrors <- validateArtifacts fifoLock snapshot
    requireFixture "FIFO expected artifact" (contains "unsafe non-regular expected artifact" fifoErrors)
    removePathForcibly fifo
  createDirectoryIfMissing True outside
  let outsideArtifact = outside </> "config.json"
      ancestorLink = modelSnapshot </> "escape"
      directLink = modelSnapshot </> "linked"
  BS.writeFile outsideArtifact "outside fixture"
  outsideHash <- sha256File outsideArtifact
  ancestorLinkResult <- createFixtureDirectoryLink outside ancestorLink
  case ancestorLinkResult of
    Right () -> do
      ancestorErrors <- validateArtifacts (fixtureLock { lockArtifacts = lockArtifacts fixtureLock <> [Artifact "escape/config.json" outsideHash] }) snapshot
      requireFixture "ancestor symlink escape" (contains "unsafe symlink artifact ancestor" ancestorErrors)
      removePathForcibly ancestorLink
    Left err | os == "mingw32" -> putStrLn ("Windows ancestor-link negative skipped: " <> displayException err)
    Left _ -> requireFixture "ancestor symlink setup" False
  directLinkResult <- createFixtureDirectoryLink outside directLink
  case directLinkResult of
    Right () -> do
      directLinkErrors <- validateArtifacts (fixtureLock { lockArtifacts = lockArtifacts fixtureLock <> [Artifact "linked/config.json" outsideHash] }) snapshot
      requireFixture "expected-file ancestor symlink" (contains "unsafe symlink artifact ancestor" directLinkErrors)
      removePathForcibly directLink
    Left err | os == "mingw32" -> putStrLn ("Windows expected-path-link negative skipped: " <> displayException err)
    Left _ -> requireFixture "expected-file ancestor symlink setup" False
  let linkedBundle = root </> "linked-bundle"
  bundleLinkResult <- createFixtureDirectoryLink snapshot linkedBundle
  case bundleLinkResult of
    Right () -> do
      bundleLinkErrors <- validateArtifacts fixtureLock linkedBundle
      requireFixture "artifact-root symlink" (contains "unsafe symlink bundle ancestor" bundleLinkErrors)
      removePathForcibly linkedBundle
    Left err | os == "mingw32" -> putStrLn ("Windows artifact-root-link negative skipped: " <> displayException err)
    Left _ -> requireFixture "artifact-root symlink setup" False
  writeFile manifest staticFixture
  parsed <- parseAt root
  requireFixture "pinned runtime manifest" (null parsed)
  modifyFile manifest (replace "@sha256:ad950d30878eceb72aaf32024d26fa2b1d04a75304fa0b4776b49aa1941fea07" ":cpu-1.9.3")
  tagErrors <- parseAt root
  requireFixture "image tag" (contains "registry digest" tagErrors)
  writeFile manifest staticFixture
  modifyFile manifest (replace "source_layer_digest: sha256:51345600aa8d1cd784c97a2bc6ba969a535841224986d40ac91770c504e472a9" "source_layer_digest: sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
  layerErrors <- parseAt root
  requireFixture "runtime source-layer drift" (contains "derivation must match" layerErrors)
  writeFile manifest staticFixture
  modifyFile manifest (replace "ld_preload: /usr/local/lib/libfakeintel.so" "ld_preload: /tmp/untrusted.so")
  loaderErrors <- parseAt root
  requireFixture "uncontrolled loader" (contains "controlled TEI loader" loaderErrors)
  writeFile manifest staticFixture
  modifyFile manifest (replace "path: usr/local/lib/libfakeintel.so" "path: ../libfakeintel.so")
  pathErrors <- parseAt root
  requireFixture "runtime path escape" (contains "unsafe" pathErrors)
  writeFile manifest staticFixture
  modifyFile manifest (replace "path: usr/local/bin/text-embeddings-router" "path: usr/local//bin/text-embeddings-router")
  noncanonicalErrors <- parseAt root
  requireFixture "noncanonical runtime path" (contains "unsafe" noncanonicalErrors)
  writeFile manifest staticFixture
  modifyFile manifest (replace "expected_embedding_dimensions: 1536" "expected_embedding_dimensions: 768")
  dimensionErrors <- parseAt root
  requireFixture "dimension drift" (contains "must be 1536" dimensionErrors)
  writeFile manifest staticFixture
  modifyFile manifest (replace "revision: 1cad2ab3ff41c2671f34e135d29831368ee26b68" "revision: main")
  revisionErrors <- parseAt root
  requireFixture "floating model revision" (contains "immutable revision" revisionErrors)
  writeFile manifest staticFixture
  modifyFile manifest (replace "artifacts: [ { path: config.json, sha256: aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa } ]" "artifacts: [ { path: config.json, sha256: aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa }, { path: config.json, sha256: aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa } ]")
  duplicateErrors <- parseAt root
  requireFixture "duplicate model artifact" (contains "duplicate paths" duplicateErrors)
  writeFile manifest staticFixture
  modifyFile manifest (replace "licenses:" "license_records:")
  missingLicenseErrors <- parseAt root
  requireFixture "missing license records" (contains "licenses is required" missingLicenseErrors)
  writeFile manifest staticFixture
  modifyFile manifest (replace "component: Alibaba-NLP/gte-Qwen2-1.5B-instruct" "component: text-embeddings-inference")
  duplicateLicenseErrors <- parseAt root
  requireFixture "duplicate license record" (contains "duplicate component" duplicateLicenseErrors)
  writeFile manifest staticFixture
  modifyFile manifest (replace "component: text-embeddings-inference" "component: unknown")
  unknownLicenseErrors <- parseAt root
  requireFixture "unknown license component" (contains "unknown component" unknownLicenseErrors)
  removePathForcibly license
  missingLicenseFileErrors <- validateLicenses root fixtureLock
  requireFixture "missing license file" (contains "missing license notice" missingLicenseFileErrors)
  BS.writeFile license "fixture Apache notice"
  putStrLn "Managed embedding provenance self-test passed (full lock parsing; bounded large-file hashing; both bundle trees; missing, extra, drift, non-regular, symlink, ancestor escape, canonical path, pin, dimension, revision, duplicate, license, and loader negatives)."

parseAt :: FilePath -> IO [String]
parseAt root = do
  decoded <- Yaml.decodeFileEither (root </> "config/managed-embedding-provenance.yaml")
  pure $ case decoded of
    Left err -> [Yaml.prettyPrintParseException err]
    Right value -> either id (const []) (parseLock value)

parseFixtureLock :: FilePath -> [Artifact] -> IO Lock
parseFixtureLock root runtimeArtifacts = do
  decoded <- Yaml.decodeFileEither (root </> "config/managed-embedding-provenance.yaml")
  case decoded of
    Left err -> die [Yaml.prettyPrintParseException err]
    Right value -> either die pure (parseLockAgainst runtimeArtifacts value)

createFixtureDirectoryLink :: FilePath -> FilePath -> IO (Either SomeException ())
createFixtureDirectoryLink target link = do
  direct <- try (createDirectoryLink target link) :: IO (Either SomeException ())
  case direct of
    Right () -> pure direct
    Left _ | os == "mingw32" -> try $ do
      let quote path = "'" <> concatMap escapeQuote path <> "'"
          escapeQuote '\'' = "''"
          escapeQuote character = [character]
          command = "New-Item -ItemType Junction -Path " <> quote link <> " -Target " <> quote target <> " -ErrorAction Stop | Out-Null"
      (status, output, errorOutput) <- readProcessWithExitCode "powershell.exe" ["-NoProfile", "-NonInteractive", "-Command", command] ""
      if status == ExitSuccess
        then pure ()
        else ioError (userError ("New-Item Junction failed: " <> output <> errorOutput))
    Left _ -> pure direct

staticFixture :: String
staticFixture = unlines $
  [ "schema_version: 1"
  , "profile: managed-tei-gte-qwen2-1.5b-instruct"
  , "expected_embedding_dimensions: 1536"
  , "tei:"
  , "  release: v1.9.3"
  , "  source_commit: 06670157fb6c1523482219bdb2d1660277d38088"
  , "  source_archive: { url: https://example.test/06670157fb6c1523482219bdb2d1660277d38088, sha256: aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa }"
  , "  cpu_image:"
  , "    reference: ghcr.io/huggingface/text-embeddings-inference@sha256:ad950d30878eceb72aaf32024d26fa2b1d04a75304fa0b4776b49aa1941fea07"
  , "    platform: linux/amd64"
  , "    registry_manifest_url: https://ghcr.io/v2/huggingface/text-embeddings-inference/manifests/sha256:ad950d30878eceb72aaf32024d26fa2b1d04a75304fa0b4776b49aa1941fea07"
  , "    index_digest: sha256:ad950d30878eceb72aaf32024d26fa2b1d04a75304fa0b4776b49aa1941fea07"
  , "    platform_manifest_digest: sha256:c26a226262ad4ff3330fb30b76653c1bb65da2fcf413b92284545a010e0a8a48"
  , "    config_digest: sha256:f50bc806338120e138d774236098580f7e5840dbe8bc001ef64615847c1685e8"
  , "    executable_layer_digest: sha256:51345600aa8d1cd784c97a2bc6ba969a535841224986d40ac91770c504e472a9"
  , "  runtime_bundle:"
  , "    artifact_root: tei-runtime"
  , "    source_platform_manifest_digest: sha256:c26a226262ad4ff3330fb30b76653c1bb65da2fcf413b92284545a010e0a8a48"
  , "    source_config_digest: sha256:f50bc806338120e138d774236098580f7e5840dbe8bc001ef64615847c1685e8"
  , "    source_layer_digest: sha256:51345600aa8d1cd784c97a2bc6ba969a535841224986d40ac91770c504e472a9"
  , "    system_abi_source: exact-pinned-platform-image"
  , "    retrieval_phase: build-time-only"
  , "    runtime_use: verified-local-bundle"
  , "    loader: { inherit_environment: false, ld_library_path: /usr/local/lib, ld_preload: /usr/local/lib/libfakeintel.so }"
  , "    artifacts:"
  ] <> ["      - { path: " <> artifactPath artifact <> ", sha256: " <> artifactHash artifact <> " }" | artifact <- expectedRuntimeArtifacts] <>
  [ "model:"
  , "  id: Alibaba-NLP/gte-Qwen2-1.5B-instruct"
  , "  revision: 1cad2ab3ff41c2671f34e135d29831368ee26b68"
  , "  revision_url: https://example.test/1cad2ab3ff41c2671f34e135d29831368ee26b68"
  , "  snapshot_url: https://example.test/1cad2ab3ff41c2671f34e135d29831368ee26b68"
  , "  artifact_root: model"
  , "  retrieval_phase: build-time-only"
  , "  runtime_use: local-read-only-snapshot"
  , "  artifacts: [ { path: config.json, sha256: aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa } ]"
  , "licenses:"
  , "  - { component: text-embeddings-inference, spdx: Apache-2.0, notice_path: licenses/managed-embedding/notice.txt, notice_sha256: aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa, source_url: https://example.test/tei-license }"
  , "  - { component: Alibaba-NLP/gte-Qwen2-1.5B-instruct, spdx: Apache-2.0, notice_path: licenses/managed-embedding/notice.txt, notice_sha256: aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa, source_url: https://example.test/model-license }"
  ]

fullFixture :: [Artifact] -> [Artifact] -> String -> String
fullFixture modelArtifacts runtimeArtifacts licenseHash =
  let withFixtureHash = replaceAll "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" licenseHash staticFixture
      withRuntime = replaceSection "    artifacts:\n" "model:\n" (renderArtifacts "      " runtimeArtifacts) withFixtureHash
      modelLine = "  artifacts: [ " <> intercalate ", " ["{ path: " <> artifactPath artifact <> ", sha256: " <> artifactHash artifact <> " }" | artifact <- modelArtifacts] <> " ]"
  in replace ("  artifacts: [ { path: config.json, sha256: " <> licenseHash <> " } ]") modelLine withRuntime

renderArtifacts :: String -> [Artifact] -> String
renderArtifacts indent artifacts = concat [indent <> "- { path: " <> artifactPath artifact <> ", sha256: " <> artifactHash artifact <> " }\n" | artifact <- artifacts]

replaceSection :: String -> String -> String -> String -> String
replaceSection start marker replacement value = case breakOn start value of
  Nothing -> value
  Just (before, startAndRest) ->
    let rest = drop (length start) startAndRest
    in case breakOn marker rest of
      Nothing -> value
      Just (_, markerAndAfter) -> before <> start <> replacement <> markerAndAfter

replaceAll :: String -> String -> String -> String
replaceAll needle replacement value = case breakOn needle value of
  Nothing -> value
  Just (before, after) -> before <> replacement <> replaceAll needle replacement (drop (length needle) after)

modifyFile :: FilePath -> (String -> String) -> IO ()
modifyFile path change = BS.readFile path >>= writeFile path . change . BSC.unpack

replace :: String -> String -> String -> String
replace needle replacement value = case breakOn needle value of
  Nothing -> value
  Just (before, after) -> before <> replacement <> drop (length needle) after

breakOn :: String -> String -> Maybe (String, String)
breakOn needle = go []
  where
    go _ [] = Nothing
    go before rest@(value:remaining)
      | needle `isPrefixOf` rest = Just (reverse before, rest)
      | otherwise = go (value : before) remaining

contains :: String -> [String] -> Bool
contains needle = any (isInfix needle)

requireFixture :: String -> Bool -> IO ()
requireFixture label ok = unless ok (die ["self-test did not reject " <> label])

die :: [String] -> IO a
die errors = do
  putStrLn "Managed embedding provenance check failed:"
  forM_ errors (putStrLn . ("  - " <>))
  exitFailure
