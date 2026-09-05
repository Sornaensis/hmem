#!/usr/bin/env stack
-- stack script --resolver lts-24.2 --package aeson --package yaml --package crypton --package directory --package filepath --package bytestring --package text --package containers --package scientific --package temporary

-- | Offline verifier for the managed TEI provenance lockfile.  It never
-- contacts a registry or model hub: '--artifact-root' must be a pre-fetched
-- snapshot whose exact inventory is checked against the lockfile.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
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
import System.Directory (createDirectoryIfMissing, doesFileExist, listDirectory, removePathForcibly)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath ((</>), isAbsolute, normalise, splitDirectories, takeDirectory)
import System.IO (IOMode (ReadMode, WriteMode), hSetFileSize, withBinaryFile)
import System.IO.Temp (withSystemTempDirectory)

data Artifact = Artifact { artifactPath :: FilePath, artifactHash :: String } deriving (Eq, Show)
data LicenseRecord = LicenseRecord { licenseComponent :: String, licensePath :: FilePath, licenseHash :: String } deriving (Eq, Show)
data Lock = Lock { lockArtifacts :: [Artifact], lockLicenses :: [LicenseRecord] } deriving (Eq, Show)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> validate "." Nothing
    ["--root", root] -> validate root Nothing
    ["--root", root, "--artifact-root", artifactRoot] -> validate root (Just artifactRoot)
    ["--self-test"] -> selfTest
    _ -> die ["usage: check-managed-embedding-provenance.hs [--root ROOT [--artifact-root SNAPSHOT] | --self-test]"]

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
parseLock value = do
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
  whenE (retrieval /= "build-time-only" || runtimeUse /= "local-read-only-snapshot") "manifest must separate build-time retrieval from read-only runtime use"
  artifacts <- requiredArray "artifacts" model >>= traverse parseArtifact
  whenE (null artifacts) "model.artifacts must not be empty"
  let artifactPaths = map artifactPath artifacts
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
  pure (Lock artifacts licenses)

parseArtifact :: Value -> Either [String] Artifact
parseArtifact value = do
  object <- objectAt "model artifact" value
  path <- T.unpack <$> requiredText "path" object
  hash <- T.unpack <$> requiredText "sha256" object
  whenE (not (isSafeRelative path)) ("artifact path is unsafe: " <> path)
  whenE (not (isSha256 (T.pack hash))) ("artifact checksum is not SHA-256: " <> path)
  pure (Artifact path hash)

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
validateArtifacts lock artifactRoot = do
  missingOrDrift <- fmap concat . forM (lockArtifacts lock) $ \artifact -> do
    let path = artifactRoot </> artifactPath artifact
    exists <- doesFileExist path
    if not exists
      then pure ["missing prefetched artifact: " <> artifactPath artifact]
      else do
        actual <- sha256File path
        pure ["artifact checksum drift: " <> artifactPath artifact | actual /= artifactHash artifact]
  actualPaths <- sort <$> filesUnder artifactRoot
  let expectedPaths = sort (map artifactPath (lockArtifacts lock))
      unexpected = filter (`notElem` expectedPaths) actualPaths
      absent = filter (`notElem` actualPaths) expectedPaths
  pure $ missingOrDrift
    <> ["absent prefetched artifact: " <> path | path <- absent]
    <> ["unlisted prefetched artifact: " <> path | path <- unexpected]

filesUnder :: FilePath -> IO [FilePath]
filesUnder root = go ""
  where
    go relative = do
      entries <- listDirectory (root </> relative)
      fmap concat . forM entries $ \entry -> do
        let next = if null relative then entry else relative </> entry
            full = root </> next
        isFile <- doesFileExist full
        if isFile then pure [normalise next] else go next

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
  not (null path) && not (isAbsolute path)
    && all (/= "..") (splitDirectories (normalise path))

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
      license = root </> "licenses" </> "managed-embedding" </> "notice.txt"
      artifact = snapshot </> "config.json"
      largeArtifact = snapshot </> "large.sparse"
      manifest = root </> "config" </> "managed-embedding-provenance.yaml"
  createDirectoryIfMissing True (takeDirectory artifact)
  createDirectoryIfMissing True (takeDirectory license)
  createDirectoryIfMissing True (takeDirectory manifest)
  BS.writeFile artifact "fixture artifact"
  -- A 256 MiB logical artifact exercises the bounded chunking path.  A
  -- whole-file strict read would require the complete artifact in memory.
  withBinaryFile largeArtifact WriteMode $ \handle -> do
    BS.hPut handle "x"
    hSetFileSize handle (256 * 1024 * 1024)
  BS.writeFile license "fixture Apache notice"
  artifactHash <- sha256File artifact
  largeArtifactHash <- sha256File largeArtifact
  licenseHash <- sha256File license
  let artifacts = [("config.json", artifactHash), ("large.sparse", largeArtifactHash)]
  writeFile manifest (fixture artifacts licenseHash)
  baseline <- errorsAt root (Just snapshot)
  requireFixture "baseline" (null baseline)
  modifyFile manifest (replace "@sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" ":cpu-1.9.3")
  tagErrors <- errorsAt root (Just snapshot)
  requireFixture "image tag" (contains "registry digest" tagErrors)
  modifyFile manifest (const (fixture artifacts licenseHash))
  modifyFile manifest (replace "/manifests/sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" "/manifests/cpu-1.9.3")
  evidenceErrors <- errorsAt root (Just snapshot)
  requireFixture "floating registry evidence" (contains "registry_manifest_url must address" evidenceErrors)
  modifyFile manifest (const (fixture artifacts licenseHash))
  modifyFile manifest (replace "1cad2ab3ff41c2671f34e135d29831368ee26b68" "main")
  branchErrors <- errorsAt root (Just snapshot)
  requireFixture "branch revision" (contains "immutable revision" branchErrors)
  modifyFile manifest (const (fixture artifacts licenseHash))
  modifyFile manifest (replace ("    - { path: config.json, sha256: " <> artifactHash <> " }") ("    - { path: config.json, sha256: " <> artifactHash <> " }\n    - { path: config.json, sha256: " <> artifactHash <> " }"))
  duplicateErrors <- errorsAt root (Just snapshot)
  requireFixture "duplicate artifact" (contains "duplicate paths" duplicateErrors)
  modifyFile manifest (const (fixture artifacts licenseHash))
  BS.writeFile artifact "drift"
  driftErrors <- errorsAt root (Just snapshot)
  requireFixture "checksum drift" (contains "checksum drift" driftErrors)
  BS.writeFile artifact "fixture artifact"
  removePathForcibly artifact
  missingErrors <- errorsAt root (Just snapshot)
  requireFixture "missing artifact" (contains "missing prefetched artifact" missingErrors)
  BS.writeFile artifact "fixture artifact"
  modifyFile manifest (replace "expected_embedding_dimensions: 1536" "expected_embedding_dimensions: 768")
  dimensionErrors <- errorsAt root (Just snapshot)
  requireFixture "dimension drift" (contains "must be 1536" dimensionErrors)
  modifyFile manifest (const (fixture artifacts licenseHash))
  modifyFile manifest (replace "licenses:" "license_records:")
  recordErrors <- errorsAt root (Just snapshot)
  requireFixture "missing license records" (contains "licenses is required" recordErrors)
  modifyFile manifest (const (fixture artifacts licenseHash))
  modifyFile manifest (replace "component: text-embeddings-inference" "component: unknown")
  unknownLicenseErrors <- errorsAt root (Just snapshot)
  requireFixture "unknown license component" (contains "unknown component" unknownLicenseErrors)
  modifyFile manifest (const (fixture artifacts licenseHash))
  modifyFile manifest (replace "component: Alibaba-NLP/gte-Qwen2-1.5B-instruct" "component: text-embeddings-inference")
  duplicateLicenseErrors <- errorsAt root (Just snapshot)
  requireFixture "duplicate license component" (contains "duplicate component" duplicateLicenseErrors)
  modifyFile manifest (const (fixture artifacts licenseHash))
  removePathForcibly license
  licenseErrors <- errorsAt root (Just snapshot)
  requireFixture "missing license" (contains "missing license notice" licenseErrors)
  putStrLn "Managed embedding provenance self-test passed (unique temporary fixture; 256 MiB large artifact; tag, evidence, branch, duplicate, checksum, missing, dimension, license-record/component, and license-file negatives)."

errorsAt :: FilePath -> Maybe FilePath -> IO [String]
errorsAt root artifactRoot = do
  decoded <- Yaml.decodeFileEither (root </> "config/managed-embedding-provenance.yaml")
  case decoded of
    Left err -> pure [Yaml.prettyPrintParseException err]
    Right value -> case parseLock value of
      Left errors -> pure errors
      Right lock -> (<>) <$> validateLicenses root lock <*> maybe (pure []) (validateArtifacts lock) artifactRoot

fixture :: [(FilePath, String)] -> String -> String
fixture artifacts licenseHash = unlines $
  [ "schema_version: 1"
  , "profile: managed-tei-gte-qwen2-1.5b-instruct"
  , "expected_embedding_dimensions: 1536"
  , "tei:"
  , "  release: v1.9.3"
  , "  source_commit: 06670157fb6c1523482219bdb2d1660277d38088"
  , "  source_archive: { url: https://example.test/06670157fb6c1523482219bdb2d1660277d38088, sha256: aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa }"
  , "  cpu_image: { reference: registry.example/tei@sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa, platform: linux/amd64, registry_manifest_url: https://registry.example/v2/tei/manifests/sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa }"
  , "model:"
  , "  id: Alibaba-NLP/gte-Qwen2-1.5B-instruct"
  , "  revision: 1cad2ab3ff41c2671f34e135d29831368ee26b68"
  , "  revision_url: https://example.test/1cad2ab3ff41c2671f34e135d29831368ee26b68"
  , "  snapshot_url: https://example.test/1cad2ab3ff41c2671f34e135d29831368ee26b68"
  , "  retrieval_phase: build-time-only"
  , "  runtime_use: local-read-only-snapshot"
  , "  artifacts:"
  ] <> ["    - { path: " <> path <> ", sha256: " <> hash <> " }" | (path, hash) <- artifacts] <>
  [ "licenses:"
  , "  - { component: text-embeddings-inference, spdx: Apache-2.0, notice_path: licenses/managed-embedding/notice.txt, notice_sha256: " <> licenseHash <> ", source_url: https://example.test/tei-license }"
  , "  - { component: Alibaba-NLP/gte-Qwen2-1.5B-instruct, spdx: Apache-2.0, notice_path: licenses/managed-embedding/notice.txt, notice_sha256: " <> licenseHash <> ", source_url: https://example.test/model-license }"
  ]

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
