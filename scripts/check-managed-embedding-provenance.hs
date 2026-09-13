#!/usr/bin/env stack
-- stack script --resolver lts-24.2 --package aeson --package yaml --package crypton --package directory --package filepath --package bytestring --package text --package containers --package scientific --package temporary --package process --package vector

-- | Offline verifier for the native TEI GPU production lock. The immutable
-- trust anchors are compiled into this checker; editing the installed manifest
-- cannot bless replacement model, runtime, image, numerical, or license bytes.
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Exception (SomeException, try)
import Control.Monad (forM, forM_, unless)
import Crypto.Hash (Context, Digest, hashFinalize, hashInit, hashUpdate)
import Crypto.Hash.Algorithms (SHA256)
import Data.Aeson (Object, Value (..))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString qualified as BS
import Data.Char (isHexDigit, toLower)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.List (nub, sort)
import Data.Scientific (floatingOrInteger)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Data.Yaml qualified as Yaml
import System.Directory
  ( canonicalizePath, createDirectoryIfMissing, createDirectoryLink
  , doesDirectoryExist, doesFileExist, doesPathExist, getCurrentDirectory
  , listDirectory, pathIsSymbolicLink, removePathForcibly )
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitFailure)
import System.FilePath ((</>), isAbsolute, normalise, splitDirectories, takeDirectory, takeDrive)
import System.Info (os)
import System.IO (Handle, IOMode (ReadMode), hFileSize, withBinaryFile)
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcessWithExitCode)

data Artifact = Artifact
  { artifactPath :: FilePath
  , artifactBytes :: Integer
  , artifactHash :: String
  } deriving (Eq, Ord, Show)

data SourcedArtifact = SourcedArtifact
  { sourcedArtifact :: Artifact
  , artifactSourcePath :: FilePath
  } deriving (Eq, Ord, Show)

data ImageLibrary = ImageLibrary
  { libraryRole :: String
  , libraryArtifact :: Artifact
  } deriving (Eq, Ord, Show)

data LicenseRecord = LicenseRecord
  { licenseComponent :: String
  , licenseExpression :: String
  , licenseArtifact :: Artifact
  , licenseSourceIdentity :: String
  } deriving (Eq, Ord, Show)

data Lock = Lock
  { lockModelRoot :: FilePath
  , lockModelArtifacts :: [Artifact]
  , lockRuntimeRoot :: FilePath
  , lockRuntimeArtifacts :: [Artifact]
  , lockImageLibraries :: [Artifact]
  , lockLicenses :: [LicenseRecord]
  } deriving (Eq, Show)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> validate "." Nothing
    ["--root", root] -> validate root Nothing
    ["--root", root, "--artifact-root", bundle] ->
      validate root (Just (bundle </> "model", bundle </> "tei-runtime", Nothing))
    ["--root", root, "--model-root", modelRoot, "--runtime-root", runtimeRoot] ->
      validate root (Just (modelRoot, runtimeRoot, Nothing))
    ["--root", root, "--model-root", modelRoot, "--runtime-root", runtimeRoot, "--image-root", imageRoot] ->
      validate root (Just (modelRoot, runtimeRoot, Just imageRoot))
    ["--self-test"] -> selfTest
    _ -> die
      [ "usage: check-managed-embedding-provenance.hs"
      , "       [--root ROOT]"
      , "       [--root ROOT --artifact-root BUNDLE]"
      , "       [--root ROOT --model-root MODEL --runtime-root RUNTIME [--image-root IMAGE_ROOT]]"
      , "       [--self-test]"
      ]

validate :: FilePath -> Maybe (FilePath, FilePath, Maybe FilePath) -> IO ()
validate root suppliedRoots = do
  decoded <- Yaml.decodeFileEither (root </> manifestPath)
  case decoded of
    Left err -> die ["invalid YAML: " <> Yaml.prettyPrintParseException err]
    Right value -> case parseLock value of
      Left errors -> die errors
      Right lock -> do
        licenseErrors <- validateLicenses root (lockLicenses lock)
        numericalErrors <- validatePinnedFiles "numerical authority" root expectedNumericalFixtures
        bundleErrors <- case suppliedRoots of
          Nothing -> pure []
          Just (modelRoot, runtimeRoot, imageRoot) -> do
            modelErrors <- validateArtifactTree "model artifact" modelRoot (lockModelArtifacts lock)
            runtimeErrors <- validateArtifactTree "runtime artifact" runtimeRoot (lockRuntimeArtifacts lock)
            imageErrors <- maybe (pure []) (\path -> validateArtifactTree "image ELF artifact" path (lockImageLibraries lock)) imageRoot
            pure (modelErrors <> runtimeErrors <> imageErrors)
        let errors = licenseErrors <> numericalErrors <> bundleErrors
        if null errors
          then putStrLn "Managed embedding GPU provenance check passed."
          else die errors

parseLock :: Value -> Either [String] Lock
parseLock value = do
  root <- objectAt "root" value
  requireKeys "root"
    [ "schema_version", "profile", "semantic_space_fingerprint"
    , "expected_embedding_dimensions", "historical_cpu_authority", "installation", "tei", "model"
    , "numerical_validation", "operational_recommendations"
    , "historical_trial_envelope", "licenses" ] root
  requiredEq "schema_version" (Number 2) root
  requiredEq "profile" (String expectedProfile) root
  requiredEq "semantic_space_fingerprint" (String expectedSemanticSpace) root
  requiredEq "expected_embedding_dimensions" (Number 1536) root
  historicalCpu <- requiredObject "historical_cpu_authority" root
  requireKeys "historical_cpu_authority" ["path", "bytes", "sha256", "active_production_authority"] historicalCpu
  requireTextEq "historical_cpu_authority.path" "C:\\Users\\Sornaensis\\AppData\\Local\\Temp\\hmem-gpu-transition-planning-ba60c4e-r1.json" "path" historicalCpu
  requireIntegerEq "historical_cpu_authority.bytes" 133506 "bytes" historicalCpu
  requireTextEq "historical_cpu_authority.sha256" "ed9df8168ecb02c0439bd335a400ee73bd3fe04cc6cd88adebce703b5ac313a0" "sha256" historicalCpu
  requiredEq "historical_cpu_authority.active_production_authority" (Bool False) historicalCpu

  installation <- requiredObject "installation" root
  requireKeys "installation" ["root", "manifest_path", "model_root", "runtime_root", "entrypoint_path", "router_path"] installation
  requireTextEq "installation.root" "/opt/hmem/managed-embedding" "root" installation
  requireTextEq "installation.manifest_path" "/opt/hmem/managed-embedding/manifest/managed-embedding-provenance.yaml" "manifest_path" installation
  requireTextEq "installation.model_root" expectedInstalledModelPath "model_root" installation
  requireTextEq "installation.runtime_root" "/opt/hmem/managed-embedding/tei-runtime" "runtime_root" installation
  requireTextEq "installation.entrypoint_path" "/opt/hmem/managed-embedding/tei-runtime/entrypoint.sh" "entrypoint_path" installation
  requireTextEq "installation.router_path" "/opt/hmem/managed-embedding/tei-runtime/text-embeddings-router" "router_path" installation

  tei <- requiredObject "tei" root
  requireKeys "tei" ["release", "source_commit", "source_archive", "source_compatibility", "gpu_image", "runtime_bundle"] tei
  requireTextEq "tei.release" "v1.9.3" "release" tei
  requireTextEq "tei.source_commit" expectedTeiCommit "source_commit" tei
  archive <- requiredObject "source_archive" tei
  requireKeys "tei.source_archive" ["url", "bytes", "sha256"] archive
  requireTextEq "tei.source_archive.url"
    "https://api.github.com/repos/huggingface/text-embeddings-inference/tarball/06670157fb6c1523482219bdb2d1660277d38088"
    "url" archive
  requireIntegerEq "tei.source_archive.bytes" 1183985 "bytes" archive
  requireTextEq "tei.source_archive.sha256" "d71a340344ed1737fc54a283685885708624993b3b950578eae86ba2457efff7" "sha256" archive
  compatibility <- requiredObject "source_compatibility" tei
  requireKeys "tei.source_compatibility"
    ["backend", "model_class", "compile_compute_capability", "runtime_compute_capability", "cuda_build_runtime", "ubuntu_release", "requires_cuda", "requires_f16", "uses_config_is_causal"] compatibility
  requireTextEq "tei.source_compatibility.backend" "candle-cuda" "backend" compatibility
  requireTextEq "tei.source_compatibility.model_class" "FlashQwen2Model" "model_class" compatibility
  requireIntegerEq "tei.source_compatibility.compile_compute_capability" 120 "compile_compute_capability" compatibility
  requireIntegerEq "tei.source_compatibility.runtime_compute_capability" 120 "runtime_compute_capability" compatibility
  requireTextEq "tei.source_compatibility.cuda_build_runtime" "12.9.1" "cuda_build_runtime" compatibility
  requireTextEq "tei.source_compatibility.ubuntu_release" "24.04" "ubuntu_release" compatibility
  forM_ ["requires_cuda", "requires_f16", "uses_config_is_causal"] $ \key ->
    requiredEq ("tei.source_compatibility." <> T.unpack key) (Bool True) compatibility

  image <- requiredObject "gpu_image" tei
  requireKeys "tei.gpu_image"
    ["discovery_tag_evidence", "reference", "registry_manifest_url", "index_digest", "platform", "platform_manifest_digest", "config_digest", "config", "layers"] image
  requireTextEq "tei.gpu_image.discovery_tag_evidence" "120-1.9.3" "discovery_tag_evidence" image
  requireTextEq "tei.gpu_image.reference" expectedImageReference "reference" image
  requireTextEq "tei.gpu_image.registry_manifest_url" expectedRegistryUrl "registry_manifest_url" image
  requireTextEq "tei.gpu_image.index_digest" expectedIndexDigest "index_digest" image
  platform <- requiredObject "platform" image
  requireKeys "tei.gpu_image.platform" ["os", "architecture"] platform
  requireTextEq "tei.gpu_image.platform.os" "linux" "os" platform
  requireTextEq "tei.gpu_image.platform.architecture" "amd64" "architecture" platform
  requireTextEq "tei.gpu_image.platform_manifest_digest" expectedPlatformDigest "platform_manifest_digest" image
  requireTextEq "tei.gpu_image.config_digest" expectedConfigDigest "config_digest" image
  imageConfig <- requiredObject "config" image
  requireKeys "tei.gpu_image.config"
    ["entrypoint", "command", "working_directory", "source_label", "revision_label", "version_label", "cuda_version", "use_flash_attention"] imageConfig
  requireTextsEq "tei.gpu_image.config.entrypoint" ["./entrypoint.sh"] "entrypoint" imageConfig
  requireTextsEq "tei.gpu_image.config.command" ["--json-output"] "command" imageConfig
  requireTextEq "tei.gpu_image.config.working_directory" "/" "working_directory" imageConfig
  requireTextEq "tei.gpu_image.config.source_label" "https://github.com/huggingface/text-embeddings-inference" "source_label" imageConfig
  requireTextEq "tei.gpu_image.config.revision_label" expectedTeiCommit "revision_label" imageConfig
  requireTextEq "tei.gpu_image.config.version_label" "120-1.9.3" "version_label" imageConfig
  requireTextEq "tei.gpu_image.config.cuda_version" "12.9.1" "cuda_version" imageConfig
  requireTextEq "tei.gpu_image.config.use_flash_attention" "True" "use_flash_attention" imageConfig
  layers <- requiredArray "layers" image >>= traverse parseLayer
  whenE (layers /= expectedLayers) "tei.gpu_image.layers must be the complete ordered platform layer lock"

  runtime <- requiredObject "runtime_bundle" tei
  requireKeys "tei.runtime_bundle"
    [ "artifact_root", "source_image_reference", "source_platform_manifest_digest", "source_config_digest"
    , "system_abi_source", "retrieval_phase", "runtime_use", "artifacts"
    , "image_library_inventory_scope", "image_library_inventory", "cuda_dependency_binding"
    , "driver_injected_library", "loader", "qualified_host" ] runtime
  runtimeRoot <- T.unpack <$> requiredText "artifact_root" runtime
  whenE (not (isSafeRelative runtimeRoot) || runtimeRoot /= "tei-runtime") "tei.runtime_bundle.artifact_root must be tei-runtime"
  requireTextEq "tei.runtime_bundle.source_image_reference" expectedImageReference "source_image_reference" runtime
  requireTextEq "tei.runtime_bundle.source_platform_manifest_digest" expectedPlatformDigest "source_platform_manifest_digest" runtime
  requireTextEq "tei.runtime_bundle.source_config_digest" expectedConfigDigest "source_config_digest" runtime
  requireTextEq "tei.runtime_bundle.system_abi_source" "exact-pinned-platform-image" "system_abi_source" runtime
  requireTextEq "tei.runtime_bundle.retrieval_phase" "build-time-only" "retrieval_phase" runtime
  requireTextEq "tei.runtime_bundle.runtime_use" "verified-local-bundle" "runtime_use" runtime
  runtimeArtifacts <- requiredArray "artifacts" runtime >>= traverse parseSourcedArtifact
  rejectDuplicatePaths "tei.runtime_bundle.artifacts" (map sourcedArtifact runtimeArtifacts)
  whenE (runtimeArtifacts /= expectedRuntimeArtifacts) "tei.runtime_bundle.artifacts must be the complete pinned installed runtime inventory"
  requireTextEq "tei.runtime_bundle.image_library_inventory_scope" "direct-elf-loader-closure-plus-driver-boundary" "image_library_inventory_scope" runtime
  libraries <- requiredArray "image_library_inventory" runtime >>= traverse parseImageLibrary
  rejectDuplicatePaths "tei.runtime_bundle.image_library_inventory" (map libraryArtifact libraries)
  whenE (libraries /= expectedImageLibraries) "tei.runtime_bundle.image_library_inventory must match the inspected direct ELF closure"
  requireTextEq "tei.runtime_bundle.cuda_dependency_binding" "exact-pinned-image-layers" "cuda_dependency_binding" runtime
  driver <- requiredObject "driver_injected_library" runtime
  requireKeys "tei.runtime_bundle.driver_injected_library" ["soname", "source", "image_file", "host_copy_allowed", "hash_policy"] driver
  requireTextEq "driver_injected_library.soname" "libcuda.so.1" "soname" driver
  requireTextEq "driver_injected_library.source" "nvidia-container-runtime" "source" driver
  requiredEq "driver_injected_library.image_file" (Bool False) driver
  requiredEq "driver_injected_library.host_copy_allowed" (Bool False) driver
  requireTextEq "driver_injected_library.hash_policy" "driver-injected-not-image-bundled" "hash_policy" driver
  loader <- requiredObject "loader" runtime
  requireKeys "tei.runtime_bundle.loader"
    ["inherit_environment", "working_directory", "path", "ld_library_path", "ld_preload", "mkl_environment_allowed", "cuda_compatibility_injection", "nvidia_visible_devices", "nvidia_driver_capabilities", "hf_hub_offline", "transformers_offline", "use_flash_attention", "argv"] loader
  requiredEq "loader.inherit_environment" (Bool False) loader
  requireTextEq "loader.working_directory" "/opt/hmem/managed-embedding/tei-runtime" "working_directory" loader
  requireTextEq "loader.path" "/opt/hmem/managed-embedding/tei-runtime:/usr/local/cuda/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin" "path" loader
  requireTextEq "loader.ld_library_path" "/usr/local/cuda/lib64:/usr/local/cuda/lib64" "ld_library_path" loader
  requiredEq "loader.ld_preload" Null loader
  requiredEq "loader.mkl_environment_allowed" (Bool False) loader
  requireTextEq "loader.cuda_compatibility_injection" "forbidden-for-qualified-driver-cuda-13.2" "cuda_compatibility_injection" loader
  requireTextEq "loader.nvidia_visible_devices" "0" "nvidia_visible_devices" loader
  requireTextEq "loader.nvidia_driver_capabilities" "compute,utility" "nvidia_driver_capabilities" loader
  forM_ ["hf_hub_offline", "transformers_offline"] $ \key -> requireTextEq ("loader." <> T.unpack key) "1" key loader
  requireTextEq "loader.use_flash_attention" "True" "use_flash_attention" loader
  requireTextsEq "loader.argv" expectedArgv "argv" loader
  host <- requiredObject "qualified_host" runtime
  requireKeys "tei.runtime_bundle.qualified_host"
    ["device", "device_index", "compute_capability", "total_memory_mib", "driver_version", "driver_cuda_version", "minimum_device_contract", "minimum_driver_contract"] host
  requireTextEq "qualified_host.device" "NVIDIA GeForce RTX 5090" "device" host
  requireIntegerEq "qualified_host.device_index" 0 "device_index" host
  requireTextEq "qualified_host.compute_capability" "12.0" "compute_capability" host
  requireIntegerEq "qualified_host.total_memory_mib" 32607 "total_memory_mib" host
  requireTextEq "qualified_host.driver_version" "596.49" "driver_version" host
  requireTextEq "qualified_host.driver_cuda_version" "13.2" "driver_cuda_version" host
  requireTextEq "qualified_host.minimum_device_contract" "sm120" "minimum_device_contract" host
  requireTextEq "qualified_host.minimum_driver_contract" "satisfies-image-NVIDIA_REQUIRE_CUDA-and-not-lower-than-qualified-596.49" "minimum_driver_contract" host

  model <- requiredObject "model" root
  requireKeys "model"
    ["id", "revision", "revision_url", "snapshot_url", "artifact_root", "retrieval_phase", "runtime_use", "source_config", "serving", "endpoint_alias_contract", "artifacts"] model
  requireTextEq "model.id" "Alibaba-NLP/gte-Qwen2-1.5B-instruct" "id" model
  requireTextEq "model.revision" expectedModelRevision "revision" model
  requireTextEq "model.revision_url" ("https://huggingface.co/Alibaba-NLP/gte-Qwen2-1.5B-instruct/tree/" <> expectedModelRevision) "revision_url" model
  requireTextEq "model.snapshot_url" ("https://huggingface.co/Alibaba-NLP/gte-Qwen2-1.5B-instruct/resolve/" <> expectedModelRevision) "snapshot_url" model
  modelRoot <- T.unpack <$> requiredText "artifact_root" model
  whenE (not (isSafeRelative modelRoot) || modelRoot /= "model") "model.artifact_root must be model"
  requireTextEq "model.retrieval_phase" "build-time-only" "retrieval_phase" model
  requireTextEq "model.runtime_use" "local-read-only-snapshot" "runtime_use" model
  sourceConfig <- requiredObject "source_config" model
  requireKeys "model.source_config" ["path", "bytes", "sha256", "torch_dtype", "is_causal"] sourceConfig
  requireTextEq "model.source_config.path" "config.json" "path" sourceConfig
  requireIntegerEq "model.source_config.bytes" 901 "bytes" sourceConfig
  requireTextEq "model.source_config.sha256" "6be8440493f63c39e989842d4747127a71b1d680839af44dc105245e06a333d1" "sha256" sourceConfig
  requireTextEq "model.source_config.torch_dtype" "float32" "torch_dtype" sourceConfig
  requiredEq "model.source_config.is_causal" (Bool False) sourceConfig
  serving <- requiredObject "serving" model
  requireKeys "model.serving"
    ["inventory", "backend", "dtype", "pooling", "eos_token_id", "pad_token_id", "normalize", "auto_truncate", "max_input_tokens", "hmem_max_formatted_utf8_bytes", "default_prompt", "query_prefix"] serving
  requireTextEq "model.serving.inventory" "complete-upstream-authority" "inventory" serving
  requireTextEq "model.serving.backend" "candle-cuda" "backend" serving
  requireTextEq "model.serving.dtype" "float16" "dtype" serving
  requireTextEq "model.serving.pooling" "last-valid-token" "pooling" serving
  requireIntegerEq "model.serving.eos_token_id" 151643 "eos_token_id" serving
  requireIntegerEq "model.serving.pad_token_id" 151643 "pad_token_id" serving
  requiredEq "model.serving.normalize" (Bool True) serving
  requiredEq "model.serving.auto_truncate" (Bool False) serving
  requireIntegerEq "model.serving.max_input_tokens" 32768 "max_input_tokens" serving
  requireIntegerEq "model.serving.hmem_max_formatted_utf8_bytes" 32767 "hmem_max_formatted_utf8_bytes" serving
  requiredEq "model.serving.default_prompt" Null serving
  requireTextEq "model.serving.query_prefix" expectedQueryPrefix "query_prefix" serving
  aliasContract <- requiredObject "endpoint_alias_contract" model
  requireKeys "model.endpoint_alias_contract" ["launch_model_id", "accepted_model_id_aliases", "accepted_served_model_name_aliases", "null_model_sha_requires_operator_profile_assertion", "non_null_model_sha"] aliasContract
  requireTextEq "model.endpoint_alias_contract.launch_model_id" expectedInstalledModelPath "launch_model_id" aliasContract
  requireTextsEq "model.endpoint_alias_contract.accepted_model_id_aliases" expectedEndpointAliases "accepted_model_id_aliases" aliasContract
  requireTextsEq "model.endpoint_alias_contract.accepted_served_model_name_aliases" expectedEndpointAliases "accepted_served_model_name_aliases" aliasContract
  requiredEq "model.endpoint_alias_contract.null_model_sha_requires_operator_profile_assertion" (Bool True) aliasContract
  requireTextEq "model.endpoint_alias_contract.non_null_model_sha" expectedModelRevision "non_null_model_sha" aliasContract
  modelArtifacts <- requiredArray "artifacts" model >>= traverse parseArtifact
  rejectDuplicatePaths "model.artifacts" modelArtifacts
  whenE (modelArtifacts /= expectedModelArtifacts) "model.artifacts must be the complete ordered 20-file upstream authority"

  numerical <- requiredObject "numerical_validation" root
  requireKeys "numerical_validation" ["authority", "independent_method", "fixtures", "cases", "thresholds", "negative_probe"] numerical
  requireTextEq "numerical_validation.authority" "immutable-embedded-foundation-fixtures" "authority" numerical
  requireTextEq "numerical_validation.independent_method" "original-sdpa-math-cuda-f16-v3" "independent_method" numerical
  numericalFixtures <- requiredArray "fixtures" numerical >>= traverse parseArtifact
  whenE (numericalFixtures /= expectedNumericalFixtures) "numerical_validation.fixtures must match the immutable foundation files"
  cases <- requiredArray "cases" numerical >>= traverse parseCase
  whenE (cases /= expectedCases) "numerical_validation.cases must match the frozen case identity, role, token count, and formatted bytes"
  thresholds <- requiredObject "thresholds" numerical
  requireKeys "numerical_validation.thresholds"
    ["minimum_cosine", "maximum_l2_distance", "maximum_coordinate_absolute_error", "maximum_unit_norm_absolute_error", "dimensions", "maximum_compact_probe_tokens"] thresholds
  requiredEq "thresholds.minimum_cosine" (Number 0.9995) thresholds
  requiredEq "thresholds.maximum_l2_distance" (Number 0.032) thresholds
  requiredEq "thresholds.maximum_coordinate_absolute_error" (Number 0.005) thresholds
  requiredEq "thresholds.maximum_unit_norm_absolute_error" (Number 0.0001) thresholds
  requiredEq "thresholds.dimensions" (Number 1536) thresholds
  requiredEq "thresholds.maximum_compact_probe_tokens" (Number 2048) thresholds
  requireTextEq "numerical_validation.negative_probe" "wrong-prompt-must-fail" "negative_probe" numerical

  operational <- requiredObject "operational_recommendations" root
  requireKeys "operational_recommendations"
    ["authority_path", "startup_timeout_seconds", "compact_request_timeout_seconds", "full_context_request_timeout_seconds", "qualified_compact_max_input_tokens", "qualified_compact_batch_items", "qualified_compact_concurrent_requests", "hmem_aggregate_input_budget", "hmem_long_request_admission", "hmem_aggregate_token_budget", "four_maximum_length_requests_qualified", "full_context_concurrency_qualified"] operational
  requireTextEq "operational_recommendations.authority_path" "hmem-server/test/fixtures/embedding-gpu-viability/gpu-foundation-qualification-v1.json" "authority_path" operational
  forM_ [("startup_timeout_seconds",120),("compact_request_timeout_seconds",30),("full_context_request_timeout_seconds",300),("qualified_compact_max_input_tokens",2048),("qualified_compact_batch_items",4),("qualified_compact_concurrent_requests",2),("hmem_aggregate_input_budget",4),("hmem_aggregate_token_budget",32768)] $ \(key, expected) ->
    requireIntegerEq ("operational_recommendations." <> key) expected (T.pack key) operational
  requireTextEq "operational_recommendations.hmem_long_request_admission" "singleton" "hmem_long_request_admission" operational
  requiredEq "operational_recommendations.four_maximum_length_requests_qualified" (Bool False) operational
  requiredEq "operational_recommendations.full_context_concurrency_qualified" (Bool False) operational
  historical <- requiredObject "historical_trial_envelope" root
  requireKeys "historical_trial_envelope"
    ["authority_path", "production_defaults", "startup_timeout_seconds", "short_request_timeout_seconds", "full_context_request_timeout_seconds", "max_concurrent_requests"] historical
  requireTextEq "historical_trial_envelope.authority_path" "hmem-server/test/fixtures/embedding-gpu-viability/foundation-contract-v1.json" "authority_path" historical
  requiredEq "historical_trial_envelope.production_defaults" (Bool False) historical
  forM_ [("startup_timeout_seconds",600),("short_request_timeout_seconds",120),("full_context_request_timeout_seconds",1800),("max_concurrent_requests",8)] $ \(key, expected) ->
    requireIntegerEq ("historical_trial_envelope." <> key) expected (T.pack key) historical

  licenses <- requiredArray "licenses" root >>= traverse parseLicense
  let paths = map (artifactPath . licenseArtifact) licenses
      components = map licenseComponent licenses
  whenE (length paths /= length (nub paths)) "licenses contains duplicate notice paths"
  whenE (length components /= length (nub components)) "licenses contains duplicate components"
  whenE (licenses /= expectedLicenses) "licenses must match the exact TEI, model, and NVIDIA notice authority"
  whenE (modelRoot == runtimeRoot) "model and runtime artifact roots must differ"
  pure Lock
    { lockModelRoot = modelRoot
    , lockModelArtifacts = modelArtifacts
    , lockRuntimeRoot = runtimeRoot
    , lockRuntimeArtifacts = map sourcedArtifact runtimeArtifacts
    , lockImageLibraries = map (stripLeadingSlash . libraryArtifact) libraries
    , lockLicenses = licenses
    }

manifestPath :: FilePath
manifestPath = "config/managed-embedding-provenance.yaml"

expectedProfile, expectedSemanticSpace, expectedTeiCommit, expectedModelRevision :: Text
expectedProfile = "native-tei-gte-qwen2-1.5b-instruct-cuda-sm120-f16-v1"
expectedSemanticSpace = "Alibaba-NLP/gte-Qwen2-1.5B-instruct@1cad2ab3ff41c2671f34e135d29831368ee26b68:1536:attention=noncausal:v1"
expectedTeiCommit = "06670157fb6c1523482219bdb2d1660277d38088"
expectedModelRevision = "1cad2ab3ff41c2671f34e135d29831368ee26b68"

expectedImageReference, expectedIndexDigest, expectedPlatformDigest, expectedConfigDigest, expectedRegistryUrl :: Text
expectedIndexDigest = "sha256:aedf3b34836dc57289583142adcf2b93836cda0736ac8e6ce43691b9c2c67170"
expectedImageReference = "ghcr.io/huggingface/text-embeddings-inference@" <> expectedIndexDigest
expectedPlatformDigest = "sha256:144aaa80ddcb520d49df83f915dc188ddd7cc6b1b3b9684a829c21dd39cbe3c5"
expectedConfigDigest = "sha256:affa793eda6c6c6583d9c4041372dd710d74a55ab0a89025a1f322dd71b92eb2"
expectedRegistryUrl = "https://ghcr.io/v2/huggingface/text-embeddings-inference/manifests/" <> expectedIndexDigest

expectedQueryPrefix :: Text
expectedQueryPrefix = "Instruct: Given a web search query, retrieve relevant passages that answer the query\nQuery: "

expectedInstalledModelPath :: Text
expectedInstalledModelPath = "/opt/hmem/managed-embedding/model"

expectedEndpointAliases :: [Text]
expectedEndpointAliases =
  [ expectedInstalledModelPath
  , "/model"
  , "Alibaba-NLP/gte-Qwen2-1.5B-instruct"
  ]

expectedArgv :: [Text]
expectedArgv =
  [ "/opt/hmem/managed-embedding/tei-runtime/entrypoint.sh", "--model-id"
  , expectedInstalledModelPath, "--dtype", "float16"
  , "--max-batch-tokens", "32768", "--auto-truncate", "false"
  , "--max-concurrent-requests", "4", "--max-client-batch-size", "4"
  , "--tokenization-workers", "2", "--hostname", "127.0.0.1"
  , "--port", "8080", "--prometheus-port", "9000", "--json-output" ]

expectedLayers :: [(Integer, Text, Integer)]
expectedLayers =
  [ (1, "sha256:32f112e3802cadcab3543160f4d2aa607b3cc1c62140d57b4f5441384f40e927", 29721175)
  , (2, "sha256:644e9b20358325501941bab7efe2465969b1101fd546be263fc7c2d12d2d8c6c", 4546956)
  , (3, "sha256:02559cd4bc8db240554ff3a4e38df5909d4745846b3e3a4df865d102f0a6d0d9", 103517429)
  , (4, "sha256:2cd52cbb1ebefad9b697ee1c60a7ba7ce8293404a1ad596fd257efdf0e9ec8d6", 182)
  , (5, "sha256:6e8af4fd0a071982e528b634ba99dec2474c21147f99748be708f36e10e3f4c2", 6885)
  , (6, "sha256:15a17189b2df6f2a5b84716dd422741e91bec18d40f6cd72f7dea007a02d45aa", 2303978379)
  , (7, "sha256:02cb0e091e334f5f0b222d9bc044d174ed386f1b30d3959d0e79ed725eb9cb15", 59653)
  , (8, "sha256:9c3d619183d2383e5d618638762511e6711c112a9e8f7e50031f94b839b2c25e", 1683)
  , (9, "sha256:7f7602a82106bfe07dabfc1cd3799c2b0ee0f1314f4cb28535f7f1b7c00fbec4", 1523)
  , (10, "sha256:5d0fd49fa0beb31c828306a31b7d82dd5a05caafd729b59ebdf4f6258eef50f8", 8293206)
  , (11, "sha256:accfe16c0f24bec67e21206f8de8ad2d88efc9b69e362b635370dac1d74bf455", 635)
  , (12, "sha256:218b749184cd4a74888c41a36f79c7d56790e6d49487c6e2709c868a5668600e", 708022063)
  ]

expectedRuntimeArtifacts :: [SourcedArtifact]
expectedRuntimeArtifacts =
  [ SourcedArtifact (Artifact "entrypoint.sh" 917 "1306281c21ea511af3c6dbcfe9a80ee11627c7597b07cb8163608d8f55308bac") "/entrypoint.sh"
  , SourcedArtifact (Artifact "text-embeddings-router" 1208166216 "8eeff6c10326b9ebaf3796bfa938a1f6ecc77b856a4622f747d73593e3587990") "/usr/local/bin/text-embeddings-router"
  ]

expectedImageLibraries :: [ImageLibrary]
expectedImageLibraries =
  [ lib "dynamic-loader" "/usr/lib/x86_64-linux-gnu/ld-linux-x86-64.so.2" 236616 "4f961aefd1ecbc91b6de5980623aa389ca56e8bfb5f2a1d2a0b94b54b0fde894"
  , lib "libc" "/usr/lib/x86_64-linux-gnu/libc.so.6" 2125328 "de259f5276c4a991f78bf87225d6b40e56edbffe0dcbc0ffca36ec7fe30f3f77"
  , lib "libm" "/usr/lib/x86_64-linux-gnu/libm.so.6" 952616 "3c24a53ee35c2ce0c67240e62bff699c4bddcd7cf8993d5d7ad29157ba072c99"
  , lib "libgcc" "/usr/lib/x86_64-linux-gnu/libgcc_s.so.1" 183024 "02f3f192bf5f79b811f1e34a650fea443d407408703448906585232383957f60"
  , lib "libcrypto" "/usr/lib/x86_64-linux-gnu/libcrypto.so.3" 5309400 "d6fc1bc9de29c55fc905f77edba1ccc7c7a50b32bd2bb9086b0d0b00104eafc4"
  , lib "libssl" "/usr/lib/x86_64-linux-gnu/libssl.so.3" 696512 "0c0f298a5b4b44526d20a07d126a55bf44b85eaab053b2b0118e5d806d28ea13"
  , lib "libstdc++" "/usr/lib/x86_64-linux-gnu/libstdc++.so.6.0.33" 2592224 "a68762c86d371e6041f03f03a33a78fa235809ae7d81c90185940c93c3535aed"
  ]
  where lib role path bytes hash = ImageLibrary role (Artifact path bytes hash)

expectedModelArtifacts :: [Artifact]
expectedModelArtifacts =
  [ art ".gitattributes" 1519 "11ad7efa24975ee4b0c3c3a38ed18737f0658a5f75a0a96787b576a78a023361"
  , art "1_Pooling/config.json" 297 "40d120b92655c16390bd52c43540ada5f74ab94991d00838df6742c0d640d35b"
  , art "README.md" 146265 "06f46ea6b3ab7eb581f1fb170e6144c94493c076232dae7a65a75bef45133fea"
  , art "added_tokens.json" 80 "6a475432c61f8d6154d10d28c37671a36e5717daf3d15002a988968fee54a500"
  , art "config.json" 901 "6be8440493f63c39e989842d4747127a71b1d680839af44dc105245e06a333d1"
  , art "config_sentence_transformers.json" 284 "89df38eb06c9f934ee613d846fd9ea88468cc99791e036b5bbc412411fbb1c95"
  , art "generation_config.json" 117 "71e135315a5c53cfbd7418a5fa02b03dad5e59df3e28c16f9970553c157805a9"
  , art "merges.txt" 1671853 "8831e4f1a044471340f7c0a83d7bd71306a5b867e95fd870f74d0c5308a904d5"
  , art "model-00001-of-00002.safetensors" 4994888704 "0842014813f9ecd814eac671e2577d84281cd02d74f70436583c044fac724072"
  , art "model-00002-of-00002.safetensors" 2109938216 "661830fd8d0426f38747a747ec37cbdc05466331790dcf7b83e12a0e000ce0cc"
  , art "model.safetensors.index.json" 27751 "6ed4fc9a5fed84fc2401db6223b1f673eb2e0c01597660d67eddf63b525fbc97"
  , art "modeling_qwen.py" 65201 "8851d692b05bbf3b06a9ada6c0c9c857df6461f2a2b093e7fa831c1078040602"
  , art "modules.json" 349 "84e40c8e006c9b1d6c122e02cba9b02458120b5fb0c87b746c41e0207cf642cf"
  , art "scripts/eval_mteb.py" 36724 "2a53266b072c36d8ecb0577af67d3439b4a240250d9c870f18a57c777b52232a"
  , art "sentence_bert_config.json" 55 "1140b92d307aec9383d897169c9f489f3a568787b80bf760f5d7cc9d25169a65"
  , art "special_tokens_map.json" 370 "daf48284de8f4779b1dbf20963a68180002fba2a34a5da72292380c5d9fb6af2"
  , art "tokenization_qwen.py" 10811 "e15b8ea81d39b3dda3acf751b4a00a7dabbf9f687e026608c06b3849925e3169"
  , art "tokenizer.json" 7028015 "f7c9b2dba4a296b1aa76c16a34b8225c0c118978400d4bb66bff0902d702f5b8"
  , art "tokenizer_config.json" 1312 "d1b4928d0e7e7c1881a23eb235f6081d6d9db3d67f6ce5272f571feaf12fd944"
  , art "vocab.json" 2776833 "ca10d7e9fb3ed18575dd1e277a2579c16d108e32f27439684afa0e10b1440910"
  ]
  where art = Artifact

expectedNumericalFixtures :: [Artifact]
expectedNumericalFixtures =
  [ Artifact "hmem-server/test/fixtures/embedding-gpu-viability/foundation-contract-v1.json" 7902 "ba776b330ff908a539b37fa41da7f0cd248a18f749c0ce606854171c6e6f6f0e"
  , Artifact "hmem-server/test/fixtures/embedding-gpu-viability/parity-probes-v1.json" 68993 "1eded3104b9d3f18ae0a152aa6796d066c637ad1f3ceabea55b299d32b39aff7"
  , Artifact "hmem-server/test/fixtures/embedding-gpu-viability/reference-golden-v1.json" 150199 "564a22ba414bd41e1e13d3d07db79eb88d45a30e3b2d9813ee5f02b5a32a6c93"
  ]

type CaseLock = (Text, Text, Integer, Text)
expectedCases :: [CaseLock]
expectedCases =
  [ ("doc_short", "document", 10, "69fa9cfe78c80c6e560acb593e8a5e744d1094c396035222f2268714a2c97141")
  , ("mixed_long", "document", 2048, "1c674117e897f8b03f7bf6acf7f3b3880e7c19907c595a641c5e94a0efe6cb67")
  , ("multilingual", "document", 21, "d0e359a7284619e28991002111d6e5efe2abd51cccb6a309cdeeba3262b5dbfc")
  , ("query_search", "query", 26, "4b2a5f15a7132d24234475398c238dfa06c92e48678d4c6b33098c75e1dd5aef")
  ]

expectedLicenses :: [LicenseRecord]
expectedLicenses =
  [ lic "text-embeddings-inference" "Apache-2.0" "licenses/managed-embedding/tei-APACHE-2.0.txt" 11342 "28216184827860ae83a60244c8868de4cf84e16357d16cdb975a46afe3afa9db" "https://raw.githubusercontent.com/huggingface/text-embeddings-inference/06670157fb6c1523482219bdb2d1660277d38088/LICENSE"
  , lic "Alibaba-NLP/gte-Qwen2-1.5B-instruct" "Apache-2.0" "licenses/managed-embedding/gte-Qwen2-1.5B-instruct-license-metadata.txt" 20 "5e05dfca6eeba9772cf92c589149b62c0fd75573469b8801649008df8f8e8f5f" "https://huggingface.co/Alibaba-NLP/gte-Qwen2-1.5B-instruct/blob/1cad2ab3ff41c2671f34e135d29831368ee26b68/README.md"
  , lic "NVIDIA-CUDA-runtime-container" "LicenseRef-NVIDIA-Deep-Learning-Container" "licenses/managed-embedding/nvidia-deep-learning-container-license.txt" 17294 "e4196076c5496c4bb5509be61e3d1cddf36b92a449a10ece1779afce3c65e684" "oci://ghcr.io/huggingface/text-embeddings-inference@sha256:aedf3b34836dc57289583142adcf2b93836cda0736ac8e6ce43691b9c2c67170/NGC-DL-CONTAINER-LICENSE"
  ]
  where lic component expression path bytes hash source =
          LicenseRecord component expression (Artifact path bytes hash) source

parseArtifact :: Value -> Either [String] Artifact
parseArtifact value = do
  object <- objectAt "artifact" value
  requireKeys "artifact" ["path", "bytes", "sha256"] object
  path <- T.unpack <$> requiredText "path" object
  bytes <- integerAt "bytes" object
  hash <- T.unpack <$> requiredText "sha256" object
  whenE (not (isSafeRelative path)) ("artifact path is unsafe: " <> path)
  whenE (bytes < 0) ("artifact size is negative: " <> path)
  whenE (not (isSha256 hash)) ("artifact checksum is not SHA-256: " <> path)
  pure (Artifact path bytes hash)

parseSourcedArtifact :: Value -> Either [String] SourcedArtifact
parseSourcedArtifact value = do
  object <- objectAt "runtime artifact" value
  requireKeys "runtime artifact" ["path", "source_path", "bytes", "sha256"] object
  path <- T.unpack <$> requiredText "path" object
  source <- T.unpack <$> requiredText "source_path" object
  bytes <- integerAt "bytes" object
  hash <- T.unpack <$> requiredText "sha256" object
  whenE (not (isSafeRelative path)) ("runtime artifact path is unsafe: " <> path)
  whenE (not (isSafeAbsoluteImagePath source)) ("runtime source path is unsafe: " <> source)
  whenE (bytes < 0 || not (isSha256 hash)) ("runtime artifact lock is invalid: " <> path)
  pure (SourcedArtifact (Artifact path bytes hash) source)

parseImageLibrary :: Value -> Either [String] ImageLibrary
parseImageLibrary value = do
  object <- objectAt "image library" value
  requireKeys "image library" ["role", "path", "bytes", "sha256"] object
  role <- T.unpack <$> requiredText "role" object
  path <- T.unpack <$> requiredText "path" object
  bytes <- integerAt "bytes" object
  hash <- T.unpack <$> requiredText "sha256" object
  whenE (null role || not (isSafeAbsoluteImagePath path)) ("image library path is unsafe: " <> path)
  whenE (bytes < 0 || not (isSha256 hash)) ("image library lock is invalid: " <> path)
  pure (ImageLibrary role (Artifact path bytes hash))

parseLayer :: Value -> Either [String] (Integer, Text, Integer)
parseLayer value = do
  object <- objectAt "image layer" value
  requireKeys "image layer" ["ordinal", "digest", "bytes"] object
  ordinal <- integerAt "ordinal" object
  digest <- requiredText "digest" object
  bytes <- integerAt "bytes" object
  whenE (not (isDigest digest) || bytes <= 0) "image layer digest or size is invalid"
  pure (ordinal, digest, bytes)

parseCase :: Value -> Either [String] CaseLock
parseCase value = do
  object <- objectAt "numerical case" value
  requireKeys "numerical case" ["id", "role", "token_count", "formatted_utf8_sha256"] object
  identifier <- requiredText "id" object
  role <- requiredText "role" object
  count <- integerAt "token_count" object
  hash <- requiredText "formatted_utf8_sha256" object
  whenE (not (isSha256 (T.unpack hash))) "numerical case formatted hash is invalid"
  pure (identifier, role, count, hash)

parseLicense :: Value -> Either [String] LicenseRecord
parseLicense value = do
  object <- objectAt "license" value
  requireKeys "license" ["component", "expression", "notice_path", "notice_bytes", "notice_sha256", "source_identity"] object
  component <- T.unpack <$> requiredText "component" object
  expression <- T.unpack <$> requiredText "expression" object
  path <- T.unpack <$> requiredText "notice_path" object
  bytes <- integerAt "notice_bytes" object
  hash <- T.unpack <$> requiredText "notice_sha256" object
  source <- T.unpack <$> requiredText "source_identity" object
  whenE (not (isSafeRelative path) || not ("licenses/managed-embedding/" `isPrefixOf` canonicalPath path)) ("license path is unsafe: " <> path)
  whenE (bytes < 0 || not (isSha256 hash)) ("license lock is invalid: " <> path)
  whenE (not ("https://" `isPrefixOf` source || "oci://" `isPrefixOf` source)) ("license source identity is not immutable upstream evidence: " <> component)
  pure (LicenseRecord component expression (Artifact path bytes hash) source)

validateLicenses :: FilePath -> [LicenseRecord] -> IO [String]
validateLicenses root records =
  fmap concat . forM records $ \record ->
    validateOneFile "license notice" root (licenseArtifact record)

validatePinnedFiles :: String -> FilePath -> [Artifact] -> IO [String]
validatePinnedFiles label root artifacts =
  fmap concat (forM artifacts (validateOneFile label root))

validateArtifactTree :: String -> FilePath -> [Artifact] -> IO [String]
validateArtifactTree label root expected = do
  rootExists <- doesPathExist root
  rootLinked <- if rootExists then pathIsSymbolicLink root else pure False
  rootDirectory <- if rootExists && not rootLinked then doesDirectoryExist root else pure False
  containment <- if rootExists then canonicalSelfErrors root else pure []
  if not rootExists
    then pure ["missing " <> label <> " root: " <> root]
    else if rootLinked
      then pure ["unsafe symlink " <> label <> " root: " <> root]
      else if not rootDirectory
        then pure ["unsafe non-directory " <> label <> " root: " <> root]
        else do
          (actual, safetyErrors) <- filesUnder root
          let expectedPaths = sort (map (canonicalPath . artifactPath) expected)
              absent = filter (`notElem` actual) expectedPaths
              extra = filter (`notElem` expectedPaths) actual
          fileErrors <- fmap concat (forM expected (validateOneFile label root))
          pure $ containment <> safetyErrors <> fileErrors
            <> ["absent " <> label <> ": " <> path | path <- absent]
            <> ["unlisted " <> label <> ": " <> path | path <- extra]

validateOneFile :: String -> FilePath -> Artifact -> IO [String]
validateOneFile label root artifact = do
  pathErrors <- validateExpectedFilePath root (artifactPath artifact)
  let path = root </> artifactPath artifact
  exists <- doesPathExist path
  if not exists
    then pure (pathErrors <> ["missing " <> label <> ": " <> artifactPath artifact])
    else if not (null pathErrors)
      then pure pathErrors
    else do
      regular <- isRegularFile path
      if not regular
        then pure (pathErrors <> ["unsafe non-regular " <> label <> ": " <> artifactPath artifact])
        else do
          size <- withBinaryFile path ReadMode hFileSize
          hash <- if size == artifactBytes artifact then sha256File path else pure ""
          pure $
            ["size drift for " <> label <> ": " <> artifactPath artifact | size /= artifactBytes artifact]
            <> ["checksum drift for " <> label <> ": " <> artifactPath artifact | size == artifactBytes artifact && hash /= artifactHash artifact]

validateExpectedFilePath :: FilePath -> FilePath -> IO [String]
validateExpectedFilePath root relative
  | not (isSafeRelative relative) = pure ["unsafe expected artifact path: " <> relative]
  | otherwise = go root (splitDirectories relative)
  where
    go _ [] = pure []
    go current (component:rest) = do
      let next = current </> component
      exists <- doesPathExist next
      if not exists then pure [] else do
        linked <- pathIsSymbolicLink next
        if linked
          then pure ["unsafe symlink artifact path: " <> next]
          else if null rest
            then do
              regular <- isRegularFile next
              pure ["unsafe non-regular expected artifact: " <> next | not regular]
            else do
              directory <- doesDirectoryExist next
              if directory then go next rest else pure ["unsafe non-directory artifact ancestor: " <> next]

filesUnder :: FilePath -> IO ([FilePath], [String])
filesUnder root = go ""
  where
    go relative = do
      entries <- listDirectory (root </> relative)
      parts <- forM entries $ \entry -> do
        let next = if null relative then entry else relative </> entry
            full = root </> next
        linked <- pathIsSymbolicLink full
        if linked
          then pure ([], ["unsafe symlink artifact entry: " <> full])
          else do
            directory <- doesDirectoryExist full
            file <- doesFileExist full
            if directory
              then go next
              else if file
                then pure ([canonicalPath next], [])
                else pure ([], ["unsafe non-regular artifact entry: " <> full])
      pure (sort (concatMap fst parts), concatMap snd parts)

canonicalSelfErrors :: FilePath -> IO [String]
canonicalSelfErrors root = do
  canonical <- canonicalizePath root
  let driveKey = map toLower . canonicalPath . takeDrive
  pure ["artifact root canonicalization changed drive unexpectedly: " <> root | driveKey canonical /= driveKey root && not (null (takeDrive root))]

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

sha256File :: FilePath -> IO String
sha256File path = withBinaryFile path ReadMode (go hashInit)
  where
    go :: Context SHA256 -> Handle -> IO String
    go !context handle = do
      chunk <- BS.hGetSome handle (1024 * 1024)
      if BS.null chunk
        then pure (show (hashFinalize context :: Digest SHA256))
        else go (hashUpdate context chunk) handle

objectAt :: String -> Value -> Either [String] Object
objectAt _ (Object value) = Right value
objectAt label _ = Left [label <> " must be an object"]

requiredObject :: Text -> Object -> Either [String] Object
requiredObject key object = maybe (Left [T.unpack key <> " is required"]) (objectAt (T.unpack key)) (KM.lookup (Key.fromText key) object)

requiredArray :: Text -> Object -> Either [String] [Value]
requiredArray key object = case KM.lookup (Key.fromText key) object of
  Just (Array values) -> Right (V.toList values)
  Just _ -> Left [T.unpack key <> " must be an array"]
  Nothing -> Left [T.unpack key <> " is required"]

requiredText :: Text -> Object -> Either [String] Text
requiredText key object = case KM.lookup (Key.fromText key) object of
  Just (String value) -> Right value
  Just _ -> Left [T.unpack key <> " must be text"]
  Nothing -> Left [T.unpack key <> " is required"]

integerAt :: Text -> Object -> Either [String] Integer
integerAt key object = case KM.lookup (Key.fromText key) object of
  Just (Number value) -> case (floatingOrInteger value :: Either Double Integer) of
    Right integer -> Right integer
    Left _ -> Left [T.unpack key <> " must be an integer"]
  Just _ -> Left [T.unpack key <> " must be an integer"]
  Nothing -> Left [T.unpack key <> " is required"]

requiredEq :: String -> Value -> Object -> Either [String] ()
requiredEq label expected object =
  let key = last (splitOnDot label)
  in case KM.lookup (Key.fromText (T.pack key)) object of
    Just actual | actual == expected -> Right ()
    Just _ -> Left [label <> " does not match the immutable GPU contract"]
    Nothing -> Left [label <> " is required"]

requireTextEq :: String -> Text -> Text -> Object -> Either [String] ()
requireTextEq label expected key object = requiredText key object >>= \actual -> whenE (actual /= expected) (label <> " does not match the immutable GPU contract")

requireIntegerEq :: String -> Integer -> Text -> Object -> Either [String] ()
requireIntegerEq label expected key object = integerAt key object >>= \actual -> whenE (actual /= expected) (label <> " does not match the immutable GPU contract")

requireTextsEq :: String -> [Text] -> Text -> Object -> Either [String] ()
requireTextsEq label expected key object = do
  values <- requiredArray key object
  actual <- traverse asText values
  whenE (actual /= expected) (label <> " does not match the immutable GPU contract")
  where asText (String value) = Right value
        asText _ = Left [label <> " entries must be text"]

requireKeys :: String -> [Text] -> Object -> Either [String] ()
requireKeys label expected object =
  let actual = sort (map Key.toText (KM.keys object))
      wanted = sort expected
  in whenE (actual /= wanted) (label <> " keys differ from schema_version 2")

rejectDuplicatePaths :: String -> [Artifact] -> Either [String] ()
rejectDuplicatePaths label artifacts =
  let paths = map (canonicalPath . artifactPath) artifacts
  in whenE (length paths /= length (nub paths)) (label <> " contains duplicate paths")

stripLeadingSlash :: Artifact -> Artifact
stripLeadingSlash artifact = artifact { artifactPath = dropWhile (== '/') (artifactPath artifact) }

isSafeRelative :: FilePath -> Bool
isSafeRelative path =
  not (null path) && not (isAbsolute path) && null (takeDrive path)
    && all safePart (splitDirectories path)
    && all safePart (splitDirectories (normalise path))
  where safePart part = part /= ".." && part /= "." && not (null part)

isSafeAbsoluteImagePath :: FilePath -> Bool
isSafeAbsoluteImagePath path =
  "/" `isPrefixOf` path && not ("//" `isPrefixOf` path)
    && all safePart (filter (not . null) (splitDirectories path))
    && all safePart (filter (not . null) (splitDirectories (normalise path)))
  where safePart part = part /= ".." && part /= "."

canonicalPath :: FilePath -> FilePath
canonicalPath = map (\c -> if c == '\\' then '/' else c) . normalise

isSha256 :: String -> Bool
isSha256 value = length value == 64 && all isHexDigit value && map toLowerAscii value == value

isDigest :: Text -> Bool
isDigest value = "sha256:" `T.isPrefixOf` value && isSha256 (T.unpack (T.drop 7 value))

toLowerAscii :: Char -> Char
toLowerAscii c | c >= 'A' && c <= 'F' = toEnum (fromEnum c + 32)
               | otherwise = c

whenE :: Bool -> String -> Either [String] ()
whenE condition message = if condition then Left [message] else Right ()

isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys

splitOnDot :: String -> [String]
splitOnDot value = case break (== '.') value of
  (before, []) -> [before]
  (before, _:after) -> before : splitOnDot after

selfTest :: IO ()
selfTest = do
  counter <- newIORef 0
  cwd <- getCurrentDirectory
  decoded <- Yaml.decodeFileEither (cwd </> manifestPath)
  production <- case decoded of
    Left err -> die ["self-test cannot load production manifest: " <> Yaml.prettyPrintParseException err]
    Right value -> pure value
  pass counter "production schema positive" (either (const False) (const True) (parseLock production))
  case parseLock production of
    Left errors -> die errors
    Right lock -> do
      licenseErrors <- validateLicenses cwd (lockLicenses lock)
      numericalErrors <- validatePinnedFiles "numerical authority" cwd expectedNumericalFixtures
      pass counter "production license and numerical bytes positive" (null (licenseErrors <> numericalErrors))

  let mutations =
        [ ("old schema", "schema_version", setPath ["schema_version"] (Number 1))
        , ("wrong profile", "profile", setPath ["profile"] (String "managed-tei-gte-qwen2-1.5b-instruct"))
        , ("wrong semantic space", "semantic_space_fingerprint", setPath ["semantic_space_fingerprint"] (String "wrong"))
        , ("historical CPU self promotion", "historical_cpu_authority.active_production_authority", setPath ["historical_cpu_authority","active_production_authority"] (Bool True))
        , ("wrong installation path", "installation.router_path", setPath ["installation","router_path"] (String "/tmp/router"))
        , ("CPU image field", "tei keys", insertPath ["tei","cpu_image"] (Object KM.empty))
        , ("wrong source", "tei.source_commit", setPath ["tei","source_commit"] (String "0000000000000000000000000000000000000000"))
        , ("wrong image index", "tei.gpu_image.index_digest", setPath ["tei","gpu_image","index_digest"] (String "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"))
        , ("wrong image config", "tei.gpu_image.config_digest", setPath ["tei","gpu_image","config_digest"] (String "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"))
        , ("wrong platform", "tei.gpu_image.platform.architecture", setPath ["tei","gpu_image","platform","architecture"] (String "arm64"))
        , ("missing image layer", "tei.gpu_image.layers", modifyArrayPath ["tei","gpu_image","layers"] dropLast)
        , ("reordered image layers", "tei.gpu_image.layers", modifyArrayPath ["tei","gpu_image","layers"] V.reverse)
        , ("wrong runtime profile source", "tei.runtime_bundle.source_image_reference", setPath ["tei","runtime_bundle","source_image_reference"] (String "wrong"))
        , ("missing runtime artifact", "tei.runtime_bundle.artifacts", modifyArrayPath ["tei","runtime_bundle","artifacts"] dropLast)
        , ("duplicate runtime artifact", "duplicate paths", modifyArrayPath ["tei","runtime_bundle","artifacts"] duplicateFirst)
        , ("unsafe runtime artifact", "unsafe", setPath ["tei","runtime_bundle","artifacts","0","path"] (String "../router"))
        , ("wrong runtime hash", "runtime inventory", setPath ["tei","runtime_bundle","artifacts","0","sha256"] (String (T.replicate 64 "a")))
        , ("wrong runtime size", "runtime inventory", setPath ["tei","runtime_bundle","artifacts","0","bytes"] (Number 1))
        , ("wrong library inventory", "ELF closure", setPath ["tei","runtime_bundle","image_library_inventory","0","bytes"] (Number 1))
        , ("driver libcuda image file", "driver_injected_library.image_file", setPath ["tei","runtime_bundle","driver_injected_library","image_file"] (Bool True))
        , ("host libcuda copy", "driver_injected_library.host_copy_allowed", setPath ["tei","runtime_bundle","driver_injected_library","host_copy_allowed"] (Bool True))
        , ("LD_PRELOAD", "loader.ld_preload", setPath ["tei","runtime_bundle","loader","ld_preload"] (String "/tmp/lib.so"))
        , ("wrong device", "qualified_host.compute_capability", setPath ["tei","runtime_bundle","qualified_host","compute_capability"] (String "8.0"))
        , ("wrong serving dtype", "model.serving.dtype", setPath ["model","serving","dtype"] (String "float32"))
        , ("wrong source dtype", "model.source_config.torch_dtype", setPath ["model","source_config","torch_dtype"] (String "float16"))
        , ("causal source config", "model.source_config.is_causal", setPath ["model","source_config","is_causal"] (Bool True))
        , ("wrong launch alias", "endpoint_alias_contract.launch_model_id", setPath ["model","endpoint_alias_contract","launch_model_id"] (String "/model"))
        , ("installed model alias omitted", "endpoint_alias_contract.accepted_model_id_aliases", setPath ["model","endpoint_alias_contract","accepted_model_id_aliases","0"] (String "/tmp/model"))
        , ("arbitrary served-model alias", "endpoint_alias_contract.accepted_served_model_name_aliases", setPath ["model","endpoint_alias_contract","accepted_served_model_name_aliases","2"] (String "/tmp/model"))
        , ("wrong model revision", "model.revision", setPath ["model","revision"] (String "0000000000000000000000000000000000000000"))
        , ("missing model artifact", "20-file", modifyArrayPath ["model","artifacts"] dropLast)
        , ("extra model artifact", "20-file", modifyArrayPath ["model","artifacts"] appendExtraArtifact)
        , ("duplicate model artifact", "duplicate paths", modifyArrayPath ["model","artifacts"] duplicateFirst)
        , ("mutable manifest self blessing", "20-file", setPath ["model","artifacts","0","sha256"] (String (T.replicate 64 "a")))
        , ("wrong numerical method", "independent_method", setPath ["numerical_validation","independent_method"] (String "endpoint-self-golden"))
        , ("wrong numerical fixture", "foundation files", setPath ["numerical_validation","fixtures","0","sha256"] (String (T.replicate 64 "a")))
        , ("wrong numerical case", "frozen case", setPath ["numerical_validation","cases","0","id"] (String "other"))
        , ("wrong threshold", "thresholds.minimum_cosine", setPath ["numerical_validation","thresholds","minimum_cosine"] (Number 0.9))
        , ("wrong operational recommendation", "operational_recommendations.startup_timeout_seconds", setPath ["operational_recommendations","startup_timeout_seconds"] (Number 600))
        , ("two TEI input permits", "loader.argv", setPath ["tei","runtime_bundle","loader","argv","10"] (String "2"))
        , ("historical eight TEI input permits", "loader.argv", setPath ["tei","runtime_bundle","loader","argv","10"] (String "8"))
        , ("changed client batch ceiling", "loader.argv", setPath ["tei","runtime_bundle","loader","argv","12"] (String "8"))
        , ("logical concurrency promoted to input permits", "operational_recommendations.qualified_compact_concurrent_requests", setPath ["operational_recommendations","qualified_compact_concurrent_requests"] (Number 4))
        , ("aggregate input budget exceeds evidence", "operational_recommendations.hmem_aggregate_input_budget", setPath ["operational_recommendations","hmem_aggregate_input_budget"] (Number 8))
        , ("long requests admitted concurrently", "operational_recommendations.hmem_long_request_admission", setPath ["operational_recommendations","hmem_long_request_admission"] (String "router-ceiling"))
        , ("four maximum requests promoted", "operational_recommendations.four_maximum_length_requests_qualified", setPath ["operational_recommendations","four_maximum_length_requests_qualified"] (Bool True))
        , ("historical limits promoted", "historical_trial_envelope.production_defaults", setPath ["historical_trial_envelope","production_defaults"] (Bool True))
        , ("missing NVIDIA license", "licenses", modifyArrayPath ["licenses"] dropLast)
        ]
  forM_ mutations $ \(label, needle, mutation) ->
    expectRejected counter label needle (mutation production)

  withSystemTempDirectory "hmem-gpu-provenance" $ \temporary -> do
    let modelRoot = temporary </> "model"
        runtimeRoot = temporary </> "runtime"
        modelFile = modelRoot </> "nested/model.bin"
        runtimeFile = runtimeRoot </> "router.bin"
    createDirectoryIfMissing True (takeDirectory modelFile)
    createDirectoryIfMissing True runtimeRoot
    BS.writeFile modelFile "model fixture"
    BS.writeFile runtimeFile "runtime fixture"
    modelHash <- sha256File modelFile
    runtimeHash <- sha256File runtimeFile
    let modelArtifact = Artifact "nested/model.bin" 13 modelHash
        runtimeArtifact = Artifact "router.bin" 15 runtimeHash
    positiveModel <- validateArtifactTree "model fixture" modelRoot [modelArtifact]
    positiveRuntime <- validateArtifactTree "runtime fixture" runtimeRoot [runtimeArtifact]
    pass counter "positive full model/runtime fixture" (null (positiveModel <> positiveRuntime))

    missing <- validateArtifactTree "model fixture" modelRoot [Artifact "missing.bin" 0 (replicate 64 '0')]
    pass counter "missing artifact" (contains "absent model fixture" missing)
    BS.writeFile (modelRoot </> "extra.bin") "extra"
    extra <- validateArtifactTree "model fixture" modelRoot [modelArtifact]
    pass counter "extra artifact" (contains "unlisted model fixture" extra)
    removePathForcibly (modelRoot </> "extra.bin")
    wrongSize <- validateArtifactTree "model fixture" modelRoot [modelArtifact { artifactBytes = 1 }]
    pass counter "size mismatch" (contains "size drift" wrongSize)
    wrongHash <- validateArtifactTree "model fixture" modelRoot [modelArtifact { artifactHash = replicate 64 '0' }]
    pass counter "hash mismatch" (contains "checksum drift" wrongHash)
    removePathForcibly modelFile
    createDirectoryIfMissing True modelFile
    nonRegular <- validateArtifactTree "model fixture" modelRoot [modelArtifact]
    pass counter "non-regular artifact" (contains "non-regular" nonRegular)
    removePathForcibly modelRoot

    let target = temporary </> "target"
        linked = temporary </> "linked"
    createDirectoryIfMissing True target
    linkedResult <- createFixtureDirectoryLink target linked
    case linkedResult of
      Left _ -> putStrLn "self-test note: symlink root case unavailable on this host"
      Right () -> do
        symlinkErrors <- validateArtifactTree "linked fixture" linked []
        pass counter "symlink root" (contains "symlink" symlinkErrors)

  count <- readIORef counter
  putStrLn ("Managed embedding GPU provenance self-test passed (" <> show count <> " cases).")

pass :: IORef Int -> String -> Bool -> IO ()
pass counter label ok =
  if ok then modifyIORef' counter (+1) else die ["self-test failed: " <> label]

expectRejected :: IORef Int -> String -> String -> Value -> IO ()
expectRejected counter label needle value =
  case parseLock value of
    Left errors -> pass counter label (contains needle errors)
    Right _ -> die ["self-test did not reject " <> label]

setPath :: [Text] -> Value -> Value -> Value
setPath path replacement = updatePath path (const replacement)

insertPath :: [Text] -> Value -> Value -> Value
insertPath [] replacement _ = replacement
insertPath [key] replacement (Object object) =
  Object (KM.insert (Key.fromText key) replacement object)
insertPath (key:rest) replacement (Object object) =
  Object (adjustKey key (insertPath rest replacement) object)
insertPath _ _ value = value

updatePath :: [Text] -> (Value -> Value) -> Value -> Value
updatePath [] change value = change value
updatePath (key:rest) change (Object object) =
  Object (adjustKey key (updatePath rest change) object)
updatePath (indexText:rest) change (Array values) =
  case readMaybeInt indexText of
    Just index | index >= 0 && index < V.length values ->
      Array (values V.// [(index, updatePath rest change (values V.! index))])
    _ -> Array values
updatePath _ _ value = value

adjustKey :: Text -> (Value -> Value) -> Object -> Object
adjustKey key change object =
  case KM.lookup aesonKey object of
    Nothing -> object
    Just value -> KM.insert aesonKey (change value) object
  where aesonKey = Key.fromText key

modifyArrayPath :: [Text] -> (V.Vector Value -> V.Vector Value) -> Value -> Value
modifyArrayPath path change = updatePath path apply
  where apply (Array values) = Array (change values)
        apply value = value

dropLast :: V.Vector Value -> V.Vector Value
dropLast values | V.null values = values
                | otherwise = V.init values

duplicateFirst :: V.Vector Value -> V.Vector Value
duplicateFirst values | V.null values = values
                      | otherwise = V.snoc values (V.head values)

appendExtraArtifact :: V.Vector Value -> V.Vector Value
appendExtraArtifact values
  | V.null values = values
  | otherwise = V.snoc values (setPath ["path"] (String "extra.bin") (V.head values))

readMaybeInt :: Text -> Maybe Int
readMaybeInt value = case reads (T.unpack value) of
  [(number, "")] -> Just number
  _ -> Nothing

createFixtureDirectoryLink :: FilePath -> FilePath -> IO (Either SomeException ())
createFixtureDirectoryLink target link = do
  direct <- try (createDirectoryLink target link)
  case direct of
    Right () -> pure direct
    Left _ | os == "mingw32" -> try $ do
      let quoted path = "'" <> concatMap escape path <> "'"
          escape '\'' = "''"
          escape character = [character]
          command = "New-Item -ItemType Junction -Path " <> quoted link <> " -Target " <> quoted target <> " -ErrorAction Stop | Out-Null"
      (status, output, errors) <- readProcessWithExitCode "powershell.exe" ["-NoProfile", "-NonInteractive", "-Command", command] ""
      unless (status == ExitSuccess) (ioError (userError (output <> errors)))
    Left _ -> pure direct

contains :: String -> [String] -> Bool
contains needle = any (containsText needle)

containsText :: String -> String -> Bool
containsText needle haystack
  | null needle = True
  | length haystack < length needle = False
  | take (length needle) haystack == needle = True
  | otherwise = containsText needle (drop 1 haystack)

die :: [String] -> IO a
die errors = do
  putStrLn "Managed embedding GPU provenance check failed:"
  forM_ errors (putStrLn . ("  - " <>))
  exitFailure
