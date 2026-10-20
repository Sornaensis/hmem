module HMem.Server.ConfiguredObservationDiscoverySpec (spec) where

import Control.Exception (Exception, IOException, SomeAsyncException, SomeException, fromException, mask, throwIO, try)
import Control.Monad (unless)
import Crypto.Hash (Digest, SHA256, hash)
import Data.Aeson (Value(..), decode, encode, object, (.=))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString qualified as BS
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.UUID qualified as UUID
import Data.Yaml qualified as Yaml
import Network.HTTP.Types (HeaderName, methodGet, methodPost, parseQuery, status200, status401, status403)
import Network.Wai qualified as Wai
import Network.Wai.Test (SRequest(..), SResponse(..), runSession, srequest)
import System.Directory (doesFileExist, getFileSize)
import System.Environment (lookupEnv)
import System.IO (IOMode(ReadMode), withBinaryFile)
import System.Timeout (timeout)
import Test.Hspec

import HMem.Config qualified as Config
import HMem.DB.Auth qualified as Auth
import HMem.DB.Embedding qualified as Embedding
import HMem.DB.Observation qualified as Observation
import HMem.DB.TestHarness (TestDb(..), TestEnv(..), createTestWorkspace, withTestEnv)
import HMem.Embedding.HttpProcess (SessionPolicy(..))
import HMem.Server.AccessTracker (newAccessTracker)
import HMem.Server.App (mkAppWithDiscovery)
import HMem.Server.AuthTokens (IssuedAccessToken(..))
import HMem.Server.Embedding.GteQwen2 (embedGteQwen2)
import HMem.Server.Embedding.Http (makeValidatedGpuEmbeddingProviderWithInvalidation)
import HMem.Server.Embedding.HttpSpec (testHelperExecutable)
import HMem.Server.Embedding.Provider
import HMem.Server.ObservationDiscovery
import HMem.Server.TestHarness (mkDeployedSandboxConfig, createDeployedSandboxUser, issueDeployedSandboxPAT)
import HMem.Server.WebSocket (newWSState)
import HMem.Types

-- Captured before SpecHook redirects HOME and scrubs test DB variables. No
-- operator file is read during discovery, and nothing is captured without opt-in.
data QualificationInputs = QualificationInputs FilePath (Maybe Text) (Maybe Text) Bool
data QualificationFailure = QualificationFailure String deriving stock (Show)
instance Exception QualificationFailure

spec :: Spec
spec = do
  enabled <- runIO ((== Just "1") <$> lookupEnv "HMEM_TEST_CONFIGURED_DISCOVERY")
  if not enabled
    then it "ConfiguredDiscovery is explicitly opt-in" $
      pendingWith "set HMEM_TEST_CONFIGURED_DISCOVERY=1 to qualify the existing HTTP GPU provider"
    else do
      inputs <- runIO $ do
        override <- lookupEnv "HMEM_TEST_DISCOVERY_CONFIG"
        path <- maybe Config.configFilePath pure override
        mode <- fmap T.pack <$> lookupEnv "HMEM_EMBEDDING_PROVIDER"
        endpoint <- fmap T.pack <$> lookupEnv "HMEM_EMBEDDING_ENDPOINT"
        external <- lookupEnv "HMEM_TEST_EXTERNAL_DB"
        allowExternal <- lookupEnv "HMEM_TEST_ALLOW_EXTERNAL_DB"
        pure (QualificationInputs path mode endpoint (external /= Nothing || allowExternal == Just "1"))
      it "ConfiguredDiscovery qualifies the configured GPU through authorized WAI discovery" $ do
        phase <- newIORef "configuration"
        attempted <- try @SomeException $ timeout 180000000 $ do
          providerConfig <- readProviderConfig inputs
          writeIORef phase "ephemeral_fixture"
          withTestEnv $ \env -> do
            ensure (not env.testDb.testDbUnsafeExternal) "external_database_refused"
            helper <- testHelperExecutable
            helperSize <- getFileSize helper
            ensure (helperSize <= 64 * 1024 * 1024) "helper_asset_too_large"
            helperBytes <- BS.readFile helper
            putStrLn ("ConfiguredDiscovery helper_sha256=" <> show (hash helperBytes :: Digest SHA256))
            let policy = SessionPolicy helper (const (pure ())) (const (pure ()))
            writeIORef phase "provider_validation"
            made <- makeValidatedGpuEmbeddingProviderWithInvalidation policy providerConfig Nothing
            case made of
              Left failure -> reject ("provider_" <> show failure.errorCode)
              Right (provider,invalidate) -> do
                calls <- newIORef (0 :: Int,0 :: Int)
                let counted = provider
                      { availability = modifyIORef' calls (\(a,e) -> (a+1,e)) >> provider.availability
                      , embed = \request -> modifyIORef' calls (\(a,e) -> (a,e+1)) >> provider.embed request }
                withQualifiedProvider counted invalidate $ \service -> do
                  provider.availability >>= \case
                    EmbeddingAvailable -> pure ()
                    EmbeddingDisabled -> reject "provider_disabled"
                    EmbeddingUnavailable failure -> reject ("provider_" <> show failure.errorCode)
                  putStrLn "ConfiguredDiscovery validation=pinned_golden_pass"
                  writeIORef phase "authorized_discovery"
                  qualifyRequests env provider service calls
        case attempted of
          Right (Just ()) -> pure ()
          Right Nothing -> expectationFailure "ConfiguredDiscovery reason=total_deadline_exceeded"
          Left failure
            | Just (_ :: SomeAsyncException) <- fromException failure -> throwIO failure
            | Just (QualificationFailure reason) <- fromException failure ->
                expectationFailure ("ConfiguredDiscovery reason=" <> reason)
            | otherwise -> readIORef phase >>= \stage ->
                expectationFailure ("ConfiguredDiscovery reason=unexpected_failure phase=" <> stage)

readProviderConfig :: QualificationInputs -> IO Config.EmbeddingProviderConfig
readProviderConfig (QualificationInputs path modeOverride endpointOverride externalRequested) = do
  ensure (not externalRequested) "external_database_refused"
  exists <- doesFileExist path
  ensure exists "configuration_missing"
  readResult <- try @IOException $ withBinaryFile path ReadMode (\handle -> BS.hGet handle (1024 * 1024 + 1))
  bytes <- either (const (reject "configuration_unreadable")) pure readResult
  ensure (BS.length bytes <= 1024 * 1024) "configuration_too_large"
  cfg <- either (const (reject "configuration_invalid")) pure (Yaml.decodeEither' bytes :: Either Yaml.ParseException Config.HMemConfig)
  overridden <- either (const (reject "provider_override_invalid")) pure $
    Config.applyEmbeddingProviderEnvOverrides modeOverride endpointOverride cfg
  let provider = overridden.embeddingProvider
      (_,validated) = Config.validateConfig overridden
  ensure (provider == validated.embeddingProvider) "provider_configuration_incompatible"
  ensure (provider.mode == Config.EmbeddingProviderHttp) "provider_http_mode_required"
  ensure (provider.gpuProfile == Just Config.managedTeiGpuProfile
       && provider.spaceFingerprint == Config.managedTeiSpaceFingerprint) "provider_profile_incompatible"
  ensure (maybe False (const True) provider.endpoint) "provider_endpoint_missing"
  pure provider

withQualifiedProvider :: EmbeddingProvider -> IO () -> (ObservationDiscovery -> IO ()) -> IO ()
withQualifiedProvider provider invalidate action = mask $ \restore -> do
  service <- newObservationDiscovery True
  withdraw <- publishDiscoveryProvider service provider
  result <- try @SomeException (restore (action service))
  cleaned <- try @SomeException (timeout 2000000 (withdraw >> invalidate))
  let cleanupPassed = case cleaned of Right (Just ()) -> True; _ -> False
  putStrLn ("ConfiguredDiscovery cleanup=" <> if cleanupPassed then "complete" else "failed")
  case result of
    Left failure -> throwIO failure
    Right () -> ensure cleanupPassed "cleanup_failed"

qualifyRequests :: TestEnv -> EmbeddingProvider -> ObservationDiscovery -> IORef (Int,Int) -> IO ()
qualifyRequests env provider service calls = do
  let query = "observation discovery qualification"
  workspace <- createTestWorkspace env "configured-discovery-qualification"
  row <- Observation.createObservation env.pool
    (CreateObservation workspace.id [ObservationSubject SubjectFile "qualification/fixture.hs"] (T.replicate 40 "a") query)
  seeded <- embedGteQwen2 provider (EmbeddingRequest [EmbeddingInput EmbeddingDocument query])
  vector <- case seeded of
    Right batch | [value] <- batch.vectors -> pure value
    Left _ -> reject "fixture_embedding_unavailable"
    _ -> reject "fixture_embedding_invalid"
  space <- maybe (reject "fixture_space_invalid") pure (parseEmbeddingSpaceFingerprint Config.managedTeiSpaceFingerprint)
  Embedding.setObservationEmbeddingInSpace env.pool workspace.id row.id space vector
  -- Count request-driven calls separately from the explicit validation/fixture
  -- producer calls. Wrapping preserves the exact same validated provider gate.
  result <- try @SomeException $ do
    tracker <- newAccessTracker env.pool 3600
    wsState <- newWSState
    let cfg = mkDeployedSandboxConfig env
    app <- mkAppWithDiscovery service cfg.changeStream id cfg.auth cfg.cors cfg.rateLimit env.pool tracker wsState Nothing True
    reader <- createDeployedSandboxUser env False False
    stranger <- createDeployedSandboxUser env False False
    _ <- Auth.upsertWorkspaceMembership env.pool workspace.id (Auth.UpsertWorkspaceMembership reader Auth.WorkspaceRoleRead) Nothing
    readerToken <- issueDeployedSandboxPAT env reader "qualification-reader"
    strangerToken <- issueDeployedSandboxPAT env stranger "qualification-stranger"
    let capabilityPath = "/api/v1/observation-discovery/capability?workspace_id=" <> TE.encodeUtf8 (UUID.toText workspace.id)
        discoveryPath = "/api/v1/observations/discovery"
        create = object ["workspace_id" .= workspace.id,"filters" .= object ["content_query" .= query],"search_session" .= UUID.nil,"generation" .= (1 :: Int)]
        headers token = [("Authorization","Bearer " <> TE.encodeUtf8 token.rawToken)
          ,("X-HMem-Bot-Identity","{\"role\":\"qualification\",\"model-id\":\"test:configured:gpu\"}")]
    anonymous <- send app methodPost discoveryPath [] create
    ensure (anonymous.simpleStatus == status401) "anonymous_authorization_failed"
    denied <- send app methodPost discoveryPath (headers strangerToken) create
    ensure (denied.simpleStatus == status403) "workspace_authorization_failed"
    deniedCapability <- send app methodGet capabilityPath (headers strangerToken) Null
    ensure (deniedCapability.simpleStatus == status403) "capability_authorization_failed"
    beforeRequests <- readIORef calls
    ensure (beforeRequests == (0,0)) "unauthorized_provider_invocation"
    capability <- send app methodGet capabilityPath (headers readerToken) Null
    ensure (capability.simpleStatus == status200 && field ["mode"] capability == Just (String "hybrid"))
      ("capability_not_hybrid_" <> safeReason ["reason"] capability)
    created <- send app methodPost discoveryPath (headers readerToken) create
    ensure (created.simpleStatus == status200 && field ["retrieval","mode"] created == Just (String "hybrid"))
      ("query_not_hybrid_" <> safeReason ["retrieval","reason"] created)
    token <- maybe (reject "context_token_missing") pure (field ["context_token"] created)
    produced <- readIORef calls
    ensure (snd produced == 1) "query_producer_not_invoked"
    continued <- send app methodPost discoveryPath (headers readerToken)
      (object ["workspace_id" .= workspace.id,"context_token" .= token,"view" .= ("count" :: Text)])
    ensure (continued.simpleStatus == status200 && field ["retrieval","mode"] continued == Just (String "hybrid")
         && field ["page","counts","match_count"] continued == Just (Number 1)) "context_continuation_failed"
    afterRequests <- readIORef calls
    ensure (afterRequests == produced) "context_repeated_provider_work"
    putStrLn "ConfiguredDiscovery mode=hybrid reason=none assertions=authorization,query,continuation"
  either throwIO pure result

send :: Wai.Application -> BS.ByteString -> BS.ByteString -> [(HeaderName,BS.ByteString)] -> Value -> IO SResponse
send app method path headers body = runSession (srequest (SRequest request (if method == methodGet then "" else encode body))) app
  where
    (rawPath,rawQuery) = BS.break (== 63) path
    request = Wai.defaultRequest
      { Wai.requestMethod = method, Wai.rawPathInfo = rawPath, Wai.rawQueryString = rawQuery
      , Wai.pathInfo = filter (not . T.null) (T.splitOn "/" (TE.decodeUtf8 rawPath)), Wai.queryString = parseQuery rawQuery
      , Wai.requestHeaders = ("Content-Type","application/json") : headers }

field :: [Text] -> SResponse -> Maybe Value
field keys response = decode response.simpleBody >>= descend keys
  where
    descend [] value = Just value
    descend (key:rest) (Object values) = KM.lookup (Key.fromText key) values >>= descend rest
    descend _ _ = Nothing

ensure :: Bool -> String -> IO ()
ensure condition reason = unless condition (reject reason)

reject :: String -> IO a
reject = throwIO . QualificationFailure

safeReason :: [Text] -> SResponse -> String
safeReason keys response = case field keys response of
  Just (String reason) | reason `elem` ["disabled","provider_unavailable","storage_unavailable","no_compatible_vectors","timed_out","invalid_response","retired","input_too_long","empty_query"] -> T.unpack reason
  _ -> "unavailable"
