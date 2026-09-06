module HMem.ConfigSpec (spec) where

import Control.Exception (bracket, try)
import Data.ByteString.Char8 qualified as BS8
import Data.Either (isLeft)
import Data.List (isInfixOf)
import Data.Yaml qualified as Yaml
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import HMem.Config

spec :: Spec
spec = do
  describe "applyEnvOverrides" $ do
    it "uses HMEM_DB_PASSWORD when present" $ do
      let cfg = defaultConfig
            { database = defaultConfig.database { password = Just "from-config" }
            }
          overridden = applyEnvOverrides (Just "from-env") Nothing Nothing cfg
      overridden.database.password `shouldBe` Just "from-env"

    it "keeps config password when env var is absent" $ do
      let cfg = defaultConfig
            { database = defaultConfig.database { password = Just "from-config" }
            }
          overridden = applyEnvOverrides Nothing Nothing Nothing cfg
      overridden.database.password `shouldBe` Just "from-config"

    it "leaves password unset when neither source provides one" $ do
      let overridden = applyEnvOverrides Nothing Nothing Nothing defaultConfig
      overridden.database.password `shouldBe` Nothing

    it "uses HMEM_API_KEY when present" $ do
      let cfg = defaultConfig
            { auth = defaultConfig.auth { enabled = True, apiKey = Just "from-config" } }
          overridden = applyEnvOverrides Nothing (Just "from-env") Nothing cfg
      overridden.auth.apiKey `shouldBe` Just "from-env"

    it "applies change-stream environment overrides before validation" $ do
      let overridden = applyChangeStreamEnvOverrides (Just "100") (Just "40") (Just "20") defaultConfig
      overridden.changeStream `shouldBe` ChangeStreamConfig 100 40 20

  describe "validateConfig" $ do
    it "defaults change-stream retention and hand-off lifetimes" $ do
      defaultConfig.changeStream `shouldBe` ChangeStreamConfig
        { retentionSeconds = 604800
        , resumeTokenTtlSeconds = 86400
        , snapshotSessionTtlSeconds = 300
        }

    it "keeps the change-stream TTL budget below retention" $ do
      let cfg = defaultConfig
            { changeStream = ChangeStreamConfig
                { retentionSeconds = 100
                , resumeTokenTtlSeconds = 90
                , snapshotSessionTtlSeconds = 20
                }
            }
          (warnings, validated) = validateConfig cfg
      warnings `shouldSatisfy` any ("exceeds maximum" `isInfixOf`)
      validated.changeStream.resumeTokenTtlSeconds
        + validated.changeStream.snapshotSessionTtlSeconds
        `shouldSatisfy` (< validated.changeStream.retentionSeconds)

    it "constructs a valid change-stream budget at the retention boundary" $ do
      let cfg = defaultConfig { changeStream = ChangeStreamConfig 1 999 999 }
          (_, validated) = validateConfig cfg
          stream = validated.changeStream
      stream.retentionSeconds `shouldBe` 3
      stream.resumeTokenTtlSeconds `shouldBe` 1
      stream.snapshotSessionTtlSeconds `shouldBe` 1
      stream.resumeTokenTtlSeconds + stream.snapshotSessionTtlSeconds
        `shouldSatisfy` (< stream.retentionSeconds)

    it "warns when legacy static bearer auth is enabled without an API key" $ do
      let cfg = defaultConfig { auth = defaultConfig.auth { enabled = True, apiKey = Nothing } }
          (warnings, validated) = validateConfig cfg
      warnings `shouldSatisfy` (not . null)
      validated.auth.enabled `shouldBe` True
      authStaticBearerEnabled validated.auth `shouldBe` False

    it "allows deployed mode config without disabling auth.enabled" $ do
      let cfg = defaultConfig
            { auth = defaultConfig.auth
                { mode = AuthModeDeployed
                , enabled = True
                , apiKey = Nothing
                , deployed = defaultConfig.auth.deployed { issuer = Just "https://issuer.example" }
                }
            }
          (_, validated) = validateConfig cfg
      validated.auth.enabled `shouldBe` True
      validated.auth.mode `shouldBe` AuthModeDeployed
      authStaticBearerEnabled validated.auth `shouldBe` False

    it "does not emit the legacy api_key warning in deployed mode" $ do
      let cfg = defaultConfig
            { auth = defaultConfig.auth
                { mode = AuthModeDeployed
                , enabled = True
                , apiKey = Nothing
                , deployed = defaultConfig.auth.deployed { issuer = Just "https://issuer.example" }
                }
            }
          (warnings, _) = validateConfig cfg
      warnings `shouldSatisfy` all (/= "auth.enabled is true but no auth.api_key or HMEM_API_KEY is configured; current runtime will not enable legacy static bearer auth until richer mode-specific auth is implemented")

    it "does not activate the legacy static bearer path in deployed mode even when api_key is present" $ do
      let cfg = defaultConfig
            { auth = defaultConfig.auth
                { mode = AuthModeDeployed
                , enabled = True
                , apiKey = Just "legacy-secret"
                , deployed = defaultConfig.auth.deployed { issuer = Just "https://issuer.example" }
                }
            }
          (_, validated) = validateConfig cfg
      authStaticBearerEnabled validated.auth `shouldBe` False

    it "warns when implicit local superadmin is configured on a non-loopback host without the escape hatch" $ do
      let remoteServer = ServerConfig { port = defaultConfig.server.port, host = "0.0.0.0" }
          cfg = defaultConfig
            { server = remoteServer
            , auth = defaultConfig.auth
                { mode = AuthModeLocal
                , local = defaultConfig.auth.local { bootstrapEnabled = True, allowRemoteBootstrap = False }
                }
            }
          (warnings, _) = validateConfig cfg
      warnings `shouldSatisfy` any ("loopback/CORS-local only by default" `isInfixOf`)
      localImplicitBootstrapActive cfg `shouldBe` True
      localImplicitBootstrapAllowed cfg `shouldBe` False
      localImplicitBootstrapExposesRemote cfg `shouldBe` True

    it "warns and disallows implicit local superadmin with permissive CORS" $ do
      let cfg = defaultConfig { cors = CorsConfig { allowedOrigins = ["*"] } }
          (warnings, _) = validateConfig cfg
      warnings `shouldSatisfy` any ("cors.allowed_origins permits remote origins" `isInfixOf`)
      corsAllowsRemoteOrigins cfg.cors `shouldBe` True
      localImplicitBootstrapAllowed cfg `shouldBe` False
      localImplicitBootstrapStartupError cfg `shouldSatisfy` (/= Nothing)

    it "allows but strongly warns for explicit remote local bootstrap" $ do
      let remoteServer = ServerConfig { port = defaultConfig.server.port, host = "0.0.0.0" }
          cfg = defaultConfig
            { server = remoteServer
            , auth = defaultConfig.auth
                { mode = AuthModeLocal
                , local = defaultConfig.auth.local { bootstrapEnabled = True, allowRemoteBootstrap = True }
                }
            }
          (warnings, _) = validateConfig cfg
      warnings `shouldSatisfy` any ("allow_remote_bootstrap is true" `isInfixOf`)
      localImplicitBootstrapAllowed cfg `shouldBe` True
      localImplicitBootstrapExposesRemote cfg `shouldBe` True
      localImplicitBootstrapStartupError cfg `shouldBe` Nothing

    it "allows explicit remote local bootstrap even with permissive CORS" $ do
      let remoteServer = ServerConfig { port = defaultConfig.server.port, host = "0.0.0.0" }
          cfg = defaultConfig
            { server = remoteServer
            , cors = CorsConfig { allowedOrigins = ["*"] }
            , auth = defaultConfig.auth
                { mode = AuthModeLocal
                , local = defaultConfig.auth.local { bootstrapEnabled = True, allowRemoteBootstrap = True }
                }
            }
          (warnings, _) = validateConfig cfg
      warnings `shouldSatisfy` any ("server.host is '0.0.0.0'" `isInfixOf`)
      warnings `shouldSatisfy` any ("cors.allowed_origins permits remote origins" `isInfixOf`)
      localImplicitBootstrapAllowed cfg `shouldBe` True
      localImplicitBootstrapStartupError cfg `shouldBe` Nothing

    it "does not apply local exposure guard when local bootstrap is disabled" $ do
      let remoteServer = ServerConfig { port = defaultConfig.server.port, host = "0.0.0.0" }
          cfg = defaultConfig
            { server = remoteServer
            , cors = CorsConfig { allowedOrigins = ["*"] }
            , auth = defaultConfig.auth
                { mode = AuthModeLocal
                , local = defaultConfig.auth.local { bootstrapEnabled = False, allowRemoteBootstrap = False }
                }
            }
          (warnings, _) = validateConfig cfg
      warnings `shouldSatisfy` all (not . ("implicit local superadmin" `isInfixOf`))
      localImplicitBootstrapActive cfg `shouldBe` False
      localImplicitBootstrapAllowed cfg `shouldBe` True
      localImplicitBootstrapStartupError cfg `shouldBe` Nothing

    it "does not apply local exposure guard in deployed mode" $ do
      let remoteServer = ServerConfig { port = defaultConfig.server.port, host = "0.0.0.0" }
          cfg = defaultConfig
            { server = remoteServer
            , cors = CorsConfig { allowedOrigins = ["*"] }
            , auth = defaultConfig.auth
                { mode = AuthModeDeployed
                , local = defaultConfig.auth.local { bootstrapEnabled = True, allowRemoteBootstrap = False }
                }
            }
          (warnings, _) = validateConfig cfg
      warnings `shouldSatisfy` all (not . ("implicit local superadmin" `isInfixOf`))
      localImplicitBootstrapActive cfg `shouldBe` False
      localImplicitBootstrapAllowed cfg `shouldBe` True
      localImplicitBootstrapExposesRemote cfg `shouldBe` False
      localImplicitBootstrapStartupError cfg `shouldBe` Nothing

    it "allows implicit local superadmin on loopback by default" $ do
      let cfg = defaultConfig
      localImplicitBootstrapActive cfg `shouldBe` True
      localImplicitBootstrapAllowed cfg `shouldBe` True
      localImplicitBootstrapExposesRemote cfg `shouldBe` False

    it "classifies only concrete loopback hosts as loopback" $ do
      serverHostIsLoopback "localhost" `shouldBe` True
      serverHostIsLoopback "127.0.0.1" `shouldBe` True
      serverHostIsLoopback "127.12.34.56" `shouldBe` True
      serverHostIsLoopback "::1" `shouldBe` True
      serverHostIsLoopback "[::1]" `shouldBe` True
      serverHostIsLoopback "LOCALHOST" `shouldBe` True
      serverHostIsLoopback "0.0.0.0" `shouldBe` False
      serverHostIsLoopback "::" `shouldBe` False
      serverHostIsLoopback "127.example.com" `shouldBe` False
      serverHostIsLoopback "127.0.0.999" `shouldBe` False

    it "classifies concrete remote CORS origins as remote" $ do
      corsAllowsRemoteOrigins (CorsConfig { allowedOrigins = ["http://localhost:3000", "http://127.0.0.1"] }) `shouldBe` False
      corsAllowsRemoteOrigins (CorsConfig { allowedOrigins = ["*"] }) `shouldBe` True
      corsAllowsRemoteOrigins (CorsConfig { allowedOrigins = ["https://example.com"] }) `shouldBe` True
      corsAllowsRemoteOrigins (CorsConfig { allowedOrigins = ["https://localhost.evil.example"] }) `shouldBe` True
      corsAllowsRemoteOrigins (CorsConfig { allowedOrigins = ["http://[::1]:8420"] }) `shouldBe` False

  describe "default auth schema" $ do
    it "defaults to local auth mode with local bootstrap enabled" $ do
      defaultConfig.auth.mode `shouldBe` AuthModeLocal
      defaultConfig.auth.local.bootstrapEnabled `shouldBe` True
      defaultConfig.auth.local.allowRemoteBootstrap `shouldBe` False

    it "defaults deployed token lookup to database" $ do
      defaultConfig.auth.deployed.tokenLookup `shouldBe` TokenLookupDatabase

  describe "change-stream schema" $ do
    it "parses configured retention and hand-off lifetimes from YAML" $ do
      let yaml = BS8.pack $ unlines
            [ "change_stream:"
            , "  retention_seconds: 120"
            , "  resume_token_ttl_seconds: 60"
            , "  snapshot_session_ttl_seconds: 30"
            ]
      case Yaml.decodeEither' yaml of
        Left err -> expectationFailure (show err)
        Right (cfg :: HMemConfig) -> cfg.changeStream `shouldBe` ChangeStreamConfig 120 60 30

  describe "embedding provider config" $ do
    it "defaults to a disabled provider without an endpoint" $ do
      managedTeiSpaceFingerprint `shouldBe`
        "Alibaba-NLP/gte-Qwen2-1.5B-instruct@1cad2ab3ff41c2671f34e135d29831368ee26b68:1536:attention=noncausal:v1"
      defaultConfig.embeddingProvider `shouldBe` EmbeddingProviderConfig
        { mode = EmbeddingProviderDisabled
        , endpoint = Nothing
        , batchSize = 32
        , timeoutMs = 30000
        , retryAttempts = 0
        , spaceFingerprint = managedTeiSpaceFingerprint
        }

    it "round-trips an operator-managed HTTP TEI endpoint through YAML" $ do
      let cfg = defaultConfig
            { embeddingProvider = EmbeddingProviderConfig
                { mode = EmbeddingProviderHttp
                , endpoint = Just "https://gpu-embeddings.example"
                , batchSize = 64
                , timeoutMs = 12000
                , retryAttempts = 2
                , spaceFingerprint = managedTeiSpaceFingerprint
                }
            }
      case Yaml.decodeEither' (Yaml.encode cfg) of
        Left err -> expectationFailure (show err)
        Right (decoded :: HMemConfig) -> do
          decoded `shouldBe` cfg
          decoded.embeddingProvider.spaceFingerprint `shouldBe` managedTeiSpaceFingerprint

    it "rejects the historical unqualified provider identity without rewriting it" $ do
      let historicalFingerprint =
            "Alibaba-NLP/gte-Qwen2-1.5B-instruct@1cad2ab3ff41c2671f34e135d29831368ee26b68:1536"
          yaml = BS8.pack $ unlines
            [ "embedding:"
            , "  mode: managed-tei"
            , "  space_fingerprint: " <> historicalFingerprint
            ]
      case Yaml.decodeEither' yaml :: Either Yaml.ParseException HMemConfig of
        Right _ -> expectationFailure "historical unqualified provider fingerprint was accepted"
        Left err -> do
          show err `shouldSatisfy` isInfixOf "historical GTE-Qwen2 space with unknown attention semantics"
          show err `shouldSatisfy` isInfixOf "attention=noncausal:v1"

    it "fails the production load path for historical managed and HTTP identities before environment overrides" $
      withSystemTempDirectory "hmem-config-spec" $ \dir -> do
        let historicalFingerprint =
              "Alibaba-NLP/gte-Qwen2-1.5B-instruct@1cad2ab3ff41c2671f34e135d29831368ee26b68:1536"
            cases =
              [ ( "managed-to-http.yaml"
                , [ "embedding:"
                  , "  mode: managed-tei"
                  , "  space_fingerprint: " <> historicalFingerprint
                  ]
                , Just "http"
                , Just "https://env-embeddings.example"
                )
              , ( "http-to-managed.yaml"
                , [ "embedding:"
                  , "  mode: http"
                  , "  endpoint: https://file-embeddings.example"
                  , "  space_fingerprint: " <> historicalFingerprint
                  ]
                , Just "managed-tei"
                , Nothing
                )
              ]
        mapM_ (assertHistoricalLoadRejected dir) cases

    it "rejects unsupported model spaces and invalid provider combinations" $ do
      let invalids = BS8.pack . unlines <$>
            [ [ "embedding:"
              , "  mode: http"
              , "  endpoint: https://embeddings.example"
              , "  space_fingerprint: arbitrary-model:768"
              ]
            , [ "embedding:"
              , "  mode: disabled"
              , "  endpoint: https://embeddings.example"
              ]
            , [ "embedding:"
              , "  mode: managed-tei"
              , "  endpoint: http://127.0.0.1:8080"
              ]
            , [ "embedding:"
              , "  mode: http"
              , "  endpoint: https://user:secret@embeddings.example"
              ]
            , [ "embedding:"
              , "  mode: http"
              , "  endpoint: http://:8080"
              ]
            , [ "embedding:"
              , "  mode: http"
              , "  endpoint: https://embeddings.example/admin"
              ]
            ]
      mapM_ (\yaml -> (Yaml.decodeEither' yaml :: Either Yaml.ParseException HMemConfig) `shouldSatisfy` isLeft) invalids

    it "parses endpoint authorities structurally" $ do
      parseEmbeddingEndpointAuthority "https://gpu-embeddings.example:8443/base"
        `shouldBe` Just (EmbeddingEndpointAuthority "https" "gpu-embeddings.example" "/base")
      mapM_ (\endpointValue -> parseEmbeddingEndpointAuthority endpointValue `shouldSatisfy` (/= Nothing))
        [ "http://127.0.0.1:8080"
        , "http://[::1]:8080"
        , "https://gpu-embeddings.example"
        , "https://a-b.example-2.test/embed"
        ]
      mapM_ (\endpointValue -> parseEmbeddingEndpointAuthority endpointValue `shouldBe` Nothing)
        [ "http://:8080"
        , "http://user@127.0.0.1:8080"
        , "http://[::1:8080"
        , "http://[not-an-ip]:8080"
        , "http://[::1::]:8080"
        , "http://127.0.0.999:8080"
        , "http://127.0.0:8080"
        , "http://-bad.example:8080"
        , "http://bad-.example:8080"
        , "http://host:99999"
        , "http://host?query=value"
        ]

    it "applies valid environment provider overrides and rejects invalid ones" $ do
      let httpConfig = defaultConfig
            { embeddingProvider = defaultConfig.embeddingProvider
                { mode = EmbeddingProviderHttp
                , endpoint = Just "https://config.example"
                }
            }
      applyEmbeddingProviderEnvOverrides (Just "http") (Just "https://gpu.example") defaultConfig
        `shouldBe` Right (defaultConfig
          { embeddingProvider = defaultConfig.embeddingProvider
              { mode = EmbeddingProviderHttp, endpoint = Just "https://gpu.example" }
          })
      applyEmbeddingProviderEnvOverrides (Just "unknown") Nothing httpConfig
        `shouldSatisfy` isLeft
      let disabledProvider :: EmbeddingProviderConfig
          disabledProvider = defaultConfig.embeddingProvider { mode = EmbeddingProviderDisabled }
          managedProvider :: EmbeddingProviderConfig
          managedProvider = defaultConfig.embeddingProvider { mode = EmbeddingProviderManagedTei }
      applyEmbeddingProviderEnvOverrides (Just "disabled") Nothing httpConfig
        `shouldBe` Right (defaultConfig
          { embeddingProvider = disabledProvider
          })
      applyEmbeddingProviderEnvOverrides (Just "managed-tei") Nothing httpConfig
        `shouldBe` Right (defaultConfig
          { embeddingProvider = managedProvider
          })
      applyEmbeddingProviderEnvOverrides (Just "disabled") (Just "https://override.example") httpConfig
        `shouldSatisfy` isLeft
      applyEmbeddingProviderEnvOverrides (Just "managed-tei") (Just "https://override.example") httpConfig
        `shouldSatisfy` isLeft

    it "redacts a configured endpoint from Show output" $ do
      let provider = defaultConfig.embeddingProvider
            { mode = EmbeddingProviderHttp
            , endpoint = Just "https://gpu.example"
            }
      show provider `shouldNotSatisfy` isInfixOf "gpu.example"

  describe "auth config parsing" $ do
    it "normalizes blank MCP provenance secrets from YAML and environment" $ do
      let yaml = BS8.pack $ unlines
            [ "auth:"
            , "  mcp_provenance_token: '   '"
            ]
      case Yaml.decodeEither' yaml of
        Left err -> expectationFailure (show err)
        Right (cfg :: HMemConfig) -> authMcpProvenanceToken cfg.auth `shouldBe` Nothing
      let configured = defaultConfig { auth = defaultConfig.auth { mcpProvenanceToken = Just "config-secret" } }
      authMcpProvenanceToken (applyMcpProvenanceEnvOverride (Just " \t ") configured).auth
        `shouldBe` Just "config-secret"
      authMcpProvenanceToken (applyMcpProvenanceEnvOverride (Just "  env-secret  ") defaultConfig).auth
        `shouldBe` Just "env-secret"

    it "parses a legacy auth config with enabled/api_key only" $ do
      let yaml = BS8.pack $ unlines
            [ "auth:"
            , "  enabled: true"
            , "  api_key: legacy-secret"
            ]
      case Yaml.decodeEither' yaml of
        Left err -> expectationFailure (show err)
        Right (cfg :: HMemConfig) -> do
          cfg.auth.mode `shouldBe` AuthModeLocal
          cfg.auth.enabled `shouldBe` True
          cfg.auth.apiKey `shouldBe` Just "legacy-secret"
          authStaticBearerEnabled cfg.auth `shouldBe` True

    it "parses a new-style deployed auth config" $ do
      let yaml = BS8.pack $ unlines
            [ "auth:"
            , "  mode: deployed"
            , "  enabled: true"
            , "  deployed:"
            , "    issuer: https://issuer.example"
            , "    audience: hmem-web"
            , "    token_lookup: database"
            , "    token_hash_secret: hmac-secret"
            ]
      case Yaml.decodeEither' yaml of
        Left err -> expectationFailure (show err)
        Right (cfg :: HMemConfig) -> do
          cfg.auth.mode `shouldBe` AuthModeDeployed
          cfg.auth.enabled `shouldBe` True
          cfg.auth.deployed.issuer `shouldBe` Just "https://issuer.example"
          cfg.auth.deployed.audience `shouldBe` Just "hmem-web"
          cfg.auth.deployed.tokenLookup `shouldBe` TokenLookupDatabase
          cfg.auth.deployed.tokenHashSecret `shouldBe` Just "hmac-secret"
          authStaticBearerEnabled cfg.auth `shouldBe` False

    it "round-trips the richer auth schema through yaml encoding" $ do
      let cfg = defaultConfig
            { auth = defaultConfig.auth
                { mode = AuthModeDeployed
                , enabled = True
                , local = LocalAuthConfig
                    { bootstrapEnabled = False
                    , allowRemoteBootstrap = True
                    , botTokens = [ LocalBotTokenConfig { label = "codex", token = "secret-token" } ]
                    }
                , deployed = DeployedAuthConfig
                    { issuer = Just "https://issuer.example"
                    , audience = Just "hmem-web"
                    , discoveryUrl = Nothing
                    , jwksUrl = Just "https://issuer.example/.well-known/jwks.json"
                    , jwks = Nothing
                    , tokenLookup = TokenLookupDatabase
                    , tokenHashSecret = Just "hash-secret"
                    , clientId = Just "hmem-web"
                    , clientSecret = Just "client-secret"
                    , redirectUri = Just "https://hmem.example/api/v1/auth/callback"
                    , scopes = ["openid", "profile", "email"]
                    , authorizationEndpoint = Just "https://issuer.example/protocol/openid-connect/auth"
                    , tokenEndpoint = Just "https://issuer.example/protocol/openid-connect/token"
                    , sessionCookieName = "hmem_session"
                    , csrfCookieName = "hmem_csrf"
                    , csrfHeaderName = "X-CSRF-Token"
                    , sessionTtlSeconds = 28800
                    , cookieSecure = True
                    , cookieSameSite = "Lax"
                    }
                }
            }
      case Yaml.decodeEither' (Yaml.encode cfg) of
        Left err -> expectationFailure (show err)
        Right (decoded :: HMemConfig) -> decoded `shouldBe` cfg

assertHistoricalLoadRejected
  :: FilePath
  -> (FilePath, [String], Maybe String, Maybe String)
  -> IO ()
assertHistoricalLoadRejected dir (filename, yamlLines, envMode, envEndpoint) = do
  let path = dir </> filename
  BS8.writeFile path (BS8.pack (unlines yamlLines))
  result <- withEnvVar "HMEM_EMBEDDING_PROVIDER" envMode
    $ withEnvVar "HMEM_EMBEDDING_ENDPOINT" envEndpoint
    $ try (loadConfigFile path)
  case result :: Either Yaml.ParseException HMemConfig of
    Right cfg -> expectationFailure $
      "historical provider identity was replaced during production loading: " <> show cfg.embeddingProvider
    Left err -> do
      show err `shouldSatisfy` isInfixOf "historical GTE-Qwen2 space with unknown attention semantics"
      show err `shouldSatisfy` isInfixOf "attention=noncausal:v1"

withEnvVar :: String -> Maybe String -> IO a -> IO a
withEnvVar name value = bracket setup restore . const
  where
    setup = do
      old <- lookupEnv name
      maybe (unsetEnv name) (setEnv name) value
      pure old

    restore = maybe (unsetEnv name) (setEnv name)
