module HMem.ConfigSpec (spec) where

import Data.ByteString.Char8 qualified as BS8
import Data.Yaml qualified as Yaml
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

  describe "validateConfig" $ do
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

  describe "default auth schema" $ do
    it "defaults to local auth mode with local bootstrap enabled" $ do
      defaultConfig.auth.mode `shouldBe` AuthModeLocal
      defaultConfig.auth.local.bootstrapEnabled `shouldBe` True

    it "defaults deployed token lookup to database" $ do
      defaultConfig.auth.deployed.tokenLookup `shouldBe` TokenLookupDatabase

  describe "auth config parsing" $ do
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
            ]
      case Yaml.decodeEither' yaml of
        Left err -> expectationFailure (show err)
        Right (cfg :: HMemConfig) -> do
          cfg.auth.mode `shouldBe` AuthModeDeployed
          cfg.auth.enabled `shouldBe` True
          cfg.auth.deployed.issuer `shouldBe` Just "https://issuer.example"
          cfg.auth.deployed.audience `shouldBe` Just "hmem-web"
          cfg.auth.deployed.tokenLookup `shouldBe` TokenLookupDatabase
          authStaticBearerEnabled cfg.auth `shouldBe` False

    it "round-trips the richer auth schema through yaml encoding" $ do
      let cfg = defaultConfig
            { auth = defaultConfig.auth
                { mode = AuthModeDeployed
                , enabled = True
                , local = LocalAuthConfig
                    { bootstrapEnabled = False
                    , botTokens = [ LocalBotTokenConfig { label = "codex", token = "secret-token" } ]
                    }
                , deployed = DeployedAuthConfig
                    { issuer = Just "https://issuer.example"
                    , audience = Just "hmem-web"
                    , discoveryUrl = Nothing
                    , jwksUrl = Just "https://issuer.example/.well-known/jwks.json"
                    , tokenLookup = TokenLookupDatabase
                    }
                }
            }
      case Yaml.decodeEither' (Yaml.encode cfg) of
        Left err -> expectationFailure (show err)
        Right (decoded :: HMemConfig) -> decoded `shouldBe` cfg
