module HMem.ConfigSpec (spec) where

import Test.Hspec

import HMem.Config

spec :: Spec
spec = do
  describe "applyEnvOverrides" $ do
    it "uses HMEM_DB_PASSWORD when present" $ do
      let cfg = defaultConfig
            { database = defaultConfig.database { password = Just "from-config" }
            }
          overridden = applyEnvOverrides (Just "from-env") Nothing cfg
      overridden.database.password `shouldBe` Just "from-env"

    it "keeps config password when env var is absent" $ do
      let cfg = defaultConfig
            { database = defaultConfig.database { password = Just "from-config" }
            }
          overridden = applyEnvOverrides Nothing Nothing cfg
      overridden.database.password `shouldBe` Just "from-config"

    it "leaves password unset when neither source provides one" $ do
      let overridden = applyEnvOverrides Nothing Nothing defaultConfig
      overridden.database.password `shouldBe` Nothing

    it "uses HMEM_API_KEY when present" $ do
      let cfg = defaultConfig
            { auth = AuthConfig { enabled = True, apiKey = Just "from-config" } }
          overridden = applyEnvOverrides Nothing (Just "from-env") cfg
      overridden.auth.apiKey `shouldBe` Just "from-env"

  describe "validateConfig" $ do
    it "disables auth when enabled without an API key" $ do
      let cfg = defaultConfig { auth = AuthConfig { enabled = True, apiKey = Nothing } }
          (warnings, validated) = validateConfig cfg
      warnings `shouldSatisfy` (not . null)
      validated.auth.enabled `shouldBe` False