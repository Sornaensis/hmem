module HMem.Server.AuthBootstrapSpec (spec) where

import Data.Text qualified as T
import Test.Hspec

import HMem.DB.Auth qualified as Auth
import HMem.DB.RequestContext (Principal(..), PrincipalAuthority(..))
import HMem.DB.TestHarness (TestEnv(..), withTestEnv)
import HMem.Server.AuthBootstrap

spec :: Spec
spec = describe "first superadmin bootstrap" $ do
  it "creates a provider-subject user with superadmin grants" $
    withTestEnv $ \env -> do
      result <- expectRight =<< bootstrapSuperadmin env.pool (bootstrapInput "provider-bootstrap-1")
      result.decision `shouldBe` BootstrapCreated

      grants <- Auth.getUserGrants env.pool result.userId
      grants `shouldBe` Just (Auth.UserGrants True True)

      mPrincipal <- Auth.resolveUserPrincipalByAuthSubject env.pool "provider-bootstrap-1"
      case mPrincipal of
        Nothing -> expectationFailure "expected provider subject to resolve to principal"
        Just principal -> do
          principal.authority `shouldBe` PrincipalGrantUser result.userId
          principal.actorLabel `shouldBe` "Primary Operator"

  it "is idempotent for the same auth subject" $
    withTestEnv $ \env -> do
      first <- expectRight =<< bootstrapSuperadmin env.pool (bootstrapInput "provider-bootstrap-idempotent")
      second <- expectRight =<< bootstrapSuperadmin env.pool (bootstrapInput "provider-bootstrap-idempotent")

      first.decision `shouldBe` BootstrapCreated
      second.decision `shouldBe` BootstrapAlreadySatisfied
      second.userId `shouldBe` first.userId

  it "refuses a different existing superadmin unless forced" $
    withTestEnv $ \env -> do
      first <- expectRight =<< bootstrapSuperadmin env.pool (bootstrapInput "provider-bootstrap-first")

      refused <- bootstrapSuperadmin env.pool (bootstrapInput "provider-bootstrap-second")
      case refused of
        Left (DifferentSuperadminExists existing) -> existing.existingUserId `shouldBe` first.userId
        other -> expectationFailure $ "expected DifferentSuperadminExists, got " <> show other

      forced <- expectRight =<< bootstrapSuperadmin env.pool
        ((bootstrapInput "provider-bootstrap-second") { force = True })
      forced.decision `shouldBe` BootstrapCreated
      forced.userId `shouldNotBe` first.userId

      rerunFirst <- expectRight =<< bootstrapSuperadmin env.pool (bootstrapInput "provider-bootstrap-first")
      rerunSecond <- expectRight =<< bootstrapSuperadmin env.pool (bootstrapInput "provider-bootstrap-second")
      rerunFirst.decision `shouldBe` BootstrapAlreadySatisfied
      rerunFirst.userId `shouldBe` first.userId
      rerunSecond.decision `shouldBe` BootstrapAlreadySatisfied
      rerunSecond.userId `shouldBe` forced.userId

bootstrapInput :: T.Text -> BootstrapSuperadminInput
bootstrapInput subject = BootstrapSuperadminInput
  { authSubject = subject
  , email = Just "operator@example.com"
  , displayName = Just "Primary Operator"
  , force = False
  }

expectRight :: (Show e) => Either e a -> IO a
expectRight = \case
  Left err -> fail ("expected Right, got Left " <> show err)
  Right value -> pure value
