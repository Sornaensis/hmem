module HMem.Server.AuthUsersSpec (spec) where

import Test.Hspec

import HMem.DB.Auth qualified as Auth
import HMem.DB.RequestContext (Principal(..), PrincipalAuthority(..))
import HMem.DB.TestHarness (TestEnv(..), withTestEnv)
import HMem.Server.AuthUsers

spec :: Spec
spec = describe "deployed user/global grant administration" $ do
  it "creates a deployed user with requested global grants" $
    withTestEnv $ \env -> do
      created <- expectRight =<< upsertUser env.pool UpsertUserInput
        { authSubject = "provider-user-create"
        , email = Just "create@example.com"
        , displayName = Just "Create User"
        , canCreateWorkspace = Just True
        , isSuperadmin = Just False
        }

      created.decision `shouldBe` UserCreated
      created.canCreateWorkspace `shouldBe` True
      created.isSuperadmin `shouldBe` False
      Auth.getUserGrants env.pool created.userId `shouldReturn` Just (Auth.UserGrants True False)

      mPrincipal <- Auth.resolveUserPrincipalByAuthSubject env.pool "provider-user-create"
      case mPrincipal of
        Nothing -> expectationFailure "expected created user subject to resolve"
        Just principal -> do
          principal.authority `shouldBe` PrincipalGrantUser created.userId
          principal.actorLabel `shouldBe` "Create User"

  it "updates global grants and preserves omitted grants" $
    withTestEnv $ \env -> do
      created <- expectRight =<< upsertUser env.pool UpsertUserInput
        { authSubject = "provider-user-update"
        , email = Nothing
        , displayName = Just "Update User"
        , canCreateWorkspace = Just True
        , isSuperadmin = Just True
        }

      updated <- expectRight =<< upsertUser env.pool UpsertUserInput
        { authSubject = "provider-user-update"
        , email = Just "updated@example.com"
        , displayName = Nothing
        , canCreateWorkspace = Nothing
        , isSuperadmin = Just False
        }

      updated.decision `shouldBe` UserUpdated
      updated.userId `shouldBe` created.userId
      updated.canCreateWorkspace `shouldBe` True
      updated.isSuperadmin `shouldBe` False
      Auth.getUserGrants env.pool updated.userId `shouldReturn` Just (Auth.UserGrants True False)

  it "defaults new users to no global grants when grant flags are omitted" $
    withTestEnv $ \env -> do
      created <- expectRight =<< upsertUser env.pool UpsertUserInput
        { authSubject = "provider-user-default-grants"
        , email = Nothing
        , displayName = Nothing
        , canCreateWorkspace = Nothing
        , isSuperadmin = Nothing
        }

      created.canCreateWorkspace `shouldBe` False
      created.isSuperadmin `shouldBe` False
      Auth.getUserGrants env.pool created.userId `shouldReturn` Just (Auth.UserGrants False False)

  it "rejects empty auth subjects" $
    withTestEnv $ \env -> do
      result <- upsertUser env.pool UpsertUserInput
        { authSubject = "  "
        , email = Nothing
        , displayName = Nothing
        , canCreateWorkspace = Just True
        , isSuperadmin = Just True
        }
      result `shouldBe` Left EmptyAuthSubject

expectRight :: (Show e) => Either e a -> IO a
expectRight = \case
  Left err -> fail ("expected Right, got Left " <> show err)
  Right value -> pure value
