module HMem.Server.AuthTokensSpec (spec) where

import Data.Text qualified as T
import Data.Time (UTCTime(..), addUTCTime, fromGregorian, getCurrentTime, secondsToDiffTime)
import Data.UUID qualified as UUID
import Hasql.Decoders qualified as Dec
import Hasql.Encoders qualified as Enc
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement
import Test.Hspec

import HMem.DB.Auth qualified as Auth
import HMem.DB.Pool qualified as DBPool
import HMem.DB.RequestContext (ActorType(..), Principal(..), PrincipalAuthority(..))
import HMem.DB.TestHarness (TestEnv(..), withTestEnv)
import HMem.Server.AuthBootstrap
import HMem.Server.AuthTokens

spec :: Spec
spec = describe "service/PAT token lifecycle" $ do
  it "issues a high-entropy display-once token and resolves it through token_hash" $
    withTestEnv $ \env -> do
      grantUserId <- createGrantUser env "token-grant-issue"
      expiry <- Just . addUTCTime 3600 <$> getCurrentTime

      issued <- expectRight =<< issueAccessToken env.pool IssueAccessTokenInput
        { grantUserId = grantUserId
        , actorType = AccessTokenActorBot
        , actorLabel = "Deploy Bot"
        , expiresAt = expiry
        }

      issued.rawToken `shouldSatisfy` isGeneratedAccessTokenFormat
      issued.rawToken `shouldSatisfy` T.isPrefixOf generatedAccessTokenPrefix
      T.length (T.drop (T.length generatedAccessTokenPrefix) issued.rawToken)
        `shouldBe` generatedAccessTokenRandomHexChars
      generatedAccessTokenMinEntropyBits `shouldBe` 256

      mResolved <- Auth.resolveAccessTokenPrincipal env.pool issued.rawToken
      case mResolved of
        Nothing -> expectationFailure "expected issued token to resolve"
        Just resolved -> do
          resolved.tokenId `shouldBe` issued.tokenId
          resolved.principal.actorType `shouldBe` ActorBot
          resolved.principal.actorLabel `shouldBe` "Deploy Bot"
          resolved.principal.authority `shouldBe` PrincipalGrantUser grantUserId

      rawHashLookup <- Auth.resolveAccessTokenPrincipal env.pool (Auth.accessTokenHash issued.rawToken)
      rawHashLookup `shouldBe` Nothing

  it "defines and validates the official hmem access-token format" $ do
    first <- generateAccessToken
    second <- generateAccessToken

    first `shouldSatisfy` isGeneratedAccessTokenFormat
    second `shouldSatisfy` isGeneratedAccessTokenFormat
    first `shouldNotBe` second

    isGeneratedAccessTokenFormat "short-token" `shouldBe` False
    isGeneratedAccessTokenFormat (generatedAccessTokenPrefix <> T.replicate (generatedAccessTokenRandomHexChars - 1) "a") `shouldBe` False
    isGeneratedAccessTokenFormat (generatedAccessTokenPrefix <> T.replicate generatedAccessTokenRandomHexChars "g") `shouldBe` False

  it "uses the explicit least-privilege grant-bearing user as token authority" $
    withTestEnv $ \env -> do
      grantUserId <- createPlainGrantUser env

      issued <- expectRight =<< issueAccessToken env.pool IssueAccessTokenInput
        { grantUserId = grantUserId
        , actorType = AccessTokenActorBot
        , actorLabel = "Least Privilege Bot"
        , expiresAt = Nothing
        }

      mResolved <- Auth.resolveAccessTokenPrincipal env.pool issued.rawToken
      case mResolved of
        Nothing -> expectationFailure "expected least-privilege token to resolve"
        Just resolved -> do
          resolved.principal.actorType `shouldBe` ActorBot
          resolved.principal.actorLabel `shouldBe` "Least Privilege Bot"
          resolved.principal.authority `shouldBe` PrincipalGrantUser grantUserId
      grants <- Auth.getUserGrants env.pool grantUserId
      grants `shouldBe` Just (Auth.UserGrants False False)

  it "stores HMAC token hashes when a token hash secret is configured and keeps legacy fallback" $
    withTestEnv $ \env -> do
      grantUserId <- createGrantUser env "token-grant-hmac"
      let hashSecret = Just "server-token-hash-secret"

      hmacIssued <- expectRight =<< issueAccessTokenWithSecret env.pool hashSecret (defaultIssueInput grantUserId)
      legacyIssued <- expectRight =<< issueAccessToken env.pool (defaultIssueInput grantUserId)

      lookupTokenHash env hmacIssued.tokenId `shouldReturn` Auth.accessTokenHmacHash "server-token-hash-secret" hmacIssued.rawToken
      lookupTokenHash env legacyIssued.tokenId `shouldReturn` Auth.accessTokenHash legacyIssued.rawToken

      Auth.resolveAccessTokenPrincipalWithSecret env.pool hashSecret hmacIssued.rawToken
        `shouldSatisfyM` maybe False ((== hmacIssued.tokenId) . (.tokenId))
      Auth.resolveAccessTokenPrincipal env.pool hmacIssued.rawToken `shouldReturn` Nothing

      Auth.resolveAccessTokenPrincipalWithSecret env.pool hashSecret legacyIssued.rawToken
        `shouldSatisfyM` maybe False ((== legacyIssued.tokenId) . (.tokenId))

  it "revokes tokens so the raw token no longer resolves" $
    withTestEnv $ \env -> do
      grantUserId <- createGrantUser env "token-grant-revoke"
      issued <- expectRight =<< issueAccessToken env.pool (defaultIssueInput grantUserId)

      firstRevoke <- revokeAccessToken env.pool issued.tokenId
      secondRevoke <- revokeAccessToken env.pool issued.tokenId
      firstRevoke `shouldBe` True
      secondRevoke `shouldBe` False

      mResolved <- Auth.resolveAccessTokenPrincipal env.pool issued.rawToken
      mResolved `shouldBe` Nothing

  it "rotates with an overlap window by default" $
    withTestEnv $ \env -> do
      grantUserId <- createGrantUser env "token-grant-rotate-overlap"
      old <- expectRight =<< issueAccessToken env.pool (defaultIssueInput grantUserId)
      new <- expectRight =<< rotateAccessToken env.pool RotateAccessTokenInput
        { sourceTokenId = old.tokenId
        , expiresAt = Nothing
        , revokeOld = False
        }

      new.tokenId `shouldNotBe` old.tokenId
      oldResolved <- Auth.resolveAccessTokenPrincipal env.pool old.rawToken
      newResolved <- Auth.resolveAccessTokenPrincipal env.pool new.rawToken
      oldResolved `shouldSatisfy` maybe False ((== old.tokenId) . (.tokenId))
      newResolved `shouldSatisfy` maybe False ((== new.tokenId) . (.tokenId))

  it "inherits or overrides expiry during rotation" $
    withTestEnv $ \env -> do
      grantUserId <- createGrantUser env "token-grant-rotate-expiry"
      let originalExpiry = UTCTime (fromGregorian 2099 5 1) (secondsToDiffTime 0)
          overrideExpiry = UTCTime (fromGregorian 2099 6 1) (secondsToDiffTime 0)
      old <- expectRight =<< issueAccessToken env.pool IssueAccessTokenInput
        { grantUserId = grantUserId
        , actorType = AccessTokenActorBot
        , actorLabel = "Deploy Bot"
        , expiresAt = Just originalExpiry
        }

      inherited <- expectRight =<< rotateAccessToken env.pool RotateAccessTokenInput
        { sourceTokenId = old.tokenId
        , expiresAt = Nothing
        , revokeOld = False
        }
      overridden <- expectRight =<< rotateAccessToken env.pool RotateAccessTokenInput
        { sourceTokenId = old.tokenId
        , expiresAt = Just overrideExpiry
        , revokeOld = False
        }

      lookupTokenExpiry env old.tokenId `shouldReturn` Just originalExpiry
      lookupTokenExpiry env inherited.tokenId `shouldReturn` Just originalExpiry
      lookupTokenExpiry env overridden.tokenId `shouldReturn` Just overrideExpiry

  it "can rotate and immediately revoke the old token" $
    withTestEnv $ \env -> do
      grantUserId <- createGrantUser env "token-grant-rotate-revoke"
      old <- expectRight =<< issueAccessToken env.pool (defaultIssueInput grantUserId)
      new <- expectRight =<< rotateAccessToken env.pool RotateAccessTokenInput
        { sourceTokenId = old.tokenId
        , expiresAt = Nothing
        , revokeOld = True
        }

      oldResolved <- Auth.resolveAccessTokenPrincipal env.pool old.rawToken
      newResolved <- Auth.resolveAccessTokenPrincipal env.pool new.rawToken
      oldResolved `shouldBe` Nothing
      newResolved `shouldSatisfy` maybe False ((== new.tokenId) . (.tokenId))

  it "rejects empty labels, missing grant users, and revoked rotation sources" $
    withTestEnv $ \env -> do
      grantUserId <- createGrantUser env "token-grant-errors"
      emptyLabel <- issueAccessToken env.pool IssueAccessTokenInput
        { grantUserId = grantUserId
        , actorType = AccessTokenActorBot
        , actorLabel = "  "
        , expiresAt = Nothing
        }
      emptyLabel `shouldBeLeft` EmptyActorLabel

      missingGrant <- issueAccessToken env.pool (defaultIssueInput UUID.nil)
      missingGrant `shouldBeLeft` GrantUserNotFound UUID.nil

      issued <- expectRight =<< issueAccessToken env.pool (defaultIssueInput grantUserId)
      revoked <- revokeAccessToken env.pool issued.tokenId
      revoked `shouldBe` True
      rotated <- rotateAccessToken env.pool RotateAccessTokenInput
        { sourceTokenId = issued.tokenId
        , expiresAt = Nothing
        , revokeOld = False
        }
      rotated `shouldBeLeft` SourceTokenNotFound issued.tokenId

createGrantUser :: TestEnv -> T.Text -> IO UUID.UUID
createGrantUser env subject = do
  result <- expectRight =<< bootstrapSuperadmin env.pool BootstrapSuperadminInput
    { authSubject = subject
    , email = Nothing
    , displayName = Just subject
    , force = False
  }
  pure result.userId

createPlainGrantUser :: TestEnv -> IO UUID.UUID
createPlainGrantUser env =
  DBPool.runSession env.pool $ Session.statement () createPlainGrantUserStatement

createPlainGrantUserStatement :: Statement.Statement () UUID.UUID
createPlainGrantUserStatement = Statement.Statement sql Enc.noParams decoder True
  where
    sql = "INSERT INTO users (can_create_workspace, is_superadmin) VALUES (false, false) RETURNING id"
    decoder = Dec.singleRow (Dec.column (Dec.nonNullable Dec.uuid))

lookupTokenExpiry :: TestEnv -> UUID.UUID -> IO (Maybe UTCTime)
lookupTokenExpiry env tokenId =
  DBPool.runSession env.pool $ Session.statement tokenId tokenExpiryStatement

tokenExpiryStatement :: Statement.Statement UUID.UUID (Maybe UTCTime)
tokenExpiryStatement = Statement.Statement sql encoder decoder True
  where
    sql = "SELECT expires_at FROM access_tokens WHERE id = $1"
    encoder = Enc.param (Enc.nonNullable Enc.uuid)
    decoder = Dec.singleRow (Dec.column (Dec.nullable Dec.timestamptz))

lookupTokenHash :: TestEnv -> UUID.UUID -> IO T.Text
lookupTokenHash env tokenId =
  DBPool.runSession env.pool $ Session.statement tokenId tokenHashStatement

tokenHashStatement :: Statement.Statement UUID.UUID T.Text
tokenHashStatement = Statement.Statement sql encoder decoder True
  where
    sql = "SELECT token_hash FROM access_tokens WHERE id = $1"
    encoder = Enc.param (Enc.nonNullable Enc.uuid)
    decoder = Dec.singleRow (Dec.column (Dec.nonNullable Dec.text))

defaultIssueInput :: UUID.UUID -> IssueAccessTokenInput
defaultIssueInput userId = IssueAccessTokenInput
  { grantUserId = userId
  , actorType = AccessTokenActorBot
  , actorLabel = "Deploy Bot"
  , expiresAt = Nothing
  }

expectRight :: (Show e) => Either e a -> IO a
expectRight = \case
  Left err -> fail ("expected Right, got Left " <> show err)
  Right value -> pure value

shouldBeLeft :: (Eq e, Show e) => Either e a -> e -> Expectation
shouldBeLeft actual expected = case actual of
  Left err -> err `shouldBe` expected
  Right _ -> expectationFailure $ "expected Left " <> show expected <> ", got Right"

shouldSatisfyM :: (Show a) => IO a -> (a -> Bool) -> Expectation
shouldSatisfyM action predicate = do
  value <- action
  value `shouldSatisfy` predicate
