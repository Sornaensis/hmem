module HMem.BotIdentitySpec (spec) where

import Data.Aeson (eitherDecode, encode)
import Data.Either (isLeft, isRight)
import Data.Text qualified as T
import HMem.BotIdentity
import HMem.DB.RequestContext
import Data.UUID qualified as UUID
import Test.Hspec

spec :: Spec
spec = describe "declared bot identity" $ do
  it "canonicalizes outer whitespace, preserves case, and round-trips JSON" $ do
    mkBotIdentity " Reviewer " " pi:gpt-6.1-sol:extra-high " `shouldBe` Right identity
    eitherDecode (encode identity) `shouldBe` Right identity
    botRole identity `shouldBe` "Reviewer"
    botModelId identity `shouldBe` "pi:gpt-6.1-sol:extra-high"
    mkBotIdentity "reviewer" (botModelId identity) `shouldNotBe` Right identity

  it "rejects absent, blank, controlled, malformed and caller-selected actor fields" $ do
    mapM_ (\raw -> (eitherDecode raw :: Either String BotIdentity) `shouldSatisfy` isLeft)
      [ "{}", "null", "{\"role\":\"Reviewer\"}"
      , "{\"role\":\"Reviewer\",\"model-id\":\"pi:m:r\",\"actor_id\":\"local-user\"}"
      , "{\"role\":null,\"model-id\":\"pi:m:r\"}" ]
    mapM_ (\(role, model) -> mkBotIdentity role model `shouldSatisfy` isLeft)
      [ (" ", "pi:m:r"), ("Reviewer\n", "pi:m:r"), ("Rev\x200biewer", "pi:m:r")
      , ("Reviewer", "pi::r"), ("Reviewer", "pi:m"), ("Reviewer", "pi:m:r:x")
      , ("Reviewer", "pi:m:r\t"), ("Reviewer", "pi:m r:r"), ("Reviewer", "pi:m:é") ]

  it "bounds raw UTF-8 bytes before canonicalization" $ do
    mkBotIdentity (T.replicate 64 "é") "pi:m:r" `shouldSatisfy` isRight
    mkBotIdentity (T.replicate 65 "é") "pi:m:r" `shouldSatisfy` isLeft
    mkBotIdentity "Reviewer" ("pi:" <> T.replicate 251 "m" <> ":r") `shouldSatisfy` isRight
    mkBotIdentity "Reviewer" ("pi:" <> T.replicate 252 "m" <> ":r") `shouldSatisfy` isLeft
    mkBotIdentity "Reviewer" (T.replicate 128 "é" <> ":m:r") `shouldSatisfy` isLeft
    mkBotIdentity (T.replicate 128 " " <> "R") "pi:m:r" `shouldSatisfy` isLeft

  it "uses a stable versioned persona and changes it with either identity field" $ do
    localDeclaredBotId identity `shouldBe`
      "local-declared-bot:v1:7dba32460b55b7e68b2f4c59c1e9b1e92f8300221224de3c4c4f3f413e820c35"
    localDeclaredBotId identity `shouldSatisfy` T.isPrefixOf "local-declared-bot:v1:"
    T.length (localDeclaredBotId identity) `shouldBe` 86
    mapM_ (\other -> localDeclaredBotId other `shouldNotBe` localDeclaredBotId identity)
      [valid "Developer" "pi:gpt-6.1-sol:extra-high", valid "Reviewer" "pi:gpt-6.1-sol:high"]

  it "keeps original authority and attribution contexts separate and restores scopes" $ do
    let principal = Principal ActorUser "local-user" "Local User" PrincipalSyntheticLocalSuperadmin
        actor = resolveEffectiveActor True principal (Just identity)
    withPrincipalContext (Just principal) $ do
      withEffectiveActorContext (Just actor) $ do
        currentPrincipal `shouldReturn` Just principal
        currentEffectiveActor `shouldReturn` Just actor
        withWorkspaceIdContext (Just UUID.nil) $ currentEffectiveActor `shouldReturn` Just actor
      currentEffectiveActor `shouldReturn` Just (resolveEffectiveActor False principal Nothing)
    actor.actorType `shouldBe` ActorBot
    actor.actorId `shouldBe` localDeclaredBotId identity
    actor.botIdentity `shouldBe` Just identity

  it "groups the same canonical persona across workspace and request lifetimes" $ do
    let principal = Principal ActorUser "local-user" "Local User" PrincipalSyntheticLocalSuperadmin
        actor = resolveEffectiveActor True principal (Just identity)
        canonicalAgain = valid " Reviewer " " pi:gpt-6.1-sol:extra-high "
    withRequestContext (emptyRequestContext { requestId = Just "first", workspaceId = Just UUID.nil }) $
      withPrincipalContext (Just principal) $ withEffectiveActorContext (Just actor) $
        currentEffectiveActor `shouldReturn` Just actor
    withRequestContext (emptyRequestContext { requestId = Just "second", workspaceId = Nothing }) $
      withPrincipalContext (Just principal) $
        withEffectiveActorContext (Just (resolveEffectiveActor True principal (Just canonicalAgain))) $
          currentEffectiveActor `shouldReturn` Just actor

  it "never replaces authenticated IDs or grants and cannot bootstrap deployed callers" $ do
    let granted = Principal ActorBot "credential-bot" "Credential Bot" (PrincipalGrantUser UUID.nil)
        local = Principal ActorUser "local-user" "Local User" PrincipalSyntheticLocalSuperadmin
    mapM_ (\principal -> do
      let actor = resolveEffectiveActor True principal (Just identity)
      actor.actorId `shouldBe` principal.actorId
      actor.actorType `shouldBe` principal.actorType)
      [granted, Principal ActorUser "credential-bot" "Credential Bot" (PrincipalGrantUser UUID.nil)
      , local { authority = PrincipalNoAuthority }
      , Principal ActorUser "another-user" "Local User" PrincipalSyntheticLocalSuperadmin
      , Principal ActorBot "local-user" "Local User" PrincipalSyntheticLocalSuperadmin]
    (resolveEffectiveActor False local (Just identity)).actorType `shouldBe` ActorUser
    (resolveEffectiveActor True local Nothing).actorType `shouldBe` ActorUser

identity :: BotIdentity
identity = valid "Reviewer" "pi:gpt-6.1-sol:extra-high"

valid :: T.Text -> T.Text -> BotIdentity
valid role model = either (error . T.unpack) id (mkBotIdentity role model)
