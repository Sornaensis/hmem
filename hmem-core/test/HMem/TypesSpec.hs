{-# OPTIONS_GHC -fno-warn-orphans #-}

module HMem.TypesSpec (spec) where

import Data.Aeson (decode, encode)
import Data.Maybe (isJust, isNothing)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

import HMem.Types

------------------------------------------------------------------------
-- Arbitrary instances
------------------------------------------------------------------------

instance Arbitrary MemoryType where
  arbitrary = elements [ShortTerm, LongTerm]

instance Arbitrary ProjectStatus where
  arbitrary = elements [ProjActive, ProjPaused, ProjCompleted, ProjArchived]

instance Arbitrary TaskStatus where
  arbitrary = elements [Todo, InProgress, Blocked, Done, Cancelled]

instance Arbitrary RelationType where
  arbitrary = elements [Related, Supersedes, Contradicts, Elaborates, Inspires, DependsOn, DerivedFrom, AlternativeTo]

instance Arbitrary WorkspaceType where
  arbitrary = elements [WsRepository, WsPlanning, WsPersonal, WsOrganization]

------------------------------------------------------------------------
-- Tests
------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "camelToSnake" $ do
    it "converts simple camelCase" $
      camelToSnake "fooBar" `shouldBe` "foo_bar"

    it "handles leading lowercase" $
      camelToSnake "alreadyLower" `shouldBe` "already_lower"

    it "handles single word" $
      camelToSnake "word" `shouldBe` "word"

    it "handles empty string" $
      camelToSnake "" `shouldBe` ""

    it "converts multiple humps" $
      camelToSnake "oneTwoThree" `shouldBe` "one_two_three"

    it "lowercases leading uppercase" $
      camelToSnake "FooBar" `shouldBe` "foo_bar"

    prop "never produces consecutive underscores" $
      \s -> let ident = filter (\c -> (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9')) (s :: String)
            in not (null ident) ==>
              not ("__" `isInfixOf'` camelToSnake ident)

    prop "output is all lowercase plus underscores" $
      \s -> let ident = filter (\c -> (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9')) (s :: String)
            in not (null ident) ==>
              all (\c -> c == '_' || (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9')) (camelToSnake ident)

  describe "MemoryType round-trip" $ do
    prop "toText/fromText roundtrips" $
      \(mt :: MemoryType) ->
        memoryTypeFromText (memoryTypeToText mt) === Just mt

    prop "JSON encode/decode roundtrips" $
      \(mt :: MemoryType) ->
        decode (encode mt) === Just mt

    it "fromText rejects garbage" $
      memoryTypeFromText "garbage" `shouldSatisfy` isNothing

    it "fromText rejects empty" $
      memoryTypeFromText "" `shouldSatisfy` isNothing

  describe "ProjectStatus round-trip" $ do
    prop "toText/fromText roundtrips" $
      \(ps :: ProjectStatus) ->
        projectStatusFromText (projectStatusToText ps) === Just ps

    prop "JSON encode/decode roundtrips" $
      \(ps :: ProjectStatus) ->
        decode (encode ps) === Just ps

    it "fromText rejects garbage" $
      projectStatusFromText "garbage" `shouldSatisfy` isNothing

    it "covers all constructors" $
      let allStatuses = [ProjActive, ProjPaused, ProjCompleted, ProjArchived]
      in all (isJust . projectStatusFromText . projectStatusToText) allStatuses
           `shouldBe` True

  describe "TaskStatus round-trip" $ do
    prop "toText/fromText roundtrips" $
      \(ts :: TaskStatus) ->
        taskStatusFromText (taskStatusToText ts) === Just ts

    prop "JSON encode/decode roundtrips" $
      \(ts :: TaskStatus) ->
        decode (encode ts) === Just ts

    it "fromText rejects garbage" $
      taskStatusFromText "garbage" `shouldSatisfy` isNothing

    it "covers all constructors" $
      let allStatuses = [Todo, InProgress, Blocked, Done, Cancelled]
      in all (isJust . taskStatusFromText . taskStatusToText) allStatuses
           `shouldBe` True

  describe "JSON serialization options" $ do
    it "jsonOptions uses snake_case field labels" $
      camelToSnake "workspaceId" `shouldBe` "workspace_id"

    it "jsonOptions handles multi-word fields" $
      camelToSnake "lastAccessedAt" `shouldBe` "last_accessed_at"

  describe "domain type JSON" $ do
    it "CreateWorkspace roundtrips through JSON" $ do
      let cw = CreateWorkspace
            { name          = "my-workspace"
            , path          = Just "/home/user/project"
            , ghOwner       = Just "user"
            , ghRepo        = Just "repo"
            , workspaceType = Just WsRepository
            }
      decode (encode cw) `shouldBe` Just cw

    it "CreateWorkspace with Nothing fields roundtrips" $ do
      let cw = CreateWorkspace
            { name          = "minimal"
            , path          = Nothing
            , ghOwner       = Nothing
            , ghRepo        = Nothing
            , workspaceType = Nothing
            }
      decode (encode cw) `shouldBe` Just cw

    it "SearchQuery roundtrips through JSON" $ do
      let sq = SearchQuery
            { workspaceId   = Just (read "00000000-0000-0000-0000-000000000001")
            , query         = Just "test query"
            , memoryType    = Just ShortTerm
            , tags          = Just ["tag1", "tag2"]
            , minImportance = Just 3
            , categoryId    = Nothing
            , pinnedOnly    = Nothing
            , searchLanguage = Nothing
            , limit         = Just 10
            , offset        = Just 0
            }
      decode (encode sq) `shouldBe` Just sq

    it "UpdateMemory with all Unchanged roundtrips" $ do
      let um = UpdateMemory
            { content    = Nothing
            , summary    = Unchanged
            , memoryType = Nothing
            , importance = Nothing
            , metadata   = Nothing
            , expiresAt  = Unchanged
            , source     = Unchanged
            , confidence = Nothing
            , pinned     = Nothing
            }
      decode (encode um) `shouldBe` Just um

    it "UpdateTask with partial fields roundtrips" $ do
      let ut = UpdateTask
            { title       = Just "new title"
            , description = Unchanged
            , status      = Just Done
            , priority    = Just 8
            , metadata    = Nothing
            , dueAt       = Unchanged
            }
      decode (encode ut) `shouldBe` Just ut

    it "CleanupResult roundtrips" $ do
      let cr = CleanupResult
            { deletedCount = 42
            , workspaceId  = read "00000000-0000-0000-0000-000000000001"
            }
      decode (encode cr) `shouldBe` Just cr

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

isInfixOf' :: String -> String -> Bool
isInfixOf' []    _  = True
isInfixOf' _     [] = False
isInfixOf' needle hay@(_:rest)
  | take (length needle) hay == needle = True
  | otherwise = isInfixOf' needle rest
