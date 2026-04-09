{-# OPTIONS_GHC -fno-warn-orphans #-}

module HMem.TypesSpec (spec) where

import Data.Aeson (decode, encode)
import Data.Maybe (isJust, isNothing)
import Data.Text qualified as T
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

    it "TaskListQuery roundtrips through JSON" $ do
      let tq = TaskListQuery
            { workspaceId = Just (read "00000000-0000-0000-0000-000000000001")
            , projectId = Nothing
            , status = Just Todo
            , priority = Just 4
            , query = Nothing
            , searchLanguage = Nothing
            , createdAfter = Just (read "2026-03-30 10:00:00 UTC")
            , createdBefore = Just (read "2026-03-30 11:00:00 UTC")
            , updatedAfter = Nothing
            , updatedBefore = Nothing
            , limit = Just 25
            , offset = Just 5
            }
      decode (encode tq) `shouldBe` Just tq

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
            , projectId   = Unchanged
            , parentId    = Unchanged
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

  describe "input validation" $ do
    it "rejects oversized memory content" $ do
      let cm = CreateMemory
            { workspaceId = read "00000000-0000-0000-0000-000000000001"
            , content = T.replicate (maxMemoryContentBytes + 1) "a"
            , summary = Nothing
            , memoryType = ShortTerm
            , importance = Nothing
            , metadata = Nothing
            , expiresAt = Nothing
            , source = Nothing
            , confidence = Nothing
            , pinned = Nothing
            , tags = Nothing
            , ftsLanguage = Nothing
            }
      validateCreateMemoryInput cm `shouldBe` ["content exceeds 524288 bytes"]

    it "rejects blank task titles" $ do
      let ct = CreateTask
            { workspaceId = read "00000000-0000-0000-0000-000000000001"
            , projectId = Nothing
            , parentId = Nothing
            , title = "   "
            , description = Nothing
            , priority = Nothing
            , metadata = Nothing
            , dueAt = Nothing
            }
      validateCreateTaskInput ct `shouldBe` ["title must not be empty"]

    it "rejects empty batches and empty batch items" $ do
      validateCreateMemoryBatchInput [] `shouldBe` ["memories must contain at least one item"]
      let cm = CreateMemory
            { workspaceId = read "00000000-0000-0000-0000-000000000001"
            , content = ""
            , summary = Nothing
            , memoryType = ShortTerm
            , importance = Nothing
            , metadata = Nothing
            , expiresAt = Nothing
            , source = Nothing
            , confidence = Nothing
            , pinned = Nothing
            , tags = Nothing
            , ftsLanguage = Nothing
            }
      validateCreateMemoryBatchInput [cm] `shouldBe` ["memories[0].content must not be empty"]

    it "rejects reversed memory list time ranges" $ do
      let mq = MemoryListQuery
            { workspaceId = Nothing
            , memoryType = Nothing
            , createdAfter = Just (read "2026-03-30 11:00:00 UTC")
            , createdBefore = Just (read "2026-03-30 10:00:00 UTC")
            , updatedAfter = Nothing
            , updatedBefore = Nothing
            , limit = Nothing
            , offset = Nothing
            }
      validateMemoryListQuery mq `shouldBe`
        ["created_after must be earlier than or equal to created_before"]

    it "rejects invalid task list filters" $ do
      let tq = TaskListQuery
            { workspaceId = Nothing
            , projectId = Nothing
            , status = Nothing
            , priority = Just 99
            , createdAfter = Nothing
            , createdBefore = Nothing
            , updatedAfter = Just (read "2026-03-30 11:00:00 UTC")
            , updatedBefore = Just (read "2026-03-30 10:00:00 UTC")
            , query = Nothing
            , searchLanguage = Nothing
            , limit = Nothing
            , offset = Nothing
            }
      validateTaskListQuery tq `shouldBe`
        [ "workspace_id or project_id is required"
        , "priority must be between 1 and 10"
        , "updated_after must be earlier than or equal to updated_before"
        ]

  describe "capPagination properties" $ do
    prop "limit is always between 1 and maxPaginationLimit" $
      \(ml :: Maybe (Positive Int)) ->
        let (lim, _) = capPagination (getPositive <$> ml) Nothing
        in lim >= 1 && lim <= maxPaginationLimit

    prop "offset is always between 0 and maxPaginationOffset" $
      \(mo :: Maybe (NonNegative Int)) ->
        let (_, off) = capPagination Nothing (getNonNegative <$> mo)
        in off >= 0 && off <= maxPaginationOffset

    it "defaults limit to 50 and offset to 0" $
      capPagination Nothing Nothing `shouldBe` (50, 0)

    it "caps extreme values" $
      capPagination (Just 999999) (Just 999999) `shouldBe` (maxPaginationLimit, maxPaginationOffset)

    it "clamps negative limit to 1" $
      fst (capPagination (Just (-5)) Nothing) `shouldBe` 1

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

isInfixOf' :: String -> String -> Bool
isInfixOf' []    _  = True
isInfixOf' _     [] = False
isInfixOf' needle hay@(_:rest)
  | take (length needle) hay == needle = True
  | otherwise = isInfixOf' needle rest
