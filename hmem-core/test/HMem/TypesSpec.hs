module HMem.TypesSpec (spec) where

import Data.Aeson (Value(Null))
import Data.UUID (UUID)
import Test.Hspec

import HMem.Types

spec :: Spec
spec = do
  describe "Unified observation search validation" $ do
    it "requires a workspace for every entity selection" $
      validateUnifiedSearchQuery observationSearch `shouldBe` ["workspace_id is required for unified search"]
    it "rejects project-only searches without a workspace" $
      validateUnifiedSearchQuery observationSearch { entityTypes = Just [SearchProject] }
        `shouldBe` ["workspace_id is required for unified search"]
  describe "Saved view validation" $ do
    it "accepts observation views and rejects legacy memory views" $ do
      validateCreateSavedViewInput savedView { entityType = "observation_search" } `shouldBe` []
      validateCreateSavedViewInput savedView { entityType = "memory_search" } `shouldSatisfy` (not . null)
  where
    workspace = read "00000000-0000-0000-0000-000000000001" :: UUID
    observationSearch = UnifiedSearchQuery
      { workspaceId = Nothing, query = Nothing, entityTypes = Nothing, searchLanguage = Nothing
      , limit = Nothing, offset = Nothing, subjectKind = Nothing, subject = Nothing, gitSha = Nothing
      , projectStatus = Nothing, taskStatus = Nothing, taskPriority = Nothing, projectId = Nothing }
    savedView = CreateSavedView workspace "view" Nothing "activity" Null
