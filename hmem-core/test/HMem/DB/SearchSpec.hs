module HMem.DB.SearchSpec (spec) where

import Control.Exception (try)
import Data.Text (Text)
import Data.UUID (UUID)
import Test.Hspec

import HMem.DB.Observation
import HMem.DB.Pool (DBException(..))
import HMem.DB.Project
import HMem.DB.Search
import HMem.DB.Task
import HMem.DB.TestHarness
import HMem.Types

spec :: Spec
spec = beforeAll setupTestPool $ aroundWith withTestTransaction $
  describe "Unified observation search" $ do
    it "searches workspace-scoped observations with exact provenance filters without enriching other hits" $ \env -> do
      workspace <- createTestWorkspace env "unified-observation-search"
      matching <- createObservation env.pool CreateObservation
        { workspaceId = workspace.id, subjectKind = SubjectFile, subject = "src/Search.hs"
        , gitSha = canonicalSha, content = "needle observation" }
      _distractor <- createObservation env.pool CreateObservation
        { workspaceId = workspace.id, subjectKind = SubjectGlob, subject = "src/**/*.hs"
        , gitSha = canonicalSha, content = "needle distractor" }
      project <- createProject env.pool CreateProject
        { workspaceId = workspace.id, parentId = Nothing, name = "needle project"
        , description = Nothing, priority = Nothing, metadata = Nothing }
      task <- createTask env.pool CreateTask
        { workspaceId = workspace.id, projectId = Just project.id, parentId = Nothing
        , title = "needle task", description = Nothing, priority = Nothing
        , metadata = Nothing, dueAt = Nothing }
      results <- searchAll env.pool (unifiedQuery (Just workspace.id) (Just [SearchObservation]) (Just SubjectFile) (Just "src/Search.hs") (Just canonicalSha))
      map (.id) results.observations `shouldBe` [matching.id]
      results.projects `shouldBe` []
      results.tasks `shouldBe` []
      allResults <- searchAll env.pool (unifiedQuery (Just workspace.id) Nothing Nothing Nothing Nothing)
      map (.id) allResults.observations `shouldMatchList` [matching.id, _distractor.id]
      map (.id) allResults.projects `shouldBe` [project.id]
      map (.id) allResults.tasks `shouldBe` [task.id]

    it "requires workspace_id for every unified search scope" $ \env -> do
      rejected <- try @DBException $ searchAll env.pool (unifiedQuery Nothing Nothing Nothing Nothing Nothing)
      rejected `shouldSatisfy` isWorkspaceRequired
      projectOnly <- try @DBException $ searchAll env.pool (unifiedQuery Nothing (Just [SearchProject]) Nothing Nothing Nothing)
      projectOnly `shouldSatisfy` isWorkspaceRequired

unifiedQuery :: Maybe UUID -> Maybe [EntitySearchType] -> Maybe SubjectKind -> Maybe Text -> Maybe Text -> UnifiedSearchQuery
unifiedQuery workspace kinds kind path sha = UnifiedSearchQuery
  { workspaceId = workspace, query = Just "needle", entityTypes = kinds, searchLanguage = Nothing
  , limit = Just 10, offset = Just 0, subjectKind = kind, subject = path, gitSha = sha
  , projectStatus = Nothing, taskStatus = Nothing, taskPriority = Nothing, projectId = Nothing }

isWorkspaceRequired :: Either DBException UnifiedSearchResults -> Bool
isWorkspaceRequired (Left (DBCheckViolation message)) = message == "workspace_id is required for unified search"
isWorkspaceRequired _ = False

canonicalSha :: Text
canonicalSha = "0123456789abcdef0123456789abcdef01234567"
