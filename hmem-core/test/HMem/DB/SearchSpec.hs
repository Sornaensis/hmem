module HMem.DB.SearchSpec (spec) where

import Control.Exception (try)
import Control.Monad (forM, forM_)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified
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
        { workspaceId = workspace.id, subjects = [ObservationSubject SubjectFile "src/Search.hs"]
        , gitSha = canonicalSha, content = "needle observation" }
      _distractor <- createObservation env.pool CreateObservation
        { workspaceId = workspace.id, subjects = [ObservationSubject SubjectGlob "src/**/*.hs"]
        , gitSha = canonicalSha, content = "needle distractor" }
      crossRow <- createObservation env.pool CreateObservation
        { workspaceId = workspace.id
        , subjects = [ObservationSubject SubjectGlob "src/Search.hs", ObservationSubject SubjectFile "src/Else.hs"]
        , gitSha = canonicalSha, content = "needle cross-row" }
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
      results.hasMore `shouldBe` Map.fromList [("observations", False)]
      results.nextOffset `shouldBe` Map.empty
      allResults <- searchAll env.pool (unifiedQuery (Just workspace.id) Nothing Nothing Nothing Nothing)
      map (.id) allResults.observations `shouldMatchList` [matching.id, _distractor.id, crossRow.id]
      map (.id) allResults.projects `shouldBe` [project.id]
      map (.id) allResults.tasks `shouldBe` [task.id]
      allResults.hasMore `shouldBe` Map.fromList [("observations", False), ("projects", False), ("tasks", False)]
      allResults.nextOffset `shouldBe` Map.empty

    it "reports independent continuation without gaps across default, subset, final, and empty pages" $ \env -> do
      workspace <- createTestWorkspace env "unified-pagination"
      observations <- forM [1 .. 11 :: Int] $ \index -> createObservation env.pool CreateObservation
        { workspaceId = workspace.id, subjects = [ObservationSubject SubjectFile ("src/Page" <> showText index <> ".hs")]
        , gitSha = canonicalSha, content = "needle page" }
      projects <- forM [1 .. 3 :: Int] $ \index -> createProject env.pool CreateProject
        { workspaceId = workspace.id, parentId = Nothing, name = "needle project " <> showText index
        , description = Nothing, priority = Nothing, metadata = Nothing }
      tasks <- forM [1 .. 2 :: Int] $ \index -> createTask env.pool CreateTask
        { workspaceId = workspace.id, projectId = Just (head projects).id, parentId = Nothing
        , title = "needle task " <> showText index, description = Nothing, priority = Nothing
        , metadata = Nothing, dueAt = Nothing }
      let base :: UnifiedSearchQuery
          base = (unifiedQuery (Just workspace.id) Nothing Nothing Nothing Nothing) { limit = Nothing, offset = Nothing }
      first <- searchAll env.pool base
      length first.observations `shouldBe` 10
      map (.id) first.projects `shouldMatchList` map (.id) projects
      map (.id) first.tasks `shouldMatchList` map (.id) tasks
      first.hasMore `shouldBe` Map.fromList [("observations", True), ("projects", False), ("tasks", False)]
      first.nextOffset `shouldBe` Map.fromList [("observations", 10)]
      lastPage <- searchAll env.pool base { entityTypes = Just [SearchObservation], offset = Just 10 }
      length lastPage.observations `shouldBe` 1
      lastPage.projects `shouldBe` []
      lastPage.tasks `shouldBe` []
      lastPage.hasMore `shouldBe` Map.fromList [("observations", False)]
      lastPage.nextOffset `shouldBe` Map.empty
      full <- searchAll env.pool base { limit = Just 200 }
      map (.id) (first.observations <> lastPage.observations) `shouldBe` map (.id) full.observations
      map (.id) full.observations `shouldMatchList` map (.id) observations
      projectFirst <- searchAll env.pool base { entityTypes = Just [SearchProject, SearchTask], limit = Just 2 }
      projectFirst.hasMore `shouldBe` Map.fromList [("projects", True), ("tasks", False)]
      projectFirst.nextOffset `shouldBe` Map.fromList [("projects", 2)]
      projectLast <- searchAll env.pool base { entityTypes = Just [SearchProject], limit = Just 2, offset = Just 2 }
      projectLast.hasMore `shouldBe` Map.fromList [("projects", False)]
      map (.id) (projectFirst.projects <> projectLast.projects) `shouldBe` map (.id) full.projects
      empty <- searchAll env.pool base { entityTypes = Just [SearchObservation, SearchTask], offset = Just 100 }
      empty.observations `shouldBe` []
      empty.projects `shouldBe` []
      empty.tasks `shouldBe` []
      empty.hasMore `shouldBe` Map.fromList [("observations", False), ("tasks", False)]
      empty.nextOffset `shouldBe` Map.empty

    it "detects a following observation at the public 200-row limit" $ \env -> do
      workspace <- createTestWorkspace env "unified-max-page"
      forM_ [1 .. 201 :: Int] $ \index -> do
        _ <- createObservation env.pool CreateObservation
          { workspaceId = workspace.id, subjects = [ObservationSubject SubjectFile ("src/Max" <> showText index <> ".hs")]
          , gitSha = canonicalSha, content = "needle max page" }
        pure ()
      let base :: UnifiedSearchQuery
          base = unifiedQuery (Just workspace.id) (Just [SearchObservation]) Nothing Nothing Nothing
      first <- searchAll env.pool base { limit = Just 200 }
      length first.observations `shouldBe` 200
      first.hasMore `shouldBe` Map.fromList [("observations", True)]
      first.nextOffset `shouldBe` Map.fromList [("observations", 200)]
      final <- searchAll env.pool base { limit = Just 200, offset = Just 200 }
      length final.observations `shouldBe` 1
      final.hasMore `shouldBe` Map.fromList [("observations", False)]
      final.nextOffset `shouldBe` Map.empty
      length (Map.fromList [(hit.id, ()) | hit <- first.observations <> final.observations]) `shouldBe` 201

    it "accepts repeatable high search offsets while generic list pagination keeps its cap" $ \env -> do
      workspace <- createTestWorkspace env "unified-offset-boundary"
      let base :: UnifiedSearchQuery
          base = unifiedQuery (Just workspace.id) Nothing Nothing Nothing Nothing
      forM_ [100000, 100001, maxUnifiedSearchOffset] $ \offsetValue -> do
        let input :: UnifiedSearchQuery
            input = base { offset = Just offsetValue }
        validateUnifiedSearchQuery input `shouldBe` []
        page <- searchAll env.pool input
        page.observations `shouldBe` []
        page.projects `shouldBe` []
        page.tasks `shouldBe` []
        page.hasMore `shouldBe` Map.fromList [("observations", False), ("projects", False), ("tasks", False)]
      validateUnifiedSearchQuery base { offset = Just (maxUnifiedSearchOffset + 1) } `shouldSatisfy` (not . null)
      capUnifiedSearchOverfetch (Just 200) (Just 100001) `shouldBe` (201, 100001)
      capUnifiedSearchOverfetch (Just 10) (Just maxUnifiedSearchOffset) `shouldBe` (11, maxUnifiedSearchOffset)
      capPaginationOverfetch (Just 200) (Just 100001) `shouldBe` (200, 100000)
      validateObservationQuery (ObservationQuery workspace.id Nothing Nothing Nothing Nothing (Just 10) (Just 100001) Nothing Nothing) `shouldSatisfy` (not . null)
      searchNextOffset 100000 10 `shouldBe` Right 100010
      searchNextOffset (maxUnifiedSearchOffset - 1) 1 `shouldBe` Right maxUnifiedSearchOffset
      searchNextOffset maxUnifiedSearchOffset 1 `shouldBe` Left unifiedSearchContinuationError

    it "requires workspace_id for every unified search scope" $ \env -> do
      rejected <- try @DBException $ searchAll env.pool (unifiedQuery Nothing Nothing Nothing Nothing Nothing)
      rejected `shouldSatisfy` isWorkspaceRequired
      projectOnly <- try @DBException $ searchAll env.pool (unifiedQuery Nothing (Just [SearchProject]) Nothing Nothing Nothing)
      projectOnly `shouldSatisfy` isWorkspaceRequired

unifiedQuery :: Maybe UUID -> Maybe [EntitySearchType] -> Maybe SubjectKind -> Maybe Text -> Maybe Text -> UnifiedSearchQuery
unifiedQuery workspace kinds kind path sha = UnifiedSearchQuery
  { currentGitSha = Nothing, historyGitSha = Nothing,  workspaceId = workspace, query = Just "needle", entityTypes = kinds, searchLanguage = Nothing
  , limit = Just 10, offset = Just 0, subjectKind = kind, subject = path, gitSha = sha
  , projectStatus = Nothing, taskStatus = Nothing, taskPriority = Nothing, projectId = Nothing }

isWorkspaceRequired :: Either DBException UnifiedSearchResults -> Bool
isWorkspaceRequired (Left (DBCheckViolation message)) = message == "workspace_id is required for unified search"
isWorkspaceRequired _ = False

canonicalSha :: Text
canonicalSha = "0123456789abcdef0123456789abcdef01234567"

showText :: Show a => a -> Text
showText = Data.Text.pack . show
