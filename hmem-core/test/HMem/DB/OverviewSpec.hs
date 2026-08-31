module HMem.DB.OverviewSpec (spec) where

import Data.List (sort)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.UUID (UUID)
import Test.Hspec

import HMem.DB.Observation
import HMem.DB.Overview
import HMem.DB.Project
import HMem.DB.Task
import HMem.DB.TestHarness
import HMem.Types

spec :: Spec
spec = beforeAll setupTestPool $ aroundWith withTestTransaction $
  describe "Overview" $ do
    it "returns project/task hierarchy, dependencies, and readiness without observation links" $ \env -> do
      workspace <- createTestWorkspace env "overview-without-observation-links"
      _observation <- createObservation env.pool CreateObservation
        { workspaceId = workspace.id, subjects = [ObservationSubject SubjectFile "src/Overview.hs"]
        , gitSha = canonicalSha, content = "independent observation" }
      project <- createProject env.pool CreateProject
        { workspaceId = workspace.id, parentId = Nothing, name = "project", description = Nothing
        , priority = Nothing, metadata = Nothing }
      child <- createProject env.pool CreateProject
        { workspaceId = workspace.id, parentId = Just project.id, name = "child", description = Nothing
        , priority = Nothing, metadata = Nothing }
      dependency <- createTask env.pool (newTask workspace.id (Just project.id) "dependency")
      task <- createTask env.pool (newTask workspace.id (Just project.id) "task")
      addDependency env.pool task.id dependency.id

      projectOverview <- getProjectOverview env.pool project.id
      case projectOverview of
        Nothing -> expectationFailure "Expected project overview"
        Just overview -> do
          map (.id) overview.tasks `shouldMatchList` [dependency.id, task.id]
          map (.id) overview.subprojects `shouldBe` [child.id]
          overview.readinessRollup.completionReady `shouldBe` False

      taskOverview <- getTaskOverview env.pool task.id
      case taskOverview of
        Nothing -> expectationFailure "Expected task overview"
        Just overview -> do
          map (.id) overview.dependencies `shouldBe` [dependency.id]
          overview.readinessRollup.completionReady `shouldBe` True

    it "keeps batched card rollups equal to the legacy single-overview semantics" $ \env -> do
      workspace <- createTestWorkspace env "overview-card-rollup-parity"
      project <- createProject env.pool CreateProject
        { workspaceId = workspace.id, parentId = Nothing, name = "parent", description = Nothing
        , priority = Nothing, metadata = Nothing }
      child <- createProject env.pool CreateProject
        { workspaceId = workspace.id, parentId = Just project.id, name = "child", description = Nothing
        , priority = Nothing, metadata = Nothing }
      dependency <- createTask env.pool (newTask workspace.id (Just project.id) "dependency")
      task <- createTask env.pool (newTask workspace.id (Just project.id) "task")
      subtask <- createTask env.pool (newTask workspace.id (Just project.id) "subtask")
      updateTask env.pool subtask.id (UpdateTask Nothing Unchanged Unchanged (SetTo task.id) Nothing Nothing Nothing Unchanged) >>= (`shouldSatisfy` maybe False (const True))
      addDependency env.pool task.id dependency.id

      projectBatch <- projectCardSummaries env.pool [child, project]
      taskBatch <- taskCardSummaries env.pool [subtask, task]
      projectSingles <- traverse (projectCardSummary env.pool) [child, project]
      taskSingles <- traverse (taskCardSummary env.pool) [subtask, task]
      projectBatch `shouldBe` projectSingles
      taskBatch `shouldBe` taskSingles

    it "retains a paused low-priority ancestor for an exact literal descendant filter" $ \env -> do
      workspace <- createTestWorkspace env "overview-filtered-project-ancestor"
      retainedRoot <- createProject env.pool CreateProject
        { workspaceId = workspace.id, parentId = Nothing, name = "retained root", description = Nothing
        , priority = Just 1, metadata = Nothing }
      _ <- updateProject env.pool retainedRoot.id UpdateProject
        { name = Nothing, description = Unchanged, parentId = Unchanged, status = Just ProjPaused, priority = Nothing, metadata = Nothing }
      _matchingChild <- createProject env.pool CreateProject
        { workspaceId = workspace.id, parentId = Just retainedRoot.id, name = "literal_100%", description = Nothing
        , priority = Just 10, metadata = Nothing }
      wildcardOnlyRoot <- createProject env.pool CreateProject
        { workspaceId = workspace.id, parentId = Nothing, name = "wildcard root", description = Nothing
        , priority = Just 1, metadata = Nothing }
      _wildcardOnlyChild <- createProject env.pool CreateProject
        { workspaceId = workspace.id, parentId = Just wildcardOnlyRoot.id, name = "literalA100Z", description = Nothing
        , priority = Just 10, metadata = Nothing }

      roots <- listFilteredProjectChildren env.pool workspace.id Nothing NavigationFilter
        { showOnly = Nothing, projectStatuses = [ProjActive], taskStatuses = []
        , priorityMode = Just "exact", priorityValue = Just 10, query = Just "literal_100%" }
        50 0

      map (.id) roots `shouldBe` [retainedRoot.id]

    it "orders bounded dependency pages by name with deterministic pagination" $ \env -> do
      workspace <- createTestWorkspace env "overview-dependency-page-order"
      task <- createTask env.pool (newTask workspace.id Nothing "dependent")
      zeta <- createTask env.pool (newTask workspace.id Nothing "zeta")
      alpha <- createTask env.pool (newTask workspace.id Nothing "alpha")
      addDependency env.pool task.id zeta.id
      addDependency env.pool task.id alpha.id
      first <- listTaskDependencyPage env.pool task.id 1 0
      second <- listTaskDependencyPage env.pool task.id 1 1
      map (.id) first.items `shouldBe` [alpha.id]
      first.hasMore `shouldBe` True
      map (.id) second.items `shouldBe` [zeta.id]
      second.hasMore `shouldBe` False
      cappedDependencies <- mapM
        (\index -> createTask env.pool (newTask workspace.id Nothing ("zz-capped-" <> Text.pack (show index))))
        [1 .. maxNavigationPageSize + 1]
      mapM_ (addDependency env.pool task.id . (.id)) cappedDependencies
      capped <- listTaskDependencyPage env.pool task.id (maxNavigationPageSize + 1) 0
      length capped.items `shouldBe` maxNavigationPageSize
      capped.hasMore `shouldBe` True

    it "uses id tie-breakers for project and task navigation pages" $ \env -> do
      workspace <- createTestWorkspace env "overview-navigation-stable-ties"
      firstProject <- createProject env.pool CreateProject
        { workspaceId = workspace.id, parentId = Nothing, name = "same", description = Nothing
        , priority = Nothing, metadata = Nothing }
      secondProject <- createProject env.pool CreateProject
        { workspaceId = workspace.id, parentId = Nothing, name = "same", description = Nothing
        , priority = Nothing, metadata = Nothing }
      firstTask <- createTask env.pool (newTask workspace.id Nothing "same")
      secondTask <- createTask env.pool (newTask workspace.id Nothing "same")
      projectFirstPage <- listFilteredProjectChildren env.pool workspace.id Nothing emptyNavigationFilter 1 0
      projectSecondPage <- listFilteredProjectChildren env.pool workspace.id Nothing emptyNavigationFilter 1 1
      taskFirstPage <- listFilteredTaskChildren env.pool workspace.id Nothing Nothing emptyNavigationFilter 1 0
      taskSecondPage <- listFilteredTaskChildren env.pool workspace.id Nothing Nothing emptyNavigationFilter 1 1
      map (.id) (projectFirstPage <> projectSecondPage) `shouldBe` sort [firstProject.id, secondProject.id]
      map (.id) (taskFirstPage <> taskSecondPage) `shouldBe` sort [firstTask.id, secondTask.id]

    it "truncates project focus ancestors at the declared bound in root-to-parent order" $ \env -> do
      workspace <- createTestWorkspace env "overview-navigation-focus-truncation"
      root <- createProject env.pool CreateProject
        { workspaceId = workspace.id, parentId = Nothing, name = "root", description = Nothing
        , priority = Nothing, metadata = Nothing }
      let createChain parent remaining
            | remaining == 0 = pure parent
            | otherwise = do
                child <- createProject env.pool CreateProject
                  { workspaceId = workspace.id, parentId = Just parent.id
                  , name = "ancestor", description = Nothing, priority = Nothing, metadata = Nothing }
                createChain child (remaining - 1)
      target <- createChain root (65 :: Int)
      complete <- getProjectAncestorIds env.pool workspace.id target.id (maxFocusAncestors + 1)
      bounded <- getProjectAncestorIds env.pool workspace.id target.id maxFocusAncestors
      length complete `shouldBe` maxFocusAncestors + 1
      bounded `shouldBe` take maxFocusAncestors complete
      bounded `shouldBe` [root.id] <> take (maxFocusAncestors - 1) (drop 1 complete)

newTask :: UUID -> Maybe UUID -> Text -> CreateTask
newTask workspace project title = CreateTask
  { workspaceId = workspace, projectId = project, parentId = Nothing, title = title
  , description = Nothing, priority = Nothing, metadata = Nothing, dueAt = Nothing }

emptyNavigationFilter :: NavigationFilter
emptyNavigationFilter = NavigationFilter
  { showOnly = Nothing, projectStatuses = [], taskStatuses = []
  , priorityMode = Nothing, priorityValue = Nothing, query = Nothing }

canonicalSha :: Text
canonicalSha = "0123456789abcdef0123456789abcdef01234567"
