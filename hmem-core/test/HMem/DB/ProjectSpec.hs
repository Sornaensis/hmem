{-# OPTIONS_GHC -Wno-x-partial -Wno-incomplete-uni-patterns #-}

module HMem.DB.ProjectSpec (spec) where

import Data.Maybe (isJust, isNothing)
import Test.Hspec

import HMem.DB.Memory (createMemory)
import HMem.DB.Project
import HMem.DB.Task (createTask, getTask, listTasksByWorkspace)
import HMem.DB.TestHarness
import HMem.Types

spec :: Spec
spec = around withTestEnv $ do

  describe "createProject / getProject" $ do
    it "creates and retrieves a project" $ \env -> do
      ws <- createTestWorkspace env "proj-ws"
      let cp = CreateProject
            { workspaceId = ws.id
            , parentId    = Nothing
            , name        = "Test Project"
            , description = Just "A test project"
            , priority    = Just 8
            , metadata    = Nothing
            }
      proj <- createProject env.pool cp
      proj.name `shouldBe` "Test Project"
      proj.priority `shouldBe` 8
      proj.status `shouldBe` ProjActive  -- default

      got <- getProject env.pool proj.id
      got `shouldSatisfy` isJust
      let Just p = got
      p.name `shouldBe` "Test Project"
      p.description `shouldBe` Just "A test project"

    it "returns Nothing for nonexistent ID" $ \env -> do
      got <- getProject env.pool (read "00000000-0000-0000-0000-000000000099")
      got `shouldSatisfy` isNothing

    it "defaults priority to 5" $ \env -> do
      ws <- createTestWorkspace env "projdef-ws"
      proj <- createProject env.pool CreateProject
        { workspaceId = ws.id, parentId = Nothing, name = "default pri"
        , description = Nothing, priority = Nothing, metadata = Nothing }
      proj.priority `shouldBe` 5

  describe "updateProject" $ do
    it "updates name and status" $ \env -> do
      ws <- createTestWorkspace env "projup-ws"
      proj <- createProject env.pool CreateProject
        { workspaceId = ws.id, parentId = Nothing, name = "Original"
        , description = Nothing, priority = Nothing, metadata = Nothing }
      updated <- updateProject env.pool proj.id UpdateProject
        { name = Just "Renamed", description = Unchanged
        , status = Just ProjPaused, priority = Nothing, metadata = Nothing }
      updated `shouldSatisfy` isJust
      let Just u = updated
      u.name `shouldBe` "Renamed"
      u.status `shouldBe` ProjPaused

    it "preserves unchanged fields" $ \env -> do
      ws <- createTestWorkspace env "projpres-ws"
      proj <- createProject env.pool CreateProject
        { workspaceId = ws.id, parentId = Nothing, name = "Keep"
        , description = Just "keep this", priority = Just 7, metadata = Nothing }
      updated <- updateProject env.pool proj.id UpdateProject
        { name = Nothing, description = Unchanged
        , status = Nothing, priority = Nothing, metadata = Nothing }
      let Just u = updated
      u.name `shouldBe` "Keep"
      u.description `shouldBe` Just "keep this"
      u.priority `shouldBe` 7

    it "returns Nothing for nonexistent ID" $ \env -> do
      result <- updateProject env.pool (read "00000000-0000-0000-0000-000000000099") UpdateProject
        { name = Just "x", description = Unchanged, status = Nothing
        , priority = Nothing, metadata = Nothing }
      result `shouldSatisfy` isNothing

  describe "deleteProject" $ do
    it "deletes an existing project" $ \env -> do
      ws <- createTestWorkspace env "projdel-ws"
      proj <- createProject env.pool CreateProject
        { workspaceId = ws.id, parentId = Nothing, name = "Doomed"
        , description = Nothing, priority = Nothing, metadata = Nothing }
      ok <- deleteProject env.pool proj.id
      ok `shouldBe` True
      got <- getProject env.pool proj.id
      got `shouldSatisfy` isNothing

    it "returns False for nonexistent ID" $ \env -> do
      ok <- deleteProject env.pool (read "00000000-0000-0000-0000-000000000099")
      ok `shouldBe` False

    it "keeps tasks by clearing their project_id" $ \env -> do
      ws <- createTestWorkspace env "projtask-ws"
      proj <- createProject env.pool CreateProject
        { workspaceId = ws.id, parentId = Nothing, name = "To Archive"
        , description = Nothing, priority = Nothing, metadata = Nothing }
      task <- createTask env.pool CreateTask
        { workspaceId = ws.id, projectId = Just proj.id, parentId = Nothing, title = "Keep me"
        , description = Nothing, priority = Nothing, metadata = Nothing, dueAt = Nothing }

      ok <- deleteProject env.pool proj.id
      ok `shouldBe` True

      got <- getTask env.pool task.id
      got `shouldSatisfy` isJust
      let Just detached = got
      detached.projectId `shouldBe` Nothing

      tasks <- listTasksByWorkspace env.pool ws.id Nothing Nothing Nothing Nothing
      map (.id) tasks `shouldContain` [task.id]

  describe "listProjects" $ do
    it "lists projects for a workspace" $ \env -> do
      ws <- createTestWorkspace env "projlist-ws"
      _ <- createProject env.pool CreateProject
        { workspaceId = ws.id, parentId = Nothing, name = "P1"
        , description = Nothing, priority = Just 3, metadata = Nothing }
      _ <- createProject env.pool CreateProject
        { workspaceId = ws.id, parentId = Nothing, name = "P2"
        , description = Nothing, priority = Just 9, metadata = Nothing }
      projs <- listProjects env.pool ws.id Nothing Nothing Nothing
      length projs `shouldBe` 2
      -- Should be ordered by priority DESC
      (head projs).priority `shouldBe` 9

    it "filters by status" $ \env -> do
      ws <- createTestWorkspace env "projfilt-ws"
      p1 <- createProject env.pool CreateProject
        { workspaceId = ws.id, parentId = Nothing, name = "Active"
        , description = Nothing, priority = Nothing, metadata = Nothing }
      _ <- updateProject env.pool p1.id UpdateProject
        { name = Nothing, description = Unchanged, status = Just ProjCompleted
        , priority = Nothing, metadata = Nothing }
      _ <- createProject env.pool CreateProject
        { workspaceId = ws.id, parentId = Nothing, name = "StillActive"
        , description = Nothing, priority = Nothing, metadata = Nothing }
      active <- listProjects env.pool ws.id (Just ProjActive) Nothing Nothing
      length active `shouldBe` 1
      (head active).name `shouldBe` "StillActive"

  describe "memory links" $ do
    it "links and unlinks project to memory" $ \env -> do
      ws <- createTestWorkspace env "projmem-ws"
      proj <- createProject env.pool CreateProject
        { workspaceId = ws.id, parentId = Nothing, name = "Linked Proj"
        , description = Nothing, priority = Nothing, metadata = Nothing }
      mem <- createMemory env.pool CreateMemory
        { workspaceId = ws.id, content = "linked memory", summary = Nothing
        , memoryType = ShortTerm, importance = Nothing, metadata = Nothing
        , expiresAt = Nothing, source = Nothing, confidence = Nothing, pinned = Nothing, tags = Nothing, ftsLanguage = Nothing }
      -- Link should not throw
      linkProjectMemory env.pool proj.id mem.id
      -- Linking again should be idempotent (DoNothing)
      linkProjectMemory env.pool proj.id mem.id
      -- Unlink
      unlinkProjectMemory env.pool proj.id mem.id
