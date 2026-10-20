module HMem.DB.ObservationDiscoverySpec (spec) where

import Control.Exception (try)
import Data.List (sort)
import Data.Text (Text)
import Data.Text qualified as T
import Data.UUID (UUID)
import Hasql.Session qualified as Session
import Test.Hspec

import HMem.DB.Embedding qualified as Embedding
import HMem.DB.Observation
import HMem.DB.ObservationFixture (reviewed, writeObservation)
import HMem.DB.Pool (DBException, runSession)
import HMem.DB.TestHarness
import HMem.Types

spec :: Spec
spec = beforeAll setupTestPool $ aroundWith withTestTransaction $ describe "Observation discovery retrieval" $ do
  it "composes subject and content independently, with literal case-sensitive subject text" $ \env -> do
    workspace <- createTestWorkspace env "discovery-and"
    subjectOnly <- createObservation env.pool (input workspace.id [ObservationSubject SubjectFile "src/needle.hs"] "ordinary")
    both <- createObservation env.pool (input workspace.id [ObservationSubject SubjectFile "src/%_Name.hs"] "needle")
    contentOnly <- createObservation env.pool (input workspace.id [ObservationSubject SubjectFile "docs/Guide.md"] "needle")
    let run f = discoverObservations env.pool f (request workspace.id f) Nothing
    allRows <- run emptyFilter
    map (.id) allRows.items `shouldMatchList` [subjectOnly.id,both.id,contentOnly.id]
    bodyRows <- run emptyFilter { contentQuery = Just "needle" }
    map (.id) bodyRows.items `shouldMatchList` [both.id,contentOnly.id]
    subjectRows <- run emptyFilter { subjectsText = Just "src/" }
    map (.id) subjectRows.items `shouldMatchList` [subjectOnly.id,both.id]
    joined <- run emptyFilter { subjectsText = Just "%_", contentQuery = Just "needle" }
    map (.id) joined.items `shouldBe` [both.id]
    mismatch <- run emptyFilter { subjectsText = Just "%_name", contentQuery = Just "needle" }
    mismatch.items `shouldBe` []

  it "shares complete distinct membership across typed tree terminals, list pages and counts" $ \env -> do
    workspace <- createTestWorkspace env "discovery-tree"
    multi <- createObservation env.pool (input workspace.id
      [ObservationSubject SubjectFile "src",ObservationSubject SubjectGlob "src",ObservationSubject SubjectFile "src/A.hs",ObservationSubject SubjectGlob "src/**/*.hs"] "needle")
    another <- createObservation env.pool (input workspace.id [ObservationSubject SubjectFile "src/A.hs"] "needle")
    _ <- createObservation env.pool (input workspace.id [ObservationSubject SubjectFile "src/Hidden.hs"] "ordinary")
    let f = emptyFilter { subjectsText = Just "src", contentQuery = Just "needle" }
        base = request workspace.id f
    first <- discoverObservations env.pool f (base :: ObservationDiscoveryRequest) { limit = Just 1 } Nothing
    second <- discoverObservations env.pool f (base :: ObservationDiscoveryRequest) { limit = Just 1,offset = Just 1 } Nothing
    (first.hasMore,second.hasMore) `shouldBe` (True,False)
    sort (map (.id) (first.items <> second.items)) `shouldBe` sort [multi.id,another.id]
    first.counts `shouldBe` ObservationCounts workspace.id 3 2
    counted <- discoverObservations env.pool f base { view = DiscoveryCount } Nothing
    (counted.items,counted.counts) `shouldBe` ([],first.counts)
    root <- discoverObservations env.pool f base { view = DiscoveryTree } Nothing
    case root.tree of
      Just tree -> do
        (tree.totalCount,tree.matchCount) `shouldBe` (3,2)
        map (.path) tree.items `shouldBe` ["src"]
        map (\t -> (t.subjectKind,t.observationCount)) (concatMap (.terminals) tree.items)
          `shouldBe` [(SubjectFile,1),(SubjectGlob,1)]
      Nothing -> expectationFailure "missing tree"
    leaf <- discoverObservations env.pool f base { terminal = Just (ObservationSubject SubjectGlob "src") } Nothing
    map (.id) leaf.items `shouldBe` [multi.id]
    children <- discoverObservations env.pool f base { view = DiscoveryTree,treePrefix = "src" } Nothing
    fmap (map (.path) . (.items)) children.tree `shouldBe` Just ["src/**","src/A.hs"]

  it "uses one subject row for kind, exact subject and substring, with every SHA predicate ANDed" $ \env -> do
    workspace <- createTestWorkspace env "discovery-provenance"
    cross <- createObservation env.pool (input workspace.id [ObservationSubject SubjectGlob "src/Exact.hs",ObservationSubject SubjectFile "docs/Other.hs"] "needle")
    kept <- createObservation env.pool (input workspace.id [ObservationSubject SubjectFile "src/Exact.hs"] "needle")
    _ <- writeObservation env.pool workspace.id kept.id (reviewed "needle")
    let f = emptyFilter { subjectKind = Just SubjectFile,subject = Just "src/Exact.hs",subjectsText = Just "src/",contentQuery = Just "needle",gitSha = Just shaA,currentGitSha = Just shaB,historyGitSha = Just shaA }
        run value = discoverObservations env.pool value (request workspace.id value) Nothing
    result <- run f
    map (.id) result.items `shouldBe` [kept.id]
    map (.id) result.items `shouldNotSatisfy` elem cross.id
    mapM_ (\value -> run value >>= \page -> page.items `shouldBe` [])
      [(f :: ObservationDiscoveryFilter) { gitSha = Just shaB },(f :: ObservationDiscoveryFilter) { currentGitSha = Just shaA },(f :: ObservationDiscoveryFilter) { historyGitSha = Just (T.replicate 40 "c") },f { subjectsText = Just "docs" }]

  it "executes FTS with the extension and embedding column absent" $ \env -> do
    runSession env.pool $ Session.sql "DROP INDEX IF EXISTS idx_observations_embedding; ALTER TABLE observations DROP COLUMN IF EXISTS embedding; DROP EXTENSION IF EXISTS vector"
    workspace <- createTestWorkspace env "discovery-no-vector"
    row <- createObservation env.pool (input workspace.id [ObservationSubject SubjectFile "src/Main.hs"] "needle")
    let f = emptyFilter { contentQuery = Just "needle" }
    result <- discoverObservations env.pool f (request workspace.id f) Nothing
    map (.id) result.items `shouldBe` [row.id]

  it "executes FTS when the extension exists but the optional column is absent" $ \env -> do
    provisionVectors env
    runSession env.pool $ Session.sql "DROP INDEX IF EXISTS idx_observations_embedding; ALTER TABLE observations DROP COLUMN IF EXISTS embedding"
    workspace <- createTestWorkspace env "discovery-no-column"
    row <- createObservation env.pool (input workspace.id [ObservationSubject SubjectFile "src/Main.hs"] "needle")
    let f = emptyFilter { contentQuery = Just "needle" }
    result <- discoverObservations env.pool f (request workspace.id f) Nothing
    map (.id) result.items `shouldBe` [row.id]

  it "unions simultaneous FTS-only, semantic-only and overlapping hits before ranking paging counts and tree pruning" $ \env -> do
    provisionVectors env
    workspace <- createTestWorkspace env "discovery-hybrid"
    foreignWorkspace <- createTestWorkspace env "discovery-foreign"
    ftsOnly <- createObservation env.pool (input workspace.id [ObservationSubject SubjectFile "src/fts.hs"] "needle needle needle")
    semanticOnly <- createObservation env.pool (input workspace.id [ObservationSubject SubjectFile "src/semantic.hs"] "other")
    overlap <- createObservation env.pool (input workspace.id [ObservationSubject SubjectFile "src/overlap.hs",ObservationSubject SubjectGlob "src/*.hs"] "needle")
    below <- createObservation env.pool (input workspace.id [ObservationSubject SubjectFile "src/below.hs"] "other")
    wrongSpace <- createObservation env.pool (input workspace.id [ObservationSubject SubjectFile "src/wrong.hs"] "other")
    foreignRow <- createObservation env.pool (input foreignWorkspace.id [ObservationSubject SubjectFile "src/foreign.hs"] "needle")
    let Just space = parseEmbeddingSpaceFingerprint "test:discovery:v1"
    mapM_ (\row -> Embedding.setObservationEmbeddingInSpace env.pool workspace.id row.id space unitX) [semanticOnly,overlap]
    Embedding.setObservationEmbeddingInSpace env.pool workspace.id below.id space unitY
    Embedding.setObservationEmbeddingInSpace env.pool workspace.id wrongSpace.id legacyManualEmbeddingSpace unitX
    Embedding.setObservationEmbeddingInSpace env.pool foreignWorkspace.id foreignRow.id space unitX
    let f = emptyFilter { subjectsText = Just "src/",contentQuery = Just "needle" }
        base = request workspace.id f
        run value = discoverObservations env.pool f value (Just (unitX,"test:discovery:v1"))
    result <- run base
    map (.id) result.items `shouldMatchList` [ftsOnly.id,semanticOnly.id,overlap.id]
    map (.id) (take 1 result.items) `shouldBe` [overlap.id]
    again <- run base
    map (.id) again.items `shouldBe` map (.id) result.items
    result.counts `shouldBe` ObservationCounts workspace.id 5 3
    first <- run (base :: ObservationDiscoveryRequest) { limit = Just 2 }
    second <- run (base :: ObservationDiscoveryRequest) { limit = Just 2,offset = Just 2 }
    map (.id) (first.items <> second.items) `shouldBe` map (.id) result.items
    (first.hasMore,second.hasMore) `shouldBe` (True,False)
    tree <- run base { view = DiscoveryTree,treePrefix = "src" }
    fmap (.matchCount) tree.tree `shouldBe` Just 3
    fmap (map (.path) . (.items)) tree.tree `shouldBe` Just ["src/*.hs","src/fts.hs","src/overlap.hs","src/semantic.hs"]
    leaf <- run base { terminal = Just (ObservationSubject SubjectFile "src/semantic.hs") }
    map (.id) leaf.items `shouldBe` [semanticOnly.id]

emptyFilter :: ObservationDiscoveryFilter
emptyFilter = ObservationDiscoveryFilter Nothing Nothing Nothing Nothing Nothing Nothing Nothing

request :: UUID -> ObservationDiscoveryFilter -> ObservationDiscoveryRequest
request workspace filters = ObservationDiscoveryRequest workspace Nothing Nothing Nothing Nothing (Just filters) DiscoveryList "" Nothing Nothing Nothing

input :: UUID -> [ObservationSubject] -> Text -> CreateObservation
input workspace subjects content = CreateObservation workspace subjects shaA content

shaA, shaB :: Text
shaA = T.replicate 40 "a"
shaB = T.replicate 40 "b"

unitX, unitY :: [Double]
unitX = 1 : replicate 1535 0
unitY = 0 : 1 : replicate 1534 0

-- Provision only inside the disposable test transaction. No installed package
-- or user database is changed. A missing optional package is a setup limitation.
provisionVectors :: TestEnv -> IO ()
provisionVectors env = do
  result <- try @DBException $ runSession env.pool $ Session.sql
    "CREATE EXTENSION IF NOT EXISTS vector; ALTER TABLE observations ADD COLUMN IF NOT EXISTS embedding vector(1536)"
  case result of
    Left _ -> pendingWith "optional pgvector PostgreSQL package unavailable in test sandbox"
    Right () -> pure ()
