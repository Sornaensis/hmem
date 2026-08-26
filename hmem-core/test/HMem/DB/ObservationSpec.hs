module HMem.DB.ObservationSpec (spec) where

import Control.Exception (try)
import Data.List (sortBy)
import Data.Maybe (isJust, isNothing)
import Data.Text qualified as T
import Data.UUID (UUID)
import Test.Hspec
import Hasql.Session qualified as Session

import HMem.DB.Observation
import HMem.DB.Pool (DBException(..), checkPgvector, runSession)
import HMem.DB.TestHarness
import HMem.Types

spec :: Spec
spec = beforeAll setupTestPool $ aroundWith withTestTransaction $ do
  describe "Observation CRUD" $ do
    it "round-trips file and glob observations and permits duplicate provenance" $ \env -> do
      workspace <- createTestWorkspace env "observation-crud"
      file <- createObservation env.pool (newObservation workspace.id SubjectFile "src/Main.hs" "first")
      glob <- createObservation env.pool (newObservation workspace.id SubjectGlob "src/**/*.hs" "second")
      duplicate <- createObservation env.pool (newObservation workspace.id SubjectFile "src/Main.hs" "third")
      file.subjectKind `shouldBe` SubjectFile
      glob.subjectKind `shouldBe` SubjectGlob
      duplicate.id `shouldNotBe` file.id
      getObservation env.pool workspace.id file.id >>= (`shouldSatisfy` isJust)

    it "isolates reads, updates, hard deletes, and lists by workspace" $ \env -> do
      owner <- createTestWorkspace env "observation-owner"
      outsider <- createTestWorkspace env "observation-outsider"
      created <- createObservation env.pool (newObservation owner.id SubjectFile "src/Main.hs" "owned")
      outsiderOwned <- createObservation env.pool (newObservation outsider.id SubjectFile "src/Outsider.hs" "outsider")
      getObservation env.pool outsider.id created.id `shouldReturn` Nothing
      updateObservation env.pool outsider.id created.id (UpdateObservation "stolen") `shouldReturn` Nothing
      deleteObservation env.pool outsider.id created.id `shouldReturn` False
      getObservation env.pool owner.id created.id >>= (`shouldSatisfy` isJust)
      outsiderRows <- listObservations env.pool (observationQuery outsider.id Nothing Nothing Nothing)
      map (.id) outsiderRows `shouldBe` [outsiderOwned.id]

    it "updates content and hard-deletes only within the owning workspace" $ \env -> do
      workspace <- createTestWorkspace env "observation-update"
      created <- createObservation env.pool (newObservation workspace.id SubjectFile "src/Main.hs" "before")
      updated <- updateObservation env.pool workspace.id created.id (UpdateObservation "after")
      fmap (.content) updated `shouldBe` Just "after"
      deleteObservation env.pool workspace.id created.id `shouldReturn` True
      getObservation env.pool workspace.id created.id >>= (`shouldSatisfy` isNothing)
      deleteObservation env.pool workspace.id created.id `shouldReturn` False

    it "requires an active repository workspace atomically at creation" $ \env -> do
      planning <- createTestWorkspace env "observation-planning-workspace"
      deleted <- createTestWorkspace env "observation-deleted-workspace"
      runSession env.pool $ Session.sql "UPDATE workspaces SET workspace_type = 'planning' WHERE name = 'observation-planning-workspace'"
      runSession env.pool $ Session.sql "UPDATE workspaces SET deleted_at = now() WHERE name = 'observation-deleted-workspace'"
      assertRejectedCreate env planning.id
      assertRejectedCreate env deleted.id

  describe "Observation queries" $ do
    it "orders equal-rank FTS ties by recency/id and paginates unranked and ranked lists" $ \env -> do
      workspace <- createTestWorkspace env "observation-query"
      newerRank <- createObservation env.pool (newObservation workspace.id SubjectFile "src/New.hs" "needle needle alpha")
      lowerRank <- createObservation env.pool (newObservation workspace.id SubjectFile "src/Lower.hs" "needle beta")
      tieA <- createObservation env.pool (newObservation workspace.id SubjectFile "src/TieA.hs" "equalrank")
      tieB <- createObservation env.pool (newObservation workspace.id SubjectFile "src/TieB.hs" "equalrank")
      tied <- listObservations env.pool (observationQuery workspace.id (Just "equalrank") Nothing Nothing)
      map (.id) tied `shouldBe` sortBy (flip compare) [tieA.id, tieB.id]
      tiedPage <- listObservations env.pool (observationQuery workspace.id (Just "equalrank") (Just 1) (Just 1))
      map (.id) tiedPage `shouldBe` [minimum [tieA.id, tieB.id]]
      ranked <- listObservations env.pool (observationQuery workspace.id (Just "needle") (Just 2) (Just 0))
      map (.id) ranked `shouldBe` [newerRank.id, lowerRank.id]
      rankedPage <- listObservations env.pool (observationQuery workspace.id (Just "needle") (Just 1) (Just 1))
      map (.id) rankedPage `shouldBe` [lowerRank.id]
      unranked <- listObservations env.pool (observationQuery workspace.id Nothing Nothing Nothing)
      map (.id) unranked `shouldBe` sortBy (flip compare) [newerRank.id, lowerRank.id, tieA.id, tieB.id]
      unrankedFirst <- listObservations env.pool (observationQuery workspace.id Nothing (Just 1) (Just 0))
      unrankedSecond <- listObservations env.pool (observationQuery workspace.id Nothing (Just 1) (Just 1))
      map (.id) unrankedFirst `shouldBe` take 1 (map (.id) unranked)
      map (.id) unrankedSecond `shouldBe` take 1 (drop 1 (map (.id) unranked))

    it "composes every exact filter with FTS in its workspace" $ \env -> do
      workspace <- createTestWorkspace env "observation-exact-fts"
      target <- createObservation env.pool (newObservation workspace.id SubjectFile "src/Main.hs" "needle target")
      kindDistractor <- createObservation env.pool (newObservation workspace.id SubjectGlob "src/**/*.hs" "needle kind")
      subjectDistractor <- createObservation env.pool (newObservation workspace.id SubjectFile "src/Other.hs" "needle subject")
      shaDistractor <- createObservation env.pool (newObservationWithSha workspace.id SubjectFile "src/Main.hs" alternateSha "needle sha")
      listIds env (queryWith workspace.id (Just SubjectGlob) Nothing Nothing) `shouldReturn` [kindDistractor.id]
      listIds env (queryWith workspace.id Nothing (Just "src/Other.hs") Nothing) `shouldReturn` [subjectDistractor.id]
      listIds env (queryWith workspace.id Nothing Nothing (Just alternateSha)) `shouldReturn` [shaDistractor.id]
      listIds env (queryWith workspace.id (Just SubjectFile) (Just "src/Main.hs") (Just canonicalSha)) `shouldReturn` [target.id]

  describe "Observation embeddings" $ do
    it "reports an explicit capability error on the verified pgvector-absent path" $ \env -> do
      workspace <- createTestWorkspace env "observation-vector-absent"
      created <- createObservation env.pool (newObservation workspace.id SubjectFile "src/Main.hs" "content")
      -- Force the absent capability state even on pgvector-enabled CI images.
      -- The surrounding transaction rolls this DDL back before the present-path example.
      runSession env.pool $ Session.sql "DROP INDEX IF EXISTS idx_observations_embedding; ALTER TABLE observations DROP COLUMN IF EXISTS embedding; DROP EXTENSION IF EXISTS vector"
      checkPgvector env.pool `shouldReturn` False
      setResult <- try @DBException $ setObservationEmbedding env.pool workspace.id created.id unitX
      setResult `shouldSatisfy` isCapabilityUnavailable
      searchResult <- try @DBException $ similarObservations env.pool (similarQuery workspace.id Nothing Nothing Nothing unitX Nothing Nothing Nothing)
      searchResult `shouldSatisfy` isCapabilityUnavailable

    it "isolates embedding mutation and vector search, and applies exact filters, thresholds, ties, and pagination when pgvector is present" $ \env -> do
      owner <- createTestWorkspace env "observation-vector-owner"
      outsider <- createTestWorkspace env "observation-vector-outsider"
      available <- checkPgvector env.pool
      if not available
        then pendingWith "pgvector is not installed; the present-capability path is exercised on pgvector-enabled environments"
        else do
          target <- createObservation env.pool (newObservation owner.id SubjectFile "src/Main.hs" "target")
          kindDistractor <- createObservation env.pool (newObservation owner.id SubjectGlob "src/**/*.hs" "kind")
          subjectDistractor <- createObservation env.pool (newObservation owner.id SubjectFile "src/Other.hs" "subject")
          shaDistractor <- createObservation env.pool (newObservationWithSha owner.id SubjectFile "src/Main.hs" alternateSha "sha")
          thresholdExcluded <- createObservation env.pool (newObservation owner.id SubjectFile "src/Excluded.hs" "excluded")
          tieA <- createObservation env.pool (newObservation owner.id SubjectFile "src/TieA.hs" "tie")
          tieB <- createObservation env.pool (newObservation owner.id SubjectFile "src/TieB.hs" "tie")
          outsiderObservation <- createObservation env.pool (newObservation outsider.id SubjectFile "src/Outsider.hs" "outsider")
          mapM_ (uncurry (setObservationEmbedding env.pool owner.id))
            [ (target.id, unitX), (kindDistractor.id, unitX), (subjectDistractor.id, unitX)
            , (shaDistractor.id, unitX), (thresholdExcluded.id, unitY), (tieA.id, unitY), (tieB.id, unitY)
            ]
          setObservationEmbedding env.pool owner.id outsiderObservation.id unitX
          similarIds env (similarQuery outsider.id Nothing Nothing Nothing unitX Nothing Nothing Nothing) `shouldReturn` []
          setObservationEmbedding env.pool outsider.id outsiderObservation.id unitX
          similarIds env (similarQuery owner.id (Just SubjectGlob) Nothing Nothing unitX Nothing Nothing Nothing) `shouldReturn` [kindDistractor.id]
          similarIds env (similarQuery owner.id Nothing (Just "src/Other.hs") Nothing unitX Nothing Nothing Nothing) `shouldReturn` [subjectDistractor.id]
          similarIds env (similarQuery owner.id Nothing Nothing (Just alternateSha) unitX Nothing Nothing Nothing) `shouldReturn` [shaDistractor.id]
          similarIds env (similarQuery owner.id (Just SubjectFile) (Just "src/Main.hs") (Just canonicalSha) unitX Nothing Nothing Nothing) `shouldReturn` [target.id]
          similarIds env (similarQuery owner.id (Just SubjectFile) (Just "src/Main.hs") (Just canonicalSha) unitX (Just 1) Nothing Nothing) `shouldReturn` [target.id]
          similarIds env (similarQuery owner.id (Just SubjectFile) (Just "src/Excluded.hs") Nothing unitX (Just 0.1) Nothing Nothing) `shouldReturn` []
          let equalSimilarity = sortBy (flip compare) [thresholdExcluded.id, tieA.id, tieB.id]
          tieIds <- similarIds env (similarQuery owner.id (Just SubjectFile) Nothing Nothing unitY (Just 1) Nothing Nothing)
          tieIds `shouldBe` equalSimilarity
          tiePage <- similarIds env (similarQuery owner.id (Just SubjectFile) Nothing Nothing unitY (Just 1) (Just 1) (Just 1))
          tiePage `shouldBe` [equalSimilarity !! 1]

assertRejectedCreate :: TestEnv -> UUID -> IO ()
assertRejectedCreate env workspace = do
  result <- try @DBException $ createObservation env.pool (newObservation workspace SubjectFile "src/Main.hs" "content")
  case result of
    Left (DBCheckViolation _) -> pure ()
    other -> expectationFailure $ "Expected DBCheckViolation, got: " <> show other

listIds :: TestEnv -> ObservationQuery -> IO [UUID]
listIds env = fmap (map (.id)) . listObservations env.pool

similarIds :: TestEnv -> SimilarObservationQuery -> IO [UUID]
similarIds env = fmap (map (.observation.id)) . similarObservations env.pool

isCapabilityUnavailable :: Either DBException a -> Bool
isCapabilityUnavailable (Left (DBCapabilityUnavailable _)) = True
isCapabilityUnavailable _ = False

newObservation :: UUID -> SubjectKind -> T.Text -> T.Text -> CreateObservation
newObservation workspace kind path body = newObservationWithSha workspace kind path canonicalSha body

newObservationWithSha :: UUID -> SubjectKind -> T.Text -> T.Text -> T.Text -> CreateObservation
newObservationWithSha workspace kind path sha body = CreateObservation
  { workspaceId = workspace, subjectKind = kind, subject = path, gitSha = sha, content = body }

observationQuery :: UUID -> Maybe T.Text -> Maybe Int -> Maybe Int -> ObservationQuery
observationQuery workspace searchTerm pageLimit pageOffset = ObservationQuery
  { workspaceId = workspace, subjectKind = Nothing, subject = Nothing, gitSha = Nothing
  , query = searchTerm, limit = pageLimit, offset = pageOffset }

queryWith :: UUID -> Maybe SubjectKind -> Maybe T.Text -> Maybe T.Text -> ObservationQuery
queryWith workspace kind path sha = ObservationQuery
  { workspaceId = workspace, subjectKind = kind, subject = path, gitSha = sha
  , query = Just "needle", limit = Nothing, offset = Nothing }

similarQuery :: UUID -> Maybe SubjectKind -> Maybe T.Text -> Maybe T.Text -> [Double] -> Maybe Double -> Maybe Int -> Maybe Int -> SimilarObservationQuery
similarQuery workspace kind path sha vector threshold pageLimit pageOffset = SimilarObservationQuery
  { workspaceId = workspace, subjectKind = kind, subject = path, gitSha = sha
  , embedding = vector, minSimilarity = threshold, limit = pageLimit, offset = pageOffset }

unitX, unitY :: [Double]
unitX = 1 : replicate (observationEmbeddingDimensions - 1) 0
unitY = 0 : 1 : replicate (observationEmbeddingDimensions - 2) 0

canonicalSha, alternateSha :: T.Text
canonicalSha = "0123456789abcdef0123456789abcdef01234567"
alternateSha = "fedcba9876543210fedcba9876543210fedcba98"
