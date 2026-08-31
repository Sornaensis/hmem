module HMem.TypesSpec (spec) where

import Data.Aeson (Value(Null), eitherDecode, encode, object, (.=))
import Data.Either (isLeft)
import Data.Text qualified as T
import Data.Time (UTCTime)
import Data.UUID (UUID)
import Test.Hspec

import HMem.Types
import HMem.ObservationSubjectMatchCorpus (observationSubjectMatchCorpus)

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
  describe "Bounded navigation validation" $ do
    it "enforces limit, offset, and combined unique summary-batch caps" $ do
      validateNavigationPage (Just 1) (Just 0) `shouldBe` []
      validateNavigationPage (Just 0) Nothing `shouldSatisfy` (not . null)
      validateNavigationPage (Just (maxNavigationPageSize + 1)) Nothing `shouldSatisfy` (not . null)
      validateNavigationPage Nothing (Just (-1)) `shouldSatisfy` (not . null)
      validateNavigationPage Nothing (Just (maxNavigationOffset + 1)) `shouldSatisfy` (not . null)
      validateNavigationSummariesRequest (NavigationSummariesRequest (replicate maxNavigationBatchIds workspace) []) `shouldSatisfy` (not . null)
      validateNavigationSummariesRequest (NavigationSummariesRequest (replicate (maxNavigationBatchIds + 1) workspace) []) `shouldSatisfy` (not . null)
  describe "Observation subject glob matching" $ do
    mapM_ assertCorpus observationSubjectMatchCorpus
    it "rejects non-canonical paths and embedded recursive globs" $ do
      validateObservationSubjects [ObservationSubject SubjectGlob "src/**.hs"] `shouldSatisfy` (not . null)
      validateObservationSubjects [ObservationSubject SubjectGlob "src/***/Main.hs"] `shouldSatisfy` (not . null)
      validateObservationSubjects [ObservationSubject SubjectGlob "src/"] `shouldSatisfy` (not . null)
    it "bounds subject and candidate path lengths before matching" $ do
      let tooLong = T.replicate 4097 "a"
      validateObservationSubjects [ObservationSubject SubjectGlob tooLong] `shouldSatisfy` (not . null)
      observationSubjectMatchesPath (ObservationSubject SubjectGlob "**") tooLong `shouldBe` False
  describe "Observation search hit JSON compatibility" $ do
    it "decodes the legacy singleton subject shape" $
      eitherDecode (encode legacySearchHit) `shouldBe` Right legacySearchHitValue
    it "decodes the canonical multi-subject shape" $
      eitherDecode (encode canonicalSearchHit) `shouldBe` Right searchHit
    it "prefers canonical subjects when both shapes are present" $
      eitherDecode (encode dualSearchHit) `shouldBe` Right searchHit
    it "emits canonical subjects and the legacy first subject" $
      eitherDecode (encode searchHit) `shouldBe` Right searchHit
  describe "Create observation JSON compatibility" $ do
    it "decodes canonical subjects without legacy fields" $
      eitherDecode (encode canonicalCreate) `shouldBe` Right create
    it "emits canonical subjects that round-trip" $
      eitherDecode (encode create) `shouldBe` Right create
    it "decodes the complete legacy singleton subject pair" $
      eitherDecode (encode legacyCreate) `shouldBe` Right legacyCreateValue
    it "rejects canonical subjects mixed with either legacy subject field" $ do
      eitherDecode (encode mixedCreateKind) `shouldSatisfy` (isLeft :: Either String CreateObservation -> Bool)
      eitherDecode (encode mixedCreateSubject) `shouldSatisfy` (isLeft :: Either String CreateObservation -> Bool)
    it "rejects partial or missing subject forms" $ do
      eitherDecode (encode missingSubjects) `shouldSatisfy` (isLeft :: Either String CreateObservation -> Bool)
      eitherDecode (encode partialLegacyKind) `shouldSatisfy` (isLeft :: Either String CreateObservation -> Bool)
      eitherDecode (encode partialLegacySubject) `shouldSatisfy` (isLeft :: Either String CreateObservation -> Bool)
  describe "Observation match JSON compatibility" $ do
    it "decodes legacy uncorrelated evidence without inventing path groups" $
      eitherDecode (encode legacyMatch) `shouldBe` Right matchValue { pathMatches = [] }
    it "round-trips canonical path-correlated evidence while retaining legacy arrays" $
      eitherDecode (encode matchValue) `shouldBe` Right matchValue
  where
    workspace = read "00000000-0000-0000-0000-000000000001" :: UUID
    observationSearch = UnifiedSearchQuery
      { workspaceId = Nothing, query = Nothing, entityTypes = Nothing, searchLanguage = Nothing
      , limit = Nothing, offset = Nothing, subjectKind = Nothing, subject = Nothing, gitSha = Nothing
      , projectStatus = Nothing, taskStatus = Nothing, taskPriority = Nothing, projectId = Nothing }
    savedView = CreateSavedView workspace "view" Nothing "activity" Null
    observedAt = read "2026-01-02 03:04:05 UTC" :: UTCTime
    observationValue = Observation workspace workspace create.subjects canonicalSha "body" observedAt observedAt
    matchValue = ObservationMatch
      { observation = observationValue
      , pathMatches =
          [ ObservationPathMatch "src/Main.hs" create.subjects
          , ObservationPathMatch "src/Other.hs" [ObservationSubject SubjectGlob "src/**/*.hs"]
          ]
      , matchedPaths = ["src/Main.hs", "src/Other.hs"]
      , matchedSubjects = create.subjects
      }
    legacyMatch = object
      [ "observation" .= observationValue
      , "matched_paths" .= matchValue.matchedPaths
      , "matched_subjects" .= matchValue.matchedSubjects
      ]
    searchHit = ObservationSearchHit workspace workspace
      [ObservationSubject SubjectFile "src/Main.hs", ObservationSubject SubjectGlob "src/**/*.hs"]
      "deadbeef" "preview" observedAt
    legacySearchHitValue = ObservationSearchHit workspace workspace
      [ObservationSubject SubjectFile "src/Main.hs"] "deadbeef" "preview" observedAt
    legacySearchHit = object
      [ "id" .= workspace, "workspace_id" .= workspace, "subject_kind" .= SubjectFile
      , "subject" .= ("src/Main.hs" :: String), "git_sha" .= ("deadbeef" :: String)
      , "content_preview" .= ("preview" :: String), "updated_at" .= observedAt ]
    canonicalSearchHit = object
      [ "id" .= workspace, "workspace_id" .= workspace, "subjects" .= searchHit.subjects
      , "git_sha" .= ("deadbeef" :: String), "content_preview" .= ("preview" :: String), "updated_at" .= observedAt ]
    dualSearchHit = object
      [ "id" .= workspace, "workspace_id" .= workspace, "subjects" .= searchHit.subjects
      , "subject_kind" .= SubjectGlob, "subject" .= ("ignored/**/*.hs" :: String)
      , "git_sha" .= ("deadbeef" :: String), "content_preview" .= ("preview" :: String), "updated_at" .= observedAt ]
    create = CreateObservation workspace [ObservationSubject SubjectFile "src/Main.hs", ObservationSubject SubjectGlob "src/**/*.hs"] canonicalSha "body"
    legacyCreateValue = CreateObservation workspace [ObservationSubject SubjectFile "src/Legacy.hs"] canonicalSha "body"
    canonicalCreate = object
      [ "workspace_id" .= workspace, "subjects" .= create.subjects
      , "git_sha" .= canonicalSha, "content" .= ("body" :: String) ]
    legacyCreate = object
      [ "workspace_id" .= workspace, "subject_kind" .= SubjectFile, "subject" .= ("src/Legacy.hs" :: String)
      , "git_sha" .= canonicalSha, "content" .= ("body" :: String) ]
    mixedCreateKind = object
      [ "workspace_id" .= workspace, "subjects" .= create.subjects, "subject_kind" .= SubjectFile
      , "git_sha" .= canonicalSha, "content" .= ("body" :: String) ]
    mixedCreateSubject = object
      [ "workspace_id" .= workspace, "subjects" .= create.subjects, "subject" .= ("src/Main.hs" :: String)
      , "git_sha" .= canonicalSha, "content" .= ("body" :: String) ]
    missingSubjects = object
      [ "workspace_id" .= workspace, "git_sha" .= canonicalSha, "content" .= ("body" :: String) ]
    partialLegacyKind = object
      [ "workspace_id" .= workspace, "subject_kind" .= SubjectFile
      , "git_sha" .= canonicalSha, "content" .= ("body" :: String) ]
    partialLegacySubject = object
      [ "workspace_id" .= workspace, "subject" .= ("src/Main.hs" :: String)
      , "git_sha" .= canonicalSha, "content" .= ("body" :: String) ]
    canonicalSha = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" :: T.Text

    assertCorpus (kind, pattern, path, expected) =
      it (show kind <> " " <> show pattern <> " matches " <> show path) $
        observationSubjectMatchesPath (ObservationSubject kind pattern) path `shouldBe` expected
