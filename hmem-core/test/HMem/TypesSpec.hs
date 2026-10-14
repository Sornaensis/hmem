module HMem.TypesSpec (spec) where

import Data.Aeson (Value(Null, Object), eitherDecode, encode, object, toJSON, (.=))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Either (isLeft)
import Data.Text qualified as T
import Data.Time (UTCTime)
import Data.UUID (UUID)
import Test.Hspec

import HMem.Types
import HMem.ObservationSubjectMatchCorpus (observationSubjectMatchCorpus)

spec :: Spec
spec = do
  describe "Due-free task JSON" $ do
    it "rejects all retired spellings including null on planning inputs, nested specs and batches" $ do
      let add key value (Object fields) = Object (KM.insert (Key.fromText key) value fields)
          add _ _ value = value
          task = CreateTask workspace Nothing Nothing "task" Nothing Nothing Nothing
          project = CreateProject workspace Nothing "project" Nothing Nothing Nothing
          specTask = ProjectSpecTask "task" Nothing Nothing
          projectSpec = CreateProjectSpec workspace "project" Nothing Nothing [specTask]
      mapM_ (\key -> mapM_ (\value -> do
        eitherDecode (encode (add key value (toJSON task))) `shouldSatisfy` (isLeft :: Either String CreateTask -> Bool)
        eitherDecode (encode (add key value (toJSON project))) `shouldSatisfy` (isLeft :: Either String CreateProject -> Bool)
        eitherDecode (encode (object [Key.fromText key .= value])) `shouldSatisfy` (isLeft :: Either String UpdateTask -> Bool)
        eitherDecode (encode (object [Key.fromText key .= value])) `shouldSatisfy` (isLeft :: Either String UpdateProject -> Bool)
        eitherDecode (encode (add key value (toJSON projectSpec))) `shouldSatisfy` (isLeft :: Either String CreateProjectSpec -> Bool)
        eitherDecode (encode (object ["workspace_id" .= workspace, "name" .= ("project" :: String), "tasks" .= [add key value (toJSON specTask)]])) `shouldSatisfy` (isLeft :: Either String CreateProjectSpec -> Bool)
        eitherDecode (encode (object ["items" .= [object ["id" .= workspace, Key.fromText key .= value]]])) `shouldSatisfy` (isLeft :: Either String BatchUpdateTaskRequest -> Bool)
        eitherDecode (encode (object ["items" .= [object ["id" .= workspace, Key.fromText key .= value]]])) `shouldSatisfy` (isLeft :: Either String BatchUpdateProjectRequest -> Bool)
        eitherDecode (encode (object ["items" .= ([] :: [Value]), Key.fromText key .= value])) `shouldSatisfy` (isLeft :: Either String BatchUpdateTaskRequest -> Bool)
        eitherDecode (encode (object ["items" .= ([] :: [Value]), Key.fromText key .= value])) `shouldSatisfy` (isLeft :: Either String BatchUpdateProjectRequest -> Bool)
        eitherDecode (encode (object ["task_ids" .= [workspace], Key.fromText key .= value])) `shouldSatisfy` (isLeft :: Either String BatchMoveTasksRequest -> Bool))
        [Null, toJSON ("2030-01-01T00:00:00Z" :: String)]) ["due_at", "dueAt", "due", "due_date", "dueDate"]

    it "preserves unrelated unknown keys, metadata and nullable update semantics" $ do
      eitherDecode "{\"description\":null,\"parent_id\":null,\"unrelated\":true,\"metadata\":{\"due_at\":null}}"
        `shouldBe` Right (UpdateTask Nothing SetNull Unchanged SetNull Nothing Nothing (Just (object ["due_at" .= Null])))
      eitherDecode "{\"description\":null,\"unrelated\":true}"
        `shouldBe` Right (UpdateProject Nothing SetNull Unchanged Nothing Nothing Nothing)

    it "round-trips tasks and create/update DTOs without a modeled due field" $ do
      let task = Task workspace workspace Nothing Nothing "task" Nothing Todo 5 (object []) Nothing 0 observedAt observedAt
          createTask = CreateTask workspace Nothing Nothing "task" Nothing Nothing Nothing
          updateTask = UpdateTask Nothing SetNull Unchanged SetNull Nothing Nothing Nothing
      eitherDecode (encode task) `shouldBe` Right task
      eitherDecode (encode createTask) `shouldBe` Right createTask
      eitherDecode (encode updateTask) `shouldBe` Right updateTask
      mapM_ (\value -> case value of
        Object fields -> KM.member "due_at" fields `shouldBe` False
        _ -> expectationFailure "Task DTO must encode an object") [toJSON task, toJSON createTask, toJSON updateTask]
      toJSON updateTask `shouldBe` object ["description" .= Null, "parent_id" .= Null]

  describe "Project pause JSON compatibility" $ do
    it "defaults new diagnostic fields when decoding older task responses" $ do
      let task = Task workspace workspace Nothing Nothing "task" Nothing Todo 5 (object []) Nothing 0 observedAt observedAt
          legacyNext = object ["task" .= task, "completion_gated" .= False, "open_descendant_count" .= (0 :: Int), "dependency_blocked" .= False, "open_dependency_count" .= (0 :: Int)]
          legacyChange = object ["task" .= task, "previous_status" .= Todo, "current_status" .= Blocked,
            "previous_auto_blocked" .= False, "auto_blocked" .= True, "previous_open_dependency_count" .= (0 :: Int),
            "open_dependency_count" .= (1 :: Int), "reason" .= ("blocked_by_open_dependencies" :: T.Text)]
      eitherDecode (encode legacyNext) `shouldBe` Right (NextTaskCandidate task False 0 False 0 False False)
      eitherDecode (encode legacyChange) `shouldBe` Right (TaskDependencyStatusChange task Todo Blocked False True 0 1 0 0 False False "blocked_by_open_dependencies")
    it "keeps plain Project decoders and older mutation responses compatible" $ do
      let project = Project workspace workspace Nothing "project" Nothing ProjPaused 5 (object []) observedAt observedAt
          empty = ProjectPauseEffects 0 0 [] [] False False False False
          mutation = ProjectMutationResult project empty
      eitherDecode (encode mutation) `shouldBe` Right project
      eitherDecode (encode project) `shouldBe` Right (ProjectMutationResult project empty)
      eitherDecode (encode mutation) `shouldBe` Right mutation

  describe "Timeline lifecycle JSON compatibility" $ do
    it "defaults additive action fields for older payloads and preserves distinct counts" $ do
      (eitherDecode "{\"created\":1,\"completed\":2,\"deleted\":3}" :: Either String TimelineBucketSeriesCounts)
        `shouldBe` Right (TimelineBucketSeriesCounts 1 2 3 0 0)
      let counts = TimelineBucketSeriesCounts 1 2 3 4 5
      eitherDecode (encode counts) `shouldBe` Right counts
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
  describe "Observation content version JSON" $ do
    it "round-trips the opaque UUID token" $
      eitherDecode (encode observationValue) `shouldBe` Right observationValue
    it "rejects missing and malformed canonical read tokens" $ do
      case toJSON observationValue of
        Object fields -> do
          eitherDecode (encode (Object (KM.delete "content_version" fields))) `shouldSatisfy` (isLeft :: Either String Observation -> Bool)
          eitherDecode (encode (Object (KM.insert "content_version" (toJSON ("not-a-uuid" :: String)) fields))) `shouldSatisfy` (isLeft :: Either String Observation -> Bool)
        _ -> expectationFailure "Observation must encode an object"
    it "requires explicit reviewed SHA and rejects server-owned field mutation" $ do
      eitherDecode "{\"content\":\"replacement\"}" `shouldSatisfy` (isLeft :: Either String ReviewedObservationUpdate -> Bool)
      eitherDecode (encode (ReviewedObservationUpdate "replacement" canonicalSha)) `shouldBe` Right (ReviewedObservationUpdate "replacement" canonicalSha)
      eitherDecode "{\"content\":\"replacement\",\"content_version\":\"00000000-0000-0000-0000-000000000001\"}" `shouldSatisfy` (isLeft :: Either String ReviewedObservationUpdate -> Bool)
      validateReviewedObservationUpdate (ReviewedObservationUpdate "text" "BAD") `shouldSatisfy` (not . null)
    it "hashes exact UTF-8 bytes without Unicode/newline normalization" $ do
      observationContentDigest "abc" `shouldBe` "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
      observationContentDigest "é\r\n" `shouldNotBe` observationContentDigest "e\x301\n"
    it "requires nullable canonical provenance fields to be present" $ do
      case toJSON observationValue of
        Object fields -> do
          eitherDecode (encode (Object (KM.delete "current_provenance" fields))) `shouldSatisfy` (isLeft :: Either String Observation -> Bool)
          eitherDecode (encode (Object (KM.delete "latest_sequence" fields))) `shouldSatisfy` (isLeft :: Either String Observation -> Bool)
        _ -> expectationFailure "Observation must encode an object"
  where
    workspace = read "00000000-0000-0000-0000-000000000001" :: UUID
    observationSearch = UnifiedSearchQuery
      { currentGitSha = Nothing, historyGitSha = Nothing,  workspaceId = Nothing, query = Nothing, entityTypes = Nothing, searchLanguage = Nothing
      , limit = Nothing, offset = Nothing, subjectKind = Nothing, subject = Nothing, gitSha = Nothing
      , projectStatus = Nothing, taskStatus = Nothing, taskPriority = Nothing, projectId = Nothing }
    savedView = CreateSavedView workspace "view" Nothing "activity" Null
    observedAt = read "2026-01-02 03:04:05 UTC" :: UTCTime
    observationValue = Observation workspace workspace create.subjects canonicalSha "body" observedAt observedAt workspace 1 Nothing Nothing
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
      "deadbeef" "preview" observedAt workspace 1 Nothing Nothing
    legacySearchHitValue = ObservationSearchHit workspace workspace
      [ObservationSubject SubjectFile "src/Main.hs"] "deadbeef" "preview" observedAt workspace 1 Nothing Nothing
    legacySearchHit = object
      [ "id" .= workspace, "workspace_id" .= workspace, "subject_kind" .= SubjectFile
      , "subject" .= ("src/Main.hs" :: String), "git_sha" .= ("deadbeef" :: String)
      , "content_preview" .= ("preview" :: String), "updated_at" .= observedAt, "content_version" .= workspace, "latest_sequence" .= (1 :: Int), "current_provenance" .= Null ]
    canonicalSearchHit = object
      [ "id" .= workspace, "workspace_id" .= workspace, "subjects" .= searchHit.subjects
      , "git_sha" .= ("deadbeef" :: String), "content_preview" .= ("preview" :: String), "updated_at" .= observedAt, "content_version" .= workspace, "latest_sequence" .= (1 :: Int), "current_provenance" .= Null ]
    dualSearchHit = object
      [ "id" .= workspace, "workspace_id" .= workspace, "subjects" .= searchHit.subjects
      , "subject_kind" .= SubjectGlob, "subject" .= ("ignored/**/*.hs" :: String)
      , "git_sha" .= ("deadbeef" :: String), "content_preview" .= ("preview" :: String), "updated_at" .= observedAt, "content_version" .= workspace, "latest_sequence" .= (1 :: Int), "current_provenance" .= Null ]
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
