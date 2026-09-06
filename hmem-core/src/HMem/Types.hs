module HMem.Types
  ( jsonOptions, camelToSnake
  , SubjectKind(..), subjectKindToText, subjectKindFromText
  , ObservationSubject(..), Observation(..), CreateObservation(..), UpdateObservation(..), ObservationQuery(..), ObservationSubjectFacetQuery(..), ObservationSubjectFacet(..), SimilarObservationQuery(..), SimilarObservation(..), ObservationMatchQuery(..), ObservationPathMatch(..), ObservationMatch(..)
  , maxObservationSubjectBytes, maxObservationSubjects, maxObservationSubjectBytesTotal, maxObservationContentBytes, observationEmbeddingDimensions
  , ObservationEmbedding(..), EmbeddingSpaceFingerprint, embeddingSpaceFingerprintText, legacyManualEmbeddingSpace, parseEmbeddingSpaceFingerprint
  , validateCreateObservationInput, validateUpdateObservationInput, validateObservationQuery, validateObservationSubjectFacetQuery, validateSimilarObservationQuery, validateObservationMatchQuery, validateObservationSubjects, normalizeObservationSubjects, observationSubjectMatchesPath
  , WorkspaceType(..), Workspace(..), CreateWorkspace(..), UpdateWorkspace(..), WorkspaceCardHydration(..), WorkspaceTaskDependencyLink(..)
  , WorkspaceGroup(..), CreateWorkspaceGroup(..), WorkspaceGroupMemberInput(..)
  , ProjectStatus(..), Project(..), CreateProject(..), UpdateProject(..), ProjectListQuery(..), ProjectOverview(..), ProjectReadinessRollup(..), ProjectCardSummary(..)
  , TaskDependencySummary(..), TaskDependencyPage(..), TaskOverview(..), TaskReadinessRollup(..), TaskCardSummary(..)
  , TaskStatus(..), Task(..), NextTaskCandidate(..), TaskDependencyAutoBlockSnapshot(..), TaskDependencyStatusChange(..), LinkDependency(..), DependencyMutationResult(..), TaskMutationResult(..), CreateTask(..), UpdateTask(..), TaskListQuery(..)
  , NavigationParent(..), NavigationFilter(..), NavigationBranchRequest(..), NavigationPage(..), NavigationBranchResponse(..), NavigationEntityType(..), NavigationSummary(..), NavigationFocusResponse(..), NavigationSummariesRequest(..), NavigationSummariesResponse(..)
  , maxNavigationPageSize, maxNavigationOffset, maxNavigationBatchIds, maxFocusAncestors, validateNavigationPage, validateNavigationSummariesRequest
  , EntitySearchType(..), ObservationSearchHit(..), UnifiedSearchQuery(..), UnifiedSearchResults(..), validateUnifiedSearchQuery
  , ActivityEvent(..), WorkspaceTimelineEvent(..), TimelineActor(..), TimelineProjectContext(..), TimelineTaskContext(..), TimelineStatusTransition(..), TimelineNavigation(..), TimelineBucketCounts(..), TimelineBucketEntityCounts(..), TimelineBucketSeriesCounts(..), TimelineBucketSeries(..), WorkspaceTimelineBucket(..), WorkspaceTimelineBucketsResponse(..)
  , SavedView(..), CreateSavedView(..), UpdateSavedView(..), SavedViewListQuery(..)
  , AuditAction(..), AuditLogEntry(..), AuditLogQuery(..), RevertResult(..), auditActionToText, auditActionFromText
  , WebSocketTicketRequest(..), WebSocketTicketResponse(..), ChangeStreamScopeRequest(..), SnapshotProfile(..), snapshotProfileToText, ChangeStreamResyncRequest(..), ChangeStreamSnapshotItem(..), ChangeStreamResyncResponse(..), CanonicalWebSocketTicketRequest(..), SessionContext(..), SessionPrincipal(..), SessionGlobalPermissions(..), SessionWorkspaceContext(..), PaginatedResult(..)
  , BatchDeleteRequest(..), BatchMoveTasksRequest(..), BatchResult(..), CascadeResult(..), BatchUpdateProjectItem(..), BatchUpdateProjectRequest(..), BatchUpdateTaskItem(..), BatchUpdateTaskRequest(..)
  , validateBatchDeleteRequest, validateBatchMoveTasksRequest, validateBatchUpdateProjectRequest, validateBatchUpdateTaskRequest
  , projectStatusToText, projectStatusFromText, taskStatusToText, taskStatusFromText, workspaceTypeToText, workspaceTypeFromText
  , FieldUpdate(..), parseFieldUpdate, fieldUpdatePair, applyNullableUpdate
  , maxNameBytes, maxDescriptionBytes, validFtsLanguage, maxPaginationOffset, maxPaginationLimit, capPagination, capPaginationOverfetch
  , validateCreateWorkspaceInput, validateUpdateWorkspaceInput, validateCreateProjectInput, validateUpdateProjectInput, validateProjectListQuery, validateCreateTaskInput, validateUpdateTaskInput, validateTaskListQuery, validateCreateWorkspaceGroupInput, validateCreateSavedViewInput, validateUpdateSavedViewInput
  ) where

import Data.Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types (Parser, Pair)
import Control.Applicative ((<|>))
import Control.Monad (unless)
import Data.ByteString qualified as BS
import Data.Char (isAlpha, isHexDigit, isLower, isSpace, isUpper, toLower)
import Data.Int (Int64)
import Data.List (nub)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time (UTCTime)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Rel8 (DBType(..), DBEq, Expr, Sql, TypeInformation(..), TypeName(..), QualifiedName(..), lit, parseTypeInformation)

------------------------------------------------------------------------
-- JSON helpers
------------------------------------------------------------------------

-- | Default Aeson options: camelCase fields → snake_case keys, omit Nothing.
jsonOptions :: Options
jsonOptions = defaultOptions
  { fieldLabelModifier    = camelToSnake
  , constructorTagModifier = camelToSnake
  , omitNothingFields     = True
  }

-- | Convert camelCase or PascalCase to snake_case.
-- Handles acronyms: @\"URLPath\"@ → @\"url_path\"@, @\"getURL\"@ → @\"get_url\"@.
camelToSnake :: String -> String
camelToSnake = map toLower . go
  where
    go [] = []
    go [c] = [c]
    go (c1:c2:rest)
      -- lower followed by upper: aB → a_B
      | isLower c1, isUpper c2 = c1 : '_' : go (c2:rest)
      -- upper followed by upper-then-lower: ABc → A_Bc  (end of acronym)
      | isUpper c1, isUpper c2, r:_ <- rest, isLower r = c1 : '_' : go (c2:rest)
      | otherwise = c1 : go (c2:rest)

------------------------------------------------------------------------
-- FieldUpdate (three-state nullable update)
------------------------------------------------------------------------

data FieldUpdate a = Unchanged | SetNull | SetTo a
  deriving (Show, Eq)

instance FromJSON a => FromJSON (FieldUpdate a) where
  parseJSON Null = pure SetNull
  parseJSON v    = SetTo <$> parseJSON v

instance ToJSON a => ToJSON (FieldUpdate a) where
  toJSON Unchanged = Null
  toJSON SetNull   = Null
  toJSON (SetTo a) = toJSON a

-- | Parse a 'FieldUpdate' from an Aeson 'Object', distinguishing
-- absent keys ('Unchanged') from explicit @null@ ('SetNull').
parseFieldUpdate :: FromJSON a => Object -> Key -> Parser (FieldUpdate a)
parseFieldUpdate o k = case KM.lookup k o of
  Nothing   -> pure Unchanged
  Just Null -> pure SetNull
  Just v    -> SetTo <$> parseJSON v

-- | Produce a key/value pair for a 'FieldUpdate', returning 'Nothing'
-- for 'Unchanged' so the field is omitted from serialized JSON.
fieldUpdatePair :: ToJSON a => Key -> FieldUpdate a -> Maybe Pair
fieldUpdatePair _ Unchanged = Nothing
fieldUpdatePair k SetNull   = Just (k .= Null)
fieldUpdatePair k (SetTo v) = Just (k .= v)

-- | Apply a 'FieldUpdate' to a nullable Rel8 column expression.
applyNullableUpdate :: Sql DBType (Maybe a) => Expr (Maybe a) -> FieldUpdate a -> Expr (Maybe a)
applyNullableUpdate old Unchanged   = old
applyNullableUpdate _   SetNull     = lit Nothing
applyNullableUpdate _   (SetTo val) = lit (Just val)

------------------------------------------------------------------------
-- Input validation
------------------------------------------------------------------------

maxNameBytes :: Int
maxNameBytes = 1024

maxDescriptionBytes :: Int
maxDescriptionBytes = 100 * 1024

validFtsLanguage :: Maybe Text -> Bool
validFtsLanguage Nothing = True
validFtsLanguage (Just language) = language `elem`
  [ "simple", "arabic", "armenian", "basque", "catalan", "danish"
  , "dutch", "english", "finnish", "french", "german", "greek"
  , "hindi", "hungarian", "indonesian", "irish", "italian"
  , "lithuanian", "nepali", "norwegian", "portuguese", "romanian"
  , "russian", "serbian", "spanish", "swedish", "tamil", "turkish"
  , "yiddish"
  ]

-- | Maximum allowed pagination offset (default: 100,000).
maxPaginationOffset :: Int
maxPaginationOffset = 100000

-- | Maximum allowed pagination limit (default: 200).
maxPaginationLimit :: Int
maxPaginationLimit = 200

-- | Cap limit and offset to safe ranges, applying defaults.
-- Limit defaults to 50, capped at 'maxPaginationLimit'.
-- Offset defaults to 0, capped at 'maxPaginationOffset'.
capPagination :: Maybe Int -> Maybe Int -> (Int, Int)
capPagination mlimit moffset =
  capPaginationWithMax maxPaginationLimit mlimit moffset

-- | Cap limit and offset for internal over-fetching pagination queries.
--
-- API handlers expose 'maxPaginationLimit' items to callers but request one
-- additional row from the database to compute @has_more@ without a COUNT
-- query.  Keep this separate from 'capPagination' so public list limits remain
-- capped at 200 while internal page-boundary detection can fetch 201 rows.
capPaginationOverfetch :: Maybe Int -> Maybe Int -> (Int, Int)
capPaginationOverfetch mlimit moffset =
  capPaginationWithMax (maxPaginationLimit + 1) mlimit moffset

capPaginationWithMax :: Int -> Maybe Int -> Maybe Int -> (Int, Int)
capPaginationWithMax maxLimit mlimit moffset =
  ( min maxLimit  (max 1 (fromMaybe 50 mlimit))
  , min maxPaginationOffset (max 0 (fromMaybe 0  moffset))
  )

validateCreateWorkspaceInput :: CreateWorkspace -> [Text]
validateCreateWorkspaceInput cw =
  validateRequiredText "name" maxNameBytes cw.name

validateUpdateWorkspaceInput :: UpdateWorkspace -> [Text]
validateUpdateWorkspaceInput uw =
  validateRequiredText "name" maxNameBytes uw.name

validateCreateProjectInput :: CreateProject -> [Text]
validateCreateProjectInput cp =
  validateRequiredText "name" maxNameBytes cp.name
  <> validateOptionalText "description" maxDescriptionBytes cp.description

validateUpdateProjectInput :: UpdateProject -> [Text]
validateUpdateProjectInput up =
  maybe [] (validateRequiredText "name" maxNameBytes) up.name
  <> validateOptionalFieldText "description" maxDescriptionBytes up.description

validateProjectListQuery :: ProjectListQuery -> [Text]
validateProjectListQuery pq =
  validateTimeRange "created_after" pq.createdAfter "created_before" pq.createdBefore
  <> validateTimeRange "updated_after" pq.updatedAfter "updated_before" pq.updatedBefore
  <> ["Invalid search_language" | not (validFtsLanguage pq.searchLanguage)]

validateCreateTaskInput :: CreateTask -> [Text]
validateCreateTaskInput ct =
  validateRequiredText "title" maxNameBytes ct.title
  <> validateOptionalText "description" maxDescriptionBytes ct.description

validateUpdateTaskInput :: UpdateTask -> [Text]
validateUpdateTaskInput ut =
  maybe [] (validateRequiredText "title" maxNameBytes) ut.title
  <> validateOptionalFieldText "description" maxDescriptionBytes ut.description

validateTaskListQuery :: TaskListQuery -> [Text]
validateTaskListQuery tq =
  ["workspace_id or project_id is required" | tq.workspaceId == Nothing && tq.projectId == Nothing]
  <> validateOptionalIntRange "priority" 1 10 tq.priority
  <> validateTimeRange "created_after" tq.createdAfter "created_before" tq.createdBefore
  <> validateTimeRange "updated_after" tq.updatedAfter "updated_before" tq.updatedBefore
  <> ["Invalid search_language" | not (validFtsLanguage tq.searchLanguage)]

validateCreateWorkspaceGroupInput :: CreateWorkspaceGroup -> [Text]
validateCreateWorkspaceGroupInput cg =
  validateRequiredText "name" maxNameBytes cg.name
  <> validateOptionalText "description" maxDescriptionBytes cg.description

validSavedViewEntityTypes :: [Text]
validSavedViewEntityTypes = ["observation_search", "observation_list", "project_list", "task_list", "activity"]

validateCreateSavedViewInput :: CreateSavedView -> [Text]
validateCreateSavedViewInput csv =
  validateRequiredText "name" maxNameBytes csv.name
  <> validateOptionalText "description" maxDescriptionBytes csv.description
  <> ["entity_type must be one of: " <> T.intercalate ", " validSavedViewEntityTypes
     | csv.entityType `notElem` validSavedViewEntityTypes]

validateUpdateSavedViewInput :: UpdateSavedView -> [Text]
validateUpdateSavedViewInput usv =
  maybe [] (validateRequiredText "name" maxNameBytes) usv.name
  <> validateOptionalFieldText "description" maxDescriptionBytes usv.description

validateRequiredText :: Text -> Int -> Text -> [Text]
validateRequiredText field maxBytes value =
  [field <> " must not be empty" | T.null (T.strip value)]
  <> validateByteLength field maxBytes value

validateOptionalText :: Text -> Int -> Maybe Text -> [Text]
validateOptionalText field maxBytes = maybe [] (validateByteLength field maxBytes)

validateOptionalFieldText :: Text -> Int -> FieldUpdate Text -> [Text]
validateOptionalFieldText _ _ Unchanged = []
validateOptionalFieldText _ _ SetNull = []
validateOptionalFieldText field maxBytes (SetTo value) = validateByteLength field maxBytes value

validateOptionalIntRange :: Text -> Int -> Int -> Maybe Int -> [Text]
validateOptionalIntRange _ _ _ Nothing = []
validateOptionalIntRange field lo hi (Just value)
  | value < lo || value > hi = [field <> " must be between " <> T.pack (show lo) <> " and " <> T.pack (show hi)]
  | otherwise = []

validateByteLength :: Text -> Int -> Text -> [Text]
validateByteLength field maxBytes value =
  [ field <> " exceeds " <> T.pack (show maxBytes) <> " bytes"
  | textSizeBytes value > maxBytes
  ]

validateTimeRange :: Text -> Maybe UTCTime -> Text -> Maybe UTCTime -> [Text]
validateTimeRange _ Nothing _ _ = []
validateTimeRange _ _ _ Nothing = []
validateTimeRange startField (Just startTime) endField (Just endTime)
  | startTime <= endTime = []
  | otherwise = [startField <> " must be earlier than or equal to " <> endField]

textSizeBytes :: Text -> Int
textSizeBytes = BS.length . TE.encodeUtf8

prefixIssues :: Text -> [Text] -> [Text]
prefixIssues prefix = map (prefix <>)

------------------------------------------------------------------------
-- Observations
------------------------------------------------------------------------

-- | The kind of repository-relative subject captured by an observation.
data SubjectKind = SubjectFile | SubjectGlob
  deriving (Show, Eq, Ord, Bounded, Enum, Generic)

instance ToJSON SubjectKind where
  toJSON SubjectFile = String "file"
  toJSON SubjectGlob = String "glob"

instance FromJSON SubjectKind where
  parseJSON = withText "SubjectKind" $ \case
    "file" -> pure SubjectFile
    "glob" -> pure SubjectGlob
    _      -> fail "Invalid subject kind: expected file or glob"

subjectKindToText :: SubjectKind -> Text
subjectKindToText SubjectFile = "file"
subjectKindToText SubjectGlob = "glob"

subjectKindFromText :: Text -> Maybe SubjectKind
subjectKindFromText "file" = Just SubjectFile
subjectKindFromText "glob" = Just SubjectGlob
subjectKindFromText _      = Nothing

instance DBType SubjectKind where
  typeInformation = case parseTypeInformation parse subjectKindToText typeInformation of
    TypeInformation enc dec delim _ ->
      TypeInformation enc dec delim (TypeName (QualifiedName "observation_subject_kind" Nothing) [] 0)
    where
      parse t = maybe (Left $ "Invalid observation_subject_kind: " <> T.unpack t) Right (subjectKindFromText t)

instance DBEq SubjectKind

maxObservationSubjectBytes :: Int
maxObservationSubjectBytes = 4096

maxObservationSubjects :: Int
maxObservationSubjects = 256

maxObservationSubjectBytesTotal :: Int
maxObservationSubjectBytesTotal = 256 * 1024

maxObservationContentBytes :: Int
maxObservationContentBytes = 512 * 1024

observationEmbeddingDimensions :: Int
observationEmbeddingDimensions = 1536

-- | An embedding is meaningful only inside the exact producer/model space
-- that created it.  Its constructor is deliberately private: all external
-- input must pass through 'parseEmbeddingSpaceFingerprint'.
newtype EmbeddingSpaceFingerprint = EmbeddingSpaceFingerprint
  { unEmbeddingSpaceFingerprint :: Text
  } deriving (Show, Eq, Ord, Generic)

instance ToJSON EmbeddingSpaceFingerprint where toJSON = toJSON . (.unEmbeddingSpaceFingerprint)
instance FromJSON EmbeddingSpaceFingerprint where
  parseJSON value = do
    raw <- parseJSON value
    maybe (fail "invalid embedding space_fingerprint") pure (parseEmbeddingSpaceFingerprint raw)

-- | Stable compatibility identity for vectors written before producer-space
-- isolation.  It is deliberately not the managed GTE-Qwen2 identity.
legacyManualEmbeddingSpace :: EmbeddingSpaceFingerprint
legacyManualEmbeddingSpace = EmbeddingSpaceFingerprint "hmem:legacy-manual:v1"

embeddingSpaceFingerprintText :: EmbeddingSpaceFingerprint -> Text
embeddingSpaceFingerprintText = (.unEmbeddingSpaceFingerprint)

parseEmbeddingSpaceFingerprint :: Text -> Maybe EmbeddingSpaceFingerprint
parseEmbeddingSpaceFingerprint raw
  | T.null raw || T.length raw > 128 = Nothing
  | T.any (\char -> isSpace char || char == '\DEL') raw = Nothing
  | otherwise = Just (EmbeddingSpaceFingerprint raw)

-- | Transparent JSON wrapper used where the HTTP contract accepts a raw
-- embedding array while its schema must still state the fixed dimension.
data ObservationEmbedding = ObservationEmbedding
  { unObservationEmbedding :: [Double]
  , embeddingSpaceFingerprint :: Maybe EmbeddingSpaceFingerprint
  } deriving (Show, Eq, Generic)

instance ToJSON ObservationEmbedding where
  toJSON value = case value.embeddingSpaceFingerprint of
    Nothing -> toJSON value.unObservationEmbedding
    Just space -> object ["embedding" .= value.unObservationEmbedding, "space_fingerprint" .= space]
instance FromJSON ObservationEmbedding where
  parseJSON value = (ObservationEmbedding <$> parseJSON value <*> pure Nothing) <|>
    withObject "ObservationEmbedding" (\objectValue -> do
      unless (KM.size objectValue == 2 && KM.member "embedding" objectValue && KM.member "space_fingerprint" objectValue)
        (fail "embedding envelope must contain only embedding and space_fingerprint")
      ObservationEmbedding <$> objectValue .: "embedding" <*> (Just <$> objectValue .: "space_fingerprint")) value

data ObservationSubject = ObservationSubject
  { subjectKind :: SubjectKind
  , subject     :: Text
  } deriving (Show, Eq, Ord, Generic)

instance ToJSON ObservationSubject where toJSON = genericToJSON jsonOptions
instance FromJSON ObservationSubject where parseJSON = genericParseJSON jsonOptions

data Observation = Observation
  { id          :: UUID
  , workspaceId :: UUID
  , subjects    :: [ObservationSubject]
  , gitSha      :: Text
  , content     :: Text
  , createdAt   :: UTCTime
  , updatedAt   :: UTCTime
  } deriving (Show, Eq, Generic)

instance ToJSON Observation where
  toJSON observation = object $
    [ "id" .= observation.id, "workspace_id" .= observation.workspaceId
    , "subjects" .= observation.subjects, "git_sha" .= observation.gitSha
    , "content" .= observation.content, "created_at" .= observation.createdAt
    , "updated_at" .= observation.updatedAt
    ] <> legacySubjectPairs observation.subjects
instance FromJSON Observation where
  parseJSON = withObject "Observation" $ \o -> Observation
    <$> o .: "id" <*> o .: "workspace_id" <*> parseSubjects o <*> o .: "git_sha"
    <*> o .: "content" <*> o .: "created_at" <*> o .: "updated_at"

-- | Provenance is supplied once, on creation, and is immutable thereafter.
data CreateObservation = CreateObservation
  { workspaceId :: UUID
  , subjects    :: [ObservationSubject]
  , gitSha      :: Text
  , content     :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON CreateObservation where
  toJSON create = object
    [ "workspace_id" .= create.workspaceId, "subjects" .= create.subjects
    , "git_sha" .= create.gitSha, "content" .= create.content
    ]
instance FromJSON CreateObservation where
  parseJSON = withObject "CreateObservation" $ \o -> CreateObservation
    <$> o .: "workspace_id" <*> parseCreateObservationSubjects o <*> o .: "git_sha" <*> o .: "content"

-- | Observation updates deliberately expose only mutable content.
newtype UpdateObservation = UpdateObservation { content :: Text }
  deriving (Show, Eq, Generic)

instance ToJSON UpdateObservation where
  toJSON = genericToJSON jsonOptions
instance FromJSON UpdateObservation where
  parseJSON = withObject "UpdateObservation" $ \updateObject -> do
    let unknownKeys = filter (`notElem` ["content"]) (map Key.toText (KM.keys updateObject))
    if null unknownKeys
      then UpdateObservation <$> updateObject .: "content"
      else fail $ "UpdateObservation accepts only content; unexpected fields: " <> show unknownKeys

-- | Exact provenance filters compose with optional full-text search.
data ObservationQuery = ObservationQuery
  { workspaceId :: UUID
  , subjectKind :: Maybe SubjectKind
  , subject     :: Maybe Text
  , gitSha      :: Maybe Text
  , query       :: Maybe Text
  , limit       :: Maybe Int
  , offset      :: Maybe Int
  } deriving (Show, Eq, Generic)

instance ToJSON ObservationQuery where
  toJSON = genericToJSON jsonOptions
instance FromJSON ObservationQuery where
  parseJSON = genericParseJSON jsonOptions

-- | Lists exact stored subjects with counts calculated over the complete
-- filtered Observation set before subject pagination is applied.
data ObservationSubjectFacetQuery = ObservationSubjectFacetQuery
  { workspaceId :: UUID
  , subjectKind :: Maybe SubjectKind
  , gitSha      :: Maybe Text
  , query       :: Maybe Text
  , limit       :: Maybe Int
  , offset      :: Maybe Int
  } deriving (Show, Eq, Generic)

instance ToJSON ObservationSubjectFacetQuery where toJSON = genericToJSON jsonOptions
instance FromJSON ObservationSubjectFacetQuery where parseJSON = genericParseJSON jsonOptions

data ObservationSubjectFacet = ObservationSubjectFacet
  { subjectKind      :: SubjectKind
  , subject          :: Text
  , observationCount :: Int64
  , latestUpdatedAt  :: UTCTime
  } deriving (Show, Eq, Generic)

instance ToJSON ObservationSubjectFacet where toJSON = genericToJSON jsonOptions
instance FromJSON ObservationSubjectFacet where parseJSON = genericParseJSON jsonOptions

-- | Vector search has the same composable exact filters as text search.
data SimilarObservationQuery = SimilarObservationQuery
  { workspaceId    :: UUID
  , subjectKind    :: Maybe SubjectKind
  , subject        :: Maybe Text
  , gitSha         :: Maybe Text
  , embedding      :: [Double]
  , spaceFingerprint :: Maybe EmbeddingSpaceFingerprint
  , minSimilarity  :: Maybe Double
  , limit          :: Maybe Int
  , offset         :: Maybe Int
  } deriving (Show, Eq, Generic)

instance ToJSON SimilarObservationQuery where
  toJSON = genericToJSON jsonOptions
instance FromJSON SimilarObservationQuery where
  parseJSON = withObject "SimilarObservationQuery" $ \objectValue -> SimilarObservationQuery
    <$> objectValue .: "workspace_id" <*> objectValue .:? "subject_kind"
    <*> objectValue .:? "subject" <*> objectValue .:? "git_sha"
    <*> objectValue .: "embedding" <*> presentFingerprint objectValue
    <*> objectValue .:? "min_similarity" <*> objectValue .:? "limit" <*> objectValue .:? "offset"

presentFingerprint :: Object -> Parser (Maybe EmbeddingSpaceFingerprint)
presentFingerprint objectValue
  | KM.member "space_fingerprint" objectValue = Just <$> objectValue .: "space_fingerprint"
  | otherwise = pure Nothing

data SimilarObservation = SimilarObservation
  { observation :: Observation
  , similarity  :: Double
  } deriving (Show, Eq, Generic)

instance ToJSON SimilarObservation where
  toJSON = genericToJSON jsonOptions
instance FromJSON SimilarObservation where
  parseJSON = genericParseJSON jsonOptions

-- | Returns each observation once for concrete repository-relative paths.
-- Matching is OR across paths and subjects; ordering is retained to make a
-- result useful to agents without additional sorting or de-duplication.
data ObservationMatchQuery = ObservationMatchQuery
  { workspaceId :: UUID
  , paths       :: [Text]
  , subjectKind :: Maybe SubjectKind
  , gitSha      :: Maybe Text
  , query       :: Maybe Text
  , limit       :: Maybe Int
  , offset      :: Maybe Int
  } deriving (Show, Eq, Generic)

instance ToJSON ObservationMatchQuery where toJSON = genericToJSON jsonOptions
instance FromJSON ObservationMatchQuery where parseJSON = genericParseJSON jsonOptions

-- | Canonical match evidence for one caller-supplied path. Stored subjects
-- retain their ordinal order within each path group.
data ObservationPathMatch = ObservationPathMatch
  { path            :: Text
  , matchedSubjects :: [ObservationSubject]
  } deriving (Show, Eq, Generic)

instance ToJSON ObservationPathMatch where toJSON = genericToJSON jsonOptions
instance FromJSON ObservationPathMatch where parseJSON = genericParseJSON jsonOptions

data ObservationMatch = ObservationMatch
  { observation     :: Observation
  , pathMatches     :: [ObservationPathMatch]
  , matchedPaths    :: [Text]
  , matchedSubjects :: [ObservationSubject]
  } deriving (Show, Eq, Generic)

instance ToJSON ObservationMatch where
  toJSON match = object
    [ "observation" .= match.observation
    , "path_matches" .= match.pathMatches
    , "matched_paths" .= match.matchedPaths
    , "matched_subjects" .= match.matchedSubjects
    ]
instance FromJSON ObservationMatch where
  parseJSON = withObject "ObservationMatch" $ \o -> ObservationMatch
    <$> o .: "observation"
    <*> o .:? "path_matches" .!= []
    <*> o .: "matched_paths"
    <*> o .: "matched_subjects"

validateCreateObservationInput :: CreateObservation -> [Text]
validateCreateObservationInput co =
  validateObservationSubjects co.subjects
  <> ["git_sha must be a lowercase 40-character hexadecimal Git SHA" | not (validGitSha co.gitSha)]
  <> validateRequiredText "content" maxObservationContentBytes co.content

validateUpdateObservationInput :: UpdateObservation -> [Text]
validateUpdateObservationInput (UpdateObservation value) =
  validateRequiredText "content" maxObservationContentBytes value

validateObservationQuery :: ObservationQuery -> [Text]
validateObservationQuery oq =
  validateObservationPagination oq.limit oq.offset
  <> maybe [] validateObservationSubject oq.subject
  <> maybe [] validateGitSha oq.gitSha

validateObservationSubjectFacetQuery :: ObservationSubjectFacetQuery -> [Text]
validateObservationSubjectFacetQuery queryValue =
  validateObservationPagination queryValue.limit queryValue.offset
  <> maybe [] validateGitSha queryValue.gitSha

validateSimilarObservationQuery :: SimilarObservationQuery -> [Text]
validateSimilarObservationQuery soq =
  validateObservationPagination soq.limit soq.offset
  <> maybe [] validateObservationSubject soq.subject
  <> maybe [] validateGitSha soq.gitSha
  <> ["embedding must contain exactly 1536 finite dimensions"
     | length soq.embedding /= observationEmbeddingDimensions
       || any (\x -> isNaN x || isInfinite x) soq.embedding]
  <> ["min_similarity must be between 0 and 1"
     | maybe False (\x -> x < 0 || x > 1 || isNaN x || isInfinite x) soq.minSimilarity]
  <> ["space_fingerprint must be a non-blank opaque value of at most 128 characters"
     | maybe False (\fingerprint -> parseEmbeddingSpaceFingerprint (embeddingSpaceFingerprintText fingerprint) /= Just fingerprint) soq.spaceFingerprint]

validateObservationMatchQuery :: ObservationMatchQuery -> [Text]
validateObservationMatchQuery omq =
  validateObservationPagination omq.limit omq.offset
  <> validateConcretePaths omq.paths
  <> maybe [] validateGitSha omq.gitSha

normalizeObservationSubjects :: [ObservationSubject] -> [ObservationSubject]
normalizeObservationSubjects = reverse . snd . foldl' keep ([], [])
  where
    keep (seen, kept) candidate
      | candidate `elem` seen = (seen, kept)
      | otherwise = (candidate : seen, candidate : kept)

validateObservationSubjects :: [ObservationSubject] -> [Text]
validateObservationSubjects values =
  ["subjects must contain between 1 and " <> T.pack (show maxObservationSubjects) <> " entries"
  | null normalized || length normalized > maxObservationSubjects]
  <> concatMap validateSubject normalized
  <> ["subjects must total at most 262144 UTF-8 bytes"
     | sum (map (BS.length . TE.encodeUtf8 . (.subject)) normalized) > maxObservationSubjectBytesTotal]
  where
    normalized = normalizeObservationSubjects values
    validateSubject candidate = validateObservationSubject candidate.subject
      <> ["file subjects must be concrete paths" | candidate.subjectKind == SubjectFile && T.any (`elem` ['*', '?']) candidate.subject]
      <> ["glob subjects support only *, ?, and ** path components" | candidate.subjectKind == SubjectGlob && not (validGlob candidate.subject)]

validateObservationSubject :: Text -> [Text]
validateObservationSubject value =
  ["subject must be a canonical repository-relative forward-slash path or glob"
  | T.null value || T.isPrefixOf "/" value || T.isPrefixOf "./" value
    || T.any (== '\\') value || isWindowsAbsolute value
    || any (`elem` ["", ".", ".."]) (T.splitOn "/" value)]
  <> ["subject must not contain control characters" | T.any (\c -> c < ' ' || c == '\DEL') value]
  <> validateByteLength "subject" maxObservationSubjectBytes value

validateConcretePaths :: [Text] -> [Text]
validateConcretePaths values =
  ["paths must contain between 1 and " <> T.pack (show maxObservationSubjects) <> " entries"
  | null normalized || length normalized > maxObservationSubjects]
  <> concatMap (\path -> validateObservationSubject path <> ["paths must be concrete paths" | T.any (`elem` ['*', '?']) path]) normalized
  <> ["paths must total at most 262144 UTF-8 bytes" | sum (map (BS.length . TE.encodeUtf8) normalized) > maxObservationSubjectBytesTotal]
  where normalized = reverse . snd $ foldl' (\(seen, kept) path -> if path `elem` seen then (seen, kept) else (path:seen, path:kept)) ([], []) values

validGlob :: Text -> Bool
validGlob value =
  not (T.any (`elem` ['[', ']', '{', '}']) value)
  && all validComponent (T.splitOn "/" value)
  where
    validComponent component
      | component == "**" = True
      | T.isInfixOf "**" component = False
      | otherwise = True

-- | Pure reference matcher.  It intentionally treats dotfiles as ordinary
-- path components and never reads the filesystem or Git state.
observationSubjectMatchesPath :: ObservationSubject -> Text -> Bool
observationSubjectMatchesPath candidate path
  | not (null (validateObservationSubject path)) || T.any (`elem` ['*', '?']) path = False
  | candidate.subjectKind == SubjectFile = candidate.subject == path
  | not (null (validateObservationSubjects [candidate])) = False
  | otherwise = matchGlob (T.unpack candidate.subject) (T.unpack path)
  where
    -- Keep this character-level definition in lockstep with the SQL function
    -- in V021.  The validated grammar permits ** only as a full path segment;
    -- **/ has the conventional zero-directory case.
    matchGlob [] [] = True
    matchGlob [] _ = False
    matchGlob ('*':'*':'/':patterns) chars =
      matchGlob patterns chars || case chars of
        [] -> False
        _ -> case break (== '/') chars of
          (_, '/':remaining) -> matchGlob ('*':'*':'/':patterns) remaining
          _ -> False
    matchGlob ('*':'*':patterns) chars =
      matchGlob patterns chars || case chars of
        [] -> False
        (_:remaining) -> matchGlob ('*':'*':patterns) remaining
    matchGlob ('*':patterns) chars =
      matchGlob patterns chars || case chars of
        [] -> False
        ('/':_) -> False
        (_:remaining) -> matchGlob ('*':patterns) remaining
    matchGlob ('?':patterns) (c:remaining) = c /= '/' && matchGlob patterns remaining
    matchGlob ('?':_) [] = False
    matchGlob (pattern:patterns) (candidateChar:remaining) = pattern == candidateChar && matchGlob patterns remaining
    matchGlob _ _ = False

parseSubjects :: Object -> Parser [ObservationSubject]
parseSubjects o = do
  supplied <- o .:? "subjects"
  case supplied of
    Just values -> pure values
    Nothing -> (: []) <$> (ObservationSubject <$> o .: "subject_kind" <*> o .: "subject")

parseCreateObservationSubjects :: Object -> Parser [ObservationSubject]
parseCreateObservationSubjects o
  | hasSubjects && hasLegacySubject = fail "subjects cannot be combined with subject_kind or subject"
  | hasSubjects = o .: "subjects"
  | otherwise = (: []) <$> (ObservationSubject <$> o .: "subject_kind" <*> o .: "subject")
  where
    hasSubjects = KM.member "subjects" o
    hasLegacySubject = KM.member "subject_kind" o || KM.member "subject" o

legacySubjectPairs :: [ObservationSubject] -> [Pair]
legacySubjectPairs [] = []
legacySubjectPairs (first:_) = ["subject_kind" .= first.subjectKind, "subject" .= first.subject]

isWindowsAbsolute :: Text -> Bool
isWindowsAbsolute value = case T.unpack (T.take 3 value) of
  (drive:':':'/':_) -> isAlpha drive
  _                 -> False

validateGitSha :: Text -> [Text]
validateGitSha value =
  ["git_sha must be a lowercase 40-character hexadecimal Git SHA" | not (validGitSha value)]

validGitSha :: Text -> Bool
validGitSha value = T.length value == 40 && T.all (\c -> isHexDigit c && (c < 'A' || c > 'F')) value

validateObservationPagination :: Maybe Int -> Maybe Int -> [Text]
validateObservationPagination mLimit mOffset =
  ["limit must be between 1 and " <> T.pack (show maxPaginationLimit)
  | maybe False (\n -> n < 1 || n > maxPaginationLimit) mLimit]
  <> ["offset must be between 0 and " <> T.pack (show maxPaginationOffset)
     | maybe False (\n -> n < 0 || n > maxPaginationOffset) mOffset]

------------------------------------------------------------------------
-- ProjectStatus
------------------------------------------------------------------------

data ProjectStatus = ProjActive | ProjPaused | ProjCompleted | ProjArchived
  deriving (Show, Eq, Ord, Bounded, Enum, Generic)

instance ToJSON ProjectStatus where
  toJSON ProjActive    = String "active"
  toJSON ProjPaused    = String "paused"
  toJSON ProjCompleted = String "completed"
  toJSON ProjArchived  = String "archived"

instance FromJSON ProjectStatus where
  parseJSON = withText "ProjectStatus" $ \case
    "active"    -> pure ProjActive
    "paused"    -> pure ProjPaused
    "completed" -> pure ProjCompleted
    "archived"  -> pure ProjArchived
    _           -> fail "Invalid project status"

projectStatusToText :: ProjectStatus -> Text
projectStatusToText ProjActive    = "active"
projectStatusToText ProjPaused    = "paused"
projectStatusToText ProjCompleted = "completed"
projectStatusToText ProjArchived  = "archived"

projectStatusFromText :: Text -> Maybe ProjectStatus
projectStatusFromText "active"    = Just ProjActive
projectStatusFromText "paused"    = Just ProjPaused
projectStatusFromText "completed" = Just ProjCompleted
projectStatusFromText "archived"  = Just ProjArchived
projectStatusFromText _           = Nothing

instance DBType ProjectStatus where
  typeInformation = case parseTypeInformation parse projectStatusToText typeInformation of
    TypeInformation enc dec delim _ ->
      TypeInformation enc dec delim (TypeName (QualifiedName "project_status_enum" Nothing) [] 0)
    where
      parse t = maybe (Left $ "Invalid project status: " <> T.unpack t) Right (projectStatusFromText t)

instance DBEq ProjectStatus

------------------------------------------------------------------------
-- TaskStatus
------------------------------------------------------------------------

data TaskStatus = Todo | InProgress | Blocked | Done | Cancelled
  deriving (Show, Eq, Ord, Bounded, Enum, Generic)

instance ToJSON TaskStatus where
  toJSON Todo       = String "todo"
  toJSON InProgress = String "in_progress"
  toJSON Blocked    = String "blocked"
  toJSON Done       = String "done"
  toJSON Cancelled  = String "cancelled"

instance FromJSON TaskStatus where
  parseJSON = withText "TaskStatus" $ \case
    "todo"        -> pure Todo
    "in_progress" -> pure InProgress
    "blocked"     -> pure Blocked
    "done"        -> pure Done
    "cancelled"   -> pure Cancelled
    _             -> fail "Invalid task status"

taskStatusToText :: TaskStatus -> Text
taskStatusToText Todo       = "todo"
taskStatusToText InProgress = "in_progress"
taskStatusToText Blocked    = "blocked"
taskStatusToText Done       = "done"
taskStatusToText Cancelled  = "cancelled"

taskStatusFromText :: Text -> Maybe TaskStatus
taskStatusFromText "todo"        = Just Todo
taskStatusFromText "in_progress" = Just InProgress
taskStatusFromText "blocked"     = Just Blocked
taskStatusFromText "done"        = Just Done
taskStatusFromText "cancelled"   = Just Cancelled
taskStatusFromText _             = Nothing

instance DBType TaskStatus where
  typeInformation = case parseTypeInformation parse taskStatusToText typeInformation of
    TypeInformation enc dec delim _ ->
      TypeInformation enc dec delim (TypeName (QualifiedName "task_status_enum" Nothing) [] 0)
    where
      parse t = maybe (Left $ "Invalid task status: " <> T.unpack t) Right (taskStatusFromText t)

instance DBEq TaskStatus

------------------------------------------------------------------------
-- WorkspaceType
------------------------------------------------------------------------

data WorkspaceType = WsRepository | WsPlanning | WsPersonal | WsOrganization
  deriving (Show, Eq, Ord, Bounded, Enum, Generic)

instance ToJSON WorkspaceType where
  toJSON WsRepository   = String "repository"
  toJSON WsPlanning     = String "planning"
  toJSON WsPersonal     = String "personal"
  toJSON WsOrganization = String "organization"

instance FromJSON WorkspaceType where
  parseJSON = withText "WorkspaceType" $ \case
    "repository"   -> pure WsRepository
    "planning"     -> pure WsPlanning
    "personal"     -> pure WsPersonal
    "organization" -> pure WsOrganization
    _              -> fail "Invalid workspace type"

workspaceTypeToText :: WorkspaceType -> Text
workspaceTypeToText WsRepository   = "repository"
workspaceTypeToText WsPlanning     = "planning"
workspaceTypeToText WsPersonal     = "personal"
workspaceTypeToText WsOrganization = "organization"

workspaceTypeFromText :: Text -> Maybe WorkspaceType
workspaceTypeFromText "repository"   = Just WsRepository
workspaceTypeFromText "planning"     = Just WsPlanning
workspaceTypeFromText "personal"     = Just WsPersonal
workspaceTypeFromText "organization" = Just WsOrganization
workspaceTypeFromText _              = Nothing

instance DBType WorkspaceType where
  typeInformation = case parseTypeInformation parse workspaceTypeToText typeInformation of
    TypeInformation enc dec delim _ ->
      TypeInformation enc dec delim (TypeName (QualifiedName "workspace_type_enum" Nothing) [] 0)
    where
      parse t = maybe (Left $ "Invalid workspace_type: " <> T.unpack t) Right (workspaceTypeFromText t)

instance DBEq WorkspaceType

------------------------------------------------------------------------
-- Workspace
------------------------------------------------------------------------

data Workspace = Workspace
  { id            :: UUID
  , name          :: Text
  , workspaceType :: WorkspaceType
  , ghOwner       :: Maybe Text
  , ghRepo        :: Maybe Text
  , createdAt     :: UTCTime
  , updatedAt     :: UTCTime
  } deriving (Show, Eq, Generic)

instance ToJSON Workspace where
  toJSON     = genericToJSON jsonOptions
instance FromJSON Workspace where
  parseJSON  = genericParseJSON jsonOptions

data CreateWorkspace = CreateWorkspace
  { name          :: Text
  , workspaceType :: Maybe WorkspaceType
  , ghOwner       :: Maybe Text
  , ghRepo        :: Maybe Text
  } deriving (Show, Eq, Generic)

instance ToJSON CreateWorkspace where
  toJSON     = genericToJSON jsonOptions
instance FromJSON CreateWorkspace where
  parseJSON  = genericParseJSON jsonOptions

-- | Workspace updates deliberately expose only the display name.  Workspace
-- identity, type, GitHub binding, and membership are managed by their own
-- contracts and must never be writable through a rename request.
newtype UpdateWorkspace = UpdateWorkspace
  { name :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON UpdateWorkspace where
  toJSON uw = object ["name" .= uw.name]
instance FromJSON UpdateWorkspace where
  parseJSON = withObject "UpdateWorkspace" $ \o -> do
    let unexpected = filter (/= "name") (Key.toText <$> KM.keys o)
    unless (null unexpected) $ fail $ "UpdateWorkspace accepts only name; unexpected fields: " <> show unexpected
    UpdateWorkspace <$> o .: "name"

data WorkspaceTaskDependencyLink = WorkspaceTaskDependencyLink
  { taskId :: UUID, dependsOnId :: UUID } deriving (Show, Eq, Generic)
instance ToJSON WorkspaceTaskDependencyLink where toJSON = genericToJSON jsonOptions
instance FromJSON WorkspaceTaskDependencyLink where parseJSON = genericParseJSON jsonOptions

data WorkspaceCardHydration = WorkspaceCardHydration
  { taskDependencies :: [WorkspaceTaskDependencyLink] } deriving (Show, Eq, Generic)
instance ToJSON WorkspaceCardHydration where toJSON = genericToJSON jsonOptions
instance FromJSON WorkspaceCardHydration where parseJSON = genericParseJSON jsonOptions

------------------------------------------------------------------------
-- Project
------------------------------------------------------------------------

data Project = Project
  { id          :: UUID
  , workspaceId :: UUID
  , parentId    :: Maybe UUID
  , name        :: Text
  , description :: Maybe Text
  , status      :: ProjectStatus
  , priority    :: Int
  , metadata    :: Value
  , createdAt   :: UTCTime
  , updatedAt   :: UTCTime
  } deriving (Show, Eq, Generic)

instance ToJSON Project where
  toJSON     = genericToJSON jsonOptions
instance FromJSON Project where
  parseJSON  = genericParseJSON jsonOptions

data CreateProject = CreateProject
  { workspaceId :: UUID
  , parentId    :: Maybe UUID
  , name        :: Text
  , description :: Maybe Text
  , priority    :: Maybe Int
  , metadata    :: Maybe Value
  } deriving (Show, Eq, Generic)

instance ToJSON CreateProject where
  toJSON     = genericToJSON jsonOptions
instance FromJSON CreateProject where
  parseJSON  = genericParseJSON jsonOptions

data UpdateProject = UpdateProject
  { name        :: Maybe Text
  , description :: FieldUpdate Text
  , parentId    :: FieldUpdate UUID
  , status      :: Maybe ProjectStatus
  , priority    :: Maybe Int
  , metadata    :: Maybe Value
  } deriving (Show, Eq, Generic)

data ProjectListQuery = ProjectListQuery
  { workspaceId :: Maybe UUID, status :: Maybe ProjectStatus, query :: Maybe Text, searchLanguage :: Maybe Text
  , createdAfter :: Maybe UTCTime, createdBefore :: Maybe UTCTime, updatedAfter :: Maybe UTCTime, updatedBefore :: Maybe UTCTime
  , limit :: Maybe Int, offset :: Maybe Int
  } deriving (Show, Eq, Generic)
instance ToJSON ProjectListQuery where toJSON = genericToJSON jsonOptions
instance FromJSON ProjectListQuery where parseJSON = genericParseJSON jsonOptions

data ProjectOverview = ProjectOverview
  { project :: Project, tasks :: [Task], subprojects :: [Project], readinessRollup :: ProjectReadinessRollup }
  deriving (Show, Eq, Generic)
instance ToJSON ProjectOverview where toJSON = genericToJSON jsonOptions
instance FromJSON ProjectOverview where parseJSON = genericParseJSON jsonOptions

data ProjectReadinessRollup = ProjectReadinessRollup
  { openProjectCount :: Int, closedProjectCount :: Int, openTaskCount :: Int, doneTaskCount :: Int
  , cancelledTaskCount :: Int, blockedTaskCount :: Int, dependencyBlockedTaskCount :: Int
  , openDependencyCount :: Int, completionReady :: Bool }
  deriving (Show, Eq, Generic)
instance ToJSON ProjectReadinessRollup where toJSON = genericToJSON jsonOptions
instance FromJSON ProjectReadinessRollup where parseJSON = genericParseJSON jsonOptions

-- | The deliberately small project payload used by workspace navigation.  Full
-- project records (including description and metadata) stay behind the detail
-- endpoint so opening a large workspace never transfers every entity body.
data ProjectCardSummary = ProjectCardSummary
  { id :: UUID, workspaceId :: UUID, parentId :: Maybe UUID, name :: Text
  , status :: ProjectStatus, priority :: Int, createdAt :: UTCTime, updatedAt :: UTCTime
  , directProjectCount :: Int, directTaskCount :: Int, hasChildren :: Bool
  , readinessRollup :: ProjectReadinessRollup
  } deriving (Show, Eq, Generic)
instance ToJSON ProjectCardSummary where toJSON = genericToJSON jsonOptions
instance FromJSON ProjectCardSummary where parseJSON = genericParseJSON jsonOptions

instance ToJSON UpdateProject where
  toJSON up = object $ catMaybes
    [ ("name" .=)     <$> up.name
    , fieldUpdatePair "description" up.description
    , fieldUpdatePair "parent_id" up.parentId
    , ("status" .=)   <$> up.status
    , ("priority" .=) <$> up.priority
    , ("metadata" .=) <$> up.metadata
    ]
instance FromJSON UpdateProject where
  parseJSON = withObject "UpdateProject" $ \o -> UpdateProject
    <$> o .:? "name"
    <*> parseFieldUpdate o "description"
    <*> parseFieldUpdate o "parent_id"
    <*> o .:? "status"
    <*> o .:? "priority"
    <*> o .:? "metadata"

------------------------------------------------------------------------
-- Task
------------------------------------------------------------------------

data Task = Task
  { id              :: UUID
  , workspaceId     :: UUID
  , projectId       :: Maybe UUID
  , parentId        :: Maybe UUID
  , title           :: Text
  , description     :: Maybe Text
  , status          :: TaskStatus
  , priority        :: Int
  , metadata        :: Value
  , dueAt           :: Maybe UTCTime
  , completedAt     :: Maybe UTCTime
  , dependencyCount :: Int
  , createdAt       :: UTCTime
  , updatedAt       :: UTCTime
  } deriving (Show, Eq, Generic)

instance ToJSON Task where
  toJSON     = genericToJSON jsonOptions
instance FromJSON Task where
  parseJSON  = genericParseJSON jsonOptions

data NextTaskCandidate = NextTaskCandidate
  { task                :: Task
  , completionGated     :: Bool
  , openDescendantCount :: Int
  , dependencyBlocked   :: Bool
  , openDependencyCount :: Int
  } deriving (Show, Eq, Generic)

instance ToJSON NextTaskCandidate where
  toJSON     = genericToJSON jsonOptions
instance FromJSON NextTaskCandidate where
  parseJSON  = genericParseJSON jsonOptions

-- | Current dependency-derived blocking state for a task.  This intentionally
-- separates dependency auto-blocking from completion-gating subtasks.
data TaskDependencyAutoBlockSnapshot = TaskDependencyAutoBlockSnapshot
  { task                :: Task
  , autoBlocked         :: Bool
  , openDependencyCount :: Int
  } deriving (Show, Eq, Generic)

instance ToJSON TaskDependencyAutoBlockSnapshot where
  toJSON     = genericToJSON jsonOptions
instance FromJSON TaskDependencyAutoBlockSnapshot where
  parseJSON  = genericParseJSON jsonOptions

data TaskDependencyStatusChange = TaskDependencyStatusChange
  { task                    :: Task
  , previousStatus          :: TaskStatus
  , currentStatus           :: TaskStatus
  , previousAutoBlocked     :: Bool
  , autoBlocked             :: Bool
  , previousOpenDependencyCount :: Int
  , openDependencyCount     :: Int
  , reason                  :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON TaskDependencyStatusChange where
  toJSON     = genericToJSON jsonOptions
instance FromJSON TaskDependencyStatusChange where
  parseJSON  = genericParseJSON jsonOptions

data LinkDependency = LinkDependency
  { dependsOnId :: UUID } deriving (Show, Eq, Generic)

instance ToJSON LinkDependency where
  toJSON = genericToJSON jsonOptions
instance FromJSON LinkDependency where
  parseJSON = genericParseJSON jsonOptions

data DependencyMutationResult = DependencyMutationResult
  { action        :: Text
  , taskId        :: UUID
  , dependsOnId   :: UUID
  , affectedTasks :: [TaskDependencyStatusChange]
  } deriving (Show, Eq, Generic)

instance ToJSON DependencyMutationResult where
  toJSON     = genericToJSON jsonOptions
instance FromJSON DependencyMutationResult where
  parseJSON  = genericParseJSON jsonOptions

-- | Task update response that remains backward-compatible with plain Task JSON
-- decoders by flattening the task fields and adding dependency_effects.
data TaskMutationResult = TaskMutationResult
  { task              :: Task
  , dependencyEffects :: [TaskDependencyStatusChange]
  } deriving (Show, Eq, Generic)

instance ToJSON TaskMutationResult where
  toJSON result = case toJSON result.task of
    Object obj -> Object (KM.insert "dependency_effects" (toJSON result.dependencyEffects) obj)
    other      -> object ["task" .= other, "dependency_effects" .= result.dependencyEffects]

instance FromJSON TaskMutationResult where
  parseJSON value@(Object obj) = TaskMutationResult
    <$> parseJSON value
    <*> obj .:? "dependency_effects" .!= []
  parseJSON other = TaskMutationResult
    <$> parseJSON other
    <*> pure []

data CreateTask = CreateTask
  { workspaceId :: UUID
  , projectId   :: Maybe UUID
  , parentId    :: Maybe UUID
  , title       :: Text
  , description :: Maybe Text
  , priority    :: Maybe Int
  , metadata    :: Maybe Value
  , dueAt       :: Maybe UTCTime
  } deriving (Show, Eq, Generic)

instance ToJSON CreateTask where
  toJSON     = genericToJSON jsonOptions
instance FromJSON CreateTask where
  parseJSON  = genericParseJSON jsonOptions

data UpdateTask = UpdateTask
  { title       :: Maybe Text
  , description :: FieldUpdate Text
  , projectId   :: FieldUpdate UUID
  , parentId    :: FieldUpdate UUID
  , status      :: Maybe TaskStatus
  , priority    :: Maybe Int
  , metadata    :: Maybe Value
  , dueAt       :: FieldUpdate UTCTime
  } deriving (Show, Eq, Generic)

data TaskListQuery = TaskListQuery
  { workspaceId :: Maybe UUID, projectId :: Maybe UUID, status :: Maybe TaskStatus, priority :: Maybe Int
  , query :: Maybe Text, searchLanguage :: Maybe Text, createdAfter :: Maybe UTCTime, createdBefore :: Maybe UTCTime
  , updatedAfter :: Maybe UTCTime, updatedBefore :: Maybe UTCTime, limit :: Maybe Int, offset :: Maybe Int
  } deriving (Show, Eq, Generic)
instance ToJSON TaskListQuery where toJSON = genericToJSON jsonOptions
instance FromJSON TaskListQuery where parseJSON = genericParseJSON jsonOptions

data TaskDependencySummary = TaskDependencySummary { id :: UUID, name :: Text } deriving (Show, Eq, Generic)
instance ToJSON TaskDependencySummary where toJSON = genericToJSON jsonOptions
instance FromJSON TaskDependencySummary where parseJSON = genericParseJSON jsonOptions

data TaskReadinessRollup = TaskReadinessRollup
  { openSubtaskCount :: Int, doneSubtaskCount :: Int, cancelledSubtaskCount :: Int, blockedSubtaskCount :: Int
  , dependencyBlockedTaskCount :: Int, openDependencyCount :: Int, completionReady :: Bool }
  deriving (Show, Eq, Generic)
instance ToJSON TaskReadinessRollup where toJSON = genericToJSON jsonOptions
instance FromJSON TaskReadinessRollup where parseJSON = genericParseJSON jsonOptions

-- | The task equivalent of 'ProjectCardSummary'.  It contains only data needed
-- to draw a card and its readiness badge; descriptions, metadata and dependency
-- names are deliberately fetched on demand.
data TaskCardSummary = TaskCardSummary
  { id :: UUID, workspaceId :: UUID, projectId :: Maybe UUID, parentId :: Maybe UUID, title :: Text
  , status :: TaskStatus, priority :: Int, dueAt :: Maybe UTCTime, completedAt :: Maybe UTCTime
  , dependencyCount :: Int, createdAt :: UTCTime, updatedAt :: UTCTime
  , directSubtaskCount :: Int, hasChildren :: Bool, readinessRollup :: TaskReadinessRollup
  } deriving (Show, Eq, Generic)
instance ToJSON TaskCardSummary where toJSON = genericToJSON jsonOptions
instance FromJSON TaskCardSummary where parseJSON = genericParseJSON jsonOptions

-- | Dependency names are not part of a card.  This page is intentionally
-- independent so expanding a dependency section cannot restart navigation.
data TaskDependencyPage = TaskDependencyPage
  { items :: [TaskDependencySummary], hasMore :: Bool } deriving (Show, Eq, Generic)
instance ToJSON TaskDependencyPage where toJSON = genericToJSON jsonOptions
instance FromJSON TaskDependencyPage where parseJSON = genericParseJSON jsonOptions

data TaskOverview = TaskOverview
  { task :: Task, dependencies :: [TaskDependencySummary], readinessRollup :: TaskReadinessRollup }
  deriving (Show, Eq, Generic)
instance ToJSON TaskOverview where toJSON = genericToJSON jsonOptions
instance FromJSON TaskOverview where parseJSON = genericParseJSON jsonOptions

------------------------------------------------------------------------
-- Bounded workspace navigation
------------------------------------------------------------------------

maxNavigationPageSize, maxNavigationOffset, maxNavigationBatchIds, maxFocusAncestors :: Int
maxNavigationPageSize = 100
maxNavigationOffset = 100000
maxNavigationBatchIds = 100
maxFocusAncestors = 64

-- | A typed branch selector.  Project and task branches are intentionally
-- distinct: a project branch returns child projects before root tasks, while a
-- task branch returns only child tasks.
data NavigationParent
  = NavigationWorkspaceRoot
  | NavigationProjectBranch UUID
  | NavigationTaskBranch UUID
  deriving (Show, Eq, Generic)

instance ToJSON NavigationParent where
  toJSON NavigationWorkspaceRoot = object ["kind" .= ("workspace_root" :: Text)]
  toJSON (NavigationProjectBranch parent) = object ["kind" .= ("project" :: Text), "parent_id" .= parent]
  toJSON (NavigationTaskBranch parent) = object ["kind" .= ("task" :: Text), "parent_id" .= parent]
instance FromJSON NavigationParent where
  parseJSON = withObject "NavigationParent" $ \o -> do
    kind <- o .: "kind" :: Parser Text
    case kind of
      "workspace_root" -> pure NavigationWorkspaceRoot
      "project" -> NavigationProjectBranch <$> o .: "parent_id"
      "task" -> NavigationTaskBranch <$> o .: "parent_id"
      _ -> fail "navigation parent kind must be workspace_root, project, or task"

-- | Filter fields mirror the existing workspace tree semantics.  The API owns
-- descendant/ancestor matching; clients never need the entire tree to filter.
data NavigationFilter = NavigationFilter
  { showOnly :: Maybe Text, projectStatuses :: [ProjectStatus], taskStatuses :: [TaskStatus]
  -- Keep the priority operator and value distinct.  The UI has exact/above/
  -- below modes, so reducing it to an equality at this boundary silently
  -- changed the existing tree-filter semantics.
  , priorityMode :: Maybe Text, priorityValue :: Maybe Int, query :: Maybe Text
  } deriving (Show, Eq, Generic)
instance ToJSON NavigationFilter where toJSON = genericToJSON jsonOptions
instance FromJSON NavigationFilter where parseJSON = genericParseJSON jsonOptions

data NavigationBranchRequest = NavigationBranchRequest
  { parent :: NavigationParent, projectLimit :: Maybe Int, projectOffset :: Maybe Int
  , taskLimit :: Maybe Int, taskOffset :: Maybe Int, filters :: Maybe NavigationFilter
  } deriving (Show, Eq, Generic)
instance ToJSON NavigationBranchRequest where toJSON = genericToJSON jsonOptions
instance FromJSON NavigationBranchRequest where parseJSON = genericParseJSON jsonOptions

data NavigationPage a = NavigationPage { items :: [a], hasMore :: Bool }
  deriving (Show, Eq, Generic)
instance ToJSON a => ToJSON (NavigationPage a) where toJSON = genericToJSON jsonOptions
instance FromJSON a => FromJSON (NavigationPage a) where parseJSON = genericParseJSON jsonOptions

data NavigationBranchResponse = NavigationBranchResponse
  { workspaceId :: UUID, parent :: NavigationParent
  , projects :: NavigationPage ProjectCardSummary, tasks :: NavigationPage TaskCardSummary
  } deriving (Show, Eq, Generic)
instance ToJSON NavigationBranchResponse where toJSON = genericToJSON jsonOptions
instance FromJSON NavigationBranchResponse where parseJSON = genericParseJSON jsonOptions

data NavigationEntityType = NavigationProject | NavigationTask deriving (Show, Eq, Generic)
instance ToJSON NavigationEntityType where
  toJSON NavigationProject = String "project"
  toJSON NavigationTask = String "task"
instance FromJSON NavigationEntityType where
  parseJSON = withText "NavigationEntityType" $ \case
    "project" -> pure NavigationProject
    "task" -> pure NavigationTask
    _ -> fail "entity_type must be project or task"

data NavigationSummary = NavigationProjectSummary ProjectCardSummary | NavigationTaskSummary TaskCardSummary
  deriving (Show, Eq, Generic)
instance ToJSON NavigationSummary where
  toJSON (NavigationProjectSummary summary) = object ["entity_type" .= NavigationProject, "summary" .= summary]
  toJSON (NavigationTaskSummary summary) = object ["entity_type" .= NavigationTask, "summary" .= summary]
instance FromJSON NavigationSummary where
  parseJSON = withObject "NavigationSummary" $ \o -> do
    entityType <- o .: "entity_type"
    case entityType of
      NavigationProject -> NavigationProjectSummary <$> o .: "summary"
      NavigationTask -> NavigationTaskSummary <$> o .: "summary"

data NavigationFocusResponse = NavigationFocusResponse
  { workspaceId :: UUID, target :: NavigationSummary, ancestors :: [NavigationSummary]
  , ancestorsTruncated :: Bool, nextAncestorOffset :: Maybe Int
  } deriving (Show, Eq, Generic)
instance ToJSON NavigationFocusResponse where toJSON = genericToJSON jsonOptions
instance FromJSON NavigationFocusResponse where parseJSON = genericParseJSON jsonOptions

data NavigationSummariesRequest = NavigationSummariesRequest
  { projectIds :: [UUID], taskIds :: [UUID] } deriving (Show, Eq, Generic)
instance ToJSON NavigationSummariesRequest where toJSON = genericToJSON jsonOptions
instance FromJSON NavigationSummariesRequest where parseJSON = genericParseJSON jsonOptions

data NavigationSummariesResponse = NavigationSummariesResponse
  { projects :: [ProjectCardSummary], tasks :: [TaskCardSummary]
  , missingProjectIds :: [UUID], missingTaskIds :: [UUID]
  } deriving (Show, Eq, Generic)
instance ToJSON NavigationSummariesResponse where toJSON = genericToJSON jsonOptions
instance FromJSON NavigationSummariesResponse where parseJSON = genericParseJSON jsonOptions

validateNavigationPage :: Maybe Int -> Maybe Int -> [Text]
validateNavigationPage maybeLimit maybeOffset =
  [ "limit must be between 1 and " <> T.pack (show maxNavigationPageSize)
  | Just value <- [maybeLimit], value < 1 || value > maxNavigationPageSize
  ] ++
  [ "offset must be between 0 and " <> T.pack (show maxNavigationOffset)
  | Just value <- [maybeOffset], value < 0 || value > maxNavigationOffset
  ]

validateNavigationSummariesRequest :: NavigationSummariesRequest -> [Text]
validateNavigationSummariesRequest request =
  let allIds = request.projectIds ++ request.taskIds
  in [ "at most " <> T.pack (show maxNavigationBatchIds) <> " summary IDs are allowed"
     | length allIds > maxNavigationBatchIds
     ] ++ [ "summary IDs must be unique" | length (nub allIds) /= length allIds ]

instance ToJSON UpdateTask where
  toJSON ut = object $ catMaybes
    [ ("title" .=)    <$> ut.title
    , fieldUpdatePair "description" ut.description
    , fieldUpdatePair "project_id" ut.projectId
    , fieldUpdatePair "parent_id" ut.parentId
    , ("status" .=)   <$> ut.status
    , ("priority" .=) <$> ut.priority
    , ("metadata" .=) <$> ut.metadata
    , fieldUpdatePair "due_at" ut.dueAt
    ]
instance FromJSON UpdateTask where
  parseJSON = withObject "UpdateTask" $ \o -> UpdateTask
    <$> o .:? "title"
    <*> parseFieldUpdate o "description"
    <*> parseFieldUpdate o "project_id"
    <*> parseFieldUpdate o "parent_id"
    <*> o .:? "status"
    <*> o .:? "priority"
    <*> o .:? "metadata"
    <*> parseFieldUpdate o "due_at"

------------------------------------------------------------------------
-- Workspace groups
------------------------------------------------------------------------

data WorkspaceGroup = WorkspaceGroup
  { id          :: UUID
  , name        :: Text
  , description :: Maybe Text
  , createdAt   :: UTCTime
  , updatedAt   :: UTCTime
  } deriving (Show, Eq, Generic)

instance ToJSON WorkspaceGroup where
  toJSON     = genericToJSON jsonOptions
instance FromJSON WorkspaceGroup where
  parseJSON  = genericParseJSON jsonOptions

data CreateWorkspaceGroup = CreateWorkspaceGroup
  { name        :: Text
  , description :: Maybe Text
  } deriving (Show, Eq, Generic)

instance ToJSON CreateWorkspaceGroup where
  toJSON     = genericToJSON jsonOptions
instance FromJSON CreateWorkspaceGroup where
  parseJSON  = genericParseJSON jsonOptions

-- | Request body for adding an active workspace to a workspace group.
newtype WorkspaceGroupMemberInput = WorkspaceGroupMemberInput
  { workspaceId :: UUID
  } deriving (Show, Eq, Generic)

instance ToJSON WorkspaceGroupMemberInput where
  toJSON     = genericToJSON jsonOptions
instance FromJSON WorkspaceGroupMemberInput where
  parseJSON  = genericParseJSON jsonOptions

------------------------------------------------------------------------
-- Activity timeline
------------------------------------------------------------------------

data ActivityEvent = ActivityEvent
  { eventType   :: Text      -- "created", "updated", "deleted"
  , entityType  :: Text      -- "observation", "project", "task"
  , entityId    :: UUID
  , workspaceId :: UUID
  , summary     :: Text
  , timestamp   :: UTCTime
  } deriving (Show, Eq, Generic)

instance ToJSON ActivityEvent where
  toJSON     = genericToJSON jsonOptions
instance FromJSON ActivityEvent where
  parseJSON  = genericParseJSON jsonOptions

-- | Curated workspace timeline event derived from audit data for the web UI.
-- The shape intentionally exposes lifecycle context instead of raw audit JSON.
data WorkspaceTimelineEvent = WorkspaceTimelineEvent
  { id               :: Text
  , workspaceId      :: UUID
  , eventType        :: Text
  , entityType       :: Text
  , entityId         :: UUID
  , title            :: Text
  , occurredAt       :: UTCTime
  , actor            :: Maybe TimelineActor
  , project          :: Maybe TimelineProjectContext
  , parentTask       :: Maybe TimelineTaskContext
  , statusTransition :: Maybe TimelineStatusTransition
  , navigation       :: TimelineNavigation
  , sourceAuditId    :: Maybe UUID
  } deriving (Show, Eq, Generic)

instance ToJSON WorkspaceTimelineEvent where
  toJSON = genericToJSON jsonOptions
instance FromJSON WorkspaceTimelineEvent where
  parseJSON = genericParseJSON jsonOptions

data TimelineActor = TimelineActor
  { actorType  :: Maybe Text
  , actorId    :: Maybe Text
  , actorLabel :: Maybe Text
  } deriving (Show, Eq, Generic)

instance ToJSON TimelineActor where
  toJSON TimelineActor {..} = object $ catMaybes
    [ ("type" .=) <$> actorType
    , ("id" .=) <$> actorId
    , ("label" .=) <$> actorLabel
    ]
instance FromJSON TimelineActor where
  parseJSON = withObject "TimelineActor" $ \o ->
    TimelineActor
      <$> o .:? "type"
      <*> o .:? "id"
      <*> o .:? "label"

data TimelineProjectContext = TimelineProjectContext
  { projectContextId   :: UUID
  , projectContextName :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON TimelineProjectContext where
  toJSON TimelineProjectContext {..} = object
    [ "id" .= projectContextId
    , "name" .= projectContextName
    ]
instance FromJSON TimelineProjectContext where
  parseJSON = withObject "TimelineProjectContext" $ \o ->
    TimelineProjectContext
      <$> o .: "id"
      <*> o .: "name"

data TimelineTaskContext = TimelineTaskContext
  { taskContextId    :: UUID
  , taskContextTitle :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON TimelineTaskContext where
  toJSON TimelineTaskContext {..} = object
    [ "id" .= taskContextId
    , "title" .= taskContextTitle
    ]
instance FromJSON TimelineTaskContext where
  parseJSON = withObject "TimelineTaskContext" $ \o ->
    TimelineTaskContext
      <$> o .: "id"
      <*> o .: "title"

data TimelineStatusTransition = TimelineStatusTransition
  { transitionFrom :: Text
  , transitionTo   :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON TimelineStatusTransition where
  toJSON TimelineStatusTransition {..} = object
    [ "from" .= transitionFrom
    , "to" .= transitionTo
    ]
instance FromJSON TimelineStatusTransition where
  parseJSON = withObject "TimelineStatusTransition" $ \o ->
    TimelineStatusTransition
      <$> o .: "from"
      <*> o .: "to"

data TimelineNavigation = TimelineNavigation
  { navigationEntityType :: Text
  , navigationEntityId   :: UUID
  } deriving (Show, Eq, Generic)

instance ToJSON TimelineNavigation where
  toJSON TimelineNavigation {..} = object
    [ "entity_type" .= navigationEntityType
    , "entity_id" .= navigationEntityId
    ]
instance FromJSON TimelineNavigation where
  parseJSON = withObject "TimelineNavigation" $ \o ->
    TimelineNavigation
      <$> o .: "entity_type"
      <*> o .: "entity_id"

-- | Counts for one lifecycle action group within a timeline histogram bucket.
data TimelineBucketCounts = TimelineBucketCounts
  { created   :: Int
  , completed :: Int
  , cancelled :: Int
  } deriving (Show, Eq, Generic)

instance ToJSON TimelineBucketCounts where
  toJSON     = genericToJSON jsonOptions
instance FromJSON TimelineBucketCounts where
  parseJSON  = genericParseJSON jsonOptions

-- | Timeline histogram counts split by UI entity kind.
data TimelineBucketEntityCounts = TimelineBucketEntityCounts
  { projectCounts    :: TimelineBucketCounts
  , subprojectCounts :: TimelineBucketCounts
  , taskCounts       :: TimelineBucketCounts
  , subtaskCounts    :: TimelineBucketCounts
  } deriving (Show, Eq, Generic)

instance ToJSON TimelineBucketEntityCounts where
  toJSON TimelineBucketEntityCounts {..} = object
    [ "project" .= projectCounts
    , "subproject" .= subprojectCounts
    , "task" .= taskCounts
    , "subtask" .= subtaskCounts
    ]
instance FromJSON TimelineBucketEntityCounts where
  parseJSON = withObject "TimelineBucketEntityCounts" $ \o ->
    TimelineBucketEntityCounts
      <$> o .: "project"
      <*> o .: "subproject"
      <*> o .: "task"
      <*> o .: "subtask"

-- | Canonical lifecycle actions for one entity series in a Timeline bucket.
-- The legacy 'TimelineBucketCounts' type remains unchanged for API-v1 clients.
data TimelineBucketSeriesCounts = TimelineBucketSeriesCounts
  { created :: Int
  , completed :: Int
  , deleted :: Int
  } deriving (Show, Eq, Generic)

instance ToJSON TimelineBucketSeriesCounts where
  toJSON = genericToJSON jsonOptions
instance FromJSON TimelineBucketSeriesCounts where
  parseJSON = genericParseJSON jsonOptions

-- | Canonical Timeline series. Nested projects fold into 'seriesProject'.
data TimelineBucketSeries = TimelineBucketSeries
  { seriesProject     :: TimelineBucketSeriesCounts
  , seriesTask        :: TimelineBucketSeriesCounts
  , seriesSubtask     :: TimelineBucketSeriesCounts
  , seriesObservation :: TimelineBucketSeriesCounts
  } deriving (Show, Eq, Generic)

instance ToJSON TimelineBucketSeries where
  toJSON TimelineBucketSeries {..} = object
    [ "project" .= seriesProject
    , "task" .= seriesTask
    , "subtask" .= seriesSubtask
    , "observation" .= seriesObservation
    ]
instance FromJSON TimelineBucketSeries where
  parseJSON = withObject "TimelineBucketSeries" $ \o ->
    TimelineBucketSeries
      <$> o .: "project"
      <*> o .: "task"
      <*> o .: "subtask"
      <*> o .: "observation"

-- | One horizontal histogram bucket for the workspace Timeline.
data WorkspaceTimelineBucket = WorkspaceTimelineBucket
  { timelineBucketStart  :: UTCTime
  , timelineBucketEnd    :: UTCTime
  , timelineBucketLabel  :: Text
  , timelineBucketCounts :: TimelineBucketEntityCounts
  , timelineBucketTotals :: TimelineBucketCounts
  , timelineBucketSeries :: TimelineBucketSeries
  , timelineBucketSeriesTotals :: TimelineBucketSeriesCounts
  } deriving (Show, Eq, Generic)

instance ToJSON WorkspaceTimelineBucket where
  toJSON WorkspaceTimelineBucket {..} = object
    [ "bucket_start" .= timelineBucketStart
    , "bucket_end" .= timelineBucketEnd
    , "label" .= timelineBucketLabel
    , "counts" .= timelineBucketCounts
    , "totals" .= timelineBucketTotals
    , "series" .= timelineBucketSeries
    , "series_totals" .= timelineBucketSeriesTotals
    ]
instance FromJSON WorkspaceTimelineBucket where
  parseJSON = withObject "WorkspaceTimelineBucket" $ \o ->
    WorkspaceTimelineBucket
      <$> o .: "bucket_start"
      <*> o .: "bucket_end"
      <*> o .: "label"
      <*> o .: "counts"
      <*> o .: "totals"
      <*> o .: "series"
      <*> o .: "series_totals"

-- | Capped bucket response for the workspace Timeline histogram.
data WorkspaceTimelineBucketsResponse = WorkspaceTimelineBucketsResponse
  { timelineBucketsWorkspaceId :: UUID
  , timelineBucketsSince       :: UTCTime
  , timelineBucketsUntil       :: UTCTime
  , timelineBucketsBucket      :: Text
  , timelineBucketsBuckets     :: [WorkspaceTimelineBucket]
  } deriving (Show, Eq, Generic)

instance ToJSON WorkspaceTimelineBucketsResponse where
  toJSON WorkspaceTimelineBucketsResponse {..} = object
    [ "workspace_id" .= timelineBucketsWorkspaceId
    , "since" .= timelineBucketsSince
    , "until" .= timelineBucketsUntil
    , "bucket" .= timelineBucketsBucket
    , "buckets" .= timelineBucketsBuckets
    ]
instance FromJSON WorkspaceTimelineBucketsResponse where
  parseJSON = withObject "WorkspaceTimelineBucketsResponse" $ \o ->
    WorkspaceTimelineBucketsResponse
      <$> o .: "workspace_id"
      <*> o .: "since"
      <*> o .: "until"
      <*> o .: "bucket"
      <*> o .: "buckets"

------------------------------------------------------------------------
-- Saved views
------------------------------------------------------------------------

data SavedView = SavedView
  { id          :: UUID
  , workspaceId :: UUID
  , name        :: Text
  , description :: Maybe Text
  , entityType  :: Text
  , queryParams :: Value
  , createdAt   :: UTCTime
  , updatedAt   :: UTCTime
  } deriving (Show, Eq, Generic)

instance ToJSON SavedView where
  toJSON     = genericToJSON jsonOptions
instance FromJSON SavedView where
  parseJSON  = genericParseJSON jsonOptions

data CreateSavedView = CreateSavedView
  { workspaceId :: UUID
  , name        :: Text
  , description :: Maybe Text
  , entityType  :: Text
  , queryParams :: Value
  } deriving (Show, Eq, Generic)

instance ToJSON CreateSavedView where
  toJSON     = genericToJSON jsonOptions
instance FromJSON CreateSavedView where
  parseJSON  = genericParseJSON jsonOptions

data UpdateSavedView = UpdateSavedView
  { name        :: Maybe Text
  , description :: FieldUpdate Text
  , queryParams :: Maybe Value
  } deriving (Show, Eq, Generic)

instance FromJSON UpdateSavedView where
  parseJSON = withObject "UpdateSavedView" $ \o -> do
    UpdateSavedView
      <$> o .:? "name"
      <*> parseFieldUpdate o "description"
      <*> o .:? "query_params"

instance ToJSON UpdateSavedView where
  toJSON uv = object $ catMaybes
    [ ("name" .=)        <$> uv.name
    , fieldUpdatePair "description" uv.description
    , ("query_params" .=) <$> uv.queryParams
    ]

data SavedViewListQuery = SavedViewListQuery
  { workspaceId :: UUID
  , limit       :: Maybe Int
  , offset      :: Maybe Int
  } deriving (Show, Eq, Generic)

instance ToJSON SavedViewListQuery where
  toJSON     = genericToJSON jsonOptions
instance FromJSON SavedViewListQuery where
  parseJSON  = genericParseJSON jsonOptions

------------------------------------------------------------------------
-- Audit log
------------------------------------------------------------------------

data AuditAction = AuditCreate | AuditUpdate | AuditDelete
  deriving (Show, Eq, Ord, Bounded, Enum, Generic)

auditActionToText :: AuditAction -> Text
auditActionToText AuditCreate = "create"
auditActionToText AuditUpdate = "update"
auditActionToText AuditDelete = "delete"

auditActionFromText :: Text -> Maybe AuditAction
auditActionFromText "create" = Just AuditCreate
auditActionFromText "update" = Just AuditUpdate
auditActionFromText "delete" = Just AuditDelete
auditActionFromText _        = Nothing

instance ToJSON AuditAction where
  toJSON = String . auditActionToText

instance FromJSON AuditAction where
  parseJSON = withText "AuditAction" $ \t ->
    case auditActionFromText t of
      Just a  -> pure a
      Nothing -> fail $ "Invalid audit action: " <> T.unpack t

data AuditLogEntry = AuditLogEntry
  { id         :: UUID
  , workspaceId :: Maybe UUID
  , entityType :: Text
  , entityId   :: Text
  , action     :: AuditAction
  , oldValues  :: Maybe Value
  , newValues  :: Maybe Value
  , requestId  :: Maybe Text
  , actorType  :: Maybe Text
  , actorId    :: Maybe Text
  , actorLabel :: Maybe Text
  , changedAt  :: UTCTime
  } deriving (Show, Eq, Generic)

instance ToJSON AuditLogEntry where
  toJSON     = genericToJSON jsonOptions
instance FromJSON AuditLogEntry where
  parseJSON  = genericParseJSON jsonOptions

data AuditLogQuery = AuditLogQuery
  { workspaceId :: Maybe UUID
  , entityType :: Maybe Text
  , entityId   :: Maybe Text
  , action     :: Maybe AuditAction
  , since      :: Maybe UTCTime
  , until      :: Maybe UTCTime
  , limit      :: Maybe Int
  , offset     :: Maybe Int
  } deriving (Show, Eq, Generic)

instance ToJSON AuditLogQuery where
  toJSON     = genericToJSON jsonOptions
instance FromJSON AuditLogQuery where
  parseJSON  = genericParseJSON jsonOptions

data RevertResult = RevertResult
  { auditEntry :: AuditLogEntry
  , entity     :: Maybe Value
  } deriving (Show, Eq, Generic)

instance ToJSON RevertResult where
  toJSON     = genericToJSON jsonOptions
instance FromJSON RevertResult where
  parseJSON  = genericParseJSON jsonOptions

------------------------------------------------------------------------
-- WebSocket auth
------------------------------------------------------------------------

newtype WebSocketTicketRequest = WebSocketTicketRequest
  { workspaceId :: UUID
  } deriving (Show, Eq, Generic)

instance ToJSON WebSocketTicketRequest where
  toJSON = genericToJSON jsonOptions
instance FromJSON WebSocketTicketRequest where
  parseJSON = genericParseJSON jsonOptions

data WebSocketTicketResponse = WebSocketTicketResponse
  { ticket    :: Text
  , expiresAt :: UTCTime
  } deriving (Show, Eq, Generic)

instance ToJSON WebSocketTicketResponse where
  toJSON = genericToJSON jsonOptions
instance FromJSON WebSocketTicketResponse where
  parseJSON = genericParseJSON jsonOptions

-- | Public change-stream scope.  This is deliberately separate from the
-- database scope type: clients name only a scope, never a cursor or audience.
data ChangeStreamScopeRequest
  = ChangeStreamWorkspace !UUID
  | ChangeStreamGlobal
  deriving (Show, Eq, Generic)

instance ToJSON ChangeStreamScopeRequest where
  toJSON ChangeStreamGlobal = object ["scope" .= ("global" :: Text)]
  toJSON (ChangeStreamWorkspace workspace) = object
    [ "scope" .= ("workspace" :: Text), "workspace_id" .= workspace ]
instance FromJSON ChangeStreamScopeRequest where
  parseJSON = withObject "ChangeStreamScopeRequest" $ \o -> do
    kind <- o .: "scope"
    case (kind :: Text) of
      "global"
        | KM.member "workspace_id" o -> fail "workspace_id is only valid for workspace scope"
        | otherwise -> pure ChangeStreamGlobal
      "workspace" -> ChangeStreamWorkspace <$> o .: "workspace_id"
      _ -> fail "scope must be workspace or global"

-- | The projection of a durable change-stream snapshot.  The omitted request
-- field deliberately remains @full_v1@ for compatibility with existing
-- clients; the bounded workspace shell is an explicit opt-in only.
data SnapshotProfile = FullV1 | WorkspaceShellV1
  deriving (Show, Eq, Generic)

snapshotProfileToText :: SnapshotProfile -> Text
snapshotProfileToText FullV1 = "full_v1"
snapshotProfileToText WorkspaceShellV1 = "workspace_shell_v1"

instance ToJSON SnapshotProfile where toJSON = String . snapshotProfileToText
instance FromJSON SnapshotProfile where
  parseJSON = withText "SnapshotProfile" $ \case
    "full_v1" -> pure FullV1
    "workspace_shell_v1" -> pure WorkspaceShellV1
    _ -> fail "snapshot_profile must be full_v1 or workspace_shell_v1"

data ChangeStreamResyncRequest = ChangeStreamResyncRequest
  { scope :: !ChangeStreamScopeRequest
  , snapshotProfile :: !(Maybe SnapshotProfile)
  , pageSize :: !(Maybe Int)
  , pageToken :: !(Maybe Text)
  , startIdempotencyKey :: !(Maybe Text)
  } deriving (Show, Eq, Generic)
instance ToJSON ChangeStreamResyncRequest where toJSON = genericToJSON jsonOptions
instance FromJSON ChangeStreamResyncRequest where
  parseJSON = withObject "ChangeStreamResyncRequest" $ \o -> do
    request <- ChangeStreamResyncRequest <$> o .: "scope" <*> o .:? "snapshot_profile" <*> o .:? "page_size" <*> o .:? "page_token" <*> o .:? "start_idempotency_key"
    case request.pageSize of
      Just size | size < 1 || size > 1000 -> fail "page_size must be between 1 and 1000"
      _ -> case (request.pageToken, request.startIdempotencyKey) of
        (Nothing, Nothing) -> fail "start_idempotency_key is required when page_token is absent"
        (Nothing, Just key) | T.length (T.strip key) < 32 || T.length key > 512 -> fail "start_idempotency_key must be between 32 and 512 characters"
        (Just token, Nothing) | T.null (T.strip token) -> fail "page_token must be nonempty"
        (Just _, Just _) -> fail "start_idempotency_key is only valid when page_token is absent"
        _ | request.pageToken /= Nothing && request.snapshotProfile /= Nothing -> fail "snapshot_profile is only valid when page_token is absent"
        _ -> pure request

-- | An immutable, allowlisted item in a resync response.  Its `data` field is
-- always an existing public REST representation, never a database row.
data ChangeStreamSnapshotItem = ChangeStreamSnapshotItem
  { schemaVersion :: !Int
  , kind :: !Text
  , data_ :: !Value
  } deriving (Show, Eq, Generic)
instance ToJSON ChangeStreamSnapshotItem where
  toJSON item = object ["schema_version" .= item.schemaVersion, "kind" .= item.kind, "data" .= item.data_]
instance FromJSON ChangeStreamSnapshotItem where
  parseJSON = withObject "ChangeStreamSnapshotItem" $ \o -> ChangeStreamSnapshotItem
    <$> o .: "schema_version" <*> o .: "kind" <*> o .: "data"

data ChangeStreamResyncResponse = ChangeStreamResyncResponse
  { items :: ![ChangeStreamSnapshotItem]
  , snapshotProfile :: !SnapshotProfile
  , hasMore :: !Bool
  , nextPageToken :: !(Maybe Text)
  , resumeToken :: !(Maybe Text)
  } deriving (Show, Eq, Generic)
instance ToJSON ChangeStreamResyncResponse where toJSON = genericToJSON jsonOptions
instance FromJSON ChangeStreamResyncResponse where parseJSON = genericParseJSON jsonOptions

-- | The canonical ticket binds the server-selected scope to the opaque token
-- created at the terminal resync page.  It intentionally has no audience
-- field: the server derives that from the authenticated principal.
data CanonicalWebSocketTicketRequest = CanonicalWebSocketTicketRequest
  { scope :: !ChangeStreamScopeRequest
  , resumeToken :: !Text
  } deriving (Show, Eq, Generic)
instance ToJSON CanonicalWebSocketTicketRequest where toJSON = genericToJSON jsonOptions
instance FromJSON CanonicalWebSocketTicketRequest where parseJSON = genericParseJSON jsonOptions

------------------------------------------------------------------------
-- Session context
------------------------------------------------------------------------

data SessionPrincipal = SessionPrincipal
  { actorType   :: Text
  , actorId     :: Text
  , actorLabel  :: Text
  , authority   :: Text
  , grantUserId :: Maybe UUID
  } deriving (Show, Eq, Generic)

instance ToJSON SessionPrincipal where
  toJSON = genericToJSON jsonOptions
instance FromJSON SessionPrincipal where
  parseJSON = genericParseJSON jsonOptions

data SessionGlobalPermissions = SessionGlobalPermissions
  { createWorkspace :: Bool
  , superadmin      :: Bool
  } deriving (Show, Eq, Generic)

instance ToJSON SessionGlobalPermissions where
  toJSON = genericToJSON jsonOptions
instance FromJSON SessionGlobalPermissions where
  parseJSON = genericParseJSON jsonOptions

data SessionWorkspaceContext = SessionWorkspaceContext
  { workspaceId :: UUID
  , role        :: Maybe Text
  , canRead     :: Bool
  , canEdit     :: Bool
  , canAdmin    :: Bool
  } deriving (Show, Eq, Generic)

instance ToJSON SessionWorkspaceContext where
  toJSON = genericToJSON jsonOptions
instance FromJSON SessionWorkspaceContext where
  parseJSON = genericParseJSON jsonOptions

data SessionContext = SessionContext
  { authMode          :: Text
  , principal         :: SessionPrincipal
  , globalPermissions :: SessionGlobalPermissions
  , workspace         :: Maybe SessionWorkspaceContext
  } deriving (Show, Eq, Generic)

instance ToJSON SessionContext where
  toJSON = genericToJSON jsonOptions
instance FromJSON SessionContext where
  parseJSON = genericParseJSON jsonOptions

------------------------------------------------------------------------
-- Pagination
------------------------------------------------------------------------

-- | Wraps a list result with pagination metadata so callers know
-- whether more pages exist without issuing a separate COUNT query.
data PaginatedResult a = PaginatedResult
  { items   :: [a]
  , hasMore :: Bool
  } deriving (Show, Eq, Generic)

instance ToJSON a => ToJSON (PaginatedResult a) where
  toJSON     = genericToJSON jsonOptions
instance FromJSON a => FromJSON (PaginatedResult a) where
  parseJSON  = genericParseJSON jsonOptions

------------------------------------------------------------------------
-- Batch operations
------------------------------------------------------------------------

newtype BatchDeleteRequest = BatchDeleteRequest
  { ids :: [UUID]
  } deriving (Show, Eq, Generic)

instance ToJSON BatchDeleteRequest where
  toJSON     = genericToJSON jsonOptions
instance FromJSON BatchDeleteRequest where
  parseJSON  = genericParseJSON jsonOptions

data BatchMoveTasksRequest = BatchMoveTasksRequest
  { taskIds   :: [UUID]
  , projectId :: Maybe UUID
  } deriving (Show, Eq, Generic)

instance ToJSON BatchMoveTasksRequest where
  toJSON     = genericToJSON jsonOptions
instance FromJSON BatchMoveTasksRequest where
  parseJSON  = genericParseJSON jsonOptions

newtype BatchResult = BatchResult { affected :: Int } deriving (Show, Eq, Generic)
instance ToJSON BatchResult where toJSON = genericToJSON jsonOptions
instance FromJSON BatchResult where parseJSON = genericParseJSON jsonOptions

data CascadeResult = CascadeResult
  { affected :: Int, projectCount :: Int, taskCount :: Int, dependencyLinkCount :: Int }
  deriving (Show, Eq, Generic)
instance ToJSON CascadeResult where toJSON = genericToJSON jsonOptions
instance FromJSON CascadeResult where parseJSON = genericParseJSON jsonOptions

validateBatchDeleteRequest :: BatchDeleteRequest -> [Text]
validateBatchDeleteRequest request =
  ["ids must contain at least one item" | null request.ids]
  <> ["ids must contain at most 100 items" | length request.ids > 100]

validateBatchMoveTasksRequest :: BatchMoveTasksRequest -> [Text]
validateBatchMoveTasksRequest request =
  ["task_ids must contain at least one item" | null request.taskIds]
  <> ["task_ids must contain at most 100 items" | length request.taskIds > 100]

data BatchUpdateProjectItem = BatchUpdateProjectItem
  { id     :: UUID
  , update :: UpdateProject
  } deriving (Show, Eq, Generic)

instance ToJSON BatchUpdateProjectItem where
  toJSON item = case toJSON item.update of
    Object o -> Object (KM.insert "id" (toJSON item.id) o)
    v        -> v

instance FromJSON BatchUpdateProjectItem where
  parseJSON = withObject "BatchUpdateProjectItem" $ \o ->
    BatchUpdateProjectItem <$> o .: "id" <*> parseJSON (Object o)

newtype BatchUpdateProjectRequest = BatchUpdateProjectRequest
  { items :: [BatchUpdateProjectItem]
  } deriving (Show, Eq, Generic)

instance ToJSON BatchUpdateProjectRequest where
  toJSON     = genericToJSON jsonOptions
instance FromJSON BatchUpdateProjectRequest where
  parseJSON  = genericParseJSON jsonOptions

data BatchUpdateTaskItem = BatchUpdateTaskItem
  { id     :: UUID
  , update :: UpdateTask
  } deriving (Show, Eq, Generic)

instance ToJSON BatchUpdateTaskItem where
  toJSON item = case toJSON item.update of
    Object o -> Object (KM.insert "id" (toJSON item.id) o)
    v        -> v

instance FromJSON BatchUpdateTaskItem where
  parseJSON = withObject "BatchUpdateTaskItem" $ \o ->
    BatchUpdateTaskItem <$> o .: "id" <*> parseJSON (Object o)

newtype BatchUpdateTaskRequest = BatchUpdateTaskRequest
  { items :: [BatchUpdateTaskItem]
  } deriving (Show, Eq, Generic)

instance ToJSON BatchUpdateTaskRequest where
  toJSON     = genericToJSON jsonOptions
instance FromJSON BatchUpdateTaskRequest where
  parseJSON  = genericParseJSON jsonOptions

validateBatchUpdateProjectRequest :: BatchUpdateProjectRequest -> [Text]
validateBatchUpdateProjectRequest br =
  ["items must contain at least one item" | null br.items]
  <> ["items must contain at most 100 items" | length br.items > 100]
  <> concat
      [ prefixIssues ("items[" <> T.pack (show idx) <> "].")
                     (validateUpdateProjectInput item.update)
      | (idx, item) <- zip [(0 :: Int) ..] br.items
      ]

validateBatchUpdateTaskRequest :: BatchUpdateTaskRequest -> [Text]
validateBatchUpdateTaskRequest br =
  ["items must contain at least one item" | null br.items]
  <> ["items must contain at most 100 items" | length br.items > 100]
  <> concat
      [ prefixIssues ("items[" <> T.pack (show idx) <> "].")
                     (validateUpdateTaskInput item.update)
      | (idx, item) <- zip [(0 :: Int) ..] br.items
      ]

------------------------------------------------------------------------
-- Unified search
------------------------------------------------------------------------

data EntitySearchType = SearchObservation | SearchProject | SearchTask
  deriving (Show, Eq, Ord, Bounded, Enum, Generic)
instance ToJSON EntitySearchType where
  toJSON SearchObservation = String "observation"
  toJSON SearchProject = String "project"
  toJSON SearchTask = String "task"
instance FromJSON EntitySearchType where
  parseJSON = withText "EntitySearchType" $ \case
    "observation" -> pure SearchObservation
    "project" -> pure SearchProject
    "task" -> pure SearchTask
    other -> fail $ "Invalid entity search type: " <> T.unpack other

data UnifiedSearchQuery = UnifiedSearchQuery
  { workspaceId :: Maybe UUID, query :: Maybe Text, entityTypes :: Maybe [EntitySearchType]
  , searchLanguage :: Maybe Text, limit :: Maybe Int, offset :: Maybe Int
  , subjectKind :: Maybe SubjectKind, subject :: Maybe Text, gitSha :: Maybe Text
  , projectStatus :: Maybe ProjectStatus, taskStatus :: Maybe TaskStatus
  , taskPriority :: Maybe Int, projectId :: Maybe UUID
  } deriving (Show, Eq, Generic)
instance ToJSON UnifiedSearchQuery where toJSON = genericToJSON jsonOptions
instance FromJSON UnifiedSearchQuery where parseJSON = genericParseJSON jsonOptions

-- | Search results deliberately expose a compact derived preview instead of
-- the full, potentially large Observation content body.
data ObservationSearchHit = ObservationSearchHit
  { id :: UUID, workspaceId :: UUID, subjects :: [ObservationSubject]
  , gitSha :: Text, contentPreview :: Text, updatedAt :: UTCTime }
  deriving (Show, Eq, Generic)
instance ToJSON ObservationSearchHit where
  toJSON hit = object $
    [ "id" .= hit.id, "workspace_id" .= hit.workspaceId, "subjects" .= hit.subjects
    , "git_sha" .= hit.gitSha, "content_preview" .= hit.contentPreview, "updated_at" .= hit.updatedAt
    ] <> legacySubjectPairs hit.subjects
instance FromJSON ObservationSearchHit where
  parseJSON = withObject "ObservationSearchHit" $ \o -> ObservationSearchHit
    <$> o .: "id" <*> o .: "workspace_id" <*> parseSubjects o <*> o .: "git_sha"
    <*> o .: "content_preview" <*> o .: "updated_at"

data UnifiedSearchResults = UnifiedSearchResults
  { observations :: [ObservationSearchHit], projects :: [Project], tasks :: [Task] }
  deriving (Show, Eq, Generic)
instance ToJSON UnifiedSearchResults where toJSON = genericToJSON jsonOptions
instance FromJSON UnifiedSearchResults where parseJSON = genericParseJSON jsonOptions

validateUnifiedSearchQuery :: UnifiedSearchQuery -> [Text]
validateUnifiedSearchQuery usq =
  ["query must not be empty" | maybe False (T.null . T.strip) usq.query]
  <> ["workspace_id is required for unified search" | usq.workspaceId == Nothing]
  <> ["invalid search_language" | not (validFtsLanguage usq.searchLanguage)]
  <> validateObservationPagination usq.limit usq.offset
  <> validateOptionalIntRange "task_priority" 1 10 usq.taskPriority
