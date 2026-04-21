module HMem.Types
  ( -- * JSON helpers
    jsonOptions
  , camelToSnake

    -- * Memory types
    , MemoryType(..)
    , MemorySortBy(..)
    , Memory(..)
    , compactMemory
    , CreateMemory(..)
    , UpdateMemory(..)
    , SearchQuery(..)
    , MemoryListQuery(..)
    , LinkedMemoryListQuery(..)
    , validateSearchQuery
    , validateLinkedMemoryListQuery
    , MemoryLink(..)
    , CreateMemoryLink(..)

    -- * Workspace types
  , WorkspaceType(..)
  , Workspace(..)
  , CreateWorkspace(..)
  , UpdateWorkspace(..)

    -- * Workspace groups
  , WorkspaceGroup(..)
  , CreateWorkspaceGroup(..)

    -- * Project types
  , ProjectStatus(..)
  , Project(..)
  , CreateProject(..)
  , UpdateProject(..)
  , ProjectListQuery(..)
  , ProjectOverview(..)
  , TaskDependencySummary(..)
  , ContextMemoryScope(..)
  , ConnectedMemorySummary(..)
  , TaskOverview(..)
  , ContextDetailLevel(..)
  , contextDetailLimit
  , ContextInfo(..)

    -- * Task types
  , TaskStatus(..)
  , Task(..)
  , CreateTask(..)
  , UpdateTask(..)
  , TaskListQuery(..)

    -- * Unified search types
  , EntitySearchType(..)
  , UnifiedSearchQuery(..)
  , LinkedMemorySummary(..)
  , ProjectSearchResult(..)
  , TaskSearchResult(..)
  , UnifiedSearchResults(..)
  , validateUnifiedSearchQuery

    -- * Cleanup types
  , CleanupPolicy(..)
  , UpsertCleanupPolicy(..)
  , CleanupResult(..)

    -- * Category types
  , MemoryCategory(..)
  , CreateMemoryCategory(..)
  , UpdateMemoryCategory(..)

    -- * Relation types
  , RelationType(..)
  , relationTypeToText
  , relationTypeFromText

    -- * Cross-entity linking
  , LinkMemory(..)
  , LinkDependency(..)

    -- * Graph types
  , MemoryGraph(..)
  , AdjustImportance(..)

    -- * Embedding / similarity
  , SimilarQuery(..)
  , SimilarMemory(..)

    -- * Activity timeline
  , ActivityEvent(..)

    -- * Saved views
  , SavedView(..)
  , CreateSavedView(..)
  , UpdateSavedView(..)
  , SavedViewListQuery(..)

    -- * Audit log
  , AuditAction(..)
  , AuditLogEntry(..)
  , AuditLogQuery(..)
  , RevertResult(..)
  , auditActionToText
  , auditActionFromText

    -- * Pagination
  , PaginatedResult(..)

    -- * Batch operations
  , BatchDeleteRequest(..)
  , BatchMoveTasksRequest(..)
  , BatchMemoryLinkRequest(..)
  , BatchSetTagsItem(..)
  , BatchSetTagsRequest(..)
  , BatchResult(..)
  , BatchUpdateMemoryItem(..)
  , BatchUpdateMemoryRequest(..)
  , BatchUpdateProjectItem(..)
  , BatchUpdateProjectRequest(..)
  , BatchUpdateTaskItem(..)
  , BatchUpdateTaskRequest(..)
  , validateBatchDeleteRequest
  , validateBatchMoveTasksRequest
  , validateBatchMemoryLinkRequest
  , validateBatchSetTagsRequest
  , validateBatchUpdateMemoryRequest
  , validateBatchUpdateProjectRequest
  , validateBatchUpdateTaskRequest

    -- * Enum text conversion helpers
  , memoryTypeToText
  , memoryTypeFromText
  , projectStatusToText
  , projectStatusFromText
  , taskStatusToText
  , taskStatusFromText
  , workspaceTypeToText
  , workspaceTypeFromText

    -- * Field update (three-state nullable)
  , FieldUpdate(..)
  , parseFieldUpdate
  , fieldUpdatePair
  , applyNullableUpdate

    -- * Input validation
  , maxMemoryContentBytes
  , maxMemorySummaryBytes
  , maxNameBytes
  , maxDescriptionBytes
  , validFtsLanguage
  , maxPaginationOffset
  , maxPaginationLimit
  , capPagination
  , validateCreateWorkspaceInput
  , validateUpdateWorkspaceInput
  , validateCreateMemoryInput
  , validateUpdateMemoryInput
  , validateCreateMemoryBatchInput
  , validateMemoryListQuery
  , validateCreateProjectInput
  , validateUpdateProjectInput
  , validateProjectListQuery
  , validateCreateTaskInput
  , validateUpdateTaskInput
  , validateTaskListQuery
  , validateCreateMemoryCategoryInput
  , validateUpdateMemoryCategoryInput
  , validateCreateWorkspaceGroupInput
  , validateCreateSavedViewInput
  , validateUpdateSavedViewInput
  ) where

import Data.Aeson
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types (Parser, Pair)
import Data.ByteString qualified as BS
import Data.Char (isLower, isUpper, toLower)
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

maxMemoryContentBytes :: Int
maxMemoryContentBytes = 512 * 1024

maxMemorySummaryBytes :: Int
maxMemorySummaryBytes = 10 * 1024

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
  ( min maxPaginationLimit  (max 1 (fromMaybe 50 mlimit))
  , min maxPaginationOffset (max 0 (fromMaybe 0  moffset))
  )

validateCreateWorkspaceInput :: CreateWorkspace -> [Text]
validateCreateWorkspaceInput cw =
  validateRequiredText "name" maxNameBytes cw.name

validateUpdateWorkspaceInput :: UpdateWorkspace -> [Text]
validateUpdateWorkspaceInput uw =
  maybe [] (validateRequiredText "name" maxNameBytes) uw.name

validateCreateMemoryInput :: CreateMemory -> [Text]
validateCreateMemoryInput cm =
  validateRequiredText "content" maxMemoryContentBytes cm.content
  <> validateOptionalText "summary" maxMemorySummaryBytes cm.summary

validateUpdateMemoryInput :: UpdateMemory -> [Text]
validateUpdateMemoryInput um =
  maybe [] (validateRequiredText "content" maxMemoryContentBytes) um.content
  <> validateOptionalFieldText "summary" maxMemorySummaryBytes um.summary

validateCreateMemoryBatchInput :: [CreateMemory] -> [Text]
validateCreateMemoryBatchInput cms =
  ["memories must contain at least one item" | null cms]
  <> ["memories must contain at most 100 items" | length cms > 100]
  <> concat
      [ prefixIssues ("memories[" <> T.pack (show idx) <> "].") (validateCreateMemoryInput cm)
      | (idx, cm) <- zip [(0 :: Int) ..] cms
      ]

validateMemoryListQuery :: MemoryListQuery -> [Text]
validateMemoryListQuery mq =
  validateTimeRange "created_after" mq.createdAfter "created_before" mq.createdBefore
  <> validateTimeRange "updated_after" mq.updatedAfter "updated_before" mq.updatedBefore
  <> case mq.minAccessCount of
       Just n | n < 0 -> ["min_access_count must be >= 0"]
       _              -> []

validateSearchQuery :: SearchQuery -> [Text]
validateSearchQuery sq =
  ["min_access_count must be >= 0" | maybe False (< 0) sq.minAccessCount]

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

validateCreateMemoryCategoryInput :: CreateMemoryCategory -> [Text]
validateCreateMemoryCategoryInput cc =
  validateRequiredText "name" maxNameBytes cc.name

validateUpdateMemoryCategoryInput :: UpdateMemoryCategory -> [Text]
validateUpdateMemoryCategoryInput uc =
  maybe [] (validateRequiredText "name" maxNameBytes) uc.name

validateCreateWorkspaceGroupInput :: CreateWorkspaceGroup -> [Text]
validateCreateWorkspaceGroupInput cg =
  validateRequiredText "name" maxNameBytes cg.name

validSavedViewEntityTypes :: [Text]
validSavedViewEntityTypes = ["memory_search", "memory_list", "project_list", "task_list", "activity"]

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
-- MemoryType
------------------------------------------------------------------------

data MemoryType = ShortTerm | LongTerm
  deriving (Show, Eq, Ord, Bounded, Enum, Generic)

instance ToJSON MemoryType where
  toJSON ShortTerm = String "short_term"
  toJSON LongTerm  = String "long_term"

instance FromJSON MemoryType where
  parseJSON = withText "MemoryType" $ \case
    "short_term" -> pure ShortTerm
    "long_term"  -> pure LongTerm
    _            -> fail "Invalid memory type: expected short_term or long_term"

memoryTypeToText :: MemoryType -> Text
memoryTypeToText ShortTerm = "short_term"
memoryTypeToText LongTerm  = "long_term"

memoryTypeFromText :: Text -> Maybe MemoryType
memoryTypeFromText "short_term" = Just ShortTerm
memoryTypeFromText "long_term"  = Just LongTerm
memoryTypeFromText _            = Nothing

instance DBType MemoryType where
  typeInformation = case parseTypeInformation parse memoryTypeToText typeInformation of
    TypeInformation enc dec delim _ ->
      TypeInformation enc dec delim (TypeName (QualifiedName "memory_type_enum" Nothing) [] 0)
    where
      parse t = maybe (Left $ "Invalid memory_type: " <> T.unpack t) Right (memoryTypeFromText t)

instance DBEq MemoryType

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
-- RelationType
------------------------------------------------------------------------

data RelationType = Related | Supersedes | Contradicts | Elaborates
  | Inspires | DependsOn | DerivedFrom | AlternativeTo
  deriving (Show, Eq, Ord, Bounded, Enum, Generic)

instance ToJSON RelationType where
  toJSON Related       = String "related"
  toJSON Supersedes    = String "supersedes"
  toJSON Contradicts   = String "contradicts"
  toJSON Elaborates    = String "elaborates"
  toJSON Inspires      = String "inspires"
  toJSON DependsOn     = String "depends_on"
  toJSON DerivedFrom   = String "derived_from"
  toJSON AlternativeTo = String "alternative_to"

instance FromJSON RelationType where
  parseJSON = withText "RelationType" $ \case
    "related"        -> pure Related
    "supersedes"     -> pure Supersedes
    "contradicts"    -> pure Contradicts
    "elaborates"     -> pure Elaborates
    "inspires"       -> pure Inspires
    "depends_on"     -> pure DependsOn
    "derived_from"   -> pure DerivedFrom
    "alternative_to" -> pure AlternativeTo
    _                -> fail "Invalid relation type"

relationTypeToText :: RelationType -> Text
relationTypeToText Related       = "related"
relationTypeToText Supersedes    = "supersedes"
relationTypeToText Contradicts   = "contradicts"
relationTypeToText Elaborates    = "elaborates"
relationTypeToText Inspires      = "inspires"
relationTypeToText DependsOn     = "depends_on"
relationTypeToText DerivedFrom   = "derived_from"
relationTypeToText AlternativeTo = "alternative_to"

relationTypeFromText :: Text -> Maybe RelationType
relationTypeFromText "related"        = Just Related
relationTypeFromText "supersedes"     = Just Supersedes
relationTypeFromText "contradicts"    = Just Contradicts
relationTypeFromText "elaborates"     = Just Elaborates
relationTypeFromText "inspires"       = Just Inspires
relationTypeFromText "depends_on"     = Just DependsOn
relationTypeFromText "derived_from"   = Just DerivedFrom
relationTypeFromText "alternative_to" = Just AlternativeTo
relationTypeFromText _                = Nothing

instance DBType RelationType where
  typeInformation = case parseTypeInformation parse relationTypeToText typeInformation of
    TypeInformation enc dec delim _ ->
      TypeInformation enc dec delim (TypeName (QualifiedName "relation_type_enum" Nothing) [] 0)
    where
      parse t = maybe (Left $ "Invalid relation_type: " <> T.unpack t) Right (relationTypeFromText t)

instance DBEq RelationType

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

data UpdateWorkspace = UpdateWorkspace
  { name          :: Maybe Text
  , workspaceType :: Maybe WorkspaceType
  , ghOwner       :: FieldUpdate Text
  , ghRepo        :: FieldUpdate Text
  } deriving (Show, Eq, Generic)

instance ToJSON UpdateWorkspace where
  toJSON uw = object $ catMaybes
    [ ("name" .=)           <$> uw.name
    , ("workspace_type" .=) <$> uw.workspaceType
    , fieldUpdatePair "gh_owner" uw.ghOwner
    , fieldUpdatePair "gh_repo" uw.ghRepo
    ]
instance FromJSON UpdateWorkspace where
  parseJSON = withObject "UpdateWorkspace" $ \o -> UpdateWorkspace
    <$> o .:? "name"
    <*> o .:? "workspace_type"
    <*> parseFieldUpdate o "gh_owner"
    <*> parseFieldUpdate o "gh_repo"

------------------------------------------------------------------------
-- Memory
------------------------------------------------------------------------

data Memory = Memory
  { id             :: UUID
  , workspaceId    :: UUID
  , content        :: Text
  , summary        :: Maybe Text
  , memoryType     :: MemoryType
  , importance     :: Int
  , metadata       :: Value
  , expiresAt      :: Maybe UTCTime
  , source         :: Maybe Text
  , confidence     :: Double
  , pinned         :: Bool
  , lastAccessedAt :: UTCTime
  , accessCount    :: Int
  , ftsLanguage    :: Text
  , tags           :: [Text]
  , createdAt      :: UTCTime
  , updatedAt      :: UTCTime
  } deriving (Show, Eq, Generic)

instance ToJSON Memory where
  toJSON     = genericToJSON jsonOptions
instance FromJSON Memory where
  parseJSON  = genericParseJSON jsonOptions

-- | Truncate content and clear metadata for list/search responses.
compactMemory :: Memory -> Memory
compactMemory m = Memory
  { id             = m.id
  , workspaceId    = m.workspaceId
  , content        = let c = m.content in if T.length c > 200 then T.take 200 c <> "..." else c
  , summary        = m.summary
  , memoryType     = m.memoryType
  , importance     = m.importance
  , metadata       = toJSON (mempty :: Object)
  , expiresAt      = m.expiresAt
  , source         = m.source
  , confidence     = m.confidence
  , pinned         = m.pinned
  , lastAccessedAt = m.lastAccessedAt
  , accessCount    = m.accessCount
  , ftsLanguage    = m.ftsLanguage
  , tags           = m.tags
  , createdAt      = m.createdAt
  , updatedAt      = m.updatedAt
  }

data CreateMemory = CreateMemory
  { workspaceId  :: UUID
  , content      :: Text
  , summary      :: Maybe Text
  , memoryType   :: MemoryType
  , importance   :: Maybe Int
  , metadata     :: Maybe Value
  , expiresAt    :: Maybe UTCTime
  , source       :: Maybe Text
  , confidence   :: Maybe Double
  , pinned       :: Maybe Bool
  , tags         :: Maybe [Text]
  , ftsLanguage  :: Maybe Text
  } deriving (Show, Eq, Generic)

instance ToJSON CreateMemory where
  toJSON     = genericToJSON jsonOptions
instance FromJSON CreateMemory where
  parseJSON  = genericParseJSON jsonOptions

data UpdateMemory = UpdateMemory
  { content    :: Maybe Text
  , summary    :: FieldUpdate Text
  , memoryType :: Maybe MemoryType
  , importance :: Maybe Int
  , metadata   :: Maybe Value
  , expiresAt  :: FieldUpdate UTCTime
  , source     :: FieldUpdate Text
  , confidence :: Maybe Double
  , pinned     :: Maybe Bool
  } deriving (Show, Eq, Generic)

instance ToJSON UpdateMemory where
  toJSON um = object $ catMaybes
    [ ("content" .=)     <$> um.content
    , fieldUpdatePair "summary" um.summary
    , ("memory_type" .=) <$> um.memoryType
    , ("importance" .=)  <$> um.importance
    , ("metadata" .=)    <$> um.metadata
    , fieldUpdatePair "expires_at" um.expiresAt
    , fieldUpdatePair "source" um.source
    , ("confidence" .=)  <$> um.confidence
    , ("pinned" .=)      <$> um.pinned
    ]
instance FromJSON UpdateMemory where
  parseJSON = withObject "UpdateMemory" $ \o -> UpdateMemory
    <$> o .:? "content"
    <*> parseFieldUpdate o "summary"
    <*> o .:? "memory_type"
    <*> o .:? "importance"
    <*> o .:? "metadata"
    <*> parseFieldUpdate o "expires_at"
    <*> parseFieldUpdate o "source"
    <*> o .:? "confidence"
    <*> o .:? "pinned"

data MemorySortBy = SortRecent | SortImportance | SortAccessCount
  deriving (Show, Eq, Generic)

instance ToJSON MemorySortBy where
  toJSON SortRecent      = String "recent"
  toJSON SortImportance  = String "importance"
  toJSON SortAccessCount = String "access_count"

instance FromJSON MemorySortBy where
  parseJSON = withText "MemorySortBy" $ \case
    "recent"       -> pure SortRecent
    "importance"   -> pure SortImportance
    "access_count" -> pure SortAccessCount
    _               -> fail "Invalid memory sort (expected recent, importance, or access_count)"

data SearchQuery = SearchQuery
  { workspaceId    :: Maybe UUID
  , query          :: Maybe Text
  , memoryType     :: Maybe MemoryType
  , tags           :: Maybe [Text]
  , minImportance  :: Maybe Int
  , minAccessCount :: Maybe Int
  , sortBy         :: Maybe MemorySortBy
  , categoryId     :: Maybe UUID
  , pinnedOnly     :: Maybe Bool
  , searchLanguage :: Maybe Text    -- ^ regconfig for plainto_tsquery (default 'english')
  , limit          :: Maybe Int
  , offset         :: Maybe Int
  } deriving (Show, Eq, Generic)

instance ToJSON SearchQuery where
  toJSON     = genericToJSON jsonOptions
instance FromJSON SearchQuery where
  parseJSON  = genericParseJSON jsonOptions

data MemoryListQuery = MemoryListQuery
  { workspaceId   :: Maybe UUID
  , memoryType    :: Maybe MemoryType
  , minAccessCount :: Maybe Int
  , sortBy        :: Maybe MemorySortBy
  , createdAfter  :: Maybe UTCTime
  , createdBefore :: Maybe UTCTime
  , updatedAfter  :: Maybe UTCTime
  , updatedBefore :: Maybe UTCTime
  , limit         :: Maybe Int
  , offset        :: Maybe Int
  } deriving (Show, Eq, Generic)

instance ToJSON MemoryListQuery where
  toJSON     = genericToJSON jsonOptions
instance FromJSON MemoryListQuery where
  parseJSON  = genericParseJSON jsonOptions

data LinkedMemoryListQuery = LinkedMemoryListQuery
  { query          :: Maybe Text
  , tags           :: Maybe [Text]
  , minImportance  :: Maybe Int
  , memoryType     :: Maybe MemoryType
  , minAccessCount :: Maybe Int
  } deriving (Show, Eq, Generic)

instance ToJSON LinkedMemoryListQuery where
  toJSON     = genericToJSON jsonOptions
instance FromJSON LinkedMemoryListQuery where
  parseJSON  = genericParseJSON jsonOptions

validateLinkedMemoryListQuery :: LinkedMemoryListQuery -> [Text]
validateLinkedMemoryListQuery lq = concat
  [ case lq.minImportance of
      Just n | n < 1 || n > 10 -> ["minImportance must be between 1 and 10"]
      _                        -> []
  , case lq.minAccessCount of
      Just n | n < 0 -> ["minAccessCount must be >= 0"]
      _              -> []
  ]

data ProjectListQuery = ProjectListQuery
  { workspaceId     :: Maybe UUID
  , status          :: Maybe ProjectStatus
  , query           :: Maybe Text
  , searchLanguage  :: Maybe Text
  , createdAfter    :: Maybe UTCTime
  , createdBefore   :: Maybe UTCTime
  , updatedAfter    :: Maybe UTCTime
  , updatedBefore   :: Maybe UTCTime
  , limit           :: Maybe Int
  , offset          :: Maybe Int
  } deriving (Show, Eq, Generic)

instance ToJSON ProjectListQuery where
  toJSON     = genericToJSON jsonOptions
instance FromJSON ProjectListQuery where
  parseJSON  = genericParseJSON jsonOptions

-- | Aggregated view of a project for planning workflows.
data ProjectOverview = ProjectOverview
  { project        :: Project
  , tasks          :: [Task]
  , subprojects    :: [Project]
  , linkedMemories :: [Memory]
  , connectedMemories :: [ConnectedMemorySummary]
  } deriving (Show, Eq, Generic)

instance ToJSON ProjectOverview where
  toJSON     = genericToJSON jsonOptions
instance FromJSON ProjectOverview where
  parseJSON  = genericParseJSON jsonOptions

data TaskListQuery = TaskListQuery
  { workspaceId     :: Maybe UUID
  , projectId       :: Maybe UUID
  , status          :: Maybe TaskStatus
  , priority        :: Maybe Int
  , query           :: Maybe Text
  , searchLanguage  :: Maybe Text
  , createdAfter    :: Maybe UTCTime
  , createdBefore   :: Maybe UTCTime
  , updatedAfter    :: Maybe UTCTime
  , updatedBefore   :: Maybe UTCTime
  , limit           :: Maybe Int
  , offset          :: Maybe Int
  } deriving (Show, Eq, Generic)

instance ToJSON TaskListQuery where
  toJSON     = genericToJSON jsonOptions
instance FromJSON TaskListQuery where
  parseJSON  = genericParseJSON jsonOptions

------------------------------------------------------------------------
-- Task overview
------------------------------------------------------------------------

data TaskDependencySummary = TaskDependencySummary
  { id   :: UUID
  , name :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON TaskDependencySummary where
  toJSON     = genericToJSON jsonOptions
instance FromJSON TaskDependencySummary where
  parseJSON  = genericParseJSON jsonOptions

data ContextMemoryScope = ScopeTask | ScopeProject | ScopeWorkspace
  deriving (Show, Eq, Ord, Bounded, Enum, Generic)

instance ToJSON ContextMemoryScope where
  toJSON ScopeTask      = String "task"
  toJSON ScopeProject   = String "project"
  toJSON ScopeWorkspace = String "workspace"

instance FromJSON ContextMemoryScope where
  parseJSON = withText "ContextMemoryScope" $ \case
    "task"      -> pure ScopeTask
    "project"   -> pure ScopeProject
    "workspace" -> pure ScopeWorkspace
    _            -> fail "Invalid context memory scope"

data ConnectedMemorySummary = ConnectedMemorySummary
  { id      :: UUID
  , summary :: Text
  , scope   :: ContextMemoryScope
  } deriving (Show, Eq, Generic)

instance ToJSON ConnectedMemorySummary where
  toJSON     = genericToJSON jsonOptions
instance FromJSON ConnectedMemorySummary where
  parseJSON  = genericParseJSON jsonOptions

data TaskOverview = TaskOverview
  { task              :: Task
  , dependencies      :: [TaskDependencySummary]
  , connectedMemories :: [ConnectedMemorySummary]
  } deriving (Show, Eq, Generic)

instance ToJSON TaskOverview where
  toJSON     = genericToJSON jsonOptions
instance FromJSON TaskOverview where
  parseJSON  = genericParseJSON jsonOptions

------------------------------------------------------------------------
-- Context info (light / medium / heavy memory retrieval)
------------------------------------------------------------------------

-- | Detail level controlling how many memories per scope are returned.
data ContextDetailLevel = ContextLight | ContextMedium | ContextHeavy
  deriving (Show, Eq, Ord, Bounded, Enum, Generic)

instance ToJSON ContextDetailLevel where
  toJSON ContextLight  = String "light"
  toJSON ContextMedium = String "medium"
  toJSON ContextHeavy  = String "heavy"

instance FromJSON ContextDetailLevel where
  parseJSON = withText "ContextDetailLevel" $ \case
    "light"  -> pure ContextLight
    "medium" -> pure ContextMedium
    "heavy"  -> pure ContextHeavy
    _        -> fail "Invalid context detail level (expected light, medium, or heavy)"

-- | How many memories per scope for each detail level.
contextDetailLimit :: ContextDetailLevel -> Int
contextDetailLimit ContextLight  = 2
contextDetailLimit ContextMedium = 5
contextDetailLimit ContextHeavy  = 10

-- | Aggregated context information for a task, with memories grouped
-- by scope and limited according to the detail level.
data ContextInfo = ContextInfo
  { task              :: Task
  , detailLevel       :: ContextDetailLevel
  , taskMemories      :: [ConnectedMemorySummary]
  , projectMemories   :: [ConnectedMemorySummary]  -- from all ancestor projects
  , workspaceMemories :: [ConnectedMemorySummary]
  } deriving (Show, Eq, Generic)

instance ToJSON ContextInfo where
  toJSON     = genericToJSON jsonOptions
instance FromJSON ContextInfo where
  parseJSON  = genericParseJSON jsonOptions

------------------------------------------------------------------------
-- Memory links
------------------------------------------------------------------------

data MemoryLink = MemoryLink
  { sourceId     :: UUID
  , targetId     :: UUID
  , relationType :: RelationType
  , strength     :: Double
  , createdAt    :: UTCTime
  } deriving (Show, Eq, Generic)

instance ToJSON MemoryLink where
  toJSON     = genericToJSON jsonOptions
instance FromJSON MemoryLink where
  parseJSON  = genericParseJSON jsonOptions

data CreateMemoryLink = CreateMemoryLink
  { targetId     :: UUID
  , relationType :: RelationType
  , strength     :: Maybe Double
  } deriving (Show, Eq, Generic)

instance ToJSON CreateMemoryLink where
  toJSON     = genericToJSON jsonOptions
instance FromJSON CreateMemoryLink where
  parseJSON  = genericParseJSON jsonOptions

------------------------------------------------------------------------
-- Memory graph
------------------------------------------------------------------------

data MemoryGraph = MemoryGraph
  { memories :: [Memory]
  , links    :: [MemoryLink]
  } deriving (Show, Eq, Generic)

instance ToJSON MemoryGraph where
  toJSON     = genericToJSON jsonOptions
instance FromJSON MemoryGraph where
  parseJSON  = genericParseJSON jsonOptions

------------------------------------------------------------------------
-- Importance adjustment
------------------------------------------------------------------------

newtype AdjustImportance = AdjustImportance { importance :: Int }
  deriving (Show, Eq, Generic)

instance ToJSON AdjustImportance where
  toJSON     = genericToJSON jsonOptions
instance FromJSON AdjustImportance where
  parseJSON  = genericParseJSON jsonOptions

------------------------------------------------------------------------
-- Embedding / vector similarity
------------------------------------------------------------------------

-- | Query for finding memories similar to a given embedding vector.
data SimilarQuery = SimilarQuery
  { workspaceId    :: UUID
  , embedding      :: [Double]   -- ^ The query vector (must match column dimension, e.g. 1536)
  , limit          :: Maybe Int
  , minSimilarity  :: Maybe Double  -- ^ Cosine similarity threshold (0.0–1.0, default 0.0)
  } deriving (Show, Eq, Generic)

instance ToJSON SimilarQuery where
  toJSON     = genericToJSON jsonOptions
instance FromJSON SimilarQuery where
  parseJSON  = genericParseJSON jsonOptions

-- | A memory together with its cosine similarity score.
data SimilarMemory = SimilarMemory
  { memory     :: Memory
  , similarity :: Double
  } deriving (Show, Eq, Generic)

instance ToJSON SimilarMemory where
  toJSON     = genericToJSON jsonOptions
instance FromJSON SimilarMemory where
  parseJSON  = genericParseJSON jsonOptions

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
  , memoryLinkCount :: Int
  , createdAt       :: UTCTime
  , updatedAt       :: UTCTime
  } deriving (Show, Eq, Generic)

instance ToJSON Task where
  toJSON     = genericToJSON jsonOptions
instance FromJSON Task where
  parseJSON  = genericParseJSON jsonOptions

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
-- Cleanup
------------------------------------------------------------------------

data CleanupPolicy = CleanupPolicy
  { id            :: UUID
  , workspaceId   :: UUID
  , memoryType    :: MemoryType
  , maxAgeHours   :: Maybe Int
  , maxCount      :: Maybe Int
  , minImportance :: Int
  , enabled       :: Bool
  , createdAt     :: UTCTime
  , updatedAt     :: UTCTime
  } deriving (Show, Eq, Generic)

instance ToJSON CleanupPolicy where
  toJSON     = genericToJSON jsonOptions
instance FromJSON CleanupPolicy where
  parseJSON  = genericParseJSON jsonOptions

-- | Input type for creating/upserting a cleanup policy.
-- The server auto-generates @id@, @created_at@, and @updated_at@.
data UpsertCleanupPolicy = UpsertCleanupPolicy
  { workspaceId   :: UUID
  , memoryType    :: MemoryType
  , maxAgeHours   :: Maybe Int
  , maxCount      :: Maybe Int
  , minImportance :: Int
  , enabled       :: Bool
  } deriving (Show, Eq, Generic)

instance ToJSON UpsertCleanupPolicy where
  toJSON     = genericToJSON jsonOptions
instance FromJSON UpsertCleanupPolicy where
  parseJSON  = genericParseJSON jsonOptions

data CleanupResult = CleanupResult
  { deletedCount :: Int
  , workspaceId  :: UUID
  } deriving (Show, Eq, Generic)

instance ToJSON CleanupResult where
  toJSON     = genericToJSON jsonOptions
instance FromJSON CleanupResult where
  parseJSON  = genericParseJSON jsonOptions

------------------------------------------------------------------------
-- Cross-entity linking request types
------------------------------------------------------------------------

newtype LinkMemory = LinkMemory { memoryId :: UUID }
  deriving (Show, Eq, Generic)

instance ToJSON LinkMemory where
  toJSON     = genericToJSON jsonOptions
instance FromJSON LinkMemory where
  parseJSON  = genericParseJSON jsonOptions

newtype LinkDependency = LinkDependency { dependsOnId :: UUID }
  deriving (Show, Eq, Generic)

instance ToJSON LinkDependency where
  toJSON     = genericToJSON jsonOptions
instance FromJSON LinkDependency where
  parseJSON  = genericParseJSON jsonOptions

------------------------------------------------------------------------
-- Memory categories
------------------------------------------------------------------------

data MemoryCategory = MemoryCategory
  { id          :: UUID
  , workspaceId :: Maybe UUID
  , name        :: Text
  , description :: Maybe Text
  , parentId    :: Maybe UUID
  , createdAt   :: UTCTime
  } deriving (Show, Eq, Generic)

instance ToJSON MemoryCategory where
  toJSON     = genericToJSON jsonOptions
instance FromJSON MemoryCategory where
  parseJSON  = genericParseJSON jsonOptions

data CreateMemoryCategory = CreateMemoryCategory
  { workspaceId :: Maybe UUID
  , name        :: Text
  , description :: Maybe Text
  , parentId    :: Maybe UUID
  } deriving (Show, Eq, Generic)

instance ToJSON CreateMemoryCategory where
  toJSON     = genericToJSON jsonOptions
instance FromJSON CreateMemoryCategory where
  parseJSON  = genericParseJSON jsonOptions

data UpdateMemoryCategory = UpdateMemoryCategory
  { name        :: Maybe Text
  , description :: FieldUpdate Text
  , parentId    :: FieldUpdate UUID
  } deriving (Show, Eq, Generic)

instance ToJSON UpdateMemoryCategory where
  toJSON uc = object $ catMaybes
    [ ("name" .=) <$> uc.name
    , fieldUpdatePair "description" uc.description
    , fieldUpdatePair "parent_id" uc.parentId
    ]
instance FromJSON UpdateMemoryCategory where
  parseJSON = withObject "UpdateMemoryCategory" $ \o -> UpdateMemoryCategory
    <$> o .:? "name"
    <*> parseFieldUpdate o "description"
    <*> parseFieldUpdate o "parent_id"

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

------------------------------------------------------------------------
-- Activity timeline
------------------------------------------------------------------------

data ActivityEvent = ActivityEvent
  { eventType   :: Text      -- "created", "updated", "deleted"
  , entityType  :: Text      -- "memory", "project", "task"
  , entityId    :: UUID
  , workspaceId :: UUID
  , summary     :: Text
  , timestamp   :: UTCTime
  } deriving (Show, Eq, Generic)

instance ToJSON ActivityEvent where
  toJSON     = genericToJSON jsonOptions
instance FromJSON ActivityEvent where
  parseJSON  = genericParseJSON jsonOptions

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
  , entityType :: Text
  , entityId   :: Text
  , action     :: AuditAction
  , oldValues  :: Maybe Value
  , newValues  :: Maybe Value
  , requestId  :: Maybe Text
  , changedAt  :: UTCTime
  } deriving (Show, Eq, Generic)

instance ToJSON AuditLogEntry where
  toJSON     = genericToJSON jsonOptions
instance FromJSON AuditLogEntry where
  parseJSON  = genericParseJSON jsonOptions

data AuditLogQuery = AuditLogQuery
  { entityType :: Maybe Text
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

newtype BatchMemoryLinkRequest = BatchMemoryLinkRequest
  { memoryIds :: [UUID]
  } deriving (Show, Eq, Generic)

instance ToJSON BatchMemoryLinkRequest where
  toJSON     = genericToJSON jsonOptions
instance FromJSON BatchMemoryLinkRequest where
  parseJSON  = genericParseJSON jsonOptions

data BatchSetTagsItem = BatchSetTagsItem
  { memoryId :: UUID
  , tags     :: [Text]
  } deriving (Show, Eq, Generic)

instance ToJSON BatchSetTagsItem where
  toJSON     = genericToJSON jsonOptions
instance FromJSON BatchSetTagsItem where
  parseJSON  = genericParseJSON jsonOptions

newtype BatchSetTagsRequest = BatchSetTagsRequest
  { items :: [BatchSetTagsItem]
  } deriving (Show, Eq, Generic)

instance ToJSON BatchSetTagsRequest where
  toJSON     = genericToJSON jsonOptions
instance FromJSON BatchSetTagsRequest where
  parseJSON  = genericParseJSON jsonOptions

newtype BatchResult = BatchResult
  { affected :: Int
  } deriving (Show, Eq, Generic)

instance ToJSON BatchResult where
  toJSON     = genericToJSON jsonOptions
instance FromJSON BatchResult where
  parseJSON  = genericParseJSON jsonOptions

validateBatchDeleteRequest :: BatchDeleteRequest -> [Text]
validateBatchDeleteRequest br =
  ["ids must contain at least one item" | null br.ids]
  <> ["ids must contain at most 100 items" | length br.ids > 100]

validateBatchMoveTasksRequest :: BatchMoveTasksRequest -> [Text]
validateBatchMoveTasksRequest bm =
  ["task_ids must contain at least one item" | null bm.taskIds]
  <> ["task_ids must contain at most 100 items" | length bm.taskIds > 100]

validateBatchMemoryLinkRequest :: BatchMemoryLinkRequest -> [Text]
validateBatchMemoryLinkRequest bl =
  ["memory_ids must contain at least one item" | null bl.memoryIds]
  <> ["memory_ids must contain at most 100 items" | length bl.memoryIds > 100]

validateBatchSetTagsRequest :: BatchSetTagsRequest -> [Text]
validateBatchSetTagsRequest bs =
  ["items must contain at least one item" | null bs.items]
  <> ["items must contain at most 100 items" | length bs.items > 100]

data BatchUpdateMemoryItem = BatchUpdateMemoryItem
  { id     :: UUID
  , update :: UpdateMemory
  } deriving (Show, Eq, Generic)

instance ToJSON BatchUpdateMemoryItem where
  toJSON item = case toJSON item.update of
    Object o -> Object (KM.insert "id" (toJSON item.id) o)
    v        -> v

instance FromJSON BatchUpdateMemoryItem where
  parseJSON = withObject "BatchUpdateMemoryItem" $ \o ->
    BatchUpdateMemoryItem <$> o .: "id" <*> parseJSON (Object o)

newtype BatchUpdateMemoryRequest = BatchUpdateMemoryRequest
  { items :: [BatchUpdateMemoryItem]
  } deriving (Show, Eq, Generic)

instance ToJSON BatchUpdateMemoryRequest where
  toJSON     = genericToJSON jsonOptions
instance FromJSON BatchUpdateMemoryRequest where
  parseJSON  = genericParseJSON jsonOptions

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

validateBatchUpdateMemoryRequest :: BatchUpdateMemoryRequest -> [Text]
validateBatchUpdateMemoryRequest br =
  ["items must contain at least one item" | null br.items]
  <> ["items must contain at most 100 items" | length br.items > 100]
  <> concat
      [ prefixIssues ("items[" <> T.pack (show idx) <> "].")
                     (validateUpdateMemoryInput item.update)
      | (idx, item) <- zip [(0 :: Int) ..] br.items
      ]

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

data EntitySearchType = SearchMemory | SearchProject | SearchTask
  deriving (Show, Eq, Ord, Bounded, Enum, Generic)

instance ToJSON EntitySearchType where
  toJSON SearchMemory  = String "memory"
  toJSON SearchProject = String "project"
  toJSON SearchTask    = String "task"

instance FromJSON EntitySearchType where
  parseJSON = withText "EntitySearchType" $ \case
    "memory"  -> pure SearchMemory
    "project" -> pure SearchProject
    "task"    -> pure SearchTask
    other     -> fail $ "Invalid entity search type: " <> T.unpack other

data UnifiedSearchQuery = UnifiedSearchQuery
  { workspaceId    :: Maybe UUID
  , query          :: Text
  , entityTypes    :: Maybe [EntitySearchType]
  , searchLanguage :: Maybe Text
  , limit          :: Maybe Int
  , offset         :: Maybe Int
  -- Memory-specific filters
  , memoryType     :: Maybe MemoryType
  , tags           :: Maybe [Text]
  , minImportance  :: Maybe Int
  , categoryId     :: Maybe UUID
  , pinnedOnly     :: Maybe Bool
  -- Project-specific filters
  , projectStatus  :: Maybe ProjectStatus
  -- Task-specific filters
  , taskStatus     :: Maybe TaskStatus
  , taskPriority   :: Maybe Int
  , projectId      :: Maybe UUID
  } deriving (Show, Eq, Generic)

instance ToJSON UnifiedSearchQuery where
  toJSON     = genericToJSON jsonOptions
instance FromJSON UnifiedSearchQuery where
  parseJSON  = genericParseJSON jsonOptions

-- | Compact summary of a linked memory included in project/task search results.
data LinkedMemorySummary = LinkedMemorySummary
  { id         :: UUID
  , summary    :: Maybe Text
  , tags       :: [Text]
  , importance :: Int
  } deriving (Show, Eq, Generic)

instance ToJSON LinkedMemorySummary where
  toJSON     = genericToJSON jsonOptions
instance FromJSON LinkedMemorySummary where
  parseJSON  = genericParseJSON jsonOptions

-- | A project search result with its linked memories.
data ProjectSearchResult = ProjectSearchResult
  { project        :: Project
  , linkedMemories :: [LinkedMemorySummary]
  } deriving (Show, Eq, Generic)

instance ToJSON ProjectSearchResult where
  toJSON     = genericToJSON jsonOptions
instance FromJSON ProjectSearchResult where
  parseJSON  = genericParseJSON jsonOptions

-- | A task search result with its linked memories.
data TaskSearchResult = TaskSearchResult
  { task           :: Task
  , linkedMemories :: [LinkedMemorySummary]
  } deriving (Show, Eq, Generic)

instance ToJSON TaskSearchResult where
  toJSON     = genericToJSON jsonOptions
instance FromJSON TaskSearchResult where
  parseJSON  = genericParseJSON jsonOptions

-- | Results from a unified search across entity types.
data UnifiedSearchResults = UnifiedSearchResults
  { memories :: [Memory]
  , projects :: [ProjectSearchResult]
  , tasks    :: [TaskSearchResult]
  } deriving (Show, Eq, Generic)

instance ToJSON UnifiedSearchResults where
  toJSON     = genericToJSON jsonOptions
instance FromJSON UnifiedSearchResults where
  parseJSON  = genericParseJSON jsonOptions

validateUnifiedSearchQuery :: UnifiedSearchQuery -> [Text]
validateUnifiedSearchQuery usq =
  ["query is required and must not be empty" | T.null (T.strip usq.query)]
  <> validateOptionalIntRange "min_importance" 1 10 usq.minImportance
  <> validateOptionalIntRange "task_priority" 1 10 usq.taskPriority
