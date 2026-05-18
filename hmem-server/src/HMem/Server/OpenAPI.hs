{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module HMem.Server.OpenAPI
  ( openApiSpec
  ) where

import Control.Lens ((&), (.~), (?~), at)
import Data.Aeson (Value)
import Data.OpenApi
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import GHC.Generics (Generic)
import Servant.OpenApi (toOpenApi)

import HMem.DB.Auth qualified as Auth
import HMem.Server.API (HMemAPI, CreateMemoryRequest, CleanupRunReq, GroupMemberReq, CategoryLink)
import HMem.Types

------------------------------------------------------------------------
-- OpenAPI spec generation
------------------------------------------------------------------------

openApiSpec :: OpenApi
openApiSpec = toOpenApi (Proxy @HMemAPI)
  & info . title   .~ "hmem API"
  & info . version .~ "0.1.0.0"
  & info . description ?~ "LLM memory and task management server. Mutating project/task endpoints may return HTTP 409 with a LifecycleConflictError body when recursive lifecycle invariants reject the requested state change. Memory creation and memory-link mutations may return HTTP 409 with a WorkflowConflictError body when at-least-one creation-link invariants are violated."
  & components . schemas . at "LifecycleConflictError" ?~ toSchema (Proxy @LifecycleConflictError)
  & components . schemas . at "WorkflowConflictError" ?~ toSchema (Proxy @WorkflowConflictError)

data LifecycleConflictError = LifecycleConflictError
  { error          :: Text
  , code           :: Text
  , message        :: Text
  , detail         :: Maybe Value
  , hint           :: Maybe Text
  , requiredAction :: Maybe Text
  } deriving stock Generic

instance ToSchema LifecycleConflictError where
  declareNamedSchema = genericDeclareNamedSchema opts

data WorkflowConflictError = WorkflowConflictError
  { workflowError          :: Text
  , workflowCode           :: Text
  , workflowMessage        :: Text
  , workflowDetail         :: Maybe Value
  , workflowHint           :: Maybe Text
  , workflowRequiredAction :: Maybe Text
  } deriving stock Generic

instance ToSchema WorkflowConflictError where
  declareNamedSchema = genericDeclareNamedSchema workflowConflictOpts

------------------------------------------------------------------------
-- ToSchema instances (orphans, scoped to hmem-server)
------------------------------------------------------------------------

opts :: SchemaOptions
opts = defaultSchemaOptions { fieldLabelModifier = camelToSnakeField }

workflowConflictOpts :: SchemaOptions
workflowConflictOpts = opts
  { fieldLabelModifier = \case
      "workflowError"   -> "error"
      "workflowCode"    -> "code"
      "workflowMessage" -> "message"
      "workflowDetail"  -> "detail"
      "workflowHint"    -> "hint"
      "workflowRequiredAction" -> "required_action"
      other             -> camelToSnakeField other
  }

-- Primitives / pre-existing
instance ToSchema Value where
  declareNamedSchema _ = pure $ NamedSchema (Just "JSONValue") mempty

-- Enum types (ToSchema + ToParamSchema for query/path use)
instance ToSchema MemoryType where
  declareNamedSchema _ = pure $ NamedSchema (Just "MemoryType") $ mempty
    & type_ ?~ OpenApiString
    & enum_ ?~ ["short_term", "long_term"]
instance ToParamSchema MemoryType where
  toParamSchema _ = mempty & type_ ?~ OpenApiString & enum_ ?~ ["short_term", "long_term"]

instance ToSchema MemorySortBy where
  declareNamedSchema _ = pure $ NamedSchema (Just "MemorySortBy") $ mempty
    & type_ ?~ OpenApiString
    & enum_ ?~ ["recent", "importance", "access_count"]
instance ToParamSchema MemorySortBy where
  toParamSchema _ = mempty & type_ ?~ OpenApiString & enum_ ?~ ["recent", "importance", "access_count"]

instance ToSchema ProjectStatus where
  declareNamedSchema _ = pure $ NamedSchema (Just "ProjectStatus") $ mempty
    & type_ ?~ OpenApiString
    & enum_ ?~ ["active", "paused", "completed", "archived"]
instance ToParamSchema ProjectStatus where
  toParamSchema _ = mempty & type_ ?~ OpenApiString & enum_ ?~ ["active", "paused", "completed", "archived"]

instance ToSchema TaskStatus where
  declareNamedSchema _ = pure $ NamedSchema (Just "TaskStatus") $ mempty
    & type_ ?~ OpenApiString
    & enum_ ?~ ["todo", "in_progress", "blocked", "done", "cancelled"]
instance ToParamSchema TaskStatus where
  toParamSchema _ = mempty & type_ ?~ OpenApiString & enum_ ?~ ["todo", "in_progress", "blocked", "done", "cancelled"]

instance ToSchema RelationType where
  declareNamedSchema _ = pure $ NamedSchema (Just "RelationType") $ mempty
    & type_ ?~ OpenApiString
    & enum_ ?~ ["related", "supersedes", "contradicts", "elaborates",
                 "inspires", "depends_on", "derived_from", "alternative_to"]
instance ToParamSchema RelationType where
  toParamSchema _ = mempty & type_ ?~ OpenApiString
    & enum_ ?~ ["related", "supersedes", "contradicts", "elaborates",
                 "inspires", "depends_on", "derived_from", "alternative_to"]

instance ToSchema WorkspaceType where
  declareNamedSchema _ = pure $ NamedSchema (Just "WorkspaceType") $ mempty
    & type_ ?~ OpenApiString
    & enum_ ?~ ["repository", "planning", "personal", "organization"]
instance ToParamSchema WorkspaceType where
  toParamSchema _ = mempty & type_ ?~ OpenApiString & enum_ ?~ ["repository", "planning", "personal", "organization"]

instance ToSchema Auth.WorkspaceRole where
  declareNamedSchema _ = pure $ NamedSchema (Just "WorkspaceRole") $ mempty
    & type_ ?~ OpenApiString
    & enum_ ?~ ["read", "edit", "admin"]

membershipOpts :: SchemaOptions
membershipOpts = opts
  { fieldLabelModifier = \case
      "membershipWorkspaceId" -> "workspace_id"
      "membershipUserId"      -> "user_id"
      "membershipRole"        -> "role"
      "membershipGrantedBy"   -> "granted_by"
      "membershipCreatedAt"   -> "created_at"
      "membershipUpdatedAt"   -> "updated_at"
      "membershipUserIdInput" -> "user_id"
      "membershipRoleInput"   -> "role"
      other                   -> camelToSnakeField other
  }

instance ToSchema ContextMemoryScope where
  declareNamedSchema _ = pure $ NamedSchema (Just "ContextMemoryScope") $ mempty
    & type_ ?~ OpenApiString
    & enum_ ?~ ["task", "project", "workspace"]

instance ToSchema ContextDetailLevel where
  declareNamedSchema _ = pure $ NamedSchema (Just "ContextDetailLevel") $ mempty
    & type_ ?~ OpenApiString
    & enum_ ?~ ["light", "medium", "heavy"]
instance ToParamSchema ContextDetailLevel where
  toParamSchema _ = mempty & type_ ?~ OpenApiString & enum_ ?~ ["light", "medium", "heavy"]

-- FieldUpdate: in JSON it's just the inner type or null
instance ToSchema a => ToSchema (FieldUpdate a) where
  declareNamedSchema _ = declareNamedSchema (Proxy @a)

-- Domain types using Generic derivation
instance ToSchema Memory              where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema CreateMemory         where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema CreateMemoryRequest  where declareNamedSchema _ = declareNamedSchema (Proxy @CreateMemory)
instance ToSchema UpdateMemory         where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema SearchQuery          where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema MemoryLink           where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema CreateMemoryLink     where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema MemoryGraph          where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema AdjustImportance     where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema SimilarQuery         where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema SimilarMemory        where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema Workspace            where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema CreateWorkspace      where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema UpdateWorkspace      where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema Auth.WorkspaceMembership where declareNamedSchema = genericDeclareNamedSchema membershipOpts
instance ToSchema Auth.UpsertWorkspaceMembership where declareNamedSchema = genericDeclareNamedSchema membershipOpts
instance ToSchema WorkspaceGroup       where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema CreateWorkspaceGroup where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema Project              where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema CreateProject        where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema UpdateProject        where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema Task                 where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema NextTaskCandidate    where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema CreateTask           where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema UpdateTask           where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema CleanupPolicy        where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema UpsertCleanupPolicy  where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema CleanupResult        where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema MemoryCategory       where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema CreateMemoryCategory where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema UpdateMemoryCategory where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema CategoryLink         where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema CleanupRunReq        where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema GroupMemberReq       where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema ActivityEvent        where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema LinkMemory           where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema LinkDependency       where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema a => ToSchema (PaginatedResult a) where
  declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema BatchDeleteRequest     where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema BatchMoveTasksRequest  where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema BatchMemoryLinkRequest where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema BatchSetTagsItem       where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema BatchSetTagsRequest    where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema BatchResult            where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema BatchUpdateMemoryItem  where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema BatchUpdateMemoryRequest where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema BatchUpdateProjectItem where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema BatchUpdateProjectRequest where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema BatchUpdateTaskItem    where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema BatchUpdateTaskRequest where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema SavedView              where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema CreateSavedView        where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema UpdateSavedView        where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema ProjectOverview        where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema TaskDependencySummary  where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema ConnectedMemorySummary where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema TaskOverview           where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema ContextInfo            where declareNamedSchema = genericDeclareNamedSchema opts

instance ToSchema EntitySearchType       where
  declareNamedSchema = genericDeclareNamedSchema defaultSchemaOptions
instance ToSchema UnifiedSearchQuery     where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema LinkedMemorySummary    where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema ProjectSearchResult    where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema TaskSearchResult       where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema UnifiedSearchResults   where declareNamedSchema = genericDeclareNamedSchema opts

-- Audit log types
instance ToSchema AuditAction where
  declareNamedSchema _ = pure $ NamedSchema (Just "AuditAction") $ mempty
    & type_ ?~ OpenApiString
    & enum_ ?~ ["create", "update", "delete"]
instance ToParamSchema AuditAction where
  toParamSchema _ = mempty & type_ ?~ OpenApiString & enum_ ?~ ["create", "update", "delete"]
instance ToSchema AuditLogEntry          where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema RevertResult           where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema WebSocketTicketRequest  where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema WebSocketTicketResponse where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema SessionContext          where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema SessionPrincipal        where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema SessionGlobalPermissions where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema SessionWorkspaceContext where declareNamedSchema = genericDeclareNamedSchema opts

------------------------------------------------------------------------
-- Helper
------------------------------------------------------------------------

camelToSnakeField :: String -> String
camelToSnakeField = camelToSnake
