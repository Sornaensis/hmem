{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module HMem.Server.OpenAPI
  ( openApiSpec
  ) where

import Control.Lens ((&), (.~), (?~))
import Data.Aeson (Value)
import Data.OpenApi
import Data.Proxy (Proxy (..))
import Servant.OpenApi (toOpenApi)

import HMem.Server.API (HMemAPI, CleanupRunReq, GroupMemberReq, CategoryLink)
import HMem.Types

------------------------------------------------------------------------
-- OpenAPI spec generation
------------------------------------------------------------------------

openApiSpec :: OpenApi
openApiSpec = toOpenApi (Proxy @HMemAPI)
  & info . title   .~ "hmem API"
  & info . version .~ "0.1.0.0"
  & info . description ?~ "LLM memory and task management server"

------------------------------------------------------------------------
-- ToSchema instances (orphans, scoped to hmem-server)
------------------------------------------------------------------------

opts :: SchemaOptions
opts = defaultSchemaOptions { fieldLabelModifier = camelToSnakeField }

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
instance ToSchema WorkspaceGroup       where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema CreateWorkspaceGroup where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema Project              where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema CreateProject        where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema UpdateProject        where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema Task                 where declareNamedSchema = genericDeclareNamedSchema opts
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

------------------------------------------------------------------------
-- Helper
------------------------------------------------------------------------

camelToSnakeField :: String -> String
camelToSnakeField = camelToSnake
