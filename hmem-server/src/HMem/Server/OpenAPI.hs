{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | OpenAPI definitions are kept beside the Servant contract so removed
-- endpoints cannot accidentally survive in generated documentation.
module HMem.Server.OpenAPI (openApiSpec) where

import Control.Lens ((&), (.~), (%~), (?~), at, _Just)
import Data.Aeson (Value)
import Data.HashSet.InsOrd qualified as InsOrdSet
import Data.OpenApi
import Data.Proxy (Proxy(..))
import Servant.OpenApi (toOpenApi)

import HMem.Server.API (HMemAPI)
import HMem.Types

openApiSpec :: OpenApi
openApiSpec = toOpenApi (Proxy @HMemAPI)
  & info . title .~ "hmem API"
  & info . version .~ "0.2.0.0"
  & info . description ?~ "Repository-scoped observation API with immutable provenance."
  & tags .~ InsOrdSet.fromList
      [ Tag "Observations" (Just "Repository-scoped, provenance-bound observations.") Nothing
      , Tag "Workspace Groups" (Just "Global-superadmin workspace group management.") Nothing
      ]
  & paths . at "/api/v1/groups" . _Just . get %~ fmap tagWorkspaceGroups
  & paths . at "/api/v1/groups" . _Just . post %~ fmap tagWorkspaceGroups
  & paths . at "/api/v1/groups/{groupId}" . _Just . get %~ fmap tagWorkspaceGroups
  & paths . at "/api/v1/groups/{groupId}" . _Just . delete %~ fmap tagWorkspaceGroups
  & paths . at "/api/v1/groups/{groupId}/members" . _Just . get %~ fmap tagWorkspaceGroups
  & paths . at "/api/v1/groups/{groupId}/members" . _Just . post %~ fmap tagWorkspaceGroups
  & paths . at "/api/v1/groups/{groupId}/members/{workspaceId}" . _Just . delete %~ fmap tagWorkspaceGroups
  & paths . at "/api/v1/observations" . _Just . get %~ fmap tagObservation
  & paths . at "/api/v1/observations" . _Just . post %~ fmap tagObservation
  & paths . at "/api/v1/observations/similar" . _Just . post %~ fmap tagObservation
  & paths . at "/api/v1/observations/{observationId}" . _Just . get %~ fmap tagObservation
  & paths . at "/api/v1/observations/{observationId}" . _Just . put %~ fmap tagObservation
  & paths . at "/api/v1/observations/{observationId}" . _Just . delete %~ fmap tagObservation
  & paths . at "/api/v1/observations/{observationId}/embedding" . _Just . put %~ fmap tagObservation
  where
    tagObservation operation = operation & tags .~ InsOrdSet.singleton "Observations"
    tagWorkspaceGroups operation = operation & tags .~ InsOrdSet.singleton "Workspace Groups"

opts :: SchemaOptions
opts = defaultSchemaOptions { fieldLabelModifier = camelToSnake }

instance ToSchema Value where declareNamedSchema _ = pure (NamedSchema (Just "JSONValue") mempty)
instance ToSchema a => ToSchema (FieldUpdate a) where declareNamedSchema _ = declareNamedSchema (Proxy @a)
instance ToSchema SubjectKind where
  declareNamedSchema _ = pure $ NamedSchema (Just "SubjectKind") (mempty & type_ ?~ OpenApiString & enum_ ?~ ["file", "glob"])
instance ToParamSchema SubjectKind where toParamSchema _ = mempty & type_ ?~ OpenApiString & enum_ ?~ ["file", "glob"]
instance ToSchema EntitySearchType where
  declareNamedSchema _ = pure $ NamedSchema (Just "EntitySearchType") (mempty & type_ ?~ OpenApiString & enum_ ?~ ["observation", "project", "task"])
instance ToSchema ObservationSearchHit where declareNamedSchema = genericDeclareNamedSchema opts
instance ToParamSchema EntitySearchType where toParamSchema _ = mempty & type_ ?~ OpenApiString & enum_ ?~ ["observation", "project", "task"]
instance ToSchema ProjectStatus where declareNamedSchema _ = pure $ NamedSchema (Just "ProjectStatus") (mempty & type_ ?~ OpenApiString & enum_ ?~ ["active", "paused", "completed", "archived"])
instance ToParamSchema ProjectStatus where toParamSchema _ = mempty & type_ ?~ OpenApiString
instance ToSchema TaskStatus where declareNamedSchema _ = pure $ NamedSchema (Just "TaskStatus") (mempty & type_ ?~ OpenApiString & enum_ ?~ ["todo", "in_progress", "blocked", "done", "cancelled"])
instance ToParamSchema TaskStatus where toParamSchema _ = mempty & type_ ?~ OpenApiString
instance ToSchema AuditAction where declareNamedSchema _ = pure $ NamedSchema (Just "AuditAction") (mempty & type_ ?~ OpenApiString & enum_ ?~ ["create", "update", "delete"])
instance ToParamSchema AuditAction where toParamSchema _ = mempty & type_ ?~ OpenApiString
instance ToSchema WorkspaceType where declareNamedSchema _ = pure $ NamedSchema (Just "WorkspaceType") (mempty & type_ ?~ OpenApiString & enum_ ?~ ["repository", "planning", "personal", "organization"])

instance ToSchema ObservationEmbedding where
  declareNamedSchema _ = pure $ NamedSchema (Just "ObservationEmbedding") embeddingSchema
instance ToSchema Observation where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema CreateObservation where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema UpdateObservation where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema SimilarObservationQuery where
  declareNamedSchema _ = do
    NamedSchema name schema <- genericDeclareNamedSchema opts (Proxy @SimilarObservationQuery)
    pure $ NamedSchema name (schema & properties . at "embedding" ?~ Inline embeddingSchema)
instance ToSchema SimilarObservation where declareNamedSchema = genericDeclareNamedSchema opts

-- Both vector endpoints accept precisely the storage dimension.  Keep this
-- schema separate from the runtime validation so the generated contract cannot
-- silently drift from pgvector(1536).
embeddingSchema :: Schema
embeddingSchema = mempty
  & type_ ?~ OpenApiArray
  & items ?~ OpenApiItemsObject (Inline (mempty & type_ ?~ OpenApiNumber))
  & minItems ?~ fromIntegral observationEmbeddingDimensions
  & maxItems ?~ fromIntegral observationEmbeddingDimensions
instance ToSchema Workspace where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema CreateWorkspace where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema WorkspaceGroup where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema CreateWorkspaceGroup where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema WorkspaceGroupMemberInput where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema Project where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema CreateProject where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema UpdateProject where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema CascadeResult where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema ProjectOverview where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema ProjectReadinessRollup where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema NextTaskCandidate where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema Task where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema CreateTask where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema UpdateTask where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema TaskMutationResult where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema TaskDependencyStatusChange where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema TaskOverview where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema TaskReadinessRollup where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema TaskDependencySummary where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema UnifiedSearchQuery where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema UnifiedSearchResults where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema AuditLogEntry where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema RevertResult where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema WebSocketTicketRequest where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema WebSocketTicketResponse where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema SessionContext where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema SessionPrincipal where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema SessionGlobalPermissions where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema SessionWorkspaceContext where declareNamedSchema = genericDeclareNamedSchema opts
instance ToSchema a => ToSchema (PaginatedResult a) where declareNamedSchema = genericDeclareNamedSchema opts
