{-# LANGUAGE DeriveGeneric #-}

module HMem.Server.Event
  ( -- * Types
    EntityType(..)
  , ChangeType(..)
  , ChangeEvent(..)
    -- * Broadcast function type
  , Broadcast
    -- * Helpers
  , entityTypeToText
  , changeTypeToText
  ) where

import Data.Aeson (ToJSON(..), Value, object, (.=))
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import GHC.Generics (Generic)

-- | The kind of entity that changed.
data EntityType
  = ETWorkspace
  | ETProject
  | ETTask
  | ETMemory
  | ETMemoryLink
  | ETCategory
  | ETWorkspaceGroup
  | ETSavedView
  | ETTaskDependency
  | ETCategoryLink
  | ETTag
  deriving (Show, Eq, Generic)

-- | Whether the entity was created, updated, or deleted.
data ChangeType = Created | Updated | Deleted
  deriving (Show, Eq, Generic)

-- | A single change event broadcast over WebSocket.
data ChangeEvent = ChangeEvent
  { changeType  :: !ChangeType
  , entityType  :: !EntityType
  , entityId    :: !UUID
  , timestamp   :: !UTCTime
  , requestId   :: !(Maybe Text)
  , payload     :: !(Maybe Value)
  } deriving (Show, Generic)

-- | Function to broadcast a change event to all connected clients.
type Broadcast = ChangeEvent -> IO ()

entityTypeToText :: EntityType -> Text
entityTypeToText = \case
  ETWorkspace      -> "workspace"
  ETProject        -> "project"
  ETTask           -> "task"
  ETMemory         -> "memory"
  ETMemoryLink     -> "memory_link"
  ETCategory       -> "category"
  ETWorkspaceGroup -> "workspace_group"
  ETSavedView      -> "saved_view"
  ETTaskDependency -> "task_dependency"
  ETCategoryLink   -> "category_link"
  ETTag            -> "tag"

changeTypeToText :: ChangeType -> Text
changeTypeToText = \case
  Created -> "entity_created"
  Updated -> "entity_updated"
  Deleted -> "entity_deleted"

instance ToJSON ChangeEvent where
  toJSON ev = object $
    [ "type"        .= changeTypeToText ev.changeType
    , "entity_type" .= entityTypeToText ev.entityType
    , "entity_id"   .= UUID.toText ev.entityId
    , "timestamp"   .= ev.timestamp
    ]
    ++ maybe [] (\rid -> ["request_id" .= rid]) ev.requestId
    ++ maybe [] (\d -> ["data" .= d]) ev.payload
