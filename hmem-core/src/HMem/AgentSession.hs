-- | Provider-neutral metadata and byte-oriented agent session inputs.
module HMem.AgentSession
  ( SessionState(..), SessionFreshness(..), SessionFilter(..), defaultSessionFilter, AgentSession(..), CreateSession(..)
  , AppendSession(..), TerminateSession(..), SessionAppendResult(..)
  , SessionPageCursor(..), SessionPage(..), SessionTask(..), SessionTaskPage(..)
  , SessionRange(..), SessionException(..), SessionErrorCode(..)
  , maxSessionChunkBytes, maxSessionRecordBytes, maxSessionRangeBytes, maxSessionPageSize
  , sessionSha256
  ) where

import Control.Exception (Exception)
import Crypto.Hash (Digest, SHA256, hash)
import Data.Aeson
import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import HMem.BotIdentity (BotIdentity)
import HMem.Types (jsonOptions)

data SessionState = Active | Terminated deriving stock (Show, Eq, Generic)
instance ToJSON SessionState where
  toJSON Active = String "active"
  toJSON Terminated = String "terminated"
instance FromJSON SessionState where
  parseJSON = withText "SessionState" $ \case
    "active" -> pure Active
    "terminated" -> pure Terminated
    _ -> fail "Invalid agent session state"

-- | Freshness is independent of lifecycle. The database applies both filters
-- before keyset pagination, using its statement clock and the 30-second policy.
data SessionFreshness = Fresh | Stale deriving stock (Show, Eq)
data SessionFilter = SessionFilter
  { state :: !(Maybe SessionState), freshness :: !(Maybe SessionFreshness)
  } deriving stock (Show, Eq)

defaultSessionFilter :: SessionFilter
defaultSessionFilter = SessionFilter Nothing Nothing

data AgentSession = AgentSession
  { id :: !UUID
  , workspaceId :: !UUID
  , formatLabel :: !Text
  , state :: !SessionState
  , creatorActorType :: !(Maybe Text)
  , creatorActorId :: !(Maybe Text)
  , creatorActorLabel :: !(Maybe Text)
  , creatorBotIdentity :: !(Maybe BotIdentity)
  , createdAt :: !UTCTime
  , lastSeenAt :: !UTCTime
  , committedBytes :: !Int64
  , committedRecords :: !Int64
  , revision :: !Int64
  , endsWithNewline :: !Bool
  , terminatedAt :: !(Maybe UTCTime)
  , finalBytes :: !(Maybe Int64)
  , finalSha256 :: !(Maybe Text)
  } deriving stock (Show, Eq, Generic)
instance ToJSON AgentSession where toJSON = genericToJSON jsonOptions
instance FromJSON AgentSession where parseJSON = genericParseJSON jsonOptions

data CreateSession = CreateSession
  { id :: !UUID, workspaceId :: !UUID, formatLabel :: !Text
  } deriving stock (Show, Eq, Generic)
instance ToJSON CreateSession where toJSON = genericToJSON jsonOptions
instance FromJSON CreateSession where parseJSON = genericParseJSON jsonOptions

-- | Bytes are never JSON encoded here. HTTP/MCP adapters own base64 encoding.
data AppendSession = AppendSession
  { expectedOffset :: !Int64, expectedRevision :: !Int64
  , bytes :: !ByteString, allowFinalRecord :: !Bool
  } deriving stock (Show, Eq)
data TerminateSession = TerminateSession
  { expectedOffset :: !Int64, expectedRevision :: !Int64
  , finalBytes :: !Int64, finalSha256 :: !Text
  } deriving stock (Show, Eq, Generic)
instance ToJSON TerminateSession where toJSON = genericToJSON jsonOptions
instance FromJSON TerminateSession where parseJSON = genericParseJSON jsonOptions
data SessionAppendResult = SessionAppendResult
  { session :: !AgentSession, replayed :: !Bool
  } deriving stock (Show, Eq, Generic)
instance ToJSON SessionAppendResult where toJSON = genericToJSON jsonOptions

-- | Descending immutable (created_at, UUID) keyset; no offset drift on creation.
data SessionPageCursor = SessionPageCursor
  { createdAt :: !UTCTime, id :: !UUID
  } deriving stock (Show, Eq, Generic)
instance ToJSON SessionPageCursor where toJSON = genericToJSON jsonOptions
instance FromJSON SessionPageCursor where parseJSON = genericParseJSON jsonOptions
data SessionPage = SessionPage
  { items :: ![AgentSession], nextCursor :: !(Maybe SessionPageCursor)
  } deriving stock (Show, Eq, Generic)
instance ToJSON SessionPage where toJSON = genericToJSON jsonOptions
data SessionTask = SessionTask
  { id :: !UUID, title :: !Text, status :: !Text
  } deriving stock (Show, Eq, Generic)
instance ToJSON SessionTask where toJSON = genericToJSON jsonOptions
instance FromJSON SessionTask where parseJSON = genericParseJSON jsonOptions
data SessionTaskPage = SessionTaskPage
  { items :: ![SessionTask], nextTaskId :: !(Maybe UUID)
  } deriving stock (Show, Eq, Generic)
instance ToJSON SessionTaskPage where toJSON = genericToJSON jsonOptions

data SessionRange = SessionRange
  { session :: !AgentSession, offset :: !Int64, bytes :: !ByteString
  , nextOffset :: !Int64, hasMore :: !Bool
  } deriving stock (Show, Eq)

data SessionErrorCode = SessionNotFound | SessionConflict | SessionTerminated | SessionInvalidInput
  deriving stock (Show, Eq)
data SessionException = SessionException
  { code :: !SessionErrorCode, message :: !Text
  } deriving stock (Show, Eq)
instance Exception SessionException

maxSessionChunkBytes, maxSessionRecordBytes, maxSessionRangeBytes, maxSessionPageSize :: Int
maxSessionChunkBytes = 1048576
maxSessionRecordBytes = 1048576
maxSessionRangeBytes = 1048576
maxSessionPageSize = 100

sessionSha256 :: ByteString -> Text
sessionSha256 value = T.pack (show (hash value :: Digest SHA256))
