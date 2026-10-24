-- | Provider-neutral metadata and byte-oriented agent session inputs.
module HMem.AgentSession
  ( SessionState(..), SessionFreshness(..), SessionFilter(..), defaultSessionFilter, AgentSession(..), CreateSession(..)
  , AppendSession(..), TerminateSession(..), SessionAppendResult(..), RuntimeIdentity(..), SetRuntimeIdentity(..), runtimeIdentityValid
  , SessionPageCursor(..), SessionPage(..), SessionTask(..), SessionTaskPage(..), SessionAncestor(..), SessionFacet(..), SessionFacetPage(..)
  , SessionRange(..), SessionException(..), SessionErrorCode(..)
  , maxSessionChunkBytes, maxSessionRecordBytes, maxSessionRangeBytes, maxSessionPageSize
  , sessionSha256
  ) where

import Control.Exception (Exception)
import Control.Monad (unless)
import Crypto.Hash (Digest, SHA256, hash)
import Data.Aeson
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.ByteString qualified as BS
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
  , projectId :: !(Maybe UUID), taskId :: !(Maybe UUID)
  } deriving stock (Show, Eq)

defaultSessionFilter :: SessionFilter
defaultSessionFilter = SessionFilter Nothing Nothing Nothing Nothing

-- | A declaration about the target session, never the connecting actor.
-- A model name alone is useful; missing provider/reasoning are not invented.
data RuntimeIdentity = RuntimeIdentity
  { model :: !Text, provider :: !(Maybe Text), reasoning :: !(Maybe Text)
  } deriving stock (Show, Eq, Generic)
instance ToJSON RuntimeIdentity where toJSON = genericToJSON jsonOptions
instance FromJSON RuntimeIdentity where
  parseJSON = withObject "RuntimeIdentity" $ \o -> do
    unless (all (`elem` ["model","provider","reasoning"]) (KM.keys o)) (fail "Unsupported runtime identity fields")
    identity <- RuntimeIdentity <$> o .: "model" <*> o .:? "provider" <*> o .:? "reasoning"
    unless (runtimeIdentityValid identity) (fail "Runtime identity requires a short model name and valid optional metadata")
    pure identity

runtimeIdentityValid :: RuntimeIdentity -> Bool
runtimeIdentityValid identity = valid 256 identity.model && all (maybe True (valid 128)) [identity.provider,identity.reasoning]
  where valid limit value = not (T.null value) && value == T.strip value && not (T.any (== '\0') value) && BS.length (TE.encodeUtf8 value) <= limit

data SetRuntimeIdentity = SetRuntimeIdentity
  { expectedRuntimeRevision :: !Int64, runtimeIdentity :: !(Maybe RuntimeIdentity)
  } deriving stock (Show, Eq, Generic)
-- Clearing is an explicit operation; null must not be omitted by jsonOptions.
instance ToJSON SetRuntimeIdentity where
  toJSON input = object ["expected_runtime_revision" .= input.expectedRuntimeRevision,"runtime_identity" .= input.runtimeIdentity]
instance FromJSON SetRuntimeIdentity where
  parseJSON = withObject "SetRuntimeIdentity" $ \o -> do
    unless (all (`elem` ["expected_runtime_revision","runtime_identity"]) (KM.keys o)) (fail "Unsupported runtime update fields")
    SetRuntimeIdentity <$> o .: "expected_runtime_revision" <*> o .: "runtime_identity"

data AgentSession = AgentSession
  { id :: !UUID
  , workspaceId :: !UUID
  , formatLabel :: !Text
  , title :: !(Maybe Text)
  , parentSessionId :: !(Maybe UUID)
  , ancestors :: ![SessionAncestor]
  , ancestorsTruncated :: !Bool
  , state :: !SessionState
  , creatorActorType :: !(Maybe Text)
  , creatorActorId :: !(Maybe Text)
  , creatorActorLabel :: !(Maybe Text)
  , creatorBotIdentity :: !(Maybe BotIdentity)
  , runtimeIdentity :: !(Maybe RuntimeIdentity)
  , runtimeRevision :: !Int64
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
instance FromJSON AgentSession where
  parseJSON = withObject "AgentSession" $ \o -> AgentSession
    <$> o .: "id" <*> o .: "workspace_id" <*> o .: "format_label"
    <*> o .:? "title" <*> o .:? "parent_session_id" <*> o .:? "ancestors" .!= [] <*> o .:? "ancestors_truncated" .!= False
    <*> o .: "state" <*> o .:? "creator_actor_type" <*> o .:? "creator_actor_id" <*> o .:? "creator_actor_label" <*> o .:? "creator_bot_identity"
    <*> o .:? "runtime_identity" <*> o .:? "runtime_revision" .!= 0
    <*> o .: "created_at" <*> o .: "last_seen_at" <*> o .: "committed_bytes" <*> o .: "committed_records" <*> o .: "revision"
    <*> o .: "ends_with_newline" <*> o .:? "terminated_at" <*> o .:? "final_bytes" <*> o .:? "final_sha256"

data SessionAncestor = SessionAncestor
  { id :: !UUID, title :: !(Maybe Text), formatLabel :: !Text, state :: !SessionState, parentSessionId :: !(Maybe UUID)
  } deriving stock (Show, Eq, Generic)
instance ToJSON SessionAncestor where toJSON = genericToJSON jsonOptions
instance FromJSON SessionAncestor where parseJSON = genericParseJSON jsonOptions

data SessionFacet = SessionFacet { id :: !UUID, title :: !Text } deriving stock (Show, Eq, Generic)
instance ToJSON SessionFacet where toJSON = genericToJSON jsonOptions
instance FromJSON SessionFacet where parseJSON = genericParseJSON jsonOptions
data SessionFacetPage = SessionFacetPage { items :: ![SessionFacet], nextId :: !(Maybe UUID), selected :: !(Maybe SessionFacet) } deriving stock (Show, Eq, Generic)
instance ToJSON SessionFacetPage where toJSON = genericToJSON jsonOptions
instance FromJSON SessionFacetPage where parseJSON = genericParseJSON jsonOptions

data CreateSession = CreateSession
  { id :: !UUID, workspaceId :: !UUID, formatLabel :: !Text, title :: !Text, parentSessionId :: !(Maybe UUID), initialRuntimeIdentity :: !(Maybe RuntimeIdentity), taskId :: !UUID
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

data SessionErrorCode = SessionNotFound | SessionTaskNotFound | SessionConflict | SessionRuntimeConflict | SessionTerminated | SessionInvalidInput | SessionTaskNotInProgress !Text
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
