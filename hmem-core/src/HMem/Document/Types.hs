-- | Document receipts are independent of agent-session seal receipts.
module HMem.Document.Types where

import Control.Exception (Exception)
import Data.Aeson
import Data.Int (Int64)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.UUID (UUID)
import GHC.Generics (Generic)

jsonOptions :: Options
jsonOptions = defaultOptions { fieldLabelModifier = camelTo2 '_' }

data DocumentRole = Source | Evidence deriving stock (Show, Eq, Generic)
instance ToJSON DocumentRole where toJSON Source = String "source"; toJSON Evidence = String "evidence"
instance FromJSON DocumentRole where parseJSON = withText "DocumentRole" $ \case "source" -> pure Source; "evidence" -> pure Evidence; _ -> fail "Unknown document role"

roleText :: DocumentRole -> Text
roleText Source = "source"
roleText Evidence = "evidence"

data OriginalManifest = OriginalManifest
  { rawBytes :: !Int64, rawSha256 :: !Text, filename :: !Text, title :: !Text, mediaType :: !Text
  } deriving stock (Show, Eq, Generic)
instance ToJSON OriginalManifest where toJSON = genericToJSON jsonOptions
instance FromJSON OriginalManifest where parseJSON = genericParseJSON jsonOptions

data PayloadManifest = PayloadManifest
  { codec :: !Text, rawBytes :: !Int64, rawSha256 :: !Text
  , compressedBytes :: !Int64, compressedSha256 :: !Text, partCount :: !Int
  } deriving stock (Show, Eq, Generic)
instance ToJSON PayloadManifest where toJSON = genericToJSON jsonOptions
instance FromJSON PayloadManifest where parseJSON = genericParseJSON jsonOptions

data UploadDocument = UploadDocument
  { workspaceId :: !UUID, taskId :: !UUID, requestId :: !UUID
  , role :: !DocumentRole, manifest :: !OriginalManifest
  } deriving stock (Show, Eq, Generic)
instance ToJSON UploadDocument where toJSON = genericToJSON jsonOptions
instance FromJSON UploadDocument where parseJSON = genericParseJSON jsonOptions

data Document = Document
  { id :: !UUID, workspaceId :: !UUID, payloadId :: !UUID
  , filename :: !Text, title :: !Text, mediaType :: !Text
  , rawBytes :: !Int64, rawSha256 :: !Text, createdAt :: !UTCTime
  , uploader :: !Value, previewState :: !Text, previewError :: !(Maybe Text)
  } deriving stock (Show, Eq, Generic)
instance ToJSON Document where toJSON = genericToJSON jsonOptions
instance FromJSON Document where parseJSON = genericParseJSON jsonOptions

data DocumentLink = DocumentLink
  { id :: !UUID, taskId :: !UUID, role :: !DocumentRole, document :: !Document
  , createdAt :: !UTCTime
  } deriving stock (Show, Eq, Generic)
instance ToJSON DocumentLink where toJSON = genericToJSON jsonOptions
instance FromJSON DocumentLink where parseJSON = genericParseJSON jsonOptions

data DocumentCursor = DocumentCursor { createdAt :: !UTCTime, id :: !UUID }
  deriving stock (Show, Eq, Generic)
instance ToJSON DocumentCursor where toJSON = genericToJSON jsonOptions
instance FromJSON DocumentCursor where parseJSON = genericParseJSON jsonOptions

data DocumentPage = DocumentPage { items :: ![DocumentLink], nextCursor :: !(Maybe DocumentCursor) }
  deriving stock (Show, Eq, Generic)
instance ToJSON DocumentPage where toJSON = genericToJSON jsonOptions
instance FromJSON DocumentPage where parseJSON = genericParseJSON jsonOptions

data DocumentCounts = DocumentCounts
  { sourceCount :: !Int64, documentEvidenceCount :: !Int64
  , agentSessionEvidenceCount :: !Int64, evidenceTotal :: !Int64
  } deriving stock (Show, Eq, Generic)
instance ToJSON DocumentCounts where toJSON = genericToJSON jsonOptions
instance FromJSON DocumentCounts where parseJSON = genericParseJSON jsonOptions

newtype DocumentException = DocumentException Text deriving stock (Show, Eq)
instance Exception DocumentException

maxOriginalBytes, maxPartBytes, maxDerivativeBytes :: Int64
maxOriginalBytes = 25 * 1024 * 1024
maxPartBytes = 1024 * 1024
maxDerivativeBytes = 100 * 1024 * 1024

-- zlib's bound plus room for deterministic gzip framing/block boundaries.
uploadReservationBytes :: Int64 -> Int64
uploadReservationBytes n = n + n `div` 1000 + 65536
