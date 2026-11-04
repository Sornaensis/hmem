-- | Nonsecret bindings for document capabilities. Raw bearer tokens are transport-only.
module HMem.Document.Grant where

import Control.Monad (unless)
import Data.Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Int (Int64)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import HMem.Auth.CredentialOrigin (GrantIssuer)
import HMem.Document.Types (DocumentRole,jsonOptions)
import HMem.Document.Preview (PreviewMetadata)

data GrantRequest = GrantRequest
  { role :: !DocumentRole, associationId :: !UUID, variant :: !Text, page :: !Int
  , generationId :: !(Maybe UUID), ttlSeconds :: !(Maybe Int)
  } deriving stock (Show,Eq,Generic)
instance FromJSON GrantRequest where
  parseJSON = withObject "GrantRequest" $ \o -> do
    unless (all ((`elem` ["role","association_id","variant","page","generation_id","ttl_seconds"]) . Key.toText) (KM.keys o))
      (fail "Unknown grant request field")
    request <- GrantRequest <$> o .: "role" <*> o .: "association_id" <*> o .: "variant" <*> o .: "page"
      <*> o .:? "generation_id" <*> o .:? "ttl_seconds"
    unless (validGrantRequest request) (fail "Invalid bounded grant representation or lifetime")
    pure request
instance ToJSON GrantRequest where toJSON = genericToJSON jsonOptions

validGrantRequest :: GrantRequest -> Bool
validGrantRequest request = maybe True (\seconds -> seconds>=1 && seconds<=900) request.ttlSeconds
  && validRepresentation request.variant request.page request.generationId

validRepresentation :: Text -> Int -> Maybe UUID -> Bool
validRepresentation variant page generation = page>=0 && page<=200 && case variant of
    "original" -> page==0 && generation==Nothing
    "styled_html_preview" -> page==0 && generation/=Nothing
    "page" -> generation/=Nothing
    "thumbnail" -> generation/=Nothing
    _ -> False

data GrantBinding = GrantBinding
  { workspaceId :: !UUID, taskId :: !UUID, documentId :: !UUID
  , role :: !DocumentRole, associationId :: !UUID, variant :: !Text, page :: !Int
  , generationId :: !(Maybe UUID), policyVersion :: !(Maybe Text)
  , originalSha256 :: !Text, payloadId :: !UUID
  , rawBytes :: !Int64, rawSha256 :: !Text, mediaType :: !Text, filename :: !Text
  } deriving stock (Show,Eq,Generic)
instance FromJSON GrantBinding where parseJSON = genericParseJSON jsonOptions
instance ToJSON GrantBinding where toJSON = genericToJSON jsonOptions

-- Never expose the internal payload identifier in product metadata.
publicBinding :: GrantBinding -> Value
publicBinding b = object
  ["workspace_id" .= b.workspaceId,"task_id" .= b.taskId,"document_id" .= b.documentId
  ,"role" .= b.role,"association_id" .= b.associationId,"variant" .= b.variant,"page" .= b.page
  ,"generation_id" .= b.generationId,"policy_version" .= b.policyVersion
  ,"manifest" .= object ["raw_bytes" .= b.rawBytes,"raw_sha256" .= b.rawSha256
    ,"media_type" .= b.mediaType,"filename" .= b.filename,"original_sha256" .= b.originalSha256]]

data StoredGrant = StoredGrant
  { id :: !UUID, binding :: !GrantBinding, issuer :: !GrantIssuer, authorityEpoch :: !UUID
  , createdAt :: !UTCTime, expiresAt :: !UTCTime, revokedAt :: !(Maybe UTCTime)
  } deriving stock (Show,Eq,Generic)
instance FromJSON StoredGrant where parseJSON = genericParseJSON jsonOptions

data PreviewOutput = PreviewOutput
  { variant :: !Text, page :: !Int, mediaType :: !Text, rawBytes :: !Int64, rawSha256 :: !Text
  } deriving stock (Show,Eq,Generic)
instance ToJSON PreviewOutput where toJSON = genericToJSON jsonOptions
instance FromJSON PreviewOutput where parseJSON = genericParseJSON jsonOptions

data PreviewStatus = PreviewStatus
  { state :: !Text, errorCode :: !(Maybe Text), generationId :: !(Maybe UUID)
  , policyVersion :: !(Maybe Text), outputs :: ![PreviewOutput]
  , generationMetadata :: !(Maybe PreviewMetadata), retryable :: !Bool
  } deriving stock (Show,Eq)
instance ToJSON PreviewStatus where
  toJSON p = object ["state" .= p.state,"error" .= p.errorCode,"generation_id" .= p.generationId
    ,"policy_version" .= p.policyVersion,"outputs" .= p.outputs
    , "retryable" .= p.retryable,"generation_metadata" .= case p.generationMetadata of
       Just metadata->toJSON metadata
       Nothing->if p.generationId==Nothing then Null else object["verification" .= ("unknown"::Text)]]
instance FromJSON PreviewStatus where
  parseJSON = withObject "PreviewStatus" $ \o -> do
    value<-o .:? "generation_metadata"
    metadata<-case value of
      Nothing->pure Nothing
      Just(Object fields) | KM.lookup "verification" fields==Just(String "unknown")->pure Nothing
      Just found->Just <$>parseJSON found
    PreviewStatus <$> o .: "state" <*> o .:? "error" <*> o .:? "generation_id"
      <*> o .:? "policy_version" <*> o .: "outputs" <*>pure metadata <*>o .:? "retryable" .!= False
