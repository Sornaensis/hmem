-- | Bounded document-preview policy and immutable verified generation metadata.
module HMem.Document.Preview where

import Control.Monad (unless)
import Data.Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as B8
import Data.Int (Int64)
import Data.List (sortOn)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import HMem.Document.Types (jsonOptions)

previewPolicyVersion, qualifiedPreviewImage :: Text
previewPolicyVersion = "hmem-static-preview-v1"
qualifiedPreviewImage = "sha256:43d5c02754efce8f533ded982f75b1a0f9eca8202c37d186cf1a1b575da890b5"

data PreviewLimits = PreviewLimits
  { originalBytes :: !Int64, imagePixels :: !Int64, pdfPages :: !Int
  , pagePixels :: !Int64, longestSide :: !Int, pdfDecodedBytes :: !Int64
  , outputBytes :: !Int64, htmlBytes :: !Int64, domNodes :: !Int
  , domDepth :: !Int, cssRules :: !Int, embeddedImages :: !Int
  } deriving stock (Show,Eq,Generic)
defaultPreviewLimits :: PreviewLimits
defaultPreviewLimits = PreviewLimits (25*1024*1024) 40000000 200 16000000 4096
  (2*1024*1024*1024) (100*1024*1024) (5*1024*1024) 20000 64 2000 100

limitPairs :: PreviewLimits -> [(Text,Int64)]
limitPairs value =
  [("original_bytes",value.originalBytes),("image_pixels",value.imagePixels),("pdf_pages",fromIntegral value.pdfPages)
  ,("page_pixels",value.pagePixels),("longest_side",fromIntegral value.longestSide),("pdf_decoded_bytes",value.pdfDecodedBytes)
  ,("output_bytes",value.outputBytes),("html_bytes",value.htmlBytes),("dom_nodes",fromIntegral value.domNodes)
  ,("dom_depth",fromIntegral value.domDepth),("css_rules",fromIntegral value.cssRules),("embedded_images",fromIntegral value.embeddedImages)]
validPreviewLimits :: PreviewLimits -> Bool
validPreviewLimits value = and(zipWith (\(_,actual) (_,maximumValue)->actual>=1 &&actual<=maximumValue)
  (limitPairs value)(limitPairs defaultPreviewLimits))
instance ToJSON PreviewLimits where toJSON = genericToJSON jsonOptions
instance FromJSON PreviewLimits where
  parseJSON = withObject "PreviewLimits" $ \o -> do
    let defaults=defaultPreviewLimits
    unless(all ((\key->elem key(map fst(limitPairs defaults))) . Key.toText)(KM.keys o))(fail "Unknown preview limit")
    value<-PreviewLimits <$> o .:? "original_bytes" .!= defaults.originalBytes <*> o .:? "image_pixels" .!= defaults.imagePixels
      <*> o .:? "pdf_pages" .!= defaults.pdfPages <*> o .:? "page_pixels" .!= defaults.pagePixels
      <*> o .:? "longest_side" .!= defaults.longestSide <*> o .:? "pdf_decoded_bytes" .!= defaults.pdfDecodedBytes
      <*> o .:? "output_bytes" .!= defaults.outputBytes <*> o .:? "html_bytes" .!= defaults.htmlBytes
      <*> o .:? "dom_nodes" .!= defaults.domNodes <*> o .:? "dom_depth" .!= defaults.domDepth
      <*> o .:? "css_rules" .!= defaults.cssRules <*> o .:? "embedded_images" .!= defaults.embeddedImages
    unless(validPreviewLimits value)(fail "Preview limits may only tighten the qualified bounds")
    pure value

-- The limits contain only known ASCII keys and positive integers. This matches
-- the worker's sorted compact ASCII JSON without reserializing floating plans.
canonicalPreviewLimits :: PreviewLimits -> ByteString
canonicalPreviewLimits value = "{"<>B8.intercalate "," [B8.pack("\""<>T.unpack key<>"\":"<>show count)
  |(key,count)<-sortOn fst(limitPairs value)]<>"}"

data PreviewConfig = PreviewConfig
  { enabled :: !Bool, pythonExecutable :: !FilePath, launcherPath :: !(Maybe FilePath)
  , imageId :: !Text, workers :: !Int, queueLimit :: !Int, maxAttempts :: !Int
  , imageSeconds :: !Int, pdfSeconds :: !Int, limits :: !PreviewLimits
  } deriving stock (Eq,Generic)
instance Show PreviewConfig where
  show config = "PreviewConfig {enabled="<>show config.enabled<>", launcherConfigured="
    <>show(config.launcherPath/=Nothing)<>", workers="<>show config.workers<>", queueLimit="<>show config.queueLimit<>"}"
defaultPreviewConfig :: PreviewConfig
defaultPreviewConfig = PreviewConfig False "python" Nothing qualifiedPreviewImage 2 32 3 30 120 defaultPreviewLimits
validHash :: Text -> Bool
validHash value = T.length value==64 &&T.all(\c->(c>='0'&&c<='9')||(c>='a'&&c<='f'))value
validPreviewConfig :: PreviewConfig -> Bool
validPreviewConfig config = config.workers>=1 &&config.workers<=2 &&config.queueLimit>=1 &&config.queueLimit<=32
  &&config.maxAttempts>=1 &&config.maxAttempts<=3 &&config.imageSeconds>=6 &&config.imageSeconds<=30
  &&config.pdfSeconds>=6 &&config.pdfSeconds<=120 &&validPreviewLimits config.limits
  &&maybe False validHash(T.stripPrefix "sha256:" config.imageId) &&not(null config.pythonExecutable)
  &&maybe True (not . null) config.launcherPath
instance ToJSON PreviewConfig where toJSON = genericToJSON jsonOptions
instance FromJSON PreviewConfig where
  parseJSON = withObject "PreviewConfig" $ \o -> do
    unless(all ((\key->elem key ["enabled","python_executable","launcher_path","image_id","workers","queue_limit","max_attempts","image_seconds","pdf_seconds","limits"]) . Key.toText)(KM.keys o))(fail "Unknown preview setting")
    let defaults=defaultPreviewConfig
    config<-PreviewConfig <$> o .:? "enabled" .!= defaults.enabled <*> o .:? "python_executable" .!= defaults.pythonExecutable
      <*> o .:? "launcher_path" <*> o .:? "image_id" .!= defaults.imageId <*> o .:? "workers" .!= defaults.workers
      <*> o .:? "queue_limit" .!= defaults.queueLimit <*> o .:? "max_attempts" .!= defaults.maxAttempts
      <*> o .:? "image_seconds" .!= defaults.imageSeconds <*> o .:? "pdf_seconds" .!= defaults.pdfSeconds
      <*> o .:? "limits" .!= defaults.limits
    unless(validPreviewConfig config)(fail "Invalid bounded preview configuration")
    pure config

data PreviewRun = PreviewRun
  { id :: !UUID, workspaceId :: !UUID, documentId :: !UUID, jobVersion :: !Int64
  , controllerId :: !UUID, leaseToken :: !UUID, deadlineAt :: !UTCTime, leaseUntil :: !UTCTime
  , rawBytes :: !Int64, rawSha256 :: !Text, mediaType :: !Text
  , policyVersion :: !Text, imageId :: !Text, limits :: !PreviewLimits
  } deriving stock (Show,Eq,Generic)
instance FromJSON PreviewRun where parseJSON = genericParseJSON jsonOptions
instance ToJSON PreviewRun where toJSON = genericToJSON jsonOptions

data PreviewMetadata = PreviewMetadata
  { planSha256 :: !Text, originalSha256 :: !Text, limitsSha256 :: !Text
  , policyVersion :: !Text, imageId :: !Text, outputCount :: !Int, pageCount :: !Int
  , decodedPixels :: !Int64, pdfDecodedBytes :: !Int64, animatedFirstFrame :: !Bool
  } deriving stock (Show,Eq,Generic)
instance ToJSON PreviewMetadata where
  toJSON metadata = case genericToJSON jsonOptions metadata of
    Object fields->Object(KM.insert "verification"(String "verified")fields)
    value->value
instance FromJSON PreviewMetadata where
  parseJSON = withObject "PreviewMetadata" $ \o -> do
    verification<-o .: "verification"
    unless(verification==("verified"::Text))(fail "Unverified generation metadata")
    value<-PreviewMetadata <$>o .: "plan_sha256" <*>o .: "original_sha256" <*>o .: "limits_sha256"
      <*>o .: "policy_version" <*>o .: "image_id" <*>o .: "output_count" <*>o .: "page_count"
      <*>o .: "decoded_pixels" <*>o .: "pdf_decoded_bytes" <*>o .: "animated_first_frame"
    unless(validPreviewMetadata value)(fail "Invalid generation metadata")
    pure value
validPreviewMetadata :: PreviewMetadata -> Bool
validPreviewMetadata value = all validHash [value.planSha256,value.originalSha256,value.limitsSha256]
  &&value.policyVersion==previewPolicyVersion &&maybe False validHash(T.stripPrefix "sha256:" value.imageId)
  &&value.outputCount>=1 &&value.outputCount<=401 &&value.pageCount>=0 &&value.pageCount<=200
  &&value.decodedPixels>=0 &&value.decodedPixels<=40000000 &&value.pdfDecodedBytes>=0 &&value.pdfDecodedBytes<=2*1024*1024*1024
