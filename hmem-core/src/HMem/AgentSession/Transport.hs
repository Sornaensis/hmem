-- | Shared HTTP/MCP wire contract. Local paths never enter these DTOs.
module HMem.AgentSession.Transport
  ( CreateSessionRequest(..), AppendSessionRequest(..), TerminateSessionRequest(..)
  , AppendSessionResponse(..), SessionPageResponse(..), SessionTaskPageResponse(..)
  , SessionRangeResponse(..), SessionTransportError(..), decodeSessionBytes, encodeSessionBytes
  , maxSessionRequestBytes, maxSessionBase64Chars, defaultSessionRangeBytes
  ) where

import Control.Monad (unless)
import Data.Aeson
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types (Parser)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Base64 qualified as Base64
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.UUID (UUID)
import GHC.Generics (Generic)
import HMem.AgentSession (AgentSession, SessionPageCursor, SessionTask, maxSessionChunkBytes)
import HMem.Types (jsonOptions)

data CreateSessionRequest = CreateSessionRequest { id :: !UUID, formatLabel :: !Text, title :: !Text, parentSessionId :: !(Maybe UUID) }
  deriving stock (Show, Eq, Generic)
instance ToJSON CreateSessionRequest where toJSON = genericToJSON jsonOptions
instance FromJSON CreateSessionRequest where
  parseJSON = strictObject "CreateSessionRequest" ["id","format_label","title","parent_session_id"] $ \o -> CreateSessionRequest <$> o .: "id" <*> o .: "format_label" <*> o .: "title" <*> o .:? "parent_session_id"

data AppendSessionRequest = AppendSessionRequest
  { expectedOffset :: !Int64, expectedRevision :: !Int64, dataBase64 :: !Text, allowFinalRecord :: !Bool }
  deriving stock (Show, Eq, Generic)
instance ToJSON AppendSessionRequest where toJSON = genericToJSON jsonOptions
instance FromJSON AppendSessionRequest where
  parseJSON = strictObject "AppendSessionRequest" ["expected_offset","expected_revision","data_base64","allow_final_record"] $ \o ->
    AppendSessionRequest <$> o .: "expected_offset" <*> o .: "expected_revision" <*> o .: "data_base64" <*> o .:? "allow_final_record" .!= False

data TerminateSessionRequest = TerminateSessionRequest
  { expectedOffset :: !Int64, expectedRevision :: !Int64, finalBytes :: !Int64, finalSha256 :: !Text }
  deriving stock (Show, Eq, Generic)
instance ToJSON TerminateSessionRequest where toJSON = genericToJSON jsonOptions
instance FromJSON TerminateSessionRequest where
  parseJSON = strictObject "TerminateSessionRequest" ["expected_offset","expected_revision","final_bytes","final_sha256"] $ \o ->
    TerminateSessionRequest <$> o .: "expected_offset" <*> o .: "expected_revision" <*> o .: "final_bytes" <*> o .: "final_sha256"

strictObject :: String -> [Key] -> (Object -> Parser a) -> Value -> Parser a
strictObject name allowed parse = withObject name $ \o -> do
  unless (all (`elem` allowed) (KM.keys o)) (fail "Unsupported session request fields")
  parse o

data AppendSessionResponse = AppendSessionResponse {session :: !AgentSession, replayed :: !Bool}
  deriving stock (Show, Eq, Generic)
instance ToJSON AppendSessionResponse where toJSON = genericToJSON jsonOptions
instance FromJSON AppendSessionResponse where parseJSON = genericParseJSON jsonOptions
data SessionPageResponse = SessionPageResponse {items :: ![AgentSession], nextCursor :: !(Maybe SessionPageCursor)}
  deriving stock (Show, Eq, Generic)
instance ToJSON SessionPageResponse where toJSON = genericToJSON jsonOptions
instance FromJSON SessionPageResponse where parseJSON = genericParseJSON jsonOptions
data SessionTaskPageResponse = SessionTaskPageResponse {items :: ![SessionTask], nextTaskId :: !(Maybe UUID)}
  deriving stock (Show, Eq, Generic)
instance ToJSON SessionTaskPageResponse where toJSON = genericToJSON jsonOptions
instance FromJSON SessionTaskPageResponse where parseJSON = genericParseJSON jsonOptions
data SessionRangeResponse = SessionRangeResponse
  { session :: !AgentSession, offset :: !Int64, dataBase64 :: !Text, nextOffset :: !Int64, hasMore :: !Bool }
  deriving stock (Show, Eq, Generic)
instance ToJSON SessionRangeResponse where toJSON = genericToJSON jsonOptions
instance FromJSON SessionRangeResponse where parseJSON = genericParseJSON jsonOptions

data SessionTransportError = SessionBase64Invalid | SessionBytesTooLarge deriving stock (Show, Eq)

maxSessionRequestBytes, maxSessionBase64Chars, defaultSessionRangeBytes :: Int
maxSessionRequestBytes = 2097152
maxSessionBase64Chars = 1398104
defaultSessionRangeBytes = 65536

encodeSessionBytes :: ByteString -> Text
encodeSessionBytes = TE.decodeUtf8 . Base64.encode

-- Standard padded base64, including canonical zero pad bits. Reject rather
-- than normalizing whitespace, URL alphabet, missing padding or alternate forms.
decodeSessionBytes :: Text -> Either SessionTransportError ByteString
decodeSessionBytes encoded
  | T.length encoded > maxSessionBase64Chars = Left SessionBytesTooLarge
  | otherwise = case Base64.decode (TE.encodeUtf8 encoded) of
      Left _ -> Left SessionBase64Invalid
      Right bytes
        | BS.length bytes > maxSessionChunkBytes -> Left SessionBytesTooLarge
        | encodeSessionBytes bytes /= encoded -> Left SessionBase64Invalid
        | otherwise -> Right bytes
