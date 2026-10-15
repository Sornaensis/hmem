-- | Self-declared metadata, never credentials or authorization grants.
module HMem.BotIdentity
  ( BotIdentity, botRole, botModelId, mkBotIdentity, localDeclaredBotId
  ) where

import Control.Monad (unless)
import Crypto.Hash (Digest, SHA256, hash)
import Data.Aeson
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Char (GeneralCategory(Format), generalCategory, isAscii, isAlphaNum, isControl)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE

-- The constructor is private so all callers share the HTTP validation contract.
data BotIdentity = BotIdentity
  { identityRole :: !Text
  , identityModelId :: !Text
  } deriving stock (Show, Eq)

botRole :: BotIdentity -> Text
botRole identity = identity.identityRole

botModelId :: BotIdentity -> Text
botModelId identity = identity.identityModelId

-- | Bounds apply to raw UTF-8 input, before stripping outer whitespace.
-- Case and internal role whitespace are preserved; no Unicode normalization or
-- model catalog is used. model-id has exactly harness:model:reasoning components.
mkBotIdentity :: Text -> Text -> Either Text BotIdentity
mkBotIdentity rawRole rawModel = do
  validateField "role" 128 rawRole
  validateField "model-id" 256 rawModel
  let role = T.strip rawRole
      model = T.strip rawModel
      parts = T.splitOn ":" model
      validPart part = not (T.null part) && T.all validComponentChar part
  unless (length parts == 3 && all validPart parts) $
    Left "model-id must be harness:model:reasoning using ASCII letters, digits, '.', '_', '/', or '-' in each component"
  pure (BotIdentity role model)
  where
    validComponentChar c = isAscii c && (isAlphaNum c || c `elem` ("._/-" :: String))
    validateField name bound value
      | T.null (T.strip value) = Left (name <> " must not be blank")
      | BS.length (TE.encodeUtf8 value) > bound = Left (name <> " exceeds its UTF-8 byte limit")
      | T.any (\c -> isControl c || generalCategory c == Format) value = Left (name <> " must not contain control or format characters")
      | otherwise = Right ()

instance FromJSON BotIdentity where
  parseJSON = withObject "BotIdentity" $ \fields -> do
    unless (all (`elem` ["role", "model-id"]) (KM.keys fields)) $
      fail "BotIdentity accepts only role and model-id; actor IDs are server-derived"
    role <- fields .: "role"
    model <- fields .: "model-id"
    either (fail . T.unpack) pure (mkBotIdentity role model)

instance ToJSON BotIdentity where
  toJSON identity = object ["role" .= botRole identity, "model-id" .= botModelId identity]

-- | JSON array encoding is an unambiguous, versioned UTF-8 tuple. Identical
-- declarations intentionally share a local persona across sessions/workspaces.
localDeclaredBotId :: BotIdentity -> Text
localDeclaredBotId identity = "local-declared-bot:v1:" <> T.pack (show digest)
  where
    canonical = encode (["hmem-local-declared-bot", "v1", botRole identity, botModelId identity] :: [Text])
    digest = hash (LBS.toStrict canonical) :: Digest SHA256
