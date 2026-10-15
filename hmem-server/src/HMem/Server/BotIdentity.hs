-- | HTTP and WebSocket upgrades share one declaration boundary. This metadata
-- never changes the authenticated principal or a ticket/resume audience.
module HMem.Server.BotIdentity (requestBotIdentity) where

import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text qualified as T
import Network.HTTP.Types (Header)
import HMem.BotIdentity (BotIdentity)

requestBotIdentity :: [Header] -> Either Text (Maybe BotIdentity)
requestBotIdentity headers = case [value | (name, value) <- headers, name == "X-HMem-Bot-Identity"] of
  [] -> Right Nothing
  [raw]
    | BS.length raw > 4096 -> Left "X-HMem-Bot-Identity exceeds 4096 bytes"
    | otherwise -> case Aeson.eitherDecodeStrict' raw of
        Left message -> Left (T.pack message)
        Right identity -> Right (Just identity)
  _ -> Left "X-HMem-Bot-Identity must occur exactly once"
