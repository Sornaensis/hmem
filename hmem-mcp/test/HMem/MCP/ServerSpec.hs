module HMem.MCP.ServerSpec (spec) where

import Control.Concurrent.MVar (newMVar)
import Control.Exception (bracket)
import Control.Concurrent.STM (TVar, newTVarIO)
import Data.Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.Foldable (toList)
import Data.Text (Text)
import Data.UUID (UUID)
import Network.HTTP.Client (Manager, defaultManagerSettings, newManager)
import System.Directory (removeFile)
import System.IO (Handle, SeekMode(..), hClose, hSeek, openTempFile)
import Test.Hspec

import HMem.MCP.Server (encodeStdioResponse, handleStdioLine, sendResponseToHandle)

spec :: Spec
spec = do
  describe "stdio JSON-RPC line handling" $ do
    it "returns parse errors for invalid JSON lines" $
      withStdioState $ \mgr initialized wsContext -> do
        mResponse <- handleStdioLine mgr unusedServerUrl Nothing initialized wsContext "not json"
        (mResponse >>= jsonField "id") `shouldBe` Just Null
        (mResponse >>= errorField "code") `shouldBe` Just (Number (-32700))
        (mResponse >>= errorField "message") `shouldBe` Just (String "Parse error")

    it "returns invalid-request errors for valid JSON without a method and preserves id" $
      withStdioState $ \mgr initialized wsContext -> do
        mResponse <- handleStdioLine mgr unusedServerUrl Nothing initialized wsContext $ jsonLine $ object
          [ "jsonrpc" .= ("2.0" :: Text)
          , "id" .= ("missing-method" :: Text)
          ]
        (mResponse >>= jsonField "id") `shouldBe` Just (String "missing-method")
        (mResponse >>= errorField "code") `shouldBe` Just (Number (-32600))
        (mResponse >>= errorField "message") `shouldBe` Just (String "Invalid Request: missing required 'method' field")

    it "does not emit responses for blank lines or notifications" $
      withStdioState $ \mgr initialized wsContext -> do
        blank <- handleStdioLine mgr unusedServerUrl Nothing initialized wsContext BS8.empty
        blank `shouldBe` Nothing
        notification <- handleStdioLine mgr unusedServerUrl Nothing initialized wsContext $ jsonLine $ object
          [ "jsonrpc" .= ("2.0" :: Text)
          , "method" .= ("notifications/progress" :: Text)
          ]
        notification `shouldBe` Nothing

    it "enforces initialization before ordinary requests" $
      withStdioState $ \mgr initialized wsContext -> do
        mResponse <- handleStdioLine mgr unusedServerUrl Nothing initialized wsContext $ jsonLine $ object
          [ "jsonrpc" .= ("2.0" :: Text)
          , "id" .= ("tools-before-init" :: Text)
          , "method" .= ("tools/list" :: Text)
          ]
        (mResponse >>= jsonField "id") `shouldBe` Just (String "tools-before-init")
        (mResponse >>= errorField "code") `shouldBe` Just (Number (-32002))
        (mResponse >>= errorField "message") `shouldBe` Just (String "Server not initialized")

    it "decodes initialized stdio request lines and dispatches JSON-RPC methods" $
      withStdioState $ \mgr initialized wsContext -> do
        initResponse <- handleStdioLine mgr unusedServerUrl Nothing initialized wsContext $ jsonLine $ object
          [ "jsonrpc" .= ("2.0" :: Text)
          , "id" .= ("init" :: Text)
          , "method" .= ("initialize" :: Text)
          ]
        (initResponse >>= jsonField "id") `shouldBe` Just (String "init")
        (initResponse >>= jsonField "result" >>= jsonField "serverInfo" >>= jsonField "name")
          `shouldBe` Just (String "hmem-mcp")

        toolsResponse <- handleStdioLine mgr unusedServerUrl Nothing initialized wsContext $ jsonLine $ object
          [ "jsonrpc" .= ("2.0" :: Text)
          , "id" .= ("tools" :: Text)
          , "method" .= ("tools/list" :: Text)
          ]
        (toolsResponse >>= jsonField "id") `shouldBe` Just (String "tools")
        case toolsResponse >>= jsonField "result" >>= jsonField "tools" of
          Just (Array tools) -> length (toList tools) `shouldSatisfy` (> 0)
          other -> expectationFailure $ "Expected tools/list array from stdio line, got: " <> show other

    it "writes newline-delimited JSON responses to handles" $ do
      let response = object
            [ "jsonrpc" .= ("2.0" :: Text)
            , "id" .= ("handle-test" :: Text)
            , "result" .= object ["ok" .= True]
            ]
      bytes <- withTempResponseFile $ \handle -> do
        lock <- newMVar ()
        sendResponseToHandle handle lock response
        hSeek handle AbsoluteSeek 0
        bytes <- BL.hGetContents handle
        BL.length bytes `seq` pure bytes
      bytes `shouldBe` encodeStdioResponse response
      BL8.last bytes `shouldBe` '\n'

unusedServerUrl :: String
unusedServerUrl = "http://127.0.0.1:9"

withStdioState :: (Manager -> TVar Bool -> TVar (Maybe UUID) -> IO a) -> IO a
withStdioState action = do
  mgr <- newManager defaultManagerSettings
  initialized <- newTVarIO False
  wsContext <- newTVarIO Nothing
  action mgr initialized wsContext

withTempResponseFile :: (Handle -> IO a) -> IO a
withTempResponseFile action =
  bracket
    (openTempFile "." "mcp-response.jsonl")
    (\(path, handle) -> hClose handle >> removeFile path)
    (\(_, handle) -> action handle)

jsonLine :: Value -> BS8.ByteString
jsonLine = BL.toStrict . encode

jsonField :: Text -> Value -> Maybe Value
jsonField field (Object o) = KM.lookup (Key.fromText field) o
jsonField _ _ = Nothing

errorField :: Text -> Value -> Maybe Value
errorField field value = jsonField "error" value >>= jsonField field
