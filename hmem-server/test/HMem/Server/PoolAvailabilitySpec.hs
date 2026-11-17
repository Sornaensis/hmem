module HMem.Server.PoolAvailabilitySpec (spec) where

import Control.Concurrent.Async (mapConcurrently, withAsync)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (throwIO)
import Data.Aeson (Value(..), decode)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.UUID qualified as UUID
import Network.HTTP.Types (Header, methodGet, parseQuery, statusCode)
import Network.Wai (Application, defaultRequest)
import Network.Wai qualified as Wai
import Network.Wai.Test qualified as WaiTest
import Servant (ServerError(..), runHandler)
import Test.Hspec

import HMem.Server.AuthTokens (IssuedAccessToken(..))
import HMem.DB.Pool (DBException(..), withConn)
import HMem.DB.TestHarness (TestEnv(..), createTestWorkspace)
import HMem.Server.API (handleDBErrors, handleSessionErrors)
import HMem.Server.TestHarness (DeployedSandboxApp(..), createDeployedSandboxUser, issueDeployedSandboxPAT, withDeployedSandboxAppPools, withDeployedSandboxAppPoolsTimeouts)
import HMem.Types (Workspace(..))

spec :: Spec
spec = describe "main pool availability without HTTP admission" $ do
  it "maps a pool checkout timeout to 503 database_unavailable with Retry-After: 5 on generic and session routes" $ do
    generic <- runHandler (handleDBErrors (throwIO DBPoolCheckoutTimeout :: IO ()))
    session <- runHandler (handleSessionErrors (throwIO DBPoolCheckoutTimeout :: IO ()))
    let shape = either (\failure -> Just (failure.errHTTPCode, lookup "Retry-After" failure.errHeaders, errorCode failure.errBody)) (const Nothing)
    shape generic `shouldBe` Just (503, Just "5", Just "database_unavailable")
    shape session `shouldBe` Just (503, Just "5", Just "database_unavailable")

  it "answers 503 database_unavailable over HTTP when no connection frees within the checkout timeout" $
    withDeployedSandboxAppPoolsTimeouts 1 Nothing 30000 1000 $ \ctx -> do
      workspace <- createTestWorkspace ctx.deployedEnv "pool-checkout-timeout"
      user <- createDeployedSandboxUser ctx.deployedEnv False False
      token <- issueDeployedSandboxPAT ctx.deployedEnv user "Pool timeout"
      holding <- newEmptyMVar
      release <- newEmptyMVar
      withAsync (withConn ctx.deployedEnv.pool $ \_ -> putMVar holding () >> takeMVar release) $ \_ -> do
        takeMVar holding
        let get path = apiGet ctx.deployedApplication path (authHeaders token.rawToken)
        generic <- get "/api/v1/workspaces"
        session <- get ("/api/v1/workspaces/" <> TE.encodeUtf8 (UUID.toText workspace.id) <> "/sessions")
        putMVar release ()
        mapM_ (\response -> do
          statusCode (WaiTest.simpleStatus response) `shouldBe` 503
          lookup "Retry-After" (WaiTest.simpleHeaders response) `shouldBe` Just "5"
          errorCode (WaiTest.simpleBody response) `shouldBe` Just "database_unavailable") [generic, session]

  it "serves 64 concurrent requests on a two-connection pool and reports no HTTP admission" $
    withDeployedSandboxAppPools 2 Nothing $ \ctx -> do
      user <- createDeployedSandboxUser ctx.deployedEnv False False
      token <- issueDeployedSandboxPAT ctx.deployedEnv user "Uncapped requests"
      responses <- mapConcurrently (const (apiGet ctx.deployedApplication "/api/v1/workspaces" (authHeaders token.rawToken))) [1 .. 64 :: Int]
      map (statusCode . WaiTest.simpleStatus) responses `shouldBe` replicate 64 200
      health <- apiGet ctx.deployedApplication "/api/v1/health" []
      case decode (WaiTest.simpleBody health) of
        Just (Object fields) -> do
          KeyMap.member "http_admission" fields `shouldBe` False
          KeyMap.member "pool" fields `shouldBe` True
        other -> expectationFailure ("unexpected health body: " <> show other)
  where
    errorCode :: LBS.ByteString -> Maybe Text
    errorCode body = case decode body of
      Just (Object fields) | Just (String code) <- KeyMap.lookup (Key.fromText "error") fields -> Just code
      _ -> Nothing

authHeaders :: Text -> [Header]
authHeaders token =
  [ ("Authorization", "Bearer " <> TE.encodeUtf8 token)
  , ("X-HMem-Bot-Identity", "{\"role\":\"Pool Test Bot\",\"model-id\":\"test:fixture:default\"}") ]

apiGet :: Application -> BS.ByteString -> [Header] -> IO WaiTest.SResponse
apiGet app path headers = WaiTest.runSession (WaiTest.srequest (WaiTest.SRequest request "")) app
  where
    (rawPath, rawQuery) = BS.break (== 63) path
    request = defaultRequest
      { Wai.requestMethod = methodGet
      , Wai.rawPathInfo = rawPath
      , Wai.rawQueryString = rawQuery
      , Wai.queryString = parseQuery rawQuery
      , Wai.pathInfo = filter (not . T.null) (T.splitOn "/" (TE.decodeUtf8 rawPath))
      , Wai.requestHeaders = headers
      }
