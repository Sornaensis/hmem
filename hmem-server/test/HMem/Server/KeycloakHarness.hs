module HMem.Server.KeycloakHarness
  ( KeycloakHarness(..)
  , keycloakEnvFlag
  , withSandboxedKeycloak
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, bracket, onException, toException, try)
import Control.Monad (forM_, unless, when)
import Data.Aeson (FromJSON(..), Value, eitherDecode, encode, object, withObject, (.:), (.:?), (.=))
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.UUID.V4 qualified as UUIDv4
import Network.HTTP.Client
  ( Manager
  , Request(..)
  , RequestBody(..)
  , Response
  , httpLbs
  , method
  , newManager
  , parseRequest
  , requestBody
  , requestHeaders
  , responseBody
  , responseHeaders
  , responseStatus
  , urlEncodedBody
  )
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (HeaderName, hAuthorization, hContentType, hLocation, statusCode)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.Process (readCreateProcessWithExitCode, proc)
import Text.Read (readMaybe)

import HMem.DB.TestHarness (TestDb(..), TestEnv(..), TestSandbox(..), assertInSandbox)

data KeycloakHarness = KeycloakHarness
  { keycloakBaseUrl :: !Text
  , keycloakRealm :: !Text
  , keycloakClientId :: !Text
  , keycloakIssuer :: !Text
  , keycloakAuthorizationEndpoint :: !Text
  , keycloakTokenEndpoint :: !Text
  , keycloakJwks :: !Value
  , keycloakUsername :: !Text
  , keycloakPassword :: !Text
  , keycloakUserSubject :: !Text
  , keycloakIdToken :: !Text
  , keycloakContainerName :: !String
  , keycloakLogFile :: !FilePath
  } deriving stock (Show, Eq)

data TokenResponse = TokenResponse
  { trAccessToken :: !Text
  , trIdToken :: !(Maybe Text)
  } deriving stock (Show, Eq)

instance FromJSON TokenResponse where
  parseJSON = withObject "TokenResponse" $ \o -> TokenResponse
    <$> o .: "access_token"
    <*> o .:? "id_token"

keycloakEnvFlag :: String
keycloakEnvFlag = "HMEM_TEST_KEYCLOAK"

withSandboxedKeycloak :: TestEnv -> (KeycloakHarness -> IO a) -> IO a
withSandboxedKeycloak env action = do
  enabled <- keycloakEnabled
  unless enabled $ fail $ "withSandboxedKeycloak requires " <> keycloakEnvFlag <> "=1"
  withClearedAmbientKeycloakEnv $
    bracket (startKeycloak env) stopKeycloak action

startKeycloak :: TestEnv -> IO KeycloakHarness
startKeycloak env = do
  let TestEnv { testSandbox = sandbox } = env
      TestSandbox { sandboxLogDir = logDir, sandboxConfigDir = configDir } = sandbox
  when env.testDb.testDbUnsafeExternal $
    fail "Refusing to run sandboxed Keycloak harness with unsafe external test database"
  suffix <- T.take 8 . T.filter (/= '-') . T.pack . show <$> UUIDv4.nextRandom
  let containerName = "hmem-keycloak-" <> T.unpack suffix
      realm = "hmem-sandbox-" <> suffix
      clientId = "hmem-sandbox-client"
      username = "sandbox-user"
      password = "sandbox-password"
      adminUser = "sandbox-admin"
      adminPassword = "sandbox-admin-password"
      logFile = logDir </> "keycloak-" <> T.unpack suffix <> ".log"
      configFile = configDir </> "keycloak-" <> T.unpack suffix <> ".json"
  assertInSandbox sandbox logFile
  assertInSandbox sandbox configFile
  image <- fromMaybe "quay.io/keycloak/keycloak:26.0" <$> lookupEnv "HMEM_TEST_KEYCLOAK_IMAGE"

  _ <- runProcOrFail "start keycloak container" "docker"
    [ "run"
    , "--rm"
    , "-d"
    , "--name", containerName
    , "-p", "127.0.0.1::8080"
    , "-e", "KEYCLOAK_ADMIN=" <> T.unpack adminUser
    , "-e", "KEYCLOAK_ADMIN_PASSWORD=" <> T.unpack adminPassword
    , image
    , "start-dev"
    , "--http-enabled=true"
    , "--hostname-strict=false"
    , "--hostname-strict-https=false"
    ]

  let cleanupOnSetupFailure = do
        appendKeycloakLogs containerName logFile
        _ <- try (runProc "cleanup failed keycloak container" "docker" ["rm", "-f", containerName]) :: IO (Either SomeException (String, String))
        pure ()

  (do
    port <- waitForMappedPort containerName
    let baseUrl = "http://127.0.0.1:" <> T.pack (show port)
        issuer = baseUrl <> "/realms/" <> realm
        authEndpoint = issuer <> "/protocol/openid-connect/auth"
        tokenEndpoint = issuer <> "/protocol/openid-connect/token"

    writeJson configFile $ object
      [ "container" .= containerName
      , "image" .= image
      , "base_url" .= baseUrl
      , "realm" .= realm
      , "client_id" .= clientId
      , "log_file" .= logFile
      ]

    manager <- newManager tlsManagerSettings
    adminToken <- waitForAdminToken manager baseUrl adminUser adminPassword
    createRealm manager baseUrl adminToken realm
    createClient manager baseUrl adminToken realm clientId
    userSubject <- createUser manager baseUrl adminToken realm username password
    idToken <- issueUserIdToken manager tokenEndpoint clientId username password
    jwks <- fetchJwks manager issuer
    appendKeycloakLogs containerName logFile

    pure KeycloakHarness
      { keycloakBaseUrl = baseUrl
      , keycloakRealm = realm
      , keycloakClientId = clientId
      , keycloakIssuer = issuer
      , keycloakAuthorizationEndpoint = authEndpoint
      , keycloakTokenEndpoint = tokenEndpoint
      , keycloakJwks = jwks
      , keycloakUsername = username
      , keycloakPassword = password
      , keycloakUserSubject = userSubject
      , keycloakIdToken = idToken
      , keycloakContainerName = containerName
      , keycloakLogFile = logFile
      }) `onException` cleanupOnSetupFailure

stopKeycloak :: KeycloakHarness -> IO ()
stopKeycloak kc = do
  appendKeycloakLogs kc.keycloakContainerName kc.keycloakLogFile
  _ <- try (runProc "stop keycloak container" "docker" ["rm", "-f", kc.keycloakContainerName]) :: IO (Either SomeException (String, String))
  pure ()

waitForAdminToken :: Manager -> Text -> Text -> Text -> IO Text
waitForAdminToken manager baseUrl adminUser adminPassword = go (90 :: Int)
  where
    tokenUrl = baseUrl <> "/realms/master/protocol/openid-connect/token"
    go attempts
      | attempts <= 0 = fail $ "Timed out waiting for sandbox Keycloak admin token at " <> T.unpack tokenUrl
      | otherwise = do
          result <- try $ formToken manager tokenUrl
            [ ("grant_type", "password")
            , ("client_id", "admin-cli")
            , ("username", adminUser)
            , ("password", adminPassword)
            ]
          case result of
            Right token -> pure token.trAccessToken
            Left (_ :: SomeException) -> do
              sleepMillis 1000
              go (attempts - 1)

createRealm :: Manager -> Text -> Text -> Text -> IO ()
createRealm manager baseUrl token realm = do
  let body = object
        [ "realm" .= realm
        , "enabled" .= True
        ]
  _ <- jsonRequest manager "POST" (baseUrl <> "/admin/realms") (Just token) body [201]
  pure ()

createClient :: Manager -> Text -> Text -> Text -> Text -> IO ()
createClient manager baseUrl token realm clientId = do
  let body = object
        [ "clientId" .= clientId
        , "enabled" .= True
        , "publicClient" .= True
        , "directAccessGrantsEnabled" .= True
        , "standardFlowEnabled" .= True
        , "redirectUris" .= ["http://127.0.0.1/*" :: Text]
        , "webOrigins" .= ["http://127.0.0.1" :: Text]
        ]
  _ <- jsonRequest manager "POST" (baseUrl <> "/admin/realms/" <> realm <> "/clients") (Just token) body [201]
  pure ()

createUser :: Manager -> Text -> Text -> Text -> Text -> Text -> IO Text
createUser manager baseUrl token realm username password = do
  let body = object
        [ "username" .= username
        , "enabled" .= True
        , "emailVerified" .= True
        , "email" .= (username <> "@example.invalid")
        , "firstName" .= ("Sandbox" :: Text)
        , "lastName" .= ("User" :: Text)
        , "credentials" .=
            [ object
                [ "type" .= ("password" :: Text)
                , "value" .= password
                , "temporary" .= False
                ]
            ]
        ]
  response <- jsonRequest manager "POST" (baseUrl <> "/admin/realms/" <> realm <> "/users") (Just token) body [201]
  case lookup hLocation (responseHeaders response) >>= locationSubject of
    Just subject -> pure subject
    Nothing -> fail "Keycloak user creation did not return a Location user id"

issueUserIdToken :: Manager -> Text -> Text -> Text -> Text -> IO Text
issueUserIdToken manager tokenEndpoint clientId username password = do
  token <- formToken manager tokenEndpoint
    [ ("grant_type", "password")
    , ("client_id", clientId)
    , ("username", username)
    , ("password", password)
    , ("scope", "openid profile email")
    ]
  case token.trIdToken of
    Just idToken -> pure idToken
    Nothing -> fail "Keycloak token response did not include an id_token"

fetchJwks :: Manager -> Text -> IO Value
fetchJwks manager issuer = do
  req <- parseRequest (T.unpack issuer <> "/protocol/openid-connect/certs")
  response <- httpLbs req manager
  expectStatus "fetch realm JWKS" [200] response
  decodeBody "realm JWKS" (responseBody response)

jsonRequest :: Manager -> Text -> Text -> Maybe Text -> Value -> [Int] -> IO (Response LBS.ByteString)
jsonRequest manager methodText url mBearer body expected = do
  req0 <- parseRequest (T.unpack url)
  let req = req0
        { method = TE.encodeUtf8 methodText
        , requestBody = RequestBodyLBS (encode body)
        , requestHeaders = [(hContentType, "application/json")] <> authHeader mBearer
        }
  response <- httpLbs req manager
  expectStatus (T.unpack methodText <> " " <> T.unpack url) expected response
  pure response

formToken :: Manager -> Text -> [(Text, Text)] -> IO TokenResponse
formToken manager url form = do
  req0 <- parseRequest (T.unpack url)
  let req = urlEncodedBody [(TE.encodeUtf8 k, TE.encodeUtf8 v) | (k, v) <- form]
        req0 { method = "POST" }
  response <- httpLbs req manager
  expectStatus ("token request " <> T.unpack url) [200] response
  decodeBody "token response" (responseBody response)

expectStatus :: String -> [Int] -> Response body -> IO ()
expectStatus label expected response = do
  let actual = statusCode (responseStatus response)
  unless (actual `elem` expected) $
    fail $ label <> " returned HTTP " <> show actual <> ", expected " <> show expected

decodeBody :: FromJSON a => String -> LBS.ByteString -> IO a
decodeBody label body = case eitherDecode body of
  Right value -> pure value
  Left err -> fail $ "Failed to decode " <> label <> ": " <> err <> "; body=" <> show body

authHeader :: Maybe Text -> [(HeaderName, ByteString)]
authHeader Nothing = []
authHeader (Just token) = [(hAuthorization, "Bearer " <> TE.encodeUtf8 token)]

keycloakEnabled :: IO Bool
keycloakEnabled = do
  value <- lookupEnv keycloakEnvFlag
  pure $ value `elem` [Just "1", Just "true", Just "TRUE", Just "yes", Just "YES"]

locationSubject :: ByteString -> Maybe Text
locationSubject raw = do
  let text = TE.decodeUtf8Lenient raw
  listToMaybe . reverse $ T.splitOn "/" text

appendKeycloakLogs :: String -> FilePath -> IO ()
appendKeycloakLogs containerName logFile = do
  result <- try $ do
    (stdoutText, stderrText) <- runProc "keycloak logs" "docker" ["logs", containerName]
    LBS.writeFile logFile $ LBS.fromStrict $ TE.encodeUtf8 $
      T.pack stdoutText <> "\n" <> T.pack stderrText
  case result of
    Left (_ :: SomeException) -> pure ()
    Right () -> pure ()

runProcOrFail :: String -> String -> [String] -> IO (String, String)
runProcOrFail label executable args = do
  result <- try (readCreateProcessWithExitCode (proc executable args) "") :: IO (Either SomeException (ExitCode, String, String))
  case result of
    Left err -> fail $ label <> " failed to execute " <> executable <> ": " <> show err <> "\ncommand: " <> unwords (executable : args)
    Right (exitCode, stdoutText, stderrText) -> case exitCode of
      ExitSuccess -> pure (stdoutText, stderrText)
      _ -> fail $ label <> " failed: " <> show exitCode <> "\ncommand: " <> unwords (executable : args) <> "\nstdout:\n" <> stdoutText <> "\nstderr:\n" <> stderrText

runProc :: String -> String -> [String] -> IO (String, String)
runProc = runProcOrFail

waitForMappedPort :: String -> IO Int
waitForMappedPort containerName = go (30 :: Int)
  where
    go attempts
      | attempts <= 0 = fail $ "Timed out waiting for Docker to publish sandbox Keycloak port for " <> containerName
      | otherwise = do
          result <- try $ runProc "inspect keycloak mapped port" "docker" ["port", containerName, "8080/tcp"]
          case result >>= maybe (Left $ toExceptionText "Docker did not report a mapped Keycloak port") Right . parseMappedPort . fst of
            Right port -> pure port
            Left (_ :: SomeException) -> do
              sleepMillis 250
              go (attempts - 1)

parseMappedPort :: String -> Maybe Int
parseMappedPort raw = do
  line <- listToMaybe $ filter (not . null) $ lines raw
  portText <- listToMaybe $ reverse $ splitOn ':' line
  readMaybe portText

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn needle = go []
  where
    go acc [] = [reverse acc]
    go acc (x:xs)
      | x == needle = reverse acc : go [] xs
      | otherwise = go (x:acc) xs

toExceptionText :: String -> SomeException
toExceptionText = toException . userError

sleepMillis :: Int -> IO ()
sleepMillis ms = threadDelay (ms * 1000)

writeJson :: FilePath -> Value -> IO ()
writeJson path = LBS.writeFile path . encode

withClearedAmbientKeycloakEnv :: IO a -> IO a
withClearedAmbientKeycloakEnv = bracket snapshot restore . const
  where
    snapshot = do
      values <- mapM (\name -> do value <- lookupEnv name; pure (name, value)) ambientKeycloakVars
      forM_ ambientKeycloakVars unsetEnv
      pure values
    restore values = forM_ values $ \case
      (name, Nothing) -> unsetEnv name
      (name, Just value) -> setEnv name value

ambientKeycloakVars :: [String]
ambientKeycloakVars =
  [ "KEYCLOAK_URL"
  , "KEYCLOAK_REALM"
  , "KEYCLOAK_CLIENT_SECRET"
  , "KEYCLOAK_ADMIN"
  , "KEYCLOAK_ADMIN_PASSWORD"
  , "KC_BOOTSTRAP_ADMIN_USERNAME"
  , "KC_BOOTSTRAP_ADMIN_PASSWORD"
  ]
