module HMem.Server.OidcCallbackSpec (spec) where

import Control.Concurrent.Async (concurrently)
import Control.Monad (forM_, replicateM)
import Crypto.Hash (Digest, SHA256, hash)
import Crypto.JOSE.JWK qualified as JWK
import Crypto.JOSE.JWS qualified as JWS
import Crypto.JWT qualified as JWT
import Data.Aeson (Value(..), fromJSON, object, Result(..), toJSON, (.=))
import Data.Aeson.Key (Key)
import Data.Aeson.KeyMap qualified as KM
import Data.ByteArray.Encoding (Base(..), convertToBase)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Time (UTCTime, addUTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Network.HTTP.Types (HeaderName, Method, methodGet, methodPost, parseQuery, statusCode)
import Network.HTTP.Types.URI (urlEncode)
import Network.Wai (Application, defaultRequest)
import Network.Wai qualified as Wai
import Network.Wai.Test (SRequest(..), SResponse(..), runSession, srequest)
import Test.Hspec

import HMem.Config qualified as Config
import HMem.DB.TestHarness (TestEnv(..), withTestEnv)
import HMem.Server.AccessTracker (newAccessTracker)
import HMem.Server.App (mkAppWithOidcCodeExchangeAndClock)
import HMem.Server.AuthTokens (IssuedAccessToken(..))
import HMem.Server.TestHarness
import HMem.Server.WebSocket (newWSState)

data CallbackApp = CallbackApp
  { application :: Application
  , idToken :: IORef (Maybe Text)
  , exchanged :: IORef [(Text, Text)]
  }

spec :: Spec
spec = describe "OIDC browser callback" $ do
  it "requires configured API JWT issuer and audience while keeping time claims optional" $
    withCallbackEnv $ \env -> do
      harness <- callbackApp env id getCurrentTime
      let claims = object
            [ "iss" .= deployedSandboxIssuer, "aud" .= deployedSandboxAudience
            , "sub" .= ("oidc-probe-user" :: Text)
            ]
          probes =
            [ ("valid without exp/iat", id, 200)
            , ("valid multiple resource audiences", replace "aud" (toJSON [deployedSandboxAudience, "another-resource"]), 200)
            , ("missing issuer", remove "iss", 401)
            , ("missing audience", remove "aud", 401)
            , ("wrong issuer", replace "iss" (String "https://untrusted.example"), 401)
            , ("wrong audience", replace "aud" (String "untrusted-client"), 401)
            ] :: [(String, Value -> Value, Int)]
      forM_ probes $ \(name, change, expected) -> do
        token <- signClaimsValue (change claims)
        response <- request harness.application methodGet "/api/v1/session" [("Authorization", "Bearer " <> encodeUtf8 token)]
        putStrLn ("API signed JWT probe: " <> show (name, status response))
        status response `shouldBe` expected
      patUser <- createDeployedSandboxUser env True False
      pat <- issueDeployedSandboxPAT env patUser "API JWT compatibility bot"
      patResponse <- request harness.application methodGet "/api/v1/session" [("Authorization", "Bearer " <> encodeUtf8 pat.rawToken)]
      status patResponse `shouldBe` 200

  it "binds S256 and the signed nonce to a one-use browser transaction" $
    withCallbackEnv $ \env -> do
      harness <- callbackApp env id getCurrentTime
      login <- beginLogin harness "/audit?view=recent" []
      status login `shouldBe` 302
      let location = headerValue "Location" login
          nonce = queryValue "nonce" location
          binding = cookieValue "hmem_session_oidc_binding" login
      queryValue "code_challenge_method" location `shouldBe` "S256"
      BS.length nonce `shouldBe` 43
      binding `shouldSatisfy` maybe False ((== 43) . BS.length)
      forM_ (cookieHeaders login) $ \value -> do
        value `shouldSatisfy` BS.isInfixOf "HttpOnly"
        value `shouldSatisfy` BS.isInfixOf "Secure"
        value `shouldSatisfy` BS.isInfixOf "Max-Age=300"
      setToken harness login id
      response <- finishLogin harness login []
      status response `shouldBe` 302
      headerValue "Location" response `shouldBe` "/audit?view=recent"
      calls <- readIORef harness.exchanged
      case calls of
        [(code, verifier)] -> do
          code `shouldBe` "test-code"
          T.length verifier `shouldBe` 43
          verifier `shouldSatisfy` T.all (\c -> c `elem` (['a'..'z'] <> ['A'..'Z'] <> ['0'..'9'] <> "-_"))
          let expected = convertToBase Base64URLUnpadded (hash (encodeUtf8 verifier) :: Digest SHA256)
          queryValue "code_challenge" location `shouldBe` expected
          encodeUtf8 verifier `shouldNotBe` nonce
          Just (encodeUtf8 verifier) `shouldNotBe` binding
          location `shouldSatisfy` (not . BS.isInfixOf (encodeUtf8 verifier))
          cookieHeaders login `shouldSatisfy` all (not . BS.isInfixOf (encodeUtf8 verifier))
        other -> expectationFailure ("expected one code exchange, got " <> show (length other))
      assertTransactionCleared response
      session <- request harness.application methodGet "/api/v1/session" [responseCookies response]
      status session `shouldBe` 200
      replay <- finishLogin harness login []
      status replay `shouldBe` 401
      length <$> readIORef harness.exchanged `shouldReturn` 1
      let csrf = maybe "" id (cookieValue "hmem_csrf" response)
      logout <- request harness.application methodPost "/api/v1/auth/logout"
        [responseCookies response, ("X-CSRF-Token", csrf)]
      status logout `shouldBe` 302
      assertTransactionCleared logout
      revoked <- request harness.application methodGet "/api/v1/session" [responseCookies response]
      status revoked `shouldBe` 401

  it "rejects missing, mixed and failed context without consuming another browser's login" $
    withCallbackEnv $ \env -> do
      harness <- callbackApp env id getCurrentTime
      first <- beginLogin harness "/" []
      second <- beginLogin harness "/" []
      setToken harness first id
      missing <- finishLogin harness first [("Cookie", cookieHeaderValue (filter ((/= "hmem_session_oidc_binding") . fst) (cookiePairs first)))]
      status missing `shouldBe` 401
      let mixed = cookieHeaderValue (replacePair "hmem_session_oidc_binding"
            (maybe "" id (cookieValue "hmem_session_oidc_binding" first)) (cookiePairs second))
      swapped <- finishLogin harness second [("Cookie", mixed)]
      status swapped `shouldBe` 401
      readIORef harness.exchanged `shouldReturn` []
      validFirst <- finishLogin harness first []
      status validFirst `shouldBe` 302
      setToken harness second id
      validSecond <- finishLogin harness second []
      status validSecond `shouldBe` 302
      failed <- beginLogin harness "/" []
      setToken harness failed (replace "nonce" (String "wrong-transaction"))
      wrongNonce <- finishLogin harness failed []
      status wrongNonce `shouldBe` 401
      assertTransactionCleared wrongNonce
      setToken harness failed id
      retry <- finishLogin harness failed []
      status retry `shouldBe` 401
      forM_ [remove "nonce", replace "nonce" Null, replace "nonce" (toJSON ["not-a-string" :: Text])] $ \change -> do
        pending <- beginLogin harness "/" []
        setToken harness pending change
        badNonce <- finishLogin harness pending []
        status badNonce `shouldBe` 401
        cookieValue "hmem_session" badNonce `shouldBe` Nothing
      badStateLogin <- beginLogin harness "/" []
      badState <- request harness.application methodGet "/api/v1/auth/callback?code=test-code&state=wrong" [responseCookies badStateLogin]
      status badState `shouldBe` 401
      assertTransactionCleared badState
      setToken harness badStateLogin id
      finishLogin harness badStateLogin [] >>= (\response -> status response `shouldBe` 401)
      pendingLogout <- beginLogin harness "/" []
      logout <- request harness.application methodPost "/api/v1/auth/logout" [responseCookies pendingLogout]
      status logout `shouldBe` 302
      setToken harness pendingLogout id
      finishLogin harness pendingLogout [] >>= (\response -> status response `shouldBe` 401)

  it "consumes a transaction atomically before concurrent provider exchanges" $
    withCallbackEnv $ \env -> do
      harness <- callbackApp env id getCurrentTime
      login <- beginLogin harness "/" []
      setToken harness login id
      (first, second) <- concurrently (finishLogin harness login []) (finishLogin harness login [])
      [status first, status second] `shouldMatchList` [302, 401]
      length <$> readIORef harness.exchanged `shouldReturn` 1

  it "bounds pending logins, retains live entries and prunes expired transactions" $
    withCallbackEnv $ \env -> do
      now <- getCurrentTime
      clock <- newIORef now
      harness <- callbackApp env id (readIORef clock)
      first <- beginLogin harness "/" []
      rest <- replicateM 1023 (beginLogin harness "/" [])
      all ((== 302) . status) rest `shouldBe` True
      blocked <- beginLogin harness "/" []
      status blocked `shouldBe` 503
      setToken harness first id
      accepted <- finishLogin harness first []
      status accepted `shouldBe` 302
      next <- beginLogin harness "/" []
      status next `shouldBe` 302
      writeIORef clock (addUTCTime 301 now)
      setToken harness next id
      expired <- finishLogin harness next []
      status expired `shouldBe` 401
      length <$> readIORef harness.exchanged `shouldReturn` 1
      fresh <- beginLogin harness "/" []
      status fresh `shouldBe` 302

  it "requires ID-token claims without tightening ordinary API JWTs" $
    withCallbackEnv $ \env -> do
      now <- realToFrac . utcTimeToPOSIXSeconds <$> getCurrentTime :: IO Double
      let invalid =
            [ ("missing issuer", remove "iss")
            , ("missing audience", remove "aud")
            , ("missing expiration", remove "exp")
            , ("missing issued-at", remove "iat")
            , ("missing subject", remove "sub")
            , ("empty subject", replace "sub" (String ""))
            , ("wrong issuer", replace "iss" (String "https://untrusted.example"))
            , ("wrong audience", replace "aud" (String "untrusted-client"))
            , ("extra untrusted audience", replace "aud" (toJSON [deployedSandboxAudience, "untrusted-client"]))
            , ("expired", replace "exp" (toJSON (now - 60)))
            , ("future issued-at", replace "iat" (toJSON (now + 3600)))
            , ("wrong authorized party", replace "azp" (String "untrusted-client"))
            , ("non-string authorized party", replace "azp" (Number 3))
            ] :: [(String, Value -> Value)]
      forM_ invalid $ \(name, change) -> do
        harness <- callbackApp env id getCurrentTime
        login <- beginLogin harness "/" []
        setToken harness login change
        response <- finishLogin harness login []
        status response `shouldBe` 401
        cookieValue "hmem_session" response `shouldBe` Nothing
        putStrLn ("OIDC rejected signed ID token: " <> name)
      forM_ [id, replace "azp" (String deployedSandboxAudience), replace "sub" (String "urn:example:user:qa")] $ \change -> do
        harness <- callbackApp env id getCurrentTime
        login <- beginLogin harness "/" []
        setToken harness login change
        finishLogin harness login [] >>= (\response -> status response `shouldBe` 302)
      harness <- callbackApp env id getCurrentTime
      apiToken <- signDeployedSandboxJwt "oidc-probe-user"
      apiSession <- request harness.application methodGet "/api/v1/session" [("Authorization", "Bearer " <> encodeUtf8 apiToken)]
      status apiSession `shouldBe` 200

  it "keeps Strict session cookies while allowing cross-site GET transaction cookies" $
    withCallbackEnv $ \env -> do
      harness <- callbackApp env (\cfg -> cfg { Config.auth = cfg.auth
        { Config.deployed = cfg.auth.deployed { Config.cookieSameSite = "Strict" } } }) getCurrentTime
      login <- beginLogin harness "/audit" []
      forM_ (cookieHeaders login) $ \value -> do
        value `shouldSatisfy` BS.isInfixOf "SameSite=Lax"
        value `shouldSatisfy` BS.isInfixOf "Secure"
        value `shouldSatisfy` BS.isInfixOf "HttpOnly"
      setToken harness login id
      response <- finishLogin harness login []
      status response `shouldBe` 302
      forM_ ["hmem_session", "hmem_csrf"] $ \name -> do
        let headers = [value | value <- cookieHeaders response, (name <> "=") `BS.isPrefixOf` value]
        headers `shouldSatisfy` (not . null)
        headers `shouldSatisfy` all (BS.isInfixOf "SameSite=Strict")
        headers `shouldSatisfy` all (BS.isInfixOf "Secure")

  it "keeps return targets local while preserving proxy prefixes and encoded queries" $
    withCallbackEnv $ \env -> do
      forM_ [("/\\example.invalid", "/"), ("//example.invalid", "/"), ("/\r\nLocation: https://example.invalid", "/"), ("https://example.invalid", "/"), ("/audit", "/audit"), ("/hmem/?query=caf%C3%A9%20%26%20code", "/hmem/?query=caf%C3%A9%20%26%20code")] $ \(target, expected) -> do
        harness <- callbackApp env id getCurrentTime
        login <- beginLogin harness target []
        setToken harness login id
        let cookies = cookieHeaderValue (cookiePairs login <> [("hmem_session_return_to", "%2F%5Cexample.invalid")])
        response <- finishLogin harness login [("Cookie", cookies)]
        status response `shouldBe` 302
        headerValue "Location" response `shouldBe` expected

withCallbackEnv :: (TestEnv -> IO a) -> IO a
withCallbackEnv action = withTestEnv $ \env -> do
  _ <- createDeployedSandboxUserWithSubject env "oidc-probe-user" True False
  _ <- createDeployedSandboxUserWithSubject env "urn:example:user:qa" True False
  action env

callbackApp :: TestEnv -> (Config.HMemConfig -> Config.HMemConfig) -> IO UTCTime -> IO CallbackApp
callbackApp env configure clock = do
  tokenRef <- newIORef Nothing
  calls <- newIORef []
  tracker <- newAccessTracker env.pool 3600
  ws <- newWSState
  let cfg = configure (mkDeployedSandboxConfig env)
      limits = cfg.rateLimit { Config.rlEnabled = False }
      exchange _ code verifier = do
        atomicModifyIORef' calls (\values -> ((code, verifier) : values, ()))
        readIORef tokenRef
  app <- mkAppWithOidcCodeExchangeAndClock clock exchange id cfg.auth cfg.cors limits env.pool tracker ws Nothing False
  pure (CallbackApp app tokenRef calls)

beginLogin :: CallbackApp -> BS.ByteString -> [(HeaderName, BS.ByteString)] -> IO SResponse
beginLogin harness target = request harness.application methodGet ("/api/v1/auth/login?return_to=" <> urlEncode True target)

finishLogin :: CallbackApp -> SResponse -> [(HeaderName, BS.ByteString)] -> IO SResponse
finishLogin harness login overrides = request harness.application methodGet
  ("/api/v1/auth/callback?code=test-code&state=" <> queryValue "state" (headerValue "Location" login))
  (if null overrides then [responseCookies login] else overrides)

setToken :: CallbackApp -> SResponse -> (Value -> Value) -> IO ()
setToken harness login change = do
  now <- realToFrac . utcTimeToPOSIXSeconds <$> getCurrentTime :: IO Double
  let claims = object
        [ "iss" .= deployedSandboxIssuer, "aud" .= deployedSandboxAudience
        , "sub" .= ("oidc-probe-user" :: Text), "exp" .= (now + 3600), "iat" .= (now - 1)
        , "nonce" .= decodeUtf8 (queryValue "nonce" (headerValue "Location" login))
        ]
  token <- signClaimsValue (change claims)
  writeIORef harness.idToken (Just token)

signClaimsValue :: Value -> IO Text
signClaimsValue value = do
  claims <- case fromJSON value of Success parsed -> pure parsed; Error err -> fail err
  let key = JWK.fromOctets (encodeUtf8 deployedSandboxJwtSecret)
  signed <- JWT.runJOSE (do
    alg <- JWK.bestJWSAlg key
    JWT.signClaims key (JWS.newJWSHeader ((), alg)) claims) :: IO (Either JWT.JWTError JWT.SignedJWT)
  either (fail . show) (pure . decodeUtf8 . LBS.toStrict . JWT.encodeCompact) signed

request :: Application -> Method -> BS.ByteString -> [(HeaderName, BS.ByteString)] -> IO SResponse
request app method fullPath headers = runSession (srequest (SRequest req "")) app
  where
    (path, query) = BS.break (== 63) fullPath
    req = defaultRequest
      { Wai.requestMethod = method, Wai.rawPathInfo = path
      , Wai.pathInfo = filter (not . T.null) (T.splitOn "/" (decodeUtf8 path))
      , Wai.rawQueryString = query, Wai.queryString = parseQuery query
      , Wai.requestHeaders = headers
      }

status :: SResponse -> Int
status = statusCode . (.simpleStatus)

headerValue :: HeaderName -> SResponse -> BS.ByteString
headerValue name response = maybe "" id (lookup name response.simpleHeaders)

queryValue :: BS.ByteString -> BS.ByteString -> BS.ByteString
queryValue name url = maybe "" id (lookup name (parseQuery (snd (BS.break (== 63) url))) >>= id)

cookieHeaders :: SResponse -> [BS.ByteString]
cookieHeaders response = [value | (name, value) <- response.simpleHeaders, name == "Set-Cookie"]

cookiePairs :: SResponse -> [(BS.ByteString, BS.ByteString)]
cookiePairs = map (\value -> let (name, suffix) = BS.break (== 61) (BS.takeWhile (/= 59) value) in (name, BS.drop 1 suffix)) . cookieHeaders

cookieValue :: BS.ByteString -> SResponse -> Maybe BS.ByteString
cookieValue name = lookup name . cookiePairs

cookieHeaderValue :: [(BS.ByteString, BS.ByteString)] -> BS.ByteString
cookieHeaderValue = BS.intercalate "; " . map (\(name, value) -> name <> "=" <> value)

responseCookies :: SResponse -> (HeaderName, BS.ByteString)
responseCookies response = ("Cookie", cookieHeaderValue (cookiePairs response))

replacePair :: BS.ByteString -> BS.ByteString -> [(BS.ByteString, BS.ByteString)] -> [(BS.ByteString, BS.ByteString)]
replacePair name value pairs = (name, value) : filter ((/= name) . fst) pairs

assertTransactionCleared :: SResponse -> Expectation
assertTransactionCleared response = forM_ ["hmem_session_oidc_state", "hmem_session_oidc_binding", "hmem_session_return_to"] $ \name ->
  cookieHeaders response `shouldSatisfy` any (\value -> (name <> "=") `BS.isPrefixOf` value && "Max-Age=0" `BS.isInfixOf` value)

remove :: Key -> Value -> Value
remove key (Object values) = Object (KM.delete key values)
remove _ value = value

replace :: Key -> Value -> Value -> Value
replace key value (Object values) = Object (KM.insert key value values)
replace _ _ value = value
