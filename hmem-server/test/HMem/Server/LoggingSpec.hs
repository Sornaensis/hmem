module HMem.Server.LoggingSpec (spec) where

import Control.Concurrent (threadDelay, yield)
import Control.Concurrent.Async (concurrently_, forConcurrently_)
import Control.Exception (ErrorCall(..), AsyncException(ThreadKilled), SomeException, evaluate, throwIO, try)
import Control.Monad (forM_, forever, unless, void, when)
import Data.Aeson (Value(..), decodeStrict)
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as BB
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Internal qualified as BSI
import Data.ByteString.Lazy qualified as LBS
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef, writeIORef)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID
import Data.Word (Word8, Word64)
import Foreign.Concurrent qualified as FC
import Foreign.Marshal.Alloc (free, mallocBytes)
import Foreign.Marshal.Utils (fillBytes)
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Types
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp (defaultSettings, setOnException, withApplicationSettings)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.Wai.Internal (Response(..), ResponseReceived(..))
import Network.Wai.Test (SResponse(..), runSession)
import Network.Wai.Test qualified as WaiTest
import Network.WebSockets qualified as WS
import System.Log.FastLogger (fromLogStr)
import System.Mem (performMajorGC)
import System.Timeout (timeout)
import Test.Hspec

import HMem.AgentSession.Transport (maxSessionRequestBytes)
import HMem.DB.Auth qualified as Auth
import HMem.DB.TestHarness (TestEnv(..), createTestWorkspace)
import HMem.Server.AuthTokens (IssuedAccessToken(..))
import HMem.Server.Logging (jsonRequestLogger, jsonRequestLoggerWith, requestRouteLabel)
import HMem.Server.TestHarness
import HMem.Types qualified as Types

spec :: Spec
spec = describe "metadata-only JSON request logger" $ do
  it "uses fixed route, method and request-ID labels without identifiers" $ do
    let workspace = "6f1c2c0e-3c38-4a8e-9a71-0c7f3c1a1b2c"
    requestRouteLabel ["api","v1","observations","count"] `shouldBe` "/api/v1/observations/count"
    requestRouteLabel ["api","v1","observations",workspace] `shouldBe` "/api/v1/observations/{observationId}"
    requestRouteLabel ["api","v1","workspaces",workspace,"sessions",""] `shouldBe` "/api/v1/workspaces/{workspaceId}/sessions"
    requestRouteLabel ["api","v1","ws"] `shouldBe` "/api/v1/ws"
    requestRouteLabel ["document-grants",workspace,"original","0"] `shouldBe` "/document-grants/{grantId}/{variant}/{page}"
    requestRouteLabel ["api","v1","no-such-route",workspace] `shouldBe` "unmatched"
    requestRouteLabel ["assets", "secret-looking-name.js"] `shouldBe` "static"
    (entriesOf, logger) <- capture
    let longId = BS.replicate 129 0x61
    forM_ [("BREW", Just "short-id"), ("GET", Just longId), ("GET", Just "has space")] $ \(method, rid) -> do
      let req = (plainRequest method ("/api/v1/observations/" <> TE.encodeUtf8 workspace) "?q=QUERY-SECRET")
            { Wai.requestHeaders = [("X-Request-Id", value) | Just value <- [rid]] }
      void $ logger (\_ respond -> respond (Wai.responseLBS status200 [] "RESPONSE-SECRET")) req drive
    [brew, long, spaced] <- entriesOf
    text "method" brew `shouldBe` Just "OTHER"
    text "request_id" brew `shouldBe` Just "short-id"
    KM.member "request_id" long `shouldBe` False
    KM.member "request_id" spaced `shouldBe` False
    forM_ [brew, long, spaced] $ \entry -> do
      text "route" entry `shouldBe` Just "/api/v1/observations/{observationId}"
      KM.member "path" entry `shouldBe` False
    raw <- rawOutput entriesOf
    forM_ ["QUERY-SECRET", "RESPONSE-SECRET", TE.encodeUtf8 workspace, longId] $ \secret ->
      BS.isInfixOf secret raw `shouldBe` False

  it "measures admission-to-preparation separately from monotonic stream completion" $ do
    now <- newIORef (1000000 :: Word64)
    sink <- newIORef []
    let logger = jsonRequestLoggerWith (readIORef now) (\entry -> modifyIORef' sink (<> [fromLogStr entry]))
        app _ respond = do
          writeIORef now 5000000
          respond $ Wai.responseStream status200 [] $ \send flush -> do
            send "first" >> flush
            writeIORef now 9000000
            send "second"
            writeIORef now 13500000
    void $ logger app (plainRequest "GET" "/api/v1/health" "") drive
    -- A regressing clock source never produces a negative duration.
    backwards <- newIORef (10000000 :: Word64)
    let regressing = atomicModifyIORef' backwards (\t -> (t - 1000000, t))
    void $ jsonRequestLoggerWith regressing (\entry -> modifyIORef' sink (<> [fromLogStr entry]))
      (\_ respond -> respond (Wai.responseLBS status200 [] "")) (plainRequest "GET" "/api/v1/health" "") drive
    [first, regressed] <- map decodeObject <$> readIORef sink
    number "prepare_ms" first `shouldBe` Just 4
    number "complete_ms" first `shouldBe` Just 12.5
    text "response" first `shouldBe` Just "stream"
    text "outcome" first `shouldBe` Just "completed"
    number "status" first `shouldBe` Just 200
    forM_ ["prepare_ms", "complete_ms"] $ \key -> number key regressed `shouldBe` Just 0

  it "reports truthful dispositions for application, response, stream and transmission failures" $ do
    (entriesOf, logger) <- capture
    let request = plainRequest "GET" "/api/v1/health" ""
        expectFailure action = (try action :: IO (Either SomeException ResponseReceived)) >>= either (const (pure ())) (const (expectationFailure "failure was swallowed"))
        failingSend = \case
          ResponseStream _ _ body -> body (\_ -> throwIO (userError "peer reset")) (pure ()) >> pure ResponseReceived
          ResponseBuilder{} -> throwIO (userError "peer reset")
          other -> drive other
    expectFailure $ logger (\_ _ -> throwIO (ErrorCall "handler failed")) request drive
    expectFailure $ logger (\_ respond -> respond (Wai.responseLBS status200 [] "x")) request failingSend
    expectFailure $ logger (\_ respond -> respond (Wai.responseLBS status200 [] (LBS.fromChunks ["ok", error "lazy body failed"]))) request drive
    expectFailure $ logger (\_ respond -> respond (Wai.responseStream status200 [] $ \send _ -> send "chunk" >> throwIO (ErrorCall "producer failed"))) request drive
    expectFailure $ logger (\_ respond -> respond (Wai.responseStream status206 [] $ \send _ -> send "chunk")) request failingSend
    expectFailure $ logger (\_ _ -> throwIO ThreadKilled) request drive
    -- A completed response is reported once even if the handler fails later.
    expectFailure $ logger (\_ respond -> respond (Wai.responseLBS status201 [] "") >> throwIO (ErrorCall "late failure")) request drive
    entries <- entriesOf
    map (\entry -> (text "outcome" entry, text "response" entry, number "status" entry, text "level" entry)) entries `shouldBe`
      [ (Just "application_failed", Nothing, Nothing, Just "warn")
      , (Just "transmission_failed", Just "buffered", Just 200, Just "warn")
      , (Just "response_failed", Just "buffered", Just 200, Just "warn")
      , (Just "stream_failed", Just "stream", Just 200, Just "warn")
      , (Just "transmission_failed", Just "stream", Just 206, Just "warn")
      , (Just "interrupted", Nothing, Nothing, Just "warn")
      , (Just "completed", Just "buffered", Just 201, Just "info")
      ]
    case entries of
      failed : _ -> KM.lookup "prepare_ms" failed `shouldBe` Just Null
      [] -> expectationFailure "no request log entries"

  it "attributes transport writes outside a stream producer, and swallowed write failures, to transmission" $ do
    (entriesOf, logger) <- capture
    let request = plainRequest "GET" "/api/v1/health" ""
        stream = Wai.responseStream status200 [] $ \send flush -> send "chunk" >> flush
        discard builder = void (evaluate (LBS.length (BB.toLazyByteString builder)))
        -- Like Warp, which writes the chunked terminator and final buffer
        -- after the producer returns, and the header before it starts.
        failsAfterBody = \case
          ResponseStream _ _ body -> body discard (pure ()) >> throwIO (userError "terminator write failed")
          other -> drive other
        failsBeforeBody = \case
          ResponseStream{} -> throwIO (userError "header write failed")
          other -> drive other
        failingSend = \case
          ResponseStream _ _ body -> body (\_ -> throwIO (userError "peer reset")) (pure ()) >> pure ResponseReceived
          other -> drive other
    afterBody <- try (logger (\_ respond -> respond stream) request failsAfterBody) :: IO (Either IOError ResponseReceived)
    either (const (pure ())) (const (expectationFailure "terminator failure was swallowed")) afterBody
    beforeBody <- try (logger (\_ respond -> respond stream) request failsBeforeBody) :: IO (Either IOError ResponseReceived)
    either (const (pure ())) (const (expectationFailure "header failure was swallowed")) beforeBody
    -- The producer hides the failed write and returns normally.
    void $ logger (\_ respond -> respond (Wai.responseStream status200 [] $ \send _ -> void (try (send "chunk") :: IO (Either IOError ())))) request failingSend
    entries <- entriesOf
    map (\entry -> (text "outcome" entry, text "response" entry, number "status" entry, text "level" entry)) entries `shouldBe`
      replicate 3 (Just "transmission_failed", Just "stream", Just 200, Just "warn")

  it "records raw upgrades distinctly from HTTP responses and their fallback" $ do
    (entriesOf, logger) <- capture
    let upgrade = Wai.responseRaw (\_ _ -> pure ()) (Wai.responseLBS status500 [] "fallback")
        noRaw = \case
          ResponseRaw _ fallback -> drive fallback
          other -> drive other
    void $ logger (\_ respond -> respond upgrade) (plainRequest "GET" "/api/v1/ws" "?ticket=T") drive
    void $ logger (\_ respond -> respond upgrade) (plainRequest "GET" "/api/v1/ws" "?ticket=T") noRaw
    failed <- try (logger (\_ respond -> respond (Wai.responseRaw (\_ _ -> throwIO (ErrorCall "socket handler failed")) (Wai.responseLBS status500 [] "")))
      (plainRequest "GET" "/api/v1/ws" "") drive) :: IO (Either ErrorCall ResponseReceived)
    either (const (pure ())) (const (expectationFailure "upgrade failure was swallowed")) failed
    [raw, fallback, broken] <- entriesOf
    (text "response" broken, KM.lookup "status" broken, text "outcome" broken) `shouldBe` (Just "upgrade", Just Null, Just "upgrade_failed")
    (text "response" raw, KM.lookup "status" raw, text "outcome" raw) `shouldBe` (Just "upgrade", Just Null, Just "upgrade_closed")
    (text "response" fallback, number "status" fallback, text "outcome" fallback) `shouldBe` (Just "upgrade", Just 500, Just "upgrade_fallback")

  it "keeps existing Warp HTTP and WebSocket behavior and logs closed and failed upgrades" $ do
    (entriesOf, logger) <- capture
    let server pendingConnection
          | "fail" `BS.isInfixOf` WS.requestPath (WS.pendingRequest pendingConnection) = throwIO (ErrorCall "upgrade handler failed")
          | otherwise = do
              conn <- WS.acceptRequest pendingConnection
              WS.sendTextData conn ("ready" :: Text)
              void (try (forever (WS.receiveDataMessage conn)) :: IO (Either WS.ConnectionException ()))
        http _ respond = respond (Wai.responseLBS status200 [] "HTTP-BODY-SECRET")
        app = logger (websocketsOr WS.defaultConnectionOptions server http)
    withApplicationSettings (setOnException (\_ _ -> pure ()) defaultSettings) (pure app) $ \port -> do
      manager <- HTTP.newManager HTTP.defaultManagerSettings
      plain <- HTTP.parseRequest ("http://127.0.0.1:" <> show port <> "/api/v1/health?token=QUERY-SECRET")
      response <- HTTP.httpLbs plain manager
      HTTP.responseBody response `shouldBe` "HTTP-BODY-SECRET"
      greeting <- WS.runClient "127.0.0.1" port "/api/v1/ws?ticket=QUERY-SECRET" $ \conn -> do
        message <- WS.receiveData conn
        WS.sendClose conn ("bye" :: Text)
        void (try (forever (WS.receiveDataMessage conn)) :: IO (Either WS.ConnectionException ()))
        pure (message :: Text)
      greeting `shouldBe` "ready"
      failed <- try (WS.runClient "127.0.0.1" port "/api/v1/ws?fail=QUERY-SECRET" WS.receiveDataMessage) :: IO (Either SomeException WS.DataMessage)
      either (const (pure ())) (const (expectationFailure "failed upgrade unexpectedly accepted")) failed
      entries <- awaitEntries entriesOf 3
      let summary entry = (text "route" entry, text "response" entry, KM.lookup "status" entry, text "outcome" entry)
      map summary entries `shouldMatchList`
        [ (Just "/api/v1/health", Just "buffered", Just (Number 200), Just "completed")
        , (Just "/api/v1/ws", Just "upgrade", Just Null, Just "upgrade_closed")
        , (Just "/api/v1/ws", Just "upgrade", Just Null, Just "upgrade_failed")
        ]
      raw <- rawOutput entriesOf
      forM_ ["QUERY-SECRET", "HTTP-BODY-SECRET", "ready"] $ \secret -> BS.isInfixOf secret raw `shouldBe` False

  it "performs no body reads before downstream authorization and the existing body limit, including rejected requests" $ do
    (entriesOf, logger) <- capture
    withDeployedSandboxAppLogged logger mkDeployedSandboxConfig $ \ctx -> do
      workspace <- createTestWorkspace ctx.deployedEnv "request-logger-body" :: IO Types.Workspace
      reader <- audience ctx workspace.id Auth.WorkspaceRoleRead
      editor <- audience ctx workspace.id Auth.WorkspaceRoleEdit
      let chunkSize = 65536
          chunks = 128
          target = "/api/v1/workspaces/" <> TE.encodeUtf8 (UUID.toText workspace.id) <> "/sessions"
          post headers = do
            bodyReads <- newIORef (0 :: Int)
            let body = do
                  n <- atomicModifyIORef' bodyReads (\count -> (count + 1, count + 1))
                  pure $ if n > chunks then BS.empty
                    else if n == 1 then "BODY-SECRET-MARKER" <> BS.replicate (chunkSize - 18) 0x41
                    else BS.replicate chunkSize 0x41
                request = Wai.setRequestBodyChunks body (plainRequest "POST" target "?token=QUERY-SECRET")
                  { Wai.requestHeaders = ("Content-Type", "application/json") : headers
                  , Wai.requestBodyLength = Wai.ChunkedBody
                  }
            response <- runSession (WaiTest.request request) ctx.deployedApplication
            count <- readIORef bodyReads
            pure (response.simpleStatus, count)
      (unauthenticated, unauthenticatedReads) <- post []
      unauthenticated `shouldBe` status401
      unauthenticatedReads `shouldBe` 0
      (denied, deniedReads) <- post reader
      denied `shouldBe` status403
      deniedReads `shouldBe` 0
      (tooLarge, limitedReads) <- post editor
      tooLarge `shouldBe` status413
      limitedReads `shouldSatisfy` (\n -> n >= 1 && n <= maxSessionRequestBytes `div` chunkSize + 1)
      entries <- entriesOf
      map (\entry -> (text "route" entry, text "method" entry, number "status" entry, text "outcome" entry)) entries `shouldBe`
        [ (Just "/api/v1/workspaces/{workspaceId}/sessions", Just "POST", Just status, Just "completed") | status <- [401, 403, 413] ]
      forM_ entries $ \entry -> text "request_id" entry `shouldSatisfy` maybe False (not . T.null)
      raw <- rawOutput entriesOf
      let secrets = ["BODY-SECRET-MARKER", "QUERY-SECRET", "Bearer", TE.encodeUtf8 (UUID.toText workspace.id)] <> concatMap (map snd) [reader, editor]
      forM_ secrets $ \secret -> BS.isInfixOf secret raw `shouldBe` False

  it "forwards matched concurrent upload and stream chunks without accumulating builders or bytes" $ do
    (entriesOf, logger) <- capture
    let flows = 8
        chunks = 32
        chunkSize = 262144
    forConcurrently_ [1 .. flows] $ \flow -> do
      workspace <- UUID.nextRandom
      session <- UUID.nextRandom
      let base = "/api/v1/workspaces/" <> TE.encodeUtf8 (UUID.toText workspace) <> "/sessions/" <> TE.encodeUtf8 (UUID.toText session)
      -- Each flow runs one upload and one stream through the shared logger at
      -- the same time as every other flow.
      concurrently_ (uploadFlow logger flow chunks chunkSize base) (streamFlow logger flow chunks chunkSize base session)
    entries <- entriesOf
    length entries `shouldBe` 2 * flows
    map (\entry -> (text "route" entry, number "status" entry, text "outcome" entry)) entries `shouldMatchList`
      concat (replicate flows
        [ (Just "/api/v1/workspaces/{workspaceId}/sessions/{sessionId}/append", Just 204, Just "completed")
        , (Just "/api/v1/workspaces/{workspaceId}/sessions/{sessionId}/archive/{generationId}/content", Just 200, Just "completed")
        ])

------------------------------------------------------------------------
-- Fixtures
------------------------------------------------------------------------

-- Upload: the downstream application sees each source chunk as it is
-- produced, and every previously read chunk becomes collectable.
uploadFlow :: Wai.Middleware -> Int -> Int -> Int -> BS.ByteString -> IO ()
uploadFlow logger flow chunks chunkSize base = do
  produced <- newIORef (0 :: Int)
  uploadCollected <- newIORef (0 :: Int)
  let source = do
        n <- atomicModifyIORef' produced (\count -> (count + 1, count + 1))
        if n > chunks then pure BS.empty else trackedChunk uploadCollected chunkSize (fromIntegral flow)
      upload = Wai.setRequestBodyChunks source (plainRequest "POST" (base <> "/append") "")
      consume req respond = do
        let loop index = do
              chunk <- Wai.getRequestBodyChunk req
              unless (BS.null chunk) $ do
                readSoFar <- readIORef produced
                when (readSoFar /= index) $ throwIO (ErrorCall ("logger read ahead of the application: " <> show readSoFar))
                _ <- evaluate (BS.length chunk)
                awaitCollected uploadCollected (index - 2)
                loop (index + 1)
        loop 1
        respond (Wai.responseLBS status204 [] "")
  void $ logger consume upload drive
  readIORef produced >>= (`shouldBe` chunks + 1)
  awaitCollected uploadCollected (chunks - 1)

-- Stream: each chunk reaches the transport before the next exists and
-- earlier chunks are released while the stream is still active.
streamFlow :: Wai.Middleware -> Int -> Int -> Int -> BS.ByteString -> UUID.UUID -> IO ()
streamFlow logger flow chunks chunkSize base session = do
  forwarded <- newIORef (0 :: Int)
  streamCollected <- newIORef (0 :: Int)
  let transport = \case
        ResponseStream _ _ body -> do
          body (\builder -> do
                  _ <- evaluate (LBS.length (BB.toLazyByteString builder))
                  atomicModifyIORef' forwarded (\count -> (count + 1, ())))
               (pure ())
          pure ResponseReceived
        other -> drive other
      stream _ respond = respond $ Wai.responseStream status200 [] $ \send flush -> do
        forM_ [1 .. chunks] $ \index -> do
          sent <- readIORef forwarded
          when (sent /= index - 1) $ throwIO (ErrorCall ("logger delayed a chunk: " <> show sent))
          awaitCollected streamCollected (index - 3)
          chunk <- trackedChunk streamCollected chunkSize (fromIntegral flow)
          send (BB.byteString chunk)
          flush
  void $ logger stream (plainRequest "GET" (base <> "/archive/" <> TE.encodeUtf8 (UUID.toText session) <> "/content") "") transport
  readIORef forwarded >>= (`shouldBe` chunks)
  awaitCollected streamCollected (chunks - 1)

capture :: IO (IO [KM.KeyMap Value], Wai.Middleware)
capture = do
  sink <- newIORef []
  logger <- jsonRequestLogger (\entry -> atomicModifyIORef' sink (\entries -> (entries <> [fromLogStr entry], ())))
  pure (map decodeObject <$> readIORef sink, logger)

rawOutput :: IO [KM.KeyMap Value] -> IO BS.ByteString
rawOutput entriesOf = BS8.pack . show <$> entriesOf

awaitEntries :: IO [KM.KeyMap Value] -> Int -> IO [KM.KeyMap Value]
awaitEntries entriesOf count = do
  found <- timeout 5000000 loop
  maybe (expectationFailure "request log entries were not emitted" >> pure []) pure found
  where
    loop = do
      entries <- entriesOf
      if length entries >= count then pure entries else threadDelay 10000 >> loop

decodeObject :: BS.ByteString -> KM.KeyMap Value
decodeObject line = case decodeStrict line of
  Just (Object entry) -> entry
  _ -> error ("request log line is not one JSON object: " <> show line)

text :: KM.Key -> KM.KeyMap Value -> Maybe Text
text key entry = case KM.lookup key entry of
  Just (String value) -> Just value
  _ -> Nothing

number :: KM.Key -> KM.KeyMap Value -> Maybe Double
number key entry = case KM.lookup key entry of
  Just (Number value) -> Just (realToFrac value)
  _ -> Nothing

plainRequest :: Method -> BS.ByteString -> BS.ByteString -> Wai.Request
plainRequest method path query = Wai.defaultRequest
  { Wai.requestMethod = method
  , Wai.rawPathInfo = path
  , Wai.pathInfo = drop 1 (T.splitOn "/" (TE.decodeUtf8 path))
  , Wai.rawQueryString = query
  , Wai.queryString = parseQuery query
  }

-- A minimal transport that runs every response shape and discards bytes.
drive :: Wai.Response -> IO ResponseReceived
drive = \case
  ResponseBuilder _ _ builder -> evaluate (LBS.length (BB.toLazyByteString builder)) >> pure ResponseReceived
  ResponseFile{} -> pure ResponseReceived
  ResponseStream _ _ body -> body (\builder -> void (evaluate (LBS.length (BB.toLazyByteString builder)))) (pure ()) >> pure ResponseReceived
  ResponseRaw raw _ -> raw (pure BS.empty) (\_ -> pure ()) >> pure ResponseReceived

-- Pinned bytes whose finalizer records collection, so retention by the
-- logger is observable without RTS statistics.
trackedChunk :: IORef Int -> Int -> Word8 -> IO BS.ByteString
trackedChunk collected size byte = do
  ptr <- mallocBytes size
  fillBytes ptr byte size
  pointer <- FC.newForeignPtr ptr (free ptr >> atomicModifyIORef' collected (\count -> (count + 1, ())))
  pure (BSI.BS pointer size)

awaitCollected :: IORef Int -> Int -> IO ()
awaitCollected collected target = when (target > 0) (go (400 :: Int))
  where
    go remaining = do
      count <- readIORef collected
      unless (count >= target) $
        if remaining == 0
          then throwIO (ErrorCall ("retained chunks: collected " <> show count <> " of " <> show target))
          else performMajorGC >> yield >> threadDelay 5000 >> go (remaining - 1)

audience :: DeployedSandboxApp -> UUID.UUID -> Auth.WorkspaceRole -> IO [Header]
audience ctx workspace role = do
  user <- createDeployedSandboxUser ctx.deployedEnv False False
  _ <- Auth.upsertWorkspaceMembership ctx.deployedEnv.pool workspace (Auth.UpsertWorkspaceMembership user role) Nothing
  token <- issueDeployedSandboxPAT ctx.deployedEnv user "request-logger"
  pure
    [ ("X-HMem-Bot-Identity", "{\"role\":\"Request Logger Test\",\"model-id\":\"test:fixture:default\"}")
    , ("Authorization", "Bearer " <> TE.encodeUtf8 token.rawToken)
    ]
