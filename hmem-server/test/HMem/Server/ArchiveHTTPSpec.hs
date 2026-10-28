module HMem.Server.ArchiveHTTPSpec (spec) where

import Control.Concurrent.Async (concurrently, withAsync, cancel, waitCatch)
import Control.Concurrent.MVar
import Control.Exception (IOException, SomeException, throwIO, try)
import Control.Monad (forM_, unless)
import Crypto.Hash (Context, Digest, SHA256, hashInit, hashUpdate, hashFinalize)
import Data.Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as B8
import Data.ByteString.Lazy qualified as LBS
import Data.IORef
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time (getCurrentTime)
import Data.UUID (UUID)
import Data.UUID.V4 qualified as UUID
import Hasql.Session qualified as SQL
import Network.HTTP.Types
import Network.HTTP.Types.Header (hIfNoneMatch, hRetryAfter, hETag)
import Network.Wai qualified as Wai
import Network.Wai.Internal (ResponseReceived(..))
import Network.Wai.Test (SRequest(..), SResponse(..), runSession, srequest)
import System.Timeout (timeout)
import Test.Hspec

import HMem.AgentSession qualified as S
import HMem.Config qualified as Config
import HMem.DB.AgentSession qualified as Sessions
import HMem.DB.Auth qualified as Auth
import HMem.DB.Pool qualified as Pool
import HMem.DB.RequestContext
import HMem.DB.SessionArchive qualified as DB
import HMem.DB.SessionArchive.Reader qualified as Reader
import HMem.DB.Task qualified as Task
import HMem.DB.TestHarness (TestEnv(..), createTestWorkspace)
import HMem.Server.Archive.HTTP
import HMem.Server.Archive.Worker (compressArchive, verifyAndPublishArchive)
import HMem.Server.AuthTokens (IssuedAccessToken(..))
import HMem.Server.TestHarness
import HMem.SessionArchive.Codec qualified as Codec
import HMem.Types qualified as Types

spec :: Spec
spec = describe "Archive HTTP" $ do
  it "schedules once, exposes honest progress, and transfers the exact complete persisted single-member gzip" $
    withLocalSandboxAppContext $ \ctx -> bounded $ do
      let env=ctx.localEnv; app=ctx.localApplication; bytes="{\"text\":\"\195\166\"}\r\n[]"
      (w,sealed) <- fixture env bytes
      let path=archivePath w sealed.id
      pending <- get app path []
      simpleStatus pending `shouldBe` status202
      body <- decoded pending
      value ["state"] body `shouldBe` Just (String "preparing")
      value ["raw_bytes"] body `shouldBe` Just (String $ decimal sealed.committedBytes)
      value ["generation_id"] body `shouldBe` Just Null
      lookup hRetryAfter pending.simpleHeaders `shouldBe` Just "1"
      Just initial <- DB.getArchiveJob env.pool w sealed.id
      _ <- get app path []
      DB.getArchiveJob env.pool w sealed.id `shouldReturn` Just initial
      manifest <- publish env w sealed.id
      ready <- get app path []
      simpleStatus ready `shouldBe` status200
      document <- decoded ready
      value ["generation_id"] document `shouldBe` Just (toJSON manifest.id)
      value ["compressed_bytes"] document `shouldBe` Just (String $ decimal manifest.compressedBytes)
      let content=contentPath manifest
      response <- get app content []
      simpleStatus response `shouldBe` status200
      persisted <- collect env manifest
      LBS.toStrict response.simpleBody `shouldBe` persisted
      lookup hContentType response.simpleHeaders `shouldBe` Just "application/gzip"
      lookup hContentEncoding response.simpleHeaders `shouldBe` Nothing
      lookup hContentLength response.simpleHeaders `shouldBe` Just (B8.pack $ show manifest.compressedBytes)
      lookup "X-HMem-Archive-Generation" response.simpleHeaders `shouldBe` Just (uuid manifest.id)
      lookup "X-HMem-Archive-SHA256" response.simpleHeaders `shouldBe` Just (TE.encodeUtf8 $ S.sessionSha256 persisted)
      lookup "X-HMem-Archive-Raw-Bytes" response.simpleHeaders `shouldBe` Just (B8.pack $ show sealed.committedBytes)
      lookup hCacheControl response.simpleHeaders `shouldBe` Just "private, no-cache, no-transform"
      source <- chunksSource [persisted]
      result <- Codec.verify sealed.committedBytes source (const $ pure ()) (const $ pure ())
      result.raw.sha256 `shouldBe` S.sessionSha256 bytes
      result.compressed.sha256 `shouldBe` manifest.compressedSha256
      Just tag <- pure $ lookup hETag response.simpleHeaders
      cached <- get app content [(hIfNoneMatch,tag)]
      (cached.simpleStatus,cached.simpleBody) `shouldBe` (status304,"")
      foreignGeneration <- UUID.nextRandom
      get app (path<>"/"<>uuid foreignGeneration<>"/content") [(hIfNoneMatch,"*")] >>= hasStatus status404
      Sessions.getSession env.pool w sealed.id `shouldReturn` Just sealed
      openapi <- get app "/api/v1/openapi.json" [] >>= decoded
      forM_ [ "/api/v1/workspaces/{workspaceId}/sessions/{sessionId}/archive"
            , "/api/v1/workspaces/{workspaceId}/sessions/{sessionId}/archive/{generationId}/content"] $ \route ->
        value ["paths",route,"get"] openapi `shouldSatisfy` (/=Nothing)
      value ["components","schemas","SessionArchiveManifest","properties","raw_bytes","type"] openapi `shouldBe` Just (String "string")

  it "archives an empty receipt as exactly twenty gzip bytes and rejects active sessions" $
    withLocalSandboxAppContext $ \ctx -> bounded $ do
      let env=ctx.localEnv; app=ctx.localApplication
      (w,sealed) <- fixture env ""
      manifest <- publish env w sealed.id
      manifest.compressedBytes `shouldBe` 20
      response <- get app (contentPath manifest) []
      hasStatus status200 response
      LBS.length response.simpleBody `shouldBe` 20
      input <- chunksSource [LBS.toStrict response.simpleBody]
      verified <- Codec.verify 0 input (const $ pure ()) (const $ pure ())
      verified.raw.byteCount `shouldBe` 0
      active <- activeFixture env w
      get app (archivePath w active.id) [] >>= hasStatus status409
      unknown <- UUID.nextRandom
      get app (archivePath w active.id<>"/"<>uuid unknown<>"/content") [] >>= hasStatus status404
      DB.getArchiveJob env.pool w active.id `shouldReturn` Nothing

  it "reports durable retry and terminal failure without publishing attempt metadata or resetting failures" $
    withLocalSandboxAppContext $ \ctx -> bounded $ do
      let env=ctx.localEnv; app=ctx.localApplication
      (w,sealed) <- fixture env "{}\n"
      Reader.ensureArchive env.pool w sealed.id
      Just first <- DB.claimArchive env.pool "fixture"
      DB.archiveProgress env.pool first "compressing" 3 12 `shouldReturn` True
      preparing <- get app (archivePath w sealed.id) [] >>= decoded
      value ["progress","raw_bytes"] preparing `shouldBe` Just (String "3")
      DB.releaseArchive env.pool first False "fixture_retry" `shouldReturn` True
      retry <- get app (archivePath w sealed.id) []
      hasStatus status202 retry
      retryBody <- decoded retry
      value ["state"] retryBody `shouldBe` Just (String "retrying")
      value ["progress","raw_bytes"] retryBody `shouldBe` Just (String "0")
      value ["compressed_bytes"] retryBody `shouldBe` Just Null
      Pool.runSession env.pool $ SQL.sql ("UPDATE session_archive_jobs SET retry_at=clock_timestamp() WHERE session_id='"<>uuid sealed.id<>"'")
      Just second <- DB.claimArchive env.pool "fixture"
      second.generationId `shouldNotBe` first.generationId
      DB.releaseArchive env.pool second True "bounded_internal_failure" `shouldReturn` True
      failed <- get app (archivePath w sealed.id) []
      hasStatus status503 failed
      failedBody <- decoded failed
      value ["state"] failedBody `shouldBe` Just (String "failed")
      value ["retryable"] failedBody `shouldBe` Just (Bool False)
      value ["retry_after_ms"] failedBody `shouldBe` Just Null
      _ <- get app (archivePath w sealed.id) []
      Just job <- DB.getArchiveJob env.pool w sealed.id
      job.state `shouldBe` "failed"

  it "never exposes a staged generation while publication and readiness polling overlap" $
    withLocalSandboxAppContext $ \ctx -> bounded $ do
      let env=ctx.localEnv; app=ctx.localApplication
      (w,sealed) <- fixture env (BS.concat $ replicate 20000 "{\"row\":true}\n")
      Reader.ensureArchive env.pool w sealed.id
      Just lease <- DB.claimArchive env.pool "fixture"
      prepared <- compressArchive env.pool lease
      get app (archivePath w sealed.id<>"/"<>uuid lease.generationId<>"/content") [] >>= hasStatus status404
      (_,responses) <- concurrently
        (verifyAndPublishArchive env.pool lease prepared)
        (mapM (const $ get app (archivePath w sealed.id) []) [1..12 :: Int])
      forM_ responses $ \response -> do
        response.simpleStatus `shouldSatisfy` (`elem` [status200,status202])
        body <- decoded response
        case value ["state"] body of
          Just (String "ready") -> value ["generation_id"] body `shouldBe` Just (toJSON lease.generationId)
          _ -> value ["generation_id"] body `shouldBe` Just Null
      get app (archivePath w sealed.id) [] >>= hasStatus status200

  it "authorizes manifests, exact-generation conditionals and credential contexts before disclosing bytes" $
    withDeployedSandboxAppContext $ \ctx -> bounded $ do
      let env=ctx.deployedEnv; app=ctx.deployedApplication
      (w,sealed) <- fixture env "{}\n"
      manifest <- publish env w sealed.id
      user <- createDeployedSandboxUser env False False
      _ <- Auth.upsertWorkspaceMembership env.pool w (Auth.UpsertWorkspaceMembership user Auth.WorkspaceRoleRead) Nothing
      tokenA <- issueDeployedSandboxPAT env user "archive-a"
      tokenB <- issueDeployedSandboxPAT env user "archive-b"
      let headers token=[(hAuthorization,"Bearer "<>TE.encodeUtf8 token.rawToken)]
      response <- get app (contentPath manifest) (headers tokenA)
      hasStatus status200 response
      Just tag <- pure $ lookup hETag response.simpleHeaders
      get app (contentPath manifest) (headers tokenA<>[(hIfNoneMatch,tag)]) >>= hasStatus status304
      switched <- get app (contentPath manifest) (headers tokenB<>[(hIfNoneMatch,tag)])
      hasStatus status200 switched
      lookup hETag switched.simpleHeaders `shouldNotBe` Just tag
      browserSession <- createDeployedSandboxAuthSession env user "archive-cookie" "archive-csrf"
      let config=ctx.deployedConfig :: Config.HMemConfig
          cookie=[(hCookie,TE.encodeUtf8 (config.auth.deployed.sessionCookieName<>"=archive-cookie"))]
      browser <- get app (contentPath manifest) cookie
      hasStatus status200 browser
      Just browserTag <- pure $ lookup hETag browser.simpleHeaders
      Pool.runSession env.pool $ SQL.sql ("UPDATE auth_sessions SET revoked_at=clock_timestamp() WHERE id='"<>uuid browserSession<>"'")
      get app (contentPath manifest) (cookie<>[(hIfNoneMatch,browserTag)]) >>= hasStatus status401
      outsider <- createTestWorkspace env "archive-other"
      get app (archivePath outsider.id sealed.id) (headers tokenA) >>= hasStatus status403
      _ <- Auth.upsertWorkspaceMembership env.pool outsider.id (Auth.UpsertWorkspaceMembership user Auth.WorkspaceRoleRead) Nothing
      get app (archivePath outsider.id sealed.id) (headers tokenA) >>= hasStatus status404
      Pool.runSession env.pool $ SQL.sql ("UPDATE access_tokens SET revoked_at=clock_timestamp() WHERE id='"<>uuid tokenA.tokenId<>"'")
      get app (contentPath manifest) (headers tokenA<>[(hIfNoneMatch,tag)]) >>= hasStatus status401
      Pool.runSession env.pool $ SQL.sql ("DELETE FROM workspace_memberships WHERE workspace_id='"<>uuid w<>"' AND user_id='"<>uuid user<>"'")
      get app (archivePath w sealed.id) (headers tokenB) >>= hasStatus status403
      get app (contentPath manifest) (headers tokenB<>[(hIfNoneMatch,"*")]) >>= hasStatus status403

  it "returns archive_unavailable before successful headers when published payloads are corrupted" $
    withLocalSandboxAppContext $ \ctx -> bounded $ do
      let env=ctx.localEnv; app=ctx.localApplication
      (w,sealed) <- fixture env "{}\n"
      manifest <- publish env w sealed.id
      -- Privileged corruption fixture only; restore the ordinary guard in the
      -- same transaction. Production cannot mutate published parts.
      Pool.runTransaction env.pool $ SQL.sql
        ("ALTER TABLE session_archive_parts DISABLE TRIGGER session_archive_stage_guard; UPDATE session_archive_parts SET data=set_byte(data,0,get_byte(data,0)#1) WHERE generation_id='"<>uuid manifest.id<>"' AND ordinal=0; ALTER TABLE session_archive_parts ENABLE TRIGGER session_archive_stage_guard")
      failed <- get app (contentPath manifest) []
      hasStatus status503 failed
      lookup hContentType failed.simpleHeaders `shouldBe` Just "application/json"
      lookup hContentEncoding failed.simpleHeaders `shouldBe` Nothing
      failure <- decoded failed
      value ["error"] failure `shouldBe` Just (String "archive_unavailable")

  it "holds no pool lease during a write and aborts an in-flight HTTP response after authority withdrawal" $
    withLocalSandboxAppContext $ \ctx -> bounded $ do
      let env=ctx.localEnv
      (w,sealed) <- fixture env (BS.concat $ replicate 20000 "{\"row\":true}\n")
      manifest <- publish env w sealed.id
      manifest.partCount `shouldSatisfy` (>1)
      allowed <- newIORef True
      sent <- newIORef (0 :: Int)
      responseStatus <- newIORef Nothing
      activeLeases <- newIORef ([] :: [Int])
      let principal=Principal ActorUser "transport-fixture" "Fixture" PrincipalSyntheticLocalSuperadmin
          resolve _=readIORef allowed >>= \yes -> pure (if yes then Just principal else Nothing)
          fallback _ respond=respond $ Wai.responseLBS status404 [] ""
          app=archiveMiddleware env.pool resolve fallback
          path=contentPath manifest
          request=Wai.defaultRequest{Wai.requestMethod=methodGet,Wai.rawPathInfo=path
            ,Wai.pathInfo=filter (not . T.null) (T.splitOn "/" $ TE.decodeUtf8 path)}
          receive response=do
            let (status,_,withBody)=Wai.responseToStream response
            writeIORef responseStatus (Just status)
            withBody $ \body -> do
              body (\_ -> do
                metrics <- Pool.getPoolMetrics
                modifyIORef' activeLeases (metrics.activeConnections:)
                modifyIORef' sent (+1)
                writeIORef allowed False) (pure ())
            pure ResponseReceived
      attempted <- withPrincipalContext (Just principal) $ try @SomeException (app request receive)
      either (const True) (const False) attempted `shouldBe` True
      readIORef responseStatus `shouldReturn` Just status200
      readIORef sent `shouldReturn` 1
      readIORef activeLeases `shouldReturn` [0]

  it "rejects missing, truncated, reordered, modified and overlong part streams" $ bounded $ do
    (manifest,parts) <- streamFixture ["first","second"]
    let run source = streamArchiveParts manifest (\n -> pure $ source n) (pure ()) (const $ pure ())
        at xs n = if n < 0 || n >= fromIntegral (length xs) then Nothing else Just (xs !! fromIntegral n)
    run (at parts)
    forM_ [ take 1 parts, reverse parts
          , [head parts,changedBytes (parts !! 1) "secon"]
          , [head parts,changedBytes (parts !! 1) "xxxxxx"]
          , parts<>[last parts]
          ] $ \bad -> do
      result <- try @IOException (run $ at bad)
      result `shouldSatisfy` either (const True) (const False)

  it "rechecks authority after every bounded fetch and stops fetching immediately after send cancellation" $ bounded $ do
    (manifest,parts) <- streamFixture ["first","second"]
    checks <- newIORef (0 :: Int)
    sent <- newIORef (0 :: Int)
    let at n = pure $ if n < fromIntegral (length parts) then Just (parts !! fromIntegral n) else Nothing
        check = atomicModifyIORef' checks (\n -> (n+1,n)) >>= \n -> unless (n < 3) (throwIO $ userError "revoked")
    result <- try @IOException $ streamArchiveParts manifest at check (\_ -> modifyIORef' sent (+1))
    result `shouldSatisfy` either (const True) (const False)
    readIORef sent `shouldReturn` 1
    entered <- newEmptyMVar
    blocked <- newEmptyMVar
    fetched <- newIORef (0 :: Int)
    let fetch n = modifyIORef' fetched (+1) >> at n
    withAsync (streamArchiveParts manifest fetch (pure ()) (\_ -> putMVar entered () >> takeMVar blocked)) $ \transfer -> do
      timeout 1000000 (takeMVar entered) `shouldReturn` Just ()
      timeout 1000000 (cancel transfer) `shouldReturn` Just ()
      _ <- waitCatch transfer
      readIORef fetched `shouldReturn` 1

  it "uses distinct cache audiences for accounts, actors, bearer credentials and browser sessions" $ do
    user <- UUID.nextRandom
    other <- UUID.nextRandom
    let p=Principal ActorUser "account" "label" (PrincipalGrantUser user)
        audience=archiveAudience p (Just "Bearer A") Nothing
    archiveAudience p (Just "Bearer B") Nothing `shouldNotBe` audience
    archiveAudience p Nothing (Just "session=A") `shouldNotBe` archiveAudience p Nothing (Just "session=B")
    archiveAudience (Principal ActorUser "other" "label" (PrincipalGrantUser user)) (Just "Bearer A") Nothing `shouldNotBe` audience
    archiveAudience (Principal ActorUser "account" "label" (PrincipalGrantUser other)) (Just "Bearer A") Nothing `shouldNotBe` audience
    archiveAudience (Principal ActorUser "account" "new label" (PrincipalGrantUser user)) (Just "Bearer A") Nothing `shouldBe` audience

  it "streams a generated 1 GiB compressed-part shape with fixed buffers and backpressure" $ bounded $ do
    (base,_) <- streamFixture ["fixture"]
    let payload=BS.replicate 65536 97
        count=16384 :: Int64
        total=count*65536
        digest n context | n == count = T.pack (show (hashFinalize context :: Digest SHA256))
                         | otherwise = let next=hashUpdate context payload in next `seq` digest (n+1) next
        expected=digest 0 (hashInit :: Context SHA256)
        manifest=DB.ArchiveManifest base.id base.sessionId base.workspaceId base.attempt
          base.codec base.codecImplementation base.codecParameters base.rawBytes base.rawRecords base.rawRevision base.rawSha256
          total expected count base.checkpointCount base.ledgerCount base.metadataSha256 base.publishedAt
        partHash=S.sessionSha256 payload
        fetch n=pure $ if n >= count then Nothing else Just (DB.ArchivePart n (n*65536) ((n+1)*65536) payload partHash "verified")
    received <- newIORef (0 :: Int64)
    streamArchiveParts manifest fetch (pure ()) $ \bytes -> do
      BS.length bytes `shouldBe` 65536
      modifyIORef' received (+fromIntegral (BS.length bytes))
    readIORef received `shouldReturn` total

bounded :: IO () -> IO ()
bounded action = timeout (60*1000000) action >>= (`shouldBe` Just ())

fixture :: TestEnv -> ByteString -> IO (UUID,S.AgentSession)
fixture env bytes = do
  workspace <- createTestWorkspace env "archive-http"
  active <- activeFixture env workspace.id
  uploaded <- if BS.null bytes then pure active else (.session) <$> Sessions.appendSession env.pool workspace.id active.id (S.AppendSession 0 0 bytes True)
  sealed <- Sessions.terminateSession env.pool workspace.id active.id
    (S.TerminateSession uploaded.committedBytes uploaded.revision uploaded.committedBytes (S.sessionSha256 bytes))
  pure (workspace.id,sealed)

activeFixture :: TestEnv -> UUID -> IO S.AgentSession
activeFixture env w = do
  task <- Task.createTask env.pool (Types.CreateTask w Nothing Nothing "Archive HTTP" Nothing Nothing Nothing)
  Pool.runSession env.pool $ SQL.sql ("UPDATE tasks SET status='in_progress' WHERE id='"<>uuid task.id<>"'")
  identifier <- UUID.nextRandom
  Sessions.createSession env.pool (S.CreateSession identifier w "jsonl" "Archive HTTP" Nothing Nothing task.id)

publish :: TestEnv -> UUID -> UUID -> IO DB.ArchiveManifest
publish env w s = do
  Reader.ensureArchive env.pool w s
  implementation <- Codec.codecImplementation
  Just lease <- DB.claimArchive env.pool implementation
  prepared <- compressArchive env.pool lease
  verifyAndPublishArchive env.pool lease prepared
  Just manifest <- Reader.verifiedManifest env.pool w s
  pure manifest

collect :: TestEnv -> DB.ArchiveManifest -> IO ByteString
collect env manifest = BS.concat <$> mapM (\n -> do
  Just part <- DB.readArchivePart env.pool manifest.workspaceId manifest.sessionId manifest.id n
  pure part.bytes) [0..manifest.partCount-1]

chunksSource :: [ByteString] -> IO (IO (Maybe ByteString))
chunksSource chunks = do
  ref <- newIORef chunks
  pure $ atomicModifyIORef' ref $ \case [] -> ([],Nothing); x:xs -> (xs,Just x)

changedBytes :: DB.ArchivePart -> ByteString -> DB.ArchivePart
changedBytes part bytes = DB.ArchivePart part.ordinal part.startOffset part.endOffset bytes part.dataSha256 part.bindingSha256

streamFixture :: [ByteString] -> IO (DB.ArchiveManifest,[DB.ArchivePart])
streamFixture payloads = do
  w <- UUID.nextRandom; s <- UUID.nextRandom; g <- UUID.nextRandom; now <- getCurrentTime
  -- Include enough bytes to meet the minimum complete-gzip length; these unit
  -- cases exercise framing/integrity, while HTTP cases verify the real codec.
  let bytes=map (<>BS.replicate 20 32) payloads
      offsets=scanl (+) 0 (map (fromIntegral . BS.length) bytes)
      parts=[DB.ArchivePart n start end payload (S.sessionSha256 payload) "verified-binding" | (n,start,end,payload)<-zip4 [0..] offsets (tail offsets) bytes]
      full=BS.concat bytes
      m=DB.ArchiveManifest g s w 1 "gzip-v1" "fixture" Null 0 0 0 (S.sessionSha256 "")
          (fromIntegral $ BS.length full) (S.sessionSha256 full) (fromIntegral $ length parts) 1 0 "fixture" now
  pure (m,parts)
  where zip4 (a:as) (b:bs) (c:cs) (d:ds)=(a,b,c,d):zip4 as bs cs ds; zip4 _ _ _ _=[]

decimal :: Int64 -> Text
decimal = T.pack . show
uuid :: UUID -> ByteString
uuid = TE.encodeUtf8 . T.pack . show
archivePath :: UUID -> UUID -> ByteString
archivePath w s = "/api/v1/workspaces/"<>uuid w<>"/sessions/"<>uuid s<>"/archive"
contentPath :: DB.ArchiveManifest -> ByteString
contentPath m = archivePath m.workspaceId m.sessionId<>"/"<>uuid m.id<>"/content"
get :: Wai.Application -> ByteString -> [Header] -> IO SResponse
get app path headers = runSession (srequest $ SRequest request "") app
  where request=Wai.defaultRequest{Wai.requestMethod=methodGet,Wai.rawPathInfo=path
        ,Wai.pathInfo=filter (not . T.null) (T.splitOn "/" $ TE.decodeUtf8 path),Wai.requestHeaders=headers}
hasStatus :: Status -> SResponse -> IO ()
hasStatus status response = response.simpleStatus `shouldBe` status
decoded :: SResponse -> IO Value
decoded response = either fail pure (eitherDecode response.simpleBody)
value :: [Text] -> Value -> Maybe Value
value [] v=Just v
value (k:ks) (Object fields)=KM.lookup (Key.fromText k) fields >>= value ks
value _ _=Nothing
