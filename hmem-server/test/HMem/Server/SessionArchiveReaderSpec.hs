module HMem.Server.SessionArchiveReaderSpec (spec) where

import Codec.Compression.Zlib.Raw qualified as Z
import Control.Concurrent (threadDelay)
import Control.Exception (bracket,bracket_)
import Control.Monad (foldM, forM_)
import Data.Aeson (FromJSON, encode, eitherDecode)
import Data.Bits (shiftL, (.|.))
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Text.Encoding qualified as TE
import Data.Text qualified
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID
import Data.Word (Word64)
import Data.Pool (destroyAllResources)
import Hasql.Session qualified as SQL
import Network.HTTP.Types
import Network.Wai qualified as Wai
import Network.Wai.Test (SResponse(..),SRequest(..),runSession,srequest,setPath)
import System.Timeout (timeout)
import Test.Hspec

import HMem.AgentSession qualified as Core
import HMem.AgentSession.Transport qualified as Wire
import HMem.Config qualified
import HMem.DB.AgentSession qualified as DB
import HMem.DB.Pool qualified as Pool
import HMem.DB.SessionArchive qualified as Archive
import HMem.DB.SessionArchive.Reader qualified as Reader
import HMem.DB.Task qualified as Task
import HMem.DB.TestHarness (TestEnv(..), TestDb(..), createTestWorkspace)
import HMem.Server.AccessTracker (newAccessTracker)
import HMem.Server.App (mkAppWithPrefixProofs)
import HMem.Server.Archive.Worker (runArchiveLease)
import HMem.Server.SessionPrefix (newPrefixRegistry,closePrefixRegistry)
import HMem.Server.TestHarness
import HMem.SessionArchive.Codec qualified as Codec
import HMem.Types qualified as Types

spec :: Spec
spec = describe "Source-absent archive compatibility HTTP" $ do
  it "serves exact evidence pages, historical/whole proofs, terminal replay and original ZIP bytes after raw removal" $
    withLocalSandboxAppContext $ \ctx -> bracket (newPrefixRegistry ctx.localEnv.pool) closePrefixRegistry $ \registry -> do
      let env=ctx.localEnv; cfg=ctx.localConfig
          first="\""<>BS.replicate 170001 120<>"\"\r\n"
          chunks=replicate 19 first<>["[true,null]"]
          raw=BS.concat chunks
      tracker <- newAccessTracker env.pool 3600
      app <- mkAppWithPrefixProofs registry Nothing cfg.changeStream id cfg.auth cfg.cors cfg.rateLimit env.pool tracker ctx.localWSState cfg.web.webStaticDir True
      workspace <- createTestWorkspace env "source-absent-http"
      task <- Task.createTask env.pool (Types.CreateTask workspace.id Nothing Nothing "Source absent compatibility" Nothing Nothing Nothing)
      Pool.runSession env.pool $ SQL.sql ("UPDATE tasks SET status='in_progress' WHERE id='"<>uuid task.id<>"'")
      identifier <- UUID.nextRandom
      initial <- DB.createSession env.pool (Core.CreateSession identifier workspace.id "jsonl" "Archived compatibility" Nothing Nothing task.id)
      uploaded <- foldM (\previous bytes -> (.session) <$> DB.appendSession env.pool workspace.id identifier
        (Core.AppendSession previous.committedBytes previous.revision bytes (not (BS.isSuffixOf "\n" bytes)))) initial chunks
      sealed <- DB.terminateSession env.pool workspace.id identifier (Core.TerminateSession uploaded.committedBytes uploaded.revision uploaded.committedBytes (Core.sessionSha256 raw))
      Reader.ensureArchive env.pool workspace.id identifier
      implementation <- Codec.codecImplementation
      Just lease <- Archive.claimArchive env.pool implementation
      timeout 30000000 (runArchiveLease env.pool lease) `shouldReturn` Just ()
      Pool.runTransaction env.pool $ SQL.sql $
        "ALTER TABLE session_chunks DISABLE TRIGGER session_chunk_guard; DELETE FROM session_chunks c WHERE session_id='"<>uuid identifier<>"' AND EXISTS(SELECT 1 FROM session_archives g WHERE g.session_id=c.session_id); ALTER TABLE session_chunks ENABLE TRIGGER session_chunk_guard"
      let target="/api/v1/workspaces/"<>uuid workspace.id<>"/sessions/"<>uuid identifier
          proofPath=target<>"/prefix-verifications"
      forM_ [0,1,170000,1048575,1048576,fromIntegral (BS.length raw)-1,fromIntegral (BS.length raw)] $ \offset -> do
        result <- request app methodGet (target<>"/evidence?offset="<>TE.encodeUtf8 (Data.Text.pack (show (offset::Int)))<>"&limit=65536") ""
        range <- decoded @Wire.SessionRangeResponse result
        Wire.decodeSessionBytes range.dataBase64 `shouldBe` Right (BS.take 65536 (BS.drop offset raw))
        range.session `shouldBe` sealed
      forM_ [(fromIntegral (BS.length first),1,Core.sessionSha256 first),(uploaded.committedBytes,uploaded.revision,Core.sessionSha256 raw)] $ \(end,revision,digest) -> do
        operation <- UUID.nextRandom
        let input=Wire.PrefixProofRequest operation end revision digest
        submitted <- request app methodPost proofPath (encode input) >>= decoded @Wire.PrefixProofResponse
        submitted.operationId `shouldBe` operation
        result <- timeout 10000000 $ await app (proofPath<>"/"<>uuid operation)
        fmap (.state) result `shouldBe` Just "verified"
      replay <- request app methodPost (target<>"/append")
        (encode (Wire.AppendSessionRequest 0 0 (Wire.encodeSessionBytes first) False)) >>= decoded @Wire.AppendSessionResponse
      (replay.replayed,replay.session) `shouldBe` (True,sealed)
      zipResponse <- request app methodGet ("/api/v1/workspaces/"<>uuid workspace.id<>"/tasks/"<>uuid task.id<>"/evidence.zip") ""
      simpleStatus zipResponse `shouldBe` status200
      let bytes=LBS.toStrict zipResponse.simpleBody
          little position count=foldr (\n acc -> acc `shiftL` 8 .|. fromIntegral (BS.index bytes (position+n))) (0::Word64) [0..count-1]
          start=30+fromIntegral (little 26 2)+fromIntegral (little 28 2)
          central=fromIntegral (little (BS.length bytes-98+48) 8)
          compressed=BS.take (central-24-start) (BS.drop start bytes)
      LBS.toStrict (Z.decompress (LBS.fromStrict compressed)) `shouldBe` raw
      Pool.getPoolMetrics >>= (\m -> m.activeConnections `shouldBe` 0)
      guardedApp <- mkAppWithPrefixProofs registry Nothing cfg.changeStream id cfg.auth cfg.cors cfg.rateLimit env.pool tracker ctx.localWSState cfg.web.webStaticDir True
      bracket (Pool.createPool env.testDb.testDbConnStr 1 30 30000) destroyAllResources $ \blockers -> Pool.withConn blockers $ \connection -> do
        let sql statement=SQL.run (SQL.sql statement) connection >>= either (fail . show) pure
        bracket_ (sql "BEGIN; LOCK session_archive_parts IN ACCESS EXCLUSIVE MODE") (sql "ROLLBACK") $ do
          limited <- timeout 10000000 $ request guardedApp methodGet (target<>"/evidence?offset=0&limit=65536") ""
          response <- maybe (fail "Archive SQL deadline did not finish its HTTP request") pure limited
          simpleStatus response `shouldBe` status503
          lookup "Retry-After" response.simpleHeaders `shouldBe` Just "1"
          LBS.toStrict response.simpleBody `shouldSatisfy` BS.isInfixOf "archive_unavailable"
          LBS.toStrict response.simpleBody `shouldSatisfy` (not . BS.isInfixOf "data_base64")
      retry <- request guardedApp methodGet (target<>"/evidence?offset=0&limit=65536") "" >>= decoded @Wire.SessionRangeResponse
      Wire.decodeSessionBytes retry.dataBase64 `shouldBe` Right (BS.take 65536 raw)
      Pool.getPoolMetrics >>= (\m -> m.activeConnections `shouldBe` 0)
      -- The header is its own first part; checkpoint zero starts at byte ten.
      -- Alter a consumed body part under private fixture authority and use a
      -- fresh application cache so previously verified bytes cannot mask it.
      Pool.runTransaction env.pool $ SQL.sql $
        "ALTER TABLE session_archive_parts DISABLE TRIGGER session_archive_stage_guard; UPDATE session_archive_parts SET data=set_byte(data,0,get_byte(data,0)#1) WHERE generation_id='"<>uuid lease.generationId<>"' AND ordinal=1; ALTER TABLE session_archive_parts ENABLE TRIGGER session_archive_stage_guard"
      fresh <- mkAppWithPrefixProofs registry Nothing cfg.changeStream id cfg.auth cfg.cors cfg.rateLimit env.pool tracker ctx.localWSState cfg.web.webStaticDir True
      unavailable <- request fresh methodGet (target<>"/evidence?offset=0&limit=65536") ""
      simpleStatus unavailable `shouldBe` status503
      lookup "Retry-After" unavailable.simpleHeaders `shouldBe` Just "1"
      LBS.toStrict unavailable.simpleBody `shouldSatisfy` BS.isInfixOf "archive_unavailable"
      LBS.toStrict unavailable.simpleBody `shouldSatisfy` (not . BS.isInfixOf "data_base64")

request :: Wai.Application -> Method -> BS.ByteString -> LBS.ByteString -> IO SResponse
request app method path body = runSession (srequest (SRequest ((setPath Wai.defaultRequest path)
  {Wai.requestMethod=method,Wai.requestHeaders=[(hContentType,"application/json"),("X-HMem-Bot-Identity","{\"role\":\"Reader Test\",\"model-id\":\"test:fixture:default\"}")]}) body)) app
decoded :: FromJSON a => SResponse -> IO a
decoded response = do
  simpleStatus response `shouldBe` status200
  either fail pure (eitherDecode response.simpleBody)
await :: Wai.Application -> BS.ByteString -> IO Wire.PrefixProofResponse
await app path = do
  result <- request app methodGet path "" >>= decoded @Wire.PrefixProofResponse
  if result.state == "pending" then threadDelay 1000 >> await app path else pure result
uuid :: UUID -> BS.ByteString
uuid = TE.encodeUtf8 . UUID.toText
