module HMem.Server.Archive.WorkerSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (concurrently, withAsync, cancel, waitCatch)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar, tryPutMVar)
import Control.Exception (bracket, try, finally, uninterruptibleMask_)
import Control.Monad (foldM, forM_)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as B8
import Data.Bits (xor, shiftL, shiftR, (.&.))
import Data.Word (Word64)
import Data.UUID (UUID)
import Data.UUID.V4 qualified as UUID
import Hasql.Session qualified as S
import System.Timeout (timeout)
import Test.Hspec

import HMem.AgentSession
import HMem.DB.AgentSession
import HMem.DB.Pool (runSession)
import HMem.DB.Pool (runTransaction)
import HMem.DB.SessionArchive qualified as DB
import HMem.DB.Task qualified as Task
import HMem.DB.TestHarness
import HMem.Server.Archive.Worker
import HMem.Server.SessionReaper (reapSessionBatch)
import HMem.SessionArchive.Codec qualified as Codec
import HMem.Types qualified as Types

spec :: Spec
spec = around withTestEnv $ describe "Archive worker" $ do
  it "publishes empty, CRLF/UTF-8, final-no-LF and maximum-record evidence without changing its receipt or task" $ \env -> bounded $ do
    forM_ [[],["{}\r\n[\"\195\166\"]"],["{}","\n[]\n"],["{}","\r\n[]\r\n"],["\""<>BS.replicate 1048574 120<>"\""],replicate 4 (BS.concat (replicate 60000 "{}\n"))<>["[]"]] $ \chunks -> do
      (w,sealed,taskId) <- fixture env chunks
      taskBefore <- Task.getTask env.pool taskId
      lease <- claim env
      prepared <- compressArchive env.pool lease
      verifyAndPublishArchive env.pool lease prepared
      Just manifest <- DB.getPublishedArchive env.pool w sealed.id
      (manifest.rawBytes,manifest.rawSha256,manifest.ledgerCount,manifest.codec) `shouldBe`
        (sealed.committedBytes,sessionSha256 (BS.concat chunks),fromIntegral (length chunks),"gzip-v1")
      getSession env.pool w sealed.id `shouldReturn` Just sealed
      Task.getTask env.pool taskId `shouldReturn` taskBefore
      original <- readSessionRange env.pool w sealed.id 0 maxSessionRangeBytes
      original.bytes `shouldBe` BS.take maxSessionRangeBytes (BS.concat chunks)
      DB.claimArchive env.pool "duplicate" `shouldReturn` Nothing

  it "finds sealed work after a seal-to-enqueue crash and joins an idle worker finitely" $ \env -> bounded $ do
    (w,sealed,_) <- fixture env ["{}\n"]
    DB.getArchiveJob env.pool w sealed.id `shouldReturn` Nothing
    bracket (startArchiveWorker env.pool) stopArchiveWorker $ \_ -> do
      let await = DB.getPublishedArchive env.pool w sealed.id >>= \case
            Just _ -> pure (); Nothing -> threadDelay 10000 >> await
      timeout 10000000 await `shouldReturn` Just ()
    worker <- startArchiveWorker env.pool
    timeout 5000000 (stopArchiveWorker worker) `shouldReturn` Just ()

  it "archives poorly compressible JSONL through multiple bounded persisted parts" $ \env -> bounded $ do
    let chunks = [BS.concat ["{\"text\":\"" <> entropyText (fromIntegral (chunk*2048+record+1)) 128 <> "\"}\n" | record <- [0..2047 :: Int]] | chunk <- [0..7 :: Int]]
    (w,sealed,_) <- fixture env chunks
    lease <- claim env
    prepared <- compressArchive env.pool lease
    prepared.encoded.compressed.byteCount `shouldSatisfy` (>1048576)
    prepared.partCount `shouldSatisfy` (>1)
    Just part <- DB.readStagedPart env.pool lease 1
    BS.length part.bytes `shouldSatisfy` (\n -> n>0 && n<=65536)
    part.endOffset-part.startOffset `shouldBe` fromIntegral (BS.length part.bytes)
    part.dataSha256 `shouldBe` sessionSha256 part.bytes
    verifyAndPublishArchive env.pool lease prepared
    Just manifest <- DB.getPublishedArchive env.pool w sealed.id
    (manifest.rawBytes,manifest.rawSha256) `shouldBe` (sealed.committedBytes,sessionSha256 (BS.concat chunks))

  it "retains and re-drains the actual renewal child after its parent exits with a failed join" $ \env -> bounded $ do
    _ <- fixture env ["{}\n"]
    attemptReady <- newEmptyMVar
    attemptBlock <- newEmptyMVar
    renewalFinalizing <- newEmptyMVar
    releaseRenewal <- newEmptyMVar
    worker <- startArchiveWorkerWith
      (putMVar attemptReady () >> takeMVar attemptBlock)
      (putMVar renewalFinalizing () >> uninterruptibleMask_ (takeMVar releaseRenewal)) env.pool
    let recover = do
          _ <- tryPutMVar releaseRenewal ()
          timeout 6000000 (stopArchiveWorker worker) >>= (`shouldBe` Just ())
    (do
      timeout 5000000 (takeMVar attemptReady) `shouldReturn` Just ()
      result <- try @ArchiveFailure (stopArchiveWorker worker)
      result `shouldSatisfy` either (const True) (const False)
      timeout 1000000 (takeMVar renewalFinalizing) `shouldReturn` Just ()
      _ <- tryPutMVar releaseRenewal ()
      timeout 6000000 (stopArchiveWorker worker) `shouldReturn` Just ()) `finally` recover

  it "archives automatically sealed idle evidence through the same durable scan" $ \env -> bounded $ do
    w <- (.id) <$> createTestWorkspace env "archive-auto-seal"
    task <- Task.createTask env.pool (Types.CreateTask w Nothing Nothing "Idle archive" Nothing Nothing Nothing)
    execute env ("UPDATE tasks SET status='in_progress' WHERE id='"<>uuid task.id<>"'")
    identifier <- UUID.nextRandom
    runTransaction env.pool $ S.sql ("INSERT INTO sessions(id,workspace_id,format_label,title,created_at,last_seen_at) VALUES ('"<>uuid identifier<>"','"<>uuid w<>"','jsonl','Idle archive',clock_timestamp()-interval '2 hours',clock_timestamp()-interval '2 hours'); INSERT INTO task_sessions(workspace_id,session_id,task_id) VALUES ('"<>uuid w<>"','"<>uuid identifier<>"','"<>uuid task.id<>"')")
    execute env ("UPDATE tasks SET status='done' WHERE id='"<>uuid task.id<>"'")
    _ <- reapSessionBatch env.pool Nothing
    lease <- claim env
    runArchiveLease env.pool lease
    Just manifest <- DB.getPublishedArchive env.pool w identifier
    manifest.rawBytes `shouldBe` 0

  it "cancels and joins an attempt with its renewal child, retaining raw bytes and a retry state" $ \env -> bounded $ do
    (w,sealed,_) <- fixture env ["{}\r\n[]"]
    lease <- claim env
    entered <- newEmptyMVar
    blocked <- newEmptyMVar
    Just initialJob <- DB.getArchiveJob env.pool w sealed.id
    withAsync (runArchiveLeaseWith (putMVar entered () >> takeMVar blocked) env.pool lease) $ \attempt -> do
      timeout 5000000 (takeMVar entered) `shouldReturn` Just ()
      let awaitRenewal = do
            Just job <- DB.getArchiveJob env.pool w sealed.id
            if job.leaseExpiresAt > initialJob.leaseExpiresAt then pure () else threadDelay 10000 >> awaitRenewal
      timeout 14000000 awaitRenewal `shouldReturn` Just ()
      timeout 5000000 (cancel attempt) `shouldReturn` Just ()
      _ <- waitCatch attempt
      pure ()
    Just job <- DB.getArchiveJob env.pool w sealed.id
    (job.state,job.generationId,job.errorCode) `shouldBe` ("retry",Nothing,Just "archive_cancelled")
    getSession env.pool w sealed.id `shouldReturn` Just sealed

  it "restarts with a fresh generation and fences an expired owner before publication" $ \env -> bounded $ do
    (w,sealed,_) <- fixture env ["{}\r\n[]"]
    old <- claim env
    prepared <- compressArchive env.pool old
    expire env sealed.id
    DB.renewArchive env.pool old `shouldReturn` False
    verifyAndPublishArchive env.pool old prepared `shouldThrow` anyException
    DB.claimArchive env.pool "restart" `shouldReturn` Nothing
    let drain = do
          n <- DB.cleanupArchiveGeneration env.pool w sealed.id old.generationId
          if n==0 then pure () else drain
    drain
    (a,b) <- concurrently (DB.claimArchive env.pool "restart") (DB.claimArchive env.pool "restart")
    let leases = [l | Just l <- [a,b]]
    length leases `shouldBe` 1
    let fresh = head leases
    fresh.generationId `shouldNotBe` old.generationId
    runArchiveLease env.pool fresh
    Just manifest <- DB.getPublishedArchive env.pool w sealed.id
    manifest.id `shouldBe` fresh.generationId
    DB.cleanupArchiveGeneration env.pool w sealed.id fresh.generationId `shouldReturn` 0

  it "rejects corrupt persisted output before publication and leaves raw evidence available" $ \env -> bounded $ do
    (w,sealed,_) <- fixture env ["{}\r\n[]"]
    lease <- claim env
    prepared <- compressArchive env.pool lease
    alterStaged env $ "UPDATE session_archive_parts SET data=set_byte(data,0,255) WHERE generation_id='"<>uuid lease.generationId<>"' AND ordinal=1"
    verifyAndPublishArchive env.pool lease prepared `shouldThrow` anyException
    DB.getPublishedArchive env.pool w sealed.id `shouldReturn` Nothing
    DB.releaseArchive env.pool lease False "test_corrupt" `shouldReturn` True
    Just job <- DB.getArchiveJob env.pool w sealed.id
    job.state `shouldBe` "retry"
    readSessionRange env.pool w sealed.id 0 20 >>= \r -> r.bytes `shouldBe` "{}\r\n[]"

  it "rejects a self-consistently bound wrong checkpoint dictionary after successful full inflation" $ \env -> bounded $ do
    let chunks = replicate 3 (BS.concat (replicate 100000 "{\"a\":1}\n"))
    (w,sealed,_) <- fixture env chunks
    lease <- claim env
    prepared <- compressArchive env.pool lease
    let poison = alterStaged env $ "UPDATE session_archive_checkpoints c SET dictionary=set_byte(c.dictionary,0,(get_byte(c.dictionary,0)+1)%256),dictionary_sha256=encode(sha256(set_byte(c.dictionary,0,(get_byte(c.dictionary,0)+1)%256)),'hex'),binding_sha256=hmem_archive_binding((SELECT p.binding_sha256 FROM session_archive_checkpoints p WHERE p.generation_id=c.generation_id AND p.ordinal=c.ordinal-1),jsonb_build_array(c.generation_id,c.ordinal,c.raw_offset,c.compressed_offset,c.unused_bits,encode(sha256(set_byte(c.dictionary,0,(get_byte(c.dictionary,0)+1)%256)),'hex'))) WHERE c.generation_id='"<>uuid lease.generationId<>"' AND c.ordinal=1"
    result <- try @ArchiveFailure (verifyAndPublishArchiveWith poison env.pool lease prepared)
    result `shouldBe` Left (ArchiveFailure "archive_checkpoint_integrity")
    DB.getPublishedArchive env.pool w sealed.id `shouldReturn` Nothing

  it "keeps the empty and 1 GiB attempt budgets finite without rounding above 64-bit boundaries" $ \_ -> do
    archiveAttemptMicros 0 `shouldBe` 30000000
    archiveAttemptMicros 1073741824 `shouldBe` 120000000

bounded :: IO () -> IO ()
bounded action = timeout 25000000 action >>= \r -> r `shouldBe` Just ()
fixture :: TestEnv -> [ByteString] -> IO (UUID,AgentSession,UUID)
fixture env chunks = do
  w <- (.id) <$> createTestWorkspace env "archive-worker"
  task <- Task.createTask env.pool (Types.CreateTask w Nothing Nothing "Archive worker fixture" Nothing Nothing Nothing)
  execute env ("UPDATE tasks SET status='in_progress' WHERE id='"<>uuid task.id<>"'")
  identifier <- UUID.nextRandom
  initial <- createSession env.pool (CreateSession identifier w "jsonl" "Archive worker fixture" Nothing Nothing task.id)
  row <- foldM (\s bytes -> (.session) <$> appendSession env.pool w identifier (AppendSession s.committedBytes s.revision bytes True)) initial chunks
  sealed <- terminateSession env.pool w identifier (TerminateSession row.committedBytes row.revision row.committedBytes (sessionSha256 (BS.concat chunks)))
  pure (w,sealed,task.id)
claim :: TestEnv -> IO DB.ArchiveLease
claim env = do
  _ <- DB.enqueueArchives env.pool Nothing 100
  implementation <- Codec.codecImplementation
  Just lease <- DB.claimArchive env.pool implementation
  pure lease
expire :: TestEnv -> UUID -> IO ()
expire env session = do
  execute env ("UPDATE session_archive_jobs SET lease_expires_at=clock_timestamp()+interval '20 milliseconds' WHERE session_id='"<>uuid session<>"'")
  threadDelay 40000
alterStaged :: TestEnv -> ByteString -> IO ()
alterStaged env sql = runTransaction env.pool $ S.sql ("ALTER TABLE session_archive_parts DISABLE TRIGGER session_archive_stage_guard; ALTER TABLE session_archive_checkpoints DISABLE TRIGGER session_archive_stage_guard; "<>sql<>"; ALTER TABLE session_archive_parts ENABLE TRIGGER session_archive_stage_guard; ALTER TABLE session_archive_checkpoints ENABLE TRIGGER session_archive_stage_guard")
execute :: TestEnv -> ByteString -> IO ()
execute env sql = runSession env.pool (S.sql sql)
uuid :: UUID -> ByteString
uuid = B8.pack . show

entropyText :: Word64 -> Int -> ByteString
entropyText seed count = fst $ BS.unfoldrN count (\state ->
  let a = state `xor` (state `shiftL` 13)
      b = a `xor` (a `shiftR` 7)
      next = b `xor` (b `shiftL` 17)
  in Just (BS.index "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_" (fromIntegral (next .&. 63)),next)) seed
