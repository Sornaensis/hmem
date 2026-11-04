module HMem.Server.Archive.WorkerSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (concurrently, withAsync, cancel, waitCatch)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar, tryPutMVar, tryReadMVar)
import Control.Exception (bracket, try, finally, uninterruptibleMask_, fromException)
import Control.Monad (foldM, forM_)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as B8
import Data.Bits (xor, shiftL, shiftR, (.&.))
import Data.Functor.Contravariant (contramap)
import Data.IORef
import Data.Time (UTCTime)
import Data.Word (Word64)
import Data.UUID (UUID)
import Data.UUID.V4 qualified as UUID
import Data.Pool (Pool, destroyAllResources)
import Data.Text qualified as T
import Hasql.Connection qualified as H
import Hasql.Decoders qualified as D
import Hasql.Encoders qualified as E
import Hasql.Statement qualified as St
import Hasql.Session qualified as S
import System.Timeout (timeout)
import Test.Hspec

import HMem.AgentSession
import HMem.AgentSession.Occurrence qualified as Occurrence
import HMem.DB.AgentSession
import HMem.DB.Pool (DBException(..), runSession, createPool)
import HMem.DB.Pool (runTransaction)
import HMem.DB.SessionArchive qualified as DB
import HMem.DB.SessionArchive.Maintenance qualified as Maintenance
import HMem.DB.Task qualified as Task
import HMem.DB.TestHarness
import HMem.Server.Archive.Worker
import HMem.Server.SessionReaper (reapSessionBatch)
import HMem.SessionArchive.Codec qualified as Codec
import HMem.SessionArchive.Replacement qualified as Replacement
import HMem.Types qualified as Types

spec :: Spec
spec = around withTestEnv $ describe "Archive worker" $ do
  it "retains both failed worker drains and re-drains every actual child from the aggregate receipt" $ \_ -> bounded $ do
    archiveReady <- newEmptyMVar
    occurrenceReady <- newEmptyMVar
    releaseArchive <- newEmptyMVar
    releaseOccurrence <- newEmptyMVar
    archiveFinished <- newEmptyMVar
    occurrenceFinished <- newEmptyMVar
    let controlled ready release finished =
          (putMVar ready () >> uninterruptibleMask_ (takeMVar release)) `finally` putMVar finished ()
    worker <- startArchiveWorkerWithLoops
      (controlled archiveReady releaseArchive archiveFinished)
      (controlled occurrenceReady releaseOccurrence occurrenceFinished)
    let recover = do
          _ <- tryPutMVar releaseArchive ()
          _ <- tryPutMVar releaseOccurrence ()
          timeout 6000000 (stopArchiveWorker worker) `shouldReturn` Just ()
    (do
      timeout 1000000 (takeMVar archiveReady) `shouldReturn` Just ()
      timeout 1000000 (takeMVar occurrenceReady) `shouldReturn` Just ()
      stopped <- timeout 6000000 (try @ArchiveDrainFailure (stopArchiveWorker worker))
      receipt <- case stopped of Just (Left failure) -> pure failure; _ -> expectationFailure "Expected two retained failed drains" >> fail "missing drain receipt"
      let ArchiveDrainFailure failures=receipt
      length failures `shouldBe` 2
      case failures of
        [(first,_),(second,_)] -> first `shouldNotBe` second
        _ -> expectationFailure "Expected both distinct actual owners"
      forM_ failures $ \(_,failure) -> (fromException failure :: Maybe ArchiveFailure) `shouldSatisfy` (\case Just (ArchiveJoinFailure _) -> True; _ -> False)
      tryReadMVar archiveFinished `shouldReturn` Nothing
      tryReadMVar occurrenceFinished `shouldReturn` Nothing
      _ <- tryPutMVar releaseArchive ()
      _ <- tryPutMVar releaseOccurrence ()
      timeout 6000000 (retryArchiveDrain receipt) `shouldReturn` Just ()
      timeout 1000000 (takeMVar archiveFinished) `shouldReturn` Just ()
      timeout 1000000 (takeMVar occurrenceFinished) `shouldReturn` Just ()) `finally` recover

  it "corrects historical gzip-only occurrence metadata without altering sealed receipts" $ \env -> bounded $ do
    let body="{\"timestamp\":\"2026-10-30T12:00:00Z\",\"type\":\"event_msg\",\"payload\":{\"type\":\"custom_event\",\"detail\":\"archived activity\"}}\r\n"
    (w,sealed,_) <- fixture env [body]
    lease <- claim env
    prepared <- compressArchive env.pool lease
    verifyAndPublishArchive env.pool lease prepared
    withOccurrenceRuntime env $ \pool -> do
      Maintenance.configureMaintenance pool Maintenance.EnableReclamation
      Just reclaim <- Maintenance.claimReclamation pool
      runReclamationLeaseWith (pure ()) pool reclaim
    runSession env.pool (S.statement () (St.Statement
      ("SELECT NOT EXISTS(SELECT 1 FROM session_chunks WHERE session_id='"<>uuid sealed.id<>"')")
      E.noParams (D.singleRow (D.column (D.nonNullable D.bool))) False)) `shouldReturn` True
    execute env ("DELETE FROM session_occurrences WHERE session_id='"<>uuid sealed.id<>"'")
    bracket (startArchiveWorker env.pool) stopArchiveWorker $ \_ -> do
      let await = getSession env.pool w sealed.id >>= \case
            Just row | row.lastActivityAt/=Nothing -> pure ()
            _ -> threadDelay 10000 >> await
      timeout 10000000 await `shouldReturn` Just ()
    Just corrected <- getSession env.pool w sealed.id
    corrected.lastActivityAt `shouldBe` Occurrence.parseOccurrenceTime "2026-10-30T12:00:00Z"
    (corrected.finalBytes,corrected.finalSha256,corrected.revision,corrected.lastSeenAt) `shouldBe`
      (sealed.finalBytes,sealed.finalSha256,sealed.revision,sealed.lastSeenAt)
    readSessionRange env.pool w sealed.id 0 maxSessionRangeBytes >>= \r -> r.bytes `shouldBe` body

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

  it "freezes replacement through retry and verifies an isolated sanitized gzip generation" $ \env -> bounded $ do
    firstPolicy <- installReplacementPolicy env "first"
    let prefix = "{\"type\":\"response_item\",\"payload\":{\"type\":\"function_call\",\"encrypted_content\":"
        suffix = ",\"padding\":\"" <> BS.replicate 7300 112 <> "\"}}\r\n"
        originalLine = prefix <> "\"" <> BS.replicate 8000 120 <> "\"" <> suffix
        sanitizedLine = prefix <> "\"first\"" <> suffix
        originalChunk = BS.concat (replicate 48 originalLine)
        originalChunks = replicate 3 originalChunk <> [" [] "]
        original = BS.concat originalChunks
        expected = BS.concat (replicate 144 sanitizedLine) <> " [] "
    (w,sealed,_) <- fixtureWithFormat env "codex_jsonl" originalChunks
    first <- claim env
    (fmap (fmap (.snapshotBytes)) <$> DB.readArchiveReplacementSnapshot env.pool first) `shouldReturn` Just (Just firstPolicy)
    before <- compressArchive env.pool first
    _ <- installReplacementPolicy env "second"
    DB.releaseArchive env.pool first False "fixture_retry" `shouldReturn` True
    execute env ("UPDATE session_archive_jobs SET retry_at=clock_timestamp() WHERE session_id='"<>uuid sealed.id<>"'")
    let drain = do
          removed <- DB.cleanupArchiveGeneration env.pool w sealed.id first.generationId
          if removed == 0 then pure () else drain
    drain
    second <- claim env
    second.generationId `shouldNotBe` first.generationId
    (fmap (fmap (.snapshotBytes)) <$> DB.readArchiveReplacementSnapshot env.pool second) `shouldReturn` Just (Just firstPolicy)
    after <- compressArchive env.pool second
    after.encoded `shouldBe` before.encoded
    after.sanitized `shouldBe` Just (DB.SanitizedContent (sessionSha256 firstPolicy)
      (fromIntegral (BS.length expected)) (sessionSha256 expected) 144)
    verifyAndPublishArchive env.pool second after
    Just job <- DB.getArchiveJob env.pool w sealed.id
    job.state `shouldBe` "ready"
    DB.getPublishedArchive env.pool w sealed.id `shouldReturn` Nothing
    DB.readArchivePart env.pool w sealed.id second.generationId 0 `shouldReturn` Nothing
    execute env ("INSERT INTO session_archive_reclamation(generation_id,session_id,workspace_id,metadata_sha256,verified_at) "
      <> "SELECT id,session_id,workspace_id,metadata_sha256,clock_timestamp() FROM session_archive_generations WHERE id='"
      <> uuid second.generationId <> "'") `shouldThrow` \case
        DBCheckViolation message -> message == "Sanitized attestation requires a fenced verified publication"
        _ -> False
    runSession env.pool (S.statement second.generationId (St.Statement
      "SELECT content_bytes,content_sha256 FROM session_archive_sanitized_contents WHERE generation_id=$1"
      (E.param (E.nonNullable E.uuid))
      (D.singleRow ((,) <$> D.column (D.nonNullable D.int8) <*> D.column (D.nonNullable D.text))) True))
      `shouldReturn` (fromIntegral (BS.length expected),sessionSha256 expected)
    parts <- runSession env.pool (S.statement second.generationId (St.Statement
      "SELECT data FROM session_archive_parts WHERE generation_id=$1 ORDER BY ordinal"
      (E.param (E.nonNullable E.uuid)) (D.rowList (D.column (D.nonNullable D.bytea))) True))
    pending <- newIORef parts
    chunks <- newIORef []
    let next = atomicModifyIORef' pending $ \case
          [] -> ([],Nothing)
          bytes:rest -> (rest,Just bytes)
    decoded <- Codec.verify (fromIntegral (BS.length expected)) next (\bytes -> modifyIORef' chunks (bytes:)) (const (pure ()))
    decoded.checkpoints `shouldBe` 2
    (BS.concat . reverse <$> readIORef chunks) `shouldReturn` expected
    ((\r -> r.bytes) <$> readSessionRange env.pool w sealed.id 0 (BS.length originalChunk))
      `shouldReturn` originalChunk
    getSession env.pool w sealed.id `shouldReturn` Just sealed
    sessionSha256 original `shouldBe` after.receipt.rawSha256

  it "records ambiguous sanitized source as failed and retains original bytes" $ \env -> bounded $ do
    _ <- installReplacementPolicy env "first"
    let duplicate = "{\"x\":1,\"x\":2}\n"
    (w,sealed,_) <- fixtureWithFormat env "codex_jsonl" [duplicate]
    lease <- claim env
    runArchiveLease env.pool lease
    Just job <- DB.getArchiveJob env.pool w sealed.id
    (job.state,job.generationId,job.errorCode) `shouldBe` ("failed",Nothing,Just "archive_replacement_record_ambiguous")
    DB.getPublishedArchive env.pool w sealed.id `shouldReturn` Nothing
    ((\r -> r.bytes) <$> readSessionRange env.pool w sealed.id 0 65536) `shouldReturn` duplicate

  forM_
    [ ("ends a sanitized attempt that exhausts its fixed reservation", "reserved_bytes=1024", "failed", "archive_replacement_reservation_exhausted")
    , ("retries a sanitized attempt blocked by global staging capacity", "staging_budget=1024", "retry", "archive_resource_blocked")
    ] $ \(description, limit, state, code) -> it description $ \env -> bounded $ do
      _ <- installReplacementPolicy env (BS.replicate 200 120)
      let original = "{\"type\":\"response_item\",\"payload\":{\"type\":\"function_call\",\"encrypted_content\":\"x\"}}\n"
      (w,sealed,_) <- fixtureWithFormat env "codex_jsonl" [original]
      lease <- claim env
      execute env ("UPDATE session_archive_control SET "<>limit<>" WHERE singleton")
      runArchiveLease env.pool lease
      Just job <- DB.getArchiveJob env.pool w sealed.id
      (job.state,job.generationId,job.errorCode) `shouldBe` (state,Nothing,Just code)
      DB.getPublishedArchive env.pool w sealed.id `shouldReturn` Nothing
      ((\r -> r.bytes) <$> readSessionRange env.pool w sealed.id 0 65536) `shouldReturn` original

  it "keeps the empty and 1 GiB attempt budgets finite without rounding above 64-bit boundaries" $ \_ -> do
    archiveAttemptMicros 0 `shouldBe` 30000000
    archiveAttemptMicros 1073741824 `shouldBe` 120000000

bounded :: IO () -> IO ()
bounded action = timeout 25000000 action >>= \r -> r `shouldBe` Just ()
fixture :: TestEnv -> [ByteString] -> IO (UUID,AgentSession,UUID)
fixture env = fixtureWithFormat env "jsonl"

fixtureWithFormat :: TestEnv -> T.Text -> [ByteString] -> IO (UUID,AgentSession,UUID)
fixtureWithFormat env format chunks = do
  w <- (.id) <$> createTestWorkspace env "archive-worker"
  task <- Task.createTask env.pool (Types.CreateTask w Nothing Nothing "Archive worker fixture" Nothing Nothing Nothing)
  execute env ("UPDATE tasks SET status='in_progress' WHERE id='"<>uuid task.id<>"'")
  identifier <- UUID.nextRandom
  initial <- createSession env.pool (CreateSession identifier w format "Archive worker fixture" Nothing Nothing task.id)
  row <- foldM (\s bytes -> (.session) <$> appendSession env.pool w identifier (AppendSession s.committedBytes s.revision bytes True)) initial chunks
  sealed <- terminateSession env.pool w identifier (TerminateSession row.committedBytes row.revision row.committedBytes (sessionSha256 (BS.concat chunks)))
  pure (w,sealed,task.id)

installReplacementPolicy :: TestEnv -> ByteString -> IO ByteString
installReplacementPolicy env replacement = do
  let cutoff = read "2000-01-01 00:00:00 UTC" :: UTCTime
      label = "codex_jsonl" :: T.Text
      config = "{\"version\":1,\"eligible_from\":\"2000-01-01T00:00:00Z\",\"formats\":{\"codex_jsonl\":[{\"match\":{\"/type\":\"response_item\",\"/payload/type\":\"function_call\"},\"path\":\"/payload/encrypted_content\",\"replacement\":\""<>replacement<>"\"}]}}"
  policy <- either (fail . show) pure (Replacement.decodePolicy config)
  frozen <- either (fail . show) pure (Replacement.freezePolicy cutoff label policy)
  (eligible,snapshot) <- maybe (fail "replacement policy was not frozen") pure frozen
  runSession env.pool $ S.statement (label,eligible,snapshot) $ St.Statement
    "INSERT INTO session_archive_replacement_policies(format_label,eligible_from,snapshot) VALUES($1,$2,$3) ON CONFLICT(format_label) DO UPDATE SET eligible_from=EXCLUDED.eligible_from,snapshot=EXCLUDED.snapshot"
    (mconcat [contramap (\(l,_,_)->l) (E.param (E.nonNullable E.text)),
      contramap (\(_,t,_)->t) (E.param (E.nonNullable E.timestamptz)),
      contramap (\(_,_,s)->s) (E.param (E.nonNullable E.bytea))]) D.noResult True
  pure snapshot
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

withOccurrenceRuntime :: TestEnv -> (Pool H.Connection -> IO a) -> IO a
withOccurrenceRuntime admin use = do
  identifier <- UUID.nextRandom
  let role="occurrence_runtime_"<>B8.filter (/='-') (uuid identifier)
      create=execute admin ("CREATE ROLE "<>role<>" LOGIN NOINHERIT NOSUPERUSER NOCREATEDB NOCREATEROLE NOREPLICATION NOBYPASSRLS; GRANT USAGE ON SCHEMA public TO "<>role<>"; GRANT SELECT,INSERT,UPDATE,DELETE ON ALL TABLES IN SCHEMA public TO "<>role<>"; GRANT EXECUTE ON ALL FUNCTIONS IN SCHEMA public TO "<>role)
      dropRole=execute admin ("DROP OWNED BY "<>role<>"; DROP ROLE "<>role)
  bracket create (const dropRole) $ \_ -> bracket
    (createPool (admin.testDb.testDbConnStr<>" user="<>T.pack (B8.unpack role)) 2 30 30000)
    destroyAllResources use

entropyText :: Word64 -> Int -> ByteString
entropyText seed count = fst $ BS.unfoldrN count (\state ->
  let a = state `xor` (state `shiftL` 13)
      b = a `xor` (a `shiftR` 7)
      next = b `xor` (b `shiftL` 17)
  in Just (BS.index "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_" (fromIntegral (next .&. 63)),next)) seed
