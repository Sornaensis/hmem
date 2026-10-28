module HMem.DB.SessionArchive.ReaderSpec (spec) where

import Control.Concurrent.Async (concurrently,withAsync,cancel,waitCatch,wait)
import Control.Concurrent.MVar
import Control.Exception (bracket, bracket_, try, finally)
import Control.Concurrent (threadDelay)
import Control.Monad (foldM, forM_)
import Crypto.Hash (Context, Digest, SHA256, hashInit, hashUpdate, hashFinalize)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as B8
import Data.IORef
import Data.List (foldl',sort)
import Data.Text qualified as T
import Data.Time (getCurrentTime,diffUTCTime)
import Data.UUID (UUID)
import Data.UUID.V4 qualified as UUID
import Hasql.Session qualified as S
import System.Timeout (timeout)
import System.Mem (performGC)
import GHC.Stats
import Test.Hspec

import HMem.AgentSession
import HMem.DB.AgentSession
import HMem.DB.Pool (runSession, runTransaction, getPoolMetrics, PoolMetrics(..),createPool,withConn)
import Data.Pool (destroyAllResources)
import Hasql.Encoders qualified as E
import Hasql.Decoders qualified as D
import Hasql.Statement qualified as St
import HMem.DB.SessionArchive qualified as A
import HMem.DB.SessionArchive.Reader qualified as R
import HMem.DB.Task qualified as Task
import HMem.DB.TestHarness
import HMem.SessionArchive.Codec qualified as C
import HMem.Types qualified as Types

spec :: Spec
spec = around withTestEnv $ describe "Compatible archive reader" $ do
  it "preserves arbitrary raw offsets, historical boundaries and exact terminal replay after raw removal" $ \env -> bounded $ do
    let chunks=replicate 19 ("\""<>BS.replicate 170001 120<>"\"\r\n")<>["[true,null]"]
        raw=BS.concat chunks
    (w,sealed,manifest) <- fixture env chunks
    rawBefore <- readSessionRange env.pool w sealed.id 169999 1048576
    removeRaw env sealed.id
    readSessionEvidenceRange env.pool w sealed.id 169999 1048576 `shouldReturn` rawBefore
    forM_ [0,1,169999,170005,1048575,1048576,2097145,fromIntegral (BS.length raw)-1,fromIntegral (BS.length raw)] $ \offset -> do
      range <- readSessionEvidenceRange env.pool w sealed.id offset 1048576
      range.bytes `shouldBe` BS.take 1048576 (BS.drop (fromIntegral offset) raw)
      range.nextOffset `shouldBe` offset+fromIntegral (BS.length range.bytes)
    let first=head chunks; firstEnd=fromIntegral (BS.length first)
    validateSessionPrefixBoundary env.pool w sealed.id firstEnd 1 `shouldReturn` True
    validateSessionPrefixBoundary env.pool w sealed.id firstEnd 2 `shouldReturn` False
    validateSessionPrefixBoundary env.pool w sealed.id 0 0 `shouldReturn` True
    replay <- appendSession env.pool w sealed.id (AppendSession 0 0 first False)
    (replay.replayed,replay.session) `shouldBe` (True,sealed)
    wrong <- try @SessionException $ appendSession env.pool w sealed.id (AppendSession 0 0 first True)
    wrong `shouldSatisfy` either ((==SessionTerminated) . (.code)) (const False)
    wrongPayload <- try @SessionException $ appendSession env.pool w sealed.id
      (AppendSession 0 0 ("\""<>BS.replicate 170001 121<>"\"\r\n") False)
    wrongPayload `shouldSatisfy` either ((==SessionTerminated) . (.code)) (const False)
    bracket (newSessionReader env.pool w sealed.id firstEnd) closeSessionReader $ \reader -> do
      readSessionReader reader 0 65536 `shouldReturn` BS.take 65536 first
      readSessionReader reader 65536 1048576 `shouldReturn` BS.drop 65536 first
    (a,b) <- concurrently (readSessionRange env.pool w sealed.id 1 1048576) (readSessionRange env.pool w sealed.id 1048563 1048576)
    a.bytes `shouldBe` BS.take 1048576 (BS.drop 1 raw)
    b.bytes `shouldBe` BS.take 1048576 (BS.drop 1048563 raw)
    manifest.rawSha256 `shouldBe` sessionSha256 raw
    getPoolMetrics >>= (\m -> m.activeConnections `shouldBe` 0)

  it "fails closed on altered part bindings and plausible output from a wrong restart dictionary" $ \env -> bounded $ do
    let chunks=replicate 5 ("\""<>BS.replicate 1048573 120<>"\"\n")
    (w,sealed,manifest) <- fixture env chunks
    removeRaw env sealed.id
    -- Rebind the corrupt dictionary locally, so successful SQL metadata checks
    -- cannot substitute for hashing the complete original source chunk.
    privateMutation env "session_archive_checkpoints" $ "UPDATE session_archive_checkpoints c SET dictionary=decode(repeat('79',32768),'hex'),dictionary_sha256=encode(sha256(decode(repeat('79',32768),'hex')),'hex'),binding_sha256=hmem_archive_binding((SELECT p.binding_sha256 FROM session_archive_checkpoints p WHERE p.generation_id=c.generation_id AND p.ordinal=0),jsonb_build_array(c.generation_id,c.ordinal,c.raw_offset,c.compressed_offset,c.unused_bits,encode(sha256(decode(repeat('79',32768),'hex')),'hex'))) WHERE c.generation_id='"<>uuid manifest.id<>"' AND ordinal=1"
    failed <- try @C.CodecFailure $ readSessionRange env.pool w sealed.id 1048576 1
    failed `shouldSatisfy` either (const True) (const False)
    privateMutation env "session_archive_parts" $ "UPDATE session_archive_parts SET data=set_byte(data,0,get_byte(data,0)#1) WHERE generation_id='"<>uuid manifest.id<>"' AND ordinal=1"
    failedPart <- try @C.CodecFailure $ readSessionRange env.pool w sealed.id 0 1
    failedPart `shouldSatisfy` either (const True) (const False)

  it "keeps tiny-page cache fills bounded and isolates authenticated audiences" $ \env -> bounded $ do
    let chunk="\""<>BS.replicate 1048573 120<>"\"\n"
    (w,sealed,manifest) <- fixture env (replicate 3 chunk)
    removeRaw env sealed.id
    bracket newReaderCache closeReaderCache $ \cache -> do
      let audience=ArchiveAudience "resolved-account:credential-A"
      forM_ [0..1023] $ \n -> do
        page <- readSessionEvidenceRangeWithCache cache audience env.pool w sealed.id n 1
        page.bytes `shouldBe` BS.take 1 (BS.drop (fromIntegral n) chunk)
      stats <- R.withCachedReader cache audience env.pool manifest R.readerMetrics
      (stats.restores,stats.inflatedBytes,stats.verifiedBytes) `shouldBe` (1,1048576,1048576)
      foreignStats <- R.withCachedReader cache (ArchiveAudience "resolved-account:credential-B") env.pool manifest R.readerMetrics
      foreignStats `shouldBe` R.ReaderMetrics 0 0 0
      -- A miss in a later original chunk restores within the fixed 1 MiB gap.
      _ <- readSessionEvidenceRangeWithCache cache audience env.pool w sealed.id 2097161 1
      after <- R.withCachedReader cache audience env.pool manifest R.readerMetrics
      after.restores `shouldBe` 2
      after.inflatedBytes `shouldSatisfy` (<=3145728)

  it "serves an already verified immutable chunk continuation without a ledger query or pool lease" $ \env -> bounded $ do
    let chunk="\""<>BS.replicate 1048573 120<>"\"\n"
    (_,_,manifest) <- fixture env [chunk,chunk]
    bracket (R.openArchiveReader env.pool manifest) R.closeArchiveReader $ \reader -> do
      R.readArchiveRange reader 0 65536 `shouldReturn` BS.take 65536 chunk
      bracket (createPool env.testDb.testDbConnStr 1 30 30000) destroyAllResources $ \blockers -> withConn blockers $ \connection -> do
        let sql statement=S.run (S.sql statement) connection >>= either (fail . show) pure
        bracket_ (sql "BEGIN; LOCK session_archive_ledger IN ACCESS EXCLUSIVE MODE") (sql "ROLLBACK") $ do
          timeout 1000000 (R.readArchiveRange reader 65536 65536) `shouldReturn` Just (BS.take 65536 (BS.drop 65536 chunk))
          getPoolMetrics >>= (\m -> m.activeConnections `shouldBe` 1)
      R.readerMetrics reader `shouldReturn` R.ReaderMetrics 1 1048576 1048576

  forM_ [1,100,1024] $ \megabytes ->
    it ("qualifies sequential and random seeks at "<>show megabytes<>" MiB with bounded decoder work") $ \env -> boundedPhases $ \phase -> do
      -- One shared <=1 MiB input, no all-session raw value or decoder index.
      enabled <- getRTSStatsEnabled
      if enabled then pure () else pendingWith "Run memory qualification with +RTS -T"
      performGC
      before <- getRTSStats
      let chunk="\""<>BS.replicate 1048573 120<>"\"\n"
          chunks=replicate megabytes chunk
          total=fromIntegral megabytes*1048576
      (w,sealed,manifest) <- fixtureWithPhases phase env chunks
      phase "raw-reclamation"
      removeRaw env sealed.id
      phase "sequential-read/hash"
      bracket (R.openArchiveReader env.pool manifest) (\reader -> phase "sequential-decoder-cleanup" >> R.closeArchiveReader reader) $ \reader -> do
        let scan offset digest
              | offset == total = pure digest
              | otherwise = do
                  bytes <- R.readArchiveRange reader offset 65536
                  getPoolMetrics >>= (\m -> m.activeConnections `shouldBe` 0)
                  scan (offset+fromIntegral (BS.length bytes)) $! hashUpdate digest bytes
        digest <- scan 0 (hashInit :: Context SHA256)
        T.pack (show (hashFinalize digest :: Digest SHA256)) `shouldBe` manifest.rawSha256
        stats <- R.readerMetrics reader
        (stats.restores,stats.inflatedBytes,stats.verifiedBytes) `shouldBe` (1,total,total)
      phase "random-read/verify"
      bracket (R.openArchiveReader env.pool manifest) (\reader -> phase "random-decoder-cleanup" >> R.closeArchiveReader reader) $ \reader -> do
        forM_ [total-17,total `div` 2+3,1,total-65539] $ \offset -> do
          bytes <- R.readArchiveRange reader offset 17
          bytes `shouldBe` BS.take 17 (BS.drop (fromIntegral (offset `mod` 1048576)) chunk)
        stats <- R.readerMetrics reader
        stats.inflatedBytes `shouldSatisfy` (<=4*3145728)
      phase "heap-sample"
      performGC
      after <- getRTSStats
      max_live_bytes after `shouldSatisfy` (<= max (max_live_bytes before) (gcdetails_live_bytes (gc before)+134217728))

  it "bounds cache admission to four users and releases cancelled request ownership" $ \env -> bounded $ do
    (_,_,manifest) <- fixture env ["{}\n"]
    entered <- newEmptyMVar
    release <- newEmptyMVar
    bracket newReaderCache closeReaderCache $ \cache -> do
      let hold key=R.withCachedReader cache (ArchiveAudience key) env.pool manifest (\_ -> putMVar entered () >> readMVar release)
      withAsync (hold "one") $ \first -> do
        takeMVar entered
        withAsync (hold "two") $ \_ -> do
          takeMVar entered
          withAsync (hold "three") $ \_ -> do
            takeMVar entered
            withAsync (hold "four") $ \_ -> do
              takeMVar entered
              full <- try @ReaderCacheBusy $ R.withCachedReader cache (ArchiveAudience "fifth") env.pool manifest (const (pure ()))
              full `shouldSatisfy` either (const True) (const False)
              closing <- try @ReaderCacheBusy (closeReaderCache cache)
              closing `shouldSatisfy` either (const True) (const False)
              cancel first
              _ <- waitCatch first
              R.withCachedReader cache (ArchiveAudience "fifth") env.pool manifest (const (pure ()))
              putMVar release ()
    getPoolMetrics >>= (\m -> m.activeConnections `shouldBe` 0)

  it "keeps cached paging through small original chunks within the matching raw latency budget" $ \env -> bounded $ do
    let chunk="\""<>BS.replicate 1021 120<>"\"\n"
        offsets=[0,4096..2093056]
        timed action=do start<-getCurrentTime; value<-action; end<-getCurrentTime; pure (value,realToFrac (diffUTCTime end start)::Double)
        percentile values=sort values !! (length values*95 `div` 100)
    (w,sealed,manifest) <- fixture env (replicate 2048 chunk)
    rawTimes <- mapM (\offset -> snd <$> timed (readSessionRange env.pool w sealed.id offset 4096)) offsets
    removeRaw env sealed.id
    bracket newReaderCache closeReaderCache $ \cache -> do
      let audience=ArchiveAudience "resolved-account:small-chunk-credential"
      archivedTimes <- mapM (\offset -> do
        (page,elapsed) <- timed (readSessionEvidenceRangeWithCache cache audience env.pool w sealed.id offset 4096)
        page.bytes `shouldBe` BS.concat (replicate 4 chunk)
        pure elapsed) offsets
      percentile archivedTimes `shouldSatisfy` (<= max 0.5 (2*percentile rawTimes))
      stats <- R.withCachedReader cache audience env.pool manifest R.readerMetrics
      stats.restores `shouldBe` fromIntegral (length offsets)
      stats.verifiedBytes `shouldBe` 2097152
      stats.inflatedBytes `shouldSatisfy` (<= fromIntegral (length offsets)*1052672)

  it "keeps archived replay outside row locks and cancels source and decoder part reads" $ \env -> bounded $ do
    (w,sealed,manifest) <- fixture env ["{}\n"]
    removeRaw env sealed.id
    bracket (createPool env.testDb.testDbConnStr 1 30 30000) destroyAllResources $ \blockers -> withConn blockers $ \connection -> do
      let sql statement=S.run (S.sql statement) connection >>= either (fail . show) pure
          blocked queryPattern=runSession env.pool $ S.statement queryPattern $ St.Statement
            "SELECT EXISTS(SELECT 1 FROM pg_stat_activity WHERE wait_event_type='Lock' AND query LIKE $1)"
            (E.param (E.nonNullable E.text)) (D.singleRow (D.column (D.nonNullable D.bool))) False
          await queryPattern=do waiting<-blocked queryPattern; if waiting then pure () else threadDelay 10000 >> await queryPattern
          decode=bracket (R.openArchiveReader env.pool manifest) R.closeArchiveReader (\reader -> R.readArchiveRange reader 0 1) >> pure ()
          replay=appendSession env.pool w sealed.id (AppendSession 0 0 "{}\n" False) >> pure ()
          range=readSessionEvidenceRange env.pool w sealed.id 0 1 >> pure ()
      forM_ [("%JOIN session_archive_parts p ON p.generation_id=g.id%",decode),("%g.metadata_sha256=hmem_archive_binding%",replay),("%g.metadata_sha256=hmem_archive_binding%",range)] $ \(queryPattern,action) ->
        bracket_ (sql "BEGIN; LOCK session_archive_parts IN ACCESS EXCLUSIVE MODE") (sql "ROLLBACK") $
          withAsync action $ \reading -> do
            timeout 2000000 (await queryPattern) `shouldReturn` Just ()
            -- Only the blocker and current bounded query own pool leases.
            -- Archived replay and a short raw range have both released their
            -- transactions before resolving or decoding an immutable source.
            getPoolMetrics >>= (\m -> m.activeConnections `shouldBe` 2)
            locked <- runTransaction env.pool $ S.statement sealed.id $ St.Statement
              "SELECT id FROM sessions WHERE id=$1 FOR UPDATE NOWAIT"
              (E.param (E.nonNullable E.uuid)) (D.singleRow (D.column (D.nonNullable D.uuid))) True
            locked `shouldBe` sealed.id
            withAsync (cancel reading) $ \stopping -> do
              -- Release our blocker before either helper's finalizer can
              -- wait for a deferred driver cancellation on a failure path.
              within <- timeout 5000000 (waitCatch stopping) `finally` sql "ROLLBACK"
              target <- timeout 5000000 (waitCatch reading)
              stopper <- timeout 5000000 (waitCatch stopping)
              target `shouldSatisfy` maybe False (either (const True) (const False))
              stopper `shouldSatisfy` maybe False (either (const False) (const True))
              within `shouldSatisfy` maybe False (either (const False) (const True))
    getPoolMetrics >>= (\m -> m.activeConnections `shouldBe` 0)
    readSessionRange env.pool w sealed.id 0 1 >>= (\r -> r.bytes `shouldBe` "{")

  it "keeps a READ COMMITTED metadata/raw race complete when reclamation wins the table lock" $ \env -> bounded $ do
    (w,sealed,_) <- fixture env ["{}\r\n[]"]
    bracket (createPool env.testDb.testDbConnStr 1 30 30000) destroyAllResources $ \blockers -> withConn blockers $ \connection -> do
      let sql statement=S.run (S.sql statement) connection >>= either (fail . show) pure
          blocked=runSession env.pool $ S.statement () $ St.Statement
            "SELECT EXISTS(SELECT 1 FROM pg_stat_activity WHERE wait_event_type='Lock' AND query LIKE 'SELECT coalesce(string_agg(substring(data%')"
            E.noParams (D.singleRow (D.column (D.nonNullable D.bool))) False
          await=do waiting<-blocked; if waiting then pure () else threadDelay 10000 >> await
      bracket_ (sql "BEGIN; LOCK session_chunks IN ACCESS EXCLUSIVE MODE") (sql "ROLLBACK") $
        withAsync (readSessionEvidenceRange env.pool w sealed.id 1 1048576) $ \reading -> do
          timeout 2000000 await `shouldReturn` Just ()
          sql ("ALTER TABLE session_chunks DISABLE TRIGGER session_chunk_guard; DELETE FROM session_chunks WHERE session_id='"<>uuid sealed.id<>"'; ALTER TABLE session_chunks ENABLE TRIGGER session_chunk_guard; COMMIT")
          result <- timeout 5000000 (wait reading)
          fmap (.bytes) result `shouldBe` Just "}\r\n[]"
    readSessionRange env.pool w sealed.id 0 1048576 >>= (\r -> r.bytes `shouldBe` "{}\r\n[]")

  it "restores archive read limits on the same backend after commit and rollback and safely reuses the pool" $ \env -> bounded $ do
    (w,sealed,manifest) <- fixture env ["{}\n"]
    let settings=St.Statement "SELECT pg_backend_pid(),current_setting('lock_timeout'),current_setting('statement_timeout')" E.noParams
          (D.singleRow ((,,) <$> D.column (D.nonNullable D.int4) <*> D.column (D.nonNullable D.text) <*> D.column (D.nonNullable D.text))) False
        sample pool=runSession pool (S.statement () settings)
        need result=either (fail . show) pure result
    bracket (createPool env.testDb.testDbConnStr 1 30 500) destroyAllResources $ \single -> do
      runSession single $ S.sql "SET lock_timeout='250ms'"
      before@(pid,_,_) <- sample single
      R.verifiedManifest single w sealed.id `shouldReturn` Just manifest
      sample single `shouldReturn` before
      withConn single $ \connection -> do
        inside <- S.run (S.sql "BEGIN" >> R.archiveReadSession () settings) connection >>= need
        inside `shouldBe` (pid,"250ms","500ms")
        S.run (S.sql "COMMIT") connection >>= need
        S.run (S.statement () settings) connection >>= need >>= (`shouldBe` before)
        failed <- S.run (S.sql "BEGIN" >> R.archiveReadSession () (St.Statement "SELECT 1/0" E.noParams (D.singleRow (D.column (D.nonNullable D.int4))) False)) connection
        failed `shouldSatisfy` either (const True) (const False)
        S.run (S.sql "ROLLBACK") connection >>= need
        S.run (S.statement () settings) connection >>= need >>= (`shouldBe` before)
      -- Exercise the public guard's actual error path as well. resource-pool
      -- may discard an exceptional backend; either reuse must be restored or
      -- its replacement must have the pool's configured baseline.
      bracket (createPool env.testDb.testDbConnStr 1 30 30000) destroyAllResources $ \blockers -> withConn blockers $ \connection -> do
        let sql statement=S.run (S.sql statement) connection >>= need
        bracket_ (sql "BEGIN; LOCK session_archive_parts IN ACCESS EXCLUSIVE MODE") (sql "ROLLBACK") $ do
          failed <- try @R.ArchiveReadFailure (R.verifiedManifest single w sealed.id)
          failed `shouldSatisfy` either (const True) (const False)
      after@(next,lockLimit,statementLimit) <- sample single
      if next == pid then after `shouldBe` before else do
        (lockLimit,statementLimit) `shouldBe` ("0","500ms")
        gone <- runSession env.pool $ S.statement pid $ St.Statement
          "SELECT NOT EXISTS(SELECT 1 FROM pg_stat_activity WHERE pid=$1)"
          (E.param (E.nonNullable E.int4)) (D.singleRow (D.column (D.nonNullable D.bool))) False
        gone `shouldBe` True
      R.verifiedManifest single w sealed.id `shouldReturn` Just manifest
    getPoolMetrics >>= (\m -> m.activeConnections `shouldBe` 0)


bounded :: IO () -> IO ()
bounded action = timeout 180000000 action >>= (`shouldBe` Just ())

-- Only a bounded phase receipt is emitted on an unresolved timeout; ordinary
-- success retains no journal, raw log or fixture copy.
boundedPhases :: ((String -> IO ()) -> IO ()) -> IO ()
boundedPhases action = do
  start <- getCurrentTime
  receipt <- newIORef ("setup",start,[] :: [(String,Double)])
  let phase label=do
        now <- getCurrentTime
        (previous,began,completed) <- readIORef receipt
        writeIORef receipt (label,now,take 16 ((previous,realToFrac (diffUTCTime now began)):completed))
  result <- timeout 180000000 (action phase)
  case result of
    Just () -> pure ()
    Nothing -> do
      now <- getCurrentTime
      (label,began,completed) <- readIORef receipt
      expectationFailure ("Reader 180s deadline; completed phases="<>show (reverse completed)<>"; current="<>show (label,realToFrac (diffUTCTime now began)::Double))

fixture :: TestEnv -> [ByteString] -> IO (UUID,AgentSession,A.ArchiveManifest)
fixture = fixtureWithPhases (const (pure ()))

fixtureWithPhases :: (String -> IO ()) -> TestEnv -> [ByteString] -> IO (UUID,AgentSession,A.ArchiveManifest)
fixtureWithPhases phase env chunks = do
  phase "fixture/session-and-append"
  w <- (.id) <$> createTestWorkspace env "compatible-archive-reader"
  task <- Task.createTask env.pool (Types.CreateTask w Nothing Nothing "Reader fixture" Nothing Nothing Nothing)
  runSession env.pool $ S.sql ("UPDATE tasks SET status='in_progress' WHERE id='"<>uuid task.id<>"'")
  identifier <- UUID.nextRandom
  initial <- createSession env.pool (CreateSession identifier w "jsonl" "Reader fixture" Nothing Nothing task.id)
  row <- foldM (\previous bytes -> (.session) <$> appendSession env.pool w identifier
    (AppendSession previous.committedBytes previous.revision bytes (not (BS.isSuffixOf "\n" bytes)))) initial chunks
  let expected=T.pack (show (hashFinalize (foldl' hashUpdate (hashInit :: Context SHA256) chunks) :: Digest SHA256))
  phase "fixture/seal-hash"
  sealed <- terminateSession env.pool w identifier (TerminateSession row.committedBytes row.revision row.committedBytes expected)
  phase "fixture/claim"
  R.ensureArchive env.pool w identifier
  implementation <- C.codecImplementation
  Just lease <- A.claimArchive env.pool implementation
  renewed <- getCurrentTime >>= newIORef
  let keepAlive = do
        now <- getCurrentTime
        previous <- readIORef renewed
        if diffUTCTime now previous < 10 then pure () else do
          A.renewArchive env.pool lease `shouldReturn` True
          writeIORef renewed now
  input <- sourceOf chunks
  partState <- newIORef (0,0)
  phase "fixture/compress-stage"
  encoded <- C.compress input $ \bytes -> do
    keepAlive
    (ordinal,offset) <- readIORef partState
    A.stageArchivePart env.pool lease ordinal offset bytes `shouldReturn` True
    writeIORef partState (ordinal+1,offset+fromIntegral (BS.length bytes))
  (parts,_) <- readIORef partState
  nextPart <- newIORef 0
  let source = do
        keepAlive
        ordinal <- readIORef nextPart
        if ordinal == parts then pure Nothing else do
          Just part <- A.readStagedPart env.pool lease ordinal
          writeIORef nextPart (ordinal+1)
          pure (Just part.bytes)
  nextPoint <- newIORef 0
  phase "fixture/readback-checkpoints"
  verified <- C.verify row.committedBytes source (const (pure ())) $ \point -> do
    keepAlive
    ordinal <- readIORef nextPoint
    A.stageArchiveCheckpoint env.pool lease ordinal point.rawOffset point.compressedOffset (fromIntegral point.unusedBits) point.dictionary `shouldReturn` True
    writeIORef nextPoint (ordinal+1)
  verified.raw `shouldBe` encoded.raw
  verified.compressed `shouldBe` encoded.compressed
  let copyLedger cursor = (keepAlive >> A.stageArchiveLedger env.pool lease cursor 8) >>= \case
        Just next | next > cursor -> copyLedger next
        Just _ -> pure ()
        Nothing -> expectationFailure "ledger lease lost"
  phase "fixture/ledger"
  copyLedger 0
  phase "fixture/publish"
  A.publishArchive env.pool lease (A.ArchiveVerification encoded.compressed.byteCount encoded.compressed.sha256 parts verified.checkpoints (fromIntegral (length chunks))) `shouldReturn` True
  Just manifest <- R.verifiedManifest env.pool w identifier
  pure (w,sealed,manifest)

sourceOf :: [ByteString] -> IO (IO (Maybe ByteString))
sourceOf values = do
  ref <- newIORef values
  pure $ atomicModifyIORef' ref $ \case [] -> ([],Nothing); x:xs -> (xs,Just x)

-- Private fixture authority is limited to the known test connection and exact
-- owned session. No caller-settable production reclamation bypass is added.
removeRaw :: TestEnv -> UUID -> IO ()
removeRaw env session = runTransaction env.pool $ S.sql $
  "ALTER TABLE session_chunks DISABLE TRIGGER session_chunk_guard; DELETE FROM session_chunks c WHERE session_id='"<>uuid session<>"' AND EXISTS(SELECT 1 FROM session_archives g WHERE g.session_id=c.session_id); ALTER TABLE session_chunks ENABLE TRIGGER session_chunk_guard"
privateMutation :: TestEnv -> ByteString -> ByteString -> IO ()
privateMutation env table sql = runTransaction env.pool $ S.sql $
  "ALTER TABLE "<>table<>" DISABLE TRIGGER session_archive_stage_guard; "<>sql<>"; ALTER TABLE "<>table<>" ENABLE TRIGGER session_archive_stage_guard"
uuid :: UUID -> ByteString
uuid = B8.pack . show
