{-# LANGUAGE BangPatterns #-}
-- Integrated fixtures own their databases, native helpers and compact receipts.
module HMem.Server.Archive.MaintenanceQualificationSpec
  ( spec, Owned(..), acquire, drain, pgTool, fixture, assertPaths ) where

import Codec.Compression.Zlib.Raw qualified as Z
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (concurrently)
import Control.Exception
import Control.Monad (foldM,forM_,unless,void,when)
import Crypto.Hash (Context,SHA256,Digest,hashInit,hashUpdate,hashFinalize,hashlazy)
import Data.Aeson
import Data.Aeson.KeyMap qualified as KM
import Data.Bits (xor,shiftL,shiftR,(.&.),(.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as B8
import Data.ByteString.Lazy qualified as LBS
import Data.Foldable (foldl')
import Data.Int (Int64)
import Data.IORef
import Data.Pool (Pool,destroyAllResources)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.UUID (UUID)
import Data.UUID.V4 qualified as UUID
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import Hasql.Connection qualified as H
import Hasql.Decoders qualified as D
import Hasql.Encoders qualified as E
import Hasql.Session qualified as S
import Hasql.Statement qualified as St
import Network.HTTP.Types
import Network.Wai qualified as Wai
import Network.Wai.Test qualified as W
import System.CPUTime (getCPUTime)
import System.Directory (createDirectoryIfMissing,getFileSize,removeFile)
import System.Environment (getExecutablePath)
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.Process
import System.IO (withBinaryFile,IOMode(..))
import System.Timeout (timeout)
import Test.Hspec

import HMem.AgentSession qualified as Core
import HMem.AgentSession.Transport qualified as Wire
import HMem.Config qualified
import HMem.DB.AgentSession qualified as DB
import HMem.DB.Pool qualified as P
import HMem.DB.SessionArchive qualified as A
import HMem.DB.SessionArchive.Maintenance qualified as M
import HMem.DB.SessionArchive.Reader qualified as R
import HMem.DB.Task qualified as Task
import HMem.DB.TestHarness
import HMem.Server.AccessTracker (newAccessTracker)
import HMem.Server.App (mkAppWithPrefixProofs)
import HMem.Server.Archive.Worker
import HMem.Server.Archive.Maintenance (verifyPublishedArchive)
import HMem.Server.SessionPrefix (newPrefixRegistry,closePrefixRegistry)
import HMem.Server.TestHarness (mkLocalSandboxConfig)
import HMem.Server.WebSocket (newWSState)
import HMem.SessionArchive.Codec qualified as C
import HMem.Types qualified as Types

spec :: Spec
spec=describe "Archive maintenance qualification" $ do
  it "restores a bounded two MiB recovery smoke fixture" $ qualify "entropy" 2
  it "measures and restores one GiB of compressible valid JSONL" $ qualify "compressible" 1024
  it "measures and restores one hundred MiB of seeded poorly compressible valid JSONL" $ qualify "entropy" 100

-- Native helpers and the replacement pool/database remain rooted until their
-- actual handles join; a failed drain is retained by the existing lifetime.
data Owned = Owned
  { helper :: IORef (Maybe ProcessHandle), replacement :: IORef (Maybe Text)
  , restoredPool :: IORef (Maybe (Pool H.Connection)), runtimePool :: Pool H.Connection
  , runtimeRole :: Text, adminEnv :: TestEnv, helperReceipts :: IORef [Value] }

acquire :: TestEnv -> IO Owned
acquire env=mask_ $ do
  identifier<-UUID.nextRandom
  let role="archive_qual_"<>T.filter (/='-') (T.pack (show identifier))
  execute env (TE.encodeUtf8 ("CREATE ROLE "<>role<>" LOGIN NOINHERIT NOSUPERUSER NOCREATEDB NOCREATEROLE NOREPLICATION NOBYPASSRLS; GRANT USAGE ON SCHEMA public TO "<>role<>"; GRANT SELECT,INSERT,UPDATE,DELETE ON ALL TABLES IN SCHEMA public TO "<>role<>"; GRANT EXECUTE ON ALL FUNCTIONS IN SCHEMA public TO "<>role))
  pool<-P.createPool (env.testDb.testDbConnStr<>" user="<>role) 6 30 30000
    `onException` execute env (TE.encodeUtf8 ("DROP OWNED BY "<>role<>"; DROP ROLE "<>role))
  Owned <$> newIORef Nothing <*> newIORef Nothing <*> newIORef Nothing <*> pure pool <*> pure role <*> pure env <*> newIORef []

drain :: Owned -> IO ()
drain owned=do
  let admin=owned.adminEnv
  pending<-readIORef owned.helper
  forM_ pending $ \handle -> do
    exited<-getProcessExitCode handle
    when (exited==Nothing) (terminateProcess handle)
    joined<-timeout 5000000 (waitForProcess handle)
    unless (joined/=Nothing) (fail "Qualification helper did not join; sandbox retained")
    writeIORef owned.helper Nothing
  readIORef owned.restoredPool >>= mapM_ destroyAllResources
  writeIORef owned.restoredPool Nothing
  readIORef owned.replacement >>= mapM_ (\name -> rawExecute admin (TE.encodeUtf8 ("DROP DATABASE "<>name)))
  writeIORef owned.replacement Nothing
  destroyAllResources owned.runtimePool
  execute admin (TE.encodeUtf8 ("DROP OWNED BY "<>owned.runtimeRole<>"; DROP ROLE "<>owned.runtimeRole))

pgTool :: Owned -> String -> [String] -> IO ()
pgTool owned tool args=do
  begin<-getMonotonicTimeNSec
  let diagnostic=owned.adminEnv.testSandbox.sandboxRoot </> "pg-tool-error.txt"
  result<-withBinaryFile diagnostic WriteMode $ \errors -> mask $ \restore -> do
    (_,_,_,handle)<-createProcess (proc tool args) {std_in=NoStream,std_out=NoStream,std_err=UseHandle errors,create_new_console=False}
    writeIORef owned.helper (Just handle)
    result<-restore (timeout 60000000 (waitForProcess handle))
    when (result/=Nothing) (writeIORef owned.helper Nothing)
    pure result
  finish<-getMonotonicTimeNSec
  let elapsed=show (fromIntegral (finish-begin)/1000000000::Double)
  case result of
    Nothing -> fail (tool<>" deadline exceeded after "<>elapsed<>"s; actual handle retained for lifetime drain")
    Just code -> do
      modifyIORef' owned.helperReceipts (<>[object ["tool".=tool,"exit_code".=show code,"elapsed_seconds".=(fromIntegral (finish-begin)/1000000000::Double),"actual_helper_joined".=True]])
      detail<-withBinaryFile diagnostic ReadMode (\handle -> BS.hGetSome handle 8192)
      removeFile diagnostic
      unless (code==ExitSuccess) $ fail (tool<>" failed: "<>show code<>" after "<>elapsed<>"s; first <=8192 diagnostic bytes: "<>B8.unpack detail)

qualify :: Text -> Int -> IO ()
qualify recipe chunkCount=do
  life<-newSandboxLifetime
  runId<-UUID.nextRandom
  completed<-newIORef Nothing
  phases<-newIORef []
  phaseStart<-getMonotonicTimeNSec
  currentPhase<-newIORef ("setup",phaseStart)
  repoRoot<-resolveRepoRoot
  let large=chunkCount>=1024
      recordPhase :: Text -> IO a -> IO a
      recordPhase name action=do
        start<-getMonotonicTimeNSec
        writeIORef currentPhase (name,start)
        result<-try @SomeException action
        finish<-getMonotonicTimeNSec
        modifyIORef' phases (<>[object ["phase".=name,"elapsed_seconds".=(fromIntegral (finish-start)/1000000000::Double),"outcome".=(either (const "failed") (const "success") result::Text)]])
        either throwIO pure result
      failureReceipt cause=do
        now<-getMonotonicTimeNSec
        (name,start)<-readIORef currentPhase
        measured<-readIORef phases
        retained<-sandboxLifetimeFailures life
        let path=repoRoot </> ".scratch" </> "sealed-archives-8ff1e216" </> "failed-"<>T.unpack recipe<>"-"<>show chunkCount<>"MiB-"<>show runId<>".json"
        createDirectoryIfMissing True (repoRoot </> ".scratch" </> "sealed-archives-8ff1e216")
        LBS.writeFile path (encode (object ["owner".=("Task7"::Text),"expiry".=("Resolve diagnostic at issue resolution/task closure"::Text),"run_id".=runId,"recipe".=recipe,"chunks".=chunkCount,"failed_phase".=name,"observed_phase_seconds".=(fromIntegral (now-start)/1000000000::Double),"completed_phases".=measured,"cause".=T.take 8192 cause,"retained_lifetime_failures".=retained]))
  attempt<-try @SomeException $ timeout 250000000 $ withSandboxedTestEnvBorrowed life acquire drain $ \admin owned -> do
    setupEnd<-getMonotonicTimeNSec
    modifyIORef' phases (<>[object ["phase".=("setup"::Text),"elapsed_seconds".=(fromIntegral (setupEnd-phaseStart)/1000000000::Double),"outcome".=("success"::Text)]])
    let env=admin {pool=owned.runtimePool}
        rawBytes=fromIntegral chunkCount*1048576::Int64
        receiptPath=admin.testSandbox.sandboxRepoRoot </> ".scratch" </> "sealed-archives-8ff1e216" </> T.unpack recipe<>"-"<>show chunkCount<>"MiB-"<>show runId<>".json"
    begin<-getMonotonicTimeNSec
    cpuBegin<-getCPUTime
    lsnStart<-text admin "SELECT pg_current_wal_insert_lsn()::text"
    (w,sealed,task)<-recordPhase "append_and_seal" (fixture env recipe chunkCount)
    baseline<-snapshot admin
    lsnRaw<-text admin "SELECT pg_current_wal_insert_lsn()::text"
    R.ensureArchive env.pool w sealed.id
    implementation<-C.codecImplementation
    Just compression<-A.claimArchive env.pool implementation
    staging<-newIORef Null
    recordPhase "compress_stored_verify_publish" (runArchiveLeaseWith (snapshot admin >>= writeIORef staging) env.pool compression)
    ready<-snapshot admin
    manifest<-recordPhase "publication_ready" $ A.getPublishedArchive env.pool w sealed.id >>= \case
      Just value -> pure value
      Nothing -> A.getArchiveJob env.pool w sealed.id >>= fail . ("Archive publication failed: "<>) . show
    peakLogical<-number admin ("SELECT "<>B8.pack (show (manifest.compressedBytes+manifest.partCount*512+manifest.checkpointCount*512+manifest.ledgerCount*1024))<>"+coalesce(sum(octet_length(dictionary)),0)::bigint FROM session_archive_checkpoints WHERE generation_id='"<>uuid manifest.id<>"'")
    lsnReady<-text admin "SELECT pg_current_wal_insert_lsn()::text"
    M.configureMaintenance env.pool M.EnableReclamation
    recordPhase "first_independent_verify_and_delete_batch" (maintainArchive env.pool)
    -- Proof/range/ZIP readers run while a second bounded deletion commits.
    recordPhase "partial_cleanup_concurrent_http" $ withApp env $ \app -> void $ concurrently
      (if large then assertBoundedPaths app recipe chunkCount w sealed else assertPaths app env recipe chunkCount w sealed task manifest)
      (maintainArchive env.pool)
    let clean=do
          remaining<-number env "SELECT count(*) FROM session_chunks"
          when (remaining>0) $ do
            maintainArchive env.pool
            after<-number env "SELECT count(*) FROM session_chunks"
            unless (after<remaining) $ M.maintenanceStatus env.pool >>= fail . ("Reclamation did not advance: "<>) . show
            clean
    recordPhase "remaining_cleanup" clean
    number env "SELECT count(*) FROM session_archive_reclamation WHERE complete" `shouldReturn` 1
    removed<-snapshot admin
    lsnRemoved<-text admin "SELECT pg_current_wal_insert_lsn()::text"
    -- Snapshot AFTER raw reclamation; restoring a pre-cleanup snapshot would
    -- not exercise archive-only recovery.
    let dump=admin.testSandbox.sandboxRoot </> "after-reclaim.dump"
    recordPhase "backup_after_reclaim" (pgTool owned "pg_dump" ["--format=custom","--file",dump,"--dbname",T.unpack admin.testDb.testDbConnStr])
    backupBytes<-getFileSize dump
    replacementId<-UUID.nextRandom
    let replacement="archive_restore_"<>T.filter (/='-') (T.pack (show replacementId))
        adminConn=admin.testDb.testDbConnStr<>" dbname="<>replacement
    mask_ $ rawExecute admin (TE.encodeUtf8 ("CREATE DATABASE "<>replacement<>" TEMPLATE template0")) >> writeIORef owned.replacement (Just replacement)
    recordPhase "restore_empty_replacement" (pgTool owned "pg_restore" ["--exit-on-error","--dbname",T.unpack adminConn,dump])
    restored<-mask_ $ do
      pool<-P.createPool (adminConn<>" user="<>owned.runtimeRole) 6 30 30000
      writeIORef owned.restoredPool (Just pool)
      pure pool
    let replacementEnv=env {pool=restored}
    number replacementEnv "SELECT count(*) FROM session_chunks" `shouldReturn` 0
    recordPhase "post_restore_verification" $
      if large then do
        verifyPublishedArchive restored manifest
        withApp replacementEnv $ \app -> assertBoundedPaths app recipe chunkCount w sealed
      else withApp replacementEnv $ \app -> assertPaths app replacementEnv recipe chunkCount w sealed task manifest
    -- Release replacement consumers before dropping its database, still inside
    -- the same owned lifetime; the final drain retries these exact handles.
    destroyAllResources restored
    writeIORef owned.restoredPool Nothing
    rawExecute admin (TE.encodeUtf8 ("DROP DATABASE "<>replacement))
    writeIORef owned.replacement Nothing
    removeFile dump
    recordPhase "ordinary_vacuum" (rawExecute admin "VACUUM session_chunks")
    vacuumed<-snapshot admin
    -- The 100-MiB case refills to observe page reuse without promising OS file
    -- shrink. The one-GiB case explicitly records no refill.
    reused<-if large then pure Null else recordPhase "same_input_refill" (fixture env recipe chunkCount >> snapshot admin)
    finish<-getMonotonicTimeNSec
    cpuFinish<-getCPUTime
    walRaw<-walDelta admin lsnStart lsnRaw
    walArchive<-walDelta admin lsnRaw lsnReady
    walDelete<-walDelta admin lsnReady lsnRemoved
    staged<-readIORef staging
    executable<-getExecutablePath
    executableHash<-fileHash executable
    sourceHashes<-mapM (\path -> do digest<-fileHash (admin.testSandbox.sandboxRepoRoot </> path); pure (object ["path".=path,"sha256".=digest]))
      ["hmem-server/test/HMem/Server/Archive/MaintenanceQualificationSpec.hs","hmem-server/migrations/V051__session_archive_maintenance.sql","hmem-core/src/HMem/DB/SessionArchive.hs","hmem-core/src/HMem/DB/SessionArchive/Maintenance.hs","hmem-server/src/HMem/Server/Archive/Maintenance.hs","hmem-server/src/HMem/Server/Archive/Worker.hs"]
    pgVersion<-text admin "SELECT version()"
    helpers<-readIORef owned.helperReceipts
    measured<-readIORef phases
    createDirectoryIfMissing True (admin.testSandbox.sandboxRepoRoot </> ".scratch" </> "sealed-archives-8ff1e216")
    let receipt=object
          ["format_version".=(1::Int),"owner".=("Task7 until verified handoff, then Task8"::Text),"expiry".=("Task8 aggregate reconciliation/closure"::Text),"run_id".=runId
          ,"base_revision".=("b7428aa960d0b526fb85e33146f36374f3b8b109"::Text),"recipe".=recipe,"seed".=(1::Int),"record_bytes".=(1024::Int),"append_bytes".=(1048576::Int)
          ,"record_count".=sealed.committedRecords,"raw_bytes".=rawBytes,"raw_sha256".=sealed.finalSha256,"compressed_bytes".=manifest.compressedBytes,"compressed_sha256".=manifest.compressedSha256
          ,"part_count".=manifest.partCount,"checkpoint_count".=manifest.checkpointCount,"ledger_count".=manifest.ledgerCount
          ,"postgres".=pgVersion,"executable_sha256".=executableHash,"sources".=sourceHashes
          ,"baseline".=baseline,"peak_staging_logical_allowance_from_immutable_counts".=peakLogical,"after_compression_sampled_relations".=staged,"ready_plus_raw".=ready,"after_delete".=removed,"after_vacuum".=vacuumed,"after_refill".=reused
          ,"wal_generated_raw".=walRaw,"wal_generated_archive".=walArchive,"wal_generated_delete".=walDelete,"post_reclaim_backup_bytes".=backupBytes
          ,"native_helpers".=helpers,"fixture_cleanup".=("All recorded native CLI helpers joined; supported BorrowedSandboxLifetime returned before receipt publication. This does not certify the deferred general test supervisor."::Text)
          ,"phases".=measured,"coverage".=(if large then "one_GiB_storage_fidelity_bounded_HTTP" else "full_HTTP_compatibility"::Text)
          ,"codec_implementation".=manifest.codecImplementation,"codec_parameters".=manifest.codecParameters
          ,"elapsed_seconds".=(fromIntegral (finish-begin)/1000000000::Double),"application_cpu_seconds_excluding_postgres_helpers".=(fromIntegral (cpuFinish-cpuBegin)/1000000000000::Double)
          ,"checks".=(if large then (["single-member stored verification","partial-cleanup concurrent bounded HTTP ranges/exact replay","post-cleanup backup","empty replacement restore","one advancing whole stored compressed/raw receipt verification after restore","archive-only bounded HTTP ranges/exact replay","ordinary vacuum"]::[Text]) else ["single-member stored verification",if chunkCount>8 then "partial-cleanup concurrent ranges/proofs/replay/ZIP/content" else "archive-only concurrent ranges/proofs/replay/ZIP/content","post-cleanup backup","empty replacement restore","archive-only ranges/proofs/replay/ZIP/content","ordinary vacuum and same-input refill"])
          ,"limitations".=((["synthetic valid JSONL","sampled relation/retained-WAL sizes are not continuous OS high-water marks","configured capacity is not measured free disk","rollback to a distinct prior binary requires separate provenance and run","no browser or production deployment qualification"]<>if large then ["One-GiB proof/ZIP/content matrix and refill are not performed here; separate 100-MiB local evidence and Task8 final matrix retain their scopes"] else [])::[Text])]
    writeIORef completed (Just (receiptPath,receipt))
    cleanupStart<-getMonotonicTimeNSec
    writeIORef currentPhase ("fixture_cleanup",cleanupStart)
  case attempt of
    Right (Just ()) -> do
      Just (path,receipt)<-readIORef completed
      (_,cleanupStart)<-readIORef currentPhase
      cleanupEnd<-getMonotonicTimeNSec
      let final=case receipt of
            Object fields -> Object (KM.insert "supported_fixture_cleanup_elapsed_seconds" (toJSON (fromIntegral (cleanupEnd-cleanupStart)/1000000000::Double)) fields)
            _ -> receipt
      LBS.writeFile path (encode final)
    Right Nothing -> failureReceipt "Qualification exceeded 250-second case budget" >> retrySandboxLifetime life >> expectationFailure "Qualification exceeded 250-second case budget"
    Left failure -> failureReceipt (T.pack (displayException failure)) >> throwIO failure

fixture :: TestEnv -> Text -> Int -> IO (UUID,Core.AgentSession,UUID)
fixture env recipe count=do
  w<-(.id) <$> createTestWorkspace env "archive-qualification"
  task<-Task.createTask env.pool (Types.CreateTask w Nothing Nothing "Qualification fixture" Nothing Nothing Nothing)
  execute env ("UPDATE tasks SET status='in_progress' WHERE id='"<>uuid task.id<>"'")
  identifier<-UUID.nextRandom
  initial<-DB.createSession env.pool (Core.CreateSession identifier w "jsonl" "Qualification fixture" Nothing Nothing task.id)
  (uploaded,context)<-foldM (\(row,context) ordinal -> do
    let bytes=chunk recipe ordinal
    next<-(.session) <$> DB.appendSession env.pool w identifier (Core.AppendSession row.committedBytes row.revision bytes True)
    let !nextContext=hashUpdate context bytes
    pure(next,nextContext)) (initial,hashInit::Context SHA256) [0..count-1]
  sealed<-DB.terminateSession env.pool w identifier (Core.TerminateSession uploaded.committedBytes uploaded.revision uploaded.committedBytes (T.pack (show (hashFinalize context::Digest SHA256))))
  pure(w,sealed,task.id)

-- Exactly 1024-byte valid JSON records; CRLF is part of each raw record.
chunk :: Text -> Int -> ByteString
chunk recipe ordinal=BS.concat [record (ordinal*1024+n) | n<-[0..1023]]
  where
    record n=
      let prefix="{\"n\":"<>B8.pack (show n)<>",\"s\":\""
          suffix="\"}\r\n"
          count=1024-BS.length prefix-BS.length suffix
          payload=if recipe=="entropy" then entropy (fromIntegral n+1) count else BS.replicate count 120
      in prefix<>payload<>suffix
entropy :: Word64 -> Int -> ByteString
entropy seed count=fst $ BS.unfoldrN count (\s -> let a=s `xor` (s `shiftL` 13); b=a `xor` (a `shiftR` 7); next=b `xor` (b `shiftL` 17)
  in Just(BS.index "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_" (fromIntegral (next .&. 63)),next)) seed

withApp :: TestEnv -> (Wai.Application -> IO a) -> IO a
withApp env use=bracket (newPrefixRegistry env.pool) closePrefixRegistry $ \registry -> do
  let cfg=mkLocalSandboxConfig env
  tracker<-newAccessTracker env.pool 3600
  ws<-newWSState
  app<-mkAppWithPrefixProofs registry Nothing cfg.changeStream id cfg.auth cfg.cors cfg.rateLimit env.pool tracker ws cfg.web.webStaticDir True
  use app

assertPaths :: Wai.Application -> TestEnv -> Text -> Int -> UUID -> Core.AgentSession -> UUID -> A.ArchiveManifest -> IO ()
assertPaths app env recipe count workspace sealed task manifest=do
  let target="/api/v1/workspaces/"<>uuid workspace<>"/sessions/"<>uuid sealed.id
      proofPath=target<>"/prefix-verifications"
      expected offset limit=let first=fromIntegral (offset `div` 1048576); within=fromIntegral (offset `mod` 1048576)
        in BS.take limit (BS.drop within (BS.concat [chunk recipe n | n<-[first..min (count-1) (first+1)]]))
  forM_ [0,1,1048575,sealed.committedBytes-1,sealed.committedBytes] $ \offset -> do
    response<-request app methodGet (target<>"/evidence?offset="<>B8.pack (show offset)<>"&limit=65536") "" >>= decoded @Wire.SessionRangeResponse
    Wire.decodeSessionBytes response.dataBase64 `shouldBe` Right (expected offset 65536)
  forM_ [(1048576,1,Core.sessionSha256 (chunk recipe 0)),(sealed.committedBytes,sealed.revision-1,maybe "" id sealed.finalSha256)] $ \(end,revision,digest) -> do
    op<-UUID.nextRandom
    _<-request app methodPost proofPath (encode (Wire.PrefixProofRequest op end revision digest)) >>= decoded @Wire.PrefixProofResponse
    result<-timeout 90000000 (await app (proofPath<>"/"<>uuid op))
    fmap (.state) result `shouldBe` Just "verified"
  replay<-request app methodPost (target<>"/append") (encode (Wire.AppendSessionRequest 0 0 (Wire.encodeSessionBytes (chunk recipe 0)) True)) >>= decoded @Wire.AppendSessionResponse
  replay.replayed `shouldBe` True
  zipResponse<-request app methodGet ("/api/v1/workspaces/"<>uuid workspace<>"/tasks/"<>uuid task<>"/evidence.zip") ""
  W.simpleStatus zipResponse `shouldBe` status200
  let bytes=W.simpleBody zipResponse
      little at width=foldr (\n acc -> acc `shiftL` 8 .|. fromIntegral (LBS.index bytes (at+n))) (0::Word64) [0..width-1]
      start=30+fromIntegral (little 26 2)+fromIntegral (little 28 2)
      central=fromIntegral (little (LBS.length bytes-98+48) 8)
      compressed=LBS.take (central-24-start) (LBS.drop start bytes)
      (!digest,!lengthBytes)=foldl' (\(!context,!size) piece -> (hashUpdate context piece,size+fromIntegral (BS.length piece))) (hashInit::Context SHA256,0::Int64) (LBS.toChunks (Z.decompress compressed))
  (lengthBytes,T.pack (show (hashFinalize digest::Digest SHA256))) `shouldBe` (sealed.committedBytes,maybe "" id sealed.finalSha256)
  content<-request app methodGet (target<>"/archive/"<>uuid manifest.id<>"/content") ""
  W.simpleStatus content `shouldBe` status200
  (LBS.length content.simpleBody,T.pack (show (hashlazy content.simpleBody::Digest SHA256))) `shouldBe` (manifest.compressedBytes,manifest.compressedSha256)
  DB.getSession env.pool workspace sealed.id `shouldReturn` Just sealed

-- The one-GiB case isolates storage/recovery cost. Complete proof/ZIP/content
-- concurrency is measured by the separate 100-MiB case, then Task8's matrix.
assertBoundedPaths :: Wai.Application -> Text -> Int -> UUID -> Core.AgentSession -> IO ()
assertBoundedPaths app recipe count workspace sealed=do
  let target="/api/v1/workspaces/"<>uuid workspace<>"/sessions/"<>uuid sealed.id
      expected offset=let first=fromIntegral (offset `div` 1048576); within=fromIntegral (offset `mod` 1048576)
        in BS.take 65536 (BS.drop within (BS.concat [chunk recipe n | n<-[first..min (count-1) (first+1)]]))
  forM_ [0,1,1048575,sealed.committedBytes-1,sealed.committedBytes] $ \offset -> do
    response<-request app methodGet (target<>"/evidence?offset="<>B8.pack (show offset)<>"&limit=65536") "" >>= decoded @Wire.SessionRangeResponse
    Wire.decodeSessionBytes response.dataBase64 `shouldBe` Right (expected offset)
  replay<-request app methodPost (target<>"/append") (encode (Wire.AppendSessionRequest 0 0 (Wire.encodeSessionBytes (chunk recipe 0)) True)) >>= decoded @Wire.AppendSessionResponse
  replay.replayed `shouldBe` True

request :: Wai.Application -> Method -> ByteString -> LBS.ByteString -> IO W.SResponse
request app method path body=W.runSession (W.srequest (W.SRequest ((W.setPath Wai.defaultRequest path)
  {Wai.requestMethod=method,Wai.requestHeaders=[(hContentType,"application/json"),("X-HMem-Bot-Identity","{\"role\":\"Qualification\",\"model-id\":\"test:fixture:default\"}")]}) body)) app
decoded :: FromJSON a => W.SResponse -> IO a
decoded response=do
  W.simpleStatus response `shouldBe` status200
  either fail pure (eitherDecode response.simpleBody)
await :: Wai.Application -> ByteString -> IO Wire.PrefixProofResponse
await app path=do response<-request app methodGet path "" >>= decoded @Wire.PrefixProofResponse
                  if response.state=="pending" then threadDelay 1000 >> await app path else pure response
snapshot :: TestEnv -> IO Value
snapshot env=runQuery env "SELECT jsonb_build_object('database_bytes',pg_database_size(current_database()),'retained_wal_bytes',(SELECT sum(size) FROM pg_ls_waldir()),'staged_accounting',(SELECT staged_bytes FROM session_archive_control WHERE singleton),'relations',(SELECT jsonb_agg(jsonb_build_object('relation',c.relname,'heap_main',pg_relation_size(c.oid),'heap_other',pg_table_size(c.oid)-pg_relation_size(c.oid)-CASE WHEN c.reltoastrelid=0 THEN 0 ELSE pg_total_relation_size(c.reltoastrelid) END,'indexes',pg_indexes_size(c.oid),'toast_total',CASE WHEN c.reltoastrelid=0 THEN 0 ELSE pg_total_relation_size(c.reltoastrelid) END,'total',pg_total_relation_size(c.oid))) FROM pg_class c JOIN pg_namespace n ON n.oid=c.relnamespace WHERE n.nspname='public' AND c.relname IN ('session_chunks','session_archive_parts','session_archive_checkpoints','session_archive_ledger','session_archive_generations','session_archive_reclamation','session_archive_control','session_archive_requests','session_archive_jobs')),'raw_stored_values',(SELECT coalesce(sum(pg_column_size(data)),0) FROM session_chunks),'dictionary_logical',(SELECT coalesce(sum(octet_length(dictionary)),0) FROM session_archive_checkpoints),'dictionary_stored_values',(SELECT coalesce(sum(pg_column_size(dictionary)),0) FROM session_archive_checkpoints),'temporary_bytes',(SELECT temp_bytes FROM pg_stat_database WHERE datname=current_database()))" D.jsonb
walDelta :: TestEnv -> Text -> Text -> IO Int64
walDelta env start end=number env ("SELECT pg_wal_lsn_diff('"<>TE.encodeUtf8 end<>"'::pg_lsn,'"<>TE.encodeUtf8 start<>"'::pg_lsn)::bigint")
execute :: TestEnv -> ByteString -> IO ()
execute env sql=P.runSession env.pool (S.sql sql)
-- CREATE/DROP DATABASE and VACUUM cannot use the application's managed BEGIN.
-- These commands act only on the explicitly owned isolated fixture database.
rawExecute :: TestEnv -> ByteString -> IO ()
rawExecute env sql=P.withConn env.pool $ \connection -> S.run (S.sql sql) connection >>= either (fail . show) pure
runQuery :: TestEnv -> ByteString -> D.Value a -> IO a
runQuery env sql decoder=P.runSession env.pool $ S.statement () (St.Statement sql E.noParams (D.singleRow (D.column (D.nonNullable decoder))) False)
number :: TestEnv -> ByteString -> IO Int64
number env sql=runQuery env sql D.int8
text :: TestEnv -> ByteString -> IO Text
text env sql=runQuery env sql D.text
fileHash :: FilePath -> IO Text
fileHash path=T.pack . show . (hashlazy::LBS.ByteString -> Digest SHA256) <$> LBS.readFile path
uuid :: UUID -> ByteString
uuid=B8.pack . show
