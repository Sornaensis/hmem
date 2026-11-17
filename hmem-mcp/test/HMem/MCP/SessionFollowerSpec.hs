{-# LANGUAGE RankNTypes #-}
module HMem.MCP.SessionFollowerSpec (spec) where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception
import Control.Monad (forM, forM_, replicateM, unless, when, void, (>=>))
import Data.Aeson
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Key qualified as Key
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as BL
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time
import Data.Maybe (fromMaybe)
import Data.List (nub)
import Data.Map.Strict qualified as Map
import Data.Char (toLower)
import Data.UUID (UUID)
import Data.UUID.V4 qualified as UUID
import Data.Int (Int64)
import GHC.Clock (getMonotonicTime)
import GHC.Conc (threadStatus, ThreadStatus(..))
import HMem.AgentSession qualified as Core
import HMem.AgentSession.Transport qualified as Wire
import HMem.BotIdentity (BotIdentity, mkBotIdentity)
import HMem.Config qualified as Config
import HMem.DB.AgentSession qualified as Sessions
import HMem.DB.Migration qualified as Migration
import HMem.DB.Pool (createPool, runSession)
import HMem.DB.SessionArchive qualified as Archive
import HMem.DB.SessionArchive.Maintenance qualified as ArchiveMaintenance
import HMem.DB.SessionArchive.Reader qualified as ArchiveReader
import Data.Pool (destroyAllResources)
import Hasql.Session qualified as SQL
import HMem.DB.RequestContext
import HMem.DB.TestHarness (TestEnv(..), TestSandbox(..), TestDb(..), createTestWorkspace, withTestEnv, withTestSandbox, withSandboxedEnv, withSandboxedPostgres, resolveMigrationsDir)
import HMem.MCP.SessionFile
import HMem.MCP.SessionFollower
import HMem.MCP.SessionHTTP
import HMem.MCP.SessionProcess
import HMem.MCP.Server (MCPContext(..), handleStdioLineWithRegistry, runMCPServerWithHandlesObservedWithFollowerOptions)
import HMem.MCP.Tools (handleToolCall)
import HMem.MCP.TaskTriage (validateArchive, validateArchiveWith)
import HMem.Server.AccessTracker (newAccessTracker)
import HMem.Server.App (mkAppWithPrefixProofs)
import HMem.Server.Archive.Worker (runArchiveLease)
import HMem.Server.SessionPrefix (PrefixRegistry, newPrefixRegistry, newPrefixRegistryAt, closePrefixRegistry, prefixTraffic)
import HMem.Server.SessionTraffic (ComputeStats(..), RequestStats(..), SessionTrafficSnapshot(..), readSessionTraffic)
import HMem.Server.WebSocket (newWSState)
import HMem.SessionArchive.Codec qualified as ArchiveCodec
import HMem.SessionArchive.Replacement qualified as Replacement
import HMem.Types (Workspace(..))
import Network.HTTP.Client (Manager, newManager, closeManager, defaultManagerSettings, managerIdleConnectionCount)
import Network.Wai.Handler.Warp (testWithApplication)
import Network.Wai qualified as Wai
import Network.HTTP.Types (hAuthorization, status200)
import Network.HTTP.Types.URI (parseQuery)
import System.Directory
import System.Environment (getEnv, lookupEnv)
import System.IO
import System.Process (createPipe)
import System.Process qualified as Process
import System.FilePath ((</>))
import System.Info (os)
import Paths_hmem_mcp qualified as Paths
import System.Timeout
import Test.Hspec
import Text.Read (readMaybe)

spec :: Spec
spec = describe "Session follower" $ do

  it "HistoricalImport uploads completed-task JSONL, retries a lost seal ack, and triages a verified archive without overwrite" $
    withBackendUsing credentialBackend $ \env wid manager base -> do
      runSession env.pool $ SQL.sql ("UPDATE tasks SET status='done' WHERE id='"<>BS8.pack (show wid)<>"'")
      let original=BS.concat (replicate 42 ("{\"text\":\""<>BS.replicate 32740 97<>"\"}\r\n"))<>"[1,true]"
      withSourceFile original $ \path -> withCapturedHTTP base $ \captured -> do
        sid <- UUID.nextRandom
        lost <- newTVarIO False
        let action verb target body = do
              result <- followerRequest captured verb target body
              if "/terminate" `T.isSuffixOf` T.pack target then do
                first <- atomically $ do old <- readTVar lost; writeTVar lost True; pure (not old)
                pure (if first then Left HTTPUnavailable else result)
              else pure result
            args=object ["workspace_id" .= wid,"session_id" .= sid,"task_id" .= wid,"path" .= path,"title" .= ("Historical completed work" :: Text),"format_label" .= ("jsonl" :: Text)]
            imported registry=handleUploadTool registry manager base Nothing identity "session_import" args
        withRegistry fast {requestOverride=Just action} $ \registry -> do
          initial <- imported registry
          payload initial `shouldSatisfy` hasStatus "validating"
          forM_ ["session_upload_flush","session_upload_stop"] $ \name -> do
            refusal <- handleUploadTool registry manager base Nothing identity name (object ["workspace_id" .= wid,"session_id" .= sid])
            nested ["structuredContent","error","code"] refusal `shouldBe` Just (String "import_control_unsupported")
          repeated <- imported registry
          field "isError" repeated `shouldBe` Nothing
          completed <- await registry manager base wid sid (hasStatus "imported")
          nested ["receipt","sha256"] completed `shouldBe` Just (String (Core.sessionSha256 original))
          Just canonical <- Sessions.getSession env.pool wid sid
          canonical.state `shouldBe` Core.Terminated
          canonical.finalBytes `shouldBe` Just (fromIntegral (BS.length original))
          canonical.importTaskId `shouldBe` Just wid
          let download offset pieces = do
                range <- Sessions.readSessionRange env.pool wid sid offset 65536
                if range.hasMore then download range.nextOffset (range.bytes:pieces) else pure (BS.concat (reverse (range.bytes:pieces)))
          download 0 [] `shouldReturn` original
          retried <- imported registry
          field "isError" retried `shouldBe` Nothing
          _ <- await registry manager base wid sid (hasStatus "imported")
          withSourceFile "" $ \destination -> do
            removeFile destination
            let callTriage=handleToolCall manager base Nothing identity (object ["name" .= ("task_triage" :: Text),"arguments" .= object ["workspace_id" .= wid,"task_id" .= wid,"path" .= destination]])
            archived <- callTriage
            nested ["structuredContent","session_count"] archived `shouldBe` Just (Number 1)
            withBinaryFile destination ReadMode validateArchive `shouldReturn` 1
            archiveBefore <- BS.readFile destination
            withSourceFile "" $ \mixedDestination -> do
              removeFile mixedDestination
              mixed <- handleToolCall manager base Nothing identity (object ["name" .= ("task_triage" :: Text),
                "arguments" .= object ["workspace_id" .= wid,"task_id" .= wid,"path" .= mixedDestination,"representation" .= ("available-v1" :: Text)]])
              nested ["structuredContent","representation"] mixed `shouldBe` Just (String "available-v1")
              withBinaryFile mixedDestination ReadMode (validateArchiveWith True) `shouldReturn` 1
              bytes <- BS.readFile mixedDestination
              let digest=TE.encodeUtf8 (Core.sessionSha256 original)
                  changed=(if BS.head digest==48 then "1" else "0")<>BS.tail digest
                  corrupt input = case BS.breakSubstring digest input of
                    (prefix,rest) | BS.null rest -> prefix
                                  | otherwise -> prefix<>changed<>corrupt (BS.drop 64 rest)
              withSourceFile (corrupt bytes) $ \invalidArchive ->
                withBinaryFile invalidArchive ReadMode (validateArchiveWith True) `shouldThrow` anyException
            refused <- callTriage
            nested ["structuredContent","error","code"] refused `shouldBe` Just (String "destination_exists")
            BS.readFile destination `shouldReturn` archiveBefore
            withSourceFile "" $ \racingDestination -> do
              removeFile racingDestination
              let raceApp _ respond = do
                    BS.writeFile racingDestination "preserve concurrent destination"
                    respond (Wai.responseLBS status200 [] (BL.fromStrict archiveBefore))
              testWithApplication (pure raceApp) $ \racePort -> do
                refusedRace <- handleToolCall manager ("http://127.0.0.1:"<>show racePort) Nothing identity
                  (object ["name" .= ("task_triage" :: Text),"arguments" .= object ["workspace_id" .= wid,"task_id" .= wid,"path" .= racingDestination]])
                nested ["structuredContent","error","code"] refusedRace `shouldBe` Just (String "destination_exists")
                BS.readFile racingDestination `shouldReturn` "preserve concurrent destination"
            forM_ [BS.take (BS.length archiveBefore-1) archiveBefore,BS.take 64 archiveBefore<>BS.singleton 0<>BS.drop 65 archiveBefore] $ \damaged ->
              testWithApplication (pure (\_ respond -> respond (Wai.responseLBS status200 [] (BL.fromStrict damaged)))) $ \badPort ->
                withSourceFile "" $ \missing -> do
                  removeFile missing
                  result <- handleToolCall manager ("http://127.0.0.1:"<>show badPort) Nothing identity
                    (object ["name" .= ("task_triage" :: Text),"arguments" .= object ["workspace_id" .= wid,"task_id" .= wid,"path" .= missing]])
                  field "isError" result `shouldBe` Just (Bool True)
                  doesPathExist missing `shouldReturn` False
          runSession env.pool (SQL.sql ("DO $$ BEGIN IF NOT EXISTS(SELECT 1 FROM tasks WHERE id='"<>BS8.pack (show wid)<>"' AND status='done') THEN RAISE EXCEPTION 'Task status changed'; END IF; END $$"))

  it "SanitizedQualification reads and triages an actual published sanitized generation through MCP" $
    withBackendUsing credentialBackend $ \env wid manager base -> do
      let original="{\"type\":\"response_item\",\"payload\":{\"type\":\"function_call\",\"encrypted_content\":\"opaque\"}}\r\n[]"
          sanitized="{\"type\":\"response_item\",\"payload\":{\"type\":\"function_call\",\"encrypted_content\":\"Encrypted Payload\"}}\r\n[]"
          encodedPolicy="{\"version\":1,\"eligible_from\":\"2000-01-01T00:00:00Z\",\"formats\":{\"codex_jsonl\":[{\"match\":{\"/type\":\"response_item\",\"/payload/type\":\"function_call\"},\"path\":\"/payload/encrypted_content\"}]}}"
      policy <- either throwIO pure (Replacement.decodePolicy encodedPolicy)
      ArchiveMaintenance.configureMaintenance env.pool (ArchiveMaintenance.InstallReplacement policy)
      sid <- UUID.nextRandom
      _ <- Sessions.createSession env.pool (Core.CreateSession sid wid "codex_jsonl" "Sanitized MCP fixture" Nothing Nothing wid)
      uploaded <- (.session) <$> Sessions.appendSession env.pool wid sid (Core.AppendSession 0 0 original True)
      _ <- Sessions.terminateSession env.pool wid sid
        (Core.TerminateSession uploaded.committedBytes uploaded.revision uploaded.committedBytes (Core.sessionSha256 original))
      runSession env.pool $ SQL.sql ("UPDATE tasks SET status='done' WHERE id='"<>BS8.pack (show wid)<>"'")
      ArchiveReader.ensureArchive env.pool wid sid
      implementation <- ArchiveCodec.codecImplementation
      Just lease <- Archive.claimArchive env.pool implementation
      runArchiveLease env.pool lease
      let arguments=["workspace_id" .= wid,"session_id" .= sid]
          call name fields=handleToolCall manager base Nothing identity
            (object ["name" .= (name::Text),"arguments" .= object fields])
          result key=nested ["structuredContent",key]
      receipt <- call "session_archive_manifest" arguments
      result "generation_id" receipt `shouldBe` Just (toJSON lease.generationId)
      result "content_sha256" receipt `shouldBe` Just (String (Core.sessionSha256 sanitized))
      exact <- call "session_evidence" arguments
      result "data_base64" exact `shouldBe` Just (String (Wire.encodeSessionBytes original))
      selected <- call "session_evidence" (arguments<>["sanitized_generation_id" .= lease.generationId])
      result "data_base64" selected `shouldBe` Just (String (Wire.encodeSessionBytes sanitized))
      withSourceFile "" $ \destination -> do
        removeFile destination
        exported <- call "task_triage" ["workspace_id" .= wid,"task_id" .= wid,"path" .= destination,"representation" .= ("available-v1"::Text)]
        result "session_count" exported `shouldBe` Just (Number 1)
        withBinaryFile destination ReadMode (validateArchiveWith True) `shouldReturn` 1

  it "HistoricalImport rejects invalid tails before remote registration and refuses live controls during validation" $
    withBackend $ \env wid manager base -> forM_ ["{}\n{", "{}\n\n",BS.pack [255],"{}\nfalse garbage"] $ \invalid ->
      withSourceFile invalid $ \path -> withRegistry fast $ \registry -> do
        sid <- UUID.nextRandom
        _ <- handleUploadTool registry manager base Nothing identity "session_import"
          (object ["workspace_id" .= wid,"session_id" .= sid,"task_id" .= wid,"path" .= path,"title" .= ("Invalid historical evidence" :: Text),"format_label" .= ("jsonl" :: Text)])
        failed <- await registry manager base wid sid (hasStatus "failed")
        field "error" failed `shouldBe` Just (String "invalid_jsonl")
        Sessions.getSession env.pool wid sid `shouldReturn` Nothing

  it "HistoricalImport reports a missing task without live-start guidance" $
    withBackend $ \env wid manager base -> withSourceFile "{}\n" $ \path -> withRegistry fast $ \registry -> do
      sid <- UUID.nextRandom; missing <- UUID.nextRandom
      _ <- handleUploadTool registry manager base Nothing identity "session_import"
        (object ["workspace_id" .= wid,"session_id" .= sid,"task_id" .= missing,"path" .= path,"title" .= ("Missing historical task" :: Text),"format_label" .= ("jsonl" :: Text)])
      failed <- await registry manager base wid sid (hasStatus "failed")
      field "error" failed `shouldBe` Just (String "session_task_not_found")
      nested ["error_details","required_status"] failed `shouldBe` Nothing
      nested ["error_details","message"] failed `shouldBe` Just (String "Import requires a visible nondeleted task in the same workspace; any task status is accepted.")
      Sessions.getSession env.pool wid sid `shouldReturn` Nothing

  it "HistoricalImport rejects source truncation and replacement during finite prevalidation" $
    withBackend $ \env wid manager base -> forM_ [False,True] $ \replace ->
      withSourceFile "{}\n[]\n" $ \path -> do
        changed <- newTVarIO False
        let mutation phase = when (phase=="import_validation_page") $ do
              first <- atomically $ do old <- readTVar changed; writeTVar changed True; pure (not old)
              when first $ if replace then renameFile path (path<>".old") >> BS.writeFile path "{}\n[]\n" else BS.writeFile path "{}"
        withRegistry fast {transition=mutation} $ \registry -> do
          sid <- UUID.nextRandom
          _ <- handleUploadTool registry manager base Nothing identity "session_import"
            (object ["workspace_id" .= wid,"session_id" .= sid,"task_id" .= wid,"path" .= path,"title" .= ("Changed historical evidence" :: Text),"format_label" .= ("jsonl" :: Text)])
          failed <- await registry manager base wid sid (hasStatus "failed")
          field "error" failed `shouldBe` Just (String (if replace then "source_identity_or_prefix_changed" else "source_truncated"))
          Sessions.getSession env.pool wid sid `shouldReturn` Nothing

  it "Reusable transport retains one helper and HTTP connection across exchanges, then joins it" $ do
    peers <- newTVarIO ([] :: [String])
    let app request respond = do
          atomically $ modifyTVar' peers (<>[show (Wai.remoteHost request)])
          respond (Wai.responseLBS status200 [] "{}")
    testWithApplication (pure app) $ \port -> do
      scope <- newProcessScope; executable <- testHelperExecutable
      captured <- captureHTTP scope executable ("http://127.0.0.1:"<>show port) Nothing identity
      flip finally (closeProcessScope scope 2000000 >>= (`shouldBe` True)) $ do
        forM_ [1..3 :: Int] $ \_ -> followerRequest captured "GET" "/reuse" Nothing `shouldReturn` Right (object [])
        processMetrics scope `shouldReturn` (1,0,False)
        nativeProcessCount `shouldReturn` 1
        addresses <- readTVarIO peers
        length addresses `shouldBe` 3
        length (nub addresses) `shouldBe` 1
      nativeProcessCount `shouldReturn` 0

  it "Reusable transport cancels native waits before reuse and restarts without stale frames" $ do
    let app _ respond = respond (Wai.responseLBS status200 [] "{}")
    testWithApplication (pure app) $ \port -> do
      executable <- testHelperExecutable
      forM_ ["dns","connect","tls","body"] $ \phase -> do
        scope <- newProcessScope
        pid <- newEmptyMVar; entered <- newEmptyMVar; done <- newEmptyMVar
        let action = try @SomeException (runReusableHelper scope executable (putMVar pid) (putMVar entered) (helperProbe phase)) >>= putMVar done
        flip finally (closeProcessScope scope 2000000 >>= (`shouldBe` True)) $ do
          tid <- forkIO action
          timeout 3000000 (readMVar entered) `shouldReturn` Just phase
          _ <- forkIO (killThread tid)
          result <- timeout 2000000 (readMVar done)
          result `shouldSatisfy` maybe False (either (\failure -> case fromException failure :: Maybe AsyncException of Just ThreadKilled -> True; _ -> False) (const False))
          readMVar pid >>= (processAlive >=> (`shouldBe` False))
          nativeProcessCount `shouldReturn` 0
          captured <- captureHTTP scope executable ("http://127.0.0.1:"<>show port) Nothing identity
          followerRequest captured "GET" "/restarted" Nothing `shouldReturn` Right (object [])
          processMetrics scope `shouldReturn` (1,0,False)
        nativeProcessCount `shouldReturn` 0

  it "PrefixProofClient rejects forged target and audience results before any append" $
    withBackend $ \env wid manager base -> withSourceFile "{}\n" $ \path -> withCapturedHTTP base $ \captured -> do
      foreignId <- UUID.nextRandom
      let changes = [("workspace_id",toJSON foreignId),("session_id",toJSON foreignId),("operation_id",toJSON foreignId)
            ,("expected_offset",Number 1),("expected_revision",Number 99),("sha256",String (Core.sessionSha256 "wrong")),("audience",String "foreign-principal")]
      forM_ changes $ \(key,replacement) -> do
        sid <- UUID.nextRandom
        appends <- newTVarIO (0 :: Int)
        let action verb target body = do
              when ("/append" `T.isSuffixOf` T.pack target) $ atomically $ modifyTVar' appends (+1)
              result <- followerRequest captured verb target body
              pure $ case result of
                Right (Object fields) | "/prefix-verifications" `T.isInfixOf` T.pack target && (key /= "audience" || verb == "GET") ->
                  Right (Object (KM.insert (Key.fromText key) replacement fields))
                _ -> result
        withRegistry fast {requestOverride = Just action} $ \registry -> do
          _ <- start registry manager base wid sid path
          failed <- await registry manager base wid sid (hasStatus "failed")
          field "error" failed `shouldBe` Just (String "source_identity_or_prefix_changed")
          readTVarIO appends `shouldReturn` 0
          Just canonical <- Sessions.getSession env.pool wid sid
          canonical.committedBytes `shouldBe` 0

  it "PrefixProofClient rehashes with a new UUID after operation loss" $
    withBackend $ \env wid manager base -> withSourceFile "{}\n[]\n" $ \path -> withCapturedHTTP base $ \captured -> do
      sid <- UUID.nextRandom
      _ <- Sessions.createSession env.pool (Core.CreateSession sid wid "jsonl" "Test session work" Nothing Nothing wid)
      _ <- Sessions.appendSession env.pool wid sid (Core.AppendSession 0 0 "{}\n" False)
      missing <- newTVarIO False; operations <- newTVarIO ([] :: [Value]); pages <- newTVarIO (0 :: Int)
      let action verb target body = do
            when (verb == "POST" && "/prefix-verifications" `T.isSuffixOf` T.pack target) $
              atomically $ modifyTVar' operations (<> maybe [] (maybe [] pure . field "operation_id") body)
            lose <- if verb == "GET" && "/prefix-verifications/" `T.isInfixOf` T.pack target
              then atomically $ do old <- readTVar missing; writeTVar missing True; pure (not old)
              else pure False
            if lose then pure (Left (HTTPStatus 404)) else followerRequest captured verb target body
          page name = when (name == "local_prefix_page") $ atomically $ modifyTVar' pages (+1)
      withRegistry fast {requestOverride = Just action, transition = page} $ \registry -> do
        _ <- start registry manager base wid sid path
        _ <- await registry manager base wid sid (\value -> hasOffset 6 value && hasStatus "caught_up" value)
        ids <- readTVarIO operations
        length ids `shouldSatisfy` (>=2)
        length (nub (take 2 ids)) `shouldBe` 2
        readTVarIO pages >>= (`shouldSatisfy` (>=3))
        part <- Sessions.readSessionRange env.pool wid sid 0 100
        part.bytes `shouldBe` "{}\n[]\n"

  it "FollowerTraffic counts one lifecycle's requests, proof recovery and committed bytes and emits one fixed-label summary" $
    withBackend $ \env wid manager base -> withSourceFile "{}\n[]\n" $ \path -> withCapturedHTTP base $ \captured -> do
      sid <- UUID.nextRandom
      _ <- Sessions.createSession env.pool (Core.CreateSession sid wid "jsonl" "Test session work" Nothing Nothing wid)
      _ <- Sessions.appendSession env.pool wid sid (Core.AppendSession 0 0 "{}\n" False)
      missing <- newTVarIO False; summaries <- newTVarIO ([] :: [Value])
      let action verb target body = do
            lose <- if verb == "GET" && "/prefix-verifications/" `T.isInfixOf` T.pack target
              then atomically $ do old <- readTVar missing; writeTVar missing True; pure (not old)
              else pure False
            if lose then pure (Left (HTTPStatus 404)) else followerRequest captured verb target body
          sink value = atomically $ modifyTVar' summaries (<> [value])
          counted :: FollowerTraffic -> (Text,Text) -> Int
          counted traffic key = Map.findWithDefault 0 key traffic.requests
      withRegistry fast {requestOverride = Just action, trafficLog = Just sink} $ \registry -> do
        _ <- start registry manager base wid sid path
        _ <- await registry manager base wid sid (\value -> hasOffset 6 value && hasStatus "caught_up" value)
        Just live <- followerTraffic registry sid
        counted live ("metadata","ok") `shouldBe` 1
        counted live ("proof_poll","verification_unknown") `shouldBe` 1
        counted live ("append","ok") `shouldBe` 1
        sum [count | ((route,_),count) <- Map.toList live.requests, route == "proof_submit"] `shouldSatisfy` (>= 2)
        (live.proofRecoveries,live.committedBytes,live.transientRetries,live.emitted) `shouldBe` (1,3,0,False)
        live.localHashBytes `shouldSatisfy` (>= 6)
        readTVarIO summaries `shouldReturn` []
        _ <- control registry manager base wid sid "session_upload_stop"
        _ <- await registry manager base wid sid (hasStatus "stopped")
        Just final <- followerTraffic registry sid
        final.emitted `shouldBe` True
        emitted <- readTVarIO summaries
        length emitted `shouldBe` 1
        let summary = head emitted
        field "event" summary `shouldBe` Just (String "session_follower_traffic")
        field "status" summary `shouldBe` Just (String "stopped")
        field "proof_recoveries" summary `shouldBe` Just (Number 1)
        field "committed_bytes" summary `shouldBe` Just (Number 3)
        let encoded = TE.decodeUtf8 (BL.toStrict (encode summary))
        forM_ [T.pack (show sid), T.pack (show wid), T.pack path] $ \secret -> encoded `shouldSatisfy` (not . T.isInfixOf secret)
      readTVarIO summaries >>= (`shouldSatisfy` ((== 1) . length))

  it "SessionTrafficMeasurement attributes matched cohort traffic and proof misses (opt-in)" $ do
    enabled <- lookupEnv "HMEM_SESSION_TRAFFIC_MEASUREMENT"
    if enabled /= Just "1" then pendingWith "opt-in session traffic attribution measurement" else measureSessionTraffic

  it "PrefixProofClient bounds repeated operation loss despite pending acknowledgements" $
    withBackend $ \env wid manager base -> withSourceFile "{}\n[]\n" $ \path -> withCapturedHTTP base $ \captured -> do
      sid <- UUID.nextRandom
      peer <- UUID.nextRandom
      _ <- Sessions.createSession env.pool (Core.CreateSession sid wid "jsonl" "Test session work" Nothing Nothing wid)
      original <- Sessions.appendSession env.pool wid sid (Core.AppendSession 0 0 "{}\n" False)
      pendingOperations <- newTVarIO ([] :: [Value])
      lookups <- newTVarIO (0 :: Int)
      appends <- newTVarIO (0 :: Int)
      firstLookup <- newEmptyMVar
      releaseLookup <- newEmptyMVar
      let prefix = "/sessions/" <> T.pack (show sid) <> "/"
          action verb target body
            | prefix `T.isInfixOf` route, verb == "POST", "/prefix-verifications" `T.isSuffixOf` route = do
                result <- followerRequest captured verb target body
                case result of
                  Right value | field "state" value == Just (String "pending")
                    , Just operation <- field "operation_id" value ->
                        atomically $ modifyTVar' pendingOperations (operation :)
                  _ -> pure ()
                pure result
            | prefix `T.isInfixOf` route, verb == "GET", "/prefix-verifications/" `T.isInfixOf` route = do
                first <- atomically $ do
                  count <- readTVar lookups
                  writeTVar lookups (count + 1)
                  pure (count == 0)
                when first $ putMVar firstLookup () >> readMVar releaseLookup
                -- Model a poll routed to a process that never saw the submission.
                pure (Left (HTTPStatus 404))
            | otherwise = do
                when (prefix `T.isInfixOf` route && "/append" `T.isSuffixOf` route) $
                  atomically $ modifyTVar' appends (+1)
                followerRequest captured verb target body
            where route = T.pack target
      withRegistry fast {requestOverride = Just action} $ \registry ->
        flip finally (void (tryPutMVar releaseLookup ())) $ do
          _ <- start registry manager base wid sid path
          timeout 2000000 (readMVar firstLookup) `shouldReturn` Just ()
          flush <- control registry manager base wid sid "session_upload_flush"
          nested ["operation","pending"] flush `shouldBe` Just (Bool True)
          _ <- start registry manager base wid peer path
          _ <- await registry manager base wid peer (\value -> hasOffset 6 value && hasStatus "caught_up" value)
          putMVar releaseLookup ()
          failed <- await registry manager base wid sid (hasStatus "failed")
          field "error" failed `shouldBe` Just (String "verification_retry_exhausted")
          field "receipt" failed `shouldBe` Just Null
          nested ["operation","pending"] failed `shouldBe` Just (Bool False)
          nested ["operation","receipt","source_verified"] failed `shouldBe` Just (Bool False)
          nested ["operation","receipt","error"] failed `shouldBe` Just (String "verification_retry_exhausted")
          operations <- readTVarIO pendingOperations
          length operations `shouldBe` 5
          length (nub operations) `shouldBe` 5
          readTVarIO lookups `shouldReturn` 5
          readTVarIO appends `shouldReturn` 0
          Just canonical <- Sessions.getSession env.pool wid sid
          canonical.committedBytes `shouldBe` original.session.committedBytes
          canonical.revision `shouldBe` original.session.revision
          canonical.state `shouldBe` Core.Active

  it "PrefixProofClient catches mutation of an already-hashed page before resumed append" $
    withBackend $ \env wid manager base -> do
      let chunk=BS.concat (replicate 30 ("\""<>BS.replicate 32765 97<>"\"\n")); original=chunk<>chunk
      withSourceFile (original<>"[]\n") $ \path -> withCapturedHTTP base $ \captured -> do
        sid <- UUID.nextRandom
        _ <- Sessions.createSession env.pool (Core.CreateSession sid wid "jsonl" "Test session work" Nothing Nothing wid)
        first <- Sessions.appendSession env.pool wid sid (Core.AppendSession 0 0 chunk False)
        _ <- Sessions.appendSession env.pool wid sid (Core.AppendSession first.session.committedBytes first.session.revision chunk False)
        changed <- newTVarIO False; verified <- newTVarIO False; appends <- newTVarIO (0 :: Int)
        let page name = when (name == "local_prefix_page") $ do
              firstPage <- atomically $ do old <- readTVar changed; writeTVar changed True; pure (not old)
              when firstPage $ withBinaryFile path ReadWriteMode $ \handle -> hSeek handle AbsoluteSeek 0 >> BS.hPut handle "["
            action verb target body = do
              when ("/append" `T.isSuffixOf` T.pack target) $ atomically $ modifyTVar' appends (+1)
              result <- followerRequest captured verb target body
              case result of Right value | "/prefix-verifications/" `T.isInfixOf` T.pack target && field "state" value == Just (String "verified") -> atomically $ writeTVar verified True; _ -> pure ()
              pure result
        withRegistry fast {requestOverride = Just action, transition = page} $ \registry -> do
          _ <- start registry manager base wid sid path
          _ <- await registry manager base wid sid (hasStatus "failed")
          readTVarIO verified `shouldReturn` True
          readTVarIO appends `shouldReturn` 0
          Just canonical <- Sessions.getSession env.pool wid sid
          canonical.committedBytes `shouldBe` fromIntegral (BS.length original)

  it "PrefixProofClient reconciles the latest canonical cursor before dispatching resumed tail bytes" $
    withBackend $ \env wid manager base -> withSourceFile "{}\n[]\ntrue\n" $ \path -> withCapturedHTTP base $ \captured -> do
      sid <- UUID.nextRandom
      _ <- Sessions.createSession env.pool (Core.CreateSession sid wid "jsonl" "Test session work" Nothing Nothing wid)
      _ <- Sessions.appendSession env.pool wid sid (Core.AppendSession 0 0 "{}\n" False)
      grew <- newTVarIO False; offsets <- newTVarIO ([] :: [Value]); proofTargets <- newTVarIO ([] :: [Value])
      let action verb target body = do
            when (verb == "POST" && "/append" `T.isSuffixOf` T.pack target) $
              atomically $ modifyTVar' offsets (<> maybe [] (maybe [] pure . field "expected_offset") body)
            when (verb == "POST" && "/prefix-verifications" `T.isSuffixOf` T.pack target) $
              atomically $ modifyTVar' proofTargets (<> maybe [] (maybe [] pure . field "expected_offset") body)
            result <- followerRequest captured verb target body
            case result of
              Right value | verb == "GET" && "/prefix-verifications/" `T.isInfixOf` T.pack target && field "state" value == Just (String "verified") -> do
                first <- atomically $ do old <- readTVar grew; writeTVar grew True; pure (not old)
                when first $ do _ <- Sessions.appendSession env.pool wid sid (Core.AppendSession 3 1 "[]\n" False); pure ()
              _ -> pure ()
            pure result
      withRegistry fast {requestOverride = Just action} $ \registry -> do
        _ <- start registry manager base wid sid path
        _ <- await registry manager base wid sid (\value -> hasOffset 11 value && hasStatus "caught_up" value)
        readTVarIO offsets `shouldReturn` [Number 6]
        targets <- readTVarIO proofTargets
        take 2 targets `shouldBe` [Number 3,Number 6]
        part <- Sessions.readSessionRange env.pool wid sid 0 100
        part.bytes `shouldBe` "{}\n[]\ntrue\n"

  it "rejects initial-task validation terminally without orphan sessions, handles or occupied capacity and can retry" $
    withBackend $ \env wid manager base -> withSourceFile "{}\n" $ \path -> withRegistry fast $ \registry -> do
      baseline <- openSourceCount
      waiting <- UUID.nextRandom
      runSession env.pool $ SQL.sql ("INSERT INTO tasks(id,workspace_id,title,status) VALUES ('"<>BS8.pack (show waiting)<>"','"<>BS8.pack (show wid)<>"','Waiting creation task','todo')")
      missing <- UUID.nextRandom
      let upload sid task=payload <$> handleUploadTool registry manager base Nothing identity "session_upload_start"
            (object ["workspace_id" .= wid,"session_id" .= sid,"path" .= path,"title" .= ("Capture running task" :: Text),"format_label" .= ("jsonl" :: Text),"task_id" .= task])
      forM_ [(waiting,"session_task_not_in_progress",Just "todo"),(missing,"session_task_not_found",Nothing)] $ \(task,code,current) -> do
        sid <- UUID.nextRandom
        initial <- upload sid task
        field "status" initial `shouldBe` Just (String "validating")
        failed <- await registry manager base wid sid (hasStatus "failed")
        field "error" failed `shouldBe` Just (String code)
        nested ["error_details","field"] failed `shouldBe` Just (String "task_id")
        nested ["error_details","required_status"] failed `shouldBe` Just (String "in_progress")
        forM_ current $ \status -> nested ["error_details","current_status"] failed `shouldBe` Just (String status)
        failed `shouldSatisfy` (not . contains (T.pack path))
        Sessions.getSession env.pool wid sid `shouldReturn` Nothing
        openSourceCount `shouldReturn` baseline
        (slots,_,_) <- registryMetrics registry
        slots `shouldBe` 0
        _ <- upload sid wid
        _ <- await registry manager base wid sid (hasOffset 3)
        runSession env.pool $ SQL.sql ("UPDATE tasks SET status='done' WHERE id='"<>BS8.pack (show wid)<>"'")
        BS.appendFile path "[]\n"
        _ <- await registry manager base wid sid (hasOffset 6)
        _ <- control registry manager base wid sid "session_upload_stop"
        _ <- await registry manager base wid sid (hasStatus "stopped")
        openSourceCount `shouldReturn` baseline
        -- Resume and stop remain valid after independent task completion.
        _ <- upload sid wid
        _ <- await registry manager base wid sid (hasStatus "caught_up")
        _ <- control registry manager base wid sid "session_upload_stop"
        _ <- await registry manager base wid sid (hasStatus "stopped")
        Just final <- Sessions.getSession env.pool wid sid
        _ <- Sessions.terminateSession env.pool wid sid (Core.TerminateSession final.committedBytes final.revision 6 (Core.sessionSha256 "{}\n[]\n"))
        openSourceCount `shouldReturn` baseline
        BS.writeFile path "{}\n"
        runSession env.pool $ SQL.sql ("UPDATE tasks SET status='in_progress' WHERE id='"<>BS8.pack (show wid)<>"'")

  it "preserves canonical runtime switches and unknown clearing through stale-start declarations and continuing capture" $
    withBackend $ \env wid manager base -> withSourceFile "{}\n[]\n" $ \path -> withRegistry fast $ \registry -> do
      sid <- UUID.nextRandom
      let a=Core.RuntimeIdentity "Target runtime A" Nothing Nothing (Just "developer") (Just "codex")
          b=Core.RuntimeIdentity "Target runtime B" Nothing (Just "high") (Just "reviewer") (Just "other-harness")
          upload=payload <$> handleUploadTool registry manager base Nothing identity "session_upload_start"
            (object ["workspace_id" .= wid,"session_id" .= sid,"path" .= path,"title" .= ("Test session work" :: Text),"format_label" .= ("jsonl" :: Text),"initial_runtime_identity" .= a,"task_id" .= wid])
      _ <- upload
      _ <- await registry manager base wid sid (hasOffset 6)
      Just original <- Sessions.getSession env.pool wid sid
      original.runtimeIdentity `shouldBe` Just a
      _ <- Sessions.setRuntimeIdentity env.pool wid sid (Core.SetRuntimeIdentity 0 (Just b))
      BL.appendFile path "true\n"
      live <- await registry manager base wid sid (\value -> hasOffset 11 value && field "runtime_identity" value==Just (toJSON b))
      field "runtime_revision" live `shouldBe` Just (Number 1)
      retried <- handleToolCall manager base Nothing identity (object ["name" .= ("session_create" :: Text),"arguments" .= object ["workspace_id" .= wid,"session_id" .= sid,"title" .= ("Test session work" :: Text),"format_label" .= ("jsonl" :: Text),"initial_runtime_identity" .= a,"task_id" .= wid]])
      nested ["summary","runtime_identity"] (payload retried) `shouldBe` Just (toJSON b)
      _ <- control registry manager base wid sid "session_upload_stop"
      _ <- await registry manager base wid sid (hasStatus "stopped")
      _ <- upload
      BL.appendFile path "null\n"
      resumed <- await registry manager base wid sid (\value -> hasOffset 16 value && field "runtime_identity" value==Just (toJSON b))
      field "runtime_revision" resumed `shouldBe` Just (Number 1)
      _ <- control registry manager base wid sid "session_upload_stop"
      _ <- await registry manager base wid sid (hasStatus "stopped")
      _ <- Sessions.setRuntimeIdentity env.pool wid sid (Core.SetRuntimeIdentity 1 Nothing)
      _ <- upload
      BL.appendFile path "false\n"
      cleared <- await registry manager base wid sid (\value -> hasOffset 22 value && field "runtime_identity" value==Just Null)
      field "runtime_revision" cleared `shouldBe` Just (Number 2)
      _ <- control registry manager base wid sid "session_upload_stop"
      stopped <- await registry manager base wid sid (hasStatus "stopped")
      receiptHash stopped `shouldBe` Just (String (Core.sessionSha256 "{}\n[]\ntrue\nnull\nfalse\n"))
      Just final <- Sessions.getSession env.pool wid sid
      final.creatorBotIdentity `shouldBe` original.creatorBotIdentity
      final.runtimeIdentity `shouldBe` Nothing

  it "requires an explicit very short title while keeping parent optional in tools/list and validation" $
    withManager $ \manager -> withRegistry fast $ \registry -> do
      let [definition]=[value | value <- uploadToolDefinitions,field "name" value==Just (String "session_upload_start")]
      nested ["inputSchema","required"] definition `shouldBe` Just (toJSON (["session_id","path","format_label","title","task_id"] :: [Text]))
      definition `shouldSatisfy` contains "Actual running agent role"
      definition `shouldSatisfy` contains "\"model\",\"role\""
      result <- handleUploadTool registry manager "http://unused.invalid" Nothing identity "session_upload_start"
        (object ["workspace_id" .= workspace,"session_id" .= workspace,"path" .= ("source.jsonl" :: Text),"format_label" .= ("jsonl" :: Text)])
      nested ["error","code"] (payload result) `shouldBe` Just (String "invalid_arguments")
      result `shouldSatisfy` contains "very short descriptive title"
      let base=object ["workspace_id" .= workspace,"session_id" .= workspace,"path" .= ("source.jsonl" :: Text),"format_label" .= ("jsonl" :: Text),"title" .= ("Capture running task" :: Text)]
          taskField value=case base of Object fields -> Object (KM.insert "task_id" value fields); other -> other
      forM_ [base,taskField Null,taskField (String "not-a-task-uuid")] $ \arguments -> do
        refused <- handleUploadTool registry manager "http://unused.invalid" Nothing identity "session_upload_start" arguments
        nested ["error","code"] (payload refused) `shouldBe` Just (String "invalid_arguments")
        refused `shouldSatisfy` contains "task_id"
        refused `shouldSatisfy` contains "in_progress"
      registryMetrics registry `shouldReturn` (0,0,0)

  it "resumes actual pre-V043 untitled active evidence with a mandatory title while retaining immutable legacy metadata" $
    withTestSandbox $ \sandbox -> withSandboxedEnv sandbox $ withSandboxedPostgres sandbox $ \db ->
      bracket (createPool db.testDbConnStr 2 30 30000) destroyAllResources $ \pool -> do
        source <- resolveMigrationsDir sandbox.sandboxRepoRoot
        let beforeDir=sandbox.sandboxRoot </> "follower-before-v043"
            env=TestEnv pool sandbox db
        createDirectory beforeDir
        files <- listDirectory source
        forM_ (filter (\name -> name < "V043" && ".sql" `T.isSuffixOf` T.pack name) files) $ \name -> copyFile (source </> name) (beforeDir </> name)
        before <- Migration.runMigrations pool beforeDir
        before.failed `shouldBe` Nothing
        wid <- (.id) <$> createTestWorkspace env "legacy-follower"
        sid <- UUID.nextRandom
        runSession pool $ SQL.sql ("INSERT INTO sessions(id,workspace_id,format_label) VALUES ('"<>BS8.pack (show sid)<>"','"<>BS8.pack (show wid)<>"','jsonl'); INSERT INTO session_chunks(session_id,start_offset,revision,data,allow_final_record,record_count,ends_with_newline) VALUES ('"<>BS8.pack (show sid)<>"',0,1,convert_to(E'{}\\n','UTF8'),false,0,false)")
        upgraded <- Migration.runMigrations pool source
        upgraded.failed `shouldBe` Nothing
        Just original <- Sessions.getSession pool wid sid
        serveBackend env wid $ \_ _ manager base -> withSourceFile "{}\n[]\n" $ \path -> withRegistry fast $ \registry -> do
          _ <- start registry manager base wid sid path
          _ <- await registry manager base wid sid (hasOffset 6)
          _ <- control registry manager base wid sid "session_upload_stop"
          stopped <- await registry manager base wid sid (hasStatus "stopped")
          receiptHash stopped `shouldBe` Just (String (Core.sessionSha256 "{}\n[]\n"))
          Just resumed <- Sessions.getSession pool wid sid
          (resumed.title,resumed.parentSessionId,resumed.creatorActorId,resumed.createdAt) `shouldBe` (Nothing,Nothing,original.creatorActorId,original.createdAt)
          range <- Sessions.readSessionRange pool wid sid 0 100
          range.bytes `shouldBe` "{}\n[]\n"

  it "poisons follower admission and retains its slot until an unconfirmed helper owner finishes cleanup" $
    withSourceFile "" $ \path -> withManager $ \manager -> do
      entered <- newEmptyMVar; release <- newEmptyMVar
      requestRelease <- newEmptyMVar
      scope <- newProcessScopeWithReapHook (putMVar entered () >> readMVar release)
      sid <- UUID.nextRandom
      let app _ respond = readMVar requestRelease >> respond (Wai.responseLBS status200 [] (encode (fakeMetadata sid 0)))
      testWithApplication (pure app) $ \port -> withRegistry fast {processScopeFactory = pure scope} $ \registry -> flip finally (void (tryPutMVar release ()) >> void (tryPutMVar requestRelease ())) $ do
        let base = "http://127.0.0.1:" <> show port
        _ <- start registry manager base workspace sid path
        timeout 6000000 (readMVar entered) >>= (`shouldSatisfy` isJustValue)
        flip finally (void (tryPutMVar release ())) $ do
          _ <- await registry manager base workspace sid (hasStatus "failed")
          (slots,_,_) <- registryMetrics registry
          slots `shouldBe` 1
          processMetrics scope `shouldReturn` (1,0,True)
          nativeProcessCount `shouldReturn` 1
          another <- UUID.nextRandom
          refused <- start registry manager base workspace another path
          nested ["error","code"] refused `shouldBe` Just (String "registry_closed")
          putMVar release ()
          waitUntil $ do (leases,pipes,_) <- processMetrics scope; pure (leases == 0 && pipes == 0)
          nativeProcessCount `shouldReturn` 0

  it "supervises helper native DNS/connect/TLS/body waits, cleanup deadlines and malformed/oversized frames" $ do
    executable <- testHelperExecutable
    forM_ ["dns","connect","tls","body"] $ \phase -> do
      scope <- newProcessScope
      pid <- newEmptyMVar; entered <- newEmptyMVar; done <- newEmptyMVar
      let payload = helperProbe phase
          action = try @SomeException (runHelper scope executable (putMVar pid) (putMVar entered) payload) >>= putMVar done
      bracket (forkIO action) (\tid -> do atomically $ cancelProcessScope scope; _ <- forkIO (killThread tid); pure ()) $ \tid -> do
        timeout 3000000 (readMVar entered) `shouldReturn` Just phase
        _ <- forkIO (killThread tid)
        result <- timeout 2000000 (readMVar done)
        result `shouldSatisfy` maybe False (either (\e -> case fromException e :: Maybe AsyncException of Just ThreadKilled -> True; _ -> False) (const False))
        processMetrics scope `shouldReturn` (0,0,False)
        readMVar pid >>= (processAlive >=> (`shouldBe` False))
        nativeProcessCount `shouldReturn` 0
    scope <- newProcessScope
    pid <- newEmptyMVar
    began <- getCurrentTime
    runHelper scope executable (putMVar pid) (const (pure ())) (helperProbe "dns") `shouldReturn` Left ProcessDeadline
    finished <- getCurrentTime
    diffUTCTime finished began `shouldSatisfy` (< 5.5)
    readMVar pid >>= (processAlive >=> (`shouldBe` False))
    runHelper scope executable (const (pure ())) (const (pure ())) "{" `shouldReturn` Right (BS.singleton 2)
    runHelper scope executable (const (throwIO (userError "injected startup observation failure"))) (const (pure ())) "{}" `shouldReturn` Left ProcessProtocol
    runHelper scope executable (const (pure ())) (const (pure ())) (BS.replicate (frameCap+1) 0) `shouldReturn` Left ProcessUnavailable
    processMetrics scope `shouldReturn` (0,0,False)
    nativeProcessCount `shouldReturn` 0

  it "preserves the exact HTTP response cap and raw exponent bytes across private helper framing" $ do
    let boundary = "\"" <> BS.replicate 2097150 97 <> "\""
        app request respond = respond $ Wai.responseLBS status200 [] $ BL.fromStrict $
          case Wai.pathInfo request of ["overflow"] -> boundary <> " "; ["exponent"] -> "1e10000000"; _ -> boundary
    testWithApplication (pure app) $ \port -> do
      scope <- newProcessScope; executable <- testHelperExecutable
      let base = "http://127.0.0.1:" <> show port
      captured <- captureHTTP scope executable base Nothing identity
      boundaryResult <- followerRequest captured "GET" "/boundary" Nothing
      case boundaryResult of Right (String text) -> T.length text `shouldBe` 2097150; _ -> expectationFailure "HTTP cap boundary lost"
      followerRequest captured "GET" "/overflow" Nothing `shouldReturn` Left HTTPTooLarge
      raw <- runHelper scope executable (const (pure ())) (const (pure ())) (BL.toStrict $ encode $ object
        ["url" .= (base <> "/exponent"),"method" .= ("GET" :: Text),"headers" .= ([] :: [(Text,Text)]),"body" .= Null])
      fmap (BS.drop 3) raw `shouldBe` Right "1e10000000"
      processMetrics scope `shouldReturn` (1,0,False)
      closeProcessScope scope 2000000 `shouldReturn` True
      processMetrics scope `shouldReturn` (0,0,False)
      nativeProcessCount `shouldReturn` 0

  it "kills production helpers on hard loss of the actual MCP parent" $
    withSourceFile "{}\n" $ \path -> do
      seen <- newTVarIO (0 :: Int); release <- newEmptyMVar
      let app _ respond = atomically (modifyTVar' seen (+1)) >> readMVar release >> respond (Wai.responseLBS status200 [] "{}")
      testWithApplication (pure app) $ \port -> do
        executable <- testHelperExecutable
        let config = (Process.proc executable ["--server-url","http://127.0.0.1:" <> show port,"--no-auth","+RTS","-N1","-RTS"])
              {Process.std_in=Process.CreatePipe,Process.std_out=Process.CreatePipe,Process.std_err=Process.CreatePipe}
        bracket (Process.createProcess config) (\(input,output,errors,process) -> do
          pid <- Process.getPid process
          forM_ pid $ \value -> void (killProcessId (fromIntegral value))
          _ <- timeout 3000000 (Process.waitForProcess process)
          forM_ [input,output,errors] $ mapM_ (\handle -> hIsClosed handle >>= \closed -> unless closed (hClose handle))) $ \(input,output,errors,process) ->
          case (input,output,errors) of
            (Just send,Just receive,Just diagnostics) -> do
              drained <- newEmptyMVar; errorsDrained <- newEmptyMVar
              let drain handle = BS.hGetSome handle 4096 >>= \bytes -> unless (BS.null bytes) (drain handle)
              _ <- forkIO (drain receive `finally` putMVar drained ())
              _ <- forkIO (drain diagnostics `finally` putMVar errorsDrained ())
              ids <- replicateM 2 UUID.nextRandom
              let call name arguments = object ["jsonrpc" .= ("2.0" :: Text),"id" .= (name :: Text),"method" .= ("tools/call" :: Text),"params" .= object ["name" .= name,"arguments" .= arguments]]
                  requests = [object ["jsonrpc" .= ("2.0" :: Text),"id" .= ("init" :: Text),"method" .= ("initialize" :: Text)]
                    ,call "set_workspace" (object ["workspace_id" .= workspace,"identity" .= identity])]
                    <> [call "session_upload_start" (object ["session_id" .= sid,"path" .= path,"title" .= ("Test session work" :: Text), "format_label" .= ("jsonl" :: Text),"task_id" .= workspace]) | sid <- ids]
              BL.hPut send (BL.concat [encode request <> "\n" | request <- requests]); hFlush send
              flip finally (void (tryPutMVar release ())) $ do
                waitUntil $ (>= 2) <$> readTVarIO seen
                parent <- Process.getPid process >>= maybe (throwIO ProcessUnavailable) (pure . fromIntegral)
                children <- childProcessIds parent
                images <- mapM (processImage >=> canonicalizePath) children
                helper <- canonicalizePath executable
                consoles <- if os == "mingw32" then do
                  root <- getEnv "SystemRoot"
                  image <- canonicalizePath (root </> "system32" </> "conhost.exe")
                  pure [image]
                  else pure []
                let same left right = (if os == "mingw32" then map toLower else id) left == (if os == "mingw32" then map toLower else id) right
                length (filter (`same` helper) images) `shouldBe` 2
                images `shouldSatisfy` all (\image -> any (same image) (helper:consoles))
                length (filter (\image -> any (same image) consoles) images) `shouldSatisfy` (<= 1)
                killProcessId parent `shouldReturn` True
                timeout 3000000 (Process.waitForProcess process) >>= (`shouldSatisfy` isJustValue)
                waitUntil $ and <$> mapM (fmap not . processAlive) children
                timeout 1000000 (readMVar drained) >>= (`shouldSatisfy` isJustValue)
                timeout 1000000 (readMVar errorsDrained) >>= (`shouldSatisfy` isJustValue)
                readTVarIO seen `shouldReturn` 2
            _ -> expectationFailure "MCP parent private pipes missing"

  it "cleans Windows attribute-initialization failure without deleting uninitialized attributes or leaking handles" $ do
    helper <- testHelperExecutable
    code <- attributeFailureProbe helper
    if code == 2 then pendingWith "Windows process attribute API is unavailable on this platform" else code `shouldBe` 1
    nativeProcessCount `shouldReturn` 0

  it "reads from a real regular handle and detects replacement, truncation and same-size preserved-mtime rewrites" $
    withSourceFile "{}\r\n" $ \path -> bracket (openSource path) closeSource $ \source -> do
      checkSource source 4 `shouldReturn` 4
      readSource source 0 4 `shouldReturn` "{}\r\n"
      stamp <- getModificationTime path
      BS.writeFile path "[]\r\n"
      setModificationTime path stamp
      checkSource source 4 `shouldReturn` 4
      readSource source 0 4 `shouldReturn` "[]\r\n"
      BS.writeFile path "{}"
      checkSource source 4 `shouldThrow` (== SourceTruncated)
      BS.writeFile path "{}\r\n"
      renameFile path (path <> ".old")
      BS.writeFile path "{}\r\n"
      checkSource source 4 `shouldThrow` (== SourceChanged)

  it "fails closed for missing/nonregular sources without path disclosure" $
    withSourceFile "" $ \path -> do
      openSource (path <> ".missing") `shouldThrow` (== SourceUnavailable)
      directory <- getTemporaryDirectory
      openSource directory `shouldThrow` (== SourceUnavailable)
      withManager $ \manager -> withRegistry fast $ \registry -> do
        sid <- UUID.nextRandom
        started <- start registry manager "http://unused.invalid" workspace sid (path <> ".missing")
        started `shouldSatisfy` (not . contains (T.pack path))
        failed <- await registry manager "http://unused.invalid" workspace sid (hasStatus "failed")
        failed `shouldSatisfy` (not . contains (T.pack path))

  it "automatically uploads exact CRLF/UTF8, holds partial tails, flushes a final record and resumes across EOF separators" $
    withBackend $ \env wid manager base -> withSourceFile "{\"event\":1}\r\n" $ \path -> withRegistry fast $ \registry -> do
      sid <- UUID.nextRandom
      started <- start registry manager base wid sid path
      hasStatus "validating" started `shouldBe` True
      _ <- await registry manager base wid sid (hasOffset 13)
      let unicode = TE.encodeUtf8 "{\"s\":\"雪\"}"
      BS.appendFile path (BS.take 8 unicode)
      pending <- await registry manager base wid sid (hasStatus "pending_tail")
      pending `shouldSatisfy` hasOffset 13
      BS.appendFile path (BS.drop 8 unicode)
      _ <- control registry manager base wid sid "session_upload_flush"
      flushed <- await registry manager base wid sid hasReceipt
      let expected = "{\"event\":1}\r\n" <> unicode
      receiptHash flushed `shouldBe` Just (String (Core.sessionSha256 expected))
      BS.appendFile path "\r\n{\"next\":2}\n"
      _ <- await registry manager base wid sid (hasOffset (BS.length expected + BS.length "\r\n{\"next\":2}\n"))
      _ <- control registry manager base wid sid "session_upload_stop"
      stopped <- await registry manager base wid sid (hasStatus "stopped")
      receiptHash stopped `shouldBe` Just (String (Core.sessionSha256 (expected <> "\r\n{\"next\":2}\n")))
      session <- Sessions.getSession env.pool wid sid
      fmap (.state) session `shouldBe` Just Core.Active
      range <- Sessions.readSessionRange env.pool wid sid 0 1048576
      range.bytes `shouldBe` expected <> "\r\n{\"next\":2}\n"

  it "retains invalid EOF tails, fails malformed terminated records and detects persistent same-size committed rewrites" $
    withBackend $ \_ wid manager base -> do
      withSourceFile "{}\n{broken" $ \path -> withRegistry fast $ \registry -> do
        sid <- UUID.nextRandom
        _ <- start registry manager base wid sid path
        _ <- await registry manager base wid sid (hasStatus "pending_tail")
        _ <- control registry manager base wid sid "session_upload_stop"
        stopped <- await registry manager base wid sid (hasStatus "stopped")
        (field "pending_tail" =<< field "receipt" stopped) `shouldBe` Just (Bool True)
      withSourceFile "{broken}\n" $ \path -> withRegistry fast $ \registry -> do
        sid <- UUID.nextRandom
        _ <- start registry manager base wid sid path
        _ <- await registry manager base wid sid (hasStatus "failed")
        pure ()
      withSourceFile "{}\n" $ \path -> withRegistry fast $ \registry -> do
        sid <- UUID.nextRandom
        _ <- start registry manager base wid sid path
        _ <- await registry manager base wid sid (hasOffset 3)
        stamp <- getModificationTime path
        BS.writeFile path "[]\n"
        setModificationTime path stamp
        changed <- await registry manager base wid sid (hasStatus "failed")
        field "error" changed `shouldBe` Just (String "source_identity_or_prefix_changed")

  it "retries an exact lost-ack append, verifies the full prefix on restart, and refuses a mismatched resume" $
    withBackend $ \env wid manager base -> withSourceFile "{}\n" $ \path -> withCapturedHTTP base $ \captured -> do
      sid <- UUID.nextRandom
      lost <- newTVarIO False
      sent <- newTVarIO ([] :: [Value])
      let action verb target body = do
            result <- followerRequest captured verb target body
            if "/append" `T.isSuffixOf` T.pack target then do
              first <- atomically $ do
                previous <- readTVar lost
                writeTVar lost True
                modifyTVar' sent (<> maybe [] pure body)
                pure (not previous)
              if first then pure (Left HTTPUnavailable) else pure result
            else pure result
      withRegistry fast {requestOverride = Just action} $ \registry -> do
        _ <- start registry manager base wid sid path
        _ <- await registry manager base wid sid (hasOffset 3)
        requests <- readTVarIO sent
        length requests `shouldSatisfy` (>= 2)
        take 2 requests `shouldBe` replicate 2 (head requests)
      session <- Sessions.getSession env.pool wid sid
      fmap (.state) session `shouldBe` Just Core.Active
      BS.appendFile path "[]\n"
      withRegistry fast $ \registry -> do
        _ <- start registry manager base wid sid path
        _ <- await registry manager base wid sid (hasOffset 6)
        _ <- control registry manager base wid sid "session_upload_stop"
        _ <- await registry manager base wid sid (hasStatus "stopped")
        pure ()
      BS.writeFile path "[ ]\n[]\n"
      withRegistry fast $ \registry -> do
        _ <- start registry manager base wid sid path
        _ <- await registry manager base wid sid (hasStatus "failed")
        pure ()

  it "caps twelve jobs and every follower HTTP category at two while normal terminate and context changes progress" $
    withBackend $ \_ wid manager base -> withSourceFile "{}\n" $ \path -> do
      blocked <- newEmptyMVar
      seen <- newTVarIO ([] :: [String])
      let action _ target _ = atomically (modifyTVar' seen (<> [target])) >> readMVar blocked >> pure (Left HTTPUnavailable)
      withRegistry fast {requestOverride = Just action} $ \registry -> do
        ids <- replicateM 13 UUID.nextRandom
        forM_ (take 11 ids) $ \sid -> start registry manager base wid sid path >>= (\result -> hasStatus "validating" result `shouldBe` True)
        let imported sid = payload <$> handleUploadTool registry manager base Nothing identity "session_import"
              (object ["workspace_id" .= wid,"session_id" .= sid,"task_id" .= wid,"path" .= path,"title" .= ("Test historical work" :: Text),"format_label" .= ("jsonl" :: Text)])
        imported (ids !! 11) >>= (\result -> hasStatus "validating" result `shouldBe` True)
        full <- start registry manager base wid (last ids) path
        nested ["error","code"] full `shouldBe` Just (String "capacity")
        importFull <- imported (last ids)
        nested ["error","code"] importFull `shouldBe` Just (String "capacity")
        importRetry <- imported (ids !! 11)
        field "error" importRetry `shouldBe` Just Null
        duplicate <- start registry manager base wid (head ids) path
        nested ["error","code"] duplicate `shouldBe` Just (String "already_following")
        _ <- waitUntil $ do (active,peak,_) <- registryMetrics registry; pure (active == 2 && peak == 2)
        initialized <- newTVarIO True
        context <- newTVarIO (Just (MCPContext (Just wid) identity))
        normal <- UUID.nextRandom
        _ <- rpc registry manager base initialized context "session_create" (object ["session_id" .= normal, "title" .= ("Test session work" :: Text), "format_label" .= ("jsonl" :: Text),"task_id" .= wid])
        terminated <- timeout 1000000 $ rpc registry manager base initialized context "session_terminate" (object ["session_id" .= normal,"expected_offset" .= (0 :: Int),"expected_revision" .= (0 :: Int),"final_bytes" .= (0 :: Int),"final_sha256" .= Core.sessionSha256 ""])
        terminated `shouldSatisfy` isJustValue
        other <- UUID.nextRandom
        _ <- rpc registry manager base initialized context "set_workspace" (object ["workspace_id" .= other, "identity" .= identity])
        following <- readTVarIO seen
        following `shouldSatisfy` all (T.isInfixOf (T.pack (show wid)) . T.pack)
        controlResult <- timeout 200000 $ control registry manager base wid (head ids) "session_upload_stop"
        controlResult `shouldSatisfy` isJustValue
        (active,peak,total) <- registryMetrics registry
        (active,peak,total) `shouldBe` (2,2,12)

  it "observes explicit live termination and closes without another append or heartbeat" $
    withBackend $ \env wid manager base -> withSourceFile "{}\n" $ \path -> withRegistry fast $ \registry -> do
      sid <- UUID.nextRandom
      _ <- start registry manager base wid sid path
      _ <- await registry manager base wid sid (hasOffset 3)
      Just session <- Sessions.getSession env.pool wid sid
      _ <- Sessions.terminateSession env.pool wid sid (Core.TerminateSession session.committedBytes session.revision 3 (Core.sessionSha256 "{}\n"))
      BS.appendFile path "[]\n"
      _ <- await registry manager base wid sid (hasStatus "terminated")
      Just sealed <- Sessions.getSession env.pool wid sid
      sealed.committedBytes `shouldBe` 3

  it "freezes a stop boundary during long verification and returns pending instead of a premature receipt" $
    withBackend $ \env wid manager base -> do
      let original = BS.concat (replicate 80000 "{}\n")
      withSourceFile original $ \path -> withCapturedHTTP base $ \captured -> do
        sid <- UUID.nextRandom
        _ <- Sessions.createSession env.pool (Core.CreateSession sid wid "jsonl" "Test session work" Nothing Nothing wid)
        _ <- Sessions.appendSession env.pool wid sid (Core.AppendSession 0 0 original False)
        seen <- newEmptyMVar; release <- newEmptyMVar; blocked <- newTVarIO False
        let action verb target body = do
              first <- if "/prefix-verifications" `T.isInfixOf` T.pack target then atomically $ do previous <- readTVar blocked; writeTVar blocked True; pure (not previous) else pure False
              if first then putMVar seen () >> readMVar release else pure ()
              followerRequest captured verb target body
        withRegistry fast {requestOverride = Just action} $ \registry -> do
          _ <- start registry manager base wid sid path
          timeout 2000000 (readMVar seen) >>= (`shouldSatisfy` isJustValue)
          waiting <- control registry manager base wid sid "session_upload_stop"
          nested ["operation","pending"] waiting `shouldBe` Just (Bool True)
          field "receipt" waiting `shouldBe` Just Null
          putMVar release ()
          _ <- await registry manager base wid sid (\value -> nested ["operation","capture_boundary"] value == Just (Number (fromIntegral (BS.length original))))
          BS.appendFile path "[]\n"
          stopped <- await registry manager base wid sid (hasStatus "stopped")
          nested ["receipt","capture_boundary"] stopped `shouldBe` Just (Number (fromIntegral (BS.length original)))
          receiptHash stopped `shouldBe` Just (String (Core.sessionSha256 original))
          Just canonical <- Sessions.getSession env.pool wid sid
          canonical.committedBytes `shouldBe` fromIntegral (BS.length original)
          canonical.state `shouldBe` Core.Active

  it "fairly progresses four jobs through create/read/append/verification/heartbeat under the shared gate" $
    withBackend $ \_ wid manager base -> withSourceFile (BS.concat (replicate 30000 "{}\n")) $ \path -> withCapturedHTTP base $ \captured -> do
      offset <- newTVarIO (0 :: NominalDiffTime)
      calls <- newTVarIO ([] :: [(BS.ByteString,String)])
      let clock = do delta <- readTVarIO offset; addUTCTime delta <$> getCurrentTime
          action verb target body = do
            atomically $ modifyTVar' calls (<> [(verb,target)])
            threadDelay 10000
            followerRequest captured verb target body
      withRegistry fast {requestOverride = Just action, now = clock} $ \registry -> do
        ids <- replicateM 4 UUID.nextRandom
        forM_ ids $ \sid -> start registry manager base wid sid path >> pure ()
        forM_ ids $ \sid -> await registry manager base wid sid (hasOffset 90000) >> pure ()
        atomically $ writeTVar offset 11
        waitUntil $ any (T.isSuffixOf "/heartbeat" . T.pack . snd) <$> readTVarIO calls
        BS.appendFile path "[]\n"
        forM_ ids $ \sid -> await registry manager base wid sid (hasOffset 90003) >> pure ()
        forM_ ids $ \sid -> control registry manager base wid sid "session_upload_stop" >> pure ()
        forM_ ids $ \sid -> await registry manager base wid sid (hasStatus "stopped") >> pure ()
        (active,peak,total) <- registryMetrics registry
        active `shouldBe` 0
        peak `shouldSatisfy` (\n -> n >= 1 && n <= 2)
        total `shouldBe` 4
        observed <- readTVarIO calls
        forM_ ["/sessions","/append","/heartbeat","/prefix-verifications"] $ \category ->
          observed `shouldSatisfy` any (T.isInfixOf category . T.pack . snd)
        observed `shouldSatisfy` all (not . T.isInfixOf "/transcript?" . T.pack . snd)

  it "fails closed on revoked permissions, reports staleness independently, and bounds terminal retention" $
    withBackend $ \_ wid manager base -> withSourceFile "{}\n" $ \path -> withCapturedHTTP base $ \captured -> do
      offset <- newTVarIO (0 :: NominalDiffTime)
      revoked <- newTVarIO False
      let clock = do delta <- readTVarIO offset; addUTCTime delta <$> getCurrentTime
          action verb target body = do
            deny <- readTVarIO revoked
            if deny then pure (Left (HTTPStatus 403)) else followerRequest captured verb target body
      withRegistry fast {requestOverride = Just action, now = clock} $ \registry -> do
        sid <- UUID.nextRandom
        _ <- start registry manager base wid sid path
        _ <- await registry manager base wid sid (hasOffset 3)
        -- An already accepted HTTP request may still acknowledge after
        -- revocation. Establish the terminal state before advancing staleness.
        atomically $ writeTVar revoked True >> writeTVar offset 11
        failed <- await registry manager base wid sid (hasStatus "failed")
        field "error" failed `shouldBe` Just (String "permission_revoked")
        field "stale" failed `shouldBe` Just (Bool False)
        atomically $ writeTVar offset 42
        stale <- await registry manager base wid sid (hasStatus "failed")
        field "stale" stale `shouldBe` Just (Bool True)
        forM_ [1..20 :: Int] $ \_ -> do
          another <- UUID.nextRandom
          _ <- start registry manager base wid another (path <> ".missing")
          _ <- await registry manager base wid another (hasStatus "failed")
          pure ()
        (_,_,retained) <- registryMetrics registry
        retained `shouldSatisfy` (<= 16)
        atomically $ writeTVar offset 400
        (_,_,expired) <- registryMetrics registry
        expired `shouldBe` 0

  it "refuses replacement/truncation and a missing EOF separator without advancing evidence" $
    withBackend $ \env wid manager base -> do
      forM_ [False,True] $ \replace -> withSourceFile "{}\n" $ \path -> withRegistry fast $ \registry -> do
        sid <- UUID.nextRandom
        _ <- start registry manager base wid sid path
        _ <- await registry manager base wid sid (hasOffset 3)
        if replace then renameFile path (path <> ".old") >> BS.writeFile path "{}\n" else BS.writeFile path ""
        _ <- await registry manager base wid sid (hasStatus "failed")
        pure ()
      withSourceFile "{}" $ \path -> withRegistry fast $ \registry -> do
        sid <- UUID.nextRandom
        _ <- start registry manager base wid sid path
        _ <- await registry manager base wid sid (hasStatus "pending_tail")
        _ <- control registry manager base wid sid "session_upload_flush"
        _ <- await registry manager base wid sid hasReceipt
        BS.appendFile path "[]\n"
        _ <- await registry manager base wid sid (hasStatus "failed")
        Just canonical <- Sessions.getSession env.pool wid sid
        canonical.committedBytes `shouldBe` 2
        canonical.state `shouldBe` Core.Active

  it "closes and joins workers on normal/exceptional lifetime and cleans partially started pools" $
    withManager $ \manager -> withSourceFile "{}\n" $ \path -> do
      ids <- newTVarIO []
      let fork :: ((forall a. IO a -> IO a) -> IO ()) -> IO ThreadId
          fork action = do tid <- forkIOWithUnmask action; atomically $ modifyTVar' ids (<> [tid]); pure tid
          options = fast {forkWorker = fork, requestOverride = Just (\_ _ _ -> atomically retry)}
      registry <- newRegistryWith options
      sid <- UUID.nextRandom
      _ <- start registry manager "http://unused.invalid" workspace sid path
      _ <- waitUntil $ do (active,_,_) <- registryMetrics registry; pure (active == 1)
      closeRegistry registry `shouldReturn` True
      openSourceCount `shouldReturn` 0
      threads <- readTVarIO ids
      forM_ threads $ \tid -> threadStatus tid >>= (`shouldSatisfy` (\s -> s == ThreadFinished || s == ThreadDied))
      BS.appendFile path "[]\n"
      counter <- newTVarIO (0 :: Int)
      let failingFork :: ((forall a. IO a -> IO a) -> IO ()) -> IO ThreadId
          failingFork action = do
            n <- atomically $ do n <- readTVar counter; writeTVar counter (n+1); pure n
            if n == 1 then throwIO (userError "injected startup failure") else fork action
      newRegistryWith options {forkWorker = failingFork} `shouldThrow` anyIOException
      allThreads <- readTVarIO ids
      waitUntil $ and <$> mapM (\tid -> do s <- threadStatus tid; pure (s == ThreadFinished || s == ThreadDied)) allThreads
      openSourceCount `shouldReturn` 0

  it "publishes startup readiness inside completion ownership before cancellation can enter either worker" $ do
    ids <- newTVarIO []
    entered <- newTVarIO (0 :: Int); release <- newEmptyMVar
    let fork :: ((forall a. IO a -> IO a) -> IO ()) -> IO ThreadId
        fork action = do
          tid <- forkIOWithUnmask $ \unmask -> action $ \work -> unmask $ do
            atomically $ modifyTVar' entered (+1)
            readMVar release
            work
          atomically $ modifyTVar' ids (<> [tid])
          pure tid
    registry <- newRegistryWith fast {forkWorker = fork}
    flip finally (void (tryPutMVar release ())) $ do
      waitUntil $ (== 2) <$> readTVarIO entered
      closeRegistry registry `shouldReturn` True
      threads <- readTVarIO ids
      length threads `shouldBe` 2
      forM_ threads $ \tid -> threadStatus tid >>= (`shouldSatisfy` (\state -> state == ThreadFinished || state == ThreadDied))
      openSourceCount `shouldReturn` 0
      nativeProcessCount `shouldReturn` 0

  it "interleaves idle heartbeats with a complete multi-megabyte resume verification" $
    withManager $ \manager -> do
      let chunk = BS.concat (replicate 30 ("\"" <> BS.replicate 32765 97 <> "\"\n"))
          original = chunk <> chunk
          wid = workspace
          base = "http://unused.invalid"
      withSourceFile original $ \path -> do
        sid <- UUID.nextRandom
        offset <- newTVarIO (0 :: NominalDiffTime)
        beats <- newTVarIO (0 :: Int)
        let clock = do delta <- readTVarIO offset; addUTCTime delta <$> getCurrentTime
            metadata _ target _ = do
              if "/heartbeat" `T.isSuffixOf` T.pack target then atomically $ modifyTVar' beats (+1) else pure ()
              pure (Right (fakeMetadata sid (BS.length original)))
            page name = when (name == "local_prefix_page") $ atomically $ modifyTVar' offset (+6)
        action <- fakeProofTransport sid original "pending" metadata
        withRegistry fast {requestOverride = Just action, now = clock, transition = page} $ \registry -> do
          _ <- start registry manager base wid sid path
          waitUntil $ (>= 2) <$> readTVarIO beats
          state <- control registry manager base wid sid "session_upload_status"
          hasStatus "validating" state `shouldBe` True

  it "anchors acknowledgement, idle heartbeat and retry deadlines to delayed HTTP completion, never local saves" $
    withManager $ \manager -> withSourceFile "" $ \path -> do
      sid <- UUID.nextRandom
      clock <- newTVarIO epoch
      gotMetadata <- newEmptyMVar; finishMetadata <- newEmptyMVar
      gotHeartbeat <- newEmptyMVar; finishHeartbeat <- newEmptyMVar
      gotProof <- newEmptyMVar; finishProof <- newEmptyMVar; blockedProof <- newTVarIO False
      beats <- newTVarIO (0 :: Int)
      let metadata _ target _
            | "/heartbeat" `T.isSuffixOf` T.pack target = do
                n <- atomically $ do n <- readTVar beats; writeTVar beats (n+1); pure n
                if n == 0 then putMVar gotHeartbeat () >> readMVar finishHeartbeat else pure ()
                pure (Right (fakeMetadata sid 0))
            | otherwise = putMVar gotMetadata () >> readMVar finishMetadata >> pure (Right (fakeMetadata sid 0))
      proof <- fakeProofTransport sid "" "verified" metadata
      let action verb target body = do
            when ("/prefix-verifications" `T.isInfixOf` T.pack target) $ do
              first <- atomically $ do old <- readTVar blockedProof; writeTVar blockedProof True; pure (not old)
              when first $ putMVar gotProof () >> readMVar finishProof
            proof verb target body
          options = fast {intervalMicros = 0, now = readTVarIO clock, requestOverride = Just action}
      withRegistry options $ \registry -> do
        _ <- start registry manager "http://unused.invalid" workspace sid path
        timeout 2000000 (readMVar gotMetadata) >>= (`shouldSatisfy` isJustValue)
        atomically $ writeTVar clock (addUTCTime 5 epoch)
        putMVar finishMetadata ()
        timeout 2000000 (readMVar gotHeartbeat) >>= (`shouldSatisfy` isJustValue)
        ack <- control registry manager "http://unused.invalid" workspace sid "session_upload_status"
        field "last_ack_at" ack `shouldBe` Just (toJSON (addUTCTime 5 epoch))
        atomically $ writeTVar clock (addUTCTime 10 epoch)
        putMVar finishHeartbeat ()
        _ <- await registry manager "http://unused.invalid" workspace sid (\s -> field "last_ack_at" s == Just (toJSON (addUTCTime 10 epoch)))
        timeout 2000000 (readMVar gotProof) >>= (`shouldSatisfy` isJustValue)
        atomically $ writeTVar clock (addUTCTime 19 epoch)
        threadDelay 80000
        -- The pending resume proof request and local saves do not refresh
        -- the acknowledgement or idle heartbeat deadline.
        readTVarIO beats `shouldReturn` 1
        local <- control registry manager "http://unused.invalid" workspace sid "session_upload_status"
        field "last_ack_at" local `shouldBe` Just (toJSON (addUTCTime 10 epoch))
        atomically $ writeTVar clock (addUTCTime 20 epoch)
        putMVar finishProof ()
        -- At 20 s the idle heartbeat due from the 10 s completion runs, then
        -- the resume cursor checkpoint.
        _ <- await registry manager "http://unused.invalid" workspace sid (hasStatus "caught_up")
        readTVarIO beats `shouldReturn` 3
        -- Repeated local quiet sweeps do not refresh the idle heartbeat deadline.
        atomically $ writeTVar clock (addUTCTime 29 epoch)
        threadDelay 80000
        readTVarIO beats `shouldReturn` 3
        atomically $ writeTVar clock (addUTCTime 30 epoch)
        waitUntil $ (== 4) <$> readTVarIO beats
      attempts <- newTVarIO (0 :: Int)
      began <- newEmptyMVar; finish <- newEmptyMVar
      atomically $ writeTVar clock epoch
      another <- UUID.nextRandom
      let transient _ _ _ = do
            n <- atomically $ do n <- readTVar attempts; writeTVar attempts (n+1); pure n
            if n == 0 then putMVar began () >> readMVar finish >> pure (Left HTTPUnavailable)
            else pure (Left (HTTPStatus 403))
      withRegistry fast {intervalMicros = 0, now = readTVarIO clock, requestOverride = Just transient} $ \registry -> do
        _ <- start registry manager "http://unused.invalid" workspace another path
        timeout 2000000 (readMVar began) >>= (`shouldSatisfy` isJustValue)
        atomically $ writeTVar clock (addUTCTime 5 epoch)
        putMVar finish ()
        _ <- await registry manager "http://unused.invalid" workspace another (hasStatus "retry")
        threadDelay 80000
        readTVarIO attempts `shouldReturn` 1
        atomically $ writeTVar clock (addUTCTime 6 epoch)
        _ <- await registry manager "http://unused.invalid" workspace another (hasStatus "failed")
        readTVarIO attempts `shouldReturn` 2

  it "PrefixProofClient bounds pending-poll backoff and periodic verification scaling" $ do
    let mib = 1048576
    map (pendingPollDelay 0.05 1) [0, 0.1, 0.25, 0.5, 1.9, 2, 30] `shouldBe` [0.05, 0.05, 0.125, 0.25, 0.95, 1, 1]
    map (pendingPollDelay 0.001 0.001) [0, 0.5, 30] `shouldBe` [0.001, 0.001, 0.001]
    map (periodicVerificationDelay 10) [0, mib, 16*mib, 24*mib, 32*mib, 96*mib, 100*mib, 10240*mib] `shouldBe` [10, 10, 10, 15, 20, 60, 60, 60]

  it "PrefixProofClient spaces polls from the completion of a slow pending reply" $
    withManager $ \manager -> withSourceFile "" $ \path -> do
      sid <- UUID.nextRandom
      clock <- newTVarIO epoch
      lookups <- newTVarIO (0 :: Int)
      firstLookup <- newEmptyMVar
      finishLookup <- newEmptyMVar
      let base = "http://unused.invalid"
          at seconds = addUTCTime seconds epoch
          setClock seconds = atomically $ writeTVar clock (at seconds)
          acknowledged seconds state = field "last_ack_at" state == Just (toJSON (at seconds))
          metadata _ _ _ = pure (Right (fakeMetadata sid 0))
      proof <- fakeProofTransport sid "" "pending" metadata
      let action verb target body
            | "/prefix-verifications" `T.isSuffixOf` route = do
                setClock 0.02
                proof verb target body
            | "/prefix-verifications/" `T.isInfixOf` route = do
                count <- atomically $ do
                  current <- readTVar lookups
                  writeTVar lookups (current + 1)
                  pure current
                if count == 0 then putMVar firstLookup () >> readMVar finishLookup >> proof verb target body
                else do
                  result <- proof verb target body
                  pure $ case result of
                    Right (Object fields) -> Right (Object (KM.insert "state" (String "verified") fields))
                    _ -> result
            | otherwise = proof verb target body
            where route = T.pack target
          options = fast {intervalMicros = 1000000, now = readTVarIO clock, requestOverride = Just action}
      withRegistry options $ \registry ->
        flip finally (void (tryPutMVar finishLookup ())) $ do
          _ <- start registry manager base workspace sid path
          _ <- await registry manager base workspace sid (acknowledged 0.02)
          setClock 0.071
          timeout 2000000 (readMVar firstLookup) `shouldReturn` Just ()
          setClock 0.27
          putMVar finishLookup ()
          _ <- await registry manager base workspace sid (acknowledged 0.27)
          -- Pending for 0.25 s since the first pending reply: wait 0.125 s.
          setClock 0.394
          threadDelay 80000
          readTVarIO lookups `shouldReturn` 1
          setClock 0.396
          _ <- await registry manager base workspace sid (hasStatus "caught_up")
          readTVarIO lookups `shouldReturn` 2

  it "PrefixProofClient scales the periodic verification interval with the verified prefix" $
    withManager $ \manager -> do
      let original = BS.concat (replicate 32 (BS.replicate 1048575 97 <> "\n"))
          base = "http://unused.invalid"
      withSourceFile original $ \path -> do
        sid <- UUID.nextRandom
        clock <- newTVarIO epoch
        proofs <- newTVarIO (0 :: Int); pages <- newTVarIO (0 :: Int); beats <- newTVarIO (0 :: Int); sweeps <- newTVarIO (0 :: Int)
        let setClock seconds = atomically $ writeTVar clock (addUTCTime seconds epoch)
            metadata _ target _ = do
              when ("/heartbeat" `T.isSuffixOf` T.pack target) $ atomically $ modifyTVar' beats (+1)
              pure (Right (fakeMetadata sid (BS.length original)))
            page name = do
              when (name == "local_prefix_page") $ atomically $ modifyTVar' pages (+1)
              when (name == "periodic_verification_complete") $ atomically $ modifyTVar' sweeps (+1)
            reach label condition = do
              let loop = condition >>= \ready -> unless ready (threadDelay 10000 >> loop)
              reached <- timeout 3000000 loop
              counts <- (,,,) <$> readTVarIO proofs <*> readTVarIO pages <*> readTVarIO beats <*> readTVarIO sweeps
              unless (isJustValue reached) $ expectationFailure (label <> ": (proofs,pages,beats,sweeps) = " <> show counts)
        proof <- fakeProofTransport sid original "verified" metadata
        let action verb target body = do
              when ("/prefix-verifications" `T.isInfixOf` T.pack target) $ atomically $ modifyTVar' proofs (+1)
              proof verb target body
        withRegistry fast {intervalMicros = 1000000, now = readTVarIO clock, requestOverride = Just action, transition = page} $ \registry -> do
          _ <- start registry manager base workspace sid path
          _ <- await registry manager base workspace sid (hasStatus "caught_up")
          -- Resume: one proof request, a hashing pass and the second local pass.
          -- Unchanged quiet sweeps rehash locally and need no further request.
          readTVarIO proofs `shouldReturn` 1
          readTVarIO pages `shouldReturn` 64
          resumeBeats <- readTVarIO beats
          -- A 32 MiB prefix waits 20 s rather than the 10 s base interval.
          setClock 15
          reach "idle heartbeat at 15 s" $ (> resumeBeats) <$> readTVarIO beats
          threadDelay 200000
          readTVarIO pages `shouldReturn` 64
          setClock 21
          reach "periodic sweep at 21 s" $ (== 1) <$> readTVarIO sweeps
          readTVarIO pages `shouldReturn` 96
          periodicBeats <- readTVarIO beats
          setClock 40
          reach "idle heartbeat at 40 s" $ (> periodicBeats) <$> readTVarIO beats
          threadDelay 200000
          readTVarIO pages `shouldReturn` 96
          setClock 41.5
          reach "periodic sweep at 41.5 s" $ (== 2) <$> readTVarIO sweeps
          readTVarIO pages `shouldReturn` 128
          readTVarIO proofs `shouldReturn` 1

  it "PrefixProofClient completes unchanged quiet sweeps locally and submits a new operation once its proof is five minutes old" $
    quietFollower $ \_ _ _ _ setClock sweeps events -> do
      let proofRequests = filter ((/= "HEARTBEAT") . fst) <$> readTVarIO events
      recorded <- proofRequests
      map fst recorded `shouldBe` ["POST"]
      forM_ (zip [1..] [15, 150, 285]) $ \(count, seconds) -> do
        setClock seconds
        waitUntil ((== count) <$> readTVarIO sweeps)
        proofRequests `shouldReturn` recorded
      -- The first sweep at 300 s submits a new UUID; the old one is never read.
      setClock 300
      waitUntil ((== 4) <$> readTVarIO sweeps)
      later <- proofRequests
      map fst later `shouldBe` ["POST","POST"]
      snd (later !! 1) `shouldNotBe` snd (head recorded)

  it "PrefixProofClient flush after skipped quiet sweeps still confirms the proof and checkpoints the cursor before its receipt" $
    quietFollower $ \registry manager sid _ setClock sweeps events -> do
      setClock 15
      waitUntil ((== 1) <$> readTVarIO sweeps)
      recorded <- readTVarIO events
      let first = snd (head (filter ((== "POST") . fst) recorded))
      _ <- control registry manager "http://unused.invalid" workspace sid "session_upload_flush"
      flushed <- await registry manager "http://unused.invalid" workspace sid hasReceipt
      nested ["receipt","source_verified"] flushed `shouldBe` Just (Bool True)
      nested ["receipt","committed_offset"] flushed `shouldBe` Just (Number 3)
      -- One remote confirmation of the recent proof, then the cursor checkpoint.
      drop (length recorded) <$> readTVarIO events `shouldReturn` [("GET",first),("HEARTBEAT","")]
      -- Re-receiving that result does not extend its lifetime from 0 s.
      setClock 300
      waitUntil ((== 2) <$> readTVarIO sweeps)
      map fst . filter ((/= "HEARTBEAT") . fst) <$> readTVarIO events `shouldReturn` ["POST","GET","POST"]

  it "PrefixProofClient submits a new operation when a quiet sweep's digest changes at the same boundary" $
    quietFollower $ \registry manager sid path setClock _ events -> do
      stamp <- getModificationTime path
      BS.writeFile path "[]\n"
      setModificationTime path stamp
      setClock 15
      failed <- await registry manager "http://unused.invalid" workspace sid (hasStatus "failed")
      field "error" failed `shouldBe` Just (String "source_identity_or_prefix_changed")
      map fst . filter ((/= "HEARTBEAT") . fst) <$> readTVarIO events `shouldReturn` ["POST","POST"]

  it "atomically admits/completes controls across retirement and stop-finalization races" $
    withManager $ \manager -> withSourceFile "" $ \path -> do
      let base = "http://unused.invalid"
      sid <- UUID.nextRandom
      requestSeen <- newEmptyMVar; failRequest <- newEmptyMVar
      admissionSeen <- newEmptyMVar; admit <- newEmptyMVar
      let failing _ _ _ = putMVar requestSeen () >> readMVar failRequest >> pure (Left (HTTPStatus 403))
          admission name = if name == "before_control_admission" then putMVar admissionSeen () >> readMVar admit else pure ()
      withRegistry fast {requestOverride = Just failing, transition = admission} $ \registry -> do
        _ <- start registry manager base workspace sid path
        timeout 2000000 (readMVar requestSeen) >>= (`shouldSatisfy` isJustValue)
        reply <- newEmptyMVar
        bracket (forkIO (control registry manager base workspace sid "session_upload_stop" >>= putMVar reply)) killThread $ \_ -> do
          timeout 2000000 (readMVar admissionSeen) >>= (`shouldSatisfy` isJustValue)
          putMVar failRequest ()
          _ <- await registry manager base workspace sid (hasStatus "failed")
          putMVar admit ()
          refused <- timeout 2000000 (readMVar reply)
          refused `shouldSatisfy` maybe False (\value -> field "operation" value == Just Null && field "requested_operation" value == Nothing)
      another <- UUID.nextRandom
      terminalSeen <- newEmptyMVar; publishTerminal <- newEmptyMVar
      let terminalHook name = if name == "before_terminal_publication" then putMVar terminalSeen () >> readMVar publishTerminal else pure ()
      withRegistry fast {requestOverride = Just (\_ _ _ -> pure (Left (HTTPStatus 403))), transition = terminalHook} $ \registry -> do
        _ <- start registry manager base workspace another path
        timeout 2000000 (readMVar terminalSeen) >>= (`shouldSatisfy` isJustValue)
        accepted <- control registry manager base workspace another "session_upload_stop"
        nested ["requested_operation","pending"] accepted `shouldBe` Just (Bool True)
        putMVar publishTerminal ()
        failed <- await registry manager base workspace another (hasStatus "failed")
        nested ["operation","pending"] failed `shouldBe` Just (Bool False)
        nested ["operation","receipt","error"] failed `shouldBe` Just (String "permission_revoked")
      lastSid <- UUID.nextRandom
      finalizing <- newEmptyMVar; publishStop <- newEmptyMVar
      clock <- newTVarIO epoch
      let stopHook name = if name == "stop_finalizing" then atomically (writeTVar clock (addUTCTime 10 epoch)) >> putMVar finalizing () >> readMVar publishStop else pure ()
      stopAction <- fakeProofTransport lastSid "" "verified" (\_ _ _ -> pure (Right (fakeMetadata lastSid 0)))
      withRegistry fast {intervalMicros = 0, now = readTVarIO clock, requestOverride = Just stopAction, transition = stopHook} $ \registry -> do
        _ <- start registry manager base workspace lastSid path
        _ <- await registry manager base workspace lastSid (hasStatus "caught_up")
        reply <- newEmptyMVar
        bracket (forkIO (control registry manager base workspace lastSid "session_upload_stop" >>= putMVar reply)) killThread $ \_ -> do
          timeout 2000000 (readMVar finalizing) >>= (`shouldSatisfy` isJustValue)
          stopping <- control registry manager base workspace lastSid "session_upload_status"
          hasStatus "finalizing" stopping `shouldBe` True
          nested ["operation","pending"] stopping `shouldBe` Just (Bool True)
          racingFlush <- control registry manager base workspace lastSid "session_upload_flush"
          field "requested_operation" racingFlush `shouldBe` Nothing
          nested ["operation","id"] racingFlush `shouldBe` nested ["operation","id"] stopping
          putMVar publishStop ()
          stopped <- await registry manager base workspace lastSid (hasStatus "stopped")
          nested ["operation","pending"] stopped `shouldBe` Just (Bool False)
          field "last_ack_at" stopped `shouldBe` Just (toJSON epoch)
          timeout 2000000 (readMVar reply) >>= (`shouldSatisfy` isJustValue)

  it "drains ordinary stdio epochs without awaiting blocked followers and closes the actual handle lifetime" $
    withSourceFile "{}\n" $ \path -> withPipe $ \(input,send) -> withPipe $ \(receive,output) -> withTempHandle $ \errors -> do
      seen <- newTVarIO (0 :: Int)
      release <- newEmptyMVar
      ids <- replicateM 2 UUID.nextRandom
      beginning <- getCurrentTime
      phases <- newTVarIO ([] :: [(Text,NominalDiffTime)])
      let stamp name = do time <- getCurrentTime; atomically $ modifyTVar' phases (<> [(name,diffUTCTime time beginning)])
          receipts = readTVarIO phases >>= putStrLn . ("stdio lifetime phase seconds: " <>) . show
      let call :: Text -> Value -> Value
          call name args = object ["jsonrpc" .= ("2.0" :: Text),"id" .= name,"method" .= ("tools/call" :: Text),"params" .= object ["name" .= name,"arguments" .= args]]
          initial = object ["jsonrpc" .= ("2.0" :: Text),"id" .= ("init" :: Text),"method" .= ("initialize" :: Text)]
          app _ respond = do
            atomically $ modifyTVar' seen (+1)
            readMVar release
            respond (Wai.responseLBS status200 [] "{}")
      testWithApplication (pure app) $ \port -> withManager $ \manager -> do
        workers <- newEmptyMVar
        finished <- newEmptyMVar
        followerIds <- newTVarIO []
        let observe tids = stamp "EOF observed" >> putMVar workers tids
            forkFollower :: ((forall a. IO a -> IO a) -> IO ()) -> IO ThreadId
            forkFollower action = do
              tid <- forkIOWithUnmask $ \unmask ->
                action (\work -> unmask $ do
                  masking <- getMaskingState
                  stamp ("follower worker entry " <> T.pack (show masking))
                  work `finally` stamp "follower worker complete")
              atomically $ modifyTVar' followerIds (<> [tid])
              pure tid
            options = defaultFollowerOptions {forkWorker = forkFollower, transition = stamp, helperExecutable = testHelperExecutable}
            run = try @SomeException (runMCPServerWithHandlesObservedWithFollowerOptions options (\action -> forkIOWithUnmask (\unmask -> unmask action)) observe 2 8 input output errors manager ("http://127.0.0.1:" <> show port) Nothing) >>= putMVar finished
            exchange label request = do
              BL.hPut send (encode request <> "\n"); hFlush send
              reply <- timeout 1000000 (BS.hGetLine receive)
              case reply >>= decodeStrict' of
                Nothing -> expectationFailure ("ordinary response missing: " <> T.unpack label) >> pure Null
                Just value -> do
                  stamp label
                  TE.decodeUtf8 (fromMaybe "" reply) `shouldSatisfy` (not . T.isInfixOf (T.pack path))
                  pure value
        bracket (forkIO run) (\tid -> do _ <- forkIO (killThread tid); pure ()) $ \_ ->
          within 25000000 (do
            stamp "setup complete"
            _ <- exchange "initialize response" initial
            _ <- exchange "workspace response" (call "set_workspace" (object ["workspace_id" .= workspace,"identity" .= identity]))
            forM_ ids $ \sid -> do
              response <- exchange "start response" (call "session_upload_start" (object ["session_id" .= sid,"path" .= path,"title" .= ("Test session work" :: Text), "format_label" .= ("jsonl" :: Text),"task_id" .= workspace]))
              nested ["result","structuredContent","status"] response `shouldBe` Just (String "validating")
            waitUntil $ (>= 2) <$> readTVarIO seen
            stamp "both HTTP slots blocked"
            _ <- exchange "ordinary context response" (call "set_workspace" (object ["workspace_id" .= Null,"identity" .= identity]))
            cleared <- exchange "ordinary get response" (call "get_workspace" (object []))
            let contextValue = do
                  Object result <- field "result" cleared
                  Array content <- KM.lookup "content" result
                  first <- case foldr (:) [] content of Object value : _ -> Just value; _ -> Nothing
                  String encoded <- KM.lookup "text" first
                  decodeStrict' (TE.encodeUtf8 encoded)
            (contextValue >>= field "workspace_id") `shouldBe` Just Null
            cleanupStarted <- getCurrentTime
            stamp "EOF sent"
            hClose send
            tids <- timeout 1000000 (readMVar workers)
            tids `shouldSatisfy` isJustValue
            let joinWorkers = do
                  states <- mapM threadStatus (fromMaybe [] tids)
                  if all (\state -> state == ThreadFinished || state == ThreadDied) states then pure () else threadDelay 10000 >> joinWorkers
            joined <- timeout 2500000 joinWorkers
            joined `shouldSatisfy` isJustValue
            stamp "ordinary workers joined"
            result <- timeout 9500000 (readMVar finished)
            stamp "server returned"
            followerStates <- readTVarIO followerIds >>= mapM threadStatus
            stamp ("follower states " <> T.pack (show followerStates))
            result `shouldSatisfy` maybe False (either (const False) (const True))
            followerStates `shouldSatisfy` all (\state -> state == ThreadFinished || state == ThreadDied)
            openSourceCount `shouldReturn` 0
            nativeProcessCount `shouldReturn` 0
            stamp "native handles closed"
            cleanupFinished <- getCurrentTime
            diffUTCTime cleanupFinished cleanupStarted `shouldSatisfy` (< 9.5)
          ) `finally` (do closed <- hIsClosed send; unless closed (hClose send); putMVar release (); receipts)

-- A caught-up three-byte follower with an injected clock and a fake transport
-- that verifies proofs at once. Events list proof requests as (verb, operation
-- UUID) and heartbeats; the counter tracks completed periodic sweeps.
quietFollower :: (Registry -> Manager -> UUID -> FilePath -> (NominalDiffTime -> IO ()) -> TVar Int -> TVar [(String,String)] -> IO a) -> IO a
quietFollower body = withManager $ \manager -> withSourceFile "{}\n" $ \path -> do
  sid <- UUID.nextRandom
  clock <- newTVarIO epoch
  sweeps <- newTVarIO 0; events <- newTVarIO []
  let base = "http://unused.invalid"
      setClock seconds = atomically $ writeTVar clock (addUTCTime seconds epoch)
      note event = atomically $ modifyTVar' events (<> [event])
      metadata _ target _ = do
        when ("/heartbeat" `T.isSuffixOf` T.pack target) $ note ("HEARTBEAT","")
        pure (Right (fakeMetadata sid 3))
      hook name = when (name == "periodic_verification_complete") $ atomically $ modifyTVar' sweeps (+1)
  proof <- fakeProofTransport sid "{}\n" "verified" metadata
  let action verb target request = do
        when ("/prefix-verifications" `T.isInfixOf` T.pack target) $ note $ if verb == "POST"
          then ("POST", maybe "" (\value -> case field "operation_id" value of Just (String operation) -> T.unpack operation; _ -> "") request)
          else (BS8.unpack verb, reverse (takeWhile (/= '/') (reverse target)))
        proof verb target request
  withRegistry fast {intervalMicros = 1000000, now = readTVarIO clock, requestOverride = Just action, transition = hook} $ \registry -> do
    _ <- start registry manager base workspace sid path
    _ <- await registry manager base workspace sid (hasStatus "caught_up")
    body registry manager sid path setClock sweeps events

fast :: FollowerOptions
fast = defaultFollowerOptions {intervalMicros = 1000, controlWaitMicros = 50000, helperExecutable = testHelperExecutable}

-- Opt-in traffic attribution: production follower cadence against a real
-- server, database and prefix registry, with matched three-follower cohorts.
-- Each cohort owns a fresh primary and replacement server process model that
-- share the database; a restart routes the followers to the replacement.
-- A mixed cohort has one quiet follower beside three live peers.
data TrafficMode = TrafficLive | TrafficQuiet | TrafficMixed deriving Eq

data TrafficServers = TrafficServers
  { route :: TVar CapturedHTTP, replacementHTTP :: CapturedHTTP, fixtureHTTP :: CapturedHTTP
  , skew :: TVar NominalDiffTime, workspaceId :: UUID, fixtureSession :: UUID }

measureSessionTraffic :: IO ()
measureSessionTraffic = withTestEnv $ \env -> do
  wid <- (.id) <$> createTestWorkspace env "traffic-measurement"
  runSession env.pool $ SQL.sql ("INSERT INTO tasks(id,workspace_id,title,status) VALUES ('"<>BS8.pack (show wid)<>"','"<>BS8.pack (show wid)<>"','Traffic measurement task','in_progress')")
  let mib = 1048576
      none _ = pure Null
  small <- trafficCohort env wid "small_live" mib TrafficLive none Nothing
  large <- trafficCohort env wid "large_live" (100*mib) TrafficLive none Nothing
  restart <- trafficCohort env wid "restart" mib TrafficQuiet (\servers -> Null <$ atomically (writeTVar servers.route servers.replacementHTTP)) (Just "unknown_since_start")
  expiry <- trafficCohort env wid "expiry" mib TrafficQuiet (\servers -> Null <$ atomically (modifyTVar' servers.skew (+301))) (Just "expired")
  eviction <- trafficCohort env wid "eviction" mib TrafficQuiet evictTerminalResults (Just "evicted")
  mixed <- trafficCohort env wid "mixed_churn" mib TrafficMixed none Nothing
  putStrLn ("session traffic measurement: " <> T.unpack (TE.decodeUtf8 (BL.toStrict (encode [small,large,restart,expiry,eviction,mixed]))))

trafficCohort :: TestEnv -> UUID -> Text -> Int64 -> TrafficMode -> (TrafficServers -> IO Value) -> Maybe Text -> IO Value
trafficCohort env wid name prefix mode disturb expected = do
  skew <- newTVarIO 0
  fixture <- UUID.nextRandom
  _ <- Sessions.createSession env.pool (Core.CreateSession fixture wid "jsonl" "Test session work" Nothing Nothing wid)
  let serverNow = addUTCTime <$> readTVarIO skew <*> getCurrentTime
      chunk = BS.concat (replicate 1024 ("{\"type\":\"traffic\",\"pad\":\"" <> BS.replicate 996 97 <> "\"}\n"))
      record = BS.concat (replicate 6 (BS.take 1024 chunk))
      unused = "http://unused.invalid"
  bracket (newPrefixRegistryAt serverNow env.pool) closePrefixRegistry $ \primary ->
    bracket (newPrefixRegistryAt serverNow env.pool) closePrefixRegistry $ \replacement ->
    withTrafficApp env primary $ \baseA -> withTrafficApp env replacement $ \baseB ->
    withCapturedHTTP baseA $ \capA -> withCapturedHTTP baseB $ \capB -> withCapturedHTTP baseA $ \capFixture -> withManager $ \manager -> do
      route <- newTVarIO capA
      let servers = TrafficServers route capB capFixture skew wid fixture
          action verb target body = readTVarIO route >>= \cap -> followerRequest cap verb target body
          options = defaultFollowerOptions {helperExecutable = testHelperExecutable, requestOverride = Just action}
      withTrafficSources (if mode == TrafficMixed then 4 else 3) (seedTrafficSource env wid prefix chunk) $ \sources -> withRegistry options $ \registry -> do
        started <- getMonotonicTime
        forM_ sources $ \(sid,path) -> start registry manager unused wid sid path
        forM_ sources $ \(sid,_) -> awaitFor 300 registry manager wid sid (\value -> hasStatus "caught_up" value && hasOffset (fromIntegral prefix) value)
        resumed <- getMonotonicTime
        fixtureTraffic <- case mode of
          TrafficQuiet -> do
            threadDelay 12000000
            value <- disturb servers
            threadDelay 22000000
            pure value
          _ -> do
            let live = if mode == TrafficMixed then drop 1 sources else sources
                tick :: Int -> IO ()
                tick n = when (n > 0) $ do
                  forM_ live $ \(_,path) -> BS.appendFile path record
                  threadDelay 100000
                  tick (n-1)
            Null <$ tick (if mode == TrafficMixed then 600 else 300)
        observed <- getMonotonicTime
        receipts <- forM sources $ \(sid,path) -> do
          _ <- control registry manager unused wid sid "session_upload_stop"
          final <- awaitFor 300 registry manager wid sid (\value -> hasStatus "stopped" value || hasStatus "failed" value)
          size <- withBinaryFile path ReadMode hFileSize
          pure (hasStatus "stopped" final && nested ["receipt","source_verified"] final == Just (Bool True)
            && nested ["receipt","committed_offset"] final == Just (Number (fromIntegral size)))
        stopped <- getMonotonicTime
        traffic <- mapM (fmap (fromMaybe (error "follower traffic missing")) . followerTraffic registry . fst) sources
        serverA <- readSessionTraffic (prefixTraffic primary)
        serverB <- readSessionTraffic (prefixTraffic replacement)
        length (filter id receipts) `shouldBe` length sources
        forM_ expected $ \reason -> do
          let misses :: SessionTrafficSnapshot -> Int
              misses snapshot = maybe 0 (.count) (Map.lookup ("proof_lookup","verification_unknown",reason) snapshot.requestStats)
          misses serverA + misses serverB `shouldSatisfy` (>= 1)
        let followerRequests = Map.unionsWith (+) (map (.requests) traffic)
            ms from to = (to - from) * 1000 :: Double
            snapshotValue :: SessionTrafficSnapshot -> Value
            snapshotValue snapshot = object
              [ "requests" .= [object ["route" .= r, "outcome" .= o, "reason" .= reason, "count" .= stats.count, "bytes" .= stats.bytes]
                  | ((r,o,reason),stats) <- Map.toList snapshot.requestStats]
              , "compute" .= [object ["result" .= result, "prefix" .= bucket, "proofs" .= stats.proofs, "prefix_bytes" .= stats.prefixBytes, "total_ms" .= stats.totalMs, "max_ms" .= stats.maxMs]
                  | ((result,bucket),stats) <- Map.toList snapshot.computeStats]
              , "dropped_events" .= snapshot.droppedEvents ]
        pure $ object
          [ "cohort" .= name, "followers" .= length sources, "prefix_bytes" .= prefix
          , "mode" .= (case mode of TrafficLive -> "live"; TrafficQuiet -> "quiet"; TrafficMixed -> "mixed" :: Text)
          , "resume_ms" .= ms started resumed, "observe_ms" .= ms resumed observed, "stop_ms" .= ms observed stopped
          , "receipts_verified" .= length (filter id receipts)
          , "follower" .= object
              [ "requests" .= [object ["route" .= r, "outcome" .= o, "count" .= c] | ((r,o),c) <- Map.toList followerRequests]
              , "committed_bytes" .= sum (map (.committedBytes) traffic), "local_hash_bytes" .= sum (map (.localHashBytes) traffic)
              , "proof_recoveries" .= sum (map (.proofRecoveries) traffic), "transient_retries" .= sum (map (.transientRetries) traffic) ]
          , "lifecycles" .= [object
              [ "requests" .= [object ["route" .= r, "outcome" .= o, "count" .= c] | ((r,o),c) <- Map.toList lifecycle.requests]
              , "committed_bytes" .= lifecycle.committedBytes, "local_hash_bytes" .= lifecycle.localHashBytes
              , "proof_recoveries" .= lifecycle.proofRecoveries ] | lifecycle <- traffic]
          , "server" .= object ["primary" .= snapshotValue serverA, "replacement" .= snapshotValue serverB]
          , "fixture" .= fixtureTraffic ]

withTrafficApp :: TestEnv -> PrefixRegistry -> (String -> IO a) -> IO a
withTrafficApp env registry action = do
  tracker <- newAccessTracker env.pool 3600
  websocket <- newWSState
  let cfg = Config.defaultConfig
      auth = cfg.auth {Config.mode = Config.AuthModeDeployed, Config.enabled = False}
      principal = Principal ActorBot "follower-tests" "Follower test" PrincipalSyntheticLocalSuperadmin
      authorized app req respond = withPrincipalContext (Just principal) (app req respond)
  app <- mkAppWithPrefixProofs registry Nothing cfg.changeStream authorized auth cfg.cors cfg.rateLimit env.pool tracker websocket Nothing True
  testWithApplication (pure app) $ \port -> action ("http://127.0.0.1:" <> show port)

withTrafficSources :: Int -> IO (UUID, FilePath) -> ([(UUID, FilePath)] -> IO a) -> IO a
withTrafficSources count acquire action
  | count <= 0 = action []
  | otherwise = bracket acquire (removeFile . snd) $ \source -> withTrafficSources (count-1) acquire (action . (source :))

seedTrafficSource :: TestEnv -> UUID -> Int64 -> BS.ByteString -> IO (UUID, FilePath)
seedTrafficSource env wid prefix chunk = do
  sid <- UUID.nextRandom
  _ <- Sessions.createSession env.pool (Core.CreateSession sid wid "jsonl" "Test session work" Nothing Nothing wid)
  temp <- getTemporaryDirectory
  (path,handle) <- openBinaryTempFile temp "hmem-traffic-jsonl"
  let pieces = fromIntegral (prefix `div` fromIntegral (BS.length chunk)) :: Int
      seedDatabase offset revision remaining = when (remaining > 0) $ do
        result <- Sessions.appendSession env.pool wid sid (Core.AppendSession offset revision chunk False)
        seedDatabase result.session.committedBytes result.session.revision (remaining - 1 :: Int)
  (do
    forM_ [1..pieces] (\_ -> BS.hPut handle chunk)
    hClose handle
    seedDatabase 0 0 pieces) `onException` (hClose handle >> removeFile path)
  pure (sid,path)

-- Sixteen later terminal results evict the followers' retained proofs.
evictTerminalResults :: TrafficServers -> IO Value
evictTerminalResults servers = do
  let path = "/api/v1/workspaces/" <> show servers.workspaceId <> "/sessions/" <> show servers.fixtureSession <> "/prefix-verifications"
  lookups <- forM [1..16 :: Int] $ \_ -> do
    operation <- UUID.nextRandom
    submitted <- followerRequest servers.fixtureHTTP "POST" path (Just (toJSON (Wire.PrefixProofRequest operation 0 0 (Core.sessionSha256 ""))))
    submitted `shouldSatisfy` either (const False) (const True)
    let poll count = do
          result <- followerRequest servers.fixtureHTTP "GET" (path <> "/" <> show operation) Nothing
          case result of
            Right value | field "state" value == Just (String "pending") -> threadDelay 10000 >> poll (count+1)
            Right value | field "state" value == Just (String "verified") -> pure (count+1)
            _ -> expectationFailure "fixture proof did not verify" >> pure (count+1)
    timeout 10000000 (poll (0 :: Int)) >>= maybe (expectationFailure "fixture proof deadline" >> pure 0) pure
  pure (object ["proof_submits" .= length lookups, "proof_lookups" .= sum lookups])

awaitFor :: Double -> Registry -> Manager -> UUID -> UUID -> (Value -> Bool) -> IO Value
awaitFor seconds registry manager wid sid predicate = do
  result <- timeout (round (seconds * 1000000)) loop
  case result of
    Just value -> pure value
    Nothing -> do
      current <- control registry manager "http://unused.invalid" wid sid "session_upload_status"
      expectationFailure ("Traffic cohort deadline: " <> show current)
      pure current
  where
    loop = do
      current <- control registry manager "http://unused.invalid" wid sid "session_upload_status"
      if predicate current then pure current else threadDelay 50000 >> loop

helperProbe :: Text -> BS.ByteString
helperProbe phase = BL.toStrict $ encode $ object ["url" .= ("http://127.0.0.1:0/" :: Text),"method" .= ("GET" :: Text),"headers" .= ([] :: [(Text,Text)]),"body" .= Null,"probe" .= phase]

testHelperExecutable :: IO FilePath
testHelperExecutable = do
  directory <- Paths.getBinDir
  canonicalizePath (directory </> if os == "mingw32" then "hmem-mcp.exe" else "hmem-mcp")

withCapturedHTTP :: String -> (CapturedHTTP -> IO a) -> IO a
withCapturedHTTP base action = bracket newProcessScope (\scope -> closeProcessScope scope 2000000 >>= (`shouldBe` True)) $ \scope -> do
  helper <- testHelperExecutable
  captureHTTP scope helper base Nothing identity >>= action
workspace :: UUID
workspace = read "11111111-2222-3333-4444-555555555555"
identity :: BotIdentity
identity = either (error . T.unpack) id (mkBotIdentity "developer" "codex:gpt-6.1-sol:high")

epoch :: UTCTime
epoch = UTCTime (fromGregorian 2026 1 1) 0

fakeMetadata :: UUID -> Int -> Value
fakeMetadata sid count = toJSON (Core.AgentSession sid workspace "jsonl" Nothing Nothing [] False Core.Active Nothing Nothing Nothing Nothing Nothing 0
  epoch epoch Nothing (fromIntegral count) 0 0 True Nothing Nothing Nothing Nothing Nothing Nothing)

-- Controlled HTTP fixture for tests of local scheduling/receipts. Canonical
-- boundary validation and authenticated audiences are separately exercised by
-- the actual backend tests; this still checks the submitted raw-prefix hash.
fakeProofTransport :: UUID -> BS.ByteString -> Text -> (BS.ByteString -> String -> Maybe Value -> IO (Either HTTPFault Value))
  -> IO (BS.ByteString -> String -> Maybe Value -> IO (Either HTTPFault Value))
fakeProofTransport sid canonical state fallback = do
  operations <- newTVarIO Map.empty
  pure $ \verb target body -> if not ("/prefix-verifications" `T.isInfixOf` T.pack target) then fallback verb target body else do
    proof <- if verb == "POST" then case body >>= \value -> case fromJSON value of Success request -> Just (request :: Wire.PrefixProofRequest); Error _ -> Nothing of
      Just request -> atomically (modifyTVar' operations (Map.insert request.operationId request)) >> pure (Just request)
      Nothing -> pure Nothing
      else do
        values <- readTVarIO operations
        pure $ readMaybe (reverse (takeWhile (/='/') (reverse target))) >>= (`Map.lookup` values)
    pure $ case proof of
      Nothing -> Left (HTTPStatus 404)
      Just request -> Right $ toJSON $ Wire.PrefixProofResponse workspace sid "test-original-principal" request.operationId request.expectedOffset request.expectedRevision request.sha256
        (if request.sha256 == Core.sessionSha256 (BS.take (fromIntegral request.expectedOffset) canonical) then state else "mismatch") Nothing

queryNumber :: BS.ByteString -> String -> Int
queryNumber name target = fromMaybe 0 $ do
  query <- lookup name (parseQuery (BS8.pack (dropWhile (/= '?') target))) >>= id
  readMaybe (BS8.unpack query)

field :: Text -> Value -> Maybe Value
field name (Object fields) = KM.lookup (Key.fromText name) fields
field _ _ = Nothing
nested :: [Text] -> Value -> Maybe Value
nested keys value = foldl (\previous key -> previous >>= field key) (Just value) keys
contains :: Text -> Value -> Bool
contains text = T.isInfixOf text . TE.decodeUtf8 . BL.toStrict . encode
hasStatus :: Text -> Value -> Bool
hasStatus status value = field "status" value == Just (String status)
hasOffset :: Int -> Value -> Bool
hasOffset count value = field "committed_offset" value == Just (Number (fromIntegral count))
hasReceipt :: Value -> Bool
hasReceipt value = maybe False (/= Null) (field "receipt" value)
receiptHash :: Value -> Maybe Value
receiptHash = nested ["receipt", "sha256"]
isJustValue :: Maybe a -> Bool
isJustValue (Just _) = True
isJustValue _ = False

within :: Int -> IO () -> IO ()
within budget action = timeout budget action >>= (`shouldSatisfy` isJustValue)

withManager :: (Manager -> IO a) -> IO a
withManager = bracket (newManager defaultManagerSettings {managerIdleConnectionCount = 0}) closeManager
withRegistry :: FollowerOptions -> (Registry -> IO a) -> IO a
withRegistry options = bracket (newRegistryWith options) (\registry -> closeRegistry registry >>= (`shouldBe` True))
withSourceFile :: BS.ByteString -> (FilePath -> IO a) -> IO a
withSourceFile bytes action = bracket acquire cleanup action
  where
    acquire = do
      temp <- getTemporaryDirectory
      (path,handle) <- openBinaryTempFile temp "hmem-follower-jsonl"
      BS.hPut handle bytes
      hClose handle
      pure path
    cleanup path = forM_ [path, path <> ".old"] $ \owned -> do
      exists <- doesFileExist owned
      if exists then removeFile owned else pure ()

withTempHandle :: (Handle -> IO a) -> IO a
withTempHandle action = bracket acquire cleanup (action . snd)
  where
    acquire = do directory <- getTemporaryDirectory; openBinaryTempFile directory "hmem-follower-handle"
    cleanup (path,handle) = do
      closed <- hIsClosed handle
      unless closed (hClose handle)
      removeFile path

withPipe :: ((Handle,Handle) -> IO a) -> IO a
withPipe = bracket (do pair@(readHandle,writeHandle) <- createPipe; hSetBinaryMode readHandle True; hSetBinaryMode writeHandle True; pure pair)
  (\(readHandle,writeHandle) -> forM_ [writeHandle,readHandle] $ \handle -> hIsClosed handle >>= \closed -> unless closed (hClose handle))

payload :: Value -> Value
payload result = maybe Null id (field "structuredContent" result)
start :: Registry -> Manager -> String -> UUID -> UUID -> FilePath -> IO Value
start registry manager base wid sid path = payload <$> handleUploadTool registry manager base Nothing identity "session_upload_start"
  (object ["workspace_id" .= wid, "session_id" .= sid, "path" .= path, "title" .= ("Test session work" :: Text), "format_label" .= ("jsonl" :: Text),"task_id" .= wid])
control :: Registry -> Manager -> String -> UUID -> UUID -> Text -> IO Value
control registry manager base wid sid name = payload <$> handleUploadTool registry manager base Nothing identity name
  (object ["workspace_id" .= wid,"session_id" .= sid])
await :: Registry -> Manager -> String -> UUID -> UUID -> (Value -> Bool) -> IO Value
await registry manager base wid sid predicate = do
  result <- timeout 8000000 loop
  case result of
    Just value -> pure value
    Nothing -> do
      current <- control registry manager base wid sid "session_upload_status"
      expectationFailure ("Follower deadline: " <> show current)
      pure current
  where
    loop = do
      current <- control registry manager base wid sid "session_upload_status"
      if predicate current then pure current else threadDelay 10000 >> loop
waitUntil :: IO Bool -> IO ()
waitUntil condition = do
  result <- timeout 3000000 loop
  result `shouldSatisfy` isJustValue
  where loop = do ready <- condition; unless ready (threadDelay 10000 >> loop)
rpc :: Registry -> Manager -> String -> TVar Bool -> TVar (Maybe MCPContext) -> Text -> Value -> IO (Maybe Value)
rpc registry manager base initialized context name arguments = handleStdioLineWithRegistry registry manager base Nothing initialized context (BL.toStrict (encode
  (object ["jsonrpc" .= ("2.0" :: Text), "id" .= ("follower" :: Text), "method" .= ("tools/call" :: Text), "params" .= object ["name" .= name,"arguments" .= arguments]])))

withBackend :: (TestEnv -> UUID -> Manager -> String -> IO a) -> IO a
withBackend = withBackendUsing serveBackend

withBackendUsing
  :: (TestEnv -> UUID -> (TestEnv -> UUID -> Manager -> String -> IO a) -> IO a)
  -> (TestEnv -> UUID -> Manager -> String -> IO a) -> IO a
withBackendUsing serve action = withTestEnv $ \env -> do
  wid <- (.id) <$> createTestWorkspace env "follower-tests"
  -- A fixture task shares the workspace UUID only to keep call helpers small.
  runSession env.pool $ SQL.sql ("INSERT INTO tasks(id,workspace_id,title,status) VALUES ('"<>BS8.pack (show wid)<>"','"<>BS8.pack (show wid)<>"','Follower creation task','in_progress')")
  serve env wid action

serveBackend :: TestEnv -> UUID -> (TestEnv -> UUID -> Manager -> String -> IO a) -> IO a
serveBackend = serveBackendWith auth authorized
  where
    auth = Config.defaultConfig.auth {Config.mode = Config.AuthModeDeployed, Config.enabled = False}
    principal = Principal ActorBot "follower-tests" "Follower test" PrincipalSyntheticLocalSuperadmin
    authorized app req respond = withPrincipalContext (Just principal) (app req respond)

credentialBackend :: TestEnv -> UUID -> (TestEnv -> UUID -> Manager -> String -> IO a) -> IO a
credentialBackend = serveBackendWith auth attachCredential
  where
    base = Config.defaultConfig.auth
    token = "follower-test-bot-token"
    auth = base {Config.mode = Config.AuthModeLocal, Config.enabled = False,
      Config.local = base.local {Config.botTokens = [Config.LocalBotTokenConfig "follower-tests" token]}}
    attachCredential app req = app req
      {Wai.requestHeaders = (hAuthorization, "Bearer " <> TE.encodeUtf8 token) : Wai.requestHeaders req}

serveBackendWith :: Config.AuthConfig -> Wai.Middleware -> TestEnv -> UUID -> (TestEnv -> UUID -> Manager -> String -> IO a) -> IO a
serveBackendWith auth authorized env wid action = do
  tracker <- newAccessTracker env.pool 3600
  websocket <- newWSState
  let cfg = Config.defaultConfig
  bracket (newPrefixRegistry env.pool) closePrefixRegistry $ \prefixes -> do
    app <- mkAppWithPrefixProofs prefixes Nothing cfg.changeStream authorized auth cfg.cors cfg.rateLimit env.pool tracker websocket Nothing True
    testWithApplication (pure app) $ \port -> withManager $ \manager -> action env wid manager ("http://127.0.0.1:" <> show port)
