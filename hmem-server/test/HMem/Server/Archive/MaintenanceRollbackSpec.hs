module HMem.Server.Archive.MaintenanceRollbackSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Exception
import Control.Monad (foldM,forM,forM_,unless,when)
import Crypto.Hash (SHA256,Digest,hashlazy)
import Data.Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as B8
import Data.ByteString.Lazy qualified as LBS
import Data.Int (Int64)
import Data.IORef
import Data.List (isPrefixOf,sort)
import Data.Pool (destroyAllResources)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.UUID (UUID)
import Data.UUID.V4 qualified as UUID
import GHC.Clock (getMonotonicTimeNSec)
import Hasql.Decoders qualified as D
import Hasql.Encoders qualified as E
import Hasql.Session qualified as S
import Hasql.Statement qualified as St
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Types
import Network.Socket qualified as Socket
import Network.Wai qualified as Wai
import Network.Wai.Test qualified as W
import System.Directory
import System.Environment (getEnvironment)
import System.Exit (ExitCode(..))
import System.FilePath
import System.IO (IOMode(ReadMode),hPutStrLn,stderr,withBinaryFile)
import System.IO.Error (ioeGetLocation,ioeSetLocation)
import System.Process
import System.Timeout (timeout)
import Data.Yaml qualified as Yaml
import Test.Hspec

import HMem.AgentSession qualified as Core
import HMem.AgentSession.Transport qualified as Wire
import HMem.Config qualified as Config
import HMem.DB.AgentSession qualified as DB
import HMem.DB.Pool qualified as P
import HMem.DB.SessionArchive qualified as A
import HMem.DB.SessionArchive.Maintenance qualified as M
import HMem.DB.SessionArchive.Reader qualified as R
import HMem.DB.Task qualified as Task
import HMem.DB.TestHarness
import HMem.Server.Archive.MaintenanceQualificationSpec qualified as Q
import HMem.Server.Archive.Worker (runArchiveLease,maintainArchive)
import HMem.Server.TestHarness (mkLocalSandboxConfig)
import HMem.SessionArchive.Codec qualified as C
import HMem.Types qualified as Types

spec :: Spec
spec=describe "Archive maintenance rollback" $ do
  it "closes the streaming digest source before copying a Windows fixture" $
    withTestSandbox $ \sandbox -> do
      let input=sandbox.sandboxRoot </> "digest-source.bin"
          output=sandbox.sandboxRoot </> "digest-copy.bin"
          bytes=BS.concat (replicate 64 (BS.pack [0..255]))
          expected=T.pack (show (hashlazy (LBS.fromStrict bytes)::Digest SHA256))
      BS.writeFile input bytes
      digest<-fileHash input
      -- Deliberately copy/remove before evaluating the returned digest. The old
      -- lazy read left its source handle open across this Windows copy.
      copyFile input output
      removeFile input
      doesFileExist input `shouldReturn` False
      BS.readFile output `shouldReturn` bytes
      digest `shouldBe` expected
      fileHash output `shouldReturn` expected
      removeFile output
      doesFileExist output `shouldReturn` False
  it "runs the reviewed prior backend derivative against restored archive-only data and current static assets" $ do
    root<-resolveRepoRoot
    let executable=root </> ".scratch" </> "sealed-archives-8ff1e216" </> "rollback-bin-v2" </> "hmem-server.exe"
    exists<-doesFileExist executable
    if not exists then pendingWith "Explicit release qualifier: build the reviewed tools/session-archive-rollback-compat.json candidate first"
      else rollbackProbe root executable

rollbackProbe :: FilePath -> FilePath -> IO ()
rollbackProbe root executable=do
  let recipePath=root </> "tools" </> "session-archive-rollback-compat.json"
  recipe<-LBS.readFile recipePath >>= either fail pure . eitherDecode @Value
  recipeHash<-fileHash recipePath
  artifact<-fileHash executable
  field ["build","artifact","sha256"] recipe `shouldBe` Just (String artifact)
  schemaHash<-fileHash (root </> "hmem-server" </> "migrations" </> "V051__session_archive_maintenance.sql")
  field ["deployment_contract","v051_source_sha256"] recipe `shouldBe` Just (String schemaHash)
  patchHash<-fileHash (root </> "tools" </> "session-archive-rollback-compat.patch")
  field ["patch","sha256"] recipe `shouldBe` Just (String patchHash)
  case field ["patch","files"] recipe of
    Just (Array files) -> forM_ files $ \entry -> case (field ["path"] entry,field ["after_sha256"] entry) of
      (Just (String path),Just (String digest)) -> fileHash (root </> ".scratch" </> "sealed-archives-8ff1e216" </> "rollback-b742" </> T.unpack path) `shouldReturn` digest
      _ -> expectationFailure "Malformed reviewed rollback source manifest"
    _ -> expectationFailure "Missing reviewed rollback source manifest"
  life<-newSandboxLifetime
  receipt<-newIORef Nothing
  phase<-newIORef ("fixture_setup"::String)
  ownedState<-newIORef Nothing
  rootState<-newIORef Nothing
  nativeStarts<-newIORef (0::Int)
  runId<-UUID.nextRandom
  attempt<-try @SomeException $ timeout 180000000 $ withSandboxedTestEnvBorrowed life Q.acquire Q.drain $ \admin owned -> do
    writeIORef ownedState (Just owned)
    writeIORef rootState (Just admin.testSandbox.sandboxRoot)
    writeIORef phase "prepare_archive_and_restore"
    let env=admin {pool=owned.runtimePool}
    (workspace,sealed,task)<-Q.fixture env "entropy" 2
    R.ensureArchive env.pool workspace sealed.id
    implementation<-C.codecImplementation
    Just lease<-A.claimArchive env.pool implementation
    runArchiveLease env.pool lease
    Just manifest<-A.getPublishedArchive env.pool workspace sealed.id
    M.configureMaintenance env.pool M.EnableReclamation
    maintainArchive env.pool
    number env "SELECT count(*) FROM session_chunks" `shouldReturn` 0
    M.configureMaintenance env.pool M.Pause
    M.configureMaintenance env.pool M.DisableReclamation
    number env "SELECT count(*) FROM session_archive_control WHERE paused AND NOT reclaim_enabled AND permit_generation IS NULL AND staged_bytes=0" `shouldReturn` 1
    let dump=admin.testSandbox.sandboxRoot </> "rollback-after-reclaim.dump"
    Q.pgTool owned "pg_dump" ["--format=custom","--file",dump,"--dbname",T.unpack admin.testDb.testDbConnStr]
    replacementId<-UUID.nextRandom
    let replacement="archive_rollback_"<>T.filter (/='-') (T.pack (show replacementId))
        adminConn=admin.testDb.testDbConnStr<>" dbname="<>replacement
        runtimeConn=adminConn<>" user="<>owned.runtimeRole
    mask_ $ rawExecute admin (TE.encodeUtf8 ("CREATE DATABASE "<>replacement<>" TEMPLATE template0")) >> writeIORef owned.replacement (Just replacement)
    Q.pgTool owned "pg_restore" ["--exit-on-error","--dbname",T.unpack adminConn,dump]
    removeFile dump
    restored<-mask_ $ do
      pool<-P.createPool runtimeConn 6 30 30000
      writeIORef owned.restoredPool (Just pool)
      pure pool
    let restoredEnv=env {pool=restored,testDb=env.testDb {testDbName=replacement,testDbConnStr=runtimeConn}}
    number restoredEnv "SELECT count(*) FROM session_chunks" `shouldReturn` 0
    boolean restoredEnv "SELECT public.hmem_archive_runtime_safe() AND EXISTS(SELECT 1 FROM pg_proc p JOIN pg_roles r ON r.oid=p.proowner WHERE p.oid='public.hmem_archive_reclaim(uuid,uuid,uuid,text,uuid)'::regprocedure AND r.rolname='hmem_archive_maintenance_owner' AND p.prosecdef AND p.proconfig=ARRAY['search_path=pg_catalog'] AND NOT EXISTS(SELECT 1 FROM aclexplode(p.proacl) WHERE grantee=0 AND privilege_type='EXECUTE'))" `shouldReturn` True
    writeIORef phase "capture_current_static_before_native_launch"
    (staticDir,assets)<-captureStatic root admin.testSandbox
    port<-unusedPort
    let home=admin.testSandbox.sandboxHomeDir
        original=mkLocalSandboxConfig restoredEnv
        cfg=original
          { Config.server=original.server {Config.host="127.0.0.1",Config.port=port}
          , Config.database=original.database {Config.name=replacement,Config.user=Just owned.runtimeRole}
          , Config.web=original.web {Config.webEnabled=True,Config.webStaticDir=Just staticDir}
          , Config.embeddingProvider=original.embeddingProvider {Config.mode=Config.EmbeddingProviderDisabled,Config.endpoint=Nothing,Config.gpuProfile=Nothing}
          }
        configPath=home </> ".hmem" </> "config.yaml"
    createDirectoryIfMissing True (takeDirectory configPath)
    assertInSandbox admin.testSandbox (takeDirectory configPath)
    Yaml.encodeFile configPath cfg
    configHash<-fileHash configPath
    codecs<-newIORef ([]::[Text])
    writeIORef phase "first_native_probe"
    withNative nativeStarts owned executable runtimeConn port $ \app -> do
      -- Every request really traverses the native derivative's loopback socket.
      Q.assertPaths app restoredEnv "entropy" 2 workspace sealed task manifest
      assertAssets app staticDir assets
      M.configureMaintenance restoredEnv.pool M.Resume
      forM_ [["{}","\n[]\n"],["{}","\r\n[]\r\n"]] $ \chunks -> do
        (w,t)<-newTask restoredEnv
        row<-createHttpSession app w t chunks
        archived<-awaitArchive restoredEnv w row.id
        archived.rawSha256 `shouldBe` Core.sessionSha256 (BS.concat chunks)
        archived.rawRecords `shouldBe` 2
        modifyIORef' codecs (<>[archived.codecImplementation])
      M.configureMaintenance restoredEnv.pool M.Pause
      awaitNoPermit restoredEnv
      (oldWorkspace,old,_)<-Q.fixture restoredEnv "compressible" 1
      execute restoredEnv "UPDATE session_archive_control SET cutover_at=clock_timestamp()"
      number restoredEnv ("SELECT count(*) FROM session_archive_requests WHERE session_id='"<>uuid old.id<>"'") `shouldReturn` 0
      M.configureMaintenance restoredEnv.pool (M.Capacity 8500000 34000000)
      requested<-request app methodGet (sessionPath oldWorkspace old.id<>"/archive") ""
      W.simpleStatus requested `shouldBe` status202
      M.configureMaintenance restoredEnv.pool M.Resume
      (smallWorkspace,smallTask)<-newTask restoredEnv
      small<-createHttpSession app smallWorkspace smallTask ["{}\n"]
      _<-awaitArchive restoredEnv smallWorkspace small.id
      A.getPublishedArchive restoredEnv.pool oldWorkspace old.id `shouldReturn` Nothing
      M.configureMaintenance restoredEnv.pool (M.Capacity 8589934592 34359738368)
      _<-awaitArchive restoredEnv oldWorkspace old.id
      M.configureMaintenance restoredEnv.pool M.Pause
      awaitNoPermit restoredEnv
    doesFileExist (home </> ".hmem" </> "logs" </> "hmem-server.log") `shouldReturn` True
    fileHash configPath `shouldReturn` configHash
    -- Deliberately retire a synthetic owner while the backend is stopped. The
    -- next actual entrypoint must detach and drain its current staging fence.
    M.configureMaintenance restoredEnv.pool M.Resume
    (staleWorkspace,stale,_)<-Q.fixture restoredEnv "compressible" 1
    R.ensureArchive restoredEnv.pool staleWorkspace stale.id
    Just staleLease<-A.claimArchive restoredEnv.pool implementation
    A.stageArchivePart restoredEnv.pool staleLease 0 0 "stale" `shouldReturn` True
    execute restoredEnv ("UPDATE session_archive_jobs SET lease_expires_at=clock_timestamp()+interval '20 milliseconds' WHERE session_id='"<>uuid stale.id<>"'")
    threadDelay 40000
    M.configureMaintenance restoredEnv.pool M.Pause
    writeIORef phase "native_restart_and_stale_drain_probe"
    withNative nativeStarts owned executable runtimeConn port $ \app -> do
      let drained=do
            n<-number restoredEnv "SELECT count(*) FROM session_archive_generations WHERE published_at IS NULL"
            if n==0 then pure () else threadDelay 20000 >> drained
      timeout 10000000 drained `shouldReturn` Just ()
      M.configureMaintenance restoredEnv.pool M.Resume
      fresh<-awaitArchive restoredEnv staleWorkspace stale.id
      fresh.id `shouldNotBe` staleLease.generationId
      fresh.attempt `shouldBe` 2
      A.renewArchive restoredEnv.pool staleLease `shouldReturn` False
      DB.getSession restoredEnv.pool staleWorkspace stale.id `shouldReturn` Just stale
      assertAssets app staticDir assets
      M.configureMaintenance restoredEnv.pool M.Pause
      awaitNoPermit restoredEnv
    -- Confirm the source bundle did not change during the captured probe.
    writeIORef phase "post_native_join_static_source_check"
    forM_ assets $ \(path,digest) -> fileHash (root </> "hmem-server" </> "static" </> path) `shouldReturn` digest
    helpers<-readIORef owned.helperReceipts
    actualCodecs<-readIORef codecs
    writeIORef receipt $ Just $ object
      ["format_version".=(1::Int),"owner".=("Task7 until verified Task8 handoff"::Text),"expiry".=("Scratch receipt/source/binary disposed at verified Task8 aggregate/rebind closure; durable tools patch/recipe until target replacement/retirement"::Text)
      ,"base_revision".=("b7428aa960d0b526fb85e33146f36374f3b8b109"::Text),"derivative".=("b7428aa-v051-archive-compat-2"::Text)
      ,"artifact_sha256".=artifact,"recipe_sha256".=recipeHash,"config_sha256".=configHash
      ,"native_helpers".=helpers,"native_new_archive_codecs".=actualCodecs
      ,"served_static".=[object ["path".=path,"sha256".=digest] | (path,digest)<-assets]
      ,"checks".=(["actual native entrypoint on V051 empty replacement restore","protected owner/grants restored","archive-only ranges/proofs/replay/ZIP/content over real HTTP","current candidate index and Worker served byte-identically","LF/CRLF intermediate-final new seals","dormant historical request plus fitting new seal under resource block","expired current staging detached/drained on actual backend restart","fresh generation retry and stale renewal fence","pause/drain/reclamation-disabled controls"]::[Text])
      ,"limitations".=(["backend-only rollback derivative; not pristine b742","served assets are the current functional Task6 candidate; final qualified frontend rebind/browser integration belongs to Task8","actual native server handles joined before database/supported sandbox release; deferred general supervisor guarantees are not claimed"]::[Text])]
  case attempt of
    Right outcome -> outcome `shouldBe` Just ()
    Left primary -> do
      diagnostic<-(do
        currentPhase<-readIORef phase
        starts<-readIORef nativeStarts
        known<-readIORef ownedState
        pending<-case known of
          Nothing -> pure ("borrower_not_acquired"::String)
          Just owned -> readIORef owned.helper >>= \case
            Nothing -> pure "no_registered_native_helper"
            Just handle -> ("registered_helper_status="<>) . show <$> getProcessExitCode handle
        knownRoot<-readIORef rootState
        rootPresent<-traverse doesDirectoryExist knownRoot
        cleanupFailures<-sandboxLifetimeFailures life
        helpers<-maybe (pure []) (readIORef . (.helperReceipts)) known
        pure $ take 8192 $ B8.unpack $ LBS.toStrict $ encode $ object
          ["phase".=currentPhase,"native_launch_count".=starts,"helper".=pending
          ,"owned_root".=knownRoot,"owned_root_exists".=rootPresent
          ,"lifetime_failures".=cleanupFailures,"joined_helpers".=helpers])
        `catch` \(secondary::SomeException) -> pure ("diagnostic unavailable: "<>take 1024 (displayException secondary))
      hPutStrLn stderr ("[rollback-probe] "<>diagnostic) `catch` \(_::IOException) -> pure ()
      -- Keep ResourceBusy (and the original IO error) visible to the test tool.
      case fromException primary of
        Just failure -> throwIO (ioeSetLocation failure (ioeGetLocation failure<>"; rollback-probe "<>take 4096 diagnostic))
        Nothing -> throwIO primary
  Just value<-readIORef receipt
  let path=root </> ".scratch" </> "sealed-archives-8ff1e216" </> "rollback-"<>show runId<>".json"
  LBS.writeFile path (encode value)

data NativeProbeCleanupFailure = NativeProbeCleanupFailure SomeException String deriving stock (Show)
instance Exception NativeProbeCleanupFailure

withNative :: IORef Int -> Q.Owned -> FilePath -> Text -> Int -> (Wai.Application -> IO a) -> IO a
withNative nativeStarts owned executable connection port use=mask $ \restore -> do
  environment<-getEnvironment
  let sandbox=owned.adminEnv.testSandbox
      allowed=["PATH","SYSTEMROOT","WINDIR","COMSPEC","PATHEXT","LANG","NUMBER_OF_PROCESSORS"]
      inherited=filter (\(key,_)->T.toUpper (T.pack key) `elem` allowed) environment
      private=inherited<>[("HMEM_CONFIG_DIR",sandbox.sandboxHomeDir </> ".hmem"),("HOME",sandbox.sandboxHomeDir),("USERPROFILE",sandbox.sandboxHomeDir),("APPDATA",sandbox.sandboxHomeDir </> "AppData" </> "Roaming"),("LOCALAPPDATA",sandbox.sandboxHomeDir </> "AppData" </> "Local"),("TEMP",sandbox.sandboxTmpDir),("TMP",sandbox.sandboxTmpDir)]
  start<-getMonotonicTimeNSec
  (_,_,_,handle)<-createProcess (proc executable ["--db",T.unpack connection,"--port",show port,"--pool-size","4","+RTS","-N2","-RTS"])
    {cwd=Just sandbox.sandboxRoot,env=Just private,std_in=NoStream,std_out=NoStream,std_err=NoStream,create_new_console=False}
  writeIORef owned.helper (Just handle)
  modifyIORef' nativeStarts (+1)
  result<-try @SomeException $ restore $ bracket (HTTP.newManager HTTP.defaultManagerSettings {HTTP.managerResponseTimeout=HTTP.responseTimeoutMicro 10000000}) HTTP.closeManager $ \manager -> do
    let origin="http://127.0.0.1:"<>show port
        ready=do
          exited<-getProcessExitCode handle
          unless (exited==Nothing) (fail ("Rollback entrypoint exited before readiness: "<>show exited))
          requestValue<-HTTP.parseRequest (origin<>"/api/v1/workspaces")
          response<-try @SomeException (HTTP.httpLbs requestValue manager)
          case response of
            Right value | HTTP.responseStatus value==status200 -> pure ()
            _ -> threadDelay 50000 >> ready
    available<-timeout 10000000 ready
    unless (available==Just ()) (fail "Rollback native listener did not become ready")
    use $ \incoming respond -> do
      body<-Wai.strictRequestBody incoming
      outgoing<-HTTP.parseRequest (origin<>B8.unpack (Wai.rawPathInfo incoming<>Wai.rawQueryString incoming))
      response<-HTTP.httpLbs outgoing {HTTP.method=Wai.requestMethod incoming,HTTP.requestHeaders=Wai.requestHeaders incoming,HTTP.requestBody=HTTP.RequestBodyLBS body} manager
      respond (Wai.responseLBS (HTTP.responseStatus response) (HTTP.responseHeaders response) (HTTP.responseBody response))
  exited<-getProcessExitCode handle
  when (exited==Nothing) (terminateProcess handle)
  joined<-timeout 5000000 (waitForProcess handle)
  case joined of
    Nothing -> case result of
      Left primary -> throwIO (NativeProbeCleanupFailure primary "Rollback native server did not join; actual handle and dependent sandbox retained")
      Right _ -> fail "Rollback native server did not join; actual handle and dependent sandbox retained"
    Just code -> do
      writeIORef owned.helper Nothing
      finish<-getMonotonicTimeNSec
      modifyIORef' owned.helperReceipts (<>[object ["tool".=("reviewed rollback hmem-server"::Text),"actual_helper_joined".=True,"exit_code".=show code,"elapsed_seconds".=(fromIntegral (finish-start)/1000000000::Double),"stop".=("actual handle termination after bounded probe, then confirmed join"::Text)]])
      either throwIO pure result

captureStatic :: FilePath -> TestSandbox -> IO (FilePath,[(FilePath,Text)])
captureStatic root sandbox=do
  source<-canonicalizePath (root </> "hmem-server" </> "static")
  let destination=sandbox.sandboxRoot </> "current-candidate-static"
  createDirectoryIfMissing True destination
  let collect relative=do
        names<-sort <$> listDirectory (source </> relative)
        fmap concat $ forM names $ \name -> do
          let path=relative </> name; input=source </> path; output=destination </> path
          resolved<-canonicalizePath input
          unless (addTrailingPathSeparator source `isPrefixOf` resolved) (fail "Static asset escapes source root")
          symbolic<-pathIsSymbolicLink input
          when symbolic (fail "Static symbolic link rejected")
          directory<-doesDirectoryExist input
          if directory then createDirectoryIfMissing True output >> collect path else do
            digest<-fileHash input
            copyFile input output `catch` \(failure::IOException) -> throwIO (ioeSetLocation failure
              (ioeGetLocation failure<>"; phase=static_copy; source="<>input<>"; target="<>output))
            fileHash output `shouldReturn` digest
            pure[(path,digest)]
  assets<-collect ""
  length assets `shouldSatisfy` (<=128)
  assets `shouldSatisfy` any ((=="index.html") . fst)
  assets `shouldSatisfy` any (isPrefixOf "session-archive-worker-" . takeFileName . fst)
  pure(destination,assets)

assertAssets :: Wai.Application -> FilePath -> [(FilePath,Text)] -> IO ()
assertAssets app _ assets=forM_ assets $ \(path,digest) -> do
  response<-request app methodGet ("/"<>B8.pack (map (\c -> if isPathSeparator c then '/' else c) path)) ""
  W.simpleStatus response `shouldBe` status200
  T.pack (show (hashlazy response.simpleBody::Digest SHA256)) `shouldBe` digest

unusedPort :: IO Int
unusedPort=Socket.withSocketsDo $ bracket (Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol) Socket.close $ \socket -> do
  Socket.bind socket (Socket.SockAddrInet 0 (Socket.tupleToHostAddress (127,0,0,1)))
  Socket.getSocketName socket >>= \case Socket.SockAddrInet port _ -> pure (fromIntegral port); _ -> fail "Expected loopback IPv4 socket"

newTask :: TestEnv -> IO (UUID,UUID)
newTask env=do
  workspace<-(.id) <$> createTestWorkspace env "rollback-native"
  task<-Task.createTask env.pool (Types.CreateTask workspace Nothing Nothing "Rollback native fixture" Nothing Nothing Nothing)
  execute env ("UPDATE tasks SET status='in_progress' WHERE id='"<>uuid task.id<>"'")
  pure(workspace,task.id)

createHttpSession :: Wai.Application -> UUID -> UUID -> [ByteString] -> IO Core.AgentSession
createHttpSession app workspace task chunks=do
  identifier<-UUID.nextRandom
  let root="/api/v1/workspaces/"<>uuid workspace<>"/sessions"
      target=root<>"/"<>uuid identifier
  initial<-request app methodPost root (encode (Wire.CreateSessionRequest identifier "jsonl" "Rollback native session" Nothing Nothing task)) >>= decoded @Core.AgentSession
  uploaded<-foldM (\row bytes -> (.session) <$> (request app methodPost (target<>"/append") (encode (Wire.AppendSessionRequest row.committedBytes row.revision (Wire.encodeSessionBytes bytes) True)) >>= decoded @Wire.AppendSessionResponse)) initial chunks
  request app methodPost (target<>"/terminate") (encode (Wire.TerminateSessionRequest uploaded.committedBytes uploaded.revision uploaded.committedBytes (Core.sessionSha256 (BS.concat chunks)))) >>= decoded @Core.AgentSession

awaitArchive :: TestEnv -> UUID -> UUID -> IO A.ArchiveManifest
awaitArchive env workspace session=do
  let wait=A.getPublishedArchive env.pool workspace session >>= \case Just manifest -> pure manifest; Nothing -> threadDelay 20000 >> wait
  timeout 15000000 wait >>= maybe (fail "Native derivative did not publish requested archive") pure
awaitNoPermit :: TestEnv -> IO ()
awaitNoPermit env=do
  let wait=number env "SELECT count(*) FROM session_archive_control WHERE permit_generation IS NOT NULL" >>= \n -> if n==0 then pure () else threadDelay 20000 >> wait
  timeout 5000000 wait `shouldReturn` Just ()
request :: Wai.Application -> Method -> ByteString -> LBS.ByteString -> IO W.SResponse
request app method path body=W.runSession (W.srequest (W.SRequest ((W.setPath Wai.defaultRequest path) {Wai.requestMethod=method,Wai.requestHeaders=[(hContentType,"application/json"),("X-HMem-Bot-Identity","{\"role\":\"Rollback Probe\",\"model-id\":\"test:fixture:default\"}")]}) body)) app
decoded :: FromJSON a => W.SResponse -> IO a
decoded response=do W.simpleStatus response `shouldBe` status200; either fail pure (eitherDecode response.simpleBody)
sessionPath :: UUID -> UUID -> ByteString
sessionPath w s="/api/v1/workspaces/"<>uuid w<>"/sessions/"<>uuid s
execute :: TestEnv -> ByteString -> IO ()
execute env sql=P.runSession env.pool (S.sql sql)
rawExecute :: TestEnv -> ByteString -> IO ()
rawExecute env sql=P.withConn env.pool $ \connection -> S.run (S.sql sql) connection >>= either (fail . show) pure
query :: TestEnv -> ByteString -> D.Value a -> IO a
query env sql decoder=P.runSession env.pool $ S.statement () (St.Statement sql E.noParams (D.singleRow (D.column (D.nonNullable decoder))) False)
number :: TestEnv -> ByteString -> IO Int64
number env sql=query env sql D.int8
boolean :: TestEnv -> ByteString -> IO Bool
boolean env sql=query env sql D.bool
fileHash :: FilePath -> IO Text
fileHash path=withBinaryFile path ReadMode $ \handle -> do
  bytes<-LBS.hGetContents handle
  let digest=T.pack (show (hashlazy bytes::Digest SHA256))
  _<-evaluate (T.length digest)
  pure digest
field :: [Text] -> Value -> Maybe Value
field [] value=Just value
field (key:rest) (Object values)=KM.lookup (Key.fromText key) values >>= field rest
field _ _=Nothing
uuid :: UUID -> ByteString
uuid=B8.pack . show
