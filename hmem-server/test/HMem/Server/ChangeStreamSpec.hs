{-# LANGUAGE TemplateHaskell #-}

module HMem.Server.ChangeStreamSpec (spec) where

import Control.Concurrent (Chan, MVar, getNumCapabilities, myThreadId, newChan, newEmptyMVar, putMVar, readChan, readMVar, takeMVar, threadDelay, throwTo, tryPutMVar, tryTakeMVar, writeChan)
import Control.Concurrent.Async (Async, AsyncCancelled(..), async, asyncOn, asyncWithUnmask, asyncThreadId, cancel, wait, waitCatch, withAsync)
import Control.Exception (AsyncException(..), IOException, MaskingState(..), SomeAsyncException, SomeException, bracket, catch, finally, fromException, getMaskingState, mask, throwIO, toException, try)
import Control.Monad (forM, forM_, replicateM_, replicateM, forever, void, when)
import Data.Aeson (FromJSON, Value(..), decode, encode, object, toJSON, (.=))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.FileEmbed (embedFile, makeRelativeToProject)
import Data.Int (Int64)
import Data.Maybe (isJust)
import Data.List (nub, sort)
import Data.Map.Strict qualified as Map
import Data.Pool (Pool, destroyAllResources)
import Hasql.Connection qualified as Hasql
import Data.IORef (atomicModifyIORef', newIORef, readIORef, writeIORef)
import GHC.Clock (getMonotonicTimeNSec)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as Text
import Data.Time (getCurrentTime, addUTCTime)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUIDV4
import Hasql.Decoders qualified as Dec
import Hasql.Encoders qualified as Enc
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement
import Network.HTTP.Types (Header, methodGet, methodPost, parseQuery, status200)
import Network.HTTP.Types qualified as HTTP
import Network.Socket qualified as Socket
import Network.Socket.ByteString qualified as SocketBytes
import Network.TLS qualified as TLS
import Network.TLS.Extra.Cipher (ciphersuite_default)
import Network.Wai (Application, defaultRequest)
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp (defaultSettings, defaultShouldDisplayException, openFreePort)
import Network.Wai.Handler.Warp.Internal (Settings(..), socketConnection)
import Network.Wai.Handler.WarpTLS (runTLSSocket, tlsSettingsMemory)
import Network.Wai.Internal (Response(..))
import Network.Wai.Test qualified as WaiTest
import Network.WebSockets qualified as WS
import Network.WebSockets.Stream qualified as WSStream
import System.Timeout (timeout)
import System.IO (hPutStrLn, stderr)
import System.Environment (lookupEnv)
import System.Directory (getFileSize)
import System.Log.FastLogger (fromLogStr)
import Test.Hspec

import HMem.Config qualified as Config
import HMem.BotIdentity (BotIdentity, mkBotIdentity, localDeclaredBotId)
import HMem.DB.Auth qualified as Auth
import HMem.AgentSession qualified as Agent
import HMem.AgentSession.Transport qualified as AgentWire
import HMem.DB.Task qualified as TaskDB
import HMem.DB.AgentSession qualified as AgentDB
import HMem.DB.ChangeStream
import HMem.DB.Pool (runSession)
import HMem.DB.Pool qualified as DBPool
import HMem.DB.Project qualified as Project
import HMem.DB.Workspace qualified as WorkspaceDB
import HMem.DB.RequestContext (ActorType(..), Principal(..), PrincipalAuthority(..), withRequestContext, emptyRequestContext)
import HMem.DB.TestHarness (TestDb(..), TestEnv(..), createTestWorkspace)
import HMem.DB.WorkspaceGroup qualified as WorkspaceGroup
import HMem.Server.TestHarness (DeployedSandboxApp(..), LocalSandboxApp(..), createDeployedSandboxUser, issueDeployedSandboxPAT, mkDeployedSandboxConfig, withDeployedSandboxAppContext, withDeployedSandboxAppLogged, withLocalSandboxAppContext)
import HMem.Server.Logging (jsonRequestLogger)
import HMem.Server.AuthTokens (IssuedAccessToken(..))
import HMem.Server.ChangeStream (ChangeStreamWorker, ChangeStreamMaintenance(..), startChangeStreamWorker, startChangeStreamWorkerWithStartupHook, stopChangeStreamWorker, readChangeStreamMaintenance, retentionScopeTurn, retentionDue, ChangeStreamDispatchEvent(..), startChangeStreamWorkerWithObserver)
import HMem.Server.Snapshot (materializeSnapshot, materializeSnapshotWithProfile)
import HMem.Server.WebSocket (WSState, newWSState, withNativeWebSocketSettings, awaitWebSocketDrain, connectionCount, createCanonicalTicket, createCanonicalTicketWithExpiry, createCanonicalTicketWithTtl, createActivityTicketWithTtl, dispatchCanonicalOutbox, canonicalReplayPending, handleCanonicalDispatchFailure, reauthorizeCanonicalConnections, attachWebSocketTelemetryLog, readWebSocketTelemetry)
import HMem.Server.WebSocketTelemetry (DeliveryGauges(..), OutcomeStats(..), WebSocketTelemetrySnapshot(..))
import HMem.Server.WebSocketDelivery (DeliveryCleanupFailure(..), DeliveryFailure(..))
import HMem.Server.WebSocketTransport (newNativeTransport, runPlainSocket, runPlainSocketWith, withNativeTransportSettings)
import HMem.Types (ChangeStreamResyncResponse(..), ChangeStreamSnapshotItem(..), CreateProject(..), CreateTask(..), CreateWorkspaceGroup(..), Project(..), Task(..), SnapshotProfile(..), UpdateWorkspace(..), WebSocketTicketResponse(..), Workspace(..), WorkspaceGroup(..))

spec :: Spec
spec = describe "canonical change-stream loopback" $ do
  it "rotates retention across scope identities and uses elapsed time despite wall-clock-independent tick counts" $ do
    ids <- sort <$> replicateM 5 UUIDV4.nextRandom
    let scopes = map WorkspaceScope ids <> [GlobalScope]
        first = retentionScopeTurn Nothing scopes
        second = retentionScopeTurn (Just (last first)) scopes
    first `shouldBe` take 4 scopes
    second `shouldBe` drop 4 scopes <> take 2 scopes
    retentionDue 0 Nothing `shouldBe` True
    retentionDue 59999999999 (Just 0) `shouldBe` False
    retentionDue 60000000000 (Just 0) `shouldBe` True
    retentionDue 1 (Just 2) `shouldBe` False

  it "automatically prunes one-day backlog on startup and publishes a truthful maintenance result" $
    withLocalSandboxAppContext $ \ctx -> do
      workspace <- createTestWorkspace ctx.localEnv "retention-worker-startup"
      void $ Project.createProject ctx.localEnv.pool (CreateProject workspace.id Nothing "expired" Nothing Nothing Nothing)
      runSession ctx.localEnv.pool $ Session.sql ("UPDATE change_stream_outbox SET occurred_at=clock_timestamp()-interval '2 days' WHERE scope='workspace' AND workspace_id='" <> Text.encodeUtf8 (UUID.toText workspace.id) <> "'")
      bracket (startChangeStreamWorker ctx.localEnv.pool Config.defaultConfig.changeStream ctx.localWSState)
        stopChangeStreamWorker $ \worker -> do
          let await = do
                status <- readChangeStreamMaintenance worker
                if status.lastSuccessAt /= Nothing && status.backlog == Just (OutboxBacklog 0 Nothing)
                  then pure status else threadDelay 10000 >> await
          status <- timeout 10000000 await >>= maybe (fail "startup retention did not finish") pure
          status.lastError `shouldBe` Nothing
          status.failureCount `shouldBe` 0
          listOutboxAfter ctx.localEnv.pool (WorkspaceScope workspace.id) 0 10 `shouldReturn` []

  it "advances retention past a contended scope and reports failure before a successful retry" $
    withLocalSandboxAppContext $ \ctx -> do
      workspaces <- forM [1..6 :: Int] $ \index -> do
        workspace <- createTestWorkspace ctx.localEnv ("retention-fair-" <> T.pack (show index))
        void $ Project.createProject ctx.localEnv.pool (CreateProject workspace.id Nothing "expired" Nothing Nothing Nothing)
        runSession ctx.localEnv.pool $ Session.sql ("UPDATE change_stream_outbox SET occurred_at=clock_timestamp()-interval '2 days' WHERE scope='workspace' AND workspace_id='" <> Text.encodeUtf8 (UUID.toText workspace.id) <> "'")
        pure workspace.id
      let identities = sort workspaces
          blocked = head identities
          late = last identities
      entered <- newEmptyMVar; release <- newEmptyMVar
      let hold holderPool = DBPool.withConn holderPool $ \connection ->
            (do
              locked <- Session.run (Session.sql ("BEGIN; SELECT hmem_change_stream_lock('workspace','" <> Text.encodeUtf8 (UUID.toText blocked) <> "')")) connection
              either (fail . show) pure locked
              putMVar entered ()
              takeMVar release)
            `finally` void (Session.run (Session.sql "ROLLBACK") connection)
      bracket (DBPool.createPool ctx.localEnv.testDb.testDbConnStr 1 5.0 30000) destroyAllResources $ \holderPool ->
       withAsync (hold holderPool) $ \holder -> do
        takeMVar entered
        bracket (startChangeStreamWorker ctx.localEnv.pool Config.defaultConfig.changeStream ctx.localWSState)
          stopChangeStreamWorker $ \worker -> do
            let awaitLate = do
                  status <- readChangeStreamMaintenance worker
                  records <- listOutboxAfter ctx.localEnv.pool (WorkspaceScope late) 0 10
                  if null records && status.failureCount > 0 then pure status else threadDelay 10000 >> awaitLate
            status <- timeout 10000000 awaitLate >>= maybe (fail "contended scope starved a later scope") pure
            status.failureCount `shouldSatisfy` (> 0)
            -- The held scope still has its original record: a failed turn did
            -- not advance its floor, while later scopes made progress.
            length <$> listOutboxAfter ctx.localEnv.pool (WorkspaceScope blocked) 0 10 `shouldReturn` 1
            putMVar release (); wait holder
            let awaitRetry = do
                  value <- readChangeStreamMaintenance worker
                  records <- listOutboxAfter ctx.localEnv.pool (WorkspaceScope blocked) 0 10
                  if null records && value.lastError == Nothing then pure () else threadDelay 10000 >> awaitRetry
            timeout 10000000 awaitRetry `shouldReturn` Just ()

  it "rotates bounded dispatcher batches past a skewed scope without an idle sleep" $
    withLocalSandboxAppContext $ \ctx -> do
      workspaces <- sort <$> forM [1..7 :: Int] (\index -> (.id) <$> createTestWorkspace ctx.localEnv ("dispatch-fair-" <> T.pack (show index)))
      let pool = ctx.localEnv.pool
          hot = head workspaces
      seedReplayRows ctx.localEnv hot "dispatch-hot" 2300
      forM_ (tail workspaces) $ \workspace ->
        void $ Project.createProject pool (CreateProject workspace Nothing "later scope" Nothing Nothing Nothing)
      reads <- newIORef []
      commits <- newIORef (0 :: Int)
      sleeps <- newIORef (0 :: Int)
      second <- newEmptyMVar
      hold <- newEmptyMVar
      let observe = \case
            DispatchBatchRead rows -> atomicModifyIORef' reads (\old -> (take 2 (old <> [rows]), ()))
            DispatchBatchCommitted _ -> do
              count <- atomicModifyIORef' commits (\old -> (old + 1, old + 1))
              when (count == 2) $ putMVar second () >> takeMVar hold
            DispatchIdle -> atomicModifyIORef' sleeps (\old -> (old + 1, ()))
            _ -> pure ()
      bracket (startChangeStreamWorkerWithObserver observe pool testChangeStreamConfig ctx.localWSState)
        stopChangeStreamWorker $ \_ -> do
          declaredPhase "two fair dispatcher turns" (takeMVar second)
          turns <- readIORef reads
          length turns `shouldBe` 2
          forM_ turns $ \rows -> do
            length rows `shouldSatisfy` (<= 4)
            forM_ rows $ \(_, _, count) -> count `shouldSatisfy` (<= 1000)
          let first = head turns
              later = last turns
          [count | (WorkspaceScope workspace, _, count) <- first, workspace == hot] `shouldBe` [1000]
          sort (concatMap (map (\(scope, _, _) -> scope)) turns)
            `shouldBe` sort (map WorkspaceScope workspaces <> [GlobalScope])
          map (\(scope, _, _) -> scope) later `shouldSatisfy` elem (WorkspaceScope (last workspaces))
          readIORef sleeps `shouldReturn` 0
          qualificationMetric "skewed-scopes" (object ["scopes" .= (length workspaces + 1), "turn_records" .= map (map (\(_, _, count) -> count)) turns, "idle_sleeps" .= (0 :: Int)])

  it "retains nonzero process watermarks and backs off a failed dispatcher batch" $
    withLocalSandboxAppContext $ \ctx -> do
      workspace <- createTestWorkspace ctx.localEnv "dispatch-failure-ack"
      void $ Project.createProject ctx.localEnv.pool (CreateProject workspace.id Nothing "retry me" Nothing Nothing Nothing)
      reads <- newIORef []
      commits <- newIORef (0 :: Int)
      deferrals <- newIORef (0 :: Int)
      retried <- newEmptyMVar
      hold <- newEmptyMVar
      let observe = \case
            DispatchBatchRead rows -> do
              now <- getMonotonicTimeNSec
              prior <- atomicModifyIORef' reads (\old -> (old <> [(rows, now)], old))
              case length prior of
                0 -> pure ()
                1 -> ioError (userError "injected failure after fetch, before managed dispatch")
                _ -> putMVar retried () >> takeMVar hold
            DispatchBatchCommitted _ -> do
              count <- atomicModifyIORef' commits (\old -> (old + 1, old + 1))
              when (count == 1) $ void $ Project.createProject ctx.localEnv.pool
                (CreateProject workspace.id Nothing "after nonzero acknowledgement" Nothing Nothing Nothing)
            DispatchDeferred -> atomicModifyIORef' deferrals (\old -> (old + 1, ()))
            _ -> pure ()
      bracket (startChangeStreamWorkerWithObserver observe ctx.localEnv.pool testChangeStreamConfig ctx.localWSState)
        stopChangeStreamWorker $ \_ -> do
          declaredPhase "failed batch retry" (takeMVar retried)
          observed <- readIORef reads
          case observed of
            [(initial, _), (failed, began), (retriedRows, retriedAt)] -> do
              forM_ initial $ \(_, previous, _) -> previous `shouldBe` 0
              retriedRows `shouldBe` failed
              retriedAt - began `shouldSatisfy` (>= 1000000000)
              forM_ retriedRows $ \(_, previous, _) -> previous `shouldSatisfy` (> 0)
            _ -> expectationFailure "unexpected number of bounded retry reads"
          readIORef commits `shouldReturn` 1
          readIORef deferrals `shouldReturn` 1

  it "reads only scopes whose committed counter passed the acknowledgement and retains it on failure" $
    withLocalSandboxAppContext $ \ctx -> do
      workspaces <- sort <$> forM [1..6 :: Int] (\index -> do
        workspace <- createTestWorkspace ctx.localEnv ("dispatch-skip-" <> T.pack (show index))
        void $ Project.createProject ctx.localEnv.pool (CreateProject workspace.id Nothing "retained" Nothing Nothing Nothing)
        pure workspace.id)
      (writtenId, prunedId, failingId) <- case workspaces of
        first:second:third:_ -> pure (first, second, third)
        _ -> fail "expected six workspaces"
      let pool = ctx.localEnv.pool
          st = ctx.localWSState
          owned = map WorkspaceScope workspaces
          written = WorkspaceScope writtenId
          pruned = WorkspaceScope prunedId
          failing = WorkspaceScope failingId
          principal = Principal { actorType = ActorUser, actorId = "skip-delivery", actorLabel = "skip-delivery", authority = PrincipalSyntheticLocalSuperadmin }
      cutoff <- addUTCTime 60 <$> getCurrentTime
      void $ pruneOutboxScopeBefore pool pruned cutoff 1000
      listOutboxAfter pool pruned 0 10 `shouldReturn` []
      resume <- terminalLocalResume ctx written (TrustedAudience "local:skip-delivery")
      ticket <- createCanonicalTicket st principal written resume
      events <- newIORef []
      failArmed <- newIORef False
      let observe event = do
            atomicModifyIORef' events (\old -> (old <> [event], ()))
            case event of
              DispatchBatchRead rows | any (\(scope, _, count) -> scope == failing && count > 0) rows -> do
                armed <- atomicModifyIORef' failArmed (\value -> (False, value))
                when armed $ ioError (userError "injected failure after fetch, before managed dispatch")
              _ -> pure ()
          rotations scope entries = length [() | DispatchBatchRead rows <- entries, any (\(identity, _, _) -> identity == scope) rows]
          -- Wait until every owned scope has been rotated twice more after a
          -- point, so the window covers complete turns of all of them.
          awaitRotations phase from extra = timeout 30000000 poll >>= maybe (fail ("dispatcher did not reach " <> phase)) pure
            where
              poll = do
                entries <- readIORef events
                let later = drop from entries
                if extra entries && all (\scope -> rotations scope later >= 2) owned
                  then pure (length entries) else threadDelay 10000 >> poll
          slice from to = take (to - from) . drop from <$> readIORef events
          ownedScans entries = filter (`elem` owned) (concat [scopes | DispatchScopesScanned scopes <- entries])
          readsOf scope entries = [(previous, count) | DispatchBatchRead rows <- entries, (identity, previous, count) <- rows, identity == scope, count > 0]
          idleCount = length . filter (== DispatchIdle)
      withWebSocketApplication st ctx.localApplication $ \port ->
        withActivityFrames port ticket.ticket $ \frames -> do
          frameType <$> declaredPhase "skip initial checkpoint" (readChan frames >>= decodeFrame) `shouldReturn` Just "checkpoint"
          bracket (startChangeStreamWorkerWithObserver observe pool testChangeStreamConfig st)
            stopChangeStreamWorker $ \_ -> do
              -- After the first idle sleep, wait for two more complete
              -- rotations and at least two further idle sleeps.
              settled <- awaitRotations "idle rotations" 0 (\entries -> idleCount entries >= 3
                && all (\scope -> rotations scope (dropWhile (/= DispatchIdle) entries) >= 3) owned)
              initial <- slice 0 settled
              -- Each retained scope, including the fully pruned one, is read
              -- once; later idle rotations read none of them.
              sort (ownedScans initial) `shouldBe` sort owned
              ownedScans (dropWhile (/= DispatchIdle) initial) `shouldBe` []
              readsOf pruned initial `shouldBe` []
              project <- Project.createProject pool (CreateProject writtenId Nothing "after idle sweeps" Nothing Nothing Nothing)
              awaitChangeChan frames project.id
              delivered <- readIORef events >>= \entries -> awaitRotations "rotations after a write" (length entries) (const True)
              afterWrite <- slice settled delivered
              ownedScans afterWrite `shouldBe` [written]
              map snd (readsOf written afterWrite) `shouldBe` [1]
              atomicModifyIORef' failArmed (const (True, ()))
              void $ Project.createProject pool (CreateProject failingId Nothing "retry after failure" Nothing Nothing Nothing)
              retriedAt <- awaitRotations "rotations after a failed turn" delivered (\entries ->
                case readsOf failing (drop delivered entries) of
                  (failedFrom, _):_:_ -> or [through > failedFrom | DispatchBatchCommitted rows <- drop delivered entries, (identity, through) <- rows, identity == failing]
                  _ -> False)
              retried <- slice delivered retriedAt
              readIORef failArmed `shouldReturn` False
              length [() | DispatchDeferred <- retried] `shouldSatisfy` (>= 1)
              ownedScans retried `shouldBe` [failing, failing]
              case readsOf failing retried of
                [(failedFrom, 1), (retriedFrom, 1)] -> do
                  retriedFrom `shouldBe` failedFrom
                  let committed = [through | DispatchBatchCommitted rows <- retried, (identity, through) <- rows, identity == failing]
                  committed `shouldSatisfy` (not . null)
                  last committed `shouldSatisfy` (> failedFrom)
                _ -> expectationFailure "expected one failed and one retried read of the written scope"

  it "keeps retention running while successful dispatcher batches remain busy" $
    withLocalSandboxAppContext $ \ctx -> do
      workspace <- createTestWorkspace ctx.localEnv "dispatch-retention-backlog"
      let pool = ctx.localEnv.pool
      seedReplayRows ctx.localEnv workspace.id "expired-dispatch" 1501
      runSession pool $ Session.sql ("UPDATE change_stream_outbox SET occurred_at=clock_timestamp()-interval '2 days' WHERE scope='workspace' AND workspace_id='" <> Text.encodeUtf8 (UUID.toText workspace.id) <> "'")
      seedReplayRows ctx.localEnv workspace.id "live-dispatch" 3000
      cutoff <- addUTCTime (-86400) <$> getCurrentTime
      backlogs <- newIORef []
      sleeps <- newIORef (0 :: Int)
      maintained <- newEmptyMVar
      hold <- newEmptyMVar
      let observe = \case
            DispatchBatchRead rows -> do
              remaining <- outboxBacklog pool cutoff
              atomicModifyIORef' backlogs (\old -> (old <> [remaining.expiredRowsLowerBound], ()))
              when (remaining.expiredRowsLowerBound == 0 && any (\(_, _, count) -> count > 0) rows) $
                putMVar maintained () >> takeMVar hold
            DispatchIdle -> atomicModifyIORef' sleeps (\old -> (old + 1, ()))
            _ -> pure ()
      bracket (startChangeStreamWorkerWithObserver observe pool Config.defaultConfig.changeStream ctx.localWSState)
        stopChangeStreamWorker $ \_ -> do
          declaredPhase "retention completion during busy dispatch" (takeMVar maintained)
          remaining <- readIORef backlogs
          remaining `shouldBe` [501, 0]
          readIORef sleeps `shouldReturn` 0

  it "reauthorizes idle clients once per elapsed tick across empty scopes and observes revocation" $
    withDeployedSandboxAppContext $ \ctx -> do
      workspaces <- forM [1..7 :: Int] $ \index -> do
        workspace <- createTestWorkspace ctx.deployedEnv ("dispatch-idle-" <> T.pack (show index))
        void $ Project.createProject ctx.deployedEnv.pool (CreateProject workspace.id Nothing "create counter" Nothing Nothing Nothing)
        pure workspace.id
      user <- createDeployedSandboxUser ctx.deployedEnv False False
      let pool = ctx.deployedEnv.pool
          st = ctx.deployedWSState
          workspace = head workspaces
          scope = WorkspaceScope workspace
          principal = grantPrincipal user
          audience = AuthenticatedAudience (UUID.toText user) user
      _ <- Auth.upsertWorkspaceMembership pool workspace (Auth.UpsertWorkspaceMembership user Auth.WorkspaceRoleRead) Nothing
      cutoff <- addUTCTime 60 <$> getCurrentTime
      forM_ (map WorkspaceScope workspaces <> [GlobalScope]) $ \identity ->
        void $ pruneOutboxScopeBefore pool identity cutoff 1000
      resume <- terminalResume ctx scope audience
      ticket <- createCanonicalTicket st principal scope resume
      visited <- newIORef []
      checks <- newIORef []
      idle <- newEmptyMVar
      release <- newEmptyMVar
      let observe = \case
            DispatchBatchRead rows -> atomicModifyIORef' visited (\old -> (nub (old <> map (\(identity, _, _) -> identity) rows), ()))
            DispatchIdleAuthorization now -> atomicModifyIORef' checks (\old -> (take 16 (old <> [now]), ()))
            DispatchIdle -> do
              first <- tryPutMVar idle ()
              when first (takeMVar release)
            _ -> pure ()
      frames <- newChan
      sqlBefore <- qualificationSqlStats pool
      withWebSocketApplication st ctx.deployedApplication $ \port ->
        withAsync ((runPlainPeer port ticket.ticket $ \conn ->
          forever (WS.receiveData conn >>= writeChan frames))
          `catch` \(_ :: WS.ConnectionException) -> pure ()) $ \peer -> do
          frameType <$> declaredPhase "idle initial checkpoint" (readChan frames >>= decodeFrame) `shouldReturn` Just "checkpoint"
          bracket (startChangeStreamWorkerWithObserver observe pool testChangeStreamConfig st)
            stopChangeStreamWorker $ \_ -> do
              declaredPhase "complete empty dispatcher sweep" (readMVar idle)
              sort <$> readIORef visited `shouldReturn` sort (map WorkspaceScope workspaces <> [GlobalScope])
              beforeRevocation <- readIORef checks
              beforeRevocation `shouldSatisfy` (not . null)
              forM_ (zip beforeRevocation (drop 1 beforeRevocation)) $ \(previous, current) ->
                current - previous `shouldSatisfy` (>= 1000000000)
              Auth.deleteWorkspaceMembership pool workspace user `shouldReturn` True
              putMVar release ()
              frameType <$> declaredPhase "idle authorization revocation" (readChan frames >>= decodeFrame)
                `shouldReturn` Just "access_revoked"
              afterRevocation <- readIORef checks
              length afterRevocation `shouldSatisfy` (> length beforeRevocation)
              forM_ (zip afterRevocation (drop 1 afterRevocation)) $ \(previous, current) ->
                current - previous `shouldSatisfy` (>= 1000000000)
              -- This peer receives and acknowledges terminal Close before we
              -- cancel its producer, rather than writing another Close after
              -- the server's native retirement has already closed the socket.
              declaredPhase "idle revoked peer acknowledged Close and joined" (wait peer)
              sqlAfter <- qualificationSqlStats pool
              qualificationMetric "idle-authorization" (object ["scopes" .= (length workspaces + 1), "check_count" .= length afterRevocation, "check_intervals_ms" .= zipWith (\a b -> fromIntegral (b-a) / 1000000 :: Double) afterRevocation (drop 1 afterRevocation), "sql_before" .= sqlBefore, "sql_after" .= sqlAfter])

  it "measures causal pool queueing and snapshot scope-lock contention" $
    withDeployedSandboxAppContext $ \ctx -> do
      workspace <- createTestWorkspace ctx.deployedEnv "qualification-snapshot"
      seedReplayRows ctx.deployedEnv workspace.id "qualification-row" 300
      let scope = WorkspaceScope workspace.id
          audience = TrustedAudience "qualification-snapshot"
          newPool = DBPool.createPool ctx.deployedEnv.testDb.testDbConnStr 1 5.0 30000
          lock = Session.sql ("SELECT hmem_change_stream_lock('workspace','" <> Text.encodeUtf8 (UUID.toText workspace.id) <> "')")
      bracket newPool destroyAllResources $ \pool -> do
        held <- newEmptyMVar
        release <- newEmptyMVar
        waiting <- newEmptyMVar
        entered <- newEmptyMVar
        withAsync (DBPool.withConn pool (\_ -> putMVar held () >> readMVar release)) $ \holder ->
          (do
            declaredPhase "qualification held pool lease" (takeMVar held)
            withAsync (do
              start <- getMonotonicTimeNSec
              putMVar waiting ()
              DBPool.withConn pool (\_ -> getMonotonicTimeNSec >>= putMVar entered)
              pure start) $ \waiter -> do
                declaredPhase "qualification waiter started" (takeMVar waiting)
                threadDelay 100000
                tryTakeMVar entered `shouldReturn` Nothing
                putMVar release ()
                start <- wait waiter
                acquired <- takeMVar entered
                qualificationMetric "pool-queue" (object ["pool_size" .= (1 :: Int), "before_withConn_to_callback_ms" .= milliseconds start acquired, "held_lease_minimum_ms" .= (100 :: Int)])
            wait holder)
          `finally` void (tryPutMVar release ())
      bracket newPool destroyAllResources $ \holderPool ->
        bracket newPool destroyAllResources $ \snapshotPool -> do
          DBPool.withConn snapshotPool $ \connection -> Session.run (Session.sql "SET application_name='change-stream-qualification-snapshot'") connection >>= either (fail . show) pure
          held <- newEmptyMVar
          release <- newEmptyMVar
          let holder = DBPool.withConn holderPool $ \connection ->
                (Session.run (Session.sql "BEGIN" *> lock) connection >>= either (fail . show) pure >> putMVar held () >> readMVar release)
                `finally` void (Session.run (Session.sql "ROLLBACK") connection)
          sqlBefore <- qualificationSqlStats ctx.deployedEnv.pool
          withAsync holder $ \holding ->
            (do
              declaredPhase "qualification held scope lock" (takeMVar held)
              started <- getMonotonicTimeNSec
              withAsync (beginResync snapshotPool 60 scope audience (materializeSnapshot scope)) $ \snapshot -> do
                let awaitWait = do
                      blocked <- runSession ctx.deployedEnv.pool $ Session.statement () qualificationLockWait
                      if blocked then pure () else threadDelay 10000 >> awaitWait
                declaredPhase "qualification observed snapshot lock waiter" awaitWait
                observed <- getMonotonicTimeNSec
                putMVar release ()
                wait holding
                begun <- wait snapshot >>= either (fail . show) pure
                finished <- getMonotonicTimeNSec
                page <- readSnapshotPageWithTtl snapshotPool 60 scope audience begun.snapshotToken 500 >>= either (fail . show) pure
                length page.snapshotPageItems `shouldSatisfy` (>= 301)
                sqlAfter <- qualificationSqlStats ctx.deployedEnv.pool
                qualificationMetric "snapshot-lock" (object ["items" .= length page.snapshotPageItems, "start_to_observed_lock_wait_ms" .= milliseconds started observed, "release_to_complete_upper_bound_ms" .= milliseconds observed finished, "sql_before" .= sqlBefore, "sql_after" .= sqlAfter]))
            `finally` void (tryPutMVar release ())

  it "continues pending replay on an acknowledged empty dispatcher turn" $
    withDeployedSandboxAppContext $ \ctx -> do
      workspace <- createTestWorkspace ctx.deployedEnv "dispatch-empty-replay"
      owner <- UUIDV4.nextRandom
      let pool = ctx.deployedEnv.pool
          st = ctx.deployedWSState
          scope = WorkspaceScope workspace.id
          principal = (grantPrincipal owner) { actorId = "empty-turn-replay", authority = PrincipalSyntheticLocalSuperadmin }
          audience = TrustedAudience "local:empty-turn-replay"
      original <- terminalResume ctx scope audience
      seedCanonicalReplay ctx.deployedEnv workspace.id
      idle <- newEmptyMVar
      release <- newEmptyMVar
      emptyRead <- newIORef False
      let observe = \case
            DispatchBatchRead rows -> when (all (\(_, _, count) -> count == 0) rows) $
              atomicModifyIORef' emptyRead (const (True, ()))
            DispatchIdle -> do
              first <- tryPutMVar idle ()
              when first (takeMVar release)
            _ -> pure ()
      bracket (startChangeStreamWorkerWithObserver observe pool testChangeStreamConfig st)
        stopChangeStreamWorker $ \_ -> do
          declaredPhase "outbox acknowledged before subscription" (readMVar idle)
          atomicModifyIORef' emptyRead (const (False, ()))
          ticket <- createCanonicalTicket st principal scope original
          withWebSocketApplication st ctx.deployedApplication $ \port ->
            withActivityFrames port ticket.ticket $ \frames -> do
              declaredPhase "first bounded replay page" $ replicateM_ 500 $
                (frameType <$> (readChan frames >>= decodeFrame)) `shouldReturn` Just "change"
              canonicalReplayPending st `shouldReturn` True
              validateCanonicalResumeToken pool scope audience original >>= (\result -> result `shouldSatisfy` either (const False) (const True))
              putMVar release ()
              declaredPhase "empty-turn replay remainder" $ replicateM_ 25 $
                (frameType <$> (readChan frames >>= decodeFrame)) `shouldReturn` Just "change"
              frameType <$> declaredPhase "empty-turn replay checkpoint" (readChan frames >>= decodeFrame) `shouldReturn` Just "checkpoint"
              readIORef emptyRead `shouldReturn` True
              let settled = canonicalReplayPending st >>= \pending -> when pending (threadDelay 1000 >> settled)
              declaredPhase "empty-turn durable acknowledgement" settled

  it "paces retained-prefix maintenance without sleeping on pending delivery" $
    withLocalSandboxAppContext $ \ctx -> do
      workspace <- createTestWorkspace ctx.localEnv "dispatch-retained-prefix"
      let pool = ctx.localEnv.pool
          scope = WorkspaceScope workspace.id
      void $ Project.createProject pool (CreateProject workspace.id Nothing "recent prefix" Nothing Nothing Nothing)
      void $ Project.createProject pool (CreateProject workspace.id Nothing "old behind prefix" Nothing Nothing Nothing)
      old <- listOutboxAfter pool scope 0 10 >>= maybe (fail "missing old record") pure . safeLast
      runSession pool $ Session.sql ("UPDATE change_stream_outbox SET occurred_at=clock_timestamp()-interval '2 days' WHERE scope='workspace' AND workspace_id='" <> Text.encodeUtf8 (UUID.toText workspace.id) <> "' AND cursor=" <> Text.encodeUtf8 (T.pack (show old.outboxCursor)))
      seedReplayRows ctx.localEnv workspace.id "busy-retained-prefix" 2300
      attempts <- newIORef (0 :: Int)
      busyTurns <- newIORef (0 :: Int)
      idleTurns <- newIORef []
      observed <- newEmptyMVar
      hold <- newEmptyMVar
      let observe = \case
            DispatchMaintenanceAttempt -> atomicModifyIORef' attempts (\count -> (count + 1, ()))
            DispatchBatchRead rows -> when (any (\(_, _, count) -> count > 0) rows) $
              atomicModifyIORef' busyTurns (\count -> (count + 1, ()))
            DispatchIdle -> do
              now <- getMonotonicTimeNSec
              count <- readIORef attempts
              prior <- atomicModifyIORef' idleTurns (\oldTurns -> (oldTurns <> [(count, now)], oldTurns))
              when (not (null prior)) (putMVar observed () >> takeMVar hold)
            _ -> pure ()
      bracket (startChangeStreamWorkerWithObserver observe pool Config.defaultConfig.changeStream ctx.localWSState)
        stopChangeStreamWorker $ \worker -> do
          declaredPhase "paced no-progress retention" (takeMVar observed)
          readIORef busyTurns >>= (`shouldSatisfy` (>= 3))
          readIORef idleTurns >>= \case
            [(first, began), (second, ended)] -> do
              first `shouldBe` 1
              second `shouldBe` 2
              ended - began `shouldSatisfy` (>= 1000000000)
            _ -> expectationFailure "unexpected idle maintenance sequence"
          status <- readChangeStreamMaintenance worker
          status.lastDeletedRows `shouldBe` 0
          fmap (.expiredRowsLowerBound) status.backlog `shouldBe` Just 1
          length <$> listOutboxAfter pool scope 0 1000 `shouldReturn` 1000

  it "lets a healthy later population progress while the first batch producer is blocked" $
    withLocalSandboxAppContext $ \ctx -> do
      slowScope <- createTestWorkspace ctx.localEnv "dispatch-slow-scope"
      healthyScope <- createTestWorkspace ctx.localEnv "dispatch-healthy-scope"
      let pool = ctx.localEnv.pool
          st = ctx.localWSState
          principal = Principal { actorType = ActorUser, actorId = "healthy-batch", actorLabel = "healthy-batch", authority = PrincipalSyntheticLocalSuperadmin }
      resume <- terminalLocalResume ctx (WorkspaceScope healthyScope.id) (TrustedAudience "local:healthy-batch")
      ticket <- createCanonicalTicket st principal (WorkspaceScope healthyScope.id) resume
      armed <- newIORef False
      writing <- newEmptyMVar
      release <- newEmptyMVar
      ended <- newEmptyMVar
      committed <- newEmptyMVar
      hold <- newEmptyMVar
      slowReady <- newEmptyMVar
      let write send bytes = do
            block <- atomicModifyIORef' armed (\enabled -> (False, enabled))
            if not block then send bytes else
              (putMVar writing () >> takeMVar release >> send bytes) `finally` putMVar ended ()
          application request respond
            | lookup "ticket" (Wai.queryString request) == Nothing = observeRawWebSocket ctx.localApplication write (pure ()) request respond
            | otherwise = ctx.localApplication request respond
          observe = \case
            DispatchBatchCommitted rows -> putMVar committed rows >> takeMVar hold
            _ -> pure ()
      withWebSocketApplication st application $ \port ->
        withAsync ((WS.runClient "127.0.0.1" port "/api/v1/ws" $ \conn ->
          putMVar slowReady () >> forever (void (WS.receiveData conn :: IO Text))) `catch` \(_ :: WS.ConnectionException) -> pure ()) $ \_ -> do
          declaredPhase "slow first legacy accepted" (takeMVar slowReady)
          let registered = connectionCount st >>= \count -> when (count /= 1) (threadDelay 1000 >> registered)
          declaredPhase "slow first legacy registered" registered
          withActivityFrames port ticket.ticket $ \frames -> do
            frameType <$> declaredPhase "healthy initial checkpoint" (readChan frames >>= decodeFrame) `shouldReturn` Just "checkpoint"
            void $ Project.createProject pool (CreateProject slowScope.id Nothing "slow population change" Nothing Nothing Nothing)
            healthy <- Project.createProject pool (CreateProject healthyScope.id Nothing "healthy population change" Nothing Nothing Nothing)
            atomicModifyIORef' armed (const (True, ()))
            bracket (startChangeStreamWorkerWithObserver observe pool testChangeStreamConfig st)
              stopChangeStreamWorker $ \_ ->
                (do
                  declaredPhase "first producer write blocked" (takeMVar writing)
                  awaitChangeChan frames healthy.id
                  tryTakeMVar ended `shouldReturn` Nothing
                  tryTakeMVar committed `shouldReturn` Nothing
                  putMVar release ()
                  rows <- declaredPhase "all batch producers completed" (takeMVar committed)
                  map fst rows `shouldSatisfy` elem (WorkspaceScope slowScope.id)
                  map fst rows `shouldSatisfy` elem (WorkspaceScope healthyScope.id)
                  declaredPhase "blocked producer actually ended" (takeMVar ended))
                `finally` void (tryPutMVar release ())

  it "joins cancelled batch producers before retaining all process acknowledgements" $
    withDeployedSandboxAppContext $ \ctx -> do
      workspace <- createTestWorkspace ctx.deployedEnv "dispatch-cancel-producers"
      let pool = ctx.deployedEnv.pool
          st = ctx.deployedWSState
          scope = WorkspaceScope workspace.id
          principal name = Principal { actorType = ActorUser, actorId = name, actorLabel = name, authority = PrincipalSyntheticLocalSuperadmin }
      first <- canonicalTicketFor ctx (principal "cancel-first") scope (TrustedAudience "local:cancel-first")
      second <- canonicalTicketFor ctx (principal "cancel-second") scope (TrustedAudience "local:cancel-second")
      armed <- newIORef False
      writing <- replicateM 2 newEmptyMVar
      ended <- replicateM 2 newEmptyMVar
      ownersEnded <- replicateM 2 newEmptyMVar
      hold <- newEmptyMVar
      commits <- newIORef (0 :: Int)
      readBatch <- newEmptyMVar
      let write index send bytes = do
            block <- readIORef armed
            if not block then send bytes else
              (putMVar (writing !! index) () >> readMVar hold) `finally` putMVar (ended !! index) ()
          application request respond = case lookup "ticket" (Wai.queryString request) of
            Just (Just value) | value == Text.encodeUtf8 first.ticket -> observeRawWebSocket ctx.deployedApplication (write 0) (putMVar (ownersEnded !! 0) ()) request respond
            Just (Just value) | value == Text.encodeUtf8 second.ticket -> observeRawWebSocket ctx.deployedApplication (write 1) (putMVar (ownersEnded !! 1) ()) request respond
            _ -> ctx.deployedApplication request respond
          observe = \case
            DispatchBatchRead rows -> void (tryPutMVar readBatch rows)
            DispatchBatchCommitted _ -> atomicModifyIORef' commits (\count -> (count + 1, ()))
            _ -> pure ()
      withWebSocketApplication st application $ \port ->
        withActivityFrames port first.ticket $ \firstFrames ->
          withActivityFrames port second.ticket $ \secondFrames -> do
            forM_ [firstFrames, secondFrames] $ \frames ->
              (frameType <$> declaredPhase "cancel peer initial checkpoint" (readChan frames >>= decodeFrame)) `shouldReturn` Just "checkpoint"
            void $ Project.createProject pool (CreateProject workspace.id Nothing "cancelled managed batch" Nothing Nothing Nothing)
            atomicModifyIORef' armed (const (True, ()))
            bracket (startChangeStreamWorkerWithObserver observe pool testChangeStreamConfig st)
              stopChangeStreamWorker $ \worker -> do
                forM_ writing (declaredPhase "concurrent producer write entered" . takeMVar)
                rows <- declaredPhase "unacknowledged batch read" (takeMVar readBatch)
                forM_ rows $ \(_, previous, _) -> previous `shouldBe` 0
                declaredPhase "cancelled dispatcher and producers joined" (stopChangeStreamWorker worker)
                forM_ ended (declaredPhase "cancelled managed child ended" . takeMVar)
                forM_ ownersEnded (declaredPhase "cancelled native owner ended" . takeMVar)
                readIORef commits `shouldReturn` 0

  it "bounds activity ticket retention globally and prunes expired capabilities" $ do
    state <- newWSState
    first <- grantPrincipal <$> UUIDV4.nextRandom
    others <- replicateM 15 (grantPrincipal <$> UUIDV4.nextRandom)
    overflow <- grantPrincipal <$> UUIDV4.nextRandom
    forM_ (first:others) $ \principal ->
      replicateM_ 8 (createActivityTicketWithTtl state 60 principal >>= (`shouldSatisfy` isJust))
    createActivityTicketWithTtl state 60 overflow >>= (`shouldSatisfy` (not . isJust))
    expiring <- newWSState
    replicateM_ 8 (createActivityTicketWithTtl expiring 0 first >>= (`shouldSatisfy` isJust))
    createActivityTicketWithTtl expiring 60 first >>= (`shouldSatisfy` isJust)

  it "serves bounded activity-only Home sockets without granting canonical Global access" $
    withDeployedSandboxAppContext $ \ctx -> do
      workspace <- createTestWorkspace ctx.deployedEnv "home-activity"
      forbidden <- createTestWorkspace ctx.deployedEnv "home-forbidden"
      user <- createDeployedSandboxUser ctx.deployedEnv False False
      _ <- Auth.upsertWorkspaceMembership ctx.deployedEnv.pool workspace.id (Auth.UpsertWorkspaceMembership user Auth.WorkspaceRoleRead) Nothing
      token <- issueDeployedSandboxPAT ctx.deployedEnv user "Home activity"
      now <- getCurrentTime
      _ <- Auth.createAuthSession ctx.deployedEnv.pool user "home-cookie" "home-csrf" (addUTCTime 300 now)
      let endpoint="/api/v1/session-activity/ticket"
          headers=authHeaders token.rawToken
          issue=apiRequest ctx.deployedApplication methodPost endpoint headers ""
          cookies=[("Cookie","hmem_session=home-cookie; hmem_csrf=home-csrf")]
          readFrame frames=declaredPhase "activity-only frame" (readChan frames >>= decodeFrame)
          ready=object ["type" .= ("session_activity_ready" :: Text),"schema_version" .= (1::Int)]
          hint=object ["type" .= ("session_activity_changed" :: Text),"schema_version" .= (1::Int),"workspace_id" .= workspace.id]
          scope=WorkspaceScope workspace.id
          cursor=maybe 0 (.outboxCursor) . safeLast <$> listOutboxAfter ctx.deployedEnv.pool scope 0 1000
          dispatchAfter before=listOutboxAfter ctx.deployedEnv.pool scope before 1000 >>= dispatchCanonicalOutbox ctx.deployedEnv.pool ctx.deployedWSState
      responseStatus <$> apiRequest ctx.deployedApplication methodPost endpoint [] "" `shouldReturn` HTTP.status401
      responseStatus <$> apiRequest ctx.deployedApplication methodPost endpoint cookies "" `shouldReturn` HTTP.status403
      responseStatus <$> apiRequest ctx.deployedApplication methodPost "/api/v1/change-stream/ticket" headers (encode (object ["scope" .= object ["scope" .= ("global" :: Text)],"resume_token" .= ("not-global" :: Text)])) `shouldReturn` HTTP.status403
      task <- TaskDB.createTask ctx.deployedEnv.pool (CreateTask workspace.id Nothing Nothing "Home running task" Nothing Nothing Nothing)
      later <- TaskDB.createTask ctx.deployedEnv.pool (CreateTask workspace.id Nothing Nothing "Home later task" Nothing Nothing Nothing)
      runSession ctx.deployedEnv.pool $ Session.sql ("UPDATE tasks SET status='in_progress' WHERE workspace_id='"<>Text.encodeUtf8 (UUID.toText workspace.id)<>"'")
      issuedResponse <- apiRequest ctx.deployedApplication methodPost endpoint (cookies<>[("X-CSRF-Token","home-csrf")]) ""
      responseStatus issuedResponse `shouldBe` status200
      issued <- decodedTicket issuedResponse
      withWebSocketApplication ctx.deployedWSState ctx.deployedApplication $ \port -> do
        withActivityFrames port issued.ticket $ \frames -> do
          readFrame frames `shouldReturn` ready
          identifier <- UUIDV4.nextRandom
          before <- cursor
          row <- AgentDB.createSession ctx.deployedEnv.pool (Agent.CreateSession identifier workspace.id "jsonl" "Home activity stream" Nothing Nothing task.id)
          dispatchAfter before
          readFrame frames `shouldReturn` hint
          beforeLink <- cursor
          AgentDB.associateSessionTask ctx.deployedEnv.pool workspace.id identifier later.id
          dispatchAfter beforeLink
          readFrame frames `shouldReturn` hint
          beforeSeal <- cursor
          _ <- AgentDB.terminateSession ctx.deployedEnv.pool workspace.id identifier (Agent.TerminateSession 0 row.revision 0 (Agent.sessionSha256 ""))
          dispatchAfter beforeSeal
          readFrame frames `shouldReturn` hint
          -- A hidden foreign mutation must not precede the subscriber's next
          -- own-membership control, proving absence without a timing assertion.
          _ <- Project.createProject ctx.deployedEnv.pool (CreateProject forbidden.id Nothing "Hidden activity" Nothing Nothing Nothing)
          hidden <- latestOutbox ctx (WorkspaceScope forbidden.id)
          dispatchCanonicalOutbox ctx.deployedEnv.pool ctx.deployedWSState [hidden]
          Auth.deleteWorkspaceMembership ctx.deployedEnv.pool workspace.id user `shouldReturn` True
          revoked <- latestOutbox ctx scope
          dispatchCanonicalOutbox ctx.deployedEnv.pool ctx.deployedWSState [revoked]
          readFrame frames `shouldReturn` object ["type" .= ("access_revoked" :: Text),"schema_version" .= (1::Int),"workspace_id" .= workspace.id]
          runSession ctx.deployedEnv.pool $ Session.sql ("UPDATE users SET disabled_at=clock_timestamp() WHERE id='"<>Text.encodeUtf8 (UUID.toText user)<>"'")
          reauthorizeCanonicalConnections ctx.deployedEnv.pool ctx.deployedWSState
          readFrame frames `shouldReturn` object ["type" .= ("access_revoked" :: Text),"schema_version" .= (1::Int)]
        -- Use a fresh enabled user for independent capacity and expiry checks.
        boundedUser <- createDeployedSandboxUser ctx.deployedEnv False False
        _ <- Auth.upsertWorkspaceMembership ctx.deployedEnv.pool workspace.id (Auth.UpsertWorkspaceMembership boundedUser Auth.WorkspaceRoleRead) Nothing
        boundedToken <- issueDeployedSandboxPAT ctx.deployedEnv boundedUser "Bounded Home"
        let boundedIssue=apiRequest ctx.deployedApplication methodPost endpoint (authHeaders boundedToken.rawToken) ""
        pending <- replicateM 8 (boundedIssue >>= decodedTicket)
        overflow <- boundedIssue
        responseStatus overflow `shouldBe` HTTP.status503
        lookup "Retry-After" (WaiTest.simpleHeaders overflow) `shouldBe` Just "1"
        forM_ pending $ \entry -> WS.runClient "127.0.0.1" port ("/api/v1/ws?ticket="<>showTicket entry.ticket) $ \conn -> do
          (WS.receiveData conn >>= decodeFrame) `shouldReturn` ready
          WS.sendClose conn ("capacity ticket consumed" :: Text)
          _ <- try @WS.ConnectionException (WS.receiveData conn :: IO Text)
          pure ()
        reused <- try @SomeException $ WS.runClient "127.0.0.1" port ("/api/v1/ws?ticket="<>showTicket issued.ticket) $ \conn -> WS.sendClose conn ("unexpected reuse" :: Text)
        reused `shouldSatisfy` either (const True) (const False)
        Just short <- createActivityTicketWithTtl ctx.deployedWSState 0.2 (grantPrincipal boundedUser)
        peerReady <- newEmptyMVar; releasePeer <- newEmptyMVar
        peerExpired <- newEmptyMVar; serverClose <- newEmptyMVar
        peer <- asyncWithUnmask $ \unmask -> unmask $
          WS.runClient "127.0.0.1" port ("/api/v1/ws?ticket="<>showTicket short.ticket) $ \conn -> do
            WS.receiveData conn >>= putMVar peerReady
            -- Do not acknowledge or even read expiry until capacity is checked.
            takeMVar releasePeer
            WS.receiveData conn >>= putMVar peerExpired
            message <- WS.receive conn
            case message of
              WS.ControlMessage (WS.Close _ _) -> putMVar serverClose True
              _ -> fail "expiry did not send a server-initiated close frame"
            WS.sendClose conn ("acknowledge server expiry" :: Text)
            ended <- try @WS.ConnectionException (WS.receive conn)
            ended `shouldSatisfy` either (const True) (const False)
        withDeclaredClient (pure peer) $ \_ -> do
          awaitFrame peerReady `shouldReturn` ready
          threadDelay 300000
          let live 0=do
                denied <- boundedIssue
                responseStatus denied `shouldBe` HTTP.status503
                lookup "Retry-After" (WaiTest.simpleHeaders denied) `shouldBe` Just "1"
                -- The expired, unacknowledged physical peer still owns one of
                -- eight slots. A later real mutation must not enqueue a hint.
                marker <- Project.createProject ctx.deployedEnv.pool (CreateProject workspace.id Nothing "After activity expiry" Nothing Nothing Nothing)
                record <- latestOutbox ctx scope
                dispatchCanonicalOutbox ctx.deployedEnv.pool ctx.deployedWSState [record]
                marker.id `seq` putMVar releasePeer ()
                awaitFrame peerExpired `shouldReturn` object ["type" .= ("session_activity_expired" :: Text),"schema_version" .= (1::Int)]
                declaredPhase "server activity Close before peer acknowledgement" (takeMVar serverClose) `shouldReturn` True
                timeout 2000000 (wait peer) `shouldReturn` Just ()
              live count=do
                entry <- boundedIssue >>= decodedTicket
                withActivityFrames port entry.ticket $ \frames -> do
                  readFrame frames `shouldReturn` ready
                  live (count-1)
          live (7::Int)

  it "pushes Global workspace delete and restore only to readable activity observers" $
    withDeployedSandboxAppContext $ \ctx -> do
      workspace <- createTestWorkspace ctx.deployedEnv "activity-workspace-lifecycle"
      other <- createTestWorkspace ctx.deployedEnv "activity-observer-marker"
      member <- createDeployedSandboxUser ctx.deployedEnv False False
      outsider <- createDeployedSandboxUser ctx.deployedEnv False False
      _ <- Auth.upsertWorkspaceMembership ctx.deployedEnv.pool workspace.id (Auth.UpsertWorkspaceMembership member Auth.WorkspaceRoleRead) Nothing
      memberToken <- issueDeployedSandboxPAT ctx.deployedEnv member "Readable activity"
      outsideToken <- issueDeployedSandboxPAT ctx.deployedEnv outsider "Hidden activity"
      task <- TaskDB.createTask ctx.deployedEnv.pool (CreateTask workspace.id Nothing Nothing "Workspace agent" Nothing Nothing Nothing)
      runSession ctx.deployedEnv.pool $ Session.sql ("UPDATE tasks SET status='in_progress' WHERE id='"<>Text.encodeUtf8 (UUID.toText task.id)<>"'")
      identifier <- UUIDV4.nextRandom
      _ <- AgentDB.createSession ctx.deployedEnv.pool (Agent.CreateSession identifier workspace.id "jsonl" "Workspace lifecycle agent" Nothing Nothing task.id)
      let issue token=apiRequest ctx.deployedApplication methodPost "/api/v1/session-activity/ticket" (authHeaders token) "" >>= decodedTicket
          receive frames=declaredPhase "workspace lifecycle control" (readChan frames >>= decodeFrame)
          ready=object ["type" .= ("session_activity_ready" :: Text),"schema_version" .= (1::Int)]
          hint=object ["type" .= ("session_activity_changed" :: Text),"schema_version" .= (1::Int),"workspace_id" .= workspace.id]
          projection=apiRequest ctx.deployedApplication methodGet ("/api/v1/session-activity?workspace_id="<>Text.encodeUtf8 (UUID.toText workspace.id)) (authHeaders memberToken.rawToken) "" >>= decodeResponse @Value
      permitted <- issue memberToken.rawToken
      denied <- issue outsideToken.rawToken
      withWebSocketApplication ctx.deployedWSState ctx.deployedApplication $ \port ->
        withActivityFrames port permitted.ticket $ \memberFrames -> withActivityFrames port denied.ticket $ \outsideFrames -> do
          receive memberFrames `shouldReturn` ready
          receive outsideFrames `shouldReturn` ready
          runSession ctx.deployedEnv.pool $ Session.sql ("UPDATE workspaces SET deleted_at=clock_timestamp() WHERE id='"<>Text.encodeUtf8 (UUID.toText workspace.id)<>"'")
          deleted <- latestOutbox ctx GlobalScope
          jsonField "workspace_id" deleted.outboxEnvelope `shouldBe` Just Null
          dispatchCanonicalOutbox ctx.deployedEnv.pool ctx.deployedWSState [deleted]
          receive memberFrames `shouldReturn` hint
          projection >>= (\value -> jsonField "workspaces" value `shouldBe` Just (toJSON ([]::[Value])))
          runSession ctx.deployedEnv.pool $ Session.sql ("UPDATE workspaces SET deleted_at=NULL WHERE id='"<>Text.encodeUtf8 (UUID.toText workspace.id)<>"'")
          restored <- latestOutbox ctx GlobalScope
          dispatchCanonicalOutbox ctx.deployedEnv.pool ctx.deployedWSState [restored]
          receive memberFrames `shouldReturn` hint
          projection >>= (\value -> jsonField "workspaces" value `shouldBe` Just (toJSON [object ["id" .= workspace.id,"active" .= True]]))
          _ <- Auth.upsertWorkspaceMembership ctx.deployedEnv.pool other.id (Auth.UpsertWorkspaceMembership outsider Auth.WorkspaceRoleRead) Nothing
          marker <- latestOutbox ctx (WorkspaceScope other.id)
          dispatchCanonicalOutbox ctx.deployedEnv.pool ctx.deployedWSState [marker]
          receive outsideFrames `shouldReturn` object ["type" .= ("access_granted" :: Text),"schema_version" .= (1::Int),"workspace_id" .= other.id]

  it "pushes authorized unselected activity membership hints without leaking or waking on transcript output" $
    withDeployedSandboxAppContext $ \ctx -> do
      current <- createTestWorkspace ctx.deployedEnv "activity-current"
      target <- createTestWorkspace ctx.deployedEnv "activity-unselected"
      user <- createDeployedSandboxUser ctx.deployedEnv False False
      outsider <- createDeployedSandboxUser ctx.deployedEnv False False
      forM_ [(current.id,user),(current.id,outsider),(target.id,user)] $ \(workspace,member) -> do
        _ <- Auth.upsertWorkspaceMembership ctx.deployedEnv.pool workspace (Auth.UpsertWorkspaceMembership member Auth.WorkspaceRoleRead) Nothing
        pure ()
      task <- TaskDB.createTask ctx.deployedEnv.pool (CreateTask target.id Nothing Nothing "Running unselected agent" Nothing Nothing Nothing)
      later <- TaskDB.createTask ctx.deployedEnv.pool (CreateTask target.id Nothing Nothing "Later unselected link" Nothing Nothing Nothing)
      runSession ctx.deployedEnv.pool $ Session.sql ("UPDATE tasks SET status='in_progress' WHERE id IN ('"<>Text.encodeUtf8 (UUID.toText task.id)<>"','"<>Text.encodeUtf8 (UUID.toText later.id)<>"')")
      let scope=WorkspaceScope current.id
          targetScope=WorkspaceScope target.id
          ticket member=canonicalTicketFor ctx (grantPrincipal member) scope (AuthenticatedAudience (UUID.toText member) member)
          cursor=maybe 0 (.outboxCursor) . safeLast <$> listOutboxAfter ctx.deployedEnv.pool targetScope 0 1000
          dispatchAfter before=listOutboxAfter ctx.deployedEnv.pool targetScope before 1000 >>= dispatchCanonicalOutbox ctx.deployedEnv.pool ctx.deployedWSState
          assertHint frame=frame `shouldBe` object ["type" .= ("session_activity_changed" :: Text),"schema_version" .= (1::Int),"workspace_id" .= target.id]
      deniedTicket <- ticket outsider
      withWebSocketApplication ctx.deployedWSState ctx.deployedApplication $ \port ->
        bracket (startOneFrameClient port deniedTicket.ticket) (\client -> joinClient "denied activity client" client.clientWorker) $ \denied -> do
          frameType <$> awaitFrame denied.clientInitial `shouldReturn` Just "checkpoint"
          let hinted :: IO a -> IO a
              hinted action=do
                issued <- ticket user
                bracket (startOneFrameClient port issued.ticket) (\client -> joinClient "activity hint client" client.clientWorker) $ \client -> do
                  frameType <$> awaitFrame client.clientInitial `shouldReturn` Just "checkpoint"
                  before <- cursor
                  result <- action
                  dispatchAfter before
                  awaitFrame client.clientObserved >>= assertHint
                  wait client.clientWorker
                  pure result
          identifier <- UUIDV4.nextRandom
          created <- hinted (AgentDB.createSession ctx.deployedEnv.pool (Agent.CreateSession identifier target.id "jsonl" "Unselected running agent" Nothing Nothing task.id))
          -- A second socket must see its own canonical marker next, despite
          -- genuine append/heartbeat/runtime events in the readable other scope.
          quietTicket <- ticket user
          bracket (startOneFrameClient port quietTicket.ticket) (\client -> joinClient "ordinary output client" client.clientWorker) $ \quiet -> do
            frameType <$> awaitFrame quiet.clientInitial `shouldReturn` Just "checkpoint"
            before <- cursor
            let body="{\"message\":\"private output\"}\n"
            _ <- AgentDB.appendSession ctx.deployedEnv.pool target.id created.id (Agent.AppendSession 0 0 body False)
            _ <- AgentDB.heartbeatSession ctx.deployedEnv.pool target.id created.id
            _ <- AgentDB.setRuntimeIdentity ctx.deployedEnv.pool target.id created.id (Agent.SetRuntimeIdentity 0 (Just (Agent.RuntimeIdentity "Actual runtime" Nothing Nothing (Just "developer") Nothing)))
            dispatchAfter before
            marker <- Project.createProject ctx.deployedEnv.pool (CreateProject current.id Nothing "Selected marker" Nothing Nothing Nothing)
            record <- latestOutbox ctx scope
            dispatchCanonicalOutbox ctx.deployedEnv.pool ctx.deployedWSState [record]
            awaitFrame quiet.clientObserved >>= assertChangeIdentity record marker.id
            awaitFrame denied.clientObserved >>= assertChangeIdentity record marker.id
            wait quiet.clientWorker
            wait denied.clientWorker
          _ <- hinted (AgentDB.associateSessionTask ctx.deployedEnv.pool target.id identifier later.id)
          project <- Project.createProject ctx.deployedEnv.pool (CreateProject target.id Nothing "Activity parent" Nothing Nothing Nothing)
          _ <- hinted (runSession ctx.deployedEnv.pool $ Session.sql ("UPDATE tasks SET project_id='"<>Text.encodeUtf8 (UUID.toText project.id)<>"' WHERE id='"<>Text.encodeUtf8 (UUID.toText task.id)<>"'"))
          Just final <- AgentDB.getSession ctx.deployedEnv.pool target.id identifier
          _ <- hinted (AgentDB.terminateSession ctx.deployedEnv.pool target.id identifier (Agent.TerminateSession final.committedBytes final.revision final.committedBytes (Agent.sessionSha256 "{\"message\":\"private output\"}\n")))
          pure ()

  it "delivers session metadata through replay and live HTTP without evidence bytes, duplicate effects or cross-workspace leakage" $
    withDeployedSandboxAppContext $ \ctx -> do
      workspace <- createTestWorkspace ctx.deployedEnv "session-stream"
      initial <- TaskDB.createTask ctx.deployedEnv.pool (CreateTask workspace.id Nothing Nothing "initial session task" Nothing Nothing Nothing)
      runSession ctx.deployedEnv.pool $ Session.sql ("UPDATE tasks SET status='in_progress' WHERE id='"<>Text.encodeUtf8 (UUID.toText initial.id)<>"'")
      other <- createTestWorkspace ctx.deployedEnv "session-stream-other"
      user <- createDeployedSandboxUser ctx.deployedEnv False False
      outsider <- createDeployedSandboxUser ctx.deployedEnv False False
      forM_ [workspace.id,other.id] $ \id -> do
        _ <- Auth.upsertWorkspaceMembership ctx.deployedEnv.pool id (Auth.UpsertWorkspaceMembership user Auth.WorkspaceRoleEdit) Nothing
        pure ()
      _ <- Auth.upsertWorkspaceMembership ctx.deployedEnv.pool other.id (Auth.UpsertWorkspaceMembership outsider Auth.WorkspaceRoleRead) Nothing
      token <- issueDeployedSandboxPAT ctx.deployedEnv user "session writer"
      outsideToken <- issueDeployedSandboxPAT ctx.deployedEnv outsider "other scope"
      identifier <- UUIDV4.nextRandom
      let scope = WorkspaceScope workspace.id
          audience = AuthenticatedAudience (UUID.toText user) user
          headers = authHeaders token.rawToken
          root = "/api/v1/workspaces/" <> Text.encodeUtf8 (UUID.toText workspace.id) <> "/sessions"
          path = root <> "/" <> Text.encodeUtf8 (UUID.toText identifier)
          sentinel = "SESSION_WEBSOCKET_PRIVATE_SENTINEL"
          bytes = Text.encodeUtf8 ("{\"message\":\"" <> sentinel <> " æ雪\"}\r\n")
          post target value = apiRequest ctx.deployedApplication methodPost target headers (encode value)
          sessionRecords = filter (\record -> (jsonField "entity" record.outboxEnvelope >>= jsonField "type") `elem` [Just (String "agent_session"),Just (String "task_session")])
          noEvidence value = T.isInfixOf sentinel (Text.decodeUtf8 (LBS.toStrict (encode value))) `shouldBe` False
      resume <- terminalResume ctx scope audience
      issued <- createCanonicalTicket ctx.deployedWSState (grantPrincipal user) scope resume
      otherTicket <- canonicalTicketFor ctx (grantPrincipal outsider) (WorkspaceScope other.id) (AuthenticatedAudience (UUID.toText outsider) outsider)
      withWebSocketApplication ctx.deployedWSState ctx.deployedApplication $ \port ->
        bracket (startOneFrameClient port otherTicket.ticket) (\client -> joinClient "session other-scope client" client.clientWorker) $ \otherClient -> do
          frameType <$> awaitFrame otherClient.clientInitial `shouldReturn` Just "checkpoint"
          response <- post root (toJSON (AgentWire.CreateSessionRequest identifier "jsonl" "Test session work" Nothing Nothing initial.id))
          responseStatus response `shouldBe` status200
          created <- decodeResponse response :: IO Agent.AgentSession
          responseStatus <$> post root (toJSON (AgentWire.CreateSessionRequest identifier "jsonl" "Test session work" Nothing Nothing initial.id)) `shouldReturn` status200
          uploadedResponse <- post (path<>"/append") (toJSON (AgentWire.AppendSessionRequest 0 0 (AgentWire.encodeSessionBytes bytes) False))
          responseStatus uploadedResponse `shouldBe` status200
          uploaded <- decodeResponse uploadedResponse :: IO AgentWire.AppendSessionResponse
          responseStatus <$> post (path<>"/append") (toJSON (AgentWire.AppendSessionRequest 0 0 (AgentWire.encodeSessionBytes bytes) False)) `shouldReturn` status200
          before <- sessionRecords <$> listOutboxAfter ctx.deployedEnv.pool scope 0 100
          length before `shouldBe` 3
          forM_ [FullV1,WorkspaceShellV1] $ \profile -> do
            items <- runSession ctx.deployedEnv.pool (materializeSnapshotWithProfile profile scope)
            map (jsonField "kind") items `shouldSatisfy` all (`notElem` [Just (String "agent_session"),Just (String "task_session"),Just (String "session_chunks")])
            noEvidence (toJSON items)
          frames <- newChan
          let receive = declaredPhase "session frame" (readChan frames >>= decodeFrame)
              client count ticket = asyncWithUnmask $ \unmask -> unmask $
                (WS.runClient "127.0.0.1" port ("/api/v1/ws?ticket=" <> showTicket ticket) $ \conn -> do
                  replicateM_ count (WS.receiveData conn >>= writeChan frames)
                  WS.sendClose conn ("session scenario complete" :: Text)) `catch` \(_ :: WS.ConnectionException) -> pure ()
          beat <- withDeclaredClient (client 5 issued.ticket) $ \_ -> do
            first <- receive; initialLink <- receive; second <- receive; checkpoint <- receive
            map frameType [first,initialLink,second,checkpoint] `shouldBe` [Just "change",Just "change",Just "change",Just "checkpoint"]
            mapM_ noEvidence [first,initialLink,second,checkpoint]
            map (\frame -> jsonField "event" frame >>= jsonField "entity" >>= jsonField "type") [first,initialLink,second] `shouldBe` [Just (String "agent_session"),Just (String "task_session"),Just (String "agent_session")]
            responseBeat <- apiRequest ctx.deployedApplication methodPost (path<>"/heartbeat") headers ""
            responseStatus responseBeat `shouldBe` status200
            canonical <- decodeResponse responseBeat :: IO Agent.AgentSession
            records <- listOutboxAfter ctx.deployedEnv.pool scope 0 100
            dispatchCanonicalOutbox ctx.deployedEnv.pool ctx.deployedWSState records
            live <- receive
            frameType live `shouldBe` Just "change"
            noEvidence live
            LBS.length (encode live) `shouldSatisfy` (< 2048)
            pure canonical
          task <- TaskDB.createTask ctx.deployedEnv.pool (CreateTask workspace.id Nothing Nothing "late association" Nothing Nothing Nothing)
          runSession ctx.deployedEnv.pool $ Session.sql ("UPDATE tasks SET status='in_progress' WHERE id='"<>Text.encodeUtf8 (UUID.toText task.id)<>"'")
          reconnect <- terminalResume ctx scope audience
          let receipt = AgentWire.TerminateSessionRequest uploaded.session.committedBytes beat.revision uploaded.session.committedBytes (Agent.sessionSha256 bytes)
          sealedResponse <- post (path<>"/terminate") (toJSON receipt)
          responseStatus sealedResponse `shouldBe` status200
          sealed <- decodeResponse sealedResponse :: IO Agent.AgentSession
          sealed.state `shouldBe` Agent.Terminated
          sealed.lastSeenAt `shouldBe` beat.lastSeenAt
          sealed.creatorActorId `shouldBe` created.creatorActorId
          responseStatus <$> post (path<>"/terminate") (toJSON receipt) `shouldReturn` status200
          let linkPath = path<>"/tasks/"<>Text.encodeUtf8 (UUID.toText task.id)
          forM_ [1,2 :: Int] $ \_ -> responseStatus <$> apiRequest ctx.deployedApplication HTTP.methodPut linkPath headers "" `shouldReturn` status200
          lateTicket <- createCanonicalTicket ctx.deployedWSState (grantPrincipal user) scope reconnect
          withDeclaredClient (client 3 lateTicket.ticket) $ \_ -> do
            seal <- receive; association <- receive; checkpoint <- receive
            map frameType [seal,association,checkpoint] `shouldBe` [Just "change",Just "change",Just "checkpoint"]
            (jsonField "event" association >>= jsonField "entity" >>= jsonField "id") `shouldBe` Just (String (UUID.toText identifier<>":"<>UUID.toText task.id))
            mapM_ noEvidence [seal,association,checkpoint]
          effects <- sessionRecords <$> listOutboxAfter ctx.deployedEnv.pool scope 0 100
          length effects `shouldBe` 6
          mapM_ (noEvidence . (.outboxEnvelope)) effects
          metadata <- apiRequest ctx.deployedApplication methodGet path headers ""
          responseStatus metadata `shouldBe` status200
          value <- decodeResponse metadata :: IO Value
          jsonField "committed_bytes" value `shouldBe` Just (toJSON sealed.committedBytes)
          noEvidence value
          responseStatus <$> apiRequest ctx.deployedApplication methodGet path (authHeaders outsideToken.rawToken) "" `shouldReturn` HTTP.status403
          let foreignPath = "/api/v1/workspaces/"<>Text.encodeUtf8 (UUID.toText other.id)<>"/sessions/"<>Text.encodeUtf8 (UUID.toText identifier)
          responseStatus <$> apiRequest ctx.deployedApplication methodGet foreignPath headers "" `shouldReturn` HTTP.status404
          revokeTicket <- canonicalTicketFor ctx (grantPrincipal user) scope audience
          initial <- newEmptyMVar; revoked <- newEmptyMVar; closed <- newEmptyMVar
          let revokeClient = asyncWithUnmask $ \unmask -> unmask $
                (WS.runClient "127.0.0.1" port ("/api/v1/ws?ticket=" <> showTicket revokeTicket.ticket) $ \conn -> do
                  WS.receiveData conn >>= putMVar initial
                  WS.receiveData conn >>= putMVar revoked
                  ended <- try @WS.ConnectionException (WS.receiveData conn :: IO Text)
                  putMVar closed (either (const True) (const False) ended)) `catch` \(_ :: WS.ConnectionException) -> pure ()
          withDeclaredClient revokeClient $ \_ -> do
            frameType <$> awaitFrame initial `shouldReturn` Just "checkpoint"
            Auth.deleteWorkspaceMembership ctx.deployedEnv.pool workspace.id user `shouldReturn` True
            revokedRecord <- latestOutbox ctx scope
            dispatchCanonicalOutbox ctx.deployedEnv.pool ctx.deployedWSState [revokedRecord]
            frameType <$> awaitFrame revoked `shouldReturn` Just "access_revoked"
            declaredPhase "session revoked socket close" (takeMVar closed) `shouldReturn` True
          responseStatus <$> apiRequest ctx.deployedApplication methodGet path headers "" `shouldReturn` HTTP.status403
          foreignProject <- Project.createProject ctx.deployedEnv.pool (CreateProject other.id Nothing "other client wake" Nothing Nothing Nothing)
          foreignRecord <- latestOutbox ctx (WorkspaceScope other.id)
          dispatchCanonicalOutbox ctx.deployedEnv.pool ctx.deployedWSState [foreignRecord]
          observed <- awaitFrame otherClient.clientObserved
          assertChangeIdentity foreignRecord foreignProject.id observed
          wait otherClient.clientWorker

  it "keeps declared HTTP and WebSocket audiences stable while replaying bot metadata and genuine system work" $
    withLocalSandboxAppContext $ \ctx -> boundedDeclaredScenario $ do
      workspace <- declaredPhase "workspace setup" (createTestWorkspace ctx.localEnv "declared-stream-audience")
      let identityA = declaredIdentity "Reviewer" "pi:gpt-6.1-sol:extra-high"
          identityB = declaredIdentity "Developer" "other:model:default"
          header identity = ("X-HMem-Bot-Identity", LBS.toStrict (encode identity))
          scope = object ["scope" .= ("workspace" :: Text), "workspace_id" .= workspace.id]
          createWith identity title = declaredPhase (T.unpack title) $ do
            response <- apiRequest ctx.localApplication methodPost "/api/v1/projects" [header identity]
              (encode (object ["workspace_id" .= workspace.id, "name" .= (title :: Text)]))
            responseStatus response `shouldBe` status200
            decodeResponse response :: IO Project
          actor identity = object ["type" .= ("service" :: Text), "id" .= localDeclaredBotId identity, "bot_identity" .= identity]
          eventActor frame = jsonField "event" frame >>= jsonField "actor"
          persistedActor identifier records =
            [jsonField "actor" record.outboxEnvelope | record <- records
            , (jsonField "entity" record.outboxEnvelope >>= jsonField "id") == Just (toJSON identifier)]
      started <- declaredPhase "snapshot setup" $ apiRequest ctx.localApplication methodPost "/api/v1/change-stream/resync" [header identityA]
        (encode (object ["scope" .= scope, "page_size" .= (100 :: Int), "start_idempotency_key" .= ("declared-stream-audience-snapshot" :: Text)]))
      responseStatus started `shouldBe` status200
      snapshot <- decodeResponse started :: IO ChangeStreamResyncResponse
      resume <- maybe (fail "expected terminal local snapshot") pure snapshot.resumeToken
      issued <- declaredPhase "ticket setup" $ apiRequest ctx.localApplication methodPost "/api/v1/change-stream/ticket" [header identityB]
        (encode (object ["scope" .= scope, "resume_token" .= resume]))
      responseStatus issued `shouldBe` status200
      ticket <- decodeResponse issued :: IO WebSocketTicketResponse
      backlog <- createWith identityA "declared backlog"
      system <- declaredPhase "system write" $ withRequestContext emptyRequestContext $
        Project.createProject ctx.localEnv.pool (CreateProject workspace.id Nothing "genuine system work" Nothing Nothing Nothing)
      backlogRecords <- declaredPhase "backlog metadata read" $ listOutboxAfter ctx.localEnv.pool (WorkspaceScope workspace.id) 0 100
      let path = "/api/v1/ws?ticket=" <> Text.encodeUtf8 ticket.ticket
          upgrade = [("Connection", "Upgrade"), ("Upgrade", "websocket"), ("Sec-WebSocket-Version", "13"), ("Sec-WebSocket-Key", "dGhlIHNhbXBsZSBub25jZQ==")]
      forM_ [[header identityA, header identityA], [("X-HMem-Bot-Identity", "null")]] $ \invalid -> do
        rejected <- declaredPhase "ticket declaration rejection" $ apiRequest ctx.localApplication methodGet path (upgrade <> invalid) ""
        responseStatus rejected `shouldBe` HTTP.status400
        body <- decodeResponse rejected :: IO Value
        jsonField "error" body `shouldBe` Just (String "invalid_bot_identity")
      withWebSocketApplication ctx.localWSState ctx.localApplication $ \port -> do
        frames <- newChan
        let receive phase = declaredPhase phase (readChan frames >>= decodeFrame)
            start = asyncWithUnmask $ \unmask -> unmask $
              WS.runClientWith "127.0.0.1" port ("/api/v1/ws?ticket=" <> showTicket ticket.ticket)
                WS.defaultConnectionOptions [header identityA] $ \conn -> do
                  replicateM_ 5 (WS.receiveData conn >>= writeChan frames)
                  WS.sendClose conn ("declared scenario complete" :: Text)
        withDeclaredClient start $ \_client -> do
          replayed <- receive "backlog receive"
          frameType replayed `shouldBe` Just "change"
          (jsonField "event" replayed >>= jsonField "entity" >>= jsonField "id") `shouldBe` Just (toJSON backlog.id)
          eventActor replayed `shouldBe` Just (actor identityA)
          [eventActor replayed] `shouldBe` persistedActor backlog.id backlogRecords
          systemFrame <- receive "system receive"
          frameType systemFrame `shouldBe` Just "change"
          (jsonField "event" systemFrame >>= jsonField "entity" >>= jsonField "id") `shouldBe` Just (toJSON system.id)
          eventActor systemFrame `shouldBe` Just (object ["type" .= ("system" :: Text), "id" .= Null])
          [eventActor systemFrame] `shouldBe` persistedActor system.id backlogRecords
          frameType <$> receive "backlog checkpoint" `shouldReturn` Just "checkpoint"
          live <- createWith identityB "declared live"
          records <- declaredPhase "live metadata read" $ listOutboxAfter ctx.localEnv.pool (WorkspaceScope workspace.id) 0 100
          declaredPhase "live dispatch" (dispatchCanonicalOutbox ctx.localEnv.pool ctx.localWSState records)
          delivered <- receive "live receive"
          frameType delivered `shouldBe` Just "change"
          (jsonField "event" delivered >>= jsonField "entity" >>= jsonField "id") `shouldBe` Just (toJSON live.id)
          eventActor delivered `shouldBe` Just (actor identityB)
          [eventActor delivered] `shouldBe` persistedActor live.id records
          frameType <$> receive "live checkpoint" `shouldReturn` Just "checkpoint"

  it "stops an acquired dispatcher when subsequent harness setup fails" $
    withLocalSandboxAppContext $ \ctx -> do
      stopped <- newEmptyMVar
      result <- try @SomeException $
        bracket
          (startChangeStreamWorker ctx.localEnv.pool testChangeStreamConfig ctx.localWSState)
          (\worker -> timeout 2000000 (stopChangeStreamWorker worker) >>= putMVar stopped)
          (\_worker -> ioError $ userError "injected harness setup failure")
      case result of
        Left _ -> pure ()
        Right () -> expectationFailure "injected harness setup failure unexpectedly succeeded"
      timeout 2000000 (takeMVar stopped) `shouldReturn` Just (Just ())

  it "starts a bracket-acquired dispatcher unmasked and joins cancellation" $
    withLocalSandboxAppContext $ \ctx -> do
      entered <- newEmptyMVar
      released <- newIORef False
      let startup = do
            masking <- getMaskingState
            putMVar entered masking
            started <- getMonotonicTimeNSec
            let spin = do
                  release <- atomicModifyIORef' released (\value -> (value, value))
                  now <- getMonotonicTimeNSec
                  if release || now - started >= 6000000000
                    then ioError (userError "fixture startup gate finished before database work")
                    else spin
            spin
          cleanup worker = do
            atomicModifyIORef' released (const (True, ()))
            timeout 2000000 (stopChangeStreamWorker worker) `shouldReturn` Just ()
      observed <- bracket
        (startChangeStreamWorkerWithStartupHook startup ctx.localEnv.pool testChangeStreamConfig ctx.localWSState)
        cleanup $ \worker -> do
          masking <- timeout 2000000 (takeMVar entered)
          stopped <- timeout 2000000 (stopChangeStreamWorker worker)
          pure (masking, stopped)
      observed `shouldBe` (Just Unmasked, Just ())

  it "publishes dispatcher completion when its startup hook fails" $
    withLocalSandboxAppContext $ \ctx -> do
      entered <- newEmptyMVar
      let startup = putMVar entered () >> ioError (userError "fixture startup failure before SQL")
          joined worker = timeout 2000000 (stopChangeStreamWorker worker) `shouldReturn` Just ()
      bracket
        (startChangeStreamWorkerWithStartupHook startup ctx.localEnv.pool testChangeStreamConfig ctx.localWSState)
        joined $ \worker -> do
          timeout 2000000 (takeMVar entered) `shouldReturn` Just ()
          joined worker

  it "reports failed delivery cleanup from dispatcher stop with its cancellation cause" $
    withLocalSandboxAppContext $ \ctx -> do
      entered <- newEmptyMVar
      swallowed <- newEmptyMVar
      let failure = DeliveryCleanupFailure (toException AsyncCancelled) (toException DeliveryUnconfirmed)
          startup = mask $ \_ -> do
            putMVar entered ()
            handleCanonicalDispatchFailure (putMVar swallowed ()) (throwIO failure)
          stopped worker = do
            outcome <- declaredPhase "unclean dispatcher stop" (try @DeliveryCleanupFailure (stopChangeStreamWorker worker))
            case outcome of
              Left (DeliveryCleanupFailure original cleanup) -> do
                fromException original `shouldBe` Just AsyncCancelled
                fromException cleanup `shouldBe` Just DeliveryUnconfirmed
              Right () -> expectationFailure "dispatcher stop hid unconfirmed cleanup"
      bracket
        (startChangeStreamWorkerWithStartupHook startup ctx.localEnv.pool testChangeStreamConfig ctx.localWSState)
        stopped $ \worker -> do
          declaredPhase "unclean dispatcher startup" (takeMVar entered)
          stopped worker
          tryTakeMVar swallowed `shouldReturn` Nothing

  it "rethrows cancellation from a deterministically blocked canonical dispatch" $
    forM_ [toException ThreadKilled, toException AsyncCancelled] $ \cancellation -> do
      entered <- newEmptyMVar
      cleanup <- newEmptyMVar
      blocker <- newEmptyMVar
      worker <- async $ handleCanonicalDispatchFailure (putMVar cleanup ()) $ do
        putMVar entered ()
        takeMVar blocker
      takeMVar entered
      throwTo (asyncThreadId worker) cancellation
      result <- waitCatch worker
      case result of
        Left err -> case fromException err :: Maybe SomeAsyncException of
          Just _ -> pure ()
          Nothing -> expectationFailure "blocked canonical dispatch raised a synchronous exception"
        Right () -> expectationFailure "blocked canonical dispatch swallowed cancellation"
      tryTakeMVar cleanup `shouldReturn` Nothing

  it "closes the accepted socket without allocating Warp resources after a failed fork hook" $ do
    transport <- newNativeTransport
    acceptedByHook <- newEmptyMVar
    spawned <- newEmptyMVar
    allowChild <- newEmptyMVar
    connectionAllocated <- newEmptyMVar
    let settings = defaultSettings
          { settingsAccept = \listener -> do
              accepted <- Socket.accept listener
              putMVar acceptedByHook (fst accepted)
              pure accepted
          , settingsFork = \launch -> do
              child <- asyncWithUnmask $ \unmask -> takeMVar allowChild >> launch unmask
              putMVar spawned child
              ioError (userError "injected original Warp fork failure")
          }
    bracket openFreePort (Socket.close . snd) $ \(port, listener) ->
      bracket (Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol) Socket.close $ \peer -> do
        Socket.connect peer (Socket.SockAddrInet (fromIntegral port) (Socket.tupleToHostAddress (127,0,0,1)))
        withNativeTransportSettings transport settings $ \native -> do
          let allocate socket = putMVar connectionAllocated () >> socketConnection native socket
              app _ respond = respond (Wai.responseLBS status200 [] "")
          failure <- try @IOException $ runPlainSocketWith allocate native listener app
          accepted <- declaredPhase "original hook accepted socket" (takeMVar acceptedByHook)
          child <- declaredPhase "fork hook spawned delayed child" (takeMVar spawned)
          flip finally (void (tryPutMVar allowChild ()) >> Socket.close peer >> joinClient "delayed fork child" child) $ do
            failure `shouldSatisfy` either (const True) (const False)
            closed <- try @IOException (Socket.getSocketName accepted)
            closed `shouldSatisfy` either (const True) (const False)
            putMVar allowChild ()
            declaredPhase "cancelled startup child exited" (wait child)
            tryTakeMVar connectionAllocated `shouldReturn` Nothing

  it "retires an idle socket after a PostgreSQL authorization timeout and allows bearer reconnect" $
    withDeployedSandboxAppContext $ \ctx -> do
      workspace <- createTestWorkspace ctx.deployedEnv "canonical-reauthorization-failure"
      subscriber <- createDeployedSandboxUser ctx.deployedEnv False False
      _ <- Auth.upsertWorkspaceMembership ctx.deployedEnv.pool workspace.id
        (Auth.UpsertWorkspaceMembership subscriber Auth.WorkspaceRoleRead) Nothing
      let principal = grantPrincipal subscriber
          scope = WorkspaceScope workspace.id
          audience = AuthenticatedAudience (UUID.toText subscriber) subscriber
          trusted = principal { actorId = "healthy-peer", authority = PrincipalSyntheticLocalSuperadmin }
          trustedAudience = TrustedAudience "local:healthy-peer"
          connectionString = ctx.deployedEnv.testDb.testDbConnStr
      resume <- terminalResume ctx scope audience
      ticket <- createCanonicalTicket ctx.deployedWSState principal scope resume
      healthy <- canonicalTicketFor ctx trusted scope trustedAudience
      entered <- newEmptyMVar
      release <- newEmptyMVar
      initial <- newEmptyMVar
      disconnected <- newEmptyMVar
      let holdMemberships pool = DBPool.withConn pool $ \connection ->
            (do
              result <- Session.run (Session.sql "BEGIN; LOCK TABLE workspace_memberships IN ACCESS EXCLUSIVE MODE") connection
              either (fail . show) pure result
              putMVar entered ()
              takeMVar release)
            `finally` void (Session.run (Session.sql "ROLLBACK") connection)
          failingPeer port =
            WS.runClient "127.0.0.1" port ("/api/v1/ws?ticket=" <> showTicket ticket.ticket) $ \conn -> do
              WS.receiveData conn >>= putMVar initial
              result <- try @WS.ConnectionException (WS.receiveData conn :: IO Text)
              putMVar disconnected (either (const True) (const False) result)
      withWebSocketApplication ctx.deployedWSState ctx.deployedApplication $ \port ->
        withAsync (failingPeer port) $ \failed ->
          withActivityFrames port healthy.ticket $ \healthyFrames -> do
              let awaitCanonical frames = declaredPhase "canonical peer frame" (readChan frames >>= decodeFrame)
              frameType <$> awaitFrame initial `shouldReturn` Just "checkpoint"
              frameType <$> awaitCanonical healthyFrames `shouldReturn` Just "checkpoint"
              bracket (DBPool.createPool connectionString 1 5.0 30000) destroyAllResources $ \holderPool ->
                bracket (DBPool.createPool connectionString 1 5.0 100) destroyAllResources $ \failurePool ->
                  withAsync (holdMemberships holderPool) $ \holder -> do
                    declaredPhase "membership lock acquisition" (takeMVar entered)
                    Auth.hasWorkspaceRole failurePool (Just principal) workspace.id Auth.WorkspaceRoleRead
                      `shouldThrow` (\case DBPool.DBStatementTimeout -> True; _ -> False)
                    failureStarted <- getMonotonicTimeNSec
                    declaredPhase "failed reauthorization" $
                      reauthorizeCanonicalConnections failurePool ctx.deployedWSState
                    declaredPhase "retired peer disconnect" (takeMVar disconnected) `shouldReturn` True
                    retired <- getMonotonicTimeNSec
                    qualificationMetric "database-failure-retirement" (object ["reauthorization_to_disconnect_ms" .= milliseconds failureStarted retired, "statement_timeout_ms" .= (100 :: Int)])
                    wait failed
                    putMVar release ()
                    wait holder
              validity <- validateCanonicalResumeToken ctx.deployedEnv.pool scope audience resume
              validity `shouldSatisfy` either (const False) (const True)
              reconnect <- createCanonicalTicket ctx.deployedWSState principal scope resume
              recoveryStarted <- getMonotonicTimeNSec
              withActivityFrames port reconnect.ticket $ \reconnectedFrames -> do
                frameType <$> awaitCanonical reconnectedFrames `shouldReturn` Just "checkpoint"
                recovered <- getMonotonicTimeNSec
                qualificationMetric "database-failure-reconnect" (object ["socket_open_to_checkpoint_ms" .= milliseconds recoveryStarted recovered])
                project <- Project.createProject ctx.deployedEnv.pool
                  (CreateProject workspace.id Nothing "visible after failed authorization" Nothing Nothing Nothing)
                record <- latestOutbox ctx scope
                dispatchCanonicalOutbox ctx.deployedEnv.pool ctx.deployedWSState [record]
                awaitCanonical reconnectedFrames >>= assertChangeIdentity record project.id
                awaitCanonical healthyFrames >>= assertChangeIdentity record project.id

  it "admits and dispatches while a catch-up blocked in PostgreSQL stays retained" $
    withDeployedSandboxAppContext $ \ctx -> do
      blockedWorkspace <- createTestWorkspace ctx.deployedEnv "retained-canonical-catch-up"
      liveWorkspace <- createTestWorkspace ctx.deployedEnv "admitted-beside-retained-catch-up"
      let principal = Principal
            { actorType = ActorUser, actorId = "retained-peer", actorLabel = "retained peer"
            , authority = PrincipalSyntheticLocalSuperadmin }
          audience = TrustedAudience "local:retained-peer"
          blockedScope = WorkspaceScope blockedWorkspace.id
          liveScope = WorkspaceScope liveWorkspace.id
          connectionString = ctx.deployedEnv.testDb.testDbConnStr
      blocked <- canonicalTicketFor ctx principal blockedScope audience
      live <- canonicalTicketFor ctx principal liveScope audience
      entered <- newEmptyMVar
      release <- newEmptyMVar
      disconnected <- newEmptyMVar
      -- Hold the scope row that every replay page locks, so the catch-up's
      -- libpq call outlasts both the work and the cleanup budgets.
      let holdScope pool = DBPool.withConn pool $ \connection ->
            (do
              result <- Session.run (Session.sql ("BEGIN; SELECT * FROM hmem_change_stream_lock('workspace', '"
                <> Text.encodeUtf8 (UUID.toText blockedWorkspace.id) <> "')")) connection
              either (fail . show) pure result
              putMVar entered ()
              takeMVar release)
            `finally` void (Session.run (Session.sql "ROLLBACK") connection)
          blockedPeer port =
            WS.runClient "127.0.0.1" port ("/api/v1/ws?ticket=" <> showTicket blocked.ticket) $ \conn -> do
              result <- try @WS.ConnectionException (WS.receiveData conn :: IO Text)
              putMVar disconnected (either (const True) (const False) result)
      bracket (DBPool.createPool connectionString 1 5.0 30000) destroyAllResources $ \observerPool ->
       bracket (DBPool.createPool connectionString 1 5.0 30000) destroyAllResources $ \holderPool ->
        withAsync (holdScope holderPool) $ \holder -> do
          let lockWaiters = runSession observerPool (Session.statement () scopeLockWaitersStatement)
              awaitLockWaiter = do
                waiting <- lockWaiters
                when (waiting /= 1) (threadDelay 20000 >> awaitLockWaiter)
              retainedDrain = do
                -- Only the retained catch-up remains after every peer closed.
                lockWaiters `shouldReturn` 1
                awaitWebSocketDrain ctx.deployedWSState 300000 `shouldThrow` (== DeliveryUnconfirmed)
                putMVar release ()
                declaredPhase "scope lock release" (wait holder)
          declaredPhase "scope lock acquisition" (takeMVar entered)
          lockWaiters `shouldReturn` 0
          -- Give every application pool stripe an idle connection, so the
          -- catch-up reaches the held lock well within its work budget.
          capabilities <- getNumCapabilities
          mapM_ wait =<< mapM (\capability -> asyncOn capability
            (runSession ctx.deployedEnv.pool (Session.sql "SELECT 1"))) [0 .. capabilities - 1]
          withWebSocketApplicationBeforeDrain ctx.deployedWSState ctx.deployedApplication retainedDrain $ \port -> do
            started <- getMonotonicTimeNSec
            withAsync (blockedPeer port) $ \peer -> do
              declaredPhase "catch-up waits on the held scope lock" awaitLockWaiter
              waited <- getMonotonicTimeNSec
              milliseconds started waited `shouldSatisfy` (< (750 :: Double))
              declaredPhase "blocked peer retired" (takeMVar disconnected) `shouldReturn` True
              retired <- getMonotonicTimeNSec
              milliseconds started retired `shouldSatisfy` (>= (750 :: Double))
              declaredPhase "blocked peer closed" (wait peer)
            -- Let the unfinished join exhaust its cleanup budget.
            threadDelay 500000
            lockWaiters `shouldReturn` 1
            withActivityFrames port live.ticket $ \frames -> do
              let awaitCanonical = declaredPhase "admitted peer frame" (readChan frames >>= decodeFrame)
              frameType <$> awaitCanonical `shouldReturn` Just "checkpoint"
              project <- Project.createProject ctx.deployedEnv.pool
                (CreateProject liveWorkspace.id Nothing "delivered beside retained catch-up" Nothing Nothing Nothing)
              record <- latestOutbox ctx liveScope
              declaredPhase "live dispatch" (dispatchCanonicalOutbox ctx.deployedEnv.pool ctx.deployedWSState [record])
              awaitCanonical >>= assertChangeIdentity record project.id
          -- The stalled catch-up passed its work deadline without a write, its
          -- join stayed unfinished, and the drain reclaimed it afterwards.
          snapshot <- awaitTelemetry ctx.deployedWSState "stalled catch-up telemetry" $ \current ->
            outcomeCount current ("websocket_replay", "live", "completed") == 1
          map (outcomeCount snapshot)
            [ ("websocket_turn", "", "retired_deadline"), ("websocket_cleanup", "", "unfinished")
            , ("websocket_reclaim", "", "reclaimed"), ("websocket_replay", "initial", "retired")
            , ("websocket_replay", "initial", "completed"), ("websocket_turn", "", "retired_write_timeout") ]
            `shouldBe` [1, 1, 1, 1, 1, 0]
          fmap (\stats -> (stats.timed, stats.maxMs >= 250)) (Map.lookup ("websocket_reclaim", "", "reclaimed") snapshot.outcomes)
            `shouldBe` Just (1, True)
          fmap (.maxMs) (Map.lookup ("websocket_turn", "", "retired_deadline") snapshot.outcomes)
            `shouldSatisfy` maybe False (>= 750)
          (snapshot.delivery.leasesQuarantined, snapshot.delivery.physicalAdmitted) `shouldBe` (0, 0)

  it "retires the canonical owner when expiry output throws" $
    withDeployedSandboxAppContext $ \ctx -> do
      armed <- newIORef False
      attempted <- newEmptyMVar
      ownerEnded <- newEmptyMVar
      let write send bytes = do
            failWrite <- readIORef armed
            if failWrite then putMVar attempted () >> ioError (userError "injected canonical output failure")
                         else send bytes
          app = observeRawWebSocket ctx.deployedApplication write (putMVar ownerEnded ())
      withExpiringCanonicalPeer ctx app $ \runPeer -> runPeer $ \conn -> do
        let initial = declaredPhase "initial expiry checkpoint" (WS.receiveData conn >>= decodeFrame)
        frameType <$> initial `shouldReturn` Just "checkpoint"
        atomicModifyIORef' armed (const (True, ()))
        declaredPhase "expiry write reached the transport" (takeMVar attempted)
        result <- declaredPhase "failed expiry write disconnected peer" $
          try @WS.ConnectionException (WS.receiveData conn :: IO Text)
        result `shouldSatisfy` either (const True) (const False)
        declaredPhase "failed expiry owner finalized" (takeMVar ownerEnded)

  it "interrupts the exact TLS socket after canonical output fails with the peer still reading" $
    withDeployedSandboxAppContext $ \ctx -> do
      armed <- newIORef False
      attempted <- newEmptyMVar
      ownerEnded <- newEmptyMVar
      let write send bytes = do
            failWrite <- readIORef armed
            if failWrite then putMVar attempted () >> ioError (userError "injected TLS output failure")
                         else send bytes
          app = observeRawWebSocket ctx.deployedApplication write (putMVar ownerEnded ())
      withTlsWebSocketApplication ctx.deployedWSState app $ \port -> do
        ticket <- expiringCanonicalTicket ctx
        runTlsPeer port ticket.ticket $ \conn -> do
          let initial = declaredPhase "initial TLS expiry checkpoint" (WS.receiveData conn >>= decodeFrame)
          frameType <$> initial `shouldReturn` Just "checkpoint"
          atomicModifyIORef' armed (const (True, ()))
          declaredPhase "TLS expiry write reached the transport" (takeMVar attempted)
          ended <- declaredPhase "native TLS abort disconnected peer" $
            (False <$ (WS.receiveData conn :: IO Text))
              `catch` (\(_ :: WS.ConnectionException) -> pure True)
              `catch` (\(_ :: TLS.TLSException) -> pure True)
          ended `shouldBe` True
          declaredPhase "failed TLS expiry owner finalized" (takeMVar ownerEnded)

  it "bounds an actual plain native write while a healthy peer progresses" $
    withDeployedSandboxAppContext $ \ctx ->
      assertBlockedNativeWrite withWebSocketApplication runPlainPeer ctx

  it "bounds an actual TLS native write while a healthy peer progresses" $
    withDeployedSandboxAppContext $ \ctx ->
      assertBlockedNativeWrite withTlsWebSocketApplication runTlsPeer ctx

  it "rejects the thirty-third physical connection before upgrade" $
    withLocalSandboxAppContext $ \ctx ->
      withWebSocketApplication ctx.localWSState ctx.localApplication $ \port ->
        withHeldLegacyPeers port 32 $ do
          headers <- upgradeResponseHeaders port
          headers `shouldSatisfy` BS.isInfixOf "503 Service Unavailable"
          headers `shouldSatisfy` BS.isInfixOf "Retry-After: 1"
          headers `shouldSatisfy` (not . BS.isInfixOf "Sec-WebSocket-Accept")
          snapshot <- awaitTelemetry ctx.localWSState "unavailable rejection telemetry" $ \current ->
            outcomeCount current ("websocket_upgrade", "none", "rejected_unavailable") == 1
          outcomeCount snapshot ("websocket_upgrade", "none", "accepted") `shouldBe` 32
          snapshot.delivery.physicalAdmitted `shouldBe` 32

  it "keeps a successful Close physically admitted until its peer acknowledges" $
    withLocalSandboxAppContext $ \ctx -> do
      workspace <- createTestWorkspace ctx.localEnv "native-close-physical-lifetime"
      let scope = WorkspaceScope workspace.id
          principal = Principal
            { actorType = ActorUser, actorId = "local-user", actorLabel = "Local User"
            , authority = PrincipalSyntheticLocalSuperadmin }
      resume <- terminalLocalResume ctx scope (TrustedAudience "local:local-user")
      ticket <- createCanonicalTicketWithTtl ctx.localWSState 0.5 principal scope resume
      closing <- newEmptyMVar
      acknowledge <- newEmptyMVar
      withWebSocketApplication ctx.localWSState ctx.localApplication $ \port ->
        withHeldLegacyPeers port 31 $ do
          let peer = runPlainPeer port ticket.ticket $ \conn -> do
                first <- WS.receiveData conn >>= decodeFrame
                frameType first `shouldBe` Just "checkpoint"
                expired <- WS.receiveData conn >>= decodeFrame
                frameType expired `shouldBe` Just "resync_required"
                putMVar closing ()
                takeMVar acknowledge
                void (try @WS.ConnectionException (WS.receiveData conn :: IO Text))
          withAsync peer $ \worker -> do
            declaredPhase "successful Close written" (takeMVar closing)
            (do
              headers <- upgradeResponseHeaders port
              headers `shouldSatisfy` BS.isInfixOf "503 Service Unavailable")
              `finally` void (tryPutMVar acknowledge ())
            declaredPhase "closing peer acknowledged" (wait worker)
            let released = do
                  headers <- upgradeResponseHeaders port
                  if "503 Service Unavailable" `BS.isInfixOf` headers
                    then threadDelay 10000 >> released
                    else headers `shouldSatisfy` BS.isInfixOf "HTTP/1.1 101 "
            declaredPhase "physical admission released after owner exit" released

  it "joins the expiry writer when its canonical peer disconnects during terminal output" $
    withDeployedSandboxAppContext $ \ctx -> do
      armed <- newIORef False
      writing <- newEmptyMVar
      release <- newEmptyMVar
      writerEnded <- newEmptyMVar
      ownerEnded <- newEmptyMVar
      let write send bytes = do
            holdWrite <- readIORef armed
            if holdWrite
              then (putMVar writing () >> takeMVar release) `finally` putMVar writerEnded ()
              else send bytes
          app = observeRawWebSocket ctx.deployedApplication write (putMVar ownerEnded ())
      withExpiringCanonicalPeer ctx app $ \runPeer ->
        (do
          runPeer $ \conn -> do
            let initial = declaredPhase "initial expiry checkpoint" (WS.receiveData conn >>= decodeFrame)
            frameType <$> initial `shouldReturn` Just "checkpoint"
            atomicModifyIORef' armed (const (True, ()))
            declaredPhase "expiry writer entered before peer disconnect" (takeMVar writing)
          declaredPhase "disconnected expiry writer joined" (takeMVar writerEnded)
          declaredPhase "disconnected expiry owner finalized" (takeMVar ownerEnded))
          `finally` void (tryPutMVar release ())

  it "measures upgrade, replay, delivery and connection outcomes without identifiers or HTTP 500 upgrades" $ do
    requestLines <- newIORef []
    requestLogger <- jsonRequestLogger (\line -> atomicModifyIORef' requestLines (\captured -> (captured <> [fromLogStr line], ())))
    withDeployedSandboxAppLogged requestLogger mkDeployedSandboxConfig $ \ctx -> do
      telemetryLines <- newIORef []
      let st = ctx.deployedWSState
          pool = ctx.deployedEnv.pool
      attachWebSocketTelemetryLog st (\line -> atomicModifyIORef' telemetryLines (\captured -> (captured <> [fromLogStr line], ())))
      workspace <- createTestWorkspace ctx.deployedEnv "websocket-telemetry"
      subscriber <- createDeployedSandboxUser ctx.deployedEnv False False
      _ <- Auth.upsertWorkspaceMembership pool workspace.id
        (Auth.UpsertWorkspaceMembership subscriber Auth.WorkspaceRoleRead) Nothing
      let principal = grantPrincipal subscriber
          scope = WorkspaceScope workspace.id
          audience = AuthenticatedAudience (UUID.toText subscriber) subscriber
      resume <- terminalResume ctx scope audience
      accepted <- createCanonicalTicket st principal scope resume
      unsupported <- createCanonicalTicket st principal scope resume
      -- The unsupported-version handshake throws; keep that expected
      -- application failure local to its own disposition.
      withSocketApplication st False runPlainSocket (pure ()) ctx.deployedApplication $ \port -> do
        withActivityFrames port accepted.ticket $ \frames -> do
          let next = declaredPhase "telemetry peer frame" (readChan frames >>= decodeFrame)
          frameType <$> next `shouldReturn` Just "checkpoint"
          project <- Project.createProject pool
            (CreateProject workspace.id Nothing "telemetry live delivery" Nothing Nothing Nothing)
          committed <- getMonotonicTimeNSec
          record <- latestOutbox ctx scope
          dispatchCanonicalOutbox pool st [record]
          next >>= assertChangeIdentity record project.id
          observed <- getMonotonicTimeNSec
          frameType <$> next `shouldReturn` Just "checkpoint"
          -- Only this independent client observes delivery; the server
          -- records socket-write completion, not receipt.
          qualificationMetric "websocket-commit-to-observed" (object ["commit_to_observed_ms" .= milliseconds committed observed])
        unauthorized <- rawUpgrade port "/api/v1/ws?ticket=unknown-telemetry-ticket" "13" []
        unauthorized `shouldSatisfy` BS.isPrefixOf "HTTP/1.1 400"
        invalidIdentity <- rawUpgrade port ("/api/v1/ws?ticket=" <> Text.encodeUtf8 accepted.ticket) "13" ["X-HMem-Bot-Identity: not-json"]
        invalidIdentity `shouldSatisfy` BS.isPrefixOf "HTTP/1.1 400"
        failed <- rawUpgrade port ("/api/v1/ws?ticket=" <> Text.encodeUtf8 unsupported.ticket) "99" []
        failed `shouldSatisfy` (not . BS.isInfixOf "101 Switching Protocols")
        snapshot <- awaitTelemetry st "connection and physical release" $ \current ->
          outcomeCount current ("websocket_connection", "canonical", "closed") == 1
            && outcomeCount current ("websocket_upgrade", "canonical", "failed") == 1
            && current.delivery.physicalAdmitted == 0
        let events event = Map.toList (Map.map (.count) (Map.filterWithKey (\(name, _, _) _ -> name == event) snapshot.outcomes))
        events "websocket_upgrade" `shouldMatchList`
          [ (("websocket_upgrade", "canonical", "accepted"), 1)
          , (("websocket_upgrade", "canonical", "failed"), 1)
          , (("websocket_upgrade", "unresolved", "rejected_unauthorized"), 1)
          , (("websocket_upgrade", "unresolved", "rejected_invalid_identity"), 1) ]
        events "websocket_connection" `shouldBe` [(("websocket_connection", "canonical", "closed"), 1)]
        events "websocket_replay" `shouldMatchList`
          [ (("websocket_replay", "initial", "completed"), 1)
          , (("websocket_replay", "live", "completed"), 1) ]
        map fst (events "websocket_turn") `shouldSatisfy` all (\(_, _, outcome) -> outcome `elem` ["completed", "refused"])
        events "websocket_cleanup" `shouldBe` []
        -- Checkpoint, change, checkpoint and Close frames completed.
        outcomeCount snapshot ("websocket_write", "", "completed") `shouldSatisfy` (>= 4)
        outcomeCount snapshot ("websocket_write", "", "timed_out") `shouldBe` 0
        forM_ (Map.elems snapshot.outcomes) $ \stats -> stats.timed `shouldBe` stats.count
        -- A replay cycle lies within its connection's lifetime.
        let maxOf key = maybe 0 (.maxMs) (Map.lookup key snapshot.outcomes)
        maxOf ("websocket_replay", "initial", "completed") `shouldSatisfy`
          (<= maxOf ("websocket_connection", "canonical", "closed"))
        (snapshot.canonicalClients, snapshot.replaysPending, snapshot.delivery.leasesInUse) `shouldBe` (0, 0, 0)
      telemetry <- map decodeLine <$> readIORef telemetryLines
      map (\entry -> (textField "event" entry, textField "outcome" entry)) telemetry `shouldMatchList`
        [ (Just "websocket_upgrade", Just "accepted")
        , (Just "websocket_upgrade", Just "rejected_unauthorized")
        , (Just "websocket_upgrade", Just "rejected_invalid_identity")
        , (Just "websocket_upgrade", Just "failed")
        , (Just "websocket_replay", Just "completed")
        , (Just "websocket_connection", Just "closed") ]
      forM_ telemetry $ \entry -> KeyMap.keys entry `shouldSatisfy` all (`elem`
        [ "timestamp", "level", "event", "ticket_kind", "phase", "outcome", "handshake_ms", "lifetime_ms"
        , "replay_ms", "admitted", "leases", "lease_waiters", "quarantined" ])
      rawTelemetry <- BS.concat <$> readIORef telemetryLines
      forM_ [accepted.ticket, unsupported.ticket, "unknown-telemetry-ticket", UUID.toText subscriber, UUID.toText workspace.id] $ \secret ->
        BS.isInfixOf (Text.encodeUtf8 secret) rawTelemetry `shouldBe` False
      -- An accepted raw upgrade closes as an upgrade, never as an HTTP 500.
      requests <- map decodeLine <$> readIORef requestLines
      let upgrades = filter ((== Just "/api/v1/ws") . textField "route") requests
      length upgrades `shouldBe` 4
      map (KeyMap.lookup "status") requests `shouldSatisfy` notElem (Just (Number 500))
      map (\entry -> (textField "outcome" entry, KeyMap.lookup "status" entry)) upgrades `shouldMatchList`
        [ (Just "upgrade_closed", Just Null)
        , (Just "upgrade_closed", Just Null)
        , (Just "completed", Just (Number 400))
        , (Just "upgrade_failed", Just Null) ]

  it "never materializes global workspace-group membership rows" $
    withDeployedSandboxAppContext $ \ctx -> do
      workspace <- createTestWorkspace ctx.deployedEnv "global-snapshot-redaction"
      group <- WorkspaceGroup.createGroup ctx.deployedEnv.pool (CreateWorkspaceGroup "redacted members" Nothing)
      WorkspaceGroup.addMember ctx.deployedEnv.pool group.id workspace.id `shouldReturn` WorkspaceGroup.MemberAdded
      items <- runSession ctx.deployedEnv.pool (materializeSnapshot GlobalScope)
      (Just (String "workspace_group_membership") `elem` map (jsonField "kind") items) `shouldBe` False

  it "consumes a local canonical ticket before any legacy fallback" $
    withLocalSandboxAppContext $ \ctx -> do
      workspace <- createTestWorkspace ctx.localEnv "local-canonical-ticket"
      let scope = WorkspaceScope workspace.id
          audience = TrustedAudience "local:canonical-ticket"
          principal = Principal
            { actorType = ActorUser, actorId = "canonical-ticket"
            , actorLabel = "canonical ticket", authority = PrincipalSyntheticLocalSuperadmin }
      resume <- terminalLocalResume ctx scope audience
      issued <- createCanonicalTicket ctx.localWSState principal scope resume
      received <- newEmptyMVar
      withWebSocketApplication ctx.localWSState ctx.localApplication $ \port -> do
        client <- async $ (WS.runClient "127.0.0.1" port ("/api/v1/ws?ticket=" <> showTicket issued.ticket) $ \conn -> do
          WS.receiveData conn >>= putMVar received
          WS.sendClose conn ("done" :: Text)) `catch` \(_ :: WS.ConnectionException) -> pure ()
        frame <- awaitFrame received
        frameType frame `shouldBe` Just "checkpoint"
        wait client

  it "publishes one committed rename to workspace then global scope and suppresses no-ops" $
    withDeployedSandboxAppContext $ \ctx -> do
      workspace <- createTestWorkspace ctx.deployedEnv "canonical-workspace-rename"
      let workspaceScope = WorkspaceScope workspace.id
          cursorFor scope = do
            existing <- listOutboxAfter ctx.deployedEnv.pool scope 0 100
            pure $ maybe 0 (.outboxCursor) (safeLast existing)
      workspaceBefore <- cursorFor workspaceScope
      globalBefore <- cursorFor GlobalScope
      renamed <- WorkspaceDB.renameWorkspace ctx.deployedEnv.pool workspace.id (UpdateWorkspace "canonical-workspace-renamed")
      renamed `shouldSatisfy` isJust
      workspaceRecords <- listOutboxAfter ctx.deployedEnv.pool workspaceScope workspaceBefore 10
      globalRecords <- listOutboxAfter ctx.deployedEnv.pool GlobalScope globalBefore 10
      length workspaceRecords `shouldBe` 1
      length globalRecords `shouldBe` 1
      let [workspaceRecord] = workspaceRecords
          [globalRecord] = globalRecords
      workspaceRecord.outboxCursor `shouldSatisfy` (< globalRecord.outboxCursor)
      let transactionId record = jsonField "transaction" record.outboxEnvelope >>= jsonField "id"
      transactionId workspaceRecord `shouldSatisfy` isJust
      transactionId workspaceRecord `shouldBe` transactionId globalRecord
      mapM_ (\record -> do
        (jsonField "entity" record.outboxEnvelope >>= jsonField "type") `shouldBe` Just (String "workspace")
        (jsonField "entity" record.outboxEnvelope >>= jsonField "id") `shouldBe` Just (String $ UUID.toText workspace.id)
        (jsonField "entity" record.outboxEnvelope >>= jsonField "action") `shouldBe` Just (String "updated")) [workspaceRecord, globalRecord]
      workspaceNoopCursor <- cursorFor workspaceScope
      globalNoopCursor <- cursorFor GlobalScope
      _ <- WorkspaceDB.renameWorkspace ctx.deployedEnv.pool workspace.id (UpdateWorkspace "canonical-workspace-renamed")
      listOutboxAfter ctx.deployedEnv.pool workspaceScope workspaceNoopCursor 10 `shouldReturn` []
      listOutboxAfter ctx.deployedEnv.pool GlobalScope globalNoopCursor 10 `shouldReturn` []
      workspaceRollbackCursor <- cursorFor workspaceScope
      globalRollbackCursor <- cursorFor GlobalScope
      rollbackResult <- DBPool.withConn ctx.deployedEnv.pool $ \connection ->
        Session.run
          (Session.sql "BEGIN" >> Session.statement workspace.id rollbackRenameStatement >> Session.sql "ROLLBACK")
          connection
      case rollbackResult of
        Left err -> expectationFailure $ "Failed to roll back workspace rename: " <> show err
        Right () -> pure ()
      listOutboxAfter ctx.deployedEnv.pool workspaceScope workspaceRollbackCursor 10 `shouldReturn` []
      listOutboxAfter ctx.deployedEnv.pool GlobalScope globalRollbackCursor 10 `shouldReturn` []

  it "replays a committed rename and converges two edit-role clients without leaking to another workspace" $
    withDeployedSandboxAppContext $ \ctx -> do
      workspace <- createTestWorkspace ctx.deployedEnv "rename-convergence"
      otherWorkspace <- createTestWorkspace ctx.deployedEnv "rename-convergence-other"
      editorOne <- createDeployedSandboxUser ctx.deployedEnv False False
      editorTwo <- createDeployedSandboxUser ctx.deployedEnv False False
      unrelated <- createDeployedSandboxUser ctx.deployedEnv False False
      let scope = WorkspaceScope workspace.id
          audience userId = AuthenticatedAudience (UUID.toText userId) userId
          grantEdit userId = Auth.upsertWorkspaceMembership ctx.deployedEnv.pool workspace.id
            (Auth.UpsertWorkspaceMembership userId Auth.WorkspaceRoleEdit) Nothing
          principal userId = Principal
            { actorType = ActorUser, actorId = UUID.toText userId, actorLabel = "rename editor"
            , authority = PrincipalGrantUser userId }
      _ <- grantEdit editorOne
      _ <- grantEdit editorTwo
      _ <- Auth.upsertWorkspaceMembership ctx.deployedEnv.pool otherWorkspace.id
        (Auth.UpsertWorkspaceMembership unrelated Auth.WorkspaceRoleEdit) Nothing
      replayResume <- terminalResume ctx scope (audience editorOne)
      firstTicket <- canonicalTicketFor ctx (principal editorOne) scope (audience editorOne)
      secondTicket <- canonicalTicketFor ctx (principal editorTwo) scope (audience editorTwo)
      unrelatedTicket <- canonicalTicketFor ctx (principal unrelated) (WorkspaceScope otherWorkspace.id) (audience unrelated)
      before <- do
        records <- listOutboxAfter ctx.deployedEnv.pool scope 0 100
        pure $ maybe 0 (.outboxCursor) (safeLast records)
      withWebSocketApplication ctx.deployedWSState ctx.deployedApplication $ \port -> do
        firstClient <- startOneFrameClient port firstTicket.ticket
        secondClient <- startOneFrameClient port secondTicket.ticket
        unrelatedClient <- startOneFrameClient port unrelatedTicket.ticket
        mapM_ (awaitFrame . (.clientInitial)) [firstClient, secondClient, unrelatedClient]
        _ <- WorkspaceDB.renameWorkspace ctx.deployedEnv.pool workspace.id (UpdateWorkspace "rename converged")
        records <- listOutboxAfter ctx.deployedEnv.pool scope before 10
        record <- case records of
          [value] -> pure value
          _ -> fail "expected exactly one workspace-scoped rename event"
        replayed <- replayAndRotateResumeToken ctx.deployedEnv.pool 60 scope (audience editorOne) replayResume 10
        case replayed of
          Right ReplayPage { replayPageRecords = [replayedRecord] } ->
            replayedRecord.outboxEventId `shouldBe` record.outboxEventId
          other -> expectationFailure $ "expected the committed rename in replay: " <> show other
        dispatchCanonicalOutbox ctx.deployedEnv.pool ctx.deployedWSState [record]
        firstFrame <- awaitFrame firstClient.clientObserved
        secondFrame <- awaitFrame secondClient.clientObserved
        mapM_ (assertChangeIdentity record workspace.id) [firstFrame, secondFrame]
        threadDelay 100000
        tryTakeMVar unrelatedClient.clientObserved `shouldReturn` Nothing
        cancel unrelatedClient.clientWorker
        mapM_ (wait . (.clientWorker)) [firstClient, secondClient]

  it "replays then remains live with ordered cursor-free frames" $
    withDeployedSandboxAppContext $ \ctx -> do
      workspace <- createTestWorkspace ctx.deployedEnv "canonical-loopback"
      userId <- createDeployedSandboxUser ctx.deployedEnv False False
      _ <- Auth.upsertWorkspaceMembership ctx.deployedEnv.pool workspace.id
        (Auth.UpsertWorkspaceMembership userId Auth.WorkspaceRoleRead) Nothing
      let principal = Principal
            { actorType = ActorUser, actorId = UUID.toText userId, actorLabel = "loopback user"
            , authority = PrincipalGrantUser userId }
          scope = WorkspaceScope workspace.id
          audience = AuthenticatedAudience (UUID.toText userId) userId
      begun <- beginResync ctx.deployedEnv.pool 60 scope audience (pure [])
      begin <- either (fail . show) pure begun
      terminal <- readSnapshotPageWithTtl ctx.deployedEnv.pool 60 scope audience begin.snapshotToken 20
      resume <- case terminal of
        Right SnapshotPage { snapshotResumeToken = Just token } -> pure token
        other -> fail (show other)
      -- Prove the exact bearer/principal pairing before it crosses the
      -- loopback transport; this also yields the one-use ticket's seed.
      checked <- replayAndRotateResumeToken ctx.deployedEnv.pool 60 scope audience resume 20
      replayResume <- case checked of
        Right ReplayPage { replayPageResumeToken = token } -> pure token
        other -> fail (show other)
      issued <- createCanonicalTicket ctx.deployedWSState principal scope replayResume
      initial <- newEmptyMVar
      live <- newEmptyMVar
      withWebSocketApplication ctx.deployedWSState ctx.deployedApplication $ \port -> do
        client <- async $ (WS.runClient "127.0.0.1" port ("/api/v1/ws?ticket=" <> showTicket issued.ticket) $ \conn -> do
          first <- WS.receiveData conn :: IO Text
          putMVar initial first
          second <- WS.receiveData conn :: IO Text
          putMVar live second
          WS.sendClose conn ("done" :: Text)) `catch` \(_ :: WS.ConnectionException) -> pure ()
        initialFrame <- awaitFrame initial
        frameType initialFrame `shouldBe` Just "checkpoint"
        _ <- Project.createProject ctx.deployedEnv.pool CreateProject
          { workspaceId = workspace.id, parentId = Nothing, name = "after-checkpoint"
          , description = Nothing, priority = Nothing, metadata = Nothing }
        records <- listOutboxAfter ctx.deployedEnv.pool scope 0 20
        case reverse records of
          record:_ -> dispatchCanonicalOutbox ctx.deployedEnv.pool ctx.deployedWSState [record]
          [] -> fail "project mutation did not commit an outbox record"
        changeFrame <- awaitFrame live
        frameType changeFrame `shouldBe` Just "change"
        let event = jsonField "event" changeFrame
        (event >>= jsonField "cursor") `shouldBe` Nothing
        -- The client remains connected after its checkpoint, proving live
        -- dispatch rather than a replay-only one-shot socket.
        wait client

  it "holds a paginated HTTP snapshot at H and replays a concurrent direct-core commit" $
    withDeployedSandboxAppContext $ \ctx -> do
      workspace <- createTestWorkspace ctx.deployedEnv "canonical-snapshot-h"
      userId <- createDeployedSandboxUser ctx.deployedEnv False False
      _ <- Auth.upsertWorkspaceMembership ctx.deployedEnv.pool workspace.id
        (Auth.UpsertWorkspaceMembership userId Auth.WorkspaceRoleAdmin) Nothing
      accessToken <- issueDeployedSandboxPAT ctx.deployedEnv userId "snapshot H client"
      let headers = authHeaders accessToken.rawToken
          scopeValue = object ["scope" .= ("workspace" :: Text), "workspace_id" .= workspace.id]
      preResponse <- apiRequest ctx.deployedApplication methodPost "/api/v1/projects" headers
        (encode (object ["workspace_id" .= workspace.id, "name" .= ("before snapshot" :: Text)]))
      responseStatus preResponse `shouldBe` status200
      preSnapshot <- decodeResponse preResponse :: IO Project

      firstResponse <- apiRequest ctx.deployedApplication methodPost "/api/v1/change-stream/resync" headers
        (encode (object
          [ "scope" .= scopeValue
          , "page_size" .= (1 :: Int)
          , "start_idempotency_key" .= ("snapshot-h-release-gate-key-000001" :: Text)
          ]))
      responseStatus firstResponse `shouldBe` status200
      firstPage <- decodeResponse firstResponse :: IO ChangeStreamResyncResponse
      firstPage.hasMore `shouldBe` True
      firstPageToken <- case firstPage.nextPageToken of
        Just token -> pure token
        Nothing -> expectationFailure "nonterminal snapshot page omitted continuation token" >> fail "unreachable"

      mismatchResponse <- apiRequest ctx.deployedApplication methodPost "/api/v1/change-stream/resync" headers
        (encode (object
          [ "scope" .= scopeValue
          , "page_token" .= firstPageToken
          , "page_size" .= (2 :: Int)
          ]))
      responseStatus mismatchResponse `shouldBe` HTTP.status409
      mismatchBody <- decodeResponse mismatchResponse :: IO Value
      jsonField "error" mismatchBody `shouldBe` Just (String "resync_required")
      let streamConfig = ctx.deployedConfig.changeStream
          audience = AuthenticatedAudience (UUID.toText userId) userId
      readSnapshotPageWithTtls ctx.deployedEnv.pool
          (fromIntegral streamConfig.snapshotSessionTtlSeconds)
          (fromIntegral streamConfig.resumeTokenTtlSeconds)
          (WorkspaceScope workspace.id) audience (SnapshotToken firstPageToken) 2
        `shouldReturn` Left SnapshotOutOfOrder

      concurrent <- Project.createProject ctx.deployedEnv.pool CreateProject
        { workspaceId = workspace.id, parentId = Nothing, name = "during snapshot"
        , description = Nothing, priority = Nothing, metadata = Nothing }
      (snapshotItems, resumeToken) <- collectSnapshotPages
        ctx.deployedApplication headers scopeValue firstPage
      let projectIds =
            [ projectId
            | item <- snapshotItems
            , item.kind == "project"
            , Just (String projectId) <- [jsonField "id" item.data_]
            ]
      UUID.toText preSnapshot.id `shouldSatisfy` (`elem` projectIds)
      UUID.toText concurrent.id `shouldSatisfy` (`notElem` projectIds)

      currentResponse <- apiRequest ctx.deployedApplication methodGet
        ("/api/v1/projects/" <> Text.encodeUtf8 (UUID.toText concurrent.id)) headers ""
      responseStatus currentResponse `shouldBe` status200
      current <- decodeResponse currentResponse :: IO Project
      current.id `shouldBe` concurrent.id

      ticketResponse <- apiRequest ctx.deployedApplication methodPost "/api/v1/change-stream/ticket" headers
        (encode (object ["scope" .= scopeValue, "resume_token" .= resumeToken]))
      responseStatus ticketResponse `shouldBe` status200
      ticket <- decodeResponse ticketResponse :: IO WebSocketTicketResponse
      durableRecord <- latestOutbox ctx (WorkspaceScope workspace.id)
      replayed <- newEmptyMVar
      checkpointed <- newEmptyMVar
      withWebSocketApplication ctx.deployedWSState ctx.deployedApplication $ \port -> do
        client <- async $ (WS.runClient "127.0.0.1" port ("/api/v1/ws?ticket=" <> showTicket ticket.ticket) $ \conn -> do
          WS.receiveData conn >>= putMVar replayed
          WS.receiveData conn >>= putMVar checkpointed
          WS.sendClose conn ("done" :: Text)) `catch` \(_ :: WS.ConnectionException) -> pure ()
        replayFrame <- awaitFrame replayed
        frameType replayFrame `shouldBe` Just "change"
        let replayEvent = jsonField "event" replayFrame
        (replayEvent >>= jsonField "event_id") `shouldBe` Just (String $ UUID.toText durableRecord.outboxEventId)
        (replayEvent >>= jsonField "entity" >>= jsonField "id") `shouldBe` Just (String $ UUID.toText concurrent.id)
        (replayEvent >>= jsonField "transaction" >>= jsonField "cause") `shouldBe` Just (String "core")
        (replayEvent >>= jsonField "cursor") `shouldBe` Nothing
        frameType <$> awaitFrame checkpointed `shouldReturn` Just "checkpoint"
        wait client

  it "delivers only to the eligible workspace and global audiences without hidden cadence frames" $
    withDeployedSandboxAppContext $ \ctx -> do
      workspaceA <- createTestWorkspace ctx.deployedEnv "canonical-audience-a"
      workspaceB <- createTestWorkspace ctx.deployedEnv "canonical-audience-b"
      memberId <- createDeployedSandboxUser ctx.deployedEnv False False
      adminId <- createDeployedSandboxUser ctx.deployedEnv False False
      unrelatedId <- createDeployedSandboxUser ctx.deployedEnv False False
      superadminId <- createDeployedSandboxUser ctx.deployedEnv False True
      _ <- Auth.upsertWorkspaceMembership ctx.deployedEnv.pool workspaceA.id
        (Auth.UpsertWorkspaceMembership memberId Auth.WorkspaceRoleRead) Nothing
      _ <- Auth.upsertWorkspaceMembership ctx.deployedEnv.pool workspaceA.id
        (Auth.UpsertWorkspaceMembership adminId Auth.WorkspaceRoleAdmin) Nothing
      _ <- Auth.upsertWorkspaceMembership ctx.deployedEnv.pool workspaceB.id
        (Auth.UpsertWorkspaceMembership unrelatedId Auth.WorkspaceRoleRead) Nothing
      let workspaceAudience userId = AuthenticatedAudience (UUID.toText userId) userId
      beginResync ctx.deployedEnv.pool 60 GlobalScope (workspaceAudience memberId) (pure [])
        `shouldReturn` Left ResyncUnauthorized
      memberTicket <- canonicalTicketFor ctx (grantPrincipal memberId) (WorkspaceScope workspaceA.id) (workspaceAudience memberId)
      adminTicket <- canonicalTicketFor ctx (grantPrincipal adminId) (WorkspaceScope workspaceA.id) (workspaceAudience adminId)
      unrelatedTicket <- canonicalTicketFor ctx (grantPrincipal unrelatedId) (WorkspaceScope workspaceB.id) (workspaceAudience unrelatedId)
      globalTicket <- canonicalTicketFor ctx (grantPrincipal superadminId) GlobalScope (workspaceAudience superadminId)
      withWebSocketApplication ctx.deployedWSState ctx.deployedApplication $ \port -> do
        memberClient <- startOneFrameClient port memberTicket.ticket
        adminClient <- startOneFrameClient port adminTicket.ticket
        unrelatedClient <- startOneFrameClient port unrelatedTicket.ticket
        mapM_ (awaitFrame . (.clientInitial)) [memberClient, adminClient, unrelatedClient]

        projectA <- Project.createProject ctx.deployedEnv.pool CreateProject
          { workspaceId = workspaceA.id, parentId = Nothing, name = "audience A"
          , description = Nothing, priority = Nothing, metadata = Nothing }
        recordA <- latestOutbox ctx (WorkspaceScope workspaceA.id)
        dispatchCanonicalOutbox ctx.deployedEnv.pool ctx.deployedWSState [recordA]
        memberFrame <- awaitFrame memberClient.clientObserved
        adminFrame <- awaitFrame adminClient.clientObserved
        mapM_ (assertChangeIdentity recordA projectA.id) [memberFrame, adminFrame]

        projectB <- Project.createProject ctx.deployedEnv.pool CreateProject
          { workspaceId = workspaceB.id, parentId = Nothing, name = "audience B"
          , description = Nothing, priority = Nothing, metadata = Nothing }
        recordB <- latestOutbox ctx (WorkspaceScope workspaceB.id)
        dispatchCanonicalOutbox ctx.deployedEnv.pool ctx.deployedWSState [recordB]
        unrelatedFrame <- awaitFrame unrelatedClient.clientObserved
        assertChangeIdentity recordB projectB.id unrelatedFrame

        -- Global sockets now intentionally receive authorized workspace-only
        -- activity wakeups. Establish this canonical global-event observer
        -- after those separately qualified cross-scope dispatches.
        globalClient <- startOneFrameClient port globalTicket.ticket
        frameType <$> awaitFrame globalClient.clientInitial `shouldReturn` Just "checkpoint"
        group <- WorkspaceGroup.createGroup ctx.deployedEnv.pool (CreateWorkspaceGroup "global audience" Nothing)
        globalRecord <- latestOutbox ctx GlobalScope
        dispatchCanonicalOutbox ctx.deployedEnv.pool ctx.deployedWSState [globalRecord]
        globalFrame <- awaitFrame globalClient.clientObserved
        frameType globalFrame `shouldBe` Just "change"
        (jsonField "event" globalFrame >>= jsonField "entity" >>= jsonField "type")
          `shouldBe` Just (String "workspace_group")
        (jsonField "event" globalFrame >>= jsonField "entity" >>= jsonField "id")
          `shouldBe` Just (String $ UUID.toText group.id)
        mapM_ (wait . (.clientWorker)) [memberClient, adminClient, unrelatedClient, globalClient]

  it "suppresses hidden-only membership activity without a frame or checkpoint" $
    withDeployedSandboxAppContext $ \ctx -> do
      workspace <- createTestWorkspace ctx.deployedEnv "canonical-hidden"
      subscriberId <- createDeployedSandboxUser ctx.deployedEnv False False
      hiddenUserId <- createDeployedSandboxUser ctx.deployedEnv False False
      _ <- Auth.upsertWorkspaceMembership ctx.deployedEnv.pool workspace.id
        (Auth.UpsertWorkspaceMembership subscriberId Auth.WorkspaceRoleRead) Nothing
      let principal = grantPrincipal subscriberId
          scope = WorkspaceScope workspace.id
          audience = AuthenticatedAudience (UUID.toText subscriberId) subscriberId
      resume <- terminalResume ctx scope audience
      issued <- createCanonicalTicket ctx.deployedWSState principal scope resume
      initial <- newEmptyMVar
      observed <- newEmptyMVar
      withWebSocketApplication ctx.deployedWSState ctx.deployedApplication $ \port -> do
        client <- async $ (WS.runClient "127.0.0.1" port ("/api/v1/ws?ticket=" <> showTicket issued.ticket) $ \conn -> do
          WS.receiveData conn >>= putMVar initial
          WS.receiveData conn >>= putMVar observed
          WS.sendClose conn ("done" :: Text)) `catch` \(_ :: WS.ConnectionException) -> pure ()
        initialFrame <- awaitFrame initial
        frameType initialFrame `shouldBe` Just "checkpoint"
        _ <- Auth.upsertWorkspaceMembership ctx.deployedEnv.pool workspace.id
          (Auth.UpsertWorkspaceMembership hiddenUserId Auth.WorkspaceRoleRead) Nothing
        record <- latestOutbox ctx scope
        dispatchCanonicalOutbox ctx.deployedEnv.pool ctx.deployedWSState [record]
        -- The next observable frame is the later visible mutation.  A frame
        -- from the hidden membership (including a checkpoint) would arrive
        -- first and fail this assertion.
        _ <- Project.createProject ctx.deployedEnv.pool CreateProject
          { workspaceId = workspace.id, parentId = Nothing, name = "visible-after-hidden"
          , description = Nothing, priority = Nothing, metadata = Nothing }
        visibleRecord <- latestOutbox ctx scope
        dispatchCanonicalOutbox ctx.deployedEnv.pool ctx.deployedWSState [visibleRecord]
        visibleFrame <- awaitFrame observed
        frameType visibleFrame `shouldBe` Just "change"
        wait client

  it "sends a targeted revoke and closes the canonical workspace scope" $
    withDeployedSandboxAppContext $ \ctx -> do
      workspace <- createTestWorkspace ctx.deployedEnv "canonical-revoke"
      userId <- createDeployedSandboxUser ctx.deployedEnv False False
      _ <- Auth.upsertWorkspaceMembership ctx.deployedEnv.pool workspace.id
        (Auth.UpsertWorkspaceMembership userId Auth.WorkspaceRoleRead) Nothing
      let principal = grantPrincipal userId
          scope = WorkspaceScope workspace.id
          audience = AuthenticatedAudience (UUID.toText userId) userId
      resume <- terminalResume ctx scope audience
      issued <- createCanonicalTicket ctx.deployedWSState principal scope resume
      initial <- newEmptyMVar
      permitRead <- newEmptyMVar
      revoked <- newEmptyMVar
      withWebSocketApplication ctx.deployedWSState ctx.deployedApplication $ \port -> do
        client <- async $ (WS.runClient "127.0.0.1" port ("/api/v1/ws?ticket=" <> showTicket issued.ticket) $ \conn -> do
          WS.receiveData conn >>= putMVar initial
          takeMVar permitRead
          WS.receiveData conn >>= putMVar revoked
          _ <- (WS.receiveData conn :: IO Text) `catch` \(_ :: WS.ConnectionException) -> pure ""
          pure ()) `catch` \(_ :: WS.ConnectionException) -> pure ()
        _ <- awaitFrame initial
        Auth.deleteWorkspaceMembership ctx.deployedEnv.pool workspace.id userId `shouldReturn` True
        record <- latestOutbox ctx scope
        dispatchCanonicalOutbox ctx.deployedEnv.pool ctx.deployedWSState [record]
        putMVar permitRead ()
        revokeFrame <- awaitFrame revoked
        frameType revokeFrame `shouldBe` Just "access_revoked"
        wait client

  it "expires a connected canonical bearer on its fixed deadline despite hidden activity" $
    withDeployedSandboxAppContext $ \ctx -> do
      workspace <- createTestWorkspace ctx.deployedEnv "canonical-fixed-expiry"
      subscriberId <- createDeployedSandboxUser ctx.deployedEnv False False
      hiddenUserId <- createDeployedSandboxUser ctx.deployedEnv False False
      _ <- Auth.upsertWorkspaceMembership ctx.deployedEnv.pool workspace.id
        (Auth.UpsertWorkspaceMembership subscriberId Auth.WorkspaceRoleRead) Nothing
      let principal = grantPrincipal subscriberId
          scope = WorkspaceScope workspace.id
          audience = AuthenticatedAudience (UUID.toText subscriberId) subscriberId
      withWebSocketApplication ctx.deployedWSState ctx.deployedApplication $ \port -> do
        resume <- terminalResume ctx scope audience
        issued <- createCanonicalTicketWithTtl ctx.deployedWSState 0.3 principal scope resume
        bracket (startOneFrameClient port issued.ticket) (\client -> joinClient "expiry client" client.clientWorker) $ \client -> do
          frameType <$> awaitFrame client.clientInitial `shouldReturn` Just "checkpoint"
          _ <- Auth.upsertWorkspaceMembership ctx.deployedEnv.pool workspace.id
            (Auth.UpsertWorkspaceMembership hiddenUserId Auth.WorkspaceRoleRead) Nothing
          hiddenRecord <- latestOutbox ctx scope
          dispatchCanonicalOutbox ctx.deployedEnv.pool ctx.deployedWSState [hiddenRecord]
          frameType <$> awaitFrame client.clientObserved `shouldReturn` Just "resync_required"
          getCurrentTime >>= (`shouldSatisfy` (>= issued.expiresAt))
          timeout 2000000 (wait client.clientWorker) `shouldReturn` Just ()

  it "keeps an aged durable reconnect on its actual expiry despite hidden activity" $
    withDeployedSandboxAppContext $ \ctx -> do
      workspace <- createTestWorkspace ctx.deployedEnv "canonical-aged-reconnect-expiry"
      subscriberId <- createDeployedSandboxUser ctx.deployedEnv False False
      hiddenUserId <- createDeployedSandboxUser ctx.deployedEnv False False
      _ <- Auth.upsertWorkspaceMembership ctx.deployedEnv.pool workspace.id
        (Auth.UpsertWorkspaceMembership subscriberId Auth.WorkspaceRoleRead) Nothing
      let principal = grantPrincipal subscriberId
          scope = WorkspaceScope workspace.id
          audience = AuthenticatedAudience (UUID.toText subscriberId) subscriberId
      withWebSocketApplication ctx.deployedWSState ctx.deployedApplication $ \port -> do
        -- Start listening before the original durable lifetime begins. Real
        -- validation ages the bearer; ticket exchange preserves its expiry.
        resume <- terminalResumeWithTtl ctx 2.0 scope audience
        validated <- validateCanonicalResumeToken ctx.deployedEnv.pool scope audience resume
        expiresAt <- either (fail . show) pure validated
        issued <- createCanonicalTicketWithExpiry ctx.deployedWSState expiresAt principal scope resume
        issued.expiresAt `shouldBe` expiresAt
        bracket (startOneFrameClient port issued.ticket)
          (\client -> joinClient "aged expiry client" client.clientWorker) $ \client -> do
            frameType <$> awaitFrame client.clientInitial `shouldReturn` Just "checkpoint"
            _ <- Auth.upsertWorkspaceMembership ctx.deployedEnv.pool workspace.id
              (Auth.UpsertWorkspaceMembership hiddenUserId Auth.WorkspaceRoleRead) Nothing
            hiddenRecord <- latestOutbox ctx scope
            dispatchCanonicalOutbox ctx.deployedEnv.pool ctx.deployedWSState [hiddenRecord]
            frameType <$> awaitFrame client.clientObserved `shouldReturn` Just "resync_required"
            getCurrentTime >>= (`shouldSatisfy` (>= expiresAt))
            timeout 2000000 (wait client.clientWorker) `shouldReturn` Just ()

  it "announces a targeted grant for a separately scoped workspace" $
    withDeployedSandboxAppContext $ \ctx -> do
      currentWorkspace <- createTestWorkspace ctx.deployedEnv "canonical-grant-current"
      grantedWorkspace <- createTestWorkspace ctx.deployedEnv "canonical-grant-target"
      userId <- createDeployedSandboxUser ctx.deployedEnv False False
      _ <- Auth.upsertWorkspaceMembership ctx.deployedEnv.pool currentWorkspace.id
        (Auth.UpsertWorkspaceMembership userId Auth.WorkspaceRoleRead) Nothing
      let principal = grantPrincipal userId
          scope = WorkspaceScope currentWorkspace.id
          audience = AuthenticatedAudience (UUID.toText userId) userId
      resume <- terminalResume ctx scope audience
      issued <- createCanonicalTicket ctx.deployedWSState principal scope resume
      initial <- newEmptyMVar
      permitGrant <- newEmptyMVar
      granted <- newEmptyMVar
      withWebSocketApplication ctx.deployedWSState ctx.deployedApplication $ \port -> do
        client <- async $ (WS.runClient "127.0.0.1" port ("/api/v1/ws?ticket=" <> showTicket issued.ticket) $ \conn -> do
          WS.receiveData conn >>= putMVar initial
          takeMVar permitGrant
          WS.receiveData conn >>= putMVar granted
          WS.sendClose conn ("done" :: Text)) `catch` \(_ :: WS.ConnectionException) -> pure ()
        _ <- awaitFrame initial
        _ <- Auth.upsertWorkspaceMembership ctx.deployedEnv.pool grantedWorkspace.id
          (Auth.UpsertWorkspaceMembership userId Auth.WorkspaceRoleRead) Nothing
        grantRecord <- latestOutbox ctx (WorkspaceScope grantedWorkspace.id)
        dispatchCanonicalOutbox ctx.deployedEnv.pool ctx.deployedWSState [grantRecord]
        putMVar permitGrant ()
        grantFrame <- awaitFrame granted
        frameType grantFrame `shouldBe` Just "access_granted"
        jsonField "workspace_id" grantFrame `shouldBe` Just (String $ UUID.toText grantedWorkspace.id)
        wait client

  it "sends a cross-scope revoke while the retained scope remains live" $
    withDeployedSandboxAppContext $ \ctx -> do
      revokedWorkspace <- createTestWorkspace ctx.deployedEnv "canonical-cross-scope-revoke-target"
      retainedWorkspace <- createTestWorkspace ctx.deployedEnv "canonical-cross-scope-revoke-retained"
      userId <- createDeployedSandboxUser ctx.deployedEnv False False
      _ <- Auth.upsertWorkspaceMembership ctx.deployedEnv.pool revokedWorkspace.id
        (Auth.UpsertWorkspaceMembership userId Auth.WorkspaceRoleRead) Nothing
      _ <- Auth.upsertWorkspaceMembership ctx.deployedEnv.pool retainedWorkspace.id
        (Auth.UpsertWorkspaceMembership userId Auth.WorkspaceRoleRead) Nothing
      let principal = grantPrincipal userId
          revokedScope = WorkspaceScope revokedWorkspace.id
          retainedScope = WorkspaceScope retainedWorkspace.id
          audience = AuthenticatedAudience (UUID.toText userId) userId
      retainedResume <- terminalResume ctx retainedScope audience
      retainedTicket <- createCanonicalTicket ctx.deployedWSState principal retainedScope retainedResume
      retainedInitial <- newEmptyMVar
      retainedRevoke <- newEmptyMVar
      retainedLive <- newEmptyMVar
      withWebSocketApplication ctx.deployedWSState ctx.deployedApplication $ \port -> do
        retainedClient <- async $ (WS.runClient "127.0.0.1" port ("/api/v1/ws?ticket=" <> showTicket retainedTicket.ticket) $ \conn -> do
          WS.receiveData conn >>= putMVar retainedInitial
          WS.receiveData conn >>= putMVar retainedRevoke
          WS.receiveData conn >>= putMVar retainedLive
          WS.sendClose conn ("done" :: Text)) `catch` \(_ :: SomeException) -> pure ()
        _ <- awaitFrame retainedInitial
        Auth.deleteWorkspaceMembership ctx.deployedEnv.pool revokedWorkspace.id userId `shouldReturn` True
        revokeRecord <- latestOutbox ctx revokedScope
        dispatchCanonicalOutbox ctx.deployedEnv.pool ctx.deployedWSState [revokeRecord]
        retainedControl <- awaitFrame retainedRevoke
        frameType retainedControl `shouldBe` Just "access_revoked"
        jsonField "workspace_id" retainedControl `shouldBe` Just (String $ UUID.toText revokedWorkspace.id)
        _ <- Project.createProject ctx.deployedEnv.pool CreateProject
          { workspaceId = retainedWorkspace.id, parentId = Nothing, name = "other-scope-remains-live"
          , description = Nothing, priority = Nothing, metadata = Nothing }
        retainedRecord <- latestOutbox ctx retainedScope
        dispatchCanonicalOutbox ctx.deployedEnv.pool ctx.deployedWSState [retainedRecord]
        frameType <$> awaitFrame retainedLive `shouldReturn` Just "change"
        wait retainedClient

  it "consumes each canonical ticket once and reconnects from its replacement token" $
    withDeployedSandboxAppContext $ \ctx -> do
      workspace <- createTestWorkspace ctx.deployedEnv "canonical-reconnect"
      userId <- createDeployedSandboxUser ctx.deployedEnv False False
      _ <- Auth.upsertWorkspaceMembership ctx.deployedEnv.pool workspace.id
        (Auth.UpsertWorkspaceMembership userId Auth.WorkspaceRoleRead) Nothing
      let principal = grantPrincipal userId
          scope = WorkspaceScope workspace.id
          audience = AuthenticatedAudience (UUID.toText userId) userId
      resume <- terminalResume ctx scope audience
      firstTicket <- createCanonicalTicket ctx.deployedWSState principal scope resume
      firstFrame <- newEmptyMVar
      withWebSocketApplication ctx.deployedWSState ctx.deployedApplication $ \port -> do
        first <- async $ (WS.runClient "127.0.0.1" port ("/api/v1/ws?ticket=" <> showTicket firstTicket.ticket) $ \conn -> do
          WS.receiveData conn >>= putMVar firstFrame
          WS.sendClose conn ("done" :: Text)) `catch` \(_ :: WS.ConnectionException) -> pure ()
        checkpoint <- awaitFrame firstFrame
        frameType checkpoint `shouldBe` Just "checkpoint"
        replacement <- checkpointResume checkpoint
        wait first
        -- The old ticket was consumed by the successful upgrade and cannot
        -- open a second delivery identity.
        admitted <- newEmptyMVar
        secondAttempt <- timeout 2000000 $
          (WS.runClient "127.0.0.1" port ("/api/v1/ws?ticket=" <> showTicket firstTicket.ticket) (\_ -> putMVar admitted ())
            `catch` \(_ :: SomeException) -> pure ())
        secondAttempt `shouldSatisfy` isJust
        tryTakeMVar admitted `shouldReturn` Nothing
        _ <- Project.createProject ctx.deployedEnv.pool CreateProject
          { workspaceId = workspace.id, parentId = Nothing, name = "after-reconnect"
          , description = Nothing, priority = Nothing, metadata = Nothing }
        reconnectTicket <- createCanonicalTicket ctx.deployedWSState principal scope replacement
        change <- newEmptyMVar
        terminal <- newEmptyMVar
        reconnect <- async $ (WS.runClient "127.0.0.1" port ("/api/v1/ws?ticket=" <> showTicket reconnectTicket.ticket) $ \conn -> do
          WS.receiveData conn >>= putMVar change
          WS.receiveData conn >>= putMVar terminal
          WS.sendClose conn ("done" :: Text)) `catch` \(_ :: WS.ConnectionException) -> pure ()
        changeFrame <- awaitFrame change
        frameType changeFrame `shouldBe` Just "change"
        (jsonField "event" changeFrame >>= jsonField "event_id") `shouldSatisfy` maybe False isText
        terminalFrame <- awaitFrame terminal
        frameType terminalFrame `shouldBe` Just "checkpoint"
        wait reconnect

  it "reconnects from the original bearer after one page and checkpoints the frozen boundary once" $
    withDeployedSandboxAppContext $ \ctx -> do
      workspace <- createTestWorkspace ctx.deployedEnv "canonical-frozen-replay"
      owner <- UUIDV4.nextRandom
      finished <- newEmptyMVar
      let pool = ctx.deployedEnv.pool
          st = ctx.deployedWSState
          scope = WorkspaceScope workspace.id
          principal = (grantPrincipal owner) { actorId = "frozen-replay", authority = PrincipalSyntheticLocalSuperadmin }
          audience = TrustedAudience "local:frozen-replay"
          application = observeRawWebSocket ctx.deployedApplication id (void (tryPutMVar finished ()))
          dispatch records = declaredPhase "frozen replay dispatch" (dispatchCanonicalOutbox pool st records)
          awaitFrameFrom frames = readChan frames >>= decodeFrame
          receiveChanges phase frames count = declaredPhase phase $ replicateM count $ do
            frame <- awaitFrameFrom frames
            frameType frame `shouldBe` Just "change"
            pure (jsonField "event" frame >>= jsonField "event_id")
          eventIds = map (Just . String . UUID.toText . (.outboxEventId))
      original <- terminalResume ctx scope audience
      expires <- validateCanonicalResumeToken pool scope audience original >>= either (fail . show) pure
      let deadline = addUTCTime (-5) expires
      seedCanonicalReplay ctx.deployedEnv workspace.id
      backlog <- listOutboxAfter pool scope 0 1000
      length backlog `shouldBe` 525
      frozenHighWater <- maybe (fail "expected replay backlog") (pure . (.outboxCursor)) (safeLast backlog)
      firstTicket <- createCanonicalTicketWithExpiry st deadline principal scope original
      withWebSocketApplication st application $ \port -> do
        withActivityFrames port firstTicket.ticket $ \frames -> do
          receiveChanges "first 500 replay records" frames 500 `shouldReturn` eventIds (take 500 backlog)
          canonicalReplayPending st `shouldReturn` True
          validateCanonicalResumeToken pool scope audience original `shouldReturn` Right expires
        declaredPhase "first replay connection retired" (takeMVar finished)
        -- The peer left during a pending catch-up without a server decision.
        disconnected <- readWebSocketTelemetry st
        map (outcomeCount disconnected)
          [("websocket_replay", "initial", "disconnected"), ("websocket_replay", "initial", "completed")]
          `shouldBe` [1, 0]
        reconnect <- createCanonicalTicketWithExpiry st deadline principal scope original
        withActivityFrames port reconnect.ticket $ \frames -> do
          receiveChanges "duplicates after reconnect" frames 500 `shouldReturn` eventIds (take 500 backlog)
          marker <- Project.createProject pool (CreateProject workspace.id Nothing "post-freeze marker" Nothing Nothing Nothing)
          laterRecords <- listOutboxAfter pool scope frozenHighWater 100
          markerRecord <- case laterRecords of
            [record] -> pure record
            _ -> fail "expected one post-freeze marker"
          dispatch [markerRecord]
          receiveChanges "rest of the frozen page" frames 25 `shouldReturn` eventIds (drop 500 backlog)
          checkpoint <- declaredPhase "frozen checkpoint" (awaitFrameFrom frames)
          frameType checkpoint `shouldBe` Just "checkpoint"
          next <- checkpointResume checkpoint
          next `shouldBe` replacementResumeToken scope audience original frozenHighWater
          validateCanonicalResumeToken pool scope audience next `shouldReturn` Right deadline
          validateCanonicalResumeToken pool scope audience original `shouldReturn` Left ResumeSuperseded
          canonicalReplayPending st `shouldReturn` True
          dispatch []
          declaredPhase "post-freeze change" (awaitFrameFrom frames) >>= assertChangeIdentity markerRecord marker.id
          laterCheckpoint <- declaredPhase "post-freeze checkpoint" (awaitFrameFrom frames)
          frameType laterCheckpoint `shouldBe` Just "checkpoint"
          later <- checkpointResume laterCheckpoint
          later `shouldBe` replacementResumeToken scope audience next markerRecord.outboxCursor
          validateCanonicalResumeToken pool scope audience later `shouldReturn` Right deadline
          canonicalReplayPending st `shouldReturn` False
          replayed <- awaitTelemetry st "frozen replay telemetry" $ \current ->
            outcomeCount current ("websocket_replay", "live", "completed") == 1
          outcomeCount replayed ("websocket_replay", "initial", "completed") `shouldBe` 1

  it "rechecks authority before continuing a partially delivered replay" $
    withDeployedSandboxAppContext $ \ctx -> do
      workspace <- createTestWorkspace ctx.deployedEnv "canonical-frozen-replay-revoke"
      user <- createDeployedSandboxUser ctx.deployedEnv False False
      let pool = ctx.deployedEnv.pool
          st = ctx.deployedWSState
          scope = WorkspaceScope workspace.id
          principal = grantPrincipal user
          audience = AuthenticatedAudience (UUID.toText user) user
      _ <- Auth.upsertWorkspaceMembership pool workspace.id (Auth.UpsertWorkspaceMembership user Auth.WorkspaceRoleRead) Nothing
      resume <- terminalResume ctx scope audience
      seedCanonicalReplay ctx.deployedEnv workspace.id
      issued <- createCanonicalTicket st principal scope resume
      withWebSocketApplication st ctx.deployedApplication $ \port ->
        withActivityFrames port issued.ticket $ \frames -> do
          frameType <$> declaredPhase "first record before revocation" (readChan frames >>= decodeFrame)
            `shouldReturn` Just "change"
          canonicalReplayPending st `shouldReturn` True
          Auth.deleteWorkspaceMembership pool workspace.id user `shouldReturn` True
          declaredPhase "reauthorization between bounded replay turns" (dispatchCanonicalOutbox pool st [])
          -- A record authorized before the membership commit may already be
          -- buffered. No checkpoint may certify this unfinished replay.
          let awaitRevocation count = do
                frame <- readChan frames >>= decodeFrame
                case frameType frame of
                  Just "change" -> awaitRevocation (count + 1)
                  kind -> (kind `shouldBe` Just "access_revoked") >> pure count
          delivered <- declaredPhase "terminal revocation" (awaitRevocation (1 :: Int))
          delivered `shouldSatisfy` (< 525)
          canonicalReplayPending st `shouldReturn` False
          revoked <- readWebSocketTelemetry st
          map (outcomeCount revoked)
            [("websocket_replay", "initial", "revoked"), ("websocket_replay", "initial", "completed")]
            `shouldBe` [1, 0]

  it "leaves the original bearer usable when the final checkpoint write fails" $
    withDeployedSandboxAppContext $ \ctx -> do
      workspace <- createTestWorkspace ctx.deployedEnv "canonical-final-checkpoint-failure"
      owner <- UUIDV4.nextRandom
      entered <- newEmptyMVar
      release <- newEmptyMVar
      first <- newEmptyMVar
      ended <- newEmptyMVar
      let pool = ctx.deployedEnv.pool
          st = ctx.deployedWSState
          scope = WorkspaceScope workspace.id
          principal = (grantPrincipal owner) { actorId = "checkpoint-failure", authority = PrincipalSyntheticLocalSuperadmin }
          audience = TrustedAudience "local:checkpoint-failure"
          write send bytes
            | "\"checkpoint\"" `BS.isInfixOf` bytes = do
                putMVar entered ()
                takeMVar release
                ioError (userError "injected final checkpoint write failure")
            | otherwise = send bytes
          application = observeRawWebSocket ctx.deployedApplication write (pure ())
      resume <- terminalResume ctx scope audience
      void $ Project.createProject pool (CreateProject workspace.id Nothing "before failed checkpoint" Nothing Nothing Nothing)
      record <- latestOutbox ctx scope
      issued <- createCanonicalTicket st principal scope resume
      withWebSocketApplication st application $ \port -> do
        let client = WS.runClient "127.0.0.1" port ("/api/v1/ws?ticket=" <> showTicket issued.ticket) $ \conn -> do
              WS.receiveData conn >>= putMVar first
              result <- try @WS.ConnectionException (WS.receiveData conn :: IO Text)
              putMVar ended (either (const True) (const False) result)
        withAsync client $ \worker ->
          (do
            frameType <$> awaitFrame first `shouldReturn` Just "change"
            declaredPhase "final checkpoint write entered" (takeMVar entered)
            validateCanonicalResumeToken pool scope audience resume >>= (\result -> result `shouldSatisfy` either (const False) (const True))
            validateCanonicalResumeToken pool scope audience (replacementResumeToken scope audience resume record.outboxCursor)
              `shouldReturn` Left ResumeNotFound
            putMVar release ()
            declaredPhase "failed checkpoint socket retired" (takeMVar ended) `shouldReturn` True
            declaredPhase "failed checkpoint client joined" (wait worker)
            validateCanonicalResumeToken pool scope audience resume >>= (\result -> result `shouldSatisfy` either (const False) (const True)))
          `finally` void (tryPutMVar release ())

  it "replays retained commits after dispatcher restart and cancels workers cleanly" $
    withDeployedSandboxAppContext $ \ctx -> do
      workspace <- createTestWorkspace ctx.deployedEnv "canonical-worker-restart"
      userId <- createDeployedSandboxUser ctx.deployedEnv False False
      _ <- Auth.upsertWorkspaceMembership ctx.deployedEnv.pool workspace.id
        (Auth.UpsertWorkspaceMembership userId Auth.WorkspaceRoleRead) Nothing
      let principal = grantPrincipal userId
          scope = WorkspaceScope workspace.id
          audience = AuthenticatedAudience (UUID.toText userId) userId
      resume <- terminalResume ctx scope audience
      let pool = ctx.deployedEnv.pool
          st = ctx.deployedWSState
          receive frames = declaredPhase "dispatcher frame" (readChan frames >>= decodeFrame)
      ticket <- createCanonicalTicket st principal scope resume
      withWebSocketApplication st ctx.deployedApplication $ \port -> do
        settled <- withDispatcherFixture ctx "first dispatcher" $ \fixture ->
          withActivityFrames port ticket.ticket $ \frames -> do
            frameType <$> receive frames `shouldReturn` Just "checkpoint"
            beforeRestart <- Project.createProject pool CreateProject
              { workspaceId = workspace.id, parentId = Nothing, name = "before-restart"
              , description = Nothing, priority = Nothing, metadata = Nothing }
            awaitChangeChan frames beforeRestart.id
            checkpoint <- receive frames
            frameType checkpoint `shouldBe` Just "checkpoint"
            token <- checkpointResume checkpoint
            let awaitAck = validateCanonicalResumeToken pool scope audience resume >>= \case
                  Left ResumeSuperseded -> pure ()
                  _ -> threadDelay 1000 >> awaitAck
            declaredPhase "durable checkpoint before restart" awaitAck
            -- Cancellation of an active delivery owns native retirement. The
            -- reconnect contract is the durable checkpoint, not socket survival.
            stopDispatcherFixture ctx "first explicit stop" fixture
            pure token
        duringRestart <- Project.createProject pool CreateProject
          { workspaceId = workspace.id, parentId = Nothing, name = "during-restart"
          , description = Nothing, priority = Nothing, metadata = Nothing }
        reconnect <- createCanonicalTicket st principal scope settled
        withDispatcherFixture ctx "replacement dispatcher" $ \_ ->
          withActivityFrames port reconnect.ticket $ \frames -> do
            awaitChangeChan frames duringRestart.id
            frameType <$> receive frames `shouldReturn` Just "checkpoint"

      withDispatcherFixture ctx "fresh dispatcher" $ \fixture ->
        stopDispatcherFixture ctx "fresh explicit stop" fixture

-- Joined cleanup is bounded even when a fixture assertion throws.
newtype DispatcherFixture = DispatcherFixture ChangeStreamWorker

withDispatcherFixture :: DeployedSandboxApp -> String -> (DispatcherFixture -> IO value) -> IO value
withDispatcherFixture ctx phase = bracket acquire (stopDispatcherFixture ctx (phase <> " cleanup"))
  where
    acquire = DispatcherFixture <$> startChangeStreamWorker ctx.deployedEnv.pool testChangeStreamConfig ctx.deployedWSState

stopDispatcherFixture :: DeployedSandboxApp -> String -> DispatcherFixture -> IO ()
stopDispatcherFixture _ctx phase (DispatcherFixture worker) = do
  stopped <- timeout 2000000 (stopChangeStreamWorker worker)
  case stopped of
    Just () -> pure ()
    Nothing -> expectationFailure (phase <> " did not join within two seconds")

joinClient :: String -> Async value -> IO ()
joinClient phase client = do
  joined <- timeout 2000000 (cancel client)
  case joined of
    Just () -> pure ()
    Nothing -> expectationFailure (phase <> " did not join within two seconds")

grantPrincipal :: UUID.UUID -> Principal
grantPrincipal userId = Principal
  { actorType = ActorUser, actorId = UUID.toText userId, actorLabel = "loopback user"
  , authority = PrincipalGrantUser userId }

terminalResume :: DeployedSandboxApp -> ChangeScope -> ChangeAudience -> IO ResumeToken
terminalResume ctx scope audience = do
  begun <- beginResync ctx.deployedEnv.pool 60 scope audience (pure [])
  begin <- either (fail . show) pure begun
  terminal <- readSnapshotPageWithTtl ctx.deployedEnv.pool 60 scope audience begin.snapshotToken 20
  resume <- case terminal of
    Right SnapshotPage { snapshotResumeToken = Just token } -> pure token
    other -> fail (show other)
  replayed <- replayAndRotateResumeToken ctx.deployedEnv.pool 60 scope audience resume 20
  case replayed of
    Right ReplayPage { replayPageResumeToken = token } -> pure token
    other -> fail (show other)

terminalResumeWithTtl :: DeployedSandboxApp -> Double -> ChangeScope -> ChangeAudience -> IO ResumeToken
terminalResumeWithTtl ctx ttl scope audience = do
  begun <- beginResync ctx.deployedEnv.pool 60 scope audience (pure [])
  begin <- either (fail . show) pure begun
  terminal <- readSnapshotPageWithTtls ctx.deployedEnv.pool 60 (realToFrac ttl) scope audience begin.snapshotToken 20
  case terminal of
    Right SnapshotPage { snapshotResumeToken = Just token } -> pure token
    other -> fail (show other)

terminalLocalResume :: LocalSandboxApp -> ChangeScope -> ChangeAudience -> IO ResumeToken
terminalLocalResume ctx scope audience = do
  begun <- beginResync ctx.localEnv.pool 60 scope audience (pure [])
  begin <- either (fail . show) pure begun
  terminal <- readSnapshotPageWithTtl ctx.localEnv.pool 60 scope audience begin.snapshotToken 20
  case terminal of
    Right SnapshotPage { snapshotResumeToken = Just token } -> pure token
    other -> fail (show other)

latestOutbox :: DeployedSandboxApp -> ChangeScope -> IO OutboxRecord
latestOutbox ctx scope = do
  records <- listOutboxAfter ctx.deployedEnv.pool scope 0 100
  case reverse records of
    record:_ -> pure record
    [] -> fail "expected a committed outbox record"

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast xs = Just (last xs)

scopeLockWaitersStatement :: Statement.Statement () Int64
scopeLockWaitersStatement = Statement.Statement
  "SELECT count(*) FROM pg_stat_activity WHERE wait_event_type = 'Lock' AND query LIKE '%hmem_change_stream_lock%'"
  Enc.noParams (Dec.singleRow (Dec.column (Dec.nonNullable Dec.int8))) True

rollbackRenameStatement :: Statement.Statement UUID.UUID ()
rollbackRenameStatement = Statement.Statement
  "UPDATE workspaces SET name = 'rolled-back-workspace-rename' WHERE id = $1"
  (Enc.param (Enc.nonNullable Enc.uuid)) Dec.noResult True

canonicalTicketFor :: DeployedSandboxApp -> Principal -> ChangeScope -> ChangeAudience -> IO WebSocketTicketResponse
canonicalTicketFor ctx principal scope audience = do
  resume <- terminalResume ctx scope audience
  createCanonicalTicket ctx.deployedWSState principal scope resume

seedCanonicalReplay :: TestEnv -> UUID.UUID -> IO ()
seedCanonicalReplay env workspace = seedReplayRows env workspace "paged-replay" 525

seedReplayRows :: TestEnv -> UUID.UUID -> Text -> Int -> IO ()
seedReplayRows env workspace prefix count =
  -- Keep ordinary fixture setup within the existing 30-second statement
  -- budget. Every project still commits its real core outbox trigger event.
  forM_ [1,501..count] $ \first -> runSession env.pool $ Session.sql
    ("INSERT INTO projects(workspace_id,name) SELECT '" <> Text.encodeUtf8 (UUID.toText workspace)
      <> "'::uuid, '" <> Text.encodeUtf8 prefix <> "-' || n FROM generate_series("
      <> Text.encodeUtf8 (T.pack (show first)) <> "," <> Text.encodeUtf8 (T.pack (show (min count (first + 499)))) <> ") n")

observeRawWebSocket :: Application -> ((BS.ByteString -> IO ()) -> BS.ByteString -> IO ()) -> IO () -> Application
observeRawWebSocket application write finished request respond =
  application request $ \case
    ResponseRaw serve fallback ->
      respond $ ResponseRaw (\source send -> serve source (write send) `finally` finished) fallback
    response -> respond response

assertBlockedNativeWrite
  :: (WSState -> Application -> (Int -> IO ()) -> IO ())
  -> (Int -> Text -> (WS.Connection -> IO ()) -> IO ())
  -> DeployedSandboxApp -> IO ()
assertBlockedNativeWrite withListener runPeer ctx = do
  workspace <- createTestWorkspace ctx.deployedEnv "native-write-backpressure"
  let scope = WorkspaceScope workspace.id
      principal name = Principal
        { actorType = ActorUser, actorId = name, actorLabel = name
        , authority = PrincipalSyntheticLocalSuperadmin }
  slow <- canonicalTicketFor ctx (principal "slow-native") scope (TrustedAudience "local:slow-native")
  healthy <- canonicalTicketFor ctx (principal "healthy-native") scope (TrustedAudience "local:healthy-native")
  armed <- newIORef False
  chunks <- newIORef (0 :: Int)
  writing <- newEmptyMVar
  writeEnded <- newEmptyMVar
  ownerEnded <- newEmptyMVar
  initial <- newEmptyMVar
  release <- newEmptyMVar
  rawTimes <- newIORef Nothing
  let chunk = BS.replicate 65536 0
      write send bytes = do
        enabled <- readIORef armed
        if not enabled then send bytes else
          (do
            rawStart <- getMonotonicTimeNSec
            atomicModifyIORef' rawTimes (const (Just (rawStart, rawStart), ()))
            putMVar writing ()
            replicateM_ 1024 $ do
              send chunk
              atomicModifyIORef' chunks (\count -> (count + 1, ())))
            `finally` (do
              rawEnd <- getMonotonicTimeNSec
              atomicModifyIORef' rawTimes (\value -> (fmap (\(begin, _) -> (begin, rawEnd)) value, ()))
              putMVar writeEnded ())
      application request respond
        | lookup "ticket" (Wai.queryString request) == Just (Just (Text.encodeUtf8 slow.ticket)) =
            observeRawWebSocket ctx.deployedApplication write (putMVar ownerEnded ()) request respond
        | otherwise = ctx.deployedApplication request respond
      slowPeer port = runPeer port slow.ticket $ \conn -> do
        WS.receiveData conn >>= putMVar initial
        readMVar release
  withListener ctx.deployedWSState application $ \port ->
    withAsync (slowPeer port) $ \slowWorker ->
      (withPeerFrames runPeer port healthy.ticket $ \frames -> do
        let next = declaredPhase "healthy native peer frame" (readChan frames >>= decodeFrame)
        frameType <$> awaitFrame initial `shouldReturn` Just "checkpoint"
        frameType <$> next `shouldReturn` Just "checkpoint"
        atomicModifyIORef' armed (const (True, ()))
        project <- Project.createProject ctx.deployedEnv.pool
          (CreateProject workspace.id Nothing "healthy during blocked write" Nothing Nothing Nothing)
        record <- latestOutbox ctx scope
        started <- getMonotonicTimeNSec
        declaredPhase "bounded native dispatch" $
          dispatchCanonicalOutbox ctx.deployedEnv.pool ctx.deployedWSState [record]
        ended <- getMonotonicTimeNSec
        declaredPhase "native raw write entered" (takeMVar writing)
        declaredPhase "native raw write ended" (takeMVar writeEnded)
        declaredPhase "native owner ended before peer release" (takeMVar ownerEnded)
        -- The parent's write deadline retired the blocked native write.
        telemetry <- readWebSocketTelemetry ctx.deployedWSState
        map (outcomeCount telemetry)
          [ ("websocket_turn", "", "retired_write_timeout"), ("websocket_write", "", "timed_out")
          , ("websocket_replay", "live", "retired"), ("websocket_turn", "", "retired_deadline") ]
          `shouldBe` [1, 1, 1, 0]
        fmap (.maxMs) (Map.lookup ("websocket_turn", "", "retired_write_timeout") telemetry.outcomes)
          `shouldSatisfy` maybe False (\ms -> ms >= 250 && ms < 750)
        count <- readIORef chunks
        count `shouldSatisfy` (> 0)
        count `shouldSatisfy` (< 1024)
        (ended - started) `shouldSatisfy` (< 1500000000)
        next >>= assertChangeIdentity record project.id
        received <- getMonotonicTimeNSec
        raw <- readIORef rawTimes
        qualificationMetric "native-blocked-peer" (object ["dispatch_ms" .= milliseconds started ended, "healthy_observed_ms" .= milliseconds started received, "raw_write_lifetime_ms" .= fmap (uncurry milliseconds) raw, "write_budget_ms" .= (250 :: Int), "work_budget_ms" .= (750 :: Int), "cleanup_budget_ms" .= (250 :: Int), "completed_64KiB_chunks_before_abort" .= count]))
        `finally` void (tryPutMVar release ())
      >> declaredPhase "nonreading peer closed" (wait slowWorker)

milliseconds :: Integral a => a -> a -> Double
milliseconds start end = fromIntegral (end - start) / 1000000

qualificationMetric :: String -> Value -> IO ()
qualificationMetric name value = do
  enabled <- lookupEnv "HMEM_TEST_SESSION_DIAGNOSTICS"
  when (enabled == Just "1") $ hPutStrLn stderr ("change_stream_qualification " <> T.unpack (Text.decodeUtf8 (LBS.toStrict (encode (object ["name" .= name, "observation" .= value])))))

qualificationSqlStats :: Pool Hasql.Connection -> IO Value
qualificationSqlStats pool = do
  enabled <- lookupEnv "HMEM_TEST_SESSION_DIAGNOSTICS"
  if enabled /= Just "1" then pure Null else do
    available <- runSession pool $ Session.statement () $ Statement.Statement "SELECT current_setting('shared_preload_libraries') LIKE '%pg_stat_statements%'" Enc.noParams (Dec.singleRow (Dec.column (Dec.nonNullable Dec.bool))) True
    if not available then do
      path <- lookupEnv "HMEM_TEST_SANDBOX_DB_LOG" >>= maybe (fail "missing owned PostgreSQL log") pure
      size <- getFileSize path
      size `shouldSatisfy` (<= 134217728)
      rows <- T.lines . Text.decodeUtf8 <$> BS.readFile path
      let statements = filter (\line -> "LOG:" `T.isInfixOf` line && ("statement:" `T.isInfixOf` line || "execute " `T.isInfixOf` line)) rows
          count predicate = length (filter predicate statements)
      pure $ object ["method" .= ("observed parameter-suppressed statement log" :: Text), "log_bytes" .= size,
        "scope_scan_calls" .= count (T.isInfixOf "SELECT scope,workspace_id FROM"),
        "authorization_calls" .= count (\line -> "SELECT" `T.isInfixOf` line && "workspace_memberships" `T.isInfixOf` line),
        "snapshot_insert_calls" .= count (T.isInfixOf "INSERT INTO change_stream_snapshot_items")]
    else do
      runSession pool $ Session.sql "CREATE EXTENSION IF NOT EXISTS pg_stat_statements"
      raw <- runSession pool $ Session.statement () $ Statement.Statement
        "SELECT json_build_object('scope_scan_calls',coalesce(sum(calls) FILTER (WHERE query LIKE 'SELECT scope,workspace_id FROM%'),0),'authorization_calls',coalesce(sum(calls) FILTER (WHERE query ILIKE '%workspace_memberships%' AND query ILIKE 'SELECT%'),0),'snapshot_insert_calls',coalesce(sum(calls) FILTER (WHERE query ILIKE 'INSERT INTO change_stream_snapshot_items%'),0),'snapshot_insert_exec_ms',coalesce(sum(total_exec_time) FILTER (WHERE query ILIKE 'INSERT INTO change_stream_snapshot_items%'),0))::text FROM pg_stat_statements WHERE dbid=(SELECT oid FROM pg_database WHERE datname=current_database())"
        Enc.noParams (Dec.singleRow (Dec.column (Dec.nonNullable Dec.text))) True
      maybe (fail "invalid qualification SQL counters") pure (decode (LBS.fromStrict (Text.encodeUtf8 raw)))

qualificationLockWait :: Statement.Statement () Bool
qualificationLockWait = Statement.Statement "SELECT EXISTS(SELECT 1 FROM pg_stat_activity WHERE application_name='change-stream-qualification-snapshot' AND wait_event_type='Lock')" Enc.noParams (Dec.singleRow (Dec.column (Dec.nonNullable Dec.bool))) True

runPlainPeer :: Int -> Text -> (WS.Connection -> IO ()) -> IO ()
runPlainPeer port ticket = WS.runClient "127.0.0.1" port ("/api/v1/ws?ticket=" <> showTicket ticket)

withHeldLegacyPeers :: Int -> Int -> IO () -> IO ()
withHeldLegacyPeers _ 0 use = use
withHeldLegacyPeers port count use = do
  ready <- newEmptyMVar
  release <- newEmptyMVar
  let peer = WS.runClient "127.0.0.1" port "/api/v1/ws" $ \conn -> do
        putMVar ready ()
        takeMVar release
        WS.sendClose conn ("capacity fixture complete" :: Text)
        void (try @WS.ConnectionException (WS.receiveData conn :: IO Text))
  withAsync peer $ \worker -> do
    declaredPhase "physical peer accepted" (takeMVar ready)
    withHeldLegacyPeers port (count - 1) use
      `finally` void (tryPutMVar release ())
    declaredPhase "physical peer joined" (wait worker)

-- Sends one upgrade request with the given path, protocol version and extra
-- header lines, returning the response headers.
rawUpgrade :: Int -> BS.ByteString -> BS.ByteString -> [BS.ByteString] -> IO BS.ByteString
rawUpgrade port path version extra =
  bracket (Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol) Socket.close $ \socket -> do
    Socket.connect socket (Socket.SockAddrInet (fromIntegral port) (Socket.tupleToHostAddress (127,0,0,1)))
    SocketBytes.sendAll socket $ BS.concat $
      [ "GET ", path, " HTTP/1.1\r\nHost: localhost\r\nUpgrade: websocket\r\nConnection: Upgrade\r\n"
      , "Sec-WebSocket-Version: ", version, "\r\nSec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==\r\n" ]
      <> map (<> "\r\n") extra <> ["\r\n"]
    let receive bytes
          | "\r\n\r\n" `BS.isInfixOf` bytes = pure bytes
          | BS.length bytes >= 8192 = fail "upgrade response header limit exceeded"
          | otherwise = do
              next <- SocketBytes.recv socket 4096
              if BS.null next then pure bytes else receive (bytes <> next)
    declaredPhase "raw upgrade response" (receive BS.empty)

awaitTelemetry :: WSState -> String -> (WebSocketTelemetrySnapshot -> Bool) -> IO WebSocketTelemetrySnapshot
awaitTelemetry st phase ready = declaredPhase phase loop
  where
    loop = do
      snapshot <- readWebSocketTelemetry st
      if ready snapshot then pure snapshot else threadDelay 10000 >> loop

outcomeCount :: WebSocketTelemetrySnapshot -> (Text, Text, Text) -> Int
outcomeCount snapshot key = maybe 0 (.count) (Map.lookup key snapshot.outcomes)

decodeLine :: BS.ByteString -> KeyMap.KeyMap Value
decodeLine line = case decode (LBS.fromStrict line) of
  Just (Object entry) -> entry
  _ -> error ("log line is not one JSON object: " <> show line)

textField :: Key.Key -> KeyMap.KeyMap Value -> Maybe Text
textField key entry = case KeyMap.lookup key entry of
  Just (String value) -> Just value
  _ -> Nothing

upgradeResponseHeaders :: Int -> IO BS.ByteString
upgradeResponseHeaders port =
  bracket (Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol) Socket.close $ \socket -> do
    Socket.connect socket (Socket.SockAddrInet (fromIntegral port) (Socket.tupleToHostAddress (127,0,0,1)))
    SocketBytes.sendAll socket
      "GET /api/v1/ws HTTP/1.1\r\nHost: localhost\r\nUpgrade: websocket\r\nConnection: Upgrade\r\nSec-WebSocket-Version: 13\r\nSec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==\r\n\r\n"
    let receive bytes
          | "\r\n\r\n" `BS.isInfixOf` bytes = pure bytes
          | BS.length bytes >= 8192 = fail "upgrade response header limit exceeded"
          | otherwise = do
              next <- SocketBytes.recv socket 4096
              if BS.null next then fail "upgrade response ended before its headers" else receive (bytes <> next)
    declaredPhase "physical rejection headers" (receive BS.empty)

withWebSocketApplication :: WSState -> Application -> (Int -> IO a) -> IO a
withWebSocketApplication state = withSocketApplication state True runPlainSocket (pure ())

-- Runs its step after the listener stops and before the native drain.
withWebSocketApplicationBeforeDrain :: WSState -> Application -> IO () -> (Int -> IO a) -> IO a
withWebSocketApplicationBeforeDrain state application beforeDrain =
  withSocketApplication state True runPlainSocket beforeDrain application

withTlsWebSocketApplication :: WSState -> Application -> (Int -> IO ()) -> IO ()
withTlsWebSocketApplication state = withSocketApplication state False (runTLSSocket (tlsSettingsMemory cert key)) (pure ())
  where
    cert = $(makeRelativeToProject "test/fixtures/change-stream/loopback-test-cert.pem" >>= embedFile)
    key = $(makeRelativeToProject "test/fixtures/change-stream/loopback-test-key.pem" >>= embedFile)

withSocketApplication
  :: WSState -> Bool -> (Settings -> Socket.Socket -> Application -> IO ()) -> IO () -> Application -> (Int -> IO a) -> IO a
withSocketApplication state forwardApplicationFailures runListener beforeDrain application use =
  bracket openFreePort (Socket.close . snd) $ \(port, listener) -> do
    testOwner <- myThreadId
    ready <- newEmptyMVar
    stopping <- newIORef False
    let reportFailure
          | forwardApplicationFailures = \_ failure -> do
              closing <- readIORef stopping
              when (not closing && defaultShouldDisplayException failure) (throwTo testOwner failure)
          | otherwise = settingsOnException defaultSettings
        settings = defaultSettings
          { settingsBeforeMainLoop = putMVar ready ()
          , settingsOnException = reportFailure
          }
        run = withNativeWebSocketSettings state settings $ \native -> runListener native listener application
    withAsync run $ \server -> mask $ \restore -> do
      primary <- try @SomeException $ restore (declaredPhase "loopback listener" (takeMVar ready) >> use port)
      cleanup <- try @SomeException $ do
        atomicModifyIORef' stopping (const (True, ()))
        Socket.close listener
        declaredPhase "loopback listener join" (wait server)
        beforeDrain
        declaredPhase "native worker drain" (awaitWebSocketDrain state 2000000)
      case primary of
        Left failure -> do
          case cleanup of
            Left cleanupFailure -> hPutStrLn stderr ("loopback fixture cleanup failed: " <> show cleanupFailure)
            Right () -> pure ()
          throwIO failure
        Right result -> either throwIO (const (pure result)) cleanup

runTlsPeer :: Int -> Text -> (WS.Connection -> IO ()) -> IO ()
runTlsPeer port ticket use =
  bracket (Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol) Socket.close $ \socket -> do
    Socket.connect socket (Socket.SockAddrInet (fromIntegral port) (Socket.tupleToHostAddress (127,0,0,1)))
    let defaults = TLS.defaultParamsClient "localhost" ""
        -- This isolated peer intentionally trusts the embedded loopback
        -- fixture; production TLS verification is not used or altered.
        params = defaults
          { TLS.clientHooks = defaults.clientHooks { TLS.onServerCertificate = \_ _ _ _ -> pure [] }
          , TLS.clientSupported = defaults.clientSupported { TLS.supportedCiphers = ciphersuite_default }
          }
    context <- TLS.contextNew socket params
    TLS.handshake context
    let receive = do
          bytes <- TLS.recvData context
          pure (if BS.null bytes then Nothing else Just bytes)
        send = mapM_ (TLS.sendData context)
    stream <- WSStream.makeStream receive send
    WS.runClientWithStream stream "localhost" ("/api/v1/ws?ticket=" <> showTicket ticket)
      WS.defaultConnectionOptions [] use

withExpiringCanonicalPeer
  :: DeployedSandboxApp -> Application -> (((WS.Connection -> IO ()) -> IO ()) -> IO ()) -> IO ()
withExpiringCanonicalPeer ctx application use =
  withWebSocketApplication ctx.deployedWSState application $ \port -> do
    ticket <- expiringCanonicalTicket ctx
    use (WS.runClient "127.0.0.1" port ("/api/v1/ws?ticket=" <> showTicket ticket.ticket))

expiringCanonicalTicket :: DeployedSandboxApp -> IO WebSocketTicketResponse
expiringCanonicalTicket ctx = do
  workspace <- createTestWorkspace ctx.deployedEnv "canonical-expiry-transport"
  subscriber <- createDeployedSandboxUser ctx.deployedEnv False False
  _ <- Auth.upsertWorkspaceMembership ctx.deployedEnv.pool workspace.id
    (Auth.UpsertWorkspaceMembership subscriber Auth.WorkspaceRoleRead) Nothing
  let scope = WorkspaceScope workspace.id
      principal = grantPrincipal subscriber
      audience = AuthenticatedAudience (UUID.toText subscriber) subscriber
  resume <- terminalResume ctx scope audience
  createCanonicalTicketWithTtl ctx.deployedWSState 2 principal scope resume

data OneFrameClient = OneFrameClient
  { clientWorker :: !(Async ())
  , clientInitial :: !(MVar Text)
  , clientObserved :: !(MVar Text)
  }

startOneFrameClient :: Int -> Text -> IO OneFrameClient
startOneFrameClient port ticket = do
  initial <- newEmptyMVar
  observed <- newEmptyMVar
  worker <- asyncWithUnmask $ \unmask -> unmask $ (WS.runClient "127.0.0.1" port ("/api/v1/ws?ticket=" <> showTicket ticket) $ \conn -> do
    WS.receiveData conn >>= putMVar initial
    WS.receiveData conn >>= putMVar observed
    WS.sendClose conn ("done" :: Text)) `catch` \(_ :: WS.ConnectionException) -> pure ()
  pure OneFrameClient { clientWorker = worker, clientInitial = initial, clientObserved = observed }

withActivityFrames :: Int -> Text -> (Chan Text -> IO a) -> IO a
withActivityFrames = withPeerFrames runPlainPeer

withPeerFrames
  :: (Int -> Text -> (WS.Connection -> IO ()) -> IO ())
  -> Int -> Text -> (Chan Text -> IO a) -> IO a
withPeerFrames runPeer port ticket use = mask $ \restore -> do
  frames <- newChan
  stop <- newEmptyMVar
  worker <- asyncWithUnmask $ \unmask -> unmask $
    (runPeer port ticket $ \conn ->
      withAsync (forever (WS.receiveData conn >>= writeChan frames)) $ \reader -> do
        takeMVar stop
        WS.sendClose conn ("activity fixture complete" :: Text)
        void (waitCatch reader)) `catch` \(_ :: WS.ConnectionException) -> pure ()
  primary <- try @SomeException (restore (use frames))
  putMVar stop ()
  cleanup <- timeout 2000000 (waitCatch worker)
  let failure=case cleanup of
        Nothing -> Just "activity-only client did not join within two seconds after graceful close"
        Just (Left exception) -> Just ("activity-only client cleanup failed: "<>show exception)
        Just (Right ()) -> Nothing
  case primary of
    Left exception -> maybe (pure ()) (hPutStrLn stderr) failure >> throwIO exception
    Right result -> maybe (pure result) fail failure

decodedTicket :: WaiTest.SResponse -> IO WebSocketTicketResponse
decodedTicket response = do
  responseStatus response `shouldBe` status200
  decodeResponse response

assertChangeIdentity :: OutboxRecord -> UUID.UUID -> Value -> Expectation
assertChangeIdentity record entityId frame = do
  frameType frame `shouldBe` Just "change"
  (jsonField "event" frame >>= jsonField "event_id")
    `shouldBe` Just (String $ UUID.toText record.outboxEventId)
  (jsonField "event" frame >>= jsonField "entity" >>= jsonField "id")
    `shouldBe` Just (String $ UUID.toText entityId)

authHeaders :: Text -> [Header]
authHeaders token =
  [("Authorization", "Bearer " <> Text.encodeUtf8 token)
  , ("X-HMem-Bot-Identity", "{\"role\":\"Stream Test Bot\",\"model-id\":\"test:fixture:default\"}")]

declaredIdentity :: Text -> Text -> BotIdentity
declaredIdentity role model = either (error . T.unpack) id (mkBotIdentity role model)

-- These guards belong only to this integration scenario. Preserve the original
-- assertion/timeout exception if bounded client cleanup also fails.
boundedDeclaredScenario :: IO a -> IO a
boundedDeclaredScenario action = timeout 20000000 action >>=
  maybe (fail "declared stream overall scenario deadline (setup/ticket/backlog/system/live)") pure

declaredPhase :: String -> IO a -> IO a
declaredPhase phase action = timeout 5000000 action >>=
  maybe (fail ("declared stream " <> phase <> " deadline")) pure

withDeclaredClient :: IO (Async a) -> (Async a -> IO b) -> IO b
withDeclaredClient acquire use = mask $ \restore -> do
  client <- acquire
  primary <- try @SomeException (restore (use client))
  cleanup <- try @SomeException $ timeout 1000000 $ case primary of
    Right _ -> fmap (fmap (const ())) (waitCatch client)
    Left _ -> cancel client >> pure (Right ())
  let diagnostic disposition = Just ("declared stream cleanup failure: ownedThreadId=" <> show (asyncThreadId client)
          <> "; " <> disposition <> "; Warp fixture teardown follows")
      cleanupProblem = case cleanup of
        Right (Just (Right ())) -> Nothing
        Right (Just (Left childError)) -> diagnostic ("client joined with exception: " <> show childError)
        Right Nothing -> diagnostic "client unjoined after deadline"
        Left cleanupError -> diagnostic ("join disposition unknown; cleanup exception: " <> show cleanupError)
  case primary of
    Left original -> do
      maybe (pure ()) (hPutStrLn stderr) cleanupProblem
      throwIO original
    Right result -> maybe (pure result) fail cleanupProblem

apiRequest :: Application -> BS.ByteString -> BS.ByteString -> [Header] -> LBS.ByteString -> IO WaiTest.SResponse
apiRequest app method path headers body = WaiTest.runSession (WaiTest.srequest (WaiTest.SRequest request body)) app
  where
    (rawPath, rawQuery) = BS.break (== 63) path
    request = defaultRequest
      { Wai.requestMethod = method
      , Wai.rawPathInfo = rawPath
      , Wai.rawQueryString = rawQuery
      , Wai.queryString = parseQuery rawQuery
      , Wai.pathInfo = filter (not . T.null) (T.splitOn "/" (Text.decodeUtf8 rawPath))
      , Wai.requestHeaders = ("Content-Type", "application/json") : headers
      }

responseStatus :: WaiTest.SResponse -> HTTP.Status
responseStatus = WaiTest.simpleStatus

decodeResponse :: FromJSON a => WaiTest.SResponse -> IO a
decodeResponse response =
  case decode (WaiTest.simpleBody response) of
    Just value -> pure value
    Nothing -> expectationFailure "canonical API returned invalid JSON" >> fail "unreachable"

collectSnapshotPages :: Application -> [Header] -> Value -> ChangeStreamResyncResponse -> IO ([ChangeStreamSnapshotItem], Text)
collectSnapshotPages app headers scopeValue = go []
  where
    go accumulated page =
      let items = accumulated <> page.items
      in case page.nextPageToken of
        Just pageToken -> do
          response <- apiRequest app methodPost "/api/v1/change-stream/resync" headers
            (encode (object ["scope" .= scopeValue, "page_token" .= pageToken]))
          responseStatus response `shouldBe` status200
          next <- decodeResponse response
          go items next
        Nothing -> case page.resumeToken of
          Just resume -> pure (items, resume)
          Nothing -> expectationFailure "terminal snapshot page omitted resume token" >> fail "unreachable"

checkpointResume :: Value -> IO ResumeToken
checkpointResume value = case jsonField "resume_token" value of
  Just (String token) -> pure (ResumeToken token)
  _ -> fail "canonical checkpoint omitted its opaque resume token"

isText :: Value -> Bool
isText (String _) = True
isText _ = False

awaitChangeChan :: Chan Text -> UUID.UUID -> IO ()
awaitChangeChan frames projectId = timeout 5000000 consume >>= \case
  Just () -> pure ()
  Nothing -> fail "timed out waiting for dispatcher change delivery"
  where
    consume = do
      frame <- readChan frames >>= decodeFrame
      case frameType frame of
        Just "change"
          | (jsonField "event" frame >>= jsonField "entity" >>= jsonField "id") == Just (String (UUID.toText projectId)) -> pure ()
          | otherwise -> consume
        Just "checkpoint" -> consume
        Just "session_activity_changed" -> do
          -- These out-of-band wakeups have no canonical replay/checkpoint
          -- meaning. Validate the complete narrow control before continuing
          -- to require the exact expected canonical project event.
          frame `shouldSatisfy` (\value -> case value of
            Object fields -> KeyMap.size fields==3
              && jsonField "schema_version" value==Just (toJSON (1::Int))
              && case jsonField "workspace_id" value of
                Just (String workspace) -> isJust (UUID.fromText workspace)
                _ -> False
            _ -> False)
          consume
        other -> do
          hPutStrLn stderr ("canonical marker wait received unexpected control: "<>show other)
          fail $ "expected canonical change frame, got " <> show other

decodeFrame :: Text -> IO Value
decodeFrame text = case decode (LBS.fromStrict $ Text.encodeUtf8 text) of
  Just frame -> pure frame
  Nothing -> fail "received a non-JSON canonical websocket frame"

testChangeStreamConfig :: Config.ChangeStreamConfig
testChangeStreamConfig = Config.ChangeStreamConfig
  { Config.retentionSeconds = 604800
  , Config.resumeTokenTtlSeconds = 86400
  , Config.snapshotSessionTtlSeconds = 300
  }

awaitFrame :: MVar Text -> IO Value
awaitFrame slot = do
  received <- timeout 5000000 (takeMVar slot)
  case received >>= decode . LBS.fromStrict . Text.encodeUtf8 of
    Just frame -> pure frame
    Nothing -> fail "timed out waiting for canonical websocket frame"

showTicket :: Text -> String
showTicket = T.unpack

frameType :: Value -> Maybe Text
frameType value = jsonField "type" value >>= \case String result -> Just result; _ -> Nothing

jsonField :: Text -> Value -> Maybe Value
jsonField name = \case
  Object fields -> KeyMap.lookup (Key.fromText name) fields
  _ -> Nothing
