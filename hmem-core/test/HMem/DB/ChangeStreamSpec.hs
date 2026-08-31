module HMem.DB.ChangeStreamSpec (spec) where

import Data.Aeson (Value(..), object, toJSON, (.=))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString (ByteString)
import Control.Concurrent (newEmptyMVar, putMVar, takeMVar, threadDelay, tryPutMVar)
import Control.Concurrent.Async (async, concurrently, wait)
import Control.Exception (SomeException, finally, try)
import Control.Monad (void)
import Crypto.Hash (Digest, SHA256, hash)
import Data.Either (isLeft, isRight)
import Data.Functor.Contravariant (contramap)
import Data.Int (Int64)
import Data.List (find)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time (UTCTime, addUTCTime, getCurrentTime)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUIDV4
import Hasql.Decoders qualified as Dec
import Hasql.Encoders qualified as Enc
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement
import Test.Hspec
import System.Timeout (timeout)
import Text.Read (readMaybe)

import HMem.DB.Auth qualified as Auth
import HMem.DB.ChangeStream
import HMem.DB.Pool (checkPgvector, runSession, runTransaction, withConn)
import HMem.DB.Observation qualified as Observation
import HMem.DB.Project (createProject)
import HMem.DB.Project qualified as Project
import HMem.DB.Task qualified as Task
import HMem.DB.TestHarness
import HMem.DB.WorkspaceGroup qualified as WorkspaceGroup
import HMem.Types

-- This spec deliberately runs outside the per-example rollback wrapper: a
-- serializable transaction must be the transaction's first command.  The
-- ephemeral PostgreSQL sandbox is still discarded by SpecHook after the run.
spec :: Spec
spec = beforeAll setupTestPool $ describe "Change-stream state machine" $ do
  it "retries nonterminal and terminal snapshot pages with their identical ranges" $ \env -> do
    workspace <- createTestWorkspace env "change-stream-snapshot-state"
    let scope = WorkspaceScope workspace.id
        audience = TrustedAudience "test-private-audience"
        items = [String "first", String "second"]
    begun <- beginResync env.pool 60 scope audience (pure items)
    begin <- case begun of
      Right value -> pure value
      Left err -> expectationFailure (show err) >> fail "unreachable"
    first <- readSnapshotPage env.pool scope audience begin.snapshotToken 0 1
    case first of
      Right page | page.snapshotPageItems == [String "first"]
                 , page.snapshotPageHasMore
                 , Just next <- page.snapshotNextToken
                 , Nothing <- page.snapshotResumeToken -> do
        -- Simulate response loss: retrying the original bearer is safe and
        -- returns exactly the committed ordinal range and continuation.
        readSnapshotPage env.pool scope audience begin.snapshotToken 99 9 `shouldReturn` Right page
        terminal <- readSnapshotPage env.pool scope audience next 0 2
        case terminal of
          Right terminalPage | terminalPage.snapshotPageItems == [String "second"]
                             , not terminalPage.snapshotPageHasMore
                             , Nothing <- terminalPage.snapshotNextToken
                             , Just _ <- terminalPage.snapshotResumeToken -> do
            -- Terminal response loss must expose the same item and the same
            -- activation bearer while authorization remains current.
            readSnapshotPage env.pool scope audience next 17 1 `shouldReturn` Right terminalPage
          _ -> expectationFailure (show terminal)
      _ -> expectationFailure (show first)
  it "keeps full and shell snapshot profiles distinct across idempotent retries" $ \env -> do
    workspace <- createTestWorkspace env "change-stream-snapshot-profile"
    let scope = WorkspaceScope workspace.id
        audience = TrustedAudience "snapshot-profile-audience"
        shellKey = "snapshot-profile-shell-key"
        legacyKey = "snapshot-profile-legacy-key"
    shellBegin <- beginResyncWithStartKeyAndProfile env.pool 60 scope audience shellKey 10 WorkspaceShellV1 (pure [String "shell"])
    shell <- case shellBegin of
      Right value -> pure value
      Left err -> expectationFailure (show err) >> fail "unreachable"
    shellPage <- readSnapshotPageWithStoredTtls env.pool 60 60 scope audience shell.snapshotToken
    case shellPage of
      Right SnapshotPage { snapshotPageItems = [String "shell"], snapshotPageHasMore = False, snapshotNextToken = Nothing, snapshotResumeToken = Just _, snapshotProfile = WorkspaceShellV1 } -> pure ()
      other -> expectationFailure (show other)
    beginResyncWithStartKeyAndProfile env.pool 60 scope audience shellKey 10 FullV1 (pure [String "full"])
      `shouldReturn` Left SnapshotOutOfOrder
    legacyBegin <- beginResyncWithStartKey env.pool 60 scope audience legacyKey 10 (pure [String "legacy"])
    legacy <- case legacyBegin of
      Right value -> pure value
      Left err -> expectationFailure (show err) >> fail "unreachable"
    legacyPage <- readSnapshotPageWithStoredTtls env.pool 60 60 scope audience legacy.snapshotToken
    case legacyPage of
      Right SnapshotPage { snapshotPageItems = [String "legacy"], snapshotPageHasMore = False, snapshotNextToken = Nothing, snapshotResumeToken = Just _, snapshotProfile = FullV1 } -> pure ()
      other -> expectationFailure (show other)
  it "denies expired sessions and removes their materialized items" $ \env -> do
    workspace <- createTestWorkspace env "change-stream-expiry"
    let scope = WorkspaceScope workspace.id
        audience = TrustedAudience "test-expiry-audience"
    begun <- beginResync env.pool (-1) scope audience (pure [String "never-visible"])
    begin <- case begun of
      Right value -> pure value
      Left err -> expectationFailure (show err) >> fail "unreachable"
    readSnapshotPage env.pool scope audience begin.snapshotToken 0 1 `shouldReturn` Left SnapshotExpired
    now <- getCurrentTime
    (_, removedSessions) <- cleanupChangeStream env.pool now
    removedSessions `shouldBe` 1
    readSnapshotPage env.pool scope audience begin.snapshotToken 0 1 `shouldReturn` Left SnapshotNotFound
  it "uses fresh server-random lineage after an expired start key and serializes concurrent retries" $ \env -> do
    workspace <- createTestWorkspace env "change-stream-start-idempotency"
    let scope = WorkspaceScope workspace.id
        audience = TrustedAudience "start-idempotency-audience"
        startKey = "opaque-response-loss-key"
    firstResult <- beginResyncWithStartKey env.pool 0.1 scope audience startKey 10 (pure [String "expired"])
    first <- case firstResult of Right value -> pure value; Left err -> expectationFailure (show err) >> fail "unreachable"
    firstTerminal <- readSnapshotPageWithTtls env.pool 0.1 2 scope audience first.snapshotToken 10
    firstResume <- case firstTerminal of Right SnapshotPage { snapshotResumeToken = Just value } -> pure value; other -> expectationFailure (show other) >> fail "unreachable"
    threadDelay 250000
    freshResult <- beginResyncWithStartKey env.pool 60 scope audience startKey 10 (pure [String "fresh"])
    fresh <- case freshResult of Right value -> pure value; Left err -> expectationFailure (show err) >> fail "unreachable"
    fresh.snapshotToken `shouldNotBe` first.snapshotToken
    validateCanonicalResumeToken env.pool scope audience firstResume `shouldReturn` Left ResumeNotFound
    freshPage <- readSnapshotPageWithTtls env.pool 60 2 scope audience fresh.snapshotToken 10
    freshResume <- case freshPage of Right SnapshotPage { snapshotResumeToken = Just value } -> pure value; other -> expectationFailure (show other) >> fail "unreachable"
    freshResume `shouldNotBe` firstResume
    (first, second) <- concurrently
      (beginResyncWithStartKey env.pool 60 scope audience "concurrent-opaque-key" 10 (pure [String "one"]))
      (beginResyncWithStartKey env.pool 60 scope audience "concurrent-opaque-key" 10 (pure [String "two"]))
    case (first, second) of
      (Right left, Right right) -> do
        left.snapshotToken `shouldBe` right.snapshotToken
        page <- readSnapshotPageWithTtl env.pool 60 scope audience left.snapshotToken 10
        case page of
          Right SnapshotPage { snapshotPageItems = [String item], snapshotResumeToken = Just _ } ->
            item `shouldSatisfy` (`elem` ["one", "two"])
          _ -> expectationFailure (show page)
      other -> expectationFailure (show other)
  it "keeps a terminal resume bearer alive beyond an idle snapshot session, then expires it at its own TTL" $ \env -> do
    workspace <- createTestWorkspace env "change-stream-distinct-resume-ttl"
    let scope = WorkspaceScope workspace.id
        audience = TrustedAudience "distinct-resume-ttl-audience"
    begun <- beginResync env.pool 0.1 scope audience (pure [])
    begin <- case begun of Right value -> pure value; Left err -> expectationFailure (show err) >> fail "unreachable"
    terminal <- readSnapshotPageWithTtls env.pool 0.1 0.6 scope audience begin.snapshotToken 10
    resume <- case terminal of Right SnapshotPage { snapshotResumeToken = Just value } -> pure value; other -> expectationFailure (show other) >> fail "unreachable"
    threadDelay 250000
    validated <- validateCanonicalResumeToken env.pool scope audience resume
    validated `shouldSatisfy` isRight
    threadDelay 500000
    validateCanonicalResumeToken env.pool scope audience resume `shouldReturn` Left ResumeExpired
  it "rejects a snapshot bearer that expires while its token lock is held" $ \env -> do
    workspace <- createTestWorkspace env "change-stream-snapshot-lock-expiry"
    let scope = WorkspaceScope workspace.id
        audience = TrustedAudience "snapshot-lock-expiry-audience"
    begun <- beginResync env.pool 0.1 scope audience (pure [String "must-expire"])
    begin <- case begun of Right value -> pure value; Left err -> expectationFailure (show err) >> fail "unreachable"
    acquired <- newEmptyMVar
    release <- newEmptyMVar
    holder <- async $ holdSnapshotTokenLock env.pool begin.snapshotToken acquired release
    takeMVar acquired
    reader <- async $ readSnapshotPageWithTtl env.pool 0.1 scope audience begin.snapshotToken 1
    -- A reader that starts before this wait must still use the DB clock after
    -- it acquires the bearer and scope locks, not its pre-lock wall time.
    threadDelay 250000
    putMVar release ()
    void (wait holder)
    wait reader `shouldReturn` Left SnapshotExpired
  it "binds start-derived bearers to their scope and audience" $ \env -> do
    firstWorkspace <- createTestWorkspace env "change-stream-start-token-first"
    secondWorkspace <- createTestWorkspace env "change-stream-start-token-second"
    firstUser <- UUIDV4.nextRandom
    secondUser <- UUIDV4.nextRandom
    runSession env.pool $ Session.statement firstUser insertTestUserStatement
    runSession env.pool $ Session.statement secondUser insertTestUserStatement
    _ <- Auth.upsertWorkspaceMembership env.pool firstWorkspace.id (Auth.UpsertWorkspaceMembership firstUser Auth.WorkspaceRoleRead) Nothing
    _ <- Auth.upsertWorkspaceMembership env.pool firstWorkspace.id (Auth.UpsertWorkspaceMembership secondUser Auth.WorkspaceRoleRead) Nothing
    let startKey = "opaque-start-capability-key"
        firstScope = WorkspaceScope firstWorkspace.id
        firstAudience = AuthenticatedAudience "first" firstUser
        secondAudience = AuthenticatedAudience "second" secondUser
    first <- beginResyncWithStartKey env.pool 60 firstScope firstAudience startKey 10 (pure [])
    otherScope <- beginResyncWithStartKey env.pool 60 (WorkspaceScope secondWorkspace.id) (TrustedAudience "other-scope") startKey 10 (pure [])
    otherUser <- beginResyncWithStartKey env.pool 60 firstScope secondAudience startKey 10 (pure [])
    case (first, otherScope, otherUser) of
      (Right firstBegin, Right otherScopeBegin, Right otherUserBegin) -> do
        firstBegin.snapshotToken `shouldNotBe` otherScopeBegin.snapshotToken
        firstBegin.snapshotToken `shouldNotBe` otherUserBegin.snapshotToken
        T.isInfixOf startKey firstBegin.snapshotToken.unSnapshotToken `shouldBe` False
      result -> expectationFailure (show result)
  it "orders retry starts and snapshot pages without an interleaved lock cycle" $ \env -> do
    workspace <- createTestWorkspace env "change-stream-start-page-lock-order"
    let scope = WorkspaceScope workspace.id
        audience = TrustedAudience "start-page-lock-order"
        startKey = "start-page-lock-order-key"
    begun <- beginResyncWithStartKey env.pool 60 scope audience startKey 10 (pure [String "item"])
    begin <- case begun of Right value -> pure value; Left err -> expectationFailure (show err) >> fail "unreachable"
    acquired <- newEmptyMVar
    release <- newEmptyMVar
    holder <- async $ holdScopeLock env.pool workspace.id acquired release
    takeMVar acquired
    page <- async $ readSnapshotPageWithTtl env.pool 60 scope audience begin.snapshotToken 10
    -- In the former page-token-then-scope order this has acquired the page
    -- row and is blocked on scope before the retry acquires scope and blocks
    -- on that page row. The fixed order serializes both behind scope.
    threadDelay 100000
    retry <- async $ beginResyncWithStartKey env.pool 60 scope audience startKey 10 (pure [String "other"])
    threadDelay 100000
    putMVar release ()
    completed <- timeout 2000000 ((,) <$> wait page <*> wait retry)
    void (wait holder)
    completed `shouldSatisfy` \case
      Just (Right SnapshotPage { snapshotPageItems = [String "item"] }, Right _) -> True
      _ -> False
  it "starts a short snapshot-session TTL only after materialization" $ \env -> do
    workspace <- createTestWorkspace env "change-stream-scope-lock-session-ttl"
    let scope = WorkspaceScope workspace.id
        audience = TrustedAudience "scope-lock-session-ttl-audience"
    -- The materializer delays longer than this short TTL. Capturing the clock
    -- before materialization would return an expired bearer; capturing it
    -- afterwards leaves a usable interval after the marker.
    begun <- beginResync env.pool 0.2 scope audience $ do
      marker <- Session.statement () snapshotTimingMarkerStatement
      pure [String (T.pack (show marker))]
    begin <- case begun of Right value -> pure value; Left err -> expectationFailure (show err) >> fail "unreachable"
    expiresAt <- runSession env.pool $ Session.statement (tokenHashForTest begin.snapshotToken.unSnapshotToken) snapshotExpiryStatement
    page <- readSnapshotPageWithTtl env.pool 0.2 scope audience begin.snapshotToken 1
    case page of
      Right SnapshotPage { snapshotPageItems = [String markerText] } ->
        case readMaybe (T.unpack markerText) of
          Just marker -> expiresAt `shouldSatisfy` (> marker)
          Nothing -> expectationFailure "snapshot materialization marker must be a UTC timestamp"
      _ -> expectationFailure (show page)
  it "rejects a resume bearer that expires while its token lock is held" $ \env -> do
    workspace <- createTestWorkspace env "change-stream-resume-lock-expiry"
    let scope = WorkspaceScope workspace.id
        audience = TrustedAudience "resume-lock-expiry-audience"
    begun <- beginResync env.pool 60 scope audience (pure [])
    begin <- case begun of Right value -> pure value; Left err -> expectationFailure (show err) >> fail "unreachable"
    terminal <- readSnapshotPageWithTtl env.pool 0.1 scope audience begin.snapshotToken 1
    resume <- case terminal of Right SnapshotPage { snapshotResumeToken = Just value } -> pure value; _ -> expectationFailure (show terminal) >> fail "unreachable"
    acquired <- newEmptyMVar
    release <- newEmptyMVar
    holder <- async $ holdResumeTokenLock env.pool resume acquired release
    takeMVar acquired
    reader <- async $ replayAndRotateResumeToken env.pool 60 scope audience resume 1
    threadDelay 250000
    putMVar release ()
    void (wait holder)
    wait reader `shouldReturn` Left ResumeExpired
  it "invalidates a materialized session when membership changes its auth epoch" $ \env -> do
    workspace <- createTestWorkspace env "change-stream-auth-epoch"
    userId <- UUIDV4.nextRandom
    runSession env.pool $ Session.statement userId insertTestUserStatement
    let scope = WorkspaceScope workspace.id
        audience = AuthenticatedAudience "test-user-audience" userId
        grant = Auth.UpsertWorkspaceMembership userId Auth.WorkspaceRoleRead
    _ <- Auth.upsertWorkspaceMembership env.pool workspace.id grant Nothing
    begun <- beginResync env.pool 60 scope audience (pure [String "authorized-at-begin"])
    begin <- case begun of
      Right value -> pure value
      Left err -> expectationFailure (show err) >> fail "unreachable"
    terminal <- readSnapshotPage env.pool scope audience begin.snapshotToken 0 1
    terminal `shouldSatisfy` \case Right SnapshotPage { snapshotPageItems = [String "authorized-at-begin"] } -> True; _ -> False
    _ <- Auth.upsertWorkspaceMembership env.pool workspace.id (Auth.UpsertWorkspaceMembership userId Auth.WorkspaceRoleEdit) Nothing
    -- A terminal retry rechecks auth/epoch and must not leak its cached item.
    readSnapshotPage env.pool scope audience begin.snapshotToken 0 1 `shouldReturn` Left SnapshotSuperseded
  it "binds snapshot and resume bearers to audience kind, key, and authenticated user separately" $ \env -> do
    workspace <- createTestWorkspace env "change-stream-audience-components"
    firstUser <- UUIDV4.nextRandom
    secondUser <- UUIDV4.nextRandom
    runSession env.pool $ Session.statement firstUser insertTestUserStatement
    runSession env.pool $ Session.statement secondUser insertTestUserStatement
    _ <- Auth.upsertWorkspaceMembership env.pool workspace.id (Auth.UpsertWorkspaceMembership firstUser Auth.WorkspaceRoleRead) Nothing
    _ <- Auth.upsertWorkspaceMembership env.pool workspace.id (Auth.UpsertWorkspaceMembership secondUser Auth.WorkspaceRoleRead) Nothing
    let scope = WorkspaceScope workspace.id
        trusted = TrustedAudience "same-key"
        authenticated = AuthenticatedAudience "same-key" firstUser
        wrongUser = AuthenticatedAudience "same-key" secondUser
        wrongKey = AuthenticatedAudience "other-key" firstUser
    begun <- beginResync env.pool 60 scope trusted (pure [String "private"])
    begin <- case begun of Right value -> pure value; Left err -> expectationFailure (show err) >> fail "unreachable"
    readSnapshotPage env.pool scope authenticated begin.snapshotToken 0 1 `shouldReturn` Left SnapshotNotFound
    readSnapshotPage env.pool scope wrongUser begin.snapshotToken 0 1 `shouldReturn` Left SnapshotNotFound
    readSnapshotPage env.pool scope wrongKey begin.snapshotToken 0 1 `shouldReturn` Left SnapshotNotFound
    terminal <- readSnapshotPage env.pool scope trusted begin.snapshotToken 0 1
    resume <- case terminal of Right SnapshotPage { snapshotResumeToken = Just value } -> pure value; _ -> expectationFailure (show terminal) >> fail "unreachable"
    replayAndRotateResumeToken env.pool 60 scope authenticated resume 1 `shouldReturn` Left ResumeNotFound
    replayAndRotateResumeToken env.pool 60 scope wrongUser resume 1 `shouldReturn` Left ResumeNotFound
    replayAndRotateResumeToken env.pool 60 scope wrongKey resume 1 `shouldReturn` Left ResumeNotFound
  it "locks a same-value membership upsert without advancing epoch or emitting an event" $ \env -> do
    workspace <- createTestWorkspace env "change-stream-membership-noop"
    userId <- UUIDV4.nextRandom
    runSession env.pool $ Session.statement userId insertTestUserStatement
    let scope = WorkspaceScope workspace.id
        audience = AuthenticatedAudience "same-value-membership-user" userId
        grant = Auth.UpsertWorkspaceMembership userId Auth.WorkspaceRoleRead
    _ <- Auth.upsertWorkspaceMembership env.pool workspace.id grant Nothing
    epochBefore <- runSession env.pool $ Session.statement ("workspace", Just workspace.id) scopeEpochStatement
    recordsBefore <- listOutboxAfter env.pool scope 0 100
    begun <- beginResync env.pool 60 scope audience (pure [String "still-authorized"])
    begin <- case begun of Right value -> pure value; Left err -> expectationFailure (show err) >> fail "unreachable"
    _ <- Auth.upsertWorkspaceMembership env.pool workspace.id grant Nothing
    epochAfter <- runSession env.pool $ Session.statement ("workspace", Just workspace.id) scopeEpochStatement
    epochAfter `shouldBe` epochBefore
    listOutboxAfter env.pool scope (maybe 0 (.outboxCursor) (safeLast recordsBefore)) 10 `shouldReturn` []
    page <- readSnapshotPage env.pool scope audience begin.snapshotToken 0 1
    page `shouldSatisfy` \case Right SnapshotPage { snapshotPageItems = [String "still-authorized"] } -> True; _ -> False
  it "retains a dependency workspace scope through an FK hard-delete cascade" $ \env -> do
    workspace <- createTestWorkspace env "change-stream-fk-dependency-cascade"
    prerequisite <- Task.createTask env.pool CreateTask
      { workspaceId = workspace.id, projectId = Nothing, parentId = Nothing, title = "prerequisite", description = Nothing
      , priority = Nothing, metadata = Nothing, dueAt = Nothing }
    dependent <- Task.createTask env.pool CreateTask
      { workspaceId = workspace.id, projectId = Nothing, parentId = Nothing, title = "dependent", description = Nothing
      , priority = Nothing, metadata = Nothing, dueAt = Nothing }
    Task.addDependency env.pool dependent.id prerequisite.id
    beforeDelete <- listOutboxAfter env.pool (WorkspaceScope workspace.id) 0 100
    runSession env.pool $ Session.statement prerequisite.id hardDeleteTaskStatement
    records <- listOutboxAfter env.pool (WorkspaceScope workspace.id) (maybe 0 (.outboxCursor) (safeLast beforeDelete)) 100
    let dependencyDeletes =
          [ record
          | record <- records
          , envelopeEntityField "type" record == Just (String "task_dependency")
          , envelopeEntityField "action" record == Just (String "deleted")
          ]
    dependencyDeletes `shouldSatisfy` \case [record] -> envelopeField "workspace_id" record == Just (toJSON workspace.id); _ -> False
  it "rejects disabled-user bearers and permits fresh sessions after re-enable" $ \env -> do
    workspace <- createTestWorkspace env "change-stream-user-disabled"
    userId <- UUIDV4.nextRandom
    runSession env.pool $ Session.statement userId insertTestUserStatement
    _ <- Auth.upsertWorkspaceMembership env.pool workspace.id (Auth.UpsertWorkspaceMembership userId Auth.WorkspaceRoleRead) Nothing
    runSession env.pool $ Session.statement userId grantTestSuperadminStatement
    let workspaceScope = WorkspaceScope workspace.id
        audience = AuthenticatedAudience "disabled-user-audience" userId
    workspaceBegin <- beginResync env.pool 60 workspaceScope audience (pure [String "workspace-before-disable"])
    globalBegin <- beginResync env.pool 60 GlobalScope audience (pure [String "global-before-disable"])
    workspaceToken <- case workspaceBegin of Right value -> pure value.snapshotToken; Left err -> expectationFailure (show err) >> fail "unreachable"
    globalToken <- case globalBegin of Right value -> pure value.snapshotToken; Left err -> expectationFailure (show err) >> fail "unreachable"
    runSession env.pool $ Session.statement userId disableTestUserStatement
    readSnapshotPage env.pool workspaceScope audience workspaceToken 0 1 `shouldReturn` Left ResyncUnauthorized
    readSnapshotPage env.pool GlobalScope audience globalToken 0 1 `shouldReturn` Left ResyncUnauthorized
    runSession env.pool $ Session.statement userId enableTestUserStatement
    workspaceReenabled <- beginResync env.pool 60 workspaceScope audience (pure [String "workspace-after-enable"])
    workspaceReenabled `shouldSatisfy` \case Right _ -> True; _ -> False
    globalReenabled <- beginResync env.pool 60 GlobalScope audience (pure [String "global-after-enable"])
    globalReenabled `shouldSatisfy` \case Right _ -> True; _ -> False
  it "requires current global-superadmin entitlement and revokes global sessions" $ \env -> do
    userId <- UUIDV4.nextRandom
    runSession env.pool $ Session.statement userId insertTestUserStatement
    let audience = AuthenticatedAudience "global-stream-user" userId
    beginResync env.pool 60 GlobalScope audience (pure [String "global"])
      `shouldReturn` Left ResyncUnauthorized
    runSession env.pool $ Session.statement userId grantTestSuperadminStatement
    begun <- beginResync env.pool 60 GlobalScope audience (pure [String "global"])
    begin <- case begun of
      Right value -> pure value
      Left err -> expectationFailure (show err) >> fail "unreachable"
    runSession env.pool $ Session.statement userId revokeTestSuperadminStatement
    readSnapshotPage env.pool GlobalScope audience begin.snapshotToken 0 1
      `shouldReturn` Left ResyncUnauthorized
  it "authorizes active workspace scopes for superadmins without membership and never revives old bearers after regrant" $ \env -> do
    workspace <- createTestWorkspace env "change-stream-workspace-superadmin"
    userId <- UUIDV4.nextRandom
    runSession env.pool $ Session.statement userId insertTestUserStatement
    let scope = WorkspaceScope workspace.id
        audience = AuthenticatedAudience "workspace-superadmin-audience" userId
    beginResync env.pool 60 scope audience (pure [String "requires-superadmin"])
      `shouldReturn` Left ResyncUnauthorized
    runSession env.pool $ Session.statement userId grantTestSuperadminStatement
    begun <- beginResync env.pool 60 scope audience (pure [String "superadmin-without-membership"])
    begin <- case begun of Right value -> pure value; Left err -> expectationFailure (show err) >> fail "unreachable"
    terminal <- readSnapshotPage env.pool scope audience begin.snapshotToken 0 1
    resume <- case terminal of
      Right SnapshotPage { snapshotResumeToken = Just value } -> pure value
      _ -> expectationFailure (show terminal) >> fail "unreachable"
    runSession env.pool $ Session.statement userId revokeTestSuperadminStatement
    readSnapshotPage env.pool scope audience begin.snapshotToken 0 1 `shouldReturn` Left ResyncUnauthorized
    replayAndRotateResumeToken env.pool 60 scope audience resume 10 `shouldReturn` Left ResyncUnauthorized
    runSession env.pool $ Session.statement userId grantTestSuperadminStatement
    fresh <- beginResync env.pool 60 scope audience (pure [String "fresh-after-regrant"])
    fresh `shouldSatisfy` \case Right _ -> True; _ -> False
    readSnapshotPage env.pool scope audience begin.snapshotToken 0 1 `shouldReturn` Left SnapshotSuperseded
    replayAndRotateResumeToken env.pool 60 scope audience resume 10 `shouldReturn` Left ResumeSuperseded
  it "invalidates pre-delete snapshot and resume bearers across delete and restore" $ \env -> do
    workspace <- createTestWorkspace env "change-stream-soft-deleted-workspace"
    userId <- UUIDV4.nextRandom
    runSession env.pool $ Session.statement userId insertTestUserStatement
    _ <- Auth.upsertWorkspaceMembership env.pool workspace.id (Auth.UpsertWorkspaceMembership userId Auth.WorkspaceRoleRead) Nothing
    let scope = WorkspaceScope workspace.id
        audience = AuthenticatedAudience "soft-deleted-workspace-audience" userId
    begun <- beginResync env.pool 60 scope audience (pure [String "visible-before-delete"])
    begin <- case begun of Right value -> pure value; Left err -> expectationFailure (show err) >> fail "unreachable"
    terminal <- readSnapshotPage env.pool scope audience begin.snapshotToken 0 1
    resume <- case terminal of
      Right SnapshotPage { snapshotResumeToken = Just value } -> pure value
      _ -> expectationFailure (show terminal) >> fail "unreachable"
    runSession env.pool $ Session.statement workspace.id softDeleteWorkspaceStatement
    beginResync env.pool 60 scope audience (pure [String "must-not-materialize"])
      `shouldReturn` Left ResyncUnauthorized
    readSnapshotPage env.pool scope audience begin.snapshotToken 0 1 `shouldReturn` Left ResyncUnauthorized
    replayAndRotateResumeToken env.pool 60 scope audience resume 10 `shouldReturn` Left ResyncUnauthorized
    -- Restoring access must advance the workspace epoch again rather than
    -- revive materialized credentials that were minted before deletion.
    runSession env.pool $ Session.statement workspace.id restoreWorkspaceStatement
    begunAfterRestore <- beginResync env.pool 60 scope audience (pure [String "fresh-after-restore"])
    begunAfterRestore `shouldSatisfy` \case Right _ -> True; _ -> False
    readSnapshotPage env.pool scope audience begin.snapshotToken 0 1 `shouldReturn` Left SnapshotSuperseded
    replayAndRotateResumeToken env.pool 60 scope audience resume 10 `shouldReturn` Left ResumeSuperseded
  it "rolls back an unauthorized resync without persisting scope or bearer state" $ \env -> do
    workspace <- createTestWorkspace env "change-stream-unauthorized-resync"
    let scope = WorkspaceScope workspace.id
    userId <- UUIDV4.nextRandom
    let unauthorizedAudience = AuthenticatedAudience "unauthorized-resync-user" userId
    beforeCounters <- runSession env.pool $ Session.statement workspace.id countScopeCountersStatement
    beforeSessions <- runSession env.pool $ Session.statement workspace.id countSnapshotSessionsStatement
    beforeTokens <- runSession env.pool $ Session.statement workspace.id countSnapshotPageTokensStatement
    beginResync env.pool 60 scope unauthorizedAudience (pure [String "must-not-materialize"])
      `shouldReturn` Left ResyncUnauthorized
    runSession env.pool (Session.statement workspace.id countScopeCountersStatement) `shouldReturn` beforeCounters
    runSession env.pool (Session.statement workspace.id countSnapshotSessionsStatement) `shouldReturn` beforeSessions
    runSession env.pool (Session.statement workspace.id countSnapshotPageTokensStatement) `shouldReturn` beforeTokens
  it "publishes exact audience-scoped invalidations for membership create, update, and delete" $ \env -> do
    workspace <- createTestWorkspace env "change-stream-membership-invalidations"
    userId <- UUIDV4.nextRandom
    runSession env.pool $ Session.statement userId insertTestUserStatement
    let expected = toJSON
          [ object ["kind" .= ("collection" :: T.Text), "target" .= ("workspace:" <> T.pack (show workspace.id) <> ":memberships"), "audience" .= ("workspace-admins" :: T.Text)]
          , object ["kind" .= ("catalogue" :: T.Text), "target" .= ("workspace-catalog" :: T.Text), "audience" .= ("user:" <> T.pack (show userId))]
          , object ["kind" .= ("session_authorization" :: T.Text), "target" .= ("session-authorization" :: T.Text), "audience" .= ("user:" <> T.pack (show userId))]
          , object ["kind" .= ("permission_cache" :: T.Text), "target" .= ("permission-cache" :: T.Text), "audience" .= ("user:" <> T.pack (show userId))]
          ]
        assertOne cursor action = do
          records <- listOutboxAfter env.pool (WorkspaceScope workspace.id) (cursor - 1) 1
          case records of
            [record] -> do
              case record.outboxEnvelope of
                Object envelope -> do
                  KeyMap.lookup "invalidations" envelope `shouldBe` Just expected
                  let actualAction = KeyMap.lookup "entity" envelope >>= \case
                        Object entity -> KeyMap.lookup "action" entity
                        _ -> Nothing
                  actualAction `shouldBe` Just (String action)
                _ -> expectationFailure "outbox envelope must be an object"
            _ -> expectationFailure "expected exactly one membership outbox record"
    _ <- Auth.upsertWorkspaceMembership env.pool workspace.id (Auth.UpsertWorkspaceMembership userId Auth.WorkspaceRoleRead) Nothing
    assertOne 1 "created"
    _ <- Auth.upsertWorkspaceMembership env.pool workspace.id (Auth.UpsertWorkspaceMembership userId Auth.WorkspaceRoleEdit) Nothing
    assertOne 2 "updated"
    Auth.deleteWorkspaceMembership env.pool workspace.id userId `shouldReturn` True
    assertOne 3 "deleted"
  it "records embedding-only observation updates with exact redacted invalidations and transaction metadata" $ \env -> do
    available <- checkPgvector env.pool
    if not available then pendingWith "pgvector is not installed; the embedding outbox path requires its column" else do
      workspace <- createTestWorkspace env "change-stream-observation-embedding"
      observation <- Observation.createObservation env.pool CreateObservation
        { workspaceId = workspace.id
        , subjects = [ObservationSubject SubjectFile "src/ChangeStream.hs"]
        , gitSha = T.replicate 40 "a"
        , content = "embedding redaction"
        }
      beforeRecords <- listOutboxAfter env.pool (WorkspaceScope workspace.id) 0 100
      Observation.setObservationEmbedding env.pool workspace.id observation.id (replicate observationEmbeddingDimensions 0.25)
      records <- listOutboxAfter env.pool (WorkspaceScope workspace.id) (maybe 0 (.outboxCursor) (safeLast beforeRecords)) 10
      case records of
        [record] -> do
          envelopeEntityField "type" record `shouldBe` Just (String "observation")
          envelopeEntityField "action" record `shouldBe` Just (String "updated")
          envelopeField "invalidations" record `shouldBe` Just (observationInvalidations workspace.id observation.id)
          show record.outboxEnvelope `shouldNotContain` "embedding"
          assertCoreTransactionMetadata record
        _ -> expectationFailure "expected exactly one embedding-only observation outbox record"
  it "records task status, project/task soft-delete, and restore triggers with exact invalidations" $ \env -> do
    workspace <- createTestWorkspace env "change-stream-soft-delete-outbox"
    project <- createProject env.pool CreateProject
      { workspaceId = workspace.id, parentId = Nothing, name = "outbox project", description = Nothing, priority = Nothing, metadata = Nothing }
    task <- Task.createTask env.pool CreateTask
      { workspaceId = workspace.id, projectId = Just project.id, parentId = Nothing, title = "outbox task", description = Nothing, priority = Nothing, metadata = Nothing, dueAt = Nothing }
    let scope = WorkspaceScope workspace.id
        assertOne cursor entity action expected = do
          records <- listOutboxAfter env.pool scope cursor 20
          case find (\record -> envelopeEntityField "type" record == Just (String entity) && envelopeEntityField "action" record == Just (String action)) records of
            Just record -> do
                envelopeField "invalidations" record `shouldBe` Just expected
                assertCoreTransactionMetadata record
                pure record.outboxCursor
            _ -> expectationFailure ("expected " <> T.unpack entity <> " " <> T.unpack action <> " outbox record") >> fail "unreachable"
    initial <- listOutboxAfter env.pool scope 0 100
    let cursor0 = maybe 0 (.outboxCursor) (safeLast initial)
    runSession env.pool $ Session.statement task.id updateTaskStatusForTestStatement
    cursor1 <- assertOne cursor0 "task" "updated" (taskInvalidations workspace.id task.id (Just project.id))
    Task.deleteTask env.pool task.id `shouldReturn` True
    cursor2 <- assertOne cursor1 "task" "deleted" (taskInvalidations workspace.id task.id (Just project.id))
    Task.restoreTask env.pool task.id `shouldReturn` True
    cursor3 <- assertOne cursor2 "task" "restored" (taskInvalidations workspace.id task.id (Just project.id))
    Project.deleteProject env.pool project.id `shouldReturn` True
    cursor4 <- assertOne cursor3 "project" "deleted" (projectInvalidations workspace.id project.id)
    Project.restoreProject env.pool project.id `shouldReturn` True
    void $ assertOne cursor4 "project" "restored" (projectInvalidations workspace.id project.id)
  it "rejects replay when the token watermark has been retention-pruned" $ \env -> do
    workspace <- createTestWorkspace env "change-stream-retention"
    let scope = WorkspaceScope workspace.id
        audience = TrustedAudience "retention-audience"
    begun <- beginResync env.pool 60 scope audience (pure [])
    begin <- case begun of Right value -> pure value; Left err -> expectationFailure (show err) >> fail "unreachable"
    terminal <- readSnapshotPage env.pool scope audience begin.snapshotToken 0 1
    token <- case terminal of
      Right page | Just value <- page.snapshotResumeToken -> pure value
      _ -> expectationFailure (show terminal) >> fail "unreachable"
    _ <- createProject env.pool CreateProject
      { workspaceId = workspace.id, parentId = Nothing, name = "retention-event", description = Nothing, priority = Nothing, metadata = Nothing }
    now <- getCurrentTime
    pruned <- pruneOutboxBefore env.pool (addUTCTime 1 now)
    pruned `shouldSatisfy` (> 0)
    replayAndRotateResumeToken env.pool 60 scope audience token 1 `shouldReturn` Left ReplayRetentionPruned
  it "advances replay only through the last scanned record and reports backlog" $ \env -> do
    workspace <- createTestWorkspace env "change-stream-replay-backlog"
    let scope = WorkspaceScope workspace.id
        audience = TrustedAudience "replay-backlog-audience"
    begun <- beginResync env.pool 60 scope audience (pure [])
    begin <- case begun of Right value -> pure value; Left err -> expectationFailure (show err) >> fail "unreachable"
    terminal <- readSnapshotPage env.pool scope audience begin.snapshotToken 0 1
    initialToken <- case terminal of
      Right SnapshotPage { snapshotResumeToken = Just value } -> pure value
      _ -> expectationFailure (show terminal) >> fail "unreachable"
    mapM_ (\name -> createProject env.pool CreateProject
      { workspaceId = workspace.id, parentId = Nothing, name = name, description = Nothing, priority = Nothing, metadata = Nothing })
      ["backlog-one", "backlog-two", "backlog-three"]
    first <- replayAndRotateResumeToken env.pool 60 scope audience initialToken 2
    replacement <- case first of
      Right ReplayPage { replayPageRecords = records, replayPageResumeToken = next, replayPageHasMore = hasMore } -> do
        map (.outboxCursor) records `shouldBe` [1, 2]
        hasMore `shouldBe` True
        pure next
      _ -> expectationFailure (show first) >> fail "unreachable"
    second <- replayAndRotateResumeToken env.pool 60 scope audience replacement 2
    case second of
      Right ReplayPage { replayPageRecords = records, replayPageHasMore = hasMore } -> do
        map (.outboxCursor) records `shouldBe` [3]
        hasMore `shouldBe` False
      _ -> expectationFailure (show second)
  it "keeps the prior bearer reconnectable until an explicitly acknowledged replay page" $ \env -> do
    workspace <- createTestWorkspace env "change-stream-send-failure-reconnect"
    let scope = WorkspaceScope workspace.id
        audience = TrustedAudience "send-failure-reconnect-audience"
    begun <- beginResync env.pool 60 scope audience (pure [])
    begin <- case begun of Right value -> pure value; Left err -> expectationFailure (show err) >> fail "unreachable"
    terminal <- readSnapshotPage env.pool scope audience begin.snapshotToken 0 1
    token <- case terminal of Right SnapshotPage { snapshotResumeToken = Just value } -> pure value; _ -> expectationFailure (show terminal) >> fail "unreachable"
    _ <- createProject env.pool CreateProject
      { workspaceId = workspace.id, parentId = Nothing, name = "retry-after-send-failure", description = Nothing, priority = Nothing, metadata = Nothing }
    unsent <- replayUnacknowledgedResumeToken env.pool scope audience token 10
    page <- case unsent of Right value -> pure value; Left err -> expectationFailure (show err) >> fail "unreachable"
    retry <- replayUnacknowledgedResumeToken env.pool scope audience token 10
    retry `shouldBe` Right page
    cursor <- case reverse page.replayPageRecords of record:_ -> pure record.outboxCursor; [] -> expectationFailure "expected replay record" >> fail "unreachable"
    successor <- acknowledgeReplayPage env.pool 60 scope audience token cursor
    successor `shouldSatisfy` \case Right _ -> True; _ -> False
    replayUnacknowledgedResumeToken env.pool scope audience token 10 `shouldReturn` Left ResumeSuperseded
  it "preserves the original bearer expiry while rebasing hidden delivery" $ \env -> do
    workspace <- createTestWorkspace env "change-stream-hidden-rebase-expiry"
    let scope = WorkspaceScope workspace.id
        audience = TrustedAudience "hidden-rebase-expiry-audience"
    begun <- beginResync env.pool 60 scope audience (pure [])
    begin <- case begun of Right value -> pure value; Left err -> expectationFailure (show err) >> fail "unreachable"
    terminal <- readSnapshotPageWithTtl env.pool 60 scope audience begin.snapshotToken 1
    token <- case terminal of Right SnapshotPage { snapshotResumeToken = Just value } -> pure value; _ -> expectationFailure (show terminal) >> fail "unreachable"
    _ <- createProject env.pool CreateProject
      { workspaceId = workspace.id, parentId = Nothing, name = "hidden-rebase-record", description = Nothing, priority = Nothing, metadata = Nothing }
    records <- listOutboxAfter env.pool scope 0 10
    record <- case safeLast records of
      Just value -> pure value
      Nothing -> expectationFailure "expected hidden rebase record" >> fail "unreachable"
    expiresBefore <- runSession env.pool $ Session.statement (tokenHashForTest token.unResumeToken) resumeExpiryStatement
    rebased <- rebaseResumeTokenAfterHidden env.pool 86400 scope audience token record.outboxCursor
    rebased `shouldBe` Right token
    expiresAfter <- runSession env.pool $ Session.statement (tokenHashForTest token.unResumeToken) resumeExpiryStatement
    expiresAfter `shouldBe` expiresBefore
  it "cleans up a superseded resume bearer without deleting its replacement" $ \env -> do
    workspace <- createTestWorkspace env "change-stream-superseded-cleanup"
    let scope = WorkspaceScope workspace.id
        audience = TrustedAudience "superseded-cleanup-audience"
    -- This spec shares an ephemeral database across examples.  Clear bearers
    -- superseded by earlier examples so the assertion below proves this
    -- rotation contributes exactly one removable bearer.
    beforeRotation <- getCurrentTime
    _ <- cleanupChangeStream env.pool beforeRotation
    begun <- beginResync env.pool 60 scope audience (pure [])
    begin <- case begun of Right value -> pure value; Left err -> expectationFailure (show err) >> fail "unreachable"
    terminal <- readSnapshotPage env.pool scope audience begin.snapshotToken 0 1
    token <- case terminal of Right SnapshotPage { snapshotResumeToken = Just value } -> pure value; _ -> expectationFailure (show terminal) >> fail "unreachable"
    rotated <- replayAndRotateResumeToken env.pool 60 scope audience token 10
    replacement <- case rotated of Right ReplayPage { replayPageResumeToken = value } -> pure value; _ -> expectationFailure (show rotated) >> fail "unreachable"
    now <- getCurrentTime
    (removedTokens, _) <- cleanupChangeStream env.pool now
    removedTokens `shouldBe` 1
    replayAndRotateResumeToken env.pool 60 scope audience token 10 `shouldReturn` Left ResumeNotFound
    replacementReplay <- replayAndRotateResumeToken env.pool 60 scope audience replacement 10
    replacementReplay `shouldSatisfy` \case Right _ -> True; _ -> False
  it "keeps cursors independent across workspace and global scopes" $ \env -> do
    previousGlobal <- listOutboxAfter env.pool GlobalScope 0 100
    first <- createTestWorkspace env "change-stream-scope-one"
    second <- createTestWorkspace env "change-stream-scope-two"
    let globalStart = maybe 0 (.outboxCursor) (safeLast previousGlobal)
    _ <- createProject env.pool CreateProject
      { workspaceId = first.id, parentId = Nothing, name = "scope-one", description = Nothing, priority = Nothing, metadata = Nothing }
    _ <- createProject env.pool CreateProject
      { workspaceId = second.id, parentId = Nothing, name = "scope-two", description = Nothing, priority = Nothing, metadata = Nothing }
    one <- listOutboxAfter env.pool (WorkspaceScope first.id) 0 10
    two <- listOutboxAfter env.pool (WorkspaceScope second.id) 0 10
    global <- listOutboxAfter env.pool GlobalScope globalStart 10
    map (.outboxCursor) one `shouldBe` [1]
    map (.outboxCursor) two `shouldBe` [1]
    map (.outboxCursor) global `shouldBe` [globalStart + 1, globalStart + 2]
  it "timestamps after scope serialization so retention never exposes an internal cursor hole" $ \env -> do
    workspace <- createTestWorkspace env "change-stream-prune-ordering"
    let scope = WorkspaceScope workspace.id
        audience = TrustedAudience "prune-ordering-audience"
    begun <- beginResync env.pool 60 scope audience (pure [])
    begin <- case begun of Right value -> pure value; Left err -> expectationFailure (show err) >> fail "unreachable"
    terminal <- readSnapshotPage env.pool scope audience begin.snapshotToken 0 1
    token <- case terminal of Right SnapshotPage { snapshotResumeToken = Just value } -> pure value; _ -> expectationFailure (show terminal) >> fail "unreachable"
    now <- getCurrentTime
    _ <- pruneOutboxBefore env.pool now
    _ <- concurrently
      (createProject env.pool CreateProject
        { workspaceId = workspace.id, parentId = Nothing, name = "first", description = Nothing, priority = Nothing, metadata = Nothing })
      (createProject env.pool CreateProject
        { workspaceId = workspace.id, parentId = Nothing, name = "second", description = Nothing, priority = Nothing, metadata = Nothing })
    records <- listOutboxAfter env.pool scope 0 10
    map (.outboxCursor) records `shouldBe` [1, 2]
    map (.outboxOccurredAt) records `shouldSatisfy` \case [first, second] -> first <= second; _ -> False
    let secondTimestamp = (records !! 1).outboxOccurredAt
    pruneOutboxBefore env.pool secondTimestamp `shouldReturn` 1
    retained <- listOutboxAfter env.pool scope 0 10
    map (.outboxCursor) retained `shouldBe` [2]
    replayAndRotateResumeToken env.pool 60 scope audience token 10 `shouldReturn` Left ReplayRetentionPruned
  it "locks retention scopes in combined user-authorization order under concurrency" $ \_ -> withTestEnv $ \env -> do
    first <- createTestWorkspace env "change-stream-prune-lock-first"
    second <- createTestWorkspace env "change-stream-prune-lock-second"
    userId <- UUIDV4.nextRandom
    runSession env.pool $ Session.statement userId insertTestUserStatement
    _ <- Auth.upsertWorkspaceMembership env.pool first.id (Auth.UpsertWorkspaceMembership userId Auth.WorkspaceRoleRead) Nothing
    _ <- Auth.upsertWorkspaceMembership env.pool second.id (Auth.UpsertWorkspaceMembership userId Auth.WorkspaceRoleRead) Nothing
    runSession env.pool $ Session.statement userId grantTestSuperadminStatement
    (pruned, ()) <- exerciseOrderedLockContention env.pool (max first.id second.id) "change-stream-prune-user-auth" $
      \cutoff -> do
        pruner <- async (pruneOutboxWithActivity env.pool "change-stream-pruner" cutoff)
        waitForLockWait env.pool "change-stream-pruner"
        invalidator <- async (disableAndRevokeWithActivity env.pool "change-stream-user-invalidator" userId)
        waitForLockWait env.pool "change-stream-user-invalidator"
        pure (pruner, invalidator)
    pruned `shouldSatisfy` (> 0)
  it "locks multi-workspace group deletes before the global cascade event" $ \_ -> withTestEnv $ \env -> do
    first <- createTestWorkspace env "change-stream-group-delete-first"
    second <- createTestWorkspace env "change-stream-group-delete-second"
    group <- WorkspaceGroup.createGroup env.pool (CreateWorkspaceGroup "change-stream-group-delete" Nothing)
    _ <- WorkspaceGroup.addMember env.pool group.id first.id
    _ <- WorkspaceGroup.addMember env.pool group.id second.id
    (pruned, deleted) <- exerciseOrderedLockContention env.pool (max first.id second.id) "change-stream-prune-group-delete" $
      \cutoff -> do
        pruner <- async (pruneOutboxWithActivity env.pool "change-stream-pruner" cutoff)
        waitForLockWait env.pool "change-stream-pruner"
        deleter <- async (deleteGroupWithActivity env.pool "change-stream-group-deleter" group.id)
        waitForLockWait env.pool "change-stream-group-deleter"
        pure (pruner, deleter)
    pruned `shouldSatisfy` (> 0)
    deleted `shouldBe` True
  it "does not retain an outbox record when its writer transaction rolls back" $ \env -> do
    workspace <- createTestWorkspace env "change-stream-rollback"
    project <- createProject env.pool CreateProject
      { workspaceId = workspace.id, parentId = Nothing, name = "rollback-target", description = Nothing, priority = Nothing, metadata = Nothing }
    rolledBack <- try @SomeException $ runTransaction env.pool $ do
      Session.statement project.id rollbackUpdateProjectStatement
      Session.sql "SELECT 1 / 0"
    rolledBack `shouldSatisfy` isLeft
    listOutboxAfter env.pool (WorkspaceScope workspace.id) 1 10 `shouldReturn` []
  it "invalidates distinct source and destination task associations exactly once" $ \env -> do
    workspace <- createTestWorkspace env "change-stream-task-move-invalidations"
    oldProject <- createProject env.pool CreateProject
      { workspaceId = workspace.id, parentId = Nothing, name = "old project", description = Nothing, priority = Nothing, metadata = Nothing }
    newProject <- createProject env.pool CreateProject
      { workspaceId = workspace.id, parentId = Nothing, name = "new project", description = Nothing, priority = Nothing, metadata = Nothing }
    oldParent <- Task.createTask env.pool CreateTask
      { workspaceId = workspace.id, projectId = Just oldProject.id, parentId = Nothing, title = "old parent", description = Nothing, priority = Nothing, metadata = Nothing, dueAt = Nothing }
    newParent <- Task.createTask env.pool CreateTask
      { workspaceId = workspace.id, projectId = Just newProject.id, parentId = Nothing, title = "new parent", description = Nothing, priority = Nothing, metadata = Nothing, dueAt = Nothing }
    moved <- Task.createTask env.pool CreateTask
      { workspaceId = workspace.id, projectId = Just oldProject.id, parentId = Just oldParent.id, title = "moved task", description = Nothing, priority = Nothing, metadata = Nothing, dueAt = Nothing }
    priorRecords <- listOutboxAfter env.pool (WorkspaceScope workspace.id) 0 20
    runSession env.pool $ Session.statement (moved.id, newProject.id, newParent.id) moveTaskStatement
    records <- listOutboxAfter env.pool (WorkspaceScope workspace.id) (maybe 0 (.outboxCursor) (safeLast priorRecords)) 10
    let idText = T.pack . show
        expected = toJSON
          [ object ["kind" .= ("entity" :: T.Text), "target" .= ("task:" <> idText moved.id)]
          , object ["kind" .= ("collection" :: T.Text), "target" .= ("tasks:" <> idText workspace.id)]
          , object ["kind" .= ("tree" :: T.Text), "target" .= ("workspace:" <> idText workspace.id)]
          , object ["kind" .= ("readiness" :: T.Text), "target" .= ("task:" <> idText moved.id)]
          , object ["kind" .= ("next_task" :: T.Text), "target" .= ("workspace:" <> idText workspace.id)]
          , object ["kind" .= ("search" :: T.Text), "target" .= ("workspace:" <> idText workspace.id)]
          , object ["kind" .= ("readiness" :: T.Text), "target" .= ("project:" <> idText oldProject.id)]
          , object ["kind" .= ("readiness" :: T.Text), "target" .= ("project:" <> idText newProject.id)]
          , object ["kind" .= ("readiness" :: T.Text), "target" .= ("task:" <> idText oldParent.id)]
          , object ["kind" .= ("readiness" :: T.Text), "target" .= ("task:" <> idText newParent.id)]
          ]
    case records of
      [record] -> envelopeField "invalidations" record `shouldBe` Just expected
      _ -> expectationFailure "expected one outbox record for the direct task move"

observationInvalidations :: UUID.UUID -> UUID.UUID -> Value
observationInvalidations workspaceId observationId = toJSON
  [ object ["kind" .= ("entity" :: T.Text), "target" .= ("observation:" <> T.pack (show observationId))]
  , object ["kind" .= ("collection" :: T.Text), "target" .= ("observations:" <> T.pack (show workspaceId))]
  , object ["kind" .= ("search" :: T.Text), "target" .= ("workspace:" <> T.pack (show workspaceId))]
  ]

taskInvalidations :: UUID.UUID -> UUID.UUID -> Maybe UUID.UUID -> Value
taskInvalidations workspaceId taskId projectId = toJSON $
  [ object ["kind" .= ("entity" :: T.Text), "target" .= ("task:" <> T.pack (show taskId))]
  , object ["kind" .= ("collection" :: T.Text), "target" .= ("tasks:" <> T.pack (show workspaceId))]
  , object ["kind" .= ("tree" :: T.Text), "target" .= ("workspace:" <> T.pack (show workspaceId))]
  , object ["kind" .= ("readiness" :: T.Text), "target" .= ("task:" <> T.pack (show taskId))]
  , object ["kind" .= ("next_task" :: T.Text), "target" .= ("workspace:" <> T.pack (show workspaceId))]
  , object ["kind" .= ("search" :: T.Text), "target" .= ("workspace:" <> T.pack (show workspaceId))]
  ] <> maybe [] (\project -> [object ["kind" .= ("readiness" :: T.Text), "target" .= ("project:" <> T.pack (show project))]]) projectId

projectInvalidations :: UUID.UUID -> UUID.UUID -> Value
projectInvalidations workspaceId projectId = toJSON
  [ object ["kind" .= ("entity" :: T.Text), "target" .= ("project:" <> T.pack (show projectId))]
  , object ["kind" .= ("collection" :: T.Text), "target" .= ("projects:" <> T.pack (show workspaceId))]
  , object ["kind" .= ("tree" :: T.Text), "target" .= ("workspace:" <> T.pack (show workspaceId))]
  , object ["kind" .= ("readiness" :: T.Text), "target" .= ("project:" <> T.pack (show projectId))]
  , object ["kind" .= ("next_task" :: T.Text), "target" .= ("workspace:" <> T.pack (show workspaceId))]
  , object ["kind" .= ("search" :: T.Text), "target" .= ("workspace:" <> T.pack (show workspaceId))]
  ]

assertCoreTransactionMetadata :: OutboxRecord -> IO ()
assertCoreTransactionMetadata record =
  case envelopeField "transaction" record of
    Just (Object transaction) -> do
      KeyMap.lookup "cause" transaction `shouldBe` Just (String "core")
      KeyMap.lookup "request_id" transaction `shouldBe` Just Null
      KeyMap.lookup "id" transaction `shouldSatisfy` \case Just (String _) -> True; _ -> False
    _ -> expectationFailure "outbox envelope must include redacted core transaction metadata"

safeLast :: [a] -> Maybe a
safeLast = \case
  [] -> Nothing
  values -> Just (last values)

-- Hold the last workspace lock while each writer advances through its first
-- lock.  The activity wait proves the pruner has reached the held scope; the
-- peer's wait then proves it has attempted the same first scope before the
-- blocker is released.  Reversing either production lock order turns this
-- into a real cycle rather than a timing-dependent eventual-completion test.
exerciseOrderedLockContention pool blockedWorkspace label start = do
  cutoff <- addUTCTime 1 <$> getCurrentTime
  acquired <- newEmptyMVar
  release <- newEmptyMVar
  holder <- async (holdScopeLock pool blockedWorkspace acquired release)
  takeMVar acquired
  result <- (timeout 5000000 $ do
    (first, second) <- start cutoff
    putMVar release ()
    void (wait holder)
    (,) <$> wait first <*> wait second)
    `finally` (void (tryPutMVar release ()) >> void (wait holder))
  case result of
    Just values -> pure values
    Nothing -> expectationFailure (label <> " deadlocked after both transactions reached their first lock") >> fail "unreachable"

holdScopeLock pool workspaceId acquired release = withConn pool $ \connection ->
  (do
    started <- Session.run (Session.sql "BEGIN" *> Session.statement ("workspace", Just workspaceId) lockScopeForTestStatement) connection
    case started of
      Left err -> fail (show err)
      Right _ -> pure ()
    putMVar acquired ()
    takeMVar release
    committed <- Session.run (Session.sql "COMMIT") connection
    case committed of
      Left err -> fail (show err)
      Right _ -> pure ())
  `finally` void (Session.run (Session.sql "ROLLBACK") connection)

-- Keep the persisted bearer row locked past its short TTL.  The caller starts
-- the production operation while this transaction is open, which makes the
-- regression deterministic: a timestamp captured before @FOR UPDATE@ would
-- be stale by the time the lock is released.
holdSnapshotTokenLock pool token acquired release = withConn pool $ \connection ->
  (do
    started <- Session.run (Session.sql "BEGIN" *> Session.statement (tokenHashForTest token.unSnapshotToken) lockSnapshotTokenForTestStatement) connection
    case started of
      Left err -> fail (show err)
      Right _ -> pure ()
    putMVar acquired ()
    takeMVar release
    committed <- Session.run (Session.sql "COMMIT") connection
    case committed of
      Left err -> fail (show err)
      Right _ -> pure ())
  `finally` void (Session.run (Session.sql "ROLLBACK") connection)

holdResumeTokenLock pool token acquired release = withConn pool $ \connection ->
  (do
    started <- Session.run (Session.sql "BEGIN" *> Session.statement (tokenHashForTest token.unResumeToken) lockResumeTokenForTestStatement) connection
    case started of
      Left err -> fail (show err)
      Right _ -> pure ()
    putMVar acquired ()
    takeMVar release
    committed <- Session.run (Session.sql "COMMIT") connection
    case committed of
      Left err -> fail (show err)
      Right _ -> pure ())
  `finally` void (Session.run (Session.sql "ROLLBACK") connection)

tokenHashForTest raw = TE.encodeUtf8 $ T.pack $ show (hash (TE.encodeUtf8 raw) :: Digest SHA256)

waitForLockWait pool activityName = go (500 :: Int)
  where
    go 0 = expectationFailure ("timed out waiting for lock observation: " <> T.unpack activityName)
    go attempts = do
      waiting <- runSession pool $ Session.statement activityName activityWaitingForLockStatement
      if waiting
        then pure ()
        else threadDelay 10000 >> go (attempts - 1)

pruneOutboxWithActivity pool activityName cutoff = runTransaction pool $ do
  Session.statement activityName setLocalApplicationNameStatement
  Session.statement cutoff pruneOutboxForTestStatement

disableAndRevokeWithActivity pool activityName userId = runTransaction pool $ do
  Session.statement activityName setLocalApplicationNameStatement
  Session.statement userId disableAndRevokeTestUserStatement

deleteGroupWithActivity pool activityName groupId = runTransaction pool $ do
  Session.statement activityName setLocalApplicationNameStatement
  (> 0) <$> Session.statement groupId deleteGroupForTestStatement

envelopeField :: T.Text -> OutboxRecord -> Maybe Value
envelopeField field OutboxRecord { outboxEnvelope = Object envelope } = KeyMap.lookup (Key.fromText field) envelope
envelopeField _ _ = Nothing

envelopeEntityField :: T.Text -> OutboxRecord -> Maybe Value
envelopeEntityField field record = envelopeField "entity" record >>= \case
  Object entity -> KeyMap.lookup (Key.fromText field) entity
  _ -> Nothing

insertTestUserStatement :: Statement.Statement UUID.UUID ()
insertTestUserStatement = Statement.Statement
  "INSERT INTO users(id, auth_subject, display_name) VALUES ($1, 'change-stream-test-user-' || $1::text, 'Change stream test')"
  (Enc.param (Enc.nonNullable Enc.uuid)) Dec.noResult True

grantTestSuperadminStatement :: Statement.Statement UUID.UUID ()
grantTestSuperadminStatement = Statement.Statement
  "UPDATE users SET is_superadmin = true WHERE id = $1"
  (Enc.param (Enc.nonNullable Enc.uuid)) Dec.noResult True

revokeTestSuperadminStatement :: Statement.Statement UUID.UUID ()
revokeTestSuperadminStatement = Statement.Statement
  "UPDATE users SET is_superadmin = false WHERE id = $1"
  (Enc.param (Enc.nonNullable Enc.uuid)) Dec.noResult True

disableTestUserStatement :: Statement.Statement UUID.UUID ()
disableTestUserStatement = Statement.Statement
  "UPDATE users SET disabled_at = now() WHERE id = $1"
  (Enc.param (Enc.nonNullable Enc.uuid)) Dec.noResult True

enableTestUserStatement :: Statement.Statement UUID.UUID ()
enableTestUserStatement = Statement.Statement
  "UPDATE users SET disabled_at = NULL WHERE id = $1"
  (Enc.param (Enc.nonNullable Enc.uuid)) Dec.noResult True

disableAndRevokeTestUserStatement :: Statement.Statement UUID.UUID ()
disableAndRevokeTestUserStatement = Statement.Statement
  "UPDATE users SET disabled_at = now(), is_superadmin = false WHERE id = $1"
  (Enc.param (Enc.nonNullable Enc.uuid)) Dec.noResult True

scopeEpochStatement :: Statement.Statement (T.Text, Maybe UUID.UUID) Int64
scopeEpochStatement = Statement.Statement
  "SELECT authorization_epoch FROM change_stream_scope_counters WHERE scope = $1 AND workspace_id IS NOT DISTINCT FROM $2"
  ((contramap fst $ Enc.param $ Enc.nonNullable Enc.text) <> (contramap snd $ Enc.param $ Enc.nullable Enc.uuid))
  (Dec.singleRow (Dec.column (Dec.nonNullable Dec.int8))) True

countScopeCountersStatement :: Statement.Statement UUID.UUID Int64
countScopeCountersStatement = Statement.Statement
  "SELECT count(*)::bigint FROM change_stream_scope_counters WHERE scope = 'workspace' AND workspace_id = $1"
  (Enc.param (Enc.nonNullable Enc.uuid))
  (Dec.singleRow (Dec.column (Dec.nonNullable Dec.int8))) True

countSnapshotSessionsStatement :: Statement.Statement UUID.UUID Int64
countSnapshotSessionsStatement = Statement.Statement
  "SELECT count(*)::bigint FROM change_stream_snapshot_sessions WHERE scope = 'workspace' AND workspace_id = $1"
  (Enc.param (Enc.nonNullable Enc.uuid))
  (Dec.singleRow (Dec.column (Dec.nonNullable Dec.int8))) True

countSnapshotPageTokensStatement :: Statement.Statement UUID.UUID Int64
countSnapshotPageTokensStatement = Statement.Statement
  "SELECT count(*)::bigint FROM change_stream_snapshot_page_tokens p JOIN change_stream_snapshot_sessions s ON s.session_hash = p.session_hash WHERE s.scope = 'workspace' AND s.workspace_id = $1"
  (Enc.param (Enc.nonNullable Enc.uuid))
  (Dec.singleRow (Dec.column (Dec.nonNullable Dec.int8))) True

lockScopeForTestStatement :: Statement.Statement (T.Text, Maybe UUID.UUID) Int64
lockScopeForTestStatement = Statement.Statement
  "SELECT (hmem_change_stream_lock($1, $2)).authorization_epoch"
  ((contramap fst $ Enc.param $ Enc.nonNullable Enc.text) <> (contramap snd $ Enc.param $ Enc.nullable Enc.uuid))
  (Dec.singleRow (Dec.column (Dec.nonNullable Dec.int8))) True

updateTaskStatusForTestStatement :: Statement.Statement UUID.UUID ()
updateTaskStatusForTestStatement = Statement.Statement
  "UPDATE tasks SET status = 'done'::task_status_enum WHERE id = $1"
  (Enc.param (Enc.nonNullable Enc.uuid)) Dec.noResult True

snapshotTimingMarkerStatement :: Statement.Statement () UTCTime
snapshotTimingMarkerStatement = Statement.Statement
  "SELECT clock_timestamp() FROM (SELECT pg_sleep(0.5)) AS delayed"
  Enc.noParams (Dec.singleRow (Dec.column (Dec.nonNullable Dec.timestamptz))) True

snapshotExpiryStatement :: Statement.Statement ByteString UTCTime
snapshotExpiryStatement = Statement.Statement
  "SELECT s.expires_at FROM change_stream_snapshot_sessions s JOIN change_stream_snapshot_page_tokens p ON p.session_hash = s.session_hash WHERE p.token_hash = $1"
  (Enc.param (Enc.nonNullable Enc.bytea))
  (Dec.singleRow (Dec.column (Dec.nonNullable Dec.timestamptz))) True

resumeExpiryStatement :: Statement.Statement ByteString UTCTime
resumeExpiryStatement = Statement.Statement
  "SELECT expires_at FROM change_stream_resume_tokens WHERE token_hash = $1"
  (Enc.param (Enc.nonNullable Enc.bytea))
  (Dec.singleRow (Dec.column (Dec.nonNullable Dec.timestamptz))) True

lockSnapshotTokenForTestStatement = Statement.Statement
  "SELECT 1 FROM change_stream_snapshot_page_tokens WHERE token_hash = $1 FOR UPDATE"
  (Enc.param (Enc.nonNullable Enc.bytea))
  (Dec.singleRow (Dec.column (Dec.nonNullable Dec.int4))) True

lockResumeTokenForTestStatement = Statement.Statement
  "SELECT 1 FROM change_stream_resume_tokens WHERE token_hash = $1 FOR UPDATE"
  (Enc.param (Enc.nonNullable Enc.bytea))
  (Dec.singleRow (Dec.column (Dec.nonNullable Dec.int4))) True

setLocalApplicationNameStatement :: Statement.Statement T.Text T.Text
setLocalApplicationNameStatement = Statement.Statement
  "SELECT set_config('application_name', $1, true)"
  (Enc.param (Enc.nonNullable Enc.text))
  (Dec.singleRow (Dec.column (Dec.nonNullable Dec.text))) True

activityWaitingForLockStatement :: Statement.Statement T.Text Bool
activityWaitingForLockStatement = Statement.Statement
  "SELECT EXISTS (SELECT 1 FROM pg_stat_activity WHERE application_name = $1 AND wait_event_type = 'Lock')"
  (Enc.param (Enc.nonNullable Enc.text))
  (Dec.singleRow (Dec.column (Dec.nonNullable Dec.bool))) True

pruneOutboxForTestStatement :: Statement.Statement UTCTime Int64
pruneOutboxForTestStatement = Statement.Statement
  "SELECT hmem_change_stream_prune_outbox($1)"
  (Enc.param (Enc.nonNullable Enc.timestamptz))
  (Dec.singleRow (Dec.column (Dec.nonNullable Dec.int8))) True

deleteGroupForTestStatement :: Statement.Statement UUID.UUID Int64
deleteGroupForTestStatement = Statement.Statement
  "DELETE FROM workspace_groups WHERE id = $1"
  (Enc.param (Enc.nonNullable Enc.uuid))
  Dec.rowsAffected True

softDeleteWorkspaceStatement :: Statement.Statement UUID.UUID ()
softDeleteWorkspaceStatement = Statement.Statement
  "UPDATE workspaces SET deleted_at = now() WHERE id = $1"
  (Enc.param (Enc.nonNullable Enc.uuid)) Dec.noResult True

restoreWorkspaceStatement :: Statement.Statement UUID.UUID ()
restoreWorkspaceStatement = Statement.Statement
  "UPDATE workspaces SET deleted_at = NULL WHERE id = $1"
  (Enc.param (Enc.nonNullable Enc.uuid)) Dec.noResult True

hardDeleteTaskStatement :: Statement.Statement UUID.UUID ()
hardDeleteTaskStatement = Statement.Statement
  "DELETE FROM tasks WHERE id = $1"
  (Enc.param (Enc.nonNullable Enc.uuid)) Dec.noResult True

rollbackUpdateProjectStatement :: Statement.Statement UUID.UUID ()
rollbackUpdateProjectStatement = Statement.Statement
  "UPDATE projects SET name = name || '-rolled-back' WHERE id = $1"
  (Enc.param (Enc.nonNullable Enc.uuid)) Dec.noResult True

moveTaskStatement :: Statement.Statement (UUID.UUID, UUID.UUID, UUID.UUID) ()
moveTaskStatement = Statement.Statement
  "UPDATE tasks SET project_id = $2, parent_id = $3 WHERE id = $1"
  (contramap (\(taskId, _, _) -> taskId) (Enc.param (Enc.nonNullable Enc.uuid))
    <> contramap (\(_, projectId, _) -> projectId) (Enc.param (Enc.nonNullable Enc.uuid))
    <> contramap (\(_, _, parentId) -> parentId) (Enc.param (Enc.nonNullable Enc.uuid)))
  Dec.noResult True
