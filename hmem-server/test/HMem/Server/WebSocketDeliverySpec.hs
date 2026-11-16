module HMem.Server.WebSocketDeliverySpec (spec) where

import Control.Concurrent (newEmptyMVar, putMVar, readMVar, takeMVar, threadDelay, tryPutMVar, tryTakeMVar)
import Control.Concurrent.Async (AsyncCancelled(..), cancel, mapConcurrently_, wait, waitCatch, withAsync)
import Control.Exception (finally, fromException, try, uninterruptibleMask_)
import Control.Monad (forM_, replicateM, void)
import Data.Text qualified as T
import HMem.Server.WebSocketDelivery
import System.Timeout (timeout)
import Test.Hspec

spec :: Spec
spec = describe "owned WebSocket delivery" $ do
  it "interrupts a stalled write before joining and keeps another observer moving" $ do
    hub <- newDeliveryHub
    writing <- newEmptyMVar
    interrupted <- newEmptyMVar
    writeEnded <- newEmptyMVar
    slow <- admitted hub (putMVar interrupted ())
    healthy <- admitted hub (pure ())
    let blocked = (putMVar writing () >> readMVar interrupted)
          `finally` (readMVar interrupted >> putMVar writeEnded ())
    withAsync (withDeliveryTurn slow (const (controlledWrite slow blocked))) $ \turn -> do
      within "slow write started" (takeMVar writing)
      withDeliveryTurn healthy (const (controlledWrite healthy (pure (42 :: Int))))
        `shouldReturn` DeliveryCompleted 42
      within "slow turn retired" (wait turn) `shouldReturn` DeliveryRetired
      tryTakeMVar writeEnded `shouldReturn` Just ()
    finishDeliveryOwner slow
    finishDeliveryOwner healthy
    awaitDeliveryDrain hub 250000

  it "does not let a prior write deadline retire a newer write generation" $ do
    hub <- newDeliveryHub
    owner <- admitted hub (expectationFailure "unexpected native interruption")
    let write = controlledWrite owner (threadDelay 140000)
    withDeliveryTurn owner (const (write >> write)) `shouldReturn` DeliveryCompleted ()
    finishDeliveryOwner owner

  it "uses at most four leases and wakes a waiting producer on closing" $ do
    hub <- newDeliveryHub
    owners <- replicateM 4 (admitted hub (pure ()))
    waiting <- admitted hub (pure ())
    gates <- replicateM 4 newEmptyMVar
    entered <- replicateM 4 newEmptyMVar
    fifth <- newEmptyMVar
    let run owner gate ready = withDeliveryTurn owner $ \_ -> putMVar ready () >> takeMVar gate
        runAll [] body = body
        runAll (action:rest) body = withAsync action $ \worker ->
          runAll rest (body >> void (wait worker))
        unblock = forM_ gates (\gate -> void (tryPutMVar gate ()))
    runAll (zipWith3 run owners gates entered) $
      (do
        forM_ entered (within "lease entered" . takeMVar)
        withAsync (withDeliveryTurn waiting (const (putMVar fifth ()))) $ \producer -> do
          timeout 30000 (readMVar fifth) `shouldReturn` Nothing
          abortDeliveryOwner waiting
          within "waiting producer woke" (wait producer) `shouldReturn` DeliveryRetired
          tryTakeMVar fifth `shouldReturn` Nothing)
      `finally` unblock
    forM_ (waiting:owners) finishDeliveryOwner
    awaitDeliveryDrain hub 250000

  it "rejects a foreign participant before an application Close mutates state" $ do
    hub <- newDeliveryHub
    first <- admitted hub (pure ())
    second <- admitted hub (pure ())
    entered <- newEmptyMVar
    withDeliveryTurn first (\_ -> do
      ownedClose second (putMVar entered ()) `shouldThrow` (== DeliveryOwnershipViolation)
      controlledWrite second (putMVar entered ()) `shouldThrow` (== DeliveryOwnershipViolation)
      controlledWrite first (pure ()))
      `shouldReturn` DeliveryCompleted ()
    tryTakeMVar entered `shouldReturn` Nothing
    forM_ [first, second] finishDeliveryOwner

  it "holds physical seats through abort until outer worker completion" $ do
    hub <- newDeliveryHub
    same <- replicateM 32 (admitted hub (pure ()))
    refused <- newDeliveryOwner hub (pure ())
    let oldest = head same
    reservePhysicalConnection refused "audience" `shouldReturn` False
    abortDeliveryOwner oldest
    reservePhysicalConnection refused "audience" `shouldReturn` False
    finishDeliveryOwner oldest
    reservePhysicalConnection refused "audience" `shouldReturn` True
    others <- mapM (\n -> do
      owner <- newDeliveryOwner hub (pure ())
      reservePhysicalConnection owner (T.pack (show n)) `shouldReturn` True
      pure owner) [1..96 :: Int]
    extra <- newDeliveryOwner hub (pure ())
    reservePhysicalConnection extra "over capacity" `shouldReturn` False
    beginDeliveryShutdown hub
    reservePhysicalConnection extra "closed" `shouldReturn` False
    forM_ (refused:tail same <> others) finishDeliveryOwner
    awaitDeliveryDrain hub 250000

  it "retains a lease after an unfinished join without closing other admission" $ do
    hub <- newDeliveryHub
    writing <- newEmptyMVar
    release <- newEmptyMVar
    aborted <- newEmptyMVar
    childEnded <- newEmptyMVar
    owner <- admitted hub (putMVar aborted ())
    let write = uninterruptibleMask_ (putMVar writing () >> takeMVar release)
          `finally` putMVar childEnded ()
    withAsync (try @DeliveryFailure (withDeliveryTurn owner (const (controlledWrite owner write)))) $ \parent ->
      (do
        within "masked writer entered" (takeMVar writing)
        within "parent retired the connection" (wait parent) `shouldReturn` Right DeliveryRetired
        tryTakeMVar aborted `shouldReturn` Just ()
        tryTakeMVar childEnded `shouldReturn` Nothing
        extra <- admitted hub (pure ())
        withDeliveryTurn extra (const (controlledWrite extra (pure (7 :: Int))))
          `shouldReturn` DeliveryCompleted 7
        finishDeliveryOwner extra
        -- Fill the remaining leases: the retained turn still holds the fourth.
        others <- replicateM 3 (admitted hub (pure ()))
        gates <- replicateM 3 newEmptyMVar
        entered <- replicateM 3 newEmptyMVar
        waiting <- admitted hub (pure ())
        fourth <- newEmptyMVar
        let hold other gate ready = withDeliveryTurn other $ \_ -> putMVar ready () >> takeMVar gate
        withAsync (mapConcurrently_ id (zipWith3 hold others gates entered)) $ \holders -> do
          forM_ entered (within "remaining lease entered" . takeMVar)
          withAsync (withDeliveryTurn waiting (const (putMVar fourth ()))) $ \producer -> do
            timeout 30000 (readMVar fourth) `shouldReturn` Nothing
            -- The child ends before its outer worker: only the lease returns.
            putMVar release ()
            within "released writer actually ended" (takeMVar childEnded)
            within "reclaimed lease admitted the waiting turn" (wait producer) `shouldReturn` DeliveryCompleted ()
          forM_ gates (`putMVar` ())
          within "remaining leases released" (wait holders)
        forM_ (waiting:others) finishDeliveryOwner
        awaitDeliveryDrain hub 50000 `shouldThrow` (== DeliveryUnconfirmed)
        finishDeliveryOwner owner
        awaitDeliveryDrain hub 250000)
      `finally` void (tryPutMVar release ())

  it "keeps a retained seat counted until its child ends after the outer worker" $ do
    hub <- newDeliveryHub
    writing <- newEmptyMVar
    release <- newEmptyMVar
    childEnded <- newEmptyMVar
    owner <- admitted hub (pure ())
    same <- replicateM 31 (admitted hub (pure ()))
    let write = uninterruptibleMask_ (putMVar writing () >> takeMVar release)
          `finally` putMVar childEnded ()
    (do
      withDeliveryTurn owner (const (controlledWrite owner write)) `shouldReturn` DeliveryRetired
      within "masked writer entered" (takeMVar writing)
      finishDeliveryOwner owner
      -- The aborted owner's seat still counts toward its audience limit.
      refused <- newDeliveryOwner hub (pure ())
      reservePhysicalConnection refused "audience" `shouldReturn` False
      forM_ same finishDeliveryOwner
      awaitDeliveryDrain hub 50000 `shouldThrow` (== DeliveryUnconfirmed)
      putMVar release ()
      within "released writer actually ended" (takeMVar childEnded)
      awaitDeliveryDrain hub 250000
      reservePhysicalConnection refused "audience" `shouldReturn` True
      finishDeliveryOwner refused
      awaitDeliveryDrain hub 250000)
      `finally` void (tryPutMVar release ())

  it "preserves cancellation after a successful native abort and join" $ do
    hub <- newDeliveryHub
    writing <- newEmptyMVar
    interrupted <- newEmptyMVar
    childEnded <- newEmptyMVar
    owner <- admitted hub (putMVar interrupted ())
    let write = (putMVar writing () >> readMVar interrupted)
          `finally` putMVar childEnded ()
    withAsync (withDeliveryTurn owner (const (controlledWrite owner write))) $ \parent -> do
      within "clean cancelled writer entered" (takeMVar writing)
      within "clean cancelled parent returned" (cancel parent)
      outcome <- waitCatch parent
      case outcome of
        Left err -> fromException err `shouldBe` Just AsyncCancelled
        Right _ -> expectationFailure "cancelled parent reported successful delivery"
      tryTakeMVar childEnded `shouldReturn` Just ()
    finishDeliveryOwner owner
    awaitDeliveryDrain hub 250000

  it "reports an unconfirmed join when its parent is cancelled" $ do
    hub <- newDeliveryHub
    writing <- newEmptyMVar
    release <- newEmptyMVar
    childEnded <- newEmptyMVar
    owner <- admitted hub (pure ())
    let write = uninterruptibleMask_ (putMVar writing () >> takeMVar release)
          `finally` putMVar childEnded ()
    withAsync (withDeliveryTurn owner (const (controlledWrite owner write))) $ \parent ->
      (do
        within "cancelled parent writer entered" (takeMVar writing)
        within "cancelled parent returned" (cancel parent)
        outcome <- waitCatch parent
        case outcome of
          Left err -> case fromException err of
            Just (DeliveryCleanupFailure original cleanup) -> do
              fromException original `shouldBe` Just AsyncCancelled
              fromException cleanup `shouldBe` Just DeliveryUnconfirmed
            Nothing -> expectationFailure ("lost cancellation/cleanup causes: " <> show err)
          Right _ -> expectationFailure "cancelled parent reported successful delivery"
        assertAdmissionOpen hub
        finishDeliveryOwner owner
        awaitDeliveryDrain hub 50000 `shouldThrow` (== DeliveryUnconfirmed))
      `finally` void (tryPutMVar release ())
    within "cancelled parent child actually ended" (takeMVar childEnded)
    awaitDeliveryDrain hub 250000

  it "retains cancellation and ownership when cancellation interrupts retirement" $ do
    hub <- newDeliveryHub
    writing <- newEmptyMVar
    abortEntered <- newEmptyMVar
    releaseAbort <- newEmptyMVar
    childEnded <- newEmptyMVar
    owner <- admitted hub (putMVar abortEntered () >> readMVar releaseAbort)
    let write = (putMVar writing () >> readMVar releaseAbort)
          `finally` putMVar childEnded ()
    withAsync (withDeliveryTurn owner (const (controlledWrite owner write))) $ \parent ->
      (do
        within "retirement writer entered" (takeMVar writing)
        within "retirement native abort began" (takeMVar abortEntered)
        within "retirement cancellation returned" (cancel parent)
        outcome <- waitCatch parent
        case outcome of
          Left err -> case fromException err of
            Just (DeliveryCleanupFailure original cleanup) -> do
              fromException original `shouldBe` Just AsyncCancelled
              fromException cleanup `shouldBe` Just DeliveryUnconfirmed
            Nothing -> expectationFailure ("retirement lost cancellation/cleanup causes: " <> show err)
          Right _ -> expectationFailure "interrupted retirement reported successful delivery"
        tryTakeMVar childEnded `shouldReturn` Nothing
        assertAdmissionOpen hub
        finishDeliveryOwner owner
        awaitDeliveryDrain hub 50000 `shouldThrow` (== DeliveryUnconfirmed))
      `finally` void (tryPutMVar releaseAbort ())
    within "interrupted retirement child actually ended" (takeMVar childEnded)
    awaitDeliveryDrain hub 250000

  it "completes an interrupted retirement within its original cleanup deadline" $ do
    hub <- newDeliveryHub
    writing <- newEmptyMVar
    abortEntered <- newEmptyMVar
    releaseAbort <- newEmptyMVar
    childEnded <- newEmptyMVar
    owner <- admitted hub (putMVar abortEntered () >> readMVar releaseAbort)
    let write = (putMVar writing () >> readMVar releaseAbort)
          `finally` putMVar childEnded ()
    withAsync (withDeliveryTurn owner (const (controlledWrite owner write))) $ \parent ->
      (do
        within "recoverable retirement writer entered" (takeMVar writing)
        within "recoverable retirement abort began" (takeMVar abortEntered)
        withAsync (cancel parent) $ \canceller -> do
          -- Keep the native abort blocked while cancellation arrives in the
          -- parent's cleanup wait, then permit a real join before its deadline.
          timeout 30000 (wait canceller) `shouldReturn` Nothing
          putMVar releaseAbort ()
          within "recoverable retirement cancellation joined" (wait canceller)
        outcome <- waitCatch parent
        case outcome of
          Left err -> fromException err `shouldBe` Just AsyncCancelled
          Right _ -> expectationFailure "retirement swallowed cancellation"
        tryTakeMVar childEnded `shouldReturn` Just ())
      `finally` void (tryPutMVar releaseAbort ())
    finishDeliveryOwner owner
    awaitDeliveryDrain hub 250000

  it "retains a failed native close and poisons physical admission" $ do
    hub <- newDeliveryHub
    owner <- admitted hub (ioError (userError "test native close failure"))
    abortDeliveryOwner owner `shouldThrow` (== DeliveryUnconfirmed)
    finishDeliveryOwner owner `shouldThrow` (== DeliveryUnconfirmed)
    extra <- newDeliveryOwner hub (pure ())
    reservePhysicalConnection extra "other" `shouldReturn` False
    awaitDeliveryDrain hub 250000 `shouldThrow` (== DeliveryUnconfirmed)

-- Another owner is admitted and completes a turn, then leaves no residue.
assertAdmissionOpen :: DeliveryHub -> IO ()
assertAdmissionOpen hub = do
  extra <- newDeliveryOwner hub (pure ())
  reservePhysicalConnection extra "other" `shouldReturn` True
  withDeliveryTurn extra (const (controlledWrite extra (pure ()))) `shouldReturn` DeliveryCompleted ()
  finishDeliveryOwner extra

admitted :: DeliveryHub -> IO () -> IO DeliveryOwner
admitted hub abort = do
  owner <- newDeliveryOwner hub abort
  reservePhysicalConnection owner "audience" `shouldReturn` True
  pure owner

within :: String -> IO a -> IO a
within phase action = timeout 2000000 action >>= maybe (fail (phase <> " timed out")) pure
