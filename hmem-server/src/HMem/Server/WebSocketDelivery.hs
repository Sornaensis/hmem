module HMem.Server.WebSocketDelivery
  ( DeliveryHub
  , DeliveryOwner
  , DeliveryTurn
  , DeliveryResult(..)
  , DeliveryFailure(..)
  , DeliveryCleanupFailure(..)
  , newDeliveryHub
  , newDeliveryOwner
  , reservePhysicalConnection
  , finishDeliveryOwner
  , finishDeliveryOwnerWith
  , retainFailedOwner
  , requestDeliveryAbort
  , abortDeliveryOwner
  , beginDeliveryShutdown
  , deliveryClosed
  , deliveryDrained
  , awaitDeliveryDrain
  , withDeliveryTurn
  , deliveryShouldYield
  , controlledWrite
  , ownedClose
  ) where

import Control.Concurrent (ThreadId, myThreadId)
import Control.Concurrent.Async
  ( Async, async, asyncThreadId, asyncWithUnmask, cancel, pollSTM, waitCatch, waitCatchSTM )
import Control.Concurrent.STM
import Control.Exception
  ( Exception, SomeAsyncException, SomeException, fromException, mask, mask_, onException, throwIO, toException, try
  , uninterruptibleMask_ )
import Control.Monad (forM_, unless, void, when)
import Data.Functor ((<&>))
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Unique (Unique, newUnique)
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import Network.WebSockets qualified as WS
import System.Timeout (timeout)

newtype DeliveryHub = DeliveryHub (TVar HubState)

data HubState = HubState
  { admissionClosed :: !Bool
  , poisoned :: !Bool
  , physical :: !(Map.Map Unique Admission)
  , turns :: !(Map.Map Unique ActiveTurn)
  , participants :: !(Map.Map ThreadId Participant)
  }

-- An admitted owner keeps its seat until its outer worker has finished and
-- every retained turn and its native abort have actually completed.
data Admission = Admission
  { admittedAudience :: !Text
  , admittedOwner :: !DeliveryOwner
  , finishedWorker :: !(Maybe (STM ()))
  }

data DeliveryOwner = DeliveryOwner
  { hub :: !DeliveryHub
  , ownerKey :: !Unique
  , closing :: !(TVar Bool)
  , nativeAbort :: !(IO ())
  , abortState :: !(TVar AbortState)
  }

data AbortState = AbortIdle | AbortRequesting | AbortStarted !(Async ())

data DeliveryTurn = DeliveryTurn
  { owner :: !DeliveryOwner
  , turnKey :: !Unique
  , workDeadline :: !Word64
  , writePhase :: !(TVar WritePhase)
  }

data ActiveTurn = ActiveTurn
  { activeOwner :: !Unique
  , activeChild :: !(Maybe (Async ()))
  , activeCancel :: !(Maybe (Async ()))
  , activeAbort :: !(Maybe (Async ()))
  , activeQuarantined :: !Bool
  }

-- Completion of retained work. A failed native abort or cancellation helper
-- stays retained and poisons admission.
data Settlement = Settled | Pending | Failed
  deriving (Eq, Ord)

data Cleanup = Joined | Unfinished | AbortFailed

data Participant = TurnParent | TurnChild !DeliveryTurn

data WritePhase
  = Idle !Word64
  | Writing !Word64 !Word64
  | Expired
  | WriteFailed !SomeException

data DeliveryResult a = DeliveryCompleted a | DeliveryRetired
  deriving (Show, Eq)

data DeliveryFailure
  = DeliveryOwnershipViolation
  | DeliveryWriteTimedOut
  | DeliveryQuarantined
  | DeliveryUnconfirmed
  deriving (Show, Eq)

instance Exception DeliveryFailure

-- A failed cancellation cleanup must remain fatal while retaining the
-- original asynchronous cause for its supervisor.
data DeliveryCleanupFailure = DeliveryCleanupFailure
  { originalCause :: !SomeException
  , cleanupCause :: !SomeException
  } deriving (Show)

instance Exception DeliveryCleanupFailure

newDeliveryHub :: IO DeliveryHub
newDeliveryHub = DeliveryHub <$> newTVarIO (HubState False False Map.empty Map.empty Map.empty)

newDeliveryOwner :: DeliveryHub -> IO () -> IO DeliveryOwner
newDeliveryOwner hub abort =
  DeliveryOwner hub <$> newUnique <*> newTVarIO False <*> pure abort <*> newTVarIO AbortIdle

reservePhysicalConnection :: DeliveryOwner -> Text -> IO Bool
reservePhysicalConnection owner audience = atomically $ do
  let DeliveryHub state = owner.hub
  current <- readHub state
  closing <- readTVar owner.closing
  let admitted = current.physical
      audienceCount = Map.size (Map.filter ((== audience) . (.admittedAudience)) admitted)
  if closing || current.admissionClosed || current.poisoned
      || Map.member owner.ownerKey admitted || Map.size admitted >= 128 || audienceCount >= 32
    then pure False
    else do
      writeTVar state current { physical = Map.insert owner.ownerKey (Admission audience owner Nothing) admitted }
      pure True

-- The outer transport worker calls this only after its application children
-- and adapter finalizers have returned and the native socket has been closed.
finishDeliveryOwner :: DeliveryOwner -> IO ()
finishDeliveryOwner owner = finishDeliveryOwnerWith owner (pure ())

-- Record outer-worker completion. The seat and the worker release are
-- reclaimed together, exactly once, when the owner's turns and native abort
-- have actually completed; retained work does not hold the worker here.
finishDeliveryOwnerWith :: DeliveryOwner -> STM () -> IO ()
finishDeliveryOwnerWith owner releaseWorker = mask_ $ do
  let DeliveryHub state = owner.hub
  abortFailed <- atomically $ do
    writeTVar owner.closing True
    abort <- readTVar owner.abortState >>= abortSettlement
    current <- readTVar state
    case Map.lookup owner.ownerKey current.physical of
      Just admission -> writeTVar state current
        { physical = Map.insert owner.ownerKey admission { finishedWorker = Just releaseWorker } current.physical }
      -- An owner that was never admitted has no turns or requested abort.
      Nothing -> when (abort /= Failed) releaseWorker
    reclaimRetained state
    pure (abort == Failed)
  when abortFailed $ retainFailedOwner owner >> throwIO DeliveryUnconfirmed

retainFailedOwner :: DeliveryOwner -> IO ()
retainFailedOwner owner = do
  atomically $ writeTVar owner.closing True
  poison owner.hub

-- Mark the owner closing before starting the single retained native abort.
-- Do not wait for that operation while holding the hub registry.
requestDeliveryAbort :: DeliveryOwner -> IO (Async ())
requestDeliveryAbort owner = mask_ $ do
  -- A concurrent request holds its claim only while forking the operation,
  -- so this wait cannot strand the caller's cleanup without actual handles.
  claimed <- uninterruptibleMask_ $ atomically $ do
    writeTVar owner.closing True
    readTVar owner.abortState >>= \case
      AbortStarted existing -> pure (Just existing)
      AbortRequesting -> retry
      AbortIdle -> Nothing <$ writeTVar owner.abortState AbortRequesting
  case claimed of
    Just existing -> pure existing
    Nothing -> do
      handle <- async owner.nativeAbort
        `onException` atomically (writeTVar owner.abortState AbortIdle)
      atomically $ writeTVar owner.abortState (AbortStarted handle)
      pure handle

abortDeliveryOwner :: DeliveryOwner -> IO ()
abortDeliveryOwner owner = do
  handle <- requestDeliveryAbort owner
  outcome <- timeout cleanupMicros (waitCatch handle)
  case outcome of
    Just (Right ()) -> pure ()
    _ -> retainFailedOwner owner >> throwIO DeliveryUnconfirmed

beginDeliveryShutdown :: DeliveryHub -> IO ()
beginDeliveryShutdown (DeliveryHub state) = mask_ $ do
  owners <- atomically $ do
    current <- readHub state
    writeTVar state current { admissionClosed = True }
    -- A finished worker has already closed its socket; it awaits reclamation.
    let owners = [admission.admittedOwner | admission <- Map.elems current.physical, null admission.finishedWorker]
    forM_ owners $ \owner -> writeTVar owner.closing True
    pure owners
  forM_ owners (void . requestDeliveryAbort)

awaitDeliveryDrain :: DeliveryHub -> Int -> IO ()
awaitDeliveryDrain hub micros = do
  drained <- timeout micros (atomically (deliveryDrained hub))
  case drained of
    Just () -> pure ()
    Nothing -> throwIO DeliveryUnconfirmed

deliveryClosed :: DeliveryHub -> STM Bool
deliveryClosed (DeliveryHub state) = (.admissionClosed) <$> readHub state

deliveryDrained :: DeliveryHub -> STM ()
deliveryDrained (DeliveryHub state) = do
  current <- readHub state
  when current.poisoned $ throwSTM DeliveryUnconfirmed
  check (Map.null current.physical && Map.null current.turns)

poison :: DeliveryHub -> IO ()
poison (DeliveryHub state) = atomically $ modifyTVar' state $ \current ->
  current { poisoned = True, admissionClosed = True }

-- Every hub reader first reclaims retained work whose actual handles have
-- completed. Waiting readers retry when any observed handle completes, so no
-- polling timer or extra thread is needed. Reclamation deletes the entry in
-- the same transaction that releases it, so each release happens once.
readHub :: TVar HubState -> STM HubState
readHub state = reclaimRetained state >> readTVar state

reclaimRetained :: TVar HubState -> STM ()
reclaimRetained state = do
  current <- readTVar state
  turnStates <- traverse turnSettlement (Map.filter (.activeQuarantined) current.turns)
  let doneTurns = Map.filter (== Settled) turnStates
      reclaimed = Map.intersection current.turns doneTurns
      remaining = Map.difference current.turns doneTurns
      busy = map (.activeOwner) (Map.elems remaining)
      finished = Map.filterWithKey
        (\key admission -> isJust admission.finishedWorker && key `notElem` busy) current.physical
  seatStates <- traverse (\admission -> readTVar admission.admittedOwner.abortState >>= abortSettlement) finished
  let doneSeats = Map.filter (== Settled) seatStates
      released = Map.intersection finished doneSeats
      failed = Failed `elem` Map.elems turnStates || Failed `elem` Map.elems seatStates
  forM_ released $ \admission -> sequence_ admission.finishedWorker
  unless (Map.null reclaimed && Map.null released && (not failed || current.poisoned)) $
    writeTVar state current
      { turns = remaining
      , physical = Map.difference current.physical released
      , participants = foldr Map.delete current.participants
          [asyncThreadId child | entry <- Map.elems reclaimed, Just child <- [entry.activeChild]]
      , poisoned = current.poisoned || failed
      , admissionClosed = current.admissionClosed || failed
      }

turnSettlement :: ActiveTurn -> STM Settlement
turnSettlement entry = do
  -- The child's own failure is the expected result of its cancellation.
  child <- maybe (pure Settled) (fmap (maybe Pending (const Settled)) . pollSTM) entry.activeChild
  cancelled <- maybe (pure Settled) handleSettlement entry.activeCancel
  native <- maybe (pure Settled) handleSettlement entry.activeAbort
  pure (maximum [child, cancelled, native])

abortSettlement :: AbortState -> STM Settlement
abortSettlement = \case
  AbortIdle -> pure Settled
  AbortRequesting -> pure Pending
  AbortStarted handle -> handleSettlement handle

handleSettlement :: Async a -> STM Settlement
handleSettlement handle = pollSTM handle <&> \case
  Nothing -> Pending
  Just (Right _) -> Settled
  Just (Left _) -> Failed

-- An unfinished join retains the turn's lease and handles for reclamation but
-- does not stop other admission. Its parent leaves; its child stays fenced.
quarantineTurn :: DeliveryTurn -> ThreadId -> Bool -> IO ()
quarantineTurn turn parent failed = do
  let DeliveryHub state = turn.owner.hub
  atomically $ modifyTVar' state $ \current -> current
    { poisoned = current.poisoned || failed
    , admissionClosed = current.admissionClosed || failed
    , turns = Map.adjust (\entry -> entry { activeQuarantined = True }) turn.turnKey current.turns
    , participants = Map.delete parent current.participants
    }

withDeliveryTurn :: DeliveryOwner -> (DeliveryTurn -> IO a) -> IO (DeliveryResult a)
withDeliveryTurn owner action = mask $ \restore -> do
  parent <- myThreadId
  key <- newUnique
  phase <- newTVarIO (Idle 0)
  gate <- newEmptyTMVarIO
  let DeliveryHub state = owner.hub
      release child = releaseTurn state key parent child
  admitted <- atomically $ admitTurn owner key parent
  if not admitted then pure DeliveryRetired else do
    started <- getMonotonicTimeNSec
    let turn = DeliveryTurn owner key (started + workNanos) phase
    child <- (asyncWithUnmask $ \unmask -> do
      run <- atomically (readTMVar gate)
      result <- if run
        then try (unmask (action turn))
        else pure (Left (toException WS.ConnectionClosed))
      ended <- getMonotonicTimeNSec
      pure (ended, result))
      `onException` release Nothing
    let childId = asyncThreadId child
        unconfirmed :: Maybe SomeException -> IO b
        unconfirmed = \case
          Just cause -> throwIO (DeliveryCleanupFailure cause (toException DeliveryUnconfirmed))
          Nothing -> throwIO DeliveryUnconfirmed
        -- Returns any cancellation received during cleanup and whether the
        -- join remains unfinished. Only a failed native abort or cleanup
        -- startup poisons admission.
        retire = do
          joined <- try @SomeException (abortAndJoin turn child)
          case joined of
            Right (interrupted, Joined) -> release (Just childId) >> pure (interrupted, False)
            Right (interrupted, Unfinished) -> quarantineTurn turn parent False >> pure (interrupted, True)
            Right (interrupted, AbortFailed) -> quarantineTurn turn parent True >> unconfirmed interrupted
            Left failure -> do
              quarantineTurn turn parent True
              case fromException failure :: Maybe SomeAsyncException of
                Just _ -> unconfirmed (Just failure)
                Nothing -> throwIO failure
    atomically $ do
      modifyTVar' state $ \current -> current
        { turns = Map.adjust (\entry -> entry { activeChild = Just (void child) }) key current.turns
        , participants = Map.insert childId (TurnChild turn) current.participants
        }
      closing <- readTVar owner.closing
      putTMVar gate (not closing)
    observed <- try (restore (monitorTurn turn child))
    case observed of
      Left (primary :: SomeException) -> do
        -- Cancellation is clean only after the owned native abort and child
        -- join are proven. Annotate an unconfirmed cleanup on the cause.
        retired <- try @SomeException retire
        case retired of
          Left cleanupFailure -> throwIO (DeliveryCleanupFailure primary cleanupFailure)
          Right (_, True) -> throwIO (DeliveryCleanupFailure primary (toException DeliveryUnconfirmed))
          Right (_, False) -> throwIO primary
      Right (TurnFinished result) -> do
        release (Just childId)
        DeliveryCompleted <$> either throwIO pure result
      Right (TurnRetired failure) -> do
        (interrupted, unfinished) <- retire
        case interrupted of
          Just cancellation
            | unfinished -> unconfirmed interrupted
            | otherwise -> throwIO cancellation
          -- An unfinished join is a per-connection retirement.
          Nothing -> case failure of
            Just err | Just (_ :: SomeAsyncException) <- fromException err -> throwIO err
            _ -> pure DeliveryRetired

admitTurn :: DeliveryOwner -> Unique -> ThreadId -> STM Bool
admitTurn owner key parent = do
  let DeliveryHub state = owner.hub
  current <- readHub state
  when (Map.member parent current.participants) $ throwSTM DeliveryOwnershipViolation
  when current.poisoned $ throwSTM DeliveryQuarantined
  closing <- readTVar owner.closing
  if closing || current.admissionClosed || not (Map.member owner.ownerKey current.physical)
    then pure False
    else do
      -- Retained turns keep their leases until reclaimed.
      check (Map.size current.turns < 4)
      writeTVar state current
        { turns = Map.insert key (ActiveTurn owner.ownerKey Nothing Nothing Nothing False) current.turns
        , participants = Map.insert parent TurnParent current.participants
        }
      pure True

releaseTurn :: TVar HubState -> Unique -> ThreadId -> Maybe ThreadId -> IO ()
releaseTurn state key parent child = atomically $ do
  modifyTVar' state $ \current -> current
    { turns = Map.delete key current.turns
    , participants = Map.delete parent (maybe id Map.delete child current.participants)
    }
  -- The owner's finished worker may have been waiting only for this turn.
  reclaimRetained state

data TurnOutcome a = TurnFinished !(Either SomeException a) | TurnRetired !(Maybe SomeException)

monitorTurn :: DeliveryTurn -> Async (Word64, Either SomeException a) -> IO (TurnOutcome a)
monitorTurn turn child = go
  where
    go = do
      observed <- atomically $ do
        closing <- readTVar turn.owner.closing
        phase <- readTVar turn.writePhase
        finished <- pollSTM child
        pure (closing, phase, finished)
      case observed of
        (_, WriteFailed err, _) -> pure (TurnRetired (Just err))
        (_, Expired, _) -> pure (TurnRetired Nothing)
        (True, _, _) -> pure (TurnRetired Nothing)
        (_, _, Just (Right (ended, result)))
          | ended < turn.workDeadline -> pure (TurnFinished result)
          | otherwise -> pure (TurnRetired (either Just (const Nothing) result))
        (_, _, Just (Left err)) -> pure (TurnRetired (Just err))
        (_, phase, Nothing) -> do
          now <- getMonotonicTimeNSec
          let deadline = case phase of
                Writing _ writeDeadline -> min turn.workDeadline writeDeadline
                _ -> turn.workDeadline
          if now >= deadline
            then atomically $ do
              current <- readTVar turn.writePhase
              finished <- pollSTM child
              case finished of
                Nothing
                  | samePhase phase current && now >= deadline ->
                      writeTVar turn.writePhase Expired
                _ -> pure ()
            else void $ timeout (nanosToMicros (deadline - now)) $ atomically $
              (void $ waitCatchSTM child)
                `orElse` (readTVar turn.owner.closing >>= check)
                `orElse` (readTVar turn.writePhase >>= check . not . samePhase phase)
          go

samePhase :: WritePhase -> WritePhase -> Bool
samePhase (Idle a) (Idle b) = a == b
samePhase (Writing a deadlineA) (Writing b deadlineB) = a == b && deadlineA == deadlineB
samePhase _ _ = False

abortAndJoin :: DeliveryTurn -> Async a -> IO (Maybe SomeException, Cleanup)
abortAndJoin turn child = mask_ $ do
  started <- getMonotonicTimeNSec
  abort <- requestDeliveryAbort turn.owner
  -- The cancellation helper cannot interrupt the child ahead of the first
  -- native operation. Both helpers stay owned by a quarantined turn.
  stopper <- async $ void (waitCatch abort) >> cancel child
  let DeliveryHub state = turn.owner.hub
  atomically $ modifyTVar' state $ \current -> current
    { turns = Map.adjust (\entry -> entry { activeCancel = Just stopper, activeAbort = Just abort })
        turn.turnKey current.turns }
  let deadline = started + fromIntegral cleanupMicros * 1000
      await interrupted = do
        now <- getMonotonicTimeNSec
        if now >= deadline then pure (interrupted, Unfinished) else do
          -- timeout unregisters its scoped timer on interruption. Rearming
          -- uses the unchanged absolute deadline, never a fresh cleanup budget.
          joined <- try @SomeAsyncException $ timeout (nanosToMicros (deadline - now)) $
            atomically $ do
              native <- waitCatchSTM abort
              cancelled <- waitCatchSTM stopper
              _ <- waitCatchSTM child
              pure (native, cancelled)
          case joined of
            Left cancellation -> await $ case interrupted of
              Just original -> Just original
              Nothing -> Just (toException cancellation)
            Right (Just (Right (), Right ())) -> pure (interrupted, Joined)
            Right (Just _) -> pure (interrupted, AbortFailed)
            Right Nothing -> pure (interrupted, Unfinished)
  await Nothing

-- Called only at a completed-record boundary. Leave one full write budget
-- before the absolute deadline to avoid starting a record during cleanup.
deliveryShouldYield :: DeliveryTurn -> IO Bool
deliveryShouldYield turn = do
  now <- getMonotonicTimeNSec
  closing <- readTVarIO turn.owner.closing
  pure (closing || now + writeNanos >= turn.workDeadline)

controlledWrite :: DeliveryOwner -> IO a -> IO a
controlledWrite owner write = lookupParticipant owner >>= \case
  Just (TurnChild turn) | turn.owner.ownerKey == owner.ownerKey -> writeInTurn turn write
  Just _ -> throwIO DeliveryOwnershipViolation
  Nothing -> withDeliveryTurn owner (\turn -> writeInTurn turn write) >>= resultOrClosed

-- Enter a lease before calling sendClose, since the library sets its Close
-- flag before reaching the framed writer. A foreign participant cannot do so.
ownedClose :: DeliveryOwner -> IO a -> IO a
ownedClose owner close = lookupParticipant owner >>= \case
  Just (TurnChild turn) | turn.owner.ownerKey == owner.ownerKey -> do
    closing <- readTVarIO owner.closing
    when closing $ throwIO WS.ConnectionClosed
    close
  Just _ -> throwIO DeliveryOwnershipViolation
  Nothing -> withDeliveryTurn owner (const close) >>= resultOrClosed

lookupParticipant :: DeliveryOwner -> IO (Maybe Participant)
lookupParticipant owner = do
  thread <- myThreadId
  let DeliveryHub state = owner.hub
  Map.lookup thread . (.participants) <$> readTVarIO state

resultOrClosed :: DeliveryResult a -> IO a
resultOrClosed = \case
  DeliveryCompleted value -> pure value
  DeliveryRetired -> throwIO WS.ConnectionClosed

writeInTurn :: DeliveryTurn -> IO a -> IO a
writeInTurn turn write = mask $ \restore -> do
  start <- getMonotonicTimeNSec
  generation <- atomically $ do
    closing <- readTVar turn.owner.closing
    when closing $ throwSTM WS.ConnectionClosed
    phase <- readTVar turn.writePhase
    case phase of
      Idle previous
        | start < turn.workDeadline -> do
            let generation = previous + 1
            writeTVar turn.writePhase (Writing generation (start + writeNanos))
            pure generation
      Expired -> throwSTM DeliveryWriteTimedOut
      Idle _ -> throwSTM DeliveryWriteTimedOut
      _ -> throwSTM DeliveryOwnershipViolation
  result <- try @SomeException (restore write)
  ended <- getMonotonicTimeNSec
  withinBudget <- atomically $ do
    phase <- readTVar turn.writePhase
    case phase of
      Writing active deadline | active == generation -> do
        let withinBudget = ended < min deadline turn.workDeadline
        writeTVar turn.writePhase $
          if not withinBudget then Expired else case result of
            Left err -> WriteFailed err
            Right _ -> Idle generation
        pure withinBudget
      _ -> pure False
  value <- either throwIO pure result
  unless withinBudget $ throwIO DeliveryWriteTimedOut
  pure value

workNanos, writeNanos :: Word64
workNanos = 750000000
writeNanos = 250000000

cleanupMicros :: Int
cleanupMicros = 250000

nanosToMicros :: Word64 -> Int
nanosToMicros nanos = fromIntegral ((nanos + 999) `div` 1000)
