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

import Control.Concurrent (MVar, ThreadId, modifyMVar, myThreadId, newMVar, readMVar)
import Control.Concurrent.Async
  ( Async, async, asyncThreadId, asyncWithUnmask, cancel, pollSTM, waitCatch, waitCatchSTM )
import Control.Concurrent.STM
import Control.Exception
  ( Exception, SomeAsyncException, SomeException, fromException, mask, mask_, onException, throwIO, toException, try )
import Control.Monad (forM_, unless, void, when)
import Data.Map.Strict qualified as Map
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
  , physical :: !(Map.Map Unique (Text, DeliveryOwner))
  , turns :: !(Map.Map Unique ActiveTurn)
  , participants :: !(Map.Map ThreadId Participant)
  }

data DeliveryOwner = DeliveryOwner
  { hub :: !DeliveryHub
  , ownerKey :: !Unique
  , closing :: !(TVar Bool)
  , nativeAbort :: !(IO ())
  , abortHandle :: !(MVar (Maybe (Async ())))
  }

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
  , activeQuarantined :: !Bool
  }

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
  DeliveryOwner hub <$> newUnique <*> newTVarIO False <*> pure abort <*> newMVar Nothing

reservePhysicalConnection :: DeliveryOwner -> Text -> IO Bool
reservePhysicalConnection owner audience = atomically $ do
  let DeliveryHub state = owner.hub
  current <- readTVar state
  closing <- readTVar owner.closing
  let admitted = current.physical
      audienceCount = Map.size (Map.filter ((== audience) . fst) admitted)
  if closing || current.admissionClosed || current.poisoned
      || Map.member owner.ownerKey admitted || Map.size admitted >= 128 || audienceCount >= 32
    then pure False
    else do
      writeTVar state current { physical = Map.insert owner.ownerKey (audience, owner) admitted }
      pure True

-- The outer transport worker calls this only after its application children
-- and adapter finalizers have returned and the native socket has been closed.
finishDeliveryOwner :: DeliveryOwner -> IO ()
finishDeliveryOwner owner = finishDeliveryOwnerWith owner (pure ())

finishDeliveryOwnerWith :: DeliveryOwner -> STM () -> IO ()
finishDeliveryOwnerWith owner releaseWorker = mask_ $ do
  atomically $ writeTVar owner.closing True
  let DeliveryHub state = owner.hub
  finished <- try @SomeException $ timeout cleanupMicros $ do
    retained <- atomically $ do
      current <- readTVar state
      let owned = filter ((== owner.ownerKey) . (.activeOwner)) (Map.elems current.turns)
      check (null owned || any (.activeQuarantined) owned)
      pure owned
    abort <- readMVar owner.abortHandle
    forM_ abort $ \handle -> waitCatch handle >>= either throwIO pure
    forM_ retained $ \turn -> do
      forM_ turn.activeCancel (void . waitCatch)
      forM_ turn.activeChild (void . waitCatch)
    -- A later join does not erase the previous unclean outcome or reopen
    -- physical admission on this transport.
    unless (null retained) $ throwIO DeliveryUnconfirmed
  case finished of
    Right (Just ()) -> atomically $ do
      releaseWorker
      modifyTVar' state $ \current ->
        current { physical = Map.delete owner.ownerKey current.physical }
    Left failure -> do
      retainFailedOwner owner
      case fromException failure :: Maybe SomeAsyncException of
        Just _ -> throwIO failure
        Nothing -> throwIO DeliveryUnconfirmed
    Right Nothing -> retainFailedOwner owner >> throwIO DeliveryUnconfirmed

retainFailedOwner :: DeliveryOwner -> IO ()
retainFailedOwner owner = do
  atomically $ writeTVar owner.closing True
  poison owner.hub

-- Mark the owner closing before starting the single retained native abort.
-- Do not wait for that operation while holding the hub registry.
requestDeliveryAbort :: DeliveryOwner -> IO (Async ())
requestDeliveryAbort owner = mask_ $ do
  atomically $ writeTVar owner.closing True
  modifyMVar owner.abortHandle $ \case
    Just existing -> pure (Just existing, existing)
    Nothing -> do
      handle <- async owner.nativeAbort
      pure (Just handle, handle)

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
    current <- readTVar state
    writeTVar state current { admissionClosed = True }
    let owners = map snd (Map.elems current.physical)
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
deliveryClosed (DeliveryHub state) = (.admissionClosed) <$> readTVar state

deliveryDrained :: DeliveryHub -> STM ()
deliveryDrained (DeliveryHub state) = do
  current <- readTVar state
  when current.poisoned $ throwSTM DeliveryUnconfirmed
  check (Map.null current.physical && Map.null current.turns)

poison :: DeliveryHub -> IO ()
poison (DeliveryHub state) = atomically $ modifyTVar' state $ \current ->
  current { poisoned = True, admissionClosed = True }

quarantineTurn :: DeliveryTurn -> IO ()
quarantineTurn turn = do
  let DeliveryHub state = turn.owner.hub
  atomically $ modifyTVar' state $ \current -> current
    { poisoned = True
    , admissionClosed = True
    , turns = Map.adjust (\entry -> entry { activeQuarantined = True }) turn.turnKey current.turns
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
        retire = do
          joined <- try @SomeException (abortAndJoin turn child)
          case joined of
            Right interrupted -> release (Just childId) >> pure interrupted
            Left failure -> do
              quarantineTurn turn
              case fromException failure :: Maybe SomeAsyncException of
                Just _ -> throwIO (DeliveryCleanupFailure failure (toException DeliveryUnconfirmed))
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
        -- join are proven. Preserve an unconfirmed cleanup as the result.
        retired <- try @SomeException retire
        case retired of
          Left cleanupFailure -> throwIO (DeliveryCleanupFailure primary cleanupFailure)
          Right _ -> throwIO primary
      Right (TurnFinished result) -> do
        release (Just childId)
        DeliveryCompleted <$> either throwIO pure result
      Right (TurnRetired failure) -> do
        interrupted <- retire
        case interrupted of
          Just cancellation -> throwIO cancellation
          Nothing -> case failure of
            Just err | Just (_ :: SomeAsyncException) <- fromException err -> throwIO err
            _ -> pure DeliveryRetired

admitTurn :: DeliveryOwner -> Unique -> ThreadId -> STM Bool
admitTurn owner key parent = do
  let DeliveryHub state = owner.hub
  current <- readTVar state
  when (Map.member parent current.participants) $ throwSTM DeliveryOwnershipViolation
  when current.poisoned $ throwSTM DeliveryQuarantined
  closing <- readTVar owner.closing
  if closing || current.admissionClosed || not (Map.member owner.ownerKey current.physical)
    then pure False
    else do
      check (Map.size current.turns < 4)
      writeTVar state current
        { turns = Map.insert key (ActiveTurn owner.ownerKey Nothing Nothing False) current.turns
        , participants = Map.insert parent TurnParent current.participants
        }
      pure True

releaseTurn :: TVar HubState -> Unique -> ThreadId -> Maybe ThreadId -> IO ()
releaseTurn state key parent child = atomically $ modifyTVar' state $ \current ->
  current
    { turns = Map.delete key current.turns
    , participants = Map.delete parent (maybe id Map.delete child current.participants)
    }

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

abortAndJoin :: DeliveryTurn -> Async a -> IO (Maybe SomeException)
abortAndJoin turn child = mask_ $ do
  started <- getMonotonicTimeNSec
  abort <- requestDeliveryAbort turn.owner
  -- The cancellation helper cannot interrupt the child ahead of the first
  -- native operation. Both helpers stay owned by the quarantined turn.
  stopper <- async $ void (waitCatch abort) >> cancel child
  let DeliveryHub state = turn.owner.hub
  atomically $ modifyTVar' state $ \current -> current
    { turns = Map.adjust (\entry -> entry { activeCancel = Just stopper }) turn.turnKey current.turns }
  let deadline = started + fromIntegral cleanupMicros * 1000
      unconfirmed interrupted = case interrupted of
        Just cause -> throwIO (DeliveryCleanupFailure cause (toException DeliveryUnconfirmed))
        Nothing -> throwIO DeliveryUnconfirmed
      await interrupted = do
        now <- getMonotonicTimeNSec
        if now >= deadline then unconfirmed interrupted else do
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
            Right (Just (Right (), Right ())) -> pure interrupted
            _ -> unconfirmed interrupted
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
