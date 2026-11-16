{-# LANGUAGE RankNTypes #-}

module HMem.Server.WebSocketTransport
  ( NativeTransport
  , NativeConnection
  , newNativeTransport
  , withNativeTransportSettings
  , runNativePlain
  , runPlainSocket
  , runPlainSocketWith
  , currentNativeConnection
  , abortNativeConnection
  , reservePhysicalConnection
  , beginNativeShutdown
  , awaitNativeDrain
  , DeliveryTurn
  , DeliveryResult(..)
  , DeliveryFailure(..)
  , DeliveryCleanupFailure(..)
  , withDeliveryTurn
  , deliveryShouldYield
  , installControlledWriter
  , sendOwnedClose
  ) where

import Control.Concurrent (ThreadId, myThreadId)
import Control.Concurrent.STM
import Control.Exception (IOException, bracket, bracket_, catch, finally, mask_, onException, throwIO)
import Control.Monad (forM_, when)
import Data.Map.Strict qualified as Map
import Data.Streaming.Network (bindPortTCP)
import Data.Text (Text)
import Data.Unique (Unique, newUnique)
import Network.Socket qualified as Socket
import Network.Wai (Application)
import Network.Wai.Handler.Warp.Internal
  ( Connection, Settings(..), runSettingsConnectionMaker, setSocketCloseOnExec, socketConnection )
import Network.WebSockets qualified as WS
import Network.WebSockets.Connection qualified as WSConnection
import System.Timeout (timeout)

import HMem.Server.WebSocketDelivery
  ( DeliveryFailure(..), DeliveryCleanupFailure(..), DeliveryResult(..), DeliveryTurn, deliveryShouldYield )
import HMem.Server.WebSocketDelivery qualified as Delivery

data NativeTransport = NativeTransport
  { acceptedSockets :: !(TVar (Map.Map ThreadId Socket.Socket))
  , startingSockets :: !(TVar (Map.Map Unique Startup))
  , workerSockets :: !(TVar (Map.Map ThreadId NativeConnection))
  , unclosedSockets :: !(TVar [Socket.Socket])
  , deliveryHub :: !Delivery.DeliveryHub
  , runningScopes :: !(TVar Int)
  }

newtype NativeConnection = NativeConnection Delivery.DeliveryOwner

data Startup = Startup !Socket.Socket !(TVar StartupState)

data StartupState = AwaitingFork | ForkReturned | Claimed | Cancelled

newNativeTransport :: IO NativeTransport
newNativeTransport =
  NativeTransport <$> newTVarIO Map.empty <*> newTVarIO Map.empty <*> newTVarIO Map.empty
    <*> newTVarIO [] <*> Delivery.newDeliveryHub <*> newTVarIO 0

runNativePlain :: NativeTransport -> Settings -> Application -> IO ()
runNativePlain transport settings application = Socket.withSocketsDo $
  bracket (bindPortTCP settings.settingsPort settings.settingsHost) Socket.close $ \listener -> do
    setSocketCloseOnExec listener
    withNativeTransportSettings transport settings $ \native ->
      runPlainSocket native listener application

runPlainSocket :: Settings -> Socket.Socket -> Application -> IO ()
runPlainSocket settings = runPlainSocketWith (socketConnection settings) settings

-- Keep Warp's usual accept setup, but defer its Connection allocation until
-- the exact worker claims the socket. A cancelled startup has only a Socket
-- to close, so it cannot strand a preallocated Warp write buffer.
runPlainSocketWith
  :: (Socket.Socket -> IO Connection) -> Settings -> Socket.Socket -> Application -> IO ()
runPlainSocketWith makeConnection settings listener application = do
  settingsInstallShutdownHandler settings (Socket.close listener)
  runSettingsConnectionMaker settings getConnection application
  where
    getConnection = do
      (socket, address) <- settingsAccept settings listener
      setSocketCloseOnExec socket
      -- NoDelay is unavailable for Unix-domain listeners.
      Socket.setSocketOption socket Socket.NoDelay 1 `catch` \(_ :: IOException) -> pure ()
      pure (makeConnection socket, address)

-- Warp and WarpTLS both use these hooks. Keep the caller's accept, fork and
-- unmask behavior, and bind the real socket before its connection maker runs.
withNativeTransportSettings :: NativeTransport -> Settings -> (Settings -> IO a) -> IO a
withNativeTransportSettings transport settings run =
  bracket_ acquireScope releaseScope $
    run settings
      { settingsAccept = trackedAccept
      , settingsFork = trackedFork
      , settingsOnException = \request failure ->
          disposeAccepted transport `finally` settingsOnException settings request failure
      , settingsInstallShutdownHandler = \stopListener ->
          settingsInstallShutdownHandler settings
            (beginNativeShutdown transport `finally` stopListener)
      }
  where
    acquireScope = atomically $ do
      closed <- Delivery.deliveryClosed transport.deliveryHub
      when closed $ throwSTM DeliveryUnconfirmed
      modifyTVar' transport.runningScopes (+ 1)

    releaseScope = do
      beginNativeShutdown transport `finally` disposeUnclaimed transport
      atomically $ modifyTVar' transport.runningScopes (subtract 1)

    trackedAccept listener = mask_ $ do
      disposeAccepted transport
      (socket, address) <- settingsAccept settings listener
      (do
        acceptor <- myThreadId
        atomically $ modifyTVar' transport.acceptedSockets (Map.insert acceptor socket)
        pure (socket, address))
        `onException` closeUnclaimed transport socket

    trackedFork :: ((forall a. IO a -> IO a) -> IO ()) -> IO ()
    trackedFork start = mask_ $ do
      acceptor <- myThreadId
      accepted <- atomically $ takeAccepted transport acceptor
      case accepted of
        Nothing -> settingsFork settings start
        Just socket -> startWorker transport settings acceptor socket start

startWorker
  :: NativeTransport -> Settings -> ThreadId -> Socket.Socket
  -> ((forall a. IO a -> IO a) -> IO ()) -> IO ()
startWorker transport settings acceptor socket start = do
  (key, state, owner) <- (do
    key <- newUnique
    state <- newTVarIO AwaitingFork
    owner <- NativeConnection <$> Delivery.newDeliveryOwner transport.deliveryHub (abortSocket socket)
    atomically $ modifyTVar' transport.startingSockets (Map.insert key (Startup socket state))
    pure (key, state, owner)) `onException` closeUnclaimed transport socket
  let cancel = cancelStartup transport key socket state
      launch :: (forall a. IO a -> IO a) -> IO ()
      launch unmask = mask_ $ do
        worker <- myThreadId
        when (worker == acceptor) $ fail "Warp fork did not create a connection worker"
        claimed <- atomically (claimStartup transport key state worker owner) `onException` cancel
        when claimed $
          start unmask `finally` do
            -- Warp normally closes the connection itself. Also cover an
            -- exception before its connection maker installs that finalizer.
            let NativeConnection deliveryOwner = owner
            Socket.close' socket `onException` Delivery.retainFailedOwner deliveryOwner
            -- Retained delivery work defers this release, with the physical
            -- seat, until its actual handles complete.
            Delivery.finishDeliveryOwnerWith deliveryOwner $
              modifyTVar' transport.workerSockets (Map.delete worker)
  (do
    settingsFork settings launch
    atomically $ readTVar state >>= \case
      AwaitingFork -> writeTVar state ForkReturned
      _ -> pure ())
    `onException` cancel

claimStartup :: NativeTransport -> Unique -> TVar StartupState -> ThreadId -> NativeConnection -> STM Bool
claimStartup transport key state worker owner = readTVar state >>= \case
  AwaitingFork -> retry
  ForkReturned -> do
    writeTVar state Claimed
    modifyTVar' transport.startingSockets (Map.delete key)
    modifyTVar' transport.workerSockets (Map.insert worker owner)
    pure True
  _ -> pure False

cancelStartup :: NativeTransport -> Unique -> Socket.Socket -> TVar StartupState -> IO ()
cancelStartup transport key socket state = mask_ $ do
  cancelled <- atomically $ readTVar state >>= \case
    Claimed -> pure False
    Cancelled -> pure False
    _ -> do
      writeTVar state Cancelled
      modifyTVar' transport.startingSockets (Map.delete key)
      pure True
  when cancelled $ closeUnclaimed transport socket

takeAccepted :: NativeTransport -> ThreadId -> STM (Maybe Socket.Socket)
takeAccepted transport acceptor = do
  accepted <- readTVar transport.acceptedSockets
  writeTVar transport.acceptedSockets (Map.delete acceptor accepted)
  pure (Map.lookup acceptor accepted)

disposeAccepted :: NativeTransport -> IO ()
disposeAccepted transport = mask_ $ do
  acceptor <- myThreadId
  accepted <- atomically $ takeAccepted transport acceptor
  forM_ accepted (closeUnclaimed transport)

disposeUnclaimed :: NativeTransport -> IO ()
disposeUnclaimed transport = mask_ $ do
  sockets <- atomically $ do
    accepted <- swapTVar transport.acceptedSockets Map.empty
    starting <- swapTVar transport.startingSockets Map.empty
    pending <- traverse (\(Startup socket state) -> writeTVar state Cancelled >> pure socket)
      (Map.elems starting)
    pure (Map.elems accepted <> pending)
  foldr (\socket rest -> closeUnclaimed transport socket `finally` rest) (pure ()) sockets

closeUnclaimed :: NativeTransport -> Socket.Socket -> IO ()
closeUnclaimed transport socket =
  Socket.close' socket `onException`
    atomically (modifyTVar' transport.unclosedSockets (socket :))

currentNativeConnection :: NativeTransport -> IO (Maybe NativeConnection)
currentNativeConnection transport = do
  worker <- myThreadId
  Map.lookup worker <$> readTVarIO transport.workerSockets

-- The first caller interrupts the real socket before any driver join. Later
-- callers observe that operation's result, including a failed interruption.
abortNativeConnection :: NativeConnection -> IO ()
abortNativeConnection (NativeConnection owner) = Delivery.abortDeliveryOwner owner

abortSocket :: Socket.Socket -> IO ()
abortSocket socket =
  (Socket.shutdown socket Socket.ShutdownBoth `catch` \(_ :: IOException) -> pure ())
    `finally` Socket.close' socket

reservePhysicalConnection :: NativeConnection -> Text -> IO Bool
reservePhysicalConnection (NativeConnection owner) = Delivery.reservePhysicalConnection owner

withDeliveryTurn :: NativeConnection -> (DeliveryTurn -> IO a) -> IO (DeliveryResult a)
withDeliveryTurn (NativeConnection owner) = Delivery.withDeliveryTurn owner

installControlledWriter :: NativeConnection -> WS.Connection -> WS.Connection
installControlledWriter (NativeConnection owner) original =
  original { WSConnection.connectionWrite = \messages ->
    Delivery.controlledWrite owner (WSConnection.connectionWrite original messages) }

sendOwnedClose :: WS.WebSocketsData a => NativeConnection -> WS.Connection -> a -> IO ()
sendOwnedClose (NativeConnection owner) connection payload =
  Delivery.ownedClose owner (WS.sendClose connection payload)

beginNativeShutdown :: NativeTransport -> IO ()
beginNativeShutdown transport = Delivery.beginDeliveryShutdown transport.deliveryHub

awaitNativeDrain :: NativeTransport -> Int -> IO ()
awaitNativeDrain transport micros = do
  result <- timeout micros $ atomically $ do
    Delivery.deliveryDrained transport.deliveryHub
    unclosed <- readTVar transport.unclosedSockets
    when (not (null unclosed)) $ throwSTM DeliveryUnconfirmed
    active <- readTVar transport.workerSockets
    scopes <- readTVar transport.runningScopes
    check (Map.null active && scopes == 0)
  case result of
    Just () -> pure ()
    Nothing -> throwIO DeliveryUnconfirmed
