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
  ) where

import Control.Concurrent (MVar, ThreadId, modifyMVarMasked, myThreadId, newMVar)
import Control.Concurrent.STM
import Control.Exception (IOException, SomeException, bracket, catch, finally, mask_, onException, throwIO, try)
import Control.Monad (forM_, when)
import Data.Map.Strict qualified as Map
import Data.Streaming.Network (bindPortTCP)
import Data.Unique (Unique, newUnique)
import Network.Socket qualified as Socket
import Network.Wai (Application)
import Network.Wai.Handler.Warp.Internal
  ( Connection, Settings(..), runSettingsConnectionMaker, setSocketCloseOnExec, socketConnection )

data NativeTransport = NativeTransport
  { acceptedSockets :: !(TVar (Map.Map ThreadId Socket.Socket))
  , startingSockets :: !(TVar (Map.Map Unique Startup))
  , workerSockets :: !(TVar (Map.Map ThreadId NativeConnection))
  , unclosedSockets :: !(TVar [Socket.Socket])
  }

data NativeConnection = NativeConnection
  { nativeSocket :: !Socket.Socket
  , abortResult :: !(MVar (Maybe (Either SomeException ())))
  }

data Startup = Startup !Socket.Socket !(TVar StartupState)

data StartupState = AwaitingFork | ForkReturned | Claimed | Cancelled

newNativeTransport :: IO NativeTransport
newNativeTransport =
  NativeTransport <$> newTVarIO Map.empty <*> newTVarIO Map.empty <*> newTVarIO Map.empty
    <*> newTVarIO []

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
  run settings
    { settingsAccept = trackedAccept
    , settingsFork = trackedFork
    , settingsOnException = \request failure ->
        disposeAccepted transport `finally` settingsOnException settings request failure
    } `finally` disposeUnclaimed transport
  where
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
    owner <- NativeConnection socket <$> newMVar Nothing
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
            Socket.close' socket
            atomically $ modifyTVar' transport.workerSockets (Map.delete worker)
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
abortNativeConnection owner = do
  result <- modifyMVarMasked owner.abortResult $ \case
    Just previous -> pure (Just previous, previous)
    Nothing -> do
      interrupted <- try $ do
        Socket.shutdown owner.nativeSocket Socket.ShutdownBoth
          `catch` \(_ :: IOException) -> pure ()
        -- On Windows, shutdown alone does not cancel an already pending
        -- overlapped receive. Close the same accepted Socket before joining.
        Socket.close' owner.nativeSocket
      pure (Just interrupted, interrupted)
  either throwIO pure result
