{-# LANGUAGE ForeignFunctionInterface #-}
-- | A slot remains owned until the native child and its pipe owner have exited.
module HMem.MCP.SessionProcess
  ( ProcessScope, ProcessFault(..), newProcessScope, newProcessScopeWithReapHook, closeProcessScope, cancelProcessScope, scopePoisoned
  , runHelper, processMetrics, nativeProcessCount, processAlive, watchParent
  , frameHeader, parseFrameHeader, frameCap
  , attributeFailureProbe
  , killProcessId, childProcessIds, processImage, blockChildProbe
  ) where

import Control.Concurrent
import Control.Concurrent.Async (async, asyncBound, cancel, waitCatch, waitCatchSTM)
import Control.Concurrent.STM
import Control.Exception
import Control.Monad (forM_, unless, void, when)
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Word (Word8, Word64)
import Foreign (Ptr, alloca, allocaBytes, castPtr, nullPtr, peek, peekArray, plusPtr, poke)
import Foreign.C
import GHC.Clock (getMonotonicTimeNSec)
import System.Directory (canonicalizePath, doesFileExist)
import System.FilePath (isAbsolute)
import System.Timeout (timeout)

data NativeProcess
foreign import ccall safe "hmem_session_process_spawn" spawnNative :: CString -> Ptr (Ptr NativeProcess) -> IO CInt
foreign import ccall safe "hmem_session_process_kill" killNative :: Ptr NativeProcess -> IO CInt
foreign import ccall safe "hmem_session_process_wait" waitNative :: Ptr NativeProcess -> CInt -> IO CInt
foreign import ccall unsafe "hmem_session_process_destroy" destroyNative :: Ptr NativeProcess -> IO CInt
foreign import ccall safe "hmem_session_process_write" writeNative :: Ptr NativeProcess -> Ptr Word8 -> CInt -> IO CInt
foreign import ccall safe "hmem_session_process_read" readNative :: Ptr NativeProcess -> Ptr Word8 -> CInt -> IO CInt
foreign import ccall safe "hmem_session_process_close_input" closeInput :: Ptr NativeProcess -> IO CInt
foreign import ccall unsafe "hmem_session_process_pid" nativePid :: Ptr NativeProcess -> IO CInt
foreign import ccall unsafe "hmem_session_process_count" countNative :: IO CInt
foreign import ccall safe "hmem_session_process_alive" aliveNative :: CInt -> IO CInt
foreign import ccall safe "hmem_session_child_watch_parent" watchParent :: IO CInt
foreign import ccall safe "hmem_session_process_attribute_probe" probeNative :: CString -> IO CInt
foreign import ccall safe "hmem_session_process_kill_pid" killPidNative :: CInt -> IO CInt
foreign import ccall safe "hmem_session_process_children" childrenNative :: CInt -> Ptr CInt -> CInt -> IO CInt
foreign import ccall safe "hmem_session_process_image" imageNative :: CInt -> CString -> CInt -> IO CInt
foreign import ccall safe "hmem_session_child_probe_block" blockChildProbe :: IO ()

killProcessId :: Int -> IO Bool
killProcessId pid = (/= 0) <$> killPidNative (fromIntegral pid)
childProcessIds :: Int -> IO [Int]
childProcessIds pid = allocaBytes (32*4) $ \raw -> do
  count <- fromIntegral <$> childrenNative (fromIntegral pid) raw 32
  if count < 0 then throwIO ProcessUnavailable else map fromIntegral <$> peekArray count raw
processImage :: Int -> IO FilePath
processImage pid = allocaBytes 131072 $ \raw -> do
  size <- imageNative (fromIntegral pid) raw 131072
  if size <= 0 then throwIO ProcessUnavailable else T.unpack . TE.decodeUtf8 <$> BS.packCStringLen (raw,fromIntegral size)

attributeFailureProbe :: FilePath -> IO Int
attributeFailureProbe path = BS.useAsCString (TE.encodeUtf8 (T.pack path)) (fmap fromIntegral . probeNative)

data ProcessFault = ProcessUnavailable | ProcessProtocol | ProcessDeadline | ProcessOwnership
  deriving (Eq, Show)
instance Exception ProcessFault
data Lease = Lease { cancelled :: TVar Bool, result :: MVar (Either ProcessFault BS.ByteString) }
data ProcessScope = ProcessScope
  { leases :: TVar (Map.Map Int Lease), sequenceNumber :: TVar Int, poisoned :: TVar Bool, pipes :: TVar Int, closing :: TVar Bool, afterReap :: IO () }

newProcessScope :: IO ProcessScope
newProcessScope = newProcessScopeWithReapHook (pure ())
newProcessScopeWithReapHook :: IO () -> IO ProcessScope
newProcessScopeWithReapHook hook = ProcessScope <$> newTVarIO Map.empty <*> newTVarIO 0 <*> newTVarIO False <*> newTVarIO 0 <*> newTVarIO False <*> pure hook
scopePoisoned :: ProcessScope -> STM Bool
scopePoisoned scope = readTVar scope.poisoned
nativeProcessCount :: IO Int
nativeProcessCount = fromIntegral <$> countNative
processAlive :: Int -> IO Bool
processAlive pid = (/= 0) <$> aliveNative (fromIntegral pid)
processMetrics :: ProcessScope -> IO (Int,Int,Bool)
processMetrics scope = atomically $ (,,) <$> (Map.size <$> readTVar scope.leases) <*> readTVar scope.pipes <*> readTVar scope.poisoned

closeProcessScope :: ProcessScope -> Int -> IO Bool
closeProcessScope scope budget = do
  atomically $ cancelProcessScope scope
  completed <- timeout budget $ atomically $ do
    remaining <- readTVar scope.leases; n <- readTVar scope.pipes
    check (Map.null remaining && n == 0)
  pure $ maybe False (const True) completed

cancelProcessScope :: ProcessScope -> STM ()
cancelProcessScope scope = do
  writeTVar scope.closing True
  readTVar scope.leases >>= mapM_ (\lease -> writeTVar lease.cancelled True)

-- Five seconds includes startup, readiness, HTTP, bounded IPC and cleanup.
-- One second is reserved for kill/reap and pipe-owner join. If the OS fails to
-- acknowledge ownership, return a fatal result and quarantine the lease. Its
-- bound creator remains alive and keeps attempting cleanup; admission is poisoned.
runHelper :: ProcessScope -> FilePath -> (Int -> IO ()) -> (T.Text -> IO ()) -> BS.ByteString -> IO (Either ProcessFault BS.ByteString)
runHelper scope executable onSpawn onPhase payload = mask $ \restore -> do
  start <- getMonotonicTimeNSec
  let deadline = start + 5000000000
      cutoff = deadline - 1000000000
  valid <- if isAbsolute executable then doesFileExist executable else pure False
  canonical <- if valid then canonicalizePath executable else pure ""
  if not valid || canonical /= executable || BS.length payload > frameCap then pure (Left ProcessUnavailable) else do
    cancelled <- newTVarIO False; result <- newEmptyMVar
    let lease = Lease cancelled result
    key <- atomically $ do
      poisoned <- readTVar scope.poisoned
      when poisoned $ throwSTM ProcessOwnership
      closing <- readTVar scope.closing
      when closing $ throwSTM ProcessUnavailable
      key <- readTVar scope.sequenceNumber
      writeTVar scope.sequenceNumber (key+1)
      modifyTVar' scope.leases (Map.insert key lease)
      pure key
    let finish value = do
          atomically $ modifyTVar' scope.leases (Map.delete key)
          putMVar result value
        cancelAndWait = do
          atomically $ writeTVar cancelled True
          left <- remaining deadline
          ready <- tryReadMVar result
          done <- case ready of Just value -> pure (Just value); Nothing -> timeout left (readMVar result)
          case done of
            Just answer -> pure answer
            Nothing -> atomically (writeTVar scope.poisoned True) >> throwIO ProcessOwnership
    _ <- asyncBound (ownProcess scope lease executable cutoff onSpawn onPhase payload >>= finish)
      `onException` atomically (modifyTVar' scope.leases (Map.delete key))
    micros <- remaining cutoff
    observed <- restore (timeout micros (readMVar result)) `onException` void cancelAndWait
    case observed of
      Just answer -> do
        left <- remaining deadline
        pure $ if left <= 0 then Left ProcessDeadline else answer
      Nothing -> cancelAndWait >>= \_ -> pure (Left ProcessDeadline)

ownProcess :: ProcessScope -> Lease -> FilePath -> Word64 -> (Int -> IO ()) -> (T.Text -> IO ()) -> BS.ByteString -> IO (Either ProcessFault BS.ByteString)
ownProcess scope lease executable cutoff onSpawn onPhase payload = mask $ \restore -> do
  spawned <- BS.useAsCString (TE.encodeUtf8 (T.pack executable)) $ \path -> alloca $ \out -> do
    poke out nullPtr
    code <- spawnNative path out
    pointer <- peek out
    pure (code,pointer)
  case spawned of
    (_,pointer) | pointer == nullPtr -> pure (Left ProcessUnavailable)
    (code,pointer) -> do
      atomically $ modifyTVar' scope.pipes (+1)
      let reap = do
            _ <- killNative pointer
            exited <- waitNative pointer 100
            unless (exited == 1) (threadDelay 1000 >> reap)
          destroy = do
            destroyed <- destroyNative pointer
            unless (destroyed == 1) $ throwIO ProcessOwnership
      worker <- (async $ restore (do
        pid <- fromIntegral <$> nativePid pointer
        onSpawn pid
        ready <- receiveFrame pointer
        unless (ready == "HMSESSION1") $ throwIO ProcessProtocol
        sendFrame pointer payload
        _ <- closeInput pointer
        let receive count = do
              answer <- receiveFrame pointer
              if "PHASE:" `BS.isPrefixOf` answer && BS.length answer <= 64
                then do
                  when (count >= (8 :: Int)) $ throwIO ProcessProtocol
                  onPhase (TE.decodeUtf8 (BS.drop 6 answer)); receive (count+1)
                else BS.length answer `seq` pure answer
        receive 0) `finally` atomically (modifyTVar' scope.pipes (subtract 1)))
        `onException` (atomically (modifyTVar' scope.pipes (subtract 1)) >> reap >> destroy)
      let waiting = do
            cancel <- readTVar lease.cancelled
            if cancel then pure Nothing else Just <$> waitCatchSTM worker
      left <- remaining cutoff
      observed <- if code /= 1 then pure Nothing else restore (timeout left (atomically waiting)) >>= pure . joinMaybe
      -- Native termination always precedes touching a potentially blocked pipe
      -- owner. Keep the bound creator and native pointer until both are confirmed.
      reap
      hook <- try @SomeException scope.afterReap
      -- Child death makes its private read/write pipe operations return EOF/error.
      cancel worker
      _ <- waitCatch worker
      destroy
      pure $ case (hook,observed) of
        (Left _,_) -> Left ProcessProtocol
        (_,Just (Right bytes)) -> Right bytes
        (_,Just (Left exception)) -> case fromException exception of Just fault -> Left fault; Nothing -> Left ProcessProtocol
        (_,Nothing) -> Left ProcessDeadline
  where
    joinMaybe (Just value) = value
    joinMaybe Nothing = Nothing

remaining :: Word64 -> IO Int
remaining deadline = do
  time <- getMonotonicTimeNSec
  pure $ if time >= deadline then 0 else fromIntegral ((deadline-time) `div` 1000)

frameCap :: Int
frameCap = 4194304
frameHeader :: Int -> BS.ByteString
frameHeader size = BS.pack [fromIntegral (size `div` 16777216),fromIntegral (size `div` 65536),fromIntegral (size `div` 256),fromIntegral size]
parseFrameHeader :: BS.ByteString -> Maybe Int
parseFrameHeader bytes
  | BS.length bytes /= 4 = Nothing
  | otherwise = let size = BS.foldl' (\n byte -> n*256+fromIntegral byte) (0 :: Integer) bytes
                in if size > fromIntegral frameCap then Nothing else Just (fromIntegral size)
sendFrame :: Ptr NativeProcess -> BS.ByteString -> IO ()
sendFrame pointer payload = writeAll (frameHeader (BS.length payload) <> payload)
  where
    writeAll bytes = BS.useAsCStringLen bytes $ \(raw,size) -> go (castPtr raw) size
    go _ 0 = pure ()
    go raw left = do
      count <- fromIntegral <$> writeNative pointer raw (fromIntegral (min left 16384))
      when (count <= 0) $ throwIO ProcessProtocol
      go (raw `plusPtr` count) (left-count)
receiveFrame :: Ptr NativeProcess -> IO BS.ByteString
receiveFrame pointer = do
  header <- readAll 4 []
  size <- maybe (throwIO ProcessProtocol) pure (parseFrameHeader header)
  readAll size []
  where
    readAll 0 pieces = pure (BS.concat (reverse pieces))
    readAll left pieces = allocaBytes (min left 16384) $ \raw -> do
      count <- fromIntegral <$> readNative pointer raw (fromIntegral (min left 16384))
      when (count <= 0) $ throwIO ProcessProtocol
      bytes <- BS.packCStringLen (castPtr raw,count)
      readAll (left-count) (bytes:pieces)
