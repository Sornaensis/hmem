{-# LANGUAGE ForeignFunctionInterface #-}
-- | A slot remains owned until the native child and its pipe owner have exited.
module HMem.MCP.SessionProcess
  ( ProcessScope, ProcessFault(..), newProcessScope, newProcessScopeWithReapHook, closeProcessScope, cancelProcessScope, scopePoisoned
  , runHelper, processMetrics, nativeProcessCount, processAlive, watchParent
  , runReusableHelper
  , frameHeader, parseFrameHeader, frameCap
  , attributeFailureProbe
  , killProcessId, childProcessIds, processImage, blockChildProbe
  , AttachmentPipes(..), runAttachmentExchange, attachmentContextReady, attachmentCancellationRequested
  ) where

import Control.Concurrent
import Control.Concurrent.Async (async, asyncBound, asyncWithUnmask, cancel, waitCatch, waitCatchSTM)
import Control.Concurrent.STM
import Control.Exception
import Control.Monad (forM_, unless, void, when)
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.IORef
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
foreign import ccall safe "hmem_session_process_spawn_kind" spawnKindNative :: CString -> CInt -> Ptr (Ptr NativeProcess) -> IO CInt
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
data Exchange = Exchange
  { payload :: BS.ByteString, cancelled :: TVar Bool, result :: MVar (Either ProcessFault BS.ByteString), cutoff :: Word64
  , onSpawn :: Int -> IO (), onPhase :: T.Text -> IO () }
data Reusable = Reusable
  { key :: Int, executable :: FilePath, lease :: Lease, request :: TMVar Exchange, busy :: TVar Bool }
data ProcessScope = ProcessScope
  { leases :: TVar (Map.Map Int Lease), sequenceNumber :: TVar Int, poisoned :: TVar Bool, pipes :: TVar Int, closing :: TVar Bool, afterReap :: IO (), reusable :: TVar [Reusable] }

newProcessScope :: IO ProcessScope
newProcessScope = newProcessScopeWithReapHook (pure ())
newProcessScopeWithReapHook :: IO () -> IO ProcessScope
newProcessScopeWithReapHook hook = ProcessScope <$> newTVarIO Map.empty <*> newTVarIO 0 <*> newTVarIO False <*> newTVarIO 0 <*> newTVarIO False <*> pure hook <*> newTVarIO []
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

-- Ordinary attachment ownership is server-scoped and independent of follower
-- leases. A timed-out caller never erases a still-owned child or pipe reader.
attachmentContextReady :: ProcessScope -> STM Bool
attachmentContextReady scope = do
  leases <- readTVar scope.leases
  pipes <- readTVar scope.pipes
  pure (Map.null leases && pipes == 0)

-- Observe cleanup entry without sleeps when qualifying interrupted retirement.
attachmentCancellationRequested :: ProcessScope -> STM Bool
attachmentCancellationRequested scope = do
  leases <- readTVar scope.leases
  or <$> mapM (readTVar . (.cancelled)) (Map.elems leases)

data AttachmentPipes = AttachmentPipes
  { sendAttachmentFrame :: BS.ByteString -> IO ()
  , receiveAttachmentFrame :: IO BS.ByteString
  , expectAttachmentEOF :: IO ()
  }

runAttachmentExchange :: ProcessScope -> FilePath -> Word64 -> Word64 -> (Int -> IO ())
  -> (AttachmentPipes -> IO ()) -> (AttachmentPipes -> IO BS.ByteString)
  -> IO (Either ProcessFault BS.ByteString)
runAttachmentExchange scope executable cutoff deadline onSpawn input output = mask $ \restore -> do
  valid <- if isAbsolute executable then doesFileExist executable else pure False
  canonical <- if valid then canonicalizePath executable else pure ""
  available <- remaining cutoff
  if not valid || canonical /= executable then pure (Left ProcessUnavailable)
  else if available <= 0 then pure (Left ProcessDeadline)
  else do
    cancelled <- newTVarIO False
    result <- newEmptyMVar
    retired <- newEmptyTMVarIO
    complete <- newEmptyTMVarIO
    replies <- newTBQueueIO 2
    readerFailure <- newTVarIO Nothing
    let lease = Lease cancelled result
    key <- atomically $ do
      poisoned <- readTVar scope.poisoned
      closing <- readTVar scope.closing
      when poisoned (throwSTM ProcessOwnership)
      when closing (throwSTM ProcessUnavailable)
      key <- readTVar scope.sequenceNumber
      writeTVar scope.sequenceNumber (key+1)
      modifyTVar' scope.leases (Map.insert key lease)
      pure key
    let finish value = do
          atomically $ do
            modifyTVar' scope.leases (Map.delete key)
            putTMVar retired value
          putMVar result value
        poisonIfUnconfirmed = atomically $ do
          confirmed <- tryReadTMVar retired
          case confirmed of
            Just _ -> pure ()
            Nothing -> writeTVar scope.poisoned True
        cancelAndWait = (do
          atomically $ writeTVar cancelled True
          left <- remaining deadline
          done <- timeout left (atomically (readTMVar retired))
          case done of
            Just value -> pure value
            Nothing -> poisonIfUnconfirmed >> throwIO ProcessOwnership)
          `onException` poisonIfUnconfirmed
        receive = atomically $ readTBQueue replies `orElse` do
          failure <- readTVar readerFailure
          case failure of
            Just fault -> throwSTM fault
            Nothing -> do
              done <- readTMVar retired
              case done of Left fault -> throwSTM fault; Right _ -> pure Nothing
        readFrame = receive >>= maybe (throwIO ProcessProtocol) pure
        eof = receive >>= \frame -> unless (frame == Nothing) (throwIO ProcessProtocol)
        -- Input executes only in the owned writer. Semantic callbacks and file
        -- writes execute in the caller, never in a retained background pump.
        consumer = AttachmentPipes (const (throwIO ProcessProtocol)) readFrame eof
    _ <- asyncBound (ownAttachmentProcess scope lease executable cutoff onSpawn input complete replies readerFailure >>= finish)
      `onException` atomically (modifyTVar' scope.leases (Map.delete key))
    left <- remaining cutoff
    observed <- try @SomeException (restore (timeout left (output consumer)))
    case observed of
      Left exception -> cancelAndWait >> throwIO exception
      Right Nothing -> cancelAndWait >> pure (Left ProcessDeadline)
      Right (Just bytes) -> do
        atomically $ putTMVar complete bytes
        rest <- remaining deadline
        answer <- restore (timeout rest (readMVar result)) `onException` void cancelAndWait
        case answer of
          Just value -> pure value
          Nothing -> cancelAndWait >> pure (Left ProcessDeadline)

ownAttachmentProcess :: ProcessScope -> Lease -> FilePath -> Word64 -> (Int -> IO ())
  -> (AttachmentPipes -> IO ()) -> TMVar BS.ByteString -> TBQueue (Maybe BS.ByteString)
  -> TVar (Maybe ProcessFault) -> IO (Either ProcessFault BS.ByteString)
ownAttachmentProcess scope lease executable cutoff onSpawn input complete replies readerFailure = mask $ \restore -> do
  let permitted = do
        left <- remaining cutoff
        stopped <- readTVarIO lease.cancelled
        pure (left > 0 && not stopped)
  before <- permitted
  if not before then pure (Left ProcessDeadline) else do
    spawned <- BS.useAsCString (TE.encodeUtf8 (T.pack executable)) $ \path -> alloca $ \out -> do
      poke out nullPtr
      code <- spawnKindNative path 1 out
      pointer <- peek out
      pure (code,pointer)
    case spawned of
      (_,pointer) | pointer == nullPtr -> pure (Left ProcessUnavailable)
      (code,pointer) -> do
        pumps <- newIORef []
        ready <- newEmptyMVar
        let reap = do
              _ <- killNative pointer
              exited <- waitNative pointer 100
              unless (exited == 1) (threadDelay 1000 >> reap)
            startPipe action = do
              atomically $ modifyTVar' scope.pipes (+1)
              worker <- asyncBound (restore action `finally` atomically (modifyTVar' scope.pipes (subtract 1)))
                `onException` atomically (modifyTVar' scope.pipes (subtract 1))
              modifyIORef' pumps (worker:)
              pure worker
            boundedSend bytes = do
              allowed <- permitted
              unless allowed (throwIO ProcessDeadline)
              when (BS.length bytes > 1048577) (throwIO ProcessProtocol)
              sendFrame pointer bytes
            channel = AttachmentPipes boundedSend (throwIO ProcessProtocol) (throwIO ProcessProtocol)
            run = do
              pid <- fromIntegral <$> nativePid pointer
              onSpawn pid
              allowed <- permitted
              unless (code == 1 && allowed) (throwIO ProcessDeadline)
              _ <- startPipe $ (do
                hello <- receiveAttachmentNative pointer
                unless (hello == Just "HMATTACH1") (throwIO ProcessProtocol)
                putMVar ready ()
                let receive = do
                      frame <- receiveAttachmentNative pointer
                      atomically $ writeTBQueue replies frame
                      case frame of Nothing -> pure (); Just _ -> receive
                receive) `catch` \(exception :: SomeException) ->
                  atomically (writeTVar readerFailure (Just (maybe ProcessProtocol id (fromException exception))))
              _ <- startPipe $ do
                takeMVar ready
                allowedNow <- permitted
                unless allowedNow (throwIO ProcessDeadline)
                input channel `finally` void (closeInput pointer)
              let waiting = do
                    cancelled <- readTVar lease.cancelled
                    if cancelled then pure Nothing else Just <$> readTMVar complete
              left <- remaining cutoff
              observed <- restore (timeout left (atomically waiting))
              pure $ case observed of Just (Just bytes) -> Right bytes; _ -> Left ProcessDeadline
        outcome <- try @SomeException run
        -- Spawn failure cleanup can outlive the caller only while this bound
        -- creator and its lease retain every native handle and pipe pump.
        reap
        hook <- try @SomeException scope.afterReap
        ownedPumps <- readIORef pumps
        mapM_ cancel ownedPumps
        mapM_ waitCatch ownedPumps
        let destroy = do
              destroyed <- destroyNative pointer
              unless (destroyed == 1) (threadDelay 1000 >> destroy)
        destroy
        pure $ case (hook,outcome) of
          (Left _,_) -> Left ProcessProtocol
          (_,Right value) -> value
          (_,Left exception) -> Left (maybe ProcessProtocol id (fromException exception))

receiveAttachmentNative :: Ptr NativeProcess -> IO (Maybe BS.ByteString)
receiveAttachmentNative pointer = allocaBytes 1 $ \raw -> do
  count <- readNative pointer raw 1
  if count == 0 then pure Nothing else do
    unless (count == 1) (throwIO ProcessProtocol)
    first <- BS.packCStringLen (castPtr raw,1)
    rest <- readAttachmentBytes pointer 3
    size <- maybe (throwIO ProcessProtocol) pure (parseFrameHeader (first <> rest))
    when (size > 1048577) (throwIO ProcessProtocol)
    Just <$> readAttachmentBytes pointer size

readAttachmentBytes :: Ptr NativeProcess -> Int -> IO BS.ByteString
readAttachmentBytes pointer count = go count []
  where
    go 0 chunks = pure (BS.concat (reverse chunks))
    go left chunks = allocaBytes (min left 16384) $ \raw -> do
      n <- fromIntegral <$> readNative pointer raw (fromIntegral (min left 16384))
      when (n <= 0) (throwIO ProcessProtocol)
      bytes <- BS.packCStringLen (castPtr raw,n)
      go (left-n) (bytes:chunks)

-- Two process-owned transports retain native ownership while idle. Every
-- borrowed exchange still includes startup/recovery and kill/reap/pipe joins
-- within its five-second deadline. Failed joins quarantine the scope.
runReusableHelper :: ProcessScope -> FilePath -> (Int -> IO ()) -> (T.Text -> IO ()) -> BS.ByteString -> IO (Either ProcessFault BS.ByteString)
runReusableHelper scope executable onSpawn onPhase payload = mask $ \restore -> do
  start <- getMonotonicTimeNSec
  let deadline = start+5000000000; cutoff = deadline-1000000000
  valid <- if isAbsolute executable then doesFileExist executable else pure False
  canonical <- if valid then canonicalizePath executable else pure ""
  if not valid || canonical /= executable || BS.length payload > frameCap then pure (Left ProcessUnavailable) else do
    ownerResult <- newEmptyMVar
    remainingAdmission <- remaining cutoff
    acquired <- timeout remainingAdmission $ atomically $ do
      poisoned <- readTVar scope.poisoned; closing <- readTVar scope.closing
      when poisoned $ throwSTM ProcessOwnership
      when closing $ throwSTM ProcessUnavailable
      assets <- readTVar scope.reusable
      unless (all ((==executable) . (.executable)) assets) $ throwSTM ProcessUnavailable
      states <- mapM (readTVar . (.busy)) assets
      case [asset | (asset,False) <- zip assets states] of
        asset:_ -> writeTVar asset.busy True >> pure (asset,False)
        [] -> do
          check (length assets < 2)
          key <- readTVar scope.sequenceNumber
          writeTVar scope.sequenceNumber (key+1)
          cancelled <- newTVar False; request <- newEmptyTMVar; busy <- newTVar True
          let lease = Lease cancelled ownerResult
              asset = Reusable key executable lease request busy
          modifyTVar' scope.leases (Map.insert key lease)
          writeTVar scope.reusable (assets<>[asset])
          pure (asset,True)
    case acquired of
      Nothing -> pure (Left ProcessDeadline)
      Just (asset,fresh) -> do
        when fresh $ do
          _ <- asyncBound (reusableOwner scope asset) `onException` atomically (do
            modifyTVar' scope.leases (Map.delete asset.key)
            modifyTVar' scope.reusable (filter ((/=asset.key) . (.key))))
          pure ()
        cancelled <- newTVarIO False; result <- newEmptyMVar
        let exchange = Exchange payload cancelled result cutoff onSpawn onPhase
            await = do
              left <- remaining deadline
              observed <- timeout left (readMVar result)
              case observed of
                Just value -> pure value
                Nothing -> atomically (writeTVar scope.poisoned True) >> throwIO ProcessOwnership
            cancelAndJoin = atomically (writeTVar cancelled True) >> await
            release = atomically $ do
              poisoned <- readTVar scope.poisoned
              unless poisoned $ writeTVar asset.busy False
        (do atomically $ do
              closing <- readTVar scope.closing
              when closing $ throwSTM ProcessUnavailable
              putTMVar asset.request exchange
            left <- remaining cutoff
            observed <- restore (timeout left (readMVar result)) `onException` (cancelAndJoin >> pure ())
            maybe cancelAndJoin pure observed) `finally` release

-- The bound owner keeps the native pointer until child death and all pipe
-- owners are confirmed. It also owns recovery; no caller can release a broken
-- transport while cleanup is still running.
reusableOwner :: ProcessScope -> Reusable -> IO ()
reusableOwner scope asset = mask_ $ do
  native <- newIORef Nothing
  pipe <- newIORef Nothing
  active <- newIORef Nothing
  let retire = do
        current <- readIORef native
        case current of
          Nothing -> pure ()
          Just pointer -> do
            let reap = do
                  _ <- killNative pointer
                  exited <- waitNative pointer 100
                  unless (exited == 1) (threadDelay 1000 >> reap)
            reap
            _ <- try @SomeException scope.afterReap
            pipeOwner <- readIORef pipe
            case pipeOwner of
              Nothing -> pure ()
              Just task -> cancel task >> waitCatch task >> writeIORef pipe Nothing
            let destroy = do
                  released <- destroyNative pointer
                  unless (released == 1) $ do
                    atomically $ writeTVar scope.poisoned True
                    threadDelay 1000 >> destroy
            destroy
            writeIORef native Nothing
      answer :: Exchange -> Either ProcessFault BS.ByteString -> IO ()
      answer exchange value = do
        _ <- tryPutMVar exchange.result value
        writeIORef active Nothing
      loop = do
        next <- atomically $ do
          closing <- readTVar scope.closing
          cancelled <- readTVar asset.lease.cancelled
          poisoned <- readTVar scope.poisoned
          if closing || cancelled || poisoned then pure Nothing else Just <$> takeTMVar asset.request
        case next of
          Nothing -> pure ()
          Just exchange -> do
            writeIORef active (Just exchange)
            current <- readIORef native
            (pointer,fresh,code) <- case current of
              Just pointer -> pure (pointer,False,1)
              Nothing -> BS.useAsCString (TE.encodeUtf8 (T.pack asset.executable)) $ \path -> alloca $ \out -> do
                poke out nullPtr
                code <- spawnNative path out
                pointer <- peek out
                when (pointer /= nullPtr) $ writeIORef native (Just pointer)
                pure (pointer,True,code)
            if pointer == nullPtr || code /= 1 then do
              retire
              answer exchange (Left ProcessUnavailable)
            else do
              atomically $ modifyTVar' scope.pipes (+1)
              task <- asyncWithUnmask (\unmask -> unmask (do
                when fresh $ do
                  pid <- fromIntegral <$> nativePid pointer
                  exchange.onSpawn pid
                  ready <- receiveFrame pointer
                  unless (ready == "HMSESSION1") $ throwIO ProcessProtocol
                sendFrame pointer exchange.payload
                let receive count = do
                      bytes <- receiveFrame pointer
                      if "PHASE:" `BS.isPrefixOf` bytes && BS.length bytes <= 64 then do
                        when (count >= (8 :: Int)) $ throwIO ProcessProtocol
                        exchange.onPhase (TE.decodeUtf8 (BS.drop 6 bytes))
                        receive (count+1)
                      else pure bytes
                receive 0) `finally` atomically (modifyTVar' scope.pipes (subtract 1)))
                `onException` atomically (modifyTVar' scope.pipes (subtract 1))
              writeIORef pipe (Just task)
              let waiting = do
                    closing <- readTVar scope.closing
                    cancelled <- readTVar exchange.cancelled
                    ownerCancelled <- readTVar asset.lease.cancelled
                    poisoned <- readTVar scope.poisoned
                    if closing || cancelled || ownerCancelled || poisoned then pure Nothing else Just <$> waitCatchSTM task
              left <- remaining exchange.cutoff
              observed <- timeout left (atomically waiting)
              case observed of
                Just (Just (Right bytes)) -> do
                  -- waitCatchSTM observes the complete pipe-owner finalizer.
                  _ <- waitCatch task
                  writeIORef pipe Nothing
                  answer exchange (Right bytes)
                _ -> do
                  retire
                  answer exchange (Left ProcessDeadline)
            loop
  outcome <- try @SomeException loop
  -- No exception from setup or an exchange may skip native retirement.
  retire
  current <- readIORef active
  forM_ current $ \exchange -> answer exchange (Left ProcessProtocol)
  queued <- atomically $ tryTakeTMVar asset.request
  forM_ queued $ \exchange -> answer exchange (Left ProcessUnavailable)
  atomically $ do
    when (either (const True) (const False) outcome) $ writeTVar scope.poisoned True
    modifyTVar' scope.leases (Map.delete asset.key)
    modifyTVar' scope.reusable (filter ((/=asset.key) . (.key)))
  _ <- tryPutMVar asset.lease.result (either (const (Left ProcessProtocol)) (const (Right BS.empty)) outcome)
  pure ()

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
