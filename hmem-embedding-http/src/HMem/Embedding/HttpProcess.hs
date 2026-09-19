{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
module HMem.Embedding.HttpProcess
  ( SessionPolicy (..)
  , HttpSession
  , SessionFailure (..)
  , withHttpSession
  , callHttp
  , sessionProcessId
  ) where

import Control.Concurrent.Async (Async, async, asyncBound, cancel, waitCatch)
import Control.Concurrent.MVar (MVar, newMVar, putMVar, takeMVar, withMVar)
import Control.Exception (IOException, SomeException, finally, mask, onException,
                          throwIO, try, uninterruptibleMask_)
import Control.Monad (unless, void, when)
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.IORef (IORef, atomicWriteIORef, newIORef, readIORef, writeIORef)
import Data.Word (Word8, Word64)
import Foreign (Ptr, alloca, allocaBytes, castPtr, nullPtr, peek, plusPtr, poke)
import Foreign.C.String (CString, withCString)
import Foreign.C.Types (CInt (..))
import GHC.Clock (getMonotonicTimeNSec)
import System.Directory (canonicalizePath, doesFileExist)
import System.FilePath (isAbsolute)
import System.Timeout (timeout)

import HMem.Embedding.HttpProtocol
  ( HttpCall, HttpReply (..), TransportCode (..), WireReply (..), decodeWireReply
  , encodeCall, frameHeader, parseFrameHeader, responseFrameCap
  )

data OwnedProcess

foreign import ccall safe "hmem_process_spawn"
  cSpawn :: CString -> Ptr (Ptr OwnedProcess) -> IO CInt
foreign import ccall safe "hmem_process_write"
  cWrite :: Ptr OwnedProcess -> Ptr Word8 -> CInt -> IO CInt
foreign import ccall safe "hmem_process_read_out"
  cReadOut :: Ptr OwnedProcess -> Ptr Word8 -> CInt -> IO CInt
foreign import ccall safe "hmem_process_read_err"
  cReadErr :: Ptr OwnedProcess -> Ptr Word8 -> CInt -> IO CInt
foreign import ccall safe "hmem_process_close_input"
  cCloseInput :: Ptr OwnedProcess -> IO CInt
foreign import ccall safe "hmem_process_kill"
  cKill :: Ptr OwnedProcess -> IO CInt
foreign import ccall safe "hmem_process_wait"
  cWait :: Ptr OwnedProcess -> CInt -> IO CInt
foreign import ccall unsafe "hmem_process_pid"
  cPid :: Ptr OwnedProcess -> IO CInt
foreign import ccall unsafe "hmem_process_destroy"
  cDestroy :: Ptr OwnedProcess -> IO ()

data SessionPolicy = SessionPolicy
  { helperExecutable :: !FilePath
  , afterSpawnBeforeReady :: !(Int -> IO ())
  , afterSuccessfulResponse :: !(HttpReply -> IO ())
  }

data SessionFailure
  = InvalidHelperPath
  | SpawnFailed
  | DeadlineExceeded
  | ProtocolViolation
  | PipeFailure
  | ProcessOwnershipFailure
  | ChildFailure !TransportCode
  deriving (Eq, Show)

data HttpSession = HttpSession
  { ownedProcess :: !(Ptr OwnedProcess)
  , cachedPid :: !Int
  , logicalDeadline :: !Word64
  , serialLock :: !(MVar ())
  , terminationLock :: !(MVar ())
  , stderrReader :: !(Async ())
  , stderrOverflow :: !(IORef Bool)
  , retired :: !(IORef Bool)
  }

sessionProcessId :: HttpSession -> IO Int
sessionProcessId = pure . cachedPid

-- The caller supplies its original absolute monotonic deadline. Fifty milliseconds
-- of every logical operation is reserved for local process shutdown; under an OS
-- stall, acknowledgement may take longer and admission must remain owned.
operationCutoff :: Word64 -> Word64
operationCutoff deadline = deadline - min deadline 50000000

-- The caller's forcing action must evaluate every part of the result that it
-- intends to use after this session closes. It runs inside the same deadline.
withHttpSession :: (a -> IO ()) -> SessionPolicy -> Word64 -> (HttpSession -> IO a)
                -> IO (Either SessionFailure a)
withHttpSession forceResult policy deadline action = mask $ \restore -> do
  -- Keep the bound creator alive until acquisition and all owned cleanup have
  -- finished, even if native spawn is temporarily uninterruptible. The outer
  -- waiter may pass its deadline only while it is still joining that owner.
  before <- remainingMicros deadline
  if before <= 0 then pure (Left DeadlineExceeded) else do
    owner <- asyncBound (restore (withOwnedSession forceResult policy deadline action))
    let joinOwner = uninterruptibleMask_ $ do
          cancel owner
          void (waitCatch owner)
    observed <- restore (timeout before (waitCatch owner)) `onException` joinOwner
    case observed of
      Nothing -> joinOwner >> pure (Left DeadlineExceeded)
      Just (Left exception) -> throwIO exception
      Just (Right outcome) -> do
        remaining <- remainingMicros deadline
        pure $ if remaining <= 0 then Left DeadlineExceeded else outcome

withOwnedSession :: (a -> IO ()) -> SessionPolicy -> Word64
                 -> (HttpSession -> IO a) -> IO (Either SessionFailure a)
withOwnedSession forceResult policy deadline action = mask $ \restore -> do
  -- The bound OS thread that forks a Linux helper stays alive through reaping.
  -- The deadline is supplied by the caller and is checked before any acquisition.
  before <- remainingMicros (operationCutoff deadline)
  if before <= 0 then pure (Left DeadlineExceeded) else do
    valid <- validExecutable (helperExecutable policy)
    beforeSpawn <- remainingMicros (operationCutoff deadline)
    if beforeSpawn <= 0 then pure (Left DeadlineExceeded)
    else if not valid then pure (Left InvalidHelperPath)
    else do
      spawned <- uninterruptibleMask_ $ withCString (helperExecutable policy) $ \path ->
        alloca $ \slot -> do
          poke slot nullPtr
          ok <- cSpawn path slot
          if ok == 1 then Just <$> peek slot else pure Nothing
      case spawned of
        Nothing -> do
          after <- remainingMicros (operationCutoff deadline)
          pure (Left (if after <= 0 then DeadlineExceeded else SpawnFailed))
        Just process -> do
          -- Masking keeps the returned native owner alive until its finalizer is
          -- installed, including when cancellation arrived during CreateProcess.
          session <- newSession process deadline `onException` rawClose process
          outcome <- try (restore (runSession forceResult policy session action))
          closed <- closeSession session
          case outcome of
            Left exception -> throwIO (exception :: SomeException)
            Right value -> case value of
              Left problem -> pure (Left problem)
              Right result -> do
                remaining <- remainingMicros deadline
                pure $ if remaining <= 0 then Left DeadlineExceeded
                       else either Left (const (Right result)) closed

newSession :: Ptr OwnedProcess -> Word64 -> IO HttpSession
newSession process deadline = do
  pid <- fromIntegral <$> cPid process
  overflow <- newIORef False
  lock <- newMVar ()
  termination <- newMVar ()
  terminal <- newIORef False
  reader <- async (drainStderr process overflow)
  pure $ HttpSession process pid deadline lock termination reader overflow terminal

rawClose :: Ptr OwnedProcess -> IO ()
rawClose process = uninterruptibleMask_ $ do
  terminateAndReap process
  cDestroy process

runSession :: (a -> IO ()) -> SessionPolicy -> HttpSession -> (HttpSession -> IO a)
           -> IO (Either SessionFailure a)
runSession forceResult policy session action = do
  let cutoff = operationCutoff (logicalDeadline session)
  remaining <- remainingMicros cutoff
  if remaining <= 0 then pure (Left DeadlineExceeded) else do
    observedSpawn <- supervise session cutoff $ afterSpawnBeforeReady policy (cachedPid session)
    case observedSpawn of
      Left problem -> pure (Left problem)
      Right () -> do
        ready <- supervise session cutoff (receiveFrame (ownedProcess session))
        case ready of
          Left problem -> pure (Left problem)
          Right value | value /= "HMEM1" -> pure (Left ProtocolViolation)
          Right _ -> supervise session cutoff $ do
            result <- action session
            forceResult result
            pure result

validExecutable :: FilePath -> IO Bool
validExecutable path
  | not (isAbsolute path) = pure False
  | otherwise = do
      exists <- doesFileExist path
      if not exists then pure False else do
        canonical <- try (canonicalizePath path) :: IO (Either IOException FilePath)
        pure $ either (const False) (== path) canonical

callHttp :: SessionPolicy -> HttpSession -> HttpCall
         -> IO (Either SessionFailure HttpReply)
callHttp policy session call = mask $ \restore -> do
  alreadyRetired <- readIORef (retired session)
  if alreadyRetired then pure (Left ProcessOwnershipFailure) else do
    micros <- remainingMicros (operationCutoff (logicalDeadline session))
    if micros <= 0 then pure (Left DeadlineExceeded) else do
      acquired <- restore (timeout micros (takeMVar (serialLock session)))
      case acquired of
        Nothing -> pure (Left DeadlineExceeded)
        Just () -> do
          result <- (restore (callLocked policy session call)
                      `onException` atomicWriteIORef (retired session) True)
                      `finally` putMVar (serialLock session) ()
          pure result

callLocked :: SessionPolicy -> HttpSession -> HttpCall
           -> IO (Either SessionFailure HttpReply)
callLocked policy session call = do
  dead <- readIORef (retired session)
  if dead then pure (Left ProcessOwnershipFailure) else do
    result <- supervise session (operationCutoff (logicalDeadline session)) $ do
      case encodeCall call of
        Nothing -> pure (Left ProtocolViolation)
        Just payload -> do
          sendFrame (ownedProcess session) payload
          frame <- receiveFrame (ownedProcess session)
          case decodeWireReply frame of
            Left _ -> pure (Left ProtocolViolation)
            Right (WireFailure code) -> pure (Left (ChildFailure code))
            Right (WireHttp reply) -> do
              BS.length (replyBody reply) `seq` replyStatus reply `seq` pure ()
              when (replyStatus reply >= 200 && replyStatus reply < 300) $
                afterSuccessfulResponse policy reply
              pure (Right reply)
    case result of
      Left problem -> pure (Left problem) -- supervise has already retired/reaped.
      Right (Left problem@(ChildFailure code)) | code /= InvalidFrame ->
        pure (Left problem)
      Right (Left problem) -> do
        atomicWriteIORef (retired session) True
        terminateSession session
        pure (Left problem)
      Right (Right reply) -> pure (Right reply)

-- This supervisor never executes parent pipe FFI on the deadline thread.
-- Termination precedes joining a blocked writer/reader, so a stuck child cannot
-- make a timeout appear successful or release the operation prematurely.
supervise :: HttpSession -> Word64 -> IO a
          -> IO (Either SessionFailure a)
supervise session cutoff operation = mask $ \restore -> do
  before <- remainingMicros cutoff
  if before <= 0 then do
    atomicWriteIORef (retired session) True
    terminateSession session
    pure (Left DeadlineExceeded)
  else do
    worker <- async (restore operation)
    let abort = uninterruptibleMask_ $ do
          atomicWriteIORef (retired session) True
          terminateSession session
          cancel worker
          void (waitCatch worker)
    micros <- remainingMicros cutoff
    if micros <= 0 then abort >> pure (Left DeadlineExceeded)
    else do
      observed <- restore (timeout micros (waitCatch worker)) `onException` abort
      case observed of
        Nothing -> abort >> pure (Left DeadlineExceeded)
        Just (Left _) -> abort >> pure (Left PipeFailure)
        Just (Right value) -> do
          left <- remainingMicros cutoff
          if left <= 0 then abort >> pure (Left DeadlineExceeded)
          else pure (Right value)

terminateSession :: HttpSession -> IO ()
terminateSession session = uninterruptibleMask_ $
  withMVar (terminationLock session) $ \_ ->
    terminateAndReap (ownedProcess session)

remainingMicros :: Word64 -> IO Int
remainingMicros cutoff = do
  now <- getMonotonicTimeNSec
  pure $ if cutoff <= now then 0
         else fromIntegral (min (fromIntegral (maxBound :: Int))
                        ((cutoff - now) `div` 1000))

terminateAndReap :: Ptr OwnedProcess -> IO ()
terminateAndReap process = do
  _ <- cKill process
  awaitExit process

awaitExit :: Ptr OwnedProcess -> IO ()
awaitExit process = do
  status <- cWait process 100
  unless (status == 1) (awaitExit process)

closeSession :: HttpSession -> IO (Either SessionFailure ())
closeSession session = uninterruptibleMask_ $ do
  atomicWriteIORef (retired session) True
  withMVar (serialLock session) $ \_ -> do
    -- No caller can dereference the process after this lock holder destroys it.
    writeIORef (retired session) True
    let process = ownedProcess session
    _ <- cCloseInput process
    now <- getMonotonicTimeNSec
    let remaining = if now >= logicalDeadline session then 0
                    else min 50 ((logicalDeadline session - now) `div` 1000000)
    withMVar (terminationLock session) $ \_ -> do
      exited <- cWait process (fromIntegral remaining)
      when (exited /= 1) (terminateAndReap process)
    reader <- waitCatch (stderrReader session)
    overflow <- readIORef (stderrOverflow session)
    cDestroy process
    pure $ if overflow || either (const True) (const False) reader
      then Left PipeFailure else Right ()

drainStderr :: Ptr OwnedProcess -> IORef Bool -> IO ()
drainStderr process overflow = go 0
  where
    go :: Int -> IO ()
    go seen = allocaBytes 4096 $ \buffer -> do
      count <- cReadErr process buffer 4096
      if count > 0 then do
        let total = seen + fromIntegral count
        when (total > 65536) $ writeIORef overflow True
        go total
      else if count == 0 then pure ()
      else writeIORef overflow True

sendFrame :: Ptr OwnedProcess -> ByteString -> IO ()
sendFrame process payload = case frameHeader (BS.length payload) of
  Nothing -> throwIO (userError "invalid bounded helper frame")
  Just header -> writeExact process (BS.append header payload)

writeExact :: Ptr OwnedProcess -> ByteString -> IO ()
writeExact process bytes = BS.useAsCStringLen bytes $ \(pointer, lengthBytes) ->
  go (castPtr pointer) lengthBytes
  where
    go _ 0 = pure ()
    go pointer remaining = do
      count <- cWrite process pointer (fromIntegral (min remaining 16384))
      if count <= 0 then throwIO (userError "helper pipe write failed")
      else go (pointer `plusPtr` fromIntegral count) (remaining - fromIntegral count)

receiveFrame :: Ptr OwnedProcess -> IO ByteString
receiveFrame process = do
  header <- readExact process 4
  case parseFrameHeader responseFrameCap header of
    Nothing -> throwIO (userError "invalid helper frame length")
    Just size -> readExact process size

readExact :: Ptr OwnedProcess -> Int -> IO ByteString
readExact process size = go size []
  where
    go remaining pieces
      | remaining == 0 = pure (BS.concat (reverse pieces))
      | otherwise = allocaBytes (min remaining 8192) $ \buffer -> do
          count <- cReadOut process buffer (fromIntegral (min remaining 8192))
          if count <= 0 then throwIO (userError "truncated helper frame")
          else do
            piece <- BS.packCStringLen (castPtr buffer, fromIntegral count)
            go (remaining - fromIntegral count) (piece : pieces)
