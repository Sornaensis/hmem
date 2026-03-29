module HMem.DB.RequestContext
  ( withRequestIdContext
  , currentRequestId
  ) where

import Control.Concurrent (ThreadId, myThreadId)
import Control.Exception (bracket)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import System.IO.Unsafe (unsafePerformIO)

data RestoreAction
  = NoRestore
  | RestoreRequestId !ThreadId !(Maybe Text)

withRequestIdContext :: Maybe Text -> IO a -> IO a
withRequestIdContext requestId = bracket (pushRequestId requestId) popRequestId . const

currentRequestId :: IO (Maybe Text)
currentRequestId = do
  tid <- myThreadId
  Map.lookup tid <$> readIORef requestIdsRef

pushRequestId :: Maybe Text -> IO RestoreAction
pushRequestId Nothing = pure NoRestore
pushRequestId (Just requestId) = do
  tid <- myThreadId
  previous <- atomicModifyIORef' requestIdsRef $ \m ->
    let prior = Map.lookup tid m
    in (Map.insert tid requestId m, prior)
  pure (RestoreRequestId tid previous)

popRequestId :: RestoreAction -> IO ()
popRequestId NoRestore = pure ()
popRequestId (RestoreRequestId tid previous) =
  atomicModifyIORef' requestIdsRef $ \m ->
    let restored = case previous of
          Just prior -> Map.insert tid prior m
          Nothing    -> Map.delete tid m
    in (restored, ())

requestIdsRef :: IORef (Map ThreadId Text)
{-# NOINLINE requestIdsRef #-}
requestIdsRef = unsafePerformIO (newIORef Map.empty)