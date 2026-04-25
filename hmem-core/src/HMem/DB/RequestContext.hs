module HMem.DB.RequestContext
  ( ActorType(..)
  , Principal(..)
  , RequestContext(..)
  , emptyRequestContext
  , withRequestContext
  , withRequestIdContext
  , withPrincipalContext
  , withWorkspaceIdContext
  , currentRequestContext
  , currentRequestId
  , currentPrincipal
  , currentWorkspaceId
  ) where

import Control.Concurrent (ThreadId, myThreadId)
import Control.Exception (bracket)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.UUID (UUID)
import System.IO.Unsafe (unsafePerformIO)

data ActorType
  = ActorUser
  | ActorBot
  deriving stock (Show, Eq)

data Principal = Principal
  { actorType  :: !ActorType
  , actorId    :: !Text
  , actorLabel :: !Text
  } deriving stock (Show, Eq)

data RequestContext = RequestContext
  { requestId   :: !(Maybe Text)
  , principal   :: !(Maybe Principal)
  , workspaceId :: !(Maybe UUID)
  } deriving stock (Show, Eq)

emptyRequestContext :: RequestContext
emptyRequestContext = RequestContext
  { requestId = Nothing
  , principal = Nothing
  , workspaceId = Nothing
  }

data RestoreAction
  = NoRestore
  | RestoreContext !ThreadId !(Maybe RequestContext)

withRequestContext :: RequestContext -> IO a -> IO a
withRequestContext ctx = bracket (pushContext (const ctx)) popContext . const

withRequestIdContext :: Maybe Text -> IO a -> IO a
withRequestIdContext mRequestId = bracket (pushContext update) popContext . const
  where
    update ctx = ctx { requestId = mRequestId }

withPrincipalContext :: Maybe Principal -> IO a -> IO a
withPrincipalContext mPrincipal = bracket (pushContext update) popContext . const
  where
    update ctx = ctx { principal = mPrincipal }

withWorkspaceIdContext :: Maybe UUID -> IO a -> IO a
withWorkspaceIdContext mWorkspaceId = bracket (pushContext update) popContext . const
  where
    update ctx = ctx { workspaceId = mWorkspaceId }

currentRequestContext :: IO RequestContext
currentRequestContext = do
  tid <- myThreadId
  maybe emptyRequestContext id . Map.lookup tid <$> readIORef requestContextsRef

currentRequestId :: IO (Maybe Text)
currentRequestId = (.requestId) <$> currentRequestContext

currentPrincipal :: IO (Maybe Principal)
currentPrincipal = (.principal) <$> currentRequestContext

currentWorkspaceId :: IO (Maybe UUID)
currentWorkspaceId = (.workspaceId) <$> currentRequestContext

pushContext :: (RequestContext -> RequestContext) -> IO RestoreAction
pushContext update = do
  tid <- myThreadId
  previous <- atomicModifyIORef' requestContextsRef $ \m ->
    let prior = Map.lookup tid m
        next = update (maybe emptyRequestContext id prior)
    in (Map.insert tid next m, prior)
  pure (RestoreContext tid previous)

popContext :: RestoreAction -> IO ()
popContext NoRestore = pure ()
popContext (RestoreContext tid previous) =
  atomicModifyIORef' requestContextsRef $ \m ->
    let restored = case previous of
          Just prior -> Map.insert tid prior m
          Nothing    -> Map.delete tid m
    in (restored, ())

requestContextsRef :: IORef (Map ThreadId RequestContext)
{-# NOINLINE requestContextsRef #-}
requestContextsRef = unsafePerformIO (newIORef Map.empty)
