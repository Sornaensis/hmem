module HMem.Server.Exception
  ( trySynchronous
  ) where

import Control.Exception
  ( SomeAsyncException
  , SomeException
  , fromException
  , throwIO
  , try
  )

-- | Catch ordinary failures while preserving thread cancellation and every
-- exception registered as asynchronous through 'SomeAsyncException'.
trySynchronous :: IO a -> IO (Either SomeException a)
trySynchronous action = do
  attempted <- try action
  case attempted of
    Left err -> case fromException err :: Maybe SomeAsyncException of
      Just _ -> throwIO err
      Nothing -> pure attempted
    Right _ -> pure attempted
