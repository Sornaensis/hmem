-- | Dedicated cookieless listener. It never falls back to application routes.
module HMem.Server.Document.PreviewOrigin (newPreviewApplication,withPreviewListener) where

import Control.Concurrent.Async (race,race_,withAsync,waitCatch)
import Control.Concurrent.MVar (newEmptyMVar,putMVar,takeMVar)
import Control.Exception (throwIO)
import Data.String (fromString)
import Data.Text qualified
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import System.Timeout (timeout)
import HMem.Server.Document.Grant
import HMem.Config (RateLimitConfig)
import HMem.Server.App (rateLimitMiddleware)

newPreviewApplication :: DocumentAuthority -> RateLimitConfig -> IO Wai.Application
newPreviewApplication authority rateConfig=do
  admit<-rateLimitMiddleware rateConfig
  pure $ \request respond->do
    csp<-previewResponseCSP authority
    admit (redeemApplication authority True) request (respond . Wai.mapResponseHeaders (ensurePolicy csp . documentResponseHeaders))
  where
    ensurePolicy csp headers=if any((=="Content-Security-Policy") . fst)headers then headers else ("Content-Security-Policy",csp):headers

withPreviewListener :: DocumentAuthority -> RateLimitConfig -> Wai.Middleware -> IO() -> IO()
withPreviewListener authority rateConfig logger action=previewListener authority >>= \case
  Nothing->action
  Just(host,port)->do
    ready<-newEmptyMVar
    application<-newPreviewApplication authority rateConfig
    let settings=Warp.setHost(fromString(showHost host)) $ Warp.setPort port $ Warp.setTimeout 60
          $ Warp.setGracefulShutdownTimeout(Just 5) $ Warp.setBeforeMainLoop(putMVar ready()) Warp.defaultSettings
        unexpected result=either throwIO (const(throwIO(userError "document preview listener exited")))result
    withAsync (Warp.runSettings settings(logger application)) $ \listener->do
      started<-race (timeout 5000000(takeMVar ready)) (waitCatch listener)
      case started of
        Left(Just())->race_ action (waitCatch listener >>=unexpected)
        Left Nothing->throwIO(userError "document preview listener startup timed out")
        Right result->unexpected result
  where showHost=Data.Text.unpack
