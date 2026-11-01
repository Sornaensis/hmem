{-# LANGUAGE ForeignFunctionInterface #-}
module HMem.DB.AuditMaintenance.Cancel
  ( Cancel, capabilityVersion, withCancel, dispatchBefore,finishCancel ) where

import Control.Exception (mask,mask_,finally,onException)
import Control.Monad (void)
import Data.Text (Text)
import Data.Word (Word64)
import Foreign hiding (void)
import Foreign.C.Types
import GHC.Clock (getMonotonicTimeNSec)
import Hasql.Connection qualified as H
import Database.PostgreSQL.LibPQ.Internal qualified as PQ

newtype Cancel = Cancel (Ptr ())
foreign import ccall safe "hmem_audit_cancel_capability" c_capability :: Ptr CInt -> IO CInt
foreign import ccall safe "hmem_audit_cancel_open" c_open :: Ptr () -> Ptr (Ptr ()) -> IO CInt
foreign import ccall safe "hmem_audit_cancel_start" c_start :: Ptr () -> IO CInt
foreign import ccall safe "hmem_audit_cancel_poll" c_poll :: Ptr () -> CInt -> IO CInt
foreign import ccall safe "hmem_audit_cancel_finish" c_finish :: Ptr () -> IO ()
foreign import ccall safe "hmem_audit_cancel_reset" c_reset :: Ptr () -> IO CInt
foreign import ccall safe "hmem_audit_cancel_close" c_close :: Ptr () -> IO ()

reason :: CInt -> Text
reason code=case abs code of
  1 -> "modern_cancel_unsupported_client_or_platform"
  2 -> "modern_cancel_linked_module_unproven"
  3 -> "modern_cancel_exports_unavailable"
  4 -> "modern_cancel_creation_failed"
  5 -> "modern_cancel_start_failed"
  6 -> "modern_cancel_transport_failed"
  8 -> "modern_cancel_reset_failed"
  _ -> "modern_cancel_owner_invalid"

capabilityVersion :: IO (Either Text Int)
capabilityVersion=alloca $ \version -> do
  result<-c_capability version
  if result==0 then Right . fromIntegral <$>peek version else pure (Left (reason result))

-- The single poller must already be stopped. Also retires an unused allocated
-- capsule, so no old operation transport can precede later protocol statements.
finishCancel :: Cancel -> IO ()
finishCancel (Cancel pointer)=mask_ (c_finish pointer)

-- The actual acquisition/publication is masked. The operation root must keep
-- this bracket until its query and cancel poller have both joined. No asynchronous
-- cancellation may unwind it beneath a live child.
withCancel :: H.Connection -> (Cancel -> IO a) -> IO (Either Text a)
withCancel connection action=mask $ \restore -> do
  acquired<-H.withLibPQConnection connection $ \pq -> PQ.withConn pq $ \raw -> alloca $ \output -> do
    poke output nullPtr
    result<-c_open (castPtr raw) output
    if result==0 then Right . Cancel <$>peek output else pure (Left (reason result))
  case acquired of
    Left failure -> pure (Left failure)
    Right owner@(Cancel pointer) -> (Right <$>restore (action owner)) `finally` c_close pointer

-- One generation-fenced serial poller may retry while the same query is pending.
-- Reset closes each transport, retaining only this operation's immutable target.
-- The
-- absolute monotonic deadline is independent of libpq connect_timeout. Finish
-- the transport before returning even on timeout; successful dispatch remains
-- signal-only and never authorizes another statement before the query joins.
dispatchBefore :: Word64 -> Cancel -> IO (Either Text ())
dispatchBefore deadline (Cancel pointer)=mask_ $ do
  result<-(do
    started<-c_start pointer
    if started/=0 then pure (Left (reason started)) else loop)
    `onException` void(c_reset pointer)
  reset<-c_reset pointer
  pure(if reset==0 then result else Left(reason reset))
  where
    loop=do
      now<-getMonotonicTimeNSec
      if now>=deadline then pure (Left "modern_cancel_transport_deadline") else do
        let milliseconds=fromIntegral (min 100 ((deadline-now) `div` 1000000))
        result<-c_poll pointer milliseconds
        case result of
          1 -> pure (Right ())
          0 -> loop
          _ -> pure (Left (reason result))
