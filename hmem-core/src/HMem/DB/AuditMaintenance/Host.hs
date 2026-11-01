{-# LANGUAGE ForeignFunctionInterface #-}
module HMem.DB.AuditMaintenance.Host
  ( AuditHost, HostIdentity(..), AuditRelation(..), VolumeCapacity(..), HostCapacity(..)
  , withAuditHost, withAuditHostWith, sampleAuditHost, backendHasExited ) where

import Control.Monad (forM)
import Control.Exception (mask,finally)
import Data.Aeson (FromJSON(..),withObject,(.:))
import Data.ByteString qualified as BS
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Data.Word
import Foreign
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Utils (withMany)

data AuditRelation = AuditRelation
  { oid :: !Int64, kind :: !Text, name :: !Text, allocated :: !Int64, directory :: !Text, valid :: !Bool }
  deriving stock (Show,Eq)
instance FromJSON AuditRelation where
  parseJSON = withObject "AuditRelation" $ \o -> AuditRelation <$> o .: "oid" <*> o .: "kind"
    <*> o .: "name" <*> o .: "allocated" <*> o .: "directory" <*> o .: "valid"

data HostIdentity = HostIdentity
  { serverAddress :: !Text, serverPort :: !Word16, clientAddress :: !Text, clientPort :: !Word16
  , backendPid :: !Word32, postmasterPid :: !Word32, backendStartUs :: !Int64, postmasterStartUs :: !Int64
  , dataDirectory :: !Text, systemIdentifier :: !Text, databaseOid :: !Int64, relations :: ![AuditRelation] }
  deriving stock (Eq)
instance FromJSON HostIdentity where
  parseJSON = withObject "AuditHostIdentity" $ \o -> HostIdentity
    <$> o .: "server_address" <*> o .: "server_port" <*> o .: "client_address" <*> o .: "client_port"
    <*> o .: "backend_pid" <*> o .: "postmaster_pid" <*> o .: "backend_start_us" <*> o .: "postmaster_start_us"
    <*> o .: "data_directory" <*> o .: "system_identifier" <*> o .: "database_oid" <*> o .: "relations"

data VolumeCapacity = VolumeCapacity { key :: !Text, available :: !Integer, totalFree :: !Integer }
  deriving stock (Show,Eq)
data HostCapacity = HostCapacity { volumes :: ![VolumeCapacity], directoryVolumes :: ![Int] }
  deriving stock (Show,Eq)
newtype AuditHost = AuditHost (Ptr ())

foreign import ccall safe "hmem_audit_host_open" c_open :: CString -> Word16 -> CString -> Word16
  -> Word32 -> Word32 -> Int64 -> Int64 -> CString -> CString -> Ptr CString -> Word32 -> Ptr (Ptr ()) -> IO CInt
foreign import ccall safe "hmem_audit_host_refresh" c_refresh :: Ptr () -> IO CInt
foreign import ccall safe "hmem_audit_host_backend_exited" c_backendExited :: Ptr () -> Ptr CInt -> IO CInt
foreign import ccall unsafe "hmem_audit_host_close" c_close :: Ptr () -> IO ()
foreign import ccall unsafe "hmem_audit_host_volume_count" c_volumeCount :: Ptr () -> IO Word32
foreign import ccall unsafe "hmem_audit_host_directory_count" c_directoryCount :: Ptr () -> IO Word32
foreign import ccall unsafe "hmem_audit_host_directory_volume" c_directoryVolume :: Ptr () -> Word32 -> IO CInt
foreign import ccall unsafe "hmem_audit_host_volume_read" c_read :: Ptr () -> Word32 -> Ptr Word64 -> Ptr Word64 -> IO CInt
foreign import ccall unsafe "hmem_audit_host_volume_key" c_key :: Ptr () -> Word32 -> CString -> CSize -> IO CInt

hostReason :: CInt -> Text
hostReason code = case code of
  1 -> "native_host_unsupported_platform_or_pg_format"
  2 -> "native_host_invalid_proof"
  3 -> "native_host_connection_not_bound"
  4 -> "native_host_process_unavailable"
  5 -> "native_host_identity_mismatch"
  6 -> "native_host_requires_same_sid_and_logon"
  7 -> "native_host_path_remote_unavailable_or_non_ntfs"
  8 -> "native_host_volume_capacity_unknown"
  9 -> "native_host_identity_changed"
  10 -> "native_host_allocation_failed"
  11 -> "native_host_limit_exceeded"
  _ -> "native_host_unknown_failure"

openAuditHost :: HostIdentity -> IO (Either Text AuditHost)
openAuditHost proof
  | length proof.relations > 254 = pure (Left "relation_limit")
  | otherwise = withMany BS.useAsCString (map TE.encodeUtf8
      ([proof.serverAddress,proof.clientAddress,proof.dataDirectory,proof.systemIdentifier] <> map (.directory) proof.relations)) $ \strings ->
      case strings of
        server:client:datadir:system:directories -> withArray directories $ \paths -> alloca $ \output -> do
          poke output nullPtr
          code <- c_open server proof.serverPort client proof.clientPort proof.backendPid proof.postmasterPid
            proof.backendStartUs proof.postmasterStartUs datadir system paths (fromIntegral (length directories)) output
          if code == 0 then Right . AuditHost <$> peek output else pure (Left (hostReason code))
        _ -> pure (Left "native_host_invalid_proof")

closeAuditHost :: AuditHost -> IO ()
closeAuditHost (AuditHost owner) = c_close owner

withAuditHost :: HostIdentity -> (AuditHost -> IO a) -> IO (Either Text a)
withAuditHost=withAuditHostWith (pure ())

-- Publication seam runs only after a cleanup owner is installed, while masked.
-- Interruptible waits in the seam and exceptions delivered before the restored
-- action therefore release the same actual native capability.
withAuditHostWith :: IO () -> HostIdentity -> (AuditHost -> IO a) -> IO (Either Text a)
withAuditHostWith published proof action=mask $ \restore -> openAuditHost proof >>= \case
  Left reason -> pure (Left reason)
  Right owner -> (published >> (Right <$> restore (action owner))) `finally` closeAuditHost owner

sampleAuditHost :: AuditHost -> IO (Either Text HostCapacity)
sampleAuditHost (AuditHost owner) = do
  refreshed <- c_refresh owner
  if refreshed /= 0 then pure (Left (hostReason refreshed)) else do
    count <- c_volumeCount owner
    directories <- c_directoryCount owner
    if count > 256 || directories > 256 then pure (Left "native_host_limit_exceeded") else do
      readings <- forM [0 .. fromIntegral count-1 :: Int] $ \i ->
        alloca $ \available -> alloca $ \free -> allocaBytes 64 $ \key -> do
          code <- c_read owner (fromIntegral i) available free
          keyCode <- c_key owner (fromIntegral i) key 64
          if code /= 0 || keyCode /= 0 then pure Nothing else do
            a <- toInteger <$> peek available
            f <- toInteger <$> peek free
            k <- TE.decodeUtf8 <$> BS.packCString key
            pure (Just (VolumeCapacity k a f))
      mapping <- mapM (fmap fromIntegral . c_directoryVolume owner . fromIntegral) [0 .. fromIntegral directories-1 :: Int]
      pure $ case sequence readings of
        Just values | all (\i -> i>=0 && i<length values) mapping -> Right (HostCapacity values mapping)
        _ -> Left "native_host_capacity_unknown"

backendHasExited :: AuditHost -> IO (Either Text Bool)
backendHasExited (AuditHost owner)=alloca $ \exited -> do
  result<-c_backendExited owner exited
  if result==0 then Right . (/=0) <$>peek exited else pure (Left (hostReason result))
