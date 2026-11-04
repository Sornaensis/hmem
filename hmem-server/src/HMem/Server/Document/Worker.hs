-- | Bounded controller for the qualified document-preview launcher.
-- Native handles remain owned here until their sole reapers and pipe users join.
module HMem.Server.Document.Worker
  ( PreviewWorker, startPreviewWorker, stopPreviewWorker, withPreviewWorker ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad (unless,when,void,forM_)
import Data.IORef
import Data.Map.Strict qualified as Map
import Data.Pool (Pool)
import Data.Text (Text)
import Data.Time (getCurrentTime,diffUTCTime)
import Data.UUID (UUID)
import Data.UUID.V4 qualified as UUID
import Hasql.Connection qualified as H
import System.Directory (doesFileExist,findExecutable,canonicalizePath)
import System.Timeout (timeout)
import HMem.DB.DocumentPreview qualified as DB
import HMem.DB.Pool (DBException(..))
import HMem.Document.Preview
import HMem.Document.Types (DocumentException(..))
import HMem.Server.Document.Launcher qualified as Launcher

data OwnedJob=OwnedJob
  { run :: !PreviewRun, deadline :: !Launcher.Deadline, task :: !(Async ()) }
data PreviewWorker=PreviewWorker
  { closing :: !(IORef Bool), monitor :: !(Async ())
  , publication :: !(MVar ())
  , jobs :: !(MVar(Map.Map UUID OwnedJob))
  , natives :: !Launcher.NativeRegistry }
newtype PreviewShutdownFailure=PreviewShutdownFailure Text deriving stock(Show)
instance Exception PreviewShutdownFailure

trySynchronous :: IO a -> IO(Either SomeException a)
trySynchronous action=try action >>= \case
  Left failure | Just _<-(fromException failure::Maybe SomeAsyncException)->throwIO failure
  result->pure result
bounded :: IO a -> IO a
bounded action=timeout 2000000 action >>=maybe(throwIO(PreviewShutdownFailure "preview_database_unavailable"))pure

startPreviewWorker :: Pool H.Connection -> PreviewConfig -> IO PreviewWorker
startPreviewWorker pool requested=mask $ \restore->do
  closing<-newIORef False
  jobs<-newMVar Map.empty
  natives<-Launcher.newNativeRegistry
  publication<-newMVar ()
  controller<-UUID.nextRandom
  resolved<-restore $ trySynchronous $ if not requested.enabled ||not(validPreviewConfig requested) then pure Nothing else do
    path<-maybe(pure Nothing)(\path->do
      exists<-doesFileExist path
      if exists then Just <$>canonicalizePath path else pure Nothing)requested.launcherPath
    python<-findExecutable requested.pythonExecutable
    pure $ case(path,python)of
      (Just launcher,Just executable)->Just requested{launcherPath=Just launcher,pythonExecutable=executable}
      _->Nothing
  let configuration=either(const Nothing)id resolved
  monitor<-async $ case configuration of
    Nothing->unavailableLoop closing
    Just config->loop publication closing jobs natives controller config
  pure(PreviewWorker closing monitor publication jobs natives)
  where
    unavailableLoop closing=do
      stopping<-readIORef closing
      unless stopping $ do
        void $ trySynchronous $ bounded $ do
          DB.recoverDue pool
          DB.nextJob pool >>=mapM_(\(workspace,document,_)->void(DB.unavailable pool workspace document))
        threadDelay 1000000
        unavailableLoop closing
    loop publication closing jobs natives controller config=do
      stopping<-readIORef closing
      unless stopping $ do
        -- A database or launcher outage leaves originals readable. A stale
        -- heartbeat cannot admit a new job; unresolved owners stay quarantined.
        void $ trySynchronous $ bounded $ do
          DB.heartbeat pool config
          DB.recoverDue pool
        modifyMVar_ jobs $ \current->do
          alive<-mapM (\(key,owned)->do
            result<-poll owned.task
            pure(case result of Nothing->Just(key,owned); Just _->Nothing))(Map.toList current)
          pure(Map.fromList [entry | Just entry<-alive])
        current<-readMVar jobs
        withMVar publication $ \_->do
          closingNow<-readIORef closing
          when(not closingNow&&Map.size current<config.workers) $ do
            candidate<-trySynchronous(bounded(DB.nextJob pool))
            case candidate of
              Right(Just(workspace,document,media))->do
                owner<-UUID.nextRandom; token<-UUID.nextRandom
                claimed<-trySynchronous(bounded(DB.claim pool config workspace document owner controller token media))
                case claimed of
                  Right(Just run)->do
                    now<-getCurrentTime
                    deadline<-Launcher.deadlineFromRemaining
                      (fromInteger(max 0(min 120000000(floor(diffUTCTime run.deadlineAt now*1000000)))))
                    -- Install the async in the bounded registry before it can
                    -- spawn. Shutdown cannot miss an in-flight acquisition.
                    gate<-newEmptyMVar
                    task<-async(takeMVar gate >>runJob publication closing natives config run deadline)
                    modifyMVar_ jobs(pure . Map.insert run.id(OwnedJob run deadline task))
                    putMVar gate ()
                  _->pure()
              _->pure()
        threadDelay 200000
        loop publication closing jobs natives controller config

    runJob publication closing natives config run deadline=do
      launchAttempted<-newIORef False
      settledProof<-newIORef Nothing
      let register _=pure()
          settle outcome=do
            writeIORef settledProof(Just outcome.physical)
            when outcome.physical.nativeExited $ bounded $
              DB.closed pool run outcome.physical.cleanupVerified outcome.physical.creationUncertain
            when(outcome.physical.nativeExited&&outcome.physical.cleanupVerified&&not outcome.physical.creationUncertain) $
              Launcher.forgetNativeOwner natives run.id
          hooks=Launcher.LauncherHooks register
            (\pid->bounded(DB.started pool run pid))
            (\_->bounded(DB.inspected pool run))
            (\inspection->bounded(DB.reserve pool run inspection.planSha256
              (length inspection.outputShape)inspection.pageCount 105906176))
            (bounded(DB.rendering pool run)) settle
          failKnown code automatic=void(bounded(DB.failRun pool run code automatic))
      attempted<-try @SomeException $ do
        original<-Launcher.within(Launcher.workDeadline deadline)(DB.readOriginal pool run)
        stopped<-readIORef closing
        when stopped(throwIO(Launcher.ProtocolFailure "cancelled"))
        writeIORef launchAttempted True
        outcome<-Launcher.launchGeneration natives config run original deadline hooks
        case outcome.result of
          Left code->do
            let failureCode=maybe code id(outcome.cause >>= databaseFailureCode)
            failKnown failureCode(failureCode=="worker_crashed"||failureCode=="worker_failed")
          Right verified->Launcher.within deadline $ withMVar publication $ \_->do
            stoppedNow<-readIORef closing
            if stoppedNow then failKnown "cancelled" False
              else DB.publish pool run verified.metadata verified.outputs
      case attempted of
        Right()->pure()
        Left failure->do
          native<-Map.lookup run.id <$>readIORef natives
          case native of
            Nothing->do
              -- No child was installed. Acquisition failures before successful
              -- createProcess and read/hash failures have no daemon producer.
              -- A settled callback already removed a fully joined child.
              attemptedLaunch<-readIORef launchAttempted
              proof<-readIORef settledProof
              let verified=not attemptedLaunch || case proof of
                    Just physical->physical.nativeExited&&physical.cleanupVerified&&not physical.creationUncertain
                    Nothing->False
              when verified(void(trySynchronous(bounded(DB.closed pool run True False))))
              void(trySynchronous(failKnown(if verified then classify failure else "cleanup_unverified")False))
            Just children->do
              joined<-and <$>mapM(Launcher.stopNative deadline)children
              proof<-readIORef settledProof
              let verified=joined&&case proof of
                    Just physical->physical.nativeExited&&physical.cleanupVerified&&not physical.creationUncertain
                    Nothing->False
              -- Preserve a verified receipt if its first DB acknowledgement
              -- failed. Current absence alone never establishes this proof.
              when joined $void(trySynchronous(bounded(DB.closed pool run verified(not verified))))
              when verified(Launcher.forgetNativeOwner natives run.id)
              void(trySynchronous(failKnown(if verified then classify failure else "cleanup_unverified")False))
          case fromException failure::Maybe SomeAsyncException of
            Just _->throwIO failure
            Nothing->pure()

classify :: SomeException -> Text
classify failure
  | Just code<-databaseFailureCode failure=code
  | Just(DocumentException code)<-fromException failure=code
  | Just(DBCheckViolation code)<-fromException failure=code
  | Just(Launcher.ProtocolFailure code)<-fromException failure=code
  | otherwise="renderer_unavailable"

-- A reservation rejection and an expired or stale attempt cannot be fixed by
-- an automatic native retry. Keep the existing transient-worker retry policy.
databaseFailureCode :: SomeException -> Maybe Text
databaseFailureCode failure
  | Just(DBCheckViolation "document_quota_exceeded")<-fromException failure=Just "quota_exceeded"
  | Just(DBCheckViolation code)<-fromException failure
  , code `elem` ["document_preview_deadline","document_preview_stale_run","document_stale_preview_attempt"]=Just code
  | Just(DocumentException "document_preview_deadline")<-fromException failure=Just "document_preview_deadline"
  | otherwise=Nothing

-- Close admission first and let each actual owner finish within its original
-- absolute deadline. Never publish after shutdown begins, signal an old PID,
-- or equate a dead CLI with physical container closure.
stopPreviewWorker :: PreviewWorker -> IO()
stopPreviewWorker worker=mask $ \restore->do
  withMVar worker.publication(\_->writeIORef worker.closing True)
  monitorJoined<-restore(timeout 5000000(waitCatch worker.monitor))
  case monitorJoined of
    Nothing->throwIO(PreviewShutdownFailure "preview_controller_join_unverified")
    Just _->pure()
  owned<-Map.elems <$>readMVar worker.jobs
  forM_ owned $ \job->do
    remaining<-Launcher.remainingMicros job.deadline
    finished<-restore(timeout(max 1 remaining)(waitCatch job.task))
    case finished of
      Nothing->throwIO(PreviewShutdownFailure "preview_generation_join_unverified")
      Just _->pure()
  nativeOwners<-Map.elems <$>readIORef worker.natives
  unless(null nativeOwners)(throwIO(PreviewShutdownFailure "cleanup_unverified"))

withPreviewWorker :: Pool H.Connection -> PreviewConfig -> IO a -> IO a
withPreviewWorker pool config=bracket(startPreviewWorker pool config)stopPreviewWorker . const
