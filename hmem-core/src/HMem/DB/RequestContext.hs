module HMem.DB.RequestContext
  ( ActorType(..)
  , actorTypeToText
  , ChangeCause(..)
  , changeCauseToText
  , PrincipalAuthority(..)
  , Principal(..)
  , EffectiveActor(..)
  , resolveEffectiveActor
  , RequestContext(..)
  , emptyRequestContext
  , withRequestContext
  , withRequestIdContext
  , withPrincipalContext
  , withEffectiveActorContext
  , withWorkspaceIdContext
  , withChangeCauseContext
  , currentRequestContext
  , currentRequestId
  , currentPrincipal
  , currentEffectiveActor
  , currentWorkspaceId
  , currentChangeCause
  ) where

import Control.Concurrent (ThreadId, myThreadId)
import Control.Exception (bracket)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.UUID (UUID)
import System.IO.Unsafe (unsafePerformIO)
import HMem.BotIdentity (BotIdentity, botRole, localDeclaredBotId)

data ActorType
  = ActorUser
  | ActorBot
  deriving stock (Show, Eq)

actorTypeToText :: ActorType -> Text
actorTypeToText ActorUser = "user"
actorTypeToText ActorBot  = "bot"

data PrincipalAuthority
  = PrincipalNoAuthority
  | PrincipalGrantUser !UUID
  | PrincipalSyntheticLocalSuperadmin
  deriving stock (Show, Eq)

data Principal = Principal
  { actorType  :: !ActorType
  , actorId    :: !Text
  , actorLabel :: !Text
  , authority  :: !PrincipalAuthority
  } deriving stock (Show, Eq)

-- | Operation attribution has no authority. Keep Principal for every grant,
-- session and stream-audience decision, including implicit local superadmin.
data EffectiveActor = EffectiveActor
  { actorType :: !ActorType
  , actorId :: !Text
  , actorLabel :: !Text
  , botIdentity :: !(Maybe BotIdentity)
  } deriving stock (Show, Eq)

resolveEffectiveActor :: Bool -> Principal -> Maybe BotIdentity -> EffectiveActor
resolveEffectiveActor isLocal original declaration = case declaration of
  Just identity
    | isLocal, original.actorType == ActorUser, original.actorId == "local-user"
    , original.authority == PrincipalSyntheticLocalSuperadmin ->
        EffectiveActor ActorBot (localDeclaredBotId identity) (botRole identity) declaration
  _ -> EffectiveActor original.actorType original.actorId original.actorLabel declaration

-- | The initiating writer is deliberately a closed set.  It is persisted by
-- V022 and must never be populated from arbitrary client text.
data ChangeCause
  = ChangeCauseRest
  | ChangeCauseMcp
  | ChangeCauseAuditRevert
  | ChangeCauseCore
  | ChangeCauseMigration
  deriving stock (Show, Eq)

changeCauseToText :: ChangeCause -> Text
changeCauseToText = \case
  ChangeCauseRest        -> "rest"
  ChangeCauseMcp         -> "mcp"
  ChangeCauseAuditRevert -> "audit_revert"
  ChangeCauseCore        -> "core"
  ChangeCauseMigration   -> "migration"

data RequestContext = RequestContext
  { requestId   :: !(Maybe Text)
  , principal   :: !(Maybe Principal)
  , effectiveActor :: !(Maybe EffectiveActor)
  , workspaceId :: !(Maybe UUID)
  , changeCause :: !(Maybe ChangeCause)
  } deriving stock (Show, Eq)

emptyRequestContext :: RequestContext
emptyRequestContext = RequestContext
  { requestId = Nothing
  , principal = Nothing
  , effectiveActor = Nothing
  , workspaceId = Nothing
  , changeCause = Nothing
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
    update ctx = ctx { principal = mPrincipal, effectiveActor = Nothing }

withEffectiveActorContext :: Maybe EffectiveActor -> IO a -> IO a
withEffectiveActorContext actor = bracket (pushContext (\ctx -> ctx { effectiveActor = actor })) popContext . const

withWorkspaceIdContext :: Maybe UUID -> IO a -> IO a
withWorkspaceIdContext mWorkspaceId = bracket (pushContext update) popContext . const
  where
    update ctx = ctx { workspaceId = mWorkspaceId }

withChangeCauseContext :: Maybe ChangeCause -> IO a -> IO a
withChangeCauseContext mCause = bracket (pushContext update) popContext . const
  where
    update ctx = ctx { changeCause = mCause }

currentRequestContext :: IO RequestContext
currentRequestContext = do
  tid <- myThreadId
  maybe emptyRequestContext id . Map.lookup tid <$> readIORef requestContextsRef

currentRequestId :: IO (Maybe Text)
currentRequestId = (.requestId) <$> currentRequestContext

currentPrincipal :: IO (Maybe Principal)
currentPrincipal = (.principal) <$> currentRequestContext

currentEffectiveActor :: IO (Maybe EffectiveActor)
currentEffectiveActor = do
  ctx <- currentRequestContext
  pure $ case ctx.effectiveActor of
    Just actor -> Just actor
    Nothing -> fmap (\p -> resolveEffectiveActor False p Nothing) ctx.principal

currentWorkspaceId :: IO (Maybe UUID)
currentWorkspaceId = (.workspaceId) <$> currentRequestContext

currentChangeCause :: IO (Maybe ChangeCause)
currentChangeCause = (.changeCause) <$> currentRequestContext

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
