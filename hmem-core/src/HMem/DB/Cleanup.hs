module HMem.DB.Cleanup
  ( runCleanup
  , getCleanupPolicies
  , upsertCleanupPolicy
  ) where

import Control.Exception (throwIO)
import Data.Functor.Contravariant ((>$<))
import Data.Int (Int16, Int32, Int64)
import Data.Maybe (fromMaybe)
import Data.Pool (Pool)
import Data.Time (UTCTime)
import Data.UUID (UUID)
import Hasql.Connection qualified as Hasql
import Hasql.Session qualified as Session
import Rel8 hiding (filter)

import HMem.DB.Pool (runSession, runTransaction, DBException(..))
import HMem.DB.Schema
import HMem.Types

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

rowToCleanupPolicy :: CleanupPolicyT Result -> CleanupPolicy
rowToCleanupPolicy r = CleanupPolicy
  { id            = r.cpId
  , workspaceId   = r.cpWorkspaceId
  , memoryType    = r.cpMemoryType
  , maxAgeHours   = fmap fromIntegral r.cpMaxAgeHours
  , maxCount      = fmap fromIntegral r.cpMaxCount
  , minImportance = fromIntegral r.cpMinImportance
  , enabled       = r.cpEnabled
  , createdAt     = r.cpCreatedAt
  , updatedAt     = r.cpUpdatedAt
  }

------------------------------------------------------------------------
-- Run cleanup for a workspace
------------------------------------------------------------------------

-- | Run cleanup according to all enabled policies for the given workspace.
-- Returns the total number of deleted memories.
runCleanup :: Pool Hasql.Connection -> UUID -> IO CleanupResult
runCleanup pool wsId = do
  policies <- getCleanupPolicies pool wsId Nothing Nothing
  total <- Prelude.sum <$> mapM (applyPolicy pool wsId) (Prelude.filter (.enabled) policies)
  pure CleanupResult
    { deletedCount = fromIntegral total
    , workspaceId  = wsId
    }

applyPolicy :: Pool Hasql.Connection -> UUID -> CleanupPolicy -> IO Int64
applyPolicy pool wsId policy = do
  _decayed <- decayStaleImportance pool wsId policy
  aged <- cleanByAge pool wsId policy
  excess <- cleanByCount pool wsId policy
  pure (aged + excess)

-- | Delete memories older than max_age_hours with importance < min_importance.
-- Uses NOT EXISTS to protect linked memories without loading all IDs.
cleanByAge :: Pool Hasql.Connection -> UUID -> CleanupPolicy -> IO Int64
cleanByAge pool wsId policy =
  case policy.maxAgeHours of
    Nothing -> pure 0
    Just hours -> do
      let imp = fromIntegral policy.minImportance :: Int16
          cutoff = unsafeLiteral ("now() - make_interval(hours := " ++ show (hours :: Int) ++ ")") :: Expr UTCTime
      runTransaction pool $ do
        -- Select IDs to delete, excluding linked memories via NOT EXISTS
        idsToDelete <- Session.statement () $ run $ select $ do
          row <- each memorySchema
          where_ $ row.memWorkspaceId ==. lit wsId
          where_ $ row.memMemoryType ==. lit policy.memoryType
          where_ $ row.memImportance <. lit imp
          where_ $ row.memCreatedAt <. cutoff
          absent $ do
            link <- each memoryLinkSchema
            where_ $ link.mlSourceId ==. row.memId ||. link.mlTargetId ==. row.memId
          pure row.memId
        case idsToDelete of
          [] -> pure 0
          _  -> Session.statement () $ runN $
            delete Delete
              { from = memorySchema
              , using = pure ()
              , deleteWhere = \_ row -> in_ row.memId (map lit idsToDelete)
              , returning = NoReturning
              }

-- | If there are more than max_count memories, delete the least important / oldest.
-- Uses NOT EXISTS to protect linked memories without loading all IDs.
cleanByCount :: Pool Hasql.Connection -> UUID -> CleanupPolicy -> IO Int64
cleanByCount pool wsId policy =
  case policy.maxCount of
    Nothing -> pure 0
    Just maxN -> do
      let imp = fromIntegral policy.minImportance :: Int16
      runTransaction pool $ do
        -- Select IDs to delete with the top-N exclusion kept as a SQL
        -- subquery (NOT EXISTS) so that the keep-list never leaves
        -- PostgreSQL, avoiding a large IN (...) literal list.
        idsToDelete <- Session.statement () $ run $ select $ do
          row <- each memorySchema
          where_ $ row.memWorkspaceId ==. lit wsId
          where_ $ row.memMemoryType ==. lit policy.memoryType
          where_ $ row.memImportance <. lit imp
          -- Exclude memories in the top-N (stays in SQL)
          absent $ do
            kept <- limit (fromIntegral maxN) $
              orderBy (((\r -> r.memImportance) >$< desc) <> ((\r -> r.memLastAccessedAt) >$< desc)) $ do
                r <- each memorySchema
                where_ $ r.memWorkspaceId ==. lit wsId
                where_ $ r.memMemoryType ==. lit policy.memoryType
                pure r
            where_ $ kept.memId ==. row.memId
          -- Exclude linked memories
          absent $ do
            link <- each memoryLinkSchema
            where_ $ link.mlSourceId ==. row.memId ||. link.mlTargetId ==. row.memId
          pure row.memId
        case idsToDelete of
          [] -> pure 0
          _  -> Session.statement () $ runN $
            delete Delete
              { from = memorySchema
              , using = pure ()
              , deleteWhere = \_ row -> in_ row.memId (map lit idsToDelete)
              , returning = NoReturning
              }

------------------------------------------------------------------------
-- Policy CRUD
------------------------------------------------------------------------

getCleanupPolicies :: Pool Hasql.Connection -> UUID -> Maybe Int -> Maybe Int -> IO [CleanupPolicy]
getCleanupPolicies pool wsId mlimit moffset = do
  let lim = fromMaybe 50 mlimit
      off = fromMaybe 0  moffset
  rows <- runSession pool $ Session.statement () $ run $ select $
    limit (fromIntegral lim) $ offset (fromIntegral off) $ do
      row <- each cleanupPolicySchema
      where_ $ row.cpWorkspaceId ==. lit wsId
      pure row
  pure $ map rowToCleanupPolicy rows

upsertCleanupPolicy :: Pool Hasql.Connection -> UpsertCleanupPolicy -> IO CleanupPolicy
upsertCleanupPolicy pool p = do
  rows <- runSession pool $ Session.statement () $ run $
    insert Insert
      { into = cleanupPolicySchema
      , rows = values
          [ CleanupPolicyT
              { cpId            = unsafeDefault
              , cpWorkspaceId   = lit p.workspaceId
              , cpMemoryType    = lit p.memoryType
              , cpMaxAgeHours   = lit (fmap fromIntegral p.maxAgeHours :: Maybe Int32)
              , cpMaxCount      = lit (fmap fromIntegral p.maxCount :: Maybe Int32)
              , cpMinImportance = lit (fromIntegral p.minImportance :: Int16)
              , cpEnabled       = lit p.enabled
              , cpCreatedAt     = unsafeDefault
              , cpUpdatedAt     = unsafeDefault
              }
          ]
      , onConflict = DoUpdate Upsert
          { index = \tbl -> (tbl.cpWorkspaceId, tbl.cpMemoryType)
          , predicate = Nothing
          , set = \new _old -> new
              { cpId        = _old.cpId
              , cpCreatedAt = _old.cpCreatedAt
              , cpUpdatedAt = unsafeDefault
              }
          , updateWhere = \_ _ -> lit True
          }
      , returning = Returning id
      }
  case rows of
    (r:_) -> pure $ rowToCleanupPolicy r
    []    -> throwIO $ DBOtherError "upsertCleanupPolicy: INSERT returned no rows"

------------------------------------------------------------------------
-- Importance decay
------------------------------------------------------------------------

-- | Exponential decay for stale memories (not accessed within max_age_hours).
-- Reduces importance to ~80%% of current value: GREATEST(1, (importance * 4) / 5).
-- Importance will not drop below 1.  Returns the number of decayed rows.
decayStaleImportance :: Pool Hasql.Connection -> UUID -> CleanupPolicy -> IO Int64
decayStaleImportance pool wsId policy = case policy.maxAgeHours of
  Nothing -> pure 0
  Just hours -> do
    let cutoff = unsafeLiteral ("now() - make_interval(hours := " ++ show (hours :: Int) ++ ")") :: Expr UTCTime
    runSession pool $ Session.statement () $ runN $
      update Update
        { target = memorySchema
        , from = pure ()
        , set = \_ row ->
            let decayed = rawBinaryOperator "/" (row.memImportance * lit (4 :: Int16)) (lit (5 :: Int16)) :: Expr Int16
            in row { memImportance = boolExpr (lit 1) decayed (decayed >. lit 1) }
        , updateWhere = \_ row ->
            row.memWorkspaceId ==. lit wsId
            &&. row.memMemoryType ==. lit policy.memoryType
            &&. row.memLastAccessedAt <. cutoff
            &&. row.memImportance >. lit (1 :: Int16)
        , returning = NoReturning
        }


