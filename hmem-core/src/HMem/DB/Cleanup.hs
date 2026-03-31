module HMem.DB.Cleanup
  ( runCleanup
  , getCleanupPolicies
  , upsertCleanupPolicy
  ) where

import Control.Exception (throwIO)
import Data.Functor.Contravariant ((>$<))
import Data.Int (Int16, Int32, Int64)
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

softDeleteMemoriesS :: [UUID] -> Session.Session Int64
softDeleteMemoriesS [] = pure 0
softDeleteMemoriesS ids = do
  n <- Session.statement () $ runN $
    update Update
      { target = memorySchema
      , from = pure ()
      , set = \_ row -> row { memDeletedAt = deletedNow }
      , updateWhere = \_ row -> in_ row.memId (map lit ids) &&. activeMemory row
      , returning = NoReturning
      }
  Session.statement () $ run_ $
    delete Delete
      { from = memoryLinkSchema
      , using = pure ()
      , deleteWhere = \_ row -> in_ row.mlSourceId (map lit ids) ||. in_ row.mlTargetId (map lit ids)
      , returning = NoReturning
      }
  Session.statement () $ run_ $
    delete Delete
      { from = memoryTagSchema
      , using = pure ()
      , deleteWhere = \_ row -> in_ row.mtMemoryId (map lit ids)
      , returning = NoReturning
      }
  Session.statement () $ run_ $
    delete Delete
      { from = memoryCategoryLinkSchema
      , using = pure ()
      , deleteWhere = \_ row -> in_ row.mclMemoryId (map lit ids)
      , returning = NoReturning
      }
  Session.statement () $ run_ $
    delete Delete
      { from = projectMemoryLinkSchema
      , using = pure ()
      , deleteWhere = \_ row -> in_ row.pmlMemoryId (map lit ids)
      , returning = NoReturning
      }
  Session.statement () $ run_ $
    delete Delete
      { from = taskMemoryLinkSchema
      , using = pure ()
      , deleteWhere = \_ row -> in_ row.tmlMemoryId (map lit ids)
      , returning = NoReturning
      }
  pure n

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
          where_ $ activeMemory row
          where_ $ row.memImportance <. lit imp
          where_ $ row.memCreatedAt <. cutoff
          absent $ do
            link <- each memoryLinkSchema
            where_ $ link.mlSourceId ==. row.memId ||. link.mlTargetId ==. row.memId
          pure row.memId
        case idsToDelete of
          [] -> pure 0
          _  -> softDeleteMemoriesS idsToDelete

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
          where_ $ activeMemory row
          where_ $ row.memImportance <. lit imp
          -- Exclude memories in the top-N (stays in SQL)
          absent $ do
            kept <- limit (fromIntegral maxN) $
              orderBy (((\r -> r.memImportance) >$< desc) <> ((\r -> r.memLastAccessedAt) >$< desc)) $ do
                r <- each memorySchema
                where_ $ r.memWorkspaceId ==. lit wsId
                where_ $ r.memMemoryType ==. lit policy.memoryType
                where_ $ activeMemory r
                pure r
            where_ $ kept.memId ==. row.memId
          -- Exclude linked memories
          absent $ do
            link <- each memoryLinkSchema
            where_ $ link.mlSourceId ==. row.memId ||. link.mlTargetId ==. row.memId
          pure row.memId
        case idsToDelete of
          [] -> pure 0
          _  -> softDeleteMemoriesS idsToDelete

------------------------------------------------------------------------
-- Policy CRUD
------------------------------------------------------------------------

getCleanupPolicies :: Pool Hasql.Connection -> UUID -> Maybe Int -> Maybe Int -> IO [CleanupPolicy]
getCleanupPolicies pool wsId mlimit moffset = do
  let (lim, off) = capPagination mlimit moffset
  rows <- runSession pool $ Session.statement () $ run $ select $
    limit (fromIntegral lim) $ offset (fromIntegral off) $ do
      row <- each cleanupPolicySchema
      where_ $ row.cpWorkspaceId ==. lit wsId
      present $ do
        ws <- each workspaceSchema
        where_ $ ws.wsId ==. row.cpWorkspaceId
        where_ $ activeWorkspace ws
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
          &&. activeMemory row
            &&. row.memLastAccessedAt <. cutoff
            &&. row.memImportance >. lit (1 :: Int16)
        , returning = NoReturning
        }


