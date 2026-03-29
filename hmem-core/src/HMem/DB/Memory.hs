module HMem.DB.Memory
  ( -- * CRUD
    createMemory
  , createMemoryBatch
  , getMemory
  , updateMemory
  , deleteMemory

    -- * Querying
  , listMemories
  , listMemoriesWithQuery
  , searchMemories

    -- * Tags
  , setTags
  , getTags

    -- * Links
  , linkMemories
  , unlinkMemories
  , getMemoryLinks

    -- * Access tracking
  , touchMemory
  , touchMemoryBatch

    -- * Importance
  , adjustImportance

    -- * Pinning
  , togglePin

    -- * Graph traversal
  , getRelatedGraph
  , findByRelation

    -- * Cross-entity memory lists
  , getProjectMemories
  , getTaskMemories

    -- * Activity timeline
  , getRecentActivity

    -- * Embedding / vector similarity
  , setEmbedding
  , similarMemories
  ) where

import Control.Exception (throwIO)
import Data.Aeson (Object, Value, toJSON)
import Data.ByteString.Char8 qualified as BS8
import Data.Functor.Contravariant ((>$<), contramap)
import Data.Int (Int16, Int32)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Pool (Pool)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime)
import Data.UUID (UUID)
import Hasql.Connection qualified as Hasql
import Hasql.Decoders qualified as Dec
import Hasql.Encoders qualified as Enc
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement
import Rel8

import HMem.DB.Pool (runSession, runTransaction, DBException(..))
import HMem.DB.Schema
import HMem.Types

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

-- | Convert a MemoryT Result to a Memory domain type (without tags).
rowToMemory :: MemoryT Result -> [Text] -> Memory
rowToMemory r tags = Memory
  { id             = r.memId
  , workspaceId    = r.memWorkspaceId
  , content        = r.memContent
  , summary        = r.memSummary
  , memoryType     = r.memMemoryType
  , importance     = fromIntegral r.memImportance
  , metadata       = r.memMetadata
  , expiresAt      = r.memExpiresAt
  , source         = r.memSource
  , confidence     = r.memConfidence
  , pinned         = r.memPinned
  , lastAccessedAt = r.memLastAccessedAt
  , accessCount    = fromIntegral r.memAccessCount
  , ftsLanguage    = r.memFtsLanguage
  , tags           = tags
  , createdAt      = r.memCreatedAt
  , updatedAt      = r.memUpdatedAt
  }

------------------------------------------------------------------------
-- Create
------------------------------------------------------------------------

createMemory :: Pool Hasql.Connection -> CreateMemory -> IO Memory
createMemory pool cm = do
  let imp     = maybe 5 fromIntegral (cm.importance) :: Int16
      meta    = fromMaybe (toJSON (mempty :: Object)) (cm.metadata)
      conf    = fromMaybe 1.0 cm.confidence
      pin     = fromMaybe False cm.pinned
      memTags = fromMaybe [] (cm.tags)
      lang    = fromMaybe "english" cm.ftsLanguage
  rows <- runTransaction pool $ do
    rs <- Session.statement () $ run $
      insert Insert
        { into = memorySchema
        , rows = values
            [ MemoryT
                { memId             = unsafeDefault
                , memWorkspaceId    = lit cm.workspaceId
                , memContent        = lit cm.content
                , memSummary        = lit cm.summary
                , memMemoryType     = lit cm.memoryType
                , memImportance     = lit imp
                , memMetadata       = lit meta
                , memExpiresAt      = lit cm.expiresAt
                , memSource         = lit cm.source
                , memConfidence     = lit conf
                , memPinned         = lit pin
                , memLastAccessedAt = unsafeDefault
                , memAccessCount    = unsafeDefault
                , memFtsLanguage    = lit lang
                , memSearchVector   = unsafeDefault
                , memDeletedAt      = unsafeDefault
                , memCreatedAt      = unsafeDefault
                , memUpdatedAt      = unsafeDefault
                }
            ]
        , onConflict = Abort
        , returning  = Returning id
        }
    case rs of
      (r:_) -> do
        let mid = (r :: MemoryT Result).memId
        case memTags of
          [] -> pure ()
          _  -> Session.statement () $ run_ $
            insert Insert
              { into = memoryTagSchema
              , rows = values [MemoryTagT { mtMemoryId = lit mid, mtTag = lit tg } | tg <- memTags]
              , onConflict = DoNothing
              , returning = NoReturning
              }
      _ -> pure ()
    pure rs
  case rows of
    []    -> throwIO $ DBOtherError "createMemory: INSERT returned no rows"
    (r:_) -> pure $ rowToMemory r memTags

-- | Batch-create multiple memories in a single transaction.
-- Uses a single multi-row INSERT for memories and a single multi-row
-- INSERT for all tags, rather than N sequential inserts.
createMemoryBatch :: Pool Hasql.Connection -> [CreateMemory] -> IO [Memory]
createMemoryBatch _pool [] = pure []
createMemoryBatch pool cms = do
  results <- runTransaction pool $ do
    -- Insert all memories in one multi-row VALUES statement
    rs <- Session.statement () $ run $
      insert Insert
        { into = memorySchema
        , rows = values
            [ MemoryT
                { memId             = unsafeDefault
                , memWorkspaceId    = lit cm.workspaceId
                , memContent        = lit cm.content
                , memSummary        = lit cm.summary
                , memMemoryType     = lit cm.memoryType
                , memImportance     = lit (maybe 5 fromIntegral cm.importance :: Int16)
                , memMetadata       = lit (fromMaybe (toJSON (mempty :: Object)) cm.metadata)
                , memExpiresAt      = lit cm.expiresAt
                , memSource         = lit cm.source
                , memConfidence     = lit (fromMaybe 1.0 cm.confidence)
                , memPinned         = lit (fromMaybe False cm.pinned)
                , memLastAccessedAt = unsafeDefault
                , memAccessCount    = unsafeDefault
                , memFtsLanguage    = lit (fromMaybe "english" cm.ftsLanguage)
                , memSearchVector   = unsafeDefault
                , memDeletedAt      = unsafeDefault
                , memCreatedAt      = unsafeDefault
                , memUpdatedAt      = unsafeDefault
                }
            | cm <- cms
            ]
        , onConflict = Abort
        , returning  = Returning id
        }
    -- Zip returned rows (in insertion order) with original cms
    let pairs = zip rs cms
    -- Build all tag rows in one batch
    let allTags = [ MemoryTagT { mtMemoryId = lit (r :: MemoryT Result).memId, mtTag = lit tg }
                  | (r, cm) <- pairs
                  , tg <- fromMaybe [] cm.tags
                  ]
    case allTags of
      [] -> pure ()
      _  -> Session.statement () $ run_ $
        insert Insert
          { into = memoryTagSchema
          , rows = values allTags
          , onConflict = DoNothing
          , returning = NoReturning
          }
    pure pairs
  pure [rowToMemory r (fromMaybe [] cm.tags) | (r, cm) <- results]

------------------------------------------------------------------------
-- Read
------------------------------------------------------------------------

getMemory :: Pool Hasql.Connection -> UUID -> IO (Maybe Memory)
getMemory pool mid = do
  (rows, tags) <- runSession pool $ do
    rs <- Session.statement () $ run $ select $ do
      row <- each memorySchema
      where_ $ row.memId ==. lit mid
      where_ $ activeMemory row
      pure row
    ts <- case rs of
      [] -> pure []
      _  -> Session.statement () $ run $ select $
        orderBy (Prelude.id >$< asc) $ do
          row <- each memoryTagSchema
          where_ $ row.mtMemoryId ==. lit mid
          pure row.mtTag
    pure (rs, ts)
  case rows of
    []    -> pure Nothing
    (r:_) -> pure . Just $ rowToMemory r tags

------------------------------------------------------------------------
-- Update
------------------------------------------------------------------------

updateMemory :: Pool Hasql.Connection -> UUID -> UpdateMemory -> IO (Maybe Memory)
updateMemory pool mid um = do
  (rows, tags) <- runTransaction pool $ do
    rs <- Session.statement () $ run $
      update Update
        { target = memorySchema
        , from = pure ()
        , set = \_ row -> row
            { memContent    = maybe row.memContent    lit um.content
            , memSummary    = applyNullableUpdate row.memSummary um.summary
            , memMemoryType = maybe row.memMemoryType lit um.memoryType
            , memImportance = maybe row.memImportance (lit . fromIntegral) um.importance
            , memMetadata   = maybe row.memMetadata   lit um.metadata
            , memExpiresAt  = applyNullableUpdate row.memExpiresAt um.expiresAt
            , memSource     = applyNullableUpdate row.memSource um.source
            , memConfidence = maybe row.memConfidence lit um.confidence
            , memPinned     = maybe row.memPinned lit um.pinned
            }
        , updateWhere = \_ row -> row.memId ==. lit mid &&. activeMemory row
        , returning = Returning id
        }
    ts <- case rs of
      [] -> pure []
      _  -> Session.statement () $ run $ select $
        orderBy (Prelude.id >$< asc) $ do
          row <- each memoryTagSchema
          where_ $ row.mtMemoryId ==. lit mid
          pure row.mtTag
    pure (rs, ts)
  case rows of
    []    -> pure Nothing
    (r:_) -> pure . Just $ rowToMemory r tags

------------------------------------------------------------------------
-- Delete
------------------------------------------------------------------------

deleteMemory :: Pool Hasql.Connection -> UUID -> IO Bool
deleteMemory pool mid = do
  runTransaction pool $ do
    n <- Session.statement () $ runN $
      update Update
        { target = memorySchema
        , from = pure ()
        , set = \_ row -> row { memDeletedAt = deletedNow }
        , updateWhere = \_ row -> row.memId ==. lit mid &&. activeMemory row
        , returning = NoReturning
        }
    if n == 0
      then pure False
      else do
        Session.statement () $ run_ $
          delete Delete
            { from = memoryLinkSchema
            , using = pure ()
            , deleteWhere = \_ row -> row.mlSourceId ==. lit mid ||. row.mlTargetId ==. lit mid
            , returning = NoReturning
            }
        Session.statement () $ run_ $
          delete Delete
            { from = memoryTagSchema
            , using = pure ()
            , deleteWhere = \_ row -> row.mtMemoryId ==. lit mid
            , returning = NoReturning
            }
        Session.statement () $ run_ $
          delete Delete
            { from = memoryCategoryLinkSchema
            , using = pure ()
            , deleteWhere = \_ row -> row.mclMemoryId ==. lit mid
            , returning = NoReturning
            }
        Session.statement () $ run_ $
          delete Delete
            { from = projectMemoryLinkSchema
            , using = pure ()
            , deleteWhere = \_ row -> row.pmlMemoryId ==. lit mid
            , returning = NoReturning
            }
        Session.statement () $ run_ $
          delete Delete
            { from = taskMemoryLinkSchema
            , using = pure ()
            , deleteWhere = \_ row -> row.tmlMemoryId ==. lit mid
            , returning = NoReturning
            }
        pure True

------------------------------------------------------------------------
-- List
------------------------------------------------------------------------

listMemories
  :: Pool Hasql.Connection
  -> Maybe UUID      -- ^ workspace_id (Nothing = all workspaces)
  -> Maybe MemoryType
  -> Maybe Int      -- ^ limit
  -> Maybe Int      -- ^ offset
  -> IO [Memory]
listMemories pool mWsId mtype mlimit moffset =
  listMemoriesWithQuery pool MemoryListQuery
    { workspaceId = mWsId
    , memoryType = mtype
    , createdAfter = Nothing
    , createdBefore = Nothing
    , updatedAfter = Nothing
    , updatedBefore = Nothing
    , limit = mlimit
    , offset = moffset
    }

listMemoriesWithQuery :: Pool Hasql.Connection -> MemoryListQuery -> IO [Memory]
listMemoriesWithQuery pool mq = do
  let lim = fromMaybe 50 mq.limit
      off = fromMaybe 0  mq.offset
  runSession pool $ do
    rows <- Session.statement () $ run $ select $
      limit (fromIntegral lim) $ offset (fromIntegral off) $
      orderBy ((\row -> row.memCreatedAt) >$< desc) $ do
        row <- each memorySchema
        where_ $ activeMemory row
        case mq.workspaceId of
          Just wsId -> where_ $ row.memWorkspaceId ==. lit wsId
          Nothing   -> pure ()
        case mq.memoryType of
          Nothing -> pure ()
          Just t  -> where_ $ row.memMemoryType ==. lit t
        case mq.createdAfter of
          Just createdAfter -> where_ $ row.memCreatedAt >=. lit createdAfter
          Nothing -> pure ()
        case mq.createdBefore of
          Just createdBefore -> where_ $ row.memCreatedAt <=. lit createdBefore
          Nothing -> pure ()
        case mq.updatedAfter of
          Just updatedAfter -> where_ $ row.memUpdatedAt >=. lit updatedAfter
          Nothing -> pure ()
        case mq.updatedBefore of
          Just updatedBefore -> where_ $ row.memUpdatedAt <=. lit updatedBefore
          Nothing -> pure ()
        pure row
    enrichRowsS rows

------------------------------------------------------------------------
-- Full-text search
------------------------------------------------------------------------

-- | Search memories using optional full-text search, filters, and pagination.
-- The @tags@ filter uses ANY-match semantics (SQL @IN@): a memory matches if
-- it has /at least one/ of the supplied tags, not necessarily all of them.
searchMemories :: Pool Hasql.Connection -> SearchQuery -> IO [Memory]
searchMemories pool sq = do
  let lim = fromMaybe 50 (sq.limit)
      off = fromMaybe 0  (sq.offset)
      imp = maybe 1 fromIntegral (sq.minImportance) :: Int16
      applyFilters row = do
        where_ $ activeMemory row
        case sq.workspaceId of
          Just wsId -> where_ $ row.memWorkspaceId ==. lit wsId
          Nothing   -> pure ()
        where_ $ row.memImportance >=. lit imp
        case sq.memoryType of
          Just mt -> where_ $ row.memMemoryType ==. lit mt
          Nothing -> pure ()
        case sq.tags of
          Just ts@(_:_) -> present $ do
            tagRow <- each memoryTagSchema
            where_ $ tagRow.mtMemoryId ==. row.memId
            where_ $ in_ tagRow.mtTag (map lit ts)
          _ -> pure ()
        case sq.categoryId of
          Just catId -> present $ do
            catLink <- each memoryCategoryLinkSchema
            cat <- each memoryCategorySchema
            where_ $ catLink.mclMemoryId ==. row.memId
            where_ $ catLink.mclCategoryId ==. lit catId
            where_ $ cat.mcId ==. catLink.mclCategoryId
            where_ $ activeCategory cat
          Nothing -> pure ()
        case sq.pinnedOnly of
          Just True -> where_ $ row.memPinned ==. lit True
          _         -> pure ()
  rows <- case sq.query of
    Nothing -> runSession pool $ do
      rs <- Session.statement () $ run $ select $
        limit (fromIntegral lim) $ offset (fromIntegral off) $
        orderBy (((\row -> row.memImportance) >$< desc) <> ((\row -> row.memCreatedAt) >$< desc)) $ do
          row <- each memorySchema
          applyFilters row
          pure row
      enrichRowsS rs
    Just q -> do
      let searchLang = fromMaybe "english" sq.searchLanguage
      runSession pool $ do
        results <- Session.statement () $ run $ select $
          limit (fromIntegral lim) $ offset (fromIntegral off) $
          orderBy (snd >$< desc) $ do
            row <- each memorySchema
            applyFilters row
            let config = unsafeCastExpr (lit searchLang) :: Expr PgRegConfig
            let tsq = function "plainto_tsquery" (config, lit q) :: Expr PgTSQuery
            -- Use the pre-computed search_vector column (GIN-indexed)
            -- instead of recomputing to_tsvector() per row.
            let tsvec = row.memSearchVector :: Expr PgTSVector
            where_ $ rawBinaryOperator "@@" tsvec tsq
            let tsRank = function "ts_rank" (tsvec, tsq) :: Expr Double
            pure (row, tsRank)
        enrichRowsS (map fst results)
  pure rows

------------------------------------------------------------------------
-- Tags
------------------------------------------------------------------------

setTags :: Pool Hasql.Connection -> UUID -> [Text] -> IO ()
setTags pool mid tags = runTransaction pool $ do
  -- Delete existing tags
  Session.statement () $ run_ $
    delete Delete
      { from = memoryTagSchema
      , using = pure ()
      , deleteWhere = \_ row -> row.mtMemoryId ==. lit mid
      , returning = NoReturning
      }
  -- Insert new tags in a single batch
  case tags of
    [] -> pure ()
    _  -> Session.statement () $ run_ $
      insert Insert
        { into = memoryTagSchema
        , rows = values [MemoryTagT { mtMemoryId = lit mid, mtTag = lit tg } | tg <- tags]
        , onConflict = DoNothing
        , returning = NoReturning
        }

getTags :: Pool Hasql.Connection -> UUID -> IO [Text]
getTags = fetchTags

fetchTags :: Pool Hasql.Connection -> UUID -> IO [Text]
fetchTags pool mid = do
  runSession pool $ Session.statement () $ run $ select $
    orderBy (Prelude.id >$< asc) $ do
      row <- each memoryTagSchema
      present $ do
        mem <- each memorySchema
        where_ $ mem.memId ==. row.mtMemoryId
        where_ $ activeMemory mem
      where_ $ row.mtMemoryId ==. lit mid
      pure row.mtTag

------------------------------------------------------------------------
-- Links
------------------------------------------------------------------------

linkMemories :: Pool Hasql.Connection -> UUID -> CreateMemoryLink -> IO ()
linkMemories pool sourceId cml = do
  let str = fromMaybe 1.0 (cml.strength)
  runSession pool $ Session.statement () $ run_ $
    insert Insert
      { into = memoryLinkSchema
      , rows = values
          [ MemoryLinkT
              { mlSourceId     = lit sourceId
              , mlTargetId     = lit cml.targetId
              , mlRelationType = lit cml.relationType
              , mlStrength     = lit str
              , mlCreatedAt    = unsafeDefault
              }
          ]
      , onConflict = DoUpdate Upsert
          { index = \tbl -> (tbl.mlSourceId, tbl.mlTargetId, tbl.mlRelationType)
          , predicate = Nothing
          , set = \new _old -> new
          , updateWhere = \_ _ -> lit True
          }
      , returning = NoReturning
      }

getMemoryLinks :: Pool Hasql.Connection -> UUID -> IO [MemoryLink]
getMemoryLinks pool mid = do
  rows <- runSession pool $ Session.statement () $ run $ select $
    orderBy ((\row -> row.mlCreatedAt) >$< desc) $ do
      row <- each memoryLinkSchema
      present $ do
        src <- each memorySchema
        where_ $ src.memId ==. row.mlSourceId
        where_ $ activeMemory src
      present $ do
        tgt <- each memorySchema
        where_ $ tgt.memId ==. row.mlTargetId
        where_ $ activeMemory tgt
      where_ $ row.mlSourceId ==. lit mid ||. row.mlTargetId ==. lit mid
      pure row
  pure $ map linkRowToMemoryLink rows

unlinkMemories :: Pool Hasql.Connection -> UUID -> UUID -> RelationType -> IO Bool
unlinkMemories pool sourceId targetId relType = do
  n <- runSession pool $ Session.statement () $ runN $
    delete Delete
      { from = memoryLinkSchema
      , using = pure ()
      , deleteWhere = \_ row ->
          row.mlSourceId ==. lit sourceId &&.
          row.mlTargetId ==. lit targetId &&.
          row.mlRelationType ==. lit relType
      , returning = NoReturning
      }
  pure (n > 0)

linkRowToMemoryLink :: MemoryLinkT Result -> MemoryLink
linkRowToMemoryLink r = MemoryLink
  { sourceId     = r.mlSourceId
  , targetId     = r.mlTargetId
  , relationType = r.mlRelationType
  , strength     = r.mlStrength
  , createdAt    = r.mlCreatedAt
  }

------------------------------------------------------------------------
-- Access tracking
------------------------------------------------------------------------

touchMemory :: Pool Hasql.Connection -> UUID -> IO ()
touchMemory pool mid =
  runSession pool $ Session.statement () $ run_ $
    update Update
      { target = memorySchema
      , from = pure ()
      , set = \_ row -> row
          { memLastAccessedAt = function "now" ()
          , memAccessCount    = row.memAccessCount + lit 1
          }
      , updateWhere = \_ row -> row.memId ==. lit mid &&. activeMemory row
      , returning = NoReturning
      }

-- | Batch-update access counts for multiple memories in a single statement.
-- Uses unnest($1::uuid[], $2::int4[]) to do one round-trip instead of N.
touchMemoryBatch :: Pool Hasql.Connection -> [(UUID, Int)] -> IO ()
touchMemoryBatch _ [] = pure ()
touchMemoryBatch pool pairs =
  runTransaction pool $
    Session.statement (uuids, counts) touchMemoryBatchStmt
  where
    uuids  = map fst pairs
    counts = map (fromIntegral . snd) pairs :: [Int32]

touchMemoryBatchStmt :: Statement.Statement ([UUID], [Int32]) ()
touchMemoryBatchStmt = Statement.Statement sql encoder Dec.noResult True
  where
    sql = "UPDATE memories SET \
          \last_accessed_at = now(), \
          \access_count = memories.access_count + data.cnt \
          \FROM unnest($1::uuid[], $2::int4[]) AS data(id, cnt) \
          \WHERE memories.id = data.id AND memories.deleted_at IS NULL"
    encoder =
      contramap fst (Enc.param (Enc.nonNullable (Enc.foldableArray (Enc.nonNullable Enc.uuid))))
      <> contramap snd (Enc.param (Enc.nonNullable (Enc.foldableArray (Enc.nonNullable Enc.int4))))

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

-- | Batch-fetch tags for a list of memory rows and assemble Memory values.
-- Runs within an existing session to avoid N+1 connection checkouts.
enrichRowsS :: [MemoryT Result] -> Session.Session [Memory]
enrichRowsS rows = do
  let mids = map (\r -> r.memId) rows
  tagMap <- batchGetTagsS mids
  pure [rowToMemory r (Map.findWithDefault [] r.memId tagMap) | r <- rows]

batchGetTagsS :: [UUID] -> Session.Session (Map.Map UUID [Text])
batchGetTagsS [] = pure Map.empty
batchGetTagsS mids = do
  pairs <- Session.statement () $ run $ select $ do
    row <- each memoryTagSchema
    where_ $ in_ row.mtMemoryId (map lit mids)
    pure (row.mtMemoryId, row.mtTag)
  pure $ Map.fromListWith (++) [(m, [t]) | (m, t) <- pairs]

------------------------------------------------------------------------
-- Importance adjustment
------------------------------------------------------------------------

adjustImportance :: Pool Hasql.Connection -> UUID -> Int -> IO (Maybe Memory)
adjustImportance pool mid newImp = do
  let imp = fromIntegral (Prelude.max 1 (Prelude.min 10 newImp)) :: Int16
  (rows, tags) <- runTransaction pool $ do
    rs <- Session.statement () $ run $
      update Update
        { target = memorySchema
        , from = pure ()
        , set = \_ row -> row { memImportance = lit imp }
        , updateWhere = \_ row -> row.memId ==. lit mid &&. activeMemory row
        , returning = Returning id
        }
    ts <- case rs of
      [] -> pure []
      _  -> Session.statement () $ run $ select $
        orderBy (Prelude.id >$< asc) $ do
          row <- each memoryTagSchema
          where_ $ row.mtMemoryId ==. lit mid
          pure row.mtTag
    pure (rs, ts)
  case rows of
    []    -> pure Nothing
    (r:_) -> pure . Just $ rowToMemory r tags

------------------------------------------------------------------------
-- Graph traversal
------------------------------------------------------------------------

-- | Get all memories reachable from a source memory within N hops,
-- along with the links between them.  Uses a single recursive CTE
-- instead of iterative BFS to eliminate N+1 query overhead.
getRelatedGraph :: Pool Hasql.Connection -> UUID -> Int -> IO MemoryGraph
getRelatedGraph pool sourceId maxDepth = do
  let clampedDepth = Prelude.max 0 (Prelude.min 5 maxDepth)
  -- Single recursive CTE: find all reachable links in one round-trip
  linkRows <- runSession pool $ Session.statement (sourceId, fromIntegral clampedDepth :: Int32) graphCTEStatement
  let allLinks = mapMaybe toLinkFromRow linkRows
      allIds = Set.fromList [uid | l <- allLinks, uid <- [l.sourceId, l.targetId]]
               `Set.union` Set.singleton sourceId
  mems <- fetchMemoriesByIds pool (Set.toList allIds)
  pure MemoryGraph { memories = mems, links = allLinks }

-- | Recursive CTE that finds all links reachable from a source memory
-- within a given depth.  Uses UNION ALL with two directed sub-selects
-- (one per index) to avoid sequential scans on the OR condition.
graphCTEStatement :: Statement.Statement (UUID, Int32) [(UUID, UUID, Text, Double, UTCTime)]
graphCTEStatement = Statement.Statement sql encoder decoder True
  where
    sql = BS8.pack $ unlines
      [ "WITH RECURSIVE reachable(id, depth, path) AS ("
      , "  SELECT m.id, 0, ARRAY[m.id]"
      , "  FROM memories m"
      , "  WHERE m.id = $1 AND m.deleted_at IS NULL"
      , "  UNION ALL"
      , "  SELECT sub.next_id, r.depth + 1, r.path || sub.next_id"
      , "  FROM reachable r"
      , "  CROSS JOIN LATERAL ("
      , "    SELECT ml.target_id AS next_id"
      , "    FROM memory_links ml"
      , "    JOIN memories tgt ON tgt.id = ml.target_id AND tgt.deleted_at IS NULL"
      , "    WHERE ml.source_id = r.id"
      , "    UNION ALL"
      , "    SELECT ml.source_id AS next_id"
      , "    FROM memory_links ml"
      , "    JOIN memories src ON src.id = ml.source_id AND src.deleted_at IS NULL"
      , "    WHERE ml.target_id = r.id"
      , "  ) sub"
      , "  WHERE r.depth < $2"
      , "    AND NOT (sub.next_id = ANY(r.path))"
      , "),"
      , "reachable_ids AS (SELECT DISTINCT id FROM reachable)"
      , "SELECT source_id, target_id, relation_type::text,"
      , "       strength::float8, created_at"
      , "FROM memory_links ml"
      , "JOIN memories src ON src.id = ml.source_id AND src.deleted_at IS NULL"
      , "JOIN memories tgt ON tgt.id = ml.target_id AND tgt.deleted_at IS NULL"
      , "WHERE source_id IN (SELECT id FROM reachable_ids)"
      , "UNION"
      , "SELECT source_id, target_id, relation_type::text,"
      , "       strength::float8, created_at"
      , "FROM memory_links ml"
      , "JOIN memories src ON src.id = ml.source_id AND src.deleted_at IS NULL"
      , "JOIN memories tgt ON tgt.id = ml.target_id AND tgt.deleted_at IS NULL"
      , "WHERE target_id IN (SELECT id FROM reachable_ids)"
      ]
    encoder =
      contramap fst (Enc.param (Enc.nonNullable Enc.uuid)) <>
      contramap snd (Enc.param (Enc.nonNullable Enc.int4))
    decoder = Dec.rowList $ (,,,,)
      <$> Dec.column (Dec.nonNullable Dec.uuid)
      <*> Dec.column (Dec.nonNullable Dec.uuid)
      <*> Dec.column (Dec.nonNullable Dec.text)
      <*> Dec.column (Dec.nonNullable Dec.float8)
      <*> Dec.column (Dec.nonNullable Dec.timestamptz)

-- | Convert a raw CTE result row to a typed MemoryLink.
toLinkFromRow :: (UUID, UUID, Text, Double, UTCTime) -> Maybe MemoryLink
toLinkFromRow (sid, tid, rtText, str, ca) =
  case relationTypeFromText rtText of
    Just rt -> Just MemoryLink
      { sourceId     = sid
      , targetId     = tid
      , relationType = rt
      , strength     = str
      , createdAt    = ca
      }
    Nothing -> Nothing

fetchMemoriesByIds :: Pool Hasql.Connection -> [UUID] -> IO [Memory]
fetchMemoriesByIds _ [] = pure []
fetchMemoriesByIds pool ids =
  runSession pool $ do
    rows <- Session.statement () $ run $ select $ do
      row <- each memorySchema
      where_ $ in_ row.memId (map lit ids)
      where_ $ activeMemory row
      pure row
    enrichRowsS rows

-- | Find all memory links of a given relation type within a workspace.
findByRelation :: Pool Hasql.Connection -> UUID -> RelationType -> IO [MemoryLink]
findByRelation pool wsId relType = do
  rows <- runSession pool $ Session.statement () $ run $ select $ do
    link <- each memoryLinkSchema
    present $ do
      src <- each memorySchema
      where_ $ src.memId ==. link.mlSourceId
      where_ $ activeMemory src
    present $ do
      tgt <- each memorySchema
      where_ $ tgt.memId ==. link.mlTargetId
      where_ $ activeMemory tgt
    where_ $ link.mlRelationType ==. lit relType
    -- Match links where at least one endpoint is in the workspace
    present $ do
      mem <- each memorySchema
      where_ $ mem.memWorkspaceId ==. lit wsId
      where_ $ activeMemory mem
      where_ $ mem.memId ==. link.mlSourceId ||. mem.memId ==. link.mlTargetId
    pure link
  pure $ map linkRowToMemoryLink rows

------------------------------------------------------------------------
-- Cross-entity memory lists
------------------------------------------------------------------------

-- | List all memories linked to a project.
getProjectMemories :: Pool Hasql.Connection -> UUID -> IO [Memory]
getProjectMemories pool projId =
  runSession pool $ do
    rows <- Session.statement () $ run $ select $ limit 200 $ do
      present $ do
        proj <- each projectSchema
        where_ $ proj.projId ==. lit projId
        where_ $ activeProject proj
      pml <- each projectMemoryLinkSchema
      where_ $ pml.pmlProjectId ==. lit projId
      mem <- each memorySchema
      where_ $ mem.memId ==. pml.pmlMemoryId
      where_ $ activeMemory mem
      pure mem
    enrichRowsS rows

-- | List all memories linked to a task.
getTaskMemories :: Pool Hasql.Connection -> UUID -> IO [Memory]
getTaskMemories pool taskId =
  runSession pool $ do
    rows <- Session.statement () $ run $ select $ limit 200 $ do
      present $ do
        task <- each taskSchema
        where_ $ task.taskId ==. lit taskId
        where_ $ activeTask task
      tml <- each taskMemoryLinkSchema
      where_ $ tml.tmlTaskId ==. lit taskId
      mem <- each memorySchema
      where_ $ mem.memId ==. tml.tmlMemoryId
      where_ $ activeMemory mem
      pure mem
    enrichRowsS rows

------------------------------------------------------------------------
-- Pinning
------------------------------------------------------------------------

-- | Toggle the pinned status of a memory.
togglePin :: Pool Hasql.Connection -> UUID -> Bool -> IO (Maybe Memory)
togglePin pool mid pinVal = do
  (rows, tags) <- runTransaction pool $ do
    rs <- Session.statement () $ run $
      update Update
        { target = memorySchema
        , from = pure ()
        , set = \_ row -> row { memPinned = lit pinVal }
        , updateWhere = \_ row -> row.memId ==. lit mid &&. activeMemory row
        , returning = Returning Prelude.id
        }
    ts <- case rs of
      [] -> pure []
      _  -> Session.statement () $ run $ select $
        orderBy (Prelude.id >$< asc) $ do
          row <- each memoryTagSchema
          where_ $ row.mtMemoryId ==. lit mid
          pure row.mtTag
    pure (rs, ts)
  case rows of
    []    -> pure Nothing
    (r:_) -> pure . Just $ rowToMemory r tags

------------------------------------------------------------------------
-- Activity timeline
------------------------------------------------------------------------

-- | Get recent activity events across one or all workspaces.
-- Uses a single UNION ALL query to fetch the top-N events in one
-- database round-trip, rather than 3 separate queries merged in Haskell.
getRecentActivity
  :: Pool Hasql.Connection
  -> Maybe UUID      -- ^ workspace_id (Nothing = global)
  -> Maybe Int       -- ^ limit (default 50)
  -> IO [ActivityEvent]
getRecentActivity pool mWsId mlimit = do
  let lim = fromMaybe 50 mlimit
  runSession pool $ Session.statement (mWsId, fromIntegral lim :: Int32) activityUnionStatement

-- | Raw SQL statement that performs UNION ALL across memories, projects,
-- and tasks, ordering by timestamp and applying LIMIT in the database.
activityUnionStatement :: Statement.Statement (Maybe UUID, Int32) [ActivityEvent]
activityUnionStatement = Statement.Statement sql encoder decoder True
  where
    sql = BS8.pack $ unlines
      [ "SELECT event_type, entity_type, entity_id, workspace_id, summary, ts FROM ("
      , "  SELECT"
      , "    CASE WHEN m.created_at = m.updated_at THEN 'created' ELSE 'updated' END AS event_type,"
      , "    'memory' AS entity_type,"
      , "    m.id AS entity_id,"
      , "    m.workspace_id,"
      , "    left(m.content, 100) AS summary,"
      , "    m.updated_at AS ts"
      , "  FROM memories m"
      , "  WHERE ($1::uuid IS NULL OR m.workspace_id = $1)"
      , "    AND m.deleted_at IS NULL"
      , "  UNION ALL"
      , "  SELECT"
      , "    CASE WHEN p.created_at = p.updated_at THEN 'created' ELSE 'updated' END,"
      , "    'project',"
      , "    p.id,"
      , "    p.workspace_id,"
      , "    p.name,"
      , "    p.updated_at"
      , "  FROM projects p"
      , "  WHERE ($1::uuid IS NULL OR p.workspace_id = $1)"
      , "    AND p.deleted_at IS NULL"
      , "  UNION ALL"
      , "  SELECT"
      , "    CASE WHEN t.created_at = t.updated_at THEN 'created' ELSE 'updated' END,"
      , "    'task',"
      , "    t.id,"
      , "    t.workspace_id,"
      , "    t.title,"
      , "    t.updated_at"
      , "  FROM tasks t"
      , "  WHERE ($1::uuid IS NULL OR t.workspace_id = $1)"
      , "    AND t.deleted_at IS NULL"
      , ") sub"
      , "ORDER BY ts DESC"
      , "LIMIT $2"
      ]
    encoder =
      contramap fst (Enc.param (Enc.nullable Enc.uuid)) <>
      contramap snd (Enc.param (Enc.nonNullable Enc.int4))
    decoder = Dec.rowList $ ActivityEvent
      <$> Dec.column (Dec.nonNullable Dec.text)
      <*> Dec.column (Dec.nonNullable Dec.text)
      <*> Dec.column (Dec.nonNullable Dec.uuid)
      <*> Dec.column (Dec.nonNullable Dec.uuid)
      <*> Dec.column (Dec.nonNullable Dec.text)
      <*> Dec.column (Dec.nonNullable Dec.timestamptz)

------------------------------------------------------------------------
-- Embedding / vector similarity
------------------------------------------------------------------------

-- | Store an embedding vector for a memory.  Requires the pgvector
-- extension and the @embedding@ column on the @memories@ table.
setEmbedding :: Pool Hasql.Connection -> UUID -> [Double] -> IO ()
setEmbedding pool mid vec =
  runSession pool $ Session.statement (mid, vecText vec) setEmbeddingStatement

setEmbeddingStatement :: Statement.Statement (UUID, Text) ()
setEmbeddingStatement = Statement.Statement sql encoder decoder True
  where
    sql = "UPDATE memories SET embedding = $2::vector WHERE id = $1 AND deleted_at IS NULL"
    encoder =
      contramap fst (Enc.param (Enc.nonNullable Enc.uuid)) <>
      contramap snd (Enc.param (Enc.nonNullable Enc.text))
    decoder = Dec.noResult

-- | Find memories whose embedding is most similar to the query vector,
-- ranked by cosine similarity.  Requires the pgvector extension.
similarMemories :: Pool Hasql.Connection -> SimilarQuery -> IO [SimilarMemory]
similarMemories pool sq = do
  let lim   = fromIntegral (fromMaybe 10 sq.limit) :: Int32
      minS  = fromMaybe 0.0 sq.minSimilarity
      vt    = vecText sq.embedding
  runSession pool $ do
    rows <- Session.statement (vt, sq.workspaceId, minS, lim) similarStatement
    -- Batch-fetch tags for the returned memories
    let mids = map (\(mid, _,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_sim) -> mid) rows
    tagMap <- batchGetTagsS mids
    pure [ SimilarMemory
          { memory = Memory
              { id             = mid
              , workspaceId    = wid
              , content        = cnt
              , summary        = summ
              , memoryType     = fromMaybe ShortTerm (memoryTypeFromText mt)
              , importance     = fromIntegral imp
              , metadata       = meta
              , expiresAt      = expAt
              , source         = src
              , confidence     = conf
              , pinned         = pin
              , lastAccessedAt = lastAcc
              , accessCount    = fromIntegral acc
              , ftsLanguage    = lang
              , tags           = Map.findWithDefault [] mid tagMap
              , createdAt      = cAt
              , updatedAt      = uAt
              }
          , similarity = sim
          }
       | (mid, wid, cnt, summ, mt, imp, meta, expAt, src, conf, pin, lastAcc, acc, lang, cAt, uAt, sim) <- rows
       ]

similarStatement
  :: Statement.Statement
       (Text, UUID, Double, Int32)
       [(UUID, UUID, Text, Maybe Text, Text, Int16, Value, Maybe UTCTime,
         Maybe Text, Double, Bool, UTCTime, Int32, Text, UTCTime, UTCTime, Double)]
similarStatement = Statement.Statement sql encoder decoder True
  where
    sql = BS8.pack $ unlines
      [ "SELECT m.id, m.workspace_id, m.content, m.summary,"
      , "       m.memory_type::text, m.importance, m.metadata,"
      , "       m.expires_at, m.source, m.confidence, m.pinned,"
      , "       m.last_accessed_at, m.access_count, m.fts_language,"
      , "       m.created_at, m.updated_at,"
      , "       1 - (m.embedding <=> $1::vector) AS similarity"
      , "FROM memories m"
      , "WHERE m.workspace_id = $2"
      , "  AND m.deleted_at IS NULL"
      , "  AND m.embedding IS NOT NULL"
      , "  AND 1 - (m.embedding <=> $1::vector) >= $3"
      , "ORDER BY m.embedding <=> $1::vector"
      , "LIMIT $4"
      ]
    encoder =
      contramap (\(v,_,_,_) -> v) (Enc.param (Enc.nonNullable Enc.text)) <>
      contramap (\(_,w,_,_) -> w) (Enc.param (Enc.nonNullable Enc.uuid)) <>
      contramap (\(_,_,s,_) -> s) (Enc.param (Enc.nonNullable Enc.float8)) <>
      contramap (\(_,_,_,l) -> l) (Enc.param (Enc.nonNullable Enc.int4))
    decoder = Dec.rowList $ (,,,,,,,,,,,,,,,,)
      <$> Dec.column (Dec.nonNullable Dec.uuid)         -- id
      <*> Dec.column (Dec.nonNullable Dec.uuid)         -- workspace_id
      <*> Dec.column (Dec.nonNullable Dec.text)         -- content
      <*> Dec.column (Dec.nullable Dec.text)            -- summary
      <*> Dec.column (Dec.nonNullable Dec.text)         -- memory_type::text
      <*> Dec.column (Dec.nonNullable Dec.int2)         -- importance
      <*> Dec.column (Dec.nonNullable Dec.jsonb)        -- metadata
      <*> Dec.column (Dec.nullable Dec.timestamptz)     -- expires_at
      <*> Dec.column (Dec.nullable Dec.text)            -- source
      <*> Dec.column (Dec.nonNullable Dec.float8)       -- confidence
      <*> Dec.column (Dec.nonNullable Dec.bool)         -- pinned
      <*> Dec.column (Dec.nonNullable Dec.timestamptz)  -- last_accessed_at
      <*> Dec.column (Dec.nonNullable Dec.int4)         -- access_count
      <*> Dec.column (Dec.nonNullable Dec.text)         -- fts_language
      <*> Dec.column (Dec.nonNullable Dec.timestamptz)  -- created_at
      <*> Dec.column (Dec.nonNullable Dec.timestamptz)  -- updated_at
      <*> Dec.column (Dec.nonNullable Dec.float8)       -- similarity

-- | Render a Haskell @[Double]@ as the pgvector text literal @[0.1,0.2,...]@.
vecText :: [Double] -> Text
vecText ds = "[" <> T.intercalate "," (map (T.pack . show) ds) <> "]"
