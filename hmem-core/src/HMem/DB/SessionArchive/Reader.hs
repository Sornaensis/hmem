-- | Compatible bounded reads from one immutable published archive. Every SQL
-- call returns its pool lease before decoding, hashing or invoking a consumer.
module HMem.DB.SessionArchive.Reader
  ( ArchiveReader, openArchiveReader, closeArchiveReader, readArchiveRange
  , verifiedManifest, verifiedPart, ensureArchive
  , verifiedLedger, ReaderCache, ArchiveAudience(..), ReaderCacheBusy(..)
  , newReaderCache, closeReaderCache, withCachedReader
  , ReaderMetrics(..), readerMetrics
  , ArchiveReadFailure(..)
  , archiveReadSession, runArchiveRead
  ) where

import Control.Concurrent.MVar
import Control.Exception
import Control.Monad (unless, when, forM_, (>=>))
import Data.Aeson (toJSON)
import Data.Aeson qualified
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Functor.Contravariant (contramap)
import Data.Int (Int64)
import Data.IORef
import Data.List (sortOn)
import Data.Pool (Pool)
import Data.Text (Text)
import Data.Time
import Data.UUID (UUID)
import Data.Text qualified as T
import Hasql.Connection qualified as H
import Hasql.Decoders qualified as D
import Hasql.Encoders qualified as E
import Hasql.Session qualified as S
import Hasql.Pipeline qualified as P
import Hasql.Statement qualified as St

import HMem.AgentSession (sessionSha256)
import HMem.DB.Pool (DBException,runSession, runTransaction)
import HMem.DB.SessionArchive qualified as A
import HMem.SessionArchive.Codec qualified as C

require :: Bool -> IO ()
require ok = unless ok (throwIO (C.CodecFailure "archive_unavailable"))
param :: (a -> b) -> E.Value b -> E.Params a
param f v = contramap f (E.param (E.nonNullable v))
params :: E.Params (UUID,UUID,UUID,Int64)
params = param (\(w,_,_,_)->w) E.uuid <> param (\(_,s,_,_)->s) E.uuid
  <> param (\(_,_,g,_)->g) E.uuid <> param (\(_,_,_,n)->n) E.int8
column :: D.Value a -> D.Row a
column = D.column . D.nonNullable

data ArchiveReadFailure = ArchiveReadFailure DBException deriving stock (Show)
instance Exception ArchiveReadFailure

-- A blocked libpq query can defer Haskell cancellation. Bound the archive
-- statement itself on PostgreSQL, preserving stricter caller settings. Both
-- settings are transaction-local and execute before the query in one pipeline
-- turn; no extra round trip or connection is retained by the reader.
readLimits :: St.Statement () (Text,Text)
readLimits = St.Statement
  "SELECT set_config('lock_timeout',(CASE WHEN l.setting::bigint=0 THEN 1000 ELSE least(l.setting::bigint,1000) END)::text||'ms',true),set_config('statement_timeout',(CASE WHEN s.setting::bigint=0 THEN 4000 ELSE least(s.setting::bigint,4000) END)::text||'ms',true) FROM pg_catalog.pg_settings l CROSS JOIN pg_catalog.pg_settings s WHERE l.name='lock_timeout' AND s.name='statement_timeout'"
  E.noParams (D.singleRow ((,) <$> column D.text <*> column D.text)) True

archiveQuery :: Pool H.Connection -> input -> St.Statement input output -> IO output
archiveQuery pool input statement =
  (runSession pool $ archiveReadSession input statement)
    `catch` (\failure -> throwIO (ArchiveReadFailure failure))

-- Used by the sealed evidence adapter for its existing metadata/raw queries.
runArchiveRead :: Pool H.Connection -> input -> St.Statement input output -> IO output
runArchiveRead = archiveQuery

-- Composes with an owned transaction; the caller retains rollback/commit
-- ownership. Also permits same-backend restoration checks after SQL errors.
archiveReadSession :: input -> St.Statement input output -> S.Session output
archiveReadSession input statement = S.pipeline $ P.statement () readLimits *> P.statement input statement

query :: Pool H.Connection -> (UUID,UUID,UUID,Int64) -> ByteString -> D.Row a -> IO (Maybe a)
query pool input sql row = archiveQuery pool input (St.Statement sql params (D.rowMaybe row) True)

-- The authorized transport may make a particular sealed session eligible
-- without an unbounded global scan. This operation never seals or changes it.
ensureArchive :: Pool H.Connection -> UUID -> UUID -> IO ()
ensureArchive pool workspace session = runTransaction pool $ S.statement (workspace,session) $ St.Statement
  "WITH requested AS (INSERT INTO session_archive_requests(session_id,workspace_id) SELECT s.id,s.workspace_id FROM sessions s JOIN workspaces w ON w.id=s.workspace_id WHERE s.workspace_id=$1 AND s.id=$2 AND s.state='terminated' AND w.deleted_at IS NULL ON CONFLICT DO NOTHING) INSERT INTO session_archive_jobs(session_id,workspace_id) SELECT s.id,s.workspace_id FROM sessions s JOIN workspaces w ON w.id=s.workspace_id WHERE s.workspace_id=$1 AND s.id=$2 AND s.state='terminated' AND w.deleted_at IS NULL ON CONFLICT DO NOTHING"
  (param fst E.uuid <> param snd E.uuid) D.noResult True

-- Bind manifest fields and every chain head before using a generation. Child
-- reads independently check position/payload bindings, including seek starts.
verifiedManifest :: Pool H.Connection -> UUID -> UUID -> IO (Maybe A.ArchiveManifest)
verifiedManifest pool workspace session = do
  found <- archiveQuery pool (workspace,session) $ St.Statement
    "SELECT to_jsonb(g) FROM session_archives g WHERE g.workspace_id=$1 AND g.session_id=$2"
    (param fst E.uuid <> param snd E.uuid)
    (D.rowMaybe (column (D.refine (\value -> case Data.Aeson.fromJSON value of
      Data.Aeson.Success result -> Right result
      Data.Aeson.Error message -> Left (T.pack message)) D.jsonb))) True
  case found of
    Nothing -> pure Nothing
    Just m -> do
      valid <- query pool (workspace,session,m.id,0)
        "SELECT g.metadata_sha256=hmem_archive_binding('',jsonb_build_array(g.id,g.session_id,g.workspace_id,g.codec,g.codec_implementation,g.codec_parameters,g.raw_bytes,g.raw_records,g.raw_revision,g.raw_sha256,g.compressed_bytes,g.compressed_sha256,g.part_count,g.checkpoint_count,g.ledger_count,(SELECT binding_sha256 FROM session_archive_parts WHERE generation_id=g.id ORDER BY ordinal DESC LIMIT 1),(SELECT binding_sha256 FROM session_archive_checkpoints WHERE generation_id=g.id ORDER BY ordinal DESC LIMIT 1),coalesce((SELECT binding_sha256 FROM session_archive_ledger WHERE generation_id=g.id ORDER BY revision DESC LIMIT 1),''))) FROM session_archives g WHERE g.workspace_id=$1 AND g.session_id=$2 AND g.id=$3 AND $4::bigint=0"
        (column D.bool)
      require (valid == Just True && m.codec == "gzip-v1" && m.rawBytes >= 0 && m.compressedBytes >= 20
        && m.codecParameters == toJSON (parameters :: Parameters))
      pure (Just m)

-- Parameters are compared structurally, avoiding dependence on JSON key order.
type Parameters = Data.Aeson.Value
parameters :: Parameters
parameters = Data.Aeson.object ["level" Data.Aeson..= (6::Int), "window_bits" Data.Aeson..= (31::Int), "memory_level" Data.Aeson..= (8::Int), "strategy" Data.Aeson..= ("default"::Text), "mtime" Data.Aeson..= (0::Int), "os" Data.Aeson..= (255::Int), "checkpoint_raw_bytes" Data.Aeson..= (1048576::Int)]

partRow :: D.Row A.ArchivePart
partRow = A.ArchivePart <$> column D.int8 <*> column D.int8 <*> column D.int8 <*> column D.bytea <*> column D.text <*> column D.text
partSQL :: ByteString
partSQL = "SELECT p.ordinal,p.start_offset,p.end_offset,p.data,p.data_sha256,p.binding_sha256 FROM session_archives g JOIN session_archive_parts p ON p.generation_id=g.id WHERE g.workspace_id=$1 AND g.session_id=$2 AND g.id=$3 AND p.ordinal=$4 AND p.data_sha256=encode(sha256(p.data),'hex') AND p.binding_sha256=hmem_archive_binding(coalesce((SELECT q.binding_sha256 FROM session_archive_parts q WHERE q.generation_id=p.generation_id AND q.ordinal=p.ordinal-1),''),jsonb_build_array(p.generation_id,p.ordinal,p.start_offset,p.end_offset,p.data_sha256))"
verifiedPart :: Pool H.Connection -> UUID -> UUID -> UUID -> Int64 -> IO (Maybe A.ArchivePart)
verifiedPart pool w s g n = do
  part <- query pool (w,s,g,n) partSQL partRow
  forM_ part $ \p -> require (p.ordinal == n && p.startOffset >= 0 && p.endOffset-p.startOffset == fromIntegral (BS.length p.bytes)
    && not (BS.null p.bytes) && BS.length p.bytes <= 1048576 && sessionSha256 p.bytes == p.dataSha256)
  pure part

ledgerRow :: D.Row A.ArchiveLedger
ledgerRow = A.ArchiveLedger <$> column D.int8 <*> column D.int8 <*> column D.int8 <*> column D.bool <*> column D.int8 <*> column D.bool <*> column D.text <*> column D.text
verifiedLedger :: Pool H.Connection -> UUID -> UUID -> UUID -> Int64 -> IO (Maybe A.ArchiveLedger)
verifiedLedger pool w s g offset = query pool (w,s,g,offset)
  "SELECT l.revision,l.start_offset,l.end_offset,l.allow_final_record,l.record_count,l.ends_with_newline,l.raw_sha256,l.binding_sha256 FROM session_archives g JOIN session_archive_ledger l ON l.generation_id=g.id WHERE g.workspace_id=$1 AND g.session_id=$2 AND g.id=$3 AND l.start_offset<=$4 AND l.end_offset>$4 AND l.binding_sha256=hmem_archive_binding(coalesce((SELECT p.binding_sha256 FROM session_archive_ledger p WHERE p.generation_id=l.generation_id AND p.revision=l.revision-1),''),jsonb_build_array(l.generation_id,l.revision,l.start_offset,l.end_offset,l.allow_final_record,l.record_count,l.ends_with_newline,l.raw_sha256)) ORDER BY l.start_offset DESC LIMIT 1"
  ledgerRow

data DecodeState = DecodeState !Int64 !C.Decoder
data Chunk = Chunk !Int64 !Text !ByteString
data ReaderState = ReaderState !(Maybe DecodeState) ![Chunk] !Bool
data ArchiveReader = ArchiveReader
  { archiveManifest :: !A.ArchiveManifest, pool :: !(Pool H.Connection), state :: !(MVar ReaderState)
  , metrics :: !(IORef ReaderMetrics) }

data ReaderMetrics = ReaderMetrics { restores :: !Int64, inflatedBytes :: !Int64, verifiedBytes :: !Int64 }
  deriving stock (Show,Eq)
readerMetrics :: ArchiveReader -> IO ReaderMetrics
readerMetrics reader = readIORef reader.metrics

addMetrics :: ArchiveReader -> Int64 -> Int64 -> Int64 -> IO ()
addMetrics reader restores inflated verified = do
  old <- readIORef reader.metrics
  a <- C.checkedAdd old.restores restores
  b <- C.checkedAdd old.inflatedBytes inflated
  c <- C.checkedAdd old.verifiedBytes verified
  writeIORef reader.metrics (ReaderMetrics a b c)

openArchiveReader :: Pool H.Connection -> A.ArchiveManifest -> IO ArchiveReader
openArchiveReader pool manifest = ArchiveReader manifest pool <$> newMVar (ReaderState Nothing [] False) <*> newIORef (ReaderMetrics 0 0 0)
closeArchiveReader :: ArchiveReader -> IO ()
closeArchiveReader reader = modifyMVar_ reader.state $ \(ReaderState decoder _ _) -> do
  forM_ decoder $ \(DecodeState _ d) -> C.closeDecoder d
  pure (ReaderState Nothing [] True)

pauseArchiveReader :: ArchiveReader -> IO ()
pauseArchiveReader reader = modifyMVar_ reader.state $ \(ReaderState decoder chunks closed) -> do
  forM_ decoder $ \(DecodeState _ d) -> C.closeDecoder d
  pure (ReaderState Nothing chunks closed)

-- Both compressed and raw seek lookups use the unique generation/offset index.
restart :: ArchiveReader -> Int64 -> IO DecodeState
restart reader offset = mask $ \restore -> do
  let m = reader.archiveManifest; w = m.workspaceId; s = m.sessionId; g = m.id
  point <- query reader.pool (w,s,g,offset)
    "WITH chosen AS MATERIALIZED (SELECT c.* FROM session_archives g JOIN session_archive_checkpoints c ON c.generation_id=g.id WHERE g.workspace_id=$1 AND g.session_id=$2 AND g.id=$3 AND c.raw_offset<=$4 ORDER BY c.raw_offset DESC LIMIT 1) SELECT c.raw_offset,c.compressed_offset,c.unused_bits,c.dictionary FROM chosen c WHERE c.dictionary_sha256=encode(sha256(c.dictionary),'hex') AND c.binding_sha256=hmem_archive_binding(coalesce((SELECT p.binding_sha256 FROM session_archive_checkpoints p WHERE p.generation_id=c.generation_id AND p.ordinal=c.ordinal-1),''),jsonb_build_array(c.generation_id,c.ordinal,c.raw_offset,c.compressed_offset,c.unused_bits,c.dictionary_sha256))"
    (C.Checkpoint <$> column D.int8 <*> column D.int8 <*> (fromIntegral <$> column D.int2) <*> column D.bytea)
  cp <- maybe (throwIO (C.CodecFailure "archive_checkpoint_unavailable")) pure point
  require (cp.rawOffset <= offset && offset-cp.rawOffset <= 1048576 && cp.compressedOffset < m.compressedBytes)
  compressedAt <- newIORef (cp.compressedOffset - if cp.unusedBits == 0 then 0 else 1)
  let source = do
        at <- readIORef compressedAt
        if at == m.compressedBytes then pure Nothing else do
          ordinal <- query reader.pool (w,s,g,at)
            "SELECT p.ordinal FROM session_archives g JOIN session_archive_parts p ON p.generation_id=g.id WHERE g.workspace_id=$1 AND g.session_id=$2 AND g.id=$3 AND p.start_offset<=$4 AND p.end_offset>$4 ORDER BY p.start_offset DESC LIMIT 1" (column D.int8)
          p <- maybe (throwIO (C.CodecFailure "archive_part_unavailable")) (verifiedPart reader.pool w s g >=> maybe (throwIO (C.CodecFailure "archive_part_corrupt")) pure) ordinal
          require (p.startOffset <= at && p.endOffset > at && p.endOffset <= m.compressedBytes)
          writeIORef compressedAt p.endOffset
          pure (Just (BS.drop (fromIntegral (at-p.startOffset)) p.bytes))
  decoder <- restore (C.newDecoder cp source)
  restore (do
    _ <- C.readDecoder decoder (fromIntegral (offset-cp.rawOffset))
    addMetrics reader 1 (offset-cp.rawOffset) 0
    pure (DecodeState offset decoder)) `onException` C.closeDecoder decoder

readArchiveRange :: ArchiveReader -> Int64 -> Int -> IO ByteString
readArchiveRange reader offset limit = mask $ \restore -> do
  require (offset >= 0 && offset <= reader.archiveManifest.rawBytes && limit >= 0 && limit <= 1048576)
  old <- takeMVar reader.state
  let ReaderState initial _ _ = old
  owned <- newIORef initial
  let cleanup = readIORef owned >>= mapM_ (\(DecodeState _ d) -> C.closeDecoder d)
      poisoned = ReaderState Nothing [] True
      run (ReaderState decoder chunks closed) = do
        require (not closed)
        let end = offset + min (fromIntegral limit) (reader.archiveManifest.rawBytes-offset)
            go at active cached pieces
              | at == end = pure (BS.concat (reverse pieces),ReaderState active cached False)
              | otherwise = case [chunk | chunk@(Chunk start _ bytes) <- cached, start <= at, at-start < fromIntegral (BS.length bytes)] of
                  hit@(Chunk start _ bytes):_ -> do
                    -- A cache entry contains a complete immutable original
                    -- chunk already checked against its bound ledger SHA.
                    let stop=min end (start+fromIntegral (BS.length bytes))
                        piece=BS.take (fromIntegral (stop-at)) (BS.drop (fromIntegral (at-start)) bytes)
                        retained=hit:[c | c@(Chunk other _ _) <- cached,other /= start]
                    go stop active retained (piece:pieces)
                  [] -> do
                    let m=reader.archiveManifest
                    row <- verifiedLedger reader.pool m.workspaceId m.sessionId m.id at >>= maybe (throwIO (C.CodecFailure "archive_ledger_unavailable")) pure
                    require (row.startOffset <= at && row.endOffset > at && row.endOffset-row.startOffset <= 1048576 && row.endOffset <= m.rawBytes)
                    chosen <- case active of
                      Just d@(DecodeState cursor _) | cursor == row.startOffset -> pure d
                      _ -> mask_ $ do
                        forM_ active $ \(DecodeState _ d) -> C.closeDecoder d
                        writeIORef owned Nothing
                        value <- restart reader row.startOffset
                        writeIORef owned (Just value)
                        pure value
                    let DecodeState _ d = chosen
                    bytes <- C.readDecoder d (fromIntegral (row.endOffset-row.startOffset)) `onException` C.closeDecoder d
                    require (fromIntegral (BS.length bytes) == row.endOffset-row.startOffset && sessionSha256 bytes == row.rawSha256) `onException` C.closeDecoder d
                    addMetrics reader 0 (row.endOffset-row.startOffset) (row.endOffset-row.startOffset)
                    let retained=take 2 (Chunk row.startOffset row.rawSha256 bytes:cached)
                    go at (Just (DecodeState row.endOffset d)) retained pieces
        go offset decoder chunks []
  result <- try @SomeException (restore (run old))
  case result of
    Right (bytes,next) -> putMVar reader.state next >> pure bytes
    Left failure -> cleanup `finally` putMVar reader.state poisoned >> throwIO failure

newtype ArchiveAudience = ArchiveAudience Text deriving stock (Eq,Ord,Show)
data ReaderCacheBusy = ReaderCacheBusy deriving stock (Show)
instance Exception ReaderCacheBusy
type Key = (ArchiveAudience,UUID,UUID,UUID)
data Entry = Entry !Key !UTCTime !Bool !ArchiveReader
data CacheState = CacheState !Bool ![Entry]
newtype ReaderCache = ReaderCache (MVar CacheState)
newReaderCache :: IO ReaderCache
newReaderCache = ReaderCache <$> newMVar (CacheState False [])
closeReaderCache :: ReaderCache -> IO ()
closeReaderCache (ReaderCache lock) = modifyMVar_ lock $ \(CacheState _ entries) -> do
  -- Application shutdown must first drain HTTP users. Refuse to release a
  -- live decoder beneath a request; retain the capability for a later retry.
  when (any (\(Entry _ _ busy _) -> busy) entries) (throwIO ReaderCacheBusy)
  mapM_ (\(Entry _ _ _ r) -> closeArchiveReader r) entries
  pure (CacheState True [])

-- Caller has revalidated authorization. Only idle entries expire/evict.
withCachedReader :: ReaderCache -> ArchiveAudience -> Pool H.Connection -> A.ArchiveManifest -> (ArchiveReader -> IO a) -> IO a
withCachedReader (ReaderCache lock) audience pool manifest action = mask $ \restore -> do
  now <- getCurrentTime
  let key=(audience,manifest.workspaceId,manifest.sessionId,manifest.id)
  reader <- modifyMVar lock $ \(CacheState closed entries) -> do
    when closed (throwIO ReaderCacheBusy)
    let expired (Entry _ used busy _) = not busy && diffUTCTime now used >= 30
        stale=filter expired entries; live=filter (not . expired) entries
    mapM_ (\(Entry _ _ _ r)->closeArchiveReader r) stale
    case [e | e@(Entry k _ _ _) <- live,k == key] of
      Entry _ _ True _:_->throwIO ReaderCacheBusy
      Entry _ _ False r:_->pure (CacheState False [if k==key then Entry k now True value else e | e@(Entry k _ _ value)<-live],r)
      [] -> do
        let idle=sortOn (\(Entry _ used _ _)->used) [e | e@(Entry _ _ busy _)<-live,not busy]
        (retained,evicted) <- if length live < 4 then pure (live,[]) else case idle of
          [] -> throwIO ReaderCacheBusy
          e@(Entry oldKey _ _ _):_ -> pure ([x | x@(Entry k _ _ _)<-live,k/=oldKey],[e])
        mapM_ (\(Entry _ _ _ r)->closeArchiveReader r) evicted
        r <- openArchiveReader pool manifest
        pure (CacheState False (Entry key now True r:retained),r)
  -- Ordinary pagination caches verified bytes only. The native seek state is
  -- request-owned and always released before returning its admission lease.
  result <- try @SomeException (restore (action reader) `finally` pauseArchiveReader reader)
  ended <- getCurrentTime
  modifyMVar_ lock $ \(CacheState closed entries) -> do
    case result of Left _ -> closeArchiveReader reader; Right _ -> pure ()
    pure (CacheState closed [Entry k (if k==key then ended else used) (if k==key then False else busy) r
      | Entry k used busy r <- entries, k/=key || either (const False) (const True) result])
  either throwIO pure result
