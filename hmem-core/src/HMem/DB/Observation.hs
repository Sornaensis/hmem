module HMem.DB.Observation
  ( createObservation
  , getObservation
  , updateObservation
  , deleteObservation
  , listObservations
  , listObservationsOverfetch
  , listObservationSubjectFacets
  , listObservationSubjectFacetsOverfetch
  , matchObservations
  , matchObservationsOverfetch
  , setObservationEmbedding
  , similarObservations
  ) where

import Control.Exception (throwIO)
import Data.Aeson qualified as Aeson
import Data.Aeson (eitherDecodeStrict')
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as LBS
import Data.Functor.Contravariant (contramap)
import Data.Int (Int32)
import Data.Maybe (fromMaybe)
import Data.Pool (Pool)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.UUID (UUID)
import Hasql.Connection qualified as Hasql
import Hasql.Decoders qualified as Dec
import Hasql.Encoders qualified as Enc
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement
import Rel8 hiding (update)
import Rel8 qualified

import HMem.DB.Pool (DBException(..), runSession)
import HMem.DB.Schema
import HMem.Types

validateOrThrow :: [Text] -> IO ()
validateOrThrow [] = pure ()
validateOrThrow errors = throwIO $ DBCheckViolation (T.intercalate "; " errors)

-- | The repository predicate and insert share one statement.  The CTE locks
-- the workspace row, preventing a type/deletion change from racing a create.
createObservation :: Pool Hasql.Connection -> CreateObservation -> IO Observation
createObservation pool create = do
  let normalizedCreate = CreateObservation
        { workspaceId = create.workspaceId
        , subjects = normalizeObservationSubjects create.subjects
        , gitSha = create.gitSha
        , content = create.content
        }
  validateOrThrow $ validateCreateObservationInput normalizedCreate
  rows <- runSession pool $ Session.statement
    ( normalizedCreate.workspaceId
    , subjectsJson normalizedCreate.subjects
    , normalizedCreate.gitSha
    , normalizedCreate.content
    ) createObservationStatement
  case rows of
    (row:_) -> pure row
    [] -> throwIO $ DBCheckViolation "observations require an active repository workspace"

createObservationStatement :: Statement.Statement (UUID, Text, Text, Text) [Observation]
createObservationStatement = Statement.Statement sql encoder (Dec.rowList observationDecoder) True
  where
    sql = BS8.pack $ unlines
      [ "WITH repository_workspace AS ("
      , "  SELECT id FROM workspaces"
      , "  WHERE id = $1 AND workspace_type = 'repository' AND deleted_at IS NULL"
      , "  FOR UPDATE"
      , "), inserted AS ("
      , "  INSERT INTO observations (workspace_id, git_sha, content, subject_set_open)"
      , "  SELECT id, $3, $4, TRUE FROM repository_workspace"
      , "  RETURNING id, workspace_id, git_sha, content, created_at, updated_at"
      , "), inserted_subjects AS ("
      , "  INSERT INTO observation_subjects (observation_id, ordinal, subject_kind, subject)"
      , "  SELECT inserted.id, entry.ordinality - 1, (entry.value ->> 'subject_kind')::observation_subject_kind, entry.value ->> 'subject'"
      , "  FROM inserted CROSS JOIN jsonb_array_elements($2::jsonb) WITH ORDINALITY AS entry(value, ordinality)"
      , "  RETURNING observation_id, ordinal, subject_kind, subject"
      , ")"
      , "SELECT o.id, o.workspace_id, o.git_sha, o.content, o.created_at, o.updated_at,"
      , "       jsonb_agg(jsonb_build_object('subject_kind', s.subject_kind::text, 'subject', s.subject) ORDER BY s.ordinal)::text"
      , "FROM inserted o JOIN inserted_subjects s ON s.observation_id = o.id"
      , "GROUP BY o.id, o.workspace_id, o.git_sha, o.content, o.created_at, o.updated_at"
      ]
    encoder =
         contramap (\(a,_,_,_) -> a) (Enc.param (Enc.nonNullable Enc.uuid))
      <> contramap (\(_,b,_,_) -> b) (Enc.param (Enc.nonNullable Enc.text))
      <> contramap (\(_,_,c,_) -> c) (Enc.param (Enc.nonNullable Enc.text))
      <> contramap (\(_,_,_,d) -> d) (Enc.param (Enc.nonNullable Enc.text))

getObservation :: Pool Hasql.Connection -> UUID -> UUID -> IO (Maybe Observation)
getObservation pool workspace observationId = do
  rows <- runSession pool $ Session.statement (workspace, observationId) getObservationStatement
  pure $ case rows of
    (row:_) -> Just row
    [] -> Nothing

updateObservation :: Pool Hasql.Connection -> UUID -> UUID -> UpdateObservation -> IO (Maybe Observation)
updateObservation pool workspace observationId update = do
  validateOrThrow $ validateUpdateObservationInput update
  rows <- runSession pool $ Session.statement () $ run $
    Rel8.update Update
      { target = observationSchema
      , from = pure ()
      , set = \_ row -> row { obsContent = lit update.content }
      , updateWhere = \_ row -> row.obsId ==. lit observationId &&. row.obsWorkspaceId ==. lit workspace
      , returning = Returning id
      }
  -- Rel8 can return the parent row, then retrieve its subjects in one SQL
  -- statement.  This preserves the content-only update while avoiding a
  -- mutable subject path.
  case rows of
    (row:_) -> getObservation pool workspace observationId
    [] -> pure Nothing

-- | Observations are permanently deleted; there is no soft-delete state.
deleteObservation :: Pool Hasql.Connection -> UUID -> UUID -> IO Bool
deleteObservation pool workspace observationId = do
  deletedCount <- runSession pool $ Session.statement () $ runN $
    delete Delete
      { from = observationSchema
      , using = pure ()
      , deleteWhere = \_ row -> row.obsId ==. lit observationId &&. row.obsWorkspaceId ==. lit workspace
      , returning = NoReturning
      }
  pure (deletedCount > 0)

-- | Lists observations with composable exact provenance filters and optional
-- FTS. FTS is ranked first, with recency and ID as deterministic tie-breakers.
listObservations :: Pool Hasql.Connection -> ObservationQuery -> IO [Observation]
listObservations pool queryValue = do
  validateOrThrow $ validateObservationQuery queryValue
  listObservationsUnchecked pool queryValue

-- | The HTTP layer needs one extra row solely to calculate @has_more@.  Keep
-- that implementation detail out of public validation so limit=200 remains a
-- valid client request while limit=201 remains invalid client input.
listObservationsOverfetch :: Pool Hasql.Connection -> ObservationQuery -> IO [Observation]
listObservationsOverfetch pool queryValue = do
  validateOrThrow $ validateObservationQuery queryValue
  let queryWithExtra :: ObservationQuery
      queryWithExtra = queryValue { limit = Just (fromMaybe 50 queryValue.limit + 1) }
  listObservationsUnchecked pool queryWithExtra

listObservationsUnchecked :: Pool Hasql.Connection -> ObservationQuery -> IO [Observation]
listObservationsUnchecked pool queryValue = do
  let limitValue = fromIntegral (fromMaybe 50 queryValue.limit) :: Int32
      offsetValue = fromIntegral (fromMaybe 0 queryValue.offset) :: Int32
  runSession pool $ Session.statement
    ( queryValue.workspaceId
    , subjectKindToText <$> queryValue.subjectKind
    , queryValue.subject
    , queryValue.gitSha
    , queryValue.query
    , limitValue
    , offsetValue
    ) listObservationsStatement

-- | Aggregate exact stored subjects over the full filtered Observation set.
-- Pagination is applied only after grouping and deterministic ordering.
listObservationSubjectFacets :: Pool Hasql.Connection -> ObservationSubjectFacetQuery -> IO [ObservationSubjectFacet]
listObservationSubjectFacets pool queryValue = do
  validateOrThrow $ validateObservationSubjectFacetQuery queryValue
  listObservationSubjectFacetsUnchecked pool queryValue

listObservationSubjectFacetsOverfetch :: Pool Hasql.Connection -> ObservationSubjectFacetQuery -> IO [ObservationSubjectFacet]
listObservationSubjectFacetsOverfetch pool queryValue = do
  validateOrThrow $ validateObservationSubjectFacetQuery queryValue
  listObservationSubjectFacetsUnchecked pool queryValue
    { limit = Just (fromMaybe 50 queryValue.limit + 1) }

listObservationSubjectFacetsUnchecked :: Pool Hasql.Connection -> ObservationSubjectFacetQuery -> IO [ObservationSubjectFacet]
listObservationSubjectFacetsUnchecked pool queryValue = do
  let limitValue = fromIntegral (fromMaybe 50 queryValue.limit) :: Int32
      offsetValue = fromIntegral (fromMaybe 0 queryValue.offset) :: Int32
  runSession pool $ Session.statement
    ( queryValue.workspaceId
    , subjectKindToText <$> queryValue.subjectKind
    , queryValue.gitSha
    , queryValue.query
    , limitValue
    , offsetValue
    ) listObservationSubjectFacetsStatement

-- | Match concrete repository-relative paths against stored file and glob
-- subjects.  The SQL predicate is the scalable counterpart to
-- 'observationSubjectMatchesPath'; it returns evidence in stable caller and
-- stored-subject order without duplicating observations.
matchObservations :: Pool Hasql.Connection -> ObservationMatchQuery -> IO [ObservationMatch]
matchObservations pool queryValue = do
  validateOrThrow $ validateObservationMatchQuery queryValue
  matchObservationsUnchecked pool queryValue

matchObservationsUnchecked :: Pool Hasql.Connection -> ObservationMatchQuery -> IO [ObservationMatch]
matchObservationsUnchecked pool queryValue = do
  let limitValue = fromIntegral (fromMaybe 50 queryValue.limit) :: Int32
      offsetValue = fromIntegral (fromMaybe 0 queryValue.offset) :: Int32
  runSession pool $ Session.statement
    ( queryValue.workspaceId, subjectsJson (map (ObservationSubject SubjectFile) (normalizePaths queryValue.paths))
    , subjectKindToText <$> queryValue.subjectKind, queryValue.gitSha, queryValue.query, limitValue, offsetValue
    ) matchObservationsStatement

matchObservationsOverfetch :: Pool Hasql.Connection -> ObservationMatchQuery -> IO [ObservationMatch]
matchObservationsOverfetch pool queryValue = do
  validateOrThrow $ validateObservationMatchQuery queryValue
  matchObservationsUnchecked pool queryValue { limit = Just (fromMaybe 50 queryValue.limit + 1) }

listObservationsStatement :: Statement.Statement
  (UUID, Maybe Text, Maybe Text, Maybe Text, Maybe Text, Int32, Int32) [Observation]
listObservationsStatement = Statement.Statement sql encoder (Dec.rowList observationDecoder) True
  where
    sql = BS8.pack $ unlines
      [ "SELECT o.id, o.workspace_id, o.git_sha, o.content, o.created_at, o.updated_at,"
      , "       jsonb_agg(jsonb_build_object('subject_kind', s.subject_kind::text, 'subject', s.subject) ORDER BY s.ordinal)::text"
      , "FROM observations o JOIN observation_subjects s ON s.observation_id = o.id"
      , "WHERE o.workspace_id = $1"
      , "  AND (($2::text IS NULL AND $3::text IS NULL) OR EXISTS (SELECT 1 FROM observation_subjects f WHERE f.observation_id = o.id AND ($2::text IS NULL OR f.subject_kind::text = $2) AND ($3::text IS NULL OR f.subject = $3)))"
      , "  AND ($4::text IS NULL OR o.git_sha = $4)"
      , "  AND ($5::text IS NULL OR o.search_vector @@ plainto_tsquery('simple', $5))"
      , "GROUP BY o.id, o.workspace_id, o.git_sha, o.content, o.created_at, o.updated_at, o.search_vector"
      , "ORDER BY"
      , "  CASE WHEN $5::text IS NULL THEN 0 ELSE ts_rank(search_vector, plainto_tsquery('simple', $5)) END DESC,"
      , "  o.updated_at DESC, o.id DESC"
      , "LIMIT $6 OFFSET $7"
      ]
    encoder =
         contramap (\(a,_,_,_,_,_,_) -> a) (Enc.param (Enc.nonNullable Enc.uuid))
      <> contramap (\(_,b,_,_,_,_,_) -> b) (Enc.param (Enc.nullable Enc.text))
      <> contramap (\(_,_,c,_,_,_,_) -> c) (Enc.param (Enc.nullable Enc.text))
      <> contramap (\(_,_,_,d,_,_,_) -> d) (Enc.param (Enc.nullable Enc.text))
      <> contramap (\(_,_,_,_,e,_,_) -> e) (Enc.param (Enc.nullable Enc.text))
      <> contramap (\(_,_,_,_,_,f,_) -> f) (Enc.param (Enc.nonNullable Enc.int4))
      <> contramap (\(_,_,_,_,_,_,g) -> g) (Enc.param (Enc.nonNullable Enc.int4))

listObservationSubjectFacetsStatement :: Statement.Statement
  (UUID, Maybe Text, Maybe Text, Maybe Text, Int32, Int32) [ObservationSubjectFacet]
listObservationSubjectFacetsStatement = Statement.Statement sql encoder (Dec.rowList observationSubjectFacetDecoder) True
  where
    sql = BS8.pack $ unlines
      [ "SELECT s.subject_kind::text, s.subject, COUNT(DISTINCT o.id)::bigint, MAX(o.updated_at)"
      , "FROM observations o JOIN observation_subjects s ON s.observation_id = o.id"
      , "WHERE o.workspace_id = $1"
      , "  AND ($2::text IS NULL OR s.subject_kind::text = $2)"
      , "  AND ($3::text IS NULL OR o.git_sha = $3)"
      , "  AND ($4::text IS NULL OR o.search_vector @@ plainto_tsquery('simple', $4))"
      , "GROUP BY s.subject_kind, s.subject"
      , "ORDER BY COUNT(DISTINCT o.id) DESC, MAX(o.updated_at) DESC, s.subject_kind::text ASC, s.subject ASC"
      , "LIMIT $5 OFFSET $6"
      ]
    encoder =
         contramap (\(a,_,_,_,_,_) -> a) (Enc.param (Enc.nonNullable Enc.uuid))
      <> contramap (\(_,b,_,_,_,_) -> b) (Enc.param (Enc.nullable Enc.text))
      <> contramap (\(_,_,c,_,_,_) -> c) (Enc.param (Enc.nullable Enc.text))
      <> contramap (\(_,_,_,d,_,_) -> d) (Enc.param (Enc.nullable Enc.text))
      <> contramap (\(_,_,_,_,e,_) -> e) (Enc.param (Enc.nonNullable Enc.int4))
      <> contramap (\(_,_,_,_,_,f) -> f) (Enc.param (Enc.nonNullable Enc.int4))

matchObservationsStatement :: Statement.Statement
  (UUID, Text, Maybe Text, Maybe Text, Maybe Text, Int32, Int32) [ObservationMatch]
matchObservationsStatement = Statement.Statement sql encoder (Dec.rowList matchObservationDecoder) True
  where
    sql = BS8.pack $ unlines
      [ "WITH requested_paths AS ("
      , "  SELECT path.value ->> 'subject' AS path, path.ordinality AS path_ordinal"
      , "  FROM jsonb_array_elements($2::jsonb) WITH ORDINALITY AS path(value, ordinality)"
      , "), scoped_candidates AS ("
      , "  SELECT o.id"
      , "  FROM observations o"
      , "  WHERE o.workspace_id = $1"
      , "    AND ($4::text IS NULL OR o.git_sha = $4)"
      , "    AND ($5::text IS NULL OR o.search_vector @@ plainto_tsquery('simple', $5))"
      , "), matching_subjects AS ("
      , "  SELECT s.observation_id, s.ordinal, s.subject_kind, s.subject, rp.path, rp.path_ordinal"
      , "  FROM scoped_candidates c JOIN observation_subjects s ON s.observation_id = c.id"
      , "  JOIN requested_paths rp ON s.subject_kind = 'file' AND s.subject = rp.path"
      , "  WHERE $3::text IS NULL OR s.subject_kind::text = $3"
      , "  UNION ALL"
      , "  SELECT s.observation_id, s.ordinal, s.subject_kind, s.subject, rp.path, rp.path_ordinal"
      , "  FROM scoped_candidates c JOIN observation_subjects s ON s.observation_id = c.id"
      , "  JOIN requested_paths rp ON s.subject_kind = 'glob' AND hmem_observation_subject_matches(s.subject_kind, s.subject, rp.path)"
      , "  WHERE $3::text IS NULL OR s.subject_kind::text = $3"
      , "), candidate_ids AS ("
      , "  SELECT DISTINCT observation_id AS id FROM matching_subjects"
      , "), candidates AS ("
      , "  SELECT o.id, o.workspace_id, o.git_sha, o.content, o.created_at, o.updated_at, o.search_vector,"
      , "         jsonb_agg(jsonb_build_object('subject_kind', s.subject_kind::text, 'subject', s.subject) ORDER BY s.ordinal)::text AS subjects_json"
      , "  FROM candidate_ids ids JOIN observations o ON o.id = ids.id"
      , "  JOIN observation_subjects s ON s.observation_id = o.id"
      , "  GROUP BY o.id, o.workspace_id, o.git_sha, o.content, o.created_at, o.updated_at, o.search_vector"
      , "), matched_pairs AS ("
      , "  SELECT DISTINCT c.*, ms.path, ms.path_ordinal, ms.ordinal, ms.subject_kind, ms.subject"
      , "  FROM candidates c JOIN matching_subjects ms ON ms.observation_id = c.id"
      , "), matched AS ("
      , "  SELECT DISTINCT p.id, p.workspace_id, p.git_sha, p.content, p.created_at, p.updated_at, p.search_vector, p.subjects_json,"
      , "    (SELECT jsonb_agg(path ORDER BY path_ordinal) FROM (SELECT DISTINCT path, path_ordinal FROM matched_pairs q WHERE q.id = p.id) paths) AS paths_json,"
      , "    (SELECT jsonb_agg(jsonb_build_object('subject_kind', subject_kind::text, 'subject', subject) ORDER BY ordinal) FROM (SELECT DISTINCT ordinal, subject_kind, subject FROM matched_pairs q WHERE q.id = p.id) subjects) AS matched_subjects_json,"
      , "    (SELECT jsonb_agg(jsonb_build_object('path', path_group.path, 'matched_subjects', path_group.matched_subjects) ORDER BY path_group.path_ordinal)"
      , "       FROM (SELECT path_subjects.path, path_subjects.path_ordinal,"
      , "                    jsonb_agg(jsonb_build_object('subject_kind', path_subjects.subject_kind::text, 'subject', path_subjects.subject) ORDER BY path_subjects.ordinal) AS matched_subjects"
      , "             FROM (SELECT DISTINCT path, path_ordinal, ordinal, subject_kind, subject FROM matched_pairs q WHERE q.id = p.id) path_subjects"
      , "             GROUP BY path_subjects.path, path_subjects.path_ordinal) path_group) AS path_matches_json"
      , "  FROM matched_pairs p"
      , ")"
      , "SELECT id, workspace_id, git_sha, content, created_at, updated_at, subjects_json, paths_json::text, matched_subjects_json::text, path_matches_json::text"
      , "FROM matched"
      , "ORDER BY CASE WHEN $5::text IS NULL THEN 0 ELSE ts_rank(search_vector, plainto_tsquery('simple', $5)) END DESC, updated_at DESC, id DESC"
      , "LIMIT $6 OFFSET $7"
      ]
    encoder =
         contramap (\(a,_,_,_,_,_,_) -> a) (Enc.param (Enc.nonNullable Enc.uuid))
      <> contramap (\(_,b,_,_,_,_,_) -> b) (Enc.param (Enc.nonNullable Enc.text))
      <> contramap (\(_,_,c,_,_,_,_) -> c) (Enc.param (Enc.nullable Enc.text))
      <> contramap (\(_,_,_,d,_,_,_) -> d) (Enc.param (Enc.nullable Enc.text))
      <> contramap (\(_,_,_,_,e,_,_) -> e) (Enc.param (Enc.nullable Enc.text))
      <> contramap (\(_,_,_,_,_,f,_) -> f) (Enc.param (Enc.nonNullable Enc.int4))
      <> contramap (\(_,_,_,_,_,_,g) -> g) (Enc.param (Enc.nonNullable Enc.int4))

similarObservations :: Pool Hasql.Connection -> SimilarObservationQuery -> IO [SimilarObservation]
similarObservations pool queryValue = do
  validateOrThrow $ validateSimilarObservationQuery queryValue
  requirePgvector pool
  let limitValue = fromIntegral (fromMaybe 50 queryValue.limit) :: Int32
      offsetValue = fromIntegral (fromMaybe 0 queryValue.offset) :: Int32
      minSimilarityValue = fromMaybe 0 queryValue.minSimilarity
  runSession pool $ Session.statement
    ( vecText queryValue.embedding
    , queryValue.workspaceId
    , subjectKindToText <$> queryValue.subjectKind
    , queryValue.subject
    , queryValue.gitSha
    , minSimilarityValue
    , limitValue
    , offsetValue
    ) similarObservationsStatement

setObservationEmbedding :: Pool Hasql.Connection -> UUID -> UUID -> [Double] -> IO ()
setObservationEmbedding pool workspace observationId embeddingValue = do
  validateOrThrow ["embedding must contain exactly 1536 finite dimensions"
    | length embeddingValue /= observationEmbeddingDimensions
      || any (\x -> isNaN x || isInfinite x) embeddingValue]
  requirePgvector pool
  runSession pool $ Session.statement (workspace, observationId, vecText embeddingValue) setObservationEmbeddingStatement

requirePgvector :: Pool Hasql.Connection -> IO ()
requirePgvector pool = do
  available <- runSession pool $ Session.statement () observationVectorCapabilityStatement
  if available then pure () else throwIO $ DBCapabilityUnavailable "pgvector embedding support is unavailable for observations"

-- The extension and column are both conditional migration artifacts.  Check
-- both before preparing any vector-cast statement, including when pgvector was
-- installed after V020 and therefore never added the column.
observationVectorCapabilityStatement :: Statement.Statement () Bool
observationVectorCapabilityStatement = Statement.Statement
  "SELECT EXISTS (SELECT 1 FROM pg_extension WHERE extname = 'vector') \
  \AND EXISTS (SELECT 1 FROM information_schema.columns \
  \            WHERE table_schema = 'public' AND table_name = 'observations' AND column_name = 'embedding')"
  Enc.noParams
  (Dec.singleRow (Dec.column (Dec.nonNullable Dec.bool)))
  True

setObservationEmbeddingStatement :: Statement.Statement (UUID, UUID, Text) ()
setObservationEmbeddingStatement = Statement.Statement
  "UPDATE observations SET embedding = $3::vector WHERE workspace_id = $1 AND id = $2"
  ( contramap (\(a,_,_) -> a) (Enc.param (Enc.nonNullable Enc.uuid))
 <> contramap (\(_,b,_) -> b) (Enc.param (Enc.nonNullable Enc.uuid))
 <> contramap (\(_,_,c) -> c) (Enc.param (Enc.nonNullable Enc.text)))
  Dec.noResult
  True

similarObservationsStatement :: Statement.Statement
  (Text, UUID, Maybe Text, Maybe Text, Maybe Text, Double, Int32, Int32) [SimilarObservation]
similarObservationsStatement = Statement.Statement sql encoder (Dec.rowList similarObservationDecoder) True
  where
    sql = BS8.pack $ unlines
      [ "SELECT o.id, o.workspace_id, o.git_sha, o.content, o.created_at, o.updated_at,"
      , "       jsonb_agg(jsonb_build_object('subject_kind', s.subject_kind::text, 'subject', s.subject) ORDER BY s.ordinal)::text,"
      , "       1 - (o.embedding <=> $1::vector) AS similarity"
      , "FROM observations o JOIN observation_subjects s ON s.observation_id = o.id"
      , "WHERE o.embedding IS NOT NULL"
      , "  AND o.workspace_id = $2"
      , "  AND (($3::text IS NULL AND $4::text IS NULL) OR EXISTS (SELECT 1 FROM observation_subjects f WHERE f.observation_id = o.id AND ($3::text IS NULL OR f.subject_kind::text = $3) AND ($4::text IS NULL OR f.subject = $4)))"
      , "  AND ($5::text IS NULL OR o.git_sha = $5)"
      , "  AND 1 - (o.embedding <=> $1::vector) >= $6"
      , "GROUP BY o.id, o.workspace_id, o.git_sha, o.content, o.created_at, o.updated_at, o.embedding"
      , "ORDER BY o.embedding <=> $1::vector ASC, o.updated_at DESC, o.id DESC"
      , "LIMIT $7 OFFSET $8"
      ]
    encoder =
         contramap (\(a,_,_,_,_,_,_,_) -> a) (Enc.param (Enc.nonNullable Enc.text))
      <> contramap (\(_,b,_,_,_,_,_,_) -> b) (Enc.param (Enc.nonNullable Enc.uuid))
      <> contramap (\(_,_,c,_,_,_,_,_) -> c) (Enc.param (Enc.nullable Enc.text))
      <> contramap (\(_,_,_,d,_,_,_,_) -> d) (Enc.param (Enc.nullable Enc.text))
      <> contramap (\(_,_,_,_,e,_,_,_) -> e) (Enc.param (Enc.nullable Enc.text))
      <> contramap (\(_,_,_,_,_,f,_,_) -> f) (Enc.param (Enc.nonNullable Enc.float8))
      <> contramap (\(_,_,_,_,_,_,g,_) -> g) (Enc.param (Enc.nonNullable Enc.int4))
      <> contramap (\(_,_,_,_,_,_,_,h) -> h) (Enc.param (Enc.nonNullable Enc.int4))

observationDecoder :: Dec.Row Observation
observationDecoder = do
  observationId <- Dec.column (Dec.nonNullable Dec.uuid)
  workspace <- Dec.column (Dec.nonNullable Dec.uuid)
  sha <- Dec.column (Dec.nonNullable Dec.text)
  observationContent <- Dec.column (Dec.nonNullable Dec.text)
  created <- Dec.column (Dec.nonNullable Dec.timestamptz)
  updated <- Dec.column (Dec.nonNullable Dec.timestamptz)
  subjectsJson <- Dec.column (Dec.nonNullable Dec.text)
  subjectsValue <- either (fail . show) pure (eitherDecodeStrict' (TE.encodeUtf8 subjectsJson))
  pure Observation
    { id = observationId, workspaceId = workspace, subjects = subjectsValue
    , gitSha = sha, content = observationContent
    , createdAt = created, updatedAt = updated
    }

similarObservationDecoder :: Dec.Row SimilarObservation
similarObservationDecoder = SimilarObservation <$> observationDecoder <*> Dec.column (Dec.nonNullable Dec.float8)

observationSubjectFacetDecoder :: Dec.Row ObservationSubjectFacet
observationSubjectFacetDecoder = do
  kindText <- Dec.column (Dec.nonNullable Dec.text)
  kind <- maybe (fail $ "Invalid observation subject kind: " <> T.unpack kindText) pure (subjectKindFromText kindText)
  ObservationSubjectFacet kind
    <$> Dec.column (Dec.nonNullable Dec.text)
    <*> Dec.column (Dec.nonNullable Dec.int8)
    <*> Dec.column (Dec.nonNullable Dec.timestamptz)

matchObservationDecoder :: Dec.Row ObservationMatch
matchObservationDecoder = do
  matchedObservation <- observationDecoder
  pathsJson <- Dec.column (Dec.nonNullable Dec.text)
  subjectsValue <- Dec.column (Dec.nonNullable Dec.text)
  pathMatchesJson <- Dec.column (Dec.nonNullable Dec.text)
  pathsValue <- either (fail . show) pure (eitherDecodeStrict' (TE.encodeUtf8 pathsJson))
  matchedSubjectsValue <- either (fail . show) pure (eitherDecodeStrict' (TE.encodeUtf8 subjectsValue))
  pathMatchesValue <- either (fail . show) pure (eitherDecodeStrict' (TE.encodeUtf8 pathMatchesJson))
  pure ObservationMatch
    { observation = matchedObservation
    , pathMatches = pathMatchesValue
    , matchedPaths = pathsValue
    , matchedSubjects = matchedSubjectsValue
    }

vecText :: [Double] -> Text
vecText vectorValues = "[" <> T.intercalate "," (map (T.pack . show) vectorValues) <> "]"

subjectsJson :: [ObservationSubject] -> Text
subjectsJson = TE.decodeUtf8 . LBS.toStrict . Aeson.encode

normalizePaths :: [Text] -> [Text]
normalizePaths = map (.subject) . normalizeObservationSubjects . map (ObservationSubject SubjectFile)

getObservationStatement :: Statement.Statement (UUID, UUID) [Observation]
getObservationStatement = Statement.Statement sql encoder (Dec.rowList observationDecoder) True
  where
    sql = BS8.pack $ unlines
      [ "SELECT o.id, o.workspace_id, o.git_sha, o.content, o.created_at, o.updated_at,"
      , "       jsonb_agg(jsonb_build_object('subject_kind', s.subject_kind::text, 'subject', s.subject) ORDER BY s.ordinal)::text"
      , "FROM observations o JOIN observation_subjects s ON s.observation_id = o.id"
      , "WHERE o.workspace_id = $1 AND o.id = $2"
      , "GROUP BY o.id, o.workspace_id, o.git_sha, o.content, o.created_at, o.updated_at"
      ]
    encoder = contramap fst (Enc.param (Enc.nonNullable Enc.uuid))
           <> contramap snd (Enc.param (Enc.nonNullable Enc.uuid))
