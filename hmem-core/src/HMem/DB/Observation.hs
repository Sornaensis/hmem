module HMem.DB.Observation
  ( createObservation
  , getObservation
  , updateObservation
  , deleteObservation
  , listObservations
  , listObservationsOverfetch
  , setObservationEmbedding
  , similarObservations
  ) where

import Control.Exception (throwIO)
import Data.ByteString.Char8 qualified as BS8
import Data.Functor.Contravariant (contramap)
import Data.Int (Int32)
import Data.Maybe (fromMaybe)
import Data.Pool (Pool)
import Data.Text (Text)
import Data.Text qualified as T
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

rowToObservation :: ObservationT Result -> Observation
rowToObservation row = Observation
  { id = row.obsId
  , workspaceId = row.obsWorkspaceId
  , subjectKind = row.obsSubjectKind
  , subject = row.obsSubject
  , gitSha = row.obsGitSha
  , content = row.obsContent
  , createdAt = row.obsCreatedAt
  , updatedAt = row.obsUpdatedAt
  }

validateOrThrow :: [Text] -> IO ()
validateOrThrow [] = pure ()
validateOrThrow errors = throwIO $ DBCheckViolation (T.intercalate "; " errors)

-- | The repository predicate and insert share one statement.  The CTE locks
-- the workspace row, preventing a type/deletion change from racing a create.
createObservation :: Pool Hasql.Connection -> CreateObservation -> IO Observation
createObservation pool create = do
  validateOrThrow $ validateCreateObservationInput create
  rows <- runSession pool $ Session.statement
    ( create.workspaceId
    , subjectKindToText create.subjectKind
    , create.subject
    , create.gitSha
    , create.content
    ) createObservationStatement
  case rows of
    (row:_) -> pure row
    [] -> throwIO $ DBCheckViolation "observations require an active repository workspace"

createObservationStatement :: Statement.Statement (UUID, Text, Text, Text, Text) [Observation]
createObservationStatement = Statement.Statement sql encoder (Dec.rowList observationDecoder) True
  where
    sql = BS8.pack $ unlines
      [ "WITH repository_workspace AS ("
      , "  SELECT id FROM workspaces"
      , "  WHERE id = $1 AND workspace_type = 'repository' AND deleted_at IS NULL"
      , "  FOR UPDATE"
      , "), inserted AS ("
      , "  INSERT INTO observations (workspace_id, subject_kind, subject, git_sha, content)"
      , "  SELECT id, $2::observation_subject_kind, $3, $4, $5 FROM repository_workspace"
      , "  RETURNING id, workspace_id, subject_kind::text, subject, git_sha, content, created_at, updated_at"
      , ") SELECT * FROM inserted"
      ]
    encoder =
         contramap (\(a,_,_,_,_) -> a) (Enc.param (Enc.nonNullable Enc.uuid))
      <> contramap (\(_,b,_,_,_) -> b) (Enc.param (Enc.nonNullable Enc.text))
      <> contramap (\(_,_,c,_,_) -> c) (Enc.param (Enc.nonNullable Enc.text))
      <> contramap (\(_,_,_,d,_) -> d) (Enc.param (Enc.nonNullable Enc.text))
      <> contramap (\(_,_,_,_,e) -> e) (Enc.param (Enc.nonNullable Enc.text))

getObservation :: Pool Hasql.Connection -> UUID -> UUID -> IO (Maybe Observation)
getObservation pool workspace observationId = do
  rows <- runSession pool $ Session.statement () $ run $ select $ do
    row <- each observationSchema
    where_ $ row.obsId ==. lit observationId
    where_ $ row.obsWorkspaceId ==. lit workspace
    pure row
  pure $ rowToObservation <$> case rows of
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
  pure $ rowToObservation <$> case rows of
    (row:_) -> Just row
    [] -> Nothing

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
      queryWithExtra = queryValue { limit = (+ 1) <$> queryValue.limit }
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

listObservationsStatement :: Statement.Statement
  (UUID, Maybe Text, Maybe Text, Maybe Text, Maybe Text, Int32, Int32) [Observation]
listObservationsStatement = Statement.Statement sql encoder (Dec.rowList observationDecoder) True
  where
    sql = BS8.pack $ unlines
      [ "SELECT id, workspace_id, subject_kind::text, subject, git_sha, content, created_at, updated_at"
      , "FROM observations"
      , "WHERE workspace_id = $1"
      , "  AND ($2::text IS NULL OR subject_kind::text = $2)"
      , "  AND ($3::text IS NULL OR subject = $3)"
      , "  AND ($4::text IS NULL OR git_sha = $4)"
      , "  AND ($5::text IS NULL OR search_vector @@ plainto_tsquery('simple', $5))"
      , "ORDER BY"
      , "  CASE WHEN $5::text IS NULL THEN 0 ELSE ts_rank(search_vector, plainto_tsquery('simple', $5)) END DESC,"
      , "  updated_at DESC, id DESC"
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
      [ "SELECT id, workspace_id, subject_kind::text, subject, git_sha, content, created_at, updated_at,"
      , "       1 - (embedding <=> $1::vector) AS similarity"
      , "FROM observations"
      , "WHERE embedding IS NOT NULL"
      , "  AND workspace_id = $2"
      , "  AND ($3::text IS NULL OR subject_kind::text = $3)"
      , "  AND ($4::text IS NULL OR subject = $4)"
      , "  AND ($5::text IS NULL OR git_sha = $5)"
      , "  AND 1 - (embedding <=> $1::vector) >= $6"
      , "ORDER BY embedding <=> $1::vector ASC, updated_at DESC, id DESC"
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
  kindText <- Dec.column (Dec.nonNullable Dec.text)
  kind <- maybe (fail $ "Unexpected observation_subject_kind: " <> T.unpack kindText) pure (subjectKindFromText kindText)
  observationSubject <- Dec.column (Dec.nonNullable Dec.text)
  sha <- Dec.column (Dec.nonNullable Dec.text)
  observationContent <- Dec.column (Dec.nonNullable Dec.text)
  created <- Dec.column (Dec.nonNullable Dec.timestamptz)
  updated <- Dec.column (Dec.nonNullable Dec.timestamptz)
  pure Observation
    { id = observationId, workspaceId = workspace, subjectKind = kind
    , subject = observationSubject, gitSha = sha, content = observationContent
    , createdAt = created, updatedAt = updated
    }

similarObservationDecoder :: Dec.Row SimilarObservation
similarObservationDecoder = SimilarObservation <$> observationDecoder <*> Dec.column (Dec.nonNullable Dec.float8)

vecText :: [Double] -> Text
vecText vectorValues = "[" <> T.intercalate "," (map (T.pack . show) vectorValues) <> "]"
