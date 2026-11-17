module HMem.DB.ObservationDiscoverySpec (spec) where

import Control.Exception (try)
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as LBS
import Data.Functor.Contravariant (contramap)
import Data.Pool (Pool)
import Data.Text.Encoding qualified as TE
import Hasql.Connection qualified as Hasql
import Hasql.Decoders qualified as Dec
import Hasql.Encoders qualified as Enc
import Hasql.Statement qualified as Statement
import Data.List (sort)
import Data.Text (Text)
import Data.Text qualified as T
import Data.UUID (UUID)
import Hasql.Session qualified as Session
import Test.Hspec

import HMem.DB.Embedding qualified as Embedding
import HMem.DB.Observation hiding (discoverObservations)
import HMem.DB.Observation qualified as Observation
import HMem.DB.ObservationFixture (reviewed, writeObservation)
import HMem.DB.Pool (DBException, runSession)
import HMem.DB.TestHarness
import HMem.Types

spec :: Spec
spec = beforeAll setupTestPool $ aroundWith withTestTransaction $ describe "Observation discovery retrieval" $ do
  it "plans list, continuation and tree views without joins filtering CTE scans" $ \env -> do
    workspace <- createTestWorkspace env "discovery-plan"
    _ <- createObservation env.pool (input workspace.id [ObservationSubject SubjectFile "src/plan.hs"] "needle")
    let f = emptyFilter { subjectsText = Just "src/", contentQuery = Just "needle" }
        base = request workspace.id f
    plans <- mapM (explainDiscovery env f)
      [ base, (base :: ObservationDiscoveryRequest) { offset = Just 50 }
      , base { terminal = Just (ObservationSubject SubjectFile "src/plan.hs") }, base { view = DiscoveryTree, treePrefix = "src" } ]
    -- The pre-repair statement joined CTE scans under nested-loop join filters.
    mapM_ (\plan -> cteJoinFilters plan `shouldBe` []) plans

  it "composes subject and content independently, with literal case-sensitive subject text" $ \env -> do
    workspace <- createTestWorkspace env "discovery-and"
    subjectOnly <- createObservation env.pool (input workspace.id [ObservationSubject SubjectFile "src/needle.hs"] "ordinary")
    both <- createObservation env.pool (input workspace.id [ObservationSubject SubjectFile "src/%_Name.hs"] "needle")
    contentOnly <- createObservation env.pool (input workspace.id [ObservationSubject SubjectFile "docs/Guide.md"] "needle")
    let run f = discoverObservations env.pool f (request workspace.id f) Nothing
    allRows <- run emptyFilter
    map (.id) allRows.items `shouldMatchList` [subjectOnly.id,both.id,contentOnly.id]
    bodyRows <- run emptyFilter { contentQuery = Just "needle" }
    map (.id) bodyRows.items `shouldMatchList` [both.id,contentOnly.id]
    subjectRows <- run emptyFilter { subjectsText = Just "src/" }
    map (.id) subjectRows.items `shouldMatchList` [subjectOnly.id,both.id]
    joined <- run emptyFilter { subjectsText = Just "%_", contentQuery = Just "needle" }
    map (.id) joined.items `shouldBe` [both.id]
    mismatch <- run emptyFilter { subjectsText = Just "%_name", contentQuery = Just "needle" }
    mismatch.items `shouldBe` []

  it "shares complete distinct membership across typed tree terminals, list pages and counts" $ \env -> do
    workspace <- createTestWorkspace env "discovery-tree"
    multi <- createObservation env.pool (input workspace.id
      [ObservationSubject SubjectFile "src",ObservationSubject SubjectGlob "src",ObservationSubject SubjectFile "src/A.hs",ObservationSubject SubjectGlob "src/**/*.hs"] "needle")
    another <- createObservation env.pool (input workspace.id [ObservationSubject SubjectFile "src/A.hs"] "needle")
    _ <- createObservation env.pool (input workspace.id [ObservationSubject SubjectFile "src/Hidden.hs"] "ordinary")
    let f = emptyFilter { subjectsText = Just "src", contentQuery = Just "needle" }
        base = request workspace.id f
    first <- discoverObservations env.pool f (base :: ObservationDiscoveryRequest) { limit = Just 1 } Nothing
    second <- discoverObservations env.pool f (base :: ObservationDiscoveryRequest) { limit = Just 1,offset = Just 1 } Nothing
    (first.hasMore,second.hasMore) `shouldBe` (True,False)
    sort (map (.id) (first.items <> second.items)) `shouldBe` sort [multi.id,another.id]
    first.counts `shouldBe` ObservationCounts workspace.id 3 2
    counted <- discoverObservations env.pool f base { view = DiscoveryCount } Nothing
    (counted.items,counted.counts) `shouldBe` ([],first.counts)
    root <- discoverObservations env.pool f base { view = DiscoveryTree } Nothing
    case root.tree of
      Just tree -> do
        (tree.totalCount,tree.matchCount) `shouldBe` (3,2)
        map (.path) tree.items `shouldBe` ["src"]
        map (\t -> (t.subjectKind,t.observationCount)) (concatMap (.terminals) tree.items)
          `shouldBe` [(SubjectFile,1),(SubjectGlob,1)]
      Nothing -> expectationFailure "missing tree"
    leaf <- discoverObservations env.pool f base { terminal = Just (ObservationSubject SubjectGlob "src") } Nothing
    map (.id) leaf.items `shouldBe` [multi.id]
    children <- discoverObservations env.pool f base { view = DiscoveryTree,treePrefix = "src" } Nothing
    fmap (map (.path) . (.items)) children.tree `shouldBe` Just ["src/**","src/A.hs"]

  it "uses one subject row for kind, exact subject and substring, with every SHA predicate ANDed" $ \env -> do
    workspace <- createTestWorkspace env "discovery-provenance"
    cross <- createObservation env.pool (input workspace.id [ObservationSubject SubjectGlob "src/Exact.hs",ObservationSubject SubjectFile "docs/Other.hs"] "needle")
    kept <- createObservation env.pool (input workspace.id [ObservationSubject SubjectFile "src/Exact.hs"] "needle")
    _ <- writeObservation env.pool workspace.id kept.id (reviewed "needle")
    let f = emptyFilter { subjectKind = Just SubjectFile,subject = Just "src/Exact.hs",subjectsText = Just "src/",contentQuery = Just "needle",gitSha = Just shaA,currentGitSha = Just shaB,historyGitSha = Just shaA }
        run value = discoverObservations env.pool value (request workspace.id value) Nothing
    result <- run f
    map (.id) result.items `shouldBe` [kept.id]
    map (.id) result.items `shouldNotSatisfy` elem cross.id
    mapM_ (\value -> run value >>= \page -> page.items `shouldBe` [])
      [(f :: ObservationDiscoveryFilter) { gitSha = Just shaB },(f :: ObservationDiscoveryFilter) { currentGitSha = Just shaA },(f :: ObservationDiscoveryFilter) { historyGitSha = Just (T.replicate 40 "c") },f { subjectsText = Just "docs" }]

  it "executes FTS with the extension and embedding column absent" $ \env -> do
    runSession env.pool $ Session.sql "DROP INDEX IF EXISTS idx_observations_embedding; ALTER TABLE observations DROP COLUMN IF EXISTS embedding; DROP EXTENSION IF EXISTS vector"
    workspace <- createTestWorkspace env "discovery-no-vector"
    row <- createObservation env.pool (input workspace.id [ObservationSubject SubjectFile "src/Main.hs"] "needle")
    let f = emptyFilter { contentQuery = Just "needle" }
    result <- discoverObservations env.pool f (request workspace.id f) Nothing
    map (.id) result.items `shouldBe` [row.id]

  it "executes FTS when the extension exists but the optional column is absent" $ \env -> do
    provisionVectors env
    runSession env.pool $ Session.sql "DROP INDEX IF EXISTS idx_observations_embedding; ALTER TABLE observations DROP COLUMN IF EXISTS embedding"
    workspace <- createTestWorkspace env "discovery-no-column"
    row <- createObservation env.pool (input workspace.id [ObservationSubject SubjectFile "src/Main.hs"] "needle")
    let f = emptyFilter { contentQuery = Just "needle" }
    result <- discoverObservations env.pool f (request workspace.id f) Nothing
    map (.id) result.items `shouldBe` [row.id]

  it "unions simultaneous FTS-only, semantic-only and overlapping hits before ranking paging counts and tree pruning" $ \env -> do
    provisionVectors env
    workspace <- createTestWorkspace env "discovery-hybrid"
    foreignWorkspace <- createTestWorkspace env "discovery-foreign"
    ftsOnly <- createObservation env.pool (input workspace.id [ObservationSubject SubjectFile "src/fts.hs"] "needle needle needle")
    semanticOnly <- createObservation env.pool (input workspace.id [ObservationSubject SubjectFile "src/semantic.hs"] "other")
    overlap <- createObservation env.pool (input workspace.id [ObservationSubject SubjectFile "src/overlap.hs",ObservationSubject SubjectGlob "src/*.hs"] "needle")
    below <- createObservation env.pool (input workspace.id [ObservationSubject SubjectFile "src/below.hs"] "other")
    wrongSpace <- createObservation env.pool (input workspace.id [ObservationSubject SubjectFile "src/wrong.hs"] "other")
    foreignRow <- createObservation env.pool (input foreignWorkspace.id [ObservationSubject SubjectFile "src/foreign.hs"] "needle")
    let Just space = parseEmbeddingSpaceFingerprint "test:discovery:v1"
    mapM_ (\row -> Embedding.setObservationEmbeddingInSpace env.pool workspace.id row.id space unitX) [semanticOnly,overlap]
    Embedding.setObservationEmbeddingInSpace env.pool workspace.id below.id space unitY
    Embedding.setObservationEmbeddingInSpace env.pool workspace.id wrongSpace.id legacyManualEmbeddingSpace unitX
    Embedding.setObservationEmbeddingInSpace env.pool foreignWorkspace.id foreignRow.id space unitX
    let f = emptyFilter { subjectsText = Just "src/",contentQuery = Just "needle" }
        base = request workspace.id f
        run value = discoverObservations env.pool f value (Just (unitX,"test:discovery:v1"))
    result <- run base
    map (.id) result.items `shouldMatchList` [ftsOnly.id,semanticOnly.id,overlap.id]
    map (.id) (take 1 result.items) `shouldBe` [overlap.id]
    again <- run base
    map (.id) again.items `shouldBe` map (.id) result.items
    result.counts `shouldBe` ObservationCounts workspace.id 5 3
    first <- run (base :: ObservationDiscoveryRequest) { limit = Just 2 }
    second <- run (base :: ObservationDiscoveryRequest) { limit = Just 2,offset = Just 2 }
    map (.id) (first.items <> second.items) `shouldBe` map (.id) result.items
    (first.hasMore,second.hasMore) `shouldBe` (True,False)
    tree <- run base { view = DiscoveryTree,treePrefix = "src" }
    fmap (.matchCount) tree.tree `shouldBe` Just 3
    fmap (map (.path) . (.items)) tree.tree `shouldBe` Just ["src/*.hs","src/fts.hs","src/overlap.hs","src/semantic.hs"]
    leaf <- run base { terminal = Just (ObservationSubject SubjectFile "src/semantic.hs") }
    map (.id) leaf.items `shouldBe` [semanticOnly.id]

emptyFilter :: ObservationDiscoveryFilter
emptyFilter = ObservationDiscoveryFilter Nothing Nothing Nothing Nothing Nothing Nothing Nothing

request :: UUID -> ObservationDiscoveryFilter -> ObservationDiscoveryRequest
request workspace filters = ObservationDiscoveryRequest workspace Nothing Nothing Nothing Nothing (Just filters) DiscoveryList "" Nothing Nothing Nothing

input :: UUID -> [ObservationSubject] -> Text -> CreateObservation
input workspace subjects content = CreateObservation workspace subjects shaA content

shaA, shaB :: Text
shaA = T.replicate 40 "a"
shaB = T.replicate 40 "b"

unitX, unitY :: [Double]
unitX = 1 : replicate 1535 0
unitY = 0 : 1 : replicate 1534 0

-- Provision only inside the disposable test transaction. No installed package
-- or user database is changed. A missing optional package is a setup limitation.
provisionVectors :: TestEnv -> IO ()
provisionVectors env = do
  result <- try @DBException $ runSession env.pool $ Session.sql
    "CREATE EXTENSION IF NOT EXISTS vector; ALTER TABLE observations ADD COLUMN IF NOT EXISTS embedding vector(1536)"
  case result of
    Left _ -> pendingWith "optional pgvector PostgreSQL package unavailable in test sandbox"
    Right () -> pure ()

-- The pre-repair discovery statement, kept verbatim as an equivalence oracle.
oracleDiscoveryStatement :: Bool -> Statement.Statement (Text, Maybe Text, Maybe Text) ObservationDiscoveryPage
oracleDiscoveryStatement hybrid = Statement.Statement (BS8.pack (unlines sql)) encoder decoder True
  where
    -- Both variants bind only ordinary text parameters. The FTS variant never
    -- mentions vector types or the optional embedding column, even in dead SQL.
    sql =
      [ "WITH args AS (SELECT $1::jsonb AS p, $2::text AS vector_text, $3::text AS space),"
      , "matching_subjects AS MATERIALIZED ("
      , " SELECT s.observation_id AS id,s.subject_kind::text AS subject_kind,s.subject FROM observation_subjects s"
      , " JOIN observations o ON o.id=s.observation_id CROSS JOIN args a"
      , " WHERE o.workspace_id=(a.p->>'workspace_id')::uuid"
      , " AND (a.p->'filters'->>'subject_kind' IS NULL OR s.subject_kind::text=a.p->'filters'->>'subject_kind')"
      , " AND (a.p->'filters'->>'subject' IS NULL OR s.subject=a.p->'filters'->>'subject')"
      , " AND (a.p->'filters'->>'subject_query' IS NULL OR position(a.p->'filters'->>'subject_query' IN s.subject)>0)"
      , "), eligible AS MATERIALIZED ("
      , " SELECT o.*,a.p->'filters'->>'content_query' AS body_query,"
      , " to_tsvector('simple',o.content) @@ plainto_tsquery('simple',a.p->'filters'->>'content_query') AS fts_match,"
      , " ts_rank(to_tsvector('simple',o.content),plainto_tsquery('simple',a.p->'filters'->>'content_query')) AS fts_score"
      , " FROM observations o CROSS JOIN args a WHERE o.id IN (SELECT id FROM matching_subjects)"
      , " AND (a.p->'filters'->>'git_sha' IS NULL OR o.git_sha=a.p->'filters'->>'git_sha')"
      , " AND (a.p->'filters'->>'current_git_sha' IS NULL OR o.current_provenance->>'reviewed_git_sha'=a.p->'filters'->>'current_git_sha')"
      , " AND (a.p->'filters'->>'history_git_sha' IS NULL OR EXISTS (SELECT 1 FROM observation_revision_events e WHERE e.observation_id=o.id AND e.reviewed_git_sha=a.p->'filters'->>'history_git_sha'))"
      , "), fts_ranked AS (SELECT id,row_number() OVER (ORDER BY fts_score DESC,updated_at DESC,id DESC) AS rank FROM eligible WHERE fts_match),"
      ] <> semanticSql <>
      [ "membership AS MATERIALIZED ("
      , " SELECT e.id,e.updated_at," <> scoreSql <> " AS rank FROM eligible e"
      , " LEFT JOIN fts_ranked f ON f.id=e.id LEFT JOIN semantic_ranked s ON s.id=e.id"
      , " WHERE e.body_query IS NULL OR f.id IS NOT NULL OR s.id IS NOT NULL"
      , "), terminals AS MATERIALIZED (SELECT ms.* FROM matching_subjects ms JOIN membership m ON m.id=ms.id),"
      , "list_page AS MATERIALIZED (SELECT m.* FROM membership m CROSS JOIN args a"
      , " WHERE a.p->>'view'='list' AND ((a.p->>'terminal') IS NULL OR EXISTS (SELECT 1 FROM terminals t WHERE t.id=m.id AND t.subject=a.p->'terminal'->>'subject' AND t.subject_kind=a.p->'terminal'->>'subject_kind'))"
      , " ORDER BY m.rank DESC,m.updated_at DESC,m.id DESC LIMIT ((SELECT (p->>'limit')::int FROM args)+1) OFFSET (SELECT (p->>'offset')::int FROM args)),"
      , "descendants AS (SELECT t.*,CASE WHEN a.p->>'prefix'='' THEN split_part(t.subject,'/',1) ELSE a.p->>'prefix'||'/'||split_part(substr(t.subject,length(a.p->>'prefix')+2),'/',1) END AS path"
      , " FROM terminals t CROSS JOIN args a WHERE a.p->>'view'='tree' AND (a.p->>'prefix'='' OR left(t.subject,length(a.p->>'prefix')+1)=a.p->>'prefix'||'/')),"
      , "nodes AS (SELECT path,COUNT(DISTINCT id) AS observation_count,bool_or(subject<>path) AS has_children FROM descendants GROUP BY path),"
      , "tree_page AS MATERIALIZED (SELECT * FROM nodes ORDER BY path COLLATE \"C\" LIMIT (SELECT (p->>'limit')::int FROM args) OFFSET (SELECT (p->>'offset')::int FROM args)),"
      , "counts AS (SELECT (SELECT COUNT(*) FROM observations o CROSS JOIN args a WHERE o.workspace_id=(a.p->>'workspace_id')::uuid) AS total_count,(SELECT COUNT(*) FROM membership) AS match_count),"
      , "tree_result AS (SELECT jsonb_build_object('total_count',c.total_count,'match_count',c.match_count,"
      , " 'has_more',(SELECT COUNT(*) FROM nodes)>(a.p->>'offset')::bigint+(a.p->>'limit')::bigint,"
      , " 'items',COALESCE((SELECT jsonb_agg(jsonb_build_object('path',p.path,'segment',CASE WHEN a.p->>'prefix'='' THEN p.path ELSE substr(p.path,length(a.p->>'prefix')+2) END,"
      , " 'observation_count',p.observation_count,'has_children',p.has_children,'terminals',COALESCE((SELECT jsonb_agg(jsonb_build_object('subject_kind',ts.subject_kind,'subject',ts.subject,'observation_count',ts.n) ORDER BY ts.subject_kind COLLATE \"C\")"
      , " FROM (SELECT subject_kind,subject,COUNT(DISTINCT id) AS n FROM terminals WHERE subject=p.path GROUP BY subject_kind,subject) ts),'[]'::jsonb)) ORDER BY p.path COLLATE \"C\") FROM tree_page p),'[]'::jsonb)) AS value FROM args a CROSS JOIN counts c)"
      , "SELECT jsonb_build_object('counts',jsonb_build_object('workspace_id',a.p->>'workspace_id','total_count',c.total_count,'match_count',c.match_count),"
      , " 'has_more',(SELECT COUNT(*) FROM list_page)>(a.p->>'limit')::int,"
      , " 'tree',CASE WHEN a.p->>'view'='tree' THEN (SELECT value FROM tree_result) ELSE NULL END,"
      , " 'items',COALESCE((SELECT jsonb_agg(row_data.value ORDER BY row_data.rank DESC,row_data.updated_at DESC,row_data.id DESC) FROM ("
      , " SELECT lp.rank,lp.updated_at,lp.id,jsonb_build_object('id',o.id,'workspace_id',o.workspace_id,'git_sha',o.git_sha,'content',o.content,'created_at',o.created_at,'updated_at',o.updated_at,'content_version',o.content_version,'latest_sequence',o.latest_sequence,'current_provenance',o.current_provenance,"
      , " 'subjects',(SELECT jsonb_agg(jsonb_build_object('subject_kind',s.subject_kind::text,'subject',s.subject) ORDER BY s.ordinal) FROM observation_subjects s WHERE s.observation_id=o.id)) AS value"
      , " FROM list_page lp JOIN observations o ON o.id=lp.id ORDER BY lp.rank DESC,lp.updated_at DESC,lp.id DESC LIMIT (SELECT (p->>'limit')::int FROM args)) row_data),'[]'::jsonb)) FROM args a CROSS JOIN counts c"
      ]
    semanticSql
      | hybrid =
          [ "distances AS MATERIALIZED (SELECT e.id,e.updated_at,1-(e.embedding <=> a.vector_text::vector) AS similarity FROM eligible e CROSS JOIN args a WHERE e.embedding IS NOT NULL AND e.embedding_space_fingerprint=a.space),"
          , "semantic_ranked AS (SELECT id,row_number() OVER (ORDER BY similarity DESC,updated_at DESC,id DESC) AS rank FROM distances WHERE similarity>=0.5)," ]
      | otherwise = ["semantic_ranked AS (SELECT NULL::uuid AS id,NULL::bigint AS rank WHERE false),"]
    scoreSql
      | hybrid = "CASE WHEN e.body_query IS NULL THEN 0::float8 ELSE COALESCE(1.0::float8/(60+f.rank),0)+COALESCE(1.0::float8/(60+s.rank),0) END"
      | otherwise = "COALESCE(e.fts_score,0)::float8"
    encoder = contramap (\(a,_,_) -> a) (Enc.param (Enc.nonNullable Enc.text))
      <> contramap (\(_,b,_) -> b) (Enc.param (Enc.nullable Enc.text))
      <> contramap (\(_,_,c) -> c) (Enc.param (Enc.nullable Enc.text))
    decoder = Dec.singleRow $ Dec.column (Dec.nonNullable Dec.jsonb) >>= \value -> case Aeson.fromJSON value of
      Aeson.Success result -> pure result
      Aeson.Error message -> fail message

-- Every discovery in this spec is also checked against the oracle.
discoverObservations :: Pool Hasql.Connection -> ObservationDiscoveryFilter -> ObservationDiscoveryRequest -> Maybe ([Double], Text) -> IO ObservationDiscoveryPage
discoverObservations pool f value semantic = do
  actual <- Observation.discoverObservations pool f value semantic
  expected <- discoverObservationsUsing oracleDiscoveryStatement pool f value semantic
  actual `shouldBe` expected
  pure actual

-- EXPLAIN of the FTS discovery statement for one request, with its exact
-- parameter payload.
explainDiscovery :: TestEnv -> ObservationDiscoveryFilter -> ObservationDiscoveryRequest -> IO Aeson.Value
explainDiscovery env rawFilter value = do
  plan <- runSession env.pool $ do
    let f = normalizeObservationDiscoveryFilter rawFilter
        payload = Aeson.object
          [ "filters" Aeson..= f, "view" Aeson..= value.view, "prefix" Aeson..= value.treePrefix
          , "terminal" Aeson..= value.terminal
          , "limit" Aeson..= maybe 50 id value.limit, "offset" Aeson..= maybe 0 id value.offset
          , "workspace_id" Aeson..= value.workspaceId ]
        literal = BS8.pack (concatMap (\c -> if c == '\'' then "''" else [c]) (T.unpack (TE.decodeUtf8 (LBS.toStrict (Aeson.encode payload)))))
        Statement.Statement sqlText _ _ _ = discoveryStatement False
    -- Base tables use their indexes, as at production size.
    Session.sql "SET LOCAL enable_seqscan = off"
    Session.sql ("PREPARE discovery_plan(text, text, text) AS " <> sqlText)
    rows <- Session.statement () (Statement.Statement ("EXPLAIN (FORMAT JSON) EXECUTE discovery_plan('" <> literal <> "', NULL, NULL)")
      Enc.noParams (Dec.rowList (Dec.column (Dec.nonNullable Dec.text))) False)
    Session.sql "DEALLOCATE discovery_plan"
    pure rows
  -- EXPLAIN returns its JSON document as text.
  either fail pure (Aeson.eitherDecodeStrict (TE.encodeUtf8 (T.concat plan)))

-- Join filters of nested loops whose inputs both read a candidate CTE: each
-- pair of candidate rows is compared, so removed rows grow quadratically.
-- The one-row parameter CTE is not a candidate set.
cteJoinFilters :: Aeson.Value -> [Text]
cteJoinFilters = go
  where
    go = \case
      Aeson.Array nodes -> concatMap go nodes
      Aeson.Object node ->
        let children = plansOf node
            here = case (KeyMap.lookup "Node Type" node, KeyMap.lookup "Join Filter" node) of
              (Just (Aeson.String "Nested Loop"), Just (Aeson.String condition))
                | inputs <- filter isInput children, length inputs == 2 && all readsCte inputs -> [condition]
              _ -> []
        in here <> concatMap go (children <> [plan | Just plan <- [KeyMap.lookup "Plan" node]])
      _ -> []
    isInput = \case
      Aeson.Object child -> KeyMap.lookup "Parent Relationship" child `elem` map (Just . Aeson.String) ["Outer", "Inner"]
      _ -> False
    plansOf node = case KeyMap.lookup "Plans" node of Just (Aeson.Array plans) -> foldr (:) [] plans; _ -> []
    readsCte = \case
      Aeson.Object child -> case KeyMap.lookup "Node Type" child of
        Just (Aeson.String "CTE Scan") -> KeyMap.lookup "CTE Name" child /= Just (Aeson.String "args")
        _ -> any readsCte (plansOf child)
      _ -> False
