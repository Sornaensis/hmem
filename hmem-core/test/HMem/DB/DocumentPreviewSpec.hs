module HMem.DB.DocumentPreviewSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (mapConcurrently)
import Control.Exception (SomeException,try,bracket)
import Control.Monad (forM,void)
import Data.Aeson
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Either (isLeft)
import Data.Functor.Contravariant (contramap)
import Data.Pool (destroyAllResources)
import Data.Maybe (catMaybes)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time (getCurrentTime,diffUTCTime)
import Data.UUID (UUID)
import Data.UUID.V4 qualified as UUID
import Hasql.Decoders qualified as D
import Hasql.Encoders qualified as E
import Hasql.Session qualified as S
import Hasql.Statement qualified as St
import Test.Hspec
import HMem.DB.Document qualified as Document
import HMem.DB.DocumentPreview qualified as Preview
import HMem.DB.DocumentGrant qualified as Grant
import HMem.DB.Pool (runSession,runTransaction,createPool,DBException(..))
import HMem.DB.Task qualified as Task
import HMem.DB.TestHarness
import HMem.Document.Codec (payloadSha256)
import HMem.Document.Grant (PreviewStatus(..))
import HMem.Document.Preview
import HMem.Document.Types qualified as Doc
import HMem.Types qualified as Types

spec :: Spec
spec=around withTestEnv $ describe "Persisted document preview jobs" $ do
  it "records missing-launcher and global queue saturation without rejecting originals" $ \env->do
    missing<-fixture env
    shouldBe missing.document.previewState "failed"
    shouldBe missing.document.previewError(Just "renderer_unavailable")
    fixtures<-forM[1..33::Int](const(fixtureParts env))
    Preview.heartbeat env.pool config
    documents<-mapConcurrently(\(w,t)->upload env w t)fixtures
    shouldBe(length documents)33
    shouldReturn(json env "SELECT to_jsonb(count(*)) FROM document_preview_jobs WHERE state='queued'")(Number 32)
    shouldReturn(json env "SELECT to_jsonb(count(*)) FROM document_previews WHERE error_code='queue_full'")(Number 1)
    shouldReturn(Document.readOriginal env.pool missing.workspace missing.task missing.document.id)original

  it "preserves another healthy controller and settles queued backlog only after availability expires" $ \env->do
    Preview.heartbeat env.pool config
    f<-fixture env
    shouldReturn(Preview.unavailable env.pool f.workspace f.document.id)False
    execute env "UPDATE document_preview_runtime SET available_until=clock_timestamp()-interval '1 second'"
    shouldReturn(Preview.unavailable env.pool f.workspace f.document.id)True
    shouldReturn(Preview.unavailable env.pool f.workspace f.document.id)False
    Just status<-Grant.previewStatus env.pool f.workspace f.task f.document.id
    shouldBe status.state "failed"
    shouldBe status.errorCode(Just "renderer_unavailable")
    shouldBe status.retryable True
    shouldReturn(Document.readOriginal env.pool f.workspace f.task f.document.id)original

  it "serializes two physical worker claims across independent controllers" $ \env->do
    Preview.heartbeat env.pool config
    fixtures<-forM[1..3::Int](const(fixture env))
    attempts<-mapConcurrently(\f->do
      owner<-UUID.nextRandom; controller<-UUID.nextRandom; token<-UUID.nextRandom
      Preview.claim env.pool config f.workspace f.document.id owner controller token f.document.mediaType)fixtures
    shouldBe(length(catMaybes attempts))2
    shouldReturn(json env "SELECT to_jsonb(count(*)) FROM document_preview_runs WHERE released_at IS NULL")(Number 2)
    shouldReturn(json env "SELECT to_jsonb(count(*)) FROM document_preview_jobs WHERE state='queued'")(Number 1)

  it "publishes verified metadata, payloads and job completion together and keeps originals exact" $ \env->do
    (f,run)<-claimed env config
    metadata<-admitted env run
    Preview.closed env.pool run True False
    Preview.publish env.pool run metadata outputs
    shouldReturn(json env "SELECT to_jsonb(count(*)) FROM document_preview_jobs WHERE state='ready'")(Number 1)
    shouldReturn(json env "SELECT to_jsonb(count(*)) FROM document_preview_runs WHERE released_at IS NOT NULL")(Number 1)
    shouldReturn(json env "SELECT to_jsonb(count(*)) FROM document_reservations WHERE kind='preview'")(Number 0)
    Just status<-Grant.previewStatus env.pool f.workspace f.task f.document.id
    shouldBe status.generationMetadata(Just metadata)
    shouldBe status.retryable False
    shouldReturn(Document.readOriginal env.pool f.workspace f.task f.document.id)original
    reject(Preview.publish env.pool run metadata outputs)
    reject(execute env "UPDATE document_generations SET preview_metadata='{}'")
    shouldReturn(json env "SELECT to_jsonb(count(*)) FROM document_generations")(Number 1)

  it "rolls quota admission and manipulated output receipts back while retaining originals" $ \env->do
    (f,run)<-claimed env config
    Preview.started env.pool run 42
    Preview.inspected env.pool run
    execute env "UPDATE document_budgets SET binary_limit=1048576"
    reject(Preview.reserve env.pool run(payloadSha256 "plan")2 0 105906176)
    shouldReturn(json env "SELECT to_jsonb(count(*)) FROM document_reservations WHERE kind='preview'")(Number 0)
    Preview.closed env.pool run True False
    shouldReturn(Preview.failRun env.pool run "quota_exceeded" False)True
    shouldReturn(Document.readOriginal env.pool f.workspace f.task f.document.id)original
    shouldReturn(json env "SELECT to_jsonb(count(*)) FROM document_generations")(Number 0)

  it "rejects integer narrowing and stale success or failure after a later generation wins" $ \env->do
    (f,old)<-claimed env config
    Preview.started env.pool old 42; Preview.inspected env.pool old
    reject(Preview.reserve env.pool old(payloadSha256 "plan")4294967296 0 105906176)
    reject(Preview.reserve env.pool old(payloadSha256 "plan")2 4294967296 105906176)
    Preview.reserve env.pool old(payloadSha256 "plan")2 0 105906176
    Preview.rendering env.pool old
    Preview.closed env.pool old True False
    shouldReturn(Preview.failRun env.pool old "worker_crashed" True)True
    threadDelay 1200000
    Preview.heartbeat env.pool config
    run<-claimFixture env config f
    metadata<-admitted env run
    Preview.closed env.pool run True False
    Preview.publish env.pool run metadata outputs
    shouldReturn(Preview.failRun env.pool old "delayed_old_failure" False)False
    reject(Preview.publish env.pool old metadata outputs)
    Just status<-Grant.previewStatus env.pool f.workspace f.task f.document.id
    shouldBe status.generationId(Just run.id)
    shouldBe status.state "ready"

  it "exhausts the configured finite automatic attempts without creating a fourth run or losing originals" $ \env->do
    (f,first)<-claimed env config
    let failAttempt run=do
          Preview.started env.pool run 42
          Preview.inspected env.pool run
          Preview.closed env.pool run True False
          shouldReturn(Preview.failRun env.pool run "worker_crashed" True)True
    failAttempt first
    threadDelay 1200000
    Preview.heartbeat env.pool config
    second<-claimFixture env config f
    failAttempt second
    threadDelay 2200000
    Preview.heartbeat env.pool config
    third<-claimFixture env config f
    failAttempt third
    shouldReturn(Preview.nextJob env.pool)Nothing
    owner<-UUID.nextRandom;controller<-UUID.nextRandom;token<-UUID.nextRandom
    shouldReturn(Preview.claim env.pool config f.workspace f.document.id owner controller token f.document.mediaType)Nothing
    shouldReturn(json env "SELECT to_jsonb(count(*)) FROM document_preview_runs")(Number 3)
    shouldReturn(json env "SELECT to_jsonb(count(*)) FROM document_preview_runs WHERE released_at IS NULL")(Number 0)
    shouldReturn(json env "SELECT to_jsonb(attempts) FROM document_preview_jobs")(Number 3)
    Just status<-Grant.previewStatus env.pool f.workspace f.task f.document.id
    shouldBe status.state "failed"
    shouldBe status.errorCode(Just "worker_crashed")
    shouldBe status.retryable True
    shouldReturn(Document.readOriginal env.pool f.workspace f.task f.document.id)original

  it "automatically recovers only expired runs with verified native/container closure" $ \env->do
    Preview.heartbeat env.pool config
    f<-fixture env
    owner<-expired env f True
    Preview.recoverDue env.pool
    shouldReturn(json env "SELECT to_jsonb(count(*)) FROM document_preview_jobs WHERE state='queued'")(Number 1)
    shouldReturn(json env("SELECT to_jsonb(released_at IS NOT NULL) FROM document_preview_runs WHERE id='"<>uuid owner<>"'"))(Bool True)
    shouldReturn(Document.readOriginal env.pool f.workspace f.task f.document.id)original

  it "quarantines unknown creation across restart/recovery and refuses replacement admission" $ \env->do
    Preview.heartbeat env.pool config
    f<-fixture env
    owner<-expired env f False
    Preview.recoverDue env.pool
    Preview.recoverDue env.pool
    shouldReturn(json env "SELECT to_jsonb(hmem_document_preview_poisoned())")(Bool True)
    shouldReturn(json env("SELECT to_jsonb(released_at IS NULL) FROM document_preview_runs WHERE id='"<>uuid owner<>"'"))(Bool True)
    shouldReturn(Preview.retry env.pool f.workspace f.task f.document.id)False
    another<-fixture env
    shouldBe another.document.previewError(Just "cleanup_unverified")
    shouldReturn(Document.readOriginal env.pool another.workspace another.task another.document.id)original
    shouldReturn(json env "SELECT to_jsonb(count(*)) FROM document_preview_runs")(Number 1)

  it "coalesces retries, preserves active/ready jobs and admits a queue-full retry only after capacity exists" $ \env->do
    let bounded=config{queueLimit=1}
    Preview.heartbeat env.pool bounded
    first<-fixture env
    second<-fixture env
    shouldBe second.document.previewError(Just "queue_full")
    shouldReturn(Preview.retry env.pool second.workspace second.task second.document.id)False
    shouldReturn(Preview.retry env.pool first.workspace first.task first.document.id)False
    run<-claimFixture env bounded first
    metadata<-admitted env run
    shouldReturn(Preview.retry env.pool first.workspace first.task first.document.id)False
    Preview.closed env.pool run True False
    Preview.publish env.pool run metadata outputs
    shouldReturn(Preview.retry env.pool first.workspace first.task first.document.id)False
    accepted<-mapConcurrently(const(Preview.retry env.pool second.workspace second.task second.document.id))[1..8::Int]
    shouldBe(length(filter id accepted))1
    shouldReturn(json env "SELECT to_jsonb(hmem_document_preview_admitted())")(Number 1)

  it "rolls a slow publication back at the final SQL deadline fence, including generation/events/job completion" $ \env->do
    (f,run)<-claimed env (config{imageSeconds=6})
    metadata<-admitted env run
    Preview.closed env.pool run True False
    execute env "CREATE FUNCTION preview_test_slow_ready() RETURNS TRIGGER LANGUAGE plpgsql AS $$ BEGIN IF NEW.state='ready' THEN PERFORM pg_sleep(0.4); END IF; RETURN NULL; END $$; CREATE TRIGGER z_preview_test_slow_ready AFTER UPDATE ON document_previews FOR EACH ROW EXECUTE FUNCTION preview_test_slow_ready()"
    -- Native producers are simulated closed in this DB-only fixture. Exercise
    -- the SQL fence independently of the host timeout, without widening the
    -- six-second generation deadline or disabling any publication guard.
    prepared<-Document.preparePreview outputs
    waitUntilRemaining run 0.2
    reject(runTransaction env.pool $ do
      S.statement(run.id,run.leaseToken)(St.Statement "SELECT hmem_document_preview_require_run($1,$2,TRUE)" pair D.noResult True)
      descriptors<-Document.storePreparedPreview run.workspaceId prepared
      S.statement(run.id,run.leaseToken,descriptors,toJSON metadata)(St.Statement
        "SELECT hmem_document_preview_publish_run($1,$2,$3,$4)" parameters D.noResult True))
    execute env "DROP TRIGGER z_preview_test_slow_ready ON document_previews; DROP FUNCTION preview_test_slow_ready()"
    shouldReturn(json env "SELECT to_jsonb(count(*)) FROM document_generations")(Number 0)
    shouldReturn(json env "SELECT to_jsonb(count(*)) FROM document_payloads WHERE NOT original")(Number 0)
    shouldReturn(json env "SELECT to_jsonb(count(*)) FROM document_preview_runs WHERE released_at IS NOT NULL")(Number 0)
    shouldReturn(Document.readOriginal env.pool f.workspace f.task f.document.id)original

  it "bounds the whole facade preparation/transaction and preserves a stricter SQL timeout on rollback" $ \env->do
    (f,run)<-claimed env (config{imageSeconds=6})
    metadata<-admitted env run
    Preview.closed env.pool run True False
    execute env "CREATE FUNCTION preview_test_slow_insert() RETURNS TRIGGER LANGUAGE plpgsql AS $$ BEGIN PERFORM pg_sleep(1); RETURN NEW; END $$; CREATE TRIGGER preview_test_slow_insert BEFORE INSERT ON document_generations FOR EACH ROW EXECUTE FUNCTION preview_test_slow_insert()"
    -- A separate one-connection fixture pool has a genuinely stricter session
    -- timeout. If publication widened it to the generation budget, this first
    -- attempt would incorrectly sleep one second and publish successfully.
    bracket(createPool env.testDb.testDbConnStr 1 5 50)destroyAllResources $ \strict->do
      attempted<-try @DBException(Preview.publish strict run metadata outputs)
      case attempted of Left DBStatementTimeout->pure();_->expectationFailure "A stricter SQL timeout was widened or ignored"
      setting<-runSession strict(S.statement ()(St.Statement "SELECT current_setting('statement_timeout')" E.noParams
        (D.singleRow(D.column(D.nonNullable D.text)))True))
      shouldBe setting "50ms"
    shouldReturn(json env "SELECT to_jsonb(count(*)) FROM document_generations")(Number 0)
    waitUntilRemaining run 0.3
    reject(Preview.publish env.pool run metadata outputs)
    execute env "DROP TRIGGER preview_test_slow_insert ON document_generations; DROP FUNCTION preview_test_slow_insert()"
    shouldReturn(json env "SELECT to_jsonb(count(*)) FROM document_generations")(Number 0)
    shouldReturn(json env "SELECT to_jsonb(count(*)) FROM document_payloads WHERE NOT original")(Number 0)
    shouldReturn(Document.readOriginal env.pool f.workspace f.task f.document.id)original

data Fixture=Fixture{workspace::UUID,task::UUID,document::Doc.Document}
config :: PreviewConfig
config=defaultPreviewConfig{enabled=True,launcherPath=Just "simulated-db-only-launcher"}
original :: ByteString
original=png
fixtureParts :: TestEnv -> IO(UUID,UUID)
fixtureParts env=do
  workspace<-createTestWorkspace env "preview-jobs"
  task<-Task.createTask env.pool(Types.CreateTask workspace.id Nothing Nothing "preview task" Nothing Nothing Nothing)
  pure(workspace.id,task.id)
upload :: TestEnv -> UUID -> UUID -> IO Doc.Document
upload env workspace task=do
  request<-UUID.nextRandom
  Document.uploadDocument env.pool(Doc.UploadDocument workspace task request Doc.Source
    (Doc.OriginalManifest(fromIntegral(BS.length original))(payloadSha256 original)"image.png" "Image" "image/png"))original
fixture :: TestEnv -> IO Fixture
fixture env=do
  (workspace,task)<-fixtureParts env
  document<-upload env workspace task
  pure(Fixture workspace task document)
claimed :: TestEnv -> PreviewConfig -> IO(Fixture,PreviewRun)
claimed env cfg=do
  Preview.heartbeat env.pool cfg
  f<-fixture env
  run<-claimFixture env cfg f
  pure(f,run)
claimFixture :: TestEnv -> PreviewConfig -> Fixture -> IO PreviewRun
claimFixture env cfg f=do
  owner<-UUID.nextRandom; controller<-UUID.nextRandom; token<-UUID.nextRandom
  Preview.claim env.pool cfg f.workspace f.document.id owner controller token f.document.mediaType >>=maybe(fail "Expected current claim")pure
admitted :: TestEnv -> PreviewRun -> IO PreviewMetadata
admitted env run=do
  Preview.started env.pool run 42
  Preview.inspected env.pool run
  let metadata=PreviewMetadata(payloadSha256 "plan")run.rawSha256(payloadSha256(canonicalPreviewLimits run.limits))
        run.policyVersion run.imageId 2 0 1 0 False
  Preview.reserve env.pool run metadata.planSha256 2 0 105906176
  Preview.rendering env.pool run
  pure metadata
outputs :: [Document.Derivative]
outputs=[Document.Derivative "page" 0 "image/png" png,Document.Derivative "thumbnail" 0 "image/png" png]
png :: ByteString
png=BS.pack[137,80,78,71,13,10,26,10,0,0,0,13,73,72,68,82,0,0,0,1,0,0,0,1,8,4,0,0,0,181,28,12,2,0,0,0,11,73,68,65,84,120,218,99,100,248,15,0,1,5,1,1,39,24,227,102,0,0,0,0,73,69,78,68,174,66,96,130]
expired :: TestEnv -> Fixture -> Bool -> IO UUID
expired env f verified=do
  owner<-UUID.nextRandom; controller<-UUID.nextRandom; token<-UUID.nextRandom
  let boolean=if verified then "true" else "false"
      uncertain=if verified then "false" else "true"
      phase=if verified then "closed" else "allocated"
      limits=LBS.toStrict(encode config.limits)
  execute env("INSERT INTO document_preview_runs(id,workspace_id,document_id,job_version,controller_id,lease_token,original_sha256,policy_version,image_id,limits,limits_sha256,deadline_at,lease_until,phase,native_exited,cleanup_verified,creation_uncertain,created_at) SELECT '"<>uuid owner<>"',d.workspace_id,d.id,j.version,'"<>
    uuid controller<>"','"<>uuid token<>"',d.raw_sha256,'"<>TE.encodeUtf8 previewPolicyVersion<>"','"<>TE.encodeUtf8 config.imageId<>"','"<>
    limits<>"'::jsonb,'"<>TE.encodeUtf8(payloadSha256(canonicalPreviewLimits config.limits))<>"',clock_timestamp()-interval '30 seconds',clock_timestamp()-interval '20 seconds','"<>
    phase<>"',"<>boolean<>","<>boolean<>","<>uncertain<>",clock_timestamp()-interval '60 seconds' FROM documents d JOIN document_preview_jobs j ON j.document_id=d.id WHERE d.id='"<>uuid f.document.id<>"'")
  execute env("UPDATE document_preview_jobs SET state='processing',run_id='"<>uuid owner<>"' WHERE document_id='"<>uuid f.document.id<>"'; UPDATE document_previews SET state='processing' WHERE document_id='"<>uuid f.document.id<>"'")
  pure owner
waitUntilRemaining :: PreviewRun -> Double -> IO()
waitUntilRemaining run seconds=do
  now<-getCurrentTime
  let micros=max 0(floor((realToFrac(diffUTCTime run.deadlineAt now)-seconds)*1000000))
  threadDelay micros
execute :: TestEnv -> ByteString -> IO()
execute env sql=runSession env.pool(S.sql sql)
json :: TestEnv -> ByteString -> IO Value
json env sql=runSession env.pool(S.statement ()(St.Statement sql E.noParams(D.singleRow(D.column(D.nonNullable D.jsonb)))False))
uuid :: UUID -> ByteString
uuid=TE.encodeUtf8 . T.pack . show
reject :: IO a -> IO()
reject action=try @SomeException(void action) >>= \result->shouldSatisfy result isLeft
pair :: E.Params(UUID,UUID)
pair=contramap fst(E.param(E.nonNullable E.uuid))<>contramap snd(E.param(E.nonNullable E.uuid))
parameters :: E.Params(UUID,UUID,Value,Value)
parameters=contramap(\(a,_,_,_)->a)(E.param(E.nonNullable E.uuid))<>contramap(\(_,b,_,_)->b)(E.param(E.nonNullable E.uuid))
  <>contramap(\(_,_,c,_)->c)(E.param(E.nonNullable E.jsonb))<>contramap(\(_,_,_,d)->d)(E.param(E.nonNullable E.jsonb))
