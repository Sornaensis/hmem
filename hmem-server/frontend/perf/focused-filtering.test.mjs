import test from 'node:test'
import assert from 'node:assert/strict'
import path from 'node:path'
import { FOCUSED_FILTERING_BASE, FOCUSED_FILTERING_TASK, FOCUSED_FILTERING_PROJECT, FOCUSED_FILTERING_REVISION, LOADING_EFFICIENCY_BASE, LOADING_EFFICIENCY_TASK, evidenceProfile, evidenceOwnerFields, isolatedRecordOutputs, assertEvidenceIdentity, assertEvidenceWrite, prepareEvidenceScratch, createEvidenceOperations, checkQualification } from './evidence-profile.mjs'

const frontend = path.resolve('fixture-root/hmem-server/frontend'), temporary = path.resolve('outside-temporary')
const plan = () => evidenceProfile(FOCUSED_FILTERING_BASE, frontend, temporary)
const head = '1'.repeat(40)
const identity = profile => ({ taskId: profile.task.taskId, ...evidenceOwnerFields(profile), evidenceBaseCommit: profile.baseCommit, measurementRevision: profile.revision,
  sourceProvenance: { reviewBaseCommit: profile.baseCommit, headCommit: head }, inputQualification: { implementationCommit: head } })

test('focused filtering owns five isolated receipts with unchanged ordinary scenarios and historical owners', () => {
  const profile = plan()
  assert.deepEqual([profile.task.taskId, profile.task.projectId, profile.revision], [FOCUSED_FILTERING_TASK, FOCUSED_FILTERING_PROJECT, FOCUSED_FILTERING_REVISION])
  assert.deepEqual([profile.scaling, profile.rendering, profile.planningCounts, profile.dueRemoval, profile.loadingEfficiency], [false, false, false, false, false])
  assert.equal(profile.strictSourceIdentity, true); assert.equal(profile.isolatedEvidence, true); assert.equal(profile.qualifiedRetirement, true)
  assert.equal(profile.root, path.join(temporary, 'hmem-focused-filtering-' + FOCUSED_FILTERING_TASK))
  assert.equal(profile.trace, path.join(profile.root, 'temporary/large-focused-filtering-trace.zip'))
  assert.equal(new Set(Object.values(profile.files)).size, 5)
  assert.match(profile.retention, /closure plus 30 days, then dispose/); assert.match(profile.retention, /64 MiB/)
  assert.equal(evidenceProfile(LOADING_EFFICIENCY_BASE, frontend, temporary).task.taskId, LOADING_EFFICIENCY_TASK)
  assert.throws(() => evidenceProfile(FOCUSED_FILTERING_BASE, frontend, frontend), /outside/)
})

test('focused receipts reject foreign identity, redirected outputs, scenario changes and oversized writes', () => {
  const profile = plan(), record = identity(profile), baseline = path.join(frontend, 'perf/baseline.v1.json')
  assert.doesNotThrow(() => assertEvidenceIdentity(profile, record))
  for (const field of ['taskId', 'projectId', 'evidenceBaseCommit', 'measurementRevision']) assert.throws(() => assertEvidenceIdentity(profile, { ...record, [field]: 'foreign' }), /identity mismatch/)
  assert.throws(() => assertEvidenceIdentity(profile, { ...record, inputQualification: { implementationCommit: '2'.repeat(40) } }), /identity mismatch/)
  for (const altered of [{ ...profile, focusedFiltering: false }, { ...profile, root: path.join(temporary, 'foreign') }, { ...profile, scaling: true }, { ...profile, strictSourceIdentity: false }, { ...profile, files: { ...profile.files, after: baseline } }]) assert.throws(() => assertEvidenceIdentity(altered, record), /mismatch/)
  assert.throws(() => isolatedRecordOutputs(profile, baseline, baseline, 'legacy', 'legacy-trace'), /task-owned/)
  assert.throws(() => assertEvidenceWrite(profile, baseline, '{}'), /Unowned/)
  assert.throws(() => assertEvidenceWrite(profile, profile.files.after, Buffer.alloc(64 * 1024 * 1024 + 1)), /64 MiB/)
})

test('focused evidence writes require verified stable temporary directory ownership', () => {
  const profile = plan(), entries = new Map([[temporary, { ino: 1 }], [profile.root, { ino: 2 }]])
  let writes = 0
  const io = { existsSync: file => entries.has(file), realpathSync: file => file,
    lstatSync: file => ({ dev: 1, ino: entries.get(file).ino, isSymbolicLink: () => false, isDirectory: () => true, isFile: () => false }),
    readdirSync: () => [], writeFileSync() { writes++ } }
  const operations = createEvidenceOperations(profile, temporary, io)
  assert.throws(() => operations.write(profile.files.after, '{}'), /not been admitted/)
  prepareEvidenceScratch(profile, temporary, io); entries.get(profile.root).ino = 3
  assert.throws(() => operations.write(profile.files.after, '{}'), /lifetime changed.*no fallback/)
  assert.equal(writes, 0)
})

test('qualification requires both fresh evaluations and completed trace retirement', () => {
  const input = { recordEvaluation: { passed: true }, checkEvaluation: { passed: true }, retirement: { passed: true }, commands: ['npm run perf:self-check', 'node perf/harness.mjs record', 'node perf/harness.mjs check'].map(command => ({ command, exitCode: 0 })) }
  assert.equal(checkQualification(input).passed, true)
  for (const field of ['recordEvaluation', 'checkEvaluation', 'retirement']) assert.equal(checkQualification({ ...input, [field]: { passed: false } }).passed, false)
})
