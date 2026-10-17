import test from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import path from 'node:path'
import { LOADING_EFFICIENCY_BASE, LOADING_EFFICIENCY_TASK, LOADING_EFFICIENCY_PROJECT, LOADING_EFFICIENCY_REVISION, evidenceProfile, evidenceOwnerFields, isolatedRecordOutputs, assertEvidenceIdentity, assertEvidenceWrite, prepareEvidenceScratch, createEvidenceOperations } from './evidence-profile.mjs'
import { generateFixture, fixtureHash, navigationBranchResponse, navigationSummariesResponse } from './fixtures.mjs'
import { createNavigationCompletionIndex, HARNESS_CONFIGURATION } from './contracts.mjs'
import { createTracker, fixtureResponder, inputQualification } from './harness.mjs'

const frontend = path.resolve('fixture-root/hmem-server/frontend'), temporary = path.resolve('outside-temporary')
const plan = () => evidenceProfile(LOADING_EFFICIENCY_BASE, frontend, temporary)
const implementation = '1'.repeat(40)
const identity = profile => ({ taskId: profile.task.taskId, ...evidenceOwnerFields(profile), evidenceBaseCommit: profile.baseCommit, measurementRevision: profile.revision,
  sourceProvenance: { reviewBaseCommit: profile.baseCommit, headCommit: implementation }, inputQualification: { implementationCommit: implementation } })

test('loading qualification owns isolated evidence with ordinary unchanged scenarios and fixtures', () => {
  const profile = plan()
  assert.deepEqual([profile.task.taskId, profile.task.projectId, profile.revision], [LOADING_EFFICIENCY_TASK, LOADING_EFFICIENCY_PROJECT, LOADING_EFFICIENCY_REVISION])
  assert.deepEqual([profile.scaling, profile.rendering, profile.planningCounts, profile.dueRemoval], [false, false, false, false])
  assert.equal(profile.strictSourceIdentity, true)
  assert.equal(profile.root, path.join(temporary, 'hmem-loading-efficiency-' + LOADING_EFFICIENCY_TASK))
  assert.equal(profile.trace, path.join(profile.root, 'temporary/large-loading-efficiency-trace.zip'))
  assert.equal(profile.qualifiedRetirement, true)
  assert.equal(new Set(Object.values(profile.files)).size, 5)
  assert.match(profile.retention, /Final verification task .*project .*closure plus 30 days, then dispose/)
  assert.equal(HARNESS_CONFIGURATION.warmups, 2); assert.equal(HARNESS_CONFIGURATION.samples, 5)
  assert.equal(fixtureHash(generateFixture('small')), '0c425c77e5069b8d4cf26dbdebcf94037a22ee977e61d63ab3cceb1a7f090bc4')
  assert.equal(fixtureHash(generateFixture('large')), 'b98d53a1a1ca28641dc90cbb598f3026949edf702f0ae4696614520b5140dd63')
  assert.throws(() => evidenceProfile(LOADING_EFFICIENCY_BASE, frontend, frontend), /outside/)
})

test('loading outputs reject baseline redirects and foreign owner/source identities', () => {
  const profile = plan(), legacy = path.join(frontend, 'perf/final-working-tree.after.v1.json'), trace = path.join(frontend, 'perf/final-working-tree.trace-manifest.v1.json')
  assert.deepEqual(isolatedRecordOutputs(profile, legacy, trace, legacy, trace), { output: profile.files.after, traceManifest: profile.files.traceManifest })
  for (const file of [path.join(frontend, 'perf/baseline.v1.json'), path.join(temporary, 'foreign.json')]) {
    assert.throws(() => isolatedRecordOutputs(profile, file, trace, legacy, trace), /task-owned/)
    assert.throws(() => assertEvidenceWrite(profile, file, '{}'), /Unowned/)
  }
  const record = identity(profile)
  assert.doesNotThrow(() => assertEvidenceIdentity(profile, record))
  for (const field of ['taskId', 'projectId', 'evidenceBaseCommit', 'measurementRevision']) assert.throws(() => assertEvidenceIdentity(profile, { ...record, [field]: 'foreign' }), /identity mismatch/)
  for (const source of [{ ...record.sourceProvenance, reviewBaseCommit: '2'.repeat(40) }, { ...record.sourceProvenance, headCommit: '2'.repeat(40) }]) assert.throws(() => assertEvidenceIdentity(profile, { ...record, sourceProvenance: source }), /identity mismatch/)
  for (const changed of [{ ...profile, root: path.join(temporary, 'foreign') }, { ...profile, scaling: true }, { ...profile, files: { ...profile.files, after: legacy } }]) assert.throws(() => assertEvidenceWrite(changed, changed.files.after, '{}'), /mismatch/)
  assert.throws(() => assertEvidenceWrite(profile, profile.files.after, Buffer.alloc(64 * 1024 * 1024 + 1)), /64 MiB/)
  const inputs = inputQualification({ plan: profile, capture: { read: () => Buffer.from(implementation) } })
  assert.equal(inputs.implementationCommit, implementation)
  assert.ok(inputs.source.some(value => value.path.endsWith('/tests-js/observation-viewport-fixture.mjs')))
})

test('loading persistence requires admitted unchanged directory identity before any write', () => {
  const profile = plan(), entries = new Map([[temporary, { ino: 1 }], [profile.root, { ino: 2 }]])
  let writes = 0
  const io = { existsSync: file => entries.has(file), realpathSync: file => file,
    lstatSync: file => ({ dev: 1, ino: entries.get(file).ino, isSymbolicLink: () => false, isDirectory: () => true, isFile: () => false }),
    readdirSync: () => [], writeFileSync() { writes++ } }
  const operations = createEvidenceOperations(profile, temporary, io)
  assert.throws(() => operations.write(profile.files.after, '{}'), /not been admitted/)
  prepareEvidenceScratch(profile, temporary, io)
  entries.get(profile.root).ino = 3
  assert.throws(() => operations.write(profile.files.after, '{}'), /lifetime changed.*no fallback/)
  assert.equal(writes, 0)
})

test('navigation fixture skips requested kinds before selection and preserves legacy defaults and summary batches', () => {
  const fixture = generateFixture('small'), legacy = navigationBranchResponse(fixture)
  assert.deepEqual(navigationBranchResponse(fixture, { includeProjects: 'true', includeTasks: true }), legacy)
  assert.deepEqual(navigationBranchResponse(fixture, { includeProjects: false }).projects, { items: [], has_more: false })
  assert.deepEqual(navigationBranchResponse(fixture, { includeTasks: 'false' }).tasks, { items: [], has_more: false })
  Object.defineProperty(fixture, 'projects', { get() { throw new Error('skipped project query') } })
  assert.deepEqual(navigationBranchResponse(fixture, { includeProjects: false, includeTasks: false }).projects, { items: [], has_more: false })
  assert.throws(() => navigationBranchResponse(generateFixture('small'), { includeProjects: 'yes' }), /boolean/)
  const fresh = generateFixture('small'), ids = fresh.tasks.slice(0, 3).map(task => task.id)
  assert.deepEqual(navigationSummariesResponse(fresh, [], [...ids].reverse()).tasks.map(task => task.id), [...ids].reverse())
})

test('completion receipts leave skipped kinds pending rather than accepting authoritative emptiness', () => {
  const proof = createNavigationCompletionIndex({ children: {}, rootProjects: ['p'], rootTasks: ['t'], context: 'normal' })
  proof.completeSession(proof.beginSession(), true)
  const request = { owner: 'workspace_root', context: 'normal', projectOffset: 0, taskOffset: 0, projectLimit: 50, taskLimit: 50 }
  const skipped = proof.admit({ ...request, projectIncluded: false, taskIncluded: true })
  assert.deepEqual(Object.keys(skipped.slots), ['task'])
  const previous = { ...skipped.state.kinds.project }
  proof.complete(skipped, { projects: { ids: [], hasMore: false }, tasks: { ids: ['t'], hasMore: false } })
  assert.deepEqual(skipped.state.kinds.project, previous)
  assert.equal(proof.ready(), false)
  const legacy = proof.admit(request)
  assert.deepEqual(Object.keys(legacy.slots), ['project', 'task'])
})

test('actual fixture route records demand, empty skipped payloads and their exact body bytes', async () => {
  const fixture = generateFixture('small'), tracker = createTracker(fixture), respond = fixtureResponder(fixture, tracker)
  const url = `http://perf.local/api/v1/workspaces/${fixture.workspace.id}/navigation?parent_kind=workspace_root&project_limit=50&project_offset=0&task_limit=50&task_offset=0&include_projects=false&include_tasks=true`
  const request = { url: () => url, method: () => 'GET' }
  let body
  await respond({ request: () => request, fulfill: async response => { body = response.body } })
  const receipt = tracker.requests[0], value = JSON.parse(body)
  assert.deepEqual(Object.keys(receipt.proofNavigation.slots), ['task'])
  assert.deepEqual(value.projects, { items: [], has_more: false })
  assert.ok(value.tasks.items.length > 0)
  assert.equal(receipt.bytes, Buffer.byteLength(body))
  assert.equal(tracker.bytes['navigation:branch'], Buffer.byteLength(body))
  assert.ok(JSON.parse(fs.readFileSync('package.json')).scripts.test.includes('tests-js/readiness-batches.browser.test.js'))
})
