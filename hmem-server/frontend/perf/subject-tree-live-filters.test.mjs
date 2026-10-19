import test from 'node:test'
import assert from 'node:assert/strict'
import path from 'node:path'
import fs from 'node:fs'
import vm from 'node:vm'
import { SUBJECT_TREE_LIVE_FILTERS_BASE as BASE, SUBJECT_TREE_LIVE_FILTERS_TASK as TASK, SUBJECT_TREE_LIVE_FILTERS_PROJECT as PROJECT, SUBJECT_TREE_LIVE_FILTERS_REVISION as REVISION,
  FOCUSED_FILTERING_BASE, FOCUSED_FILTERING_TASK, evidenceProfile, evidenceOwnerFields, assertEvidenceIdentity, assertEvidenceWrite, isolatedRecordOutputs, prepareEvidenceScratch, createEvidenceOperations, checkQualification } from './evidence-profile.mjs'
import { SUBJECT_TREE_LIVE_FILTERS_CONTRACT, HARNESS_CONFIGURATION, assertObservationContentScopeUrl, observationPageReceipt, assertObservationPageReceipt, classifyLiveObservationHydration } from './contracts.mjs'
import { generateFixture, fixtureHash, queryObservations, queryObservationCounts } from './fixtures.mjs'
import { createTracker, fixtureResponder, observationResetProof, requiredLiveInputDoubleFrame } from './harness.mjs'

const frontend = path.resolve('fixture-root/hmem-server/frontend'), temporary = path.resolve('outside-temporary')
const plan = () => evidenceProfile(BASE, frontend, temporary)
const head = '1'.repeat(40)
const identity = profile => ({ taskId: TASK, projectId: PROJECT, evidenceBaseCommit: BASE, measurementRevision: REVISION,
  sourceProvenance: { reviewBaseCommit: BASE, headCommit: head }, inputQualification: { implementationCommit: head } })

test('Subject Tree qualification has its exact isolated owner, five bounded artifacts and strict source/trace retirement', () => {
  const profile = plan()
  assert.deepEqual([profile.task.taskId, profile.task.projectId, profile.revision], [TASK, PROJECT, REVISION])
  assert.deepEqual(evidenceOwnerFields(profile), {projectId: PROJECT})
  assert.deepEqual([profile.scaling, profile.rendering, profile.planningCounts, profile.dueRemoval, profile.loadingEfficiency, profile.focusedFiltering], Array(6).fill(false))
  assert.equal(profile.subjectTreeLiveFilters, true); assert.equal(profile.strictSourceIdentity, true)
  assert.equal(profile.isolatedEvidence, true); assert.equal(profile.qualifiedRetirement, true)
  assert.equal(profile.root, path.join(temporary, 'hmem-subject-tree-live-filters-' + TASK))
  assert.equal(profile.trace, path.join(profile.root, 'temporary/large-subject-tree-live-filters-trace.zip'))
  assert.equal(new Set(Object.values(profile.files)).size, 5)
  for (const file of Object.values(profile.files)) assert.equal(path.dirname(file), profile.root)
  assert.match(profile.retention, /closure plus 30 days, then dispose/); assert.match(profile.retention, /64 MiB/)
  assert.equal(evidenceProfile(FOCUSED_FILTERING_BASE, frontend, temporary).task.taskId, FOCUSED_FILTERING_TASK)
  assert.throws(() => evidenceProfile(BASE, frontend, frontend), /outside/)
  assert.throws(() => evidenceProfile('f'.repeat(40), frontend, temporary), /Unmapped/)
})

test('new profile rejects foreign owners, scenario switches, source drift, redirected outputs and oversized evidence', () => {
  const profile = plan(), record = identity(profile), baseline = path.join(frontend, 'perf/baseline.v1.json')
  assert.doesNotThrow(() => assertEvidenceIdentity(profile, record))
  for (const key of ['taskId', 'projectId', 'evidenceBaseCommit', 'measurementRevision']) assert.throws(() => assertEvidenceIdentity(profile, {...record, [key]: 'foreign'}), /identity mismatch/)
  assert.throws(() => assertEvidenceIdentity(profile, {...record, inputQualification: {implementationCommit: '2'.repeat(40)}}), /identity mismatch/)
  for (const altered of [{...profile, subjectTreeLiveFilters: false}, {...profile, focusedFiltering: true}, {...profile, strictSourceIdentity: false}, {...profile, root: path.join(temporary, 'foreign')}, {...profile, files: {...profile.files, after: baseline}}]) assert.throws(() => assertEvidenceIdentity(altered, record), /mismatch/)
  assert.throws(() => isolatedRecordOutputs(profile, baseline, baseline, 'legacy', 'legacy-trace'), /task-owned/)
  assert.throws(() => assertEvidenceWrite(profile, baseline, '{}'), /Unowned/)
  assert.throws(() => assertEvidenceWrite(profile, profile.files.after, Buffer.alloc(64 * 1024 * 1024 + 1)), /64 MiB/)
  const entries = new Map([[temporary, {ino: 1}], [profile.root, {ino: 2}]])
  let writes = 0
  const io = {existsSync: file => entries.has(file), realpathSync: file => file,
    lstatSync: file => ({dev: 1, ino: entries.get(file).ino, isSymbolicLink: () => false, isDirectory: () => true, isFile: () => false}),
    readdirSync: () => [], writeFileSync() {writes++}}
  const operations = createEvidenceOperations(profile, temporary, io)
  assert.throws(() => operations.write(profile.files.after, '{}'), /not been admitted/)
  prepareEvidenceScratch(profile, temporary, io); entries.get(profile.root).ino++
  assert.throws(() => operations.write(profile.files.after, '{}'), /lifetime changed.*no fallback/)
  assert.equal(writes, 0)
  const input = {recordEvaluation: {passed: true}, checkEvaluation: {passed: true}, retirement: {passed: true}, commands: ['npm run perf:self-check', 'node perf/harness.mjs record', 'node perf/harness.mjs check'].map(command => ({command, exitCode: 0}))}
  assert.equal(checkQualification(input).passed, true)
  assert.equal(checkQualification({...input, retirement: {passed: false}}).passed, false)
  assert.equal(checkQualification({...input, checkEvaluation: {passed: false}}).passed, false)
})

function scopedFixture() {
  const fixture = generateFixture('small'), row = fixture.observations[0]
  fixture.observations = [
    {...row, id: 'subject-only', content: 'unrelated evidence', subject: 'src/auth/subjectonlytoken', subjects: [{subject_kind: 'file', subject: 'src/auth/subjectonlytoken'}]},
    {...row, id: 'content-only', content: 'src auth contenttoken evidence', subject: 'docs/main.md', subjects: [{subject_kind: 'file', subject: 'docs/main.md'}]}
  ]
  return fixture
}

test('explicit content list/count search excludes subject lexemes while omitted and combined scope remain compatible', () => {
  const fixture = scopedFixture(), workspace_id = fixture.workspace.id
  const ids = scope => queryObservations(fixture, {query: 'subjectonlytoken', queryScope: scope}).items.map(row => row.id)
  assert.deepEqual(ids(null), ['subject-only']); assert.deepEqual(ids('combined'), ['subject-only']); assert.deepEqual(ids('content'), [])
  assert.deepEqual(queryObservations(fixture, {query: 'src auth', queryScope: 'content'}).items.map(row => row.id), ['content-only'])
  assert.deepEqual(queryObservations(fixture, {query: 'src auth'}).items.map(row => row.id).sort(), ['content-only', 'subject-only'])
  assert.equal(queryObservationCounts(fixture, {workspace_id, query: 'subjectonlytoken'}).match_count, 1)
  assert.equal(queryObservationCounts(fixture, {workspace_id, query: 'subjectonlytoken', query_scope: 'content'}).match_count, 0)
  for (const scope of ['', 'unknown', 'CONTENT']) assert.throws(() => queryObservations(fixture, {queryScope: scope}), /Unsupported/)
  for (const size of ['small', 'large']) assert.equal(fixtureHash(generateFixture(size)), {small: '0c425c77e5069b8d4cf26dbdebcf94037a22ee977e61d63ab3cceb1a7f090bc4', large: 'b98d53a1a1ca28641dc90cbb598f3026949edf702f0ae4696614520b5140dd63'}[size])
})

async function send(respond, suffix, method = 'GET', body = null) {
  let response
  const request = {url: () => 'http://perf.local/api/v1/' + suffix, method: () => method, postDataJSON: () => body}
  await respond({request: () => request, fulfill: async value => {response = value}})
  return JSON.parse(response.body)
}

test('actual list and count response bytes use explicit URL scope; strict profile rejects omission, duplicates and extras', async () => {
  const fixture = scopedFixture(), tracker = createTracker(fixture), respond = fixtureResponder(fixture, tracker)
  const list = 'observations?workspace_id=' + fixture.workspace.id + '&limit=200&offset=0&query=subjectonlytoken'
  assert.deepEqual((await send(respond, list)).items.map(row => row.id), ['subject-only'])
  assert.deepEqual((await send(respond, list + '&query_scope=content')).items, [])
  const body = {workspace_id: fixture.workspace.id, query: 'subjectonlytoken'}
  assert.equal((await send(respond, 'observations/count?query_scope=content', 'POST', body)).match_count, 0)
  assert.equal((await send(respond, 'observations/count', 'POST', {...body, query_scope: 'content'})).match_count, 1, 'body cannot substitute for URL scope')
  const strict = fixtureResponder(fixture, createTracker(fixture), {contentOnly: true})
  for (const suffix of [list, list + '&query_scope=combined', list + '&query_scope=content&query_scope=content', list + '&query_scope=content&extra=1', 'observations/count', 'observations/count?query_scope=combined', 'observations/count?query_scope=content&query=x']) {
    await assert.rejects(() => send(strict, suffix, suffix.includes('/count') ? 'POST' : 'GET', body), /scope.*mismatch|Duplicate/)
  }
  assert.equal(tracker.active, 0)
  for (const receipt of tracker.requests) assert.equal(receipt.fulfilled, true)
})

test('content page receipts bind exact singleton URL parameters and retain the omitted-scope legacy contract', () => {
  const fixture = generateFixture('small'), value = queryObservations(fixture, {limit: 200}), plain = 'http://perf.local/api/v1/observations?workspace_id=' + fixture.workspace.id + '&limit=200&offset=0'
  const expected = {workspaceId: fixture.workspace.id, offset: 0, expectedIds: value.items.map(row => row.id), hasMore: false}
  const receipt = suffix => {const result = observationPageReceipt(plain + suffix, 'GET', value); result.response.fulfilled = true; return result}
  assert.doesNotThrow(() => assertObservationPageReceipt(receipt(''), expected))
  assert.doesNotThrow(() => assertObservationPageReceipt(receipt('&query_scope=content'), {...expected, queryScope: 'content'}))
  assert.throws(() => assertObservationPageReceipt(receipt(''), {...expected, queryScope: 'content'}), /mismatch/)
  assert.throws(() => assertObservationPageReceipt(receipt('&query_scope=content'), expected), /mismatch/)
  assert.throws(() => assertObservationPageReceipt(receipt('&query_scope='), expected), /mismatch/)
  for (const suffix of ['&query_scope=combined', '&query_scope=content&extra=1', '&query_scope=content&offset=0']) assert.throws(() => assertObservationPageReceipt(receipt(suffix), {...expected, queryScope: 'content'}), /mismatch/)
  assert.doesNotThrow(() => assertObservationContentScopeUrl('http://perf.local/api/v1/observations/count?query_scope=content'))
})

test('live cache proof requires four parameters only in the new explicit-content context; historical three remain strict', async () => {
  const fixture = generateFixture('large'), tracker = createTracker(fixture), respond = fixtureResponder(fixture, tracker, {contentOnly: true})
  await send(respond, 'change-stream/resync', 'POST', {workspace_id: fixture.workspace.id, snapshot_profile: 'workspace_shell_v1'})
  const list = 'observations?workspace_id=' + fixture.workspace.id + '&limit=200&offset=0&query_scope=content'
  await send(respond, list)
  const reset = observationResetProof(tracker, fixture, tracker.requests.at(-1).observationPage, 200)
  reset.recipeCompleted = true
  const liveStartIndex = tracker.requests.length
  await send(respond, list)
  const requests = tracker.requests.slice(liveStartIndex), bytes = requests[0].bytes
  const input = {runId: tracker.paintRunId, workspaceId: fixture.workspace.id, total: 2000, expectedPrefix: queryObservations(fixture, {limit: 200}).items.map(row => row.id), reset, intervening: [], requests, liveStartIndex,
    delta: {count: 1, bytes, routes: {'observations:list': 1}, routeBytes: {'observations:list': bytes}}, active: 0, unknownRoutes: [], snapshotGeneration: tracker.model.snapshotGeneration, queryScope: 'content'}
  assert.equal(classifyLiveObservationHydration(input).passed, true)
  assert.equal(classifyLiveObservationHydration({...input, queryScope: null}).passed, false)
  for (const suffix of ['', '&query_scope=combined', '&query_scope=content&extra=x', '&query_scope=content&query_scope=content']) {
    const altered = structuredClone(input)
    altered.requests[0].url = altered.requests[0].url.replace('&query_scope=content', suffix)
    assert.equal(classifyLiveObservationHydration(altered).passed, false)
  }
})

test('live input records dispatch through exactly two paints independently of deferred debounce settlement', async () => {
  const events = []; let now = 0
  class Input {focus() {events.push('focus')} dispatchEvent(event) {events.push(event.type + ':' + this.value)}}
  const element = new Input()
  const page = {evaluate: (action, arg) => vm.runInNewContext('(' + action.toString() + ')(arg)', {arg, HTMLInputElement: Input, document: {querySelector: () => element}, Event: class {constructor(type) {this.type = type}}, performance: {now: () => now}, requestAnimationFrame: callback => {events.push('paint'); now += 10; callback()}})}
  for (const value of ['evidence', '']) {
    events.length = 0
    const elapsed = await requiredLiveInputDoubleFrame(page, '#observation-query', value)
    assert.deepEqual(events, ['focus', 'input:' + value, 'paint', 'paint']); assert.equal(elapsed, 20)
  }
  const budget = JSON.parse(fs.readFileSync('perf/budgets.v1.json', 'utf8'))
  assert.equal(budget.large.localInteraction.maxP95Ms, 100)
  assert.equal(HARNESS_CONFIGURATION.warmups, 2); assert.equal(HARNESS_CONFIGURATION.samples, 5)
  assert.match(SUBJECT_TREE_LIVE_FILTERS_CONTRACT.settledPhase, /without a new timing threshold/)
})
