import test from 'node:test'
import assert from 'node:assert/strict'
import { generateFixture, fixtureHash, navigationBatchDescriptors, navigationBatchResponse, navigationBranchResponse, queryObservations } from './fixtures.mjs'
import { assertNavigationBatchOutcomes, navigationBatchTransportProof, perfApiRouteKey, hashJson, liveHydrationAccepted, classifyLiveObservationHydration, LIVE_OBSERVATION_REFRESH_CONTRACT } from './contracts.mjs'
import { createTracker, fixtureResponder, navigationPageReceipts, assertLinkedNavigationPage, observationResetProof } from './harness.mjs'

const fixture = () => generateFixture('small')
const url = f => `http://perf.local/api/v1/workspaces/${f.workspace.id}/navigation/batch`
const body = (f, count = 4) => ({ requests: f.projects.slice(0, count).map((project, i) => ({ request_id: String(i + 1), parent: { kind: 'project', parent_id: project.id } })) })

test('navigation batch pages preserve GET filters, independent cursors, demand defaults and fixture bytes', () => {
  const f = fixture(), before = fixtureHash(f)
  for (const filters of [undefined, {}, { project_statuses: [], task_statuses: [] },
    { project_statuses: ['active','paused'], task_statuses: ['todo','blocked'], show_empty_projects: true },
    { query: 'Deterministic', show_only: 'projects', show_empty_projects: false, priority_mode: 'above', priority_value: 1 },
    { query: 'long description needle ' + 'x'.repeat(2048), task_statuses: ['done'] }]) {
    const request = { ...body(f), ...(filters ? { filters } : {}) }
    request.requests[1] = { ...request.requests[1], include_projects: false, task_offset: 50 }
    request.requests[2] = { ...request.requests[2], include_tasks: false, project_offset: 50 }
    request.requests[3] = { ...request.requests[3], include_projects: false, include_tasks: false }
    const descriptors = navigationBatchDescriptors(request, url(f)), result = navigationBatchResponse(f, request)
    assert.equal(result.outcomes.length, 4)
    for (const [index, descriptor] of descriptors.entries()) {
      assert.equal(descriptor.requestId.length, 1)
      assert.deepEqual(result.outcomes[index].response, navigationBranchResponse(f, descriptor.options))
      assert.equal(new URL(descriptor.logicalUrl).pathname, `/api/v1/workspaces/${f.workspace.id}/navigation`)
    }
    assertNavigationBatchOutcomes(f.workspace.id, request.requests, result)
  }
  assert.equal(fixtureHash(f), before)
})

test('navigation batch rejects invalid whole identity/filter/page sets before selection and keeps missing owners local', () => {
  const f = fixture(), valid = body(f, 2)
  const invalid = [ {}, { requests: [] }, body(f, 4), structuredClone(valid), structuredClone(valid), structuredClone(valid), structuredClone(valid), structuredClone(valid) ]
  invalid[2].requests.push({ ...invalid[2].requests[0], request_id: '5' })
  invalid[3].requests[1].request_id = invalid[3].requests[0].request_id
  invalid[4].requests[1].parent = invalid[4].requests[0].parent
  invalid[5].requests[0].parent = { kind: 'workspace_root' }
  invalid[6].requests[0].include_tasks = 'false'
  invalid[7].requests[0].task_offset = -1
  for (const request of [...invalid, { ...valid, filters: { task_statuses: ['unknown'] } }, { ...valid, filters: { query: ' ' } }, { ...valid, filters: { priority_mode: 'above' } }]) assert.throws(() => navigationBatchDescriptors(request, url(f)), /Invalid navigation batch/)
  const missing = structuredClone(valid); missing.requests[0].parent.parent_id = 'ffffffff-ffff-4fff-8fff-ffffffffffff'
  const result = navigationBatchResponse(f, missing)
  assert.deepEqual(result.outcomes.map(item => item.status), [404,200])
  assertNavigationBatchOutcomes(f.workspace.id, missing.requests, result)
  assert.equal(perfApiRouteKey(url(f), 'POST'), 'navigation:batch')
  assert.equal(perfApiRouteKey(url(f), 'GET'), null)
})

test('one actual POST accounts exact whole bytes once while four logical admissions survive retirement until fulfillment', async () => {
  const f = fixture(), tracker = createTracker(f), requestBody = body(f), request = { url: () => url(f), method: () => 'POST', postDataJSON: () => requestBody }
  let release, entered, responseBody
  const held = new Promise(resolve => { release = resolve }), arrived = new Promise(resolve => { entered = resolve })
  const pending = fixtureResponder(f, tracker)({ request: () => request, fulfill: async value => { responseBody = value.body; entered(); await held } })
  await arrived
  assert.equal(tracker.requests.length, 1); assert.equal(tracker.counts['navigation:batch'], 1)
  assert.equal(tracker.activeBranches, 4); assert.equal(tracker.maxExpandedBranches, 4)
  tracker.navigationProof.beginSession()
  assert.equal(tracker.activeBranches, 4)
  assert.ok(navigationPageReceipts(tracker).every(page => !page.done && page.counted === false && page.bytes === 0))
  release(); await pending
  assert.equal(tracker.activeBranches, 0); assert.equal(tracker.active, 0)
  assert.equal(tracker.requests[0].bytes, Buffer.byteLength(responseBody))
  assert.equal(tracker.bytes['navigation:batch'], Buffer.byteLength(responseBody))
  assert.equal(tracker.bytes['navigation:branch'], undefined)
  assert.equal(navigationPageReceipts(tracker).length, 4)
  for (const page of navigationPageReceipts(tracker)) assertLinkedNavigationPage(tracker, page)
  const proof = navigationBatchTransportProof(tracker.requests[0], f.workspace.id)
  assert.equal(proof.ownerCount, 4)
  assert.equal(hashJson(proof.batchResponse), hashJson(JSON.parse(responseBody)))
})

test('owner failures retain successful siblings and malformed complete outcomes never gain batch authority', async () => {
  const f = fixture(), tracker = createTracker(f), requestBody = body(f, 2), request = { url: () => url(f), method: () => 'POST', postDataJSON: () => requestBody }
  await fixtureResponder(f, tracker, { navigationResponseStatus: logical => logical.searchParams.get('parent_id') === f.projects[0].id ? 500 : 200 })({ request: () => request, fulfill: async () => {} })
  assert.deepEqual(navigationPageReceipts(tracker).map(page => page.navigationStatus), [500,200])
  assert.equal(tracker.requests.length, 1); assert.equal(tracker.activeBranches, 0)
  const good = navigationBatchResponse(f, requestBody)
  const variants = [structuredClone(good), structuredClone(good), structuredClone(good), structuredClone(good), structuredClone(good), structuredClone(good)]
  variants[0].outcomes.pop(); variants[1].outcomes.reverse(); variants[2].outcomes[1].request_id = variants[2].outcomes[0].request_id
  variants[3].outcomes[0].response.workspace_id = 'foreign'; variants[4].outcomes[0].error = 'mixed'; variants[5].outcomes[0].parent.kind = 'task'
  for (const value of variants) assert.throws(() => assertNavigationBatchOutcomes(f.workspace.id, requestBody.requests, value), /Navigation batch|navigation batch/)
  const skipped = structuredClone(requestBody); skipped.requests[0].include_tasks = false
  const malformed = navigationBatchResponse(f, skipped); malformed.outcomes[0].response.tasks.has_more = true
  assert.throws(() => assertNavigationBatchOutcomes(f.workspace.id, skipped.requests, malformed), /stream/)
})

test('hydration approval requires physical batch lineage and exact bytes while rejecting forged logical links', async () => {
  const f = fixture(), tracker = createTracker(f), requestBody = body(f, 2)
  await fixtureResponder(f, tracker)({ request: () => ({ url: () => url(f), method: () => 'POST', postDataJSON: () => requestBody }), fulfill: async () => {} })
  const transport = navigationBatchTransportProof(tracker.requests[0], f.workspace.id)
  const proof = { runId: tracker.paintRunId, workspaceId: f.workspace.id, accountedRequests: 1, accountedBytes: transport.bytes,
    liveStartIndex: 0, liveEndIndex: 1, requestsSucceeded: true, enumerationRequests: 0, observationEntityRequests: 0 }
  const value = { revision: LIVE_OBSERVATION_REFRESH_CONTRACT.revision, passed: true, wholeWorkspaceReload: false,
    proofBasis: LIVE_OBSERVATION_REFRESH_CONTRACT.retainedContextProof, reason: 'No enumeration', classification: 'no-enumeration',
    legacyWholeWorkspaceReload: false, proof, navigationBatchCount: 1, navigationBatches: [transport] }
  assert.equal(liveHydrationAccepted(value), true)
  for (const mutate of [v => { v.navigationBatchCount = 2 }, v => { v.navigationBatches[0].bytes++ },
    v => { v.navigationBatches[0].runId = 'foreign' }, v => { v.navigationBatches[0].requestIndex = 1 },
    v => { v.navigationBatches[0].batchResponse.outcomes.pop() }, v => { v.navigationBatches[0].method = 'GET' }]) {
    const invalid = structuredClone(value); mutate(invalid); assert.equal(liveHydrationAccepted(invalid), false)
  }
  const page = navigationPageReceipts(tracker)[0]
  for (const mutation of [{ transportRequestId: 999 }, { ownerIndex: 1 }, { counted: true }, { bytes: 1 }, { method: 'GET' }]) {
    assert.throws(() => assertLinkedNavigationPage(tracker, { ...page, ...mutation }), /Navigation|navigation|logical|Logical/)
  }
})

test('actual intervening POST batch needs physical proof while legacy intervening routes remain GET-only', async () => {
  const f = generateFixture('large'), tracker = createTracker(f), respond = fixtureResponder(f, tracker)
  const send = async (path, method = 'GET', requestBody = null) => respond({ request: () => ({ url: () => 'http://perf.local/api/v1/' + path, method: () => method, postDataJSON: () => requestBody }), fulfill: async () => {} })
  await send('change-stream/resync', 'POST', { workspace_id: f.workspace.id, snapshot_profile: 'workspace_shell_v1' })
  const list = 'observations?workspace_id=' + f.workspace.id + '&limit=200&offset=0'
  await send(list)
  const reset = observationResetProof(tracker, f, tracker.requests.at(-1).observationPage, 200)
  await send('workspaces/' + f.workspace.id + '/navigation/batch', 'POST', body(f, 2))
  reset.recipeCompleted = true
  const liveStartIndex = tracker.requests.length
  await send(list)
  const requests = tracker.requests.slice(liveStartIndex), receipt = requests[0]
  const input = { runId: tracker.paintRunId, workspaceId: f.workspace.id, total: f.observations.length,
    expectedPrefix: queryObservations(f, { offset: 0, limit: 200 }).items.map(item => item.id), reset,
    intervening: tracker.requests.slice(reset.requestIndex + 1, liveStartIndex), requests, liveStartIndex,
    delta: { count: 1, bytes: receipt.bytes, routes: { 'observations:list': 1 }, routeBytes: { 'observations:list': receipt.bytes } },
    active: 0, unknownRoutes: [], snapshotGeneration: tracker.model.snapshotGeneration }
  assert.equal(liveHydrationAccepted(classifyLiveObservationHydration(input)), true)
  for (const mutate of [v => { v.intervening[0].method = 'GET' }, v => { v.intervening[0].batchBody.requests = [] },
    v => { v.intervening[0].batchResponse.outcomes.pop() }, v => { v.intervening[0].bytes++ },
    v => { v.intervening[0].workspaceId = 'foreign' }, v => { v.intervening[0].fulfilled = false },
    v => { const r = v.intervening[0]; r.key = 'navigation:branch'; r.url = r.url.replace('/batch', ''); r.method = 'POST' }]) {
    const invalid = structuredClone(input); mutate(invalid)
    assert.equal(liveHydrationAccepted(classifyLiveObservationHydration(invalid)), false)
  }
})
