import test from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import { createHash } from 'node:crypto'
import { classifyLiveObservationHydration, liveHydrationAccepted, aggregateLiveHydration, liveWholeWorkspaceReload, LIVE_OBSERVATION_REFRESH_CONTRACT, HARNESS_CONFIGURATION, hashJson } from './contracts.mjs'
import { generateFixture, queryObservations } from './fixtures.mjs'
import { createTracker, fixtureResponder, observationResetProof, liveObservationRefreshSource } from './harness.mjs'

function account(requests) {
  return requests.reduce((sum, request) => ({ count: sum.count + 1, bytes: sum.bytes + request.bytes,
    routes: { ...sum.routes, [request.key]: (sum.routes[request.key] || 0) + 1 },
    routeBytes: { ...sum.routeBytes, [request.key]: (sum.routeBytes[request.key] || 0) + request.bytes } }), { count: 0, bytes: 0, routes: {}, routeBytes: {} })
}

async function actualInput(scale = 'large') {
  const fixture = generateFixture(scale), tracker = createTracker(fixture), respond = fixtureResponder(fixture, tracker)
  const send = async (path, method = 'GET', body = null) => {
    const request = { url: () => 'http://perf.local/api/v1/' + path, method: () => method, postDataJSON: () => body }
    await respond({ request: () => request, fulfill: async () => {} })
  }
  await send('change-stream/resync', 'POST', { workspace_id: fixture.workspace.id, snapshot_profile: 'workspace_shell_v1' })
  const list = 'observations?workspace_id=' + fixture.workspace.id + '&limit=200&offset=0'
  await send(list)
  const page = tracker.requests.at(-1).observationPage
  const reset = observationResetProof(tracker, fixture, page, Math.min(200, fixture.observations.length))
  await send(`workspaces/${fixture.workspace.id}/timeline?limit=50`)
  await send(`workspaces/${fixture.workspace.id}/timeline/buckets?paged=true&since=2026-08-01T00%3A00%3A00Z&until=2026-08-31T00%3A00%3A00Z&bucket=week`)
  reset.recipeCompleted = true
  const liveStartIndex = tracker.requests.length
  const updatedObservationIds = [...new Set(fixture.liveFrames.filter(frame => frame.event.entity.type === 'observation').map(frame => frame.event.entity.id))]
  // Task53 route order with production body fields and deterministic fixture IDs;
  // account the bytes actually emitted by this replica without claiming the prior run's bytes.
  await send(`workspaces/${fixture.workspace.id}/planning-counts`)
  if (scale === 'large') {
    await send('observations/count', 'POST', { workspace_id: fixture.workspace.id })
    await send(list)
    for (let index = 0; index < 3; index++) await send(`workspaces/${fixture.workspace.id}/navigation/summaries`, 'POST', { project_ids: [fixture.projects[index].id], task_ids: [] })
    await send(`workspaces/${fixture.workspace.id}/navigation?parent_kind=workspace_root&project_limit=50&project_offset=0&task_limit=50&task_offset=0&show_empty_projects=true&priority_mode=any`)
  } else {
    for (const id of updatedObservationIds) await send('observations/' + id)
    await send(`workspaces/${fixture.workspace.id}/navigation/summaries`, 'POST', { project_ids: [fixture.projects[0].id], task_ids: [] })
  }
  const requests = tracker.requests.slice(liveStartIndex)
  return { fixture, tracker, input: { runId: tracker.paintRunId, workspaceId: fixture.workspace.id, total: fixture.observations.length,
    expectedPrefix: queryObservations(fixture, { offset: 0, limit: 200 }).items.map(item => item.id), reset,
    updatedObservationIds, expectedObservationEntities: Object.fromEntries(fixture.observations.filter(value => updatedObservationIds.includes(value.id)).map(value => [value.id, { hash: hashJson(value), bytes: Buffer.byteLength(JSON.stringify(value)) }])),
    intervening: tracker.requests.slice(reset.requestIndex + 1, liveStartIndex), requests, liveStartIndex,
    delta: account(requests), active: tracker.active, unknownRoutes: tracker.unhandledApiRoutes, snapshotGeneration: tracker.model.snapshotGeneration } }
}

test('actual same-run reset and seven successful fully accounted live receipts prove only a bounded retained prefix refresh', async () => {
  const { input } = await actualInput(), result = classifyLiveObservationHydration(input)
  assert.equal(result.classification, 'bounded-cache-refresh'); assert.equal(liveHydrationAccepted(result), true)
  assert.equal(result.wholeWorkspaceReload, false); assert.equal(result.legacyWholeWorkspaceReload, true)
  assert.equal(liveWholeWorkspaceReload(input.delta.routes), true)
  assert.equal(result.proof.offscreenModelObserved, false); assert.match(result.proofBasis, /Inferred/)
  assert.equal(result.proof.backingTotal, 2000); assert.equal(result.proof.live.count, 200)
  assert.equal(result.proof.reset.prefixHash, result.proof.live.prefixHash)
  assert.equal(result.proof.accountedRequests, 7); assert.equal(result.proof.accountedBytes, input.delta.bytes)
  assert.deepEqual(input.delta.routes, { 'workspaces:planning-counts': 1, 'observations:count': 1, 'observations:list': 1, 'navigation:summaries': 3, 'navigation:branch': 1 })
  assert.equal(JSON.stringify(result).length < 4096, true, 'supporting classification receipt is bounded')
  const smallInput = (await actualInput('small')).input, small = classifyLiveObservationHydration(smallInput)
  assert.equal(small.classification, 'no-enumeration'); assert.equal(liveHydrationAccepted(small), true)
  assert.equal(smallInput.requests.length, 18); assert.equal(small.proof.observationEntityRequests, 16)
  assert.equal(small.proof.accountedBytes, smallInput.delta.bytes)
  for (const entity of smallInput.requests.filter(request => request.key.startsWith('observations:entity:'))) {
    assert.equal(entity.workspaceId, smallInput.workspaceId, 'existing reply DTO supplies entity workspace binding')
    assert.equal(entity.observationEntity.workspaceId, smallInput.workspaceId)
    assert.equal(entity.observationEntity.dtoHash, smallInput.expectedObservationEntities[entity.observationEntity.id].hash)
  }
  assert.equal(aggregateLiveHydration(Array(5).fill(small)).passed, true)
})

test('missing/stale/foreign/unexecuted reset proof and intervening context changes cannot be accepted', async () => {
  const { input } = await actualInput()
  for (const mutate of [
    value => { value.reset = null }, value => { value.reset.runId = 'foreign' }, value => { value.reset.workspaceId = 'foreign' },
    value => { value.reset.snapshotGeneration++ }, value => { value.snapshotGeneration = undefined },
    value => { value.reset.membership.executed = false }, value => { value.reset.membership.expected = 400 },
    value => { value.reset.recipeCompleted = false }, value => { value.reset.recipe = 'changed' },
    value => { value.reset.requestIndex++ }, value => { value.reset.requestId++ },
    value => { value.reset.receipt.fulfilled = false }, value => { value.reset.receipt.observationPage.response.fulfilled = false },
    value => { value.reset.receipt.observationPage.response.ids.reverse() },
    value => { value.intervening.pop() }, value => { value.intervening[0].requestIndex++ },
    value => { value.intervening[0].runId = 'foreign' }, value => { value.intervening[0].workspaceId = 'foreign' },
    value => { value.intervening[0].key = 'session'; value.intervening[0].url = 'http://perf.local/api/v1/session' },
    value => { value.intervening[0].key = 'observations:list'; value.intervening[0].url = value.reset.receipt.url },
    value => { value.intervening[0].key = 'change-stream:resync'; value.intervening[0].method = 'POST'; value.intervening[0].url = 'http://perf.local/api/v1/change-stream/resync' }
  ]) {
    const value = structuredClone(input); mutate(value)
    assert.equal(liveHydrationAccepted(classifyLiveObservationHydration(value)), false)
  }
})

test('filtered foreign mismatched unfulfilled oversized reordered duplicate and non-first-page live DTOs fail closed', async () => {
  const { input } = await actualInput()
  for (const mutate of [
    request => { request.url += '&query=anything' }, request => { request.url += '&query=' },
    request => { request.url += '&subject_kind=file' }, request => { request.url += '&current_git_sha=' + 'a'.repeat(40) },
    request => { request.url += '&workspace_id=foreign' }, request => { request.url = request.url.replace(input.workspaceId, 'foreign') },
    request => { request.workspaceId = 'foreign' }, request => { request.runId = 'foreign' },
    request => { request.url = request.url.replace('limit=200', 'limit=2000') }, request => { request.url = request.url.replace('offset=0', 'offset=200') },
    request => { request.observationPage.response.ids.pop() }, request => { request.observationPage.response.ids.push('extra') },
    request => { request.observationPage.response.ids[0] = request.observationPage.response.ids[1] },
    request => { request.observationPage.response.ids.reverse() }, request => { request.observationPage.response.hasMore = false },
    request => { request.observationPage.response.fulfilled = false }, request => { request.fulfilled = false },
    request => { request.done = false }, request => { request.status = 403 },
    request => { request.requestIndex++ }, request => { request.requestId++ }, request => { request.url = 'malformed' }
  ]) {
    const value = structuredClone(input); mutate(value.requests.find(request => request.key === 'observations:list'))
    assert.equal(liveHydrationAccepted(classifyLiveObservationHydration(value)), false)
  }
  for (const total of [60, 200, null, Infinity]) assert.equal(liveHydrationAccepted(classifyLiveObservationHydration({ ...input, total })), false)
})

test('full enumeration resync project/task lists extra Observation pages and restarts never use the bounded exception', async () => {
  const { input } = await actualInput()
  for (const [key, method, path] of [['change-stream:resync', 'POST', 'change-stream/resync'], ['projects:list', 'GET', 'projects'], ['tasks:list', 'GET', 'tasks'], ['workspaces:list', 'GET', 'workspaces'], ['observations:list', 'GET', `observations?workspace_id=${input.workspaceId}&limit=200&offset=200`]]) {
    const value = structuredClone(input), original = value.requests[2]
    value.requests.push({ ...original, key, method, url: 'http://perf.local/api/v1/' + path, requestIndex: value.liveStartIndex + value.requests.length, requestId: value.liveStartIndex + value.requests.length + 1 })
    value.delta = account(value.requests)
    assert.equal(classifyLiveObservationHydration(value).classification, 'whole-hydration')
  }
  const restart = structuredClone(input), original = restart.requests[2]
  restart.requests.push({ ...original, requestIndex: restart.liveStartIndex + restart.requests.length, requestId: restart.liveStartIndex + restart.requests.length + 1 })
  restart.delta = account(restart.requests)
  assert.equal(classifyLiveObservationHydration(restart).classification, 'whole-hydration')
})

test('capture/accounting faults and unknown or session/context traffic cannot be waived', async () => {
  const { input } = await actualInput()
  for (const mutate of [
    value => { value.active = 1 }, value => { value.unknownRoutes.push('unknown') },
    value => { value.delta.count-- }, value => { value.delta.bytes-- },
    value => { value.delta.routes['observations:list'] = 0 }, value => { value.delta.routeBytes['observations:list']-- },
    value => { value.requests[0].bytes = NaN }, value => { value.requests[0].bytes = Number.MAX_SAFE_INTEGER + 1 },
    value => { value.requests[0].done = false }, value => { value.requests[0].key = 'unknown' },
    value => { value.requests[0].key = 'session'; value.requests[0].url = 'http://perf.local/api/v1/session'; value.delta = account(value.requests) },
    value => { value.requests[0].workspaceId = 'foreign' }, value => { value.requests.splice(0, 1); value.delta = account(value.requests) }
  ]) {
    const value = structuredClone(input); mutate(value)
    assert.equal(liveHydrationAccepted(classifyLiveObservationHydration(value)), false)
  }
  for (const value of [null, {}, { revision: LIVE_OBSERVATION_REFRESH_CONTRACT.revision, classification: 'unproven', passed: true, wholeWorkspaceReload: false }]) assert.equal(liveHydrationAccepted(value), false)
  const valid = classifyLiveObservationHydration(input)
  assert.equal(liveHydrationAccepted({ ...valid, proof: null }), false)
})

test('approval and actual aggregation reject sparse proof, both missing digests and incomplete lineage/status/accounting', async () => {
  const valid = classifyLiveObservationHydration((await actualInput()).input)
  assert.equal(aggregateLiveHydration(Array(5).fill(valid)).passed, true)
  assert.equal(aggregateLiveHydration(Array(4).fill(valid)).passed, false)
  const sparse = { ...valid, proof: { offscreenModelObserved: false, inferredRetainedCount: 200, backingTotal: 2000,
    executedResetMembership: { executed: true, expected: 200 }, reset: { count: 200, hasMore: true, fulfilled: true }, live: { count: 200, hasMore: true, fulfilled: true } } }
  assert.equal(liveHydrationAccepted(sparse), false)
  for (const mutate of [
    value => { delete value.proof.reset.prefixHash; delete value.proof.live.prefixHash },
    value => { value.proof.reset.prefixHash = ''; value.proof.live.prefixHash = '' },
    value => { value.proof.reset.prefixHash = 'x'.repeat(64); value.proof.live.prefixHash = 'x'.repeat(64) },
    value => { value.proof.reset.prefixHash = 'a'.repeat(63); value.proof.live.prefixHash = 'a'.repeat(63) },
    value => { value.proof.live.prefixHash = 'f'.repeat(64) },
    value => { delete value.proof.runId }, value => { delete value.proof.workspaceId },
    value => { delete value.proof.live.status }, value => { value.proof.reset.status = 403 },
    value => { delete value.proof.reset.requestIndex }, value => { value.proof.live.requestId++ },
    value => { value.proof.live.runId = 'foreign' }, value => { value.proof.live.workspaceId = 'foreign' },
    value => { value.proof.live.url += '&query=filtered' }, value => { delete value.proof.live.firstId },
    value => { value.proof.live.lastId = value.proof.live.firstId },
    value => { delete value.proof.accountedRequests }, value => { value.proof.accountedRequests = 13 },
    value => { delete value.proof.accountedBytes }, value => { value.proof.accountedBytes = 0 },
    value => { value.proof.accountedBytes = NaN }, value => { value.proof.live.bytes = value.proof.accountedBytes + 1 },
    value => { delete value.proof.liveStartIndex }, value => { value.proof.liveEndIndex++ },
    value => { value.proof.interveningReceipts++ }, value => { delete value.proof.requestsSucceeded }
  ]) {
    const bad = structuredClone(valid); mutate(bad)
    assert.equal(liveHydrationAccepted(bad), false)
    assert.equal(aggregateLiveHydration([valid, valid, bad, valid, valid]).passed, false)
  }
  const small = classifyLiveObservationHydration((await actualInput('small')).input)
  assert.equal(liveHydrationAccepted(small), true)
  assert.equal(liveHydrationAccepted({ ...small, proof: null }), false)
  const small18 = structuredClone(small)
  small18.proof.accountedRequests = 18
  small18.proof.liveEndIndex = small18.proof.liveStartIndex + 18
  assert.equal(liveHydrationAccepted(small18), true, 'the retained 12-request budget applies to large; no new small cap')
  assert.equal(aggregateLiveHydration(Array(5).fill(small18)).passed, true)
})

test('source/integration anchors preserve raw legacy flag, executed reset ordering, all sample proof gates and unchanged live budgets/cuts', () => {
  for (const entry of liveObservationRefreshSource()) assert.equal(entry.sha256, createHash('sha256').update(fs.readFileSync(entry.path.replace('hmem-server/frontend/', ''))).digest('hex'))
  const source = fs.readFileSync('perf/harness.mjs', 'utf8')
  assert.match(source, /paintRunId: randomUUID\(\)/, 'runtime run ownership is an actual UUID')
  const reset = source.indexOf('const resetProof = observationResetProof(')
  assert.equal(reset > source.indexOf("'Observation filter reset')"), true)
  assert.equal(source.indexOf('resetProof.recipeCompleted = true') > reset, true)
  assert.match(source, /wholeWorkspaceReload: liveWholeWorkspaceReload\(liveDelta.routes\), hydration/)
  assert.match(source, /hydrationPassed: aggregateLiveHydration\(runs.map\(run => run.liveBatch.hydration\), evidencePlan.subjectTreeLiveFilters \? 'content' : null\).passed/)
  assert.match(source, /large.liveBatch.hydrationPassed === true/)
  assert.match(source, /small.liveBatch.hydrationPassed === true/)
  assert.match(source, /fixtureBytes: liveRequestDeltas.map\(delta => delta.bytes\)/)
  assert.match(source, /liveObservationRefreshContractHash: hashJson\(LIVE_OBSERVATION_REFRESH_CONTRACT\)/)
  assert.match(source, /liveTimingSummary\(\{ startedAt: liveStart, settledAt,/)
  assert.match(source, /await assertLiveStability\(page, tracker, 500\)/)
  const api = fs.readFileSync('src/Api.elm', 'utf8')
  assert.match(api, /\( "project_ids", E.list E.string projectIds \)/)
  assert.match(api, /\( "task_ids", E.list E.string taskIds \)/)
  const budget = JSON.parse(fs.readFileSync('perf/budgets.v1.json')).large.liveBatch
  assert.deepEqual(budget, { frameCount: 50, maxFollowUpRequests: 12, maxP95Ms: 500, allowWholeWorkspaceReload: false, maxDuplicateRequestsPerRepeatedTarget: 0 })
  assert.equal(HARNESS_CONFIGURATION.samples, 5); assert.equal(HARNESS_CONFIGURATION.warmups, 2)
})

test('small individual details require updated cached IDs, exact successful DTOs and never exempt a duplicate or full entity walk', async () => {
  const { input } = await actualInput('small')
  for (const mutate of [
    value => { value.requests[1].observationEntity.id = 'foreign' },
    value => { value.requests[1].observationEntity.workspaceId = 'foreign' },
    value => { delete value.requests[1].observationEntity },
    value => { value.requests[1].observationEntity.dtoHash = 'a'.repeat(64) },
    value => { value.requests[1].observationEntity.bytes++ },
    value => { value.requests[1].fulfilled = false }, value => { value.requests[1].status = 404 },
    value => { value.requests[1].url += '?query=any' },
    value => { value.updatedObservationIds = [] }, value => { value.expectedPrefix.shift() },
    value => { value.updatedObservationIds.push(value.updatedObservationIds[0]) },
    value => { value.reset.membership.executed = false }, value => { value.reset.receipt.observationPage.response.fulfilled = false },
    value => { value.intervening.pop() },
    value => { value.requests[2].key = value.requests[1].key; value.requests[2].url = value.requests[1].url; value.requests[2].observationEntity = value.requests[1].observationEntity; value.delta = account(value.requests) }
  ]) {
    const bad = structuredClone(input); mutate(bad)
    assert.equal(liveHydrationAccepted(classifyLiveObservationHydration(bad)), false)
  }
  const fullWalk = structuredClone(input)
  fullWalk.total = 16
  fullWalk.expectedPrefix = [...fullWalk.updatedObservationIds]
  assert.equal(classifyLiveObservationHydration(fullWalk).classification, 'whole-hydration')
  const valid = classifyLiveObservationHydration(input)
  for (const mutate of [
    value => { delete value.proof.observationEntityRequests }, value => { delete value.proof.entities },
    value => { value.proof.entities[0].expectedDtoHash = undefined; value.proof.entities[0].dtoHash = undefined },
    value => { value.proof.entities[0].workspaceId = 'foreign' }, value => { value.proof.updatedIds = [] },
    value => { value.proof.cacheIds.reverse() }, value => { value.proof.entities[0].status = 404 },
    value => { value.proof.entities[1].id = value.proof.entities[0].id },
    value => { value.proof.entities[0].requestIndex = value.proof.liveEndIndex },
    value => { value.proof.entities[0].url += '?offset=0' }
  ]) {
    const bad = structuredClone(valid); mutate(bad)
    assert.equal(liveHydrationAccepted(bad), false)
    assert.equal(aggregateLiveHydration([valid, valid, bad, valid, valid]).passed, false)
  }
})
