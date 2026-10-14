import test from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import { createHash } from 'node:crypto'
import { OBSERVATION_PAGINATION_CONTRACT, HARNESS_CONFIGURATION, observationPageReceipt, assertObservationPageReceipt, aggregateObservationLoadMore, observationPaginationMetrics } from './contracts.mjs'
import { generateFixture, queryObservations, queryObservationFacets, fixtureHash } from './fixtures.mjs'
import { observationPaginationSource, assertObservationPagination, fixtureResponder, createTracker } from './harness.mjs'

function receiptFor(fixture, offset = 0, query = null) {
  const url = new URL('http://perf.local/api/v1/observations')
  url.searchParams.set('workspace_id', fixture.workspace.id)
  url.searchParams.set('offset', String(offset)); url.searchParams.set('limit', '200')
  if (query) url.searchParams.set('query', query)
  const value = queryObservations(fixture, { query, offset, limit: 200 })
  const receipt = observationPageReceipt(url.href, 'GET', value)
  receipt.response.fulfilled = true
  return { receipt, value }
}

function samples(scale) {
  const fixture = generateFixture(scale)
  const receipt = receiptFor(fixture, scale === 'large' ? 200 : 0).receipt
  return Array.from({ length: 5 }, (_, index) => scale === 'small'
    ? { applicable: false, reason: 'terminal-first-page', buttonAbsent: true, hasMore: false, loaded: 60, receipt }
    : { applicable: true, exercised: true, loaded: 400, ms: 100 + index, count: 2, receipt })
}

test('actual production pagination is separately source-anchored at 200 without changing historical configuration, budgets or fixtures', () => {
  const proof = observationPaginationSource()
  assert.equal(proof.pageSize, 200)
  assert.equal(proof.path, 'hmem-server/frontend/src/Feature/Observation.elm')
  assert.equal(proof.sha256, createHash('sha256').update(fs.readFileSync('src/Feature/Observation.elm')).digest('hex'))
  assert.equal(OBSERVATION_PAGINATION_CONTRACT.pageSize, 200)
  assert.equal(OBSERVATION_PAGINATION_CONTRACT.legacyTimingComparability.comparable, false)
  assert.equal(OBSERVATION_PAGINATION_CONTRACT.legacyTimingComparability.pageSize, 50)
  assert.equal(HARNESS_CONFIGURATION.warmups, 2)
  assert.equal(HARNESS_CONFIGURATION.samples, 5)
  assert.deepEqual(JSON.parse(fs.readFileSync('perf/budgets.v1.json')).large.observationLoadMore, { maxP95Ms: 500, maxRequests: 2 })
  assert.equal(fixtureHash(generateFixture('small')), '0c425c77e5069b8d4cf26dbdebcf94037a22ee977e61d63ab3cceb1a7f090bc4')
  assert.equal(fixtureHash(generateFixture('large')), 'b98d53a1a1ca28641dc90cbb598f3026949edf702f0ae4696614520b5140dd63')
})

test('receipt proves exact small terminal 60, large 200-to-400, filter one and reset first page', () => {
  for (const scale of ['small', 'large']) {
    const fixture = generateFixture(scale)
    for (const [offset, query, count, hasMore] of [[0, null, scale === 'small' ? 60 : 200, scale === 'large'], [0, 'Observation 00001 evidence', 1, false], [0, null, scale === 'small' ? 60 : 200, scale === 'large'], ...(scale === 'large' ? [[200, null, 200, true]] : [])]) {
      const { receipt, value } = receiptFor(fixture, offset, query)
      assert.equal(value.items.length, count); assert.equal(value.has_more, hasMore)
      assertObservationPageReceipt(receipt, { workspaceId: fixture.workspace.id, offset, query, expectedIds: value.items.map(item => item.id), hasMore })
      const tracker = { requests: [{ key: 'observations:list', observationPage: receipt }] }
      assert.deepEqual(assertObservationPagination(tracker, { index: 0 }, fixture, offset, query, 'page'), receipt)
    }
    const { receipt, value } = receiptFor(fixture)
    for (const bad of [
      { ...receipt, request: { ...receipt.request, method: 'POST' } },
      { ...receipt, request: { ...receipt.request, workspace_id: 'foreign' } },
      { ...receipt, request: { ...receipt.request, offset: 50 } },
      { ...receipt, request: { ...receipt.request, limit: 50 } },
      { ...receipt, request: { ...receipt.request, current_git_sha: 'a'.repeat(40) } },
      { ...receipt, response: { ...receipt.response, fulfilled: false } },
      { ...receipt, response: { ...receipt.response, hasMore: !receipt.response.hasMore } },
      { ...receipt, response: { ...receipt.response, ids: [...receipt.response.ids].reverse() } }
    ]) assert.throws(() => assertObservationPageReceipt(bad, { workspaceId: fixture.workspace.id, offset: 0, expectedIds: value.items.map(item => item.id), hasMore: value.has_more }), /receipt mismatch/)
  }
})

test('initial page proof includes eager startup hydration while tab timing retains its action boundary', () => {
  const fixture = generateFixture('small'), { receipt } = receiptFor(fixture)
  const startupBefore = { index: 0 }
  const tracker = { requests: [
    { key: 'navigation:branch' },
    { key: 'observations:list', observationPage: receipt },
    { key: 'tasks:list' }
  ] }
  const tabBefore = { index: tracker.requests.length }
  assert.deepEqual(assertObservationPagination(tracker, startupBefore, fixture, 0, null, 'initial'), receipt)
  assert.throws(() => assertObservationPagination(tracker, tabBefore, fixture, 0, null, 'initial'), /observed 0/)
  for (const pages of [[], [receipt, receipt], [{ ...receipt, response: { ...receipt.response, fulfilled: false } }], [{ ...receipt, request: { ...receipt.request, offset: 200 } }]]) {
    const bad = { requests: pages.map(observationPage => ({ key: 'observations:list', observationPage })) }
    assert.throws(() => assertObservationPagination(bad, startupBefore, fixture, 0, null, 'initial'))
  }
  const source = fs.readFileSync('perf/harness.mjs', 'utf8')
  const run = source.slice(source.indexOf('async function measureRun('))
  const startupCut = run.indexOf('const observationStartupBefore = requestSnapshot(tracker)')
  assert.ok(startupCut >= 0 && startupCut < run.indexOf('await navigateToFirstUsefulViewport('))
  assert.match(run, /initialObservationPage = assertObservationPagination\(tracker, observationStartupBefore,/)
  assert.match(run, /observationsTabMs = \{ ms: obsTabMs, \.\.\.requestDelta\(tracker, observationsTabBefore\)/)
})

test('small aggregation is an exact terminal eligibility receipt with no invented timing or no-op count', () => {
  const summary = aggregateObservationLoadMore(samples('small'), 'small')
  assert.equal(summary.applicable, false); assert.equal(summary.terminalSamples, 5)
  assert.equal('rawMs' in summary, false); assert.equal('p95Ms' in summary, false); assert.equal('requestCounts' in summary, false)
  for (const patch of [{ ms: 0 }, { count: 0 }, { hasMore: true }, { buttonAbsent: false }, { loaded: 50 }, { applicable: true }]) {
    const values = samples('small'); values[0] = { ...values[0], ...patch }
    assert.throws(() => aggregateObservationLoadMore(values, 'small'))
  }
})

test('large aggregation requires five exercised finite actual timings and rejects missing, mixed or terminal claims', () => {
  const summary = aggregateObservationLoadMore(samples('large'), 'large')
  assert.equal(summary.applicable, true); assert.equal(summary.exercisedSamples, 5)
  assert.deepEqual(summary.rawMs, [100, 101, 102, 103, 104]); assert.equal(summary.p95Ms, 104)
  for (const patch of [{ ms: undefined }, { ms: NaN }, { ms: 0 }, { exercised: false }, { applicable: false }, { count: 0 }, { loaded: 100 }, { receipt: null }]) {
    const values = samples('large'); values[0] = { ...values[0], ...patch }
    assert.throws(() => aggregateObservationLoadMore(values, 'large'))
  }
  assert.throws(() => aggregateObservationLoadMore(samples('large').slice(1), 'large'))
})

test('evaluation keeps large two-request/500ms gates and never waives eligibility or numeric evidence', () => {
  const small = aggregateObservationLoadMore(samples('small'), 'small'), large = aggregateObservationLoadMore(samples('large'), 'large')
  const budget = { maxRequests: 2, maxP95Ms: 500 }
  assert.equal(observationPaginationMetrics(small, large, budget, true).every(metric => metric.pass), true)
  for (const comparable of [true, false]) {
    for (const invalid of [{ applicable: false }, { exercisedSamples: 4 }, { rawMs: [100, 100, 100, 100, NaN] }, { requestCounts: [1, 1, 1, 1, 3] }]) assert.equal(observationPaginationMetrics(small, { ...large, ...invalid }, budget, comparable).every(metric => metric.pass), false)
  }
  assert.equal(observationPaginationMetrics(small, { ...large, rawMs: [501, 501, 501, 501, 501] }, budget, true).every(metric => metric.pass), false)
  assert.equal(observationPaginationMetrics(small, { ...large, rawMs: [501, 501, 501, 501, 501] }, budget, false).every(metric => metric.pass), true)
  assert.equal(observationPaginationMetrics({ ...small, terminalSamples: 4 }, large, budget, false).every(metric => metric.pass), false)
})

test('actual list receipt captures wire method, workspace, offset, limit, filters, order and hasMore; revision fields reach list and facet responders', async () => {
  const fixture = generateFixture('small'), tracker = createTracker(fixture), respond = fixtureResponder(fixture, tracker)
  fixture.observations[0].current_provenance = { reviewed_git_sha: 'b'.repeat(40) }
  fixture.observations[0].revision_history = [{ reviewed_git_sha: 'c'.repeat(40) }]
  const query = { currentGitSha: 'b'.repeat(40), historyGitSha: 'c'.repeat(40), limit: 200, offset: 0 }
  for (const [path, expected] of [['/api/v1/observations', queryObservations(fixture, query)], ['/api/v1/observations/subject-facets', queryObservationFacets(fixture, query)]]) {
    const url = new URL('http://perf.local' + path)
    for (const [name, value] of Object.entries({ workspace_id: fixture.workspace.id, current_git_sha: query.currentGitSha, history_git_sha: query.historyGitSha, offset: '0', limit: '200' })) url.searchParams.set(name, value)
    let response
    const request = { url: () => url.href, method: () => 'GET' }
    await respond({ request: () => request, fulfill: async value => { response = value } })
    assert.deepEqual(JSON.parse(response.body), expected)
    if (path.endsWith('/observations')) {
      const receipt = tracker.requests.at(-1).observationPage
      assert.equal(receipt.request.current_git_sha, query.currentGitSha)
      assert.equal(receipt.request.history_git_sha, query.historyGitSha)
      assert.equal(receipt.request.workspace_id, fixture.workspace.id)
      assert.equal(receipt.request.offset, 0); assert.equal(receipt.request.limit, 200)
      assert.equal(receipt.response.fulfilled, true)
      assert.equal(tracker.bytes['observations:list'], Buffer.byteLength(response.body))
    }
  }
})
