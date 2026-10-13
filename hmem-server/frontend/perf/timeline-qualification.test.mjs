import test from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import { createHash } from 'node:crypto'
import { TIMELINE_CURRENT_CONTRACT, HARNESS_CONFIGURATION, timelineReceipt, assertTimelineReceipt, timelineChartExpectation, renderBudgetEvaluation, renderMaximum } from './contracts.mjs'
import { TIMELINE_DEFAULT_UI_QUERY, generateFixture, queryTimelineEvents, queryTimelineBuckets, fixtureHash } from './fixtures.mjs'
import { timelineSource, timelineDomReady, timelineDomSnapshot, waitTimeline, assertTimelineTransport, fixtureResponder, createTracker, measureRun, staticServer } from './harness.mjs'

function expectedFor(fixture) {
  const events = queryTimelineEvents(fixture, { limit: 50 }), buckets = queryTimelineBuckets(fixture, TIMELINE_CURRENT_CONTRACT.query)
  return { events, buckets, chart: timelineChartExpectation(events, buckets) }
}

function trackerFor(fixture) {
  const expected = expectedFor(fixture), tracker = createTracker(fixture)
  for (const kind of ['events', 'buckets']) {
    const url = new URL(`http://perf.local/api/v1/workspaces/${fixture.workspace.id}/timeline${kind === 'events' ? '' : '/buckets'}`)
    for (const [name, value] of Object.entries(kind === 'events' ? { limit: '50' } : { ...TIMELINE_CURRENT_CONTRACT.query, paged: 'true' })) url.searchParams.set(name, value)
    const receipt = timelineReceipt(url.href, 'GET', expected[kind])
    receipt.response.fulfilled = true
    tracker.requests.push({ key: 'timeline:' + kind, method: 'GET', url: url.href, timeline: receipt })
  }
  Object.assign(tracker.model, { timelineEvents: expected.events.items.length, timelineBuckets: expected.buckets.buckets.length, timelineBucketRequest: TIMELINE_CURRENT_CONTRACT.query })
  tracker.lastActivityAt = 0
  return { tracker, expected }
}

function documentFor(expected) {
  const node = (id, attributes = {}, textContent = '') => ({ id, namespaceURI: 'http://www.w3.org/2000/svg', textContent,
    getAttribute: name => attributes[name] ?? null, classList: { contains: name => attributes.class === name } })
  const paths = expected.actions.map(action => node('', { class: 'timeline-series-' + action, d: 'M 44 100 L 172 100' }))
  const toggles = expected.actions.map(action => node('', { class: 'timeline-series-' + action, 'aria-pressed': 'true' }))
  const entityToggles = ['projects', 'tasks', 'subtasks', 'observations'].map(entity => node('', { class: 'timeline-series-' + entity, 'aria-pressed': 'true' }))
  const points = expected.points.map(point => ({ ...node(point.id, { role: point.role, tabindex: point.tabindex, 'aria-pressed': point.pressed, 'aria-label': point.label }),
    querySelector: selector => selector === 'title' ? { textContent: point.title } : null,
    querySelectorAll: () => [{}] }))
  const lists = {
    '.timeline-event-card': expected.eventIds.map(id => node(id)),
    'svg.timeline-line-chart': [node('', { 'aria-labelledby': 'timeline-chart-lifecycle-title timeline-chart-lifecycle-description' })],
    '.timeline-graph-controls select': [{ value: '30' }, { value: 'week' }],
    'svg.timeline-line-chart path.timeline-line': paths,
    'path.timeline-line': paths,
    '.timeline-series-toggle': [...entityToggles, ...toggles],
    '[aria-label="Visible lifecycle events"] .timeline-series-toggle': toggles,
    '[aria-label="Visible entity series"] .timeline-series-toggle': entityToggles,
    '.timeline-point-control': points
  }
  const ids = { 'timeline-chart-lifecycle-title': { textContent: 'Lifecycle activity lifecycle counts by UTC bucket' },
    'timeline-chart-lifecycle-description': { textContent: 'Each interactive point selects its half-open UTC bucket for the event cards.' } }
  let blocked = false
  return { lists, ids, points, paths, toggles, entityToggles, block: () => { blocked = true },
    getElementById: id => ids[id] || null,
    querySelectorAll: selector => lists[selector] || [],
    querySelector: selector => selector === '.timeline-panel' ? {} : blocked && selector.includes('timeline-graph-error') ? { textContent: 'error '.repeat(100) } : null }
}

async function withDocument(document, action) {
  const old = globalThis.document
  globalThis.document = document
  try { return await action() } finally { if (old === undefined) delete globalThis.document; else globalThis.document = old }
}

test('current Timeline contract source anchors 30-day weekly SVG separately from preserved historical fixture/config/budgets', () => {
  const source = timelineSource()
  for (const entry of source.sources) assert.equal(entry.sha256, createHash('sha256').update(fs.readFileSync(entry.path.replace('hmem-server/frontend/', ''))).digest('hex'))
  assert.equal(source.window, '30'); assert.equal(source.bucket, 'week')
  assert.equal(TIMELINE_CURRENT_CONTRACT.legacyTimingComparability.comparable, false)
  assert.equal(HARNESS_CONFIGURATION.browserClockUtc, TIMELINE_CURRENT_CONTRACT.clockUtc)
  assert.equal(HARNESS_CONFIGURATION.warmups, 2); assert.equal(HARNESS_CONFIGURATION.samples, 5)
  for (const scale of ['small', 'large']) {
    const fixture = generateFixture(scale)
    assert.equal(queryTimelineBuckets(fixture, TIMELINE_DEFAULT_UI_QUERY).buckets.length, 13)
    assert.equal(expectedFor(fixture).buckets.buckets.length, 5)
    assert.equal(expectedFor(fixture).chart.points.length, 25)
    assert.equal(fixtureHash(fixture), scale === 'small' ? '2f269eb2256dc4dde701cf769e2d83c73a99c29de26b1da8ae24a343cd39e3cb' : '0c966ae9c5476317ba8f7f05ad0200a9c8526e16c430e6068a8a4daf9e35a077')
  }
  assert.equal(typeof measureRun, 'function'); assert.equal(typeof staticServer, 'function')
})

test('actual fulfilled Timeline receipts prove exact method/workspace/filters/DTO and reject missing or duplicate pages and stale model', () => {
  const fixture = generateFixture('small'), { tracker, expected } = trackerFor(fixture)
  assert.deepEqual(assertTimelineTransport(tracker, { index: 0 }, fixture).chart, expected.chart)
  for (const kind of ['events', 'buckets']) {
    const good = tracker.requests.find(request => request.key === 'timeline:' + kind).timeline
    for (const bad of [
      { ...good, request: { ...good.request, method: 'POST' } },
      { ...good, request: { ...good.request, path: good.request.path.replace(fixture.workspace.id, 'foreign') } },
      { ...good, request: { ...good.request, query: { ...good.request.query, offset: '0' } } },
      { ...good, response: { ...good.response, fulfilled: false } },
      { ...good, response: { ...good.response, status: 403 } },
      { ...good, response: { ...good.response, value: {} } }
    ]) assert.throws(() => assertTimelineReceipt(bad, fixture.workspace.id, kind, expected[kind]), /receipt mismatch/)
  }
  assert.throws(() => assertTimelineTransport(tracker, { index: 2 }, fixture), /observed 0/)
  tracker.requests.push(tracker.requests[0])
  assert.throws(() => assertTimelineTransport(tracker, { index: 0 }, fixture), /observed 2/)
  tracker.requests.pop(); tracker.model.timelineBuckets = 13
  assert.throws(() => assertTimelineTransport(tracker, { index: 0 }, fixture), /model/)
})

test('canonical enabled lifecycle points accept zero activity and enforce count/range/title/keyboard/namespace semantics', async () => {
  const fixture = generateFixture('small'), expected = expectedFor(fixture)
  for (const bucket of expected.buckets.buckets) for (const counts of Object.values(bucket.series)) for (const key of Object.keys(counts)) counts[key] = 0
  const chart = timelineChartExpectation({ items: [] }, expected.buckets)
  assert.equal(chart.points.every(point => point.label.endsWith(', 0, not selected')), true)
  await withDocument(documentFor(chart), () => {
    assert.equal(document.querySelectorAll('.timeline-series-toggle').length, 9)
    assert.equal(timelineDomReady(chart), true)
    assert.equal(timelineDomSnapshot().lifecycleToggles.length, 5)
    assert.equal(timelineDomSnapshot().entityToggles.length, 4)
  })
  for (const mutate of [
    doc => { doc.lists['svg.timeline-line-chart'] = [] },
    doc => { doc.paths[0].namespaceURI = 'http://www.w3.org/1999/xhtml' },
    doc => { doc.paths[0].getAttribute = () => '' },
    doc => { doc.points.pop() },
    doc => { doc.points.reverse() },
    doc => { doc.points[0].getAttribute = () => 'wrong' },
    doc => { doc.points[0].querySelector = () => ({ textContent: 'wrong count/range' }) },
    doc => { doc.points[0].querySelectorAll = () => [] },
    doc => { doc.toggles[0].getAttribute = () => 'false' },
    doc => { doc.toggles[0].classList.contains = () => false },
    doc => { doc.lists['[aria-label="Visible lifecycle events"] .timeline-series-toggle'] = doc.entityToggles },
    doc => { doc.entityToggles[0].getAttribute = () => 'false' },
    doc => { doc.entityToggles.pop() },
    doc => { doc.lists['.timeline-graph-controls select'][0].value = '6months' },
    doc => { doc.block() }
  ]) {
    const doc = documentFor(chart); mutate(doc)
    await withDocument(doc, () => assert.equal(timelineDomReady(chart), false))
  }
  const bad = structuredClone(expected.buckets); bad.buckets[0].series.project.created = -1
  assert.throws(() => timelineChartExpectation(expected.events, bad), /invalid/)
})

test('real Timeline fixture responder retains exact wire DTO accounting and fulfilled model readiness', async () => {
  const fixture = generateFixture('large'), { tracker: expectedTracker } = trackerFor(fixture)
  const tracker = createTracker(fixture), respond = fixtureResponder(fixture, tracker)
  for (const expected of expectedTracker.requests) {
    let result
    const request = { url: () => expected.url, method: () => 'GET' }
    await respond({ request: () => request, fulfill: async response => { result = response } })
    assert.deepEqual(JSON.parse(result.body), expected.timeline.response.value)
    assert.equal(tracker.requests.at(-1).timeline.response.fulfilled, true)
    assert.equal(tracker.bytes[expected.key], Buffer.byteLength(result.body))
  }
  assert.equal(assertTimelineTransport(tracker, { index: 0 }, fixture).chart.points.length, 25)
})

test('Timeline wait retains deadline, exact success proof and bounded original-cause failure diagnostics', async () => {
  const fixture = generateFixture('small'), { tracker, expected } = trackerFor(fixture)
  let captures = 0
  await withDocument(documentFor(expected.chart), async () => {
    const result = await waitTimeline({
      waitForFunction: async (predicate, input, options) => { assert.equal(predicate, timelineDomReady); assert.equal(predicate(input), true); assert.deepEqual(options, { timeout: 30000 }) },
      evaluate: async capture => { assert.notEqual(capture, timelineDomSnapshot); captures += 1 }
    }, tracker, { index: 0 }, fixture)
    assert.equal(result.chart.points.length, 25); assert.equal(captures, 1)
    const timeout = Object.assign(new Error('Timeout 30000ms exceeded'), { name: 'TimeoutError' })
    const doc = documentFor(expected.chart); doc.block()
    await withDocument(doc, () => assert.rejects(waitTimeline({ waitForFunction: async () => { throw timeout }, evaluate: async capture => capture() }, tracker, { index: 0 }, fixture), error => {
      assert.equal(error.cause, timeout); assert.equal(error.timelineQualification.dom.points, 25)
      assert.equal(error.timelineQualification.dom.error.length, 256)
      assert.equal(error.timelineQualification.requests.length, 2)
      assert.equal(error.timelineQualification.model.buckets, 5)
      return true
    }))
    await assert.rejects(waitTimeline({ waitForFunction: async () => { throw timeout }, evaluate: async () => new Promise(() => {}) }, tracker, { index: 0 }, fixture), error => {
      assert.equal(error.cause, timeout); assert.match(error.timelineQualification.dom.captureError, /1000ms/); return true
    })
  })
})

test('physical metrics count SVG controls alongside legacy rows without changing budgets or later timing guards', () => {
  const source = fs.readFileSync('perf/harness.mjs', 'utf8')
  assert.equal((source.match(/\.timeline-value-table tbody tr,\.timeline-point-control/g) || []).length, 3)
  assert.match(source, /timelineCurrentContractHash: hashJson\(TIMELINE_CURRENT_CONTRACT\)/)
  assert.match(source, /timelineCurrentSource: timelineSource\(\)/)
  assert.match(source, /timelineTabMs = await requiredDoubleFrame/)
  assert.match(source, /const directFocus = await measureDirectFocus/)
  assert.match(source, /await assertLiveStability\(page, tracker, 500\)/)
  assert.match(source, /HeapProfiler.collectGarbage/)
  assert.match(source, /stage\('Timeline'\)/)
  assert.match(source, /stage\('direct focus'\)/)
  assert.equal((source.match(/timelineQualification: error.timelineQualification/g) || []).length, 3)
  assert.match(source, /timelineQualification: failure.timelineQualification/)
  const budget = JSON.parse(fs.readFileSync('perf/budgets.v1.json')).large.render
  assert.equal(renderBudgetEvaluation(renderMaximum([{ nodes: budget.maxDomNodes, rows: budget.maxCollectionRows }]), budget).passed, true)
  assert.equal(renderBudgetEvaluation({ nodes: budget.maxDomNodes, rows: budget.maxCollectionRows + 25 }, budget).passed, false)
})
