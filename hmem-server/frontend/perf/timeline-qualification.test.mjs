import test from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import { createHash } from 'node:crypto'
import { TIMELINE_CURRENT_CONTRACT, HARNESS_CONFIGURATION, timelineReceipt, assertTimelineReceipt, timelineChartExpectation, renderBudgetEvaluation, renderMaximum } from './contracts.mjs'
import { TIMELINE_DEFAULT_UI_QUERY, generateFixture, queryTimelineEvents, queryTimelineBuckets, fixtureHash } from './fixtures.mjs'
import { timelineSource, timelineDomReady, timelineDomSnapshot, timelineBrowserTimeZone, waitTimeline, assertTimelineTransport, fixtureResponder, createTracker, measureRun, staticServer } from './harness.mjs'

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
  const paths = expected.series.map(action => node('', { class: 'timeline-series-' + action, d: 'M 44 100 L 172 100' }))
  const toggles = expected.actions.map(action => node('', { class: 'timeline-series-' + action, 'aria-pressed': 'true' }))
  const entityToggles = ['projects', 'tasks', 'subtasks', 'observations'].map(entity => node('', { class: 'timeline-series-' + entity, 'aria-pressed': 'true' }))
  const points = expected.points.map(point => ({ ...node(point.id, { role: point.role, tabindex: point.tabindex, 'aria-pressed': point.pressed, 'aria-label': point.label }),
    querySelector: selector => selector === 'title' ? { textContent: point.label } : null,
    querySelectorAll: () => [{}] }))
  const lists = {
    '.timeline-event-card': expected.eventIds.map(id => node(id)),
    'svg.timeline-line-chart': [node('', { 'aria-labelledby': 'timeline-chart-lifecycle-title timeline-chart-lifecycle-description' })],
    '.timeline-graph-controls select': [{ value: '30' }, { value: 'week' }],
    'svg.timeline-line-chart path.timeline-line': paths,
    'path.timeline-line': paths,
    '.timeline-series-toggle': [...entityToggles, ...toggles],
    '[aria-label="Lifecycle line actions"] .timeline-series-toggle': toggles,
    '[aria-label="Lifecycle line entities"] .timeline-series-toggle': entityToggles,
    '.timeline-point-control': points
  }
  const ids = { 'timeline-chart-lifecycle-title': { textContent: expected.title },
    'timeline-chart-lifecycle-description': { textContent: expected.description } }
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
    assert.equal(expectedFor(fixture).chart.points.length, 30)
    assert.equal(fixtureHash(fixture), scale === 'small' ? '0c425c77e5069b8d4cf26dbdebcf94037a22ee977e61d63ab3cceb1a7f090bc4' : 'b98d53a1a1ca28641dc90cbb598f3026949edf702f0ae4696614520b5140dd63')
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
    doc => { doc.paths.pop() },
    doc => { doc.paths.push(doc.paths[0]) },
    doc => { doc.points.push(doc.points[0]) },
    doc => { doc.ids['timeline-chart-lifecycle-title'].textContent = 'old chart title' },
    doc => { doc.ids['timeline-chart-lifecycle-description'].textContent = 'old chart description' },
    doc => { doc.paths[0].namespaceURI = 'http://www.w3.org/1999/xhtml' },
    doc => { doc.paths[0].getAttribute = () => '' },
    doc => { doc.points.pop() },
    doc => { doc.points[0].namespaceURI = 'http://www.w3.org/1999/xhtml' },
    doc => { doc.points.reverse() },
    doc => { doc.points[0].getAttribute = () => 'wrong' },
    doc => { doc.points[0].getAttribute = name => name === 'aria-label' ? 'wrong count/range' : null },
    doc => { doc.points[0].querySelectorAll = () => [] },
    doc => { doc.toggles[0].getAttribute = () => 'false' },
    doc => { doc.toggles[0].classList.contains = () => false },
    doc => { doc.lists['[aria-label="Lifecycle line actions"] .timeline-series-toggle'] = doc.entityToggles },
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
  assert.equal(assertTimelineTransport(tracker, { index: 0 }, fixture).chart.points.length, 30)
})

test('Timeline wait retains deadline, exact success proof and bounded original-cause failure diagnostics', async () => {
  const fixture = generateFixture('small'), { tracker, expected } = trackerFor(fixture)
  let captures = 0
  await withDocument(documentFor(expected.chart), async () => {
    const result = await waitTimeline({
      waitForFunction: async (predicate, input, options) => { assert.equal(predicate, timelineDomReady); assert.equal(predicate(input), true); assert.deepEqual(options, { timeout: 30000 }) },
      evaluate: async capture => { if (capture === timelineBrowserTimeZone) return 'UTC'; assert.notEqual(capture, timelineDomSnapshot); captures += 1 }
    }, tracker, { index: 0 }, fixture)
    assert.equal(result.chart.points.length, 30); assert.equal(captures, 1)
    const timeout = Object.assign(new Error('Timeout 30000ms exceeded'), { name: 'TimeoutError' })
    const doc = documentFor(expected.chart); doc.block()
    await withDocument(doc, () => assert.rejects(waitTimeline({ waitForFunction: async () => { throw timeout }, evaluate: async capture => capture() }, tracker, { index: 0 }, fixture), error => {
      assert.equal(error.cause, timeout); assert.equal(error.timelineQualification.dom.points, 30)
      assert.equal(error.timelineQualification.dom.error.length, 256)
      assert.equal(error.timelineQualification.requests.length, 2)
      assert.equal(error.timelineQualification.model.buckets, 5)
      return true
    }))
    await assert.rejects(waitTimeline({ waitForFunction: async () => { throw timeout }, evaluate: async capture => capture === timelineBrowserTimeZone ? 'UTC' : new Promise(() => {}) }, tracker, { index: 0 }, fixture), error => {
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


test('current six-series counts and summary labels are independent and follow captured browser timezone', () => {
  const fixture = generateFixture('small'), expected = expectedFor(fixture), histogram = structuredClone(expected.buckets)
  const bucket = histogram.buckets[0]
  Object.assign(bucket.series.project, { created: 1, completed: 2, deleted: 3, archived: 4, cancelled: 999 })
  Object.assign(bucket.series.task, { created: 5, completed: 6, deleted: 7, archived: 999, cancelled: 8 })
  Object.assign(bucket.series.subtask, { created: 9, completed: 10, deleted: 11, archived: 999, cancelled: 12 })
  Object.assign(bucket.series.observation, { created: 13, completed: 999, deleted: 999, archived: 999, cancelled: 999 })
  const chart = timelineChartExpectation(expected.events, histogram, { timeZone: 'Europe/Copenhagen' })
  assert.deepEqual(chart.series, ['created', 'completed', 'deleted', 'archived', 'cancelled', 'observations'])
  const first = chart.points.filter(point => point.id.endsWith('-0'))
  assert.deepEqual(first.map(point => Number(point.label.match(/exclusive, (\d+),/)[1])), [15, 18, 21, 4, 20, 13])
  assert.ok(first.every(point => point.label.startsWith('13 Observations; 1 Projects Created; 14 Tasks/Subtasks Created; 2 Projects Completed; 16 Tasks/Subtasks Completed; 3 Projects Deleted; 18 Tasks/Subtasks Deleted; 20 Tasks/Subtasks Cancelled; 4 Projects Archived. ')))
  assert.match(first[0].label, /Lifecycle activity, Create, 2026-07-27 02:00 to 2026-08-03 02:00 exclusive, 15, not selected$/)
  assert.notEqual(first[0].label, timelineChartExpectation(expected.events, histogram, { timeZone: 'UTC' }).points[0].label)
  assert.throws(() => timelineChartExpectation(expected.events, histogram, { timeZone: '' }), /timezone is missing/)
  assert.throws(() => timelineChartExpectation(expected.events, histogram, { timeZone: 'invalid' }), /Invalid time zone/)
  const observations = chart.points.find(point => point.id.includes('-observations-'))
  assert.match(observations.label, /Observations, .*exclusive, 13, not selected$/)
  assert.equal(chart.title, 'Lifecycle activity by interval')
})


test('Timeline capture rejects missing browser timezone before any readiness success', async () => {
  const fixture = generateFixture('small'), { tracker } = trackerFor(fixture)
  let readinessCalls = 0
  const page = { evaluate: async capture => capture === timelineBrowserTimeZone ? undefined : { missingZone: true },
    waitForFunction: async () => { readinessCalls++ } }
  await assert.rejects(waitTimeline(page, tracker, { index: 0 }, fixture), /browser timezone is missing/)
  assert.equal(readinessCalls, 0)
})
