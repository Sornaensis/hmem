import { createHash, randomUUID } from 'node:crypto'
import { spawnSync } from 'node:child_process'
import fs from 'node:fs'

export const BASE_COMMIT = 'ca04f51c3494c83c433d12bd7791f89be876daea'
export const HARNESS_CONFIGURATION = Object.freeze({
  schemaVersion: 1,
  buildCommand: 'npm run build',
  buildMode: 'production',
  transport: 'Playwright-intercepted HTTP and canonical WebSocket',
  viewport: { width: 1440, height: 1000 },
  warmups: 2,
  samples: 5,
  browserClockUtc: '2026-08-30T12:00:00Z',
  percentile: 'nearest-rank p95: sorted[ceil(0.95*n)-1]; with five samples p95 is the maximum',
  scenarioIsolation: 'cold cuts at authorized workspace_shell_v1 and the first painted root-summary anchor; count every request arrival and fixture byte before that cut, including held responses; separately verify complete automatically expanded membership, terminal pages, scroll reachability, physical admission and lifetime render/observer maxima; tab/live use deterministic unfiltered/unfocused restoration; direct focus uses a fresh focus-first shell session',
  initialTransport: 'production workspace bootstrap requests workspace_shell_v1 and one demand-driven root page, while effectively expanded descendants automatically drain independent 50-item streams under four branch admissions; mounted-detail demand is capped at six; the immutable full snapshot remains a legacy compatibility fixture only',
  routeFidelity: 'snapshot kind-rank/identity ordering; capped list pagination; Project/Task DTO and overview ordering; Observation simple-lexeme AND/rank/subject/facet ordering; Timeline event ordering and day/week/month/quarter UTC bucket range aggregation/caps are frozen against production source anchors',
  directFocus: 'navigate a project focus first from an empty shell state; observe one bounded navigation-focus lookup and render for 750ms without requiring legacy full resync',
  liveTiming: 'settle ends after the dispatched checkpoint turn, current root/filter lifetime and independent latest-pass terminal coverage of every effective-expanded descendant (cached untouched passes remain valid), completion of every follow-up that actually started (zero is valid), transport idle, representative UI anchor, and two paints; a separate 500ms stability window must observe no late request',
  heapPoint: 'after the 50-frame live batch settles and its untimed stability assertion, Projects is unfiltered and unfocused with complete intercepted-model transport and representative anchors, then Chromium garbage collection runs'
})

export function hashJson(value) {
  return createHash('sha256').update(JSON.stringify(value)).digest('hex')
}

export const OBSERVATION_PAGINATION_CONTRACT = Object.freeze({
  revision: 'observation-pagination.200.v1', method: 'GET', path: '/api/v1/observations', pageSize: 200,
  source: 'hmem-server/frontend/src/Feature/Observation.elm:pageSize',
  reviewBase: 'f546cafd26adae37772d2383262e87596ea7151b',
  small: Object.freeze({ total: 60, initial: 60, loadMoreApplicable: false }),
  large: Object.freeze({ total: 2000, initial: 200, afterLoadMore: 400, loadMoreApplicable: true }),
  legacyTimingComparability: Object.freeze({ pageSize: 50, comparable: false, reason: 'Current 200-item pagination has a different measured workload from retained 50-item pagination.' })
})

export function observationPageReceipt(input, method, value, status = 200) {
  const url = new URL(input)
  const integer = name => { const raw = url.searchParams.get(name); return raw != null && /^\d+$/.test(raw) ? Number(raw) : null }
  return { request: { method, path: url.pathname, workspace_id: url.searchParams.get('workspace_id'),
    query: url.searchParams.get('query'), subject_kind: url.searchParams.get('subject_kind'), subject: url.searchParams.get('subject'),
    git_sha: url.searchParams.get('git_sha'), current_git_sha: url.searchParams.get('current_git_sha'), history_git_sha: url.searchParams.get('history_git_sha'),
    offset: integer('offset'), limit: integer('limit') },
    response: { ids: value.items?.map(item => item.id) || null, hasMore: value.has_more, status, fulfilled: false } }
}

export function assertObservationPageReceipt(receipt, { workspaceId, offset, query = null, expectedIds, hasMore }) {
  const request = receipt?.request, response = receipt?.response
  if (!request || request.method !== 'GET' || request.path !== OBSERVATION_PAGINATION_CONTRACT.path
    || request.workspace_id !== workspaceId || request.offset !== offset || request.limit !== 200
    || (request.query || null) !== (query || null)
    || ['subject_kind', 'subject', 'git_sha', 'current_git_sha', 'history_git_sha'].some(key => request[key] != null)
    || !response || response.status !== 200 || response.fulfilled !== true || response.hasMore !== hasMore
    || JSON.stringify(response.ids) !== JSON.stringify(expectedIds)) throw new Error('Observation pagination wire/prefix receipt mismatch')
  return receipt
}

export function aggregateObservationLoadMore(samples, scale) {
  if (!['small', 'large'].includes(scale) || samples.length !== 5) throw new Error('Observation pagination requires five samples from one known scale')
  const applicable = scale === 'large'
  if (samples.some(sample => sample?.applicable !== applicable)) throw new Error('Observation load-more applicability changed or contradicts its scale')
  for (const sample of samples) {
    const receipt = sample.receipt, ids = receipt?.response?.ids
    if (!Array.isArray(ids) || new Set(ids).size !== ids.length || !receipt?.request?.workspace_id) throw new Error('Observation pagination lacks an actual unique page receipt')
    assertObservationPageReceipt(receipt, { workspaceId: receipt.request.workspace_id, offset: applicable ? 200 : 0, expectedIds: ids, hasMore: applicable })
    if (ids.length !== (applicable ? 200 : 60) || sample.loaded !== (applicable ? 400 : 60)) throw new Error('Observation pagination membership/exhaustion mismatch')
    if (applicable) {
      if (sample.exercised !== true || !Number.isFinite(sample.ms) || sample.ms <= 0 || !Number.isInteger(sample.count) || sample.count < 1) throw new Error('Eligible Observation load-more requires actual finite timing and requests')
    } else if (sample.reason !== 'terminal-first-page' || sample.buttonAbsent !== true || sample.hasMore !== false || 'ms' in sample || 'count' in sample) throw new Error('Terminal small pagination must not invent an exercised timing or request count')
  }
  if (!applicable) return { applicable: false, reason: 'terminal-first-page', terminalSamples: 5, receipts: samples.map(sample => sample.receipt) }
  const rawMs = assertFiveSamples(samples.map(sample => sample.ms), 'Observation load more')
  return { applicable: true, exercisedSamples: 5, rawMs, medianMs: median(rawMs), p95Ms: nearestRankP95(rawMs), requestCounts: samples.map(sample => sample.count), receipts: samples.map(sample => sample.receipt) }
}

export function observationPaginationMetrics(small, large, budget, comparableEnvironment) {
  const exercised = large?.applicable === true && large.exercisedSamples === 5 && large.rawMs?.length === 5
    && large.rawMs.every(ms => Number.isFinite(ms) && ms > 0) && large.requestCounts?.length === 5
    && large.requestCounts.every(count => Number.isInteger(count) && count > 0)
  const requests = exercised ? Math.max(...large.requestCounts) : null
  const timing = exercised ? nearestRankP95(large.rawMs) : null
  return [
    { name: 'small Observation terminal pagination', actual: small?.terminalSamples ?? null, expected: 5, pass: small?.applicable === false && small.terminalSamples === 5, category: 'exact' },
    { name: 'large Observation load-more exercised samples', actual: large?.exercisedSamples ?? null, expected: 5, pass: !!exercised, category: 'exact' },
    { name: 'Observation load-more requests', actual: requests, expected: budget.maxRequests, pass: !!exercised && requests <= budget.maxRequests, category: 'exact' },
    { name: 'Observation load-more p95 ms', actual: timing, expected: budget.maxP95Ms, pass: !!exercised && (!comparableEnvironment || timing <= budget.maxP95Ms), category: comparableEnvironment ? 'pinned-environment' : 'informational' }
  ]
}

export const TIMELINE_CURRENT_CONTRACT = Object.freeze({
  revision: 'timeline-window-svg.v2', clockUtc: '2026-08-30T12:00:00Z', window: '30',
  query: Object.freeze({ since: '2026-08-01T00:00:00Z', until: '2026-08-31T00:00:00Z', bucket: 'week' }),
  eventLimit: 50, actions: Object.freeze(['created', 'completed', 'deleted', 'archived', 'cancelled']),
  series: Object.freeze(['created', 'completed', 'deleted', 'archived', 'cancelled', 'observations']),
  title: 'Lifecycle activity by interval',
  description: 'Lines show lifecycle activity and observations on the left axis. Stacked bars show open projects and tasks plus subtasks on the right axis. Hover or focus a line to highlight it and show selected activity counts. Bars and points select their interval for event cards. Arrow keys move between intervals. Axis dates use your browser timezone.',
  source: 'hmem-server/frontend/src/Feature/Timeline.elm',
  legacyTimingComparability: Object.freeze({ comparable: false, reason: 'Current 30-day SVG qualification differs from the retained three-month table workload.' })
})

export function timelineReceipt(input, method, value, status = 200) {
  const url = new URL(input)
  return { request: { method, path: url.pathname, query: Object.fromEntries([...url.searchParams].sort()) },
    response: { status, fulfilled: false, value } }
}

export function assertTimelineReceipt(receipt, workspaceId, kind, expected) {
  const request = receipt?.request, response = receipt?.response
  const query = kind === 'events' ? { limit: '50' } : { bucket: 'week', paged: 'true', since: TIMELINE_CURRENT_CONTRACT.query.since, until: TIMELINE_CURRENT_CONTRACT.query.until }
  const suffix = kind === 'events' ? '' : '/buckets'
  if (!['events', 'buckets'].includes(kind) || request?.method !== 'GET'
    || request.path !== `/api/v1/workspaces/${workspaceId}/timeline${suffix}`
    || JSON.stringify(request.query) !== JSON.stringify(query) || response?.status !== 200 || response.fulfilled !== true
    || JSON.stringify(response.value) !== JSON.stringify(expected)) throw new Error('Timeline exact request/DTO receipt mismatch: ' + kind)
  return receipt
}

export function timelineChartExpectation(events, histogram, { timeZone = 'UTC' } = {}) {
  if (typeof timeZone !== 'string' || !timeZone) throw new Error('Timeline browser timezone is missing')
  const formatter = new Intl.DateTimeFormat('en-GB', { timeZone, calendar: 'gregory', numberingSystem: 'latn', hourCycle: 'h23',
    year: 'numeric', month: '2-digit', day: '2-digit', hour: '2-digit', minute: '2-digit' })
  const format = value => {
    const parts = Object.fromEntries(formatter.formatToParts(new Date(value)).map(part => [part.type, part.value]))
    return `${parts.year}-${parts.month}-${parts.day} ${parts.hour}:${parts.minute}`
  }
  const labels = { created: 'Create', completed: 'Complete', deleted: 'Delete', archived: 'Archive', cancelled: 'Cancel', observations: 'Observations' }
  const count = (bucket, kind, action) => {
    const value = bucket.series[kind][action] ?? 0
    if (!Number.isSafeInteger(value) || value < 0) throw new Error('Timeline canonical count is invalid')
    return value
  }
  const total = (bucket, action) => action === 'observations' ? count(bucket, 'observation', 'created')
    : (action === 'cancelled' ? 0 : count(bucket, 'project', action))
      + (action === 'archived' ? 0 : count(bucket, 'task', action) + count(bucket, 'subtask', action))
  const summary = bucket => [count(bucket, 'observation', 'created') + ' Observations',
    ...[['created', 'Created'], ['completed', 'Completed'], ['deleted', 'Deleted'], ['cancelled', 'Cancelled'], ['archived', 'Archived']].flatMap(([action, label]) => [
      ...(action === 'cancelled' ? [] : [count(bucket, 'project', action) + ' Projects ' + label]),
      ...(action === 'archived' ? [] : [count(bucket, 'task', action) + count(bucket, 'subtask', action) + ' Tasks/Subtasks ' + label])
    ])].join('; ')
  const points = TIMELINE_CURRENT_CONTRACT.series.flatMap(action => histogram.buckets.map((bucket, index) => ({
    id: `timeline-point-lifecycle-${action}-${index}`, role: 'button', tabindex: index === 0 ? '0' : '-1', pressed: 'false',
    label: `${summary(bucket)}. Lifecycle activity, ${labels[action]}, ${format(bucket.bucket_start)} to ${format(bucket.bucket_end)} exclusive, ${total(bucket, action)}, not selected`
  })))
  return { eventIds: events.items.map(event => 'timeline-event-' + event.id), points, actions: TIMELINE_CURRENT_CONTRACT.actions,
    series: TIMELINE_CURRENT_CONTRACT.series, title: TIMELINE_CURRENT_CONTRACT.title, description: TIMELINE_CURRENT_CONTRACT.description, timeZone, window: '30', bucket: 'week' }
}

export function nearestRankP95(values) {
  if (!Array.isArray(values) || values.length === 0 || values.some(value => !Number.isFinite(value))) throw new Error('p95 requires non-empty finite samples')
  const sorted = [...values].sort((a, b) => a - b)
  return sorted[Math.ceil(sorted.length * 0.95) - 1]
}

export function median(values) {
  if (!Array.isArray(values) || values.length === 0 || values.some(value => !Number.isFinite(value))) throw new Error('median requires non-empty finite samples')
  const sorted = [...values].sort((a, b) => a - b)
  return sorted[Math.floor(sorted.length / 2)]
}

export function liveWholeWorkspaceReload(routes) {
  return ['change-stream:resync', 'projects:list', 'tasks:list', 'observations:list'].some(key => (routes[key] || 0) > 0)
}

export const LIVE_OBSERVATION_REFRESH_CONTRACT = Object.freeze({
  revision: 'live-observation-retained-prefix.v1', pageSize: 200, maximumRefreshPages: 1,
  retainedContextProof: 'Inferred from the same-run fulfilled unfiltered reset, executed logical membership guard, source and unchanged intervening recipe; offscreen cache state is not directly observed.',
  legacyRouteFlag: 'Retained separately; any Observation list remains true in the historical route-only classifier.',
  individualRefreshes: 'Distinct successful detail GETs must match independently updated IDs inside the validated reset cache and exact fixture DTOs; a whole-cache or whole-entity walk fails.'
})

export function classifyLiveObservationHydration(input) {
  const { runId, workspaceId, total, expectedPrefix, reset, intervening, requests, delta, active, unknownRoutes, snapshotGeneration, liveStartIndex, updatedObservationIds, expectedObservationEntities } = input
  const legacy = liveWholeWorkspaceReload(delta?.routes || {})
  const result = (classification, reason, proof = null) => ({ revision: LIVE_OBSERVATION_REFRESH_CONTRACT.revision, classification,
    passed: classification === 'bounded-cache-refresh' || classification === 'no-enumeration', wholeWorkspaceReload: classification === 'whole-hydration' || classification === 'unproven',
    legacyWholeWorkspaceReload: legacy, proofBasis: LIVE_OBSERVATION_REFRESH_CONTRACT.retainedContextProof, reason, proof })
  const sameMap = (left, right) => JSON.stringify(Object.entries(left || {}).sort()) === JSON.stringify(Object.entries(right || {}).sort())
  const routeKey = request => { try { return perfApiRouteKey(request.url, request.method) } catch { return null } }
  if (!runId || !workspaceId || !Array.isArray(requests) || !Array.isArray(unknownRoutes) || unknownRoutes.length || active !== 0 || !delta || !Number.isInteger(liveStartIndex) || liveStartIndex < 0) return result('unproven', 'Missing or incomplete live capture')
  const accounted = { count: requests.length, bytes: 0, routes: {}, routeBytes: {} }
  for (const [index, request] of requests.entries()) {
    if (request.runId !== runId || !request.done || request.fulfilled !== true || !Number.isInteger(request.status) || request.status < 200 || request.status >= 300
      || request.requestIndex !== liveStartIndex + index || request.requestId !== request.requestIndex + 1
      || !Number.isSafeInteger(request.bytes) || request.bytes < 0 || typeof request.key !== 'string' || routeKey(request) !== request.key) return result('unproven', 'Unsuccessful, unknown, foreign or malformed live receipt')
    accounted.bytes += request.bytes
    accounted.routes[request.key] = (accounted.routes[request.key] || 0) + 1
    accounted.routeBytes[request.key] = (accounted.routeBytes[request.key] || 0) + request.bytes
  }
  if (!Number.isSafeInteger(accounted.bytes) || delta.count !== accounted.count || delta.bytes !== accounted.bytes || !sameMap(delta.routes, accounted.routes) || !sameMap(delta.routeBytes, accounted.routeBytes)) return result('unproven', 'Live request/byte accounting mismatch')
  if (['change-stream:resync', 'projects:list', 'tasks:list', 'workspaces:list'].some(key => (accounted.routes[key] || 0) > 0)) return result('whole-hydration', 'Workspace or entity-list enumeration')
  if (requests.some(request => ['session', 'change-stream:ticket', 'workspaces:entity'].includes(request.key))) return result('unproven', 'Live workspace/session context admission or retirement')
  if (requests.some(request => request.workspaceId !== workspaceId || (!['workspaces:planning-counts', 'observations:count', 'observations:list', 'navigation:summaries', 'navigation:branch'].includes(request.key)
    && !/^(projects|tasks|observations):entity:/.test(request.key)))) return result('unproven', 'Foreign workspace or uncorroborated live context traffic')
  const lists = requests.filter(request => request.key === 'observations:list')
  const entities = requests.filter(request => request.key.startsWith('observations:entity:'))
  const requestAllowedBetween = request => request.runId === runId && request.workspaceId === workspaceId && request.done === true && request.fulfilled === true && request.status === 200 && request.method === 'GET'
    && routeKey(request) === request.key
    && (['timeline:events', 'timeline:buckets', 'navigation:branch', 'navigation:focus'].includes(request.key) || /^(projects|tasks):entity:/.test(request.key))
  const resetContext = expectedCount => reset && reset.runId === runId && reset.workspaceId === workspaceId && Number.isInteger(reset.requestIndex) && Number.isInteger(reset.requestId)
    && Number.isInteger(snapshotGeneration) && snapshotGeneration >= 1 && reset.snapshotGeneration === snapshotGeneration && reset.membership?.executed === true && reset.membership.expected === expectedCount
    && reset.recipe === 'reset-timeline-projects-separate-focus-projects-live.v1' && Array.isArray(intervening) && reset.recipeCompleted === true
    && reset.requestIndex < liveStartIndex && intervening.length === liveStartIndex - reset.requestIndex - 1
    && reset.receipt?.requestId === reset.requestId && reset.receipt.requestIndex === reset.requestIndex
    && !intervening.some((request, index) => !requestAllowedBetween(request) || request.requestIndex !== reset.requestIndex + index + 1 || request.requestId !== request.requestIndex + 1
      || (['timeline:events', 'timeline:buckets', 'navigation:branch', 'navigation:focus'].includes(request.key) && !new URL(request.url).pathname.startsWith(`/api/v1/workspaces/${workspaceId}/`)))
  const exactPage = (request, hasMore = true) => {
    const url = new URL(request.url)
    const keys = [...url.searchParams.keys()]
    if (request.runId !== runId || request.workspaceId !== workspaceId || request.method !== 'GET' || url.pathname !== '/api/v1/observations' || request.done !== true || request.fulfilled !== true || request.status !== 200
      || keys.length !== 3 || new Set(keys).size !== 3 || keys.some(key => !['workspace_id', 'limit', 'offset'].includes(key))
      || url.searchParams.get('workspace_id') !== workspaceId || url.searchParams.get('limit') !== '200' || url.searchParams.get('offset') !== '0') throw new Error('Not an exact current unfiltered first page')
    assertObservationPageReceipt(request.observationPage, { workspaceId, offset: 0, expectedIds: expectedPrefix, hasMore })
  }
  const compact = request => ({ runId: request.runId, workspaceId: request.workspaceId, requestId: request.requestId, requestIndex: request.requestIndex, method: request.method, url: request.url,
    bytes: request.bytes, status: request.status, fulfilled: request.fulfilled, count: request.observationPage.response.ids.length,
    prefixHash: hashJson(request.observationPage.response.ids), firstId: request.observationPage.response.ids[0], lastId: request.observationPage.response.ids.at(-1), hasMore: request.observationPage.response.hasMore })
  if (!lists.length) {
    const proof = { runId, workspaceId, accountedRequests: accounted.count, accountedBytes: accounted.bytes,
      liveStartIndex, liveEndIndex: liveStartIndex + accounted.count, requestsSucceeded: true, enumerationRequests: 0, observationEntityRequests: entities.length }
    if (entities.length) {
      const cached = Math.min(200, total), ids = entities.map(request => request.key.slice('observations:entity:'.length))
      if (!Number.isSafeInteger(total) || total <= 0 || !Array.isArray(expectedPrefix) || expectedPrefix.length !== cached || new Set(expectedPrefix).size !== cached
        || entities.length >= cached || entities.length >= total || new Set(ids).size !== ids.length) return result('whole-hydration', 'Duplicate or whole-cache Observation detail walk')
      if (!resetContext(cached) || !Array.isArray(updatedObservationIds) || new Set(updatedObservationIds).size !== updatedObservationIds.length || !expectedObservationEntities) return result('unproven', 'Missing current reset or independently updated Observation IDs')
      try { exactPage(reset.receipt, total > cached) } catch { return result('unproven', 'Detail refresh reset cache is not corroborated') }
      for (const request of entities) {
        const id = request.key.slice('observations:entity:'.length), url = new URL(request.url), dto = request.observationEntity, expected = expectedObservationEntities[id]
        if (request.method !== 'GET' || url.pathname !== '/api/v1/observations/' + id || url.search || request.status !== 200
          || !updatedObservationIds.includes(id) || !expectedPrefix.includes(id) || !dto || dto.id !== id || dto.workspaceId !== workspaceId
          || !expected || dto.dtoHash !== expected.hash || dto.bytes !== expected.bytes || request.bytes !== expected.bytes) return result('unproven', 'Observation detail ID/path/workspace/updated-cache/DTO mismatch')
      }
      Object.assign(proof, { backingTotal: total, inferredRetainedCount: cached, offscreenModelObserved: false, executedResetMembership: reset.membership,
        reset: compact(reset.receipt), interveningReceipts: intervening.length, cacheIds: expectedPrefix, updatedIds: updatedObservationIds,
        entities: entities.map(request => ({ runId: request.runId, workspaceId: request.workspaceId, requestId: request.requestId, requestIndex: request.requestIndex,
          id: request.observationEntity.id, method: request.method, url: request.url, status: request.status, fulfilled: request.fulfilled,
          bytes: request.bytes, dtoHash: request.observationEntity.dtoHash, expectedDtoHash: expectedObservationEntities[request.observationEntity.id].hash })) })
    }
    return result('no-enumeration', 'Complete live capture has only corroborated individual refreshes and no workspace/entity list enumeration', proof)
  }
  if (lists.length !== 1) return result('whole-hydration', 'Observation page growth, restart or multiple refresh pages')
  if (entities.length) return result('unproven', 'Mixed list/detail cache refresh is not corroborated by this contract')
  if (!Number.isSafeInteger(total) || total <= 200 || !Array.isArray(expectedPrefix) || expectedPrefix.length !== 200 || new Set(expectedPrefix).size !== 200) return result('whole-hydration', 'Observation first page is not independently bounded below full membership')
  if (!resetContext(200)) return result('unproven', 'Missing, foreign or stale accepted reset context')
  try { exactPage(reset.receipt); exactPage(lists[0]) }
  catch { return result('unproven', 'Reset/live wire, fulfillment or unique ordered fixture prefix mismatch') }
  if (lists[0].requestIndex <= reset.requestIndex) return result('unproven', 'Reset/live request provenance is missing or out of order')
  return result('bounded-cache-refresh', 'One fulfilled refresh of the independently corroborated retained first-page prefix', {
    runId, workspaceId, backingTotal: total, inferredRetainedCount: 200, offscreenModelObserved: false,
    executedResetMembership: reset.membership, reset: compact(reset.receipt), live: compact(lists[0]),
    interveningReceipts: intervening.length, accountedRequests: accounted.count, accountedBytes: accounted.bytes,
    liveStartIndex, liveEndIndex: liveStartIndex + accounted.count, requestsSucceeded: true })
}

export function liveHydrationAccepted(value) {
  if (value?.revision !== LIVE_OBSERVATION_REFRESH_CONTRACT.revision || value.passed !== true || value.wholeWorkspaceReload !== false
    || value.proofBasis !== LIVE_OBSERVATION_REFRESH_CONTRACT.retainedContextProof || typeof value.reason !== 'string' || !value.reason.length || value.reason.length > 256) return false
  const proof = value.proof
  const uuid = input => typeof input === 'string' && /^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/i.test(input)
  const integer = input => Number.isSafeInteger(input) && input >= 0
  const digest = input => typeof input === 'string' && /^[0-9a-f]{64}$/i.test(input)
  if (!proof || !uuid(proof.runId) || !uuid(proof.workspaceId) || !integer(proof.accountedRequests)
    || !integer(proof.accountedBytes) || !integer(proof.liveStartIndex) || !integer(proof.liveEndIndex)
    || proof.liveEndIndex !== proof.liveStartIndex + proof.accountedRequests || proof.requestsSucceeded !== true
    || (proof.accountedRequests > 0 && proof.accountedBytes <= 0) || (proof.accountedRequests === 0 && proof.accountedBytes !== 0)) return false
  const page = (receipt, count = 200, hasMore = true) => {
    if (!receipt || receipt.runId !== proof.runId || receipt.workspaceId !== proof.workspaceId || !integer(receipt.requestIndex)
      || receipt.requestId !== receipt.requestIndex + 1 || receipt.method !== 'GET' || receipt.status !== 200 || receipt.fulfilled !== true
      || receipt.count !== count || receipt.hasMore !== hasMore || !integer(receipt.bytes) || receipt.bytes <= 0 || !digest(receipt.prefixHash)
      || !uuid(receipt.firstId) || !uuid(receipt.lastId) || receipt.firstId === receipt.lastId) return false
    try {
      const url = new URL(receipt.url), keys = [...url.searchParams.keys()]
      return url.pathname === '/api/v1/observations' && keys.length === 3 && new Set(keys).size === 3
        && keys.every(key => ['workspace_id', 'limit', 'offset'].includes(key)) && url.searchParams.get('workspace_id') === proof.workspaceId
        && url.searchParams.get('limit') === '200' && url.searchParams.get('offset') === '0'
    } catch { return false }
  }
  if (value.classification === 'no-enumeration') {
    if (value.legacyWholeWorkspaceReload !== false || proof.enumerationRequests !== 0 || !integer(proof.observationEntityRequests) || proof.observationEntityRequests > proof.accountedRequests) return false
    if (!proof.observationEntityRequests) return proof.entities == null || (Array.isArray(proof.entities) && !proof.entities.length)
    if (!integer(proof.backingTotal) || proof.backingTotal < 1 || proof.inferredRetainedCount !== Math.min(200, proof.backingTotal)
      || proof.offscreenModelObserved !== false || proof.executedResetMembership?.executed !== true || proof.executedResetMembership.expected !== proof.inferredRetainedCount
      || !integer(proof.interveningReceipts) || !page(proof.reset, proof.inferredRetainedCount, proof.backingTotal > proof.inferredRetainedCount)
      || proof.reset.requestIndex + proof.interveningReceipts + 1 !== proof.liveStartIndex
      || !Array.isArray(proof.cacheIds) || proof.cacheIds.length !== proof.inferredRetainedCount || new Set(proof.cacheIds).size !== proof.cacheIds.length || !proof.cacheIds.every(uuid)
      || hashJson(proof.cacheIds) !== proof.reset.prefixHash || proof.cacheIds[0] !== proof.reset.firstId || proof.cacheIds.at(-1) !== proof.reset.lastId
      || !Array.isArray(proof.updatedIds) || !proof.updatedIds.every(uuid) || new Set(proof.updatedIds).size !== proof.updatedIds.length
      || !Array.isArray(proof.entities) || proof.entities.length !== proof.observationEntityRequests || proof.entities.length >= proof.cacheIds.length
      || new Set(proof.entities.map(entity => entity.id)).size !== proof.entities.length || new Set(proof.entities.map(entity => entity.requestIndex)).size !== proof.entities.length) return false
    let entityBytes = 0
    for (const entity of proof.entities) {
      if (!uuid(entity.id) || !proof.cacheIds.includes(entity.id) || !proof.updatedIds.includes(entity.id) || entity.runId !== proof.runId || entity.workspaceId !== proof.workspaceId
        || !integer(entity.requestIndex) || entity.requestId !== entity.requestIndex + 1 || entity.requestIndex < proof.liveStartIndex || entity.requestIndex >= proof.liveEndIndex
        || entity.method !== 'GET' || entity.status !== 200 || entity.fulfilled !== true || !integer(entity.bytes) || entity.bytes <= 0
        || !digest(entity.dtoHash) || !digest(entity.expectedDtoHash) || entity.dtoHash !== entity.expectedDtoHash) return false
      try { const url = new URL(entity.url); if (url.pathname !== '/api/v1/observations/' + entity.id || url.search) return false } catch { return false }
      entityBytes += entity.bytes
    }
    return integer(entityBytes) && entityBytes <= proof.accountedBytes
  }
  return value.classification === 'bounded-cache-refresh' && value.legacyWholeWorkspaceReload === true
    && proof.offscreenModelObserved === false && proof.inferredRetainedCount === 200 && integer(proof.backingTotal) && proof.backingTotal > 200
    && proof.executedResetMembership?.executed === true && proof.executedResetMembership.expected === 200
    && integer(proof.interveningReceipts) && proof.accountedRequests > 0 && proof.accountedRequests <= 12 && page(proof.reset) && page(proof.live)
    && proof.reset.requestIndex + proof.interveningReceipts + 1 === proof.liveStartIndex
    && proof.live.requestIndex >= proof.liveStartIndex && proof.live.requestIndex < proof.liveEndIndex
    && proof.live.bytes <= proof.accountedBytes && proof.reset.prefixHash === proof.live.prefixHash
    && proof.reset.firstId === proof.live.firstId && proof.reset.lastId === proof.live.lastId
}

export function aggregateLiveHydration(values) {
  return { passed: Array.isArray(values) && values.length === 5 && values.every(liveHydrationAccepted) }
}

export function perfApiRouteKey(input, method = 'GET') {
  const pathname = input instanceof URL ? input.pathname : new URL(input, 'http://perf.local').pathname
  const verb = method.toUpperCase()
  const is = (allowed, pattern) => allowed.includes(verb) && pattern.test(pathname)
  if (is(['GET'], /^\/api\/v1\/session$/)) return 'session'
  if (is(['POST'], /^\/api\/v1\/change-stream\/resync$/)) return 'change-stream:resync'
  if (is(['POST'], /^\/api\/v1\/change-stream\/ticket$/)) return 'change-stream:ticket'
  if (is(['GET'], /^\/api\/v1\/workspaces$/)) return 'workspaces:list'
  if (is(['GET'], /^\/api\/v1\/workspaces\/[^/]+\/planning-counts$/)) return 'workspaces:planning-counts'
  if (is(['GET'], /^\/api\/v1\/workspaces\/[^/]+\/timeline\/buckets$/)) return 'timeline:buckets'
  if (is(['GET'], /^\/api\/v1\/workspaces\/[^/]+\/timeline$/)) return 'timeline:events'
  if (is(['GET'], /^\/api\/v1\/workspaces\/[^/]+\/memberships$/)) return 'workspaces:memberships'
  if (is(['GET'], /^\/api\/v1\/workspaces\/[^/]+\/navigation\/focus\/(project|task)\/[^/]+$/)) return 'navigation:focus'
  if (is(['GET'], /^\/api\/v1\/workspaces\/[^/]+\/navigation$/)) return 'navigation:branch'
  if (is(['POST'], /^\/api\/v1\/workspaces\/[^/]+\/navigation\/summaries$/)) return 'navigation:summaries'
  if (is(['GET'], /^\/api\/v1\/workspaces\/[^/]+$/)) return 'workspaces:entity'
  if (is(['GET'], /^\/api\/v1\/projects$/)) return 'projects:list'
  if (is(['GET'], /^\/api\/v1\/projects\/[^/]+\/overview$/)) return 'projects:overview'
  if (is(['GET'], /^\/api\/v1\/projects\/[^/]+$/)) return `projects:entity:${pathname.split('/')[4]}`
  if (is(['GET'], /^\/api\/v1\/tasks$/)) return 'tasks:list'
  if (is(['GET'], /^\/api\/v1\/tasks\/[^/]+\/overview$/)) return 'tasks:overview'
  if (is(['GET'], /^\/api\/v1\/tasks\/[^/]+$/)) return `tasks:entity:${pathname.split('/')[4]}`
  if (is(['GET'], /^\/api\/v1\/observations\/subject-facets$/)) return 'observations:facets'
  if (is(['POST'], /^\/api\/v1\/observations\/match$/)) return 'observations:match'
  if (is(['POST'], /^\/api\/v1\/observations\/count$/)) return 'observations:count'
  if (is(['GET'], /^\/api\/v1\/observations$/)) return 'observations:list'
  if (is(['GET'], /^\/api\/v1\/observations\/[^/]+$/)) return `observations:entity:${pathname.split('/')[4]}`
  return null
}

export function representativeReadiness({ modelComplete, activeRequests, loading, focused, anchorVisible }) {
  return modelComplete === true && activeRequests === 0 && loading === false && focused === false && anchorVisible === true
}

export function transportContractReady({ protocol, transportedItems, expectedItems, fullBackingItems, transportedPages, expectedPages, complete }) {
  const validProtocolCardinality = protocol === 'current-full'
    ? expectedItems === fullBackingItems
    : protocol === 'bounded'
      ? expectedItems < fullBackingItems
      : false
  return Number.isInteger(transportedItems)
    && Number.isInteger(expectedItems)
    && Number.isInteger(fullBackingItems)
    && Number.isInteger(transportedPages)
    && Number.isInteger(expectedPages)
    && expectedItems > 0
    && fullBackingItems >= expectedItems
    && validProtocolCardinality
    && transportedItems === expectedItems
    && transportedPages === expectedPages
    && complete === true
}

export function renderBudgetEvaluation({ nodes, rows }, { maxDomNodes, maxCollectionRows }) {
  const nodesPass = Number.isInteger(nodes) && nodes >= 0 && nodes <= maxDomNodes
  const rowsPass = Number.isInteger(rows) && rows >= 0 && rows <= maxCollectionRows
  return { nodesPass, rowsPass, passed: nodesPass && rowsPass }
}

export function renderMaximum(states) {
  if (!Array.isArray(states) || states.length === 0 || states.some(state => !Number.isInteger(state?.nodes) || state.nodes < 0 || !Number.isInteger(state?.rows) || state.rows < 0)) {
    throw new Error('render maximum requires non-empty DOM/row measurements')
  }
  return {
    nodes: Math.max(...states.map(state => state.nodes)),
    rows: Math.max(...states.map(state => state.rows))
  }
}

export function liveTimingSummary({ startedAt, settledAt, stabilityStartedAt, stabilityEndedAt }) {
  const values = [startedAt, settledAt, stabilityStartedAt, stabilityEndedAt]
  if (values.some(value => !Number.isFinite(value)) || settledAt < startedAt || stabilityStartedAt < settledAt || stabilityEndedAt < stabilityStartedAt) {
    throw new Error('live timing boundaries must be finite and monotonic')
  }
  return { settleMs: settledAt - startedAt, stabilityMs: stabilityEndedAt - stabilityStartedAt }
}

export function liveSettleReady({ dispatchTurnComplete, activeRequests, loading, focused, anchorVisible, followUpRequests, logicalNavigationComplete }) {
  return logicalNavigationComplete === true
    && dispatchTurnComplete === true
    && Number.isInteger(followUpRequests) && followUpRequests >= 0
    && activeRequests === 0
    && loading === false
    && focused === false
    && anchorVisible === true
}

export function assertFiveSamples(values, label) {
  if (!Array.isArray(values) || values.length !== HARNESS_CONFIGURATION.samples || values.some(value => !Number.isFinite(value))) {
    throw new Error(`${label} must contain exactly ${HARNESS_CONFIGURATION.samples} finite samples`)
  }
  return values
}

export function assertCompleteNavigationStream(pages, expectedIds, label) {
  const active = pages.filter(page => page.limit > 0)
  if (!active.length) throw new Error(label + ' has no admitted stream page')
  const ids = []
  let terminal = false, terminalPage = null, echoes = 0
  for (const page of active) {
    if (terminal && page.done && page.limit === 50 && page.offset === terminalPage.offset && page.hasMore === false && JSON.stringify(page.ids) === JSON.stringify(terminalPage.ids)) { echoes += 1; continue }
    if (!page.done || terminal || page.limit !== 50 || page.offset !== ids.length) throw new Error(label + ' has unfinished, repeated, or out-of-order pages')
    const expected = expectedIds.slice(page.offset, page.offset + page.limit)
    if (JSON.stringify(page.ids) !== JSON.stringify(expected)) throw new Error(label + ' membership/order differs from authoritative children')
    const hasMore = page.offset + expected.length < expectedIds.length
    if (page.hasMore !== hasMore || (page.hasMore && page.ids.length !== 50)) throw new Error(label + ' terminal state differs from authoritative children')
    ids.push(...page.ids)
    terminal = !page.hasMore
    if (terminal) terminalPage = page
  }
  if (!terminal || JSON.stringify(ids) !== JSON.stringify(expectedIds)) throw new Error(label + ' lacks independent terminal membership')
  return { ids, terminal: true, pages: active.length - echoes, echoes }
}

// Distinct native observers may share a row. Disconnecting one must not hide
// another observer's still-live targets from the measured physical high-water.
export function createHierarchyObserverLedger() {
  const owners = new Map()
  const targets = new Map()
  let maximum = 0
  const remove = target => {
    const remaining = targets.get(target) - 1
    if (remaining === 0) targets.delete(target)
    else targets.set(target, remaining)
  }
  return {
    observe(owner, target) {
      if (!owners.has(owner)) owners.set(owner, new Set())
      const owned = owners.get(owner)
      if (!owned.has(target)) {
        owned.add(target)
        targets.set(target, (targets.get(target) || 0) + 1)
        maximum = Math.max(maximum, targets.size)
      }
    },
    unobserve(owner, target) {
      if (owners.get(owner)?.delete(target)) remove(target)
    },
    disconnect(owner) {
      for (const target of owners.get(owner) || []) remove(target)
      owners.delete(owner)
    },
    metrics() { return { current: targets.size, maximum } }
  }
}

export function assertNavigationCapacity({ aggregate, expanded, details }) {
  if (expanded > 4 || aggregate > 5 || details > 6) throw new Error('Physical admission exceeded four descendants, root-plus-four aggregate, or six details')
}

// Attempt every owned retirement even when an earlier close rejects or stalls.
// These receipts must precede any persisted success qualification.
export async function retireOwnedResources(actions, timeoutMs = 5000) {
  const receipts = []
  for (const { resource, close } of actions) {
    let timer
    const started = performance.now()
    try {
      if (typeof close !== 'function') throw new Error('owned retirement requires a callable close callback')
      await Promise.race([
        Promise.resolve().then(close),
        new Promise((_, reject) => { timer = setTimeout(() => reject(new Error('retirement exceeded ' + timeoutMs + 'ms')), timeoutMs) })
      ])
      receipts.push({ resource, passed: true, durationMs: Math.round(performance.now() - started) })
    } catch (error) {
      receipts.push({ resource, passed: false, durationMs: Math.round(performance.now() - started), error: error.message })
    } finally { clearTimeout(timer) }
  }
  return { passed: receipts.every(receipt => receipt.passed), boundPerResourceMs: timeoutMs, receipts }
}

export function currentNavigationPassComplete(pages, projects, tasks) {
  const starts = pages.map((page, index) => page.projectOffset === 0 && page.taskOffset === 0 ? index : -1).filter(index => index >= 0)
  if (!starts.length) return false
  const current = pages.slice(starts.at(-1))
  if (!current.length || current.some(page => !page.done || !page.projects || !page.tasks)) return false
  try {
    for (const [kind, expected] of [['project', projects], ['task', tasks]]) {
      assertCompleteNavigationStream(current.map(page => ({
        offset: page[kind + 'Offset'], limit: page[kind + 'Limit'],
        ids: page[kind + 's'].ids, hasMore: page[kind + 's'].hasMore, done: page.done
      })), expected, 'current ' + kind)
    }
    return true
  } catch { return false }
}

export function logicalExpandedNavigationComplete({ roots, children, passes, collapsed = [] }) {
  const skipped = new Set(collapsed), seen = new Set(), pending = [...roots]
  for (let index = 0; index < pending.length; index++) {
    const key = pending[index]
    if (seen.has(key) || skipped.has(key)) continue
    seen.add(key)
    const expected = children[key]
    if (!expected) return false
    if (!expected.projects.length && !expected.tasks.length) continue
    if (!currentNavigationPassComplete(passes[key] || [], expected.projects, expected.tasks)) return false
    pending.push(...expected.projects.map(id => 'project:' + id), ...expected.tasks.map(id => 'task:' + id))
  }
  return true
}

export function currentRootNavigationPass(pages) {
  const starts = pages.map((page, index) => page.projectOffset === 0 && page.taskOffset === 0 ? index : -1).filter(index => index >= 0)
  if (!starts.length) return null
  const demanded = { project: new Set(), task: new Set() }
  for (const page of pages) for (const kind of ['project', 'task']) {
    if (page[kind + 'Limit'] > 0) demanded[kind].add(page[kind + 'Offset'])
  }
  const current = pages.slice(starts.at(-1)), coverage = { project: [], task: [] }
  for (const kind of ['project', 'task']) for (const offset of demanded[kind]) {
    const page = current.findLast(value => value[kind + 'Limit'] > 0 && value[kind + 'Offset'] === offset)
    if (!page) {
      const exhausted = current.some(value => {
        const stream = value[kind + 's']
        return value.done && value[kind + 'Limit'] > 0 && value[kind + 'Offset'] < offset
          && stream && (stream.has_more === false || stream.hasMore === false)
          && value[kind + 'Offset'] + (stream.items || stream.ids || []).length <= offset
      })
      if (exhausted) continue
      return null
    }
    if (!page.done || !page[kind + 's']) return null
    coverage[kind].push(page)
  }
  return coverage
}

export function createNavigationCompletionIndex({ children, rootProjects, rootTasks, context }) {
  let epoch = 0, authorized = false, activeContext = null, rootEpoch = 0, revision = 0, checkedRevision = -1, cachedReady = false
  let owners = new Map(), rootCoverage = { project: new Map(), task: new Map() }, demand = { project: new Set(), task: new Set() }
  let rootAccepted = false, authoritativeRootRefresh = false
  let rootResult = null, collapsed = new Set(), subtree = new Map(), parents = new Map()
  let snapshotEpoch = 0, acceptedSnapshotEpoch = -1, hasAcceptedSnapshot = false, retryIntents = new Map(), intentEpoch = 0
  const armedSelections = new WeakSet()
  const relink = () => {
    parents = new Map()
    for (const [key, value] of Object.entries(children)) for (const child of [...value.projects.map(id => 'project:' + id), ...value.tasks.map(id => 'task:' + id)]) {
      if (!parents.has(child)) parents.set(child, new Set())
      parents.get(child).add(key)
    }
  }
  relink()
  const dirty = key => {
    const pending = [key], seen = new Set()
    while (pending.length) {
      const current = pending.pop()
      if (seen.has(current)) continue
      seen.add(current); subtree.delete(current)
      pending.push(...(parents.get(current) || []))
    }
    revision++
  }
  const resetContext = next => {
    activeContext = next; owners = new Map(); subtree = new Map(); retryIntents.clear(); intentEpoch++
    rootCoverage = { project: new Map(), task: new Map() }; demand = { project: new Set(), task: new Set() }
    rootEpoch++; rootAccepted = false; authoritativeRootRefresh = false; rootResult = null; revision++
  }
  const ownerComplete = key => {
    const expected = children[key], state = owners.get(key)
    if (!expected || !state) return false
    if (state.dirty) {
      state.complete = !state.unknown && !Object.values(state.kinds).some(kind => kind.paused || kind.pending)
      try {
        for (const kind of ['project', 'task']) {
          const pages = [...state.coverage[kind].values()].sort((a, b) => a.offset - b.offset)
          if (!pages.length || pages.some(page => !page.done || page.error || !page.stream)) throw new Error('pending or failed kind')
          assertCompleteNavigationStream(pages.map(page => ({ offset: page.offset, limit: page.limit, ids: page.stream.ids, hasMore: page.stream.hasMore, done: page.done })), expected[kind + 's'], 'indexed ' + kind)
        }
      } catch { state.complete = false }
      state.dirty = false
    }
    return state.complete
  }
  const nodeComplete = (key, visiting = new Set()) => {
    if (collapsed.has(key)) return true
    if (subtree.has(key)) return subtree.get(key)
    const expected = children[key]
    if (!expected || visiting.has(key)) return false
    if (!expected.projects.length && !expected.tasks.length) { subtree.set(key, true); return true }
    if (!ownerComplete(key)) { subtree.set(key, false); return false }
    visiting.add(key)
    const complete = [...expected.projects.map(id => 'project:' + id), ...expected.tasks.map(id => 'task:' + id)].every(child => nodeComplete(child, visiting))
    visiting.delete(key); subtree.set(key, complete)
    return complete
  }
  const currentRoots = () => {
    if (rootResult !== null) return rootResult
    const roots = []
    if (!demand.project.size && !demand.task.size) return false
    const state = owners.get('workspace_root')
    if (!state || state.unknown || Object.values(state.kinds).some(kind => kind.paused || kind.pending)) return false
    for (const [kind, expected] of [['project', rootProjects], ['task', rootTasks]]) for (const offset of demand[kind]) {
      const page = rootCoverage[kind].get(offset)
      if (!page) {
        const exhausted = [...rootCoverage[kind].values()].some(value => {
          const stream = value.stream
          return value.done && !value.error && stream && stream.hasMore === false && value.offset < offset
            && value.offset + stream.ids.length <= offset
        })
        if (exhausted) continue
        return false
      }
      const stream = page.stream, wanted = expected.slice(offset, offset + page.limit)
      if (!page.done || page.error || !stream || page.limit !== 50
        || JSON.stringify(stream.ids) !== JSON.stringify(wanted) || stream.hasMore !== (offset + 50 < expected.length)) return false
      roots.push(...stream.ids.map(id => kind + ':' + id))
    }
    rootResult = roots
    return roots
  }
  const retryOwnerQuiet = selection => {
    const state = owners.get(selection.owner)
    return authorized && activeContext === context && selection.epoch === epoch && selection.intentEpoch === intentEpoch
      && selection.state === state && state?.latest?.done && !Object.values(state.kinds).some(kind => kind.pending)
      && !(snapshotEpoch > 0 && snapshotEpoch !== acceptedSnapshotEpoch)
  }
  return {
    beginSession() { epoch++; authorized = false; collapsed = new Set(); snapshotEpoch = 0; acceptedSnapshotEpoch = -1; hasAcceptedSnapshot = false; resetContext(null); return epoch },
    admitSnapshot(firstPage) { if (firstPage) { snapshotEpoch++; retryIntents.clear(); intentEpoch++ } return { epoch, snapshotEpoch } },
    completeSnapshot(stamp, profile = 'workspace_shell_v1') {
      if (!stamp || stamp.epoch !== epoch || stamp.snapshotEpoch !== snapshotEpoch || acceptedSnapshotEpoch === stamp.snapshotEpoch) return
      acceptedSnapshotEpoch = stamp.snapshotEpoch
      const initialShell = !hasAcceptedSnapshot && profile === 'workspace_shell_v1'
      hasAcceptedSnapshot = true
      if (initialShell) return
      // A later accepted authoritative snapshot revalidates every expanded owner.
      // Preserve manual root demand and effective collapse, but retire old callbacks.
      owners = new Map(); subtree = new Map(); retryIntents.clear(); intentEpoch++; rootEpoch++; authoritativeRootRefresh = true
      rootCoverage = { project: new Map(), task: new Map() }; rootResult = null; revision++
    },
    completeSession(stamp, canRead) { if (stamp !== epoch) return; authorized = canRead === true; revision++ },
    admit(request) {
      const stamp = { ...request, epoch, done: false, error: false, projects: null, tasks: null }
      const requested = kind => request[kind + 'Included'] !== false
      if (request.owner === 'workspace_root' && request.context !== activeContext) resetContext(request.context)
      if (request.context !== activeContext) { stamp.retired = true; return stamp }
      if (request.owner === 'workspace_root') {
        let state = owners.get(request.owner)
        const intent = retryIntents.get(request.owner)
        retryIntents.clear(); intentEpoch++
        const selected = intent && intent.epoch === epoch && intent.context === activeContext && intent.state === state
          && intent.projectOffset === request.projectOffset && intent.taskOffset === request.taskOffset ? intent.kind : null
        const zero = request.projectOffset === 0 && request.taskOffset === 0
        // An unstamped zero while paused is ambiguous with a selected-kind retry.
        // Session/filter/accepted authoritative snapshot retirement removes state.
        if (!state || (!selected && zero && !Object.values(state.kinds).some(kind => kind.paused))) {
          rootEpoch++; rootCoverage = { project: new Map(), task: new Map() }
          state = { refreshing: rootAccepted || authoritativeRootRefresh, wholeErrorMask: null, kinds: { project: { offset: 0, pending: true, paused: false, more: true }, task: { offset: 0, pending: true, paused: false, more: true } }, latest: null, unknown: false }
          owners.set(request.owner, state)
        }
        stamp.rootEpoch = rootEpoch; stamp.state = state; state.latest = stamp; stamp.slots = {}
        const automatic = ['project', 'task'].filter(kind => state.kinds[kind].pending && state.kinds[kind].offset === request[kind + 'Offset'])
        const manual = !selected && !automatic.length ? ['project', 'task'].filter(kind => !state.kinds[kind].paused && request[kind + 'Offset'] > state.kinds[kind].offset) : []
        const retryMask = selected && state.wholeErrorMask && !Object.values(state.kinds).some(kind => kind.pauseReason === 'stream') ? state.wholeErrorMask : null
        const eligible = (selected ? retryMask || [selected] : automatic.length ? automatic : manual).filter(requested)
        if (eligible.length) state.wholeErrorMask = null
        state.unknown = eligible.length === 0
        for (const kind of eligible) if (request[kind + 'Limit'] > 0) {
          const slot = { offset: request[kind + 'Offset'], limit: request[kind + 'Limit'], done: false, error: false, stream: null }
          demand[kind].add(slot.offset); rootCoverage[kind].set(slot.offset, slot); stamp.slots[kind] = slot
        }
        rootResult = null; revision++
      } else {
        let state = owners.get(request.owner)
        const intent = retryIntents.get(request.owner)
        retryIntents.delete(request.owner)
        const selected = intent && intent.epoch === epoch && intent.context === activeContext && intent.state === state
          && intent.projectOffset === request.projectOffset && intent.taskOffset === request.taskOffset ? intent.kind : null
        if (!state || (!selected && request.projectOffset === 0 && request.taskOffset === 0)) {
          state = { coverage: { project: new Map(), task: new Map() }, kinds: { project: { offset: 0, pending: true, paused: false }, task: { offset: 0, pending: true, paused: false } }, dirty: true, complete: false, latest: null, unknown: false }
          owners.set(request.owner, state)
        }
        stamp.state = state; state.latest = stamp; stamp.slots = {}
        const automatic = ['project', 'task'].filter(kind => state.kinds[kind].pending && state.kinds[kind].offset === request[kind + 'Offset'])
        const eligible = (selected ? [...new Set([selected, ...automatic])] : automatic).filter(requested)
        state.unknown = eligible.length === 0
        for (const kind of eligible) {
          const slot = { offset: stamp[kind + 'Offset'], limit: stamp[kind + 'Limit'], done: false, error: false, stream: null }
          stamp.slots[kind] = slot; state.coverage[kind].set(slot.offset, slot)
        }
        state.dirty = true; dirty(request.owner)
      }
      return stamp
    },
    complete(stamp, streams, status = 200) {
      if (stamp.retired || stamp.epoch !== epoch || stamp.context !== activeContext) return
      if (stamp.owner === 'workspace_root' ? stamp.rootEpoch !== rootEpoch || owners.get(stamp.owner) !== stamp.state || stamp.state.latest !== stamp : owners.get(stamp.owner) !== stamp.state || stamp.state.latest !== stamp) return
      if (stamp.done) return
      stamp.done = true; stamp.error = status >= 400 || !streams
      stamp.projects = streams?.projects || null; stamp.tasks = streams?.tasks || null
      if (stamp.owner === 'workspace_root') {
        const state = stamp.state
        for (const [kind, slot] of Object.entries(stamp.slots)) {
          const stream = streams?.[kind + 's'], expected = kind === 'project' ? rootProjects : rootTasks
          const valid = !stamp.error && stream && slot.limit === 50
            && JSON.stringify(stream.ids) === JSON.stringify(expected.slice(slot.offset, slot.offset + slot.limit))
            && stream.hasMore === (slot.offset + 50 < expected.length)
          slot.done = true; slot.error = !valid; slot.stream = stream || null
          const pending = !!(state.refreshing && valid && stream.hasMore && [...demand[kind]].some(offset => offset >= slot.offset + stream.ids.length))
          state.kinds[kind] = { offset: slot.offset + (pending ? 50 : 0), pending, paused: !valid, pauseReason: valid ? null : stamp.error ? 'http' : 'stream', more: !!stream?.hasMore }
        }
        if (stamp.error) {
          state.wholeErrorMask = Object.keys(stamp.slots)
          for (const kind of ['project', 'task']) state.kinds[kind].pending = false
        } else rootAccepted = true
        rootResult = null; revision++
      }
      else {
        const state = stamp.state, expected = children[stamp.owner]
        for (const [kind, slot] of Object.entries(stamp.slots)) {
          const stream = streams?.[kind + 's'], wanted = expected?.[kind + 's']?.slice(slot.offset, slot.offset + slot.limit)
          const valid = !stamp.error && stream && slot.limit === 50 && wanted
            && JSON.stringify(stream.ids) === JSON.stringify(wanted) && stream.hasMore === (slot.offset + 50 < expected[kind + 's'].length)
          slot.done = true; slot.error = !valid; slot.stream = stream || null
          state.kinds[kind] = { offset: slot.offset + (valid && stream.hasMore ? 50 : 0), pending: !!(valid && stream.hasMore), paused: !valid }
        }
        if (stamp.error) for (const kind of ['project', 'task']) state.kinds[kind].pending = false
        state.dirty = true; dirty(stamp.owner)
      }
    },
    retrySelection(owner, kind) {
      const state = owners.get(owner)
      if (!authorized || activeContext !== context || !state || !state.kinds[kind]?.paused || !state.latest?.done) throw new Error('Retry selection has no current paused owner/kind')
      return { owner, kind, epoch, intentEpoch, context: activeContext, state, projectOffset: state.kinds.project.offset, taskOffset: state.kinds.task.offset,
        buttonIndex: ['project', 'task'].filter(value => state.kinds[value].paused).indexOf(kind) }
    },
    retryOwnerQuiet,
    armRetry(selection, requireQuiet = false) {
      if (requireQuiet && !retryOwnerQuiet(selection)) throw new Error('Retry owner is unsettled before click')
      const state = owners.get(selection.owner)
      if (!authorized || activeContext !== context || armedSelections.has(selection) || selection.intentEpoch !== intentEpoch || selection.epoch !== epoch || selection.context !== activeContext || selection.state !== state || !state?.kinds[selection.kind]?.paused
        || selection.projectOffset !== state.kinds.project.offset || selection.taskOffset !== state.kinds.task.offset) throw new Error('Retry selection retired before dispatch')
      armedSelections.add(selection); retryIntents.set(selection.owner, selection); return selection
    },
    cancelRetry(selection) { if (retryIntents.get(selection.owner) === selection) retryIntents.delete(selection.owner) },
    setCollapsed(keys) {
      const next = new Set(keys)
      if ([...new Set([...collapsed, ...next])].some(key => collapsed.has(key) !== next.has(key))) { retryIntents.clear(); intentEpoch++ }
      for (const key of new Set([...collapsed, ...next])) if (collapsed.has(key) !== next.has(key)) dirty(key)
      collapsed = next
    },
    replaceChildren(key, value) {
      // Dirty the old ancestry before relinking, then the new ancestry.
      retryIntents.clear(); intentEpoch++; dirty(key); children = { ...children, [key]: value }; relink(); dirty(key)
      const state = owners.get(key); if (state) state.dirty = true
    },
    ready() {
      if (checkedRevision === revision) return cachedReady
      checkedRevision = revision
      if (!authorized || activeContext !== context) { cachedReady = false; return false }
      const roots = currentRoots()
      cachedReady = Array.isArray(roots) && roots.every(key => nodeComplete(key))
      return cachedReady
    }
  }
}

// Same-browser-clock response/paint comparison. A 1 ms allowance for each
// Chromium reading exceeds its supported 100 µs TimeClamper granularity.
// Boundary uncertainty fails closed; it adds no wait and changes no accounting cut.
export function createColdDiagnostics() {
  const histories = Object.fromEntries(['arrivals', 'completions', 'evaluations'].map(kind => [kind, { total: 0, first: null, decisive: null, latest: [] }]))
  let cut = null
  const record = (kind, value, decisive = false) => {
    if (cut) return
    const history = histories[kind], entry = { index: ++history.total, ...value }
    if (!history.first) history.first = entry
    if (decisive && !history.decisive) history.decisive = entry
    history.latest.push(entry)
    if (history.latest.length > (kind === 'evaluations' ? 4 : 8)) history.latest.shift()
  }
  return {
    arrival: value => record('arrivals', value),
    completion: value => record('completions', value),
    evaluation: value => record('evaluations', value, value.ready),
    cut(at, requestCount, completedCount, activeCount) { if (!cut) cut = { at, requestCount, completedCount, activeCount } },
    snapshot() {
      return { schemaVersion: 1, clocks: { host: 'Node performance.now; same run only', browser: 'page performance.now; compare resource responseEnd only within its evaluation' },
        paintAcknowledgements: 'not observed; no nonce/ACK inference from DOM stamps', cut,
        histories: Object.fromEntries(Object.entries(histories).map(([kind, history]) => {
          const retained = new Map([history.first, history.decisive, ...history.latest].filter(Boolean).map(entry => [entry.index, entry]))
          return [kind, { total: history.total, omitted: history.total - retained.size, firstIndex: history.first?.index ?? null,
            decisiveIndex: history.decisive?.index ?? null, latestIndex: history.latest.at(-1)?.index ?? null,
            retained: [...retained.values()].sort((a, b) => a.index - b.index) }]
        })) }
    }
  }
}

export function createUsablePaintReadiness(workspaceId, expectedItems, onDecision = null) {
  let sessionEpoch = 0, snapshotEpoch = 0, session = null, snapshot = null
  const time = at => Number.isFinite(at) && at >= 0
  const lifetime = () => ({ workspaceId, sessionEpoch, snapshotEpoch })
  const receiptFailure = (receipt, observation) => {
    if (!receipt || typeof receipt.receiptId !== 'string' || !receipt.receiptId || !Array.isArray(observation.receipts)) return 'missing-receipt'
    let expectedUrl
    try {
      expectedUrl = new URL(receipt.url)
      if (expectedUrl.origin !== observation.origin) return 'foreign-origin'
    } catch { return 'invalid-url' }
    const matches = observation.receipts.filter(entry => entry?.id === receipt.receiptId)
    if (matches.length !== 1) return matches.length ? 'duplicate-timing' : 'missing-timing'
    const entry = matches[0]
    if (entry.url !== expectedUrl.href) return 'unmatched-url'
    if (!time(entry.startTime) || !time(entry.responseEnd) || entry.responseEnd <= 0 || entry.startTime > entry.responseEnd) return 'invalid-timing'
    return entry.responseEnd <= observation.clock.frameAt - 2 ? null : 'late-or-uncertain-timing'
  }
  return {
    lifetime,
    beginSession(at, receiptId, url) {
      sessionEpoch++; snapshotEpoch = 0; snapshot = null
      session = { at, receiptId, url, completeAt: null, canRead: false, workspaceId: null }
      return { sessionEpoch, receiptId }
    },
    completeSession(stamp, value, at) {
      if (stamp?.sessionEpoch !== sessionEpoch || stamp.receiptId !== session?.receiptId || !session || !time(at) || !time(session.at) || at < session.at) return
      session = { ...session, completeAt: at, canRead: value?.canRead === true, workspaceId: value?.workspaceId }
    },
    admitSnapshot(firstPage, at, receiptId, url) {
      if (firstPage) { snapshotEpoch++; snapshot = { at, receiptId, url, completeAt: null, items: 0, pages: 0, profile: null, workspaceId: null, invalid: false } }
      else if (snapshot) snapshot.invalid = true
      return { sessionEpoch, snapshotEpoch, receiptId }
    },
    completeSnapshot(stamp, value, at) {
      if (stamp?.sessionEpoch !== sessionEpoch || stamp?.snapshotEpoch !== snapshotEpoch || stamp.receiptId !== snapshot?.receiptId || !snapshot
        || !time(at) || !time(snapshot.at) || at < snapshot.at) return
      const invalid = snapshot.invalid || (snapshot.profile !== null && snapshot.profile !== value.profile)
        || (snapshot.workspaceId !== null && value.workspaceId != null && snapshot.workspaceId !== value.workspaceId)
        || !Number.isInteger(value.items) || value.items < 0
      snapshot = { ...snapshot, invalid, profile: value.profile, workspaceId: value.workspaceId ?? snapshot.workspaceId,
        items: snapshot.items + value.items, pages: snapshot.pages + 1, completeAt: value.complete === true ? at : null }
    },
    readyAt(observedLifetime, observation) {
      const clock = observation?.clock
      const finish = reason => {
        if (onDecision) {
          const ids = [session?.receiptId, snapshot?.receiptId]
          const receipts = Array.isArray(observation?.receipts) ? observation.receipts : []
          const matched = receipts.filter(entry => ids.includes(entry?.id)).slice(0, 8)
          onDecision({ ready: reason === null, reason: reason || 'accepted', observedLifetime, currentLifetime: lifetime(),
            session: session && { ...session }, snapshot: snapshot && { ...snapshot },
            observation: { painted: observation?.painted, origin: observation?.origin, clock,
              receipts: matched, receiptCount: receipts.length, omittedReceipts: receipts.length - matched.length } })
        }
        return reason === null
      }
      if (![clock?.startedAt, clock?.frameAt, clock?.endedAt].every(time) || clock.startedAt > clock.frameAt || clock.frameAt > clock.endedAt) return finish('invalid-clock')
      if (observedLifetime?.workspaceId !== workspaceId || observedLifetime?.sessionEpoch !== sessionEpoch || observedLifetime?.snapshotEpoch !== snapshotEpoch) return finish('retired-lifetime')
      if (!session?.canRead || session.workspaceId !== workspaceId || !time(session.completeAt)) return finish('session-metadata')
      const sessionFailure = receiptFailure(session, observation)
      if (sessionFailure) return finish('session-' + sessionFailure)
      if (!snapshot || snapshot.invalid || snapshot.workspaceId !== workspaceId || snapshot.profile !== 'workspace_shell_v1'
        || snapshot.items !== expectedItems || snapshot.pages !== 1 || !time(snapshot.completeAt)) return finish('snapshot-metadata')
      const snapshotFailure = receiptFailure(snapshot, observation)
      return finish(snapshotFailure ? 'snapshot-' + snapshotFailure : null)
    }
  }
}

export const EVIDENCE_OUTPUT_LIMITS = Object.freeze({
  commandBytes: 32 * 1024 * 1024, totalBytes: 128 * 1024 * 1024,
  commandTimeoutMs: 30000, totalTimeoutMs: 60000
})

// Every subprocess is joined by spawnSync; no-index exit 1 is a valid diff only
// when the child returned complete bounded binary output without an error.
export function createEvidenceCapture({ cwd, limits = EVIDENCE_OUTPUT_LIMITS, run = spawnSync, now = () => performance.now() }) {
  for (const key of Object.keys(EVIDENCE_OUTPUT_LIMITS)) {
    if (!Number.isSafeInteger(limits[key]) || limits[key] <= 0) throw new Error('Invalid evidence limit: ' + key)
  }
  const started = now()
  let used = 0
  return {
    read(args, { label = args.join(' '), acceptedExitCodes = [0] } = {}) {
      const remainingMs = Math.floor(limits.totalTimeoutMs - (now() - started))
      const remainingBytes = Math.min(limits.commandBytes, limits.totalBytes - used)
      if (remainingMs <= 0 || remainingBytes <= 0) throw new Error('Evidence collection limit reached before ' + label)
      const result = run('git', args, { cwd, encoding: null, stdio: ['ignore', 'pipe', 'pipe'], windowsHide: true,
        maxBuffer: remainingBytes, timeout: Math.min(limits.commandTimeoutMs, remainingMs), killSignal: 'SIGKILL' })
      const stdout = result.stdout, stderr = result.stderr
      const byteCount = Buffer.isBuffer(stdout) && Buffer.isBuffer(stderr) ? stdout.length + stderr.length : null
      if (result.error || result.signal || !acceptedExitCodes.includes(result.status) || byteCount === null
        || byteCount > remainingBytes || (result.status === 1 && stdout.length === 0)
        || now() - started > limits.totalTimeoutMs) {
        throw new Error('Evidence command failed: ' + label + '; status=' + result.status + '; signal=' + result.signal
          + '; bytes=' + byteCount + '/' + remainingBytes + '; timeoutMs=' + Math.min(limits.commandTimeoutMs, remainingMs)
          + '; ' + (result.error?.code || result.error?.message || 'incomplete or unsuccessful output'), { cause: result.error })
      }
      used += byteCount
      return stdout
    },
    bytes: () => used
  }
}

export function atomicEvidenceWrite(file, bytes, { write = fs.writeFileSync, rename = fs.renameSync, remove = fs.unlinkSync } = {}) {
  const temporary = file + '.' + randomUUID() + '.tmp'
  try { write(temporary, bytes, { flag: 'wx' }); rename(temporary, file) }
  finally {
    try { remove(temporary) } catch (error) { if (error.code !== 'ENOENT') throw error }
  }
}

// Invalidate success before collection/writes. On any partial failure retain
// explicit failure receipts, or remove old success if the filesystem rejects them.
export function persistEvidenceAttempt({ manifestPath, validationPath, failureValidation, failureManifest,
  write = atomicEvidenceWrite, remove = fs.unlinkSync, verify = () => {} }, action) {
  const discard = file => { verify(); try { remove(file) } catch (error) { if (error.code !== 'ENOENT') throw error } }
  try {
    discard(manifestPath)
    return action()
  } catch (error) {
    // An ownership failure forbids even failure-receipt fallback persistence.
    verify()
    const failures = []
    for (const file of [manifestPath, validationPath]) {
      verify()
      try { discard(file) } catch (failure) { failures.push(failure) }
    }
    for (const [file, receipt] of [[validationPath, failureValidation], [manifestPath, failureManifest]]) {
      verify()
      try { write(file, JSON.stringify(receipt(error), null, 2) + '\n') } catch (failure) { failures.push(failure) }
    }
    if (failures.length) throw new Error(error.message + '; failure receipt persistence failed: ' + failures.map(f => f.message).join('; '), { cause: error })
    throw error
  }
}
