import { createHash } from 'node:crypto'

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
  scenarioIsolation: 'cold/tab/live measurements use production workspace_shell_v1 plus one bounded root-navigation page with deterministic unfiltered/unfocused restoration; direct focus uses a fresh focus-first shell session; rendered cardinality is measured, never a readiness prerequisite',
  initialTransport: 'production workspace bootstrap explicitly requests workspace_shell_v1, transports only its authorized workspace root, then loads one capped navigation branch; the immutable full snapshot remains a legacy compatibility fixture only',
  routeFidelity: 'snapshot kind-rank/identity ordering; capped list pagination; Project/Task DTO and overview ordering; Observation simple-lexeme AND/rank/subject/facet ordering; Timeline event ordering and day/week/month/quarter UTC bucket range aggregation/caps are frozen against production source anchors',
  directFocus: 'navigate a project focus first from an empty shell state; observe one bounded navigation-focus lookup and render for 750ms without requiring legacy full resync',
  liveTiming: 'settle ends after the dispatched checkpoint turn, completion of every follow-up that actually started (zero is valid), transport idle, representative UI anchor, and two paints; a separate 500ms stability window must observe no late request',
  heapPoint: 'after the 50-frame live batch settles and its untimed stability assertion, Projects is unfiltered and unfocused with complete intercepted-model transport and representative anchors, then Chromium garbage collection runs'
})

export function hashJson(value) {
  return createHash('sha256').update(JSON.stringify(value)).digest('hex')
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

export function perfApiRouteKey(input, method = 'GET') {
  const pathname = input instanceof URL ? input.pathname : new URL(input, 'http://perf.local').pathname
  const verb = method.toUpperCase()
  const is = (allowed, pattern) => allowed.includes(verb) && pattern.test(pathname)
  if (is(['GET'], /^\/api\/v1\/session$/)) return 'session'
  if (is(['POST'], /^\/api\/v1\/change-stream\/resync$/)) return 'change-stream:resync'
  if (is(['POST'], /^\/api\/v1\/change-stream\/ticket$/)) return 'change-stream:ticket'
  if (is(['GET'], /^\/api\/v1\/workspaces$/)) return 'workspaces:list'
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

export function liveSettleReady({ dispatchTurnComplete, activeRequests, loading, focused, anchorVisible, followUpRequests }) {
  return dispatchTurnComplete === true
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
