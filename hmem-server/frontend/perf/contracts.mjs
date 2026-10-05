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
        const eligible = selected ? retryMask || [selected] : automatic.length ? automatic : manual
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
        const eligible = selected ? [...new Set([selected, ...automatic])] : automatic
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
