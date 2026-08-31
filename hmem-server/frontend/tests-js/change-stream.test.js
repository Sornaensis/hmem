import assert from 'node:assert/strict'
import test from 'node:test'
import { canonicalEndpoint, canonicalTicketBody, clearCheckpoint, createCanonicalFrameBatcher, createChangeStreamManager, opaqueAudienceKey, readCheckpoint, scopeStorageKey, startIdempotencyKey, writeCheckpoint } from '../src/change-stream.js'

function storage() { const values = new Map(); return { getItem: key => values.get(key) || null, setItem: (key, value) => values.set(key, value), removeItem: key => values.delete(key) } }

test('storage is audience-scope keyed and corruption is absent', () => {
  const s = storage(); assert.notEqual(scopeStorageKey('actor-a', 'workspace', 'a'), scopeStorageKey('actor-a', 'workspace', 'b'))
  assert.notEqual(scopeStorageKey('actor-a', 'workspace', 'a'), scopeStorageKey('actor-b', 'workspace', 'a'))
  assert.doesNotMatch(scopeStorageKey('actor-a', 'workspace', 'a'), /actor-a/)
  assert.match(opaqueAudienceKey('actor-a'), /^[0-9a-f]{32}$/)
  writeCheckpoint(s, 'actor-a', 'workspace', 'a', 'token-a', ['1', '1']); assert.deepEqual(readCheckpoint(s, 'actor-a', 'workspace', 'a'), { resumeToken: 'token-a', eventIds: ['1'] })
  s.setItem(scopeStorageKey('actor-a', 'workspace', 'b'), '{bad'); assert.equal(readCheckpoint(s, 'actor-a', 'workspace', 'b'), null)
  clearCheckpoint(s, 'actor-a', 'workspace', 'a'); assert.equal(readCheckpoint(s, 'actor-a', 'workspace', 'a'), null)
  writeCheckpoint(s, 'actor-a', 'workspace', 'a', 'bounded', Array.from({ length: 140 }, (_, index) => `event-${index}`))
  assert.equal(readCheckpoint(s, 'actor-a', 'workspace', 'a').eventIds.length, 128)
})

test('canonical ticket shape never uses legacy workspace_id root field', () => {
  assert.deepEqual(canonicalTicketBody('workspace', 'w', 'resume'), { scope: { scope: 'workspace', workspace_id: 'w' }, resume_token: 'resume' })
  assert.deepEqual(canonicalTicketBody('global', null, 'resume'), { scope: { scope: 'global' }, resume_token: 'resume' })
})

test('start key uses CSPRNG material and has at least 32 characters', () => {
  let call = 0; const key = startIdempotencyKey({ getRandomValues: bytes => { call += 1; bytes.fill(9) } }); assert.equal(call, 1); assert.ok(key.length >= 32); assert.match(key, /^[0-9a-f]+$/)
})

test('scopes retain independent sockets and reconnect uses latest checkpoint', async () => {
  const s = storage(); const requests = []; const sockets = []
  class FakeSocket { constructor(url) { this.url = url; sockets.push(this); queueMicrotask(() => this.onopen()) } close() {} }
  const manager = createChangeStreamManager({ apiUrl: 'https://api.example', wsUrl: 'wss://ws.example/ws', storage: s, WebSocketImpl: FakeSocket, fetchImpl: async (_url, options) => { requests.push(JSON.parse(options.body)); return { ok: true, status: 200, json: async () => ({ ticket: `t${requests.length}` }) } }, setTimer: () => 0, clearTimer: () => {} })
  manager.connect({ audienceId: 'actor', scope: 'workspace', workspaceId: 'a', resumeToken: 'one' }); manager.connect({ audienceId: 'actor', scope: 'workspace', workspaceId: 'b', resumeToken: 'two' }); await new Promise(resolve => setTimeout(resolve, 0))
  assert.equal(sockets.length, 2); manager.connect({ audienceId: 'actor', scope: 'workspace', workspaceId: 'a' }); await new Promise(resolve => setTimeout(resolve, 0)); assert.equal(sockets.length, 2)
  manager.checkpoint('workspace', 'a', 'three', ['event']); manager.disconnectAll(); assert.deepEqual(requests.map(request => request.scope.workspace_id), ['a', 'b']); assert.deepEqual(readCheckpoint(s, 'actor', 'workspace', 'a'), { resumeToken: 'three', eventIds: ['event'] })
})

test('socket close automatically reconnects with the latest checkpoint and a fresh one-use ticket', async () => {
  const s = storage(); const requests = []; const sockets = []; const timers = []
  class FakeSocket { constructor(url) { this.url = url; sockets.push(this); queueMicrotask(() => this.onopen()) } close() {} }
  const manager = createChangeStreamManager({
    apiUrl: 'https://api.example', wsUrl: 'wss://ws.example/ws', storage: s, WebSocketImpl: FakeSocket,
    fetchImpl: async (_url, options) => {
      requests.push(JSON.parse(options.body))
      return { ok: true, status: 200, json: async () => ({ ticket: `one-use-${requests.length}` }) }
    },
    setTimer: (fn, milliseconds) => { timers.push({ fn, milliseconds }); return timers.length },
    clearTimer: () => {}
  })
  manager.connect({ audienceId: 'actor', scope: 'workspace', workspaceId: 'a', resumeToken: 'initial-token' })
  await new Promise(resolve => setTimeout(resolve, 0))
  sockets[0].onmessage({ data: JSON.stringify({ schema_version: 1, type: 'checkpoint', catch_up: 'complete', resume_token: 'latest-token' }) })
  sockets[0].onclose()
  assert.equal(timers.length, 1)
  assert.equal(timers[0].milliseconds, 500)
  timers[0].fn()
  await new Promise(resolve => setTimeout(resolve, 0))
  assert.deepEqual(requests.map(request => request.resume_token), ['initial-token', 'latest-token'])
  assert.deepEqual(sockets.map(socket => new URL(socket.url).searchParams.get('ticket')), ['one-use-1', 'one-use-2'])
})

test('resync accumulates every page before ticketing and keeps snapshot material out of storage', async () => {
  const s = storage(); const requests = []; const snapshots = []; const sockets = []
  writeCheckpoint(s, 'actor', 'workspace', 'a', 'stale-model-token', ['stale-event'])
  class FakeSocket { constructor(url) { this.url = url; sockets.push(this); queueMicrotask(() => this.onopen()) } close() {} }
  const manager = createChangeStreamManager({
    apiUrl: 'https://api.example', wsUrl: 'wss://ws.example/ws', storage: s, WebSocketImpl: FakeSocket,
    onSnapshot: (_scope, _workspace, snapshot) => snapshots.push(snapshot),
    fetchImpl: async (_url, options) => {
      const body = JSON.parse(options.body); requests.push(body)
      if (body.start_idempotency_key) return { ok: true, status: 200, json: async () => ({ snapshot_profile: 'workspace_shell_v1', items: [{ schema_version: 1, kind: 'workspace', data: { id: 'a' } }], has_more: true, next_page_token: 'page-2' }) }
      if (body.page_token) return { ok: true, status: 200, json: async () => ({ snapshot_profile: 'workspace_shell_v1', items: [], has_more: false, resume_token: 'opaque-resume' }) }
      return { ok: true, status: 200, json: async () => ({ ticket: 'one-use-ticket' }) }
    }, setTimer: () => 0, clearTimer: () => {}
  })
  manager.connect({ audienceId: 'actor', scope: 'workspace', workspaceId: 'a' }); await new Promise(resolve => setTimeout(resolve, 0))
  assert.deepEqual(requests.map(request => request.page_token || (request.start_idempotency_key ? 'start' : 'ticket')), ['start', 'page-2', 'ticket'])
  assert.deepEqual(snapshots, [{ items: [{ schema_version: 1, kind: 'workspace', data: { id: 'a' } }], resumeToken: 'opaque-resume', snapshotProfile: 'workspace_shell_v1' }])
  assert.equal(sockets.length, 1)
  assert.deepEqual(readCheckpoint(s, 'actor', 'workspace', 'a'), { resumeToken: 'opaque-resume', eventIds: [] })
})

test('access revocation clears only its scope and cannot schedule a reconnect', async () => {
  const s = storage(); const timers = []; const sockets = []
  class FakeSocket { constructor() { sockets.push(this); queueMicrotask(() => this.onopen()) } close() {} }
  const manager = createChangeStreamManager({ apiUrl: 'https://api.example', wsUrl: 'wss://ws.example/ws', storage: s, WebSocketImpl: FakeSocket, fetchImpl: async () => ({ ok: true, status: 200, json: async () => ({ ticket: 'ticket' }) }), setTimer: fn => { timers.push(fn); return timers.length }, clearTimer: () => {} })
  writeCheckpoint(s, 'actor', 'workspace', 'a', 'a-token', []); writeCheckpoint(s, 'actor', 'workspace', 'b', 'b-token', []); writeCheckpoint(s, 'actor', 'global', null, 'global-token', [])
  manager.connect({ audienceId: 'actor', scope: 'workspace', workspaceId: 'a', resumeToken: 'a-token' }); manager.connect({ audienceId: 'actor', scope: 'workspace', workspaceId: 'b', resumeToken: 'b-token' }); manager.connect({ audienceId: 'actor', scope: 'global', resumeToken: 'global-token' }); await new Promise(resolve => setTimeout(resolve, 0))
  sockets[2].onmessage({ data: JSON.stringify({ schema_version: 1, type: 'access_revoked', workspace_id: 'a' }) }); assert.deepEqual(readCheckpoint(s, 'actor', 'global', null), { resumeToken: 'global-token', eventIds: [] })
  sockets[0].onmessage({ data: JSON.stringify({ schema_version: 1, type: 'access_revoked', workspace_id: 'a' }) }); sockets[0].onclose()
  assert.equal(readCheckpoint(s, 'actor', 'workspace', 'a'), null)
  assert.deepEqual(readCheckpoint(s, 'actor', 'workspace', 'b'), { resumeToken: 'b-token', eventIds: [] })
  assert.equal(timers.length, 0)
  manager.disconnect('workspace', 'b'); manager.clear('workspace', 'b', 'actor'); assert.equal(readCheckpoint(s, 'actor', 'workspace', 'b'), null)
  sockets[2].onclose(); assert.equal(timers.length, 1)
})

test('canonical HTTP endpoints preserve root and configured deployment prefixes', () => {
  assert.equal(canonicalEndpoint('https://api.example', '/api/v1/change-stream/ticket'), 'https://api.example/api/v1/change-stream/ticket')
  assert.equal(canonicalEndpoint('https://api.example/hmem/', '/api/v1/change-stream/resync'), 'https://api.example/hmem/api/v1/change-stream/resync')
})

test('canonical replay frames coalesce through their checkpoint boundary', () => {
  const delivered = []; const timers = new Map(); let nextTimer = 0
  const batcher = createCanonicalFrameBatcher({
    onBatch: (...args) => delivered.push(args),
    setTimer: fn => { nextTimer += 1; timers.set(nextTimer, fn); return nextTimer },
    clearTimer: timer => timers.delete(timer)
  })
  const first = { schema_version: 1, type: 'change', event: { event_id: 'one' } }
  const second = { schema_version: 1, type: 'change', event: { event_id: 'two' } }
  const checkpoint = { schema_version: 1, type: 'checkpoint', catch_up: 'complete', resume_token: 'opaque' }
  batcher.queue('workspace', 'a', first)
  batcher.queue('workspace', 'a', second)
  assert.equal(delivered.length, 0)
  batcher.queue('workspace', 'a', checkpoint)
  assert.deepEqual(delivered, [['workspace', 'a', [first, second, checkpoint]]])
  assert.equal(timers.size, 0)
})

test('standalone access grant is an immediate boundary without failing its carrier scope', () => {
  const delivered = []; const incomplete = []; const timers = new Map(); let nextTimer = 0
  const batcher = createCanonicalFrameBatcher({
    onBatch: (...args) => delivered.push(args),
    onIncomplete: (...args) => incomplete.push(args),
    setTimer: fn => { nextTimer += 1; timers.set(nextTimer, fn); return nextTimer },
    clearTimer: timer => timers.delete(timer)
  })
  const grant = { schema_version: 1, type: 'access_granted', workspace_id: 'newly-readable' }
  const change = { schema_version: 1, type: 'change', event: { event_id: 'after-grant' } }
  const checkpoint = { schema_version: 1, type: 'checkpoint', catch_up: 'complete', resume_token: 'carrier-token' }
  batcher.queue('global', null, grant)
  assert.deepEqual(delivered, [['global', null, [grant]]])
  assert.deepEqual(incomplete, [])
  assert.equal(timers.size, 0)
  batcher.queue('global', null, change)
  batcher.queue('global', null, checkpoint)
  assert.deepEqual(delivered, [['global', null, [grant]], ['global', null, [change, checkpoint]]])
  assert.deepEqual(incomplete, [])
})

test('an incomplete delivery times out into scoped recovery without applying partial frames', () => {
  const delivered = []; const incomplete = []; const timers = new Map(); let nextTimer = 0
  const batcher = createCanonicalFrameBatcher({
    onBatch: (...args) => delivered.push(args),
    onIncomplete: (...args) => incomplete.push(args),
    setTimer: (fn, milliseconds) => { nextTimer += 1; timers.set(nextTimer, { fn, milliseconds }); return nextTimer },
    clearTimer: timer => timers.delete(timer),
    incompleteDelay: 750
  })
  const change = { schema_version: 1, type: 'change', event: { event_id: 'one' } }
  const checkpoint = { schema_version: 1, type: 'checkpoint', catch_up: 'complete', resume_token: 'opaque' }
  batcher.queue('workspace', 'a', change)
  assert.equal(timers.get(1).milliseconds, 750)
  timers.get(1).fn()
  batcher.queue('workspace', 'a', checkpoint)
  assert.deepEqual(delivered, [])
  assert.deepEqual(incomplete, [['workspace', 'a']])
  batcher.clear('workspace', 'a')
  batcher.queue('workspace', 'a', checkpoint)
  assert.deepEqual(delivered, [['workspace', 'a', [checkpoint]]])
})

test('cancelled resync aborts and cannot emit snapshot, state, socket, or storage writes', async () => {
  const s = storage(); const snapshots = []; const states = []; const sockets = []; let release; let signal
  class FakeSocket { constructor() { sockets.push(this) } close() {} }
  const manager = createChangeStreamManager({
    apiUrl: 'https://api.example/prefix', wsUrl: 'wss://api.example/prefix/api/v1/ws', storage: s, WebSocketImpl: FakeSocket,
    onSnapshot: (...args) => snapshots.push(args), onState: (...args) => states.push(args),
    fetchImpl: async (_url, options) => { signal = options.signal; return new Promise(resolve => { release = resolve }) },
    setTimer: () => 0, clearTimer: () => {}
  })
  manager.connect({ audienceId: 'actor-a', scope: 'workspace', workspaceId: 'a' })
  manager.disconnect('workspace', 'a')
  assert.equal(signal.aborted, true)
  release({ ok: true, status: 200, json: async () => ({ items: [{ schema_version: 1, kind: 'workspace', data: { id: 'a' } }], has_more: false, resume_token: 'cancelled-token' }) })
  await new Promise(resolve => setTimeout(resolve, 0))
  assert.deepEqual(snapshots, []); assert.equal(sockets.length, 0); assert.equal(readCheckpoint(s, 'actor-a', 'workspace', 'a'), null)
  assert.deepEqual(states, [['workspace', 'a', 'resyncing']])
})

test('audience replacement makes every old async continuation inert', async () => {
  const s = storage(); const pending = []; const snapshots = []; const states = []; const sockets = []
  class FakeSocket { constructor(url) { this.url = url; sockets.push(this) } close() {} }
  const manager = createChangeStreamManager({
    apiUrl: 'https://api.example', wsUrl: 'wss://api.example/ws', storage: s, WebSocketImpl: FakeSocket,
    onSnapshot: (_scope, _workspace, snapshot) => snapshots.push(snapshot), onState: (...args) => states.push(args),
    fetchImpl: (url, options) => new Promise(resolve => pending.push({ url, options, resolve })), setTimer: () => 0, clearTimer: () => {}
  })
  manager.connect({ audienceId: 'actor-a', scope: 'workspace', workspaceId: 'a' })
  manager.connect({ audienceId: 'actor-b', scope: 'workspace', workspaceId: 'a' })
  assert.equal(pending.length, 2); assert.equal(pending[0].options.signal.aborted, true)
  pending[0].resolve({ ok: true, status: 200, json: async () => ({ snapshot_profile: 'workspace_shell_v1', items: [{ schema_version: 1, kind: 'workspace', data: { id: 'a' } }], has_more: false, resume_token: 'old-token' }) })
  pending[1].resolve({ ok: true, status: 200, json: async () => ({ snapshot_profile: 'workspace_shell_v1', items: [{ schema_version: 1, kind: 'workspace', data: { id: 'a' } }], has_more: false, resume_token: 'new-token' }) })
  await new Promise(resolve => setTimeout(resolve, 0))
  assert.equal(pending.length, 3)
  pending[2].resolve({ ok: true, status: 200, json: async () => ({ ticket: 'new-ticket' }) })
  await new Promise(resolve => setTimeout(resolve, 0))
  assert.deepEqual(snapshots, [{ items: [{ schema_version: 1, kind: 'workspace', data: { id: 'a' } }], resumeToken: 'new-token', snapshotProfile: 'workspace_shell_v1' }])
  assert.equal(readCheckpoint(s, 'actor-a', 'workspace', 'a'), null)
  assert.deepEqual(readCheckpoint(s, 'actor-b', 'workspace', 'a'), { resumeToken: 'new-token', eventIds: [] })
  assert.equal(sockets.length, 1); assert.match(sockets[0].url, /ticket=new-ticket/)
  assert.equal(states.filter(([, , state]) => state === 'connecting').length, 1)
})

test('401 is unauthenticated while 403 is a scope-local forbidden result', async () => {
  async function stateFor(status) {
    const states = []
    const manager = createChangeStreamManager({ apiUrl: 'https://api.example', wsUrl: 'wss://api.example/ws', storage: storage(), WebSocketImpl: class {}, fetchImpl: async () => ({ ok: false, status }), onState: (...args) => states.push(args), setTimer: () => 0, clearTimer: () => {} })
    manager.connect({ audienceId: 'actor', scope: 'workspace', workspaceId: 'a', resumeToken: 'resume' })
    await new Promise(resolve => setTimeout(resolve, 0))
    return states.at(-1)[2]
  }
  assert.equal(await stateFor(401), 'unauthenticated')
  assert.equal(await stateFor(403), 'scope_forbidden')
})

test('a late 403 from a replaced principal cannot revoke the replacement scope', async () => {
  const pending = []; const states = []; const sockets = []
  class FakeSocket { constructor(url) { this.url = url; sockets.push(this) } close() {} }
  const manager = createChangeStreamManager({
    apiUrl: 'https://api.example', wsUrl: 'wss://api.example/ws', storage: storage(), WebSocketImpl: FakeSocket,
    fetchImpl: (url, options) => new Promise(resolve => pending.push({ url, options, resolve })), onState: (...args) => states.push(args), setTimer: () => 0, clearTimer: () => {}
  })
  manager.connect({ audienceId: 'actor-a', scope: 'workspace', workspaceId: 'a', resumeToken: 'old' })
  await Promise.resolve()
  manager.connect({ audienceId: 'actor-b', scope: 'workspace', workspaceId: 'a', resumeToken: 'new' })
  await Promise.resolve()
  pending[0].resolve({ ok: false, status: 403 })
  pending[1].resolve({ ok: true, status: 200, json: async () => ({ ticket: 'replacement' }) })
  await new Promise(resolve => setTimeout(resolve, 0))
  assert.equal(states.some(([, , state]) => state === 'scope_forbidden'), false)
  assert.equal(sockets.length, 1); assert.match(sockets[0].url, /ticket=replacement/)
})

test('resync retry classes are bounded and deterministic', async () => {
  async function attemptsFor(status) {
    let attempts = 0; const states = []; const delays = []
    const manager = createChangeStreamManager({
      apiUrl: 'https://api.example', wsUrl: 'wss://api.example/ws', storage: storage(), WebSocketImpl: class {},
      fetchImpl: async () => { attempts += 1; return { ok: false, status } }, onState: (...args) => states.push(args),
      setTimer: (fn, milliseconds) => { delays.push(milliseconds); queueMicrotask(fn); return delays.length }, clearTimer: () => {}
    })
    manager.connect({ audienceId: 'actor', scope: 'workspace', workspaceId: 'a' })
    await new Promise(resolve => setTimeout(resolve, 10))
    return { attempts, delays, state: states.at(-1)[2] }
  }
  assert.deepEqual(await attemptsFor(500), { attempts: 6, delays: [100, 200, 400, 800, 1600], state: 'resync_500' })
  assert.deepEqual(await attemptsFor(429), { attempts: 6, delays: [100, 200, 400, 800, 1600], state: 'resync_429' })
  assert.deepEqual(await attemptsFor(400), { attempts: 1, delays: [], state: 'resync_400' })
})

test('resync retries preserve each page body and continuation token byte-for-byte', async () => {
  const bodies = []; const delays = []; let resyncAttempt = 0; const snapshots = []
  class FakeSocket { constructor() {} close() {} }
  const manager = createChangeStreamManager({
    apiUrl: 'https://api.example/prefix', wsUrl: 'wss://api.example/prefix/ws', storage: storage(), WebSocketImpl: FakeSocket,
    cryptoImpl: { getRandomValues: bytes => bytes.fill(7) }, onSnapshot: (_scope, _workspace, snapshot) => snapshots.push(snapshot),
    fetchImpl: async (url, options) => {
      if (url.endsWith('/change-stream/ticket')) return { ok: true, status: 200, json: async () => ({ ticket: 'ticket' }) }
      bodies.push(options.body); resyncAttempt += 1
      if (resyncAttempt === 1 || resyncAttempt === 3) return { ok: false, status: 500 }
      if (resyncAttempt === 2) return { ok: true, status: 200, json: async () => ({ snapshot_profile: 'workspace_shell_v1', items: [{ schema_version: 1, kind: 'workspace', data: { id: 'a' } }], has_more: true, next_page_token: 'continuation' }) }
      return { ok: true, status: 200, json: async () => ({ snapshot_profile: 'workspace_shell_v1', items: [], has_more: false, resume_token: 'resume' }) }
    },
    setTimer: (fn, milliseconds) => { delays.push(milliseconds); queueMicrotask(fn); return delays.length }, clearTimer: () => {}
  })
  manager.connect({ audienceId: 'actor', scope: 'workspace', workspaceId: 'a' })
  await new Promise(resolve => setTimeout(resolve, 10))
  assert.equal(bodies.length, 4)
  assert.equal(bodies[0], bodies[1])
  assert.equal(bodies[2], bodies[3])
  assert.equal(JSON.parse(bodies[2]).page_token, 'continuation')
  assert.deepEqual(delays, [100, 100])
  assert.equal(snapshots.length, 1)
})

test('disconnect cancels an in-progress resync backoff before another fetch', async () => {
  const timers = []; const states = []; let attempts = 0
  const manager = createChangeStreamManager({
    apiUrl: 'https://api.example', wsUrl: 'wss://api.example/ws', storage: storage(), WebSocketImpl: class {},
    fetchImpl: async () => { attempts += 1; return { ok: false, status: 500 } }, onState: (...args) => states.push(args),
    setTimer: (fn, milliseconds) => { timers.push({ fn, milliseconds, cleared: false }); return timers.length },
    clearTimer: timer => { if (timers[timer - 1]) timers[timer - 1].cleared = true }
  })
  manager.connect({ audienceId: 'actor', scope: 'workspace', workspaceId: 'a' })
  await new Promise(resolve => setTimeout(resolve, 0))
  assert.equal(attempts, 1)
  assert.equal(timers[0].milliseconds, 100)
  manager.disconnect('workspace', 'a')
  assert.equal(timers[0].cleared, true)
  timers[0].fn()
  await new Promise(resolve => setTimeout(resolve, 0))
  assert.equal(attempts, 1)
  assert.deepEqual(states, [['workspace', 'a', 'resyncing']])
})

test('malformed or cross-scope snapshot pages never become durable', async () => {
  const s = storage(); const snapshots = []; const states = []
  const manager = createChangeStreamManager({
    apiUrl: 'https://api.example', wsUrl: 'wss://api.example/ws', storage: s, WebSocketImpl: class {}, onSnapshot: (...args) => snapshots.push(args), onState: (...args) => states.push(args),
    fetchImpl: async () => ({ ok: true, status: 200, json: async () => ({ items: [{ schema_version: 1, kind: 'project', data: { id: 'p', workspace_id: 'other' } }], has_more: false, resume_token: 'unsafe' }) }), setTimer: () => 0, clearTimer: () => {}
  })
  manager.connect({ audienceId: 'actor', scope: 'workspace', workspaceId: 'a' })
  await new Promise(resolve => setTimeout(resolve, 0))
  assert.deepEqual(snapshots, []); assert.equal(readCheckpoint(s, 'actor', 'workspace', 'a'), null); assert.equal(states.at(-1)[2], 'invalid_snapshot')
})
