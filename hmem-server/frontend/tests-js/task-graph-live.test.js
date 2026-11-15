import assert from 'node:assert/strict'
import test from 'node:test'
import { createTaskGraphLive, GRAPH_MAX_RESEEDS } from '../src/task-graph-live.js'
import { createTaskGraphActivity } from '../src/task-graph-activity.js'

const guard = { workspaceId: 'w', projectId: 'p', audienceId: 'reader', sessionEpoch: 1, targetGeneration: 2 }
const checkpoint = { schema_version: 1, type: 'checkpoint', catch_up: 'complete', resume_token: 'private-checkpoint' }
const event = (id, type = 'task', action = 'updated') => ({ schema_version: 1, type: 'change', event: {
  schema_version: 1, event_id: id, scope: 'workspace', workspace_id: 'w', occurred_at: '2026-11-14T20:00:00Z',
  actor: { type: 'system', id: null }, transaction: { id: 'tx-' + id, cause: 'core', request_id: null },
  entity: { type, id: 'target', action }, invalidations: [{ kind: 'entity', target: type + ':target' }] } })
const tick = () => new Promise(resolve => setImmediate(resolve))

function clock() {
  let time = 0, next = 0
  const timers = new Map()
  return { now: () => time, timers, jump: delta => { time += delta },
    setTimer(fn, delay) { const id = ++next; timers.set(id, { fn, at: time + delay }); return id },
    clearTimer(id) { timers.delete(id) },
    async next() { const item = [...timers].sort((a, b) => a[1].at - b[1].at)[0]; assert.ok(item, 'expected pending timer'); timers.delete(item[0]); time = item[1].at; item[1].fn(); await tick() }
  }
}

function harness() {
  const c = clock(), streams = [], loaders = [], snapshots = [], states = [], authorities = [], activity = []
  let context = { ...guard }, accepting = true, activityConsumer = () => {}
  const live = createTaskGraphLive({ apiUrl: 'https://api.example', wsUrl: 'wss://api.example/ws', ...c,
    isCurrent: value => JSON.stringify(value) === JSON.stringify(context),
    onSnapshot: (owner, items) => { snapshots.push({ owner, items }); return accepting }, onState: (...value) => states.push(value),
    onActivity: (...value) => { activity.push(value); activityConsumer(...value) },
    onUnauthorized: owner => authorities.push(['unauthenticated', owner]), onForbidden: owner => authorities.push(['forbidden', owner]),
    streamFactory(options) {
      const stream = { options, stopped: false, config: null,
        connect(config) { this.config = config }, disconnectAll() { this.stopped = true },
        snapshot(items = ['seed']) { return options.onSnapshot('workspace', 'w', { items, resumeToken: 'private-seed', snapshotProfile: 'full_v1' }) },
        frame(frame) { return options.onFrame('workspace', 'w', JSON.stringify(frame)) }, state(value) { options.onState('workspace', 'w', value) } }
      streams.push(stream); return stream
    },
    loaderFactory(options) {
      const loader = { reads: [], pending: [], cancels: 0,
        load(owner, headers) { this.reads.push({ owner, headers }); return new Promise(resolve => this.pending.push(resolve)) },
        finish(items = ['refresh']) { const resolve = this.pending.shift(); const accepted = options.onSnapshot(guard, items); resolve?.(accepted) },
        fail(reason) { options.onState(guard, 'failed', reason); this.pending.shift()?.(false) },
        cancel() { this.cancels++; for (const resolve of this.pending.splice(0)) resolve(false) } }
      loaders.push(loader); return loader
    } })
  return { live, c, streams, loaders, snapshots, states, authorities, activity, stream: () => streams.at(-1), loader: () => loaders.at(-1),
    accept(value) { accepting = value }, activityConsumer(value) { activityConsumer = value }, context(value) { context = value }, start() { live.load(guard, { Authorization: 'test-only' }); streams.at(-1).snapshot(); streams.at(-1).frame(checkpoint) } }
}

test('activity has a separate lifecycle with progress ignored and stream reseeds retiring all proofs', async () => {
  const h = harness(); h.start(); h.activity.length = 0
  h.stream().frame(event('progress', 'agent_session'))
  assert.deepEqual(h.activity, [])
  const foreign = event('foreign-membership', 'agent_session'); foreign.event.invalidations.push({ kind: 'collection', target: 'active_sessions:other' }); h.stream().frame(foreign)
  const wrongKind = event('noncanonical-kind', 'agent_session'); wrongKind.event.invalidations.push({ kind: 'active_sessions', target: 'w' }); h.stream().frame(wrongKind)
  assert.deepEqual(h.activity, [])
  const membership = event('termination', 'agent_session'); membership.event.invalidations.push({ kind: 'collection', target: 'active_sessions:w' })
  h.stream().frame(membership); h.stream().frame(event('association', 'task_session'))
  h.stream().frame({ schema_version: 1, type: 'session_activity_changed', workspace_id: 'w' })
  assert.deepEqual(h.activity.map(v => v[1]), ['invalidate', 'invalidate', 'invalidate'])
  assert.equal(h.loader().reads.length, 0, 'activity never requests a graph snapshot')
  h.stream().state('reconnecting'); assert.equal(h.activity.at(-1)[1], 'pause')
  h.stream().frame(checkpoint); assert.equal(h.activity.at(-1)[1], 'ready')
  h.stream().frame({ schema_version: 1, type: 'resync_required' }); await h.c.next()
  h.stream().snapshot(); assert.equal(h.activity.at(-1)[1], 'pause', 'true stream seed invalidates positive AND negative proofs')
  h.stream().frame(checkpoint); assert.equal(h.activity.at(-1)[1], 'ready')
  h.live.cancel()
})

test('finite snapshot Retry restores activity proof through a healthy stream without another checkpoint', async () => {
  const h = harness(), proofs = []
  let reads = 0
  const activity = createTaskGraphActivity({ apiUrl: 'https://api.example', isCurrent: () => true, onProof: (_, id, active) => proofs.push([id, active]), setTimer: h.c.setTimer, clearTimer: h.c.clearTimer,
    fetchImpl: async () => { reads++; return { ok: true, status: 200, json: async () => ({ items: [{ id: 's', workspace_id: 'w', state: 'active', format_label: 'codex', created_at: 'date', last_seen_at: 'old', committed_bytes: 0, committed_records: 0, revision: 0, ends_with_newline: true }], next_cursor: null }) } } })
  activity.load(guard); activity.reconcile(guard, ['task']); h.activityConsumer((owner, kind) => activity.signal(owner, kind)); h.start(); await tick()
  assert.equal(reads, 1); assert.deepEqual(proofs.at(-1), ['task', true])
  h.stream().frame(event('task-invalidated')); await h.c.next(); h.loader().fail('snapshot_500'); await tick()
  assert.deepEqual(proofs.at(-1), ['task', null]); assert.equal(reads, 1)
  h.live.retry(); h.loader().finish(); await tick()
  assert.equal(h.states.at(-1)[1], 'live'); assert.equal(reads, 2); assert.deepEqual(proofs.at(-1), ['task', true])
  h.stream().frame(event('another-task')); await h.c.next(); h.loader().fail('snapshot_500'); await tick()
  h.accept(false); h.live.retry(); h.loader().finish(); await tick()
  h.stream().frame(checkpoint); await tick()
  assert.equal(reads, 2, 'rejected finite snapshot cannot resume activity proof')
  assert.deepEqual(proofs.at(-1), ['task', null])
  activity.cancel(); h.live.cancel()
})

test('own full snapshot seeds its stream, capabilities stay private and no primary storage is accepted', () => {
  const h = harness(); h.start()
  assert.equal(h.stream().config.snapshotProfile, 'full_v1')
  assert.equal(h.stream().config.audienceId, 'reader')
  assert.equal(Object.hasOwn(h.stream().config, 'resumeToken'), false)
  assert.deepEqual(h.snapshots, [{ owner: guard, items: ['seed'] }])
  assert.equal(h.states.at(-1)[1], 'live')
  assert.doesNotMatch(JSON.stringify([h.snapshots, h.states]), /private-seed|private-checkpoint/)
  h.stream().options.storage.setItem('own', 'private')
  assert.equal(h.stream().options.storage.getItem('own'), 'private')
  h.live.cancel(); assert.equal(h.stream().options.storage.getItem('own'), null)
})

test('all relevant canonical entity changes trigger serialized reads with one queued follow-up', async () => {
  const h = harness(); h.start()
  for (const type of ['task', 'project', 'task_dependency', 'workspace']) h.stream().frame(event('before-' + type, type))
  assert.equal(h.c.timers.size, 1)
  await h.c.next(); assert.equal(h.loader().reads.length, 1)
  for (let index = 0; index < 30; index++) h.stream().frame(event('during-' + index))
  assert.equal(h.c.timers.size, 0)
  assert.equal(h.loader().cancels, 0, 'ordinary invalidations never abort an active read')
  h.loader().finish(['first-current-snapshot']); await tick()
  assert.equal(h.c.timers.size, 1)
  await h.c.next(); assert.equal(h.loader().reads.length, 2)
  h.loader().finish(['second-current-snapshot']); await tick()
  assert.equal(h.states.at(-1)[1], 'live')
  assert.equal(h.streams.length, 1, 'refresh does not restart the stream')
  h.stream().frame(event('after')); await h.c.next()
  assert.equal(h.loader().reads.length, 3)
  h.live.cancel()
})

test('duplicate IDs and unrelated session/observation/activity storms do not create refresh storms', async () => {
  const h = harness(); h.start()
  for (let index = 0; index < 1000; index++) h.stream().frame(event('same'))
  assert.equal(h.c.timers.size, 1)
  await h.c.next(); h.loader().finish(); await tick()
  for (let index = 0; index < 100; index++) {
    for (const type of ['observation', 'agent_session', 'task_session', 'document', 'task_document']) h.stream().frame(event(type + index, type))
    h.stream().frame({ schema_version: 1, type: 'session_activity_changed', workspace_id: 'w' })
  }
  assert.equal(h.c.timers.size, 0)
  assert.equal(h.loader().reads.length, 1)
  h.live.cancel()
})

test('failed refresh stays stale until explicit retry while its own stream still observes revocation', async () => {
  const h = harness(); h.start(); h.stream().frame(event('first')); await h.c.next()
  h.loader().fail('snapshot_500'); await tick()
  assert.equal(h.states.at(-1)[1], 'failed')
  h.stream().state('reconnecting'); h.stream().state('connecting'); h.stream().state('replaying'); h.stream().frame(checkpoint)
  assert.equal(h.states.at(-1)[1], 'failed', 'socket recovery cannot erase a paused read failure')
  for (let index = 0; index < 40; index++) h.stream().frame(event('after-failure-' + index))
  assert.equal(h.c.timers.size, 0); assert.equal(h.states.at(-1)[1], 'failed')
  h.live.retry(); assert.equal(h.loader().reads.length, 2)
  h.loader().finish(); await tick(); assert.equal(h.states.at(-1)[1], 'live')
  h.stream().frame({ schema_version: 1, type: 'access_revoked', workspace_id: 'w' })
  assert.equal(h.states.at(-1)[1], 'retired'); assert.equal(h.authorities[0][0], 'forbidden')
  assert.equal(h.stream().stopped, true); assert.equal(h.c.timers.size, 0)
})

test('a backwards clock observation cannot extend refresh cooldown beyond its local bound', async () => {
  const h = harness(); h.start(); h.stream().frame(event('first')); await h.c.next()
  h.loader().finish(); await tick(); h.c.jump(-100000)
  h.stream().frame(event('after-clock-change'))
  const pending = [...h.c.timers.values()]
  assert.equal(pending.length, 1)
  assert.ok(pending[0].at - h.c.now() <= 500)
  await h.c.next(); assert.equal(h.loader().reads.length, 2); h.live.cancel()
})

test('workspace/session/audience/project/history-target changes fence old callbacks', async () => {
  for (const [key, value] of [['workspaceId', 'other'], ['sessionEpoch', 8], ['audienceId', 'other'], ['projectId', 'other'], ['targetGeneration', 9]]) {
    const h = harness(); h.start(); h.stream().frame(event('start')); await h.c.next()
    const old = h.stream(), oldLoader = h.loader(), before = h.snapshots.length
    const next = { ...guard, [key]: value }; h.context(next); h.live.load(next)
    old.snapshot(['late']); old.frame(event('late')); old.state('scope_forbidden'); oldLoader.finish(['late-read'])
    await tick(); assert.equal(h.snapshots.length, before); assert.deepEqual(h.authorities, [])
    assert.equal(old.stopped, true); h.live.cancel(); assert.equal(h.c.timers.size, 0)
  }
})

test('401, 403 and access revocation during a read retire all sensitive work immediately', async () => {
  for (const reason of ['unauthenticated', 'scope_forbidden', 'access_revoked']) {
    const h = harness(); h.start(); h.stream().frame(event('read')); await h.c.next()
    const before = h.snapshots.length
    if (reason === 'access_revoked') h.stream().frame({ schema_version: 1, type: reason, workspace_id: 'w' })
    else h.loader().fail(reason)
    h.loader().finish(['late']); await tick()
    assert.equal(h.snapshots.length, before)
    assert.equal(h.states.at(-1)[1], 'retired'); assert.equal(h.stream().stopped, true)
    assert.equal(h.c.timers.size, 0); assert.equal(h.authorities.length, 1)
  }
})

test('explicit authority retirement cancels even after the external credential guard changed', () => {
  const h = harness(); h.start(); h.context({ ...guard, audienceId: 'changed' })
  h.live.retire()
  assert.equal(h.stream().stopped, true); assert.equal(h.states.at(-1)[1], 'retired')
  assert.equal(h.c.timers.size, 0)
})

test('reconnect is not live until its matching checkpoint; 409/invalid-ticket reseeds are bounded', async () => {
  const h = harness(); h.start(); h.stream().state('reconnecting')
  assert.equal(h.states.at(-1)[1], 'recovering')
  h.stream().frame(checkpoint); assert.equal(h.states.at(-1)[1], 'live')
  for (let attempt = 0; attempt < GRAPH_MAX_RESEEDS; attempt++) {
    const old = h.stream(); old.state(attempt % 2 ? 'invalid_ticket' : 'resync_required')
    assert.equal(old.stopped, true); await h.c.next()
    assert.equal(h.streams.length, attempt + 2)
    assert.equal(Object.hasOwn(h.stream().config, 'resumeToken'), false)
    old.snapshot(['late-seed']); assert.equal(h.snapshots.length, 1)
  }
  h.stream().state('resync_required')
  assert.equal(h.states.at(-1)[1], 'failed'); assert.equal(h.c.timers.size, 0)
  h.live.retry(); assert.equal(h.streams.length, GRAPH_MAX_RESEEDS + 2)
  h.stream().snapshot(['recovered']); h.stream().frame(checkpoint)
  assert.equal(h.states.at(-1)[1], 'live'); h.live.cancel()
})

test('malformed envelopes fail closed rather than treating request-body scope as canonical events', async () => {
  const variants = [
    frame => { frame.event.scope = { scope: 'workspace', workspace_id: 'w' } },
    frame => { delete frame.event.actor }, frame => { frame.event.transaction.request_id = '' },
    frame => { frame.event.entity.action = 'unknown' }, frame => { frame.event.invalidations = [] },
    frame => { frame.event.workspace_id = 'other' }
  ]
  for (const change of variants) {
    const h = harness(); h.start(); const frame = event('invalid'); change(frame)
    h.stream().frame(frame)
    assert.equal(h.loader().reads.length, 0); assert.equal(h.stream().stopped, true)
    await h.c.next(); assert.equal(h.streams.length, 2); h.live.cancel()
  }
})

for (const timerFired of [false, true]) test(`retry owns one new authoritative seed across recovery (timerFired=${timerFired})`, async () => {
  const h = harness(); h.start()
  h.stream().state('resync_required')
  if (timerFired) await h.c.next()
  const abandoned = h.stream(), before = h.snapshots.length
  h.live.retry()
  const owner = h.stream()
  assert.notEqual(owner, abandoned)
  assert.equal(abandoned.stopped, true)
  assert.equal(h.c.timers.size, 0, 'retry removes the delayed reseed authority')
  assert.equal(h.loader().reads.length, 0, 'retry cannot compete with manager seeding via a finite read')
  assert.equal(h.loader().pending.length, 0)
  abandoned.snapshot(['abandoned-boundary']); abandoned.frame(checkpoint)
  assert.equal(h.snapshots.length, before)
  owner.snapshot(['authoritative-boundary']); owner.frame(checkpoint)
  assert.deepEqual(h.snapshots.at(-1).items, ['authoritative-boundary'])
  assert.equal(h.states.at(-1)[1], 'live')
  owner.frame(event('after-authoritative-boundary')); await h.c.next()
  h.loader().finish(['after-authoritative-change']); await tick()
  assert.deepEqual(h.snapshots.at(-1).items, ['after-authoritative-change'])
  assert.equal(h.streams.length, timerFired ? 3 : 2)
  h.live.cancel(); assert.equal(h.c.timers.size, 0)
})

test('actual manager waits for the final seed page before ticketing and catches events during refresh', async () => {
  const c = clock(), requests = [], snapshots = [], states = [], sockets = [], order = []
  let releaseSeed, releaseRefresh, starts = 0, random = 0
  const seedGate = new Promise(resolve => { releaseSeed = resolve })
  const refreshGate = new Promise(resolve => { releaseRefresh = resolve })
  const root = { schema_version: 1, kind: 'workspace', data: { id: 'w' } }
  const task = title => ({ schema_version: 1, kind: 'task', data: { id: 't', workspace_id: 'w', title } })
  const terminal = title => ({ snapshot_profile: 'full_v1', items: [root, task(title)], has_more: false, resume_token: 'private-refresh-boundary' })
  const response = page => ({ ok: true, status: 200, json: async () => page })
  class Socket {
    constructor() { sockets.push(this) }
    close() { this.closed = true; this.onclose?.({}) }
    emit(frame) { this.onmessage?.({ data: JSON.stringify(frame) }) }
  }
  const live = createTaskGraphLive({ apiUrl: 'https://api.example', wsUrl: 'wss://api.example/ws', ...c,
    isCurrent: () => true, WebSocketImpl: Socket, cryptoImpl: { getRandomValues(bytes) { bytes.fill(++random) } },
    onSnapshot(owner, items) { order.push('snapshot'); snapshots.push({ owner, items }); return true },
    onState: (...value) => states.push(value),
    fetchImpl: async (url, config) => {
      const body = JSON.parse(config.body); requests.push({ url, body })
      if (url.endsWith('/ticket')) { order.push('ticket'); return response({ ticket: 'private-ticket' }) }
      if (body.page_token) return seedGate
      starts++
      if (starts === 1) return response({ snapshot_profile: 'full_v1', items: [root], has_more: true, next_page_token: 'private-seed-page' })
      if (starts === 2) return refreshGate
      return response(terminal('after-during-read-event'))
    } })
  live.load(guard)
  await tick()
  assert.equal(requests.length, 2); assert.equal(snapshots.length, 0); assert.equal(sockets.length, 0)
  releaseSeed(response({ snapshot_profile: 'full_v1', items: [task('seed')], has_more: false, resume_token: 'private-seed-boundary' }))
  await tick()
  assert.deepEqual(order, ['snapshot', 'ticket'])
  assert.equal(requests.at(-1).body.resume_token, 'private-seed-boundary')
  sockets[0].onopen(); sockets[0].emit(checkpoint)
  assert.equal(states.at(-1)[1], 'live')
  sockets[0].emit(event('first-real-change')); await c.next()
  assert.equal(starts, 2)
  sockets[0].emit(event('during-real-read'))
  releaseRefresh(response(terminal('read-start-boundary'))); await tick()
  assert.equal(snapshots.at(-1).items.at(-1).data.title, 'read-start-boundary')
  await c.next()
  assert.equal(starts, 3); assert.equal(snapshots.at(-1).items.at(-1).data.title, 'after-during-read-event')
  assert.equal(sockets.length, 1); assert.equal(states.at(-1)[1], 'live')
  assert.equal(requests.filter(value => value.url.endsWith('/ticket')).length, 1)
  assert.doesNotMatch(JSON.stringify([snapshots, states]), /private-/)
  live.cancel(); assert.equal(c.timers.size, 0)
})
