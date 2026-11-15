import assert from 'node:assert/strict'
import test from 'node:test'
import { activeSessionProof, createTaskGraphActivity } from '../src/task-graph-activity.js'

const guard = { workspaceId: 'w', projectId: 'p', audienceId: 'reader', sessionEpoch: 1, targetGeneration: 1 }
const tick = () => new Promise(resolve => setImmediate(resolve))
const row = (changes = {}) => ({ id: 'session', workspace_id: 'w', state: 'active', format_label: 'codex', created_at: '2026-01-01', last_seen_at: '2020-01-01', committed_bytes: 0, committed_records: 0, revision: 0, ends_with_newline: true, ...changes })
const page = active => ({ items: active ? [row()] : [], next_cursor: null })
function harness() {
  let context = guard, serial = 0
  const requests = [], proofs = [], timers = new Map()
  const activity = createTaskGraphActivity({ apiUrl: 'https://app.test', isCurrent: value => JSON.stringify(value) === JSON.stringify(context),
    onProof: (...proof) => proofs.push(proof), setTimer(fn, delay) { const id = ++serial; timers.set(id, { fn, delay }); return id }, clearTimer: id => timers.delete(id),
    fetchImpl(url, options) { return new Promise(resolve => requests.push({ url, options, resolve, settled: false })) } })
  return { activity, requests, proofs, timers, context: value => { context = value },
    start(ids = ['a', 'b', 'c']) { activity.load(context, { Authorization: 'test-only' }); activity.reconcile(context, ids); activity.signal(context, 'ready') },
    async finish(index, body = page(true), status = 200) { const request = requests[index]; request.settled = true; request.resolve({ ok: status === 200, status, json: async () => body }); await tick() },
    async wake() { const entry = [...timers].find(([, value]) => value.delay === 150); assert.ok(entry); timers.delete(entry[0]); entry[1].fn(); await tick() } }
}

test('only a valid direct active-session first page proves activity; quiet freshness is irrelevant', () => {
  assert.equal(activeSessionProof(page(true), 'w'), true)
  assert.equal(activeSessionProof(page(false), 'w'), false)
  for (const body of [{ items: [], next_cursor: { id: 'more', created_at: 'date' } }, { items: [row({ state: 'terminated' })], next_cursor: null }, { items: [row({ workspace_id: 'foreign' })] }, { items: [{ state: 'active' }] }, { items: [row(), row()] }, { items: [row()], next_cursor: 1 }]) assert.throws(() => activeSessionProof(body, 'w'))
})

test('visible demand is idempotent and physically bounded to two reads using active lifecycle only', async () => {
  const h = harness(); h.start()
  for (let i = 0; i < 20; i++) h.activity.reconcile(guard, ['a', 'b', 'c'])
  assert.equal(h.requests.length, 2)
  const url = new URL(h.requests[0].url)
  assert.equal(url.pathname, '/api/v1/workspaces/w/tasks/a/sessions'); assert.equal(url.search, '?limit=1&state=active')
  assert.equal(h.requests[0].options.credentials, 'include')
  await h.finish(0); assert.equal(h.requests.length, 3)
  await h.finish(1, page(false)); await h.finish(2)
  h.activity.reconcile(guard, ['a', 'b', 'c']); assert.equal(h.requests.length, 3)
  assert.deepEqual(h.proofs.map(v => [v[1], v[2]]), [['a', true], ['b', false], ['c', true]])
  h.activity.cancel()
})

test('logical scope retirement retains old physical slots until each real callback settles', async () => {
  const h = harness(); h.start(['a', 'b'])
  const next = { ...guard, sessionEpoch: 2 }; h.context(next); h.activity.load(next); h.activity.reconcile(next, ['a', 'c']); h.activity.signal(next, 'ready')
  assert.equal(h.requests.length, 2); assert.ok(h.requests.every(r => r.options.signal.aborted))
  await h.finish(0); assert.equal(h.requests.length, 3)
  await h.finish(1); assert.equal(h.requests.length, 4)
  assert.deepEqual(h.proofs, [])
  await h.finish(2); await h.finish(3); assert.ok(h.proofs.every(v => v[0].sessionEpoch === 2))
  h.activity.cancel()
})

test('filter remove/re-add ABA and membership invalidation reject old positives', async () => {
  const h = harness(); h.start(['a'])
  h.activity.reconcile(guard, []); h.activity.reconcile(guard, ['a'])
  assert.equal(h.requests.length, 2)
  await h.finish(0); assert.deepEqual(h.proofs, [])
  h.activity.signal(guard, 'invalidate'); await h.finish(1)
  assert.ok(h.proofs.every(v => v[2] === null))
  await h.wake(); assert.equal(h.requests.length, 3)
  await h.finish(2, page(false)); assert.equal(h.proofs.at(-1)[2], false)
  h.activity.cancel()
})

test('hint storms coalesce and reconnect/reseed revalidates positive and negative proofs', async () => {
  const h = harness(); h.start(['a', 'b']); await h.finish(0); await h.finish(1, page(false))
  h.activity.signal(guard, 'pause'); assert.deepEqual(h.proofs.slice(-2).map(v => v[2]), [null, null])
  h.activity.signal(guard, 'ready'); assert.equal(h.requests.length, 4)
  for (let i = 0; i < 100; i++) h.activity.signal(guard, 'invalidate')
  assert.equal([...h.timers.values()].filter(v => v.delay === 150).length, 1)
  await h.finish(2); await h.finish(3); assert.ok(h.proofs.slice(-2).every(v => v[2] === null))
  await h.wake(); assert.equal(h.requests.length, 6)
  await h.finish(4, page(false)); await h.finish(5); assert.deepEqual(h.proofs.slice(-2).map(v => v[2]), [false, true])
  h.activity.cancel()
})

test('errors and timeouts clear proof and do not retry from repeated rendering', async () => {
  const h = harness(); h.start(['a']); await h.finish(0, { items: [], next_cursor: { id: 'more', created_at: 'date' } })
  assert.deepEqual(h.proofs.at(-1).slice(2), [null, true])
  h.activity.reconcile(guard, ['a']); assert.equal(h.requests.length, 1)
  h.activity.retry(guard); assert.equal(h.requests.length, 2)
  const deadline = [...h.timers.values()].find(v => v.delay === 20000); deadline.fn()
  assert.deepEqual(h.proofs.at(-1).slice(2), [null, true]); assert.equal(h.requests[1].options.signal.aborted, true)
  h.activity.retry(guard); assert.equal(h.requests.length, 2, 'expired request retains actual occupancy/read ownership')
  await h.finish(1); assert.equal(h.requests.length, 3); await h.finish(2, page(false)); assert.equal(h.proofs.at(-1)[2], false)
  h.activity.cancel()
})
