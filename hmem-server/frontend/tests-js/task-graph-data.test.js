import assert from 'node:assert/strict'
import test from 'node:test'
import { createTaskGraphDataLoader } from '../src/task-graph-data.js'

const guard = { workspaceId: 'w', projectId: 'p', audienceId: 'reader', sessionEpoch: 3, targetGeneration: 4 }
const item = (kind, id, extra = {}) => ({ kind, schema_version: kind === 'observation' ? 2 : 1,
  data: { id, workspace_id: 'w', ...extra } })
const root = item('workspace', 'w')
const terminal = items => ({ snapshot_profile: 'full_v1', items, has_more: false, resume_token: 'private-resume' })
const more = (items, token = 'private-page') => ({ snapshot_profile: 'full_v1', items, has_more: true, next_page_token: token })
const response = (page, status = 200) => ({ ok: status >= 200 && status < 300, status, json: async () => page })
const defer = () => { let resolve; const promise = new Promise(done => { resolve = done }); return { promise, resolve } }

function harness(fetchImpl, options = {}) {
  const states = [], snapshots = [], requests = [], timers = new Map()
  let timerId = 0
  let context = { ...guard }
  const loader = createTaskGraphDataLoader({ apiUrl: 'https://api.example/prefix/',
    cryptoImpl: { getRandomValues: bytes => { bytes.fill(7) } },
    isCurrent: value => JSON.stringify(value) === JSON.stringify(context),
    onState: (...args) => states.push(args),
    onSnapshot: (...args) => { snapshots.push(args); return true },
    fetchImpl: (url, config) => { requests.push({ url, config, body: JSON.parse(config.body) }); return fetchImpl(url, config) },
    setTimer: fn => { timers.set(++timerId, fn); return timerId }, clearTimer: id => timers.delete(id), ...options })
  return { loader, states, snapshots, requests, timers, context: value => { context = value } }
}

test('full snapshot pages publish atomically with mixed schemas and private capabilities', async () => {
  const second = defer()
  let calls = 0
  const observation = item('observation', 'o', { latest_sequence: 1, current_provenance: null })
  const h = harness(async () => ++calls === 1 ? response(more([root, observation])) : second.promise)
  const pending = h.loader.load(guard, { Authorization: 'test-only', 'X-CSRF-Token': 'test-csrf' })
  await new Promise(resolve => setTimeout(resolve, 0))
  assert.equal(h.snapshots.length, 0)
  assert.equal(h.requests.length, 2)
  assert.deepEqual(h.requests[0].body, { scope: { scope: 'workspace', workspace_id: 'w' }, snapshot_profile: 'full_v1', page_size: 100,
    start_idempotency_key: '07'.repeat(24) })
  assert.deepEqual(h.requests[1].body, { scope: { scope: 'workspace', workspace_id: 'w' }, page_token: 'private-page' })
  assert.equal(h.requests[0].url, 'https://api.example/prefix/api/v1/change-stream/resync')
  assert.equal(h.requests[0].config.credentials, 'include')
  assert.equal(h.requests[0].config.headers['X-CSRF-Token'], 'test-csrf')
  second.resolve(response(terminal([item('project', 'p'), item('task', 't')])))
  assert.equal(await pending, true)
  assert.equal(h.snapshots.length, 1)
  assert.equal(h.snapshots[0][1].length, 4)
  assert.doesNotMatch(JSON.stringify(h.snapshots), /private-page|private-resume/)
  assert.deepEqual(h.states.map(value => value[1]), ['loading', 'complete'])
  assert.equal(h.timers.size, 0)
  assert.equal(h.requests.length, 2, 'no ticket or socket request follows a graph snapshot')
})

test('malformed, nonprogress, duplicate and cross-scope pages never publish partial data', async () => {
  const cases = [
    [{ ...terminal([]), snapshot_profile: 'workspace_shell_v1' }],
    [more([root]), terminal([item('task', 'foreign', { workspace_id: 'other' })])],
    [more([root]), terminal([root])],
    [more([], 'empty')],
    [more([root], 'cycle'), more([item('task', 't')], 'cycle')],
    [{ ...more([root]), resume_token: 'premature' }],
    [{ ...terminal([root]), next_page_token: 'extra' }],
    [{ ...terminal([root]), resume_token: '' }],
    [terminal([root, { ...item('observation', 'o', { latest_sequence: 1, current_provenance: null }), schema_version: 1 }])],
    [terminal([root, item('observation', 'o', { latest_sequence: 1 })])]
  ]
  for (const pages of cases) {
    let index = 0
    const h = harness(async () => response(pages[index++]))
    assert.equal(await h.loader.load(guard), false)
    assert.equal(h.snapshots.length, 0)
    assert.deepEqual(h.states.at(-1).slice(1), ['failed', 'invalid_snapshot'])
    assert.equal(h.timers.size, 0)
  }
})

test('authorization/epoch rejection on a continuation cannot expose the earlier page', async () => {
  for (const [status, reason] of [[401, 'unauthenticated'], [403, 'scope_forbidden'], [409, 'resync_required'], [503, 'snapshot_503']]) {
    let calls = 0
    const h = harness(async () => ++calls === 1 ? response(more([root])) : response(null, status))
    assert.equal(await h.loader.load(guard), false)
    assert.equal(h.snapshots.length, 0)
    assert.deepEqual(h.states.at(-1).slice(1), ['failed', reason])
  }
})

test('workspace, project, audience, session and target retirement make delayed callbacks inert', async () => {
  for (const [key, value] of [['workspaceId', 'other'], ['projectId', 'other'], ['audienceId', 'other'], ['sessionEpoch', 5], ['targetGeneration', 9]]) {
    const delayed = defer()
    const h = harness(() => delayed.promise)
    const pending = h.loader.load(guard)
    h.context({ ...guard, [key]: value })
    delayed.resolve(response(terminal([root])))
    assert.equal(await pending, false)
    assert.equal(h.snapshots.length, 0)
    assert.equal(h.states.length, 1, 'stale failure must not overwrite the next view')
    assert.equal(h.timers.size, 0)
  }
})

test('new target cancels an old request even when the transport ignores abort', async () => {
  const delayed = defer()
  let calls = 0
  const h = harness(() => ++calls === 1 ? delayed.promise : Promise.resolve(response(terminal([root]))))
  const old = h.loader.load(guard)
  const newer = { ...guard, targetGeneration: 5 }
  h.context(newer)
  const current = h.loader.load(newer)
  assert.equal(await old, false)
  assert.equal(h.requests[0].config.signal.aborted, true)
  assert.equal(await current, true)
  delayed.resolve(response(terminal([root])))
  await Promise.resolve()
  assert.equal(h.snapshots.length, 1)
  assert.deepEqual(h.snapshots[0][0], newer)
  assert.equal(h.timers.size, 0)
})

test('explicit cancel settles pending work and forbids delayed publication', async () => {
  const delayed = defer()
  const h = harness(() => delayed.promise)
  const pending = h.loader.load(guard)
  h.loader.cancel()
  assert.equal(await pending, false)
  assert.equal(h.timers.size, 0)
  delayed.resolve(response(terminal([root])))
  await Promise.resolve()
  assert.equal(h.snapshots.length, 0)
})

test('request deadline aborts its exact request and reports a truthful failure', async () => {
  const h = harness(() => new Promise(() => {}))
  const pending = h.loader.load(guard)
  h.timers.values().next().value()
  assert.equal(await pending, false)
  assert.equal(h.requests[0].config.signal.aborted, true)
  assert.deepEqual(h.states.at(-1).slice(1), ['failed', 'transport_timeout'])
  assert.equal(h.timers.size, 0)
})

test('decode and consumer failures cannot report a complete graph', async () => {
  for (const fetchImpl of [async () => { throw new Error('network') }, async () => ({ ok: true, json: async () => { throw new Error('bad_json') } })]) {
    const h = harness(fetchImpl)
    assert.equal(await h.loader.load(guard), false)
    assert.equal(h.snapshots.length, 0)
    assert.equal(h.states.at(-1)[1], 'failed')
  }
  const h = harness(async () => response(terminal([root])), { onSnapshot: () => false })
  assert.equal(await h.loader.load(guard), false)
  assert.deepEqual(h.states.at(-1).slice(1), ['failed', 'snapshot_delivery_failed'])
})

test('required context and unpredictable start identity fail closed', async () => {
  assert.throws(() => createTaskGraphDataLoader({ apiUrl: 'https://api.example' }), /consumers required/)
  const h = harness(async () => response(terminal([root])), { cryptoImpl: null })
  assert.equal(await h.loader.load(guard), false)
  assert.equal(h.requests.length, 0)
  assert.deepEqual(h.states.at(-1).slice(1), ['failed', 'csprng-unavailable'])
  await assert.rejects(h.loader.load({ ...guard, audienceId: '' }), /invalid graph guard/)
})
