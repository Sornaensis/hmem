import assert from 'node:assert/strict'
import test from 'node:test'
import { installTaskGraphNavigation } from '../src/task-graph-navigation.js'

function harness({ ready = false, target = true } = {}) {
  let clock = 0, nextTimer = 0, mutations, subscribed, stopped = false
  const frames = [], timers = new Map(), events = new Map(), receipts = [], focus = []
  const state = { scope: { workspace: 'w', epoch: 2, audience: 'reader' }, ready,
    return: { id: 'origin', token: 1, project: 'p', top: 100, anchor: 20 } }
  const scroll = { scrollTop: 100, getBoundingClientRect: () => ({ top: 0 }) }
  const card = { getClientRects: () => [{}], getBoundingClientRect: () => ({ top: 120 - scroll.scrollTop }) }
  const control = { focus: options => focus.push(options) }
  const node = { dataset: {}, contains: value => value === card, querySelector: () => ({ dataset: { hierarchyContext: 'current-layout' } }) }
  const paint = () => { node.dataset.taskGraphNavigation = JSON.stringify(state) }
  paint()
  const dispose = installTaskGraphNavigation({ ports: {
    taskGraphNavigationEvent: { send: value => receipts.push(value) },
    taskGraphNavigationCommand: { subscribe: callback => { subscribed = callback }, unsubscribe: callback => { assert.equal(callback, subscribed); subscribed = null } }
  } }, {
    document: { body: {}, querySelector: () => node,
      getElementById: id => id === 'main-content-scroll' ? scroll : target ? (id === 'entity-p' ? card : control) : null,
      addEventListener: (kind, callback) => events.set(kind, callback), removeEventListener: kind => events.delete(kind) },
    window: { MutationObserver: class { constructor(callback) { mutations = callback } observe() {} disconnect() { stopped = true } } },
    schedule: callback => frames.push(callback), now: () => clock,
    setTimer: (callback, delay) => { const id = ++nextTimer; timers.set(id, { callback, at: clock + delay }); return id },
    clearTimer: id => timers.delete(id)
  })
  return { state, timers, receipts, focus, events, dispose,
    flush() { frames.splice(0).forEach(callback => callback()) },
    advance(ms) { clock += ms; for (const [id, timer] of timers) if (timer.at <= clock) { timers.delete(id); timer.callback() } },
    paint() { paint(); mutations() }, stopped: () => stopped }
}

for (const options of [{ ready: false }, { ready: true, target: false }]) {
  test(`return expires without another DOM mutation: ${JSON.stringify(options)}`, () => {
    const h = harness(options)
    h.flush(); assert.equal(h.timers.size, 1)
    h.advance(4999); h.flush(); assert.equal(h.receipts.length, 0)
    h.advance(1); h.flush()
    assert.deepEqual(h.receipts, [{ kind: 'restored', id: 'origin', token: 1, scope: h.state.scope }])
    assert.equal(h.timers.size, 0); assert.equal(h.focus.length, 0)
    h.dispose()
  })
}

test('replacement cancels the old deadline and only settles the current return', () => {
  const h = harness()
  h.flush(); h.advance(1000); h.state.return = { ...h.state.return, token: 2 }; h.paint(); h.flush()
  assert.equal(h.timers.size, 1)
  h.advance(4000); h.flush(); assert.equal(h.receipts.length, 0)
  h.advance(1000); h.flush(); assert.equal(h.receipts[0].token, 2)
  assert.equal(h.timers.size, 0); h.dispose()
})

test('real user input cancels a quiet waiting return and clears its deadline', () => {
  for (const kind of ['pointerdown', 'keydown', 'wheel']) {
    const h = harness()
    h.flush(); h.events.get(kind)(); h.flush()
    assert.equal(h.receipts.length, 1); assert.equal(h.timers.size, 0)
    h.advance(5000); h.flush(); assert.equal(h.receipts.length, 1)
    h.dispose()
  }
})

test('successful restoration clears its deadline after stable current-layout paints', () => {
  const h = harness({ ready: true })
  for (let paint = 0; paint < 4; paint++) h.flush()
  assert.equal(h.receipts.length, 1); assert.equal(h.timers.size, 0)
  assert.ok(h.focus.length > 0); assert.deepEqual(h.focus[0], { preventScroll: true })
  h.advance(5000); h.flush(); assert.equal(h.receipts.length, 1)
  h.dispose()
})

test('retiring the marker or disposing clears deadlines and prevents late queued effects', () => {
  const retired = harness()
  retired.flush(); retired.state.return = null; retired.paint(); retired.flush()
  assert.equal(retired.timers.size, 0); retired.advance(5000); retired.flush()
  assert.equal(retired.receipts.length, 0); retired.dispose()
  const disposed = harness()
  disposed.flush(); disposed.events.get('wheel')(); disposed.dispose()
  assert.equal(disposed.timers.size, 0); disposed.advance(5000); disposed.flush()
  assert.equal(disposed.receipts.length, 0); assert.equal(disposed.events.size, 0); assert.equal(disposed.stopped(), true)
})
