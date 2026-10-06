import assert from 'node:assert/strict'
import test from 'node:test'
import { installObservationViewport } from '../src/observation-viewport.js'

function harness({ fallback = false } = {}) {
  let sync, mutations, resized, disposed = false, nextFrame = 0
  const events = new Map(), frames = new Map(), sent = [], effects = [], observed = new Set()
  const stamp = { workspace: 'repo', epoch: 2, generation: 'query', revision: 1 }
  const panel = { dataset: { observationContext: JSON.stringify({ workspaceId: 'repo', sessionEpoch: 2, token: 0, selectedId: null }) } }
  const root = { dataset: { observationViewportContext: JSON.stringify(stamp) }, clientWidth: 800,
    contains: element => !!element && rows.includes(element), closest: selector => selector === '#observation-panel' ? panel : null,
    getBoundingClientRect: () => ({ top: 100 - scroll.scrollTop }), querySelectorAll: () => rows }
  const scroll = { scrollTop: 0, clientHeight: 600, getBoundingClientRect: () => ({ top: 0 }),
    addEventListener: (name, fn) => events.set('scroll:' + name, fn), removeEventListener: name => events.delete('scroll:' + name) }
  const row = key => {
    const value = { dataset: { observationKey: key }, getBoundingClientRect: () => ({ height: 180 }), scrollIntoView: () => effects.push('scroll:' + key) }
    value.controls = [{ tagName: 'BUTTON', type: 'button', className: 'primary', disabled: false, tabIndex: 0, closest: selector => selector === '[data-observation-key]' ? value : null,
      getClientRects: () => [{}], focus: () => { doc.activeElement = value.controls[0]; effects.push('focus:' + key) } }]
    value.querySelectorAll = () => value.controls
    return value
  }
  let rows = [row('0'), row('1')]
  const doc = { body: { closest: () => null }, activeElement: rows[1].controls[0], getElementById: id => id === 'observation-viewport' ? (disposed ? null : root) : scroll,
    addEventListener: (name, fn) => events.set(name, fn), removeEventListener: name => events.delete(name) }
  const win = { addEventListener: (name, fn) => events.set('window:' + name, fn), removeEventListener: name => events.delete('window:' + name) }
  const bridge = installObservationViewport({ ports: { onObservationViewport: { send: value => sent.push(value) },
    syncObservationViewport: { subscribe: fn => { sync = fn }, unsubscribe: fn => { assert.equal(fn, sync); sync = null } } } }, {
    document: doc, window: win,
    ResizeObserver: fallback ? null : class { constructor(fn) { resized = fn } observe(value) { observed.add(value) } unobserve(value) { observed.delete(value) } disconnect() { observed.clear() } },
    MutationObserver: class { constructor(fn) { mutations = fn } observe() {} disconnect() { mutations = null } },
    requestAnimationFrame: fn => { const id = ++nextFrame; frames.set(id, fn); return id }, cancelAnimationFrame: id => frames.delete(id)
  })
  return { stamp, root, panel, scroll, doc, frames, sent, effects, observed, bridge,
    sync(change = {}) { sync({ stamp, navigationToken: 0, keys: ['0', '1', '2'], target: null, adjustment: 0, top: null, settled: true, ...change }) },
    flush() { const queued = [...frames.values()]; frames.clear(); queued.forEach(fn => fn()) },
    mount(key) { rows.push(row(key)); mutations?.() },
    replace(key) { rows = rows.map(value => value.dataset.observationKey === key ? row(key) : value); mutations?.() },
    move(key) { rows = [...rows.filter(value => value.dataset.observationKey !== key), ...rows.filter(value => value.dataset.observationKey === key)]; mutations?.() },
    retire() { disposed = true; mutations?.() },
    keyboard(backwards = false) { let prevented = false; events.get('keydown')({ key: 'Tab', target: doc.activeElement, shiftKey: backwards, preventDefault() { prevented = true } }); return prevented },
    resize() { resized?.() },
    events
  }
}

test('bridge measures only mounted rows and ignores a changed painted context', () => {
  const h = harness(); h.sync(); h.flush()
  assert.deepEqual(h.sent[0].measurements.map(value => value.key), ['0', '1'])
  assert.equal(h.observed.size, 3, 'two wrappers plus the current scroller')
  h.root.dataset.observationViewportContext = JSON.stringify({ ...h.stamp, epoch: 3 })
  h.resize(); h.flush(); assert.equal(h.sent.length, 1)
  h.bridge.dispose(); assert.equal(h.observed.size, 0)
})

test('offscreen Tab requests exact next key and focuses only after stamped mount', () => {
  const h = harness(); h.sync(); h.flush()
  assert.equal(h.keyboard(), true)
  assert.deepEqual(h.sent.at(-1).target, { key: '2', edge: 'first' })
  assert.deepEqual(h.effects, [])
  h.sync({ target: { key: '2', edge: 'first', offset: 360 } }); h.flush()
  assert.deepEqual(h.effects, [])
  h.mount('2'); h.flush()
  assert.deepEqual(h.effects, ['focus:2', 'scroll:2'])
  assert.equal(h.scroll.scrollTop, 460)
  h.bridge.dispose()
})

test('retired pending mount cannot revive after unmount or stale destination', () => {
  const h = harness(); h.sync(); h.flush()
  h.sync({ target: { key: '2', edge: 'first', offset: 360 } })
  h.retire(); h.flush(); h.mount('2'); h.flush()
  assert.deepEqual(h.effects, [])
  h.bridge.dispose(); assert.equal(h.frames.size, 0); assert.equal(h.events.size, 0)
})

test('mounted fallback measurement and native anchor adjustment work without ResizeObserver', () => {
  const h = harness({ fallback: true }); h.sync(); h.flush()
  assert.equal(h.sent[0].measurements.length, 2)
  h.scroll.scrollTop = 500
  h.sync({ keys: null, adjustment: 70 }); h.flush()
  assert.equal(h.scroll.scrollTop, 570)
  h.bridge.dispose()
})

test('native Tab skips noninteractive path labels while retaining exact next card ownership', () => {
  const h = harness(); h.sync({ keys: ['0', '1', 'observation-path-label', '2'] }); h.flush()
  assert.equal(h.keyboard(), true)
  assert.deepEqual(h.sent.at(-1).target, { key: '2', edge: 'first' })
  h.bridge.dispose()
})

test('a superseding layout revision retires a queued old-width anchor correction', () => {
  const h = harness(); h.sync(); h.flush(); h.scroll.scrollTop = 500
  h.sync({ adjustment: 70 }); h.sync({ stamp: { ...h.stamp, revision: 2 }, adjustment: 0 })
  h.root.dataset.observationViewportContext = JSON.stringify({ ...h.stamp, revision: 2 })
  h.flush(); assert.equal(h.scroll.scrollTop, 500)
  h.bridge.dispose()
})

test('native focus reports the current painted owner before queued geometry can evict it', () => {
  const h = harness(); h.sync(); h.flush()
  h.doc.activeElement = h.root.querySelectorAll()[0].controls[0]
  h.events.get('focusin')()
  assert.equal(h.sent.at(-1).focus, '0', 'receipt is synchronous, before the queued frame')
  h.doc.activeElement = { closest: () => null }
  h.events.get('focusin')()
  assert.equal(h.sent.at(-1).focus, '@outside', 'only an actual native outside focus change retires a pin')
  const count = h.sent.length
  h.root.dataset.observationViewportContext = JSON.stringify({ ...h.stamp, revision: 2 })
  h.doc.activeElement = h.root.querySelectorAll()[1].controls[0]
  h.events.get('focusin')()
  assert.equal(h.sent.length, count, 'retired DOM/model stamp cannot claim native ownership')
  h.bridge.dispose()
})

test('an acknowledged native focus owner recovers only the same moved control after body focus loss', () => {
  const h = harness(); h.sync(); h.flush()
  h.doc.activeElement = h.root.querySelectorAll()[0].controls[0]; h.events.get('focusin')()
  h.root.dataset.observationFocusKey = '0'; h.sync({ settled: false }); h.flush()
  assert.deepEqual(h.effects, [], 'acknowledgement alone does not retire the still-focused physical owner')
  h.move('0'); h.doc.activeElement = h.doc.body
  h.root.dataset.observationFocusKey = '0'; h.sync(); h.flush()
  assert.deepEqual(h.effects, [], 'changed mounted geometry needs its own accepted model acknowledgement')
  h.sync(); h.flush(); h.flush()
  assert.deepEqual(h.effects, ['focus:0'])
  h.doc.activeElement = h.doc.body; h.move('0'); h.flush()
  assert.deepEqual(h.effects, ['focus:0'], 'one intent, consumed before focus')
  h.bridge.dispose()
})

for (const retirement of ['outside', 'stale-revision', 'navigation', 'unmount', 'control-incompatibility', 'different-control-node']) test('native focus movement intent retires on ' + retirement, () => {
  const h = harness(); h.sync(); h.flush()
  h.doc.activeElement = h.root.querySelectorAll()[0].controls[0]; h.events.get('focusin')()
  h.move('0'); h.doc.activeElement = h.doc.body; h.root.dataset.observationFocusKey = '0'
  if (retirement === 'outside') h.events.get('focusin')()
  if (retirement === 'stale-revision') {
    h.root.dataset.observationViewportContext = JSON.stringify({ ...h.stamp, revision: 2 })
  }
  if (retirement === 'navigation') h.panel.dataset.observationContext = 'navigation2'
  if (retirement === 'unmount') h.retire()
  if (retirement === 'control-incompatibility') h.root.querySelectorAll().find(row => row.dataset.observationKey === '0').controls[0].className = 'different-action'
  if (retirement === 'different-control-node') h.replace('0')
  h.sync(); h.flush(); h.flush(); assert.deepEqual(h.effects, [])
  h.bridge.dispose()
})

test('a native owner transfers only after its current successor revision acknowledges stable geometry and exact control', () => {
  const h = harness(); h.sync(); h.flush()
  h.doc.activeElement = h.root.querySelectorAll()[0].controls[0]; h.events.get('focusin')()
  h.move('0'); h.doc.activeElement = h.doc.body; h.root.dataset.observationFocusKey = '0'
  const next = { ...h.stamp, revision: 2 }
  h.root.dataset.observationViewportContext = JSON.stringify(next)
  h.sync({ stamp: next, settled: false, adjustment: 20 }); h.flush()
  assert.deepEqual(h.effects, [])
  h.sync({ stamp: next, settled: true }); h.flush()
  assert.deepEqual(h.effects, [], 'current settled receipt still requires the next paint')
  h.flush(); assert.deepEqual(h.effects, ['focus:0'])
  h.bridge.dispose()
})

test('navigation waits for accepted geometry and claims the scroller without replaying retired corrections', () => {
  const h = harness(), ready = []
  const command = { intent: 'detail', destination: JSON.parse(h.panel.dataset.observationContext), viewport: h.stamp }
  h.sync({ settled: false }); h.flush()
  h.bridge.awaitNavigation(command, value => ready.push(value)); h.flush()
  assert.equal(ready.length, 0, 'painted measurements alone do not prove model settlement')
  h.sync(); h.flush(); assert.equal(ready.length, 1)
  assert.equal(h.bridge.claimNavigation(ready[0]), true)
  h.sync({ settled: false, adjustment: 70 }); h.flush()
  assert.equal(h.scroll.scrollTop, 0, 'late height correction cannot displace an owned detail reveal')
  const next = { ...h.stamp, revision: 2 }
  h.root.dataset.observationViewportContext = JSON.stringify(next)
  h.sync({ stamp: next, settled: false, adjustment: 90 }); h.flush()
  assert.equal(h.scroll.scrollTop, 0, 'current successor layout does not take shared scroll authority')
  h.events.get('wheel')(); h.flush()
  assert.equal(h.scroll.scrollTop, 0, 'releasing authority does not replay discarded work')
  h.sync({ stamp: next, adjustment: 20 }); h.flush(); assert.equal(h.scroll.scrollTop, 20)
  h.bridge.dispose()
})

test('superseded navigation-token corrections and pending reveals cannot revive', () => {
  const h = harness(), ready = []
  h.sync({ settled: false }); h.flush()
  h.bridge.awaitNavigation({ intent: 'detail', destination: JSON.parse(h.panel.dataset.observationContext), viewport: h.stamp }, value => ready.push(value))
  h.panel.dataset.observationContext = JSON.stringify({ workspaceId: 'repo', sessionEpoch: 2, token: 1, selectedId: 'A' })
  h.sync({ navigationToken: 1, settled: false }); h.flush()
  h.sync({ navigationToken: 0, adjustment: 100 }); h.flush()
  assert.equal(h.scroll.scrollTop, 0); assert.equal(ready.length, 0)
  h.bridge.dispose()
})

test('arming asks for one fresh receipt and unchanged acknowledged geometry does not create a frame loop', () => {
  const h = harness()
  h.sync(); h.flush(); assert.equal(h.sent.length, 1)
  h.sync({ settled: false }); h.flush(); assert.equal(h.sent.length, 2, 'one current arming receipt')
  h.sync(); h.flush(); h.flush(); h.flush()
  assert.equal(h.sent.length, 2); assert.equal(h.frames.size, 0)
  h.sync({ navigationToken: -1, settled: false }); h.flush()
  assert.equal(h.sent.length, 2, 'retired navigation cannot request another read')
  h.bridge.dispose()
})
