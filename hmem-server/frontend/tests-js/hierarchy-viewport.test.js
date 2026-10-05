import { test } from 'node:test'
import assert from 'node:assert/strict'
import { installHierarchyViewport } from '../src/hierarchy-viewport.js'

function harness({ resizeObserver = true } = {}) {
  const listeners = new Map(), windowListeners = new Map(), frames = new Map(), sent = [], observers = []
  let nextFrame = 0, rows = [], viewport = true
  const elements = new Map()
  const stamp = { workspace: 'ws', epoch: 1, generation: 2, revision: 3 }
  const scroller = { scrollTop: 0, clientHeight: 600, getBoundingClientRect: () => ({ top: 0 }), addEventListener() {}, removeEventListener() {} }
  const container = { dataset: { hierarchyContext: JSON.stringify(stamp) }, contains: row => rows.includes(row), querySelectorAll: () => rows, getBoundingClientRect: () => ({ top: -scroller.scrollTop }) }
  const doc = { documentElement: {}, activeElement: null,
    getElementById(id) { if (elements.has(id)) return elements.get(id); if (id === 'main-content-scroll') return scroller; if (id === 'hierarchy-viewport') return viewport ? container : null; return rows.find(row => 'entity-' + row.dataset.hierarchyKey.split(':')[1] === id) || null },
    addEventListener(name, fn) { listeners.set(name, fn) }, removeEventListener(name) { listeners.delete(name) }
  }
  const callbacks = {}
  const app = { ports: { onHierarchyViewport: { send: value => sent.push(value) }, syncHierarchyViewport: { subscribe: fn => { callbacks.sync = fn }, unsubscribe() {} }, scrollHierarchyTarget: { subscribe: fn => { callbacks.target = fn }, unsubscribe() {} } } }
  class Resize { constructor() { this.rows = new Set(); observers.push(this) } observe(row) { this.rows.add(row) } unobserve(row) { this.rows.delete(row) } disconnect() { this.rows.clear() } }
  const mutationCallbacks = []
  class Mutation { constructor(callback) { mutationCallbacks.push(callback) } observe() {} disconnect() {} }
  const win = { addEventListener(name, fn) { windowListeners.set(name, fn) }, removeEventListener(name) { windowListeners.delete(name) }, getComputedStyle(control) { return control.style || { display: 'block', visibility: 'visible' } } }
  const bridge = installHierarchyViewport(app, { document: doc, window: win, ResizeObserver: resizeObserver ? Resize : undefined, MutationObserver: Mutation,
    requestAnimationFrame(fn) { const id = ++nextFrame; frames.set(id, fn); return id }, cancelAnimationFrame(id) { frames.delete(id) } })
  function row(key, top, height = 160, next = '') {
    const value = { dataset: { hierarchyKey: key, hierarchyNext: next, hierarchyPrevious: '' }, getBoundingClientRect: () => ({ top: top - scroller.scrollTop, height }), closest: selector => selector === '[data-hierarchy-key]' ? value : null, contains: control => control === value.button }
    const button = { isConnected: true, tabIndex: 0, closest: selector => selector === '[data-hierarchy-key]' ? value : null, getClientRects: () => [{}], focus() { doc.activeElement = button } }
    value.querySelector = () => button; value.querySelectorAll = () => [button]; value.button = button
    return value
  }
  function flush() { const current = [...frames.values()]; frames.clear(); for (const fn of current) fn() }
  return { bridge, row, flush, sent, callbacks, stamp, scroller, doc, observers, listeners, windowListeners, elements,
    setRows(value) { rows = value }, setStamp(value) { container.dataset.hierarchyContext = JSON.stringify(value) }, mutate(records) { for (const callback of mutationCallbacks) callback(records) }, hide() { viewport = false } }
}

test('only a proven same-row move restores the exact retained native control', () => {
  const h = harness(), row = h.row('project:p', 0)
  h.setRows([row]); h.flush(); row.button.focus(); h.listeners.get('focusin')({ target: row.button })
  h.doc.activeElement = null; h.listeners.get('focusout')({ target: row.button, relatedTarget: null })
  h.mutate([{ removedNodes: [row], addedNodes: [row] }]); h.flush()
  assert.equal(h.doc.activeElement, row.button)
  h.bridge.dispose()
})

test('native recovery rejects unproven blur, outside pointer intent and changed lifetime', () => {
  for (const retirement of ['no-move', 'pointer', 'lifetime', 'tab', 'outside-row']) {
    const h = harness(), row = h.row('project:p', 0)
    h.setRows([row]); h.flush(); row.button.focus(); h.listeners.get('focusin')({ target: row.button })
    h.doc.activeElement = null; h.listeners.get('focusout')({ target: row.button, relatedTarget: null })
    if (retirement === 'pointer') h.listeners.get('pointerdown')({ target: {} })
    if (retirement === 'lifetime') h.callbacks.sync({ ...h.stamp, epoch: 2 })
    if (retirement === 'tab') h.listeners.get('keydown')({ key: 'Tab', target: row.button, ctrlKey: true })
    if (retirement === 'outside-row') row.contains = () => false
    if (retirement !== 'no-move') h.mutate([{ removedNodes: [row], addedNodes: [row] }])
    h.flush(); assert.equal(h.doc.activeElement, null, retirement)
    h.bridge.dispose()
  }
})

test('native recovery cannot combine movement and blur from different paint episodes', () => {
  for (const first of ['move', 'blur']) {
    const h = harness(), row = h.row('project:p', 0)
    h.setRows([row]); h.flush(); row.button.focus(); h.listeners.get('focusin')({ target: row.button })
    const move = () => h.mutate([{ removedNodes: [row], addedNodes: [row] }])
    const blur = () => { h.doc.activeElement = null; h.listeners.get('focusout')({ target: row.button, relatedTarget: null }) }
    if (first === 'move') move(); else blur()
    h.flush()
    if (first === 'move') blur(); else move()
    h.flush(); assert.equal(h.doc.activeElement, null, first)
    h.bridge.dispose()
  }
})

test('history and fragment navigation retire native recovery even when the hierarchy stamp is unchanged', () => {
  for (const event of ['hashchange', 'popstate']) {
    const h = harness(), row = h.row('project:p', 0)
    h.setRows([row]); h.flush(); row.button.focus(); h.listeners.get('focusin')({ target: row.button })
    h.doc.activeElement = null; h.listeners.get('focusout')({ target: row.button, relatedTarget: null })
    h.mutate([{ removedNodes: [row], addedNodes: [row] }]); h.windowListeners.get(event)(); h.flush()
    assert.equal(h.doc.activeElement, null, event)
    h.bridge.dispose(); assert.equal(h.windowListeners.has(event), false)
  }
})

test('native focus captures the painted row stamp rather than an older bridge receipt', () => {
  const h = harness(), row = h.row('project:p', 0), next = { ...h.stamp, generation: 3 }
  h.setRows([row]); h.flush(); h.setStamp(next); h.callbacks.sync(next)
  row.button.focus(); h.listeners.get('focusin')({ target: row.button })
  h.doc.activeElement = null; h.listeners.get('focusout')({ target: row.button, relatedTarget: null })
  h.mutate([{ removedNodes: [row], addedNodes: [row] }]); h.flush()
  assert.equal(h.doc.activeElement, row.button)
  h.bridge.dispose()
})

test('mounted-row observers report exact wrapper heights and release detached rows', () => {
  const h = harness(), first = h.row('project:a', 0, 206), last = h.row('task:z', 10000, 86)
  h.setRows([first, last]); h.flush()
  assert.deepEqual(h.sent.at(-1).measurements, [{ key: 'project:a', height: 206 }, { key: 'task:z', height: 86 }])
  assert.equal(h.observers[0].rows.size, 2)
  h.setRows([last]); h.bridge.flush(); assert.equal(h.observers[0].rows.has(first), false)
  h.hide(); h.bridge.flush(); assert.equal(h.observers[0].rows.size, 0)
  h.bridge.dispose()
})

test('missing ResizeObserver still measures only mounted rows and retains native focus', () => {
  const h = harness({ resizeObserver: false }), first = h.row('project:a', 0)
  h.setRows([first]); h.doc.activeElement = first.button; h.flush()
  assert.deepEqual(h.sent.at(-1).pins, ['project:a'])
  assert.equal(h.sent.at(-1).measurements.length, 1)
  h.bridge.dispose()
})

test('stale layout stamps cannot change the active scroll position', () => {
  const h = harness(), first = h.row('project:a', 0)
  h.setRows([first]); h.flush(); h.scroller.scrollTop = 120
  h.callbacks.sync({ ...h.stamp, generation: 1, top: 9000 }); h.flush()
  assert.equal(h.scroller.scrollTop, 120)
  h.bridge.dispose()
})

test('direct focus first requests logical mounting and scrolls only after stamped mount', () => {
  const h = harness(), first = h.row('project:a', 0)
  h.setRows([first]); h.flush(); h.callbacks.target('entity-z'); h.flush()
  assert.equal(h.sent.at(-1).request, 'entity:z'); assert.equal(h.scroller.scrollTop, 0)
  const target = h.row('task:z', 9000)
  h.setRows([target]); h.callbacks.sync({ ...h.stamp, anchor: null, delta: 0, top: 9000, target: 'task:z' }); h.flush()
  assert.equal(h.scroller.scrollTop, 9000); assert.equal(h.sent.at(-1).acknowledged, true)
  h.bridge.dispose()
})

test('keyboard mounts the next logical entity before focusing it', () => {
  const h = harness(), first = h.row('project:a', 0, 160, 'project:z')
  h.setRows([first]); h.flush()
  let prevented = false
  h.listeners.get('keydown')({ key: 'Tab', target: first.button, preventDefault() { prevented = true } }); h.flush()
  assert.equal(prevented, true); assert.equal(h.sent.at(-1).request, 'project:z')
  const target = h.row('project:z', 9000)
  h.setRows([first, target]); h.callbacks.sync({ ...h.stamp, anchor: null, top: 9000, target: 'project:z' }); h.flush()
  assert.equal(h.doc.activeElement, target.button)
  h.bridge.dispose()
})

test('layout anchors preserve a measured row and its intrarow offset', () => {
  const h = harness(), anchor = h.row('project:a', 700)
  h.setRows([anchor]); h.flush(); h.scroller.scrollTop = 500
  h.callbacks.sync({ ...h.stamp, anchor: 'project:a', delta: 15, top: 715 }); h.flush()
  assert.equal(h.scroller.scrollTop, 715)
  h.bridge.dispose()
})

test('a newer physical scroll wins over an older matching-stamp measurement layout', () => {
  const h = harness(), row = h.row('project:a', 700)
  h.setRows([row]); h.flush(); h.scroller.scrollTop = 100
  h.callbacks.sync({ ...h.stamp, anchor: 'project:a', delta: 15, top: 715 })
  h.scroller.scrollTop = 500; h.flush()
  assert.equal(h.scroller.scrollTop, 500)
  assert.equal(h.sent.at(-1).top, 500)
  h.bridge.dispose()
})

test('a layout admitted after a bridge-owned scroll still preserves its new measurement anchor', () => {
  const h = harness(), row = h.row('project:a', 700)
  h.setRows([row]); h.flush(); h.scroller.scrollTop = 100
  h.callbacks.sync({ ...h.stamp, anchor: 'project:a', delta: 15, top: 715 }); h.flush()
  assert.equal(h.scroller.scrollTop, 715)
  row.getBoundingClientRect = () => ({ top: 900 - h.scroller.scrollTop, height: 200 })
  h.callbacks.sync({ ...h.stamp, anchor: 'project:a', delta: 15, top: 915 }); h.flush()
  assert.equal(h.scroller.scrollTop, 915)
  h.bridge.dispose()
})


test('backward keyboard entry selects the last eligible control', () => {
  const h = harness(), current = h.row('project:z', 9000)
  current.dataset.hierarchyPrevious = 'project:a'
  h.setRows([current]); h.flush()
  h.listeners.get('keydown')({ key: 'Tab', shiftKey: true, target: current.button, preventDefault() {} }); h.flush()
  const target = h.row('project:a', 0), last = { tabIndex: 0, closest: () => null, getClientRects: () => [{}], focus() { h.doc.activeElement = last } }
  target.querySelectorAll = () => [target.button, last]
  h.setRows([target, current]); h.callbacks.sync({ ...h.stamp, anchor: null, top: 0, target: 'project:a' }); h.flush()
  assert.equal(h.doc.activeElement, last)
  h.bridge.dispose()
})

test('a pending keyboard target is retired on a filter generation change', () => {
  const h = harness(), first = h.row('project:a', 0, 160, 'project:z')
  h.setRows([first]); h.flush()
  h.listeners.get('keydown')({ key: 'Tab', target: first.button, preventDefault() {} }); h.flush()
  const target = h.row('project:z', 9000)
  h.setRows([target]); h.setStamp({ ...h.stamp, generation: 3, revision: 4 }); h.bridge.flush()
  assert.equal(h.sent.at(-1).request, null)
  assert.equal(h.doc.activeElement, null)
  h.bridge.dispose()
})


test('Markdown links participate in keyboard edges while hidden and negative-tabindex controls do not', () => {
  const h = harness(), first = h.row('project:a', 0, 160, 'project:z')
  const link = { tagName: 'A', tabIndex: 0, closest: selector => selector === '[data-hierarchy-key]' ? first : null, getClientRects: () => [{}] }
  const hidden = { tabIndex: 0, closest: () => null, getClientRects: () => [], style: { display: 'none', visibility: 'visible' } }
  const negative = { tabIndex: -1, closest: () => null, getClientRects: () => [{}] }
  first.querySelectorAll = selector => { assert.ok(selector.includes('a[href]')); return [first.button, link, hidden, negative] }
  h.setRows([first]); h.flush()
  let prevented = false
  h.listeners.get('keydown')({ key: 'Tab', target: link, preventDefault() { prevented = true } }); h.flush()
  assert.equal(prevented, true); assert.equal(h.sent.at(-1).request, 'project:z')
  h.bridge.dispose()
})


test('observation-detail scrolling waits for a delayed non-hierarchy mount', () => {
  const h = harness()
  h.hide(); h.flush()
  h.callbacks.sync(h.stamp); h.flush()
  let scrolls = 0
  h.callbacks.target('observation-detail'); h.flush()
  assert.equal(scrolls, 0)
  h.elements.set('observation-detail', { closest: () => null, scrollIntoView(options) { assert.deepEqual(options, { block: 'center' }); scrolls++ } })
  h.bridge.flush()
  assert.equal(scrolls, 1)
  h.bridge.flush(); assert.equal(scrolls, 1)
  h.bridge.dispose()
})


test('a delayed non-hierarchy target is retired by workspace or session context without a hierarchy container', () => {
  for (const changed of [{ workspace: 'other' }, { epoch: 2 }]) {
    const h = harness()
    h.hide(); h.flush(); h.callbacks.sync(h.stamp); h.flush()
    let scrolls = 0
    h.callbacks.target('observation-detail'); h.flush()
    h.callbacks.sync({ ...h.stamp, ...changed }); h.flush()
    h.elements.set('observation-detail', { scrollIntoView() { scrolls++ } })
    h.bridge.flush(); assert.equal(scrolls, 0)
    h.bridge.dispose()
  }
})

function reverseFixture(mounted = true) {
  const h = harness(), previous = h.row('project:a', 0), current = h.row('project:b', 700)
  const last = { tabIndex: 0, closest: () => null, getClientRects: () => [{}], focus() { h.doc.activeElement = last } }
  previous.querySelectorAll = () => [previous.button, last]
  current.dataset.hierarchyPrevious = 'project:a'
  h.setRows(mounted ? [previous, current] : [current]); h.flush()
  const reverse = () => h.listeners.get('keydown')({ key: 'Tab', shiftKey: true, target: current.button, preventDefault() {} })
  const layout = target => ({ ...h.stamp, anchor: null, top: 0, target })
  return { h, previous, current, last, reverse, layout }
}

test('a mounted reverse keyboard target wins over an earlier admitted layout target', () => {
  const { h, last, reverse, layout } = reverseFixture()
  h.callbacks.sync(layout('project:b')); reverse(); h.flush()
  assert.equal(h.doc.activeElement, last)
  h.bridge.dispose()
})

test('an offscreen reverse keyboard target stays requested instead of the earlier layout target', () => {
  const { h, reverse, layout } = reverseFixture(false)
  h.callbacks.sync(layout('project:b')); reverse(); h.flush()
  assert.equal(h.sent.at(-1).request, 'project:a')
  assert.equal(h.doc.activeElement, null)
  h.bridge.dispose()
})

test('a repeated same-stamp layout echo cannot become a newer keyboard intent', () => {
  const { h, last, reverse, layout } = reverseFixture()
  h.callbacks.sync(layout('project:b')); reverse(); h.callbacks.sync(layout('project:b')); h.flush()
  assert.equal(h.doc.activeElement, last)
  h.bridge.dispose()
})

test('a genuinely newer different layout target supersedes keyboard without borrowing its focus mode', () => {
  const { h, current, reverse, layout } = reverseFixture()
  const route = h.row('task:c', 1400)
  h.setRows([current, route]); h.flush()
  reverse(); h.callbacks.sync(layout('task:c')); h.flush()
  assert.equal(h.scroller.scrollTop, 1400)
  assert.equal(h.doc.activeElement, null)
  assert.equal(h.sent.at(-1).acknowledged, true)
  h.bridge.dispose()
})

test('a newer direct route target supersedes the pending keyboard predecessor', () => {
  const { h, reverse } = reverseFixture()
  reverse(); h.callbacks.target('entity-b'); h.flush()
  assert.equal(h.scroller.scrollTop, 700)
  assert.equal(h.doc.activeElement, null)
  h.bridge.dispose()
})

test('a newer painted stamp retires the old keyboard target and accepts its layout target', () => {
  const { h, reverse, layout } = reverseFixture()
  reverse(); const next = { ...layout('project:b'), revision: h.stamp.revision + 1 }
  h.callbacks.sync(next); h.setStamp(next); h.flush()
  assert.equal(h.scroller.scrollTop, 700)
  assert.equal(h.doc.activeElement, null)
  assert.equal(h.sent.at(-1).request, null)
  h.bridge.dispose()
})


test('a completed reverse target keeps its physical scroll and focus against a later old layout echo', () => {
  const { h, last, reverse, layout } = reverseFixture()
  h.callbacks.sync({ ...layout('project:b'), top: 700 }); reverse(); h.flush()
  assert.equal(h.doc.activeElement, last); assert.equal(h.scroller.scrollTop, 0)
  h.callbacks.sync({ ...layout('project:b'), top: 700 }); h.flush()
  assert.equal(h.doc.activeElement, last); assert.equal(h.scroller.scrollTop, 0)
  assert.equal(h.sent.at(-1).request, null); assert.equal(h.sent.at(-1).acknowledged, false)
  h.bridge.dispose()
})

test('genuine pointer focus and history intents cannot reauthorize an older echoed target', () => {
  for (const intent of ['pointer', 'focus', 'hashchange', 'popstate']) {
    const { h, current, reverse, layout } = reverseFixture()
    h.callbacks.sync({ ...layout('project:b'), top: 700 }); reverse(); h.flush()
    h.scroller.scrollTop = 80; h.doc.activeElement = current.button
    if (intent === 'pointer') h.listeners.get('pointerdown')({ target: current.button })
    else if (intent === 'focus') h.listeners.get('focusin')({ target: current.button })
    else h.windowListeners.get(intent)()
    h.callbacks.sync({ ...layout('project:b'), top: 700 }); h.flush()
    assert.equal(h.scroller.scrollTop, 80, intent)
    assert.equal(h.doc.activeElement, current.button, intent)
    assert.equal(h.sent.at(-1).acknowledged, false, intent)
    h.bridge.dispose()
  }
})


test('bridge-owned recovery emits native focusin without retiring a newer mounted or offscreen route target', () => {
  for (const mounted of [true, false]) {
    const h = harness(), retained = h.row('project:b', 0), route = h.row('task:c', 1400)
    retained.button.focus = () => { h.doc.activeElement = retained.button; h.listeners.get('focusin')({ target: retained.button }) }
    h.setRows([retained]); h.flush(); retained.button.focus()
    h.doc.activeElement = null; h.listeners.get('focusout')({ target: retained.button, relatedTarget: null })
    h.mutate([{ removedNodes: [retained], addedNodes: [retained] }])
    if (mounted) h.setRows([retained, route])
    h.callbacks.sync({ ...h.stamp, anchor: null, top: 0, target: 'task:c' }); h.flush()
    assert.equal(h.doc.activeElement, retained.button)
    if (mounted) { assert.equal(h.scroller.scrollTop, 1400); assert.equal(h.sent.at(-1).acknowledged, true) }
    else assert.equal(h.sent.at(-1).request, 'task:c')
    h.bridge.dispose()
  }
})
