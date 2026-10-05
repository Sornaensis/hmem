import { test } from 'node:test'
import assert from 'node:assert/strict'
import { installHierarchyViewport } from '../src/hierarchy-viewport.js'

function harness({ resizeObserver = true } = {}) {
  const listeners = new Map(), frames = new Map(), sent = [], observers = []
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
  class Mutation { observe() {} disconnect() {} }
  const win = { addEventListener() {}, removeEventListener() {}, getComputedStyle(control) { return control.style || { display: 'block', visibility: 'visible' } } }
  const bridge = installHierarchyViewport(app, { document: doc, window: win, ResizeObserver: resizeObserver ? Resize : undefined, MutationObserver: Mutation,
    requestAnimationFrame(fn) { const id = ++nextFrame; frames.set(id, fn); return id }, cancelAnimationFrame(id) { frames.delete(id) } })
  function row(key, top, height = 160, next = '') {
    const value = { dataset: { hierarchyKey: key, hierarchyNext: next, hierarchyPrevious: '' }, getBoundingClientRect: () => ({ top: top - scroller.scrollTop, height }), closest: selector => selector === '[data-hierarchy-key]' ? value : null }
    const button = { tabIndex: 0, closest: selector => selector === '[data-hierarchy-key]' ? value : null, getClientRects: () => [{}], focus() { doc.activeElement = button } }
    value.querySelector = () => button; value.querySelectorAll = () => [button]; value.button = button
    return value
  }
  function flush() { const current = [...frames.values()]; frames.clear(); for (const fn of current) fn() }
  return { bridge, row, flush, sent, callbacks, stamp, scroller, doc, observers, listeners, elements,
    setRows(value) { rows = value }, setStamp(value) { container.dataset.hierarchyContext = JSON.stringify(value) }, hide() { viewport = false } }
}

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
