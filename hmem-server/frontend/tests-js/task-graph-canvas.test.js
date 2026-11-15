import assert from 'node:assert/strict'
import test from 'node:test'
import { createTaskGraphCanvas, installTaskGraphCanvas, zoomAt } from '../src/task-graph-canvas.js'

function harness(bounds = { x: -100, y: -50, width: 1000, height: 600 }, options = {}) {
  const handlers = new Map(), captures = new Set(), views = [], measures = [], attrs = new Map()
  let observer
  const canvas = { clientWidth: 800, clientHeight: 600, dataset: {}, classList: { add() {}, remove() {} },
    getBoundingClientRect: () => ({ left: 0, top: 0 }), querySelector: () => ({ setAttribute: (key, value) => attrs.set(key, value) }), querySelectorAll: () => [],
    addEventListener: (kind, value) => handlers.set(kind, value), removeEventListener: kind => handlers.delete(kind),
    setPointerCapture: id => captures.add(id), hasPointerCapture: id => captures.has(id), releasePointerCapture: id => captures.delete(id) }
  const metadata = { key: 'scoped-entry', bounds, selected: options.selected || { x: 0, y: 0 }, initialAnchor: options.initialAnchor, measured: false }
  const controller = createTaskGraphCanvas(canvas, metadata, {
    saved: Object.hasOwn(options, 'saved') ? options.saved : { x: 0, y: 0, scale: 1 }, now: options.now,
    save: value => views.push(value), measure: value => measures.push(value),
    ResizeObserverImpl: class { constructor(callback) { observer = callback } observe() {} unobserve() {} disconnect() {} }
  })
  return { canvas, controller, captures, views, measures, handlers, metadata,
    event(kind, fields = {}) { const event = { button: 0, pointerType: 'mouse', pointerId: 1, clientX: 100, clientY: 100, detail: 1, target: { closest: () => canvas }, preventDefault() { this.prevented = true }, stopImmediatePropagation() { this.stopped = true }, ...fields }; handlers.get(kind)(event); return event },
    measure: () => observer(), view: () => JSON.parse(canvas.dataset.graphTransform) }
}

test('cursor zoom keeps both world coordinates fixed', () => {
  const old = { x: -40, y: 20, scale: .8 }, point = { x: 450, y: 120 }, next = zoomAt(old, 1.7, point)
  for (const axis of ['x', 'y']) assert.ok(Math.abs((point[axis] - old[axis]) / old.scale - (point[axis] - next[axis]) / next.scale) < 1e-9)
})

test('only the initial unsaved readability fallback centers an in-scope card instead of an empty project center', () => {
  const anchor = { x: 1000, y: 1800 }, h = harness({ x: 0, y: 0, width: 8000, height: 8000 }, { saved: null, selected: { x: 4000, y: 4000 }, initialAnchor: anchor })
  assert.equal(h.view().scale, 1)
  assert.equal(anchor.x + h.view().x, 400); assert.equal(anchor.y + h.view().y, 300)
  h.controller.control('fit'); const complete = h.view()
  assert.ok(complete.scale < 1)
  h.controller.update({ ...h.metadata, measured: true, revision: 2, initialAnchor: { x: 50, y: 70 } })
  assert.deepEqual(h.view(), complete)
  h.controller.dispose()
  const saved = { x: -27, y: 19, scale: .8 }, restored = harness(undefined, { saved, initialAnchor: anchor })
  assert.deepEqual(restored.view(), saved); restored.controller.dispose()
})

test('Fit contains huge world corners and zoom-out remains available below .05', () => {
  const bounds = { x: -20000, y: -50000, width: 200000, height: 100000 }, h = harness(bounds)
  h.controller.control('fit'); const view = h.view()
  assert.ok(view.scale < .01)
  for (const [x, y] of [[bounds.x, bounds.y], [bounds.x + bounds.width, bounds.y + bounds.height]]) {
    assert.ok(x * view.scale + view.x >= 0 && x * view.scale + view.x <= 800)
    assert.ok(y * view.scale + view.y >= 0 && y * view.scale + view.y <= 600)
  }
  h.controller.control('out'); assert.ok(h.view().scale < view.scale); h.controller.dispose()
})

test('drag threshold suppresses only dragged click, never keyboard or a fresh deliberate click', () => {
  const h = harness()
  h.event('pointerdown'); h.event('pointermove', { clientX: 104 }); assert.equal(h.view().x, 0)
  h.event('pointermove', { clientX: 110 }); assert.equal(h.view().x, 10)
  h.event('pointerup', { clientX: 110 }); assert.equal(h.captures.size, 0)
  assert.equal(h.event('click').stopped, true)
  assert.equal(h.event('click', { detail: 0 }).stopped, undefined)
  h.event('pointerdown', { pointerId: 2 }); h.event('pointerup', { pointerId: 2 })
  assert.equal(h.event('click').stopped, undefined)
  h.controller.dispose(); assert.equal(h.handlers.size, 0)
})

test('midpoint pinch preserves its world anchor and pointer cancellation releases ownership', () => {
  const h = harness()
  h.event('pointerdown', { clientX: 0, clientY: 0 })
  h.event('pointerdown', { pointerId: 2, clientX: 100, clientY: 0 })
  h.event('pointermove', { pointerId: 2, clientX: 200, clientY: 0 })
  const view = h.view(); assert.equal(view.scale, 2); assert.equal((100 - view.x) / view.scale, 50)
  h.event('pointercancel'); assert.equal(h.captures.size, 0)
  const count = h.views.length; h.event('pointermove', { clientX: 400 }); assert.equal(h.views.length, count)
  h.controller.dispose()
})

test('intrinsic measurements stay unscaled and equal observations publish once', () => {
  const h = harness(), button = { dataset: { graphTask: 'long' }, offsetHeight: 282, clientHeight: 280, scrollHeight: 280 }
  h.canvas.querySelectorAll = selector => selector === '[data-graph-task]' ? [button] : []
  h.measure(); h.controller.control('in'); h.measure(); h.measure()
  assert.deepEqual(h.measures, [{ key: 'scoped-entry', measurements: [{ id: 'long', height: 282 }], labels: [] }])
  h.controller.dispose()
})

test('untouched provisional camera settles to refined geometry once, then remains fixed', () => {
  const h = harness({ x: 0, y: 0, width: 2000, height: 2000 }, { saved: null, selected: { x: 500, y: 500 } })
  const refined = { ...h.metadata, selected: { x: 1500, y: 1500 }, bounds: { x: 0, y: 0, width: 3000, height: 3000 }, measured: true }
  h.controller.update(refined)
  assert.equal(1500 * h.view().scale + h.view().x, 400)
  assert.equal(1500 * h.view().scale + h.view().y, 300)
  const settled = h.view()
  h.controller.update({ ...refined, selected: { x: 2500, y: 2500 } })
  assert.deepEqual(h.view(), settled); h.controller.dispose()
})

test('saved, user-panned and expired initial cameras never refit on measurement updates', () => {
  for (const kind of ['saved', 'pan', 'zoom', 'wheel', 'keyboard', 'expired']) {
    let clock = 0
    const h = harness(undefined, { saved: kind === 'saved' ? { x: 80, y: 90, scale: .7 } : null, now: () => clock })
    if (kind === 'pan') { h.event('pointerdown'); h.event('pointermove', { clientX: 120 }); h.event('pointerup', { clientX: 120 }) }
    if (kind === 'zoom') h.controller.control('in')
    if (kind === 'wheel') h.event('wheel', { deltaY: -120 })
    if (kind === 'keyboard') h.event('keydown')
    if (kind === 'expired') clock = 1501
    const before = h.view()
    h.controller.update({ ...h.metadata, selected: { x: 1500, y: 1500 }, bounds: { x: 0, y: 0, width: 3000, height: 3000 }, measured: true })
    assert.deepEqual(h.view(), before, kind); h.controller.dispose()
  }
})

test('a new canonical snapshot preserves even an untouched provisional camera', () => {
  const h = harness(undefined, { saved: null })
  const before = h.view()
  h.controller.update({ ...h.metadata, revision: 2, selected: { x: 10000, y: 20000 }, bounds: { x: -300, y: -800, width: 30000, height: 40000 }, measured: true })
  assert.deepEqual(h.view(), before)
  h.controller.dispose()
})

test('one explicit filter refit waits for measurements; ordinary revisions preserve the resulting camera', () => {
  const h = harness(), before = h.view(), count = h.views.length
  const requested = { ...h.metadata, fitRevision: 1, measured: false, bounds: { x: 0, y: 0, width: 400, height: 400 } }
  h.controller.update(requested)
  assert.deepEqual(h.view(), before)
  h.controller.update({ ...requested, measured: true })
  assert.equal(h.views.length, count + 1)
  const fitted = h.view()
  h.controller.update({ ...requested, measured: true, revision: 10, bounds: { x: 0, y: 0, width: 4000, height: 4000 } })
  h.controller.update({ ...requested, measured: true })
  assert.deepEqual(h.view(), fitted)
  assert.equal(h.views.length, count + 1)
  h.controller.dispose()
})

test('explicit filter revision remeasures surviving intrinsic content before its fit', () => {
  const h = harness(), button = { dataset: { graphTask: 'survivor' }, offsetHeight: 92, clientHeight: 90, scrollHeight: 90 }
  h.canvas.querySelectorAll = selector => selector === '[data-graph-task]' ? [button] : []
  h.measure(); h.measure(); assert.equal(h.measures.length, 1)
  h.controller.update({ ...h.metadata, fitRevision: 1, measured: false })
  assert.equal(h.measures.length, 2, 'fresh receipt is needed even when the DOM height stayed equal')
  h.controller.update({ ...h.metadata, fitRevision: 1, measured: true })
  h.controller.dispose()
})

test('a quiet unmeasured explicit refit has a finite deadline and disposal cancels it', async () => {
  const h = harness(), before = h.views.length
  h.controller.update({ ...h.metadata, fitRevision: 1, measured: false })
  await new Promise(resolve => setTimeout(resolve, 1550))
  assert.equal(h.views.length, before + 1)
  h.controller.update({ ...h.metadata, fitRevision: 2, measured: false })
  h.controller.dispose()
  const final = h.views.length
  await new Promise(resolve => setTimeout(resolve, 1550))
  assert.equal(h.views.length, final)
})

test('a hidden return target acknowledges focus on a native filter even with no SVG', () => {
  let paint, changed, focusCalls = 0
  const receipts = [], scene = { dataset: { taskGraphScene: JSON.stringify({ key: 'entry', ready: true, focus: { taskId: 'hidden', token: 3 } }) } }
  const control = { focus(options) { assert.deepEqual(options, { preventScroll: true }); focusCalls++ } }
  const doc = { body: {}, querySelector: selector => selector === '[data-task-graph-scene]' ? scene : null,
    getElementById: id => id === 'task-graph-show-done' ? control : null, addEventListener() {}, removeEventListener() {} }
  const win = { requestAnimationFrame(callback) { paint = callback }, MutationObserver: class { constructor(callback) { changed = callback } observe() {} disconnect() {} } }
  const dispose = installTaskGraphCanvas({ ports: { taskGraphCanvasEvent: { send: receipt => receipts.push(receipt) } } }, { document: doc, window: win })
  paint(); changed(); paint()
  assert.equal(focusCalls, 1)
  assert.deepEqual(receipts, [{ kind: 'focused', key: 'entry', token: 3, fallback: true }])
  dispose()
})

test('empty-scene fits are retired before live repopulation, while a new explicit toggle still fits', () => {
  const h = harness(); h.controller.dispose()
  let paint, changed, click, visible = true
  let metadata = { ...h.metadata, measured: true, fitRevision: 0 }
  const scene = { dataset: {} }
  const doc = { body: {}, querySelector: selector => selector === '[data-task-graph-scene]' ? scene : visible ? h.canvas : null,
    getElementById: () => null, addEventListener(kind, handler) { if (kind === 'click') click = handler }, removeEventListener() {} }
  const win = { requestAnimationFrame(callback) { paint = callback }, MutationObserver: class { constructor(callback) { changed = callback } observe() {} disconnect() {} } }
  const render = () => { scene.dataset.taskGraphScene = JSON.stringify({ key: metadata.key, fitRevision: metadata.fitRevision, ready: true }); h.canvas.dataset.taskGraphCanvas = JSON.stringify(metadata) }
  render()
  const dispose = installTaskGraphCanvas({ ports: { taskGraphCanvasEvent: { send() {} } } }, { document: doc, window: win })
  paint(); click({ target: { closest: () => ({ dataset: { graphControl: 'in' } }) } })
  const saved = h.view()
  visible = false; metadata = { ...metadata, fitRevision: 1 }; render(); changed(); paint()
  visible = true; metadata = { ...metadata, revision: 2, bounds: { x: 0, y: 0, width: 3000, height: 3000 } }; render(); changed(); paint()
  assert.deepEqual(h.view(), saved, 'live repopulation cannot apply an old empty-scene fit')
  // Detail/history unmount and restoration keep the consumed receipt/camera.
  visible = false; scene.dataset.taskGraphScene = 'null'; changed(); paint()
  visible = true; render(); changed(); paint(); assert.deepEqual(h.view(), saved)
  metadata = { ...metadata, fitRevision: 2 }; render(); changed(); paint()
  assert.ok(h.view().scale < saved.scale, 'a genuinely new explicit request still fits')
  dispose()
})
