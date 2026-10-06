import assert from 'node:assert/strict'
import test from 'node:test'
import { installObservationNavigation } from '../src/observation-navigation.js'

function harness({ toolbarEnabled = false, viewportBridge } = {}) {
  let subscribed, toolbarSubscribed, captureClick, current = { workspaceId: 'workspace', sessionEpoch: 3, token: 0, selectedId: null }
  const events = new Map()
  const frames = new Map(), elements = new Map(), effects = []
  let mutations, observerStopped = false
  let token = 0
  const scroll = { scrollTop: 123, getBoundingClientRect: () => ({ top: 0, bottom: 600 }) }
  let root = { dataset: { observationContext: JSON.stringify(current) }, contains: element => element.inPanel }
  const element = (id, card = false) => ({ id, inPanel: true, hidden: false, classList: { contains: name => card && name === 'observation-card' },
    closest: () => null, getClientRects() { return this.hidden ? [] : [{}] }, getBoundingClientRect: () => ({ top: 100, bottom: 200 }),
    focus(options) { effects.push({ id, options }); doc.activeElement = this }, scrollIntoView() { effects.push({ scroll: id }); scroll.scrollTop = 900 } })
  const heading = element('observation-detail-heading'), results = element('observation-results')
  elements.set(heading.id, heading); elements.set(results.id, results)
  elements.set('first', element('first', true)); elements.set('duplicate', element('duplicate', true))
  elements.set('main-content-scroll', scroll); elements.set('observation-panel', root)
  const doc = { documentElement: {}, body: {}, activeElement: null, getElementById(id) { return elements.get(id) }, addEventListener(name, fn, capture) { assert.equal(capture, true); if (name === 'click') captureClick = fn; else events.set(name, fn) }, removeEventListener(name, fn) { if (name === 'click') { assert.equal(fn, captureClick); captureClick = null } else events.delete(name) } }
  const bridge = installObservationNavigation({ ports: { navigateObservationDetail: { subscribe(fn) { subscribed = fn }, unsubscribe(fn) { assert.equal(fn, subscribed); subscribed = null } },
    ...(toolbarEnabled ? { focusObservationToolbar: { subscribe(fn) { toolbarSubscribed = fn }, unsubscribe(fn) { assert.equal(fn, toolbarSubscribed); toolbarSubscribed = null } } } : {}) } }, {
    document: doc, viewport: viewportBridge, window: { getComputedStyle() { return { display: 'block', visibility: 'visible' } } },
    MutationObserver: class { constructor(fn) { mutations = fn } observe(target, options) { assert.deepEqual(options, { childList: true, subtree: true }) } disconnect() { observerStopped = true } },
    requestAnimationFrame(fn) { const id = ++token; frames.set(id, fn); return id }, cancelAnimationFrame(id) { frames.delete(id) }
  })
  return { scroll, effects, elements, frames, bridge, doc, events, element,
    click(id) { captureClick({ target: { closest: () => elements.get(id) } }) },
    command(intent, originId, selectedId, overrides = {}, painted = false) { const previous = current, next = { ...current, token: current.token + 1, selectedId, ...overrides }; if (painted) root.dataset.observationContext = JSON.stringify(next); subscribed({ intent, originId, previous, destination: next }); current = next; root.dataset.observationContext = JSON.stringify(current) },
    stamp(change) { root.dataset.observationContext = JSON.stringify({ ...current, ...change }) },
    select(id) { current = { ...current, selectedId: id }; root.dataset.observationContext = JSON.stringify(current) },
    remount() { elements.delete('observation-panel'); root = { ...root, dataset: { ...root.dataset } }; elements.set('observation-panel', root) },
    retirePanel() { mutations([{ removedNodes: [root] }]) },
    observerStopped() { return observerStopped },
    toolbar() { toolbarSubscribed({ targetId: 'observation-for-files', originId: 'observation-close-file-composer', context: current, viewport: { workspace: 'workspace', epoch: 3, generation: 'query', revision: 1 } }) },
    flush() { const queued = [...frames.values()]; frames.clear(); queued.forEach(fn => fn()) }
  }
}

test('detail and return restore one exact repeated-card origin with preventScroll', () => {
  const h = harness()
  h.command('detail', 'duplicate', 'A'); h.flush()
  assert.equal(h.effects[0].id, 'observation-detail-heading')
  h.command('return', 'duplicate', null); h.flush()
  assert.equal(h.effects.at(-1).id, 'duplicate')
  assert.deepEqual(h.effects.at(-1).options, { preventScroll: true })
  assert.equal(h.scroll.scrollTop, 123)
  h.bridge.dispose()
})

test('virtual return waits one bounded current-layout paint and rechecks navigation before focus', () => {
  const h = harness()
  const viewport = { dataset: { observationViewportContext: JSON.stringify({ workspace: 'workspace', epoch: 3, generation: 'query', revision: 1 }), observationLayoutReady: 'false' } }
  h.elements.set('observation-viewport', viewport)
  h.command('detail', 'duplicate', 'A'); h.flush()
  const detailEffects = h.effects.length
  h.command('return', 'duplicate', null); h.flush()
  assert.equal(h.effects.length, detailEffects)
  viewport.dataset.observationLayoutReady = 'true'
  viewport.dataset.observationViewportContext = JSON.stringify({ workspace: 'workspace', epoch: 3, generation: 'query', revision: 2 })
  h.flush(); assert.equal(h.effects.at(-1).id, 'duplicate'); assert.equal(h.scroll.scrollTop, 123)
  h.command('detail', 'duplicate', 'A'); h.flush()
  h.command('return', 'duplicate', null); h.flush(); h.stamp({ sessionEpoch: 4 })
  const before = h.effects.length; h.flush(); assert.equal(h.effects.length, before)
  h.bridge.dispose()
})

test('native old-DOM capture survives port delivery after destination paint', () => {
  const h = harness()
  h.click('duplicate'); h.scroll.scrollTop = 777
  h.command('detail', 'duplicate', 'A', {}, true); h.flush()
  assert.equal(h.effects[0].id, 'observation-detail-heading')
  h.command('return', 'duplicate', null, {}, true); h.flush()
  assert.equal(h.effects.at(-1).id, 'duplicate'); assert.equal(h.scroll.scrollTop, 123)
  h.bridge.dispose()
})

test('destination paint without old activation proof focuses detail but returns to results', () => {
  const h = harness()
  h.command('detail', 'first', 'A', {}, true); h.flush()
  assert.equal(h.effects[0].id, 'observation-detail-heading')
  h.command('return', 'first', null, {}, true); h.flush()
  assert.equal(h.effects.at(-1).scroll, 'observation-results')
  h.bridge.dispose()
})

test('captured origin cannot survive a changed selection within the same context', () => {
  const h = harness()
  h.click('first'); h.command('detail', 'first', 'A', {}, true); h.flush()
  h.select('B'); h.command('return', 'first', null, {}, true); h.flush()
  assert.equal(h.effects.filter(value => value.id === 'first').length, 0)
  assert.equal(h.effects.at(-1).scroll, 'observation-results')
  h.bridge.dispose()
})

test('capture ignores an unavailable scroller', () => {
  const h = harness()
  h.elements.delete('main-content-scroll'); h.click('first')
  h.command('detail', 'first', 'A', {}, true); h.flush()
  assert.deepEqual(h.effects, [])
  h.bridge.dispose()
})

test('return reveals the exact card when a canonical mutation has moved its layout', () => {
  const h = harness()
  h.command('detail', 'first', 'A'); h.flush()
  h.elements.get('first').getBoundingClientRect = () => ({ top: -200, bottom: -100 })
  h.command('return', 'first', null); h.flush()
  assert.equal(h.effects.at(-2).id, 'first')
  assert.equal(h.effects.at(-1).scroll, 'first')
  h.bridge.dispose()
})

test('completed activation cannot revive its origin on a same-stamp remounted panel', () => {
  const h = harness()
  h.command('detail', 'first', 'A'); h.flush()
  h.remount()
  h.command('return', 'first', null); h.flush()
  assert.equal(h.effects.at(-1).scroll, 'observation-results')
  h.bridge.dispose()
})

test('deferred focus cannot cross a same-stamp panel replacement', () => {
  const h = harness()
  h.command('detail', 'first', 'A'); h.remount(); h.flush()
  assert.deepEqual(h.effects, [])
  h.bridge.dispose()
})

test('panel retirement clears the bounded origin and pending frame, and disposal disconnects its observer', () => {
  const h = harness()
  h.command('detail', 'first', 'A')
  assert.equal(h.frames.size, 1)
  h.retirePanel(); assert.equal(h.frames.size, 0)
  h.remount(); h.command('return', 'first', null); h.flush()
  assert.equal(h.effects.at(-1).scroll, 'observation-results')
  h.bridge.dispose(); assert.equal(h.observerStopped(), true)
})

test('a newer activation cancels pending focus and owns the return origin', () => {
  const h = harness()
  h.command('detail', 'first', 'A')
  h.scroll.scrollTop = 456
  h.command('detail', 'duplicate', 'A')
  assert.equal(h.frames.size, 1); h.flush()
  assert.equal(h.effects.filter(value => value.id === 'observation-detail-heading').length, 1)
  h.command('return', 'duplicate', null); h.flush()
  assert.equal(h.effects.at(-1).id, 'duplicate'); assert.equal(h.scroll.scrollTop, 456)
  h.bridge.dispose()
})

for (const retirement of ['workspace', 'session', 'selection', 'token', 'panel']) test('deferred focus is inert after ' + retirement + ' retirement', () => {
  const h = harness()
  h.command('detail', 'first', 'A')
  if (retirement === 'panel') h.elements.delete('observation-panel')
  else h.stamp(retirement === 'workspace' ? { workspaceId: 'other' } : retirement === 'session' ? { sessionEpoch: 4 } : retirement === 'selection' ? { selectedId: 'B' } : { token: 99 })
  h.flush(); assert.deepEqual(h.effects, [])
  h.bridge.dispose()
})

for (const fallback of ['direct', 'removed', 'hidden', 'outside']) test('return uses results fallback for ' + fallback + ' origin', () => {
  const h = harness()
  h.command('detail', fallback === 'direct' ? null : 'first', 'A'); h.flush()
  if (fallback === 'removed') h.elements.delete('first')
  if (fallback === 'hidden') h.elements.get('first').hidden = true
  if (fallback === 'outside') h.elements.get('first').inPanel = false
  h.command('return', fallback === 'direct' ? null : 'first', null); h.flush()
  assert.equal(h.effects.at(-1).scroll, 'observation-results')
  h.bridge.dispose()
})

test('old DOM mismatch prevents origin capture or deferred focus and dispose retires pending work', () => {
  const h = harness()
  h.stamp({ token: 42 })
  h.command('detail', 'first', 'A'); h.flush(); assert.deepEqual(h.effects, [])
  h.command('detail', 'first', 'B'); assert.equal(h.frames.size, 1)
  h.bridge.dispose(); assert.equal(h.frames.size, 0)
})

function toolbarHarness() {
  const h = harness({ toolbarEnabled: true })
  for (const id of ['observation-close-file-composer', 'observation-for-files', 'observation-advanced-toggle']) h.elements.set(id, h.element(id))
  h.elements.set('observation-viewport', { dataset: { observationViewportContext: JSON.stringify({ workspace: 'workspace', epoch: 3, generation: 'query', revision: 1 }) } })
  h.doc.activeElement = h.elements.get('observation-close-file-composer')
  h.click('observation-close-file-composer'); h.doc.activeElement = h.doc.body
  return h
}

test('Close composer returns to For files after its owned paint', () => {
  const h = toolbarHarness(); h.toolbar(); h.flush()
  assert.deepEqual(h.effects, [])
  h.flush(); assert.equal(h.doc.activeElement.id, 'observation-for-files')
  h.bridge.dispose()
})

for (const phase of ['before-port', 'after-port']) test('new Advanced keyboard intent cancels Close transfer ' + phase, () => {
  const h = toolbarHarness(), advanced = h.elements.get('observation-advanced-toggle')
  if (phase === 'after-port') h.toolbar()
  h.doc.activeElement = advanced; h.events.get('focusin')({ target: advanced }); h.events.get('keydown')({ target: advanced, key: ' ' })
  if (phase === 'before-port') h.toolbar()
  h.flush(); h.flush()
  assert.equal(h.doc.activeElement, advanced); assert.deepEqual(h.effects, [])
  h.bridge.dispose()
})

for (const cause of ['query', 'session', 'navigation', 'target', 'unmount']) test('toolbar transfer retires on ' + cause, () => {
  const h = toolbarHarness(); h.toolbar()
  if (cause === 'query') h.elements.get('observation-viewport').dataset.observationViewportContext = JSON.stringify({ workspace: 'workspace', epoch: 3, generation: 'other', revision: 2 })
  if (cause === 'session') h.stamp({ sessionEpoch: 4 })
  if (cause === 'navigation') h.stamp({ token: 1 })
  if (cause === 'target') h.elements.delete('observation-for-files')
  if (cause === 'unmount') h.retirePanel()
  h.flush(); h.flush(); assert.deepEqual(h.effects, [])
  h.bridge.dispose()
})

test('unmount retires the Close activation proof before its deferred port arrives', () => {
  const h = toolbarHarness()
  h.retirePanel(); h.remount(); h.toolbar(); h.flush(); h.flush()
  assert.deepEqual(h.effects, [])
  h.bridge.dispose()
})
