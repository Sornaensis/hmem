import assert from 'node:assert/strict'
import test from 'node:test'
import { installAuditLayout } from '../src/audit-layout.js'

function fixture(resize = true) {
  const style = () => ({ values: new Map(), getPropertyValue(key) { return this.values.get(key) || '' }, setProperty(key, value) { this.values.set(key, value) }, removeProperty(key) { this.values.delete(key) } })
  let current = null, next = 0
  const frames = new Map(), listeners = new Map(), observers = [], mutations = []
  const scroll = { clientHeight: 700, querySelector() { return current } }
  const win = { requestAnimationFrame(callback) { const id = ++next; frames.set(id, callback); return id }, cancelAnimationFrame(id) { frames.delete(id) }, addEventListener(key, callback) { listeners.set(key, callback) }, removeEventListener(key) { listeners.delete(key) }, MutationObserver: class { constructor(callback) { this.callback = callback; this.connected = true; mutations.push(this) } observe() {} disconnect() { this.connected = false } } }
  if (resize) win.ResizeObserver = class { constructor(callback) { this.callback = callback; this.targets = new Set(); observers.push(this) } observe(target) { this.targets.add(target) } disconnect() { this.targets.clear() } }
  const doc = { getElementById() { return scroll }, fonts: { addEventListener(key, callback) { listeners.set('fonts-' + key, callback) }, removeEventListener(key) { listeners.delete('fonts-' + key) } } }
  return { doc, win, scroll, frames, listeners, observers, mutations,
    mount(height) { const filters = { offsetHeight: height }; current = { isConnected: true, style: style(), querySelector() { return filters } }; return { root: current, filters } },
    leave() { current.isConnected = false; current = null; mutations[0].callback() },
    flush() { const batch = [...frames.values()]; frames.clear(); batch.forEach(callback => callback()) },
  }
}

test('mounted audit layout measures CSS units, coalesces resize work, and releases its prior route owner', () => {
  const h = fixture(), first = h.mount(71), dispose = installAuditLayout({ document: h.doc, window: h.win })
  h.flush()
  assert.equal(first.root.style.getPropertyValue('--audit-filter-height'), '71px')
  assert.equal(first.root.style.getPropertyValue('--audit-scroll-height'), '700px')
  first.filters.offsetHeight = 143; h.scroll.clientHeight = 530
  h.observers[0].callback(); h.observers[0].callback(); h.mutations[0].callback()
  assert.equal(h.frames.size, 1)
  h.flush(); assert.equal(first.root.style.getPropertyValue('--audit-filter-height'), '143px')
  assert.equal(first.root.style.getPropertyValue('--audit-scroll-height'), '530px')
  h.leave(); h.flush(); assert.equal(h.observers[0].targets.size, 0); assert.equal(first.root.style.values.size, 0)
  const second = h.mount(62); h.mutations[0].callback(); h.flush()
  assert.equal(h.observers.length, 2); assert.equal(h.observers[1].targets.size, 2)
  h.observers[1].callback(); dispose(); dispose()
  assert.equal(h.frames.size, 0); assert.equal(h.listeners.size, 0); assert.equal(h.observers[1].targets.size, 0)
  assert.equal(h.mutations[0].connected, false); assert.equal(second.root.style.values.size, 0)
})

test('resize observer fallback still remeasures wrapped controls and cleans its listeners', () => {
  const h = fixture(false), view = h.mount(82), dispose = installAuditLayout({ document: h.doc, window: h.win })
  h.flush(); view.filters.offsetHeight = 196; h.listeners.get('resize')(); h.flush()
  assert.equal(view.root.style.getPropertyValue('--audit-filter-height'), '196px')
  dispose(); assert.equal(h.listeners.size, 0); assert.equal(h.frames.size, 0)
})
