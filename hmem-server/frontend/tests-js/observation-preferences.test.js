import assert from 'node:assert/strict'
import test from 'node:test'
import { installObservationPreferences } from '../src/observation-preferences.js'

const owner = { workspaceId: '00000000-0000-4000-8000-000000000001', actorId: 'actor', authority: 'local', runtimeId: 'runtime', epoch: 3, requestId: 1, touch: 0 }
const preference = { version: 1, workspaceId: owner.workspaceId, actorId: owner.actorId, authority: owner.authority, detail: null, subjects: [], groups: [] }
function harness(storage) {
  let command; const frames = [], received = []
  installObservationPreferences({ ports: { observationPreferenceCommand: { subscribe(fn) { command = fn } }, observationPreferencesReceived: { send(value) { received.push(value) } } } }, { storage, schedule: fn => frames.push(fn) })
  return { send: value => command(value), received, paint() { frames.splice(0).forEach(fn => fn()) } }
}
test('read is scoped to stable actor/workspace namespace and returns complete owner; replacement retires queued hydration', () => {
  const reads = []; const h = harness({ getItem(key) { reads.push(key); return JSON.stringify(preference) } })
  h.send({ operation: 'read', owner }); h.send({ operation: 'read', owner: { ...owner, actorId: 'other', requestId: 2 } }); h.paint()
  assert.equal(h.received.length, 1); assert.equal(h.received[0].owner.actorId, 'other')
  assert.ok(reads[0].includes(':actor:local')); assert.ok(!reads[0].includes('runtime'))
  h.send({ operation: 'read', owner }); h.send({ operation: 'retire' }); h.paint(); assert.equal(h.received.length, 1)
})
test('write requires acknowledged current owner and rejects scope drift without writing other actor storage', () => {
  const writes = []; const h = harness({ getItem() { return null }, setItem(key, value) { writes.push({ key, value }) } })
  h.send({ operation: 'write', owner, value: preference }); assert.equal(writes.length, 0)
  h.send({ operation: 'read', owner }); h.paint()
  h.send({ operation: 'write', owner: { ...owner, epoch: 99 }, value: preference })
  h.send({ operation: 'write', owner, value: { ...preference, actorId: 'foreign' } }); assert.equal(writes.length, 0)
  h.send({ operation: 'write', owner, value: preference }); assert.equal(writes.length, 1)
})
test('UTF8 byte bound deterministically evicts ordered groups before subjects and oversized reads are unavailable preferences', () => {
  const writes = []; const h = harness({ getItem() { return ' '.repeat(32769) }, setItem(key, value) { writes.push(value) } })
  h.send({ operation: 'read', owner }); h.paint(); assert.equal(h.received[0].value, null)
  const value = { ...preference, groups: Array.from({ length: 128 }, (_, i) => String(i).padStart(3, '0') + ':' + 'é'.repeat(1000)), subjects: ['00000000-0000-4000-8000-000000000002'] }
  h.send({ operation: 'write', owner, value }); h.send({ operation: 'write', owner, value })
  assert.equal(writes[0], writes[1]); assert.ok(Buffer.byteLength(writes[0]) <= 32768)
  const stored = JSON.parse(writes[0]); assert.ok(stored.groups.length < 128); assert.deepEqual(stored.subjects, value.subjects)
})
test('disabled/malformed storage does not break UI or leak untagged values', () => {
  const h = harness({ getItem() { throw new Error('denied') }, setItem() { throw new Error('quota') } })
  h.send({ operation: 'read', owner }); h.paint(); assert.equal(h.received[0].available, false)
  assert.doesNotThrow(() => h.send({ operation: 'write', owner, value: preference }))
  h.send({ operation: 'read', owner: { ...owner, workspaceId: 'invalid' } }); h.paint(); assert.equal(h.received.length, 1)
})
