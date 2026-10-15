import test from 'node:test'
import assert from 'node:assert/strict'
import { createTimestampFormatter, installTimestampDisplay } from '../src/timestamp-display.js'

const cases = [
  ['UTC', '2026-01-01T00:00:00Z', '2026-01-01 00:00'],
  ['Europe/Copenhagen', '2026-01-15T23:45:00Z', '2026-01-16 00:45'],
  ['Europe/Copenhagen', '2026-07-15T23:45:00Z', '2026-07-16 01:45'],
  ['Europe/Copenhagen', '2026-03-29T00:59:59.999999Z', '2026-03-29 01:59'],
  ['Europe/Copenhagen', '2026-03-29T01:00:00Z', '2026-03-29 03:00'],
  ['Europe/Copenhagen', '2026-10-25T00:59:59Z', '2026-10-25 02:59'],
  ['Europe/Copenhagen', '2026-10-25T01:00:00Z', '2026-10-25 02:00'],
  ['America/Los_Angeles', '2026-01-01T00:15:00Z', '2025-12-31 16:15'],
  ['Asia/Kathmandu', '2026-01-31T23:45:00.123456789Z', '2026-02-01 05:30'],
  ['Asia/Kathmandu', '2026-01-01T01:15:00.123456+01:30', '2026-01-01 05:30'],
  ['UTC', '2026-01-01T00:15:00-05:45', '2026-01-01 06:00'],
  ['UTC', '0099-01-01T00:00:00Z', '0099-01-01 00:00'],
  ['UTC', '2000-02-29t00:00:00z', '2000-02-29 00:00']
]
for (const [zone, source, expected] of cases) {
  test(`${zone} formats ${source} per instant`, () => {
    assert.deepEqual(createTimestampFormatter(zone)(source), { text: expected, day: expected.slice(0, 10) })
  })
}

test('rejects malformed or ambiguous timestamps before Date can normalize them', () => {
  const format = createTimestampFormatter('UTC')
  for (const source of [null, undefined, '', ' ', 'yesterday', '2026-01-01', '2026-01-01T00:00:00', '2026-02-29T00:00:00Z', '1900-02-29T00:00:00Z', '2026-04-31T00:00:00Z', '2026-00-01T00:00:00Z', '2026-13-01T00:00:00Z', '2026-01-00T00:00:00Z', '2026-01-01T24:00:00Z', '2026-01-01T00:60:00Z', '2026-01-01T00:00:60Z', '2026-01-01T00:00:00+24:00', '2026-01-01T00:00:00+01:60', '0000-01-01T00:00:00Z']) {
    assert.equal(format(source), null, String(source))
  }
})

test('does not mislabel instants outside the four-digit Gregorian display range', () => {
  const format = createTimestampFormatter('UTC')
  assert.equal(format('0001-01-01T00:00:00+01:00'), null)
  assert.equal(format('9999-12-31T23:59:59-01:00'), null)
})

test('captures the browser default timezone when the formatter is created', () => {
  const previous = process.env.TZ
  try {
    process.env.TZ = 'Asia/Kathmandu'
    const initial = createTimestampFormatter()
    process.env.TZ = 'America/Los_Angeles'
    assert.equal(initial('2026-01-01T00:00:00Z').text, '2026-01-01 05:45')
    assert.equal(createTimestampFormatter()('2026-01-01T00:00:00Z').text, '2025-12-31 16:00')
  } finally {
    if (previous === undefined) delete process.env.TZ
    else process.env.TZ = previous
  }
})

function harness(options = {}) {
  const sent = [], scheduled = []
  let callback, unsubscribed
  const app = { ports: {
    formatTimestamps: { subscribe(fn) { callback = fn }, unsubscribe(fn) { unsubscribed = fn } },
    timestampsFormatted: { send(value) { sent.push(value) } }
  } }
  const bridge = installTimestampDisplay(app, { format: createTimestampFormatter('Asia/Kathmandu'), schedule(fn) { scheduled.push(fn) }, ...options })
  return { sent, bridge, send(value) { callback(value) }, flush() { scheduled.splice(0).forEach(fn => fn()) }, get unsubscribed() { return unsubscribed === callback } }
}

test('bridge batches, deduplicates exact raw strings, stamps replies and handles new requests without retaining a cache', () => {
  const h = harness(), source = '2026-01-01T00:00:00.000001Z'
  const request = { context: 'workspace/session', id: 42, sources: [source, source, 'bad'] }
  h.send(request)
  assert.deepEqual(h.sent, [])
  h.flush()
  assert.deepEqual(h.sent, [{ context: request.context, id: 42, values: [
    { source, local: { text: '2026-01-01 05:45', day: '2026-01-01' } }, { source: 'bad', local: null }
  ] }])
  assert.deepEqual(request.sources, [source, source, 'bad'])
  h.send({ context: 'next/session', id: 43, sources: ['2026-02-01T00:00:00Z'] })
  h.flush()
  assert.equal(h.sent[1].values[0].local.text, '2026-02-01 05:45')
})

test('bridge enforces the batch bound and ignores malformed envelopes', () => {
  const h = harness()
  for (const request of [null, {}, { context: 'x', id: 1, sources: Array(129).fill('x') }, { context: 'x', id: 1, sources: [5] }]) h.send(request)
  h.flush()
  assert.equal(h.sent.length, 0)
  h.send({ context: 'x', id: 2, sources: Array.from({ length: 128 }, (_, i) => String(i)) })
  h.flush()
  assert.equal(h.sent[0].values.length, 128)
})

test('disposing bridge retires queued responses and unsubscribes', () => {
  const h = harness()
  h.send({ context: 'x', id: 1, sources: ['2026-01-01T00:00:00Z'] })
  h.bridge.dispose()
  h.flush()
  assert.deepEqual(h.sent, [])
  assert.equal(h.unsubscribed, true)
})
