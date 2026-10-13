import test from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import { observationMembershipReady, observationMembershipSnapshot, waitObservationMembers } from './harness.mjs'

function fixtureDocument(options = {}) {
  const { viewport = true, mounted = 12, loading = false, error = null, layout = 'true', context = 'context' } = options
  const loaded = Object.hasOwn(options, 'loaded') ? options.loaded : '50'
  return {
    getElementById: id => id === 'observation-viewport' && viewport ? { dataset: { observationLoadedCount: loaded, observationLayoutReady: layout, observationViewportContext: context } } : null,
    querySelectorAll: selector => { assert.equal(selector, '.observation-card'); return { length: mounted } },
    querySelector: selector => selector === '.observation-state-loading' ? (loading ? {} : null) : selector === '.observation-state-error' && error ? { textContent: error } : null
  }
}

async function withDocument(value, action) {
  const prior = globalThis.document
  globalThis.document = value
  try { return await action() } finally { if (prior === undefined) delete globalThis.document; else globalThis.document = prior }
}

test('present virtual viewport membership uses exact logical counts independently of mounted rows', async () => {
  for (const [expected, mounted] of [[0, 0], [1, 0], [50, 12], [60, 14], [100, 16]]) {
    await withDocument(fixtureDocument({ loaded: String(expected), mounted }), () => {
      assert.equal(observationMembershipReady({ expected }), true)
      assert.equal(observationMembershipReady({ expected: expected + 1 }), false)
    })
  }
  await withDocument(fixtureDocument({ loaded: '101', mounted: 100 }), () => assert.equal(observationMembershipReady({ expected: 100 }), false, 'expected membership remains exact'))
})

test('malformed or missing present logical counts fail closed without physical fallback', async () => {
  for (const loaded of [undefined, null, '', ' 50', '50 ', '050', '-1', '5e1', '50.0', 'NaN', '9007199254740992']) {
    await withDocument(fixtureDocument({ loaded, mounted: 50 }), () => assert.equal(observationMembershipReady({ expected: 50 }), false, String(loaded)))
  }
})

test('legacy absence uses physical equality and filtered membership preserves the existing loading guard', async () => {
  await withDocument(fixtureDocument({ viewport: false, mounted: 50 }), () => assert.equal(observationMembershipReady({ expected: 50 }), true))
  await withDocument(fixtureDocument({ viewport: false, mounted: 12 }), () => assert.equal(observationMembershipReady({ expected: 50 }), false))
  for (const viewport of [true, false]) {
    await withDocument(fixtureDocument({ viewport, loaded: '1', mounted: 1, loading: true }), () => {
      assert.equal(observationMembershipReady({ expected: 1, requireIdle: true }), false)
      assert.equal(observationMembershipReady({ expected: 1 }), true)
    })
    await withDocument(fixtureDocument({ viewport, loaded: '1', mounted: 1, loading: false }), () => assert.equal(observationMembershipReady({ expected: 1, requireIdle: true }), true))
  }
})

test('production wait uses the tested capability predicate with unchanged deadline and no success diagnostics', async () => {
  let evaluations = 0
  await withDocument(fixtureDocument({ loaded: '50', mounted: 12 }), async () => {
    await waitObservationMembers({
      waitForFunction: async (predicate, input, options) => {
        assert.equal(predicate, observationMembershipReady)
        assert.deepEqual(input, { expected: 50, requireIdle: false })
        assert.deepEqual(options, { timeout: 30000 })
        assert.equal(predicate(input), true)
      },
      evaluate: async () => { evaluations += 1 }
    }, 50, 'Observations initial page')
  })
  assert.equal(evaluations, 0)
})

test('timeout snapshots retain actual logical, mounted, loading, error and context with bounded fields', async () => {
  const original = Object.assign(new Error('Timeout 30000ms exceeded'), { name: 'TimeoutError' })
  await withDocument(fixtureDocument({ loaded: '49', mounted: 12, loading: true, error: 'error '.repeat(100), context: 'x'.repeat(1000) }), async () => {
    const snapshot = observationMembershipSnapshot({ expected: 50, label: 'initial', phase: 'small warmup 1: initial', requireIdle: false })
    assert.equal(snapshot.rawLogicalCount, '49')
    assert.equal(snapshot.logicalCount, 49)
    assert.equal(snapshot.mountedCards, 12)
    assert.equal(snapshot.loading, true)
    assert.equal(snapshot.error, true)
    assert.equal(snapshot.errorText.length, 256)
    assert.equal(snapshot.context.length, 512)
    assert.equal(snapshot.layoutReady, 'true')
    await assert.rejects(waitObservationMembers({
      waitForFunction: async () => { throw original },
      evaluate: async (capture, input) => { assert.equal(capture, observationMembershipSnapshot); return capture(input) }
    }, 50, 'initial'), error => {
      assert.equal(error.cause, original)
      assert.equal(error.observationMembership.expected, 50)
      assert.equal(error.observationMembership.logicalCount, 49)
      assert.equal(error.observationMembership.mountedCards, 12)
      assert.match(error.message, /Timeout 30000ms exceeded/)
      return true
    })
  })
})

test('timeout diagnostic capture failures report unknown values and other failures do not add reads', async () => {
  const timeout = Object.assign(new Error('Timeout 30000ms exceeded'), { name: 'TimeoutError' })
  await assert.rejects(waitObservationMembers({ waitForFunction: async () => { throw timeout }, evaluate: async () => { throw new Error('Browser unavailable') } }, 50, 'initial'), error => {
    assert.equal(error.cause, timeout)
    assert.equal(error.observationMembership.captureError, 'Browser unavailable')
    assert.equal(Object.hasOwn(error.observationMembership, 'logicalCount'), false)
    return true
  })
  const failure = new Error('Supervisor failure')
  let captures = 0
  await assert.rejects(waitObservationMembers({ waitForFunction: async () => { throw failure }, evaluate: async () => { captures += 1 } }, 50), error => error === failure)
  assert.equal(captures, 0)
})

test('ordinary initial, load-more, filter and reset use one logical helper while physical budget selectors remain intact', () => {
  const source = fs.readFileSync('perf/harness.mjs', 'utf8')
  assert.match(source, /waitObservationMembers\(page, Math\.min\(observationPageSize, fixture\.observations\.length\), 'Observations initial page'\)/)
  assert.match(source, /waitObservationMembers\(page, 2 \* observationPageSize, 'Observation load more'\)/)
  assert.match(source, /waitObservationMembers\(page, 1, 'Observation filter', true\)/)
  assert.match(source, /waitObservationMembers\(page, Math\.min\(observationPageSize, fixture\.observations\.length\), 'Observation filter reset'\)/)
  const metrics = source.split('async function domMetrics(page)')[1].split('async function')[0]
  assert.match(metrics, /observations: document\.querySelectorAll\('\.observation-card'\)\.length/)
  assert.match(source, /m\.rows=Math\.max\(m\.rows,document\.querySelectorAll\('\.card-project,\.card-task,\.card-subtask,\.observation-card,\.timeline-event-card,\.timeline-value-table tbody tr,\.timeline-point-control'\)\.length\)/)
  assert.match(source, /observationMembership: error\.observationMembership \|\| null/)
})

test('a stalled timeout diagnostic capture is finite and retains the original failure', async () => {
  const timeout = Object.assign(new Error('Timeout 30000ms exceeded'), { name: 'TimeoutError' })
  await assert.rejects(waitObservationMembers({ waitForFunction: async () => { throw timeout }, evaluate: () => new Promise(() => {}) }, 50, 'initial'), error => {
    assert.equal(error.cause, timeout)
    assert.match(error.observationMembership.captureError, /capture exceeded 1000ms/)
    assert.equal(Object.hasOwn(error.observationMembership, 'logicalCount'), false)
    return true
  })
})
