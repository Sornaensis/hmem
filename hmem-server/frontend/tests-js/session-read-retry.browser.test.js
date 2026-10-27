import assert from 'node:assert/strict'
import test from 'node:test'
import { openSessionExplorer } from './support/sessions-ux-fixture.mjs'

const delay = ms => new Promise(resolve => setTimeout(resolve, ms))
const until = async predicate => {
  const deadline = Date.now() + 10000
  while (Date.now() < deadline) { if (await predicate()) return; await delay(20) }
  throw new Error('Selected read did not converge')
}
const select = async h => {
  await h.start(); await h.idle()
  await h.page.getByRole('button', { name: 'Sessions', exact: true }).click()
  await h.page.locator('.session-summary a').first().click()
}
const readPath = (h, kind) => '/api/v1/workspaces/' + h.fixture.workspace.id + '/sessions/' + h.active.id + (kind === 'range' ? '/transcript' : '')

for (const kind of ['metadata', 'range']) {
  test('selected ' + kind + ' recovers once after callback and 500ms with the exact target', { timeout: 45000 }, async () => {
    const h = await openSessionExplorer(), arrivals = []
    let release, failedAt
    const held = new Promise(resolve => { release = resolve })
    try {
      await h.page.route('**/api/v1/**', async route => {
        const url = new URL(route.request().url())
        if (url.pathname !== readPath(h, kind)) return route.fallback()
        arrivals.push({ at: Date.now(), query: url.search })
        if (arrivals.length === 1) {
          await held; failedAt = Date.now()
          await route.fulfill({ status: 503, json: { error: 'temporary' } })
        } else await route.fallback()
      })
      await select(h)
      await until(() => arrivals.length === 1)
      await delay(650)
      assert.equal(arrivals.length, 1, 'an outstanding original request never retries')
      release()
      await until(() => arrivals.length === 2)
      assert.ok(arrivals[1].at - failedAt >= 500, 'backoff begins only after the failed response')
      assert.equal(arrivals[1].query, arrivals[0].query)
      await h.page.getByRole('heading', { name: 'A calmer session explorer' }).waitFor()
      await delay(650)
      assert.equal(arrivals.length, 2)
      assert.equal(await h.page.getByRole('heading', { name: 'A calmer session explorer' }).count(), 1, 'retry publishes no duplicate transcript chunk')
      assert.ok(h.peak() <= 2); assert.deepEqual(h.errors, [])
    } finally { release(); await h.close() }
  })
}

test('progress cannot bypass backoff or exhaustion; manual Retry restores a bounded attempt', { timeout: 45000 }, async () => {
  const h = await openSessionExplorer(), arrivals = []
  let failures = 2
  try {
    await h.page.route('**/api/v1/**', async route => {
      const url = new URL(route.request().url())
      if (url.pathname !== readPath(h, 'range')) return route.fallback()
      arrivals.push({ at: Date.now(), query: url.search })
      if (failures-- > 0) await route.fulfill({ status: 500, json: { error: 'temporary' } })
      else await route.fallback()
    })
    await select(h)
    await until(() => arrivals.length === 1)
    for (let i = 0; i < 4; i++) { await h.appendRecords({ text: 'Progress while waiting ' + i }); await delay(60) }
    assert.equal(arrivals.length, 1)
    await until(() => arrivals.length === 2)
    for (let i = 0; i < 6; i++) { await h.appendRecords({ text: 'Progress after exhaustion ' + i }); await delay(100) }
    assert.equal(arrivals.length, 2, 'two failed requests cannot become an endless chain')
    const alert = h.page.locator('.session-conversation [role=alert]')
    await alert.getByRole('button', { name: 'Retry', exact: true }).click()
    await until(() => arrivals.length >= 3)
    assert.equal(arrivals[2].query, arrivals[0].query, 'manual retry preserves the failed finite range')
    await h.page.getByRole('heading', { name: 'A calmer session explorer' }).waitFor()
    assert.ok(h.peak() <= 2); assert.deepEqual(h.errors, [])
  } finally { await h.close() }
})

test('manual Retry during backoff retires the delayed ticket', { timeout: 45000 }, async () => {
  const h = await openSessionExplorer(), arrivals = []
  try {
    await h.page.route('**/api/v1/**', async route => {
      const url = new URL(route.request().url())
      if (url.pathname !== readPath(h, 'metadata')) return route.fallback()
      arrivals.push(Date.now())
      if (arrivals.length === 1) await route.fulfill({ status: 502, json: {} })
      else await route.fallback()
    })
    await select(h)
    await h.page.locator('.session-detail > [role=alert]').getByRole('button', { name: 'Retry', exact: true }).click()
    await h.page.getByRole('heading', { name: 'A calmer session explorer' }).waitFor()
    await delay(750)
    assert.equal(arrivals.length, 2, 'old timer cannot admit another request after manual recovery')
    assert.ok(h.peak() <= 2); assert.deepEqual(h.errors, [])
  } finally { await h.close() }
})

for (const failure of ['permanent', 'decoding', 'invalid-response']) {
  test('selected metadata ' + failure + ' failure has no automatic retry', { timeout: 45000 }, async () => {
    const h = await openSessionExplorer()
    let count = 0
    try {
      await h.page.route('**/api/v1/**', async route => {
        if (new URL(route.request().url()).pathname !== readPath(h, 'metadata')) return route.fallback()
        count++
        if (failure === 'permanent') await route.fulfill({ status: 404, json: {} })
        else if (failure === 'decoding') await route.fulfill({ status: 200, contentType: 'application/json', body: '{invalid' })
        else await route.fulfill({ json: { ...h.active, workspace_id: 'foreign' } })
      })
      await select(h)
      await h.page.locator('.session-detail > [role=alert]').waitFor()
      await h.appendRecords({ text: 'Progress cannot clear a permanent failure' })
      await delay(800)
      assert.equal(count, 1)
      assert.deepEqual(h.errors, [])
    } finally { await h.close() }
  })
}


test('A to B to A retires the old backoff ticket even when A is selected again', { timeout: 45000 }, async () => {
  const h = await openSessionExplorer({ activeCount: 2 })
  let aReads = 0
  try {
    await h.page.route('**/api/v1/**', async route => {
      if (new URL(route.request().url()).pathname !== readPath(h, 'metadata')) return route.fallback()
      aReads++
      if (aReads === 1) await route.fulfill({ status: 504, json: {} })
      else await route.fallback()
    })
    await select(h)
    await h.page.locator('.session-detail > [role=alert]').waitFor()
    await h.page.evaluate(id => { location.hash = 'tab=sessions&session=' + id }, h.activeSessions[1].id)
    await until(() => h.reads.some(read => read.path.endsWith('/sessions/' + h.activeSessions[1].id)))
    await h.page.evaluate(id => { location.hash = 'tab=sessions&session=' + id }, h.active.id)
    await h.page.getByRole('heading', { name: 'A calmer session explorer' }).waitFor()
    await delay(750)
    assert.equal(aReads, 2, 'a prior A lifetime cannot spend a retry in the new lifetime')
    assert.ok(h.peak() <= 2); assert.deepEqual(h.errors, [])
  } finally { await h.close() }
})

test('slow retired callbacks keep both physical admissions until exact completion', { timeout: 45000 }, async () => {
  const h = await openSessionExplorer({ activeCount: 2 })
  const releases = [], heldPaths = [], bPaths = []
  let active = 0, peak = 0
  try {
    h.active.title = 'Selected A'; h.activeSessions[1].title = 'Selected B'
    await h.page.route('**/api/v1/**', async route => {
      const path = new URL(route.request().url()).pathname
      if (!path.includes('/sessions')) return route.fallback()
      active++; peak = Math.max(peak, active)
      try {
        if ((path === readPath(h, 'metadata') || path === readPath(h, 'metadata') + '/tasks') && heldPaths.length < 2) {
          heldPaths.push(path)
          await new Promise(resolve => releases.push(resolve))
        }
        if (path.includes(h.activeSessions[1].id)) bPaths.push(path)
        await route.fallback()
      } finally { active-- }
    })
    await select(h)
    await until(() => heldPaths.length === 2)
    await h.page.evaluate(id => { location.hash = 'tab=sessions&session=' + id }, h.activeSessions[1].id)
    await delay(150)
    assert.equal(bPaths.length, 0, 'logical retirement cannot free either native request')
    releases.shift()()
    await h.page.getByRole('heading', { name: 'Selected B', exact: true }).waitFor()
    assert.equal(await h.page.locator('.session-detail-title').filter({ hasText: 'Selected A' }).count(), 0)
    releases.shift()()
    await h.page.getByRole('heading', { name: 'A calmer session explorer' }).waitFor()
    assert.ok(peak <= 2, 'all custom and fallback session HTTP requests share two physical slots')
    assert.deepEqual(h.errors, [])
  } finally { for (const release of releases) release(); await h.close() }
})
