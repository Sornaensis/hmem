import assert from 'node:assert/strict'
import test from 'node:test'
import { mkdir } from 'node:fs/promises'
import { join } from 'node:path'
import { createHash } from 'node:crypto'
import { openDiscovery } from './observation-discovery-fixture.mjs'
import { paint } from './observation-viewport-fixture.mjs'
import { hierarchyFixture } from './hierarchy-fixture.mjs'

const originalSha = '0123456789abcdef0123456789abcdef01234567'
const reviewedSha = 'b'.repeat(40)
const version = '20000000-0000-4000-8000-000000000000'
const provenance = sequence => ({ sequence, event_kind: sequence === 1 ? 'legacy_creation' : 'update', reviewed_git_sha: sequence === 1 ? originalSha : sequence % 2 ? reviewedSha : originalSha,
  content_version: sequence === 1 ? null : version, content_digest: sequence === 1 ? null : 'a'.repeat(64),
  recorded_at: `2021-03-${String(sequence % 25 + 1).padStart(2, '0')}T${String(sequence % 24).padStart(2, '0')}:17:49+02:00`, actor_type: sequence === 1 ? null : 'user', actor_id: sequence === 1 ? null : 'reviewer', actor_label: sequence === 1 ? null : 'Technical actor label' })
const transform = values => values.slice(0, 2).map((value, index) => ({ ...value, latest_sequence: index ? 53 : 1, content_version: index ? version : value.content_version,
  created_at: '2020-02-03T04:05:59+02:00', current_provenance: index ? { ...provenance(53), content_digest: createHash('sha256').update(value.content).digest('hex') } : null }))
const rows = h => h.page.locator('.observation-history-entry')
const card = (h, id = 'cache-main') => h.page.locator('.observation-result[data-observation-id=' + JSON.stringify(id) + ']').first()
const stamp = value => value.replace('T', ' ').slice(0, 16)
async function screenshot(h, name) {
  if (!process.env.HMEM_HISTORY_SCREENSHOT_DIR) return
  await mkdir(process.env.HMEM_HISTORY_SCREENSHOT_DIR, { recursive: true })
  await h.page.locator('#main-content-scroll').evaluate(element => { element.scrollTop = 0 })
  await paint(h.page)
  await h.page.screenshot({ path: join(process.env.HMEM_HISTORY_SCREENSHOT_DIR, name) })
}
async function historyRoutes(h, respond) {
  const receipts = [], releases = new Set()
  await h.page.route('**/api/v1/observations/*/history?**', async route => {
    const url = new URL(route.request().url()), id = url.pathname.split('/').at(-2), offset = Number(url.searchParams.get('offset'))
    const head = h.fixture.observations.find(value => value.id === id).latest_sequence
    const receipt = { id, offset, head, done: false }; receipts.push(receipt)
    assert.equal(url.searchParams.get('limit'), '25')
    const observation = h.fixture.observations.find(value => value.id === id)
    const body = { items: Array.from({ length: Math.min(25, head - offset) }, (_, i) => { const seq = head - offset - i; return { observation_id: id, ...provenance(seq), ...(seq === head && observation.current_provenance ? observation.current_provenance : {}), ...(seq === 1 ? { recorded_at: observation.created_at } : {}) } }), has_more: offset + 25 < head }
    try { await respond({ route, receipt, body, hold: async () => { await new Promise(resolve => { receipt.release = resolve; releases.add(resolve) }); releases.delete(receipt.release) } }) }
    finally { receipt.done = true }
  })
  return { receipts, releaseAll() { for (const release of releases) release() }, async waitFor(predicate) { await h.bounded((async () => { while (!receipts.some(predicate)) await h.page.waitForTimeout(10) })(), 5000, 'History request arrival') } }
}

test('production compact history auto-pages once, retains repeated SHAs and sequence order, and collapse/retry preserve accurate revisions', { timeout: 60000 }, async () => {
  const h = await openDiscovery(undefined, transform)
  let history
  try {
    await h.page.addInitScript(() => { window.historyCopies = []; Object.defineProperty(navigator.clipboard, 'writeText', { value: value => { window.historyCopies.push(value); return Promise.resolve() } }) })
    let failLast = true
    history = await historyRoutes(h, async ({ route, receipt, body, hold }) => {
      if (receipt.offset === 25) await hold()
      if (receipt.offset === 50 && failLast) { failLast = false; await route.fulfill({ status: 500, body: 'controlled failed page' }) }
      else await route.fulfill({ status: 200, contentType: 'application/json', body: JSON.stringify(body) })
    })
    await h.start()
    assert.match(await card(h).innerText(), /Creation revision.*2020-02-03 04:05/)
    assert.match(await card(h, 'cache-view').innerText(), /Current reviewed revision.*2021-03-04 05:17/)
    await card(h).locator('.observation-sha .copyable-value').click()
    await h.page.waitForFunction(() => window.historyCopies.length === 1)
    assert.equal(await h.page.evaluate(() => window.historyCopies[0]), originalSha)
    await screenshot(h, 'folded-revisions.png')
    await card(h).locator('.observation-card').click(); await h.idle()
    assert.equal(await h.page.locator('.observation-detail-sha').count(), 1)
    await h.page.locator('#observation-history-toggle').click()
    await h.page.waitForFunction(() => document.querySelectorAll('.observation-history-entry').length === 1)
    assert.equal(await rows(h).locator('.copyable-value').innerText(), originalSha)
    assert.equal(await rows(h).locator('.observation-history-time').innerText(), '2020-02-03 04:05')
    await screenshot(h, 'legacy-compact-history.png')
    await card(h, 'cache-view').locator('.observation-card').click(); await h.idle()
    await h.page.locator('#observation-history-toggle').click()
    await history.waitFor(value => value.offset === 25)
    assert.equal(await rows(h).count(), 25)
    assert.equal(await h.page.locator('.observation-history [role=status]').innerText(), 'Loading…')
    const placement = await h.page.locator('#observation-history-toggle').evaluate(element => ({ sharesValue: !!element.closest('dd')?.querySelector('.observation-revision-value'), top: element.getBoundingClientRect().top, shaBottom: element.closest('dd')?.querySelector('.observation-revision-value').getBoundingClientRect().bottom }))
    assert.ok(placement.sharesValue); assert.ok(placement.top >= placement.shaBottom)
    await h.page.locator('#observation-history-toggle').click()
    history.receipts.find(value => value.offset === 25).release()
    await history.waitFor(value => value.offset === 25 && value.done); await paint(h.page)
    assert.equal(await rows(h).count(), 0); assert.deepEqual(history.receipts.filter(value => value.id === 'cache-view').map(value => value.offset), [0, 25])
    await h.page.locator('#observation-history-toggle').click()
    await h.page.locator('.observation-history [role=alert]').waitFor()
    assert.equal(await rows(h).count(), 50); await paint(h.page)
    assert.deepEqual(history.receipts.filter(value => value.id === 'cache-view').map(value => value.offset), [0, 25, 50])
    await h.page.locator('#observation-history-load').click()
    await h.page.waitForFunction(() => document.querySelectorAll('.observation-history-entry').length === 53)
    assert.deepEqual(history.receipts.filter(value => value.id === 'cache-view').map(value => value.offset), [0, 25, 50, 50])
    assert.equal(await h.page.locator('.observation-history [role=status], .observation-history [role=alert]').count(), 0)
    const expected = Array.from({ length: 53 }, (_, index) => provenance(53 - index))
    assert.deepEqual(await rows(h).locator('.copyable-value').allTextContents(), expected.map(value => value.reviewed_git_sha))
    assert.deepEqual(await rows(h).locator('.observation-history-time').allTextContents(), expected.map(value => stamp(value.sequence === 1 ? h.fixture.observations[1].created_at : value.recorded_at)))
    assert.ok(!(await h.page.locator('.observation-history').innerText()).match(/head|assertion|actor|update|legacy_creation|version|digest/i))
    await rows(h).first().locator('.copyable-value').focus(); await h.page.keyboard.press('Enter')
    await h.page.waitForFunction(() => window.historyCopies.length === 2)
    assert.equal(await h.page.evaluate(() => window.historyCopies[1]), expected[0].reviewed_git_sha)
    const current = h.page.locator('.observation-current-provenance')
    assert.match(await current.innerText(), new RegExp(reviewedSha + '.*2021-03-04 05:17', 's'))
    await current.locator('.observation-revision-value .copyable-value').click()
    await h.page.waitForFunction(() => window.historyCopies.length === 3)
    assert.equal(await h.page.evaluate(() => window.historyCopies[2]), reviewedSha)
    await screenshot(h, 'current-revision.png')
    assert.deepEqual(h.errors, [])
  } finally { history?.releaseAll(); await h.close() }
})

test('production delayed history cannot populate another observation and an open changed head restarts its pages', { timeout: 60000 }, async () => {
  const h = await openDiscovery(undefined, transform)
  let history
  try {
    history = await historyRoutes(h, async ({ route, receipt, body, hold }) => {
      if (receipt.id === 'cache-view' && receipt.offset === 25 && history.receipts.filter(value => value.id === 'cache-view' && value.offset === 25).length === 1) await hold()
      await route.fulfill({ status: 200, contentType: 'application/json', body: JSON.stringify(body) })
    })
    await h.start(); await card(h, 'cache-view').locator('.observation-card').click(); await h.idle()
    await h.page.locator('#observation-history-toggle').click(); await history.waitFor(value => value.offset === 25)
    await card(h).locator('.observation-card').click(); await h.idle()
    history.receipts.find(value => value.id === 'cache-view' && value.offset === 25).release()
    await history.waitFor(value => value.id === 'cache-view' && value.offset === 25 && value.done); await paint(h.page)
    assert.equal(await rows(h).count(), 0)
    await card(h, 'cache-view').locator('.observation-card').click(); await h.idle()
    await h.page.locator('#observation-history-toggle').click()
    await h.page.waitForFunction(() => document.querySelectorAll('.observation-history-entry').length === 53)
    h.fixture.observations[1] = { ...h.fixture.observations[1], latest_sequence: 54, content_version: '30000000-0000-4000-8000-000000000000', current_provenance: { ...provenance(54), content_version: '30000000-0000-4000-8000-000000000000', content_digest: createHash('sha256').update(h.fixture.observations[1].content).digest('hex') } }
    await h.page.evaluate(workspace => window.pushHierarchyFrames([{ schema_version: 1, type: 'change', event: { schema_version: 1, event_id: 'history-refresh', scope: 'workspace', workspace_id: workspace, occurred_at: '2026-10-08T10:00:00Z', transaction: { id: 'history-refresh-tx', cause: 'rest', request_id: null }, actor: { type: 'system', id: null }, entity: { type: 'observation', id: 'cache-view', action: 'updated' }, invalidations: [{ kind: 'collection', target: 'observations:' + workspace }] } }]), h.fixture.workspace.id)
    await h.page.waitForFunction(() => document.querySelectorAll('.observation-history-entry').length === 54)
    assert.deepEqual(history.receipts.filter(value => value.id === 'cache-view').map(value => [value.head, value.offset]), [[53, 0], [53, 25], [53, 0], [53, 25], [53, 50], [54, 0], [54, 25], [54, 50]])
    assert.equal(await rows(h).first().locator('.copyable-value').innerText(), provenance(54).reviewed_git_sha)
    assert.deepEqual(h.errors, [])
  } finally { history?.releaseAll(); await h.close() }
})

test('production completed old-workspace history cannot restore rows after a workspace switch', { timeout: 60000 }, async () => {
  const other = hierarchyFixture()
  other.workspace = { ...other.workspace, id: 'other-workspace', name: 'Other workspace' }
  other.projects = []; other.tasks = []; other.observations = []
  const h = await openDiscovery(undefined, transform, [other])
  let history
  try {
    history = await historyRoutes(h, async ({ route, receipt, body, hold }) => {
      if (receipt.offset === 25) await hold()
      await route.fulfill({ status: 200, contentType: 'application/json', body: JSON.stringify(body) })
    })
    await h.start(); await card(h, 'cache-view').locator('.observation-card').click(); await h.idle()
    await h.page.locator('#observation-history-toggle').click(); await history.waitFor(value => value.offset === 25)
    await h.page.locator('a.sidebar-link[href="/workspace/other-workspace"]').click({ timeout: 5000 })
    await h.page.getByRole('button', { name: 'Observations', exact: true }).click(); await h.idle()
    history.receipts.find(value => value.offset === 25).release()
    await history.waitFor(value => value.offset === 25 && value.done); await paint(h.page)
    assert.ok(h.page.url().includes('/workspace/other-workspace'))
    assert.equal(await rows(h).count(), 0)
    assert.equal(await h.page.locator('#observation-detail').count(), 0)
    assert.deepEqual(history.receipts.map(value => value.offset), [0, 25])
    assert.deepEqual(h.errors, [])
  } finally { history?.releaseAll(); await h.close() }
})
