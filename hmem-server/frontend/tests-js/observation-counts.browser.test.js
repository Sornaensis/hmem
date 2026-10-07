import assert from 'node:assert/strict'
import test from 'node:test'
import { mkdir } from 'node:fs/promises'
import { join } from 'node:path'
import { openDiscovery } from './observation-discovery-fixture.mjs'
import { paint } from './observation-viewport-fixture.mjs'
import { fixtureObservationCounts } from './hierarchy-fixture.mjs'

const counts = h => h.receipts.filter(value => value.endpoint.endsWith('/count'))
async function capture(h, name) {
  if (!process.env.HMEM_OBSERVATION_COUNT_SCREENSHOT_DIR) return
  await mkdir(process.env.HMEM_OBSERVATION_COUNT_SCREENSHOT_DIR, { recursive: true })
  await h.page.locator('#main-content-scroll').hover(); await h.page.mouse.wheel(0, -10000); await paint(h.page)
  await h.page.locator('.workspace-header').waitFor({ state: 'visible' })
  if (name === 'observation-counts-filtered-narrow') {
    const geometry = await h.page.evaluate(() => {
      const host = document.getElementById('main-content-scroll'), rect = element => element.getBoundingClientRect().toJSON()
      return { host: rect(host), scrollWidth: host.scrollWidth, clientWidth: host.clientWidth, top: host.scrollTop,
        title: rect(document.querySelector('.workspace-header-title')), name: rect(document.querySelector('.workspace-header-title .editable-text')),
        summary: rect(document.querySelector('.sticky-workspace-summary')), header: rect(document.querySelector('.observation-mode-heading')),
        overflow: [...host.querySelectorAll('*')].filter(element => { const r = element.getBoundingClientRect(); return r.width && (r.left < 0 || r.right > 321) }).slice(0, 12).map(element => ({ tag: element.tagName, class: element.className, rect: rect(element) })) }
    })
    console.log('Enlarged narrow layout: ' + JSON.stringify(geometry))
    // The Observation workspace bar deliberately scrolls with content. Prove
    // its count first, then use actual wheel intent to reach the filter header.
    assert.match(await h.page.locator('.sticky-workspace-summary').textContent(), /265 Observations total/)
    const deadline = Date.now() + 5000
    while (Date.now() < deadline) {
      const distance = await h.page.locator('.observation-mode-heading').evaluate(element => element.getBoundingClientRect().top - document.getElementById('main-content-scroll').getBoundingClientRect().top - 12)
      if (Math.abs(distance) < 2) break
      await h.page.locator('#main-content-scroll').hover(); await h.page.mouse.wheel(0, distance); await h.page.waitForTimeout(50); await paint(h.page)
    }
    const visibleHeader = await h.page.locator('.observation-mode-navigation').evaluate(element => {
      const bounds = element.getBoundingClientRect(), host = document.getElementById('main-content-scroll').getBoundingClientRect()
      return { visible: bounds.top >= host.top && bounds.bottom <= host.bottom, bounds: bounds.toJSON(), host: host.toJSON() }
    })
    assert.ok(visibleHeader.visible, 'Applied mode and full match count are genuinely visible after user scroll at 200% text: ' + JSON.stringify(visibleHeader))
  }
  await h.page.screenshot({ path: join(process.env.HMEM_OBSERVATION_COUNT_SCREENSHOT_DIR, name + '.png') })
}

for (const width of [1440, 320]) test(`production ${width}: 200 pages and authoritative applied/workspace counts`, { timeout: 60000 }, async () => {
  const h = await openDiscovery({ width, height: 1000 }, values => [...values, ...Array.from({ length: 201 }, (_, i) => ({ ...values[3], id: 'extra-' + i, subjects: [{ subject_kind: 'file', subject: `docs/Extra-${i}.md` }, { subject_kind: 'glob', subject: 'docs/**/*.md' }] }))])
  try {
    await h.start()
    assert.equal(await h.page.locator('#observation-viewport').getAttribute('data-observation-loaded-count'), '200')
    assert.equal(await h.page.locator('.workspace-header .workspace-observation-total').textContent(), '265 Observations total')
    assert.equal(await h.page.locator('.observation-mode-announcement').count(), 0)
    assert.ok(await h.page.locator('[data-observation-key]').count() <= 28)
    const first = h.receipts.find(value => value.endpoint === '/api/v1/observations')
    assert.equal(first.params.limit, '200'); assert.equal(first.response.items.length, 200); assert.equal(first.response.has_more, true)
    await capture(h, `observation-counts-unfiltered-${width === 320 ? 'narrow' : 'desktop'}`)
    const countRequests = counts(h).length
    await h.page.getByRole('button', { name: 'Load more', exact: true }).click(); await h.idle()
    assert.equal(await h.page.locator('#observation-viewport').getAttribute('data-observation-loaded-count'), '265')
    assert.equal(counts(h).length, countRequests)
    assert.equal(h.receipts.filter(value => value.endpoint === '/api/v1/observations').at(-1).params.offset, '200')
    await h.page.locator('#observation-query').fill('Cache')
    assert.equal(counts(h).length, countRequests); assert.equal(await h.page.locator('.observation-mode-announcement').count(), 0)
    await h.page.getByRole('button', { name: 'Apply filters', exact: true }).click(); await h.idle()
    assert.equal(await h.page.locator('.observation-mode-announcement').textContent(), '2 Observations match')
    assert.equal(await h.page.locator('.workspace-header .workspace-observation-total').textContent(), '265 Observations total')
    if (width === 320) await h.page.evaluate(() => { document.documentElement.style.fontSize = '200%' })
    await capture(h, `observation-counts-filtered-${width === 320 ? 'narrow' : 'desktop'}`)
    assert.ok(await h.page.evaluate(() => document.documentElement.scrollWidth <= innerWidth + 1))
    assert.ok(await h.page.evaluate(() => {
      const host = document.getElementById('main-content-scroll'), edge = host.getBoundingClientRect()
      return host.scrollWidth <= host.clientWidth + 1 && ['.workspace-header-title .editable-text', '.sticky-workspace-summary', '.observation-mode-heading', '#observation-query'].every(selector => {
        const bounds = document.querySelector(selector).getBoundingClientRect()
        return bounds.left >= edge.left - 1 && bounds.right <= edge.right + 1
      })
    }), 'Workspace name, total and applied-filter controls fit their actual scroll host at enlarged text')
    await h.page.locator('#main-content-scroll').hover(); await h.page.mouse.wheel(0, 1200); await paint(h.page)
    assert.match(await h.page.locator('.sticky-workspace-bar .workspace-observation-total').textContent(), /265 Observations total/)
    await h.page.locator('#observation-query').fill('absentword'); await h.page.getByRole('button', { name: 'Apply filters', exact: true }).click(); await h.idle()
    assert.equal(await h.page.locator('.observation-mode-announcement').textContent(), '0 Observations match')
    assert.equal(await h.page.locator('.workspace-header .workspace-observation-total').textContent(), '265 Observations total')
    await h.page.locator('#observation-query').fill('Documentation'); await h.page.getByRole('button', { name: 'Apply filters', exact: true }).click(); await h.idle()
    assert.equal(await h.page.locator('.observation-mode-announcement').textContent(), '262 Observations match', 'Full filtered count exceeds the 200 loaded rows')
    await h.page.locator('#observation-query').fill(''); await h.page.getByRole('button', { name: 'Apply filters', exact: true }).click(); await h.idle()
    await h.page.getByRole('button', { name: 'Files', exact: true }).click()
    await h.page.locator('#observation-match-paths').fill('src/Main.elm\nsrc/View.elm\nsrc/Main.elm')
    await h.page.getByRole('button', { name: 'Match files', exact: true }).click(); await h.idle()
    assert.equal(await h.page.locator('.observation-mode-announcement').textContent(), '2 Observations match')
    assert.equal(counts(h).at(-1).response.match_count, 2)
    assert.deepEqual(h.errors, [])
  } finally { await h.close() }
})

test('production inactive workspace totals coalesce stale replies, count unloaded deletion and recover from failure', { timeout: 60000 }, async () => {
  const h = await openDiscovery(undefined, values => [...values, ...Array.from({ length: 201 }, (_, i) => ({ ...values[3], id: 'extra-' + i, subjects: [{ subject_kind: 'file', subject: `docs/Extra-${i}.md` }] }))])
  let release
  try {
    await h.start()
    assert.equal(await h.page.locator('#observation-viewport').getAttribute('data-observation-loaded-count'), '200')
    assert.ok(!h.receipts.find(value => value.endpoint === '/api/v1/observations').response.items.some(value => value.id === 'extra-0'), 'Deleted ID is outside the admitted 200-row cache')
    await h.page.getByRole('button', { name: 'Projects', exact: true }).click(); await h.idle()
    assert.equal(await h.page.locator('#observation-panel').count(), 0)
    const requests = []; let held = true, fail = false, sequence = 0
    await h.page.route('**/api/v1/observations/count', async route => {
      const query = route.request().postDataJSON(); requests.push(query)
      const body = fixtureObservationCounts(h.fixture, query)
      if (held) { held = false; await new Promise(resolve => { release = resolve }); body.total_count = 999; body.match_count = 999 }
      if (fail) { fail = false; await route.fulfill({ status: 503, body: '{}' }); return }
      await route.fulfill({ status: 200, contentType: 'application/json', body: JSON.stringify(body) })
    })
    const push = async (action = 'created', id = 'unloaded-observation') => h.page.evaluate(frame => window.pushHierarchyFrames([frame]), {
      schema_version: 1, type: 'change', event: { schema_version: 1, event_id: 'count-' + (++sequence), scope: 'workspace', workspace_id: h.fixture.workspace.id,
        occurred_at: '2026-10-07T00:00:00Z', transaction: { id: 'count-tx-' + sequence, cause: 'rest', request_id: null }, actor: { type: 'system', id: null },
        entity: { type: 'observation', id, action }, invalidations: [{ kind: 'collection', target: 'observations:' + h.fixture.workspace.id }] }
    })
    await push(); await h.bounded((async () => { while (!release) await h.page.waitForTimeout(20) })(), 5000, 'Held inactive count request')
    assert.match(await h.page.locator('.workspace-header .workspace-observation-total').textContent(), /Updating Observation total/)
    await push(); await push(); await paint(h.page); assert.equal(requests.length, 1)
    release(); release = null; await h.idle()
    assert.equal(requests.length, 2); assert.equal(await h.page.locator('.workspace-header .workspace-observation-total').textContent(), '265 Observations total')
    h.fixture.observations = h.fixture.observations.filter(value => value.id !== 'extra-0')
    await push('deleted', 'extra-0'); await h.idle()
    assert.equal(await h.page.locator('.workspace-header .workspace-observation-total').textContent(), '264 Observations total', 'Committed off-page deletion invalidates the full total')
    fail = true; await push(); await h.idle()
    assert.match(await h.page.locator('.workspace-header .workspace-observation-total').textContent(), /Observation total unavailable/)
    const failedRequests = requests.length; await paint(h.page); assert.equal(requests.length, failedRequests)
    await h.page.locator('.workspace-header').getByRole('button', { name: 'Retry counts', exact: true }).click(); await h.idle()
    assert.equal(requests.length, failedRequests + 1); assert.equal(await h.page.locator('.workspace-header .workspace-observation-total').textContent(), '264 Observations total')
    assert.equal(await h.page.locator('#observation-panel').count(), 0)
  } finally { release?.(); await h.close() }
})

test('production held count success cannot survive live read revocation and a fresh session reload', { timeout: 60000 }, async () => {
  const h = await openDiscovery()
  let release
  try {
    let canRead = true, hold = true
    await h.page.route('**/api/v1/session?**', async route => {
      await route.fulfill({ status: 200, contentType: 'application/json', body: JSON.stringify({ auth_mode: 'local',
        principal: { actor_type: 'user', actor_id: 'fixture-user', actor_label: 'Fixture User', authority: 'local', grant_user_id: null }, global_permissions: { create_workspace: false, superadmin: false },
        workspace: { workspace_id: h.fixture.workspace.id, role: canRead ? 'owner' : null, can_read: canRead, can_edit: canRead, can_admin: canRead } }) })
    })
    await h.page.route('**/api/v1/observations/count', async route => {
      const body = fixtureObservationCounts(h.fixture, route.request().postDataJSON())
      if (hold) { hold = false; await new Promise(resolve => { release = resolve }); body.total_count = 999; body.match_count = 999 }
      await route.fulfill({ status: 200, contentType: 'application/json', body: JSON.stringify(body) })
    })
    await h.start(); await h.bounded((async () => { while (!release) await h.page.waitForTimeout(20) })(), 5000, 'Count held before revocation')
    assert.match(await h.page.locator('.workspace-header .workspace-observation-total').textContent(), /Loading Observation total/)
    canRead = false
    await h.page.evaluate(workspace => window.pushHierarchyFrames([{ schema_version: 1, type: 'access_revoked', workspace_id: workspace }]), h.fixture.workspace.id)
    await h.page.waitForFunction(() => !document.getElementById('observation-panel'), null, { timeout: 5000 })
    release(); release = null; await h.idle()
    assert.equal(await h.page.locator('.workspace-observation-total').count(), 0)
    assert.ok(!(await h.page.locator('body').textContent()).includes('999 Observations'))
    canRead = true; await h.page.reload(); await h.page.locator('#observation-panel').waitFor(); await h.idle()
    assert.equal(await h.page.locator('.workspace-header .workspace-observation-total').textContent(), '64 Observations total')
  } finally { release?.(); await h.close() }
})
