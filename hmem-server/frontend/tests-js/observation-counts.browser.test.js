import assert from 'node:assert/strict'
import test from 'node:test'
import { mkdir } from 'node:fs/promises'
import { join } from 'node:path'
import { openDiscovery } from './observation-discovery-fixture.mjs'
import { paint } from './observation-viewport-fixture.mjs'
import { fixtureObservationCounts, hierarchyFixture } from './hierarchy-fixture.mjs'

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
    assert.match(await h.page.locator('.sticky-workspace-summary').textContent(), /265 observations/)
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
    assert.equal((await h.page.locator('.workspace-header .workspace-observation-total').textContent()).replace(/^\s*·\s*/, ''), '265 observations')
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
    await h.page.locator('#observation-query').press('Enter'); await h.idle()
    assert.equal(await h.page.locator('.observation-mode-announcement').textContent(), '2 Observations match')
    assert.equal((await h.page.locator('.workspace-header .workspace-observation-total').textContent()).replace(/^\s*·\s*/, ''), '265 observations')
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
    assert.match(await h.page.locator('.sticky-workspace-bar .workspace-observation-total').textContent(), /265 observations/)
    await h.page.locator('#observation-query').fill('absentword'); await h.page.locator('#observation-query').press('Enter'); await h.idle()
    assert.equal(await h.page.locator('.observation-mode-announcement').textContent(), '0 Observations match')
    assert.equal((await h.page.locator('.workspace-header .workspace-observation-total').textContent()).replace(/^\s*·\s*/, ''), '265 observations')
    await h.page.locator('#observation-query').fill('Documentation'); await h.page.locator('#observation-query').press('Enter'); await h.idle()
    assert.equal(await h.page.locator('.observation-mode-announcement').textContent(), '262 Observations match', 'Full filtered count exceeds the 200 loaded rows')
    await h.page.locator('#observation-query').fill(''); await h.page.locator('#observation-query').press('Enter'); await h.idle()
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
    await h.page.route('**/api/v1/observations/count**', async route => {
      const query = route.request().postDataJSON()
      if (new URL(route.request().url()).searchParams.has('query_scope')) {
        await route.fulfill({ status: 200, json: fixtureObservationCounts(h.fixture, query) }); return
      }
      requests.push(query)
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
    await push(); await h.bounded((async () => { while (!release) await h.page.waitForTimeout(20) })(), 5000, 'Held inactive count request'); await paint(h.page)
    assert.match((await h.page.locator('.workspace-header .workspace-observation-total').textContent()).replace(/^\s*·\s*/, ''), /Updating Observation total/)
    await push(); await push(); await paint(h.page); assert.equal(requests.length, 1)
    release(); release = null; await h.idle()
    assert.equal(requests.length, 2); assert.equal((await h.page.locator('.workspace-header .workspace-observation-total').textContent()).replace(/^\s*·\s*/, ''), '265 observations')
    h.fixture.observations = h.fixture.observations.filter(value => value.id !== 'extra-0')
    await push('deleted', 'extra-0'); await h.idle()
    assert.equal((await h.page.locator('.workspace-header .workspace-observation-total').textContent()).replace(/^\s*·\s*/, ''), '264 observations', 'Committed off-page deletion invalidates the full total')
    fail = true; await push(); await h.idle()
    assert.match((await h.page.locator('.workspace-header .workspace-observation-total').textContent()).replace(/^\s*·\s*/, ''), /Observation total unavailable/)
    const failedRequests = requests.length; await paint(h.page); assert.equal(requests.length, failedRequests)
    await h.page.locator('.workspace-header').getByRole('button', { name: 'Retry counts', exact: true }).click(); await h.idle()
    assert.equal(requests.length, failedRequests + 1); assert.equal((await h.page.locator('.workspace-header .workspace-observation-total').textContent()).replace(/^\s*·\s*/, ''), '264 observations')
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
    await h.page.route('**/api/v1/observations/count**', async route => {
      const body = fixtureObservationCounts(h.fixture, route.request().postDataJSON())
      if (new URL(route.request().url()).searchParams.has('query_scope')) {
        await route.fulfill({ status: 200, json: body }); return
      }
      if (hold) { hold = false; await new Promise(resolve => { release = resolve }); body.total_count = 999; body.match_count = 999 }
      await route.fulfill({ status: 200, contentType: 'application/json', body: JSON.stringify(body) })
    })
    await h.start(); await h.bounded((async () => { while (!release) await h.page.waitForTimeout(20) })(), 5000, 'Count held before revocation')
    assert.match((await h.page.locator('.workspace-header .workspace-observation-total').textContent()).replace(/^\s*·\s*/, ''), /Loading Observation total/)
    canRead = false
    await h.page.evaluate(workspace => window.pushHierarchyFrames([{ schema_version: 1, type: 'access_revoked', workspace_id: workspace }]), h.fixture.workspace.id)
    await h.page.waitForFunction(() => !document.getElementById('observation-panel'), null, { timeout: 5000 })
    release(); release = null; await h.idle()
    assert.equal(await h.page.locator('.workspace-observation-total').count(), 0)
    assert.ok(!(await h.page.locator('body').textContent()).includes('999 observations'))
    canRead = true; await h.page.reload(); await h.page.locator('#observation-panel').waitFor(); await h.idle()
    assert.equal((await h.page.locator('.workspace-header .workspace-observation-total').textContent()).replace(/^\s*·\s*/, ''), '64 observations')
  } finally { release?.(); await h.close() }
})


for (const phase of ['Initial', 'HeaderOnly']) test(`production ${phase}: held workspace and stale browse counts settle after Tree entry and release bounded project demand`, { timeout: 60000 }, async () => {
  const h = await openDiscovery()
  const hierarchy = hierarchyFixture()
  h.fixture.projects = hierarchy.projects
  h.fixture.tasks = hierarchy.tasks
  const releases = []
  const requests = []
  let hold = phase === 'Initial'
  try {
    await h.page.route('**/api/v1/observations/count**', async route => {
      const query = route.request().postDataJSON()
      const scoped = new URL(route.request().url()).searchParams.has('query_scope')
      const receipt = { query, scoped, done: false }
      requests.push(receipt)
      const body = fixtureObservationCounts(h.fixture, query)
      if (hold) {
        await new Promise(resolve => releases.push(resolve))
        if (scoped) { body.total_count = 999; body.match_count = 999 }
      }
      await route.fulfill({ status: 200, json: body })
      receipt.done = true
    })
    await h.start()
    if (phase === 'HeaderOnly') {
      assert.match(await h.page.locator('.workspace-header .workspace-observation-total').textContent(), /64 observations/)
      hold = true
      await h.page.evaluate(workspace => window.pushHierarchyFrames([{ schema_version: 1, type: 'change', event: {
        schema_version: 1, event_id: 'held-header-refresh', scope: 'workspace', workspace_id: workspace,
        occurred_at: '2026-10-19T00:00:00Z', transaction: { id: 'held-header-tx', cause: 'rest', request_id: null }, actor: { type: 'system', id: null },
        entity: { type: 'observation', id: 'unloaded-observation', action: 'created' }, invalidations: [{ kind: 'collection', target: 'observations:' + workspace }]
      } }]), h.fixture.workspace.id)
    }
    await h.bounded((async () => { while (releases.length < 2) await h.page.waitForTimeout(20) })(), 5000, 'Both independent count lifetimes held')
    const heldCount = requests.length
    await h.page.getByRole('button', { name: 'Subject Tree', exact: true }).click()
    await h.bounded((async () => { while (!h.receipts.some(value => value.endpoint.endsWith('/subject-tree') && value.done)) await h.page.waitForTimeout(20) })(), 5000, 'Tree root completed while counts held')
    assert.match(await h.page.locator('.workspace-header .workspace-observation-total').textContent(), phase === 'Initial' ? /Loading Observation total/ : /Updating Observation total/)
    await h.page.getByRole('button', { name: 'Projects', exact: true }).click()
    await h.page.locator('#entity-root-project').waitFor({ timeout: 5000 })
    await paint(h.page)
    assert.equal(requests.length, heldCount, 'Mode changes neither retire nor replace the occupied workspace read')
    if (phase === 'Initial') assert.ok(requests.length + h.requests.filter(value => value.path.endsWith('/planning-counts') || (value.kind && value.parent) || /\/api\/v1\/(projects|tasks)\/[^/]+$/.test(value.path)).length <= 6, 'Initial headers and ordinary owners share six startup credits')
    hold = false
    for (const release of releases.splice(0)) release()
    await h.idle()
    await h.bounded((async () => { while (requests.some(value => !value.done)) await h.page.waitForTimeout(20) })(), 5000, 'Exact count callbacks complete')
    assert.match(await h.page.locator('.workspace-header .workspace-observation-total').textContent(), /64 observations/)
    await h.bounded((async () => { while (!h.requests.some(value => value.parent === 'root-project' && value.kind === 'project' && value.done)) await h.page.waitForTimeout(20) })(), 5000, 'Project branch progresses after header settlement')
    assert.ok(h.requests.some(value => /\/api\/v1\/(projects|tasks)\/[^/]+$/.test(value.path) && value.done), 'Visible project/task details progress')
    assert.ok(h.caps().maxBranches <= 4 && h.caps().maxDetails <= 6)
    assert.ok(await h.page.locator('[data-hierarchy-key]').count() <= 28)
    assert.ok(requests.filter(value => !value.scoped).length <= (phase === 'Initial' ? 2 : 3), 'At most one coalesced canonical invalidation follows the held workspace read')
    assert.deepEqual(h.errors, [])
  } finally { for (const release of releases) release(); await h.close() }
})


test('production direct Tree selection keeps workspace total independent of subject search and inactive live changes', { timeout: 60000 }, async () => {
  const h = await openDiscovery()
  try {
    const query = ['tree', 'src', null, '', '', null, null, [], '', '']
    await h.page.goto(h.origin + '/workspace/' + h.fixture.workspace.id + '#tab=observations&ov=1&oq=' + encodeURIComponent(JSON.stringify(query)) + '&observation=cache-main')
    await h.page.locator('#observation-panel').waitFor(); await h.idle()
    assert.match(await h.page.locator('.workspace-header .workspace-observation-total').textContent(), /64 observations/)
    assert.equal(await h.page.getByRole('button', { name: 'Subject Tree', exact: true }).getAttribute('aria-pressed'), 'true')
    assert.ok(h.receipts.some(value => value.endpoint.endsWith('/subject-tree') && value.params.subject_query === 'src'))
    assert.ok(counts(h).every(value => Object.keys(value.payload).join() === 'workspace_id'), 'Header query excludes every Tree/selection filter')
    const before = counts(h).length
    await h.page.locator('#observation-query').fill('docs'); await h.page.locator('#observation-query').press('Enter'); await h.idle()
    assert.equal(counts(h).length, before, 'Subject search does not recount the workspace')
    await h.page.getByRole('button', { name: 'Projects', exact: true }).click(); await h.idle()
    h.fixture.observations.push({ ...h.fixture.observations[0], id: 'new-unloaded', subjects: [{ subject_kind: 'file', subject: 'other/Unloaded.elm' }] })
    await h.page.evaluate(workspace => window.pushHierarchyFrames([{ schema_version: 1, type: 'change', event: {
      schema_version: 1, event_id: 'tree-inactive-create', scope: 'workspace', workspace_id: workspace,
      occurred_at: '2026-10-19T00:00:00Z', transaction: { id: 'tree-inactive-tx', cause: 'rest', request_id: null }, actor: { type: 'system', id: null },
      entity: { type: 'observation', id: 'new-unloaded', action: 'created' }, invalidations: [{ kind: 'collection', target: 'observations:' + workspace }]
    } }]), h.fixture.workspace.id)
    await h.idle()
    assert.match(await h.page.locator('.workspace-header .workspace-observation-total').textContent(), /65 observations/)
    for (const action of ['updated', 'deleted']) {
      const reads = counts(h).length
      if (action === 'deleted') h.fixture.observations = h.fixture.observations.filter(value => value.id !== 'new-unloaded')
      await h.page.evaluate(({ workspace, action }) => window.pushHierarchyFrames([{ schema_version: 1, type: 'change', event: {
        schema_version: 1, event_id: 'tree-inactive-' + action, scope: 'workspace', workspace_id: workspace,
        occurred_at: '2026-10-19T00:00:00Z', transaction: { id: 'tree-inactive-' + action + '-tx', cause: 'rest', request_id: null }, actor: { type: 'system', id: null },
        entity: { type: 'observation', id: 'new-unloaded', action }, invalidations: [{ kind: 'collection', target: 'observations:' + workspace }]
      } }]), { workspace: h.fixture.workspace.id, action })
      await h.idle()
      assert.ok(counts(h).length > reads, action + ' refreshes the independent total')
      assert.match(await h.page.locator('.workspace-header .workspace-observation-total').textContent(), action === 'deleted' ? /64 observations/ : /65 observations/)
    }
    h.fixture.observations = h.fixture.observations.filter(value => value.id !== 'other-js')
    const beforeResync = counts(h).length
    await h.page.evaluate(() => window.pushHierarchyFrames([{ schema_version: 1, type: 'resync_required' }]))
    await h.idle()
    assert.ok(counts(h).length > beforeResync, 'Canonical resync refreshes total with Tree filtered and inactive')
    assert.match(await h.page.locator('.workspace-header .workspace-observation-total').textContent(), /63 observations/)
    assert.equal(await h.page.locator('#observation-panel').count(), 0)
    await h.page.getByRole('button', { name: 'Observations', exact: true }).click(); await h.idle()
    assert.equal(await h.page.getByRole('button', { name: 'Subject Tree', exact: true }).getAttribute('aria-pressed'), 'true')
    assert.equal(await h.page.locator('#observation-query').inputValue(), 'docs')
    assert.match(await h.page.locator('.workspace-header .workspace-observation-total').textContent(), /63 observations/)
  } finally { await h.close() }
})
