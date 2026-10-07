import assert from 'node:assert/strict'
import test from 'node:test'
import { openDiscovery } from './observation-discovery-fixture.mjs'

const resultReceipts = h => h.receipts.filter(value => !value.endpoint.endsWith('/count'))
const tuple = (mode, query = '', kind = null, subject = '', sha = '', facetKind = null, facet = null, paths = []) => [mode, query, kind, subject, sha, facetKind, facet, paths]
const fragment = (query, id = null) => '#tab=observations&ov=1&oq=' + encodeURIComponent(JSON.stringify(query)) + (id ? '&observation=' + encodeURIComponent(id) : '')
const applied = page => page.locator('.observation-applied-filters').innerText()
async function ready(h) { await h.page.locator('#observation-panel').waitFor(); await h.idle() }
async function history(h, direction) { await h.page.evaluate(direction => window.history[direction](), direction); await ready(h) }
async function navigate(h, query, id) { await h.page.goto(h.origin + '/workspace/' + h.fixture.workspace.id + fragment(query, id)); await ready(h) }

test('production applied flat URI excludes drafts, has no Copy link, and reloads once after admission', { timeout: 60000 }, async () => {
  const h = await openDiscovery()
  try {
    await h.start()
    await h.page.locator('#observation-query').fill('Cache evidence')
    await h.page.locator('#observation-query').press('Enter'); await ready(h)
    const appliedUrl = h.page.url(), before = resultReceipts(h).length
    assert.deepEqual(JSON.parse(new URLSearchParams(new URL(appliedUrl).hash.slice(1)).get('oq')), tuple('flat', 'Cache evidence'))
    await h.page.locator('#observation-query').fill('unapplied draft & secret')
    assert.equal(await h.page.getByRole('button', { name: 'Copy link', exact: true }).count(), 0)
    assert.equal(await h.page.locator('.observation-share-controls').count(), 0)
    assert.equal(h.page.url(), appliedUrl)
    assert.equal(resultReceipts(h).length, before)
    const count = resultReceipts(h).length
    await h.page.reload(); await ready(h)
    assert.equal(await h.page.locator('#observation-query').inputValue(), 'Cache evidence')
    assert.equal(resultReceipts(h).length, count + 1)
    assert.equal(resultReceipts(h).at(-1).params.query, 'Cache evidence')
    assert.deepEqual(resultReceipts(h).at(-1).response.items.map(row => row.id).sort(), ['cache-main', 'cache-view'])
    assert.equal(h.page.url(), appliedUrl)
    await h.page.locator('.observation-card').first().click(); await ready(h)
    await h.page.locator('#observation-delete').click()
    await h.page.getByRole('dialog', { name: 'Delete observation permanently?' }).waitFor()
    assert.equal(await h.page.locator('.observation-curation-background').getAttribute('inert'), '')
    assert.equal(await h.page.locator('.observation-curation-background').getAttribute('aria-hidden'), 'true')
  } finally { await h.close() }
})

test('production subject and locked exact links reload the correct mode and Back restores the previous applied tuple without echo', { timeout: 60000 }, async () => {
  const h = await openDiscovery()
  try {
    await h.start(); await h.page.getByRole('button', { name: 'Subject', exact: true }).click(); await ready(h)
    const facetUrl = h.page.url()
    await h.page.reload(); await ready(h)
    assert.equal(resultReceipts(h).at(-1).endpoint, '/api/v1/observations/subject-facets')
    await h.page.locator('.observation-facet').filter({ hasText: 'src/**/*.elm' }).locator('.observation-facet-card').click(); await ready(h)
    await h.page.locator('#observation-query').fill('Cache evidence'); await h.page.locator('#observation-query').press('Enter'); await ready(h)
    const exactUrl = h.page.url(), before = resultReceipts(h).length
    await h.page.reload(); await ready(h)
    assert.equal(resultReceipts(h).length, before + 1)
    assert.equal(resultReceipts(h).at(-1).params.subject_kind, 'glob'); assert.equal(resultReceipts(h).at(-1).params.subject, 'src/**/*.elm')
    assert.equal(resultReceipts(h).at(-1).params.query, 'Cache evidence')
    await h.page.locator('#observation-advanced-toggle').click()
    assert.match(await h.page.locator('.observation-selected-facet-value').innerText(), /Glob: src\/\*\*\/\*\.elm/)
    assert.equal(await h.page.locator('#observation-subject').count(), 0)
    assert.equal(h.page.url(), exactUrl)
    await history(h, 'back'); assert.equal(resultReceipts(h).at(-1).params.query, undefined)
    await history(h, 'back'); assert.equal(h.page.url(), facetUrl); assert.equal(resultReceipts(h).at(-1).endpoint, '/api/v1/observations/subject-facets')
    const count = resultReceipts(h).length
    assert.equal(await h.page.getByRole('button', { name: 'Copy link', exact: true }).count(), 0)
    assert.equal(h.page.url(), facetUrl)
    assert.equal(resultReceipts(h).length, count)
  } finally { await h.close() }
})

test('production ordered file context restores Match and A-to-B-to-Back restores read-only selection across tab history', { timeout: 60000 }, async () => {
  const h = await openDiscovery()
  try {
    await navigate(h, tuple('match', 'Cache evidence', null, '', h.sha, null, null, [' src/Main.elm ', 'src/View.elm', 'src/Main.elm']))
    assert.deepEqual(resultReceipts(h).at(-1).payload.paths, ['src/Main.elm', 'src/View.elm'])
    assert.deepEqual(await h.page.locator('.observation-path-heading').allTextContents(), ['src/Main.elm', 'src/View.elm'])
    await h.page.locator('.observation-subject-group').filter({ hasText: 'src/**/*.elm' }).locator('.observation-subject-group-toggle').first().click()
    await h.page.locator('.observation-result[data-observation-id="cache-main"] .observation-card').first().click(); await ready(h)
    assert.equal(await h.page.locator('#observation-edit, #observation-edit-content').count(), 0)
    const aUrl = h.page.url(), appliedSummary = await applied(h.page)
    await h.page.locator('.observation-result[data-observation-id="cache-view"] .observation-card').first().click(); await ready(h)
    assert.match(h.page.url(), /observation=cache-view/)
    await history(h, 'back')
    assert.equal(h.page.url(), aUrl)
    assert.equal(await h.page.locator('.observation-detail-content').textContent(), 'Cache evidence for Main')
    assert.equal(await applied(h.page), appliedSummary)
    await h.page.getByRole('button', { name: 'Projects', exact: true }).click(); await h.idle()
    await history(h, 'back')
    assert.equal(await h.page.locator('.observation-detail-content').textContent(), 'Cache evidence for Main')
    await h.page.evaluate(() => { window.location.hash = '#tab=observations&ov=1&oq=' + encodeURIComponent(JSON.stringify(['flat', '', null, '', '', null, null, []])) })
    await ready(h)
    assert.equal(await h.page.locator('.observation-retained-draft').count(), 0)
  } finally { await h.close() }
})

test('production malformed and oversized links restore atomically with an explicit notice and canonical cleanup', { timeout: 60000 }, async () => {
  const h = await openDiscovery()
  try {
    const bad = ['#tab=observations&observation=cache-main&ov=2&oq=[]', '#tab=observations&ov=1&oq=%ZZ', fragment(tuple('flat')) + '&ov=1', fragment(tuple('flat')) + '&unknown=1', fragment(tuple('match', '', null, '', '', null, null, ['../bad'])), '#tab=observations&ov=1&oq=' + 'a'.repeat(4097)]
    for (const suffix of bad) {
      await h.page.goto(h.origin + '/'); await h.idle()
      const count = resultReceipts(h).length
      await h.page.goto(h.origin + '/workspace/' + h.fixture.workspace.id + suffix); await ready(h)
      assert.equal(resultReceipts(h).length, count + 1)
      assert.equal(resultReceipts(h).at(-1).endpoint, '/api/v1/observations')
      assert.equal(resultReceipts(h).at(-1).params.query, undefined)
      assert.equal(await h.page.locator('.observation-detail').count(), 0)
      assert.match(await h.page.locator('.observation-url-notice').innerText(), /No filters or selection were restored/)
      assert.ok(Buffer.byteLength(h.page.url()) <= 4096)
      assert.match(h.page.url(), /ov=1&oq=/)
    }
  } finally { await h.close() }
})

test('production valid oversized queries remain active, replace bounded markers, and Back or reload explicitly defaults', { timeout: 60000 }, async () => {
  const h = await openDiscovery({ width: 320, height: 800 })
  try {
    await h.start()
    await h.page.locator('.observation-card').first().click(); await ready(h)
    await h.page.getByRole('button', { name: 'Files', exact: true }).click()
    const path = 'src/' + 'é'.repeat(1800) + '.elm'
    await h.page.locator('#observation-match-paths').fill(path)
    const initialHistory = await h.page.evaluate(() => history.length), count = resultReceipts(h).length
    await h.page.getByRole('button', { name: 'Match files', exact: true }).click(); await ready(h)
    const firstMarker = h.page.url()
    assert.equal(resultReceipts(h).length, count + 1); assert.deepEqual(resultReceipts(h).at(-1).payload.paths, [path])
    assert.match(firstMarker, /&ox=/); assert.ok(Buffer.byteLength(firstMarker) <= 4096)
    assert.equal(await h.page.getByRole('button', { name: 'Copy link', exact: true }).count(), 0)
    assert.match(await h.page.locator('.observation-url-notice').innerText(), /not restored by Back, reload/)
    assert.equal(await h.page.evaluate(() => history.length), initialHistory)
    await h.page.locator('#observation-delete').click()
    await h.page.getByRole('dialog', { name: 'Delete observation permanently?' }).waitFor()
    assert.equal(await h.page.locator('.observation-url-notice').getAttribute('inert'), '')
    assert.equal(await h.page.locator('.observation-url-notice').getAttribute('aria-hidden'), 'true')
    assert.equal(await h.page.locator('.observation-curation-background').getAttribute('inert'), '')
    await h.page.locator('#observation-delete-cancel').click()
    await h.page.getByRole('dialog', { name: 'Delete observation permanently?' }).waitFor({ state: 'hidden' })
    assert.equal(await h.page.locator('.observation-url-notice').getAttribute('inert'), null)
    assert.equal(await h.page.locator('.observation-url-notice').getAttribute('aria-hidden'), null)
    await h.page.locator('#observation-match-paths').fill(path + 'x')
    await h.page.getByRole('button', { name: 'Match files', exact: true }).click(); await ready(h)
    assert.notEqual(h.page.url(), firstMarker); assert.equal(await h.page.evaluate(() => history.length), initialHistory)
    const marker = h.page.url()
    await h.page.locator('#observation-match-paths').fill('src/Main.elm')
    await h.page.getByRole('button', { name: 'Match files', exact: true }).click(); await ready(h)
    assert.equal(await h.page.evaluate(() => history.length), initialHistory + 1)
    await history(h, 'back'); assert.equal(h.page.url(), marker)
    assert.equal(resultReceipts(h).at(-1).endpoint, '/api/v1/observations')
    assert.match(await h.page.locator('.observation-url-notice').innerText(), /not restored by Back, reload/)
    await h.page.reload(); await ready(h)
    assert.equal(resultReceipts(h).at(-1).endpoint, '/api/v1/observations')
    assert.equal(await h.page.locator('#observation-query').inputValue(), '')
    const overflow = await h.page.evaluate(() => document.documentElement.scrollWidth - innerWidth)
    assert.ok(overflow <= 1, String(overflow))
  } finally { await h.close() }
})

test('production restored off-page 404 selection is retired while read denial issues no Observation request', { timeout: 60000 }, async () => {
  const h = await openDiscovery()
  try {
    await h.page.route('**/api/v1/observations/deleted-selection', route => route.fulfill({ status: 404, body: '{}' }))
    await navigate(h, tuple('flat', 'Cache evidence'), 'deleted-selection')
    assert.equal(new URL(h.page.url()).hash.includes('observation='), false)
    assert.equal(await h.page.locator('.observation-detail').count(), 0)
    await h.page.route('**/api/v1/session**', route => route.fulfill({ status: 200, contentType: 'application/json', body: JSON.stringify({ auth_mode: 'local', principal: { actor_type: 'user', actor_id: 'denied', actor_label: 'Denied', authority: 'local', grant_user_id: null }, global_permissions: { create_workspace: false, superadmin: false }, workspace: { workspace_id: h.fixture.workspace.id, role: null, can_read: false, can_edit: false, can_admin: false } }) }))
    await h.page.goto(h.origin + '/'); await h.idle()
    const count = h.receipts.length
    await h.page.goto(h.origin + '/workspace/' + h.fixture.workspace.id + fragment(tuple('match', '', null, '', '', null, null, ['src/Main.elm']), 'cache-main'))
    await h.idle()
    assert.equal(h.receipts.length, count)
    assert.equal(await h.page.locator('#observation-panel').count(), 0)
  } finally { await h.close() }
})


test('production percent-encoded reserved Unicode context survives copy-sized reload and stale prior pages stay inert', { timeout: 60000 }, async () => {
  const search = 'Cache & # +é🙂'
  const h = await openDiscovery(undefined, values => values.map(value => ({ ...value, content: search + ' ' + value.content })))
  let release, completed
  const completion = new Promise(resolve => { completed = resolve })
  try {
    await navigate(h, tuple('flat', search, 'file', 'src/Main.elm', h.sha))
    assert.equal(resultReceipts(h).at(-1).params.query, search)
    assert.equal(resultReceipts(h).at(-1).params.subject, 'src/Main.elm')
    assert.deepEqual(resultReceipts(h).at(-1).response.items.map(value => value.id), ['cache-main'])
    await h.page.reload(); await ready(h)
    assert.equal(resultReceipts(h).at(-1).params.query, search)
    let arrived
    const arrival = new Promise(resolve => { arrived = resolve }), held = new Promise(resolve => { release = resolve })
    await h.page.route('**/api/v1/observations?**', async route => {
      const url = new URL(route.request().url())
      if (url.searchParams.get('query') !== 'held-query') return route.fallback()
      arrived(); await held
      await route.fulfill({ status: 200, contentType: 'application/json', body: JSON.stringify({ items: [h.fixture.observations.find(value => value.id === 'other-js')], has_more: false }) }); completed()
    })
    await h.page.locator('#observation-query').fill('held-query'); await h.page.locator('#observation-query').press('Enter')
    await h.bounded(arrival, 5000, 'Old applied page admitted')
    await h.page.evaluate(hash => { location.hash = hash }, fragment(tuple('flat', search, 'file', 'src/Main.elm', h.sha)))
    await ready(h)
    release(); await h.bounded(completion, 5000, 'Held old page completion'); await ready(h)
    assert.equal(await h.page.locator('.observation-card').count(), 1)
    assert.match(await h.page.locator('.observation-result').innerText(), /Cache evidence for Main/)
    assert.equal(await h.page.locator('#observation-query').inputValue(), search)
  } finally { release?.(); await h.close() }
})

for (const mode of ['exact', 'match']) test(`production ${mode} public URL context survives401 and fresh login while the private draft retires`, { timeout: 60000 }, async () => {
  const h = await openDiscovery()
  let unauthorized = false
  try {
    await h.page.route('**/api/v1/observations**', async route => {
      if (unauthorized) {
        unauthorized = false
        await route.fulfill({ status: 401, contentType: 'application/json', body: '{}' })
      } else await route.fallback()
    })
    const query = mode === 'exact'
      ? tuple('exact', 'Cache evidence', 'file', 'unapplied-to-locked-facet', h.sha, 'glob', 'src/**/*.elm')
      : tuple('match', 'Cache evidence', null, '', h.sha, null, null, ['src/Main.elm', 'src/View.elm'])
    await navigate(h, query, 'cache-main')
    const publicUrl = h.page.url()
    assert.equal(await h.page.locator('#observation-edit, #observation-edit-content').count(), 0)
    unauthorized = true
    await h.page.getByRole('button', { name: 'Apply filters', exact: true }).click()
    await h.page.waitForFunction(() => !document.getElementById('observation-panel'), null, { timeout: 5000 })
    assert.equal(h.page.url(), publicUrl)
    await h.page.evaluate(() => { window.HMEM_CONFIG.authToken = 'public-controlled-fixture-login'; window.dispatchEvent(new Event('focus')) })
    await ready(h)
    assert.equal(h.page.url(), publicUrl)
    assert.equal(JSON.parse(await h.page.locator('#observation-panel').getAttribute('data-observation-context')).selectedId, 'cache-main')
    assert.equal(await h.page.locator('#observation-edit-content').count(), 0)
    const result = resultReceipts(h).filter(receipt => receipt.endpoint === (mode === 'match' ? '/api/v1/observations/match' : '/api/v1/observations')).at(-1)
    if (mode === 'match') assert.deepEqual(result.payload.paths, query[7])
    else { assert.equal(result.params.subject_kind, 'glob'); assert.equal(result.params.subject, 'src/**/*.elm') }
    assert.equal(mode === 'match' ? result.payload.query : result.params.query, 'Cache evidence')
    assert.equal(mode === 'match' ? result.payload.git_sha : result.params.git_sha, h.sha)
    assert.equal(await h.page.locator('.observation-detail-content').textContent(), 'Cache evidence for Main')
  } finally { await h.close() }
})

test('production populated unified-search Observation activation pushes B and Back restores A with its read-only content', { timeout: 60000 }, async () => {
  const h = await openDiscovery()
  try {
    await h.page.route('**/api/v1/search', async route => {
      assert.equal(route.request().method(), 'POST')
      const payload = route.request().postDataJSON()
      assert.equal(payload.workspace_id, h.fixture.workspace.id)
      assert.ok(payload.entity_types.includes('observation'))
      const requested = payload.query
      const observation = h.fixture.observations.find(value => value.id === (requested.includes('Main') ? 'cache-main' : 'cache-view'))
      await route.fulfill({ status: 200, contentType: 'application/json', body: JSON.stringify({ projects: [], tasks: [], observations: [{ id: observation.id, workspace_id: observation.workspace_id, subject_kind: observation.subject_kind, subject: observation.subject, git_sha: observation.git_sha, content_preview: observation.content, updated_at: observation.updated_at }] }) })
    })
    await navigate(h, tuple('flat', 'Cache evidence'))
    await h.page.locator('.observation-result[data-observation-id="cache-main"] .observation-card').click(); await ready(h)
    assert.equal(await h.page.locator('#observation-edit, #observation-edit-content').count(), 0)
    const aUrl = h.page.url()
    await h.page.locator('.search-input').fill('View'); await h.page.locator('.search-input').press('Enter')
    await h.page.locator('#search-result-cache-view .search-result-action').click(); await ready(h)
    assert.match(h.page.url(), /observation=cache-view/)
    await history(h, 'back'); assert.equal(h.page.url(), aUrl)
    assert.equal(await h.page.locator('.observation-detail-content').textContent(), 'Cache evidence for Main')
    await h.page.getByRole('button', { name: 'Projects', exact: true }).click(); await h.idle()
    const projectsUrl = h.page.url()
    await h.page.locator('.search-input').fill('Main'); await h.page.locator('.search-input').press('Enter')
    await h.page.locator('#search-result-cache-main .search-result-action').click(); await ready(h)
    assert.equal(await h.page.locator('.observation-detail-content').textContent(), 'Cache evidence for Main')
    await h.page.evaluate(() => history.back())
    await h.page.waitForFunction(() => !document.getElementById('observation-panel'), null, { timeout: 5000 }); await h.idle()
    assert.equal(h.page.url(), projectsUrl)
    assert.equal(await h.page.locator('#observation-panel').count(), 0)
    assert.equal(await h.page.locator('.observation-retained-draft').count(), 0)
  } finally { await h.close() }
})
