import assert from 'node:assert/strict'
import test from 'node:test'
import { openDiscovery } from './observation-discovery-fixture.mjs'
import { scanObservationRows } from './observation-viewport-fixture.mjs'

test('production cached incremental failure retries the exact applied offset and changed-query Apply replaces page zero', { timeout: 60000 }, async () => {
  const h = await openDiscovery()
  try {
    const observed = []
    let failNext = true
    await h.page.route('**/api/v1/observations?**', async route => {
      const params = Object.fromEntries(new URL(route.request().url()).searchParams)
      observed.push(params)
      if (params.offset === '50' && failNext) {
        failNext = false
        await route.fulfill({ status: 503, contentType: 'application/json', body: JSON.stringify({ error: 'Controlled incremental failure' }) })
      } else await route.fallback()
    })
    await h.start()
    assert.equal((await scanObservationRows(h.page)).cards.size, 50)
    await h.page.getByRole('button', { name: 'Load more', exact: true }).click()
    await h.page.getByRole('button', { name: 'Retry results', exact: true }).waitFor({ timeout: 5000 })
    assert.equal((await scanObservationRows(h.page)).cards.size, 50)
    assert.match(await h.page.locator('.observation-state-error').innerText(), /loaded results|cached|Retry results/i)
    await h.page.locator('#observation-query').fill('unapplied private control')
    await h.page.getByRole('button', { name: 'Retry results', exact: true }).click(); await h.idle()
    assert.equal((await scanObservationRows(h.page)).cards.size, 64)
    assert.deepEqual(observed.filter(value => value.offset === '50').map(value => ({ query: value.query || '', offset: value.offset, limit: value.limit })), [
      { query: '', offset: '50', limit: '50' }, { query: '', offset: '50', limit: '50' }
    ])
    await h.page.locator('#observation-query').fill('Cache evidence')
    await h.page.getByRole('button', { name: 'Apply filters', exact: true }).click(); await h.idle()
    assert.equal(await h.page.locator('.observation-card').count(), 2)
    assert.equal(observed.at(-1).offset, '0')
    assert.equal(observed.at(-1).query, 'Cache evidence')
  } finally { await h.close() }
})

test('production failed off-page detail and retained return offer reachable recovery without losing the draft', { timeout: 60000 }, async () => {
  const h = await openDiscovery()
  try {
    let failures = 1, detailRequests = 0
    await h.page.route('**/api/v1/observations/cache-main', async route => {
      detailRequests++
      if (failures > 0) {
        failures--
        await route.fulfill({ status: 503, contentType: 'application/json', body: JSON.stringify({ error: 'Controlled detail failure' }) })
      } else await route.fallback()
    })
    const tuple = ['flat', 'Documentation', null, '', '', null, null, []]
    await h.page.goto(h.origin + '/workspace/' + h.fixture.workspace.id + '#tab=observations&observation=cache-main&ov=1&oq=' + encodeURIComponent(JSON.stringify(tuple)))
    await h.page.getByRole('button', { name: 'Retry detail', exact: true }).waitFor({ timeout: 5000 })
    assert.equal(await h.page.locator('.observation-detail-content').count(), 0)
    await h.page.getByRole('button', { name: 'Retry detail', exact: true }).click(); await h.idle()
    assert.equal(detailRequests, 2)
    await h.page.locator('#observation-edit').click()
    await h.page.locator('#observation-edit-content').fill('Retained recovery draft')
    await h.page.getByRole('button', { name: 'Projects', exact: true }).click(); await h.idle()
    failures = 1
    await h.page.getByRole('button', { name: 'Return to draft', exact: true }).click()
    await h.page.getByRole('button', { name: 'Retry detail', exact: true }).waitFor({ timeout: 5000 })
    assert.equal(await h.page.locator('#observation-edit-content').inputValue(), 'Retained recovery draft')
    assert.equal(await h.page.getByRole('button', { name: 'Save content', exact: true }).isEnabled(), true)
    assert.equal(await h.page.getByRole('button', { name: 'Cancel', exact: true }).isEnabled(), true)
    await h.page.getByRole('button', { name: 'Retry detail', exact: true }).click(); await h.idle()
    assert.equal(await h.page.locator('#observation-edit-content').inputValue(), 'Retained recovery draft')
    assert.equal(detailRequests, 4)
  } finally { await h.close() }
})

test('production live read revocation retires private curation controls and authorized reload restores only the public link', { timeout: 60000 }, async () => {
  const h = await openDiscovery()
  try {
    let canRead = true
    await h.page.route('**/api/v1/session?**', async route => {
      const body = { auth_mode: 'local', principal: { actor_type: 'user', actor_id: 'fixture-user', actor_label: 'Fixture User', authority: 'local', grant_user_id: null }, global_permissions: { create_workspace: false, superadmin: false },
        workspace: { workspace_id: h.fixture.workspace.id, role: canRead ? 'owner' : null, can_read: canRead, can_edit: canRead, can_admin: canRead } }
      await route.fulfill({ status: 200, contentType: 'application/json', body: JSON.stringify(body) })
    })
    await h.page.goto(h.origin + '/workspace/' + h.fixture.workspace.id + '#tab=observations&observation=cache-main')
    await h.idle()
    await h.page.locator('#observation-edit').click()
    await h.page.locator('#observation-edit-content').fill('Retired permission draft')
    const publicUrl = h.page.url()
    canRead = false
    await h.page.evaluate(workspace => window.pushHierarchyFrames([{ schema_version: 1, type: 'access_revoked', workspace_id: workspace }]), h.fixture.workspace.id)
    await h.page.waitForFunction(() => !document.getElementById('observation-edit-content'), null, { timeout: 5000 })
    assert.equal(await h.page.locator('.observation-retained-draft').count(), 0)
    assert.equal(await h.page.locator('#observation-delete').count(), 0)
    assert.equal(h.page.url(), publicUrl)
    canRead = true
    // The revoked workspace socket is disconnected. This ordinary user has
    // no global stream; a fresh document obtains the new grant from session HTTP.
    await h.page.reload()
    await h.page.locator('#observation-edit').waitFor({ timeout: 5000 }); await h.idle()
    assert.equal(await h.page.locator('#observation-edit-content').count(), 0)
    await h.page.locator('#observation-edit').click()
    assert.equal(await h.page.locator('#observation-edit-content').inputValue(), 'Cache evidence for Main')
  } finally { await h.close() }
})
