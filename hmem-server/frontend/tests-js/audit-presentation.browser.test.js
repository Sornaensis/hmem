import assert from 'node:assert/strict'
import test from 'node:test'
import { mkdir, writeFile } from 'node:fs/promises'
import { resolve } from 'node:path'
import { openSessionExplorer } from './support/sessions-ux-fixture.mjs'

const scratch = resolve('../../.scratch/audit-ui-b2a5acef')
const palette = { '--bg': '#f1f5f9', '--bg-surface': '#ffffff', '--bg-elevated': '#e2e8f0', '--text': '#0f172a', '--text-muted': '#475569', '--border': '#94a3b8', '--accent': '#4f46e5', '--accent-hover': '#4338ca' }
const settle = h => h.page.evaluate(() => new Promise(resolve => requestAnimationFrame(() => requestAnimationFrame(resolve))))
async function snapshot(h, name) {
  if (!process.env.HMEM_AUDIT_SCREENSHOTS) return
  assert.equal(resolve(process.env.HMEM_AUDIT_SCREENSHOTS), scratch, 'Screenshots have one explicitly owned scratch root')
  await mkdir(scratch, { recursive: true })
  const pixels = await h.page.screenshot({ fullPage: false })
  assert.ok(pixels.length < 2 * 1024 ** 2)
  await writeFile(resolve(scratch, name + '.png'), pixels)
}
async function setup() {
  const h = await openSessionExplorer(), f = h.fixture
  const entries = Array.from({ length: 65 }, (_, index) => ({ id: 'visual-' + index, workspace_id: f.workspace.id, entity_type: 'project', entity_id: f.projects[0].id, action: 'update', old_values: { name: 'Earlier project', description: 'Earlier description' }, new_values: { name: index === 0 ? 'A long audit target ' + 'long-name-without-spaces'.repeat(8) : 'Change ' + index, description: 'Literal Markdown and JSON remain inspectable.\n' + 'long_unbroken_recorded_value_'.repeat(500), payload: { nested: { recorded: 'unbroken_json_value_'.repeat(80) }, state: ['a', 'b', 'c'] } }, changed_at: '2026-10-15T10:00:00Z', request_id: null, actor_type: 'user', actor_id: 'fixture-user', actor_label: index ? 'Fixture User' : 'An attributed actor with a long display name' }))
  await h.page.route('**/api/v1/session?*', route => { const url = new URL(route.request().url()); return route.fulfill({ json: { auth_mode: 'local', principal: { actor_type: 'user', actor_id: 'fixture-user', actor_label: 'Fixture User', authority: 'local_superadmin', grant_user_id: null }, global_permissions: { create_workspace: false, superadmin: true }, workspace: { workspace_id: url.searchParams.get('workspace_id') || f.workspace.id, role: 'owner', can_read: true, can_edit: true, can_admin: true } } }) })
  await h.page.route('**/api/v1/session', route => route.fulfill({ json: { auth_mode: 'local', principal: { actor_type: 'user', actor_id: 'fixture-user', actor_label: 'Fixture User', authority: 'local_superadmin', grant_user_id: null }, global_permissions: { create_workspace: false, superadmin: true }, workspace: null } }))
  await h.page.route('**/api/v1/audit?*', route => { const url = new URL(route.request().url()), offset = Number(url.searchParams.get('offset') || 0); return route.fulfill({ json: { items: entries.slice(offset, offset + 50), has_more: entries.length > offset + 50 } }) })
  await h.page.route('**/api/v1/change-stream/resync', route => route.request().postDataJSON().scope.scope === 'global' ? route.fulfill({ json: { snapshot_profile: 'full_v1', items: [{ schema_version: 1, kind: 'workspace', data: f.workspace }], has_more: false, resume_token: 'visual-global' } }) : route.fallback())
  return h
}
async function geometry(h) {
  await settle(h)
  return h.page.evaluate(() => {
    const root = document.querySelector('.audit-log-view'), filters = root.querySelector('.audit-log-filters'), detail = root.querySelector('.audit-entry-detail, .audit-ledger-empty'), scroll = document.getElementById('main-content-scroll'), rows = root.querySelector('.audit-ledger-rows')
    const box = node => { const rect = node.getBoundingClientRect(); return { x: rect.x, y: rect.y, width: rect.width, height: rect.height, right: rect.right, bottom: rect.bottom } }
    return { root: box(root), filters: box(filters), detail: box(detail), scroll: box(scroll), rows: box(rows), cssFilterHeight: Number.parseFloat(root.style.getPropertyValue('--audit-filter-height')), logicalFilterHeight: filters.offsetHeight, detailPosition: getComputedStyle(detail).position, filtersPosition: getComputedStyle(filters).position, columns: getComputedStyle(root.querySelector('.audit-ledger')).gridTemplateColumns, overflow: document.documentElement.scrollWidth > innerWidth || scroll.scrollWidth > scroll.clientWidth, rootFont: Number.parseFloat(getComputedStyle(document.documentElement).fontSize) }
  })
}

test('Audit cards and responsive sticky panels remain usable across scope, width, theme, wrapping and zoom', { timeout: 60000 }, async () => {
  const h = await setup()
  try {
    await h.page.setViewportSize({ width: 1640, height: 1000 })
    await h.start(); await h.idle(); await h.page.getByRole('button', { name: 'Audit', exact: true }).click()
    await h.page.locator('#audit-event-visual-0').waitFor()
    assert.equal(await h.page.getByText('Workspace audit', { exact: true }).count(), 0)
    assert.equal(await h.page.getByText('Recent changes recorded for this workspace.', { exact: true }).count(), 0)
    const ordinary = h.page.locator('#audit-event-visual-1 .audit-entry-row')
    const surface = await ordinary.evaluate(node => { const css = getComputedStyle(node); return { background: css.backgroundColor, border: css.borderTopWidth, color: css.color } })
    assert.notEqual(surface.background, 'rgba(0, 0, 0, 0)'); assert.equal(surface.border, '1px')
    await ordinary.hover(); const hover = await ordinary.evaluate(node => getComputedStyle(node).backgroundColor); assert.notEqual(hover, surface.background)
    await ordinary.locator('.audit-expand-icon').focus(); assert.equal(await ordinary.evaluate(node => getComputedStyle(node).outlineStyle), 'solid')
    await h.page.locator('#audit-event-visual-0 .audit-expand-icon').click()
    await h.page.getByRole('complementary', { name: 'Selected audit event', exact: true }).waitFor()
    const selected = await h.page.locator('#audit-event-visual-0 .audit-entry-row').evaluate(node => getComputedStyle(node).backgroundColor)
    assert.notEqual(selected, surface.background)
    let layout = await geometry(h)
    assert.ok(Math.abs(layout.detail.width - 540) < 2, JSON.stringify(layout))
    assert.equal(layout.overflow, false)
    await snapshot(h, 'audit-workspace-wide')
    await h.page.locator('#main-content-scroll').evaluate(node => { node.scrollTop = 550 })
    layout = await geometry(h)
    assert.ok(Math.abs(layout.filters.y - layout.scroll.y) < 2, JSON.stringify(layout))
    assert.ok(layout.detail.y >= layout.filters.bottom + 10, JSON.stringify(layout))
    assert.ok(layout.detail.bottom <= layout.scroll.bottom + 1, JSON.stringify(layout))
    const measured = [{ viewport: 1640, detail: layout.detail.width, filters: layout.logicalFilterHeight, sticky: layout.detailPosition }]
    for (const width of [1180, 1024, 850]) {
      await h.page.setViewportSize({ width, height: 1000 }); layout = await geometry(h)
      assert.equal(layout.overflow, false, JSON.stringify(layout))
      assert.ok(layout.detail.width >= 270, JSON.stringify(layout))
      assert.equal(layout.cssFilterHeight, layout.logicalFilterHeight)
      measured.push({ viewport: width, detail: layout.detail.width, filters: layout.logicalFilterHeight, sticky: layout.detailPosition })
      if (layout.detailPosition === 'sticky') assert.ok(layout.detail.y >= layout.filters.bottom + 10, JSON.stringify(layout))
    }
    await h.page.setViewportSize({ width: 1640, height: 1000 })
    await h.page.evaluate(() => { document.documentElement.style.zoom = '1.25'; document.querySelector('.audit-log-filters').style.maxWidth = '470px' })
    layout = await geometry(h)
    assert.ok(layout.logicalFilterHeight > 90, JSON.stringify(layout))
    assert.equal(layout.cssFilterHeight, layout.logicalFilterHeight)
    assert.equal(layout.overflow, false)
    assert.equal(layout.detailPosition, 'sticky', JSON.stringify(layout))
    assert.ok(layout.detail.y >= layout.filters.bottom + 10, JSON.stringify(layout))
    assert.ok(layout.detail.bottom <= layout.scroll.bottom + 1, JSON.stringify(layout))
    measured.push({ viewport: 1640, zoom: 1.25, detail: layout.detail.width, filters: layout.logicalFilterHeight, sticky: layout.detailPosition })
    await snapshot(h, 'audit-intermediate-wrap')
    await h.page.evaluate(() => { document.documentElement.style.zoom = ''; document.querySelector('.audit-log-filters').style.maxWidth = '' })
    await h.page.setViewportSize({ width: 1640, height: 1000 })
    await h.page.getByRole('link', { name: 'Audit Log', exact: true }).click(); await h.page.locator('.page.audit-log-view #audit-event-visual-0').waitFor()
    await h.page.locator('#audit-event-visual-0 .audit-expand-icon').click()
    await h.page.evaluate(colors => { for (const [key, value] of Object.entries(colors)) document.documentElement.style.setProperty(key, value) }, palette)
    await h.page.waitForFunction(() => getComputedStyle(document.querySelector('#audit-event-visual-1 .audit-entry-row')).backgroundColor === 'rgb(255, 255, 255)')
    await h.page.locator('#main-content-scroll').evaluate(node => { node.scrollTop = 400 })
    layout = await geometry(h); assert.equal(layout.overflow, false); assert.ok(layout.detail.y >= layout.filters.bottom + 10)
    assert.equal(await h.page.locator('#audit-event-visual-1 .audit-entry-row').evaluate(node => getComputedStyle(node).backgroundColor), 'rgb(255, 255, 255)', await h.page.locator('#audit-event-visual-1 .audit-entry-row').evaluate(node => JSON.stringify({ surface: getComputedStyle(node).getPropertyValue('--bg-surface'), root: document.documentElement.style.getPropertyValue('--bg-surface'), elevated: getComputedStyle(node).getPropertyValue('--bg-elevated'), classes: node.parentElement.className, hovered: node.matches(':hover') })))
    await snapshot(h, 'audit-global-light')
    await h.page.setViewportSize({ width: 360, height: 900 }); layout = await geometry(h)
    assert.equal(layout.filtersPosition, 'static'); assert.equal(layout.detailPosition, 'static'); assert.equal(layout.overflow, false, JSON.stringify(layout))
    await h.page.locator('#main-content-scroll').evaluate(node => { node.scrollTop = 0 }); await settle(h)
    await snapshot(h, 'audit-mobile')
    await h.page.locator('.audit-entry-detail .audit-view-diff').first().click(); await h.page.locator('#audit-diff-dialog').waitFor()
    assert.equal(await h.page.evaluate(() => document.documentElement.scrollWidth <= innerWidth), true)
    assert.equal(await h.page.locator('#audit-diff-dialog').evaluate(node => node.open && node.getBoundingClientRect().width <= innerWidth), true)
    await snapshot(h, 'audit-diff-mobile')
    await h.page.keyboard.press('Escape'); await h.page.locator('#audit-diff-dialog').waitFor({ state: 'detached' })
    assert.deepEqual(h.errors, [])
    console.info('Audit measured geometry: ' + JSON.stringify(measured))
  } finally { await h.close() }
})
