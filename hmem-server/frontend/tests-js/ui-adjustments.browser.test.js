import assert from 'node:assert/strict'
import test from 'node:test'
import { hierarchyFixture, openHierarchy } from './hierarchy-fixture.mjs'

function smallFixture() {
  const fixture = hierarchyFixture()
  fixture.projects = [fixture.projects[0], { ...fixture.projects[1], id: 'empty-project', name: 'Empty project' }]
  fixture.tasks = [{ ...fixture.tasks[0], id: 'working-task', status: 'in_progress' }]
  return fixture
}

async function workspaceFixture() {
  const first = smallFixture()
  const second = { ...first, workspace: { ...first.workspace, id: 'workspace-b', name: 'Workspace B' }, projects: [], tasks: [] }
  first.workspace.name = 'Workspace A'
  const h = await openHierarchy(first, [second])
  const workspaces = [first.workspace, second.workspace]
  const group = { id: 'group-a', name: 'Workspace group', description: null, created_at: first.workspace.created_at, updated_at: first.workspace.updated_at }
  const requests = []
  let deleteStatus = 200, holdDelete = null, holdSession = null, holdCatalogue = null
  let catalogue = workspaces, superadmin = true
  await h.page.route('**/api/v1/**', async route => {
    const request = route.request(), url = new URL(request.url()), path = url.pathname
    let value
    if (path === '/api/v1/session') {
      const id = url.searchParams.get('workspace_id') || first.workspace.id
      value = { auth_mode: 'local', principal: { actor_type: 'user', actor_id: 'local-owner', actor_label: 'Local owner', authority: 'local', grant_user_id: null }, global_permissions: { create_workspace: false, superadmin }, workspace: { workspace_id: id, role: 'admin', can_read: true, can_edit: true, can_admin: true } }
      if (id === second.workspace.id && holdSession) await holdSession
    } else if (path === '/api/v1/change-stream/ticket' && request.postDataJSON().scope.scope === 'global') {
      value = { ticket: 'global-ticket', expires_at: '2099-01-01T00:00:00Z' }
    } else if (path === '/api/v1/change-stream/resync' && request.postDataJSON().scope.scope === 'global') {
      value = { snapshot_profile: 'full_v1', items: [...workspaces.map(data => ({ schema_version: 1, kind: 'workspace', data })), { schema_version: 1, kind: 'workspace_group', data: group }], has_more: false, resume_token: 'global-shell' }
    } else if (path === '/api/v1/workspaces') {
      if (holdCatalogue) await holdCatalogue
      value = { items: catalogue, has_more: false }
    } else if (path === '/api/v1/groups') value = { items: [group], has_more: false }
    else if (path === '/api/v1/groups/group-a/members') value = [first.workspace.id]
    else if (path === '/api/v1/workspaces/' + first.workspace.id && request.method() === 'DELETE') {
      requests.push({ method: 'DELETE', path })
      if (holdDelete) await holdDelete
      await route.fulfill({ status: deleteStatus, contentType: 'application/json', body: deleteStatus === 200 ? '' : JSON.stringify({ error: 'Controlled deletion failure' }) })
      return
    } else return route.fallback()
    requests.push({ method: request.method(), path })
    await route.fulfill({ status: 200, contentType: 'application/json', body: JSON.stringify(value) })
  })
  return { ...h, first, second, requests, setSuperadmin(value) { superadmin = value }, setCatalogue(values) { catalogue = values }, renameGroup(name) { group.name = name }, holdSession() { let release; holdSession = new Promise(resolve => { release = resolve }); return release }, holdCatalogue() { let release; holdCatalogue = new Promise(resolve => { release = resolve }); return release }, failDeletion() { deleteStatus = 500 }, allowDeletion() { deleteStatus = 200 }, holdDeletion() { let release; holdDelete = new Promise(resolve => { release = resolve }); return release } }
}

test('compact UUID controls retain keyboard copying and activity accent is independent of task filters', { timeout: 60000 }, async () => {
  const h = await openHierarchy(smallFixture())
  try {
    await h.start(); await h.idle()
    const root = h.page.locator('#entity-root-project')
    assert.equal(await root.locator('.card-project-in-progress').count(), 1)
    assert.equal(await root.getByText('In progress', { exact: true }).count(), 0)
    const copy = root.locator('.card-id-copy')
    const geometry = await copy.evaluate(element => ({ font: parseFloat(getComputedStyle(element).fontSize), visible: element.getBoundingClientRect().width > 0, label: element.getAttribute('aria-label') }))
    assert.ok(geometry.font <= 11.1 && geometry.visible && geometry.label.includes('root-project'), JSON.stringify(geometry))
    await h.page.evaluate(() => { window.fixtureCopies = []; Object.defineProperty(navigator, 'clipboard', { configurable: true, value: { writeText: async value => window.fixtureCopies.push(value) } }) })
    await copy.focus(); await h.page.keyboard.press('Enter'); await h.page.keyboard.press('Space')
    await h.page.waitForFunction(() => window.fixtureCopies.length === 2)
    assert.deepEqual(await h.page.evaluate(() => window.fixtureCopies), ['root-project', 'root-project'])
    await h.page.locator('.filter-bar').getByRole('button', { name: 'Todo', exact: true }).click(); await h.idle()
    assert.equal(await root.locator('.card-project-in-progress').count(), 1)
    const checkbox = h.page.getByRole('checkbox', { name: 'Show empty projects' })
    assert.equal(await checkbox.isChecked(), true)
    await checkbox.uncheck(); await h.idle()
    assert.equal(await h.page.locator('.card-project').count(), 0)
    await h.page.locator('.filter-bar').getByRole('button', { name: 'Todo', exact: true }).click(); await h.idle()
    assert.equal(await h.page.locator('#entity-root-project .card-project-in-progress').count(), 1)
    assert.equal(await h.page.locator('#entity-empty-project').count(), 0)
    await h.page.reload(); await h.page.locator('#entity-root-project').waitFor(); await h.idle()
    assert.equal(await checkbox.isChecked(), false)
    if (process.env.HMEM_UI_QA_SCREENSHOT) await h.page.screenshot({ path: process.env.HMEM_UI_QA_SCREENSHOT, fullPage: true })
  } finally { await h.close() }
})

test('workspace switch preserves global group disclosure, sidebar nodes and catalogue requests', { timeout: 60000 }, async () => {
  const h = await workspaceFixture()
  try {
    await h.start(); await h.idle()
    const group = h.page.locator('.sidebar-group-title').filter({ hasText: 'Workspace group' })
    await group.click()
    await h.page.getByRole('link', { name: /Workspace A/ }).waitFor({ state: 'detached' })
    await h.page.evaluate(() => { const sidebar = document.querySelector('.sidebar'); window.originalSidebar = sidebar; window.sidebarReplacements = 0; window.sidebarMutations = []; window.sidebarObserver = new MutationObserver(records => { window.sidebarReplacements += records.filter(record => record.type === 'childList').length; window.sidebarMutations.push(...records.filter(record => record.type === 'childList').map(record => ({ target: record.target.className, added: [...record.addedNodes].map(node => node.textContent?.slice(0,100)), removed: [...record.removedNodes].map(node => node.textContent?.slice(0,100)) }))) }); window.sidebarObserver.observe(sidebar, { subtree: true, childList: true }) })
    const counts = h.requests.filter(request => ['/api/v1/workspaces', '/api/v1/groups', '/api/v1/groups/group-a/members', '/api/v1/change-stream/resync'].includes(request.path)).length
    await h.page.getByRole('link', { name: /Workspace B/ }).click()
    await h.page.locator('.workspace-header-title').filter({ hasText: 'Workspace B' }).waitFor(); await h.idle()
    assert.equal(await h.page.getByRole('link', { name: /Workspace A/ }).count(), 0)
    assert.equal(await h.page.evaluate(() => document.querySelector('.sidebar') === window.originalSidebar && window.sidebarReplacements === 0), true, JSON.stringify(await h.page.evaluate(() => window.sidebarMutations)))
    assert.equal(h.requests.filter(request => ['/api/v1/workspaces', '/api/v1/groups', '/api/v1/groups/group-a/members', '/api/v1/change-stream/resync'].includes(request.path)).length, counts)
    assert.deepEqual(await h.page.evaluate(() => JSON.parse(localStorage.getItem('hmem-workspace-groups')).collapsedGroups), { 'group-a': true })
    await h.page.reload(); await h.page.locator('.workspace-header-title').filter({ hasText: 'Workspace B' }).waitFor(); await h.idle()
    assert.equal(await h.page.getByRole('link', { name: /Workspace A/ }).count(), 0)
  } finally { await h.close() }
})

test('local workspace deletion confirms, prevents repeats, retains failures and preserves a switched selection', { timeout: 60000 }, async () => {
  const h = await workspaceFixture()
  try {
    await h.start(); await h.idle()
    h.failDeletion()
    await h.page.getByRole('button', { name: 'Delete workspace', exact: true }).click()
    const dialog = h.page.getByRole('dialog')
    assert.equal(h.requests.filter(request => request.method === 'DELETE').length, 0)
    await dialog.getByRole('button', { name: 'Delete workspace', exact: true }).click()
    await dialog.getByRole('alert').waitFor()
    assert.equal(await h.page.getByRole('link', { name: /Workspace A/ }).count(), 1)
    h.allowDeletion()
    const release = h.holdDeletion()
    await dialog.getByRole('button', { name: 'Delete workspace', exact: true }).click()
    await dialog.getByRole('button', { name: 'Deleting...', exact: true }).waitFor()
    assert.equal(await dialog.getByRole('button', { name: 'Deleting...', exact: true }).isDisabled(), true)
    assert.equal(await dialog.getByRole('button', { name: 'Cancel', exact: true }).isDisabled(), true)
    await h.page.evaluate(id => { history.pushState(null, '', '/workspace/' + id); window.dispatchEvent(new PopStateEvent('popstate')) }, h.second.workspace.id)
    await h.page.locator('.workspace-header-title').filter({ hasText: 'Workspace B' }).waitFor(); await h.idle()
    release()
    await dialog.waitFor({ state: 'detached' })
    assert.ok(h.page.url().includes('/workspace/' + h.second.workspace.id))
    assert.equal(await h.page.getByRole('link', { name: /Workspace A/ }).count(), 0)
    assert.equal(h.requests.filter(request => request.method === 'DELETE').length, 2)
  } finally { await h.close() }
})

test('successful selected workspace deletion returns home', { timeout: 60000 }, async () => {
  const h = await workspaceFixture()
  try {
    await h.start(); await h.idle()
    await h.page.getByRole('button', { name: 'Delete workspace', exact: true }).click()
    await h.page.getByRole('dialog').getByRole('button', { name: 'Delete workspace', exact: true }).click()
    await h.page.waitForURL(h.origin + '/')
    await h.page.getByRole('link', { name: /Workspace A/ }).waitFor({ state: 'detached' })
    assert.equal(await h.page.getByRole('link', { name: /Workspace A/ }).count(), 0)
  } finally { await h.close() }
})


function globalChange(type, id, invalidations, identity) {
  return { schema_version: 1, type: 'change', event: { schema_version: 1, event_id: identity, scope: 'global', workspace_id: null, occurred_at: '2026-08-30T12:00:00Z', transaction: { id: identity, cause: 'rest', request_id: null }, actor: { type: 'service', id: 'controlled-browser' }, entity: { type, id, action: 'updated' }, invalidations } }
}

test('global structural events and pending catalogue replies survive delayed route admission', { timeout: 60000 }, async () => {
  const h = await workspaceFixture()
  let releaseSession, releaseCatalogue
  try {
    await h.start(); await h.idle()
    const before = h.requests.filter(request => request.path === '/api/v1/change-stream/resync').length
    releaseCatalogue = h.holdCatalogue()
    const extra = { ...h.second.workspace, id: 'workspace-c', name: 'Workspace C' }
    h.setCatalogue([h.first.workspace, h.second.workspace, extra])
    const pendingCatalogue = h.page.waitForRequest(request => new URL(request.url()).pathname === '/api/v1/workspaces')
    await h.page.evaluate(frame => window.pushHierarchyFrames([frame]), globalChange('workspace', extra.id, [{ kind: 'catalogue', target: 'workspace-catalog' }], 'catalogue-before-route'))
    await pendingCatalogue
    releaseSession = h.holdSession()
    await h.page.getByRole('link', { name: /Workspace B/ }).click()
    h.renameGroup('Reorganized group')
    await h.page.evaluate(frame => window.pushHierarchyFrames([frame]), globalChange('workspace_group', 'group-a', [{ kind: 'collection', target: 'workspace-groups' }], 'group-during-route'))
    releaseCatalogue()
    await h.page.getByRole('link', { name: /Workspace C/ }).waitFor()
    await h.page.locator('.sidebar-group-title').filter({ hasText: 'Reorganized group' }).waitFor()
    releaseSession()
    await h.page.locator('.workspace-header-title').filter({ hasText: 'Workspace B' }).waitFor(); await h.idle()
    assert.equal(h.requests.filter(request => request.path === '/api/v1/workspaces').length, 1)
    assert.equal(h.requests.filter(request => request.path === '/api/v1/change-stream/resync').length, before)
    await h.page.getByRole('link', { name: /Workspace A/ }).click()
    await h.page.locator('.workspace-header-title').filter({ hasText: 'Workspace A' }).waitFor(); await h.idle()
    assert.equal(h.requests.filter(request => request.path === '/api/v1/workspaces').length, 1)
    assert.equal(h.requests.filter(request => request.path === '/api/v1/change-stream/resync').length, before)
  } finally { releaseSession?.(); releaseCatalogue?.(); await h.close() }
})


test('global authorization refresh uses route epoch and retires stale route replies', { timeout: 60000 }, async () => {
  const h = await workspaceFixture()
  let release
  try {
    await h.start(); await h.idle()
    await h.page.getByRole('link', { name: /Workspace B/ }).click()
    await h.page.locator('.workspace-header-title').filter({ hasText: 'Workspace B' }).waitFor(); await h.idle()
    release = h.holdSession()
    h.setSuperadmin(false)
    const refresh = h.page.waitForRequest(request => new URL(request.url()).pathname === '/api/v1/session')
    await h.page.evaluate(() => window.pushHierarchyGlobalFrames([{ schema_version: 1, type: 'access_revoked', workspace_id: null }]))
    await refresh
    h.setSuperadmin(true)
    await h.page.getByRole('link', { name: /Workspace A/ }).click()
    await h.page.locator('.workspace-header-title').filter({ hasText: 'Workspace A' }).waitFor(); await h.idle()
    const staleReply = h.page.waitForResponse(response => new URL(response.url()).pathname === '/api/v1/session' && new URL(response.url()).searchParams.get('workspace_id') === h.second.workspace.id)
    release(); await staleReply; await h.idle()
    await h.page.locator('.sidebar-group-title').filter({ hasText: 'Workspace group' }).waitFor()
    h.setSuperadmin(false)
    await h.page.evaluate(() => window.pushHierarchyGlobalFrames([{ schema_version: 1, type: 'access_revoked', workspace_id: null }]))
    await h.page.locator('.sidebar-group-title').waitFor({ state: 'detached' })
    assert.ok(h.page.url().includes('/workspace/' + h.first.workspace.id))
    assert.equal(await h.page.getByRole('button', { name: 'Delete workspace', exact: true }).count(), 1)
  } finally { release?.(); await h.close() }
})
