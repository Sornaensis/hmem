import assert from 'node:assert/strict'
import test from 'node:test'
import { hierarchyFixture, openHierarchy } from './hierarchy-fixture.mjs'

const event = (fixture, id, kind = 'task', action = 'update') => ({
  id, workspace_id: fixture.workspace.id, entity_type: kind, entity_id: id,
  action, old_values: action === 'create' ? null : { title: 'Before ' + id },
  new_values: { title: 'After ' + id, workspace_id: fixture.workspace.id },
  changed_at: '2026-10-15T10:00:00Z', request_id: null,
  actor_type: 'user', actor_id: 'fixture-user', actor_label: 'Fixture User',
})

async function setup() {
  const f = hierarchyFixture()
  f.projects = [f.projects[0]]; f.tasks = []
  const second = { ...f, workspace: { ...f.workspace, id: 'other-workspace', name: 'Other workspace' }, projects: [], tasks: [] }
  const h = await openHierarchy(f, [second])
  const events = [
    ...Array.from({ length: 65 }, (_, i) => event(f, 'task-' + i)),
    event(f, 'session-a', 'agent_session', 'create'), event(f, 'session-b', 'agent_session', 'update'),
    event(f, 'link-a', 'task_session', 'create'), event(f, 'link-b', 'task_session', 'create'),
  ]
  const reads = [], controls = []
  let authorized = true
  await h.page.route('**/api/v1/**', async route => {
    const request = route.request(), url = new URL(request.url()), path = url.pathname
    let value
    if (path === '/api/v1/session') value = { auth_mode: 'local', principal: { actor_type: 'user', actor_id: 'fixture-user', actor_label: 'Fixture User', authority: 'local_superadmin', grant_user_id: null }, global_permissions: { create_workspace: false, superadmin: authorized }, workspace: url.searchParams.has('workspace_id') ? { workspace_id: url.searchParams.get('workspace_id'), role: authorized ? 'owner' : null, can_read: authorized, can_edit: authorized, can_admin: authorized } : null }
    else if (path === '/api/v1/audit') {
      const query = Object.fromEntries(url.searchParams); reads.push(query)
      const control = controls.find(control => !control.used && control.match(query))
      if (control) {
        control.used = true; control.arrive()
        if (control.hold) await control.wait
        return route.fulfill({ status: control.status || 200, contentType: 'application/json', body: JSON.stringify(control.response || { items: [], has_more: false }) })
      }
      const filtered = events.filter(entry => (!query.workspace_id || entry.workspace_id === query.workspace_id) && (!query.entity_type || entry.entity_type === query.entity_type)
        && (!query.action || entry.action === query.action)
        && (!query.since || entry.changed_at >= query.since) && (!query.until || entry.changed_at <= query.until))
      const offset = Number(query.offset || 0), limit = Number(query.limit || 50)
      value = { items: filtered.slice(offset, offset + limit), has_more: filtered.length > offset + limit }
    } else if (path === '/api/v1/groups') value = { items: [], has_more: false }
    else if (path.endsWith('/change-stream/ticket') && request.postDataJSON().scope.scope === 'global') value = { ticket: 'global-ticket', expires_at: '2099-01-01T00:00:00Z' }
    else if (path.endsWith('/change-stream/resync') && request.postDataJSON().scope.scope === 'global') value = { snapshot_profile: 'full_v1', items: [f, second].map(item => ({ schema_version: 1, kind: 'workspace', data: item.workspace })), has_more: false, resume_token: 'global-shell' }
    else return route.fallback()
    await route.fulfill({ status: 200, contentType: 'application/json', body: JSON.stringify(value) })
  })
  return { ...h, f, second, reads, events, revoke() { authorized = false }, control(match, options = {}) {
    let release, arrive
    const wait = new Promise(resolve => { release = resolve }), arrived = new Promise(resolve => { arrive = resolve })
    const control = { match, ...options, used: false, wait, arrived, release, arrive }; controls.push(control); return control
  }, async close() { controls.forEach(control => control.release()); await h.close() } }
}

async function settled(h) {
  await h.page.waitForFunction(() => !document.querySelector('.audit-log-view .loading-indicator'))
}
async function callbacksSettled(h, response) {
  await response.finished()
  await h.page.evaluate(() => new Promise(resolve => requestAnimationFrame(() => requestAnimationFrame(resolve))))
}
async function change(h, action, match) {
  const response = h.page.waitForResponse(response => { const url = new URL(response.url()); return url.pathname === '/api/v1/audit' && match(Object.fromEntries(url.searchParams)) })
  await action(); await response; await settled(h)
}
async function workspaceAudit(h) {
  await h.start(); await h.idle()
  await change(h, () => h.page.getByRole('button', { name: 'Audit', exact: true }).click(), query => query.workspace_id === h.f.workspace.id)
}

test('workspace and global audit controls automatically filter, reset and paginate the current query', { timeout: 45000 }, async () => {
  const h = await setup()
  try {
    await workspaceAudit(h)
    for (const global of [false, true]) {
      if (global) await change(h, () => h.page.getByRole('link', { name: 'Audit Log', exact: true }).click(), query => !query.workspace_id)
      assert.equal(await h.page.getByRole('button', { name: 'Apply', exact: true }).count(), 0)
      assert.equal(await h.page.getByRole('button', { name: 'Clear Filters', exact: true }).count(), 0)
      assert.equal(await h.page.locator('.audit-entry').count(), 50)
      await change(h, () => h.page.getByRole('button', { name: 'Load more...', exact: true }).click(), query => query.offset === '50')
      assert.equal(await h.page.locator('.audit-entry').count(), 69)
      await h.page.locator('.audit-expand-icon').first().click()
      await h.page.getByRole('complementary', { name: 'Selected audit event', exact: true }).waitFor()
      await change(h, () => h.page.getByLabel('Entity type', { exact: true }).selectOption('agent_session'), query => query.entity_type === 'agent_session')
      assert.equal(await h.page.locator('.audit-entry').count(), 2)
      assert.equal(await h.page.locator('.audit-entry-detail').count(), 0)
      assert.equal(h.reads.at(-1).offset, undefined)
      await change(h, () => h.page.getByLabel('Action', { exact: true }).selectOption('create'), query => query.action === 'create')
      assert.equal(await h.page.locator('.audit-entry').count(), 1)
      await change(h, () => h.page.getByLabel('Entity type', { exact: true }).selectOption('task_session'), query => query.entity_type === 'task_session')
      assert.equal(await h.page.locator('.audit-entry').count(), 2)
      await change(h, () => h.page.getByLabel('Since', { exact: true }).fill('2026-10-16'), query => query.since === '2026-10-16T00:00:00Z')
      assert.equal(await h.page.locator('.audit-entry').count(), 0)
      assert.match(await h.page.locator('.audit-log-view').innerText(), /No (workspace )?audit (log )?entries found/)
      await change(h, () => h.page.getByLabel('Until', { exact: true }).fill('2026-10-29'), query => query.until === '2026-10-29T23:59:59Z')
      await change(h, () => h.page.getByRole('button', { name: 'Clear Filters', exact: true }).click(), query => !query.entity_type && !query.action && !query.since && !query.until)
      assert.equal(await h.page.locator('.audit-entry').count(), 50)
      assert.equal(await h.page.getByRole('button', { name: 'Clear Filters', exact: true }).count(), 0)
      assert.equal(h.reads.at(-1).workspace_id, global ? undefined : h.f.workspace.id)
      await change(h, () => h.page.getByLabel('Action', { exact: true }).selectOption('create'), query => query.action === 'create')
      await change(h, () => h.page.getByLabel('Action', { exact: true }).selectOption(''), query => !query.action)
      assert.equal(await h.page.getByRole('button', { name: 'Clear Filters', exact: true }).count(), 0)
    }
  } finally { await h.close() }
})

test('production audit rejects stale A-B-A and load-more completions and recovers from current errors', { timeout: 45000 }, async () => {
  const h = await setup()
  try {
    await workspaceAudit(h)
    const oldA = h.control(query => query.entity_type === 'task', { hold: true, response: { items: [event(h.f, 'stale-a')], has_more: true } })
    await h.page.getByLabel('Entity type', { exact: true }).selectOption('task'); await oldA.arrived
    await change(h, () => h.page.getByLabel('Entity type', { exact: true }).selectOption('project'), query => query.entity_type === 'project')
    const currentA = h.control(query => query.entity_type === 'task', { response: { items: [event(h.f, 'fresh-a')], has_more: true } })
    await change(h, () => h.page.getByLabel('Entity type', { exact: true }).selectOption('task'), query => query.entity_type === 'task')
    await h.page.locator('#audit-diff-row-fresh-a').waitFor()
    const staleResponse = h.page.waitForResponse(response => new URL(response.url()).searchParams.get('entity_type') === 'task')
    oldA.release(); await callbacksSettled(h, await staleResponse)
    await h.page.locator('#audit-diff-row-fresh-a').waitFor()
    assert.equal(await h.page.locator('#audit-diff-row-stale-a').count(), 0)
    const oldPage = h.control(query => query.offset === '1', { hold: true, response: { items: [event(h.f, 'stale-page')], has_more: false } })
    await h.page.getByRole('button', { name: 'Load more...', exact: true }).click(); await oldPage.arrived
    await change(h, () => h.page.getByLabel('Entity type', { exact: true }).selectOption('agent_session'), query => query.entity_type === 'agent_session')
    const pageResponse = h.page.waitForResponse(response => new URL(response.url()).searchParams.get('offset') === '1')
    oldPage.release(); await callbacksSettled(h, await pageResponse); await settled(h)
    assert.equal(await h.page.locator('.audit-entry').count(), 2)
    assert.equal(await h.page.locator('#audit-diff-row-stale-page').count(), 0)
    h.control(query => query.action === 'delete', { status: 500, response: { error: 'fixture failure' } })
    await change(h, () => h.page.getByLabel('Action', { exact: true }).selectOption('delete'), query => query.action === 'delete')
    await h.page.getByRole('alert').filter({ hasText: 'Failed to load audit entries for these filters.' }).waitFor()
    assert.equal(await h.page.locator('.audit-entry').count(), 0)
    await change(h, () => h.page.getByLabel('Action', { exact: true }).selectOption(''), query => !query.action)
    assert.equal(await h.page.locator('.audit-entry').count(), 2)
    assert.equal(await h.page.locator('.audit-log-view [role=alert]').count(), 0)
  } finally { await h.close() }
})

test('production audit return restores filters, selected event and page while workspace changes retire old reads', { timeout: 45000 }, async () => {
  const h = await setup()
  h.events.unshift(event(h.f, 'root-project', 'project'))
  try {
    await workspaceAudit(h)
    await change(h, () => h.page.getByLabel('Entity type', { exact: true }).selectOption('project'), query => query.entity_type === 'project')
    await h.page.locator('.audit-expand-icon').click()
    await h.page.getByRole('complementary', { name: 'Selected audit event', exact: true }).waitFor()
    await h.page.locator('.audit-entry .audit-entity-summary').click()
    await h.page.getByRole('button', { name: 'Back to workspace Audit entry', exact: true }).waitFor()
    await change(h, () => h.page.getByRole('button', { name: 'Back to workspace Audit entry', exact: true }).click(), query => query.entity_type === 'project')
    assert.equal(await h.page.getByLabel('Entity type', { exact: true }).inputValue(), 'project')
    await h.page.getByRole('complementary', { name: 'Selected audit event', exact: true }).waitFor()
    assert.equal(await h.page.locator('.audit-entry').count(), 1)
    const pending = h.control(query => query.entity_type === 'task', { hold: true, response: { items: [event(h.f, 'old-scope')], has_more: false } })
    await h.page.getByLabel('Entity type', { exact: true }).selectOption('task'); await pending.arrived
    const authority = h.page.waitForResponse(response => { const url = new URL(response.url()); return url.pathname === '/api/v1/session' && url.searchParams.get('workspace_id') === h.second.workspace.id })
    await h.page.evaluate(id => { history.pushState({}, '', '/workspace/' + id); dispatchEvent(new PopStateEvent('popstate')) }, h.second.workspace.id)
    await authority
    await change(h, () => h.page.getByRole('button', { name: 'Audit', exact: true }).click(), query => query.workspace_id === h.second.workspace.id)
    const oldResponse = h.page.waitForResponse(response => { const url = new URL(response.url()); return url.pathname === '/api/v1/audit' && url.searchParams.get('workspace_id') === h.f.workspace.id })
    pending.release(); await callbacksSettled(h, await oldResponse)
    assert.equal(await h.page.locator('.audit-entry').count(), 0)
    assert.match(await h.page.locator('.audit-log-view').innerText(), /No workspace audit entries found/)
  } finally { await h.close() }
})

test('production authorization retirement prevents a delayed audit response from restoring protected entries', { timeout: 45000 }, async () => {
  const h = await setup()
  try {
    await workspaceAudit(h)
    const pending = h.control(query => query.entity_type === 'task', { hold: true, response: { items: [event(h.f, 'old-auth')], has_more: false } })
    await h.page.getByLabel('Entity type', { exact: true }).selectOption('task'); await pending.arrived
    h.revoke()
    const authority = h.page.waitForResponse(response => new URL(response.url()).pathname === '/api/v1/session')
    await h.page.evaluate(workspace => window.pushHierarchyFrames([{ schema_version: 1, type: 'change', event: { schema_version: 1, event_id: 'audit-auth-retired', scope: 'workspace', workspace_id: workspace, occurred_at: '2026-10-29T12:00:00Z', transaction: { id: 'audit-auth-retired', cause: 'rest', request_id: 'other-request' }, actor: { type: 'system', id: null }, entity: { type: 'workspace', id: workspace, action: 'updated' }, invalidations: [{ kind: 'session_authorization', target: 'session-authorization' }] } }]), h.f.workspace.id)
    await callbacksSettled(h, await authority)
    await h.page.getByText('No workspace access', { exact: true }).waitFor()
    const oldResponse = h.page.waitForResponse(response => { const url = new URL(response.url()); return url.pathname === '/api/v1/audit' && url.searchParams.get('entity_type') === 'task' })
    pending.release(); await callbacksSettled(h, await oldResponse)
    assert.equal(await h.page.locator('.audit-entry').count(), 0)
    assert.equal(await h.page.locator('.audit-entry-detail').count(), 0)
    await h.page.getByText('No workspace access', { exact: true }).waitFor()
  } finally { await h.close() }
})
