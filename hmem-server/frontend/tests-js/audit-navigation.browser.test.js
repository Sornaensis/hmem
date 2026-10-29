import assert from 'node:assert/strict'
import test from 'node:test'
import { createHash } from 'node:crypto'
import { gzipSync } from 'node:zlib'
import { openSessionExplorer } from './support/sessions-ux-fixture.mjs'
import { generateFixture } from '../perf/fixtures.mjs'

const hash = bytes => createHash('sha256').update(bytes).digest('hex')
const missing = '90000000-0000-4000-8000-000000000001'
const forbidden = '90000000-0000-4000-8000-000000000002'
const otherWorkspace = '90000000-0000-4000-8000-000000000003'
const row = (h, id) => h.page.locator('#audit-event-' + id)
const event = (h, id, kind, target, values = {}, old = null) => ({ id, workspace_id: h.fixture.workspace.id, entity_type: kind, entity_id: target, action: 'update', old_values: old, new_values: values, changed_at: '2026-10-15T10:00:00Z', request_id: null, actor_type: 'user', actor_id: 'fixture-user', actor_label: 'Fixture User' })

async function setup() {
  const h = await openSessionExplorer()
  h.active.title = 'Audit active conversation'; h.saved.title = 'Audit saved evidence'
  const observation = { ...generateFixture('small').observations[0], workspace_id: h.fixture.workspace.id, content: 'Exact Observation selected from Audit.' }
  // Audit detail is deliberately absent from loaded/discovery membership.
  h.fixture.observations = []
  const project = h.fixture.projects[0], task = h.fixture.tasks[0], dependency = h.fixture.tasks[1]
  const events = [
    event(h, 'project', 'project', project.id, { name: project.name }),
    event(h, 'task', 'task', task.id, { title: task.title }),
    event(h, 'observation', 'observation', observation.id, { content: observation.content }),
    event(h, 'active', 'agent_session', h.active.id, { title: h.active.title }),
    event(h, 'saved', 'agent_session', h.saved.id, { title: h.saved.title }),
    event(h, 'connection', 'task_session', h.active.id + ':' + task.id, {}, { session_id: h.active.id, task_id: task.id }),
    event(h, 'dependency', 'task_dependency', task.id + ':' + dependency.id, {}, { task_id: task.id, depends_on_id: dependency.id }),
    event(h, 'workspace', 'workspace', h.fixture.workspace.id, { name: h.fixture.workspace.name }),
    ...['user', 'token', 'workspace_group', 'workspace_group_member', 'workspace_member', 'memory', 'category', 'unknown'].map(kind => event(h, 'unsupported-' + kind, kind, task.id)),
    event(h, 'malformed', 'task', 'not-a-uuid', { title: 'Malformed target' }),
    event(h, 'missing', 'task', missing, { title: 'Missing task' }),
    event(h, 'deleted', 'project', missing, { name: 'Deleted project', deleted_at: '2026-10-15T10:00:00Z' }),
    event(h, 'forbidden', 'task', forbidden, { title: 'Unavailable task' }),
    event(h, 'missing-observation', 'observation', missing, { content: 'Unavailable observation' }),
    event(h, 'forbidden-observation', 'observation', forbidden, { content: 'Unavailable observation' }),
    event(h, 'missing-session', 'agent_session', missing, { title: 'Unavailable session' }),
    { ...event(h, 'other-workspace', 'task', task.id, { title: 'Another workspace target' }), workspace_id: otherWorkspace },
  ]
  const controls = [], auditReads = [], archiveReads = []
  let principal = 'fixture-user'
  await h.page.route('**/api/v1/**', async route => {
    const request = route.request(), url = new URL(request.url()), path = url.pathname
    if (path === '/api/v1/session') {
      const workspace = url.searchParams.get('workspace_id'), denied = workspace === otherWorkspace
      if (denied) return route.fulfill({ status: 403, json: { error: 'Workspace unavailable' } })
      return route.fulfill({ json: { auth_mode: 'local', principal: { actor_type: 'user', actor_id: principal, actor_label: principal, authority: 'local_superadmin', grant_user_id: null }, global_permissions: { create_workspace: true, superadmin: true }, workspace: workspace ? { workspace_id: workspace, role: denied ? null : 'owner', can_read: !denied, can_edit: !denied, can_admin: !denied } : null } })
    }
    if (path === '/api/v1/observations/' + observation.id) return route.fulfill({ json: observation })
    if (path === '/api/v1/observations/' + missing || path.endsWith('/sessions/' + missing)) return route.fulfill({ status: 404, json: { error: 'Entity unavailable' } })
    if (path === '/api/v1/observations/' + forbidden) return route.fulfill({ status: 403, json: { error: 'Entity unavailable' } })
    if (path === '/api/v1/audit') {
      const query = Object.fromEntries(url.searchParams); auditReads.push(query)
      const control = controls.find(control => !control.used && control.match(query))
      if (control) { control.used = true; control.arrive(); await control.wait }
      const filtered = events.filter(entry => (!query.workspace_id || entry.workspace_id === query.workspace_id) && (!query.entity_type || entry.entity_type === query.entity_type) && (!query.action || entry.action === query.action))
      const offset = Number(query.offset || 0), limit = Number(query.limit || 50)
      return route.fulfill({ json: { items: filtered.slice(offset, offset + limit), has_more: filtered.length > offset + limit } })
    }
    if (path.includes('/navigation/focus/') && (path.endsWith(missing) || path.endsWith(forbidden))) return route.fulfill({ status: path.endsWith(forbidden) ? 403 : 404, json: { error: 'Entity unavailable' } })
    if (path.endsWith('/change-stream/ticket') && request.postDataJSON().scope.scope === 'global') return route.fulfill({ json: { ticket: 'global-ticket', expires_at: '2099-01-01T00:00:00Z' } })
    if (path.endsWith('/change-stream/resync') && request.postDataJSON().scope.scope === 'global') return route.fulfill({ json: { snapshot_profile: 'full_v1', items: [{ schema_version: 1, kind: 'workspace', data: h.fixture.workspace }], has_more: false, resume_token: 'global-audit-shell' } })
    if (path.includes('/sessions/' + h.saved.id + '/archive')) {
      archiveReads.push(path)
      const raw = h.content.get(h.saved.id), compressed = gzipSync(raw), generation = '33333333-3333-4333-8333-333333333333', etag = '"audit-sealed-evidence"'
      if (path.endsWith('/content')) {
        if (request.headers()['if-none-match']) return route.fulfill({ status: 304, headers: { ETag: etag } })
        return route.fulfill({ body: compressed, headers: { ETag: etag, 'Content-Type': 'application/gzip', 'Content-Length': String(compressed.length), 'X-HMem-Archive-Generation': generation, 'X-HMem-Archive-SHA256': hash(compressed), 'X-HMem-Archive-Raw-Bytes': String(raw.length), 'X-HMem-Archive-Raw-SHA256': hash(raw) } })
      }
      return route.fulfill({ json: { workspace_id: h.fixture.workspace.id, session_id: h.saved.id, state: 'ready', generation_id: generation, codec: 'gzip-v1', raw_bytes: String(raw.length), raw_records: String(raw.toString('utf8').split('\n').filter(Boolean).length), raw_sha256: hash(raw), compressed_bytes: String(compressed.length), compressed_sha256: hash(compressed), content_path: `/api/v1/workspaces/${h.fixture.workspace.id}/sessions/${h.saved.id}/archive/${generation}/content` } })
    }
    return route.fallback()
  })
  return { ...h, observation, events, auditReads, archiveReads, principal(value) { principal = value }, gate(match) {
    let release, arrive
    const wait = new Promise(resolve => { release = resolve }), arrived = new Promise(resolve => { arrive = resolve })
    const control = { match, release, arrive, wait, arrived, used: false }; controls.push(control); return control
  }, async close() { controls.forEach(control => control.release()); await h.close() } }
}

async function openAudit(h, global = false) {
  await h.start(); await h.idle()
  await h.page.getByRole(global ? 'link' : 'button', { name: global ? 'Audit Log' : 'Audit', exact: true }).click()
  await row(h, 'project').waitFor()
}
async function returnToAudit(h, global = false) {
  await h.page.getByRole('button', { name: global ? 'Back to global Audit entry' : 'Back to workspace Audit entry', exact: true }).click()
  await row(h, 'project').waitFor()
}
async function activate(h, id, related = false) {
  await row(h, id).scrollIntoViewIfNeeded()
  if (await row(h, id).locator('.audit-expand-icon').getAttribute('aria-expanded') !== 'true') await row(h, id).locator('.audit-expand-icon').click()
  await row(h, id).locator('.audit-entity-link').nth(related ? 1 : 0).click()
}

test('Audit destinations select actual Project Task Observation and active or sealed session evidence', { timeout: 60000 }, async () => {
  const h = await setup()
  let phase = 'initial audit'
  try {
    await openAudit(h)
    for (const [id, target, kind] of [['project', h.fixture.projects[0].id, 'project'], ['task', h.fixture.tasks[0].id, 'task']]) {
      phase = id
      const start = h.requests.length
      await activate(h, id); await h.page.locator('#entity-' + target).waitFor(); await h.idle()
      assert.ok(h.requests.slice(start).some(request => request.path.endsWith('/navigation/focus/' + kind + '/' + target)), 'Audit verifies a bounded exact target, even if cached')
      assert.ok(h.caps().maxBranches <= 4); assert.ok(h.caps().maxDetails <= 6)
      await returnToAudit(h); assert.equal(await row(h, id).getAttribute('class'), 'audit-entry audit-entry-selected')
    }
    phase = 'observation'
    await activate(h, 'observation')
    await h.page.locator('.observation-detail-content').waitFor({ timeout: 10000 })
    assert.equal(await h.page.locator('.observation-detail-content').textContent(), h.observation.content)
    assert.equal(new URLSearchParams(new URL(h.page.url()).hash.slice(1)).get('observation'), h.observation.id)
    assert.equal(new URLSearchParams(new URL(h.page.url()).hash.slice(1)).get('focus'), null)
    await returnToAudit(h)
    for (const [id, session, heading] of [['active', h.active, 'Audit active conversation'], ['saved', h.saved, 'Audit saved evidence']]) {
      phase = id
      await activate(h, id)
      await h.page.locator('.session-detail-header').getByRole('heading', { name: heading, exact: true }).waitFor()
      await h.page.locator('.session-record').first().waitFor()
      assert.equal(new URLSearchParams(new URL(h.page.url()).hash.slice(1)).get('session'), session.id)
      assert.equal(new URLSearchParams(new URL(h.page.url()).hash.slice(1)).get('focus'), null)
      if (id === 'saved') { await h.page.getByText('Verified complete archive.', { exact: true }).waitFor(); assert.ok(h.archiveReads.some(path => path.endsWith('/content'))) }
      await returnToAudit(h)
    }
    await activate(h, 'connection'); await h.page.locator('.session-detail-header').getByRole('heading', { name: 'Audit active conversation', exact: true }).waitFor(); await returnToAudit(h)
    await activate(h, 'connection', true); await h.page.locator('#entity-' + h.fixture.tasks[0].id).waitFor(); await returnToAudit(h)
    await activate(h, 'dependency'); await h.page.locator('#entity-' + h.fixture.tasks[0].id).waitFor(); await returnToAudit(h)
    await activate(h, 'dependency', true); await h.page.locator('#entity-' + h.fixture.tasks[1].id).waitFor(); await returnToAudit(h)
    await activate(h, 'workspace'); await h.page.locator('#entity-' + h.fixture.projects[0].id).waitFor()
    assert.equal(new URLSearchParams(new URL(h.page.url()).hash.slice(1)).get('focus'), null)
    await returnToAudit(h)
  } catch (error) { throw new Error(error.message + '\n' + JSON.stringify({ phase, url: h.page.url(), text: (await h.page.locator('body').innerText()).slice(-2500), reads: h.requests.slice(-4) }), { cause: error }) }
  finally { await h.close() }
})

test('Audit link text owns keyboard and edge activation while summary whitespace and other controls only inspect or copy', { timeout: 45000 }, async () => {
  const h = await setup()
  try {
    await openAudit(h)
    const entry = row(h, 'task'), summary = entry.locator('.audit-entity-summary'), link = entry.locator('.audit-entity-link').first()
    const bounds = await summary.boundingBox(), text = await link.boundingBox(), location = h.page.url()
    assert.ok(text.width < bounds.width - 30, 'The text control must not own the full summary width')
    await h.page.mouse.click(bounds.x + bounds.width - 3, bounds.y + bounds.height / 2)
    await h.page.waitForFunction(id => document.getElementById('audit-event-' + id)?.classList.contains('audit-entry-selected'), 'task')
    assert.equal(h.page.url(), location)
    assert.equal(await entry.getAttribute('class'), 'audit-entry audit-entry-selected')
    await h.page.getByRole('button', { name: 'Copy audit entry ID: task', exact: true }).click()
    assert.equal(h.page.url(), location)
    await link.focus(); await h.page.keyboard.press('Enter'); await h.page.locator('#entity-' + h.fixture.tasks[0].id).waitFor(); await returnToAudit(h)
    await entry.locator('.audit-entity-link').first().focus(); await h.page.keyboard.press('Space'); await h.page.locator('#entity-' + h.fixture.tasks[0].id).waitFor(); await returnToAudit(h)
    const edge = await entry.locator('.audit-entity-link').first().boundingBox()
    await h.page.mouse.click(edge.x + edge.width - 1, edge.y + edge.height / 2); await h.page.locator('#entity-' + h.fixture.tasks[0].id).waitFor(); await returnToAudit(h)
    for (const id of ['malformed', ...['user', 'token', 'workspace_group', 'workspace_group_member', 'workspace_member', 'memory', 'category', 'unknown'].map(kind => 'unsupported-' + kind)]) {
      assert.equal(await row(h, id).locator('.audit-entity-link').count(), 0)
      assert.match(await row(h, id).innerText(), /Destination unavailable/)
    }
  } finally { await h.close() }
})

test('missing deleted and forbidden targets have explicit outcomes and global cross-workspace return remains usable', { timeout: 45000 }, async () => {
  const h = await setup()
  try {
    await openAudit(h, true)
    for (const id of ['missing', 'deleted', 'forbidden']) {
      await activate(h, id)
      await h.page.getByRole('status').filter({ hasText: 'Entity destination unavailable. It may no longer exist or be accessible.' }).waitFor()
      assert.equal(await h.page.locator('#entity-' + h.fixture.tasks[0].id).count(), 0, 'An unavailable target never displays an unrelated cached card')
      await returnToAudit(h, true)
      assert.equal(await row(h, id).getAttribute('class'), 'audit-entry audit-entry-selected')
    }
    for (const id of ['missing-observation', 'forbidden-observation']) {
      await activate(h, id)
      await h.page.getByRole('status').filter({ hasText: 'Entity destination unavailable. It may no longer exist or be accessible.' }).waitFor()
      await returnToAudit(h, true)
      assert.equal(await row(h, id).getAttribute('class'), 'audit-entry audit-entry-selected')
    }
    await activate(h, 'missing-session')
    await h.page.getByRole('alert').filter({ hasText: 'Session could not be loaded.' }).waitFor()
    await returnToAudit(h, true)
    assert.equal(await row(h, 'missing-session').getAttribute('class'), 'audit-entry audit-entry-selected')
    await activate(h, 'other-workspace')
    try { await h.page.getByText('You do not have access to this workspace or session scope', { exact: true }).waitFor({ timeout: 10000 }) }
    catch (error) { throw new Error(error.message + '\n' + JSON.stringify({ url: h.page.url(), text: (await h.page.locator('body').innerText()).slice(-1800), reads: h.requests.slice(-3) }), { cause: error }) }
    await returnToAudit(h, true)
    assert.equal(await row(h, 'other-workspace').getAttribute('class'), 'audit-entry audit-entry-selected')
  } finally { await h.close() }
})

test('Audit restores an exact scrolled source and a newer filter retires a held return viewport', { timeout: 45000 }, async () => {
  const h = await setup()
  h.events.unshift(...Array.from({ length: 35 }, (_, index) => event(h, 'filler-' + index, 'task', h.fixture.tasks[0].id, { title: 'Filler history ' + index })))
  try {
    await openAudit(h)
    await row(h, 'task').scrollIntoViewIfNeeded()
    const before = await h.page.locator('#main-content-scroll').evaluate(node => node.scrollTop)
    assert.ok(before > 200)
    await activate(h, 'task'); await h.page.locator('#entity-' + h.fixture.tasks[0].id).waitFor()
    await returnToAudit(h)
    await h.page.waitForFunction(top => Math.abs(document.getElementById('main-content-scroll').scrollTop - top) < 2, before)
    await activate(h, 'task'); await h.page.locator('#entity-' + h.fixture.tasks[0].id).waitFor()
    const held = h.gate(query => !query.action)
    await h.page.getByRole('button', { name: 'Back to workspace Audit entry', exact: true }).click(); await held.arrived
    const changed = h.page.waitForResponse(response => {const url = new URL(response.url()); return url.pathname === '/api/v1/audit' && url.searchParams.get('action') === 'create'})
    await h.page.getByLabel('Action', { exact: true }).selectOption('create'); await changed
    await h.page.getByText('No workspace audit entries found.', { exact: true }).waitFor()
    const top = await h.page.locator('#main-content-scroll').evaluate(node => node.scrollTop)
    const oldResponse = h.page.waitForResponse(response => {const url = new URL(response.url());return url.pathname === '/api/v1/audit' && !url.searchParams.has('action')})
    held.release(); await (await oldResponse).finished(); await h.page.evaluate(() => new Promise(resolve => requestAnimationFrame(() => requestAnimationFrame(resolve))))
    assert.equal(await h.page.locator('#main-content-scroll').evaluate(node => node.scrollTop), top)
    assert.equal(await h.page.locator('.audit-entry').count(), 0)
  } finally { await h.close() }
})

test('uncached deep Project and Task audit targets use bounded direct loading without unrelated cache selection', { timeout: 45000 }, async () => {
  const h = await setup()
  try {
    await h.start(); await h.idle()
    const project = { ...h.fixture.projects[0], id: '80000000-0000-4000-8000-000000000001', parent_id: h.fixture.projects[0].id, name: 'Uncached Audit project' }
    const task = { ...h.fixture.tasks[0], id: '80000000-0000-4000-8000-000000000002', parent_id: h.fixture.tasks[0].id, title: 'Uncached Audit task' }
    h.fixture.projects.push(project); h.fixture.tasks.push(task)
    h.events.push(event(h, 'uncached-project', 'project', project.id, { name: project.name }), event(h, 'uncached-task', 'task', task.id, { title: task.title }))
    await h.page.getByRole('button', { name: 'Audit', exact: true }).click(); await row(h, 'uncached-project').waitFor()
    for (const [id, entity, kind] of [['uncached-project', project, 'project'], ['uncached-task', task, 'task']]) {
      const start = h.requests.length
      await activate(h, id); await h.page.locator('#entity-' + entity.id).waitFor(); await h.idle()
      assert.match(await h.page.locator('#entity-' + entity.id).innerText(), new RegExp(entity.name || entity.title))
      assert.equal(h.requests.slice(start).filter(request => request.path.endsWith('/navigation/focus/' + kind + '/' + entity.id)).length, 1)
      assert.ok(h.caps().maxBranches <= 4); assert.ok(h.caps().maxDetails <= 6)
      await returnToAudit(h)
    }
  } finally { await h.close() }
})
