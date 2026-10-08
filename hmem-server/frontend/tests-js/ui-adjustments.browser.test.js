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

test('audit status and timestamps retain minute precision without truncating timestamp-shaped titles', { timeout: 30000 }, async () => {
  const h = await openHierarchy(smallFixture())
  h.page.setDefaultTimeout(5000)
  const timestamp = '2026-10-08T12:34:56.789+02:00', title = '2026-10-08T12:34 integration notes'
  const entry = { id: 'audit-status', workspace_id: h.fixture.workspace.id, entity_type: 'task', entity_id: 'working-task', action: 'update', old_values: { title: 'Before', status: 'todo', due_at: '2026-10-07T09:08:07Z' }, new_values: { title, status: 'done', due_at: timestamp }, changed_at: timestamp }
  await h.page.route('**/api/v1/audit**', route => route.fulfill({ status: 200, contentType: 'application/json', body: JSON.stringify({ items: [entry], has_more: false }) }))
  try {
    await h.start(); await h.idle()
    const project = h.page.locator('#entity-root-project')
    await project.locator('.btn-extras-toggle').click(); await h.idle()
    await project.locator('.entity-history-toggle').click(); await project.locator('.history-entry').waitFor()
    assert.equal(await project.locator('.history-timestamp').innerText(), '2026-10-08 12:34')
    assert.match(await project.locator('.history-diff').innerText(), /todo.*done/s)
    const geometry = await project.evaluate(element => { const scroll = document.getElementById('main-content-scroll'); return { top: element.getBoundingClientRect().top, host: scroll.getBoundingClientRect().top, viewport: scroll.clientHeight } })
    assert.ok(geometry.top >= geometry.host && geometry.top < geometry.host + geometry.viewport - 72, JSON.stringify(geometry))
    if (process.env.HMEM_UI_REFINEMENT_SHOTS) await h.page.screenshot({ path: process.env.HMEM_UI_REFINEMENT_SHOTS + '/expanded-project-card.png' })
    await h.page.getByRole('button', { name: 'Audit', exact: true }).click()
    await h.page.locator('.audit-entry').waitFor()
    assert.equal(await h.page.getByRole('button', { name: 'Open', exact: true }).count(), 0)
    assert.equal(await h.page.locator('.audit-timestamp').innerText(), '2026-10-08 12:34')
    assert.equal(await h.page.locator('.audit-status-change').innerText(), 'todo → done')
    assert.equal(await h.page.locator('option[value=observation]').innerText(), 'Observations'); assert.equal(await h.page.locator('option[value=memory]').count(), 0)
    await h.page.locator('.audit-expand-icon').click(); await h.page.locator('.audit-entry-detail').waitFor()
    assert.match(await h.page.locator('.audit-entry-detail').innerText(), /2026-10-08 12:34/)
    assert.equal(await h.page.locator('.history-diff-new').filter({ hasText: title }).innerText(), title)
    assert.equal(await h.page.locator('.audit-entry-detail').getByText(timestamp, { exact: true }).count(), 0)
  } finally { await h.close() }
})

test('lifecycle graph preferences persist per workspace and breadcrumbs reveal the exact tall event', { timeout: 60000 }, async () => {
  const h = await workspaceFixture(), bucketRequests = []
  const actions = { created: 1, completed: 2, deleted: 3, archived: 4, cancelled: 5 }
  const legacy = { created: 1, completed: 2, cancelled: 3 }
  const bucket = { bucket_start: '2026-09-01T00:00:00Z', bucket_end: '2026-09-08T00:00:00Z', label: 'Sep 1', counts: { project: legacy, subproject: legacy, task: legacy, subtask: legacy }, totals: legacy, series: { project: actions, task: actions, subtask: actions, observation: { ...actions, archived: 0, cancelled: 0 } }, series_totals: actions }
  const event = { id: 'source-event', workspace_id: h.first.workspace.id, event_type: 'project_completed', entity_type: 'project', entity_id: 'root-project', title: 'Long project title '.repeat(120), occurred_at: '2026-09-07T12:34:56.987Z', status_transition: { from: 'active', to: 'completed' }, navigation: { entity_type: 'project', entity_id: 'root-project' } }
  await h.page.route('**/api/v1/**', async route => {
    const url = new URL(route.request().url()), id = url.pathname.match(/workspaces\/([^/]+)/)?.[1]
    let value
    if (url.pathname.endsWith('/timeline/buckets')) {
      bucketRequests.push(url)
      assert.equal(url.searchParams.get('paged'), 'true')
      value = { workspace_id: id, since: url.searchParams.get('since') || '2026-09-01T00:00:00Z', until: url.searchParams.get('until'), bucket: url.searchParams.get('bucket'), buckets: [bucket, { ...bucket, bucket_start: '2026-09-08T00:00:00Z', bucket_end: '2026-09-15T00:00:00Z', label: 'Sep 8', series: { ...bucket.series, project: { ...actions, created: 4, completed: 0, deleted: 2, archived: 1, cancelled: 2 } } }, { ...bucket, bucket_start: '2026-09-15T00:00:00Z', bucket_end: '2026-09-22T00:00:00Z', label: 'Sep 15' }], next_since: null }
    } else if (url.pathname.endsWith('/timeline')) value = { items: id === h.first.workspace.id ? [event] : [], has_more: false }
    else return route.fallback()
    await route.fulfill({ status: 200, contentType: 'application/json', body: JSON.stringify(value) })
  })
  const choices = () => h.page.locator('.timeline-graph-control select')
  const ready = async () => { await h.page.locator('.timeline-line-chart').waitFor(); await h.page.waitForFunction(() => !document.querySelector('.timeline-graph-control select:disabled')); await h.idle() }
  try {
    await h.start(); await h.idle(); await h.page.getByRole('button', { name: 'Timeline', exact: true }).click(); await ready()
    assert.equal(await choices().nth(0).inputValue(), '30'); assert.equal(await choices().nth(1).inputValue(), 'week')
    assert.deepEqual(await choices().nth(0).locator('option').allTextContents(), ['Last 7 days', 'Last 14 days', 'Last 30 days', 'Last 6 months', 'Last Year', 'All time'])
    assert.equal(await h.page.locator('.timeline-line-chart').count(), 1); assert.equal(await h.page.locator('.timeline-graph-content table,input[type=date]').count(), 0)
    assert.equal(await h.page.locator('.timeline-event-time').innerText(), '2026-09-07 12:34')
    assert.match(await h.page.locator('.timeline-status-transition').innerText(), /Active.*Completed/s)
    for (const action of ['created', 'completed', 'deleted', 'archived', 'cancelled']) {
      assert.deepEqual(await h.page.evaluate(action => ({ marker: getComputedStyle(document.querySelector('.timeline-series-toggle.timeline-series-' + action + ' .timeline-series-marker')).borderTopColor, point: getComputedStyle(document.querySelector('g.timeline-series-' + action + ' .timeline-point')).fill }), action).then(value => value.marker === value.point), true)
    }
    if (process.env.HMEM_UI_REFINEMENT_SHOTS) await h.page.screenshot({ path: process.env.HMEM_UI_REFINEMENT_SHOTS + '/lifecycle.png' })
    await choices().nth(0).selectOption('year'); await ready(); await choices().nth(1).selectOption('day'); await ready()
    await h.page.reload(); await ready(); assert.equal(await choices().nth(0).inputValue(), 'year'); assert.equal(await choices().nth(1).inputValue(), 'day')
    await h.page.getByRole('link', { name: /Workspace B/ }).click(); await h.page.locator('.workspace-header-title').filter({ hasText: 'Workspace B' }).waitFor()
    await h.page.getByRole('button', { name: 'Timeline', exact: true }).click(); await ready()
    assert.equal(await choices().nth(0).inputValue(), '30'); assert.equal(await choices().nth(1).inputValue(), 'week')
    await choices().nth(0).selectOption('14'); await ready()
    await h.page.getByRole('link', { name: /Workspace A/ }).click(); await h.page.getByRole('button', { name: 'Timeline', exact: true }).click(); await ready()
    assert.equal(await choices().nth(0).inputValue(), 'year'); assert.equal(await choices().nth(1).inputValue(), 'day')
    await choices().nth(0).selectOption('all'); await ready(); assert.equal(bucketRequests.at(-1).searchParams.has('since'), false)
    await h.page.setViewportSize({ width: 320, height: 800 })
    const card = h.page.locator('#timeline-event-source-event')
    await card.click({ position: { x: 20, y: 20 } }); await h.page.locator('#entity-root-project').waitFor(); await h.idle()
    await h.page.getByText('Back to Timeline event', { exact: true }).click()
    await h.page.waitForFunction(() => document.activeElement?.id === 'timeline-event-source-event')
    const geometry = await card.evaluate(element => { const scroll = document.getElementById('main-content-scroll'); return { top: element.getBoundingClientRect().top, host: scroll.getBoundingClientRect().top, height: element.getBoundingClientRect().height, viewport: scroll.clientHeight } })
    assert.ok(geometry.height > geometry.viewport, JSON.stringify(geometry)); assert.ok(geometry.top >= geometry.host && geometry.top < geometry.host + geometry.viewport - 72, JSON.stringify(geometry))
    if (process.env.HMEM_UI_REFINEMENT_SHOTS) await h.page.screenshot({ path: process.env.HMEM_UI_REFINEMENT_SHOTS + '/returned-timeline-card.png' })
  } finally { await h.close() }
})

test('lifecycle bucket pages concatenate once and reject a completed stale result after range switch', { timeout: 30000 }, async () => {
  const h = await workspaceFixture(), requests = []
  h.page.setDefaultTimeout(5000)
  const edge = '2000-01-02T00:00:00Z'
  const zero = { created: 0, completed: 0, deleted: 0, archived: 0, cancelled: 0 }
  const legacy = { created: 0, completed: 0, cancelled: 0 }
  const row = (start, end, label, count) => ({ bucket_start: start, bucket_end: end, label, counts: { project: legacy, subproject: legacy, task: legacy, subtask: legacy }, totals: legacy, series: { project: { ...zero, created: count }, task: zero, subtask: zero, observation: zero }, series_totals: { ...zero, created: count } })
  let allTimePass = 0, releaseStale, secondArrived
  const staleSecondArrived = new Promise(resolve => { secondArrived = resolve })
  const staleSecondRelease = new Promise(resolve => { releaseStale = resolve })
  await h.page.route('**/api/v1/**', async route => {
    const url = new URL(route.request().url()), id = url.pathname.match(/workspaces\/([^/]+)/)?.[1]
    if (url.pathname.endsWith('/timeline')) {
      await route.fulfill({ status: 200, contentType: 'application/json', body: JSON.stringify({ items: [], has_more: false }) })
      return
    }
    if (!url.pathname.endsWith('/timeline/buckets')) return route.fallback()
    const since = url.searchParams.get('since'), until = url.searchParams.get('until'), bucket = url.searchParams.get('bucket')
    assert.equal(url.searchParams.get('paged'), 'true')
    if (!since) allTimePass++
    const pass = !since || since === edge ? allTimePass : 0
    requests.push({ since, until, bucket, pass })
    let buckets, next = null
    if (!since) {
      assert.equal(bucket, 'day')
      buckets = [row('2000-01-01T00:00:00Z', edge, 'First page', pass === 1 ? 3 : 101)]
      next = edge
    } else if (since === edge) {
      assert.equal(bucket, 'day')
      buckets = [row(edge, '2000-01-03T00:00:00Z', 'Second page', pass === 1 ? 7 : 202)]
      if (pass === 2) { secondArrived(); await staleSecondRelease }
    } else {
      buckets = [row(since, until, 'Current range', 23)]
    }
    await route.fulfill({ status: 200, contentType: 'application/json', body: JSON.stringify({ workspace_id: id, since: since || '2000-01-01T00:00:00Z', until, bucket, buckets, next_since: next }) })
  })
  const choices = () => h.page.locator('.timeline-graph-control select')
  const ready = async () => { await h.page.locator('.timeline-line-chart').waitFor(); await h.page.waitForFunction(() => !document.querySelector('.timeline-graph-control select:disabled')); await h.idle() }
  const created = () => h.page.locator('g.timeline-point-control.timeline-series-created')
  try {
    await h.start(); await h.idle(); await h.page.getByRole('button', { name: 'Timeline', exact: true }).click(); await ready()
    await choices().nth(1).selectOption('day'); await ready()
    await choices().nth(0).selectOption('all'); await ready()
    assert.equal(await choices().nth(1).inputValue(), 'day')
    assert.equal(await created().count(), 2)
    const labels = await created().evaluateAll(points => points.map(point => point.getAttribute('aria-label')))
    assert.match(labels[0], /First page, 2000-01-01 00:00 to 2000-01-02 00:00 exclusive, 3, not selected$/)
    assert.match(labels[1], /Second page, 2000-01-02 00:00 to 2000-01-03 00:00 exclusive, 7, not selected$/)
    const complete = requests.filter(request => request.pass === 1)
    assert.equal(complete.length, 2)
    assert.deepEqual(complete.map(request => request.since), [null, edge])
    assert.deepEqual(complete.map(request => request.bucket), ['day', 'day'])
    assert.equal(complete[0].until, complete[1].until)
    assert.equal(await h.page.locator('g.timeline-point-control').count(), 10, 'each of two pages contributes exactly one bucket to five action lines')
    await choices().nth(0).selectOption('14'); await ready()
    await choices().nth(0).selectOption('all')
    await h.bounded(staleSecondArrived, 5000, 'Stale second bucket page arrival')
    await choices().nth(0).selectOption('7'); await ready()
    assert.equal(await choices().nth(0).inputValue(), '7')
    assert.equal(await choices().nth(1).inputValue(), 'day')
    const replacementLabels = await created().evaluateAll(points => points.map(point => point.getAttribute('aria-label')))
    assert.equal(replacementLabels.length, 1); assert.match(replacementLabels[0], /Current range, .*exclusive, 23, not selected$/)
    const completedStale = h.page.waitForResponse(response => new URL(response.url()).pathname.endsWith('/timeline/buckets') && new URL(response.url()).searchParams.get('since') === edge)
    releaseStale()
    const staleResponse = await completedStale
    assert.equal(staleResponse.status(), 200)
    assert.equal(await staleResponse.finished(), null)
    assert.equal((await staleResponse.json()).buckets[0].series.project.created, 202)
    await h.page.evaluate(() => new Promise(resolve => requestAnimationFrame(() => requestAnimationFrame(() => requestAnimationFrame(resolve)))))
    assert.deepEqual(await created().evaluateAll(points => points.map(point => point.getAttribute('aria-label'))), replacementLabels)
    assert.equal(await choices().nth(0).inputValue(), '7'); assert.equal(await choices().nth(1).inputValue(), 'day')
    assert.equal(requests.filter(request => request.pass === 2).length, 2)
    assert.equal(await h.page.locator('.timeline-graph-error').count(), 0)
  } finally { releaseStale(); await h.close() }
})

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
