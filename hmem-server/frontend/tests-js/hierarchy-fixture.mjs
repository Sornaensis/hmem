import assert from 'node:assert/strict'
import { createObservationDiscoveryAdapter } from './support/observation-discovery-adapter.mjs'
import { readFile } from 'node:fs/promises'
import { createServer } from 'node:http'
import { resolve, sep } from 'node:path'
import { chromium } from '@playwright/test'
import { generateFixture, queryObservations, navigationBranchResponse, navigationBatchDescriptors, navigationBatchResponse, navigationFocusResponse, navigationSummariesResponse, projectOverviewResponse, taskOverviewResponse, taskReadinessRollup, workspaceShellSnapshotItems } from '../perf/fixtures.mjs'

// Controlled HTTP-fixture equivalent; backend predicate equivalence is tested
// separately against PostgreSQL. Never count a paginated fixture prefix.
export function allFixtureObservations(fixture, options) {
  const values = []
  for (let offset = 0; ; offset += 200) {
    const page = queryObservations(fixture, { ...options, offset, limit: 200 })
    values.push(...page.items)
    if (!page.has_more) return values
  }
}

export function fixtureObservationCounts(fixture, query) {
  const scoped = fixture.observations.filter(value => value.workspace_id === query.workspace_id)
  let values = allFixtureObservations({ ...fixture, observations: scoped }, { query: query.query, subjectKind: query.subject_kind, subject: query.subject, gitSha: query.git_sha, currentGitSha: query.current_git_sha, historyGitSha: query.history_git_sha })
  if (query.paths) values = values.filter(value => value.subjects.some(subject => (!query.subject_kind || subject.subject_kind === query.subject_kind) && query.paths.some(path => {
    if (subject.subject_kind === 'file') return subject.subject === path
    const pattern = subject.subject.split('/').map((part, i, all) => part === '**' ? (i === all.length - 1 ? '.*' : '(?:[^/]+/)*') : part.replace(/[.+^${}()|[\]\\]/g, '\\$&').replace(/\*/g, '[^/]*').replace(/\?/g, '[^/]') + (i === all.length - 1 ? '' : '/')).join('')
    return new RegExp('^' + pattern + '$').test(path)
  })))
  return { workspace_id: query.workspace_id, total_count: scoped.length, match_count: values.length }
}

export function hierarchyFixture() {
  const base = generateFixture('small')
  const project = (id, parent = null) => ({ ...base.projects[0], id, parent_id: parent, name: id, status: 'active', priority: 1, description: 'A [Markdown link](https://example.invalid/reference) for ' + id })
  const task = (id, parent = null) => ({ ...base.tasks[0], id, project_id: 'root-project', parent_id: parent, title: id, status: 'todo', priority: 1, dependency_count: 0, description: 'A [task link](https://example.invalid/task) for ' + id })
  return { ...base, projects: [project('root-project'), ...Array.from({ length: 63 }, (_, i) => project('project-' + String(i).padStart(3, '0'), 'root-project'))],
    tasks: [...Array.from({ length: 121 }, (_, i) => task('task-' + String(i).padStart(3, '0'))), ...Array.from({ length: 113 }, (_, i) => task('subtask-' + String(i).padStart(3, '0'), 'task-000'))],
    dependencies: [], observations: [], timelineEvents: [], timelineBuckets: [], liveFrames: [] }
}

// Match the production candidate contract, including unloaded subtasks. The
// database remains the authority; this supplies controlled browser responses.
export function projectNextTasksResponse(fixture, projectId, includeBlocked, limit, offset = 0, page = false) {
  const projects = new Map(fixture.projects.map(project => [project.id, project]))
  const tasks = new Map(fixture.tasks.map(task => [task.id, task]))
  const open = task => task && !task.deleted_at && ['todo', 'in_progress', 'blocked'].includes(task.status)
  const projectScope = new Set()
  const visitProject = id => {
    const project = projects.get(id)
    if (!project || project.deleted_at || !['active', 'paused'].includes(project.status) || projectScope.has(id)) return
    projectScope.add(id)
    for (const child of fixture.projects) if (child.parent_id === id && child.workspace_id === project.workspace_id) visitProject(child.id)
  }
  visitProject(projectId)
  const scope = new Map(fixture.tasks.filter(task => !task.deleted_at && projectScope.has(task.project_id) && task.workspace_id === projects.get(task.project_id).workspace_id).map(task => [task.id, task.project_id]))
  for (let changed = true; changed;) {
    changed = false
    for (const task of fixture.tasks) if (!task.deleted_at && scope.has(task.parent_id) && !scope.has(task.id) && task.workspace_id === tasks.get(task.parent_id).workspace_id) {
      scope.set(task.id, scope.get(task.parent_id)); changed = true
    }
  }
  const ancestryHas = (rows, initial, predicate) => {
    const seen = new Set()
    for (let id = initial; id != null && !seen.has(id); id = rows.get(id)?.parent_id) {
      seen.add(id)
      if (predicate(rows.get(id))) return true
    }
    return false
  }
  const projectPaused = task => ancestryHas(tasks, task.id, ancestor => ancestor && ancestryHas(projects, ancestor.project_id, project => project?.status === 'paused'))
  const closedPlacement = task => ancestryHas(tasks, task.parent_id, ancestor => !ancestor || ancestor.deleted_at || ['done', 'cancelled'].includes(ancestor.status))
    || ancestryHas(tasks, task.id, ancestor => ancestor && ancestryHas(projects, ancestor.project_id, project => !project || project.deleted_at || ['completed', 'archived'].includes(project.status)))
  const dependencies = task => fixture.dependencies.filter(edge => edge.task_id === task.id && open(tasks.get(edge.depends_on_id))).length
  const descendants = task => [...scope.keys()].filter(id => id !== task.id && open(tasks.get(id)) && ancestryHas(tasks, tasks.get(id).parent_id, ancestor => ancestor?.id === task.id)).length
  return [...scope.keys()].map(id => tasks.get(id))
    .filter(task => open(task) && !closedPlacement(task)
      && (includeBlocked || (task.status !== 'blocked' && !projectPaused(task)))
      && (!task.parent_id || (tasks.has(task.parent_id) && !projectPaused(tasks.get(task.parent_id)) && taskReadinessRollup(fixture, task.parent_id).can_start_subtasks) || (includeBlocked && (page || projectPaused(task)))))
    .map(task => { const openDescendants = descendants(task), openDependencies = dependencies(task); return { task, completion_gated: openDescendants > 0, open_descendant_count: openDescendants, dependency_blocked: openDependencies > 0, open_dependency_count: openDependencies, project_paused: projectPaused(task), parent_blocked: !!task.parent_id && !taskReadinessRollup(fixture, task.parent_id).can_start_subtasks } })
    .filter(candidate => includeBlocked || !candidate.dependency_blocked)
    .sort((a, b) => projects.get(scope.get(b.task.id)).priority - projects.get(scope.get(a.task.id)).priority || b.task.priority - a.task.priority || a.task.created_at.localeCompare(b.task.created_at) || a.task.id.localeCompare(b.task.id))
    .slice(offset, offset + (page ? limit : Math.min(200, Math.max(1, limit))))
}

async function bounded(action, milliseconds, label) {
  let timer
  try { return await Promise.race([action, new Promise((_, reject) => { timer = setTimeout(() => reject(new Error(label + ' exceeded ' + milliseconds + 'ms')), milliseconds) })]) }
  finally { clearTimeout(timer) }
}

const socketScript = () => {
  const sockets = []
  class FixtureSocket {
    static OPEN = 1; static CONNECTING = 0; static CLOSED = 3
    constructor(url) { this.url = url; this.readyState = 0; sockets.push(this); setTimeout(() => { this.readyState = 1; this.onopen?.({ type: 'open' }); setTimeout(() => this.emit({ schema_version: 1, type: 'checkpoint', catch_up: 'complete', resume_token: 'browser-ready' }), 0) }, 0) }
    send() {}
    close() { this.readyState = 3; this.onclose?.({ code: 1000, reason: 'fixture' }) }
    addEventListener(type, fn) { this['on' + type] = fn }
    removeEventListener(type, fn) { if (this['on' + type] === fn) this['on' + type] = null }
    emit(frame) { if (this.readyState === 1) this.onmessage?.({ data: JSON.stringify(frame) }) }
  }
  window.WebSocket = FixtureSocket
  // Explicit raw frames allow tests to observe a non-live phase before a later
  // checkpoint. The established helpers below retain their automatic checkpoint.
  window.pushHierarchyRawFrames = frames => { for (const socket of sockets.filter(s => s.readyState === 1)) for (const frame of frames) socket.emit(frame) }
  window.pushHierarchyGlobalFrames = frames => { for (const socket of sockets.filter(s => s.readyState === 1 && new URL(s.url).searchParams.get('ticket') === 'global-ticket')) { for (const frame of frames) socket.emit(frame); socket.emit({ schema_version: 1, type: 'checkpoint', catch_up: 'complete', resume_token: 'global-refresh' }) } }
  window.pushHierarchyFrames = frames => { for (const socket of sockets.filter(s => s.readyState === 1)) { for (const frame of frames) socket.emit(frame); socket.emit({ schema_version: 1, type: 'checkpoint', catch_up: 'complete', resume_token: 'browser-next' }) } }
  const NativeObserver = window.ResizeObserver
  window.hierarchyObserved = new Set()
  window.hierarchyMaximum = 0
  window.hierarchyObserverMaximum = 0
  if (NativeObserver) window.ResizeObserver = class extends NativeObserver {
    observe(row, ...args) { if (row.hasAttribute('data-hierarchy-key')) window.hierarchyObserved.add(row); window.hierarchyObserverMaximum = Math.max(window.hierarchyObserverMaximum, window.hierarchyObserved.size); super.observe(row, ...args) }
    unobserve(row) { window.hierarchyObserved.delete(row); super.unobserve(row) }
    disconnect() { window.hierarchyObserved.clear(); super.disconnect() }
  }
  new MutationObserver(() => { window.hierarchyMaximum = Math.max(window.hierarchyMaximum, document.querySelectorAll('[data-hierarchy-key]').length) }).observe(document, { childList: true, subtree: true })
}

export async function openHierarchy(fixture = hierarchyFixture(), additionalFixtures = [], authority = null, pageOptions = {}, launchOptions = {}) {
  const fixtures = [fixture, ...additionalFixtures]
  const staticRoot = resolve(process.cwd(), '../static')
  const server = createServer(async (request, response) => {
    const path = new URL(request.url, 'http://fixture').pathname
    if (path === '/hmem-runtime-config.js') { response.writeHead(200, { 'content-type': 'application/javascript' }); response.end("window.HMEM_CONFIG={authMode:'local',apiUrl:window.location.origin,wsUrl:'ws://fixture.invalid/api/v1/ws',authTokenStorage:'memory'};"); return }
    if (path === '/favicon.ico') { response.writeHead(204); response.end(); return }
    const file = resolve(staticRoot, path === '/' || path.startsWith('/workspace/') ? 'index.html' : path.slice(1))
    if (!file.startsWith(staticRoot + sep)) { response.writeHead(403); response.end(); return }
    try { const body = await readFile(file); response.writeHead(200, { 'content-type': file.endsWith('.js') ? 'application/javascript' : file.endsWith('.css') ? 'text/css' : 'text/html' }); response.end(body) }
    catch { response.writeHead(404); response.end() }
  })
  await new Promise(resolveListen => server.listen(0, '127.0.0.1', resolveListen))
  let browser, browserServer
  const held = new Set(), controls = [], requests = [], errors = [], unhandled = []
  let principalId = 'fixture-user'
  const discovery = createObservationDiscoveryAdapter(fixtures, { allObservations: allFixtureObservations, actor: () => principalId })
  let activeBranches = 0, activeDetails = 0, maxBranches = 0, maxDetails = 0
  async function retire() {
    for (const release of held) release()
    for (const control of controls) control.release()
    try {
      try { if (browser) await bounded(browser.close(), 5000, 'Owned browser close') }
      finally { if (browserServer) await bounded(browserServer.kill(), 5000, 'Owned Chromium retirement') }
    } finally {
      server.closeAllConnections()
      await bounded(new Promise((resolveClose, rejectClose) => server.close(error => error ? rejectClose(error) : resolveClose())), 5000, 'Owned HTTP server close')
    }
  }
  try {
    browserServer = await chromium.launchServer({ headless: true, ...launchOptions })
    browser = await chromium.connect(browserServer.wsEndpoint())
    const page = await browser.newPage({ viewport: { width: 1440, height: 1000 }, timezoneId: 'UTC', ...pageOptions })
    page.on('pageerror', error => errors.push(error.message))
    await page.addInitScript(socketScript)
    await page.route('**/api/v1/**', async route => {
      const request = route.request(), url = new URL(request.url()), path = url.pathname
      const navigation = path.endsWith('/navigation')
      const navigationBatch = path.endsWith('/navigation/batch')
      const logicalPages = [], ownerControls = []
      const detail = /^\/api\/v1\/(projects|tasks)\/[^/]+$/.test(path)
      const receipt = { method: request.method(), path, parent: url.searchParams.get('parent_id'), kind: url.searchParams.get('parent_kind'), includeProjects: url.searchParams.get('include_projects') !== 'false', includeTasks: url.searchParams.get('include_tasks') !== 'false', projectOffset: Number(url.searchParams.get('project_offset') || 0), taskOffset: Number(url.searchParams.get('task_offset') || 0), query: url.searchParams.get('query') || '', showOnly: url.searchParams.get('show_only'), projectStatuses: url.searchParams.getAll('project_status'), includeBlocked: url.searchParams.get('include_blocked') === 'true', arrived: performance.now(), done: false }
      requests.push(receipt)
      if (navigation) { activeBranches++; maxBranches = Math.max(maxBranches, activeBranches) }
      if (detail) { activeDetails++; maxDetails = Math.max(maxDetails, activeDetails) }
      try {
      const scopeId = url.searchParams.get('workspace_id') || path.match(/^\/api\/v1\/workspaces\/([^/]+)/)?.[1] || (path === '/api/v1/change-stream/resync' ? request.postDataJSON().scope.workspace_id : null)
      const entityId = path.match(/^\/api\/v1\/(projects|tasks)\/([^/]+)/)?.[2]
      const fixture = fixtures.find(value => value.workspace.id === scopeId || (entityId && [...value.projects,...value.tasks].some(item => item.id === entityId))) || fixtures[0]
      if (navigation) receipt.viewport = await page.evaluate(() => { try { return JSON.parse(document.getElementById("hierarchy-viewport")?.dataset.hierarchyContext || "null") } catch { return null } })
        let value, status = 200
        if (path === '/api/v1/session') value = { auth_mode: 'local', principal: { actor_type: 'user', actor_id: principalId, actor_label: 'Fixture User', authority: 'local', grant_user_id: null }, global_permissions: { create_workspace: false, superadmin: false }, workspace: { workspace_id: fixture.workspace.id, role: 'owner', can_read: true, can_edit: true, can_admin: true } }
        else if (path === '/api/v1/change-stream/resync') { const body = request.postDataJSON(); assert.equal(body.scope.workspace_id, fixture.workspace.id); value = { snapshot_profile: 'workspace_shell_v1', items: workspaceShellSnapshotItems(fixture), has_more: false, resume_token: 'browser-shell' } }
        else if (path === '/api/v1/change-stream/ticket') value = { ticket: 'browser-ticket', expires_at: '2099-01-01T00:00:00Z' }
        else if (path === '/api/v1/workspaces') value = { items: fixtures.map(value => value.workspace), has_more: false }
        else if (path === '/api/v1/workspaces/' + fixture.workspace.id) value = fixture.workspace
        else if (path === '/api/v1/workspaces/' + fixture.workspace.id + '/planning-counts') value = {
          open_projects: fixture.projects.filter(row => row.workspace_id === fixture.workspace.id && !row.deleted_at && ['active','paused'].includes(row.status)).length,
          open_tasks: fixture.tasks.filter(row => row.workspace_id === fixture.workspace.id && !row.deleted_at && ['todo','in_progress','blocked'].includes(row.status)).length,
        }
        else if (path === '/api/v1/workspaces/' + fixture.workspace.id + '/memberships') value = { items: [], has_more: false }
        else if (path === '/api/v1/workspaces/' + fixture.workspace.id + '/timeline') value = { items: fixture.timelineEvents, has_more: false }
        else if (path === '/api/v1/workspaces/' + fixture.workspace.id + '/timeline/buckets') value = { workspace_id: fixture.workspace.id, since: url.searchParams.get('since') || '', until: url.searchParams.get('until'), bucket: url.searchParams.get('bucket'), buckets: [], next_since: null }
        else if (navigation) {
          assert.equal(path, '/api/v1/workspaces/' + fixture.workspace.id + '/navigation')
          assert.equal(url.searchParams.get('project_limit'), '50'); assert.equal(url.searchParams.get('task_limit'), '50')
          assert.ok(['workspace_root', 'project', 'task'].includes(receipt.kind)); assert.ok(receipt.projectOffset % 50 === 0 && receipt.taskOffset % 50 === 0)
          value = navigationBranchResponse(fixture, { parentKind: receipt.kind, parentId: receipt.parent, projectLimit: 50, taskLimit: 50, projectOffset: receipt.projectOffset, taskOffset: receipt.taskOffset, query: receipt.query, showOnly: url.searchParams.get('show_only'), showEmptyProjects: url.searchParams.get('show_empty_projects') !== 'false', projectStatuses: url.searchParams.getAll('project_status'), taskStatuses: url.searchParams.getAll('task_status'), priorityMode: url.searchParams.get('priority_mode'), priorityValue: url.searchParams.get('priority_value') })
          // Mirror the API's skipped-page contract; default omitted demand to both.
          if (!receipt.includeProjects) value.projects = { items: [], has_more: false }
          if (!receipt.includeTasks) value.tasks = { items: [], has_more: false }
          receipt.projectItems = value.projects.items.length; receipt.taskItems = value.tasks.items.length
          receipt.projectHasMore = value.projects.has_more; receipt.taskHasMore = value.tasks.has_more
        }
        else if (path.endsWith('/navigation/summaries')) { const body = request.postDataJSON(); receipt.projectIds = body.project_ids; receipt.taskIds = body.task_ids; value = navigationSummariesResponse(fixture, body.project_ids, body.task_ids) }
        else if (navigationBatch) {
          assert.equal(request.method(), 'POST')
          const body = request.postDataJSON(), descriptors = navigationBatchDescriptors(body, request.url(), { allowSyntheticIds: true })
          value = navigationBatchResponse(fixture, body, request.url(), { allowSyntheticIds: true })
          receipt.body = body; receipt.ownerPages = logicalPages
          activeBranches += descriptors.length; maxBranches = Math.max(maxBranches, activeBranches)
          const viewport = await page.evaluate(() => { try { return JSON.parse(document.getElementById('hierarchy-viewport')?.dataset.hierarchyContext || 'null') } catch { return null } })
          for (const [index, descriptor] of descriptors.entries()) {
            const options = descriptor.options, outcome = value.outcomes[index]
            const logical = { method: 'POST', path: path.slice(0, -6), logical: true, counted: false, transportIndex: requests.indexOf(receipt), requestId: descriptor.requestId,
              parent: options.parentId, kind: options.parentKind, includeProjects: options.includeProjects, includeTasks: options.includeTasks,
              projectOffset: options.projectOffset, taskOffset: options.taskOffset, query: options.query || '', showOnly: options.showOnly,
              projectStatuses: options.projectStatuses, viewport, arrived: receipt.arrived, done: false }
            const control = controls.find(candidate => !candidate.used && candidate.match(logical))
            if (control) { control.used = true; control.receipt = logical; ownerControls.push(control)
              if (control.error) { outcome.status = control.error; delete outcome.response; outcome.error = 'Controlled failure' }
              else if (control.transform && outcome.response) outcome.response = control.transform(outcome.response)
            }
            logical.status = outcome.status
            if (outcome.response) { logical.projectItems = outcome.response.projects.items.length; logical.taskItems = outcome.response.tasks.items.length
              logical.projectIds = outcome.response.projects.items.map(item => item.id); logical.taskIds = outcome.response.tasks.items.map(item => item.id)
              logical.projectHasMore = outcome.response.projects.has_more; logical.taskHasMore = outcome.response.tasks.has_more }
            logicalPages.push(logical); requests.push(logical)
          }
        }
        else if (path.includes('/navigation/focus/')) { const parts = path.split('/'); value = navigationFocusResponse(fixture, parts.at(-2), parts.at(-1), Number(url.searchParams.get('ancestor_offset') || 0)) }
        else if (/^\/api\/v1\/projects\/[^/]+\/overview$/.test(path)) value = projectOverviewResponse(fixture, path.split('/')[4])
        else if (/^\/api\/v1\/projects\/[^/]+\/next-task-page$/.test(path)) {
          receipt.offset = Number(url.searchParams.get('offset') || 0)
          const limit = Number(url.searchParams.get('limit') || 50)
          const values = projectNextTasksResponse(fixture, entityId, receipt.includeBlocked, limit + 1, receipt.offset, true)
          value = { items: values.slice(0, limit), has_more: values.length > limit }
        }
        else if (/^\/api\/v1\/tasks\/[^/]+\/dependencies$/.test(path) && request.method() === 'GET') {
          const dependencies = taskOverviewResponse(fixture, entityId).dependencies
          const offset = Number(url.searchParams.get('offset') || 0), limit = Number(url.searchParams.get('limit') || 50)
          value = { items: dependencies.slice(offset, offset + limit), has_more: dependencies.length > offset + limit }
        }
        else if (/^\/api\/v1\/projects\/[^/]+\/next-tasks$/.test(path)) {
          value = projectNextTasksResponse(fixture, entityId, receipt.includeBlocked, Number(url.searchParams.get('limit')))
        }
        else if (/^\/api\/v1\/tasks\/[^/]+\/overview$/.test(path)) value = taskOverviewResponse(fixture, path.split('/')[4])
        else if (detail) {
          value = (path.includes('/projects/') ? fixture.projects : fixture.tasks).find(item => item.id === path.split('/')[4])
          if (request.method() === 'PUT') {
            const update = request.postDataJSON(); assert.equal(typeof update.request_id, 'string'); receipt.update = update
            if (path.includes('/projects/') && authority) value = authority.updateProject(fixture, entityId, update)
            else if (path.includes('/tasks/') && authority?.updateTask) value = authority.updateTask(fixture, entityId, update)
            else { assert.deepEqual(Object.keys(update).sort(), ['request_id','title']); value.title = update.title }
          }
        }
        else if (path === '/api/v1/observations/discovery') { receipt.payload = request.postDataJSON(); const outcome = discovery.handle(receipt.payload); value = outcome.body; status = outcome.status; receipt.discovery = outcome.receipt }
        else if (path === '/api/v1/observations/count') value = fixtureObservationCounts(fixture, request.postDataJSON())
        else if (path === '/api/v1/observations' || path.endsWith('/subject-facets')) value = { items: [], has_more: false }
        else { unhandled.push(request.method() + ' ' + path); status = 501; value = { error: 'Unhandled controlled route' } }
        const control = controls.find(candidate => !candidate.used && candidate.match(receipt))
        if (control) { control.used = true; control.receipt = receipt; if (control.error) { status = control.error; value = { error: 'Controlled failure' } }; if (control.transform) value = control.transform(value) }
        if (navigation && status < 400) { receipt.projectIds = value.projects.items.map(item => item.id); receipt.taskIds = value.tasks.items.map(item => item.id); receipt.projectHasMore = value.projects.has_more; receipt.taskHasMore = value.tasks.has_more }
        receipt.status = status
        if (receipt.discovery) { receipt.discovery.status = status; receipt.discovery.response = value }
        const responseBody = JSON.stringify(value)
        receipt.bytes = Buffer.byteLength(responseBody)
        for (const ownerControl of ownerControls) { ownerControl.arrivedResolve(ownerControl.receipt); if (ownerControl.hold) held.add(ownerControl.release) }
        await Promise.all(ownerControls.filter(ownerControl => ownerControl.hold).map(ownerControl => ownerControl.wait.finally(() => held.delete(ownerControl.release))))
        if (control) { control.arrivedResolve(receipt); if (control.hold) { held.add(control.release); await control.wait; held.delete(control.release) } }
        await route.fulfill({ status, contentType: 'application/json', body: responseBody })
        if (authority && request.method() === 'PUT' && path.includes('/projects/')) await authority.broadcast()
      } catch (error) { errors.push(error.message); await route.abort().catch(() => {}) }
      finally { receipt.done = true; if (receipt.discovery) receipt.discovery.done = true; for (const logical of logicalPages) logical.done = true; activeBranches -= logicalPages.length; if (navigation) activeBranches--; if (detail) activeDetails-- }
    })
    const settleScroll = () => page.evaluate(async () => { const scroll = document.getElementById('main-content-scroll'); let previous = null, stable = 0; const deadline = performance.now() + 1000; while (performance.now() < deadline && stable < 3) { await new Promise(resolve => requestAnimationFrame(() => requestAnimationFrame(resolve))); const geometry = [scroll.scrollTop, scroll.scrollHeight].join(':'); stable = geometry === previous ? stable + 1 : 0; previous = geometry }; if (stable < 3) throw new Error('Scroll geometry did not settle before complete logical scan');  })
    const origin = 'http://127.0.0.1:' + server.address().port
    return { page, fixture, discovery, requests, errors, unhandled, origin, bounded, setPrincipal(id) { principalId = id },
      gate(match, options = {}) { let release, arrivedResolve; const wait = new Promise(resolveRelease => { release = resolveRelease }); const arrived = new Promise(resolveArrival => { arrivedResolve = resolveArrival }); const control = { match, ...options, wait, release, arrived, arrivedResolve, used: false }; controls.push(control); return control },
      async start(fragment = 'tab=projects', anchor = 'root-project') { await page.goto(origin + '/workspace/' + fixture.workspace.id + '#' + fragment); try { await page.locator('#entity-' + anchor).waitFor({ timeout: 10000 }) } catch (error) { throw new Error(error.message + '\n' + JSON.stringify({ errors, navigation: requests.filter(r=>r.kind).slice(0,8), geometry:await page.evaluate(()=>({top:document.getElementById('main-content-scroll')?.scrollTop,keys:[...document.querySelectorAll('[data-hierarchy-key]')].slice(0,4).map(row=>row.dataset.hierarchyKey)})), text: (await page.locator('body').innerText()).slice(0, 1200) })) } },
      async idle() { await page.waitForFunction(() => !document.querySelector('.loading-indicator')); const deadline = Date.now() + 10000; let quiet = 0, count = requests.length; while (Date.now() < deadline) { if (!activeBranches && !activeDetails && requests.every(r => r.done) && requests.length === count) { quiet++; if (quiet >= 5) break } else quiet = 0; count = requests.length; await page.waitForTimeout(20) }; assert.ok(quiet >= 5 && activeBranches === 0 && activeDetails === 0 && requests.every(r => r.done), 'Controlled routes must reach stable quiet: ' + JSON.stringify({ quiet, activeBranches, activeDetails, unfinished: requests.filter(r => !r.done).slice(-8) })); assert.deepEqual(errors, []); assert.deepEqual(unhandled, []) },
      async scrollTo(key) { await settleScroll(); const deadline = Date.now() + 15000; await page.locator('#main-content-scroll').evaluate(element => { element.scrollTop = 0 }); await page.evaluate(() => new Promise(resolve => requestAnimationFrame(() => requestAnimationFrame(resolve)))); while (Date.now() < deadline) { const row = page.locator('[data-hierarchy-key="' + key + '"]'); if (await page.evaluate(key => { const element = document.querySelector('[data-hierarchy-key="' + key + '"]'); if (!element) return false; const scroll = document.getElementById('main-content-scroll'); scroll.scrollTop += element.getBoundingClientRect().top - scroll.getBoundingClientRect().top; return true }, key)) { await page.evaluate(() => new Promise(resolvePaint => requestAnimationFrame(() => requestAnimationFrame(resolvePaint)))); if (await row.count()) return row }; const end = await page.locator('#main-content-scroll').evaluate(element => { const previous = element.scrollTop; element.scrollTop += 600; return previous === element.scrollTop }); await page.evaluate(() => new Promise(resolve => requestAnimationFrame(() => requestAnimationFrame(resolve)))); if (end) { if (await row.count()) return row; break } }; throw new Error('Logical row was not scroll-reachable: ' + key + ' ' + JSON.stringify(await page.evaluate(() => { const scroll = document.getElementById('main-content-scroll'); return { top: scroll.scrollTop, height: scroll.scrollHeight, viewport: scroll.clientHeight, context: document.getElementById('hierarchy-viewport')?.dataset.hierarchyContext, keys: [...document.querySelectorAll('[data-hierarchy-key]')].map(row => row.dataset.hierarchyKey) } }))) },
      async logicalKeys() { const keys = new Set(); await settleScroll(); await page.locator('#main-content-scroll').evaluate(element => { element.scrollTop = 0 }); const deadline = Date.now() + 15000; let end = false; while (Date.now() < deadline) { await page.evaluate(() => new Promise(resolvePaint => requestAnimationFrame(() => requestAnimationFrame(resolvePaint)))); for (const key of await page.locator('[data-hierarchy-key]').evaluateAll(rows => rows.map(row => row.dataset.hierarchyKey))) keys.add(key); if (end) return keys; end = await page.locator('#main-content-scroll').evaluate(element => { const before = element.scrollTop; element.scrollTop += element.clientHeight / 2; return before === element.scrollTop }) }; throw new Error('Logical scan exceeded finite deadline') },
      caps() { return { maxBranches, maxDetails, activeBranches, activeDetails } },
      async close() { await retire() }
    }
  } catch (error) { await retire(); throw error }
}
