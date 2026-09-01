import { createHash } from 'node:crypto'
import { execFileSync } from 'node:child_process'
import fs from 'node:fs'
import http from 'node:http'
import os from 'node:os'
import path from 'node:path'
import { fileURLToPath } from 'node:url'
import { chromium } from '@playwright/test'
import { assertFiveSamples, BASE_COMMIT, HARNESS_CONFIGURATION, hashJson, liveSettleReady, liveTimingSummary, liveWholeWorkspaceReload, median, nearestRankP95, perfApiRouteKey, renderBudgetEvaluation, representativeReadiness, transportContractReady } from './contracts.mjs'
import { DIRECT_FOCUS_CONTRACT, OBSERVATION_MEASURED_QUERY, TIMELINE_BROWSER_NOW, TIMELINE_DEFAULT_UI_QUERY, deepFocusFixture, directFocusFixture, fixtureHash, generateFixture, navigationBranchResponse, navigationFocusResponse, navigationSummariesResponse, paginate, projectOverviewResponse, queryObservationFacets, queryObservations, queryProjects, queryTasks, queryTimelineBuckets, queryTimelineEvents, snapshotHash, snapshotItems, taskOverviewResponse, workspaceShellSnapshotItems } from './fixtures.mjs'

const here = path.dirname(fileURLToPath(import.meta.url))
const frontendRoot = path.dirname(here)
const staticRoot = path.resolve(frontendRoot, '..', 'static')
const baselinePath = path.join(here, 'baseline.v1.json')
const traceManifestPath = path.join(here, 'trace-manifest.v1.json')
const budgets = JSON.parse(fs.readFileSync(path.join(here, 'budgets.v1.json'), 'utf8'))
const mode = process.argv[2] || 'check'
const recordAuthorized = process.argv.includes('--authorize-baseline')
const afterArtifactAuthorized = process.argv.includes('--authorize-after-artifact')
const outputArgument = process.argv.indexOf('--output')
const traceOutputArgument = process.argv.indexOf('--trace-output')
// A task-local evidence run can name its immutable review base without
// changing the approved baseline or the default compatibility record.
const evidenceBaseCommit = process.env.HMEM_EVIDENCE_BASE_COMMIT || '818cc3cc634bfa8c0ebe14c13fc70b4c2d059e83'
const TASK_2503_BASE_COMMIT = '04fd7ba26b1a63b5f1c601939b046ef2fe5f51d6'
const evidenceRevision = evidenceBaseCommit === TASK_2503_BASE_COMMIT ? 'v2' : 'v1'
const legacyAfterArtifactPath = path.join(here, 'final-working-tree.after.v1.json')
const legacyAfterTraceArtifactPath = path.join(here, 'final-working-tree.trace-manifest.v1.json')
const evidenceManifestPath = path.join(here, `final-working-tree.evidence-manifest.${evidenceRevision}.json`)
const evidenceDiffPath = path.join(here, 'final-working-tree.complete.diff')
const validationRecordPath = path.join(here, `final-working-tree.validation-record.${evidenceRevision}.json`)
const afterArtifactPath = path.join(here, `final-working-tree.after.${evidenceRevision}.json`)
const afterTraceArtifactPath = path.join(here, `final-working-tree.trace-manifest.${evidenceRevision}.json`)
const requestedRecordOutputPath = outputArgument === -1 ? baselinePath : path.resolve(frontendRoot, process.argv[outputArgument + 1] || '')
const requestedRecordTraceManifestPath = traceOutputArgument === -1 ? traceManifestPath : path.resolve(frontendRoot, process.argv[traceOutputArgument + 1] || '')
// Keep the v1 package command usable for the previous task while preventing it
// from recreating v1 evidence when the 04fd review subject is selected.
const recordOutputPath = evidenceRevision === 'v2' && requestedRecordOutputPath === legacyAfterArtifactPath ? afterArtifactPath : requestedRecordOutputPath
const recordTraceManifestPath = evidenceRevision === 'v2' && requestedRecordTraceManifestPath === legacyAfterTraceArtifactPath ? afterTraceArtifactPath : requestedRecordTraceManifestPath
const WARMUPS = HARNESS_CONFIGURATION.warmups
const PRODUCTION_SNAPSHOT_PROFILE = 'workspace_shell_v1'
const SAMPLES = HARNESS_CONFIGURATION.samples
const tracePath = path.join(here, '.artifacts', 'large-baseline-trace.zip')

if (!['record', 'check'].includes(mode)) throw new Error('usage: node perf/harness.mjs <record|check> [--output path --trace-output path]')
if ((outputArgument !== -1 && !process.argv[outputArgument + 1]) || (traceOutputArgument !== -1 && !process.argv[traceOutputArgument + 1])) throw new Error('--output and --trace-output require paths')
if (mode === 'record' && recordOutputPath === baselinePath && !recordAuthorized) throw new Error('record mode overwrites the approved baseline; rerun with --authorize-baseline')
if (mode === 'record' && recordOutputPath !== baselinePath && !afterArtifactAuthorized) throw new Error('after-artifact record requires --authorize-after-artifact and must not overwrite baseline.v1.json')

function platformExecutable(name) {
  return process.platform === 'win32' ? `${name}.cmd` : name
}

function requiredCommandVersion(label, command, args) {
  try {
    const executable = process.platform === 'win32' && command.toLowerCase().endsWith('.cmd') ? (process.env.ComSpec || 'cmd.exe') : command
    const parameters = executable === command ? args : ['/d', '/s', '/c', command, ...args]
    const version = execFileSync(executable, parameters, { cwd: frontendRoot, encoding: 'utf8', stdio: ['ignore', 'pipe', 'pipe'] }).trim()
    if (!version) throw new Error('empty version output')
    return version
  } catch (error) {
    throw new Error(`required ${label} version probe failed (${command} ${args.join(' ')}): ${error.message}`)
  }
}

function runTaskEvidencePrerequisites() {
  if (mode !== 'record' || evidenceRevision !== 'v2' || !afterArtifactAuthorized) return
  const command = platformExecutable('npm')
  const executable = process.platform === 'win32' && command.toLowerCase().endsWith('.cmd') ? (process.env.ComSpec || 'cmd.exe') : command
  const parameters = executable === command ? ['run', 'perf:self-check'] : ['/d', '/s', '/c', command, 'run', 'perf:self-check']
  execFileSync(executable, parameters, { cwd: frontendRoot, stdio: 'inherit' })
}

function sha256File(file) {
  return createHash('sha256').update(fs.readFileSync(file)).digest('hex')
}

function normalizedRepositoryPath(file) {
  return path.relative(path.resolve(frontendRoot, '..', '..'), file).split(path.sep).join('/')
}

function untrackedFileDiff(repositoryRoot, relativePath) {
  try {
    return execFileSync('git', ['diff', '--binary', '--no-index', '--no-ext-diff', '--src-prefix=a/', '--dst-prefix=b/', '--', '/dev/null', relativePath], { cwd: repositoryRoot })
  } catch (error) {
    if (error.status === 1 && error.stdout) return error.stdout
    throw error
  }
}

function finalWorkingTreeEvidence() {
  const repositoryRoot = path.resolve(frontendRoot, '..', '..')
  // Review subjects deliberately exclude generated evidence from their source
  // patch. Retain v1 alongside v2 here so a historical artifact cannot leak
  // into the 04fd complete diff merely because it remains untracked locally.
  const artifactPaths = new Set([
    legacyAfterArtifactPath,
    legacyAfterTraceArtifactPath,
    path.join(here, 'final-working-tree.evidence-manifest.v1.json'),
    afterArtifactPath,
    afterTraceArtifactPath,
    evidenceManifestPath,
    evidenceDiffPath,
    validationRecordPath
  ].map(normalizedRepositoryPath))
  const trackedDiff = execFileSync('git', ['diff', '--binary', '--no-ext-diff', evidenceBaseCommit, '--'], { cwd: repositoryRoot })
  const trackedPaths = execFileSync('git', ['diff', '--name-only', evidenceBaseCommit, '--'], { cwd: repositoryRoot, encoding: 'utf8' })
    .trim().split(/\r?\n/).filter(Boolean)
  const untrackedPaths = execFileSync('git', ['ls-files', '--others', '--exclude-standard'], { cwd: repositoryRoot, encoding: 'utf8' })
    .trim().split(/\r?\n/).filter(Boolean)
    .filter(relative => !artifactPaths.has(relative))
  const diff = Buffer.concat([trackedDiff, ...untrackedPaths.map(relative => untrackedFileDiff(repositoryRoot, relative))])
  fs.writeFileSync(evidenceDiffPath, diff)
  const baselineAtBase = execFileSync('git', ['show', `${evidenceBaseCommit}:hmem-server/frontend/perf/baseline.v1.json`], { cwd: repositoryRoot })
  const baselineNow = fs.readFileSync(baselinePath)
  const changedPaths = [...new Set([...trackedPaths, ...untrackedPaths])]
    .map(relative => path.join(repositoryRoot, relative))
    .filter(fs.existsSync)
    .map(file => ({ path: normalizedRepositoryPath(file), sha256: sha256File(file) }))
  // `check` has no record CLI output arguments, so its final manifest refresh
  // must still hash the selected task-local after artifacts rather than the
  // immutable baseline inputs.
  const evidenceRecordPath = evidenceRevision === 'v2' ? afterArtifactPath : recordOutputPath
  const evidenceTracePath = evidenceRevision === 'v2' ? afterTraceArtifactPath : recordTraceManifestPath
  const artifacts = [evidenceRecordPath, evidenceTracePath, evidenceDiffPath]
    .concat(fs.existsSync(validationRecordPath) ? [validationRecordPath] : [])
    .map(file => ({ path: normalizedRepositoryPath(file), sha256: sha256File(file), sizeBytes: fs.statSync(file).size }))
  const manifest = {
    schemaVersion: 1,
    taskId: evidenceRevision === 'v2' ? '2503e08f-ff82-4f2c-adff-24e14fec8299' : null,
    evidenceBaseCommit,
    normalization: 'repository-relative POSIX paths; SHA-256 of exact file bytes; binary Git diff without external diff drivers; untracked source files represented by deterministic no-index additions; generated evidence artifacts separately hash-listed',
    recipe: `HMEM_EVIDENCE_BASE_COMMIT=${evidenceBaseCommit} npm run perf:record-after`,
    equality: {
      baselinePath: normalizedRepositoryPath(baselinePath),
      source: `${evidenceBaseCommit}:hmem-server/frontend/perf/baseline.v1.json`,
      byteForByteEqual: baselineAtBase.equals(baselineNow),
      baseSha256: createHash('sha256').update(baselineAtBase).digest('hex'),
      workingTreeSha256: sha256File(baselinePath)
    },
    artifacts,
    changedPaths,
    completeDiff: { path: normalizedRepositoryPath(evidenceDiffPath), sha256: sha256File(evidenceDiffPath), sizeBytes: fs.statSync(evidenceDiffPath).size }
  }
  if (!manifest.equality.byteForByteEqual) throw new Error('immutable baseline differs from evidence base')
  fs.writeFileSync(evidenceManifestPath, `${JSON.stringify(manifest, null, 2)}\n`)
}

function fixtureResponder(fixture, tracker, options = {}) {
  const snapshots = options.snapshots || snapshotItems(fixture)
  const resyncGate = options.resyncGate || null
  const focusContinuationPause = options.focusContinuationPause || null
  const projects = new Map(fixture.projects.map(item => [item.id, item]))
  const tasks = new Map(fixture.tasks.map(item => [item.id, item]))
  const observations = new Map(fixture.observations.map(item => [item.id, item]))
  const reply = async (route, request, value, status = 200, explicitKey = null) => {
    const body = value == null ? '' : JSON.stringify(value)
    const key = explicitKey || perfApiRouteKey(request.url(), request.method())
    if (!key) throw new Error(`implemented perf response has no route contract: ${request.method()} ${request.url()}`)
    tracker.active += 1
    tracker.lastActivityAt = performance.now()
    tracker.requests.push({ key, method: request.method(), url: request.url(), bytes: Buffer.byteLength(body), at: performance.now() })
    tracker.counts[key] = (tracker.counts[key] || 0) + 1
    tracker.bytes[key] = (tracker.bytes[key] || 0) + Buffer.byteLength(body)
    try {
      await route.fulfill({ status, contentType: 'application/json', body })
    } finally {
      if (status < 400 && key === 'change-stream:resync') {
        const requestBody = request.postDataJSON()
        if (!requestBody.page_token) {
          tracker.model.snapshotGeneration += 1
          tracker.model.snapshotItems = 0
          tracker.model.snapshotPages = 0
          tracker.model.snapshotComplete = false
          tracker.model.snapshotProfile = value.snapshot_profile
        }
        tracker.model.snapshotItems += value.items.length
        tracker.model.snapshotPages += 1
        tracker.model.snapshotComplete = value.has_more === false
      }
      if (status < 400 && key === 'timeline:events') tracker.model.timelineEvents = value.items.length
      if (status < 400 && key === 'timeline:buckets') {
        tracker.model.timelineBuckets = value.buckets.length
        tracker.model.timelineBucketRequest = { since: value.since, until: value.until, bucket: value.bucket }
      }
      tracker.active -= 1
      tracker.completed += 1
      tracker.lastActivityAt = performance.now()
    }
  }
  return async route => {
    const request = route.request()
    const url = new URL(request.url())
    const pathname = url.pathname
    const contractedKey = perfApiRouteKey(url, request.method())
    if (!contractedKey) {
      const exact = `${request.method()} ${request.url()}`
      tracker.unhandledApiRoutes.push(exact)
      return reply(route, request, { error: `unimplemented perf API route: ${exact}` }, 501, `unimplemented:${request.method()}:${pathname}`)
    }
    if (pathname === '/api/v1/session') return reply(route, request, {
      auth_mode: 'local', principal: { actor_type: 'user', actor_id: 'perf-user', actor_label: 'Performance User', authority: 'local', grant_user_id: null },
      global_permissions: { create_workspace: false, superadmin: false },
      workspace: { workspace_id: fixture.workspace.id, role: 'owner', can_read: true, can_edit: true, can_admin: true }
    })
    if (pathname === '/api/v1/change-stream/resync') {
      const body = request.postDataJSON()
      const profile = body.snapshot_profile || 'full_v1'
      const selectedSnapshots = profile === 'workspace_shell_v1' ? workspaceShellSnapshotItems(fixture) : snapshots
      const offset = body.page_token ? Number(String(body.page_token).split(':')[1]) : 0
      const pageSize = Number(body.page_size) || 100
      if (resyncGate && !resyncGate.used && offset === resyncGate.pauseOffset) {
        resyncGate.used = true
        resyncGate.signalPaused()
        await resyncGate.waitForRelease
      }
      const page = selectedSnapshots.slice(offset, offset + pageSize)
      const hasMore = offset + pageSize < selectedSnapshots.length
      return reply(route, request, { snapshot_profile: profile, items: page, has_more: hasMore, ...(hasMore ? { next_page_token: `offset:${offset + pageSize}` } : { resume_token: `fixture-${fixture.size}-resume` }) })
    }
    if (pathname === '/api/v1/change-stream/ticket') return reply(route, request, { ticket: `fixture-${fixture.size}-ticket`, expires_at: '2099-01-01T00:00:00Z' })
    if (pathname === '/api/v1/workspaces') return reply(route, request, paginate([fixture.workspace], url.searchParams.get('offset'), url.searchParams.get('limit')))
    if (pathname === `/api/v1/workspaces/${fixture.workspace.id}`) return reply(route, request, fixture.workspace)
    if (pathname === `/api/v1/workspaces/${fixture.workspace.id}/navigation`) return reply(route, request, navigationBranchResponse(fixture, {
      parentKind: url.searchParams.get('parent_kind'), parentId: url.searchParams.get('parent_id'),
      projectLimit: url.searchParams.get('project_limit'), projectOffset: url.searchParams.get('project_offset'),
      taskLimit: url.searchParams.get('task_limit'), taskOffset: url.searchParams.get('task_offset'),
      showOnly: url.searchParams.get('show_only'), projectStatuses: url.searchParams.getAll('project_status'), taskStatuses: url.searchParams.getAll('task_status'),
      priorityMode: url.searchParams.get('priority_mode'), priorityValue: url.searchParams.get('priority_value'), query: url.searchParams.get('query')
    }))
    if (pathname === `/api/v1/workspaces/${fixture.workspace.id}/navigation/summaries`) {
      const body = request.postDataJSON()
      const value = navigationSummariesResponse(fixture, body.project_ids || [], body.task_ids || [])
      return reply(route, request, value || { error: 'invalid summary batch' }, value ? 200 : 400)
    }
    const navigationFocus = pathname.match(new RegExp(`^/api/v1/workspaces/${fixture.workspace.id}/navigation/focus/(project|task)/([^/]+)$`))
    if (navigationFocus) {
      const entityId = decodeURIComponent(navigationFocus[2])
      const ancestorOffset = Number(url.searchParams.get('ancestor_offset') || 0)
      const value = navigationFocusResponse(fixture, navigationFocus[1], entityId, ancestorOffset)
      if (focusContinuationPause && !focusContinuationPause.used && navigationFocus[1] === focusContinuationPause.entityType && entityId === focusContinuationPause.entityId && ancestorOffset === focusContinuationPause.ancestorOffset) {
        focusContinuationPause.used = true
        focusContinuationPause.signalPaused()
        await focusContinuationPause.waitForRelease
      }
      return reply(route, request, value || { error: 'not found' }, value ? 200 : 404)
    }
    if (pathname === `/api/v1/workspaces/${fixture.workspace.id}/memberships`) return reply(route, request, {
      items: [{ workspace_id: fixture.workspace.id, user_id: 'perf-user', role: 'owner', granted_by: null, created_at: fixture.workspace.created_at, updated_at: fixture.workspace.updated_at }], has_more: false
    })
    if (pathname === '/api/v1/projects') return reply(route, request, queryProjects(fixture, {
      status: url.searchParams.get('status'), offset: url.searchParams.get('offset'), limit: url.searchParams.get('limit')
    }))
    if (pathname === '/api/v1/tasks') return reply(route, request, queryTasks(fixture, {
      projectId: url.searchParams.get('project_id'), status: url.searchParams.get('status'), priority: url.searchParams.get('priority'),
      offset: url.searchParams.get('offset'), limit: url.searchParams.get('limit')
    }))
    if (pathname === '/api/v1/observations') return reply(route, request, queryObservations(fixture, {
      query: url.searchParams.get('query'), subjectKind: url.searchParams.get('subject_kind'), subject: url.searchParams.get('subject'), gitSha: url.searchParams.get('git_sha'),
      offset: url.searchParams.get('offset'), limit: url.searchParams.get('limit')
    }))
    if (pathname === '/api/v1/observations/subject-facets') return reply(route, request, queryObservationFacets(fixture, {
      query: url.searchParams.get('query'), subjectKind: url.searchParams.get('subject_kind'), gitSha: url.searchParams.get('git_sha'),
      offset: url.searchParams.get('offset'), limit: url.searchParams.get('limit')
    }))
    if (pathname === '/api/v1/observations/match') return reply(route, request, { items: [], has_more: false })
    const projectOverview = pathname.match(/^\/api\/v1\/projects\/([^/]+)\/overview$/)
    if (projectOverview) {
      return reply(route, request, projectOverviewResponse(fixture, decodeURIComponent(projectOverview[1])))
    }
    const taskOverview = pathname.match(/^\/api\/v1\/tasks\/([^/]+)\/overview$/)
    if (taskOverview) {
      return reply(route, request, taskOverviewResponse(fixture, decodeURIComponent(taskOverview[1])))
    }
    const projectEntity = pathname.match(/^\/api\/v1\/projects\/([^/]+)$/)
    if (projectEntity) {
      const value = projects.get(decodeURIComponent(projectEntity[1]))
      return reply(route, request, value || { error: 'project not found' }, value ? 200 : 404)
    }
    const taskEntity = pathname.match(/^\/api\/v1\/tasks\/([^/]+)$/)
    if (taskEntity) {
      const value = tasks.get(decodeURIComponent(taskEntity[1]))
      return reply(route, request, value || { error: 'task not found' }, value ? 200 : 404)
    }
    const observationEntity = pathname.match(/^\/api\/v1\/observations\/([^/]+)$/)
    if (observationEntity) {
      const value = observations.get(decodeURIComponent(observationEntity[1]))
      return reply(route, request, value || { error: 'observation not found' }, value ? 200 : 404)
    }
    if (pathname === `/api/v1/workspaces/${fixture.workspace.id}/timeline`) {
      return reply(route, request, queryTimelineEvents(fixture, {
        entityType: url.searchParams.get('entity_type'), eventType: url.searchParams.get('event_type'), since: url.searchParams.get('since'), until: url.searchParams.get('until'),
        offset: url.searchParams.get('offset'), limit: url.searchParams.get('limit')
      }))
    }
    if (pathname === `/api/v1/workspaces/${fixture.workspace.id}/timeline/buckets`) {
      try {
        return reply(route, request, queryTimelineBuckets(fixture, {
          since: url.searchParams.get('since'), until: url.searchParams.get('until'), bucket: url.searchParams.get('bucket') || 'week'
        }))
      } catch (error) {
        throw new Error(`Timeline bucket fixture request rejected like production API: ${error.message}`)
      }
    }
    const exact = `${request.method()} ${request.url()}`
    tracker.unhandledApiRoutes.push(exact)
    return reply(route, request, { error: `contracted but unimplemented perf API route: ${exact}` }, 501, `unimplemented:${request.method()}:${pathname}`)
  }
}

async function staticServer() {
  const server = http.createServer((request, response) => {
    const pathname = new URL(request.url, 'http://local').pathname
    if (pathname === '/hmem-runtime-config.js') {
      response.writeHead(200, { 'Content-Type': 'application/javascript', 'Cache-Control': 'no-store' })
      response.end("window.HMEM_CONFIG={authMode:'local',apiUrl:window.location.origin,wsUrl:'ws://perf.invalid/api/v1/ws',authTokenStorage:'memory'};")
      return
    }
    if (pathname === '/favicon.ico') {
      response.writeHead(204); response.end(); return
    }
    const relative = pathname === '/' || pathname.startsWith('/workspace/') ? 'index.html' : pathname.replace(/^\//, '')
    const file = path.resolve(staticRoot, relative)
    if (!file.startsWith(staticRoot) || !fs.existsSync(file) || !fs.statSync(file).isFile()) {
      response.writeHead(404); response.end('not found'); return
    }
    const contentType = file.endsWith('.html') ? 'text/html' : file.endsWith('.js') ? 'application/javascript' : file.endsWith('.css') ? 'text/css' : 'application/octet-stream'
    response.writeHead(200, { 'Content-Type': contentType, 'Cache-Control': 'no-store' })
    fs.createReadStream(file).pipe(response)
  })
  await new Promise(resolve => server.listen(0, '127.0.0.1', resolve))
  return { server, origin: `http://127.0.0.1:${server.address().port}` }
}

const fakeWebSocketScript = `
(() => {
  Date.now = () => ${Date.parse(TIMELINE_BROWSER_NOW)};
  const sockets = [];
  class PerfWebSocket {
    static OPEN = 1; static CONNECTING = 0; static CLOSED = 3;
    constructor(url) { this.url=url; this.readyState=0; sockets.push(this); setTimeout(() => { this.readyState=1; if (this.onopen) this.onopen({type:'open'}); setTimeout(() => this.__emit({schema_version:1,type:'checkpoint',catch_up:'complete',resume_token:'fixture-live'}), 0); }, 0); }
    send() {}
    close() { this.readyState=3; if (this.onclose) this.onclose({code:1000,reason:'fixture'}); }
    addEventListener(type, fn) { this['on'+type]=fn; }
    removeEventListener(type, fn) { if (this['on'+type]===fn) this['on'+type]=null; }
    __emit(frame) { if (this.readyState===1 && this.onmessage) this.onmessage({data:JSON.stringify(frame)}); }
  }
  window.WebSocket=PerfWebSocket;
  window.__perfPushFrames=(frames) => { const open=sockets.filter(socket => socket.readyState===1); const marker={frames:frames.length,sockets:open.length,resumeToken:'fixture-live-after',dispatchTurnComplete:false}; window.__perfLastPush=marker; for (const frame of frames) for (const socket of open) socket.__emit(frame); for (const socket of open) socket.__emit({schema_version:1,type:'checkpoint',catch_up:'complete',resume_token:'fixture-live-after'}); setTimeout(() => { marker.dispatchTurnComplete=true; }, 0); return open.length; };
})();`

async function requiredDoubleFrame(page, selector) {
  return page.evaluate(async value => {
    const element = document.querySelector(value)
    if (!element) throw new Error('required interaction target missing: ' + value)
    const start = performance.now()
    element.click()
    await new Promise(resolve => requestAnimationFrame(() => requestAnimationFrame(resolve)))
    return performance.now() - start
  }, selector)
}

async function requiredDoubleFrameByText(page, selector, label) {
  return page.evaluate(async ({ selector, label }) => {
    const element = Array.from(document.querySelectorAll(selector)).find(value => value.textContent.trim() === label)
    if (!element) throw new Error('required interaction target missing: ' + selector + ' text=' + label)
    const start = performance.now()
    element.click()
    await new Promise(resolve => requestAnimationFrame(() => requestAnimationFrame(resolve)))
    return performance.now() - start
  }, { selector, label })
}

function assertNoUnhandledApiRoutes(tracker) {
  if (tracker.unhandledApiRoutes.length > 0) {
    throw new Error(`unimplemented perf API route(s): ${tracker.unhandledApiRoutes.join(', ')}`)
  }
}

function representativeProjectAnchors(fixture) {
  const statusOrder = { active: 0, paused: 1, completed: 2, archived: 3 }
  const roots = fixture.projects
    .filter(project => project.parent_id == null)
    .sort((left, right) => (statusOrder[left.status] - statusOrder[right.status]) || (right.priority - left.priority) || left.name.localeCompare(right.name))
  if (roots.length === 0) throw new Error('fixture requires a root project anchor')
  const first = roots[0]
  // The bounded bootstrap deliberately owns just the root slice. Tree-child
  // rendering belongs to the follow-on lazy-branch scenario, not cold readiness.
  return { first: first.id, expanded: first.id, selected: fixture.projects.at(-1).id }
}

async function waitForTransportQuiescence(page, tracker, label, quietMs = 250, timeoutMs = 30000) {
  const deadline = performance.now() + timeoutMs
  while (performance.now() < deadline) {
    assertNoUnhandledApiRoutes(tracker)
    if (tracker.active === 0 && performance.now() - tracker.lastActivityAt >= quietMs) {
      await page.evaluate(() => new Promise(resolve => requestAnimationFrame(() => requestAnimationFrame(resolve))))
      if (tracker.active === 0 && performance.now() - tracker.lastActivityAt >= quietMs) return
    }
    await page.waitForTimeout(25)
  }
  throw new Error(`${label} did not reach transport/UI quiescence within ${timeoutMs}ms (active=${tracker.active})`)
}

async function projectUiSignals(page, anchorId) {
  return page.evaluate(id => ({
    loading: Boolean(document.querySelector('.loading-indicator')),
    focused: Boolean(document.querySelector('.focus-breadcrumb-bar')),
    anchorVisible: Boolean(document.querySelector(`#entity-${id}`))
  }), anchorId)
}

async function waitForWorkspaceReady(page, tracker, fixture, anchorId, label) {
  const expectedSnapshotItems = workspaceShellSnapshotItems(fixture).length
  const expectedPages = Math.ceil(expectedSnapshotItems / DIRECT_FOCUS_CONTRACT.pageSize)
  const modelReady = () => transportContractReady({ protocol: 'bounded', transportedItems: tracker.model.snapshotItems, expectedItems: expectedSnapshotItems, fullBackingItems: snapshotItems(fixture).length, transportedPages: tracker.model.snapshotPages, expectedPages, complete: tracker.model.snapshotComplete })
  const deadline = performance.now() + 30000
  while (performance.now() < deadline) {
    assertNoUnhandledApiRoutes(tracker)
    const ui = await projectUiSignals(page, anchorId)
    if (representativeReadiness({
      modelComplete: modelReady(),
      activeRequests: tracker.active,
      loading: ui.loading,
      focused: ui.focused,
      anchorVisible: ui.anchorVisible
    })) {
      await waitForTransportQuiescence(page, tracker, label)
      const stableUi = await projectUiSignals(page, anchorId)
      if (representativeReadiness({ modelComplete: modelReady(), activeRequests: tracker.active, loading: stableUi.loading, focused: stableUi.focused, anchorVisible: stableUi.anchorVisible })) return
    }
    await page.waitForTimeout(25)
  }
  throw new Error(`${label} readiness failed: ${tracker.model.snapshotProfile} snapshot ${tracker.model.snapshotItems}/${expectedSnapshotItems} shell items, ${tracker.model.snapshotPages}/${expectedPages} pages, complete=${tracker.model.snapshotComplete}, navigation=${tracker.counts['navigation:branch'] || 0}, active=${tracker.active}, anchor=${anchorId}`)
}

async function restoreWorkspace(page, tracker, fixture, anchorId, label) {
  await page.evaluate(() => { window.location.hash = 'tab=projects' })
  await waitForWorkspaceReady(page, tracker, fixture, anchorId, label)
}

async function waitForLiveSettle(page, tracker, before, anchorId, timeoutMs = 30000) {
  const deadline = performance.now() + timeoutMs
  while (performance.now() < deadline) {
    assertNoUnhandledApiRoutes(tracker)
    const ui = await projectUiSignals(page, anchorId)
    const dispatchTurnComplete = await page.evaluate(() => window.__perfLastPush?.dispatchTurnComplete === true)
    const followUpRequests = tracker.requests.length - before.index
    if (liveSettleReady({ dispatchTurnComplete, activeRequests: tracker.active, loading: ui.loading, focused: ui.focused, anchorVisible: ui.anchorVisible, followUpRequests })) {
      await page.evaluate(() => new Promise(resolve => requestAnimationFrame(() => requestAnimationFrame(resolve))))
      if (tracker.active === 0) return performance.now()
    }
    await page.waitForTimeout(10)
  }
  throw new Error(`live batch did not settle within ${timeoutMs}ms`)
}

async function assertLiveStability(page, tracker, durationMs = 500) {
  const startedAt = performance.now()
  const requestCount = tracker.requests.length
  const completed = tracker.completed
  await page.waitForTimeout(durationMs)
  await page.evaluate(() => new Promise(resolve => requestAnimationFrame(() => requestAnimationFrame(resolve))))
  assertNoUnhandledApiRoutes(tracker)
  if (tracker.active !== 0 || tracker.requests.length !== requestCount || tracker.completed !== completed) {
    const late = tracker.requests.slice(requestCount).map(request => `${request.method} ${request.url}`)
    throw new Error(`late live activity during ${durationMs}ms stability window: ${late.join(', ') || `active=${tracker.active}`}`)
  }
  return { startedAt, endedAt: performance.now(), requests: 0 }
}

async function domMetrics(page) {
  return page.evaluate(() => ({
    nodes: document.querySelectorAll('*').length,
    rows: document.querySelectorAll('.card-project,.card-task,.card-subtask,.observation-card,.timeline-event-card,.timeline-value-table tbody tr').length,
    projects: document.querySelectorAll('.card-project').length,
    tasks: document.querySelectorAll('.card-task,.card-subtask').length,
    observations: document.querySelectorAll('.observation-card').length,
    timelineEvents: document.querySelectorAll('.timeline-event-card').length,
    timelineRows: document.querySelectorAll('.timeline-value-table tbody tr').length
  }))
}

function requestSnapshot(tracker) {
  return { index: tracker.requests.length, completed: tracker.completed, counts: { ...tracker.counts } }
}

function requestDelta(tracker, before) {
  const requests = tracker.requests.slice(before.index)
  return {
    count: requests.length,
    bytes: requests.reduce((sum, request) => sum + request.bytes, 0),
    routes: requests.reduce((acc, request) => ({ ...acc, [request.key]: (acc[request.key] || 0) + 1 }), {}),
    routeBytes: requests.reduce((acc, request) => ({ ...acc, [request.key]: (acc[request.key] || 0) + request.bytes }), {})
  }
}

function validateRequestDelta(delta, label) {
  if (delta == null || typeof delta !== 'object') throw new Error(`${label} request delta must be an object`)
  const fields = Object.keys(delta).sort()
  if (JSON.stringify(fields) !== JSON.stringify(['bytes', 'count', 'routeBytes', 'routes'])) {
    throw new Error(`${label} request delta must contain count, bytes, routes, and routeBytes`)
  }
  if (!Number.isInteger(delta.count) || delta.count < 0 || !Number.isInteger(delta.bytes) || delta.bytes < 0) {
    throw new Error(`${label} request delta count and bytes must be non-negative integers`)
  }
  for (const [route, count] of Object.entries(delta.routes)) {
    if (!route || !Number.isInteger(count) || count <= 0) throw new Error(`${label} route count is invalid for ${route}`)
  }
  for (const [route, bytes] of Object.entries(delta.routeBytes)) {
    if (!route || !Number.isInteger(bytes) || bytes < 0) throw new Error(`${label} route bytes are invalid for ${route}`)
  }
  if (Object.keys(delta.routes).length !== Object.keys(delta.routeBytes).length || Object.keys(delta.routes).some(route => !Object.hasOwn(delta.routeBytes, route))) {
    throw new Error(`${label} request delta route and byte keys differ`)
  }
  if (Object.values(delta.routes).reduce((sum, count) => sum + count, 0) !== delta.count) throw new Error(`${label} request delta count does not equal route counts`)
  if (Object.values(delta.routeBytes).reduce((sum, bytes) => sum + bytes, 0) !== delta.bytes) throw new Error(`${label} request delta bytes do not equal route bytes`)
  return delta
}

function createTracker() {
  return {
    requests: [], counts: {}, bytes: {}, active: 0, completed: 0, lastActivityAt: performance.now(), unhandledApiRoutes: [],
    model: { snapshotGeneration: 0, snapshotItems: 0, snapshotPages: 0, snapshotComplete: false, snapshotProfile: null, timelineEvents: 0, timelineBuckets: 0, timelineBucketRequest: null }
  }
}

function createResyncGate(pauseOffset) {
  let pause
  let release
  const waitForPause = new Promise(resolve => { pause = resolve })
  const waitForRelease = new Promise(resolve => { release = resolve })
  return { pauseOffset, used: false, waitForPause, waitForRelease, signalPaused: pause, release }
}

async function requiredPromiseWithin(promise, timeoutMs, label) {
  let timeout
  try {
    return await Promise.race([
      promise,
      new Promise((_, reject) => { timeout = setTimeout(() => reject(new Error(`${label} did not occur within ${timeoutMs}ms`)), timeoutMs) })
    ])
  } finally {
    clearTimeout(timeout)
  }
}

async function waitForCurrentSnapshotTransport(page, tracker, fixture, label, timeoutMs = 60000) {
  const expectedItems = workspaceShellSnapshotItems(fixture).length
  const expectedPages = Math.ceil(expectedItems / DIRECT_FOCUS_CONTRACT.pageSize)
  const ready = () => transportContractReady({
    protocol: 'bounded', transportedItems: tracker.model.snapshotItems, expectedItems, fullBackingItems: snapshotItems(fixture).length,
    transportedPages: tracker.model.snapshotPages, expectedPages, complete: tracker.model.snapshotComplete
  })
  const deadline = performance.now() + timeoutMs
  while (performance.now() < deadline) {
    assertNoUnhandledApiRoutes(tracker)
    if (ready() && tracker.active === 0) {
      await waitForTransportQuiescence(page, tracker, label)
      if (ready() && tracker.active === 0) return { items: expectedItems, pages: expectedPages }
    }
    await page.waitForTimeout(25)
  }
  throw new Error(`${label} failed: ${tracker.model.snapshotProfile} snapshot ${tracker.model.snapshotItems}/${expectedItems} shell items, ${tracker.model.snapshotPages}/${expectedPages} pages, complete=${tracker.model.snapshotComplete}, active=${tracker.active}`)
}

async function measureDirectFocus(browser, origin, fixture) {
  const direct = directFocusFixture(fixture)
  const canonicalSnapshots = workspaceShellSnapshotItems(fixture)
  if (direct.targetProject.parent_id != null) throw new Error(`direct-focus target ${direct.targetProject.id} is not a root project`)
  const tracker = createTracker()
  const context = await browser.newContext({ viewport: HARNESS_CONFIGURATION.viewport })
  await context.addInitScript(fakeWebSocketScript)
  const page = await context.newPage()
  const consoleErrors = []
  page.on('console', message => { if (message.type() === 'error') consoleErrors.push(message.text()) })
  page.on('pageerror', error => consoleErrors.push(error.message))
  await page.route('**/api/v1/**', fixtureResponder(fixture, tracker, { snapshots: canonicalSnapshots }))
  const targetId = direct.targetProject.id
  const directEntityKey = 'navigation:focus'
  const attemptBefore = requestSnapshot(tracker)
  const focusStart = performance.now()
  let requestedBeforeFullResync = false
  let renderedBeforeFullResync = false
  let focusObservedAt = focusStart
  let observationElapsedMs = 0
  try {
    await page.goto(`${origin}/workspace/${fixture.workspace.id}#tab=projects&focus=project:${targetId}`, { waitUntil: 'domcontentloaded' })
    const observationStartedAt = performance.now()
    const observationDeadline = observationStartedAt + DIRECT_FOCUS_CONTRACT.observationDeadlineMs
    while (performance.now() < observationDeadline) {
      assertNoUnhandledApiRoutes(tracker)
    requestedBeforeFullResync = (tracker.counts[directEntityKey] || 0) > 0
      renderedBeforeFullResync = await page.locator(`#entity-${targetId}`).count() > 0 && await page.locator('.focus-breadcrumb-bar').count() > 0
      if (requestedBeforeFullResync && renderedBeforeFullResync) break
      await page.waitForTimeout(10)
    }
    focusObservedAt = performance.now()
    observationElapsedMs = focusObservedAt - observationStartedAt
  } finally {}
  const attemptDelta = requestDelta(tracker, attemptBefore)
  const focusRequests = tracker.requests.slice(attemptBefore.index).filter(request =>
    request.key === directEntityKey
    || request.url.includes(`/api/v1/workspaces/${fixture.workspace.id}/navigation/focus/project/${targetId}`)
  )
  const productFailureReason = requestedBeforeFullResync && renderedBeforeFullResync
    ? null
    : requestedBeforeFullResync
      ? 'focus path requested the target but did not render its breadcrumb/card before full resync'
      : renderedBeforeFullResync
        ? 'focus path rendered the target without issuing its direct entity request before full resync'
        : 'focus path issued no direct entity request and rendered no target before full resync'
  const full = await waitForCurrentSnapshotTransport(page, tracker, fixture, 'direct-focus released shell resync')
  await page.waitForSelector('.focus-breadcrumb-bar', { timeout: 30000 })
  await page.waitForSelector(`#entity-${targetId}`, { timeout: 30000 })
  const renderedAfterFullResync = true
  assertNoUnhandledApiRoutes(tracker)
  if (consoleErrors.length > 0) throw new Error(`direct-focus browser console error(s): ${consoleErrors.join(' | ')}`)
  await context.close()
  return {
    ms: focusObservedAt - focusStart,
    observationDeadlineMs: DIRECT_FOCUS_CONTRACT.observationDeadlineMs,
    observationElapsedMs,
    targetAbsentBeforeCanonicalResync: true,
    targetId,
    targetParentId: direct.targetProject.parent_id,
    pausedSnapshotItems: 0,
    pausedSnapshotPages: 0,
    directFocusRequested: requestedBeforeFullResync,
    directFocusRendered: renderedBeforeFullResync,
    productFailureReason,
    entityLookupRequests: focusRequests.filter(request => request.key === directEntityKey).length,
    count: focusRequests.length,
    focusRequestRoutes: focusRequests.reduce((values, request) => ({ ...values, [request.key]: (values[request.key] || 0) + 1 }), {}),
    attemptRouteDelta: attemptDelta,
    canonicalSnapshotItems: full.items,
    canonicalSnapshotPages: full.pages,
    canonicalSnapshotHash: snapshotHash(fixture),
    renderedAfterFullResync
  }
}

// This is deliberately outside the recorded small/large measurements: it
// exercises continuation against a derived test fixture without changing the
// immutable baseline fixture hashes or the production direct-focus budget.
async function verifyDeepFocusContinuation(browser, origin) {
  const deep = deepFocusFixture()
  const fixture = deep.fixture
  const tracker = createTracker()
  const context = await browser.newContext({ viewport: HARNESS_CONFIGURATION.viewport })
  await context.addInitScript(fakeWebSocketScript)
  const page = await context.newPage()
  const consoleErrors = []
  page.on('console', message => { if (message.type() === 'error') consoleErrors.push(message.text()) })
  page.on('pageerror', error => consoleErrors.push(error.message))
  await page.route('**/api/v1/**', fixtureResponder(fixture, tracker, { snapshots: workspaceShellSnapshotItems(fixture) }))
  try {
    await page.goto(`${origin}/workspace/${fixture.workspace.id}#tab=projects&focus=project:${deep.targetProject.id}`, { waitUntil: 'domcontentloaded' })
    await page.waitForSelector(`#entity-${deep.targetProject.id}`, { timeout: 30000 })
    await page.waitForSelector('.focus-breadcrumb-bar', { timeout: 30000 })
    const deadline = performance.now() + 30000
    const offsets = () => tracker.requests
      .filter(request => request.key === 'navigation:focus')
      .map(request => Number(new URL(request.url).searchParams.get('ancestor_offset')))
    while (performance.now() < deadline) {
      assertNoUnhandledApiRoutes(tracker)
      if (tracker.active === 0 && JSON.stringify(offsets()) === JSON.stringify([0, 64, 128])) break
      await page.waitForTimeout(25)
    }
    const observedOffsets = offsets()
    if (JSON.stringify(observedOffsets) !== JSON.stringify([0, 64, 128])) throw new Error(`deep focus expected bounded ancestor offsets 0,64,128; received ${observedOffsets.join(',')}`)
    if (await page.locator(`#entity-${deep.targetProject.id}`).count() !== 1) throw new Error('deep focus continuation lost the target card')
    await page.waitForFunction(
      expectedCount => document.querySelectorAll('.focus-breadcrumb-bar .focus-crumb').length === expectedCount,
      131,
      { timeout: 30000 }
    )
    const crumbNames = (await page.locator('.focus-breadcrumb-bar .focus-crumb').allTextContents()).slice(1)
    const expectedCrumbNames = Array.from({ length: 130 }, (_, index) => `Deep focus project ${String(index + 1).padStart(3, '0')}`)
    if (JSON.stringify(crumbNames) !== JSON.stringify(expectedCrumbNames) || new Set(crumbNames).size !== crumbNames.length) throw new Error(`deep focus breadcrumb was incomplete, reordered, or duplicated: expected ${expectedCrumbNames.length} ordered crumbs, received ${crumbNames.length}`)
    if (consoleErrors.length > 0) throw new Error(`deep-focus continuation browser console error(s): ${consoleErrors.join(' | ')}`)
    return { ancestorCount: deep.ancestorCount, offsets: observedOffsets, requests: observedOffsets.length, targetRendered: true, breadcrumbCount: crumbNames.length, breadcrumbOrdered: true, terminal: true }
  } finally {
    await context.close()
  }
}

async function verifyStaleDeepFocusContinuation(browser, origin) {
  const deep = deepFocusFixture()
  const fixture = deep.fixture
  const replacement = fixture.projects.find(project => project.name === 'Deep focus project 100')
  if (!replacement) throw new Error('deep focus fixture did not contain the stale-response replacement target')
  const tracker = createTracker()
  let releasePause
  let signalPaused
  const pause = {
    entityType: 'project',
    entityId: deep.targetProject.id,
    ancestorOffset: 64,
    used: false,
    waitForRelease: new Promise(resolve => { releasePause = resolve }),
    signalPaused: () => signalPaused()
  }
  const paused = new Promise(resolve => { signalPaused = resolve })
  const context = await browser.newContext({ viewport: HARNESS_CONFIGURATION.viewport })
  await context.addInitScript(fakeWebSocketScript)
  const page = await context.newPage()
  try {
    await page.route('**/api/v1/**', fixtureResponder(fixture, tracker, { snapshots: workspaceShellSnapshotItems(fixture), focusContinuationPause: pause }))
    await page.goto(`${origin}/workspace/${fixture.workspace.id}#tab=projects&focus=project:${deep.targetProject.id}`, { waitUntil: 'domcontentloaded' })
    await paused
    await page.evaluate(projectId => { window.location.hash = `tab=projects&focus=project:${projectId}` }, replacement.id)
    await page.waitForSelector(`#entity-${replacement.id}`, { timeout: 30000 })
    releasePause()
    await page.waitForTimeout(250)
    const oldTerminalPageRequested = tracker.requests.some(request => {
      const url = new URL(request.url)
      return request.key === 'navigation:focus'
        && url.pathname.endsWith(`/project/${deep.targetProject.id}`)
        && Number(url.searchParams.get('ancestor_offset')) === 128
    })
    if (oldTerminalPageRequested) throw new Error('stale deep-focus continuation scheduled its old terminal page after the focus changed')
    return { staleContinuationRejected: true }
  } finally {
    await context.close()
  }
}

async function measureRun(browser, origin, fixture, measured, trace) {
  const anchors = representativeProjectAnchors(fixture)
  const tracker = createTracker()
  const context = await browser.newContext({ viewport: HARNESS_CONFIGURATION.viewport })
  await context.addInitScript(fakeWebSocketScript)
  if (trace) await context.tracing.start({ screenshots: true, snapshots: true, sources: false })
  const page = await context.newPage()
  const consoleErrors = []
  page.on('console', message => { if (message.type() === 'error') consoleErrors.push(message.text()) })
  page.on('pageerror', error => consoleErrors.push(error.message))
  await page.route('**/api/v1/**', fixtureResponder(fixture, tracker))
  const cdp = await context.newCDPSession(page)
  const blankHeap = (await cdp.send('Runtime.getHeapUsage')).usedSize
  const coldStart = performance.now()
  await page.goto(`${origin}/workspace/${fixture.workspace.id}`, { waitUntil: 'domcontentloaded' })
  await page.waitForSelector('.tree-view', { timeout: 30000 })
  await waitForWorkspaceReady(page, tracker, fixture, anchors.first, 'cold Projects model/anchor')
  const coldMs = performance.now() - coldStart
  const coldEnd = tracker.requests.length
  const coldRequests = tracker.requests.slice(0, coldEnd)
  const cold = {
    ms: coldMs, requests: coldRequests.length, bytes: coldRequests.reduce((sum, request) => sum + request.bytes, 0),
    routeCounts: { ...tracker.counts }, routeBytes: { ...tracker.bytes },
    entityOverviewRequests: (tracker.counts['projects:overview'] || 0) + (tracker.counts['tasks:overview'] || 0),
    canonicalSnapshotItems: tracker.model.snapshotItems,
    canonicalSnapshotPages: tracker.counts['change-stream:resync'] || 0
  }
  const dom = []
  dom.push({ tab: 'projects', ...(await domMetrics(page)) })
  const interactions = {}
  const tabSwitches = {}
  const filters = {}
  // Expansion/recursive-tree interaction evidence belongs to the downstream
  // rendering task. This bootstrap record intentionally owns only root slices.
  const taskFilterBefore = requestSnapshot(tracker)
  const taskFilterMs = await requiredDoubleFrameByText(page, '.filter-bar button', 'Tasks')
  await page.waitForFunction(() => document.querySelectorAll('.card-project').length === 0 && document.querySelectorAll('.card-task,.card-subtask').length > 0)
  await waitForTransportQuiescence(page, tracker, 'Tasks filter')
  interactions.taskFilterMs = { ms: taskFilterMs, ...requestDelta(tracker, taskFilterBefore) }
  const taskFilterResetBefore = requestSnapshot(tracker)
  const taskFilterResetMs = await requiredDoubleFrameByText(page, '.filter-bar button', 'All')
  await waitForWorkspaceReady(page, tracker, fixture, anchors.first, 'All filter representative restore')
  interactions.taskFilterResetMs = { ms: taskFilterResetMs, ...requestDelta(tracker, taskFilterResetBefore) }

  const observationsTabBefore = requestSnapshot(tracker)
  const obsTabMs = await requiredDoubleFrame(page, '.tabs button:nth-child(2)')
  await page.waitForSelector('#observation-results')
  await page.waitForFunction(expected => document.querySelectorAll('.observation-card').length === expected, Math.min(50, fixture.observations.length), { timeout: 30000 })
  await waitForTransportQuiescence(page, tracker, 'Observations initial page')
  interactions.observationsTabMs = { ms: obsTabMs, ...requestDelta(tracker, observationsTabBefore) }
  tabSwitches.observations = interactions.observationsTabMs
  dom.push({ tab: 'observations', ...(await domMetrics(page)) })
  const loadBefore = requestSnapshot(tracker)
  const loadStart = performance.now()
  const loadButton = page.locator('.observation-load-more')
  if (await loadButton.count() !== 1) throw new Error(`required Observation load-more target count was ${await loadButton.count()}`)
  await loadButton.click()
  await page.waitForFunction(expected => document.querySelectorAll('.observation-card').length === expected, Math.min(100, fixture.observations.length), { timeout: 30000 })
  await waitForTransportQuiescence(page, tracker, 'Observation load more')
  const observationLoadMore = { ms: performance.now() - loadStart, ...requestDelta(tracker, loadBefore) }
  const filterInput = page.locator('#observation-query')
  if (await filterInput.count() !== 1) throw new Error(`required Observation query target count was ${await filterInput.count()}`)
  const filterBefore = requestSnapshot(tracker)
  await filterInput.fill(OBSERVATION_MEASURED_QUERY)
  const observationFilterMs = await requiredDoubleFrame(page, '.observation-filter-apply')
  await page.waitForFunction(() => {
    const count = document.querySelectorAll('.observation-card').length
    return !document.querySelector('.observation-state-loading') && count === 1
  }, null, { timeout: 30000 })
  await waitForTransportQuiescence(page, tracker, 'Observation filter')
  interactions.observationFilterMs = { ms: observationFilterMs, ...requestDelta(tracker, filterBefore) }
  filters.observations = interactions.observationFilterMs
  const observationFilterResetBefore = requestSnapshot(tracker)
  await filterInput.fill('')
  const observationFilterResetMs = await requiredDoubleFrame(page, '.observation-filter-apply')
  await page.waitForFunction(expected => document.querySelectorAll('.observation-card').length === expected, Math.min(50, fixture.observations.length), { timeout: 30000 })
  await waitForTransportQuiescence(page, tracker, 'Observation filter reset')
  interactions.observationFilterResetMs = { ms: observationFilterResetMs, ...requestDelta(tracker, observationFilterResetBefore) }
  filters.observationReset = interactions.observationFilterResetMs

  const timelineTabBefore = requestSnapshot(tracker)
  const timelineTabMs = await requiredDoubleFrame(page, '.tabs button:nth-child(3)')
  await page.waitForSelector('.timeline-panel')
  await page.waitForFunction(() =>
    !document.querySelector('.timeline-panel .loading-indicator')
    && document.querySelectorAll('.timeline-event-card').length > 0
    && document.querySelectorAll('.timeline-value-table tbody tr').length > 0,
  null, { timeout: 30000 })
  await waitForTransportQuiescence(page, tracker, 'Timeline')
  const expectedTimelineBuckets = queryTimelineBuckets(fixture, TIMELINE_DEFAULT_UI_QUERY).buckets.length
  if (tracker.model.timelineEvents !== Math.min(50, fixture.timeline.events.length) || tracker.model.timelineBuckets !== expectedTimelineBuckets || JSON.stringify(tracker.model.timelineBucketRequest) !== JSON.stringify(TIMELINE_DEFAULT_UI_QUERY)) {
    throw new Error(`Timeline model readiness mismatch: events ${tracker.model.timelineEvents}/${Math.min(50, fixture.timeline.events.length)}, buckets ${tracker.model.timelineBuckets}/${expectedTimelineBuckets}, request=${JSON.stringify(tracker.model.timelineBucketRequest)}`)
  }
  interactions.timelineTabMs = { ms: timelineTabMs, ...requestDelta(tracker, timelineTabBefore) }
  tabSwitches.timeline = interactions.timelineTabMs
  dom.push({ tab: 'timeline', ...(await domMetrics(page)) })

  const projectsTabBefore = requestSnapshot(tracker)
  const returnProjectsMs = await requiredDoubleFrame(page, '.tabs button:nth-child(1)')
  await waitForWorkspaceReady(page, tracker, fixture, anchors.first, 'Projects tab representative restore')
  interactions.projectsTabMs = { ms: returnProjectsMs, ...requestDelta(tracker, projectsTabBefore) }
  tabSwitches.projects = interactions.projectsTabMs
  const directFocus = await measureDirectFocus(browser, origin, fixture)

  await restoreWorkspace(page, tracker, fixture, anchors.first, 'pre-live representative Projects restore')

  const liveBefore = requestSnapshot(tracker)
  const liveStart = performance.now()
  const pushedSockets = await page.evaluate(frames => window.__perfPushFrames(frames), fixture.liveFrames)
  if (pushedSockets !== 1) throw new Error(`live fixture expected one open socket, observed ${pushedSockets}`)
  await page.waitForFunction(expected => window.__perfLastPush?.frames === expected && window.__perfLastPush?.resumeToken === 'fixture-live-after', fixture.liveFrames.length)
  const settledAt = await waitForLiveSettle(page, tracker, liveBefore, anchors.first)
  const stability = await assertLiveStability(page, tracker, 500)
  const liveTiming = liveTimingSummary({ startedAt: liveStart, settledAt, stabilityStartedAt: stability.startedAt, stabilityEndedAt: stability.endedAt })
  const postLiveUi = await projectUiSignals(page, anchors.first)
  const expectedSnapshotItems = workspaceShellSnapshotItems(fixture).length
  const expectedSnapshotPages = Math.ceil(expectedSnapshotItems / DIRECT_FOCUS_CONTRACT.pageSize)
  if (!representativeReadiness({
    modelComplete: transportContractReady({ protocol: 'bounded', transportedItems: tracker.model.snapshotItems, expectedItems: expectedSnapshotItems, fullBackingItems: snapshotItems(fixture).length, transportedPages: tracker.model.snapshotPages, expectedPages: expectedSnapshotPages, complete: tracker.model.snapshotComplete }),
    activeRequests: tracker.active,
    loading: postLiveUi.loading,
    focused: postLiveUi.focused,
    anchorVisible: postLiveUi.anchorVisible
  })) throw new Error('post-live representative workspace readiness failed')
  const liveDelta = requestDelta(tracker, liveBefore)
  const entityRoutes = Object.entries(liveDelta.routes).filter(([key]) => key.includes(':entity:'))
  const duplicateRequests = entityRoutes.reduce((sum, [, count]) => sum + Math.max(0, count - 1), 0)
  const liveBatch = {
    frames: fixture.liveFrames.length, sockets: pushedSockets, ms: liveTiming.settleMs, stabilityMs: liveTiming.stabilityMs, stabilityRequests: stability.requests,
    ...liveDelta, requests: liveDelta.count, duplicateRequests,
    wholeWorkspaceReload: liveWholeWorkspaceReload(liveDelta.routes)
  }
  dom.push({ tab: 'post-live-projects', ...(await domMetrics(page)) })

  await cdp.send('HeapProfiler.collectGarbage')
  const heap = await cdp.send('Runtime.getHeapUsage')
  assertNoUnhandledApiRoutes(tracker)
  const result = {
    fixture: fixture.size, measured, cold, tabSwitches, filters, interactions, observationLoadMore, directFocus, liveBatch, dom,
    maxDomNodes: Math.max(...dom.map(value => value.nodes)), maxCollectionRows: Math.max(...dom.map(value => value.rows)),
    attributableHeapBytes: Math.max(0, heap.usedSize - blankHeap),
    heapPoint: HARNESS_CONFIGURATION.heapPoint,
    readiness: { protocol: PRODUCTION_SNAPSHOT_PROFILE, fullBackingSnapshotItems: snapshotItems(fixture).length, expectedSnapshotPages, transportedSnapshotItems: tracker.model.snapshotItems, transportedSnapshotPages: tracker.model.snapshotPages, snapshotComplete: tracker.model.snapshotComplete, timelineEvents: tracker.model.timelineEvents, timelineBuckets: tracker.model.timelineBuckets, timelineBucketRequest: tracker.model.timelineBucketRequest, anchors },
    consoleErrors
  }
  if (trace) {
    fs.mkdirSync(path.dirname(tracePath), { recursive: true })
    await context.tracing.stop({ path: tracePath })
  }
  await context.close()
  return result
}

function aggregates(runs) {
  if (runs.length !== SAMPLES) throw new Error(`aggregate requires exactly ${SAMPLES} measured runs`)
  const values = (key, label) => assertFiveSamples(runs.map(key), label)
  const cold = values(run => run.cold.ms, 'cold')
  const focus = values(run => run.directFocus.ms, 'direct focus')
  const load = values(run => run.observationLoadMore.ms, 'Observation load more')
  const live = values(run => run.liveBatch.ms, 'live batch')
  const interactionNames = Object.keys(runs[0].interactions).sort()
  if (interactionNames.length === 0) throw new Error('at least one named local interaction is required')
  const localScenarios = Object.fromEntries(interactionNames.map(name => {
    if (runs.some(run => JSON.stringify(Object.keys(run.interactions).sort()) !== JSON.stringify(interactionNames))) {
      throw new Error('named interaction set changed between samples')
    }
    const rawMs = values(run => run.interactions[name].ms, `local interaction ${name}`)
    const requestDeltas = runs.map(run => validateRequestDelta({
      count: run.interactions[name].count,
      bytes: run.interactions[name].bytes,
      routes: run.interactions[name].routes,
      routeBytes: run.interactions[name].routeBytes
    }, `local interaction ${name}`))
    return [name, {
      rawMs, medianMs: median(rawMs), p95Ms: nearestRankP95(rawMs), requestDeltas,
      requestCounts: requestDeltas.map(delta => delta.count), fixtureBytes: requestDeltas.map(delta => delta.bytes),
      routeCounts: requestDeltas.map(delta => delta.routes), routeBytes: requestDeltas.map(delta => delta.routeBytes)
    }]
  }))
  const liveRequestDeltas = runs.map(run => validateRequestDelta({
    count: run.liveBatch.count,
    bytes: run.liveBatch.bytes,
    routes: run.liveBatch.routes,
    routeBytes: run.liveBatch.routeBytes
  }, 'live batch'))
  return {
    cold: { rawMs: cold, medianMs: median(cold), p95Ms: nearestRankP95(cold), requestCounts: runs.map(run => run.cold.requests), fixtureBytes: runs.map(run => run.cold.bytes), entityOverviewRequests: runs.map(run => run.cold.entityOverviewRequests), canonicalSnapshotItems: runs.map(run => run.cold.canonicalSnapshotItems), canonicalSnapshotPages: runs.map(run => run.cold.canonicalSnapshotPages) },
    render: { maxDomNodes: Math.max(...runs.map(run => run.maxDomNodes)), maxCollectionRows: Math.max(...runs.map(run => run.maxCollectionRows)) },
    localInteractions: { scenarios: localScenarios, worstP95Ms: Math.max(...Object.values(localScenarios).map(value => value.p95Ms)) },
    directFocus: {
      rawMs: focus, medianMs: median(focus), p95Ms: nearestRankP95(focus),
      requestCounts: runs.map(run => run.directFocus.count), entityLookupRequests: runs.map(run => run.directFocus.entityLookupRequests),
      requested: runs.map(run => run.directFocus.directFocusRequested), rendered: runs.map(run => run.directFocus.directFocusRendered),
      failureReasons: runs.map(run => run.directFocus.productFailureReason)
    },
    observationLoadMore: { rawMs: load, medianMs: median(load), p95Ms: nearestRankP95(load), requestCounts: runs.map(run => run.observationLoadMore.count) },
    liveBatch: {
      rawMs: live, medianMs: median(live), p95Ms: nearestRankP95(live), requestDeltas: liveRequestDeltas,
      requestCounts: liveRequestDeltas.map(delta => delta.count), fixtureBytes: liveRequestDeltas.map(delta => delta.bytes),
      routeCounts: liveRequestDeltas.map(delta => delta.routes), routeBytes: liveRequestDeltas.map(delta => delta.routeBytes),
      duplicateRequests: runs.map(run => run.liveBatch.duplicateRequests), wholeWorkspaceReload: runs.some(run => run.liveBatch.wholeWorkspaceReload)
    },
    heap: { values: runs.map(run => run.attributableHeapBytes), maxAttributableBytes: Math.max(...runs.map(run => run.attributableHeapBytes)) },
    consoleErrors: runs.flatMap(run => run.consoleErrors)
  }
}

function evaluate(result, comparableEnvironment = true) {
  const target = budgets.large
  const small = result.aggregates.small
  const large = result.aggregates.large
  const metrics = []
  const add = (name, actual, expected, pass, category = 'exact') => metrics.push({ name, actual, expected, pass, category })
  add('cold HTTP requests', Math.max(...large.cold.requestCounts), target.cold.maxHttpRequests, Math.max(...large.cold.requestCounts) <= target.cold.maxHttpRequests)
  add('cold fixture bytes', Math.max(...large.cold.fixtureBytes), target.cold.maxFixtureBytes, Math.max(...large.cold.fixtureBytes) <= target.cold.maxFixtureBytes)
  add('entity overview requests', Math.max(...large.cold.entityOverviewRequests), target.cold.maxEntityOverviewRequests, Math.max(...large.cold.entityOverviewRequests) <= target.cold.maxEntityOverviewRequests)
  add('canonical snapshot items', Math.min(...large.cold.canonicalSnapshotItems), target.cold.requiredCanonicalSnapshotItems, large.cold.canonicalSnapshotItems.every(value => value === target.cold.requiredCanonicalSnapshotItems))
  add('canonical snapshot pages', Math.min(...large.cold.canonicalSnapshotPages), target.cold.requiredCanonicalSnapshotPages, large.cold.canonicalSnapshotPages.every(value => value === target.cold.requiredCanonicalSnapshotPages))
  const requestDelta = Math.max(...large.cold.requestCounts) - Math.max(...small.cold.requestCounts)
  add('small-to-large request delta', requestDelta, target.scaling.maxSmallToLargeRequestDelta, requestDelta <= target.scaling.maxSmallToLargeRequestDelta)
  const renderEvaluation = renderBudgetEvaluation({ nodes: large.render.maxDomNodes, rows: large.render.maxCollectionRows }, target.render)
  add('DOM nodes', large.render.maxDomNodes, target.render.maxDomNodes, renderEvaluation.nodesPass)
  add('rendered collection rows/cards', large.render.maxCollectionRows, target.render.maxCollectionRows, renderEvaluation.rowsPass)
  add('direct focus requests', Math.max(...large.directFocus.requestCounts), target.directFocus.maxRequests, Math.max(...large.directFocus.requestCounts) <= target.directFocus.maxRequests)
  add('direct focus requested', large.directFocus.requested.every(Boolean), target.directFocus.requireRequested, large.directFocus.requested.every(Boolean) === target.directFocus.requireRequested)
  add('direct focus rendered', large.directFocus.rendered.every(Boolean), target.directFocus.requireRendered, large.directFocus.rendered.every(Boolean) === target.directFocus.requireRendered)
  add('Observation load-more requests', Math.max(...large.observationLoadMore.requestCounts), target.observationLoadMore.maxRequests, Math.max(...large.observationLoadMore.requestCounts) <= target.observationLoadMore.maxRequests)
  add('live follow-up requests', Math.max(...large.liveBatch.requestCounts), target.liveBatch.maxFollowUpRequests, Math.max(...large.liveBatch.requestCounts) <= target.liveBatch.maxFollowUpRequests)
  add('live whole-workspace reload', large.liveBatch.wholeWorkspaceReload, false, !large.liveBatch.wholeWorkspaceReload)
  add('live repeated-target duplicate requests', Math.max(...large.liveBatch.duplicateRequests), target.liveBatch.maxDuplicateRequestsPerRepeatedTarget, Math.max(...large.liveBatch.duplicateRequests) <= target.liveBatch.maxDuplicateRequestsPerRepeatedTarget)
  add('browser console errors', large.consoleErrors.length, 0, large.consoleErrors.length === 0)
  for (const [scenario, values] of Object.entries(large.localInteractions.scenarios)) {
    add(`local interaction ${scenario} p95 ms`, values.p95Ms, target.localInteraction.maxP95Ms, !comparableEnvironment || values.p95Ms <= target.localInteraction.maxP95Ms, comparableEnvironment ? 'pinned-environment' : 'informational')
  }
  for (const [name, actual, expected] of [
    ['cold median ms', large.cold.medianMs, target.cold.maxMedianMs], ['cold p95 ms', large.cold.p95Ms, target.cold.maxP95Ms],
    ['local interaction worst p95 ms', large.localInteractions.worstP95Ms, target.localInteraction.maxP95Ms], ['direct focus p95 ms', large.directFocus.p95Ms, target.directFocus.maxP95Ms],
    ['Observation load-more p95 ms', large.observationLoadMore.p95Ms, target.observationLoadMore.maxP95Ms], ['live batch p95 ms', large.liveBatch.p95Ms, target.liveBatch.maxP95Ms],
    ['attributable heap bytes', large.heap.maxAttributableBytes, target.heap.maxAttributableBytes]
  ]) add(name, actual, expected, !comparableEnvironment || actual <= expected, comparableEnvironment ? 'pinned-environment' : 'informational')
  return { comparableEnvironment, passed: metrics.every(metric => metric.pass), metrics }
}

async function main() {
  // The final v2 write is deliberately last: its fixture/self-check validation
  // must succeed before any review artifact is replaced.
  runTaskEvidencePrerequisites()
  if (!fs.existsSync(path.join(staticRoot, 'index.html'))) throw new Error(`production build missing at ${staticRoot}; run npm run build first`)
  const fixtures = { small: generateFixture('small'), large: generateFixture('large') }
  const contracts = {
    budgetsHash: hashJson(budgets),
    configurationHash: hashJson(HARNESS_CONFIGURATION),
    directFocusContractHash: hashJson(DIRECT_FOCUS_CONTRACT),
    fixtures: { small: fixtureHash(fixtures.small), large: fixtureHash(fixtures.large) },
    snapshots: { small: snapshotHash(fixtures.small), large: snapshotHash(fixtures.large) }
  }
  let recordedBaseline = null
  let recordedTraceManifest = null
  if (mode === 'check') {
    if (!fs.existsSync(baselinePath)) throw new Error(`approved baseline missing: ${baselinePath}`)
    if (!fs.existsSync(traceManifestPath)) throw new Error(`trace provenance manifest missing: ${traceManifestPath}`)
    const immutableBaseline = JSON.parse(fs.readFileSync(baselinePath, 'utf8'))
    const immutableTraceManifest = JSON.parse(fs.readFileSync(traceManifestPath, 'utf8'))
    if (immutableBaseline.baseCommit !== BASE_COMMIT) throw new Error(`baseline base commit mismatch: expected ${BASE_COMMIT}, observed ${immutableBaseline.baseCommit}`)
    if (JSON.stringify(immutableTraceManifest.trace) !== JSON.stringify(immutableBaseline.trace)) throw new Error('immutable baseline and trace provenance differ')
    if (immutableBaseline.recordAuthorization !== 'explicit --authorize-baseline') throw new Error('baseline lacks explicit record authorization provenance')
    if (!fs.existsSync(afterArtifactPath) || !fs.existsSync(afterTraceArtifactPath)) throw new Error('final working-tree after evidence is missing; run npm run perf:record-after')
    recordedBaseline = JSON.parse(fs.readFileSync(afterArtifactPath, 'utf8'))
    recordedTraceManifest = JSON.parse(fs.readFileSync(afterTraceArtifactPath, 'utf8'))
    if (recordedBaseline.baseCommit !== BASE_COMMIT) throw new Error(`after-artifact base commit mismatch: expected ${BASE_COMMIT}, observed ${recordedBaseline.baseCommit}`)
    if (JSON.stringify(recordedBaseline.contracts) !== JSON.stringify(contracts)) throw new Error(`baseline fixture/configuration/budget contracts do not match current inputs`)
    if (JSON.stringify(recordedTraceManifest.contracts) !== JSON.stringify(contracts)) throw new Error(`after trace manifest fixture/configuration/budget contracts do not match current inputs`)
    if (JSON.stringify(recordedTraceManifest.trace) !== JSON.stringify(recordedBaseline.trace)) throw new Error('after trace manifest and after artifact provenance differ')
    if (recordedBaseline.recordAuthorization !== 'explicit --authorize-after-artifact') throw new Error('after artifact lacks explicit record authorization provenance')
  }
  const packageVersion = relativePath => {
    const file = path.join(frontendRoot, 'node_modules', ...relativePath, 'package.json')
    if (!fs.existsSync(file)) throw new Error(`required package metadata missing: ${file}`)
    const version = JSON.parse(fs.readFileSync(file, 'utf8')).version
    if (!version) throw new Error(`required package version missing: ${file}`)
    return version
  }
  const toolVersions = {
    node: requiredCommandVersion('Node', process.execPath, ['--version']),
    npm: requiredCommandVersion('npm', platformExecutable('npm'), ['--version']),
    elm: requiredCommandVersion('Elm', path.join(frontendRoot, 'node_modules', '.bin', platformExecutable('elm')), ['--version']),
    vite: packageVersion(['vite']),
    playwright: packageVersion(['@playwright', 'test'])
  }
  const server = await staticServer()
  let browser = null
  try {
    browser = await chromium.launch({ headless: true, args: ['--js-flags=--expose-gc', '--enable-precise-memory-info'] })
    const environment = {
      os: `${os.type()} ${os.release()} ${os.arch()}`, cpu: `${os.cpus()[0]?.model || 'unknown'} (${os.cpus().length} logical)`, ramBytes: os.totalmem(),
      ...toolVersions, chromium: browser.version(), viewport: `${HARNESS_CONFIGURATION.viewport.width}x${HARNESS_CONFIGURATION.viewport.height}`, headless: true
    }
    environment.fingerprint = hashJson(environment)
    process.stdout.write('deep-focus continuation... ')
    const staleFocusContinuation = await verifyStaleDeepFocusContinuation(browser, server.origin)
    const deepFocusContinuation = { ...await verifyDeepFocusContinuation(browser, server.origin), ...staleFocusContinuation }
    process.stdout.write(`${deepFocusContinuation.requests} bounded requests\n`)
    const runs = { small: [], large: [] }
    for (const size of ['small', 'large']) {
      for (let index = 0; index < WARMUPS + SAMPLES; index += 1) {
        const measured = index >= WARMUPS
        process.stdout.write(`${size} ${measured ? `sample ${index - WARMUPS + 1}/${SAMPLES}` : `warmup ${index + 1}/${WARMUPS}`}... `)
        const captureTrace = mode === 'record' && size === 'large' && measured && index === WARMUPS + SAMPLES - 1
        const result = await measureRun(browser, server.origin, fixtures[size], measured, captureTrace)
        if (measured) runs[size].push(result)
        process.stdout.write(`${Math.round(result.cold.ms)}ms, ${result.cold.requests} requests\n`)
      }
    }
    let trace = recordedBaseline?.trace || null
    if (mode === 'record') {
      if (!fs.existsSync(tracePath)) throw new Error(`record trace was not created: ${tracePath}`)
      const firstHash = sha256File(tracePath)
      const secondHash = sha256File(tracePath)
      if (firstHash !== secondHash) throw new Error('record trace hash was not reproducible during verification')
      trace = {
        logicalName: 'large measured sample 5/5 Playwright trace',
        sha256: firstHash,
        sizeBytes: fs.statSync(tracePath).size,
        capturedRun: 'large sample 5/5 after two warmups',
        verifiedDuringRecord: true,
        retention: 'generated under ignored perf/.artifacts, intentionally removed after record/check; this versioned manifest retains verified provenance and does not claim the opaque archive is available'
      }
    }
    const result = {
      schemaVersion: 1, baseCommit: BASE_COMMIT, recordedAtUtc: new Date().toISOString(),
      recordAuthorization: mode === 'record'
        ? (recordOutputPath === baselinePath ? 'explicit --authorize-baseline' : 'explicit --authorize-after-artifact')
        : recordedBaseline.recordAuthorization,
      environment, configuration: HARNESS_CONFIGURATION, contracts,
      fixtures: {
        schemaVersion: 1, seed: fixtures.large.seed, directFocusContract: DIRECT_FOCUS_CONTRACT,
        deepFocusContinuation,
        small: { hash: fixtureHash(fixtures.small), snapshotHash: snapshotHash(fixtures.small), scale: fixtures.small.scale, directFocusTarget: directFocusFixture(fixtures.small).targetProject },
        large: { hash: fixtureHash(fixtures.large), snapshotHash: snapshotHash(fixtures.large), scale: fixtures.large.scale, directFocusTarget: directFocusFixture(fixtures.large).targetProject }
      },
      runs, aggregates: { small: aggregates(runs.small), large: aggregates(runs.large) },
      trace,
      hotspots: [
        { downstreamTask: 'bd2eba73-6e33-4bc1-8a45-3b024c662343', evidence: ['src/Feature/DataLoading.elm:297 recursive 200-row project paging then per-project overview fan-out', 'src/Feature/DataLoading.elm:349 recursive 200-row task paging then per-task overview fan-out', 'canonical resync snapshot pages are counted by change-stream:resync'] },
        { downstreamTask: '2503e08f-ff82-4f2c-adff-24e14fec8299', evidence: ['Projects/Observations/Timeline DOM and input-to-next-paint measurements', '50-frame canonical mixed/repeated-target request coalescing and settle measurement'] }
      ]
    }
    let comparable = true
    if (mode === 'check') comparable = recordedBaseline.environment.fingerprint === environment.fingerprint
    result.evaluation = evaluate(result, comparable)
    if (mode === 'record') {
      const traceManifest = { schemaVersion: 1, baseCommit: BASE_COMMIT, contracts, trace }
      fs.writeFileSync(recordOutputPath, `${JSON.stringify(result, null, 2)}\n`)
      fs.writeFileSync(recordTraceManifestPath, `${JSON.stringify(traceManifest, null, 2)}\n`)
      if (recordOutputPath !== baselinePath) finalWorkingTreeEvidence()
    }
    for (const metric of result.evaluation.metrics) console.log(`${metric.pass ? 'PASS' : 'FAIL'} ${metric.name}: ${metric.actual} (budget ${metric.expected}${metric.category === 'informational' ? ', informational environment' : ''})`)
    if (mode === 'record') console.log(`AUTHORIZED RECORD wrote ${recordOutputPath} and ${recordTraceManifestPath}; evaluation is preserved, and budget failures do not fail explicitly authorized record mode.`)
    if (mode === 'check' && !result.evaluation.passed) process.exitCode = 1
    if (mode === 'check' && evidenceRevision === 'v2' && result.evaluation.passed && fs.existsSync(validationRecordPath)) finalWorkingTreeEvidence()
  } finally {
    if (browser) await browser.close()
    await new Promise(resolve => server.server.close(resolve))
  }
}

main().catch(error => { console.error(error.stack || error); process.exitCode = 2 })
