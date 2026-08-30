import fs from 'node:fs'
import http from 'node:http'
import path from 'node:path'
import { fileURLToPath } from 'node:url'
import { TIMELINE_BROWSER_NOW, generateFixture, paginate, projectOverviewResponse, queryObservationFacets, queryObservations, queryProjects, queryTasks, queryTimelineBuckets, queryTimelineEvents, snapshotItems, taskOverviewResponse } from './fixtures.mjs'

const frontendRoot = path.dirname(path.dirname(fileURLToPath(import.meta.url)))
const staticRoot = path.resolve(frontendRoot, '..', 'static')
const fixture = generateFixture(process.argv[2] || 'large')
const snapshots = snapshotItems(fixture)
const projects = new Map(fixture.projects.map(item => [item.id, item]))
const tasks = new Map(fixture.tasks.map(item => [item.id, item]))
const observations = new Map(fixture.observations.map(item => [item.id, item]))

const fakeWebSocket = `<script>(()=>{Date.now=()=>${Date.parse(TIMELINE_BROWSER_NOW)};class W{static OPEN=1;constructor(){this.readyState=0;setTimeout(()=>{this.readyState=1;this.onopen?.({type:'open'});setTimeout(()=>this.onmessage?.({data:JSON.stringify({schema_version:1,type:'checkpoint',catch_up:'complete',resume_token:'manual-smoke'})}),0)},0)}send(){}close(){this.readyState=3;this.onclose?.({code:1000})}addEventListener(t,f){this['on'+t]=f}removeEventListener(t,f){if(this['on'+t]===f)this['on'+t]=null}}window.WebSocket=W})()</script>`

function json(response, value, status = 200) {
  const body = JSON.stringify(value)
  response.writeHead(status, { 'Content-Type': 'application/json', 'Cache-Control': 'no-store' })
  response.end(body)
}

function body(request) {
  return new Promise(resolve => {
    let value = ''
    request.setEncoding('utf8')
    request.on('data', chunk => { value += chunk })
    request.on('end', () => resolve(value ? JSON.parse(value) : {}))
  })
}

async function api(request, response, url) {
  const pathname = url.pathname
  if (pathname === '/api/v1/session') return json(response, {
    auth_mode: 'local', principal: { actor_type: 'user', actor_id: 'perf-user', actor_label: 'Performance User', authority: 'local', grant_user_id: null },
    global_permissions: { create_workspace: false, superadmin: false },
    workspace: { workspace_id: fixture.workspace.id, role: 'owner', can_read: true, can_edit: true, can_admin: true }
  })
  if (pathname === '/api/v1/change-stream/resync') {
    const requestBody = await body(request)
    const offset = requestBody.page_token ? Number(String(requestBody.page_token).split(':')[1]) : 0
    const pageSize = Number(requestBody.page_size) || 100
    const items = snapshots.slice(offset, offset + pageSize)
    const hasMore = offset + pageSize < snapshots.length
    return json(response, { items, has_more: hasMore, ...(hasMore ? { next_page_token: `offset:${offset + pageSize}` } : { resume_token: 'manual-resume' }) })
  }
  if (pathname === '/api/v1/change-stream/ticket') return json(response, { ticket: 'manual-ticket', expires_at: '2099-01-01T00:00:00Z' })
  if (pathname === '/api/v1/workspaces') return json(response, paginate([fixture.workspace], url.searchParams.get('offset'), url.searchParams.get('limit')))
  if (pathname === `/api/v1/workspaces/${fixture.workspace.id}`) return json(response, fixture.workspace)
  if (pathname.endsWith('/memberships')) return json(response, { items: [{ workspace_id: fixture.workspace.id, user_id: 'perf-user', role: 'owner', granted_by: null, created_at: fixture.workspace.created_at, updated_at: fixture.workspace.updated_at }], has_more: false })
  if (pathname === '/api/v1/projects') return json(response, queryProjects(fixture, { status: url.searchParams.get('status'), offset: url.searchParams.get('offset'), limit: url.searchParams.get('limit') }))
  if (pathname === '/api/v1/tasks') return json(response, queryTasks(fixture, { projectId: url.searchParams.get('project_id'), status: url.searchParams.get('status'), priority: url.searchParams.get('priority'), offset: url.searchParams.get('offset'), limit: url.searchParams.get('limit') }))
  if (pathname === '/api/v1/observations') return json(response, queryObservations(fixture, { query: url.searchParams.get('query'), subjectKind: url.searchParams.get('subject_kind'), subject: url.searchParams.get('subject'), gitSha: url.searchParams.get('git_sha'), offset: url.searchParams.get('offset'), limit: url.searchParams.get('limit') }))
  if (pathname === '/api/v1/observations/subject-facets') return json(response, queryObservationFacets(fixture, { query: url.searchParams.get('query'), subjectKind: url.searchParams.get('subject_kind'), gitSha: url.searchParams.get('git_sha'), offset: url.searchParams.get('offset'), limit: url.searchParams.get('limit') }))
  if (pathname === '/api/v1/observations/match') return json(response, { items: [], has_more: false })
  const projectOverview = pathname.match(/^\/api\/v1\/projects\/([^/]+)\/overview$/)
  if (projectOverview) return json(response, projectOverviewResponse(fixture, decodeURIComponent(projectOverview[1])))
  const taskOverview = pathname.match(/^\/api\/v1\/tasks\/([^/]+)\/overview$/)
  if (taskOverview) return json(response, taskOverviewResponse(fixture, decodeURIComponent(taskOverview[1])))
  const projectEntity = pathname.match(/^\/api\/v1\/projects\/([^/]+)$/)
  if (projectEntity) {
    const value = projects.get(decodeURIComponent(projectEntity[1]))
    return json(response, value || { error: 'project not found' }, value ? 200 : 404)
  }
  const taskEntity = pathname.match(/^\/api\/v1\/tasks\/([^/]+)$/)
  if (taskEntity) {
    const value = tasks.get(decodeURIComponent(taskEntity[1]))
    return json(response, value || { error: 'task not found' }, value ? 200 : 404)
  }
  const observationEntity = pathname.match(/^\/api\/v1\/observations\/([^/]+)$/)
  if (observationEntity) {
    const value = observations.get(decodeURIComponent(observationEntity[1]))
    return json(response, value || { error: 'observation not found' }, value ? 200 : 404)
  }
  if (pathname === `/api/v1/workspaces/${fixture.workspace.id}/timeline`) return json(response, queryTimelineEvents(fixture, { entityType: url.searchParams.get('entity_type'), eventType: url.searchParams.get('event_type'), since: url.searchParams.get('since'), until: url.searchParams.get('until'), offset: url.searchParams.get('offset'), limit: url.searchParams.get('limit') }))
  if (pathname === `/api/v1/workspaces/${fixture.workspace.id}/timeline/buckets`) {
    try {
      return json(response, queryTimelineBuckets(fixture, { since: url.searchParams.get('since'), until: url.searchParams.get('until'), bucket: url.searchParams.get('bucket') || 'week' }))
    } catch (error) {
      return json(response, { error: 'validation_error', message: error.message }, error.status || 400)
    }
  }
  return json(response, { error: `manual smoke route ${pathname}` }, 404)
}

const server = http.createServer(async (request, response) => {
  const url = new URL(request.url, 'http://local')
  if (url.pathname.startsWith('/api/v1/')) return api(request, response, url)
  if (url.pathname === '/hmem-runtime-config.js') {
    response.writeHead(200, { 'Content-Type': 'application/javascript', 'Cache-Control': 'no-store' })
    response.end("window.HMEM_CONFIG={authMode:'local',apiUrl:window.location.origin,wsUrl:'ws://perf.invalid/api/v1/ws',authTokenStorage:'memory'};")
    return
  }
  if (url.pathname === '/favicon.ico') { response.writeHead(204); response.end(); return }
  const relative = url.pathname === '/' || url.pathname.startsWith('/workspace/') ? 'index.html' : url.pathname.replace(/^\//, '')
  const file = path.resolve(staticRoot, relative)
  if (!file.startsWith(staticRoot) || !fs.existsSync(file)) { response.writeHead(404); response.end('not found'); return }
  if (file.endsWith('index.html')) {
    const html = fs.readFileSync(file, 'utf8').replace('<script type="module"', `${fakeWebSocket}<script type="module"`)
    response.writeHead(200, { 'Content-Type': 'text/html', 'Cache-Control': 'no-store' }); response.end(html); return
  }
  const contentType = file.endsWith('.js') ? 'application/javascript' : file.endsWith('.css') ? 'text/css' : 'application/octet-stream'
  response.writeHead(200, { 'Content-Type': contentType, 'Cache-Control': 'no-store' })
  fs.createReadStream(file).pipe(response)
})

await new Promise(resolve => server.listen(0, '127.0.0.1', resolve))
console.log(`PERF_SMOKE_URL=http://127.0.0.1:${server.address().port}/workspace/${fixture.workspace.id}`)
process.on('SIGINT', () => server.close(() => process.exit(0)))
