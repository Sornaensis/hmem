#!/usr/bin/env node
// Seeds and measures the throwaway Docker performance environment of
// tools/perftest/compose.yaml, described in
// docs/reference/request-performance.md ("Container sample"). It talks only to
// the hmem-perftest containers: HTTP on the loopback port Docker publishes for
// hmem-perftest-server, and psql through `docker exec hmem-perftest-postgres`.
//
//   node tools/perftest/perftest.mjs seed-planning [--workspaces 6]
//   node tools/perftest/perftest.mjs seed-history --updates N [--clients 16]
//   node tools/perftest/perftest.mjs seed-sessions [--scale 1] [--clients 8]
//   node tools/perftest/perftest.mjs clone-history --factor N
//   node tools/perftest/perftest.mjs spread-history
//   node tools/perftest/perftest.mjs manifest
//   node tools/perftest/perftest.mjs load [--rounds 100] [--profiles seq,c4,c16,c16_capture]
//   node tools/perftest/perftest.mjs plans [--variants a,b] [--min-ms 200] [--repeat 1]
//
// PERFTEST_DIR (default .tmp/perftest) holds secrets.env, state.json and
// results/. Credentials are read from secrets.env and never printed.
import { createHash, randomUUID } from 'node:crypto'
import { execFileSync, spawnSync } from 'node:child_process'
import { mkdirSync, readFileSync, writeFileSync, existsSync } from 'node:fs'
import { join } from 'node:path'

const DIR = process.env.PERFTEST_DIR || '.tmp/perftest'
const PG = 'hmem-perftest-postgres', SERVER = 'hmem-perftest-server'
const STATE = join(DIR, 'state.json')
const argv = process.argv.slice(3), opt = {}
for (let i = 0; i < argv.length; i += 2) opt[argv[i].replace(/^--/, '')] = argv[i + 1]
const num = (key, fallback) => opt[key] === undefined ? fallback : Number(opt[key])
const sleep = ms => new Promise(resolve => setTimeout(resolve, ms))
const r3 = x => Math.round(x * 1000) / 1000
const secrets = Object.fromEntries(readFileSync(join(DIR, 'secrets.env'), 'utf8').split(/\r?\n/).filter(Boolean)
  .map(line => [line.slice(0, line.indexOf('=')), line.slice(line.indexOf('=') + 1)]))
const loadState = () => existsSync(STATE) ? JSON.parse(readFileSync(STATE, 'utf8')) : { workspaces: [] }
const saveState = state => writeFileSync(STATE, JSON.stringify(state))

// ---------------------------------------------------------------- transport

let base = null
function server() {
  if (base) return base
  const mapped = execFileSync('docker', ['port', SERVER, '8420/tcp'], { encoding: 'utf8' }).trim().split(/\r?\n/)[0]
  if (!/^127\.0\.0\.1:\d+$/.test(mapped)) throw new Error('perftest server must be published on 127.0.0.1 only')
  return (base = 'http://' + mapped)
}
function psql(sql) {
  return execFileSync('docker', ['exec', '-i', PG, 'psql', '-U', 'hmem', '-d', 'hmem', '-AtqX', '-v', 'ON_ERROR_STOP=1'],
    { input: sql, encoding: 'utf8', maxBuffer: 256 << 20 }).trim()
}
const psqlJson = sql => JSON.parse(psql(sql) || 'null')

let counter = 0
async function call(ctx, label, method, path, body) {
  const rid = ctx.phase + '.' + label + '.' + counter++
  const data = body === undefined ? undefined : JSON.stringify(body)
  const started = performance.now()
  let status = 0, text = ''
  try {
    const response = await fetch(server() + path, { method, body: data, headers: {
      Authorization: 'Bearer ' + secrets.HMEM_API_KEY, 'Content-Type': 'application/json', 'X-Request-Id': rid,
      'X-HMem-Bot-Identity': JSON.stringify({ role: 'Perftest client ' + (ctx.index ?? 0), 'model-id': 'test:perftest:default' }) } })
    status = response.status
    text = await response.text()
  } catch (error) { text = String(error) }
  ctx.samples?.push({ rid, phase: ctx.phase, label, status, req: data ? Buffer.byteLength(data) : 0, res: Buffer.byteLength(text), clientMs: performance.now() - started })
  return { status, text }
}
async function must(ctx, label, method, path, body) {
  const { status, text } = await call(ctx, label, method, path, body)
  if (status < 200 || status >= 300) throw new Error(label + ' failed with HTTP ' + status + ': ' + text.slice(0, 300))
  return text ? JSON.parse(text) : null
}
async function pool(items, workers, work) {
  let next = 0
  await Promise.all(Array.from({ length: workers }, async (_, worker) => {
    while (next < items.length) { const i = next++; await work(items[i], i, worker) }
  }))
}

// ------------------------------------------------- deterministic generator
// SplitMix64 and vocabulary mirror RequestLatencyMeasurementSpec.

const MASK = (1n << 64n) - 1n
function generator(seed) {
  let state = BigInt(seed)
  return n => {
    state = (state + 0x9e3779b97f4a7c15n) & MASK
    let z = state
    z = ((z ^ (z >> 30n)) * 0xbf58476d1ce4e5b9n) & MASK
    z = ((z ^ (z >> 27n)) * 0x94d049bb133111ebn) & MASK
    return Number((z ^ (z >> 31n)) % BigInt(Math.max(1, n)))
  }
}
const SYLLABLES = ['ka', 'lo', 'mi', 're', 'tu', 'sa', 'vi', 'no', 'pe', 'da', 'fi', 'gu']
const VOCAB = SYLLABLES.flatMap(a => SYLLABLES.flatMap(b => SYLLABLES.map(c => a + b + c))).slice(0, 300)
const word = draw => { const u = draw(1000); return VOCAB[Math.floor(u * u * VOCAB.length / 1e6)] }
const sentence = (draw, n) => Array.from({ length: n }, () => word(draw)).join(' ')
const pad = n => String(n).padStart(2, '0')

// The large shape of hmem-server/frontend/perf/fixtures.mjs, seeded through the API.
async function seedWorkspace(seed, name, ctx) {
  const draw = generator(seed)
  const workspace = (await must(ctx, 'workspace_create', 'POST', '/api/v1/workspaces', { name, workspace_type: 'repository' })).id
  const projects = []
  for (let i = 0; i < 250; i++) {
    let parent = null
    if (i >= 25) { const candidate = projects[draw(projects.length)]; if (candidate.depth < 3) parent = candidate }
    const title = sentence(draw, 3), priority = draw(10) + 1
    const created = await must(ctx, 'project_create', 'POST', '/api/v1/projects',
      { workspace_id: workspace, name: title + ' ' + i, priority, ...(parent ? { parent_id: parent.id } : {}) })
    projects.push({ id: created.id, depth: parent ? parent.depth + 1 : 0 })
  }
  const task = async (label, extra) => {
    const title = sentence(draw, 4), description = sentence(draw, 24), priority = draw(10) + 1
    return (await must(ctx, label, 'POST', '/api/v1/tasks', { workspace_id: workspace, title, description, priority, ...extra })).id
  }
  const tops = []
  for (let i = 0; i < 1000; i++) { const p = projects[draw(projects.length)].id; tops.push({ id: await task('task_create', { project_id: p }), project: p }) }
  const subs = []
  for (let i = 0; i < 500; i++) {
    const parent = tops[draw(tops.length)]
    subs.push({ id: await task('subtask_create', { project_id: parent.project, parent_id: parent.id }), parent: parent.id })
  }
  const byProject = new Map()
  for (const t of tops) byProject.set(t.project, [...(byProject.get(t.project) || []), t.id])
  const eligible = [...byProject.keys()].sort().map(k => byProject.get(k)).filter(ts => ts.length >= 2)
  const dependents = new Set()
  for (let i = 0; i < 1200; i++) {
    const members = eligible[draw(eligible.length)], later = draw(members.length - 1) + 1, earlier = draw(later)
    const { status } = await call(ctx, 'dependency_link', 'POST', '/api/v1/tasks/' + members[later] + '/dependencies', { depends_on_id: members[earlier] })
    if (status < 300) dependents.add(members[later])
  }
  for (let i = 0; i < 2000; i++) {
    const subjects = Array.from({ length: draw(3) + 1 }, () => {
      const m = draw(20), glob = draw(10) < 1
      if (glob) return { subject_kind: 'glob', subject: 'src/m' + pad(m) + '/**/*.hs' }
      const s = draw(10), f = draw(20)
      return { subject_kind: 'file', subject: 'src/m' + pad(m) + '/s' + s + '/File' + f + '.hs' }
    })
    const gitSha = Array.from({ length: 40 }, () => '0123456789abcdef'[draw(16)]).join('')
    const content = sentence(draw, draw(90) + 30)
    await must(ctx, 'observation_create', 'POST', '/api/v1/observations', { workspace_id: workspace, subjects, git_sha: gitSha, content })
  }
  const parents = new Set(subs.map(s => s.parent))
  const topLeaves = tops.map(t => t.id).filter(id => !parents.has(id))
  const todo = new Set()
  for (const id of [...topLeaves, ...subs.map(s => s.id)]) {
    const r = draw(100), target = r < 20 ? 'done' : r < 30 ? 'in_progress' : r < 35 ? 'cancelled' : null
    if (target) await call(ctx, 'task_status_seed', 'PUT', '/api/v1/tasks/' + id, { status: target }); else todo.add(id)
  }
  return { id: workspace, name, seed, projectIds: projects.map(p => p.id), taskIds: [...tops.map(t => t.id), ...subs.map(s => s.id)],
    statusTargets: topLeaves.filter(id => todo.has(id) && !dependents.has(id)) }
}

async function seedPlanning() {
  const state = loadState(), count = num('workspaces', 6), started = performance.now()
  const missing = Array.from({ length: count }, (_, i) => i).filter(i => !state.workspaces[i])
  await Promise.all(missing.map(async i => {
    const seed = 0x9bb3fedd + i, name = i === 0 ? 'latency-fixture' : 'perftest-background-' + i
    state.workspaces[i] = await seedWorkspace(seed, name, { phase: 'seed', index: i })
  }))
  saveState(state)
  console.log(JSON.stringify({ seeded: missing.length, workspaces: state.workspaces.length, ms: Math.round(performance.now() - started) }))
}

// Field edits only, so status targets stay in todo. The measured workspace takes
// the share given by --primary-share; the rest is spread over the others.
async function seedHistory() {
  const state = loadState(), updates = num('updates', 0), clients = num('clients', 16), share = num('primary-share', 0.25)
  const draw = generator(0x5eed + (state.historyUpdates || 0)), started = performance.now()
  const items = Array.from({ length: updates }, () => {
    const w = draw(1e6) < share * 1e6 || state.workspaces.length === 1 ? 0 : 1 + draw(state.workspaces.length - 1)
    const ws = state.workspaces[w]
    return { task: ws.taskIds[draw(ws.taskIds.length)], body: draw(3) === 0
      ? { priority: draw(10) + 1 } : { title: sentence(draw, 4), description: sentence(draw, 24 + draw(120)) } }
  })
  let failures = 0
  await pool(items, clients, async (item, i, worker) => {
    const { status } = await call({ phase: 'history', index: worker }, 'task_history_update', 'PUT', '/api/v1/tasks/' + item.task, item.body)
    if (status >= 300) failures++
    if (i % 20000 === 0) console.error('history', i, Math.round(performance.now() - started) + ' ms')
  })
  const latest = loadState()
  latest.historyUpdates = (latest.historyUpdates || 0) + updates
  saveState(latest)
  console.log(JSON.stringify({ updates, failures, ms: Math.round(performance.now() - started) }))
}

// Production audit volume is impractical through the API here (about 20 task
// updates/s while sessions upload), so API-produced task update rows are
// replicated in SQL with fresh ids and request ids.
function cloneHistory() {
  const factor = num('factor', 0), started = performance.now()
  if (!Number.isInteger(factor) || factor < 1) throw new Error('--factor must be a positive integer')
  psql(`SET session_replication_role = replica;
    INSERT INTO audit_log (entity_type, entity_id, action, old_values, new_values, request_id, changed_at, workspace_id, actor_type, actor_id, actor_label, bot_identity)
    SELECT entity_type, entity_id, action, old_values, new_values, 'perftest-clone-' || gen_random_uuid(), changed_at, workspace_id, actor_type, actor_id, actor_label, bot_identity
    FROM audit_log, generate_series(1, ${factor}) WHERE entity_type = 'task' AND action = 'update' AND coalesce(request_id, '') NOT LIKE 'perftest-clone-%';
    SET session_replication_role = origin;`)
  console.log(JSON.stringify({ factor, ms: Math.round(performance.now() - started) }))
}

// Spread planning history uniformly over 90 days, as the isolated baseline
// does, then compact the rewritten audit table and refresh statistics.
function spreadHistory() {
  const started = performance.now()
  psql('SET session_replication_role = replica;' +
    ' UPDATE audit_log SET changed_at = now() - make_interval(secs => abs(hashtext(id::text)) % 7776000);' +
    ' SET session_replication_role = origin; VACUUM FULL audit_log; VACUUM ANALYZE;')
  console.log(JSON.stringify({ spread: true, ms: Math.round(performance.now() - started) }))
}

// --------------------------------------------------------------- sessions
// Transcript-like JSONL with about the production gzip ratio (2.44:1). Sizes
// are log-normal and normalised to the 2 Nov 2026 production totals.

const TERMINATED_BYTES = 3266580717, ACTIVE_BYTES = 347465270
function recordPool(draw) {
  const words = Array.from({ length: 16000 }, () => Array.from({ length: 2 + draw(3) }, () => SYLLABLES[draw(12)] + 'bcdfgklmnprstvz'[draw(15)]).join(''))
  const text = n => Array.from({ length: n }, () => words[Math.floor(Math.pow(draw(10000) / 10000, 1.3) * words.length)]).join(' ')
  const blob = n => Array.from({ length: n }, () => 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/'[draw(64)]).join('')
  return Array.from({ length: 2048 }, (_, i) => {
    const size = Math.min(60000, Math.floor(Math.exp(Math.log(1800) + 1.1 * gaussian(draw))))
    const kind = i % 4 === 0 ? 'tool_result' : i % 4 === 1 ? 'tool_use' : 'assistant'
    const body = kind === 'tool_result' && draw(3) === 0 ? blob(Math.floor(size * 0.6)) + ' ' + text(Math.max(4, Math.floor(size / 14)))
      : text(Math.max(4, Math.floor(size / 7)))
    return '"type":"' + kind + '","message":{"role":"' + (kind === 'tool_result' ? 'user' : 'assistant') + '","content":[{"type":"text","text":' + JSON.stringify(body) + '}]}'
  })
}
function gaussian(draw) {
  const u = (draw(1e9) + 1) / (1e9 + 1), v = (draw(1e9) + 1) / (1e9 + 1)
  return Math.sqrt(-2 * Math.log(u)) * Math.cos(2 * Math.PI * v)
}
function* records(draw, templates, startMs) {
  for (let n = 0; ; n++) {
    const ts = new Date(startMs + n * 1500).toISOString()
    yield Buffer.from('{"uuid":"' + randomUUID() + '","timestamp":"' + ts + '",' + templates[draw(templates.length)] + '}\n', 'utf8')
  }
}
function sizes(draw, count, total, sigma) {
  const raw = Array.from({ length: count }, () => Math.exp(sigma * gaussian(draw)))
  const sum = raw.reduce((a, b) => a + b, 0)
  return raw.map(x => Math.max(8192, Math.min(1 << 30, Math.round(x / sum * total))))
}

async function uploadSession(ctx, workspace, task, bytes, draw, templates, terminate) {
  const id = randomUUID(), path = '/api/v1/workspaces/' + workspace + '/sessions'
  let session = await must(ctx, 'session_create', 'POST', path, { id, task_id: task, title: 'Perftest transcript', format_label: 'claude-code-jsonl' })
  const hash = createHash('sha256'), source = records(draw, templates, Date.now() - 86400000 * (1 + draw(90)))
  let offset = 0, appends = 0
  while (offset < bytes) {
    const target = Math.min(1 << 20, 16384 << draw(5)), parts = []
    let length = 0
    while (length < target && offset + length < bytes) {
      const record = source.next().value
      if (length + record.length > 1 << 20) break
      parts.push(record); length += record.length
    }
    const chunk = Buffer.concat(parts)
    const result = await must(ctx, 'session_append', 'POST', path + '/' + id + '/append',
      { expected_offset: session.committed_bytes, expected_revision: session.revision, data_base64: chunk.toString('base64') })
    session = result.session; hash.update(chunk); offset += chunk.length; appends++
  }
  if (terminate) session = await must(ctx, 'session_terminate', 'POST', path + '/' + id + '/terminate',
    { expected_offset: session.committed_bytes, expected_revision: session.revision, final_bytes: offset, final_sha256: hash.digest('hex') })
  return { id, workspace, task, bytes: offset, appends, revision: session.revision, terminated: terminate }
}

async function seedSessions() {
  const state = loadState(), scale = num('scale', 1), clients = num('clients', 8), share = num('primary-share', 0.4)
  const draw = generator(0x5e55), templates = recordPool(generator(0x7e47)), started = performance.now()
  const plan = [...sizes(draw, Math.round(562 * scale), TERMINATED_BYTES * scale, 1.2).map(b => ({ bytes: b, terminate: true })),
                ...sizes(draw, Math.max(1, Math.round(12 * scale)), ACTIVE_BYTES * scale, 0.8).map(b => ({ bytes: b, terminate: false }))]
  const ctx = { phase: 'seed', index: 0 }
  // One work task per four sessions, in progress while capturing.
  for (const [i, item] of plan.entries()) {
    item.workspace = draw(1e6) < share * 1e6 ? 0 : 1 + draw(state.workspaces.length - 1)
    if (i % 4 === 0 || !item.terminate) {
      const ws = state.workspaces[item.workspace].id
      item.task = (await must(ctx, 'task_create', 'POST', '/api/v1/tasks', { workspace_id: ws, title: 'Session work ' + i, description: 'Perftest capture task' })).id
      await must(ctx, 'task_status_seed', 'PUT', '/api/v1/tasks/' + item.task, { status: 'in_progress' })
    } else { item.task = plan[i - (i % 4)].task; item.workspace = plan[i - (i % 4)].workspace }
  }
  const done = []
  await pool(plan, clients, async (item, i, worker) => {
    const sessionDraw = generator(0x5e55 + 1 + i)
    done[i] = await uploadSession({ phase: 'seed', index: worker }, state.workspaces[item.workspace].id, item.task, item.bytes, sessionDraw, templates, item.terminate)
    if (i % 50 === 0) console.error('sessions', i, Math.round(performance.now() - started) + ' ms')
  })
  const activeTasks = new Set(plan.filter(p => !p.terminate).map(p => p.task))
  for (const task of new Set(plan.filter(p => p.terminate).map(p => p.task))) {
    if (!activeTasks.has(task)) await call(ctx, 'task_status_seed', 'PUT', '/api/v1/tasks/' + task, { status: 'done' })
  }
  const latest = loadState()
  latest.sessions = done
  saveState(latest)
  console.log(JSON.stringify({ sessions: done.length, bytes: done.reduce((a, s) => a + s.bytes, 0),
    appends: done.reduce((a, s) => a + s.appends, 0), ms: Math.round(performance.now() - started) }))
}

// --------------------------------------------------------------- manifest

function manifest() {
  const tables = ['session_chunks', 'session_archive_parts', 'session_archive_checkpoints', 'session_archive_ledger', 'session_archive_content_blocks',
    'session_archive_generations', 'session_archive_jobs', 'session_occurrences', 'sessions', 'change_stream_outbox', 'audit_log', 'tasks', 'task_dependencies',
    'projects', 'observations', 'observation_subjects', 'workspaces']
  const sizesSql = tables.map(t => `'${t}', json_build_object('rows', (SELECT count(*) FROM ${t}), 'bytes', pg_total_relation_size('${t}'))`).join(', ')
  const out = psqlJson(`SELECT json_build_object('database_bytes', pg_database_size(current_database()), 'server_version', current_setting('server_version'),
    'relations', json_build_object(${sizesSql}),
    'sessions', (SELECT json_object_agg(state, json_build_object('count', n, 'bytes', b)) FROM (SELECT state, count(*) n, sum(committed_bytes) b FROM sessions GROUP BY state) s),
    'audit_span_days', (SELECT round(extract(epoch FROM max(changed_at) - min(changed_at)) / 86400) FROM audit_log),
    'measured_workspace', (SELECT json_build_object('audit_rows', count(*)) FROM audit_log a JOIN workspaces w ON w.id = a.workspace_id WHERE w.name = 'latency-fixture'))`)
  writeFileSync(join(DIR, 'manifest.json'), JSON.stringify(out, null, 1))
  console.log(JSON.stringify(out, null, 1))
}

// ------------------------------------------------------------------- load

function variants(f, now, clientIndex, clients, discovery) {
  const ws = f.id, pick = (xs, i) => xs[((i % xs.length) + xs.length) % xs.length]
  const task = r => pick(f.taskIds, clientIndex + r * clients * 13), project = r => pick(f.projectIds, clientIndex * 31 + r * 17)
  const owned = f.statusTargets.filter((_, i) => i % clients === clientIndex), vocab = r => pick(VOCAB, (clientIndex * 7 + r * 37) % 60)
  const iso = days => new Date(now + days * 86400000).toISOString().replace(/\.\d+Z$/, 'Z')
  const get = (label, path) => ({ label, run: (c, r) => call(c, label, 'GET', typeof path === 'function' ? path(r) : path) })
  return [
    get('health', '/api/v1/health'),
    get('task_get', r => '/api/v1/tasks/' + task(r)),
    { label: 'task_update_fields', run: (c, r) => call(c, 'task_update_fields', 'PUT', '/api/v1/tasks/' + task(r + 7), { title: 'Measured update ' + r, priority: 1 + r % 10 }) },
    { label: 'task_update_status', run: (c, r) => call(c, 'task_update_status', 'PUT', '/api/v1/tasks/' + pick(owned, Math.floor(r / 2)),
      { status: r % 2 === 0 ? 'in_progress' : 'todo' }) },
    get('task_list_query', r => '/api/v1/tasks?workspace_id=' + ws + '&query=' + vocab(r) + '&limit=50'),
    { label: 'search_unified', run: (c, r) => call(c, 'search_unified', 'POST', '/api/v1/search', { workspace_id: ws, query: vocab(r + 3), limit: 10 }) },
    get('nav_root', '/api/v1/workspaces/' + ws + '/navigation?parent_kind=workspace_root'),
    get('nav_project', r => '/api/v1/workspaces/' + ws + '/navigation?parent_kind=project&parent_id=' + project(r)),
    { label: 'nav_batch', run: (c, r) => call(c, 'nav_batch', 'POST', '/api/v1/workspaces/' + ws + '/navigation/batch',
      { requests: [0, 1, 2, 3].map(i => ({ request_id: 'p' + i, parent: { kind: 'project', parent_id: project(r + i) } })) }) },
    get('nav_focus', r => '/api/v1/workspaces/' + ws + '/navigation/focus/task/' + task(r + 11)),
    get('planning_counts', '/api/v1/workspaces/' + ws + '/planning-counts'),
    get('project_overview', r => '/api/v1/projects/' + project(r) + '/overview'),
    get('observation_list_query', r => '/api/v1/observations?workspace_id=' + ws + '&query=' + vocab(r + 5)),
    get('observation_subject_tree', '/api/v1/observations/subject-tree?workspace_id=' + ws + '&prefix=src'),
    { label: 'discovery_content', run: async (c, r) => {
      const { status, text } = await call(c, 'discovery_content', 'POST', '/api/v1/observations/discovery', { workspace_id: ws,
        search_session: discovery(c, 1), generation: ++discovery.generation, filters: { content_query: vocab(r + 9) }, view: 'list', limit: 50 })
      const token = status < 300 ? JSON.parse(text).context_token : null
      if (token) await call(c, 'discovery_continue', 'POST', '/api/v1/observations/discovery', { workspace_id: ws, context_token: token, view: 'list', offset: 50, limit: 50 })
    } },
    { label: 'discovery_subject_tree', run: (c, r) => call(c, 'discovery_subject_tree', 'POST', '/api/v1/observations/discovery', { workspace_id: ws,
      search_session: discovery(c, 2), generation: ++discovery.generation, filters: { subject_query: 'src/m' + (r % 10) }, view: 'tree', prefix: 'src' }) },
    get('timeline_buckets_week', '/api/v1/workspaces/' + ws + '/timeline/buckets?since=' + iso(-91) + '&until=' + iso(1) + '&bucket=week'),
    get('timeline_buckets_day', '/api/v1/workspaces/' + ws + '/timeline/buckets?since=' + iso(-91) + '&until=' + iso(1) + '&bucket=day'),
    get('timeline_buckets_all_time', '/api/v1/workspaces/' + ws + '/timeline/buckets?until=' + iso(1) + '&bucket=week&paged=true'),
    get('timeline_events', '/api/v1/workspaces/' + ws + '/timeline?limit=50'),
  ]
}

const SAMPLER = "SELECT count(*) FILTER (WHERE a.state = 'active') || '|' || count(*) FILTER (WHERE a.wait_event_type = 'Lock') || '|' || (SELECT count(*) FROM pg_locks WHERE NOT granted) || '|' || coalesce(string_agg(coalesce(a.wait_event_type, 'CPU') || ':' || coalesce(a.wait_event, '-'), ',') FILTER (WHERE a.state = 'active'), '') FROM pg_stat_activity a WHERE a.datname = current_database() AND a.pid <> pg_backend_pid() AND a.backend_type = 'client backend' \\watch 0.025\n"
const DB_COUNTERS = 'SELECT row_to_json(d) FROM (SELECT xact_commit, xact_rollback, blks_read, blks_hit, temp_files, temp_bytes, deadlocks' +
  ' FROM pg_stat_database WHERE datname = current_database()) d'
const health = async () => JSON.parse((await call({ phase: 'probe' }, 'health_probe', 'GET', '/api/v1/health')).text).pool?.checkout

async function appender(session, stop, index, samples) {
  const ctx = { phase: stop.phase, index: 100 + index, samples }, path = '/api/v1/workspaces/' + session.workspace + '/sessions/' + session.id
  const current = JSON.parse((await call({ phase: 'probe' }, 'session_get', 'GET', path)).text)
  let offset = current.committed_bytes, revision = current.revision
  const draw = generator(0xa99 + index + offset), templates = appender.templates ||= recordPool(generator(0x7e47)), source = records(draw, templates, Date.now())
  while (!stop.done) {
    const parts = []; let length = 0
    while (length < 4096 + draw(28672)) { const record = source.next().value; parts.push(record); length += record.length }
    const { status, text } = await call(ctx, 'session_append', 'POST', path + '/append',
      { expected_offset: offset, expected_revision: revision, data_base64: Buffer.concat(parts).toString('base64') })
    if (status < 300) { const s = JSON.parse(text).session; offset = s.committed_bytes; revision = s.revision } else await sleep(1000)
    await sleep(250)
  }
}

async function phase(name, clients, rounds, fixture, now, appenders, samples, discovery) {
  const hasStatements = psql("SELECT count(*) FROM pg_extension WHERE extname = 'pg_stat_statements'") === '1'
  if (hasStatements) psql('SELECT pg_stat_statements_reset()')
  const poolBefore = await health(), dbBefore = JSON.parse(psql(DB_COUNTERS))
  execFileSync('docker', ['exec', '-i', PG, 'sh', '-c', 'cat > /tmp/perftest-sampler.sql'], { input: SAMPLER })
  execFileSync('docker', ['exec', '-d', PG, 'sh', '-c',
    'echo $$ > /tmp/perftest-sampler.pid; exec psql -U hmem -d hmem -AtqX -o /tmp/perftest-activity.txt -f /tmp/perftest-sampler.sql'])
  const stop = { done: false, phase: name }, started = performance.now()
  const appending = (appenders || []).map((s, i) => appender(s, stop, i, samples))
  await Promise.all(Array.from({ length: clients }, async (_, i) => {
    const vs = variants(fixture, now, i, clients, discovery), rotated = [...vs.slice(i % vs.length), ...vs.slice(0, i % vs.length)]
    for (let r = 0; r < rounds; r++) for (const v of rotated) await v.run({ phase: name, index: i, samples }, r)
  }))
  stop.done = true
  await Promise.all(appending)
  const elapsedMs = performance.now() - started
  execFileSync('docker', ['exec', PG, 'sh', '-c', 'kill $(cat /tmp/perftest-sampler.pid)'])
  await sleep(1100)
  const poolAfter = await health(), dbAfter = JSON.parse(psql(DB_COUNTERS))
  const activity = execFileSync('docker', ['exec', PG, 'cat', '/tmp/perftest-activity.txt'], { encoding: 'utf8', maxBuffer: 256 << 20 })
  const rows = activity.split('\n').filter(l => l.includes('|')).map(l => l.split('|'))
  const events = {}
  for (const [, , , list] of rows) for (const e of list.split(',').filter(Boolean)) events[e] = (events[e] || 0) + 1
  const statements = hasStatements ? psqlJson("SELECT coalesce(json_agg(s), '[]') FROM (SELECT left(regexp_replace(query, '\\s+', ' ', 'g'), 120) query, calls, round(total_exec_time::numeric, 1) total_ms, round(mean_exec_time::numeric, 3) mean_ms, round(max_exec_time::numeric, 1) max_ms, rows, shared_blks_hit hit, shared_blks_read read, temp_blks_written temp_written FROM pg_stat_statements WHERE dbid = (SELECT oid FROM pg_database WHERE datname = current_database()) AND query NOT LIKE '%pg_stat_%' AND query NOT LIKE '%pg_locks%' ORDER BY total_exec_time DESC LIMIT 12) s") : []
  const delta = (a, b) => Object.fromEntries(Object.keys(b).filter(k => typeof b[k] === 'number' && k !== 'wait_max_ms').map(k => [k, r3(b[k] - (a[k] || 0))]))
  return { phase: name, clients, rounds, elapsedMs: r3(elapsedMs), appenders: (appenders || []).length,
    pool: { ...delta(poolBefore, poolAfter), wait_max_ms_process: poolAfter.wait_max_ms,
      wait_buckets: poolAfter.wait_buckets.map((b, i) => ({ le_ms: b.le_ms, count: b.count - poolBefore.wait_buckets[i].count })) },
    activity: { samples: rows.length, maxActive: Math.max(0, ...rows.map(r => +r[0])), samplesWithLockWait: rows.filter(r => +r[1] > 0).length,
      maxUngrantedLocks: Math.max(0, ...rows.map(r => +r[2])), activeWaitEvents: events },
    database: delta(dbBefore, dbAfter), statements }
}

function harvestLog() {
  const text = execFileSync('docker', ['exec', SERVER, 'sh', '-c', 'cat /var/lib/hmem/.hmem/logs/hmem-server.log*'], { encoding: 'utf8', maxBuffer: 512 << 20 })
  const entries = new Map()
  for (const line of text.split('\n')) {
    if (!line.startsWith('{') || !line.includes('"request_id"')) continue
    try { const e = JSON.parse(line); if (e.request_id) entries.set(e.request_id, e) } catch {}
  }
  return entries
}
const pct = (xs, p) => { if (!xs.length) return 0; const s = [...xs].sort((a, b) => a - b); return s[Math.max(0, Math.min(s.length - 1, Math.ceil(p * s.length) - 1))] }
const mean = (xs, f) => xs.length ? r3(xs.reduce((a, x) => a + f(x), 0) / xs.length) : 0
function summarize(phaseName, rows) {
  const groups = new Map()
  for (const r of rows.filter(r => r.s.phase === phaseName)) groups.set(r.s.label, [...(groups.get(r.s.label) || []), r])
  return [...groups].map(([label, group]) => {
    const warm = phaseName === 'seq' ? group.slice(1) : group, completes = warm.map(r => r.e.complete_ms), threshold = pct(completes, 0.95)
    const app = r => Math.max(0, r.e.complete_ms - r.e.db_wait_ms - r.e.db_hold_ms - (r.e.provider_ms || 0)), tail = warm.filter(r => r.e.complete_ms >= threshold)
    const count = f => Object.fromEntries([...group.reduce((m, r) => m.set(f(r), (m.get(f(r)) || 0) + 1), new Map())])
    return { phase: phaseName, variant: label, route: group[0].e.route, n: warm.length, status: count(r => Math.floor(r.s.status / 100) + 'xx'), outcome: count(r => r.e.outcome),
      completeMs: [0.5, 0.95, 0.99, 1].map(p => r3(pct(completes, p))), prepareP50: r3(pct(warm.map(r => r.e.prepare_ms ?? 0), 0.5)),
      clientP50: r3(pct(warm.map(r => r.s.clientMs), 0.5)),
      meanMs: { wait: mean(warm, r => r.e.db_wait_ms), connect: mean(warm, r => r.e.db_connect_ms), hold: mean(warm, r => r.e.db_hold_ms),
        provider: mean(warm, r => r.e.provider_ms || 0), app: mean(warm, app) },
      checkouts: mean(warm, r => r.e.db_checkouts),
      tail: { n: tail.length, complete: mean(tail, r => r.e.complete_ms), wait: mean(tail, r => r.e.db_wait_ms), hold: mean(tail, r => r.e.db_hold_ms),
        app: mean(tail, app) },
      bytes: [mean(warm, r => r.s.req), mean(warm, r => r.s.res)] }
  }).sort((a, b) => b.completeMs[1] - a.completeMs[1])
}

// The image the perftest server container actually runs, and its labels.
function serverImage() {
  const id = execFileSync('docker', ['inspect', '--format', '{{.Image}}', SERVER], { encoding: 'utf8' }).trim()
  const labels = JSON.parse(execFileSync('docker', ['image', 'inspect', '--format', '{{json .Config.Labels}}', id],
    { encoding: 'utf8' }).trim() || 'null') || {}
  return { id, revision: labels['hmem.perftest.revision'] || null, labels }
}

async function load() {
  const state = loadState(), fixture = state.workspaces[0], rounds = num('rounds', 100), now = Date.now()
  const profiles = (opt.profiles || 'seq,c4,c16,c16_capture').split(',')
  const unknown = profiles.filter(p => !['seq', 'c4', 'c16', 'c16_capture'].includes(p))
  if (unknown.length) throw new Error('unknown profiles: ' + unknown.join(','))
  const image = serverImage()
  if (!/^[0-9a-f]{40}$/.test(image.revision || '')) throw new Error(SERVER + ' runs ' + image.id + ' without a hmem.perftest.revision label')
  const active = (state.sessions || []).filter(s => !s.terminated).slice(0, 4)
  const out = join(DIR, 'results', new Date().toISOString().replace(/[:.]/g, '-')), samples = []
  mkdirSync(out, { recursive: true })
  const phases = []
  const result = { revision: image.revision, image: image.id, imageLabels: image.labels, startedAt: new Date(now).toISOString(), rounds,
    samples: 0, joined: 0, phases, routes: [] }
  // The summary is written even when a phase or the log harvest fails.
  try {
    // One discovery search session per client and view for the whole run, so
    // later phases replace contexts instead of adding to the server's limit.
    const discovery = (c, tag) => '9bb3fedd-' + String(c.index).padStart(4, '0') + '-4000-8' + String(tag).padStart(3, '0') + '-' + discovery.run
    discovery.generation = 0; discovery.run = randomUUID().slice(-12)
    // Warm-up: one unrecorded round of every variant.
    for (const v of variants(fixture, now, 0, 1, discovery)) await v.run({ phase: 'warm', index: 0 }, 0)
    const spec = { seq: [1, rounds, null], c4: [4, Math.ceil(rounds / 4), null], c16: [16, Math.ceil(rounds / 12), null],
      c16_capture: [16, Math.ceil(rounds / 12), active] }
    for (const name of profiles) {
      const [clients, r, appenders] = spec[name]
      phases.push(await phase(name, clients, r, fixture, now, appenders, samples, discovery))
      console.error(name, Math.round(phases.at(-1).elapsedMs) + ' ms')
    }
    // Flush the per-capability log buffers with cheap unmatched requests, then harvest.
    await pool(Array.from({ length: 500 }), 48, () => call({ phase: 'flush' }, 'flush', 'GET', '/api/v1/perftest-flush'))
    await sleep(2000)
    const entries = harvestLog(), rows = samples.flatMap(s => entries.has(s.rid) ? [{ s, e: entries.get(s.rid) }] : [])
    result.joined = rows.length
    result.routes = profiles.flatMap(p => summarize(p, rows))
  } catch (error) {
    result.error = String(error?.stack || error)
    process.exitCode = 1
  }
  result.samples = samples.length
  try {
    const after = serverImage().id
    if (after !== image.id) { result.imageAfter = after; process.exitCode = 1 }
  } catch (error) { result.imageAfterError = String(error?.message || error) }
  writeFileSync(join(out, 'summary.json'), JSON.stringify(result, null, 1))
  if (result.error) console.error(result.error)
  console.log(join(out, 'summary.json'))
}

// Matched plans: auto_explain (analyze, buffers) for statements slower than
// --min-ms while each named variant runs --repeat times. The perftest server is
// restarted so its pooled connections load the settings, and again afterwards.
async function plans() {
  const labels = (opt.variants || 'timeline_buckets_day').split(','), minMs = num('min-ms', 200), repeat = num('repeat', 1)
  const settings = [['session_preload_libraries', "'auto_explain'"], ['auto_explain.log_min_duration', String(minMs)], ['auto_explain.log_analyze', 'on'],
    ['auto_explain.log_buffers', 'on'], ['auto_explain.log_nested_statements', 'on'], ['auto_explain.log_parameter_max_length', '0']]
  const restart = async () => {
    execFileSync('docker', ['restart', SERVER])
    for (let i = 0; i < 120 && (await call({ phase: 'probe' }, 'health_probe', 'GET', '/api/v1/health')).status !== 200; i++) await sleep(1000)
  }
  const state = loadState(), since = new Date().toISOString(), discovery = (c, tag) => '9bb3fedd-0000-4000-9' + String(tag).padStart(3, '0') + '-' + discovery.run
  discovery.generation = 0; discovery.run = randomUUID().slice(-12)
  try {
    for (const [key, value] of settings) psql(`ALTER DATABASE hmem SET ${key} = ${value}`)
    await restart()
    const vs = variants(state.workspaces[0], Date.now(), 0, 1, discovery)
    for (const label of labels) for (let r = 0; r < repeat; r++) await vs.find(v => v.label === label).run({ phase: 'plan', index: 0 }, r)
  } finally {
    for (const [key] of settings) psql(`ALTER DATABASE hmem RESET ${key}`)
    await restart()
  }
  const file = join(DIR, 'results', 'plans-' + since.replace(/[:.]/g, '-') + '.log')
  mkdirSync(join(DIR, 'results'), { recursive: true })
  const logs = spawnSync('docker', ['logs', '--since', since, PG], { encoding: 'utf8', maxBuffer: 256 << 20 })
  writeFileSync(file, logs.stdout + logs.stderr)
  console.log(file)
}

const commands = { 'seed-planning': seedPlanning, 'seed-history': seedHistory, 'seed-sessions': seedSessions, 'clone-history': cloneHistory,
  'spread-history': spreadHistory, manifest, load, plans }
const command = commands[process.argv[2]]
if (!command) { console.error('usage: perftest.mjs ' + Object.keys(commands).join('|')); process.exit(2) }
await command()
