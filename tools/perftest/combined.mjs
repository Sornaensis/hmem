#!/usr/bin/env node
// Combined contention workload for the throwaway Docker environment of
// tools/perftest/compose.yaml; see docs/reference/server-performance-qualification.md.
// It drives the running hmem-perftest-server through its loopback port only,
// runs psql only through `docker exec hmem-perftest-postgres`, reads the server
// log and process status through `docker exec hmem-perftest-server`, and runs
// one stalled WebSocket observer as a throwaway node container on the perftest
// network. Credentials and tickets are never printed.
//
//   node tools/perftest/combined.mjs [--label NAME] [--idle-s 30] [--contention-s 120]
//
// Reads PERFTEST_DIR (default .tmp/perftest) secrets.env and state.json from
// perftest.mjs seeding, writes one summary JSON to PERFTEST_OUT (default
// .tmp/perftest/combined) and prints its path. Run it once per fresh copy of the
// same seeded volume: it creates workspaces and sessions and stops the server.
// Contention events start at 20 to 70 s, so --contention-s must be at least 80.
// Unknown, repeated or valueless flags are rejected. The server image needs the
// hmem.perftest.revision label; PERFTEST_OBSERVER_IMAGE overrides the stalled
// observer's node image, pinned by default to the digest the receipt records.
import { createHash, randomUUID } from 'node:crypto'
import { execFileSync, spawn } from 'node:child_process'
import { mkdirSync, readFileSync, writeFileSync } from 'node:fs'
import { join } from 'node:path'

const DIR = process.env.PERFTEST_DIR || '.tmp/perftest', OUT = process.env.PERFTEST_OUT || join(DIR, 'combined')
const PG = 'hmem-perftest-postgres', SERVER = 'hmem-perftest-server', NETWORK = 'hmem-perftest-net'
const OBSERVER_IMAGE = process.env.PERFTEST_OBSERVER_IMAGE || 'node:24.18.0-bookworm@sha256:5711a0d445a1af54af9589066c646df387d1831a608226f4cd694fc59e745059'
const USAGE = 'usage: node tools/perftest/combined.mjs [--label NAME] [--idle-s 30] [--contention-s 120]'
const argv = process.argv.slice(2), opt = {}
for (let i = 0; i < argv.length; i += 2) {
  const name = argv[i].startsWith('--') ? argv[i].slice(2) : '', value = argv[i + 1]
  if (!['label', 'idle-s', 'contention-s'].includes(name) || name in opt || value === undefined || value.startsWith('--')) throw new Error(USAGE)
  opt[name] = value
}
if (opt.label !== undefined && !/^[A-Za-z0-9._-]{1,64}$/.test(opt.label)) throw new Error('--label must be 1-64 letters, digits, dots, underscores or hyphens')
const IDLE_MS = Number(opt['idle-s'] || 30) * 1000, CONTENTION_MS = Number(opt['contention-s'] || 120) * 1000
if (!(IDLE_MS > 0) || !(CONTENTION_MS >= 80000)) throw new Error('--idle-s must be positive and --contention-s at least 80')
const sleep = ms => new Promise(resolve => setTimeout(resolve, ms))
const r1 = x => Math.round(x * 10) / 10
const secrets = Object.fromEntries(readFileSync(join(DIR, 'secrets.env'), 'utf8').split(/\r?\n/).filter(Boolean)
  .map(line => [line.slice(0, line.indexOf('=')), line.slice(line.indexOf('=') + 1)]))
const state = JSON.parse(readFileSync(join(DIR, 'state.json'), 'utf8'))
const psql = sql => execFileSync('docker', ['exec', '-i', PG, 'psql', '-U', 'hmem', '-d', 'hmem', '-AtqX', '-v', 'ON_ERROR_STOP=1'],
  { input: sql, encoding: 'utf8', maxBuffer: 64 << 20 }).trim()
const pct = (xs, p) => { if (!xs.length) return null; const s = [...xs].sort((a, b) => a - b); return r1(s[Math.max(0, Math.ceil(p * s.length) - 1)]) }
const dist = xs => ({ n: xs.length, p50: pct(xs, 0.5), p95: pct(xs, 0.95), p99: pct(xs, 0.99), max: pct(xs, 1) })

// ---------------------------------------------------------------- transport

const mapped = execFileSync('docker', ['port', SERVER, '8420/tcp'], { encoding: 'utf8' }).trim().split(/\r?\n/)[0]
if (!/^127\.0\.0\.1:\d+$/.test(mapped)) throw new Error('perftest server must be published on 127.0.0.1 only')
const BASE = 'http://' + mapped
const IDENTITY = JSON.stringify({ role: 'Perftest combined', 'model-id': 'test:perftest:default' })
const samples = []
let counter = 0
async function call(phase, label, method, path, body, keep = true) {
  const rid = 'combined.' + phase + '.' + label + '.' + counter++, data = body === undefined ? undefined : JSON.stringify(body)
  const started = performance.now()
  let status = 0, text = '', error
  try {
    const response = await fetch(BASE + path, { method, body: data, signal: AbortSignal.timeout(60000), headers: {
      Authorization: 'Bearer ' + secrets.HMEM_API_KEY, 'Content-Type': 'application/json', 'X-Request-Id': rid, 'X-HMem-Bot-Identity': IDENTITY } })
    status = response.status
    text = await response.text()
  } catch (e) { error = e.name || 'error' }
  const ended = performance.now()
  if (keep) samples.push({ rid, phase, label, status, error, started, ms: ended - started })
  return { status, text, rid, started, ended, json: () => JSON.parse(text) }
}
async function must(phase, label, method, path, body) {
  const result = await call(phase, label, method, path, body)
  if (result.status < 200 || result.status >= 300) throw new Error(label + ' failed with HTTP ' + result.status + ' ' + (result.error || result.text.slice(0, 200)))
  return result.text ? result.json() : null
}

// ------------------------------------------------------------ change stream

const scopeOf = workspace => ({ scope: 'workspace', workspace_id: workspace })
async function resync(phase, workspace, profile) {
  const started = performance.now()
  let page = await must(phase, 'resync_' + profile, 'POST', '/api/v1/change-stream/resync',
    { scope: scopeOf(workspace), snapshot_profile: profile, page_size: 100, start_idempotency_key: randomUUID() + randomUUID() })
  let pages = 1, items = page.items.length
  while (page.has_more) {
    page = await must(phase, 'resync_page', 'POST', '/api/v1/change-stream/resync', { scope: scopeOf(workspace), page_token: page.next_page_token })
    pages++; items += page.items.length
  }
  return { profile, pages, items, ms: r1(performance.now() - started), resume: page.resume_token }
}
async function ticket(phase, workspace, resume) {
  return (await must(phase, 'ticket', 'POST', '/api/v1/change-stream/ticket', { scope: scopeOf(workspace), resume_token: resume })).ticket
}

// A healthy observer reads every frame and records its arrival time.
function observe(name, workspace, ticketValue) {
  const o = { name, workspace, frames: [], closed: null, opened: null, error: null }
  const started = performance.now()
  const socket = new WebSocket(BASE.replace('http', 'ws') + '/api/v1/ws?ticket=' + encodeURIComponent(ticketValue))
  o.ready = new Promise(resolve => {
    socket.onopen = () => { o.opened = performance.now() - started; resolve(true) }
    socket.onerror = () => { o.error = 'error'; resolve(false) }
  })
  socket.onmessage = event => {
    let frame = null
    try { frame = JSON.parse(String(event.data)) } catch {}
    o.frames.push({ at: performance.now(), type: frame?.type, rid: frame?.event?.transaction?.request_id, catchUp: frame?.catch_up })
  }
  socket.onclose = event => { o.closed = { at: performance.now(), code: event.code } }
  o.close = () => { try { socket.close() } catch {} }
  return o
}
const firstFrame = (o, type, after = 0) => new Promise(resolve => {
  const check = () => { const f = o.frames.find(f => f.type === type && f.at >= after); if (f || o.closed) resolve(f || null); else setTimeout(check, 10) }
  check()
})

// The stalled observer runs on the perftest network so that only kernel socket
// buffers lie between it and the server. It completes the handshake, stops
// reading for HOLD_MS, then drains for 5 s and reports what it received.
const STALLED = `
import net from 'node:net'
const hold = Number(process.env.HOLD_MS), started = performance.now(), out = { handshake: null, bytesBeforePause: 0, bytesAfterResume: 0, frames: {}, close: null, ended: null }
const s = net.connect(8420, 'hmem-perftest-server')
let buf = Buffer.alloc(0), upgraded = false
s.on('data', chunk => {
  if (!upgraded) {
    buf = Buffer.concat([buf, chunk]); const at = buf.indexOf('\\r\\n\\r\\n'); if (at < 0) return
    out.handshake = buf.subarray(0, buf.indexOf('\\r\\n')).toString(); upgraded = true; buf = buf.subarray(at + 4)
    s.pause(); setTimeout(() => { out.resumedAtMs = Math.round(performance.now() - started); s.resume() }, hold); return
  }
  if (out.resumedAtMs === undefined) out.bytesBeforePause += chunk.length; else out.bytesAfterResume += chunk.length
  buf = Buffer.concat([buf, chunk])
  for (;;) {
    if (buf.length < 2) break
    let len = buf[1] & 127, off = 2
    if (len === 126) { if (buf.length < 4) break; len = buf.readUInt16BE(2); off = 4 } else if (len === 127) { if (buf.length < 10) break; len = Number(buf.readBigUInt64BE(2)); off = 10 }
    if (buf.length < off + len) break
    const op = buf[0] & 15, payload = buf.subarray(off, off + len); buf = buf.subarray(off + len)
    if (op === 8) out.close = payload.length >= 2 ? payload.readUInt16BE(0) : 0
    else if (op === 1) { let t = 'unparsed'; try { t = JSON.parse(payload.toString()).type } catch {} out.frames[t] = (out.frames[t] || 0) + 1 }
  }
})
s.on('end', () => { out.ended = Math.round(performance.now() - started) })
s.on('error', e => { out.error = e.code })
s.write('GET /api/v1/ws?ticket=' + encodeURIComponent(process.env.TICKET) + ' HTTP/1.1\\r\\nHost: hmem-perftest-server:8420\\r\\nUpgrade: websocket\\r\\nConnection: Upgrade\\r\\nSec-WebSocket-Key: ' + Buffer.from(crypto.getRandomValues(new Uint8Array(16))).toString('base64') + '\\r\\nSec-WebSocket-Version: 13\\r\\n\\r\\n')
setTimeout(() => { s.destroy(); console.log(JSON.stringify(out)); process.exit(0) }, hold + 5000)
`
function stalledObserver(ticketValue, holdMs) {
  const child = spawn('docker', ['run', '--rm', '-i', '--name', 'hmem-perftest-stalled-observer', '--network', NETWORK, '--memory', '256m',
    '-e', 'TICKET', '-e', 'HOLD_MS=' + holdMs, OBSERVER_IMAGE, 'node', '--input-type=module', '-'], { env: { ...process.env, TICKET: ticketValue } })
  let stdout = '', stderr = ''
  child.stdout.on('data', d => { stdout += d }); child.stderr.on('data', d => { stderr += d })
  child.stdin.end(STALLED)
  return new Promise(resolve => child.on('close', code => {
    try { resolve({ exit: code, ...JSON.parse(stdout.trim().split('\n').at(-1)) }) } catch { resolve({ exit: code, error: stderr.slice(0, 300) }) }
  }))
}

// --------------------------------------------------------------- telemetry

const STATEMENTS = "SELECT coalesce(json_agg(s), '[]') FROM (SELECT left(regexp_replace(query, '\\s+', ' ', 'g'), 160) query, calls, round(total_exec_time::numeric, 1) total_ms, rows FROM pg_stat_statements WHERE dbid = (SELECT oid FROM pg_database WHERE datname = current_database()) AND query NOT LIKE '%pg_stat_%' AND query NOT LIKE '%pg_locks%') s"
const CLASSES = {
  outbox_scan: /FROM change_stream_outbox/i, scope_lock: /hmem_change_stream_lock/i,
  snapshot_item_insert: /INSERT INTO change_stream_snapshot_items/i, snapshot_session_insert: /INSERT INTO change_stream_snapshot_sessions/i,
  snapshot_token_insert: /INSERT INTO change_stream_snapshot_page_tokens/i, resume_token: /change_stream_resume_tokens/i,
  authorization: /workspace_memberships|workspace_member|grant_users|user_workspace|global_permission|superadmin|hmem_.*authori[sz]/i,
}
function statements() {
  const rows = JSON.parse(psql(STATEMENTS) || '[]'), classes = {}
  for (const [name, re] of Object.entries(CLASSES)) {
    const hit = rows.filter(r => re.test(r.query))
    classes[name] = { statements: hit.length, calls: hit.reduce((a, r) => a + Number(r.calls), 0), total_ms: r1(hit.reduce((a, r) => a + Number(r.total_ms), 0)) }
  }
  return { classes, total_calls: rows.reduce((a, r) => a + Number(r.calls), 0),
    top: rows.sort((a, b) => b.calls - a.calls).slice(0, 8).map(r => ({ calls: Number(r.calls), total_ms: Number(r.total_ms), query: r.query })) }
}
const resetStatements = () => psql('SELECT pg_stat_statements_reset()')
const health = async () => { const r = await call('probe', 'health', 'GET', '/api/v1/health', undefined, false); try { return r.json().pool?.checkout ?? null } catch { return null } }
const checkoutDelta = (a, b) => a && b ? Object.fromEntries(['checkouts', 'unattributed', 'capacity_waits', 'wait_total_ms', 'hold_total_ms', 'connects']
  .map(k => [k, r1(b[k] - a[k])])) : null
function serverMemory() {
  const text = execFileSync('docker', ['exec', SERVER, 'sh', '-c',
    'for p in /proc/[0-9]*; do [ "$(cat $p/comm 2>/dev/null)" = hmem-server ] && grep -E "^(VmRSS|VmHWM):" $p/status; done'], { encoding: 'utf8' })
  const kb = name => Number((text.match(new RegExp(name + ':\\s+(\\d+)')) || [])[1] || NaN)
  return { rss: kb('VmRSS') * 1024, hwm: kb('VmHWM') * 1024 }
}
function sampler(intervalMs) {
  const s = { rows: [], stop: false }
  s.done = (async () => {
    while (!s.stop) {
      const at = performance.now()
      try {
        const mem = serverMemory()
        const [active, lockWaits, ungranted] = psql("SELECT count(*) FILTER (WHERE state = 'active') || '|' || count(*) FILTER (WHERE wait_event_type = 'Lock') || '|' || (SELECT count(*) FROM pg_locks WHERE NOT granted) FROM pg_stat_activity WHERE datname = current_database() AND backend_type = 'client backend' AND pid <> pg_backend_pid()").split('|').map(Number)
        s.rows.push({ at, ...mem, active, lockWaits, ungranted })
      } catch (e) { s.rows.push({ at, error: String(e.message).slice(0, 80) }) }
      await sleep(Math.max(0, intervalMs - (performance.now() - at)))
    }
  })()
  return s
}
const memorySummary = (rows, from = -Infinity, to = Infinity) => {
  const xs = rows.filter(r => r.at >= from && r.at <= to && Number.isFinite(r.rss))
  return { samples: xs.length, rss_min: Math.min(...xs.map(r => r.rss)), rss_max: Math.max(...xs.map(r => r.rss)), hwm_end: xs.at(-1)?.hwm ?? null,
    lock_wait_samples: xs.filter(r => r.lockWaits > 0).length, max_ungranted: Math.max(0, ...xs.map(r => r.ungranted)) }
}
function harvestLog() {
  const text = execFileSync('docker', ['exec', SERVER, 'sh', '-c', 'cat /var/lib/hmem/.hmem/logs/hmem-server.log* 2>/dev/null || true'], { encoding: 'utf8', maxBuffer: 512 << 20 })
  const requests = new Map(), events = {}
  for (const line of text.split('\n')) {
    if (!line.startsWith('{')) continue
    let e; try { e = JSON.parse(line) } catch { continue }
    if (e.request_id && 'db_wait_ms' in e) requests.set(e.request_id, e)
    // Keys never carry query strings, which may hold WebSocket tickets.
    const raw = e.route || e.path, route = typeof raw === 'string' ? raw.split('?')[0] : null
    if (route && route.startsWith('/api/v1/ws')) {
      const key = 'request_log:' + route + ':' + (e.status ?? 'null') + ':' + (e.outcome || '-') + ':' + (e.response || '-')
      const v = events[key] ||= { count: 0, max_ms: 0 }
      v.count++
      const d = e.complete_ms ?? e.duration_ms
      if (typeof d === 'number') v.max_ms = Math.max(v.max_ms, r1(d))
    }
    if (e.event) {
      const key = e.event + ':' + (e.outcome || e.result || '-') + (typeof e.route === 'string' ? ':' + e.route.split('?')[0] : '') + (e.phase ? ':' + e.phase : '') + (e.ticket_kind ? ':' + e.ticket_kind : '') + (e.reason ? ':' + e.reason : '')
      const v = events[key] ||= { count: 0, max_ms: 0 }
      v.count++
      const d = e.handshake_ms ?? e.replay_ms ?? e.turn_ms ?? e.write_ms ?? e.cleanup_ms ?? e.quarantine_ms ?? e.lifetime_ms ?? e.compute_ms
      if (typeof d === 'number') v.max_ms = Math.max(v.max_ms, r1(d))
    }
  }
  return { requests, events }
}

// ------------------------------------------------------------------ workload

const ws = state.workspaces, W0 = ws[0], HOT = ws[1], WARM = ws[2]
const LIGHT = (f, i) => {
  const pick = (xs, n) => xs[((n % xs.length) + xs.length) % xs.length], words = ['kaloka', 'mirepe', 'tusavi', 'nopeda', 'figuka']
  return [
    ['task_get', r => call('fg', 'task_get', 'GET', '/api/v1/tasks/' + pick(f.taskIds, i + r * 53))],
    ['task_update_fields', r => call('fg', 'task_update_fields', 'PUT', '/api/v1/tasks/' + pick(f.taskIds, i * 7 + r * 61), { title: 'Combined update ' + r, priority: 1 + r % 10 })],
    ['task_list_query', r => call('fg', 'task_list_query', 'GET', '/api/v1/tasks?workspace_id=' + f.id + '&query=' + pick(words, r) + '&limit=50')],
    ['search_unified', r => call('fg', 'search_unified', 'POST', '/api/v1/search', { workspace_id: f.id, query: pick(words, r + i), limit: 10 })],
    ['nav_root', () => call('fg', 'nav_root', 'GET', '/api/v1/workspaces/' + f.id + '/navigation?parent_kind=workspace_root')],
    ['nav_project', r => call('fg', 'nav_project', 'GET', '/api/v1/workspaces/' + f.id + '/navigation?parent_kind=project&parent_id=' + pick(f.projectIds, i * 31 + r * 17))],
    ['nav_focus', r => call('fg', 'nav_focus', 'GET', '/api/v1/workspaces/' + f.id + '/navigation/focus/task/' + pick(f.taskIds, i + r * 11))],
    ['planning_counts', () => call('fg', 'planning_counts', 'GET', '/api/v1/workspaces/' + f.id + '/planning-counts')],
    ['project_overview', r => call('fg', 'project_overview', 'GET', '/api/v1/projects/' + pick(f.projectIds, i + r * 13) + '/overview')],
    ['observation_list_query', r => call('fg', 'observation_list_query', 'GET', '/api/v1/observations?workspace_id=' + f.id + '&query=' + pick(words, r + 2))],
  ]
}
const until = deadline => performance.now() < deadline

// Deterministic JSONL records for sessions whose whole prefix this run knows.
function* jsonl(seed) {
  for (let n = 0; ; n++) {
    const body = createHash('sha256').update(seed + ':' + n).digest('hex').repeat(8 + (n % 24))
    yield Buffer.from(JSON.stringify({ uuid: seed + '-' + n, type: n % 3 ? 'assistant' : 'tool_result', message: { content: [{ type: 'text', text: body }] } }) + '\n')
  }
}
async function newSession(phase, workspace, title) {
  const task = (await must(phase, 'task_create', 'POST', '/api/v1/tasks', { workspace_id: workspace, title, description: 'Combined capture task' })).id
  await must(phase, 'task_status', 'PUT', '/api/v1/tasks/' + task, { status: 'in_progress' })
  const id = randomUUID()
  const s = await must(phase, 'session_create', 'POST', '/api/v1/workspaces/' + workspace + '/sessions', { id, task_id: task, title, format_label: 'claude-code-jsonl' })
  return { id, workspace, task, path: '/api/v1/workspaces/' + workspace + '/sessions/' + id, offset: s.committed_bytes, revision: s.revision,
    hash: createHash('sha256'), source: jsonl(id), kept: [], proofs: [], appends: 0 }
}
// Appends whole records up to targetBytes; the server accepts at most 1 MiB per append.
async function appendTo(phase, label, s, targetBytes) {
  const parts = []; let length = 0
  for (;;) { const r = s.pending || s.source.next().value; if (parts.length && length + r.length > targetBytes) { s.pending = r; break } s.pending = null; parts.push(r); length += r.length }
  const chunk = Buffer.concat(parts)
  const r = await call(phase, label, 'POST', s.path + '/append', { expected_offset: s.offset, expected_revision: s.revision, data_base64: chunk.toString('base64') })
  if (r.status === 200) {
    const next = r.json().session
    s.hash.update(chunk); s.offset = next.committed_bytes; s.revision = next.revision; s.appends++
    if ([1, 40, 120].includes(s.appends)) s.kept.push({ offset: s.offset - chunk.length, bytes: chunk.subarray(0, 4096) })
  }
  return r.status
}
async function prove(phase, s) {
  const op = randomUUID(), started = performance.now(), sha = s.hash.copy().digest('hex')
  let r = await call(phase, 'proof_submit', 'POST', s.path + '/prefix-verifications', { operation_id: op, expected_offset: s.offset, expected_revision: s.revision, sha256: sha })
  let result = r.status === 200 || r.status === 202 ? r.json().state : 'http_' + r.status, polls = 0
  while (result === 'pending' && performance.now() - started < 30000) {
    await sleep(100); polls++
    r = await call(phase, 'proof_poll', 'GET', s.path + '/prefix-verifications/' + op)
    result = r.status === 200 ? r.json().state : 'http_' + r.status
  }
  s.proofs.push({ result, polls, ms: r1(performance.now() - started), bytes: s.offset })
}
async function appender(phase, session, deadline, index) {
  const path = '/api/v1/workspaces/' + session.workspace + '/sessions/' + session.id
  const current = (await call('probe', 'session_get', 'GET', path, undefined, false)).json()
  const s = { path, offset: current.committed_bytes, revision: current.revision, hash: createHash('sha256'), source: jsonl('seeded-' + index), kept: [], appends: 0 }
  while (until(deadline)) { const status = await appendTo(phase, 'session_append', s, 4096 + (index * 7919 + s.appends * 104729) % 28672); await sleep(status === 200 ? 250 : 1000) }
  return s.appends
}

async function run() {
  const image = execFileSync('docker', ['inspect', '--format', '{{.Image}}', SERVER], { encoding: 'utf8' }).trim()
  const labels = JSON.parse(execFileSync('docker', ['image', 'inspect', '--format', '{{json .Config.Labels}}', image], { encoding: 'utf8' }).trim() || 'null') || {}
  if (!/^[0-9a-f]{40}$/.test(labels['hmem.perftest.revision'] || '')) throw new Error(SERVER + ' runs ' + image + ' without a hmem.perftest.revision label')
  for (let i = 0; i < 120 && (await call('probe', 'health', 'GET', '/api/v1/health', undefined, false)).status !== 200; i++) await sleep(1000)
  const result = { label: opt.label || labels['hmem.perftest.revision'].slice(0, 8), revision: labels['hmem.perftest.revision'], image, labels,
    startedAt: new Date().toISOString(), idleMs: IDLE_MS, contentionMs: CONTENTION_MS, phases: {} }
  const P = result.phases, mem = sampler(1000), observers = []
  try {
    // Setup: empty retained scopes, a blocked scope, two proof sessions and two bulk sessions.
    const empties = []
    for (let i = 0; i < 16; i++) empties.push((await must('setup', 'workspace_create', 'POST', '/api/v1/workspaces', { name: 'combined-empty-' + i, workspace_type: 'repository' })).id)
    const blockWs = (await must('setup', 'workspace_create', 'POST', '/api/v1/workspaces', { name: 'combined-blocked', workspace_type: 'repository' })).id
    const proofSessions = [await newSession('setup', W0.id, 'Combined proof 0'), await newSession('setup', W0.id, 'Combined proof 1')]
    const bulkSessions = [await newSession('setup', W0.id, 'Combined bulk 0'), await newSession('setup', W0.id, 'Combined bulk 1')]
    for (const s of [...proofSessions, ...bulkSessions]) await appendTo('setup', 'session_append_setup', s, 256 << 10)

    // Snapshot recovery without other load: statements and scope-lock occupancy seen by a same-scope writer.
    P.snapshots = []
    for (const [f, profile] of [[W0, 'full_v1'], [W0, 'workspace_shell_v1'], [HOT, 'full_v1']]) {
      await sleep(1500); resetStatements()
      const writes = [], stop = { done: false }
      const writer = (async () => { for (let r = 0; !stop.done; r++) { const w = await call('snapshot', 'snapshot_writer', 'PUT', '/api/v1/tasks/' + f.taskIds[(r * 97) % f.taskIds.length], { priority: 1 + r % 10 }); writes.push(w); await sleep(25) } })()
      const snap = await resync('snapshot', f.id, profile)
      const end = performance.now(); stop.done = true; await writer
      const during = writes.filter(w => w.started < end).map(w => w.ended - w.started)
      P.snapshots.push({ workspace: f === W0 ? 'W0' : 'HOT', profile, pages: snap.pages, items: snap.items, ms: snap.ms,
        writer_ms: dist(during), statements: statements().classes })
    }

    // Observers: healthy sockets on the hot, measured and every empty scope.
    const connect = async (name, workspace, profile = 'workspace_shell_v1') => {
      const snap = await resync('connect', workspace, profile)
      const o = observe(name, workspace, await ticket('connect', workspace, snap.resume)); observers.push(o)
      await Promise.race([o.ready.then(() => firstFrame(o, 'checkpoint')), sleep(15000)]); return o
    }
    const hot = []; for (let i = 0; i < 4; i++) hot.push(await connect('hot-' + i, HOT.id))
    const measured = [await connect('measured-0', W0.id), await connect('measured-1', W0.id)]
    const warm = await connect('warm-0', WARM.id)
    const idle = []; for (let i = 0; i < empties.length; i++) idle.push(await connect('empty-' + i, empties[i]))
    result.observers = { hot: hot.length, measured: measured.length, warm: 1, empty: idle.length, stalled: 1, blocked: 1, late: 2,
      upgrade_ms: dist(observers.map(o => o.opened).filter(Number.isFinite)) }

    // Idle: every scope empty; count database work for the retained sockets.
    await sleep(2000)
    let h0 = await health(); resetStatements(); const idleStart = performance.now()
    await sleep(IDLE_MS)
    P.idle = { ms: r1(performance.now() - idleStart), scopes: observers.length, statements: statements(), checkout: checkoutDelta(h0, await health()),
      frames_received: observers.reduce((a, o) => a + o.frames.filter(f => f.at >= idleStart).length, 0), memory: memorySummary(mem.rows, idleStart) }

    // Contention: foreground, capture, proofs, skewed writes, a stalled socket,
    // snapshot recovery, archive streaming and large bodies. Hot writes are
    // open-loop at four per second per writer so both revisions see the same events.
    const stalledResume = (await resync('connect', HOT.id, 'workspace_shell_v1')).resume
    const stalled = stalledObserver(await ticket('connect', HOT.id, stalledResume), CONTENTION_MS + 10000)
    await sleep(2000)
    h0 = await health(); resetStatements()
    const cStart = performance.now(), deadline = cStart + CONTENTION_MS, hotWrites = [], tasks = []
    for (let i = 0; i < 4; i++) {
      const fg = LIGHT(W0, i)
      tasks.push((async () => { for (let r = 0; until(deadline); r++) for (const [, run] of fg) { if (!until(deadline)) break; await run(r) } })())
    }
    for (let i = 0; i < 4; i++) tasks.push((async () => {
      for (let r = 0; until(deadline); r++) {
        const due = cStart + i * 62.5 + r * 250
        if (due > performance.now()) await sleep(due - performance.now())
        if (until(deadline)) hotWrites.push(await call('hot', 'hot_update', 'PUT', '/api/v1/tasks/' + HOT.taskIds[(i * 389 + r * 13) % HOT.taskIds.length], { title: 'Hot ' + i + ' ' + r, priority: 1 + r % 10 }))
      }
    })())
    tasks.push((async () => { for (let r = 0; until(deadline); r++) { await call('warm', 'warm_update', 'PUT', '/api/v1/tasks/' + WARM.taskIds[(r * 29) % WARM.taskIds.length], { priority: 1 + r % 10 }); await sleep(1000) } })())
    const seeded = (state.sessions || []).filter(s => !s.terminated).slice(0, 4)
    const appenders = seeded.map((s, i) => appender('capture', s, deadline, i))
    tasks.push(...proofSessions.map(s => (async () => {
      let next = performance.now() + 5000
      while (until(deadline)) {
        await appendTo('capture', 'proof_append', s, 16384); await sleep(250)
        if (performance.now() >= next) { await prove('capture', s); next = performance.now() + 5000 }
      }
    })()))
    const events = {}
    const at = (ms, name, work) => tasks.push((async () => { await sleep(ms); const t = performance.now(); try { events[name] = { at_s: r1((t - cStart) / 1000), ...(await work()) } } catch (e) { events[name] = { error: String(e.message).slice(0, 200) } } })())
    at(20000, 'full_snapshot_hot', async () => { const s = await resync('recovery', HOT.id, 'full_v1'); delete s.resume; return s })
    at(25000, 'shell_snapshot_measured', async () => { const s = await resync('recovery', W0.id, 'workspace_shell_v1'); delete s.resume; return s })
    at(40000, 'archive_stream', async () => {
      const rows = JSON.parse(psql("SELECT json_agg(t) FROM (SELECT workspace_id, session_id, id generation_id, raw_bytes, compressed_bytes, compressed_sha256 FROM session_archive_generations WHERE published_at IS NOT NULL ORDER BY compressed_bytes DESC LIMIT 2) t"))
      const before = memorySummary(mem.rows, performance.now() - 10000)
      const streams = await Promise.all(rows.map(async t => {
        const started = performance.now(), hash = createHash('sha256'); let bytes = 0
        const response = await fetch(BASE + '/api/v1/workspaces/' + t.workspace_id + '/sessions/' + t.session_id + '/archive/' + t.generation_id + '/content',
          { headers: { Authorization: 'Bearer ' + secrets.HMEM_API_KEY, 'X-Request-Id': 'combined.archive.' + counter++ } })
        if (response.status === 200) for await (const chunk of response.body) { hash.update(chunk); bytes += chunk.length }
        return { status: response.status, compressed_bytes: bytes, receipt_match: bytes === Number(t.compressed_bytes) && hash.digest('hex') === t.compressed_sha256, ms: r1(performance.now() - started) }
      }))
      return { streams, rss_before_max: before.rss_max }
    })
    at(70000, 'large_bodies', async () => {
      const statuses = [], started = performance.now()
      const before = memorySummary(mem.rows, started - 10000, started)
      await Promise.all(bulkSessions.map(async s => { for (let k = 0; k < 6; k++) statuses.push(await appendTo('bodies', 'large_append', s, 1000000)) }))
      await sleep(1000)
      return { appends: statuses.length, ok: statuses.filter(s => s === 200).length, ms: r1(performance.now() - started),
        rss_before_max: before.rss_max, rss_during_max: memorySummary(mem.rows, started, performance.now()).rss_max }
    })
    await Promise.all(tasks)
    const appended = await Promise.all(appenders)
    const cEnd = performance.now()
    P.contention = { ms: r1(cEnd - cStart), statements: statements(), checkout: checkoutDelta(h0, await health()), events,
      seeded_appenders: seeded.length, seeded_appends: appended.reduce((a, b) => a + b, 0), memory: memorySummary(mem.rows, cStart, cEnd) }
    const archiveEvent = events.archive_stream
    if (archiveEvent?.streams) archiveEvent.rss_during_max = memorySummary(mem.rows, cStart + 40000, cStart + 40000 + Math.max(...archiveEvent.streams.map(s => s.ms)) + 1000).rss_max

    // Delivery: commit (response end) to arrival of the matching change frame per healthy observer.
    const byRid = new Map(hotWrites.filter(w => w.status === 200).map(w => [w.rid, w]))
    const delivery = o => {
      const lat = [], late = []; let seen = 0
      for (const f of o.frames) if (f.type === 'change' && byRid.has(f.rid)) { seen++; lat.push(f.at - byRid.get(f.rid).ended); late.push(f.at - byRid.get(f.rid).started) }
      const gaps = o.frames.filter(f => f.at >= cStart && f.at <= cEnd).map(f => f.at).reduce((g, t, i, xs) => i ? [...g, t - xs[i - 1]] : g, [])
      const changes = o.frames.filter(f => f.type === 'change' && f.at >= cStart)
      return { matched: seen, changes: changes.length, distinct_changes: new Set(changes.map(f => f.rid)).size, frame_types: o.frames.reduce((m, f) => ({ ...m, [f.type]: (m[f.type] || 0) + 1 }), {}),
        commit_to_frame_ms: dist(lat), start_to_frame_ms: dist(late), max_gap_ms: gaps.length ? r1(Math.max(...gaps)) : null, closed: o.closed?.code ?? null }
    }
    P.delivery = { hot_writes_ok: byRid.size, hot_writes_failed: hotWrites.length - byRid.size, observers: hot.map(delivery), warm: delivery(warm) }
    P.stalled = await stalled

    // Stuck canonical catch-up (6d9ef144): hold the replay scope lock for 8 s,
    // past the 750 ms work and 250 ms cleanup budgets, while healthy peers
    // receive open-loop hot writes and new sockets are admitted.
    const HOLD_S = 8
    const blockSnap = await resync('stuck', blockWs, 'workspace_shell_v1'), blockTicket = await ticket('stuck', blockWs, blockSnap.resume)
    const lateResume = (await resync('stuck', empties[0], 'workspace_shell_v1')).resume
    const holder = spawn('docker', ['exec', '-i', PG, 'psql', '-U', 'hmem', '-d', 'hmem', '-AtqX'])
    holder.stdin.end(`BEGIN; SELECT hmem_change_stream_lock('workspace', '${blockWs}'); SELECT pg_sleep(${HOLD_S}); ROLLBACK;
`)
    let released = null
    const holderDone = new Promise(resolve => holder.on('close', () => { released = performance.now(); resolve() }))
    const waiters = () => Number(psql("SELECT count(*) FROM pg_stat_activity WHERE wait_event_type = 'Lock' AND query LIKE '%hmem_change_stream_lock%'"))
    for (let i = 0; i < 100 && psql(`SELECT count(*) FROM pg_stat_activity WHERE query LIKE '%pg_sleep(${HOLD_S})%' AND state = 'active' AND pid <> pg_backend_pid()`) !== '1'; i++) await sleep(20)
    const sStart = performance.now(), blocked = observe('blocked', blockWs, blockTicket); observers.push(blocked)
    const stuckWrites = [], waiterSamples = []
    const writer = (async () => {
      for (let r = 0; released === null || performance.now() < released + 1000; r++) {
        const due = sStart + r * 250
        if (due > performance.now()) await sleep(due - performance.now())
        stuckWrites.push(await call('stuck', 'hot_update', 'PUT', '/api/v1/tasks/' + HOT.taskIds[(r * 7) % HOT.taskIds.length], { title: 'Beside stuck catch-up ' + r }))
      }
    })()
    const sampling = (async () => { while (released === null) { waiterSamples.push({ at: performance.now(), n: waiters() }); await sleep(200) } })()
    const stuck = { hold_s: HOLD_S, upgrade_ok: await blocked.ready }
    await sleep(Math.max(0, 1500 - (performance.now() - sStart)))
    stuck.blocked_closed_by_1500ms = blocked.closed ? r1(blocked.closed.at - sStart) : null
    const lateStart = performance.now(), late = observe('late', empties[0], await ticket('stuck', empties[0], lateResume)); observers.push(late)
    stuck.late_upgrade_ok = await late.ready
    const lateCheckpoint = await Promise.race([firstFrame(late, 'checkpoint'), sleep(5000).then(() => null)])
    stuck.late_checkpoint_ms = lateCheckpoint ? r1(lateCheckpoint.at - lateStart) : null
    await holderDone; await writer; await sampling
    await sleep(1500)
    stuck.release_ms = r1(released - sStart)
    stuck.lock_waiters = { max: Math.max(0, ...waiterSamples.map(x => x.n)), last_seen_ms: r1(Math.max(0, ...waiterSamples.filter(x => x.n > 0).map(x => x.at - sStart))) }
    stuck.lock_waiters_after_release = waiters()
    // Per write: the slowest healthy observer's commit-to-frame time.
    const perWrite = stuckWrites.filter(w => w.status === 200).map(w => {
      const arrivals = hot.map(o => o.frames.find(f => f.rid === w.rid)?.at)
      return { sent: w.started - sStart, ms: arrivals.every(Number.isFinite) ? Math.max(...arrivals) - w.ended : null }
    })
    const held = perWrite.filter(x => x.sent < stuck.release_ms), afterRelease = perWrite.filter(x => x.sent >= stuck.release_ms)
    stuck.writes = { during_hold: held.length, after_release: afterRelease.length, failed: stuckWrites.length - perWrite.length,
      undelivered: perWrite.filter(x => x.ms === null).length,
      during_hold_ms: dist(held.filter(x => x.ms !== null).map(x => x.ms)), after_release_ms: dist(afterRelease.filter(x => x.ms !== null).map(x => x.ms)),
      over_1s_sent_at_ms: held.filter(x => x.ms > 1000).map(x => r1(x.sent)).slice(0, 12) }
    stuck.blocked_outcome = blocked.closed ? { code: blocked.closed.code, ms: r1(blocked.closed.at - sStart) } : { open: true, frames: blocked.frames.map(f => f.type) }
    const afterStart = performance.now(), after = observe('after-release', empties[1], await ticket('stuck', empties[1], (await resync('stuck', empties[1], 'workspace_shell_v1')).resume)); observers.push(after)
    stuck.after_release_upgrade_ok = await after.ready
    const afterCheckpoint = await Promise.race([firstFrame(after, 'checkpoint'), sleep(5000).then(() => null)])
    stuck.after_release_checkpoint_ms = afterCheckpoint ? r1(afterCheckpoint.at - afterStart) : null
    stuck.observer_closures = observers.filter(o => o.closed && o.closed.at >= sStart).map(o => ({ name: o.name, code: o.closed.code, ms: r1(o.closed.at - sStart) }))
    P.stuck = stuck

    // Evidence: terminate proof sessions with their exact receipts, then read ranges back.
    P.evidence = []
    for (const s of [...proofSessions, ...bulkSessions]) {
      const sha = s.hash.copy().digest('hex')
      const t = await call('evidence', 'session_terminate', 'POST', s.path + '/terminate', { expected_offset: s.offset, expected_revision: s.revision, final_bytes: s.offset, final_sha256: sha })
      // Bots read evidence only once every linked task is done.
      await call('evidence', 'task_status', 'PUT', '/api/v1/tasks/' + s.task, { status: 'done' })
      let ranges = 0
      for (const k of s.kept) {
        const r = await call('evidence', 'evidence_range', 'GET', s.path + '/evidence?offset=' + k.offset + '&limit=' + k.bytes.length)
        if (r.status === 200 && Buffer.from(r.json().data_base64, 'base64').equals(k.bytes)) ranges++
      }
      const proofs = {}; for (const p of s.proofs) proofs[p.result] = (proofs[p.result] || 0) + 1
      P.evidence.push({ terminate_status: t.status, final_bytes: s.offset, terminated_sha_match: t.status === 200 && t.json().final_sha256 === sha,
        ranges_checked: s.kept.length, ranges_matched: ranges, proofs, proof_ms: dist(s.proofs.map(p => p.ms)), proof_polls: dist(s.proofs.map(p => p.polls)) })
    }
  } catch (error) {
    result.error = String(error?.stack || error).slice(0, 2000)
    process.exitCode = 1
  } finally {
    mem.stop = true; await mem.done
    for (const o of observers) o.close()
  }
  result.memory = memorySummary(mem.rows)

  // Foreground and capture latency by label and phase, from client samples.
  const groups = {}
  for (const s of samples) (groups[s.phase + ':' + s.label] ||= []).push(s)
  result.requests = Object.fromEntries(Object.entries(groups).map(([k, xs]) => [k, { ...dist(xs.map(x => x.ms)),
    status: xs.reduce((m, x) => ({ ...m, [x.error || x.status]: (m[x.error || x.status] || 0) + 1 }), {}) }]))
  const fg = samples.filter(s => s.phase === 'fg')
  result.foreground = { ...dist(fg.map(s => s.ms)), failures: fg.filter(s => s.status < 200 || s.status >= 300).length }

  // Server-side request phases and WebSocket/session telemetry, when the image logs them.
  await sleep(1500)
  try {
    const { requests, events } = harvestLog(), joined = samples.filter(s => requests.has(s.rid))
    const tail = fg.filter(s => requests.has(s.rid)).sort((a, b) => b.ms - a.ms).slice(0, Math.max(1, Math.ceil(fg.length * 0.05)))
    const meanOf = (xs, k) => xs.length ? r1(xs.reduce((a, s) => a + (requests.get(s.rid)[k] || 0), 0) / xs.length) : null
    result.serverLog = { joined: joined.length, foreground_tail: { n: tail.length, complete_ms: meanOf(tail, 'complete_ms'), db_wait_ms: meanOf(tail, 'db_wait_ms'), db_hold_ms: meanOf(tail, 'db_hold_ms') },
      outcomes: joined.reduce((m, s) => { const o = requests.get(s.rid).outcome; m[o] = (m[o] || 0) + 1; return m }, {}), events }
  } catch (e) { result.serverLog = { error: String(e.message).slice(0, 200) } }

  // Drain: SIGINT with the compose grace period, then the exit status. The
  // server log lives on a tmpfs, so its shutdown lines are followed live.
  const follower = spawn('docker', ['exec', SERVER, 'tail', '-n', '0', '-F', '/var/lib/hmem/.hmem/logs/hmem-server.log'])
  let shutdownLog = ''
  follower.stdout.on('data', d => { shutdownLog += d })
  await sleep(500)
  const stopStart = performance.now()
  try { execFileSync('docker', ['stop', '-t', '180', SERVER]) } catch {}
  await new Promise(resolve => { follower.on('close', resolve); setTimeout(resolve, 3000) })
  result.drain = { stop_ms: r1(performance.now() - stopStart),
    shutdown_lines: shutdownLog.split('\n').filter(l => l && !l.includes('"request_id"')).map(l => l.slice(0, 200)).slice(-8),
    exit_code: Number(execFileSync('docker', ['inspect', '--format', '{{.State.ExitCode}}', SERVER], { encoding: 'utf8' }).trim()),
    log_tail_warnings: execFileSync('docker', ['logs', '--tail', '40', SERVER], { encoding: 'utf8', stdio: ['ignore', 'pipe', 'pipe'] })
      .split('\n').filter(l => /drain|unclean|unconfirmed|quarantin|exception|error/i.test(l)).map(l => l.slice(0, 200)).slice(0, 8) }
  result.finishedAt = new Date().toISOString()
  mkdirSync(OUT, { recursive: true })
  const file = join(OUT, result.label + '-' + result.startedAt.replace(/[:.]/g, '-') + '.json')
  writeFileSync(file, JSON.stringify(result, null, 1))
  console.log(file)
}

await run()
