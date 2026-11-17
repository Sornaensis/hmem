#!/usr/bin/env node
// High-concurrency HTTP load runner for the throwaway Docker environment of
// tools/perftest/compose.yaml; see docs/reference/concurrency-runner.md.
// Closed-loop virtual clients, each with one request in
// flight and its own keep-alive connection, drive the running
// hmem-perftest-server. They run in worker processes: by default throwaway
// hmem-perftest-load-* containers of the pinned node image on the perftest
// network (removed on exit, the runner mounted read-only), or with
// --client local host processes using the loopback port. Host workers on
// Windows intermittently crashed natively at 1,000 connections. The main
// process uses the loopback port only, runs psql only through
// `docker exec hmem-perftest-postgres` and reads the server log, process status
// and limits through `docker exec hmem-perftest-server`. Credentials and
// tickets are never printed; workers get the API key in their environment.
//
//   node tools/perftest/concurrency.mjs [--steps 300,500,1000] [--ramp-s 15] [--hold-s 120]
//     [--cooldown-s 30] [--timeout-s 30] [--client network|local] [--workers auto]
//     [--discovery-clients 8] [--observers 32] [--appenders 8] [--weights route=w,...]
//     [--seed 1] [--label NAME]
//   node tools/perftest/concurrency.mjs --self-test   (offline checks only)
//
// Reads PERFTEST_DIR (default .tmp/perftest) secrets.env and state.json from
// perftest.mjs seeding and writes one summary JSON to PERFTEST_OUT (default
// .tmp/perftest/concurrency) after every step, printing its path at the end.
// It creates tasks and capture sessions and changes task fields and statuses.
// Before the first step it sets every status target to todo and stops if one
// cannot be set; after each step it puts todo on every status target a client
// attempted, whatever the outcome, and after an abort on every status target.
// Unknown, repeated or valueless flags are rejected before anything is
// contacted.
// PERFTEST_WORKER_IMAGE overrides the worker image.
import { createHash, randomUUID } from 'node:crypto'
import { execFile, execFileSync, spawn } from 'node:child_process'
import { mkdirSync, readFileSync, writeFileSync } from 'node:fs'
import http from 'node:http'
import os from 'node:os'
import { basename, dirname, join } from 'node:path'
import { monitorEventLoopDelay } from 'node:perf_hooks'
import { fileURLToPath } from 'node:url'

const IS_WORKER = process.env.HMEM_PERFTEST_CONCURRENCY_WORKER === '1'
const WORKER_IMAGE = process.env.PERFTEST_WORKER_IMAGE || 'node:24.18.0-bookworm@sha256:5711a0d445a1af54af9589066c646df387d1831a608226f4cd694fc59e745059'

const PG = 'hmem-perftest-postgres', SERVER = 'hmem-perftest-server', NETWORK = 'hmem-perftest-net'
const sleep = ms => new Promise(resolve => setTimeout(resolve, ms))
const now = () => performance.timeOrigin + performance.now()
const r1 = x => Math.round(x * 10) / 10
const r3 = x => Math.round(x * 1000) / 1000
const pct = (s, p) => s.length ? s[Math.max(0, Math.ceil(p * s.length) - 1)] : null
const dist = xs => { const s = Float64Array.from(xs).sort(); return { n: s.length, p50: r1(pct(s, 0.5)), p95: r1(pct(s, 0.95)), p99: r1(pct(s, 0.99)), max: r1(pct(s, 1)) } }
const mean = xs => xs.length ? xs.reduce((a, b) => a + b, 0) / xs.length : null
const tally = (xs, f) => { const m = {}; for (const x of xs) { const k = f(x); m[k] = (m[k] || 0) + 1 } return m }
// Latency over every outcome of samples { ms, cls }. A client timeout is
// censored at the timeout: a percentile that falls on one reads '>=<timeout>'.
// dist() over 2xx samples only is reported beside it as *_2xx_ms.
function censoredDist(xs, timeoutMs) {
  const s = Float64Array.from(xs.map(x => x.cls === 'timeout' ? Infinity : x.ms)).sort()
  const at = p => { const v = pct(s, p); return v === null ? null : v === Infinity ? '>=' + timeoutMs : r1(v) }
  return { n: s.length, timeouts: xs.filter(x => x.cls === 'timeout').length, p50: at(0.5), p95: at(0.95), p99: at(0.99), max: at(1) }
}
// One status toggle. The precondition makes every target todo, so a target's
// first change is to in_progress. The id counts as attempted before the request,
// so it is restored even when the response is lost or times out.
async function toggleStatus(st, id, put) {
  st.attempted.add(id)
  const next = st.status.get(id) === 'in_progress' ? 'todo' : 'in_progress'
  const res = await put(id, next)
  if (res.status === 200) st.status.set(id, next)
  return res
}
// PUT todo on each distinct id, `width` at a time; failures are kept by class and id.
async function restoreStatuses(ids, put, width = 8) {
  const list = [...new Set(ids)], out = { attempted: 0, restored: 0, failed: {}, failedIds: [] }
  for (let i = 0; i < list.length; i += width) {
    const part = list.slice(i, i + width), rs = await Promise.all(part.map(id => put(id)))
    rs.forEach((r, k) => {
      out.attempted++
      if (r.status === 200) out.restored++
      else { out.failed[r.cls] = (out.failed[r.cls] || 0) + 1; out.failedIds.push(part[k]) }
    })
  }
  return out
}

// Mix routes and default weights (percent). Light routes stayed below 60 ms
// sequential p95 in the container sample; the rest are heavy.
const WEIGHTS = { task_get: 18, nav_root: 6, nav_project: 11, nav_focus: 6, nav_batch: 4, planning_counts: 6, project_overview: 6,
  task_list_query: 7, search_unified: 6, observation_list_query: 9, observation_subject_tree: 4, timeline_events: 5,
  timeline_buckets_day: 1, timeline_buckets_week: 1, task_update_fields: 6, task_update_status: 4 }
const LIGHT = new Set(['task_get', 'nav_root', 'nav_project', 'nav_focus', 'planning_counts', 'project_overview', 'task_list_query',
  'search_unified', 'observation_list_query', 'observation_subject_tree', 'timeline_events'])
const WRITES = new Set(['task_update_fields', 'task_update_status'])

// ------------------------------------------------- deterministic generator
// SplitMix64 and vocabulary as in perftest.mjs.
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
const VOCAB = SYLLABLES.flatMap(a => SYLLABLES.flatMap(b => SYLLABLES.map(c => a + b + c))).slice(0, 60)

// ---------------------------------------------------------------- transport
// One request on the given agent. Bodies are kept for errors and when asked;
// otherwise only counted. Outcome classes keep refusals, resets, client
// timeouts, 429s, 503 server_busy and other 5xx apart.
function request(base, apiKey, agent, method, path, body, rid, identity, timeoutMs, keep) {
  const data = body === undefined ? null : Buffer.from(JSON.stringify(body))
  const started = now()
  return new Promise(resolve => {
    let settled = false, timer = null
    const finish = (status, cls, text, extra) => {
      if (settled) return
      settled = true; clearTimeout(timer)
      resolve({ status, cls, text, start: started, ms: now() - started, ...extra })
    }
    const headers = { Authorization: 'Bearer ' + apiKey, 'X-Request-Id': rid, 'X-HMem-Bot-Identity': identity }
    if (data) { headers['Content-Type'] = 'application/json'; headers['Content-Length'] = data.length }
    const req = http.request({ host: base.host, port: base.port, method, path, agent, headers }, res => {
      const chunks = [], keepBody = keep || res.statusCode >= 400
      let bytes = 0
      res.on('data', c => { bytes += c.length; if (keepBody && bytes <= 4 << 20) chunks.push(c) })
      res.on('end', () => {
        const status = res.statusCode, text = keepBody ? Buffer.concat(chunks).toString('utf8') : ''
        let code = null
        if (status >= 400) { try { code = JSON.parse(text).error ?? null } catch {} }
        const cls = status < 300 ? '2xx' : status === 503 && code === 'server_busy' ? '503:server_busy'
          : status === 429 ? '429:' + (code || '-') : status >= 500 ? '5xx:' + status + (code ? ':' + code : '') : status + (code ? ':' + code : '')
        finish(status, cls, text, { bytes, retryAfter: res.headers['retry-after'] ?? null })
      })
      res.on('error', e => finish(0, errorClass(e), ''))
    })
    req.on('socket', s => { if (!s.counted) { s.counted = true; request.connections++ } })
    req.on('error', e => finish(0, errorClass(e), ''))
    timer = setTimeout(() => { req.destroy(Object.assign(new Error('client timeout'), { code: 'CLIENT_TIMEOUT' })) }, timeoutMs)
    req.end(data)
  })
}
request.connections = 0
function errorClass(e) {
  const code = e?.code || ''
  if (code === 'CLIENT_TIMEOUT') return 'timeout'
  if (code === 'ECONNREFUSED') return 'refused'
  if (['ECONNRESET', 'EPIPE', 'ECONNABORTED', 'UND_ERR_SOCKET'].includes(code) || /socket hang up/i.test(e?.message || '')) return 'reset'
  return 'error:' + (code || 'unknown')
}

// =================================================================== worker
// A worker process runs a slice of the step's clients on its own event loop and
// returns compact samples, its in-flight gauge, CPU time and event-loop delay.
// It speaks newline-delimited JSON on stdin/stdout and reads the API key from
// HMEM_API_KEY. All times it sends and receives are in its own clock; the main
// process converts them with the offset measured by ping.
if (IS_WORKER) {
  const d = {}, agents = [], samples = [], inflightTicks = {}, statusAttempted = new Set()
  const loop = monitorEventLoopDelay({ resolution: 10 })
  let inflight = 0, tick = null
  const send = async (c, label, method, path, body, keep) => {
    const rid = d.ridPrefix + label + '.' + c.index + '.' + c.n++
    inflight++
    const r = await request(d.base, d.apiKey, c.agent, method, path, body, rid, c.identity, d.timeoutMs, keep)
    inflight--
    samples.push([c.index, label, r.start, r.ms, r.status, r.cls, rid, r.retryAfter === null ? 0 : 1])
    return r
  }
  const iso = days => new Date(d.now + days * 86400000).toISOString().replace(/\.\d+Z$/, 'Z')
  const pick = (xs, n) => xs[((n % xs.length) + xs.length) % xs.length]
  const choose = draw => {
    const labels = Object.keys(d.weights).filter(k => d.weights[k] > 0), total = labels.reduce((a, k) => a + d.weights[k], 0)
    let x = draw(1e6) / 1e6 * total
    for (const k of labels) { x -= d.weights[k]; if (x < 0) return k }
    return labels.at(-1)
  }
  async function mixClient(c) {
    const w = d.workspaces[c.workspace], ws = w.id, draw = generator(d.seed * 1000003 + c.index)
    const owned = w.taskIds.filter((_, i) => i % c.slots === c.slot), statusOwned = w.statusTargets.filter(id => owned.includes(id))
    const statusState = { status: new Map(), attempted: statusAttempted }
    const run = {
      task_get: r => send(c, 'task_get', 'GET', '/api/v1/tasks/' + pick(w.taskIds, r)),
      nav_root: () => send(c, 'nav_root', 'GET', '/api/v1/workspaces/' + ws + '/navigation?parent_kind=workspace_root'),
      nav_project: r => send(c, 'nav_project', 'GET', '/api/v1/workspaces/' + ws + '/navigation?parent_kind=project&parent_id=' + pick(w.projectIds, r)),
      nav_focus: r => send(c, 'nav_focus', 'GET', '/api/v1/workspaces/' + ws + '/navigation/focus/task/' + pick(w.taskIds, r)),
      nav_batch: r => send(c, 'nav_batch', 'POST', '/api/v1/workspaces/' + ws + '/navigation/batch',
        { requests: [0, 1, 2, 3].map(i => ({ request_id: 'p' + i, parent: { kind: 'project', parent_id: pick(w.projectIds, r + i * 17) } })) }),
      planning_counts: () => send(c, 'planning_counts', 'GET', '/api/v1/workspaces/' + ws + '/planning-counts'),
      project_overview: r => send(c, 'project_overview', 'GET', '/api/v1/projects/' + pick(w.projectIds, r) + '/overview'),
      task_list_query: r => send(c, 'task_list_query', 'GET', '/api/v1/tasks?workspace_id=' + ws + '&query=' + pick(VOCAB, r) + '&limit=50'),
      search_unified: r => send(c, 'search_unified', 'POST', '/api/v1/search', { workspace_id: ws, query: pick(VOCAB, r), limit: 10 }),
      observation_list_query: r => send(c, 'observation_list_query', 'GET', '/api/v1/observations?workspace_id=' + ws + '&query=' + pick(VOCAB, r)),
      observation_subject_tree: () => send(c, 'observation_subject_tree', 'GET', '/api/v1/observations/subject-tree?workspace_id=' + ws + '&prefix=src'),
      timeline_events: () => send(c, 'timeline_events', 'GET', '/api/v1/workspaces/' + ws + '/timeline?limit=50'),
      timeline_buckets_day: () => send(c, 'timeline_buckets_day', 'GET', '/api/v1/workspaces/' + ws + '/timeline/buckets?since=' + iso(-91) + '&until=' + iso(1) + '&bucket=day'),
      timeline_buckets_week: () => send(c, 'timeline_buckets_week', 'GET', '/api/v1/workspaces/' + ws + '/timeline/buckets?since=' + iso(-91) + '&until=' + iso(1) + '&bucket=week'),
      task_update_fields: r => owned.length ? send(c, 'task_update_fields', 'PUT', '/api/v1/tasks/' + pick(owned, r), { title: 'Concurrency update ' + d.ridPrefix + c.index + '.' + c.n, priority: 1 + r % 10 })
        : run.task_get(r),
      task_update_status: r => statusOwned.length
        ? toggleStatus(statusState, pick(statusOwned, r), (id, next) => send(c, 'task_update_status', 'PUT', '/api/v1/tasks/' + id, { status: next }))
        : run.task_update_fields(r),
    }
    while (now() < d.holdEnd) await run[choose(draw)](draw(1e6))
  }
  // Discovery clients keep one search session each for the whole run, so a new
  // generation replaces their previous context instead of adding one. Audiences
  // (8 clients each) are per run: contexts an earlier run left for up to 300 s
  // do not count against this run's per-audience limit, only the global 64.
  async function discoveryClient(c) {
    const ws = d.workspaces[c.workspace].id
    let generation = c.generation
    for (let r = c.index; now() < d.holdEnd; r++) {
      const content = await send(c, 'discovery_content', 'POST', '/api/v1/observations/discovery',
        { workspace_id: ws, search_session: c.session, generation: ++generation, filters: { content_query: pick(VOCAB, r * 7) }, view: 'list', limit: 50 }, true)
      // Like an interactive client, a rejected discovery request waits 1 s before retrying.
      if (content.status !== 200) { await sleep(1000); continue }
      let token = null
      try { token = JSON.parse(content.text).context_token } catch {}
      if (token && now() < d.holdEnd) await send(c, 'discovery_continue', 'POST', '/api/v1/observations/discovery', { workspace_id: ws, context_token: token, view: 'list', offset: 50, limit: 50 })
      if (now() < d.holdEnd) {
        const tree = await send(c, 'discovery_subject_tree', 'POST', '/api/v1/observations/discovery',
          { workspace_id: ws, search_session: c.session, generation: ++generation, filters: { subject_query: 'src/m' + pick(['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'], r) }, view: 'tree', prefix: 'src' })
        if (tree.status !== 200) await sleep(1000)
      }
    }
  }
  const run = async go => {
    d.startAt = go.startAt; d.holdEnd = go.holdEnd
    tick = setInterval(() => { inflightTicks[Math.floor(now() / 200) * 200] = inflight }, 50)
    // Event-loop delay and CPU time cover the hold window only.
    let cpuStart = null, cpu = null
    setTimeout(() => { loop.enable(); cpuStart = process.cpuUsage() }, Math.max(0, go.holdStart - now()))
    setTimeout(() => { if (cpuStart) cpu = process.cpuUsage(cpuStart) }, Math.max(0, go.holdEnd - now()))
    await Promise.all(d.clients.map(async spec => {
      const c = { ...spec, n: 0, agent: new http.Agent({ keepAlive: true, maxSockets: 1, maxFreeSockets: 1 }) }
      agents.push(c.agent)
      const wait = d.startAt + spec.offsetMs - now()
      if (wait > 0) await sleep(wait)
      try { await (spec.kind === 'discovery' ? discoveryClient(c) : mixClient(c)) } catch (e) { samples.push([c.index, 'client_error', now(), 0, 0, 'error:' + (e?.message || e).toString().slice(0, 60), '', 0]) }
    }))
    loop.disable(); clearInterval(tick)
    const clientsDoneAt = now(), connections = request.connections
    for (const a of agents) a.destroy()
    const ns = x => r1(x / 1e6)
    // The main process restores attempted status targets once the server has drained.
    reply({ type: 'done', samples, inflightTicks, connections, clientsDoneAt, statusAttempted: [...statusAttempted],
      cpuMs: cpu ? (cpu.user + cpu.system) / 1000 : null,
      loopDelayMs: { p50: ns(loop.percentile(50)), p99: ns(loop.percentile(99)), max: ns(loop.max) } }, 0)
  }
  const reply = (message, exit) => process.stdout.write(JSON.stringify(message) + '\n', () => { if (exit !== undefined) process.exit(exit) })
  d.apiKey = process.env.HMEM_API_KEY
  let buffer = ''
  process.stdin.on('data', chunk => {
    buffer += chunk
    for (let at; (at = buffer.indexOf('\n')) >= 0;) {
      const message = JSON.parse(buffer.slice(0, at)); buffer = buffer.slice(at + 1)
      if (message.type === 'ping') reply({ type: 'pong', id: message.id, now: now() })
      else if (message.type === 'init') {
        Object.assign(d, message.data)
        const read = p => { try { return readFileSync(p, 'utf8').replace(/\s+/g, ' ').trim() } catch { return null } }
        reply({ type: 'ready', env: { node: process.version, platform: os.platform(), cpus: os.cpus().length, ports: read('/proc/sys/net/ipv4/ip_local_port_range'),
          open_files: (read('/proc/self/limits') || '').match(/Max open files\s+(\d+)/)?.[1] ?? null } })
      }
      else if (message.type === 'go') run(message).catch(e => reply({ type: 'error', error: String(e?.stack || e).slice(0, 500) }, 1))
    }
  })
  process.stdin.on('end', () => process.exit(1))
} else await main()

// ===================================================================== main
async function main() {
  const USAGE = 'usage: node tools/perftest/concurrency.mjs [--steps 300,500,1000] [--ramp-s 15] [--hold-s 120] [--cooldown-s 30] [--timeout-s 30]' +
    ' [--client network|local] [--workers auto|N] [--discovery-clients 8] [--observers 32] [--appenders 8] [--weights route=w,...] [--seed 1] [--label NAME] | --self-test'
  const FLAGS = ['steps', 'ramp-s', 'hold-s', 'cooldown-s', 'timeout-s', 'client', 'workers', 'discovery-clients', 'observers', 'appenders', 'weights', 'seed', 'label']
  const argv = process.argv.slice(2), opt = {}
  if (argv.length === 1 && argv[0] === '--self-test') return selfTest()
  const fail = message => { console.error(message); console.error(USAGE); process.exit(2) }
  for (let i = 0; i < argv.length; i += 2) {
    const name = argv[i].startsWith('--') ? argv[i].slice(2) : '', value = argv[i + 1]
    if (!FLAGS.includes(name) || name in opt || value === undefined || value.startsWith('--')) fail('invalid argument: ' + argv[i])
    opt[name] = value
  }
  const int = (key, fallback, min, max) => {
    const v = opt[key] === undefined ? fallback : Number(opt[key])
    if (!Number.isInteger(v) || v < min || v > max) fail('--' + key + ' must be an integer from ' + min + ' to ' + max)
    return v
  }
  const steps = (opt.steps ?? '300,500,1000').split(',').map(Number)
  if (!steps.length || steps.some(n => !Number.isInteger(n) || n < 1 || n > 2000)) fail('--steps must be comma-separated integers from 1 to 2000')
  const RAMP_MS = int('ramp-s', 15, 0, 300) * 1000, HOLD_MS = int('hold-s', 120, 5, 1800) * 1000, COOLDOWN_MS = int('cooldown-s', 30, 0, 600) * 1000
  const TIMEOUT_MS = int('timeout-s', 30, 1, 300) * 1000, SEED = int('seed', 1, 0, 1e9)
  const DISCOVERY = int('discovery-clients', 8, 0, 64), OBSERVERS = int('observers', 32, 0, 32), APPENDERS = int('appenders', 8, 0, 32)
  const CLIENT = opt.client ?? 'network'
  if (!['network', 'local'].includes(CLIENT)) fail('--client must be network or local')
  const WORKERS = opt.workers === undefined || opt.workers === 'auto' ? null : int('workers', 0, 1, 32)
  if (steps.some(n => n <= DISCOVERY)) fail('every step needs more clients than --discovery-clients')
  if (opt.label !== undefined && !/^[A-Za-z0-9._-]{1,64}$/.test(opt.label)) fail('--label must be 1-64 letters, digits, dots, underscores or hyphens')
  const weights = { ...WEIGHTS }
  if (opt.weights !== undefined) {
    const seen = new Set()
    for (const part of opt.weights.split(',')) {
      const [k, v, extra] = part.split('='), x = Number(v)
      if (!Object.hasOwn(WEIGHTS, k) || seen.has(k) || v === undefined || v === '' || extra !== undefined || !Number.isFinite(x) || x < 0) fail('--weights takes route=weight pairs, each route once, over: ' + Object.keys(WEIGHTS).join(','))
      weights[k] = x; seen.add(k)
    }
    if (!Object.values(weights).some(x => x > 0)) fail('--weights needs a positive weight')
  }

  const DIR = process.env.PERFTEST_DIR || '.tmp/perftest', OUT = process.env.PERFTEST_OUT || join(DIR, 'concurrency')
  const secrets = Object.fromEntries(readFileSync(join(DIR, 'secrets.env'), 'utf8').split(/\r?\n/).filter(Boolean)
    .map(line => [line.slice(0, line.indexOf('=')), line.slice(line.indexOf('=') + 1)]))
  const state = JSON.parse(readFileSync(join(DIR, 'state.json'), 'utf8'))
  if (!state.workspaces?.length || state.workspaces.some(w => !w?.taskIds?.length || !w.projectIds?.length)) throw new Error('state.json needs perftest.mjs seed-planning workspaces')
  const sh = (args, input) => execFileSync('docker', args, { input, encoding: 'utf8', maxBuffer: 512 << 20, stdio: ['pipe', 'pipe', 'pipe'] }).trim()
  const psql = sql => sh(['exec', '-i', PG, 'psql', '-U', 'hmem', '-d', 'hmem', '-AtqX', '-v', 'ON_ERROR_STOP=1'], sql)
  const mapped = sh(['port', SERVER, '8420/tcp']).split(/\r?\n/)[0]
  if (!/^127\.0\.0\.1:\d+$/.test(mapped)) throw new Error('perftest server must be published on 127.0.0.1 only')
  const base = { host: '127.0.0.1', port: Number(mapped.split(':')[1]) }, BASE = 'http://' + mapped
  const imageId = sh(['inspect', '--format', '{{.Image}}', SERVER])
  const labels = JSON.parse(sh(['image', 'inspect', '--format', '{{json .Config.Labels}}', imageId]) || 'null') || {}
  if (!/^[0-9a-f]{40}$/.test(labels['hmem.perftest.revision'] || '')) throw new Error(SERVER + ' runs ' + imageId + ' without a hmem.perftest.revision label')
  if (CLIENT === 'network' && !(NETWORK in JSON.parse(sh(['inspect', '--format', '{{json .NetworkSettings.Networks}}', SERVER]) || '{}'))) throw new Error(SERVER + ' is not on ' + NETWORK)

  const agent = new http.Agent({ keepAlive: true, maxSockets: 64 }), IDENTITY = 'Perftest concurrency'
  const RUN = randomUUID().slice(0, 8)
  let counter = 0
  const identity = role => JSON.stringify({ role, 'model-id': 'test:perftest:default' })
  const call = (label, method, path, body, keep = true, who = IDENTITY) =>
    request(base, secrets.HMEM_API_KEY, agent, method, path, body, 'cc.' + RUN + '.main.' + label + '.' + counter++, identity(who), TIMEOUT_MS, keep)
  const must = async (label, method, path, body) => {
    const r = await call(label, method, path, body)
    if (r.status < 200 || r.status >= 300) throw new Error(label + ' failed with HTTP ' + r.status + ' ' + r.cls)
    return r.text ? JSON.parse(r.text) : null
  }
  // The server must answer health with HTTP 200 within 120 s.
  const healthWaitStart = now()
  let startupHealth = null
  while (!startupHealth && now() - healthWaitStart < 120000) {
    const r = await call('health', 'GET', '/api/v1/health')
    if (r.status === 200) startupHealth = r; else await sleep(1000)
  }
  if (!startupHealth) throw new Error('no HTTP 200 from /api/v1/health within 120 s')
  const startupHealthMs = r1(now() - healthWaitStart)

  // ------------------------------------------------------------ host limits
  const pid = sh(['exec', SERVER, 'sh', '-c', 'for p in /proc/[0-9]*; do [ "$(cat $p/comm 2>/dev/null)" = hmem-server ] && echo ${p#/proc/} && break; done'])
  const inServer = cmd => { try { return sh(['exec', SERVER, 'sh', '-c', cmd]) } catch (e) { return 'unavailable: ' + String(e.stderr || e.message).trim().slice(0, 120) } }
  const limits = {
    client: { platform: os.platform() + ' ' + os.release(), cpus: os.cpus().length, node: process.version, memory_bytes: os.totalmem(),
      dynamic_ports: os.platform() === 'win32' ? (() => { try { return execFileSync('netsh', ['int', 'ipv4', 'show', 'dynamicport', 'tcp'], { encoding: 'utf8' }).replace(/\s+/g, ' ').trim() } catch { return null } })()
        : (() => { try { return readFileSync('/proc/sys/net/ipv4/ip_local_port_range', 'utf8').trim() } catch { return null } })() },
    server: { nproc: inServer('nproc'), somaxconn: inServer('cat /proc/sys/net/core/somaxconn'), tcp_max_syn_backlog: inServer('cat /proc/sys/net/ipv4/tcp_max_syn_backlog'),
      ulimit_n: inServer('ulimit -n'), process_open_files: inServer('grep "Max open files" /proc/' + pid + '/limits').replace(/\s+/g, ' '),
      cmdline: inServer('tr "\\0" " " < /proc/' + pid + '/cmdline'), ghcrts: inServer('tr "\\0" "\\n" < /proc/' + pid + '/environ | grep ^GHCRTS= || echo unset'),
      rts_info: inServer('hmem-server +RTS --info 2>&1 | grep -E "RTS opts|Flag -with-rtsopts|RTS way" | tr -s " \\n" " "'),
      threads_idle: inServer('grep ^Threads: /proc/' + pid + '/status') },
    postgres: { max_connections: psql('SHOW max_connections'), shared_buffers: psql('SHOW shared_buffers'), somaxconn: sh(['exec', PG, 'cat', '/proc/sys/net/core/somaxconn']) },
    docker: sh(['info', '--format', '{{.ServerVersion}} {{.NCPU}} CPUs {{.MemTotal}} bytes {{.OperatingSystem}}']) }
  // The periodic monitor reads it asynchronously so the main event loop keeps
  // timestamping WebSocket frames.
  const PROC = 'cat /proc/' + pid + '/stat; echo; grep -E "^(VmRSS|VmHWM|Threads):" /proc/' + pid + '/status | tr "\\n" " "'
  const procAsync = () => new Promise((resolve, reject) => execFile('docker', ['exec', SERVER, 'sh', '-c', PROC], { encoding: 'utf8' },
    (e, out) => e ? reject(e) : resolve(parseProc(out.trim()))))
  const serverProc = () => parseProc(inServer(PROC))
  const parseProc = text => {
    const [stat, status] = text.split('\n').filter(Boolean)
    const f = stat.slice(stat.lastIndexOf(')') + 2).split(' '), tck = 100
    const kb = name => Number((status.match(new RegExp(name + ':\\s+(\\d+)')) || [])[1] || NaN)
    return { cpu_s: (Number(f[11]) + Number(f[12])) / tck, rss: kb('VmRSS') * 1024, hwm: kb('VmHWM') * 1024, threads: kb('Threads') }
  }
  const containerState = () => Object.fromEntries([SERVER, PG].map(n => [n, sh(['inspect', '--format', '{{.State.StartedAt}} {{.RestartCount}} {{.State.Running}} {{.State.OOMKilled}}', n])]))

  // --------------------------------------------------------------- monitors
  const snapshot = async () => {
    const at = now(), r = await call('health_probe', 'GET', '/api/v1/health')
    let body = null; try { body = JSON.parse(r.text) } catch {}
    return { at, ms: r.ms, status: r.status, health: r.status + ':' + (body?.status ?? '-'), ok: body?.status === 'ok', pool: body?.pool ?? null, delivery: body?.delivery_pool ?? null,
      admission: body?.http_admission ?? null }
  }
  const healthRows = []
  const monitor = { stop: false }
  monitor.done = (async () => {
    while (!monitor.stop) {
      const row = await snapshot()
      try { row.proc = await procAsync() } catch { row.proc = null }
      healthRows.push(row)
      await sleep(Math.max(0, 5000 - (now() - row.at)))
    }
  })()
  const statsRows = []
  const stats = spawn('docker', ['stats', '--format', '{{json .}}', SERVER, PG])
  let statsBuf = ''
  stats.stdout.on('data', chunk => {
    statsBuf += chunk.toString().replace(/\x1b\[[0-9;?]*[A-Za-z]/g, '\n')
    const lines = statsBuf.split(/\r?\n/); statsBuf = lines.pop()
    for (const line of lines) {
      if (!line.startsWith('{')) continue
      try { const s = JSON.parse(line); statsRows.push({ at: now(), name: s.Name, cpu: parseFloat(s.CPUPerc), mem: s.MemUsage.split(' / ')[0] }) } catch {}
    }
  })
  const mainLoop = monitorEventLoopDelay({ resolution: 10 })

  // ---------------------------------------------------- observers, appenders
  const W = state.workspaces
  // Observers stay open for the whole run in one audience (canonical sockets
  // are capped at 32 per audience and 128 overall).
  const observers = []
  const openObservers = async () => { for (let i = 0; i < OBSERVERS; i++) {
    const w = W[i % W.length], scope = { scope: 'workspace', workspace_id: w.id }
    let page = await must('resync', 'POST', '/api/v1/change-stream/resync', { scope, snapshot_profile: 'workspace_shell_v1', page_size: 100, start_idempotency_key: randomUUID() + randomUUID() })
    while (page.has_more) page = await must('resync_page', 'POST', '/api/v1/change-stream/resync', { scope, page_token: page.next_page_token })
    const ticket = (await must('ticket', 'POST', '/api/v1/change-stream/ticket', { scope, resume_token: page.resume_token })).ticket
    const o = { workspace: w.id, frames: [], closed: null, opened: false }
    const socket = new WebSocket(BASE.replace('http', 'ws') + '/api/v1/ws?ticket=' + encodeURIComponent(ticket))
    o.ready = new Promise(resolve => { socket.onopen = () => { o.opened = true; resolve(true) }; socket.onerror = () => resolve(false) })
    socket.onmessage = event => {
      let frame = null; try { frame = JSON.parse(String(event.data)) } catch {}
      o.frames.push({ at: now(), type: frame?.type, rid: frame?.event?.transaction?.request_id })
    }
    socket.onclose = event => { o.closed = { at: now(), code: event.code } }
    o.close = () => { try { socket.close() } catch {} }
    observers.push(o)
    await o.ready
  } }
  // Capture appenders write fresh sessions whose whole prefix this run knows,
  // so each step ends with an exact prefix proof.
  const capture = []
  function* jsonl(seed) {
    for (let n = 0; ; n++) {
      const body = createHash('sha256').update(seed + ':' + n).digest('hex').repeat(8 + (n % 24))
      yield Buffer.from(JSON.stringify({ uuid: seed + '-' + n, type: n % 3 ? 'assistant' : 'tool_result', message: { content: [{ type: 'text', text: body }] } }) + '\n')
    }
  }
  const openCapture = async () => { for (let i = 0; i < APPENDERS; i++) {
    const w = W[i % W.length]
    const task = (await must('task_create', 'POST', '/api/v1/tasks', { workspace_id: w.id, title: 'Concurrency capture ' + RUN + ' ' + i, description: 'Concurrency capture task' })).id
    await must('task_status', 'PUT', '/api/v1/tasks/' + task, { status: 'in_progress' })
    const id = randomUUID()
    const s = await must('session_create', 'POST', '/api/v1/workspaces/' + w.id + '/sessions', { id, task_id: task, title: 'Concurrency capture ' + i, format_label: 'claude-code-jsonl' })
    capture.push({ path: '/api/v1/workspaces/' + w.id + '/sessions/' + id, offset: s.committed_bytes, revision: s.revision, hash: createHash('sha256'), source: jsonl(RUN + '-' + i), index: i })
  } }
  const appendLoop = (s, stop, rows) => (async () => {
    while (!stop.done) {
      const parts = []; let length = 0
      while (length < 4096 + (s.index * 7919 + rows.length * 104729) % 28672) { const r = s.pending || s.source.next().value; s.pending = null; parts.push(r); length += r.length }
      const chunk = Buffer.concat(parts)
      const r = await call('session_append', 'POST', s.path + '/append', { expected_offset: s.offset, expected_revision: s.revision, data_base64: chunk.toString('base64') })
      rows.push(r)
      if (r.status === 200) { const next = JSON.parse(r.text).session; s.hash.update(chunk); s.offset = next.committed_bytes; s.revision = next.revision }
      else { s.pending = null; await sleep(1000) }
      await sleep(250)
    }
  })()
  // Proofs run one at a time; a busy verification admission is retried for up
  // to 30 s and reported by its error code otherwise.
  async function prove(s) {
    const op = randomUUID(), started = now()
    const submit = () => call('proof_submit', 'POST', s.path + '/prefix-verifications', { operation_id: op, expected_offset: s.offset, expected_revision: s.revision, sha256: s.hash.copy().digest('hex') })
    let r = await submit()
    while (r.cls === '5xx:503:verification_busy' && now() - started < 30000) { await sleep(1000); r = await submit() }
    let st = r.status === 200 || r.status === 202 ? JSON.parse(r.text).state : r.cls
    while (st === 'pending' && now() - started < 30000) {
      await sleep(250)
      r = await call('proof_poll', 'GET', s.path + '/prefix-verifications/' + op)
      st = r.status === 200 ? JSON.parse(r.text).state : r.cls
    }
    return st
  }

  // ------------------------------------------------------------------ steps
  const SAMPLER = "SELECT count(*) || '|' || count(*) FILTER (WHERE a.state = 'active') || '|' || count(*) FILTER (WHERE a.state LIKE 'idle in transaction%') || '|' || count(*) FILTER (WHERE a.wait_event_type = 'Lock') || '|' || (SELECT count(*) FROM pg_locks WHERE NOT granted) || '|' || coalesce(string_agg(coalesce(a.wait_event_type, 'CPU') || ':' || coalesce(a.wait_event, '-'), ',') FILTER (WHERE a.state = 'active'), '') FROM pg_stat_activity a WHERE a.datname = current_database() AND a.pid <> pg_backend_pid() AND a.backend_type = 'client backend' \\watch 0.5\n"
  const DB = 'SELECT row_to_json(d) FROM (SELECT xact_commit, xact_rollback, blks_read, blks_hit, temp_files, temp_bytes, deadlocks FROM pg_stat_database WHERE datname = current_database()) d'
  const hasStatements = psql("SELECT count(*) FROM pg_extension WHERE extname = 'pg_stat_statements'") === '1'
  const STATEMENTS = "SELECT coalesce(json_agg(s), '[]') FROM (SELECT left(regexp_replace(query, '\\s+', ' ', 'g'), 120) query, calls, round(total_exec_time::numeric) total_ms, round(mean_exec_time::numeric, 1) mean_ms, temp_blks_written FROM pg_stat_statements WHERE dbid = (SELECT oid FROM pg_database WHERE datname = current_database()) AND query NOT LIKE '%pg_stat_%' AND query NOT LIKE '%pg_locks%' ORDER BY total_exec_time DESC LIMIT 8) s"
  const numericDelta = (a, b) => {
    if (a == null || b == null) return null
    const out = {}
    for (const [k, v] of Object.entries(b)) {
      if (typeof v === 'number' && typeof a[k] === 'number') out[k] = r3(v - a[k])
      else if (v && typeof v === 'object' && !Array.isArray(v) && a[k] && typeof a[k] === 'object') out[k] = numericDelta(a[k], v)
    }
    return out
  }
  const poolDelta = (a, b) => {
    if (!a?.checkout || !b?.checkout) return null
    const d = numericDelta(a.checkout, b.checkout)
    delete d.wait_max_ms
    return { ...d, capacity_wait_share: d.checkouts ? r3(d.capacity_waits / d.checkouts) : null, wait_max_ms_process: b.checkout.wait_max_ms,
      wait_buckets: b.checkout.wait_buckets.map((x, i) => ({ le_ms: x.le_ms, count: x.count - a.checkout.wait_buckets[i].count })) }
  }
  const harvestLog = (prefix, from, to) => {
    const text = sh(['exec', SERVER, 'sh', '-c', 'cat /var/lib/hmem/.hmem/logs/hmem-server.log* 2>/dev/null || true'])
    const entries = new Map(), events = {}
    for (const line of text.split('\n')) {
      if (!line.startsWith('{')) continue
      if (line.includes(prefix)) { try { const e = JSON.parse(line); if (e.request_id?.startsWith(prefix)) entries.set(e.request_id, e) } catch {} continue }
      // WebSocket and change-stream telemetry of the step; keys carry no query strings.
      if (!line.includes('"event"') || !/websocket|change_stream|delivery/.test(line)) continue
      try {
        const e = JSON.parse(line), t = Date.parse(e.timestamp)
        if (!(t >= from && t <= to)) continue
        const key = e.event + ':' + (e.outcome || e.result || '-') + (e.reason ? ':' + e.reason : '')
        events[key] = (events[key] || 0) + 1
      } catch {}
    }
    return { entries, events, bytes: text.length }
  }
  // Workers run as throwaway containers on the perftest network (default) or as
  // host processes (--client local, which goes through Docker's published port).
  const SCRIPT = fileURLToPath(import.meta.url)
  function startWorker(stepIndex, k) {
    const name = 'hmem-perftest-load-' + RUN + '-' + stepIndex + '-' + k
    const [cmd, args] = CLIENT === 'network'
      ? ['docker', ['run', '--rm', '-i', '--name', name, '--network', NETWORK, '--memory', '2g', '-e', 'HMEM_API_KEY', '-e', 'HMEM_PERFTEST_CONCURRENCY_WORKER=1',
          '-v', dirname(SCRIPT).replace(/\\/g, '/') + ':/runner:ro', WORKER_IMAGE, 'node', '/runner/' + basename(SCRIPT)]]
      : [process.execPath, [SCRIPT]]
    const child = spawn(cmd, args, { env: { ...process.env, HMEM_API_KEY: secrets.HMEM_API_KEY, HMEM_PERFTEST_CONCURRENCY_WORKER: '1' }, stdio: ['pipe', 'pipe', 'inherit'] })
    const w = { waiters: [], exited: null }
    let buffer = ''
    child.stdout.on('data', chunk => {
      buffer += chunk
      for (let at; (at = buffer.indexOf('\n')) >= 0;) {
        let m; try { m = JSON.parse(buffer.slice(0, at)) } catch { m = { type: 'error', error: 'unparsable worker output' } }
        buffer = buffer.slice(at + 1)
        const i = m.type === 'error' ? 0 : w.waiters.findIndex(x => x.type === m.type)
        if (i < 0 || !w.waiters.length) continue
        const [x] = w.waiters.splice(i, 1)
        if (m.type === 'error') x.reject(new Error('worker ' + k + ': ' + m.error)); else x.resolve(m)
      }
    })
    child.on('exit', code => { w.exited = code; for (const x of w.waiters.splice(0)) x.reject(new Error('worker ' + k + ' exited with ' + code + ' before ' + x.type)) })
    w.send = m => child.stdin.write(JSON.stringify(m) + '\n')
    w.expect = type => new Promise((resolve, reject) => w.exited !== null ? reject(new Error('worker ' + k + ' exited with ' + w.exited)) : w.waiters.push({ type, resolve, reject }))
    w.stop = () => {
      if (w.exited !== null) return
      try { child.stdin.end() } catch {}
      if (CLIENT === 'network') { try { execFileSync('docker', ['rm', '-f', name], { stdio: 'ignore' }) } catch {} }
      child.kill()
    }
    return w
  }
  // Clock offset (worker minus main) from the ping with the shortest round trip.
  async function clockOffset(w) {
    let best = null
    for (let i = 0; i < 5; i++) {
      const t0 = now(), pong = w.expect('pong')
      w.send({ type: 'ping', id: i })
      const m = await pong, t1 = now()
      if (!best || t1 - t0 < best.rtt) best = { rtt: t1 - t0, offset: m.now - (t0 + t1) / 2 }
    }
    return best
  }
  // pg_stat_activity sampler: psql \watch in the PostgreSQL container, files unique per run.
  const TMP = '/tmp/hmem-concurrency-' + RUN, sampler = { running: false }
  const stopSampler = () => { if (sampler.running) { sh(['exec', PG, 'sh', '-c', 'kill $(cat ' + TMP + '-sampler.pid)']); sampler.running = false } }
  // Status targets: PUT todo, and wait for the server to drain before restoring.
  const allStatusTargets = W.flatMap(w => w.statusTargets || [])
  const putTodo = label => id => call(label, 'PUT', '/api/v1/tasks/' + id, { status: 'todo' }, false)
  const awaitRecovery = async (from, seconds) => {
    for (let t = 0; t < seconds; t++) {
      const r = await call('health_probe', 'GET', '/api/v1/health')
      let b = null; try { b = JSON.parse(r.text) } catch {}
      if (r.status === 200 && b?.status === 'ok') return { ok_after_hold_ms: r1(now() - from), pool_active: b.pool?.active_connections, http_admission: b.http_admission ?? null }
      await sleep(1000)
    }
    return null
  }
  let restorePending = false, restoreIncomplete = false
  const generations = new Map()
  const discoverySessions = Array.from({ length: DISCOVERY }, () => randomUUID())
  const result = { label: opt.label || labels['hmem.perftest.revision'].slice(0, 8), revision: labels['hmem.perftest.revision'], image: imageId, labels,
    startedAt: new Date().toISOString(), run: RUN, startup_health_ms: startupHealthMs,
    config: { steps, rampMs: RAMP_MS, holdMs: HOLD_MS, cooldownMs: COOLDOWN_MS, timeoutMs: TIMEOUT_MS, seed: SEED, client: CLIENT, workerImage: CLIENT === 'network' ? WORKER_IMAGE : null, workers: WORKERS ?? 'auto',
      discoveryClients: DISCOVERY, discoveryAudiences: Math.ceil(DISCOVERY / 8), observers: OBSERVERS, appenders: APPENDERS, weights,
      light: [...LIGHT], workspaces: W.length, clientModel: 'closed loop, one request in flight per client, no think time, one keep-alive connection per client (node:http Agent maxSockets 1), clients spread round-robin over worker processes' },
    limits, steps: [] }
  mkdirSync(OUT, { recursive: true })
  const file = join(OUT, result.label + '-' + result.startedAt.replace(/[:.]/g, '-') + '.json')
  let live = []

  try {
    // Precondition (unmeasured): every status target is todo before the first step.
    result.status_precondition = await restoreStatuses(allStatusTargets, putTodo('status_precondition'))
    if (result.status_precondition.restored !== result.status_precondition.attempted)
      throw new Error('status precondition: ' + result.status_precondition.failedIds.length + ' status targets could not be set to todo ' + JSON.stringify(result.status_precondition.failed))
    await openObservers(); await openCapture()
    result.observersOpened = observers.filter(o => o.opened).length
    for (const [stepIndex, clients] of steps.entries()) {
      const prefix = 'cc.' + RUN + '.s' + stepIndex + '.'
      const workers = WORKERS ?? Math.min(16, Math.max(2, Math.ceil(clients / 125)))
      const mixClients = clients - DISCOVERY, perWs = W.map((_, w) => Math.ceil((mixClients - w) / W.length))
      const specs = Array.from({ length: clients }, (_, index) => {
        const offsetMs = RAMP_MS * index / clients
        if (index < DISCOVERY) {
          const g = generations.get(index) || 0; generations.set(index, g + 1000000)
          return { index, kind: 'discovery', workspace: index % W.length, session: discoverySessions[index], generation: g, offsetMs,
            identity: identity('Perftest discovery ' + RUN + ' ' + Math.floor(index / 8)) }
        }
        const m = index - DISCOVERY
        return { index, kind: 'mix', workspace: m % W.length, slot: Math.floor(m / W.length), slots: perWs[m % W.length], offsetMs,
          identity: identity('Perftest concurrency client ' + index) }
      })
      restorePending = true
      const pool = live = Array.from({ length: workers }, (_, k) => startWorker(stepIndex, k))
      const clocks = await Promise.all(pool.map(clockOffset))
      const ready = pool.map(w => w.expect('ready'))
      pool.forEach((w, k) => w.send({ type: 'init', data: { worker: k, base: CLIENT === 'network' ? { host: SERVER, port: 8420 } : base, ridPrefix: prefix,
        timeoutMs: TIMEOUT_MS, seed: SEED + stepIndex, weights, now: Date.now(), clients: specs.filter(s => s.index % workers === k),
        workspaces: W.map(w => ({ id: w.id, taskIds: w.taskIds, projectIds: w.projectIds, statusTargets: w.statusTargets || [] })) } }))
      const readies = await Promise.all(ready)
      const startAt = now() + 1000, holdStart = startAt + RAMP_MS, holdEnd = holdStart + HOLD_MS
      const done = Promise.all(pool.map(w => w.expect('done')))
      done.catch(() => {})
      pool.forEach((w, k) => w.send({ type: 'go', startAt: startAt + clocks[k].offset, holdStart: holdStart + clocks[k].offset, holdEnd: holdEnd + clocks[k].offset }))
      const step = { clients, mixClients, discoveryClients: DISCOVERY, workers, client_mode: CLIENT, worker_env: readies[0].env,
        clock_offset_ms: clocks.map(c => r1(c.offset)), clock_rtt_ms: clocks.map(c => r1(c.rtt)) }
      result.steps.push(step)
      const stop = { done: false }, appendRows = capture.map(() => [])
      const appending = capture.map((s, i) => appendLoop(s, stop, appendRows[i]))
      await sleep(Math.max(0, holdStart - now()))
      // Hold window: server-side counters and samplers. These synchronous docker
      // exec and psql calls block the main process (observer frame timestamps and
      // appenders); they are timed as main_blocking_ms and excluded from
      // main_loop_delay_ms.
      let blockStart = now()
      if (hasStatements) psql('SELECT pg_stat_statements_reset()')
      const dbBefore = JSON.parse(psql(DB)), stateBefore = containerState()
      let procBefore = null; try { procBefore = serverProc() } catch {}
      const activityFile = TMP + '-activity-' + stepIndex + '.txt'
      sh(['exec', '-i', PG, 'sh', '-c', 'cat > ' + TMP + '-sampler.sql'], SAMPLER)
      sampler.running = true
      sh(['exec', '-d', PG, 'sh', '-c', 'echo $$ > ' + TMP + '-sampler.pid; exec psql -U hmem -d hmem -AtqX -o ' + activityFile + ' -f ' + TMP + '-sampler.sql'])
      const mainBlocking = { hold_start: r1(now() - blockStart) }
      const cpuBefore = process.cpuUsage(), wallBefore = now()
      mainLoop.reset(); mainLoop.enable()
      const hBefore = await snapshot()
      await sleep(Math.max(0, holdEnd - now()))
      const hAfter = await snapshot()
      mainLoop.disable()
      const cpu = process.cpuUsage(cpuBefore), wall = now() - wallBefore
      blockStart = now()
      stopSampler()
      const dbAfter = JSON.parse(psql(DB)), statements = hasStatements ? JSON.parse(psql(STATEMENTS) || '[]') : []
      let procAfter = null; try { procAfter = serverProc() } catch {}
      mainBlocking.hold_end = r1(now() - blockStart)
      // Drain: clients stop issuing at holdEnd; wait for in-flight requests.
      // Worker times are converted to this process's clock.
      const outs = (await done).map((o, k) => {
        const off = clocks[k].offset
        o.samples.forEach(s => { s[2] -= off })
        o.clientsDoneAt -= off
        o.inflightTicks = Object.fromEntries(Object.entries(o.inflightTicks).map(([t, v]) => [Math.floor((Number(t) - off) / 200), v]))
        return o
      })
      const drainedAt = now()
      stop.done = true; await Promise.all(appending)
      // Recovery: health OK within 30 s of the load stopping.
      const recovery = await awaitRecovery(holdEnd, 30)
      // Unmeasured: PUT todo on every status target a client attempted, including
      // timed-out attempts, once the server has drained (or 30 s have passed).
      const statusRestore = await restoreStatuses(outs.flatMap(o => o.statusAttempted), putTodo('status_restore'))
      if (statusRestore.failedIds.length) restoreIncomplete = true
      restorePending = restoreIncomplete
      const proofs = []
      for (const s of capture) proofs.push(await prove(s))
      const activity = sh(['exec', PG, 'cat', activityFile])
      const stateAfter = containerState()
      for (const w of pool) w.stop()

      // ---------------------------------------------------- step summary
      const samples = outs.flatMap(o => o.samples).map(([client, label, start, ms, status, cls, rid, retryAfter]) => ({ client, label, start, ms, status, cls, rid, retryAfter }))
      const inHold = samples.filter(s => s.start >= holdStart && s.start < holdEnd && s.label !== 'client_error')
      const mix = inHold.filter(s => s.label in WEIGHTS)
      // Log join after flushing the per-capability log buffers with cheap unmatched requests.
      await Promise.all(Array.from({ length: 16 }, async () => { for (let i = 0; i < 32; i++) await call('flush', 'GET', '/api/v1/perftest-flush') }))
      await sleep(2000)
      const log = harvestLog(prefix, startAt, now()), joined = inHold.filter(s => log.entries.has(s.rid))
      const routeOf = xs => {
        const j = xs.filter(s => log.entries.has(s.rid)), e = s => log.entries.get(s.rid)
        const tail = [...j].sort((a, b) => b.ms - a.ms).slice(0, Math.max(1, Math.ceil(j.length * 0.05)))
        const admission = j.map(s => e(s).admission_wait_ms ?? e(s).queue_ms).filter(x => typeof x === 'number')
        return { latency_2xx_ms: dist(xs.filter(s => s.cls === '2xx').map(s => s.ms)), latency_all_ms: censoredDist(xs, TIMEOUT_MS), outcomes: tally(xs, s => s.cls),
          server: j.length ? { joined: j.length, complete_ms: dist(j.map(s => e(s).complete_ms)), wait_mean: r1(mean(j.map(s => e(s).db_wait_ms))),
            hold_mean: r1(mean(j.map(s => e(s).db_hold_ms))), checkouts_mean: r1(mean(j.map(s => e(s).db_checkouts))),
            tail_wait_share: tail.length ? r3(mean(tail.map(s => e(s).db_wait_ms)) / Math.max(1e-9, mean(tail.map(s => e(s).complete_ms)))) : null,
            admission_wait_ms: admission.length ? dist(admission) : null,
            transport_ms: dist(j.map(s => s.ms - e(s).complete_ms)) } : null }
      }
      const routes = {}
      for (const label of [...new Set(inHold.map(s => s.label))].sort()) routes[label] = routeOf(inHold.filter(s => s.label === label))
      const perClient = new Map(specs.filter(s => s.kind === 'mix').map(s => [s.index, 0]))
      for (const s of mix) perClient.set(s.client, (perClient.get(s.client) || 0) + 1)
      const counts = [...perClient.values()], sum = counts.reduce((a, b) => a + b, 0), sq = counts.reduce((a, b) => a + b * b, 0)
      const ticks = {}
      for (const o of outs) for (const [t, v] of Object.entries(o.inflightTicks)) ticks[t] = (ticks[t] || 0) + v
      const holdTicks = Object.entries(ticks).filter(([t]) => t * 200 >= holdStart && t * 200 < holdEnd).map(([, v]) => v)
      // WebSocket delivery: commit (response end) to the matching change frame.
      const writes = new Map(inHold.filter(s => WRITES.has(s.label) && s.status === 200).map(s => [s.rid, s]))
      const writeWs = new Map(samples.filter(s => writes.has(s.rid)).map(s => [s.rid, specs[s.client].workspace]))
      // Delivery is aggregated over observers open through the drain only; observers
      // that never opened or closed (or were retired) before then are listed apart.
      const lat = [], perObserver = [], missing = {}, frameTypes = {}
      for (const [i, o] of observers.entries()) {
        const wsIndex = W.findIndex(w => w.id === o.workspace), expected = [...writes.values()].filter(s => writeWs.get(s.rid) === wsIndex)
        const frames = new Map(o.frames.filter(f => f.type === 'change' && writes.has(f.rid)).map(f => [f.rid, f.at]))
        const state = !o.opened ? 'never_opened' : !o.closed || o.closed.at >= drainedAt ? 'open' : o.closed.at < holdStart ? 'closed_before_hold' : 'closed_during_step'
        for (const s of expected) {
          if (frames.has(s.rid)) { if (state === 'open') lat.push(frames.get(s.rid) - (s.start + s.ms)) }
          else if (state === 'open') missing[s.label] = (missing[s.label] || 0) + 1
        }
        for (const f of o.frames) if (f.at >= holdStart && f.at < drainedAt) {
          const key = f.type === 'change' && !writes.has(f.rid) ? 'change_unmatched:' + (f.rid?.startsWith(prefix) ? f.rid.slice(prefix.length).split('.')[0] : f.rid ? 'other' : 'none') : f.type
          frameTypes[key] = (frameTypes[key] || 0) + 1
        }
        perObserver.push({ observer: i, workspace: wsIndex, state, expected: expected.length, delivered: expected.filter(s => frames.has(s.rid)).length,
          closed: o.closed ? { code: o.closed.code, at_from_hold_start_ms: r1(o.closed.at - holdStart) } : null })
      }
      const openObs = perObserver.filter(o => o.state === 'open')
      const rows = activity.split('\n').filter(l => l.includes('|')).map(l => l.split('|'))
      const events = {}
      for (const r of rows) for (const ev of r[5].split(',').filter(Boolean)) events[ev] = (events[ev] || 0) + 1
      const holdHealth = healthRows.filter(h => h.at >= holdStart && h.at < holdEnd)
      const holdStats = statsRows.filter(s => s.at >= holdStart && s.at < holdEnd)
      const statOf = name => { const xs = holdStats.filter(s => s.name === name); return { samples: xs.length, cpu_mean: r1(mean(xs.map(s => s.cpu)) ?? NaN), cpu_max: r1(Math.max(0, ...xs.map(s => s.cpu))), mem_last: xs.at(-1)?.mem ?? null } }
      const loops = outs.map(o => o.loopDelayMs), loopP99 = Math.max(...loops.map(l => l.p99))
      const appended = appendRows.flat().filter(r => r.start >= holdStart && r.start < holdEnd)
      Object.assign(step, {
        window: { rampMs: RAMP_MS, holdMs: HOLD_MS, drainMs: r1(Math.max(...outs.map(o => o.clientsDoneAt)) - holdEnd) },
        valid: loopP99 <= 50, invalidReason: loopP99 <= 50 ? null : 'client-bound: worker event-loop p99 ' + loopP99 + ' ms > 50 ms',
        inFlight: { ...dist(holdTicks), mean: r1(mean(holdTicks) ?? NaN) },
        requests: inHold.length, throughput_rps: r1(inHold.length / (HOLD_MS / 1000)), outcomes: tally(inHold, s => s.cls),
        busy_without_retry_after: inHold.filter(s => s.cls === '503:server_busy' && !s.retryAfter).length,
        pooled_2xx_ms: dist(mix.filter(s => s.cls === '2xx').map(s => s.ms)), pooled_all_ms: censoredDist(mix, TIMEOUT_MS),
        light_p95_2xx_ms: dist(mix.filter(s => s.cls === '2xx' && LIGHT.has(s.label)).map(s => s.ms)).p95,
        heavy_p95_2xx_ms: dist(mix.filter(s => s.cls === '2xx' && !LIGHT.has(s.label)).map(s => s.ms)).p95,
        light_p95_all_ms: censoredDist(mix.filter(s => LIGHT.has(s.label)), TIMEOUT_MS).p95,
        heavy_p95_all_ms: censoredDist(mix.filter(s => !LIGHT.has(s.label)), TIMEOUT_MS).p95,
        routes,
        fairness: { clients: counts.length, min: Math.min(...counts), mean: r1(sum / counts.length), max: Math.max(...counts),
          min_over_mean: r3(Math.min(...counts) / (sum / counts.length)), jain: r3(sum * sum / (counts.length * sq)),
          below_half_mean: counts.filter(c => c < 0.5 * sum / counts.length).length },
        client: { worker_loop_delay_ms: loops, main_loop_delay_ms: { p99: r1(mainLoop.percentile(99) / 1e6), max: r1(mainLoop.max / 1e6) }, main_blocking_ms: mainBlocking,
          main_cpu_cores: r3((cpu.user + cpu.system) / 1000 / wall), worker_cpu_cores: outs.map(o => o.cpuMs === null ? null : r3(o.cpuMs / wall)),
          connections_opened: outs.reduce((a, o) => a + o.connections, 0),
          status_restore: { attempted: statusRestore.attempted, restored: statusRestore.restored, failed: statusRestore.failed, after_recovery: recovery !== null } },
        server: {
          pool: poolDelta(hBefore?.pool, hAfter?.pool), delivery_pool: poolDelta(hBefore?.delivery, hAfter?.delivery),
          pool_active_max: Math.max(0, ...holdHealth.map(h => h.pool?.active_connections ?? 0)),
          delivery_active_max: Math.max(0, ...holdHealth.map(h => h.delivery?.active_connections ?? 0)),
          http_admission: hAfter?.admission ? { before: hBefore?.admission ?? null, after: hAfter.admission, delta: numericDelta(hBefore?.admission, hAfter.admission) } : null,
          health_probe_ms: dist(holdHealth.map(h => h.ms)), health_status: tally(holdHealth, h => h.health),
          rss_max: Math.max(0, ...holdHealth.map(h => h.proc?.rss ?? 0)), hwm: procAfter?.hwm ?? null, threads: procAfter?.threads ?? null,
          cpu_cores: procBefore && procAfter ? r3((procAfter.cpu_s - procBefore.cpu_s) / (wall / 1000)) : null,
          docker: { [SERVER]: statOf(SERVER), [PG]: statOf(PG) },
          restarted: JSON.stringify(stateBefore) !== JSON.stringify(stateAfter), state: stateAfter },
        postgres: { samples: rows.length, backends_max: Math.max(0, ...rows.map(r => +r[0])), active_max: Math.max(0, ...rows.map(r => +r[1])),
          active_mean: r1(mean(rows.map(r => +r[1])) ?? NaN), idle_in_tx_max: Math.max(0, ...rows.map(r => +r[2])),
          lock_wait_samples: rows.filter(r => +r[3] > 0).length, ungranted_max: Math.max(0, ...rows.map(r => +r[4])),
          active_wait_events: Object.fromEntries(Object.entries(events).sort((a, b) => b[1] - a[1]).slice(0, 10)),
          database: numericDelta(dbBefore, dbAfter), statements },
        websocket: { observers: observers.length, open_at_end: observers.filter(o => o.opened && !o.closed).length, writes: writes.size,
          open: { observers: openObs.length, expected: openObs.reduce((a, o) => a + o.expected, 0), delivered: openObs.reduce((a, o) => a + o.delivered, 0),
            min_observer_fraction: openObs.length ? r3(Math.min(...openObs.map(o => o.expected ? o.delivered / o.expected : 1))) : null,
            undelivered_by_route: missing, commit_to_frame_ms: dist(lat) },
          not_open: perObserver.filter(o => o.state !== 'open'),
          per_observer: perObserver.map(o => [o.observer, o.workspace, o.state, o.expected, o.delivered]), per_observer_columns: ['observer', 'workspace', 'state', 'expected', 'delivered'],
          frame_types: frameTypes },
        capture: { appenders: capture.length, appends_2xx_ms: dist(appended.filter(r => r.status === 200).map(r => r.ms)), appends_all_ms: censoredDist(appended, TIMEOUT_MS),
          outcomes: tally(appended, r => r.cls), proofs: tally(proofs, x => x) },
        recovery,
        websocket_events: log.events,
        log: { samples: inHold.length, joined: joined.length, join_rate: inHold.length ? r3(joined.length / inHold.length) : null, log_bytes: log.bytes,
          server_status: tally(joined, s => log.entries.get(s.rid).status) } })
      console.error('step ' + clients + ': ' + step.throughput_rps + ' req/s, pooled p95 2xx-only ' + step.pooled_2xx_ms.p95 + ' ms, all-outcome ' + step.pooled_all_ms.p95 +
        ' ms, outcomes ' + JSON.stringify(step.outcomes) +
        ', join ' + step.log.join_rate + ', loop p99 ' + loopP99 + ' ms')
      writeFileSync(file, JSON.stringify(result, null, 1))
      if (stepIndex < steps.length - 1) await sleep(COOLDOWN_MS)
    }
  } catch (error) {
    result.error = String(error?.stack || error).slice(0, 2000)
    process.exitCode = 1
  } finally {
    for (const w of live) w.stop()
    try { stopSampler() } catch (e) { result.samplerStopError = String(e.message).slice(0, 200) }
    try { sh(['exec', PG, 'sh', '-c', 'rm -f ' + TMP + '-*']) } catch {}
    // After an abort or a failed restore: wait for the server to drain, then PUT todo
    // on every status target (attempts lost with the workers are unknown).
    if (restorePending) {
      try {
        const recovered = await awaitRecovery(now(), 30) !== null
        result.abort_status_restore = { ...await restoreStatuses(allStatusTargets, putTodo('abort_status_restore')), after_recovery: recovered }
        if (result.abort_status_restore.failedIds.length) process.exitCode = 1
      } catch (e) { result.abort_status_restore = { error: String(e?.message || e).slice(0, 200) }; process.exitCode = 1 }
    }
    monitor.stop = true; await monitor.done
    stats.kill()
    for (const o of observers) o.close()
    agent.destroy()
  }
  try { if (sh(['inspect', '--format', '{{.Image}}', SERVER]) !== imageId) { result.imageChanged = true; process.exitCode = 1 } } catch (e) { result.imageAfterError = String(e.message).slice(0, 200) }
  result.finishedAt = new Date().toISOString()
  writeFileSync(file, JSON.stringify(result, null, 1))
  console.log(file)
}

// Offline checks of latency censoring and status restoration; contacts nothing.
async function selfTest() {
  const eq = (name, actual, expected) => {
    if (JSON.stringify(actual) !== JSON.stringify(expected)) throw new Error('self-test ' + name + ': ' + JSON.stringify(actual) + ' !== ' + JSON.stringify(expected))
  }
  // 97 successes of 1..97 ms and 3 timeouts: p99 and max are censored, p95 is not.
  const xs = [...Array.from({ length: 97 }, (_, i) => ({ ms: i + 1, cls: '2xx' })), ...[30001, 30002, 30000].map(ms => ({ ms, cls: 'timeout' }))]
  eq('censored', censoredDist(xs, 30000), { n: 100, timeouts: 3, p50: 50, p95: 95, p99: '>=30000', max: '>=30000' })
  eq('2xx-only', dist(xs.filter(x => x.cls === '2xx').map(x => x.ms)), { n: 97, p50: 49, p95: 93, p99: 97, max: 97 })
  // A fast non-timeout failure is an ordinary sample.
  eq('reset', censoredDist([{ ms: 5, cls: 'reset' }, { ms: 9, cls: '2xx' }], 30000), { n: 2, timeouts: 0, p50: 5, p95: 9, p99: 9, max: 9 })
  // Half timeouts: the median is censored.
  eq('median', censoredDist([{ ms: 1, cls: '2xx' }, { ms: 30000, cls: 'timeout' }, { ms: 30000, cls: 'timeout' }], 30000).p50, '>=30000')
  eq('empty', censoredDist([], 30000), { n: 0, timeouts: 0, p50: null, p95: null, p99: null, max: null })
  // Toggles start from the todo precondition; failed or timed-out attempts are
  // still restored, and a successful toggle back to todo is restored too.
  const st = { status: new Map(), attempted: new Set() }, sent = []
  const reply = { a: [200, 200], b: [0], c: [409] }
  const put = (id, next) => { sent.push(id + ':' + next); const status = reply[id].shift(); return Promise.resolve({ status, cls: status === 200 ? '2xx' : status === 0 ? 'timeout' : '409:conflict' }) }
  await toggleStatus(st, 'a', put); await toggleStatus(st, 'a', put); await toggleStatus(st, 'b', put); await toggleStatus(st, 'c', put)
  eq('toggles', sent, ['a:in_progress', 'a:todo', 'b:in_progress', 'c:in_progress'])
  eq('client map', [...st.status], [['a', 'todo']])
  const restored = [], fails = new Set(['c'])
  const out = await restoreStatuses([...st.attempted, 'a', 'b'], id => { restored.push(id); return Promise.resolve(fails.has(id) ? { status: 0, cls: 'timeout' } : { status: 200, cls: '2xx' }) }, 2)
  eq('restore ids', restored, ['a', 'b', 'c'])
  eq('restore result', out, { attempted: 3, restored: 2, failed: { timeout: 1 }, failedIds: ['c'] })
  console.log('self-test passed')
}
