// Task-owned measured integration; historical synchronization receipts stay unchanged.
import assert from 'node:assert/strict'
import { createHash, randomUUID } from 'node:crypto'
import { spawn } from 'node:child_process'
import { once } from 'node:events'
import { readFile, writeFile, access, realpath, readdir, stat } from 'node:fs/promises'
import http from 'node:http'
import { resolve, join, delimiter } from 'node:path'
import { performance } from 'node:perf_hooks'
import { chromium, request } from '../hmem-server/frontend/node_modules/playwright/index.mjs'
import { createChangeStreamManager } from '../hmem-server/frontend/src/change-stream.js'
import { makeScratch, nativeProductionInputs, DatabaseObserver, networkProxy, bounded, ownedPath, writePrefix } from '../hmem-server/frontend/perf/session-sync.mjs'

const task = '4c367662-699b-4b74-93f1-1b99f54abebb'
const candidate = 'f60573ec7ab33d6e004e0b445cdbb86f4d966dc8'
const repo = resolve(import.meta.dirname, '..')
const receiptPath = join(repo, 'tools/change-stream-qualification.json')
const workdir = '.stack-work/socket-retirement/native'
const hash = value => createHash('sha256').update(value).digest('hex')
const delay = milliseconds => new Promise(done => setTimeout(done, milliseconds))
const receipt = { schema: 1, task, candidate, owner: 'repository maintainers', retention: 'Retain compact receipt and reusable runner until superseded by matching integration qualification; task scratch is removed after owned processes join.', method: 'Windows native isolated PostgreSQL; serial builds/tests and loopback HTTP/WebSocket/browser/MCP. Monotonic phase durations; SQL counters are logical executions, sampled locks are not exact cumulative waits.', phases: [], observations: [], limitations: ['Representative controlled workload only; not the historical session-throughput/three-browser performance matrix.', 'No broad suite-root/foreground-PostgreSQL supervisor qualification.', 'OS scheduling affects observed deadline/queue intervals. Snapshot lock completion is an upper bound after the observed waiter is released.'] }
let scratch
let fixtureStarted = false, fixtureClosed = false
const retainedOwners = []
const ownedChildren = new Set()
const selfTest = process.argv.includes('--self-test')
const loadedDriverSha = hash(await readFile(new URL(import.meta.url)))
function runFlags(args) {
  assert.ok(!(args.includes('--resume-build') && args.includes('--refresh-evidence')), 'Build reuse and input refresh are incompatible')
  assert.ok(!args.includes('--integration-only') || args.includes('--resume-build'), 'Integration-only requires exact build reuse')
  return { reuseBuild: args.includes('--resume-build'), refreshEvidence: args.includes('--refresh-evidence'), integrationOnly: args.includes('--integration-only') }
}
function priorIntegrationFailure(previous) {
  if (previous.integrationFailure) return previous.integrationFailure
  if (previous.outcome === 'failed' && previous.methodSha256 === 'b82f380bb585334539353b75ad899d011736f437e657e1e8cc30c71dc33d1731'
    && previous.failure?.message === 'POST /api/v1/change-stream/resync: 502') return {
    classification: 'Historical proxy upstream502; original driver did not retain native cause/page ordinal. Unresolved original cause; not a zero-failure run.',
    phase: 'full snapshot traversal after exact SQL fixture seeding', observedAtUTC: '2026-11-11T08:33:44.791Z',
    timeAuthority: 'Coordinator observed exited-error tool receipt, not the HTTP request timestamp'
  }
  return undefined
}
function compiledInputs(values) { const copy = { ...values }; delete copy['tools/change-stream-qualification.mjs']; return copy }
function lineParser(onMetric) {
  let pending = '', failure = null
  return {
    write(text) {
      if (failure) return
      try {
        pending += text
        for (;;) {
          const at = pending.indexOf('\n'); if (at < 0) break
          assert.ok(at <= 65536, 'Oversized child line')
          const line = pending.slice(0, at).trim(); pending = pending.slice(at + 1)
          const marker = 'change_stream_qualification '
          if (line.startsWith(marker)) onMetric(JSON.parse(line.slice(marker.length)))
        }
        assert.ok(pending.length <= 65536, 'Oversized child line')
      } catch (error) { pending = ''; failure = error }
    },
    get failure() { return failure }
  }
}
async function save() { await writeFile(receiptPath, JSON.stringify(receipt, null, 2) + '\n') }
async function inputs() {
  const values = await nativeProductionInputs()
  async function tree(directory, pattern) {
    let entries
    try { entries = await readdir(join(repo, directory), { withFileTypes: true }) } catch (error) { if (error.code === 'ENOENT') return; throw error }
    for (const entry of entries) {
      const path = directory + '/' + entry.name
      assert.ok(!entry.isSymbolicLink(), 'Input manifest refuses symlinks')
      if (entry.isDirectory()) await tree(path, pattern)
      else if (pattern.test(entry.name)) values[path] = hash(await readFile(join(repo, path)))
    }
  }
  for (const directory of ['hmem-core/cbits', 'hmem-server/cbits', 'hmem-server/migrations', 'hmem-embedding-http/src', 'hmem-embedding-http/app', 'hmem-embedding-http/cbits', 'hmem-core/test', 'hmem-server/test', 'hmem-server/frontend/src', 'hmem-server/frontend/tests-js/support']) await tree(directory, /\.(hs|c|h|sql|elm|js|mjs|pem)$/)
  for (const directory of ['hmem-server/frontend/src', 'hmem-server/frontend/public', 'hmem-server/frontend/tests', 'hmem-server/test/fixtures']) await tree(directory, /.*/)
  for (const path of ['hmem-server/frontend/index.html', 'hmem-server/frontend/perf/fixtures.mjs']) values[path] = hash(await readFile(join(repo, path)))
  values['config/managed-embedding-provenance.yaml'] = hash(await readFile(join(repo, 'config/managed-embedding-provenance.yaml')))
  for (const path of ['hmem-embedding-http/hmem-embedding-http.cabal', 'hmem-embedding-http/package.yaml', 'hmem-server/frontend/elm.json', 'hmem-server/frontend/package.json', 'hmem-server/frontend/vite.config.js', 'hmem-server/frontend/tests/ChangeStreamTest.elm', 'hmem-server/frontend/tests-js/hierarchy-fixture.mjs', 'hmem-server/frontend/perf/session-sync.mjs']) {
    try { values[path] = hash(await readFile(join(repo, path))) } catch (error) { if (error.code !== 'ENOENT') throw error }
  }
  for (const path of ['tools/change-stream-qualification.mjs', 'hmem-server/test/HMem/Server/ChangeStreamSpec.hs', 'hmem-server/test/HMem/Server/WebSocketDeliverySpec.hs', 'hmem-core/test/HMem/DB/ChangeStreamSpec.hs', 'hmem-server/frontend/package-lock.json', 'hmem-server/frontend/src/change-stream.js', 'hmem-server/frontend/src/main.js', 'hmem-server/frontend/src/Feature/ChangeStream.elm', 'hmem-server/frontend/tests-js/change-stream.test.js', 'hmem-server/frontend/tests-js/change-stream-recovery.browser.test.js']) values[path] = hash(await readFile(join(repo, path)))
  return values
}
async function consumerCleanup(consumers, fixture, retain, primaryFailure = null) {
  const failures = []
  for (const [name, action] of consumers) try { await action() } catch (error) { failures.push({ name, error }) }
  if (failures.length) {
    retain(failures)
    throw new AggregateError([...(primaryFailure ? [primaryFailure] : []), ...failures.map(value => value.error)], 'Consumers unconfirmed; fixture/PG teardown retained')
  }
  try { await fixture() } catch (error) { retain([{ name: 'fixture', error }]); if (primaryFailure) throw new AggregateError([primaryFailure, error], 'Primary scenario and fixture cleanup failed'); throw error }
}
function observeProxyErrors(origin, events) {
  const target = new URL(origin), original = http.request
  function observed(options, ...rest) {
    const child = original.call(this, options, ...rest)
    if (options?.hostname === target.hostname && String(options.port) === target.port) child.once('error', error => {
      if (events.length < 32) events.push({ code: typeof error.code === 'string' ? error.code : 'unknown', syscall: typeof error.syscall === 'string' ? error.syscall : null })
    })
    return child
  }
  http.request = observed
  return () => { assert.equal(http.request, observed); http.request = original }
}
function trackedMcpSpawn(owners) {
  return (command, args, options) => {
    const child = spawn(command, args, options)
    const owner = { child, errorCode: null }
    ownedChildren.add(child)
    owner.closed = new Promise(resolve => child.once('close', (code, signal) => { ownedChildren.delete(child); resolve([code, signal]) }))
    child.on('error', error => { owner.errorCode = error.code || 'unknown' })
    owners.push(owner)
    return child
  }
}
async function child(command, args, { cwd = repo, env = scratch.env, limitMs = 600000, metrics = true } = {}) {
  const started = performance.now()
  const process = spawn(command, args, { cwd, env, windowsHide: true, stdio: ['ignore', 'pipe', 'pipe'] })
  const closed = once(process, 'close')
  ownedChildren.add(process)
  closed.then(() => ownedChildren.delete(process), () => ownedChildren.delete(process))
  let tail = '', stdout = ''
  const stdoutParser = lineParser(value => { if (metrics) receipt.observations.push(value) })
  const stderrParser = lineParser(value => { if (metrics) receipt.observations.push(value) })
  function consume(bytes, parser) {
    const text = bytes.toString(); tail = (tail + text).slice(-12000)
    parser.write(text)
  }
  process.stdout.on('data', bytes => { stdout = (stdout + bytes.toString()).slice(-65536); consume(bytes, stdoutParser) }); process.stderr.on('data', bytes => consume(bytes, stderrParser))
  // A timeout preserves the owned child and scratch for explicit disposition;
  // it never signals an inferred PID or deletes a live child's resources.
  const [code, signal] = await bounded(closed, limitMs, 'owned child completion')
  const phase = { command, args, exitCode: code, signal, milliseconds: performance.now() - started, summary: tail.slice(-1200) }
  const parserFailure = stdoutParser.failure || stderrParser.failure
  if (parserFailure) phase.parserFailure = parserFailure.message
  if (!selfTest) { receipt.phases.push(phase); await save() }
  if (parserFailure) throw parserFailure
  assert.equal(code, 0, tail)
  return stdout
}
async function gates(reuseBuild = false, measurementsOnly = false, integrationOnly = false) {
  if (!reuseBuild) {
    receipt.prebuild = await inputs(); receipt.capturedAt = new Date().toISOString(); await save()
    await child('stack', ['--work-dir', workdir, 'build', 'hmem-core:test:hmem-core-test', 'hmem-server:test:hmem-server-test', 'hmem-server:exe:hmem-server', 'hmem-server:exe:hmem-test-harness', 'hmem-mcp:exe:hmem-mcp', '--test', '--no-run-tests'], { limitMs: 1200000 })
  }
  assert.deepEqual(compiledInputs(await inputs()), compiledInputs(receipt.prebuild), 'Frozen compiled source/package inputs changed during build')
  if (integrationOnly) {
    for (const name of ['hmem-server', 'hmem-test-harness', 'hmem-mcp']) assert.equal(hash(await readFile(receipt.binaries[name].path)), receipt.binaries[name].sha256, 'Reused binary drift: ' + name)
    return
  }
  const install = (await child('stack', ['--work-dir', workdir, 'path', '--local-install-root'], { metrics: false })).trim()
  const expectedBinaries = reuseBuild ? receipt.binaries : null
  receipt.binaries = {}
  for (const name of ['hmem-server', 'hmem-test-harness', 'hmem-mcp']) {
    const path = join(install, 'bin', name + '.exe'); receipt.binaries[name] = { path, sha256: hash(await readFile(path)) }
    if (expectedBinaries) assert.equal(receipt.binaries[name].sha256, expectedBinaries[name].sha256, 'Reused executable drift: ' + name)
  }
  const selections = measurementsOnly ? [
    ['hmem-server', 'hmem-server-test', 'reauthorizes idle clients once per elapsed tick'],
    ['hmem-server', 'hmem-server-test', 'measures causal pool queueing']
  ] : [
    ['hmem-core', 'hmem-core-test', 'Change-stream state machine'],
    ['hmem-server', 'hmem-server-test', 'canonical change-stream loopback'],
    ['hmem-server', 'hmem-server-test', 'owned WebSocket delivery']
  ]
  for (const [packageName, executable, match] of selections) {
    const path = join(repo, packageName, workdir, 'dist/56bb250d/build', executable, executable + '.exe')
    receipt.binaries[executable] = { path, sha256: hash(await readFile(path)) }
    await child(path, ['--match', match], { limitMs: 600000 })
  }
  await save()
}
async function integration(large = true) {
  const native = receipt.binaries['hmem-test-harness'].path
  const managed = spawn(native, ['deployed', '--interactive'], { cwd: repo, env: { ...scratch.env, HMEM_TEST_SESSION_DIAGNOSTICS: '1', HMEM_CHANGE_STREAM_SNAPSHOT_SESSION_TTL_SECONDS: '120', HMEM_CHANGE_STREAM_RESUME_TOKEN_TTL_SECONDS: '600' }, windowsHide: true, stdio: ['pipe', 'pipe', 'pipe'] })
  const closed = once(managed, 'close'); managed.stderr.resume()
  ownedChildren.add(managed)
  closed.then(() => ownedChildren.delete(managed), () => ownedChildren.delete(managed))
  fixtureStarted = true
  const coordinates = {}; let token, lines = '', browser, page, api, observer, proxy, mcp, primaryFailure, manager, restoreProxyObserver
  const browserDiagnostics = { apiResponses: {}, consoleErrors: 0, pageErrorNames: [] }
  const webSocketCloses = [], proxyErrors = [], nativeErrorClasses = [], mcpOwners = []
  const progress = { phase: 'native startup', snapshotAttempts: 0, acceptedPages: 0 }
  managed.stderr.on('data', bytes => {
    const text = bytes.toString()
    for (const [pattern, classification] of [[/statement.?timeout|canceling statement due to statement timeout/i, 'statement_timeout'], [/lock.?timeout/i, 'lock_timeout'], [/connection reset|ECONNRESET/i, 'connection_reset'], [/DBException|DBOtherError/i, 'database_exception']]) if (pattern.test(text) && !nativeErrorClasses.includes(classification)) nativeErrorClasses.push(classification)
  })
  const banner = lineParser(() => {})
  let bannerFailure = null
  managed.stdout.on('data', bytes => {
    // Event callbacks latch errors. Awaited setup throws inside the owning
    // try/finally, which closes clients before requesting normal harness exit.
    try {
      banner.write(bytes.toString())
      if (banner.failure) { bannerFailure = banner.failure; return }
      lines += bytes.toString(); assert.ok(lines.length < 65536)
      for (;;) { const at = lines.indexOf('\n'); if (at < 0) break; const line = lines.slice(0, at).trim(); lines = lines.slice(at + 1)
      for (const [label, field] of [['Sandbox root:', 'root'], ['Config file :', 'config'], ['Static dir  :', 'static'], ['Postgres log:', 'pgLog'], ['Server URL  :', 'url']]) if (line.startsWith(label)) coordinates[field] = line.slice(label.length).trim()
      if (line.startsWith('Bearer PAT:')) token = line.slice('Bearer PAT:'.length).trim()
      if (line.startsWith('Press Enter')) coordinates.ready = true
      }
    } catch (error) { bannerFailure = error; lines = '' }
  })
  async function until(predicate, milliseconds, phase) { const start = performance.now(); while (!await predicate()) { if (bannerFailure) throw bannerFailure; assert.ok(performance.now() - start < milliseconds, phase); assert.equal(managed.exitCode, null, phase + ': native exited'); await delay(20) }; if (bannerFailure) throw bannerFailure }
  try {
    await until(() => coordinates.ready, 120000, 'native readiness')
    ownedPath(join(scratch.root, 'tmp'), await realpath(coordinates.root))
    ownedPath(await realpath(coordinates.root), await realpath(coordinates.static))
    const config = JSON.parse(await readFile(coordinates.config, 'utf8'))
    const origin = coordinates.url || `http://127.0.0.1:${config.server.port}`
    receipt.integrationConfiguration = { topology: 'unshaped loopback proxy, RTT0/unlimited; native PostgreSQL local', actualChangeStream: config.change_stream }
    restoreProxyObserver = observeProxyErrors(origin, proxyErrors)
    proxy = await networkProxy(origin)
    api = await request.newContext({ baseURL: proxy.origin, extraHTTPHeaders: { Authorization: 'Bearer ' + token, 'X-HMem-Bot-Identity': JSON.stringify({ role: 'Change-stream qualification', 'model-id': 'test:production:default' }) } })
    const call = async (path, method = 'GET', data) => { const response = await api.fetch(path, { method, data, timeout: 30000 }); assert.ok(response.ok(), `${method} ${path}: ${response.status()}`); return response.json() }
    observer = new DatabaseObserver(config.database, scratch.env, coordinates.pgLog); receipt.databaseMethod = await observer.initialize()
    const workspace = await call('/api/v1/workspaces', 'POST', { name: 'Combined change-stream qualification', workspace_type: 'repository' })
    const taskRow = await call('/api/v1/tasks', 'POST', { workspace_id: workspace.id, title: 'Real MCP and browser delivery' })
    const openMcp = () => import('../hmem-server/frontend/tests-js/support/session-mcp.js').then(({ startSessionMcp }) => startSessionMcp({ executable: receipt.binaries['hmem-mcp'].path, origin: proxy.origin, token, workspace: workspace.id, identity: { role: 'Change-stream qualification', 'model-id': 'test:production:default' }, env: scratch.env, spawnProcess: trackedMcpSpawn(mcpOwners) }))
    mcp = await openMcp()
    const before = await observer.stats()
    const changed = await mcp.tool('task_update', { task_id: taskRow.id, title: 'Observed combined MCP update' }); assert.ok(changed)
    const after = await observer.stats()
    receipt.observations.push({ name: 'combined-mcp-http', observation: { taskUpdated: true, statementCalls: after.statements.calls - before.statements.calls, milliseconds: after.at - before.at } })
    // Complete an actual paged full snapshot, then retain its terminal token
    // through independent materialized-session expiry and real worker cleanup.
    const browserSetupOnly = process.argv.includes('--browser-setup')
    const seeded = large && !browserSetupOnly ? 25601 : 301
    progress.phase = 'bounded SQL fixture seed'
    for (let first = 1; first <= seeded; first += 500) await observer.raw(`INSERT INTO projects(workspace_id,name) SELECT '${workspace.id}'::uuid,'qualification-full-'||n FROM generate_series(${first},${Math.min(first + 499, seeded)}) n`)
    const expected = await observer.query(`SELECT json_build_object('count',count(*),'md5',md5(string_agg(id::text,',' ORDER BY id::text))) FROM projects WHERE workspace_id='${workspace.id}' AND name LIKE 'qualification-full-%'`)
    if (!browserSetupOnly) {
    const scope = { scope: 'workspace', workspace_id: workspace.id }
    const started = performance.now()
    progress.phase = 'single production-manager full snapshot'
    const statuses = {}, accepted = new Set(), starts = new Set(), values = new Map()
    let snapshot, acceptedToken, acceptedPages = 0, finish, reject
    const complete = new Promise((resolve, fail) => { finish = resolve; reject = fail })
    class Socket extends WebSocket {
      constructor(url) { super(url); webSocketCloses.push(new Promise(resolve => this.addEventListener('close', resolve, { once: true }))) }
    }
    manager = createChangeStreamManager({ apiUrl: proxy.origin, wsUrl: proxy.origin.replace('http:', 'ws:') + '/api/v1/ws', WebSocketImpl: Socket,
      storage: { getItem: key => values.get(key) || null, setItem: (key, value) => values.set(key, value), removeItem: key => values.delete(key) },
      fetchImpl: async (url, options) => {
        const body = JSON.parse(options.body)
        if (body.start_idempotency_key) starts.add(body.start_idempotency_key)
        const response = await api.fetch(url, { method: options.method, headers: options.headers, data: options.body, timeout: 30000 })
        const resync = url.endsWith('/resync')
        if (resync) { progress.snapshotAttempts++; statuses[response.status()] = (statuses[response.status()] || 0) + 1 }
        return { ok: response.ok(), status: response.status(), headers: { get: name => response.headers()[name.toLowerCase()] }, json: async () => {
          const page = await response.json()
          if (resync) { acceptedPages++; progress.acceptedPages = acceptedPages; accepted.add(JSON.stringify(body.page_token ? ['page', body.page_token] : ['start', body.start_idempotency_key])) }
          return page
        } }
      },
      onSnapshot: (_scope, _workspace, value) => { assert.equal(snapshot, undefined, 'No snapshot restart or duplicate publication'); snapshot = value; return true },
      onFrame: (_scope, _workspace, raw) => { const frame = JSON.parse(raw); if (frame.type === 'checkpoint') { acceptedToken = frame.resume_token; queueMicrotask(finish) }; return true },
      onState: (_scope, _workspace, phase) => { if (!['resyncing', 'connecting', 'replaying', 'reconnecting'].includes(phase)) reject(new Error('Real snapshot stream stopped: ' + phase)) }
    })
    manager.connect({ audienceId: 'qualification', scope: 'workspace', workspaceId: workspace.id, snapshotProfile: 'full_v1' })
    await bounded(complete, 180000, 'single production-manager full snapshot attempt')
    assert.equal(starts.size, 1); assert.equal(acceptedPages, accepted.size)
    const ids = snapshot.items.filter(item => item.kind === 'project' && item.data.name.startsWith('qualification-full-')).map(item => item.data.id).sort()
    assert.equal(ids.length, seeded); assert.equal(new Set(ids).size, seeded)
    assert.equal(expected.count, seeded); assert.equal(createHash('md5').update(ids.join(',')).digest('hex'), expected.md5)
    if (large) assert.ok(acceptedPages > 256)
    assert.ok(acceptedToken)
    const resume = acceptedToken
    progress.phase = 'ordinary cleanup with accepted successor bearer'
    receipt.observations.push({ name: large ? 'real-full-snapshot' : 'real-small-snapshot-probe', observation: { acceptedPages, acceptedUniquePages: accepted.size, seededProjects: seeded, exactSeededIdentitySetMatched: true, items: snapshot.items.length, statusAttempts: statuses, proxyErrors, milliseconds: performance.now() - started, pageSize: 100 } })
    // Expire only snapshot material in the owned DB, then wait for the ordinary
    // worker's next elapsed maintenance turn; independent bearer validation is
    // performed through its public ticket endpoint.
    await observer.raw(`UPDATE change_stream_snapshot_sessions SET expires_at=clock_timestamp()-interval '1 second' WHERE workspace_id='${workspace.id}'`)
    await until(async () => (await observer.query(`SELECT count(*)::int FROM change_stream_snapshot_sessions WHERE workspace_id='${workspace.id}'`)) === 0, 75000, 'ordinary snapshot cleanup')
    const ticket = await call('/api/v1/change-stream/ticket', 'POST', { scope, resume_token: resume }); assert.ok(ticket.ticket)
    receipt.observations.push({ name: 'cleanup-independent-bearer', observation: { ordinaryCleanupObserved: true, publicTicketAccepted: true, bearer: 'accepted final checkpoint successor of the same snapshot lineage' } })
    }
    const systemChrome = 'C:/Program Files/Google/Chrome/Application/chrome.exe'
    const bundledAvailable = await access(chromium.executablePath()).then(() => true, () => false)
    browser = await chromium.launch({ headless: true, ...(bundledAvailable ? {} : { executablePath: systemChrome }) })
    progress.phase = 'real browser and MCP integration'
    page = await browser.newPage()
    page.on('response', response => { const path = new URL(response.url()).pathname; if (path.startsWith('/api/v1/')) { const key = path.replace(/[0-9a-f]{8}-[0-9a-f-]{27,}/g, ':id') + ':' + response.status(); browserDiagnostics.apiResponses[key] = (browserDiagnostics.apiResponses[key] || 0) + 1 } })
    page.on('console', message => { if (message.type() === 'error') browserDiagnostics.consoleErrors++ })
    page.on('pageerror', error => browserDiagnostics.pageErrorNames.push(error.name))
    // Build production assets for this actual loopback origin before navigating.
    const buildEnv = { ...scratch.env, VITE_HMEM_API_URL: proxy.origin, VITE_HMEM_WS_URL: proxy.origin.replace('http:', 'ws:') + '/api/v1/ws', VITE_HMEM_AUTH_MODE: 'deployed' }
    const pathKey = Object.keys(buildEnv).find(key => key.toLowerCase() === 'path') || 'PATH'
    buildEnv[pathKey] = join(repo, 'hmem-server/frontend/node_modules/.bin') + delimiter + (buildEnv[pathKey] || '')
    await child(process.execPath, ['node_modules/vite/bin/vite.js', 'build', '--outDir', coordinates.static, '--emptyOutDir'], { cwd: join(repo, 'hmem-server/frontend'), env: buildEnv, limitMs: 90000 })
    await page.context().addCookies([{ name: 'hmem_session', value: 'sandbox-session-token', url: proxy.origin }, { name: 'hmem_csrf', value: 'sandbox-csrf-token', url: proxy.origin }])
    if (browserSetupOnly) {
      const sessionResponse = page.waitForResponse(response => new URL(response.url()).pathname === '/api/v1/session', { timeout: 10000 })
      await page.goto(proxy.origin + '/workspace/' + workspace.id + '#tab=tasks')
      browserDiagnostics.sessionHttpStatus = (await sessionResponse).status()
      await page.locator('#hierarchy-viewport').waitFor({ timeout: 10000 })
      browserDiagnostics.beforeFocus = await page.evaluate(id => ({ unsupportedFragment: location.hash === '#tab=tasks', taskMounted: Boolean(document.getElementById('entity-' + id)), mountedProjects: document.querySelectorAll('[data-hierarchy-key^="project:"]').length, mountedTasks: document.querySelectorAll('[data-hierarchy-key^="task:"]').length }), taskRow.id)
    }
    await page.goto(proxy.origin + '/workspace/' + workspace.id + '#tab=projects&focus=task:' + taskRow.id)
    await page.getByText('Observed combined MCP update', { exact: true }).first().waitFor({ timeout: 30000 })
    browserDiagnostics.supportedFocusedTaskVisible = true
    receipt.observations.push({ name: 'browser-focused-route-setup', observation: browserDiagnostics })
    const liveStarted = performance.now()
    await mcp.tool('task_update', { task_id: taskRow.id, title: 'Visible combined live update' })
    await page.getByText('Visible combined live update', { exact: true }).first().waitFor({ timeout: 30000 })
    receipt.observations.push({ name: 'real-browser-mcp-visible', observation: { milliseconds: performance.now() - liveStarted, browserVersion: browser.version() } })
    if (browserSetupOnly) { assert.deepEqual(browserDiagnostics.pageErrorNames, []); return }
    // Exercise the merged follower path, including fresh-process verification
    // of an existing immutable prefix, while the real browser remains live.
    await mcp.tool('task_start', { task_id: taskRow.id })
    const source = join(scratch.root, 'combined-follower.jsonl')
    const fixture = await writePrefix(source, 1048576)
    const binding = { session_id: randomUUID(), path: source, task_id: taskRow.id, title: 'Combined stream qualification', format_label: 'Bounded benchmark JSONL' }
    async function caughtUp() {
      let state
      await until(async () => {
        state = await mcp.tool('session_upload_status', { session_id: binding.session_id })
        assert.notEqual(state.status, 'failed', 'Follower failed')
        return state.status === 'caught_up' && state.committed_offset === fixture.bytes
      }, 30000, 'finite one-MiB follower catchup')
      return state
    }
    async function stopVerified() {
      const stopped = await mcp.tool('session_upload_stop', { session_id: binding.session_id })
      let operation = stopped.requested_operation || stopped.operation
      assert.ok(operation?.id)
      const id = operation.id
      await until(async () => {
        if (operation?.id === id && !operation.pending && operation.receipt) return true
        operation = (await mcp.tool('session_upload_status', { session_id: binding.session_id })).operation
        return false
      }, 30000, 'finite one-MiB stop receipt')
      const proof = operation.receipt
      assert.equal(proof.source_verified, true); assert.equal(proof.capture_boundary, fixture.bytes)
      assert.equal(proof.committed_offset, fixture.bytes); assert.equal(proof.pending_tail, false); assert.equal(proof.sha256, fixture.sha256)
    }
    const uploadStarted = performance.now()
    await mcp.tool('session_upload_start', binding); await caughtUp(); await stopVerified()
    const uploadMs = performance.now() - uploadStarted
    await mcp.close()
    mcp = await openMcp()
    const resumed = performance.now()
    await mcp.tool('session_upload_start', binding); await caughtUp()
    const verifiedMs = performance.now() - resumed
    const healthy = performance.now()
    await mcp.tool('task_update', { task_id: taskRow.id, title: 'Healthy during resumed follower' })
    await page.getByText('Healthy during resumed follower', { exact: true }).first().waitFor({ timeout: 30000 })
    const healthyMs = performance.now() - healthy
    await stopVerified()
    const canonical = await mcp.tool('session_detail', { session_id: binding.session_id })
    await mcp.tool('session_terminate', { session_id: binding.session_id, expected_offset: canonical.committed_bytes, expected_revision: canonical.revision, final_bytes: fixture.bytes, final_sha256: fixture.sha256 })
    const sealed = await mcp.tool('session_detail', { session_id: binding.session_id })
    assert.equal(sealed.state, 'terminated'); assert.equal(sealed.final_sha256, fixture.sha256)
    receipt.observations.push({ name: 'combined-real-follower-restart', observation: { bytes: fixture.bytes, uploadAndVerifiedStopMs: uploadMs, freshProcessVerifiedResumeMs: verifiedMs, ordinaryMcpToBrowserUpperBoundMs: healthyMs, exactStopsAndSealVerified: true } })
  } catch (error) {
    primaryFailure = error
    if (page) browserDiagnostics.document = await bounded(page.evaluate(() => ({ readyState: document.readyState, title: document.title, hasHierarchy: Boolean(document.querySelector('#hierarchy-viewport')), hasPasswordInput: Boolean(document.querySelector('input[type=password]')), mountedRows: document.querySelectorAll('[data-hierarchy-key]').length })), 1000, 'bounded failure document observation').catch(() => ({ unavailable: true }))
    receipt.integrationFailure = { ...progress, observedAtUTC: new Date().toISOString(), proxyErrors, nativeErrorClasses, browserDiagnostics, classification: proxyErrors.length ? 'observed proxy upstream socket error; see sanitized code, not a native HTTP status' : 'no classified proxy upstream error observed' }
    throw error
  }
  finally {
    await consumerCleanup([
      ['real manager', async () => { if (manager) manager.disconnectAll(); await bounded(Promise.all(webSocketCloses), 10000, 'owned real WebSocket clients closed') }],
      ['browser', async () => { if (browser) await bounded(browser.close(), 15000, 'owned browser close') }],
      ['MCP', async () => { if (mcp) await mcp.close(); await bounded(Promise.all(mcpOwners.map(owner => owner.closed)), 10000, 'all actual MCP children joined including rejected setup') }],
      ['SQL observer', async () => { if (observer) await observer.close() }],
      ['HTTP', async () => { if (api) await bounded(api.dispose(), 5000, 'HTTP cleanup') }],
      ['proxy', async () => { if (proxy) await proxy.close(); if (restoreProxyObserver) restoreProxyObserver() }]
    ], async () => {
        if (managed.exitCode === null) managed.stdin.end('\n')
        const [code] = await bounded(closed, 30000, 'owned native teardown'); assert.equal(code, 0)
        assert.ok(coordinates.root, 'Native joined but scratch identity was never observed')
        await assert.rejects(access(coordinates.root), error => error.code === 'ENOENT')
        receipt.cleanup = { nativeExitCode: code, ownedSandboxRemoved: true }
        fixtureClosed = true
    }, failures => {
      retainedOwners.push({ managed, closed, browser, mcp, mcpOwners, observer, proxy, manager, webSocketCloses, primaryFailure, failures })
      receipt.retained = { owner: 'qualification developer', reason: 'unconfirmed consumers; actual owners/handles and shared fixture retained', disposition: 'Resolve with the same owners before any PG/scratch teardown; next agent checkpoint must explicitly audit this unresolved failure. Do not treat a PID or file as cleanup authority.' }
    }, primaryFailure)
  }
}
async function parserSelfTests() {
  scratch = { env: process.env }
  assert.throws(() => runFlags(['--resume-build', '--refresh-evidence']))
  assert.deepEqual(runFlags(['--refresh-evidence']), { reuseBuild: false, refreshEvidence: true, integrationOnly: false })
  assert.throws(() => runFlags(['--integration-only']))
  assert.equal(runFlags(['--resume-build', '--integration-only']).integrationOnly, true)
  assert.equal(priorIntegrationFailure({ outcome: 'browser-setup-passed' }), undefined)
  assert.equal(priorIntegrationFailure({ outcome: 'failed', failure: { message: 'unrelated setup assertion' } }), undefined)
  assert.ok(priorIntegrationFailure({ outcome: 'failed', methodSha256: 'b82f380bb585334539353b75ad899d011736f437e657e1e8cc30c71dc33d1731', failure: { message: 'POST /api/v1/change-stream/resync: 502' } }).classification.includes('Unresolved'))
  const values = [], parser = lineParser(value => values.push(value))
  parser.write('change_stream_qualifi'); parser.write('cation {"name":"ok"}\n')
  assert.equal(parser.failure, null); assert.deepEqual(values, [{ name: 'ok' }])
  for (const code of ['console.log("change_stream_qualification {")', 'console.log("x".repeat(70000))']) {
    await assert.rejects(child(process.execPath, ['-e', code], { limitMs: 5000 }))
    assert.equal(ownedChildren.size, 0, 'Parser failure must be reported only after actual child close')
  }
  const fake = spawn(process.execPath, ['-e', 'console.log("x".repeat(70000));process.stdin.resume();process.stdin.on("end",()=>process.exit(0))'], { windowsHide: true, stdio: ['pipe', 'pipe', 'pipe'] })
  const closed = once(fake, 'close'), failed = lineParser(() => {})
  fake.stdout.on('data', bytes => failed.write(bytes.toString())); fake.stderr.resume()
  let observed = false
  try {
    const end = performance.now() + 5000
    while (!failed.failure) { assert.ok(performance.now() < end); await delay(10) }
    observed = true
    throw failed.failure
  } catch (error) { assert.equal(error, failed.failure) }
  finally { fake.stdin.end('\n'); const [code] = await bounded(closed, 5000, 'parser failure normal stdin cleanup'); assert.equal(code, 0) }
  assert.equal(observed, true)
  await assert.rejects(child(process.execPath, ['-e', 'setTimeout(()=>{},150)'], { limitMs: 20 }))
  assert.equal(ownedChildren.size, 1, 'A timed-out child remains actually owned until close')
  await Promise.all([...ownedChildren].map(process => once(process, 'close')))
  assert.equal(ownedChildren.size, 0)
  const calls = [], handle = {}, retained = []
  const primary = new Error('fake primary scenario failure')
  await assert.rejects(consumerCleanup([
    ['failed', async () => { calls.push('failed'); throw new Error('fake consumer timeout') }],
    ['other', async () => { calls.push('other') }]
  ], async () => calls.push('fixture'), failures => retained.push({ handle, failures }), primary), error => error.errors[0] === primary && error.errors.length === 2)
  assert.deepEqual(calls, ['failed', 'other']); assert.equal(retained[0].handle, handle)
  await assert.rejects(consumerCleanup([], async () => { throw new Error('fake native join timeout') }, failures => retained.push({ handle, failures })))
  assert.equal(retained.length, 2)
  const setupOwners = []
  const spawnSetup = trackedMcpSpawn(setupOwners)
  let fixtureStopped = false, setupRetained = false
  try {
    await assert.rejects((async () => { spawnSetup(process.execPath, ['-e', 'process.stdin.resume();process.stdin.on("end",()=>process.exit(0))'], { windowsHide: true, stdio: ['pipe', 'pipe', 'pipe'] }); throw new Error('fake init and helper join rejected') })())
    await assert.rejects(consumerCleanup([
      ['MCP setup owner', () => bounded(setupOwners[0].closed, 20, 'fake setup join')]
    ], async () => { fixtureStopped = true }, () => { setupRetained = true }))
    assert.equal(fixtureStopped, false); assert.equal(setupRetained, true); assert.equal(ownedChildren.size, 1)
  } finally { setupOwners[0].child.stdin.end('\n'); await bounded(setupOwners[0].closed, 5000, 'fake setup child normal exit') }
  assert.equal(ownedChildren.size, 0)
  const upstream = http.createServer((_request, response) => response.destroy())
  await new Promise(resolve => upstream.listen(0, '127.0.0.1', resolve))
  const errors = [], origin = 'http://127.0.0.1:' + upstream.address().port
  const restore = observeProxyErrors(origin, errors)
  const proxy = await networkProxy(origin)
  try { const response = await fetch(proxy.origin); assert.equal(response.status, 502); assert.equal(errors[0].code, 'ECONNRESET') }
  finally { await proxy.close(); restore(); await new Promise(resolve => upstream.close(resolve)) }
  console.log('Parser, timeout ownership, consumer-gated cleanup, fixture-failure retention and sanitized proxy502 classification self-tests passed')
}
if (selfTest) { await parserSelfTests(); process.exitCode = 0 }
else {
try {
  const { reuseBuild, refreshEvidence, integrationOnly } = runFlags(process.argv)
  if (reuseBuild || refreshEvidence) {
    const previous = JSON.parse(await readFile(receiptPath, 'utf8'))
    assert.equal(previous.candidate, candidate)
    assert.equal(previous.phases.findLast(phase => phase.command === 'stack' && phase.args.includes('build'))?.exitCode, 0, 'Only the latest completed actual build may be reused')
    const current = await inputs()
    for (const [path, expected] of Object.entries(previous.prebuild)) if (!['tools/change-stream-qualification.mjs', ...(refreshEvidence ? ['hmem-server/test/HMem/Server/ChangeStreamSpec.hs'] : [])].includes(path)) assert.equal(current[path], expected, 'Captured input drift: ' + path)
    Object.assign(receipt, previous)
    receipt.attempts = [...(previous.attempts || []), { methodSha256: previous.methodSha256, outcome: previous.outcome, failure: previous.failure, integrationFailure: priorIntegrationFailure(previous), cleanup: previous.cleanup, phaseCount: previous.phases.length, observationCount: previous.observations.length }]
    receipt.priorBuilds = [...(previous.priorBuilds || []), { capturedAt: previous.capturedAt, prebuild: previous.prebuild, binaries: previous.binaries }]
    receipt.methodAmendment = { originalSha256: previous.prebuild['tools/change-stream-qualification.mjs'], currentSha256: loadedDriverSha, reason: refreshEvidence ? 'Expanded complete relevant input manifest, consumer-gated teardown, failure scratch disposition, classified proxy errors and actual production-manager snapshot recovery. Diagnostic-only server SQL log fallback renewed; prior native gates remain separately bound.' : 'Driver parser errors remain awaited; same compiled source inputs and successful binaries are reused.' }
    delete receipt.failure; delete receipt.outcome; delete receipt.integrationFailure; delete receipt.cleanup
  }
  receipt.methodSha256 = loadedDriverSha
  receipt.runtime = { node: process.version, platform: process.platform, arch: process.arch, nodeExecutableSha256: hash(await readFile(process.execPath)) }
  receipt.measurementBoundaries = { pool: 'Before withConn to callback is checkout/admission latency, including any connection setup/bookkeeping; controlled held single lease, not a production wait histogram.', rawWrite: 'Injected raw adapter lifetime is a subset of the controlled framed writer span, not a direct internal deadline timestamp.', healthyObserved: 'Frame consumed after dispatch returns is a delivery upper bound.', budgets: '250ms write/750ms work/250ms cleanup fields are configured bounds; controller tests and native behavior are separate evidence.', sql: 'Logical SQL calls; statement-log fallback counts only observed complete log lines and has logger-resolution limits, not RTTs.' }
  scratch = await makeScratch(join(repo, '.tmp/change-stream-qualification-4c367662'), task)
  scratch.env.HMEM_TEST_SESSION_DIAGNOSTICS = '1'
  await gates(reuseBuild, refreshEvidence, integrationOnly)
  await integration(!process.argv.includes('--probe') && !process.argv.includes('--browser-setup'))
  assert.deepEqual(compiledInputs(await inputs()), compiledInputs(receipt.prebuild), 'Source drift before qualification completion')
  assert.equal(hash(await readFile(new URL(import.meta.url))), loadedDriverSha, 'Driver changed during run')
  receipt.outcome = process.argv.includes('--browser-setup') ? 'browser-setup-passed' : process.argv.includes('--probe') ? 'probe-passed' : 'passed'
  await scratch.clean(); receipt.cleanup.taskScratchRemoved = true
} catch (error) {
  receipt.outcome = 'failed'; receipt.failure = { name: error.name, message: String(error.message).slice(-3000), causes: error.errors?.map(cause => ({ name: cause.name, message: String(cause.message).replace(/Bearer\s+\S+/gi, 'Bearer [redacted]').slice(-1000) })) }
  process.exitCode = 1
  if (scratch && ownedChildren.size === 0 && retainedOwners.length === 0 && (!fixtureStarted || fixtureClosed)) {
    try { await scratch.clean(); receipt.cleanup = { ...receipt.cleanup, taskScratchRemoved: true } } catch (cleanup) { receipt.scratchCleanupFailure = { message: cleanup.message, owner: 'qualification developer', reviewBy: new Date(Date.now() + 900000).toISOString(), disposition: 'Contained unused scratch cleanup must be retried or explicitly handed off before reviewBy; diagnostics expire on resolution/task closure.' } }
  } else if (scratch) receipt.retained = { ...receipt.retained, owner: 'qualification developer', reviewBy: new Date(Date.now() + 900000).toISOString(), disposition: 'Audit retained actual owners at next checkpoint and before reviewBy; dispose diagnostics on resolution/task closure. Active fixture resources are never deleted on a timer.' }
} finally { await save(); console.log(JSON.stringify({ outcome: receipt.outcome, failure: receipt.failure, phases: receipt.phases.length, observations: receipt.observations.length })) }
}
