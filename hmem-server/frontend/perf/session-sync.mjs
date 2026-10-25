import assert from 'node:assert/strict'
import { createHash, randomBytes, randomUUID } from 'node:crypto'
import { once } from 'node:events'
import { access, appendFile, mkdir, open, readFile, readdir, realpath, rm, stat, writeFile } from 'node:fs/promises'
import http from 'node:http'
import net from 'node:net'
import { cpus, freemem, platform, release, totalmem } from 'node:os'
import { dirname, delimiter, isAbsolute, join, relative, resolve } from 'node:path'
import { spawn, execFile } from 'node:child_process'
import { promisify } from 'node:util'
import { Transform } from 'node:stream'
import { pipeline } from 'node:stream/promises'
import { pathToFileURL } from 'node:url'
import { chromium, request as playwrightRequest } from '@playwright/test'
import { startSessionMcp, closeSessionMcps } from '../tests-js/support/session-mcp.js'

export const CONTRACT = Object.freeze({
  schema: 1, task: '6bfb2638-0f93-4169-81cf-cbdef3bcc52d',
  browsers: 3, prefixes: [1048576, 104857600, 1073741824], followers: [1, 4],
  history: 53, latencySamples: 100, warmRuns: 5, coldRuns: 3,
  sustainableFraction: 0.7, sourceRecordBytes: [1024, 65536],
  network: { rttMs: 50, bitsPerSecond: 100000000 },
  budgets: { sourceCommitP95Ms: 2000, commitVisibleP95Ms: 2000, interactionP95Ms: 100,
    progressP95Ms: 250, initialLocalMs: 2000, initialRemoteMs: 3000, controlReplyMs: 1000,
    finiteCalibrationMultiplier: 2, finiteOverheadMs: 5000, unrelatedRegressionFraction: 0.1 },
})
const repo = resolve(import.meta.dirname, '../../..')
const frontend = resolve(import.meta.dirname, '..')
const exec = promisify(execFile)
const delay = ms => new Promise(done => setTimeout(done, ms))
const sha = bytes => createHash('sha256').update(bytes).digest('hex')
const loadedMethodSha256 = sha(await readFile(new URL(import.meta.url)))
const now = () => performance.timeOrigin + performance.now()
const runtimeInputs = Symbol('runtime-only fixture/session bindings')
let runDeadline = Infinity
function checkRunDeadline() { if (now() >= runDeadline) throw new Error('Whole-run 20-minute wall guard reached') }

export async function disposeAll(actions, primaryFailure = null) {
  const failures = []
  for (const [name, action] of actions) {
    try { await action() } catch (error) { failures.push(new Error(name + ': ' + error.message)) }
  }
  if (failures.length) {
    const error = new AggregateError([...(primaryFailure ? [primaryFailure] : []), ...failures], 'Owned resource cleanup failed; scratch retained for sync_developer')
    error.retainScratch = true
    throw error
  }
}
export async function rejectFailedSetup(error, close) {
  await disposeAll([['failed setup stack', close]], error)
  throw error
}
export async function cleanupRun(stack, scratch, failure = null) {
  try {
    await disposeAll([['native stack', async () => { if (stack) await bounded(stack.close(), 90000, 'stack cleanup') }]], failure)
    if (!failure?.retainScratch) await scratch.clean()
  } catch (error) { error.retainScratch = true; return error }
  return failure
}
export async function extractApiResponse(response, label) {
  try {
    if (!response.ok()) {
      let code = 'unavailable'
      try { const value = await response.json(); code = value.code || value.error?.code || (typeof value.error === 'string' ? value.error.slice(0, 160) : Object.keys(value).join(',')) } catch {}
      throw new Error(label + ': ' + response.status() + ' ' + code)
    }
    return response.status() === 204 ? null : await response.json()
  } finally { await response.dispose() }
}

export function percentile(values, p = 0.95) {
  if (!values.length) return null
  if (!(p > 0 && p <= 1) || values.some(value => !Number.isFinite(value))) throw new Error('Invalid percentile input')
  const sorted = [...values].sort((a, b) => a - b)
  return sorted[Math.ceil(sorted.length * p) - 1]
}
export function summary(values) {
  return { samples: values.length, min: values.length ? Math.min(...values) : null,
    median: percentile(values, 0.5), p95: percentile(values), max: values.length ? Math.max(...values) : null }
}
export function finiteTargets(calibrations) {
  return Object.fromEntries(Object.entries(calibrations).map(([key, milliseconds]) => {
    if (!Number.isFinite(milliseconds) || milliseconds < 0) throw new Error('Invalid calibration')
    return [key, 2 * milliseconds + 5000]
  }))
}
export function ownedPath(root, path) {
  const inside = relative(resolve(root), resolve(path))
  if (!inside || inside.startsWith('..') || isAbsolute(inside)) throw new Error('Path is not an owned descendant')
  return resolve(path)
}
export async function bounded(promise, ms, phase) {
  let timer
  try { return await Promise.race([promise, new Promise((_, reject) => { timer = setTimeout(() => reject(new Error(phase + ' deadline')), ms) })]) }
  finally { clearTimeout(timer) }
}
async function until(predicate, ms, phase) {
  const end = now() + ms
  do { const result = await predicate(); if (result) return result; await delay(25) } while (now() < end)
  throw new Error(phase + ' deadline')
}

// Every record is complete UTF-8 JSONL. Random payloads avoid unrealistically
// compressible gigabyte fixtures; generated bytes and their hash stay local.
export function fixtureRecord(bytes, marker = '') {
  const prefix = Buffer.from('{"text":"', 'utf8'), suffix = Buffer.from(' λ ' + marker + '"}\r\n')
  if (bytes < prefix.length + suffix.length) throw new Error('Record is too small')
  const remaining = bytes - prefix.length - suffix.length
  const text = randomBytes(Math.ceil(remaining * 3 / 4)).toString('base64').slice(0, remaining)
  return Buffer.concat([prefix, Buffer.from(text), suffix])
}
export async function writePrefix(path, bytes, recordBytes = 65536) {
  if (!Number.isSafeInteger(bytes) || bytes < 1024 || !Number.isSafeInteger(recordBytes) || recordBytes < 1024 || recordBytes > 1048576) throw new Error('Invalid prefix size')
  const file = await open(path, 'wx'), hash = createHash('sha256')
  let offset = 0, records = 0
  const start = now()
  try {
    while (offset < bytes) {
      checkRunDeadline()
      let length = Math.min(recordBytes, bytes - offset)
      const remainder = bytes - offset - length
      if (remainder > 0 && remainder < 32) length -= 32 - remainder
      const record = fixtureRecord(length)
      let written = 0
      while (written < record.length) { const result = await file.write(record, written, record.length - written); if (!result.bytesWritten) throw new Error('Source write made no progress'); written += result.bytesWritten }
      hash.update(record); offset += record.length; records++
    }
    await file.sync()
  } finally { await file.close() }
  return { bytes: offset, records, sha256: hash.digest('hex'), milliseconds: now() - start, maxWorkingBufferBytes: recordBytes }
}
export async function scanPrefix(path, bytes) {
  const file = await open(path, 'r'), hash = createHash('sha256'), buffer = Buffer.alloc(1048576)
  let offset = 0
  const start = now()
  try {
    while (offset < bytes) {
      checkRunDeadline()
      const { bytesRead } = await file.read(buffer, 0, Math.min(buffer.length, bytes - offset), offset)
      if (!bytesRead) throw new Error('Prefix truncated during calibration')
      hash.update(buffer.subarray(0, bytesRead)); offset += bytesRead
    }
  } finally { await file.close() }
  return { bytes: offset, sha256: hash.digest('hex'), milliseconds: now() - start, maxWorkingBufferBytes: buffer.length }
}

// Bounded stream shaping: one RTT per HTTP exchange and a per-direction
// bandwidth ceiling. Chunk pacing does not add another RTT to every chunk.
export class Bandwidth extends Transform {
  constructor(bitsPerSecond = Infinity) { super({ highWaterMark: 65536 }); this.bytesPerMs = bitsPerSecond / 8000; this.next = performance.now(); this.timer = null }
  _transform(chunk, _encoding, done) {
    const start = Math.max(this.next, performance.now())
    this.next = start + chunk.length / this.bytesPerMs
    const wait = Math.max(0, this.next - performance.now())
    this.timer = setTimeout(() => { this.timer = null; this.push(chunk); done() }, wait)
  }
  _destroy(error, done) { clearTimeout(this.timer); done(error) }
}
// One pair of these links is shared by all HTTP and WebSocket streams. A
// stream can queue at most 256 KiB before its producer receives backpressure.
export class SharedLink {
  constructor(bitsPerSecond = Infinity, latencyMs = 0) { this.setPolicy(bitsPerSecond, latencyMs); this.next = performance.now(); this.timers = new Set(); this.bytes = 0; this.peakQueuedBytes = 0; this.queuedBytes = 0 }
  setPolicy(bitsPerSecond, latencyMs) { this.bytesPerMs = bitsPerSecond / 8000; this.latencyMs = latencyMs }
  stream({ latency = true } = {}) {
    const link = this
    return new class extends Transform {
      constructor() { super({ highWaterMark: 65536 }); this.queue = []; this.queued = 0; this.ready = true; this.held = null; this.flushed = null }
      _transform(chunk, _encoding, done) {
        this.queued += chunk.length; link.queuedBytes += chunk.length; link.peakQueuedBytes = Math.max(link.peakQueuedBytes, link.queuedBytes)
        const start = Math.max(link.next, performance.now()); link.next = start + chunk.length / link.bytesPerMs
        const packet = { chunk, eligible: false, timer: null }; this.queue.push(packet)
        packet.timer = setTimeout(() => { link.timers.delete(packet.timer); packet.eligible = true; this.drainReady() }, Math.max(0, link.next + (latency ? link.latencyMs : 0) - performance.now()))
        link.timers.add(packet.timer)
        if (this.queued >= 262144) this.held = done; else done()
      }
      drainReady() {
        while (this.ready && this.queue[0]?.eligible) {
          const packet = this.queue.shift(); this.queued -= packet.chunk.length; link.queuedBytes -= packet.chunk.length; link.bytes += packet.chunk.length
          this.ready = this.push(packet.chunk)
        }
        if (this.held && this.queued < 131072) { const done = this.held; this.held = null; done() }
        if (this.flushed && !this.queue.length) { const done = this.flushed; this.flushed = null; done() }
      }
      _read(size) { this.ready = true; this.drainReady(); super._read(size) }
      _flush(done) { if (this.queue.length) this.flushed = done; else done() }
      _destroy(error, done) {
        for (const packet of this.queue) { clearTimeout(packet.timer); link.timers.delete(packet.timer); link.queuedBytes -= packet.chunk.length }
        this.queue = []; if (this.held) { const held = this.held; this.held = null; held(error || new Error('Link closed')) } done(error)
      }
    }()
  }
  close() { for (const timer of this.timers) clearTimeout(timer); this.timers.clear() }
}
export class WireEvents {
  constructor(observe) { this.observe = observe; this.buffer = Buffer.alloc(0); this.headers = true; this.fragments = []; this.fragmentBytes = 0; this.unsupported = 0 }
  push(bytes) {
    this.buffer = Buffer.concat([this.buffer, bytes])
    if (this.buffer.length > 2097152) throw new Error('WebSocket measurement frame exceeded bound')
    if (this.headers) {
      const end = this.buffer.indexOf('\r\n\r\n')
      if (end < 0) { if (this.buffer.length > 8192) throw new Error('WebSocket handshake exceeded bound'); return }
      this.buffer = this.buffer.subarray(end + 4); this.headers = false
    }
    while (this.buffer.length >= 2) {
      const first = this.buffer[0], second = this.buffer[1], opcode = first & 15, final = Boolean(first & 128)
      let length = second & 127, header = 2
      if (length === 126) { if (this.buffer.length < 4) return; length = this.buffer.readUInt16BE(2); header = 4 }
      if (length === 127) { if (this.buffer.length < 10) return; const value = this.buffer.readBigUInt64BE(2); if (value > 1048576n) throw new Error('WebSocket metadata payload exceeded bound'); length = Number(value); header = 10 }
      if (second & 128 || first & 112) { this.unsupported++; this.buffer = Buffer.alloc(0); return }
      if (length > 1048576) throw new Error('WebSocket metadata payload exceeded bound')
      if (this.buffer.length < header + length) return
      const payload = this.buffer.subarray(header, header + length); this.buffer = this.buffer.subarray(header + length)
      if (![0, 1].includes(opcode)) continue
      if (opcode === 1) { this.fragments = []; this.fragmentBytes = 0 }
      this.fragments.push(payload); this.fragmentBytes += payload.length
      if (this.fragmentBytes > 1048576) throw new Error('WebSocket fragmented metadata exceeded bound')
      if (final) {
        try {
          const frame = JSON.parse(Buffer.concat(this.fragments, this.fragmentBytes).toString())
          if (frame.schema_version === 1 && frame.type === 'change' && typeof frame.event?.event_id === 'string') this.observe({ eventId: frame.event.event_id, entity: frame.event.entity?.id, entityType: frame.event.entity?.type, at: now() })
        } catch {}
        this.fragments = []; this.fragmentBytes = 0
      }
    }
  }
}
export async function networkProxy(target, initialPolicy = {}, observe = () => {}) {
  const origin = new URL(target), sockets = new Set(), pending = new Set()
  let policy = { rttMs: 0, bitsPerSecond: Infinity, ...initialPolicy }
  const upload = new SharedLink(policy.bitsPerSecond, policy.rttMs / 2), download = new SharedLink(policy.bitsPerSecond, policy.rttMs / 2)
  const agent = new http.Agent({ keepAlive: true, maxSockets: 16, maxFreeSockets: 8 })
  const counts = { downstreamConnections: 0, upstreamHttpConnections: 0, reusedUpstreamHttpRequests: 0, webSocketConnections: 0, successfulFollowerHttp: 0 }, seenUpstream = new WeakSet(), clients = new Map(), slowClients = new Set()
  const clientOf = req => /(?:^|;\s*)session_sync_client=([012])(?:;|$)/.exec(req.headers.cookie || '')?.[1] || 'unlabeled'
  function remember(client, entry) { if (!clients.has(client)) clients.set(client, new Set()); clients.get(client).add(entry); entry.socket.once('close', () => clients.get(client)?.delete(entry)) }
  const server = http.createServer((req, res) => {
    const { rttMs, bitsPerSecond } = policy
    const admitted = now(), path = new URL(req.url, target).pathname
    const client = clientOf(req)
    const timer = setTimeout(() => {
      pending.delete(timer)
      const upstream = http.request({ hostname: origin.hostname, port: origin.port, method: req.method, path: req.url,
        headers: { ...req.headers, host: origin.host, ...(req.headers.origin ? { origin: origin.origin } : {}) }, agent }, response => {
        const responseAt = now()
        let upstreamReadEnd = null, downstreamEnded = false
        response.once('end', () => { upstreamReadEnd = now(); observe({ phase: 'upstream-read-end', method: req.method, path, admitted, responseAt, upstreamReadEnd, status: response.statusCode }) })
        res.once('finish', () => {
          downstreamEnded = true
          try { if (JSON.parse(req.headers['x-hmem-bot-identity'] || '{}').role === 'Synchronization performance' && path.includes('/sessions') && response.statusCode < 400) counts.successfulFollowerHttp++ } catch {}
          observe({ phase: 'downstream-finish', client, method: req.method, path, admitted, responseAt, upstreamReadEnd, completed: now(), status: response.statusCode })
        })
        res.once('close', () => { if (!downstreamEnded) observe({ phase: 'downstream-close', method: req.method, path, admitted, completed: now(), status: response.statusCode }) })
        const responseTimer = setTimeout(() => {
          pending.delete(responseTimer)
          res.writeHead(response.statusCode, { ...response.headers,
            ...(response.headers['access-control-allow-origin'] === origin.origin ? { 'access-control-allow-origin': 'http://127.0.0.1:' + server.address().port } : {}) })
          pipeline(response, download.stream({ latency: false }), res).catch(() => res.destroy())
        }, rttMs / 2)
        pending.add(responseTimer)
      })
      upstream.once('socket', socket => { if (!seenUpstream.has(socket)) { seenUpstream.add(socket); counts.upstreamHttpConnections++ } else counts.reusedUpstreamHttpRequests++ })
      upstream.on('error', () => { if (!res.headersSent) res.writeHead(502); res.end() })
      req.on('aborted', () => upstream.destroy())
      pipeline(req, upload.stream({ latency: false }), upstream).catch(() => upstream.destroy())
    }, rttMs / 2)
    pending.add(timer)
  })
  server.on('connection', socket => { socket.setMaxListeners(20); counts.downstreamConnections++; sockets.add(socket); socket.once('close', () => sockets.delete(socket)) })
  server.on('upgrade', (req, socket, head) => {
    counts.webSocketConnections++
    const client = clientOf(req)
    const upstream = net.connect(Number(origin.port), origin.hostname)
    upstream.setMaxListeners(20) // Two owned pipeline directions plus finite connection bookkeeping.
    remember(client, { socket, upstream, webSocket: true })
    const wire = new WireEvents(event => observe({ phase: 'websocket-egress-observed', client, ...event }))
    upstream.on('data', bytes => { try { wire.push(bytes) } catch { observe({ phase: 'websocket-measurement-overflow', client }) } })
    sockets.add(upstream); upstream.once('close', () => sockets.delete(upstream))
    upstream.once('connect', () => {
      const timer = setTimeout(() => {
        pending.delete(timer)
        upstream.write(req.method + ' ' + req.url + ' HTTP/' + req.httpVersion + '\r\n' + Object.entries({ ...req.headers, host: origin.host, ...(req.headers.origin ? { origin: origin.origin } : {}) }).map(([key, value]) => key + ': ' + value).join('\r\n') + '\r\n\r\n')
        const outward = upload.stream()
        if (head.length) outward.write(head)
        pipeline(socket, outward, upstream).catch(destroy)
        pipeline(upstream, download.stream(), socket).catch(destroy)
        if (slowClients.has(client)) upstream.pause()
      }, policy.rttMs / 2)
      pending.add(timer)
    })
    const destroy = () => { socket.destroy(); upstream.destroy() }
    socket.on('error', destroy); upstream.on('error', destroy); socket.on('close', destroy)
  })
  server.maxConnections = 64
  await new Promise(done => server.listen(0, '127.0.0.1', done))
  return { origin: 'http://127.0.0.1:' + server.address().port,
    setPolicy(value) { policy = { rttMs: 0, bitsPerSecond: Infinity, ...value }; upload.setPolicy(policy.bitsPerSecond, policy.rttMs / 2); download.setPolicy(policy.bitsPerSecond, policy.rttMs / 2) },
    pauseClient(client, paused) { if (paused) slowClients.add(client); else slowClients.delete(client); for (const entry of clients.get(client) || []) if (entry.webSocket) paused ? entry.upstream.pause() : entry.upstream.resume() },
    disconnectClient(client) { for (const entry of clients.get(client) || []) { entry.socket.destroy(); entry.upstream.destroy() } },
    metrics() { return { ...counts, upload: { bytes: upload.bytes, peakQueuedBytes: upload.peakQueuedBytes }, download: { bytes: download.bytes, peakQueuedBytes: download.peakQueuedBytes }, upstreamReuse: 'shared keep-alive agent; upstream connections are proxy-owned', helperStarts: 'successful follower HTTP mapping is inferred one current helper per successful request; pre-network failures are unobserved' } },
    async close() { for (const timer of pending) clearTimeout(timer); for (const socket of sockets) socket.destroy(); agent.destroy(); upload.close(); download.close(); await bounded(new Promise(done => server.close(done)), 5000, 'proxy cleanup') } }
}

export async function makeScratch(path) {
  const root = resolve(path)
  ownedPath(join(repo, '.scratch'), root)
  await mkdir(dirname(root), { recursive: true })
  await mkdir(root) // Never adopt a preexisting directory.
  await writeFile(join(root, 'owner.json'), JSON.stringify({ task: CONTRACT.task, owner: 'sync_developer', pid: process.pid, created: new Date().toISOString() }))
  await mkdir(join(root, 'tmp'))
  return { root, env: { ...process.env, TEMP: join(root, 'tmp'), TMP: join(root, 'tmp'), TMPDIR: join(root, 'tmp'), HMEM_TEST_POSTGRES_BACKEND: 'native' },
    async clean() {
      const owner = JSON.parse(await readFile(join(root, 'owner.json'), 'utf8'))
      if (owner.task !== CONTRACT.task || owner.pid !== process.pid) throw new Error('Scratch ownership changed')
      ownedPath(join(repo, '.scratch'), root)
      await rm(root, { recursive: true, force: false, maxRetries: 3, retryDelay: 200 })
      await assert.rejects(access(root), error => error.code === 'ENOENT')
    } }
}

function runChild(command, args, options = {}) {
  const child = spawn(command, args, { windowsHide: true, stdio: ['pipe', 'pipe', 'pipe'], ...options })
  const closed = once(child, 'close'), diagnostics = { tail: '' }
  child.stderr.on('data', bytes => { diagnostics.tail = (diagnostics.tail + bytes.toString()).slice(-2048) })
  return { child, closed, diagnostics }
}
async function checkedExec(command, args, options = {}) {
  checkRunDeadline()
  return exec(command, args, { windowsHide: true, maxBuffer: 1048576, ...options, timeout: Math.min(options.timeout || 30000, Math.max(1, runDeadline - now())) })
}

// Kept separate from correctness fixtures: this owns its entire sandbox and
// returns only non-secret coordinates. The bearer stays in this closure.
export async function startStack(scratch, diagnostics = true) {
  const { stdout } = await checkedExec('stack', ['path', '--local-install-root'], { cwd: repo, env: scratch.env })
  const bin = join(stdout.trim(), 'bin'), extension = process.platform === 'win32' ? '.exe' : ''
  const executable = join(bin, 'hmem-test-harness' + extension), mcpExecutable = join(bin, 'hmem-mcp' + extension)
  const provenance = { harness: sha(await readFile(executable)), mcp: sha(await readFile(mcpExecutable)), sources: {
    runner: loadedMethodSha256, sessionMcpHelper: sha(await readFile(join(frontend, 'tests-js/support/session-mcp.js'))), nativeHarness: sha(await readFile(join(repo, 'hmem-core/src/HMem/DB/TestHarness.hs'))) } }
  const probe = net.createServer(); await new Promise(done => probe.listen(0, '127.0.0.1', done)); const port = probe.address().port; await new Promise(done => probe.close(done))
  const managed = runChild(executable, ['deployed', '--interactive', '--port', String(port)], {
    cwd: repo, env: { ...scratch.env, HMEM_TEST_SESSION_DIAGNOSTICS: diagnostics ? '1' : '0' },
  })
  const coordinates = {}, clients = [], activeMcpPids = new Set()
  let pending = '', token, api, browser, proxy, postgresPid
  managed.child.stdout.on('data', chunk => {
    pending += chunk.toString()
    if (pending.length > 16384) { managed.child.kill(); return }
    let at
    while ((at = pending.indexOf('\n')) >= 0) {
      const line = pending.slice(0, at).trim(); pending = pending.slice(at + 1)
      for (const [label, key] of [['Sandbox root:', 'root'], ['Static dir  :', 'static'], ['Config file :', 'config'], ['Postgres log:', 'pgLog']]) if (line.startsWith(label)) coordinates[key] = line.slice(label.length).trim()
      if (line.startsWith('Bearer PAT:')) token = line.slice('Bearer PAT:'.length).trim()
      if (line.startsWith('Press Enter')) coordinates.ready = true
    }
  })
  async function close() {
    const failures = []
    async function finish(action) { try { await action() } catch (error) { failures.push(error.message) } }
    await finish(async () => { if (browser) await bounded(browser.close(), 15000, 'browser cleanup') })
    await finish(async () => { if (api) await bounded(api.dispose(), 5000, 'HTTP context cleanup') })
    await finish(() => closeSessionMcps(clients))
    await finish(async () => { if (proxy) await proxy.close() })
    await finish(async () => { if (managed.child.exitCode === null) managed.child.stdin.end('\n'); await bounded(managed.closed, 30000, 'sandbox cleanup') })
    await finish(async () => { if (coordinates.root) await assert.rejects(access(coordinates.root), error => error.code === 'ENOENT') })
    await finish(async () => { if (postgresPid) assert.throws(() => process.kill(postgresPid, 0), error => error.code === 'ESRCH') })
    if (failures.length) throw new Error('Cleanup failed: ' + failures.join('; '))
  }
  try {
    await until(() => coordinates.ready || managed.child.exitCode !== null, 120000, 'sandbox startup')
    assert.equal(managed.child.exitCode, null, 'native sandbox startup')
    assert.ok(token, 'private seeded bearer')
    ownedPath(join(scratch.root, 'tmp'), await realpath(coordinates.root))
    postgresPid = Number((await readFile(join(coordinates.root, 'postgres/data/postmaster.pid'), 'utf8')).split('\n')[0])
    const config = JSON.parse(await readFile(coordinates.config, 'utf8'))
    let db = config.database
    if (!db?.port || !db?.name?.startsWith('hmem_test_')) throw new Error('Unexpected sandbox database')
    const actualOrigin = 'http://127.0.0.1:' + port
    const proxyEvents = []
    proxy = await networkProxy(actualOrigin, {}, event => { if (proxyEvents.length < 20000) proxyEvents.push(event) })
    const origin = proxy.origin
    const buildEnv = { ...scratch.env, VITE_HMEM_API_URL: origin, VITE_HMEM_WS_URL: origin.replace('http:', 'ws:') + '/api/v1/ws', VITE_HMEM_AUTH_MODE: 'deployed' }
    const pathKey = Object.keys(buildEnv).find(key => key.toLowerCase() === 'path') || 'PATH'
    buildEnv[pathKey] = join(frontend, 'node_modules/.bin') + delimiter + (buildEnv[pathKey] || '')
    await checkedExec(process.execPath, ['node_modules/vite/bin/vite.js', 'build', '--outDir', coordinates.static, '--emptyOutDir'], { cwd: frontend, env: buildEnv, timeout: 90000 })
    provenance.assets = Object.fromEntries(await Promise.all((await readdir(join(coordinates.static, 'assets'))).map(async name => [name, sha(await readFile(join(coordinates.static, 'assets', name)))])))
    api = await playwrightRequest.newContext({ baseURL: origin, extraHTTPHeaders: { Authorization: 'Bearer ' + token, 'X-HMem-Bot-Identity': JSON.stringify({ role: 'Synchronization baseline', 'model-id': 'test:production:default' }) } })
    async function call(path, method = 'GET', data) {
      checkRunDeadline()
      const response = await api.fetch(path, { method, data, timeout: Math.min(30000, Math.max(1, runDeadline - now())) })
      return extractApiResponse(response, 'HTTP ' + method + ' ' + path.replace(/[0-9a-f]{8}-[0-9a-f-]{27,}/g, ':id'))
    }
    return { origin, actualOrigin, call, provenance, coordinates: { root: coordinates.root, pgLog: coordinates.pgLog, db }, proxyEvents, setNetwork: value => proxy.setPolicy(value), pauseClient: (id, paused) => proxy.pauseClient(id, paused), disconnectClient: id => proxy.disconnectClient(id), transportMetrics: () => proxy.metrics(), ownedPids: () => [process.pid, managed.child.pid, postgresPid, ...activeMcpPids],
      async mcp(workspace, { role = 'Synchronization performance' } = {}) { const client = await startSessionMcp({ executable: mcpExecutable, origin, token, workspace,
        identity: { role, 'model-id': 'test:production:default' }, env: scratch.env, requestTimeoutMs: 60000 }); clients.push(client); activeMcpPids.add(client.pid)
        const close = client.close; client.close = async options => { try { return await close(options) } finally { activeMcpPids.delete(client.pid) } }; return client },
      async browsers({ fresh = false } = {}) {
        if (fresh && browser) { await browser.close(); browser = null }
        if (!browser) browser = await chromium.launch({ headless: true })
        const pages = []
        for (let i = 0; i < CONTRACT.browsers; i++) {
          const context = await browser.newContext({ viewport: { width: 1440, height: 1000 } })
          await context.addCookies([{ name: 'hmem_session', value: 'sandbox-session-token', url: origin }, { name: 'hmem_csrf', value: 'sandbox-csrf-token', url: origin }, { name: 'session_sync_client', value: String(i), url: origin }])
          await context.addInitScript({ content: 'window.__sessionMarkerVisible = (' + visibleMarkerWitness.toString() + ')' })
          await context.addInitScript(browserObserver)
          pages.push(await context.newPage())
        }
        return { pages, version: browser.version() }
      },
      close,
    }
  } catch (error) { return rejectFailedSetup(error, close) }
}

// Observation wraps native transports, rather than substituting Elm delivery.
export function visibleMarkerWitness(key) {
  const marker = 'sync-' + key.replace(':', '-') + '-'
  for (const element of document.querySelectorAll('.session-record-text')) {
    const walker = document.createTreeWalker(element, NodeFilter.SHOW_TEXT)
    let text
    while ((text = walker.nextNode())) {
      const at = text.textContent.indexOf(marker)
      if (at < 0) continue
      const range = document.createRange(); range.setStart(text, at); range.setEnd(text, at + marker.length)
      let clip = { left: 0, top: 0, right: innerWidth, bottom: innerHeight }, visible = true
      for (let ancestor = element; ancestor; ancestor = ancestor.parentElement) {
        const style = getComputedStyle(ancestor), bounds = ancestor.getBoundingClientRect()
        if (style.visibility !== 'visible' || style.display === 'none' || Number(style.opacity) === 0) { visible = false; break }
        const left = bounds.left + ancestor.clientLeft, top = bounds.top + ancestor.clientTop
        if (/(auto|scroll|hidden|clip)/.test(style.overflowX)) { clip.left = Math.max(clip.left, left); clip.right = Math.min(clip.right, left + ancestor.clientWidth) }
        if (/(auto|scroll|hidden|clip)/.test(style.overflowY)) { clip.top = Math.max(clip.top, top); clip.bottom = Math.min(clip.bottom, top + ancestor.clientHeight) }
      }
      if (visible) for (const rect of range.getClientRects()) {
        if (rect.width > 0 && rect.height > 0 && Math.min(rect.right, clip.right) > Math.max(rect.left, clip.left) && Math.min(rect.bottom, clip.bottom) > Math.max(rect.top, clip.top)) return { at: performance.timeOrigin + performance.now(), clip, markerRect: { left: rect.left, top: rect.top, right: rect.right, bottom: rect.bottom } }
      }
    }
  }
  return null
}
export async function renderedWitness(page, key) {
  if (!await page.evaluate(visibleMarkerWitness, key)) return null
  await page.screenshot({ type: 'jpeg', quality: 30, timeout: 5000 })
  if (!await page.evaluate(visibleMarkerWitness, key)) return null
  return now() // Conservative upper bound only when the marker survives both clip checks.
}
export function browserObserver() {
  const state = window.__sessionSync = { domReady: {}, sockets: [], http: [], longTasks: [], peakSessionHTTP: 0, activeSessionHTTP: 0 }
  const stamp = () => performance.timeOrigin + performance.now()
  const NativeXHR = window.XMLHttpRequest
  window.XMLHttpRequest = class extends NativeXHR {
    constructor() {
      super()
      const finish = () => {
        if (!this.syncActive) return
        this.syncActive = false; state.activeSessionHTTP--
        if (state.http.length < 20000) state.http.push({ path: this.syncPath, start: this.syncAt, end: stamp(), status: this.status })
      }
      for (const name of ['load', 'error', 'timeout', 'abort']) this.addEventListener(name, finish)
    }
    open(method, url, ...rest) { this.syncPath = new URL(url, location.href).pathname; return super.open(method, url, ...rest) }
    send(...args) { if (this.syncPath.includes('/sessions')) { this.syncActive = true; this.syncAt = stamp(); state.activeSessionHTTP++; state.peakSessionHTTP = Math.max(state.peakSessionHTTP, state.activeSessionHTTP) } return super.send(...args) }
  }
  const NativeWS = window.WebSocket
  window.WebSocket = class extends NativeWS {
    constructor(...args) { super(...args); this.addEventListener('message', event => { if (state.sockets.length < 20000) { try { const value = JSON.parse(event.data); state.sockets.push({ at: stamp(), type: value.type, eventId: value.event?.event_id, entity: value.event?.entity?.id, entityType: value.event?.entity?.type }) } catch {} } }) }
  }
  try { new PerformanceObserver(list => { for (const item of list.getEntries()) if (state.longTasks.length < 2000) state.longTasks.push(item.duration) }).observe({ type: 'longtask', buffered: true }) } catch {}
  const collect = () => {
    for (const node of document.querySelectorAll('.session-record-text')) {
      for (const match of node.textContent.matchAll(/sync-([0-9a-f]{8})-(\d+)-/g)) {
        const key = match[1] + ':' + match[2]
        if (state.domReady[key] === undefined) state.domReady[key] = stamp()
      }
    }
  }
  document.addEventListener('DOMContentLoaded', () => new MutationObserver(() => requestAnimationFrame(collect)).observe(document.body, { childList: true, subtree: true, characterData: true }))
}

export class DatabaseObserver {
  constructor(db, env, logPath = null) {
    if (!db.name.startsWith('hmem_test_') || !Number.isInteger(db.port)) throw new Error('Refusing non-sandbox database')
    this.managed = runChild('psql', ['-X', '-A', '-t', '-q', '-v', 'ON_ERROR_STOP=1', '-h', '127.0.0.1', '-p', String(db.port), '-d', db.name], { env: { ...env, PGAPPNAME: 'session-sync-observer' } })
    this.buffer = ''; this.current = null; this.chain = Promise.resolve(); this.sequence = 0
    this.logPath = logPath; this.logOffset = 0; this.logTail = ''; this.statementCalls = 0; this.maxLogBytes = 134217728
    this.managed.child.stdout.on('data', bytes => {
      this.buffer += bytes.toString()
      if (this.buffer.length > 8 * 1024 * 1024) { this.current?.reject(new Error('DB observation frame exceeded bound')); this.managed.child.kill(); return }
      let at
      while ((at = this.buffer.indexOf('\n')) >= 0) {
        const line = this.buffer.slice(0, at).trim(); this.buffer = this.buffer.slice(at + 1)
        if (!this.current) continue
        if (line === this.current.marker) { const current = this.current; this.current = null; current.resolve(current.lines); continue }
        if (line) this.current.lines.push(line)
      }
    })
    this.managed.closed.then(() => { if (this.current) { this.current.reject(new Error('DB observer exited')); this.current = null } }, () => {})
  }
  raw(sql) {
    checkRunDeadline()
    const operation = this.chain.then(() => { checkRunDeadline(); return bounded(new Promise((resolve, reject) => {
      const marker = '__session_sync_' + (++this.sequence)
      this.current = { marker, lines: [], resolve, reject }
      this.managed.child.stdin.write('/*session-sync-observer*/ ' + sql + ';\n\\echo ' + marker + '\n')
    }), Math.min(30000, Math.max(1, runDeadline - now())), 'DB observation') })
    this.chain = operation.catch(() => {})
    return operation
  }
  async query(sql) { const lines = await this.raw(sql); return lines.length ? JSON.parse(lines.at(-1)) : null }
  async initialize() {
    const available = await this.query("SELECT json_build_object('commit_timestamps',current_setting('track_commit_timestamp'),'preload',current_setting('shared_preload_libraries'),'available',EXISTS(SELECT 1 FROM pg_available_extensions WHERE name='pg_stat_statements'),'version',version())")
    assert.equal(available.commit_timestamps, 'on', 'direct commit timestamps configured')
    this.statements = available.preload.split(',').map(value => value.trim()).includes('pg_stat_statements') && available.available
    if (this.statements) await this.raw('CREATE EXTENSION IF NOT EXISTS pg_stat_statements')
    return { commit: 'pg_xact_commit_timestamp(session_chunks.xmin)', statements: this.statements ? 'pg_stat_statements.calls (SQL executions, not TCP packet count)' : 'parameter-suppressed sandbox log statement count', locks: 'sampled pg_stat_activity/pg_locks; wait-time estimate is sampling resolution limited', postgres: available.version }
  }
  async stats() {
    const activity = await this.query("SELECT json_build_object('lock_waiters',count(*) FILTER(WHERE wait_event_type='Lock'),'active',count(*) FILTER(WHERE state='active'),'connections',count(*)) FROM pg_stat_activity WHERE application_name<>'session-sync-observer' AND datname=current_database()")
    const statements = this.statements ? await this.query("SELECT json_build_object('calls',coalesce(sum(calls),0),'execution_ms',coalesce(sum(total_exec_time),0),'rows',coalesce(sum(rows),0),'blocks_read',coalesce(sum(shared_blks_read),0),'blocks_hit',coalesce(sum(shared_blks_hit),0)) FROM pg_stat_statements WHERE dbid=(SELECT oid FROM pg_database WHERE datname=current_database()) AND query NOT LIKE '/*session-sync-observer*/%'") : await this.logStats()
    return { at: now(), activity, statements }
  }
  async logStats() {
    if (!this.logPath) return null
    const size = (await stat(this.logPath)).size
    if (size > this.maxLogBytes) throw new Error('Task-owned PostgreSQL diagnostic log exceeded 128 MiB bound')
    const file = await open(this.logPath, 'r'), buffer = Buffer.alloc(1048576)
    try {
      while (this.logOffset < size) {
        const { bytesRead } = await file.read(buffer, 0, Math.min(buffer.length, size - this.logOffset), this.logOffset)
        if (!bytesRead) break
        this.logOffset += bytesRead; const lines = (this.logTail + buffer.subarray(0, bytesRead).toString()).split('\n'); this.logTail = lines.pop()
        if (this.logTail.length > 65536) throw new Error('Unexpected oversized diagnostic statement line')
        for (const line of lines) if (/LOG:\s+(?:execute .*?:|statement:)/.test(line) && !line.includes('/*session-sync-observer*/')) this.statementCalls++
      }
    } finally { await file.close() }
    return { calls: this.statementCalls, logBytes: size, method: 'parameter-suppressed-log', physicalRoundTrips: null }
  }
  async commits(session, { after = 0, limit = 256 } = {}) {
    if (!/^[0-9a-f-]{36}$/.test(session)) throw new Error('Invalid session ID')
    if (!Number.isSafeInteger(after) || after < 0 || !Number.isInteger(limit) || limit < 1 || limit > 512) throw new Error('Invalid bounded commit range')
    return this.query("SELECT coalesce(json_agg(json_build_object('offset',start_offset,'end',end_offset,'revision',revision,'commit_ms',commit_ms,'transaction',transaction_id) ORDER BY start_offset),'[]'::json) FROM (SELECT start_offset,start_offset+octet_length(data) AS end_offset,revision,extract(epoch FROM pg_xact_commit_timestamp(xmin))*1000 AS commit_ms,xmin::text AS transaction_id FROM session_chunks WHERE session_id='" + session + "' AND start_offset>=" + after + ' ORDER BY start_offset LIMIT ' + limit + ') bounded_commits')
  }
  async outbox(workspace, cursor = 0) {
    if (!/^[0-9a-f-]{36}$/.test(workspace) || !Number.isSafeInteger(cursor) || cursor < 0) throw new Error('Invalid outbox measurement scope')
    return this.query("SELECT coalesce(json_agg(row_to_json(bounded_events) ORDER BY cursor),'[]'::json) FROM (SELECT event_id::text AS event_id,cursor,xmin::text AS transaction,extract(epoch FROM pg_xact_commit_timestamp(xmin))*1000 AS commit_ms,envelope->'entity'->>'id' AS entity FROM change_stream_outbox WHERE scope='workspace' AND workspace_id='" + workspace + "' AND cursor>" + cursor + ' ORDER BY cursor LIMIT 256) bounded_events')
  }
  async counts(workspace) {
    if (!/^[0-9a-f-]{36}$/.test(workspace)) throw new Error('Invalid measured workspace')
    return this.query("SELECT json_build_object('active',count(*) FILTER(WHERE state='active'),'terminated',count(*) FILTER(WHERE state='terminated')) FROM sessions WHERE workspace_id='" + workspace + "'")
  }
  async close() { this.managed.child.stdin.end('\\q\n'); await bounded(this.managed.closed, 5000, 'DB observer cleanup') }
  async scan(session, bytes) {
    if (!/^[0-9a-f-]{36}$/.test(session) || !Number.isSafeInteger(bytes) || bytes < 0) throw new Error('Invalid bounded scan')
    const hash = createHash('sha256'), start = now()
    let offset = 0, queries = 0, peakBytes = 0
    while (offset < bytes) {
      checkRunDeadline()
      const rows = await this.query("SELECT coalesce(json_agg(json_build_object('offset',start_offset,'data',encode(data,'base64')) ORDER BY start_offset),'[]'::json) FROM (SELECT start_offset,data FROM session_chunks WHERE session_id='" + session + "' AND start_offset>=" + offset + ' AND start_offset<' + Math.min(bytes, offset + 1048576) + ' ORDER BY start_offset LIMIT 2048) bounded_prefix')
      queries++
      let batchBytes = 0
      if (!rows.length) throw new Error('Reference DB prefix has a gap')
      for (const row of rows) {
        if (row.offset !== offset) throw new Error('Reference DB prefix is not contiguous')
        const data = Buffer.from(row.data, 'base64')
        if (offset + data.length > bytes) throw new Error('Reference target is not an append boundary')
        hash.update(data); offset += data.length; batchBytes += data.length
      }
      peakBytes = Math.max(peakBytes, batchBytes)
      if (batchBytes > 2097152) throw new Error('Reference DB window exceeded bound')
    }
    return { bytes: offset, sha256: hash.digest('hex'), milliseconds: now() - start, queries, maxRawBatchBytes: peakBytes,
      scope: 'bounded independent DB scan over actual immutable chunks; no production follower pacing' }
  }
}

async function seedSession(stack, workspace, task, path, bytes, { id = randomUUID(), chunkBytes = 1048576 } = {}) {
  const base = '/api/v1/workspaces/' + workspace + '/sessions'
  let session = await stack.call(base, 'POST', { id, task_id: task, title: 'Synchronization baseline', format_label: 'Bounded benchmark JSONL' })
  const file = await open(path, 'r'), buffer = Buffer.alloc(chunkBytes)
  let offset = 0, requests = 0
  const start = now()
  try {
    while (offset < bytes) {
      const { bytesRead } = await file.read(buffer, 0, Math.min(chunkBytes, bytes - offset), offset)
      if (!bytesRead) throw new Error('Seed source truncated')
      const result = await stack.call(base + '/' + id + '/append', 'POST', { expected_offset: session.committed_bytes, expected_revision: session.revision, data_base64: buffer.subarray(0, bytesRead).toString('base64') })
      session = result.session; offset += bytesRead; requests++
    }
  } finally { await file.close() }
  return { session, requests, bytes: offset, milliseconds: now() - start }
}

async function environmentFingerprint() {
  const values = await Promise.all(['ghc', 'psql'].map(async command => { try { return [command, (await checkedExec(command, ['--version'])).stdout.trim()] } catch { return [command, 'unavailable'] } }))
  return { platform: platform(), release: release(), arch: process.arch, node: process.version,
    cpu: cpus()[0]?.model, logicalCpus: cpus().length, totalMemoryBytes: totalmem(), freeMemoryBytes: freemem(), versions: Object.fromEntries(values) }
}

async function probeStack(stack, scratch) {
  const observer = new DatabaseObserver(stack.coordinates.db, scratch.env, stack.coordinates.pgLog)
  try {
    const measurement = await observer.initialize(), initial = await observer.stats()
    const workspace = await stack.call('/api/v1/workspaces', 'POST', { name: 'Synchronization baseline', workspace_type: 'repository' })
    const task = await stack.call('/api/v1/tasks', 'POST', { workspace_id: workspace.id, title: 'Measure synchronization' })
    await stack.call('/api/v1/tasks/' + task.id, 'PUT', { status: 'in_progress' })
    const source = join(scratch.root, 'probe.jsonl'), fixture = await writePrefix(source, 1048576)
    const seed = await seedSession(stack, workspace.id, task.id, source, fixture.bytes)
    const commits = await observer.commits(seed.session.id)
    assert.equal(commits.length, 1)
    assert.ok(Number.isFinite(commits[0].commit_ms))
    const client = await stack.mcp(workspace.id), start = now()
    await client.tool('session_upload_start', { session_id: seed.session.id, path: source, task_id: task.id, title: 'Synchronization baseline', format_label: 'Bounded benchmark JSONL' })
    const state = await until(async () => { const value = await client.tool('session_upload_status', { session_id: seed.session.id }); if (value.status === 'failed') throw new Error('Production follower failed: ' + value.error); return value.status === 'caught_up' ? value : null }, 30000, 'production follower resume')
    const resumedMs = now() - start
    const { pages, version } = await stack.browsers()
    const initialWindows = [], initialViewStates = []
    for (const page of pages) {
      const errors = []; page.on('pageerror', error => errors.push(error.message))
      const at = now(); await page.goto(stack.origin + '/workspace/' + workspace.id + '#tab=sessions&session=' + seed.session.id)
      await page.getByRole('heading', { name: 'Sessions', exact: true }).waitFor()
      await page.locator('.session-record-text').first().waitFor()
      initialWindows.push(now() - at)
      // A user must initially view the selected transcript. This setup action
      // happens before source growth; no later marker is scrolled into view.
      await page.locator('.session-records').scrollIntoViewIfNeeded()
      await page.locator('.session-records').press('End')
      initialViewStates.push(await page.locator('.session-records').evaluate(element => { const rect = element.getBoundingClientRect(); return { top: rect.top, bottom: rect.bottom, windowHeight: innerHeight, outerScroll: scrollY, scrollTop: element.scrollTop, scrollHeight: element.scrollHeight, clientHeight: element.clientHeight } }))
      assert.deepEqual(errors, [])
    }
    const sourceAt = now(), marker = 'sync-' + seed.session.id.slice(0, 8) + '-0-'
    await appendFile(source, fixtureRecord(1024, marker))
    await until(async () => { const value = await client.tool('session_upload_status', { session_id: seed.session.id }); if (value.status === 'failed') throw new Error('Production append failed'); return value.committed_offset === fixture.bytes + 1024 }, 15000, 'production append')
    const commit = (await observer.commits(seed.session.id, { after: fixture.bytes })).at(-1)
    const visible = await Promise.all(pages.map(async page => {
      try { return { complete: true, at: await until(() => renderedWitness(page, seed.session.id.slice(0, 8) + ':0'), 5000, 'visible clipped marker raster') } }
      catch (error) {
        if (error.message !== 'visible clipped marker raster deadline') throw error
        return { complete: false, censoredAtMs: 5000, reason: 'marker_not_visible_in_viewport_and_scroll_clips',
          geometry: await page.evaluate(key => {
            const marker = 'sync-' + key.replace(':', '-') + '-', record = [...document.querySelectorAll('.session-record-text')].find(element => element.textContent.includes(marker)), viewport = document.querySelector('.session-records'), rect = element => { const r = element?.getBoundingClientRect(); return r ? { top: r.top, bottom: r.bottom, left: r.left, right: r.right } : null }
            return { domPresent: Boolean(record), domReadyAt: window.__sessionSync.domReady[key] || null, windowHeight: innerHeight, windowScroll: scrollY, transcript: rect(viewport), record: rect(record), scrollTop: viewport?.scrollTop, scrollHeight: viewport?.scrollHeight, clientHeight: viewport?.clientHeight }
          }, seed.session.id.slice(0, 8) + ':0') }
      }
    }))
    const final = await observer.stats()
    const result = { measurement, initialStats: initial, finalStats: final, seed: { bytes: seed.bytes, requests: seed.requests, milliseconds: seed.milliseconds },
      resumedMs, follower: { state: state.status, committedBytes: state.committed_offset }, chromium: version, initialWindows, initialViewStates, setupViewAction: 'Once before source growth: view transcript and native End key; no per-marker scrolling',
      direct: { sourceToCommitMs: commit.commit_ms - sourceAt, render: visible.map(value => value.complete ? { complete: true, milliseconds: value.at - commit.commit_ms } : value) },
      browser: await Promise.all(pages.map(page => page.evaluate(() => ({ http: window.__sessionSync.http.length, peak: window.__sessionSync.peakSessionHTTP, longTasks: window.__sessionSync.longTasks })))),
      limitations: ['Probe is a measurement-validity check, not a calibrated profile or qualification.', 'SQL execution counts are not claimed to be physical TCP round trips.'] }
    await client.tool('session_upload_stop', { session_id: seed.session.id })
    return result
  } finally { await observer.close() }
}

async function makeWork(stack, title) {
  const workspace = await stack.call('/api/v1/workspaces', 'POST', { name: title, workspace_type: 'repository' })
  const task = await stack.call('/api/v1/tasks', 'POST', { workspace_id: workspace.id, title })
  await stack.call('/api/v1/tasks/' + task.id, 'PUT', { status: 'in_progress' })
  return { workspace: workspace.id, task: task.id }
}
async function createSession(stack, work) {
  return stack.call('/api/v1/workspaces/' + work.workspace + '/sessions', 'POST', { id: randomUUID(), task_id: work.task, title: 'Synchronization baseline', format_label: 'Bounded benchmark JSONL' })
}
async function referenceTrial(stack, work, rate, durationMs = 5000) {
  const record = fixtureRecord(65536), session = await createSession(stack, work), start = now(), period = record.length * 1000 / rate
  let current = session, bytes = 0, count = 0, maximumBacklog = 0
  const latencies = [], lateness = []
  while ((count + 1) * period <= durationMs) {
    const planned = start + (count + 1) * period
    await delay(Math.max(0, planned - now()))
    if (now() - start > durationMs) break
    const at = now(); lateness.push(Math.max(0, at - planned))
    const result = await stack.call('/api/v1/workspaces/' + work.workspace + '/sessions/' + session.id + '/append', 'POST', { expected_offset: current.committed_bytes, expected_revision: current.revision, data_base64: record.toString('base64') })
    current = result.session; latencies.push(now() - at); count++; bytes += record.length
    maximumBacklog = Math.max(maximumBacklog, Math.max(0, (now() - start) * rate / 1000 - bytes))
  }
  const elapsed = now() - start, offeredRecords = Math.floor(durationMs / period), missedRecords = offeredRecords - count
  return { targetBytesPerSecond: rate, bytes, requests: count, durationMs: elapsed, offeredRecords, missedRecords,
    completedBytesPerSecond: bytes * 1000 / elapsed, maximumVirtualBacklogBytes: maximumBacklog,
    stable: count >= offeredRecords * 0.95 && (lateness.at(-1) || 0) - (lateness[0] || 0) <= period,
    requestLatencyMs: summary(latencies), admissionLatenessMs: summary(lateness),
    method: 'fixed five-second offered 64 KiB-record schedule; serial authorized HTTP; virtual offered backlog is not an uploaded file' }
}
async function coldScan(path, bytes, env) {
  const script = "const fs=require('fs'),crypto=require('crypto');const start=performance.now(),hash=crypto.createHash('sha256');let read=0;const source=fs.createReadStream(process.argv[1],{highWaterMark:1048576,start:0,end:Number(process.argv[2])-1});source.on('data',chunk=>{read+=chunk.length;hash.update(chunk)});source.on('end',()=>console.log(JSON.stringify({bytes:read,sha256:hash.digest('hex'),milliseconds:performance.now()-start})));source.on('error',()=>process.exit(1));"
  const result = await checkedExec(process.execPath, ['-e', script, path, String(bytes)], { env, timeout: 120000 })
  return JSON.parse(result.stdout)
}
async function resourceSnapshot(pids, descendants = false) {
  if (process.platform !== 'win32') return { method: 'supervisor-only', rssBytes: process.memoryUsage().rss, cpuSeconds: process.cpuUsage().user / 1000000 }
  const known = [...new Set(pids)].filter(value => Number.isInteger(value) && value > 0)
  const tree = descendants ? '$measurementLinks=@(Get-CimInstance Win32_Process|Select-Object ProcessId,ParentProcessId); for($measurementDepth=0;$measurementDepth -lt 32;$measurementDepth++){ $measurementNext=@($measurementLinks|Where-Object{[int]$_.ParentProcessId -in $measurementIds -and [int]$_.ProcessId -notin $measurementIds}|ForEach-Object{[int]$_.ProcessId}); if(!$measurementNext.Count){break}; $measurementIds+= $measurementNext; if($measurementIds.Count -gt 256){throw "Owned process tree exceeds measurement bound"}}; $measurementIds=@($measurementIds|Where-Object{$_ -ne $PID}); ' : ''
  const command = '$measurementIds=@(' + known.join(',') + '); ' + tree + '$measurementProcesses=@(Get-Process -Id $measurementIds -ErrorAction SilentlyContinue); [pscustomobject]@{rssBytes=($measurementProcesses|Measure-Object WorkingSet64 -Sum).Sum;rows=@($measurementProcesses|ForEach-Object{[pscustomobject]@{pid=$_.Id;rssBytes=$_.WorkingSet64;cpuSeconds=$_.CPU}})}|ConvertTo-Json -Depth 4 -Compress'
  const result = await checkedExec('powershell', ['-NoProfile', '-NonInteractive', '-Command', command], { timeout: 10000 })
  return { method: descendants ? 'sampled owned process tree including Chromium descendants and PostgreSQL workers; sampler PowerShell excluded; short-lived helpers may be missed' : 'sampled working set/CPU of explicitly owned stable PIDs; excludes unobserved short-lived helpers', ...JSON.parse(result.stdout) }
}
function startResourceSampler(pids, { descendants = false } = {}) {
  const samples = [], failures = [], cpu = new Map()
  let stopped = false
  const running = (async () => { while (!stopped) { try { if (samples.length < 2048) {
    const value = { at: now(), ...await resourceSnapshot(typeof pids === 'function' ? await pids() : pids, descendants) }
    for (const row of value.rows || []) { const prior = cpu.get(row.pid); cpu.set(row.pid, { first: prior?.first ?? (samples.length ? 0 : row.cpuSeconds), last: Math.max(prior?.last || 0, row.cpuSeconds || 0) }) }
    samples.push(value)
  } } catch { failures.push('resource sample unavailable') } if (!stopped) await delay(500) } })()
  return { async stop() { stopped = true; await bounded(running, 65000, 'resource observer cleanup'); return { samples: samples.length, peakRssBytes: samples.length ? Math.max(...samples.map(value => value.rssBytes || 0)) : null,
    cpuSeconds: cpu.size ? [...cpu.values()].reduce((total, value) => total + value.last - value.first, 0) : null, samplingWaitMs: 500,
    actualIntervalsMs: summary(samples.slice(1).map((value, index) => value.at - samples[index].at)), peakObservedPids: Math.max(0, ...samples.map(value => value.rows?.length || 0)), failures: failures.length, method: samples[0]?.method } } }
}
async function history(stack, work) {
  for (let i = 0; i < CONTRACT.history; i++) {
    const session = await createSession(stack, work)
    await stack.call('/api/v1/workspaces/' + work.workspace + '/sessions/' + session.id + '/terminate', 'POST', { expected_offset: 0, expected_revision: 0, final_bytes: 0, final_sha256: sha(Buffer.alloc(0)) })
  }
}
async function finiteReceipt(client, session, name, capMs) {
  const start = now(), response = await client.tool(name, { session_id: session }), replyMs = now() - start
  const requested = response.requested_operation || response.operation
  if (!requested) return { complete: false, accepted: false, replyMs, reason: 'no_control_operation', status: response.status }
  if (!requested.pending && requested.receipt) return { complete: true, accepted: true, operationId: requested.id, replyMs, completionMs: now() - start, receipt: requested.receipt }
  const end = start + capMs
  let state
  do {
    await delay(100); state = await client.tool('session_upload_status', { session_id: session })
    if (state.status === 'failed') return { complete: false, replyMs, reason: state.error || 'follower_failed', observedMs: now() - start }
    if (state.operation?.id === requested.id && !state.operation.pending && state.operation.receipt) return { complete: true, accepted: true, operationId: requested.id, replyMs, completionMs: now() - start, receipt: state.operation.receipt }
  } while (now() < end)
  return { complete: false, accepted: true, operationId: requested.id, replyMs, reason: 'old_path_cap', observedMs: now() - start, capMs, captureBoundary: state?.operation?.capture_boundary, progress: { committedOffset: state?.committed_offset, status: state?.status } }
}
async function oldPathResume(stack, work, source, session, followers, capMs) {
  const client = await stack.mcp(work.workspace), ids = [session.id], peerPaths = new Map()
  // Peers use small independent canonical prefixes, rather than pretending
  // that all four jobs share one writer or one session identity.
  for (let i = 1; i < followers; i++) {
    const peerPath = source + '.peer-' + i, file = await open(source, 'r'), bytes = Buffer.alloc(1048576)
    try { const result = await file.read(bytes, 0, bytes.length, 0); assert.equal(result.bytesRead, bytes.length) } finally { await file.close() }
    await writeFile(peerPath, bytes, { flag: 'wx' })
    const peer = await seedSession(stack, work.workspace, work.task, peerPath, 1048576)
    ids.push(peer.session.id)
    peerPaths.set(peer.session.id, peerPath)
  }
  const start = now(), startReplies = []
  for (const id of ids) { const at = now(); await client.tool('session_upload_start', { session_id: id, path: peerPaths.get(id) || source, task_id: work.task, title: 'Synchronization baseline', format_label: 'Bounded benchmark JSONL' }); startReplies.push(now() - at) }
  let states = [], complete = false
  do {
    checkRunDeadline()
    await delay(250)
    states = await Promise.all(ids.map(id => client.tool('session_upload_status', { session_id: id })))
    if (states.some(value => value.status === 'failed')) break
    complete = states.every(value => value.status === 'caught_up')
  } while (!complete && now() - start < capMs)
  const result = { complete, milliseconds: complete ? now() - start : null, observedMs: now() - start, capMs,
    startReplyMs: summary(startReplies), followers, states: states.map(value => ({ status: value.status, committedBytes: value.committed_offset, error: value.error || null })),
    scope: 'actual production follower interval and authorized transports; incomplete runs have no completion percentile' }
  // Process loss deliberately leaves canonical sessions active. Joining this
  // owned producer is finite and prevents background jobs tainting calibration.
  await client.close({ hard: true })
  return result
}
async function calibrate(stack, scratch) {
  const observer = new DatabaseObserver(stack.coordinates.db, scratch.env, stack.coordinates.pgLog)
  const result = { measurement: await observer.initialize(), reference: {}, prefixes: [], qualificationContract: CONTRACT,
    measuredScope: 'Direct HTTP reference calibration and zero-browser production MCP resume cases; separate browser probes are not combined qualification.',
    coldDefinition: 'Local scan: fresh Node process/source handle, three runs; warm: same process, five runs. OS filesystem cache is uncontrolled. Database scan runs are repeated warm scans; no cold-disk claim.',
    limitations: ['SQL execution counts are logical database work, not physical TCP round trips.', 'Resource working sets sample explicitly owned stable processes; short-lived helper CPU/peaks are not fully captured.'] }
  const sampler = startResourceSampler(async () => [...stack.ownedPids(), ...await observer.query("SELECT coalesce(json_agg(pid),'[]'::json) FROM pg_stat_activity WHERE datname=current_database()")])
  let failure
  try {
    const work = await makeWork(stack, 'Synchronization calibration')
    result[runtimeInputs] = { work, prefixes: [] }
    await history(stack, work)
    for (const [name, network] of [['local', {}], ['controlled', CONTRACT.network]]) {
      stack.setNetwork(network)
      const trials = []
      for (const rate of [262144, 1048576, 4194304, 8388608]) {
        const trial = await referenceTrial(stack, work, rate); trials.push(trial)
        console.log(JSON.stringify({ phase: 'reference-rate', profile: name, offeredBytesPerSecond: rate, stable: trial.stable, completedBytesPerSecond: trial.completedBytesPerSecond, requests: trial.requests }))
      }
      const stable = trials.filter(value => value.stable)
      if (!stable.length) throw new Error('No stable reference rate measured')
      const capacity = Math.max(...stable.map(value => value.targetBytesPerSecond))
      result.reference[name] = { trials, stableBytesPerSecond: capacity, sustainableBytesPerSecond: capacity * CONTRACT.sustainableFraction, fraction: CONTRACT.sustainableFraction,
        measuredScope: 'direct authorized HTTP reference; zero production followers and zero browser clients; not combined qualification', activeSessionCounts: name === 'local' ? [1, 2, 3, 4] : [5, 6, 7, 8], historySessions: CONTRACT.history }
    }
    stack.setNetwork({})
    for (const bytes of CONTRACT.prefixes) {
      const source = join(scratch.root, 'prefix-' + bytes + '.jsonl'), fixture = await writePrefix(source, bytes)
      console.log(JSON.stringify({ phase: 'fixture-generated', bytes, milliseconds: fixture.milliseconds }))
      const seeded = await seedSession(stack, work.workspace, work.task, source, bytes)
      result[runtimeInputs].prefixes.push({ source, session: seeded.session, fixture })
      console.log(JSON.stringify({ phase: 'canonical-seeded', bytes, milliseconds: seeded.milliseconds, requests: seeded.requests }))
      const warm = [], cold = []
      for (let i = 0; i < 5; i++) { const value = await scanPrefix(source, bytes); assert.equal(value.sha256, fixture.sha256); warm.push(value.milliseconds) }
      for (let i = 0; i < 3; i++) { const value = await coldScan(source, bytes, scratch.env); assert.equal(value.sha256, fixture.sha256); cold.push(value.milliseconds) }
      const databaseScans = []
      for (let i = 0; i < 2; i++) { const value = await observer.scan(seeded.session.id, bytes); assert.equal(value.sha256, fixture.sha256); databaseScans.push(value) }
      const combined = Math.max(...warm, ...cold) + Math.max(...databaseScans.map(value => value.milliseconds))
      const prefix = { bytes, canonicalCommittedBytes: bytes, frozenSourceBoundary: bytes, pendingSourceBytes: 0, fixture, seed: { requests: seeded.requests, milliseconds: seeded.milliseconds }, sourceWarmMs: summary(warm), sourceFreshProcessMs: summary(cold), databaseScans,
        referenceVerifiedPrefixMs: combined, absoluteFiniteTargetMs: 2 * combined + 5000, calibrationIncludes: 'complete local source SHA256 plus complete independent bounded DB SHA256; metadata proof RTT fits fixed overhead, no raw-prefix HTTP-transfer mandate' }
      const network = bytes === 1073741824 ? CONTRACT.network : {}
      stack.setNetwork(network)
      prefix.productionResume = await oldPathResume(stack, work, source, seeded.session, bytes === 1048576 ? 1 : 4, bytes === 1048576 ? 35000 : 20000)
      prefix.actualProfile = { primaryPrefixBytes: bytes, peerPrefixBytes: bytes === 1048576 ? [] : [1048576, 1048576, 1048576], browserClients: 0, productionFollowers: bytes === 1048576 ? 1 : 4,
        lifecycleActiveSessions: bytes === 1048576 ? 9 : bytes === 104857600 ? 13 : 17, terminatedHistory: CONTRACT.history, network: bytes === 1073741824 ? CONTRACT.network : { rttMs: 0, bitsPerSecond: null } }
      console.log(JSON.stringify({ phase: 'production-resume', bytes, complete: prefix.productionResume.complete, observedMs: prefix.productionResume.observedMs }))
      result.prefixes.push(prefix)
      stack.setNetwork({})
    }
    result.database = await observer.stats()
    result.resources = await sampler.stop()
    const referenceBytes = Object.values(result.reference).flatMap(value => value.trials).reduce((total, value) => total + value.bytes, 0)
    const prefixBytes = result.prefixes.reduce((total, value) => total + value.bytes, 0)
    const completedReferenceWorkBytes = referenceBytes + prefixBytes * 12 // generate + seed + eight source scans + two database scans
    const completedDatabaseWorkBytes = referenceBytes + prefixBytes * 3 // seed + two independent database scans
    result.actualSampleCounts = { directReferenceHttpRequests: Object.values(result.reference).flatMap(value => value.trials).reduce((total, value) => total + value.requests, 0), sourceWarmScans: 15, sourceFreshProcessScans: 9,
      databasePrefixScans: 6, productionResumeRuns: 3, completedProductionResumeRuns: result.prefixes.filter(value => value.productionResume.complete).length, combinedThreeBrowserLiveLatencySamples: 0 }
    result.freeze = { historicalSessions: CONTRACT.history, followersPerProcess: [1, 4], lifecycleActiveSessionCountsAtResume: [9, 13, 17], typicalRecordBytes: CONTRACT.sourceRecordBytes, appendChunkBytes: 65536,
      sustainableBytesPerSecond: Object.fromEntries(Object.entries(result.reference).map(([key, value]) => [key, value.sustainableBytesPerSecond])),
      absoluteOneGiBFiniteTargetMs: result.prefixes.find(value => value.bytes === 1073741824).absoluteFiniteTargetMs,
      rssCeilingBytes: result.resources.peakRssBytes ? Math.ceil(result.resources.peakRssBytes * 1.25 + 67108864) : null,
      rssScope: 'zero-browser calibration: supervisor/server/PostgreSQL/current MCP stable PIDs; not combined three-browser RSS',
      cpuCeilingReference: { measuredCpuSeconds: result.resources.cpuSeconds, measuredScope: result.resources.method, completedReferenceWorkBytes,
        maxSecondsPerReferenceGiB: result.resources.cpuSeconds * 1073741824 / completedReferenceWorkBytes * 1.25,
        scope: 'Exact matched zero-browser calibration workflow, normalized by completed reference/seed/source-scan/DB-scan bytes; 25% headroom. Not a whole final-matrix total or unobserved-helper CPU claim.' },
      databaseStatementCeiling: { observedCalls: result.database.statements?.calls, completedDatabaseWorkBytes,
        maxCallsPerDatabaseMiB: result.database.statements?.calls * 1048576 / completedDatabaseWorkBytes * 1.1,
        scope: 'Exact matched zero-browser calibration workflow, normalized by completed HTTP reference/seed/DB-scan bytes; 10% headroom. Logical executions include background work, not physical TCP round trips.' },
      pendingTransferRule: 'Finite target = 2 * (verified-prefix reference + bounded measured transfer reference for B-N pending bytes on actual wire profile) + 5000 ms; absoluteOneGiBFiniteTargetMs applies to N=B=1 GiB, zero pending bytes only.',
      finiteRunsRequiredAtFinal: { warm: 5, cold: 3 }, liveSamplesRequiredAtFinal: 100 }
    result.freeze.measuredRecordDistribution = { calibration: { recordBytes: 65536, fraction: 1 }, browserProbeTail: { recordBytes: 1024, samples: 1 }, maximumRecordBoundary: 'supported by generator self-check; not claimed as a completed combined production profile' }
    return result
  } catch (error) { failure = error; throw error }
  finally { await disposeAll([['resource observer', () => sampler.stop()], ['database observer', () => observer.close()]], failure) }
}

async function overhead(scratch) {
  const source = join(scratch.root, 'overhead-source.jsonl'), fixture = await writePrefix(source, 1048576), results = {}
  for (const diagnostics of [false, true]) {
    let stack, observer, sampler, failure
    try {
      stack = await startStack(scratch, diagnostics)
      const work = await makeWork(stack, 'Diagnostic overhead reference')
      if (diagnostics) { observer = new DatabaseObserver(stack.coordinates.db, scratch.env, stack.coordinates.pgLog); await observer.initialize(); sampler = startResourceSampler(async () => [...stack.ownedPids(), ...await observer.query("SELECT coalesce(json_agg(pid),'[]'::json) FROM pg_stat_activity WHERE datname=current_database()")]) }
      const times = []
      for (let i = 0; i < 8; i++) { const seeded = await seedSession(stack, work.workspace, work.task, source, fixture.bytes); times.push(seeded.milliseconds); if (observer) await observer.stats() }
      results[diagnostics ? 'enabled' : 'disabled'] = { appendMs: summary(times), bytesPerAppend: fixture.bytes, sha256: fixture.sha256, provenance: stack.provenance,
        scope: diagnostics ? 'sandbox diagnostics plus baseline resource observer' : 'ordinary native sandbox defaults; no DB/resource observer' }
    } catch (error) { failure = error; throw error }
    finally { await disposeAll([['resource observer', async () => { if (sampler) await sampler.stop() }], ['database observer', async () => { if (observer) await observer.close() }], ['native stack', async () => { if (stack) await stack.close() }]], failure) }
  }
  results.medianOverheadFraction = results.enabled.appendMs.median / results.disabled.appendMs.median - 1
  results.limitations = ['Eight actual 1 MiB authorized appends per arm; separate fresh isolated clusters, same source bytes. This estimates DB/resource observation overhead, not browser-screenshot overhead or a throughput qualification.']
  return results
}

async function copyPrefix(source, target, bytes) {
  const from = await open(source, 'r'), to = await open(target, 'wx'), buffer = Buffer.alloc(1048576), hash = createHash('sha256')
  let offset = 0
  try {
    while (offset < bytes) {
      checkRunDeadline(); const { bytesRead } = await from.read(buffer, 0, Math.min(buffer.length, bytes - offset), offset)
      if (!bytesRead) throw new Error('Fixture copy truncated')
      let written = 0; while (written < bytesRead) written += (await to.write(buffer, written, bytesRead - written)).bytesWritten
      hash.update(buffer.subarray(0, bytesRead)); offset += bytesRead
    }
  } finally { await disposeAll([['copy source', () => from.close()], ['copy target', () => to.close()]]) }
  return { bytes, sha256: hash.digest('hex') }
}
async function showTranscript(stack, pages, workspace, session) {
  const views = []
  for (const page of pages) {
    const start = now(); await page.goto(stack.origin + '/workspace/' + workspace + '#tab=sessions&session=' + session)
    await page.locator('.session-record-text').first().waitFor({ timeout: 15000 })
    await page.locator('.session-records').scrollIntoViewIfNeeded()
    await page.locator('.session-records').press('End')
    views.push({ milliseconds: now() - start, geometry: await page.locator('.session-records').evaluate(element => { const r = element.getBoundingClientRect(); return { top: r.top, bottom: r.bottom, viewport: innerHeight, scrollTop: element.scrollTop, scrollHeight: element.scrollHeight, clientHeight: element.clientHeight } }) })
  }
  return views
}
export async function produce(input, rate, start, samples = 100, active = () => true, onEvent = () => {}, { wait = delay, openFile = open } = {}) {
  const file = await openFile(input.source, 'a'), events = []
  let plannedBytes = 0, actualWritten = 0, chunks = 0, writeCalls = 0, initialBoundary, frozenBoundary
  const lateness = []
  try {
    initialBoundary = (await file.stat()).size
    for (let seq = 0; seq < samples; seq++) {
      if (!active()) break
      const bytes = [1024, 1024, 1024, 65536][seq % 4], marker = 'sync-' + input.session.id.slice(0, 8) + '-' + seq + '-'
      const record = fixtureRecord(bytes, marker); plannedBytes += bytes
      const planned = start + plannedBytes * 1000 / rate
      await wait(Math.max(0, planned - now())); checkRunDeadline(); if (!active()) break
      const at = now(); lateness.push(Math.max(0, at - planned))
      for (let offset = 0; offset < record.length; offset += 1024) {
        const part = record.subarray(offset, offset + 1024); let written = 0
        while (written < part.length) {
          const { bytesWritten } = await file.write(part, written, part.length - written)
          if (!Number.isInteger(bytesWritten) || bytesWritten <= 0 || bytesWritten > part.length - written) throw new Error('Source write made no valid progress')
          written += bytesWritten; actualWritten += bytesWritten; writeCalls++
        }
        chunks++
      }
      const event = { seq, bytes, end: initialBoundary + actualWritten, sourceAt: at, writeFinishedAt: now() }; events.push(event); onEvent(event)
    }
    frozenBoundary = (await file.stat()).size
    assert.equal(frozenBoundary, initialBoundary + actualWritten, 'observed source boundary matches successful writes')
  } finally { await file.close() }
  const end = now()
  return { events, initialBoundary, frozenBoundary, bytes: actualWritten, plannedBytes, filesystemChunks: chunks, writeCalls, elapsedMs: end - start, actualBytesPerSecond: actualWritten * 1000 / (end - start), offeredBytesPerSecond: rate, admissionLatenessMs: summary(lateness) }
}
function deltaMetrics(before, after) {
  return Object.fromEntries(['downstreamConnections', 'upstreamHttpConnections', 'reusedUpstreamHttpRequests', 'webSocketConnections', 'successfulFollowerHttp'].map(key => [key, after[key] - before[key]]))
}
let liveSampling = { independentMinimum: 0, maxLiveMs: 180000 }
export function sourceVisibilityCounts(events, paints) {
  const displayed = new Set(paints.map(value => value.seq)).size
  const highWater = Math.max(-1, ...paints.map(value => value.seq))
  const represented = events.filter(value => value.seq <= highWater).length
  return { displayed, represented, superseded: represented - displayed, censored: events.length - represented }
}
export function independentPaintCohorts(events, commits, paints) {
  const seen = new Set(), result = []
  for (const paint of paints) {
    const event = events[paint.seq], commit = event && commits.find(value => value.end >= event.end)
    if (!commit || seen.has(commit.revision)) continue
    seen.add(commit.revision); result.push({ revision: commit.revision, sourceSequence: paint.seq, representedEnd: event.end, at: paint.at, commitToVisibleMs: paint.at - commit.commit_ms })
  }
  return result
}
async function liveProfile(stack, scratch, { name, work, inputs, selected = 0, network = {}, aggregateRate = 183500.8, independentMinimum = liveSampling.independentMinimum, maxLiveMs = liveSampling.maxLiveMs }) {
  stack.setNetwork(network)
  const observer = new DatabaseObserver(stack.coordinates.db, scratch.env, stack.coordinates.pgLog), client = await stack.mcp(work.workspace)
  let failure, watching = true, browserPages = [], producerResults, ordinary = [], paints, clientSnapshots, inputResponse = [], slow = [], backgroundFailure, resourceSampler, resourceStart
  const background = [], producerWork = [], activity = [], liveSourceEvents = inputs.map(() => [])
  function tracked(promise) { const task = promise.catch(error => { backgroundFailure = error; watching = false }); background.push(task); return task }
  const commitRows = new Map(), outboxRows = new Map(), statusSamples = []
  try {
    await observer.initialize()
    const { pages, version } = await stack.browsers({ fresh: true }); browserPages = pages; paints = pages.map(() => [])
    const initialViews = await showTranscript(stack, pages, work.workspace, inputs[selected].session.id)
    resourceStart = now()
    resourceSampler = startResourceSampler(() => stack.ownedPids(), { descendants: true })
    const startReplies = [], resumeStart = now()
    for (const input of inputs) { const at = now(); await client.tool('session_upload_start', { session_id: input.session.id, path: input.source, task_id: work.task, title: 'Synchronization baseline', format_label: 'Bounded benchmark JSONL' }); startReplies.push(now() - at) }
    let resumeComplete = false
    const resumeCap = inputs.some(value => value.session.committed_bytes > 1048576) ? 20000 : 35000
    do { const values = await Promise.all(inputs.map(input => client.tool('session_upload_status', { session_id: input.session.id }))); if (values.some(value => value.status === 'failed')) break; resumeComplete = values.every(value => value.status === 'caught_up'); if (!resumeComplete) await delay(250) } while (!resumeComplete && now() - resumeStart < resumeCap)
    const resume = { complete: resumeComplete, observedMs: now() - resumeStart, capMs: resumeCap, startReplyMs: summary(startReplies) }
    const scopeCounts = await observer.counts(work.workspace)
    let cursor = (await observer.query("SELECT json_build_object('cursor',coalesce(max(cursor),0)) FROM change_stream_outbox WHERE scope='workspace' AND workspace_id='" + work.workspace + "'" )).cursor
    const selectedInput = inputs[selected], prefix = selectedInput.session.id.slice(0, 8), phaseStart = now(), transportBefore = stack.transportMetrics(), proxyStart = stack.proxyEvents.length
    const perFollowerRate = aggregateRate / inputs.length, sourceBytes = 25 * (3 * 1024 + 65536), expectedDuration = independentMinimum ? maxLiveMs : sourceBytes * 1000 / perFollowerRate
    const canProduce = () => watching && (!independentMinimum || now() < phaseStart + maxLiveMs && !independenceSatisfied())
    const independenceSatisfied = () => {
      const commits = commitRows.get(selectedInput.session.id) || []
      return ordinary.length >= 100 && inputResponse.length >= 100 && commits.length >= independentMinimum && paints.every(values => independentPaintCohorts(liveSourceEvents[selected], commits, values).length >= independentMinimum) && [...outboxRows.values()].filter(value => value.entity === selectedInput.session.id && commits.some(commit => commit.transaction === value.transaction)).length >= independentMinimum
    }
    const paintLoop = tracked((async () => {
      while (watching) {
        await Promise.all(pages.map(async (page, index) => {
          const last = paints[index].at(-1)?.seq ?? -1
          const candidate = await page.evaluate(({ prefix, last }) => {
            const values = Object.keys(window.__sessionSync?.domReady || {}).filter(key => key.startsWith(prefix + ':')).map(key => Number(key.split(':')[1])).filter(seq => seq > last).sort((a, b) => b - a)
            for (const seq of values.slice(0, 5)) if (window.__sessionMarkerVisible(prefix + ':' + seq)) return seq
            return null
          }, { prefix, last })
          if (candidate !== null) { const at = await renderedWitness(page, prefix + ':' + candidate); if (at !== null) paints[index].push({ seq: candidate, at }) }
        }))
        await delay(100)
      }
    })())
    const beforeDb = await observer.stats()
    const dbLoop = tracked((async () => {
      while (watching) {
        for (const input of inputs) {
          const known = commitRows.get(input.session.id) || [], after = known.at(-1)?.end || input.session.committed_bytes
          const rows = await observer.commits(input.session.id, { after, limit: 256 }); known.push(...rows); commitRows.set(input.session.id, known)
        }
        const events = await observer.outbox(work.workspace, cursor)
        for (const event of events) { outboxRows.set(event.event_id, event); cursor = event.cursor }
        if (activity.length < 1000) activity.push(await observer.stats())
        await delay(250)
      }
    })())
    const ordinaryWork = tracked((async () => {
      for (let i = 0; i < 100; i++) {
        if (!watching) break
        await delay(Math.max(0, phaseStart + (i + 1) * expectedDuration / 100 - now()))
        const start = now(); await client.tool('task_detail', { task_id: work.task }); ordinary.push(now() - start)
        const progressStart = now(), state = await client.tool('session_upload_status', { session_id: selectedInput.session.id }); statusSamples.push({ milliseconds: now() - progressStart, at: now(), committed: state.committed_offset, status: state.status })
        const at = now(); await pages[0].keyboard.press('Shift'); await pages[0].evaluate(() => new Promise(done => requestAnimationFrame(() => requestAnimationFrame(done)))); inputResponse.push(now() - at)
      }
    })())
    const slowWork = tracked(inputs.length === 4 ? (async () => {
      await delay(4000); if (!watching) return; stack.pauseClient('2', true); slow.push({ phase: 'pause-server-to-observer-read', at: now() })
      await delay(8000); if (!watching) return; stack.pauseClient('2', false); slow.push({ phase: 'resume-observer-read', at: now() })
      await delay(4000); if (!watching) return; stack.disconnectClient('2'); slow.push({ phase: 'disconnect-observer-socket', at: now() })
    })() : Promise.resolve())
    producerWork.push(...inputs.map((input, index) => produce(input, perFollowerRate, phaseStart, independentMinimum ? 10000 : 100, canProduce, event => liveSourceEvents[index].push(event))))
    producerResults = await Promise.all(producerWork)
    await Promise.all([ordinaryWork, slowWork])
    const drainUntil = now() + 20000
    const lastSourceSequence = producerResults[selected].events.at(-1)?.seq ?? -1
    while (now() < drainUntil && !paints.every(value => (value.at(-1)?.seq ?? -1) >= lastSourceSequence)) await delay(100)
    watching = false
    await Promise.all([paintLoop, dbLoop])
    if (backgroundFailure) throw backgroundFailure
    clientSnapshots = await Promise.all(pages.map(page => page.evaluate(() => ({ http: window.__sessionSync.http, sockets: window.__sessionSync.sockets, peakSessionHTTP: window.__sessionSync.peakSessionHTTP, longTasks: window.__sessionSync.longTasks, heapBytes: performance.memory?.usedJSHeapSize || null }))))
    const finalViews = await Promise.all(pages.map(page => page.evaluate(() => {
      const element = document.querySelector('.session-records'), active = document.activeElement
      const rect = value => { const r = value.getBoundingClientRect(); return { top: r.top, bottom: r.bottom, left: r.left, right: r.right, width: r.width, height: r.height } }
      const clips = []
      for (let node = element?.parentElement; node && clips.length < 12; node = node.parentElement) {
        const style = getComputedStyle(node)
        if (/(auto|scroll|hidden|clip)/.test(style.overflow + style.overflowX + style.overflowY)) clips.push({ tag: node.tagName, class: String(node.className).slice(0, 160), rectangle: rect(node), scrollTop: node.scrollTop, clientHeight: node.clientHeight })
      }
      return { hash: location.hash.slice(0, 200), visibility: document.visibilityState, hasFocus: document.hasFocus(), activeElement: { tag: active?.tagName, class: String(active?.className || '').slice(0, 160) }, viewport: { width: innerWidth, height: innerHeight, scrollY }, transcript: element ? { rectangle: rect(element), scrollTop: element.scrollTop, scrollHeight: element.scrollHeight, clientHeight: element.clientHeight } : null, clips }
    })))
    const resources = { ...await resourceSampler.stop(), observedWallMs: now() - resourceStart, scope: 'matching live profile from initial transcript view through resume, concurrent production/ordinary MCP/three browsers and drain; source generation included, seeding excluded' }
    const generated = producerResults[selected], commits = commitRows.get(selectedInput.session.id) || [], metrics = [], sourceCommit = [], progressLatency = pages.map(() => []), censored = { sourceCommit: 0, representedVisibleProgress: pages.map(() => 0) }
    for (const event of generated.events) {
      const commit = commits.find(value => value.end >= event.end)
      if (commit) sourceCommit.push(commit.commit_ms - event.sourceAt); else censored.sourceCommit++
      const witness = paints.map((values, index) => {
        const value = values.find(paint => paint.seq >= event.seq)
        if (!value || !commit) { censored.representedVisibleProgress[index]++; return null }
        progressLatency[index].push(value.at - commit.commit_ms)
        return { representedBySequence: value.seq, renderedUpperBoundAt: value.at, commitToVisibleMs: value.at - commit.commit_ms }
      })
      metrics.push({ sequence: event.seq, sourceAt: event.sourceAt, sourceWriteFinishedAt: event.writeFinishedAt, sourceEnd: event.end, committedRevision: commit?.revision || null, canonicalChunkEnd: commit?.end || null, commitAt: commit?.commit_ms || null, visibleProgress: witness })
    }
    const wire = stack.proxyEvents.slice(proxyStart).filter(value => value.phase === 'websocket-egress-observed'), delivery = []
    for (const [id, event] of outboxRows) {
      if (event.entity !== selectedInput.session.id) continue
      const commitsForEvent = commits.filter(value => value.transaction === event.transaction)
      if (!commitsForEvent.length) continue
      const egress = wire.filter(value => value.eventId === id)
      delivery.push({ eventId: id, commitRevision: commitsForEvent.at(-1).revision, commitAt: event.commit_ms,
        clients: clientSnapshots.map((snapshot, index) => { const received = snapshot.sockets.find(value => value.eventId === id), sent = egress.find(value => value.client === String(index)); return { client: index, dispatchLoopbackUpperBoundMs: sent ? sent.at - event.commit_ms : null, proxyToBrowserMs: received && sent ? received.at - sent.at : null, commitToBrowserMs: received ? received.at - event.commit_ms : null } }) })
    }
    const state = await client.tool('session_upload_status', { session_id: selectedInput.session.id }), canonical = await stack.call('/api/v1/workspaces/' + work.workspace + '/sessions/' + selectedInput.session.id)
    const result = { name, scopeCounts, followers: inputs.length, browserClients: 3, selectedPrefixBytes: selectedInput.session.committed_bytes, prefixBytes: inputs.map(value => value.session.committed_bytes), network, chromium: version, initialViews, finalViews, resources, resume,
      load: { requestedAggregateBytesPerSecond: aggregateRate, requestedPerFollowerBytesPerSecond: perFollowerRate, elapsedMs: Math.max(...producerResults.map(value => value.elapsedMs)), generatedBytes: producerResults.reduce((total, value) => total + value.bytes, 0), actualAggregateBytesPerSecond: producerResults.reduce((total, value) => total + value.bytes, 0) * 1000 / Math.max(...producerResults.map(value => value.elapsedMs)), producers: producerResults.map(value => ({ ...value, events: undefined })), pattern: [1024, 1024, 1024, 65536], filesystemChunkBytes: 1024, markersIncludedInRate: true },
      samples: { independentSourceEvents: generated.events.length, uniqueCommittedChunks: commits.length, uniqueOutboxEvents: delivery.length, representedPaintFrames: paints.map(value => value.length), independentRenderCohorts: paints.map(value => independentPaintCohorts(generated.events, commits, value).length), displayedSourceSequences: paints.map(value => value.map(paint => paint.seq)), supersededIntermediateSequences: paints.map(value => sourceVisibilityCounts(generated.events, value).superseded), visibilityAccounting: paints.map(value => sourceVisibilityCounts(generated.events, value)), ordinaryMcpCalls: ordinary.length },
      sourceToCommitMs: summary(sourceCommit), representedCommitToVisibleMs: progressLatency.map(summary), ordinaryMcpMs: summary(ordinary), progressReplyMs: summary(statusSamples.map(value => value.milliseconds)), nativeInputToTwoFrameMs: summary(inputResponse),
      inputScope: 'actual Shift key on the focused product transcript to two animation frames, including driver/native main-thread delay; does not replace the existing frontend local-interaction suite',
      censored, observations: metrics, delivery, slowObserver: { transitions: slow, scope: 'server-side proxy socket read pause/disconnection; not a claim of kernel send-buffer saturation' },
      client: clientSnapshots.map(value => ({ nativeSessionHttpPeak: value.peakSessionHTTP, completedSessionHttp: value.http.length, byteDeliveryRequests: value.http.filter(item => item.path.endsWith('/transcript')).length, longTaskMs: summary(value.longTasks), heapBytes: value.heapBytes })),
      database: { logicalStatementCalls: (activity.at(-1)?.statements?.calls || 0) - (beforeDb.statements?.calls || 0), sampledPeakLockWaiters: Math.max(0, ...activity.map(value => value.activity.lock_waiters)), samples: activity.length, scope: 'logical executions and sampled lock waiters, not TCP round trips or exact cumulative waits' },
      transport: deltaMetrics(transportBefore, stack.transportMetrics()), inferredHelperStarts: { successfulFollowerHttp: stack.provenance.mcp === '18594374f1d60e527c24324ff3c671febb06b366d6d36b415384f03b1267fa12' ? stack.transportMetrics().successfulFollowerHttp - transportBefore.successfulFollowerHttp : null, basis: 'verified classic artifact one-helper-per-successful-follower-HTTP mapping; changed artifacts and pre-network failures are unobserved' },
      boundary: { N: canonical.committed_bytes, B: generated.frozenBoundary, pendingBytes: generated.frozenBoundary - canonical.committed_bytes, revision: canonical.revision, uploaderStatus: state.status },
      limitations: ['Three witnesses are correlated views of one source stream; browser count never multiplies independent samples.', 'Visible high water represents coalesced committed progress; intermediate sequences may be superseded.', 'Missing old-path progress remains censored; no completion percentile is invented.', 'Dispatch measurement includes server dispatch plus loopback receipt and is not an exact send timestamp.'] }
    const commitCohorts = commits.map(commit => { const covered = generated.events.filter(event => event.end > commit.offset && event.end <= commit.end); return covered.length ? { revision: commit.revision, sourceToCommitMs: commit.commit_ms - covered[0].sourceAt } : null }).filter(Boolean)
    result.independentLatency = { sourceCommitCohorts: commitCohorts, sourceToCommitMs: summary(commitCohorts.map(value => value.sourceToCommitMs)), renderCohorts: paints.map(value => independentPaintCohorts(generated.events, commits, value)),
      commitToVisibleMs: paints.map(value => summary(independentPaintCohorts(generated.events, commits, value).map(item => item.commitToVisibleMs))), requiredAtQualification: 100,
      requestedMinimum: independentMinimum, maxProducerMs: maxLiveMs, sufficient: commitCohorts.length >= 100 && delivery.length >= 100 && paints.every(value => independentPaintCohorts(generated.events, commits, value).length >= 100),
      scope: 'unique canonical commit/outbox/revision-changing raster cohorts; correlated source markers and three views never multiply independent counts' }
    result.latencySamplingDisposition = result.independentLatency.sufficient ? 'independent-cohort count reached; budget values still require assessment' : 'insufficient independent qualification cohorts; preserve actual baseline observations/censoring'
    console.log(JSON.stringify({ phase: 'real-live-profile', name, rate: result.load.actualAggregateBytesPerSecond, sourceEvents: result.samples.independentSourceEvents, committed: sourceCommit.length, visibleProgress: progressLatency.map(value => value.length), uniquePaints: result.samples.representedPaintFrames, backlogBytes: result.boundary.pendingBytes }))
    result[runtimeInputs] = { pages, work, inputs }
    return result
  } catch (error) { failure = error; throw error }
  finally {
    watching = false; stack.pauseClient('2', false)
    await disposeAll([['live observers', () => bounded(Promise.allSettled([...background, ...producerWork]), 65000, 'live observation joins')], ['live resource observer', async () => { if (resourceSampler) await resourceSampler.stop() }], ['live uploader', () => client.close({ hard: true })], ['live database observer', () => observer.close()]], failure)
  }
}

async function standaloneLive(stack, scratch) {
  const work = await makeWork(stack, 'Real live synchronization samples'), source = join(scratch.root, 'live-local.jsonl')
  await history(stack, work); const fixture = await writePrefix(source, 1048576), seeded = await seedSession(stack, work.workspace, work.task, source, fixture.bytes)
  return liveProfile(stack, scratch, { name: 'local-one-follower-three-browser', work, inputs: [{ source, session: seeded.session, fixture }] })
}

async function receiptProof(receipt, source, boundary) {
  if (!receipt) return { checked: false, reason: 'no_completed_receipt' }
  const hash = await scanPrefix(source, receipt.committed_offset)
  const valid = receipt.source_verified === true && receipt.capture_boundary === boundary && receipt.committed_offset <= boundary && receipt.sha256 === hash.sha256 && receipt.pending_tail === (receipt.committed_offset < boundary)
  return { checked: true, valid, committedOffset: receipt.committed_offset, revision: receipt.revision, captureBoundary: receipt.capture_boundary, pendingTail: receipt.pending_tail, sha256: receipt.sha256, locallyReconstructedSha256: hash.sha256 }
}
async function sealPrefix(manager, work, input) {
  const canonical = await manager.tool('session_detail', { session_id: input.session.id })
  const length = canonical.committed_bytes, boundary = (await stat(input.source)).size, hash = await scanPrefix(input.source, length)
  const start = now()
  const acknowledgment = await manager.tool('session_terminate', { session_id: input.session.id, expected_offset: length, expected_revision: canonical.revision, final_bytes: length, final_sha256: hash.sha256 })
  const replyAt = now(), summary = acknowledgment.summary || acknowledgment, sealed = await manager.tool('session_detail', { session_id: input.session.id })
  return { complete: summary.state === 'terminated' && sealed.state === 'terminated', milliseconds: replyAt - start, readbackMs: now() - replyAt, sessionId: input.session.id, N: length, B: boundary, pendingBytes: boundary - length, expectedRevision: canonical.revision,
    finalRevision: sealed.revision, sha256: sealed.final_sha256, locallyReconstructedSha256: hash.sha256, hashVerified: sealed.final_sha256 === hash.sha256 && sealed.final_bytes === length,
    scope: 'independent ordinary managing MCP; seals only canonical committed N, not unuploaded source B-N' }
}
async function finiteLedger(stack, scratch, { name, work, baseSource, followers = 1, network = {}, sealOnly = false }) {
  stack.setNetwork(network)
  let uploader = await stack.mcp(work.workspace), manager = await stack.mcp(work.workspace, { role: 'Synchronization manager' }), failure
  let { pages } = await stack.browsers({ fresh: true })
  const ledger = [], witnesses = []
  async function one(kind, index, state, witness = false) {
    if (state === 'fresh-process') { await uploader.close({ hard: true }); uploader = await stack.mcp(work.workspace); ({ pages } = await stack.browsers({ fresh: true })) }
    const source = join(scratch.root, name + '-finite-' + kind + '-' + index + (witness ? '-witness' : '') + '.jsonl')
    const n = witness ? 0 : 1048576
    if (witness) await writeFile(source, Buffer.alloc(0), { flag: 'wx' }); else await copyPrefix(baseSource, source, n)
    const seeded = await seedSession(stack, work.workspace, work.task, source, n)
    const tail = fixtureRecord(witness ? 1024 : 4096, 'finite-' + index)
    await appendFile(source, tail)
    const b = n + tail.length, identity = await stat(source, { bigint: true }), inputs = [{ source, session: seeded.session }]
    for (let peer = 1; peer < followers; peer++) {
      const path = source + '.peer-' + peer; await copyPrefix(baseSource, path, 1048576)
      const value = await seedSession(stack, work.workspace, work.task, path, 1048576); inputs.push({ source: path, session: value.session })
    }
    if (!witness) await showTranscript(stack, pages, work.workspace, seeded.session.id)
    const start = now(), starts = []
    for (const input of inputs) { const at = now(); await uploader.tool('session_upload_start', { session_id: input.session.id, path: input.source, task_id: work.task, title: 'Synchronization baseline', format_label: 'Bounded benchmark JSONL' }); starts.push(now() - at) }
    const control = sealOnly ? { complete: false, accepted: false, reason: 'seal-only revalidation; control not remeasured' } : await finiteReceipt(uploader, seeded.session.id, kind === 'flush' ? 'session_upload_flush' : 'session_upload_stop', witness ? 15000 : 8000)
    const proof = await receiptProof(control.receipt, source, b)
    const finalIdentity = await stat(source, { bigint: true })
    const sourceIdentityUnchanged = identity.dev === finalIdentity.dev && identity.ino === finalIdentity.ino && finalIdentity.size === BigInt(b)
    const seals = []
    for (const input of inputs) seals.push(await sealPrefix(manager, work, input))
    let terminal = []
    const retireStart = now()
    do {
      terminal = await Promise.all(inputs.map(input => uploader.tool('session_upload_status', { session_id: input.session.id })))
      if (terminal.every(value => ['terminated', 'stopped', 'failed'].includes(value.status))) break
      await delay(100)
    } while (now() - retireStart < 10000)
    const value = { operation: sealOnly ? 'seal-only' : kind, sourceControlKind: kind, sessionId: seeded.session.id, index, state, witness, followers, mcpPid: uploader.pid, managerPid: manager.pid,
      reset: { mcpAndFollowerVerificationAndTransport: state === 'fresh-process' ? 'fresh process' : 'same warm process across cases; distinct new follower jobs', browser: state === 'fresh-process' ? 'fresh browser process/contexts' : 'warm browser process', nativeServerDatabase: 'retained warm instance', osCache: 'uncontrolled' },
      boundary: { N: n, B: b, pendingBytes: b - n }, sourceIdentity: { device: identity.dev.toString(), inode: identity.ino.toString(), unchanged: sourceIdentityUnchanged },
      resume: { registrationReplyMs: summary(starts), observedThroughControlMs: now() - start, fullyReconciled: terminal[0]?.committed_offset === b }, control, receiptVerification: proof,
      primarySeal: seals[0], peerSeals: seals.slice(1), uploaderAfterIndependentSeal: terminal.map(item => ({ status: item.status, committedBytes: item.committed_offset })), retireObservedMs: now() - retireStart }
    console.log(JSON.stringify({ phase: 'finite-attempt', profile: name, operation: kind, state, witness, controlComplete: control.complete, replyMs: control.replyMs, receiptChecked: proof.checked, sealComplete: value.primarySeal.complete, N: n, B: b }))
    return value
  }
  try {
    for (let index = 0; index < 8; index++) {
      const state = index < 5 ? 'warm' : 'fresh-process'
      for (const kind of ['flush', 'stop']) ledger.push(await one(kind, index, state))
    }
    if (!sealOnly) for (const kind of ['flush', 'stop']) witnesses.push(await one(kind, 0, 'fresh-process', true))
    const counts = {}
    for (const kind of ['flush', 'stop']) {
      counts[kind] = {}
      for (const state of ['warm', 'fresh-process']) {
        const values = ledger.filter(value => value.operation === kind && value.state === state)
        counts[kind][state] = { attempts: values.length, accepted: values.filter(value => value.control.accepted).length, completed: values.filter(value => value.control.complete).length, censored: values.filter(value => !value.control.complete).length, independentlySealed: values.filter(value => value.primarySeal.hashVerified).length }
      }
    }
    return { name, network, sealOnly, ledger, witnesses, counts,
      sealingCounts: { warm: ledger.filter(value => value.state === 'warm' && value.primarySeal.complete).length, freshProcess: ledger.filter(value => value.state === 'fresh-process' && value.primarySeal.complete).length, additionalSmallWitnesses: witnesses.filter(value => value.primarySeal.complete).length },
      scope: 'five warm and three genuinely reset MCP/browser runs OF EACH control kind; each has an independent canonical-prefix seal; capped controls have no invented completion percentile' }
  } catch (error) { failure = error; throw error }
  finally { await disposeAll([['finite uploader', () => uploader.close({ hard: true })], ['finite manager', () => manager.close()]], failure) }
}
async function standaloneFinite(stack, scratch, sealOnly = false) {
  const work = await makeWork(stack, 'Finite synchronization baseline'), source = join(scratch.root, 'finite-base.jsonl')
  await writePrefix(source, 1048576)
  return finiteLedger(stack, scratch, { name: 'local-finite', work, baseSource: source, sealOnly })
}

async function repairedSuite(stack, scratch) {
  const calibration = await calibrate(stack, scratch), runtime = calibration[runtimeInputs], profiles = []
  for (const [name, followers, network] of [['local', 1, {}], ['controlled', 4, CONTRACT.network], ['mixed-large-catchup', 4, CONTRACT.network]]) {
    runDeadline = now() + 20 * 60 * 1000
    let inputs = [], selected = 0
    if (name === 'mixed-large-catchup') { inputs = [runtime.prefixes[2], runtime.prefixes[1]]; selected = 2 }
    for (let i = inputs.length; i < followers; i++) {
      const source = join(scratch.root, name + '-source-' + i + '.jsonl'), fixture = await copyPrefix(runtime.prefixes[0].source, source, 1048576)
      const value = await seedSession(stack, runtime.work.workspace, runtime.work.task, source, fixture.bytes); inputs.push({ source, session: value.session, fixture })
    }
    const live = await liveProfile(stack, scratch, { name, work: runtime.work, inputs, selected, network, aggregateRate: calibration.freeze.sustainableBytesPerSecond[name === 'local' ? 'local' : 'controlled'] })
    const finite = await finiteLedger(stack, scratch, { name, work: runtime.work, baseSource: runtime.prefixes[0].source, followers, network })
    let largeSeal = null
    if (name === 'mixed-large-catchup') { const manager = await stack.mcp(runtime.work.workspace, { role: 'Synchronization manager' }); try { largeSeal = await sealPrefix(manager, runtime.work, inputs[0]) } finally { await manager.close() } }
    profiles.push({ name, live, finite, largeSeal })
  }
  return { calibration, profiles, qualificationStatus: 'not_qualified', scope: 'actual baseline profile attempts and bounded old-path censoring; independent optimized final acceptance is still required' }
}

async function resourceSuite(stack, scratch) {
  const receiptPath = join(frontend, 'perf/session-sync.repaired-baseline.json'), receiptBytes = await readFile(receiptPath)
  if (receiptBytes.length > 2097152) throw new Error('Baseline receipt exceeds input bound')
  const receipt = JSON.parse(receiptBytes), calibration = receipt.result.calibration
  if (receipt.mode !== 'repair' || receipt.baseSha !== '43c52ffd2377e3af82d31f7da82a4bc0c395cc17') throw new Error('Unexpected resource reference input')
  const work = await makeWork(stack, 'Combined synchronization resource reference'), prefixes = [], profiles = []
  await history(stack, work)
  for (const bytes of CONTRACT.prefixes) {
    const source = join(scratch.root, 'resource-prefix-' + bytes + '.jsonl'), fixture = await writePrefix(source, bytes), seeded = await seedSession(stack, work.workspace, work.task, source, bytes)
    prefixes.push({ source, fixture, session: seeded.session })
    console.log(JSON.stringify({ phase: 'resource-prefix-seeded', bytes, milliseconds: seeded.milliseconds }))
  }
  // Recreate metadata population without repeating reference trials or finite
  // controls. Non-followed padding contains no transcript; this state difference
  // is explicit and must also be used for matching resource comparisons.
  const observer = new DatabaseObserver(stack.coordinates.db, scratch.env, stack.coordinates.pgLog)
  let failure
  try {
    await observer.initialize()
    for (const reference of receipt.result.profiles) {
      runDeadline = now() + 20 * 60 * 1000
      const inputs = reference.name === 'mixed-large-catchup' ? [prefixes[2], prefixes[1]] : []
      while (inputs.length < reference.live.followers) {
        const source = join(scratch.root, reference.name + '-resource-' + inputs.length + '.jsonl'), fixture = await copyPrefix(prefixes[0].source, source, 1048576), seeded = await seedSession(stack, work.workspace, work.task, source, fixture.bytes)
        inputs.push({ source, fixture, session: seeded.session })
      }
      let population = await observer.counts(work.workspace)
      while (population.active < reference.live.scopeCounts.active) { await createSession(stack, work); population.active++ }
      while (population.terminated < reference.live.scopeCounts.terminated) {
        const session = await createSession(stack, work)
        await stack.call('/api/v1/workspaces/' + work.workspace + '/sessions/' + session.id + '/terminate', 'POST', { expected_offset: 0, expected_revision: 0, final_bytes: 0, final_sha256: sha(Buffer.alloc(0)) }); population.terminated++
      }
      assert.equal(population.active, reference.live.scopeCounts.active)
      const live = await liveProfile(stack, scratch, { name: reference.name, work, inputs, selected: reference.name === 'mixed-large-catchup' ? 2 : 0, network: reference.live.network, aggregateRate: reference.live.load.requestedAggregateBytesPerSecond })
      live.resourceCeilings = combinedResourceCeilings(live, calibration.freeze)
      profiles.push(live)
      console.log(JSON.stringify({ phase: 'combined-resource-profile', name: live.name, samples: live.resources.samples, failures: live.resources.failures, peakRssBytes: live.resources.peakRssBytes, cpuSeconds: live.resources.cpuSeconds }))
    }
    return { referenceReceiptSha256: sha(receiptBytes), referenceMethodSha256: receipt.methodSha256, profiles,
      combinedRssCeilingBytes: Math.max(...profiles.map(value => value.resourceCeilings.rssBytes)),
      state: 'fresh isolated server/DB, warm seeded prefixes, uncontrolled OS cache; actual follower prefix sizes, browser count, metadata counts, link and producer workload matched; non-followed population padding is empty rather than replayed prior calibration/finite payloads',
      scope: 'combined live resource follow-up only; no repeated calibration, finite ledger or performance qualification claim' }
  } catch (error) { failure = error; throw error }
  finally { await disposeAll([['resource population observer', () => observer.close()]], failure) }
}

export function combinedResourceCeilings(live, reference) {
  if (live.resources.samples < 2 || !Number.isFinite(live.resources.peakRssBytes) || live.resources.peakRssBytes <= 0 || !Number.isFinite(live.resources.cpuSeconds) || live.resources.cpuSeconds < 0 || !Number.isFinite(live.load.generatedBytes) || live.load.generatedBytes <= 0) throw new Error('Insufficient combined resource observations')
  const generatedMiB = live.load.generatedBytes / 1048576, prefixGiB = live.prefixBytes.reduce((a, b) => a + b, 0) / 1073741824
  return { rssBytes: Math.ceil(live.resources.peakRssBytes * 1.25 + 67108864),
        liveCpuSecondsPerGeneratedMiB: live.resources.cpuSeconds * 1.25 / generatedMiB,
        prefixCpuAllowanceSeconds: prefixGiB * reference.cpuCeilingReference.maxSecondsPerReferenceGiB,
        liveSqlCallsPerGeneratedMiB: live.database.logicalStatementCalls * 1.1 / generatedMiB,
        prefixSqlAllowanceCalls: prefixGiB * 1024 * reference.databaseStatementCeiling.maxCallsPerDatabaseMiB,
        scope: 'same live record/rate/profile; CPU/SQL allowance = per-generated-MiB live allowance plus one linear complete-prefix reference allowance; RSS has 25% plus 64 MiB headroom and never scales with prefix size' }
}

async function main() {
  const mode = process.argv[2], args = process.argv.slice(3), options = {}
  for (let i = 0; i < args.length; i += 2) { if (!args[i]?.startsWith('--') || args[i + 1] === undefined) throw new Error('Use --name value'); options[args[i].slice(2)] = args[i + 1] }
  if (options['independent-min'] !== undefined) { const value = Number(options['independent-min']); if (!Number.isInteger(value) || value < 0 || value > 1000) throw new Error('Invalid independent cohort minimum'); liveSampling.independentMinimum = value }
  if (options['max-live-ms'] !== undefined) { const value = Number(options['max-live-ms']); if (!Number.isInteger(value) || value < 1000 || value > 600000) throw new Error('Invalid maximum live duration'); liveSampling.maxLiveMs = value }
  if (!['probe', 'calibrate', 'overhead', 'live', 'finite', 'seal-revalidate', 'repair', 'resources'].includes(mode)) throw new Error('Expected a documented session synchronization measurement mode')
  if (!/^[0-9a-f]{40}$/.test(options['base-sha'] || '')) throw new Error('Exact --base-sha required from coordinator')
  const scratch = await makeScratch(options.scratch || join(repo, '.scratch/responsive-sessions-baseline-6bfb2638'))
  runDeadline = now() + 20 * 60 * 1000
  let stack, output, failure
  try {
    const start = now(); if (mode !== 'overhead') stack = await startStack(scratch)
    console.log(JSON.stringify({ phase: 'setup', milliseconds: now() - start, phaseWallGuardMs: 1200000, maximumSuiteWallMs: ['repair', 'resources'].includes(mode) ? 4800000 : 1200000, cleanupBudgetMs: 180000 }))
    output = { schema: 1, mode, qualificationStatus: 'not_qualified', baseSha: options['base-sha'], measuredAt: new Date().toISOString(), environment: await environmentFingerprint(), provenance: stack?.provenance || null,
      methodSha256: loadedMethodSha256, result: mode === 'probe' ? await probeStack(stack, scratch) : mode === 'calibrate' ? await calibrate(stack, scratch) : mode === 'live' ? await standaloneLive(stack, scratch) : mode === 'finite' || mode === 'seal-revalidate' ? await standaloneFinite(stack, scratch, mode === 'seal-revalidate') : mode === 'repair' ? await repairedSuite(stack, scratch) : mode === 'resources' ? await resourceSuite(stack, scratch) : await overhead(scratch) }
  } catch (error) { failure = error }
  finally { failure = await cleanupRun(stack, scratch, failure) }
  if (failure) throw failure
  if (options.output) {
    const path = ownedPath(join(frontend, 'perf'), resolve(options.output))
    await writeFile(path, JSON.stringify(output, null, 2) + '\n')
  }
  console.log(JSON.stringify({ phase: 'complete', mode, output: options.output || null, summary: mode === 'calibrate' ? { absoluteOneGiBFiniteTargetMs: output.result.freeze.absoluteOneGiBFiniteTargetMs, sustainableBytesPerSecond: output.result.freeze.sustainableBytesPerSecond } : mode === 'overhead' ? { medianOverheadFraction: output.result.medianOverheadFraction } : mode === 'live' ? { samples: output.result.samples, censored: output.result.censored } : mode === 'probe' ? { resumedMs: output.result.resumedMs, visible: output.result.direct.render.map(value => value.complete) } : { completed: true } }))
}
if (process.argv[1] && import.meta.url === pathToFileURL(resolve(process.argv[1])).href) {
  main().catch(error => { console.error(JSON.stringify({ phase: 'failure', error: error.message })); process.exitCode = 1 })
}
