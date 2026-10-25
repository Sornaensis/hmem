import assert from 'node:assert/strict'
import test from 'node:test'
import { mkdtemp, readFile, rm, stat, open } from 'node:fs/promises'
import { tmpdir } from 'node:os'
import { join, resolve } from 'node:path'
import { createHash } from 'node:crypto'
import http from 'node:http'
import { finiteTargets, fixtureRecord, networkProxy, ownedPath, percentile, scanPrefix, writePrefix, SharedLink, renderedWitness, visibleMarkerWitness, disposeAll, extractApiResponse, WireEvents, independentPaintCohorts, combinedResourceCeilings, sourceVisibilityCounts, rejectFailedSetup, cleanupRun, produce } from './session-sync.mjs'
import { chromium } from '@playwright/test'
import { Writable } from 'node:stream'
import { pipeline } from 'node:stream/promises'
import { componentUploadArguments, PostgresFrames, sourceSql, seedDatabaseChunks, verifyNativeBuild } from './session-sync.mjs'
import { spawn } from 'node:child_process'
import { once } from 'node:events'
import { createInterface } from 'node:readline'

test('native admission rejects source drift, uncompleted builds and mismatched compiled binaries', () => {
  const inputs = { 'core.hs': 'a'.repeat(64) }, binaries = { harness: 'b'.repeat(64), mcp: 'c'.repeat(64), server: 'd'.repeat(64) }
  const receipt = { task: 'e04fbd45-9e61-405b-9196-c51a153f8cd8', buildExitCode: 0, capturedAt: '2026-10-25T18:00:00Z', builtAt: '2026-10-25T18:01:00Z', productionInputs: inputs, binaries }
  verifyNativeBuild(receipt, inputs, binaries)
  assert.throws(() => verifyNativeBuild(receipt, { 'core.hs': 'e'.repeat(64) }, binaries), /prebuild frozen inputs/)
  assert.throws(() => verifyNativeBuild(receipt, inputs, { ...binaries, harness: 'f'.repeat(64) }), /harness pairing/)
  assert.throws(() => verifyNativeBuild({ ...receipt, buildExitCode: null }, inputs, binaries), /completed native build/)
})

test('database fixture COPY batches preserve exact source bytes, global cursor and bounded admission', async () => {
  const root = await mkdtemp(join(tmpdir(), 'session-sync-db-copy-')), source = join(root, 'source.jsonl')
  const state = { id: '01234567-0000-0000-0000-000000000000', committed_bytes: 0, revision: 0 }, batches = []
  let rows = [], copiedBytes = 0; const digest = createHash('sha256')
  try {
    const fixture = await writePrefix(source, 5242880, 1048576)
    const stack = { async call(path, method) { return { ...state } } }
    const observer = { async raw(sql) {
      if (sql.startsWith('\\copy ')) {
        const value = await readFile(source + '.copy'); assert.equal(value.subarray(0, 11).toString('binary'), 'PGCOPY\n\xff\r\n\0')
        let at = 19, total = 0; rows = []
        while (value.readInt16BE(at) !== -1) {
          assert.equal(value.readInt16BE(at), 3); assert.equal(value.readInt32BE(at + 2), 8); assert.equal(value.readInt32BE(at + 14), 8)
          const offset = Number(value.readBigInt64BE(at + 6)), revision = Number(value.readBigInt64BE(at + 18)), length = value.readInt32BE(at + 26)
          const bytes = value.subarray(at + 30, at + 30 + length); assert.equal(bytes.length, length); rows.push({ offset, revision, bytes }); total += length; at += 30 + length
        }
        assert.equal(at + 2, value.length); assert.ok(total <= 4194304 && rows.length <= 256); batches.push({ bytes: total, rows: rows.length })
      } else if (sql.startsWith('DO $$ DECLARE chunk record;')) {
        for (const row of rows) { assert.equal(row.offset, state.committed_bytes); assert.equal(row.revision, state.revision + 1); state.committed_bytes += row.bytes.length; state.revision++; digest.update(row.bytes); copiedBytes += row.bytes.length }; rows = []
      }
      return []
    } }
    const result = await seedDatabaseChunks(stack, observer, { root }, { workspace: 'workspace', task: 'task' }, source, fixture.bytes, 1048576)
    assert.equal(copiedBytes, fixture.bytes); assert.equal(digest.digest('hex'), fixture.sha256)
    assert.deepEqual(batches, [{ bytes: 4194304, rows: 4 }, { bytes: 1048576, rows: 1 }]); assert.equal(result.chunks, 5); assert.equal(result.batches, 2)
    await assert.rejects(stat(source + '.copy'), error => error.code === 'ENOENT')
  } finally { await rm(root, { recursive: true, force: true }) }
})

test('native counter accepts Windows CRLF control lines and joins without a database connection', async () => {
  const child = spawn(process.execPath, [resolve(import.meta.dirname, 'session-sync.mjs'), 'pg-counter', '1'], { windowsHide: true, stdio: ['pipe', 'pipe', 'pipe'] })
  child.stderr.resume(); const closed = once(child, 'close', { signal: AbortSignal.timeout(10000) }), lines = createInterface({ input: child.stdout })
  try {
    const [ready] = await once(lines, 'line', { signal: AbortSignal.timeout(5000) }); assert.ok(JSON.parse(ready).port > 0)
    const next = once(lines, 'line', { signal: AbortSignal.timeout(5000) }); child.stdin.write('snapshot\r\n')
    const [line] = await next; const counts = JSON.parse(line); assert.equal(counts.backendReady, 0); assert.equal(counts.unsupported, 0)
    child.stdin.end('close\r\n'); const [code] = await closed; assert.equal(code, 0)
  } finally { lines.close(); if (child.exitCode === null && child.signalCode === null) { const joined = once(child, 'close', { signal: AbortSignal.timeout(5000) }); child.kill(); await joined }; child.stdin.destroy(); child.stdout.destroy() }
})

test('plan collection reads exact current list and descendant SQL including Haskell string gaps', async () => {
  const source = await readFile(resolve(import.meta.dirname, '../../../hmem-core/src/HMem/DB/AgentSession.hs'), 'utf8')
  const list = sourceSql(source, 'listStatement'), activity = sourceSql(source, 'projectSessionActivity')
  assert.ok(list.includes('ORDER BY s.created_at DESC,s.id DESC LIMIT $5'))
  assert.ok(list.includes('hmem_session_project_tasks($1,$8)'))
  assert.ok(activity.startsWith('WITH RECURSIVE project_tree(root,id)'))
  assert.ok(activity.includes('task_tree(root,id)')); assert.ok(!activity.includes('\\\n'))
  assert.throws(() => sourceSql(source, 'missingBinding'), /unavailable/)
})

test('PostgreSQL cycle observation discards fragmented payloads and counts protocol frames only', () => {
  const seen = [], frames = new PostgresFrames(value => seen.push(value), { startup: true })
  const start = Buffer.alloc(8); start.writeUInt32BE(8); start.writeUInt32BE(196608, 4)
  const frame = (kind, bytes) => { const header = Buffer.alloc(5); header[0] = kind.charCodeAt(0); header.writeUInt32BE(bytes + 4, 1); return Buffer.concat([header, Buffer.alloc(bytes, 83)]) }
  const bytes = Buffer.concat([start, frame('Q', 12000), frame('S', 0), frame('E', 12)])
  for (let offset = 0; offset < bytes.length; offset += 3) frames.push(bytes.subarray(offset, offset + 3))
  assert.deepEqual(seen, [196608, 'Q', 'S', 'E'])
  assert.equal(frames.header.length, 8); assert.equal(frames.skip, 0)
  const invalid = Buffer.from([81, 127, 255, 255, 255]); assert.throws(() => frames.push(invalid), /Unsupported/)
})

test('component uploader preserves the complete immutable seed binding before any large fixture', () => {
  const work = { workspace: 'workspace', task: 'task' }
  const input = { source: 'source.jsonl', session: { id: 'session', workspace_id: work.workspace, state: 'active', title: 'Synchronization baseline', format_label: 'Bounded benchmark JSONL', parent_session_id: 'parent' } }
  assert.deepEqual(componentUploadArguments(input, work), { workspace_id: 'workspace', session_id: 'session', path: 'source.jsonl', task_id: 'task', title: input.session.title, format_label: input.session.format_label, parent_session_id: 'parent' })
  assert.equal(componentUploadArguments({ ...input, session: { ...input.session, parent_session_id: null } }, work).parent_session_id, null)
  assert.throws(() => componentUploadArguments(input, { ...work, workspace: 'foreign' }), /workspace/)
  assert.throws(() => componentUploadArguments({ ...input, session: { ...input.session, title: null } }, work), /binding/)
})

test('cohort stop during a wait excludes the unwritten record and counts short writes exactly', async () => {
  const root = await mkdtemp(join(tmpdir(), 'session-sync-producer-')), source = join(root, 'source.jsonl')
  try {
    await writePrefix(source, 1024, 1024)
    let active = true, waits = 0
    const result = await produce({ source, session: { id: '01234567-0000-0000-0000-000000000000' } }, 262144, performance.timeOrigin + performance.now(), 100, () => active, () => {}, {
      async wait() { if (++waits === 2) active = false },
      async openFile(path, flags) {
        const file = await open(path, flags)
        return { stat: () => file.stat(), close: () => file.close(), write: (bytes, offset, length) => file.write(bytes, offset, Math.min(length, 13)) }
      },
    })
    assert.equal(waits, 2)
    assert.equal(result.plannedBytes, 2048)
    assert.equal(result.bytes, 1024)
    assert.equal(result.events.length, 1)
    assert.equal(result.events[0].end, 2048)
    assert.equal(result.frozenBoundary, (await stat(source)).size)
    assert.equal(result.frozenBoundary, 2048)
    assert.equal(result.writeCalls, Math.ceil(1024 / 13))
    assert.equal(result.filesystemChunks, 1)
    assert.equal((await readFile(source, 'utf8')).split('\r\n').filter(Boolean).length, 2)
  } finally { await rm(root, { recursive: true, force: true }) }
})

test('a zero-byte source write fails finitely and closes its handle', async () => {
  let closed = false
  await assert.rejects(produce({ source: 'controlled', session: { id: '01234567' } }, 262144, performance.timeOrigin + performance.now(), 1, () => true, () => {}, {
    async wait() {},
    async openFile() { return { async stat() { return { size: 0 } }, async write() { return { bytesWritten: 0 } }, async close() { closed = true } } },
  }), /Source write made no valid progress/)
  assert.equal(closed, true)
})

test('censored source events are distinct from represented superseded markers', () => {
  const events = Array.from({ length: 100 }, (_, seq) => ({ seq }))
  assert.deepEqual(sourceVisibilityCounts(events, [{ seq: 22 }, { seq: 54 }]), { displayed: 2, represented: 55, superseded: 53, censored: 45 })
  assert.deepEqual(sourceVisibilityCounts(events, []), { displayed: 0, represented: 0, superseded: 0, censored: 100 })
})

test('missing resource samples cannot become a 64 MiB approved ceiling', () => {
  const live = { resources: { samples: 0, peakRssBytes: null, cpuSeconds: null }, load: { generatedBytes: 1048576 }, prefixBytes: [1048576], database: { logicalStatementCalls: 10 } }
  const reference = { cpuCeilingReference: { maxSecondsPerReferenceGiB: 10 }, databaseStatementCeiling: { maxCallsPerDatabaseMiB: 50 } }
  assert.throws(() => combinedResourceCeilings(live, reference), /Insufficient/)
  live.resources = { samples: 2, peakRssBytes: 104857600, cpuSeconds: 2 }
  const small = combinedResourceCeilings(live, reference)
  live.prefixBytes = [1073741824]
  const large = combinedResourceCeilings(live, reference)
  assert.equal(large.rssBytes, small.rssBytes, 'memory allowance never grows with transcript size')
  assert.ok(large.prefixCpuAllowanceSeconds > small.prefixCpuAllowanceSeconds)
})

test('nearest-rank p95 and finite targets preserve the approved rule', () => {
  assert.equal(percentile(Array.from({ length: 100 }, (_, i) => i + 1)), 95)
  assert.equal(percentile([]), null)
  assert.deepEqual(finiteTargets({ resume: 1234, seal: 5 }), { resume: 7468, seal: 5010 })
  assert.throws(() => finiteTargets({ resume: NaN }))
  assert.throws(() => percentile([NaN]))
})
test('joining failures attempt every independent owner and retain scratch disposition', async () => {
  const attempted = [], original = new Error('measurement failed')
  await assert.rejects(disposeAll([
    ['sampler', async () => { attempted.push('sampler'); throw new Error('join deadline') }],
    ['observer', async () => { attempted.push('observer'); throw new Error('connection stuck') }],
    ['stack', async () => { attempted.push('stack') }],
  ], original), error => error.retainScratch === true && error.errors[0] === original && error.errors.length === 3)
  assert.deepEqual(attempted, ['sampler', 'observer', 'stack'])
})

test('failed setup cleanup retains scratch when outer main never acquired the stack', async () => {
  const attempted = [], setupFailure = new Error('sandbox startup failed')
  let stack, failure, deleted = false
  try {
    stack = await rejectFailedSetup(setupFailure, () => disposeAll([
      ['browser', async () => { attempted.push('browser'); throw new Error('join deadline') }],
      ['HTTP context', async () => { attempted.push('HTTP context') }],
      ['sandbox', async () => { attempted.push('sandbox') }],
    ]))
  } catch (error) { failure = error }
  failure = await cleanupRun(stack, { async clean() { deleted = true } }, failure)
  assert.equal(stack, undefined, 'failed acquisition never assigned main stack')
  assert.equal(failure.retainScratch, true)
  assert.equal(failure.errors[0], setupFailure)
  assert.deepEqual(attempted, ['browser', 'HTTP context', 'sandbox'])
  assert.equal(deleted, false, 'outer-main cleanup cannot delete an unjoined sandbox')
})
test('API bodies are disposed after success, HTTP failure and JSON parser failure', async () => {
  let retained = 0, peak = 0
  function response({ good = true, malformed = false } = {}) {
    retained++; peak = Math.max(peak, retained)
    return { ok: () => good, status: () => good ? 200 : 400,
      async json() { if (malformed) throw new Error('invalid body'); return good ? { items: [1, 2, 3] } : { code: 'bad_fixture' } },
      async dispose() { retained-- } }
  }
  for (let i = 0; i < 1000; i++) assert.deepEqual(await extractApiResponse(response(), 'fixture'), { items: [1, 2, 3] })
  await assert.rejects(extractApiResponse(response({ malformed: true }), 'fixture'), /invalid body/)
  await assert.rejects(extractApiResponse(response({ good: false }), 'fixture'), /400 bad_fixture/)
  assert.equal(retained, 0); assert.equal(peak, 1)
})
test('wire observations retain event identity but never checkpoint tokens or raw payloads', () => {
  const events = [], wire = new WireEvents(value => events.push(value))
  wire.push(Buffer.from('HTTP/1.1 101 Switching Protocols\r\n\r\n'))
  function frame(value) { const bytes = Buffer.from(JSON.stringify(value)), header = Buffer.alloc(4); header[0] = 0x81; header[1] = 126; header.writeUInt16BE(bytes.length, 2); return Buffer.concat([header, bytes]) }
  const changed = frame({ schema_version: 1, type: 'change', event: { event_id: 'event-1', entity: { id: 'session-1', type: 'agent_session' } } })
  wire.push(changed.subarray(0, 7)); wire.push(changed.subarray(7))
  wire.push(frame({ schema_version: 1, type: 'checkpoint', resume_token: 'must-not-be-retained' }))
  assert.equal(events.length, 1); assert.equal(events[0].eventId, 'event-1'); assert.equal(events[0].entity, 'session-1')
  assert.equal(JSON.stringify(events).includes('must-not-be-retained'), false)
})
test('coalesced source markers never multiply independent rendered revision cohorts', () => {
  const events = Array.from({ length: 100 }, (_, seq) => ({ seq, end: (seq + 1) * 1024 }))
  const commits = [{ revision: 1, end: 51200, commit_ms: 10 }, { revision: 2, end: 102400, commit_ms: 20 }]
  const paints = [{ seq: 10, at: 30 }, { seq: 49, at: 35 }, { seq: 99, at: 40 }]
  const cohorts = independentPaintCohorts(events, commits, paints)
  assert.equal(cohorts.length, 2)
  assert.deepEqual(cohorts.map(value => value.revision), [1, 2])
})
test('scratch containment refuses root, parent and sibling prefixes', () => {
  assert.equal(ownedPath('/owned', '/owned/task'), resolve('/owned/task'))
  for (const path of ['/owned', '/owned/../elsewhere', '/owned-other/task']) assert.throws(() => ownedPath('/owned', path))
})
test('bounded source generation preserves exact UTF-8 CRLF records and independent hash', async () => {
  const root = await mkdtemp(join(tmpdir(), 'session-sync-test-')), path = join(root, 'source.jsonl')
  try {
    const result = await writePrefix(path, 1048576, 65536), scan = await scanPrefix(path, result.bytes), bytes = await readFile(path)
    assert.equal((await stat(path)).size, 1048576)
    assert.equal(result.maxWorkingBufferBytes, 65536)
    assert.equal(scan.maxWorkingBufferBytes, 1048576)
    assert.equal(result.sha256, scan.sha256)
    assert.equal(result.sha256, createHash('sha256').update(bytes).digest('hex'))
    assert.equal(bytes.toString('utf8').split('\r\n').filter(Boolean).map(JSON.parse).length, 16)
    assert.equal(fixtureRecord(1024, 'sync-test-1-').length, 1024)
    const maximum = fixtureRecord(1048576, 'sync-max-1-')
    assert.equal(maximum.length, 1048576)
    assert.ok(JSON.parse(maximum.toString('utf8')).text.endsWith('sync-max-1-'))
    const unevenPath = join(root, 'uneven.jsonl')
    assert.equal((await writePrefix(unevenPath, 1030, 1024)).bytes, 1030)
    assert.equal((await readFile(unevenPath, 'utf8')).split('\r\n').filter(Boolean).map(JSON.parse).length, 2)
    await assert.rejects(scanPrefix(path, bytes.length + 1), /truncated/)
  } finally { await rm(root, { recursive: true, force: true }) }
})
test('controlled HTTP path preserves bytes and adds one declared RTT, not one per chunk', async () => {
  const bytes = Buffer.alloc(262144, 42), server = http.createServer((_request, response) => response.end(bytes))
  await new Promise(done => server.listen(0, '127.0.0.1', done))
  const receipts = [], proxy = await networkProxy('http://127.0.0.1:' + server.address().port, { rttMs: 50, bitsPerSecond: 100000000 }, value => receipts.push(value))
  try {
    const start = performance.now(), response = await fetch(proxy.origin), actual = Buffer.from(await response.arrayBuffer()), elapsed = performance.now() - start
    assert.deepEqual(actual, bytes)
    assert.ok(elapsed >= 50, 'RTT and byte pacing are measured')
    assert.equal(receipts.filter(value => value.phase === 'upstream-read-end').length, 1)
    assert.equal(receipts.filter(value => value.phase === 'downstream-finish').length, 1)
    assert.equal(receipts[0].status, 200)
  } finally { await proxy.close(); await new Promise(done => server.close(done)) }
})
test('HTTP/WS-equivalent streams share one directional rate, latency and bounded backpressure', async () => {
  const link = new SharedLink(8000000, 25), sizes = [524288, 524288, 524288, 524288], start = performance.now()
  let bytes = 0
  try {
    await Promise.all(sizes.map(size => {
      const source = link.stream()
      const finished = pipeline(source, new Writable({ write(chunk, _encoding, done) { bytes += chunk.length; setTimeout(done, 1) } }))
      for (let i = 0; i < size; i += 65536) source.write(Buffer.alloc(65536))
      source.end(); return finished
    }))
    assert.equal(bytes, sizes.reduce((a, b) => a + b))
    assert.ok(performance.now() - start >= 2000, 'four streams do not each receive a separate 8 Mbit/s budget')
    assert.ok(link.peakQueuedBytes <= 4 * 262144)
  } finally { link.close() }
})
test('real concurrent HTTP and WebSocket traffic shares capacity and WS delivery follows policy changes', async () => {
  const bytes = Buffer.alloc(524288, 7)
  let upgraded
  const frame = data => { const header = Buffer.alloc(10); header[0] = 0x82; header[1] = 127; header.writeBigUInt64BE(BigInt(data.length), 2); return Buffer.concat([header, data]) }
  const server = http.createServer((_request, response) => response.end(bytes))
  server.on('upgrade', (request, socket) => {
    upgraded = socket
    const accept = createHash('sha1').update(request.headers['sec-websocket-key'] + '258EAFA5-E914-47DA-95CA-C5AB0DC85B11').digest('base64')
    socket.write('HTTP/1.1 101 Switching Protocols\r\nUpgrade: websocket\r\nConnection: Upgrade\r\nSec-WebSocket-Accept: ' + accept + '\r\n\r\n')
    socket.write(frame(bytes))
  })
  await new Promise(done => server.listen(0, '127.0.0.1', done))
  const proxy = await networkProxy('http://127.0.0.1:' + server.address().port, { rttMs: 50, bitsPerSecond: 8000000 })
  let socket
  try {
    const start = performance.now()
    socket = new WebSocket(proxy.origin.replace('http:', 'ws:'))
    const opening = new Promise((resolve, reject) => { socket.addEventListener('open', () => resolve(performance.now() - start), { once: true }); socket.addEventListener('error', reject, { once: true }) })
    const received = new Promise(resolve => socket.addEventListener('message', event => resolve(event.data), { once: true }))
    const [httpBytes, wsBytes, opened] = await Promise.all([fetch(proxy.origin).then(response => response.arrayBuffer()), received, opening])
    assert.equal(httpBytes.byteLength, bytes.length); assert.equal(wsBytes.size, bytes.length)
    assert.ok(opened >= 45, 'WebSocket handshake includes both one-way delays')
    assert.ok(performance.now() - start >= 1000, 'HTTP and WS do not each get a full separate link')
    proxy.setPolicy({ rttMs: 200, bitsPerSecond: 100000000 })
    const at = performance.now(), next = new Promise(resolve => socket.addEventListener('message', resolve, { once: true }))
    upgraded.write(frame(Buffer.alloc(1)))
    await next
    assert.ok(performance.now() - at >= 90, 'existing WS delivery observes new one-way latency')
  } finally { upgraded?.destroy(); await proxy.close(); await new Promise(done => server.close(done)) }
})
test('a DOM-present marker clipped from the viewport cannot produce a rendered timestamp', async () => {
  const browser = await chromium.launch({ headless: true })
  try {
    const page = await browser.newPage({ viewport: { width: 800, height: 600 } })
    await page.setContent('<div id="clip" style="overflow:auto;height:80px;width:500px"><div style="height:500px"></div><div class="session-record-text">sync-deadbeef-1-</div></div>')
    assert.equal(await page.locator('.session-record-text').count(), 1)
    assert.equal(await page.evaluate(visibleMarkerWitness, 'deadbeef:1'), null)
    assert.equal(await renderedWitness(page, 'deadbeef:1'), null)
    await page.locator('#clip').evaluate(element => { element.scrollTop = element.scrollHeight })
    assert.ok(await page.evaluate(visibleMarkerWitness, 'deadbeef:1'))
    assert.ok(Number.isFinite(await renderedWitness(page, 'deadbeef:1')))
    await page.locator('#clip').evaluate(element => { element.style.marginTop = '1000px' })
    assert.equal(await renderedWitness(page, 'deadbeef:1'), null, 'outside the window remains non-visible')
  } finally { await browser.close() }
})
