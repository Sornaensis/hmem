#!/usr/bin/env node
// Read-only readback sample for published exact session archives. For each
// target it performs only authorized GETs:
//   GET .../sessions/{id}/archive/{generation}/content  (stored gzip object)
//   GET .../sessions/{id}/evidence?offset&limit          (original-byte ranges)
// It checks the binding headers and compressed count/SHA-256, decompresses
// locally to check the original count/SHA-256, and compares the evidence ranges
// with the decompressed bytes. It never requests the archive manifest route,
// which records an archive request for the session.
//
//   node tools/perftest/archive-readback.mjs --base http://127.0.0.1:18429 --targets targets.json
//
// targets.json is an array of {workspace_id, session_id, generation_id,
// raw_bytes, raw_sha256, compressed_bytes, compressed_sha256} rows, e.g. from
// session_archive_generations. The bearer token is HMEM_API_KEY from an explicit
// --secrets FILE, else the HMEM_API_KEY environment variable. Only a loopback
// --base (127.0.0.1, localhost, ::1) falls back to the throwaway
// .tmp/perftest/secrets.env. Plain http is refused for other hosts unless
// --allow-http is given, and redirects are refused. Unknown, repeated or
// valueless flags are errors. Each request has a --timeout-ms deadline (default
// 120000). The token is never printed. Output is one JSON document on stdout.
import { createHash } from 'node:crypto'
import { readFileSync, existsSync } from 'node:fs'
import { createGunzip } from 'node:zlib'
import { Readable } from 'node:stream'

const usage = 'usage: --base URL --targets FILE [--secrets FILE] [--range-bytes N] [--timeout-ms N] [--allow-http]'
const valued = ['base', 'targets', 'secrets', 'range-bytes', 'timeout-ms']
const argv = process.argv.slice(2), opt = {}
for (let i = 0; i < argv.length; i++) {
  const name = argv[i].startsWith('--') ? argv[i].slice(2) : undefined
  if (name === undefined || name in opt) throw new Error(usage)
  if (name === 'allow-http') opt[name] = true
  else if (valued.includes(name) && i + 1 < argv.length && !argv[i + 1].startsWith('--')) opt[name] = argv[++i]
  else throw new Error(usage)
}
if (!opt.base || !opt.targets) throw new Error(usage)
const base = new URL(opt.base)
if (base.protocol !== 'http:' && base.protocol !== 'https:') throw new Error('--base must be an http(s) URL')
const loopback = ['127.0.0.1', 'localhost', '[::1]'].includes(base.hostname)
if (base.protocol === 'http:' && !loopback && !opt['allow-http'])
  throw new Error('refusing plain http to a non-loopback host; use https or pass --allow-http')
const fileToken = file => existsSync(file)
  ? readFileSync(file, 'utf8').split(/\r?\n/).find(line => line.startsWith('HMEM_API_KEY='))?.slice(13) : undefined
const token = opt.secrets ? fileToken(opt.secrets)
  : process.env.HMEM_API_KEY || (loopback ? fileToken('.tmp/perftest/secrets.env') : undefined)
if (!token) throw new Error(opt.secrets ? 'HMEM_API_KEY is not set in the --secrets file'
  : loopback ? 'HMEM_API_KEY is unavailable'
  : 'HMEM_API_KEY is unavailable: set it or pass --secrets FILE for a non-loopback --base')
const rangeBytes = Number(opt['range-bytes'] || 65536)
const timeoutMs = Number(opt['timeout-ms'] || 120000)
if (!(rangeBytes > 0) || !(timeoutMs > 0)) throw new Error(usage)
const targets = JSON.parse(readFileSync(opt.targets, 'utf8'))
const headers = { Authorization: 'Bearer ' + token }
const get = url => fetch(url, { method: 'GET', headers, redirect: 'error', signal: AbortSignal.timeout(timeoutMs) })
const sessionPath = t => base.origin + base.pathname.replace(/\/+$/, '') +
  '/api/v1/workspaces/' + t.workspace_id + '/sessions/' + t.session_id

async function readback(t) {
  const raw = BigInt(t.raw_bytes)
  const offsets = [...new Set([0n, raw / 2n, raw > BigInt(rangeBytes) ? raw - BigInt(rangeBytes) : 0n])]
  const wanted = offsets.map(start => ({ start, end: start + BigInt(rangeBytes) < raw ? start + BigInt(rangeBytes) : raw, parts: [] }))
  const started = performance.now()
  const response = await get(sessionPath(t) + '/archive/' + t.generation_id + '/content')
  const result = { session_id: t.session_id, generation_id: t.generation_id, http_status: response.status }
  if (response.status !== 200) return { ...result, outcome: 'failed', body: (await response.text()).slice(0, 200) }
  result.binding_headers_match =
    response.headers.get('x-hmem-archive-generation') === t.generation_id &&
    response.headers.get('x-hmem-archive-sha256') === t.compressed_sha256 &&
    response.headers.get('x-hmem-archive-raw-bytes') === String(t.raw_bytes) &&
    response.headers.get('x-hmem-archive-raw-sha256') === t.raw_sha256 &&
    response.headers.get('content-encoding') === null
  const compressedHash = createHash('sha256'), rawHash = createHash('sha256')
  let compressedCount = 0n, rawCount = 0n
  const gunzip = createGunzip()
  const decoded = (async () => {
    for await (const chunk of gunzip) {
      const start = rawCount, end = rawCount + BigInt(chunk.length)
      for (const w of wanted) {
        if (end > w.start && start < w.end) {
          const from = Number((w.start > start ? w.start : start) - start)
          const to = Number((w.end < end ? w.end : end) - start)
          w.parts.push(Buffer.from(chunk.subarray(from, to)))
        }
      }
      rawHash.update(chunk); rawCount = end
    }
  })()
  for await (const chunk of Readable.fromWeb(response.body)) {
    compressedHash.update(chunk); compressedCount += BigInt(chunk.length)
    if (!gunzip.write(chunk)) await new Promise(resolve => gunzip.once('drain', resolve))
  }
  gunzip.end()
  await decoded
  result.content_ms = Math.round(performance.now() - started)
  result.compressed_receipt_match = compressedCount === BigInt(t.compressed_bytes) && compressedHash.digest('hex') === t.compressed_sha256
  result.raw_receipt_match = rawCount === raw && rawHash.digest('hex') === t.raw_sha256
  let matched = 0
  for (const w of wanted) {
    const length = Number(w.end - w.start)
    const reply = await get(sessionPath(t) + '/evidence?offset=' + w.start + '&limit=' + length)
    if (reply.status !== 200) continue
    const body = await reply.json()
    if (Buffer.from(body.data_base64, 'base64').equals(Buffer.concat(w.parts)) && BigInt(body.offset) === w.start) matched++
  }
  result.evidence_ranges = { requested: wanted.length, matched, bytes_each: rangeBytes }
  result.outcome = result.binding_headers_match && result.compressed_receipt_match && result.raw_receipt_match &&
    matched === wanted.length ? 'passed' : 'failed'
  return result
}

const results = []
for (const t of targets) results.push(await readback(t))
console.log(JSON.stringify({ schema: 'hmem/archive-readback-sample/v1', captured_at: new Date().toISOString(),
  targets: results.length, passed: results.filter(r => r.outcome === 'passed').length, results }, null, 1))
