import assert from 'node:assert/strict'
import test from 'node:test'
import { createServer } from 'node:net'
import { spawn, execFile } from 'node:child_process'
import { once } from 'node:events'
import { createInterface } from 'node:readline'
import { promisify } from 'node:util'
import { readFile, realpath, access, readdir } from 'node:fs/promises'
import { resolve, join, relative, basename, sep, delimiter } from 'node:path'
import { tmpdir } from 'node:os'
import { createHash } from 'node:crypto'
import { chromium, request as playwrightRequest } from '@playwright/test'

// One fixture-owned native PostgreSQL sandbox, with a ten-minute lifetime.
// Credentials stay in memory; stdout consumes only public sandbox coordinates.
const repo = resolve(import.meta.dirname, '../../..')
const frontend = resolve(import.meta.dirname, '..')
const digest = value => createHash('sha256').update(value).digest('hex')
const delay = ms => new Promise(resolveDelay => setTimeout(resolveDelay, ms))
async function bounded(promise, ms, phase) {
  let timer
  try { return await Promise.race([promise, new Promise((_, reject) => { timer = setTimeout(() => reject(new Error(phase + ' exceeded its finite deadline')), ms) })]) }
  finally { clearTimeout(timer) }
}
async function until(predicate, phase, ms = 15000) {
  const end = Date.now() + ms
  while (Date.now() < end) { if (await predicate()) return; await delay(25) }
  throw new Error(phase + ' did not reach its expected state')
}
async function freePort() {
  const socket = createServer()
  await new Promise(resolveListen => socket.listen(0, '127.0.0.1', resolveListen))
  const port = socket.address().port
  await new Promise(resolveClose => socket.close(resolveClose))
  return port
}
async function exists(path) { try { await access(path); return true } catch { return false } }
function pidAlive(pid) { try { process.kill(pid, 0); return true } catch (error) { if (error.code === 'ESRCH') return false; throw error } }

test('production read-only Elm converges after real conditional writers and delayed canonical notifications', { timeout: 600000 }, async () => {
  let harness, harnessClosed, buildProcess, buildClosed, browser, browserServer, context, writer, root, postgresPid, lifetime
  let primaryFailure
  const coordinates = {}, received = [], held = [], errors = [], pagePuts = []
  let holding = false, heldBytes = 0
  try {
    const { stdout } = await promisify(execFile)('stack', ['path', '--local-install-root'], { cwd: repo, windowsHide: true, timeout: 30000 })
    const executable = join(stdout.trim(), 'bin', process.platform === 'win32' ? 'hmem-test-harness.exe' : 'hmem-test-harness')
    const harnessHash = digest(await readFile(executable))
    const port = await freePort(), origin = 'http://127.0.0.1:' + port
    harness = spawn(executable, ['deployed', '--interactive', '--port', String(port)], { cwd: repo, env: process.env, windowsHide: true, stdio: ['pipe', 'pipe', 'pipe'] })
    harnessClosed = once(harness, 'close')
    harness.stderr.resume()
    createInterface({ input: harness.stdout }).on('line', line => {
      for (const [label, key] of [['Sandbox root:', 'root'], ['Static dir  :', 'static'], ['Server URL  :', 'url']]) {
        if (line.startsWith(label)) coordinates[key] = line.slice(label.length).trim()
      }
      if (line.startsWith('Press Enter')) coordinates.ready = true
    })
    lifetime = setTimeout(() => harness.stdin.end('\n'), 570000)
    await until(() => coordinates.ready || harness.exitCode !== null, 'Native harness startup', 180000)
    assert.equal(harness.exitCode, null, 'Harness must remain alive with its owned stdin pipe open')
    assert.equal(coordinates.url, origin)
    root = await realpath(coordinates.root)
    const temporary = await realpath(tmpdir()), staticRoot = await realpath(coordinates.static)
    assert.ok(root.startsWith(temporary + sep) && basename(root).startsWith('hmem-sandbox'), 'Only the newly printed fixture-owned sandbox is writable')
    assert.equal(relative(root, staticRoot), 'static')
    postgresPid = Number((await readFile(join(root, 'postgres/data/postmaster.pid'), 'utf8')).split('\n')[0])
    assert.ok(Number.isSafeInteger(postgresPid) && postgresPid > 0 && pidAlive(postgresPid))
    const buildEnv = { ...process.env, VITE_HMEM_API_URL: origin, VITE_HMEM_WS_URL: 'ws://127.0.0.1:' + port + '/api/v1/ws', VITE_HMEM_AUTH_MODE: 'deployed' }
    const pathKey = Object.keys(buildEnv).find(key => key.toLowerCase() === 'path') || 'PATH'
    buildEnv[pathKey] = join(frontend, 'node_modules/.bin') + delimiter + (buildEnv[pathKey] || '')
    buildProcess = spawn(process.execPath, ['node_modules/vite/bin/vite.js', 'build', '--outDir', staticRoot, '--emptyOutDir'], {
      cwd: frontend, windowsHide: true, stdio: ['ignore', 'pipe', 'pipe'],
      env: buildEnv
    })
    let buildDiagnostic = ''
    const captureBuild = chunk => { buildDiagnostic = (buildDiagnostic + chunk.toString()).slice(-8192) }
    buildClosed = once(buildProcess, 'close')
    buildProcess.stdout.on('data', captureBuild); buildProcess.stderr.on('data', captureBuild)
    const [buildCode] = await bounded(buildClosed, 60000, 'Fixture production build')
    assert.equal(buildCode, 0, 'Fixture production build: ' + buildDiagnostic)
    const assets = await readdir(join(staticRoot, 'assets'))
    const assetHashes = Object.fromEntries(await Promise.all(assets.map(async name => [name, digest(await readFile(join(staticRoot, 'assets', name)))])))
    writer = await playwrightRequest.newContext({ baseURL: origin, extraHTTPHeaders: { Cookie: 'hmem_session=sandbox-session-token; hmem_csrf=sandbox-csrf-token', 'X-CSRF-Token': 'sandbox-csrf-token' } })
    async function api(path, method = 'GET', data, headers = {}) {
      const response = await writer.fetch(path, { method, data, headers })
      return { status: response.status(), body: await response.json() }
    }
    assert.equal((await api('/api/v1/session')).status, 200)
    const workspaces = await api('/api/v1/workspaces')
    let workspace = workspaces.body.items.find(value => value.workspace_type === 'repository')
    if (!workspace) {
      const created = await api('/api/v1/workspaces', 'POST', { name: 'Conditional Observation browser fixture', workspace_type: 'repository' })
      assert.ok([200, 201].includes(created.status)); workspace = created.body
    }
    const subjects = [{ subject_kind: 'file', subject: 'src/Conditional.elm' }, { subject_kind: 'glob', subject: 'src/**/*.elm' }]
    const created = await api('/api/v1/observations', 'POST', { workspace_id: workspace.id, subjects, git_sha: '0123456789abcdef0123456789abcdef01234567', content: 'Original real Observation' })
    assert.ok([200, 201].includes(created.status))
    const original = created.body, target = '/api/v1/observations/' + original.id
    const provenance = value => ({ id: value.id, workspace_id: value.workspace_id, subjects: value.subjects, git_sha: value.git_sha, created_at: value.created_at })
    browserServer = await chromium.launchServer({ headless: true })
    browser = await chromium.connect(browserServer.wsEndpoint())
    context = await browser.newContext({ viewport: { width: 1440, height: 900 } })
    await context.addCookies([{ name: 'hmem_session', value: 'sandbox-session-token', url: origin }, { name: 'hmem_csrf', value: 'sandbox-csrf-token', url: origin }])
    await context.routeWebSocket('**/api/v1/ws*', socket => {
      const server = socket.connectToServer()
      // Browser->server forwarding remains real. Only genuine server deliveries
      // are delayed; no response, conflict, or frame is synthesized.
      server.onMessage(message => {
        const raw = typeof message === 'string' ? message : message.toString('utf8')
        received.push(JSON.parse(raw).type)
        if (received.length > 256) received.shift()
        if (holding) {
          heldBytes += Buffer.byteLength(raw)
          assert.ok(held.length < 128 && heldBytes <= 1024 * 1024, 'Real frame buffer stays bounded')
          held.push({ socket, message, hash: digest(message) })
        } else socket.send(message)
      })
    })
    const page = await context.newPage()
    // The routeWebSocket proxy does not expose DevTools frame events. Observe
    // actual browser message delivery passively; neither payload nor forwarding
    // is changed, and only this fixture's own API socket is observed.
    await page.addInitScript(({ url }) => {
      globalThis.__conditionalDeliveredFrames = []
      const NativeWebSocket = globalThis.WebSocket
      globalThis.WebSocket = class extends NativeWebSocket {
        constructor(...args) {
          super(...args)
          if (String(args[0]).startsWith(url)) this.addEventListener('message', event => {
            if (typeof event.data === 'string') {
              globalThis.__conditionalDeliveredFrames.push(event.data)
              if (globalThis.__conditionalDeliveredFrames.length > 256) globalThis.__conditionalDeliveredFrames.shift()
            }
          })
        }
      }
    }, { url: 'ws://127.0.0.1:' + port + '/api/v1/ws' })
    page.on('pageerror', error => errors.push(error.message))
    page.on('response', async response => {
      if (response.request().method() === 'PUT' && new URL(response.url()).pathname === target) {
        pagePuts.push({ status: response.status(), headers: response.request().headers(), data: response.request().postDataJSON(), body: await response.json() })
      }
    })
    await page.goto(origin + '/workspace/' + workspace.id + '#tab=observations&observation=' + original.id)
    await page.locator('.observation-detail-content').waitFor()
    assert.equal(await page.locator('#observation-edit, #observation-edit-content').count(), 0)
    await until(() => received.includes('checkpoint'), 'Real WebSocket initial checkpoint')
    holding = true
    const competing = await api(target, 'PUT', { content: 'Independent writer canonical', reviewed_git_sha: 'b'.repeat(40) }, { 'If-Match': '"' + original.content_version + '"', 'X-Request-Id': 'conditional-browser-competing' })
    assert.equal(competing.status, 200)
    assert.notEqual(competing.body.content_version, original.content_version)
    assert.deepEqual(provenance(competing.body), provenance(original))
    await until(() => held.some(value => value.message.toString().includes(original.id)), 'Held genuine competing-write delivery')
    const stale = await api(target, 'PUT', { content: 'Explicit conditional writer draft', reviewed_git_sha: 'c'.repeat(40) }, { 'If-Match': '"' + original.content_version + '"', 'X-Request-Id': 'conditional-browser-stale' })
    assert.equal(stale.status, 409)
    assert.equal(stale.body.code, 'observation_content_conflict')
    assert.equal(stale.body.latest.content_version, competing.body.content_version)
    assert.equal((await api(target)).body.content, 'Independent writer canonical')
    const rebased = await api(target, 'PUT', { content: 'Explicit conditional writer draft', reviewed_git_sha: 'c'.repeat(40) }, { 'If-Match': '"' + competing.body.content_version + '"', 'X-Request-Id': 'conditional-browser-rebased' })
    assert.equal(rebased.status, 200)
    const accepted = rebased.body
    assert.notEqual(accepted.content_version, competing.body.content_version)
    assert.deepEqual(provenance(accepted), provenance(original))
    assert.equal(pagePuts.length, 0, 'Read-only body never sends a UI content PUT')
    const actualHeld = held.splice(0), heldDigest = digest(actualHeld.map(value => value.hash).join('\n'))
    const deliveryStart = await page.evaluate(() => globalThis.__conditionalDeliveredFrames.length)
    holding = false
    for (const value of actualHeld) value.socket.send(value.message)
    await until(async () => {
      const delivered = (await page.evaluate(start => globalThis.__conditionalDeliveredFrames.slice(start), deliveryStart)).map(digest)
      return actualHeld.every(value => delivered.filter(hash => hash === value.hash).length >= actualHeld.filter(other => other.hash === value.hash).length)
    }, 'Every genuine held frame reaches the browser')
    await page.waitForFunction(() => !document.querySelector('.loading-indicator'))
    await until(async () => (await page.locator('.observation-detail-content').innerText()).includes('Explicit conditional writer draft'), 'Released genuine frames preserve accepted canonical')
    const final = await api(target)
    assert.equal(final.body.content_version, accepted.content_version)
    assert.equal(final.body.content, accepted.content)
    assert.deepEqual(provenance(final.body), provenance(original))
    assert.equal(await page.locator('#observation-edit, #observation-edit-content').count(), 0)
    assert.equal(pagePuts.length, 0)
    // Explicit UI reviews use the same real conditional transport as independent writers.
    await page.locator('#observation-review').click()
    await page.locator('#observation-reviewed-sha').fill('d'.repeat(40))
    await page.getByRole('button', { name: 'Record re-audit', exact: true }).click()
    await until(() => pagePuts.length === 1, 'Real UI same-content re-audit')
    assert.equal(pagePuts[0].status, 200)
    assert.deepEqual(Object.keys(pagePuts[0].data).sort(), ['content', 'reviewed_git_sha'])
    assert.equal(pagePuts[0].headers['if-match'], '"' + accepted.content_version + '"')
    const audited = pagePuts[0].body
    assert.equal(audited.content, accepted.content)
    assert.notEqual(audited.content_version, accepted.content_version)
    assert.equal(audited.latest_sequence, accepted.latest_sequence + 1)
    assert.equal(audited.current_provenance.reviewed_git_sha, 'd'.repeat(40))
    assert.deepEqual(provenance(audited), provenance(original))
    await until(async () => await page.locator('#observation-edit-content').count() === 0, 'Re-audit editor retirement')
    await page.locator('#observation-review').click()
    await page.locator('#observation-edit-content').fill('Both browser draft fields survive conflict')
    await page.locator('#observation-reviewed-sha').fill('e'.repeat(40))
    holding = true
    const intervening = await api(target, 'PUT', { content: 'Intervening canonical review', reviewed_git_sha: 'f'.repeat(40) }, { 'If-Match': '"' + audited.content_version + '"' })
    assert.equal(intervening.status, 200)
    await until(() => held.some(value => value.message.toString().includes(original.id)), 'Held intervening review delivery')
    await page.getByRole('button', { name: 'Save reviewed content', exact: true }).click()
    await until(() => pagePuts.length === 2, 'UI stale review conflict')
    assert.equal(pagePuts[1].status, 409)
    assert.equal(await page.locator('#observation-edit-content').inputValue(), 'Both browser draft fields survive conflict')
    assert.equal(await page.locator('#observation-reviewed-sha').inputValue(), 'e'.repeat(40))
    await until(async () => await page.getByRole('button', { name: 'Keep my draft', exact: true }).isEnabled(), 'Conflict canonical refresh')
    await page.getByRole('button', { name: 'Keep my draft', exact: true }).click()
    assert.equal(await page.locator('#observation-reviewed-sha').inputValue(), 'e'.repeat(40))
    await page.getByRole('button', { name: 'Save reviewed content', exact: true }).click()
    await until(() => pagePuts.length === 3, 'UI explicit rebase accepted')
    assert.equal(pagePuts[2].status, 200)
    assert.equal(pagePuts[2].headers['if-match'], '"' + intervening.body.content_version + '"')
    assert.equal(pagePuts[2].body.current_provenance.reviewed_git_sha, 'e'.repeat(40))
    holding = false
    for (const value of held.splice(0)) value.socket.send(value.message)
    await until(async () => (await page.locator('.observation-detail-content').innerText()).includes('Both browser draft fields survive conflict'), 'UI canonical convergence after delayed reviews')
    await page.locator('#observation-review').click()
    await page.locator('#observation-reviewed-sha').fill('f'.repeat(40))
    const shaOnlyWriter = await api(target, 'PUT', { content: 'Independent write during SHA-only draft', reviewed_git_sha: 'b'.repeat(40) }, { 'If-Match': '"' + pagePuts[2].body.content_version + '"' })
    assert.equal(shaOnlyWriter.status, 200)
    await until(async () => await page.getByRole('button', { name: 'Keep my draft', exact: true }).count() === 1, 'SHA-only draft conflicts on live canonical review')
    assert.equal(await page.locator('#observation-edit-content').inputValue(), 'Both browser draft fields survive conflict')
    assert.equal(await page.locator('#observation-reviewed-sha').inputValue(), 'f'.repeat(40))
    assert.equal(await page.getByRole('button', { name: 'Save reviewed content', exact: true }).count(), 0)
    assert.equal(await page.getByRole('button', { name: 'Record re-audit', exact: true }).isEnabled(), false)
    await page.locator('.observation-card-header .tree-toggle').click()
    await page.getByRole('button', { name: 'Return to draft', exact: true }).click()
    assert.equal(await page.locator('#observation-reviewed-sha').inputValue(), 'f'.repeat(40))
    assert.equal(await page.locator('#observation-edit-content').inputValue(), 'Both browser draft fields survive conflict')
    await page.getByRole('button', { name: 'Cancel', exact: true }).click()
    let historyHead = (await api(target)).body
    for (let i = 0; i < 22; i++) {
      const repeated = await api(target, 'PUT', { content: historyHead.content, reviewed_git_sha: historyHead.current_provenance.reviewed_git_sha }, { 'If-Match': '"' + historyHead.content_version + '"' })
      assert.equal(repeated.status, 200); historyHead = repeated.body
    }
    await until(async () => (await page.locator('.observation-history').innerText()).includes('head ' + historyHead.latest_sequence + '.'), 'Real current assertion head refresh')
    await page.locator('#observation-history-load').click()
    await until(async () => await page.locator('.observation-history-entry').count() === 25, 'Real bounded first revision page')
    await page.locator('#observation-history-load').click()
    await until(async () => await page.locator('.observation-history-entry').count() === historyHead.latest_sequence, 'Real bounded revision continuation')
    const historyAdvance = await api(target, 'PUT', { content: historyHead.content, reviewed_git_sha: historyHead.current_provenance.reviewed_git_sha }, { 'If-Match': '"' + historyHead.content_version + '"' })
    assert.equal(historyAdvance.status, 200)
    await until(async () => await page.locator('.observation-history-entry').count() === 0, 'Live assertion head retires retained history')
    await page.locator('#observation-history-load').click()
    await until(async () => await page.locator('.observation-history-entry').count() === 25, 'History restarts at current head')
    assert.ok((await page.locator('.observation-history').innerText()).includes('Content-bound update'))
    assert.equal(await page.locator('.observation-detail-card h2, .observation-detail-card h3').count(), 0, 'Subject header owns expanded content')
    assert.equal(await page.locator('.observation-card-header .tree-toggle').count(), 1)
    assert.ok(await page.locator('.observation-detail-sha .copyable-value').count())
    assert.equal(await page.locator('#observation-edit, #observation-edit-content').count(), 0)
    assert.deepEqual(errors, [])
    console.log(JSON.stringify({ phase: 'real-conditional-writers-readonly-client', harnessHash, chromium: browser.version(), assetHashes, statuses: [competing.status, stale.status, rebased.status], versionAdvance: true, provenanceSHA256: digest(JSON.stringify(provenance(final.body))), heldFrames: actualHeld.length, heldBytes, heldDigest, canonicalReadConverged: true, rollback: false }))
  } catch (error) {
    primaryFailure = error
    throw error
  } finally {
    const cleanupFailures = []
    const retire = async action => { try { await action() } catch (error) { cleanupFailures.push(error) } }
    if (buildProcess && buildProcess.exitCode === null && buildProcess.signalCode === null) await retire(async () => {
      buildProcess.kill()
      await bounded(buildClosed, 5000, 'Owned fixture build termination')
    })
    for (const value of held.splice(0)) await retire(async () => value.socket.send(value.message))
    holding = false
    if (context) await retire(() => bounded(context.close(), 5000, 'Browser context cleanup'))
    if (writer) await retire(() => bounded(writer.dispose(), 5000, 'Independent writer cleanup'))
    if (browser) await retire(() => bounded(browser.close(), 5000, 'Browser cleanup'))
    if (browserServer) await retire(() => bounded(browserServer.kill(), 5000, 'Browser process cleanup'))
    if (harness) await retire(async () => {
      if (!harness.stdin.writableEnded) harness.stdin.end('\n')
      try { const [code] = await bounded(harnessClosed, 30000, 'Native harness and PostgreSQL cleanup'); assert.equal(code, 0) }
      catch (error) {
        if (harness.exitCode === null) { harness.kill(); await bounded(harnessClosed, 5000, 'Owned harness termination') }
        throw error
      }
    })
    if (postgresPid) await retire(() => until(() => !pidAlive(postgresPid), 'Owned PostgreSQL process absence', 5000))
    if (root) await retire(async () => assert.equal(await exists(root), false, 'Fixture-owned sandbox must be removed at closure'))
    clearTimeout(lifetime)
    if (cleanupFailures.length) throw new AggregateError([...(primaryFailure ? [primaryFailure] : []), ...cleanupFailures], 'Owned fixture cleanup failed; every retirement was attempted')
  }
})
