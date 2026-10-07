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
import { paint, scanObservationRows, revealObservationRow } from './observation-viewport-fixture.mjs'

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

test('production startup profiles carry real multipage versioned snapshots through Elm and canonical replay', { timeout: 600000 }, async () => {
  let harness, harnessClosed, buildProcess, buildClosed, browser, browserServer, context, writer, root, postgresPid, lifetime
  let primaryFailure
  const coordinates = {}
  try {
    const { stdout } = await promisify(execFile)('stack', ['path', '--local-install-root'], { cwd: repo, windowsHide: true, timeout: 30000 })
    const executable = join(stdout.trim(), 'bin', process.platform === 'win32' ? 'hmem-test-harness.exe' : 'hmem-test-harness')
    const harnessHash = digest(await readFile(executable))
    assert.equal(harnessHash, 'c37f5c8f51e258d2af7dc44801f7e340b3926504c1e53174413831462ebb20a1', 'Use the installed harness relinked with the Observation count API')
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
    const createdWorkspace = await api('/api/v1/workspaces', 'POST', { name: 'Populated full snapshot fixture', workspace_type: 'repository' })
    assert.ok([200, 201].includes(createdWorkspace.status))
    const workspace = createdWorkspace.body
    const sha = '0123456789abcdef0123456789abcdef01234567'
    const savedSubjects = [
      [{ subject_kind: 'file', subject: 'src/Main.elm' }, { subject_kind: 'glob', subject: 'src/**/*.elm' }],
      [{ subject_kind: 'file', subject: 'src/View.elm' }, { subject_kind: 'glob', subject: 'src/**/*.elm' }],
      [{ subject_kind: 'glob', subject: 'src/**/*.elm' }, { subject_kind: 'file', subject: 'src/Main.elm' }, { subject_kind: 'file', subject: 'src/View.elm' }]
    ]
    const seed = async (subjects, content) => {
      const result = await api('/api/v1/observations', 'POST', { workspace_id: workspace.id, subjects, git_sha: sha, content })
      assert.ok([200, 201].includes(result.status))
      assert.match(result.body.content_version, /^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/i)
      assert.deepEqual(result.body.subjects, subjects)
      return result.body
    }
    const representatives = []
    for (let i = 0; i < savedSubjects.length; i++) representatives.push(await seed(savedSubjects[i], 'Real populated evidence ' + ['Main', 'View', 'Shared'][i]))
    for (let index = 0; index < 109; index += 4) await Promise.all(Array.from({ length: Math.min(4, 109 - index) }, (_, offset) => {
      const number = index + offset
      return seed([{ subject_kind: 'file', subject: 'docs/Guide-' + number + '.md' }], 'Real guide ' + number)
    }))
    const paths = ['src/View.elm', 'src/Main.elm']
    const realMatch = await api('/api/v1/observations/match', 'POST', { workspace_id: workspace.id, paths, query: 'Real populated evidence', git_sha: sha, offset: 0, limit: 50 })
    assert.equal(realMatch.status, 200)
    assert.equal(realMatch.body.items.length, 3)
    const sharedMatch = realMatch.body.items.find(item => item.observation.id === representatives[2].id)
    assert.deepEqual(sharedMatch.path_matches.map(item => item.path), paths)
    assert.deepEqual(sharedMatch.path_matches.map(item => item.matched_subjects), [
      [savedSubjects[2][0], savedSubjects[2][2]], [savedSubjects[2][0], savedSubjects[2][1]]
    ])
    browserServer = await chromium.launchServer({ headless: true })
    browser = await chromium.connect(browserServer.wsEndpoint())
    const receipts = [], frames = [], contexts = []
    async function createClient(profile) {
      const client = await browser.newContext({ viewport: { width: 1440, height: 900 } })
      contexts.push(client)
      await client.addCookies([{ name: 'hmem_session', value: 'sandbox-session-token', url: origin }, { name: 'hmem_csrf', value: 'sandbox-csrf-token', url: origin }])
      const page = await client.newPage()
      if (profile !== undefined) await page.route('**/hmem-runtime-config.js', async route => {
        const response = await route.fetch()
        await route.fulfill({ response, body: (await response.text()) + '\nwindow.HMEM_CONFIG = { ...(window.HMEM_CONFIG || {}), workspaceSnapshotProfile: ' + JSON.stringify(profile) + ' };' })
      })
      const clientErrors = []
      page.on('pageerror', error => clientErrors.push(error.message))
      const clientReceipts = []
      page.on('response', response => {
        const request = response.request(), path = new URL(response.url()).pathname
        if (request.method() === 'POST' && ['/api/v1/change-stream/resync', '/api/v1/change-stream/ticket'].includes(path)) {
          const receipt = { path, request: request.postDataJSON(), status: response.status(), complete: false }
          clientReceipts.push(receipt); receipts.push(receipt)
          response.json().then(body => { receipt.body = body; receipt.complete = true }, error => clientErrors.push(error.message))
        }
      })
      page.on('websocket', socket => socket.on('framereceived', event => {
        const frame = JSON.parse(event.payload.toString())
        frames.push({ page, type: frame.type, frame })
        assert.ok(frames.length <= 512, 'Real frame receipt remains bounded')
      }))
      return { client, page, receipts: clientReceipts, errors: clientErrors }
    }
    const baseUrl = origin + '/workspace/' + workspace.id
    const defaultClient = await createClient()
    await defaultClient.page.goto(baseUrl + '#tab=observations')
    await until(() => defaultClient.receipts.some(value => value.complete && value.request.scope.workspace_id === workspace.id && value.path.endsWith('/resync')), 'Default shell snapshot')
    const shellPage = defaultClient.receipts.find(value => value.path.endsWith('/resync') && value.request.scope.workspace_id === workspace.id)
    assert.equal(shellPage.request.snapshot_profile, 'workspace_shell_v1')
    assert.equal(shellPage.body.snapshot_profile, 'workspace_shell_v1')
    assert.deepEqual(shellPage.body.items.map(item => item.kind), ['workspace'])
    assert.equal(shellPage.body.has_more, false)
    assert.deepEqual(defaultClient.errors, [])
    await bounded(defaultClient.client.close(), 5000, 'Default client cleanup')
    for (const invalid of ['unknown_profile', null, 7]) {
      const invalidClient = await createClient(invalid)
      await invalidClient.page.goto(baseUrl + '#tab=observations')
      await until(() => invalidClient.errors.length > 0, 'Invalid startup profile rejection', 5000)
      assert.deepEqual(invalidClient.errors, ['HMEM_CONFIG.workspaceSnapshotProfile must be workspace_shell_v1 or full_v1'])
      assert.equal(invalidClient.receipts.length, 0)
      assert.equal(await invalidClient.page.locator('#observation-panel').count(), 0)
      await bounded(invalidClient.client.close(), 5000, 'Invalid client cleanup')
    }
    const fullClient = await createClient('full_v1')
    context = fullClient.client
    const page = fullClient.page
    const heldRest = []
    let releaseRest
    const restGate = new Promise(resolveGate => { releaseRest = resolveGate })
    const holdRest = async route => {
      const response = await route.fetch()
      heldRest.push({ path: new URL(route.request().url()).pathname, status: response.status() })
      assert.ok(heldRest.length <= 8, 'Initial held real REST responses are bounded')
      await restGate
      await route.fulfill({ response })
    }
    await page.route('**/api/v1/observations**', holdRest)
    try {
      await page.goto(baseUrl + '#tab=observations&observation=' + representatives[0].id)
      await until(() => fullClient.receipts.some(value => value.complete && value.path.endsWith('/resync') && value.request.scope.workspace_id === workspace.id && value.body.has_more === false), 'Terminal full snapshot')
      // No Observation REST response has been delivered: this detail must come
      // from the actual accumulated full snapshot through the production port.
      await page.locator('.observation-detail-content').waitFor({ timeout: 5000 })
      assert.equal(await page.locator('.observation-detail-content').innerText(), representatives[0].content)
      assert.ok(heldRest.length > 0)
    } finally { releaseRest(); await page.unroute('**/api/v1/observations**', holdRest) }
    await until(() => fullClient.receipts.some(value => value.complete && value.path.endsWith('/ticket') && value.request.scope.workspace_id === workspace.id), 'Real ticket handoff')
    await until(() => frames.some(value => value.page === page && value.type === 'checkpoint'), 'Real WebSocket checkpoint')
    await until(async () => (await page.locator('.workspace-header .workspace-observation-total').textContent()) === '112 Observations total', 'Real authorized aggregate reaches production workspace information')
    const fullPages = fullClient.receipts.filter(value => value.path.endsWith('/resync') && value.request.scope.workspace_id === workspace.id)
    assert.equal(fullPages.length, 2)
    assert.equal(fullPages[0].request.snapshot_profile, 'full_v1')
    assert.equal(fullPages[0].request.page_size, 100)
    assert.equal(fullPages[1].request.page_token, fullPages[0].body.next_page_token)
    assert.equal(fullPages[0].body.resume_token ?? null, null)
    assert.equal(fullPages[0].body.has_more, true)
    assert.equal(fullPages[1].body.has_more, false)
    const snapshotItems = fullPages.flatMap(value => value.body.items)
    const snapshotObservations = snapshotItems.filter(item => item.kind === 'observation').map(item => item.data)
    assert.equal(snapshotItems.length, 113)
    assert.equal(snapshotObservations.length, 112)
    for (const value of snapshotObservations) assert.match(value.content_version, /^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/i)
    for (const value of representatives) {
      const candidate = snapshotObservations.find(item => item.id === value.id)
      const { created_at: created, updated_at: updated, ...fields } = candidate
      const { created_at: originalCreated, updated_at: originalUpdated, ...originalFields } = value
      assert.deepEqual(fields, originalFields)
      assert.equal(Date.parse(created), Date.parse(originalCreated))
      assert.equal(Date.parse(updated), Date.parse(originalUpdated))
    }
    const ticket = fullClient.receipts.find(value => value.path.endsWith('/ticket') && value.request.scope.workspace_id === workspace.id)
    assert.equal(ticket.request.resume_token, fullPages[1].body.resume_token)
    assert.equal(ticket.status, 200)
    const globalStart = fullClient.receipts.find(value => value.path.endsWith('/resync') && value.request.scope.scope === 'global')
    assert.equal(globalStart.request.snapshot_profile, 'full_v1')
    const original = representatives[0], target = '/api/v1/observations/' + original.id
    const changed = await api(target, 'PUT', { content: 'Real canonical full-profile update' }, { 'If-Match': '"' + original.content_version + '"' })
    assert.equal(changed.status, 200)
    assert.notEqual(changed.body.content_version, original.content_version)
    assert.deepEqual(changed.body.subjects, original.subjects)
    await until(async () => (await page.locator('.observation-detail-content').innerText()).includes(changed.body.content), 'Canonical real update reaches production Elm')
    assert.equal(await page.locator('#observation-edit, #observation-edit-content').count(), 0)
    await page.locator('.observation-detail-content').click()
    assert.equal(await page.locator('[contenteditable=true], #observation-edit-content').count(), 0)
    const applyRoute = async tuple => {
      await page.evaluate(fragment => { location.hash = fragment }, 'tab=observations&ov=1&oq=' + encodeURIComponent(JSON.stringify(tuple)))
    }
    await applyRoute(['facets', 'Real populated evidence', null, '', sha, null, null, []])
    await page.locator('.observation-facet-card').first().waitFor({ timeout: 5000 })
    await applyRoute(['exact', 'Real populated evidence', null, '', sha, 'glob', 'src/**/*.elm', []])
    await until(async () => (await page.locator('.observation-card').count()) === 2, 'Real exact facet results')
    await applyRoute(['match', '', null, '', sha, null, null, paths])
    await until(async () => (await page.locator('.observation-path-group').count()) === 2 && (await page.locator('.observation-subject-group-toggle').count()) === 4, 'Real ordered path groups')
    const groupKeys = await page.locator('[data-observation-group]').evaluateAll(elements => elements.map(element => element.dataset.observationGroup))
    for (const key of groupKeys) {
      const toggle = page.locator('[data-observation-group=' + JSON.stringify(key) + '] .observation-subject-group-toggle')
      await revealObservationRow(page, toggle); await toggle.click(); await paint(page)
    }
    const reached = await scanObservationRows(page)
    assert.equal(reached.cards.size, 10)
    assert.deepEqual([...reached.paths.values()], paths)
    assert.equal([...reached.cards.values()].filter(row => row.id === representatives[2].id).length, 4)
    assert.deepEqual([...reached.groups.values()].map(group => group.label.match(/\d+ loaded/)[0]).sort(), ['2 loaded', '2 loaded', '3 loaded', '3 loaded'])
    const oldStartKey = fullPages[0].request.start_idempotency_key
    const oldSnapshotCount = fullClient.receipts.length
    await page.reload()
    await until(() => fullClient.receipts.slice(oldSnapshotCount).some(value => value.complete && value.path.endsWith('/resync') && value.request.scope.workspace_id === workspace.id && value.request.start_idempotency_key), 'Reload fresh full snapshot')
    const reloadStart = fullClient.receipts.slice(oldSnapshotCount).find(value => value.path.endsWith('/resync') && value.request.scope.workspace_id === workspace.id && value.request.start_idempotency_key)
    assert.notEqual(reloadStart.request.start_idempotency_key, oldStartKey)
    assert.equal(reloadStart.request.snapshot_profile, 'full_v1')
    await until(() => fullClient.receipts.slice(oldSnapshotCount).some(value => value.complete && value.path.endsWith('/resync') && value.request.scope.workspace_id === workspace.id && value.body.has_more === false), 'Reload terminal full snapshot')
    assert.deepEqual(fullClient.errors, [])
    for (const client of contexts) if (client !== context) await bounded(client.close(), 5000, 'Prior client cleanup')
    console.log(JSON.stringify({ phase: 'real-populated-full-profile', harnessHash, chromium: browser.version(), assetHashes, observations: snapshotObservations.length, snapshotItems: snapshotItems.length, pages: fullPages.length, orderedSubjects: true, validVersions: true, terminalTicketLineage: true, selectedFromFullReducerBeforeRest: true, realCanonicalWriterStatus: changed.status, readOnlyBody: true, realMatchedObservations: realMatch.body.items.length, orderedPaths: paths, reloadFreshStart: true, globalProfile: globalStart.request.snapshot_profile }))
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
    console.log(JSON.stringify({ phase: 'real-populated-cleanup', harnessRetired: harness?.exitCode === 0, postgresAbsent: postgresPid ? !pidAlive(postgresPid) : null, sandboxAbsent: root ? !(await exists(root)) : null }))
  }
})
