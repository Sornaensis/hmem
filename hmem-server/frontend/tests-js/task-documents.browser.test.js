import assert from 'node:assert/strict'
import { createHash } from 'node:crypto'
import { mkdtemp, readFile, rm, rmdir } from 'node:fs/promises'
import { tmpdir } from 'node:os'
import { join } from 'node:path'
import test from 'node:test'
import { openSessionExplorer } from './support/sessions-ux-fixture.mjs'

const png = Buffer.from('iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mP8/x8AAwMCAO+a4d0AAAAASUVORK5CYII=', 'base64')
const stamp = '2026-11-03T18:00:00Z'
const uuid = number => '41000000-0000-4000-8000-' + String(number).padStart(12, '0')
const sha = bytes => createHash('sha256').update(bytes).digest('hex')
const pause = ms => new Promise(resolve => setTimeout(resolve, ms))
async function until(fn) {
  for (let end = Date.now() + 12000; Date.now() < end; ) {
    if (await fn()) return
    await pause(25)
  }
  assert.fail('Document UI did not converge')
}

function item(task, number, role, filename, mediaType, original = png) {
  return { id: uuid(number + 100), task_id: task, role, created_at: stamp, document: {
    id: uuid(number), filename, title: filename, media_type: mediaType, raw_bytes: original.length,
    raw_sha256: sha(original), created_at: stamp, uploader: { actor_label: 'Fixture uploader' }, preview_state: 'ready',
  } }
}

async function push(h, type, id, action, invalidations) {
  await h.page.evaluate(({ workspace, type, id, action, invalidations }) => {
    window.pushHierarchyFrames([{ schema_version: 1, type: 'change', event: {
      schema_version: 1, event_id: crypto.randomUUID(), scope: 'workspace', workspace_id: workspace,
      occurred_at: new Date().toISOString(), transaction: { id: crypto.randomUUID(), cause: 'rest', request_id: null },
      actor: { type: 'system', id: null }, entity: { type, id, action }, invalidations,
    } }])
  }, { workspace: h.fixture.workspace.id, type, id, action, invalidations })
}

const taskHints = task => [
  { kind: 'entity', target: 'task:' + task },
  { kind: 'collection', target: 'task_documents:' + task },
]
const counts = (source, documents, sessions) => ({
  source_count: source, document_evidence_count: documents, agent_session_evidence_count: sessions, evidence_total: documents + sessions,
})

function distinctTasks(h) {
  const map = new Map(h.fixture.tasks.map(task => [task.id, task.id.replace(/^10000000/, '11000000')]))
  for (const task of h.fixture.tasks) task.id = map.get(task.id)
  for (const [session, tasks] of h.sessionLinks) h.sessionLinks.set(session, new Set([...tasks].map(task => map.get(task) || task)))
  return h
}

test('Sources and paged evidence share derivatives and original downloads in the existing hierarchy card', { timeout: 80000 }, async () => {
  const h = distinctTasks(await openSessionExplorer({ savedCount: 27 }))
  const { page } = h
  const task = h.fixture.tasks[0].id
  const documentBase = '**/api/v1/workspaces/*/tasks/' + task + '/documents'
  const image = item(task, 1, 'source', 'screen.png', 'image/png')
  const html = item(task, 2, 'source', 'wireframe.html', 'text/html', Buffer.from('<p>original</p>'))
  const pdf = item(task, 3, 'evidence', 'review.pdf', 'application/pdf', Buffer.from('%PDF fixture'))
  const evidenceImage = { ...image, id: uuid(301), role: 'evidence' }
  const evidenceHtml = { ...html, id: uuid(302), role: 'evidence' }
  let source = [image, html]
  let evidence = [evidenceImage, evidenceHtml]
  const docs = [image, html, pdf]
  const uploads = []
  let countReads = 0
  let refreshBarrier = null
  let releaseRefresh = () => {}
  const generation = uuid(900)
  const makeOutput = (variant, pageIndex, mediaType, bytes) => ({
    variant, page: pageIndex, media_type: mediaType, raw_bytes: bytes.length, raw_sha256: sha(bytes),
  })
  const styled = Buffer.from('<!doctype html><p>Sanitized styled wireframe</p><a href="#visible">Safe link</a><p id="visible">Target</p>')
  const altHost = 'http://localhost:' + new URL(h.origin).port
  await page.route(altHost + '/document-grants/**', route => route.fulfill({
    body: styled, contentType: 'text/html',
    headers: { 'Content-Security-Policy': "sandbox; default-src 'none'; script-src 'none'; frame-ancestors " + h.origin, 'Referrer-Policy': 'no-referrer', 'Cache-Control': 'no-store' },
  }))
  await page.route(documentBase + '**', async route => {
    const request = route.request()
    const url = new URL(request.url())
    const suffix = url.pathname.split('/documents')[1]
    if (suffix === '/counts') {
      countReads++
      return route.fulfill({ json: counts(source.length, evidence.length, 27) })
    }
    if (suffix === '' && request.method() === 'POST') {
      const encoded = request.headers()['x-hmem-document-metadata']
      const manifest = JSON.parse(encoded)
      uploads.push(manifest)
      assert.ok(!/[^\x00-\x7f]/.test(encoded))
      assert.equal(manifest.role, 'source')
      assert.equal(manifest.manifest.filename, 'diagram-😀.txt')
      assert.equal(manifest.manifest.raw_sha256, sha(request.postDataBuffer()))
      if (uploads.length === 1) return route.fulfill({ status: 503, json: { error: 'temporary' } })
      const upload = item(task, 4, 'source', manifest.manifest.filename, manifest.manifest.media_type, request.postDataBuffer())
      upload.document.preview_state = 'unsupported'
      source = [upload, ...source]
      docs.push(upload)
      return route.fulfill({ status: 201, json: upload })
    }
    if (suffix === '') {
      const rows = url.searchParams.get('role') === 'source' ? source : evidence
      if (rows === evidence && refreshBarrier) await refreshBarrier
      if (rows === evidence && rows.length > 2 && !url.searchParams.has('before_id')) {
        return route.fulfill({ json: { items: rows.slice(0, 2), next_cursor: { id: rows[1].id, created_at: stamp } } })
      }
      return route.fulfill({ json: { items: url.searchParams.has('before_id') ? rows.slice(2) : rows, next_cursor: null } })
    }
    const [docId, resource, variant, pageIndex] = suffix.slice(1).split('/')
    const selected = docs.find(row => row.document.id === docId)
    assert.ok(selected)
    if (resource === 'source' && request.method() === 'DELETE') {
      source = source.filter(row => row !== selected)
      return route.fulfill({ status: 204 })
    }
    if (resource === 'original') {
      const original = selected === image ? png : selected === html ? Buffer.from('<p>original</p>') : Buffer.from('%PDF fixture')
      return route.fulfill({ body: original, contentType: 'application/octet-stream', headers: { 'Content-Disposition': 'attachment' } })
    }
    if (resource === 'download-grants') {
      const body = request.postDataJSON()
      assert.equal(body.variant, 'styled_html_preview')
      assert.equal(body.generation_id, generation)
      assert.equal(body.association_id, body.role === 'source' ? html.id : evidenceHtml.id)
      return route.fulfill({ json: { ...body, url: altHost + '/document-grants/' + uuid(990) + '/styled_html_preview/0?token=fixture' } })
    }
    if (resource === 'previews' && variant === 'current') {
      const outputs = selected === html ? [makeOutput('styled_html_preview', 0, 'text/html', styled)]
        : selected === pdf ? [1, 2].map(page => makeOutput('page', page, 'image/png', png))
        : [makeOutput('thumbnail', 0, 'image/png', png), makeOutput('page', 0, 'image/png', png)]
      return route.fulfill({ json: { state: 'ready', generation_id: generation, outputs, retryable: false, generation_metadata: null } })
    }
    assert.equal(variant, generation)
    assert.ok(['page', 'thumbnail'].includes(pageIndex))
    return route.fulfill({ body: png, contentType: 'image/png' })
  })
  await page.route('**/api/v1/**/tasks/' + task + '/sessions?**', route => {
    const params = new URL(route.request().url()).searchParams
    if (params.get('state') !== 'terminated') return route.fallback()
    const older = params.has('before_id'), rows = h.savedSessions
    return route.fulfill({ json: { items: older ? rows.slice(20) : rows.slice(0, 20),
      next_cursor: older ? null : { id: rows[19].id, created_at: rows[19].created_at } } })
  })

  try {
    await h.start()
    await h.idle()
    const card = page.locator('#entity-' + task)
    const layout = () => card.evaluate(root => {
      const header = root.querySelector(':scope > .card-header')
      return { classes: root.className, header: [...header.children].map(child => child.className),
        controls: [...header.querySelectorAll('button')].map(button => button.className),
        width: Math.round(root.getBoundingClientRect().width) }
    })
    const before = await layout()
    await card.locator('.card-details-control').click()
    const evidenceTab = card.getByRole('button', { name: 'Evidence', exact: true })
    await until(async () => (await evidenceTab.innerText()) === 'Evidence · 29')
    assert.equal(await card.getByRole('button', { name: 'Work', exact: true }).getAttribute('aria-pressed'), 'true')
    await card.getByRole('button', { name: 'Sources', exact: true }).click()
    const sourcePanel = card.locator('#task-' + task + '-sources')
    await sourcePanel.getByText('Fixture uploader', { exact: false }).first().waitFor()
    assert.deepEqual(await layout(), before)
    assert.deepEqual(await page.locator('#entity-' + h.fixture.projects[0].id + ' .card-local-sections button').allTextContents(), [])
    const thumb = sourcePanel.getByRole('button', { name: 'Preview screen.png' })
    await thumb.scrollIntoViewIfNeeded()
    await until(async () => (await thumb.locator('img').getAttribute('src'))?.startsWith('blob:'))
    await thumb.focus()
    await page.keyboard.press('Enter')
    let dialog = page.getByRole('dialog')
    await dialog.getByRole('img', { name: 'Preview of screen.png' }).waitFor()
    await dialog.getByRole('button', { name: 'Zoom', exact: true }).click()
    await dialog.getByRole('button', { name: 'Fit', exact: true }).waitFor()
    await page.keyboard.press('Escape')
    await dialog.waitFor({ state: 'detached' })
    assert.equal(await thumb.evaluate(node => node === document.activeElement), true)

    await sourcePanel.getByRole('button', { name: 'Upload source', exact: true }).click()
    dialog = page.getByRole('dialog')
    assert.equal(await dialog.locator('select').count(), 0)
    await dialog.getByLabel('File').setInputFiles({ name: 'diagram-😀.txt', mimeType: 'text/plain', buffer: Buffer.from('verified original') })
    await dialog.getByRole('button', { name: 'Upload source', exact: true }).click()
    await dialog.getByText('The document operation could not finish. Please retry.').waitFor()
    await dialog.getByRole('button', { name: 'Upload source', exact: true }).click()
    await dialog.waitFor({ state: 'detached' })
    assert.equal(uploads.length, 2)
    assert.equal(uploads[0].request_id, uploads[1].request_id)
    await sourcePanel.getByText('diagram-😀.txt', { exact: true }).waitFor()
    await until(async () => (await evidenceTab.innerText()) === 'Evidence · 29')
    await sourcePanel.getByRole('heading', { name: 'Sources · 3' }).waitFor()
    await sourcePanel.getByRole('button', { name: 'Remove from Sources: screen.png' }).click()
    await sourcePanel.getByText('screen.png', { exact: true }).waitFor({ state: 'detached' })
    await sourcePanel.getByText('Preview unavailable — export PNG or PDF').waitFor()

    evidence.push(pdf)
    await push(h, 'task_document', pdf.id, 'created', taskHints(task))
    await until(async () => (await evidenceTab.innerText()) === 'Evidence · 30')
    await evidenceTab.click()
    const documents = card.locator('#task-' + task + '-evidence').getByRole('region', { name: 'Document evidence' })
    await documents.getByText('screen.png', { exact: true }).waitFor()
    assert.equal(await card.getByRole('button', { name: /Upload evidence|Add to evidence/ }).count(), 0)
    const download = documents.locator('.task-document-row').filter({ hasText: 'screen.png' }).getByRole('button', { name: '↓ Download', exact: true })
    const pendingDownload = page.waitForEvent('download')
    await download.click()
    const receipt = await pendingDownload
    assert.equal(receipt.suggestedFilename(), 'screen.png')
    const scratch = await mkdtemp(join(tmpdir(), 'hmem-document-download-'))
    const received = join(scratch, 'screen.png')
    try {
      await receipt.saveAs(received)
      assert.deepEqual(await readFile(received), png)
    } finally {
      await rm(received, { force: true })
      await rmdir(scratch)
    }

    const styledOrigin = documents.getByRole('button', { name: 'Preview wireframe.html' })
    await styledOrigin.click()
    dialog = page.getByRole('dialog')
    const frame = dialog.locator('iframe')
    assert.equal(await frame.getAttribute('sandbox'), '')
    assert.equal(await frame.getAttribute('referrerpolicy'), 'no-referrer')
    await dialog.frameLocator('iframe').getByText('Sanitized styled wireframe').waitFor()
    assert.equal(await page.getByText('Sanitized styled wireframe').count(), 0)
    const close = dialog.getByRole('button', { name: 'Close', exact: true })
    const link = dialog.frameLocator('iframe').getByRole('link', { name: 'Safe link' })
    await link.focus()
    await page.keyboard.press('Shift+Tab')
    assert.equal(await close.evaluate(node => node === document.activeElement), true)
    await link.focus()
    await page.keyboard.press('Tab')
    assert.equal(await close.evaluate(node => node === document.activeElement), true)
    // A canonical refresh can remount the invoking node before dismissal.
    const originalOrigin = await styledOrigin.elementHandle()
    refreshBarrier = new Promise(resolve => { releaseRefresh = resolve })
    await push(h, 'document', html.document.id, 'updated', taskHints(task))
    await until(async () => !(await originalOrigin.evaluate(node => node.isConnected)))
    releaseRefresh()
    await styledOrigin.waitFor()
    await close.click()
    await dialog.waitFor({ state: 'detached' })
    assert.equal(await styledOrigin.evaluate(node => node === document.activeElement), true)

    await documents.getByRole('button', { name: 'Older documents' }).click()
    await documents.getByRole('button', { name: 'Preview review.pdf' }).click()
    dialog = page.getByRole('dialog')
    await dialog.getByText('Page 1 of 2').waitFor()
    await dialog.getByRole('button', { name: 'Next page' }).click()
    await dialog.getByText('Page 2 of 2').waitFor()
    await dialog.getByRole('button', { name: 'Close', exact: true }).click()
    await card.getByRole('button', { name: 'Older sessions', exact: true }).click()
    await card.getByRole('heading', { name: 'Agent sessions · 27' }).waitFor()
    assert.equal(await evidenceTab.innerText(), 'Evidence · 30')

    await h.idle()
    const priorReads = countReads
    await h.appendRecords({ type: 'message', text: 'ordinary append' })
    await h.idle()
    assert.equal(countReads, priorReads, 'ordinary append must not invalidate document counts')
    await page.setViewportSize({ width: 360, height: 900 })
    const mobile = await layout()
    assert.deepEqual(mobile.header, before.header)
    assert.deepEqual(mobile.controls, before.controls)
    assert.ok(await page.evaluate(() => document.documentElement.scrollWidth <= window.innerWidth))
    assert.deepEqual(h.errors, [])
    assert.deepEqual(h.unhandled, [])
  } finally {
    releaseRefresh()
    await h.close()
  }
})

test('raster transport honors the authorized type and size and stale 401 cannot retire a new credential', { timeout: 45000 }, async () => {
  const h = distinctTasks(await openSessionExplorer({ savedCount: 0 }))
  const fixture = h.fixture
  const { page } = h
  const task = fixture.tasks[0].id
  const row = item(task, 30, 'source', 'large.jpg', 'image/jpeg')
  const generation = uuid(931)
  let raster
  let mismatch = false
  await page.route('**/hmem-runtime-config.js', route => route.fulfill({
    contentType: 'application/javascript',
    body: "window.HMEM_CONFIG={authMode:'local',apiUrl:window.location.origin,wsUrl:'ws://fixture.invalid/api/v1/ws',authTokenStorage:'local'}",
  }))
  await page.addInitScript(() => {
    const realFetch = window.fetch.bind(window)
    let intercepted = false
    window.fetch = (url, options) => {
      if (!intercepted && String(url).endsWith('/original')) {
        intercepted = true
        // Simulate a reply already dispatched by the old authority. Abort is
        // deliberately ignored so the bridge's post-response fence is tested.
        return new Promise(resolve => { window.releaseStaleDocument401 = () => resolve(new Response('', { status: 401 })) })
      }
      return realFetch(url, options)
    }
  })
  await page.route('**/api/v1/workspaces/*/tasks/' + task + '/documents**', async route => {
    const url = new URL(route.request().url())
    if (url.pathname.endsWith('/counts')) return route.fulfill({ json: counts(1, 0, 0) })
    if (url.pathname.endsWith('/documents')) return route.fulfill({ json: { items: [row], next_cursor: null } })
    if (url.pathname.endsWith('/current')) return route.fulfill({ json: {
      state: 'ready', generation_id: generation, retryable: false, generation_metadata: null,
      outputs: [{ variant: 'page', page: 0, media_type: 'image/jpeg', raw_bytes: raster.length, raw_sha256: sha(raster) }],
    } })
    assert.ok(url.pathname.endsWith('/' + generation + '/page/0'))
    return route.fulfill({ contentType: mismatch ? 'text/html' : 'image/jpeg', body: raster })
  })
  try {
    await h.start()
    const jpeg = Buffer.from(await page.evaluate(() => {
      const canvas = document.createElement('canvas')
      canvas.width = canvas.height = 1
      canvas.getContext('2d').fillRect(0, 0, 1, 1)
      return canvas.toDataURL('image/jpeg').split(',')[1]
    }), 'base64')
    const padding = Buffer.alloc(60004)
    padding.writeUInt16BE(0xffef, 0)
    padding.writeUInt16BE(60002, 2)
    raster = Buffer.concat([jpeg.subarray(0, 2), ...Array(360).fill(padding), jpeg.subarray(2)])
    assert.ok(raster.length > 20 * 1024 * 1024)
    const card = page.locator('#entity-' + task)
    await card.locator('.card-details-control').click()
    await card.getByRole('button', { name: 'Sources', exact: true }).click()
    await card.getByRole('button', { name: '↓ Download', exact: true }).click()
    await page.waitForFunction(() => typeof window.releaseStaleDocument401 === 'function')
    const authReads = () => h.requests.filter(request => request.path === '/api/v1/session').length
    const oldReads = authReads()
    h.setPrincipal(uuid(997))
    await page.evaluate(() => {
      const key = 'hmem-auth-token', value = 'replacement-fixture-credential'
      localStorage.setItem(key, value)
      window.dispatchEvent(new StorageEvent('storage', { key, newValue: value }))
    })
    await until(() => authReads() > oldReads)
    await h.idle()
    await page.getByRole('button', { name: 'Expand All', exact: true }).click()
    await h.idle()
    await h.scrollTo('task:' + task)
    const sources = card.getByRole('button', { name: 'Sources', exact: true })
    if (!await sources.isVisible()) await card.locator('.card-details-control').click()
    await sources.click()
    await card.getByRole('button', { name: '↓ Download', exact: true }).waitFor()
    const before = authReads()
    await page.evaluate(() => window.releaseStaleDocument401())
    await h.idle()
    assert.equal(authReads(), before)
    assert.equal(await card.getByRole('button', { name: 'Sources', exact: true }).getAttribute('aria-pressed'), 'true')

    const thumbnail = card.getByRole('button', { name: 'Preview large.jpg' })
    await thumbnail.click()
    const dialog = page.getByRole('dialog')
    const image = dialog.getByRole('img', { name: 'Preview of large.jpg' })
    await until(async () => (await image.getAttribute('src'))?.startsWith('blob:'))
    assert.deepEqual(await image.evaluate(async node => {
      const data = await (await fetch(node.src)).blob()
      return { bytes: data.size, type: data.type }
    }), { bytes: raster.length, type: 'image/jpeg' })
    await until(() => image.evaluate(node => node.naturalWidth === 1))
    await dialog.getByRole('button', { name: 'Close', exact: true }).click()
    mismatch = true
    await thumbnail.click()
    await dialog.getByText('Preview is unavailable. You can download the original.').waitFor()
    assert.equal(await image.getAttribute('src'), null)
    assert.equal(await dialog.getByRole('button', { name: '↓ Download', exact: true }).isEnabled(), true)
    assert.deepEqual(h.errors, [])
  } finally {
    await h.close()
  }
})


test('expanded cards finish multiple shared detail waves with no geometry changes', { timeout: 40000 }, async () => {
  const h = distinctTasks(await openSessionExplorer({ linkedCount: 12, savedCount: 0 }))
  await h.page.setViewportSize({ width: 1440, height: 2800 })
  const countReads = new Set()
  let active = 0
  let peak = 0
  await h.page.route('**/api/v1/workspaces/*/tasks/*/documents/counts', async route => {
    countReads.add(new URL(route.request().url()).pathname.split('/').at(-3))
    peak = Math.max(peak, ++active)
    try {
      await pause(75)
      await route.fulfill({ json: counts(0, 1, 0) })
    } finally {
      active--
    }
  })
  try {
    await h.start()
    await h.idle()
    // Keep mounted row geometry stable when the card disclosure changes so
    // later paint credits must arrive without help from a resize callback.
    await h.page.addStyleTag({ content: '.task-card .card-section-panel { display: none !important }' })
    const rows = h.page.locator('.task-card .card-details-control[aria-expanded="false"]')
    const ids = await rows.evaluateAll(nodes => nodes.slice(0, 9).map(node => node.id))
    assert.equal(ids.length, 9)
    const taskIds = ids.map(id => id.replace(/^task-/, '').replace(/-details-toggle$/, ''))
    await h.page.evaluate(values => values.forEach(id => document.getElementById(id)?.click()), ids)
    try {
      await until(() => taskIds.every(id => countReads.has(id)))
    } catch (error) {
      const state = await h.page.evaluate(values => ({
        expanded: values.map(id => document.getElementById(id)?.getAttribute('aria-expanded')),
        layout: document.getElementById('hierarchy-viewport')?.dataset.hierarchyContext,
      }), ids)
      throw new Error(JSON.stringify({ expected: taskIds, requests: [...countReads], peak, state }), { cause: error })
    }
    await h.idle()
    assert.ok(await h.page.getByText('Evidence · 1', { exact: true }).count() >= ids.length)
    assert.ok(peak > 0 && peak <= 6)
  } finally {
    await h.close()
  }
})

