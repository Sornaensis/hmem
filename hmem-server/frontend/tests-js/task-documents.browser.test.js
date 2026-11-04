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

function outlineStyle(button) {
  return button.evaluate(node => {
    const style = getComputedStyle(node)
    return { background: style.backgroundColor, border: style.borderTop, radius: style.borderRadius,
      color: style.color, fontSize: style.fontSize, padding: style.padding }
  })
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
  let deleteBarrier = null
  let releaseDelete = () => {}
  let releaseFrame = () => {}
  let releaseStalledFrame = () => {}
  let styledReads = 0
  let styledGrants = 0
  let styledNavigations = 0
  const generation = uuid(900)
  let styledGeneration = generation
  const makeOutput = (variant, pageIndex, mediaType, bytes) => ({
    variant, page: pageIndex, media_type: mediaType, raw_bytes: bytes.length, raw_sha256: sha(bytes),
  })
  const styled = Buffer.from('<!doctype html><style>body p:last-child{color:red}</style><p>Sanitized styled wireframe</p><a href="about:srcdoc#visible">Safe link</a><p id="visible">Target</p>')
  const srcdoc = styled.toString().replace(/[&"<>]/g, char => ({ '&': '&amp;', '"': '&quot;', '<': '&lt;', '>': '&gt;' })[char])
  const wrapped = '<!doctype html><body style="margin:0;overflow:hidden"><iframe title="Styled preview" sandbox="" referrerpolicy="no-referrer" style="display:block;width:100%;height:100vh;border:0" srcdoc="' + srcdoc + '"></iframe>'
  const firstFrame = new Promise(resolve => { releaseFrame = resolve })
  const stalledFrame = new Promise(resolve => { releaseStalledFrame = resolve })
  const altHost = 'http://localhost:' + new URL(h.origin).port
  await page.route(altHost + '/document-grants/**', async route => {
    const request = route.request()
    const requestHeaders = await request.allHeaders()
    assert.equal(new URL(request.url()).searchParams.get('view'), 'frame')
    assert.equal(requestHeaders.cookie, undefined)
    assert.equal(requestHeaders.authorization, undefined)
    assert.equal(requestHeaders.referer, undefined)
    const headers = {
      'Content-Security-Policy': "default-src 'none'; script-src 'none'; style-src 'unsafe-inline'; img-src data:; font-src 'none'; connect-src 'none'; frame-src 'none'; child-src 'none'; worker-src 'none'; object-src 'none'; media-src 'none'; base-uri 'none'; form-action 'none'; sandbox; frame-ancestors " + h.origin,
      'Referrer-Policy': 'no-referrer', 'Cache-Control': 'no-store',
    }
    if (++styledNavigations === 1) {
      await firstFrame
      return route.fulfill({ status: 403, headers, body: '' })
    }
    if (styledNavigations === 2) {
      await stalledFrame
      return route.abort().catch(() => {})
    }
    return route.fulfill({ body: wrapped, contentType: 'text/html;charset=utf-8', headers })
  })
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
      if (deleteBarrier) await deleteBarrier
      source = source.filter(row => row !== selected)
      return route.fulfill({ status: 204 })
    }
    if (resource === 'original') {
      const original = selected === image ? png : selected === html ? Buffer.from('<p>original</p>') : Buffer.from('%PDF fixture')
      return route.fulfill({ body: original, contentType: 'application/octet-stream', headers: { 'Content-Disposition': 'attachment' } })
    }
    if (resource === 'download-grants') {
      styledGrants++
      const body = request.postDataJSON()
      assert.equal(body.variant, 'styled_html_preview')
      assert.equal(body.generation_id, styledGeneration)
      assert.equal(body.association_id, body.role === 'source' ? html.id : evidenceHtml.id)
      return route.fulfill({ json: { ...body, url: altHost + '/document-grants/' + uuid(990) + '/styled_html_preview/0?token=fixture' } })
    }
    if (resource === 'previews' && variant === 'current') {
      if (selected === html) styledReads++
      const outputs = selected === html ? [makeOutput('styled_html_preview', 0, 'text/html', styled)]
        : selected === pdf ? [1, 2].map(page => makeOutput('page', page, 'image/png', png))
        : [makeOutput('thumbnail', 0, 'image/png', png), makeOutput('page', 0, 'image/png', png)]
      return route.fulfill({ json: { state: 'ready', generation_id: selected === html ? styledGeneration : generation, outputs, retryable: false, generation_metadata: null } })
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
    await page.clock.install()
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
    const sourcesTab = card.getByRole('button', { name: 'Sources', exact: true })
    await until(async () => (await evidenceTab.innerText()) === 'Evidence · 29')
    assert.equal(await sourcesTab.innerText(), 'Sources · 2')
    assert.equal(await card.getByRole('button', { name: 'Work', exact: true }).getAttribute('aria-pressed'), 'true')
    await sourcesTab.click()
    const sourcePanel = card.locator('#task-' + task + '-sources')
    await sourcePanel.getByText('Fixture uploader', { exact: false }).first().waitFor()
    assert.deepEqual(await layout(), before)
    assert.deepEqual(await page.locator('#entity-' + h.fixture.projects[0].id + ' .card-local-sections button').allTextContents(), [])
    const thumb = sourcePanel.getByRole('button', { name: 'Preview screen.png' })
    await thumb.scrollIntoViewIfNeeded()
    await until(async () => (await thumb.locator('img').getAttribute('src'))?.startsWith('blob:'))
    assert.deepEqual(await thumb.evaluate(node => [node.offsetWidth, node.offsetHeight]), [60, 42])
    assert.equal(await thumb.locator('[data-lucide="zoom-in"]').count(), 1)
    const remove = sourcePanel.getByRole('button', { name: 'Remove from Sources: screen.png' })
    assert.equal(await remove.evaluate(node => node.parentElement.classList.contains('task-document-label')
      && getComputedStyle(node).borderTopStyle === 'none'), true)
    const sourceUpload = sourcePanel.getByRole('button', { name: 'Upload source', exact: true })
    const uploadStyle = await outlineStyle(sourceUpload)
    assert.equal(uploadStyle.background, 'rgba(0, 0, 0, 0)')
    assert.equal(uploadStyle.border, '1px solid rgb(71, 85, 105)')
    await thumb.focus()
    await page.keyboard.press('Enter')
    let dialog = page.getByRole('dialog')
    await dialog.getByRole('img', { name: 'Preview of screen.png' }).waitFor()
    await dialog.getByRole('button', { name: 'Zoom', exact: true }).click()
    await dialog.getByRole('button', { name: 'Fit', exact: true }).waitFor()
    await page.keyboard.press('Escape')
    await dialog.waitFor({ state: 'detached' })
    assert.equal(await thumb.evaluate(node => node === document.activeElement), true)

    await sourceUpload.click()
    dialog = page.getByRole('dialog')
    assert.equal((await dialog.boundingBox()).width, 680)
    await page.mouse.click(4, 4)
    assert.equal(await dialog.isVisible(), true, 'backdrop clicks leave the upload open')
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
    assert.equal(await sourcesTab.innerText(), 'Sources · 3')
    await sourcePanel.getByRole('button', { name: 'Remove from Sources: screen.png' }).click()
    await sourcePanel.getByText('screen.png', { exact: true }).waitFor({ state: 'detached' })
    await sourcePanel.getByText('Preview unavailable — export PNG or PDF').waitFor()
    await until(async () => (await sourcesTab.innerText()) === 'Sources · 2')
    assert.equal(await evidenceTab.innerText(), 'Evidence · 29')

    evidence.push(pdf)
    await push(h, 'task_document', pdf.id, 'created', taskHints(task))
    await until(async () => (await evidenceTab.innerText()) === 'Evidence · 30')
    assert.equal(await sourcesTab.innerText(), 'Sources · 2')
    await evidenceTab.click()
    const documents = card.locator('#task-' + task + '-evidence').getByRole('region', { name: 'Document evidence' })
    await documents.getByText('screen.png', { exact: true }).waitFor()
    assert.equal(await card.getByRole('button', { name: /Upload evidence|Add to evidence/ }).count(), 0)
    const download = documents.locator('.task-document-row').filter({ hasText: 'screen.png' }).getByRole('button', { name: '↓ Download', exact: true })
    const sessionDownload = card.getByRole('region', { name: 'Agent sessions' }).getByRole('button', { name: /^Download session / }).first()
    assert.deepEqual(await outlineStyle(download), await outlineStyle(sessionDownload))
    assert.deepEqual(await outlineStyle(download), uploadStyle)
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
    assert.equal(await styledOrigin.evaluate(node => node.nextElementSibling?.classList.contains('session-download')), true)
    await styledOrigin.click()
    dialog = page.getByRole('dialog')
    assert.deepEqual(await dialog.evaluate(node => {
      const rect = node.getBoundingClientRect(), style = getComputedStyle(node)
      return { center: Math.round((rect.left + rect.right) / 2) === Math.round(innerWidth / 2),
        width: rect.width, background: style.backgroundColor, color: style.color }
    }), { center: true, width: 1120, background: 'rgb(30, 41, 59)', color: 'rgb(248, 250, 252)' })
    const frame = dialog.locator('iframe')
    const stage = dialog.locator('.document-styled-stage')
    const innerFrame = dialog.frameLocator('iframe').frameLocator('iframe')
    const frameContent = innerFrame.getByText('Sanitized styled wireframe')
    await until(() => styledNavigations === 1)
    await dialog.getByText('Loading preview…', { exact: true }).waitFor()
    assert.equal(await frame.isVisible(), false, 'the navigating frame cannot flash before content arrives')
    const stageBefore = await stage.boundingBox()
    releaseFrame()
    const retry = dialog.getByRole('button', { name: 'Retry', exact: true })
    await dialog.getByText('Preview could not be loaded.', { exact: true }).waitFor()
    await retry.waitFor()
    assert.deepEqual(await stage.boundingBox(), stageBefore, 'a failed transfer keeps the loading geometry')
    await retry.focus()
    await page.keyboard.press('Enter')
    await until(() => styledNavigations === 2)
    assert.equal(await frame.isVisible(), false)
    await page.clock.fastForward(30001)
    await dialog.getByText('Preview loading timed out.', { exact: true }).waitFor()
    releaseStalledFrame()
    styledGeneration = uuid(901)
    await retry.click()
    await frameContent.waitFor()
    assert.deepEqual(await stage.boundingBox(), stageBefore, 'successful content replaces the loading surface in place')
    assert.equal(styledReads, 3, 'retry refreshes the generation before requesting its grant')
    assert.equal(styledGrants, 3, 'retry requests a fresh isolated grant')
    assert.equal(styledNavigations, 3)
    assert.equal(await retry.isVisible(), false)
    assert.equal(await frame.getAttribute('sandbox'), '')
    assert.equal(await frame.getAttribute('referrerpolicy'), 'no-referrer')
    assert.equal(await dialog.frameLocator('iframe').locator('iframe').getAttribute('sandbox'), '')
    assert.equal(await innerFrame.getByText('Target').evaluate(node => getComputedStyle(node).color), 'rgb(255, 0, 0)')
    assert.ok((await frame.boundingBox()).height >= 600, 'styled previews use more of the desktop viewport')
    assert.equal(await page.getByText('Sanitized styled wireframe').count(), 0)
    const heading = dialog.getByRole('heading', { name: 'wireframe.html' })
    await heading.click()
    const headingBox = await heading.boundingBox()
    await page.mouse.move(headingBox.x + 4, headingBox.y + 4)
    await page.mouse.down()
    await page.mouse.move(4, 4)
    await page.mouse.up()
    assert.equal(await dialog.isVisible(), true, 'a drag from inside the dialog does not dismiss it')
    const close = dialog.getByRole('button', { name: 'Close', exact: true })
    const link = innerFrame.getByRole('link', { name: 'Safe link' })
    await link.focus()
    await page.keyboard.press('Shift+Tab')
    assert.equal(await close.evaluate(node => node === document.activeElement), true)
    await link.focus()
    await page.keyboard.press('Tab')
    assert.equal(await close.evaluate(node => node === document.activeElement), true)
    // A canonical refresh can remount the invoking node before dismissal.
    const originalOrigin = await styledOrigin.elementHandle()
    const originalFrame = await frame.elementHandle()
    const readsBeforeRefresh = styledReads
    refreshBarrier = new Promise(resolve => { releaseRefresh = resolve })
    await push(h, 'document', pdf.document.id, 'updated', taskHints(task))
    await until(async () => !(await originalOrigin.evaluate(node => node.isConnected)))
    await until(() => styledReads > readsBeforeRefresh)
    releaseRefresh()
    await styledOrigin.waitFor()
    await h.idle()
    assert.equal(await originalFrame.evaluate(node => node.isConnected), true, 'same-generation refresh preserves the live frame')
    assert.equal(await frameContent.isVisible(), true)
    assert.equal(styledGrants, 3)
    assert.equal(styledNavigations, 3)
    await page.keyboard.press('Escape')
    await dialog.waitFor({ state: 'detached' })
    assert.equal(await styledOrigin.evaluate(node => node === document.activeElement), true)

    await styledOrigin.click()
    await link.click()
    assert.equal(await frame.evaluate(node => node === document.activeElement), true)
    await page.mouse.click(4, 4)
    await dialog.waitFor({ state: 'detached' })
    assert.equal(await styledOrigin.evaluate(node => node === document.activeElement), true)

    const desktop = page.viewportSize()
    await page.setViewportSize({ width: 360, height: 520 })
    await styledOrigin.click()
    await frame.waitFor()
    const smallDialog = await dialog.boundingBox()
    const smallClose = await close.boundingBox()
    assert.ok(smallDialog.x > 0 && smallDialog.x + smallDialog.width < 360)
    assert.ok(smallDialog.y > 0 && smallDialog.y + smallDialog.height < 520)
    assert.ok(smallClose.x >= smallDialog.x && smallClose.x + smallClose.width <= smallDialog.x + smallDialog.width)
    assert.ok(smallClose.y >= smallDialog.y && smallClose.y + smallClose.height <= smallDialog.y + smallDialog.height)
    await close.click()
    await dialog.waitFor({ state: 'detached' })
    await page.setViewportSize(desktop)

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
    const mobileActions = documents.getByRole('button', { name: 'Preview review.pdf' })
    assert.equal(await mobileActions.evaluate(node => {
      const actions = node.parentElement, row = actions.parentElement
      return getComputedStyle(row).display === 'grid'
        && getComputedStyle(actions).gridColumn === '1 / -1'
        && actions.getBoundingClientRect().right <= row.getBoundingClientRect().right
    }), true)
    await documents.getByRole('button', { name: 'Newest documents' }).click()
    await styledOrigin.click()
    await frameContent.waitFor()
    evidence = evidence.filter(row => row !== evidenceHtml)
    await push(h, 'task_document', evidenceHtml.id, 'removed', taskHints(task))
    await dialog.waitFor({ state: 'detached' })
    await until(async () => (await evidenceTab.innerText()) === 'Evidence · 29')

    await sourcesTab.click()
    deleteBarrier = new Promise(resolve => { releaseDelete = resolve })
    await sourcePanel.getByRole('button', { name: 'Remove from Sources: diagram-😀.txt' }).click()
    await sourcePanel.getByRole('button', { name: 'Preview wireframe.html' }).click()
    await frameContent.waitFor()
    const sourceFrame = await frame.elementHandle()
    const countsBeforeDelete = [styledGrants, styledNavigations]
    releaseDelete()
    await until(async () => (await sourcesTab.innerText()) === 'Sources · 1')
    await h.idle()
    assert.equal(await sourceFrame.evaluate(node => node.isConnected), true, 'an unrelated local write preserves the live frame')
    assert.deepEqual([styledGrants, styledNavigations], countsBeforeDelete)
    await close.click()
    await dialog.waitFor({ state: 'detached' })
    assert.deepEqual(h.errors, [])
    assert.deepEqual(h.unhandled, [])
  } finally {
    releaseFrame()
    releaseStalledFrame()
    releaseRefresh()
    releaseDelete()
    await h.close()
  }
})

test('Source tabs distinguish pending counts from zero and keep each canonical total through refresh', { timeout: 45000 }, async () => {
  const h = distinctTasks(await openSessionExplorer())
  const { page } = h
  const [first, second] = h.fixture.tasks
  const source = item(first.id, 20, 'source', 'visible.txt', 'text/plain', Buffer.from('sample source'))
  source.document.preview_state = 'unsupported'
  let firstTotal = 5, firstRequested = false, releaseFirst
  const firstRead = new Promise(resolve => { releaseFirst = resolve })
  await page.route('**/tasks/*/documents/counts', async route => {
    const isFirst = new URL(route.request().url()).pathname.endsWith('/' + first.id + '/documents/counts')
    if (isFirst) {
      firstRequested = true
      await firstRead
    }
    return route.fulfill({ json: counts(isFirst ? firstTotal : 0, 0, 0) })
  })
  await page.route('**/tasks/*/documents?**', route => {
    const isFirst = new URL(route.request().url()).pathname.endsWith('/' + first.id + '/documents')
    return route.fulfill({ json: { items: isFirst ? [source] : [], next_cursor: null } })
  })

  try {
    await h.start()
    await h.idle()
    const firstCard = page.locator('#entity-' + first.id)
    const secondCard = page.locator('#entity-' + second.id)
    const firstTab = firstCard.getByRole('button', { name: 'Sources', exact: true })
    const secondTab = secondCard.getByRole('button', { name: 'Sources', exact: true })
    await firstCard.locator('.card-details-control').click()
    await until(() => firstRequested)
    assert.equal(await firstTab.innerText(), 'Sources')
    assert.equal(await firstTab.isEnabled(), true)
    await secondCard.locator('.card-details-control').click()
    await until(async () => (await secondTab.innerText()) === 'Sources · 0')
    await secondTab.click()
    await secondCard.getByText('No Sources.', { exact: true }).waitFor()
    releaseFirst()
    await until(async () => (await firstTab.innerText()) === 'Sources · 5')
    assert.equal(await firstTab.getAttribute('aria-pressed'), 'false')
    await firstTab.click()
    await firstCard.getByText('visible.txt', { exact: true }).waitFor()
    assert.equal(await firstTab.innerText(), 'Sources · 5', 'the canonical total is independent of visible rows')
    assert.equal(await firstTab.getAttribute('aria-description'), 'Sources · 5')

    firstTotal = 7
    await push(h, 'task_document', source.id, 'created', taskHints(first.id))
    await until(async () => (await firstTab.innerText()) === 'Sources · 7')
    assert.equal(await secondTab.innerText(), 'Sources · 0')

    firstTotal = 8
    const resync = page.waitForResponse(response => new URL(response.url()).pathname === '/api/v1/change-stream/resync')
    await page.evaluate(() => window.pushHierarchyFrames([{ schema_version: 1, type: 'resync_required' }]))
    await resync
    await until(async () => (await firstTab.innerText()) === 'Sources · 8')
    assert.equal(await firstTab.getAttribute('aria-pressed'), 'true')
    assert.equal(await secondTab.innerText(), 'Sources · 0')
    assert.deepEqual(h.errors, [])
  } finally {
    releaseFirst()
    await h.close()
  }
})

test('MP4 Source uploads and both task panels use a static first frame and keep the original downloadable', { timeout: 45000 }, async () => {
  const h = distinctTasks(await openSessionExplorer({ savedCount: 0 }))
  const { page } = h
  const task = h.fixture.tasks[0].id
  const endpoint = '**/api/v1/workspaces/*/tasks/' + task + '/documents'
  const image = item(task, 21, 'source', 'screen.png', 'image/png')
  const original = Buffer.from('fixture MP4 original bytes')
  const video = item(task, 22, 'source', 'walkthrough.MP4', 'video/mp4', original)
  const evidenceVideo = { ...video, id: uuid(322), role: 'evidence' }
  const failedVideo = item(task, 23, 'evidence', 'unavailable.mp4', 'video/mp4', original)
  failedVideo.document.preview_state = 'failed'
  let uploaded = false
  const poster = Buffer.from(await page.evaluate(() => {
    const canvas = document.createElement('canvas')
    canvas.width = 4
    canvas.height = 2
    const ctx = canvas.getContext('2d')
    ctx.fillStyle = 'tomato'
    ctx.fillRect(0, 0, 4, 2)
    return canvas.toDataURL().split(',')[1]
  }), 'base64')
  const generation = uuid(902)

  await page.route(endpoint + '**', async route => {
    const request = route.request()
    const url = new URL(request.url())
    const suffix = url.pathname.split('/documents')[1]
    if (suffix === '/counts') return route.fulfill({ json: counts(uploaded ? 2 : 1, 2, 0) })
    if (suffix === '' && request.method() === 'POST') {
      const metadata = JSON.parse(request.headers()['x-hmem-document-metadata'])
      assert.equal(metadata.role, 'source')
      assert.equal(metadata.manifest.media_type, 'video/mp4')
      assert.equal(metadata.manifest.filename, video.document.filename)
      assert.equal(metadata.manifest.raw_sha256, sha(original))
      assert.deepEqual(request.postDataBuffer(), original)
      uploaded = true
      return route.fulfill({ status: 201, json: video })
    }
    if (suffix === '') {
      const rows = url.searchParams.get('role') === 'source' ? (uploaded ? [image, video] : [image]) : [evidenceVideo, failedVideo]
      return route.fulfill({ json: { items: rows, next_cursor: null } })
    }
    const [, doc, resource, version, variant, rasterPage] = suffix.split('/')
    const raster = doc === image.document.id ? png : poster
    if (resource === 'original') return route.fulfill({ body: original, contentType: 'application/octet-stream' })
    if (version === 'current') {
      if (doc === failedVideo.document.id) {
        return route.fulfill({ json: { state: 'failed', generation_id: null, outputs: [], retryable: false, generation_metadata: null } })
      }
      const outputs = ['thumbnail', 'page'].map(name => ({
        variant: name, page: 0, media_type: 'image/png', raw_bytes: raster.length, raw_sha256: sha(raster),
      }))
      return route.fulfill({ json: { state: 'ready', generation_id: generation, outputs, retryable: false, generation_metadata: null } })
    }
    assert.equal(resource, 'previews')
    assert.equal(version, generation)
    assert.ok(['thumbnail', 'page'].includes(variant))
    assert.equal(rasterPage, '0')
    return route.fulfill({ body: raster, contentType: 'image/png' })
  })

  try {
    await h.start()
    await h.idle()
    const card = page.locator('#entity-' + task)
    await card.locator('.card-details-control').click()
    await card.getByRole('button', { name: 'Sources', exact: true }).click()
    const sources = card.getByRole('region', { name: 'Sources' })
    await sources.getByRole('button', { name: 'Upload source' }).click()
    const upload = page.getByRole('dialog', { name: 'Upload source' })
    const file = upload.getByLabel('File')
    assert.ok((await file.getAttribute('accept')).split(',').includes('.mp4'))
    await file.setInputFiles({ name: video.document.filename, mimeType: 'application/octet-stream', buffer: original })
    await upload.getByRole('button', { name: 'Upload source' }).click()
    await upload.waitFor({ state: 'detached' })

    const decodedWidth = async raster => {
      await raster.scrollIntoViewIfNeeded()
      await until(() => raster.evaluate(img => img.complete && img.naturalWidth > 0))
      return raster.evaluate(img => img.naturalWidth)
    }
    assert.equal(await decodedWidth(sources.getByRole('button', { name: 'Preview screen.png' }).locator('img')), 1)
    await page.setViewportSize({ width: 390, height: 1000 })
    const documents = card.getByRole('region', { name: 'Document evidence' })
    for (const panel of [sources, documents]) {
      if (panel === documents) await card.getByRole('button', { name: 'Evidence', exact: true }).click()
      await panel.getByText('Video thumbnail · first frame').waitFor()
      const thumbnail = panel.getByRole('button', { name: 'Preview walkthrough.MP4' })
      assert.equal(await decodedWidth(thumbnail.locator('img')), 4)
      await thumbnail.focus()
      await page.keyboard.press('Enter')
      const dialog = page.getByRole('dialog', { name: 'walkthrough.MP4' })
      await dialog.getByText('Video preview · first frame shown').waitFor()
      assert.equal(await decodedWidth(dialog.getByRole('img', { name: 'Preview of walkthrough.MP4' })), 4)
      assert.equal(await dialog.locator('video, source, iframe').count(), 0)
      await page.keyboard.press('Escape')
      await dialog.waitFor({ state: 'detached' })
      assert.equal(await thumbnail.evaluate(node => document.activeElement === node), true)
    }
    const failedRow = documents.locator('.task-document-row').filter({ hasText: 'unavailable.mp4' })
    await failedRow.getByText('Preview failed').waitFor()
    assert.equal(await failedRow.locator('img').count(), 0)
    const downloaded = page.waitForEvent('download')
    const originalResponse = page.waitForResponse(response => response.url().endsWith('/' + failedVideo.document.id + '/original'))
    await failedRow.getByRole('button', { name: '↓ Download' }).click()
    assert.equal((await downloaded).suggestedFilename(), 'unavailable.mp4')
    assert.deepEqual(await (await originalResponse).body(), original)
    assert.deepEqual(h.errors, [])
    assert.deepEqual(h.unhandled, [])
  } finally {
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

