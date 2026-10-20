import assert from 'node:assert/strict'
import test from 'node:test'
import { mkdir, stat } from 'node:fs/promises'
import { dirname } from 'node:path'
import { hierarchyFixture, openHierarchy } from './hierarchy-fixture.mjs'
import { openDiscovery } from './observation-discovery-fixture.mjs'

const empty = { projects: [], tasks: [], observations: [] }
const search = h => h.page.getByRole('searchbox', { name: 'Search all entities', exact: true })
const resultsReady = h => h.page.waitForFunction(() => document.querySelector('.search-results-title') && !document.querySelector('.search-results-state'))
const respond = (route, body) => route.fulfill({ contentType: 'application/json', body: JSON.stringify(body) })
const buttonId = (kind, id) => 'search-jump-' + kind + '-' + id
const focused = (h, id) => h.page.waitForFunction(id => document.activeElement?.id === id, id, { timeout: 10000 })
function fixture() {
  const f = hierarchyFixture(), root = f.projects[0], task = f.tasks[0]
  f.projects = [root, ...Array.from({ length: 70 }, (_, index) => ({ ...root, id: 'deep-project-' + index, parent_id: index ? 'deep-project-' + (index - 1) : root.id, name: 'Deep project ' + index, status: 'completed', description: 'Long project content '.repeat(100) }))]
  f.tasks = Array.from({ length: 10 }, (_, index) => ({ ...task, id: 'hidden-task-' + index, project_id: 'deep-project-69', parent_id: null, title: 'Hidden task ' + index, status: 'done', description: 'Long task content '.repeat(100) }))
  return f
}
function searchPayload(f) {
  return { projects: f.projects.slice(-10), tasks: f.tasks, observations: [{ id: 'preview-observation', workspace_id: f.workspace.id, subject_kind: 'file', subject: 'src/' + 'long-path/'.repeat(30) + 'Main.elm', git_sha: '0123456789abcdef0123456789abcdef01234567', content_preview: 'Observation preview '.repeat(100), content_version: '10000000-0000-4000-8000-000000000001', latest_sequence: 1, current_provenance: null, updated_at: '2026-01-01T00:00:00Z' }] }
}

test('condensed cards avoid result hydration; keyboard Jump To loads uncached paginated planning targets and returns exact search position', { timeout: 60000 }, async () => {
  const f = fixture(), h = await openHierarchy(f), payload = searchPayload(f)
  await h.page.route('**/api/v1/search', route => respond(route, route.request().postDataJSON().query === 'preview' ? { projects: payload.projects.slice(-1), tasks: payload.tasks.slice(-1), observations: payload.observations } : payload))
  try {
    await h.start(); await h.idle()
    await h.page.getByPlaceholder('Filter project hierarchy...').fill('unrelated hierarchy'); await h.idle()
    const saved = await h.page.evaluate(id => localStorage.getItem('hmem-ws-' + id), f.workspace.id)
    const before = h.requests.length
    await search(h).fill('preview'); await resultsReady(h)
    assert.equal(await h.page.locator('.search-result-condensed').count(), 3)
    assert.deepEqual(await h.page.locator('.search-result-action').allTextContents(), ['Jump To', 'Jump To', 'Jump To'])
    assert.ok((await h.page.locator('.search-result-summary').allTextContents()).every(text => [...text].length <= 240))
    assert.ok((await h.page.locator('.search-result-title').allTextContents()).every(text => [...text].length <= 120))
    assert.equal(h.requests.slice(before).filter(r => /^\/api\/v1\/(projects|tasks)\/[^/]+$/.test(r.path)).length, 0, 'Rendering global cards must not hydrate the full hits')
    if (process.env.HMEM_SEARCH_SCREENSHOT) {
      await h.page.locator('#main-content-scroll').evaluate(element => { element.scrollTop = 300 })
      await h.page.evaluate(() => new Promise(resolve => requestAnimationFrame(() => requestAnimationFrame(resolve))))
      await mkdir(dirname(process.env.HMEM_SEARCH_SCREENSHOT), { recursive: true })
      await h.page.screenshot({ path: process.env.HMEM_SEARCH_SCREENSHOT, fullPage: false })
      assert.ok((await stat(process.env.HMEM_SEARCH_SCREENSHOT)).size < 4 * 1024 * 1024)
    }
    await search(h).fill('needle'); await resultsReady(h)
    for (const [kind, id, back] of [['project', 'deep-project-69', false], ['task', 'hidden-task-9', true]]) {
      const action = h.page.locator('#' + buttonId(kind, id))
      await action.scrollIntoViewIfNeeded(); await action.focus()
      const top = await h.page.locator('#main-content-scroll').evaluate(element => element.scrollTop)
      assert.ok(top > 300, 'Return receipt must cover a scrolled result set')
      await h.page.keyboard.press('Enter')
      await h.page.locator('#entity-' + id).waitFor({ state: 'visible', timeout: 10000 }); await focused(h, 'entity-' + id)
      const rect = await h.page.locator('#entity-' + id).evaluate(element => { const r = element.getBoundingClientRect(), p = document.getElementById('main-content-scroll').getBoundingClientRect(); return { top: r.top, bottom: r.bottom, viewport: p.bottom } })
      assert.ok(rect.top < rect.viewport && rect.bottom > 0, JSON.stringify(rect))
      assert.ok(h.requests.some(r => r.path.endsWith('/navigation/focus/' + kind + '/' + id)))
      if (back) await h.page.goBack()
      else await h.page.getByRole('button', { name: 'Back to search results', exact: true }).click()
      await focused(h, buttonId(kind, id)); await resultsReady(h)
      assert.equal(await search(h).inputValue(), 'needle')
      const returned = await h.page.locator('#main-content-scroll').evaluate(element => element.scrollTop)
      assert.ok(Math.abs(returned - top) <= 3, 'Restore actual scroller: ' + JSON.stringify({ returned, top }))
      assert.equal(await h.page.evaluate(id => localStorage.getItem('hmem-ws-' + id), f.workspace.id), saved)
    }
    assert.ok(h.requests.filter(r => r.path.endsWith('/navigation/focus/task/hidden-task-9')).length >= 2, 'Deep target needs bounded ancestor continuation')
    assert.ok(h.caps().maxBranches <= 4); assert.ok(h.caps().maxDetails <= 6)
    await h.page.setViewportSize({ width: 390, height: 800 })
    assert.ok(await h.page.locator('.search-result-condensed').evaluateAll(cards => cards.every(card => card.getBoundingClientRect().width <= document.getElementById('main-content-scroll').clientWidth)))
    assert.deepEqual(h.errors, []); assert.deepEqual(h.unhandled, [])
  } finally { await h.close() }
})

test('Observation Jump To reveals full readonly detached detail outside Subject Tree filters and Back retains those filters', { timeout: 60000 }, async () => {
  const h = await openDiscovery({ width: 390, height: 800 }, rows => rows.map(row => row.id === 'cache-main' ? { ...row, content: 'Full content '.repeat(1800) + 'END OF CANONICAL CONTENT' } : row))
  const target = h.fixture.observations.find(row => row.id === 'cache-main')
  await h.page.route('**/api/v1/search', route => respond(route, { ...empty, observations: [{ ...target, content_preview: target.content }] }))
  try {
    await h.start(); await h.page.getByRole('button', { name: 'Subject Tree', exact: true }).click(); await h.idle()
    await h.page.getByLabel('Search subjects', { exact: true }).fill('docs/'); await h.page.waitForTimeout(350); await h.idle()
    const tree = await h.page.getByRole('tree', { name: 'Stored subject paths', exact: true }).innerText()
    const saved = await h.page.evaluate(id => localStorage.getItem('hmem-ws-' + id), h.fixture.workspace.id)
    await search(h).fill('cache'); await resultsReady(h)
    const before = h.receipts.length
    const action = h.page.getByRole('button', { name: 'Jump To observation: src/Main.elm', exact: true })
    await action.focus(); await h.page.keyboard.press('Enter')
    try { await h.page.locator('#search-observation-detail').waitFor({ state: 'visible', timeout: 10000 }) }
    catch (error) { throw new Error(error.message + '\n' + JSON.stringify({ url: h.page.url(), config: await h.page.locator('[data-search-navigation]').getAttribute('data-search-navigation'), text: (await h.page.locator('body').innerText()).slice(-2000), requests: h.receipts.slice(before).map(receipt => ({ endpoint: receipt.endpoint, done: receipt.done, id: receipt.response?.id, contentLength: receipt.response?.content?.length })), errors: h.errors })) }
    await focused(h, 'search-observation-detail')
    const reader = h.page.getByLabel('Full observation content (read only)', { exact: true })
    assert.equal(await reader.inputValue(), target.content); assert.equal(await reader.getAttribute('readonly'), '')
    assert.equal(await h.page.locator('#observation-review, #observation-delete, .observation-edit-content').count(), 0)
    assert.ok(h.receipts.slice(before).every(receipt => receipt.endpoint === '/api/v1/observations/cache-main'), 'Opening detached detail must not refilter tree/list/counts')
    await h.page.goBack(); await focused(h, buttonId('observation', target.id))
    assert.equal(await search(h).inputValue(), 'cache')
    await h.page.getByRole('button', { name: 'Clear all entities search', exact: true }).click()
    await h.page.getByLabel('Search subjects', { exact: true }).waitFor()
    assert.equal(await h.page.getByLabel('Search subjects', { exact: true }).inputValue(), 'docs/')
    assert.equal(await h.page.getByRole('tree', { name: 'Stored subject paths', exact: true }).innerText(), tree)
    assert.equal(await h.page.evaluate(id => localStorage.getItem('hmem-ws-' + id), h.fixture.workspace.id), saved)
    assert.deepEqual(h.errors, [])
  } finally { await h.close() }
})

test('retired target callbacks cannot steal a newer jump or query, and unavailable targets retain a working search return', { timeout: 60000 }, async () => {
  const f = fixture(), h = await openHierarchy(f), payload = searchPayload(f)
  await h.page.route('**/api/v1/search', route => respond(route, payload))
  const held = h.gate(r => r.path.endsWith('/navigation/focus/task/hidden-task-9'), { hold: true })
  try {
    await h.start(); await h.idle(); await search(h).fill('race'); await resultsReady(h)
    await h.page.locator('#' + buttonId('task', 'hidden-task-9')).click()
    await h.bounded(held.arrived, 10000, 'Delayed canonical target')
    await h.page.getByRole('button', { name: 'Back to search results', exact: true }).click()
    await focused(h, buttonId('task', 'hidden-task-9'))
    await h.page.locator('#' + buttonId('project', 'deep-project-69')).click()
    await focused(h, 'entity-deep-project-69')
    held.release(); await h.idle()
    assert.equal(await h.page.evaluate(() => document.activeElement.id), 'entity-deep-project-69')
    await search(h).fill('new query'); await resultsReady(h)
    assert.equal(await h.page.locator('.search-jump-target').count(), 0)
    const missing = h.gate(r => r.path.endsWith('/navigation/focus/task/hidden-task-8'), { error: 404 })
    await h.page.locator('#' + buttonId('task', 'hidden-task-8')).click()
    await h.bounded(missing.arrived, 10000, 'Missing canonical target')
    await focused(h, 'search-jump-unavailable')
    assert.match(await h.page.locator('#search-jump-unavailable').innerText(), /unavailable or access has changed/)
    await h.page.getByRole('button', { name: 'Back to search results', exact: true }).click()
    await focused(h, buttonId('task', 'hidden-task-8')); assert.equal(await search(h).inputValue(), 'new query')
    const denied = h.gate(r => r.path.endsWith('/navigation/focus/task/hidden-task-7'), { error: 403 })
    await h.page.locator('#' + buttonId('task', 'hidden-task-7')).click()
    await h.bounded(denied.arrived, 10000, 'Revoked canonical target')
    await focused(h, 'search-jump-unavailable')
    assert.match(await h.page.locator('#search-jump-unavailable').innerText(), /unavailable or access has changed/)
    await h.page.getByRole('button', { name: 'Back to search results', exact: true }).click()
    await focused(h, buttonId('task', 'hidden-task-7')); assert.equal(await search(h).inputValue(), 'new query')
    assert.deepEqual(h.errors, []); assert.deepEqual(h.unhandled, [])
  } finally { held.release(); await h.close() }
})


test('Observation Jump To URL reload preserves excluded Subject Tree filters and canonical readonly detail without an in-memory receipt', { timeout: 60000 }, async () => {
  const h = await openDiscovery(), target = h.fixture.observations.find(row => row.id === 'cache-main')
  await h.page.route('**/api/v1/search', route => respond(route, { ...empty, observations: [{ ...target, content_preview: target.content }] }))
  try {
    await h.start(); await h.page.getByRole('button', { name: 'Subject Tree', exact: true }).click(); await h.idle()
    await h.page.getByLabel('Search subjects', { exact: true }).fill('docs/'); await h.page.waitForTimeout(350); await h.idle()
    await search(h).fill('cache'); await resultsReady(h)
    await h.page.locator('#' + buttonId('observation', target.id)).click()
    await h.page.locator('#search-observation-detail').waitFor(); await h.idle()
    const link = h.page.url()
    assert.ok(Buffer.byteLength(link) <= 4096); assert.match(link, /search-jump=1/)
    await h.page.reload(); await h.page.locator('#observation-panel').waitFor(); await h.idle()
    await h.page.locator('#search-observation-detail').waitFor()
    assert.equal(h.page.url(), link)
    assert.equal(await h.page.getByLabel('Search subjects', { exact: true }).inputValue(), 'docs/')
    assert.equal(await h.page.locator('#search-observation-detail .observation-detail-content').textContent(), target.content)
    assert.equal(await h.page.locator('#observation-edit, #observation-edit-content, #observation-review, #observation-delete').count(), 0)
    assert.equal(await h.page.getByRole('button', { name: 'Back to search results', exact: true }).count(), 0, 'Reload must not invent a saved result receipt')
    assert.equal(JSON.parse(await h.page.locator('#observation-panel').getAttribute('data-observation-context')).selectedId, target.id)
    assert.deepEqual(h.errors, [])
  } finally { await h.close() }
})

test('search invalidations during target navigation revalidate membership and counts on explicit and history return', { timeout: 60000 }, async () => {
  const f = fixture(), h = await openHierarchy(f)
  let payload = searchPayload(f), held = null, calls = 0
  const invalidate = async () => {
    await h.page.evaluate(workspace => {
        // This event belongs to the workspace delivery. The generic fixture
        // helper also sends global sockets, which would deliberately resync
        // an unrelated scope and introduce a second search invalidation.
        const prototype = window.WebSocket.prototype, original = prototype.emit
        prototype.emit = function (frame) { if (new URL(this.url).searchParams.get('ticket') === 'browser-ticket') original.call(this, frame) }
        try { window.pushHierarchyFrames([{ schema_version: 1, type: 'change', event: { schema_version: 1, event_id: 'search-live-' + Date.now(), scope: 'workspace', workspace_id: workspace, occurred_at: '2026-08-30T12:00:00Z', transaction: { id: 'search-live', cause: 'rest', request_id: null }, actor: { type: 'service', id: 'controlled-browser' }, entity: { type: 'project', id: 'deep-project-60', action: 'updated' }, invalidations: [{ kind: 'search', target: 'workspace:' + workspace }] } }]) } finally { prototype.emit = original }
      }, f.workspace.id)
  }
  let returnedTop = 0
  await h.page.route('**/api/v1/search', async route => { calls++; if (held) await held.promise; await respond(route, payload) })
  try {
    await h.start(); await h.idle(); await search(h).fill('live'); await resultsReady(h)
    for (const history of [false, true]) {
      const action = h.page.locator('#' + buttonId('task', 'hidden-task-9'))
      await action.scrollIntoViewIfNeeded(); await action.focus()
      const top = await h.page.locator('#main-content-scroll').evaluate(element => element.scrollTop)
      returnedTop = top
      await h.page.keyboard.press('Enter'); await focused(h, 'entity-hidden-task-9')
      const before = calls
      payload = { ...payload, projects: payload.projects.map((value, index) => index ? value : { ...value, name: 'Renamed live project' }), tasks: [...payload.tasks, { ...payload.tasks[0], id: 'added-' + before, title: 'Added while target open' }] }
      await invalidate()
      await h.idle(); assert.equal(calls, before, 'Target invalidation marks the saved search instead of dispatching an empty query')
      let release; const promise = new Promise(resolve => { release = resolve }); held = { promise, release }
      if (history) await h.page.goBack(); else await h.page.getByRole('button', { name: 'Back to search results', exact: true }).click()
      await focused(h, buttonId('task', 'hidden-task-9'))
      await h.page.waitForFunction(() => document.querySelector('.search-results-state'))
      assert.ok(Math.abs((await h.page.locator('#main-content-scroll').evaluate(element => element.scrollTop)) - top) <= 3)
      held.release(); held = null; await resultsReady(h)
      assert.equal(calls, before + 1)
      assert.equal(await h.page.locator('.search-result-condensed').count(), payload.projects.length + payload.tasks.length + payload.observations.length)
      assert.match(await h.page.locator('.search-results-count').innerText(), new RegExp(String(payload.projects.length + payload.tasks.length + payload.observations.length)))
      assert.match(await h.page.locator('.search-result-title').first().innerText(), /Renamed live project/)
      await focused(h, buttonId('task', 'hidden-task-9'))
      assert.ok(Math.abs((await h.page.locator('#main-content-scroll').evaluate(element => element.scrollTop)) - top) <= 3)
    }
    const beforeLive = calls
    payload = { ...payload, tasks: [...payload.tasks, { ...payload.tasks[0], id: 'source-live-added', title: 'Added after Back' }] }
    const liveRequest = h.page.waitForRequest(request => new URL(request.url()).pathname === '/api/v1/search')
    await invalidate(); await liveRequest
    const freshCount = payload.projects.length + payload.tasks.length + payload.observations.length
    await h.page.waitForFunction(count => document.querySelectorAll('.search-result-condensed').length === count, freshCount)
    await resultsReady(h); assert.equal(calls, beforeLive + 1)
    await h.page.goForward(); await focused(h, 'entity-hidden-task-9')
    await h.page.goBack(); await focused(h, buttonId('task', 'hidden-task-9')); await resultsReady(h)
    assert.equal(await h.page.locator('.search-result-condensed').count(), freshCount)
    assert.equal(await h.page.locator('.search-results-count').innerText(), freshCount + ' results')
    assert.equal(calls, beforeLive + 1, 'Forward/Back reuses the accepted live source membership, not the original receipt')
    assert.ok(Math.abs((await h.page.locator('#main-content-scroll').evaluate(element => element.scrollTop)) - returnedTop) <= 3)
    assert.deepEqual(h.errors, []); assert.deepEqual(h.unhandled, [])
  } finally { held?.release(); await h.close() }
})


test('return remount honors new pointer and wheel intent and gives removed results a useful focus fallback', { timeout: 60000 }, async () => {
  const f = fixture(), h = await openHierarchy(f)
  let payload = searchPayload(f), held = null
  await h.page.route('**/api/v1/search', async route => { if (held) await held.promise; await respond(route, payload) })
  try {
    await h.start(); await h.idle(); await search(h).fill('intent'); await resultsReady(h)
    for (const kind of ['pointer', 'wheel', 'removed']) {
      const action = h.page.locator('#' + buttonId('task', 'hidden-task-9'))
      await action.scrollIntoViewIfNeeded(); await action.click(); await focused(h, 'entity-hidden-task-9')
      await h.page.evaluate(workspace => {
        const prototype = window.WebSocket.prototype, original = prototype.emit
        prototype.emit = function (frame) { if (new URL(this.url).searchParams.get('ticket') === 'browser-ticket') original.call(this, frame) }
        try { window.pushHierarchyFrames([{ schema_version: 1, type: 'change', event: { schema_version: 1, event_id: 'intent-' + Date.now(), scope: 'workspace', workspace_id: workspace, occurred_at: '2026-08-30T12:00:00Z', transaction: { id: 'intent', cause: 'rest', request_id: null }, actor: { type: 'service', id: 'controlled-browser' }, entity: { type: 'project', id: 'deep-project-60', action: 'updated' }, invalidations: [{ kind: 'search', target: 'workspace:' + workspace }] } }]) } finally { prototype.emit = original }
      }, f.workspace.id)
      await h.idle()
      let release; held = { promise: new Promise(resolve => { release = resolve }), release: () => release() }
      await h.page.getByRole('button', { name: 'Back to search results', exact: true }).click()
      await focused(h, buttonId('task', 'hidden-task-9'))
      const before = await h.page.locator('#main-content-scroll').evaluate(element => element.scrollTop)
      if (kind === 'pointer') await search(h).click()
      else if (kind === 'wheel') {
        const bounds = await h.page.locator('#main-content-scroll').boundingBox()
        await h.page.mouse.move(bounds.x + bounds.width / 2, bounds.y + bounds.height / 2)
        await h.page.mouse.wheel(0, -500)
        await h.page.waitForFunction(top => document.getElementById('main-content-scroll').scrollTop < top - 100, before)
      }
      if (kind === 'removed') payload = { ...payload, tasks: payload.tasks.filter(value => value.id !== 'hidden-task-9') }
      held.release(); held = null; await resultsReady(h)
      await h.page.evaluate(() => new Promise(resolve => requestAnimationFrame(() => requestAnimationFrame(resolve))))
      if (kind !== 'wheel') assert.equal(await search(h).evaluate(element => document.activeElement === element), true)
      else assert.ok((await h.page.locator('#main-content-scroll').evaluate(element => element.scrollTop)) < before - 100, 'Completion must not undo the new wheel position')
    }
    assert.deepEqual(h.errors, []); assert.deepEqual(h.unhandled, [])
  } finally { held?.release(); await h.close() }
})
