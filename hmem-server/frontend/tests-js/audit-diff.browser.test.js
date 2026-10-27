import assert from 'node:assert/strict'
import test from 'node:test'
import { hierarchyFixture, openHierarchy } from './hierarchy-fixture.mjs'

const dialog = h => h.page.locator('#audit-diff-dialog')
const detail = (kind, id) => '/api/v1/' + (kind === 'project' ? 'projects' : 'tasks') + '/' + id
const beforeText = 'Old specification ' + 'beyond preview '.repeat(18) + '\n# Markdown source\n<script>literal</script>\n'
const afterText = 'New specification ' + 'beyond preview '.repeat(18) + '\n# Markdown source\n<b>literal</b>'
const event = (f, kind, id) => ({ id: 'event-' + id, workspace_id: f.workspace.id, entity_type: kind, entity_id: id, action: 'update', old_values: { [kind === 'project' ? 'name' : 'title']: 'Before ' + id, description: beforeText, updated_at: '2026-10-14T10:00:00Z', priority: 1 }, new_values: { [kind === 'project' ? 'name' : 'title']: 'After ' + id, description: afterText, updated_at: '2026-10-15T10:00:00Z', priority: 1 }, changed_at: '2026-10-15T10:00:01Z', request_id: null, actor_type: 'user', actor_id: 'fixture-user', actor_label: 'Fixture User' })

async function setup() {
  const f = hierarchyFixture()
  f.projects = [f.projects[0]]
  f.tasks = [{ ...f.tasks[0], id: 'parent', title: 'Current parent' }, { ...f.tasks[1], id: 'child', title: 'Current child', parent_id: 'parent' }]
  const second = { ...f, workspace: { ...f.workspace, id: 'other-workspace', name: 'Other workspace' }, projects: [], tasks: [] }
  const h = await openHierarchy(f, [second])
  const events = [event(f, 'project', 'root-project'), event(f, 'task', 'parent'), event(f, 'task', 'child')]
  let authorized = true
  const reads = [], controls = [], releases = []
  await h.page.route('**/api/v1/**', async route => {
    const request = route.request(), url = new URL(request.url()), path = url.pathname
    let value
    if (path === '/api/v1/session') value = { auth_mode: 'local', principal: { actor_type: 'user', actor_id: 'diff-user', actor_label: 'Diff User', authority: 'local_superadmin', grant_user_id: null }, global_permissions: { create_workspace: false, superadmin: authorized }, workspace: { workspace_id: url.searchParams.get('workspace_id') || f.workspace.id, role: authorized ? 'owner' : null, can_read: authorized, can_edit: authorized, can_admin: authorized } }
    else if (path === '/api/v1/audit') value = { items: events.filter(entry => !url.searchParams.has('entity_id') || entry.entity_id === url.searchParams.get('entity_id')), has_more: false }
    else if (path === '/api/v1/groups') value = { items: [], has_more: false }
    else if (path.endsWith('/change-stream/ticket') && request.postDataJSON().scope.scope === 'global') value = { ticket: 'global-ticket', expires_at: '2099-01-01T00:00:00Z' }
    else if (path.endsWith('/change-stream/resync') && request.postDataJSON().scope.scope === 'global') value = { snapshot_profile: 'full_v1', items: [f, second].map(item => ({ schema_version: 1, kind: 'workspace', data: item.workspace })), has_more: false, resume_token: 'global-shell' }
    else if (/^\/api\/v1\/(projects|tasks)\/[^/]+$/.test(path) && request.method() === 'GET') {
      reads.push(path)
      const control = controls.find(control => !control.used && control.path === path)
      if (!control) return route.fallback()
      control.used = true
      const data = structuredClone((path.includes('/projects/') ? f.projects : f.tasks).find(row => row.id === path.split('/').at(-1)))
      value = control.transform ? control.transform(data) : data
      control.arrive()
      if (control.hold) await control.wait
      if (control.abort) return route.abort('failed')
      await route.fulfill({ status: control.status || 200, contentType: 'application/json', body: JSON.stringify(value) })
      return
    } else return route.fallback()
    await route.fulfill({ status: 200, contentType: 'application/json', body: JSON.stringify(value) })
  })
  return { ...h, f, second, events, reads, revoke() { authorized = false }, control(kind, id, options = {}) {
    let release, arrive
    const wait = new Promise(resolve => { release = resolve }), arrived = new Promise(resolve => { arrive = resolve })
    const control = { path: detail(kind, id), ...options, wait, arrived, release, arrive, used: false }
    controls.push(control); releases.push(release); return control
  }, async close() { for (const release of releases) release(); await h.close() } }
}

async function history(h, kind, id) {
  await h.scrollTo(kind + ':' + id)
  const card = h.page.locator('#entity-' + id)
  await card.locator('.btn-extras-toggle').first().click()
  if(kind==='task')await card.getByRole('button',{name:'History',exact:true}).click()
  await card.locator('.entity-history-toggle').first().click()
  await card.locator('.history-entry').waitFor()
  return card
}
async function open(h, id, field = 'description') {
  await h.page.locator('#audit-diff-origin-event-' + id + '-' + field).click()
  await dialog(h).waitFor()
  await h.page.waitForFunction(() => document.querySelector('#audit-diff-dialog')?.open)
}
async function current(h) {
  await dialog(h).getByRole('button', { name: 'Against current', exact: true }).click()
  await dialog(h).getByText(/Current loaded at/).waitFor()
  await h.page.waitForFunction(() => !document.querySelector('#audit-diff-dialog')?.textContent.includes('Loading current…'))
}
const close = h => dialog(h).getByRole('button', { name: 'Close', exact: true }).click()
const content = h => dialog(h).locator('.audit-diff-content')
const frame = (h, kind, id, eventId, invalidations = [{ kind: 'entity', target: kind + ':' + id }]) => ({ schema_version: 1, type: 'change', event: { schema_version: 1, event_id: eventId, scope: 'workspace', workspace_id: h.f.workspace.id, occurred_at: '2026-10-15T12:00:00Z', transaction: { id: eventId, cause: 'rest', request_id: 'other-request' }, actor: { type: 'system', id: null }, entity: { type: kind, id, action: 'updated' }, invalidations } })
const emit = (h, frames) => h.page.evaluate(frames => window.pushHierarchyFrames(frames), frames)

test('production collapsed audit row action has stable eligibility/defaults and preserves row/field keyboard origins', { timeout: 45000 }, async () => {
  const h = await setup()
  const unchanged = { title: 'Same title', description: 'Same description' }
  h.events.push(
    { ...h.events[1], id: 'status-only', old_values: { ...unchanged, status: 'todo' }, new_values: { ...unchanged, status: 'in_progress' } },
    { ...h.events[1], id: 'created', action: 'create', old_values: null },
    { ...h.events[1], id: 'deleted', action: 'delete', new_values: null },
    { ...h.events[1], id: 'observation', entity_type: 'observation' },
    { ...h.events[1], id: 'description-only', old_values: { title: 'Same title', description: 'Old description' }, new_values: { title: 'Same title', description: 'New description' } },
  )
  try {
    await h.start(); await h.idle()
    await h.page.getByRole('button', { name: 'Audit', exact: true }).click()
    await h.page.locator('#audit-diff-row-event-parent').waitFor()
    for (const surface of ['workspace', 'global']) {
      if (surface === 'global') {
        const globalSession = h.page.waitForResponse(response => {
          const url = new URL(response.url())
          return url.pathname === '/api/v1/session' && !url.searchParams.has('workspace_id')
        })
        const globalAudit = h.page.waitForResponse(response => {
          const url = new URL(response.url())
          return url.pathname === '/api/v1/audit' && !url.searchParams.has('workspace_id')
        })
        await h.page.getByRole('link', { name: 'Audit Log', exact: true }).click()
        await Promise.all([globalSession, globalAudit])
        await h.page.locator('.page.audit-log-view #audit-diff-row-event-parent').waitFor()
      }
      assert.equal(await h.page.locator('.audit-row-diff').count(), 4)
      for (const id of ['status-only', 'created', 'deleted', 'observation']) assert.equal(await h.page.locator('#audit-diff-row-' + id).count(), 0)
      const location = h.page.url(), origin = h.page.locator('#audit-diff-row-event-parent')
      for (const key of ['Enter', 'Space']) {
        await origin.focus(); await h.page.keyboard.press(key); await dialog(h).waitFor()
        assert.equal(await dialog(h).getByLabel('Field', { exact: true }).inputValue(), 'title')
        assert.equal(await dialog(h).getAttribute('data-dialog-origin'), 'audit-diff-row-event-parent')
        assert.equal(await h.page.locator('.audit-entry-detail').count(), 0)
        assert.equal(h.page.url(), location)
        await dialog(h).getByLabel('Field', { exact: true }).selectOption('description')
        await h.page.keyboard.press('Escape'); await dialog(h).waitFor({ state: 'detached' })
        await h.page.waitForFunction(() => document.activeElement?.id === 'audit-diff-row-event-parent')
      }
      await h.page.locator('#audit-diff-row-event-root-project').click(); await dialog(h).waitFor()
      assert.equal(await dialog(h).getByLabel('Field', { exact: true }).inputValue(), 'name')
      assert.equal(await h.page.locator('.audit-entry-detail').count(), 0)
      await close(h)
      await h.page.locator('#audit-diff-row-description-only').click(); await dialog(h).waitFor()
      assert.equal(await dialog(h).getByLabel('Field', { exact: true }).inputValue(), 'description')
      await close(h)
    }
    const entry = h.page.locator('.audit-entry').filter({ has: h.page.locator('#audit-diff-row-event-parent') })
    await entry.locator('.audit-expand-icon').click()
    await h.page.getByRole('complementary', { name: 'Selected audit event', exact: true }).waitFor()
    assert.equal(await entry.locator('.audit-row-diff').count(), 1)
    assert.equal(await h.page.locator('.audit-entry-detail .audit-view-diff').count(), 2)
    assert.equal(await h.page.locator('.audit-entry-detail').getByRole('button', { name: 'Copy audit entry ID: event-parent', exact: true }).count(), 1)
    assert.equal(await h.page.locator('[id]').evaluateAll(elements => new Set(elements.map(element => element.id)).size === elements.length), true)
    await entry.locator('#audit-diff-row-event-parent').click(); await dialog(h).waitFor()
    await dialog(h).getByLabel('Field', { exact: true }).selectOption('description')
    await h.page.keyboard.press('Escape'); await dialog(h).waitFor({ state: 'detached' })
    await h.page.waitForFunction(() => document.activeElement?.id === 'audit-diff-row-event-parent')
    await h.page.locator('.audit-entry-detail #audit-diff-origin-event-parent-title').click(); await dialog(h).waitFor()
    await dialog(h).getByLabel('Field', { exact: true }).selectOption('description')
    await h.page.keyboard.press('Escape'); await dialog(h).waitFor({ state: 'detached' })
    await h.page.waitForFunction(() => document.activeElement?.id === 'audit-diff-origin-event-parent-title')
    await h.idle()
  } finally { await h.close() }
})

test('production panel stays centered, theme-native and scrollable at desktop and 320px', { timeout: 45000 }, async () => {
  const h = await setup()
  try {
    await h.start(); await h.idle()
    await h.page.getByRole('button', { name: 'Audit', exact: true }).click()
    await h.page.locator('#audit-diff-row-event-parent').click(); await dialog(h).waitFor()
    await dialog(h).getByLabel('Field', { exact: true }).selectOption('description')
    const geometry = () => dialog(h).evaluate(element => {
      const box = element.getBoundingClientRect()
      return { centeredX: Math.abs(box.x + box.width / 2 - innerWidth / 2) < 1, centeredY: Math.abs(box.y + box.height / 2 - innerHeight / 2) < 1, contained: box.x >= 0 && box.y >= 0 && box.right <= innerWidth && box.bottom <= innerHeight }
    })
    assert.deepEqual(await geometry(), { centeredX: true, centeredY: true, contained: true })
    assert.equal(await dialog(h).evaluate(element => getComputedStyle(element).colorScheme), 'dark')
    await h.page.evaluate(() => { const root = document.documentElement; root.style.setProperty('--bg-surface', '#243247'); root.style.setProperty('--text', '#eef2f7'); root.style.setProperty('--border', '#52637c'); root.style.setProperty('--accent', '#715ee8') })
    await h.page.waitForFunction(() => getComputedStyle(document.querySelector('#audit-diff-dialog [aria-pressed="true"]')).backgroundColor === 'rgb(113, 94, 232)')
    assert.deepEqual(await dialog(h).evaluate(element => {
      const panel = getComputedStyle(element), active = getComputedStyle(element.querySelector('[aria-pressed="true"]')), select = getComputedStyle(element.querySelector('select'))
      return { surface: panel.backgroundColor, text: panel.color, border: panel.borderTopColor, active: active.backgroundColor, select: select.backgroundColor }
    }), { surface: 'rgb(36, 50, 71)', text: 'rgb(238, 242, 247)', border: 'rgb(82, 99, 124)', active: 'rgb(113, 94, 232)', select: 'rgb(15, 23, 42)' })
    await current(h)
    for (const height of [740, 480]) {
      await h.page.setViewportSize({ width: 320, height })
      assert.deepEqual(await geometry(), { centeredX: true, centeredY: true, contained: true })
      await content(h).evaluate(element => { element.scrollLeft = element.scrollWidth })
      assert.ok(await content(h).evaluate(element => element.scrollLeft) > 0)
      const closeBox = await dialog(h).getByRole('button', { name: 'Close', exact: true }).boundingBox()
      assert.ok(closeBox.y >= 0 && closeBox.y + closeBox.height <= height)
    }
    await h.page.keyboard.press('Escape'); await dialog(h).waitFor({ state: 'detached' })
    assert.ok((await h.page.locator('#audit-diff-row-event-parent').boundingBox()).height >= 32)
    assert.equal(await h.page.locator('.audit-entry-detail').count(), 0)
    await h.idle()
  } finally { await h.close() }
})

test('production shared diff opens from project/task/subtask History and workspace/global Audit with complete literal text', { timeout: 60000 }, async () => {
  const h = await setup()
  try {
    await h.start(); await h.idle()
    for (const [kind, id] of [['project', 'root-project'], ['task', 'parent'], ['task', 'child']]) {
      const card = await history(h, kind, id)
      assert.equal(await card.locator('.audit-view-diff').count(), 2, 'only changed in-scope fields offer diff')
      await open(h, id)
      assert.equal(await dialog(h).locator('script, b').count(), 0, 'raw source cannot become HTML')
      assert.match(await content(h).innerText(), /beyond preview.*beyond preview/)
      assert.deepEqual(await dialog(h).locator('.audit-diff-endpoint strong').allTextContents(), ['- Before', '+ After'])
      await h.page.waitForFunction(() => document.querySelector('#audit-diff-dialog .audit-diff-endpoints')?.textContent.includes('2026-10-14 10:00'))
      assert.match(await dialog(h).locator('.audit-diff-endpoints').innerText(), /2026-10-14 10:00.*2026-10-15 10:00/s)
      assert.equal(await dialog(h).locator('.audit-diff-no-newline').count(), 1)
      await dialog(h).getByLabel('Field', { exact: true }).selectOption(kind === 'project' ? 'name' : 'title')
      assert.ok(await dialog(h).locator('mark').count() > 0)
      const reads = h.reads.length
      await current(h)
      assert.equal(h.reads.length - reads, 1)
      assert.equal(h.reads.at(-1), detail(kind, id), 'subtasks use full task endpoint')
      await close(h)
    }
    await h.page.getByRole('button', { name: 'Audit', exact: true }).click()
    await h.page.locator('.audit-entry').first().waitFor()
    await h.page.locator('.audit-expand-icon').first().click()
    await open(h, 'root-project'); await close(h)
    await h.page.getByRole('link', { name: 'Audit Log', exact: true }).click()
    await h.page.locator('.audit-entry').first().waitFor()
    await h.page.locator('.audit-expand-icon').first().click()
    await open(h, 'root-project'); await current(h)
    assert.match(await dialog(h).innerText(), /Global Audit does not subscribe to every workspace/)
    await close(h); await h.idle()
  } finally { await h.close() }
})

test('production keyboard/modal/mobile behavior and frozen snapshot reuse require explicit Refresh', { timeout: 45000 }, async () => {
  const h = await setup()
  try {
    await h.start(); await h.idle(); await history(h, 'task', 'parent')
    const origin = h.page.locator('#audit-diff-origin-event-parent-title')
    await origin.focus(); await h.page.keyboard.press('Enter'); await dialog(h).waitFor()
    assert.equal(await dialog(h).getByRole('button', { name: 'Close', exact: true }).evaluate(e => document.activeElement === e), true)
    await h.page.keyboard.press('Shift+Tab')
    assert.equal(await content(h).evaluate(e => document.activeElement === e), true)
    await h.page.keyboard.press('Tab')
    assert.equal(await dialog(h).getByRole('button', { name: 'Close', exact: true }).evaluate(e => document.activeElement === e), true)
    assert.equal(await h.page.locator('#main-content-scroll').evaluate(e => { e.focus(); return document.activeElement === e }), false)
    const before = h.reads.length
    await current(h)
    assert.equal(h.reads.length - before, 1)
    await dialog(h).getByLabel('Field', { exact: true }).selectOption('description')
    await dialog(h).getByLabel('Historical baseline').selectOption('before')
    await h.page.waitForFunction(() => document.querySelector('#audit-diff-dialog .audit-diff-endpoint strong')?.textContent === '- Before')
    await dialog(h).getByRole('button', { name: 'This change', exact: true }).click()
    await dialog(h).getByRole('button', { name: 'Against current', exact: true }).click()
    assert.equal(h.reads.length - before, 1)
    h.f.tasks[0].description = 'Refreshed complete text'
    assert.doesNotMatch(await content(h).innerText(), /Refreshed complete text/)
    await dialog(h).getByRole('button', { name: 'Refresh', exact: true }).click()
    await h.page.waitForFunction(() => document.querySelector('#audit-diff-dialog .audit-diff-content')?.textContent.includes('Refreshed complete text'))
    assert.match(await content(h).innerText(), /Refreshed complete text/)
    await h.page.setViewportSize({ width: 320, height: 640 })
    const geometry = await dialog(h).evaluate(e => ({ width: e.getBoundingClientRect().width, scroll: e.scrollHeight > e.clientHeight, max: e.getBoundingClientRect().bottom <= innerHeight }))
    assert.ok(geometry.width <= 320 && geometry.max)
    await content(h).evaluate(e => { e.scrollLeft = e.scrollWidth })
    assert.ok(await content(h).evaluate(e => e.scrollLeft) > 0, 'long literal lines scroll on narrow screens')
    await h.page.keyboard.press('Escape'); await dialog(h).waitFor({ state: 'detached' })
    assert.equal(await origin.evaluate(e => document.activeElement === e), true, 'field switching preserves original opener focus')
    await h.idle()
  } finally { await h.close() }
})

test('production matching invalidation during GET survives late response; refresh and close/reopen races reject old text', { timeout: 45000 }, async () => {
  const h = await setup()
  try {
    await h.start(); await h.idle(); await history(h, 'task', 'parent'); await open(h, 'parent', 'title')
    const first = h.control('task', 'parent', { hold: true })
    await dialog(h).getByRole('button', { name: 'Against current', exact: true }).click(); await first.arrived
    h.f.tasks[0].title = 'Later authority'
    await emit(h, [frame(h, 'task', 'parent', 'during-fetch')])
    await dialog(h).getByText('Current has changed — Refresh.').waitFor()
    first.release(); await current(h)
    assert.match(await content(h).innerText(), /Current parent/)
    assert.doesNotMatch(await content(h).innerText(), /Later authority/)
    assert.match(await dialog(h).innerText(), /Current has changed — Refresh/)
    const old = h.control('task', 'parent', { hold: true, transform: value => ({ ...value, title: 'stale refresh' }) })
    await dialog(h).getByRole('button', { name: 'Refresh', exact: true }).click(); await old.arrived
    await dialog(h).getByRole('button', { name: 'Refresh', exact: true }).click()
    await h.page.waitForFunction(() => document.querySelector('#audit-diff-dialog .audit-diff-content')?.textContent.includes('Later authority'))
    old.release(); await h.idle()
    assert.doesNotMatch(await content(h).innerText(), /stale refresh/)
    const stale = h.control('task', 'parent', { hold: true, transform: value => ({ ...value, title: 'stale old opening' }) })
    await dialog(h).getByRole('button', { name: 'Refresh', exact: true }).click(); await stale.arrived
    await close(h); await open(h, 'parent', 'title'); await current(h)
    stale.release(); await h.idle()
    assert.doesNotMatch(await content(h).innerText(), /stale old opening/)
    const resync = h.gate(receipt => receipt.path.endsWith('/change-stream/resync'), { hold: true })
    await h.page.evaluate(() => window.pushHierarchyRawFrames([{ schema_version: 1, type: 'resync_required' }]))
    await resync.arrived
    await dialog(h).getByText('Freshness unknown: disconnected or resynchronizing.').waitFor()
    assert.match(await content(h).innerText(), /Later authority/)
    resync.release()
    await dialog(h).getByText('Freshness unknown: disconnected or resynchronizing.').waitFor({ state: 'detached' })
    assert.match(await content(h).innerText(), /Later authority/)
  } finally { await h.close() }
})

test('production current errors preserve historical comparison, reject mismatched identity and retain typed unavailable baselines', { timeout: 60000 }, async () => {
  const h = await setup()
  try {
    await h.start(); await h.idle(); await history(h, 'task', 'parent')
    for (const [options, message] of [[{ status: 403 }, /denied \(403\)/], [{ status: 404 }, /unavailable \(404\).*deleted or inaccessible/], [{ abort: true }, /network request failed/], [{ transform: () => ({ title: 'malformed' }) }, /Malformed current response/], [{ transform: value => ({ ...value, workspace_id: 'wrong' }) }, /entity or workspace does not match/], [{ transform: value => ({ ...value, id: 'wrong' }) }, /entity or workspace does not match/], [{ transform: value => { delete value.description; return value } }, /Malformed current response/], [{ transform: value => ({ ...value, description: 5 }) }, /Malformed current response/]]) {
      await open(h, 'parent', 'title')
      h.control('task', 'parent', options)
      await dialog(h).getByRole('button', { name: 'Against current', exact: true }).click()
      await dialog(h).locator('.audit-diff-error').waitFor()
      assert.match(await dialog(h).innerText(), message)
      assert.match(await content(h).innerText(), /Comparison unavailable/)
      await dialog(h).getByRole('button', { name: 'This change', exact: true }).click()
      await h.page.waitForFunction(() => document.querySelector('#audit-diff-dialog .audit-diff-content')?.textContent.includes('Before parent'))
      assert.match(await content(h).innerText(), /Before parent.*After parent/s)
      await close(h)
    }
    await history(h, 'project', 'root-project')
    await open(h, 'root-project')
    h.control('project', 'root-project', { transform: value => { delete value.description; return value } })
    await dialog(h).getByRole('button', { name: 'Against current', exact: true }).click()
    await dialog(h).locator('.audit-diff-error').waitFor()
    assert.match(await dialog(h).innerText(), /Malformed current response/)
    assert.match(await content(h).innerText(), /Comparison unavailable/)
    assert.doesNotMatch(await dialog(h).locator('.audit-diff-endpoints').innerText(), /Explicit null/)
    await dialog(h).getByRole('button', { name: 'This change', exact: true }).click()
    await h.page.waitForFunction(() => document.querySelector('#audit-diff-dialog .audit-diff-content')?.textContent.includes('Old specification'))
    await close(h)
    h.events[1].old_values.description = null; h.events[1].new_values.description = ''
    await h.page.reload(); await h.page.locator('#entity-parent').waitFor(); await h.idle(); await history(h, 'task', 'parent'); await open(h, 'parent')
    assert.match(await dialog(h).locator('.audit-diff-endpoints').innerText(), /Explicit null.*Empty string/s)
    assert.doesNotMatch(await content(h).innerText(), /No differences/)
  } finally { await h.close() }
})

test('production navigation/workspace and authorization retirement prevent stale callbacks from reviving dialog', { timeout: 45000 }, async () => {
  const h = await setup()
  try {
    await h.start(); await h.idle(); await history(h, 'task', 'parent'); await open(h, 'parent', 'title')
    const held = h.control('task', 'parent', { hold: true })
    await dialog(h).getByRole('button', { name: 'Against current', exact: true }).click(); await held.arrived
    await h.page.evaluate(() => { location.hash = 'tab=projects&focus=task:child' })
    await dialog(h).waitFor({ state: 'detached' }); held.release(); await h.idle()
    await h.page.goto(h.origin + '/workspace/' + h.f.workspace.id); await h.page.locator('#entity-parent').waitFor(); await h.idle(); await history(h, 'task', 'parent'); await open(h, 'parent', 'title')
    const oldWorkspace = h.control('task', 'parent', { hold: true })
    await dialog(h).getByRole('button', { name: 'Against current', exact: true }).click(); await oldWorkspace.arrived
    await h.page.evaluate(id => { history.pushState({}, '', '/workspace/' + id); dispatchEvent(new PopStateEvent('popstate')) }, h.second.workspace.id)
    await dialog(h).waitFor({ state: 'detached' }); oldWorkspace.release(); await h.idle()
    await h.page.goto(h.origin + '/workspace/' + h.f.workspace.id); await h.page.locator('#entity-parent').waitFor(); await h.idle(); await history(h, 'task', 'parent'); await open(h, 'parent', 'title')
    const retired = h.control('task', 'parent', { hold: true })
    await dialog(h).getByRole('button', { name: 'Against current', exact: true }).click(); await retired.arrived
    h.revoke()
    await emit(h, [frame(h, 'workspace', h.f.workspace.id, 'retire-authorization', [{ kind: 'session_authorization', target: 'session-authorization' }])])
    await dialog(h).waitFor({ state: 'detached' }); retired.release(); await h.idle()
    assert.equal(await dialog(h).count(), 0)
  } finally { await h.close() }
})

test('production equal/typed/missing/malformed endpoints and complete large-input fallback are explicit', { timeout: 45000 }, async () => {
  const h = await setup()
  try {
    await h.start(); await h.idle(); await history(h, 'task', 'parent'); await open(h, 'parent', 'title')
    h.f.tasks[0].title = 'After parent'
    await current(h)
    await content(h).getByText('No differences', { exact: true }).waitFor()
    await dialog(h).getByLabel('Historical baseline').selectOption('before')
    await h.page.waitForFunction(() => document.querySelector('#audit-diff-dialog .audit-diff-content')?.textContent.includes('Before parent'))
    await close(h)
    h.f.tasks[0].description = null
    await open(h, 'parent'); await current(h)
    assert.match(await dialog(h).locator('.audit-diff-endpoints').innerText(), /Current.*Explicit null/s)
    assert.doesNotMatch(await content(h).innerText(), /Comparison unavailable/)
    await close(h)
    const cases = [
      [null, 'null', /Explicit null.*Text/s, /null/],
      [undefined, 'available', /missing field.*Text/s, /Comparison unavailable/],
      [{ malformed: true }, 'available', /malformed data.*Text/s, /Comparison unavailable/],
      ['one\n'.repeat(20000) + 'COMPLETE OLD END', 'new\nCOMPLETE NEW END', /Text.*Text/s, /simplified full before\/after/],
    ]
    for (const [before, after, endpoints, output] of cases) {
      h.events[1].old_values.description = before
      h.events[1].new_values.description = after
      await h.page.reload(); await h.page.locator('#entity-parent').waitFor(); await h.idle(); await history(h, 'task', 'parent'); await open(h, 'parent')
      assert.match(await dialog(h).locator('.audit-diff-endpoints').innerText(), endpoints)
      assert.match(await content(h).innerText(), output)
      if (before?.includes?.('COMPLETE OLD END')) {
        assert.equal(await content(h).locator('pre').first().textContent(), before)
        assert.equal(await content(h).locator('pre').last().textContent(), after)
        await content(h).evaluate(e => { e.scrollTop = e.scrollHeight })
        assert.ok(await content(h).evaluate(e => e.scrollTop) > 0)
      }
      await close(h)
    }
  } finally { await h.close() }
})

test('production full entity GET timeout keeps This change usable and permits retry', { timeout: 30000 }, async () => {
  const h = await setup()
  try {
    await h.start(); await h.idle(); await history(h, 'task', 'parent'); await open(h, 'parent', 'title')
    const held = h.control('task', 'parent', { hold: true })
    await dialog(h).getByRole('button', { name: 'Against current', exact: true }).click(); await held.arrived
    await dialog(h).getByText('Current request timed out. Refresh to retry.', { exact: true }).waitFor({ timeout: 23000 })
    await dialog(h).getByRole('button', { name: 'This change', exact: true }).click()
    await h.page.waitForFunction(() => document.querySelector('#audit-diff-dialog .audit-diff-content')?.textContent.includes('Before parent'))
    held.release()
    await dialog(h).getByRole('button', { name: 'Against current', exact: true }).click()
    await dialog(h).getByRole('button', { name: 'Refresh', exact: true }).click()
    await dialog(h).getByText(/Current loaded at/).waitFor()
    assert.match(await content(h).innerText(), /Current parent/)
  } finally { await h.close() }
})
