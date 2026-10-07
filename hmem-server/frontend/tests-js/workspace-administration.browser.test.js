import assert from 'node:assert/strict'
import test from 'node:test'
import { mkdir } from 'node:fs/promises'
import { join } from 'node:path'
import { openDiscovery } from './observation-discovery-fixture.mjs'
import { paint } from './observation-viewport-fixture.mjs'

const memberId = '00000000-0000-4000-8000-000000000001'
const newId = '00000000-0000-4000-8000-000000000002'
async function openAdmin() {
  const h = await openDiscovery(), requests = [], pending = new Set(), controls = []
  let role = 'owner', actor = 'admin-actor', implicit = false, sessionReads = 0
  let members = [{ workspace_id: h.fixture.workspace.id, user_id: memberId, role: 'edit', granted_by: null, created_at: '2026-01-01T00:00:00Z', updated_at: '2026-10-07T00:00:00Z' }]
  await h.page.route('**/api/v1/session**', async route => {
    sessionReads++
    const control = controls.find(c => !c.used && c.method === 'SESSION')
    if (control) { control.used = true; control.arrive(); await control.wait }
    if (control && control.status !== 200) { await route.fulfill({ status: control.status, body: '{}' }); return }
    await route.fulfill({ status: 200, contentType: 'application/json', body: JSON.stringify({
      auth_mode: 'local', principal: { actor_type: 'user', actor_id: actor, actor_label: 'Workspace Owner', authority: implicit ? 'local_superadmin' : 'local', grant_user_id: null },
      global_permissions: { create_workspace: false, superadmin: implicit },
      workspace: { workspace_id: h.fixture.workspace.id, role, can_read: true, can_edit: role !== 'read', can_admin: role === 'owner' || role === 'admin' }
    }) })
  })
  await h.page.route('**/api/v1/workspaces/*/memberships**', async route => {
    const request = route.request(), receipt = { method: request.method(), path: new URL(request.url()).pathname, payload: request.postData() ? request.postDataJSON() : null, done: false }
    requests.push(receipt); pending.add(receipt)
    const control = controls.find(c => !c.used && c.method === receipt.method)
    if (control) { control.used = true; control.arrive(receipt); await control.wait }
    const status = control?.status || 200
    receipt.status = status
    let body
    if (status !== 200) body = { error: 'Controlled membership failure' }
    else if (receipt.method === 'GET') body = { items: members, has_more: false }
    else if (receipt.method === 'POST') {
      assert.deepEqual(Object.keys(receipt.payload).sort(), ['request_id', 'role', 'user_id'])
      assert.ok(receipt.payload.request_id)
      const member = { ...members[0], workspace_id: h.fixture.workspace.id, user_id: receipt.payload.user_id, role: receipt.payload.role, created_at: '2026-01-01T00:00:00Z', updated_at: '2026-10-07T00:00:00Z' }
      members = [member, ...members.filter(m => m.user_id !== member.user_id)]; body = member
    } else {
      assert.equal(receipt.method, 'DELETE'); assert.ok(request.headers()['x-request-id'])
      members = members.filter(m => !receipt.path.endsWith('/' + m.user_id))
    }
    await route.fulfill({ status, contentType: 'application/json', body: body ? JSON.stringify(body) : '' })
    receipt.done = true; pending.delete(receipt)
  })
  await h.page.addInitScript(() => {
    window.adminCopies = []
    Object.defineProperty(navigator.clipboard, 'writeText', { value: value => { window.adminCopies.push(value); return Promise.resolve() } })
  })
  return { ...h, adminRequests: requests, get sessionReads() { return sessionReads },
    authority(nextRole, nextActor = actor, nextImplicit = false) { role = nextRole; actor = nextActor; implicit = nextImplicit },
    hold(method, status = 200) { let release, arrive; const wait = new Promise(resolve => { release = resolve }), arrived = new Promise(resolve => { arrive = resolve }); const c = { method, status, wait, arrived, release, arrive, used: false }; controls.push(c); return c },
    async quiet() { await h.idle(); await h.bounded((async () => { while (pending.size) await h.page.waitForTimeout(20) })(), 10000, 'Owned membership quiet') },
    async close() { for (const c of controls) c.release(); await h.close() }
  }
}
const tab = h => h.page.getByRole('button', { name: 'Administration', exact: true })
async function enter(h) { await tab(h).click(); await h.page.locator('#workspace-membership-user').waitFor(); await h.quiet() }
async function capture(h, name) {
  if (!process.env.HMEM_ADMIN_SCREENSHOT_DIR) return
  await mkdir(process.env.HMEM_ADMIN_SCREENSHOT_DIR, { recursive: true })
  await h.page.locator('#main-content-scroll').hover(); await h.page.mouse.wheel(0, -5000); await paint(h.page)
  await h.page.locator(name.startsWith('workspace-administration') ? '#workspace-administration-heading' : '#observation-mode-heading').scrollIntoViewIfNeeded(); await paint(h.page)
  if (h.page.viewportSize().width === 320) {
    await h.page.locator(name.startsWith('workspace-administration') ? '#workspace-administration-heading' : '#observation-mode-heading').evaluate(element => element.scrollIntoView({ block: 'start' })); await paint(h.page)
  }
  await h.page.getByText('Copied to clipboard', { exact: true }).waitFor({ state: 'hidden', timeout: 5000 })
  assert.ok(await h.page.evaluate(() => document.documentElement.scrollWidth <= innerWidth + 1))
  await h.page.screenshot({ path: join(process.env.HMEM_ADMIN_SCREENSHOT_DIR, name + '.png') })
}
let sequence = 0
async function refreshAuthority(h) {
  const eventId = 'admin-authority-' + (++sequence)
  await h.page.evaluate(frame => window.pushHierarchyFrames([frame]), { schema_version: 1, type: 'change', event: {
    schema_version: 1, event_id: eventId, scope: 'workspace', workspace_id: h.fixture.workspace.id, occurred_at: '2026-10-07T00:00:00Z',
    transaction: { id: eventId, cause: 'rest', request_id: null }, actor: { type: 'system', id: null },
    entity: { type: 'workspace_membership', id: memberId, action: 'updated' }, invalidations: [{ kind: 'session_authorization', target: 'session-authorization' }]
  } })
}

test('production dedicated Administration routes and responsive native controls keep Observation chrome clean', { timeout: 60000 }, async () => {
  const h = await openAdmin()
  try {
    await h.start()
    assert.equal(await h.page.locator('.workspace-administration,.permission-summary,.workspace-memberships').count(), 0)
    assert.equal(h.adminRequests.length, 0)
    assert.deepEqual(await h.page.locator('.workspace-tabs button').allTextContents(), ['Projects', 'Observations', 'Timeline', 'Audit', 'Administration'])
    for (const width of [1440, 320]) {
      await h.page.setViewportSize({ width, height: 900 }); await paint(h.page)
      await capture(h, width === 1440 ? 'workspace-observations-desktop' : 'workspace-observations-narrow')
      await enter(h)
      assert.match(h.page.url(), /tab=administration/)
      assert.equal(await h.page.locator('.search-input,#observation-panel').count(), 0)
      assert.equal(await h.page.getByLabel('User UUID', { exact: true }).count(), 1)
      assert.equal(await h.page.getByLabel('Role', { exact: true }).count(), 1)
      await capture(h, width === 1440 ? 'workspace-administration-desktop' : 'workspace-administration-narrow')
      const copy = h.page.locator('.membership-user')
      await copy.focus(); await h.page.keyboard.press('Enter')
      await h.page.waitForFunction(count => window.adminCopies.length === count, width === 1440 ? 1 : 2, { timeout: 5000 })
      assert.equal((await h.page.evaluate(() => window.adminCopies)).at(-1), memberId)
      assert.match(h.page.url(), /tab=administration/)
      await h.page.getByRole('button', { name: 'Observations', exact: true }).click(); await h.quiet()
    }
    await h.page.addStyleTag({ content: 'html { font-size: 32px !important; }' }); await paint(h.page)
    await h.page.getByRole('button', { name: 'Projects', exact: true }).focus()
    for (let i = 0; i < 4; i++) await h.page.keyboard.press('Tab')
    assert.equal(await h.page.evaluate(() => document.activeElement.textContent), 'Administration')
    await h.page.keyboard.press('Enter'); await h.quiet()
    assert.ok(await h.page.evaluate(() => document.documentElement.scrollWidth <= innerWidth + 1))
    await h.page.getByLabel('User UUID', { exact: true }).fill(newId)
    assert.equal(await h.page.getByLabel('User UUID', { exact: true }).inputValue(), newId)
    await h.page.evaluate(() => history.back()); await h.page.locator('#observation-panel').waitFor(); await h.quiet()
    await h.page.evaluate(() => history.forward()); await h.page.locator('#workspace-membership-user').waitFor(); await h.quiet()
    await h.page.reload(); await h.page.locator('#workspace-membership-user').waitFor(); await h.quiet()
    assert.equal(await h.page.locator('.membership-row').count(), 1)
    assert.deepEqual(h.errors, [])
  } finally { await h.close() }
})

test('production membership list Retry, failed writes, duplicate prevention and owned session rechecks use real receipts', { timeout: 60000 }, async () => {
  const h = await openAdmin()
  try {
    await h.start()
    const first = h.hold('GET', 503)
    const entering = tab(h).click()
    await first.arrived; first.release(); await entering; await h.quiet()
    await h.page.getByRole('button', { name: 'Retry memberships', exact: true }).click(); await h.quiet()
    assert.equal(h.adminRequests.filter(r => r.method === 'GET').length, 2)
    assert.equal(await h.page.locator('.membership-row').count(), 1)
    await h.page.getByLabel('User UUID', { exact: true }).fill(newId)
    await h.page.getByLabel('Role', { exact: true }).selectOption('admin')
    const failure = h.hold('POST', 503), sessions = h.sessionReads
    await h.page.locator('#workspace-membership-submit').click(); await failure.arrived
    await h.page.waitForFunction(() => document.getElementById('workspace-membership-submit')?.disabled, null, { timeout: 5000 })
    assert.equal(await h.page.locator('#workspace-membership-submit').isDisabled(), true)
    assert.equal(await h.page.getByLabel('User UUID', { exact: true }).isDisabled(), true)
    assert.equal(await h.page.locator('.membership-row button').last().isDisabled(), true)
    await h.page.locator('.membership-form').evaluate(form => form.dispatchEvent(new Event('submit', { bubbles: true, cancelable: true })))
    assert.equal(h.adminRequests.filter(r => r.method === 'POST').length, 1)
    failure.release(); await h.quiet()
    assert.equal(await h.page.getByLabel('User UUID', { exact: true }).inputValue(), newId)
    assert.equal(await h.page.getByLabel('Role', { exact: true }).inputValue(), 'admin')
    assert.equal(await h.page.locator('.membership-row').count(), 1)
    assert.equal(h.sessionReads, sessions)
    const forbidden = h.hold('POST', 403)
    await h.page.locator('#workspace-membership-submit').click(); await forbidden.arrived
    await h.page.waitForFunction(() => document.getElementById('workspace-membership-submit')?.disabled, null, { timeout: 5000 })
    forbidden.release(); await h.quiet()
    assert.equal(h.adminRequests.at(-1).status, 403)
    assert.equal(h.adminRequests.at(-1).payload.user_id, newId)
    assert.equal(await h.page.getByLabel('User UUID', { exact: true }).inputValue(), newId)
    assert.equal(await h.page.getByLabel('Role', { exact: true }).inputValue(), 'admin')
    assert.equal(await h.page.locator('.membership-row').count(), 1)
    assert.equal(await h.page.locator('#workspace-membership-submit').isDisabled(), false)
    assert.match(await h.page.locator('.workspace-memberships .form-error').innerText(), /Could not save membership/)
    assert.equal(h.sessionReads, sessions)
    const recheck = h.hold('SESSION')
    await h.page.locator('#workspace-membership-submit').click(); await recheck.arrived
    await h.page.waitForFunction(() => document.getElementById('workspace-membership-submit')?.textContent === 'Checking access...', null, { timeout: 5000 })
    assert.equal(await h.page.locator('#workspace-membership-submit').isDisabled(), true)
    assert.match(await h.page.locator('.workspace-memberships').innerText(), /Checking current workspace access/)
    const writeCount = h.adminRequests.filter(r => r.method === 'POST').length
    await h.page.locator('.membership-form').evaluate(form => form.dispatchEvent(new Event('submit', { bubbles: true, cancelable: true })))
    assert.equal(h.adminRequests.filter(r => r.method === 'POST').length, writeCount)
    recheck.release(); await h.quiet()
    assert.equal(h.sessionReads, sessions + 1)
    assert.equal(await h.page.locator('#workspace-membership-submit').isDisabled(), false)
    assert.equal(await h.page.locator('.membership-row').count(), 2)
    const row = h.page.locator('[data-member-user="' + newId + '"]'), failedDelete = h.hold('DELETE', 503)
    await row.getByRole('button', { name: 'Remove', exact: true }).click(); await failedDelete.arrived
    failedDelete.release(); await h.quiet()
    assert.equal(await row.count(), 1)
    await row.getByRole('button', { name: 'Remove', exact: true }).click(); await h.quiet()
    assert.equal(await row.count(), 0); assert.equal(h.sessionReads, sessions + 2)
  } finally { await h.close() }
})

test('production direct unavailable and implicit Admin links emit no membership requests; revocation retires held list', { timeout: 60000 }, async () => {
  const h = await openAdmin()
  try {
    for (const [role, implicit] of [['read', false], ['edit', false], ['owner', true]]) {
      h.authority(role, 'admin-actor', implicit)
      const before = h.adminRequests.length
      await h.page.goto(h.origin + '/workspace/' + h.fixture.workspace.id + '#tab=administration'); await h.quiet()
      assert.equal(await tab(h).count(), 0)
      assert.equal(await h.page.locator('#workspace-membership-user').count(), 0)
      assert.match(await h.page.locator('.workspace-administration').innerText(), /unavailable/)
      assert.equal(h.adminRequests.length, before)
    }
    h.authority('owner'); await h.page.goto(h.origin + '/'); await h.quiet(); await h.start()
    const held = h.hold('GET'), entering = tab(h).click()
    await held.arrived; await entering
    assert.match(await h.page.locator('.workspace-memberships').innerText(), /Loading memberships/)
    h.authority('read'); await refreshAuthority(h)
    await h.page.waitForFunction(() => !document.getElementById('workspace-membership-user'), null, { timeout: 5000 })
    assert.equal(await tab(h).count(), 0)
    held.release(); await h.quiet()
    assert.equal(await h.page.locator('.membership-row').count(), 0)
    assert.equal(await h.page.locator('#workspace-membership-user').count(), 0)
  } finally { await h.close() }
})

test('production failed owned authorization recheck keeps writes closed and Retry session reloads verified memberships', { timeout: 60000 }, async () => {
  const h = await openAdmin()
  try {
    await h.start(); await enter(h)
    await h.page.getByLabel('User UUID', { exact: true }).fill(newId)
    const failed = h.hold('SESSION', 503)
    await h.page.locator('#workspace-membership-submit').click(); await failed.arrived
    await h.page.waitForFunction(() => document.getElementById('workspace-membership-submit')?.disabled, null, { timeout: 5000 })
    failed.release()
    await h.page.getByRole('heading', { name: 'Session unavailable', exact: true }).waitFor()
    assert.equal(await h.page.locator('#workspace-membership-submit').count(), 0)
    const posts = h.adminRequests.filter(r => r.method === 'POST').length
    await h.page.getByRole('button', { name: 'Retry session', exact: true }).click()
    await h.page.locator('#workspace-membership-user').waitFor(); await h.quiet()
    assert.equal(await h.page.locator('.membership-row').count(), 2)
    assert.equal(await h.page.locator('#workspace-membership-submit').isDisabled(), false)
    assert.equal(h.adminRequests.filter(r => r.method === 'POST').length, posts)
  } finally { await h.close() }
})

