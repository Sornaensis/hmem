import assert from 'node:assert/strict'
import test from 'node:test'
import { mkdir } from 'node:fs/promises'
import { resolve } from 'node:path'
import { hierarchyFixture, openHierarchy } from './hierarchy-fixture.mjs'
import { generateFixture } from '../perf/fixtures.mjs'

const model = 'pi:gpt-6.1-sol:extra-high'
const longModel = 'codex:' + 'complete-model-name-'.repeat(6) + 'gpt-6.1-sol:extra-high'
const actors = [
  { actor_type: 'bot', actor_id: 'local-declared-bot:v1:persona', actor_label: 'Reviewer', bot_identity: { role: 'Reviewer', 'model-id': model } },
  { actor_type: 'bot', actor_id: 'credential-derived-bot', actor_label: 'Authenticated Bot', bot_identity: { role: 'Developer', 'model-id': 'codex:gpt-6.1-sol:high' } },
  { actor_type: 'user', actor_id: 'human-alice', actor_label: 'Alice' },
  { actor_type: 'user', actor_id: 'local', actor_label: 'Local User' },
  { actor_type: 'bot', actor_id: 'historical-bot', actor_label: 'Historical Bot' },
  { actor_type: 'system', actor_id: null, actor_label: 'System' },
  { actor_type: 'service', actor_id: 'worker', actor_label: 'Historical Service' },
  { actor_type: null, actor_id: null, actor_label: null },
  { actor_type: 'user', actor_id: 'human-assisted', actor_label: 'Alice', bot_identity: { role: 'Reviewer', 'model-id': longModel } },
]
const labels = ['Reviewer · ' + model, 'Developer · codex:gpt-6.1-sol:high', 'Alice', 'Local User', 'Historical Bot', 'System', 'Historical Service', 'Local User', 'Alice · Reviewer · ' + longModel]
const kinds = ['Model', 'Model', 'User', 'User', 'Bot', 'System', 'Service', 'User', 'Model']

async function assertActors(rows) {
  assert.equal(await rows.count(), actors.length)
  assert.deepEqual(await rows.locator('.actor-attribution-kind').allTextContents(), kinds)
  assert.deepEqual(await rows.locator('.actor-attribution').allTextContents(), labels.map((label, index) => kinds[index] === label ? label : kinds[index] + ' ' + label))
  assert.equal(await rows.locator('.actor-details').count(), 0)
}

async function assertSessionContext(page, expected) {
  await page.locator('.global-context').scrollIntoViewIfNeeded()
  assert.equal(await page.locator('.sidebar-session-principal').innerText(), expected)
  assert.doesNotMatch(await page.locator('.sidebar-session').innerText(), /gpt-6|authority-local|local_superadmin|Actor details/)
  const geometry = await page.locator('.global-context').evaluate(header => {
    const bounds = header.getBoundingClientRect(), session = header.querySelector('.sidebar-session').getBoundingClientRect()
    const main = document.querySelector('.main-content').getBoundingClientRect()
    return { contained: session.top >= bounds.top && session.bottom <= bounds.bottom + 1, separated: main.top >= bounds.bottom - 1 }
  })
  assert.equal(geometry.contained, true, 'session is reachable inside its current global header')
  assert.equal(geometry.separated, true, 'global header and main content do not overlap')
}

test('production attribution keeps declared, authenticated and historical actors readable across existing views', { timeout: 60000 }, async () => {
  const fixture = hierarchyFixture()
  fixture.projects = fixture.projects.slice(0, 1); fixture.tasks = fixture.tasks.slice(0, 1)
  const observation = { ...generateFixture('small').observations[0], workspace_id: fixture.workspace.id, latest_sequence: actors.length }
  const revisions = actors.map((actor, index) => ({ observation_id: observation.id, sequence: actors.length - index, event_kind: 'update', reviewed_git_sha: observation.git_sha, content_version: observation.content_version, content_digest: 'a'.repeat(64), recorded_at: observation.updated_at, ...actor }))
  observation.current_provenance = revisions[0]; fixture.observations = [observation]
  const audits = actors.map((actor, index) => ({ id: 'actor-audit-' + index, workspace_id: fixture.workspace.id, entity_type: 'project', entity_id: 'root-project', action: 'update', old_values: { name: 'Before' }, new_values: { name: 'After' }, changed_at: observation.updated_at, ...actor }))
  const events = actors.map((actor, index) => ({ id: 'actor-event-' + index, workspace_id: fixture.workspace.id, event_type: 'project_created', entity_type: 'project', entity_id: 'root-project', title: 'Actor example ' + index, occurred_at: observation.updated_at, actor: { type: actor.actor_type, id: actor.actor_id, label: actor.actor_label, bot_identity: actor.bot_identity }, navigation: { entity_type: 'project', entity_id: 'root-project' } }))
  const h = await openHierarchy(fixture)
  h.page.setDefaultTimeout(5000)
  const shots = process.env.HMEM_ACTOR_SHOTS
  const shot = async name => { if (shots) { await mkdir(shots, { recursive: true }); await h.page.screenshot({ path: resolve(shots, name + '.png') }) } }
  let authenticated = false
  await h.page.route('**/api/v1/**', async route => {
    const url = new URL(route.request().url()), path = url.pathname
    let value
    if (path === '/api/v1/session') value = { auth_mode: authenticated ? 'deployed' : 'local', principal: { actor_type: 'user', actor_id: 'authority-local', actor_label: authenticated ? 'Alice Authenticated User' : 'Local User', authority: authenticated ? 'grant_user' : 'local_superadmin', grant_user_id: null }, effective_actor: actors[0], global_permissions: { create_workspace: false, superadmin: false }, workspace: { workspace_id: fixture.workspace.id, role: 'admin', can_read: true, can_edit: true, can_admin: true } }
    else if (path === '/api/v1/audit') value = { items: audits, has_more: false }
    else if (path.endsWith('/timeline/buckets')) value = { workspace_id: fixture.workspace.id, since: url.searchParams.get('since'), until: url.searchParams.get('until'), bucket: url.searchParams.get('bucket'), buckets: [], next_since: null }
    else if (path.endsWith('/timeline')) value = { items: events, has_more: false }
    else if (path === '/api/v1/observations') value = { items: [observation], has_more: false }
    else if (path === '/api/v1/observations/' + observation.id) value = observation
    else if (path === '/api/v1/observations/' + observation.id + '/history') value = { items: revisions, has_more: false }
    else return route.fallback()
    await route.fulfill({ status: 200, contentType: 'application/json', body: JSON.stringify(value) })
  })
  try {
    await h.start(); await h.idle()
    await assertSessionContext(h.page, 'Local User')
    const project = h.page.locator('#entity-root-project')
    await project.locator('.btn-extras-toggle').click(); await project.locator('.entity-history-toggle').click()
    await project.locator('.history-entry').first().waitFor()
    await assertActors(project.locator('.history-entry'))
    await shot('history-desktop')
    await h.page.setViewportSize({ width: 320, height: 1000 })
    await assertSessionContext(h.page, 'Local User'); await shot('session-320')
    await h.page.setViewportSize({ width: 1440, height: 1000 })
    await h.page.getByRole('button', { name: 'Audit', exact: true }).click()
    await h.page.locator('.audit-entry').first().waitFor()
    await assertActors(h.page.locator('.audit-entry'))
    for (const icon of await h.page.locator('.audit-expand-icon').all()) await icon.click()
    await h.page.locator('.audit-entry').first().scrollIntoViewIfNeeded()
    await assertActors(h.page.locator('.audit-entry'))
    assert.doesNotMatch(await h.page.locator('.audit-entry-detail').first().innerText(), /Actor ID|Actor type|Actor label|Declared role|Reviewer/)
    assert.ok((await h.page.locator('.audit-entry-detail').first().innerText()).includes('root-project'))
    for (const width of [1440, 320]) {
      await h.page.setViewportSize({ width, height: 1000 })
      await h.page.locator('.audit-entry').first().scrollIntoViewIfNeeded()
      await shot('audit-' + width)
      const overflowing = await h.page.locator('.audit-detail-value').evaluateAll(rows => rows.filter(row => row.scrollWidth > row.clientWidth + 1).length)
      assert.equal(overflowing, 0, 'audit metadata values wrap at ' + width)
      if (width === 320) {
        const tracks = await h.page.locator('.audit-detail-grid').first().evaluate(row => ({ widths: getComputedStyle(row).gridTemplateColumns.split(' ').map(parseFloat), available: row.clientWidth, gap: parseFloat(getComputedStyle(row).columnGap) }))
        assert.equal(tracks.widths.length, 2)
        assert.equal(tracks.widths[0], 65, 'Accepted narrow Audit keeps its label track')
        assert.ok(tracks.widths[1] > 0 && tracks.widths[0] + tracks.widths[1] + tracks.gap <= tracks.available + 1, 'Audit values use the remaining bounded content width')
      }
    }
    await h.page.setViewportSize({ width: 1440, height: 1000 })
    await h.page.getByRole('button', { name: 'Timeline', exact: true }).click()
    await h.page.locator('.timeline-event-card').first().waitFor()
    for (let index = 0; index < actors.length; index++) {
      const row = h.page.locator('#timeline-event-actor-event-' + index)
      assert.equal(await row.locator('.actor-attribution').count(), 1)
      assert.ok((await row.innerText()).includes(labels[index]))
      assert.equal(await row.locator('button, details, .timeline-actor-details').count(), 0)
    }
    await h.page.locator('#timeline-event-actor-event-8').scrollIntoViewIfNeeded(); await shot('timeline-desktop')
    await h.page.setViewportSize({ width: 320, height: 1000 })
    await h.page.locator('#timeline-event-actor-event-8').scrollIntoViewIfNeeded(); await shot('timeline-320')
    assert.equal(await h.page.locator('.timeline-context-chip').evaluateAll(rows => rows.filter(row => row.scrollWidth > row.clientWidth + 1).length), 0)
    assert.equal(await h.page.locator('.timeline-event-attribution').evaluateAll(rows => rows.filter(row => row.scrollWidth > row.clientWidth + 1).length), 0)
    const target = h.page.locator('#timeline-event-actor-event-0')
    await target.focus(); await target.press('Enter')
    await h.page.waitForFunction(() => new URLSearchParams(location.hash.slice(1)).get('focus') === 'project:root-project')
    await h.page.setViewportSize({ width: 1440, height: 1000 })
    await h.page.getByLabel('Workspace sections').getByRole('button', { name: 'Observations', exact: true }).click()
    await h.page.getByRole('button', { name: 'All Observations', exact: true }).click()
    await h.page.locator('.observation-card').first().waitFor()
    await h.page.locator('.observation-card').first().click()
    await h.page.locator('.observation-current-provenance').waitFor()
    await h.page.locator('#observation-history-toggle').click()
    await h.page.locator('.observation-history-entry').first().waitFor()
    await assertActors(h.page.locator('.observation-history-entry'))
    assert.equal(await h.page.locator('.observation-current-provenance > dd > .observation-actor .actor-attribution').count(), 1)
    assert.equal(await h.page.locator('.observation-history-entry > .observation-revision-value').count(), actors.length)
    assert.equal(await h.page.locator('.observation-history-entry .copyable-value').first().getAttribute('title'), 'Copy recorded revision: ' + observation.git_sha)
    await h.page.locator('.observation-history-entry').first().scrollIntoViewIfNeeded(); await shot('observation-desktop')
    await h.page.setViewportSize({ width: 320, height: 1000 })
    await h.page.locator('.observation-history-entry').last().scrollIntoViewIfNeeded(); await shot('observation-320')
    assert.equal(await h.page.locator('.observation-actor').evaluateAll(rows => rows.filter(row => row.scrollWidth > row.clientWidth + 1).length), 0)
    assert.equal(await h.page.locator('.actor-details, .actor-authority-details').count(), 0)
    await h.page.locator('#observation-history-toggle').click()
    await h.page.locator('.observation-history-entry').first().waitFor({ state: 'detached' })
    assert.equal(await h.page.locator('.observation-history-entry').count(), 0)
    await h.page.locator('#observation-history-toggle').click()
    await h.page.locator('.observation-history-entry').first().waitFor()
    authenticated = true
    await h.page.reload(); await h.page.locator('.sidebar-session-principal').getByText('Alice Authenticated User').waitFor()
    await assertSessionContext(h.page, 'Alice Authenticated User'); await shot('session-authenticated-320')
    assert.deepEqual(h.errors, [])
  } finally { await h.close() }
})
