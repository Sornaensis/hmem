import assert from 'node:assert/strict'
import test from 'node:test'
import { mkdir } from 'node:fs/promises'
import { resolve } from 'node:path'
import { hierarchyFixture, openHierarchy } from './hierarchy-fixture.mjs'
import { generateFixture } from '../perf/fixtures.mjs'

const model = 'pi:gpt-6.1-sol:extra-high'
const actors = [
  { actor_type: 'bot', actor_id: 'local-declared-bot:v1:persona', actor_label: 'Reviewer', bot_identity: { role: 'Reviewer', 'model-id': model } },
  { actor_type: 'bot', actor_id: 'credential-derived-bot', actor_label: 'Authenticated Bot', bot_identity: { role: 'Developer', 'model-id': 'codex:gpt-6.1-sol:high' } },
  { actor_type: 'user', actor_id: 'human-alice', actor_label: 'Alice' },
  { actor_type: 'user', actor_id: 'local', actor_label: 'Local User' },
  { actor_type: 'bot', actor_id: 'historical-bot', actor_label: 'Historical Bot' },
  { actor_type: 'system', actor_id: null, actor_label: 'System' },
  { actor_type: 'service', actor_id: 'worker', actor_label: 'Historical Service' },
  { actor_type: null, actor_id: null, actor_label: null },
]
const labels = ['Reviewer · ' + model, 'Developer · codex:gpt-6.1-sol:high', 'Alice', 'Local User', 'Historical Bot', 'System', 'Historical Service', 'Local User']

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
  await h.page.route('**/api/v1/**', async route => {
    const url = new URL(route.request().url()), path = url.pathname
    let value
    if (path === '/api/v1/session') value = { auth_mode: 'local', principal: { actor_type: 'user', actor_id: 'authority-local', actor_label: 'Local User', authority: 'local_superadmin', grant_user_id: null }, effective_actor: actors[0], global_permissions: { create_workspace: false, superadmin: false }, workspace: { workspace_id: fixture.workspace.id, role: 'admin', can_read: true, can_edit: true, can_admin: true } }
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
    assert.match(await h.page.locator('.sidebar-session-principal').innerText(), new RegExp(model))
    assert.match(await h.page.locator('.actor-authority-details').innerText(), /authority-local/)
    await shot('session-desktop')
    const project = h.page.locator('#entity-root-project')
    await project.locator('.btn-extras-toggle').click(); await project.locator('.entity-history-toggle').click()
    await project.locator('.history-entry').first().waitFor()
    assert.deepEqual(await project.locator('.history-actor').allTextContents(), labels.map(label => 'By ' + label))
    await h.page.getByRole('button', { name: 'Audit', exact: true }).click()
    await h.page.locator('.audit-entry').first().waitFor()
    for (const icon of await h.page.locator('.audit-expand-icon').all()) await icon.click()
    await h.page.locator('.audit-entry').first().scrollIntoViewIfNeeded()
    assert.match(await h.page.locator('.audit-entry-detail').first().innerText(), /local-declared-bot:v1:persona/)
    assert.match(await h.page.locator('.audit-entry-detail').nth(1).innerText(), /Authenticated Bot/)
    for (const width of [1440, 320]) {
      await h.page.setViewportSize({ width, height: 1000 })
      await h.page.locator('.audit-entry-detail').first().scrollIntoViewIfNeeded()
      await shot('audit-' + width)
      const overflowing = await h.page.locator('.audit-detail-value').evaluateAll(rows => rows.filter(row => row.scrollWidth > row.clientWidth + 1).length)
      assert.equal(overflowing, 0, 'actor detail values wrap at ' + width)
    }
    await h.page.setViewportSize({ width: 1440, height: 1000 })
    await h.page.getByRole('button', { name: 'Timeline', exact: true }).click()
    await h.page.locator('.timeline-event-card').first().waitFor()
    const timelineText = await h.page.locator('.timeline-date-events').innerText()
    for (const label of labels) assert.ok(timelineText.includes('By ' + label), label)
    await h.page.locator('#timeline-event-actor-event-0').scrollIntoViewIfNeeded(); await shot('timeline-desktop')
    await h.page.setViewportSize({ width: 320, height: 1000 })
    await h.page.locator('#timeline-event-actor-event-0').scrollIntoViewIfNeeded(); await shot('timeline-320')
    assert.equal(await h.page.locator('.timeline-context-chip').evaluateAll(rows => rows.filter(row => row.scrollWidth > row.clientWidth + 1).length), 0)
    await h.page.setViewportSize({ width: 1440, height: 1000 })
    await h.page.getByLabel('Workspace sections').getByRole('button', { name: 'Observations', exact: true }).click()
    await h.page.locator('.observation-card').first().waitFor()
    await h.page.locator('.observation-card').first().click()
    await h.page.locator('.observation-current-provenance').waitFor()
    await h.page.locator('#observation-history-toggle').click()
    await h.page.locator('.observation-history-entry').first().waitFor()
    assert.deepEqual(await h.page.locator('.observation-history-entry .observation-actor').allTextContents(), labels.map(label => 'By ' + label))
    await h.page.locator('.observation-current-provenance .actor-details summary').first().click()
    assert.match(await h.page.locator('.observation-current-provenance').innerText(), /local-declared-bot:v1:persona/)
    await h.page.locator('.observation-history-entry').first().scrollIntoViewIfNeeded(); await shot('observation-desktop')
    await h.page.setViewportSize({ width: 320, height: 1000 })
    await h.page.locator('.observation-history-entry').first().scrollIntoViewIfNeeded(); await shot('observation-320')
    assert.equal(await h.page.locator('.observation-actor').evaluateAll(rows => rows.filter(row => row.scrollWidth > row.clientWidth + 1).length), 0)
    assert.deepEqual(h.errors, [])
  } finally { await h.close() }
})
