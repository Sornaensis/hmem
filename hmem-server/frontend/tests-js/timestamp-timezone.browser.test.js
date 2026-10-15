import assert from 'node:assert/strict'
import test from 'node:test'
import { generateFixture } from '../perf/fixtures.mjs'
import { hierarchyFixture, openHierarchy } from './hierarchy-fixture.mjs'

const source = '2026-01-01T00:15:30.123456Z'
const offsetSource = '2026-01-01T02:15:30.123456+02:00'
const nextSource = '2026-01-01T00:16:00Z'
const liveSource = '2026-01-01T00:17:00.123456789Z'
const since = '2026-01-01T00:00:00Z', until = '2026-01-02T00:00:00Z'

async function textEquals(page, selector, expected) {
  await page.waitForFunction(({ selector, expected }) => document.querySelector(selector)?.textContent === expected, { selector, expected })
}

async function textIncludes(page, selector, expected) {
  await page.waitForFunction(({ selector, expected }) => document.querySelector(selector)?.textContent.includes(expected), { selector, expected })
}

function liveFrame(fixture, entityType, id, sequence) {
  return { schema_version: 1, type: 'change', event: {
    schema_version: 1, event_id: 'timezone-' + sequence, scope: 'workspace', workspace_id: fixture.workspace.id,
    occurred_at: liveSource, transaction: { id: 'timezone-tx-' + sequence, cause: 'rest', request_id: null }, actor: { type: 'system', id: null },
    entity: { type: entityType, id, action: 'updated' }, invalidations: [{ kind: 'entity', target: entityType + ':' + id }]
  } }
}

async function openTimezone(timezoneId, timestamp = source) {
  const fixture = hierarchyFixture()
  fixture.workspace.created_at = timestamp; fixture.workspace.updated_at = offsetSource
  fixture.projects = [{ ...fixture.projects[0], created_at: timestamp, updated_at: offsetSource }]
  fixture.tasks = [
    { ...fixture.tasks[0], id: 'timezone-task', title: '2050-01-01T12:34:00Z', created_at: timestamp, updated_at: offsetSource, completed_at: source },
    { ...fixture.tasks[0], id: 'timezone-invalid', title: 'Invalid input', created_at: '', updated_at: '2026-02-30T00:00:00Z' }
  ]
  let observation = { ...generateFixture('small').observations[0], id: 'timezone-observation', workspace_id: fixture.workspace.id,
    content: source, created_at: source, updated_at: source, latest_sequence: 26 }
  const provenance = (sequence, recordedAt) => ({ sequence, event_kind: 'update', reviewed_git_sha: observation.git_sha,
    content_version: observation.content_version, content_digest: 'a'.repeat(64), recorded_at: recordedAt, actor_type: 'user', actor_id: 'owner', actor_label: 'Owner' })
  observation.current_provenance = provenance(26, offsetSource)
  fixture.observations = [observation]
  const audit = { id: 'timezone-audit', workspace_id: fixture.workspace.id, entity_type: 'project', entity_id: 'root-project', action: 'update',
    old_values: { name: '2040-01-01T00:00:00Z', due_at: source }, new_values: { name: '2050-01-01T00:00:00Z', due_at: nextSource },
    changed_at: source, actor_type: 'user', actor_id: 'owner', actor_label: 'Owner' }
  const events = [
    { id: 'timezone-event', workspace_id: fixture.workspace.id, event_type: 'task_created', entity_type: 'task', entity_id: 'timezone-task', title: 'Timezone event', occurred_at: source, navigation: { entity_type: 'task', entity_id: 'timezone-task' } },
    { id: 'timezone-event-next-day', workspace_id: fixture.workspace.id, event_type: 'task_created', entity_type: 'task', entity_id: 'timezone-task', title: 'Later event', occurred_at: '2026-01-01T08:15:00Z', navigation: { entity_type: 'task', entity_id: 'timezone-task' } }
  ]
  const zero = { created: 0, completed: 0, deleted: 0, archived: 0, cancelled: 0 }, count = { ...zero, created: 5 }
  const legacy = { created: 0, completed: 0, cancelled: 0 }
  const bucket = { bucket_start: since, bucket_end: until, label: 'server UTC bucket label',
    counts: { project: legacy, subproject: legacy, task: legacy, subtask: legacy }, totals: legacy,
    series: { project: zero, task: count, subtask: zero, observation: zero }, series_totals: count }
  const h = await openHierarchy(fixture, [], null, { timezoneId })
  h.page.setDefaultTimeout(7000)
  await h.page.addInitScript(() => { Date.now = () => Date.parse('2026-01-02T12:00:00Z') })
  const receipts = []
  await h.page.route('**/api/v1/**', async route => {
    const url = new URL(route.request().url()), path = url.pathname
    let value
    if (path === '/api/v1/audit') value = { items: [audit], has_more: false }
    else if (path.endsWith('/memberships')) value = { items: [{ workspace_id: fixture.workspace.id, user_id: 'member', role: 'read', created_at: source, updated_at: source }], has_more: false }
    else if (path.endsWith('/timeline/buckets')) value = { workspace_id: fixture.workspace.id, since: url.searchParams.get('since'), until: url.searchParams.get('until'), bucket: url.searchParams.get('bucket'), buckets: [bucket], next_since: null }
    else if (path.endsWith('/timeline')) value = { items: events.filter(event => (!url.searchParams.has('since') || Date.parse(event.occurred_at) >= Date.parse(url.searchParams.get('since'))) && (!url.searchParams.has('until') || Date.parse(event.occurred_at) < Date.parse(url.searchParams.get('until')))), has_more: false }
    else if (path === '/api/v1/observations') value = { items: [observation], has_more: false }
    else if (path.endsWith('/subject-facets')) value = { items: [{ subject_kind: observation.subject_kind, subject: observation.subject, observation_count: 1, latest_updated_at: observation.updated_at }], has_more: false }
    else if (path === '/api/v1/observations/' + observation.id + '/history') {
      const offset = Number(url.searchParams.get('offset') || 0), head = observation.latest_sequence
      const revisions = Array.from({ length: head }, (_, i) => ({ observation_id: observation.id, ...provenance(head - i, head - i === 1 ? nextSource : source) }))
      value = { items: revisions.slice(offset, offset + 25), has_more: offset + 25 < head }
    } else if (path === '/api/v1/observations/' + observation.id) value = observation
    else return route.fallback()
    receipts.push({ path, method: route.request().method(), params: Object.fromEntries(url.searchParams) })
    await route.fulfill({ status: 200, contentType: 'application/json', body: JSON.stringify(value) })
  })
  return { ...h, receipts, audit, events, bucket,
    observation: () => observation,
    async updateObservation() {
      observation = { ...observation, latest_sequence: 27, content_version: '20000000-0000-4000-8000-000000000027', updated_at: liveSource }
      observation.current_provenance = provenance(27, liveSource); fixture.observations = [observation]
      await h.page.evaluate(frame => window.pushHierarchyFrames([frame]), liveFrame(fixture, 'observation', observation.id, 1))
    }
  }
}

for (const [zone, expected, next, live, rangeStart, rangeEnd] of [
  ['UTC', '2026-01-01 00:15', '2026-01-01 00:16', '2026-01-01 00:17', '2026-01-01 00:00', '2026-01-02 00:00'],
  ['Europe/Copenhagen', '2026-01-01 01:15', '2026-01-01 01:16', '2026-01-01 01:17', '2026-01-01 01:00', '2026-01-02 01:00'],
  ['America/Los_Angeles', '2025-12-31 16:15', '2025-12-31 16:16', '2025-12-31 16:17', '2025-12-31 16:00', '2026-01-01 16:00'],
  ['Asia/Kathmandu', '2026-01-01 06:00', '2026-01-01 06:01', '2026-01-01 06:02', '2026-01-01 05:45', '2026-01-02 05:45']
]) test('production Main localizes all representative surfaces in ' + zone, { timeout: 60000 }, async () => {
  const h = await openTimezone(zone)
  try {
    await h.start(); await h.idle()
    await textEquals(h.page, '.workspace-details .workspace-detail:nth-last-child(2) .workspace-detail-value', expected)
    await textIncludes(h.page, '#entity-root-project .card-meta-group', 'Created: ' + expected)
    await textIncludes(h.page, '#entity-timezone-task .card-meta-group', 'Updated: ' + expected)
    await textIncludes(h.page, '#entity-timezone-task .card-meta-group', 'Completed: ' + expected)
    assert.ok((await h.page.locator('#entity-timezone-task').textContent()).includes('2050-01-01T12:34:00Z'))
    await textIncludes(h.page, '#entity-timezone-invalid .card-meta-group', 'Created: —')
    await textIncludes(h.page, '#entity-timezone-invalid .card-meta-group', 'Updated: Invalid timestamp')

    const project = h.page.locator('#entity-root-project')
    await project.locator('.btn-extras-toggle').click(); await project.locator('.entity-history-toggle').click()
    await textEquals(h.page, '#entity-root-project .history-timestamp', expected)
    await h.page.getByRole('button', { name: 'Administration', exact: true }).click()
    await textEquals(h.page, '.membership-updated', 'Updated ' + expected)
    await h.page.getByRole('button', { name: 'Audit', exact: true }).click()
    await textEquals(h.page, '.audit-timestamp', expected)
    await h.page.locator('.audit-expand-icon').click()
    await textIncludes(h.page, '.audit-entry-detail', next)
    const detailText = await h.page.locator('.audit-entry-detail').textContent()
    assert.ok(detailText.includes(expected) && !detailText.includes('Invalid timestamp'))
    assert.ok(detailText.includes('2040-01-01T00:00:00Z') && detailText.includes('2050-01-01T00:00:00Z'))
    assert.equal(h.audit.old_values.due_at, source); assert.equal(h.audit.new_values.due_at, nextSource)

    await h.page.getByRole('button', { name: 'Timeline', exact: true }).click()
    await textEquals(h.page, '#timeline-event-timezone-event .timeline-event-time', expected)
    const day = h.page.locator('.timeline-date-group').filter({ has: h.page.locator('#timeline-event-timezone-event') }).locator('.timeline-date-heading')
    assert.equal(await day.textContent(), expected.slice(0, 10))
    const point = h.page.locator('#timeline-point-lifecycle-created-0')
    await h.page.waitForFunction(value => document.getElementById('timeline-point-lifecycle-created-0')?.getAttribute('aria-label').includes(value), rangeStart + ' to ' + rangeEnd)
    assert.ok((await point.locator('title').textContent()).includes(`[${rangeStart}, ${rangeEnd}): 5`))
    assert.equal(await h.page.locator('.timeline-chart-x-axis').first().textContent(), rangeStart)
    await point.click()
    await h.page.waitForFunction(() => document.getElementById('timeline-point-lifecycle-created-0')?.getAttribute('aria-pressed') === 'true')
    const selected = h.receipts.filter(r => r.path.endsWith('/timeline') && r.params.since).at(-1)
    assert.deepEqual([selected.params.since, selected.params.until], [since, until])
    assert.match(await point.getAttribute('aria-label'), /, 5, selected$/)
    assert.equal(h.bucket.bucket_start, since); assert.equal(h.bucket.series_totals.created, 5)

    await h.page.getByLabel('Workspace sections').getByRole('button', { name: 'Observations', exact: true }).click()
    await textIncludes(h.page, '.observation-updated', expected)
    await h.page.locator('.observation-card').first().click()
    await textIncludes(h.page, '.observation-current-provenance', expected)
    assert.equal(await h.page.locator('.observation-detail-content').textContent(), source)
    await h.page.locator('#observation-history-toggle').click()
    await h.page.waitForFunction(() => document.querySelectorAll('.observation-history-time').length === 26)
    await textEquals(h.page, '.observation-history-entry:last-child .observation-history-time', next)
    assert.deepEqual(h.receipts.filter(r => r.path.endsWith('/history')).map(r => r.params.offset), ['0', '25'])
    await h.updateObservation()
    await textIncludes(h.page, '.observation-detail-meta', 'Content updated' + live)
    assert.equal(h.observation().updated_at, liveSource)
    assert.equal(await h.page.locator('.observation-detail-content').textContent(), source)
    assert.equal(h.fixture.workspace.created_at, source)
    await h.page.reload()
    await textEquals(h.page, '.workspace-details .workspace-detail:nth-last-child(2) .workspace-detail-value', expected)
    assert.deepEqual(h.errors, [])
  } finally { await h.close() }
})

test('production per-instant conversion covers summer DST directions offsets and month rollover', { timeout: 60000 }, async () => {
  for (const [zone, raw, expected] of [
    ['Europe/Copenhagen', '2026-07-15T23:45:00.123456Z', '2026-07-16 01:45'],
    ['Europe/Copenhagen', '2026-03-29T00:59:59.999999Z', '2026-03-29 01:59'],
    ['Europe/Copenhagen', '2026-03-29T01:00:00Z', '2026-03-29 03:00'],
    ['Europe/Copenhagen', '2026-10-25T00:59:59Z', '2026-10-25 02:59'],
    ['Europe/Copenhagen', '2026-10-25T01:00:00Z', '2026-10-25 02:00'],
    ['Asia/Kathmandu', '2026-01-31T23:45:00.123456789Z', '2026-02-01 05:30'],
    ['UTC', '2026-01-01T01:15:00.123456+01:30', '2025-12-31 23:45']
  ]) {
    const h = await openTimezone(zone, raw)
    try {
      await h.start(); await h.idle()
      await textEquals(h.page, '.workspace-details .workspace-detail:nth-last-child(2) .workspace-detail-value', expected)
      assert.equal(h.fixture.workspace.created_at, raw)
    } finally { await h.close() }
  }
})

test('real formatting replies are presentation only and reload adopts a changed browser timezone', { timeout: 60000 }, async () => {
  // Control the zone through one CDP session so it can change within this page.
  const h = await openTimezone(undefined)
  const cdp = await h.page.context().newCDPSession(h.page)
  await cdp.send('Emulation.setTimezoneOverride', { timezoneId: 'Europe/Copenhagen' })
  await h.page.addInitScript(() => {
    const schedule = window.queueMicrotask.bind(window)
    window.timestampReplyJobs = []; window.holdTimestampReplies = true
    window.queueMicrotask = callback => window.holdTimestampReplies ? window.timestampReplyJobs.push(callback) : schedule(callback)
  })
  try {
    await h.start(); await h.idle()
    await h.page.waitForFunction(() => window.timestampReplyJobs.length > 0)
    assert.ok((await h.page.locator('.workspace-details').textContent()).includes('…'))
    await h.page.waitForFunction(() => ['timezone-task', 'timezone-invalid'].every(id => document.querySelector('#entity-' + id + ' .card-meta-group')?.textContent.includes('Updated: …')))
    await h.idle()
    const before = [h.requests.length, h.receipts.length]
    await h.page.evaluate(() => { window.holdTimestampReplies = false; for (const job of window.timestampReplyJobs.splice(0)) job() })
    await textEquals(h.page, '.workspace-details .workspace-detail:nth-last-child(2) .workspace-detail-value', '2026-01-01 01:15')
    await h.idle()
    assert.deepEqual([h.requests.length, h.receipts.length], before, 'Timestamp reply alone must not refetch API data')
    await cdp.send('Emulation.setTimezoneOverride', { timezoneId: 'Asia/Kathmandu' })
    assert.ok(['Asia/Kathmandu', 'Asia/Katmandu'].includes(await h.page.evaluate(() => Intl.DateTimeFormat().resolvedOptions().timeZone)))
    assert.ok((await h.page.locator('.workspace-details').textContent()).includes('2026-01-01 01:15'))
    // The reload creates a fresh formatter. Release its actual queued reply too.
    await h.page.reload(); await h.page.waitForFunction(() => window.timestampReplyJobs.length > 0)
    await h.page.evaluate(() => { window.holdTimestampReplies = false; for (const job of window.timestampReplyJobs.splice(0)) job() })
    await textEquals(h.page, '.workspace-details .workspace-detail:nth-last-child(2) .workspace-detail-value', '2026-01-01 06:00')
  } finally { await h.close() }
})
