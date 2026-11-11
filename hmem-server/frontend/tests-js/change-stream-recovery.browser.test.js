import assert from 'node:assert/strict'
import test from 'node:test'
import { hierarchyFixture, openHierarchy } from './hierarchy-fixture.mjs'

const checkpoint = token => ({ schema_version: 1, type: 'checkpoint', catch_up: 'complete', resume_token: token })
const change = fixture => ({ schema_version: 1, type: 'change', event: {
  schema_version: 1, event_id: 'browser-recovery-change', scope: 'workspace', workspace_id: fixture.workspace.id,
  occurred_at: '2026-11-11T00:00:00Z', transaction: { id: 'browser-recovery-tx', cause: 'rest', request_id: null },
  actor: { type: 'system', id: null }, entity: { type: 'task', id: fixture.tasks[0].id, action: 'updated' },
  invalidations: [{ kind: 'entity', target: 'task:' + fixture.tasks[0].id }, { kind: 'next_task', target: 'workspace:' + fixture.workspace.id }]
} })
const resyncs = h => h.requests.filter(request => request.path === '/api/v1/change-stream/resync').length
const savedTokens = page => page.evaluate(() => Object.entries(localStorage)
  .filter(([key]) => key.startsWith('hmem.change-stream.')).map(([, value]) => JSON.parse(value).resumeToken))

test('production main and Elm accept slowly progressing changes without a replacement snapshot', { timeout: 30000 }, async () => {
  const fixture = hierarchyFixture()
  const h = await openHierarchy(fixture)
  try {
    await h.start(); await h.idle()
    const before = resyncs(h)
    await h.page.evaluate(frame => window.pushHierarchyRawFrames([frame]), change(fixture))
    await h.page.waitForTimeout(1250)
    await h.idle()
    assert.equal(resyncs(h), before, 'A gap greater than the former one-second heuristic must preserve the bearer')
    assert.ok(!(await savedTokens(h.page)).includes('slow-replay-complete'))
    await h.page.evaluate(frame => window.pushHierarchyRawFrames([frame]), checkpoint('slow-replay-complete'))
    await h.page.waitForFunction(() => Object.entries(localStorage).some(([key, value]) => key.startsWith('hmem.change-stream.') && JSON.parse(value).resumeToken === 'slow-replay-complete'))
    assert.equal(resyncs(h), before)
    assert.deepEqual(h.errors, [])
  } finally { await h.close() }
})

test('production Elm rejection before a checkpoint retires that generation and cannot reuse its token', { timeout: 30000 }, async () => {
  const fixture = hierarchyFixture()
  const h = await openHierarchy(fixture)
  const ticketBodies = []
  h.page.on('request', request => {
    if (new URL(request.url()).pathname === '/api/v1/change-stream/ticket') ticketBodies.push(request.postDataJSON())
  })
  await h.page.addInitScript(() => {
    window.recoveryCheckpointWrites = []
    const original = Storage.prototype.setItem
    Storage.prototype.setItem = function (key, value) {
      if (key.startsWith('hmem.change-stream.')) window.recoveryCheckpointWrites.push(JSON.parse(value).resumeToken)
      return original.call(this, key, value)
    }
  })
  try {
    await h.start(); await h.idle()
    const before = resyncs(h)
    const malformed = change(fixture)
    malformed.event.entity.type = 'unsupported'
    await h.page.evaluate(frames => window.pushHierarchyRawFrames(frames), [malformed, checkpoint('rejected-generation')])
    await h.page.waitForFunction(() => window.recoveryCheckpointWrites.filter(token => token === 'browser-shell').length >= 2, null, { timeout: 5000 })
    await h.idle()
    assert.ok(resyncs(h) > before, 'The actual Elm reducer must request scoped resynchronization')
    assert.ok(!(await savedTokens(h.page)).includes('rejected-generation'))
    assert.ok(!ticketBodies.some(body => body.resume_token === 'rejected-generation'))
    assert.ok(!(await h.page.evaluate(() => window.recoveryCheckpointWrites)).includes('rejected-generation'))
    assert.deepEqual(h.errors, [])
  } finally { await h.close() }
})
