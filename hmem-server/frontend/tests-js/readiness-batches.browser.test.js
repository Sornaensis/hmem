import assert from 'node:assert/strict'
import test from 'node:test'
import { hierarchyFixture, openHierarchy } from './hierarchy-fixture.mjs'

function dependencyFrame(workspace, action, eventId) {
  return { schema_version: 1, type: 'change', event: {
    schema_version: 1, event_id: eventId, scope: 'workspace', workspace_id: workspace,
    occurred_at: '2026-10-17T08:00:00Z', transaction: { id: eventId, cause: 'rest', request_id: null },
    actor: { type: 'service', id: 'controlled-browser' },
    entity: { type: 'task_dependency', id: 'child:prerequisite', action },
    invalidations: [
      { kind: 'entity', target: 'task_dependency:child:prerequisite' },
      { kind: 'readiness', target: 'task:child' },
      { kind: 'readiness', target: 'task:child' },
    ],
  } }
}

test('dependency ancestors refresh through bounded readiness summaries without overview reads', { timeout: 60000 }, async () => {
  const fixture = hierarchyFixture(), project = fixture.projects[0], task = fixture.tasks[0]
  fixture.projects = [project, { ...project, id: 'nested-project', name: 'Nested project', parent_id: project.id }]
  fixture.tasks = [
    { ...task, id: 'parent', title: 'Parent', project_id: 'nested-project', status: 'blocked', auto_blocked: true },
    { ...task, id: 'child', title: 'Child', project_id: 'nested-project', parent_id: 'parent', status: 'blocked', auto_blocked: true },
    { ...task, id: 'prerequisite', title: 'Prerequisite', project_id: 'nested-project' },
  ]
  const h = await openHierarchy(fixture)
  try {
    await h.start(); await h.scrollTo('task:child'); await h.idle()
    const mark = h.requests.length
    for (const action of ['created', 'deleted']) {
      fixture.dependencies = action === 'created' ? [{ task_id: 'child', depends_on_id: 'prerequisite' }] : []
      const batch = h.gate(r => r.path.endsWith('/navigation/summaries') && r.taskIds.includes('parent'), { hold: true })
      await h.page.evaluate(frames => window.pushHierarchyFrames(frames), [dependencyFrame(fixture.workspace.id, action, 'edge-' + action)])
      await h.bounded(batch.arrived, 10000, 'Ancestor readiness summary batch')
      batch.release(); await h.idle()
      const availability = await h.page.locator('#entity-child .status-select option').evaluateAll(options => Object.fromEntries(options.map(option => [option.value, !option.disabled])))
      for (const target of ['todo', 'in_progress', 'done']) assert.equal(availability[target], action === 'deleted', action + ' prerequisite: ' + target)
      assert.equal(availability.blocked, true); assert.equal(availability.cancelled, true)
    }
    const refreshed = h.requests.slice(mark)
    const ancestorBatches = refreshed.filter(r => r.path.endsWith('/navigation/summaries') && r.taskIds.includes('parent'))
    assert.equal(ancestorBatches.length, 2)
    for (const batch of ancestorBatches) {
      assert.deepEqual(batch.taskIds, ['parent'])
      assert.deepEqual([...batch.projectIds].sort(), ['nested-project', 'root-project'])
      const ids = [...batch.projectIds, ...batch.taskIds]
      assert.equal(new Set(ids).size, ids.length)
      assert.ok(ids.length <= 100)
    }
    assert.equal(refreshed.filter(r => r.path.endsWith('/overview')).length, 0)
    assert.ok(refreshed.some(r => r.path.endsWith('/navigation/summaries') && r.taskIds.includes('child')), 'Dependent summary remains independently refreshed')
    assert.ok(h.caps().maxBranches <= 4); assert.ok(h.caps().maxDetails <= 6)
    assert.deepEqual(h.errors, []); assert.deepEqual(h.unhandled, [])
  } finally { await h.close() }
})
