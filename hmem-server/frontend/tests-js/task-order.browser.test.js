import assert from 'node:assert/strict'
import test from 'node:test'
import { hierarchyFixture, openHierarchy } from './hierarchy-fixture.mjs'

test('production hierarchy orders blocked parents with running children and refreshes readiness-only changes', { timeout: 30000 }, async () => {
  const fixture = hierarchyFixture(), template = fixture.tasks[0]
  fixture.projects = [fixture.projects[0]]
  fixture.tasks = [
    { ...template, id: 'parent', project_id: null, parent_id: null, title: 'Z parent', status: 'blocked', priority: 1, auto_blocked: true },
    { ...template, id: 'child', project_id: null, parent_id: 'parent', title: 'Child', status: 'in_progress' },
    { ...template, id: 'todo', project_id: null, parent_id: null, title: 'A todo', status: 'todo', priority: 10 },
    { ...template, id: 'running', project_id: null, parent_id: null, title: 'A running', status: 'in_progress', priority: 1 },
  ]
  const h = await openHierarchy(fixture)
  try {
    await h.start(); await h.idle()
    assert.equal((await h.page.locator('#entity-parent .tree-toggle').textContent()).trim(), '▶')
    const order = () => h.page.locator('[data-hierarchy-key^="task:"]').evaluateAll(rows => rows.map(row => row.dataset.hierarchyKey))
    assert.deepEqual(await order(), ['task:running', 'task:parent', 'task:todo'])
    assert.equal(await h.page.locator('#entity-parent .status-select').inputValue(), 'blocked')
    const refresh = async (status, id) => {
      fixture.tasks.find(task => task.id === 'child').status = status
      await h.page.evaluate(({ workspace, id }) => window.pushHierarchyFrames([{ schema_version: 1, type: 'change', event: {
        schema_version: 1, event_id: id, scope: 'workspace', workspace_id: workspace, occurred_at: '2026-10-16T12:00:00Z',
        transaction: { id, cause: 'rest', request_id: null }, actor: { type: 'service', id: 'controlled-browser' },
        entity: { type: 'task', id: 'child', action: 'updated' }, invalidations: [{ kind: 'readiness', target: 'task:parent' }],
      } }]), { workspace: fixture.workspace.id, id })
      await h.idle()
    }
    await refresh('todo', 'running-child-stopped')
    assert.deepEqual(await order(), ['task:running', 'task:todo', 'task:parent'])
    await refresh('in_progress', 'running-child-resumed')
    assert.deepEqual(await order(), ['task:running', 'task:parent', 'task:todo'])
    assert.equal(await h.page.locator('#entity-child').count(), 0)
    assert.ok(h.caps().maxBranches <= 4); assert.ok(h.caps().maxDetails <= 6)
  } finally { await h.close() }
})
