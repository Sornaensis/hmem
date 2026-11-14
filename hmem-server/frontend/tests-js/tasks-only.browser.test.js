import assert from 'node:assert/strict'
import test from 'node:test'
import { hierarchyFixture, openHierarchy } from './hierarchy-fixture.mjs'

test('Tasks-only active families load cold, page across projects, retain children and survive filter toggles', { timeout: 60000 }, async () => {
  const fixture = hierarchyFixture()
  fixture.projects = fixture.projects.slice(0, 1)
  const roots = fixture.tasks.filter(task => !task.parent_id).slice(0, 55)
    .map((task, index) => ({ ...task, status: index === 0 ? 'blocked' : 'in_progress' }))
  const child = { ...roots[0], id: 'active-child', parent_id: roots[0].id, title: 'Active child', status: 'in_progress' }
  const orphan = { ...roots[0], id: 'inactive-orphan', parent_id: null, project_id: null, title: 'Inactive orphan', status: 'todo' }
  fixture.tasks = [...roots, child, orphan]
  fixture.sessionActivity = id => id !== roots[0].id && id !== orphan.id
  const h = await openHierarchy(fixture)
  try {
    await h.page.addInitScript(id => localStorage.setItem('hmem-ws-' + id, JSON.stringify({
      filterShowOnly: 'tasks', filterHasActiveSession: true, filterShowEmptyProjects: false,
      filterProjectStatuses: ['archived'], filterTaskStatuses: ['todo', 'in_progress', 'blocked'], nodesCollapsedByDefault: true
    })), fixture.workspace.id)
    await h.start('tab=projects', roots[1].id); await h.idle()
    assert.equal(await h.page.locator('.card-project').count(), 0)
    assert.equal(h.requests.some(request => request.parent === fixture.projects[0].id), false, 'Flat discovery never walks a hidden project')
    await h.scrollTo('root-status:task')
    await h.page.getByRole('button', { name: 'Load more tasks', exact: true }).click(); await h.idle()
    let keys = await h.logicalKeys()
    for (const task of roots) assert.ok(keys.has('task:' + task.id), task.id)
    assert.equal(keys.has('task:' + orphan.id), false)
    assert.ok(h.requests.some(request => request.kind === 'workspace_root' && request.taskOffset === 50))
    await h.scrollTo('task:' + roots[0].id)
    await h.page.locator('#entity-' + roots[0].id + ' .tree-toggle').click(); await h.idle()
    await h.page.locator('#entity-' + child.id).waitFor({ state: 'visible' })
    const active = h.page.getByRole('button', { name: 'Has active session', exact: true })
    await active.click(); await h.idle()
    await active.click(); await h.idle()
    await h.scrollTo('root-status:task')
    await h.page.getByRole('button', { name: 'Load more tasks', exact: true }).click(); await h.idle()
    keys = await h.logicalKeys()
    assert.ok(keys.has('task:' + roots[0].id), 'A descendant session retains its parent after cached toggles')
    assert.equal(keys.has('task:' + orphan.id), false)
    fixture.sessionActivity = () => false
    await active.click(); await h.idle(); await active.click(); await h.idle()
    await h.page.getByText('No projects or tasks match the current filters.', { exact: true }).waitFor({ state: 'visible' })
    assert.equal(await h.page.locator('[data-hierarchy-key^="task:"]').count(), 0)
    assert.ok(h.caps().maxBranches <= 4); assert.ok(h.caps().maxDetails <= 6)
    assert.deepEqual(h.errors, []); assert.deepEqual(h.unhandled, [])
  } finally { await h.close() }
})
