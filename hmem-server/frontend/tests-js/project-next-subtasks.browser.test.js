import assert from 'node:assert/strict'
import test from 'node:test'
import { hierarchyFixture, openHierarchy } from './hierarchy-fixture.mjs'

function nextSubtasksFixture() {
  const fixture = hierarchyFixture(), project = fixture.projects[0], task = fixture.tasks[0]
  fixture.projects = [project]
  fixture.tasks = [
    ...Array.from({ length: 55 }, (_, i) => ({ ...task, id: 'filler-' + i, title: 'A filler ' + String(i).padStart(2, '0'), priority: 10, status: 'blocked', auto_blocked: false })),
    { ...task, id: 'eligible-parent', title: 'Eligible parent', status: 'blocked', auto_blocked: true },
    { ...task, id: 'ready-child', title: 'Ready child', parent_id: 'eligible-parent', priority: 9 },
    { ...task, id: 'blocked-child', title: 'Blocked child', parent_id: 'eligible-parent', status: 'blocked', auto_blocked: true },
    { ...task, id: 'manual-parent', title: 'Manual parent', status: 'blocked', auto_blocked: false },
    { ...task, id: 'manual-child', title: 'Manual child', parent_id: 'manual-parent' },
    { ...task, id: 'dependency-parent', title: 'Dependency parent', status: 'blocked', auto_blocked: true },
    { ...task, id: 'parent-gated-child', title: 'Parent gated child', parent_id: 'dependency-parent' },
    { ...task, id: 'prerequisite', title: 'Prerequisite' },
  ]
  fixture.dependencies = [
    { task_id: 'blocked-child', depends_on_id: 'prerequisite' },
    { task_id: 'dependency-parent', depends_on_id: 'prerequisite' },
  ]
  return fixture
}

test('project next tasks displays an unloaded eligible subtask and Jump focuses its identity', { timeout: 30000 }, async () => {
  const harness = await openHierarchy(nextSubtasksFixture())
  const missingChildDetail = harness.gate(request => /^\/api\/v1\/tasks\/ready-child(?:\/overview)?$/.test(request.path), { error: 404 })
  const missingParentDetail = harness.gate(request => /^\/api\/v1\/tasks\/eligible-parent(?:\/overview)?$/.test(request.path), { error: 404 })
  try {
    await harness.start()
    await harness.idle()
    assert.equal(await harness.page.locator('#entity-eligible-parent').count(), 0)
    assert.equal(await harness.page.locator('#entity-ready-child').count(), 0)
    await harness.page.locator('#entity-root-project .btn-extras-toggle').first().click()
    await harness.idle()
    const ready = harness.page.locator('#entity-root-project .project-next-tasks-section > .project-next-tasks-list .next-task-candidate')
    assert.deepEqual(await ready.locator('.popover-card-title').allTextContents(), ['Ready child', 'Prerequisite'])
    const child = ready.filter({ hasText: 'Ready child' })
    assert.equal(await child.locator('.entity-type-label').innerText(), 'SUB')
    assert.equal(await child.locator('.popover-card-priority').innerText(), 'P9')
    assert.equal(await child.locator('.next-task-rationale').innerText(), 'Ready subtask: parent allows subtask work.')
    assert.equal(await child.getByRole('button', { name: 'Start', exact: true }).count(), 0)
    missingChildDetail.error = null
    missingParentDetail.error = null
    await child.getByRole('button', { name: 'Jump', exact: true }).click()
    await harness.idle()
    assert.equal(new URLSearchParams(new URL(harness.page.url()).hash.slice(1)).get('focus'), 'task:ready-child')
    assert.match(await harness.page.locator('#entity-ready-child').innerText(), /Ready child/)
  } finally {
    await harness.close()
  }
})
