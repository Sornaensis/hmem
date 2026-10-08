import assert from 'node:assert/strict'
import test from 'node:test'
import { mkdir } from 'node:fs/promises'
import { join } from 'node:path'
import { hierarchyFixture, openHierarchy } from './hierarchy-fixture.mjs'
import { projectReadinessRollup } from '../perf/fixtures.mjs'

const paint = page => page.evaluate(() => new Promise(resolve => requestAnimationFrame(() => requestAnimationFrame(resolve))))
const statuses = ['todo', 'in_progress', 'blocked', 'done', 'cancelled']
const colors = { todo: 'rgb(30, 41, 59)', blocked: 'rgb(30, 41, 59)', in_progress: 'rgba(14, 165, 233, 0.06)', done: 'rgba(34, 197, 94, 0.06)', cancelled: 'rgba(148, 163, 184, 0.06)' }
function fixture() {
  const data = hierarchyFixture(), base = data.tasks[0]
  data.projects = [data.projects[0]]
  data.tasks = statuses.map(status => ({ ...base, id: 'status-' + status, title: status, status }))
  data.tasks.push(...statuses.map(status => ({ ...base, id: 'child-' + status, title: 'Child ' + status, status, parent_id: 'status-in_progress' })))
  data.tasks.push({ ...base, id: 'closed-child', title: 'Cancelled child', status: 'cancelled', parent_id: 'status-cancelled' })
  data.tasks.push(...Array.from({ length: 75 }, (_, i) => ({ ...base, id: 'tail-' + String(i).padStart(3, '0'), title: 'Tail ' + i, parent_id: 'status-in_progress' })))
  return data
}
async function shot(h, name) {
  if (!process.env.HMEM_STATUS_SHOTS) return
  await mkdir(process.env.HMEM_STATUS_SHOTS, { recursive: true })
  await h.page.screenshot({ path: join(process.env.HMEM_STATUS_SHOTS, name) })
}
async function appearance(h, id) {
  await h.scrollTo('task:' + id); await h.page.mouse.move(0, 0); await paint(h.page)
  return h.page.locator('#entity-' + id).evaluate(card => {
    const style = getComputedStyle(card), family = card.closest('[data-task-family]'), familyStyle = getComputedStyle(family)
    return { background: style.backgroundColor, border: style.borderTopColor, opacity: style.opacity, className: card.className,
      familyClass: family.className, familyBackground: familyStyle.backgroundColor, familyOpacity: familyStyle.opacity }
  })
}

test('five task statuses and mixed subtasks retain independent palette, neutral blocked semantics and single opacity', { timeout: 60000 }, async () => {
  const h = await openHierarchy(fixture())
  try {
    await h.page.setViewportSize({ width: 1600, height: 1000 })
    await h.start(); await h.idle()
    for (const status of statuses) {
      const parent = await appearance(h, 'status-' + status)
      assert.equal(parent.background, colors[status], JSON.stringify(parent))
      assert.equal(parent.familyBackground, colors[status], JSON.stringify(parent))
      assert.equal(parent.opacity, status === 'cancelled' ? '0.75' : '1')
      assert.equal(parent.familyOpacity, '1', 'family must not multiply individual card opacity')
      assert.equal(parent.familyClass.includes('card-status-' + status), status !== 'blocked')
      if (status === 'blocked') {
        assert.ok(!parent.className.includes('card-status-blocked'))
        assert.equal(await h.page.locator('#entity-status-blocked select').first().inputValue(), 'blocked')
      }
      const child = await appearance(h, 'child-' + status)
      assert.equal(child.background, colors[status], JSON.stringify(child))
      assert.equal(child.familyBackground, colors.in_progress)
      assert.equal(child.familyOpacity, '1')
    }
    const closed = await appearance(h, 'closed-child')
    assert.equal(closed.opacity, '0.75'); assert.equal(closed.familyOpacity, '1')
    await h.scrollTo('task:status-in_progress')
    await h.page.locator('#entity-status-in_progress').evaluate(card => { const scroll = document.getElementById('main-content-scroll'); scroll.scrollTop += card.getBoundingClientRect().top - scroll.getBoundingClientRect().top - 120 })
    await paint(h.page); await shot(h, 'task-status-palette.png')
    await h.scrollTo('task:tail-060')
    assert.equal(await h.page.locator('#entity-status-in_progress').count(), 0, 'parent stays outside ordinary window')
    assert.ok(await h.page.locator('[data-task-family="status-in_progress"]').count() > 0)
    assert.equal(await h.page.locator('[data-task-family="status-in_progress"]').evaluateAll(families => families.every(family => family.classList.contains('card-status-in_progress'))), true)
    await h.page.locator('#entity-tail-060 .editable-text').first().click()
    const input = h.page.locator('#entity-tail-060 .inline-edit-input')
    await input.waitFor(); await input.evaluate(element => { element.setSelectionRange(2, 5); window.statusEditor = element; window.statusEditorRow = element.closest('[data-hierarchy-key]') })
    await h.page.locator('#main-content-scroll').evaluate(element => { element.scrollTop = 0 }); await paint(h.page)
    assert.deepEqual(await input.evaluate(element => ({ same: element === window.statusEditor, row: element.closest('[data-hierarchy-key]') === window.statusEditorRow, focused: document.activeElement === element, selection: [element.selectionStart, element.selectionEnd] })), { same: true, row: true, focused: true, selection: [2, 5] })
    const families = await h.page.locator('[data-task-family="status-in_progress"]').evaluateAll(values => values.map(family => ({ status: family.classList.contains('card-status-in_progress'), width: family.getBoundingClientRect().width,
      contained: [...family.querySelectorAll('.card-subtask')].every(child => child.getBoundingClientRect().left > family.getBoundingClientRect().left && child.getBoundingClientRect().right < family.getBoundingClientRect().right) })))
    assert.ok(families.length >= 2, 'protected editor splits the canonical family into stable segments')
    assert.ok(families.every(family => family.status && family.width <= 78 * 16 && family.contained), JSON.stringify(families))
    assert.ok(await h.page.locator('[data-hierarchy-key]').count() <= 27)
    await h.page.keyboard.press('Escape')
  } finally { await h.close() }
})

test('collapsed Projects show blue canonical deep unloaded activity and clear it after positive-to-zero refresh', { timeout: 60000 }, async () => {
  const data = hierarchyFixture(), baseProject = data.projects[0], baseTask = data.tasks[0]
  data.projects = [baseProject, { ...baseProject, id: 'deep-project', name: 'Deep project', parent_id: baseProject.id }, { ...baseProject, id: 'leaf-project', name: 'Leaf project', parent_id: 'deep-project' }]
  data.tasks = [{ ...baseTask, id: 'deep-task', project_id: 'leaf-project', status: 'in_progress' }, { ...baseTask, id: 'deep-subtask', project_id: 'leaf-project', parent_id: 'deep-task', status: 'in_progress' }]
  assert.equal(projectReadinessRollup(data, baseProject.id).in_progress_task_count, 2)
  const h = await openHierarchy(data)
  const held = h.gate(receipt => receipt.kind === 'project' && receipt.parent === baseProject.id, { hold: true })
  try {
    await h.page.setViewportSize({ width: 1600, height: 1000 })
    await h.start(); await h.bounded(held.arrived, 5000, 'Root branch arrival')
    assert.equal(h.requests.some(receipt => receipt.path.endsWith('/deep-task') || receipt.path.endsWith('/deep-subtask')), false, 'deep activity is not inferred from hydrated tasks')
    const card = h.page.locator('#entity-root-project .card-project')
    assert.equal(await card.evaluate(element => getComputedStyle(element).borderLeftColor), 'rgb(14, 165, 233)')
    await card.locator('.tree-toggle').click(); held.release(); await h.idle(); await h.page.mouse.move(0, 0); await paint(h.page)
    assert.ok(await card.evaluate(element => element.classList.contains('card-project-in-progress')))
    await shot(h, 'project-deep-activity.png')
    for (const task of h.fixture.tasks) task.status = 'done'
    assert.equal(projectReadinessRollup(h.fixture, baseProject.id).in_progress_task_count, 0)
    await h.page.evaluate(workspace => window.pushHierarchyFrames([{ schema_version: 1, type: 'change', event: { schema_version: 1, event_id: 'status-zero', scope: 'workspace', workspace_id: workspace, occurred_at: '2026-10-08T12:00:00Z', transaction: { id: 'status-zero-tx', cause: 'rest', request_id: null }, actor: { type: 'system', id: null }, entity: { type: 'task', id: 'deep-task', action: 'updated' }, invalidations: [{ kind: 'entity', target: 'task:deep-task' }, { kind: 'tree', target: 'workspace:' + workspace }] } }]), h.fixture.workspace.id)
    await h.idle(); await paint(h.page)
    assert.equal(await card.evaluate(element => element.classList.contains('card-project-in-progress')), false)
    assert.equal(await card.evaluate(element => getComputedStyle(element).borderLeftColor), 'rgb(99, 102, 241)')
    assert.equal(await h.page.locator('#entity-deep-task').count(), 0)
  } finally { held.release(); await h.close() }
})
