import assert from 'node:assert/strict'
import test from 'node:test'
import { mkdir } from 'node:fs/promises'
import { join } from 'node:path'
import { hierarchyFixture, openHierarchy } from './hierarchy-fixture.mjs'
import { projectReadinessRollup, taskReadinessRollup } from '../perf/fixtures.mjs'

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

function prerequisiteFixture() {
  const data = hierarchyFixture(), base = data.tasks[0], project = data.projects[0]
  data.projects = [project, { ...project, id: 'hidden-project', name: 'Hidden prerequisites', parent_id: project.id }]
  data.tasks = [
    { ...base, id: 'own', title: 'Own prerequisite', status: 'blocked', dependency_count: 1 },
    { ...base, id: 'eligible-parent', title: 'Eligible parent', status: 'blocked', auto_blocked: true },
    { ...base, id: 'own-child', title: 'Child prerequisite', parent_id: 'eligible-parent', status: 'blocked', dependency_count: 1 },
    { ...base, id: 'prerequisite', title: 'Unloaded prerequisite', project_id: 'hidden-project' },
    { ...base, id: 'manual', title: 'Manual block', status: 'blocked', auto_blocked: false },
  ]
  data.dependencies = ['own', 'own-child'].map(task_id => ({ task_id, depends_on_id: 'prerequisite' }))
  return data
}
const statusControl = (h, id) => h.page.locator('#entity-' + id + ' .status-select').first()
async function transitionAvailability(h, id) {
  return statusControl(h, id).locator('option').evaluateAll(options => Object.fromEntries(options.map(option => [option.value, !option.disabled])))
}
function prerequisiteFrame(workspace, eventId) {
  // V022 emits exact dependent readiness IDs even when their links and endpoint
  // rows were never disclosed. Ancestor readiness is carried independently.
  return { schema_version: 1, type: 'change', event: { schema_version: 1, event_id: eventId,
    scope: 'workspace', workspace_id: workspace, occurred_at: '2026-10-19T12:00:00Z',
    transaction: { id: eventId, cause: 'rest', request_id: null }, actor: { type: 'service', id: 'controlled-browser' },
    entity: { type: 'task', id: 'prerequisite', action: 'updated' }, invalidations: [
      { kind: 'entity', target: 'task:prerequisite' },
      ...['own', 'own-child', 'eligible-parent'].map(id => ({ kind: 'readiness', target: 'task:' + id })),
      { kind: 'readiness', target: 'project:root-project' },
    ] } }
}

for (const focused of [false, true]) for (const subtask of [false, true]) {
  test(`done label and value survive ${focused ? 'focused' : 'hierarchy'} ${subtask ? 'subtask' : 'task'} status changes`, { timeout: 60000 }, async () => {
    const data = hierarchyFixture(), base = data.tasks[0], target = subtask ? 'done-child' : 'done-task'
    data.projects = [data.projects[0]]
    data.tasks = [
      { ...base, id: 'parent', status: 'blocked', auto_blocked: true },
      { ...base, id: 'done-task', status: 'done' },
      { ...base, id: 'done-child', parent_id: 'parent', status: 'done' },
      { ...base, id: 'open-child', parent_id: 'parent', status: 'todo' },
    ]
    const h = await openHierarchy(data, [], { updateTask(fixture, id, update) {
      assert.equal(id, target)
      assert.ok(['todo', 'done'].includes(update.status))
      const task = fixture.tasks.find(task => task.id === id)
      task.status = update.status
      return task
    } })
    try {
      await h.page.addInitScript(({ id, statuses }) => localStorage.setItem('hmem-ws-' + id, JSON.stringify({ filterTaskStatuses: statuses })), { id: data.workspace.id, statuses })
      await h.start(focused ? 'tab=projects&focus=task:' + target : 'tab=projects', focused ? target : 'root-project')
      await h.idle(); await h.scrollTo('task:' + target)
      const control = statusControl(h, target)
      assert.equal(await control.inputValue(), 'done')
      assert.equal(await control.locator('option:checked').innerText(), 'done')
      assert.deepEqual(await control.locator('option').allTextContents(), ['todo', 'in progress', 'blocked', 'done', 'cancelled'])
      await control.selectOption('todo'); await h.idle()
      await control.selectOption({ label: 'done' }); await h.idle()
      assert.deepEqual(h.requests.filter(r => r.path === '/api/v1/tasks/' + target && r.method === 'PUT').map(r => r.update.status), ['todo', 'done'])
      assert.equal(await control.inputValue(), 'done')
      assert.equal(await control.locator('option:checked').innerText(), 'done')
      if (!focused) {
        await h.scrollTo('project:root-project')
        const project = h.page.locator('#entity-root-project .card-project .status-select').first()
        assert.equal(await project.inputValue(), 'active')
        assert.equal(await project.locator('option[value="completed"]').innerText(), 'completed')
        assert.equal(await project.locator('option[value="done"]').count(), 0)
      }
    } finally { await h.close() }
  })
}

test('canonical prerequisite authority gates hierarchy and focused task/subtask controls before detail or dependency disclosure', { timeout: 60000 }, async () => {
  for (const target of ['own', 'own-child']) {
    const h = await openHierarchy(prerequisiteFixture())
    const detail = h.gate(r => r.path === '/api/v1/tasks/' + target, { hold: true })
    try {
      await h.start('tab=projects&focus=task:' + target, target)
      await h.bounded(detail.arrived, 5000, 'Focused detail request')
      assert.deepEqual(await transitionAvailability(h, target), { todo: false, in_progress: false, blocked: true, done: false, cancelled: true })
      assert.equal(await statusControl(h, target).inputValue(), 'blocked')
      assert.equal(await statusControl(h, target).locator('option[value="done"]').innerText(), 'done')
      assert.doesNotMatch(await h.page.locator('#entity-' + target).innerText(), /Finish or cancel \d+ open prerequisite/)
      assert.match(await statusControl(h, target).locator('option[value="in_progress"]').getAttribute('title'), /open prerequisite/)
      assert.equal(h.requests.some(r => r.path.endsWith('/dependencies') || r.path.endsWith('/overview') || r.path === '/api/v1/tasks/prerequisite'), false)
    } finally { detail.release(); await h.close() }
  }
  const h = await openHierarchy(prerequisiteFixture())
  const hidden = h.gate(r => r.kind === 'project' && r.parent === 'hidden-project', { hold: true })
  const ownDetail = h.gate(r => r.path === '/api/v1/tasks/own', { hold: true })
  const childDetail = h.gate(r => r.path === '/api/v1/tasks/own-child', { hold: true })
  try {
    await h.page.setViewportSize({ width: 1600, height: 1200 })
    await h.start(); await h.bounded(hidden.arrived, 5000, 'Hidden branch request')
    await h.page.locator('#entity-hidden-project .tree-toggle').first().click(); hidden.release()
    for (const id of ['own', 'own-child']) {
      await h.page.locator('#entity-' + id).waitFor()
      assert.deepEqual(await transitionAvailability(h, id), { todo: false, in_progress: false, blocked: true, done: false, cancelled: true })
      assert.doesNotMatch(await h.page.locator('#entity-' + id).innerText(), /Finish or cancel \d+ open prerequisite/)
      assert.match(await statusControl(h, id).locator('option[value="in_progress"]').getAttribute('title'), /open prerequisite/)
    }
    assert.equal(await h.page.locator('#entity-prerequisite').count(), 0)
    assert.equal(h.requests.some(r => r.path.endsWith('/dependencies') || r.path.endsWith('/overview')), false)
    ownDetail.release(); childDetail.release(); await h.idle()
    assert.deepEqual(await transitionAvailability(h, 'manual'), { todo: true, in_progress: true, blocked: true, done: true, cancelled: true })
    assert.match(await statusControl(h, 'eligible-parent').locator('option[value="in_progress"]').getAttribute('title'), /incomplete subtasks/)
    h.fixture.tasks.find(task => task.id === 'prerequisite').status = 'cancelled'
    await h.page.evaluate(frame => window.pushHierarchyFrames([frame]), prerequisiteFrame(h.fixture.workspace.id, 'prerequisite-cancelled'))
    await h.idle()
    for (const id of ['own', 'own-child']) assert.deepEqual(await transitionAvailability(h, id), { todo: true, in_progress: true, blocked: true, done: true, cancelled: true })
    assert.equal(h.requests.some(r => r.path.endsWith('/dependencies') || r.path.endsWith('/overview')), false)
    assert.ok(h.caps().maxBranches <= 4); assert.ok(h.caps().maxDetails <= 6)
    assert.deepEqual(h.errors, []); assert.deepEqual(h.unhandled, [])
  } finally { hidden.release(); ownDetail.release(); childDetail.release(); await h.close() }
})

test('legacy unknown eligibility retries bounded summaries and delayed ready replies cannot undo a live prerequisite blocker', { timeout: 60000 }, async () => {
  const h = await openHierarchy(prerequisiteFixture())
  const legacy = h.gate(r => r.path.includes('/navigation/focus/task/own'), { transform(value) { delete value.target.summary.readiness_rollup.own_open_dependency_count; return value } })
  const detail = h.gate(r => r.path === '/api/v1/tasks/own', { hold: true })
  try {
    await h.start('tab=projects&focus=task:own', 'own')
    await h.bounded(detail.arrived, 5000, 'Legacy detail request')
    assert.ok(legacy.used)
    assert.equal((await transitionAvailability(h, 'own')).in_progress, false)
    assert.match(await h.page.locator('#entity-own .card-filter-context-note').innerText(), /Prerequisite eligibility is unavailable\. Refresh to retry\./)
    const retry = h.page.locator('#entity-own').getByRole('button', { name: 'Refresh eligibility' })
    const failed = h.gate(r => r.path.endsWith('/navigation/summaries') && r.taskIds.includes('own'), { error: 503 })
    await retry.click(); await h.bounded(failed.arrived, 5000, 'Failed eligibility retry')
    await h.page.waitForTimeout(50)
    assert.equal((await transitionAvailability(h, 'own')).done, false)
    assert.match(await h.page.locator('#entity-own .card-filter-context-note').innerText(), /Prerequisite eligibility is unavailable\. Refresh to retry\./)
    const ready = h.gate(r => r.path.endsWith('/navigation/summaries') && r.taskIds.includes('own'), { hold: true, transform(value) { value.tasks.find(task => task.id === 'own').readiness_rollup.own_open_dependency_count = 0; return value } })
    await retry.click(); await h.bounded(ready.arrived, 5000, 'Delayed ready summary')
    const current = h.gate(r => r.path.endsWith('/navigation/summaries') && r.taskIds.includes('own'), { hold: true })
    await h.page.evaluate(frame => window.pushHierarchyFrames([frame]), prerequisiteFrame(h.fixture.workspace.id, 'new-prerequisite-authority'))
    await h.bounded(current.arrived, 5000, 'Current prerequisite summary')
    assert.equal((await transitionAvailability(h, 'own')).done, false, 'refresh retires prior authority before completion')
    current.release(); await h.page.waitForTimeout(50); ready.release(); detail.release(); await h.idle()
    assert.deepEqual(await transitionAvailability(h, 'own'), { todo: false, in_progress: false, blocked: true, done: false, cancelled: true })
    const retries = h.requests.filter(r => r.path.endsWith('/navigation/summaries'))
    assert.ok(retries.length >= 3); assert.ok(retries.every(r => r.taskIds.length + r.projectIds.length <= 100))
    assert.equal(h.requests.some(r => r.path.endsWith('/dependencies') || r.path.endsWith('/overview')), false)
    assert.deepEqual(h.errors, []); assert.deepEqual(h.unhandled, [])
  } finally { detail.release(); await h.close() }
})

test('closed prerequisites recover subtask starts after a failed parent eligibility refresh', { timeout: 60000 }, async () => {
  for (const closedStatus of ['done', 'cancelled']) for (const focused of [false, true]) {
    const data = prerequisiteFixture()
    data.tasks = data.tasks.filter(task => ['eligible-parent', 'own-child', 'prerequisite'].includes(task.id))
    data.dependencies = ['eligible-parent', 'own-child'].map(task_id => ({ task_id, depends_on_id: 'prerequisite' }))
    const authority = { updateTask(fixture, id, update) {
      assert.equal(update.status, 'in_progress')
      assert.equal(taskReadinessRollup(fixture, id).own_open_dependency_count, 0)
      assert.equal(taskReadinessRollup(fixture, 'eligible-parent').can_start_subtasks, true)
      const task = fixture.tasks.find(value => value.id === id); task.status = update.status; return task
    } }
    const h = await openHierarchy(data, [], authority)
    try {
      await h.start(focused ? 'tab=projects&focus=task:own-child' : 'tab=projects', focused ? 'own-child' : 'root-project')
      await h.idle()
      if (!focused) await h.scrollTo('task:own-child')
      await statusControl(h, 'own-child').locator('option[value="in_progress"]').waitFor({ state: 'attached' })
      assert.equal((await transitionAvailability(h, 'own-child')).in_progress, false)
      const failed = h.gate(r => r.path.endsWith('/navigation/summaries') && r.taskIds.includes('eligible-parent'), { error: 503 })
      h.fixture.tasks.find(task => task.id === 'prerequisite').status = closedStatus
      const frame = prerequisiteFrame(h.fixture.workspace.id, 'parent-recovery-' + closedStatus)
      frame.event.invalidations = frame.event.invalidations.filter(value => ['task:prerequisite', 'task:own-child', 'task:eligible-parent'].includes(value.target))
      await h.page.evaluate(value => window.pushHierarchyFrames([value]), frame)
      await h.bounded(failed.arrived, 5000, 'Failed parent eligibility refresh'); await h.idle()
      assert.equal((await transitionAvailability(h, 'own-child')).in_progress, false)
      assert.doesNotMatch(await statusControl(h, 'own-child').locator('option[value="in_progress"]').getAttribute('title'), /open prerequisite/)
      const retry = h.page.locator('#entity-own-child').getByRole('button', { name: 'Refresh eligibility' })
      assert.equal(await retry.count(), 1, 'closed own prerequisites must retain recovery for unavailable parent authority')
      const recovered = h.gate(r => r.path.endsWith('/navigation/summaries') && r.taskIds.includes('own-child'), {})
      await retry.click(); await h.bounded(recovered.arrived, 5000, 'Subtask eligibility recovery'); await h.idle()
      assert.equal((await transitionAvailability(h, 'own-child')).in_progress, true,
        'closed own prerequisites must become startable after recovery: ' + JSON.stringify({ retryTaskIds: recovered.receipt.taskIds,
          reason: await statusControl(h, 'own-child').locator('option[value="in_progress"]').getAttribute('title'),
          recoveryButtons: await retry.count() }))
      assert.ok(recovered.receipt.taskIds.includes('eligible-parent'), 'retry must refresh canonical parent eligibility with the child')
      assert.equal(await retry.count(), 0)
      await statusControl(h, 'own-child').selectOption('in_progress'); await h.idle()
      assert.equal(h.requests.filter(r => r.path === '/api/v1/tasks/own-child' && r.method === 'PUT').length, 1)
      assert.equal(await statusControl(h, 'own-child').inputValue(), 'in_progress')
      assert.equal(h.requests.some(r => r.path.endsWith('/dependencies') || r.path.endsWith('/overview')), false)
      assert.ok(h.caps().maxBranches <= 4); assert.ok(h.caps().maxDetails <= 6)
    } finally { await h.close() }
  }
})

test('legacy-topology authority permits eligible subtask beneath an automatic parent', { timeout: 60000 }, async () => {
  for (const focused of [false, true]) {
    const data = prerequisiteFixture(), base = data.tasks[0]
    data.tasks = [
      { ...base, id: 'parent', title: 'Integrate capabilities, shared REST and legacy HTTP', status: 'blocked', auto_blocked: true, dependency_count: 0 },
      { ...base, id: 'child', title: 'Port the isolated-session HTTP compatibility adapter', parent_id: 'parent', status: 'todo', dependency_count: 2 },
      { ...base, id: 'sibling-a', title: 'Sibling A', parent_id: 'parent', status: 'blocked', dependency_count: 3 },
      { ...base, id: 'sibling-b', title: 'Sibling B', parent_id: 'parent', status: 'blocked', dependency_count: 2 },
      { ...base, id: 'finished-child', title: 'Finished child', parent_id: 'parent', status: 'done', dependency_count: 0 },
      ...['done', 'cancelled'].map(status => ({ ...base, id: 'closed-' + status, title: status, project_id: 'hidden-project', status, dependency_count: 0 })),
      ...Array.from({ length: 5 }, (_, index) => ({ ...base, id: 'open-' + index, title: 'Open prerequisite ' + index, project_id: 'hidden-project', status: 'todo', dependency_count: 0 })),
    ]
    data.dependencies = [
      ...['done', 'cancelled'].map(status => ({ task_id: 'child', depends_on_id: 'closed-' + status })),
      ...Array.from({ length: 5 }, (_, index) => ({ task_id: index < 3 ? 'sibling-a' : 'sibling-b', depends_on_id: 'open-' + index })),
    ]
    const parent = taskReadinessRollup(data, 'parent'), child = taskReadinessRollup(data, 'child')
    assert.equal(parent.open_subtask_count, 3); assert.equal(parent.done_subtask_count, 1)
    assert.equal(parent.open_dependency_count, 5); assert.equal(parent.own_open_dependency_count, 0)
    assert.equal(parent.can_start_subtasks, true); assert.equal(child.own_open_dependency_count, 0)
    const h = await openHierarchy(data, [], { updateTask(fixture, id, update) {
      assert.equal(id, 'child'); assert.equal(update.status, 'in_progress')
      const task = fixture.tasks.find(value => value.id === id); task.status = update.status; return task
    } })
    try {
      await h.start(focused ? 'tab=projects&focus=task:child' : 'tab=projects', focused ? 'child' : 'root-project'); await h.idle()
      if (!focused) assert.match(await h.page.locator('#entity-parent').innerText(), /5 open dependencies/)
      await h.scrollTo('task:child')
      assert.equal((await transitionAvailability(h, 'child')).in_progress, true)
      await statusControl(h, 'child').selectOption('in_progress'); await h.idle()
      assert.equal(h.requests.filter(r => r.path === '/api/v1/tasks/child' && r.method === 'PUT').length, 1)
      assert.equal(await statusControl(h, 'child').inputValue(), 'in_progress')
    } finally { await h.close() }
  }
})

test('five task statuses and mixed subtasks retain independent palette, neutral blocked semantics and single opacity', { timeout: 60000 }, async () => {
  const h = await openHierarchy(fixture())
  try {
    await h.page.addInitScript(({ id, statuses }) => localStorage.setItem('hmem-ws-' + id, JSON.stringify({ filterTaskStatuses: statuses })), { id: h.fixture.workspace.id, statuses })
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
