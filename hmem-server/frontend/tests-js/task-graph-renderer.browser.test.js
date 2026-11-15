import assert from 'node:assert/strict'
import test from 'node:test'
import { mkdir, readFile, rm } from 'node:fs/promises'
import { execFile } from 'node:child_process'
import { promisify } from 'node:util'
import { resolve } from 'node:path'
import { hierarchyFixture, openHierarchy } from './hierarchy-fixture.mjs'

function fixture() {
  const f = hierarchyFixture(), p = f.projects[0], t = f.tasks[0]
  f.projects = [{ ...p, id: 'root-project', name: 'Graph project' }, { ...p, id: 'external', parent_id: null, name: 'External project' }]
  f.tasks = [
    { ...t, id: 'parent', parent_id: null, title: 'Render task cards', status: 'blocked' },
    { ...t, id: 'child-a', parent_id: 'parent', title: 'Layout subtasks', status: 'done' },
    { ...t, id: 'child-b', parent_id: 'parent', title: 'Connect subtask edges', status: 'todo' },
    { ...t, id: 'external-task', project_id: 'external', parent_id: null, title: 'External prerequisite', status: 'done' },
    { ...t, id: 'external-dependent', project_id: 'external', parent_id: null, title: 'External dependent', status: 'blocked' }
  ]
  f.dependencies = [{ task_id: 'child-b', depends_on_id: 'child-a' }, { task_id: 'parent', depends_on_id: 'external-task' }, { task_id: 'external-dependent', depends_on_id: 'parent' }]
  return f
}

async function openGraph(f = fixture(), options = {}) {
  const h = await openHierarchy(f)
  let count = 0
  await h.page.route('**/api/v1/change-stream/resync', async route => {
    if (route.request().postDataJSON().snapshot_profile !== 'full_v1') return route.fallback()
    count++
    if (options.wait && (!options.waitFirstOnly || count === 1)) await (typeof options.wait === 'function' ? options.wait(count) : options.wait)
    if (typeof options.failure === 'function' ? options.failure(count) : options.failure) return route.fulfill({ status: 500, json: { error: 'controlled' } })
    const item = (kind, data) => ({ schema_version: 1, kind, data })
    return route.fulfill({ json: { snapshot_profile: 'full_v1', items: [item('workspace', f.workspace), ...f.projects.map(v => item('project', v)), ...f.tasks.map(v => item('task', v)), ...f.dependencies.map(v => item('task_dependency', v))], has_more: false, resume_token: 'graph-snapshot' } })
  })
  return { ...h, snapshots: () => count }
}

const transform = async h => {
  await h.page.waitForFunction(() => document.getElementById('task-graph-canvas')?.dataset.graphTransform)
  return h.page.locator('#task-graph-canvas').evaluate(n => JSON.parse(n.dataset.graphTransform))
}
const world = h => h.page.locator('[data-graph-world]').evaluate(n => [...n.querySelectorAll('foreignObject')].map(v => [v.getAttribute('x'), v.getAttribute('y'), v.getAttribute('width'), v.getAttribute('height')]))

const diagramGeometry = h => h.page.locator('#task-graph-canvas').evaluate(canvas => {
  const box = n => ({ x: +n.getAttribute('x'), y: +n.getAttribute('y'), width: +n.getAttribute('width'), height: +n.getAttribute('height') })
  return { circles: [...canvas.querySelectorAll('[data-graph-project]')].map(n => ({ id: n.dataset.graphProject, x: +n.getAttribute('cx'), y: +n.getAttribute('cy'), radius: +n.getAttribute('r') })),
    labels: [...canvas.querySelectorAll('[data-graph-project-label]')].map(n => box(n.parentNode)),
    nodes: [...canvas.querySelectorAll('[data-graph-task]')].map(n => ({ id: n.dataset.graphTask, ...box(n.parentNode) })),
    edges: [...canvas.querySelectorAll('[data-prerequisite]')].map(n => ({ source: n.dataset.prerequisite, target: n.dataset.dependent, points: n.getAttribute('points').split(' ').map(pair => pair.split(',').map(Number)) })),
    bounds: JSON.parse(canvas.dataset.taskGraphCanvas).bounds }
})

function assertSafeSegments(geometry) {
  const intersects = (b, a, z) => a[0] === z[0]
    ? a[0] > b.x + 1e-6 && a[0] < b.x + b.width - 1e-6 && Math.max(Math.min(a[1], z[1]), b.y) < Math.min(Math.max(a[1], z[1]), b.y + b.height) - 1e-6
    : a[1] === z[1] ? a[1] > b.y + 1e-6 && a[1] < b.y + b.height - 1e-6 && Math.max(Math.min(a[0], z[0]), b.x) < Math.min(Math.max(a[0], z[0]), b.x + b.width) - 1e-6 : true
  for (const edge of geometry.edges) {
    const obstacles = [...geometry.labels, ...geometry.nodes.filter(n => n.id !== edge.source && n.id !== edge.target)]
    for (let i = 1; i < edge.points.length; i++) for (const obstacle of obstacles) assert.equal(intersects(obstacle, edge.points[i - 1], edge.points[i]), false, `edge ${edge.source}→${edge.target} crosses unrelated content`)
    for (const [x, y] of edge.points) assert.ok(x > geometry.bounds.x && y > geometry.bounds.y && x < geometry.bounds.x + geometry.bounds.width && y < geometry.bounds.y + geometry.bounds.height)
  }
}

test('nested project forest follows retained external ancestry and filters/live reparenting preserve scope and camera', { timeout: 35000 }, async () => {
  const f = fixture(), p = f.projects[0], t = f.tasks[0]
  p.parent_id = 'ancestor'
  f.projects.push(...[
    ['ancestor', 'outer'], ['outer', null], ['nested-a', 'root-project'], ['nested-b', 'root-project'], ['grandchild', 'nested-a'], ['empty', 'nested-b'], ['cut-intermediate', 'outer']
  ].map(([id, parent_id]) => ({ ...p, id, name: id, parent_id })))
  f.projects.find(v => v.id === 'external').parent_id = 'cut-intermediate'
  f.tasks.push(...[['ancestor-task', 'ancestor'], ['outer-task', 'outer'], ['a-task', 'nested-a'], ['b-task', 'nested-b'], ['grand-task', 'grandchild'], ['unassigned', null]].map(([id, project_id]) => ({ ...t, id, title: id, project_id, parent_id: null, status: 'todo' })))
  f.dependencies.push(...[
    ['parent', 'ancestor-task'], ['parent', 'outer-task'], ['parent', 'unassigned'], ['grand-task', 'parent'], ['b-task', 'grand-task'], ['grand-task', 'external-task']
  ].map(([task_id, depends_on_id]) => ({ task_id, depends_on_id })))
  const h = await openGraph(f)
  const nesting = geometry => {
    const circles = new Map(geometry.circles.map(c => [c.id, c]))
    for (const project of f.projects) if (circles.has(project.id) && circles.has(project.parent_id)) {
      const c = circles.get(project.id), parent = circles.get(project.parent_id)
      assert.ok(Math.hypot(c.x - parent.x, c.y - parent.y) + c.radius <= parent.radius + 1e-6, `${c.id} fits ${parent.id}`)
      assert.ok(geometry.circles.indexOf(parent) < geometry.circles.indexOf(c), 'ancestor is painted first')
    }
    assert.equal(circles.has('cut-intermediate'), false, 'missing retained intermediate remains a cut')
    assert.equal(circles.has(''), false, 'unassigned task has no invented project')
    const external = circles.get('external'), outer = circles.get('outer')
    assert.ok(Math.hypot(external.x - outer.x, external.y - outer.y) > external.radius + outer.radius, 'missing intermediate is never skipped')
    assertSafeSegments(geometry)
  }
  try {
    await h.start('tab=projects&focus=project%3Aroot-project'); await h.page.locator('#task-graph-open-root-project').click()
    await h.page.locator('[data-graph-live="true"]').waitFor(); await h.idle()
    const initial = await diagramGeometry(h)
    nesting(initial)
    const originalIds = initial.nodes.map(n => n.id).sort()
    assert.equal(originalIds.includes('cut-intermediate'), false)
    assert.equal(initial.circles.length, 8)
    const canvas = await h.page.locator('#task-graph-canvas').boundingBox()
    const inScope = ['parent', 'child-a', 'child-b', 'a-task', 'b-task', 'grand-task']
    const readable = await h.page.locator('[data-graph-task]').evaluateAll((nodes, ids) => nodes.filter(n => ids.includes(n.dataset.graphTask)).map(n => ({ box: n.getBoundingClientRect().toJSON(), font: parseFloat(getComputedStyle(n).fontSize) })), inScope)
    assert.ok(readable.some(({ box, font }) => font >= 14 && box.left >= canvas.x && box.right <= canvas.x + canvas.width && box.top >= canvas.y && box.bottom <= canvas.y + canvas.height), 'initial unsaved view shows a readable in-scope card')
    await h.page.getByRole('checkbox', { name: 'Show Done', exact: true }).uncheck()
    await h.page.locator('[data-graph-task="child-a"]').waitFor({ state: 'detached' }); await h.idle()
    nesting(await diagramGeometry(h))
    await h.page.getByRole('button', { name: 'Zoom in', exact: true }).click()
    const camera = await transform(h), before = await diagramGeometry(h)
    await h.page.locator('[data-graph-task="grand-task"]').evaluate(n => { window.nestedSurvivor = n; n.focus() })
    f.projects.find(v => v.id === 'grandchild').parent_id = 'root-project'
    await h.page.evaluate(frame => window.pushHierarchyFrames([frame]), graphEvent(f, 'nested-reparent', 'project', 'grandchild'))
    await h.page.waitForFunction(y => +document.querySelector('[data-graph-project="grandchild"]').getAttribute('cy') !== y, before.circles.find(c => c.id === 'grandchild').y)
    const after = await diagramGeometry(h); nesting(after)
    assert.deepEqual(after.nodes.map(n => n.id).sort(), before.nodes.map(n => n.id).sort())
    assert.deepEqual(await transform(h), camera)
    assert.equal(await h.page.evaluate(() => document.activeElement === window.nestedSurvivor), true)
    await h.page.getByRole('button', { name: 'Fit', exact: true }).click(); await h.idle()
    if (process.env.HMEM_TASK_GRAPH_NESTING_SCREENSHOT === '1') {
      await h.page.setViewportSize({ width: 1600, height: 1600 }); await h.page.getByRole('button', { name: 'Fit', exact: true }).click(); await h.idle()
      await mkdir('../../output/playwright/task-graph-nesting', { recursive: true })
      await h.page.screenshot({ path: '../../output/playwright/task-graph-nesting/nested-projects.png' })
    }
    assert.deepEqual(h.errors, []); assert.deepEqual(h.unhandled, [])
  } finally { await h.close() }
})

test('three-level project nesting shows own cards and cross-circle arrows in the current app style', { timeout: 30000 }, async () => {
  const f = fixture(), p = f.projects[0], t = f.tasks[0]
  f.projects = [{ ...p, name: 'Application' }, { ...p, id: 'frontend', name: 'Frontend', parent_id: p.id }, { ...p, id: 'graph', name: 'Task Graph', parent_id: 'frontend' }]
  f.tasks = [['application-task', p.id, 'Plan application', 'todo', false], ['frontend-task', 'frontend', 'Build interface', 'blocked', true], ['graph-task', 'graph', 'Draw task graph', 'blocked', true]].map(([id, project_id, title, status, auto_blocked]) => ({ ...t, id, project_id, title, status, auto_blocked, parent_id: null }))
  f.dependencies = [{ task_id: 'frontend-task', depends_on_id: 'application-task' }, { task_id: 'graph-task', depends_on_id: 'frontend-task' }]
  const h = await openGraph(f)
  try {
    await h.start(); await h.page.locator('#task-graph-open-root-project').click(); await h.page.locator('[data-graph-live="true"]').waitFor(); await h.idle()
    const geometry = await diagramGeometry(h)
    assert.equal(geometry.circles.length, 3); assert.equal(geometry.nodes.length, 3); assert.equal(geometry.edges.length, 2)
    assert.deepEqual(geometry.edges.map(edge => [edge.source, edge.target]).sort(), [['application-task', 'frontend-task'], ['frontend-task', 'graph-task']])
    for (const [id, badge] of [['application-task', 'TODO'], ['frontend-task', 'blocked'], ['graph-task', 'blocked']]) assert.equal(await h.page.locator(`[data-graph-task="${id}"] .task-graph-status`).innerText(), badge)
    assertSafeSegments(geometry)
    for (let i = 1; i < 3; i++) assert.ok(Math.hypot(geometry.circles[i].x - geometry.circles[i - 1].x, geometry.circles[i].y - geometry.circles[i - 1].y) + geometry.circles[i].radius < geometry.circles[i - 1].radius)
    const canvas = h.page.locator('#task-graph-canvas'), box = await canvas.boundingBox(), initial = await transform(h), graphUrl = h.page.url()
    // Start on bare SVG, away from every card, so native card clicks remain
    // independently exercised after the pan and wheel gestures.
    await h.page.mouse.move(box.x + 24, box.y + 24); await h.page.mouse.down()
    await h.page.mouse.move(box.x + 94, box.y + 64, { steps: 4 }); await h.page.mouse.up()
    const panned = await transform(h)
    assert.ok(Math.abs(panned.x - initial.x - 70) < 1 && Math.abs(panned.y - initial.y - 40) < 1)
    assert.equal(h.page.url(), graphUrl)
    await canvas.evaluate(n => n.addEventListener('wheel', event => { n.dataset.readinessWheelPoint = JSON.stringify({ x: event.clientX - n.getBoundingClientRect().left, y: event.clientY - n.getBoundingClientRect().top }) }, { once: true }))
    await h.page.mouse.move(box.x + box.width * .6, box.y + box.height * .4); await h.page.mouse.wheel(0, -120)
    const zoomed = await transform(h), point = await canvas.evaluate(n => JSON.parse(n.dataset.readinessWheelPoint))
    assert.ok(zoomed.scale > panned.scale)
    for (const axis of ['x', 'y']) assert.ok(Math.abs((point[axis] - panned[axis]) / panned.scale - (point[axis] - zoomed[axis]) / zoomed.scale) < .01)
    assert.equal(h.page.url(), graphUrl)
    await h.page.locator('[data-graph-task="application-task"]').click()
    await h.page.waitForURL(/focus=task%3Aapplication-task/)
    await h.page.locator('#entity-application-task').waitFor()
    await h.page.getByRole('button', { name: 'Back to graph', exact: true }).click(); await h.page.waitForURL(graphUrl)
    await h.page.waitForFunction(() => document.activeElement?.dataset.graphTask === 'application-task')
    assert.deepEqual(await transform(h), zoomed)
    if (process.env.HMEM_TASK_GRAPH_NESTING_SCREENSHOT === '1') {
      await h.page.setViewportSize({ width: 1600, height: 1600 }); await h.page.getByRole('button', { name: 'Fit', exact: true }).click(); await h.idle()
      await mkdir('../../output/playwright/task-graph-readiness', { recursive: true })
      await h.page.screenshot({ path: '../../output/playwright/task-graph-readiness/nested-three-levels-readiness.png' })
    }
    assert.deepEqual(h.errors, []); assert.deepEqual(h.unhandled, [])
  } finally { await h.close() }
})

for (const depth of [8, 32]) test(`deep nested chain opens on readable in-scope content before complete Fit (depth=${depth})`, { timeout: 30000 }, async () => {
  const f = fixture(), p = f.projects[0], t = f.tasks[0]
  f.projects = [p, ...Array.from({ length: depth - 1 }, (_, i) => ({ ...p, id: `level-${i + 1}`, name: `Level ${i + 1}`, parent_id: i === 0 ? p.id : `level-${i}` }))]
  f.tasks = [{ ...t, id: 'leaf', project_id: `level-${depth - 1}`, title: 'Readable nested leaf', parent_id: null, status: 'todo' }]; f.dependencies = []
  const h = await openGraph(f)
  try {
    await h.start(); await h.page.locator('#task-graph-open-root-project').click(); await h.page.locator('[data-graph-live="true"]').waitFor(); await h.idle()
    const canvas = await h.page.locator('#task-graph-canvas').boundingBox(), card = await h.page.locator('[data-graph-task="leaf"]').boundingBox()
    assert.equal((await transform(h)).scale, 1)
    assert.ok(card.x >= canvas.x && card.x + card.width <= canvas.x + canvas.width && card.y >= canvas.y && card.y + card.height <= canvas.y + canvas.height)
    const geometry = await diagramGeometry(h); assert.equal(geometry.circles.length, depth)
    await h.page.getByRole('button', { name: 'Fit', exact: true }).click(); const fitted = await transform(h)
    for (const c of geometry.circles) for (const [x, y] of [[c.x - c.radius, c.y - c.radius], [c.x + c.radius, c.y + c.radius]]) assert.ok(x * fitted.scale + fitted.x >= 0 && x * fitted.scale + fitted.x <= canvas.width && y * fitted.scale + fitted.y >= 0 && y * fitted.scale + fitted.y <= canvas.height)
    assert.deepEqual(h.errors, []); assert.deepEqual(h.unhandled, [])
  } finally { await h.close() }
})

test('status filters are independent local presentation, prune external orphans and retain stable native nodes', { timeout: 30000 }, async () => {
  const f = fixture(), t = f.tasks[0]
  f.tasks.push({ ...t, id: 'cancelled-parent', title: 'Cancelled parent', status: 'cancelled' },
    { ...t, id: 'open-child', title: 'Visible detached child', status: 'in_progress', parent_id: 'cancelled-parent' },
    { ...t, id: 'orphan', title: 'Cancelled-only neighbor', status: 'todo', project_id: 'external' })
  f.dependencies.push({ task_id: 'cancelled-parent', depends_on_id: 'orphan' })
  const h = await openGraph(f)
  try {
    await h.start(); await h.page.locator('#task-graph-open-root-project').click()
    await h.page.locator('[data-graph-live="true"]').waitFor(); await h.idle()
    const requests = [], request = value => { if (value.url().includes('/api/')) requests.push(value.url().split('?')[0]) }
    h.page.on('request', request)
    const beforeUrl = new URL(h.page.url()), beforeLength = await h.page.evaluate(() => history.length)
    const done = h.page.getByRole('checkbox', { name: 'Show Done', exact: true }), cancelled = h.page.getByRole('checkbox', { name: 'Show Cancelled', exact: true })
    assert.equal(await done.isChecked(), true); assert.equal(await cancelled.isChecked(), true)
    await h.page.locator('[data-graph-task="open-child"]').evaluate(node => { window.filterSurvivor = node })
    await done.focus(); await h.page.keyboard.press('Space')
    await h.page.locator('[data-graph-task="child-a"]').waitFor({ state: 'detached' })
    assert.equal(await h.page.locator('[data-graph-task="external-task"]').count(), 0)
    assert.equal(await h.page.locator('[data-graph-task="cancelled-parent"]').count(), 1)
    await cancelled.uncheck()
    await h.page.locator('[data-graph-task="cancelled-parent"]').waitFor({ state: 'detached' })
    assert.equal(await h.page.locator('[data-graph-task="orphan"]').count(), 0)
    const child = h.page.locator('[data-graph-task="open-child"]')
    assert.equal(await child.evaluate(n => n === window.filterSurvivor), true)
    assert.equal(await h.page.locator('rect.task-graph-family').count(), 1, 'only visible parent family remains')
    assert.equal(await h.page.locator('[data-prerequisite="orphan"]').count(), 0)
    if (process.env.HMEM_TASK_GRAPH_FILTER_SCREENSHOT === '1') {
      await h.page.setViewportSize({ width: 1600, height: 1200 })
      await h.page.getByRole('button', { name: 'Fit', exact: true }).click(); await h.idle()
      await mkdir('../../output/playwright/task-graph-filters', { recursive: true })
      await h.page.screenshot({ path: '../../output/playwright/task-graph-filters/filtered-graph.png' })
    }
    await done.check(); await h.page.locator('[data-graph-task="child-a"]').waitFor()
    assert.equal(await h.page.locator('[data-graph-task="cancelled-parent"]').count(), 0)
    await cancelled.check(); await h.page.locator('[data-graph-task="orphan"]').waitFor(); await h.idle()
    assert.equal(new URL(h.page.url()).hash, beforeUrl.hash)
    assert.equal(await h.page.evaluate(() => history.length), beforeLength)
    assert.equal(h.snapshots(), 1)
    assert.deepEqual(requests, [], 'checkboxes must not reload metadata, snapshots or live tickets')
    h.page.off('request', request)
    const admitted = await h.page.locator('[data-graph-task]').evaluateAll(nodes => nodes.map(n => n.dataset.graphTask))
    for (const edge of await h.page.locator('[data-prerequisite]').evaluateAll(nodes => nodes.map(n => [n.dataset.prerequisite, n.dataset.dependent]))) assert.ok(edge.every(id => admitted.includes(id)))
    assert.deepEqual(h.errors, []); assert.deepEqual(h.unhandled, [])
  } finally { await h.close() }
})

test('an explicit empty filter cannot refit a later ordinary live repopulation', { timeout: 30000 }, async () => {
  const f = fixture(); f.tasks = [{ ...f.tasks[0], status: 'done' }]; f.dependencies = []
  const h = await openGraph(f)
  try {
    await h.start(); await h.page.locator('#task-graph-open-root-project').click(); await h.page.locator('[data-graph-live="true"]').waitFor()
    await h.page.getByRole('button', { name: 'Zoom in', exact: true }).click()
    const camera = await transform(h)
    await h.page.getByRole('checkbox', { name: 'Show Done', exact: true }).uncheck()
    await h.page.getByRole('status').filter({ hasText: 'No tasks match filters' }).waitFor()
    assert.equal(await h.page.locator('#task-graph-canvas').count(), 0)
    f.tasks[0].status = 'todo'
    await h.page.evaluate(frame => window.pushHierarchyFrames([frame]), graphEvent(f, 'live-visible-after-empty-toggle', 'task', 'parent'))
    await h.page.locator('[data-graph-task="parent"]').waitFor(); await h.idle()
    assert.deepEqual(await transform(h), camera, 'the empty toggle was consumed before ordinary live population')
    await h.page.getByRole('checkbox', { name: 'Show Cancelled', exact: true }).uncheck()
    await h.page.waitForFunction(scale => JSON.parse(document.getElementById('task-graph-canvas').dataset.graphTransform).scale < scale, camera.scale)
    assert.equal(await h.page.getByRole('checkbox', { name: 'Show Done', exact: true }).isChecked(), false)
    assert.deepEqual(h.errors, []); assert.deepEqual(h.unhandled, [])
  } finally { await h.close() }
})

for (const focused of [false, true]) test(`filtered graph details preserve flags, same history slot and camera before hierarchy Back (focused=${focused})`, { timeout: 35000 }, async () => {
  const h = await openGraph()
  try {
    await h.start('tab=projects' + (focused ? '&focus=project%3Aroot-project' : '')); await h.idle()
    await h.page.locator('#task-graph-open-root-project').click(); await h.page.locator('[data-graph-live="true"]').waitFor()
    await h.page.getByRole('checkbox', { name: 'Show Cancelled', exact: true }).uncheck()
    await h.page.waitForURL(/graph-cancelled=0/); await h.idle()
    await h.page.getByRole('button', { name: 'Zoom in', exact: true }).click()
    const graphUrl = h.page.url(), viewport = await transform(h), length = await h.page.evaluate(() => history.length)
    await h.page.locator('[data-graph-task="child-b"]').focus(); await h.page.keyboard.press('Enter')
    await h.page.waitForURL(/focus=task%3Achild-b/)
    await h.page.locator('#task-child-b-details-toggle').click(); await h.idle()
    await h.page.locator('#entity-child-b summary').filter({ hasText: 'closed dependency' }).click()
    await h.page.locator('#entity-child-b').getByTitle('Jump to task', { exact: true }).click(); await h.page.waitForURL(/focus=task%3Achild-a/)
    await h.page.getByRole('button', { name: 'Back to graph', exact: true }).click(); await h.page.waitForURL(graphUrl)
    await h.page.waitForFunction(() => document.activeElement?.dataset.graphTask === 'child-b')
    assert.deepEqual(await transform(h), viewport)
    assert.equal(await h.page.getByRole('checkbox', { name: 'Show Cancelled', exact: true }).isChecked(), false)
    assert.equal(await h.page.evaluate(() => history.length), length)
    await h.page.getByRole('button', { name: 'Back', exact: true }).click(); await h.page.locator('#hierarchy-viewport').waitFor(); await h.idle()
    assert.equal(h.page.url().includes('focus='), focused)
    await h.page.goForward(); await h.page.locator('[data-graph-task="child-b"]').waitFor()
    assert.equal(h.page.url(), graphUrl)
    assert.deepEqual(await transform(h), viewport, 'Forward restores camera without an explicit toggle refit')
    assert.deepEqual(h.errors, []); assert.deepEqual(h.unhandled, [])
  } finally { await h.close() }
})

for (const empty of [false, true]) test(`a live-hidden detail target retires focus to filters without reviving cards (empty=${empty})`, { timeout: 30000 }, async () => {
  const f = fixture(); f.tasks = f.tasks.filter(t => t.id === 'parent' || (!empty && t.id === 'child-b')); f.dependencies = []
  f.tasks.forEach(t => { t.parent_id = null; t.status = 'todo' })
  const h = await openGraph(f)
  try {
    await h.start(); await h.page.locator('#task-graph-open-root-project').click(); await h.page.locator('[data-graph-live="true"]').waitFor()
    await h.page.locator('[data-graph-task="parent"]').evaluate(node => { window.queuedFilteredCard = node })
    await h.page.getByRole('checkbox', { name: 'Show Done', exact: true }).uncheck(); await h.page.waitForURL(/graph-done=0/)
    await h.page.getByRole('button', { name: 'Zoom in', exact: true }).click()
    const viewport = await transform(h), graphUrl = h.page.url()
    await h.page.locator('[data-graph-task="parent"]').click(); await h.page.waitForURL(/focus=task%3Aparent/)
    f.tasks[0].status = 'done'
    await h.page.evaluate(frame => window.pushHierarchyFrames([frame]), graphEvent(f, 'hidden-parent', 'task', 'parent'))
    await h.page.waitForTimeout(350)
    await h.page.getByRole('button', { name: 'Back to graph', exact: true }).click(); await h.page.waitForURL(graphUrl)
    await h.page.waitForFunction(() => document.activeElement?.id === 'task-graph-show-done')
    assert.equal(await h.page.locator('[data-graph-task="parent"]').count(), 0)
    await h.page.getByRole('status').filter({ hasText: 'The task is not visible' }).waitFor()
    if (empty) {
      assert.equal(await h.page.locator('#task-graph-canvas').count(), 0)
      await h.page.getByRole('status').filter({ hasText: 'No tasks match filters' }).waitFor()
      assert.equal(await h.page.getByRole('checkbox').count(), 2)
      if (process.env.HMEM_TASK_GRAPH_FILTER_SCREENSHOT === '1') {
        await mkdir('../../output/playwright/task-graph-filters', { recursive: true })
        await h.page.screenshot({ path: '../../output/playwright/task-graph-filters/filtered-empty.png' })
      }
    } else assert.deepEqual(await transform(h), viewport)
    await h.page.evaluate(() => window.queuedFilteredCard.click())
    await h.idle(); assert.equal(h.page.url(), graphUrl, 'stale native activation cannot open a hidden task')
    await h.page.getByRole('checkbox', { name: 'Show Done', exact: true }).check()
    await h.page.locator('[data-graph-task="parent"]').waitFor()
    assert.deepEqual(h.errors, []); assert.deepEqual(h.unhandled, [])
  } finally { await h.close() }
})

for (const focused of [false, true]) test(`graph→existing task→graph→origin retains history and viewport (focused=${focused})`, { timeout: 45000 }, async () => {
  const h = await openGraph()
  try {
    await h.start('tab=projects' + (focused ? '&focus=project%3Aroot-project' : '')); await h.idle()
    const query = h.page.locator('.hierarchy-search-input')
    await query.fill('Graph'); await h.idle()
    const originTop = await h.page.locator('#entity-root-project').evaluate(n => n.getBoundingClientRect().top)
    await h.page.locator('#task-graph-open-root-project').click()
    await h.page.locator('[data-graph-task="child-b"]').waitFor()
    assert.equal(h.snapshots(), 1)
    const graphUrl = h.page.url(), length = await h.page.evaluate(() => history.length)
    await h.page.getByRole('button', { name: 'Zoom in', exact: true }).click()
    const viewport = await transform(h)
    const task = h.page.locator('[data-graph-task="child-b"]')
    await task.focus(); await h.page.keyboard.press('Enter')
    await h.page.waitForURL(/focus=task%3Achild-b/)
    await h.page.locator('#entity-child-b').waitFor()
    await h.page.locator('#task-child-b-details-toggle').click()
    await h.idle()
    await h.page.locator('#entity-child-b summary').filter({ hasText: 'closed dependency' }).click()
    await h.page.locator('#entity-child-b').getByTitle('Jump to task', { exact: true }).click()
    await h.page.waitForURL(/focus=task%3Achild-a/)
    await h.page.locator('#entity-child-a').waitFor()
    await h.page.getByRole('button', { name: 'Back to graph', exact: true }).click()
    await h.page.waitForURL(graphUrl)
    await h.page.locator('#task-graph-canvas').waitFor()
    await h.page.waitForFunction(() => document.activeElement?.dataset.graphTask === 'child-b')
    assert.equal(await h.page.evaluate(() => history.length), length)
    assert.deepEqual(await transform(h), viewport)
    await h.page.locator('[data-graph-task="parent"]').click()
    await h.page.waitForURL(/focus=task%3Aparent/)
    await h.page.getByRole('button', { name: 'Back to graph', exact: true }).click()
    await h.page.waitForURL(graphUrl); await h.page.locator('#task-graph-canvas').waitFor()
    await h.page.waitForFunction(() => document.activeElement?.dataset.graphTask === 'parent')
    const child = h.page.locator('[data-graph-task="child-a"]')
    await child.focus(); await h.page.keyboard.press('Space')
    await h.page.waitForURL(/focus=task%3Achild-a/)
    await h.page.getByRole('button', { name: 'Back to graph', exact: true }).click()
    await h.page.waitForURL(graphUrl); await h.page.locator('#task-graph-canvas').waitFor()
    await h.page.waitForFunction(() => document.activeElement?.dataset.graphTask === 'child-a')
    assert.deepEqual(await transform(h), viewport)
    assert.equal(await h.page.evaluate(() => history.length), length)
    await h.page.getByRole('button', { name: 'Back', exact: true }).click()
    await h.page.locator('#hierarchy-viewport').waitFor(); await h.idle()
    assert.equal(h.page.url().includes('focus='), focused)
    assert.equal(await query.inputValue(), 'Graph')
    assert.ok(Math.abs(await h.page.locator('#entity-root-project').evaluate(n => n.getBoundingClientRect().top) - originTop) < 1)
    assert.deepEqual(h.errors, []); assert.deepEqual(h.unhandled, [])
  } finally { await h.close() }
})

test('canvas wheel zoom preserves world geometry and settles measurements', { timeout: 30000 }, async () => {
  const h = await openGraph()
  try {
    await h.start(); await h.page.locator('#task-graph-open-root-project').click()
    await h.page.locator('[data-graph-task="child-b"]').waitFor(); await h.idle()
    if (process.env.HMEM_TASK_GRAPH_SCREENSHOT === '1') {
      await mkdir('../../output/task-graph-renderer', { recursive: true })
      await h.page.screenshot({ path: '../../output/task-graph-renderer/graph-page.png' })
    }
    const before = await world(h), canvas = h.page.locator('#task-graph-canvas'), box = await canvas.boundingBox(), a = await transform(h)
    const point = { x: box.width * .6, y: box.height * .4 }
    await canvas.evaluate(n => n.addEventListener('wheel', event => { n.dataset.wheelPoint = JSON.stringify({ x: event.clientX - n.getBoundingClientRect().left, y: event.clientY - n.getBoundingClientRect().top }) }, { once: true }))
    await h.page.mouse.move(box.x + point.x, box.y + point.y); await h.page.mouse.wheel(0, -120)
    const b = await transform(h)
    const actualPoint = await canvas.evaluate(n => JSON.parse(n.dataset.wheelPoint))
    assert.ok(b.scale > a.scale)
    assert.ok(Math.abs((actualPoint.x - a.x) / a.scale - (actualPoint.x - b.x) / b.scale) < .01)
    assert.ok(Math.abs((actualPoint.y - a.y) / a.scale - (actualPoint.y - b.y) / b.scale) < .01)
    assert.deepEqual(await world(h), before)
    await h.page.getByRole('button', { name: 'Fit', exact: true }).click()
    assert.deepEqual(await world(h), before)
    const panBefore = await transform(h)
    await h.page.mouse.move(box.x + 30, box.y + 30); await h.page.mouse.down()
    await h.page.mouse.move(box.x + 100, box.y + 70, { steps: 4 }); await h.page.mouse.up()
    const panAfter = await transform(h)
    assert.ok(Math.abs(panAfter.x - panBefore.x - 70) < 1)
    assert.ok(Math.abs(panAfter.y - panBefore.y - 40) < 1)
    const card = h.page.locator('[data-graph-task="parent"]'), cardBox = await card.boundingBox(), url = h.page.url()
    await h.page.mouse.move(cardBox.x + 10, cardBox.y + 10); await h.page.mouse.down()
    await h.page.mouse.move(cardBox.x + 55, cardBox.y + 20, { steps: 3 }); await h.page.mouse.up()
    assert.equal(h.page.url(), url)
    await card.click(); await h.page.waitForURL(/focus=task%3Aparent/)
    assert.deepEqual(h.errors, []); assert.deepEqual(h.unhandled, [])
  } finally { await h.close() }
})

test('loading, empty, failed and unavailable project states remain explicit', { timeout: 30000 }, async () => {
  let release
  const wait = new Promise(resolve => { release = resolve })
  const empty = fixture(); empty.tasks = []; empty.dependencies = []
  const h = await openGraph(empty, { wait })
  try {
    await h.start(); await h.page.locator('#task-graph-open-root-project').click()
    await h.page.getByRole('status').filter({ hasText: 'Loading a complete task graph' }).waitFor()
    assert.equal(await h.page.locator('[data-graph-task]').count(), 0)
    release(); await h.page.getByText('This project has no tasks.', { exact: true }).waitFor()
  } finally { release(); await h.close() }
  const failure = await openGraph(fixture(), { failure: true })
  try {
    await failure.start(); await failure.page.locator('#task-graph-open-root-project').click()
    await failure.page.getByRole('alert').filter({ hasText: 'Task graph could not load' }).waitFor()
    assert.equal(await failure.page.locator('[data-graph-task]').count(), 0)
    const retryFinished = failure.page.waitForResponse(response => response.url().endsWith('/api/v1/change-stream/resync') && failure.snapshots() === 12)
    await failure.page.getByRole('button', { name: 'Retry', exact: true }).click()
    await retryFinished
    await failure.page.getByRole('alert').filter({ hasText: 'Task graph could not load' }).waitFor()
    assert.equal(failure.snapshots(), 12, 'each explicit attempt retains the canonical manager’s six-request retry budget')
  } finally { await failure.close() }
  const missing = await openGraph()
  try {
    await missing.page.goto(missing.origin + '/workspace/' + missing.fixture.workspace.id + '#tab=projects&task-graph=missing')
    await missing.page.getByRole('alert').filter({ hasText: 'selected graph project is unavailable' }).waitFor()
    assert.equal(await missing.page.locator('[data-graph-task]').count(), 0)
  } finally { await missing.close() }
})

function graphEvent(f, id, type, target, action = 'updated') {
  return { schema_version: 1, type: 'change', event: { schema_version: 1, event_id: id,
    scope: 'workspace', workspace_id: f.workspace.id, occurred_at: '2026-11-14T20:00:00Z',
    actor: { type: 'system', id: null }, transaction: { id: 'tx-' + id, cause: 'core', request_id: null },
    entity: { type, id: target, action }, invalidations: [{ kind: 'entity', target: type + ':' + target }] } }
}

for (const focused of [false, true]) test(`deleted original project is not revived by graph Back (focused=${focused})`, { timeout: 20000 }, async () => {
  const f = fixture(), h = await openGraph(f)
  try {
    await h.start('tab=projects' + (focused ? '&focus=project%3Aroot-project' : '')); await h.idle()
    await h.page.locator('#task-graph-open-root-project').click()
    await h.page.locator('[data-graph-live="true"]').waitFor()
    await h.page.route(/\/api\/v1\/projects\/root-project(?:\/(?:overview|next-tasks|next-task-page))?(?:\?|$)/, route =>
      f.projects.some(row => row.id === 'root-project') ? route.fallback() : route.fulfill({ status: 404, json: { error: 'Project not found' } }))
    f.projects = f.projects.filter(row => row.id !== 'root-project')
    const remaining = new Set(f.tasks.filter(row => row.project_id !== 'root-project').map(row => row.id))
    f.tasks = f.tasks.filter(row => remaining.has(row.id)); f.dependencies = f.dependencies.filter(edge => remaining.has(edge.task_id) && remaining.has(edge.depends_on_id))
    await h.page.evaluate(frame => window.pushHierarchyFrames([frame]), graphEvent(f, 'original-project-deleted', 'project', 'root-project', 'deleted'))
    await h.page.locator('[data-graph-task="parent"]').waitFor({ state: 'detached' })
    await h.page.getByRole('button', { name: 'Back', exact: true }).click()
    await h.page.locator('#hierarchy-viewport').waitFor(); await h.idle()
    await h.page.waitForFunction(() => {
      const node = document.querySelector('[data-task-graph-navigation]')
      return node && JSON.parse(node.dataset.taskGraphNavigation).return === null
    }, null, { timeout: 6500 })
    assert.equal(await h.page.locator('#entity-root-project').count(), 0, 'deleted original card must not return from a cached focus/origin')
    const rendered = await h.page.locator('.hierarchy-content-section').innerText()
    assert.match(rendered, /original project.*(?:unavailable|no longer|cannot|could not)|(?:unavailable|no longer|cannot|could not).*original project/i, rendered.slice(0, 700))
  } finally { await h.close() }
})

for (const change of ['rename', 'status', 'reparent']) for (const focused of [false, true]) test(`graph Back uses current ${change} membership (focused=${focused})`, { timeout: 20000 }, async () => {
  const f = fixture(), h = await openGraph(f)
  try {
    await h.start('tab=projects' + (focused ? '&focus=project%3Aroot-project' : '')); await h.idle()
    await h.page.getByRole('button', { name: 'Collapse All', exact: true }).click()
    await h.page.locator('.hierarchy-search-input').fill('Graph'); await h.idle()
    await h.page.locator('#task-graph-open-root-project').click()
    await h.page.locator('[data-graph-live="true"]').waitFor()
    const refreshed = h.page.waitForResponse(response => new URL(response.url()).pathname === '/api/v1/change-stream/resync' && response.request().postDataJSON().snapshot_profile === 'full_v1', { timeout: 5000 })
    if (change === 'rename') f.projects[0].name = 'Renamed current project'
    if (change === 'status') { f.projects[0].status = 'completed'; for (const task of f.tasks) if (task.project_id === 'root-project') task.status = 'done' }
    if (change === 'reparent') f.projects[0].parent_id = 'external'
    await h.page.evaluate(frame => window.pushHierarchyFrames([frame]), graphEvent(f, 'original-' + change, 'project', 'root-project'))
    await (await refreshed).finished(); await h.page.locator('[data-graph-live="true"]').waitFor()
    await h.page.getByRole('button', { name: 'Back', exact: true }).click()
    await h.page.locator('#hierarchy-viewport').waitFor(); await h.idle()
    await h.page.waitForFunction(() => JSON.parse(document.querySelector('[data-task-graph-navigation]').dataset.taskGraphNavigation).return === null, null, { timeout: 6500 })
    assert.equal(await h.page.locator('.hierarchy-search-input').inputValue(), 'Graph')
    assert.equal(h.page.url().includes('focus='), focused)
    const card = h.page.locator('#entity-root-project')
    if (focused) {
      assert.equal(await card.count(), 1)
      if (change === 'rename') assert.match(await card.innerText(), /Renamed current project/)
      if (change === 'status') assert.equal(await card.locator('select').first().inputValue(), 'completed')
      if (change === 'reparent') await h.page.locator('.titlebar-breadcrumbs').filter({ hasText: 'External project' }).waitFor()
      assert.equal(await h.page.locator('[data-task-graph-return-notice]').count(), 0)
    } else {
      assert.equal(await card.count(), 0, 'current filtered/collapsed membership must exclude the old original row')
      await h.page.getByRole('status').filter({ hasText: 'The original project could not be restored in this view' }).waitFor()
      await h.page.getByRole('button', { name: 'Dismiss', exact: true }).click()
      await h.page.locator('[data-task-graph-return-notice]').waitFor({ state: 'detached' })
      assert.equal(await h.page.locator('[data-task-graph-return-notice]').count(), 0)
    }
    assert.deepEqual(h.errors, []); assert.deepEqual(h.unhandled, [])
  } finally { await h.close() }
})

test('tab-away cancels an old return before reentry beyond its deadline', { timeout: 20000 }, async () => {
  const f = fixture(), h = await openGraph(f)
  try {
    await h.start(); await h.page.locator('.hierarchy-search-input').fill('Graph'); await h.idle()
    await h.page.locator('#task-graph-open-root-project').click(); await h.page.locator('[data-graph-live="true"]').waitFor()
    f.projects[0].name = 'Renamed outside saved search'
    await h.page.evaluate(frame => window.pushHierarchyFrames([frame]), graphEvent(f,'tab-return-rename','project','root-project'))
    await h.page.locator('[data-graph-project-label="root-project"]').filter({hasText:'Renamed outside saved search'}).waitFor()
    await h.page.getByRole('button',{name:'Back',exact:true}).click()
    await h.page.locator('#hierarchy-viewport').waitFor()
    await h.page.waitForFunction(() => JSON.parse(document.querySelector('[data-task-graph-navigation]').dataset.taskGraphNavigation).return !== null)
    await h.page.getByRole('button',{name:'Search',exact:true}).click()
    await h.page.locator('[data-task-graph-navigation]').waitFor({state:'detached'})
    await h.page.waitForTimeout(5100)
    await h.page.locator('.tab[aria-label="Projects"]').click()
    await h.page.locator('#hierarchy-viewport').waitFor(); await h.idle()
    const before = await h.page.locator('#main-content-scroll').evaluate(n => n.scrollTop)
    assert.equal(await h.page.locator('[data-task-graph-navigation]').evaluate(n => JSON.parse(n.dataset.taskGraphNavigation).return), null)
    assert.equal(await h.page.locator('[data-task-graph-return-notice]').count(), 0)
    await h.page.waitForTimeout(250)
    assert.equal(await h.page.locator('#main-content-scroll').evaluate(n => n.scrollTop), before)
    assert.equal(await h.page.evaluate(() => document.activeElement?.id === 'task-graph-open-root-project'), false)
    assert.equal(await h.page.locator('#entity-root-project').count(), 0)
    assert.deepEqual(h.errors, []); assert.deepEqual(h.unhandled, [])
  } finally { await h.close() }
})

test('live snapshots update cards, ownership and incident edges without replacing native focus or camera', { timeout: 45000 }, async () => {
  const f = fixture()
  f.tasks.push({ ...f.tasks[3], id: 'remote', title: 'Not directly connected' })
  const h = await openGraph(f)
  const push = frame => h.page.evaluate(value => window.pushHierarchyFrames([value]), frame)
  try {
    await h.start(); await h.page.locator('#task-graph-open-root-project').click()
    await h.page.locator('[data-graph-live="true"]').waitFor()
    assert.equal(await h.page.locator('[data-graph-task="remote"]').count(), 0)
    await h.page.getByRole('button', { name: 'Zoom in', exact: true }).click()
    const viewport = await transform(h)
    const card = h.page.locator('[data-graph-task="child-b"]')
    await card.focus()
    await card.evaluate(node => { window.liveTaskButton = node; window.liveGraphCanvas = document.getElementById('task-graph-canvas') })
    f.tasks.find(row => row.id === 'child-b').title = 'Live subtask title'
    f.tasks.find(row => row.id === 'child-b').status = 'in_progress'
    await push(graphEvent(f, 'title-status', 'task', 'child-b'))
    await h.page.waitForFunction(() => document.querySelector('[data-graph-task="child-b"]')?.textContent.includes('Live subtask title') && document.querySelector('[data-graph-task="child-b"]')?.textContent.includes('in progress'))
    await h.page.locator('[data-graph-live="true"]').waitFor()
    assert.equal(await h.page.evaluate(() => document.activeElement === window.liveTaskButton && document.getElementById('task-graph-canvas') === window.liveGraphCanvas), true)
    assert.deepEqual(await transform(h), viewport)
    f.dependencies = f.dependencies.filter(row => !(row.task_id === 'parent' && row.depends_on_id === 'external-task'))
    await push(graphEvent(f, 'remove-incident', 'task_dependency', 'parent:external-task', 'deleted'))
    await h.page.locator('[data-graph-task="external-task"]').waitFor({ state: 'detached' })
    assert.equal(await h.page.locator('[data-prerequisite="external-task"]').count(), 0)
    f.projects.find(row => row.id === 'external').parent_id = 'root-project'
    f.projects.find(row => row.id === 'external').name = 'Reparented child project'
    await push(graphEvent(f, 'project-reparent', 'project', 'external'))
    await h.page.locator('[data-graph-task="remote"]').waitFor()
    await h.page.locator('[data-graph-project-label="external"]').filter({ hasText: 'Reparented child project' }).waitFor()
    assert.equal(await h.page.locator('.graph-external-project').count(), 0)
    f.projects.find(row => row.id === 'external').parent_id = null
    await push(graphEvent(f, 'project-detach', 'project', 'external'))
    await h.page.locator('[data-graph-task="remote"]').waitFor({ state: 'detached' })
    const moved = f.tasks.find(row => row.id === 'child-b'); moved.parent_id = null; moved.project_id = 'external'
    f.dependencies = f.dependencies.filter(row => row.task_id !== 'child-b')
    f.dependencies.push({ task_id: 'child-b', depends_on_id: 'parent' })
    await push(graphEvent(f, 'task-reparent', 'task', 'child-b'))
    await h.page.locator('[data-prerequisite="parent"][data-dependent="child-b"]').waitFor()
    assert.equal(await card.evaluate(node => node.classList.contains('graph-subtask')), false)
    assert.equal(await h.page.evaluate(() => document.activeElement === window.liveTaskButton && document.querySelector('[data-graph-task="child-b"]') === window.liveTaskButton), true)
    assert.deepEqual(await transform(h), viewport)
    await card.focus(); await h.page.keyboard.press('Enter')
    await h.page.waitForURL(/focus=task%3Achild-b/)
    await h.page.locator('#entity-child-b').waitFor()
    // A deleted detail endpoint returns 404 while the canonical invalidation
    // and complete graph refresh race the ordinary focused-task reader.
    await h.page.route(/\/api\/v1\/tasks\/child-b(?:\/(?:overview|dependencies))?(?:\?|$)/, route =>
      f.tasks.some(row => row.id === 'child-b') ? route.fallback() : route.fulfill({ status: 404, json: { error: 'Task not found' } }))
    f.tasks = f.tasks.filter(row => row.id !== 'child-b'); f.dependencies = f.dependencies.filter(row => row.task_id !== 'child-b' && row.depends_on_id !== 'child-b')
    await push(graphEvent(f, 'task-delete', 'task', 'child-b', 'deleted'))
    await h.page.waitForURL(/task-graph=root-project/)
    await h.page.locator('#task-graph-canvas').waitFor()
    assert.equal(await h.page.locator('[data-graph-task="child-b"]').count(), 0)
    assert.deepEqual(await transform(h), viewport)
    assert.deepEqual(h.errors, []); assert.deepEqual(h.unhandled, [])
  } finally { await h.close() }
})

test('semantic snapshot rejection stays stale through a checkpoint until valid retry acceptance', { timeout: 30000 }, async () => {
  const f = fixture(), h = await openGraph(f)
  try {
    await h.start(); await h.page.locator('#task-graph-open-root-project').click()
    await h.page.locator('[data-graph-live="true"]').waitFor()
    f.dependencies.push({ task_id: 'parent', depends_on_id: 'missing-task' })
    await h.page.evaluate(frame => window.pushHierarchyFrames([frame]), graphEvent(f, 'bad-reference', 'task_dependency', 'missing-link'))
    const retry = h.page.getByRole('button', { name: 'Retry', exact: true })
    await retry.waitFor()
    assert.equal(await h.page.locator('[data-graph-live="true"]').count(), 0)
    await h.page.evaluate(() => window.pushHierarchyFrames([]))
    await retry.waitFor()
    assert.equal(await h.page.locator('[data-graph-live="true"]').count(), 0)
    assert.equal(await h.page.locator('[data-graph-task="parent"]').count(), 1)
    f.dependencies.pop(); f.tasks[0].title = 'Accepted after semantic rejection'
    await retry.click()
    await h.page.locator('[data-graph-task="parent"]').filter({ hasText: 'Accepted after semantic rejection' }).waitFor()
    await h.page.locator('[data-graph-live="true"]').waitFor()
    assert.deepEqual(h.errors, []); assert.deepEqual(h.unhandled, [])
  } finally { await h.close() }
})

test('Retry during reseed wait publishes one manager-owned seed through real Elm acceptance', { timeout: 30000 }, async () => {
  const f = fixture(); let release, arrived
  const held = new Promise(resolve => { release = resolve }), seedStarted = new Promise(resolve => { arrived = resolve })
  const h = await openGraph(f, { wait: count => { if (count === 2) { arrived(); return held } } })
  try {
    await h.start(); await h.page.locator('#task-graph-open-root-project').click()
    await h.page.locator('[data-graph-live="true"]').waitFor()
    await h.page.evaluate(() => window.pushHierarchyRawFrames([{ schema_version: 1, type: 'resync_required' }]))
    await h.page.getByRole('button', { name: 'Retry', exact: true }).click()
    await seedStarted
    f.tasks[0].title = 'Authoritative replacement seed'
    release()
    await h.page.locator('[data-graph-task="parent"]').filter({ hasText: 'Authoritative replacement seed' }).waitFor()
    await h.page.locator('[data-graph-live="true"]').waitFor()
    await h.page.waitForTimeout(750)
    assert.equal(h.snapshots(), 2, 'no competing finite read or delayed second manager seed')
    f.tasks[0].title = 'Change after authoritative replacement'
    await h.page.evaluate(frame => window.pushHierarchyFrames([frame]), graphEvent(f, 'after-reseed-retry', 'task', 'parent'))
    await h.page.locator('[data-graph-task="parent"]').filter({ hasText: 'Change after authoritative replacement' }).waitFor()
    await h.page.locator('[data-graph-live="true"]').waitFor()
    assert.deepEqual(h.errors, []); assert.deepEqual(h.unhandled, [])
  } finally { release(); await h.close() }
})

test('queued Retry retires an in-flight manager seed before real Elm accepts its replacement', { timeout: 30000 }, async () => {
  const f = fixture(); let releaseOld, releaseNew, oldStarted, newStarted
  const oldGate = new Promise(resolve => { releaseOld = resolve }), newGate = new Promise(resolve => { releaseNew = resolve })
  const oldArrived = new Promise(resolve => { oldStarted = resolve }), newArrived = new Promise(resolve => { newStarted = resolve })
  const h = await openGraph(f, { wait: count => {
    if (count === 2) { oldStarted(); return oldGate }
    if (count === 3) { newStarted(); return newGate }
  } })
  try {
    await h.start(); await h.page.locator('#task-graph-open-root-project').click()
    await h.page.locator('[data-graph-live="true"]').waitFor()
    await h.page.evaluate(() => window.pushHierarchyRawFrames([{ schema_version: 1, type: 'resync_required' }]))
    await h.page.getByRole('button', { name: 'Retry', exact: true }).evaluate(button => { window.queuedGraphRetry = button })
    await oldArrived
    // Exercise a user activation queued from the recovery control while its
    // original seed is already in flight; the real Elm handler remains bound.
    await h.page.evaluate(() => window.queuedGraphRetry.click())
    await newArrived
    f.tasks[0].title = 'Replacement owns the accepted boundary'
    releaseNew()
    await h.page.locator('[data-graph-task="parent"]').filter({ hasText: 'Replacement owns the accepted boundary' }).waitFor()
    await h.page.locator('[data-graph-live="true"]').waitFor()
    f.tasks[0].title = 'Abandoned seed must not publish'
    releaseOld()
    await h.page.waitForTimeout(750)
    assert.equal(h.snapshots(), 3, 'initial seed, abandoned seed and sole replacement seed')
    assert.equal(await h.page.locator('[data-graph-task="parent"]').textContent().then(text => text.includes('Replacement owns the accepted boundary')), true)
    f.tasks[0].title = 'Subsequent canonical event remains visible'
    await h.page.evaluate(frame => window.pushHierarchyFrames([frame]), graphEvent(f, 'after-inflight-retry', 'task', 'parent'))
    await h.page.locator('[data-graph-task="parent"]').filter({ hasText: 'Subsequent canonical event remains visible' }).waitFor()
    await h.page.locator('[data-graph-live="true"]').waitFor()
    assert.deepEqual(h.errors, []); assert.deepEqual(h.unhandled, [])
  } finally { releaseOld(); releaseNew(); await h.close() }
})

test('live refresh failure keeps stale cards and viewport until explicit Retry', { timeout: 30000 }, async () => {
  const f = fixture(); let failing = false
  const h = await openGraph(f, { failure: () => failing })
  try {
    await h.start(); await h.page.locator('#task-graph-open-root-project').click()
    await h.page.locator('[data-graph-live="true"]').waitFor()
    await h.page.getByRole('button', { name: 'Zoom in', exact: true }).click()
    const viewport = await transform(h)
    await h.page.locator('[data-graph-task="parent"]').evaluate(node => { window.staleGraphCard = node })
    failing = true
    await h.page.evaluate(frame => window.pushHierarchyFrames([frame]), graphEvent(f, 'failed-refresh', 'task', 'parent'))
    await h.page.locator('[data-graph-live="false"]').filter({ hasText: 'Task graph could not load' }).waitFor()
    assert.equal(await h.page.evaluate(() => document.querySelector('[data-graph-task="parent"]') === window.staleGraphCard), true)
    assert.deepEqual(await transform(h), viewport)
    const reads = h.snapshots()
    await h.page.evaluate(frame => window.pushHierarchyFrames([frame]), graphEvent(f, 'while-failed', 'task', 'parent'))
    await h.page.waitForTimeout(750)
    assert.equal(h.snapshots(), reads)
    failing = false
    await h.page.getByRole('button', { name: 'Retry', exact: true }).click()
    await h.page.locator('[data-graph-live="true"]').waitFor()
    assert.equal(h.snapshots(), reads + 1)
    assert.deepEqual(await transform(h), viewport)
    assert.deepEqual(h.errors, []); assert.deepEqual(h.unhandled, [])
  } finally { await h.close() }
})

test('direct nested-project entry uses readable ancestor breadcrumbs without hierarchy cache authority', { timeout: 30000 }, async () => {
  const f = fixture()
  f.projects.push({ ...f.projects[0], id: 'nested', parent_id: 'root-project', name: 'Nested project' })
  f.tasks = [{ ...f.tasks[0], id: 'nested-task', project_id: 'nested', title: 'Nested task' }]; f.dependencies = []
  const h = await openGraph(f)
  try {
    await h.page.goto(h.origin + '/workspace/' + f.workspace.id + '#tab=projects&task-graph=nested')
    await h.page.locator('[data-graph-task="nested-task"]').waitFor()
    await h.page.locator('.titlebar-breadcrumbs a.focus-crumb').filter({ hasText: 'Graph project' }).waitFor()
    await h.page.locator('.titlebar-breadcrumbs a.focus-crumb').filter({ hasText: 'Nested project' }).waitFor()
    assert.deepEqual(h.errors, []); assert.deepEqual(h.unhandled, [])
  } finally { await h.close() }
})

test('long labels fit intrinsic boxes and zoom cannot trigger layout feedback', { timeout: 30000 }, async () => {
  const f = fixture(); f.tasks[2].title = 'A long unbroken title ' + 'abcdefghijklmnopqrstuv'.repeat(70)
  const h = await openGraph(f)
  try {
    await h.start(); await h.page.locator('#task-graph-open-root-project').click()
    await h.page.locator('[data-graph-task="child-b"]').waitFor(); await transform(h)
    const paints = () => h.page.evaluate(() => new Promise(resolve => { let count = 0; const next = () => ++count >= 12 ? resolve() : requestAnimationFrame(next); requestAnimationFrame(next) }))
    await paints()
    const boxes = await h.page.locator('[data-graph-task]').evaluateAll(buttons => buttons.map(button => ({ height: button.offsetHeight, scroll: button.scrollHeight, border: button.offsetHeight - button.clientHeight, worldHeight: Number(button.parentNode.getAttribute('height')) })))
    assert.ok(boxes.every(box => box.scroll + box.border <= box.worldHeight && box.height <= box.worldHeight))
    const settled = await world(h)
    for (let i = 0; i < 5; i++) await h.page.getByRole('button', { name: 'Zoom in', exact: true }).click()
    await paints(); assert.deepEqual(await world(h), settled)
    await h.page.getByRole('button', { name: 'Zoom out', exact: true }).click(); await paints()
    assert.deepEqual(await world(h), settled)
    assert.deepEqual(h.errors, []); assert.deepEqual(h.unhandled, [])
  } finally { await h.close() }
})

test('keyed native task identity and focus survive earlier insertion and deletion', { timeout: 45000 }, async () => {
  const scratch = resolve('../../output/task-graph-renderer'), compiled = resolve(scratch, 'keyed-harness.js')
  await mkdir(scratch, { recursive: true })
  let h
  try {
    await promisify(execFile)(process.execPath, [resolve('node_modules/elm/bin/elm'), 'make', 'tests-js/support/TaskGraphKeyedHarness.elm', '--output=' + compiled], { timeout: 30000, maxBuffer: 1024 * 1024 })
    h = await openGraph()
    await h.start()
    const css = await h.page.locator('link[rel="stylesheet"]').getAttribute('href')
    await h.page.route('**/__graph-keyed.js', route => route.fulfill({ contentType: 'application/javascript', path: compiled }))
    await h.page.route('**/__graph-keyed', route => route.fulfill({ contentType: 'text/html', body: `<link rel="stylesheet" href="${css}"><div id="app"></div><script src="/__graph-keyed.js"></script><script>window.keyedApp=Elm.TaskGraphKeyedHarness.init({node:document.getElementById('app')})</script>` }))
    await h.page.goto(h.origin + '/__graph-keyed')
    await h.page.locator('[data-graph-task="z-target"]').focus()
    await h.page.evaluate(() => { window.keyedTarget = document.activeElement; window.keyedApp.ports.changeGraph.send(true) })
    await h.page.locator('[data-graph-task="a-new"]').waitFor()
    assert.equal(await h.page.evaluate(() => document.activeElement === window.keyedTarget && window.keyedTarget.dataset.graphTask === 'z-target'), true)
    await h.page.evaluate(() => window.keyedApp.ports.changeGraph.send(false))
    await h.page.locator('[data-graph-task="a-new"]').waitFor({ state: 'detached' })
    assert.equal(await h.page.evaluate(() => document.activeElement === window.keyedTarget && window.keyedTarget.dataset.graphTask === 'z-target'), true)
  } finally { try { await h?.close() } finally { await rm(compiled, { force: true }) } }
})

test('narrow wrapped chrome leaves the canvas and controls in the visible page remainder', { timeout: 30000 }, async () => {
  const h = await openGraph()
  try {
    await h.page.setViewportSize({ width: 640, height: 820 })
    await h.start(); await h.page.locator('#task-graph-open-root-project').click()
    await h.page.locator('[data-graph-task="parent"]').waitFor(); await transform(h)
    await h.page.waitForFunction(() => { const c = document.getElementById('task-graph-canvas'), host = document.getElementById('main-content-scroll'); return c.getBoundingClientRect().bottom <= Math.min(innerHeight, host.getBoundingClientRect().bottom) - 8 })
    assert.equal((await transform(h)).scale, 1)
    const geometry = await h.page.locator('#task-graph-canvas').evaluate(n => ({ left: n.getBoundingClientRect().left, right: n.getBoundingClientRect().right, height: n.clientHeight }))
    assert.ok(geometry.left >= 0 && geometry.right <= 640 && geometry.height > 100)
    for (const label of ['Zoom in', 'Zoom out', 'Fit']) {
      const box = await h.page.getByRole('button', { name: label, exact: true }).boundingBox()
      assert.ok(box.y >= 0 && box.y + box.height <= 820)
    }
    await h.page.setViewportSize({ width: 900, height: 900 })
    await h.page.waitForFunction(() => document.getElementById('task-graph-canvas').getBoundingClientRect().bottom <= innerHeight - 8)
    assert.equal((await transform(h)).scale, 1)
  } finally { await h.close() }
})

test('native touch pinch keeps the midpoint world anchor', { timeout: 30000 }, async () => {
  const h = await openGraph()
  let session
  try {
    await h.start(); await h.page.locator('#task-graph-open-root-project').click()
    await h.page.locator('[data-graph-task="parent"]').waitFor(); await h.idle(); const before = await transform(h)
    const box = await h.page.locator('#task-graph-canvas').boundingBox()
    await h.page.locator('#task-graph-canvas').evaluate(n => { n.pointerReceipt = []; for (const kind of ['pointerdown', 'pointermove']) n.addEventListener(kind, e => n.pointerReceipt.push({ kind, id: e.pointerId, x: e.clientX - n.getBoundingClientRect().left, y: e.clientY - n.getBoundingClientRect().top })) })
    session = await h.page.context().newCDPSession(h.page)
    await session.send('Emulation.setTouchEmulationEnabled', { enabled: true, maxTouchPoints: 2 })
    const left = Math.round(box.x + 150), top = Math.round(box.y + 100)
    await session.send('Input.dispatchTouchEvent', { type: 'touchStart', touchPoints: [{ x: left, y: top, id: 1 }, { x: left + 100, y: top, id: 2 }] })
    await session.send('Input.dispatchTouchEvent', { type: 'touchMove', touchPoints: [{ x: left - 25, y: top + 20, id: 1 }, { x: left + 175, y: top + 20, id: 2 }] })
    await session.send('Input.dispatchTouchEvent', { type: 'touchEnd', touchPoints: [] })
    const after = await transform(h)
    assert.ok(Math.abs(after.scale - before.scale * 2) < .001)
    const receipt = await h.page.locator('#task-graph-canvas').evaluate(n => n.pointerReceipt)
    const down = receipt.filter(point => point.kind === 'pointerdown'), moved = receipt.filter(point => point.kind === 'pointermove')
    assert.equal(down.length, 2); assert.equal(moved.length, 2)
    const midpoint = points => ({ x: (points[0].x + points[1].x) / 2, y: (points[0].y + points[1].y) / 2 })
    const oldPoint = midpoint(down), newPoint = midpoint(moved)
    for (const axis of ['x', 'y']) assert.ok(Math.abs((oldPoint[axis] - before[axis]) / before.scale - (newPoint[axis] - after[axis]) / after.scale) < .01)
  } finally { await session?.detach(); await h.close() }
})

test('a1024-byte project name is measured, enclosed and included in world bounds', { timeout: 30000 }, async () => {
  const f = fixture(); f.projects[0].name = 'W'.repeat(1024)
  const h = await openGraph(f)
  try {
    await h.start(); await h.page.locator('#task-graph-open-root-project').click()
    await h.page.locator('[data-graph-project-label="root-project"]').waitFor()
    await h.page.waitForFunction(() => JSON.parse(document.getElementById('task-graph-canvas').dataset.taskGraphCanvas).measured)
    await h.page.waitForFunction(() => {
      const canvas = document.getElementById('task-graph-canvas'), metadata = JSON.parse(canvas.dataset.taskGraphCanvas), view = JSON.parse(canvas.dataset.graphTransform), anchor = metadata.initialAnchor || metadata.selected
      return Math.abs(anchor.x * view.scale + view.x - canvas.clientWidth / 2) < 1 && Math.abs(anchor.y * view.scale + view.y - canvas.clientHeight / 2) < 1
    })
    const label = await h.page.locator('[data-graph-project-label="root-project"]').evaluate(n => {
      const box = n.parentNode, group = box.parentNode, circle = group.querySelector('circle')
      return { height: n.scrollHeight, boxHeight: Number(box.getAttribute('height')), x: Number(box.getAttribute('x')), y: Number(box.getAttribute('y')), width: Number(box.getAttribute('width')), cx: Number(circle.getAttribute('cx')), cy: Number(circle.getAttribute('cy')), r: Number(circle.getAttribute('r')) }
    })
    assert.ok(label.height > 60 && label.height <= label.boxHeight)
    for (const [x, y] of [[label.x, label.y], [label.x + label.width, label.y + label.boxHeight]]) assert.ok((x - label.cx) ** 2 + (y - label.cy) ** 2 <= label.r ** 2)
    const bounds = await h.page.locator('#task-graph-canvas').evaluate(n => JSON.parse(n.dataset.taskGraphCanvas).bounds)
    assert.ok(label.x >= bounds.x && label.y >= bounds.y && label.x + label.width <= bounds.x + bounds.width && label.y + label.boxHeight <= bounds.y + bounds.height)
    assert.deepEqual(h.errors, []); assert.deepEqual(h.unhandled, [])
  } finally { await h.close() }
})

test('real credential reset cancels a held old graph request before replacement session admission', { timeout: 30000 }, async () => {
  let release
  const wait = new Promise(resolve => { release = resolve })
  const h = await openGraph(fixture(), { wait, waitFirstOnly: true })
  let reauth
  try {
    await h.page.route('**/hmem-runtime-config.js', route => route.fulfill({ contentType: 'application/javascript', body: "window.HMEM_CONFIG={authMode:'local',apiUrl:window.location.origin,wsUrl:'ws://fixture.invalid/api/v1/ws',authTokenStorage:'local'};" }))
    await h.page.addInitScript(() => {
      const original = window.fetch
      window.graphRequestAborts = []
      window.fetch = function (url, options) {
        if (String(url).endsWith('/api/v1/change-stream/resync') && JSON.parse(options.body).snapshot_profile === 'full_v1') {
          const record = { aborted: false }; window.graphRequestAborts.push(record)
          options.signal?.addEventListener('abort', () => { record.aborted = true })
        }
        return original.apply(this, arguments)
      }
    })
    await h.start(); await h.page.locator('#task-graph-open-root-project').click()
    await h.page.waitForFunction(() => window.graphRequestAborts.length === 1)
    reauth = h.gate(request => request.path === '/api/v1/session', { hold: true })
    await h.page.evaluate(() => {
      localStorage.setItem('hmem-auth-token', 'task-graph-test-credential')
      dispatchEvent(new StorageEvent('storage', { key: 'hmem-auth-token', newValue: 'task-graph-test-credential' }))
    })
    await h.bounded(reauth.arrived, 5000, 'Replacement session request admission')
    await h.page.waitForFunction(() => window.graphRequestAborts[0].aborted)
    assert.equal(h.snapshots(), 1)
    assert.equal(await h.page.locator('[data-graph-task]').count(), 0)
    release(); reauth.release()
    await h.page.locator('[data-graph-task="parent"]').waitFor()
    assert.equal(h.snapshots(), 2)
    assert.deepEqual(h.errors, []); assert.deepEqual(h.unhandled, [])
  } finally { release(); reauth?.release(); await h.close() }
})

test('hierarchy Back then Forward keeps native focus receipts fresh for the same graph entry', { timeout: 30000 }, async () => {
  const h = await openGraph()
  try {
    await h.start(); await h.page.locator('#task-graph-open-root-project').click()
    await h.page.locator('[data-graph-task="child-a"]').waitFor(); await h.idle()
    const graphUrl = h.page.url()
    await h.page.locator('[data-graph-task="child-a"]').click()
    await h.page.waitForURL(/focus=task%3Achild-a/)
    await h.page.getByRole('button', { name: 'Back to graph', exact: true }).click()
    await h.page.waitForURL(graphUrl)
    await h.page.waitForFunction(() => document.activeElement?.dataset.graphTask === 'child-a', null, { timeout: 5000 })
    await h.page.getByRole('button', { name: 'Back', exact: true }).click()
    await h.page.locator('#hierarchy-viewport').waitFor(); await h.idle()
    await h.page.goForward(); await h.page.waitForURL(graphUrl)
    await h.page.locator('[data-graph-task="parent"]').waitFor(); await h.idle()
    const viewport = await transform(h)
    await h.page.locator('[data-graph-task="parent"]').click()
    await h.page.waitForURL(/focus=task%3Aparent/)
    await h.page.getByRole('button', { name: 'Back to graph', exact: true }).click()
    await h.page.waitForURL(graphUrl)
    await h.page.waitForFunction(() => document.activeElement?.dataset.graphTask === 'parent', null, { timeout: 5000 })
    assert.deepEqual(await transform(h), viewport)
    assert.deepEqual(h.errors, []); assert.deepEqual(h.unhandled, [])
  } finally { await h.close() }
})
