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
    if (options.wait && (!options.waitFirstOnly || count === 1)) await options.wait
    if (options.failure) return route.fulfill({ status: 500, json: { error: 'controlled' } })
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
    await failure.page.getByRole('button', { name: 'Retry', exact: true }).click()
    await failure.page.getByRole('alert').filter({ hasText: 'Task graph could not load' }).waitFor()
    assert.equal(failure.snapshots(), 2)
  } finally { await failure.close() }
  const missing = await openGraph()
  try {
    await missing.page.goto(missing.origin + '/workspace/' + missing.fixture.workspace.id + '#tab=projects&task-graph=missing')
    await missing.page.getByRole('alert').filter({ hasText: 'selected graph project is unavailable' }).waitFor()
    assert.equal(await missing.page.locator('[data-graph-task]').count(), 0)
  } finally { await missing.close() }
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
      const canvas = document.getElementById('task-graph-canvas'), metadata = JSON.parse(canvas.dataset.taskGraphCanvas), view = JSON.parse(canvas.dataset.graphTransform)
      return Math.abs(metadata.selected.x * view.scale + view.x - canvas.clientWidth / 2) < 1 && Math.abs(metadata.selected.y * view.scale + view.y - canvas.clientHeight / 2) < 1
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
