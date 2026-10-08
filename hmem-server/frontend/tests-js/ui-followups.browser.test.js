import assert from 'node:assert/strict'
import test from 'node:test'
import { hierarchyFixture, openHierarchy } from './hierarchy-fixture.mjs'

function fixture() {
  const data = hierarchyFixture()
  data.projects = [data.projects[0]]
  data.tasks = data.tasks.filter(task => task.parent_id || Number(task.id.slice(5)) < 24)
  for (const task of data.tasks) if (!task.parent_id && task.id !== 'task-000') task.status = Number(task.id.slice(5)) % 2 ? 'done' : 'todo'
  return data
}

const paint = page => page.evaluate(() => new Promise(resolve => requestAnimationFrame(() => requestAnimationFrame(resolve))))
const scrollTop = page => page.locator('#main-content-scroll').evaluate(element => element.scrollTop)

test('project filters preserve physical scroll before and after delayed replacement and measurements', { timeout: 60000 }, async () => {
  const h = await openHierarchy(fixture())
  try {
    await h.start(); await h.idle()
    await h.page.locator('#main-content-scroll').evaluate(element => { element.scrollTop = 150 })
    await h.page.waitForTimeout(300)
    await h.page.locator('#main-content-scroll').evaluate(element => { element.scrollTop = 150 })
    await paint(h.page); await h.idle()
    const before = await scrollTop(h.page)
    assert.ok(before > 100)
    const held = h.gate(receipt => receipt.kind === 'workspace_root', { hold: true })
    const filter = h.page.locator('.filter-bar').getByRole('button', { name: 'Todo', exact: true })
    const box = await filter.boundingBox()
    await h.page.mouse.click(box.x + box.width / 2, box.y + box.height / 2)
    await h.bounded(held.arrived, 10000, 'Filtered root arrival'); await paint(h.page)
    assert.equal(await scrollTop(h.page), before, 'click and pending result preserve the physical offset')
    assert.ok(await h.page.locator('[data-hierarchy-key]').count() <= 25)
    await h.page.mouse.move(900, 700); await h.page.mouse.wheel(0, 30); await h.page.waitForTimeout(100); await paint(h.page)
    const current = await scrollTop(h.page)
    assert.ok(current > before, 'new wheel movement remains available during replacement')
    held.release(); await h.idle(); await paint(h.page)
    assert.equal(await scrollTop(h.page), current, 'replacement and measured rows preserve the latest physical offset')
    await h.page.waitForTimeout(150); await paint(h.page)
    assert.equal(await scrollTop(h.page), current, 'later measurement echo does not restore an entity anchor')
    await h.page.locator('.filter-bar').getByRole('button', { name: 'Todo', exact: true }).click(); await h.idle(); await paint(h.page)
    assert.equal(await scrollTop(h.page), current, 'clearing a filter also preserves the physical offset')
    await h.page.locator('.filter-bar').getByRole('button', { name: 'Archived', exact: true }).click(); await h.idle(); await paint(h.page)
    const settled = await h.page.evaluate(() => { const scroll = document.getElementById('main-content-scroll'); return { top: scroll.scrollTop, bottom: Math.max(0, scroll.scrollHeight - scroll.clientHeight), minimum: parseFloat(getComputedStyle(document.getElementById('hierarchy-viewport')).minHeight) } })
    assert.equal(settled.minimum, 0, 'settled filtering releases its temporary extent')
    assert.equal(settled.top, settled.bottom, 'final shorter content clamps naturally at the bottom')
  } finally { await h.close() }
})

test('virtual subtasks share the parent enclosure with independent rows and bounded omitted runs', { timeout: 60000 }, async () => {
  const h = await openHierarchy(fixture())
  try {
    await h.start(); await h.idle(); await h.scrollTo('task:task-000'); await paint(h.page)
    const geometry = await h.page.evaluate(() => {
      const parent = document.getElementById('entity-task-000').closest('[data-task-family]')
      const children = [...document.querySelectorAll('.card-subtask')]
      const box = parent.getBoundingClientRect()
      return { parent: parent.dataset.taskFamily, children: children.map(child => {
        const row = child.closest('[data-hierarchy-key]'), bounds = child.getBoundingClientRect()
        return { family: child.closest('[data-task-family]')?.dataset.taskFamily, nestedRow: !!row.parentElement.closest('[data-hierarchy-key]'), inside: bounds.left >= box.left && bounds.right <= box.right && bounds.top >= box.top && bounds.bottom <= box.bottom }
      }), rows: document.querySelectorAll('[data-hierarchy-key]').length }
    })
    assert.equal(geometry.parent, 'task-000'); assert.ok(geometry.children.length > 1, JSON.stringify(geometry))
    assert.ok(geometry.children.every(child => child.family === 'task-000' && child.inside && !child.nestedRow), JSON.stringify(geometry))
    assert.ok(geometry.rows <= 25)
    await h.page.locator('#entity-task-000').evaluate(element => { const scroll = document.getElementById('main-content-scroll'); scroll.scrollTop += element.getBoundingClientRect().top - scroll.getBoundingClientRect().top - 250 })
    await paint(h.page)
    if (process.env.HMEM_UI_FOLLOWUP_SHOTS) await h.page.screenshot({ path: process.env.HMEM_UI_FOLLOWUP_SHOTS + '/nested-tasks.png' })
    await h.scrollTo('task:subtask-112')
    assert.equal(await h.page.locator('#entity-subtask-112').evaluate(element => element.closest('[data-task-family]').dataset.taskFamily), 'task-000')
    assert.ok(await h.page.locator('.hierarchy-spacer').count() > 0)
    const ordering = await h.page.locator('[data-hierarchy-index]').evaluateAll(rows => rows.map(row => Number(row.dataset.hierarchyIndex)))
    assert.deepEqual(ordering, [...ordering].sort((a, b) => a - b))
    await h.scrollTo('task:task-000'); await h.page.locator('#entity-task-000 .tree-toggle').click(); await h.idle()
    assert.equal(await h.page.locator('.card-subtask').count(), 0)
    await h.page.locator('#entity-task-000 .tree-toggle').click(); await h.idle()
    await h.scrollTo('task:subtask-112')
    assert.equal(await h.page.locator('#entity-subtask-112').evaluate(element => element.closest('[data-task-family]').dataset.taskFamily), 'task-000')
    assert.ok(await h.page.locator('[data-hierarchy-key]').count() <= 25)
  } finally { await h.close() }
})

test('a deep filter to a short prefix clamps safely and drains multiple discovery waves without scrolling', { timeout: 60000 }, async () => {
  const data = hierarchyFixture()
  for (let index = 0; index < 12; index++) data.tasks.push({ ...data.tasks[0], id: 'completed-' + index, project_id: 'project-' + String(index).padStart(3, '0'), parent_id: null, status: 'done' })
  const h = await openHierarchy(data)
  try {
    await h.start(); await h.idle(); await h.scrollTo('task:subtask-100')
    const before = await scrollTop(h.page), requestStart = h.requests.length
    assert.ok(before > 10000)
    // Exercise the same Elm filter event at a deep retained offset without
    // locator actionability scrolling to the toolbar first.
    await h.page.locator('.filter-bar').getByRole('button', { name: 'Done', exact: true }).evaluate(button => button.click())
    await h.idle(); await paint(h.page)
    const geometry = await h.page.evaluate(() => { const scroll = document.getElementById('main-content-scroll'); return { top: scroll.scrollTop, bottom: scroll.scrollHeight - scroll.clientHeight, minimum: parseFloat(getComputedStyle(document.getElementById('hierarchy-viewport')).minHeight) } })
    assert.ok(geometry.top < before && geometry.top <= geometry.bottom, JSON.stringify(geometry))
    assert.equal(geometry.minimum, 0)
    const branches = h.requests.slice(requestStart).filter(receipt => receipt.kind === 'project' && receipt.parent?.startsWith('project-'))
    assert.equal(new Set(branches.map(receipt => receipt.parent)).size, 12, 'all twelve filtered child branches drain beyond a single four-slot wave')
    assert.ok(h.caps().maxBranches <= 4); assert.ok(await h.page.locator('[data-hierarchy-key]').count() <= 25)
  } finally { await h.close() }
})

test('native subtask drag retains one keyed family through drop boundaries and distant pins', { timeout: 60000 }, async () => {
  const h = await openHierarchy(fixture())
  try {
    await h.start(); await h.idle(); await h.scrollTo('task:subtask-001')
    const card = h.page.locator('#entity-subtask-001')
    await card.evaluate(element => { const scroll = document.getElementById('main-content-scroll'); scroll.scrollTop += element.getBoundingClientRect().top - scroll.getBoundingClientRect().top - scroll.clientHeight / 2 })
    await paint(h.page)
    await card.evaluate(element => { window.dragCard = element; window.dragRow = element.closest('[data-hierarchy-key]'); window.dragStarted = false; document.addEventListener('dragstart', () => { window.dragStarted = true }) })
    const box = await card.locator('.entity-type-label').boundingBox()
    await h.page.mouse.move(box.x + box.width / 2, box.y + box.height / 2); await h.page.mouse.down()
    await h.page.mouse.move(box.x + box.width / 2 + 40, box.y + box.height / 2 + 20, { steps: 5 })
    await h.page.waitForFunction(() => window.dragStarted, undefined, { timeout: 5000 }); await paint(h.page)
    const families = await h.page.locator('.hierarchy-segment').evaluateAll(segments => segments.map(segment => [...segment.children].filter(element => element.dataset.taskFamily).map(element => element.dataset.taskFamily)))
    assert.ok(families.every(ids => ids.length === new Set(ids).size), JSON.stringify(families))
    assert.ok(await h.page.locator('[data-task-family="task-000"] [data-hierarchy-key^="drop:task-subtasks:"]').count() > 0)
    await h.page.locator('#main-content-scroll').evaluate(element => { element.scrollTop = element.scrollHeight }); await paint(h.page)
    assert.deepEqual(await h.page.evaluate(() => ({ card: document.getElementById('entity-subtask-001') === window.dragCard, row: document.querySelector('[data-hierarchy-key="task:subtask-001"]') === window.dragRow, connected: window.dragCard.isConnected, family: window.dragCard.closest('[data-task-family]')?.dataset.taskFamily })), { card: true, row: true, connected: true, family: 'task-000' })
    const keys = await h.page.locator('[data-hierarchy-key]').evaluateAll(rows => rows.map(row => row.dataset.hierarchyKey))
    assert.equal(keys.length, new Set(keys).size)
    assert.ok(keys.filter(key => !['project:root-project', 'task:task-000', 'task:subtask-001'].includes(key)).length <= 25)
    await h.page.keyboard.press('Escape'); await h.page.mouse.up()
  } finally { await h.page.mouse.up().catch(() => {}); await h.close() }
})
