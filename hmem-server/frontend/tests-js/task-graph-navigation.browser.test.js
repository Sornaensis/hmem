import assert from 'node:assert/strict'
import test from 'node:test'
import { hierarchyFixture, openHierarchy } from './hierarchy-fixture.mjs'

function fixture() {
  const f = hierarchyFixture(), project = f.projects[0]
  f.projects = [...Array.from({ length: 28 }, (_, i) => ({ ...project, id: 'root-' + i, parent_id: null, name: 'Root ' + String(i).padStart(2, '0') })),
    { ...project, id: 'child', parent_id: 'root-12', name: 'Root child' }]
  f.tasks = []; f.dependencies = []
  return f
}

const twoPaints = page => page.evaluate(() => new Promise(resolve => requestAnimationFrame(() => requestAnimationFrame(resolve))))
const context = page => page.evaluate(() => {
  const scroll = document.getElementById('main-content-scroll'), card = document.getElementById('entity-root-12')
  return { top: scroll.scrollTop, anchor: card.getBoundingClientRect().top - scroll.getBoundingClientRect().top,
    query: document.querySelector('.hierarchy-search-input').value,
    focused: location.hash.includes('focus='), tree: card.querySelector('.tree-toggle')?.textContent,
    statuses: [...document.querySelectorAll('.filter-pill[aria-pressed]')].map(n => [n.textContent, n.getAttribute('aria-pressed')]) }
})

async function backToCard(h, expected, layoutChanged = false) {
  await h.page.getByRole('button', { name: 'Back', exact: true }).click()
  await h.page.locator('#hierarchy-viewport').waitFor()
  await h.page.waitForFunction(() => document.activeElement?.id === 'task-graph-open-root-12', null, { timeout: 6000 }).catch(async error => {
    throw new Error(error.message + ' ' + JSON.stringify(await h.page.evaluate(() => ({ url: location.href, active: document.activeElement?.id,
      marker: document.querySelector('[data-task-graph-navigation]')?.dataset.taskGraphNavigation,
      top: document.getElementById('main-content-scroll')?.scrollTop, target: document.getElementById('entity-root-12')?.getBoundingClientRect().toJSON(), keys: [...document.querySelectorAll('[data-hierarchy-key]')].map(n => n.dataset.hierarchyKey).slice(0, 30) }))))
  })
  await h.idle(); await twoPaints(h.page)
  const actual = await context(h.page)
  assert.equal(actual.focused, expected.focused)
  assert.equal(actual.query, expected.query)
  assert.equal(actual.tree, expected.tree)
  assert.deepEqual(actual.statuses, expected.statuses)
  if (!layoutChanged) assert.ok(Math.abs(actual.top - expected.top) < 1, `scroll ${actual.top} != ${expected.top}`)
  assert.ok(Math.abs(actual.anchor - expected.anchor) < 1, `anchor ${actual.anchor} != ${expected.anchor}`)
}

test('graph action and Back retain focused/unfocused origin independently through browser history', { timeout: 60000 }, async () => {
  const h = await openHierarchy(fixture())
  try {
    await h.page.setViewportSize({ width: 1440, height: 950 })
    await h.start('tab=projects', 'root-0'); await h.idle()
    await h.page.locator('.hierarchy-search-input').fill('Root')
    await h.idle()
    const row = await h.scrollTo('project:root-12')
    await row.locator('.tree-toggle').click(); await h.idle()
    const action = h.page.locator('#task-graph-open-root-12')
    assert.equal(await action.getAttribute('type'), 'button')
    assert.equal(await action.evaluate(n => n.previousElementSibling.getAttribute('aria-label')), 'Move project')
    await h.page.mouse.move(0, 0); await action.evaluate(n => n.blur()); await twoPaints(h.page)
    assert.equal(await action.locator('.card-action-label').evaluate(n => getComputedStyle(n).maxWidth), '0px')
    await action.hover(); await action.locator('.card-action-label').waitFor({ state: 'visible' })
    await h.page.waitForTimeout(320)
    assert.equal(await action.locator('.card-action-label').evaluate(n => getComputedStyle(n).opacity), '1')
    const unfocused = await context(h.page)
    assert.ok(unfocused.top > 0)
    await action.click()
    await h.page.locator('#task-graph-page').waitFor()
    assert.match(h.page.url(), /task-graph=root-12/)
    const graphA = h.page.url()
    await backToCard(h, unfocused)
    await h.page.goForward(); await h.page.locator('#task-graph-page').waitFor()
    assert.equal(h.page.url(), graphA)
    await backToCard(h, unfocused)
    // Navigate to a distinct focused origin while graph A remains in the
    // browser stack, then prove neither entry overwrites the other's receipt.
    await h.page.goForward(); await h.page.locator('#task-graph-page').waitFor()
    const projectCrumb = h.page.locator('.titlebar-breadcrumbs a.focus-crumb').filter({ hasText: 'Root 12' })
    assert.match(await projectCrumb.getAttribute('href'), /focus=project%3Aroot-12$/)
    assert.doesNotMatch(await projectCrumb.getAttribute('href'), /task-graph/)
    await projectCrumb.click()
    await h.page.locator('#hierarchy-viewport').waitFor(); await h.idle()
    await h.page.locator('.hierarchy-search-input').fill('Root 12'); await h.idle()
    const focused = await context(h.page)
    await h.page.locator('#task-graph-open-root-12').click(); await h.page.locator('#task-graph-page').waitFor()
    const graphB = h.page.url()
    assert.notEqual(graphB, graphA)
    await backToCard(h, focused)
    await h.page.goBack(); await h.page.locator('#task-graph-page').waitFor()
    assert.equal(h.page.url(), graphA)
    // Filtered reloads can remeasure offscreen rows; preserve the exact card
    // anchor rather than forcing stale prefix heights over current geometry.
    await backToCard(h, unfocused, true)
    assert.deepEqual(h.errors, []); assert.deepEqual(h.unhandled, [])
  } finally { await h.close() }
})

test('direct graph route Back uses full hierarchy without project focus', { timeout: 30000 }, async () => {
  const h = await openHierarchy(fixture())
  try {
    await h.page.goto(h.origin + '/workspace/' + h.fixture.workspace.id + '#tab=projects&task-graph=root-12')
    await h.page.locator('#task-graph-page').waitFor()
    await h.page.getByRole('button', { name: 'Back', exact: true }).click()
    await h.page.locator('#hierarchy-viewport').waitFor(); await h.idle()
    assert.doesNotMatch(h.page.url(), /focus=/)
    assert.equal(await h.page.locator('#entity-root-12').isVisible(), true)
    assert.deepEqual(h.errors, []); assert.deepEqual(h.unhandled, [])
  } finally { await h.close() }
})

test('a readable focused card restores despite an unrelated failed root load', { timeout: 30000 }, async () => {
  const h = await openHierarchy(fixture())
  try {
    const failure = h.gate(request => request.kind === 'workspace_root', { error: 500 })
    await h.start('tab=projects&focus=project%3Aroot-12', 'root-12')
    await failure.arrived; await h.idle()
    const focused = await context(h.page)
    assert.equal(focused.focused, true)
    await h.page.locator('#task-graph-open-root-12').click()
    await h.page.locator('#task-graph-page').waitFor()
    await backToCard(h, focused)
    assert.deepEqual(h.errors, []); assert.deepEqual(h.unhandled, [])
  } finally { await h.close() }
})
