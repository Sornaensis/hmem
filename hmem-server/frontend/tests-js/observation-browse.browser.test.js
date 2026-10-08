import assert from 'node:assert/strict'
import test from 'node:test'
import { mkdir } from 'node:fs/promises'
import { join } from 'node:path'
import { openDiscovery } from './observation-discovery-fixture.mjs'
import { hierarchyFixture, openHierarchy } from './hierarchy-fixture.mjs'
import { paint, revealObservationRow } from './observation-viewport-fixture.mjs'

const top = h => h.page.locator('#main-content-scroll').evaluate(element => element.scrollTop)
const mode = (h, name) => h.page.locator('.observation-mode-actions').getByRole('button', { name, exact: true })
async function activate(h, name) {
  await mode(h, name).evaluate(element => element.focus({ preventScroll: true }))
  await h.page.keyboard.press('Space'); await paint(h.page)
}
async function position(h, value) {
  await h.page.locator('#main-content-scroll').hover(); await h.page.mouse.wheel(0, 1)
  await paint(h.page)
  await h.page.locator('#main-content-scroll').evaluate((element, value) => { element.scrollTop = value }, value)
  await h.page.evaluate(async () => {
    const scroll = document.getElementById('main-content-scroll'), deadline = performance.now() + 2000
    let previous = null, stable = 0
    while (performance.now() < deadline && stable < 5) {
      await new Promise(resolve => requestAnimationFrame(() => requestAnimationFrame(resolve)))
      const viewport = document.getElementById('observation-viewport')
      const geometry = JSON.stringify([scroll.scrollTop, scroll.scrollHeight, viewport?.dataset.observationViewportContext,
        [...(viewport?.querySelectorAll('[data-observation-key]') || [])].map(row => [row.dataset.observationKey, row.getBoundingClientRect().height])])
      stable = geometry === previous ? stable + 1 : 0; previous = geometry
    }
    if (stable < 5) throw new Error('Observation geometry did not settle before scoped browse action')
  })
  return top(h)
}
async function gateFacets(h, empty = false, failure = false) {
  let release, arrived
  const wait = new Promise(resolve => { release = resolve }), arrival = new Promise(resolve => { arrived = resolve })
  let used = false
  await h.page.route('**/api/v1/observations/subject-facets?**', async route => {
    if (used) { await route.fallback(); return }
    used = true; arrived(); await wait
    if (failure) await route.fulfill({ status: 500, body: 'Controlled facet failure' })
    else if (empty) await route.fulfill({ status: 200, contentType: 'application/json', body: JSON.stringify({ items: [], has_more: false }) })
    else await route.fallback()
  })
  return { release, arrival }
}
async function screenshot(h, name) {
  if (!process.env.HMEM_BROWSE_SCREENSHOT_DIR) return
  await mkdir(process.env.HMEM_BROWSE_SCREENSHOT_DIR, { recursive: true })
  await h.page.screenshot({ path: join(process.env.HMEM_BROWSE_SCREENSHOT_DIR, name) })
}

test('Observation mode changes preserve current physical scroll through delayed responses and reflow, with newer scroll and focus authoritative', { timeout: 60000 }, async () => {
  const h = await openDiscovery({ width: 2560, height: 900 })
  let held
  try {
    await h.start(); const positioned = await position(h, 160)
    assert.ok(positioned > 100)
    held = await gateFacets(h)
    const subject = await mode(h, 'Subject').boundingBox()
    const before = await top(h)
    await h.page.mouse.click(subject.x + subject.width / 2, subject.y + subject.height / 2)
    await h.bounded(held.arrival, 5000, 'Subject request arrival'); await paint(h.page)
    assert.equal(await top(h), before, 'immediate mode switch retains physical offset')
    assert.equal(await h.page.locator('#main-content-scroll').evaluate(element => getComputedStyle(element).overflowAnchor), 'none', 'native anchoring is disabled only by the current preservation phase')
    assert.equal(await mode(h, 'Subject').evaluate(element => document.activeElement === element), true, 'clicked mode retains keyboard focus')
    const latest = await position(h, before + 40)
    assert.equal(latest, before + 40, 'newer scroll remains exact while the response is held')
    await h.page.locator('#observation-query').evaluate(element => element.focus({ preventScroll: true }))
    held.release(); await h.idle(); await paint(h.page)
    assert.equal(await top(h), latest, 'delayed replacement retains the current offset, including newer input')
    assert.equal(await h.page.locator('#observation-query').evaluate(element => document.activeElement === element), true)
    await h.page.addStyleTag({ content: '.observation-facet { font-size: 19px; }' }); await h.page.waitForTimeout(150); await paint(h.page)
    assert.equal(await top(h), latest, 'later measured height changes do not move the physical offset')
    assert.equal(await h.page.locator('#observation-viewport').evaluate(element => parseFloat(getComputedStyle(element).minHeight)), 0, 'settled response releases temporary extent')
    await activate(h, 'All'); await h.idle(); await paint(h.page)
    assert.equal(await top(h), latest, 'reverse switch also retains physical offset')
    await screenshot(h, 'wide-observations-scroll.png')
    assert.ok(await h.page.locator('[data-observation-key]').count() <= 27)
  } finally { held?.release(); await h.close() }
})

test('superseded mode/workspace replies cannot restore scroll and shorter results clamp naturally', { timeout: 60000 }, async () => {
  const other = hierarchyFixture(); other.workspace = { ...other.workspace, id: 'browse-other', name: 'Browse other' }; other.projects = []; other.tasks = []; other.observations = []
  const h = await openDiscovery(undefined, values => values, [other])
  let held
  try {
    await h.start(); const before = await position(h, 1600)
    assert.ok(before > 1200, 'rapid replacement starts deeper than the viewport height')
    held = await gateFacets(h)
    await activate(h, 'Subject'); await h.bounded(held.arrival, 5000, 'First Subject request arrival')
    await activate(h, 'All'); await h.idle(); await paint(h.page)
    const current = await top(h)
    assert.equal(current, before)
    held.release(); await h.idle(); await paint(h.page)
    assert.equal(await top(h), current); assert.equal(await mode(h, 'All').getAttribute('aria-pressed'), 'true')
    held = await gateFacets(h, true)
    await activate(h, 'Subject'); await h.bounded(held.arrival, 5000, 'Shorter Subject request arrival')
    assert.equal(await top(h), current)
    held.release(); await h.idle(); await paint(h.page)
    const shorter = await h.page.locator('#main-content-scroll').evaluate(element => ({ top: element.scrollTop, bottom: Math.max(0, element.scrollHeight - element.clientHeight), extent: parseFloat(getComputedStyle(document.getElementById('observation-viewport')).minHeight) }))
    assert.equal(shorter.extent, 0); assert.equal(shorter.top, shorter.bottom)
    await activate(h, 'All'); await h.idle(); await position(h, 120)
    held = await gateFacets(h, false, true)
    await activate(h, 'Subject'); await h.bounded(held.arrival, 5000, 'Failed Subject request arrival')
    held.release(); await h.idle(); await paint(h.page)
    assert.ok((await h.page.locator('.observation-state-error').innerText()).includes('Unable to load shared subjects'))
    assert.equal(await h.page.locator('#observation-viewport').evaluate(element => parseFloat(getComputedStyle(element).minHeight)), 0, 'failed request also releases temporary extent')
    await activate(h, 'All'); await h.idle(); await position(h, 120)
    held = await gateFacets(h)
    await activate(h, 'Subject'); await h.bounded(held.arrival, 5000, 'Old workspace Subject request arrival')
    await h.page.locator('a.sidebar-link[href="/workspace/browse-other"]').click({ timeout: 5000 })
    await h.page.getByRole('button', { name: 'Observations', exact: true }).click(); await h.idle()
    const switched = await top(h)
    held.release(); await h.idle(); await paint(h.page)
    assert.equal(await top(h), switched); assert.ok(h.page.url().includes('/workspace/browse-other'))
    assert.equal(await h.page.locator('[data-observation-key]').count(), 0)
    assert.equal(await h.page.locator('#observation-viewport').evaluate(element => parseFloat(getComputedStyle(element).minHeight)), 0)
    assert.equal(await h.page.locator('#main-content-scroll').evaluate(element => getComputedStyle(element).overflowAnchor), 'auto', 'workspace replacement retires native anchoring suppression')
    assert.deepEqual(h.errors, [])
  } finally { held?.release(); await h.close() }
})

test('All and Subject keep physical scroll when leaving Files with an expanded subject group', { timeout: 60000 }, async () => {
  const h = await openDiscovery(undefined, values => values.map(value => ({ ...value, subjects: [{ subject_kind: 'file', subject: value.subject }, { subject_kind: 'glob', subject: 'src/**/*.elm' }] })))
  let held
  try {
    await h.start()
    for (const name of ['All', 'Subject']) {
      await h.page.getByRole('button', { name: 'Files', exact: true }).click()
      await h.page.locator('#observation-match-paths').fill('src/Main.elm')
      await h.page.getByRole('button', { name: 'Match files', exact: true }).click(); await h.idle()
      assert.equal(await h.page.locator('#main-content-scroll').evaluate(element => getComputedStyle(element).overflowAnchor), 'auto', 'explicit Files navigation retires native anchoring suppression')
      const group = h.page.locator('.observation-subject-group-toggle').filter({ hasText: 'Glob' })
      if (await group.getAttribute('aria-expanded') === 'false') await group.click()
      await paint(h.page)
      const before = await position(h, 400)
      assert.ok(before > 100)
      if (name === 'Subject') held = await gateFacets(h)
      await activate(h, name)
      if (held) await h.bounded(held.arrival, 5000, 'Files to Subject arrival')
      assert.equal(await top(h), before, 'internal group reset does not retire mode scroll preservation')
      held?.release(); await h.idle(); await paint(h.page)
      assert.equal(await top(h), before)
    }
    await h.page.getByRole('button', { name: 'Projects', exact: true }).click(); await h.idle()
    assert.equal(await h.page.locator('#observation-viewport').count(), 0)
    assert.equal(await h.page.locator('#main-content-scroll').evaluate(element => getComputedStyle(element).overflowAnchor), 'auto', 'unmounted Observation roots cannot suppress native anchoring')
  } finally { held?.release(); await h.close() }
})

for (const width of [2560, 320]) test(`responsive ${width} Observation and Project outer card surfaces cap at 78rem with aligned measured wrappers`, { timeout: 60000 }, async () => {
  const h = await openDiscovery({ width, height: 900 }, values => values.map(value => ({ ...value, content: 'A long content line '.repeat(100), updated_at: value.id === 'cache-main' ? '2026-10-07T00:00:00Z' : value.updated_at })))
  const project = await openHierarchy()
  try {
    await h.start(); await project.page.setViewportSize({ width, height: 900 }); await project.start(); await project.idle()
    for (const [browser, selector, surface] of [[h, '#observation-viewport', '.observation-result'], [project, '#hierarchy-viewport', '.tree-card, .card-project, .hierarchy-task-family']]) {
      const geometry = await browser.page.locator(selector).evaluate((element, surface) => {
        const box = element.getBoundingClientRect(), parent = element.parentElement.getBoundingClientRect(), rem = parseFloat(getComputedStyle(document.documentElement).fontSize)
        return { width: box.width, left: box.left, parentLeft: parent.left, parentWidth: parent.width, cap: 78 * rem, cards: [...element.querySelectorAll(surface)].map(card => { const bounds = card.getBoundingClientRect(); return { left: bounds.left, right: bounds.right } }), right: box.right, overflow: document.documentElement.scrollWidth > innerWidth + 1 }
      }, surface)
      assert.ok(geometry.width <= geometry.cap + 1); assert.ok(Math.abs(geometry.left - geometry.parentLeft) <= 1)
      assert.ok(geometry.cards.length > 0); assert.ok(geometry.cards.every(card => card.left >= geometry.left - 1 && card.right <= geometry.right + 1))
      assert.equal(geometry.overflow, false)
      if (width === 2560) { assert.ok(Math.abs(geometry.width - geometry.cap) <= 1); assert.ok(geometry.parentWidth > geometry.cap + 100) }
      if (width === 320) assert.ok(geometry.width <= geometry.parentWidth + 1)
    }
    await activate(h, 'Subject'); await h.idle(); await paint(h.page)
    assert.ok(await h.page.locator('.observation-facet').first().evaluate(element => element.getBoundingClientRect().width <= parseFloat(getComputedStyle(document.documentElement).fontSize) * 78 + 1))
    await screenshot(h, width === 2560 ? 'wide-subjects.png' : 'narrow-subjects.png')
    await screenshot(project, width === 2560 ? 'wide-projects.png' : 'narrow-projects.png')
    await activate(h, 'All'); await h.idle()
    const main = h.page.locator('.observation-result[data-observation-id="cache-main"] .observation-card').first()
    await revealObservationRow(h.page, main); await main.click({ timeout: 5000 }); await h.idle()
    await h.page.getByRole('button', { name: 'Files', exact: true }).click()
    await h.page.locator('#observation-match-paths').fill('docs/Guide-000.md')
    await h.page.getByRole('button', { name: 'Match files', exact: true }).click(); await h.idle()
    const fileGeometry = await h.page.evaluate(() => {
      const viewport = document.getElementById('observation-viewport').getBoundingClientRect(), detail = document.querySelector('.observation-detached').getBoundingClientRect()
      return { cap: parseFloat(getComputedStyle(document.documentElement).fontSize) * 78, list: viewport.width, detail: detail.width, aligned: viewport.left === detail.left,
        groups: [...document.querySelectorAll('.observation-path-group, .observation-subject-group')].map(element => { const box = element.getBoundingClientRect(); return box.left >= viewport.left && box.right <= viewport.right + 1 }) }
    })
    assert.ok(fileGeometry.list <= fileGeometry.cap + 1 && fileGeometry.detail <= fileGeometry.cap + 1 && fileGeometry.aligned)
    assert.ok(fileGeometry.groups.length > 0 && fileGeometry.groups.every(Boolean))
    await project.scrollTo('task:subtask-000'); await paint(project.page)
    const children = await project.page.locator('.card-subtask').evaluateAll(elements => elements.map(element => { const box = element.getBoundingClientRect(), family = element.closest('[data-task-family]').getBoundingClientRect(); return { left: box.left, right: box.right, familyLeft: family.left, familyRight: family.right } }))
    assert.ok(children.length > 0 && children.every(child => child.left > child.familyLeft && child.right < child.familyRight), JSON.stringify(children))
  } finally { await h.close(); await project.close() }
})
