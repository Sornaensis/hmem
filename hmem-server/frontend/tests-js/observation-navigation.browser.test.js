import assert from 'node:assert/strict'
import test from 'node:test'
import { openObservations } from './observation-navigation-fixture.mjs'
import { paint, scanObservationRows, revealObservationRow } from './observation-viewport-fixture.mjs'

async function flatCard(page, position) {
  await page.waitForFunction(() => document.getElementById('observation-viewport')?.dataset.observationLoadedCount === '40', null, { timeout: 5000 })
  const receipt = await scanObservationRows(page)
  const entry = [...receipt.cards.values()][position]
  assert.ok(entry, 'Complete logical flat position exists')
  const card = page.locator('.observation-result[data-observation-id=' + JSON.stringify(entry.id) + '] .observation-card')
  await revealObservationRow(page, card)
  return card
}

const focusId = page => page.evaluate(() => document.activeElement?.id)
const scrollTop = page => page.locator('#main-content-scroll').evaluate(element => element.scrollTop)
async function assertUnobscured(page, target) {
  const result = await target.evaluate(element => {
    const rect = element.getBoundingClientRect(), main = document.getElementById('main-content-scroll').getBoundingClientRect()
    const top = Math.max(rect.top, main.top, 0), bottom = Math.min(rect.bottom, main.bottom, innerHeight)
    const x = Math.max(1, Math.min(innerWidth - 1, rect.left + rect.width / 2)), y = top + (bottom - top) / 2
    const hit = document.elementFromPoint(x, y)
    return { visible: bottom > top, unobscured: element.contains(hit), target: element.id || element.className, hit: hit?.id || hit?.className, top, bottom }
  })
  assert.ok(result.visible && result.unobscured, JSON.stringify(result))
}
async function installFocusProbe(page) {
  // The arrow is already focused before activation. Observe the real bridge's
  // stamped focus call rather than treating its changed attribute as readiness.
  await page.evaluate(() => {
    if (window.observationArrowFocusCalls) return
    window.observationArrowFocusCalls = []
    const focus = HTMLElement.prototype.focus
    HTMLElement.prototype.focus = function (...args) {
      const result = focus.apply(this, args)
      if (this.matches('.observation-card, #observation-results, [data-observation-detail-anchor]')) window.observationArrowFocusCalls.push({ id: this.id,
        context: document.getElementById('observation-panel')?.dataset.observationContext,
        viewport: document.getElementById('observation-viewport')?.dataset.observationViewportContext })
      return result
    }
  })
}
async function keyboardActivate(page, card) {
  await installFocusProbe(page)
  await card.focus()
  const calls = await page.evaluate(() => window.observationArrowFocusCalls.length)
  const before = await page.locator('#observation-panel').getAttribute('data-observation-context')
  await page.keyboard.press('Enter')
  try { await page.waitForFunction(calls => window.observationArrowFocusCalls.length > calls && document.activeElement?.matches('[data-observation-detail-anchor]'), calls, { timeout: 5000 }) }
  catch (error) { throw new Error(error.message + ' ' + JSON.stringify(await page.evaluate(before => ({ before, after: document.getElementById('observation-panel')?.dataset.observationContext, focus: document.activeElement?.id, heading: !!document.querySelector('[data-observation-detail-anchor]') }), before))) }
}
async function returnToResults(page) {
  await installFocusProbe(page)
  const calls = await page.evaluate(() => window.observationArrowFocusCalls.length)
  await page.locator('[data-observation-detail-anchor]').click()
  await page.waitForFunction(calls => !document.getElementById('observation-detail') && window.observationArrowFocusCalls.length > calls
    && document.activeElement?.matches('.observation-card, #observation-results'), calls, { timeout: 5000 })
}

test('production keyboard detail entry and return restore the exact card and physical scroll', { timeout: 60000 }, async () => {
  const h = await openObservations()
  try {
    await h.start()
    const card = await flatCard(h.page, 25)
    await card.scrollIntoViewIfNeeded(); await card.focus()
    const origin = await card.getAttribute('id'), before = await scrollTop(h.page)
    assert.ok(before > 100)
    await keyboardActivate(h.page, card); await h.idle()
    assert.equal(await focusId(h.page), origin)
    await assertUnobscured(h.page, h.page.locator('[data-observation-detail-anchor]'))
    await h.page.keyboard.press('Tab')
    assert.ok(await h.page.locator('.observation-result:has([data-observation-detail-anchor]) .observation-subject').evaluate(element => element === document.activeElement))
    await assertUnobscured(h.page, h.page.locator('.observation-result:has([data-observation-detail-anchor]) .observation-subject'))
    await returnToResults(h.page)
    await h.page.waitForFunction(id => document.activeElement?.id === id, origin)
    await assertUnobscured(h.page, card)
    const after = await scrollTop(h.page)
    assert.ok(Math.abs(after - before) <= 2, JSON.stringify({ before, after, offsetDifference: after - before }))
  } finally { await h.close() }
})

test('production detail reveal keeps shared scroll authority through subsequent width and height measurement', { timeout: 60000 }, async () => {
  const h = await openObservations()
  try {
    await h.start()
    const card = await flatCard(h.page, 25), origin = await card.getAttribute('id')
    await keyboardActivate(h.page, card); await h.idle()
    const before = await h.page.locator('#observation-viewport').evaluate(root => ({ stamp: JSON.parse(root.dataset.observationViewportContext), width: root.clientWidth }))
    await card.evaluate(control => { window.widthOwner = { control, row: control.closest('[data-observation-key]'), identity: [control.tagName, control.id, control.type, control.className] } })
    await h.page.addStyleTag({ content: '.observation-viewport { width: 90%; } .observation-viewport-row { padding-block: 40px; }' })
    await h.page.waitForFunction(before => {
      const root = document.getElementById('observation-viewport'), stamp = JSON.parse(root?.dataset.observationViewportContext || 'null')
      return stamp?.revision > before.stamp.revision && root.clientWidth !== before.width
    }, before, { timeout: 5000 })
    await paint(h.page)
    try { await h.page.waitForFunction(id => document.activeElement?.id === id, origin, { timeout: 5000 }) }
    catch (error) { throw new Error(error.message + ' ' + JSON.stringify(await h.page.evaluate(id => {
      const root = document.getElementById('observation-viewport'), actual = document.getElementById(id), owner = window.widthOwner
      return { focus: document.activeElement?.outerHTML.slice(0, 160), focusKey: root?.dataset.observationFocusKey, stamp: root?.dataset.observationViewportContext,
        navigation: document.getElementById('observation-panel')?.dataset.observationContext, ready: root?.dataset.observationLayoutReady,
        sameControl: actual === owner.control, sameRow: actual?.closest('[data-observation-key]') === owner.row,
        oldIdentity: owner.identity, identity: actual && [actual.tagName, actual.id, actual.type, actual.className], calls: window.observationArrowFocusCalls }
    }, origin))) }
    await assertUnobscured(h.page, h.page.locator('[data-observation-detail-anchor]'))
    assert.equal(await focusId(h.page), origin)
    await returnToResults(h.page)
    await h.page.waitForFunction(id => document.activeElement?.id === id, origin, { timeout: 5000 })
    await assertUnobscured(h.page, card)
  } finally { await h.close() }
})

test('production repeated match cards and same-ID activation retain read-only ownership and exact origin', { timeout: 60000 }, async () => {
  const h = await openObservations()
  try {
    await h.start(); await h.page.getByRole('button', { name: 'Files', exact: true }).click(); await h.page.locator('#observation-match-paths').fill(h.path)
    await h.page.getByRole('button', { name: 'Match files', exact: true }).click(); await h.idle()
    const toggles = h.page.locator('.observation-subject-group-toggle')
    assert.equal(await toggles.count(), 2)
    const groups = await toggles.evaluateAll(elements => elements.map(element => element.closest('[data-observation-group]').dataset.observationGroup))
    for (const key of groups) {
      const toggle = h.page.locator('[data-observation-group=' + JSON.stringify(key) + '] .observation-subject-group-toggle')
      await revealObservationRow(h.page, toggle); await toggle.click(); await paint(h.page)
    }
    const first = h.page.locator('.observation-result[data-observation-id="observation-0"][data-observation-context-key=' + JSON.stringify(groups[0]) + '] .observation-card')
    const second = h.page.locator('.observation-result[data-observation-id="observation-0"][data-observation-context-key=' + JSON.stringify(groups[1]) + '] .observation-card')
    await revealObservationRow(h.page, first); const firstId = await first.getAttribute('id')
    await revealObservationRow(h.page, second); const secondId = await second.getAttribute('id')
    assert.notEqual(firstId, secondId); await revealObservationRow(h.page, first)
    await keyboardActivate(h.page, first); await h.idle()
    assert.equal(await h.page.locator('#observation-edit, #observation-edit-content').count(), 0)
    const detailRequests = h.receipts.filter(value => value.endpoint.endsWith('/observation-0')).length
    await revealObservationRow(h.page, second); await second.scrollIntoViewIfNeeded(); await second.focus()
    const origin = await second.getAttribute('id'), before = await scrollTop(h.page)
    const narrowWidth = (await second.boundingBox()).width
    await keyboardActivate(h.page, second)
    assert.equal(await h.page.locator('#observation-detail').count(), 1)
    assert.equal(h.receipts.filter(value => value.endpoint.endsWith('/observation-0')).length, detailRequests)
    await returnToResults(h.page)
    await h.page.waitForFunction(id => document.activeElement?.id === id, origin)
    const returned = await second.boundingBox()
    assert.ok(Math.abs(returned.width - narrowWidth) <= 1, 'Inline detail preserves full-width result cards')
    const main = await h.page.locator('#main-content-scroll').boundingBox()
    assert.ok(returned.y + returned.height > main.y && returned.y < main.y + main.height)
    await assertUnobscured(h.page, second)
    assert.equal(await h.page.locator('#observation-detail').count(), 0)
  } finally { await h.close() }
})

test('production tab unmount and direct-link remount retire the completed activation origin', { timeout: 60000 }, async () => {
  const h = await openObservations()
  try {
    await h.start()
    const card = await flatCard(h.page, 25)
    await card.scrollIntoViewIfNeeded(); await keyboardActivate(h.page, card); await h.idle()
    const selected = JSON.parse(await h.page.locator('#observation-panel').getAttribute('data-observation-context')).selectedId
    await h.page.evaluate(() => { location.hash = 'tab=projects' })
    await h.page.waitForFunction(() => !document.getElementById('observation-panel'))
    await h.page.evaluate(id => { location.hash = 'tab=observations&observation=' + id }, selected)
    await h.page.locator('#observation-detail').waitFor(); await h.idle()
    await returnToResults(h.page)
    await h.page.waitForFunction(() => document.activeElement?.id === 'observation-results')
    await assertUnobscured(h.page, h.page.locator('#observation-results'))
  } finally { await h.close() }
})

test('production direct off-page detail has a useful results fallback and late hydration cannot move focus', { timeout: 60000 }, async () => {
  const h = await openObservations()
  try {
    await h.start('tab=observations&observation=observation-99')
    assert.equal((await scanObservationRows(h.page)).cards.size, 40)
    await returnToResults(h.page)
    await h.page.waitForFunction(() => document.activeElement?.id === 'observation-results')
    const held = h.holdDetail('observation-0')
    const next = h.page.locator('.observation-result[data-observation-id="observation-0"] .observation-card').first()
    await revealObservationRow(h.page, next)
    const nativeOrigin = await next.getAttribute('id')
    await keyboardActivate(h.page, next)
    await h.bounded(held.arrived, 10000, 'Held detail request')
    await returnToResults(h.page)
    await h.page.waitForFunction(id => document.activeElement?.id === id, nativeOrigin, { timeout: 5000 })
    const before = await focusId(h.page)
    held.release(); await h.idle()
    assert.equal(await focusId(h.page), before)
    assert.equal(await h.page.locator('#observation-detail').count(), 0)
  } finally { await h.close() }
})

for (const equivalentZoom of [false, true]) test(equivalentZoom ? 'production 400-percent equivalent reflow at 320 CSS pixels with 200-percent text supports read-only content delete and return' : 'production 320 CSS-pixel navigation supports long paths read-only content delete and return', { timeout: 60000 }, async () => {
  const h = await openObservations({ width: 320, height: equivalentZoom ? 200 : 800 })
  try {
    if (equivalentZoom) {
      const cdp = await h.page.context().newCDPSession(h.page)
      await cdp.send('Emulation.setDeviceMetricsOverride', { width: 320, height: 200, deviceScaleFactor: 4, mobile: false })
    }
    await h.start()
    if (equivalentZoom) await h.page.addStyleTag({ content: 'html { font-size: 200%; }' })
    const viewport = await h.page.evaluate(() => ({ width: innerWidth, scale: devicePixelRatio }))
    assert.equal(viewport.width, 320)
    if (equivalentZoom) assert.equal(viewport.scale, 4)
    const layout = await h.page.evaluate(() => ({ mainHeight: document.getElementById('main-content-scroll').clientHeight, sidebarHeight: document.querySelector('.sidebar').clientHeight, sidebarScroll: getComputedStyle(document.querySelector('.sidebar')).overflowY, textSize: getComputedStyle(document.documentElement).fontSize }))
    assert.ok(layout.mainHeight > 0 && layout.sidebarHeight > 0, JSON.stringify(layout))
    assert.equal(layout.sidebarScroll, 'auto')
    if (equivalentZoom) assert.equal(layout.textSize, '32px')
    const card = await flatCard(h.page, 20)
    await card.scrollIntoViewIfNeeded(); await keyboardActivate(h.page, card); await h.idle()
    const heading = await h.page.locator('[data-observation-detail-anchor]').boundingBox()
    assert.ok(heading.y >= 0 && heading.y < (equivalentZoom ? 200 : 800), JSON.stringify(await h.page.evaluate(heading => ({ heading, height: innerHeight, scroll: document.getElementById('main-content-scroll').getBoundingClientRect().toJSON(), fontSize: getComputedStyle(document.documentElement).fontSize }), heading)))
    await assertUnobscured(h.page, h.page.locator('[data-observation-detail-anchor]'))
    await h.page.keyboard.press('Tab')
    assert.ok(await h.page.locator('.observation-result:has([data-observation-detail-anchor]) .observation-subject').evaluate(element => element === document.activeElement))
    await assertUnobscured(h.page, h.page.locator('.observation-result:has([data-observation-detail-anchor]) .observation-subject'))
    const overflow = () => h.page.locator('#observation-panel').evaluate(element => { const rect = element.getBoundingClientRect(); return { content: element.scrollWidth, width: element.clientWidth, outside: [...element.querySelectorAll('*')].filter(child => child.getBoundingClientRect().right > rect.right + 1).slice(0, 8).map(child => ({ tag: child.tagName, class: child.className, text: child.textContent.slice(0, 60), width: child.getBoundingClientRect().width })) } })
    let dimensions = await overflow(); assert.ok(dimensions.content <= dimensions.width + 1, JSON.stringify(dimensions))
    await h.page.locator('.observation-detail-content').click()
    assert.equal(await h.page.locator('#observation-edit, #observation-edit-content').count(), 0)
    dimensions = await overflow(); assert.ok(dimensions.content <= dimensions.width + 1, JSON.stringify(dimensions))
    await h.page.locator('#observation-delete').click()
    await h.page.locator('#observation-delete-cancel').waitFor()
    await h.page.waitForFunction(() => document.activeElement?.id === 'observation-delete-cancel')
    await h.page.keyboard.press('Tab')
    await h.page.waitForFunction(() => document.activeElement?.id === 'observation-delete-confirm', null, { timeout: 5000 })
    assert.equal(await focusId(h.page), 'observation-delete-confirm')
    await h.page.keyboard.press('Escape')
    await h.page.waitForFunction(() => !document.getElementById('observation-delete-dialog'))
    await returnToResults(h.page)
    await h.page.waitForFunction(() => document.activeElement?.classList.contains('observation-card'))
    const returned = await h.page.evaluate(() => ({ card: document.activeElement.getBoundingClientRect().toJSON(), main: document.getElementById('main-content-scroll').getBoundingClientRect().toJSON() }))
    assert.ok(returned.card.bottom > returned.main.top && returned.card.top < returned.main.bottom, JSON.stringify(returned))
    await assertUnobscured(h.page, h.page.locator('.observation-card:focus'))
    if (!equivalentZoom) {
      const selected = h.page.locator('.observation-card:focus')
      await keyboardActivate(h.page, selected); await h.idle()
      await h.page.locator('#observation-delete').click()
      await h.page.locator('#observation-delete-confirm').click(); await h.idle()
      await h.page.waitForFunction(() => document.activeElement?.id === 'observation-results')
      assert.equal((await scanObservationRows(h.page)).cards.size, 39)
      assert.equal(await h.page.locator('#observation-detail').count(), 0)
    }
  } finally { await h.close() }
})
