import assert from 'node:assert/strict'
import test from 'node:test'
import { openObservations } from './observation-navigation-fixture.mjs'

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
async function keyboardActivate(page, card) {
  await card.focus()
  const before = await page.locator('#observation-panel').getAttribute('data-observation-context')
  await page.keyboard.press('Enter')
  try { await page.waitForFunction(() => document.activeElement?.id === 'observation-detail-heading', null, { timeout: 5000 }) }
  catch (error) { throw new Error(error.message + ' ' + JSON.stringify(await page.evaluate(before => ({ before, after: document.getElementById('observation-panel')?.dataset.observationContext, focus: document.activeElement?.id, heading: !!document.getElementById('observation-detail-heading') }), before))) }
}
async function returnToResults(page) { await page.getByRole('button', { name: 'Back to results', exact: true }).click(); await page.waitForFunction(() => !document.getElementById('observation-detail')) }

test('production keyboard detail entry and return restore the exact card and physical scroll', { timeout: 60000 }, async () => {
  const h = await openObservations()
  try {
    await h.start()
    const card = h.page.locator('.observation-card').nth(25)
    await card.scrollIntoViewIfNeeded(); await card.focus()
    const origin = await card.getAttribute('id'), before = await scrollTop(h.page)
    assert.ok(before > 100)
    await keyboardActivate(h.page, card); await h.idle()
    assert.equal(await focusId(h.page), 'observation-detail-heading')
    await assertUnobscured(h.page, h.page.locator('#observation-detail-heading'))
    await h.page.keyboard.press('Tab')
    assert.ok(await h.page.locator('.observation-return').evaluate(element => element === document.activeElement))
    await assertUnobscured(h.page, h.page.locator('.observation-return'))
    await returnToResults(h.page)
    await h.page.waitForFunction(id => document.activeElement?.id === id, origin)
    await assertUnobscured(h.page, card)
    assert.ok(Math.abs(await scrollTop(h.page) - before) <= 2)
  } finally { await h.close() }
})

test('production repeated match cards and same-ID activation retain editor ownership and exact origin', { timeout: 60000 }, async () => {
  const h = await openObservations()
  try {
    await h.start(); await h.page.getByRole('button', { name: 'For files', exact: true }).click(); await h.page.locator('#observation-match-paths').fill(h.path)
    await h.page.getByRole('button', { name: 'Match files', exact: true }).click(); await h.idle()
    const toggles = h.page.locator('.observation-subject-group-toggle')
    assert.equal(await toggles.count(), 2)
    await toggles.nth(0).click(); await toggles.nth(1).click()
    const first = h.page.locator('.observation-subject-group').nth(0).locator('.observation-card').first()
    const second = h.page.locator('.observation-subject-group').nth(1).locator('.observation-card').first()
    assert.notEqual(await first.getAttribute('id'), await second.getAttribute('id'))
    await keyboardActivate(h.page, first); await h.idle()
    await h.page.locator('#observation-edit').click()
    await h.page.locator('#observation-edit-content').fill('Protected navigation draft')
    const detailRequests = h.receipts.filter(value => value.endpoint.endsWith('/observation-0')).length
    await second.scrollIntoViewIfNeeded(); await second.focus()
    const origin = await second.getAttribute('id'), before = await scrollTop(h.page)
    const narrowWidth = (await second.boundingBox()).width
    await keyboardActivate(h.page, second)
    assert.equal(await h.page.locator('#observation-edit-content').inputValue(), 'Protected navigation draft')
    assert.equal(h.receipts.filter(value => value.endpoint.endsWith('/observation-0')).length, detailRequests)
    await returnToResults(h.page)
    await h.page.waitForFunction(id => document.activeElement?.id === id, origin)
    const returned = await second.boundingBox()
    assert.ok(returned.width > narrowWidth, 'Closing detail widens the repeated result cards')
    const main = await h.page.locator('#main-content-scroll').boundingBox()
    assert.ok(returned.y + returned.height > main.y && returned.y < main.y + main.height)
    await assertUnobscured(h.page, second)
    await h.page.getByRole('button', { name: 'Return to draft', exact: true }).click()
    assert.equal(await h.page.locator('#observation-edit-content').inputValue(), 'Protected navigation draft')
  } finally { await h.close() }
})

test('production tab unmount and direct-link remount retire the completed activation origin', { timeout: 60000 }, async () => {
  const h = await openObservations()
  try {
    await h.start()
    const card = h.page.locator('.observation-card').nth(25)
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
    assert.equal(await h.page.locator('.observation-card').count(), 40)
    await returnToResults(h.page)
    await h.page.waitForFunction(() => document.activeElement?.id === 'observation-results')
    const held = h.holdDetail('observation-0')
    await keyboardActivate(h.page, h.page.locator('.observation-card').filter({ hasText: /Observation 0\n/ }).first())
    await h.bounded(held.arrived, 10000, 'Held detail request')
    await returnToResults(h.page)
    const before = await focusId(h.page)
    held.release(); await h.idle()
    assert.equal(await focusId(h.page), before)
    assert.equal(await h.page.locator('#observation-detail').count(), 0)
  } finally { await h.close() }
})

for (const equivalentZoom of [false, true]) test(equivalentZoom ? 'production 400-percent equivalent reflow at 320 CSS pixels with 200-percent text supports edit delete and return' : 'production 320 CSS-pixel navigation supports long paths edit delete and return', { timeout: 60000 }, async () => {
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
    const card = h.page.locator('.observation-card').nth(20)
    await card.scrollIntoViewIfNeeded(); await keyboardActivate(h.page, card); await h.idle()
    const heading = await h.page.locator('#observation-detail-heading').boundingBox()
    assert.ok(heading.y >= 0 && heading.y < (equivalentZoom ? 200 : 800), JSON.stringify(await h.page.evaluate(heading => ({ heading, height: innerHeight, scroll: document.getElementById('main-content-scroll').getBoundingClientRect().toJSON(), fontSize: getComputedStyle(document.documentElement).fontSize }), heading)))
    await assertUnobscured(h.page, h.page.locator('#observation-detail-heading'))
    await h.page.keyboard.press('Tab')
    assert.ok(await h.page.locator('.observation-return').evaluate(element => element === document.activeElement))
    await assertUnobscured(h.page, h.page.locator('.observation-return'))
    const overflow = () => h.page.locator('#observation-panel').evaluate(element => { const rect = element.getBoundingClientRect(); return { content: element.scrollWidth, width: element.clientWidth, outside: [...element.querySelectorAll('*')].filter(child => child.getBoundingClientRect().right > rect.right + 1).slice(0, 8).map(child => ({ tag: child.tagName, class: child.className, text: child.textContent.slice(0, 60), width: child.getBoundingClientRect().width })) } })
    let dimensions = await overflow(); assert.ok(dimensions.content <= dimensions.width + 1, JSON.stringify(dimensions))
    await h.page.locator('#observation-edit').click()
    await h.page.locator('#observation-edit-content').fill('Narrow screen editable content')
    await h.page.getByRole('button', { name: 'Save content', exact: true }).click(); await h.idle()
    assert.ok((await h.page.locator('.observation-detail-content').innerText()).includes('Narrow screen editable content'))
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
      assert.equal(await h.page.locator('.observation-card').count(), 39)
      assert.equal(await h.page.locator('#observation-detail').count(), 0)
    }
  } finally { await h.close() }
})
