import assert from 'node:assert/strict'
import test from 'node:test'
import { mkdir } from 'node:fs/promises'
import { join } from 'node:path'
import { openDiscovery } from './observation-discovery-fixture.mjs'
import { paint, revealObservationRow } from './observation-viewport-fixture.mjs'

const context = page => page.locator('#observation-panel').getAttribute('data-observation-context')
const copies = page => page.evaluate(() => window.inlineCopies)
const injectClipboard = page => page.addInitScript(() => {
  window.inlineCopies = []; window.inlineClipboard = 'success'
  Object.defineProperty(navigator.clipboard, 'writeText', { value: value => {
    window.inlineCopies.push(value)
    if (window.inlineClipboard === 'denied') return Promise.reject(new Error('denied'))
    if (window.inlineClipboard === 'pending') return new Promise(resolve => { window.completeInlineCopy = resolve })
    return Promise.resolve()
  } })
})

for (const width of [1440, 320]) test(`production ${width} inline owner, keyboard copy, clipboard outcomes and responsive layout`, { timeout: 60000 }, async () => {
  const h = await openDiscovery({ width, height: 900 }, values => values.map(value => value.id === 'cache-main' ? { ...value, updated_at: '2026-10-07T00:00:00Z' } : value))
  try {
    await injectClipboard(h.page); await h.start()
    const first = h.page.locator('.observation-result[data-observation-id="cache-main"]').first()
    const initial = await context(h.page), requests = h.receipts.length
    for (const key of ['Enter', 'Space']) {
      await first.locator('.observation-subject').focus(); await h.page.keyboard.press(key)
      await h.page.waitForFunction(count => window.inlineCopies.length === count, key === 'Enter' ? 1 : 2)
      assert.equal(await context(h.page), initial)
      assert.equal(await h.page.locator('#observation-detail').count(), 0)
    }
    assert.deepEqual(await copies(h.page), ['src/Main.elm', 'src/Main.elm'])
    await first.locator('.observation-sha .copyable-value').click()
    await h.page.waitForFunction(() => window.inlineCopies.length === 3)
    assert.equal((await copies(h.page))[2], h.sha); assert.equal(h.receipts.length, requests)
    await first.locator('.observation-card').click(); await h.idle()
    assert.equal(await first.locator('#observation-detail').count(), 1)
    assert.equal(await h.page.locator('#observation-detail').count(), 1)
    assert.equal(await first.locator('.observation-detail-content').textContent(), 'Cache evidence for Main')
    assert.equal(await first.locator('button button').count(), 0)
    assert.equal(await h.page.locator('.observation-sha-copy').count(), 0)
    const opened = await context(h.page)
    await first.locator('.observation-detail-workspace .copyable-value').focus(); await h.page.keyboard.press('Space')
    await h.page.waitForFunction(() => window.inlineCopies.length === 4)
    assert.equal((await copies(h.page))[3], h.fixture.workspace.id); assert.equal(await context(h.page), opened)
    await h.page.evaluate(() => { window.inlineClipboard = 'pending' })
    await first.locator('.observation-detail-sha .copyable-value').click()
    await h.page.waitForFunction(() => !!window.completeInlineCopy)
    const beforeAck = await h.page.locator('.toast').count()
    await paint(h.page); assert.equal(await h.page.locator('.toast').count(), beforeAck)
    await h.page.evaluate(() => window.completeInlineCopy()); await paint(h.page)
    assert.ok((await h.page.locator('.toast').allTextContents()).some(value => value.includes('Copied to clipboard')))
    await h.page.evaluate(() => { window.inlineClipboard = 'denied' })
    await first.locator('.observation-detail-sha .copyable-value').click()
    await h.page.getByText('Unable to copy to clipboard', { exact: true }).waitFor()
    assert.equal(await context(h.page), opened)
    assert.equal(await first.locator('.observation-card').textContent(), '▼')
    assert.equal(await first.locator('.observation-card').getAttribute('aria-expanded'), 'true')
    assert.equal(await first.locator('.observation-card').getAttribute('aria-controls'), 'observation-detail')
    assert.equal(await h.page.locator('#observation-edit, #observation-edit-content, #observation-detail-heading, .observation-return, .observation-collapse-label, details.observation-card-provenance').count(), 0)
    assert.equal(await first.getByRole('button', { name: 'Delete', exact: true }).count(), 1)
    assert.equal(await first.locator('.observation-card-footer').count(), 1)
    await first.locator('.observation-detail-content').click()
    assert.equal(await h.page.locator('[contenteditable=true], #observation-edit-content').count(), 0)
    await h.page.locator('#main-content-scroll').evaluate(scroll => { scroll.scrollTop = scroll.scrollHeight }); await paint(h.page)
    assert.equal(await first.locator('.observation-detail-content').textContent(), 'Cache evidence for Main')
    assert.ok(await h.page.locator('[data-observation-key]').count() <= 28)
    await first.locator('.observation-card').focus(); await h.page.keyboard.press('Space'); await paint(h.page)
    assert.equal(await h.page.locator('#observation-detail').count(), 0)
    assert.equal(await first.locator('.observation-card').textContent(), '▶')
    assert.equal(await first.locator('.observation-card').getAttribute('aria-expanded'), 'false')
    const foldedBody = await first.locator('.observation-card').getAttribute('aria-controls')
    assert.equal(await h.page.locator('[id=' + JSON.stringify(foldedBody) + ']').count(), 1)
    await first.locator('.observation-card').focus(); await h.page.keyboard.press('Enter'); await h.idle()
    assert.equal(await h.page.locator('#observation-detail').count(), 1)
    await h.page.locator('#observation-advanced-toggle').click()
    await paint(h.page)
    assert.ok(await h.page.evaluate(() => document.documentElement.scrollWidth <= innerWidth + 1))
    assert.equal(await h.page.locator('.observation-layout').evaluate(el => getComputedStyle(el).display), 'block')
    if (process.env.HMEM_INLINE_SCREENSHOT_DIR) {
      await mkdir(process.env.HMEM_INLINE_SCREENSHOT_DIR, { recursive: true })
      await h.page.locator('.toast').evaluateAll(elements => elements.forEach(el => el.click()))
      await h.page.locator('[data-observation-detail-anchor]').evaluate(el => el.scrollIntoView({ block: 'center' })); await paint(h.page)
      await h.page.screenshot({ path: join(process.env.HMEM_INLINE_SCREENSHOT_DIR, width === 1440 ? 'observation-card-simplified-desktop.png' : 'observation-card-simplified-narrow.png') })
    }
    if (width === 320) { await h.page.addStyleTag({ content: 'html { font-size: 200%; }' }); await paint(h.page); assert.ok(await h.page.evaluate(() => document.documentElement.scrollWidth <= innerWidth + 1)) }
  } finally { await h.close() }
})

test('production repeated match occurrence owns one read-only body and group collapse retires it without moving detail', { timeout: 60000 }, async () => {
  const h = await openDiscovery()
  try {
    await h.start(); await h.page.getByRole('button', { name: 'Files', exact: true }).click()
    await h.page.locator('#observation-match-paths').fill('src/Main.elm')
    await h.page.getByRole('button', { name: 'Match files', exact: true }).click(); await h.idle()
    const groups = await h.page.locator('[data-observation-group]').evaluateAll(elements => elements.map(el => el.dataset.observationGroup))
    for (const group of groups) {
      const toggle = h.page.locator('[data-observation-group=' + JSON.stringify(group) + '] .observation-subject-group-toggle')
      await revealObservationRow(h.page, toggle); await toggle.click(); await paint(h.page)
    }
    const repeated = h.page.locator('.observation-result[data-observation-id="cache-main"]')
    assert.equal(await repeated.count(), 2)
    await repeated.nth(1).locator('.observation-card').click(); await h.idle()
    assert.equal(await repeated.nth(0).locator('#observation-detail').count(), 0)
    assert.equal(await repeated.nth(1).locator('#observation-detail').count(), 1)
    assert.equal(await h.page.locator('.observation-detail-content').textContent(), 'Cache evidence for Main')
    assert.equal(await h.page.locator('#observation-edit-content').count(), 0)
    const ownerGroup = await repeated.nth(1).getAttribute('data-observation-context-key')
    const toggle = h.page.locator('[data-observation-group=' + JSON.stringify(ownerGroup) + '] .observation-subject-group-toggle')
    await toggle.click(); await paint(h.page)
    assert.equal(JSON.parse(await context(h.page)).selectedId, null)
    assert.equal(await h.page.locator('#observation-detail').count(), 0)
    assert.equal(await h.page.locator('#observation-viewport').getAttribute('data-observation-loaded-count'), '2')
    assert.equal(await h.page.locator('[data-observation-detail-anchor]').count(), 0)
    assert.equal(await h.page.locator('.observation-retained-draft').count(), 0)
  } finally { await h.close() }
})
