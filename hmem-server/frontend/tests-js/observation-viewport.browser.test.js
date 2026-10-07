import assert from 'node:assert/strict'
import test from 'node:test'
import { openDiscovery } from './observation-discovery-fixture.mjs'
import { generateObservationScalingFixture, OBSERVATION_SCALING_CONTRACT } from '../perf/observation-scaling.mjs'
import { paint, scanObservationRows, revealObservationRow } from './observation-viewport-fixture.mjs'

const populate = values => {
  const source = generateObservationScalingFixture('large').observations
  return [...source.slice(0, 149), source.at(-1)].map(value => ({ ...value, workspace_id: values[0].workspace_id }))
}

test('production ordered150 Match members and every repeated group stay scroll-reachable under a global row cap', { timeout: 60000 }, async () => {
  const h = await openDiscovery({ width: 1440, height: 900 }, populate)
  try {
    await h.page.addInitScript(() => {
      let owner = null
      document.addEventListener('focusin', event => {
        if (!event.target.classList?.contains('observation-subject-group-toggle')) return
        const row = event.target.closest('[data-observation-key]'), viewport = row?.parentNode
        if (viewport?.id === 'observation-viewport') owner = { control: event.target, row, viewport, parent: viewport.parentNode, stamp: viewport.dataset.observationViewportContext, key: row.dataset.observationKey }
      }, true)
      document.addEventListener('DOMContentLoaded', () => {
        const observer = new MutationObserver(() => {
          if (!owner || window.observationNativeMove || document.activeElement !== document.body) return
          const viewport = document.getElementById('observation-viewport')
          const row = [...(viewport?.querySelectorAll('[data-observation-key]') || [])].find(row => row.dataset.observationKey === owner.key)
          if (!row) return
          window.observationNativeMove = { phase: 'keyed-window-pin-paint', sameControl: row.querySelector('.observation-subject-group-toggle') === owner.control,
            sameRow: row === owner.row, sameViewport: viewport === owner.viewport, sameParent: viewport.parentNode === owner.parent,
            sameStamp: viewport.dataset.observationViewportContext === owner.stamp, nativeFocusFellToBody: true }
          owner = null; observer.disconnect()
        })
        observer.observe(document.body, { childList: true, subtree: true })
      }, { once: true })
    })
    await h.start()
    await h.page.getByRole('button', { name: 'For files', exact: true }).click()
    await h.page.locator('#observation-match-paths').fill(OBSERVATION_SCALING_CONTRACT.paths.join('\n'))
    await h.page.getByRole('button', { name: 'Match files', exact: true }).click(); await h.idle()
    for (const loaded of [50, 100, 150]) {
      assert.equal(await h.page.locator('#observation-viewport').getAttribute('data-observation-loaded-count'), String(loaded))
      const response = h.receipts.filter(value => value.endpoint.endsWith('/match')).at(-1)
      assert.deepEqual(response.payload.paths, OBSERVATION_SCALING_CONTRACT.paths)
      if (loaded < 150) { await h.page.locator('.observation-load-more').click(); await h.idle() }
    }
    const closed = await scanObservationRows(h.page)
    assert.equal(closed.groups.size, 4)
    assert.equal(closed.cards.size, 0)
    for (const key of closed.groups.keys()) {
      const toggle = h.page.locator('.observation-subject-group[data-observation-group=' + JSON.stringify(key) + '] .observation-subject-group-toggle')
      await revealObservationRow(h.page, toggle)
      await toggle.click(); await paint(h.page)
    }
    const expanded = await scanObservationRows(h.page)
    assert.equal(expanded.cards.size, 455, 'all ordered repeated evidence remains reachable, not deduplicated')
    assert.equal(new Set([...expanded.cards.values()].map(row => row.id)).size, 150)
    assert.equal(expanded.keys.size, 461, 'two path labels plus four headers plus455 repeated cards')
    for (const key of closed.groups.keys()) assert.equal(expanded.groups.get(key).expanded, 'true')
    const nativeMove = await h.page.evaluate(() => window.observationNativeMove)
    assert.deepEqual(nativeMove, { phase: 'keyed-window-pin-paint', sameControl: true, sameRow: true, sameViewport: true, sameParent: true, sameStamp: true, nativeFocusFellToBody: true })
    console.log(JSON.stringify({ fixture: 'Observation native keyed movement', ...nativeMove }))
    assert.ok(expanded.maxMounted <= 27)
    const first = h.receipts.find(value => value.endpoint.endsWith('/match')).response.items[0]
    assert.deepEqual(first.path_matches[0].matched_subjects.map(subject => subject.subject), ['src/**/*.elm', 'src/Shared.elm'])
    const rows = [...expanded.cards.values()].filter(value => value.id === first.observation.id)
    assert.equal(rows.length, first.path_matches.reduce((count, match) => count + match.matched_subjects.length, 0))
  } finally { await h.close() }
})

test('production native Tab mounts offscreen logical targets and retained detail survives list eviction', { timeout: 60000 }, async () => {
  const h = await openDiscovery({ width: 320, height: 800 }, populate)
  try {
    await h.start()
    const initial = await h.page.locator('#observation-viewport').getAttribute('data-observation-logical-count')
    assert.equal(initial, '50')
    const last = h.page.locator('.observation-viewport-row').last()
    const originKey = await last.getAttribute('data-observation-key')
    const copy = last.locator('.observation-sha .copyable-value').last()
    await copy.focus(); await paint(h.page); await h.page.keyboard.press('Tab')
    try { await h.page.waitForFunction(key => document.activeElement?.closest('[data-observation-key]')?.dataset.observationKey !== key && document.activeElement?.classList.contains('observation-card'), originKey, { timeout: 5000 }) } catch (error) { console.log('Bounded native focus diagnostic', await h.page.evaluate(() => ({ active: document.activeElement?.outerHTML?.slice(0, 350), stamp: document.querySelector('#observation-viewport')?.dataset.observationViewportContext, keys: [...document.querySelectorAll('[data-observation-key]')].map(row => row.dataset.observationPosition), top: document.querySelector('#main-content-scroll')?.scrollTop }))); throw error }
    const card = h.page.locator('.observation-card:focus'), origin = await card.getAttribute('id')
    await card.focus()
    // The same arrow is already active: its changed attribute is not proof
    // that the stamped bridge has claimed and focused the selected owner.
    await h.page.evaluate(() => {
      window.arrowBridgeFocus = 0
      const focus = HTMLElement.prototype.focus
      HTMLElement.prototype.focus = function (...args) {
        const result = focus.apply(this, args)
        if (this.matches('[data-observation-detail-anchor]')) window.arrowBridgeFocus++
        return result
      }
      const arrow = document.activeElement, scroll = document.getElementById('main-content-scroll')
      window.tabOrigin = { rect: arrow.getBoundingClientRect().toJSON(), scroll: scroll.getBoundingClientRect().toJSON(), top: scroll.scrollTop }
    })
    await h.page.keyboard.press('Enter'); await h.page.waitForFunction(() => window.arrowBridgeFocus > 0 && document.activeElement?.matches('[data-observation-detail-anchor]'), null, { timeout: 5000 })
    const content = await h.page.locator('.observation-detail-content').textContent()
    assert.equal(await h.page.locator('#observation-edit, #observation-edit-content').count(), 0)
    await h.page.locator('#main-content-scroll').evaluate(scroll => { scroll.scrollTop = scroll.scrollHeight }); await paint(h.page)
    assert.equal(await h.page.locator('.observation-detail-content').textContent(), content)
    assert.ok(await h.page.locator('[data-observation-key]').count() <= 28)
    await h.page.locator('[data-observation-detail-anchor]').click()
    await h.page.waitForFunction(() => !document.getElementById('observation-detail'), null, { timeout: 5000 })
    try { await h.page.waitForFunction(id => document.activeElement?.id === id, origin, { timeout: 5000 }) } catch (error) { console.log('Bounded return diagnostic', await h.page.evaluate(id => ({ originMounted: !!document.getElementById(id), active: document.activeElement?.id, positions: [...document.querySelectorAll('[data-observation-key]')].map(row => row.dataset.observationPosition), top: document.querySelector('#main-content-scroll')?.scrollTop, initial: window.tabOrigin, stamp: document.querySelector('#observation-viewport')?.dataset.observationViewportContext }), origin)); throw error }
    assert.equal(await h.page.locator('#observation-detail').count(), 0)
    await h.page.addStyleTag({ content: 'html { font-size: 200%; }' }); await paint(h.page)
    assert.ok(await h.page.evaluate(() => document.documentElement.scrollWidth <= innerWidth + 1))
  } finally { await h.close() }
})
