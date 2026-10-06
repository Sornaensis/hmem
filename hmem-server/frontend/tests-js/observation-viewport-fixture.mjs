import assert from 'node:assert/strict'

export async function paint(page) {
  await page.evaluate(() => new Promise(resolve => requestAnimationFrame(() => requestAnimationFrame(resolve))))
}

export async function scanObservationRows(page) {
  const keys = new Set(), positions = new Map(), cards = new Map(), groups = new Map(), facets = new Map(), paths = new Map()
  await page.locator('#main-content-scroll').evaluate(scroll => { scroll.scrollTop = 0 })
  const deadline = Date.now() + 15000
  let end = false, maxMounted = 0
  while (Date.now() < deadline) {
    await paint(page)
    const receipt = await page.locator('#observation-viewport').evaluate(root => ({
      logicalCount: Number(root.dataset.observationLogicalCount), keys: [...root.querySelectorAll('[data-observation-key]')].map(row => ({ key: row.dataset.observationKey, position: Number(row.dataset.observationPosition) })),
      cards: [...root.querySelectorAll('.observation-result')].map(row => ({ key: row.closest('[data-observation-key]').dataset.observationKey, position: Number(row.closest('[data-observation-key]').dataset.observationPosition), id: row.dataset.observationId, context: row.dataset.observationContextKey })),
      groups: [...root.querySelectorAll('[data-observation-group]')].map(row => ({ key: row.dataset.observationGroup, position: Number(row.closest('[data-observation-key]').dataset.observationPosition), label: row.querySelector('.observation-subject-group-toggle').textContent, expanded: row.querySelector('.observation-subject-group-toggle').getAttribute('aria-expanded') })),
      facets: [...root.querySelectorAll('.observation-facet-card')].map(row => ({ key: row.closest('[data-observation-key]').dataset.observationKey, position: Number(row.closest('[data-observation-key]').dataset.observationPosition), label: row.textContent })),
      paths: [...root.querySelectorAll('.observation-path-heading')].map(row => ({ key: row.id, position: Number(row.closest('[data-observation-key]').dataset.observationPosition), path: row.textContent }))
    }))
    maxMounted = Math.max(maxMounted, receipt.keys.length)
    assert.ok(receipt.keys.length <= 27, 'global logical rows plus two pins')
    receipt.keys.forEach(row => { keys.add(row.key); positions.set(row.key, row.position) }); receipt.cards.forEach(row => cards.set(row.key, row)); receipt.groups.forEach(row => groups.set(row.key, row))
    receipt.facets.forEach(row => facets.set(row.key, row))
    receipt.paths.forEach(row => paths.set(row.key, row.path))
    if (end && keys.size === receipt.logicalCount) {
      const ordered = values => new Map([...values].sort((a, b) => a[1].position - b[1].position))
      return { keys: new Set([...positions].sort((a, b) => a[1] - b[1]).map(([key]) => key)), cards: ordered(cards), groups: ordered(groups), facets: ordered(facets),
        paths: new Map([...paths].sort((a, b) => positions.get(a[0]) - positions.get(b[0]))), maxMounted }
    }
    if (end) {
      // Newly measured heights can extend the physical end or anchor a paint
      // past an unseen row. Revisit actual scroll geometry within the same
      // deadline; success requires every current logical key to be observed.
      await page.locator('#main-content-scroll').evaluate(scroll => { scroll.scrollTop = 0 })
      end = false
      continue
    }
    end = await page.locator('#main-content-scroll').evaluate(scroll => { const before = scroll.scrollTop; scroll.scrollTop += Math.max(120, scroll.clientHeight / 2); return before === scroll.scrollTop })
  }
  const geometry = await page.locator('#observation-viewport').evaluate(root => ({ logicalCount: root.dataset.observationLogicalCount,
    top: document.getElementById('main-content-scroll').scrollTop, height: document.getElementById('main-content-scroll').scrollHeight,
    mountedPositions: [...root.querySelectorAll('[data-observation-key]')].map(row => row.dataset.observationPosition) }))
  throw new Error('Observation logical scan exceeded its finite 15-second deadline: ' + JSON.stringify({ observed: keys.size, positions: [...positions.values()].sort((a, b) => a - b), ...geometry }))
}

export async function revealObservationRow(page, predicate) {
  await page.locator('#main-content-scroll').evaluate(scroll => { scroll.scrollTop = 0 })
  const deadline = Date.now() + 15000
  let end = false
  while (Date.now() < deadline) {
    await paint(page)
    const row = page.locator('[data-observation-key]').filter({ has: predicate })
    if (await row.count()) {
      // Acquire the same native focus pin a keyboard user would hold before
      // automatic actionability scrolling can change the mounted window.
      const key = await row.first().getAttribute('data-observation-key')
      await predicate.first().evaluate(control => control.focus({ preventScroll: true }))
      await page.waitForFunction(key => {
        const root = document.getElementById('observation-viewport')
        return root?.dataset.observationFocusKey === key && root.contains(document.activeElement)
          && document.activeElement.closest('[data-observation-key]')?.dataset.observationKey === key
      }, key, { timeout: 5000 })
      await row.first().scrollIntoViewIfNeeded(); await paint(page); return row.first()
    }
    if (end) break
    end = await page.locator('#main-content-scroll').evaluate(scroll => { const before = scroll.scrollTop; scroll.scrollTop += Math.max(120, scroll.clientHeight / 2); return before === scroll.scrollTop })
  }
  throw new Error('Observation logical target is not scroll-reachable')
}
