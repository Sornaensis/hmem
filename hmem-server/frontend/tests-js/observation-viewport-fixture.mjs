import assert from 'node:assert/strict'

export async function paint(page) {
  await page.evaluate(() => new Promise(resolve => requestAnimationFrame(() => requestAnimationFrame(resolve))))
}

async function retireSelectedReveal(page) {
  // Deliberate scanning is user scroll intent even without a selected detail.
  // It supersedes queued anchor/reveal work, while preserving native/model
  // focus pins and their exact stamped admission and acknowledgement guards.
  await page.locator('#main-content-scroll').hover()
  await page.mouse.wheel(0, 1)
  await paint(page)
}

export async function scanObservationRows(page) {
  await retireSelectedReveal(page)
  const keys = new Set(), positions = new Map(), cards = new Map(), groups = new Map(), facets = new Map(), paths = new Map()
  await page.locator('#main-content-scroll').evaluate(scroll => { const list = document.getElementById('observation-viewport'); scroll.scrollTop += list.getBoundingClientRect().top - scroll.getBoundingClientRect().top })
  const deadline = Date.now() + 15000
  let end = false, maxMounted = 0
  while (Date.now() < deadline) {
    await paint(page)
    // Capture every half-height sample and advance in one browser round trip;
    // RPC overhead must not consume the fixed logical reachability deadline.
    const receipt = await page.locator('#observation-viewport').evaluate((root, atEnd) => {
      const scroll = document.getElementById('main-content-scroll')
      const before = scroll.scrollTop
      const sample = {
      logicalCount: Number(root.dataset.observationLogicalCount), keys: [...root.querySelectorAll('[data-observation-key]')].map(row => ({ key: row.dataset.observationKey, position: Number(row.dataset.observationPosition) })),
      cards: [...root.querySelectorAll('.observation-result')].map(row => ({ key: row.closest('[data-observation-key]').dataset.observationKey, position: Number(row.closest('[data-observation-key]').dataset.observationPosition), id: row.dataset.observationId, context: row.dataset.observationContextKey })),
      groups: [...root.querySelectorAll('[data-observation-group]')].map(row => ({ key: row.dataset.observationGroup, position: Number(row.closest('[data-observation-key]').dataset.observationPosition), label: row.querySelector('.observation-subject-group-toggle').textContent, expanded: row.querySelector('.observation-subject-group-toggle').getAttribute('aria-expanded') })),
      facets: [...root.querySelectorAll('.observation-facet-card')].map(row => ({ key: row.closest('[data-observation-key]').dataset.observationKey, position: Number(row.closest('[data-observation-key]').dataset.observationPosition), label: row.closest('.observation-facet').textContent })),
      paths: [...root.querySelectorAll('.observation-path-heading')].map(row => ({ key: row.id, position: Number(row.closest('[data-observation-key]').dataset.observationPosition), path: row.textContent }))
      }
      if (atEnd) scroll.scrollTop += root.getBoundingClientRect().top - scroll.getBoundingClientRect().top
      else scroll.scrollTop += Math.max(120, scroll.clientHeight / 2)
      return { ...sample, end: !atEnd && before === scroll.scrollTop }
    }, end)
    maxMounted = Math.max(maxMounted, receipt.keys.length)
    assert.ok(receipt.keys.length <= 28, '25 ordinary logical rows plus three deduplicated owners')
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
      end = false
      continue
    }
    end = receipt.end
  }
  const geometry = await page.locator('#observation-viewport').evaluate(root => ({ logicalCount: root.dataset.observationLogicalCount,
    top: document.getElementById('main-content-scroll').scrollTop, height: document.getElementById('main-content-scroll').scrollHeight, clientHeight: document.getElementById('main-content-scroll').clientHeight, rootRect: root.getBoundingClientRect().toJSON(), scrollRect: document.getElementById('main-content-scroll').getBoundingClientRect().toJSON(),
    mountedPositions: [...root.querySelectorAll('[data-observation-key]')].map(row => row.dataset.observationPosition) }))
  throw new Error('Observation logical scan exceeded its finite 15-second deadline: ' + JSON.stringify({ observed: keys.size, positions: [...positions.values()].sort((a, b) => a - b), ...geometry }))
}

export async function revealObservationRow(page, predicate) {
  await retireSelectedReveal(page)
  await page.locator('#main-content-scroll').evaluate(scroll => { const list = document.getElementById('observation-viewport'); scroll.scrollTop += list.getBoundingClientRect().top - scroll.getBoundingClientRect().top })
  const deadline = Date.now() + 15000
  let end = false
  while (Date.now() < deadline) {
    await paint(page)
    const row = page.locator('[data-observation-key]').filter({ has: predicate })
    if (await row.count()) {
      const distance = await predicate.first().evaluate(element => {
        const bounds = element.getBoundingClientRect(), host = document.getElementById('main-content-scroll').getBoundingClientRect()
        return bounds.top < host.top || bounds.bottom > host.bottom ? (bounds.top + bounds.bottom - host.top - host.bottom) / 2 : 0
      })
      if (Math.abs(distance) > 1) {
        await page.locator('#main-content-scroll').hover(); await page.mouse.wheel(0, distance); await paint(page)
        continue
      }
      // Acquire the same native focus pin a keyboard user would hold before
      // automatic actionability scrolling can change the mounted window.
      const key = await row.first().getAttribute('data-observation-key')
      const settled = await predicate.first().evaluate(async (control, { key, remaining }) => {
        const end = performance.now() + Math.min(1000, remaining)
        let previous = null, stable = 0, ownedNode = null
        while (performance.now() < end) {
          await new Promise(resolve => requestAnimationFrame(() => requestAnimationFrame(resolve)))
          const root = document.getElementById('observation-viewport'), host = document.getElementById('main-content-scroll')
          const target = [...(root?.querySelectorAll('[data-observation-key]') || [])].find(element => element.dataset.observationKey === key)
          if (!target || !host || !control.isConnected || !target.contains(control)) return false
          const bounds = control.getBoundingClientRect(), edge = host.getBoundingClientRect(), rowBounds = target.getBoundingClientRect()
          const geometry = JSON.stringify([root.dataset.observationViewportContext, [...root.querySelectorAll('[data-observation-key]')].map(element => element.dataset.observationKey), host.scrollTop, host.scrollHeight, bounds.top, bounds.height, rowBounds.height])
          stable = target === ownedNode && geometry === previous ? stable + 1 : 0
          previous = geometry; ownedNode = target
          if (stable >= 3) {
            if (bounds.top < edge.top || bounds.bottom > edge.bottom) return false
            control.focus({ preventScroll: true })
            return true
          }
        }
        return false
      }, { key, remaining: Math.max(0, deadline - Date.now()) })
      if (!settled) continue
      try { await page.waitForFunction(key => {
        const root = document.getElementById('observation-viewport')
        return root?.dataset.observationFocusKey === key && root.contains(document.activeElement)
          && document.activeElement.closest('[data-observation-key]')?.dataset.observationKey === key
      }, key, { timeout: 5000 }) } catch (error) {
        throw new Error(error.message + ' ' + JSON.stringify(await page.evaluate(key => ({ key,
          focus: document.activeElement?.outerHTML?.slice(0, 300), claimed: document.getElementById('observation-viewport')?.dataset.observationFocusKey,
          stamp: document.getElementById('observation-viewport')?.dataset.observationViewportContext,
          mounted: [...document.querySelectorAll('[data-observation-key]')].map(row => row.dataset.observationPosition), scroll: document.getElementById('main-content-scroll')?.getBoundingClientRect().toJSON(), top: document.getElementById('main-content-scroll')?.scrollTop,
          navigation: document.getElementById('observation-panel')?.dataset.observationContext }), key)))
      }
      await row.first().scrollIntoViewIfNeeded(); await paint(page); return row.first()
    }
    if (end) break
    end = await page.locator('#main-content-scroll').evaluate(scroll => { const before = scroll.scrollTop; scroll.scrollTop += Math.max(120, scroll.clientHeight); return before === scroll.scrollTop })
  }
  const geometry = await page.evaluate(() => ({ stamp: document.getElementById('observation-viewport')?.dataset.observationViewportContext,
    focus: document.activeElement?.id, claimed: document.getElementById('observation-viewport')?.dataset.observationFocusKey,
    top: document.getElementById('main-content-scroll')?.scrollTop,
    host: document.getElementById('main-content-scroll')?.getBoundingClientRect().toJSON(),
    mounted: [...document.querySelectorAll('[data-observation-key]')].map(row => ({ key: row.dataset.observationKey, position: row.dataset.observationPosition, bounds: row.getBoundingClientRect().toJSON() })) }))
  throw new Error('Observation logical target is not scroll-reachable: ' + JSON.stringify(geometry))
}
