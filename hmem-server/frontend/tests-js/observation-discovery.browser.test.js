import assert from 'node:assert/strict'
import test from 'node:test'
import { mkdir } from 'node:fs/promises'
import { join } from 'node:path'
import { openDiscovery } from './observation-discovery-fixture.mjs'
import { paint, scanObservationRows, revealObservationRow } from './observation-viewport-fixture.mjs'

const applied = h => h.page.locator('.observation-applied-filters .observation-applied-summary').textContent()
async function activate(page, button, key = 'Enter') {
  const expanded = await button.getAttribute('aria-expanded'), id = await button.getAttribute('id')
  await button.focus(); await page.keyboard.press(key)
  if (expanded !== null) await page.waitForFunction(({ id, expected }) => document.getElementById(id)?.getAttribute('aria-expanded') === expected, { id, expected: id === 'observation-for-files' || expanded !== 'true' ? 'true' : 'false' }, { timeout: 5000 })
}
async function composer(h) {
  await activate(h.page, h.page.getByRole('button', { name: 'Files', exact: true }))
  await h.page.waitForFunction(() => document.activeElement?.id === 'observation-match-paths', null, { timeout: 5000 })
}
async function assertReachable(page, target) {
  await target.scrollIntoViewIfNeeded()
  const geometry = await target.evaluate(element => {
    const rect = element.getBoundingClientRect(), main = document.getElementById('main-content-scroll').getBoundingClientRect()
    const top = Math.max(rect.top, main.top, 0), bottom = Math.min(rect.bottom, main.bottom, innerHeight)
    const x = Math.max(1, Math.min(innerWidth - 1, rect.left + rect.width / 2)), y = top + (bottom - top) / 2
    return { visible: bottom > top, unobscured: element.contains(document.elementFromPoint(x, y)), width: rect.width }
  })
  assert.ok(geometry.visible && geometry.unobscured && geometry.width > 0, JSON.stringify(geometry))
}
test('production discovery disclosures are presentation only; explicit Match sends normalized concrete paths and truthful subject evidence', { timeout: 60000 }, async () => {
  const h = await openDiscovery()
  try {
    await h.start()
    if (process.env.HMEM_HEADER_SCREENSHOT_DIR) {
      await mkdir(process.env.HMEM_HEADER_SCREENSHOT_DIR, { recursive: true })
      for (const width of [1440, 320]) {
        await h.page.setViewportSize({ width, height: 900 }); await paint(h.page)
        await h.page.locator('#main-content-scroll').hover()
        await h.page.mouse.wheel(0, -5000); await paint(h.page)
        await h.page.locator('#observation-mode-heading').scrollIntoViewIfNeeded(); await paint(h.page)
        const geometry = await h.page.locator('.observation-mode-navigation').evaluate(element => {
          const header = element.getBoundingClientRect(), scroll = document.getElementById('main-content-scroll').getBoundingClientRect()
          return { visible: header.top >= scroll.top && header.bottom <= scroll.bottom, overflow: document.documentElement.scrollWidth - innerWidth }
        })
        assert.ok(geometry.visible && geometry.overflow <= 1, JSON.stringify(geometry))
        await h.page.screenshot({ path: join(process.env.HMEM_HEADER_SCREENSHOT_DIR, width === 1440 ? 'observation-header-desktop.png' : 'observation-header-narrow.png') })
      }
      await h.page.setViewportSize({ width: 1440, height: 900 }); await paint(h.page)
    }
    assert.equal(await h.page.getByRole('button', { name: 'All', exact: true }).getAttribute('aria-pressed'), 'true')
    assert.equal(await h.page.getByRole('button', { name: 'Copy link', exact: true }).count(), 0)
    assert.equal(await h.page.locator('#observation-match-paths').isVisible(), false)
    assert.equal(await h.page.locator('#observation-subject-kind').isVisible(), false)
    const summary = await applied(h), before = h.receipts.length, genericBefore = h.requests.length
    await composer(h)
    assert.equal(await h.page.getByRole('button', { name: 'Files', exact: true }).getAttribute('aria-pressed'), 'false')
    assert.equal(await h.page.getByRole('button', { name: 'Files', exact: true }).getAttribute('aria-expanded'), 'true')
    assert.equal(await h.page.getByRole('button', { name: 'All', exact: true }).getAttribute('aria-pressed'), 'true')
    assert.equal(h.receipts.length, before); assert.equal(await applied(h), summary)
    await activate(h.page, h.page.locator('#observation-advanced-toggle'), 'Space')
    assert.equal(await h.page.locator('#observation-advanced-toggle').getAttribute('aria-expanded'), 'true')
    assert.equal(await h.page.locator('#observation-advanced-toggle').getAttribute('aria-controls'), 'observation-advanced-filters')
    await h.page.locator('#observation-git-sha').fill(h.sha)
    await activate(h.page, h.page.locator('#observation-advanced-toggle'), 'Space')
    await activate(h.page, h.page.locator('#observation-advanced-toggle'), 'Enter')
    assert.equal(await h.page.locator('#observation-git-sha').inputValue(), h.sha)
    assert.equal(h.receipts.length, before); assert.equal(h.requests.length, genericBefore)
    await h.page.locator('#observation-query').fill('Cache evidence')
    await h.page.locator('#observation-match-paths').fill('  src/Main.elm  \nsrc/View.elm\nsrc/Main.elm\n')
    await h.page.getByRole('button', { name: 'Match files', exact: true }).click(); await h.idle()
    const matches = h.receipts.filter(value => value.endpoint.endsWith('/match'))
    assert.equal(await h.page.getByRole('button', { name: 'Files', exact: true }).getAttribute('aria-pressed'), 'true')
    assert.equal(matches.length, 1)
    assert.deepEqual(matches[0].payload.paths, ['src/Main.elm', 'src/View.elm'])
    assert.equal(matches[0].payload.query, 'Cache evidence'); assert.equal(matches[0].payload.git_sha, h.sha)
    assert.deepEqual(matches[0].response.items.map(value => value.observation.id).sort(), ['cache-main', 'cache-view'])
    const view = matches[0].response.items.find(value => value.observation.id === 'cache-view')
    assert.deepEqual(view.path_matches.find(value => value.path === 'src/Main.elm').matched_subjects, [{ subject_kind: 'glob', subject: 'src/**/*.elm' }])
    assert.deepEqual(await h.page.locator('.observation-path-heading').allTextContents(), ['src/Main.elm', 'src/View.elm'])
    assert.equal(await h.page.locator('.observation-subject-group-toggle').count(), 4)
    assert.equal(await h.page.locator('.observation-mode-announcement').textContent(), '2 Observations match')
    const matchedSummary = await applied(h)
    const closeContext = JSON.parse(await h.page.locator('#observation-panel').getAttribute('data-observation-context'))
    assert.ok(closeContext.sessionEpoch > 0, 'Close transfer uses the current authenticated session, including before any card activation')
    await h.page.getByRole('button', { name: 'Close file composer', exact: true }).click()
    await h.page.waitForFunction(() => document.activeElement?.id === 'observation-for-files')
    assert.equal(await h.page.getByRole('button', { name: 'Files', exact: true }).getAttribute('aria-pressed'), 'true')
    assert.equal(await h.page.getByRole('button', { name: 'Files', exact: true }).getAttribute('aria-expanded'), 'false')
    assert.deepEqual(JSON.parse(await h.page.locator('#observation-panel').getAttribute('data-observation-context')), closeContext)
    console.log(JSON.stringify({ fixture: 'Observation standalone Close focus', context: closeContext, focus: await h.page.evaluate(() => document.activeElement?.id) }))
    assert.equal(await applied(h), matchedSummary); assert.equal(h.receipts.length, before + 2)
  } finally { await h.close() }
})

test('production By subject paginates populated facets and locks exact provenance; invalid file input preserves results', { timeout: 60000 }, async () => {
  const h = await openDiscovery(undefined, values => [...values, ...Array.from({ length: 201 }, (_, i) => ({ ...values[3], id: 'facet-extra-' + i, subjects: [{ subject_kind: 'file', subject: `docs/Extra-${i}.md` }] }))])
  try {
    await h.start(); await h.page.getByRole('button', { name: 'Subject', exact: true }).click(); await h.idle()
    assert.equal(await h.page.getByRole('button', { name: 'Subject', exact: true }).getAttribute('aria-pressed'), 'true')
    assert.equal((await scanObservationRows(h.page)).facets.size, 200)
    await h.page.getByRole('button', { name: 'Load more subjects', exact: true }).click(); await h.idle()
    assert.ok((await scanObservationRows(h.page)).facets.size > 200)
    assert.equal(h.receipts.filter(value => value.endpoint.endsWith('/subject-facets')).at(-1).params.offset, '200')
    const facet = h.page.locator('.observation-facet').filter({ hasText: 'src/**/*.elm' }).locator('.observation-facet-card')
    await revealObservationRow(h.page, facet); await facet.click(); await h.idle()
    assert.equal(await h.page.getByRole('button', { name: 'Subject', exact: true }).getAttribute('aria-pressed'), 'true')
    await h.page.locator('#observation-advanced-toggle').click()
    assert.match(await h.page.locator('.observation-selected-facet-value').textContent(), /Glob: src\/\*\*\/\*\.elm/)
    assert.equal(await h.page.locator('#observation-subject').count(), 0)
    await h.page.locator('#observation-subject-kind').selectOption('file')
    await h.page.locator('#observation-query').fill('Cache evidence')
    await h.page.getByRole('button', { name: 'Apply filters', exact: true }).click(); await h.idle()
    const exact = h.receipts.filter(value => value.endpoint === '/api/v1/observations').at(-1)
    assert.equal(exact.params.subject_kind, 'glob'); assert.equal(exact.params.subject, 'src/**/*.elm')
    assert.deepEqual(exact.response.items.map(value => value.id).sort(), ['cache-main', 'cache-view'])
    const summary = await applied(h), count = h.receipts.length
    await composer(h)
    for (const invalid of ['src/**/*.elm', '']) {
      await h.page.locator('#observation-match-paths').fill(invalid)
      await h.page.getByRole('button', { name: 'Match files', exact: true }).click()
      await h.page.locator('#observation-file-composer .form-error').waitFor()
      assert.equal(h.receipts.length, count); assert.equal(await applied(h), summary)
      assert.equal(await h.page.locator('.observation-card').count(), 2)
    }
  } finally { await h.close() }
})

test('production native search submission and discovery controls preserve selected read-only content', { timeout: 60000 }, async () => {
  const h = await openDiscovery()
  try {
    await h.start()
    const initialCount = h.receipts.length
    await h.page.locator('#observation-query').fill('Cache evidence'); await h.page.locator('#observation-query').press('Enter'); await h.idle()
    assert.equal(h.receipts.length, initialCount + 2)
    assert.equal(h.receipts.filter(value => value.endpoint === '/api/v1/observations').at(-1).params.query, 'Cache evidence')
    assert.equal(h.receipts.filter(value => value.endpoint.endsWith('/count')).at(-1).payload.query, 'Cache evidence')
    assert.equal(await h.page.locator('.observation-card').count(), 2)
    await h.page.locator('.observation-card').first().click(); await h.idle()
    assert.equal(await h.page.locator('#observation-edit, #observation-edit-content').count(), 0)
    const selectedContent = await h.page.locator('.observation-detail-content').textContent()
    const stamp = await h.page.locator('#observation-panel').getAttribute('data-observation-context'), summary = await applied(h), count = h.receipts.length
    await composer(h); await h.page.locator('#observation-match-paths').fill('src/Main.elm')
    await activate(h.page, h.page.locator('#observation-advanced-toggle'), 'Space')
    await h.page.locator('#observation-git-sha').fill(h.sha)
    await activate(h.page, h.page.locator('#observation-advanced-toggle'), 'Space')
    await h.page.getByRole('button', { name: 'Close file composer', exact: true }).click()
    await h.page.waitForFunction(() => document.activeElement?.id === 'observation-for-files' && document.getElementById('observation-file-composer').hidden, null, { timeout: 5000 })
    await composer(h)
    assert.equal(await h.page.locator('#observation-match-paths').inputValue(), 'src/Main.elm')
    assert.equal(await h.page.locator('.observation-detail-content').textContent(), selectedContent)
    assert.equal(await h.page.locator('#observation-panel').getAttribute('data-observation-context'), stamp)
    assert.equal(await applied(h), summary); assert.equal(h.receipts.length, count)
  } finally { await h.close() }
})

test('production 320 CSS-pixel discovery controls remain reachable with enlarged text', { timeout: 60000 }, async () => {
  const h = await openDiscovery({ width: 320, height: 800 })
  try {
    await h.start()
    for (const label of ['All', 'Subject', 'Files']) await assertReachable(h.page, h.page.getByRole('button', { name: label, exact: true }))
    await assertReachable(h.page, h.page.locator('#observation-query'))
    await h.page.addStyleTag({ content: 'html { font-size: 200%; }' })
    await composer(h); await h.page.locator('#observation-match-paths').fill('src/Main.elm')
    await assertReachable(h.page, h.page.locator('#observation-match-paths'))
    await assertReachable(h.page, h.page.getByRole('button', { name: 'Match files', exact: true }))
    await h.page.getByRole('button', { name: 'Match files', exact: true }).click(); await h.idle()
    await activate(h.page, h.page.getByRole('button', { name: 'All', exact: true }), 'Space'); await h.idle()
    await h.page.waitForFunction(() => document.activeElement?.id === 'observation-mode-heading', null, { timeout: 5000 })
    assert.equal(h.receipts.at(-1).endpoint, '/api/v1/observations')
    assert.equal(await h.page.getByRole('button', { name: 'All', exact: true }).getAttribute('aria-pressed'), 'true')
    await composer(h)
    await h.page.evaluate(() => {
      const records = []; window.observationToolbarEvents = records
      for (const type of ['focusin', 'keydown', 'click']) document.addEventListener(type, event => {
        if (records.length < 24 && event.target?.id?.startsWith('observation-')) records.push({ type, id: event.target.id, key: event.key || null, context: document.getElementById('observation-panel')?.dataset.observationContext })
      }, true)
    })
    await h.page.getByRole('button', { name: 'Close file composer', exact: true }).click()
    await activate(h.page, h.page.locator('#observation-advanced-toggle'), 'Space')
    assert.equal(await h.page.locator('#observation-git-sha').isVisible(), true)
    await assertReachable(h.page, h.page.locator('#observation-git-sha'))
    const layout = await h.page.evaluate(() => ({ width: innerWidth, text: getComputedStyle(document.documentElement).fontSize, overflow: document.documentElement.scrollWidth - innerWidth, main: document.getElementById('main-content-scroll').clientHeight }))
    assert.equal(layout.width, 320); assert.equal(layout.text, '32px'); assert.ok(layout.main > 0); assert.ok(layout.overflow <= 1, JSON.stringify(layout))
    const receipt = await h.page.evaluate(() => ({ events: window.observationToolbarEvents, focus: document.activeElement?.id, expanded: document.getElementById('observation-advanced-toggle')?.getAttribute('aria-expanded') }))
    assert.ok(receipt.events.some(event => event.type === 'focusin' && event.id === 'observation-advanced-toggle'))
    assert.ok(receipt.events.some(event => event.type === 'keydown' && event.id === 'observation-advanced-toggle' && event.key === ' '))
    assert.equal(receipt.focus, 'observation-advanced-toggle'); assert.equal(receipt.expanded, 'true')
    console.log(JSON.stringify({ fixture: 'Observation toolbar native supersession', ...receipt }))
  } finally { await h.close() }
})
