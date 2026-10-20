import assert from 'node:assert/strict'
import test from 'node:test'
import { mkdir } from 'node:fs/promises'
import { join } from 'node:path'
import { openDiscovery } from './observation-discovery-fixture.mjs'
import { paint, scanObservationRows, revealObservationRow } from './observation-viewport-fixture.mjs'

const applied = h => h.page.locator('.observation-applied-filters').textContent()
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
for (const width of [1440, 320]) test(`production ${width} mode DOM visual and native Tab order preserves the Subject Tree default`, { timeout: 60000 }, async () => {
  const h = await openDiscovery({ width, height: 800 })
  try {
    await h.page.goto(h.origin + '/workspace/' + h.fixture.workspace.id + '#tab=observations'); await h.idle()
    if (width === 320) { await h.page.addStyleTag({ content: 'html { font-size: 200%; }' }); await paint(h.page) }
    const labels = ['Subject Tree', 'All Observations', 'Files']
    const buttons = h.page.getByRole('group', { name: 'Observation browse mode', exact: true }).getByRole('button')
    assert.deepEqual(await buttons.allTextContents(), labels)
    assert.deepEqual(await buttons.evaluateAll(elements => elements.map(element => element.getAttribute('aria-pressed'))), ['true', 'false', 'false'])
    assert.equal(await h.page.getByRole('button', { name: 'All', exact: true }).count(), 0)
    const boxes = await buttons.evaluateAll(elements => elements.map(element => element.getBoundingClientRect().toJSON()))
    for (let index = 1; index < boxes.length; index++) {
      const previous = boxes[index - 1], current = boxes[index]
      assert.ok(current.top > previous.top + 1 || (Math.abs(current.top - previous.top) <= 1 && current.left >= previous.right - 1), JSON.stringify(boxes))
    }
    await buttons.first().focus()
    for (let index = 0; index < labels.length; index++) {
      assert.equal(await h.page.locator(':focus').textContent(), labels[index])
      if (index + 1 < labels.length) await h.page.keyboard.press('Tab')
    }
    await h.page.keyboard.press('Shift+Tab')
    assert.equal(await h.page.locator(':focus').textContent(), 'All Observations')
    assert.deepEqual(await buttons.evaluateAll(elements => elements.map(element => element.getAttribute('aria-pressed'))), ['true', 'false', 'false'], 'Focus order retains Subject Tree')
    assert.ok(h.receipts.some(value => value.endpoint.endsWith('/subject-tree')))
    assert.equal(h.receipts.some(value => value.endpoint === '/api/v1/observations'), false, 'Fresh entry reads the Subject Tree')
    assert.deepEqual(h.errors, [])
  } finally { await h.close() }
})

test('production explicit Files mode and filter disclosures send live predicates; explicit Match sends normalized concrete paths and truthful subject evidence', { timeout: 60000 }, async () => {
  const h = await openDiscovery()
  try {
    await h.start()
    assert.equal(await h.page.getByRole('button', { name: 'All Observations', exact: true }).getAttribute('aria-pressed'), 'true')
    assert.equal(await h.page.getByRole('button', { name: 'Copy link', exact: true }).count(), 0)
    assert.equal(await h.page.locator('#observation-match-paths').isVisible(), false)
    assert.equal(await h.page.locator('#observation-subject-kind').isVisible(), false)
    const before = h.receipts.length
    await composer(h)
    assert.equal(await h.page.getByRole('button', { name: 'Files', exact: true }).getAttribute('aria-pressed'), 'true')
    assert.equal(await h.page.getByRole('button', { name: 'Files', exact: true }).getAttribute('aria-expanded'), 'true')
    assert.equal(await h.page.getByRole('button', { name: 'All Observations', exact: true }).getAttribute('aria-pressed'), 'false')
    assert.equal(h.receipts.length, before)
    await activate(h.page, h.page.locator('#observation-advanced-toggle'), 'Space')
    assert.equal(await h.page.locator('#observation-advanced-toggle').getAttribute('aria-expanded'), 'true')
    assert.equal(await h.page.locator('#observation-advanced-toggle').getAttribute('aria-controls'), 'observation-advanced-filters')
    await h.page.locator('#observation-git-sha').fill(h.sha)
    await activate(h.page, h.page.locator('#observation-advanced-toggle'), 'Space')
    await activate(h.page, h.page.locator('#observation-advanced-toggle'), 'Enter')
    assert.equal(await h.page.locator('#observation-git-sha').inputValue(), h.sha)
    assert.equal(h.receipts.length, before, 'Files without valid submitted paths does not issue a result read')
    await h.page.locator('#observation-query').fill('Cache evidence'); await h.page.locator('#observation-query').press('Enter'); await h.idle()
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
    assert.equal(await applied(h), matchedSummary); assert.ok(h.receipts.length >= before + 2)
  } finally { await h.close() }
})

test('production tree lazily pages a populated branch and exact typed terminals omit content search; invalid Files input preserves its last results', { timeout: 60000 }, async () => {
  const h = await openDiscovery(undefined, values => [...values, ...Array.from({ length: 201 }, (_, i) => ({ ...values[3], id: 'tree-extra-' + i, subjects: [{ subject_kind: 'file', subject: `docs/Extra-${String(i).padStart(3, '0')}.md` }] }))])
  const row = path => h.page.locator('[data-tree-path=' + JSON.stringify(path) + ']')
  const expand = async path => { await revealObservationRow(h.page, row(path)); await row(path).locator('.observation-tree-chevron').click(); await h.idle() }
  try {
    await h.start(); await h.page.getByRole('button', { name: 'Subject Tree', exact: true }).click(); await h.idle()
    assert.equal(await row('docs').getAttribute('aria-expanded'), 'false')
    await expand('docs')
    assert.ok(h.receipts.some(value => value.endpoint.endsWith('/subject-tree') && value.params.prefix === 'docs' && value.params.offset === '200'))
    assert.ok(await h.page.locator('[data-observation-key]').count() <= 28)
    await h.page.locator('#observation-query').fill('src'); await h.page.locator('#observation-query').press('Enter'); await h.idle()
    await expand('src'); await expand('src/**'); await expand('src/**/*.elm')
    const exact = h.receipts.filter(value => value.endpoint === '/api/v1/observations' && value.params.subject === 'src/**/*.elm').at(-1)
    assert.equal(exact.params.subject_kind, 'glob'); assert.equal(exact.params.query, undefined)
    assert.deepEqual(exact.response.items.map(value => value.id).sort(), ['cache-main', 'cache-view'])
    await h.page.locator('#observation-advanced-toggle').click(); await h.page.locator('#observation-subject-kind').selectOption('file'); await h.idle()
    assert.equal(await row('src/**').count(), 0)
    assert.ok(h.receipts.filter(value => value.endpoint.endsWith('/subject-tree')).at(-1).params.subject_kind === 'file')
    await composer(h); await h.page.locator('#observation-query').fill('Cache evidence'); await h.page.locator('#observation-query').press('Enter'); await h.idle()
    await h.page.locator('#observation-match-paths').fill('src/Main.elm'); await h.page.getByRole('button', { name: 'Match files', exact: true }).click(); await h.idle()
    await h.page.locator('.observation-subject-group-toggle').first().click(); await h.idle()
    const summary = await applied(h), count = h.receipts.length, cards = await h.page.locator('.observation-card').count()
    assert.ok(cards > 0)
    for (const invalid of ['src/**/*.elm', '']) {
      await h.page.locator('#observation-match-paths').fill(invalid)
      await h.page.getByRole('button', { name: 'Match files', exact: true }).click()
      await h.page.locator('#observation-file-composer .form-error').waitFor()
      assert.equal(h.receipts.length, count); assert.equal(await applied(h), summary)
      assert.equal(await h.page.locator('.observation-card').count(), cards)
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
    const selection = JSON.parse(await h.page.locator('#observation-panel').getAttribute('data-observation-context')).selectedId
    await composer(h); await h.page.locator('#observation-match-paths').fill('src/Main.elm')
    await activate(h.page, h.page.locator('#observation-advanced-toggle'), 'Space')
    await h.page.locator('#observation-git-sha').fill(h.sha)
    await activate(h.page, h.page.locator('#observation-advanced-toggle'), 'Space')
    await h.page.getByRole('button', { name: 'Close file composer', exact: true }).click()
    await h.page.waitForFunction(() => document.activeElement?.id === 'observation-for-files' && document.getElementById('observation-file-composer').hidden, null, { timeout: 5000 })
    await composer(h)
    assert.equal(await h.page.locator('#observation-match-paths').inputValue(), 'src/Main.elm')
    assert.equal(await h.page.locator('.observation-detail-content').textContent(), selectedContent)
    assert.equal(JSON.parse(await h.page.locator('#observation-panel').getAttribute('data-observation-context')).selectedId, selection)
    assert.equal(await h.page.locator('#observation-edit, #observation-edit-content').count(), 0)
  } finally { await h.close() }
})

test('production 320 CSS-pixel discovery controls remain reachable with enlarged text', { timeout: 60000 }, async () => {
  const h = await openDiscovery({ width: 320, height: 800 })
  try {
    await h.start()
    for (const label of ['Subject Tree', 'All Observations', 'Files']) await assertReachable(h.page, h.page.getByRole('button', { name: label, exact: true }))
    await assertReachable(h.page, h.page.locator('#observation-query'))
    await h.page.addStyleTag({ content: 'html { font-size: 200%; }' })
    await composer(h); await h.page.locator('#observation-match-paths').fill('src/Main.elm')
    await assertReachable(h.page, h.page.locator('#observation-match-paths'))
    await assertReachable(h.page, h.page.getByRole('button', { name: 'Match files', exact: true }))
    await h.page.getByRole('button', { name: 'Match files', exact: true }).click(); await h.idle()
    await activate(h.page, h.page.getByRole('button', { name: 'All Observations', exact: true }), 'Space'); await h.idle()
    assert.equal(await h.page.getByRole('button', { name: 'All Observations', exact: true }).evaluate(element => document.activeElement === element), true)
    assert.equal(h.receipts.at(-1).endpoint, '/api/v1/observations')
    assert.equal(await h.page.getByRole('button', { name: 'All Observations', exact: true }).getAttribute('aria-pressed'), 'true')
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


test('production original, current and historical SHA predicates keep one current row and restore independent URL selectors', { timeout: 60000 }, async () => {
  const current = 'b'.repeat(40), historical = 'c'.repeat(40)
  const h = await openDiscovery(undefined, values => values.map(value => value.id === 'cache-main' ? { ...value, latest_sequence: 3,
    current_provenance: { sequence: 3, event_kind: 'update', reviewed_git_sha: current, content_version: value.content_version, content_digest: 'a'.repeat(64), recorded_at: value.updated_at, actor_type: 'local', actor_id: null, actor_label: 'Fixture reviewer' },
    revision_history: [{ reviewed_git_sha: value.git_sha }, { reviewed_git_sha: historical }, { reviewed_git_sha: historical }, { reviewed_git_sha: current }] } : value))
  try {
    await h.start()
    await h.page.locator('#observation-advanced-toggle').click()
    await h.page.locator('#observation-git-sha').fill(h.sha)
    await h.page.locator('#observation-current-git-sha').fill(current)
    await h.page.locator('#observation-history-git-sha').fill(historical)
    await h.page.locator('#observation-query').press('Enter'); await h.idle()
    const receipt = h.receipts.filter(value => value.endpoint === '/api/v1/observations').at(-1)
    assert.equal(receipt.params.git_sha, h.sha); assert.equal(receipt.params.current_git_sha, current); assert.equal(receipt.params.history_git_sha, historical)
    assert.deepEqual(receipt.response.items.map(value => value.id), ['cache-main'])
    const counts = h.receipts.filter(value => value.endpoint.endsWith('/count')).at(-1)
    assert.equal(counts.payload.current_git_sha, current); assert.equal(counts.payload.history_git_sha, historical)
    const tuple = JSON.parse(new URLSearchParams(new URL(h.page.url()).hash.slice(1)).get('oq'))
    assert.equal(tuple[4], h.sha); assert.equal(tuple[8], current); assert.equal(tuple[9], historical)
    await h.page.locator('.observation-card').click(); await h.idle()
    assert.match(await h.page.locator('.observation-detail-content').innerText(), /Cache evidence for Main/)
    assert.match(await h.page.locator('.observation-provenance-match').innerText(), /recorded history claim/)
    assert.match(await h.page.locator('.observation-current-provenance').innerText(), new RegExp(current))
    const restoredUrl = h.page.url()
    await h.page.goto(restoredUrl); await h.page.locator('#observation-panel').waitFor(); await h.idle()
    const restored = h.receipts.filter(value => value.endpoint === '/api/v1/observations').at(-1)
    assert.equal(restored.params.current_git_sha, current); assert.equal(restored.params.history_git_sha, historical)
    assert.deepEqual(restored.response.items.map(value => value.id), ['cache-main'])
    await composer(h); await h.page.locator('#observation-match-paths').fill('src/Main.elm')
    await h.page.getByRole('button', { name: 'Match files', exact: true }).click(); await h.idle()
    const match = h.receipts.filter(value => value.endpoint.endsWith('/match')).at(-1)
    assert.equal(match.payload.current_git_sha, current); assert.equal(match.payload.history_git_sha, historical)
    assert.equal(match.response.items.length, 1); assert.equal(match.response.items[0].observation.current_provenance.reviewed_git_sha, current)
    for (const id of ['observation-git-sha', 'observation-current-git-sha', 'observation-history-git-sha']) {
      if (!(await h.page.locator('#' + id).isVisible())) await h.page.locator('#observation-advanced-toggle').click()
      await h.page.locator('#' + id).fill('')
    }
    await h.page.locator('#observation-query').press('Enter'); await h.idle()
    assert.equal(await h.page.locator('.observation-provenance-match').count(), 0, 'Cleared query must not revive prior history proof')
    const unfiltered = h.receipts.filter(value => value.endpoint.endsWith('/match')).at(-1)
    assert.equal('current_git_sha' in unfiltered.payload, false); assert.equal('history_git_sha' in unfiltered.payload, false)
    await h.page.locator('#observation-git-sha').fill(h.sha)
    await h.page.locator('#observation-query').press('Enter'); await h.idle()
    const replacedContext = await h.page.locator('.observation-provenance-match').innerText()
    assert.match(replacedContext, /original creation claim/); assert.doesNotMatch(replacedContext, /recorded history claim/)
  } finally { await h.close() }
})

for (const width of [1440, 320]) test(`production ${width} compact live filters have mode labels associated errors and distinct explorer controls`, {timeout: 60000}, async () => {
  const longPath = 'src/' + 'long-canonical-segment-'.repeat(10) + 'main.elm'
  const h = await openDiscovery({width, height: 1100}, values => values.map((value, i) => i ? value : {...value, subject: longPath, subjects: [{subject_kind: 'file', subject: longPath}, value.subjects[1]]}))
  const snapshot = async (name, target) => {
    if (!process.env.HMEM_HEADER_SCREENSHOT_DIR) return
    await mkdir(process.env.HMEM_HEADER_SCREENSHOT_DIR, {recursive: true})
    if (target) { await target.evaluate(element => element.scrollIntoView({block: 'center'})); await paint(h.page) }
    const image = width === 320 ? h.page.locator('#main-content-scroll') : h.page
    await image.screenshot({path: join(process.env.HMEM_HEADER_SCREENSHOT_DIR, `${name}-${width}.png`)})
  }
  const noOverflow = async () => assert.ok(await h.page.evaluate(() => document.documentElement.scrollWidth <= innerWidth + 1), 'Long paths and SHA controls fit the page')
  try {
    await h.start()
    assert.equal(await h.page.getByLabel('Search content', {exact: true}).count(), 1)
    assert.equal(await h.page.getByRole('button', {name: /^(Apply|Revert)$/}).count(), 0)
    assert.equal(await h.page.locator('#observation-panel dialog').count(), 0)
    assert.equal(await h.page.locator('.observation-filter-mode-help, .observation-applied-summary').count(), 0)
    await activate(h.page, h.page.locator('#observation-advanced-toggle'))
    for (const label of ['Subject kind', 'Exact subject', 'Creation Git SHA', 'Current reviewed Git SHA', 'Recorded history Git SHA']) assert.equal(await h.page.getByLabel(label, {exact: true}).count(), 1)
    await h.page.locator('#observation-subject-kind').selectOption('file'); await h.idle()
    await h.page.locator('#observation-git-sha').fill(h.sha); await h.idle()
    await activate(h.page, h.page.locator('#observation-advanced-toggle'))
    assert.match(await h.page.locator('#observation-advanced-toggle').innerText(), /2 active/)
    assert.equal(await h.page.locator('#observation-advanced-toggle').getAttribute('aria-expanded'), 'false')
    await h.page.locator('#observation-query').fill('Cache evidence'); await h.page.waitForTimeout(300); await h.idle()
    assert.equal(h.receipts.filter(value => value.endpoint === '/api/v1/observations').at(-1).params.query, 'Cache evidence')
    await activate(h.page, h.page.locator('#observation-advanced-toggle'))
    if (width === 320) await h.page.addStyleTag({content: 'html {font-size: 200%;}'})
    await noOverflow()
    if (width === 320) assert.ok(await h.page.locator('.observation-card-header .observation-subject').first().evaluate(element => element.getBoundingClientRect().width >= 80), 'Card subject retains readable width at enlarged text')
    await h.page.locator('#observation-mode-heading').scrollIntoViewIfNeeded(); await paint(h.page); await snapshot('advanced', h.page.locator('#observation-subject-kind'))
    if (width === 320) await snapshot('advanced-sha', h.page.locator('#observation-current-git-sha'))
    const before = h.receipts.length
    await h.page.locator('#observation-git-sha').fill('abc'); await h.page.waitForTimeout(300); await h.idle()
    assert.equal(h.receipts.length, before, 'Invalid populated SHA remains local')
    assert.equal(await h.page.locator('#observation-git-sha').getAttribute('aria-invalid'), 'true')
    assert.equal(await h.page.locator('#observation-git-sha').getAttribute('aria-describedby'), 'observation-filter-validation')
    assert.match(await h.page.locator('#observation-filter-validation').innerText(), /40-character Git SHA/)
    await activate(h.page, h.page.locator('#observation-advanced-toggle'))
    assert.match(await h.page.locator('#observation-advanced-toggle').innerText(), /Editing/)
    assert.equal(await h.page.locator('#observation-filter-validation').isVisible(), true)
    await activate(h.page, h.page.locator('#observation-advanced-toggle')); await h.page.locator('#observation-git-sha').fill(''); await h.idle()
    assert.equal(await h.page.locator('#observation-git-sha').getAttribute('aria-invalid'), 'false')
    assert.equal(await h.page.locator('#observation-filter-validation').count(), 0)
    assert.ok(h.receipts.length > before)
    await h.page.locator('#observation-subject-kind').selectOption(''); await h.idle()
    await activate(h.page, h.page.locator('#observation-advanced-toggle'))
    await h.page.getByRole('button', {name: 'Subject Tree', exact: true}).click(); await h.idle()
    assert.equal(await h.page.getByLabel('Search subjects', {exact: true}).count(), 1)
    assert.equal(await h.page.locator('#observation-subject').count(), 0)
    const row = path => h.page.locator('[data-tree-path=' + JSON.stringify(path) + ']')
    await revealObservationRow(h.page, row('src')); await row('src').locator('.observation-tree-chevron').click(); await h.idle()
    await revealObservationRow(h.page, row(longPath)); await row(longPath).locator('.observation-tree-label').click(); await paint(h.page)
    assert.ok(await row(longPath).evaluate(element => element.getBoundingClientRect().height <= document.getElementById('main-content-scroll').clientHeight), 'Long path keeps its disclosure and focus row reachable')
    assert.equal(await row(longPath).getAttribute('aria-selected'), 'true')
    assert.equal(await row(longPath).getAttribute('aria-expanded'), 'false')
    assert.equal(await row(longPath).locator('.observation-tree-chevron').getAttribute('tabindex'), '-1')
    assert.equal(await h.page.locator('.observation-detail').count(), 0)
    assert.equal(await row(longPath).locator('.observation-tree-copy').getAttribute('aria-label'), 'Copy canonical subject path: ' + longPath)
    await h.page.keyboard.press('ArrowRight'); await h.idle(); await paint(h.page)
    assert.equal(await row(longPath).getAttribute('aria-expanded'), 'true')
    assert.equal(await h.page.locator('.observation-detail').count(), 0)
    await noOverflow(); await snapshot('tree', row(longPath))
    await composer(h)
    assert.equal(await h.page.getByLabel('Search file matches', {exact: true}).count(), 1)
    assert.equal(await h.page.locator('#observation-subject').count(), 0)
  } finally { await h.close() }
})
