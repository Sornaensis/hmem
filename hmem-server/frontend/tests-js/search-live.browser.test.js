import assert from 'node:assert/strict'
import test from 'node:test'
import { hierarchyFixture, openHierarchy } from './hierarchy-fixture.mjs'

const enterSearch = async h => { if (!await h.page.locator('.search-input').count()) await h.page.getByRole('button', { name: 'Search', exact: true }).click() }
const empty = { projects: [], tasks: [], observations: [] }
const input = h => h.page.getByRole('searchbox', { name: 'Search all entities', exact: true })
const bar = h => h.page.getByRole('group', { name: 'Search Entities', exact: true })
const toggle = (h, label) => bar(h).getByRole('button', { name: label, exact: true })
const waitFor = async (h, predicate) => {
  const deadline = Date.now() + 5000
  while (!predicate() && Date.now() < deadline) await h.page.waitForTimeout(20)
  assert.ok(predicate(), 'Expected controlled request before deadline')
}
const respond = (route, body, status = 200) => route.fulfill({ status, contentType: 'application/json', body: JSON.stringify(body) })

test('clearing a global query keeps entity options visible and restores saved choices on the next query', { timeout: 30000 }, async () => {
  const h = await openHierarchy(), requests = []
  await h.page.route('**/api/v1/search', route => { requests.push(route.request().postDataJSON()); return respond(route, empty) })
  try {
    await h.start(); await h.idle()
    await enterSearch(h); await input(h).fill('first'); await h.page.locator('.search-results-title').waitFor()
    await toggle(h, 'Tasks').click(); await waitFor(h, () => requests.length === 2)
    await input(h).focus(); await h.page.keyboard.press('ControlOrMeta+A'); await h.page.keyboard.press('Backspace')
    await h.page.waitForFunction(() => document.querySelector('.search-input')?.value === '')
    assert.equal(await bar(h).count(), 1, 'Keyboard deletion keeps entity options visible')
    const count = requests.length
    await enterSearch(h); await input(h).fill('   '); await h.page.waitForTimeout(300)
    assert.equal(await bar(h).count(), 1, 'Whitespace is an empty query')
    assert.equal(requests.length, count)
    await enterSearch(h); await input(h).fill('second'); await h.page.locator('.search-results-title').waitFor()
    assert.equal(await toggle(h, 'Tasks').getAttribute('aria-pressed'), 'false')
    assert.deepEqual(requests.at(-1).entity_types, ['project', 'observation'])
    await h.page.getByRole('button', { name: 'Clear all entities search', exact: true }).click()
    await h.page.waitForFunction(() => document.querySelector('.search-input')?.value === '')
    assert.equal(await bar(h).count(), 1, 'Explicit clear keeps entity options visible')
    assert.equal(await h.page.getByPlaceholder('Filter project hierarchy...').count(), 0)
    assert.deepEqual(h.errors, []); assert.deepEqual(h.unhandled, [])
  } finally { await h.close() }
})

for (const focused of [false, true]) test((focused ? 'Focused' : 'Normal') + ' hierarchy controls stay visible in their content section and retain native input identity', { timeout: 30000 }, async () => {
  const fixture = hierarchyFixture(), h = await openHierarchy(fixture)
  try {
    await h.start(focused ? 'tab=projects&focus=project:' + fixture.projects[0].id : ''); await h.idle()
    const hierarchy = h.page.getByPlaceholder(focused ? 'Filter focused view...' : 'Filter project hierarchy...')
    assert.equal(await hierarchy.evaluate(element => !!element.closest('.hierarchy-content-section')), true)
    assert.equal(await h.page.locator('.search-input, .search-section-toggle').count(), 0)
    for (const width of [1440, 390]) {
      await h.page.setViewportSize({ width, height: 800 })
      const field = await hierarchy.elementHandle()
      await hierarchy.fill(' '); await h.idle(); await hierarchy.press('Backspace'); await h.idle()
      assert.equal(await field.evaluate(element => element.isConnected && element === document.activeElement), true)
      assert.equal(await h.page.getByRole('button', { name: 'Expand All', exact: true }).count(), 1)
      assert.equal(await h.page.getByRole('button', { name: 'Collapse All', exact: true }).count(), 1)
      const controls = await h.page.locator('.project-filter-panel button, .project-filter-panel select').evaluateAll(elements => elements.map(element => { const r=element.getBoundingClientRect(), section=element.closest('.project-filter-panel').getBoundingClientRect(); return { visible: getComputedStyle(element).display !== 'none' && r.width > 0 && r.height > 0, fits:r.left>=section.left-1&&r.right<=section.right+1 } }))
      assert.ok(controls.length >= 15 && controls.every(control=>control.visible&&control.fits), JSON.stringify({width,controls}))
    }
    assert.deepEqual(h.errors, []); assert.deepEqual(h.unhandled, [])
  } finally { await h.close() }
})

test('global input position and focus stay stable on the first character and keyboard clear', { timeout: 30000 }, async () => {
  const fixture = hierarchyFixture()
  fixture.projects = fixture.projects.slice(0, 10).map(project => ({ ...project, parent_id: null }))
  fixture.projects[0].name = '000 root'; fixture.tasks = []
  const h = await openHierarchy(fixture)
  await h.page.route('**/api/v1/search', route => respond(route, empty))
  try {
    await h.start(); await h.idle(); await enterSearch(h)
    for (const width of [1440, 390]) for (const top of [0]) {
      await h.page.setViewportSize({ width, height: 700 })
      await h.page.locator('#main-content-scroll').evaluate((host, top) => { host.scrollTop = top }, top)
      await h.page.waitForTimeout(300)
      await input(h).click()
      const element = await input(h).elementHandle()
      const measure = () => element.evaluate(element => {
        const r = element.getBoundingClientRect(), host = document.getElementById('main-content-scroll'), bounds = document.getElementById('main-content-scroll').getBoundingClientRect()
        return { top: r.top, bottom: r.bottom, scroll: host.scrollTop, focused: element === document.activeElement, connected: element.isConnected, visible: r.top >= bounds.top && r.bottom <= bounds.bottom }
      })
      const before = await measure()
      assert.equal(before.visible, true, JSON.stringify({ width, top, before }))
      assert.ok(Math.abs(before.scroll - top) <= 1, 'Input is usable at the requested scroll position')
      await h.page.keyboard.type('z'); await h.page.waitForTimeout(400); await h.idle()
      const typed = await measure()
      await h.page.keyboard.press('ControlOrMeta+A'); await h.page.keyboard.press('Backspace'); await h.idle()
      const cleared = await measure()
      for (const after of [typed, cleared]) {
        assert.ok(after.focused && after.connected && after.visible, JSON.stringify({ width, top, before, after }))
        assert.ok(Math.abs(after.top - before.top) <= 1 && Math.abs(after.bottom - before.bottom) <= 1 && Math.abs(after.scroll - before.scroll) <= 1, JSON.stringify({ width, top, before, after }))
      }
    }
    assert.deepEqual(h.errors, []); assert.deepEqual(h.unhandled, [])
  } finally { await h.close() }
})

test('global search is live and independent with persistent actor preferences through pending/error/empty/clear', { timeout: 60000 }, async () => {
  const fixture = hierarchyFixture(), h = await openHierarchy(fixture), requests = []
  let releasePending, retry = false
  const pending = new Promise(resolve => { releasePending = resolve })
  await h.page.route('**/api/v1/search', async route => {
    const body = route.request().postDataJSON(); requests.push(body)
    if (body.query === 'pending' && !retry) { await pending; return respond(route, { error: 'Controlled failure' }, 500) }
    return respond(route, body.query === 'alpha' ? { ...empty, projects: fixture.projects } : empty)
  })
  try {
    await h.start(); await h.idle()
    await h.page.getByPlaceholder('Filter project hierarchy...').fill('root'); await h.idle()
    const hierarchyState = await h.page.evaluate(id => JSON.parse(localStorage.getItem('hmem-ws-' + id)), fixture.workspace.id)
    await enterSearch(h); await input(h).fill('alpha')
    await h.page.waitForTimeout(100); assert.equal(requests.length, 0, 'Typing must debounce')
    await h.page.locator('.search-results-title').waitFor()
    assert.equal(requests.length, 1); assert.deepEqual(requests[0].entity_types, ['project', 'task', 'observation'])
    assert.equal(await toggle(h, 'Tasks').getAttribute('aria-pressed'), 'true')
    await toggle(h, 'Tasks').click()
    await waitFor(h, () => requests.length === 2)
    assert.deepEqual(requests[1].entity_types, ['project', 'observation'])
    await h.page.waitForFunction(() => !document.querySelector('.search-results-state'))
    await h.page.setViewportSize({ width: 390, height: 700 })
    await h.page.locator('#main-content-scroll').evaluate(element => { element.scrollTop = 700 })
    await h.page.waitForTimeout(150)
    assert.equal(await input(h).evaluate(element => !!element.closest('.sticky-workspace-bar')), false)
    await enterSearch(h); await input(h).fill('pending')
    await waitFor(h, () => requests.length === 3)
    assert.match(await h.page.locator('.search-results-state').innerText(), /Searching/)
    assert.match(await h.page.locator('.search-previous-results .search-results-title').innerText(), /alpha/)
    assert.equal(await bar(h).count(), 1)
    releasePending()
    await h.page.getByRole('button', { name: 'Retry search', exact: true }).waitFor()
    assert.match(await h.page.locator('.search-previous-results .search-results-title').innerText(), /alpha/)
    assert.equal(await bar(h).count(), 1)
    retry = true
    await h.page.getByRole('button', { name: 'Retry search', exact: true }).click()
    await h.page.getByText('No results found', { exact: true }).waitFor()
    assert.equal(await bar(h).count(), 1)
    await h.page.getByRole('button', { name: 'Clear all entities search', exact: true }).click()
    await h.page.waitForFunction(() => document.querySelector('.search-input')?.value === '')
    assert.equal(await input(h).inputValue(), '')
    assert.equal(await h.page.getByPlaceholder('Filter project hierarchy...').count(), 0)
    assert.equal(await bar(h).count(), 1)
    assert.deepEqual(await h.page.evaluate(id => JSON.parse(localStorage.getItem('hmem-ws-' + id)), fixture.workspace.id), hierarchyState)
    await h.page.setViewportSize({ width: 1440, height: 1000 })
    await h.page.reload(); await h.page.locator('.workspace-tabs').waitFor(); await h.idle()
    await enterSearch(h); await input(h).fill('restored'); await h.page.locator('.search-results-title').waitFor()
    assert.equal(await toggle(h, 'Tasks').getAttribute('aria-pressed'), 'false')
    assert.deepEqual(requests.at(-1).entity_types, ['project', 'observation'])
    await toggle(h, 'Projects').click(); await waitFor(h, () => requests.at(-1).entity_types.join() === 'observation')
    await toggle(h, 'Observations').click()
    const count = requests.length
    await h.page.waitForTimeout(400)
    assert.equal(requests.length, count, 'Explicit none must not become a default-all request')
    assert.match(await bar(h).innerText(), /Select at least one/)
    await h.page.getByRole('button', { name: 'Clear all entities search', exact: true }).click()
    await h.page.waitForFunction(() => document.querySelector('.search-input')?.value === '')
    assert.equal(await bar(h).count(), 1)
    await h.page.waitForTimeout(300); assert.equal(requests.length, count, 'Empty query must not search')
    await enterSearch(h); await input(h).fill('none selected')
    await h.page.waitForTimeout(300); assert.equal(requests.length, count, 'The next query preserves explicit none')
    await toggle(h, 'Tasks').click()
    await waitFor(h, () => requests.length === count + 1)
    h.setPrincipal('other-user')
    await h.page.reload(); await h.page.locator('.workspace-tabs').waitFor(); await h.idle()
    await enterSearch(h); await input(h).fill('other actor'); await h.page.locator('.search-results-title').waitFor()
    assert.deepEqual(requests.at(-1).entity_types, ['project', 'task', 'observation'])
    h.setPrincipal('fixture-user')
    await h.page.reload(); await h.page.locator('.workspace-tabs').waitFor(); await h.idle()
    await enterSearch(h); await input(h).fill('first actor again'); await h.page.locator('.search-results-title').waitFor()
    assert.deepEqual(requests.at(-1).entity_types, ['task'], 'Each actor retains independent choices')
    assert.deepEqual(h.errors, []); assert.deepEqual(h.unhandled, [])
  } finally { releasePending(); await h.close() }
})

test('composition suppresses intermediate searches; late query and type replies cannot replace the current display', { timeout: 60000 }, async () => {
  const fixture = hierarchyFixture(), h = await openHierarchy(fixture), requests = [], held = []
  await h.page.route('**/api/v1/search', async route => {
    const body = route.request().postDataJSON(); requests.push(body)
    if (body.query === 'old' || (body.query === 'types' && body.entity_types.includes('task'))) {
      await new Promise(resolve => held.push(resolve))
    }
    return respond(route, { ...empty, projects: [{ ...fixture.projects[0], name: body.query + ':' + body.entity_types.join(',') }] })
  })
  try {
    await h.start(); await h.idle()
    await enterSearch(h); await input(h).dispatchEvent('compositionstart')
    await enterSearch(h); await input(h).fill('intermediate'); await h.page.waitForTimeout(350)
    assert.equal(requests.length, 0)
    await enterSearch(h); await input(h).fill('complete'); await input(h).dispatchEvent('compositionend')
    await h.page.locator('.search-results-title').waitFor()
    assert.equal(requests.length, 1); assert.equal(requests[0].query, 'complete')
    await enterSearch(h); await input(h).fill('old'); await waitFor(h, () => requests.some(row => row.query === 'old'))
    await enterSearch(h); await input(h).fill('new')
    await h.page.locator('.search-results-title').filter({ hasText: 'new' }).waitFor()
    held.shift()(); await h.page.waitForTimeout(150)
    assert.match(await h.page.locator('.search-result-title').innerText(), /^new:/)
    await enterSearch(h); await input(h).fill('types'); await waitFor(h, () => requests.some(row => row.query === 'types'))
    await toggle(h, 'Tasks').click()
    await h.page.locator('.search-result-title').filter({ hasText: 'types:project,observation' }).waitFor()
    held.shift()(); await h.page.waitForTimeout(150)
    assert.equal(await h.page.locator('.search-result-title').innerText(), 'types:project,observation')
    const beforeLive = requests.length
    await h.page.evaluate(frame => window.pushHierarchyFrames([frame]), { schema_version: 1, type: 'change', event: {
      schema_version: 1, event_id: 'entity-search-refresh', scope: 'workspace', workspace_id: fixture.workspace.id, occurred_at: '2026-10-20T00:00:00Z',
      transaction: { id: 'entity-search-refresh', cause: 'rest', request_id: null }, actor: { type: 'system', id: null },
      entity: { type: 'task', id: fixture.tasks[0].id, action: 'updated' }, invalidations: [{ kind: 'search', target: 'workspace:' + fixture.workspace.id }]
    } })
    await waitFor(h, () => requests.length > beforeLive)
    assert.equal(requests.at(-1).query, 'types'); assert.deepEqual(requests.at(-1).entity_types, ['project', 'observation'])
    await h.page.waitForFunction(() => !document.querySelector('.search-results-state'))
    await enterSearch(h); await input(h).fill('old'); await waitFor(h, () => held.length === 1)
    await enterSearch(h); await input(h).fill('')
    held.shift()(); await h.page.waitForTimeout(350)
    assert.equal(await h.page.locator('.unified-search-results').count(), 0)
    assert.equal(await bar(h).count(), 1)
    assert.deepEqual(h.errors, []); assert.deepEqual(h.unhandled, [])
  } finally { for (const release of held) release(); await h.close() }
})

test('planning workspaces omit Observation controls and default to their supported types', { timeout: 30000 }, async () => {
  const fixture = hierarchyFixture(); fixture.workspace.workspace_type = 'planning'
  const h = await openHierarchy(fixture), requests = []
  await h.page.route('**/api/v1/search', route => { requests.push(route.request().postDataJSON()); return respond(route, empty) })
  try {
    await h.start(); await h.idle()
    await enterSearch(h); await input(h).fill('planning'); await h.page.locator('.search-results-title').waitFor()
    assert.deepEqual(requests[0].entity_types, ['project', 'task'])
    assert.equal(await toggle(h, 'Observations').count(), 0)
    assert.deepEqual(h.errors, []); assert.deepEqual(h.unhandled, [])
  } finally { await h.close() }
})


test('global and hierarchy inputs fit their separate desktop and narrow sections', { timeout: 30000 }, async () => {
  const h = await openHierarchy()
  try {
    await h.start(); await h.idle()
    for (const width of [1440,390]) for(const label of ['Projects','Search']) {
      await h.page.setViewportSize({width,height:800})
      await h.page.locator('.workspace-tabs').getByRole('button',{name:label,exact:true}).click()
      const fields = h.page.locator('.search-input, .hierarchy-search-input')
      assert.equal(await fields.count(),1)
      assert.ok(await fields.evaluateAll(elements=>elements.every(element=>{const r=element.getBoundingClientRect(),p=element.parentElement.getBoundingClientRect();return r.width>0&&r.left>=p.left-1&&r.right<=p.right+1})))
    }
    assert.deepEqual(h.errors,[]);assert.deepEqual(h.unhandled,[])
  } finally {await h.close()}
})
