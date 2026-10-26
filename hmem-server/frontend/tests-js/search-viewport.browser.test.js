import assert from 'node:assert/strict'
import test from 'node:test'
import { openDiscovery } from './observation-discovery-fixture.mjs'
import { hierarchyFixture, openHierarchy } from './hierarchy-fixture.mjs'

const frames = page => page.evaluate(() => new Promise(resolve => { let count = 0; const tick = () => ++count === 8 ? resolve() : requestAnimationFrame(tick); requestAnimationFrame(tick) }))
async function observe(page) {
  return page.evaluate(() => {
    const host = document.getElementById('main-content-scroll')
    const samples = { initial: host.scrollTop, values: [], running: true }
    window.searchViewportSamples = samples
    const tick = () => { samples.values.push(host.scrollTop); if (samples.running) requestAnimationFrame(tick) }
    tick(); return samples.initial
  })
}
async function stationary(page, initial) {
  await frames(page)
  const values = await page.evaluate(() => { window.searchViewportSamples.running = false; return window.searchViewportSamples.values })
  assert.ok(values.length >= 2)
  assert.ok(values.every(top => Math.abs(top - initial) <= 1), JSON.stringify({ initial, min: Math.min(...values), max: Math.max(...values), final: values.at(-1) }))
}
async function focusAtScroll(page, input) {
  await input.click()
  await page.locator('#main-content-scroll').hover(); await page.mouse.wheel(0, -100000); await page.waitForTimeout(250)
  const distance = await input.evaluate(e => { const host = document.getElementById('main-content-scroll'); return host.contains(e) ? Math.min(120, Math.floor(e.getBoundingClientRect().top - host.getBoundingClientRect().top - 12 - 4)) : 120 })
  assert.ok(distance >= 90, 'Native wheel setup retains a meaningful physical position')
  await page.mouse.wheel(0, distance); await page.waitForTimeout(300); await frames(page)
  assert.equal(await input.evaluate(e => e === document.activeElement), true)
  const geometry = await input.evaluate(e => { const r = e.getBoundingClientRect(), host = document.getElementById('main-content-scroll'), h = (host.contains(e) ? host : e.closest('.workspace-context-persistent')).getBoundingClientRect(); return { visible: r.top >= h.top && r.bottom <= h.bottom, input: r.toJSON(), host: h.toJSON(), top: host.scrollTop } })
  assert.ok(geometry.visible, 'Input is visible and focused before the first real key: ' + JSON.stringify(geometry))
  const initial = await observe(page)
  assert.ok(initial >= 90, 'A nonzero physical position makes restoration observable')
  return initial
}
const populated = values => [...values, ...Array.from({ length: 100 }, (_, i) => ({ ...values[3], id: 'wide-' + i, subjects: [{ subject_kind: 'file', subject: `wide-${i}/Guide.md` }] }))]
const searchFixture = () => {
  const fixture = hierarchyFixture()
  fixture.projects = fixture.projects.slice(0, 10).map(value => ({ ...value, parent_id: null }))
  fixture.tasks = []
  return fixture
}

for (const mode of ['All Observations', 'Subject Tree']) for (const field of ['Subjects', 'Search observations']) {
  test(`${mode} ${field}: real edits and keyboard deletion keep every frame stationary`, { timeout: 60000 }, async () => {
    const h = await openDiscovery(undefined, populated)
    try {
      await h.start(); await h.page.getByRole('button', { name: mode, exact: true }).click(); await h.idle()
      const input = h.page.getByRole('searchbox', { name: field, exact: true })
      let initial = await focusAtScroll(h.page, input)
      await h.page.keyboard.type(field === 'Subjects' ? 'src' : 'Cache', { delay: 35 }); await h.idle(); await stationary(h.page, initial)
      initial = await observe(h.page)
      await h.page.keyboard.press('ControlOrMeta+A'); await h.page.keyboard.press('Backspace'); await h.idle(); await stationary(h.page, initial)
      assert.equal(await input.inputValue(), '')
    } finally { await h.close() }
  })
}

test('delayed and stale search replies preserve newer user wheel intent while the input remains focused', { timeout: 60000 }, async () => {
  const h = await openDiscovery(undefined, populated)
  try {
    await h.start()
    const input = h.page.getByRole('searchbox', { name: 'Search observations', exact: true })
    let initial = await focusAtScroll(h.page, input)
    const held = h.gate(r => r.payload?.filters?.content_query === 'Cache', { hold: true })
    await h.page.keyboard.type('Cache', { delay: 25 }); await h.bounded(held.arrived, 5000, 'Held search'); await stationary(h.page, initial)
    await h.page.mouse.wheel(0, -80); await h.page.waitForTimeout(250)
    const moved = await h.page.locator('#main-content-scroll').evaluate(e => e.scrollTop)
    assert.ok(moved < initial - 50, 'The focused search never locks deliberate scrolling')
    initial = await observe(h.page)
    await h.page.keyboard.press('ControlOrMeta+A'); await h.page.keyboard.type('Documentation', { delay: 20 })
    await h.page.waitForTimeout(600); held.release(); await h.idle(); await stationary(h.page, initial)
    assert.equal(await input.evaluate(e => e === document.activeElement), true)
  } finally { await h.close() }
})

test('composition and ordinary empty edits are stationary while the explicit clear control still returns to top', { timeout: 60000 }, async () => {
  const h = await openDiscovery(undefined, populated)
  try {
    await h.start()
    const input = h.page.getByRole('searchbox', { name: 'Search observations', exact: true })
    const initial = await focusAtScroll(h.page, input)
    await input.evaluate(e => { e.dispatchEvent(new CompositionEvent('compositionstart', { bubbles: true })); e.value = 'Cache'; e.dispatchEvent(new InputEvent('input', { bubbles: true, isComposing: true, inputType: 'insertCompositionText', data: 'Cache' })); e.dispatchEvent(new CompositionEvent('compositionend', { bubbles: true, data: 'Cache' })) })
    await h.idle(); await stationary(h.page, initial)
    const transferred = await observe(h.page)
    await h.page.keyboard.press('Shift+Tab')
    assert.equal(await h.page.getByRole('searchbox', { name: 'Subjects', exact: true }).evaluate(e => e === document.activeElement), true)
    await stationary(h.page, transferred)
    await h.page.keyboard.press('Tab')
    await h.page.getByRole('button', { name: 'Clear Search observations', exact: true }).click(); await h.idle(); await frames(h.page)
    assert.equal(await h.page.locator('#main-content-scroll').evaluate(e => e.scrollTop), 0)
  } finally { await h.close() }
})

test('search geometry releases on owner retirement and keeps only same-page query history', { timeout: 60000 }, async () => {
  const h = await openHierarchy(searchFixture())
  try {
    await h.page.route('**/api/v1/search', route => route.fulfill({ status: 200, contentType: 'application/json', body: JSON.stringify({ projects: [], tasks: [], observations: [] }) }))
    await h.page.goto(h.origin + '/workspace/' + h.fixture.workspace.id); await h.idle()
    const input = h.page.getByRole('searchbox', { name: 'Search all entities', exact: true })
    const element = await input.elementHandle()
    const leased = () => h.page.locator('#main-content-scroll').getAttribute('data-search-viewport-editing')
    const acquire = async () => { await input.focus(); await h.page.keyboard.type('z'); await frames(h.page); assert.equal(await leased(), 'true') }
    const released = async () => {
      await frames(h.page); assert.equal(await leased(), null)
      assert.equal(await h.page.locator('#main-content-scroll > .page').evaluate(e => e.style.minHeight), '')
      assert.equal(await h.page.locator('#main-content-scroll').evaluate(e => e.style.overflowAnchor), '')
    }
    await acquire()
    await h.page.evaluate(() => history.replaceState(null, '', location.pathname + '#tab=projects&query-only=changed'))
    await frames(h.page); assert.equal(await leased(), 'true')
    await input.evaluate(e => e.blur()); await released()
    for (const kind of ['hidden', 'disabled', 'inert', 'window-blur', 'canceled-pointer', 'history']) {
      await acquire()
      await input.evaluate((e, kind) => {
        if (kind === 'hidden') e.style.visibility = 'hidden'
        if (kind === 'disabled') e.disabled = true
        if (kind === 'inert') e.parentElement.inert = true
        if (kind === 'window-blur') window.dispatchEvent(new Event('blur'))
        if (kind === 'canceled-pointer') { document.body.dispatchEvent(new PointerEvent('pointerdown', { bubbles: true })); e.blur(); document.body.dispatchEvent(new PointerEvent('pointercancel', { bubbles: true })) }
        if (kind === 'history') window.dispatchEvent(new PopStateEvent('popstate'))
      }, kind)
      await released()
      await element.evaluate(e => { e.style.visibility = ''; e.disabled = false; e.parentElement.inert = false })
    }
  } finally { await h.close() }
})

for (const kind of ['global', 'hierarchy']) test(`${kind} search preserves physical position through short results and empty keyboard edits`, { timeout: 60000 }, async () => {
  const h = await openHierarchy(searchFixture())
  try {
    await h.page.route('**/api/v1/search', route => route.fulfill({ status: 200, contentType: 'application/json', body: JSON.stringify({ projects: [], tasks: [], observations: [] }) }))
    await h.page.goto(h.origin + '/workspace/' + h.fixture.workspace.id); await h.idle()
    const input = kind === 'global' ? h.page.getByRole('searchbox', { name: 'Search all entities', exact: true }) : h.page.locator('.workspace-search-filter-bar .hierarchy-search-input')
    let initial = await focusAtScroll(h.page, input)
    await h.page.keyboard.type('zzzznotfound', { delay: 25 }); await h.idle(); await h.page.waitForTimeout(350); await stationary(h.page, initial)
    initial = await observe(h.page)
    await h.page.keyboard.press('ControlOrMeta+A'); await h.page.keyboard.press('Delete'); await h.idle(); await stationary(h.page, initial)
  } finally { await h.close() }
})
