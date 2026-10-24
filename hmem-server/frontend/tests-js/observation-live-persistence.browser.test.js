import assert from 'node:assert/strict'
import test from 'node:test'
import { mkdir } from 'node:fs/promises'
import { join } from 'node:path'
import { openDiscovery, interceptDiscovery, discoverySelection } from './observation-discovery-fixture.mjs'
import { queryObservations } from '../perf/fixtures.mjs'
import { paint, revealObservationRow } from './observation-viewport-fixture.mjs'

const uuid = i => '00000000-0000-4000-8000-' + String(i + 1).padStart(12, '0')
const transform = values => values.map((value, i) => ({ ...value, id: uuid(i), updated_at: i === 0 ? '2026-10-07T00:00:00Z' : value.updated_at }))
let sequence = 0
const change = workspace => ({ schema_version: 1, type: 'change', event: { schema_version: 1, event_id: 'refresh-' + (++sequence), scope: 'workspace', workspace_id: workspace, occurred_at: '2026-10-07T00:00:00Z', transaction: { id: 'refresh-tx-' + sequence, cause: 'rest', request_id: null }, actor: { type: 'system', id: null }, entity: { type: 'observation', id: uuid(200), action: 'created' }, invalidations: [{ kind: 'collection', target: 'observations:' + workspace }] } })
const push = (h, count = 1) => h.page.evaluate(frames => window.pushHierarchyFrames(frames), Array.from({ length: count }, () => change(h.fixture.workspace.id)))
const owner = page => page.locator('#observation-panel').getAttribute('data-observation-context')
const stored = page => page.evaluate(() => Object.entries(localStorage).filter(([key]) => key.startsWith('hmem.observation-ui.v1:')))

test('same-pair preference hydration hands a held bootstrap to the current page with exactly one POST', {timeout:60000},async()=>{
  const h=await openDiscovery(undefined,transform)
  let bootstrap
  try {
    await h.start();await h.page.locator('.observation-subjects-toggle').first().click();await paint(h.page)
    await h.page.waitForFunction(()=>Object.values(localStorage).some(value=>{try{return JSON.parse(value).subjects?.includes('00000000-0000-4000-8000-000000000001')}catch{return false}}))
    assert.equal(JSON.parse((await stored(h.page))[0][1]).detail,null,'Hydration-only control has no saved selection')
    await h.page.addInitScript(()=>{
      const get=Storage.prototype.getItem,frame=window.requestAnimationFrame.bind(window)
      let pending=false
      Storage.prototype.getItem=function(key){const value=get.call(this,key);if(key.startsWith('hmem.observation-ui.v1:'))pending=true;return value}
      window.requestAnimationFrame=callback=>{if(pending){pending=false;window.releasePreferenceRead=()=>frame(callback);return -1}return frame(callback)}
    })
    const before=h.receipts.length
    bootstrap=h.gate(r=>r.payload?.view==='list'&&!r.payload.context_token,{hold:true})
    await h.page.goto('about:blank')
    await h.page.goto(h.origin+'/workspace/'+h.fixture.workspace.id+'#tab=observations&ov=1&oq='+encodeURIComponent(JSON.stringify(['flat','',null,'','',null,null,[],'','',''])))
    await h.bounded(bootstrap.arrived,5000,'One held physical bootstrap')
    await h.page.evaluate(()=>{
      window.handoffInjectedChanges=0
      for(const name of ['pushHierarchyFrames','pushHierarchyRawFrames','pushHierarchyGlobalFrames']){
        const original=window[name]
        window[name]=frames=>{window.handoffInjectedChanges+=frames.filter(frame=>frame.type==='change').length;return original(frames)}
      }
    })
    await h.page.waitForFunction(()=>typeof window.releasePreferenceRead==='function',null,{timeout:5000})
    await h.page.evaluate(()=>window.releasePreferenceRead());await paint(h.page)
    assert.equal(await h.page.locator('#observation-detail').count(),0,'No canonical selection can invalidate membership')
    bootstrap.release();await h.idle()
    const requests=h.receipts.slice(before).filter(r=>r.endpoint.endsWith('/discovery'))
    assert.equal(requests.length,1,JSON.stringify(requests.map(r=>r.payload)))
    assert.equal(h.receipts.slice(before).filter(r=>r.method==='GET'&&/\/observations\/[^/]+$/.test(r.endpoint)).length,0,'No canonical detail GET introduced a membership invalidation')
    assert.equal(await h.page.evaluate(()=>window.handoffInjectedChanges),0,'No injected live membership change')
    assert.equal(await h.page.locator('#observation-viewport').getAttribute('data-observation-loaded-count'),'50')
    assert.equal(await h.page.locator('.loading-indicator').count(),0)
    assert.equal(await h.page.locator('.observation-subjects-toggle').first().getAttribute('aria-expanded'),'true','Saved disclosure proves that hydration was accepted')
    assert.match(await h.page.locator('.workspace-header .workspace-observation-total').innerText(),/64 observations/)
  } finally {bootstrap?.release();await h.close()}
})

test('production card and independently ordered subjects survive live refresh and plain reload, and actor namespace isolates preferences', { timeout: 60000 }, async () => {
  const h = await openDiscovery(undefined, transform)
  try {
    await h.start()
    const card = h.page.locator('[data-observation-id=' + JSON.stringify(uuid(0)) + ']').first()
    await card.locator('.observation-subjects-toggle').focus(); await h.page.keyboard.press('Space'); await paint(h.page)
    assert.equal(await card.locator('.observation-subjects-toggle').getAttribute('aria-expanded'), 'true')
    assert.equal(await h.page.locator('#observation-detail').count(), 0)
    await card.locator('.observation-card').click(); await h.idle()
    const subjectToggle = card.locator('.observation-subjects-toggle')
    assert.equal(await subjectToggle.getAttribute('aria-expanded'), 'true')
    assert.deepEqual(await card.locator('.observation-detail-subject .copyable-value').allTextContents(), ['src/Main.elm', 'src/**/*.elm'])
    await h.page.waitForFunction(() => Object.values(localStorage).some(value => { try { return JSON.parse(value).subjects?.length === 1 } catch { return false } }))
    const prefs = JSON.parse((await stored(h.page))[0][1]); assert.equal(prefs.detail.id, uuid(0)); assert.ok(!JSON.stringify(prefs).includes('Cache evidence'))
    await subjectToggle.focus(); const before = await owner(h.page)
    await push(h); await h.idle(); await paint(h.page)
    assert.equal(await owner(h.page), before)
    assert.equal(await subjectToggle.getAttribute('aria-expanded'), 'true'); assert.equal(await card.locator('#observation-detail').count(), 1)
    assert.equal(await h.page.evaluate(() => document.activeElement.classList.contains('observation-subjects-toggle')), true)
    await h.page.addInitScript(() => {
      const getItem = Storage.prototype.getItem, frame = window.requestAnimationFrame.bind(window)
      let holdNextPreferenceFrame = false
      Storage.prototype.getItem = function (key) {
        const value = getItem.call(this, key)
        if (location.hash === '#tab=projects' && key.startsWith('hmem.observation-ui.v1:')) holdNextPreferenceFrame = true
        return value
      }
      window.requestAnimationFrame = callback => {
        if (holdNextPreferenceFrame) { holdNextPreferenceFrame = false; window.releasePreferenceRead = () => frame(callback); return -1 }
        return frame(callback)
      }
    })
    await h.page.goto(h.origin + '/workspace/' + h.fixture.workspace.id + '#tab=projects'); await h.page.reload()
    await h.page.waitForFunction(() => typeof window.releasePreferenceRead === 'function', null, { timeout: 5000 }); await h.idle()
    assert.equal(await h.page.locator('#observation-panel').count(), 0)
    await h.page.getByRole('button', { name: 'Observations', exact: true }).click()
    await h.page.waitForFunction(() => location.hash.includes('ov=1'), null, { timeout: 5000 })
    assert.equal(await h.page.locator('#observation-detail').count(), 0)
    await h.page.evaluate(() => window.releasePreferenceRead())
    await h.page.locator('#observation-detail .observation-subjects-toggle').waitFor(); await h.idle()
    assert.equal(await h.page.locator('[data-observation-detail-anchor]').getAttribute('id'), prefs.detail.occurrence)
    assert.equal(await h.page.locator('.observation-detail-content').textContent(), 'Cache evidence for Main')
    assert.equal(await h.page.locator('#observation-detail .observation-subjects-toggle').getAttribute('aria-expanded'), 'true')
    assert.equal(await h.page.locator('#observation-detail').count(), 1)
    await h.page.waitForFunction(id => new URLSearchParams(location.hash.slice(1)).get('observation') === id, uuid(0), { timeout: 5000 })
    await h.page.reload(); await h.page.locator('#observation-detail .observation-subjects-toggle').waitFor(); await h.idle()
    assert.equal(await h.page.locator('[data-observation-detail-anchor]').getAttribute('id'), prefs.detail.occurrence)
    assert.equal(await h.page.locator('#observation-detail .observation-subjects-toggle').getAttribute('aria-expanded'), 'true')
    await h.page.goto(h.origin + '/workspace/' + h.fixture.workspace.id + '#tab=observations'); await h.page.reload(); await h.page.locator('#observation-detail .observation-subjects-toggle').waitFor(); await h.idle().catch(async error => {
      const receipt = { phase: 'bare-link-reload', url: h.page.url(), loading: await h.page.locator('.loading-indicator').allTextContents(), requests: h.receipts.slice(-8).map(r => ({ payload: r.payload, status: r.status, done: r.done, counts: r.response?.page?.counts })), viewport: await h.page.locator('#observation-viewport').getAttribute('data-observation-context') }
      throw new Error(error.message + ' ' + JSON.stringify(receipt))
    })
    assert.equal(await h.page.locator('#observation-detail .observation-subjects-toggle').getAttribute('aria-expanded'), 'true')
    assert.equal(await h.page.locator('.observation-detail-content').textContent(), 'Cache evidence for Main')
    assert.equal(await h.page.locator('#observation-edit-content').count(), 0)
    if (process.env.HMEM_INLINE_SCREENSHOT_DIR) {
      await mkdir(process.env.HMEM_INLINE_SCREENSHOT_DIR, { recursive: true })
      for (const width of [1440, 320]) {
        await h.page.setViewportSize({ width, height: 900 })
        await h.page.locator('[data-observation-detail-anchor]').evaluate(element => element.scrollIntoView({ block: 'center' })); await paint(h.page)
        assert.ok(await h.page.evaluate(() => document.documentElement.scrollWidth <= innerWidth + 1))
        await h.page.screenshot({ path: join(process.env.HMEM_INLINE_SCREENSHOT_DIR, width === 1440 ? 'observation-live-persistence-desktop.png' : 'observation-live-persistence-narrow.png') })
      }
    }
    h.setPrincipal('different-actor'); await h.page.reload(); await h.page.locator('#observation-panel').waitFor(); await h.idle()
    assert.equal(await h.page.locator('#observation-detail').count(), 0)
    assert.equal((await stored(h.page)).length, 2)
  } finally { await h.close() }
})

test('production automatic refresh stages loaded span, coalesces bursts, keeps disclosures and cache, and explicit Retry is fresh', { timeout: 60000 }, async () => {
  const h = await openDiscovery(undefined, values => transform([...values, ...Array.from({ length: 201 }, (_, i) => ({ ...values[3], id: 'live-extra-' + i, subjects: [{ subject_kind: 'file', subject: `docs/Extra-${i}.md` }] }))]))
  let release
  try {
    await h.start()
    const card = h.page.locator('[data-observation-id=' + JSON.stringify(uuid(0)) + ']').first()
    await card.locator('.observation-card').click(); await h.idle()
    while(Number(await h.page.locator('[data-observation-loaded-count]').getAttribute('data-observation-loaded-count'))<265){await h.page.getByRole('button',{name:'Load more',exact:true}).click();await h.idle()}
    await card.locator('.observation-subjects-toggle').click(); await paint(h.page)
    await card.locator('.observation-detail-sha .copyable-value').focus()
    const before = await owner(h.page); const requests = []; let hold = true, fail = false
    await interceptDiscovery(h,async(route,outcome)=>{
      const payload=route.request().postDataJSON();requests.push(payload)
      if(hold){hold=false;await new Promise(resolve=>{release=resolve})}
      if(fail){fail=false;return route.fulfill({status:503,json:{error:'Controlled staged failure'}})}
      await route.fallback()
    })
    await push(h); await h.page.waitForFunction(() => true); const deadline = Date.now() + 5000
    while (!release && Date.now() < deadline) await h.page.waitForTimeout(20)
    assert.ok(release, 'exact first automatic request arrived')
    assert.equal(await owner(h.page), before); assert.equal(await card.locator('#observation-detail').count(), 1)
    await push(h, 4); await paint(h.page); assert.equal(requests.length, 1)
    release(); release = null; await h.idle(); await paint(h.page)
    assert.deepEqual(requests.map(value => value.offset), [0,0,50,100,150,200,250],JSON.stringify(requests.map(value=>({view:value.view,offset:value.offset,token:value.context_token,generation:value.generation}))))
    assert.ok(requests.every(value => !discoverySelection(h,value).content_query)); assert.equal(await card.locator('.observation-subjects-toggle').getAttribute('aria-expanded'), 'true')
    assert.equal(await owner(h.page), before); assert.equal(await h.page.locator('[data-observation-loaded-count]').getAttribute('data-observation-loaded-count'), '265')
    assert.equal(await h.page.evaluate(() => document.activeElement.closest('.observation-detail-sha') !== null), true)
    assert.equal(await h.page.getByText('Results may have changed.', { exact: true }).count(), 0)
    fail = true; await push(h); await h.page.getByText('Automatic refresh failed.', { exact: false }).waitFor(); await h.idle()
    const afterFailure = requests.length; await paint(h.page); assert.equal(requests.length, afterFailure)
    assert.equal(await card.locator('#observation-detail').count(), 1)
    await h.page.getByRole('button', { name: 'Retry refresh', exact: true }).click(); await h.idle()
    assert.deepEqual(requests.slice(afterFailure).map(value => value.offset), [0,50,100,150,200,250])
  } finally { release?.(); await h.close() }
})

test('production ordered match group and subject disclosure stay independent across refresh and applied-query reload', { timeout: 60000 }, async () => {
  const h = await openDiscovery(undefined, transform)
  try {
    await h.start(); await h.page.getByRole('button', { name: 'Files', exact: true }).click()
    await h.page.locator('#observation-match-paths').fill('src/Main.elm'); await h.page.getByRole('button', { name: 'Match files', exact: true }).click(); await h.idle()
    const group = h.page.locator('.observation-subject-group-toggle').first(); await group.click(); await paint(h.page)
    await h.page.locator('.observation-subject-group-toggle').nth(1).click(); await paint(h.page)
    const card = h.page.locator('[data-observation-id=' + JSON.stringify(uuid(0)) + ']').last(); await card.locator('.observation-card').click(); await h.idle()
    const selectedOccurrence = await card.locator('.observation-card').getAttribute('id')
    await card.locator('.observation-subjects-toggle').click(); await paint(h.page)
    const location = h.page.url(); await push(h); await h.idle()
    assert.equal(await group.getAttribute('aria-expanded'), 'true'); assert.equal(await card.locator('.observation-subjects-toggle').getAttribute('aria-expanded'), 'true')
    await h.page.reload(); await h.page.locator('#observation-detail').waitFor(); await h.idle()
    assert.equal(h.page.url(), location); assert.equal(await h.page.locator('.observation-subject-group-toggle').first().getAttribute('aria-expanded'), 'true')
    assert.equal(await h.page.locator('[data-observation-detail-anchor]').getAttribute('id'), selectedOccurrence)
    assert.equal(await h.page.locator('#observation-detail .observation-subjects-toggle').getAttribute('aria-expanded'), 'true')
    assert.equal(await h.page.locator('#observation-detail').count(), 1)
  } finally { await h.close() }
})


test('hidden invalidation retires a held visible staging pass and reopening fetches a fresh catalog', {timeout:60000},async()=>{
  const h=await openDiscovery(undefined,values=>transform([...values,...Array.from({length:201},(_,i)=>({...values[3],id:'hidden-'+i}))]))
  try {
    await h.start();await h.page.getByRole('button',{name:'Load more',exact:true}).click();await h.idle()
    const held=h.gate(r=>r.payload?.view==='list'&&r.payload.offset===50,{hold:true})
    await push(h);await h.bounded(held.arrived,5000,'Visible staged second page held')
    await h.page.getByRole('button',{name:'Projects',exact:true}).click();await paint(h.page)
    const hiddenAt=h.receipts.length
    h.fixture.observations=h.fixture.observations.filter(value=>value.id!==uuid(0))
    await push(h);await paint(h.page)
    held.release();await h.idle()
    assert.equal(h.receipts.slice(hiddenAt).some(r=>r.payload?.view==='list'),false,'Hidden completion cannot start membership work')
    assert.match(await h.page.locator('.workspace-header .workspace-observation-total').innerText(),/264 observations/)
    await h.page.getByRole('button',{name:'Observations',exact:true}).click();await h.idle()
    assert.equal(await h.page.locator('[data-observation-id='+JSON.stringify(uuid(0))+']').count(),0)
    assert.equal(await h.page.locator('#observation-viewport').getAttribute('data-observation-loaded-count'),'100')
    assert.ok(h.receipts.slice(hiddenAt).some(r=>r.payload?.view==='list'&&r.payload.offset===0&&r.done),'Reopening admits a fresh membership pass')
  } finally {await h.close()}
})
