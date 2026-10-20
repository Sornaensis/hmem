import assert from 'node:assert/strict'
import test from 'node:test'
import { mkdir, stat } from 'node:fs/promises'
import { resolve } from 'node:path'
import { openDiscovery } from './observation-discovery-fixture.mjs'
import { revealObservationRow } from './observation-viewport-fixture.mjs'

const creates = h => h.receipts.filter(r => r.endpoint.endsWith('/discovery') && !r.payload.context_token)
const waitCreates = (h, count) => h.bounded((async () => { while (creates(h).length < count) await h.page.waitForTimeout(20) })(), 5000, 'Live discovery create')
const logicalCount = h => h.page.locator('#observation-viewport').getAttribute('data-observation-logical-count')

test('paired live filters use one keyed hybrid bootstrap and token-only typed continuations', { timeout: 60000 }, async () => {
  const h = await openDiscovery()
  try {
    h.discovery.configure({ mode: 'hybrid', semantic: (query, value) => query === 'Cache' && value.id === 'other-js' })
    await h.start()
    assert.equal(creates(h).length, 1)
    assert.equal(h.receipts.some(r => r.endpoint.endsWith('/count')), false, 'Bootstrap owns counts')
    await h.page.locator('#observation-subject-query').fill('src/')
    await h.page.locator('#observation-query').fill('Cache')
    await waitCreates(h, 2); await h.idle()
    const request = creates(h).at(-1)
    assert.equal(request.payload.filters.subject_query, 'src/')
    assert.equal(request.payload.filters.content_query, 'Cache')
    assert.deepEqual(request.response.page.items.map(r => r.id).sort(), ['cache-main', 'cache-view', 'other-js'])
    assert.equal(request.response.page.counts.match_count, 3)
    assert.equal(await h.page.getByText('Hybrid content search', { exact: true }).count(), 1)
    await h.page.getByRole('button', { name: 'Subject Tree', exact: true }).click(); await h.idle()
    const tree = creates(h).at(-1)
    assert.equal(tree.payload.view, 'tree')
    assert.equal(tree.payload.filters.subject_query, '')
    await h.page.locator('#observation-subject-query').fill('src/')
    await h.page.locator('#observation-query').fill('Cache')
    await waitCreates(h, creates(h).length + 1); await h.idle()
    const root = creates(h).at(-1)
    assert.equal(root.response.page.counts.match_count, 3)
    assert.deepEqual(root.response.page.tree.items.map(r => r.path), ['src'])
    await h.page.locator('[data-tree-path="src"] .observation-tree-chevron').click(); await h.idle()
    const continuation = h.receipts.filter(r => r.payload?.context_token).at(-1)
    assert.equal(continuation.payload.context_token, root.response.context_token)
    assert.deepEqual(Object.keys(continuation.payload).sort(), ['context_token','limit','offset','prefix','view','workspace_id'])
    assert.equal(h.discovery.maximumContexts, 1)
    assert.ok(Number(await logicalCount(h)) > 0)
  } finally { await h.close() }
})

test('tokenless pending bootstrap is superseded by a higher generation before its old success returns', { timeout: 60000 }, async () => {
  const h = await openDiscovery()
  try {
    await h.start()
    const before = creates(h).length
    const held = h.gate(r => r.payload?.filters?.content_query === 'Cache', { hold: true })
    await h.page.locator('#observation-query').fill('Cache')
    await h.bounded(held.arrived, 5000, 'Held tokenless create')
    await h.page.locator('#observation-query').fill('Documentation')
    await waitCreates(h, before + 2)
    const old = creates(h).at(-2), current = creates(h).at(-1)
    assert.equal(old.payload.search_session, current.payload.search_session)
    assert.ok(current.payload.generation > old.payload.generation)
    assert.equal(h.discovery.maximumContexts, 1)
    held.release(); await h.idle()
    assert.equal(await h.page.locator('#observation-query').inputValue(), 'Documentation')
    assert.equal(await h.page.getByText('Full-text search · vector search disabled', { exact: true }).count(), 1)
    assert.equal(current.response.page.counts.match_count, 61)
    assert.equal(await h.page.locator('.observation-result').filter({ hasText: 'Cache evidence' }).count(), 0)
  } finally { await h.close() }
})

for (const mode of ['All Observations', 'Subject Tree']) for (const field of ['subjects', 'body']) test(`clearing ${field} in ${mode} retains the other predicate and defeats late scroll/response`, { timeout: 60000 }, async () => {
  const h = await openDiscovery()
  try {
    await h.start()
    if (mode === 'Subject Tree') { await h.page.getByRole('button', { name: mode, exact: true }).click(); await h.idle() }
    await h.page.locator('#observation-subject-query').fill('docs/')
    await h.page.locator('#observation-query').fill('Documentation')
    await waitCreates(h, creates(h).length + 1); await h.idle()
    if (mode === 'Subject Tree') { await h.page.locator('[data-tree-path="docs"] .observation-tree-chevron').click(); await h.idle() }
    const held = h.gate(r => r.payload?.filters?.[field === 'subjects' ? 'subject_query' : 'content_query'] === (field === 'subjects' ? 'docs/G' : 'Documentation guide'), { hold: true })
    const input = h.page.locator(field === 'subjects' ? '#observation-subject-query' : '#observation-query')
    await input.fill(field === 'subjects' ? 'docs/G' : 'Documentation guide')
    await h.bounded(held.arrived, 5000, 'Old filtered response held')
    await input.evaluate(element => element.focus({ preventScroll: true }))
    const before = await h.page.locator('#main-content-scroll').evaluate(element => { element.scrollTop = 650; return element.scrollTop })
    assert.ok(before > 100, 'Completed rows retain a scrollable viewport while pending: ' + JSON.stringify(await h.page.evaluate(() => ({ top: document.getElementById('main-content-scroll').scrollTop, height: document.getElementById('main-content-scroll').scrollHeight, logical: document.getElementById('observation-viewport')?.dataset.observationLogicalCount, rows: document.querySelectorAll('[data-observation-key]').length, status: document.querySelector('.observation-applied-filters')?.textContent, errors: [...document.querySelectorAll('.observation-state-error,.observation-refresh-error')].map(e=>e.textContent) }))))
    if (field === 'subjects') await h.page.getByRole('button', { name: 'Clear Subjects', exact: true }).evaluate(element => element.click())
    else await h.page.getByRole('button', { name: 'Clear Search observations', exact: true }).evaluate(element => element.click())
    await h.page.waitForFunction(() => document.getElementById('main-content-scroll').scrollTop === 0)
    assert.equal(await input.inputValue(), '')
    assert.equal(await h.page.locator(field === 'subjects' ? '#observation-query' : '#observation-subject-query').inputValue(), field === 'subjects' ? 'Documentation' : 'docs/')
    held.release(); await h.idle()
    await h.page.locator('#main-content-scroll').evaluate(element => { element.scrollTop = 320; element.dispatchEvent(new Event('scroll')) })
    await h.page.waitForFunction(() => document.getElementById('main-content-scroll').scrollTop === 0)
    const current = creates(h).at(-1)
    assert.equal(current.payload.filters.subject_query, field === 'subjects' ? '' : 'docs/')
    assert.equal(current.payload.filters.content_query, field === 'body' ? '' : 'Documentation')
    assert.deepEqual(h.errors, [])
  } finally { await h.close() }
})


test('typed terminals share the bootstrap context and delayed leaves remain owned by their terminal', { timeout: 60000 }, async () => {
  const h = await openDiscovery()
  try {
    await h.start(); await h.page.getByRole('button', {name:'Subject Tree',exact:true}).click(); await h.idle()
    await h.page.locator('#observation-subject-query').fill('src/'); await h.page.locator('#observation-query').fill('Cache'); await waitCreates(h, creates(h).length+1); await h.idle()
    const count = creates(h).length, root = creates(h).at(-1)
    await h.page.locator('[data-tree-path="src"] .observation-tree-chevron').click(); await h.idle()
    const held = h.gate(r=>r.payload?.terminal?.subject === 'src/Main.elm', {hold:true})
    await h.page.locator('[data-tree-path="src/Main.elm"] .observation-tree-chevron').click()
    await h.bounded(held.arrived,5000,'Held first typed terminal')
    await h.page.locator('[data-tree-path="src/View.elm"] .observation-tree-chevron').click()
    await h.page.locator('[data-tree-path="src/View.elm"] .observation-tree-label').click()
    held.release(); await h.idle()
    assert.equal(creates(h).length,count,'Typed presentation does not create a new context')
    const leaves = h.receipts.filter(r=>r.payload?.terminal)
    assert.deepEqual(leaves.map(r=>r.payload.terminal.subject).sort(),['src/Main.elm','src/View.elm'])
    for(const leaf of leaves) { assert.equal(leaf.payload.context_token,root.response.context_token); assert.equal(leaf.payload.filters,undefined); assert.deepEqual(leaf.response.page.items.map(r=>r.subject),[leaf.payload.terminal.subject]) }
    assert.equal(await h.page.locator('[data-tree-path="src/View.elm"]').getAttribute('aria-selected'),'true')
    assert.equal(await h.page.locator('.observation-result').filter({hasText:'Cache evidence for Main'}).count(),1)
    assert.equal(await h.page.locator('.observation-result').filter({hasText:'Cache evidence for View'}).count(),1)
  } finally { await h.close() }
})

test('clearing body in a selected typed terminal retires its old page and saved scroll while keeping Subjects', {timeout:60000},async()=>{
  const h=await openDiscovery(undefined,values=>[...values,...Array.from({length:80},(_,i)=>({...values[0],id:'selected-guide-'+i,subjects:[{subject_kind:'file',subject:'docs/000-selected.md'}],subject_kind:'file',subject:'docs/000-selected.md',content:'Documentation selected guide '+i}))])
  let held
  try {
    await h.start();await h.page.getByRole('button',{name:'Subject Tree',exact:true}).click();await h.idle()
    await h.page.locator('#observation-subject-query').fill('docs/');await h.page.locator('#observation-query').fill('Documentation');await h.idle()
    await h.page.locator('[data-tree-path="docs"] .observation-tree-chevron').click();await h.idle()
    const leaf=h.page.locator('[data-tree-path="docs/000-selected.md"]')
    await leaf.locator('.observation-tree-chevron').click();await h.idle();await leaf.locator('.observation-tree-label').click();await h.idle()
    assert.equal(await leaf.getAttribute('aria-selected'),'true')
    held=h.gate(r=>r.payload?.terminal?.subject==='docs/000-selected.md'&&r.payload.offset===50,{hold:true})
    const more=h.page.getByRole('button',{name:'Load more observations',exact:true})
    await revealObservationRow(h.page,more);await more.click()
    await h.bounded(held.arrived,5000,'Selected old terminal continuation')
    await h.page.locator('#observation-query').evaluate(input=>input.focus({preventScroll:true}))
    assert.ok(await h.page.locator('#main-content-scroll').evaluate(scroll=>{scroll.scrollTop=650;return scroll.scrollTop})>100)
    await h.page.getByRole('button',{name:'Clear Search observations',exact:true}).evaluate(button=>button.click())
    await h.page.waitForFunction(()=>document.getElementById('main-content-scroll').scrollTop===0)
    held.release();await h.idle()
    assert.equal(await h.page.locator('#observation-subject-query').inputValue(),'docs/')
    assert.equal(await h.page.locator('#observation-query').inputValue(),'')
    assert.equal(creates(h).at(-1).payload.filters.subject_query,'docs/')
    assert.equal(creates(h).at(-1).payload.filters.content_query,'')
    await h.page.locator('#main-content-scroll').evaluate(scroll=>{scroll.scrollTop=350;scroll.dispatchEvent(new Event('scroll'))})
    await h.page.waitForFunction(()=>document.getElementById('main-content-scroll').scrollTop===0)
    assert.deepEqual(h.errors,[])
  } finally {held?.release();await h.close()}
})

test('composition sends no intermediate create and capacity retry keeps the last completed result', {timeout:60000}, async()=>{
  const h=await openDiscovery()
  try {
    await h.start(); await h.page.locator('#observation-query').fill('Cache'); await waitCreates(h,2); await h.idle()
    const before=creates(h).length, input=h.page.locator('#observation-query')
    await input.dispatchEvent('compositionstart'); await input.fill('Documentation'); await h.page.waitForTimeout(350)
    assert.equal(creates(h).length,before)
    const busy=h.gate(r=>r.payload?.filters?.content_query==='Documentation',{error:429})
    await input.dispatchEvent('compositionend',{data:'Documentation'}); await h.bounded(busy.arrived,5000,'Capacity response'); await h.idle()
    assert.equal(await h.page.locator('.observation-result').filter({hasText:'Cache evidence'}).count(),2)
    assert.ok(!(await h.page.locator('.observation-mode-navigation').innerText()).includes('Counting'))
    assert.ok(!(await h.page.locator('.workspace-header .workspace-observation-total').innerText()).includes('Updating'))
    await h.page.getByRole('button',{name:'Retry search',exact:true}).click(); await h.idle()
    const old=creates(h).at(-2),latest=creates(h).at(-1)
    assert.ok(latest.payload.generation>old.payload.generation)
    assert.equal(latest.response.page.counts.match_count,61)
    assert.equal(await h.page.locator('.observation-result').filter({hasText:'Cache evidence'}).count(),0)
  } finally { await h.close() }
})


test('concurrent expired branches coalesce one fresh bootstrap and reject later expired callbacks', {timeout:60000}, async()=>{
  const h=await openDiscovery()
  try {
    await h.start(); await h.page.getByRole('button',{name:'Subject Tree',exact:true}).click(); await h.idle()
    const root=creates(h).at(-1),before=creates(h).length
    h.discovery.expire(root.response.context_token)
    const first=h.gate(r=>r.payload?.context_token===root.response.context_token&&r.payload?.prefix==='src',{hold:true})
    const second=h.gate(r=>r.payload?.context_token===root.response.context_token&&r.payload?.prefix==='docs',{hold:true})
    await h.page.locator('[data-tree-path="src"] .observation-tree-chevron').click(); await h.bounded(first.arrived,5000,'First expired branch')
    await h.page.locator('[data-tree-path="docs"] .observation-tree-chevron').click(); await h.bounded(second.arrived,5000,'Second expired branch')
    first.release(); await waitCreates(h,before+1)
    second.release(); await h.idle()
    assert.equal(creates(h).length,before+1)
    assert.ok(creates(h).at(-1).payload.generation>root.payload.generation)
    assert.equal(creates(h).at(-1).payload.search_session,root.payload.search_session)
    assert.equal(creates(h).at(-1).response.page.counts.match_count,64)
    assert.equal(h.discovery.maximumContexts,1)
  } finally { await h.close() }
})

test('paired mode links reload and inactive pairs restore; both populated views remain usable on mobile', {timeout:60000}, async()=>{
  const h=await openDiscovery()
  try {
    await h.start(); await h.page.locator('#observation-subject-query').fill('src/'); await h.page.locator('#observation-query').fill('Cache'); await waitCreates(h,2); await h.idle()
    const artifacts=resolve('../../.task-artifacts/3b6a739f')
    // Required bounded visual evidence: frontend/project maintainer owns these
    // two viewport PNGs through coordinator inspection and project closure.
    await mkdir(artifacts,{recursive:true})
    await h.page.screenshot({path:resolve(artifacts,'paired-all.png')})
    await h.page.getByRole('button',{name:'Subject Tree',exact:true}).click(); await h.idle()
    await h.page.locator('#observation-subject-query').fill('docs/'); await h.page.locator('#observation-query').fill('Documentation'); await waitCreates(h,creates(h).length+1); await h.idle()
    const url=h.page.url()
    await h.page.reload(); await h.idle()
    assert.equal(h.page.url(),url)
    assert.equal(await h.page.locator('#observation-subject-query').inputValue(),'docs/')
    assert.equal(await h.page.locator('#observation-query').inputValue(),'Documentation')
    assert.equal(await h.page.getByText('Full-text search · vector search disabled',{exact:true}).count(),1)
    await h.page.screenshot({path:resolve(artifacts,'paired-tree.png')})
    for(const file of ['paired-all.png','paired-tree.png']) assert.ok((await stat(resolve(artifacts,file))).size<=4*1024*1024)
    await h.page.getByRole('button',{name:'All Observations',exact:true}).click(); await h.idle()
    assert.equal(await h.page.locator('#observation-subject-query').inputValue(),'src/')
    assert.equal(await h.page.locator('#observation-query').inputValue(),'Cache')
    await h.page.setViewportSize({width:320,height:800})
    for(const id of ['observation-subject-query','observation-query']) { const input=h.page.locator('#'+id); await input.scrollIntoViewIfNeeded(); const box=await input.boundingBox(); assert.ok(box.x>=0&&box.x+box.width<=321); await input.focus(); assert.equal(await h.page.locator(':focus').getAttribute('id'),id) }
    await h.page.getByRole('button',{name:'Subject Tree',exact:true}).click(); await h.idle()
    const docs=h.page.locator('[data-tree-path="docs"]'); await docs.focus(); await h.page.keyboard.press('Enter'); await h.idle()
    assert.equal(await docs.getAttribute('aria-expanded'),'true')
    assert.equal(await h.page.locator('#observation-query').inputValue(),'Documentation')
  } finally { await h.close() }
})


test('a real scrollbar pointer drag supersedes clear-to-top ownership and late callbacks preserve user scroll', {timeout:60000}, async()=>{
  const h=await openDiscovery(undefined,values=>values,[],{ignoreDefaultArgs:['--hide-scrollbars']})
  try {
    await h.start(); await h.page.locator('#observation-subject-query').fill('docs/'); await h.page.locator('#observation-query').fill('Documentation'); await waitCreates(h,2); await h.idle()
    const late=h.gate(r=>r.payload?.filters?.content_query==='Documentation guide',{hold:true})
    await h.page.locator('#observation-query').fill('Documentation guide'); await h.bounded(late.arrived,5000,'Old response before scrollbar intent')
    await h.page.getByRole('button',{name:'Clear Search observations',exact:true}).click()
    await h.bounded((async()=>{while(!creates(h).some(r=>r.payload.filters.content_query===''&&r.payload.filters.subject_query==='docs/'&&r.done)) await h.page.waitForTimeout(20)})(),5000,'Clear bootstrap accepted')
    await h.page.waitForFunction(()=>document.getElementById('main-content-scroll').scrollTop===0)
    await h.page.getByText('Showing matching subjects',{exact:true}).waitFor()
    await h.page.addStyleTag({content:'#main-content-scroll { scrollbar-gutter: stable; overflow-y: scroll } #main-content-scroll::-webkit-scrollbar { width: 16px }'})
    await h.page.evaluate(()=>{window.__scrollPointer=[];document.addEventListener('pointerdown',e=>window.__scrollPointer.push(e.target.id||e.target.tagName),true)})
    await h.page.evaluate(async()=>{const s=document.getElementById('main-content-scroll');let previous='',stable=0;const end=performance.now()+2000;while(performance.now()<end&&stable<5){await new Promise(r=>requestAnimationFrame(()=>requestAnimationFrame(r)));const current=[s.scrollTop,s.scrollHeight,s.clientWidth].join(':');stable=current===previous?stable+1:0;previous=current}if(stable<5)throw new Error('Pre-drag geometry did not settle')})
    const bounds=await h.page.locator('#main-content-scroll').evaluate(element=>{const r=element.getBoundingClientRect();return{x:r.left+element.clientWidth+(element.offsetWidth-element.clientWidth)/2,y:r.top,gutter:element.offsetWidth-element.clientWidth,thumb:element.clientHeight*element.clientHeight/element.scrollHeight}})
    assert.ok(bounds.gutter>=8,'Actual native scrollbar gutter is present')
    await h.page.mouse.move(bounds.x,bounds.y+Math.min(bounds.thumb/2,40)); await h.page.mouse.down(); await h.page.mouse.move(bounds.x,bounds.y+400,{steps:8}); await h.page.mouse.up()
    await h.page.waitForFunction(()=>document.getElementById('main-content-scroll').scrollTop>100,null,{timeout:5000}).catch(async error=>{throw new Error(error.message+' '+JSON.stringify(await h.page.evaluate(()=>{const s=document.getElementById('main-content-scroll');return{top:s.scrollTop,height:s.scrollHeight,client:s.clientHeight,pointer:window.__scrollPointer}})))})
    const userTop=await h.page.evaluate(async()=>{const s=document.getElementById('main-content-scroll');let previous=-1,stable=0;const end=performance.now()+2000;while(performance.now()<end&&stable<5){await new Promise(r=>requestAnimationFrame(()=>requestAnimationFrame(r)));stable=s.scrollTop===previous?stable+1:0;previous=s.scrollTop}if(stable<5)throw new Error('Native drag geometry did not settle');return s.scrollTop})
    late.release(); await h.idle()
    await h.page.locator('#main-content-scroll').evaluate(element=>element.dispatchEvent(new Event('scroll')))
    await h.page.evaluate(()=>new Promise(resolve=>requestAnimationFrame(()=>requestAnimationFrame(resolve))))
    assert.equal(await h.page.locator('#main-content-scroll').evaluate(element=>element.scrollTop),userTop)
  } finally { await h.close() }
})


test('initial failed bootstrap settles both count displays and retry restores authoritative totals', {timeout:60000}, async()=>{
  const h=await openDiscovery()
  try {
    h.gate(r=>r.path==='/api/v1/observations/discovery',{error:429})
    await h.start()
    assert.ok(!(await h.page.locator('.workspace-header .workspace-observation-total').innerText()).includes('Loading'))
    assert.ok(!(await h.page.locator('.observation-mode-navigation').innerText()).includes('Counting'))
    await h.page.getByRole('button',{name:'Retry search',exact:true}).click(); await h.idle()
    assert.match(await h.page.locator('.workspace-header .workspace-observation-total').innerText(),/64 observations/)
  } finally { await h.close() }
})
