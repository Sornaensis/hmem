import assert from 'node:assert/strict'
import test from 'node:test'
import { join } from 'node:path'
import { openDiscovery } from './observation-discovery-fixture.mjs'
const keyPrefix='hmem.observation-ui.v1:'
const tuple=(mode,query='')=>[mode,query,null,'','',null,null,[],'','','']
const fragment=(mode,query='')=>'#tab=observations&ov=1&oq='+encodeURIComponent(JSON.stringify(tuple(mode,query)))
const tree=page=>page.getByRole('button',{name:'Subject Tree',exact:true})
const flat=page=>page.getByRole('button',{name:'All Observations',exact:true})
const ready=async h=>{await h.page.locator('#observation-panel').waitFor();await h.idle()}
const bare=async h=>{await h.page.goto(h.origin+'/workspace/'+h.fixture.workspace.id+'#tab=observations');await ready(h)}
const stored=page=>page.evaluate(prefix=>Object.entries(localStorage).find(([key])=>key.startsWith(prefix)),keyPrefix)
const replace=async(h,update)=>{const entry=await stored(h.page);assert.ok(entry,'A scoped preference was persisted');const prefs=update(JSON.parse(entry[1]));await h.page.evaluate(([key,prefs])=>localStorage.setItem(key,JSON.stringify(prefs)),[entry[0],prefs]);await h.page.goto('about:blank');await bare(h)}
const until=async fn=>{const end=Date.now()+10000;while(Date.now()<end){if(await fn())return;await new Promise(r=>setTimeout(r,25))}throw Error('Observation mode did not converge')}

test('fresh and invalid/legacy bare entries default to Tree, while explicit and meaningful Flat preferences persist',{timeout:60000},async()=>{
 const h=await openDiscovery(),{page}=h
 try{
  await bare(h);assert.equal(await tree(page).getAttribute('aria-pressed'),'true');assert.ok(h.receipts.filter(r=>r.payload?.view==='tree'&&r.payload.prefix===''&&r.payload.offset===0).length<=2,'fresh Tree entry and initial canonical handoff remain bounded');if(process.env.HMEM_DOCK_SCREENSHOTS)await page.screenshot({path:join(process.env.HMEM_DOCK_SCREENSHOTS,'observations-tree-default.png'),fullPage:true})
  await flat(page).click();await ready(h);await until(async()=>JSON.parse((await stored(page))?.[1]||'{}').modeIntent===true)
  await page.goto('about:blank');await bare(h);assert.equal(await flat(page).getAttribute('aria-pressed'),'true','a new explicit All choice persists')
  await replace(h,prefs=>{delete prefs.modeIntent;prefs.context=encodeURIComponent(JSON.stringify(tuple('flat')));prefs.detail=null;return prefs});assert.equal(await tree(page).getAttribute('aria-pressed'),'true','old implicit blank All context migrates')
  await flat(page).click();await ready(h)
  await replace(h,prefs=>{delete prefs.modeIntent;prefs.context=encodeURIComponent(JSON.stringify(tuple('flat','Cache')));return prefs});assert.equal(await flat(page).getAttribute('aria-pressed'),'true');assert.equal(await page.locator('#observation-query').inputValue(),'Cache','meaningful legacy Flat context is retained')
  await replace(h,prefs=>({...prefs,context:'%not-a-query'}));assert.equal(await tree(page).getAttribute('aria-pressed'),'true')
  await page.goto(h.origin+'/workspace/'+h.fixture.workspace.id+fragment('flat'));await ready(h);assert.equal(await flat(page).getAttribute('aria-pressed'),'true','external Flat URL wins')
  await page.goto('about:blank');await bare(h);assert.equal(await flat(page).getAttribute('aria-pressed'),'true','accepted explicit Flat URL records intent')
  await tree(page).click();await ready(h);await flat(page).click();await ready(h)
  await page.evaluate(()=>history.back());await ready(h);assert.equal(await tree(page).getAttribute('aria-pressed'),'true')
  await page.evaluate(()=>history.forward());await ready(h);assert.equal(await flat(page).getAttribute('aria-pressed'),'true')
  assert.deepEqual(h.errors,[])
 }finally{await h.close()}
})

test('late scoped hydration replaces the internally generated Tree entry with saved explicit Flat and user changes win',{timeout:60000},async()=>{
 const h=await openDiscovery(),{page}=h
 try{
  await bare(h);await flat(page).click();await ready(h);await until(async()=>JSON.parse((await stored(page))?.[1]||'{}').modeIntent===true)
  await page.addInitScript(()=>{
   const get=Storage.prototype.getItem,frame=requestAnimationFrame.bind(window);let pending=false
   Storage.prototype.getItem=function(key){const value=get.call(this,key);if(location.hash==='#tab=projects'&&key.startsWith('hmem.observation-ui.v1:'))pending=true;return value}
   window.requestAnimationFrame=callback=>{if(pending){pending=false;window.releasePreferenceRead=()=>frame(callback);return -1}return frame(callback)}
  })
  await page.goto('about:blank');await page.goto(h.origin+'/workspace/'+h.fixture.workspace.id+'#tab=projects');await page.waitForFunction(()=>typeof window.releasePreferenceRead==='function');await h.idle()
  await page.getByRole('button',{name:'Observations',exact:true}).click();await ready(h);assert.equal(await tree(page).getAttribute('aria-pressed'),'true')
  await page.evaluate(()=>window.releasePreferenceRead());await until(async()=>await flat(page).getAttribute('aria-pressed')==='true');await ready(h)
  assert.equal(JSON.parse(new URLSearchParams(new URL(page.url()).hash.slice(1)).get('oq'))[0],'flat','owned Tree echo is rewritten to restored Flat')
  await page.goto('about:blank');await page.goto(h.origin+'/workspace/'+h.fixture.workspace.id+'#tab=projects');await page.waitForFunction(()=>typeof window.releasePreferenceRead==='function');await h.idle()
  await page.getByRole('button',{name:'Observations',exact:true}).click();await ready(h);await flat(page).click();await ready(h);await tree(page).click();await ready(h)
  await page.evaluate(()=>window.releasePreferenceRead());await ready(h);assert.equal(await tree(page).getAttribute('aria-pressed'),'true','new user intent supersedes the held Flat read')
  assert.deepEqual(h.errors,[])
 }finally{await h.close()}
})


test('initial canonical shell retires the held old Tree root and one current root installs fresh membership',{timeout:45000},async()=>{
 const h=await openDiscovery(),{page}=h
 const root=r=>r.payload?.view==='tree'&&r.payload.prefix===''&&r.payload.offset===0
 const shell=h.gate(r=>r.path==='/api/v1/change-stream/resync',{hold:true}),first=h.gate(root,{hold:true}),second=h.gate(root,{hold:true})
 try{
  await page.goto(h.origin+'/workspace/'+h.fixture.workspace.id+'#tab=observations')
  await h.bounded(Promise.all([shell.arrived,first.arrived]),5000,'Initial shell and admitted Tree root')
  assert.equal(h.receipts.filter(r=>root({payload:r.payload})).length,1)
  const original=h.fixture.observations[0]
  h.fixture.observations.push({...original,id:'00000000-0000-4000-8000-000000009999',subject:'new-born/after-shell.elm',subjects:[{subject_kind:'file',subject:'new-born/after-shell.elm'}],content:'Fresh canonical membership'})
  shell.release();await page.locator('.connection-status.connected').waitFor();first.release()
  await h.bounded(second.arrived,5000,'Current Tree revalidation after retired callback')
  assert.equal(first.receipt.discovery.done,true,'old physical response releases before the next root')
  assert.equal(await page.locator('[data-tree-path]').count(),0,'the retired old root cannot install stale paths')
  const reads=h.receipts.filter(r=>root({payload:r.payload}));assert.equal(reads.length,2)
  assert.equal(reads[1].payload.context_token,reads[0].response.context_token);assert.equal(reads[1].payload.search_session,undefined)
  assert.ok(reads.every(r=>r.payload.limit===50&&r.payload.offset===0&&r.payload.prefix===''))
  second.release();await h.idle();assert.equal(await page.locator('[data-tree-path="new-born"]').count(),1)
  assert.match(await page.locator('.workspace-header .workspace-observation-total').innerText(),/65 observations/)
  assert.equal(h.receipts.filter(r=>root({payload:r.payload})).length,2,'no third request after current membership settles')
  assert.deepEqual(h.errors,[])
 }finally{shell.release();first.release();second.release();await h.close()}
})
