import assert from 'node:assert/strict'
import test from 'node:test'
import {gzipSync} from 'node:zlib'
import {createHash} from 'node:crypto'
import {openSessionExplorer,continuousScrollStep} from './support/sessions-ux-fixture.mjs'
const hash=b=>createHash('sha256').update(b).digest('hex')
const generation='33333333-3333-4333-8333-333333333333'
const until=async fn=>{const deadline=Date.now()+10000;while(Date.now()<deadline){if(await fn())return;await new Promise(r=>setTimeout(r,20))}throw Error('completed reader did not converge')}
async function archive(h,mode=()=> 'ready'){
 const raw=h.content.get(h.saved.id),gzip=gzipSync(raw)
 await h.page.route('**/sessions/**/archive**',async route=>{
  const headers={ETag:'"completed-reader"','Content-Type':'application/gzip','Content-Length':String(gzip.length),'X-HMem-Archive-Generation':generation,'X-HMem-Archive-SHA256':hash(gzip),'X-HMem-Archive-Raw-Bytes':String(raw.length),'X-HMem-Archive-Raw-SHA256':hash(raw)}
  if(new URL(route.request().url()).pathname.endsWith('/content'))return route.request().headers()['if-none-match']?route.fulfill({status:304,headers:{ETag:headers.ETag}}):route.fulfill({body:gzip,headers})
  if(mode()==='preparing')return route.fulfill({status:202,json:{workspace_id:h.fixture.workspace.id,session_id:h.saved.id,state:'preparing',retry_after_ms:60000}})
  if(mode()==='error')return route.fulfill({status:503,json:{error:'archive_unavailable'}})
  return route.fulfill({json:{workspace_id:h.fixture.workspace.id,session_id:h.saved.id,state:'ready',generation_id:generation,codec:'gzip-v1',raw_bytes:String(raw.length),raw_records:String(raw.toString('utf8').split('\n').filter(Boolean).length),raw_sha256:hash(raw),compressed_bytes:String(gzip.length),compressed_sha256:hash(gzip),content_path:`/api/v1/workspaces/${h.fixture.workspace.id}/sessions/${h.saved.id}/archive/${generation}/content`}})
 })
}
async function open(h){await h.start();await h.idle();await h.page.getByRole('button',{name:'Sessions',exact:true}).click();await h.page.getByRole('button',{name:'Live',exact:true}).click();await h.page.getByRole('button',{name:'Completed',exact:true}).click();await h.page.locator('.session-summary a[href*="'+h.saved.id+'"]').click()}
const message=i=>({type:'response_item',payload:{type:'message',role:'assistant',content:[{type:'output_text',text:'Page message '+String(i).padStart(3,'0')}]}})
const firstText=page=>page.locator('#session-records .session-record').first().innerText()

test('completed workspace reader keeps continuous bounded history and truthful known endpoints',{timeout:45000},async()=>{
 const raw=Buffer.from(Array.from({length:150},(_,i)=>JSON.stringify(message(i))).join('\n')+'\n'),h=await openSessionExplorer({transcriptBytes:raw}),{page}=h
 try{
  await archive(h);await open(h);await page.getByText('Verified complete archive.',{exact:true}).waitFor()
  await page.getByText('Page message 149',{exact:true}).waitFor();await h.idle()
  const seen=new Set(),collect=async()=>{const values=await page.locator('.session-record-text').allTextContents();for(const text of values)for(const match of text.matchAll(/Page message (\d+)/g))seen.add(Number(match[1]));assert.ok(await page.locator('.session-record').count()<=200)}
  await collect();for(let step=0;seen.size<150;step++){assert.ok(step<12);await continuousScrollStep(page,-1);await h.idle();await collect()}
  assert.deepEqual([...seen].sort((a,b)=>a-b),Array.from({length:150},(_,i)=>i));assert.match(await firstText(page),/Page message 000/)
  assert.equal(await page.locator('#sealed-search,#sealed-position,[data-sealed-action=search],[data-sealed-action=search-next],[data-sealed-action=position]').count(),0)
  assert.equal(await page.locator('.session-sealed-workspace').getByRole('button',{name:/^(First|Previous|Next|Last)$/}).count(),0)
  for(const [width,height,zoom]of[[1440,1000,1],[430,650,1],[960,667,1],[430,900,1.5]]){
   await page.setViewportSize({width,height});await page.evaluate(z=>{document.body.style.zoom=String(z)},zoom)
   const phase=async name=>console.log(JSON.stringify({phase:'completed-responsive-'+name,width,height,zoom,state:await page.locator('#session-records').evaluate(node=>({attrs:{...node.dataset},host:{...node.closest('.session-viewer').dataset},top:node.scrollTop,height:node.clientHeight,scroll:node.scrollHeight,first:node.querySelector('.session-record')?.textContent.slice(0,60),last:[...node.querySelectorAll('.session-record')].at(-1)?.textContent.slice(0,60)}))}))
   await page.locator('#session-records').press('End');await until(()=>page.locator('#session-records').evaluate(node=>node.dataset.latestPending==='false'&&node.scrollHeight-node.scrollTop-node.clientHeight<=4)).catch(async error=>{await phase('End');throw error});await page.locator('#session-records').press('Home');await until(async()=>/Page message 000/.test(await firstText(page))&&await page.locator('#session-records').evaluate(node=>node.dataset.archiveCanPrevious==='false'&&node.scrollTop<=4)).catch(async error=>{await phase('Home');throw error})
   const bounds=await page.evaluate(()=>{const r=s=>document.querySelector(s).getBoundingClientRect();return {identity:r('.session-sealed-workspace > .session-identity').toJSON(),modes:r('.session-presentation-tabs').toJSON(),status:r('.session-archive-status').toJSON(),records:r('#session-records').toJSON(),footer:r('.session-archive-footer').toJSON(),width:innerWidth,outer:document.documentElement.scrollWidth}})
   assert.ok(bounds.outer<=bounds.width);assert.ok(bounds.records.height>0);assert.ok(bounds.records.top-bounds.status.bottom<30*zoom,JSON.stringify(bounds));assert.ok(bounds.modes.top-bounds.identity.bottom<15*zoom,JSON.stringify(bounds));assert.ok(bounds.status.top-bounds.modes.bottom<15*zoom,JSON.stringify(bounds))
   assert.equal(await page.getByRole('button',{name:'Cancel',exact:true}).count(),0);assert.equal(await page.locator('.session-sealed-workspace').getByRole('button',{name:'Retry',exact:true}).count(),0)
  }
  assert.deepEqual(h.errors,[])
 }finally{await h.close()}
})

for(const count of[0,1,64])test('completed '+count+'-record archives preserve conservative acquisition without stranding the reader',{timeout:30000},async()=>{
 const raw=Buffer.from(Array.from({length:count},(_,i)=>JSON.stringify(message(i))).join('\n')+(count?'\n':'')),h=await openSessionExplorer({transcriptBytes:raw}),{page}=h
 try{
  await archive(h);await open(h);await page.locator('.session-archive-status').filter({hasText:count?'Verified complete archive.':'This session has no messages.'}).waitFor()
  assert.equal(await page.locator('.session-archive-footer').getByRole('button',{name:/^(First|Previous|Next|Last)$/}).count(),0)
  if(count){await page.getByText('Page message '+String(count-1).padStart(3,'0'),{exact:true}).waitFor();await h.idle();await page.locator('#session-records').press('Home');await page.getByText('Page message 000',{exact:true}).waitFor();await until(()=>page.locator('#session-records').evaluate(node=>node.dataset.archiveCanPrevious==='false'));await h.idle();await page.locator('#session-records').press('End');await until(()=>page.locator('#session-records').evaluate(node=>node.dataset.latestPending==='false'&&node.dataset.archiveCanNext==='false'&&node.scrollHeight-node.scrollTop-node.clientHeight<=4)).catch(async error=>{console.log(JSON.stringify({phase:'completed-exact-end',count,state:await page.locator('#session-records').evaluate(node=>({...node.dataset,top:node.scrollTop,height:node.clientHeight,scroll:node.scrollHeight}))}));throw error})}
  else assert.equal(await page.locator('#session-records .session-record').count(),0)
  assert.deepEqual(h.errors,[])
 }finally{await h.close()}
})

test('completed recovery controls are contextual and cancelled/error states publish no transcript',{timeout:45000},async()=>{
 const h=await openSessionExplorer(),{page}=h;let mode='preparing'
 try{
  await archive(h,()=>mode);await open(h);await page.getByRole('button',{name:'Cancel',exact:true}).waitFor()
  assert.equal(await page.locator('#session-records .session-record').count(),0)
  await page.getByRole('button',{name:'Cancel',exact:true}).click();await page.getByText('Archive loading cancelled.',{exact:true}).waitFor();assert.equal(await page.getByRole('button',{name:'Cancel',exact:true}).count(),0)
  mode='error';await page.locator('.session-archive-footer').getByRole('button',{name:'Retry',exact:true}).click();await page.getByRole('alert').filter({hasText:'archive_unavailable'}).waitFor();assert.equal(await page.locator('#session-records .session-record').count(),0)
  mode='ready';await page.locator('.session-archive-footer').getByRole('button',{name:'Retry',exact:true}).click();await page.getByText('Verified complete archive.',{exact:true}).waitFor();await page.getByRole('heading',{name:'A calmer session explorer'}).waitFor()
  assert.equal(await page.getByRole('button',{name:'Cancel',exact:true}).count(),0);assert.equal(await page.locator('.session-archive-footer').getByRole('button',{name:'Retry',exact:true}).count(),0)
  const activity=page.locator('.session-activity-group').first();await activity.locator(':scope > summary').click();const tool=activity.locator('.session-content-tool').first();await tool.locator(':scope > summary').click();await tool.locator('pre').waitFor()
  const downloadReady=page.waitForEvent('download');await page.getByRole('button',{name:/Download session/}).click();const download=await downloadReady,parts=[]
  for await(const part of await download.createReadStream())parts.push(part)
  assert.deepEqual(Buffer.concat(parts),h.content.get(h.saved.id))
  assert.deepEqual(h.errors,[])
 }finally{await h.close()}
})


test('completed native tool body parts remain reachable independently of ordinary pages',{timeout:30000},async()=>{
 const value={type:'response_item',payload:{type:'function_call',name:'Read',arguments:JSON.stringify({payload:'x'.repeat(140000)})}},h=await openSessionExplorer({transcriptBytes:Buffer.from(JSON.stringify(value)+'\n')}),{page}=h
 try{
  await archive(h);await open(h);await page.getByText('Verified complete archive.',{exact:true}).waitFor()
  await page.locator('.session-activity-group > summary').click();const tool=page.locator('.session-content-tool');await tool.locator(':scope > summary').click()
  await tool.locator('pre').first().waitFor();await page.locator('#session-records').press('Home');await page.locator('[data-body-part-index="0"]').waitFor()
  const part=()=>page.locator('#session-records').evaluate(node=>Math.max(-1,...[...node.querySelectorAll('[data-body-part-index]')].map(part=>Number(part.dataset.bodyPartIndex))))
  for(let step=0;await part()===0;step++){assert.ok(step<40);await continuousScrollStep(page,1);await h.idle()}
  assert.ok(await part()>0);await page.locator('#session-records').press('Home');await until(()=>page.locator('[data-body-part-index="0"]').count())
  assert.equal(await page.getByRole('button',{name:/^(Continue body|Previous body part|First|Previous|Next|Last)$/}).count(),0);assert.ok(await page.locator('[data-body-part-index]').count()<=3)
  assert.deepEqual(h.errors,[])
 }finally{await h.close()}
})
