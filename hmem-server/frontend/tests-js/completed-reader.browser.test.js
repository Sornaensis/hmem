import assert from 'node:assert/strict'
import test from 'node:test'
import {gzipSync} from 'node:zlib'
import {createHash} from 'node:crypto'
import {openSessionExplorer} from './support/sessions-ux-fixture.mjs'
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

test('completed workspace reader keeps bounded ordinary pages and truthful known endpoints',{timeout:45000},async()=>{
 const raw=Buffer.from(Array.from({length:150},(_,i)=>JSON.stringify(message(i))).join('\n')+'\n'),h=await openSessionExplorer({transcriptBytes:raw}),{page}=h
 try{
  await archive(h);await open(h);await page.getByText('Verified complete archive.',{exact:true}).waitFor()
  const button=name=>page.locator('.session-archive-footer').getByRole('button',{name,exact:true})
  assert.match(await firstText(page),/Page message 000/);assert.equal(await button('Previous').isDisabled(),true);assert.equal(await button('Next').isEnabled(),true)
  assert.equal(await page.locator('#sealed-search,#sealed-position,[data-sealed-action=search],[data-sealed-action=search-next],[data-sealed-action=position]').count(),0)
  await button('Next').click();await until(async()=>/Page message 064/.test(await firstText(page)));assert.equal(await button('Previous').isEnabled(),true)
  await button('Previous').click();await until(async()=>/Page message 000/.test(await firstText(page)));assert.equal(await button('Previous').isDisabled(),true)
  await button('Last').click();await page.getByText('Page message 149',{exact:true}).waitFor();assert.equal(await button('Next').isDisabled(),true);assert.equal(await button('Last').isDisabled(),true)
  await button('First').click();await until(async()=>/Page message 000/.test(await firstText(page)));assert.equal(await button('First').isDisabled(),true)
  for(const [width,height,zoom]of[[1440,1000,1],[430,650,1],[960,667,1],[430,900,1.5]]){
   await page.setViewportSize({width,height});await page.evaluate(z=>{document.body.style.zoom=String(z)},zoom)
   await button('Next').scrollIntoViewIfNeeded();await button('Next').click();await until(async()=>/Page message 064/.test(await firstText(page)));await button('Previous').click();await until(async()=>/Page message 000/.test(await firstText(page)))
   const bounds=await page.evaluate(()=>{const r=s=>document.querySelector(s).getBoundingClientRect();return {identity:r('.session-sealed-workspace > .session-identity').toJSON(),status:r('.session-archive-status').toJSON(),records:r('#session-records').toJSON(),footer:r('.session-archive-footer').toJSON(),width:innerWidth,outer:document.documentElement.scrollWidth}})
   assert.ok(bounds.outer<=bounds.width);assert.ok(bounds.records.height>0);assert.ok(bounds.records.top-bounds.status.bottom<30*zoom,JSON.stringify(bounds));assert.ok(bounds.status.top-bounds.identity.bottom<15*zoom,JSON.stringify(bounds))
   assert.equal(await page.getByRole('button',{name:'Cancel',exact:true}).count(),0);assert.equal(await page.locator('.session-sealed-workspace').getByRole('button',{name:'Retry',exact:true}).count(),0)
  }
  assert.deepEqual(h.errors,[])
 }finally{await h.close()}
})

for(const count of[0,1,64])test('completed '+count+'-record archives preserve conservative paging without stranding the reader',{timeout:30000},async()=>{
 const raw=Buffer.from(Array.from({length:count},(_,i)=>JSON.stringify(message(i))).join('\n')+(count?'\n':'')),h=await openSessionExplorer({transcriptBytes:raw}),{page}=h
 try{
  await archive(h);await open(h);await page.locator('.session-archive-status').filter({hasText:count?'Verified complete archive.':'No matching messages on this page.'}).waitFor()
  const footer=page.locator('.session-archive-footer');assert.equal(await footer.getByRole('button',{name:'Previous',exact:true}).isDisabled(),true)
  if(count<64)assert.equal(await footer.getByRole('button',{name:'Next',exact:true}).isDisabled(),true)
  else{
   // The existing worker conservatively reports more at an exact64 boundary.
   assert.equal(await footer.getByRole('button',{name:'Next',exact:true}).isEnabled(),true)
   await footer.getByRole('button',{name:'Last',exact:true}).click();await until(()=>footer.getByRole('button',{name:'Next',exact:true}).isDisabled());await page.getByText('Page message 063',{exact:true}).waitFor();assert.equal(await footer.getByRole('button',{name:'Previous',exact:true}).isDisabled(),true);assert.equal(await footer.getByRole('button',{name:'Next',exact:true}).isDisabled(),true)
  }
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
  const continuation=page.getByRole('button',{name:'Continue body',exact:true});await continuation.waitFor();await continuation.click()
  const previous=page.getByRole('button',{name:'Previous body part',exact:true});await previous.waitFor();await previous.click();await previous.waitFor({state:'detached'})
  assert.equal(await page.locator('.session-archive-footer').getByRole('button',{name:'Next',exact:true}).isDisabled(),true)
  assert.deepEqual(h.errors,[])
 }finally{await h.close()}
})
