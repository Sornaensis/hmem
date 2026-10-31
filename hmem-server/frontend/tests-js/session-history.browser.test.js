import assert from 'node:assert/strict'
import test from 'node:test'
import {openSessionExplorer} from './support/sessions-ux-fixture.mjs'

const until=async fn=>{const end=Date.now()+15000;while(Date.now()<end){if(await fn())return;await new Promise(resolve=>setTimeout(resolve,25))}throw Error('Progressive conversation did not converge')}
const state=page=>page.locator('#session-records').evaluate(node=>({before:Number(node.dataset.historyBefore),after:Number(node.dataset.historyAfter),latest:Number(node.dataset.historyLatest),loading:node.dataset.historyLoading==='true',offsets:[...node.querySelectorAll('.session-record')].map(row=>Number(row.dataset.offset))}))

test('progressive large hydration and multi-window bursts preserve follow while real historical input pauses it',{timeout:60000},async()=>{
 const record=(size,marker)=>{const head=JSON.stringify({text:marker}).slice(0,-2);return Buffer.from(head+'x'.repeat(size-Buffer.byteLength(head)-3)+'"}\n')}
 const prefix=Buffer.concat(Array.from({length:16},(_,i)=>record(65536,`Initial ${i} `))),burst=Buffer.concat(Array.from({length:32},(_,i)=>record(8192,`Burst ${i} `)))
 const h=await openSessionExplorer({transcriptBytes:prefix}),{page}=h;let hold=false,releaseRead=null
 const settled=()=>page.locator('#session-records').evaluate(node=>node.dataset.historyLoading==='false'&&Number(node.dataset.historyAfter)>=Number(node.dataset.historyLatest)&&node.scrollHeight-node.scrollTop-node.clientHeight<=4)
 try{
  await page.route('**/sessions/**/transcript**',async route=>{if(hold){hold=false;await new Promise(resolve=>{releaseRead=resolve})}else await new Promise(resolve=>setTimeout(resolve,20));await route.fallback()})
  await h.start();await h.idle();await page.goto(h.origin+'/workspace/'+h.fixture.workspace.id+'#tab=sessions&session='+h.active.id);await page.locator('.session-record').first().waitFor()
  const viewport=page.locator('#session-records');assert.equal(await viewport.getAttribute('data-history-loading'),'true')
  await viewport.press('End');await until(settled);assert.equal((await state(page)).after,prefix.length)
  await h.appendBytes(burst);await until(settled);assert.equal((await state(page)).after,prefix.length+burst.length)
  hold=true;await h.appendBytes(burst);await until(async()=>releaseRead!==null)
  await viewport.hover();await page.mouse.wheel(0,-1400)
  await until(async()=>await viewport.evaluate(node=>node.scrollHeight-node.scrollTop-node.clientHeight>48))
  const anchor=await viewport.evaluate(node=>{const bounds=node.getBoundingClientRect(),row=[...node.querySelectorAll('[data-block-key]')].find(row=>row.getBoundingClientRect().bottom>bounds.top);return {key:row.dataset.blockKey,top:row.getBoundingClientRect().top-bounds.top}})
  releaseRead();releaseRead=null;await until(async()=>!(await state(page)).loading)
  const historical=await state(page);assert.ok(historical.after<historical.latest,'historical input retires further live-window demand')
  assert.ok(await viewport.evaluate((node,anchor)=>{const row=[...node.querySelectorAll('[data-block-key]')].find(row=>row.dataset.blockKey===anchor.key);return row&&Math.abs(row.getBoundingClientRect().top-node.getBoundingClientRect().top-anchor.top)<=4},anchor))
  await page.getByRole('button',{name:'Jump to latest',exact:true}).click();await until(settled)
  assert.ok(h.peak()<=2);assert.ok(h.reads.filter(read=>read.path.endsWith('/transcript')).every(read=>Number(new URLSearchParams(read.query).get('limit'))<=65536));assert.deepEqual(h.errors,[])
 }finally{releaseRead?.();await h.close()}
})

test('Jump to latest is shared by dock and workspace and reacquires the logical active tail',{timeout:60000},async()=>{
 const bytes=Buffer.from(Array.from({length:420},(_,i)=>JSON.stringify({text:`Latest occurrence ${i} `+'word '.repeat(25)})+'\n').join(''))
 const h=await openSessionExplorer({transcriptBytes:bytes}),{page}=h
 const latest=()=>page.getByRole('button',{name:'Jump to latest',exact:true})
 const atEnd=()=>page.locator('#session-records').evaluate(node=>node.dataset.latestPending==='false'&&Number(node.dataset.historyAfter)>=Number(node.dataset.historyLatest)&&node.scrollHeight-node.scrollTop-node.clientHeight<=4)
 try{
  await h.start();await h.idle();const card=page.locator('#entity-'+h.fixture.tasks[0].id);await until(()=>card.locator('.task-sessions-icon').isEnabled());await card.locator('.task-sessions-icon').click();await page.locator('.session-record').first().waitFor()
  for(const surface of ['dock','workspace']){
   if(surface==='workspace'){await card.getByRole('link',{name:'Open full session ↗'}).click();await page.locator('.session-detail').waitFor()}
   await until(async()=>!(await state(page)).loading)
   await page.locator('#session-records').evaluate(node=>{node.scrollTop=0});await latest().waitFor({state:'visible'})
   assert.equal(await latest().locator('[aria-hidden="true"]').count(),2);assert.equal((await latest().textContent()).replaceAll(/\s/g,''),'↓Jumptolatest↓')
   if(surface==='workspace')for(const width of [1440,430]){
    await page.setViewportSize({width,height:900});await h.idle();await page.locator('#session-records').evaluate(node=>{node.scrollTop=0});await latest().waitFor({state:'visible'})
    const button=await latest().boundingBox(),records=await page.locator('#session-records').boundingBox()
    assert.ok(Math.abs(button.x+button.width/2-records.x-records.width/2)<8);assert.ok(button.y+button.height<=records.y+records.height-8)
   }
   await latest().focus();await page.keyboard.press(surface==='dock'?'Enter':'Space');await until(atEnd);await latest().waitFor({state:'hidden'})
   assert.equal(await page.locator('#session-records').evaluate(node=>document.activeElement===node),true)
  }
  await page.getByRole('button',{name:'Earlier messages',exact:true}).click();await until(async()=>{const value=await state(page);return !value.loading&&value.after<value.latest})
  await page.locator('#session-records').evaluate(node=>{node.scrollTop=node.scrollHeight});await latest().waitFor({state:'visible'})
  assert.ok(await page.locator('#session-records').evaluate(node=>node.querySelector('.session-record:last-child').getBoundingClientRect().bottom<=node.closest('.session-viewer').querySelector('.session-jump-latest').getBoundingClientRect().top+2),'reserved space keeps the control clear of the last mounted message')
  await h.appendRecords({text:'Newest output outside the mounted history'});await latest().click();await page.getByText('Newest output outside the mounted history',{exact:true}).waitFor();await until(atEnd)
  await h.appendRecords({text:'Following resumes after latest navigation'});await page.getByText('Following resumes after latest navigation',{exact:true}).waitFor();await until(atEnd)
  assert.deepEqual(h.errors,[])
 }finally{await h.close()}
})

test('a failed active latest request retains history and the same overlay retries it',{timeout:45000},async()=>{
 const bytes=Buffer.from(Array.from({length:500},(_,i)=>JSON.stringify({text:`Retry active occurrence ${i} `+'text '.repeat(50)})+'\n').join(''))
 const h=await openSessionExplorer({transcriptBytes:bytes}),{page}=h;let failing=false
 try{
  await page.route('**/sessions/**/transcript**',route=>failing?route.fulfill({status:400,json:{error:'controlled_latest_failure'}}):route.fallback())
  await h.start();await h.idle();await page.goto(h.origin+'/workspace/'+h.fixture.workspace.id+'#tab=sessions&session='+h.active.id);await page.locator('.session-record').first().waitFor();await until(async()=>!(await state(page)).loading)
  await page.getByRole('button',{name:'Earlier messages',exact:true}).click();await until(async()=>{const value=await state(page);return !value.loading&&value.after<value.latest})
  const old=(await state(page)).offsets
  await h.appendRecords({text:'Output beyond the historical page'})
  failing=true;const jump=page.getByRole('button',{name:'Jump to latest',exact:true});await jump.click()
  await page.getByText('Latest messages could not be reached. Activate Jump to latest to retry.',{exact:true}).waitFor()
  assert.deepEqual((await state(page)).offsets,old);assert.equal(await jump.isEnabled(),true)
  failing=false;await jump.click();await page.getByText('Output beyond the historical page',{exact:true}).waitFor()
  await until(async()=>await page.locator('#session-records').evaluate(node=>node.dataset.latestPending==='false'&&node.scrollHeight-node.scrollTop-node.clientHeight<=4))
  await jump.waitFor({state:'hidden'});assert.deepEqual(h.errors,[])
 }finally{await h.close()}
})

test('shared reader progressively reaches every occurrence across byte and row limits in both directions',{timeout:90000},async()=>{
 const records=Array.from({length:340},(_,i)=>({type:'event_msg',payload:{type:'agent_message',message:`Occurrence ${i} `+'x'.repeat(i%9===0?65000:30)}}))
 const lines=records.map(record=>JSON.stringify(record)+'\r\n'),bytes=Buffer.from(lines.join('')),expected=[];let cursor=0
 for(const line of lines){expected.push(cursor);cursor+=Buffer.byteLength(line)}
 assert.ok(bytes.length>2097152)
 const h=await openSessionExplorer({transcriptBytes:bytes}),{page}=h
 try{
  await h.start();await h.idle();await page.goto(h.origin+'/workspace/'+h.fixture.workspace.id+'#tab=sessions&session='+h.active.id);await page.locator('.session-record').first().waitFor()
  await until(async()=>!(await state(page)).loading)
  const backward=new Set((await state(page)).offsets)
  for(let pages=0;(await state(page)).before>0;pages++){
   assert.ok(pages<40,'history must progress in bounded pages')
   const previous=await state(page)
   await page.getByRole('button',{name:'Earlier messages',exact:true}).click()
   await until(async()=>{const current=await state(page);return !current.loading&&current.before<previous.before})
   for(const offset of (await state(page)).offsets)backward.add(offset)
  }
  assert.deepEqual([...backward].sort((a,b)=>a-b),expected)
  const forward=new Set((await state(page)).offsets)
  for(let pages=0;(await state(page)).after<bytes.length;pages++){
   assert.ok(pages<40,'newer traversal must progress in bounded pages')
   const previous=await state(page)
   await page.getByRole('button',{name:'Newer messages',exact:true}).click()
   await until(async()=>{const current=await state(page);return !current.loading&&current.after>previous.after})
   for(const offset of (await state(page)).offsets)forward.add(offset)
  }
  assert.deepEqual([...forward].sort((a,b)=>a-b),expected)
  assert.deepEqual(h.errors,[]);assert.ok(h.peak()<=2)
 }finally{await h.close()}
})

for(const suffix of ['', '\r\n'])test(`an initial lookback inside a large final record ${suffix?'with CRLF':'without LF'} expands to publish it`,{timeout:45000},async()=>{
 const bytes=Buffer.from(JSON.stringify({text:'Earlier record'})+'\n'+JSON.stringify({text:'Final large record '+'x'.repeat(180000)})+suffix)
 const h=await openSessionExplorer({transcriptBytes:bytes}),{page}=h
 try{
  await h.start();await h.idle();await page.goto(h.origin+'/workspace/'+h.fixture.workspace.id+'#tab=sessions&session='+h.active.id)
  await page.getByText(/^Final large record/).waitFor()
  await until(async()=>!(await state(page)).loading)
  assert.equal((await state(page)).after,bytes.length)
  assert.deepEqual(h.errors,[])
 }finally{await h.close()}
})

test('real boundary scrolling preserves the visible anchor through history, appends and resize',{timeout:45000},async()=>{
 const bytes=Buffer.from(Array.from({length:420},(_,i)=>JSON.stringify({type:'event_msg',payload:{type:'agent_message',message:`Reading anchor ${i} `+'word '.repeat(20)}})+'\n').join(''))
 const h=await openSessionExplorer({transcriptBytes:bytes}),{page}=h
 const anchor=()=>page.locator('#session-records').evaluate(node=>{const bounds=node.getBoundingClientRect(),row=[...node.querySelectorAll('[data-block-key]')].find(row=>row.getBoundingClientRect().bottom>bounds.top);return {key:row.dataset.blockKey,top:row.getBoundingClientRect().top-bounds.top}})
 const position=key=>page.locator('#session-records').evaluate((node,key)=>{const row=[...node.querySelectorAll('[data-block-key]')].find(row=>row.dataset.blockKey===key);return row?row.getBoundingClientRect().top-node.getBoundingClientRect().top:null},key)
 try{
  await h.start();await h.idle();await page.goto(h.origin+'/workspace/'+h.fixture.workspace.id+'#tab=sessions&session='+h.active.id)
  await page.locator('.session-record').first().waitFor();await until(async()=>!(await state(page)).loading)
  const viewport=page.locator('#session-records');await viewport.evaluate(node=>{node.scrollTop=0});await h.idle()
  const before=await state(page),reading=await anchor()
  await viewport.hover();await page.mouse.wheel(0,-400)
  await until(async()=>{const current=await state(page);return !current.loading&&current.before<before.before})
  await h.idle();assert.ok(Math.abs((await position(reading.key))-reading.top)<=4,'earlier page preserves the same visible occurrence')
  await h.appendRecords({text:'New live output while reading history'});await h.idle()
  assert.ok(Math.abs((await position(reading.key))-reading.top)<=4,'appends preserve a historical anchor')
  await page.setViewportSize({width:900,height:720});await h.idle()
  assert.ok(Math.abs((await position(reading.key))-reading.top)<=4,'resize preserves the same occurrence and intrarow position')
  assert.deepEqual(h.errors,[])
 }finally{await h.close()}
})
