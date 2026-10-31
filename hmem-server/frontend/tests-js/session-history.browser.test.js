import assert from 'node:assert/strict'
import test from 'node:test'
import {gzipSync} from 'node:zlib'
import {createHash} from 'node:crypto'
import {join} from 'node:path'
import {openSessionExplorer} from './support/sessions-ux-fixture.mjs'

const until=async fn=>{const end=Date.now()+15000;while(Date.now()<end){if(await fn())return;await new Promise(resolve=>setTimeout(resolve,25))}throw Error('Progressive conversation did not converge')}
const state=page=>page.locator('#session-records').evaluate(node=>({before:Number(node.dataset.historyBefore),after:Number(node.dataset.historyAfter),latest:Number(node.dataset.historyLatest),loading:node.dataset.historyLoading==='true',offsets:[...node.querySelectorAll('.session-record')].map(row=>Number(row.dataset.offset))}))

test('progressive large hydration and multi-window bursts preserve follow while real historical input pauses it',{timeout:60000},async()=>{
 const record=(size,marker)=>{const head=JSON.stringify({text:marker}).slice(0,-2);return Buffer.from(head+'x'.repeat(size-Buffer.byteLength(head)-3)+'"}\n')}
 const prefix=Buffer.concat(Array.from({length:16},(_,i)=>record(65536,`Initial ${i} `))),burst=Buffer.concat(Array.from({length:32},(_,i)=>record(8192,`Burst ${i} `)))
 const h=await openSessionExplorer({transcriptBytes:prefix}),{page}=h;let hold=false,releaseRead=null
 const settled=()=>page.locator('#session-records').evaluate(node=>node.dataset.historyLoading==='false'&&Number(node.dataset.historyAfter)>=Number(node.dataset.historyLatest)&&node.scrollHeight-node.scrollTop-node.clientHeight<=4)
 try{
  await page.addInitScript(()=>document.addEventListener('keydown',event=>{if(event.key!=='End'||event.target.id!=='session-records')return;const node=event.target,snapshot=()=>({attrs:{...node.dataset},top:node.scrollTop,height:node.scrollHeight,client:node.clientHeight});window.initialEndProof={before:snapshot(),trusted:event.isTrusted,scrolls:[]};node.addEventListener('scroll',()=>{window.initialEndProof.scrolls.push(snapshot());window.initialEndProof.scrolls=window.initialEndProof.scrolls.slice(-8)})},true))
  await page.route('**/sessions/**/transcript**',async route=>{if(hold){hold=false;await new Promise(resolve=>{releaseRead=resolve})}else await new Promise(resolve=>setTimeout(resolve,20));await route.fallback()})
  await h.start();await h.idle();await page.goto(h.origin+'/workspace/'+h.fixture.workspace.id+'#tab=sessions&session='+h.active.id);await page.locator('.session-record').first().waitFor()
  const viewport=page.locator('#session-records');assert.equal(await viewport.getAttribute('data-history-loading'),'true')
  await viewport.press('End');await until(settled).catch(async error=>{console.log(JSON.stringify({phase:'initial-End',proof:await page.evaluate(()=>window.initialEndProof),after:await viewport.evaluate(node=>({attrs:{...node.dataset},top:node.scrollTop,height:node.scrollHeight,client:node.clientHeight})),reads:h.reads.filter(read=>read.path.endsWith('/transcript')).slice(-3)}));throw error});assert.equal((await state(page)).after,prefix.length)
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
   await h.idle();await until(atEnd)
   await page.locator('#session-records').hover();await page.mouse.wheel(0,-320);await h.idle();await latest().waitFor({state:'visible'})
   assert.equal(await latest().locator('[aria-hidden="true"]').count(),2);assert.equal((await latest().textContent()).replaceAll(/\s/g,''),'↓Jumptolatest↓')
   if(surface==='workspace')for(const width of [1440,430]){
    await page.setViewportSize({width,height:900});await h.idle();await latest().waitFor({state:'visible'})
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

for(const sealed of [false,true])for(const width of [1440,430])test(`${sealed?'sealed':'active'} latest failure keeps readable retry chrome clear at ${width}px`,{timeout:45000},async()=>{
 const raw=Buffer.from(Array.from({length:180},(_,i)=>JSON.stringify({text:`Layout occurrence ${i} `+'Readable context. '.repeat(30)})+'\n').join(''))
 const h=await openSessionExplorer({transcriptBytes:raw}),{page}=h;let failing=false,injected=false
 try{
  await page.setViewportSize({width,height:width===430?720:900});await page.emulateMedia({reducedMotion:'reduce'})
  if(sealed){
   const hash=bytes=>createHash('sha256').update(bytes).digest('hex'),gzip=gzipSync(raw),generation='33333333-3333-4333-8333-333333333333',etag='"layout-verified-archive"'
   const manifest={workspace_id:h.fixture.workspace.id,session_id:h.saved.id,state:'ready',generation_id:generation,codec:'gzip-v1',raw_bytes:String(raw.length),raw_records:'180',raw_sha256:hash(raw),compressed_bytes:String(gzip.length),compressed_sha256:hash(gzip),content_path:`/api/v1/workspaces/${h.fixture.workspace.id}/sessions/${h.saved.id}/archive/${generation}/content`}
   await page.route('**/sessions/**/archive**',route=>new URL(route.request().url()).pathname.endsWith('/content')?route.fulfill(route.request().headers()['if-none-match']?{status:304,headers:{ETag:etag}}:{headers:{'Content-Type':'application/gzip',ETag:etag,'Content-Length':String(gzip.length),'X-HMem-Archive-Generation':generation,'X-HMem-Archive-SHA256':hash(gzip),'X-HMem-Archive-Raw-Bytes':String(raw.length),'X-HMem-Archive-Raw-SHA256':hash(raw)},body:gzip}):route.fulfill({json:manifest}))
   await page.route('**/assets/session-archive-worker-*.js*',async route=>{
    if(new URL(route.request().url()).searchParams.get('phase')!=='reader'||injected)return route.continue()
    injected=true;const response=await route.fetch(),source=await response.text()
    const fault="let layoutPages=0,layoutFault=false;const layoutTransaction=IDBDatabase.prototype.transaction;IDBDatabase.prototype.transaction=function(...args){if(layoutFault){layoutFault=false;throw Error('controlled_layout_page_failure')}return layoutTransaction.apply(this,args)};Object.defineProperty(self,'onmessage',{configurable:true,set(handler){self.addEventListener('message',event=>{if(event.data.kind==='page'&&++layoutPages===2)layoutFault=true;handler(event)})}});\n"
    await route.fulfill({response,body:fault+source})
   })
  }else await page.route('**/sessions/**/transcript**',route=>failing?route.fulfill({status:400,json:{error:'controlled_layout_latest_failure'}}):route.fallback())
  await h.start();await page.goto(h.origin+'/workspace/'+h.fixture.workspace.id+'#tab=sessions&session='+(sealed?h.saved.id:h.active.id));await page.locator('.session-record').first().waitFor()
  if(sealed)await page.getByText('Verified complete archive.',{exact:true}).waitFor()
  else{await until(async()=>!(await state(page)).loading);await page.getByRole('button',{name:'Earlier messages',exact:true}).click();await until(async()=>{const value=await state(page);return !value.loading&&value.after<value.latest});failing=true}
  const old=await page.locator('.session-record').evaluateAll(nodes=>nodes.map(node=>node.dataset.blockKey)),jump=page.getByRole('button',{name:'Jump to latest',exact:true})
  await jump.click();await page.getByText('Latest messages could not be reached. Activate Jump to latest to retry.',{exact:true}).waitFor()
  assert.deepEqual(await page.locator('.session-record').evaluateAll(nodes=>nodes.map(node=>node.dataset.blockKey)),old)
  await page.locator('#session-records').evaluate(node=>{node.scrollTop=node.scrollHeight})
  await page.locator('.session-latest-error').evaluate(node=>node.scrollIntoView({block:'start',inline:'nearest'}));await jump.focus();await page.keyboard.press('ArrowLeft');await h.idle()
  const geometry=await jump.evaluate(button=>{
   const host=button.closest('.session-viewer'),records=host.querySelector('#session-records'),b=button.getBoundingClientRect(),r=records.getBoundingClientRect(),style=getComputedStyle(button),overlap=node=>{const a=node.getBoundingClientRect();return a.width>0&&a.height>0&&a.left<b.right&&a.right>b.left&&a.top<b.bottom&&a.bottom>b.top}
   const owners=[];let clipped=false;for(let node=button.parentElement;node;node=node.parentElement){const style=getComputedStyle(node);if(/auto|scroll|hidden/.test(style.overflowY)){const rect=node.getBoundingClientRect();owners.push({class:node.className,top:rect.top,bottom:rect.bottom,client:node.clientHeight,scroll:node.scrollHeight,position:node.scrollTop});if(b.top<rect.top-1||b.bottom>rect.bottom+1)clipped=true}}
   const last=records.querySelector('.session-record:last-child')?.getBoundingClientRect(),alert=host.querySelector('.session-latest-error'),a=alert.getBoundingClientRect(),main=host.closest('.main-content').getBoundingClientRect()
   return {clipped:clipped||b.top<0||b.bottom>innerHeight,center:Math.abs((b.left+b.right-r.left-r.right)/2),inside:b.top>=r.top&&b.bottom<=r.bottom-6,overlap:[...host.querySelectorAll('[role="alert"],.sessions-pagination,.session-archive-footer')].some(overlap),lastClearance:last?b.top-last.bottom:null,recordClient:records.clientHeight,recordScroll:records.scrollHeight,focus:button===document.activeElement,outline:style.outlineWidth,transition:style.transitionDuration,owners,button:b.toJSON(),records:r.toJSON(),alert:{rect:a.toJSON(),visible:a.top>=main.top&&a.bottom<=main.bottom&&owners.every(owner=>a.top>=owner.top-1&&a.bottom<=owner.bottom+1),text:alert.textContent},stamp:{...records.dataset},document:{client:document.documentElement.clientHeight,scroll:document.documentElement.scrollHeight,position:scrollY}}
  })
  if(process.env.SESSION_LATEST_LAYOUT_SCREENSHOTS){const path=join(process.env.SESSION_LATEST_LAYOUT_SCREENSHOTS,`${sealed?'sealed':'active'}-error-${width}.png`);await page.screenshot({path});console.log(JSON.stringify({layout:{sealed,width,geometry,path}}))}
  assert.equal(geometry.alert.visible,true,'failure alert and retained reader stay visible in the same frame');assert.equal(geometry.stamp.latestError,'true');assert.equal(geometry.stamp.latestPending,'false')
  assert.equal(geometry.clipped,false);assert.ok(geometry.center<2&&geometry.inside);assert.equal(geometry.overlap,false);assert.ok(geometry.lastClearance>=0,'reserved record space keeps the last mounted message clear');assert.ok(geometry.focus&&parseFloat(geometry.outline)>=3);assert.equal(geometry.transition,'0s');assert.ok(geometry.recordClient>=120&&geometry.recordClient<900&&geometry.recordScroll>geometry.recordClient,'intrinsic chrome minimum leaves readable space and an independently scrollable bounded transcript')
  assert.equal(await jump.locator('[aria-hidden="true"]').count(),2)
  failing=false;await page.keyboard.press('Enter')
  await until(()=>page.locator('#session-records').evaluate((node,sealed)=>node.dataset.latestPending==='false'&&(sealed?node.dataset.archiveCanNext==='false'&&node.dataset.renderMore==='false':Number(node.dataset.historyAfter)>=Number(node.dataset.historyLatest))&&node.scrollHeight-node.scrollTop-node.clientHeight<=4,sealed));await jump.waitFor({state:'hidden'});assert.deepEqual(h.errors,[])
 }finally{await h.close()}
})

for(const width of [1440,430])test(`sealed presentation controls stay clear of latest navigation at ${width}px`,{timeout:45000},async()=>{
 const raw=Buffer.from(Array.from({length:220},(_,i)=>JSON.stringify({text:`Conversation ${i}\n\nA readable paragraph with **emphasis**, a [reference](https://example.com), and enough text to exercise bounded history. `+'Reading context. '.repeat(12)})+'\n').join(''))
 const h=await openSessionExplorer({transcriptBytes:raw}),{page}=h
 try{
  const hash=bytes=>createHash('sha256').update(bytes).digest('hex'),gzip=gzipSync(raw),generation='33333333-3333-4333-8333-333333333333',etag='"presentation-verified-archive"'
  const manifest={workspace_id:h.fixture.workspace.id,session_id:h.saved.id,state:'ready',generation_id:generation,codec:'gzip-v1',raw_bytes:String(raw.length),raw_records:'220',raw_sha256:hash(raw),compressed_bytes:String(gzip.length),compressed_sha256:hash(gzip),content_path:`/api/v1/workspaces/${h.fixture.workspace.id}/sessions/${h.saved.id}/archive/${generation}/content`}
  await page.route('**/sessions/**/archive**',route=>new URL(route.request().url()).pathname.endsWith('/content')?route.fulfill(route.request().headers()['if-none-match']?{status:304,headers:{ETag:etag}}:{headers:{'Content-Type':'application/gzip',ETag:etag,'Content-Length':String(gzip.length),'X-HMem-Archive-Generation':generation,'X-HMem-Archive-SHA256':hash(gzip),'X-HMem-Archive-Raw-Bytes':String(raw.length),'X-HMem-Archive-Raw-SHA256':hash(raw)},body:gzip}):route.fulfill({json:manifest}))
  await page.setViewportSize({width,height:width===430?720:900});await h.start();await page.goto(h.origin+'/workspace/'+h.fixture.workspace.id+'#tab=sessions&session='+h.saved.id);await page.getByRole('button',{name:'Continue presentation',exact:true}).waitFor()
  await page.locator('#session-records').evaluate(node=>{node.scrollTop=node.scrollHeight})
  const jump=page.getByRole('button',{name:'Jump to latest',exact:true});await jump.evaluate(node=>node.scrollIntoView({block:'center',inline:'nearest'}));await jump.focus();await h.idle()
  const geometry=await jump.evaluate(button=>{const b=button.getBoundingClientRect(),host=button.closest('.session-viewer'),records=host.querySelector('#session-records'),r=records.getBoundingClientRect(),controls=[...host.querySelectorAll('.session-render-continuation,.session-archive-footer')].map(node=>node.getBoundingClientRect());return {inside:b.top>=r.top&&b.bottom<=r.bottom-6,visible:b.top>=0&&b.bottom<=innerHeight,overlap:controls.some(a=>a.top<b.bottom&&a.bottom>b.top&&a.left<b.right&&a.right>b.left),height:records.clientHeight,scroll:records.scrollHeight}})
  assert.ok(geometry.inside&&geometry.visible);assert.equal(geometry.overlap,false);assert.ok(geometry.height>=120&&geometry.height<900&&geometry.scroll>geometry.height)
  await page.getByRole('button',{name:'Continue presentation',exact:true}).focus();await page.keyboard.press('Enter')
  await until(()=>page.locator('#session-records').evaluate(node=>Number(node.dataset.renderPart)>0&&node.scrollTop<=4));assert.ok(await page.locator('.session-record').count()<=64)
  await jump.focus();await page.keyboard.press('Enter');await jump.waitFor({state:'hidden'});assert.deepEqual(h.errors,[])
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
