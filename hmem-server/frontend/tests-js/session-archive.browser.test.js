import test from 'node:test'
import assert from 'node:assert/strict'
import {gzipSync,gunzipSync,createGzip,constants as zlibConstants} from 'node:zlib'
import {createHash,randomUUID} from 'node:crypto'
import {readFile,readdir,mkdir,unlink,rmdir,writeFile,rename,rm,lstat,realpath} from 'node:fs/promises'
import {createReadStream,createWriteStream} from 'node:fs'
import {resolve,sep} from 'node:path'
import {createServer} from 'node:http'
import {Transform} from 'node:stream'
import {pipeline} from 'node:stream/promises'
import {once,EventEmitter} from 'node:events'
import os from 'node:os'
import {openSessionExplorer,continuousScrollStep} from './support/sessions-ux-fixture.mjs'
import {attributedProcessTotals,memorySnapshot,privateFootprint} from './support/session-archive-memory.mjs'
const hash=bytes=>createHash('sha256').update(bytes).digest('hex')
const hashFile=async path=>{const digest=createHash('sha256');for await(const bytes of createReadStream(path,{highWaterMark:65536}))digest.update(bytes);return digest.digest('hex')}
async function renderedLinks(h,destination,expected,kind='plain'){
 const page=h.page,viewport=page.locator('#session-records'),seen=new Map(),counts={1:0,2:0,3:0},slices=new Set(),progress=[];let turns=0,maxBytes=0,previous='',stalled=0,middleVisible=kind!=='table'
 await page.locator('#session-records a').first().waitFor();await h.idle()
 await viewport.evaluate((node,{destination,expected})=>{const rows=new Map(),pending=[],parts=new Set();window.archiveLinkWitness={pending,parts,error:null};const collect=()=>{parts.add(node.dataset.renderPart);for(const link of node.querySelectorAll('a')){const leaf=link.querySelector('[data-fragment-path]')||link.closest('[data-fragment-path]'),column=link.closest('td')?.getAttribute('aria-colindex'),value={key:leaf.dataset.fragmentPath+':'+Math.max(0,[...leaf.querySelectorAll('a')].indexOf(link)),ordinal:link.closest('li')?.value,column,header:column?node.querySelector('thead th[aria-colindex="'+column+'"]')?.textContent:null};if(link.getAttribute('href')!==destination)window.archiveLinkWitness.error='resolved href changed';const serialized=JSON.stringify(value);if(rows.has(value.key)){if(rows.get(value.key)!==serialized)window.archiveLinkWitness.error='keyed link context changed'}else{rows.set(value.key,serialized);pending.push(value)}if(rows.size>expected)window.archiveLinkWitness.error='unexpected source occurrence'}};window.collectArchiveLinks=collect;window.archiveLinkObserver=new MutationObserver(collect);window.archiveLinkObserver.observe(node,{childList:true,subtree:true});collect()},{destination,expected})
 await viewport.press('Home');await page.waitForFunction(()=>{const node=document.getElementById('session-records');return node.scrollTop<=1&&node.querySelector('[data-fragment-part="0"]')})
 while(seen.size<expected||!middleVisible){
  const state=await viewport.evaluate(node=>{window.collectArchiveLinks();return {bytes:new TextEncoder().encode(node.outerHTML).length,links:window.archiveLinkWitness.pending.splice(0),parts:[...window.archiveLinkWitness.parts],error:window.archiveLinkWitness.error}})
  assert.equal(state.error,null);for(const part of state.parts)slices.add(part)
  for(const link of state.links){if(seen.has(link.key))assert.deepEqual(seen.get(link.key),link);else{seen.set(link.key,link);if(kind==='list'){assert.ok([1,2,3].includes(link.ordinal));counts[link.ordinal]++}if(kind==='table'){assert.equal(link.column,'2');assert.equal(link.header,'Column B')}}}
  maxBytes=Math.max(maxBytes,state.bytes);assert.ok(state.bytes<=2097152);assert.ok(state.bytes+await page.evaluate(()=>window.archiveTelemetry?.maxPageBytes||0)<=4194304)
  if(kind==='table'&&!middleVisible){
   middleVisible=await viewport.evaluate(node=>{const box=node.getBoundingClientRect(),link=[...node.querySelectorAll('a')].find(link=>(link.querySelector('[data-fragment-path]')||link.closest('[data-fragment-path]'))?.dataset.fragmentPath.includes(':body:0:1:250:'));if(!link)return false;const r=link.getBoundingClientRect(),cell=link.closest('td'),c=cell?.getBoundingClientRect(),visible=r.height>0&&r.bottom>Math.max(box.top,c.top)&&r.top<Math.min(box.bottom,c.bottom)&&r.right>Math.max(box.left,c.left)&&r.left<Math.min(box.right,c.right)&&cell?.getAttribute('aria-colindex')==='2'&&node.querySelector('thead th[aria-colindex="2"]')?.textContent==='Column B';if(visible)window.middleTableReading={node:link,top:r.top-box.top};return visible})
   if(middleVisible){await h.idle();await viewport.evaluate(async node=>{for(let frame=0;frame<4;frame++)await new Promise(resolve=>requestAnimationFrame(resolve));const witness=window.middleTableReading;if(!witness.node.isConnected||Math.abs(witness.node.getBoundingClientRect().top-node.getBoundingClientRect().top-witness.top)>3)throw Error('The actual middle table link lost its settled reading position')})}
  }
  if(seen.size===expected&&middleVisible)break
  const fingerprint=await viewport.evaluate(node=>[node.dataset.renderPart,Math.round(node.scrollTop)].join(':'));stalled=fingerprint===previous?stalled+1:0;assert.ok(stalled<6,JSON.stringify({phase:'continuous-render-stall',kind,seen:seen.size,expected,slices:[...slices].slice(-8),progress}));previous=fingerprint
  const trace=await viewport.evaluate(node=>({part:node.dataset.renderPart,request:node.dataset.renderRequest,top:Math.round(node.scrollTop),height:node.clientHeight,total:node.scrollHeight,gaps:[...node.querySelectorAll('[data-fragment-from]')].map(gap=>({from:gap.dataset.fragmentFrom,to:gap.dataset.fragmentTo,top:Math.round(gap.getBoundingClientRect().top-node.getBoundingClientRect().top),bottom:Math.round(gap.getBoundingClientRect().bottom-node.getBoundingClientRect().top)}))}));slices.add(trace.part);progress.push({turns,seen:seen.size,...trace});if(progress.length>6)progress.shift()
  // The legacy whole-budget bound is independently retained in SessionTranscriptTest.
  // Continuous presentation has up to three overlapping slices per mounted window.
  assert.ok(++turns<=300,JSON.stringify({phase:'continuous-slice-progress',expected,seen:seen.size,distinctSlices:slices.size,firstKeys:[...seen.keys()].slice(0,2),lastKeys:[...seen.keys()].slice(-2),progress}));await continuousScrollStep(page,1)
 }
 if(kind==='table'){
  assert.equal(await page.evaluate(()=>window.middleTableReading.node.isConnected),false,'bounded forward traversal actually evicts the old middle link')
  while(!await viewport.evaluate(node=>{const box=node.getBoundingClientRect(),link=[...node.querySelectorAll('a')].find(link=>(link.querySelector('[data-fragment-path]')||link.closest('[data-fragment-path]'))?.dataset.fragmentPath.includes(':body:0:1:250:'));if(!link)return false;const r=link.getBoundingClientRect();return link!==window.middleTableReading.node&&r.height>0&&r.bottom>box.top&&r.top<box.bottom&&r.right>box.left&&r.left<box.right})){
   assert.ok(++turns<=300,'evicted middle table content remains reachable within the same finite input bound');await continuousScrollStep(page,-1)
  }
 }
 await page.evaluate(()=>{window.archiveLinkObserver.disconnect();delete window.archiveLinkObserver;delete window.archiveLinkWitness;delete window.collectArchiveLinks;delete window.middleTableReading});assert.equal(seen.size,expected);return {turns,maxBytes,counts}
}

const phaseArchiveETag='"fixture-authority-bound-archive"'
const phaseArchiveRoute=handler=>route=>handler(new Proxy(route,{get(target,key){
  if(key==='fulfill')return options=>{
    const content=new URL(target.request().url()).pathname.endsWith('/content'),gzip=(options.headers?.['Content-Type']||'').startsWith('application/gzip')
    if(content&&gzip&&(!options.status||options.status===200)){
      const conditional=target.request().headers()['if-none-match']
      if(conditional){assert.equal(conditional,phaseArchiveETag);return target.fulfill({status:304,headers:{ETag:phaseArchiveETag}})}
      return target.fulfill({...options,headers:{...options.headers,ETag:phaseArchiveETag}})
    }
    return target.fulfill(options)
  }
  const value=Reflect.get(target,key);return typeof value==='function'?value.bind(target):value
}}))

const generation='33333333-3333-4333-8333-333333333333'
const makeManifest=(h,raw,gzip)=>({workspace_id:h.fixture.workspace.id,session_id:h.saved.id,state:'ready',generation_id:generation,codec:'gzip-v1',raw_bytes:String(raw.length),raw_records:String(raw.length?raw.toString('utf8').split('\n').filter(Boolean).length:0),raw_sha256:hash(raw),compressed_bytes:String(gzip.length),compressed_sha256:hash(gzip),content_path:`/api/v1/workspaces/${h.fixture.workspace.id}/sessions/${h.saved.id}/archive/${generation}/content`})
const workerAsset=async()=>'/assets/'+(await readdir('../static/assets')).find(name=>/^session-archive-worker-.*\.js$/.test(name))
const profileScratch=()=>resolve(process.env.HMEM_ARCHIVE_PROFILE_SCRATCH || '../../.tmp/sealed-archives-e0ac6260')

test('sanitized viewer labels and downloads the selected content receipt',{timeout:45000},async()=>{
 const original=Buffer.from('{"text":"opaque original content"}\r\n{"text":"tail"}')
 const content=Buffer.from('{"text":"Encrypted Payload"}\r\n{"text":"tail"}'),gzip=gzipSync(content)
 const h=await openSessionExplorer({transcriptBytes:original}),{page}=h
 try{
  await page.addInitScript(()=>{
   const set=window.setTimeout.bind(window),clear=window.clearTimeout.bind(window),longTimers=new WeakMap()
   const add=window.addEventListener.bind(window),remove=window.removeEventListener.bind(window),owned=new WeakSet()
   const removeEntry=FileSystemDirectoryHandle.prototype.removeEntry
   window.sanitizedCleanup={added:0,removed:0,attempts:0}
   window.setTimeout=(handler,delay,...args)=>{
    const timer=set(handler,delay,...args)
    if(delay===60000&&typeof handler==='function')longTimers.set(handler,timer)
    return timer
   }
   window.addEventListener=(kind,handler,options)=>{
    if(kind==='pagehide'&&longTimers.has(handler)){
     clear(longTimers.get(handler));owned.add(handler);window.sanitizedCleanup.added++;set(handler,1500)
    }
    add(kind,handler,options)
   }
   window.removeEventListener=(kind,handler,options)=>{
    if(kind==='pagehide'&&owned.has(handler)){owned.delete(handler);window.sanitizedCleanup.removed++}
    remove(kind,handler,options)
   }
   FileSystemDirectoryHandle.prototype.removeEntry=async function(name,options){
    if(name.startsWith('hmem-download-')&&++window.sanitizedCleanup.attempts===1)
     throw new DOMException('fixture removal failure','NoModificationAllowedError')
    return removeEntry.call(this,name,options)
   }
  })
  const exact=makeManifest(h,original,gzip),root=exact.content_path.slice(0,-8)
  const manifest={...exact,representation:'sanitized-v1',content_bytes:String(content.length),content_sha256:hash(content),
    policy_sha256:'a'.repeat(64),original_available:false,content_path:root+'/sanitized-content',evidence_path:root+'/sanitized-evidence'}
  const headers={'Content-Type':'application/gzip','Content-Length':String(gzip.length),ETag:phaseArchiveETag,
    'X-HMem-Archive-Generation':generation,'X-HMem-Archive-SHA256':hash(gzip),'X-HMem-Archive-Raw-Bytes':String(original.length),
    'X-HMem-Archive-Raw-SHA256':hash(original),'X-HMem-Archive-Representation':'sanitized-v1',
    'X-HMem-Archive-Content-Bytes':String(content.length),'X-HMem-Archive-Content-SHA256':hash(content)}
  await page.route('**/sessions/**/archive**',route=>{
   const request=route.request(),url=new URL(request.url())
   if(url.pathname.endsWith('/sanitized-content'))return request.headers()['if-none-match']
     ? route.fulfill({status:304,headers}):route.fulfill({headers,body:gzip})
   if(url.pathname.endsWith('/sanitized-evidence')){
    assert.equal(url.searchParams.get('offset'),'0')
    return route.fulfill({json:{workspace_id:manifest.workspace_id,session_id:manifest.session_id,generation_id:generation,
      representation:'sanitized-v1',policy_sha256:manifest.policy_sha256,content_bytes:manifest.content_bytes,
      content_sha256:manifest.content_sha256,offset:0,next_offset:content.length,has_more:false,data_base64:content.toString('base64')}})
   }
   assert.match(request.headers().accept,/representation=available-v1/)
   return route.fulfill({json:manifest})
  })
  await h.start();await h.idle();await page.goto(h.origin+'/workspace/'+h.fixture.workspace.id+'#tab=sessions&session='+h.saved.id)
  await page.getByText('Verified sanitized transcript.',{exact:true}).waitFor()
  await page.getByText('Encrypted Payload',{exact:true}).waitFor()
  assert.equal(await page.getByText('opaque original content',{exact:true}).count(),0)
  const downloading=page.waitForEvent('download')
  await page.getByRole('button',{name:'↓ Download sanitized JSONL',exact:true}).click()
  const download=await downloading
  assert.equal(download.suggestedFilename(),h.saved.id+'-sanitized.jsonl')
  const parts=[]
  for await(const bytes of await download.createReadStream())parts.push(bytes)
  assert.deepEqual(Buffer.concat(parts),content)
  await page.getByText('Temporary sanitized download cleanup is blocked. Retry cleanup before downloading again.',{exact:true}).waitFor()
  await page.getByRole('button',{name:'↓ Download sanitized JSONL',exact:true}).click()
  await page.getByText('Temporary download removed. Download again to save a new copy.',{exact:true}).waitFor()
  assert.deepEqual(await page.evaluate(()=>window.sanitizedCleanup),{added:1,removed:1,attempts:2})
  assert.deepEqual(h.errors,[])
 }finally{await h.close()}
})

test('Jump to latest reaches a verified final archive page and its actual last formatted part',{timeout:45000},async()=>{
 const destination='https://example.test/'+'x'.repeat(12000),markdown=Array.from({length:800},(_,i)=>`[Final occurrence ${i}][late]`).join(' ')+'\n\n[late]: '+destination
 const raw=Buffer.from([...Array.from({length:180},(_,i)=>({text:`Archive occurrence ${i}`})),{text:markdown}].map(record=>JSON.stringify(record)+'\n').join(''))
 const h=await openSessionExplorer({transcriptBytes:raw}),{page}=h
 try{
  const gzip=gzipSync(raw),manifest=makeManifest(h,raw,gzip),reads=[]
  await page.addInitScript(captureArchiveTelemetry)
  await page.route('**/sessions/**/archive**',phaseArchiveRoute(route=>{reads.push(route.request().url());return new URL(route.request().url()).pathname.endsWith('/content')?route.fulfill({headers:{'Content-Type':'application/gzip','Content-Length':String(gzip.length),'X-HMem-Archive-Generation':generation,'X-HMem-Archive-SHA256':hash(gzip),'X-HMem-Archive-Raw-Bytes':String(raw.length),'X-HMem-Archive-Raw-SHA256':hash(raw)},body:gzip}):route.fulfill({json:manifest})}))
  await h.start();await h.idle();await page.goto(h.origin+'/workspace/'+h.fixture.workspace.id+'#tab=sessions&session='+h.saved.id)
  await page.getByRole('link',{name:'Final occurrence 799',exact:true}).waitFor();const viewport=page.locator('#session-records'),jump=page.getByRole('button',{name:'Jump to latest',exact:true})
  await viewport.press('Home');await jump.waitFor({state:'visible'});await page.getByText('Archive occurrence 0',{exact:true}).waitFor()
  const before=reads.length;await jump.focus();await page.keyboard.press('Enter')
  await page.waitForFunction(()=>{const node=document.getElementById('session-records');return node.dataset.latestPending==='false'&&node.dataset.archiveCanNext==='false'&&node.dataset.renderMore==='false'&&node.dataset.lastProseMore==='false'&&Number(node.dataset.renderPart)>0&&node.scrollHeight-node.scrollTop-node.clientHeight<=4})
  await jump.waitFor({state:'hidden'});await page.getByRole('link',{name:'Final occurrence 799',exact:true}).waitFor()
  assert.equal(reads.length,before,'verified local latest navigation makes no additional archive HTTP request')
  const metric=await page.evaluate(()=>({wire:window.archiveTelemetry.maxPageBytes,body:window.archiveTelemetry.maxBodyBytes,dom:new TextEncoder().encode(document.getElementById('session-records').outerHTML).length}))
  assert.ok(metric.body<=65536);assert.ok(metric.wire+Buffer.byteLength(JSON.stringify(markdown))+metric.dom<=4194304)
  assert.deepEqual(h.errors,[])
 }finally{await h.close()}
})

test('sealed boundary scrolling continuously admits consecutive render slices with stamped positioning',{timeout:45000},async()=>{
 const destination='https://example.test/'+'x'.repeat(12000),markdown=Array.from({length:800},(_,i)=>`[Occurrence ${i}][r]`).join(' ')+'\n\n[r]: '+destination
 const h=await openSessionExplorer({transcriptBytes:Buffer.from(JSON.stringify({text:markdown})+'\n')}),{page}=h
 const view=()=>page.locator('#session-records').evaluate(node=>({part:Number(node.dataset.renderPart),request:Number(node.dataset.renderRequest),top:node.scrollTop,bottom:node.scrollHeight-node.scrollTop-node.clientHeight,occurrences:[...node.querySelectorAll('a')].map(link=>Number(link.textContent.split(' ')[1]))}))
 try{
  const raw=h.content.get(h.saved.id),gzip=gzipSync(raw),manifest=makeManifest(h,raw,gzip)
  await page.route('**/sessions/**/archive**',phaseArchiveRoute(route=>new URL(route.request().url()).pathname.endsWith('/content')?route.fulfill({headers:{'Content-Type':'application/gzip','Content-Length':String(gzip.length),'X-HMem-Archive-Generation':generation,'X-HMem-Archive-SHA256':hash(gzip),'X-HMem-Archive-Raw-Bytes':String(raw.length),'X-HMem-Archive-Raw-SHA256':hash(raw)},body:gzip}):route.fulfill({json:manifest})))
  await h.start();await h.idle();await page.goto(h.origin+'/workspace/'+h.fixture.workspace.id+'#tab=sessions&session='+h.saved.id)
  await page.getByText('Verified complete archive.',{exact:true}).waitFor()
  await page.waitForFunction(()=>document.querySelector('#session-records a')?.getAttribute('href')?.length>12000)
  const viewport=page.locator('#session-records');await viewport.press('Home');await page.waitForFunction(()=>document.getElementById('session-records').dataset.renderPart==='0')
  const visited=new Set((await view()).occurrences),parts=new Set([0]);let previous=await view()
  for(let steps=0;previous.part<3;steps++){
   assert.ok(steps<30);await continuousScrollStep(page,1);await h.idle();const current=await view();assert.ok(current.part<=previous.part+2,'overlapping windows admit every intervening slice');if(current.part!==previous.part)assert.ok(current.request>previous.request)
   parts.add(current.part);for(const occurrence of current.occurrences)visited.add(occurrence);previous=current
  }
  assert.ok(parts.size>=3);assert.deepEqual([...visited].sort((a,b)=>a-b),Array.from({length:visited.size},(_,i)=>i),'overlapping mounted slices contain every expected occurrence without holes')
  await viewport.press('Home');await page.waitForFunction(()=>{const node=document.getElementById('session-records');return node.scrollTop<=1&&node.querySelector('[data-fragment-part="0"]')})
  assert.equal(await page.getByRole('button',{name:/^(Earlier messages|Newer messages|First|Previous|Next|Last|Continue presentation|Previous presentation)$/}).count(),0)
  assert.deepEqual(h.errors,[])
 }finally{await h.close()}
})

for(const jumpLatest of [false,true])test('failed native reader page retries through closed handles, revalidation and a fresh reader'+(jumpLatest?' via Jump to latest':''),{timeout:45000},async()=>{
 const raw=Buffer.from(Array.from({length:180},(_,i)=>JSON.stringify({text:`Retry occurrence ${i}`})+'\n').join(''))
 const h=await openSessionExplorer({transcriptBytes:raw}),conditional=[];let injected=false,readerIncarnations=0
 try{
  await h.page.route('**/assets/session-archive-worker-*.js*',async route=>{
   const url=new URL(route.request().url())
   if(url.searchParams.get('phase')!=='reader')return route.continue()
   readerIncarnations++
   if(injected)return route.continue();injected=true
   const response=await route.fetch(),source=await response.text()
   const prelude="let hmemRetryPages=0,hmemRetryFault=false;const hmemRetryTransaction=IDBDatabase.prototype.transaction;IDBDatabase.prototype.transaction=function(...args){if(hmemRetryFault){hmemRetryFault=false;throw Error('controlled_reader_page_failure')}return hmemRetryTransaction.apply(this,args)};Object.defineProperty(self,'onmessage',{configurable:true,set(handler){self.addEventListener('message',event=>{if(event.data.kind==='page'&&++hmemRetryPages===2)hmemRetryFault=true;handler(event)})}});\n"
   await route.fulfill({response,body:prelude+source})
  })
  const gzip=gzipSync(raw),manifest=makeManifest(h,raw,gzip)
  await h.page.route('**/sessions/**/archive**',phaseArchiveRoute(async route=>{
   if(new URL(route.request().url()).pathname.endsWith('/content')){
    if(route.request().headers()['if-none-match'])conditional.push(route.request().headers()['if-none-match'])
    await route.fulfill({headers:{'Content-Type':'application/gzip','Content-Length':String(gzip.length),'X-HMem-Archive-Generation':generation,'X-HMem-Archive-SHA256':hash(gzip),'X-HMem-Archive-Raw-Bytes':String(raw.length),'X-HMem-Archive-Raw-SHA256':hash(raw)},body:gzip})
   }else await route.fulfill({json:manifest})
  }))
  await h.start();await h.idle();await h.page.goto(h.origin+'/workspace/'+h.fixture.workspace.id+'#tab=sessions&session='+h.saved.id)
  await h.page.getByText('Retry occurrence 179',{exact:true}).waitFor();await h.idle()
  const admitted=await h.page.locator('#session-records').evaluate(node=>[node.dataset.sealedFirst,node.dataset.sealedLast])
  await h.page.locator('#session-records').press('Home')
  await h.page.getByText('controlled_reader_page_failure',{exact:true}).waitFor()
  assert.deepEqual(await h.page.locator('#session-records').evaluate(node=>[node.dataset.sealedFirst,node.dataset.sealedLast]),admitted,'failed acquisition preserves the admitted source interval even when real Home changes its mounted slice')
  assert.ok(await h.page.locator('.session-record').evaluateAll((nodes,[first,last])=>nodes.every(node=>node.dataset.blockKey>=first&&node.dataset.blockKey<=last),admitted))
  await h.page.getByRole('button',{name:jumpLatest?'Jump to latest':'Retry',exact:true}).click()
  if(jumpLatest)await h.page.getByText('Retry occurrence 179',{exact:true}).waitFor()
  else await h.page.waitForFunction(first=>document.getElementById('session-records').dataset.sealedFirst<first,admitted[0])
  assert.equal(readerIncarnations,2);assert.deepEqual(conditional,[phaseArchiveETag,phaseArchiveETag])
  assert.ok(await h.page.locator('.session-record').count()<=200)
  assert.deepEqual(h.errors,[])
 }finally{await h.close()}
})


const retainedFamilySha256='488805c6ff29f021b5f21ff3f9a14e632439f402b33f73a138c33b22d4f9102b'
const retainedFamilyBytes=825435
function encodeQualificationFamily(bytes) {
  assert.ok(Buffer.isBuffer(bytes)&&bytes.length===retainedFamilyBytes&&bytes.length<=1048576,'one bounded approved evidence family')
  assert.equal(hash(bytes),retainedFamilySha256,'exact immutable family beforeimage')
  const original=JSON.parse(bytes.toString('utf8'))
  assert.equal(original.schemaVersion,2);assert.ok(!original.retainedFamily,'no nested or repeated evidence family')
  assert.equal(original.attempts.length,8)
  const compressed=gzipSync(bytes,{level:9})
  return {codec:'gzip-base64-v1',sourceSha256:retainedFamilySha256,sourceBytes:bytes.length,canonicalSha256:hash(JSON.stringify(original)),gzipSha256:hash(compressed),gzipBytes:compressed.length,attemptCount:8,attemptObjectSha256:original.attempts.map(value=>hash(JSON.stringify(value))),matchedObjectSha256:hash(JSON.stringify(original.matchedActiveLive)),owner:original.owner,retention:original.retention,data:compressed.toString('base64')}
}
function decodeQualificationFamily(family) {
  assert.ok(family&&family.codec==='gzip-base64-v1','supported fixed evidence family codec')
  assert.equal(family.sourceSha256,retainedFamilySha256);assert.equal(family.sourceBytes,retainedFamilyBytes)
  assert.ok(Number.isSafeInteger(family.gzipBytes)&&family.gzipBytes>0&&family.gzipBytes<=1048576,'bounded compressed family')
  assert.ok(typeof family.data==='string'&&family.data.length<=1398104,'bounded encoded family')
  const compressed=Buffer.from(family.data,'base64')
  assert.equal(compressed.toString('base64'),family.data,'canonical base64 family')
  assert.equal(compressed.length,family.gzipBytes);assert.equal(hash(compressed),family.gzipSha256)
  const bytes=gunzipSync(compressed,{maxOutputLength:retainedFamilyBytes})
  assert.equal(bytes.length,retainedFamilyBytes);assert.equal(hash(bytes),retainedFamilySha256)
  const original=JSON.parse(bytes.toString('utf8'))
  assert.equal(original.schemaVersion,2);assert.ok(!original.retainedFamily,'no nested or repeated evidence family')
  assert.equal(original.taskId,'e0ac6260-641c-4891-b4c1-cc3694f31158');assert.equal(original.attempts.length,8);assert.equal(family.attemptCount,8)
  assert.equal(hash(JSON.stringify(original)),family.canonicalSha256)
  assert.deepEqual(original.attempts.map(value=>hash(JSON.stringify(value))),family.attemptObjectSha256)
  assert.equal(hash(JSON.stringify(original.matchedActiveLive)),family.matchedObjectSha256)
  assert.equal(family.owner,original.owner);assert.equal(family.retention,original.retention)
  return {bytes,original}
}
function qualificationFamilyCandidate(bytes) {
  const family=encodeQualificationFamily(bytes),{original}=decodeQualificationFamily(family)
  const candidate={...original,schemaVersion:3,attempts:[],retainedFamily:family}
  assert.ok(Buffer.byteLength(JSON.stringify(candidate,null,2)+'\n')<=1048576,'bounded current evidence candidate')
  return candidate
}

async function readQualificationEvidence(path=resolve('perf/session-archive.qualification.json')) {
  assert.equal(path,resolve('perf/session-archive.qualification.json'))
  const current=await lstat(path);assert.ok(current.isFile()&&!current.isSymbolicLink()&&current.size<=1048576,'bounded current qualification file')
  assert.equal(await realpath(path),path,'current qualification containment')
  const evidence=JSON.parse(await readFile(path,'utf8'))
  assert.equal(evidence.taskId,'e0ac6260-641c-4891-b4c1-cc3694f31158')
  if(evidence.schemaVersion===3){
    const {original}=decodeQualificationFamily(evidence.retainedFamily)
    for(const key of Object.keys(original))if(!['schemaVersion','attempts','matchedActiveLive','diagnostics'].includes(key))assert.deepEqual(evidence[key],original[key],'retained family metadata '+key)
    assert.ok(Array.isArray(evidence.attempts)&&evidence.attempts.length<=8,'bounded successor active attempts')
    assert.ok(Array.isArray(evidence.diagnostics)&&evidence.diagnostics.length<=4,'bounded append-only diagnostics');assert.deepEqual(evidence.diagnostics.slice(0,original.diagnostics.length),original.diagnostics,'retained diagnostic prefix')
  }else assert.ok(!evidence.retainedFamily,'family requires schema3')

  assert.ok([1,2,3].includes(evidence.schemaVersion),'supported qualification schema')
  if(evidence.schemaVersion===2||evidence.schemaVersion===3)assert.ok(evidence.historyRef,'schema2 requires its immutable history reference')
  if(evidence.historyRef){
    const ref=evidence.historyRef;assert.equal(ref.path,'session-archive.historical.json','one fixed sibling history')
    assert.deepEqual(evidence.custody,{files:2,maxFileBytes:1048576,maxTotalBytes:2097152})
    assert.ok(/^[a-f0-9]{64}$/.test(ref.sha256))
    const historicalPath=resolve('perf',ref.path),info=await lstat(historicalPath)
    assert.ok(info.isFile()&&!info.isSymbolicLink()&&info.size<=1048576,'bounded immutable history file')
    assert.equal(await realpath(historicalPath),historicalPath,'historical qualification containment')
    assert.equal(info.size,ref.bytes);assert.ok(current.size+info.size<=2097152)
    const bytes=await readFile(historicalPath);assert.equal(hash(bytes),ref.sha256,'immutable historical qualification bytes')
    const historical=JSON.parse(bytes);assert.equal(historical.taskId,evidence.taskId);assert.equal(historical.historyRef,undefined,'no history chains')
    assert.deepEqual(ref.counts,{attempts:historical.attempts.length,historicalVectors:historical.historicalVectors?.attempts.length || 0,diagnostics:historical.diagnostics?.length || 0})
    assert.ok(ref.counts.attempts<=8&&ref.counts.historicalVectors<=6&&ref.counts.diagnostics<=4)
  }
  return evidence
}

test('requested memory dump selection rejects ambiguous attribution, detail and PID coverage',()=>{
  const actual=[{id:1,type:'browser'},{id:2,type:'renderer'}],guid='0x2',detail='background'
  const source=[{name:'GlobalMemoryDump',ph:'b',pid:1,id2:{local:'0x7'},scope:'memory',ts:10,args:{dump_guid:guid,success:true,dump_type:'explicitly_triggered',level_of_detail:detail}},{name:'GlobalMemoryDump',ph:'e',pid:1,id2:{local:'0x7'},scope:'memory',ts:30},...[1,2].map(pid=>({name:'ProcessMemoryDump',ph:'b',pid,ts:15,args:{dump_guid:guid}})),...[1,2].map(pid=>({ph:'v',id:'0x0',pid,ts:20,args:{dumps:{level_of_detail:detail,process_totals:{private_footprint_bytes:'10'}}}}))]
  const result=attributedProcessTotals(source,guid,actual,detail)
  assert.equal(result.attribution.requestGuid,'0x2');assert.equal(result.attribution.exportRowId,'0x0')
  assert.deepEqual(result.processes.map(value=>value.exportRowId),['0x0','0x0'])
  assert.equal(privateFootprint(result.processes),32)
  assert.deepEqual(result.processes.map(value=>value.privateBytes),[16,16])
  const footprint=value=>({totals:{private_footprint_bytes:value}})
  for(const value of ['','nope','10x','-10','0x10','1.0',' 10','20000000000000',42])assert.throws(()=>privateFootprint([footprint(value)]),/private footprint/)
  assert.throws(()=>privateFootprint([footprint('1fffffffffffff'),footprint('1')]),/safe total/)
  assert.throws(()=>privateFootprint([footprint('0')]),/positive/)
  const invalid=[
    events=>{delete events[0].args.dump_guid},events=>{events[0].args.dump_guid='0x1'},
    events=>events.pop(),events=>events.push(structuredClone(events.at(-1))),
    events=>{events.at(-1).id='0x1'},events=>{events.at(-1).ts=21},
    events=>{for(const event of events.filter(value=>value.ph==='v'))event.ts=31},
    events=>{events.at(-1).args.dumps.level_of_detail='detailed'},
    events=>{events[0].args.level_of_detail='detailed'},events=>{events[0].args.success=false},
    events=>{events[0].args.dump_type='periodic_interval'},events=>events.splice(1,1),
    events=>{events[1].id2.local='0x8'},events=>{events[1].pid=2},
    events=>{events[1].scope='other'},events=>{events[1].ts=9},
    events=>events.push(structuredClone(events[0])),events=>{events[2].args.dump_guid='0x1'},
    events=>events.splice(2,1),events=>{events[2].ts=9},
    events=>{delete events.at(-1).id},events=>{events.at(-1).pid=1}
  ]
  for(const [index,mutate]of invalid.entries()){const events=structuredClone(source);mutate(events);assert.throws(()=>attributedProcessTotals(events,guid,actual,detail),undefined,'invalid causal input '+index)}
})

// A stuck diagnostic cannot keep the owned fixture alive indefinitely. The
// caller's finally closes the actual BrowserServer and HTTP handles on expiry.
const boundedMemorySnapshot=(h,options)=>h.bounded(memorySnapshot(h.page,options),10000,'Native memory snapshot')

// Diagnostic-only isolate counters. Browser-process private memory remains the
// acceptance metric; these fields neither replace nor subtract from that metric.

function validateArchiveWorkerTopology(targets,epochs) {
  assert.ok(Array.isArray(epochs)&&epochs.length<=2,'at most two explicit archive incarnations')
  for(const[index,epoch]of epochs.entries()){assert.equal(epoch.phase,index===0?'processor':'reader');assert.match(epoch.incarnation,/^[a-f0-9-]{36}$/);if(index)assert.ok(epochs[0].detached&&epochs[0].terminated,'reader requires joined processor termination invocation')}
  if(epochs.length===2)assert.notEqual(epochs[0].incarnation,epochs[1].incarnation,'distinct incarnations')
  assert.ok(targets.length<=2,'only the two known physical target epochs may be visible')
  for(const target of targets){const url=new URL(target.url);assert.match(url.pathname,/\/assets\/session-archive-worker-[^/]+\.js$/);assert.ok(epochs.some(epoch=>epoch.phase===url.searchParams.get('phase')&&epoch.incarnation===url.searchParams.get('incarnation')),'every target has explicit phase lineage')}
  const admitted=epochs.filter(epoch=>!epoch.terminated);assert.ok(admitted.length<=1,'one admitted resource Worker')
  return admitted
}
function selectArchiveWorkerEpoch(targets,epochs) {
  const admitted=validateArchiveWorkerTopology(targets,epochs)
  if(!admitted.length){if(!epochs.length)return {target:null};const old=epochs.at(-1);assert.ok(old.phase==='processor'&&old.detached&&old.terminated,'only the explicit processor-reader gap may omit a Worker');return {target:null,transition:{phase:'processor-detached-awaiting-reader',incarnation:old.incarnation}}}
  const epoch=admitted[0],matching=targets.filter(target=>new URL(target.url).searchParams.get('incarnation')===epoch.incarnation)
  assert.equal(matching.length,1,'the admitted incarnation has exactly one target')
  return {target:matching[0],epoch}
}

const retiredTargetProtocolError=error=>/No target with given id found|No session with given id|Target closed|Session closed/.test(String(error?.message || ''))
function provedProcessorTransition(topology,proof) {
  const admitted=validateArchiveWorkerTopology(topology.targets,topology.lineage),processor=topology.lineage[0]
  return !!processor&&processor.phase==='processor'&&processor.detached&&processor.terminated
    &&(!proof||(proof.phase==='processor'&&proof.incarnation===processor.incarnation))
    &&(!admitted.length||admitted[0].phase==='reader')
}
function staleProcessorTargetPair(topology) {
  if(!provedProcessorTransition(topology))return false
  const reader=topology.lineage[1]
  return !!reader&&!reader.terminated&&!topology.targets.some(target=>new URL(target.url).searchParams.get('incarnation')===reader.incarnation)
}

test('ordered auxiliary Worker epochs never relabel targets or require retired processor identity',()=>{
  const first={phase:'processor',incarnation:'11111111-1111-4111-8111-111111111111',detached:false,terminated:false},second={phase:'reader',incarnation:'22222222-2222-4222-8222-222222222222',terminated:false}
  const target=epoch=>({targetId:epoch.phase,url:'https://fixture.test/assets/session-archive-worker-test.js?phase='+epoch.phase+'&incarnation='+epoch.incarnation})
  assert.equal(selectArchiveWorkerEpoch([target(first)],[first]).target.targetId,'processor')
  const retired={...first,detached:true,terminated:true};assert.equal(selectArchiveWorkerEpoch([],[retired]).transition.phase,'processor-detached-awaiting-reader')
  assert.equal(selectArchiveWorkerEpoch([target(retired),target(second)],[retired,second]).target.targetId,'reader')
  assert.throws(()=>selectArchiveWorkerEpoch([target(second)],[first,second]))
  assert.throws(()=>selectArchiveWorkerEpoch([target({...second,incarnation:first.incarnation})],[retired,second]))
  assert.throws(()=>selectArchiveWorkerEpoch([],[retired,second]))
})

async function openIsolateDiagnostic(h) {
  const owner={pageSession:null,browserSession:null,workerSession:null,pending:null,sequence:0,closed:false,epochs:[],detachedSessions:new Set()}
  const remaining=()=>{const value=owner.snapshotDeadline?owner.snapshotDeadline-performance.now():2000;if(value<=0)throw Error('isolate_snapshot_deadline');return value}
  const command=(session,method,params={})=>{const budget=remaining();return h.bounded(session.send(method,params),budget,method)}
  const finishPending=(error,value)=>{const pending=owner.pending;if(pending){owner.pending=null;error?pending.reject(error):pending.resolve(value)}}
  const detached=event=>{if(owner.workerSession!==event.sessionId)return;owner.detachedSessions.add(event.sessionId);if(owner.workerSession===event.sessionId){const error=Error('isolate_epoch_retired');error.retiredEpoch={...owner.currentEpoch,sessionId:event.sessionId};finishPending(error);owner.workerSession=null;owner.workerTarget=null;owner.workerId=null;owner.currentEpoch=null}}
  const received=event=>{
    if(event.sessionId!==owner.workerSession||!owner.pending)return
    if(event.message.length>4096){finishPending(Error('isolate_response_limit'));return}
    let message;try{message=JSON.parse(event.message)}catch(error){finishPending(error);return}
    if(message.id===owner.pending.id)finishPending(message.error?Error('isolate_protocol_'+message.error.message):null,message.result)
  }
  const workerCommand=async method=>{
    const budget=remaining()
    assert.equal(owner.pending,null,'one pending isolate RPC')
    const id=++owner.sequence,response=new Promise((resolve,reject)=>{owner.pending={id,resolve,reject}})
    response.catch(()=>{})
    owner.pendingSend=owner.browserSession.send('Target.sendMessageToTarget',{sessionId:owner.workerSession,message:JSON.stringify({id,method})})
    return h.bounded(owner.pendingSend.then(()=>response),budget,method)
  }
  const counters=value=>{for(const key of ['usedSize','totalSize','embedderHeapUsedSize','backingStorageSize'])assert.ok(Number.isFinite(value[key])&&value[key]>=0,'finite isolate '+key);return value}
  owner.snapshot=async({requireWorker=false}={})=>{
    assert.equal(owner.closed,false);assert.ok(!owner.snapshotDeadline,'one bounded snapshot');assert.equal(owner.pending,null,'no abandoned pending RPC')
    owner.snapshotDeadline=performance.now()+2000
    try{
      const pageId=(await command(owner.pageSession,'Runtime.getIsolateId')).id
      assert.ok(typeof pageId==='string'&&pageId.length>0&&pageId.length<=128,'page isolate identity')
      owner.pageId ??= pageId;assert.equal(pageId,owner.pageId,'stable page isolate')
      const page={isolateId:pageId,...counters(await command(owner.pageSession,'Runtime.getHeapUsage'))}
      const topology=async()=>{
        const targets=(await command(owner.browserSession,'Target.getTargets')).targetInfos.filter(value=>value.type==='worker'),budget=remaining()
        const lineage=await h.bounded(h.page.evaluate(()=>window.archiveTelemetry?.workerEpochs || []),budget,'Archive Worker epoch lineage')
        return {targets,lineage}
      }
      const observe=async state=>{
        const selected=selectArchiveWorkerEpoch(state.targets,state.lineage)
        if(owner.workerSession&&(!selected.target||owner.workerTarget!==selected.target.targetId)){
          const session=owner.workerSession
          try{await command(owner.browserSession,'Target.detachFromTarget',{sessionId:session})}catch(error){
            const provedGone=retiredTargetProtocolError(error)&&provedProcessorTransition(state,owner.currentEpoch)&&!state.targets.some(target=>target.targetId===owner.workerTarget)
            if(!owner.detachedSessions.has(session)&&!provedGone)throw error
          }
          owner.workerSession=null;owner.workerTarget=null;owner.workerId=null;owner.currentEpoch=null
        }
        if(!selected.target){assert.ok(!requireWorker||selected.transition,'diagnostic Worker or explicit handoff gap required');return {page,worker:null,...(selected.transition?{workerTransition:selected.transition}:{})}}
        const {target,epoch}=selected
        const acquire=async action=>{try{return await action()}catch(error){if(epoch.phase==='processor'&&retiredTargetProtocolError(error)){error.processorRetirementCandidate=true;error.retiredEpoch={phase:epoch.phase,incarnation:epoch.incarnation,targetId:target.targetId}}throw error}}
        if(!owner.workerSession){
          owner.workerTarget=target.targetId;owner.workerSession=(await acquire(()=>command(owner.browserSession,'Target.attachToTarget',{targetId:target.targetId,flatten:false}))).sessionId
          assert.ok(!owner.epochs.some(value=>value.incarnation===epoch.incarnation),'an epoch cannot reopen as a different target');assert.ok(owner.epochs.length<2)
          owner.epochs.push({phase:epoch.phase,incarnation:epoch.incarnation,targetId:target.targetId})
        }
        owner.currentEpoch={phase:epoch.phase,incarnation:epoch.incarnation,targetId:target.targetId}
        assert.equal(target.targetId,owner.workerTarget,'stable target inside one explicit epoch')
        const workerId=(await acquire(()=>workerCommand('Runtime.getIsolateId'))).id
        assert.ok(typeof workerId==='string'&&workerId.length>0&&workerId.length<=128,'Worker isolate identity')
        owner.workerId ??= workerId;assert.equal(workerId,owner.workerId,'stable isolate inside one explicit epoch');assert.notEqual(workerId,pageId,'page and Worker are distinct isolates')
        const heap=counters(await acquire(()=>workerCommand('Runtime.getHeapUsage')))
        return {page,worker:{phase:epoch.phase,incarnation:epoch.incarnation,isolateId:workerId,targetId:target.targetId,...heap},observedWorkerEpochs:owner.epochs.map(value=>({...value}))}
      }
      let state=await topology(),rechecked=false
      if(staleProcessorTargetPair(state)){
        const proof=state.lineage[0];state=await topology();rechecked=true
        assert.ok(provedProcessorTransition(state,proof),'recheck preserves the exact proved processor transition')
      }
      try{return await observe(state)}catch(error){
        if(rechecked||(error.message!=='isolate_epoch_retired'&&!error.processorRetirementCandidate)||error.retiredEpoch?.phase!=='processor')throw error
        state=await topology();assert.ok(provedProcessorTransition(state,error.retiredEpoch),'only matched processor retirement may cross a snapshot')
        finishPending(Error('isolate_epoch_retired'))
        return await observe(state)
      }
    }finally{owner.snapshotDeadline=null}
  }

  owner.domCounters=async()=>{const value=await command(owner.pageSession,'Memory.getDOMCounters');for(const key of ['documents','nodes','jsEventListeners'])assert.ok(Number.isSafeInteger(value[key])&&value[key]>=0,'finite DOM '+key);return value}
  owner.confirmFixtureClosed=()=>{owner.browserSession?.off('Target.receivedMessageFromTarget',received);owner.browserSession?.off('Target.detachedFromTarget',detached);finishPending(Error('isolate_fixture_closed'));owner.closed=true}
  owner.close=async()=>{
    if(owner.closed)return
    if(owner.workerSession)await command(owner.browserSession,'Target.detachFromTarget',{sessionId:owner.workerSession})
    finishPending(Error('isolate_diagnostic_closed'))
    if(owner.pageSession)await h.bounded(owner.pageSession.detach(),2000,'Page isolate detach')
    if(owner.browserSession){owner.browserSession.off('Target.receivedMessageFromTarget',received);owner.browserSession.off('Target.detachedFromTarget',detached);await h.bounded(owner.browserSession.detach(),2000,'Browser isolate detach')}
    owner.closed=true
  }
  try{owner.pageSession=await h.bounded(h.page.context().newCDPSession(h.page),2000,'Page isolate attach');owner.browserSession=await h.bounded(h.page.context().browser().newBrowserCDPSession(),2000,'Browser isolate attach');owner.browserSession.on('Target.receivedMessageFromTarget',received);owner.browserSession.on('Target.detachedFromTarget',detached);return owner}
  catch(error){error.isolateOwner=owner;throw error}
}


test('observer transition fence joins matched detach and stale topology without consuming late replies',async()=>{
  for(const scenario of ['heap-to-reader','heap-to-gap','stale-pair','unproved-retirement','reader-retirement','attach-to-reader','attach-to-gap','attach-unproved','attach-unknown','first-command-to-reader']){
    const processor={phase:'processor',incarnation:'11111111-1111-4111-8111-111111111111',detached:false,terminated:false},reader={phase:'reader',incarnation:'22222222-2222-4222-8222-222222222222',detached:false,terminated:false},retired={...processor,detached:true,terminated:true}
    const target=epoch=>({type:'worker',targetId:epoch.phase+'-target',url:'https://fixture.test/assets/session-archive-worker-test.js?phase='+epoch.phase+'&incarnation='+epoch.incarnation})
    let lineage=scenario==='reader-retirement'?[retired,reader]:[processor],targets=[target(scenario==='reader-retirement'?reader:processor)],held,oldReplyCount=0,topologyReads=0,lineageReads=0
    const heap={usedSize:1,totalSize:2,embedderHeapUsedSize:3,backingStorageSize:4}
    class BrowserSession extends EventEmitter{
      async send(method,params={}){
        if(method==='Target.getTargets'){topologyReads++;return {targetInfos:structuredClone(targets)}}
        if(method==='Target.attachToTarget'){
          if(params.targetId==='processor-target'&&scenario.startsWith('attach-'))return new Promise((resolve,reject)=>{held={kind:'attach',release(){if(scenario!=='attach-unproved'){lineage=scenario==='attach-to-gap'?[retired]:[retired,reader];targets=scenario==='attach-to-gap'?[]:[target(reader)]}reject(Error(scenario==='attach-unknown'?'permission denied':'Protocol error (Target.attachToTarget): No target with given id found'))}}})
          return {sessionId:params.targetId.replace('-target','-session')}
        }
        if(method==='Target.detachFromTarget'){this.emit('Target.detachedFromTarget',{sessionId:params.sessionId});return {}}
        if(method==='Target.sendMessageToTarget'){
          const message=JSON.parse(params.message),phase=params.sessionId.startsWith('processor')?'processor':'reader'
          if(phase==='processor'&&message.method==='Runtime.getIsolateId'&&scenario==='first-command-to-reader'){lineage=[retired,reader];targets=[target(reader)];throw Error('Protocol error (Target.sendMessageToTarget): No session with given id')}
          const shouldHold=message.method==='Runtime.getHeapUsage'&&((phase==='processor'&&scenario!=='stale-pair')||(phase==='reader'&&scenario==='reader-retirement'))
          if(shouldHold){held={session:params.sessionId,id:message.id};return {}}
          if(phase==='reader'&&message.method==='Runtime.getIsolateId'&&['heap-to-reader','heap-to-gap','attach-to-reader','attach-to-gap','first-command-to-reader'].includes(scenario)){
            oldReplyCount++;this.emit('Target.receivedMessageFromTarget',{sessionId:'processor-session',message:JSON.stringify({id:message.id,result:{id:'late-retired-isolate',...heap,usedSize:999999}})})
          }
          queueMicrotask(()=>this.emit('Target.receivedMessageFromTarget',{sessionId:params.sessionId,message:JSON.stringify({id:message.id,result:message.method==='Runtime.getIsolateId'?{id:phase+'-isolate'}:{...heap,usedSize:phase==='reader'?20:10}})}));return {}
        }
        throw Error('unexpected fake command '+method)
      }
      async detach(){this.closed=true}
    }
    const browser=new BrowserSession(),pageSession={send:async method=>method==='Runtime.getIsolateId'?{id:'page-isolate'}:heap,async detach(){this.closed=true}}
    const h={page:{context:()=>({newCDPSession:async()=>pageSession,browser:()=>({newBrowserCDPSession:async()=>browser})}),evaluate:async()=>{lineageReads++;if(scenario==='stale-pair'&&lineageReads===1){lineage=[retired,reader];targets=[target(reader)]}return structuredClone(lineage)}},bounded:async(action,ms,label)=>{let timer;try{return await Promise.race([action,new Promise((_,reject)=>{timer=setTimeout(()=>reject(Error(label+'_deadline')),ms)})])}finally{clearTimeout(timer)}}}
    const owner=await openIsolateDiagnostic(h)
    try{
      const snapshot=owner.snapshot({requireWorker:true});snapshot.catch(()=>{})
      if(scenario==='stale-pair'){
        const result=await snapshot;assert.equal(result.worker.phase,'reader');assert.equal(result.worker.isolateId,'reader-isolate');assert.equal(topologyReads,2);assert.equal(lineageReads,2)
      }else if(scenario.startsWith('attach-')){
        for(let turn=0;turn<20&&!held;turn++)await new Promise(resolve=>setImmediate(resolve));assert.equal(held?.kind,'attach');held.release()
        if(scenario==='attach-unproved')await assert.rejects(snapshot,/only matched processor retirement/)
        else if(scenario==='attach-unknown')await assert.rejects(snapshot,/permission denied/)
        else{
          let result=await snapshot
          if(scenario==='attach-to-gap'){assert.equal(result.worker,null);assert.equal(result.workerTransition.phase,'processor-detached-awaiting-reader');lineage=[retired,reader];targets=[target(reader)];result=await owner.snapshot({requireWorker:true})}
          assert.equal(result.worker.phase,'reader');assert.equal(result.worker.isolateId,'reader-isolate');assert.equal(result.worker.usedSize,20);assert.equal(oldReplyCount,1);assert.equal(owner.pending,null)
        }
      }else if(scenario==='first-command-to-reader'){
        const result=await snapshot;assert.equal(result.worker.phase,'reader');assert.equal(result.worker.isolateId,'reader-isolate');assert.equal(result.worker.usedSize,20);assert.equal(oldReplyCount,1);assert.equal(owner.pending,null)
      }else{
        for(let turn=0;turn<20&&!held;turn++)await new Promise(resolve=>setImmediate(resolve));assert.ok(held,'held actual helper heap RPC')
        if(scenario==='heap-to-reader'){lineage=[retired,reader];targets=[target(reader)]}
        if(scenario==='heap-to-gap'){lineage=[retired];targets=[]}
        browser.emit('Target.detachedFromTarget',{sessionId:held.session})
        if(scenario==='unproved-retirement')await assert.rejects(snapshot,/only matched processor retirement/)
        else if(scenario==='reader-retirement')await assert.rejects(snapshot,/isolate_epoch_retired/)
        else{
          let result=await snapshot
          if(scenario==='heap-to-gap'){assert.equal(result.worker,null);assert.equal(result.workerTransition.phase,'processor-detached-awaiting-reader');lineage=[retired,reader];targets=[target(reader)];result=await owner.snapshot({requireWorker:true})}
          assert.equal(result.worker.phase,'reader');assert.equal(result.worker.isolateId,'reader-isolate');assert.equal(result.worker.usedSize,20);assert.equal(oldReplyCount,1)
          assert.equal(owner.pending,null)
        }
      }
    }finally{await owner.close();assert.equal(owner.closed,true);assert.equal(pageSession.closed,true);assert.equal(browser.closed,true)}
  }
})

async function tinyIsolateCapability(image,body) {
  const scratch=resolve('../../.tmp/sealed-archives-e0ac6260'),profile=resolve(scratch,'archive-profile-'+randomUUID())
  body.current();await mkdir(profile)
  let launch,h,isolate,primary;const receipt={purpose:'tiny-public-CDP-isolate-capability',status:'running'}
  try{
    body.current()
    launch=ownedPersistentFixture({path:profile,executable:image.path});body.owner=launch;h=await launch.get();receipt.actualBrowser=h.browserImage;body.current()
    const raw=h.content.get(h.saved.id),gzip=gzipSync(raw),manifest=makeManifest(h,raw,gzip)
    await h.page.route('**/sessions/**/archive**',phaseArchiveRoute(route=>new URL(route.request().url()).pathname.endsWith('/content')?route.fulfill({headers:{'Content-Type':'application/gzip','Content-Length':String(gzip.length),'X-HMem-Archive-Generation':generation,'X-HMem-Archive-SHA256':hash(gzip),'X-HMem-Archive-Raw-Bytes':String(raw.length),'X-HMem-Archive-Raw-SHA256':hash(raw)},body:gzip}):route.fulfill({json:manifest})))
    await h.page.addInitScript(captureArchiveTelemetry)
    await h.start();await h.idle();body.current()
    await h.page.goto(h.origin+'/workspace/'+h.fixture.workspace.id+'#tab=sessions&session='+h.saved.id)
    await h.page.getByText('Verified complete archive.',{exact:true}).waitFor({timeout:10000});body.current()
    isolate=await openIsolateDiagnostic(h)
    receipt.first=await isolate.snapshot({requireWorker:true});receipt.second=await isolate.snapshot({requireWorker:true});receipt.status='passed'
  }catch(error){isolate ||= error.isolateOwner;primary=error;receipt.status='failed';receipt.failure=error.message}
  finally{
    try{await isolate?.close()}catch(error){primary=primary?new AggregateError([primary,error],'isolate_capability_cleanup_failed'):error;receipt.status='failed'}
    const cleanup={h,profileLaunch:launch,profile,removeProfile:async()=>{if(launch&&!launch.closed)throw Error('profile_closure_unconfirmed');assert.equal(await realpath(profile),profile);assert.equal((await lstat(profile)).isSymbolicLink(),false);assert.ok(profile.startsWith(await realpath(scratch)+sep));await rm(profile,{recursive:true,maxRetries:0});receipt.profileCleanup='confirmed'}}
    try{await cleanupQualificationOwner(cleanup);isolate?.confirmFixtureClosed();body.owner=null}catch(error){primary=primary?new AggregateError([primary,error],'isolate_capability_fixture_cleanup_failed'):error;receipt.status='failed';receipt.cleanupFailure=error.message}
  }
  if(primary){primary.capabilityReceipt=receipt;throw primary}
  return receipt
}

// Retain the actual pending persistent launch before any deadline can win.
// Retirement joins that launch and adopts/cleans its eventual owner exactly once.
function ownedPersistentFixture(profile) {
  const retained=[globalThis.retainedPersistentFixtureLaunchOwner,globalThis.retainedArchiveQualificationOwner].find(owner=>owner&&!owner.closed)
  if(retained&&!retained.closed){const error=Error('persistent_fixture_owner_occupied');error.owner=retained;throw error}
  const owner={profile,closed:false,closing:null}
  globalThis.retainedPersistentFixtureLaunchOwner=owner
  owner.pending=openSessionExplorer({persistentProfile:profile})
  owner.settled=owner.pending.then(h=>{owner.h=h;return {h}},error=>{owner.failure=error;return {error}})
  owner.get=async()=>{const result=await owner.settled;if(result.error)throw result.error;return result.h}
  owner.close=()=>{
    if(owner.closed)return Promise.resolve()
    if(!owner.closing){
      const closing=owner.settled.then(async result=>{
        const capability=result.h || result.error?.setupOwner
        if(!capability)throw new AggregateError([result.error].filter(Boolean),'persistent_launch_cleanup_owner_missing')
        owner.actualCloseOwner=capability;await capability.close();owner.closed=true
        if(globalThis.retainedPersistentSessionFixtureSetupOwner===capability)delete globalThis.retainedPersistentSessionFixtureSetupOwner
        if(globalThis.retainedPersistentFixtureLaunchOwner===owner)delete globalThis.retainedPersistentFixtureLaunchOwner
      })
      owner.closing=closing;owner.lastClosing=closing
      closing.then(()=>{},error=>{owner.cleanupFailure=error;if(owner.closing===closing)owner.closing=null})
    }
    return owner.closing
  }
  return owner
}
async function closePersistentFixture(owner) {
  let timer
  try{await Promise.race([owner.close(),new Promise((_,reject)=>{timer=setTimeout(()=>reject(Error('persistent_launch_retirement_deadline')),10000)})])}
  finally{clearTimeout(timer)}
}

function cleanupQualificationOwner(owner,milliseconds=10000) {
  if(owner.closed)return owner.actualCleanup
  const retained=globalThis.retainedArchiveQualificationOwner
  if(retained&&retained!==owner&&!retained.closed){const error=Error('qualification_cleanup_owner_occupied');error.owner=retained;throw error}
  if(owner.cleanupDisposition)return owner.cleanupDisposition
  globalThis.retainedArchiveQualificationOwner=owner
  owner.closed=false;owner.completedCleanup=[];owner.cleanupFailures=[]
  let failDeadline
  const deadline=new Promise((_,reject)=>{failDeadline=reject})
  const stage=async(name,action)=>{
    owner.cleanupStage=name;owner.pendingCleanup=Promise.resolve().then(action)
    const timer=setTimeout(()=>{const error=Error('qualification_'+name+'_cleanup_deadline');error.owner=owner;owner.cleanupFailures.push({stage:name,error});failDeadline(error)},milliseconds)
    try{await owner.pendingCleanup;owner.completedCleanup.push(name)}
    catch(error){owner.cleanupFailures.push({stage:name,error});throw error}
    finally{clearTimeout(timer)}
  }
  owner.actualCleanup=(async()=>{
    await stage('browser',()=>owner.profileLaunch?closePersistentFixture(owner.profileLaunch):owner.h?.close())
    await stage('http',()=>owner.http?.close())
    await stage('gzip',()=>owner.fixture?.dispose())
    await stage('profile',()=>owner.removeProfile?.())
    owner.closed=true
    if(globalThis.retainedArchiveQualificationOwner===owner)delete globalThis.retainedArchiveQualificationOwner
  })()
  owner.cleanupDisposition=Promise.race([owner.actualCleanup,deadline])
  owner.actualCleanup.then(()=>{},error=>{owner.actualCleanupFailure=error})
  return owner.cleanupDisposition
}

async function closeIsolateAndFixture(isolate,owner,primary,milliseconds=10000) {
  if(!isolate){try{await cleanupQualificationOwner(owner,milliseconds)}catch(error){throw primary?new AggregateError([primary,error],'qualification_cleanup_failed'):error}return}
  let isolateFailure,fixtureFailure
  try{await isolate?.close()}catch(error){isolateFailure=error}
  try{await cleanupQualificationOwner(owner,milliseconds)}catch(error){fixtureFailure=error}
  if(!fixtureFailure)isolate?.confirmFixtureClosed()
  if(isolateFailure||fixtureFailure)throw new AggregateError([primary,isolateFailure,fixtureFailure].filter(Boolean),'qualification_cleanup_failed')
}

test('isolate and held fixture cleanup preserve all three causes and ownership',async()=>{
  const primary=Error('diagnostic_primary'),detach=Error('diagnostic_detach_failure')
  let release,confirmed=false;const held=new Promise(resolve=>{release=resolve})
  const isolate={async close(){throw detach},confirmFixtureClosed(){confirmed=true}}
  const owner={h:{async close(){}},http:{async close(){await held}},fixture:{async dispose(){}},async removeProfile(){}}
  try{
    await assert.rejects(closeIsolateAndFixture(isolate,owner,primary,20),error=>{
      assert.ok(error instanceof AggregateError);assert.equal(error.errors.length,3)
      assert.equal(error.errors[0],primary);assert.equal(error.errors[1],detach);assert.equal(error.errors[2].message,'qualification_http_cleanup_deadline');return true
    })
    assert.equal(confirmed,false);assert.equal(owner.closed,false);assert.equal(owner.cleanupStage,'http')
    assert.equal(globalThis.retainedArchiveQualificationOwner,owner);assert.ok(owner.pendingCleanup instanceof Promise)
    assert.throws(()=>ownedPersistentFixture({path:'task-owned/blocked'}),error=>error.owner===owner)
  }finally{release();await owner.actualCleanup;isolate.confirmFixtureClosed()}
  assert.equal(owner.closed,true);assert.equal(confirmed,true);assert.equal(globalThis.retainedArchiveQualificationOwner,undefined)
})

test('qualification cleanup deadlines retain stalled HTTP, gzip and profile capabilities',async()=>{
  for(const stalled of ['http','gzip','profile']){
    let release;const held=new Promise(resolve=>{release=resolve}),calls=[]
    const action=name=>async()=>{calls.push(name);if(name===stalled)await held}
    const owner={h:{close:action('browser')},http:{close:action('http')},fixture:{file:'task-owned/fixture.gz',dispose:action('gzip')},profile:'task-owned/profile',removeProfile:action('profile')}
    try{
      await assert.rejects(cleanupQualificationOwner(owner,20),new RegExp('qualification_'+stalled+'_cleanup_deadline'))
      assert.equal(owner.closed,false);assert.equal(owner.cleanupStage,stalled);assert.ok(owner.pendingCleanup instanceof Promise)
      assert.equal(globalThis.retainedArchiveQualificationOwner,owner)
      assert.throws(()=>ownedPersistentFixture({path:owner.profile}),error=>{assert.equal(error.owner,owner);return true})
      assert.deepEqual(calls,['browser','http','gzip','profile'].slice(0,['browser','http','gzip','profile'].indexOf(stalled)+1))
    }finally{release();await owner.actualCleanup}
    assert.equal(owner.closed,true);await cleanupQualificationOwner(owner,20);assert.deepEqual(owner.completedCleanup,['browser','http','gzip','profile']);assert.equal(globalThis.retainedArchiveQualificationOwner,undefined)
  }
})

test('paired native memory dump modes retain process coverage',{skip:process.env.HMEM_ARCHIVE_DUMP_CANARY!=='1',timeout:110000},async()=>{
  const evidencePath=resolve('perf/session-archive.qualification.json'),evidence=await readQualificationEvidence(evidencePath)
  assert.equal(evidence.taskId,'e0ac6260-641c-4891-b4c1-cc3694f31158')
  const image=evidence.browserImage || evidence.attempts[0].browserImage,executable=image.path
  assert.equal(await hashFile(executable),image.sha256)
  const scratch=resolve('../../.tmp/sealed-archives-e0ac6260'),profile=resolve(scratch,'archive-profile-'+randomUUID())
  assert.ok(profile.startsWith(scratch+sep));await mkdir(profile)
  const receipt={kind:'paired-memory-dump-mode',at:new Date().toISOString(),deterministic:false,categoryConfig:{includedCategories:['disabled-by-default-memory-infra']},capCharacters:16777216,diagnosticUnit:'decoded UTF16 characters',browserImage:image,sourceHashes:{},snapshots:[],status:'running'}
  for(const path of ['tests-js/session-archive.browser.test.js','tests-js/hierarchy-fixture.mjs','tests-js/support/sessions-ux-fixture.mjs','tests-js/support/session-archive-memory.mjs'])receipt.sourceHashes[path]=await hashFile(path)
  const deadline=Date.now()+60000
  const bounded=async(action,label)=>{const remaining=deadline-Date.now();assert.ok(remaining>0,'Memory dump canary60s body deadline');let timer;try{return await Promise.race([action,new Promise((_,reject)=>{timer=setTimeout(()=>reject(Error(label+' body deadline')),remaining)})])}finally{clearTimeout(timer)}}
  let owner,h,primary
  try{
    owner=ownedPersistentFixture({path:profile,executable});h=await bounded(owner.get(),'Persistent canary launch')
    receipt.actualBrowser=h.browserImage
    const raw=h.content.get(h.saved.id),gzip=gzipSync(raw),manifest=makeManifest(h,raw,gzip)
    await h.page.route('**/sessions/**/archive**',phaseArchiveRoute(route=>new URL(route.request().url()).pathname.endsWith('/content')?route.fulfill({headers:{'Content-Type':'application/gzip','Content-Length':String(gzip.length),'X-HMem-Archive-Generation':generation,'X-HMem-Archive-SHA256':hash(gzip),'X-HMem-Archive-Raw-Bytes':String(raw.length),'X-HMem-Archive-Raw-SHA256':hash(raw)},body:gzip}):route.fulfill({json:manifest})))
    await bounded(h.start(),'Tiny canary start');await bounded(h.idle(),'Tiny canary idle')
    await h.page.goto(h.origin+'/workspace/'+h.fixture.workspace.id+'#tab=sessions&session='+h.saved.id)
    await h.page.getByText('Verified complete archive.',{exact:true}).waitFor({timeout:10000})
    for(const phase of ['before-idb','after-idb']){
      if(phase==='after-idb')receipt.idb=await bounded(h.page.evaluate(async()=>{
        const opening=indexedDB.open('hmem-archive-telemetry-canary',1);opening.onupgradeneeded=()=>opening.result.createObjectStore('rows',{keyPath:'key'})
        const db=await new Promise((resolve,reject)=>{opening.onsuccess=()=>resolve(opening.result);opening.onerror=()=>reject(opening.error)})
        try{for(let start=0;start<4096;start+=256){const tx=db.transaction('rows','readwrite'),done=new Promise((resolve,reject)=>{tx.oncomplete=resolve;tx.onabort=tx.onerror=()=>reject(tx.error)});for(let key=start;key<start+256;key++)tx.objectStore('rows').put({key,text:'x'.repeat(128)});await done}return {records:4096,payloadBytes:4096*128,batchRows:256}}finally{db.close()}
      }),'Bounded IDB canary writes')
      for(const levelOfDetail of ['detailed','background']){
        const began=performance.now(),snapshot=await bounded(boundedMemorySnapshot(h,{levelOfDetail}),levelOfDetail+' native dump')
        assert.equal(snapshot.traceLoss,false);assert.equal(snapshot.workerTargets.length,1)
        assert.ok(snapshot.processes.some(value=>value.pid===h.browserImage.pid&&value.type==='browser'))
        assert.ok(snapshot.processes.every(value=>typeof value.totals.private_footprint_bytes==='string'&&/^[a-f0-9]+$/i.test(value.totals.private_footprint_bytes)))
        receipt.snapshots.push({phase,levelOfDetail,requestGuid:snapshot.requestGuid,exportRowId:snapshot.exportRowId,attribution:snapshot.attribution,dumpMs:performance.now()-began,traceLoss:snapshot.traceLoss,trace:snapshot.trace,processes:snapshot.processes.map(value=>({pid:value.pid,type:value.type,name:value.name,fields:Object.keys(value.totals).sort(),privateBytes:parseInt(value.totals.private_footprint_bytes,16)})),workers:snapshot.workerTargets,totalPrivateBytes:snapshot.totalPrivateBytes})
      }
    }
    const identities=receipt.snapshots[0].processes.map(value=>value.pid).sort((a,b)=>a-b)
    const workerIds=receipt.snapshots[0].workers.map(value=>value.id)
    for(const snapshot of receipt.snapshots){assert.deepEqual(snapshot.processes.map(value=>value.pid).sort((a,b)=>a-b),identities);assert.deepEqual(snapshot.workers.map(value=>value.id),workerIds)}
    assert.deepEqual(h.errors,[]);receipt.status='passed'
  }catch(error){primary=error;receipt.status='failed';receipt.failure={message:error.message,diagnostic:error.diagnostic,stack:String(error.stack).slice(0,2000)}}
  finally{
    const cleanupOwner={h,profileLaunch:owner,profile,removeProfile:async()=>{if(!owner?.closed)throw Error('profile_closure_unconfirmed');assert.equal(await realpath(profile),profile);assert.equal((await lstat(profile)).isSymbolicLink(),false);assert.ok(profile.startsWith(await realpath(scratch)+sep));await rm(profile,{recursive:true,maxRetries:0});receipt.profileCleanup='confirmed'}}
    try{await cleanupQualificationOwner(cleanupOwner)}catch(error){receipt.cleanupFailure={message:error.message,stage:cleanupOwner.cleanupStage};primary=primary?new AggregateError([primary,error],'memory_dump_canary_cleanup_failed'):error;receipt.status='failed'}
  }
  assert.equal(await hashFile(executable),image.sha256)
  for(const[path,value]of Object.entries(receipt.sourceHashes))assert.equal(await hashFile(path),value)
  evidence.diagnostics ||= [];assert.ok(evidence.diagnostics.length<4);evidence.diagnostics.push(receipt)
  const encoded=JSON.stringify(evidence,null,2)+'\n';assert.ok(Buffer.byteLength(encoded)<=1048576)
  const temporary=resolve(scratch,'qualification-'+randomUUID()+'.json');await writeFile(temporary,encoded,{flag:'wx'});await rename(temporary,evidencePath)
  console.log(JSON.stringify({memoryModeCanary:{status:receipt.status,idb:receipt.idb,snapshots:receipt.snapshots.map(value=>({phase:value.phase,mode:value.levelOfDetail,requestGuid:value.requestGuid,exportRowId:value.exportRowId,characters:value.trace.characters,utf8Bytes:value.trace.utf8Bytes,pids:value.processes.map(process=>process.pid),workerCount:value.workers.length,traceLoss:value.traceLoss})),cleanup:receipt.profileCleanup,evidenceSha256:hash(Buffer.from(encoded))}}))
  if(primary)throw primary
})

test('ordinary fixture retirement joins its owned context before disconnect and propagates rejection',{timeout:20000},async()=>{
  for(const rejected of [false,true]){
    const h=await openSessionExplorer(),context=h.page.context(),browser=context.browser(),nativeClose=context.close.bind(context),injected=Error('owned_context_close_failure')
    let release,entered,settled=false,primary
    const gate=new Promise(resolve=>{release=resolve}),entry=new Promise(resolve=>{entered=resolve})
    context.close=async(...args)=>{entered();await gate;if(rejected)throw injected;return nativeClose(...args)}
    const closing=h.close().finally(()=>{settled=true});closing.catch(()=>{})
    try{
      assert.equal(await Promise.race([entry.then(()=>true),closing.then(()=>false,()=>false)]),true,'ordinary retirement must join the exact owned native context before disconnect')
      assert.equal(browser.isConnected(),true);assert.equal(settled,false,'retirement waits for held context completion')
      release()
      if(rejected)await assert.rejects(closing,error=>error===injected,'the primary context cleanup failure is retained')
      else await closing
      assert.equal(browser.isConnected(),false)
      assert.throws(()=>process.kill(h.browserImage.pid,0),error=>error.code==='ESRCH','the owned Chromium process has actually retired')
    }catch(error){primary=error}
    finally{release();try{await closing}catch(error){if(error!==injected)primary=primary?new AggregateError([primary,error],'ordinary_fixture_causal_cleanup_failed'):error}}
    if(primary)throw primary
  }
})

for(const mode of ['setup-cleanup-failure','late-launch'])test('persistent fixture owner retains '+mode,{timeout:30000},async()=>{
  const probe=await openSessionExplorer(),executable=probe.browserImage.executable
  await probe.close()
  const scratch=profileScratch(),profile=resolve(scratch,'archive-profile-'+randomUUID())
  await mkdir(profile)
  let owner,release,primary,context
  try {
    if(mode==='setup-cleanup-failure'){
      const held=new Promise(resolve=>{release=resolve})
      owner=ownedPersistentFixture({path:profile,executable,afterAcquire:({context:actual})=>{
        context=actual;const close=actual.close.bind(actual);actual.close=()=>held.then(()=>close());throw Error('persistent_setup_primary')
      }})
      await assert.rejects(owner.get(),error=>{
        assert.equal(error.message,'persistent_fixture_setup_cleanup_failed');assert.equal(error.errors[0].message,'persistent_setup_primary')
        assert.ok(error.errors[1].errors.some(value=>value.message.includes('5000ms')))
        assert.equal(error.setupOwner.context,context);assert.equal(error.setupOwner.browser.isConnected(),true)
        assert.equal(globalThis.retainedPersistentFixtureSetupOwner,error.setupOwner);return true
      })
      assert.equal(owner.closed,false);assert.ok(await lstat(profile))
      release();await closePersistentFixture(owner);assert.equal(context.browser().isConnected(),false)
    }else{
      let arrived;const entered=new Promise(resolve=>{arrived=resolve}),held=new Promise(resolve=>{release=resolve})
      owner=ownedPersistentFixture({path:profile,executable,afterAcquire:async({context:actual})=>{context=actual;arrived();await held}})
      await entered
      let timer
      try{await assert.rejects(Promise.race([owner.get(),new Promise((_,reject)=>{timer=setTimeout(()=>reject(Error('injected_launch_deadline')),20)})]),/injected_launch_deadline/)}finally{clearTimeout(timer)}
      const retiring=owner.close();assert.equal(owner.closed,false);assert.equal(owner.h,undefined);assert.equal(globalThis.retainedPersistentFixtureLaunchOwner,owner)
      assert.throws(()=>ownedPersistentFixture({path:profile,executable}),error=>{assert.equal(error.message,'persistent_fixture_owner_occupied');assert.equal(error.owner,owner);return true})
      assert.equal(globalThis.retainedPersistentFixtureLaunchOwner,owner)
      release();await retiring
      assert.equal(owner.actualCloseOwner,owner.h);assert.equal(owner.h.page.isClosed(),true);assert.equal(context.browser().isConnected(),false)
    }
    assert.equal(owner.closed,true)
  }catch(error){primary=error}
  finally{
    release?.()
    if(owner&&!owner.closed){try{await closePersistentFixture(owner)}catch(error){primary=primary?new AggregateError([primary,error],'causal_profile_cleanup_failed'):error}}
    if(owner?.closed){assert.equal(await realpath(profile),profile);assert.equal((await lstat(profile)).isSymbolicLink(),false);assert.ok(profile.startsWith(await realpath(scratch)+sep));await rm(profile,{recursive:true,maxRetries:0})}
    else globalThis.retainedArchiveCausalOwner={owner,profile}
  }
  if(primary)throw primary
})

test('persistent profile canary and repeated native tracing idle control',{skip:process.env.HMEM_ARCHIVE_PROFILE_DIAGNOSTIC!=='1',timeout:150000},async()=>{
  const evidencePath=resolve('perf/session-archive.qualification.json'),evidence=await readQualificationEvidence(evidencePath)
  assert.equal(evidence.taskId,'e0ac6260-641c-4891-b4c1-cc3694f31158')
  const image=evidence.browserImage || evidence.attempts[0].browserImage,executable=image.path
  assert.equal(await hashFile(executable),image.sha256)
  const scratch=resolve('../../.tmp/sealed-archives-e0ac6260'),profile=resolve(scratch,'archive-profile-'+randomUUID())
  assert.ok(profile.startsWith(scratch+sep));await mkdir(profile)
  const inputs={};for(const path of ['tests-js/hierarchy-fixture.mjs','tests-js/support/sessions-ux-fixture.mjs','tests-js/support/session-archive-memory.mjs','tests-js/session-archive.browser.test.js'])inputs[path]=await hashFile(path)
  const receipt={at:new Date().toISOString(),kind:'persistent-profile-and-idle-tracing',contextMode:'task-owned persistent normal profile',method:'56 separate native Tracing.start/end snapshots; no archive processing during idle control; no forced GC',inputReferenceAttempt:0,inputHashes:inputs,browserImage:image,nodeImage:{path:process.execPath,sha256:await hashFile(process.execPath)},sources:[{url:'https://playwright.dev/docs/api/class-browsercontext',fact:'Nonpersistent browser contexts do not write browsing data to disk.'},{url:'https://chromium.googlesource.com/chromium/src/+/main/storage/browser/file_system/file_system_options.h',fact:'FileSystemOptions receives profile mode and force-in-memory choices; incognito filesystems use memory.'},{url:'https://raw.githubusercontent.com/chromium/chromium/main/storage/browser/file_system/file_system_options.cc',fact:'is_in_memory returns force_in_memory_ || is_incognito().'}],samples:[],status:'running'}
  let h,launchOwner,primary,safeToDelete=false
  const deadline=Date.now()+120000
  const bounded=async(action,label)=>{const remaining=deadline-Date.now();assert.ok(remaining>0,'Profile diagnostic overall120s deadline');let timer;try{return await Promise.race([action,new Promise((_,reject)=>{timer=setTimeout(()=>reject(Error(label+' overall deadline')),remaining)})])}finally{clearTimeout(timer)}}
  const canary=Buffer.from(Array.from({length:4096},(_,index)=>(index*37+19)%256)),canarySha=hash(canary)
  const canaryOperation=async(mode,origin)=>{
    await h.page.route(origin+'/archive-profile-canary',route=>route.fulfill({contentType:'text/html',body:'<!doctype html><title>Task6 storage canary</title>'}))
    await h.page.goto(origin+'/archive-profile-canary')
    return h.page.evaluate(async({mode,bytes})=>{
      const source=`self.onmessage=async({data})=>{let handle;try{const root=await navigator.storage.getDirectory(),directory=await root.getDirectoryHandle('hmem-archive-profile-canary',{create:data.mode==='write'}),file=await directory.getFileHandle('canary.bin',{create:data.mode==='write'});if(data.mode==='write'){handle=await file.createSyncAccessHandle();handle.write(new Uint8Array(data.bytes));handle.flush();handle.close();handle=null}const value=await file.getFile();if(value.size!==4096)throw Error('canary_size');self.postMessage({bytes:[...new Uint8Array(await value.arrayBuffer())]})}catch(error){self.postMessage({error:error.message})}finally{handle?.close()}}`
      const url=URL.createObjectURL(new Blob([source],{type:'text/javascript'})),worker=new Worker(url)
      try{return await new Promise((resolve,reject)=>{const timer=setTimeout(()=>reject(Error('canary_worker_deadline')),10000);worker.onmessage=({data})=>{clearTimeout(timer);data.error?reject(Error(data.error)):resolve(data.bytes)};worker.onerror=event=>{clearTimeout(timer);reject(Error(event.message))};worker.postMessage({mode,bytes})})}
      finally{worker.terminate();URL.revokeObjectURL(url)}
    },{mode,bytes:[...canary]})
  }
  try {
    launchOwner=ownedPersistentFixture({path:profile,executable});h=await bounded(launchOwner.get(),'First persistent launch')
    receipt.firstBrowser=h.browserImage
    const origin=h.origin;assert.equal(hash(Buffer.from(await bounded(canaryOperation('write',origin),'Canary write'))),canarySha)
    await closePersistentFixture(launchOwner);h=null;launchOwner=null;safeToDelete=true
    const disk={files:0,bytes:0,canaryFiles:[]}
    const inspect=async(directory)=>{for(const entry of await readdir(directory,{withFileTypes:true})){const path=resolve(directory,entry.name);assert.ok(path.startsWith(profile+sep));const info=await lstat(path);assert.equal(info.isSymbolicLink(),false);if(entry.isDirectory())await inspect(path);else if(entry.isFile()){assert.ok(++disk.files<=4096);disk.bytes+=info.size;assert.ok(disk.bytes<=67108864);if(info.size===4096&&await hashFile(path)===canarySha)disk.canaryFiles.push(path.slice(profile.length+1))}}}
    await bounded(inspect(profile),'Closed profile disk inspection');assert.ok(disk.canaryFiles.length,'Exact OPFS canary must exist as a physical task-owned profile file after browser exit');receipt.closedProfileDisk=disk
    safeToDelete=false;launchOwner=ownedPersistentFixture({path:profile,executable});h=await bounded(launchOwner.get(),'Persistent reopen');receipt.reopenedBrowser=h.browserImage
    assert.notEqual(h.browserImage.pid,receipt.firstBrowser.pid)
    assert.equal(hash(Buffer.from(await bounded(canaryOperation('read',origin),'Canary reopen read'))),canarySha);receipt.canary={bytes:4096,sha256:canarySha,survivedCloseReopen:true}
    const raw=h.content.get(h.saved.id),gzip=gzipSync(raw),manifest=makeManifest(h,raw,gzip)
    await h.page.route('**/sessions/**/archive**',phaseArchiveRoute(route=>new URL(route.request().url()).pathname.endsWith('/content')?route.fulfill({headers:{'Content-Type':'application/gzip','Content-Length':String(gzip.length),'X-HMem-Archive-Generation':generation,'X-HMem-Archive-SHA256':hash(gzip),'X-HMem-Archive-Raw-Bytes':String(raw.length),'X-HMem-Archive-Raw-SHA256':hash(raw)},body:gzip}):route.fulfill({json:manifest})))
    await bounded(h.start(),'Tiny fixture start');await bounded(h.idle(),'Tiny fixture idle');await h.page.goto(h.origin+'/workspace/'+h.fixture.workspace.id+'#tab=sessions&session='+h.saved.id);await h.page.getByText('Verified complete archive.',{exact:true}).waitFor({timeout:10000})
    const start=performance.now()
    for(let index=0;index<56;index++){
      const began=performance.now(),snapshot=await bounded(boundedMemorySnapshot(h),'Idle native snapshot')
      assert.ok(snapshot.processes.some(value=>value.pid===h.browserImage.pid&&value.type==='browser'));assert.ok(snapshot.workerTargets.length)
      receipt.samples.push({atMs:performance.now()-start,dumpMs:performance.now()-began,totalPrivateBytes:snapshot.totalPrivateBytes,processes:snapshot.processes.map(value=>({pid:value.pid,type:value.type,bytes:parseInt(value.totals.private_footprint_bytes,16)})),traceLoss:snapshot.traceLoss})
      if(index!==55)await bounded(new Promise(resolve=>setTimeout(resolve,1000)),'Idle cadence')
    }
    const first=receipt.samples[0].totalPrivateBytes;receipt.sampledIdlePrivateIncrease=Math.max(...receipt.samples.map(value=>value.totalPrivateBytes-first));receipt.elapsedMs=performance.now()-start;receipt.status='completed';assert.deepEqual(h.errors,[])
  }catch(error){primary=error;receipt.status='failed';receipt.failure={message:error.message,stack:String(error.stack).slice(0,2000)}}
  finally {
    if(launchOwner){try{await closePersistentFixture(launchOwner);h=null;safeToDelete=true}catch(error){globalThis.retainedArchiveDiagnosticOwner={launchOwner,h,profile};receipt.cleanupFailure=error.message;primary=primary?new AggregateError([primary,error],'profile_diagnostic_cleanup_failed'):error;receipt.status='failed'}}
    if(safeToDelete){assert.equal(await realpath(profile),profile);assert.equal((await lstat(profile)).isSymbolicLink(),false);assert.ok(profile.startsWith(await realpath(scratch)+sep));await rm(profile,{recursive:true,maxRetries:0});receipt.profileCleanup='confirmed'}
    else {receipt.profileCleanup='retained on uncertain browser closure';receipt.retainedProfile=profile;receipt.status='failed'}
  }
  assert.equal(await hashFile(executable),image.sha256)
  for(const[path,value]of Object.entries(inputs))assert.equal(await hashFile(path),value)
  evidence.diagnostics ||= [];assert.ok(evidence.diagnostics.length<4);evidence.diagnostics.push(receipt)
  for(const attempt of evidence.attempts){attempt.contextMode ||= 'nonpersistent (incognito)';attempt.memoryMethod ||= 'separate native Tracing.start/end for each sample'}
  const encoded=JSON.stringify(evidence,null,2)+'\n';assert.ok(Buffer.byteLength(encoded)<=1048576)
  const temporary=resolve(scratch,'qualification-'+randomUUID()+'.json');await writeFile(temporary,encoded,{flag:'wx'});await rename(temporary,evidencePath)
  console.log(JSON.stringify({profileDiagnostic:{status:receipt.status,canary:receipt.canary,closedProfileDisk:receipt.closedProfileDisk,samples:receipt.samples.length,sampledIdlePrivateIncrease:receipt.sampledIdlePrivateIncrease,elapsedMs:receipt.elapsedMs,cleanup:receipt.profileCleanup,evidenceSha256:hash(Buffer.from(encoded))}}))
  if(primary)throw primary
})

async function nativeProbe(h,raw,bytes,{prelude='',manifest=makeManifest(h,raw,bytes),page={},cacheReadControl=null}={}) {
  const asset=await workerAsset()
  return h.page.evaluate(async({asset,manifest,bytes,prelude,page,cacheReadControl})=>{
    const entry=prelude?URL.createObjectURL(new Blob(['const queued=[];self.onmessage=event=>queued.push(event);'+prelude+'\nawait import('+JSON.stringify(new URL(asset,location.href).href)+');for(const event of queued)self.onmessage(event);'],{type:'text/javascript'})):asset
    const worker=new Worker(entry,{type:'module'}),stamp={workspace:manifest.workspace_id,session:manifest.session_id,generation:manifest.generation_id,load:1,authEpoch:1}
    let id=0;const pending=new Map(),progress=[]
    worker.onmessage=({data})=>{if(data.kind==='progress'){progress.push(data.phase);return}const waiter=pending.get(data.request);if(waiter){pending.delete(data.request);data.kind==='error'?waiter.reject(Error(data.code)):waiter.resolve(data)}}
    const rpc=(kind,extra={})=>new Promise((resolve,reject)=>{const request=++id,timer=setTimeout(()=>reject(Error('browser_rpc_timeout')),15000);pending.set(request,{resolve:value=>{clearTimeout(timer);resolve(value)},reject:error=>{clearTimeout(timer);reject(error)}});worker.postMessage({kind,stamp,request,...extra},extra.bytes?[extra.bytes]:[])})
    let error=null,result=null,cleanup=null;const cachePages=[]
    try{
      await rpc('start',{name:crypto.randomUUID(),manifest})
      let buffer=new ArrayBuffer(65536)
      for(let at=0;at<bytes.length;at+=65536){
        const part=bytes.slice(at,at+65536);new Uint8Array(buffer).set(part)
        const received=await rpc('chunk',{bytes:buffer,length:part.length})
        if(received.kind!=='ack'||received.bytes?.byteLength!==65536)throw Error('browser_chunk_ack_mismatch')
        buffer=received.bytes
      }
      await rpc('finish');result=await rpc('page',{page});if(cacheReadControl){
      const check=value=>{for(const row of value.rows)if(row.body?.text!==cacheReadControl.expected[Number(row.offset)/1024])throw Error('cache_body_fidelity');cachePages.push({first:value.first,last:value.last,rows:value.rows.length,wireBytes:new TextEncoder().encode(JSON.stringify(value)).length,...value.rawReadControl})}
      check(result);for(let index=0;index<21;index++){result=await rpc('page',{page:index%2===0?{last:true}:{}});check(result)}
    }}
    catch(failure){error=failure.message}
    finally{try{await rpc('retire');cleanup=true}catch(failure){cleanup=failure.message}worker.terminate();if(prelude)URL.revokeObjectURL(entry)}
    return {error,result,cleanup,progress,cachePages}
  },{asset,manifest,bytes:[...bytes],prelude,page,cacheReadControl})
}

// Finite task-owned inputs. One 64 KiB raw block, one gzip history and one
// compressed file; no raw fixture or full-input Buffer exists at scale.
async function scaleFixture(rawBytes,{signal}={}) {
  const scratch=resolve('../../.tmp/sealed-archives-e0ac6260')
  await mkdir(scratch,{recursive:true})
  const file=resolve(scratch,'browser-'+randomUUID()+'.gz')
  assert.ok(file.startsWith(scratch+sep))
  const rawHash=createHash('sha256'),compressedHash=createHash('sha256');let compressedBytes=0,seed=0x51a7
  const pattern=Array.from({length:128},()=>{seed^=seed<<13;seed^=seed>>>17;seed^=seed<<5;return String.fromCharCode(65+(seed>>>0)%26)}).join('')
  const object={type:'response_item',timestamp:'2026-10-28T00:00:00Z',payload:{type:'message',role:'assistant',content:[{type:'output_text',text:'row 00000000 λ😀 '+pattern}]}}
  const initial=Buffer.from(JSON.stringify(object)+'\r\n');object.payload.content[0].text+='A'.repeat(1024-initial.length)
  const template=Buffer.from(JSON.stringify(object)+'\r\n');assert.equal(template.length,1024)
  const counterAt=template.indexOf('00000000'),count=rawBytes/1024;assert.ok(Number.isSafeInteger(count))
  const gzip=createGzip({level:6,memLevel:8,strategy:zlibConstants.Z_DEFAULT_STRATEGY}),abort=new AbortController(),timer=setTimeout(()=>abort.abort(Error('fixture_generation_deadline')),120000)
  const externallyAborted=()=>abort.abort(signal.reason)
  if(signal?.aborted)externallyAborted();else signal?.addEventListener('abort',externallyAborted,{once:true})
  let compressedAt=0
  const binding=new Transform({transform(chunk,_,done){const bytes=Buffer.from(chunk);if(compressedAt<=9 && compressedAt+bytes.length>9)bytes[9-compressedAt]=255;compressedAt+=bytes.length;compressedBytes+=bytes.length;compressedHash.update(bytes);done(null,bytes)}})
  const writing=pipeline(gzip,binding,createWriteStream(file,{flags:'wx',highWaterMark:65536}),{signal:abort.signal})
  // Install a rejection handler immediately, while the producer feeds input.
  const receipt=writing.then(()=>({}),error=>({error}))
  try {
    for(let first=0;first<count;first+=64){
      abort.signal.throwIfAborted()
      const block=Buffer.alloc(Math.min(64,count-first)*1024)
      for(let row=0;row<block.length/1024;row++){template.copy(block,row*1024);block.write((first+row).toString(16).padStart(8,'0'),row*1024+counterAt,8,'ascii')}
      rawHash.update(block)
      if(!gzip.write(block))await once(gzip,'drain')
      if((first+64)%1024===0)await new Promise((yes,no)=>gzip.flush(zlibConstants.Z_BLOCK,error=>error?no(error):yes()))
    }
    gzip.end();const result=await receipt;if(result.error)throw result.error
    return {file,scratch,rawBytes,rawRecords:count,rawSha256:rawHash.digest('hex'),compressedBytes,compressedSha256:compressedHash.digest('hex'),recipe:'jsonl-1024-v1:xorshift32-0x51a7:hex-seq:utf8-crlf:gzip6-zblock-1MiB-os255',async dispose(){await unlink(file);try{await rmdir(scratch)}catch(error){if(error.code!=='ENOTEMPTY'&&error.code!=='ENOENT')throw error}}}
  }catch(error){abort.abort(error);await receipt;await unlink(file).catch(failure=>{if(failure.code!=='ENOENT')throw failure});throw error}
  finally{clearTimeout(timer);signal?.removeEventListener('abort',externallyAborted)}
}

async function fixtureHttp(fixture) {
  const active=new Set(),failures=[];let origin='',sent=0,preflightMs=0,revalidations=0
  const server=createServer((request,response)=>{
    const abort=new AbortController();response.on('close',()=>{if(!response.writableFinished)abort.abort()})
    const work=(async()=>{
      const began=performance.now(),digest=createHash('sha256');let count=0
      for await(const bytes of createReadStream(fixture.file,{highWaterMark:65536,signal:abort.signal})){count+=bytes.length;digest.update(bytes)}
      assert.equal(count,fixture.compressedBytes);assert.equal(digest.digest('hex'),fixture.compressedSha256);preflightMs+=performance.now()-began
      if(request.headers['if-none-match']){assert.equal(request.headers['if-none-match'],phaseArchiveETag);revalidations++;response.writeHead(304,{ETag:phaseArchiveETag,'Access-Control-Allow-Origin':origin,'Access-Control-Allow-Credentials':'true'});response.end();return}
      response.writeHead(200,{ETag:phaseArchiveETag,'Content-Type':'application/gzip','Content-Length':String(fixture.compressedBytes),'X-HMem-Archive-Generation':generation,'X-HMem-Archive-SHA256':fixture.compressedSha256,'X-HMem-Archive-Raw-Bytes':String(fixture.rawBytes),'X-HMem-Archive-Raw-SHA256':fixture.rawSha256,'Access-Control-Allow-Origin':origin,'Access-Control-Allow-Credentials':'true'})
      let timer;const start=performance.now()
      const limit=new Transform({transform(bytes,_,done){sent+=bytes.length;const delay=Math.max(0,sent*1000/12500000-(performance.now()-start));timer=setTimeout(()=>done(null,bytes),delay)},destroy(error,done){clearTimeout(timer);done(error)}})
      await pipeline(createReadStream(fixture.file,{highWaterMark:65536}),limit,response,{signal:abort.signal})
    })().catch(error=>{if(!abort.signal.aborted)failures.push({phase:'fixture-http',code:error.code || error.message});response.destroy()}).finally(()=>active.delete(work))
    active.add(work)
  })
  await new Promise(resolve=>server.listen(0,'127.0.0.1',resolve))
  return {url:'http://127.0.0.1:'+server.address().port+'/archive-object',setOrigin(value){origin=value},snapshot:()=>({sent,preflightMs,revalidations,failures}),async close(){server.closeAllConnections();await Promise.all([...active]);await new Promise((yes,no)=>server.close(error=>error?no(error):yes()))}}
}

function captureArchiveTelemetry(navigationDiagnostic=false) {
  const Native=Worker
  window.archiveTelemetry={workerEpochs:[],phases:{},maxPageBytes:0,maxRows:0,maxBodyBytes:0,maxChunk:0,maxNativeOutput:0}
  const nav=navigationDiagnostic?{worker:null,stamp:null,pending:null,next:-1,poisoned:false,completed:0,maximumPending:0}:null
  if(nav){
    window.archiveNavigationDiagnostic=nav
    window.archiveNavigationHelpers={observers:0,timers:0,completed:0,timedOut:0}
    nav.rpc=last=>new Promise((resolve,reject)=>{
      if(nav.poisoned||nav.pending){reject(Error('navigation_rpc_owner_occupied'));return}
      if(!nav.worker||!nav.stamp){reject(Error('navigation_rpc_owner_missing'));return}
      if(nav.next < -10){reject(Error('navigation_rpc_limit'));return}
      const request=nav.next--,pending={request,resolve,reject,timer:null}
      nav.pending=pending;nav.maximumPending=Math.max(nav.maximumPending,1)
      pending.timer=setTimeout(()=>{pending.timer=null;nav.poisoned=true;reject(Error('navigation_rpc_deadline'))},2000)
      try{nav.worker.postMessage({kind:'page',stamp:nav.stamp,request,page:last?{last:true}:{}})}
      catch(error){clearTimeout(pending.timer);pending.timer=null;nav.pending=null;nav.poisoned=true;reject(error)}
    })
  }
  window.Worker=class extends Native {
    constructor(url,options){if(nav?.worker)throw Error('navigation_multiple_workers');super(url,options);this.archiveURL=new URL(String(url),location.href).href;if(nav)nav.worker=this
      this.addEventListener('message',({data})=>{const metric=window.archiveTelemetry
      if(data.kind==='detached'&&this.epoch)this.epoch.detached=true
      if(data.kind==='progress'){metric.phases[data.phase]??=performance.now();metric.maxNativeOutput=Math.max(metric.maxNativeOutput,data.nativeOutputChunkBytes || 0)}
      if(data.kind==='page'){metric.maxPageBytes=Math.max(metric.maxPageBytes,new TextEncoder().encode(JSON.stringify(data)).length);metric.maxRows=Math.max(metric.maxRows,data.rows.length);for(const row of data.rows)if(row.body)metric.maxBodyBytes=Math.max(metric.maxBodyBytes,new TextEncoder().encode(row.body.text).length)}
      const pending=nav?.pending
      if(pending&&pending.request===data.request){
        clearTimeout(pending.timer);nav.pending=null;nav.completed++
        if(['workspace','session','generation','load','authEpoch'].some(key=>data[key]!==nav.stamp[key])){nav.poisoned=true;pending.reject(Error('navigation_rpc_stamp_mismatch'))}
        else if(data.kind==='error')pending.reject(Error(data.code))
        else if(data.kind!=='page'){nav.poisoned=true;pending.reject(Error('navigation_rpc_kind_mismatch'))}
        else pending.resolve({first:data.first,last:data.last,rows:data.rows.length})
      }
    })}
    terminate(){if(this.epoch)this.epoch.terminated=true;if(nav?.worker===this){nav.worker=null;nav.stamp=null}return super.terminate()}
    postMessage(message,transfer){if(['start','adopt'].includes(message.kind)){const epochs=window.archiveTelemetry.workerEpochs;if(epochs.length>=2)throw Error('archive_epoch_limit');this.epoch={phase:message.kind==='start'?'processor':'reader',incarnation:message.stamp.incarnation,targetURL:this.archiveURL,terminated:false,detached:false};epochs.push(this.epoch);if(nav)nav.stamp=message.stamp;}if(message.kind==='chunk'){if(message.bytes.byteLength!==65536||!Number.isInteger(message.length)||message.length<1||message.length>65536||transfer?.length!==1||transfer[0]!==message.bytes)throw Error('archive_chunk_transfer_bound');window.archiveTelemetry.maxChunk=Math.max(window.archiveTelemetry.maxChunk,message.length)}return super.postMessage(message,transfer)}
  }
}

function navigationInPage(action) {
  return new Promise((resolve,reject)=>{
    const viewport=document.getElementById('session-records'),before=viewport.querySelector('[data-offset]')?.getAttribute('data-offset'),start=performance.now(),metrics=window.archiveNavigationHelpers
    let timer,done=false,observing=false,timed=false
    const finish=error=>{
      if(done)return;done=true;if(observing){observer.disconnect();if(metrics)metrics.observers--}
      if(timed){clearTimeout(timer);if(metrics)metrics.timers--}if(metrics){metrics.completed++;if(error?.message==='local_navigation_deadline')metrics.timedOut++}
      error?reject(error):resolve(performance.now()-start)
    }
    const observer=new MutationObserver(()=>{const offset=viewport.querySelector('[data-offset]')?.getAttribute('data-offset');if(offset!==before&&document.querySelector('.session-sealed-viewer [role="status"]')?.textContent==='Verified complete archive.')finish()})
    try{
      observer.observe(document.querySelector('.session-sealed-viewer'),{childList:true,subtree:true,characterData:true});observing=true;if(metrics)metrics.observers++
      timer=setTimeout(()=>finish(Error('local_navigation_deadline')),2000);timed=true;if(metrics)metrics.timers++
      document.querySelector('[data-sealed-action="'+action+'"]').click()
    }catch(error){finish(error)}
  })
}
async function navigationSample(page,action) {return page.evaluate(navigationInPage,action)}

const NAVIGATION_BLOCKS=Object.freeze(['worker-only','dom','worker-only','dom'])
async function navigationDOMBlock(page,navigation) {
  for(let index=0;index<5;index++){
    navigation.push(await navigationSample(page,navigation.length%2?'first':'last'))
    const active=await page.evaluate(()=>window.archiveNavigationHelpers)
    assert.equal(active.observers,0);assert.equal(active.timers,0)
  }
}


test('navigation diagnostic helpers retire observers and timers on success, timeout and click failure',async()=>{
  const names=['window','document','MutationObserver','setTimeout','clearTimeout'],original=Object.fromEntries(names.map(name=>[name,Object.getOwnPropertyDescriptor(globalThis,name)]))
  try{for(const mode of ['success','timeout','click-failure']){
    let offset='first',observer,nextTimer=0,activeObservers=0
    const timers=new Map(),metrics={observers:0,timers:0,completed:0,timedOut:0}
    globalThis.window={archiveNavigationHelpers:metrics}
    globalThis.setTimeout=(callback,ms)=>{const id=++nextTimer;timers.set(id,{callback,ms});return id};globalThis.clearTimeout=id=>timers.delete(id)
    globalThis.MutationObserver=class {constructor(callback){this.callback=callback;observer=this}observe(){activeObservers++}disconnect(){activeObservers--}}
    const viewport={querySelector:()=>({getAttribute:()=>offset})}
    globalThis.document={getElementById:()=>viewport,querySelector:selector=>{
      if(selector.includes('[role='))return {textContent:'Verified complete archive.'}
      if(selector.includes('[data-sealed-action='))return {click(){
        if(mode==='click-failure')throw Error('injected_click_failure')
        if(mode==='timeout'){const [id,timer]=[...timers][0];assert.equal(timer.ms,2000);timers.delete(id);timer.callback()}
        else{offset='last';observer.callback()}
      }}
      return {}
    }}
    if(mode==='success')assert.ok(await navigationInPage('last')>=0)
    else await assert.rejects(navigationInPage('last'),mode==='timeout'?/local_navigation_deadline/:/injected_click_failure/)
    assert.equal(activeObservers,0);assert.equal(timers.size,0);assert.deepEqual(metrics,{observers:0,timers:0,completed:1,timedOut:mode==='timeout'?1:0})
  }}finally{for(const name of names)if(original[name])Object.defineProperty(globalThis,name,original[name]);else delete globalThis[name]}
})

test('navigation diagnostic helpers retain one timed-out negative RPC and discard page bodies',async()=>{
  const names=['window','Worker','location','setTimeout','clearTimeout'],original=Object.fromEntries(names.map(name=>[name,Object.getOwnPropertyDescriptor(globalThis,name)]))
  let nextTimer=0;const timers=new Map()
  try{
    globalThis.window={}
    globalThis.location={href:'http://fixture.invalid/'}
    globalThis.setTimeout=(callback,ms)=>{const id=++nextTimer;timers.set(id,{callback,ms});return id};globalThis.clearTimeout=id=>timers.delete(id)
    globalThis.Worker=class extends EventTarget {postMessage(message){this.lastMessage=message}}
    captureArchiveTelemetry(true)
    const worker=new window.Worker('fake'),stamp={workspace:'workspace',session:'session',generation:'generation',load:1,authEpoch:1},nav=window.archiveNavigationDiagnostic
    worker.postMessage({kind:'start',stamp})
    const first=nav.rpc(true),request=worker.lastMessage.request
    assert.equal(request,-1);assert.deepEqual(worker.lastMessage.page,{last:true});assert.equal([...timers.values()][0].ms,2000)
    await assert.rejects(nav.rpc(false),/navigation_rpc_owner_occupied/)
    worker.dispatchEvent(new MessageEvent('message',{data:{...stamp,request,kind:'page',first:'a',last:'z',rows:[{body:{text:'body must not be retained'}}]}}))
    assert.deepEqual(await first,{first:'a',last:'z',rows:1});assert.equal(nav.pending,null);assert.equal(timers.size,0)
    const held=nav.rpc(false),pending=nav.pending,[id,timer]=[...timers][0]
    assert.equal(worker.lastMessage.request,-2);timers.delete(id);timer.callback()
    await assert.rejects(held,/navigation_rpc_deadline/);assert.equal(nav.pending,pending);assert.equal(nav.worker,worker)
    await assert.rejects(nav.rpc(true),/navigation_rpc_owner_occupied/);assert.equal(nav.pending,pending)
    worker.dispatchEvent(new MessageEvent('message',{data:{...stamp,request:-2,kind:'page',first:'a',last:'z',rows:[]}}))
    assert.equal(nav.pending,null);assert.equal(nav.poisoned,true);assert.equal(nav.maximumPending,1);assert.equal(timers.size,0)
    await assert.rejects(nav.rpc(true),/navigation_rpc_owner_occupied/)
  }finally{for(const name of names)if(original[name])Object.defineProperty(globalThis,name,original[name]);else delete globalThis[name]}
})

test('navigation diagnostic helpers use the actual block helper for20operations and ten distinct DOM moves',async()=>{
  const names=['window','Worker','location','document','MutationObserver','setTimeout','clearTimeout'],original=Object.fromEntries(names.map(name=>[name,Object.getOwnPropertyDescriptor(globalThis,name)]))
  let offset='first',observer,nextTimer=0,workerOperations=0
  const timers=new Map(),actions=[],navigation=[]
  try{
    globalThis.window={}
    globalThis.location={href:'http://fixture.invalid/'}
    globalThis.setTimeout=(callback,ms)=>{const id=++nextTimer;timers.set(id,{callback,ms});return id};globalThis.clearTimeout=id=>timers.delete(id)
    globalThis.MutationObserver=class {constructor(callback){this.callback=callback;observer=this}observe(){}disconnect(){}}
    const viewport={get outerHTML(){return '<records page="'+offset+'"></records>'},querySelector:()=>({getAttribute:()=>offset})}
    globalThis.document={getElementById:()=>viewport,querySelector:selector=>{
      if(selector.includes('[role='))return {textContent:'Verified complete archive.'}
      const action=selector.match(/data-sealed-action="(first|last)"/)?.[1]
      return action?{click(){assert.notEqual(action,offset,'each actual DOM action must change offset');offset=action;actions.push(action);observer.callback()}}:{}
    }}
    globalThis.Worker=class extends EventTarget {postMessage(message){
      if(message.kind==='page'){
        assert.ok(message.request<0);workerOperations++
        queueMicrotask(()=>this.dispatchEvent(new MessageEvent('message',{data:{...message.stamp,request:message.request,kind:'page',first:'a',last:'z',rows:[]}})))
      }
    }}
    captureArchiveTelemetry(true)
    const worker=new window.Worker('fake'),stamp={workspace:'w',session:'s',generation:'g',load:1,authEpoch:1}
    worker.postMessage({kind:'start',stamp})
    const page={evaluate:(fn,arg)=>Promise.resolve(fn(arg))}
    for(const kind of NAVIGATION_BLOCKS){
      if(kind==='worker-only'){const before=viewport.outerHTML;for(let index=0;index<5;index++)await window.archiveNavigationDiagnostic.rpc(index%2===0);assert.equal(viewport.outerHTML,before)}
      else await navigationDOMBlock(page,navigation)
      assert.equal(window.archiveNavigationHelpers.observers,0);assert.equal(window.archiveNavigationHelpers.timers,0)
    }
    assert.deepEqual(actions,['last','first','last','first','last','first','last','first','last','first'])
    assert.equal(offset,'first');assert.equal(workerOperations,10);assert.equal(navigation.length,10);assert.equal(workerOperations+actions.length,20)
    assert.equal(window.archiveNavigationDiagnostic.completed,10);assert.equal(window.archiveNavigationHelpers.completed,10);assert.equal(timers.size,0)
  }finally{for(const name of names)if(original[name])Object.defineProperty(globalThis,name,original[name]);else delete globalThis[name]}
})

async function selectArchiveAcknowledgement(page,workspace,session) {
  return page.evaluate(({workspace,session})=>new Promise((resolve,reject)=>{
    const prior=new Map([...document.querySelectorAll('.session-sealed-viewer')].map(node=>[node,node.querySelector('[data-viewer-owner]')?.getAttribute('data-viewer-owner')]))
    const start=performance.now();let timer,done=false
    const finish=(error,value)=>{if(done)return;done=true;observer.disconnect();clearTimeout(timer);removeEventListener('pagehide',retired);error?reject(error):resolve(value)}
    const retired=()=>finish(Error('archive_ack_selection_retired'))
    const observer=new MutationObserver(()=>{
      if(location.pathname!=='/workspace/'+workspace || new URLSearchParams(location.hash.slice(1)).get('session')!==session)return
      for(const node of document.querySelectorAll('.session-sealed-viewer')){
        const owner=node.querySelector('[data-viewer-owner]')?.getAttribute('data-viewer-owner')
        if(!owner || prior.get(node)===owner)continue
        let stamp;try{stamp=JSON.parse(owner)}catch{continue}
        if(!Array.isArray(stamp) || stamp[0]!==workspace || stamp[3]!==session)continue
        const status=node.querySelector('[role="status"]')?.textContent || ''
        if(/^(Preparing complete archive|Downloading complete archive|Verifying and indexing)/.test(status)){finish(null,{ms:performance.now()-start,status:status.slice(0,128)});return}
      }
    })
    observer.observe(document.body,{childList:true,subtree:true,characterData:true,attributes:true,attributeFilter:['data-viewer-owner','class']})
    addEventListener('pagehide',retired,{once:true});timer=setTimeout(()=>finish(Error('archive_initial_ack_deadline')),2000)
    location.hash='tab=sessions&session='+session
  }),{workspace,session})
}

function installMatchedProbe() {
  const originalPush=window.pushHierarchyFrames
  const state={pending:null,last:null,observers:0,timers:0,completed:0}
  const ownerMatches=(owner,workspace,session)=>{try{const stamp=JSON.parse(owner);return Array.isArray(stamp)&&stamp[0]===workspace&&stamp[3]===session}catch{return false}}
  const begin=(kind,workspace,session,marker,eventId)=>{
    if(state.pending)throw Error('matched_probe_occupied')
    const viewport=document.getElementById('session-records')
    if(viewport?.textContent.includes(marker))throw Error('matched_marker_already_present')
    let yes,no
    const promise=new Promise((resolve,reject)=>{yes=resolve;no=reject});promise.catch(()=>{})
    const pending={kind,workspace,session,marker,eventId,start:null,promise,beforeOwner:viewport?.getAttribute('data-viewer-owner') || '',done:false}
    state.last=null;state.pending=pending
    const finish=(error,result)=>{
      if(pending.done)return;pending.done=true;observer.disconnect();state.observers--;clearTimeout(timer);state.timers--;state.completed++
      state.pending=null;state.last={marker,error,result};error?no(error):yes(result)
    }
    const observer=new MutationObserver(()=>{
      const node=document.getElementById('session-records'),owner=node?.getAttribute('data-viewer-owner') || ''
      if(pending.start===null||!node||!ownerMatches(owner,workspace,session)||!node.textContent.includes(marker)||(kind==='active'&&owner===pending.beforeOwner))return
      finish(null,{ms:performance.now()-pending.start,kind,marker,owner})
    })
    observer.observe(document.body,{childList:true,subtree:true,characterData:true,attributes:true,attributeFilter:['data-viewer-owner']});state.observers++
    const timer=setTimeout(()=>finish(Error('matched_DOM_deadline')),2000);state.timers++;pending.fail=error=>finish(error)
    return pending
  }
  window.pushHierarchyFrames=function(frames,...rest){
    const pending=state.pending
    if(pending?.kind==='live'&&pending.start===null&&frames.some(frame=>frame.event?.event_id===pending.eventId&&frame.event?.entity?.id===pending.session))pending.start=performance.now()
    return originalPush.call(this,frames,...rest)
  }
  const retired=()=>state.pending?.fail(Error('matched_probe_retired'))
  addEventListener('pagehide',retired)
  window.matchedProbe={
    active(workspace,session,marker){const pending=begin('active',workspace,session,marker);pending.start=performance.now();location.hash='tab=sessions&session='+session;return pending.promise},
    live(workspace,session,marker,eventId){begin('live',workspace,session,marker,eventId)},
    wait(marker){if(state.pending?.marker===marker)return state.pending.promise;if(state.last?.marker!==marker)throw Error('matched_marker_owner_mismatch');if(state.last.error)throw state.last.error;return state.last.result},
    stats:()=>({observers:state.observers,timers:state.timers,completed:state.completed}),
    close(){retired();window.pushHierarchyFrames=originalPush;removeEventListener('pagehide',retired)}
  }
}

test('matched active live probe binds actual owner markers, invalidation clock and zero teardown',async()=>{
  const names=['window','document','location','MutationObserver','performance','setTimeout','clearTimeout','addEventListener','removeEventListener'],prior=Object.fromEntries(names.map(name=>[name,Object.getOwnPropertyDescriptor(globalThis,name)]))
  let clock=10,observer,marker='',owner='old',next=0;const timers=new Map()
  const viewport={get textContent(){return marker},getAttribute:()=>owner}
  try{
    globalThis.window={pushHierarchyFrames(){marker='LiveMarker';clock=30;observer.callback()}}
    globalThis.document={body:{},getElementById:()=>viewport}
    Object.defineProperty(globalThis,'performance',{value:{now:()=>clock},configurable:true})
    globalThis.setTimeout=callback=>{const id=++next;timers.set(id,callback);return id};globalThis.clearTimeout=id=>timers.delete(id)
    globalThis.addEventListener=()=>{};globalThis.removeEventListener=()=>{}
    globalThis.MutationObserver=class{constructor(callback){this.callback=callback;observer=this}observe(){}disconnect(){}}
    globalThis.location={set hash(value){owner=JSON.stringify(['w',1,1,'wrong']);marker='ActiveMarker';clock=15;observer.callback();assert.equal(window.matchedProbe.stats().observers,1,'wrong transcript owner cannot finish');owner=JSON.stringify(['w',1,1,'s']);clock=20;observer.callback()}}
    installMatchedProbe();const probe=window.matchedProbe
    const active=await probe.active('w','s','ActiveMarker');assert.equal(active.ms,10);assert.equal(active.marker,'ActiveMarker');assert.equal(active.owner,owner)
    assert.deepEqual(probe.stats(),{observers:0,timers:0,completed:1});assert.equal(timers.size,0)
    clock=25;probe.live('w','s','LiveMarker','append-2')
    marker='LiveMarker';observer.callback();assert.equal(probe.stats().observers,1,'DOM marker without matching invalidation cannot finish')
    marker='';window.pushHierarchyFrames([{event:{event_id:'append-2',entity:{id:'s'}}}])
    const live=await probe.wait('LiveMarker');assert.equal(live.ms,5);assert.equal(live.kind,'live')
    assert.deepEqual(probe.stats(),{observers:0,timers:0,completed:2});assert.equal(timers.size,0)
    assert.throws(()=>probe.live('w','s','LiveMarker','append-3'),/already_present/)
    probe.live('w','s','MissingMarker','append-3');const missing=probe.wait('MissingMarker');[...timers.values()][0]();await assert.rejects(missing,/matched_DOM_deadline/)
    assert.equal(probe.stats().observers,0);assert.equal(probe.stats().timers,0);probe.close()
  }finally{for(const name of names)if(prior[name])Object.defineProperty(globalThis,name,prior[name]);else delete globalThis[name]}
})

async function matchedBundle(label,directory,expected) {
  const base=await realpath(directory),bodies=new Map(),manifest={}
  assert.equal((await lstat(directory)).isSymbolicLink(),false)
  for(const [name,digest]of Object.entries(expected)){
    assert.ok(name==='index.html'||/^assets\/[^/]+$/.test(name))
    const path=resolve(base,name);assert.ok(path.startsWith(base+sep));assert.equal((await lstat(path)).isSymbolicLink(),false)
    const bytes=await readFile(path);assert.equal(hash(bytes),digest,'served bundle '+label+' '+name)
    bodies.set(name,bytes);manifest[name]={sha256:digest,bytes:bytes.length}
  }
  return {label,bodies,manifest,served:{}}
}
async function serveMatchedBundle(h,bundle) {
  const documents=new Set(['/','/workspace/'+h.fixture.workspace.id])
  await h.page.route('**/*',async route=>{
    const request=route.request(),url=new URL(request.url())
    let key
    if(url.origin===h.origin&&request.isNavigationRequest()&&documents.has(url.pathname))key='index.html'
    else if(url.origin===h.origin&&url.pathname.startsWith('/assets/'))key=url.pathname.slice(1)
    if(key){
      assert.ok(bundle.bodies.has(key),'only manifest-bound static paths')
      bundle.served[key]=(bundle.served[key] || 0)+1
      return route.fulfill({contentType:key.endsWith('.js')?'application/javascript':key.endsWith('.css')?'text/css':'text/html',body:bundle.bodies.get(key)})
    }
    return route.fallback()
  })
}
function createMatchedFixtureOwner(scratch,{createDirectory=mkdir,launch=openSessionExplorer,removeProfile=async entry=>{
  if(!entry.creationStarted){entry.profileRemoved=true;return}
  if(!entry.created){
    try{await lstat(entry.profile)}catch(error){if(error.code==='ENOENT'){entry.profileRemoved=true;return}throw error}
    throw Error('matched_directory_creation_unconfirmed')
  }
  assert.equal(await realpath(entry.profile),entry.profile);assert.equal((await lstat(entry.profile)).isSymbolicLink(),false);assert.ok(entry.profile.startsWith(scratch+sep))
  await rm(entry.profile,{recursive:true,maxRetries:0});entry.profileRemoved=true
}}={}) {
  const retained=[globalThis.retainedPersistentFixtureLaunchOwner,globalThis.retainedArchiveQualificationOwner].find(value=>value&&!value.closed)
  if(retained){const error=Error('matched_fixture_owner_occupied');error.owner=retained;throw error}
  const owner={entries:[],closed:false,retiring:false,closing:null}
  globalThis.retainedPersistentFixtureLaunchOwner=owner
  owner.add=(profile,options)=>{
    if(owner.retiring||owner.closed)throw Error('matched_fixture_retired')
    assert.ok(owner.entries.length<2);assert.ok(profile.startsWith(scratch+sep));assert.equal(options.persistentProfile.path,profile)
    const entry={profile};owner.entries.push(entry)
    // The whole setup is registered before mkdir or launch can await.
    entry.settled=Promise.resolve().then(async()=>{
      if(owner.retiring)throw Error('matched_fixture_retired')
      entry.creationStarted=true;await createDirectory(profile);entry.created=true
      if(owner.retiring)throw Error('matched_fixture_retired')
      entry.launchStarted=true;entry.pending=launch(options)
      const h=await entry.pending;entry.h=h;return {h}
    }).catch(error=>{entry.failure=error;return {error}})
    return entry
  }
  owner.close=()=>{
    owner.retiring=true
    if(owner.closed)return Promise.resolve()
    if(owner.closing)return owner.closing
    const closing=(async()=>{
      const errors=[]
      for(const entry of owner.entries){
        try{
          const settled=await entry.settled,capability=settled.h || settled.error?.setupOwner
          if(entry.launchStarted&&!capability)throw new AggregateError([settled.error].filter(Boolean),'matched_setup_cleanup_owner_missing')
          if(!entry.browserClosed){if(capability)await capability.close();entry.browserClosed=true}
          if(!entry.profileRemoved)await removeProfile(entry)
        }catch(error){errors.push(error)}
      }
      if(errors.length)throw new AggregateError(errors,'matched_fixture_cleanup_failed')
      owner.closed=true
    })()
    owner.closing=closing;closing.catch(()=>{if(owner.closing===closing)owner.closing=null});return closing
  }
  return owner
}

test('matched fixture owner refuses preexisting admission without overwriting actual owner',async()=>{
  const names=['retainedPersistentFixtureLaunchOwner','retainedArchiveQualificationOwner'],prior=Object.fromEntries(names.map(name=>[name,Object.getOwnPropertyDescriptor(globalThis,name)]))
  try{for(const name of names){
    for(const key of names)delete globalThis[key]
    const existing={closed:false};globalThis[name]=existing
    assert.throws(()=>createMatchedFixtureOwner(resolve('matched-causal-scratch')),error=>error.message==='matched_fixture_owner_occupied'&&error.owner===existing)
    assert.equal(globalThis[name],existing)
  }}finally{for(const name of names)if(prior[name])Object.defineProperty(globalThis,name,prior[name]);else delete globalThis[name]}
})

test('matched fixture owner retirement joins held directory and late launch before admitting replacement',async()=>{
  const names=['retainedPersistentFixtureLaunchOwner','retainedArchiveQualificationOwner'],prior=Object.fromEntries(names.map(name=>[name,Object.getOwnPropertyDescriptor(globalThis,name)]))
  let releaseDirectory,releaseLaunch,releaseClose,first,second
  try{
    for(const name of names)delete globalThis[name]
    const scratch=resolve('matched-causal-scratch'),profile=resolve(scratch,'held-directory'),directoryGate=new Promise(resolve=>{releaseDirectory=resolve})
    let directoryStarted=false,launched=0,removed=0
    first=createMatchedFixtureOwner(scratch,{createDirectory:async()=>{directoryStarted=true;await directoryGate},launch:async()=>{launched++;return {close:async()=>{}}},removeProfile:async entry=>{assert.equal(entry.created,true);entry.profileRemoved=true;removed++}})
    const entry=first.add(profile,{persistentProfile:{path:profile}})
    await new Promise(resolve=>setImmediate(resolve));assert.equal(directoryStarted,true);assert.equal(first.entries[0],entry)
    const closing=first.close();assert.equal(first.retiring,true);assert.equal(first.closed,false)
    assert.throws(()=>first.add(resolve(scratch,'late'),{persistentProfile:{path:resolve(scratch,'late')}}),/matched_fixture_retired/)
    assert.throws(()=>createMatchedFixtureOwner(scratch),/matched_fixture_owner_occupied/)
    releaseDirectory();const result=await entry.settled;assert.match(result.error.message,/matched_fixture_retired/);await closing
    assert.equal(launched,0);assert.equal(removed,1);assert.equal(first.closed,true);assert.equal(entry.profileRemoved,true)

    const launchGate=new Promise(resolve=>{releaseLaunch=resolve}),closeGate=new Promise(resolve=>{releaseClose=resolve})
    const a=resolve(scratch,'late-launch'),bb=resolve(scratch,'already-launched');let closeA=0,closeB=0,removedSecond=0,launches=0
    const capA={async close(){closeA++;await closeGate}},capB={async close(){closeB++}}
    second=createMatchedFixtureOwner(scratch,{createDirectory:async()=>{},launch:options=>{launches++;return options.persistentProfile.path===a?launchGate:Promise.resolve(capB)},removeProfile:async entry=>{entry.profileRemoved=true;removedSecond++}})
    second.add(a,{persistentProfile:{path:a}});second.add(bb,{persistentProfile:{path:bb}})
    await new Promise(resolve=>setImmediate(resolve));assert.equal(launches,2);assert.equal(second.entries.length,2)
    const lateClosing=second.close();releaseLaunch(capA);await new Promise(resolve=>setImmediate(resolve))
    assert.equal(closeA,1);assert.equal(second.closed,false);assert.equal(globalThis.retainedPersistentFixtureLaunchOwner,second)
    assert.throws(()=>createMatchedFixtureOwner(scratch),error=>error.owner===second)
    releaseClose();await lateClosing;assert.equal(closeB,1);assert.equal(removedSecond,2);assert.equal(second.closed,true)
    assert.ok(second.entries.every(entry=>entry.browserClosed&&entry.profileRemoved))
  }finally{
    releaseDirectory?.();releaseLaunch?.({close:async()=>{}});releaseClose?.()
    await first?.close();await second?.close()
    for(const name of names)if(prior[name])Object.defineProperty(globalThis,name,prior[name]);else delete globalThis[name]
  }
})


function matchedPriorEvidence(previous,aggregateSha256) {
  if(!previous)return {}
  assert.ok(!previous.priorCandidate,'at most two candidate comparisons; no prior-candidate chain or further admission')
  const recordSha256=hash(JSON.stringify(previous))
  if(previous.status==='passed'){
    assert.ok(previous.cleanup?.closed&&previous.cleanup.profiles?.length===2&&previous.cleanup.profiles.every(profile=>profile.browserClosed&&profile.profileRemoved),'prior candidate must be fully closed')
    assert.equal(aggregateSha256,'14c3ad711e31d86abe3e718043da363901c5f4fc2ac38c33627b1ddd98d49171','only the approved prior aggregate')
    assert.equal(recordSha256,'1aef0e9245db4b6c2366cb8d0bf37649784c06c56bf7982cdc39aab2e1405f94','only the approved complete prior candidate')
    return {priorCandidate:{aggregateSha256,recordSha256,maxCandidateComparisons:2,receipt:previous}}
  }
  assert.ok(previous.status==='failed'&&!previous.priorSetupFailure&&!previous.p95&&previous.cleanup?.closed&&previous.bundles.every(bundle=>bundle.activeCount===0&&bundle.liveCount===0)&&previous.failure?.message.includes('archive-profile-'),'only the one approved zero-action profile-validation failure may be preserved on retry')
  return {priorSetupFailure:{aggregateSha256,recordSha256,phase:'persistent-profile-name-validation before measurements',interpretation:'Prior cleanup.launched records API invocation attempted, not observed native browser acquisition.',receipt:previous}}
}

test('matched receipt preserves one complete prior candidate and refuses further admission',async()=>{
  const bytes=await readFile('perf/session-archive.qualification.json'),evidence=await readQualificationEvidence(),current=evidence.matchedActiveLive
  const previous=current.priorCandidate?.receipt || current,aggregate=current.priorCandidate?.aggregateSha256 || hash(bytes),before=JSON.stringify(previous),attemptsBefore=JSON.stringify(evidence.attempts)
  const prior=matchedPriorEvidence(previous,aggregate)
  assert.equal(prior.priorCandidate.receipt,previous);assert.equal(JSON.stringify(prior.priorCandidate.receipt),before)
  assert.equal(hash(JSON.stringify(prior.priorCandidate.receipt)),'1aef0e9245db4b6c2366cb8d0bf37649784c06c56bf7982cdc39aab2e1405f94')
  assert.deepEqual(prior.priorCandidate.receipt.priorSetupFailure,previous.priorSetupFailure)
  assert.throws(()=>matchedPriorEvidence({...previous,...prior},aggregate),/no prior-candidate chain/)
  assert.throws(()=>matchedPriorEvidence({...previous,cleanup:{...previous.cleanup,closed:false}},aggregate),/fully closed/)
  assert.throws(()=>matchedPriorEvidence(previous,'0'.repeat(64)),/approved prior aggregate/)
  assert.throws(()=>matchedPriorEvidence({...previous,purpose:'changed'},aggregate),/approved complete prior candidate/)
  const projected=structuredClone(previous);delete projected.priorSetupFailure;Object.assign(projected,prior)
  assert.ok(Buffer.byteLength(JSON.stringify(projected))<=65536)
  assert.equal(JSON.stringify(previous),before);assert.equal(JSON.stringify(evidence.attempts),attemptsBefore)
})

test('matched active live comparison uses historical and current served bundles',{skip:process.env.HMEM_ARCHIVE_MATCHED!=='1',timeout:180000},async()=>{
  const evidence=await readQualificationEvidence(),path=resolve('perf/session-archive.qualification.json')
  const previousMatched=evidence.matchedActiveLive
  const priorMatched=matchedPriorEvidence(previousMatched,hash(await readFile(path)))
  assert.ok(Buffer.byteLength(JSON.stringify(evidence,null,2))+65536<=1048576,'reserve bounded matched receipt before launch')
  assert.ok(!globalThis.retainedArchiveQualificationOwner||globalThis.retainedArchiveQualificationOwner.closed)
  assert.ok(!globalThis.retainedPersistentFixtureLaunchOwner||globalThis.retainedPersistentFixtureLaunchOwner.closed)
  const image=evidence.browserImage;assert.equal(await hashFile(image.path),image.sha256)
  const scratch=await realpath(resolve('../../.tmp/sealed-archives-e0ac6260')),baselineRoot=resolve(scratch,'baseline-static'),buildPath=resolve(scratch,'historical-b742/build-inputs.json'),buildBytes=await readFile(buildPath),build=JSON.parse(buildBytes)
  assert.equal(build.sourceRevision,'b7428aa960d0b526fb85e33146f36374f3b8b109');assert.equal(build.zipSha256,'ccf428aa6a87a80ad1a83c9dd12f4356784d9587dc462f89056747c8492c49b8')
  const common={'assets/index-BNSYx-iy.css':'1e1ba27f0dfbe1d166d59a3f6e9fe8a39f6599042ae0c289629167a8aa8f2101','assets/vendor-W2tPfneV.js':'c533b37dd70e3e6478382630cbd9ba1d3ced3d6e0ff0992c431effab0afe3147'}
  const oldExpected={...common,'index.html':'0a51be961ba5d303fd301ca9be9410c3afe36bb1be4cac22cb945c99dcc9f5cf','assets/index-Ym7d4U9A.js':'0e42b9f42c24a5af117e0d15df95209212d858d09c62fd3ee0ea5c0c4bfa2a35'}
  for(const [name,digest]of Object.entries(oldExpected))assert.equal(build.assets[name],digest)
  const currentExpected={}
  for(const name of ['index.html',...(await readdir('../static/assets')).map(name=>'assets/'+name)]){assert.ok(name==='index.html'||/^assets\/[^/]+$/.test(name));currentExpected[name]=hash(await readFile(resolve('../static',name)))}
  const bundles=[await matchedBundle('baseline',baselineRoot,oldExpected),await matchedBundle('current',resolve('../static'),currentExpected)]
  const stamp=new Date().toISOString(),runtime={model:'matched-model',provider:'fixture',reasoning:'high',role:'developer'}
  const receipt={status:'running',purpose:'matched active-session selection and live-update DOM commits',contextMode:'two task-owned normal persistent profiles; one measured action at a time',warmupsPerAction:5,measurementsPerAction:40,order:'baseline/current on even rounds, current/baseline on odd rounds',clock:'selection hash action or exact append invalidation to actual transcript-owner plus unique rendered marker MutationObserver commit',bodyMs:120000,actionMs:2000,baselineRevision:build.sourceRevision,baselineBuildManifestSha256:hash(buildBytes),baselineZipSha256:build.zipSha256,sourceSha256:hash(await readFile('tests-js/session-archive.browser.test.js')),helperSha256:hash(await readFile('tests-js/hierarchy-fixture.mjs')),sessionFixtureSha256:hash(await readFile('tests-js/support/sessions-ux-fixture.mjs')),nodeImage:{path:process.execPath,sha256:await hashFile(process.execPath),version:process.version},browserImage:image,environment:{platform:os.platform(),release:os.release(),arch:os.arch(),cpu:os.cpus()[0]?.model,logicalCpus:os.cpus().length},fixture:{stamp,runtime},bundles:[]}
  Object.assign(receipt,priorMatched)
  const owner=createMatchedFixtureOwner(scratch)
  let primary,rejectDeadline
  const deadline=new Promise((_,reject)=>{rejectDeadline=reject});deadline.catch(()=>{})
  const began=performance.now(),timer=setTimeout(()=>{const error=Error('matched_owned_body_deadline');rejectDeadline(error);owner.retirement=owner.close();owner.retirement.catch(value=>{owner.retirementError=value})},120000)
  const current=()=>{if(owner.retiring||performance.now()-began>=120000)throw Error('matched_owned_body_deadline')}
  try{
    for(const bundle of bundles){
      current();const profile=resolve(scratch,'archive-profile-'+randomUUID())
      const entry=owner.add(profile,{stamp,runtime,persistentProfile:{path:profile,executable:image.path}});entry.bundle=bundle.label
      const settled=await Promise.race([entry.settled,deadline]);current();if(settled.error)throw settled.error
      const h=settled.h;entry.h=h;bundle.h=h;assert.equal(h.active.state,'active');assert.equal(h.browserImage.mode,'persistent')
      assert.equal(await hashFile(h.browserImage.executable),image.sha256);await serveMatchedBundle(h,bundle);await h.start();await h.idle()
      await h.page.evaluate(installMatchedProbe)
      const cdp=await h.bounded(h.page.context().browser().newBrowserCDPSession(),2000,'Matched browser identity')
      try{bundle.version=await h.bounded(cdp.send('Browser.getVersion'),2000,'Matched browser version');assert.equal((await h.bounded(cdp.send('Target.getTargets'),2000,'Matched target identity')).targetInfos.filter(value=>value.type==='worker').length,0)}finally{await h.bounded(cdp.detach(),2000,'Matched identity detach')}
      bundle.active=[];bundle.live=[];bundle.fixtureHash=hash(JSON.stringify(h.fixture));bundle.initialBytesSha256=hash(h.bytes);bundle.viewport=h.page.viewportSize()
      if(bundle!==bundles[0]){assert.equal(bundle.fixtureHash,bundles[0].fixtureHash);assert.equal(bundle.initialBytesSha256,bundles[0].initialBytesSha256);assert.deepEqual(bundle.viewport,bundles[0].viewport);assert.deepEqual(bundle.version,bundles[0].version)}
    }
    for(let round=0;round<45;round++)for(const bundle of round%2?[bundles[1],bundles[0]]:bundles){
      current();const h=bundle.h,activeMarker='MatchedActiveProbe'+String(round).padStart(3,'0'),liveMarker='MatchedLiveProbe'+String(round).padStart(3,'0')
      await h.page.evaluate(()=>{location.hash='tab=sessions'});await h.page.waitForFunction(()=>!document.getElementById('session-records'),null,{timeout:2000});await h.idle()
      await h.appendRecords({type:'response_item',timestamp:stamp,payload:{type:'message',role:'assistant',content:[{type:'output_text',text:activeMarker}]}});await h.idle()
      const active=await h.page.evaluate(({workspace,session,marker})=>window.matchedProbe.active(workspace,session,marker),{workspace:h.fixture.workspace.id,session:h.active.id,marker:activeMarker})
      assert.equal(active.marker,activeMarker);await h.idle()
      await h.page.evaluate(({workspace,session,marker,eventId})=>window.matchedProbe.live(workspace,session,marker,eventId),{workspace:h.fixture.workspace.id,session:h.active.id,marker:liveMarker,eventId:'append-'+(h.active.revision+1)})
      await h.appendRecords({type:'response_item',timestamp:stamp,payload:{type:'message',role:'assistant',content:[{type:'output_text',text:liveMarker}]}})
      const live=await h.page.evaluate(marker=>window.matchedProbe.wait(marker),liveMarker);assert.equal(live.marker,liveMarker)
      const stats=await h.page.evaluate(()=>window.matchedProbe.stats());assert.equal(stats.observers,0);assert.equal(stats.timers,0);bundle.lastHelpers=stats;await h.idle()
      if(round>=5){bundle.active.push(active.ms);bundle.live.push(live.ms)}
    }
    for(const bundle of bundles){
      assert.equal(bundle.active.length,40);assert.equal(bundle.live.length,40);assert.deepEqual(bundle.h.errors,[])
      assert.ok(!bundle.h.reads.some(row=>row.path.includes('/archive')||row.path.endsWith('/evidence')),'both targets use active/live transcript paths')
      for(const name of Object.keys(bundle.manifest).filter(name=>!name.includes('session-archive-worker')))assert.ok(bundle.served[name]>=1,'actual served body '+name)
      receipt.bundles.push({name:bundle.label,activeCount:40,liveCount:40,activeMs:bundle.active,liveMs:bundle.live,manifest:bundle.manifest,served:bundle.served,version:bundle.version,viewport:bundle.viewport,fixtureHash:bundle.fixtureHash,initialBytesSha256:bundle.initialBytesSha256,finalBytesSha256:hash(bundle.h.content.get(bundle.h.active.id)),helpers:await bundle.h.page.evaluate(()=>window.matchedProbe.stats())})
    }
    for(const bundle of bundles)for(const [name,entry]of Object.entries(bundle.manifest)){const path=resolve(bundle.label==='baseline'?baselineRoot:resolve('../static'),name);assert.equal(await hashFile(path),entry.sha256,'bundle unchanged after measured actions')}
    const p95=values=>[...values].sort((a,b)=>a-b)[Math.ceil(values.length*.95)-1]
    receipt.p95={baseline:{active:p95(bundles[0].active),live:p95(bundles[0].live)},current:{active:p95(bundles[1].active),live:p95(bundles[1].live)}}
    receipt.ratios={active:receipt.p95.current.active/receipt.p95.baseline.active,live:receipt.p95.current.live/receipt.p95.baseline.live}
    assert.ok(receipt.ratios.active<=1.10,'matched active selection p95 regression');assert.ok(receipt.ratios.live<=1.10,'matched live update p95 regression');receipt.status='passed'
  }catch(error){primary=error;receipt.status='failed';receipt.failure={message:error.message}}
  finally{
    clearTimeout(timer)
    receipt.bundles=bundles.map(bundle=>({name:bundle.label,activeCount:bundle.active?.length || 0,liveCount:bundle.live?.length || 0,activeMs:bundle.active || [],liveMs:bundle.live || [],manifest:bundle.manifest,served:bundle.served,version:bundle.version,viewport:bundle.viewport,fixtureHash:bundle.fixtureHash,initialBytesSha256:bundle.initialBytesSha256,finalBytesSha256:bundle.h?hash(bundle.h.content.get(bundle.h.active.id)):undefined,helpers:bundle.lastHelpers}))
    const probes={async close(){const errors=[];for(const entry of owner.entries)if(entry.h)try{await entry.h.bounded(entry.h.page.evaluate(()=>window.matchedProbe?.close()),2000,'Matched probe cleanup')}catch(error){errors.push(error)}if(errors.length)throw new AggregateError(errors,'matched_probe_cleanup_failed')},confirmFixtureClosed(){}}
    try{await closeIsolateAndFixture(probes,{h:owner},primary)}catch(error){primary=error;receipt.status='failed';receipt.cleanupFailure={message:error.message,causes:error.errors?.map(value=>value.message).slice(0,3)}}
    receipt.elapsedMs=performance.now()-began;receipt.cleanup={closed:owner.closed,profiles:owner.entries.map(entry=>({bundle:entry.bundle,launchRequested:!!entry.launchStarted,browserObserved:!!(entry.h?.browserImage?.pid||entry.failure?.setupOwner?.browser),browserPid:entry.h?.browserImage?.pid,directoryCreated:!!entry.created,browserClosed:!!entry.browserClosed,profileRemoved:!!entry.profileRemoved}))}
  }
  assert.ok(Buffer.byteLength(JSON.stringify(receipt,null,2))<=65536,'bounded matched receipt')
  const latest=await readQualificationEvidence();assert.equal(JSON.stringify(latest.matchedActiveLive || null),JSON.stringify(previousMatched || null),'prior matched receipt unchanged before atomic replacement');latest.matchedActiveLive=receipt
  const encoded=JSON.stringify(latest,null,2)+'\n';assert.ok(Buffer.byteLength(encoded)<=1048576)
  const temporary=resolve(scratch,'matched-qualification-'+randomUUID()+'.tmp');assert.ok(temporary.startsWith(scratch+sep))
  let created=false,writeFailure
  try{await writeFile(temporary,encoded,{flag:'wx'});created=true;await rename(temporary,path)}
  catch(error){writeFailure=error;error.qualificationTemporary=temporary;throw error}
  finally{if(created)try{await unlink(temporary)}catch(error){if(error.code!=='ENOENT')throw writeFailure?new AggregateError([writeFailure,error],'matched_receipt_cleanup_failed'):error}}

  console.log(JSON.stringify({matchedActiveLive:{status:receipt.status,p95:receipt.p95,ratios:receipt.ratios,cleanup:receipt.cleanup,sha256:hash(encoded)}}))
  if(primary)throw primary
})



// Product measurements finish before the unchanged diagnostic disk verification.
async function measuredNavigationThenVerification({current,navigate,measure,verify}) {
  current();await navigate();current()
  const measurement=await measure();current()
  const verification=await verify();current()
  return {measurement,verification}
}
test('product memory scope completes navigation and samples before held disk verification and preserves verification failure',async()=>{
  for(const failure of [false,true]){
    const order=['ready'];let release,success=false,closed=false,verificationStarted=false
    const terminal=new Promise(resolve=>{release=resolve}),primary=Error('disk_count_mismatch')
    const operation=(async()=>{
      try{
        const result=await measuredNavigationThenVerification({current(){},navigate:async()=>{for(let index=0;index<20;index++)order.push(index%2?'first':'last');order.push('final-last')},measure:async()=>{order.push('final-native');await Promise.resolve();order.push('final-aux');return {increment:1}},verify:async()=>{verificationStarted=true;order.push('start-counts');order.push('requests-succeeded');await terminal;order.push('idb-terminal');order.push('db-close');order.push('all-disk-assertions');if(failure)throw primary;return {verified:true}}})
        assert.equal(result.measurement.increment,1);assert.equal(result.verification.verified,true);success=true;order.push('success')
      }finally{await cleanupQualificationOwner({h:{close:async()=>{closed=true;order.push('fixture-close')}}})}
    })();operation.catch(()=>{})
    try{
      for(let turn=0;turn<10&&!verificationStarted;turn++)await Promise.resolve()
      assert.equal(verificationStarted,true);assert.deepEqual(order,['ready',...Array.from({length:20},(_,index)=>index%2?'first':'last'),'final-last','final-native','final-aux','start-counts','requests-succeeded'])
      assert.equal(success,false);assert.equal(closed,false)
    }finally{release()}
    if(failure)await assert.rejects(operation,error=>error===primary);else await operation
    assert.equal(success,!failure);assert.equal(closed,true)
    assert.deepEqual(order.slice(-5),failure?['requests-succeeded','idb-terminal','db-close','all-disk-assertions','fixture-close']:['idb-terminal','db-close','all-disk-assertions','success','fixture-close'])
  }
})

// Ready is observation1; the previous large case had94 post-baseline samples.
// Idle callbacks may only wait or sample: disk verification runs after all94.
async function observeReadyIdle({current,sample,wait,record,count=94}) {
  assert.equal(count,94,'fixed post-baseline observation count')
  for(let index=0;index<count;index++){
    current();if(index)await wait(1000);current()
    const value=await sample(index===0?'ready':'idle');current();record(value,index)
  }
}
async function archiveDiskReceipt(page) {
  return page.evaluate(async()=>{
    const root=await(await navigator.storage.getDirectory()).getDirectoryHandle('hmem-session-archives-v1'),files={};let owner
    for await(const[name,entry]of root.entries()){owner=name;for await(const[fileName,handle]of entry.entries())if(handle.kind==='file')files[fileName]=(await handle.getFile()).size}
    const opening=indexedDB.open('hmem-session-archive-v1-'+owner),db=await new Promise((yes,no)=>{opening.onsuccess=()=>yes(opening.result);opening.onerror=()=>no(opening.error)})
    try{
      const tx=db.transaction(['records','pieces'],'readonly'),terminal=new Promise((yes,no)=>{tx.oncomplete=yes;tx.onabort=()=>no(tx.error || Error('disk_verification_aborted'))});terminal.catch(()=>{})
      const verifyRecordPages=()=>new Promise((yes,no)=>{
        const operation=tx.objectStore('records').openCursor()
        let offset=0n,records=0,finished=false
        operation.onerror=()=>no(operation.error)
        operation.onsuccess=()=>{
          try{
            const cursor=operation.result
            if(!cursor){
              if(offset!==BigInt(files['raw.jsonl']))throw Error('record_pages_do_not_cover_raw_file')
              yes(records);return
            }
            const {spans}=cursor.value
            if(finished || cursor.key!==offset.toString().padStart(19,'0') || !Array.isArray(spans) || !spans.length || spans.length>256)throw Error('invalid_record_page')
            for(let index=0;index<spans.length;index++){
              const span=spans[index],delimiter=span%4,length=Math.floor(span/4)
              if(!Number.isSafeInteger(span) || delimiter<0 || delimiter>2 || length<=delimiter || length>1048576+delimiter || (!delimiter && index!==spans.length-1))throw Error('invalid_record_span')
              offset+=BigInt(length);records++
            }
            finished=spans.length<256 || spans.at(-1)%4===0
            cursor.continue()
          }catch(error){no(error)}
        }
      })
      const verifyPiecePages=()=>new Promise((yes,no)=>{
        const operation=tx.objectStore('pieces').openCursor()
        let pieces=0,previous='',packedBytes=0,singleBytes=0
        operation.onerror=()=>no(operation.error)
        operation.onsuccess=()=>{
          try{
            const cursor=operation.result
            if(!cursor){yes({pieces,packedBytes,singleBytes});return}
            const value=cursor.value
            const accept=key=>{
              if(typeof key!=='string'||key<=previous)throw Error('unordered_piece_page')
              previous=key;pieces++
            }
            if(value.data){
              const data=value.data,view=new DataView(data.buffer,data.byteOffset,data.byteLength)
              const count=view.getUint16(6,true)
              if(view.getUint32(0,true)!==0x58494d48||view.getUint8(4)!==1||view.getUint8(5)!==1
                || count<1||count>256||view.getUint32(8,true)!==data.byteLength||data.byteLength>1048576)throw Error('invalid_piece_page')
              let at=12
              for(let index=0;index<count;index++){
                const offset=view.getBigUint64(at,true),block=view.getUint32(at+8,true),length=view.getUint32(at+12,true)
                if(offset>9223372036854775807n||block>9999999||length<41||at+length>data.byteLength)throw Error('invalid_piece_page')
                accept(offset.toString().padStart(19,'0')+':'+String(block).padStart(7,'0'))
                at+=length
              }
              if(at!==data.byteLength)throw Error('invalid_piece_page')
              packedBytes+=data.byteLength
            }else{
              const bytes=new TextEncoder().encode(JSON.stringify(value)).byteLength
              if(bytes>1048576)throw Error('invalid_piece_page')
              accept(value.key);singleBytes+=bytes
            }
            if(previous!==cursor.key)throw Error('invalid_piece_page')
            cursor.continue()
          }catch(error){no(error)}
        }
      })
      let records,index,primary;try{[records,index]=await Promise.all([verifyRecordPages(),verifyPiecePages()])}catch(error){primary=error}
      try{await terminal}catch(error){throw primary || error}if(primary)throw primary
      return {files,records,...index,estimate:await navigator.storage.estimate()}
    }finally{db.close()}
  })
}

test('ready idle control has exactly94 observations and owned retirement stops further sampling',async()=>{
  const calls=[],controller=new AbortController();let samples=0
  await observeReadyIdle({current:()=>controller.signal.throwIfAborted(),wait:async ms=>{assert.equal(ms,1000);calls.push('wait')},sample:async phase=>{calls.push(phase);return ++samples},record:(value,index)=>assert.equal(value,index+1)})
  assert.equal(samples,94);assert.equal(calls.filter(value=>value==='wait').length,93);assert.equal(calls[0],'ready');assert.equal(calls.at(-1),'idle')
  let heldRelease,actualClose=false;const held=new Promise(resolve=>{heldRelease=resolve}),abort=new AbortController(),owner={h:{close:async()=>{await held;actualClose=true}}}
  let sampled=0,recorded=0
  const observing=observeReadyIdle({current:()=>abort.signal.throwIfAborted(),sample:async()=>++sampled,record:()=>recorded++,wait:async()=>{abort.abort(Error('idle_owned_deadline'));await held}})
  observing.catch(()=>{})
  await Promise.resolve();await Promise.resolve();const cleanup=cleanupQualificationOwner(owner,20)
  try{await assert.rejects(cleanup,/cleanup_deadline/);assert.equal(owner.closed,false);assert.equal(globalThis.retainedArchiveQualificationOwner,owner);assert.equal(sampled,1);assert.equal(recorded,1)}finally{heldRelease();await owner.actualCleanup}
  await assert.rejects(observing,/idle_owned_deadline/);assert.equal(actualClose,true);assert.equal(owner.closed,true);assert.equal(sampled,1);assert.equal(globalThis.retainedArchiveQualificationOwner,undefined)
})

test('ready archive idle same-cadence memory control',{skip:process.env.HMEM_ARCHIVE_READY_IDLE!=='1',timeout:220000},async()=>{
  for(const flag of ['HMEM_ARCHIVE_SCALE','HMEM_ARCHIVE_MEMORY_CAUSAL','HMEM_ARCHIVE_NAV_CAUSAL','HMEM_ARCHIVE_HEAP_AUXILIARY','HMEM_ARCHIVE_MATCHED'])assert.notEqual(process.env[flag],'1','idle control has a distinct purpose')
  const path=resolve('perf/session-archive.qualification.json'),evidence=await readQualificationEvidence(path),before=hash(await readFile(path)),scratch=resolve('../../.tmp/sealed-archives-e0ac6260')
  assert.ok(evidence.diagnostics.length<4);assert.ok(Buffer.byteLength(JSON.stringify(evidence))+131072<=1048576,'reserve bounded idle receipt before launch')
  const image=evidence.browserImage;assert.equal(await hashFile(image.path),image.sha256)
  const receipt={kind:'ready-archive-idle-same-dump-cadence',status:'running',at:new Date().toISOString(),purpose:'Natural application idle plus measurement; application timers remain active. Diagnostic only, not pure tracing or resource acceptance; no subtraction from prior failures.',contextMode:'task-owned persistent normal profile',observationsAfterBaseline:94,expectedTotalNativeSamples:95,cadenceMs:1000,memoryDumpMode:'background',deterministic:false,limits:{ownedBodyMs:180000,commandMs:2000,dumpMs:10000,lastSamples:8},phaseBoundary:'No page, navigation or disk-count operations between Ready and final idle measurement; disk verification follows all measurements.',browserImage:image,inputHashes:{},samples:[],maxima:{privateBytes:null,incrementBytes:null,perProcess:{},isolates:{},gapMs:0}}
  for(const input of ['src/session-archive-store.js','src/session-archive-worker.js','tests-js/session-archive.browser.test.js','tests-js/hierarchy-fixture.mjs','tests-js/support/sessions-ux-fixture.mjs','tests-js/support/session-archive-memory.mjs','../static/index.html',...Object.keys(await readdir('../static/assets').then(names=>Object.fromEntries(names.map(name=>['../static/assets/'+name,true]))))])receipt.inputHashes[input]=await hashFile(input)
  receipt.nodeImage={path:process.execPath,sha256:await hashFile(process.execPath)}
  const body={owner:null,abort:new AbortController(),current(){this.abort.signal.throwIfAborted()}},began=performance.now()
  const timer=setTimeout(()=>{body.abort.abort(Error('ready_idle_owned_deadline'));if(body.owner)body.retirement=body.owner.close().then(()=>{body.retirementResult={closed:true}},error=>{body.retirementResult={failure:error.message}})},180000)
  let fixture,http,profile,launch,h,isolate,primary
  try{
    fixture=await scaleFixture(1048576,{signal:body.abort.signal});body.current();http=await fixtureHttp(fixture);body.current()
    profile=resolve(scratch,'archive-profile-'+randomUUID());await mkdir(profile);body.current();launch=ownedPersistentFixture({path:profile,executable:image.path});body.owner=launch;h=await launch.get();body.current();receipt.actualBrowser=h.browserImage;http.setOrigin(h.origin)
    Object.assign(h.saved,{committed_bytes:fixture.rawBytes,committed_records:fixture.rawRecords,final_bytes:fixture.rawBytes,final_sha256:fixture.rawSha256})
    const manifest={...makeManifest(h,Buffer.alloc(0),Buffer.alloc(0)),raw_bytes:String(fixture.rawBytes),raw_records:String(fixture.rawRecords),raw_sha256:fixture.rawSha256,compressed_bytes:String(fixture.compressedBytes),compressed_sha256:fixture.compressedSha256}
    await h.page.route('**/sessions/**/archive**',phaseArchiveRoute(route=>new URL(route.request().url()).pathname.endsWith('/content')?route.continue({url:http.url}):route.fulfill({json:manifest})))
    await h.page.addInitScript(captureArchiveTelemetry)
    await h.start();await h.idle();await h.page.evaluate(id=>{location.hash='tab=sessions&session='+id},h.active.id);await h.page.locator('#session-records .session-record').first().waitFor();await h.idle();body.current()
    const baseline=await boundedMemorySnapshot(h,{levelOfDetail:'background'});assert.equal(baseline.workerTargets.length,0)
    assert.equal(await h.page.evaluate(async()=>{try{await(await navigator.storage.getDirectory()).getDirectoryHandle('hmem-session-archives-v1');return true}catch(error){if(error.name==='NotFoundError')return false;throw error}}),false)
    assert.ok(baseline.processes.some(value=>value.pid===h.browserImage.pid&&value.type==='browser'))
    isolate=await openIsolateDiagnostic(h);receipt.baseline={privateBytes:baseline.totalPrivateBytes,processes:baseline.processes.map(value=>({pid:value.pid,type:value.type,bytes:parseInt(value.totals.private_footprint_bytes,16)})),attribution:baseline.attribution,trace:baseline.trace,traceLoss:baseline.traceLoss,isolates:await isolate.snapshot()}
    const start=performance.now();receipt.initialAck=await selectArchiveAcknowledgement(h.page,h.fixture.workspace.id,h.saved.id);await h.page.getByText('Verified complete archive.',{exact:true}).waitFor({timeout:10000});body.current();receipt.fullViewMs=performance.now()-start
    let lastAt=0,lastHeartbeat=0
    await observeReadyIdle({current:()=>body.current(),wait:ms=>new Promise(resolve=>setTimeout(resolve,ms)),sample:async phase=>{const sampleStart=performance.now(),memory=await boundedMemorySnapshot(h,{levelOfDetail:'background'}),isolates=await isolate.snapshot({requireWorker:true});return {phase,memory,isolates,atMs:performance.now()-start,dumpMs:performance.now()-sampleStart}},record:({phase,memory,isolates,atMs,dumpMs},index)=>{
      assert.equal(memory.traceLoss,false);assert.equal(memory.workerTargets.length,1);assert.deepEqual(memory.processes.map(value=>value.pid).sort((a,b)=>a-b),baseline.processes.map(value=>value.pid).sort((a,b)=>a-b))
      const sample={observation:index+1,phase,atMs,dumpMs,privateBytes:memory.totalPrivateBytes,incrementBytes:memory.totalPrivateBytes-baseline.totalPrivateBytes,processes:memory.processes.map(value=>({pid:value.pid,type:value.type,bytes:parseInt(value.totals.private_footprint_bytes,16)})),attribution:memory.attribution,trace:memory.trace,traceLoss:memory.traceLoss,isolates}
      receipt.samples.push(sample);if(receipt.samples.length>8)receipt.samples.shift();receipt.actualObservations=index+1
      if(index===0)receipt.ready=sample
      if(receipt.maxima.privateBytes===null||sample.privateBytes>receipt.maxima.privateBytes){receipt.maxima.privateBytes=sample.privateBytes;receipt.maxima.incrementBytes=sample.incrementBytes;receipt.maxima.sample=sample}
      receipt.maxima.gapMs=Math.max(receipt.maxima.gapMs,atMs-lastAt);lastAt=atMs
      for(const process of sample.processes){const old=receipt.maxima.perProcess[process.pid];if(!old||process.bytes>old.bytes)receipt.maxima.perProcess[process.pid]={...process,atMs,observation:index+1}}
      for(const[kind,value]of Object.entries(isolates)){if(!value||!['page','worker'].includes(kind))continue;const key=kind==='worker'?'worker:'+value.phase+':'+value.incarnation:kind;const maxima=receipt.maxima.isolates[key] ||= {isolateId:value.isolateId};assert.equal(maxima.isolateId,value.isolateId);for(const field of ['usedSize','totalSize','embedderHeapUsedSize','backingStorageSize'])maxima[field]=Math.max(maxima[field] || 0,value[field])}
      if(atMs-lastHeartbeat>=15000){lastHeartbeat=atMs;console.log(JSON.stringify({readyIdlePhase:{observation:index+1,atMs,privateBytes:sample.privateBytes,maximumIncrementBytes:receipt.maxima.incrementBytes}}))}
    }})
    receipt.idleMeasurementsFinishedAtMs=performance.now()-start;assert.equal(receipt.actualObservations,94)
    receipt.diskVerificationPhase='outside all idle measurements';receipt.disk=await archiveDiskReceipt(h.page);body.current();assert.equal(receipt.disk.records,fixture.rawRecords);assert.equal(receipt.disk.pieces,fixture.rawRecords);assert.equal(receipt.disk.files['raw.jsonl'],fixture.rawBytes);assert.equal(receipt.disk.files['compressed.gz'],fixture.compressedBytes)
    receipt.fixture={rawBytes:fixture.rawBytes,rawRecords:fixture.rawRecords,rawSha256:fixture.rawSha256,compressedBytes:fixture.compressedBytes,compressedSha256:fixture.compressedSha256,recipe:fixture.recipe};receipt.http=http.snapshot();assert.deepEqual(receipt.http.failures,[]);assert.deepEqual(h.errors,[]);receipt.status='completed'
  }catch(error){isolate ||= error.isolateOwner;primary=error;receipt.status='failed';receipt.failure={message:error.message,diagnostic:error.diagnostic}}
  finally{
    const cleanup={h,http,fixture,profile,profileLaunch:launch,removeProfile:profile?async()=>{if(!launch?.closed)throw Error('profile_closure_unconfirmed');assert.equal(await realpath(profile),profile);assert.equal((await lstat(profile)).isSymbolicLink(),false);assert.ok(profile.startsWith(await realpath(scratch)+sep));await rm(profile,{recursive:true,maxRetries:0});receipt.profileCleanup='confirmed'}:null}
    try{await closeIsolateAndFixture(isolate,cleanup,primary);body.owner=null;receipt.cleanup={closed:cleanup.closed,stages:cleanup.completedCleanup}}catch(error){primary=error;receipt.status='failed';receipt.cleanupFailure={message:error.message,causes:error.errors?.map(value=>value.message).slice(0,3),stage:cleanup.cleanupStage}}
    clearTimeout(timer);receipt.bodyElapsedMs=performance.now()-began;if(body.retirement)receipt.deadlineRetirement=body.retirementResult || {retained:true}
  }
  for(const[input,value]of Object.entries(receipt.inputHashes))assert.equal(await hashFile(input),value,'idle diagnostic inputs unchanged')
  assert.equal(await hashFile(image.path),image.sha256);assert.equal(hash(await readFile(path)),before,'evidence CAS before diagnostic append')
  assert.ok(Buffer.byteLength(JSON.stringify(receipt,null,2))<=131072,'bounded idle diagnostic receipt');evidence.diagnostics.push(receipt)
  const encoded=JSON.stringify(evidence,null,2)+'\n';assert.ok(Buffer.byteLength(encoded)<=1048576);assert.ok(Buffer.byteLength(encoded)+evidence.historyRef.bytes<=2097152)
  const temporary=resolve(scratch,'qualification-'+randomUUID()+'.json');await writeFile(temporary,encoded,{flag:'wx'});await rename(temporary,path)
  console.log(JSON.stringify({readyIdle:{status:receipt.status,observations:receipt.actualObservations,maximum:receipt.maxima,cleanup:receipt.cleanup,profileCleanup:receipt.profileCleanup,evidenceSha256:hash(encoded)}}))
  if(primary)throw primary
})

function auxiliaryQualificationPurpose(sizes) {
  if(sizes.length===1&&sizes[0]===1024)return 'strict-qualification-1024MiB-with-auxiliary-isolate-attribution'
  if(sizes.length===2&&sizes[0]===1&&sizes[1]===100)return 'strict-qualification-1-100MiB-with-auxiliary-isolate-attribution'
  if(sizes.length===3&&sizes[0]===1&&sizes[1]===100&&sizes[2]===1024)return 'strict-qualification-1-100-1024MiB-with-auxiliary-isolate-attribution'
  assert.fail('auxiliary heap qualification requires exact ordered1024MiB,1,100MiB or1,100,1024MiB')
}

test('auxiliary strict group admission accepts only the exact ordered qualification shapes',()=>{
  assert.equal(auxiliaryQualificationPurpose([1024]),'strict-qualification-1024MiB-with-auxiliary-isolate-attribution')
  assert.equal(auxiliaryQualificationPurpose([1,100]),'strict-qualification-1-100MiB-with-auxiliary-isolate-attribution')
  assert.equal(auxiliaryQualificationPurpose([1,100,1024]),'strict-qualification-1-100-1024MiB-with-auxiliary-isolate-attribution')
  for(const sizes of [[],[1],[100],[256],[100,1],[1,1],[100,100],[1024,1],[1024,100,1],[1,1024,100],[1,100,100],[1,100,1024,1024],['1',100],[1,'100']])assert.throws(()=>auxiliaryQualificationPurpose(sizes),{code:'ERR_ASSERTION'})
})

test('scale qualification uses actual complete browser archives',{skip:process.env.HMEM_ARCHIVE_SCALE!=='1',timeout:600000},async()=>{
  const inputHashes={},inputPaths={}
  async function include(path,label=path){inputHashes[label]=hash(await readFile(path));inputPaths[label]=path;assert.ok(Object.keys(inputHashes).length<=512)}
  async function sources(directory){for(const entry of await readdir(directory,{withFileTypes:true})){const path=directory+'/'+entry.name;if(entry.isDirectory())await sources(path);else if(entry.isFile()&&/\.(elm|js|css)$/.test(path))await include(path)}}
  await sources('src')
  for(const path of ['package-lock.json','elm.json','vite.config.js','tests-js/session-archive.browser.test.js','tests-js/hierarchy-fixture.mjs','tests-js/support/sessions-ux-fixture.mjs','tests-js/support/session-archive-memory.mjs','tests-js/support/observation-discovery-adapter.mjs','perf/fixtures.mjs','public/hmem-runtime-config.js'])await include(path)
  const assets={};for(const name of await readdir('../static/assets')){const path='../static/assets/'+name;assets[name]=hash(await readFile(path));inputPaths['asset:'+name]=path;inputHashes['asset:'+name]=assets[name]}
  inputHashes['node-image']=await hashFile(process.execPath);inputPaths['node-image']=process.execPath
  const environment={node:process.version,nodeImage:{path:process.execPath,sha256:inputHashes['node-image']},zlib:process.versions.zlib,platform:os.platform(),release:os.release(),arch:os.arch(),cpu:os.cpus()[0]?.model,logicalCpus:os.cpus().length,totalMemory:os.totalmem()}
  const sizes=process.env.HMEM_ARCHIVE_SCALE_SIZES?process.env.HMEM_ARCHIVE_SCALE_SIZES.split(',').map(Number):[1,100,1024]
  const isolateDiagnostic=process.env.HMEM_ARCHIVE_MEMORY_CAUSAL==='1'
  const navigationDiagnostic=process.env.HMEM_ARCHIVE_NAV_CAUSAL==='1';assert.ok(!navigationDiagnostic||isolateDiagnostic,'navigation attribution requires the existing256MiB isolate diagnostic')
  const auxiliaryHeap=process.env.HMEM_ARCHIVE_HEAP_AUXILIARY==='1',trackIsolates=isolateDiagnostic||auxiliaryHeap
  assert.ok(!(isolateDiagnostic&&auxiliaryHeap),'distinct diagnostic and strict qualification purposes')
  const auxiliaryPurpose=auxiliaryHeap?auxiliaryQualificationPurpose(sizes):null
  assert.ok(isolateDiagnostic?sizes.length===1&&sizes[0]===256:sizes.length>0&&sizes.length<=3&&sizes.every(value=>[1,100,1024].includes(value)))
  const persistent=process.env.HMEM_ARCHIVE_SCALE_PROFILE==='persistent'
  const memoryDumpMode=process.env.HMEM_ARCHIVE_DUMP_MODE || 'detailed'
  assert.ok(['detailed','background'].includes(memoryDumpMode),'qualification dump mode must be detailed or background')
  assert.ok(!process.env.HMEM_ARCHIVE_SCALE_PROFILE || persistent)
  let persistentImage
  if(persistent){const evidence=await readQualificationEvidence();persistentImage=evidence.browserImage || evidence.attempts[0].browserImage;assert.equal(await hashFile(persistentImage.path),persistentImage.sha256)}
  const attempt={at:new Date().toISOString(),status:'running',contextMode:persistent?'task-owned persistent normal profile':'nonpersistent (incognito)',memoryMethod:'separate native Tracing.start/end for each sample; active-view baseline before transfer, without archive Worker or staged object',memoryDumpMode,memoryAttributionBasis:'explicit-global-request-span',suppliedBase:'b7428aa960d0b526fb85e33146f36374f3b8b109',invocationRevision:process.env.HMEM_ARCHIVE_INPUT_REVISION || null,inputHashes,assets,environment,sizes,results:[]}
  let primary,bodyTimer
  const body={owner:null,abort:new AbortController(),current(){this.abort.signal.throwIfAborted()}}
  if(trackIsolates){
    assert.ok(persistent&&memoryDumpMode==='background','isolate attribution requires normal persistent background mode')
    inputHashes['browser-image']=persistentImage.sha256;inputPaths['browser-image']=persistentImage.path;attempt.browserImage=persistentImage
    if(isolateDiagnostic){
      attempt.purpose=navigationDiagnostic?'diagnostic-navigation-allocation-256MiB':'diagnostic-attribution-256MiB';attempt.diagnosticLimits={bodyMs:60000,commandMs:2000,dumpMs:10000,lastSamples:8}
      attempt.hypotheses=['Native gzip input quantum may enqueue multiple decoded output chunks/backing stores.','Per-record promises and normalized block stringify/reparse may drive allocation highwater.'];attempt.hypothesesAreUnproved=true
      if(navigationDiagnostic)attempt.navigationCausal={order:[...NAVIGATION_BLOCKS],operationsPerBlock:5,limitations:'Diagnostic-only fixed-order blocks share allocator/cache highwater. Trends distinguish paths, not retention, collection, sole cause or an acceptance pass.',snapshots:[]}
    }else{
      attempt.purpose=auxiliaryPurpose
      attempt.auxiliaryHeapAttribution={enabled:true,commandMs:2000,dumpMs:10000,lastSamples:8,overallOwnedUnwindMs:180000,forcedGc:false,scope:'Additional distinct page/Worker isolate fields; native process-private acceptance metric and original budgets unchanged.'}
    }
    body.began=performance.now()
    bodyTimer=setTimeout(()=>{body.abort.abort(Error(isolateDiagnostic?'isolate_diagnostic_body_deadline':'archive_qualification_body_deadline'));if(body.owner)body.retirement=body.owner.close().then(()=>{body.retirementResult={closed:true}},error=>{body.retirementResult={error}})},isolateDiagnostic?60000:180000)
  }
  try {
    if(isolateDiagnostic)attempt.isolateCapability=await tinyIsolateCapability(persistentImage,body)
    for(const mebibytes of sizes) {
    assert.ok(!globalThis.retainedArchiveQualificationOwner || globalThis.retainedArchiveQualificationOwner.closed,'A prior qualification cleanup still owns its resources')
    let fixture,http,h,profile,profileLaunch,caseFailure,isolate
    let viewBegan=0
    const samples=[]
    const observations={mebibytes,sampleCount:0,maximumPrivateBytes:null,maximumIncrementBytes:null,maximumSampleGapMs:null,lastPhase:'setup'}
    attempt.currentCase=observations
    try {
      body.current();fixture=await scaleFixture(mebibytes*1048576,{signal:trackIsolates?body.abort.signal:undefined});body.current();http=await fixtureHttp(fixture);body.current()
      if(persistent){const scratch=resolve('../../.tmp/sealed-archives-e0ac6260');profile=resolve(scratch,'archive-profile-'+randomUUID());assert.ok(profile.startsWith(scratch+sep));await mkdir(profile);attempt.retainedProfile=profile;attempt.profileCleanup='pending confirmed browser closure'}
      if(persistent){body.current();profileLaunch=ownedPersistentFixture({path:profile,executable:persistentImage.path});if(trackIsolates)body.owner=profileLaunch;h=await profileLaunch.get()}else h=await openSessionExplorer()
      body.current()
      http.setOrigin(h.origin)
      Object.assign(h.saved,{committed_bytes:fixture.rawBytes,committed_records:fixture.rawRecords,final_bytes:fixture.rawBytes,final_sha256:fixture.rawSha256})
      const manifest={...makeManifest(h,Buffer.alloc(0),Buffer.alloc(0)),raw_bytes:String(fixture.rawBytes),raw_records:String(fixture.rawRecords),raw_sha256:fixture.rawSha256,compressed_bytes:String(fixture.compressedBytes),compressed_sha256:fixture.compressedSha256}
      await h.page.addInitScript(captureArchiveTelemetry,navigationDiagnostic)
      await h.page.route('**/sessions/**/archive**',phaseArchiveRoute(route=>new URL(route.request().url()).pathname.endsWith('/content')?route.continue({url:http.url}):route.fulfill({json:manifest})))
      await h.start();await h.idle();await h.page.evaluate(id=>{location.hash='tab=sessions&session='+id},h.active.id)
      await h.page.locator('#session-records .session-record').first().waitFor();await h.idle()
      const baseline=await boundedMemorySnapshot(h,{levelOfDetail:memoryDumpMode}),deadlineMs=2*fixture.compressedBytes/12500000*1000+120000*Math.max(1,Math.ceil(fixture.rawBytes/1073741824))+5000
      observations.baseline={privateBytes:baseline.totalPrivateBytes,processes:baseline.processes.map(value=>({pid:value.pid,type:value.type,bytes:parseInt(value.totals.private_footprint_bytes,16)})),attribution:baseline.attribution,traceLoss:baseline.traceLoss,trace:baseline.trace}
      if(trackIsolates){isolate=await openIsolateDiagnostic(h);observations.baseline.isolates=await isolate.snapshot();observations.isolateMaxima={};if(navigationDiagnostic)observations.baseline.dom=await isolate.domCounters();body.current()}
      assert.equal(baseline.workerTargets.length,0,'Pre-transfer baseline cannot contain an archive Worker')
      assert.equal(await h.page.evaluate(async()=>{try{await(await navigator.storage.getDirectory()).getDirectoryHandle('hmem-session-archives-v1');return true}catch(error){if(error.name==='NotFoundError')return false;throw error}}),false,'Pre-transfer baseline cannot contain a staged archive generation')
      assert.ok(baseline.processes.some(value=>value.pid===h.browserImage.pid&&value.type==='browser'),'actual BrowserServer process must match the sampled browser')
      if(!inputHashes['browser-image']){inputHashes['browser-image']=await hashFile(h.browserImage.executable);inputPaths['browser-image']=h.browserImage.executable;attempt.browserImage={path:h.browserImage.executable,sha256:inputHashes['browser-image'],version:baseline.browserVersion}}
      const start=performance.now();viewBegan=start
      let lastReported=0
      const recordMemory=(memory,phase,atMs,dumpMs,isolates)=>{
        const gap=atMs-(samples.at(-1)?.atMs || 0),increment=memory.totalPrivateBytes-baseline.totalPrivateBytes
        samples.push({atMs,dumpMs,phase,totalPrivateBytes:memory.totalPrivateBytes,processes:memory.processes.map(value=>({pid:value.pid,type:value.type,bytes:parseInt(value.totals.private_footprint_bytes,16)})),workers:memory.workerTargets,attribution:memory.attribution,traceLoss:memory.traceLoss,trace:{levelOfDetail:memory.trace.levelOfDetail,characters:memory.trace.characters,utf8Bytes:memory.trace.utf8Bytes},...(isolates?{isolates}:{})})
        if(trackIsolates){
          if(samples.length>8)samples.shift()
          for(const [kind,value]of Object.entries(isolates || {})){if(!value||!['page','worker'].includes(kind))continue;const key=kind==='worker'?'worker:'+value.phase+':'+value.incarnation:kind;const maxima=observations.isolateMaxima[key] ||= {isolateId:value.isolateId};assert.equal(maxima.isolateId,value.isolateId);for(const field of ['usedSize','totalSize','embedderHeapUsedSize','backingStorageSize'])if(maxima[field]===undefined||value[field]>maxima[field]){maxima[field]=value[field];maxima[field+'AtMs']=atMs}}
        }
        observations.sampleCount++;observations.lastPhase=phase;observations.lastSampleAtMs=atMs;observations.lastTrace=memory.trace
        if(observations.maximumPrivateBytes===null || memory.totalPrivateBytes>observations.maximumPrivateBytes){observations.maximumPrivateBytes=memory.totalPrivateBytes;observations.maximumIncrementBytes=increment;observations.maximumPhase=phase;observations.maximumAtMs=atMs;if(trackIsolates)observations.maximumSample=samples.at(-1)}
        observations.maximumSampleGapMs=Math.max(observations.maximumSampleGapMs || 0,gap)
        if(atMs-lastReported>=15000){lastReported=atMs;console.log(JSON.stringify({archivePhase:{mebibytes,atMs,phase,maximumIncrementBytes:observations.maximumIncrementBytes,maximumSampleGapMs:observations.maximumSampleGapMs}}))}
      }
      let done=false,failure
      const initialAck=await selectArchiveAcknowledgement(h.page,h.fixture.workspace.id,h.saved.id)
      observations.initialAck=initialAck
      assert.ok(initialAck.ms<=250,'initial current sealed-view DOM acknowledgement')
      const complete=h.page.getByText('Verified complete archive.',{exact:true}).waitFor({timeout:deadlineMs}).then(()=>{done=true},error=>{done=true;failure=error})
      while(!done){body.current();await Promise.race([complete,new Promise(resolve=>setTimeout(resolve,1000))]);if(!done){const began=performance.now(),memory=await boundedMemorySnapshot(h,{levelOfDetail:memoryDumpMode}),phase=await h.page.locator('.session-sealed-viewer [role="status"]').innerText(),isolates=isolate?await isolate.snapshot({requireWorker:true}):undefined;body.current();recordMemory(memory,phase,performance.now()-start,performance.now()-began,isolates)}}
      await complete;if(failure)throw failure
      const fullViewMs=performance.now()-start,loaded=await boundedMemorySnapshot(h,{levelOfDetail:memoryDumpMode})
      recordMemory(loaded,'ready',performance.now()-start,undefined,isolate?await isolate.snapshot({requireWorker:true}):undefined);body.current()
      const viewPeak={incrementBytes:observations.maximumIncrementBytes,privateBytes:observations.maximumPrivateBytes,phase:observations.maximumPhase,atMs:observations.maximumAtMs,...(trackIsolates?{sample:observations.maximumSample}:{})};observations.viewPeak=viewPeak
      const navigation=[]
      const {measurement:postNavigation,verification:disk}=await measuredNavigationThenVerification({current:()=>body.current(),navigate:async()=>{
      if(navigationDiagnostic){
        attempt.navigationCausal.readyDOM=await isolate.domCounters()
        for(const [block,kind]of attempt.navigationCausal.order.entries()){
          body.current()
          if(kind==='worker-only'){
            const summary=await h.page.evaluate(async()=>{
              const viewport=document.getElementById('session-records'),before=viewport.outerHTML,nav=window.archiveNavigationDiagnostic
              let maximumRows=0
              for(let index=0;index<5;index++){const result=await nav.rpc(index%2===0);if(result.rows>200)throw Error('navigation_rpc_row_limit');maximumRows=Math.max(maximumRows,result.rows)}
              if(viewport.outerHTML!==before)throw Error('worker_only_changed_transcript_DOM')
              if(nav.pending||nav.poisoned)throw Error('navigation_rpc_owner_unsettled')
              return {unchangedTranscriptDOM:true,maximumRows,completed:nav.completed,maximumPending:nav.maximumPending}
            })
            attempt.navigationCausal.workerSummary=summary
          }else await navigationDOMBlock(h.page,navigation)
          const began=performance.now(),memory=await boundedMemorySnapshot(h,{levelOfDetail:memoryDumpMode}),isolates=await isolate.snapshot({requireWorker:true}),dom=await isolate.domCounters(),helpers=await h.page.evaluate(()=>window.archiveNavigationHelpers)
          body.current();assert.equal(helpers.observers,0);assert.equal(helpers.timers,0)
          recordMemory(memory,'navigation-block-'+(block+1)+'-'+kind,performance.now()-start,performance.now()-began,isolates)
          attempt.navigationCausal.snapshots.push({block:block+1,kind,atMs:observations.lastSampleAtMs,absolutePrivateBytes:memory.totalPrivateBytes,originalBaselineIncrementBytes:memory.totalPrivateBytes-baseline.totalPrivateBytes,processes:samples.at(-1).processes,attribution:memory.attribution,traceLoss:memory.traceLoss,trace:samples.at(-1).trace,isolates,dom,helpers})
          assert.ok(attempt.navigationCausal.snapshots.length<=4)
        }
      }else{
        for(let index=0;index<20;index++)navigation.push(await navigationSample(h.page,index%2?'first':'last'))
        await navigationSample(h.page,'last')
      }
      const last=await h.page.locator('#session-records [data-offset]').last().getAttribute('data-offset')
      if(navigationDiagnostic){
        const first=await h.page.locator('#session-records [data-offset]').first().getAttribute('data-offset'),rows=await h.page.locator('#session-records [data-offset]').count()
        assert.equal(first,'0');assert.equal(last,String(BigInt(rows-1)*1024n));assert.equal(navigation.length,10)
        const completed=await h.page.evaluate(()=>window.archiveNavigationDiagnostic.completed);assert.equal(completed,10)
        attempt.navigationCausal.finalPosition={page:'first',firstOffset:first,lastOffset:last,rows,workerOperations:completed,domOperations:navigation.length,totalOperations:completed+navigation.length}
      }else assert.equal(last,String(fixture.rawBytes-1024))
      },measure:async()=>{
      body.current();const postNavigationBegan=performance.now(),postNavigation=await boundedMemorySnapshot(h,{levelOfDetail:memoryDumpMode}),postNavigationIsolates=isolate?await isolate.snapshot({requireWorker:true}):undefined
      recordMemory(postNavigation,'post-navigation',performance.now()-start,performance.now()-postNavigationBegan,postNavigationIsolates);body.current()
        observations.productMeasurementFinishedAtMs=performance.now()-start
        return postNavigation
      },verify:async()=>{
        const verification=observations.diskVerification={order:'after final product native and auxiliary sample',status:'running',startedAtMs:performance.now()-start}
        try{
          const disk=await archiveDiskReceipt(h.page)
          assert.equal(disk.files['raw.jsonl'],fixture.rawBytes);assert.equal(disk.files['compressed.gz'],fixture.compressedBytes);assert.equal(disk.records,fixture.rawRecords);assert.equal(disk.pieces,fixture.rawRecords)
          verification.finishedAtMs=performance.now()-start;verification.status='verified';return disk
        }catch(error){verification.finishedAtMs=performance.now()-start;verification.status='failed';throw error}
      }})
      const telemetry=await h.page.evaluate(()=>window.archiveTelemetry),domBytes=await h.page.locator('#session-records').evaluate(node=>new TextEncoder().encode(node.outerHTML).length)
      assert.ok(telemetry.maxRows<=200);assert.ok(telemetry.maxPageBytes+domBytes<=4194304);assert.ok(telemetry.maxBodyBytes<=65536);assert.ok(telemetry.maxChunk<=65536)
      const sorted=[...navigation].sort((a,b)=>a-b),p95=sorted[Math.ceil(sorted.length*.95)-1],peakPrivateIncrement=trackIsolates?observations.maximumIncrementBytes:Math.max(...samples.map(value=>value.totalPrivateBytes-baseline.totalPrivateBytes)),gaps=samples.map((value,index)=>value.atMs-(samples[index-1]?.atMs || 0))
      const result={rawBytes:fixture.rawBytes,rawRecords:fixture.rawRecords,rawSha256:fixture.rawSha256,compressedBytes:fixture.compressedBytes,compressedSha256:fixture.compressedSha256,recipe:fixture.recipe,initialAck,fullViewMs,deadlineMs,p95NavigationMs:p95,navigationMs:navigation,baselinePrivateBytes:baseline.totalPrivateBytes,baselineProcesses:baseline.processes.map(value=>({pid:value.pid,type:value.type,bytes:parseInt(value.totals.private_footprint_bytes,16)})),sampledPeakPrivateIncrement:peakPrivateIncrement,memoryScope:'selection through verified view and product navigation, including final native and auxiliary sample; diagnostic disk verification follows measurement',measurementOrder:'Ready -> product navigation -> final native+auxiliary -> unchanged disk verification',productMeasurementFinishedAtMs:observations.productMeasurementFinishedAtMs,diskVerification:observations.diskVerification,viewPeak,postNavigationIncrementBytes:postNavigation.totalPrivateBytes-baseline.totalPrivateBytes,maxSampleGapMs:trackIsolates?observations.maximumSampleGapMs:Math.max(...gaps),memoryMetric:'all-owned-browser-process private_footprint_bytes; sampled; not RSS/continuous peak',samples,disk,telemetry,domBytes,http:http.snapshot(),browser:await h.page.evaluate(()=>navigator.userAgent)}
      attempt.results.push(result)
      console.log(JSON.stringify({scale:{rawBytes:result.rawBytes,rawSha256:result.rawSha256,compressedBytes:result.compressedBytes,compressedSha256:result.compressedSha256,initialAck,fullViewMs,deadlineMs,p95NavigationMs:p95,sampledPeakPrivateIncrement:peakPrivateIncrement,maxSampleGapMs:result.maxSampleGapMs,maxNativeOutput:telemetry.maxNativeOutput,domBytes}}))
      assert.ok(fullViewMs<=deadlineMs,'declared full-view deadline');assert.ok(p95<=100,'local navigation p95');assert.ok(peakPrivateIncrement<=134217728,'sampled native private-footprint increment');assert.ok(!samples.some(value=>value.traceLoss))
      assert.deepEqual(http.snapshot().failures,[]);assert.ok(!h.reads.some(row=>row.path.endsWith('/evidence')))
    }catch(error){isolate ||= error.isolateOwner;caseFailure=error;attempt.partial={mebibytes,phase:viewBegan?'frontend-view':'setup',failure:error.message,diagnostic:error.diagnostic,elapsedViewMs:viewBegan?performance.now()-viewBegan:null,lastSamples:samples.slice(-8),fixture:fixture?{rawBytes:fixture.rawBytes,rawRecords:fixture.rawRecords,rawSha256:fixture.rawSha256,compressedBytes:fixture.compressedBytes,compressedSha256:fixture.compressedSha256,recipe:fixture.recipe}:null,http:http?.snapshot()};throw error}
    finally{
      const cleanupOwner={h,http,fixture,profile,profileLaunch,removeProfile:profile?async()=>{if(trackIsolates?profileLaunch&&!profileLaunch.closed:!profileLaunch?.closed)throw Error('profile_closure_unconfirmed');assert.equal(await realpath(profile),profile);assert.equal((await lstat(profile)).isSymbolicLink(),false);assert.ok(profile.startsWith(await realpath(resolve('../../.tmp/sealed-archives-e0ac6260'))+sep));await rm(profile,{recursive:true,maxRetries:0});attempt.profileCleanup='confirmed';delete attempt.retainedProfile}:null}
      try {
        await closeIsolateAndFixture(isolate,cleanupOwner,caseFailure);if(trackIsolates)body.owner=null
      }catch(error){attempt.cleanupFailure={message:error.message,causes:error.errors?.map(value=>value.message).slice(0,3),stage:cleanupOwner.cleanupStage,profile,fixture:fixture?.file};throw error}
    }
  }body.current();attempt.status='passed'}catch(error){primary=error;attempt.status='failed';if(error.capabilityReceipt)attempt.isolateCapability=error.capabilityReceipt;attempt.failure={message:error.message,bodyDeadline:body.abort.signal.aborted?body.abort.signal.reason.message:undefined,stack:String(error.stack).slice(0,2000)}}
  finally{clearTimeout(bodyTimer);if(trackIsolates)attempt[isolateDiagnostic?'diagnosticBodyElapsedMs':'overallBodyElapsedMs']=performance.now()-body.began;if(body.retirement)attempt[isolateDiagnostic?'diagnosticDeadlineRetirement':'overallOwnedRetirement']=body.retirementResult?.error?{failure:body.retirementResult.error.message}:body.owner&&!body.owner.closed?{retained:'actual fixture retirement is still owned by cleanup'}:{closed:true}}
  const changed=[];for(const[label,path]of Object.entries(inputPaths))if((label.endsWith('-image')?await hashFile(path):hash(await readFile(path)))!==inputHashes[label])changed.push(label)
  attempt.changedInputs=changed;if(changed.length){attempt.status='failed';primary ||= Error('qualification_input_drift')}
  const path=resolve('perf/session-archive.qualification.json'),scratch=resolve('../../.tmp/sealed-archives-e0ac6260')
  let evidence={schemaVersion:1,taskId:'e0ac6260-641c-4891-b4c1-cc3694f31158',scope:'Frontend-only controlled loopback HTTP/file preflight; no PostgreSQL/backend qualification. Native private-footprint phase samples are not RSS or a continuous peak.',owner:'Task6 viewer developer until verified handoff; then Task8 owner/repository maintainers',retention:'Required compact benchmark evidence until a reviewed Task8 aggregate supersedes or reconciles it.',attempts:[]}
  try{const previous=await readQualificationEvidence(path);assert.equal(previous.taskId,evidence.taskId);evidence=previous}catch(error){if(error.code!=='ENOENT')throw error}
  if(evidence.historicalVectors){assert.ok(Array.isArray(evidence.historicalVectors.attempts)&&evidence.historicalVectors.attempts.length<=6,'at most six immutable historical vectors');assert.deepEqual(evidence.historicalVectors.attempts.map(value=>hash(Buffer.from(JSON.stringify(value)))),evidence.historicalVectors.attemptObjectSha256,'historical vectors must remain unchanged')}
  assert.ok(evidence.attempts.length<8,'retain at most eight bounded qualification attempts; require explicit reconciliation beyond that')
  evidence.attempts.push(attempt)
  const encoded=JSON.stringify(evidence,null,2)+'\n';assert.ok(Buffer.byteLength(encoded)<=1048576,'compact required benchmark evidence')
  await mkdir(scratch,{recursive:true});const temporary=resolve(scratch,'qualification-'+randomUUID()+'.json');assert.ok(temporary.startsWith(scratch+sep))
  await writeFile(temporary,encoded,{flag:'wx'});await rename(temporary,path)
  console.log('qualification evidence '+path+' sha256 '+hash(Buffer.from(encoded)))
  if(primary)throw primary
})

test('actual Chromium Worker verifies private OPFS/IDB and rejects empty second member, garbage and truncation',{timeout:60000},async()=>{
  const h=await openSessionExplorer()
  try {
    await h.start();await h.idle()
    let seed=0x51a7
    const entropy=Array.from({length:32768},()=>{seed^=seed<<13;seed^=seed>>>17;seed^=seed<<5;return String.fromCharCode(33+(seed>>>0)%90)}).join('')
    const call=(name,timestamp)=>JSON.stringify({type:'response_item',timestamp,payload:{type:'function_call',name,arguments:'{}'}})
    const endTime='2026-01-01T00:00:01Z'
    const records=[JSON.stringify({type:'metadata',entropy})+'\n','{"text":"λ😀","role":"\\ud800"}\r\n',call('first','2026-01-01T00:00:00Z')+'\n',call('second',endTime)+'\n','{"text":"final"}']
    const asset=await workerAsset(),raw=Buffer.from(records.join('')),gzip=gzipSync(raw)
    assert.ok(gzip.length>4096&&gzip.length<65536,'native integrity fixture must cross compressed writer quanta within one download chunk')
    const cases=[['valid',gzip],['truncated',gzip.subarray(0,-2)],['empty-second',Buffer.concat([gzip,gzipSync(Buffer.alloc(0))])],['garbage',Buffer.concat([gzip,Buffer.from([1,2,3])])],['zero-padding',Buffer.concat([gzip,Buffer.from([0,0])])],['invalid-chunk-length',gzip],['invalid-chunk-capacity',gzip]]
    for(const [name,bytes] of cases) {
      const manifest=makeManifest(h,raw,bytes)
      const result=await h.page.evaluate(async({asset,manifest,bytes,name})=>{
        const worker=new Worker(asset,{type:'module'}),stamp={workspace:manifest.workspace_id,session:manifest.session_id,generation:manifest.generation_id,load:1,authEpoch:1}
        let id=0;const pending=new Map(),progress=[]
        worker.onmessage=({data})=>{if(data.kind==='progress'){progress.push(data.phase);return}const waiter=pending.get(data.request);if(waiter){pending.delete(data.request);data.kind==='error'?waiter.reject(Error(data.code)):waiter.resolve(data)}}
        const rpc=(kind,extra={})=>new Promise((resolve,reject)=>{const request=++id;const timeout=setTimeout(()=>reject(Error('browser_rpc_timeout')),15000);pending.set(request,{resolve:value=>{clearTimeout(timeout);resolve(value)},reject:error=>{clearTimeout(timeout);reject(error)}});worker.postMessage({kind,stamp,request,...extra},extra.bytes?[extra.bytes]:[])})
        let error=null,page=null,cleanup=null,schema=null;const ownerName=crypto.randomUUID()
        try{
          await rpc('start',{name:ownerName,manifest})
          const buffer=new Uint8Array(name==='invalid-chunk-capacity'?65535:65536).fill(231)
          buffer.set(bytes)
          const result=rpc('chunk',{bytes:buffer.buffer,length:name==='invalid-chunk-length'?65537:bytes.length})
          if(buffer.byteLength!==0)throw Error('browser_chunk_not_transferred')
          const receipt=await result
          if(receipt.kind!=='ack'||receipt.bytes?.byteLength!==65536||new Uint8Array(receipt.bytes).at(-1)!==231)throw Error('browser_chunk_ack_mismatch')
          await rpc('finish');page=await rpc('page',{page:{}})
          if(name==='valid'){
            const opening=indexedDB.open('hmem-session-archive-v1-'+ownerName),db=await new Promise((yes,no)=>{opening.onsuccess=()=>yes(opening.result);opening.onerror=()=>no(opening.error)})
            let terminal
            try{
              const tx=db.transaction(['records','pieces','groups'],'readonly')
              terminal=new Promise(resolve=>{tx.oncomplete=()=>resolve({});tx.onabort=()=>resolve({error:tx.error || Error('inspection_aborted')})})
              const count=table=>new Promise((yes,no)=>{const request=tx.objectStore(table).count();request.onsuccess=()=>yes(request.result);request.onerror=()=>no(request.error)})
              const inspectRecords=new Promise((yes,no)=>{const request=tx.objectStore('records').get('0000000000000000000');request.onsuccess=()=>yes(request.result?.spans);request.onerror=()=>no(request.error)})
              const inspect=(table,tag,read)=>new Promise((yes,no)=>{
                const request=tx.objectStore(table).openCursor()
                request.onsuccess=()=>{
                  try{
                    const cursor=request.result,data=cursor.value.data,view=new DataView(data.buffer,data.byteOffset,data.byteLength)
                    const count=view.getUint16(6,true),rows=[]
                    if(view.getUint32(0,true)!==0x58494d48||view.getUint8(4)!==1||view.getUint8(5)!==tag
                      || view.getUint32(8,true)!==data.byteLength||count<1||count>256)throw Error('invalid_typed_index')
                    let at=12,fence=''
                    for(let index=0;index<count;index++){
                      const key=view.getBigUint64(at,true).toString().padStart(19,'0')+':'+String(view.getUint32(at+8,true)).padStart(7,'0')
                      const length=view.getUint32(at+12,true)
                      if(key<=fence||length<(tag===1?41:28)||at+length>data.byteLength)throw Error('invalid_typed_row')
                      rows.push(read(view,at,key,length));fence=key;at+=length
                    }
                    if(at!==data.byteLength||fence!==cursor.key)throw Error('invalid_typed_fence')
                    yes({rows,bytes:data.byteLength})
                  }catch(error){no(error)}
                }
                request.onerror=()=>no(request.error)
              })
              const inspectPieces=inspect('pieces',1,(view,at)=>view.getBigUint64(at+16,true).toString())
              const inspectGroups=inspect('groups',2,(view,at,key,length)=>{
                const count=view.getBigUint64(at+16,true).toString(),units=view.getUint32(at+24,true)
                if(28+units*2!==length)throw Error('invalid_typed_group')
                let endTime=''
                for(let index=0;index<units;index++)endTime+=String.fromCharCode(view.getUint16(at+28+index*2,true))
                return {key,count,endTime}
              })
              const indexNames=Array.from(tx.objectStore('pieces').indexNames),[recordPages,spans,piecePages,pieces,groupPages,groups]=await Promise.all([count('records'),inspectRecords,count('pieces'),inspectPieces,count('groups'),inspectGroups]),done=await terminal
              if(done.error)throw done.error
              schema={indexNames,recordPages,spans,piecePages,pieceOffsets:pieces.rows,groupPages,groups:groups.rows,typedBytes:[pieces.bytes,groups.bytes],transactionComplete:true}
            }finally{if(terminal)await terminal;db.close();if(schema)schema.databaseClosed=true}
          }
        }catch(failure){error=failure.message}
        finally{try{await rpc('retire');cleanup=true}catch(failure){cleanup=failure.message}worker.terminate()}
        return {name,error,page,cleanup,progress,schema,browser:navigator.userAgent}
      },{asset,manifest,bytes:[...bytes],name})
      assert.equal(result.cleanup,true,JSON.stringify(result))
      if(name==='valid'){
        assert.equal(result.error,null);assert.equal(result.page.rows[0].body.text,'λ😀');assert.equal(result.page.rows[0].role,'\ud800');assert.equal(result.page.rows.at(-1).body.text,'final')
        let offset=0
        const offsets=records.map(record=>{const start=offset;offset+=Buffer.byteLength(record);return String(start)})
        const spans=records.map(record=>Buffer.byteLength(record)*4+(record.endsWith('\r\n')?2:record.endsWith('\n')?1:0))
        const [first,second]=result.page.rows.slice(1,3)
        for(const row of [first,second]){assert.equal(row.group,offsets[2]+':0');assert.equal(row.count,'2');assert.equal(row.endTime,endTime)}
        const {typedBytes,...schema}=result.schema
        assert.deepEqual(schema,{indexNames:[],recordPages:1,spans,piecePages:1,pieceOffsets:offsets.slice(1),groupPages:1,groups:[{key:first.key,count:'2',endTime}],transactionComplete:true,databaseClosed:true})
        assert.ok(typedBytes[0]>0&&typedBytes[0]<1024);assert.equal(typedBytes[1],12+28+endTime.length*2)
        const pieces=result.page.rows.map(({key,offset,end,delimiter,index,kind,label,role,time,group})=>({key,offset,end,delimiter,index,kind,label,role,time,group}))
        const legacyBytes=[pieces,schema.groups].map(rows=>Buffer.byteLength(JSON.stringify(rows)))
        assert.ok(typedBytes.every((bytes,index)=>bytes<legacyBytes[index]))
        console.log(JSON.stringify({nativeIndexBytes:{typedBytes,legacyBytes,singleBytes:0}}))
      }
      else {assert.ok(result.error,JSON.stringify(result));assert.ok(!result.progress.includes('ready'));if(name.startsWith('invalid-chunk-'))assert.equal(result.error,'chunk_limit')}
      console.log(JSON.stringify({case:name,error:result.error,cleanup:result.cleanup,schema:result.schema,browser:result.browser}))
    }
    assert.deepEqual(h.errors,[])
  }finally{await h.close()}
})


for(const delivery of ['fast','slow'])test('production local navigation status coalesces '+delivery+' page delivery',{timeout:30000},async()=>{
  const h=await openSessionExplorer()
  try{
    const raw=Buffer.from(Array.from({length:160},(_,index)=>JSON.stringify({text:'local navigation row '+index})+'\n').join('')),gzip=gzipSync(raw),manifest=makeManifest(h,raw,gzip)
    Object.assign(h.saved,{committed_bytes:raw.length,committed_records:160,final_bytes:raw.length,final_sha256:hash(raw)})
    await h.page.addInitScript(()=>{
      const Native=Worker;window.navigationPageReplies=0
      window.Worker=class extends Native{
        constructor(url,options){super(url,options);this.addEventListener('message',({data})=>{if(data.kind==='page'){window.navigationPageReplies++;window.navigationLastPageRows=data.rows.length;if(window.navigationTiming?.request===data.request)window.navigationTiming.replied=performance.now()}})}
        postMessage(message,transfer){if(message.kind==='page'&&window.holdNavigationPage){window.holdNavigationPage=false;window.navigationTiming={request:message.request,held:performance.now()};window.releaseNavigationPage=()=>{window.releaseNavigationPage=null;window.navigationTiming.released=performance.now();Native.prototype.postMessage.call(this,message,transfer)};return}return super.postMessage(message,transfer)}
      }
    })
    await h.page.route('**/sessions/**/archive**',phaseArchiveRoute(route=>new URL(route.request().url()).pathname.endsWith('/content')?route.fulfill({headers:{'Content-Type':'application/gzip','Content-Length':String(gzip.length),'X-HMem-Archive-Generation':generation,'X-HMem-Archive-SHA256':hash(gzip),'X-HMem-Archive-Raw-Bytes':String(raw.length),'X-HMem-Archive-Raw-SHA256':hash(raw)},body:gzip}):route.fulfill({json:manifest})))
    await h.start();await h.idle();await h.page.goto(h.origin+'/workspace/'+h.fixture.workspace.id+'#tab=sessions&session='+h.saved.id)
    await h.page.getByText('Verified complete archive.',{exact:true}).waitFor({timeout:10000})
    const previous=await h.page.evaluate(delivery=>{
      const status=document.querySelector('.session-sealed-viewer [role="status"]');window.navigationStatuses=[]
      window.navigationStatusObserver=new MutationObserver(()=>{const value=status.textContent;if(window.navigationStatuses.at(-1)!==value)window.navigationStatuses.push(value)})
      window.navigationStatusObserver.observe(status,{childList:true,subtree:true,characterData:true});window.holdNavigationPage=true
      return window.navigationPageReplies
    },delivery)
    for(let step=0;!await h.page.evaluate(()=>typeof window.releaseNavigationPage==='function');step++){assert.ok(step<8,'actual upward input reaches one indexed continuation');await continuousScrollStep(h.page,-1);await h.page.waitForTimeout(25)}
    if(delivery==='slow'){
      await h.page.waitForFunction(()=>typeof window.releaseNavigationPage==='function',null,{timeout:2000})
      await h.page.getByText('Reading local index…',{exact:true}).waitFor({timeout:2000})
      assert.equal(await h.page.evaluate(()=>window.navigationStatuses.filter(value=>value==='Reading local index…').length),1)
    }
    await h.page.evaluate(()=>window.releaseNavigationPage())
    await h.page.waitForFunction(previous=>window.navigationPageReplies===previous+1,previous,{timeout:2000})
    await h.page.getByText('Verified complete archive.',{exact:true}).waitFor({timeout:2000});await h.page.waitForTimeout(150)
    const state=await h.page.evaluate(()=>{window.navigationStatusObserver.disconnect();const timing=window.navigationTiming;return {statuses:window.navigationStatuses,status:document.querySelector('.session-sealed-viewer [role="status"]').textContent,rows:document.querySelectorAll('#session-records [data-offset]').length,acquiredRows:window.navigationLastPageRows,holdMs:timing.released-timing.held,replyMs:timing.replied-timing.released}})
    assert.equal(state.status,'Verified complete archive.');assert.ok(state.rows>0&&state.rows<=200);assert.equal(state.acquiredRows,64);assert.equal(state.statuses.filter(value=>value==='Reading local index…').length,delivery==='slow'?1:0,JSON.stringify(state))
    assert.deepEqual(h.errors,[]);console.log(JSON.stringify({localNavigationStatus:{delivery,...state}}))
  }finally{await h.close()}
})


test('tiny production handoff retains generation custody and reopens the compiled tagged reader',{timeout:60000},async()=>{
  const evidence=await readQualificationEvidence(),image=evidence.browserImage,scratch=profileScratch(),profile=resolve(scratch,'archive-profile-'+randomUUID())
  assert.equal(await hashFile(image.path),image.sha256);await mkdir(profile)
  let launch,h,isolate,primary,releaseConditional,releaseContent,deadlineFailure,deadlineReject,phase='launch'
  const nativeSamples=[]
  const deadline=new Promise((_,reject)=>{deadlineReject=reject});deadline.catch(()=>{})
  const timer=setTimeout(()=>{deadlineFailure=Error('tiny_phase_owned_deadline');deadlineReject(deadlineFailure);launch?.close().catch(()=>{})},30000)
  try{
    launch=ownedPersistentFixture({path:profile,executable:image.path});h=await Promise.race([launch.get(),deadline]);phase='fixture-ready'
    const raw=h.content.get(h.saved.id),gzip=gzipSync(raw),manifest=makeManifest(h,raw,gzip),etag='"tiny-current-authority"',conditional=new Promise(resolve=>{releaseConditional=resolve}),contentReady=new Promise(resolve=>{releaseContent=resolve})
    await h.page.addInitScript(captureArchiveTelemetry)
    await h.page.addInitScript(()=>{
      const Native=Worker;window.archivePhaseProof=[]
      window.Worker=class extends Native{
        constructor(url,options){const resolved=new URL(String(url),location.href),previous=window.archivePhaseProof.at(-1);if(previous&&!previous.terminated)throw Error('phase_admission_overlap');if(window.archivePhaseProof.length===2)throw Error('phase_count_limit');super(url,options);this.phase={url:resolved.href,mode:resolved.searchParams.get('phase'),incarnation:resolved.searchParams.get('incarnation'),terminated:false,detached:false,last:[]};window.archivePhaseProof.push(this.phase);this.addEventListener('message',({data})=>{this.phase.last.push({kind:data.kind,request:data.request,code:data.code,phase:data.phase});if(this.phase.last.length>8)this.phase.last.shift();if(data.kind==='detached')this.phase.detached=true;if(data.kind==='cleaned')this.phase.cleaned=true})}
        postMessage(message,transfer){if(['start','adopt'].includes(message.kind)){this.phase.name=message.name;this.phase.messageKind=message.kind;if(message.stamp.incarnation!==this.phase.incarnation)throw Error('phase_url_stamp_mismatch')}return super.postMessage(message,transfer)}
        terminate(){if(this.phase.mode==='processor'&&!this.phase.detached&&!this.phase.cleaned)throw Error('processor_termination_without_joined_handoff');this.phase.terminated=true;return super.terminate()}
      }
    })
    await h.page.route('**/sessions/**/archive**',async route=>{
      if(!new URL(route.request().url()).pathname.endsWith('/content'))return route.fulfill({json:manifest})
      const match=route.request().headers()['if-none-match']
      if(match){assert.equal(match,etag);await conditional;return route.fulfill({status:304,headers:{ETag:etag}})}
      await contentReady
      return route.fulfill({headers:{ETag:etag,'Content-Type':'application/gzip','Content-Length':String(gzip.length),'X-HMem-Archive-Generation':generation,'X-HMem-Archive-SHA256':hash(gzip),'X-HMem-Archive-Raw-Bytes':String(raw.length),'X-HMem-Archive-Raw-SHA256':hash(raw)},body:gzip})
    })
    phase='active-reader';await h.start();await h.idle();await h.page.evaluate(id=>{location.hash='tab=sessions&session='+id},h.active.id);await h.page.locator('#session-records .session-record').first().waitFor();await h.idle();phase='isolate-baseline'
    isolate=await openIsolateDiagnostic(h)
    const capture=async phase=>{const native=await boundedMemorySnapshot(h,{levelOfDetail:'background'}),aux=await isolate.snapshot({requireWorker:phase!=='baseline'});nativeSamples.push({phase,privateBytes:native.totalPrivateBytes,processes:native.processes.map(value=>({pid:value.pid,type:value.type,bytes:parseInt(value.totals.private_footprint_bytes,16)})),attribution:native.attribution,trace:native.trace,traceLoss:native.traceLoss,aux});return aux}
    await capture('baseline');phase='archive-downloading';await h.page.goto(h.origin+'/workspace/'+h.fixture.workspace.id+'#tab=sessions&session='+h.saved.id)
    await h.page.waitForFunction(()=>window.archivePhaseProof[0]?.last.some(message=>message.kind==='progress'&&message.phase==='downloading'),null,{timeout:10000})
    phase='processor-snapshot';const processorAux=await capture('processor');assert.equal(processorAux.worker.phase,'processor');releaseContent();phase='processor-detach'
    await h.page.waitForFunction(()=>window.archivePhaseProof.length===1&&window.archivePhaseProof[0].detached&&window.archivePhaseProof[0].terminated,null,{timeout:10000})
    const gap=await h.page.evaluate(async()=>{
      const phase=window.archivePhaseProof[0],lock=await navigator.locks.request('hmem-session-archive-v1-'+phase.name,{ifAvailable:true},value=>!!value)
      const root=await(await navigator.storage.getDirectory()).getDirectoryHandle('hmem-session-archives-v1');await root.getDirectoryHandle(phase.name)
      return {lockAvailable:lock,phases:window.archivePhaseProof}
    });assert.equal(gap.lockAvailable,false);assert.equal(gap.phases.length,1)
    const gapAux=await capture('detached-gap');assert.equal(gapAux.worker,null);assert.equal(gapAux.workerTransition.phase,'processor-detached-awaiting-reader')
    releaseConditional();phase='reader-adoption';await h.page.getByText('Verified complete archive.',{exact:true}).waitFor({timeout:10000})
    const readerAux=await capture('reader');assert.equal(readerAux.worker.phase,'reader');assert.notEqual(readerAux.worker.isolateId,processorAux.worker.isolateId);assert.equal(readerAux.observedWorkerEpochs.length,2)
    for(const sample of nativeSamples){assert.equal(sample.traceLoss,false);assert.deepEqual(sample.processes.map(value=>value.pid).sort((a,b)=>a-b),nativeSamples[0].processes.map(value=>value.pid).sort((a,b)=>a-b))}
    const phases=await h.page.evaluate(()=>window.archivePhaseProof);assert.deepEqual(phases.map(value=>value.mode),['processor','reader']);assert.deepEqual(phases.map(value=>value.messageKind),['start','adopt']);assert.notEqual(phases[0].incarnation,phases[1].incarnation);assert.equal(phases[0].name,phases[1].name)
    for(const phase of phases){const url=new URL(phase.url);assert.match(url.pathname,/\/assets\/session-archive-worker-[^/]+\.js$/);assert.match(phase.incarnation,/^[a-f0-9-]{36}$/)}
    const group=h.page.locator('.session-activity-group').first();await group.locator(':scope > summary').click();const tool=group.locator('.session-content-tool').first();await tool.locator('summary').click();await tool.locator('pre').waitFor();assert.ok((await tool.locator('pre').innerText()).includes('src/Feature/Sessions.elm'))
    await h.page.getByRole('link',{name:'Close session',exact:true}).click();await h.page.locator('#session-records').waitFor({state:'detached'});await h.page.waitForFunction(()=>window.archivePhaseProof[1]?.cleaned&&window.archivePhaseProof[1]?.terminated)
    await h.page.waitForFunction(async()=>{const phase=window.archivePhaseProof[0],root=await(await navigator.storage.getDirectory()).getDirectoryHandle('hmem-session-archives-v1');let count=0;for await(const entry of root.entries())count++;if(count)return false;return navigator.locks.request('hmem-session-archive-v1-'+phase.name,{ifAvailable:true},value=>!!value)},null,{timeout:10000})
    assert.deepEqual(h.errors,[]);console.log(JSON.stringify({tinyPhaseHandoff:{gapCustodyHeld:true,phases,nativeSamples,compiledImportsWorked:true,toolBodyExact:true,deletedAndLockReleased:true,publicTerminateReceipt:'invoked only; no physical-thread or RSS acknowledgement'}}))
  }catch(error){isolate ||= error.isolateOwner;primary=error;if(h)try{console.log(JSON.stringify({tinyPhaseFailure:await h.bounded(h.page.evaluate(()=>({status:(document.querySelector('.session-sealed-viewer [role="status"],.session-sealed-viewer [role="alert"]')?.textContent||'').slice(0,500),phases:window.archivePhaseProof?.slice(0,2)})),2000,'Tiny phase failure facts'),pageErrors:h.errors.slice(0,4)}))}catch(captureError){error.captureFailure=captureError.message}}
  finally{
    clearTimeout(timer);primary ||= deadlineFailure
    releaseContent?.();releaseConditional?.()
    try{await closeIsolateAndFixture(isolate,{h,profileLaunch:launch,profile,removeProfile:async()=>{if(!launch?.closed)throw Error('profile_closure_unconfirmed');assert.equal(await realpath(profile),profile);assert.equal((await lstat(profile)).isSymbolicLink(),false);assert.ok(profile.startsWith(await realpath(scratch)+sep));await rm(profile,{recursive:true,maxRetries:0})}},primary)}catch(error){primary=primary?new AggregateError([primary,error],'tiny_phase_cleanup_failed'):error}
  }
  if(primary){console.log(JSON.stringify({tinyFailurePhase:phase,errors:[primary,...(primary.errors||[])].slice(0,5).map(error=>error.message)}));throw primary}
})

test('production sealed view uses complete gzip only and preserves lazy tool disclosure',{timeout:60000},async()=>{
  const h=await openSessionExplorer()
  try {
    const raw=h.content.get(h.saved.id),gzip=gzipSync(raw),manifest=makeManifest(h,raw,gzip),reads=[]
    await h.page.route('**/sessions/**/archive**',phaseArchiveRoute(async route=>{
      reads.push(route.request().url())
      if(new URL(route.request().url()).pathname.endsWith('/content'))await route.fulfill({status:200,headers:{'Content-Type':'application/gzip','Content-Length':String(gzip.length),'X-HMem-Archive-Generation':generation,'X-HMem-Archive-SHA256':hash(gzip),'X-HMem-Archive-Raw-Bytes':String(raw.length),'X-HMem-Archive-Raw-SHA256':hash(raw)},body:gzip})
      else await route.fulfill({json:manifest})
    }))
    await h.start();await h.idle()
    const baseline=await boundedMemorySnapshot(h)
    assert.ok(baseline.processes.some(value=>value.pid===h.browserImage.pid&&value.type==='browser'))
    const browserImage={...h.browserImage,sha256:await hashFile(h.browserImage.executable)}
    await h.page.goto(h.origin+'/workspace/'+h.fixture.workspace.id+'#tab=sessions&session='+h.saved.id)
    await h.page.getByText('Verified complete archive.',{exact:true}).waitFor({timeout:20000})
    assert.equal(h.reads.filter(row=>row.path.endsWith('/evidence')).length,0)
    assert.equal(reads.length,3)
    const group=h.page.locator('.session-activity-group').first();await group.locator(':scope > summary').click()
    const tool=group.locator('.session-content-tool').first();assert.equal(await tool.locator('pre').count(),0)
    await tool.locator('summary').click();await tool.locator('pre').waitFor();assert.ok((await tool.locator('pre').innerText()).includes('src/Feature/Sessions.elm'))
    const size=await h.page.locator('#session-records').evaluate(node=>new TextEncoder().encode(node.outerHTML).length)
    assert.ok(size<4194304);assert.ok(await h.page.locator('.session-record').count()<=64)
    assert.equal(await h.page.getByRole('button',{name:/^(First|Previous|Next|Last)$/}).count(),0);assert.equal(await h.page.locator('#session-records').getAttribute('data-archive-can-next'),'false','the verified end has no later source interval')
    assert.equal(reads.length,3,'ordinary local navigation performs no HTTP')
    const loaded=await boundedMemorySnapshot(h)
    console.log(JSON.stringify({memoryCapability:{browserImage,baseline,loaded}}))
    assert.deepEqual(h.errors,[])
  }finally{await h.close()}
})

test('native archive cases include empty, tiny, seeded entropy, corruption, unsupported resources and actual quota-write failure',{timeout:60000},async()=>{
  const h=await openSessionExplorer()
  try {
    await h.start();await h.idle()
    let seed=0x51a7;const next=()=>{seed^=seed<<13;seed^=seed>>>17;seed^=seed<<5;return seed>>>0}
    const entropy=Array.from({length:80000},()=>String.fromCharCode(33+next()%90)).join('')
    for(const raw of [Buffer.alloc(0),Buffer.from('{"text":"tiny"}'),Buffer.from(JSON.stringify({text:entropy})+'\n')]) {
      const result=await nativeProbe(h,raw,gzipSync(raw));assert.equal(result.error,null);assert.equal(result.cleanup,true)
    }
    const raw=Buffer.from('{"text":"verified"}\n'),gzip=gzipSync(raw)
    const compressed=await nativeProbe(h,raw,gzip,{manifest:{...makeManifest(h,raw,gzip),compressed_sha256:'0'.repeat(64)}})
    assert.equal(compressed.error,'compressed_receipt_mismatch');assert.ok(!compressed.progress.includes('decoding'));assert.equal(compressed.cleanup,true)
    const original=await nativeProbe(h,raw,gzip,{manifest:{...makeManifest(h,raw,gzip),raw_sha256:'0'.repeat(64)}})
    assert.equal(original.error,'raw_receipt_mismatch');assert.ok(!original.progress.includes('ready'));assert.equal(original.cleanup,true)
    for(const prelude of ['self.DecompressionStream=undefined;','Object.defineProperty(self,"indexedDB",{value:undefined});','navigator.storage.getDirectory=undefined;']) {
      const result=await nativeProbe(h,raw,gzip,{prelude});assert.equal(result.error,'archive_unsupported');assert.equal(result.cleanup,true)
    }
    const quota=await nativeProbe(h,raw,gzip,{prelude:'const nativeWrite=FileSystemSyncAccessHandle.prototype.write;let writes=0;FileSystemSyncAccessHandle.prototype.write=function(...args){if(++writes===2)throw new DOMException("fixture quota","QuotaExceededError");return nativeWrite.apply(this,args)}; '})
    assert.equal(quota.error,'archive_quota');assert.ok(!quota.progress.includes('ready'));assert.equal(quota.cleanup,true)
    const advisory=Buffer.from(Array.from({length:1000},(_,index)=>JSON.stringify({text:index+'x'.repeat(2000)})).join('\n')+'\n')
    for(const prelude of ['navigator.storage.estimate=async()=>({quota:1,usage:2});','navigator.storage.estimate=async()=>{throw Error("estimate_unavailable")};']){
      const result=await nativeProbe(h,advisory,gzipSync(advisory),{prelude});assert.equal(result.error,null);assert.equal(result.cleanup,true)
    }
    assert.deepEqual(h.errors,[])
  }finally{await h.close()}
})

test('crashed private generations recover without deleting a second live tab owner',{timeout:60000},async()=>{
  const h=await openSessionExplorer();let other
  try {
    await h.start();await h.idle();const popup=h.page.waitForEvent('popup');await h.page.evaluate(url=>window.open(url,'second-archive-tab'),h.origin+'/');other=await popup;await other.waitForLoadState('domcontentloaded')
    const raw=Buffer.from('{"text":"owned evidence"}\n'),gzip=gzipSync(raw),manifest=makeManifest(h,raw,gzip),asset=await workerAsset()
    const launch=async(page,label)=>page.evaluate(async({label,asset,manifest,bytes})=>{
      const name=crypto.randomUUID(),worker=new Worker(asset,{type:'module'}),stamp={workspace:manifest.workspace_id,session:manifest.session_id,generation:manifest.generation_id,load:1,authEpoch:1},pending=new Map();let request=0
      worker.onmessage=({data})=>{if(data.kind==='progress')return;const waiter=pending.get(data.request);if(waiter){pending.delete(data.request);data.kind==='error'?waiter.reject(Error(data.code)):waiter.resolve(data)}}
      const rpc=(kind,extra={})=>new Promise((resolve,reject)=>{const id=++request,timer=setTimeout(()=>reject(Error('recovery_rpc_timeout')),10000);pending.set(id,{resolve:value=>{clearTimeout(timer);resolve(value)},reject:error=>{clearTimeout(timer);reject(error)}});worker.postMessage({kind,stamp,request:id,...extra},extra.bytes?[extra.bytes]:[])})
      const owned={worker,rpc,name};window[label]=owned
      await rpc('start',{name,manifest})
      const buffer=new Uint8Array(65536);buffer.set(bytes)
      const receipt=await rpc('chunk',{bytes:buffer.buffer,length:bytes.length})
      if(receipt.kind!=='ack'||receipt.bytes?.byteLength!==65536)throw Error('browser_chunk_ack_mismatch')
      await rpc('finish');return name
    },{label,asset,manifest,bytes:[...gzip]})
    const first=await launch(h.page,'crashed'),live=await launch(other,'live')
    await h.page.evaluate(()=>window.crashed.worker.terminate())
    await h.page.waitForFunction(async name=>!(await navigator.locks.query()).held.some(lock=>lock.name==='hmem-session-archive-v1-'+name),first,{timeout:5000})
    const third=await launch(h.page,'replacement')
    const state=await h.page.evaluate(async()=>{const root=await(await navigator.storage.getDirectory()).getDirectoryHandle('hmem-session-archives-v1');const names=[];for await(const [name] of root.entries())names.push(name);return names})
    assert.ok(!state.includes(first));assert.ok(state.includes(live));assert.ok(state.includes(third))
    const stillLive=await other.evaluate(async()=>window.live.rpc('page',{page:{}}));assert.equal(stillLive.rows[0].body.text,'owned evidence')
    await other.evaluate(async()=>{await window.live.rpc('retire');window.live.worker.terminate()})
    await h.page.evaluate(async()=>{await window.replacement.rpc('retire');window.replacement.worker.terminate()})
    const left=await h.page.evaluate(async()=>{const root=await(await navigator.storage.getDirectory()).getDirectoryHandle('hmem-session-archives-v1');let count=0;for await(const entry of root.entries())count++;return count})
    assert.equal(left,0)
  }finally{await other?.close();await h.close()}
})

test('known credential switch and authorization loss synchronously retire visible local evidence',{timeout:60000},async()=>{
  const h=await openSessionExplorer({extraRecords:Array.from({length:300},(_,i)=>({text:'Authorization history '+i+' keeps indexed scrolling available.'}))});let release
  try {
    await h.page.addInitScript(()=>{
      const Native=Worker
      window.holdArchivePage=false
      window.Worker=class extends Native{postMessage(message,transfer){
        if(window.holdArchivePage&&message.kind==='page'){
          window.holdArchivePage=false
          window.releaseArchivePage=discard=>{window.releaseArchivePage=null;if(!discard)super.postMessage(message,transfer)}
        }else super.postMessage(message,transfer)
      }}
    })
    await h.page.route('**/hmem-runtime-config.js',route=>route.fulfill({contentType:'application/javascript',body:"window.HMEM_CONFIG={authMode:'local',apiUrl:window.location.origin,wsUrl:'ws://fixture.invalid/api/v1/ws',authTokenStorage:'local'};"}))
    const raw=h.content.get(h.saved.id),gzip=gzipSync(raw),manifest=makeManifest(h,raw,gzip);let switched=false
    const gate=new Promise(resolve=>{release=resolve})
    await h.page.route('**/sessions/**/archive**',phaseArchiveRoute(async route=>{
      if(new URL(route.request().url()).pathname.endsWith('/content')){if(switched)await gate;await route.fulfill({headers:{'Content-Type':'application/gzip','Content-Length':String(gzip.length),'X-HMem-Archive-Generation':generation,'X-HMem-Archive-SHA256':hash(gzip),'X-HMem-Archive-Raw-Bytes':String(raw.length),'X-HMem-Archive-Raw-SHA256':hash(raw)},body:gzip})}
      else await route.fulfill({json:manifest})
    }))
    await h.start();await h.idle();await h.page.goto(h.origin+'/workspace/'+h.fixture.workspace.id+'#tab=sessions&session='+h.saved.id)
    await h.page.getByText('Verified complete archive.',{exact:true}).waitFor()
    await h.page.locator('#session-records').press('Home');await h.page.getByRole('heading',{name:'A calmer session explorer'}).waitFor()
    const group=h.page.locator('.session-activity-group').first();await group.locator(':scope > summary').click()
    await group.locator('.session-content-tool').first().locator(':scope > summary').click()
    await group.locator('.session-content-tool pre').first().waitFor()
    assert.equal(await h.page.locator('.session-event-inspector,.session-inspect-event,.session-ancestor-path').count(),0)
    assert.equal(await h.page.locator('#session-records').evaluate(node=>node.parentElement.tagName),'SECTION')
    await h.page.locator('#session-records').press('End');await h.page.waitForFunction(()=>{const node=document.getElementById('session-records');return node.dataset.latestPending==='false'&&node.dataset.archiveCanNext==='false'})
    await h.page.evaluate(()=>{window.holdArchivePage=true})
    await h.page.locator('#session-records').press('Home')
    await h.page.waitForFunction(()=>typeof window.releaseArchivePage==='function')
    await h.page.getByRole('button',{name:'Cancel',exact:true}).waitFor({state:'visible'})
    assert.ok(await h.page.locator('#session-records').evaluate(node=>node.children.length>0),'busy local navigation retains verified messages before cancellation')
    const cancelled=await h.page.evaluate(()=>{document.querySelector('[data-sealed-action="cancel"]').click();return {records:document.getElementById('session-records').children.length,inspector:document.querySelector('.session-inspected-event')?.children.length || 0}})
    assert.deepEqual(cancelled,{records:0,inspector:0})
    await h.page.getByText('Archive loading cancelled.',{exact:true}).waitFor()
    await h.page.evaluate(()=>window.releaseArchivePage?.(true))
    await h.page.locator('.session-sealed-viewer').getByRole('button',{name:'Retry',exact:true}).click()
    await h.page.getByText('Verified complete archive.',{exact:true}).waitFor()
    await h.page.locator('#session-records').press('Home');await h.page.getByRole('heading',{name:'A calmer session explorer'}).waitFor()
    await group.locator(':scope > summary').click()
    await group.locator('.session-content-tool').first().locator(':scope > summary').click()
    await group.locator('.session-content-tool pre').first().waitFor()
    const before=await h.page.evaluate(async()=>{const root=await(await navigator.storage.getDirectory()).getDirectoryHandle('hmem-session-archives-v1');const names=[];for await(const[name]of root.entries())names.push(name);return names})
    switched=true
    const ack=await h.page.evaluate(()=>{const start=performance.now();localStorage.setItem('hmem-auth-token','fixture-changed-credential');dispatchEvent(new StorageEvent('storage',{key:'hmem-auth-token',newValue:'fixture-changed-credential'}));return {ms:performance.now()-start,children:document.getElementById('session-records')?.children.length || 0,inspector:document.querySelector('.session-inspected-event')?.children.length || 0}})
    assert.equal(ack.children,0);assert.equal(ack.inspector,0);assert.ok(ack.ms<=250)
    await h.page.waitForFunction(async old=>{const root=await(await navigator.storage.getDirectory()).getDirectoryHandle('hmem-session-archives-v1');for await(const[name]of root.entries())if(old.includes(name))return false;return true},before)
    release();await h.page.getByText('Verified complete archive.',{exact:true}).waitFor()
    await h.page.route('**/api/v1/fixture-unauthorized',route=>route.fulfill({status:401,json:{error:'unauthorized'}}))
    const lost=await h.page.evaluate(async()=>{await new Promise(resolve=>{const request=new XMLHttpRequest();request.open('GET','/api/v1/fixture-unauthorized');request.send();request.addEventListener('loadend',resolve)});return document.getElementById('session-records')?.children.length || 0})
    assert.equal(lost,0)
    assert.equal(await h.page.evaluate(()=>document.querySelector('.session-inspected-event')?.children.length || 0),0)
    assert.deepEqual(h.errors,[],'clearing and reopening transcript details must not corrupt Elm virtual DOM ownership')
    assert.ok(!h.reads.some(row=>row.path.endsWith('/evidence')))
  }finally{release?.();await h.page.evaluate(()=>window.releaseArchivePage?.(true)).catch(()=>{});await h.close()}
})

test('actual native file-close failure blocks replacement and requires reload',{timeout:60000},async()=>{
  const h=await openSessionExplorer()
  try {
    await h.page.addInitScript(()=>{
      const Native=Worker;window.createdArchiveWorkers=0
      window.Worker=class extends Native{constructor(url,options){if(String(url).includes('session-archive-worker-')){window.createdArchiveWorkers++;const entry=URL.createObjectURL(new Blob(['const queued=[];self.onmessage=event=>queued.push(event);FileSystemSyncAccessHandle.prototype.close=function(){throw Error("fixture_actual_close_failed")};await import('+JSON.stringify(new URL(url,location.href).href)+');for(const event of queued)self.onmessage(event);'],{type:'text/javascript'}));super(entry,{type:'module'})}else super(url,options)}}
    })
    const raw=h.content.get(h.saved.id),gzip=gzipSync(raw),manifest=makeManifest(h,raw,gzip)
    await h.page.route('**/sessions/**/archive**',phaseArchiveRoute(route=>new URL(route.request().url()).pathname.endsWith('/content')?route.fulfill({headers:{'Content-Type':'application/gzip','Content-Length':String(gzip.length),'X-HMem-Archive-Generation':generation,'X-HMem-Archive-SHA256':hash(gzip),'X-HMem-Archive-Raw-Bytes':String(raw.length),'X-HMem-Archive-Raw-SHA256':hash(raw)},body:gzip}):route.fulfill({json:manifest})))
    await h.start();await h.idle();await h.page.goto(h.origin+'/workspace/'+h.fixture.workspace.id+'#tab=sessions&session='+h.saved.id)
    await h.page.getByRole('alert').filter({hasText:'Archive cleanup could not be confirmed'}).waitFor()
    assert.equal(await h.page.evaluate(()=>window.createdArchiveWorkers),1)
    assert.equal(await h.page.getByRole('button',{name:'Retry',exact:true}).count(),0,'unconfirmed cleanup has no in-place retry')
    await h.page.waitForTimeout(100)
    await h.page.getByRole('alert').filter({hasText:'Archive cleanup could not be confirmed'}).waitFor({timeout:2000})
    assert.equal(await h.page.evaluate(()=>window.createdArchiveWorkers),1)
  }finally{await h.close()}
})

test('production compressed download shows accepted bytes before completion and cancels stale progress',{timeout:60000},async()=>{
  for(const cancel of [false,true]){
    let h,http,response
    try{
      h=await openSessionExplorer()
      const raw=h.content.get(h.saved.id),gzip=gzipSync(raw),manifest=makeManifest(h,raw,gzip);assert.ok(gzip.length>24)
      const server=createServer((request,value)=>{
        if(request.headers['if-none-match']){
          assert.equal(request.headers['if-none-match'],phaseArchiveETag)
          value.writeHead(304,{ETag:phaseArchiveETag,'Access-Control-Allow-Origin':h.origin,'Access-Control-Allow-Credentials':'true'});value.end();return
        }
        response=value
        value.writeHead(200,{ETag:phaseArchiveETag,'Content-Type':'application/gzip','Content-Length':String(gzip.length),'X-HMem-Archive-Generation':generation,'X-HMem-Archive-SHA256':hash(gzip),'X-HMem-Archive-Raw-Bytes':String(raw.length),'X-HMem-Archive-Raw-SHA256':hash(raw),'Access-Control-Allow-Origin':h.origin,'Access-Control-Allow-Credentials':'true'})
        value.flushHeaders();value.write(gzip.subarray(0,8))
      })
      http={async close(){server.closeAllConnections();if(server.listening)await new Promise((yes,no)=>server.close(error=>error?no(error):yes()))}}
      await new Promise((yes,no)=>{server.once('error',no);server.listen(0,'127.0.0.1',yes)})
      const url='http://127.0.0.1:'+server.address().port+'/held-progress'
      await h.page.addInitScript(()=>{
        const Native=Worker;window.acceptedProgress=[]
        window.Worker=class extends Native{
          constructor(url,options){super(url,options);window.progressWorker=this;this.addEventListener('message',({data})=>{if(data.kind==='progress'&&data.phase==='downloading'){window.acceptedProgress.push(data.compressedBytes);if(window.acceptedProgress.length>16)throw Error('bounded_progress_events')}})}
          postMessage(message,transfer){if(message.kind==='start')window.progressStamp=message.stamp;return super.postMessage(message,transfer)}
        }
      })
      await h.page.route('**/sessions/**/archive**',phaseArchiveRoute(route=>new URL(route.request().url()).pathname.endsWith('/content')?route.continue({url}):route.fulfill({json:manifest})))
      await h.start();await h.idle()
      await h.page.goto(h.origin+'/workspace/'+h.fixture.workspace.id+'#tab=sessions&session='+h.saved.id)
      await h.page.getByText('Downloading complete archive… 8 bytes',{exact:true}).waitFor({timeout:10000})
      assert.equal(await h.page.locator('#session-records').evaluate(node=>node.children.length),0)
      await new Promise(resolve=>setTimeout(resolve,1050));response.write(gzip.subarray(8,16))
      await h.page.getByText('Downloading complete archive… 16 bytes',{exact:true}).waitFor({timeout:10000})
      const stale=await h.page.evaluate(()=>{
        window.progressWorker.onmessage({data:{...window.progressStamp,load:window.progressStamp.load+1,kind:'progress',phase:'downloading',compressedBytes:'999',rawBytes:'0'}})
        return document.querySelector('.session-sealed-viewer [role="status"]').textContent
      })
      assert.equal(stale,'Downloading complete archive… 16 bytes')
      if(cancel){
        await h.page.evaluate(()=>{window.retiredProgressCallback=window.progressWorker.onmessage;window.retiredProgressStamp={...window.progressStamp};if(typeof window.retiredProgressCallback!=='function')throw Error('missing_current_progress_callback')})
        await h.page.locator('[data-sealed-action="cancel"]').click()
        await h.page.getByText('Archive loading cancelled.',{exact:true}).waitFor({timeout:10000})
        const late=await h.page.evaluate(()=>{
          if(window.progressWorker.onmessage!==null)throw Error('retired_progress_callback_still_attached')
          window.retiredProgressCallback({data:{...window.retiredProgressStamp,kind:'progress',phase:'downloading',compressedBytes:'999',rawBytes:'0'}})
          return {text:document.querySelector('.session-sealed-viewer [role="status"]').textContent,rows:document.getElementById('session-records').children.length}
        })
        assert.deepEqual(late,{text:'Archive loading cancelled.',rows:0})
        await h.page.waitForFunction(async()=>{try{const root=await(await navigator.storage.getDirectory()).getDirectoryHandle('hmem-session-archives-v1');for await(const entry of root.entries())return false;return true}catch(error){if(error.name==='NotFoundError')return true;throw error}},null,{timeout:10000})
      }else{
        response.end(gzip.subarray(16))
        await h.page.getByText('Verified complete archive.',{exact:true}).waitFor({timeout:10000})
        const counts=await h.page.evaluate(()=>window.acceptedProgress)
        assert.equal(counts[0],'0');assert.ok(counts.includes('8'));assert.ok(counts.includes('16'));assert.equal(counts.at(-1),String(gzip.length))
      }
      assert.equal(h.reads.filter(row=>row.path.endsWith('/evidence')).length,0);assert.deepEqual(h.errors,[])
    }finally{await cleanupQualificationOwner({h,http})}
  }
})

test('production Cancel retires a held content request before it can publish',{timeout:60000},async()=>{
  const h=await openSessionExplorer({extraRecords:Array.from({length:100},(_,i)=>({text:'Authorization history '+i+' keeps indexed scrolling available.'}))});let release
  try {
    const raw=h.content.get(h.saved.id),gzip=gzipSync(raw),manifest=makeManifest(h,raw,gzip),gate=new Promise(resolve=>{release=resolve})
    await h.page.route('**/sessions/**/archive**',phaseArchiveRoute(async route=>{
      if(new URL(route.request().url()).pathname.endsWith('/content')){await gate;await route.fulfill({headers:{'Content-Type':'application/gzip','Content-Length':String(gzip.length),'X-HMem-Archive-Generation':generation,'X-HMem-Archive-SHA256':hash(gzip),'X-HMem-Archive-Raw-Bytes':String(raw.length),'X-HMem-Archive-Raw-SHA256':hash(raw)},body:gzip}).catch(()=>{})}
      else await route.fulfill({json:manifest})
    }))
    await h.start();await h.idle();await h.page.goto(h.origin+'/workspace/'+h.fixture.workspace.id+'#tab=sessions&session='+h.saved.id)
    await h.page.getByText('Downloading complete archive… 0 bytes',{exact:true}).waitFor()
    await h.page.getByRole('button',{name:'Cancel',exact:true}).click()
    await h.page.getByText('Archive loading cancelled.',{exact:true}).waitFor({timeout:2000})
    release();await h.page.waitForTimeout(100)
    assert.equal(await h.page.getByText('Verified complete archive.',{exact:true}).count(),0)
    assert.equal(await h.page.locator('#session-records .session-record').count(),0);assert.deepEqual(h.errors,[])
  }finally{release?.();await h.close()}
})

test('production sealed transcript details hydrate the source block without a separate inspector',{timeout:60000},async()=>{
  const h=await openSessionExplorer()
  try {
    const raw=h.content.get(h.saved.id),gzip=gzipSync(raw),manifest=makeManifest(h,raw,gzip)
    await h.page.route('**/sessions/**/archive**',phaseArchiveRoute(route=>new URL(route.request().url()).pathname.endsWith('/content')?route.fulfill({headers:{'Content-Type':'application/gzip','Content-Length':String(gzip.length),'X-HMem-Archive-Generation':generation,'X-HMem-Archive-SHA256':hash(gzip),'X-HMem-Archive-Raw-Bytes':String(raw.length),'X-HMem-Archive-Raw-SHA256':hash(raw)},body:gzip}):route.fulfill({json:manifest})))
    await h.start();await h.idle();await h.page.goto(h.origin+'/workspace/'+h.fixture.workspace.id+'#tab=sessions&session='+h.saved.id)
    await h.page.getByText('Verified complete archive.',{exact:true}).waitFor()
    const group=h.page.locator('.session-activity-group').first();await group.locator(':scope > summary').click()
    const tool=group.locator('.session-content-tool').first();await tool.locator(':scope > summary').click();await tool.locator('pre').waitFor({timeout:2000})
    assert.deepEqual(JSON.parse(await tool.locator('pre').innerText()),{path:'src/Feature/Sessions.elm'});assert.ok(await tool.locator('.json-key').count()>0)
    assert.equal(await h.page.locator('.session-ancestor-path,.session-event-inspector,.session-inspect-event').count(),0)
    assert.deepEqual(h.errors,[])
  }finally{await h.close()}
})

test('resolved Markdown reference expansion is paged before mounting every exact link',{timeout:60000},async()=>{
  const destination='https://example.test/'+'x'.repeat(8400),markdown='[r]: '+destination+'\n\n'+Array.from({length:1000},()=>'[x][r]').join('\n\n')
  assert.ok(Buffer.byteLength(markdown)>16384 && Buffer.byteLength(markdown)<=1048576)
  assert.ok(Buffer.byteLength(destination)*1000>8388608,'all resolved href attributes exceed eight MiB before bounded mounting')
  const h=await openSessionExplorer({transcriptBytes:Buffer.from(JSON.stringify({text:markdown})+'\n')})
  try {
    let firstError=false;h.page.on('pageerror',error=>{if(!firstError){firstError=true;console.log('bounded-markdown-error '+String(error.stack).slice(0,2400))}})
    await h.page.addInitScript(captureArchiveTelemetry)
    const raw=h.content.get(h.saved.id),gzip=gzipSync(raw),manifest=makeManifest(h,raw,gzip)
    await h.page.route('**/sessions/**/archive**',phaseArchiveRoute(route=>new URL(route.request().url()).pathname.endsWith('/content')?route.fulfill({headers:{'Content-Type':'application/gzip','Content-Length':String(gzip.length),'X-HMem-Archive-Generation':generation,'X-HMem-Archive-SHA256':hash(gzip),'X-HMem-Archive-Raw-Bytes':String(raw.length),'X-HMem-Archive-Raw-SHA256':hash(raw)},body:gzip}):route.fulfill({json:manifest})))
    await h.start();await h.idle();await h.page.goto(h.origin+'/workspace/'+h.fixture.workspace.id+'#tab=sessions&session='+h.saved.id)
    await h.page.getByText('Verified complete archive.',{exact:true}).waitFor()
    await h.page.locator('#session-records a').first().waitFor()
    const {turns,maxBytes}=await renderedLinks(h,destination,1000);assert.ok(turns>0);assert.ok(maxBytes<4194304);assert.deepEqual(h.errors,[])
  }finally{await h.close()}
})

test('complete source-block hydration preserves fences and late reference definitions across wire chunks',{timeout:60000},async()=>{
  const markdown='```text\n'+'fenced line\n'.repeat(2200)+'```\n\n[Late reference][late]\n\n[late]: https://example.test/full-source\n'
  const h=await openSessionExplorer({transcriptBytes:Buffer.from(JSON.stringify({text:markdown})+'\n')})
  try {
    await h.page.addInitScript(captureArchiveTelemetry)
    const raw=h.content.get(h.saved.id),gzip=gzipSync(raw),manifest=makeManifest(h,raw,gzip)
    await h.page.route('**/sessions/**/archive**',phaseArchiveRoute(route=>new URL(route.request().url()).pathname.endsWith('/content')?route.fulfill({headers:{'Content-Type':'application/gzip','Content-Length':String(gzip.length),'X-HMem-Archive-Generation':generation,'X-HMem-Archive-SHA256':hash(gzip),'X-HMem-Archive-Raw-Bytes':String(raw.length),'X-HMem-Archive-Raw-SHA256':hash(raw)},body:gzip}):route.fulfill({json:manifest})))
    await h.start();await h.idle();await h.page.goto(h.origin+'/workspace/'+h.fixture.workspace.id+'#tab=sessions&session='+h.saved.id)
    await h.page.getByText('Verified complete archive.',{exact:true}).waitFor()
    const link=h.page.locator('#session-records a[href="https://example.test/full-source"]');await link.waitFor({timeout:10000})
    assert.equal(await link.innerText(),'Late reference')
    const code=await h.page.locator('#session-records pre code.language-text').allTextContents();assert.equal(code.join(''),'fenced line\n'.repeat(2200))
    const state=await h.page.evaluate(()=>({metric:window.archiveTelemetry,dom:new TextEncoder().encode(document.getElementById('session-records').outerHTML).length}))
    assert.ok(state.metric.maxPageBytes+Buffer.byteLength(JSON.stringify(markdown))+state.dom<=4194304);assert.ok(state.metric.maxBodyBytes<=65536)
    assert.deepEqual(h.errors,[])
  }finally{await h.close()}
})

test('bounded recovery reports preserved-directory limit honestly and stale deletion retries make progress',{timeout:60000},async()=>{
  for(const mode of ['preserved-limit','stale-progress']) {
    const h=await openSessionExplorer()
    try {
      await h.start();await h.idle()
      const owners=await h.page.evaluate(async mode=>{
        const root=await(await navigator.storage.getDirectory()).getDirectoryHandle('hmem-session-archives-v1',{create:true}),names=[]
        for(let index=0;index<(mode==='preserved-limit'?129:10);index++){const name=crypto.randomUUID();await root.getDirectoryHandle(name,{create:true});names.push(name)}
        const ordered=[];for await(const[name]of root.entries())ordered.push(name)
        for(const name of mode==='preserved-limit'?[ordered.at(-1)]:names){const entry=await root.getDirectoryHandle(name),writer=await(await entry.getFileHandle('owner.json',{create:true})).createWritable();await writer.write(JSON.stringify({format:'hmem-session-archive',version:1,owner:name}));await writer.close()}
        return {preserved:mode==='preserved-limit'?ordered.slice(0,128):[],stale:ordered.at(-1)}
      },mode)
      const raw=Buffer.from('{"text":"fresh"}\n'),gzip=gzipSync(raw),first=await nativeProbe(h,raw,gzip)
      assert.equal(first.error,mode==='preserved-limit'?'archive_recovery_limit':'archive_recovery_pending');assert.equal(first.cleanup,true)
      const names=await h.page.evaluate(async()=>{const root=await(await navigator.storage.getDirectory()).getDirectoryHandle('hmem-session-archives-v1'),names=[];for await(const[name]of root.entries())names.push(name);return names})
      if(mode==='preserved-limit'){assert.ok(owners.preserved.every(name=>names.includes(name)));assert.ok(names.includes(owners.stale));const again=await nativeProbe(h,raw,gzip);assert.equal(again.error,'archive_recovery_limit')}
      else {assert.equal(names.length,2);const again=await nativeProbe(h,raw,gzip);assert.equal(again.error,null);assert.equal(again.cleanup,true)}
    }finally{await h.close()}
  }
})

test('ordered-list ordinals and table column/header context survive rendered continuations',{timeout:60000},async()=>{
  const destination='https://example.test/'+'x'.repeat(8000)
  const examples=[
    {kind:'list',text:'7. '+ '[x][r]'.repeat(300)+'\n8. '+ '[x][r]'.repeat(300)+'\n9. '+ '[x][r]'.repeat(300)+'\n\n[r]: '+destination,links:900},
    {kind:'table',text:'| Column A | Column B | Column C |\n| --- | --- | --- |\n| a | '+ '[x][r]'.repeat(500)+' | c |\n\n[r]: '+destination,links:500}
  ]
  for(const example of examples) {
    const h=await openSessionExplorer({transcriptBytes:Buffer.from(JSON.stringify({text:example.text})+'\n')})
    try {
      const raw=h.content.get(h.saved.id),gzip=gzipSync(raw),manifest=makeManifest(h,raw,gzip)
      await h.page.route('**/sessions/**/archive**',phaseArchiveRoute(route=>new URL(route.request().url()).pathname.endsWith('/content')?route.fulfill({headers:{'Content-Type':'application/gzip','Content-Length':String(gzip.length),'X-HMem-Archive-Generation':generation,'X-HMem-Archive-SHA256':hash(gzip),'X-HMem-Archive-Raw-Bytes':String(raw.length),'X-HMem-Archive-Raw-SHA256':hash(raw)},body:gzip}):route.fulfill({json:manifest})))
      await h.start();await h.idle();await h.page.goto(h.origin+'/workspace/'+h.fixture.workspace.id+'#tab=sessions&session='+h.saved.id)
      try{await h.page.getByText('Verified complete archive.',{exact:true}).waitFor({timeout:3000})}catch(error){throw Error('structural '+example.kind+': '+JSON.stringify({errors:h.errors.slice(0,2),status:await h.page.locator('.session-sealed-viewer [role="status"]').allTextContents()}))}
      const {counts}=await renderedLinks(h,destination,example.links,example.kind);if(example.kind==='list')assert.deepEqual(counts,{1:300,2:300,3:300});assert.deepEqual(h.errors,[])
    }finally{await h.close()}
  }
})

test('only approved oversized structured reasoning displays exact JSON with literal Markdown',{timeout:60000},async()=>{
  let deep=0;for(let index=0;index<800;index++)deep=[deep]
  const values=[{type:'response_item',payload:{type:'reasoning',summary:{note:'[Open](https://example.test/normal)',small:true}}},{type:'response_item',payload:{type:'reasoning',summary:{note:'[Open](https://example.test/large)',deep}}}]
  const h=await openSessionExplorer({transcriptBytes:Buffer.from(values.map(value=>JSON.stringify(value)).join('\n')+'\n')})
  try {
    const raw=h.content.get(h.saved.id),gzip=gzipSync(raw),manifest=makeManifest(h,raw,gzip)
    await h.page.route('**/sessions/**/archive**',phaseArchiveRoute(route=>new URL(route.request().url()).pathname.endsWith('/content')?route.fulfill({headers:{'Content-Type':'application/gzip','Content-Length':String(gzip.length),'X-HMem-Archive-Generation':generation,'X-HMem-Archive-SHA256':hash(gzip),'X-HMem-Archive-Raw-Bytes':String(raw.length),'X-HMem-Archive-Raw-SHA256':hash(raw)},body:gzip}):route.fulfill({json:manifest})))
    await h.start();await h.idle();await h.page.goto(h.origin+'/workspace/'+h.fixture.workspace.id+'#tab=sessions&session='+h.saved.id);await h.page.getByText('Verified complete archive.',{exact:true}).waitFor()
    await h.page.locator('.session-activity-group').first().locator(':scope > summary').click()
    const reasoning=h.page.locator('.session-content-thinking');await reasoning.first().locator('summary').click();await h.page.locator('a[href="https://example.test/normal"]').waitFor({timeout:5000}).catch(async error=>{console.log(JSON.stringify({phase:'normal-structured-reasoning',details:await reasoning.evaluateAll(nodes=>nodes.map(node=>({key:node.dataset.eventKey,open:node.open,html:node.innerHTML.slice(0,1200)}))),reader:await h.page.locator('#session-records').evaluate(node=>({attrs:{...node.dataset},top:node.scrollTop,height:node.scrollHeight,needed:[...node.querySelectorAll('[data-sealed-body-needed]')].map(node=>({...node.dataset}))}))}));throw error})
    await reasoning.nth(1).locator('summary').click();await reasoning.nth(1).locator('pre').first().waitFor()
    const parts=new Map();for(let sample=0;sample<2;sample++){for(const part of await reasoning.nth(1).locator('[data-body-part-index]').evaluateAll(nodes=>nodes.map(node=>({part:Number(node.dataset.bodyPartIndex),text:node.textContent})))){if(parts.has(part.part))assert.equal(parts.get(part.part),part.text);parts.set(part.part,part.text)}await h.idle()}
    assert.ok([...parts].sort((a,b)=>a[0]-b[0]).map(([,text])=>text).join('').includes('[Open](https://example.test/large)'));assert.equal(await h.page.locator('a[href="https://example.test/large"]').count(),0)
    await h.page.getByText('Large structured reasoning · exact JSON',{exact:true}).waitFor()
    assert.deepEqual(h.errors,[])
  }finally{await h.close()}
})

test('diagnostic dense table uses the inherited active Markdown parser',{skip:process.env.HMEM_ARCHIVE_DENSE_DIAGNOSTIC!=='1',timeout:15000},async()=>{
  const text='| A | B | C |\n| --- | --- | --- |\n| a | '+'[x][r]'.repeat(1000)+' | c |\n\n[r]: https://example.test/'+'x'.repeat(8000)
  const h=await openSessionExplorer({transcriptBytes:Buffer.from(JSON.stringify({text})+'\n')})
  try{await h.start();await h.idle();const failing=h.page.waitForEvent('pageerror',{timeout:5000});await h.page.goto(h.origin+'/workspace/'+h.fixture.workspace.id+'#tab=sessions&session='+h.active.id);const error=await failing;assert.match(error.message,/Maximum call stack/);console.log('Inherited active-path diagnostic: '+String(error.stack).slice(0,1200))}
  finally{await h.close()}
})


test('bounded tool JSON DOM text remains exact across incomplete highlighted body parts',{timeout:60000},async()=>{
  const payload={text:'λ😀'+'abcdefg'.repeat(6000)},expected=JSON.stringify(payload,null,2)
  const raw=Buffer.from(JSON.stringify({type:'response_item',payload:{type:'function_call',name:'long_payload',arguments:JSON.stringify(payload)}})+'\n')
  assert.ok(Buffer.byteLength(expected)>40000 && raw.length<=1048576)
  const h=await openSessionExplorer({transcriptBytes:raw})
  try{
    await h.page.addInitScript(captureArchiveTelemetry)
    const gzip=gzipSync(raw),manifest=makeManifest(h,raw,gzip)
    await h.page.route('**/sessions/**/archive**',phaseArchiveRoute(route=>new URL(route.request().url()).pathname.endsWith('/content')?route.fulfill({headers:{'Content-Type':'application/gzip','Content-Length':String(gzip.length),'X-HMem-Archive-Generation':generation,'X-HMem-Archive-SHA256':hash(gzip),'X-HMem-Archive-Raw-Bytes':String(raw.length),'X-HMem-Archive-Raw-SHA256':hash(raw)},body:gzip}):route.fulfill({json:manifest})))
    await h.start();await h.idle();await h.page.goto(h.origin+'/workspace/'+h.fixture.workspace.id+'#tab=sessions&session='+h.saved.id)
    await h.page.getByText('Verified complete archive.',{exact:true}).waitFor()
    await h.page.locator('.session-activity-group > summary').click()
    await h.page.locator('details.session-content-tool > summary').click()
    const pre=h.page.locator('#session-records pre.session-tool-content');await pre.waitFor()
    const parts=new Map()
    for(let turn=0;turn<10;turn++){
      for(const part of await h.page.locator('[data-body-part-index]').evaluateAll(nodes=>nodes.map(node=>({part:Number(node.dataset.bodyPartIndex),text:node.textContent})))){if(parts.has(part.part))assert.equal(parts.get(part.part),part.text);parts.set(part.part,part.text)}
      if([...parts.values()].join('').length===expected.length)break
      await continuousScrollStep(h.page,1);await h.idle()
    }
    assert.ok(parts.size>=3&&parts.size<10);assert.deepEqual([...parts.keys()].sort((a,b)=>a-b),Array.from({length:parts.size},(_,i)=>i));assert.equal([...parts].sort((a,b)=>a[0]-b[0]).map(([,text])=>text).join(''),expected,'actual displayed token spans reconstruct complete exact pretty JSON')
    const telemetry=await h.page.evaluate(()=>window.archiveTelemetry),domBytes=await h.page.locator('#session-records').evaluate(node=>new TextEncoder().encode(node.outerHTML).length)
    assert.ok(telemetry.maxBodyBytes<=65536);assert.ok(telemetry.maxPageBytes+domBytes<=4194304);assert.deepEqual(h.errors,[])
  }finally{await h.close()}
})


test('fixed evidence family codec reconstructs all original bytes records metadata and timing without writes',async()=>{
  const currentBytes=await readFile('perf/session-archive.qualification.json'),current=JSON.parse(currentBytes)
  const before=current.schemaVersion===3?decodeQualificationFamily(current.retainedFamily).bytes:currentBytes
  const candidate=qualificationFamilyCandidate(before),again=qualificationFamilyCandidate(before),decoded=decodeQualificationFamily(candidate.retainedFamily),original=JSON.parse(before)
  assert.equal(JSON.stringify(candidate),JSON.stringify(again),'deterministic one-off candidate')
  assert.deepEqual(decoded.bytes,before);assert.equal(hash(decoded.bytes),retainedFamilySha256)
  assert.deepEqual(decoded.original,original);assert.deepEqual(candidate.matchedActiveLive,original.matchedActiveLive)
  assert.equal(candidate.attempts.length,0);assert.equal(decoded.original.attempts.length,8)
  assert.deepEqual(candidate.historyRef,original.historyRef);assert.deepEqual(candidate.custody,{files:2,maxFileBytes:1048576,maxTotalBytes:2097152})
  assert.throws(()=>qualificationFamilyCandidate(Buffer.from(JSON.stringify(candidate))),/bounded approved evidence family|immutable family beforeimage/)
  assert.throws(()=>decodeQualificationFamily({...candidate.retainedFamily,sourceBytes:1048577}))
  assert.throws(()=>decodeQualificationFamily({...candidate.retainedFamily,data:candidate.retainedFamily.data+' '}),/canonical base64/)
  assert.throws(()=>decodeQualificationFamily({...candidate.retainedFamily,gzipSha256:'0'.repeat(64)}))
  const oversized=gzipSync(Buffer.alloc(retainedFamilyBytes+1))
  assert.throws(()=>decodeQualificationFamily({...candidate.retainedFamily,data:oversized.toString('base64'),gzipBytes:oversized.length,gzipSha256:hash(oversized)}),/larger than|too large|length/i)
  assert.deepEqual(await readFile('perf/session-archive.qualification.json'),currentBytes,'codec preview never replaces current evidence')
  const encoded=Buffer.from(JSON.stringify(candidate,null,2)+'\n');console.log(JSON.stringify({evidenceFamilyPreview:{candidateSha256:hash(encoded),candidateBytes:encoded.length,sourceSha256:hash(decoded.bytes),sourceBytes:decoded.bytes.length,canonicalSha256:hash(JSON.stringify(decoded.original)),attemptObjectSha256:candidate.retainedFamily.attemptObjectSha256,matchedObjectSha256:candidate.retainedFamily.matchedObjectSha256,timingVerbatim:JSON.stringify(candidate.matchedActiveLive)===JSON.stringify(original.matchedActiveLive)}}))
})


test('actual verified raw cache coalesces first last pages without changing exact native bodies',{timeout:60000},async()=>{
  const expected=Array.from({length:256},(_,index)=>{
    let text='row '+String(index).padStart(8,'0')+' λ😀 '
    text+='x'.repeat(1024-Buffer.byteLength(JSON.stringify({text})+'\r\n'))
    return text
  })
  const raw=Buffer.from(expected.map(text=>JSON.stringify({text})+'\r\n').join(''));assert.equal(raw.length,256*1024)
  const h=await openSessionExplorer()
  try{
    await h.start();await h.idle()
    const prelude="const stats={reads:0,readBytes:0};const read=FileSystemSyncAccessHandle.prototype.read;FileSystemSyncAccessHandle.prototype.read=function(bytes,options){stats.reads++;stats.readBytes+=bytes.byteLength;return read.call(this,bytes,options)};const post=self.postMessage.bind(self);self.postMessage=(value,...rest)=>post(value.kind==='page'?{...value,rawReadControl:{...stats}}:value,...rest);"
    const result=await nativeProbe(h,raw,gzipSync(raw),{prelude,cacheReadControl:{expected}})
    assert.equal(result.error,null);assert.equal(result.cleanup,true);assert.equal(result.cachePages.length,22)
    assert.equal(result.cachePages[0].reads,1);assert.equal(result.cachePages[1].reads,2)
    assert.ok(result.cachePages.every(page=>page.rows===64&&page.wireBytes<=4194304&&page.reads<=2&&page.readBytes<=131072))
    assert.equal(result.cachePages.at(-1).readBytes,131072)
    const left=await h.page.evaluate(async()=>{const root=await(await navigator.storage.getDirectory()).getDirectoryHandle('hmem-session-archives-v1');let count=0;for await(const entry of root.entries())count++;return count})
    assert.equal(left,0);assert.deepEqual(h.errors,[])
    console.log(JSON.stringify({nativeCacheReadControl:{pages:result.cachePages.length,rowsPerPage:64,rawReads:result.cachePages.at(-1).reads,rawReadBytes:result.cachePages.at(-1).readBytes,exactBodies:true,cleanup:result.cleanup,remainingGenerations:left}}))
  }finally{await h.close()}
})
