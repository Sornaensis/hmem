import assert from 'node:assert/strict'
import test from 'node:test'
import {gzipSync} from 'node:zlib'
import {createHash} from 'node:crypto'
import {join} from 'node:path'
import {openSessionExplorer,continuousScrollStep,traceSessionBridge} from './support/sessions-ux-fixture.mjs'
import {createEventPayloadPager} from '../src/session-event-json.js'

const until=async fn=>{const end=Date.now()+15000;while(Date.now()<end){if(await fn())return;await new Promise(resolve=>setTimeout(resolve,25))}throw Error('Progressive conversation did not converge')}
const state=page=>page.locator('#session-records').evaluate(node=>({before:Number(node.dataset.historyBefore),after:Number(node.dataset.historyAfter),latest:Number(node.dataset.historyLatest),loading:node.dataset.historyLoading==='true',offsets:[...node.querySelectorAll('.session-record')].map(row=>Number(row.dataset.offset))}))
const armLatestOverlay=async page=>{const viewport=page.locator('#session-records'),before=await viewport.evaluate(node=>({top:node.scrollTop,height:node.clientHeight,scroll:node.scrollHeight,attrs:{...node.dataset}}));await viewport.hover();await page.mouse.wheel(0,-320);await until(()=>page.getByRole('button',{name:'Jump to latest',exact:true}).isVisible()).catch(async error=>{console.log(JSON.stringify({phase:'arm-overlay',before,after:await viewport.evaluate(node=>({top:node.scrollTop,height:node.clientHeight,scroll:node.scrollHeight,attrs:{...node.dataset},host:{...node.closest('.session-viewer').dataset}}))}));throw error})}

const conversationPaging=page=>page.locator('.session-viewer').getByRole('button',{name:/^(Earlier messages|Newer messages|First|Previous|Next|Last|Previous presentation|Continue presentation|Previous body part|Continue body|Read complete message)$/})
const payloadReadingPoint=viewport=>viewport.evaluate(node=>{
 const box=node.getBoundingClientRect()
 for(const span of node.querySelectorAll('[data-payload-start]')){
  if(span.firstChild?.nodeType!==3||!span.textContent.length)continue
  let top=box.top,bottom=box.bottom,parent=span.parentElement
  for(let depth=0;parent&&parent!==node&&depth<64;depth++,parent=parent.parentElement){if(/auto|scroll|hidden|clip/.test(getComputedStyle(parent).overflowY)){const r=parent.getBoundingClientRect();top=Math.max(top,r.top+parent.clientTop);bottom=Math.min(bottom,r.top+parent.clientTop+parent.clientHeight)}}
  const r=span.getBoundingClientRect();if(!r.height||r.bottom<=top||r.top>=bottom)continue
  const point=offset=>{const range=document.createRange();range.setStart(span.firstChild,offset);range.setEnd(span.firstChild,Math.min(span.firstChild.length,offset+1));return range}
  let first=0,last=span.firstChild.length;while(first<last){const middle=Math.floor((first+last)/2);if(point(middle).getBoundingClientRect().bottom>top+1)last=middle;else first=middle+1}
  const range=point(first),rect=range.getBoundingClientRect();if(rect.height&&rect.top<bottom)return {offset:Number(span.dataset.payloadStart)+first,top:rect.top-box.top,text:range.toString()}
 }
 throw Error('No actual opened payload character intersects its scroll clips')
})
const routeVerifiedArchive=async(h,raw)=>{
 const hash=bytes=>createHash('sha256').update(bytes).digest('hex'),gzip=gzipSync(raw),generation='33333333-3333-4333-8333-333333333333',etag='"continuous-verified-archive"'
 const manifest={workspace_id:h.fixture.workspace.id,session_id:h.saved.id,state:'ready',generation_id:generation,codec:'gzip-v1',raw_bytes:String(raw.length),raw_records:String(raw.toString().split('\n').filter(Boolean).length),raw_sha256:hash(raw),compressed_bytes:String(gzip.length),compressed_sha256:hash(gzip),content_path:`/api/v1/workspaces/${h.fixture.workspace.id}/sessions/${h.saved.id}/archive/${generation}/content`}
 await h.page.route('**/sessions/**/archive**',route=>new URL(route.request().url()).pathname.endsWith('/content')?route.fulfill(route.request().headers()['if-none-match']?{status:304,headers:{ETag:etag}}:{headers:{'Content-Type':'application/gzip',ETag:etag,'Content-Length':String(gzip.length),'X-HMem-Archive-Generation':generation,'X-HMem-Archive-SHA256':hash(gzip),'X-HMem-Archive-Raw-Bytes':String(raw.length),'X-HMem-Archive-Raw-SHA256':hash(raw)},body:gzip}):route.fulfill({json:manifest}))
}
const shortTailTranscript=()=>Buffer.from([
 ...Array.from({length:12},(_,i)=>JSON.stringify({text:`Earlier occurrence ${i}. `+'Reading context. '.repeat(150)})),
 JSON.stringify({text:'Large preceding message. '+'Readable history. '.repeat(14000)}),
 ...Array.from({length:20},(_,i)=>JSON.stringify({type:'response_item',payload:{type:'function_call',name:'inspect',call_id:`short-tail-${i}`,arguments:JSON.stringify({value:'x'.repeat(350)})}})),
].join('\n')+'\n')

test('continuous contract unknown JSON events preserve raw tokens in a bounded continuous inspector',{timeout:60000},async()=>{
 const source='{"type":"large_unknown","n":9007199254740993,"overflow":1e400,"underflow":1e-400,"negative":-0,"escaped":"\\u0061","duplicate":1,"duplicate":2,"values":'+JSON.stringify(Array.from({length:2000},(_,i)=>`${i===0?'EVENT START':i===1999?'EVENT END':'Event'} ${i}. `+'x'.repeat(95)))+'}'
 const raw=Buffer.from('{"type":"event_msg","payload":{"type":"tiny_unknown","value":1}}\n{"type":"event_msg","payload":'+source+'}\n'),h=await openSessionExplorer({transcriptBytes:raw}),{page}=h,viewport=()=>page.locator('#session-records'),inspector=()=>page.locator('session-event-json')
 try{
  await h.start();await page.goto(h.origin+'/workspace/'+h.fixture.workspace.id+'#tab=sessions&session='+h.active.id)
  await page.locator('.session-activity-summary').click();await page.locator('.session-content-event summary').first().click();await inspector().locator('code').waitFor();await h.idle()
  const initial=await viewport().evaluate(node=>({top:node.scrollTop,request:node.dataset.latestRequest}))
  await inspector().press('Home');await inspector().press('End');await inspector().hover();await page.mouse.wheel(0,-100);await h.idle()
  assert.deepEqual(await viewport().evaluate(node=>({top:node.scrollTop,request:node.dataset.latestRequest})),initial,'inner navigation without outer movement never starts outer latest acquisition')
  assert.equal(await page.getByRole('button',{name:'Jump to latest',exact:true}).isVisible(),false)
  await inspector().evaluate(node=>{window.retiredInspector=node});await page.locator('.session-content-event summary').first().click()
  await until(()=>page.evaluate(()=>!window.retiredInspector.isConnected))
  assert.deepEqual(await page.evaluate(()=>({source:window.retiredInspector.payload,text:window.retiredInspector.textContent,before:!!window.retiredInspector._before,after:!!window.retiredInspector._after})),{source:null,text:'',before:false,after:false})
  await page.locator('.session-content-event summary').last().click();await inspector().locator('code').waitFor();await h.idle()
  assert.equal(await inspector().getAttribute('payload'),null);assert.equal(await inspector().evaluate(node=>node.payload),source)
  assert.equal(await page.getByRole('button',{name:/^(Previous JSON part|Continue JSON)$/}).count(),0)
  // A keyed move must retain a property-only source without an Elm reassignment.
  assert.equal(await inspector().evaluate(node=>{const next=node.nextSibling,parent=node.parentNode;parent.insertBefore(node,next);return node.payload}),source)
  await inspector().press('Home');await until(async()=>await inspector().evaluate(node=>node.textContent.includes('EVENT START 0.')))
  const reference=createEventPayloadPager(source,1024)
  const verify=async()=>{const parts=await inspector().evaluate(node=>({bytes:new TextEncoder().encode(node.outerHTML).length,parts:[...node.querySelectorAll('[data-event-json-part]')].map(part=>({index:Number(part.dataset.eventJsonPart),text:part.textContent}))}));assert.ok(parts.bytes<=262144,JSON.stringify({bytes:parts.bytes,count:parts.parts.length}));for(const part of parts.parts)assert.equal(part.text,reference.read(part.index).text);return parts}
  await verify()
  await viewport().hover();await page.mouse.wheel(0,160);await h.idle()
  const witness=await inspector().evaluate(node=>{const root=node.closest('#session-records'),box=root.getBoundingClientRect(),leaf=[...node.querySelectorAll('[data-event-json-part]>span')].find(leaf=>{const r=leaf.getBoundingClientRect();return r.height&&r.bottom>box.top&&r.top<box.bottom});if(!leaf)throw Error('No actual event token intersects viewport');window.eventToken=leaf;return leaf.textContent})
  const first=Number(await inspector().getAttribute('data-event-json-last'))
  for(let step=0;step<12&&Number(await inspector().getAttribute('data-event-json-last'))<=first;step++){await viewport().press('ArrowDown');await h.idle()}
  assert.ok(Number(await inspector().getAttribute('data-event-json-last'))>first,'real scrolling admits the adjacent token part')
  assert.deepEqual(await page.evaluate(()=>({connected:window.eventToken.isConnected,text:window.eventToken.textContent})),{connected:true,text:witness},'adjacent admission preserves the actual token node')
  await verify()
  const request=await viewport().getAttribute('data-latest-request');await inspector().press('End');await until(async()=>await inspector().evaluate(node=>node.textContent.includes('EVENT END 1999.')));await verify()
  assert.equal(await viewport().getAttribute('data-latest-request'),request,'inner End never invokes the outer latest seek')
  await inspector().press('Home');await until(async()=>await inspector().evaluate(node=>node.textContent.includes('EVENT START 0.')));await verify()
  assert.equal(await viewport().getAttribute('data-latest-request'),request);assert.equal(await conversationPaging(page).count(),0);assert.deepEqual(h.errors,[])
 }finally{await h.close()}
})

test('continuous contract removes conversation paging controls',{timeout:30000},async()=>{
 const h=await openSessionExplorer({transcriptBytes:shortTailTranscript()}),{page}=h
 try{
  await h.start();await page.goto(h.origin+'/workspace/'+h.fixture.workspace.id+'#tab=sessions&session='+h.active.id)
  await page.locator('.session-activity-group').waitFor();await until(async()=>!(await state(page)).loading)
  assert.equal(await conversationPaging(page).count(),0,'the conversation has no user-facing page navigation')
 }finally{await h.close()}
})

for(const nativeBlocks of [false,true])test(`continuous contract preserves the actual historical text position when active evidence seals (${nativeBlocks?'within400 native blocks':'ordinary in-window'})`,{timeout:60000},async()=>{
 const paragraph=i=>`Sealing position ${i}. `+'Readable original context. '.repeat(20)
 const raw=Buffer.from((nativeBlocks?JSON.stringify({type:'assistant',message:{role:'assistant',content:Array.from({length:400},(_,i)=>({type:'text',text:paragraph(i)}))}}):Array.from({length:60},(_,i)=>JSON.stringify({text:paragraph(i)})).join('\n'))+'\n')
 const h=await openSessionExplorer({transcriptBytes:raw}),{page}=h,viewport=()=>page.locator('#session-records')
 try{
  await routeVerifiedArchive({...h,saved:{...h.saved,id:h.active.id}},raw);await h.start();await page.goto(h.origin+'/workspace/'+h.fixture.workspace.id+'#tab=sessions&session='+h.active.id)
  await until(async()=>await viewport().evaluate(node=>node.dataset.historyLoading==='false'&&node.scrollHeight-node.scrollTop-node.clientHeight<=4))
  await h.idle()
  const wheelStart=await viewport().evaluate(node=>({top:node.scrollTop,attrs:{...node.dataset}}))
  await viewport().hover();await page.mouse.wheel(0,-380);await until(()=>page.getByRole('button',{name:'Jump to latest',exact:true}).isVisible()).catch(async error=>{console.log(JSON.stringify({phase:'sealing-manual-wheel',nativeBlocks,before:wheelStart,after:await viewport().evaluate(node=>({top:node.scrollTop,attrs:{...node.dataset},manual:node.closest('.session-viewer').dataset.manualUp}))}));throw error})
  if(nativeBlocks){
   let beginning=false
   for(let step=0;step<12&&!beginning;step++){await viewport().press('Home');await h.idle();beginning=await viewport().evaluate(node=>node.scrollTop<=1&&node.textContent.includes('Sealing position 0.'))}
   assert.equal(beginning,true,'genuine Home reaches the beginning before the historical within-record movement')
   for(let step=0;step<8;step++){await viewport().press('PageDown');await h.idle()}
  }
  await h.idle();await viewport().evaluate(async node=>{let last=node.scrollTop,stable=0;for(let frame=0;frame<90&&stable<4;frame++){await new Promise(resolve=>requestAnimationFrame(resolve));stable=Math.abs(last-node.scrollTop)<0.25?stable+1:0;last=node.scrollTop}if(stable<4)throw Error('Historical input did not settle')})
  const before=await viewport().evaluate(node=>{const box=node.getBoundingClientRect(),leaf=[...node.querySelectorAll('[data-fragment-path]')].find(leaf=>{const r=leaf.getBoundingClientRect();return !leaf.querySelector('[data-history-gap],[data-body-gap],[data-sealed-body-needed]')&&r.height>0&&r.bottom>box.top&&r.top<box.bottom&&leaf.closest('.session-record')});if(!leaf)throw Error('No historical reading text');return {text:leaf.textContent,path:leaf.dataset.fragmentPath,key:leaf.dataset.fragmentPath.split(':').slice(0,2).join(':'),top:leaf.getBoundingClientRect().top-box.top}})
  if(nativeBlocks)assert.ok(Number(before.key.split(':')[1])>0&&Number(before.key.split(':')[1])<399,'the reading position is inside the multi-block record: '+JSON.stringify(before))
  Object.assign(h.active,{state:'terminated',terminated_at:new Date().toISOString(),final_bytes:raw.length,final_sha256:createHash('sha256').update(raw).digest('hex'),revision:h.active.revision+1})
  await page.evaluate(({workspace,session})=>window.pushHierarchyFrames([{schema_version:1,type:'change',event:{schema_version:1,event_id:'seal-position',scope:'workspace',workspace_id:workspace,occurred_at:new Date().toISOString(),transaction:{id:'seal-position',cause:'rest',request_id:null},actor:{type:'system',id:null},entity:{type:'agent_session',id:session,action:'updated'},invalidations:[{kind:'entity',target:'agent_session:'+session},{kind:'collection',target:'sessions:'+workspace}]}}]),{workspace:h.fixture.workspace.id,session:h.active.id})
  await until(async()=>await viewport().evaluate((node,text)=>node.dataset.historyMode==='sealed'&&[...node.querySelectorAll('[data-fragment-path]')].some(leaf=>leaf.textContent===text&&!leaf.querySelector('[data-sealed-body-needed]')),before.text));await h.idle()
  const after=await viewport().evaluate((node,text)=>{const leaf=[...node.querySelectorAll('[data-fragment-path]')].find(leaf=>leaf.textContent===text);return {text:leaf.textContent,top:leaf.getBoundingClientRect().top-node.getBoundingClientRect().top,key:leaf.closest('.session-record').dataset.blockKey,manual:node.closest('.session-viewer').dataset.manualUp}},before.text)
  assert.equal(after.text,before.text);assert.ok(Math.abs(after.top-before.top)<=3,JSON.stringify({before,after}));assert.equal(after.manual,'true');assert.equal(await page.getByRole('button',{name:'Jump to latest',exact:true}).isVisible(),true)
  assert.equal(BigInt(after.key.split(':')[0]),BigInt(before.key.split(':')[0]));assert.equal(Number(after.key.split(':')[1]),Number(before.key.split(':')[1]));assert.equal(await conversationPaging(page).count(),0);assert.deepEqual(h.errors,[])
  await viewport().press('End');await until(async()=>await viewport().evaluate(node=>node.dataset.latestPending==='false'&&node.scrollHeight-node.scrollTop-node.clientHeight<=4&&node.closest('.session-viewer').dataset.manualUp==='false'));assert.equal(await page.getByRole('button',{name:'Jump to latest',exact:true}).isVisible(),false)
 }finally{await h.close()}
})

test('continuous contract fills a collapsed latest tail without input or a latest overlay',{timeout:30000},async()=>{
 const h=await openSessionExplorer({transcriptBytes:shortTailTranscript()}),{page}=h
 try{
  await h.start();await page.goto(h.origin+'/workspace/'+h.fixture.workspace.id+'#tab=sessions&session='+h.active.id)
  await page.locator('.session-activity-group').waitFor();await until(async()=>!(await state(page)).loading)
  const filled=await page.locator('#session-records').evaluate(async node=>{
   const end=performance.now()+2500
   const inspect=()=>{const r=node.getBoundingClientRect(),children=[...node.children].map(child=>child.getBoundingClientRect()).filter(box=>box.height>0);return {before:Number(node.dataset.historyBefore),scanBefore:Number(node.dataset.historyScanBefore),generation:node.dataset.viewerGeneration,loading:node.dataset.historyLoading,after:Number(node.dataset.historyAfter),latest:Number(node.dataset.historyLatest),client:node.clientHeight,content:children.length?Math.max(...children.map(box=>box.bottom))-Math.min(...children.map(box=>box.top)):0,top:node.scrollTop,scroll:node.scrollHeight}}
   let value=inspect();while(value.before>0&&value.content<value.client-4&&performance.now()<end){await new Promise(resolve=>setTimeout(resolve,25));value=inspect()}return value
  })
  assert.ok(filled.before===0||filled.content>=filled.client-4,'short latest content must backfill older records without input: '+JSON.stringify({...filled,reads:h.reads.filter(read=>read.path.endsWith('/transcript')).slice(-8)}))
  assert.ok(filled.after>=filled.latest,'automatic short-view fill preserves logical latest')
  assert.equal(await page.getByRole('button',{name:'Jump to latest',exact:true}).isVisible(),false,'loading/backfill is not manual upward intent')
  assert.deepEqual(h.errors,[]);assert.ok(h.peak()<=2)
 }finally{await h.close()}
})

for(const kind of ['tool','unknown event'])test(`continuous contract preserves opened ${kind} text coordinates and disclosures when active evidence seals`,{timeout:60000},async()=>{
 const values=Array.from({length:1800},(_,i)=>`Activity value ${i}. `+'x'.repeat(80)+' λ😀')
 const payload=kind==='tool'?JSON.stringify({values}):'{"type":"future_json","n":9007199254740993,"negative":-0,"duplicate":1,"duplicate":2,"values":'+JSON.stringify(values)+'}'
 const record=kind==='tool'?JSON.stringify({type:'response_item',payload:{type:'function_call',name:'sealing_activity',arguments:payload}}):'{"type":"event_msg","payload":'+payload+'}'
 const raw=Buffer.from(JSON.stringify({text:'Earlier readable context. '+'Context. '.repeat(300)})+'\n'+record+'\n'),h=await openSessionExplorer({transcriptBytes:raw}),{page}=h,viewport=()=>page.locator('#session-records')
 try{
  await page.setViewportSize({width:1200,height:700});await routeVerifiedArchive({...h,saved:{...h.saved,id:h.active.id}},raw);await h.start();await page.goto(h.origin+'/workspace/'+h.fixture.workspace.id+'#tab=sessions&session='+h.active.id)
  await page.locator('.session-activity-summary').click();await page.locator(kind==='tool'?'.session-content-tool summary':'.session-content-event summary').click();await page.locator('[data-payload-start]').first().waitFor();await h.idle()
  if(kind==='tool'){await page.locator('.session-tool-content').hover();await page.mouse.wheel(0,1000000);await h.idle()}
  else{
   let advanced=false
   for(let step=0;step<80&&!advanced;step++){await viewport().press('PageDown');await h.idle();advanced=await viewport().evaluate(node=>{const box=node.getBoundingClientRect();return [...node.querySelectorAll('[data-payload-start]')].some(span=>Number(span.dataset.payloadStart)>20000&&span.getBoundingClientRect().bottom>box.top&&span.getBoundingClientRect().top<box.bottom)})}
   assert.equal(advanced,true,'genuine scrolling reaches a nonadjacent exact inspector part')
  }
  await viewport().evaluate(async node=>{let last=node.scrollTop,stable=0;for(let frame=0;frame<90&&stable<4;frame++){await new Promise(resolve=>requestAnimationFrame(resolve));stable=Math.abs(last-node.scrollTop)<0.25?stable+1:0;last=node.scrollTop}if(stable<4)throw Error('Opened activity input did not settle')})
  const beforeUp=await viewport().evaluate(node=>({top:node.scrollTop,height:node.scrollHeight,client:node.clientHeight}))
  await viewport().press('ArrowUp');await until(()=>page.getByRole('button',{name:'Jump to latest',exact:true}).isVisible()).catch(async error=>{console.log(JSON.stringify({phase:'activity-sealing-manual',kind,before:beforeUp,after:await viewport().evaluate(node=>({top:node.scrollTop,height:node.scrollHeight,client:node.clientHeight,attrs:{...node.dataset},host:{...node.closest('.session-viewer').dataset},parts:[...node.querySelectorAll('[data-payload-start]')].slice(0,2).map(span=>({...span.dataset,top:span.getBoundingClientRect().top}))}))}));throw error});await h.idle()
  const before=await payloadReadingPoint(viewport())
  assert.ok(before.offset>16384,'the reading point requires a nonzero verified body part: '+JSON.stringify(before))
  const reference=createEventPayloadPager(payload),parts=[];let target
  for(let part=0;part<1000;part++){const body=reference.read(part);parts.push(body.text);if(before.offset<body.start+body.text.length){target={part,local:before.offset-body.start};break}if(!body.more)break}
  assert.ok(target&&target.part>0);assert.equal(parts.join('').slice(before.offset,before.offset+1),before.text)
  Object.assign(h.active,{state:'terminated',terminated_at:new Date().toISOString(),final_bytes:raw.length,final_sha256:createHash('sha256').update(raw).digest('hex'),revision:h.active.revision+1})
  await page.evaluate(({workspace,session})=>window.pushHierarchyFrames([{schema_version:1,type:'change',event:{schema_version:1,event_id:'seal-open-activity',scope:'workspace',workspace_id:workspace,occurred_at:new Date().toISOString(),transaction:{id:'seal-open-activity',cause:'rest',request_id:null},actor:{type:'system',id:null},entity:{type:'agent_session',id:session,action:'updated'},invalidations:[{kind:'entity',target:'agent_session:'+session},{kind:'collection',target:'sessions:'+workspace}]}}]),{workspace:h.fixture.workspace.id,session:h.active.id})
  await until(async()=>await viewport().evaluate((node,part)=>node.dataset.historyMode==='sealed'&&!!node.querySelector(`[data-body-part-index="${part}"]`),target.part));await h.idle()
  const after=await viewport().evaluate((node,target)=>{const part=node.querySelector(`[data-body-part-index="${target.part}"]`),span=[...part.querySelectorAll('[data-payload-start]')].find(span=>Number(span.dataset.payloadStart)<=target.local&&Number(span.dataset.payloadStart)+span.textContent.length>target.local);if(!span)throw Error('Exact target text is not mounted');const range=document.createRange();range.setStart(span.firstChild,target.local-Number(span.dataset.payloadStart));range.setEnd(span.firstChild,Math.min(span.firstChild.length,target.local-Number(span.dataset.payloadStart)+1));const rect=range.getBoundingClientRect(),box=node.getBoundingClientRect(),pre=span.closest('pre').getBoundingClientRect();return {visible:rect.bottom>Math.max(box.top,pre.top)&&rect.top<Math.min(box.bottom,pre.bottom),text:range.toString(),top:rect.top-box.top,piece:span.closest('[data-event-key]')?.open,group:span.closest('[data-group-key]')?.open,parts:node.querySelectorAll('[data-body-part-index]').length,cost:[...node.querySelectorAll('[data-fragment-cost]')].reduce((sum,leaf)=>sum+Number(leaf.dataset.fragmentCost),0),bytes:new TextEncoder().encode(node.innerHTML).length,manual:node.closest('.session-viewer').dataset.manualUp}},target)
  assert.equal(after.text,before.text);assert.ok(Math.abs(after.top-before.top)<=3,JSON.stringify({before,after}));assert.equal(after.visible,true,'the same character remains inside both reader and body scroll clips');assert.equal(after.piece,true);assert.equal(after.group,true);assert.equal(after.manual,'true');assert.ok(after.parts<=3&&after.cost<=1048576&&after.bytes<=1048576);assert.equal(await conversationPaging(page).count(),0);assert.deepEqual(h.errors,[])
 }finally{await h.close()}
})

test('same-authority route retains the actual reading character while an old framer publication is held',{timeout:45000},async()=>{
 const raw=Buffer.from(Array.from({length:420},(_,i)=>JSON.stringify({text:`Pending reading anchor ${i}. `+'Readable context. '.repeat(15)})+'\n').join('')),h=await openSessionExplorer({transcriptBytes:raw}),{page}=h,viewer=()=>page.locator('#session-records')
 try{
  await traceSessionBridge(h)
  await page.addInitScript(()=>{window.sessionBridgeDispatch=(send,value)=>{if(window.holdViewerPublication&&value.kind==='viewer'){window.holdViewerPublication=false;window.heldViewerPublication={stamp:value.stamp,generation:value.generation,publication:value.publication};window.releaseViewerPublication=()=>{window.releaseViewerPublication=null;send(value)};return}send(value)}})
  await h.start();const card=page.locator('#entity-'+h.fixture.tasks[0].id);await until(()=>card.locator('.task-sessions-icon').isEnabled());await card.locator('.task-sessions-icon').click();await until(()=>viewer().evaluate(node=>node.dataset.historyLoading==='false'))
  await page.evaluate(()=>window.holdViewerPublication=true)
  for(let step=0;!await page.evaluate(()=>!!window.heldViewerPublication);step++){assert.ok(step<16,'genuine upward input reaches one finite held acquisition');await continuousScrollStep(page,-1)}
  const point=await page.evaluate(()=>window.sessionPointFacts.filter(fact=>!fact.restore).at(-1)?.point);assert.ok(point)
  const readPoint=()=>viewer().evaluate((node,point)=>{const leaf=[...node.querySelectorAll('[data-fragment-path]')].find(leaf=>leaf.dataset.fragmentPath===point.path);if(!leaf)return null;const walker=document.createTreeWalker(leaf,4);let text=walker.nextNode(),offset=point.offset;while(text&&offset>text.length){offset-=text.length;text=walker.nextNode()}if(!text)return null;const range=document.createRange();range.setStart(text,offset);range.setEnd(text,Math.min(text.length,offset+1));const r=range.getBoundingClientRect(),box=node.getBoundingClientRect();return {char:range.toString(),top:r.top-box.top,visible:r.height>0&&r.bottom>box.top&&r.top<box.bottom,manual:node.closest('.session-viewer').dataset.manualUp}},point)
  const before=await readPoint();assert.equal(before?.visible,true);assert.equal(before.manual,'true');const readsBeforeRoute=h.reads.filter(read=>read.path.endsWith('/transcript')).length
  await card.getByRole('link',{name:'Open full session ↗',exact:true}).click();await page.locator('.session-detail').waitFor();await until(()=>viewer().evaluate(node=>node.dataset.historyLoading==='false'))
  const handoff=await page.evaluate(()=>window.sessionOwnerChanges.at(-1));assert.equal(handoff.continueViewer,false);assert.equal(handoff.preservePresentation,true);assert.notEqual(handoff.reset,null);assert.ok(h.reads.filter(read=>read.path.endsWith('/transcript')).length>readsBeforeRoute,'fresh transport replays the captured window');const after=await readPoint();assert.equal(after?.char,before.char,JSON.stringify({phase:'pending-route',before,after,point,held:await page.evaluate(()=>window.heldViewerPublication),changes:await page.evaluate(()=>window.sessionOwnerChanges)}));assert.equal(after?.visible,true);assert.equal(after.manual,'true');assert.ok(Math.abs(after.top-before.top)<=4)
  await page.evaluate(()=>window.releaseViewerPublication?.());await h.idle();const stale=await readPoint();assert.equal(stale?.char,before.char);assert.equal(stale?.visible,true);assert.ok(Math.abs(stale.top-before.top)<=4,'old owner publication cannot overwrite the restored point');assert.ok(h.peak()<=2);assert.deepEqual(h.errors,[])
 }finally{await page.evaluate(()=>window.releaseViewerPublication?.()).catch(()=>{});await h.close()}
})

for(const distance of [320,1200])test(`continuous contract dock and workspace preserve manual intent and follow new turns without paging (${distance}px upward)`,{timeout:45000},async()=>{
 const h=await openSessionExplorer({transcriptBytes:shortTailTranscript()}),{page}=h,viewport=()=>page.locator('#session-records'),jump=()=>page.getByRole('button',{name:'Jump to latest',exact:true})
 try{
  await traceSessionBridge(h)
  await h.start();const card=page.locator('#entity-'+h.fixture.tasks[0].id);await until(()=>card.locator('.task-sessions-icon').isEnabled());await card.locator('.task-sessions-icon').click()
  await until(async()=>await viewport().evaluate(node=>node.dataset.historyLoading==='false'&&!!node.querySelector('.session-record-text')&&node.scrollHeight-node.scrollTop-node.clientHeight<=4))
  assert.equal(await conversationPaging(page).count(),0);assert.equal(await jump().isVisible(),false)
  await viewport().hover();await page.mouse.wheel(0,-distance);await until(()=>jump().isVisible());await h.idle()
  const selectedPoint=await page.evaluate(()=>window.sessionPointFacts.filter(fact=>!fact.restore).at(-1)?.point);assert.ok(selectedPoint)
  const readPoint=()=>viewport().evaluate((node,point)=>{const leaf=[...node.querySelectorAll('[data-fragment-path]')].find(leaf=>leaf.dataset.fragmentPath===point.path);if(!leaf)throw Error('Selected source fragment is not mounted');const walker=document.createTreeWalker(leaf,4);let text=walker.nextNode(),offset=point.offset;while(text&&offset>text.length){offset-=text.length;text=walker.nextNode()}if(!text)throw Error('Selected source text offset is missing');const range=document.createRange();range.setStart(text,offset);range.setEnd(text,Math.min(text.length,offset+1));const r=range.getBoundingClientRect(),box=node.getBoundingClientRect();return {character:range.toString(),top:r.top-box.top,visible:r.height>0&&r.bottom>box.top&&r.top<box.bottom&&r.right>box.left&&r.left<box.right,scrollTop:node.scrollTop,maximum:node.scrollHeight-node.clientHeight,manual:node.closest('.session-viewer').dataset.manualUp}},selectedPoint)
  const beforePoint=await readPoint();assert.equal(beforePoint.visible,true)
  const oldHandoff=await viewport().evaluate(node=>{const box=node.getBoundingClientRect(),leaf=[...node.querySelectorAll('[data-fragment-path]')].find(leaf=>{const r=leaf.getBoundingClientRect();return !leaf.querySelector('[data-fragment-path],[data-history-gap]')&&r.height&&r.bottom>box.top&&r.top<box.bottom});return {attrs:{...node.dataset},top:node.scrollTop,height:node.clientHeight,scroll:node.scrollHeight,reading:leaf?{path:leaf.dataset.fragmentPath,text:leaf.textContent.slice(0,50),top:leaf.getBoundingClientRect().top-box.top}:null}})
  await card.getByRole('link',{name:'Open full session ↗'}).click();await page.locator('.session-detail').waitFor();await h.idle()
  const afterPoint=await readPoint(),wanted=afterPoint.scrollTop+afterPoint.top-beforePoint.top,clamped=wanted>afterPoint.maximum+3&&Math.abs(afterPoint.scrollTop-afterPoint.maximum)<=3||wanted< -3&&afterPoint.scrollTop<=3
  assert.equal(afterPoint.character,beforePoint.character);assert.equal(afterPoint.visible,true);assert.equal(afterPoint.manual,'true');assert.ok(Math.abs(afterPoint.top-beforePoint.top)<=3||clamped,JSON.stringify({beforePoint,afterPoint,clamped,selectedPoint}));if(distance===1200)assert.equal(clamped,false,'the genuinely historical witness has room to preserve its exact character pixel')
  assert.equal(await jump().isVisible(),afterPoint.scrollTop<afterPoint.maximum-4,'only a handoff that actually still trails latest needs the overlay');assert.equal(await conversationPaging(page).count(),0)
  await viewport().press('End');await until(async()=>await viewport().evaluate(node=>node.dataset.latestPending==='false'&&node.scrollHeight-node.scrollTop-node.clientHeight<=4));await jump().waitFor({state:'hidden'})
  await h.appendRecords({text:'Newest turn after a continuous dock handoff'});await page.getByText('Newest turn after a continuous dock handoff',{exact:true}).waitFor();assert.equal(await jump().isVisible(),false);assert.deepEqual(h.errors,[])
 }finally{await h.close()}
})

test('continuous contract preserves opened thinking code and its inner scroll when active evidence seals',{timeout:45000},async()=>{
 const code=Array.from({length:400},(_,i)=>`Reasoning code line ${i}. `+'literal λ😀 '.repeat(8)).join('\n')+'\n',raw=Buffer.from(JSON.stringify({type:'response_item',payload:{type:'reasoning',summary:[{type:'summary_text',text:'```text\n'+code+'```'}]}})+'\n'),h=await openSessionExplorer({transcriptBytes:raw}),{page}=h,viewport=()=>page.locator('#session-records')
 try{
  await page.setViewportSize({width:1200,height:700});await routeVerifiedArchive({...h,saved:{...h.saved,id:h.active.id}},raw);await h.start();await page.goto(h.origin+'/workspace/'+h.fixture.workspace.id+'#tab=sessions&session='+h.active.id)
  await page.locator('.session-activity-summary').click();await page.locator('.session-content-thinking summary').click();await page.locator('.session-record-text pre').waitFor();await page.locator('.session-record-text pre').hover();await page.mouse.wheel(0,1000000);await h.idle()
  await viewport().press('ArrowUp');await page.getByRole('button',{name:'Jump to latest',exact:true}).waitFor();await h.idle()
  const before=await viewport().evaluate(node=>{const pre=node.querySelector('.session-record-text pre'),box=node.getBoundingClientRect(),clip=pre.getBoundingClientRect(),walker=document.createTreeWalker(pre,4);let text,total=0;while(text=walker.nextNode()){let first=0,last=text.length;const range=document.createRange();while(first<last){const mid=Math.floor((first+last)/2);range.setStart(text,mid);range.setEnd(text,Math.min(text.length,mid+1));if(range.getBoundingClientRect().bottom>Math.max(box.top,clip.top)+1)last=mid;else first=mid+1}if(first<text.length){range.setStart(text,first);range.setEnd(text,first+1);const r=range.getBoundingClientRect();if(r.top<Math.min(box.bottom,clip.bottom))return {offset:total+first,text:range.toString(),top:r.top-box.top,scroll:pre.scrollTop}}total+=text.length}throw Error('No thinking code character is visibly readable')})
  assert.ok(before.scroll>0)
  Object.assign(h.active,{state:'terminated',terminated_at:new Date().toISOString(),final_bytes:raw.length,final_sha256:createHash('sha256').update(raw).digest('hex'),revision:h.active.revision+1})
  await page.evaluate(({workspace,session})=>window.pushHierarchyFrames([{schema_version:1,type:'change',event:{schema_version:1,event_id:'seal-thinking',scope:'workspace',workspace_id:workspace,occurred_at:new Date().toISOString(),transaction:{id:'seal-thinking',cause:'rest',request_id:null},actor:{type:'system',id:null},entity:{type:'agent_session',id:session,action:'updated'},invalidations:[{kind:'entity',target:'agent_session:'+session},{kind:'collection',target:'sessions:'+workspace}]}}]),{workspace:h.fixture.workspace.id,session:h.active.id})
  await until(async()=>await viewport().evaluate(node=>node.dataset.historyMode==='sealed'&&!!node.querySelector('.session-record-text pre')));await h.idle()
  const after=await viewport().evaluate((node,offset)=>{const pre=node.querySelector('.session-record-text pre'),walker=document.createTreeWalker(pre,4),box=node.getBoundingClientRect();let text;while(text=walker.nextNode()){if(offset<text.length){const range=document.createRange();range.setStart(text,offset);range.setEnd(text,offset+1);return {text:range.toString(),top:range.getBoundingClientRect().top-box.top,scroll:pre.scrollTop,body:pre.textContent,piece:pre.closest('[data-event-key]').open,group:pre.closest('[data-group-key]').open}}offset-=text.length}throw Error('Thinking code coordinate missing')},before.offset)
  assert.equal(after.body,code);assert.equal(after.text,before.text);assert.ok(Math.abs(after.top-before.top)<=3,JSON.stringify({before,after:{...after,body:undefined}}));assert.equal(after.scroll,before.scroll);assert.equal(after.piece,true);assert.equal(after.group,true);assert.deepEqual(h.errors,[])
 }finally{await h.close()}
})

for(const sealed of [false,true])test(`continuous contract ${sealed?'sealed':'active'} initial acquisition failure keeps ordinary Retry without manual Jump`,{timeout:45000},async()=>{
 const raw=Buffer.from(JSON.stringify({text:'Newest retry proof.'})+'\n'),h=await openSessionExplorer({transcriptBytes:raw}),{page}=h;let failing=true
 try{
  if(sealed){await routeVerifiedArchive(h,raw);await page.route('**/sessions/**/archive/**/content',route=>failing?route.fulfill({status:400,json:{error:'controlled_initial_failure'}}):route.fallback())}
  else await page.route('**/sessions/**/transcript**',route=>failing?route.fulfill({status:400,json:{error:'controlled_initial_failure'}}):route.fallback())
  await h.start();await page.goto(h.origin+'/workspace/'+h.fixture.workspace.id+'#tab=sessions&session='+(sealed?h.saved.id:h.active.id))
  await page.locator('#session-records').waitFor();await page.locator('.session-viewer').getByRole('button',{name:'Retry',exact:true}).first().waitFor()
  assert.equal(await page.getByRole('button',{name:'Jump to latest',exact:true}).isVisible(),false,'errors cannot invent upward scroll intent')
  if(!sealed){await page.locator('#session-records').press('End');await page.locator('.session-latest-error').waitFor();assert.equal(await page.getByRole('button',{name:'Jump to latest',exact:true}).isVisible(),false)}
  failing=false;const retry=!sealed?page.locator('.session-latest-error').getByRole('button',{name:'Retry',exact:true}):page.locator('.session-viewer').getByRole('button',{name:'Retry',exact:true}).first();await retry.click()
  await page.getByText('Newest retry proof.',{exact:true}).waitFor();assert.equal(await page.getByRole('button',{name:'Jump to latest',exact:true}).isVisible(),false);assert.equal(await conversationPaging(page).count(),0)
 }finally{await h.close()}
})

test('continuous contract opens a newly selected verified archive at latest',{timeout:45000},async()=>{
 const raw=Buffer.from(Array.from({length:180},(_,i)=>JSON.stringify({text:`Continuous archive occurrence ${i}. `+'Readable history. '.repeat(25)})+'\n').join(''))
 const h=await openSessionExplorer({transcriptBytes:raw}),{page}=h
 try{
  const hash=bytes=>createHash('sha256').update(bytes).digest('hex'),gzip=gzipSync(raw),generation='33333333-3333-4333-8333-333333333333',etag='"continuous-verified-archive"'
  const manifest={workspace_id:h.fixture.workspace.id,session_id:h.saved.id,state:'ready',generation_id:generation,codec:'gzip-v1',raw_bytes:String(raw.length),raw_records:'180',raw_sha256:hash(raw),compressed_bytes:String(gzip.length),compressed_sha256:hash(gzip),content_path:`/api/v1/workspaces/${h.fixture.workspace.id}/sessions/${h.saved.id}/archive/${generation}/content`}
  await page.route('**/sessions/**/archive**',route=>new URL(route.request().url()).pathname.endsWith('/content')?route.fulfill(route.request().headers()['if-none-match']?{status:304,headers:{ETag:etag}}:{headers:{'Content-Type':'application/gzip',ETag:etag,'Content-Length':String(gzip.length),'X-HMem-Archive-Generation':generation,'X-HMem-Archive-SHA256':hash(gzip),'X-HMem-Archive-Raw-Bytes':String(raw.length),'X-HMem-Archive-Raw-SHA256':hash(raw)},body:gzip}):route.fulfill({json:manifest}))
  await h.start();await page.goto(h.origin+'/workspace/'+h.fixture.workspace.id+'#tab=sessions&session='+h.saved.id)
  await page.getByText('Verified complete archive.',{exact:true}).waitFor();await page.locator('.session-record').first().waitFor()
  const latest=await page.locator('#session-records').evaluate(node=>({canNext:node.dataset.archiveCanNext,more:node.dataset.renderMore,hasLatest:node.textContent.includes('Continuous archive occurrence 179.'),rows:node.querySelectorAll('.session-record').length,first:node.querySelector('.session-record')?.textContent.slice(0,80),last:[...node.querySelectorAll('.session-record')].at(-1)?.textContent.slice(0,80),top:node.scrollTop,height:node.scrollHeight,client:node.clientHeight}))
  assert.ok(latest.hasLatest,'new verified selection starts at the final source occurrence: '+JSON.stringify(latest))
  assert.equal(latest.canNext,'false');assert.equal(latest.more,'false')
  assert.ok(latest.height-latest.top-latest.client<=4)
  assert.equal(await conversationPaging(page).count(),0)
  assert.equal(await page.getByRole('button',{name:'Jump to latest',exact:true}).isVisible(),false)
  for(const size of [{width:430,height:1100},{width:1440,height:1000}]){
   await page.setViewportSize(size)
   await until(async()=>await page.locator('#session-records').evaluate(node=>{const last=[...node.querySelectorAll('.session-record')].at(-1),box=node.getBoundingClientRect(),tail=last?.getBoundingClientRect();return node.dataset.following==='true'&&node.dataset.logicalLatest==='true'&&node.scrollHeight-node.scrollTop-node.clientHeight<=4&&tail?.bottom>box.top&&tail.top<box.bottom}))
   assert.equal(await page.getByRole('button',{name:'Jump to latest',exact:true}).isVisible(),false,'sealed latest resize follows the actual tail without manufacturing manual intent')
  }
  assert.deepEqual(h.errors,[])
 }finally{await h.close()}
})

test('continuous contract reconciles a backward native pair through real framing and wire publication',{timeout:45000},async()=>{
 const line=value=>Buffer.from(JSON.stringify(value)+'\n'),text='A canonical turn with one genuine identical following turn.',time='2026-10-24T10:00:00.000Z'
 const prefix=line({text:'Earlier readable context. '+'Reading. '.repeat(18000)})
 const canonical=line({type:'response_item',timestamp:time,payload:{type:'message',role:'assistant',content:[{type:'output_text',text}]}})
 const envelope=line({type:'event_msg',timestamp:time,payload:{type:'agent_message',message:text}}),genuine=line({type:'event_msg',timestamp:'2026-10-24T10:00:00.050Z',payload:{type:'agent_message',message:text}})
 const activity=value=>line({type:'response_item',payload:{type:'function_call',name:'inspect',arguments:JSON.stringify({value})}})
 const tailSize=65535-envelope.length-genuine.length,tail=activity('x'.repeat(tailSize-activity('').length))
 assert.equal(tail.length,tailSize)
 const canonicalOffset=prefix.length,envelopeOffset=prefix.length+canonical.length,genuineOffset=envelopeOffset+envelope.length,bytes=Buffer.concat([prefix,canonical,envelope,genuine,tail])
 const h=await openSessionExplorer({transcriptBytes:bytes}),{page}=h
 try{
  await page.addInitScript(()=>{window.continuousSeamFrames=[];new MutationObserver(()=>{const node=document.getElementById('session-records');if(!node)return;const offsets=[...node.querySelectorAll('.session-record')].map(row=>Number(row.dataset.offset));if(!offsets.length)return;const last=window.continuousSeamFrames.at(-1);if(!last||JSON.stringify(last)!==JSON.stringify(offsets)){window.continuousSeamFrames.push(offsets);if(window.continuousSeamFrames.length>12)window.continuousSeamFrames.shift()}}).observe(document,{subtree:true,childList:true})})
  await h.start();await page.goto(h.origin+'/workspace/'+h.fixture.workspace.id+'#tab=sessions&session='+h.active.id)
  await until(async()=>{const value=await state(page);return !value.loading&&value.before===0})
  const receipt=await page.locator('#session-records').evaluate((node,text)=>({offsets:[...node.querySelectorAll('.session-record')].map(row=>Number(row.dataset.offset)),occurrences:[...node.querySelectorAll('.session-record')].filter(row=>row.textContent.includes(text)).map(row=>Number(row.dataset.offset)),frames:window.continuousSeamFrames}),text)
  assert.ok(receipt.frames.some(frame=>frame.includes(envelopeOffset)&&!frame.includes(canonicalOffset)),'the first live frame starts at the previously unpaired event envelope')
  assert.deepEqual(receipt.occurrences,[canonicalOffset,genuineOffset],'authoritative overlap retires only the paired envelope, preserving the real identical turn')
  assert.equal(receipt.offsets.includes(envelopeOffset),false)
  assert.equal(await page.getByRole('button',{name:'Jump to latest',exact:true}).isVisible(),false)
  assert.deepEqual(h.errors,[])
 }finally{await h.close()}
})

test('continuous contract prepends through real input while retaining the visible DOM anchor and follow intent',{timeout:60000},async()=>{
 const bytes=Buffer.from(Array.from({length:120},(_,i)=>JSON.stringify({text:`Continuous occurrence ${i}. `+'Readable context. '.repeat(100)})+'\n').join(''))
 const h=await openSessionExplorer({transcriptBytes:bytes}),{page}=h;let hold=false,held=false,release
 const viewport=()=>page.locator('#session-records'),jump=()=>page.getByRole('button',{name:'Jump to latest',exact:true})
 const atEnd=()=>viewport().evaluate(node=>node.dataset.historyLoading==='false'&&Number(node.dataset.historyAfter)>=Number(node.dataset.historyLatest)&&node.scrollHeight-node.scrollTop-node.clientHeight<=4)
 try{
  await page.route('**/sessions/**/transcript**',async route=>{if(hold&&Number(new URL(route.request().url()).searchParams.get('offset'))<bytes.length-65536){held=true;await new Promise(resolve=>{release=resolve})}return route.fallback()})
  await h.start();await page.goto(h.origin+'/workspace/'+h.fixture.workspace.id+'#tab=sessions&session='+h.active.id);await until(atEnd)
  assert.equal(await jump().isVisible(),false)
  await page.setViewportSize({width:1000,height:820});await h.idle();assert.equal(await jump().isVisible(),false,'resize does not arm manual history')
  await viewport().evaluate(node=>{node.scrollTop-=70});await h.idle();assert.equal(await jump().isVisible(),false,'programmatic movement does not arm manual history')
  hold=true;await viewport().focus();await page.keyboard.press('Home');await until(async()=>held).catch(async error=>{console.log(JSON.stringify({phase:'active-anchor-older-demand',state:await viewport().evaluate(node=>({attrs:{...node.dataset},top:node.scrollTop,height:node.scrollHeight,client:node.clientHeight,host:{...node.closest('.session-viewer').dataset},focus:document.activeElement?.id,first:[...node.querySelectorAll('.session-record')].slice(0,2).map(row=>({offset:row.dataset.offset,top:row.getBoundingClientRect().top})),gaps:[...node.querySelectorAll('[data-fragment-from]')].slice(0,3).map(gap=>({...gap.dataset,top:gap.getBoundingClientRect().top,height:gap.getBoundingClientRect().height}))})),reads:h.reads.filter(read=>read.path.endsWith('/transcript')).slice(-4)}));throw error})
  await viewport().evaluate(async node=>{let last=node.scrollTop,stable=0;for(let frame=0;frame<90&&stable<4;frame++){await new Promise(resolve=>requestAnimationFrame(resolve));stable=Math.abs(last-node.scrollTop)<0.25?stable+1:0;last=node.scrollTop}if(stable<4)throw Error('Held active Home did not settle before its reading witness')})
  const before=await viewport().evaluate(node=>{const box=node.getBoundingClientRect(),anchor=[...node.querySelectorAll('.session-record')].find(row=>row.getBoundingClientRect().bottom>box.top);window.continuousAnchorNode=anchor;return {offset:Number(anchor.dataset.offset),top:anchor.getBoundingClientRect().top-box.top,min:Number(node.dataset.historyBefore)}})
  hold=false;release();await until(async()=>{const value=await state(page);return !value.loading&&value.before<before.min})
  const after=await viewport().evaluate((node,offset)=>{const anchor=[...node.querySelectorAll('.session-record')].find(row=>Number(row.dataset.offset)===offset),box=node.getBoundingClientRect();return {same:anchor===window.continuousAnchorNode&&anchor.isConnected,top:anchor?.getBoundingClientRect().top-box.top}},before.offset)
  assert.equal(after.same,true,'prepending retains the unaffected keyed DOM occurrence');assert.ok(Math.abs(after.top-before.top)<3,JSON.stringify({before,after}))
  assert.equal(await jump().isVisible(),true,'actual upward movement arms the latest overlay')
  await h.appendRecords({text:'Appended while reading continuous history'});await h.idle()
  const anchored=await viewport().evaluate((node,offset)=>{const anchor=[...node.querySelectorAll('.session-record')].find(row=>Number(row.dataset.offset)===offset);return anchor.getBoundingClientRect().top-node.getBoundingClientRect().top},before.offset)
  assert.ok(Math.abs(anchored-after.top)<3,'an append cannot steal the historical reading anchor')
  await jump().focus();await page.keyboard.press('Enter');await until(atEnd);await jump().waitFor({state:'hidden'})
  await h.appendRecords({text:'Automatic follow after continuous return'});await page.getByText('Automatic follow after continuous return',{exact:true}).waitFor();await until(atEnd)
  assert.equal(await jump().isVisible(),false);assert.equal(await conversationPaging(page).count(),0);assert.deepEqual(h.errors,[]);assert.ok(h.peak()<=2)
 }finally{release?.();await h.close()}
})

test('continuous contract backfills and refetches a collapsed run beyond framer and composed retention',{timeout:60000},async()=>{
 const line=value=>JSON.stringify(value)+'\n'
 const prefix=line({text:'Before the long activity run. '+'Visible readable context. '.repeat(120)})
 const tools=Array.from({length:900},(_,i)=>line({type:'response_item',payload:{type:'function_call',name:`bounded_tool_${i}`,call_id:`bounded-${i}`,arguments:JSON.stringify({value:'x'.repeat(180)})}}))
 const bytes=Buffer.from(prefix+tools.join('')),firstOffset=Buffer.byteLength(prefix),lastOffset=bytes.length-Buffer.byteLength(tools.at(-1))
 const h=await openSessionExplorer({transcriptBytes:bytes}),{page}=h,viewport=()=>page.locator('#session-records')
 try{
  await h.start();await page.goto(h.origin+'/workspace/'+h.fixture.workspace.id+'#tab=sessions&session='+h.active.id)
  await until(async()=>await viewport().evaluate(node=>node.dataset.historyLoading==='false'&&Number(node.dataset.historyScanBefore)===0))
  const collapsed=await viewport().evaluate(node=>({rows:node.querySelectorAll('.session-record').length,summary:node.querySelector('.session-activity-summary')?.textContent,gaps:node.querySelectorAll('[data-history-gap="source"]').length,prose:node.textContent.includes('Before the long activity run.'),manual:node.closest('.session-viewer').dataset.manualUp}))
  assert.ok(collapsed.prose,'automatic scan traverses the collapsed run to older readable prose: '+JSON.stringify(collapsed))
  assert.ok(collapsed.rows<=200&&collapsed.gaps<=201,'payload and omitted-span metadata stay bounded')
  assert.match(collapsed.summary,/900 events/,'run count includes discarded and compacted occurrences exactly once')
  assert.equal(await page.getByRole('button',{name:'Jump to latest',exact:true}).isVisible(),false)
  await page.locator('.session-activity-summary').click();await viewport().focus();await page.keyboard.press('Home')
  await until(async()=>await viewport().evaluate((node,offset)=>node.dataset.historyLoading==='false'&&[...node.querySelectorAll('.session-record')].some(row=>Number(row.dataset.offset)===offset),firstOffset))
  assert.ok(await viewport().evaluate(node=>node.querySelectorAll('.session-record').length<=200))
  const middleOffset=Buffer.byteLength(prefix+tools.slice(0,450).join('')),readsBefore=h.reads.filter(read=>read.path.endsWith('/transcript')).length
  assert.equal(await viewport().evaluate((node,offset)=>[...node.querySelectorAll('.session-record')].some(row=>Number(row.dataset.offset)===offset),middleOffset),false,'the target is genuinely omitted, rather than already cached')
  let reached=false,stalled=0,previous='',trace=[]
  for(let step=0;step<160&&!reached;step++){
   await viewport().press('PageDown');await h.idle()
   const progress=await viewport().evaluate((node,offset)=>({reached:[...node.querySelectorAll('.session-record')].some(row=>Number(row.dataset.offset)===offset),rows:node.querySelectorAll('.session-record').length,key:[node.dataset.viewerGeneration,node.dataset.renderPart,node.scrollTop,node.dataset.historyBefore,node.dataset.historyAfter].join(':'),open:[...node.querySelectorAll('[data-group-key]')].map(group=>({key:group.dataset.groupKey,open:group.open})),gaps:[...node.querySelectorAll('[data-history-gap="source"]')].slice(0,3).map(gap=>({...gap.dataset,top:gap.getBoundingClientRect().top,height:gap.getBoundingClientRect().height})),text:node.textContent.slice(-100)}),middleOffset)
   assert.ok(progress.rows<=200);reached=progress.reached;stalled=progress.key===previous?stalled+1:0;previous=progress.key
   trace=[...trace.slice(-11),{step,...progress}]
   assert.ok(stalled<6,'real continuous scrolling must keep making viewport/source progress: '+JSON.stringify(trace))
  }
  assert.equal(reached,true,'real PageDown traversal reaches the genuinely omitted middle occurrence: '+JSON.stringify({trace,reads:h.reads.filter(read=>read.path.endsWith('/transcript')).slice(-5)}))
  assert.ok(h.reads.filter(read=>read.path.endsWith('/transcript')).length>readsBefore,'entering the omitted range rehydrates its payload')
  await viewport().press('End')
  await until(async()=>await viewport().evaluate((node,offset)=>node.dataset.historyLoading==='false'&&[...node.querySelectorAll('.session-record')].some(row=>Number(row.dataset.offset)===offset),lastOffset)).catch(async error=>{console.log(JSON.stringify({phase:'long-run-latest',lastOffset,state:await viewport().evaluate(node=>({attrs:{...node.dataset},top:node.scrollTop,height:node.scrollHeight,manual:node.closest('.session-viewer').dataset.manualUp,offsets:[...node.querySelectorAll('.session-record')].map(row=>Number(row.dataset.offset)).filter((_,i,all)=>i<3||i>all.length-4),gaps:[...node.querySelectorAll('[data-history-gap="source"]')].slice(0,5).map(gap=>({...gap.dataset,top:gap.getBoundingClientRect().top,height:gap.getBoundingClientRect().height}))})),reads:h.reads.filter(read=>read.path.endsWith('/transcript')).slice(-5)}));throw error})
  await until(async()=>await viewport().evaluate(node=>node.scrollHeight-node.scrollTop-node.clientHeight<=4&&node.closest('.session-viewer').dataset.manualUp==='false'))
  await h.appendRecords({text:'Following after a continuous virtual End'});await page.getByText('Following after a continuous virtual End',{exact:true}).waitFor()
  assert.ok(h.reads.filter(read=>read.path.endsWith('/transcript')).length<40,'one scroll cannot create an unbounded acquisition loop')
  assert.equal(await conversationPaging(page).count(),0);assert.deepEqual(h.errors,[]);assert.ok(h.peak()<=2)
 }finally{await h.close()}
})

for(const [sealed,nativeBlocks] of [[false,false],[true,false],[false,true]])test(`continuous contract retains an actual Markdown fragment across ${sealed?'sealed':'active'} presentation slices${nativeBlocks?' in400 native blocks':''}`,{timeout:60000},async()=>{
 const count=nativeBlocks?400:500,paragraph=i=>`Paragraph ${i}. `+'Readable **formatted** context. '.repeat(35),prose=Array.from({length:count},(_,i)=>paragraph(i)).join('\n\n')
 const value=nativeBlocks?{type:'assistant',message:{role:'assistant',content:Array.from({length:count},(_,i)=>({type:'text',text:paragraph(i)}))}}:{text:prose}
 const raw=Buffer.from(JSON.stringify(value)+'\n'),h=await openSessionExplorer({transcriptBytes:raw}),{page}=h,viewport=()=>page.locator('#session-records')
 try{
  if(sealed)await routeVerifiedArchive(h,raw);await h.start();await page.goto(h.origin+'/workspace/'+h.fixture.workspace.id+'#tab=sessions&session='+(sealed?h.saved.id:h.active.id))
  await until(async()=>await viewport().evaluate((node,last)=>node.textContent.includes(`Paragraph ${last}.`)&&node.querySelectorAll('[data-fragment-from]').length>0,count-1)).catch(async error=>{console.log(JSON.stringify({phase:'sealed-text-initial',state:await viewport().evaluate(node=>({attrs:{...node.dataset},text:node.textContent.slice(-140),gaps:node.querySelectorAll('[data-fragment-from]').length,needed:[...node.querySelectorAll('[data-sealed-body-needed]')].map(node=>({...node.dataset})),status:node.parentElement.querySelector('[role="status"]')?.textContent})),errors:h.errors}));throw error})
  assert.equal(await conversationPaging(page).count(),0)
  assert.equal(await page.getByRole('button',{name:'Jump to latest',exact:true}).isVisible(),false)
  await until(async()=>await viewport().evaluate(node=>node.scrollHeight-node.scrollTop-node.clientHeight<=4))
  const before=await viewport().evaluate(node=>{const box=node.getBoundingClientRect(),leaf=[...node.querySelectorAll('[data-fragment-path]')].find(leaf=>leaf.getBoundingClientRect().height>0);node.scrollTop+=leaf.getBoundingClientRect().top-box.top-100;window.continuousTextLeaf=leaf;return {part:Number(node.dataset.renderPart),text:leaf.textContent,top:leaf.getBoundingClientRect().top-box.top}})
  assert.ok(before.text,JSON.stringify(before))
  await viewport().press('ArrowUp')
  await until(async()=>Number(await viewport().getAttribute('data-render-part'))<before.part).catch(async error=>{console.log(JSON.stringify({phase:'sealed-fragment-seam',before,state:await viewport().evaluate(node=>({attrs:{...node.dataset},top:node.scrollTop,manual:node.closest('.session-viewer').dataset.manualUp,leaf:window.continuousTextLeaf?.isConnected,gaps:[...node.querySelectorAll('[data-fragment-from]')].map(gap=>({...gap.dataset,top:gap.getBoundingClientRect().top,height:gap.getBoundingClientRect().height})).slice(-5)})),errors:h.errors}));throw error})
  const after=await viewport().evaluate(node=>({same:window.continuousTextLeaf.isConnected,text:window.continuousTextLeaf.textContent,top:window.continuousTextLeaf.getBoundingClientRect().top-node.getBoundingClientRect().top,cost:[...node.querySelectorAll('[data-fragment-cost]')].reduce((sum,leaf)=>sum+Number(leaf.dataset.fragmentCost),0)}))
  assert.equal(after.same,true,'the actual text leaf survives the slice overlap');assert.equal(after.text,before.text);assert.ok(Math.abs(after.top-before.top-40)<=3,'only the native ArrowUp movement changes the visible text position: '+JSON.stringify({before,after}));assert.ok(after.cost<=1048576)
  await viewport().press('Home');await until(async()=>await viewport().evaluate(node=>node.textContent.includes('Paragraph 0.')))
  await viewport().press('End');await until(async()=>await viewport().evaluate((node,last)=>node.textContent.includes(`Paragraph ${last}.`),count-1)).catch(async error=>{console.log(JSON.stringify({phase:'text-End',sealed,state:await viewport().evaluate(node=>({attrs:{...node.dataset},tail:node.scrollHeight-node.scrollTop-node.clientHeight,manual:node.closest('.session-viewer').dataset.manualUp,text:node.textContent.slice(-100),statuses:[...node.parentElement.querySelectorAll('[role="status"],[role="alert"]')].map(node=>node.textContent.slice(0,100))})),errors:h.errors,reads:h.reads.filter(read=>read.path.endsWith('/transcript')).slice(-3)}));throw error})
  assert.equal(await conversationPaging(page).count(),0);assert.deepEqual(h.errors,[])
 }finally{await h.close()}
})

test('continuous contract sealed short collapsed run fills automatically across the verified index',{timeout:45000},async()=>{
 const raw=Buffer.from(JSON.stringify({text:'Older verified readable context. '+'Reading. '.repeat(300)})+'\n'+Array.from({length:900},(_,i)=>JSON.stringify({type:'response_item',payload:{type:'function_call',name:`archived_${i}`,arguments:'{}'}})+'\n').join(''))
 const h=await openSessionExplorer({transcriptBytes:raw}),{page}=h
 try{
  await routeVerifiedArchive(h,raw);await h.start();await page.goto(h.origin+'/workspace/'+h.fixture.workspace.id+'#tab=sessions&session='+h.saved.id)
  await page.getByText('Verified complete archive.',{exact:true}).waitFor()
  await until(async()=>await page.locator('#session-records').evaluate(node=>node.textContent.includes('Older verified readable context.')))
  assert.match(await page.locator('.session-activity-summary').textContent(),/900 events/)
  assert.equal(await page.getByRole('button',{name:'Jump to latest',exact:true}).isVisible(),false)
  assert.ok(await page.locator('#session-records').evaluate(node=>node.querySelectorAll('.session-record').length<=200&&node.querySelectorAll('[data-history-gap="source"]').length<=201))
  assert.equal(await conversationPaging(page).count(),0);assert.deepEqual(h.errors,[])
 }finally{await h.close()}
})

for(const layout of ['records','native blocks'])test(`continuous contract sealed wheel and indexed prepend retain the visible text and reach all history (${layout})`,{timeout:60000},async()=>{
 const count=layout==='records'?500:400,text=i=>`Verified indexed occurrence ${i}. `+'Readable context. '.repeat(35)
 const raw=Buffer.from(layout==='records'?Array.from({length:count},(_,i)=>JSON.stringify({text:text(i)})+'\n').join(''):JSON.stringify({type:'assistant',message:{role:'assistant',content:Array.from({length:count},(_,i)=>({type:'text',text:text(i)}))}})+'\n')
 const h=await openSessionExplorer({transcriptBytes:raw}),{page}=h,viewport=()=>page.locator('#session-records')
 try{
  await page.addInitScript(()=>{const original=Worker.prototype.postMessage;window.heldArchivePages=[];window.archivePageCount=0;Worker.prototype.postMessage=function(message,...rest){if(message.kind==='page'){window.archivePageCount++;if(window.holdArchivePages){window.heldArchivePages.push(()=>original.call(this,message,...rest));return}}return original.call(this,message,...rest)}})
  await page.addInitScript(()=>{window.readerWheelProof=[];for(const type of ['wheel','scroll'])document.addEventListener(type,event=>{const node=document.getElementById('session-records');if(!node)return;const entry={type,target:event.target?.id||event.target?.tagName,scoped:!!event.target?.closest?.('#session-records'),trusted:event.isTrusted,top:node.scrollTop,delta:event.deltaY};window.readerWheelProof.push(entry);window.readerWheelProof=window.readerWheelProof.slice(-8);requestAnimationFrame(()=>{entry.after=node.scrollTop})},true)})
  await page.addInitScript(()=>{window.indexedTargetProof=[];let previous='';new MutationObserver(()=>{const node=document.getElementById('session-records');if(!node)return;const key=node.dataset.sealedPageRequest+':'+node.dataset.renderPart;if(key===previous)return;previous=key;const rows=[...node.querySelectorAll('.session-record')],target=rows.find(row=>row.textContent.includes('Verified indexed occurrence 250.'));window.indexedTargetProof.push({key,first:rows[0]?.textContent.match(/occurrence (\d+)/)?.[1],last:rows.at(-1)?.textContent.match(/occurrence (\d+)/)?.[1],target:!!target,top:node.scrollTop});window.indexedTargetProof=window.indexedTargetProof.slice(-30)}).observe(document,{subtree:true,childList:true,attributes:true,attributeFilter:['data-render-part','data-sealed-page-request']})})
  await routeVerifiedArchive(h,raw);await h.start();await page.goto(h.origin+'/workspace/'+h.fixture.workspace.id+'#tab=sessions&session='+h.saved.id)
  await until(async()=>await viewport().evaluate((node,last)=>node.textContent.includes(`Verified indexed occurrence ${last}.`)&&node.scrollHeight-node.scrollTop-node.clientHeight<=4,count-1))
  await page.setViewportSize({width:1200,height:1000});await h.idle();assert.equal(await page.getByRole('button',{name:'Jump to latest',exact:true}).isVisible(),false,'sealed resize does not arm manual history')
  await viewport().evaluate(node=>{node.scrollTop-=70});await h.idle();assert.equal(await page.getByRole('button',{name:'Jump to latest',exact:true}).isVisible(),false,'sealed programmatic positioning does not arm manual history')
  await viewport().hover();await page.mouse.wheel(0,-320);await until(async()=>await page.getByRole('button',{name:'Jump to latest',exact:true}).isVisible()).catch(async error=>{console.log(JSON.stringify({phase:'sealed-real-wheel',proof:await page.evaluate(()=>window.readerWheelProof),state:await viewport().evaluate(node=>({attrs:{...node.dataset},top:node.scrollTop,height:node.scrollHeight,client:node.clientHeight,manual:node.closest('.session-viewer').dataset.manualUp,bounds:node.getBoundingClientRect().toJSON()}))}));throw error})
  assert.equal(await viewport().evaluate(node=>node.closest('.session-viewer').dataset.manualUp),'true','settled scoped wheel upward arms history')
  await page.evaluate(()=>{window.holdArchivePages=true})
  for(let step=0;step<80&&!(await page.evaluate(()=>window.heldArchivePages.length>0));step++){await viewport().press('PageUp');await h.idle()}
  assert.ok(await page.evaluate(()=>window.heldArchivePages.length>0),'real upward continuation demands the prior indexed range')
  await viewport().evaluate(async node=>{let last=node.scrollTop,stable=0;for(let frame=0;frame<90&&stable<4;frame++){await new Promise(resolve=>requestAnimationFrame(resolve));const top=node.scrollTop;stable=Math.abs(top-last)<0.25?stable+1:0;last=top}if(stable<4)throw Error('Native PageUp failed to settle before held publication')})
  const before=await viewport().evaluate(node=>{const box=node.getBoundingClientRect(),leaf=[...node.querySelectorAll('[data-fragment-path]')].find(leaf=>{const r=leaf.getBoundingClientRect();return r.height>0&&r.bottom>box.top&&r.top<box.bottom});if(!leaf)throw Error('Held prepend must have an actual visible text leaf');window.indexedTextLeaf=leaf;return {request:node.dataset.sealedPageRequest,path:leaf.dataset.fragmentPath,text:leaf.textContent.slice(0,60),top:leaf.getBoundingClientRect().top-box.top,first:node.dataset.sealedFirst,part:node.dataset.renderPart}})
  await page.evaluate(()=>{window.holdArchivePages=false;for(const release of window.heldArchivePages.splice(0))release()})
  await until(async()=>await viewport().evaluate((node,request)=>node.dataset.sealedPageRequest!==request,before.request));await h.idle()
  await viewport().evaluate(async node=>{let last=node.scrollTop,stable=0;for(let frame=0;frame<90&&stable<4;frame++){await new Promise(resolve=>requestAnimationFrame(resolve));const top=node.scrollTop;stable=Math.abs(top-last)<0.25?stable+1:0;last=top}if(stable<4)throw Error('Indexed anchor restore failed to settle')})
  const after=await viewport().evaluate((node,path)=>({same:window.indexedTextLeaf.isConnected,top:window.indexedTextLeaf.getBoundingClientRect().top-node.getBoundingClientRect().top,first:node.dataset.sealedFirst,part:node.dataset.renderPart,anchor:node.dataset.presentationAnchor,following:node.dataset.following,matching:[...node.querySelectorAll('[data-fragment-path]')].filter(leaf=>leaf.dataset.fragmentPath===path).map(leaf=>({text:leaf.textContent.slice(0,60),same:leaf===window.indexedTextLeaf})),rows:node.querySelectorAll('.session-record').length}),before.path)
  assert.equal(after.same,true,JSON.stringify({before,after}));assert.ok(Math.abs(after.top-before.top)<=3,JSON.stringify({before,after}));assert.ok(after.first<before.first)
  let beginning=false
  for(let step=0;step<16&&!beginning;step++){await viewport().press('Home');await h.idle();beginning=await viewport().evaluate(node=>node.textContent.includes('Verified indexed occurrence 0.'))}
  assert.equal(beginning,true,'repeated genuine upward scrolling reaches the first verified occurrence')
  let reached=false,stalled=0,previous='',middleMotion=[]
  for(let step=0;step<160&&!reached;step++){
   const motionBefore=await viewport().evaluate(node=>node.scrollTop);await viewport().press('PageDown');const motionPressed=await viewport().evaluate(node=>node.scrollTop);await h.idle()
   await viewport().evaluate(async node=>{let last=node.scrollTop,stable=0;for(let frame=0;frame<90&&stable<4;frame++){await new Promise(resolve=>requestAnimationFrame(resolve));stable=Math.abs(last-node.scrollTop)<0.25?stable+1:0;last=node.scrollTop}if(stable<4)throw Error('Indexed PageDown motion did not settle')})
   const progress=await viewport().evaluate(node=>({reached:node.textContent.includes('Verified indexed occurrence 250.'),rows:node.querySelectorAll('.session-record').length,key:[node.dataset.sealedPageRequest,node.dataset.renderPart,node.scrollTop].join(':'),attrs:{...node.dataset},height:node.scrollHeight,gaps:[...node.querySelectorAll('[data-history-gap="source"]')].map(gap=>({...gap.dataset,top:gap.getBoundingClientRect().top,height:gap.getBoundingClientRect().height})),status:[...node.parentElement.querySelectorAll('[role="alert"],[role="status"]')].map(node=>node.textContent.slice(0,100))}))
   assert.ok(progress.rows<=200);reached=progress.reached;stalled=progress.key===previous?stalled+1:0;previous=progress.key;assert.ok(stalled<6,JSON.stringify({phase:'sealed-middle-progress',step,progress,frames:await page.evaluate(()=>window.indexedTargetProof)}))
   middleMotion=[...middleMotion.slice(-7),{step,before:motionBefore,pressed:motionPressed,after:progress.key}]
  }
  assert.equal(reached,true,'continuous indexed scrolling reaches an omitted middle occurrence: '+JSON.stringify({frames:await page.evaluate(()=>window.indexedTargetProof),motion:middleMotion,state:await viewport().evaluate(node=>({attrs:{...node.dataset},top:node.scrollTop,height:node.scrollHeight,client:node.clientHeight,focus:document.activeElement?.id,fragments:[...node.querySelectorAll('[data-fragment-from]')].map(gap=>({...gap.dataset,top:gap.getBoundingClientRect().top,bottom:gap.getBoundingClientRect().bottom})),visible:[...node.querySelectorAll('[data-fragment-path]')].filter(leaf=>{const r=leaf.getBoundingClientRect(),b=node.getBoundingClientRect();return r.height&&r.bottom>b.top&&r.top<b.bottom}).map(leaf=>({path:leaf.dataset.fragmentPath,top:leaf.getBoundingClientRect().top})).slice(0,3)}))}))
  await viewport().press('End');await until(async()=>await viewport().evaluate((node,last)=>node.textContent.includes(`Verified indexed occurrence ${last}.`)&&node.scrollHeight-node.scrollTop-node.clientHeight<=4,count-1));await page.getByRole('button',{name:'Jump to latest',exact:true}).waitFor({state:'hidden'})
  assert.ok(await page.evaluate(()=>window.archivePageCount<80));assert.equal(await conversationPaging(page).count(),0);assert.deepEqual(h.errors,[])
 }finally{await h.close()}
})

test('continuous contract exact JSON body parts prepend without replacing the text being read',{timeout:60000},async()=>{
 const values=Array.from({length:800},(_,i)=>`${i===0?'JSON body START':i===799?'JSON body END':'JSON body'} ${i}. `+'x'.repeat(120))
 const raw=Buffer.from(JSON.stringify({type:'response_item',payload:{type:'function_call',name:'body_continuity',arguments:JSON.stringify({values})}})+'\n')
 const h=await openSessionExplorer({transcriptBytes:raw}),{page}=h,viewport=()=>page.locator('#session-records')
 try{
  await page.setViewportSize({width:1200,height:1200})
  await page.addInitScript(()=>{const original=Worker.prototype.postMessage;window.heldBodyReads=[];window.bodyReadCount=0;window.bodyReadParts=[];window.noncontiguousBodyWindow=false;Worker.prototype.postMessage=function(message,...rest){if(message.kind==='body'){window.bodyReadCount++;window.bodyReadParts.push(message.part);window.bodyReadParts=window.bodyReadParts.slice(-80);if(window.holdBodyReads){window.heldBodyReads.push(()=>original.call(this,message,...rest));return}}return original.call(this,message,...rest)};new MutationObserver(()=>{for(const node of document.querySelectorAll('[data-body-parts]')){const parts=node.dataset.bodyParts.split(',').filter(Boolean).map(Number);if(parts.some((part,index)=>index>0&&part!==parts[index-1]+1))window.noncontiguousBodyWindow=true}}).observe(document,{subtree:true,childList:true,attributes:true,attributeFilter:['data-body-parts']})})
  await routeVerifiedArchive(h,raw);await h.start();await page.goto(h.origin+'/workspace/'+h.fixture.workspace.id+'#tab=sessions&session='+h.saved.id)
  await page.locator('.session-activity-summary').click();await page.locator('.session-content-tool summary').click()
  await until(async()=>await viewport().evaluate(node=>node.textContent.includes('JSON body END 799.')))
  await h.idle();assert.equal(await conversationPaging(page).count(),0);assert.equal(await page.getByRole('button',{name:'Jump to latest',exact:true}).isVisible(),false)
  const cache=await viewport().evaluate(node=>[...node.querySelectorAll('[data-body-part-index]')].map(node=>Number(node.dataset.bodyPartIndex)))
  assert.ok(Math.min(...cache)>0&&cache.length<=3,'earlier exact body parts are omitted and refetchable')
  const beforeReopen=await page.evaluate(()=>window.bodyReadCount)
  await page.locator('.session-content-tool summary').click();await page.locator('.session-content-tool summary').click()
  await until(async()=>await viewport().evaluate(node=>node.textContent.includes('JSON body END 799.')));await h.idle()
  const reopened=await page.evaluate(before=>window.bodyReadParts.slice(-(window.bodyReadCount-before)),beforeReopen)
  for(const part of [0,1,2,3,4])assert.ok(reopened.includes(part),'nonadjacent close/reopen must keep every intermediate body part reachable: '+JSON.stringify(reopened))
  assert.equal(await page.evaluate(()=>window.noncontiguousBodyWindow),false,'every retained body window is contiguous')
  await viewport().hover();await page.mouse.wheel(0,-320);await until(async()=>await page.getByRole('button',{name:'Jump to latest',exact:true}).isVisible())
  await page.evaluate(()=>{window.holdBodyReads=true})
  for(let step=0;step<80&&!(await page.evaluate(()=>window.heldBodyReads.length>0));step++){await viewport().press('PageUp');await h.idle()}
  assert.ok(await page.evaluate(()=>window.heldBodyReads.length>0),'continuous upward scrolling refetches an omitted body part')
  await viewport().evaluate(async node=>{let last=node.scrollTop,stable=0;for(let frame=0;frame<90&&stable<4;frame++){await new Promise(resolve=>requestAnimationFrame(resolve));const top=node.scrollTop;stable=Math.abs(top-last)<0.25?stable+1:0;last=top}if(stable<4)throw Error('Body input did not settle')})
  const before=await viewport().evaluate(node=>{const box=node.getBoundingClientRect(),leaf=[...node.querySelectorAll('[data-fragment-path]')].find(leaf=>{const r=leaf.getBoundingClientRect();return !leaf.querySelector('[data-body-gap]')&&leaf.closest('[data-body-part-index]')&&r.height>0&&r.bottom>box.top&&r.top<box.bottom});if(!leaf)throw Error('Held body prepend has no visible text witness');window.bodyTextLeaf=leaf;return {top:leaf.getBoundingClientRect().top-box.top,text:leaf.textContent,part:leaf.closest('[data-body-part-index]').dataset.bodyPartIndex}})
  await page.evaluate(()=>{window.holdBodyReads=false;for(const release of window.heldBodyReads.splice(0))release()});await h.idle()
  await until(async()=>await viewport().evaluate((node,part)=>[...node.querySelectorAll('[data-body-part-index]')].some(node=>Number(node.dataset.bodyPartIndex)<Number(part)),before.part))
  const after=await viewport().evaluate(node=>({same:window.bodyTextLeaf.isConnected,text:window.bodyTextLeaf.textContent,top:window.bodyTextLeaf.getBoundingClientRect().top-node.getBoundingClientRect().top,cost:[...node.querySelectorAll('[data-fragment-cost]')].reduce((sum,node)=>sum+Number(node.dataset.fragmentCost),0),bytes:new TextEncoder().encode(node.innerHTML).length}))
  assert.equal(after.same,true,'body-part prepend preserves the actual text leaf');assert.equal(after.text,before.text);assert.ok(Math.abs(after.top-before.top)<=3,JSON.stringify({before:{top:before.top,part:before.part},after:{...after,text:undefined}}));assert.ok(after.cost<=1048576&&after.bytes<=1048576)
  let start=false;for(let step=0;step<16&&!start;step++){await viewport().press('Home');await h.idle();start=await viewport().evaluate(node=>node.textContent.includes('JSON body START 0.'))}assert.equal(start,true,JSON.stringify({phase:'JSON-start',state:await viewport().evaluate(node=>({attrs:{...node.dataset},top:node.scrollTop,parts:[...node.querySelectorAll('[data-body-part-index]')].map(node=>node.dataset.bodyPartIndex),windows:[...node.querySelectorAll('[data-body-window]')].map(node=>({...node.dataset})),gaps:[...node.querySelectorAll('[data-body-gap]')].map(node=>({...node.dataset})),prefix:node.textContent.slice(0,100),status:[...node.parentElement.querySelectorAll('[role="alert"],[role="status"]')].map(node=>node.textContent.slice(0,100))})),reads:await page.evaluate(()=>window.bodyReadCount)}))
  await viewport().press('End');await until(async()=>await viewport().evaluate(node=>node.textContent.includes('JSON body END 799.')))
  assert.equal(await conversationPaging(page).count(),0);assert.ok(await page.evaluate(()=>window.bodyReadCount<80));assert.equal(await page.evaluate(()=>window.noncontiguousBodyWindow),false);assert.deepEqual(h.errors,[])
 }finally{await h.close()}
})

test('continuous contract preserves the exact structured reasoning exception within its512KiB window',{timeout:60000},async()=>{
 const summary={note:'JSON body START 0. [Literal](https://example.test) **literal** λ😀',values:Array(100).fill(0),padding:'',ending:'JSON body END 799.'},size=1048577
 summary.padding='x'.repeat(size-Buffer.byteLength(JSON.stringify(summary,null,2)))
 const raw=Buffer.from(JSON.stringify({type:'response_item',payload:{type:'reasoning',summary}})+'\n');assert.ok(raw.length<=1048576)
 const h=await openSessionExplorer({transcriptBytes:raw}),{page}=h,viewport=()=>page.locator('#session-records')
 try{
  await routeVerifiedArchive(h,raw);await h.start();await page.goto(h.origin+'/workspace/'+h.fixture.workspace.id+'#tab=sessions&session='+h.saved.id)
  await page.locator('.session-activity-summary').click();await page.locator('.session-content-thinking summary').click()
  await until(async()=>await viewport().evaluate(node=>node.textContent.includes('JSON body END 799.')))
  await page.getByText('Large structured reasoning · exact JSON',{exact:true}).waitFor();assert.equal(await conversationPaging(page).count(),0)
  await viewport().press('Home');await until(async()=>await viewport().evaluate(node=>node.textContent.includes('JSON body START 0.')))
  const proof=await viewport().evaluate(node=>({literal:node.textContent.includes('[Literal](https://example.test) **literal** λ😀'),links:node.querySelectorAll('.session-tool-content a,.session-tool-content strong').length,cost:[...node.querySelectorAll('[data-fragment-cost]')].reduce((sum,node)=>sum+Number(node.dataset.fragmentCost),0),bytes:new TextEncoder().encode(node.innerHTML).length,parts:node.querySelectorAll('[data-body-part-index]').length}))
  assert.equal(proof.literal,true);assert.equal(proof.links,0);assert.ok(proof.cost<=524288&&proof.bytes<=524288,JSON.stringify(proof));assert.ok(proof.parts<=3);assert.deepEqual(h.errors,[])
 }finally{await h.close()}
})

for(const fixture of ['indivisible','tiny leaves','wide table'])test(`continuous contract charges the actual ${fixture} presentation window`,{timeout:45000},async()=>{
 const prose=fixture==='indivisible'?`[Supported large link](https://example.com/${'x'.repeat(80000)})`:fixture==='tiny leaves'?Array.from({length:6000},(_,i)=>`Tiny ${i}.  \n`).join(''):
  '|'+Array.from({length:180},(_,i)=>`Header ${i}`).join('|')+'|\n|'+Array(180).fill('---').join('|')+'|\n|'+Array.from({length:180},(_,i)=>`[Cell ${i}][wide]`).join('|')+'|\n\n[wide]: https://example.com/'+ 'z'.repeat(3000)
 const raw=Buffer.from(JSON.stringify({text:prose})+'\n'),h=await openSessionExplorer({transcriptBytes:raw}),{page}=h,viewport=()=>page.locator('#session-records')
 try{
  await routeVerifiedArchive(h,raw);await h.start();await page.goto(h.origin+'/workspace/'+h.fixture.workspace.id+'#tab=sessions&session='+h.saved.id)
  await until(async()=>await viewport().evaluate(node=>node.querySelectorAll('[data-fragment-cost]').length>0&&!node.querySelector('[data-sealed-body-needed]')&&node.scrollHeight-node.scrollTop-node.clientHeight<=4))
  const receipt=await viewport().evaluate(node=>({serialized:new TextEncoder().encode(node.innerHTML).length,cost:[...node.querySelectorAll('[data-fragment-cost]')].reduce((sum,leaf)=>sum+Number(leaf.dataset.fragmentCost),0),part:Number(node.dataset.renderPart),link:node.querySelector('a')?.getAttribute('href')?.length,columns:node.querySelector('table')?.getAttribute('aria-colcount'),rows:[...node.querySelectorAll('tbody tr')].filter(row=>row.querySelector('[aria-colindex]')).map(row=>({cells:[...row.children].map(cell=>({index:Number(cell.getAttribute('aria-colindex')),span:cell.colSpan,text:cell.textContent.slice(0,25)})),headers:[...node.querySelectorAll('thead [aria-colindex]')].map(cell=>({index:Number(cell.getAttribute('aria-colindex')),text:cell.textContent.slice(0,25)}))}))}))
  assert.ok(receipt.cost<=1048576,JSON.stringify(receipt));assert.ok(receipt.serialized<=1048576,'actual wrappers/markers fit the whole mounted limit: '+JSON.stringify(receipt))
  assert.equal(await page.getByText('This formatted piece exceeds the local presentation budget.',{exact:true}).count(),0)
  if(fixture==='indivisible')assert.ok(receipt.link>80000,'an indivisible leaf above one-third remains supported')
  if(fixture==='wide table'){
   assert.equal(receipt.columns,'180');assert.ok(receipt.part>0)
   for(const row of receipt.rows){assert.equal(row.cells.reduce((sum,cell)=>sum+cell.span,0),180);for(const cell of row.cells.filter(cell=>cell.index>0))assert.ok(row.headers.some(header=>header.index===cell.index&&header.text===`Header ${cell.index-1}`),'original header matches each retained source column')}
   assert.ok(receipt.rows.some(row=>row.cells.some(cell=>cell.index>1)),'the window includes a real later table column')
  }
  assert.equal(await conversationPaging(page).count(),0);assert.deepEqual(h.errors,[])
 }finally{await h.close()}
})

test('progressive large hydration and multi-window bursts preserve follow while real historical input pauses it',{timeout:60000},async()=>{
 const record=(size,marker)=>{const head=JSON.stringify({text:marker}).slice(0,-2);return Buffer.from(head+'x'.repeat(size-Buffer.byteLength(head)-3)+'"}\n')}
 const prefix=Buffer.concat(Array.from({length:16},(_,i)=>record(65536,`Initial ${i} `))),burst=Buffer.concat(Array.from({length:32},(_,i)=>record(8192,`Burst ${i} `)))
 const h=await openSessionExplorer({transcriptBytes:prefix}),{page}=h;let hold=true,releaseRead=null
 const settled=()=>page.locator('#session-records').evaluate((node,end)=>node.dataset.historyLoading==='false'&&Number(node.dataset.historyLatest)>=end&&Number(node.dataset.historyAfter)>=end&&node.scrollHeight-node.scrollTop-node.clientHeight<=4,h.active.committed_bytes)
 try{
  await page.addInitScript(()=>document.addEventListener('keydown',event=>{if(event.key!=='End'||event.target.id!=='session-records')return;const node=event.target,snapshot=()=>({attrs:{...node.dataset},top:node.scrollTop,height:node.scrollHeight,client:node.clientHeight});window.initialEndProof={before:snapshot(),trusted:event.isTrusted,scrolls:[]};node.addEventListener('scroll',()=>{window.initialEndProof.scrolls.push(snapshot());window.initialEndProof.scrolls=window.initialEndProof.scrolls.slice(-8)})},true))
  await page.route('**/sessions/**/transcript**',async route=>{if(hold){hold=false;await new Promise(resolve=>{releaseRead=resolve})}else await new Promise(resolve=>setTimeout(resolve,20));await route.fallback()})
  await h.start();await h.idle();await page.goto(h.origin+'/workspace/'+h.fixture.workspace.id+'#tab=sessions&session='+h.active.id);await page.locator('#session-records').waitFor();await until(async()=>releaseRead!==null)
  const viewport=page.locator('#session-records');assert.equal(await viewport.getAttribute('data-history-loading'),'true')
  await viewport.press('End');releaseRead();releaseRead=null;await until(settled).catch(async error=>{console.log(JSON.stringify({phase:'initial-End',proof:await page.evaluate(()=>window.initialEndProof),after:await viewport.evaluate(node=>({attrs:{...node.dataset},top:node.scrollTop,height:node.scrollHeight,client:node.clientHeight})),reads:h.reads.filter(read=>read.path.endsWith('/transcript')).slice(-3)}));throw error});assert.equal((await state(page)).after,prefix.length)
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
   assert.equal(await latest().isVisible(),false)
   const outside=await page.evaluate(()=>{const node=document.createElement('div');node.id='unrelated-scroll-host';node.style.cssText='position:fixed;z-index:9999;left:0;top:0;width:100px;height:80px;overflow:auto';node.innerHTML='<div style="height:1500px">Other scroll host</div>';document.body.append(node);node.scrollTop=500;return document.getElementById('session-records').scrollTop})
   await page.locator('#unrelated-scroll-host').hover();await page.mouse.wheel(0,-100);await until(()=>page.locator('#unrelated-scroll-host').evaluate(node=>node.scrollTop<500))
   assert.equal(await latest().isVisible(),false);assert.equal(await page.locator('#session-records').evaluate(node=>node.scrollTop),outside);await page.locator('#unrelated-scroll-host').evaluate(node=>node.remove())
   await armLatestOverlay(page);await h.idle()
   assert.equal(await latest().locator('[aria-hidden="true"]').count(),2);assert.equal((await latest().textContent()).replaceAll(/\s/g,''),'↓Jumptolatest↓')
   if(surface==='workspace')for(const width of [1440,430]){
    await page.setViewportSize({width,height:900});await h.idle();await latest().waitFor({state:'visible'})
    const button=await latest().boundingBox(),records=await page.locator('#session-records').boundingBox()
    assert.ok(Math.abs(button.x+button.width/2-records.x-records.width/2)<8);assert.ok(button.y+button.height<=records.y+records.height-8)
   }
   for(let cycle=0;cycle<3;cycle++){
    if(cycle)await armLatestOverlay(page)
    if(cycle===0){
     await page.locator('#session-records').hover();await page.mouse.wheel(0,700)
     for(let step=0;step<30&&!(await atEnd());step++){await page.locator('#session-records').press('PageDown');await h.idle();await page.locator('#session-records').evaluate(async node=>{let top=node.scrollTop,stable=0;for(let frame=0;frame<90&&stable<4;frame++){await new Promise(resolve=>requestAnimationFrame(resolve));const next=node.scrollTop;stable=Math.abs(top-next)<.25?stable+1:0;top=next}if(stable<4)throw Error('Manual return motion did not settle')})}
    }
    else {await latest().focus();await page.keyboard.press(cycle===1?'Enter':'Space')}
    await until(atEnd);await latest().waitFor({state:'hidden'})
    if(cycle)assert.equal(await page.locator('#session-records').evaluate(node=>document.activeElement===node),true)
    await h.appendRecords({text:`Following resumes on ${surface} cycle ${cycle}`});await page.getByText(`Following resumes on ${surface} cycle ${cycle}`,{exact:true}).waitFor();await until(atEnd);assert.equal(await latest().isVisible(),false)
   }
  }
  await armLatestOverlay(page);await h.idle()
  await h.appendRecords({text:'Newest output outside the mounted history'});await until(async()=>{const value=await state(page);return value.after<value.latest})
  const clearance=(await latest().boundingBox()).height+12
  await latest().click();await page.getByText('Newest output outside the mounted history',{exact:true}).waitFor();await until(atEnd)
  assert.ok(await page.locator('#session-records').evaluate((node,clearance)=>node.querySelector('.session-record:last-child').getBoundingClientRect().bottom<=node.getBoundingClientRect().bottom-clearance+2,clearance),'reserved space keeps the control clear of the actual last mounted message at logical latest')
  await h.appendRecords({text:'Following resumes after latest navigation'});await page.getByText('Following resumes after latest navigation',{exact:true}).waitFor();await until(atEnd)
  assert.deepEqual(h.errors,[])
 }finally{await h.close()}
})

test('completed workspace repeats manual return and keyboard Jump without losing viewer ownership',{timeout:45000},async()=>{
 const raw=Buffer.from(Array.from({length:180},(_,i)=>JSON.stringify({text:`Completed cycle occurrence ${i}. `+'Readable context. '.repeat(30)})+'\n').join('')),h=await openSessionExplorer({transcriptBytes:raw}),{page}=h,viewport=()=>page.locator('#session-records'),jump=()=>page.getByRole('button',{name:'Jump to latest',exact:true})
 const atEnd=()=>viewport().evaluate(node=>node.dataset.latestPending==='false'&&node.dataset.archiveCanNext==='false'&&node.dataset.renderMore==='false'&&node.scrollHeight-node.scrollTop-node.clientHeight<=4)
 try{
  await routeVerifiedArchive(h,raw);await page.setViewportSize({width:430,height:720});await page.emulateMedia({reducedMotion:'reduce'});await h.start();await page.goto(h.origin+'/workspace/'+h.fixture.workspace.id+'#tab=sessions&session='+h.saved.id);await page.getByText('Verified complete archive.',{exact:true}).waitFor();await page.locator('.session-record').first().waitFor();await h.idle();await until(atEnd);assert.equal(await jump().isVisible(),false)
  const outside=await page.evaluate(()=>{const node=document.createElement('div');node.id='unrelated-scroll-host';node.style.cssText='position:fixed;z-index:9999;left:0;top:0;width:100px;height:80px;overflow:auto';node.innerHTML='<div style="height:1500px">Other scroll host</div>';document.body.append(node);node.scrollTop=500;return document.getElementById('session-records').scrollTop})
  await page.locator('#unrelated-scroll-host').hover();await page.mouse.wheel(0,-100);await until(()=>page.locator('#unrelated-scroll-host').evaluate(node=>node.scrollTop<500));assert.equal(await jump().isVisible(),false);assert.equal(await viewport().evaluate(node=>node.scrollTop),outside);await page.locator('#unrelated-scroll-host').evaluate(node=>node.remove())
  await viewport().evaluate(node=>{node.scrollTop=Math.max(0,node.scrollTop-180)});await h.idle();assert.equal(await jump().isVisible(),false,'programmatic history positioning cannot arm the overlay');await viewport().press('End');await until(atEnd)
  for(let cycle=0;cycle<3;cycle++){
   await armLatestOverlay(page);assert.equal(await viewport().getAttribute('data-history-mode'),'sealed')
   if(cycle===0){for(let step=0;step<30&&!(await atEnd());step++){await viewport().press('PageDown');await h.idle();await viewport().evaluate(async node=>{let top=node.scrollTop,stable=0;for(let frame=0;frame<90&&stable<4;frame++){await new Promise(resolve=>requestAnimationFrame(resolve));const next=node.scrollTop;stable=Math.abs(top-next)<.25?stable+1:0;top=next}if(stable<4)throw Error('Completed manual return did not settle')})}}
   else {await jump().focus();await page.keyboard.press(cycle===1?'Enter':'Space')}
   await until(atEnd);await jump().waitFor({state:'hidden'});assert.equal(await viewport().evaluate(node=>node.closest('.session-viewer').dataset.manualUp),'false');if(cycle)assert.equal(await viewport().evaluate(node=>document.activeElement===node),true)
  }
  assert.equal(await conversationPaging(page).count(),0);assert.ok(await page.locator('.session-record').count()<=200);assert.deepEqual(h.errors,[])
 }finally{await h.close()}
})

test('a failed active latest request retains history and the same overlay retries it',{timeout:45000},async()=>{
 const bytes=Buffer.from(Array.from({length:500},(_,i)=>JSON.stringify({text:`Retry active occurrence ${i} `+'text '.repeat(50)})+'\n').join(''))
 const h=await openSessionExplorer({transcriptBytes:bytes}),{page}=h;let failing=false
 try{
  await page.route('**/sessions/**/transcript**',route=>failing?route.fulfill({status:400,json:{error:'controlled_latest_failure'}}):route.fallback())
  await h.start();await h.idle();await page.goto(h.origin+'/workspace/'+h.fixture.workspace.id+'#tab=sessions&session='+h.active.id);await page.locator('.session-record').first().waitFor();await until(async()=>!(await state(page)).loading)
  await armLatestOverlay(page);await h.idle()
  const old=(await state(page)).offsets
  failing=true;await h.appendRecords({text:'Output beyond the historical page'});await until(async()=>{const value=await state(page);return value.after<value.latest})
  const jump=page.getByRole('button',{name:'Jump to latest',exact:true});await jump.click()
  await page.locator('.session-latest-error').waitFor()
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
  else await until(async()=>!(await state(page)).loading)
  await h.idle();await armLatestOverlay(page);await h.idle();if(!sealed)failing=true
  const old=await page.locator('.session-record').evaluateAll(nodes=>nodes.map(node=>node.dataset.blockKey)),jump=page.getByRole('button',{name:'Jump to latest',exact:true})
  await jump.click();await page.locator('.session-latest-error').waitFor()
  assert.deepEqual(await page.locator('.session-record').evaluateAll(nodes=>nodes.map(node=>node.dataset.blockKey)),old)
  await page.locator('.session-latest-error').evaluate(node=>node.scrollIntoView({block:'start',inline:'nearest'}));await jump.focus();await page.keyboard.press('ArrowLeft');await h.idle()
  const geometry=await jump.evaluate(button=>{
   const host=button.closest('.session-viewer'),records=host.querySelector('#session-records'),b=button.getBoundingClientRect(),r=records.getBoundingClientRect(),style=getComputedStyle(button),overlap=node=>{const a=node.getBoundingClientRect();return a.width>0&&a.height>0&&a.left<b.right&&a.right>b.left&&a.top<b.bottom&&a.bottom>b.top}
   const owners=[];let clipped=false;for(let node=button.parentElement;node;node=node.parentElement){const style=getComputedStyle(node);if(/auto|scroll|hidden/.test(style.overflowY)){const rect=node.getBoundingClientRect();owners.push({class:node.className,top:rect.top,bottom:rect.bottom,client:node.clientHeight,scroll:node.scrollHeight,position:node.scrollTop});if(b.top<rect.top-1||b.bottom>rect.bottom+1)clipped=true}}
   const last=records.querySelector('.session-record:last-child')?.getBoundingClientRect(),alert=host.querySelector('.session-latest-error'),mainNode=host.closest('.main-content')
   if(!alert||!mainNode)throw Error(JSON.stringify({phase:'retry-geometry-host',alert:!!alert,main:!!mainNode,alerts:[...document.querySelectorAll('[role="alert"]')].map(node=>node.textContent),attrs:{...records.dataset},host:{...host.dataset},ancestors:[host.parentElement?.className,host.parentElement?.parentElement?.className]}))
   const a=alert.getBoundingClientRect(),main=mainNode.getBoundingClientRect()
   return {clipped:clipped||b.top<0||b.bottom>innerHeight,center:Math.abs((b.left+b.right-r.left-r.right)/2),inside:b.top>=r.top&&b.bottom<=r.bottom-6,overlap:[...host.querySelectorAll('[role="alert"],.sessions-pagination,.session-archive-footer')].some(overlap),reserved:parseFloat(getComputedStyle(records).paddingBottom),lastClearance:last?b.top-last.bottom:null,recordClient:records.clientHeight,recordScroll:records.scrollHeight,focus:button===document.activeElement,color:style.color,background:style.backgroundColor,outline:style.outlineWidth,transition:style.transitionDuration,owners,button:b.toJSON(),records:r.toJSON(),alert:{rect:a.toJSON(),visible:a.top>=main.top&&a.bottom<=main.bottom&&owners.every(owner=>a.top>=owner.top-1&&a.bottom<=owner.bottom+1),text:alert.textContent},stamp:{...records.dataset},document:{client:document.documentElement.clientHeight,scroll:document.documentElement.scrollHeight,position:scrollY}}
  })
  if(process.env.SESSION_LATEST_LAYOUT_SCREENSHOTS){const path=join(process.env.SESSION_LATEST_LAYOUT_SCREENSHOTS,`${sealed?'sealed':'active'}-error-${width}.png`);await page.screenshot({path});console.log(JSON.stringify({layout:{sealed,width,geometry,path}}))}
  assert.equal(geometry.alert.visible,true,'failure alert and retained reader stay visible in the same frame');assert.equal(geometry.stamp.latestError,'true');assert.equal(geometry.stamp.latestPending,'false')
  assert.equal(geometry.clipped,false);assert.ok(geometry.center<2&&geometry.inside);assert.equal(geometry.overlap,false);assert.ok(geometry.reserved>=geometry.button.height+12,'reserved transcript tail space accommodates the control and clearance');assert.ok(geometry.focus&&parseFloat(geometry.outline)>=3);assert.equal(geometry.transition,'0s');assert.ok(geometry.recordClient>=120&&geometry.recordClient<900&&geometry.recordScroll>geometry.recordClient,'intrinsic chrome minimum leaves readable space and an independently scrollable bounded transcript')
  const parse=value=>value.match(/[\d.]+/g).map(Number),foreground=parse(geometry.color),background=parse(geometry.background),alpha=background[3]??1
  const luminance=rgb=>rgb.slice(0,3).map(value=>{const s=value/255;return s<=.04045?s/12.92:((s+.055)/1.055)**2.4}).reduce((sum,value,index)=>sum+value*[.2126,.7152,.0722][index],0),fg=luminance(foreground),bg=luminance(background.slice(0,3).map(value=>value*alpha+255*(1-alpha)))
  assert.ok(alpha>0&&alpha<1);assert.ok((Math.max(fg,bg)+.05)/(Math.min(fg,bg)+.05)>=4.5,'semitransparent control retains text contrast even over white content')
  assert.equal(await jump.locator('[aria-hidden="true"]').count(),2)
  failing=false;await page.keyboard.press('Enter')
  await until(()=>page.locator('#session-records').evaluate((node,sealed)=>node.dataset.latestPending==='false'&&(sealed?node.dataset.archiveCanNext==='false'&&node.dataset.renderMore==='false':Number(node.dataset.historyAfter)>=Number(node.dataset.historyLatest))&&node.scrollHeight-node.scrollTop-node.clientHeight<=4,sealed));await jump.waitFor({state:'hidden'});assert.deepEqual(h.errors,[])
 }finally{await h.close()}
})

for(const width of [1440,430])test(`sealed continuous presentation stays clear of latest navigation at ${width}px`,{timeout:45000},async()=>{
 const raw=Buffer.from(Array.from({length:220},(_,i)=>JSON.stringify({text:`Conversation ${i}\n\nA readable paragraph with **emphasis**, a [reference](https://example.com), and enough text to exercise bounded history. `+'Reading context. '.repeat(300)})+'\n').join(''))
 const h=await openSessionExplorer({transcriptBytes:raw}),{page}=h
 try{
  const hash=bytes=>createHash('sha256').update(bytes).digest('hex'),gzip=gzipSync(raw),generation='33333333-3333-4333-8333-333333333333',etag='"presentation-verified-archive"'
  const manifest={workspace_id:h.fixture.workspace.id,session_id:h.saved.id,state:'ready',generation_id:generation,codec:'gzip-v1',raw_bytes:String(raw.length),raw_records:'220',raw_sha256:hash(raw),compressed_bytes:String(gzip.length),compressed_sha256:hash(gzip),content_path:`/api/v1/workspaces/${h.fixture.workspace.id}/sessions/${h.saved.id}/archive/${generation}/content`}
  await page.route('**/sessions/**/archive**',route=>new URL(route.request().url()).pathname.endsWith('/content')?route.fulfill(route.request().headers()['if-none-match']?{status:304,headers:{ETag:etag}}:{headers:{'Content-Type':'application/gzip',ETag:etag,'Content-Length':String(gzip.length),'X-HMem-Archive-Generation':generation,'X-HMem-Archive-SHA256':hash(gzip),'X-HMem-Archive-Raw-Bytes':String(raw.length),'X-HMem-Archive-Raw-SHA256':hash(raw)},body:gzip}):route.fulfill({json:manifest}))
  await page.setViewportSize({width,height:width===430?720:900});await h.start();await page.goto(h.origin+'/workspace/'+h.fixture.workspace.id+'#tab=sessions&session='+h.saved.id);await until(()=>page.locator('#session-records').evaluate(node=>node.dataset.archiveCanNext==='false'&&node.dataset.renderMore==='false'&&node.scrollHeight-node.scrollTop-node.clientHeight<=4));assert.equal(await conversationPaging(page).count(),0)
  await h.idle();await armLatestOverlay(page);const oldPart=Number(await page.locator('#session-records').getAttribute('data-render-part'))
  const jump=page.getByRole('button',{name:'Jump to latest',exact:true});await jump.evaluate(node=>node.scrollIntoView({block:'center',inline:'nearest'}));await jump.focus();await h.idle()
  const geometry=await jump.evaluate(button=>{const b=button.getBoundingClientRect(),host=button.closest('.session-viewer'),records=host.querySelector('#session-records'),r=records.getBoundingClientRect(),controls=[...host.querySelectorAll('.session-render-continuation,.session-archive-footer')].map(node=>node.getBoundingClientRect());return {inside:b.top>=r.top&&b.bottom<=r.bottom-6,visible:b.top>=0&&b.bottom<=innerHeight,overlap:controls.some(a=>a.top<b.bottom&&a.bottom>b.top&&a.left<b.right&&a.right>b.left),height:records.clientHeight,scroll:records.scrollHeight}})
  if(process.env.SESSION_LATEST_LAYOUT_SCREENSHOTS)await page.screenshot({path:join(process.env.SESSION_LATEST_LAYOUT_SCREENSHOTS,`sealed-presentation-${width}.png`)})
  assert.ok(geometry.inside&&geometry.visible);assert.equal(geometry.overlap,false);assert.ok(geometry.height>=120&&geometry.height<900&&geometry.scroll>geometry.height,JSON.stringify(geometry))
  await page.locator('#session-records').press('Home');await until(async()=>Number(await page.locator('#session-records').getAttribute('data-render-part'))!==oldPart)
  assert.notEqual(Number(await page.locator('#session-records').getAttribute('data-render-part')),oldPart,'genuine scrolling automatically continues bounded presentation');assert.ok(await page.locator('.session-record').count()<=200)
  await jump.focus();await page.keyboard.press('Enter');await until(()=>jump.isHidden()).catch(async error=>{console.log(JSON.stringify({phase:'presentation-Jump',width,state:await page.locator('#session-records').evaluate(node=>({attrs:{...node.dataset},top:node.scrollTop,height:node.clientHeight,scroll:node.scrollHeight,host:{...node.closest('.session-viewer').dataset},alerts:[...node.closest('.session-viewer').querySelectorAll('[role="alert"],[role="status"]')].map(node=>node.textContent),first:node.textContent.slice(0,100),last:node.textContent.slice(-100)})),reads:h.reads.slice(-3)}));throw error});assert.deepEqual(h.errors,[])
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
  await page.evaluate(()=>{
   window.historyTraversal={phase:'backward',backward:new Set(),forward:new Set(),invalid:false,peak:0}
   const collect=()=>{const state=window.historyTraversal,offsets=[...document.querySelectorAll('#session-records .session-record')].map(row=>Number(row.dataset.offset));state.peak=Math.max(state.peak,offsets.length);if(offsets.some((value,index)=>index>0&&value<=offsets[index-1]))state.invalid=true;for(const offset of offsets)state[state.phase].add(offset)}
   window.collectHistoryTraversal=collect;new MutationObserver(collect).observe(document.getElementById('session-records'),{childList:true,subtree:true});collect()
  })
  for(const direction of [-1,1]){
   if(direction>0)await page.evaluate(()=>{window.historyTraversal.phase='forward';window.collectHistoryTraversal()})
   let acquisitions=0,stalled=0,previous='',lastMotion=null
   for(let step=0;step<400;step++){
    const current=await state(page),seen=await page.evaluate(()=>window.historyTraversal[window.historyTraversal.phase].size)
    if(seen===expected.length&&(direction<0?current.before===0:current.after===bytes.length))break
    const fingerprint=await page.locator('#session-records').evaluate(node=>[node.dataset.historyBefore,node.dataset.historyAfter,node.dataset.renderPart,Math.round(node.scrollTop)].join(':'))
    stalled=fingerprint===previous?stalled+1:0;assert.ok(stalled<6,JSON.stringify({phase:'continuous-traversal-stall',direction,step,seen,acquisitions,lastMotion,reads:h.reads.filter(read=>read.path.endsWith('/transcript')).slice(-4),expectedSeam:expected.slice(50,57),state:await page.locator('#session-records').evaluate(node=>({attrs:{...node.dataset},top:node.scrollTop,height:node.clientHeight,scroll:node.scrollHeight,first:node.textContent.slice(0,90),last:node.textContent.slice(-90),gaps:[...node.querySelectorAll('[data-history-gap],[data-fragment-from]')].slice(0,5).map(gap=>({...gap.dataset,top:gap.getBoundingClientRect().top-node.getBoundingClientRect().top,bottom:gap.getBoundingClientRect().bottom-node.getBoundingClientRect().top}))}))}));previous=fingerprint
    lastMotion=await continuousScrollStep(page,direction);await h.idle()
    const next=await state(page);if(direction<0?next.before<current.before:next.after>current.after)acquisitions++
    assert.ok(acquisitions<40,'source acquisition must retain the original finite traversal bound')
    assert.ok(step<399,'continuous traversal cannot strand an occurrence')
   }
   const receipt=await page.evaluate(()=>({seen:[...window.historyTraversal[window.historyTraversal.phase]].sort((a,b)=>a-b),invalid:window.historyTraversal.invalid,peak:window.historyTraversal.peak}))
   assert.deepEqual(receipt.seen,expected);assert.equal(receipt.invalid,false,'every mounted source interval is ordered without duplicate occurrences');assert.ok(receipt.peak<=200)
  }
  assert.equal(await conversationPaging(page).count(),0)
  assert.deepEqual(h.errors,[]);assert.ok(h.peak()<=2)
 }finally{await h.close()}
})

for(const suffix of ['', '\r\n'])test(`an initial lookback inside a large final record ${suffix?'with CRLF':'without LF'} expands to publish it`,{timeout:45000},async()=>{
 const bytes=Buffer.from(JSON.stringify({text:'Earlier record'})+'\n'+JSON.stringify({text:'Final large record '+'x'.repeat(180000)})+suffix)
 const h=await openSessionExplorer({transcriptBytes:bytes}),{page}=h
 try{
  await h.start();await h.idle();await page.goto(h.origin+'/workspace/'+h.fixture.workspace.id+'#tab=sessions&session='+h.active.id)
  await page.locator('.session-record').first().waitFor()
  await until(async()=>!(await state(page)).loading)
  assert.equal((await state(page)).after,bytes.length)
  await page.locator('#session-records').press('Home');await page.getByText(/^Final large record/).waitFor()
  assert.deepEqual(h.errors,[])
 }finally{await h.close()}
})

test('an incomplete latest suffix settles an explicit Jump and real growth reparses its original prefix',{timeout:30000},async()=>{
 const prefix=Buffer.from(Array.from({length:180},(_,index)=>JSON.stringify({text:'Complete prefix '+index+' '+ 'Readable context. '.repeat(20)})+'\n').join('')),partial=Buffer.from('{"type":'),h=await openSessionExplorer({transcriptBytes:Buffer.concat([prefix,partial])}),{page}=h
 try{
  await h.start();await page.goto(h.origin+'/workspace/'+h.fixture.workspace.id+'#tab=sessions&session='+h.active.id);await page.getByText('Complete prefix 179 '+ 'Readable context. '.repeat(20),{exact:true}).waitFor();await until(async()=>!(await state(page)).loading)
  const reads=h.reads.filter(row=>row.path.endsWith('/transcript')).length;await page.waitForTimeout(250);assert.equal(h.reads.filter(row=>row.path.endsWith('/transcript')).length,reads)
  await armLatestOverlay(page);const jump=page.getByRole('button',{name:'Jump to latest',exact:true});await jump.click();await until(()=>page.locator('#session-records').evaluate(node=>node.dataset.latestPending==='false'&&node.dataset.historyLoading==='false'));await jump.waitFor({state:'hidden'})
  await h.appendBytes(Buffer.from('"text","text":"Completed latest suffix"}\n'));await page.getByText('Completed latest suffix',{exact:true}).waitFor();await until(async()=>!(await state(page)).loading)
  assert.equal(await page.locator('.session-record[data-offset="'+prefix.length+'"]').count(),1);assert.equal(await page.getByText('Incomplete record at this finite window boundary.',{exact:true}).count(),0);assert.deepEqual(h.errors,[]);assert.ok(h.peak()<=2)
 }finally{await h.close()}
})

test('real boundary scrolling preserves the visible anchor through history, appends and resize',{timeout:45000},async()=>{
 const bytes=Buffer.from(Array.from({length:420},(_,i)=>JSON.stringify({type:'event_msg',payload:{type:'agent_message',message:`Reading anchor ${i} `+'word '.repeat(20)}})+'\n').join(''))
 const h=await openSessionExplorer({transcriptBytes:bytes}),{page}=h;let hold=false,release=null
 const anchor=()=>page.locator('#session-records').evaluate(node=>{const bounds=node.getBoundingClientRect(),row=[...node.querySelectorAll('[data-block-key]')].find(row=>row.getBoundingClientRect().bottom>bounds.top);return {key:row.dataset.blockKey,top:row.getBoundingClientRect().top-bounds.top}})
 const position=key=>page.locator('#session-records').evaluate((node,key)=>{const row=[...node.querySelectorAll('[data-block-key]')].find(row=>row.dataset.blockKey===key);return row?row.getBoundingClientRect().top-node.getBoundingClientRect().top:null},key)
 try{
  await traceSessionBridge(h)
  await page.route('**/sessions/**/transcript**',async route=>{if(hold){hold=false;await new Promise(resolve=>{release=resolve})}await route.fallback()})
  await h.start();await h.idle();await page.goto(h.origin+'/workspace/'+h.fixture.workspace.id+'#tab=sessions&session='+h.active.id)
  await page.locator('.session-record').first().waitFor();await until(async()=>!(await state(page)).loading)
  const before=await state(page);hold=true
  for(let step=0;release===null;step++){assert.ok(step<12,'real upward scrolling reaches the older acquisition boundary');await continuousScrollStep(page,-1);await page.waitForTimeout(30)}
  const reading=await anchor();await page.locator('#session-records [data-block-key="'+reading.key+'"]').evaluate(node=>window.heldReadingNode=node);release();release=null
  await until(async()=>{const current=await state(page);return !current.loading&&current.before<before.before})
  await h.idle();if(await position(reading.key)===null)console.log(JSON.stringify({heldPrependTrace:await page.evaluate(()=>window.sessionBridgeFacts.filter(fact=>fact.generation>=6&&fact.direction==='event').map(({dom,...fact})=>({...fact,dom:{generation:dom.generation,part:dom.part,top:dom.top}})).slice(-16))}));assert.ok(Math.abs((await position(reading.key))-reading.top)<=4,JSON.stringify({phase:'held-prepend-reading',reading,after:await position(reading.key),state:await page.locator('#session-records').evaluate(node=>({attrs:{...node.dataset},top:node.scrollTop,height:node.clientHeight,scroll:node.scrollHeight}))}))
  assert.equal(await page.locator('#session-records').evaluate(node=>{const saved=window.heldReadingNode,box=node.getBoundingClientRect(),r=saved.getBoundingClientRect();return saved.isConnected&&node.contains(saved)&&r.bottom>box.top&&r.top<box.bottom}),true,'the exact prepublication reading node remains connected and clipped inside the viewer')
  await h.appendRecords({text:'New live output while reading history'});await h.idle()
  assert.ok(Math.abs((await position(reading.key))-reading.top)<=4,'appends preserve a historical anchor')
  await page.setViewportSize({width:900,height:720});await h.idle()
  assert.ok(Math.abs((await position(reading.key))-reading.top)<=4,'resize preserves the same occurrence and intrarow position')
  assert.deepEqual(h.errors,[])
 }finally{release?.();await h.close()}
})
