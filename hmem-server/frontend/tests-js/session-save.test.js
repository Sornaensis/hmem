import assert from 'node:assert/strict'
import test from 'node:test'
import { createHash } from 'node:crypto'
import { createEvidenceWriter, installSessionBridge, updateLatestOverlay } from '../src/session-save.js'
const digest = bytes => createHash('sha256').update(bytes).digest('hex')
const session = bytes => ({id:'s',workspace:'w',state:'terminated',finalBytes:bytes.length,finalSha256:digest(bytes),revision:7})
const chunk = (row,bytes,offset=0) => ({session:row,offset,nextOffset:offset+bytes.length,dataBase64:bytes.toString('base64')})

test('latest overlay uses logical end and only mutates its own reader host',()=>{
 const attrs={},foreign={},host={setAttribute:(key,value)=>{attrs[key]=value}},other={setAttribute:(key,value)=>{foreign[key]=value}}
 const flags={},viewport={scrollTop:900,scrollHeight:1000,clientHeight:100,closest:()=>host,getAttribute:key=>flags[key]}
 updateLatestOverlay(viewport,false);assert.equal(attrs['data-latest-visible'],'true','historical DOM bottom is still earlier history')
 updateLatestOverlay(viewport,true);assert.equal(attrs['data-latest-visible'],'false')
 viewport.scrollTop=200;updateLatestOverlay(viewport,true);assert.equal(attrs['data-latest-visible'],'true')
 viewport.scrollTop=0;viewport.scrollHeight=80;updateLatestOverlay(viewport,true);assert.equal(attrs['data-latest-visible'],'false','a complete fitting conversation needs no control')
 flags['data-latest-pending']='true';updateLatestOverlay(viewport,true);assert.equal(attrs['data-latest-visible'],'true','pending acquisition remains actionable')
 assert.deepEqual(foreign,{});assert.ok(other)
})

test('active latest waits for matching owner generation request and logical tail render',async()=>{
 const previous=globalThis.requestAnimationFrame,frames=[],events=[];let incoming
 const attrs={'data-viewer-owner':'owner','data-history-mode':'active','data-viewer-generation':'4','data-latest-request':'1','data-history-after':'50','data-history-latest':'100','data-history-loading':'false'}
 const viewport={scrollTop:20,scrollHeight:1000,clientHeight:100,getAttribute:key=>attrs[key] ?? null}
 globalThis.requestAnimationFrame=callback=>frames.push(callback)
 const paint=()=>{for(let turn=0;turn<4&&frames.length;turn++)frames.splice(0).forEach(callback=>callback())}
 try{
  installSessionBridge({ports:{sessionBridgeCommand:{subscribe(fn){incoming=fn}},sessionBridgeEvent:{send:value=>events.push(value)}}},{document:{getElementById:()=>viewport,addEventListener(){}},window:{},now:()=>0})
  const command=(extra={})=>incoming({stamp:'owner',generation:5,reset:null,chunk:null,latest:{request:2,pending:true},...extra})
  command();paint();await flush();assert.equal(viewport.scrollTop,20)
  attrs['data-viewer-generation']='5';attrs['data-latest-request']='2';command();paint();await flush();assert.equal(viewport.scrollTop,20,'historical mounted bottom cannot complete logical latest')
  attrs['data-history-after']='100';attrs['data-history-loading']='true';command();paint();await flush();assert.equal(viewport.scrollTop,20)
  attrs['data-history-loading']='false';command();paint();await flush();assert.equal(viewport.scrollTop,1000)
  assert.ok(events.some(event=>event.kind==='latest-complete'&&event.stamp==='owner'&&event.generation===5&&event.request===2))
  viewport.scrollTop=30;command({latest:{request:3,pending:true}});command({stamp:'other',generation:6,latest:{request:0,pending:false}});paint();await flush();assert.equal(viewport.scrollTop,30,'retired jump cannot scroll another selection')
 }finally{globalThis.requestAnimationFrame=previous}
})

test('owned tail positioning preserves follow through finite-window handoffs without rearming historical or stale readers',async()=>{
 const previous=globalThis.requestAnimationFrame,frames=[],events=[],listeners=new Map();let incoming,top=900
 const attrs={'data-viewer-owner':'owner','data-history-mode':'active','data-history-before':'0','data-history-after':'64','data-history-latest':'192','data-history-loading':'true'}
 const viewport={scrollHeight:1000,clientHeight:100,get scrollTop(){return top},set scrollTop(value){top=Math.max(0,Math.min(900,value))},getAttribute:key=>attrs[key] ?? null}
 const emit=(name,extra={})=>(listeners.get(name)||[]).forEach(handler=>handler({type:name,target:viewport,...extra}))
 globalThis.requestAnimationFrame=callback=>frames.push(callback)
 const paint=()=>{for(let i=0;i<8&&frames.length;i++)frames.splice(0).forEach(callback=>callback())}
 const latestFollow=()=>events.filter(event=>event.kind==='viewer-follow').at(-1)
 try{
  installSessionBridge({ports:{sessionBridgeCommand:{subscribe(fn){incoming=fn}},sessionBridgeEvent:{send:value=>events.push(value)}}},{document:{getElementById:()=>viewport,addEventListener(name,handler){listeners.set(name,[...(listeners.get(name)||[]),handler])}},window:{},now:()=>0})
  const bytes=Buffer.from('{"text":"Progressive tail"}\n')
  incoming({stamp:'owner',generation:1,reset:0,chunk:{offset:0,nextOffset:bytes.length,dataBase64:bytes.toString('base64'),final:true},direction:'tail',follow:true});paint();await flush()
  emit('scroll');await flush();assert.equal(latestFollow().following,true,'following survives an incomplete logical tail while loading')
  attrs['data-history-loading']='false';emit('scroll');await flush();assert.equal(latestFollow().following,true,'the owned position survives the finite-window scheduling gap')
  viewport.scrollHeight=1200;emit('scroll');await flush();assert.equal(latestFollow().following,true,'newly rendered rows cannot revoke follow before owned tail positioning')
  viewport.scrollHeight=1000
  incoming({stamp:'owner',generation:2,reset:null,chunk:null,direction:'tail',follow:true});emit('scroll');await flush();assert.equal(latestFollow().following,false,'an old generation position cannot rearm a new read lifetime')
  incoming({stamp:'owner',generation:2,reset:null,chunk:{offset:bytes.length,nextOffset:bytes.length*2,dataBase64:bytes.toString('base64'),final:true},direction:'tail',follow:true});paint();await flush()
  emit('wheel',{deltaY:-300});viewport.scrollTop=500;emit('scroll');await flush();assert.equal(latestFollow().following,false,'real above-tail intent pauses follow')
  incoming({stamp:'owner',generation:2,reset:null,chunk:null,direction:'tail',follow:false});viewport.scrollTop=900;attrs['data-history-loading']='true';emit('keydown',{key:'End'});emit('scroll');await flush();assert.equal(latestFollow().following,false,'End at a historical mounted bottom is not Jump to latest')
  incoming({stamp:'other',generation:3,reset:null,chunk:null,direction:'tail',follow:true});attrs['data-viewer-owner']='other';attrs['data-history-loading']='false';emit('scroll');await flush();assert.equal(latestFollow().following,false,'an old owner position cannot rearm a new selection')
 }finally{globalThis.requestAnimationFrame=previous}
})

test('streams exact bytes and incremental SHA256 before committing writable',async()=>{
  const bytes=Buffer.from('"λ"\r\n[true,null]\n'+ '"'+ 'x'.repeat(200000)+'"\n'), row=session(bytes)
  const independent=createHash('sha256'); let count=0, closed=false, aborted=false
  const writer=createEvidenceWriter({async write(part){assert.ok(part.length<=65536); independent.update(part);count+=part.length},async close(){closed=true},async abort(){aborted=true}},row)
  for(let offset=0;offset<bytes.length;offset+=65536) {
    const result=await writer.write(chunk(row,bytes.subarray(offset,offset+65536),offset))
    assert.equal(result.complete,offset+65536>=bytes.length)
  }
  assert.equal(independent.digest('hex'),row.finalSha256);assert.equal(count,bytes.length);assert.equal(closed,true);assert.equal(aborted,false)
})

test('does not close partial, mismatched hash/receipt/cursor or retired writes',async()=>{
  const bytes=Buffer.from('true\n'), row=session(bytes)
  for(const mutate of [value=>({...value,session:{...row,revision:8}}),value=>({...value,offset:1}),value=>({...value,dataBase64:Buffer.from('null\n').toString('base64')})]) {
    let closed=false,aborted=false
    const writer=createEvidenceWriter({async write(){},async close(){closed=true},async abort(){aborted=true}},row)
    await assert.rejects(writer.write(mutate(chunk(row,bytes))))
    assert.equal(closed,false);assert.equal(aborted,true)
  }
})

test('one pending write blocks further chunks and stale authority aborts before close',async()=>{
  const bytes=Buffer.from('true\n'),row=session(bytes);let release,current=true,closed=false,aborted=false
  const writer=createEvidenceWriter({write(){return new Promise(done=>{release=done})},async close(){closed=true},async abort(){aborted=true}},row,()=>current)
  const pending=writer.write(chunk(row,bytes))
  await assert.rejects(writer.write(chunk(row,bytes)),/retired_writer/)
  current=false;release();await assert.rejects(pending,/retired_writer/)
  assert.equal(closed,false);assert.equal(aborted,true)
})

test('empty sealed evidence verifies and closes without a range request',async()=>{
  const bytes=Buffer.alloc(0),row=session(bytes);let writes=0,closed=false
  const writer=createEvidenceWriter({async write(){writes++},async close(){closed=true},async abort(){}},row)
  assert.deepEqual(await writer.write(chunk(row,bytes)),{offset:0,complete:true});assert.equal(writes,0);assert.equal(closed,true)
})

test('abort keeps ownership until the pending write has unwound',async()=>{
  const bytes=Buffer.from('true\n'),row=session(bytes);let release,joined=false
  const writer=createEvidenceWriter({write(){return new Promise(done=>{release=done})},async close(){assert.fail('retired writer closed')},async abort(){}},row)
  const writing=writer.write(chunk(row,bytes))
  const retiring=writer.abort().then(()=>{joined=true})
  await new Promise(done=>setTimeout(done,5));assert.equal(joined,false);assert.equal(writer.snapshot().busy,true)
  release();await assert.rejects(writing,/retired_writer/);await retiring
  assert.equal(writer.snapshot().busy,false);assert.equal(joined,true)
})

const deferred = () => { let resolve,reject;const promise=new Promise((yes,no)=>{resolve=yes;reject=no});return {promise,resolve,reject} }
const flush = () => new Promise(done=>setImmediate(done))

test('forward incomplete batches preserve the admitted page through a failed boundary and retry',async()=>{
 const previous=globalThis.requestAnimationFrame,events=[];let incoming
 globalThis.requestAnimationFrame=()=>0
 try{
  installSessionBridge({ports:{sessionBridgeCommand:{subscribe(fn){incoming=fn}},sessionBridgeEvent:{send:value=>events.push(value)}}},{document:{addEventListener(){},getElementById:()=>null},window:{},now:()=>0})
  const old=Buffer.from('{"text":"Already admitted conversation"}\n'),large=Buffer.from(JSON.stringify({text:'Next large message '+'x'.repeat(180000)})+'\n')
  const command=(offset,bytes,final,extra={})=>incoming({stamp:'owner',generation:1,reset:null,chunk:{offset,nextOffset:offset+bytes.length,dataBase64:bytes.toString('base64'),final},follow:false,direction:'forward',aligned:true,...extra})
  command(0,old,true,{reset:0});await flush()
  command(old.length,large.subarray(0,65536),false,{generation:2,reset:old.length,historical:true})
  await flush()
  assert.equal(events.filter(event=>event.kind==='viewer-progress').at(-1).through,old.length+65536)
  assert.deepEqual(events.filter(event=>event.kind==='viewer').at(-1).rows.map(row=>row.text),['Already admitted conversation'],'a failed next request leaves the usable page admitted')
  // Transport failure sends no chunk. An explicit retry may replay the window;
  // each incomplete response remains progress until the record is usable.
  for(let at=0;at<large.length;at+=65536)command(old.length+at,large.subarray(at,at+65536),at+65536>=large.length,{generation:3,reset:at===0?old.length:null,historical:at===0})
  await flush()
  const published=events.filter(event=>event.kind==='viewer')
  assert.equal(published.length,2);assert.match(published[1].rows[0].text,/^Next large message /)
  assert.ok(events.filter(event=>event.kind==='viewer-progress').every(event=>!('rows' in event)))
 }finally{globalThis.requestAnimationFrame=previous}
})
function bridgeFixture(picker) {
  let incoming,click
  const events=[],app={ports:{sessionBridgeCommand:{subscribe(fn){incoming=fn}},sessionBridgeEvent:{send(value){events.push(value)}}}}
  installSessionBridge(app,{document:{addEventListener(name,fn){if(name==='click')click=fn}},window:{showSaveFilePicker:picker},now:()=>0})
  return {events,configure(stamp,nextOperation=1,extra={}){incoming({stamp,generation:0,reset:null,chunk:null,saveChunk:null,saveOperation:null,nextOperation,heartbeats:[],...extra})},click(stamp,operation=1){
    const config={stamp,operation,session:session(Buffer.from('true\n'))},button={disabled:false,dataset:{sessionSave:JSON.stringify(config)}}
    click({target:{closest(){return button}},preventDefault(){}})
  }}
}

test('writer acquisition retains its reservation through retirement and captured-owner abort completion',async()=>{
  const acquisition=deferred(),aborting=deferred();let picks=0,oldAborts=0,newAborts=0
  const bridge=bridgeFixture(()=>{picks++;return Promise.resolve({createWritable:()=>picks===1?acquisition.promise:Promise.resolve({async write(){},async close(){},async abort(){newAborts++}})})})
  bridge.configure('old');bridge.click('old');await flush()
  bridge.configure('new',2);bridge.click('new',2);assert.equal(picks,1,'pending createWritable owns the only reservation')
  acquisition.resolve({async write(){},async close(){},async abort(){oldAborts++;await aborting.promise}});await flush()
  bridge.click('new',2);assert.equal(picks,1,'stale acquired writer keeps reservation until abort joins')
  aborting.resolve();await flush();await flush();bridge.click('new',2);await flush()
  assert.equal(picks,2);assert.equal(oldAborts,1);assert.equal(newAborts,0)
  assert.ok(bridge.events.some(event=>event.kind==='save-ready'&&event.stamp==='new'))
})

test('a failed retired acquisition cannot dispose a later save owner',async()=>{
  const acquisition=deferred();let picks=0,newAborts=0
  const bridge=bridgeFixture(()=>{picks++;return Promise.resolve({createWritable:()=>picks===1?acquisition.promise:Promise.resolve({async write(){},async close(){},async abort(){newAborts++}})})})
  bridge.configure('old');bridge.click('old');await flush();bridge.configure('new',2)
  acquisition.reject(new Error('fixture acquisition failed'));await flush();await flush()
  bridge.click('new',2);await flush();await flush()
  assert.equal(picks,2);assert.equal(newAborts,0)
  assert.ok(bridge.events.some(event=>event.kind==='save-ready'&&event.stamp==='new'))
})

test('late viewer acknowledgements and animation callbacks retain their retired generation and cannot scroll paused resync',async()=>{
  const previous=globalThis.requestAnimationFrame,frames=[],events=[];let incoming
  const viewport={scrollTop:37,scrollHeight:900,clientHeight:863}
  globalThis.requestAnimationFrame=callback=>frames.push(callback)
  try{
    installSessionBridge({ports:{sessionBridgeCommand:{subscribe(fn){incoming=fn}},sessionBridgeEvent:{send(value){events.push(value)}}}},
      {document:{addEventListener(){},getElementById(){return viewport}},window:{},now:()=>31000})
    const bytes=Buffer.from('{"text":"before"}\n')
    const command={stamp:'authority-before-snapshot',generation:3,reset:0,chunk:{offset:0,nextOffset:bytes.length,dataBase64:bytes.toString('base64'),final:true},follow:true,heartbeats:[{id:'s',lastSeenAt:'1970-01-01T00:00:00Z'}]}
    incoming(command)
    incoming({...command,stamp:'same-authority-new-snapshot',generation:4,reset:0,chunk:null,follow:false})
    await flush();frames.forEach(callback=>callback())
    assert.equal(viewport.scrollTop,37)
    const late=events.find(event=>event.kind==='viewer')
    assert.equal(late.stamp,command.stamp);assert.equal(late.generation,3);assert.equal(late.rows.length,1)
    assert.equal(events.find(event=>event.kind==='heartbeat-status').statuses.s,'Stale (30s)')
  }finally{globalThis.requestAnimationFrame=previous}
})

test('new transcript chunks preserve a scrolled-up reader and follow readers at the tail',async()=>{
 const previous=globalThis.requestAnimationFrame,frames=[];let incoming
 const viewport={scrollTop:40,scrollHeight:900,clientHeight:300}
 globalThis.requestAnimationFrame=callback=>frames.push(callback)
 try{
  installSessionBridge({ports:{sessionBridgeCommand:{subscribe(fn){incoming=fn}},sessionBridgeEvent:{send(){}}}},{document:{addEventListener(){},getElementById(){return viewport}},window:{},now:()=>0})
  const chunk=(offset,text)=>({offset,nextOffset:offset+text.length,dataBase64:Buffer.from(text).toString('base64'),final:true})
  incoming({stamp:'current',generation:1,reset:0,chunk:chunk(0,'true\n'),follow:true})
  await flush();assert.equal(frames.length,0);assert.equal(viewport.scrollTop,40)
  viewport.scrollTop=600
  incoming({stamp:'current',generation:1,reset:null,chunk:chunk(5,'null\n'),follow:true})
  await flush();assert.equal(frames.length,1)
  viewport.scrollHeight=1100;frames.shift()();assert.equal(viewport.scrollTop,1100)
  viewport.scrollTop=800
  incoming({stamp:'current',generation:1,reset:null,chunk:chunk(10,'false\n'),follow:true})
  await flush();viewport.scrollTop=500;frames.shift()();assert.equal(viewport.scrollTop,500,'a scroll before paint also cancels auto-follow')
 }finally{globalThis.requestAnimationFrame=previous}
})

test('finite windows publish once and same-owner generation handoff replays one unacknowledged short tail before growth',async()=>{
 const previous=globalThis.requestAnimationFrame,frames=[],events=[];let incoming
 const viewport={scrollTop:0,scrollHeight:0,clientHeight:300}
 globalThis.requestAnimationFrame=callback=>frames.push(callback)
 try{
  installSessionBridge({ports:{sessionBridgeCommand:{subscribe(fn){incoming=fn}},sessionBridgeEvent:{send(value){events.push(value)}}}},{document:{addEventListener(){},getElementById(){return viewport}},window:{},now:()=>0})
  const first=Buffer.from('{"text":"First admitted message"}\n'),tail=Buffer.from('{"text":"Short tail"}\n'),growth=Buffer.from('{"text":"Later growth"}\n')
  const chunk=(offset,bytes,final)=>({offset,nextOffset:offset+bytes.length,dataBase64:bytes.toString('base64'),final})
  incoming({stamp:'owner',generation:1,reset:0,chunk:chunk(0,first,false),follow:true})
  await flush();assert.equal(events.filter(event=>event.kind==='viewer-progress').at(-1).through,first.length);assert.equal(events.filter(event=>event.kind==='viewer').length,0);assert.equal(frames.length,0)
  incoming({stamp:'owner',generation:1,reset:null,chunk:chunk(first.length,tail,true),follow:true})
  // Retire the acknowledgement after JS has framed the old finite end. Growth
  // is deliberately a later chunk; replay keeps the original short boundary.
  incoming({stamp:'owner',generation:2,reset:null,chunk:null,follow:true})
  await flush();frames.splice(0).forEach(callback=>callback())
  assert.equal(viewport.scrollTop,0,'the old generation cannot position after handoff')
  incoming({stamp:'owner',generation:2,reset:null,chunk:chunk(first.length,tail,true),follow:true})
  await flush();assert.deepEqual(events.filter(event=>event.kind==='viewer').at(-1).rows.map(row=>row.text),['First admitted message','Short tail'])
  incoming({stamp:'owner',generation:2,reset:null,chunk:chunk(first.length+tail.length,growth,true),follow:true})
  await flush();assert.deepEqual(events.filter(event=>event.kind==='viewer').at(-1).rows.map(row=>row.text),['First admitted message','Short tail','Later growth'])
  assert.equal(events.filter(event=>event.kind==='viewer-error').length,0)
 }finally{globalThis.requestAnimationFrame=previous}
})

test('explicit decode recovery retains displayed rows until its bounded finite replacement is complete',async()=>{
 const previous=globalThis.requestAnimationFrame,events=[];let incoming
 globalThis.requestAnimationFrame=()=>0
 try{
  installSessionBridge({ports:{sessionBridgeCommand:{subscribe(fn){incoming=fn}},sessionBridgeEvent:{send(value){events.push(value)}}}},{document:{addEventListener(){},getElementById(){return null}},window:{},now:()=>0})
  const chunk=(offset,text,final)=>({offset,nextOffset:offset+Buffer.byteLength(text),dataBase64:Buffer.from(text).toString('base64'),final})
  incoming({stamp:'owner',generation:1,reset:0,chunk:chunk(0,'{"text":"Admitted before recovery"}\n',true),follow:true});await flush()
  const partial='leading fragment\n{"text":"Recovered'
  incoming({stamp:'owner',generation:2,reset:100,chunk:chunk(100,partial,false),follow:true});await flush()
  assert.deepEqual(events.filter(event=>event.kind==='viewer').at(-1).rows.map(row=>row.text),['Admitted before recovery'])
  assert.equal('rows' in events.filter(event=>event.kind==='viewer-progress').at(-1),false,'intermediate recovery acknowledges only its cursor')
  incoming({stamp:'owner',generation:2,reset:null,chunk:chunk(100+Buffer.byteLength(partial),'"}\n',true),follow:true});await flush()
  assert.deepEqual(events.filter(event=>event.kind==='viewer').at(-1).rows.map(row=>row.text),['Recovered'])
  assert.equal(events.filter(event=>event.kind==='viewer-error').length,0)
 }finally{globalThis.requestAnimationFrame=previous}
})

test('queued follow and anchor positioning yield to either scroll direction, disclosure intent and owner changes',async()=>{
 const previous=globalThis.requestAnimationFrame
 globalThis.requestAnimationFrame=callback=>frames.push(callback)
 const frames=[];let incoming,offset=0,nodeTop=20
 const listeners={},node={getBoundingClientRect:()=>({height:40,top:nodeTop,bottom:nodeTop+40}),hasAttribute:key=>key==='data-block-key',getAttribute:()=> 'kept:0'}
 const viewport={scrollTop:40,scrollHeight:1000,clientHeight:300,getBoundingClientRect:()=>({top:0,bottom:300}),querySelectorAll:()=>[node],contains:target=>target===node}
 const document={addEventListener(name,fn){(listeners[name]??=[]).push(fn)},getElementById:()=>viewport}
 try{
  installSessionBridge({ports:{sessionBridgeCommand:{subscribe(fn){incoming=fn}},sessionBridgeEvent:{send(){}}}},{document,window:{},now:()=>0})
  const send=(extra={})=>{const bytes=Buffer.from('{"text":"next"}\n');incoming({stamp:'owner',generation:1,reset:offset===0?0:null,chunk:{offset,nextOffset:offset+bytes.length,dataBase64:bytes.toString('base64'),final:true},follow:true,...extra});offset+=bytes.length}
  send();await flush();nodeTop=70;frames.pop()();assert.equal(viewport.scrollTop,90,'a retained visible article preserves its relative reading anchor')
  for(const tail of [false,true])for(const direction of [-20,20]){
   viewport.scrollTop=tail?700:40;nodeTop=20;send();await flush();const callback=frames.pop();assert.equal(typeof callback,'function')
   viewport.scrollTop+=direction;nodeTop+=50;const expected=viewport.scrollTop;callback();assert.equal(viewport.scrollTop,expected)
  }
  for(const tail of [false,true]){
   viewport.scrollTop=tail?700:40;nodeTop=20;send();await flush();const callback=frames.pop()
   for(const listener of listeners.toggle)listener({target:node})
   nodeTop+=50;const expected=viewport.scrollTop;callback();assert.equal(viewport.scrollTop,expected,'a disclosure interaction supersedes positioning')
  }
  viewport.scrollTop=40;nodeTop=20;send();await flush();const callback=frames.pop()
  incoming({stamp:'different owner',generation:2,reset:null,chunk:null,follow:true});nodeTop+=50;callback();assert.equal(viewport.scrollTop,40)
 }finally{globalThis.requestAnimationFrame=previous}
})

test('task/full presentation restoration yields to newer scroll, disclosure and owner intent',async()=>{
 const previous=globalThis.requestAnimationFrame
 try{
  for(const change of ['none','up','down','toggle','owner']){
   const frames=[],listeners={};let incoming,viewport
   globalThis.requestAnimationFrame=callback=>frames.push(callback)
   const node=()=>({open:false,getAttribute:key=>key==='data-group-key'?'kept':null})
   const view=(owner,top=0)=>({scrollTop:top,querySelectorAll:()=>[disclosure],getAttribute:key=>key==='data-viewer-owner'?owner:null,contains:target=>target===disclosure})
   let disclosure=node();viewport=view('dock',40);disclosure.open=true
   const document={addEventListener(name,fn){(listeners[name]??=[]).push(fn)},getElementById:()=>viewport}
   installSessionBridge({ports:{sessionBridgeCommand:{subscribe(fn){incoming=fn}},sessionBridgeEvent:{send(){}}}},{document,window:{},now:()=>0})
   incoming({stamp:'dock',viewerScope:'authorized-source',generation:1,reset:0,chunk:null})
   for(const listener of listeners.scroll)listener({type:'scroll',target:viewport})
   disclosure=node();viewport=view('full')
   incoming({stamp:'full',viewerScope:'authorized-source',continueViewer:true,generation:2,reset:null,chunk:null})
   assert.equal(frames.length,1)
   if(change==='up')viewport.scrollTop=-20
   if(change==='down')viewport.scrollTop=20
   if(change==='toggle'){disclosure.open=true;for(const listener of listeners.toggle)listener({target:disclosure})}
   if(change==='owner')incoming({stamp:'retired',viewerScope:'new-authority',generation:3,reset:null,chunk:null})
   frames.shift()()
   assert.equal(viewport.scrollTop,change==='none'?40:change==='up'?-20:change==='down'?20:0)
   assert.equal(disclosure.open,change==='none'||change==='toggle')
  }
 }finally{globalThis.requestAnimationFrame=previous}
})

test('presentation snapshots scan admitted disclosures once and ignore unrelated document events',()=>{
 const previous=globalThis.requestAnimationFrame,frames=[],listeners={};let incoming,queries=0,keys=0,viewport
 globalThis.requestAnimationFrame=callback=>frames.push(callback)
 try{
  const nodes=Array.from({length:600},(_,index)=>({open:index===599,getAttribute(name){keys++;return name==='data-event-key'?String(index):null},closest(){assert.fail('a stable event key needs no per-detail subtree scan')}}))
  const view=owner=>({scrollTop:40,getAttribute:()=>owner,contains:target=>nodes.includes(target),querySelectorAll(){queries++;return nodes}})
  viewport=view('dock')
  installSessionBridge({ports:{sessionBridgeCommand:{subscribe(fn){incoming=fn}},sessionBridgeEvent:{send(){}}}},{document:{addEventListener(name,fn){(listeners[name]??=[]).push(fn)},getElementById:()=>viewport},window:{},now:()=>0})
  incoming({stamp:'dock',viewerScope:'source',generation:1,reset:0,chunk:null})
  for(const type of ['click','scroll','toggle'])for(const listener of listeners[type])listener({type,target:{closest:()=>null}})
  assert.equal(queries,0,'outside-viewer activity does not snapshot the admitted DOM')
  for(const listener of listeners.scroll)listener({type:'scroll',target:viewport})
  assert.equal(queries,1);assert.equal(keys,1200,'two constant-time key reads per admitted detail')
  viewport=view('full');viewport.scrollTop=0
  incoming({stamp:'full',viewerScope:'source',continueViewer:true,generation:2,reset:null,chunk:null})
  frames.shift()()
  assert.equal(queries,2);assert.equal(keys,2400);assert.equal(nodes[599].open,true)
 }finally{globalThis.requestAnimationFrame=previous}
})

test('a delayed notification for a scroll settled before framing is not newer user intent',async()=>{
 const previous=globalThis.requestAnimationFrame,frames=[],listeners={};let incoming
 const viewport={scrollTop:600,scrollHeight:900,clientHeight:300}
 globalThis.requestAnimationFrame=callback=>frames.push(callback)
 try{
  installSessionBridge({ports:{sessionBridgeCommand:{subscribe(fn){incoming=fn}},sessionBridgeEvent:{send(){}}}},{document:{addEventListener(name,fn){(listeners[name]??=[]).push(fn)},getElementById(){return viewport}},window:{},now:()=>0})
  const bytes=Buffer.from('{"text":"New tail"}\n')
  incoming({stamp:'owner',generation:1,reset:0,chunk:{offset:0,nextOffset:bytes.length,dataBase64:bytes.toString('base64'),final:true},follow:true})
  await flush();assert.equal(frames.length,1)
  // The position was already600 when framing captured it. Browsers deliver
  // its scroll notification asynchronously; no intervening input occurred.
  for(const listener of listeners.scroll||[])listener({target:viewport})
  viewport.scrollHeight=1100;frames.pop()()
  assert.equal(viewport.scrollTop,1100)
 }finally{globalThis.requestAnimationFrame=previous}
})



function binaryBridge(transport){
 let incoming;const listeners=[],events=[],downloads=[]
 const app={ports:{sessionBridgeCommand:{subscribe(fn){incoming=fn}},sessionBridgeEvent:{send(value){events.push(value)}}}}
 let file
 installSessionBridge(app,{fetchArchive:transport,document:{addEventListener(name,fn){if(name==='click')listeners.push(fn)},createElement(){return{click(){downloads.push(file)}}}},window:{navigator:{},crypto:{randomUUID:()=> 'fixture'},setTimeout(){},URL:{createObjectURL(value){file=value;return 'blob:fixture'},revokeObjectURL(){}},addEventListener(){}},now:()=>0})
 const command=(extra={})=>incoming({stamp:'scope',generation:0,reset:null,chunk:null,saveChunk:null,saveOperation:null,nextOperation:1,heartbeats:[],...extra})
 command()
 return{events,downloads,command,click(operation=1){const config={stamp:'scope',operation,task:'task',title:'Task evidence'},button={disabled:false,dataset:{sessionArchive:JSON.stringify(config)}};for(const fn of listeners)fn({target:{closest(selector){return selector==='[data-session-archive]'?button:null}},preventDefault(){}})},start(){command({nextOperation:2,saveOperation:1,archiveRequest:{id:7,stamp:'scope',operation:1,task:'task',url:'http://api.test/api/v1/workspaces/w/tasks/task/evidence.zip'}})}}
}

test('binary archive transport stages the exact server response without ZIP assembly',async()=>{
 const bytes=Buffer.from('opaque server-compressed ZIP bytes'),bridge=binaryBridge(async()=>({response:new Response(bytes,{headers:{'Content-Type':'application/zip'}}),isCurrent:()=>true}))
 bridge.click();await flush();await flush();bridge.start();for(let i=0;i<10;i++)await flush()
 assert.equal(bridge.downloads.length,1);assert.deepEqual(Buffer.from(await bridge.downloads[0].arrayBuffer()),bytes)
 assert.ok(bridge.events.some(event=>event.kind==='archive-http'&&event.request===7&&event.status===200))
})
test('an unsuccessful 200 stream never settles as ArchiveDownloaded or publishes partial bytes',async()=>{
 const stream=new ReadableStream({start(controller){controller.enqueue(new Uint8Array([80,75]));setImmediate(()=>controller.error(Error('controlled partial body failure')))}})
 const bridge=binaryBridge(async()=>({response:new Response(stream,{headers:{'Content-Type':'application/zip'}}),isCurrent:()=>true}))
 bridge.click();await flush();await flush();bridge.start();for(let i=0;i<10;i++)await flush()
 assert.equal(bridge.downloads.length,0);assert.ok(bridge.events.some(event=>event.kind==='archive-http'&&event.request===7&&event.status===0));assert.equal(bridge.events.some(event=>event.kind==='archive-http'&&event.status===200),false)
})
test('binary status failures preserve401/403 and authority retirement aborts an in-flight body',async()=>{
 for(const status of [401,403]){const bridge=binaryBridge(async()=>({response:new Response('{}',{status,headers:{'Content-Type':'application/json'}}),isCurrent:()=>true}));bridge.click();await flush();await flush();bridge.start();for(let i=0;i<5;i++)await flush();assert.equal(bridge.downloads.length,0);assert.ok(bridge.events.some(event=>event.kind==='archive-http'&&event.status===status))}
 let controller,aborted=false
 const bridge=binaryBridge(async(_url,{signal})=>{signal.addEventListener('abort',()=>{aborted=true;controller.error(Error('aborted'))});return{response:new Response(new ReadableStream({start(value){controller=value}}),{headers:{'Content-Type':'application/zip'}}),isCurrent:()=>true}})
 bridge.click();await flush();await flush();bridge.start();await flush();bridge.command({stamp:'new-scope',nextOperation:2});for(let i=0;i<5;i++)await flush()
 assert.equal(aborted,true);assert.equal(bridge.downloads.length,0);assert.ok(bridge.events.some(event=>event.kind==='archive-http'&&event.stamp==='scope'&&event.status===0))
})


test('card viewer switches preserve pending Evidence acquisition while a full-session download owner switch retires it',async()=>{
 const acquisition=deferred();let aborted=0,picks=0
 const bridge=bridgeFixture(()=>{picks++;return Promise.resolve({createWritable:()=>acquisition.promise})})
 bridge.configure('task-one',1,{downloadStamp:'workspace-evidence'})
 bridge.click('workspace-evidence');await flush()
 for(const owner of ['task-two','closed','task-one'])bridge.configure(owner,2,{downloadStamp:'workspace-evidence',saveOperation:1})
 acquisition.resolve({async write(){},async close(){},async abort(){aborted++}});await flush();await flush()
 assert.equal(picks,1);assert.equal(aborted,0);assert.ok(bridge.events.some(event=>event.kind==='save-ready'&&event.stamp==='workspace-evidence'))
 bridge.configure('full-session-b',2,{downloadStamp:'full-session-b'})
 await flush();await flush();assert.equal(aborted,1)
})

test('a task Evidence ZIP body survives dock viewer changes and still returns its exact physical receipt',async()=>{
 let body
 const bridge=binaryBridge(async()=>({response:new Response(new ReadableStream({start(value){body=value}}),{headers:{'Content-Type':'application/zip'}}),isCurrent:()=>true}))
 bridge.command({stamp:'task-one',downloadStamp:'scope'});bridge.click();await flush();await flush();bridge.start();await flush()
 bridge.command({stamp:'task-two',downloadStamp:'scope',nextOperation:2,saveOperation:1});bridge.command({stamp:'closed',downloadStamp:'scope',nextOperation:2,saveOperation:1})
 const bytes=new Uint8Array([80,75,1,2]);body.enqueue(bytes);body.close();for(let i=0;i<8;i++)await flush()
 assert.equal(bridge.downloads.length,1);assert.deepEqual(new Uint8Array(await bridge.downloads[0].arrayBuffer()),bytes)
 assert.ok(bridge.events.some(event=>event.kind==='archive-http'&&event.stamp==='scope'&&event.request===7&&event.status===200))
})
