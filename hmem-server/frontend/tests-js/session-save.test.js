import assert from 'node:assert/strict'
import test from 'node:test'
import { createHash } from 'node:crypto'
import { createEvidenceWriter, installSessionBridge, updateLatestOverlay, createReaderScrollIntent, indexedCursorRank, indexedCursorKey } from '../src/session-save.js'
const digest = bytes => createHash('sha256').update(bytes).digest('hex')
const session = bytes => ({id:'s',workspace:'w',state:'terminated',finalBytes:bytes.length,finalSha256:digest(bytes),revision:7})
const chunk = (row,bytes,offset=0) => ({session:row,offset,nextOffset:offset+bytes.length,dataBase64:bytes.toString('base64')})

test('latest overlay requires manual upward intent and only mutates its own reader host',()=>{
 const attrs={},foreign={},host={setAttribute:(key,value)=>{attrs[key]=value}},other={setAttribute:(key,value)=>{foreign[key]=value}}
 const flags={},viewport={scrollTop:900,scrollHeight:1000,clientHeight:100,closest:()=>host,getAttribute:key=>flags[key]}
 updateLatestOverlay(viewport,false,false);assert.equal(attrs['data-latest-visible'],'false','initial mounted history does not arm the control')
 updateLatestOverlay(viewport,false,true);assert.equal(attrs['data-latest-visible'],'true')
 updateLatestOverlay(viewport,true,true);assert.equal(attrs['data-latest-visible'],'false')
 viewport.scrollTop=200;updateLatestOverlay(viewport,true,false);assert.equal(attrs['data-latest-visible'],'false','programmatic/layout movement is not manual intent')
 updateLatestOverlay(viewport,true,true);assert.equal(attrs['data-latest-visible'],'true')
 viewport.scrollTop=0;viewport.scrollHeight=80;updateLatestOverlay(viewport,true,true);assert.equal(attrs['data-latest-visible'],'false','a complete fitting conversation needs no control')
 flags['data-latest-pending']='true';updateLatestOverlay(viewport,true,false);assert.equal(attrs['data-latest-visible'],'false','pending acquisition does not arm the control')
 updateLatestOverlay(viewport,true,true);assert.equal(attrs['data-latest-visible'],'true','a manually historical pending request remains actionable')
 assert.deepEqual(foreign,{});assert.ok(other)
})

test('sealed resize follows only the matching latest tail and preserves historical or superseded positions',async()=>{
 for(const variant of ['latest','historical','manual','input','owner','load','page','render-request','render-part','lagging-dom','not-following','not-latest','stale-active-follow','stale-active-manual','stale-active-load']){
  const previous=globalThis.requestAnimationFrame,frames=[],listeners={},events=[];let incoming,resize
  const attrs={'data-viewer-owner':'owner','data-history-mode':'sealed','data-sealed-dom-owner':'owner','data-sealed-dom-load':'4','data-sealed-page-request':'10','data-render-request':'0','data-render-part':'6','data-following':'true','data-logical-latest':'true','data-history-after':'100','data-history-latest':'100','data-history-loading':'true'}
  const viewport={scrollTop:900,scrollHeight:1000,clientHeight:100,getAttribute:key=>attrs[key]??null,setAttribute:(key,value)=>{attrs[key]=value},querySelectorAll:()=>[],getBoundingClientRect:()=>({top:0,bottom:100,height:100}),contains:node=>node===viewport}
  const emit=(type,extra={})=>(listeners[type]||[]).forEach(fn=>fn({type,target:viewport,isTrusted:true,...extra}))
  globalThis.requestAnimationFrame=callback=>frames.push(callback)
  try{
   installSessionBridge({ports:{sessionBridgeCommand:{subscribe(fn){incoming=fn}},sessionBridgeEvent:{send:event=>events.push(event)}}},{document:{getElementById:()=>viewport,addEventListener(type,fn){(listeners[type]||=[]).push(fn)}},window:{ResizeObserver:class{constructor(fn){resize=fn}observe(){}disconnect(){}}}})
   if(variant.startsWith('stale-active'))attrs['data-history-mode']='active'
   incoming({stamp:'owner',viewerScope:'same-reader',generation:4,reset:null,chunk:null,follow:variant.startsWith('stale-active')})
   attrs['data-history-mode']='sealed'
   if(variant==='manual'||variant==='stale-active-manual'){
    attrs['data-history-mode']='active';emit('wheel',{deltaY:-100});viewport.scrollTop=800;emit('scroll');attrs['data-history-mode']='sealed';viewport.scrollTop=900
   }
   if(variant==='historical')viewport.scrollTop=400
   if(variant==='lagging-dom'||variant==='stale-active-load')attrs['data-sealed-dom-load']='3'
   if(variant==='not-following'||variant==='stale-active-follow')attrs['data-following']='false'
   if(variant==='not-latest')attrs['data-logical-latest']='false'
   resize() // Initial delivery captures the settled pre-resize reading state.
   const top=viewport.scrollTop
   if(variant==='input')emit('keydown',{key:'Shift'})
   if(variant==='owner')attrs['data-sealed-dom-owner']='retired'
   if(variant==='load')attrs['data-sealed-dom-load']='5'
   if(variant==='page')attrs['data-sealed-page-request']='11'
   if(variant==='render-request')attrs['data-render-request']='1'
   if(variant==='render-part')attrs['data-render-part']='7'
   viewport.scrollHeight=2000;resize();await flush()
   if(variant==='manual'||variant==='stale-active-manual')assert.ok(events.some(event=>event.kind==='viewer-follow'&&event.manualUp===true),'negative really starts in manual history')
   assert.equal(viewport.scrollTop,variant==='latest'?2000:top,variant)
  }finally{globalThis.requestAnimationFrame=previous}
 }
})

test('current disclosure actions replay ahead of publication and exact acknowledgements retire only their action',async()=>{
 const previous=globalThis.requestAnimationFrame,frames=[],events=[],listeners={},timers=new Map();let incoming,timerId=0,clock=0
 const attrs={'data-viewer-owner':'owner','data-viewer-generation':'3','data-history-mode':'active','data-history-loading':'true'},node={open:true,isConnected:true,getAttribute:key=>key==='data-group-key'?'kept-run':null}
 const viewport={scrollTop:20,scrollHeight:1000,clientHeight:100,getAttribute:key=>attrs[key]??null,contains:value=>value===node,querySelectorAll:selector=>selector==='details'?[node]:[],getBoundingClientRect:()=>({top:0,bottom:100,height:100})}
 globalThis.requestAnimationFrame=fn=>frames.push(fn)
 const emit=()=>{for(const fn of listeners.toggle||[])fn({type:'toggle',target:node})},actions=()=>events.filter(value=>value.kind==='viewer-disclosure')
 const command=(generation,extra={})=>incoming({stamp:'owner',viewerScope:'scope',generation,reset:null,chunk:null,...extra})
 const ack=(action,generation=action.generation,stamp=action.stamp)=>incoming({stamp,generation,disclosureAck:{scope:action.scope,request:action.request,namespace:action.namespace,key:action.key,ancestor:action.ancestor,open:action.open}})
 try{
  const bridge=installSessionBridge({ports:{sessionBridgeCommand:{subscribe(fn){incoming=fn}},sessionBridgeEvent:{send:value=>events.push(value)}}},{document:{getElementById:()=>viewport,addEventListener(type,fn){(listeners[type]||=[]).push(fn)}},window:{setTimeout(fn){timers.set(++timerId,fn);return timerId},clearTimeout(id){timers.delete(id)}},now:()=>clock})
  command(3);emit();await flush();const first=actions().at(-1);assert.equal(first.generation,3);assert.equal(timers.size,1)
  emit();await flush();assert.equal(actions().length,1,'duplicate native notification keeps its existing bounded action')
  const bytes=Buffer.from('{"text":"Accepted publication"}\n')
  command(4,{reset:0,chunk:{offset:0,nextOffset:bytes.length,dataBase64:bytes.toString('base64'),final:true}});await flush()
  const replay=actions().at(-1);assert.equal(replay.request,first.request);assert.equal(replay.generation,4)
  assert.ok(events.indexOf(replay)<events.findIndex(value=>value.kind==='viewer'&&value.generation===4),'current UI replay is queued before the next chunk publication')
  ack(first,3);command(5);await flush();assert.equal(actions().length,2,'an older accepted generation ACK retires the same action after its replay');assert.equal(timers.size,0)
  emit();await flush();const opened=actions().at(-1);node.open=false;emit();await flush();const closed=actions().at(-1);assert.notEqual(closed.request,opened.request)
  ack(opened);ack(closed,closed.generation,'retired');incoming({stamp:'owner',generation:5,disclosureAck:{...closed,scope:'retired-scope'}});command(6);await flush();assert.equal(actions().at(-1).request,closed.request,'old/opposite, foreign and different-scope ACKs cannot clear the latest close')
  ack(closed,5);command(7);await flush();assert.equal(actions().length,5);assert.equal(timers.size,0)
  emit();await flush();const current=actions().at(-1);ack(current,1000);command(8);await flush();assert.equal(actions().length,6,'ACK-only envelope cannot configure the reader or advance its generation')
  node.open=true;emit();await flush();node.isConnected=false;command(9);await flush();assert.equal(timers.size,0,'true node eviction cancels pending UI delivery')
  node.isConnected=true;emit();await flush();bridge.retireAuthority();command(10);await flush();assert.equal(timers.size,0,'authority retirement releases its timer and node references')
  const retiredCount=actions().length;emit();await flush();assert.equal(actions().length,retiredCount,'a still-connected retired authority cannot create another pending action')
  emit();command(11,{stamp:'replacement'});await flush();assert.equal(actions().at(-1).generation,9,'a queued old-owner action cannot publish after replacement')
 }finally{globalThis.requestAnimationFrame=previous}
})

test('unacknowledged disclosure delivery has finite attempt and time limits with one owned timer',async()=>{
 for(const limit of ['attempt','time','capacity']){
  const previous=globalThis.requestAnimationFrame,events=[],listeners={},timers=new Map();let incoming,timerId=0,clock=0
  const attrs={'data-viewer-owner':'owner','data-history-mode':'active','data-history-loading':'true'},nodes=[]
  const viewport={scrollTop:20,scrollHeight:1000,clientHeight:100,getAttribute:key=>attrs[key]??null,contains:node=>nodes.includes(node),querySelectorAll:()=>[],getBoundingClientRect:()=>({top:0,bottom:100,height:100})}
  globalThis.requestAnimationFrame=()=>{}
  try{
   installSessionBridge({ports:{sessionBridgeCommand:{subscribe(fn){incoming=fn}},sessionBridgeEvent:{send:value=>events.push(value)}}},{document:{getElementById:()=>viewport,addEventListener(type,fn){(listeners[type]||=[]).push(fn)}},window:{setTimeout(fn){timers.set(++timerId,fn);return timerId},clearTimeout(id){timers.delete(id)}},now:()=>clock})
   const command=generation=>incoming({stamp:'owner',viewerScope:'scope',generation,reset:null,chunk:null});command(1)
   const toggle=index=>{const node={open:true,isConnected:true,getAttribute:key=>key==='data-group-key'?'run-'+index:null};nodes.push(node);for(const fn of listeners.toggle||[])fn({type:'toggle',target:node})}
   toggle(0);await flush();assert.equal(timers.size,1)
   if(limit==='attempt'){for(let generation=2;generation<=9;generation++){command(generation);await flush()}assert.equal(events.filter(value=>value.kind==='viewer-disclosure').length,8)}
   else if(limit==='time'){clock=5000;[...timers.values()][0]();await flush()}
   else {for(let index=1;index<=400;index++)toggle(index);await flush();assert.ok(timers.size<=1)}
   if(limit==='capacity'){command(2);await flush();assert.equal(events.filter(value=>value.kind==='viewer-disclosure'&&value.generation===2).length,400,'only the bounded latest400 owned keys can replay')}
   else assert.equal(events.filter(value=>value.kind==='viewer-disclosure-error').length,1,'the bounded control phase begins after delivery exhausts')
  }finally{globalThis.requestAnimationFrame=previous}
 }
})

test('finite disclosure control failure exposes owned Retry and only explicit Retry rearms unchanged native state',async()=>{
 for(const proof of ['none','model-error','error-ack']){
  const previous=globalThis.requestAnimationFrame,events=[],listeners={},timers=new Map();let incoming,timerId=0,clock=0
  const attrs={'data-viewer-owner':'owner','data-history-mode':'active','data-history-loading':'true'},node={open:true,isConnected:true,getAttribute:key=>key==='data-group-key'?'kept-run':null},fallback={hidden:true,getAttribute:key=>key==='data-disclosure-error-owner'?'owner':key==='data-disclosure-error-scope'?'scope':null},host={querySelector:()=>fallback}
  const viewport={scrollTop:20,scrollHeight:1000,clientHeight:100,getAttribute:key=>attrs[key]??null,contains:value=>value===node,querySelectorAll:selector=>selector==='details'?[node]:[],getBoundingClientRect:()=>({top:0,bottom:100,height:100}),closest:()=>host}
  const command=(generation,extra={})=>incoming({stamp:'owner',viewerScope:'scope',generation,reset:null,chunk:null,...extra}),actions=()=>events.filter(value=>value.kind==='viewer-disclosure')
  globalThis.requestAnimationFrame=()=>{}
  try{
   installSessionBridge({ports:{sessionBridgeCommand:{subscribe(fn){incoming=fn}},sessionBridgeEvent:{send:value=>events.push(value)}}},{document:{getElementById:()=>viewport,addEventListener(type,fn){(listeners[type]||=[]).push(fn)}},window:{setTimeout(fn){timers.set(++timerId,fn);return timerId},clearTimeout(id){timers.delete(id)}},now:()=>clock})
   command(3);for(const fn of listeners.toggle||[])fn({type:'toggle',target:node});await flush();const first=actions()[0]
   clock=5000;[...timers.values()][0]();await flush();const error=events.find(value=>value.kind==='viewer-disclosure-error');assert.equal(error.request,first.request)
   if(proof==='model-error')command(3,{viewerError:true})
   if(proof==='error-ack')incoming({stamp:'owner',generation:3,disclosureAck:{scope:'scope',request:error.request,namespace:error.namespace,key:error.key,ancestor:error.ancestor,open:error.open,error:true}})
   if(timers.size){clock=10000;[...timers.values()][0]()};await flush();assert.equal(timers.size,0,'error delivery cannot spin after its finite control deadline')
   assert.equal(fallback.hidden,proof!=='none','without an authoritative model error/ACK, the ordinary owned fallback is visible')
   const before=actions().length;command(3,{viewerError:false});await flush();assert.equal(actions().length,before,'an unrelated clear is not user Retry');assert.equal(fallback.hidden,false,'unrelated error clearing cannot strand the still-failed desired action')
   incoming({stamp:'retired',viewerScope:'scope',generation:4,disclosureRetry:true});incoming({stamp:'owner',viewerScope:'retired',generation:4,disclosureRetry:true});await flush();assert.equal(actions().length,before)
   incoming({stamp:'owner',viewerScope:'scope',generation:4,disclosureRetry:true});await flush();assert.equal(actions().length,before,'control cannot advance data generation')
   command(4);await flush();assert.equal(node.open,true);assert.equal(fallback.hidden,true);assert.equal(actions().length,before+1);assert.ok(actions().at(-1).request>first.request,'explicit Retry creates a fresh delivery of the unchanged desired state')
   incoming({stamp:'owner',viewerScope:'scope',generation:4,disclosureRetry:true});command(4);await flush();assert.equal(actions().length,before+1,'duplicate control is consumed once')
  }finally{globalThis.requestAnimationFrame=previous}
 }
})

test('reader intent distinguishes actual pre-dispatch wheel, touch and scrollbar motion from owned positions',()=>{
 const viewport={scrollTop:600,scrollHeight:1000,clientHeight:200,offsetWidth:1000,clientWidth:985,contains:node=>node===viewport,getBoundingClientRect:()=>({right:1000})},intent=createReaderScrollIntent()
 intent.configure('["w","actor","session","scope",0]');assert.equal(intent.inspect(viewport,false),false)
 viewport.scrollTop=400;intent.positioned();intent.note({type:'wheel',target:viewport,isTrusted:true,deltaY:-40},viewport);assert.equal(intent.inspect(viewport,false),false,'owned prior movement is not attributed to a later stationary wheel')
 viewport.scrollTop=360;intent.note({type:'wheel',target:viewport,isTrusted:true,deltaY:-40},viewport);assert.equal(intent.inspect(viewport,false),true,'composited scrolling before wheel dispatch still counts as real movement')
 viewport.scrollTop=800;intent.clear();intent.inspect(viewport,true)
 intent.note({type:'touchstart',target:viewport,isTrusted:true,touches:[{clientY:100}]},viewport);viewport.scrollTop=750;intent.note({type:'touchmove',target:viewport,isTrusted:true,touches:[{clientY:150}]},viewport);assert.equal(intent.inspect(viewport,false),true)
 viewport.scrollTop=800;intent.clear();intent.inspect(viewport,true);intent.note({type:'pointerdown',target:viewport,isTrusted:true,clientX:995},viewport);viewport.scrollTop=740;assert.equal(intent.inspect(viewport,false),true)
 intent.configure('["different","actor","session","scope",0]');assert.equal(intent.inspect(viewport,false),false)
 intent.note({type:'keydown',target:viewport,isTrusted:false,key:'ArrowUp'},viewport);viewport.scrollTop=700;assert.equal(intent.inspect(viewport,false),false,'synthetic input cannot arm history')
 intent.note({type:'keydown',target:viewport,isTrusted:true,key:'ArrowUp'},viewport);viewport.scrollTop=660;assert.equal(intent.inspect(viewport,false),true)
 intent.configure('["different","actor","session","scope",9]');assert.equal(intent.manual(),true,'same reader authority retirement preserves manual history intent')
 intent.clear();viewport.scrollTop=600;intent.inspect(viewport,false);intent.note({type:'keydown',target:viewport,isTrusted:true,key:'Home'},viewport);viewport.scrollTop=0;intent.note({type:'toggle',target:viewport,isTrusted:true},viewport);assert.equal(intent.inspect(viewport,false),true,'a delayed native layout toggle cannot erase actual Home movement')
 intent.clear();intent.note({type:'toggle',target:viewport,isTrusted:true},viewport);assert.equal(intent.inspect(viewport,false),false,'toggle alone cannot arm manual history')
})

test('an armed virtual edge consumes only one fresh scoped directional action',async()=>{
 let clock=0;const attrs={'data-viewer-owner':'owner','data-render-part':'2'},viewport={scrollTop:600,scrollHeight:1000,clientHeight:200,contains:()=>true,getAttribute:name=>attrs[name]??null},intent=createReaderScrollIntent({now:()=>clock}),wheel=extra=>({type:'wheel',target:viewport,isTrusted:true,deltaY:-100,...extra})
 intent.configure('reader');intent.note(wheel(),viewport);viewport.scrollTop=0;assert.equal(intent.consumeEdge(viewport,-1),false,'an edge action cannot arm manual mode');intent.inspect(viewport,false);assert.equal(intent.manual(),true);intent.positioned()
 intent.note(wheel(),viewport);assert.equal(intent.consumeEdge(viewport,-1),false,'capture cannot grant credit before dispatch completes');await Promise.resolve();assert.equal(intent.consumeEdge(viewport,-1),true);assert.equal(intent.consumeEdge(viewport,-1),false,'owned acknowledgments cannot replay the action')
 const prevented=wheel();intent.note(prevented,viewport);prevented.defaultPrevented=true;await Promise.resolve();assert.equal(intent.consumeEdge(viewport,-1),false,'check final cancellation after dispatch')
 const inner={closest:selector=>selector.includes('pre,')?inner:null};intent.note(wheel({target:inner}),viewport);await Promise.resolve();assert.equal(intent.consumeEdge(viewport,-1),false,'inner pre owns its boundary input')
 for(const extra of [{ctrlKey:true},{metaKey:true},{shiftKey:true},{deltaX:200}]){intent.note(wheel(extra),viewport);await Promise.resolve();assert.equal(intent.consumeEdge(viewport,-1),false,'zoom or horizontal intent cannot move the outer document')}
 intent.note({type:'touchstart',target:viewport,isTrusted:true,touches:[{clientY:100}]},viewport);intent.note({type:'touchmove',target:viewport,isTrusted:true,touches:[{clientY:150},{clientY:200}]},viewport);await Promise.resolve();assert.equal(intent.consumeEdge(viewport,-1),false,'pinch does not mint outer navigation credit')
 intent.note(wheel(),viewport);intent.note({type:'keydown',target:viewport,isTrusted:true,key:'Shift'},viewport);await Promise.resolve();assert.equal(intent.consumeEdge(viewport,-1),false,'new nondirectional intent retires old credit')
 intent.note(wheel(),viewport);attrs['data-render-part']='1';await Promise.resolve();assert.equal(intent.consumeEdge(viewport,-1),false,'a prior window cannot consume the action')
 intent.note(wheel(),viewport);clock=501;await Promise.resolve();assert.equal(intent.consumeEdge(viewport,-1),false,'edge credit has a finite lifetime')
 intent.note(wheel(),viewport);intent.configure('different');await Promise.resolve();assert.equal(intent.consumeEdge(viewport,-1),false,'authority retirement clears credit')
})

test('composite sealed cursors retain exact blocks above Number safe packed ranks',()=>{
 const cursor='0009007199254740991:0000250',rank=indexedCursorRank(cursor)
 assert.ok(rank>BigInt(Number.MAX_SAFE_INTEGER));assert.equal(indexedCursorKey(rank),cursor)
 assert.equal(indexedCursorKey(rank-1n),'0009007199254740991:0000249');assert.equal(indexedCursorKey(rank+1n),'0009007199254740991:0000251')
 assert.ok(indexedCursorRank('0009007199254740990:9999999')<rank)
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

test('active publication waits for its exact mounted receipt and has a bounded missing-ack disposition',async()=>{
 for(const matching of [true,false]){
  const previous=globalThis.requestAnimationFrame,frames=[],events=[];let incoming
  const attrs={'data-viewer-owner':'owner','data-viewer-generation':'3','data-history-mode':'active','data-window-publication':'0','data-history-loading':'true'},viewport={scrollTop:35,scrollHeight:1000,clientHeight:100,getAttribute:key=>attrs[key] ?? null,getBoundingClientRect:()=>({top:0,bottom:100,height:100}),querySelectorAll:()=>[]}
  globalThis.requestAnimationFrame=callback=>frames.push(callback)
  const paint=()=>frames.splice(0).forEach(callback=>callback())
  try{
   installSessionBridge({ports:{sessionBridgeCommand:{subscribe(fn){incoming=fn}},sessionBridgeEvent:{send:value=>events.push(value)}}},{document:{getElementById:()=>viewport,addEventListener(){}},window:{},now:()=>0})
   const bytes=Buffer.from('{"text":"One accepted source occurrence"}\n')
   incoming({stamp:'owner',generation:3,reset:0,chunk:{offset:0,nextOffset:bytes.length,dataBase64:bytes.toString('base64'),final:true},direction:'tail',follow:false,publication:{request:0,pending:false}});await flush()
   const publication=events.find(event=>event.kind==='viewer').publication;paint();paint();await flush()
   assert.equal(events.some(event=>event.kind==='viewer-positioned'),false,'an old DOM cannot release the new publication')
   if(matching)attrs['data-window-publication']=String(publication)
   for(let frame=0;frame<12;frame++)paint();await flush()
   assert.deepEqual(events.filter(event=>event.kind==='viewer-positioned').map(event=>({publication:event.publication,generation:event.generation,failed:event.failed})),[{publication,generation:3,failed:!matching}])
   assert.equal(viewport.scrollTop,35,'the logical acknowledgment does not invent user movement')
  }finally{globalThis.requestAnimationFrame=previous}
 }
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
  incoming({stamp:'owner',generation:2,reset:null,chunk:null,direction:'tail',follow:true});emit('scroll');await flush();assert.equal(latestFollow().manualUp,false,'a generation handoff cannot invent manual upward intent')
  incoming({stamp:'owner',generation:2,reset:null,chunk:{offset:bytes.length,nextOffset:bytes.length*2,dataBase64:bytes.toString('base64'),final:true},direction:'tail',follow:true});paint();await flush()
  emit('wheel',{deltaY:-300});viewport.scrollTop=500;emit('scroll');await flush();assert.equal(latestFollow().following,false,'real above-tail intent pauses follow')
  incoming({stamp:'owner',generation:2,reset:null,chunk:null,direction:'tail',follow:false});viewport.scrollTop=900;attrs['data-history-loading']='true';emit('keydown',{key:'End'});emit('scroll');paint();await flush();assert.equal(latestFollow().manualUp,true,'End retains the manual latch until matching latest acknowledgment');assert.ok(events.some(event=>event.kind==='viewer-end'&&event.stamp==='owner'&&event.generation===2),'real scoped End seeks the logical document end after its publication frame')
  incoming({stamp:'other',generation:3,reset:null,chunk:null,direction:'tail',follow:true});attrs['data-viewer-owner']='other';attrs['data-history-loading']='false';emit('scroll');await flush();assert.equal(latestFollow().manualUp,false,'a new selection starts following without inherited manual intent')
 }finally{globalThis.requestAnimationFrame=previous}
})

for(const action of ['End','up','historical','generation','owner','programmatic'])test(`pre-first-frame tail handoff preserves only current following End intent: ${action}`,async()=>{
 const previous=globalThis.requestAnimationFrame,frames=[],events=[],listeners=new Map();let incoming,top=0
 const attrs={'data-viewer-owner':'owner','data-history-mode':'active','data-history-before':'0','data-history-after':'64','data-history-latest':'192','data-history-loading':'true'}
 const viewport={scrollHeight:100,clientHeight:100,get scrollTop(){return top},set scrollTop(value){top=Math.max(0,Math.min(this.scrollHeight-this.clientHeight,value))},getAttribute:key=>attrs[key] ?? null}
 const emit=(type,extra={})=>(listeners.get(type)||[]).forEach(handler=>handler({type,target:viewport,...extra}))
 globalThis.requestAnimationFrame=callback=>frames.push(callback)
 const paint=()=>{for(let turn=0;turn<8&&frames.length;turn++)frames.splice(0).forEach(callback=>callback())}
 try{
  installSessionBridge({ports:{sessionBridgeCommand:{subscribe(fn){incoming=fn}},sessionBridgeEvent:{send:value=>events.push(value)}}},{document:{getElementById:()=>viewport,addEventListener(type,handler){listeners.set(type,[...(listeners.get(type)||[]),handler])}},window:{},now:()=>0})
  const bytes=Buffer.from('{"text":"First usable page"}\n'),following=action!=='historical'
  const command=extra=>incoming({stamp:'owner',generation:1,reset:null,chunk:null,direction:'tail',follow:following,...extra})
  command({reset:0,chunk:{offset:0,nextOffset:bytes.length,dataBase64:bytes.toString('base64'),final:true}})
  viewport.scrollHeight=1000
  // No positioning callback has run. End arrives before the next usable
  // chunk grows the DOM, and its inspection also precedes the tail RAF.
  emit('keydown',{key:'End'})
  command({chunk:{offset:bytes.length,nextOffset:bytes.length*2,dataBase64:bytes.toString('base64'),final:true}})
  viewport.scrollHeight=1800
  if(action==='up'||action==='historical'){viewport.scrollTop=600;emit('scroll');emit('wheel',{deltaY:-300});viewport.scrollTop=300}
  if(action==='programmatic')viewport.scrollTop=300
  if(action==='generation')command({generation:2})
  if(action==='owner'){attrs['data-viewer-owner']='other';command({stamp:'other',generation:2})}
  emit('scroll');await flush()
  const manual=events.filter(event=>event.kind==='viewer-follow').at(-1)?.manualUp
  if(action==='up'||action==='historical')assert.equal(manual,true,'actual upward movement arms history')
  else assert.notEqual(manual,true,'a pending End publication or layout handoff cannot invent manual history')
  const before=viewport.scrollTop;paint();await flush()
  assert.equal(viewport.scrollTop,action==='End'?1700:before,'retired or historical work cannot reposition the reader')
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

test('queued disclosure restoration yields to newer summary intent and retired publication authority',async()=>{
 const previous=globalThis.requestAnimationFrame
 try{
  for(const action of ['pointer','pending-toggle','keyboard','toggle','handoff','lagging-dom','owner','generation','owned-restore']){
   const frames=[],listeners={},events=[];let incoming
   globalThis.requestAnimationFrame=callback=>frames.push(callback)
   const attrs={'data-viewer-owner':'owner','data-viewer-generation':'3','data-history-mode':'active','data-window-publication':'0','data-history-loading':'true'}
   const detail={open:false,getAttribute:name=>name==='data-group-key'?'kept-run':null,getBoundingClientRect:()=>({top:20,bottom:40,height:20})},summary={closest:selector=>selector.includes('summary')?summary:null}
   const viewport={scrollTop:35,scrollHeight:1000,clientHeight:100,getAttribute:name=>attrs[name]??null,getBoundingClientRect:()=>({top:0,bottom:100,right:800,height:100}),querySelectorAll:selector=>selector==='details'?[detail]:[],contains:node=>node===detail||node===summary}
   installSessionBridge({ports:{sessionBridgeCommand:{subscribe(fn){incoming=fn}},sessionBridgeEvent:{send:value=>events.push(value)}}},{document:{getElementById:()=>viewport,addEventListener(name,fn){(listeners[name]??=[]).push(fn)}},window:{},now:()=>0})
   const bytes=Buffer.from('{"text":"One accepted publication"}\n')
   incoming({stamp:'owner',viewerScope:'source',generation:3,reset:0,chunk:{offset:0,nextOffset:bytes.length,dataBase64:bytes.toString('base64'),final:true},direction:'tail',follow:false,publication:{request:0,pending:false}});await flush()
   const publication=events.find(event=>event.kind==='viewer').publication;attrs['data-window-publication']=String(publication)
   if(action==='lagging-dom')incoming({stamp:'owner',viewerScope:'source',generation:4,reset:null,chunk:null,follow:false,publication:{request:publication,pending:true}})
   if(['pointer','pending-toggle','handoff'].includes(action))for(const listener of listeners.pointerdown)listener({type:'pointerdown',target:summary,isTrusted:true,clientX:100})
   if(action==='keyboard')for(const listener of listeners.keydown)listener({type:'keydown',target:summary,isTrusted:true,key:'Enter'})
   detail.open=true
   if(['pointer','keyboard','toggle','handoff','lagging-dom'].includes(action))for(const listener of listeners.toggle)listener({type:'toggle',target:detail,isTrusted:true})
   if(['handoff','owner','generation'].includes(action)){attrs['data-viewer-generation']='4';if(action==='owner')attrs['data-viewer-owner']='other';incoming({stamp:action==='owner'?'other':'owner',viewerScope:'source',generation:4,reset:null,chunk:null,follow:false,publication:{request:publication,pending:action==='handoff'}})}
   for(let turn=0;turn<12;turn++)frames.splice(0).forEach(callback=>callback());await flush()
   assert.equal(detail.open,action!=='owned-restore',action+' must preserve only the current disclosure owner and intent')
   if(action==='pending-toggle')for(const listener of listeners.toggle)listener({type:'toggle',target:detail,isTrusted:true})
   if(['pointer','keyboard','toggle','handoff'].includes(action))assert.ok(events.some(event=>event.kind==='viewer-disclosure'&&event.key==='kept-run'&&event.open))
   if(action==='lagging-dom')assert.ok(events.some(event=>event.kind==='viewer-disclosure'&&event.key==='kept-run'&&event.open&&event.generation===4),'current same-owner input uses accepted generation while its DOM is still publishing')
  }
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

test('a tall side-cell marker cannot skip intervening cached presentation slices in either direction',async()=>{
 for(const direction of [-1,1,'Home','Home late toggle','cancelled Home','cancelled Home late toggle'])for(const admittedParts of [[],[39,40,41],[40]]){
  const previous=globalThis.requestAnimationFrame,frames=[],events=[],listeners={};let incoming
  const attrs={'data-viewer-owner':'owner','data-history-mode':'active','data-history-before':'0','data-history-after':'1000','data-history-latest':'1000','data-history-loading':'true','data-render-part':'40','data-render-more':'true'}
  const backward=direction<0||typeof direction==='string',gap={dataset:{fragmentFrom:backward?'0':'99',fragmentTo:backward?'0':'99'},getBoundingClientRect:()=>({top:-300000,bottom:300000,height:600000})}
  const viewport={scrollTop:800,scrollHeight:1000,clientHeight:100,dataset:{get renderPart(){return attrs['data-render-part']}},getBoundingClientRect:()=>({top:0,bottom:100,height:100}),getAttribute:key=>attrs[key]??null,setAttribute:(key,value)=>{attrs[key]=value},querySelectorAll:selector=>selector==='[data-fragment-from]'?[gap]:selector==='[data-fragment-part]'?admittedParts.map(part=>({dataset:{fragmentPart:String(part)},querySelector:()=>null})):[]}
  globalThis.requestAnimationFrame=callback=>frames.push(callback)
  const emit=(type,deltaY,key)=>(listeners[type]||[]).forEach(handler=>handler({type,target:viewport,deltaY,key,isTrusted:true}))
  try{
   installSessionBridge({ports:{sessionBridgeCommand:{subscribe(fn){incoming=fn}},sessionBridgeEvent:{send:event=>events.push(event)}}},{document:{getElementById:()=>viewport,addEventListener(type,handler){(listeners[type]??=[]).push(handler)}},window:{},now:()=>0})
   incoming({stamp:'owner',generation:1,reset:null,chunk:null,follow:false});emit('wheel',-100);viewport.scrollTop=700;emit('scroll');await flush()
   if(typeof direction==='string')emit('keydown',0,'Home')
   if(typeof direction==='string'&&direction.startsWith('cancelled Home'))emit('keydown',0,'PageUp')
   if(typeof direction==='string'&&direction.endsWith('late toggle'))emit('toggle')
   attrs['data-history-loading']='false';if(typeof direction==='number')emit('wheel',100*direction);viewport.scrollTop+=backward?-10:10;emit('scroll');await flush()
   const explicitHome=typeof direction==='string'&&direction.startsWith('Home')
   if(!explicitHome){
    assert.equal(events.filter(event=>event.kind==='viewer-render').length,0,'a near side column cannot admit an unrelated unmounted frontier')
    if(backward)gap.dataset.fragmentTo=String(Math.min(40,...admittedParts)-1)
    else gap.dataset.fragmentFrom=String(Math.max(40,...admittedParts)+1)
    emit('scroll');await flush()
   }
   assert.deepEqual(events.filter(event=>event.kind==='viewer-render').map(event=>event.part),[explicitHome?0:backward?39:41],'only a matching explicit Home seeks directly; ordinary/stale Home admits the adjacent slice')
   attrs['data-viewer-owner']='retired';emit('wheel',-100);emit('scroll');await flush();assert.equal(events.filter(event=>event.kind==='viewer-render').length,1,'retired DOM cannot advance presentation')
  }finally{globalThis.requestAnimationFrame=previous}
 }
})

test('a visible center leaf survives neighbor trimming and owned positioning cannot drive another presentation',async()=>{
 const previous=globalThis.requestAnimationFrame,frames=[],events=[],listeners={};let incoming,shift=0,admitted=[39,40,41]
 const attrs={'data-viewer-owner':'owner','data-history-mode':'active','data-history-before':'0','data-history-after':'1000','data-history-latest':'1000','data-history-loading':'true','data-render-part':'40','data-render-more':'true'}
 let selection=null;const selectedToken={}
 const leaves=[39,40,41].map(part=>({isConnected:true,dataset:{fragmentPart:String(part),fragmentPath:'same-row:'+part},contains:node=>part===39&&node===selectedToken,querySelector:()=>null,closest:()=>null,getBoundingClientRect:()=>({top:720+shift-viewport.scrollTop,bottom:740+shift-viewport.scrollTop,height:20})}))
 const gap={dataset:{fragmentFrom:'42',fragmentTo:'99'},getBoundingClientRect:()=>({top:20,bottom:600000,height:599980})}
 const viewport={ownerDocument:{getSelection:()=>selection},scrollTop:800,scrollHeight:2000,clientHeight:100,dataset:{get renderPart(){return attrs['data-render-part']}},getBoundingClientRect:()=>({top:0,bottom:100,height:100}),getAttribute:key=>attrs[key]??null,setAttribute:(key,value)=>{attrs[key]=value},querySelectorAll:selector=>selector==='[data-fragment-from]'?[gap]:selector==='[data-fragment-path]'?leaves.filter(leaf=>leaf.isConnected):selector==='[data-fragment-part]'?admitted.map(part=>({dataset:{fragmentPart:String(part)},querySelector:()=>null})):[]}
 globalThis.requestAnimationFrame=callback=>frames.push(callback)
 const emit=(type,deltaY)=>(listeners[type]||[]).forEach(handler=>handler({type,target:viewport,deltaY,isTrusted:true}))
 try{
  installSessionBridge({ports:{sessionBridgeCommand:{subscribe(fn){incoming=fn}},sessionBridgeEvent:{send:event=>events.push(event)}}},{document:{getElementById:()=>viewport,addEventListener(type,handler){(listeners[type]??=[]).push(handler)}},window:{},now:()=>0})
  incoming({stamp:'owner',generation:1,reset:null,chunk:null,follow:false});emit('wheel',-100);viewport.scrollTop=700;emit('scroll');await flush();attrs['data-history-loading']='false';selection={isCollapsed:false,anchorNode:selectedToken};emit('wheel',100);viewport.scrollTop=710;emit('scroll');await flush();assert.equal(events.filter(event=>event.kind==='viewer-render').length,0,'prefetch cannot replace the selected visible neighbor with another occurrence');selection=null;admitted=[40];gap.dataset.fragmentFrom='41';emit('scroll');await flush();assert.equal(events.filter(event=>event.kind==='viewer-render').length,0,'an adaptive single-part window keeps its visible reading center');admitted=[39,40,41];gap.dataset.fragmentFrom='42';emit('scroll');await flush()
  assert.equal(events.filter(event=>event.kind==='viewer-render').at(-1).part,41)
  leaves[0].isConnected=false;shift=80;admitted=[40,41,42];gap.dataset.fragmentFrom='43';attrs['data-render-part']='41'
  for(let frame=0;frame<5;frame++)frames.splice(0).forEach(callback=>callback());await flush()
  assert.equal(viewport.scrollTop,790,'restore the same prepublication center leaf, not the removed oldest neighbor')
  assert.equal(leaves[1].getBoundingClientRect().top,10)
  assert.equal(events.filter(event=>event.kind==='viewer-render').length,1,'the owned restore cannot create a feedback chain')
  viewport.scrollHeight=800;viewport.scrollTop=700;emit('scroll');await flush();assert.equal(events.filter(event=>event.kind==='viewer-render').length,1,'a provable layout clamp is not continued movement')
  viewport.scrollHeight=2000;viewport.scrollTop=720;emit('scroll');await flush();assert.equal(events.filter(event=>event.kind==='viewer-render').at(-1).part,42,'continued native displacement in the same gesture remains admitted')
 }finally{globalThis.requestAnimationFrame=previous}
})

test('a render acknowledgement cannot undo continued native Home movement within the same input intent',async()=>{
 for(const phase of ['motion','clamp','motion with geometry']){
 const publicationClamp=phase==='clamp'
 const previous=globalThis.requestAnimationFrame,frames=[],events=[],listeners={};let incoming,leafTop=620
 const attrs={'data-viewer-owner':'owner','data-history-mode':'active','data-history-before':'100','data-history-after':'1000','data-history-latest':'1000','data-history-loading':'false','data-render-part':'1','data-render-more':'true'}
 const row={dataset:{offset:'200',blockKey:'200:0'},getAttribute:key=>key==='data-offset'?'200':null,getBoundingClientRect:()=>({top:600-viewport.scrollTop,bottom:1000-viewport.scrollTop,height:400})}
 const leaf={isConnected:true,dataset:{fragmentPath:'0000000000000000200:0000000:content:0',fragmentPart:'1'},querySelector:()=>null,closest:()=>row,getBoundingClientRect:()=>({top:leafTop-viewport.scrollTop,bottom:leafTop+30-viewport.scrollTop,height:30})}
 const gap={dataset:{fragmentFrom:'0',fragmentTo:'0'},getBoundingClientRect:()=>({top:10,bottom:30,height:20})}
 const viewport={scrollTop:800,scrollHeight:1000,clientHeight:100,dataset:{get renderPart(){return attrs['data-render-part']}},getBoundingClientRect:()=>({top:0,bottom:100,height:100}),getAttribute:key=>attrs[key] ?? null,setAttribute:(key,value)=>{attrs[key]=value},querySelectorAll:selector=>selector==='[data-fragment-from]'?[gap]:selector==='[data-fragment-path]'?[leaf]:selector==='[data-fragment-part]'?[{dataset:{fragmentPart:'0'}},{dataset:{fragmentPart:'1'}}]:selector==='.session-record,[data-group-key]'||selector==='.session-record'?[row]:[]}
 globalThis.requestAnimationFrame=callback=>frames.push(callback)
 const emit=type=>(listeners[type] || []).forEach(handler=>handler({type,target:viewport,key:phase==='motion'?'Home':'PageUp',isTrusted:true}))
 try{
  installSessionBridge({ports:{sessionBridgeCommand:{subscribe(fn){incoming=fn}},sessionBridgeEvent:{send:event=>events.push(event)}}},{document:{getElementById:()=>viewport,addEventListener(type,handler){(listeners[type]??=[]).push(handler)}},window:{},now:()=>0})
  incoming({stamp:'owner',viewerScope:'owner',generation:1,reset:null,chunk:null,follow:false})
  emit('keydown');viewport.scrollTop=600;emit('scroll');await flush()
  assert.equal(events.filter(event=>event.kind==='viewer-render').length,1,'an intermediate Home frame starts a presentation refill')
  if(publicationClamp){viewport.scrollHeight=600;viewport.scrollTop=500;leafTop=510}else {viewport.scrollTop=0;if(phase==='motion with geometry')leafTop=820}
  attrs['data-render-part']='0'
  for(let turn=0;turn<5;turn++)frames.splice(0).forEach(callback=>callback())
  assert.equal(viewport.scrollTop,publicationClamp?490:phase==='motion with geometry'?200:0,publicationClamp?'a provable publication clamp still restores the visible text displacement':phase==='motion with geometry'?'restore only prefix geometry while preserving the continued input displacement':'a genuine Home keeps its start intent through continued movement')
 }finally{globalThis.requestAnimationFrame=previous}
 }
})

test('task/full presentation restoration yields to newer scroll, disclosure and owner intent',async()=>{
 const previous=globalThis.requestAnimationFrame
 try{
  for(const continuing of [true,false])for(const change of ['none','programmatic','programmatic-changed','up','down','toggle','click','owner']){
   const frames=[],listeners={},events=[];let incoming,viewport
   globalThis.requestAnimationFrame=callback=>frames.push(callback)
   const node=()=>({open:false,getAttribute:key=>key==='data-group-key'?'kept':null,closest(selector){return selector==='summary'?this:null}})
   const view=(owner,top=0)=>({scrollTop:top,querySelectorAll:()=>[disclosure],getAttribute:key=>key==='data-viewer-owner'?owner:key==='data-window-publication'?'8':null,contains:target=>target===disclosure})
   let disclosure=node();viewport=view('dock',40);disclosure.open=true
   const document={addEventListener(name,fn){(listeners[name]??=[]).push(fn)},getElementById:()=>viewport}
   installSessionBridge({ports:{sessionBridgeCommand:{subscribe(fn){incoming=fn}},sessionBridgeEvent:{send:value=>events.push(value)}}},{document,window:{},now:()=>0})
   incoming({stamp:'dock',viewerScope:'authorized-source',generation:1,reset:0,chunk:null})
   for(const listener of listeners.scroll)listener({type:'scroll',target:viewport})
   disclosure=node();viewport=view('full')
   incoming({stamp:'full',viewerScope:'authorized-source',preservePresentation:true,continueViewer:continuing,generation:2,reset:continuing?null:0,chunk:null,publication:{request:8,pending:!continuing}})
   assert.equal(frames.length,1)
   if(change==='up')viewport.scrollTop=-20
   if(change==='down')viewport.scrollTop=20
   if(change==='toggle')for(const listener of listeners.pointerdown)listener({type:'pointerdown',target:disclosure,isTrusted:true})
   if(change==='click')for(const listener of listeners.click)listener({type:'click',target:disclosure,isTrusted:true})
   if(['toggle','click','programmatic','programmatic-changed'].includes(change)){disclosure.open=change!=='programmatic-changed';for(const listener of listeners.toggle)listener({type:'toggle',target:disclosure})}
   if(change==='owner')incoming({stamp:'retired',viewerScope:'new-authority',generation:3,reset:null,chunk:null})
   frames.shift()()
   await flush()
   assert.equal(viewport.scrollTop,['none','programmatic'].includes(change)?40:change==='up'?-20:change==='down'?20:0)
   assert.equal(disclosure.open,['none','programmatic','toggle','click'].includes(change))
   const settlements=events.filter(value=>['viewer-positioned','viewer-handoff-cancelled'].includes(value.kind))
   if(continuing||change==='owner')assert.equal(settlements.length,0)
   else assert.deepEqual(settlements.map(({kind,stamp,generation,publication})=>({kind,stamp,generation,publication})),[{kind:['none','programmatic'].includes(change)?'viewer-positioned':'viewer-handoff-cancelled',stamp:'full',generation:2,publication:8}],'pending transport must get an exact truthful success/cancellation disposition')
  }
 }finally{globalThis.requestAnimationFrame=previous}
})

test('handoff positioning fences the earlier continuity frame and has a finite missing-owner disposition',async()=>{
 const previous=globalThis.requestAnimationFrame
 try{for(const missing of [false,true,'input','movement']){
  const frames=[],listeners={},events=[];let incoming
  globalThis.requestAnimationFrame=callback=>frames.push(callback)
  const attrs={'data-viewer-owner':'dock','data-viewer-generation':'1','data-history-mode':'active','data-history-before':'100','data-history-after':'1000','data-history-latest':'1000','data-history-loading':'true','data-render-part':'0','data-render-more':'false','data-window-publication':'8'}
  const detail={open:true,getAttribute:name=>name==='data-group-key'?'kept':null}
  let viewport={scrollTop:40,scrollHeight:1000,clientHeight:200,getAttribute:name=>attrs[name]??null,setAttribute(){},getBoundingClientRect:()=>({top:0,bottom:200,left:0,right:800,height:200}),querySelectorAll:selector=>selector==='details'?[detail]:[],contains:node=>node===detail}
  const document={getElementById:()=>viewport,addEventListener(name,callback){(listeners[name]??=[]).push(callback)}}
  installSessionBridge({ports:{sessionBridgeCommand:{subscribe(callback){incoming=callback}},sessionBridgeEvent:{send:value=>events.push(value)}}},{document,window:{}})
  incoming({stamp:'dock',viewerScope:'same',generation:1,chunk:null,reset:0,follow:false})
  for(const callback of listeners.scroll)callback({type:'scroll',target:viewport})
  viewport={...viewport,scrollTop:0};attrs['data-viewer-owner']=missing===true?'not-mounted':'full';attrs['data-viewer-generation']='2';attrs['data-history-loading']='false'
  incoming({stamp:'full',viewerScope:'same',preservePresentation:true,continueViewer:false,generation:2,chunk:null,reset:0,follow:false,publication:{request:8,pending:true}})
  if(missing==='input')for(const callback of listeners.keydown)callback({type:'keydown',target:viewport,key:'Shift',isTrusted:true})
  if(missing==='movement')viewport.scrollTop=20
  if(typeof missing==='string')for(const callback of listeners.scroll)callback({type:'scroll',target:viewport})
  frames.shift()();await flush()
  if(typeof missing!=='string')assert.ok(!events.some(value=>['viewer-render','viewer-boundary'].includes(value.kind)),'the already-queued continuity frame cannot acquire before positioning')
  for(let step=0;frames.length&&step<20;step++){frames.shift()();await flush()}
  if(missing===true){assert.equal(events.filter(value=>value.kind==='viewer-handoff-error').length,1);assert.equal(frames.length,0)}else if(typeof missing==='string'){assert.deepEqual(events.filter(value=>value.kind==='viewer-handoff-cancelled').map(({stamp,generation,publication})=>({stamp,generation,publication})),[{stamp:'full',generation:2,publication:8}]);assert.equal(events.filter(value=>value.kind==='viewer-positioned').length,0);assert.equal(viewport.scrollTop,missing==='movement'?20:0)}else assert.equal(viewport.scrollTop,40)
 }}finally{globalThis.requestAnimationFrame=previous}
})

test('pending presentation without a mounted snapshot fails with an exact finite logical disposition',async()=>{
 const previous=globalThis.requestAnimationFrame,frames=[],events=[];let incoming
 globalThis.requestAnimationFrame=callback=>frames.push(callback)
 try{
  installSessionBridge({ports:{sessionBridgeCommand:{subscribe(callback){incoming=callback}},sessionBridgeEvent:{send:value=>events.push(value)}}},{document:{getElementById:()=>null,addEventListener(){}},window:{}})
  incoming({stamp:'dock',viewerScope:'same',generation:1,chunk:null,reset:0})
  incoming({stamp:'full',viewerScope:'same',generation:2,preservePresentation:true,continueViewer:false,chunk:null,reset:0,publication:{request:8,pending:true}})
  await flush();assert.deepEqual(events.filter(value=>value.kind==='viewer-handoff-error').map(({stamp,generation,publication})=>({stamp,generation,publication})),[{stamp:'full',generation:2,publication:8}]);assert.equal(events.some(value=>value.kind==='viewer-positioned'),false);assert.equal(frames.length,0)
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
