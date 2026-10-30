import test from 'node:test'
import assert from 'node:assert/strict'
import {gzipSync} from 'node:zlib'
import {createHash} from 'node:crypto'
import {createArchiveFramer,archiveChunkOffset,createArchiveEngine,gzipPipeline} from '../src/session-archive-worker.js'
import {createRecordFramer,semanticTransition,prettyPayloadSlice,presentation,visibleBlocks,payloadTokens} from '../src/session-viewer.js'
import {createSealedViewer} from '../src/session-save.js'

const phaseFixtureETag='"fixture-authority-bound-archive"'
// Fake transports still execute both production bridge incarnations. Disk and
// decoder ownership is tested separately against the real store/native Worker.
function createPhaseBridgeFixture(options) {
  let stage
  return createSealedViewer({...options,custodyFactory:async()=>async()=>{},deleteStage:async()=>{},
    async fetchArchive(url,request){
      if(request.headers?.['If-None-Match']){assert.equal(request.headers['If-None-Match'],phaseFixtureETag);return {isCurrent:()=>true,response:new Response(null,{status:304,headers:{ETag:phaseFixtureETag}})}}
      const result=await options.fetchArchive(url,request)
      if(result.response.headers.get('Content-Type')?.startsWith('application/gzip'))result.response.headers.set('ETag',phaseFixtureETag)
      return result
    },
    workerFactory(){
      const worker=options.workerFactory(),post=worker.postMessage.bind(worker)
      worker.postMessage=(message,transfer)=>{
        if(message.kind==='start')stage={name:message.name,custody:message.custody,manifest:message.manifest}
        if(message.kind==='detach'&&!worker.phaseUsesEngine){queueMicrotask(()=>worker.onmessage?.({data:{...message.stamp,request:message.request,kind:'detached',stage}}));return}
        post(message,transfer)
      }
      return worker
    }
  })
}

const digest=value=>createHash('sha256').update(value).digest('hex')
const workspace='11111111-1111-1111-1111-111111111111',session='22222222-2222-2222-2222-222222222222',generation='33333333-3333-3333-3333-333333333333'
const manifest=(raw,gzip)=>({workspace_id:workspace,session_id:session,state:'ready',generation_id:generation,codec:'gzip-v1',raw_bytes:String(raw.length),raw_records:String(raw.reduce((count,byte)=>count+(byte===10?1:0),0)+(raw.length&&raw.at(-1)!==10?1:0)),raw_sha256:digest(raw),compressed_bytes:String(gzip.length),compressed_sha256:digest(gzip),content_path:`/api/v1/workspaces/${workspace}/sessions/${session}/archive/${generation}/content`})

test('one native gzip context receives bounded compressed writes across input boundaries',async()=>{
  let seed=0x51a7
  const entropy=Array.from({length:32768},()=>{seed^=seed<<13;seed^=seed>>>17;seed^=seed<<5;return String.fromCharCode(33+(seed>>>0)%90)}).join('')
  const raw=Buffer.from(JSON.stringify({text:entropy+'λ😀'})+'\r\n'),gzip=gzipSync(raw)
  assert.ok(gzip.length>4096)
  const Native=globalThis.DecompressionStream,writes=[];let contexts=0,reader,pipeline,completion,cleanup
  globalThis.DecompressionStream=class {
    constructor(codec){contexts++;const decoder=new Native(codec);return {readable:decoder.readable,writable:{getWriter(){const writer=decoder.writable.getWriter();return {write(bytes){writes.push(bytes.length);return writer.write(bytes)},close:()=>writer.close(),abort:error=>writer.abort(error),releaseLock:()=>writer.releaseLock()}}}}}
  }
  try{
    const source=new ReadableStream({start(controller){controller.enqueue(gzip.subarray(0,5003));controller.enqueue(gzip.subarray(5003));controller.close()}})
    pipeline=gzipPipeline(source);completion=pipeline.completion.then(()=>({}),error=>({error}));cleanup=pipeline.cleanup.then(()=>({}),error=>({error}))
    reader=pipeline.readable.getReader();const hash=createHash('sha256');let bytes=0
    while(true){const part=await reader.read();if(part.done)break;hash.update(part.value);bytes+=part.value.length}
    assert.deepEqual(await completion,{});assert.deepEqual(await cleanup,{})
    assert.equal(contexts,1);assert.ok(writes.length>1);assert.ok(writes.every(length=>length>0&&length<=4096));assert.equal(writes.reduce((sum,size)=>sum+size,0),gzip.length)
    assert.equal(bytes,raw.length);assert.equal(hash.digest('hex'),digest(raw))
  }finally{reader?.releaseLock();globalThis.DecompressionStream=Native}
})
test('strict archive framing preserves UTF8/CRLF/final records across every cut',()=>{
  const input=Buffer.from('{"text":"λ😀"}\r\n{"text":"last"}')
  for(let cut=0;cut<=input.length;cut++){
    const framer=createArchiveFramer(),rows=[...framer.push(input.subarray(0,cut)),...framer.push(input.subarray(cut),true)]
    assert.equal(rows.length,2);assert.equal(rows[0].delimiter,'\r\n');assert.equal(rows[1].delimiter,'');assert.equal(framer.through,BigInt(input.length))
    assert.equal(rows[1].offset,String(input.indexOf(10)+1))
  }
  for(const input of [Buffer.from('{'),Buffer.from([34,0xc3,34]),Buffer.from('\n')])assert.throws(()=>[...createArchiveFramer().push(input,true)])
})

test('chunk-local framing retains exact absolute arithmetic across numeric boundaries',()=>{
  assert.equal(archiveChunkOffset(4294967290n,12).toString(),'4294967302')
  assert.equal(archiveChunkOffset(9007199254740989n,12).toString(),'9007199254741001')
  assert.equal(archiveChunkOffset(9223372036854710271n,65536).toString(),'9223372036854775807')
  for(const local of [-1,0.5,65537,Number.MAX_SAFE_INTEGER])assert.throws(()=>archiveChunkOffset(0n,local),/invalid_chunk_offset/)
  const input=Buffer.from(JSON.stringify({text:'x'.repeat(65517)+'λ😀'})+'\r\n'+JSON.stringify({text:'final λ😀'}))
  const lambda=input.indexOf(Buffer.from('λ')),emoji=input.indexOf(Buffer.from('😀')),crlf=input.indexOf(Buffer.from('\r\n'))
  assert.equal(crlf+2,65536)
  for(const cut of [lambda+1,emoji+1,emoji+3,crlf+1,65530,65535,65536]){
    const framer=createArchiveFramer(),rows=[...framer.push(input.subarray(0,cut)),...framer.push(input.subarray(cut),true)]
    assert.equal(rows.length,2);assert.equal(rows[0].delimiter,'\r\n');assert.equal(rows[1].delimiter,'')
    assert.equal(rows[0].end,String(input.indexOf(10)+1));assert.equal(rows[1].offset,rows[0].end)
    assert.equal(rows[1].end,String(input.length));assert.equal(framer.through,BigInt(input.length));assert.equal(rows[1].text,'final λ😀')
  }
  const empty=createArchiveFramer();assert.deepEqual([...empty.push(new Uint8Array(),true)],[]);assert.equal(empty.through,0n)
})
test('archive and live semantic transitions match pairing, metadata resets and distant anchors',()=>{
  const values=[{type:'event_msg',timestamp:'2026-01-01T00:00:00Z',payload:{type:'agent_reasoning',text:'think'}},{type:'response_item',timestamp:'2026-01-01T00:00:00.050Z',payload:{type:'reasoning',summary:[{text:'think'}]}},{type:'metadata'},...Array.from({length:50},()=>({type:'response_item',payload:{type:'function_call',name:'a',arguments:'{}'}})),{type:'event_msg',timestamp:'2026-01-01T00:00:01Z',payload:{type:'agent_message',text:'same'}},{type:'metadata'},{type:'response_item',timestamp:'2026-01-01T00:00:01Z',payload:{type:'message',role:'assistant',content:[{type:'text',text:'same'}]}}]
  const input=Buffer.from(values.map(value=>JSON.stringify(value)).join('\n'))
  const archive=[...createArchiveFramer().push(input,true)],visible=[];let previous,anchor
  for(const row of archive){const next=semanticTransition(row,previous,anchor);previous=next.previous;anchor=next.anchor;if(next.retired!==null){const old=visible.findIndex(item=>item.offset===next.retired);visible.splice(old,1)}if(row.category&&!next.hidden)visible.push(row)}
  const live=createRecordFramer().push(0,input,true).rows
  assert.deepEqual(visible.map(row=>[Number(row.offset),Number(row.presentationOffset),row.activityAnchors]),live.map(row=>[row.offset,row.presentationOffset,row.activityAnchors]))
  assert.equal(new Set(visible.filter(row=>row.category==='activity').map(row=>row.activityAnchors[0])).size,1)
})

test('archive block refs are per-row, stable and private while exact live semantics remain unchanged',async()=>{
  const event={type:'event_msg',timestamp:'2026-01-01T00:00:00Z',payload:{type:'agent_message',text:'same λ😀'}}
  const canonical={type:'response_item',timestamp:'2026-01-01T00:00:00.050Z',payload:{type:'message',role:'assistant',content:[{type:'text',text:'same λ😀'}]}}
  const values=[event,canonical,...Array.from({length:30},(_,index)=>({type:'response_item',payload:{type:'message',role:'assistant',content:[{type:'text',text:'row '+index}]}}))]
  for(const value of values){const raw=JSON.stringify(value);assert.equal(presentation(value,raw,{archive:true}).text,presentation(value,raw).text)}
  const input=Buffer.from(values.map(value=>JSON.stringify(value)).join('\n')),rows=[...createArchiveFramer().push(input,true)]
  const normalized=new Set(rows.map(row=>row.text)),parse=JSON.parse,arrays=new Set(),visible=[];let attempts=0,previous,anchor
  JSON.parse=function(value,...rest){if(normalized.has(value)){attempts++;throw Error('normalized_blocks_reparsed')}return parse(value,...rest)}
  try{for(const row of rows){
    const pairs=visibleBlocks(row),symbols=Object.getOwnPropertySymbols(row)
    assert.equal(symbols.length,2);assert.ok(symbols.every(symbol=>!Object.getOwnPropertyDescriptor(row,symbol).enumerable))
    assert.ok(!arrays.has(pairs));arrays.add(pairs)
    for(let repeat=0;repeat<100;repeat++)assert.equal(visibleBlocks(row),pairs)
    assert.equal(pairs.length,1);assert.equal(Object.getOwnPropertySymbols(row).length,2)
    const clone=structuredClone(row);assert.deepEqual(Object.getOwnPropertySymbols(clone),[]);assert.equal(clone.text,row.text);assert.ok(!Object.values(clone).some(Array.isArray))
    assert.equal(JSON.stringify(row),JSON.stringify(clone))
    const next=semanticTransition(row,previous,anchor);previous=next.previous;anchor=next.anchor
    if(next.retired!==null)visible.splice(visible.findIndex(value=>value.offset===next.retired),1)
    if(row.category&&!next.hidden)visible.push(row)
  }}finally{JSON.parse=parse}
  assert.equal(attempts,0)
  const live=createRecordFramer().push(0,input,true).rows
  assert.deepEqual(visible.map(row=>[Number(row.offset),Number(row.presentationOffset),row.text,row.activityAnchors]),live.map(row=>[row.offset,row.presentationOffset,row.text,row.activityAnchors]))
  await indexed(values,async(engine,store)=>{
    for(const [table,entries]of Object.entries(store.tables))for(const value of entries.values()){
      assert.deepEqual(Object.getOwnPropertySymbols(value),[]);assert.ok(!('text' in value));assert.ok(!Object.values(value).some(Array.isArray),table+' must not persist block arrays')
      if(table==='records')assert.deepEqual(Object.keys(value).sort(),['delimiter','end','key','offset'])
      if(table==='pieces')assert.deepEqual(Object.keys(value).sort(),['delimiter','end','group','index','key','kind','label','offset','role','time'])
    }
    const page=await engine.page();assert.ok(page.rows.length<=64)
    for(const row of page.rows){assert.deepEqual(Object.getOwnPropertySymbols(row),[]);assert.ok(!('blocks' in row));assert.ok(!Object.values(row).some(Array.isArray))}
  })
})
test('lazy pretty output preserves exact formatting without expanding a 202MB valid tool payload',()=>{
  for(const value of [{a:[1,true,null,{b:'λ😀'}]},[],{},'literal','x'.repeat(16380)+'😀end']) {
    const expected=JSON.stringify(value,null,2),parts=[]
    for(let part=0;;part++){const result=prettyPayloadSlice(value,part,7);assert.ok(result.text.length<=7);assert.equal(Buffer.from(result.text).toString(),result.text);parts.push(result.text);if(!result.more)break}
    assert.equal(parts.join(''),expected)
  }
  let value=Array(100000).fill('x');for(let depth=0;depth<1000;depth++)value=[value]
  const compact=JSON.stringify(value);assert.ok(compact.length<1048576)
  const total=202706002,part=Math.floor(total/16384),tail=prettyPayloadSlice(value,part)
  assert.equal(tail.more,false);assert.equal(tail.text.length,total%16384);assert.ok(tail.text.endsWith('\n]'))
  const input=Buffer.from(JSON.stringify({type:'response_item',payload:{type:'function_call',arguments:compact}})+'\n')
  const framer=createArchiveFramer();let row
  for(let at=0;at<input.length;at+=65536)for(const record of framer.push(input.subarray(at,at+65536),at+65536>=input.length))row=record
  assert.ok(row.text.length<1048576);assert.equal(JSON.parse(row.text)[0].tokens.length,0)
})
function memoryStore(){const tables=Object.fromEntries(['records','pieces','groups','meta','disclosures'].map(name=>[name,new Map()]));const compressed=[],raw=[];let published=false,closed=false
  return {tables,get published(){return published},get closed(){return closed},put(name,value){tables[name].set(value.key,structuredClone(value))},async get(name,key){return tables[name].get(key)},async flush(){},writeCompressed(_,bytes){compressed.push(bytes.slice())},writeRaw(_,bytes){raw.push(bytes.slice())},async detach(){closed=true},async compressedStream(){return new Blob(compressed).stream()},readRaw(start,end){return Buffer.concat(raw).subarray(Number(start),Number(end))},async publish(){published=true},async closeAndDelete(){closed=true},async scan(name,{after,before,limit}){let rows=[...tables[name].values()].sort((a,b)=>a.key.localeCompare(b.key));if(after)rows=rows.filter(row=>row.key>after);if(before)return rows.filter(row=>row.key<before).slice(-limit);return rows.slice(0,limit)},async readPageMetadata(pieces){return pieces.map(piece=>({open:tables.disclosures.get(piece.key)?.open || false,groupOpen:piece.group?tables.disclosures.get('group:'+piece.group)?.open || false:false,count:piece.group?tables.groups.get(piece.group)?.count || '0':'0'}))},async disclosure(key,open){tables.disclosures.set(key,{key,open})}}
}
async function stagedEngine(values,prepare,action){
  const prior=globalThis.Worker;globalThis.Worker=class{}
  const raw=Buffer.from(values.map(value=>JSON.stringify(value)).join('\n')+'\n'),gzip=gzipSync(raw),store=memoryStore(),events=[],options=prepare(store,raw) || {}
  const engine=createArchiveEngine({openStore:async()=>store,emit:value=>events.push(value),...options})
  try{await engine.start({name:'staged',stamp:{workspace,session},manifest:manifest(raw,gzip)});for(let at=0;at<gzip.length;at+=65536)await engine.chunk(gzip.subarray(at,at+65536));await action(engine,store,events)}
  finally{await engine.cleanup();assert.equal(store.closed,true);globalThis.Worker=prior}
}
const untilStaged=async predicate=>{for(let turn=0;turn<30&&!predicate();turn++)await new Promise(resolve=>setImmediate(resolve));assert.ok(predicate(),'bounded staged engine transition')}

test('non-flush indexing completes a whole chunk without per-record promise turns',async()=>{
  let puts=0,atFirstMicrotask=0
  await stagedEngine(Array.from({length:64},()=>({text:'x'.repeat(1012)})),(store,raw)=>{
    assert.equal(raw.length,65536)
    const put=store.put;store.put=(...args)=>{if(++puts===1)queueMicrotask(()=>{atFirstMicrotask=puts});return put(...args)}
    return {decode:()=>({readable:new ReadableStream({start(controller){controller.enqueue(raw);controller.close()}}),completion:Promise.resolve(),cleanup:Promise.resolve()})}
  },async(engine,store)=>{
    await engine.finish();assert.equal(atFirstMicrotask,128);assert.equal(puts,128)
    assert.equal(engine.snapshot().records,'64');assert.equal(store.published,true);assert.equal(store.tables.pieces.size,64)
  })
})

test('mid-group enqueue resumes once and publication waits for the terminal flush',async()=>{
  let releaseGroup,releaseTerminal,groupHeld=false,publishEntered=false
  const groupGate=new Promise(resolve=>{releaseGroup=resolve}),terminalGate=new Promise(resolve=>{releaseTerminal=resolve})
  const value={type:'assistant',message:{role:'assistant',content:Array.from({length:600},(_,index)=>({type:'thinking',thinking:'thought '+index}))}}
  await stagedEngine([value],store=>{
    const put=store.put,publish=store.publish
    store.put=(name,value)=>{if(name==='groups'&&value.count==='2'&&!groupHeld){groupHeld=true;return groupGate.then(()=>put(name,value))}return put(name,value)}
    store.publish=async value=>{publishEntered=true;await terminalGate;return publish(value)}
  },async(engine,store,events)=>{
    const finishing=engine.finish();finishing.catch(()=>{})
    try{
      await untilStaged(()=>groupHeld)
      assert.equal(store.tables.pieces.size,1);assert.equal(store.published,false);assert.equal(engine.snapshot().records,'1')
      assert.ok(!events.some(value=>value.phase==='ready'));releaseGroup()
      await untilStaged(()=>publishEntered)
      assert.equal(store.tables.pieces.size,600);assert.equal(store.published,false);assert.ok(!events.some(value=>value.phase==='ready'))
      releaseTerminal();await finishing
      const first=await engine.page(),last=await engine.page({last:true})
      assert.equal(first.rows[0].count,'600');assert.equal(last.rows.at(-1).index,599)
      assert.equal(first.rows[0].group,last.rows.at(-1).group);assert.equal(store.tables.records.size,1)
      assert.equal(events.filter(value=>value.phase==='ready').length,1)
    }finally{releaseGroup();releaseTerminal();await finishing.catch(()=>{})}
  })
})

test('held enqueue retirement and failed enqueue preserve failure and never publish',async()=>{
  for(const retire of [true,false]){
    let release,reject,held=false
    const gate=new Promise((yes,no)=>{release=yes;reject=no}),failure=Error('injected_flush_failure')
    await stagedEngine([{text:'first'},{text:'second'}],store=>{
      const put=store.put
      store.put=(name,value)=>{if(name==='records'&&value.offset!=='0'&&!held){held=true;return gate.then(()=>put(name,value))}return put(name,value)}
    },async(engine,store,events)=>{
      const finishing=engine.finish();finishing.catch(()=>{})
      try{
        await untilStaged(()=>held);assert.equal(store.tables.pieces.size,1);assert.equal(store.published,false)
        if(retire){engine.retire();release();await assert.rejects(finishing,/retired_archive/)}
        else{reject(failure);await assert.rejects(finishing,error=>error===failure)}
        assert.equal(store.tables.pieces.size,1);assert.equal(store.published,false);assert.ok(!events.some(value=>value.phase==='ready'))
      }finally{release();await finishing.catch(()=>{})}
    })
  }
})

async function indexed(values,action){
  const prior=globalThis.Worker;globalThis.Worker=class{}
  const raw=Buffer.from(values.map(value=>JSON.stringify(value)).join('\n')+'\n'),gzip=gzipSync(raw),store=memoryStore(),engine=createArchiveEngine({openStore:async()=>store})
  try{await engine.start({name:'test',stamp:{workspace,session},manifest:manifest(raw,gzip)});for(let at=0;at<gzip.length;at+=65536)await engine.chunk(gzip.subarray(at,at+65536));await engine.finish();await action(engine,store)}
  finally{await engine.cleanup();globalThis.Worker=prior}
}

test('accepted compressed progress crosses the real bridge before held EOF and suppresses stale or cancelled work',async()=>{
  const priorWorker=globalThis.Worker,priorPerformance=Object.getOwnPropertyDescriptor(globalThis,'performance')
  globalThis.Worker=class{}
  try{for(const cancel of [false,true]){
    let time=0,viewer,worker,controller,streamClosed=false,requestId=0,clears=0
    Object.defineProperty(globalThis,'performance',{value:{now:()=>time},configurable:true})
    const raw=Buffer.from(JSON.stringify({text:'accepted progress λ😀'})+'\n'),gzip=gzipSync(raw),info=manifest(raw,gzip),events=[],listeners=new Map(),store=memoryStore()
    assert.ok(gzip.length>24)
    const config={active:true,stamp:'progress'+cancel,load:1,workspace,session,authority:'principal',apiUrl:''}
    class EngineWorker{
      constructor(){this.tail=Promise.resolve();this.phaseUsesEngine=true}
      postMessage(message){
        this.stamp=message.stamp
        if(message.kind==='interrupt'){this.engine?.retire();return}
        this.tail=this.tail.then(async()=>{
          let result={kind:'ack'}
          if(message.kind==='start'||message.kind==='adopt'){this.engine=createArchiveEngine({openStore:async()=>store,emit:value=>this.onmessage?.({data:{...value,request:message.request}})});if(message.kind==='adopt')await this.engine.adopt(message);else await this.engine.start(message)}
          else if(message.kind==='chunk')await this.engine.chunk(new Uint8Array(message.bytes))
          else if(message.kind==='finish')await this.engine.finish()
          else if(message.kind==='detach')result={kind:'detached',stage:await this.engine.detach()}
          else if(message.kind==='page')result={...await this.engine.page(message.page),kind:'page'}
          else if(message.kind==='retire'){await this.engine.cleanup();result={kind:'cleaned'}}
          this.onmessage?.({data:{...message.stamp,request:message.request,...result}})
        }).catch(error=>this.onmessage?.({data:{...message.stamp,request:message.request,kind:'error',code:error.message}}))
      }
      terminate(){this.terminated=true}
    }
    const body=new ReadableStream({start(value){controller=value}})
    viewer=createPhaseBridgeFixture({
      document:{getElementById:()=>({getAttribute:name=>name==='data-sealed-dom-owner'?config.stamp:name==='data-sealed-dom-load'?String(config.load):null,replaceChildren(){clears++}}),querySelectorAll:()=>[],addEventListener:(kind,handler)=>listeners.set(kind,handler)},
      window:{setTimeout,clearTimeout,crypto:globalThis.crypto,addEventListener(){}},workerFactory:()=>{worker=new EngineWorker();return worker},
      send:event=>{events.push(event);if(event.kind==='sealed-demand')queueMicrotask(()=>viewer.configure(config,{id:++requestId,sequence:event.sequence}))},
      fetchArchive:async(url,{signal})=>{
        if(url.endsWith('/archive'))return {response:new Response(JSON.stringify(info)),isCurrent:()=>true}
        signal.addEventListener('abort',()=>{if(!streamClosed){streamClosed=true;controller.error(Error('aborted'))}},{once:true})
        return {isCurrent:()=>true,response:new Response(body,{headers:{'Content-Type':'application/gzip','Content-Length':String(gzip.length),'X-HMem-Archive-Generation':generation,'X-HMem-Archive-SHA256':digest(gzip),'X-HMem-Archive-Raw-Bytes':String(raw.length),'X-HMem-Archive-Raw-SHA256':digest(raw)}})}
      }
    })
    const counts=()=>events.filter(event=>event.kind==='sealed-progress'&&event.phase==='downloading').map(event=>event.compressedBytes)
    try{
      viewer.configure(config);await untilStaged(()=>worker?.engine&&events.filter(event=>event.kind==='sealed-http').length===1)
      controller.enqueue(Uint8Array.from(gzip.subarray(0,8)));await untilStaged(()=>counts().includes('8'))
      time=250;controller.enqueue(Uint8Array.from(gzip.subarray(8,16)));await untilStaged(()=>worker.engine.snapshot().downloadBytes==='16')
      assert.deepEqual(counts(),['0','8'])
      time=1250;controller.enqueue(Uint8Array.from(gzip.subarray(16,24)));await untilStaged(()=>counts().includes('24'))
      assert.equal(store.published,false);assert.ok(!events.some(event=>event.kind==='sealed-page'))
      worker.onmessage({data:{...worker.stamp,load:2,kind:'progress',phase:'downloading',compressedBytes:'999',rawBytes:'0'}})
      assert.ok(!counts().includes('999'))
      if(cancel){
        const before=clears
        listeners.get('click')({target:{closest:()=>({getAttribute:name=>name==='data-sealed-action'?'cancel':config.stamp})},preventDefault(){}})
        assert.ok(clears>before);assert.equal(events.at(-1).kind,'sealed-cancelled')
        worker.onmessage({data:{...worker.stamp,kind:'progress',phase:'downloading',compressedBytes:'999',rawBytes:'0'}})
        assert.ok(!counts().includes('999'));await untilStaged(()=>worker.terminated)
        assert.equal(store.published,false);assert.ok(!events.some(event=>event.kind==='sealed-page'))
      }else{
        time=1300;controller.enqueue(Uint8Array.from(gzip.subarray(24)));await untilStaged(()=>counts().includes(String(gzip.length)))
        assert.deepEqual(counts(),['0','8','24',String(gzip.length)]);assert.equal(store.published,false)
        assert.ok(!events.some(event=>event.kind==='sealed-progress'&&event.phase==='decoding'),'held EOF cannot create a decoder')
        streamClosed=true;controller.close();await untilStaged(()=>events.some(event=>event.kind==='sealed-page'))
        assert.equal(store.published,true)
      }
    }finally{viewer.configure(null);if(!streamClosed){streamClosed=true;controller.close()}await untilStaged(()=>worker.terminated)}
    assert.equal(store.closed,true)
  }}finally{globalThis.Worker=priorWorker;Object.defineProperty(globalThis,'performance',priorPerformance)}
})

test('decoding count progress is coalesced while phase entry, ready and retirement stay immediate',async()=>{
  const priorWorker=globalThis.Worker,priorPerformance=Object.getOwnPropertyDescriptor(globalThis,'performance')
  globalThis.Worker=class{}
  try{for(const retired of [false,true]){
    let time=0,writes=0,engine;const events=[],store=memoryStore(),write=store.writeRaw
    Object.defineProperty(globalThis,'performance',{value:{now:()=>time},configurable:true})
    const line=Buffer.from(JSON.stringify({text:'x'.repeat(65524)})+'\n');assert.equal(line.length,65536)
    const raw=Buffer.concat(Array(8).fill(line)),gzip=gzipSync(raw)
    store.writeRaw=(offset,bytes)=>{write(offset,bytes);time+=250;if(++writes===3&&retired)engine.retire()}
    engine=createArchiveEngine({openStore:async()=>store,emit:value=>events.push({...value,at:time,published:store.published}),decode:()=>({readable:new ReadableStream({start(controller){for(let at=0;at<raw.length;at+=65536)controller.enqueue(raw.subarray(at,at+65536));controller.close()}}),completion:Promise.resolve(),cleanup:Promise.resolve()})})
    try{
      await engine.start({name:'progress',stamp:{workspace,session},manifest:manifest(raw,gzip)});assert.equal(events[0].phase,'downloading');assert.equal(events[0].at,0)
      await engine.chunk(gzip)
      if(retired)await assert.rejects(engine.finish(),/retired_archive/);else await engine.finish()
      const decoding=events.filter(value=>value.phase==='decoding')
      assert.equal(decoding[0].at,0);assert.equal(decoding[0].rawBytes,'0')
      assert.ok(decoding.every(value=>!value.published))
      if(retired){assert.equal(decoding.length,1);assert.ok(!events.some(value=>value.phase==='ready'));assert.equal(store.published,false)}
      else{assert.deepEqual(decoding.map(value=>value.at),[0,1250]);assert.equal(decoding[1].rawBytes,String(5*65536));const ready=events.at(-1);assert.equal(ready.phase,'ready');assert.equal(ready.at,2000);assert.equal(ready.published,true);assert.equal(ready.rawBytes,String(raw.length))}
    }finally{await engine.cleanup()}
    assert.equal(store.closed,true)
  }}finally{globalThis.Worker=priorWorker;Object.defineProperty(globalThis,'performance',priorPerformance)}
})

test('query continuation progresses across dense matches and the body byte budget',async()=>{
  for(const count of [1,1000])await indexed(Array.from({length:300},(_,index)=>({text:'needle '+index+'x'.repeat(count)})),async engine=>{
    let after,seen=new Set(),turns=0
    while(true){const page=await engine.page({query:'needle',after});for(const row of page.rows){assert.ok(!seen.has(row.key),'search must not repeat a candidate');seen.add(row.key)}
      if(!page.more)break
      assert.ok(page.searchAfter,'every partial match page must expose continuation');after=page.searchAfter;assert.ok(++turns<20)}
    assert.equal(seen.size,300)
  })
})

test('an active body formatter survives other opened first-part hydrations',async()=>{
  await indexed([{type:'response_item',payload:{type:'function_call',name:'A',arguments:JSON.stringify({value:'x'.repeat(100000)})}},{type:'response_item',payload:{type:'function_call',name:'B',arguments:'{"value":"other"}'}}],async(engine,store)=>{
    const rows=(await engine.page()).rows,key=rows[0].key
    await engine.disclosure(rows[0].key,true);await engine.disclosure(rows[1].key,true)
    let reads=0;const native=store.readRaw;store.readRaw=(start,end)=>{if(start==='0')reads++;return native(start,end)}
    await engine.page({bodyKey:key,bodyPart:2});const before=reads
    const page=await engine.page({bodyKey:key,bodyPart:3});assert.equal(page.rows[0].body.part,3)
    assert.equal(reads,before,'sequential continuation must retain its active formatter')
  })
})

test('deferred structured reasoning pairs with its exact pretty event envelope in both directions',()=>{
  const event={type:'event_msg',timestamp:'2026-01-01T00:00:00Z',payload:{type:'agent_reasoning',text:'{\n  "a": 1\n}'}}
  const canonical={type:'response_item',timestamp:'2026-01-01T00:00:00Z',payload:{type:'reasoning',summary:{a:1}}}
  for(const values of [[event,canonical],[canonical,event]]){
    const input=Buffer.from(values.map(value=>JSON.stringify(value)).join('\n')),archive=[...createArchiveFramer().push(input,true)]
    const first=semanticTransition(archive[0],null,null),second=semanticTransition(archive[1],first.previous,first.anchor)
    assert.equal(second.paired,true);assert.equal(createRecordFramer().push(0,input,true).rows.length,1)
  }
})

test('page metadata is one batch and retirement after a held batch cannot return a page',async()=>{
  const values=Array.from({length:100},(_,index)=>({type:'response_item',payload:{type:'function_call',name:'tool'+index,arguments:'{}'}}))
  await indexed(values,async(engine,store)=>{
    const read=store.readPageMetadata,get=store.get;let batches=0,legacy=0
    store.readPageMetadata=pieces=>{batches++;assert.ok(pieces.length<=64);return read(pieces)}
    store.get=(table,key)=>{if(table==='disclosures'||table==='groups')legacy++;return get(table,key)}
    const first=await engine.page();assert.equal(first.rows.length,64);assert.equal(batches,1);assert.equal(legacy,0)
    let release,entered=false,returned=false
    const gate=new Promise(resolve=>{release=resolve})
    store.readPageMetadata=pieces=>{entered=true;return gate.then(()=>read(pieces))}
    const pending=engine.page({last:true}).then(value=>{returned=true;return value});pending.catch(()=>{})
    try{await untilStaged(()=>entered);engine.retire();release();await assert.rejects(pending,/retired_archive/);assert.equal(returned,false)}
    finally{release();await pending.catch(()=>{})}
  })
})

test('64-row pages preserve complete cursors, search, disclosure identity and outgoing bridge limits',async()=>{
  const value={type:'assistant',message:{role:'assistant',content:Array.from({length:600},(_,index)=>({type:'thinking',thinking:'needle thought '+index}))}}
  const raw=Buffer.from(JSON.stringify(value)+'\n'),gzip=gzipSync(raw),expected=visibleBlocks(createRecordFramer().push(0,raw,true).rows[0])
  assert.equal(expected.length,600)
  await indexed([value],async(engine,store)=>{
    const check=page=>{assert.ok(page.rows.length<=64);assert.ok(Buffer.byteLength(JSON.stringify(page))<=4194304);return page}
    const first=check(await engine.page()),last=check(await engine.page({last:true})),group=first.rows[0].group
    assert.equal(first.rows.length,64);assert.equal(first.rows[0].index,0);assert.equal(last.rows.length,64);assert.equal(last.rows.at(-1).index,599)
    const forward=[];let page=first,turns=0
    while(true){check(page);forward.push(...page.rows);if(!page.more)break;assert.ok(++turns<11);page=await engine.page({after:page.last})}
    assert.deepEqual(forward.map(row=>row.index),expected.map(([index])=>index))
    assert.equal(new Set(forward.map(row=>row.key)).size,600)
    assert.ok(forward.every(row=>row.group===group&&row.count==='600'))
    const backwards=[];page=last;turns=0
    while(true){check(page);backwards.unshift(...page.rows);if(page.first===first.first)break;assert.ok(++turns<11);page=await engine.page({before:page.first})}
    assert.deepEqual(backwards.map(row=>row.key),forward.map(row=>row.key))
    const previous=check(await engine.page({before:last.first}));assert.equal(previous.last,forward[535].key)
    assert.equal((await engine.page({offset:'0'})).first,first.first)
    const opened=forward[63]
    await engine.disclosure(opened.key,true);await engine.disclosure('group:'+group,true)
    await engine.page({last:true})
    const returned=check(await engine.page());assert.equal(returned.rows[63].key,opened.key);assert.equal(returned.rows[63].open,true);assert.equal(returned.rows[63].groupOpen,true)
    assert.equal(returned.rows[63].body.text,expected[63][1].text)
    const matched=[];let after;turns=0
    while(true){page=check(await engine.page({query:'needle',after}));matched.push(...page.rows);if(!page.more)break;assert.ok(page.searchAfter);after=page.searchAfter;assert.ok(++turns<11)}
    assert.deepEqual(matched.map(row=>[row.index,row.kind,row.label,row.body.text]),expected.map(([index,block])=>[index,block.kind,block.label,block.text]))
    assert.ok(matched.every(row=>row.group===group&&row.count==='600'))

    // Exercise the actual public bridge send with real engine pages; only the
    // Worker transport is mocked here. Native Worker coverage stays separate.
    const events=[],listeners=new Map();let viewer,requestId=0,worker
    const config={active:true,stamp:'cap64',load:1,workspace,session,authority:'principal',apiUrl:''}
    const document={getElementById:()=>({replaceChildren(){}}),querySelectorAll:()=>[],addEventListener:(name,handler)=>listeners.set(name,handler)}
    class PageWorker{
      postMessage(message){if(message.kind==='interrupt')return
        Promise.resolve().then(async()=>{
          const result=message.kind==='page'?{...check(await engine.page(message.page)),kind:'page'}:{kind:message.kind==='retire'?'cleaned':'ack'}
          this.onmessage?.({data:{...message.stamp,request:message.request,...result}})
        }).catch(()=>this.onerror())
      }
      terminate(){this.terminated=true}
    }
    viewer=createPhaseBridgeFixture({document,window:{setTimeout,clearTimeout,crypto:globalThis.crypto,addEventListener(){}},workerFactory:()=>{worker=new PageWorker();return worker},
      send:event=>{events.push(event);if(event.kind==='sealed-page')check(event);if(event.kind==='sealed-demand')queueMicrotask(()=>viewer.configure(config,{id:++requestId,sequence:event.sequence}))},
      fetchArchive:async url=>({isCurrent:()=>true,response:url.endsWith('/archive')?new Response(JSON.stringify(manifest(raw,gzip))):new Response(gzip,{headers:{'Content-Type':'application/gzip','Content-Length':String(gzip.length),'X-HMem-Archive-Generation':generation,'X-HMem-Archive-SHA256':digest(gzip),'X-HMem-Archive-Raw-Bytes':String(raw.length),'X-HMem-Archive-Raw-SHA256':digest(raw)}})})
    })
    const until=async predicate=>{for(let turn=0;turn<100&&!predicate();turn++)await new Promise(resolve=>setImmediate(resolve));assert.ok(predicate(),'bounded bridge completion')}
    try{
      viewer.configure(config);await until(()=>events.some(event=>event.kind==='sealed-page'))
      const sentFirst=events.find(event=>event.kind==='sealed-page');assert.equal(sentFirst.rows.length,64);assert.equal(sentFirst.first,first.first)
      listeners.get('click')({target:{closest:()=>({getAttribute:name=>name==='data-sealed-action'?'last':config.stamp})},preventDefault(){}})
      await until(()=>events.filter(event=>event.kind==='sealed-page').length===2)
      const sentLast=events.filter(event=>event.kind==='sealed-page').at(-1);assert.equal(sentLast.rows.length,64);assert.equal(sentLast.last,last.last)
      assert.ok(!events.some(event=>event.kind==='sealed-error'))
    }finally{viewer.configure(null);await until(()=>worker.terminated)}
    for(const entries of Object.values(store.tables))for(const item of entries.values())assert.ok(!('body' in item),'page bodies never persist')
  })
})

test('huge activity groups and multi-block records retain indexed continuation and disclosure keys',async()=>{
  const call=index=>({type:'response_item',payload:{type:'function_call',name:'tool'+index,arguments:'{"ok":true}'}})
  await indexed(Array.from({length:450},(_,index)=>call(index)),async(engine)=>{
    const first=await engine.page(),last=await engine.page({last:true})
    assert.equal(first.rows.length,64);assert.equal(first.rows[0].count,'450');assert.equal(last.rows.length,64)
    assert.equal(first.rows[0].group,last.rows.at(-1).group)
    await engine.disclosure(first.rows[0].key,true);await engine.disclosure('group:'+first.rows[0].group,true)
    const returned=await engine.page();assert.equal(returned.rows[0].key,first.rows[0].key);assert.equal(returned.rows[0].open,true);assert.equal(returned.rows[0].groupOpen,true)
    assert.deepEqual(JSON.parse(returned.rows[0].body.text),{ok:true})
    const exact=await engine.page({offset:first.rows[0].offset});assert.equal(exact.rows[0].key,first.rows[0].key)
  })
  await indexed([{type:'assistant',message:{role:'assistant',content:Array.from({length:600},(_,index)=>({type:'thinking',thinking:'thought '+index}))}}],async engine=>{
    const first=await engine.page(),next=await engine.page({after:first.last}),last=await engine.page({last:true})
    assert.equal(first.rows[0].offset,'0');assert.equal(first.rows[0].count,'600');assert.equal(next.rows[0].index,64);assert.equal(last.rows.at(-1).index,599)
  })
})

test('search reaches late and cross-part UTF8 text through bounded no-match and body cursors',async()=>{
  await indexed([...Array.from({length:300},(_,index)=>({text:'ordinary '+index})),{text:'x'.repeat(16380)+'Needle😀after'}],async engine=>{
    const first=await engine.page({query:'Needle😀'});assert.equal(first.rows.length,0);assert.equal(first.more,true);assert.ok(first.searchAfter)
    let last=first,turns=1
    while(last.more&&!last.rows.length){assert.ok(last.searchAfter);last=await engine.page({query:'Needle😀',after:last.searchAfter});assert.ok(++turns<=5)}
    assert.equal(last.rows.length,1);assert.equal(last.rows[0].body.part,1);assert.ok(last.rows[0].body.text.endsWith('after'))
  })
})

test('Last and previous pages retain their requested end when hydration consumes the page byte reserve',async()=>{
  await indexed(Array.from({length:300},(_,index)=>({text:'row'+index+'界'.repeat(16000)})),async(engine,store)=>{
    const all=[...store.tables.pieces.values()].sort((a,b)=>a.key.localeCompare(b.key)),last=await engine.page({last:true})
    assert.equal(last.last,all.at(-1).key)
    const previous=await engine.page({before:last.first}),end=all.filter(row=>row.key<last.first).at(-1).key
    assert.equal(previous.last,end)
    for(const page of [last,previous]){
      assert.ok(page.rows.length>0&&page.rows.length<64,'byte reserve must bind before the row cap')
      const bytes=page.rows.reduce((sum,row)=>sum+Buffer.byteLength(JSON.stringify(row.body)),0)
      assert.ok(bytes<=131072,'actual hydrated body payload stays within the reserve')
      const excluded=all.filter(row=>row.key<page.first).at(-1),next=(await engine.hydrate(excluded.key,0)).body
      assert.ok(bytes+Buffer.byteLength(JSON.stringify(next))>131072,'one more real candidate would exceed the body reserve')
    }
  })
})

test('the approved structured-reasoning exception uses exact formatted UTF8 bytes at the boundary',async()=>{
  for(const size of [1048575,1048576,1048577]) {
    const summary={note:'[Open](https://example.test) λ😀',values:Array(100).fill(0),padding:''}
    const base=Buffer.byteLength(JSON.stringify(summary,null,2));summary.padding='x'.repeat(size-base)
    const value={type:'response_item',payload:{type:'reasoning',summary}}
    assert.ok(Buffer.byteLength(JSON.stringify(value))<=1048576)
    await indexed([value],async engine=>{
      const first=(await engine.page()).rows[0],result=await engine.hydrate(first.key,0)
      assert.equal(result.body.format,size>1048576?'json':'markdown');assert.equal(result.body.exception,size>1048576);assert.equal(result.body.structured,true)
      assert.ok(Buffer.byteLength(result.body.text)<=65536)
      const complete=createHash('sha256');let bytes=0,part=0,segment=result.body
      while(true){complete.update(segment.text);bytes+=Buffer.byteLength(segment.text);assert.ok(Buffer.byteLength(segment.text)<=65536);assert.equal(segment.part,part);if(!segment.more)break;assert.ok(++part<100);segment=(await engine.hydrate(first.key,part)).body}
      assert.equal(bytes,size);assert.equal(complete.digest('hex'),digest(JSON.stringify(summary,null,2)))
    })
  }
})
test('both receipt barriers precede publication and decode; corruption publishes nothing',async()=>{
  const prior=globalThis.Worker;globalThis.Worker=class{}
  try{for(const mutation of ['valid','compressed','raw','truncated','trailing','json']){
    const raw=Buffer.from(mutation==='json'?'{\n':'{"text":"hello"}\n'),gzip=gzipSync(raw),store=memoryStore();let decodes=0
    const engine=createArchiveEngine({openStore:async()=>store,decode(stream){decodes++;return gzipPipeline(stream)}})
    const info=mutation==='json'?{...manifest(Buffer.from('0\n'),gzip),raw_bytes:String(raw.length),raw_sha256:digest(raw)}:manifest(raw,gzip)
    let transmitted=gzip
    if(mutation==='raw')info.raw_sha256='0'.repeat(64)
    if(mutation==='compressed')info.compressed_sha256='0'.repeat(64)
    if(mutation==='truncated'){transmitted=gzip.subarray(0,-2);info.compressed_bytes=String(transmitted.length);info.compressed_sha256=digest(transmitted)}
    if(mutation==='trailing'){transmitted=Buffer.concat([gzip,gzip]);info.compressed_bytes=String(transmitted.length);info.compressed_sha256=digest(transmitted)}
    await engine.start({name:'test',stamp:{workspace,session},manifest:info});await engine.chunk(transmitted)
    await assert.rejects(engine.page(),/archive_unverified/)
    if(mutation==='valid'){await engine.finish();assert.equal(store.published,true);assert.equal((await engine.page()).rows[0].body.text,'hello')}
    else {await assert.rejects(engine.finish());assert.equal(store.published,false)}
    assert.equal(decodes,mutation==='compressed'?0:1)
    try{await engine.cleanup()}catch(error){assert.fail(mutation+': cleanup '+error.message+' '+error.stack)}
    assert.equal(store.closed,true)
  }}finally{globalThis.Worker=prior}
})

test('failed decode retains ownership through upstream cleanup and preserves secondary cleanup failure',async()=>{
  const prior=globalThis.Worker;globalThis.Worker=class{}
  try{for(const failedCleanup of [false,true]) {
    const raw=Buffer.from('0\n'),gzip=gzipSync(raw),store=memoryStore();let release,settled=false
    const cleanup=new Promise((resolve,reject)=>{release=()=>failedCleanup?reject(Error('actual_source_close_failed')):resolve()})
    const engine=createArchiveEngine({openStore:async()=>store,decode:()=>({readable:new ReadableStream({start(controller){controller.enqueue(Buffer.from('{\n'));controller.close()}}),completion:Promise.resolve(),cleanup})})
    await engine.start({name:'owned',stamp:{workspace,session},manifest:manifest(raw,gzip)});await engine.chunk(gzip)
    const finishing=engine.finish().then(()=>assert.fail('invalid JSON published'),error=>{settled=true;return error})
    await new Promise(resolve=>setTimeout(resolve,5));assert.equal(settled,false);assert.equal(store.closed,false)
    release();const failure=await finishing
    if(failedCleanup){assert.equal(failure.message,'archive_cleanup_failed');assert.ok(failure.errors.some(error=>error.message==='invalid_json_record'));await assert.rejects(engine.cleanup(),/archive_cleanup_failed/);assert.equal(store.closed,false)}
    else {assert.equal(failure.message,'invalid_json_record');await engine.cleanup();assert.equal(store.closed,true)}
  }}finally{globalThis.Worker=prior}
})

test('sealed bridge clears authority synchronously and retains one Worker through actual cleanup acknowledgement',async()=>{
  const events=[],workers=[],listeners=new Map();let clears=0,fetches=0
  const document={getElementById:()=>({getAttribute:name=>name==='data-sealed-dom-owner'?'a':name==='data-sealed-dom-load'?'1':null,replaceChildren(){clears++}}),addEventListener(name,handler){listeners.set(name,handler)}}
  const window={setTimeout,clearTimeout,crypto:globalThis.crypto,addEventListener(){}}
  const raw=Buffer.from('0\n'),gzip=gzipSync(raw),info=manifest(raw,gzip)
  class FakeWorker {
    postMessage(message){this.stamp=message.stamp;if(message.kind==='interrupt')return
      if(message.kind==='retire'){this.cleanup=()=>this.onmessage?.({data:{...message.stamp,kind:'cleaned',request:message.request}});return}
      queueMicrotask(()=>this.onmessage?.({data:{...message.stamp,kind:'ack',request:message.request}}))}
    terminate(){this.terminated=true}
  }
  const viewer=createPhaseBridgeFixture({send:value=>events.push(value),document,window,workerFactory:()=>{const worker=new FakeWorker();workers.push(worker);return worker},async fetchArchive(url,{signal}){
    fetches++
    if(url.endsWith('/archive'))return {response:new Response(JSON.stringify(info)),isCurrent:()=>true}
    return new Promise((_,reject)=>signal.addEventListener('abort',()=>reject(Error('aborted')),{once:true}))
  }})
  const config={active:true,stamp:'a',load:1,workspace,session,authority:'principal',apiUrl:''}
  const flush=()=>new Promise(resolve=>setImmediate(resolve))
  viewer.configure(config);const demand=events.find(value=>value.kind==='sealed-demand')
  viewer.configure(config,{id:1,sequence:demand.sequence});await flush();await flush()
  const content=events.filter(value=>value.kind==='sealed-demand').at(-1)
  viewer.configure(config,{id:2,sequence:content.sequence});await flush()
  const before=clears;viewer.retireAuthority();assert.ok(clears>before)
  viewer.configure({...config,stamp:'b',load:2});viewer.configure({...config,stamp:'c',load:3})
  await flush();await flush();assert.equal(workers.length,1);assert.equal(fetches,2)
  assert.ok(events.some(value=>value.kind==='sealed-http'&&value.request===2))
  workers[0].cleanup();await flush();await flush()
  assert.equal(workers.length,2);assert.equal(workers[0].terminated,true)
  assert.equal(events.filter(value=>value.kind==='sealed-demand').at(-1).stamp,'c')
  viewer.configure(null);await flush();assert.equal(workers[1].terminated,true)
})

test('HTTP cleanup has a finite failure disposition while real streams and physical occupancy remain owned',async()=>{
  const events=[],workers=[];let releaseCancel
  const raw=Buffer.from('0\n'),gzip=gzipSync(raw),info=manifest(raw,gzip)
  const hold=new Promise(resolve=>{releaseCancel=resolve})
  const document={getElementById:()=>({replaceChildren(){}}),addEventListener(){}}
  const window={setTimeout,clearTimeout,crypto:globalThis.crypto,addEventListener(){}}
  class FakeWorker {
    postMessage(message){if(message.kind==='interrupt')return;queueMicrotask(()=>this.onmessage?.({data:{...message.stamp,kind:message.kind==='retire'?'cleaned':'ack',request:message.request}}))}
    terminate(){this.terminated=true}
  }
  const viewer=createPhaseBridgeFixture({send:value=>events.push(value),document,window,cleanupMs:20,workerFactory:()=>{const worker=new FakeWorker();workers.push(worker);return worker},async fetchArchive(url){
    return {response:url.endsWith('/archive')?new Response(JSON.stringify(info)):new Response(new ReadableStream({cancel(){return hold}})),isCurrent:()=>true}
  }})
  const config={active:true,stamp:'a',load:1,workspace,session,authority:'principal',apiUrl:''},flush=()=>new Promise(resolve=>setImmediate(resolve))
  viewer.configure(config);viewer.configure(config,{id:1,sequence:events.find(value=>value.kind==='sealed-demand').sequence});await flush();await flush()
  viewer.configure(config,{id:2,sequence:events.filter(value=>value.kind==='sealed-demand').at(-1).sequence});await flush()
  viewer.configure({...config,stamp:'new',load:2});await new Promise(resolve=>setTimeout(resolve,40))
  assert.deepEqual(viewer.snapshot(),{active:true,pending:false,blocked:true});assert.equal(workers.length,1)
  assert.ok(events.some(value=>value.kind==='sealed-error'&&value.code==='archive_cleanup_failed'))
  viewer.configure({...config,stamp:'after-failed',load:3})
  assert.ok(events.some(value=>value.kind==='sealed-error'&&value.code==='archive_cleanup_failed'&&value.stamp==='after-failed'),'Retry must retain a truthful blocked disposition')
  assert.ok(!events.some(value=>value.kind==='sealed-http'&&value.request===2),'timeout cannot invent physical completion')
  releaseCancel();await flush();await flush();await flush()
  assert.ok(events.some(value=>value.kind==='sealed-http'&&value.request===2),'actual source cancellation owns the physical receipt')
  assert.equal(workers[0].terminated,true);assert.equal(workers.length,1)
})



test('local page progress is coalesced at100ms with one owned timer and immediate accepted pages',async()=>{
  for(const retirement of ['complete','error','inactive','authority']){
    const events=[],listeners=new Map(),workers=[],timers=new Map(),callbacks=[];let viewer,requestId=0,now=0,nextTimer=0,search='latest'
    const clock={setTimeout(callback,delay){if(delay!==100)return setTimeout(callback,delay);const id={pageProgress:++nextTimer};timers.set(id,{at:now+delay,callback});callbacks.push(callback);return id},clearTimeout(id){if(id?.pageProgress)timers.delete(id);else clearTimeout(id)},advance(ms){now+=ms;for(const[id,entry]of [...timers])if(entry.at<=now){timers.delete(id);entry.callback()}}}
    const config={active:true,stamp:'progress-'+retirement,load:1,workspace,session,authority:'principal',apiUrl:''}
    const document={getElementById:id=>id==='sealed-search'?{value:search}:{replaceChildren(){}},querySelectorAll:()=>[],addEventListener:(name,handler)=>listeners.set(name,handler)}
    class HeldWorker{
      pages=[];retiring=null
      reply(message,value){this.onmessage?.({data:{...message.stamp,request:message.request,...value}})}
      postMessage(message){if(message.kind==='interrupt')return;if(message.kind==='page'){this.pages.push(message);return}if(message.kind==='retire'){this.retiring=message;return}queueMicrotask(()=>this.reply(message,{kind:'ack'}))}
      terminate(){this.terminated=true}
    }
    const raw=Buffer.from('0\n'),gzip=gzipSync(raw),navigating=()=>events.filter(value=>value.kind==='sealed-progress'&&value.phase==='navigating').length
    viewer=createPhaseBridgeFixture({document,window:{...clock,crypto:globalThis.crypto,addEventListener(){}},workerFactory:()=>{const worker=new HeldWorker();workers.push(worker);return worker},send:event=>{events.push(event);if(event.kind==='sealed-demand')queueMicrotask(()=>viewer.configure(config,{id:++requestId,sequence:event.sequence}))},fetchArchive:async url=>({isCurrent:()=>true,response:url.endsWith('/archive')?new Response(JSON.stringify(manifest(raw,gzip))):new Response(gzip,{headers:{'Content-Type':'application/gzip','Content-Length':String(gzip.length),'X-HMem-Archive-Generation':generation,'X-HMem-Archive-SHA256':digest(gzip),'X-HMem-Archive-Raw-Bytes':String(raw.length),'X-HMem-Archive-Raw-SHA256':digest(raw)}})})})
    const until=async predicate=>{for(let turn=0;turn<100&&!predicate();turn++)await new Promise(resolve=>setImmediate(resolve));assert.ok(predicate(),'bounded progress causal')}
    const click=action=>listeners.get('click')({target:{closest:()=>({getAttribute:name=>name==='data-sealed-action'?action:name==='data-sealed-owner'?config.stamp:null})},preventDefault(){}})
    const page=(worker,index)=>worker.reply(worker.pages[index],{kind:'page',rows:[],first:'first',last:'last',more:false})
    try{
      viewer.configure(config);assert.equal(events[0].phase,'preparing','initial acknowledgement remains immediate')
      await until(()=>workers.at(-1)?.pages.length===1);const worker=workers.at(-1),firstTimer=callbacks.at(-1)
      assert.equal(timers.size,1);clock.advance(99);assert.equal(navigating(),0)
      page(worker,0);await until(()=>events.some(value=>value.kind==='sealed-page'));assert.equal(timers.size,0);assert.equal(navigating(),0)
      firstTimer();assert.equal(navigating(),0,'a cancelled callback cannot paint after a fast accepted page')
      click('last');await until(()=>worker.pages.length===2);assert.equal(timers.size,1);clock.advance(99);assert.equal(navigating(),0);clock.advance(1);assert.equal(navigating(),1);clock.advance(500);assert.equal(navigating(),1,'slow progress emits once')
      page(worker,1);await until(()=>events.filter(value=>value.kind==='sealed-page').length===2);assert.equal(timers.size,0)
      click('first');await until(()=>worker.pages.length===3);const supersededCallback=callbacks.at(-1);click('last');click('search');assert.equal(worker.pages.length,3);assert.equal(timers.size,1)
      page(worker,2);await until(()=>worker.pages.length===4);assert.deepEqual(worker.pages[3].page,{query:'latest'});assert.equal(timers.size,1,'latest operation replaces rather than accumulates a timer')
      supersededCallback();assert.equal(navigating(),1,'old operation cannot paint a newer pending operation')
      const pendingCallback=callbacks.at(-1),beforePages=events.filter(value=>value.kind==='sealed-page').length
      if(retirement==='complete'){page(worker,3);await until(()=>events.filter(value=>value.kind==='sealed-page').length===beforePages+1);assert.equal(timers.size,0);viewer.configure(null)}
      else if(retirement==='error'){worker.reply(worker.pages[3],{kind:'error',code:'page_failed'});await until(()=>events.some(value=>value.kind==='sealed-error'&&value.code==='page_failed'))}
      else if(retirement==='inactive')viewer.configure(null)
      else viewer.retireAuthority()
      await until(()=>worker.retiring);assert.equal(timers.size,0);assert.equal(worker.terminated,undefined);assert.equal(viewer.snapshot().active,true,'progress timer retirement cannot release the held RPC/Worker owner')
      pendingCallback();clock.advance(1000);assert.equal(navigating(),1);assert.equal(workers.length,2)
      if(retirement!=='complete'){page(worker,3);await new Promise(resolve=>setImmediate(resolve));assert.equal(events.filter(value=>value.kind==='sealed-page').length,beforePages)}
      worker.reply(worker.retiring,{kind:'cleaned'});await until(()=>worker.terminated);assert.equal(viewer.snapshot().active,false)
    }finally{viewer.configure(null);for(const worker of workers)if(worker.retiring&&!worker.terminated)worker.reply(worker.retiring,{kind:'cleaned'});await until(()=>workers.every(worker=>worker.terminated));assert.equal(timers.size,0)}
  }
})

test('sealed local intents retain only the latest page or Markdown demand through held operations and retirement',async()=>{
  const events=[],listeners=new Map(),workers=[];let viewer,requestId=0,search=''
  const config={active:true,stamp:'local-intent',load:1,workspace,session,authority:'principal',apiUrl:''}
  const document={getElementById:id=>id==='sealed-search'?{value:search}:{replaceChildren(){}},querySelectorAll:()=>[],addEventListener:(name,handler)=>listeners.set(name,handler)}
  class HeldWorker{
    messages=[];pages=[];bodies=[]
    reply(message,value){this.onmessage?.({data:{...message.stamp,request:message.request,...value}})}
    postMessage(message){this.messages.push(message);if(message.kind==='interrupt')return
      if(message.kind==='page'){this.pages.push(message);return}
      if(message.kind==='body'){this.bodies.push(message);return}
      queueMicrotask(()=>this.reply(message,{kind:message.kind==='retire'?'cleaned':'ack'}))
    }
    terminate(){this.terminated=true}
  }
  const raw=Buffer.from('0\n'),gzip=gzipSync(raw)
  viewer=createPhaseBridgeFixture({document,window:{setTimeout,clearTimeout,crypto:globalThis.crypto,addEventListener(){}},workerFactory:()=>{const worker=new HeldWorker();workers.push(worker);return worker},
    send:event=>{events.push(event);if(event.kind==='sealed-demand')queueMicrotask(()=>viewer.configure(config,{id:++requestId,sequence:event.sequence}));if(event.kind==='sealed-markdown-chunk')queueMicrotask(()=>viewer.configure(config,null,{stamp:config.stamp,load:config.load,request:event.request}))},
    fetchArchive:async url=>({isCurrent:()=>true,response:url.endsWith('/archive')?new Response(JSON.stringify(manifest(raw,gzip))):new Response(gzip,{headers:{'Content-Type':'application/gzip','Content-Length':String(gzip.length),'X-HMem-Archive-Generation':generation,'X-HMem-Archive-SHA256':digest(gzip),'X-HMem-Archive-Raw-Bytes':String(raw.length),'X-HMem-Archive-Raw-SHA256':digest(raw)}})})
  })
  const until=async predicate=>{for(let turn=0;turn<100&&!predicate();turn++)await new Promise(resolve=>setImmediate(resolve));assert.ok(predicate(),'bounded latest-intent completion')}
  const click=(action,key='')=>listeners.get('click')({target:{closest:()=>({getAttribute:name=>name==='data-sealed-action'?action:name==='data-sealed-key'?key:name==='data-sealed-part'?'0':name==='data-sealed-owner'?config.stamp:null})},preventDefault(){}})
  const pageResult=(message,rows=[{key:'source-a'},{key:'source-b'}])=>workers.at(-1).reply(message,{kind:'page',rows,first:'first',last:'last',more:false})
  try{
    viewer.configure(config);await until(()=>workers.at(-1)?.pages.length===1);const worker=workers.at(-1)
    click('body','superseded');click('last');search='latest-search';click('search')
    assert.equal(worker.pages.length,1)
    pageResult(worker.pages[0]);await until(()=>worker.pages.length===2)
    assert.deepEqual(worker.pages[1].page,{query:'latest-search'});assert.equal(worker.bodies.length,0)
    click('first');click('last');click('body','chosen-body')
    pageResult(worker.pages[1]);await until(()=>worker.pages.length===3)
    assert.equal(worker.pages[2].page.bodyKey,'chosen-body')
    pageResult(worker.pages[2]);await until(()=>events.filter(event=>event.kind==='sealed-page').length===3)
    click('markdown','source-a');await until(()=>worker.bodies.length===1)
    click('body','superseded-body');click('last');search='after-markdown';click('search')
    assert.equal(worker.pages.length,3,'busy Markdown owns its one body request')
    worker.reply(worker.bodies[0],{kind:'body',key:'source-a',body:{part:0,text:'complete',format:'markdown',more:false}})
    await until(()=>worker.pages.length===4);assert.deepEqual(worker.pages[3].page,{query:'after-markdown'})
    click('markdown','source-b');assert.equal(worker.bodies.length,1)
    pageResult(worker.pages[3],[{key:'new-page'}]);await until(()=>worker.pages.length===5)
    assert.equal(worker.bodies.length,1,'old-row Markdown waits for its origin page to be restored')
    assert.equal(worker.pages[4].page.bodyKey,'source-b')
    pageResult(worker.pages[4],[{key:'source-b'}]);await until(()=>worker.bodies.length===2)
    assert.equal(events.filter(event=>event.kind==='sealed-page').at(-1).rows[0].key,'source-b')
    assert.equal(events.at(-1).kind,'sealed-markdown-start','origin page is published before its complete-body render sequence')
    assert.equal(worker.bodies[1].key,'source-b','latest Markdown intent restores its visible owner after held Next')
    worker.reply(worker.bodies[1],{kind:'body',key:'source-b',body:{part:0,text:'complete',format:'markdown',more:false}})
    await until(()=>events.filter(event=>event.kind==='sealed-markdown-chunk').length===2)
    click('last');await until(()=>worker.pages.length===6);click('body','retired-body')
    const published=events.filter(event=>event.kind==='sealed-page').length
    viewer.retireAuthority();await until(()=>worker.terminated)
    pageResult(worker.pages[5]);await new Promise(resolve=>setImmediate(resolve))
    assert.equal(worker.pages.length,6);assert.equal(events.filter(event=>event.kind==='sealed-page').length,published)
    assert.equal(workers.length,2);assert.ok(!events.some(event=>event.kind==='sealed-error'))
  }finally{viewer.configure(null);await until(()=>workers.at(-1).terminated)}
})

test('a fitting raw record and index item cannot produce an empty nonadvancing oversized wire page',async()=>{
  const huge={timestamp:'x'.repeat(1048400),type:'response_item',payload:{type:'message',role:'assistant',content:[{type:'output_text',text:'x'}]}}
  const raw=Buffer.from(JSON.stringify(huge)+'\n');assert.ok(raw.length<=1048576)
  await indexed([huge],async(engine,store)=>{
    const piece=[...store.tables.pieces.values()][0]
    assert.ok(Buffer.byteLength(JSON.stringify(piece))<=1048576,'persisted piece fits the existing batch bound')
    const item={...piece,open:false,groupOpen:false,count:'0',body:{text:'x',part:0,more:false,format:'markdown'}}
    assert.ok(Buffer.byteLength(JSON.stringify(item))+3>1048576,'hydrated wire representation does not fit')
    await assert.rejects(engine.page(),/archive_item_limit/)
    await assert.rejects(engine.page({last:true}),/archive_item_limit/)
    await assert.rejects(engine.page({query:'x'}),/archive_item_limit/)
  })
  await indexed([{text:'first'},huge],async engine=>{
    const first=await engine.page();assert.equal(first.rows.length,1);assert.ok(first.first&&first.last);assert.equal(first.more,true)
    await assert.rejects(engine.page({after:first.last}),/archive_item_limit/)
  })
})


test('payload highlighting preserves every byte of incomplete bounded JSON fragments',()=>{
  for(const value of ['{"x":"abc','middle of a string λ😀','\\u12','12.3e-','truefal','"λ😀\\" tail','',JSON.stringify({x:'λ😀',n:-1.25e8,b:true,nothing:null},null,2),'[0,'.repeat(3000)+']']){
    const tokens=payloadTokens(value);assert.equal(tokens.map(token=>token.text).join(''),value);assert.ok(tokens.length<=2049)
  }
  assert.deepEqual(payloadTokens('{"x":"abc'),[{text:'{"x":"abc',kind:'punctuation'}])
  assert.ok(payloadTokens(JSON.stringify({x:'complete',n:123})).some(token=>token.kind==='key'))
})


test('stamped clear epochs precede sensitive DOM removal and never clear unowned live hosts',async()=>{
  const events=[],calls=[],workers=[];let liveStamp='owned',liveLoad='1',recordChildren=1,inspectorChildren=1,foreignChildren=1
  const host=(inspector=false,foreign=false)=>({getAttribute:name=>name==='data-sealed-dom-owner'?(foreign?'foreign':liveStamp):name==='data-sealed-dom-load'?liveLoad:null,replaceChildren(){calls.push(inspector?'inspector-remove':'record-remove');if(foreign)foreignChildren=0;else if(inspector)inspectorChildren=0;else recordChildren=0}})
  const document={getElementById:()=>host(),querySelectorAll:()=>[host(true),host(true,true)],addEventListener(){}}
  class Worker{postMessage(){}terminate(){this.terminated=true}}
  const viewer=createPhaseBridgeFixture({document,window:{setTimeout,clearTimeout,crypto:globalThis.crypto,addEventListener(){}},send:event=>{events.push(event);if(event.kind==='sealed-clear')calls.push('clear-event')},workerFactory:()=>{const worker=new Worker();workers.push(worker);return worker}})
  const config={active:true,stamp:'owned',load:1,workspace,session,authority:'principal',apiUrl:''}
  viewer.configure(config);assert.equal(recordChildren,1);assert.equal(inspectorChildren,1,'initial admission does not mutate an existing Elm placeholder')
  viewer.retireAuthority()
  assert.equal(recordChildren,0);assert.equal(inspectorChildren,0);assert.equal(foreignChildren,1)
  assert.deepEqual(calls,['clear-event','record-remove','inspector-remove'])
  const clear=events.find(event=>event.kind==='sealed-clear');assert.match(clear.domEpoch,/^[a-f0-9-]{36}$/);assert.equal(clear.stamp,config.stamp);assert.equal(clear.generation,config.load)
  await new Promise(resolve=>setImmediate(resolve));assert.equal(workers[0].terminated,true)
  liveStamp='live-unowned';recordChildren=1;inspectorChildren=1;viewer.retireAuthority()
  assert.equal(recordChildren,1);assert.equal(inspectorChildren,1,'no active sealed owner cannot mutate live DOM')
  viewer.configure({...config,stamp:'next',load:2});workers[1].onerror()
  const nextClear=events.filter(event=>event.kind==='sealed-clear').at(-1),error=events.findLast(event=>event.kind==='sealed-error')
  assert.notEqual(nextClear.domEpoch,clear.domEpoch);assert.equal(nextClear.stamp,'next');assert.equal(error.domEpoch,nextClear.domEpoch)
  assert.ok(events.indexOf(nextClear)<events.indexOf(error),'model reset epoch is sent before error rendering')
  assert.equal(recordChildren,1,'a stale owner cannot remove the new live host')
  await new Promise(resolve=>setImmediate(resolve));assert.equal(workers[1].terminated,true)
})


test('retiring owner emits one clear through bounded reconcile feedback while actual cleanup remains held',async()=>{
  const events=[],workers=[];let viewer,requestId=0,desired,feedback=false,feedbackCount=0,saturated=false,hold=true,clears=0
  const config={active:true,stamp:'feedback-a',load:1,workspace,session,authority:'principal-a',apiUrl:''}
  desired=config
  const raw=Buffer.from('0\n'),gzip=gzipSync(raw),info=manifest(raw,gzip)
  const document={getElementById:()=>({getAttribute:name=>name==='data-sealed-dom-owner'?config.stamp:name==='data-sealed-dom-load'?'1':null,replaceChildren(){clears++}}),querySelectorAll:()=>[],addEventListener(){}}
  class Worker{
    postMessage(message){if(message.kind==='interrupt')return
      const respond=()=>this.onmessage?.({data:{...message.stamp,request:message.request,kind:message.kind==='retire'?'cleaned':'ack'}})
      if(message.kind==='retire'&&hold){this.cleanup=respond;return}queueMicrotask(respond)
    }
    terminate(){this.terminated=true}
  }
  viewer=createPhaseBridgeFixture({document,window:{setTimeout,clearTimeout,crypto:globalThis.crypto,addEventListener(){}},workerFactory:()=>{const worker=new Worker();workers.push(worker);return worker},
    send:event=>{events.push(event);if(event.kind==='sealed-demand')queueMicrotask(()=>viewer.configure(desired,{id:++requestId,sequence:event.sequence}));if(feedback){if(++feedbackCount<=16)queueMicrotask(()=>viewer.configure(desired));else saturated=true}},
    fetchArchive:async(url,{signal})=>url.endsWith('/archive')?{response:new Response(JSON.stringify(info)),isCurrent:()=>true}:new Promise((_,reject)=>signal.addEventListener('abort',()=>reject(Error('aborted')),{once:true}))
  })
  const until=async predicate=>{for(let turn=0;turn<100&&!predicate();turn++)await new Promise(resolve=>setImmediate(resolve));assert.ok(predicate(),'bounded retirement feedback completion')}
  try{
    viewer.configure(desired);await until(()=>events.filter(event=>event.kind==='sealed-demand').length===2)
    feedback=true;desired=null;viewer.configure(desired)
    await until(()=>workers[0].cleanup)
    for(let index=0;index<5;index++)viewer.configure(null)
    desired={...config,stamp:'feedback-b',load:2,authority:'principal-b'};viewer.configure(desired)
    desired={...config,stamp:'feedback-c',load:3,authority:'principal-c'};viewer.configure(desired)
    await new Promise(resolve=>setImmediate(resolve))
    assert.equal(saturated,false);assert.equal(events.filter(event=>event.kind==='sealed-clear').length,1);assert.equal(clears,1)
    assert.equal(workers.length,1);assert.equal(workers[0].terminated,undefined);assert.deepEqual(viewer.snapshot(),{active:true,pending:true,blocked:false})
    hold=false;workers[0].cleanup();await until(()=>workers.length===2)
    assert.equal(workers[0].terminated,true);assert.equal(events.filter(event=>event.kind==='sealed-demand').at(-1).stamp,'feedback-c')
  }finally{
    feedback=false;hold=false;desired=null;for(const worker of workers)worker.cleanup?.();viewer.configure(null);await until(()=>workers.every(worker=>worker.terminated))
  }
})


test('retirement during a held body lookup cannot reuse a verified raw or formatter cache',async()=>{
  await indexed([{type:'response_item',payload:{type:'function_call',name:'read',arguments:'{"ok":true}'}}],async(engine,store)=>{
    const key=(await engine.page()).first;await engine.hydrate(key,0)
    const get=store.get,read=store.readRaw;let release,retiredReads=0,lateReads=0
    store.retireReads=()=>{retiredReads++}
    store.get=(table,key)=>table==='pieces'?new Promise(resolve=>{release=()=>resolve(get(table,key))}):get(table,key)
    store.readRaw=(...args)=>{if(retiredReads)lateReads++;return read(...args)}
    const reading=engine.hydrate(key,0),rejected=assert.rejects(reading,/retired_archive/)
    engine.retire();assert.equal(retiredReads,1);release();await rejected;assert.equal(lateReads,0)
  })
})

test('sequential processor reader handoff keeps custody, current authorization and exact incarnation ownership',async()=>{
  for(const scenario of ['success','revalidate200','revalidate401','changed-etag','cancel-detach','auth-revalidate','constructor-failure','cancel-adopt']){
    const events=[],workers=[],order=[];let viewer,requests=0,physical=0,maxPhysical=0,locked=false,deleted=false,stage,revalidateRelease,revalidateEntered=false
    const raw=Buffer.from('{"text":"verified"}\n'),gzip=gzipSync(raw),info=manifest(raw,gzip),etag='"authority-generation-receipt"',config={active:true,stamp:'phase-'+scenario,load:1,workspace,session,authority:'principal',apiUrl:''}
    class PhaseWorker{
      reply(message,value){this.onmessage?.({data:{...message.stamp,request:message.request,...value}})}
      postMessage(message){
        order.push('worker:'+message.kind)
        if(message.kind==='interrupt')return
        if(message.kind==='start'){stage={name:message.name,custody:message.custody,manifest:message.manifest};this.resourceOpen=true}
        if(message.kind==='detach'){this.releaseDetach=()=>{this.releaseDetach=null;this.resourceOpen=false;order.push('processor-resources-closed');this.reply(message,{kind:'detached',stage})};return}
        if(message.kind==='adopt'){assert.equal(locked,true);assert.equal(workers[0].terminated,true);this.releaseAdopt=()=>{this.releaseAdopt=null;this.resourceOpen=true;this.reply(message,{kind:'ack'})};return}
        if(message.kind==='retire'){this.resourceOpen=false;order.push('retire-resources-closed');queueMicrotask(()=>this.reply(message,{kind:'cleaned'}));return}
        if(message.kind==='page'){queueMicrotask(()=>this.reply(message,{kind:'page',rows:[{key:'visible'}],first:'visible',last:'visible',more:false}));return}
        queueMicrotask(()=>this.reply(message,{kind:'ack'}))
      }
      terminate(){assert.equal(!!this.resourceOpen,false);this.terminated=true;order.push('terminate')}
    }
    viewer=createSealedViewer({document:{getElementById:()=>({replaceChildren(){}}),querySelectorAll:()=>[],addEventListener(){}},window:{setTimeout,clearTimeout,crypto:globalThis.crypto,addEventListener(){}},
      custodyFactory:async()=>{assert.equal(locked,false);locked=true;order.push('lock-acquired');return async()=>{assert.equal(deleted,true);locked=false;order.push('lock-released')}},
      deleteStage:async(name,token)=>{assert.equal(name,stage.name);assert.equal(token,stage.custody);assert.ok(workers.every(worker=>worker.terminated));deleted=true;order.push('stage-deleted')},
      workerFactory:()=>{assert.ok(workers.every(worker=>worker.terminated),'no new admitted Worker before old termination');if(workers.length===1&&scenario==='constructor-failure')throw Error('reader_constructor_failed');const worker=new PhaseWorker();workers.push(worker);return worker},
      send:event=>{events.push(event);if(event.kind==='sealed-demand')queueMicrotask(()=>viewer.configure(config,{id:++requests,sequence:event.sequence}));if(event.kind==='sealed-http')physical--},
      fetchArchive:async(url,request)=>{
        physical++;maxPhysical=Math.max(maxPhysical,physical)
        if(url.endsWith('/archive'))return {isCurrent:()=>true,response:new Response(JSON.stringify(info))}
        if(!request.headers)return {isCurrent:()=>true,response:new Response(gzip,{headers:{ETag:etag,'Content-Type':'application/gzip','Content-Length':String(gzip.length),'X-HMem-Archive-Generation':generation,'X-HMem-Archive-SHA256':digest(gzip),'X-HMem-Archive-Raw-Bytes':String(raw.length),'X-HMem-Archive-Raw-SHA256':digest(raw)}})}
        assert.deepEqual(request.headers,{'If-None-Match':etag});assert.equal(workers[0].terminated,true);assert.equal(locked,true);revalidateEntered=true;order.push('authorized-conditional-get')
        return new Promise(resolve=>{revalidateRelease=()=>resolve({isCurrent:()=>!request.signal.aborted,response:scenario==='revalidate200'?new Response(gzip,{headers:{ETag:etag}}):scenario==='revalidate401'?new Response(null,{status:401}):new Response(null,{status:304,headers:{ETag:scenario==='changed-etag'?'"changed"':etag}})})})
      }
    })
    const until=async predicate=>{for(let turn=0;turn<100&&!predicate();turn++)await new Promise(resolve=>setImmediate(resolve));assert.ok(predicate(),'bounded phase '+scenario)}
    try{
      viewer.configure(config);await until(()=>workers[0]?.releaseDetach)
      const old=workers[0],oldCallback=old.onmessage,oldStamp=stage.manifest
      assert.equal(workers.length,1);assert.equal(locked,true);assert.equal(revalidateEntered,false);assert.ok(!events.some(event=>event.kind==='sealed-page'))
      if(scenario==='cancel-detach'){viewer.configure(null);await new Promise(resolve=>setImmediate(resolve));assert.equal(locked,true);assert.equal(deleted,false)}
      old.releaseDetach()
      if(scenario==='cancel-detach'){await until(()=>!viewer.snapshot().active);assert.equal(workers.length,1);continue}
      await until(()=>revalidateEntered);assert.equal(workers.length,1);assert.equal(locked,true,'gap recovery cannot acquire bridge custody')
      if(scenario==='auth-revalidate'){viewer.retireAuthority();await new Promise(resolve=>setImmediate(resolve));assert.equal(locked,true);assert.equal(deleted,false);assert.equal(physical,1)}
      revalidateRelease()
      if(['revalidate200','revalidate401','changed-etag','auth-revalidate','constructor-failure'].includes(scenario)){
        await until(()=>!viewer.snapshot().active);assert.equal(workers.length,1);assert.ok(!events.some(event=>event.kind==='sealed-page'));if(scenario!=='auth-revalidate')assert.ok(events.some(event=>event.kind==='sealed-error'));continue
      }
      await until(()=>workers[1]?.releaseAdopt);assert.equal(locked,true)
      const pageCount=events.filter(event=>event.kind==='sealed-page').length
      oldCallback({data:{...oldStamp,workspace,session,generation,load:1,authEpoch:0,incarnation:'stale',kind:'progress',phase:'ready',rawBytes:'999'}})
      assert.ok(!events.some(event=>event.rawBytes==='999'))
      if(scenario==='cancel-adopt'){viewer.retireAuthority();await new Promise(resolve=>setImmediate(resolve));assert.equal(locked,true);assert.equal(workers[1].terminated,undefined)}
      workers[1].releaseAdopt()
      if(scenario==='success'){await until(()=>events.some(event=>event.kind==='sealed-page'));assert.equal(physical,0);assert.equal(workers.length,2);assert.equal(locked,true);viewer.configure(null)}
      await until(()=>!viewer.snapshot().active)
      if(scenario==='cancel-adopt')assert.equal(events.filter(event=>event.kind==='sealed-page').length,pageCount)
    }finally{
      viewer.configure(null);workers[0]?.releaseDetach?.();revalidateRelease?.();workers[1]?.releaseAdopt?.();await until(()=>!viewer.snapshot().active)
      assert.equal(locked,false);assert.equal(deleted,true);assert.equal(physical,0);assert.ok(maxPhysical<=2);assert.equal(order.at(-1),'lock-released')
    }
  }
})


test('ordinary archive pages and fresh owners cannot inherit hidden search or offset options',async()=>{
 const events=[],listeners=new Map(),workers=[];let viewer,requestId=0
 let config={active:true,stamp:'embedded',load:1,workspace,session,authority:'principal',apiUrl:''}
 const document={getElementById:id=>id==='sealed-search'?{value:'needle'}:id==='sealed-position'?{value:'4096'}:{replaceChildren(){}},querySelectorAll:()=>[],addEventListener:(name,handler)=>listeners.set(name,handler)}
 class Worker{
  pages=[]
  reply(message,value){this.onmessage?.({data:{...message.stamp,request:message.request,...value}})}
  postMessage(message){if(message.kind==='interrupt')return;if(message.kind==='page'){this.pages.push(message);return}queueMicrotask(()=>this.reply(message,{kind:message.kind==='retire'?'cleaned':'ack'}))}
  terminate(){this.terminated=true}
 }
 const raw=Buffer.from('0\n'),gzip=gzipSync(raw)
 viewer=createPhaseBridgeFixture({document,window:{setTimeout,clearTimeout,crypto:globalThis.crypto,addEventListener(){}},workerFactory:()=>{const worker=new Worker();workers.push(worker);return worker},send:event=>{events.push(event);if(event.kind==='sealed-demand')queueMicrotask(()=>viewer.configure(config,{id:++requestId,sequence:event.sequence}))},fetchArchive:async url=>({isCurrent:()=>true,response:url.endsWith('/archive')?new Response(JSON.stringify(manifest(raw,gzip))):new Response(gzip,{headers:{'Content-Type':'application/gzip','Content-Length':String(gzip.length),'X-HMem-Archive-Generation':generation,'X-HMem-Archive-SHA256':digest(gzip),'X-HMem-Archive-Raw-Bytes':String(raw.length),'X-HMem-Archive-Raw-SHA256':digest(raw)}})})})
 const until=async fn=>{for(let i=0;i<100&&!fn();i++)await new Promise(r=>setImmediate(r));assert.ok(fn(),'bounded ordinary page ownership')}
 const click=(action,stamp=config.stamp)=>listeners.get('click')({target:{closest:()=>({getAttribute:name=>name==='data-sealed-action'?action:name==='data-sealed-owner'?stamp:name==='data-sealed-key'?'a':name==='data-sealed-part'?'0':null})},preventDefault(){}})
 const respond=(worker,index)=>worker.reply(worker.pages[index],{kind:'page',rows:[{key:'a'}],first:'a',last:'z',more:true})
 try{
  viewer.configure(config);await until(()=>workers.at(-1)?.pages.length===1);const worker=workers.at(-1);respond(worker,0);await until(()=>events.some(e=>e.kind==='sealed-page'))
  click('search');await until(()=>worker.pages.length===2);assert.deepEqual(worker.pages[1].page,{query:'needle'});respond(worker,1);await until(()=>events.filter(e=>e.kind==='sealed-page').length===2)
  click('body');await until(()=>worker.pages.length===3);assert.equal(worker.pages[2].page.query,'needle');respond(worker,2);await until(()=>events.filter(e=>e.kind==='sealed-page').length===3)
  click('next');await until(()=>worker.pages.length===4);assert.deepEqual(worker.pages[3].page,{after:'z'});respond(worker,3);await until(()=>events.filter(e=>e.kind==='sealed-page').length===4)
  click('position');await until(()=>worker.pages.length===5);assert.deepEqual(worker.pages[4].page,{offset:'4096'});respond(worker,4);await until(()=>events.filter(e=>e.kind==='sealed-page').length===5)
  click('first');await until(()=>worker.pages.length===6);assert.deepEqual(worker.pages[5].page,{});respond(worker,5);await until(()=>events.filter(e=>e.kind==='sealed-page').length===6)
  config={...config,stamp:'workspace',load:2};viewer.configure(config);await until(()=>workers.at(-1)!==worker&&workers.at(-1).pages.length===1)
  assert.equal(worker.terminated,true);assert.deepEqual(workers.at(-1).pages[0].page,{})
  click('search','embedded');assert.equal(workers.at(-1).pages.length,1,'retired embedded action cannot filter workspace owner')
  respond(workers.at(-1),0);await until(()=>events.some(e=>e.kind==='sealed-page'&&e.stamp==='workspace'))
 }finally{viewer.configure(null);await until(()=>workers.every(w=>w.terminated))}
})
