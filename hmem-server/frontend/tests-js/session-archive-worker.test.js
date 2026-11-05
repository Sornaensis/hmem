import test from 'node:test'
import assert from 'node:assert/strict'
import {gzipSync} from 'node:zlib'
import {createHash} from 'node:crypto'
import {createArchiveFramer,archiveChunkOffset,createArchiveEngine,gzipPipeline} from '../src/session-archive-worker.js'
import {BATCH_ROWS,IO_BYTES,positionKey} from '../src/session-archive-store.js'
import {blocksFor,createRecordFramer,semanticTransition,prettyPayloadSlice,presentation,visibleBlocks,payloadTokens,MAX_RECORD_BYTES} from '../src/session-viewer.js'
import {createSealedViewer} from '../src/session-save.js'
import {copilotRecords,piRecords} from './support/native-session-fixtures.mjs'

const phaseFixtureETag='"fixture-authority-bound-archive"'
function fakeWorkerReply(worker,message,value){
  const chunk=message.kind==='chunk'&&value.kind==='ack'?{bytes:message.bytes}:{}
  worker.onmessage?.({data:{...message.stamp,request:message.request,...chunk,...value}})
}
// Fake transports still execute both production bridge incarnations. Disk and
// decoder ownership is tested separately against the real store/native Worker.
function createPhaseBridgeFixture(options) {
  let stage
  const attributes={},thin={scrollTop:0,scrollHeight:0,clientHeight:0,replaceChildren(){}},prepared=new WeakSet()
  const viewport=()=>{const candidate=options.document.getElementById?.('session-records'),node=candidate?.replaceChildren?candidate:thin
    if(!prepared.has(node)){prepared.add(node);const get=node.getAttribute?.bind(node);node.getAttribute=name=>get?.(name) ?? attributes[name] ?? null;node.querySelectorAll ??= ()=>[];node.querySelector ??= ()=>null;node.setAttribute ??= (name,value)=>{attributes[name]=value}}
    return node
  }
  // These phase fixtures publish their accepted window synchronously; real
  // stamped DOM/position barriers remain covered by the mounted browser suite.
  return createSealedViewer({...options,document:{...options.document,getElementById:id=>id==='session-records'?viewport():options.document.getElementById?.(id)},window:{...options.window,requestAnimationFrame:options.window.requestAnimationFrame || (callback=>setImmediate(callback))},send:event=>{
    if(event.kind==='sealed-page')Object.assign(attributes,{'data-sealed-dom-owner':event.stamp,'data-sealed-dom-load':String(event.load),'data-sealed-page-request':String(event.request),'data-sealed-page-first':event.first,'data-sealed-page-last':event.last,'data-sealed-first':event.first,'data-sealed-last':event.last})
    options.send(event)
  },custodyFactory:async()=>async()=>{},deleteStage:async()=>{},
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
    constructor(codec){contexts++;const decoder=new Native(codec);return {readable:decoder.readable,writable:{getWriter(){const writer=decoder.writable.getWriter();return {closed:writer.closed,write(bytes){writes.push(bytes.length);return writer.write(bytes)},close:()=>writer.close(),abort:error=>writer.abort(error),releaseLock:()=>writer.releaseLock()}}}}}
  }
  try{
    const source=new ReadableStream({start(controller){controller.enqueue(gzip.subarray(0,5003));controller.enqueue(gzip.subarray(5003));controller.close()}})
    const modes=[],getReader=source.getReader.bind(source)
    source.getReader=options=>{modes.push(options?.mode??'default');return getReader(options)}
    pipeline=gzipPipeline(source);completion=pipeline.completion.then(()=>({}),error=>({error}));cleanup=pipeline.cleanup.then(()=>({}),error=>({error}))
    reader=pipeline.readable.getReader();const hash=createHash('sha256');let bytes=0
    while(true){const part=await reader.read();if(part.done)break;hash.update(part.value);bytes+=part.value.length}
    assert.deepEqual(await completion,{});assert.deepEqual(await cleanup,{})
    assert.deepEqual(modes,['byob','default']);assert.equal(source.locked,false)
    assert.equal(contexts,1);assert.ok(writes.length>1);assert.ok(writes.every(length=>length>0&&length<=4096));assert.equal(writes.reduce((sum,size)=>sum+size,0),gzip.length)
    assert.equal(bytes,raw.length);assert.equal(hash.digest('hex'),digest(raw))
  }finally{reader?.releaseLock();globalThis.DecompressionStream=Native}
})

function useCopyingGzipSink(){
  const native=globalThis.DecompressionStream
  globalThis.DecompressionStream=class {
    constructor(codec){assert.equal(codec,'gzip');return new TransformStream({transform(bytes,controller){controller.enqueue(bytes.slice())}})}
  }
  return ()=>{globalThis.DecompressionStream=native}
}

test('gzip byte stream reuses one returned buffer across short, full and terminal reads',{timeout:5000},async()=>{
  let seed=0x51a7
  const bytes=Buffer.alloc(2*IO_BYTES),lengths=[]
  for(let at=0;at<bytes.length;at++){seed^=seed<<13;seed^=seed>>>17;seed^=seed<<5;bytes[at]=seed&255}
  let at=0,previous,tail
  const source=new ReadableStream({type:'bytes',pull(controller){
    const request=controller.byobRequest,view=request.view
    assert.equal(view.byteLength,IO_BYTES);assert.equal(view.byteOffset,0)
    if(previous){assert.equal(previous.byteLength,0);assert.equal(view.at(-1),tail)}
    else view.fill(0xa5)
    if(at===bytes.length){controller.close();request.respond(0);return}
    const length=Math.min(lengths.length===0?4099:IO_BYTES,bytes.length-at)
    view.set(bytes.subarray(at,at+length));at+=length;tail=view.at(-1);previous=view
    lengths.push(length);request.respond(length)
  }})
  const restore=useCopyingGzipSink()
  try{
    const pipeline=gzipPipeline(source)
    const [forwarded]=await Promise.all([new Response(pipeline.readable).arrayBuffer(),pipeline.completion,pipeline.cleanup])
    assert.deepEqual(Buffer.from(forwarded),bytes)
    assert.equal(lengths[0],4099);assert.ok(lengths.includes(IO_BYTES));assert.ok(lengths.at(-1)<IO_BYTES)
    assert.equal(at,bytes.length);assert.equal(source.locked,false)
  }finally{restore()}
})

test('native gzip verifies the logical bytes from a staged Blob byte reader',async()=>{
  const raw=Buffer.from('λ😀 strict gzip\n'.repeat(101)),source=new Blob([gzipSync(raw)]).stream()
  const modes=[],getReader=source.getReader.bind(source)
  source.getReader=options=>{modes.push(options?.mode??'default');return getReader(options)}
  const pipeline=gzipPipeline(source)
  const [decoded]=await Promise.all([new Response(pipeline.readable).arrayBuffer(),pipeline.completion,pipeline.cleanup])
  assert.deepEqual(Buffer.from(decoded),raw);assert.deepEqual(modes,['byob']);assert.equal(source.locked,false)
})

test('gzip byte read rejection closes the owned pipeline without retrying the default reader',async()=>{
  for(const failure of [new TypeError('actual_byte_read_failed'),NaN]){
    const modes=[],header=gzipSync('partial gzip').subarray(0,10)
    let pulls=0
    const source=new ReadableStream({type:'bytes',pull(controller){
      if(++pulls===1){const request=controller.byobRequest;request.view.set(header);request.respond(header.length)}
      else controller.error(failure)
    }})
    const getReader=source.getReader.bind(source)
    source.getReader=options=>{modes.push(options?.mode??'default');return getReader(options)}
    const pipeline=gzipPipeline(source)
    await Promise.all([
      assert.rejects(new Response(pipeline.readable).arrayBuffer()),
      assert.rejects(pipeline.completion,error=>Object.is(error,failure)),
      pipeline.cleanup
    ])
    assert.equal(pulls,2);assert.deepEqual(modes,['byob']);assert.equal(source.locked,false)
  }
})

test('gzip cancellation joins the actual byte reader even when its pending read resolves as EOF',{timeout:5000},async()=>{
  const restore=useCopyingGzipSink()
  try{for(const reason of [undefined,'retired',NaN])for(const hookFails of [false,true]){
    const header=gzipSync('partial gzip').subarray(0,10)
    let pulls=0,cancels=0,complete=false,readPending,startedCancel,finishCancel
    const pendingRead=new Promise(resolve=>{readPending=resolve})
    const cancelStarted=new Promise(resolve=>{startedCancel=resolve})
    const heldCancel=new Promise((resolve,reject)=>{finishCancel=()=>hookFails?reject(reason):resolve()})
    const source=new ReadableStream({type:'bytes',pull(controller){
      if(++pulls===1){const request=controller.byobRequest;request.view.set(header);request.respond(header.length)}
      else readPending()
    },cancel(actual){cancels++;startedCancel(actual);return heldCancel}})
    const pipeline=gzipPipeline(source),reader=pipeline.readable.getReader()
    const completion=pipeline.completion.then(()=>({failed:false}),error=>({failed:true,error})).finally(()=>{complete=true})
    const cleanup=pipeline.cleanup.then(()=>({}),error=>({error}))
    try{
      const first=await reader.read();assert.equal(first.done,false)
      assert.deepEqual(Buffer.from(first.value),header)
      const output=reader.read()
      await pendingRead
      await reader.cancel(reason)
      assert.equal(await cancelStarted,reason)
      assert.deepEqual(await output,{done:true,value:undefined})
      assert.equal(complete,false);assert.equal(source.locked,true)
      finishCancel()
      const result=await completion
      assert.equal(result.failed,true)
      if(hookFails){
        assert.equal(result.error.message,'archive_cleanup_failed');assert.equal(result.error.errors.at(-1),reason)
        assert.deepEqual(await cleanup,{error:reason});assert.equal(source.locked,true)
      }else{
        assert.deepEqual(await cleanup,{});assert.equal(source.locked,false)
      }
      assert.equal(cancels,1)
    }finally{finishCancel();reader.releaseLock()}
  }}finally{restore()}
})

test('gzip rejects invalid BYOB views before writing them and releases the sole reader',async()=>{
  for(const value of [new Uint16Array(1),new Uint8Array(IO_BYTES+1),new Uint8Array(new ArrayBuffer(IO_BYTES),1,1),new Uint8Array(new ArrayBuffer(IO_BYTES),0,0)]){
    let cancelled=0,released=0
    const source={getReader(options){
      assert.deepEqual(options,{mode:'byob'})
      return {read:async()=>({done:false,value}),closed:Promise.resolve(),cancel:async()=>{cancelled++},releaseLock:()=>{released++}}
    }}
    const pipeline=gzipPipeline(source)
    await Promise.all([
      assert.rejects(new Response(pipeline.readable).arrayBuffer(),/invalid_gzip_source_view/),
      assert.rejects(pipeline.completion,/invalid_gzip_source_view/),
      pipeline.cleanup
    ])
    assert.equal(cancelled,1);assert.equal(released,1)
  }
})
test('strict archive framing preserves per-record BOM, UTF-8, CRLF, LF and final bytes across every cut',()=>{
  const lexical='{"type": "future", "value": 9007199254740993, "λ": "😀"}'
  const records=['\ufeff{"text":"λ😀"}\r\n','\ufeff{"type":"event_msg","payload": '+lexical+'}\n','{"text":"a\ufeffb"}\n','\ufeff{"text":"last λ😀"}']
  const input=Buffer.from(records.join(''))
  let offset=0
  const coordinates=records.map((record,index)=>{
    const length=Buffer.byteLength(record),start=offset;offset+=length
    return [String(start),String(offset),['\r\n','\n','\n',''][index],length]
  })
  for(let cut=0;cut<=input.length;cut++){
    const framer=createArchiveFramer(),rows=[...framer.push(input.subarray(0,cut)),...framer.push(input.subarray(cut),true)]
    assert.deepEqual(rows.map(row=>[row.offset,row.end,row.delimiter,row.recordBytes]),coordinates)
    assert.equal(framer.through,BigInt(input.length))
    assert.equal(rows[0].text,'λ😀');assert.equal(blocksFor(rows[1])[0].text,lexical)
    assert.equal(rows[2].text,'a\ufeffb');assert.equal(rows[3].text,'last λ😀')
  }
  for(const input of [Buffer.from('{'),Buffer.from([34,0xc3,34]),Buffer.from('\n'),Buffer.from('\ufeff\ufeff{"text":"twice"}\n')])assert.throws(()=>[...createArchiveFramer().push(input,true)])
})

test('a malformed record in a complete slab fails at its own exact frame after the valid prefix',()=>{
  const prefix=Buffer.from('{"text":"λ😀"}\r\n')
  const tail=Buffer.from('{"text":"never reaches this record"}\n')
  for(const [invalid,error] of [[Buffer.from([34,0xc3,10]),TypeError],[Buffer.from('{\n'),/invalid_json_record/]]){
    const framer=createArchiveFramer(),rows=framer.push(Buffer.concat([prefix,invalid,tail]),true)
    assert.equal(rows.next().value.text,'λ😀')
    assert.throws(()=>rows.next(),error)
    assert.equal(framer.through,BigInt(prefix.length+invalid.length))
    assert.equal(rows.next().done,true)
  }
})

test('cross-slab CRLF and final records retain the exact raw byte limit',()=>{
  const take=input=>{
    const framer=createArchiveFramer(),rows=[]
    for(let at=0;at<input.length;at+=IO_BYTES)rows.push(...framer.push(input.subarray(at,at+IO_BYTES),at+IO_BYTES>=input.length))
    return rows
  }
  const body='x'.repeat(MAX_RECORD_BYTES-11)
  for(const delimiter of ['', '\r\n']){
    const exact=Buffer.from(JSON.stringify({text:body})+delimiter)
    assert.equal(exact.length,MAX_RECORD_BYTES+delimiter.length)
    const [row]=take(exact)
    assert.equal(row.recordBytes,exact.length);assert.equal(row.end,String(exact.length));assert.equal(row.delimiter,delimiter)
    assert.throws(()=>take(Buffer.from(JSON.stringify({text:body+'x'})+delimiter)),/record_limit/)
  }
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

test('archive single-text projection retains exact framing, timestamps and fallback metadata',()=>{
  const repeated='2024-02-29t23:59:59.12345+23:59',invalid='2023-02-29T00:00:00Z'
  const long='2024-01-01T00:00:00.'+'0'.repeat(145)+'z'
  const message=(content,timestamp,extra={},role='assistant')=>({type:'response_item',timestamp,payload:{type:'message',role,content},...extra})
  const text=(type,text)=>({type,text})
  const values=[
    message([text('output_text','λ😀\ud800')],repeated,{},'x'.repeat(270)),
    message([text('input_text','same instant')],undefined),
    message([text('text','repeated')],repeated),
    message([text('text','invalid')],invalid),
    message([text('text','long fraction')],long),
    message([text('text','same invalid')],invalid),
    message([text('text','valid again')],repeated),
    message([text('text','ephemeral')],repeated,{ephemeral:true}),
    message([text('output_text',42)],repeated),
    message([text('text','first'),text('output_text','second')],repeated),
    message('plain string content',repeated),
    {type:'session',version:3},
    message([text('output_text','Pi precedence')],repeated,{id:'pi',parentId:null}),
    message([text('output_text','non-Pi fallback')],repeated,{parentId:null})
  ]
  const records=values.map((value,index)=>JSON.stringify(value)+(index===values.length-1?'':index%2?'\r\n':'\n'))
  const input=Buffer.from(records.join('')),cut=input.indexOf(Buffer.from('λ'))+1,framer=createArchiveFramer()
  const rows=[...framer.push(input.subarray(0,cut)),...framer.push(input.subarray(cut),true)]
  let offset=0,piVersion=null
  for(const [index,row] of rows.entries()){
    const value=values[index],raw=JSON.stringify(value),length=Buffer.byteLength(records[index])
    if(value.type==='session')piVersion=value.version
    const reference=presentation(JSON.parse(raw),raw,{piVersion})
    assert.deepEqual([row.type,row.role,row.time,row.piVersion],[reference.type,reference.role,reference.time,reference.piVersion])
    assert.deepEqual([row.offset,row.end,row.delimiter,row.recordBytes],
      [String(offset),String(offset+length),records[index].slice(raw.length),length])
    if(row.type==='conversation'){
      assert.deepEqual(blocksFor(row),blocksFor(reference))
      assert.equal(Object.hasOwn(row,'text'),false)
    }
    offset+=length
  }
  assert.deepEqual(rows.slice(0,7).map(row=>row.time),[repeated,'',repeated,'',long,'',repeated])
  assert.equal(rows[7].type,'unknown')
  assert.ok(blocksFor(rows[12])[0].nativeContext)
  assert.equal(framer.through,BigInt(input.length))
})

test('absent archive tool payloads keep exact null text and labels without a JSON round trip',()=>{
  const values=[
    [{type:'response_item',payload:{type:'function_call',name:'mcp__check',arguments:null}},'tool','Call · check'],
    [{type:'response_item',payload:{type:'function_call_output',call_id:'request'}},'result','Result · request']
  ]
  const parse=JSON.parse,stringify=JSON.stringify
  const raw=values.map(([value])=>stringify(value))
  JSON.parse=()=>{throw Error('unexpected null JSON parse')}
  JSON.stringify=()=>{throw Error('unexpected null JSON stringify')}
  let blocks
  try{blocks=values.map(([value],index)=>blocksFor(presentation(value,raw[index],{archive:true}))[0])}
  finally{JSON.parse=parse;JSON.stringify=stringify}
  assert.deepEqual(blocks,values.map(([,kind,label])=>({kind,text:'null',label,tokens:[]})))
})

test('archive block refs are per-row, stable and private while exact live semantics remain unchanged',async()=>{
  const event={type:'event_msg',timestamp:'2026-01-01T00:00:00Z',payload:{type:'agent_message',text:'same λ😀'}}
  const canonical={type:'response_item',timestamp:'2026-01-01T00:00:00.050Z',payload:{type:'message',role:'assistant',content:[{type:'text',text:'same λ😀'}]}}
  const values=[event,canonical,...Array.from({length:30},(_,index)=>({type:'response_item',payload:{type:'message',role:'assistant',content:[{type:'text',text:'row '+index}]}}))]
  for(const value of values){const raw=JSON.stringify(value);assert.equal(JSON.stringify(blocksFor(presentation(value,raw,{archive:true}))),presentation(value,raw).text)}
  const input=Buffer.from(values.map(value=>JSON.stringify(value)).join('\n')),stringify=JSON.stringify
  let rows
  JSON.stringify=function(value,...rest){
    if(Array.isArray(value)&&value[0]?.kind==='text')throw Error('archive_blocks_serialized')
    return stringify(value,...rest)
  }
  try{rows=[...createArchiveFramer().push(input,true)]}finally{JSON.stringify=stringify}
  const normalized=new Set(rows.map(row=>JSON.stringify(blocksFor(row)))),parse=JSON.parse,arrays=new Set(),visible=[];let attempts=0,previous,anchor
  JSON.parse=function(value,...rest){if(normalized.has(value)){attempts++;throw Error('normalized_blocks_reparsed')}return parse(value,...rest)}
  try{for(const row of rows){
    const pairs=visibleBlocks(row),symbols=Object.getOwnPropertySymbols(row)
    assert.equal(symbols.length,2);assert.ok(symbols.every(symbol=>!Object.getOwnPropertyDescriptor(row,symbol).enumerable))
    assert.ok(!arrays.has(pairs));arrays.add(pairs)
    for(let repeat=0;repeat<100;repeat++)assert.equal(visibleBlocks(row),pairs)
    assert.equal(pairs.length,1);assert.equal(Object.getOwnPropertySymbols(row).length,2)
    const clone=structuredClone(row);assert.deepEqual(Object.getOwnPropertySymbols(clone),[]);assert.ok(!('text' in clone));assert.ok(!Object.values(clone).some(Array.isArray))
    assert.equal(JSON.stringify(row),JSON.stringify(clone))
    const next=semanticTransition(row,previous,anchor);previous=next.previous;anchor=next.anchor
    if(next.retired!==null)visible.splice(visible.findIndex(value=>value.offset===next.retired),1)
    if(row.category&&!next.hidden)visible.push(row)
  }}finally{JSON.parse=parse}
  assert.equal(attempts,0)
  const live=createRecordFramer().push(0,input,true).rows
  assert.deepEqual(visible.map(row=>[Number(row.offset),Number(row.presentationOffset),JSON.stringify(blocksFor(row)),row.activityAnchors]),live.map(row=>[row.offset,row.presentationOffset,row.text,row.activityAnchors]))
  await indexed(values,async(engine,store)=>{
    for(const [table,entries]of Object.entries(store.tables))for(const value of entries.values()){
      assert.deepEqual(Object.getOwnPropertySymbols(value),[]);assert.ok(!('text' in value))
      if(table==='records'){
        assert.deepEqual(Object.keys(value).sort(),['key','spans'])
        assert.ok(value.spans.length<=BATCH_ROWS&&value.spans.every(Number.isSafeInteger))
      }else assert.ok(!Object.values(value).some(Array.isArray),table+' must not persist block arrays')
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
  const [block]=blocksFor(row)
  assert.ok(block.text.length<1048576);assert.equal(block.tokens.length,0)
})
function memoryStore(){const tables=Object.fromEntries(['records','pieces','groups','meta','disclosures'].map(name=>[name,new Map()]));const compressed=[],raw=[];let published=false,closed=false
  // Keep stable input buffers in the Node codec adapter. Chromium exercises
  // the actual staged Blob byte stream in the native archive fixtures.
  const compressedStream=async()=>new ReadableStream({start(controller){for(const bytes of compressed)controller.enqueue(bytes);controller.close()}})
  return {tables,get published(){return published},get closed(){return closed},put(name,value){tables[name].set(value.key,structuredClone(value))},async get(name,key){return tables[name].get(key)},async flush(){},writeCompressed(_,bytes){compressed.push(bytes.slice())},writeRaw(_,bytes){raw.push(bytes.slice())},async detach(){closed=true},compressedStream,readRaw(start,end){return Buffer.concat(raw).subarray(Number(start),Number(end))},async publish(){published=true},async closeAndDelete(){closed=true},async scan(name,{after,before,limit}){let rows=[...tables[name].values()].sort((a,b)=>a.key.localeCompare(b.key));if(after)rows=rows.filter(row=>row.key>after);if(before)return rows.filter(row=>row.key<before).slice(-limit);return rows.slice(0,limit)},async readPageMetadata(pieces){return pieces.map(piece=>({open:tables.disclosures.get(piece.key)?.open || false,groupOpen:piece.group?tables.disclosures.get('group:'+piece.group)?.open || false:false,count:piece.group?tables.groups.get(piece.group)?.count || '0':'0',endTime:piece.group?tables.groups.get(piece.group)?.endTime || '':''}))},async disclosure(key,open){tables.disclosures.set(key,{key,open})}}
}
function indexedRecords(store) {
  return [...store.tables.records.values()].flatMap(({key,spans})=>{
    let offset=BigInt(key)
    return spans.map(span=>{
      const start=offset
      offset+=BigInt(Math.floor(span/4))
      return [start,offset,['','\n','\r\n'][span%4]]
    })
  })
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
    await engine.finish();assert.equal(atFirstMicrotask,64);assert.equal(puts,65)
    assert.equal(engine.snapshot().records,'64');assert.equal(store.published,true);assert.equal(store.tables.pieces.size,64)
  })
})

test('fresh activity groups batch without reads and keep carried and paired occurrence counts',async()=>{
  const timestamp='2026-01-01T00:00:00Z'
  const message=text=>({type:'response_item',payload:{type:'message',role:'assistant',content:[{type:'output_text',text}]}})
  const call=name=>({type:'response_item',payload:{type:'function_call',name,arguments:'{}'}})
  const pairedEvent={type:'event_msg',timestamp,payload:{type:'agent_reasoning',text:'{\n  "a": 1\n}'}}
  const canonicalTime='2026-01-01T00:00:00.050Z'
  const pairedRecord={type:'response_item',timestamp:canonicalTime,payload:{type:'reasoning',summary:{a:1}}}
  const values=[message('First'),call('one'),call('two'),message('Reset'),pairedEvent,pairedRecord,message('Reset again'),call('three')]
  let groupReads=0
  await stagedEngine(values,store=>{
    const get=store.get
    store.get=(table,key)=>{if(table==='groups')groupReads++;return get(table,key)}
  },async(engine,store)=>{
    await engine.finish()
    assert.equal(groupReads,0,'fresh monotonic groups must not force an IndexedDB lookup and flush')
    const groups=[...store.tables.groups.values()]
    assert.deepEqual(groups.map(value=>value.count),['2','1','1'])
    assert.equal(groups[1].endTime,canonicalTime)
    const pieces=[...store.tables.pieces.values()].filter(piece=>piece.group)
    assert.deepEqual(pieces.map(piece=>piece.group),[groups[0].key,groups[0].key,groups[1].key,groups[2].key])
    assert.equal(store.published,true)
  })
})

test('Pi context pieces preserve a carried activity anchor and visible text resets it inside a record',async()=>{
  const thinking=text=>({type:'thinking',thinking:text})
  const pi=(id,parentId,...content)=>({type:'message',id,parentId,message:{role:'assistant',content}})
  const values=[{type:'session',version:3},pi('a',null,thinking('first')),
    pi('b','a',thinking('carried'),{type:'text',text:'separator'},thinking('new')),
    pi('c','b',thinking('last'))]
  await indexed(values,async(engine,store)=>{
    const {rows}=await engine.page(),groups=[...store.tables.groups.values()]
    assert.deepEqual(rows.map(row=>row.group),['',groups[0].key,'',groups[0].key,'',groups[1].key,'',groups[1].key])
    assert.deepEqual(rows.map(row=>row.count),['0','2','0','2','0','2','0','2'])
    assert.deepEqual(groups.map(group=>group.count),['2','2'])
  })
})

test('a hidden paired event may establish the anchor of a later visible activity group',async()=>{
  const timestamp='2026-01-01T00:00:00Z',endTime='2026-01-01T00:00:01Z'
  const canonical={type:'conversation',role:'assistant',timestamp,
    text:JSON.stringify([{kind:'thinking',text:'reason',label:'',nativeContext:true}])}
  const hidden={type:'event_msg',timestamp,payload:{type:'agent_reasoning',text:'reason'}}
  const visible={type:'response_item',timestamp:endTime,payload:{type:'function_call',name:'next',arguments:'{}'}}
  const group=Buffer.byteLength(JSON.stringify(canonical)+'\n')+':0'
  await indexed([canonical,hidden,visible],async(engine,store)=>{
    const {rows}=await engine.page()
    assert.deepEqual(rows.map(row=>[row.kind,row.group,row.count,row.endTime]),[
      ['thinking','','0',''],['tool',group,'1',endTime]
    ])
    assert.deepEqual([...store.tables.groups.values()],[{key:group,count:'1',endTime}])
    assert.equal(indexedRecords(store).length,3)
  })
})

test('adjacent envelope pairing can replace only the one tail key at a full piece page',async()=>{
  const timestamp='2026-01-01T00:00:00Z'
  const event={type:'event_msg',timestamp,payload:{type:'agent_message',text:'paired'}}
  const canonical=(...texts)=>({type:'response_item',timestamp,payload:{type:'message',role:'assistant',content:texts.map(text=>({type:'text',text}))}})
  const visible=Array.from({length:BATCH_ROWS-1},(_,index)=>({text:'filler '+index}))
  const values=[...visible,event,canonical('paired'),{text:'separator'},canonical('paired'),event,{text:'separator'},event,canonical('paired','extra')]
  const writes=[]
  await stagedEngine(values,store=>{
    const put=store.put
    store.put=(table,value)=>{if(table==='pieces')writes.push(value);return put(table,value)}
  },async(engine,store)=>{
    await engine.finish()
    const eventPiece=writes[BATCH_ROWS-1],replacement=writes[BATCH_ROWS]
    assert.equal(replacement.key,eventPiece.key)
    assert.notEqual(replacement.offset,eventPiece.offset)
    assert.equal(store.tables.pieces.get(eventPiece.key).offset,replacement.offset)
    assert.equal(writes.length,BATCH_ROWS+7,'canonical-first suppresses its later event; two-block canonical cannot pair')
    assert.equal(store.tables.pieces.size,BATCH_ROWS+6)
    assert.ok(writes.slice(BATCH_ROWS+1).every((piece,index)=>piece.key>writes[BATCH_ROWS+index].key))
    const later=await engine.page({after:eventPiece.key})
    assert.deepEqual(later.rows.map(row=>row.body.text),['separator','paired','separator','paired','paired','extra'])
    assert.equal((await engine.hydrate(eventPiece.key,0)).body.text,'paired')
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
      assert.equal(first.rows[0].group,last.rows.at(-1).group);assert.equal(indexedRecords(store).length,1)
      assert.equal(events.filter(value=>value.phase==='ready').length,1)
    }finally{releaseGroup();releaseTerminal();await finishing.catch(()=>{})}
  })
})

test('held enqueue retirement and failed enqueue preserve failure and never publish',async()=>{
  for(const retire of [true,false]){
    let release,reject,held=false
    const gate=new Promise((yes,no)=>{release=yes;reject=no}),failure=Error('injected_flush_failure')
    await stagedEngine(Array.from({length:BATCH_ROWS+1},(_,index)=>({text:'row '+index})),store=>{
      const put=store.put
      store.put=(name,value)=>{if(name==='records'&&value.key!==positionKey(0n)&&!held){held=true;return gate.then(()=>put(name,value))}return put(name,value)}
    },async(engine,store,events)=>{
      const finishing=engine.finish();finishing.catch(()=>{})
      try{
        await untilStaged(()=>held);assert.equal(store.tables.pieces.size,BATCH_ROWS+1);assert.equal(store.published,false)
        if(retire){engine.retire();release();await assert.rejects(finishing,/retired_archive/)}
        else{reject(failure);await assert.rejects(finishing,error=>error===failure)}
        assert.equal(store.tables.pieces.size,BATCH_ROWS+1);assert.equal(store.published,false);assert.ok(!events.some(value=>value.phase==='ready'))
      }finally{release();await finishing.catch(()=>{})}
    })
  }
})

async function indexed(values,action){
  const prior=globalThis.Worker;globalThis.Worker=class{}
  const raw=Buffer.isBuffer(values)?values:Buffer.from(values.map(value=>JSON.stringify(value)).join('\n')+'\n'),gzip=gzipSync(raw),store=memoryStore(),engine=createArchiveEngine({openStore:async()=>store})
  try{await engine.start({name:'test',stamp:{workspace,session},manifest:manifest(raw,gzip)});for(let at=0;at<gzip.length;at+=65536)await engine.chunk(gzip.subarray(at,at+65536));await engine.finish();await action(engine,store)}
  finally{await engine.cleanup();globalThis.Worker=prior}
}

test('packed record pages cover every byte across the page boundary, including hidden and final records',async()=>{
  const expected=[],parts=[]
  let offset=0n
  for(let index=0;index<BATCH_ROWS+3;index++){
    const delimiter=index===BATCH_ROWS+2?'':index%2===0?'\r\n':'\n'
    const bytes=Buffer.from(JSON.stringify(index%2===0?{type:'metadata'}:{text:'λ😀 '+index})+delimiter)
    const end=offset+BigInt(bytes.length)
    expected.push([offset,end,delimiter]);parts.push(bytes);offset=end
  }
  await indexed(Buffer.concat(parts),async(engine,store)=>{
    const pages=[...store.tables.records.values()]
    assert.deepEqual(pages.map(({key,spans})=>[key,spans.length]),[[positionKey(0n),BATCH_ROWS],[positionKey(expected[BATCH_ROWS][0]),3]])
    assert.deepEqual(indexedRecords(store),expected)
    assert.equal(engine.snapshot().records,String(expected.length))
    assert.equal(engine.snapshot().rawBytes,String(offset))
    assert.equal(store.tables.pieces.size,Math.floor(expected.length/2))
  })
})

test('sanitized archive indexing verifies transformed receipts and content coordinates',async()=>{
  const prior=globalThis.Worker;globalThis.Worker=class{}
  const source=Buffer.from('{"text":"original opaque content"}\r\n{"text":"tail"}')
  const content=Buffer.from('{"text":"Encrypted Payload"}\r\n{"text":"tail"}'),gzip=gzipSync(content)
  const path=`/api/v1/workspaces/${workspace}/sessions/${session}/archive/${generation}`
  const info={...manifest(source,gzip),representation:'sanitized-v1',content_bytes:String(content.length),
    content_sha256:digest(content),policy_sha256:'a'.repeat(64),original_available:false,
    content_path:path+'/sanitized-content',evidence_path:path+'/sanitized-evidence'}
  try {
    for(const selected of [info,{...info,content_sha256:digest(source)}]){
      const store=memoryStore(),engine=createArchiveEngine({openStore:async()=>store})
      try {
        await engine.start({name:'sanitized',stamp:{workspace,session},manifest:selected})
        await engine.chunk(gzip)
        if(selected===info){
          await engine.finish()
          assert.deepEqual(store.readRaw('0',String(content.length)),content)
          assert.equal(engine.snapshot().rawBytes,String(content.length))
          assert.deepEqual(indexedRecords(store),[[0n,BigInt(content.indexOf(10)+1),'\r\n'],[BigInt(content.indexOf(10)+1),BigInt(content.length),'']])
        } else {
          await assert.rejects(engine.finish(),/raw_receipt_mismatch/)
          assert.equal(store.published,false)
        }
      } finally {await engine.cleanup()}
    }
  } finally {globalThis.Worker=prior}
})

test('native Copilot occurrences index and hydrate with the same visible blocks and exact raw source as live framing',async()=>{
 const values=[...copilotRecords,{type:'future.output',data:{content:{unrecognized:true}}}],raw=Buffer.from(values.map(value=>JSON.stringify(value)).join('\n')+'\n')
 const live=createRecordFramer().push(0,raw,true).rows
 await indexed(values,async(engine,store)=>{
  assert.deepEqual(store.readRaw('0',String(raw.length)),raw)
  const page=await engine.page(),expected=live.flatMap(row=>visibleBlocks(row).map(([index,block])=>({offset:String(row.offset),end:String(row.end),index,kind:block.kind,label:block.label,block})))
  assert.equal(page.rows.length,expected.length)
  for(let at=0;at<expected.length;at++){
   const piece=page.rows[at],want=expected[at]
   for(const field of ['offset','end','index','kind','label'])assert.equal(piece[field],want[field])
   const hydrated=await engine.hydrate(piece.key,0)
   assert.equal(hydrated.body.more,false)
   if(['tool','result','event'].includes(want.kind))assert.deepEqual(JSON.parse(hydrated.body.text),JSON.parse(want.block.text))
   else assert.equal(hydrated.body.text,want.block.text)
  }
  assert.equal(indexedRecords(store).length,values.length);assert.equal(page.more,false)
 })
})

test('pi version context, parent history and hidden display flags agree in live, archive pages and body rehydration',async()=>{
 for(const values of [piRecords,[{type:'session',version:99},piRecords[1]],piRecords.slice(5)]){
  const raw=Buffer.from(values.map(value=>JSON.stringify(value)).join('\n')+'\n'),live=createRecordFramer().push(0,raw,true).rows
  await indexed(values,async(engine,store)=>{
   assert.deepEqual(store.readRaw('0',String(raw.length)),raw)
   const expected=live.flatMap(row=>visibleBlocks(row).map(([index,block])=>({row,index,block}))),page=await engine.page()
   assert.equal(page.rows.length,expected.length)
   for(let at=0;at<expected.length;at++){
    const piece=page.rows[at],{row,index,block}=expected[at]
    assert.equal(piece.offset,String(row.offset));assert.equal(piece.index,index);assert.equal(piece.kind,block.kind);assert.equal(piece.label,block.label)
    const result=await engine.hydrate(piece.key,0)
    if(['tool','result','event'].includes(block.kind))assert.deepEqual(JSON.parse(result.body.text),JSON.parse(block.text))
    else assert.equal(result.body.text,block.text)
   }
   assert.equal(page.more,false)
  })
 }
})

test('sealed known failure-without-error controls hydrate as explicit failures and successful counterparts remain hidden',async()=>{
 const values=['hook.end','command.completed','permission.completed'].flatMap(type=>[{type,data:{success:true}},{type,data:{success:false}}])
 await indexed(values,async(engine,store)=>{
  const page=await engine.page();assert.equal(page.rows.length,3);assert.equal(indexedRecords(store).length,6)
  for(let at=0;at<page.rows.length;at++){const result=await engine.hydrate(page.rows[at].key,0);assert.match(result.body.text,/Transcript operation failed/);assert.ok(result.body.text.includes(values[at*2+1].type));assert.equal(result.body.more,false)}
 })
})

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
      postMessage(message,transfer){
        this.stamp=message.stamp
        if(message.kind==='interrupt'){this.engine?.retire();return}
        if(message.kind==='chunk'){
          const sent=message.bytes
          assert.deepEqual(transfer,[sent]);assert.equal(sent.byteLength,IO_BYTES)
          message=structuredClone(message,{transfer})
          assert.equal(sent.byteLength,0)
        }
        this.tail=this.tail.then(async()=>{
          let result={kind:'ack'}
          if(message.kind==='start'||message.kind==='adopt'){this.engine=createArchiveEngine({openStore:async()=>store,emit:value=>this.onmessage?.({data:{...value,request:message.request}})});if(message.kind==='adopt')await this.engine.adopt(message);else await this.engine.start(message)}
          else if(message.kind==='chunk'){
            const view=new Uint8Array(message.bytes)
            assert.equal(view.at(-1),this.chunks?231:0,'only one buffer is returned and reused')
            this.chunks=(this.chunks??0)+1
            await this.engine.chunk(view.subarray(0,message.length))
            view[view.length-1]=231
            result.bytes=structuredClone(message.bytes,{transfer:[message.bytes]})
            assert.equal(view.byteLength,0)
          }
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

test('archive transfer waits for the current chunk receipt and releases a retired acquisition without reuse',async()=>{
  const raw=Buffer.from('{"text":"transfer ownership"}\n'),gzip=gzipSync(raw),info=manifest(raw,gzip)
  const headers={'Content-Type':'application/gzip','Content-Length':String(gzip.length),'X-HMem-Archive-Generation':generation,
    'X-HMem-Archive-SHA256':digest(gzip),'X-HMem-Archive-Raw-Bytes':String(raw.length),'X-HMem-Archive-Raw-SHA256':digest(raw)}
  const config={active:true,stamp:'transfer-ownership',load:1,workspace,session,authority:'principal',apiUrl:''}
  for(const failure of ['missing','wrong-size','retired']){
    let viewer,worker,requestId=0
    const events=[]
    class HeldWorker {
      commands=[]
      postMessage(message,transfer){
        this.commands.push(message.kind)
        if(message.kind==='interrupt')return
        if(message.kind==='chunk'){
          assert.equal(message.bytes.byteLength,IO_BYTES)
          this.chunk=structuredClone(message,{transfer})
          assert.equal(message.bytes.byteLength,0)
          return
        }
        queueMicrotask(()=>fakeWorkerReply(this,message,{kind:message.kind==='retire'?'cleaned':'ack'}))
      }
      reply(extra){
        const {stamp,request}=this.chunk
        this.onmessage({data:{...stamp,request,kind:'ack',...extra}})
      }
      terminate(){this.terminated=true}
    }
    viewer=createPhaseBridgeFixture({
      document:{getElementById:()=>null,querySelectorAll:()=>[],addEventListener(){}},
      window:{setTimeout,clearTimeout,crypto:globalThis.crypto,addEventListener(){}},workerFactory:()=>{worker=new HeldWorker();return worker},
      send:event=>{events.push(event);if(event.kind==='sealed-demand')queueMicrotask(()=>viewer.configure(config,{id:++requestId,sequence:event.sequence}))},
      fetchArchive:async url=>({isCurrent:()=>true,response:url.endsWith('/archive')?new Response(JSON.stringify(info)):new Response(new ReadableStream({start(controller){
        controller.enqueue(Uint8Array.from(gzip.subarray(0,8)))
        controller.enqueue(Uint8Array.from(gzip.subarray(8)))
        controller.close()
      }}),{headers})})
    })
    try{
      viewer.configure(config)
      await untilStaged(()=>worker?.chunk)
      const {chunk}=worker
      assert.equal(chunk.length,8)
      worker.reply({request:chunk.request+1,bytes:new ArrayBuffer(IO_BYTES)})
      worker.reply({incarnation:'stale',bytes:new ArrayBuffer(IO_BYTES)})
      await new Promise(resolve=>setImmediate(resolve))
      assert.deepEqual(worker.commands,['start','chunk'])
      if(failure==='retired'){
        viewer.configure(null)
        assert.equal(worker.terminated,undefined,'retirement still owns the pending transfer')
        worker.reply({bytes:structuredClone(chunk.bytes,{transfer:[chunk.bytes]})})
      }else worker.reply(failure==='missing'?{}:{bytes:new ArrayBuffer(IO_BYTES-1)})
      await untilStaged(()=>worker.terminated)
      assert.equal(worker.commands.filter(kind=>kind==='chunk').length,1)
      assert.ok(!worker.commands.includes('finish'))
      assert.equal(events.some(event=>event.kind==='sealed-error'&&event.code==='archive_chunk_receipt_mismatch'),failure!=='retired')
    }finally{
      viewer.configure(null)
      if(worker?.onmessage&&worker.chunk?.bytes.byteLength)worker.reply({bytes:worker.chunk.bytes})
      await untilStaged(()=>!worker||worker.terminated)
    }
  }
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
          fakeWorkerReply(this,message,result)
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
      const sentFirst=events.find(event=>event.kind==='sealed-page');assert.equal(sentFirst.rows.length,64);assert.equal(sentFirst.last,last.last,'new readers start at verified latest')
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

test('sealed event groups expose the complete final occurrence time on every page and search',async()=>{
 const event=(index,timestamp)=>({type:'event_msg',timestamp,payload:{type:'future_event',index,result:'needle '+index}})
 for(const final of ['2026-10-30T09:00:00Z',undefined,'invalid']){
  const values=Array.from({length:140},(_,index)=>event(index,index===139?final:'2026-10-30T12:00:00Z'))
  await indexed(values,async(engine,store)=>{
   const expected=final==='2026-10-30T09:00:00Z'?final:''
   const first=await engine.page(),last=await engine.page({last:true}),search=await engine.page({query:'needle 0'})
   assert.equal(first.rows.length,64);assert.equal(last.rows.length,64)
   for(const page of [first,last,search])assert.ok(page.rows.every(row=>row.kind==='event'&&row.endTime===expected&&row.count==='140'))
   const row=first.rows[0];await engine.disclosure(row.key,true)
   const opened=await engine.page();assert.equal(opened.rows[0].body.format,'json');assert.deepEqual(JSON.parse(opened.rows[0].body.text),values[0].payload)
   assert.equal(store.tables.groups.size,1);assert.equal([...store.tables.groups.values()][0].endTime,expected)
  })
 }
})

test('raw deep and precision event evidence indexes and hydrates without lossy serialization',async()=>{
 const raw=Buffer.from('{"type":"event_msg","payload":{"type":"precision","n":9007199254740993,"overflow":1e400,"negative":-0,"escaped":"\\u0061","padding":"'+'x'.repeat(19000)+'"}}\n'+
  '{"type":"event_msg","payload":'+'['.repeat(10000)+'0'+']'.repeat(10000)+'}\n')
 await indexed(raw,async(engine,store)=>{
  const first=await engine.page(),piece=first.rows[0]
  assert.equal(first.rows.length,2);assert.equal(indexedRecords(store).length,2)
  await engine.disclosure(piece.key,true)
  const opened=await engine.page({bodyKey:piece.key}),part=opened.rows[0].body
  const next=await engine.hydrate(piece.key,1),text=part.text+next.body.text
  assert.equal(part.more,true);assert.equal(next.body.more,false)
  for(const token of ['9007199254740993','1e400','-0','\\u0061'])assert.ok(text.includes(token),token)
  const deep=await engine.hydrate(first.rows[1].key,0)
  assert.equal(deep.body.text.length,16384);assert.equal(deep.body.more,true);assert.equal(deep.body.format,'json')
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
  try{for(const failedCleanup of [false,true,'undefined rejection']) {
    const raw=Buffer.from('0\n'),gzip=gzipSync(raw),store=memoryStore();let release,settled=false
    const cleanupError=failedCleanup===true?Error('actual_source_close_failed'):undefined
    const cleanup=new Promise((resolve,reject)=>{release=()=>failedCleanup?reject(cleanupError):resolve()})
    const engine=createArchiveEngine({openStore:async()=>store,decode:()=>({readable:new ReadableStream({start(controller){controller.enqueue(Buffer.from('{\n'));controller.close()}}),completion:Promise.resolve(),cleanup})})
    await engine.start({name:'owned',stamp:{workspace,session},manifest:manifest(raw,gzip)});await engine.chunk(gzip)
    const finishing=engine.finish().then(()=>assert.fail('invalid JSON published'),error=>{settled=true;return error})
    await new Promise(resolve=>setTimeout(resolve,5));assert.equal(settled,false);assert.equal(store.closed,false)
    release();const failure=await finishing
    if(failedCleanup){assert.equal(failure.message,'archive_cleanup_failed');assert.equal(failure.errors[0].message,'invalid_json_record');assert.equal(failure.errors[1],cleanupError);await assert.rejects(engine.cleanup(),/archive_cleanup_failed/);assert.equal(store.closed,false)}
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
      queueMicrotask(()=>fakeWorkerReply(this,message,{kind:'ack'}))}
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
    postMessage(message){if(message.kind==='interrupt')return;queueMicrotask(()=>fakeWorkerReply(this,message,{kind:message.kind==='retire'?'cleaned':'ack'}))}
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
      reply(message,value){fakeWorkerReply(this,message,value)}
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
      else if(retirement==='error'){worker.reply(worker.pages[3],{kind:'error',code:'page_failed'});await until(()=>events.some(value=>value.kind==='sealed-error'&&value.code==='page_failed'));assert.equal(timers.size,0);assert.equal(worker.retiring,null,'page failure preserves the verified stage until retry or explicit retirement');viewer.configure(null)}
      else if(retirement==='inactive')viewer.configure(null)
      else viewer.retireAuthority()
      await until(()=>worker.retiring);assert.equal(timers.size,0);assert.equal(worker.terminated,undefined);assert.equal(viewer.snapshot().active,true,'progress timer retirement cannot release the held RPC/Worker owner')
      pendingCallback();clock.advance(1000);assert.equal(navigating(),1);assert.equal(workers.length,2)
      if(retirement!=='complete'){page(worker,3);await new Promise(resolve=>setImmediate(resolve));assert.equal(events.filter(value=>value.kind==='sealed-page').length,beforePages)}
      worker.reply(worker.retiring,{kind:'cleaned'});await until(()=>worker.terminated);assert.equal(viewer.snapshot().active,false)
    }finally{viewer.configure(null);for(const worker of workers)if(worker.retiring&&!worker.terminated)worker.reply(worker.retiring,{kind:'cleaned'});await until(()=>workers.every(worker=>worker.terminated));assert.equal(timers.size,0)}
  }
})

for(const input of ['End','Shift','touchstart','summary pointer'])test(`new scoped ${input} input retires a held disclosure transfer without stale publication or repeated reads`,async()=>{
 const events=[],listeners=new Map(),workers=[],calls=[];let viewer,requestId=0,release
 const key='0000000000000000000:0000000',config={active:true,stamp:'disclosure-transfer',load:1,workspace,session,authority:'principal',scope:'same-source',historical:true,apiUrl:''}
 const viewport={id:'session-records',scrollTop:0,scrollHeight:100,clientHeight:100,getAttribute:name=>name==='data-sealed-dom-owner'?config.stamp:name==='data-sealed-dom-load'?'1':null,getBoundingClientRect:()=>({top:0,bottom:100,height:100}),querySelectorAll:()=>[],replaceChildren(){}}
 class Worker{
  reply(message,value){fakeWorkerReply(this,message,value)}
  postMessage(message){calls.push(message);if(message.kind==='interrupt')return
   if(message.kind==='page'&&calls.filter(call=>call.kind==='page').length>3){queueMicrotask(()=>this.reply(message,{kind:'error',error:'controlled_repeated_page_guard'}));return}
   if(message.kind==='disclosure'&&message.key==='group:run'){release=()=>this.reply(message,{kind:'ack'});return}
   queueMicrotask(()=>this.reply(message,message.kind==='page'?{kind:'page',rows:[{key,kind:'tool',group:'run',groupOpen:false,open:false,body:{format:'json',text:'{}',part:0,more:false}}],first:key,last:key,more:false}:{kind:message.kind==='retire'?'cleaned':'ack'}))
  }
  terminate(){this.terminated=true}
 }
 const raw=Buffer.from('0\n'),gzip=gzipSync(raw)
 viewer=createPhaseBridgeFixture({document:{getElementById:()=>viewport,querySelectorAll:()=>[],addEventListener:(name,handler)=>listeners.set(name,handler)},window:{setTimeout,clearTimeout,crypto:globalThis.crypto,addEventListener(){}},workerFactory:()=>{const worker=new Worker();workers.push(worker);return worker},getActivePosition:()=>({key,fragment:key+':content:0',top:0,groupOpen:true,pieceOpen:true}),send:event=>{events.push(event);if(event.kind==='sealed-demand')queueMicrotask(()=>viewer.configure(config,{id:++requestId,sequence:event.sequence}))},fetchArchive:async url=>({isCurrent:()=>true,response:url.endsWith('/archive')?new Response(JSON.stringify(manifest(raw,gzip))):new Response(gzip,{headers:{'Content-Type':'application/gzip','Content-Length':String(gzip.length),'X-HMem-Archive-Generation':generation,'X-HMem-Archive-SHA256':digest(gzip),'X-HMem-Archive-Raw-Bytes':String(raw.length),'X-HMem-Archive-Raw-SHA256':digest(raw)}})})})
 const until=async predicate=>{for(let turn=0;turn<100&&!predicate();turn++)await new Promise(resolve=>setImmediate(resolve));assert.ok(predicate(),'bounded disclosure cancellation')}
 try{
  viewer.configure(config);await until(()=>!!release)
  assert.equal(events.filter(event=>event.kind==='sealed-page').length,0,'held disclosure publishes no historical content')
  if(input==='End'){listeners.get('keydown')({type:'keydown',target:viewport,key:'End',isTrusted:true});viewer.configure({...config,latestRequest:7})}
  else if(input==='Shift')listeners.get('keydown')({type:'keydown',target:viewport,key:'Shift',isTrusted:true})
  else if(input==='touchstart')listeners.get('touchstart')({type:'touchstart',target:viewport,touches:[{clientY:20}],isTrusted:true})
  else {const summary={closest:()=>null};viewport.contains=node=>node===summary;listeners.get('pointerdown')({type:'pointerdown',target:summary,isTrusted:true})}
  release();await until(()=>calls.filter(call=>call.kind==='page').length===2)
  assert.equal(calls.filter(call=>call.kind==='disclosure').length,1,'cancelled snapshot cannot proceed to piece-open')
  assert.deepEqual(calls.filter(call=>call.kind==='page')[1].page,input==='End'?{last:true}:{continuity:'handoff'},'latest wins, or the canceled snapshot recovers with one ordinary range read')
  await until(()=>events.filter(event=>event.kind==='sealed-page').length===1)
  assert.equal(!!events.filter(event=>event.kind==='sealed-page')[0].fromEnd,input==='End')
  await new Promise(resolve=>setImmediate(resolve));assert.equal(calls.filter(call=>call.kind==='page').length,2,'recovery is finite after non-navigation input as well as End')
  assert.equal(events.some(event=>event.kind==='sealed-error'),false)
  assert.equal(calls.filter(call=>call.kind==='body').length,0,'cancelled body position does not begin hydration')
 }finally{release?.();viewer.configure(null);await until(()=>workers.every(worker=>worker.terminated))}
})

test('sealed local intents retain only the latest page or Markdown demand through held operations and retirement',async()=>{
  const events=[],listeners=new Map(),workers=[];let viewer,requestId=0,search=''
  const config={active:true,stamp:'local-intent',load:1,workspace,session,authority:'principal',apiUrl:''}
  const document={getElementById:id=>id==='sealed-search'?{value:search}:{replaceChildren(){}},querySelectorAll:()=>[],addEventListener:(name,handler)=>listeners.set(name,handler)}
  class HeldWorker{
    messages=[];pages=[];bodies=[]
    reply(message,value){fakeWorkerReply(this,message,value)}
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
    pageResult(worker.pages[3],[{key:'new-page'}]);await until(()=>worker.bodies.length===2)
    assert.equal(worker.pages.length,4,'a composed-window body lookup does not replace its conversation with an origin page')
    assert.equal(events.filter(event=>event.kind==='sealed-page').at(-1).rows[0].key,'new-page')
    assert.equal(events.at(-1).kind,'sealed-markdown-start','accepted source publication precedes the queued exact-body sequence')
    assert.equal(worker.bodies[1].key,'source-b','latest Markdown intent keeps its exact source key after the held acquisition')
    worker.reply(worker.bodies[1],{kind:'body',key:'source-b',body:{part:0,text:'complete',format:'markdown',more:false}})
    await until(()=>events.filter(event=>event.kind==='sealed-markdown-chunk').length===2)
    click('last');await until(()=>worker.pages.length===5);click('body','retired-body')
    const published=events.filter(event=>event.kind==='sealed-page').length
    viewer.retireAuthority();await until(()=>worker.terminated)
    pageResult(worker.pages[4]);await new Promise(resolve=>setImmediate(resolve))
    assert.equal(worker.pages.length,5);assert.equal(events.filter(event=>event.kind==='sealed-page').length,published)
    assert.equal(workers.length,2);assert.ok(!events.some(event=>event.kind==='sealed-error'))
  }finally{viewer.configure(null);await until(()=>workers.at(-1).terminated)}
})

test('a fitting raw record and index item cannot produce an empty nonadvancing oversized wire page',async()=>{
  const huge={timestamp:'2026-10-30T00:00:00.'+'0'.repeat(1048379)+'Z',type:'response_item',payload:{type:'message',role:'assistant',content:[{type:'output_text',text:'x'}]}}
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
      const respond=()=>fakeWorkerReply(this,message,{kind:message.kind==='retire'?'cleaned':'ack'})
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
      reply(message,value){fakeWorkerReply(this,message,value)}
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


test('ordinary disclosure hydration waits for the accepted page and cancels an evicted native toggle',async()=>{
 const events=[],listeners=new Map(),workers=[],frames=[];let viewer,requestId=0,currentNode
 const oldKey='0000000000000000050:0000000',newKey='0000000000000000150:0000000',raw=Buffer.from('0\n'),gzip=gzipSync(raw),config={active:true,stamp:'toggle-publication',load:1,workspace,session,authority:'principal',apiUrl:''},attrs={'data-render-part':'0','data-render-request':'0','data-render-more':'false','data-last-prose-more':'false','data-latest-request':'1'}
 const node=key=>({isConnected:true,open:true,getAttribute:name=>name==='data-sealed-key'?key:name==='data-sealed-owner'?config.stamp:null})
 const old=node(oldKey),fresh=node(newKey);currentNode=old
 const viewport={id:'session-records',scrollTop:0,scrollHeight:1000,clientHeight:100,replaceChildren(){},contains:node=>node===currentNode&&node.isConnected,getAttribute:name=>attrs[name]??null,setAttribute:(name,value)=>{attrs[name]=value},querySelectorAll:()=>[],getBoundingClientRect:()=>({top:0,bottom:100,height:100})}
 class Worker{messages=[];pages=[];bodies=[];reply(message,value){fakeWorkerReply(this,message,value)}postMessage(message){this.messages.push(message);if(message.kind==='interrupt')return;if(message.kind==='page'){this.pages.push(message);return}if(message.kind==='body'){this.bodies.push(message);return}queueMicrotask(()=>this.reply(message,{kind:message.kind==='retire'?'cleaned':'ack'}))}terminate(){this.terminated=true}}
 const toggle=target=>listeners.get('toggle')({target}),paint=()=>{for(let turn=0;turn<12&&frames.length;turn++)frames.splice(0).forEach(callback=>callback())},until=async predicate=>{for(let turn=0;turn<100&&!predicate();turn++)await new Promise(resolve=>setImmediate(resolve));assert.ok(predicate(),'bounded disclosure publication')}
 viewer=createPhaseBridgeFixture({document:{getElementById:()=>viewport,querySelectorAll:()=>[],addEventListener:(name,handler)=>listeners.set(name,handler)},window:{setTimeout,clearTimeout,crypto:globalThis.crypto,addEventListener(){},requestAnimationFrame:callback=>frames.push(callback)},workerFactory:()=>{const worker=new Worker();workers.push(worker);return worker},send:event=>{events.push(event);if(event.kind==='sealed-demand')queueMicrotask(()=>viewer.configure(config,{id:++requestId,sequence:event.sequence}));if(event.kind==='sealed-page'&&event.first===newKey)toggle(old);if(event.kind==='sealed-latest-render'){attrs['data-render-request']=String(event.request);queueMicrotask(()=>viewer.configure(config))}if(event.kind==='sealed-body')queueMicrotask(()=>viewer.configure(config,null,{stamp:config.stamp,load:config.load,request:event.request}))},fetchArchive:async url=>({isCurrent:()=>true,response:url.endsWith('/archive')?new Response(JSON.stringify(manifest(raw,gzip))):new Response(gzip,{headers:{'Content-Type':'application/gzip','Content-Length':String(gzip.length),'X-HMem-Archive-Generation':generation,'X-HMem-Archive-SHA256':digest(gzip),'X-HMem-Archive-Raw-Bytes':String(raw.length),'X-HMem-Archive-Raw-SHA256':digest(raw)}})})})
 try{
  viewer.configure(config);await until(()=>workers.at(-1)?.pages.length===1);const worker=workers.at(-1),replyPage=(request,key)=>worker.reply(request,{kind:'page',rows:[{key,kind:'tool',open:true}],first:key,last:key,more:false})
  replyPage(worker.pages[0],oldKey);await until(()=>events.some(event=>event.kind==='sealed-page'));paint()
  config.latestRequest=1;viewer.configure(config);await until(()=>worker.pages.length===2);replyPage(worker.pages[1],newKey);await until(()=>events.filter(event=>event.kind==='sealed-page').length===2);await new Promise(resolve=>setImmediate(resolve))
  assert.equal(worker.bodies.length,0,'the old connected DOM cannot start a body against a newer accepted page')
  old.isConnected=false;currentNode=fresh;paint();await until(()=>events.some(event=>event.kind==='sealed-latest-complete'))
  toggle(old);toggle({isConnected:true,open:true,getAttribute:()=> 'retired-owner'});await new Promise(resolve=>setImmediate(resolve));assert.equal(worker.bodies.length,0,'retired nodes and owners cannot hydrate')
  toggle(fresh);await until(()=>worker.bodies.length===1);assert.equal(worker.bodies[0].key,newKey,'a still-admitted open node hydrates after the matching page and latest publication')
  worker.reply(worker.bodies[0],{kind:'body',key:newKey,body:{part:0,text:'{}',tokens:[],format:'json',more:false}});await until(()=>events.some(event=>event.kind==='sealed-body'));paint();assert.ok(!events.some(event=>event.kind==='sealed-error'))
 }finally{viewer.configure(null);await until(()=>workers.every(worker=>worker.terminated));paint()}
})

test('sealed continuous render admission retains manual intent and exact stamped DOM acknowledgement',async()=>{
 const events=[],workers=[],listeners=new Map(),frames=[];let viewer,requestId=0
 const raw=Buffer.from('0\n'),gzip=gzipSync(raw),config={active:true,stamp:'render-handoff',load:1,workspace,session,authority:'principal',apiUrl:''}
 const attrs={'data-sealed-dom-owner':config.stamp,'data-sealed-dom-load':'1','data-sealed-first':'first','data-sealed-last':'last','data-render-part':'1','data-render-request':'0','data-render-more':'true'}
 let top=900
 const rawGap={dataset:{gapBefore:'0000000000000000100:0000000',gapAfter:'0000000000000000200:0000000'},getBoundingClientRect:()=>({top:120,bottom:220,height:100})}
 const viewport={id:'session-records',get scrollTop(){return top},set scrollTop(value){top=Math.max(0,Math.min(900,value))},scrollHeight:1000,clientHeight:100,getAttribute:name=>attrs[name] ?? null,setAttribute:(name,value)=>{attrs[name]=value},getBoundingClientRect:()=>({top:0,bottom:100,height:100}),replaceChildren(){},querySelectorAll(selector){return selector==='[data-fragment-from]'?[{dataset:{fragmentFrom:String(Number(attrs['data-render-part'])+1),fragmentTo:String(Number(attrs['data-render-part'])+1)},getBoundingClientRect:()=>({top:70,bottom:150,height:80}),closest:()=>null}]:selector==='[data-history-gap="source"]'?[rawGap]:[]}}
 class Worker{postMessage(message){if(message.kind==='interrupt')return;queueMicrotask(()=>fakeWorkerReply(this,message,message.kind==='page'?{kind:'page',rows:[{key:'first'}],first:'first',last:'last',more:false}:{kind:message.kind==='retire'?'cleaned':'ack'}))}terminate(){this.terminated=true}}
 viewer=createPhaseBridgeFixture({document:{getElementById:()=>viewport,querySelectorAll:()=>[],addEventListener:(name,handler)=>listeners.set(name,handler)},window:{setTimeout,clearTimeout,crypto:globalThis.crypto,addEventListener(){},requestAnimationFrame:callback=>frames.push(callback)},workerFactory:()=>{const worker=new Worker();workers.push(worker);return worker},send:event=>{events.push(event);if(event.kind==='sealed-demand')queueMicrotask(()=>viewer.configure(config,{id:++requestId,sequence:event.sequence}))},fetchArchive:async url=>({isCurrent:()=>true,response:url.endsWith('/archive')?new Response(JSON.stringify(manifest(raw,gzip))):new Response(gzip,{headers:{'Content-Type':'application/gzip','Content-Length':String(gzip.length),'X-HMem-Archive-Generation':generation,'X-HMem-Archive-SHA256':digest(gzip),'X-HMem-Archive-Raw-Bytes':String(raw.length),'X-HMem-Archive-Raw-SHA256':digest(raw)}})})})
 const until=async fn=>{for(let i=0;i<100&&!fn();i++)await new Promise(resolve=>setImmediate(resolve));assert.ok(fn(),'bounded render handoff')}
 const paint=()=>{for(let i=0;i<8&&frames.length;i++)frames.splice(0).forEach(callback=>callback())}
 const scroll=()=>listeners.get('scroll')({target:viewport})
 const renderEvents=()=>events.filter(event=>event.kind==='sealed-render')
 try{
  viewer.configure(config);await until(()=>events.some(event=>event.kind==='sealed-page'));paint()
  for(let i=0;i<5;i++)scroll();paint();assert.equal(renderEvents().length,0,'layout alone cannot invent manual history intent')
  listeners.get('wheel')({type:'wheel',target:viewport,deltaY:-100});viewport.scrollTop=700;scroll();paint()
  listeners.get('wheel')({type:'wheel',target:viewport,deltaY:100});viewport.scrollTop=800;scroll();frames.splice(0).forEach(callback=>callback());assert.equal(renderEvents().length,1)
  const first=renderEvents()[0];assert.equal(first.part,2);attrs['data-render-part']='2';attrs['data-render-request']=String(first.request-1);paint()
  assert.equal(events.filter(event=>event.kind==='sealed-page').length,1,'cached presentation wins over the farther raw span in overscan')
  assert.equal(viewport.scrollTop,800,'a stale render stamp cannot reposition the continuous window')
  scroll();assert.equal(renderEvents().length,1,'no second part can run before the first handoff is acknowledged')
  attrs['data-render-request']=String(first.request);viewer.configure(config);paint();assert.equal(viewport.scrollTop,800)
  assert.equal(renderEvents().length,1,'an unchanged owned acknowledgment cannot drive another presentation')
  viewport.scrollTop=820;scroll();frames.splice(0).forEach(callback=>callback());assert.equal(renderEvents().length,2,'continued displacement in the same gesture automatically admits the next continuation')
  const second=renderEvents().at(-1);assert.equal(second.part,3)
  attrs['data-render-part']='3';attrs['data-render-request']=String(second.request);attrs['data-render-more']='false';viewport.querySelectorAll=()=>[];paint();assert.equal(viewport.scrollTop,820)
  assert.deepEqual(renderEvents().map(event=>event.part),[2,3])
 }finally{viewer.configure(null);await until(()=>workers.every(worker=>worker.terminated));paint()}
})

test('sealed latest waits for its final rendered part and retires pending jumps with authority',async()=>{
 const events=[],workers=[],listeners=new Map(),frames=[];let viewer,requestId=0
 const raw=Buffer.from('0\n'),gzip=gzipSync(raw),config={active:true,stamp:'latest-sealed',load:1,workspace,session,authority:'principal',apiUrl:'',latestRequest:7}
 const attrs={'data-sealed-dom-owner':config.stamp,'data-sealed-dom-load':'1','data-sealed-first':'end','data-sealed-last':'end','data-render-more':'false','data-last-prose-more':'false','data-latest-request':'6','data-render-request':'0'}
 const viewport={scrollTop:20,scrollHeight:1000,clientHeight:100,getAttribute:key=>attrs[key] ?? null,setAttribute:(key,value)=>{attrs[key]=value},replaceChildren(){}}
 class Worker{pages=[];postMessage(message){if(message.kind==='interrupt')return;if(message.kind==='page')this.pages.push(message);queueMicrotask(()=>fakeWorkerReply(this,message,message.kind==='page'?{kind:'page',rows:[{key:'end',kind:'text'}],first:'end',last:'end',more:false}:{kind:message.kind==='retire'?'cleaned':'ack'}))}terminate(){this.terminated=true}}
 viewer=createPhaseBridgeFixture({document:{getElementById:()=>viewport,querySelectorAll:()=>[],addEventListener:(name,handler)=>listeners.set(name,handler)},window:{setTimeout,clearTimeout,crypto:globalThis.crypto,addEventListener(){},requestAnimationFrame:callback=>frames.push(callback)},workerFactory:()=>{const worker=new Worker();workers.push(worker);return worker},send:event=>{events.push(event);if(event.kind==='sealed-demand')queueMicrotask(()=>viewer.configure(config,{id:++requestId,sequence:event.sequence}))},fetchArchive:async url=>({isCurrent:()=>true,response:url.endsWith('/archive')?new Response(JSON.stringify(manifest(raw,gzip))):new Response(gzip,{headers:{'Content-Type':'application/gzip','Content-Length':String(gzip.length),'X-HMem-Archive-Generation':generation,'X-HMem-Archive-SHA256':digest(gzip),'X-HMem-Archive-Raw-Bytes':String(raw.length),'X-HMem-Archive-Raw-SHA256':digest(raw)}})})})
 const until=async fn=>{for(let i=0;i<100&&!fn();i++)await new Promise(resolve=>setImmediate(resolve));assert.ok(fn(),'bounded sealed latest')}
 const paint=()=>{for(let turn=0;turn<8&&frames.length;turn++)frames.splice(0).forEach(callback=>callback())}
 try{
  viewer.configure(config);await until(()=>events.some(event=>event.kind==='sealed-latest-render'));const render=events.find(event=>event.kind==='sealed-latest-render')
  assert.deepEqual(workers.at(-1).pages[0].page,{last:true});paint();assert.equal(viewport.scrollTop,20)
  attrs['data-latest-request']='7';attrs['data-render-request']=String(render.request);attrs['data-render-more']='true';viewer.configure(config);paint();assert.equal(viewport.scrollTop,20,'an earlier mounted part is not logical latest')
  attrs['data-render-more']='false';viewer.configure(config);paint();assert.equal(viewport.scrollTop,1000);assert.ok(events.some(event=>event.kind==='sealed-latest-complete'&&event.latestRequest===7))
  viewport.scrollTop=35;viewer.configure({...config,latestRequest:8});await until(()=>events.some(event=>event.kind==='sealed-latest-render'&&event.latestRequest===8));viewer.retireAuthority();paint();assert.equal(viewport.scrollTop,35);assert.ok(!events.some(event=>event.kind==='sealed-latest-complete'&&event.latestRequest===8))
 }finally{viewer.configure(null);await until(()=>workers.every(worker=>worker.terminated));paint()}
})

test('reader retry retains custody through held release, release failure and authority retirement',async()=>{
 for(const scenario of ['retry','release-failure','retire']){
  const events=[],workers=[],listeners=new Map();let viewer,requestId=0
  const raw=Buffer.from('0\n'),gzip=gzipSync(raw),config={active:true,stamp:'reader-retry-'+scenario,load:1,workspace,session,authority:'principal',apiUrl:''}
  class Worker{
   pages=[]
   reply(message,value){fakeWorkerReply(this,message,value)}
   postMessage(message){
    if(message.kind==='interrupt')return
    if(message.kind==='release-reader'){this.release=()=>this.reply(message,scenario==='release-failure'?{kind:'error',code:'archive_cleanup_failed'}:{kind:'detached',stage:{name:this.stage.name,custody:this.stage.custody,manifest:this.stage.manifest}});return}
    if(message.kind==='start'||message.kind==='adopt')this.stage={name:message.name,custody:message.custody,manifest:message.manifest}
    if(message.kind==='page'){this.pages.push(message);queueMicrotask(()=>this.reply(message,this.pages.length===2?{kind:'error',code:'page_failed'}:{kind:'page',rows:[{key:message.page.after?'next':'first'}],first:message.page.after?'next':'first',last:message.page.after?'next':'first',more:true}));return}
    queueMicrotask(()=>this.reply(message,{kind:message.kind==='retire'?'cleaned':'ack'}))
   }
   terminate(){this.terminated=true}
  }
  viewer=createPhaseBridgeFixture({document:{getElementById:()=>null,querySelectorAll:()=>[],addEventListener:(name,handler)=>listeners.set(name,handler)},window:{setTimeout,clearTimeout,crypto:globalThis.crypto,addEventListener(){}},workerFactory:()=>{assert.ok(workers.every(worker=>worker.terminated));const worker=new Worker();workers.push(worker);return worker},send:event=>{events.push(event);if(event.kind==='sealed-demand')queueMicrotask(()=>viewer.configure(config,{id:++requestId,sequence:event.sequence}))},fetchArchive:async url=>({isCurrent:()=>true,response:url.endsWith('/archive')?new Response(JSON.stringify(manifest(raw,gzip))):new Response(gzip,{headers:{'Content-Type':'application/gzip','Content-Length':String(gzip.length),'X-HMem-Archive-Generation':generation,'X-HMem-Archive-SHA256':digest(gzip),'X-HMem-Archive-Raw-Bytes':String(raw.length),'X-HMem-Archive-Raw-SHA256':digest(raw)}})})})
  const until=async fn=>{for(let i=0;i<100&&!fn();i++)await new Promise(resolve=>setImmediate(resolve));assert.ok(fn(),'bounded retry '+scenario)}
  const click=action=>listeners.get('click')({target:{closest:()=>({getAttribute:name=>name==='data-sealed-action'?action:name==='data-sealed-owner'?config.stamp:null})},preventDefault(){}})
  try{
   viewer.configure(config);await until(()=>events.some(event=>event.kind==='sealed-page'));const reader=workers.at(-1)
   click('next');await until(()=>events.some(event=>event.kind==='sealed-error'&&event.code==='page_failed'))
   click('retry-page');await until(()=>reader.release)
   assert.equal(reader.terminated,undefined);assert.equal(workers.length,2);assert.equal(viewer.snapshot().active,true)
   if(scenario==='retire'){viewer.retireAuthority();await new Promise(resolve=>setImmediate(resolve));assert.equal(reader.terminated,undefined);assert.equal(viewer.snapshot().active,true)}
   reader.release()
   if(scenario==='retry'){await until(()=>workers.length===3&&events.filter(event=>event.kind==='sealed-page').length===2);assert.equal(reader.terminated,true);assert.deepEqual(workers[2].pages[0].page,{after:'first'})}
   else {await until(()=>!viewer.snapshot().active);assert.equal(workers.length,2);if(scenario==='release-failure')assert.equal(viewer.snapshot().blocked,true)}
  }finally{viewer.configure(null);await until(()=>workers.every(worker=>worker.terminated))}
 }
})

test('ordinary archive pages and fresh owners cannot inherit hidden search or offset options',async()=>{
 const events=[],listeners=new Map(),workers=[];let viewer,requestId=0
 let config={active:true,stamp:'embedded',load:1,workspace,session,authority:'principal',apiUrl:''}
 const document={getElementById:id=>id==='sealed-search'?{value:'needle'}:id==='sealed-position'?{value:'4096'}:{replaceChildren(){}},querySelectorAll:()=>[],addEventListener:(name,handler)=>listeners.set(name,handler)}
 class Worker{
  pages=[]
  reply(message,value){fakeWorkerReply(this,message,value)}
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
  assert.equal(worker.terminated,true);assert.deepEqual(workers.at(-1).pages[0].page,{last:true})
  click('search','embedded');assert.equal(workers.at(-1).pages.length,1,'retired embedded action cannot filter workspace owner')
  respond(workers.at(-1),0);await until(()=>events.some(e=>e.kind==='sealed-page'&&e.stamp==='workspace'))
 }finally{viewer.configure(null);await until(()=>workers.every(w=>w.terminated))}
})
