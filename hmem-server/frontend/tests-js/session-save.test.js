import assert from 'node:assert/strict'
import test from 'node:test'
import { createHash } from 'node:crypto'
import { createEvidenceWriter, installSessionBridge } from '../src/session-save.js'
const digest = bytes => createHash('sha256').update(bytes).digest('hex')
const session = bytes => ({id:'s',workspace:'w',state:'terminated',finalBytes:bytes.length,finalSha256:digest(bytes),revision:7})
const chunk = (row,bytes,offset=0) => ({session:row,offset,nextOffset:offset+bytes.length,dataBase64:bytes.toString('base64')})

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
function bridgeFixture(picker) {
  let incoming,click
  const events=[],app={ports:{sessionBridgeCommand:{subscribe(fn){incoming=fn}},sessionBridgeEvent:{send(value){events.push(value)}}}}
  installSessionBridge(app,{document:{addEventListener(name,fn){if(name==='click')click=fn}},window:{showSaveFilePicker:picker},now:()=>0})
  return {events,configure(stamp,nextOperation=1){incoming({stamp,generation:0,reset:null,chunk:null,saveChunk:null,saveOperation:null,nextOperation,heartbeats:[]})},click(stamp,operation=1){
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
  const viewport={scrollTop:37,scrollHeight:900}
  globalThis.requestAnimationFrame=callback=>frames.push(callback)
  try{
    installSessionBridge({ports:{sessionBridgeCommand:{subscribe(fn){incoming=fn}},sessionBridgeEvent:{send(value){events.push(value)}}}},
      {document:{addEventListener(){},getElementById(){return viewport}},window:{},now:()=>31000})
    const command={stamp:'authority-before-snapshot',generation:3,reset:0,chunk:{offset:0,nextOffset:5,dataBase64:Buffer.from('true\n').toString('base64'),final:true},follow:true,heartbeats:[{id:'s',lastSeenAt:'1970-01-01T00:00:00Z'}]}
    incoming(command)
    incoming({...command,stamp:'same-authority-new-snapshot',generation:4,reset:0,chunk:null,follow:false})
    await flush();frames.forEach(callback=>callback())
    assert.equal(viewport.scrollTop,37)
    const late=events.find(event=>event.kind==='viewer')
    assert.equal(late.stamp,command.stamp);assert.equal(late.generation,3);assert.equal(late.rows.length,1)
    assert.equal(events.find(event=>event.kind==='heartbeat-status').statuses.s,'Stale (30s)')
  }finally{globalThis.requestAnimationFrame=previous}
})


function archiveBridge(storage){
 let incoming;const listeners=[],events=[]
 const app={ports:{sessionBridgeCommand:{subscribe(fn){incoming=fn}},sessionBridgeEvent:{send(value){events.push(value)}}}}
 installSessionBridge(app,{document:{addEventListener(name,fn){if(name==='click')listeners.push(fn)},createElement(){return{click(){}}}},window:{navigator:{storage:{getDirectory:storage}},crypto:{randomUUID:()=> 'fixture'},setTimeout(){},URL:{createObjectURL(){return 'blob:fixture'},revokeObjectURL(){}},addEventListener(){}},now:()=>0})
 const command=(extra={})=>incoming({stamp:'scope',generation:0,reset:null,chunk:null,saveChunk:null,saveOperation:null,nextOperation:1,heartbeats:[],...extra})
 command()
 return{events,command,clickDownload(operation=1){const button={disabled:false,dataset:{sessionDownload:JSON.stringify({stamp:'scope',operation,session:session(Buffer.from('true\n'))})}};for(const fn of listeners)fn({target:{closest(selector){return selector==='[data-session-download]'?button:null}},preventDefault(){}})},click(operation=1){const config={stamp:'scope',operation,task:'task',title:'Task'},button={disabled:false,dataset:{sessionArchive:JSON.stringify(config)}};for(const fn of listeners)fn({target:{closest(selector){return selector==='[data-session-archive]'?button:null}},preventDefault(){}})}}
}
test('partial archive acquisition treats absent files as cleaned and can fall back safely',async()=>{
 let creates=0,aborts=0,removed=[]
 const root={async *keys(){},async getFileHandle(){if(++creates===2)throw Error('quota');return{async createWritable(){return{async abort(){aborts++}}}}},async removeEntry(name){removed.push(name);if(name.endsWith('.index')){const error=Error('absent');error.name='NotFoundError';throw error}}}
 const bridge=archiveBridge(async()=>({getDirectoryHandle:async()=>root}));bridge.click();await flush();await flush();await flush()
 assert.equal(aborts,1);assert.equal(removed.length,2);assert.ok(bridge.events.some(event=>event.kind==='archive-ready'));assert.equal(bridge.events.some(event=>event.code==='writer_cleanup_failed'),false)
 bridge.command({nextOperation:2});await flush()
})
test('unconfirmed archive acquisition cleanup keeps its reservation and reports reload-required failure',async()=>{
 let creates=0
 const root={async *keys(){},async getFileHandle(){if(++creates===2)throw Error('quota');return{async createWritable(){return{async abort(){}}}}},async removeEntry(){throw Error('cleanup denied')}}
 const bridge=archiveBridge(async()=>({getDirectoryHandle:async()=>root}));bridge.click();await flush();await flush();await flush()
 assert.ok(bridge.events.some(event=>event.kind==='archive-error'&&event.code==='writer_cleanup_failed'))
 bridge.command({nextOperation:2});await flush();bridge.click(2);await flush();assert.equal(creates,2,'failed cleanup retains the only writer reservation')
})


test('archive memory fallback counts ZIP and index overhead inside its aggregate 32 MiB bound',async()=>{
 const bridge=archiveBridge(async()=>{throw Error('private storage unavailable')});bridge.click();await flush();await flush()
 const bytes=Buffer.alloc(65536,120),hash=createHash('sha256');for(let i=0;i<512;i++)hash.update(bytes)
 const row={id:'20000000-0000-4000-8000-000000000001',workspace:'w',state:'terminated',revision:1,finalBytes:33554432,finalSha256:hash.digest('hex')}
 bridge.command({nextOperation:2,saveOperation:1,archiveEntry:{session:row,name:'Finite fallback'}});await flush();await flush()
 let writes=0
 for(let offset=0;offset<row.finalBytes;offset+=bytes.length){bridge.command({nextOperation:2,saveOperation:1,saveChunk:{session:row,offset,nextOffset:offset+bytes.length,dataBase64:bytes.toString('base64')}});writes++;await flush();if(bridge.events.some(event=>event.kind==='save-error'))break}
 assert.equal(writes,512);assert.ok(bridge.events.some(event=>event.kind==='save-error'));assert.equal(bridge.events.some(event=>event.kind==='archive-complete'),false)
})


test('another owner never age-cleans a live archive or index, including individual download acquisition',async()=>{
 const files=new Map(),now=Date.now;let instant=now()
 const root={async *keys(){yield*files.keys()},async getFileHandle(name){if(!files.has(name))files.set(name,[]);return{async createWritable(){return{async write(bytes){if(!files.has(name))throw Error('foreign staging deleted');files.get(name).push(new Uint8Array(bytes))},async close(){},async abort(){files.set(name,[])}}},async getFile(){if(!files.has(name))throw Error('foreign staging deleted');return new Blob(files.get(name))}}},async removeEntry(name){if(!files.delete(name)){const error=Error('absent');error.name='NotFoundError';throw error}}}
 const storage=async()=>({getDirectoryHandle:async()=>root}),first=archiveBridge(storage),second=archiveBridge(storage),individual=archiveBridge(storage)
 try{
  Date.now=()=>instant;first.click();await flush();await flush();const firstFiles=[...files.keys()];assert.equal(firstFiles.length,2)
  const bytes=Buffer.from('true\nfalse\n'),row={...session(bytes),id:'20000000-0000-4000-8000-000000000001'}
  first.command({nextOperation:2,saveOperation:1,archiveEntry:{session:row,name:'Long-lived archive'}});await flush();await flush()
  first.command({nextOperation:2,saveOperation:1,saveChunk:chunk(row,bytes.subarray(0,5))});await flush()
  instant+=120000;second.click();await flush();await flush();individual.clickDownload();await flush();await flush()
  for(const name of firstFiles)assert.ok(files.has(name),'another owner retained '+name)
  first.command({nextOperation:2,saveOperation:1,saveChunk:chunk(row,bytes.subarray(5),5)});await flush();await flush()
  first.command({nextOperation:2,saveOperation:1,archiveFinish:true});for(let i=0;i<10;i++)await flush()
  assert.ok(first.events.some(event=>event.kind==='archive-complete'),'original owner continues and finishes after two other acquisitions')
  assert.ok(files.has(firstFiles.find(name=>name.endsWith('.zip'))));assert.equal(files.has(firstFiles.find(name=>name.endsWith('.index'))),false)
  assert.equal(first.events.some(event=>event.kind==='archive-error'),false)
  second.command({nextOperation:2});individual.command({nextOperation:2});await flush();await flush()
 }finally{Date.now=now}
})


test('fallback final assembly composes immutable Blob parts rather than copying retained Uint8Arrays',async()=>{
 const NativeBlob=globalThis.Blob;let finalParts
 // This checks the copy-budget API invariant, not garbage collection timing.
 globalThis.Blob=class extends NativeBlob{constructor(parts,options){if(options?.type==='application/zip')finalParts=parts;super(parts,options)}}
 try{const bridge=archiveBridge(async()=>{throw Error('storage unavailable')});bridge.click();await flush();await flush();bridge.command({nextOperation:2,saveOperation:1,archiveFinish:true});for(let i=0;i<10;i++)await flush()
  assert.ok(finalParts.length>0);assert.ok(finalParts.every(part=>part instanceof NativeBlob));assert.ok(bridge.events.some(event=>event.kind==='archive-complete'))
 }finally{globalThis.Blob=NativeBlob}
})

test('index cleanup failure still aborts archive output and retains the blocked reservation',async()=>{
 const files=new Map();let outputAborts=0,acquisitions=0
 const root={async getFileHandle(name){acquisitions++;files.set(name,[]);return{async createWritable(){return{async write(bytes){files.get(name).push(bytes)},async close(){},async abort(){if(name.endsWith('.zip'))outputAborts++}}},async getFile(){return new Blob(files.get(name))}}},async removeEntry(name){if(name.endsWith('.index'))throw Error('index cleanup denied');files.delete(name)}}
 const bridge=archiveBridge(async()=>({getDirectoryHandle:async()=>root}));bridge.click();await flush();await flush();bridge.command({nextOperation:2,saveOperation:1,archiveFinish:true});for(let i=0;i<20;i++)await flush()
 assert.equal(outputAborts,1);assert.equal(files.size,1);assert.ok(bridge.events.some(event=>event.kind==='archive-error'&&event.code==='writer_cleanup_failed'));assert.equal(bridge.events.some(event=>event.kind==='archive-complete'),false)
 bridge.command({nextOperation:2});bridge.click(2);await flush();assert.equal(acquisitions,2)
})
