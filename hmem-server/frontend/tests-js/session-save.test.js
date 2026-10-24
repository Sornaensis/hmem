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
