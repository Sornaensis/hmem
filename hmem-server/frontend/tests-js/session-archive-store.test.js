import test from 'node:test'
import assert from 'node:assert/strict'
import {decimal,fileOffset,positionKey,pieceKey,serializedBytes,openArchiveStore,createRawReadCache,IO_BYTES,BATCH_ROWS,BATCH_BYTES} from '../src/session-archive-store.js'
test('archive count and ordered cursor keys retain exact signed 64-bit values',()=>{
  assert.equal(decimal('9223372036854775807'),9223372036854775807n)
  assert.ok(positionKey('10')>positionKey('2'))
  assert.ok(pieceKey('9007199254740993',1)>pieceKey('9007199254740992',99999))
  for(const value of ['01','-1','1.0','9223372036854775808',1])assert.throws(()=>decimal(value))
  assert.throws(()=>fileOffset(9007199254740993n),/unsupported_file_offset/)
  assert.equal(fileOffset(1073741824n),1073741824)
})
test('presentation budgets count actual serialized UTF8 including escaping',()=>{
  assert.equal(serializedBytes({text:'λ\n'}),Buffer.byteLength(JSON.stringify({text:'λ\n'})))
})

function controlledStore() {
  const state={transactions:[],writes:[],values:new Map(),files:new Map(),reads:[],shortRead:false,closedHandles:0,dbClosed:false,removed:false,indexCalls:0,opens:0,upgrades:0,created:false}
  const file=name=>({async getFile(){const bytes=state.files.get(name)||new Uint8Array();return {size:bytes.length,text:async()=>new TextDecoder().decode(bytes),stream:()=>new Blob([bytes]).stream()}},createWritable:async()=>({async write(value){state.files.set(name,new TextEncoder().encode(value))},async close(){},async abort(){}}),createSyncAccessHandle:async()=>({
    write(bytes,{at}){const previous=state.files.get(name)||new Uint8Array(),next=new Uint8Array(Math.max(previous.length,at+bytes.length));next.set(previous);next.set(bytes,at);state.files.set(name,next);return bytes.length},
    read(bytes,{at}){state.reads.push({name,at,length:bytes.length});const source=state.files.get(name)||new Uint8Array(),count=Math.max(0,Math.min(bytes.length,source.length-at)-(state.shortRead?1:0));bytes.set(source.subarray(at,at+count));return count},
    flush(){},close(){state.closedHandles++}})})
  let exists=false
  const directory={getFileHandle:async(name,options)=>{state.fileRequests ||= [];state.fileRequests.push({name,create:!!options?.create});if(!state.files.has(name)&&!options?.create)throw Object.assign(Error('missing file'),{name:'NotFoundError'});return file(name)}}
  const root={async *entries(){},async getDirectoryHandle(name,options){if(!exists&&!options?.create)throw Object.assign(Error('missing'),{name:'NotFoundError'});exists=true;return directory},async removeEntry(){state.removed=true;exists=false}}
  const db={version:1,objectStoreNames:['disclosures','groups','meta','pieces','records'],createObjectStore:()=>({createIndex(){state.indexCalls++;throw Error('secondary_index_forbidden')}}),close(){state.dbClosed=true},transaction(tables,mode){
    const tx={writes:[],requests:[],mode,done:false,objectStore:table=>({keyPath:state.wrongKeyPath?'wrong':'key',autoIncrement:false,indexNames:[],put:value=>tx.writes.push({table,value:structuredClone(value)}),get:key=>{const operation={table,key};tx.requests.push(operation);return operation},index(){throw Error('secondary_index_access_forbidden')},openCursor:(range,direction)=>{const entries=[...state.values].filter(([key])=>key.startsWith(table+':')).map(([,value])=>value).filter(value=>(range?.lower===undefined||value.key>range.lower)&&(range?.upper===undefined||value.key<range.upper)).sort((a,b)=>a.key.localeCompare(b.key));if(direction==='prev')entries.reverse();let at=0;const operation={};const step=()=>{let continued=false;const value=entries[at++];operation.result=value?{value,continue(){continued=true;queueMicrotask(step)}}:null;operation.onsuccess();if(!continued)queueMicrotask(()=>state.complete(tx))};queueMicrotask(step);return operation}})}
    state.transactions.push(tx);return tx
  }}
  const indexedDB={open(){state.opens++;const operation={result:db,transaction:{abort(){operation.error=Error('upgrade_aborted')}}};state.pendingOpen=operation;queueMicrotask(()=>{try{if(!state.created){state.upgrades++;operation.onupgradeneeded();if(operation.error)throw operation.error;state.created=true}if(state.holdOpen){state.releaseOpen=()=>operation.onsuccess();return}operation.onsuccess()}catch(error){operation.error=error;operation.onerror()}});return operation},deleteDatabase(){const operation={};queueMicrotask(()=>operation.onsuccess());return operation}}
  const storage={getDirectory:async()=>({getDirectoryHandle:async()=>root})}
  const locks={request:(_,options,callback)=>Promise.resolve().then(()=>callback({}))}
  state.complete=tx=>{if(tx.done)return;tx.done=true;state.writes.push(...tx.writes);for(const row of tx.writes)state.values.set(row.table+':'+row.value.key,row.value);tx.oncomplete({target:tx})}
  state.abort=(tx,error)=>{if(tx.done)return;tx.done=true;tx.error=error;tx.onabort({target:tx})}
  state.respond=(tx,error)=>{for(const operation of tx.requests){if(operation.responded)continue;operation.responded=true;if(error){operation.error=error;operation.onerror({target:operation})}else{operation.result=state.values.get(operation.table+':'+operation.key);operation.onsuccess({target:operation})}}}
  state.db=db;state.dropDirectory=()=>{exists=false};state.adapters={storage,indexedDB,locks};state.open=(options={})=>openArchiveStore('11111111-1111-4111-8111-111111111111',{storage,indexedDB,locks,...options})
  return state
}

test('real store enqueue is synchronous until an actual row or byte batch flush completes',async()=>{
  for(const byteBoundary of [false,true]){
    const state=controlledStore(),store=await state.open()
    try{
      const count=byteBoundary?1:BATCH_ROWS
      for(let index=0;index<count;index++)assert.equal(store.put('records',{key:String(index),...(byteBoundary?{padding:'x'.repeat(600000)}:{})}),undefined)
      assert.equal(state.transactions.length,0)
      let settled=false
      const held=store.put('records',{key:'next',...(byteBoundary?{padding:'x'.repeat(600000)}:{})})
      assert.equal(typeof held.then,'function');held.then(()=>{settled=true})
      await new Promise(resolve=>setImmediate(resolve));assert.equal(settled,false)
      const first=state.transactions[0];assert.equal(first.writes.length,count);assert.ok(first.writes.reduce((size,row)=>size+serializedBytes(row.value),0)<=BATCH_BYTES)
      assert.ok(!first.writes.some(row=>row.value.key==='next'));state.complete(first);await held
      const terminal=store.flush(),last=state.transactions.at(-1);assert.equal(last.writes.length,1);assert.equal(last.writes[0].value.key,'next')
      let published=false;terminal.then(()=>{published=true});await new Promise(resolve=>setImmediate(resolve));assert.equal(published,false)
      state.complete(last);await terminal;assert.equal(state.writes.length,count+1)
      assert.throws(()=>store.put('records',{key:'large',padding:'x'.repeat(BATCH_BYTES)}),/index_entry_limit/)
    }finally{for(const tx of state.transactions)state.complete(tx);await store.closeAndDelete()}
    assert.equal(state.closedHandles,2);assert.equal(state.dbClosed,true);assert.equal(state.removed,true)
    assert.throws(()=>store.put('records',{key:'retired'}),/retired_store/)
  }
})

test('real store pending flush retains terminal ownership through retirement or transaction failure',async()=>{
  for(const abort of [false,true]){
    const state=controlledStore(),store=await state.open()
    for(let index=0;index<BATCH_ROWS;index++)store.put('records',{key:String(index)})
    const held=store.put('records',{key:'after-flush'}),tx=state.transactions[0],quota=Object.assign(Error('quota'),{name:'QuotaExceededError'})
    const rejected=assert.rejects(held,error=>abort?error===quota:error.message==='retired_store')
    let cleanup
    if(abort){state.abort(tx,quota);await rejected;cleanup=store.closeAndDelete()}
    else{
      cleanup=store.closeAndDelete();await new Promise(resolve=>setImmediate(resolve))
      assert.equal(state.closedHandles,0);assert.equal(state.dbClosed,false);assert.equal(state.removed,false)
      state.complete(tx);await rejected
    }
    await cleanup
    assert.ok(!state.writes.some(row=>row.value.key==='after-flush'));assert.equal(state.closedHandles,2);assert.equal(state.dbClosed,true);assert.equal(state.removed,true)
  }
})

test('page metadata uses one bounded readonly transaction with aligned scalar defaults and shared groups',async()=>{
  const state=controlledStore(),store=await state.open(),pieces=[{key:'a',group:'g'},{key:'b',group:'g'},{key:'c',group:'missing'},{key:'d',group:''}]
  state.values.set('disclosures:a',{open:true});state.values.set('disclosures:group:g',{open:true});state.values.set('groups:g',{count:'99'})
  try{
    let settled=false
    const reading=store.readPageMetadata(pieces);reading.then(()=>{settled=true})
    await new Promise(resolve=>setImmediate(resolve))
    assert.equal(state.transactions.length,1);const tx=state.transactions[0]
    assert.equal(tx.mode,'readonly');assert.equal(tx.requests.length,8);assert.equal(tx.requests.filter(value=>value.table==='groups'&&value.key==='g').length,1)
    state.respond(tx);await new Promise(resolve=>setImmediate(resolve));assert.equal(settled,false,'request success is not terminal acknowledgement')
    await assert.rejects(store.readPageMetadata(pieces),/archive_page_read_busy/)
    state.complete(tx)
    assert.deepEqual(await reading,[{open:true,groupOpen:true,count:'99'},{open:false,groupOpen:true,count:'99'},{open:false,groupOpen:false,count:'0'},{open:false,groupOpen:false,count:'0'}])
    const before=state.transactions.length
    await assert.rejects(store.readPageMetadata(Array.from({length:65},(_,index)=>({key:String(index),group:''}))),/page_limit/)
    assert.equal(state.transactions.length,before)
    const maximum=store.readPageMetadata(Array.from({length:64},(_,index)=>({key:'p'+index,group:'g'+index})))
    await new Promise(resolve=>setImmediate(resolve));const largest=state.transactions.at(-1);assert.equal(largest.requests.length,192)
    state.respond(largest);state.complete(largest);assert.equal((await maximum).length,64)
  }finally{for(const tx of state.transactions){state.respond(tx);state.complete(tx)}await store.closeAndDelete()}
})

test('metadata terminal ownership blocks cleanup after successful requests or failed requests until actual complete or abort',async()=>{
  for(const failure of ['none','same-abort','different-abort']){
    const state=controlledStore(),store=await state.open(),primary=Error('request_failed'),terminalError=failure==='different-abort'?Error('terminal_abort'):primary
    let settled=false
    const reading=store.readPageMetadata([{key:'a',group:''},{key:'b',group:''}]);reading.then(()=>{settled=true},()=>{settled=true})
    await new Promise(resolve=>setImmediate(resolve));const tx=state.transactions[0]
    state.respond(tx,failure==='none'?undefined:primary);tx.onerror?.({target:tx})
    await new Promise(resolve=>setImmediate(resolve));assert.equal(settled,false)
    let closed=false;const cleanup=store.closeAndDelete().then(()=>{closed=true})
    await new Promise(resolve=>setImmediate(resolve));assert.equal(closed,false);assert.equal(state.closedHandles,0);assert.equal(state.dbClosed,false);assert.equal(state.removed,false)
    if(failure==='none')state.complete(tx);else state.abort(tx,terminalError)
    if(failure==='none')await assert.rejects(reading,/retired_store/)
    else await assert.rejects(reading,error=>failure==='same-abort'?error===primary:error instanceof AggregateError&&error.errors[0]===primary&&error.errors[1]===terminalError)
    await cleanup;assert.equal(state.closedHandles,2);assert.equal(state.dbClosed,true);assert.equal(state.removed,true)
  }
})

test('fresh schema and primary forward backward bounded scans never create or access a secondary group index',async()=>{
  const previous=globalThis.IDBKeyRange;globalThis.IDBKeyRange={lowerBound:lower=>({lower}),upperBound:upper=>({upper})}
  const state=controlledStore(),store=await state.open()
  try{
    assert.equal(state.indexCalls,0)
    for(let index=0;index<150;index++){const value={key:String(index).padStart(3,'0'),group:'shared'};state.values.set('pieces:'+value.key,value)}
    const first=await store.scan('pieces'),next=await store.scan('pieces',{after:first.at(-1).key})
    assert.equal(first.length,64);assert.equal(next.length,64);assert.equal(first[0].key,'000');assert.equal(next[0].key,'064')
    assert.deepEqual((await store.scan('pieces',{before:next[0].key})).map(value=>value.key),first.map(value=>value.key))
    const last=await store.scan('pieces',{before:'\uffff'});assert.equal(last[0].key,'086');assert.equal(last.at(-1).key,'149')
    assert.deepEqual((await store.scan('pieces',{before:'008',limit:3})).map(value=>value.key),['005','006','007'])
    assert.equal(state.indexCalls,0);assert.ok(state.transactions.every(tx=>tx.done))
  }finally{for(const tx of state.transactions){state.respond(tx);state.complete(tx)}await store.closeAndDelete();globalThis.IDBKeyRange=previous}
  assert.equal(state.dbClosed,true);assert.equal(state.removed,true)
})


test('two verified raw blocks coalesce repeated first and last records with fresh mutation-safe copies',()=>{
  const source=Uint8Array.from({length:IO_BYTES*4},(_,index)=>index%251),reads=[]
  const cache=createRawReadCache((bytes,{at})=>{reads.push({at,length:bytes.length});bytes.set(source.subarray(at,at+bytes.length));return bytes.length})
  cache.read('0','1024');cache.read('0','1024');assert.equal(reads.length,2,'unpublished bytes never populate the cache')
  cache.publish(String(source.length));reads.length=0
  for(let round=0;round<20;round++)for(const start of [round%64*1024,source.length-IO_BYTES+(round%64)*1024]){
    const result=cache.read(String(start),String(start+1024));assert.deepEqual(result,source.subarray(start,start+1024));result.fill(0)
  }
  assert.deepEqual(reads,[{at:0,length:IO_BYTES},{at:IO_BYTES*3,length:IO_BYTES}])
  assert.deepEqual(cache.snapshot(),{blocks:2,bytes:IO_BYTES*2,capacityBytes:IO_BYTES*2,verified:true,retired:false})
  assert.deepEqual(cache.read('1024','2048'),source.subarray(1024,2048))
  const first=cache.read('0','1024'),again=cache.read('0','1024');assert.notEqual(first.buffer,again.buffer)
  cache.invalidate();assert.equal(cache.snapshot().bytes,0);assert.equal(cache.snapshot().verified,false)
  assert.throws(()=>cache.read('0','1024'),/archive_unverified/);cache.publish(String(source.length));cache.read('0','1024');assert.equal(reads.at(-1).length,IO_BYTES)
  cache.retire();assert.equal(cache.snapshot().bytes,0);assert.throws(()=>cache.read('0','1'),/retired_store/);assert.throws(()=>cache.publish(String(source.length)),/retired_store/)
})

test('verified raw blocks retain exact cross-block UTF8 CRLF EOF and BigInt offsets with bounded direct records',()=>{
  const source=Uint8Array.from({length:IO_BYTES*3+17},(_,index)=>index%251),reads=[];let short=false
  source.set(new TextEncoder().encode('λ😀\r\n'),IO_BYTES-3)
  const cache=createRawReadCache((bytes,{at})=>{reads.push({at,length:bytes.length});const count=Math.min(bytes.length,source.length-at)-(short?1:0);bytes.set(source.subarray(at,at+count));return count})
  cache.publish(String(source.length))
  assert.deepEqual(cache.read(String(IO_BYTES-4),String(IO_BYTES+12)),source.subarray(IO_BYTES-4,IO_BYTES+12))
  assert.deepEqual(cache.read(String(source.length-17),String(source.length)),source.subarray(source.length-17))
  const before=reads.length;assert.equal(cache.read(String(source.length),String(source.length)).length,0);assert.equal(reads.length,before)
  assert.ok(cache.snapshot().bytes<=131072)
  assert.throws(()=>cache.read(String(source.length-1),String(source.length+1)),/file_short_read/);assert.equal(cache.snapshot().bytes,0)
  cache.publish(String(source.length));assert.deepEqual(cache.read('0',String(IO_BYTES+1)),source.subarray(0,IO_BYTES+1));assert.equal(reads.at(-1).length,IO_BYTES+1);assert.equal(cache.snapshot().bytes,0,'large records retain the existing direct path')
  short=true;assert.throws(()=>cache.read('0','1'),/file_short_read/);assert.equal(cache.snapshot().bytes,0);assert.equal(cache.snapshot().verified,true)
  short=false;const afterShort=reads.length;assert.throws(()=>cache.read(String(source.length),String(source.length+1)),/file_short_read/);assert.equal(reads.length,afterShort,'short-read failure retains the verified EOF bound before any later native access')
  const sparseReads=[],sparse=createRawReadCache((bytes,{at})=>{sparseReads.push(at);const base=Number(BigInt(at)%251n);for(let index=0;index<bytes.length;index++)bytes[index]=(base+index)%251;return bytes.length})
  sparse.publish('9223372036854775807')
  for(const start of [4294967295n,9007199254675468n]){
    const bytes=sparse.read(start.toString(),(start+3n).toString());assert.deepEqual([...bytes],[0n,1n,2n].map(index=>Number((start+index)%251n)))
  }
  assert.ok(sparseReads.includes(4294901760));assert.ok(sparseReads.includes(4294967296));assert.ok(sparseReads.includes(9007199254675456))
  const beforeUnsafe=sparseReads.length
  assert.throws(()=>sparse.read('9007199254740991','9007199254740992'),/unsupported_file_offset/)
  assert.throws(()=>sparse.read('9007199254740993','9007199254740994'),/unsupported_file_offset/);assert.equal(sparse.snapshot().bytes,0);assert.equal(sparseReads.length,beforeUnsafe)
  assert.equal(sparse.read('0','1048578').length,1048578);assert.throws(()=>sparse.read('0','1048579'),/record_limit/)
})

test('real store cache waits for publication terminal and invalidates writes retirement and close',async()=>{
  const state=controlledStore(),store=await state.open(),source=Uint8Array.from({length:IO_BYTES*2},(_,index)=>index%251)
  try{
    store.writeRaw(0n,source.subarray(0,IO_BYTES));store.writeRaw(BigInt(IO_BYTES),source.subarray(IO_BYTES))
    let published=false;const publishing=store.publish({raw_bytes:String(source.length)});publishing.then(()=>{published=true})
    await new Promise(resolve=>setImmediate(resolve));const tx=state.transactions.at(-1)
    store.readRaw('0','1024');store.readRaw('0','1024');assert.equal(state.reads.length,2);assert.equal(published,false)
    state.complete(tx);await publishing;state.reads.length=0
    for(let index=0;index<20;index++){store.readRaw('0','1024');store.readRaw(String(source.length-1024),String(source.length))}
    assert.deepEqual(state.reads.map(value=>value.length),[IO_BYTES,IO_BYTES])
    store.writeRaw(0n,new Uint8Array([99]));assert.throws(()=>store.readRaw('0','1'),/archive_unverified/)
    const republished=store.publish({raw_bytes:String(source.length)});await new Promise(resolve=>setImmediate(resolve));state.complete(state.transactions.at(-1));await republished;assert.equal(store.readRaw('0','1')[0],99)
    store.retireReads();assert.throws(()=>store.readRaw('0','1'),/retired_store/)
    const late=store.publish({raw_bytes:String(source.length)}),rejected=assert.rejects(late,/retired_store/)
    await new Promise(resolve=>setImmediate(resolve));state.complete(state.transactions.at(-1));await rejected
  }finally{for(const tx of state.transactions)state.complete(tx);await store.closeAndDelete()}
  assert.equal(state.closedHandles,2);assert.equal(state.dbClosed,true);assert.equal(state.removed,true);assert.throws(()=>store.readRaw('0','1'),/retired_store/)
})


test('three or more raw block misses reuse exactly two backing buffers during fill eviction EOF and errors',()=>{
  const source=Uint8Array.from({length:IO_BYTES*5+17},(_,index)=>index%251),identities=new Set(),capacity=[],lengths=[];let short=false,cache
  cache=createRawReadCache((bytes,{at})=>{
    identities.add(bytes.buffer);capacity.push(cache.snapshot().capacityBytes);lengths.push(bytes.byteLength)
    assert.ok(identities.size<=2,'a miss cannot allocate a third cached backing before eviction')
    assert.ok(cache.snapshot().capacityBytes<=131072)
    const count=Math.min(bytes.length,source.length-at)-(short?1:0);bytes.set(source.subarray(at,at+count));return count
  })
  cache.publish(String(source.length))
  for(const block of [0,1,2,3,4,5,4,3,2,1,0,2,4,1]){
    const start=block*IO_BYTES,end=Math.min(start+31,source.length)
    assert.deepEqual(cache.read(String(start),String(end)),source.subarray(start,end))
  }
  assert.deepEqual(cache.read(String(IO_BYTES-3),String(IO_BYTES+5)),source.subarray(IO_BYTES-3,IO_BYTES+5))
  assert.equal(identities.size,2);assert.ok(capacity.every(bytes=>bytes<=131072));assert.ok(lengths.includes(17))
  short=true;assert.throws(()=>cache.read(String(IO_BYTES*3),String(IO_BYTES*3+1)),/file_short_read/)
  assert.equal(cache.snapshot().blocks,0);assert.equal(cache.snapshot().capacityBytes,131072)
  short=false;cache.read('0','1');cache.read(String(IO_BYTES*2),String(IO_BYTES*2+1));assert.equal(identities.size,2)
  cache.invalidate();assert.throws(()=>cache.read('0','1'),/archive_unverified/);cache.publish(String(source.length));cache.read(String(IO_BYTES*4),String(IO_BYTES*4+1));assert.equal(identities.size,2)
  cache.retire();assert.equal(cache.snapshot().capacityBytes,0)
})

const borrowedCustody='22222222-2222-4222-8222-222222222222'
async function verifiedBorrowedStore(state){
  const store=await state.open({custody:borrowedCustody}),manifest={generation_id:'33333333-3333-4333-8333-333333333333',codec:'gzip-v1',raw_bytes:'3',raw_records:'1',raw_sha256:'a'.repeat(64),compressed_bytes:'2',compressed_sha256:'b'.repeat(64)}
  store.writeCompressed(0n,new Uint8Array([8,9]));store.writeRaw(0n,new Uint8Array([1,2,3]))
  const publishing=store.publish(manifest);await new Promise(resolve=>setImmediate(resolve));state.complete(state.transactions.at(-1));await publishing
  return {store,manifest}
}
test('borrowed verified detach joins actual transactions and handles then reader adopts without create upgrade rewrite or recovery',async()=>{
  const state=controlledStore(),{store,manifest}=await verifiedBorrowedStore(state)
  store.put('disclosures',{key:'remember',open:true});const flushing=store.flush();let detached=false
  const detaching=store.detach().then(()=>{detached=true})
  await new Promise(resolve=>setImmediate(resolve));assert.equal(detached,false);assert.equal(state.closedHandles,0);assert.equal(state.removed,false)
  state.complete(state.transactions.at(-1));await flushing;await detaching
  assert.equal(state.closedHandles,2);assert.equal(state.dbClosed,true);assert.equal(state.removed,false)
  const beforeWrites=state.writes.length,beforeFiles=state.fileRequests.length;let adopted=false
  const adopting=state.open({custody:borrowedCustody,adoptManifest:manifest}).then(value=>{adopted=true;return value})
  await new Promise(resolve=>setImmediate(resolve));const read=state.transactions.at(-1);assert.equal(read.mode,'readonly');state.respond(read)
  await new Promise(resolve=>setImmediate(resolve));assert.equal(adopted,false,'meta request success is not its terminal receipt');assert.equal(state.closedHandles,2)
  state.complete(read);const reader=await adopting
  assert.equal(state.upgrades,1);assert.equal(state.opens,2);assert.equal(state.writes.length,beforeWrites);assert.ok(state.fileRequests.slice(beforeFiles).every(value=>!value.create));assert.deepEqual([...reader.readRaw('0','3')],[1,2,3])
  assert.throws(()=>reader.put('records',{key:'forbidden'}),/archive_read_only/);assert.throws(()=>reader.writeRaw(0n,new Uint8Array([4])),/archive_read_only/)
  await assert.rejects(reader.publish(manifest),/archive_read_only/);await assert.rejects(reader.compressedStream(),/archive_read_only/)
  const disclosure=reader.disclosure('remember',false);await new Promise(resolve=>setImmediate(resolve));state.complete(state.transactions.at(-1));await disclosure;assert.equal(state.values.get('disclosures:remember').open,false)
  await reader.closeAndDelete();assert.equal(state.closedHandles,3);assert.equal(state.removed,true)
})
test('adoption mismatch missing stage and accidental upgrade never expose or rewrite a reader',async()=>{
  for(const mismatch of ['manifest','schema','keyPath','raw-length','marker','missing','upgrade']){
    const state=controlledStore(),{store,manifest}=await verifiedBorrowedStore(state);await store.detach()
    if(mismatch==='schema')state.db.version=2
    if(mismatch==='keyPath')state.wrongKeyPath=true
    if(mismatch==='raw-length')state.files.set('raw.jsonl',new Uint8Array([1]))
    if(mismatch==='marker'){const marker=JSON.parse(new TextDecoder().decode(state.files.get('owner.json')));marker.custody='wrong';state.files.set('owner.json',new TextEncoder().encode(JSON.stringify(marker)))}
    if(mismatch==='missing')state.dropDirectory()
    if(mismatch==='upgrade')state.created=false
    const before=state.writes.length,reads=state.reads.length
    const adopting=state.open({custody:borrowedCustody,adoptManifest:mismatch==='manifest'?{...manifest,raw_sha256:'c'.repeat(64)}:manifest});adopting.catch(()=>{})
    await new Promise(resolve=>setImmediate(resolve));for(const tx of state.transactions)if(!tx.done){state.respond(tx);state.complete(tx)}
    await assert.rejects(adopting);assert.equal(state.writes.length,before);assert.equal(state.reads.length,reads)
    if(mismatch==='marker')assert.equal(state.removed,false,'unknown marker cannot be deleted')
  }
})
