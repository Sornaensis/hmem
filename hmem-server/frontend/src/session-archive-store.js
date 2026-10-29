// Private to one admitted Worker. No credentials or transcript bodies enter IDB.
export const IO_BYTES = 65536
export const BATCH_ROWS = 256
export const BATCH_BYTES = 1048576
export const PAGE_ROWS = 64
export const PAGE_BYTES = 4194304
const encoder = new TextEncoder()
const UUID=/^[a-f0-9]{8}-[a-f0-9]{4}-[a-f0-9]{4}-[a-f0-9]{4}-[a-f0-9]{12}$/
const NAMESPACE='hmem-session-archives-v1'
const DATABASE='hmem-session-archive-v1-'
export const serializedBytes = value => encoder.encode(JSON.stringify(value)).length
export function decimal(value) {
  if (typeof value !== 'string' || !/^(0|[1-9][0-9]*)$/.test(value) || BigInt(value) > 9223372036854775807n) throw Error('invalid_count')
  return BigInt(value)
}
export function fileOffset(value) {
  const number = Number(value)
  if (typeof value !== 'bigint' || value < 0n || !Number.isSafeInteger(number) || BigInt(number) !== value) throw Error('unsupported_file_offset')
  return number
}
export const positionKey = value => decimal(String(value)).toString().padStart(19,'0')
export const pieceKey = (offset,index) => `${positionKey(offset)}:${String(index).padStart(7,'0')}`

// Only a successfully published immutable raw file may populate these two slots.
// Copying out keeps consumers from mutating either verified block.
export function createRawReadCache(nativeRead) {
  let verifiedSize=null,retired=false,dirty=false,blocks=[],slots=[]
  const clear=()=>{for(const slot of slots){slot.bytes.fill(0);slot.validLength=0;slot.base=null}blocks=[]}
  const invalidate=()=>{clear();if(verifiedSize!==null)dirty=true}
  const direct=(start,size)=>{
    const at=fileOffset(start);fileOffset(start+size)
    const bytes=new Uint8Array(Number(size))
    if(bytes.length&&nativeRead(bytes,{at})!==bytes.length)throw Error('file_short_read')
    return bytes
  }
  return {
    invalidate,
    publish(size){if(retired)throw Error('retired_store');clear();verifiedSize=decimal(size);dirty=false},
    retire(){retired=true;clear();slots=[]},
    snapshot:()=>({blocks:blocks.length,bytes:blocks.reduce((sum,block)=>sum+block.validLength,0),capacityBytes:slots.length*IO_BYTES,verified:verifiedSize!==null&&!dirty&&!retired,retired}),
    read(start,end){
      try{
        if(retired)throw Error('retired_store')
        if(dirty)throw Error('archive_unverified')
        const first=decimal(start),last=decimal(end),size=last-first
        if(size<0n||size>1048578n)throw Error('record_limit')
        fileOffset(first);fileOffset(last)
        if(verifiedSize!==null&&last>verifiedSize)throw Error('file_short_read')
        if(verifiedSize===null||size>BigInt(IO_BYTES))return direct(first,size)
        const result=new Uint8Array(Number(size));let at=first,written=0
        while(at<last){
          const base=at/BigInt(IO_BYTES)*BigInt(IO_BYTES)
          let block=blocks.find(value=>value.base===base)
          if(!block){
            const remaining=verifiedSize-base,safeRemaining=BigInt(Number.MAX_SAFE_INTEGER)-base
            const available=remaining<safeRemaining?remaining:safeRemaining,length=available<BigInt(IO_BYTES)?available:BigInt(IO_BYTES)
            block=slots.find(slot=>!blocks.includes(slot))
            if(!block){
              if(slots.length===2)block=blocks.pop()
              else{block={base:null,bytes:new Uint8Array(IO_BYTES),validLength:0};slots.push(block)}
            }
            block.bytes.fill(0);block.validLength=0;block.base=null
            const at=fileOffset(base);fileOffset(base+length)
            const view=block.bytes.subarray(0,Number(length))
            if(nativeRead(view,{at})!==view.length)throw Error('file_short_read')
            block.base=base;block.validLength=view.length
          }
          blocks=[block,...blocks.filter(value=>value!==block)].slice(0,2)
          const offset=Number(at-base),count=Math.min(block.validLength-offset,Number(last-at))
          if(count<=0)throw Error('file_short_read')
          result.set(block.bytes.subarray(offset,offset+count),written);written+=count;at+=BigInt(count)
        }
        return result
      }catch(error){clear();throw error}
    }
  }
}

const request = operation => new Promise((resolve,reject)=>{operation.onsuccess=()=>resolve(operation.result);operation.onerror=()=>reject(operation.error)})
const completed = transaction => new Promise((resolve,reject)=>{transaction.oncomplete=resolve;transaction.onabort=()=>reject(transaction.error || Error('index_transaction_failed'))})

async function generationLock(locks,name) {
  let admit,reject,release
  const admission=new Promise((resolve,no)=>{admit=resolve;reject=no}),hold=new Promise(resolve=>{release=resolve})
  const released=locks.request(DATABASE+name,{mode:'exclusive',ifAvailable:true},async lock=>{
    admit(!!lock);if(lock)await hold
  })
  released.catch(reject)
  if(!await admission){await released;return null}
  return async()=>{release();await released}
}


export async function acquireArchiveCustody(name,{locks=globalThis.navigator?.locks}={}) {
  if(!UUID.test(name)||!locks?.request)throw Error('archive_unsupported')
  const release=await generationLock(locks,name)
  if(!release)throw Error('archive_owner_busy')
  return release
}
export async function deleteArchiveStage(name,custody,{storage=globalThis.navigator?.storage,indexedDB=globalThis.indexedDB}={}) {
  if(!UUID.test(name)||!UUID.test(custody))throw Error('invalid_archive_owner')
  let root,directory
  try{root=await(await storage.getDirectory()).getDirectoryHandle(NAMESPACE);directory=await root.getDirectoryHandle(name)}catch(error){if(error.name==='NotFoundError')return;throw error}
  const file=await(await directory.getFileHandle('owner.json')).getFile()
  if(file.size>256)throw Error('archive_owner_mismatch')
  const marker=JSON.parse(await file.text())
  if(marker.format!=='hmem-session-archive'||marker.version!==1||marker.owner!==name||marker.custody!==custody)throw Error('archive_owner_mismatch')
  await request(indexedDB.deleteDatabase(DATABASE+name));await root.removeEntry(name,{recursive:true})
}

// Recover only this format's marked UUID directories. Per-generation locks
// protect other live tabs; no origin-wide lifetime mutex is held. An incomplete
// bounded sweep blocks new disk admission and makes progress on explicit retry.
async function recover(root,locks,indexedDB) {
  let inspected=0,removed=0
  for await(const [name,entry] of root.entries()) {
    if(++inspected>128)throw Error('archive_recovery_limit')
    if(removed===8)throw Error('archive_recovery_pending')
    if(entry.kind!=='directory' || !UUID.test(name))continue
    const release=await generationLock(locks,name)
    if(!release)continue
    let marker
    try {
      const file=await (await entry.getFileHandle('owner.json')).getFile()
      if(file.size<=256)marker=JSON.parse(await file.text())
    }catch(error){if(error.name!=='NotFoundError' && !(error instanceof SyntaxError)){await release();throw error}}
    if(marker?.format!=='hmem-session-archive' || marker.version!==1 || marker.owner!==name){await release();continue}
    try {
      await request(indexedDB.deleteDatabase(DATABASE+name))
      await root.removeEntry(name,{recursive:true});removed++
    }catch(error){
      // Keep the actual lock on an uncertain deletion. The Worker owner must
      // report cleanup failure and cannot admit a replacement generation.
      throw Error('archive_cleanup_failed',{cause:error})
    }
    await release()
  }
}

export async function openArchiveStore(name, {storage=globalThis.navigator?.storage, indexedDB=globalThis.indexedDB, locks=globalThis.navigator?.locks, custody, adoptManifest}={}) {
  if (!storage?.getDirectory || !indexedDB || !locks?.request) throw Error('archive_unsupported')
  if(!UUID.test(name))throw Error('invalid_archive_owner')
  if(custody!==undefined && !UUID.test(custody))throw Error('invalid_archive_custody')
  if(adoptManifest && !custody)throw Error('archive_custody_required')
  const root = await (await storage.getDirectory()).getDirectoryHandle(NAMESPACE,{create:!adoptManifest})
  if(!adoptManifest)await recover(root,locks,indexedDB)
  const releaseOwner=custody?null:await generationLock(locks,name)
  if(!custody&&!releaseOwner)throw Error('archive_owner_busy')
  let exists=false
  try{await root.getDirectoryHandle(name);exists=true}catch(error){if(error.name!=='NotFoundError'){await releaseOwner?.();throw error}}
  if(!adoptManifest&&exists){await releaseOwner?.();throw Error('archive_owner_exists')}
  if(adoptManifest&&!exists)throw Error('archive_stage_missing')
  let directory,db,compressed,raw,markerWriter,markerClosed=false,closed=false,stageOwned=false
  const rawReads=createRawReadCache((bytes,options)=>raw.read(bytes,options))
  const database = DATABASE+name
  const pending = []; let pendingBytes=0
  const transactions = new Set();let pageRead=null
  async function flush() {
    if (!pending.length) return
    const batch=pending.splice(0);pendingBytes=0
    const tx=db.transaction(adoptManifest?['disclosures']:['records','pieces','groups','disclosures','meta'],'readwrite')
    const done=completed(tx);transactions.add(done)
    for (const [table,value] of batch) tx.objectStore(table).put(value)
    try { await done } finally { transactions.delete(done) }
  }
  function put(table,value) {
    if(closed)throw Error('retired_store')
    if(adoptManifest&&table!=='disclosures')throw Error('archive_read_only')
    const bytes=serializedBytes(value)
    if(bytes>BATCH_BYTES)throw Error('index_entry_limit')
    if(pending.length && (pending.length===BATCH_ROWS || pendingBytes+bytes>BATCH_BYTES)){
      return flush().then(()=>{if(closed)throw Error('retired_store');pending.push([table,value]);pendingBytes+=bytes})
    }
    pending.push([table,value]);pendingBytes+=bytes
  }
  async function get(table,key) {
    await flush()
    const tx=db.transaction(table,'readonly'),done=completed(tx)
    const result=await request(tx.objectStore(table).get(key));await done;return result
  }
  async function scan(table,{after,before,limit=PAGE_ROWS}={}) {
    await flush()
    if(limit<1 || limit>PAGE_ROWS)throw Error('page_limit')
    const tx=db.transaction(table,'readonly'),done=completed(tx),rows=[]
    const store=tx.objectStore(table)
    const range=after?IDBKeyRange.lowerBound(after,true):before?IDBKeyRange.upperBound(before,true):undefined
    const operation=store.openCursor(range,before?'prev':'next')
    await new Promise((resolve,reject)=>{operation.onerror=()=>reject(operation.error);operation.onsuccess=()=>{
      const cursor=operation.result
      if(!cursor || rows.length===limit){resolve();return}
      rows.push(cursor.value);cursor.continue()
    }})
    await done
    return before?rows.reverse():rows
  }
  async function readPageMetadata(pieces) {
    if(!Array.isArray(pieces)||pieces.length>PAGE_ROWS||pieces.some(piece=>!piece||typeof piece.key!=='string'||typeof piece.group!=='string'))throw Error('page_limit')
    if(closed)throw Error('retired_store')
    await flush()
    if(closed)throw Error('retired_store')
    if(pageRead)throw Error('archive_page_read_busy')
    if(!pieces.length)return []
    const groups=[...new Set(pieces.map(piece=>piece.group).filter(Boolean))]
    const tx=db.transaction(['disclosures','groups'],'readonly')
    const terminal=new Promise(resolve=>{tx.oncomplete=()=>resolve({});tx.onabort=()=>resolve({error:tx.error || Error('index_transaction_aborted')})})
    const owned={tx,terminal};pageRead=owned
    let primary,values
    const operations=[]
    const get=(table,key)=>{const pending=request(tx.objectStore(table).get(key)).then(value=>value,error=>{primary ??= error});operations.push(pending)}
    try {
      try {
        for(const piece of pieces)get('disclosures',piece.key)
        for(const group of groups)get('disclosures','group:'+group)
        for(const group of groups)get('groups',group)
      }catch(error){primary ??= error}
      values=await Promise.all(operations)
      const result=await terminal
      if(result.error && primary && result.error!==primary)throw new AggregateError([primary,result.error],'archive_index_read_failed')
      if(primary)throw primary
      if(result.error)throw result.error
      if(closed)throw Error('retired_store')
      const indexes=new Map(groups.map((key,index)=>[key,index]))
      return pieces.map((piece,index)=>{const group=indexes.get(piece.group);return {open:values[index]?.open || false,groupOpen:group===undefined?false:values[pieces.length+group]?.open || false,count:group===undefined?'0':values[pieces.length+groups.length+group]?.count || '0'}})
    }finally{await terminal;if(pageRead===owned)pageRead=null}
  }
  async function closeHandles() {
    closed=true;rawReads.retire();pending.length=0;pendingBytes=0
    if(pageRead)await pageRead.terminal
    await Promise.all([...transactions])
    const failures=[]
    if(markerWriter&&!markerClosed){try{await markerWriter.abort();markerClosed=true}catch(error){failures.push(error)}}
    for(const kind of ['compressed','raw']){
      const handle=kind==='compressed'?compressed:raw
      if(handle)try{handle.close();if(kind==='compressed')compressed=null;else raw=null}catch(error){failures.push(error)}
    }
    if(failures.length)throw Error('archive_cleanup_failed',{cause:new AggregateError(failures)})
    db?.close();db=null
  }
  async function closeAndDelete() {
    await closeHandles()
    if(!stageOwned)throw Error('archive_owner_mismatch')
    await request(indexedDB.deleteDatabase(database))
    try{await root.removeEntry(name,{recursive:true})}catch(error){if(error.name!=='NotFoundError')throw error}
    await releaseOwner?.()
  }
  try {
    directory=await root.getDirectoryHandle(name,{create:!adoptManifest})
    if(!adoptManifest)stageOwned=true
    if(adoptManifest){
      const markerFile=await(await directory.getFileHandle('owner.json')).getFile()
      if(markerFile.size>256)throw Error('archive_owner_mismatch')
      const marker=JSON.parse(await markerFile.text())
      if(marker.format!=='hmem-session-archive'||marker.version!==1||marker.owner!==name||marker.custody!==custody)throw Error('archive_owner_mismatch')
      stageOwned=true
    }else{
      markerWriter=await (await directory.getFileHandle('owner.json',{create:true})).createWritable()
      await markerWriter.write(JSON.stringify({format:'hmem-session-archive',version:1,owner:name,...(custody?{custody}:{})}));await markerWriter.close();markerClosed=true
    }
    const opening=indexedDB.open(database,1)
    opening.onupgradeneeded=()=>{
      if(adoptManifest){opening.transaction.abort();return}
      const next=opening.result
      for(const table of ['records','groups','disclosures','meta'])next.createObjectStore(table,{keyPath:'key'})
      next.createObjectStore('pieces',{keyPath:'key'})
    }
    db=await request(opening)
    if(adoptManifest){
      if(db.version!==1||JSON.stringify([...db.objectStoreNames].sort())!==JSON.stringify(['disclosures','groups','meta','pieces','records']))throw Error('archive_schema_mismatch')
      const tx=db.transaction(['disclosures','groups','meta','pieces','records'],'readonly'),terminal=new Promise(resolve=>{tx.oncomplete=()=>resolve({});tx.onabort=()=>resolve({error:tx.error||Error('archive_adoption_aborted')})})
      transactions.add(terminal)
      let verified,primary
      try{
        for(const table of ['disclosures','groups','meta','pieces','records']){const schema=tx.objectStore(table);if(schema.keyPath!=='key'||schema.autoIncrement!==false||schema.indexNames.length)throw Error('archive_schema_mismatch')}
        verified=await request(tx.objectStore('meta').get('verified'))
      }catch(error){primary=error;try{tx.abort()}catch{}}
      const ending=await terminal;transactions.delete(terminal)
      if(primary)throw primary;if(ending.error)throw ending.error
      if(JSON.stringify(verified?.manifest)!==JSON.stringify(adoptManifest))throw Error('archive_receipt_mismatch')
    }
    const compressedFile=await directory.getFileHandle('compressed.gz',{create:!adoptManifest})
    const rawFile=await directory.getFileHandle('raw.jsonl',{create:!adoptManifest})
    if(!compressedFile.createSyncAccessHandle || !rawFile.createSyncAccessHandle)throw Error('archive_unsupported')
    if(adoptManifest){
      if(BigInt((await rawFile.getFile()).size)!==decimal(adoptManifest.raw_bytes)||BigInt((await compressedFile.getFile()).size)!==decimal(adoptManifest.compressed_bytes))throw Error('archive_receipt_mismatch')
      raw=await rawFile.createSyncAccessHandle();rawReads.publish(adoptManifest.raw_bytes)
    }else{compressed=await compressedFile.createSyncAccessHandle();raw=await rawFile.createSyncAccessHandle()}
    const write=(handle,offset,bytes)=>{
      if(adoptManifest)throw Error('archive_read_only')
      if(closed || bytes.length>IO_BYTES)throw Error('file_write_limit')
      rawReads.invalidate()
      if(handle.write(bytes,{at:fileOffset(offset)})!==bytes.length)throw Error('file_short_write')
    }
    return {name,put,get,scan,flush,readPageMetadata,
      writeCompressed:(offset,bytes)=>write(compressed,offset,bytes),
      writeRaw:(offset,bytes)=>write(raw,offset,bytes),
      async compressedStream(){if(adoptManifest)throw Error('archive_read_only');compressed.flush();compressed.close();compressed=null;return (await compressedFile.getFile()).stream()},
      readRaw:(start,end)=>rawReads.read(start,end),
      retireReads:()=>rawReads.retire(),
      async detach(){if(!custody||adoptManifest)throw Error('archive_custody_required');await closeHandles()},
      async publish(manifest){if(adoptManifest)throw Error('archive_read_only');if(closed)throw Error('retired_store');raw.flush();await put('meta',{key:'verified',manifest});await flush();if(closed)throw Error('retired_store');rawReads.publish(manifest.raw_bytes)},
      async disclosure(key,open){await put('disclosures',{key,open});await flush()},
      closeAndDelete
    }
  } catch(error) {
    try{await closeAndDelete()}catch{throw Error('archive_cleanup_failed')}
    throw error
  }
}
