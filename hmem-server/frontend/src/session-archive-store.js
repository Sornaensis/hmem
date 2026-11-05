// Private to one admitted Worker. No credentials or transcript bodies enter IDB.
export const IO_BYTES = 65536
export const BATCH_ROWS = 256
export const BATCH_BYTES = 1048576
export const PAGE_ROWS = 64
export const PAGE_BYTES = 4194304
export const INDEX_VERSION = 7
const encoder = new TextEncoder(),indexDecoder=new TextDecoder('utf-8',{fatal:true})
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
const INDEX_KEY=/^[0-9]{19}:[0-9]{7}$/
const PAGE_OVERHEAD=serializedBytes({key:'',data:[]})-2
const indexPageBytes=(key,payloadBytes)=>PAGE_OVERHEAD+key.length+payloadBytes

function groupIndexKey(group) {
  const match=/^(0|[1-9][0-9]*):(0|[1-9][0-9]{0,6})$/.exec(group)
  if(!match)throw Error('invalid_archive_group')
  return pieceKey(match[1],match[2])
}
function rowsInIndexPage(page) {
  if(!page)return []
  if(typeof page.key!=='string'||!INDEX_KEY.test(page.key))throw Error('invalid_index_page')
  if(!Object.hasOwn(page,'data'))return [page]
  const data=page.data
  if(!(data instanceof Uint8Array)||data.byteLength<4||indexPageBytes(page.key,data.byteLength)>BATCH_BYTES)throw Error('invalid_index_page')
  let rows
  try{rows=JSON.parse(indexDecoder.decode(data))}catch{throw Error('invalid_index_page')}
  if(!Array.isArray(rows)||!rows.length||rows.length>BATCH_ROWS)throw Error('invalid_index_page')
  let previous=''
  for(const row of rows){
    if(typeof row?.key!=='string'||!INDEX_KEY.test(row.key)||row.key<=previous)throw Error('invalid_index_page')
    previous=row.key
  }
  if(previous!==page.key)throw Error('invalid_index_page')
  return rows
}

function readGroupSummaries(store,keys) {
  if(!keys.length)return Promise.resolve(new Map())
  const ordered=[...keys].sort(),summaries=new Map()
  let index=0
  return new Promise((resolve,reject)=>{
    const operation=store.openCursor(IDBKeyRange.lowerBound(ordered[0]),'next')
    operation.onerror=()=>reject(operation.error)
    operation.onsuccess=()=>{
      try{
        const cursor=operation.result,rows=rowsInIndexPage(cursor?.value)
        if(!rows.length)throw Error('archive_group_missing')
        const last=rows.at(-1).key
        let at=0
        while(index<ordered.length&&ordered[index]<=last){
          const key=ordered[index++]
          while(rows[at]?.key<key)at++
          const summary=rows[at]
          if(summary?.key!==key)throw Error('archive_group_missing')
          if(typeof summary.endTime!=='string'||decimal(summary.count)===0n)throw Error('invalid_archive_group')
          summaries.set(key,{count:summary.count,endTime:summary.endTime})
        }
        if(index<ordered.length)cursor.continue(ordered[index]);else resolve(summaries)
      }catch(error){reject(error)}
    }
  })
}

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
  let encodingScratch
  const transactions = new Set();let pageRead=null
  async function flush() {
    if (!pending.length) return
    const batch=pending.splice(0);pendingBytes=0
    const tx=db.transaction(adoptManifest?['disclosures']:['records','pieces','groups','disclosures','meta'],'readwrite')
    const done=completed(tx);transactions.add(done)
    for (const [table,value] of batch) tx.objectStore(table).put(value)
    try { await done } finally { transactions.delete(done) }
  }
  function indexBytes(value) {
    const json=JSON.stringify(value)
    encodingScratch ??= new Uint8Array(BATCH_BYTES)
    const {read,written:bytes}=encoder.encodeInto(json,encodingScratch)
    if(read!==json.length)throw Error('index_entry_limit')
    return bytes
  }
  function enqueue(table,value,bytes) {
    if(pending.length && (pending.length===BATCH_ROWS || pendingBytes+bytes>BATCH_BYTES)){
      return flush().then(()=>{if(closed)throw Error('retired_store');pending.push([table,value]);pendingBytes+=bytes})
    }
    pending.push([table,value]);pendingBytes+=bytes
  }
  function orderedPages(table) {
    let buffer,end=1,tailStart=1,lastKey='',single
    const keys=[]
    const clear=()=>{end=1;tailStart=1;keys.length=0;single=null}
    function flushPage() {
      if(single){
        const {value,bytes}=single;clear()
        return enqueue(table,value,bytes)
      }
      if(!keys.length)return
      const key=keys.at(-1)
      buffer[end]=93 // ]
      const data=buffer.slice(0,end+1)
      clear()
      return enqueue(table,{key,data},indexPageBytes(key,data.byteLength))
    }
    return {
      put(value,bytes){
        const {key}=value
        if(typeof key!=='string'||!INDEX_KEY.test(key))throw Error('invalid_index_key')
        if(key<lastKey)throw Error('unordered_archive_index')
        if(key===lastKey){
          if(!keys.length&&!single)throw Error('unordered_archive_index')
          // The current group can be extended, and one adjacent event can be
          // replaced by its matching canonical record. Both update the tail.
          if(single)single=null
          else {keys.pop();end=tailStart}
        }
        const prospectiveBytes=indexPageBytes(key,end+(keys.length?1:0)+bytes+1)
        const flushed=single||keys.length===BATCH_ROWS||(keys.length&&prospectiveBytes>BATCH_BYTES)?flushPage():undefined
        // Keep an entry that fits the old bound but cannot fit a page envelope.
        if(indexPageBytes(key,bytes+2)>BATCH_BYTES)single={value,bytes}
        else{
          if(!buffer){buffer=new Uint8Array(BATCH_BYTES);buffer[0]=91} // [
          tailStart=end
          if(keys.length)buffer[end++]=44 // ,
          buffer.set(encodingScratch.subarray(0,bytes),end);end+=bytes;keys.push(key)
        }
        lastKey=key
        return flushed
      },
      flush:flushPage,
      clear(){clear();buffer=null;lastKey=''}
    }
  }
  const piecePages=orderedPages('pieces'),groupPages=orderedPages('groups')
  function put(table,value) {
    if(closed)throw Error('retired_store')
    if(adoptManifest&&table!=='disclosures')throw Error('archive_read_only')
    if(table==='groups')value={...value,key:groupIndexKey(value.key)}
    const bytes=indexBytes(value)
    if(table==='pieces')return piecePages.put(value,bytes)
    if(table==='groups')return groupPages.put(value,bytes)
    return enqueue(table,value,bytes)
  }
  async function readIndex(table,action) {
    await flush()
    if(closed)throw Error('retired_store')
    const tx=db.transaction(table,'readonly')
    const terminal=completed(tx).then(()=>({}),error=>({error}))
    transactions.add(terminal)
    try{
      let result,primary
      try{result=await action(tx.objectStore(table))}catch(error){primary=error}
      const ending=await terminal
      if(primary&&ending.error&&primary!==ending.error)throw new AggregateError([primary,ending.error],'archive_index_read_failed')
      if(primary||ending.error)throw primary||ending.error
      if(closed)throw Error('retired_store')
      return result
    }finally{transactions.delete(terminal)}
  }
  async function get(table,key) {
    if(table!=='pieces')return readIndex(table,store=>request(store.get(key)))
    return readIndex(table,async store=>{
      const cursor=await request(store.openCursor(IDBKeyRange.lowerBound(key),'next'))
      return rowsInIndexPage(cursor?.value).find(piece=>piece.key===key)
    })
  }
  async function scan(table,{after,before,limit=PAGE_ROWS}={}) {
    if(limit<1 || limit>PAGE_ROWS)throw Error('page_limit')
    const backwards=!!before,rows=[]
    await readIndex(table,store=>new Promise((resolve,reject)=>{
      const collect=page=>{
        const pieces=rowsInIndexPage(page)
        for(let index=backwards?pieces.length-1:0;index>=0&&index<pieces.length&&rows.length<limit;index+=backwards?-1:1){
          const piece=pieces[index]
          if(after?piece.key>after:!before||piece.key<before)rows.push(piece)
        }
      }
      const walk=range=>{
        const operation=store.openCursor(range,backwards?'prev':'next')
        operation.onerror=()=>reject(operation.error)
        operation.onsuccess=()=>{
          try{
            const cursor=operation.result
            if(!cursor){resolve();return}
            collect(cursor.value)
            if(rows.length===limit)resolve();else cursor.continue()
          }catch(error){reject(error)}
        }
      }
      if(!before||after||before==='\uffff'){walk(after?IDBKeyRange.lowerBound(after,true):undefined);return}
      // The first page whose last key reaches the exclusive cursor may also
      // contain earlier pieces. Read that page before walking older fences.
      const crossing=store.openCursor(IDBKeyRange.lowerBound(before),'next')
      crossing.onerror=()=>reject(crossing.error)
      crossing.onsuccess=()=>{
        try{
          const cursor=crossing.result
          if(cursor)collect(cursor.value)
          if(rows.length===limit)resolve()
          else walk(cursor?IDBKeyRange.upperBound(cursor.value.key,true):undefined)
        }catch(error){reject(error)}
      }
    }))
    return backwards?rows.reverse():rows
  }
  async function readPageMetadata(pieces) {
    if(!Array.isArray(pieces)||pieces.length>PAGE_ROWS||pieces.some(piece=>!piece||typeof piece.key!=='string'||typeof piece.group!=='string'))throw Error('page_limit')
    if(closed)throw Error('retired_store')
    await flush()
    if(closed)throw Error('retired_store')
    if(pageRead)throw Error('archive_page_read_busy')
    if(!pieces.length)return []
    const groups=new Map(pieces.filter(piece=>piece.group).map(piece=>[piece.group,groupIndexKey(piece.group)]))
    const tx=db.transaction(['disclosures','groups'],'readonly')
    const terminal=new Promise(resolve=>{tx.oncomplete=()=>resolve({});tx.onabort=()=>resolve({error:tx.error || Error('index_transaction_aborted')})})
    const owned={tx,terminal};pageRead=owned
    let primary,values,summaries
    const operations=[]
    const get=(table,key)=>{const pending=request(tx.objectStore(table).get(key)).then(value=>value,error=>{primary ??= error});operations.push(pending)}
    try {
      try {
        for(const piece of pieces)get('disclosures',piece.key)
        for(const group of groups.keys())get('disclosures','group:'+group)
        operations.push(readGroupSummaries(tx.objectStore('groups'),[...groups.values()]).then(value=>{summaries=value},error=>{primary ??= error}))
      }catch(error){primary ??= error}
      values=await Promise.all(operations)
      const result=await terminal
      if(result.error && primary && result.error!==primary)throw new AggregateError([primary,result.error],'archive_index_read_failed')
      if(primary)throw primary
      if(result.error)throw result.error
      if(closed)throw Error('retired_store')
      const indexes=new Map([...groups.keys()].map((key,index)=>[key,index]))
      return pieces.map((piece,index)=>{
        const group=indexes.get(piece.group),summary=summaries.get(groups.get(piece.group))
        return {open:values[index]?.open || false,groupOpen:group===undefined?false:values[pieces.length+group]?.open || false,count:summary?.count || '0',endTime:summary?.endTime || ''}
      })
    }finally{await terminal;if(pageRead===owned)pageRead=null}
  }
  async function closeHandles() {
    closed=true;rawReads.retire();pending.length=0;pendingBytes=0;encodingScratch=null;piecePages.clear();groupPages.clear()
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
      if(verified.indexVersion!==INDEX_VERSION)throw Error('archive_index_stale')
    }
    const compressedFile=await directory.getFileHandle('compressed.gz',{create:!adoptManifest})
    const rawFile=await directory.getFileHandle('raw.jsonl',{create:!adoptManifest})
    if(!compressedFile.createSyncAccessHandle || !rawFile.createSyncAccessHandle)throw Error('archive_unsupported')
    if(adoptManifest){
      const contentBytes=adoptManifest.content_bytes??adoptManifest.raw_bytes
      if(BigInt((await rawFile.getFile()).size)!==decimal(contentBytes)||BigInt((await compressedFile.getFile()).size)!==decimal(adoptManifest.compressed_bytes))throw Error('archive_receipt_mismatch')
      raw=await rawFile.createSyncAccessHandle();rawReads.publish(contentBytes)
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
      async releaseReader(){if(!custody||!adoptManifest)throw Error('archive_reader_required');await closeHandles()},
      async publish(manifest){
        if(adoptManifest)throw Error('archive_read_only')
        if(closed)throw Error('retired_store')
        raw.flush();await piecePages.flush();await groupPages.flush()
        await put('meta',{key:'verified',manifest,indexVersion:INDEX_VERSION});await flush()
        if(closed)throw Error('retired_store')
        rawReads.publish(manifest.content_bytes??manifest.raw_bytes)
      },
      async disclosure(key,open){await put('disclosures',{key,open});await flush()},
      closeAndDelete
    }
  } catch(error) {
    try{await closeAndDelete()}catch{throw Error('archive_cleanup_failed')}
    throw error
  }
}
