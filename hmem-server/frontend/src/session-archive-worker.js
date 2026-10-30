import {sha256} from '@noble/hashes/sha256'
import {MAX_RECORD_BYTES,presentation,semanticTransition,visibleBlocks,createPrettyPayloadReader,payloadTokens} from './session-viewer.js'
import {openArchiveStore,decimal,fileOffset,pieceKey,positionKey,serializedBytes,IO_BYTES,PAGE_ROWS,PAGE_BYTES} from './session-archive-store.js'

const utf8=new TextDecoder('utf-8',{fatal:true}),encoder=new TextEncoder()
// One compressed write can enqueue many decoded chunks before backpressure.
const GZIP_INPUT_BYTES=4096
const hex=bytes=>Array.from(bytes,byte=>byte.toString(16).padStart(2,'0')).join('')
export function validateManifest(value,workspace,session) {
  if(value?.state!=='ready' || value.workspace_id!==workspace || value.session_id!==session || value.codec!=='gzip-v1'
    || typeof value.generation_id!=='string' || !/^[a-f0-9-]{36}$/.test(value.generation_id)
    || !/^[a-f0-9]{64}$/.test(value.raw_sha256) || !/^[a-f0-9]{64}$/.test(value.compressed_sha256))throw Error('invalid_manifest')
  for(const field of ['raw_bytes','raw_records','compressed_bytes'])decimal(value[field])
  fileOffset(decimal(value.raw_bytes));fileOffset(decimal(value.compressed_bytes))
  const path=`/api/v1/workspaces/${encodeURIComponent(workspace)}/sessions/${encodeURIComponent(session)}/archive/${value.generation_id}/content`
  if(value.content_path!==path)throw Error('invalid_content_path')
  return value
}

// Strict sealed EOF differs from the live finite-window adapter. At most one
// record and one 64 KiB input are retained; offsets never pass through Number.
export function archiveChunkOffset(base,local) {
  if(typeof base!=='bigint' || base<0n || !Number.isSafeInteger(local) || local<0 || local>IO_BYTES)throw Error('invalid_chunk_offset')
  return base+BigInt(local)
}
export function createArchiveFramer() {
  let offset=0n,start=0n,size=0,piVersion=null
  const pending=new Uint8Array(MAX_RECORD_BYTES+2)
  function record(end,delimiter) {
    const length=size-(delimiter==='\r\n'?1:0)
    if(length>MAX_RECORD_BYTES)throw Error('record_limit')
    const raw=utf8.decode(pending.subarray(0,length))
    let value;try{value=JSON.parse(raw)}catch{throw Error('invalid_json_record')}
    if(value?.type==='session')piVersion=Number.isInteger(value.version)?value.version:'invalid'
    const row=presentation(value,raw,{archive:true,piVersion})
    Object.assign(row,{offset:start.toString(),presentationOffset:start.toString(),end:end.toString(),delimiter})
    size=0;start=end;return row
  }
  return { *push(input,final=false) {
    if(!(input instanceof Uint8Array) || input.length>IO_BYTES)throw Error('chunk_limit')
    const base=offset
    for(let index=0;index<input.length;index++) {
      const byte=input[index]
      if(byte===10){offset=archiveChunkOffset(base,index+1);yield record(offset,size && pending[size-1]===13?'\r\n':'\n')}
      else {if(size===pending.length)throw Error('record_limit');pending[size++]=byte}
    }
    offset=archiveChunkOffset(base,input.length)
    if(final && size)yield record(offset,'')
  },get through(){return offset} }
}

export function gzipPipeline(stream) {
  const decoder=new DecompressionStream('gzip')
  const source=stream.getReader(),writer=decoder.writable.getWriter()
  let resolveCleanup,rejectCleanup
  const cleanup=new Promise((resolve,reject)=>{resolveCleanup=resolve;rejectCleanup=reject})
  // The producer owns actual source/writer handles and a separate cleanup
  // receipt. A decoder error is not itself an upstream cleanup failure.
  const completion=(async()=>{
    let eof=false,primary
    try {
      while(true){const part=await source.read();if(part.done){eof=true;break}
        for(let at=0;at<part.value.length;at+=GZIP_INPUT_BYTES)await writer.write(part.value.subarray(at,at+GZIP_INPUT_BYTES))}
      await writer.close()
    }catch(error){primary=error}
    try {
      if(!eof)await source.cancel(primary)
      await source.closed.catch(error=>{if(error!==primary)throw error})
      if(primary)await writer.abort(primary)
      source.releaseLock();writer.releaseLock();resolveCleanup()
    }catch(error){rejectCleanup(error);throw new AggregateError([primary,error].filter(Boolean),'archive_cleanup_failed')}
    if(primary)throw primary
  })()
  return {readable:decoder.readable,completion,cleanup}
}
export function createArchiveEngine({openStore=openArchiveStore,emit=()=>{},storage=globalThis.navigator?.storage,decode=gzipPipeline}={}) {
  let store,manifest,stamp,downloadHash,downloadBytes=0n,rawHash,rawBytes=0n,records=0n,retired=false,published=false,reader
  let previous=null,anchor=null,group=null
  let pipelineCompletion,pipelineCleanup,pipelinePrimary
  let stageName,stageCustody,mode='processor',detached=false
  let formatter=null
  let cleanupPoisoned=false
  let nativeOutputChunkBytes=0,downloadReported=0n,downloadReportedAt=0
  let storageEstimateLow=false
  const current=()=>{if(retired)throw Error('retired_archive')}
  const progress=phase=>emit({...stamp,kind:'progress',phase,compressedBytes:downloadBytes.toString(),rawBytes:rawBytes.toString(),nativeOutputChunkBytes,storageEstimateLow})
  async function quota() {
    const estimate=await storage?.estimate?.().catch(()=>null)
    if(estimate?.quota && estimate?.usage && estimate.usage>estimate.quota)storageEstimateLow=true
  }
  // A bounded enqueue is synchronous; only an actual flush/group read yields.
  function index(row) {
    const transition=semanticTransition(row,previous,anchor)
    previous=transition.previous;anchor=transition.anchor
    const pending=store.put('records',{key:positionKey(row.offset),offset:row.offset,end:row.end,delimiter:row.delimiter})
    return pending?pending.then(()=>indexContent(row,transition)):indexContent(row,transition)
  }
  function indexContent(row,transition) {
    current();records++
    if(transition.hidden)return
    return indexBlocks(row,transition,visibleBlocks(row),0)
  }
  function writePiece(row,index,block) {
    return store.put('pieces',{key:pieceKey(row.presentationOffset,index),offset:row.offset,end:row.end,delimiter:row.delimiter,index,kind:block.kind,
      label:block.label || '',role:row.role,time:row.time,group:row.activityAnchors[index] || '',...(Object.hasOwn(row,'piVersion')?{piVersion:row.piVersion}:{})})
  }
  function afterGroup(row,transition,blocks,at) {
    current()
    const [index,block]=blocks[at],pending=writePiece(row,index,block)
    return pending?pending.then(()=>indexBlocks(row,transition,blocks,at+1)):indexBlocks(row,transition,blocks,at+1)
  }
  function indexBlocks(row,transition,blocks,at) {
    for(;at<blocks.length;at++){
      current()
      const [index,block]=blocks[at],identity=row.activityAnchors[index] || ''
      if(block.kind!=='text'){
        if(group?.key!==identity)return store.get('groups',identity).then(value=>{current();group=value || {key:identity,count:'0'};return indexBlocks(row,transition,blocks,at)})
        if(!transition.paired)group={...group,count:(BigInt(group.count)+1n).toString()}
        const pending=store.put('groups',group)
        if(pending)return pending.then(()=>afterGroup(row,transition,blocks,at))
      }
      const pending=writePiece(row,index,block)
      if(pending)return pending.then(()=>indexBlocks(row,transition,blocks,at+1))
    }
  }
  async function body(piece,part=0,pin=false) {
    current()
    if(!Number.isSafeInteger(part) || part<0)throw Error('invalid_body_part')
    if(formatter?.key===piece.key) {
      const cached=formatter.recent.find(value=>value.part===part)
      if(cached)return cached
      if(formatter.next===part){const value=formatter.reader.read();value.tokens=payloadTokens(value.text);value.format=formatter.format;value.structured=formatter.structured;value.exception=formatter.exception;formatter.next++;formatter.recent=[...formatter.recent.slice(-1),value];return value}
    }
    const bytes=store.readRaw(piece.offset,piece.end)
    const length=bytes.length-piece.delimiter.length
    const raw=utf8.decode(bytes.subarray(0,length)),value=JSON.parse(raw),row=presentation(value,raw,{archive:true,piVersion:piece.piVersion ?? null})
    const block=row.type==='conversation'?JSON.parse(row.text)[piece.index]:{kind:'text',text:row.text,label:''}
    if(!block || block.kind!==piece.kind)throw Error('index_mismatch')
    if(['tool','result'].includes(block.kind) || block.pretty) {
      const parsed=JSON.parse(block.text)
      let format='json'
      let exception=false
      if(block.pretty){
        const probe=createPrettyPayloadReader(parsed);let size=0
        while(size<=1048576){const value=probe.read();size+=encoder.encode(value.text).length;if(!value.more){format=size<=1048576?'markdown':'json';break}}
        exception=format==='json'
      }
      const next={key:piece.key,reader:createPrettyPayloadReader(parsed),next:0,recent:[],format,structured:!!block.pretty,exception}
      if(pin || !formatter)formatter=next
      let value
      for(let index=0;index<=part;index++){current();value=next.reader.read();if(index && index%256===0)await new Promise(resolve=>setTimeout(resolve,0))}
      value.tokens=payloadTokens(value.text);value.format=format;value.structured=!!block.pretty;value.exception=exception;next.next=part+1;next.recent=[value];return value
    }
    // UTF-16 slices are at most 64 KiB UTF-8. Avoid splitting surrogate pairs.
    let begin=part*16384,end=Math.min(block.text.length,begin+16384)
    if(begin && /[\uDC00-\uDFFF]/.test(block.text[begin]))begin--
    if(end<block.text.length && /[\uDC00-\uDFFF]/.test(block.text[end]))end--
    return {text:block.text.slice(begin,end),part,more:end<block.text.length,format:'markdown'}
  }
  return {
    async start(message) {
      stamp=message.stamp;manifest=validateManifest(message.manifest,stamp.workspace,stamp.session)
      if(typeof Worker==='undefined' || typeof DecompressionStream==='undefined')throw Error('archive_unsupported')
      const estimate=await storage?.estimate?.().catch(()=>null)
      const required=decimal(manifest.compressed_bytes)+decimal(manifest.raw_bytes)*2n+16777216n
      if(estimate?.quota!==undefined && estimate?.usage!==undefined && required>BigInt(Math.floor(Math.max(0,estimate.quota-estimate.usage))))storageEstimateLow=true
      current()
      stageName=message.name;stageCustody=message.custody
      try{store=await openStore(message.name,{custody:message.custody})}catch(error){if(error.message==='archive_cleanup_failed')cleanupPoisoned=true;throw error}
      current();downloadHash=sha256.create();progress('downloading')
    },
    async adopt(message) {
      current();mode='reader';stamp=message.stamp;manifest=validateManifest(message.manifest,stamp.workspace,stamp.session)
      stageName=message.name;stageCustody=message.custody
      if(!stageCustody)throw Error('archive_custody_required')
      try{store=await openStore(stageName,{custody:stageCustody,adoptManifest:manifest})}catch(error){if(error.message==='archive_cleanup_failed')cleanupPoisoned=true;throw error}
      current();downloadBytes=decimal(manifest.compressed_bytes);rawBytes=decimal(manifest.raw_bytes);records=decimal(manifest.raw_records);published=true;progress('ready')
    },
    async detach() {
      current();if(!published||mode!=='processor'||!stageCustody)throw Error('archive_unverified')
      await store.detach();current();detached=true;retired=true;published=false
      downloadHash?.destroy();rawHash?.destroy();downloadHash=null;rawHash=null;previous=null;anchor=null;group=null;formatter=null
      return {name:stageName,custody:stageCustody,manifest}
    },
    async chunk(bytes) {
      current();if(mode!=='processor')throw Error('archive_read_only')
      if(!(bytes instanceof Uint8Array) || bytes.length>IO_BYTES || published)throw Error('chunk_limit')
      if(downloadBytes+BigInt(bytes.length)>decimal(manifest.compressed_bytes))throw Error('compressed_count_mismatch')
      store.writeCompressed(downloadBytes,bytes);downloadHash.update(bytes);downloadBytes+=BigInt(bytes.length)
      const now=performance.now()
      if(downloadBytes>downloadReported && (downloadReported===0n || now-downloadReportedAt>=1000 || downloadBytes===decimal(manifest.compressed_bytes))){current();downloadReported=downloadBytes;downloadReportedAt=now;progress('downloading')}
    },
    async finish() {
      current();if(mode!=='processor')throw Error('archive_read_only')
      if(downloadBytes!==decimal(manifest.compressed_bytes) || hex(downloadHash.digest())!==manifest.compressed_sha256)throw Error('compressed_receipt_mismatch')
      // Publication barrier one: decoder creation happens only after this check.
      progress('decoding');rawHash=sha256.create()
      const source=await store.compressedStream();current()
      const pipeline=decode(source)
      if(!pipeline?.readable || !pipeline?.completion || !pipeline?.cleanup)throw Error('invalid_decoder_ownership')
      pipelineCompletion=Promise.resolve(pipeline.completion).then(()=>({}),error=>({error}))
      pipelineCleanup=Promise.resolve(pipeline.cleanup).then(()=>({}),error=>({error}))
      reader=pipeline.readable.getReader()
      const framer=createArchiveFramer();let sinceYield=0,lastProgress=0
      let eof=false,primary
      try {
        while(true) {
          current();const result=await reader.read();if(result.done){eof=true;break}
          nativeOutputChunkBytes=Math.max(nativeOutputChunkBytes,result.value.byteLength)
          for(let begin=0;begin<result.value.length;begin+=IO_BYTES) {
            current();const bytes=result.value.subarray(begin,begin+IO_BYTES)
            if(rawBytes+BigInt(bytes.length)>decimal(manifest.raw_bytes))throw Error('raw_count_mismatch')
            store.writeRaw(rawBytes,bytes);rawHash.update(bytes);rawBytes+=BigInt(bytes.length)
            for(const row of framer.push(bytes)){const pending=index(row);if(pending)await pending}
            if(++sinceYield===16){sinceYield=0;await store.flush();await quota();await new Promise(resolve=>setTimeout(resolve,0))}
            if(performance.now()-lastProgress>1000){lastProgress=performance.now();progress('decoding')}
          }
        }
        for(const row of framer.push(new Uint8Array(),true)){const pending=index(row);if(pending)await pending}
      }catch(error){primary=error;pipelinePrimary=error;throw error}
      finally{
        if(!eof){
          try{await reader.cancel(primary)}
          catch(error){if(error!==primary)throw new AggregateError([primary,error].filter(Boolean),'archive_cleanup_failed')}
        }
        await reader.closed.catch(error=>{if(error!==primary)throw error})
        const result=await pipelineCompletion
        const cleanup=await pipelineCleanup
        if(cleanup.error)throw new AggregateError([primary,cleanup.error].filter(Boolean),'archive_cleanup_failed')
        reader.releaseLock();reader=null
        if(result.error && !primary){pipelinePrimary=result.error;throw result.error}
      }
      current()
      if(rawBytes!==decimal(manifest.raw_bytes) || records!==decimal(manifest.raw_records) || hex(rawHash.digest())!==manifest.raw_sha256)throw Error('raw_receipt_mismatch')
      await store.publish(manifest);current();published=true;previous=null;group=null
      // Publication barrier two: no page is available until strict EOF + receipt.
      progress('ready')
    },
    async page({after,before,last=false,offset,query='',bodyKey,bodyPart=0,searchCursor}={}) {
      current();if(!published)throw Error('archive_unverified')
      if(query.length>256)throw Error('search_limit')
      let cursor=after,rows=[],visited=0,searchParts=0,resuming=searchCursor,wireBytes=2,bodyBytes=0
      if(offset!==undefined)cursor=positionKey(decimal(offset))+':'
      do {
        const page=resuming?[await store.get('pieces',resuming.key)]:await store.scan('pieces',{after:cursor,before:last?'\uffff':before,limit:PAGE_ROWS})
        if(!page.length)break
        const metadata=await store.readPageMetadata(page);current()
        const backwards=!!before||last
        for(let pieceIndex=backwards?page.length-1:0;backwards?pieceIndex>=0:pieceIndex<page.length;pieceIndex+=backwards?-1:1) {
          current()
          const piece=page[pieceIndex],item={...piece,...metadata[pieceIndex]}
          if(query || piece.kind==='text' || (bodyKey===piece.key) || item.open)item.body=await body(piece,query&&resuming?resuming.part:bodyKey===piece.key?bodyPart:0,!!query||bodyKey===piece.key)
          if(query) {
            let segment=item.body,overlap=resuming?.overlap || '',matched=false
            const needle=query.toLocaleLowerCase()
            while(true){
              current();const text=overlap+segment.text.toLocaleLowerCase()
              if(text.includes(needle)){matched=true;break}
              if(!segment.more)break
              overlap=needle.length>1?text.slice(-(needle.length-1)):''
              if(++searchParts===16)return {rows,first:rows[0]?.key || '',last:rows.at(-1)?.key || '',more:true,searchCursor:{key:piece.key,part:segment.part+1,overlap}}
              segment=await body(piece,segment.part+1,true)
              await new Promise(resolve=>setTimeout(resolve,0))
            }
            if(!matched)continue
            item.body=segment
          }
          const size=serializedBytes(item),payload=item.body?serializedBytes(item.body):0
          // Reserve room for markup, disclosure controls and the one inspector
          // mirror as well as the actual wire representation.
          if(rows.length===PAGE_ROWS || wireBytes+size+1>1048576 || (rows.length && bodyBytes+payload>131072)){
            if(!rows.length)throw Error('archive_item_limit')
            return {rows,first:rows[0].key,last:rows.at(-1).key,more:true,...(query?{searchAfter:rows.at(-1).key}:{})}
          }
          wireBytes+=size+1;bodyBytes+=payload
          if(backwards)rows.unshift(item);else rows.push(item)
        }
        cursor=page.at(-1).key
        if(resuming)return {rows,first:rows[0]?.key || '',last:rows.at(-1)?.key || '',more:true,searchAfter:cursor}
        if(!query || page.length<PAGE_ROWS || rows.length===PAGE_ROWS)break
        if(++visited===1)return {rows,first:rows[0]?.key || '',last:rows.at(-1)?.key || '',more:true,searchAfter:cursor}
      }while(true)
      return {rows,first:rows[0]?.key || '',last:rows.at(-1)?.key || '',more:rows.length===PAGE_ROWS,...(query?{searchAfter:cursor}:{})}
    },
    async disclosure(key,open){current();if(!published)throw Error('archive_unverified');await store.disclosure(key,open)},
    async hydrate(key,part){current();if(!published)throw Error('archive_unverified');const piece=await store.get('pieces',key);if(!piece)throw Error('archive_piece_missing');return {key,body:await body(piece,part,true)}},
    retire(){retired=true;formatter=null;store?.retireReads?.()},
    async cleanup(){
      retired=true;formatter=null;store?.retireReads?.()
      if(detached)return
      if(cleanupPoisoned)throw Error('archive_cleanup_failed')
      if(reader){
        await reader.cancel(pipelinePrimary).catch(error=>{if(error!==pipelinePrimary)throw error})
        await reader.closed.catch(error=>{if(error!==pipelinePrimary)throw error})
        reader.releaseLock();reader=null
      }
      await pipelineCompletion
      const result=await pipelineCleanup
      if(result?.error)throw Error('archive_cleanup_failed')
      downloadHash?.destroy();rawHash?.destroy();formatter=null;if(store)await store.closeAndDelete()
    },
    snapshot:()=>({downloadBytes:downloadBytes.toString(),rawBytes:rawBytes.toString(),records:records.toString(),published,nativeOutputChunkBytes})
  }
}

// Commands are serialized. Retirement is synchronous and interrupts decode at
// its next bounded yield; cleanup is acknowledged only after all handles close.
if(typeof self!=='undefined' && typeof document==='undefined' && typeof self.postMessage==='function') {
  let engine,tail=Promise.resolve(),owner,currentRequest
  self.onmessage=({data})=>{
    if(data.kind==='interrupt'){engine?.retire();return}
    if(data.kind==='retire')engine?.retire()
    tail=tail.then(async()=>{
      currentRequest=data.request
      if(data.kind==='start'||data.kind==='adopt'){owner=data.stamp;engine=createArchiveEngine({emit:value=>self.postMessage({...value,request:currentRequest})});if(data.kind==='adopt')await engine.adopt(data);else await engine.start(data)}
      else if(JSON.stringify(data.stamp)!==JSON.stringify(owner))return
      else if(data.kind==='chunk')await engine.chunk(new Uint8Array(data.bytes))
      else if(data.kind==='finish')await engine.finish()
      else if(data.kind==='detach'){const stage=await engine.detach();self.postMessage({...owner,kind:'detached',request:data.request,stage});return}
      else if(data.kind==='page')self.postMessage({...owner,kind:'page',request:data.request,...await engine.page(data.page)})
      else if(data.kind==='body')self.postMessage({...owner,kind:'body',request:data.request,...await engine.hydrate(data.key,data.part)})
      else if(data.kind==='disclosure')await engine.disclosure(data.key,data.open)
      else if(data.kind==='retire'){await engine.cleanup();self.postMessage({...owner,kind:'cleaned',request:data.request});return}
      self.postMessage({...owner,kind:'ack',request:data.request})
    }).catch(error=>{engine?.retire();self.postMessage({...owner,kind:'error',request:data.request,code:error.name==='QuotaExceededError'?'archive_quota':error.message})})
  }
}
