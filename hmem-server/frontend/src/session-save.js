import { createRecordFramer, decodeBase64 } from './session-viewer.js'
import { sha256 } from '@noble/hashes/sha256'

const hex = bytes => Array.from(bytes,byte => byte.toString(16).padStart(2,'0')).join('')

export function createEvidenceWriter(writable, session, isCurrent = () => true) {
  if (!session || session.state !== 'terminated' || !Number.isSafeInteger(session.finalBytes) || session.finalBytes < 0 || !/^[a-f0-9]{64}$/.test(session.finalSha256)) throw new Error('invalid_receipt')
  const hash = sha256.create()
  let offset = 0, busy = false, retired = false, closed = false, running = Promise.resolve(), aborting
  async function abortNative() {
    retired = true
    if (!closed) { aborting ||= Promise.resolve().then(()=>writable.abort()); await aborting; closed = true }
    hash.destroy()
  }
  return {
    async abort() { await abortNative(); await running.catch(()=>{}) },
    write(chunk) {
      if (busy || closed || retired || !isCurrent()) return Promise.reject(new Error('retired_writer'))
      busy = true
      running = (async()=>{ try {
        const receipt = chunk.session
        if (!receipt || receipt.id !== session.id || receipt.workspace !== session.workspace || receipt.state !== 'terminated' || receipt.finalBytes !== session.finalBytes || receipt.finalSha256 !== session.finalSha256 || receipt.revision !== session.revision) throw new Error('receipt_mismatch')
        const bytes = decodeBase64(chunk.dataBase64)
        if (chunk.offset !== offset || chunk.nextOffset !== offset+bytes.length || chunk.nextOffset > session.finalBytes || (!bytes.length && offset !== session.finalBytes)) throw new Error('cursor_mismatch')
        if (bytes.length) await writable.write(bytes)
        if (retired || !isCurrent()) throw new Error('retired_writer')
        hash.update(bytes); offset += bytes.length
        if (offset === session.finalBytes) {
          if (hex(hash.digest()) !== session.finalSha256) throw new Error('digest_mismatch')
          if (retired || !isCurrent()) throw new Error('retired_writer')
          await writable.close(); closed = true
          if (retired || !isCurrent()) throw new Error('retired_writer')
        }
        return { offset, complete:closed }
      } catch (error) {
        await abortNative()
        throw error
      } finally { busy = false }
      })()
      return running
    },
    snapshot:() => ({offset,busy,closed,retired})
  }
}

// Elm owns demand and the two HTTP slots. This bridge only consumes one
// validated chunk at a time and acknowledges it before Elm can rearm demand.
export function installSessionBridge(app, { document = globalThis.document, window = globalThis.window, now = () => Date.now(), fetchArchive = async()=>{throw Error('archive_transport_unavailable')} } = {}) {
  let stamp = '', viewerStamp = '', generation = -1, framer = null, viewerIntent = 0
  let activeSave = null
  const send = value => queueMicrotask(()=>app.ports.sessionBridgeEvent.send(value))
  let heartbeatKey = ''
  const noteViewerIntent = event => {
    const viewport=document.getElementById?.('session-records')
    if(viewport && (event.target===viewport || viewport.contains?.(event.target))) viewerIntent++
  }
  // A scroll notification may lag the position change or come from browser
  // anchoring. Actual input/disclosure events own intent; the position guard
  // below also detects later programmatic scrolling in either direction.
  for(const event of ['toggle','wheel','touchstart','pointerdown','keydown']) document.addEventListener(event,noteViewerIntent,true)
  async function retireSave(owned = activeSave) {
    if (!owned) return true
    if (owned.cleanupFailed) return false
    owned.retired = true
    owned.controller?.abort()
    try {
      if(owned.writer) await owned.writer.abort()
      if(!owned.writer && owned.native) await owned.native.abort()
      if(!owned.picking && !owned.acquiring && !owned.reading && activeSave === owned) activeSave = null
      return true
    } catch { reportCleanupFailure(owned);return false }
  }
  function reportCleanupFailure(owned){
    owned.cleanupFailed=true
    if(!owned.cleanupReported){owned.cleanupReported=true;send({kind:owned.task?'archive-error':'save-error',stamp,operation:owned.operation,code:'writer_cleanup_failed'})}
  }
  const archiveFilename=owned=>String(owned.title||'Task evidence').normalize('NFKC').replace(/[^\p{L}\p{N}._ -]/gu,'-').replace(/\.{2,}/g,'-').replace(/^[. -]+|[. -]+$/g,'').slice(0,72)+'-'+owned.task+'.zip'
  async function prepareArchive(owned){
    const trigger=async(file,cleanup=async()=>{})=>{
      if(owned.retired||stamp!==owned.stamp)throw Error('retired_download')
      const url=window.URL.createObjectURL(file),link=document.createElement('a');link.href=url;link.download=archiveFilename(owned);link.click()
      let disposed=false;const dispose=()=>{if(disposed)return;disposed=true;window.URL.revokeObjectURL(url);void cleanup().catch(()=>{})}
      window.setTimeout(dispose,60000);window.addEventListener?.('pagehide',dispose,{once:true})
    }
    let root,name,handle,writable
    try{
      if(!window.navigator?.storage?.getDirectory)throw Error('storage_unavailable')
      root=await(await window.navigator.storage.getDirectory()).getDirectoryHandle('hmem-downloads',{create:true})
      name='hmem-download-'+Date.now()+'-'+window.crypto.randomUUID()+'.zip'
      handle=await root.getFileHandle(name,{create:true});writable=await handle.createWritable()
    }catch(error){
      const attempts=[];if(writable)attempts.push(writable.abort());if(root&&name)attempts.push(root.removeEntry(name).catch(error=>{if(error.name!=='NotFoundError')throw error}))
      const results=await Promise.allSettled(attempts);if(results.some(result=>result.status==='rejected')){owned.cleanupFailed=true;throw Error('writer_cleanup_failed')}
    }
    if(writable){
      let closing,aborting,removing
      const remove=()=>removing||=(root.removeEntry(name).catch(error=>{if(error.name!=='NotFoundError')throw error}))
      return{write:bytes=>writable.write(bytes),async close(){closing||=writable.close();await closing;await trigger(await handle.getFile(),remove)},abort(){return aborting||=(async()=>{try{if(closing)await closing;else await writable.abort()}finally{await remove()}})()}}
    }
    let bytes=0,parts=[]
    return{async write(chunk){if(bytes+chunk.length>33554432)throw Error('download_storage_unavailable');parts.push(new Blob([chunk]));bytes+=chunk.length},async close(){const file=new Blob(parts,{type:'application/zip'});parts=[];await trigger(file)},async abort(){parts=[]}}
  }
  document.addEventListener('click',event=>{
    const button=event.target.closest?.('[data-session-archive]');if(!button||button.disabled)return
    event.preventDefault();let config;try{config=JSON.parse(button.dataset.sessionArchive)}catch{return}
    if(config.stamp!==stamp||activeSave)return
    const owned={...config,retired:false,writer:null,native:null,picking:false,acquiring:true,reading:false,request:null,controller:null};activeSave=owned
    send({kind:'archive-picking',stamp:owned.stamp,operation:owned.operation,task:owned.task,title:owned.title})
    ;(async()=>{try{owned.native=await prepareArchive(owned);owned.acquiring=false;if(owned.retired||stamp!==owned.stamp){await retireSave(owned);return}send({kind:'archive-ready',stamp:owned.stamp,operation:owned.operation})}
      catch(error){owned.acquiring=false;if(error.message==='writer_cleanup_failed')reportCleanupFailure(owned);else if(await retireSave(owned))send({kind:'archive-error',stamp:owned.stamp,operation:owned.operation,code:error.message})}})()
  })
  const archiveRequests=[]
  function receiveArchiveRequest(request){
    if(archiveRequests.includes(request.id))return
    archiveRequests.push(request.id);if(archiveRequests.length>32)archiveRequests.shift()
    const owned=activeSave
    if(!owned?.task||owned.retired||owned.operation!==request.operation||owned.stamp!==request.stamp||owned.task!==request.task){send({kind:'archive-http',stamp:request.stamp,operation:request.operation,request:request.id,status:0});return}
    owned.request=request.id;owned.controller=new AbortController();owned.reading=true
    ;(async()=>{
      let status=0,success=false,bytes=0,reader
      try{
        const transport=await fetchArchive(request.url,{signal:owned.controller.signal}),response=transport.response
        status=response.status
        if(!response.ok||!response.body||!/^application\/zip(?:;|$)/i.test(response.headers.get('Content-Type')||''))throw Error('archive_response_failed')
        reader=response.body.getReader()
        const current=()=>!owned.retired&&activeSave===owned&&stamp===owned.stamp&&transport.isCurrent()
        while(true){if(!current())throw Error('retired_download');const part=await reader.read();if(part.done)break
          for(let offset=0;offset<part.value.length;offset+=65536){if(!current())throw Error('retired_download');const chunk=part.value.subarray(offset,offset+65536);await owned.native.write(chunk);bytes+=chunk.length}
          send({kind:'archive-progress',stamp:owned.stamp,operation:owned.operation,offset:bytes})
        }
        if(!current())throw Error('retired_download');await owned.native.close();success=true
      }catch(error){status=status>=400?status:0;await retireSave(owned)}
      finally{
        try{if(!success)await reader?.cancel()}catch{}
        reader?.releaseLock();owned.reading=false
        if(success){if(activeSave===owned)activeSave=null}else await retireSave(owned)
        send({kind:'archive-http',stamp:owned.stamp,operation:owned.operation,request:request.id,status:success?200:status})
      }
    })()
  }
  document.addEventListener('click', event => {
    const button=event.target.closest?.('[data-session-download]')
    if(!button || button.disabled) return
    event.preventDefault()
    let config
    try {config=JSON.parse(button.dataset.sessionDownload)} catch {return}
    if(config.stamp!==stamp || activeSave) return
    const owned={...config,retired:false,writer:null,native:null,picking:false,acquiring:true}
    activeSave=owned
    send({kind:'save-picking',stamp:owned.stamp,operation:owned.operation,session:owned.session})
    ;(async()=>{
      const filename=config.session.id+'.jsonl'
      const trigger=async (file,cleanup=async()=>{})=>{
        if(owned.retired || stamp!==owned.stamp) throw new Error('retired_download')
        const url=window.URL.createObjectURL(file),link=document.createElement('a')
        link.href=url;link.download=filename;link.click()
        let disposed=false
        const dispose=()=>{if(disposed)return;disposed=true;window.URL.revokeObjectURL(url);void cleanup().catch(()=>{})}
        window.setTimeout(dispose,60000)
        window.addEventListener?.('pagehide',dispose,{once:true})
      }
      let root,temporary
      try {
        if(window.navigator?.storage?.getDirectory) {
          root=await (await window.navigator.storage.getDirectory()).getDirectoryHandle('hmem-downloads',{create:true})
          temporary='hmem-download-'+Date.now()+'-'+window.crypto.randomUUID()+'.jsonl'
          const handle=await root.getFileHandle(temporary,{create:true})
          const writable=await handle.createWritable()
          let removed=false
          const remove=async()=>{if(!removed){await root.removeEntry(temporary);removed=true}}
          owned.native={write:bytes=>writable.write(bytes),async close(){try{await writable.close();await trigger(await handle.getFile(),remove)}catch(error){await remove();throw error}},async abort(){try{await writable.abort()}finally{await remove()}}}
        }
      } catch {
        if(root&&temporary) await root.removeEntry(temporary).catch(()=>{})
      }
      if(!owned.native) {
        // Older browsers retain a finite download in memory; never allocate
        // unbounded transcripts when durable temporary storage is unavailable.
        if(config.session.finalBytes>33554432) throw new Error('download_storage_unavailable')
        let chunks=[]
        owned.native={async write(bytes){chunks.push(bytes)},async close(){await trigger(new Blob(chunks,{type:'application/x-ndjson'}));chunks=[]},async abort(){chunks=[]}}
      }
      owned.acquiring=false
      owned.writer=createEvidenceWriter(owned.native,owned.session,()=>activeSave===owned&&!owned.retired&&stamp===owned.stamp)
      if(owned.retired || stamp!==owned.stamp){await retireSave(owned);return}
      send({kind:'save-ready',stamp:owned.stamp,operation:owned.operation})
    })().catch(async error=>{owned.acquiring=false;if(await retireSave(owned))send({kind:'save-error',stamp:owned.stamp,operation:owned.operation,code:error.message==='download_storage_unavailable'?'download_storage_unavailable':'download_failed'})})
  })
  document.addEventListener('click', event => {
    const button = event.target.closest?.('[data-session-save]')
    if (!button || button.disabled) return
    event.preventDefault()
    let config
    try { config = JSON.parse(button.dataset.sessionSave) } catch { return }
    if(config.stamp !== stamp || activeSave) return
    if(typeof window.showSaveFilePicker !== 'function') { send({kind:'save-error',stamp,operation:config.operation,code:'unsupported'}); return }
    const owned = { ...config, retired:false, writer:null, native:null, picking:true, acquiring:false }
    activeSave = owned
    // Invoke the chooser directly inside this user activation, before awaiting
    // a port round-trip or a network request. No local pathname leaves JS.
    let picking
    try { picking = window.showSaveFilePicker({suggestedName:config.session.id+'.jsonl',types:[{description:'Original JSONL evidence',accept:{'application/x-ndjson':['.jsonl']}}]}) }
    catch { activeSave=null; send({kind:'save-error',stamp:owned.stamp,operation:owned.operation,code:'picker_failed'}); return }
    send({kind:'save-picking',stamp:owned.stamp,operation:owned.operation,session:owned.session})
    Promise.resolve(picking).then(async handle => {
      owned.picking = false
      if(owned.retired || activeSave !== owned || stamp !== owned.stamp) { await retireSave(owned); return }
      owned.acquiring = true
      try {
        owned.native = await handle.createWritable()
        owned.writer = createEvidenceWriter(owned.native,owned.session,()=>activeSave === owned && !owned.retired && stamp === owned.stamp)
      } finally { owned.acquiring = false }
      if(owned.retired || activeSave !== owned || stamp !== owned.stamp) { await retireSave(owned); return }
      send({kind:'save-ready',stamp:owned.stamp,operation:owned.operation})
    }).catch(async () => {
      owned.picking = false
      owned.acquiring = false
      if(await retireSave(owned))send({kind:'save-error',stamp:owned.stamp,operation:owned.operation,code:'picker_cancelled_or_failed'})
    })
  })
  queueMicrotask(()=>send({kind:'save-capability',supported:typeof window.showSaveFilePicker === 'function'}))
  app.ports.sessionBridgeCommand.subscribe(command => {
    const downloadStamp = command.downloadStamp ?? command.stamp
    if (downloadStamp !== stamp) { stamp = downloadStamp; void retireSave() }
    if (command.stamp !== viewerStamp) { viewerStamp = command.stamp; framer = null; generation = -1 }
    if(activeSave && activeSave.operation < command.nextOperation && command.saveOperation !== activeSave.operation) void retireSave()
    if(command.archiveRequest)receiveArchiveRequest(command.archiveRequest)
    if(command.saveChunk && activeSave && activeSave.operation === command.saveOperation && activeSave.writer) {
      const owned = activeSave
      owned.writer.write(command.saveChunk).then(result => {
        if(activeSave !== owned || owned.retired || stamp !== owned.stamp) return
        if(result.complete)activeSave=null
        send({kind:result.complete?'save-complete':'save-progress',stamp:owned.stamp,operation:owned.operation,offset:result.offset})
      }).catch(async error => {
        if(await retireSave(owned))send({kind:'save-error',stamp:owned.stamp,operation:owned.operation,code:error.message==='download_storage_unavailable'?'download_storage_unavailable':'write_or_verification_failed'})
      })
    }
    const statuses = Object.fromEntries((command.heartbeats || []).slice(0,52).map(session=> {
      const instant = Date.parse(session.lastSeenAt)
      return [session.id,Number.isFinite(instant)?(now()-instant>=30000?'Stale (30s)':'Fresh'):'Unknown heartbeat']
    }))
    const localNow = new Date(now())
    const today = `${localNow.getFullYear()}-${String(localNow.getMonth()+1).padStart(2,'0')}-${String(localNow.getDate()).padStart(2,'0')}`
    const fingerprint = viewerStamp+today+JSON.stringify(statuses)
    if(fingerprint!==heartbeatKey){heartbeatKey=fingerprint;send({kind:'heartbeat-status',stamp:viewerStamp,statuses,today})}
    if (command.generation < generation) return
    generation = command.generation
    if (command.reset !== null) {
      framer = createRecordFramer(command.reset)
    }
    if (!command.chunk) return
    const captured = { stamp:command.stamp, generation:command.generation, intent:viewerIntent }
    try {
      if (!framer || generation !== command.generation) throw new Error('retired_window')
      const bytes = decodeBase64(command.chunk.dataBase64)
      if (bytes.length !== command.chunk.nextOffset-command.chunk.offset) throw new Error('range_length')
      const viewport = document.getElementById('session-records')
      const atTail = !viewport || viewport.scrollHeight - viewport.scrollTop - viewport.clientHeight <= 48
      const previousTop = viewport?.scrollTop || 0
      const anchors = !atTail && viewport?.getBoundingClientRect && viewport?.querySelectorAll ? (()=>{
        const bounds = viewport.getBoundingClientRect()
        const visible = selector => [...viewport.querySelectorAll(selector)].filter(node=>{
          const rect=node.getBoundingClientRect()
          return rect.height>0 && rect.bottom>bounds.top && rect.top<bounds.bottom
        }).slice(0,8).map(node=>({attribute:node.hasAttribute('data-block-key')?'data-block-key':'data-group-key',key:node.getAttribute(node.hasAttribute('data-block-key')?'data-block-key':'data-group-key'),top:node.getBoundingClientRect().top-bounds.top}))
        return [...visible('[data-block-key]'),...visible('[data-group-key]')]
      })() : []
      const result = framer.push(command.chunk.offset,bytes,command.chunk.final)
      // A finite captured end owns publication. A large initial window (or a
      // live burst) advances the bounded byte handshake without painting and
      // following every intermediate 64 KiB history fragment.
      if (!command.chunk.final) { send({kind:'viewer-progress',...captured,through:result.through}); return }
      send({kind:'viewer',...captured,...result})
      if (command.follow && atTail) requestAnimationFrame(() => {
        if (viewerStamp !== captured.stamp || generation !== captured.generation || viewerIntent !== captured.intent) return
        const viewport = document.getElementById('session-records')
        if (viewport && Math.abs(viewport.scrollTop-previousTop)<=4) viewport.scrollTop = viewport.scrollHeight
      })
      else if (anchors.length) requestAnimationFrame(()=>{
        if (viewerStamp !== captured.stamp || generation !== captured.generation || viewerIntent !== captured.intent) return
        const viewport=document.getElementById('session-records')
        if (!viewport || Math.abs(viewport.scrollTop-previousTop)>4) return
        const bounds=viewport.getBoundingClientRect()
        for(const anchor of anchors) {
          const node=[...viewport.querySelectorAll(`[${anchor.attribute}]`)].find(node=>node.getAttribute(anchor.attribute)===anchor.key)
          if(node && node.getBoundingClientRect().height>0) {
            viewport.scrollTop += node.getBoundingClientRect().top-bounds.top-anchor.top
            break
          }
        }
      })
    } catch {
      send({ kind:'viewer-error',...captured })
    }
  })
}
