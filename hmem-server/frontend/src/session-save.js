import { createRecordFramer, decodeBase64 } from './session-viewer.js'
import { createZip64Writer, evidenceEntryName } from './session-zip.js'
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
export function installSessionBridge(app, { document = globalThis.document, window = globalThis.window, now = () => Date.now() } = {}) {
  let stamp = '', generation = -1, framer = null
  let activeSave = null
  const send = value => queueMicrotask(()=>app.ports.sessionBridgeEvent.send(value))
  let heartbeatKey = ''
  async function retireSave(owned = activeSave) {
    if (!owned) return true
    if (owned.cleanupFailed) return false
    owned.retired = true
    try {
      if(owned.writer) await owned.writer.abort()
      if(owned.archive) await owned.archive.abort()
      else if(!owned.writer && owned.native) await owned.native.abort()
      if(!owned.picking && !owned.acquiring && activeSave === owned) activeSave = null
      return true
    } catch { reportCleanupFailure(owned);return false }
  }
  function reportCleanupFailure(owned){
    owned.cleanupFailed=true
    if(!owned.cleanupReported){owned.cleanupReported=true;send({kind:owned.task?'archive-error':'save-error',stamp,operation:owned.operation,code:'writer_cleanup_failed'})}
  }
  async function archiveFailure(owned,error){
    const cleaned=await retireSave(owned)
    if(error.message==='writer_cleanup_failed'&&!owned.cleanupReported)reportCleanupFailure(owned)
    if(cleaned&&!owned.cleanupFailed)send({kind:'archive-error',stamp:owned.stamp,operation:owned.operation,code:error.message})
  }
  const triggerArchive = async (owned,file,cleanup=async()=>{}) => {
    if(owned.retired || stamp!==owned.stamp) throw Error('retired_download')
    const url=window.URL.createObjectURL(file),link=document.createElement('a')
    link.href=url;link.download=evidenceEntryName({name:owned.title,id:owned.task}).replace(/\.jsonl$/,'.zip');link.click()
    let disposed=false
    const dispose=()=>{if(disposed)return;disposed=true;window.URL.revokeObjectURL(url);void cleanup().catch(()=>{})}
    window.setTimeout(dispose,60000);window.addEventListener?.('pagehide',dispose,{once:true})
  }
  async function archiveStorage(owned) {
    let root,handles=[],writers=[],names=[]
    try {
      if(!window.navigator?.storage?.getDirectory)throw Error('storage_unavailable')
      root=await(await window.navigator.storage.getDirectory()).getDirectoryHandle('hmem-downloads',{create:true})
      const prefix='hmem-download-'+Date.now()+'-'+window.crypto.randomUUID()
      for(const extension of ['.zip','.index']){const name=prefix+extension;names.push(name);const handle=await root.getFileHandle(name,{create:true});handles.push(handle);writers.push(await handle.createWritable())}
    }catch(error){
      const results=await Promise.allSettled(writers.map(writer=>writer.abort()));
      if(root)results.push(...await Promise.allSettled(names.map(async name=>{try{await root.removeEntry(name)}catch(error){if(error.name!=='NotFoundError')throw error}})))
      if(results.some(result=>result.status==='rejected')){owned.cleanupFailed=true;throw Error('writer_cleanup_failed')}
      let used=0,archive=[],central=[]
      const retain=(list,bytes)=>{if(used+bytes.length>33554432)throw Error('download_storage_unavailable');list.push(new Blob([bytes]));used+=bytes.length}
      return {output:{async write(bytes){retain(archive,bytes)},async close(){const file=new Blob(archive,{type:'application/zip'});archive=[];await triggerArchive(owned,file);used=0},async abort(){archive=[]}},index:{async write(bytes){retain(central,bytes)},async *readChunks(){for(let i=0;i<central.length;i++){const part=central[i];yield new Uint8Array(await part.arrayBuffer());used-=part.size;central[i]=null}central=[]},async abort(){central=[]}}}
    }
    const closing=[null,null],aborting=[null,null],removing=[null,null]
    const close=i=>closing[i]||=(Promise.resolve().then(()=>writers[i].close()))
    const remove=i=>removing[i]||=(Promise.resolve().then(()=>root.removeEntry(names[i])).catch(error=>{if(error.name!=='NotFoundError')throw error}))
    const abort=i=>aborting[i]||=(async()=>{try{if(closing[i])await closing[i];else await writers[i].abort()}finally{await remove(i)}})()
    return {
      output:{write:bytes=>writers[0].write(bytes),async close(){await close(0);await triggerArchive(owned,await handles[0].getFile(),()=>remove(0))},abort:()=>abort(0)},
      index:{write:bytes=>writers[1].write(bytes),async *readChunks(){await close(1);const file=await handles[1].getFile();for(let offset=0;offset<file.size;offset+=65536)yield new Uint8Array(await file.slice(offset,offset+65536).arrayBuffer())},abort:()=>abort(1)}
    }
  }
  document.addEventListener('click',event=>{
    const button=event.target.closest?.('[data-session-archive]');if(!button||button.disabled)return
    event.preventDefault();let config;try{config=JSON.parse(button.dataset.sessionArchive)}catch{return}
    if(config.stamp!==stamp||activeSave)return
    const owned={...config,retired:false,writer:null,native:null,archive:null,picking:false,acquiring:true,entry:null,finishing:false};activeSave=owned
    send({kind:'archive-picking',stamp:owned.stamp,operation:owned.operation,task:owned.task,title:owned.title})
    ;(async()=>{try{
      const storage=await archiveStorage(owned)
      owned.archive=createZip64Writer(storage.output,storage.index,()=>activeSave===owned&&!owned.retired&&stamp===owned.stamp)
      owned.acquiring=false
      if(owned.retired||stamp!==owned.stamp){await retireSave(owned);return}
      send({kind:'archive-ready',stamp:owned.stamp,operation:owned.operation})
    }catch(error){owned.acquiring=false;await archiveFailure(owned,error)}})()
  })
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
    if (command.stamp !== stamp) { stamp = command.stamp; framer = null; generation = -1; void retireSave() }
    if(activeSave && activeSave.operation < command.nextOperation && command.saveOperation !== activeSave.operation) void retireSave()
    if(command.archiveEntry && activeSave?.archive && activeSave.operation===command.saveOperation && !activeSave.acquiring && !activeSave.writer){
      const owned=activeSave,entry=command.archiveEntry;owned.acquiring=true
      ;(async()=>{try{
        const sink=await owned.archive.entry(evidenceEntryName({name:entry.name,id:entry.session.id}),entry.session.finalBytes)
        owned.writer=createEvidenceWriter(sink,entry.session,()=>activeSave===owned&&!owned.retired&&stamp===owned.stamp)
        owned.acquiring=false
        if(owned.retired||stamp!==owned.stamp){await retireSave(owned);return}
        send({kind:'save-ready',stamp:owned.stamp,operation:owned.operation})
      }catch(error){owned.acquiring=false;await archiveFailure(owned,error)}})()
    }
    if(command.archiveFinish && activeSave?.archive && activeSave.operation===command.saveOperation && !activeSave.finishing){
      const owned=activeSave;owned.finishing=true
      owned.archive.finish().then(result=>{if(activeSave!==owned||owned.retired||stamp!==owned.stamp)return;activeSave=null;send({kind:'archive-complete',stamp:owned.stamp,operation:owned.operation,entries:result.entries})}).catch(async error=>{await archiveFailure(owned,error)})
    }
    if(command.saveChunk && activeSave && activeSave.operation === command.saveOperation && activeSave.writer) {
      const owned = activeSave
      owned.writer.write(command.saveChunk).then(result => {
        if(activeSave !== owned || owned.retired || stamp !== owned.stamp) return
        if(result.complete){if(owned.archive)owned.writer=null;else activeSave=null}
        send({kind:result.complete?'save-complete':'save-progress',stamp:owned.stamp,operation:owned.operation,offset:result.offset})
      }).catch(async error => {
        if(await retireSave(owned))send({kind:'save-error',stamp:owned.stamp,operation:owned.operation,code:error.message==='download_storage_unavailable'?'download_storage_unavailable':'write_or_verification_failed'})
      })
    }
    const statuses = Object.fromEntries((command.heartbeats || []).slice(0,52).map(session=> {
      const instant = Date.parse(session.lastSeenAt)
      return [session.id,Number.isFinite(instant)?(now()-instant>=30000?'Stale (30s)':'Fresh'):'Unknown heartbeat']
    }))
    const fingerprint = stamp+JSON.stringify(statuses)
    if(fingerprint!==heartbeatKey){heartbeatKey=fingerprint;send({kind:'heartbeat-status',stamp,statuses})}
    if (command.reset !== null) { generation = command.generation; framer = createRecordFramer(command.reset) }
    if (!command.chunk) return
    const captured = { stamp:command.stamp, generation:command.generation }
    try {
      if (!framer || generation !== command.generation) throw new Error('retired_window')
      const bytes = decodeBase64(command.chunk.dataBase64)
      if (bytes.length !== command.chunk.nextOffset-command.chunk.offset) throw new Error('range_length')
      const result = framer.push(command.chunk.offset,bytes,command.chunk.final)
      send({ kind:'viewer',...captured,...result })
      if (command.follow) requestAnimationFrame(() => {
        if (stamp !== captured.stamp || generation !== captured.generation) return
        const viewport = document.getElementById('session-records')
        if (viewport) viewport.scrollTop = viewport.scrollHeight
      })
    } catch {
      send({ kind:'viewer-error',...captured })
    }
  })
}
