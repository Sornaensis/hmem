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
export function installSessionBridge(app, { document = globalThis.document, window = globalThis.window, now = () => Date.now() } = {}) {
  let stamp = '', generation = -1, framer = null
  let activeSave = null
  const send = value => queueMicrotask(()=>app.ports.sessionBridgeEvent.send(value))
  let heartbeatKey = ''
  async function retireSave(owned = activeSave) {
    if (!owned) return
    owned.retired = true
    try {
      if(owned.writer) await owned.writer.abort()
      else if(owned.native) await owned.native.abort()
      if(!owned.picking && !owned.acquiring && activeSave === owned) activeSave = null
    } catch { send({kind:'save-error',stamp:owned.stamp,operation:owned.operation,code:'writer_cleanup_failed'}) }
  }
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
      await retireSave(owned)
      send({kind:'save-error',stamp:owned.stamp,operation:owned.operation,code:'picker_cancelled_or_failed'})
    })
  })
  queueMicrotask(()=>send({kind:'save-capability',supported:typeof window.showSaveFilePicker === 'function'}))
  app.ports.sessionBridgeCommand.subscribe(command => {
    if (command.stamp !== stamp) { stamp = command.stamp; framer = null; generation = -1; void retireSave() }
    if(activeSave && activeSave.operation < command.nextOperation && command.saveOperation !== activeSave.operation) void retireSave()
    if(command.saveChunk && activeSave && activeSave.operation === command.saveOperation && activeSave.writer) {
      const owned = activeSave
      owned.writer.write(command.saveChunk).then(result => {
        if(activeSave !== owned || owned.retired || stamp !== owned.stamp) return
        if(result.complete) activeSave = null
        send({kind:result.complete?'save-complete':'save-progress',stamp:owned.stamp,operation:owned.operation,offset:result.offset})
      }).catch(async () => {
        await retireSave(owned)
        send({kind:'save-error',stamp:owned.stamp,operation:owned.operation,code:'write_or_verification_failed'})
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
