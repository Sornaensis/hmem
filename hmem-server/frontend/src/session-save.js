import { createRecordFramer, decodeBase64 } from './session-viewer.js'
import { sha256 } from '@noble/hashes/sha256'
import { validateManifest } from './session-archive-worker.js'
import {acquireArchiveCustody,deleteArchiveStage} from './session-archive-store.js'

const hex = bytes => Array.from(bytes,byte => byte.toString(16).padStart(2,'0')).join('')

export function createSealedViewer({send,fetchArchive,document,window,workerFactory=()=>new Worker(new URL('./session-archive-worker.js',import.meta.url),{type:'module'}),cleanupMs=10000,custodyFactory=acquireArchiveCustody,deleteStage=deleteArchiveStage}) {
  let active=null,pending=null,sequence=0,authority=0,blocked=false,stopped='',authorityFence='',blockedNotice='',domEpoch=''
  const clear=owned=>{
    if(!owned || active!==owned || owned.domCleared)return
    clearPageProgress(owned)
    owned.domCleared=true
    const candidates=[document.getElementById?.('session-records'),...(document.querySelectorAll?.('.session-inspected-event') || [])]
    const nodes=candidates.filter(node=>node?.getAttribute?.('data-sealed-dom-owner')===owned.config.stamp && node.getAttribute('data-sealed-dom-load')===String(owned.config.load))
    domEpoch=window.crypto.randomUUID()
    // Capture the old hosts before Elm can replace them. The stamped clear
    // retires model evidence and keys the replacement of their emptied trees.
    event(owned,'clear')
    for(const node of nodes)node.replaceChildren()
  }
  const identity=config=>JSON.stringify([config.stamp,config.load,config.authority,authority])
  const event=(owned,kind,extra={})=>send({...extra,kind:'sealed-'+kind,stamp:owned.config.stamp,generation:owned.config.load,load:owned.config.load,request:extra.request ?? owned.lifecycleRequest,
    workspace:owned.config.workspace,session:owned.config.session,archiveGeneration:owned.manifest?.generation_id || '',authEpoch:owned.authority,domEpoch})
  const current=owned=>active===owned&&!owned.retired&&owned.authority===authority
  function clearPageProgress(owned,operation) {
    const pending=owned?.pageProgress
    if(!pending || (operation && pending!==operation))return
    owned.pageProgress=null
    if(pending.timer!==null)window.clearTimeout(pending.timer)
  }
  function cleanupFailed(owned) {
    blocked=true;pending=null;stopped=owned.identity;clear(owned)
    blockedNotice=owned.identity
    if(!owned.cleanupReported){owned.cleanupReported=true;event(owned,'error',{code:'archive_cleanup_failed'})}
  }
  function rpc(owned,kind,extra={}) {
    const request=++sequence
    return new Promise((resolve,reject)=>{
      const timer=window.setTimeout(()=>{owned.waiters.delete(request);reject(Error(kind==='retire'?'archive_cleanup_failed':'archive_worker_timeout'))},kind==='finish'?owned.deadline:30000)
      owned.waiters.set(request,{resolve,reject,timer})
      owned.worker.postMessage({kind,stamp:owned.workerStamp,request,...extra},extra.bytes?[extra.bytes]:[])
    })
  }
  async function retire(owned) {
    if(!owned || owned.cleaning)return owned?.cleaning
    clearPageProgress(owned)
    owned.retired=true;owned.localIntent=null;owned.controller?.abort()
    if(owned.markdownWaiter){window.clearTimeout(owned.markdownWaiter.timer);owned.markdownWaiter.reject(Error('retired_archive'));owned.markdownWaiter=null}
    // Interrupt the current decode before awaiting the serialized cleanup.
    if(owned.started)owned.worker.postMessage({kind:'interrupt',stamp:owned.workerStamp,request:++sequence})
    window.clearTimeout(owned.timer)
    owned.actualCleaning=(async()=>{
      try {
        await owned.transport?.catch(()=>{})
        if(owned.transportCleanupFailed)throw Error('archive_cleanup_failed')
        await owned.phaseTransition?.catch(()=>{})
        if(owned.started&&owned.worker)await rpc(owned,'retire')
        terminateWorker(owned)
        if(owned.stageName&&owned.custody)await deleteStage(owned.stageName,owned.custody)
        if(owned.releaseCustody){await owned.releaseCustody();owned.releaseCustody=null}
        for(const waiter of owned.waiters.values()){window.clearTimeout(waiter.timer);waiter.reject(Error('retired_archive'))}
        owned.waiters.clear()
        if(active===owned)active=null
        if(pending && !blocked){const latest=pending;pending=null;start(latest)}
      } catch {
        cleanupFailed(owned)
      }
    })()
    let timer
    const deadline=new Promise(resolve=>{timer=window.setTimeout(()=>{cleanupFailed(owned);resolve()},cleanupMs)})
    // A finite failure disposition does not cancel or release the real join.
    // The captured owner retains every handle until actualCleaning completes.
    owned.cleaning=Promise.race([owned.actualCleaning,deadline]).finally(()=>window.clearTimeout(timer))
    return owned.cleaning
  }
  function failure(owned,error) {
    if(!current(owned))return
    clearPageProgress(owned)
    stopped=owned.identity;clear(owned)
    event(owned,'error',{code:error.name==='QuotaExceededError'?'archive_quota':error.message})
    void retire(owned)
  }
  function demand(owned,path,kind) {
    if(!current(owned))return
    owned.demand={sequence:++sequence,path,kind}
    event(owned,'demand',owned.demand)
  }
  function constructWorker(owned,mode) {
    const incarnation=window.crypto.randomUUID(),worker=workerFactory({incarnation,mode})
    owned.worker=worker;owned.incarnation=incarnation;owned.mode=mode
    if(owned.workerStamp)owned.workerStamp={...owned.workerStamp,incarnation}
    worker.onerror=()=>{if(owned.worker===worker&&owned.incarnation===incarnation)failure(owned,Error('archive_worker_failed'))}
    worker.onmessage=({data})=>{
      if(owned.worker!==worker||owned.incarnation!==incarnation||!owned.workerStamp||data.incarnation!==incarnation
        ||data.workspace!==owned.workerStamp.workspace||data.session!==owned.workerStamp.session||data.generation!==owned.workerStamp.generation||data.load!==owned.workerStamp.load||data.authEpoch!==owned.workerStamp.authEpoch)return
      if(data.kind==='progress'){if(current(owned))event(owned,'progress',data.phase==='ready'?{phase:'index-ready'}:data);return}
      const waiter=owned.waiters.get(data.request);if(!waiter)return
      owned.waiters.delete(data.request);window.clearTimeout(waiter.timer)
      if(data.kind==='error')waiter.reject(Error(data.code));else waiter.resolve(data)
    }
  }
  function terminateWorker(owned) {
    const worker=owned.worker
    if(worker){worker.onmessage=null;worker.onerror=null;worker.terminate();owned.worker=null}
    owned.started=false
  }
  async function detachProcessor(owned) {
    await rpc(owned,'finish');if(!current(owned))return
    const response=await rpc(owned,'detach')
    if(response.stage?.name!==owned.stageName||response.stage?.custody!==owned.custody||JSON.stringify(response.stage.manifest)!==JSON.stringify(owned.manifest))throw Error('archive_receipt_mismatch')
    owned.detached=true;terminateWorker(owned)
    if(current(owned))demand(owned,owned.config.apiUrl+owned.manifest.content_path,'revalidate')
  }
  async function adoptReader(owned) {
    if(!current(owned)||!owned.detached)throw Error('retired_archive')
    constructWorker(owned,'reader');owned.started=true
    await rpc(owned,'adopt',{name:owned.stageName,custody:owned.custody,manifest:owned.manifest})
    if(current(owned))await page(owned)
  }

  function start(config) {
    const owned={config,authority,identity:identity(config),lifecycleRequest:++sequence,worker:null,waiters:new Map(),retired:false,started:false,manifest:null,page:{},deadline:125000}
    active=owned
    event(owned,'progress',{phase:'preparing',rawBytes:'0',compressedBytes:'0'})
    try {
      constructWorker(owned,'processor')
      demand(owned,config.apiUrl+`/api/v1/workspaces/${encodeURIComponent(config.workspace)}/sessions/${encodeURIComponent(config.session)}/archive`,'manifest')
    }catch(error){failure(owned,error)}
  }
  function drainLocalIntent(owned) {
    if(!current(owned) || owned.pagePending || owned.markdownBusy)return
    const intent=owned.localIntent;owned.localIntent=null
    if(intent?.kind==='markdown'){
      if(intent.page){owned.localIntent={kind:'markdown',key:intent.key};void page(owned,{...intent.page,bodyKey:intent.key},intent.key)}
      else void readMarkdown(owned,intent.key)
    }
    else if(intent)void page(owned,intent.options)
  }
  async function page(owned,options={},visibleKey) {
    if(!current(owned))return
    if(owned.pagePending || owned.markdownBusy){owned.localIntent={kind:'page',options:{...options}};return}
    owned.pagePending=true
    const operation={timer:null};owned.pageProgress=operation
    operation.timer=window.setTimeout(()=>{
      operation.timer=null
      if(current(owned) && owned.pagePending && owned.pageProgress===operation)event(owned,'progress',{phase:'navigating'})
    },100)
    try {
      const result=await rpc(owned,'page',{page:options})
      clearPageProgress(owned,operation)
      if(current(owned) && visibleKey && !result.rows.some(row=>row.key===visibleKey))throw Error('archive_piece_missing')
      if(current(owned)){owned.page=options;owned.first=result.first;owned.last=result.last;owned.searchAfter=result.searchAfter;owned.searchCursor=result.searchCursor;event(owned,'page',{...result,kind:undefined,fromEnd:!!options.last||!!options.before})}
    }catch(error){clearPageProgress(owned,operation);failure(owned,error)}finally{clearPageProgress(owned,operation);owned.pagePending=false;drainLocalIntent(owned)}
  }
  async function smallJson(response,owned) {
    if(!response.body)throw Error('archive_response_failed')
    const reader=response.body.getReader(),parts=[];let count=0
    let primary
    try{while(true){const part=await reader.read();if(part.done)break;count+=part.value.length;if(count>65536)throw Error('manifest_limit');parts.push(part.value)}
      const bytes=new Uint8Array(count);let at=0;for(const part of parts){bytes.set(part,at);at+=part.length}return JSON.parse(new TextDecoder('utf-8',{fatal:true}).decode(bytes))
    }catch(error){primary=error;throw error}finally{await closeReader(reader,primary,owned)}
  }
  async function closeReader(reader,primary,owned) {
    if(!reader)return
    const timer=window.setTimeout(()=>{cleanupFailed(owned);void retire(owned)},cleanupMs)
    try {
      await reader.cancel(primary).catch(error=>{if(error!==primary)throw error})
      await reader.closed.catch(error=>{if(error!==primary)throw error})
      reader.releaseLock()
    }catch(error){throw new AggregateError([primary,error].filter(Boolean),'archive_cleanup_failed')}
    finally{window.clearTimeout(timer)}
  }
  function receiveHttp(request) {
    const owned=active
    if(!owned || !current(owned) || owned.demand?.sequence!==request.sequence){send({...request,kind:'sealed-http',request:request.id,sequence:request.sequence,status:0});return}
    if(owned.http===request.id)return
    owned.http=request.id;owned.controller=new AbortController()
    const transportDemand=owned.demand
    owned.transport=(async()=>{
      let response,reader,status=0,next,primary
      try {
        const transport=await fetchArchive(transportDemand.path,{signal:owned.controller.signal,...(transportDemand.kind==='revalidate'?{headers:{'If-None-Match':owned.etag}}:{})})
        response=transport.response;status=response.status
        if(!current(owned)||!transport.isCurrent())throw Error('retired_archive')
        if(transportDemand.kind==='manifest') {
          const value=await smallJson(response,owned)
          if(!current(owned)||!transport.isCurrent())throw Error('retired_archive')
          if(status===202 && ['preparing','retrying'].includes(value.state)) {
            if(value.workspace_id!==owned.config.workspace || value.session_id!==owned.config.session)throw Error('invalid_manifest')
            event(owned,'progress',{phase:value.state,rawBytes:value.progress?.raw_bytes || '0',compressedBytes:value.progress?.compressed_bytes || '0'})
            const delay=Math.min(60000,Math.max(500,Number(value.retry_after_ms)||1000))
            next=()=>{owned.timer=window.setTimeout(()=>demandManifest(owned),delay)}
          } else {
            if(status!==200)throw Error(status===401||status===403?'archive_unauthorized':value.error || 'archive_unavailable')
            owned.manifest=validateManifest(value,owned.config.workspace,owned.config.session)
            owned.workerStamp={workspace:owned.config.workspace,session:owned.config.session,generation:value.generation_id,load:owned.config.load,authEpoch:owned.authority,incarnation:owned.incarnation}
            owned.deadline=Number((BigInt(value.compressed_bytes)*2000n/12500000n)+120000n*((BigInt(value.raw_bytes)+1073741823n)/1073741824n || 1n)+5000n)
            owned.stageName=window.crypto.randomUUID();owned.custody=window.crypto.randomUUID()
            owned.custodyAcquisition=custodyFactory(owned.stageName)
            owned.releaseCustody=await owned.custodyAcquisition
            if(!current(owned)||!transport.isCurrent())throw Error('retired_archive')
            owned.started=true
            await rpc(owned,'start',{name:owned.stageName,custody:owned.custody,manifest:value})
            next=()=>demand(owned,owned.config.apiUrl+value.content_path,'content')
          }
        } else if(transportDemand.kind==='revalidate') {
          if(status!==304||response.headers.get('ETag')!==owned.etag||!owned.detached)throw Error(status===401||status===403?'archive_unauthorized':'archive_revalidation_failed')
          if(!current(owned)||!transport.isCurrent())throw Error('retired_archive')
          next=()=>{owned.phaseTransition=adoptReader(owned).catch(error=>failure(owned,error))}
        } else {
          const value=owned.manifest,headers=response.headers
          owned.etag=headers.get('ETag')
          if(!owned.etag||!/^"[^"\r\n]*"$/.test(owned.etag))throw Error('archive_response_mismatch')
          reader=response.body?.getReader()
          if(status!==200 || !response.body || !/^application\/gzip(?:;|$)/i.test(headers.get('Content-Type')||'') || headers.get('Content-Encoding')
            || headers.get('Content-Length')!==value.compressed_bytes || headers.get('X-HMem-Archive-Generation')!==value.generation_id
            || headers.get('X-HMem-Archive-SHA256')!==value.compressed_sha256 || headers.get('X-HMem-Archive-Raw-Bytes')!==value.raw_bytes
            || headers.get('X-HMem-Archive-Raw-SHA256')!==value.raw_sha256)throw Error(status===401||status===403?'archive_unauthorized':'archive_response_mismatch')
          while(true){if(!current(owned)||!transport.isCurrent())throw Error('retired_archive');const part=await reader.read();if(part.done)break
            for(let at=0;at<part.value.length;at+=65536){if(!current(owned)||!transport.isCurrent())throw Error('retired_archive');const bytes=part.value.slice(at,at+65536);await rpc(owned,'chunk',{bytes:bytes.buffer})}}
          if(!current(owned)||!transport.isCurrent())throw Error('retired_archive')
          next=()=>{owned.phaseTransition=detachProcessor(owned).catch(error=>failure(owned,error))}
        }
      } catch(error){primary=error;status=status>=400?status:0;if(error.message==='archive_cleanup_failed')owned.transportCleanupFailed=true;if(current(owned)){stopped=owned.identity;clear(owned);event(owned,'error',{code:error.message});queueMicrotask(()=>void retire(owned))}}
      finally {
        if(!reader && response?.body && !response.body.locked)reader=response.body.getReader()
        try{await closeReader(reader,primary,owned)}catch{owned.transportCleanupFailed=true;cleanupFailed(owned)}
        // Exact transport completion owns physical release, including stale work.
        if(!owned.transportCleanupFailed)event(owned,'http',{request:request.id,sequence:request.sequence,status:status>=200&&status<300?200:status})
        if(current(owned)&&next)queueMicrotask(next)
      }
    })()
  }
  const demandManifest=owned=>demand(owned,owned.config.apiUrl+`/api/v1/workspaces/${encodeURIComponent(owned.config.workspace)}/sessions/${encodeURIComponent(owned.config.session)}/archive`,'manifest')
  async function readMarkdown(owned,key) {
    if(!current(owned))return
    if(owned.pagePending||owned.markdownBusy){owned.localIntent={kind:'markdown',key,page:{...owned.page}};return}
    owned.markdownBusy=true
    event(owned,'markdown-start',{key})
    let bytes=0
    try {
      for(let part=0;;part++){
        if(!current(owned))throw Error('retired_archive')
        const result=await rpc(owned,'body',{key,part}),body=result.body
        if(!current(owned))throw Error('retired_archive')
        if(result.key!==key || body.part!==part || body.format!=='markdown')throw Error('archive_body_mismatch')
        const size=new TextEncoder().encode(body.text).length;bytes+=size
        if(size>65536 || bytes>1048576)throw Error('archive_body_limit')
        const request=++sequence
        const acknowledged=new Promise((resolve,reject)=>{
          const timer=window.setTimeout(()=>{owned.markdownWaiter=null;reject(Error('archive_body_ack_timeout'))},10000)
          owned.markdownWaiter={request,resolve,reject,timer}
        })
        event(owned,'markdown-chunk',{request,key,part,text:body.text,structured:!!body.structured,complete:!body.more})
        await acknowledged
        if(!body.more)break
      }
    }catch(error){failure(owned,error)}finally{owned.markdownBusy=false;drainLocalIntent(owned)}
  }
  document.addEventListener('click',domEvent=>{
    const button=domEvent.target?.closest?.('[data-sealed-action]'),owned=active
    if(!button || !owned || !current(owned) || button.getAttribute('data-sealed-owner')!==owned.config.stamp)return
    const action=button.getAttribute('data-sealed-action');domEvent.preventDefault()
    if(action==='cancel'){stopped=owned.identity;pending=null;clear(owned);event(owned,'cancelled');void retire(owned)}
    else if(action==='first')void page(owned)
    else if(action==='last')void page(owned,{last:true})
    else if(action==='next')void page(owned,{after:owned.last})
    else if(action==='search-next')void page(owned,{query:owned.page.query,after:owned.searchAfter,searchCursor:owned.searchCursor})
    else if(action==='previous')void page(owned,{before:owned.first})
    else if(action==='body')void page(owned,{...owned.page,bodyKey:button.getAttribute('data-sealed-key'),bodyPart:Number(button.getAttribute('data-sealed-part'))})
    else if(action==='inspect'){const key=button.getAttribute('data-sealed-key');event(owned,'inspect',{key});void page(owned,{...owned.page,bodyKey:key})}
    else if(action==='render')event(owned,'render',{part:Number(button.getAttribute('data-sealed-part')),inspector:button.getAttribute('data-sealed-inspector')==='true'})
    else if(action==='markdown')void readMarkdown(owned,button.getAttribute('data-sealed-key'))
    else if(action==='search'){const input=document.getElementById('sealed-search');void page(owned,{query:input?.value || ''})}
    else if(action==='position'){const input=document.getElementById('sealed-position');void page(owned,{offset:input?.value || '0'})}
  })
  document.addEventListener('toggle',domEvent=>{
    const node=domEvent.target,owned=active,key=node.getAttribute?.('data-sealed-key')
    if(!key || !owned || !current(owned) || node.getAttribute('data-sealed-owner')!==owned.config.stamp)return
    event(owned,'disclosure',{key,open:node.open})
    void rpc(owned,'disclosure',{key,open:node.open}).then(()=>{if(node.open && !key.startsWith('group:'))return page(owned,{...owned.page,bodyKey:key})}).catch(error=>failure(owned,error))
  },true)
  window.addEventListener?.('pagehide',()=>{pending=null;clear(active);void retire(active)})
  return {
    configure(config,request,markdownAck) {
      const bodyOwner=active
      if(bodyOwner?.markdownWaiter && current(bodyOwner) && markdownAck?.stamp===bodyOwner.config.stamp && markdownAck.load===bodyOwner.config.load && markdownAck.request===bodyOwner.markdownWaiter.request){
        const waiter=bodyOwner.markdownWaiter;bodyOwner.markdownWaiter=null;window.clearTimeout(waiter.timer);waiter.resolve()
      }
      if(active && config?.active && config.authority!==active.config.authority){authority++;clear(active)}
      if(blocked && config?.active && blockedNotice!==identity(config)){
        blockedNotice=identity(config)
        send({kind:'sealed-error',stamp:config.stamp,generation:config.load,load:config.load,request:++sequence,workspace:config.workspace,session:config.session,archiveGeneration:'',authEpoch:authority,code:'archive_cleanup_failed'})
      }
      if(!config || !config.active){pending=null;stopped='';if(active){clear(active);void retire(active)}}
      else if(!blocked && config.stamp!==authorityFence && identity(config)!==stopped && (!active || active.identity!==identity(config))){pending=config;if(active){clear(active);void retire(active)}else{pending=null;start(config)}}
      if(request)receiveHttp(request)
    },
    retireAuthority(){authorityFence=active?.config.stamp || '';authority++;pending=null;if(active){clear(active);event(active,'cancelled');void retire(active)}},
    snapshot:()=>({active:!!active,pending:!!pending,blocked})
  }
}

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
export function installSessionBridge(app, { document = globalThis.document, window = globalThis.window, now = () => Date.now(), fetchArchive = async()=>{throw Error('archive_transport_unavailable')}, workerFactory } = {}) {
  let stamp = '', viewerStamp = '', generation = -1, framer = null, viewerIntent = 0
  let activeSave = null
  const send = value => queueMicrotask(()=>app.ports.sessionBridgeEvent.send(value))
  const sealed=createSealedViewer({send,fetchArchive,document,window,workerFactory})
  let heartbeatKey = ''
  let viewerScope = '', presentation = null
  const disclosureKey = node => {
    const group = node.getAttribute?.('data-group-key')
    if(group!==null && group!==undefined)return 'group:'+group
    const event=node.getAttribute?.('data-event-key')
    return event!==null && event!==undefined ? 'event:'+event : null
  }
  const rememberPresentation = event => {
    const viewport=document.getElementById?.('session-records')
    if(!viewport?.querySelectorAll || !viewerScope || viewport.getAttribute?.('data-viewer-owner')!==viewerStamp)return
    const returning=event.type==='click' && (event.target?.closest?.('.task-session-transcript a[href]') || event.target?.closest?.('button.tab')?.textContent?.trim()==='Projects')
    if(!returning && !(event.type==='scroll' && event.target===viewport) && !(event.type==='toggle' && viewport.contains?.(event.target)))return
    presentation={scope:viewerScope,top:viewport.scrollTop,disclosures:[...viewport.querySelectorAll('details')].map(node=>[disclosureKey(node),node.open])}
  }
  // One bounded snapshot holds only position/disclosure flags, never payloads.
  // Capture before route handlers replace the task/full-session DOM.
  for(const event of ['click','scroll','toggle'])document.addEventListener(event,rememberPresentation,true)
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
    sealed.configure(command.sealed,command.sealedRequest,command.sealedMarkdownAck)
    const downloadStamp = command.downloadStamp ?? command.stamp
    if (downloadStamp !== stamp) { stamp = downloadStamp; void retireSave() }
    if (command.stamp !== viewerStamp) {
      const continuing=command.continueViewer===true && command.viewerScope===viewerScope
      viewerStamp = command.stamp
      if(!continuing){framer=null;presentation=null}
      generation = -1
      if(continuing && presentation?.scope===viewerScope){
        const saved=presentation,owner=viewerStamp,scope=viewerScope,intent=viewerIntent,revision=command.generation
        const queuedViewport=document.getElementById?.('session-records')
        const queuedTop=queuedViewport?.getAttribute?.('data-viewer-owner')===owner?queuedViewport.scrollTop:0
        requestAnimationFrame(()=>{
          if(owner!==viewerStamp || scope!==viewerScope || generation!==revision || viewerIntent!==intent)return
          const viewport=document.getElementById?.('session-records')
          if(!viewport?.querySelectorAll || viewport.getAttribute?.('data-viewer-owner')!==owner || Math.abs(viewport.scrollTop-queuedTop)>4)return
          const states=new Map(saved.disclosures)
          for(const node of viewport.querySelectorAll('details')){
            const key=disclosureKey(node)
            if(states.has(key))node.open=states.get(key)
          }
          viewport.scrollTop=saved.top
        })
      }
    }
    viewerScope=command.viewerScope ?? ''
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
  return {retireAuthority:()=>sealed.retireAuthority()}
}
