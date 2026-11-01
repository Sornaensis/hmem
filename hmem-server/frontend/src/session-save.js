import { createRecordFramer, decodeBase64 } from './session-viewer.js'
import { sha256 } from '@noble/hashes/sha256'
import { validateManifest } from './session-archive-worker.js'
import {acquireArchiveCustody,deleteArchiveStage} from './session-archive-store.js'
import {installEventPayload} from './session-event-json.js'

const hex = bytes => Array.from(bytes,byte => byte.toString(16).padStart(2,'0')).join('')

// Sealed cursors stay exact even when the packed rank exceeds Number's range.
export const indexedCursorRank=key=>{const [offset,index,sub]=String(key).split(':');return BigInt(offset||'0')*10000000n+BigInt(index||'0')+(sub?1n:0n)}
export const indexedCursorKey=value=>(value/10000000n).toString().padStart(19,'0')+':'+(value%10000000n).toString().padStart(7,'0')

function visibleConversationNode(node,viewport) {
  for(let parent=node?.parentElement;parent&&parent!==viewport;parent=parent.parentElement){
    if(parent.tagName==='DETAILS'&&!parent.open){
      const summary=[...parent.children].find(child=>child.tagName==='SUMMARY')
      if(!summary?.contains(node))return false
    }
  }
  return (node?.getBoundingClientRect?.().height || 0)>0
}

const presentationGapContains=(node,part)=>Number(node.dataset.fragmentFrom)<=part&&Number(node.dataset.fragmentTo)>=part
function presentationFrontier(viewport,part,direction) {
  const admitted=[...(viewport.querySelectorAll?.('[data-fragment-part]') || [])].filter(node=>!node.querySelector?.('[data-history-gap],[data-body-gap],[data-sealed-body-needed]')).map(node=>Number(node.dataset.fragmentPart)).filter(Number.isSafeInteger)
  return direction<0?Math.max(0,Math.min(part,...admitted)-1):Math.max(part,...admitted)+1
}

function visibleRenderAnchors(viewport,part) {
  const box=viewport?.getBoundingClientRect?.(),parts=new Set(),doc=viewport?.ownerDocument,selection=doc?.getSelection?.(),selected=selection&&!selection.isCollapsed?selection.anchorNode:null
  if(!box)return []
  const visible=node=>{const r=node.getBoundingClientRect();let top=box.top,bottom=box.bottom,left=box.left??-Infinity,right=box.right??Infinity
    for(let parent=node.parentElement,depth=0;parent&&parent!==viewport&&depth<64;parent=parent.parentElement,depth++){const style=doc?.defaultView?.getComputedStyle(parent);if(style&&/auto|scroll|hidden|clip/.test(style.overflowX+' '+style.overflowY)){const clip=parent.getBoundingClientRect();top=Math.max(top,clip.top);bottom=Math.min(bottom,clip.bottom);left=Math.max(left,clip.left);right=Math.min(right,clip.right)}}
    return r.bottom>top&&r.top<bottom&&(r.right??Infinity)>left&&(r.left??-Infinity)<right}
  return [...(viewport.querySelectorAll?.('[data-fragment-path]') || [])].filter(node=>!node.querySelector?.('[data-history-gap],[data-body-gap],[data-sealed-body-needed]')&&visibleConversationNode(node,viewport)&&visible(node)).sort((a,b)=>Number(!!selected&&b.contains?.(selected))-Number(!!selected&&a.contains?.(selected))||Math.abs(Number(a.dataset.fragmentPart??part)-part)-Math.abs(Number(b.dataset.fragmentPart??part)-part)).filter(node=>{const key=node.dataset.fragmentPart??'current';if(parts.has(key))return false;parts.add(key);return true}).slice(0,3).map(node=>({node,part:Number(node.dataset.fragmentPart??part),selected:!!selected&&!!node.contains?.(selected),top:node.getBoundingClientRect().top-box.top}))
}

function protectedRenderPoint(viewport,anchors,current,target) {
  if(anchors.some(anchor=>anchor.selected&&(anchor.part<Math.min(current,target)||anchor.part>Math.max(current,target))))return true
  // An adaptive single-part window cannot evict its visible center merely
  // to prefetch. Once the user scrolls beyond that text it may continue.
  return anchors.some(anchor=>anchor.part===current)&&![...(viewport.querySelectorAll?.('[data-fragment-part]') || [])].some(node=>Number(node.dataset.fragmentPart)===target)
}

function settledRenderPosition(viewport,receipt,scrollIntent) {
  const owned=scrollIntent.ownedPosition?.(viewport)
  if(owned&&owned.epoch!==receipt.epoch){receipt.epoch=owned.epoch;receipt.top=owned.top}
  return ownedPositionUnchanged(viewport,receipt.top)
}

function ownedPositionUnchanged(viewport,previous) {
  if(Math.abs(viewport.scrollTop-previous)<=4)return true
  const maximum=viewport.scrollHeight-viewport.clientHeight
  return Number.isFinite(maximum)&&Math.abs(viewport.scrollTop-Math.max(0,Math.min(previous,maximum)))<=4
}

function payloadTextRange(node,offset) {
  const doc=node.ownerDocument,walker=doc.createTreeWalker(node,4)
  let text=walker.nextNode(),remaining=offset
  while(text&&remaining>text.length){remaining-=text.length;text=walker.nextNode()}
  if(!text)return null
  const range=doc.createRange(),at=Math.min(remaining,text.length)
  range.setStart(text,at);range.setEnd(text,Math.min(text.length,at+1))
  return range
}

function visibleTextPoint(viewport,leaf) {
  const doc=leaf?.ownerDocument,box=viewport?.getBoundingClientRect?.()
  if(!doc?.createTreeWalker||!box||!leaf.textContent.length)return null
  let top=box.top,bottom=box.bottom,left=box.left,right=box.right
  for(let parent=leaf.parentElement,depth=0;parent&&parent!==viewport&&depth<64;parent=parent.parentElement,depth++){const style=doc.defaultView.getComputedStyle(parent);if(/auto|scroll|hidden|clip/.test(style.overflowX+' '+style.overflowY)){const r=parent.getBoundingClientRect();top=Math.max(top,r.top);bottom=Math.min(bottom,r.bottom);left=Math.max(left,r.left);right=Math.min(right,r.right)}}
  let first=0,last=leaf.textContent.length
  while(first<last){const middle=Math.floor((first+last)/2),r=payloadTextRange(leaf,middle)?.getBoundingClientRect();if(r&&r.bottom>top+1)last=middle;else first=middle+1}
  const r=payloadTextRange(leaf,first)?.getBoundingClientRect(),pre=leaf.closest('pre')
  return r?.height&&r.top<bottom&&r.right>left&&r.left<right?{path:leaf.dataset.fragmentPath,offset:first,top:r.top-box.top,innerScroll:pre?.scrollTop??null}:null
}

function visiblePayloadPosition(viewport,leaf) {
  const box=viewport.getBoundingClientRect(),doc=viewport.ownerDocument
  if(!doc||!leaf.querySelectorAll)return null
  for(const span of leaf.querySelectorAll('[data-payload-start]')){
    const r=span.getBoundingClientRect();if(!r.height||r.bottom<=box.top||r.top>=box.bottom)continue
    let top=box.top,bottom=box.bottom,parent=span.parentElement
    for(let depth=0;parent&&parent!==viewport&&depth<64;depth++,parent=parent.parentElement){const style=doc.defaultView.getComputedStyle(parent);if(/auto|scroll|hidden|clip/.test(style.overflowY)){const clip=parent.getBoundingClientRect();top=Math.max(top,clip.top+parent.clientTop);bottom=Math.min(bottom,clip.top+parent.clientTop+parent.clientHeight)}}
    if(bottom<=top)continue
    let first=0,last=span.textContent.length
    while(first<last){const middle=Math.floor((first+last)/2),rect=payloadTextRange(span,middle)?.getBoundingClientRect();if(rect&&rect.bottom>top+1)last=middle;else first=middle+1}
    const range=payloadTextRange(span,first),bounds=range?.getBoundingClientRect()
    if(bounds?.height&&bounds.top<bottom){const pre=span.closest('pre'),inner=pre&&/auto|scroll/.test(doc.defaultView.getComputedStyle(pre).overflowY)&&pre.scrollHeight>pre.clientHeight+1;return {offset:Number(span.dataset.payloadStart)+first,top:bounds.top-box.top,innerTop:inner?bounds.top-pre.getBoundingClientRect().top:null}}
  }
  return null
}

function payloadChunkStart(body,offset) {
  let start=0
  for(const token of body.tokens?.length?body.tokens:[{text:body.text}]){
    for(let at=0;at<token.text.length;){let length=Math.min(8192,token.text.length-at);const last=token.text.charCodeAt(at+length-1),next=token.text.charCodeAt(at+length);if(last>=0xd800&&last<=0xdbff&&next>=0xdc00&&next<=0xdfff)length--;if(offset<start+length)return start;start+=length;at+=length}
  }
  return Math.max(0,start-1)
}

// Input is only a candidate. Layout, publication and native anchoring can also
// emit scroll events, so only actual movement after scoped input arms history.
const readerScopeKey=value=>{try{const parsed=JSON.parse(value);return JSON.stringify(Array.isArray(parsed)&&parsed.length===5?parsed.slice(0,4):parsed)}catch{return value || ''}}

export function createReaderScrollIntent({now=()=>Date.now()}={}) {
  let scope='',manual=false,input=null,touchY=null,returning=false,positions=new WeakMap(),ownedPositions=new WeakMap(),positionEpoch=0,lastViewport=null,movement=0
  const windowKey=viewport=>['data-viewer-owner','data-viewer-generation','data-sealed-dom-load','data-window-publication','data-sealed-page-request','data-render-part','data-render-request'].map(name=>viewport.getAttribute?.(name)??'').join('|')
  return {
    configure(value){const next=readerScopeKey(value);if(next!==scope){scope=next;manual=false;input=null;touchY=null;returning=false;positions=new WeakMap();ownedPositions=new WeakMap();positionEpoch=0;lastViewport=null;movement=0}},
    note(event,viewport){
      if(!viewport || event.isTrusted===false || (event.target!==viewport&&!viewport.contains?.(event.target)))return
      if(!['wheel','keydown','touchstart','touchmove','pointerdown','click'].includes(event.type))return
      input=null
      if(event.target!==viewport&&event.target?.closest?.('button,input,textarea,select,a,summary'))return
      if(event.type==='wheel'&&(event.ctrlKey||event.metaKey||event.shiftKey||Math.abs(event.deltaX||0)>Math.abs(event.deltaY||0)))return
      if(event.type?.startsWith('touch')&&event.touches?.length!==1){touchY=null;return}
      let direction=0
      if(event.type==='wheel')direction=Math.sign(event.deltaY || 0)
      else if(event.type==='keydown')direction=['ArrowUp','PageUp','Home'].includes(event.key)?-1:['ArrowDown','PageDown','End',' '].includes(event.key)?1:0
      else if(event.type==='touchstart'){touchY=event.touches?.[0]?.clientY;return}
      else if(event.type==='touchmove'){const y=event.touches?.[0]?.clientY;if(Number.isFinite(y)&&Number.isFinite(touchY))direction=Math.sign(touchY-y);touchY=y}
      else if(event.type==='pointerdown'){
        const box=viewport.getBoundingClientRect?.(),gutter=Math.max(12,(viewport.offsetWidth || 0)-viewport.clientWidth)
        if(!box || event.clientX<box.right-gutter)return
        direction=0
      }else return
      // Composited wheel/touch scrolling may change scrollTop before dispatching
      // its input event. The last inspected/owned position is the causal start.
      if(direction!==0 || event.type==='pointerdown'){
        const candidate={viewport,top:event.type==='keydown'?viewport.scrollTop:positions.get(viewport) ?? viewport.scrollTop,direction,event,at:now(),window:windowKey(viewport),edge:!event.target?.closest?.('pre,session-event-json')&&(event.type==='wheel'||event.type==='touchmove'||event.type==='keydown'&&event.target===viewport),dispatched:false,consumed:false}
        input=candidate
        queueMicrotask(()=>{candidate.dispatched=true})
      }
    },
    inspect(viewport,logicalEnd){
      if(input?.viewport===viewport){
        const moved=viewport.scrollTop-input.top
        if(moved<-.5&&input.direction<=0){manual=true;returning=false;movement=-1}
        if(moved>.5&&input.direction>=0){returning=true;movement=1}
        if(Math.abs(moved)>.5)input=null
      }
      if(returning&&logicalEnd&&viewport.scrollHeight-viewport.scrollTop-viewport.clientHeight<=4){manual=false;returning=false}
      positions.set(viewport,viewport.scrollTop);lastViewport=viewport
      return manual
    },
    positioned(){input=null;if(lastViewport){positions.set(lastViewport,lastViewport.scrollTop);ownedPositions.set(lastViewport,{epoch:++positionEpoch,top:lastViewport.scrollTop})}},
    clear(){manual=false;input=null;returning=false;movement=0;if(lastViewport){positions.set(lastViewport,lastViewport.scrollTop);ownedPositions.set(lastViewport,{epoch:++positionEpoch,top:lastViewport.scrollTop})}},
    ownedPosition:viewport=>ownedPositions.get(viewport),
    pendingHome:viewport=>input?.viewport===viewport&&input.event.type==='keydown'&&input.event.key==='Home'&&input.event.target===viewport&&input.window===windowKey(viewport),
    consumeEdge(viewport,direction){
      if(!manual||!input||input.viewport!==viewport||!input.edge||!input.dispatched||input.consumed||input.direction!==direction||input.event.defaultPrevented||now()-input.at>500||input.window!==windowKey(viewport))return false
      if(direction<0?viewport.scrollTop>1:viewport.scrollHeight-viewport.scrollTop-viewport.clientHeight>4)return false
      input.consumed=true;return true
    },
    manual:()=>manual,
    direction:()=>input?.direction || movement,
  }
}

export function updateLatestOverlay(viewport,logicalEnd,manualUp) {
  const host=viewport?.closest?.('.session-viewer')
  if(!host)return
  const pending=viewport.getAttribute('data-latest-pending')==='true',failed=viewport.getAttribute('data-latest-error')==='true'
  const atEnd=viewport.scrollHeight-viewport.scrollTop-viewport.clientHeight<=4
  const manual=manualUp===undefined?host.getAttribute?.('data-manual-up')==='true':manualUp
  host.setAttribute('data-manual-up',String(manual))
  host.setAttribute('data-latest-visible',String(manual&&!(logicalEnd&&atEnd&&!pending)))
  const bounds=host.getBoundingClientRect?.(),records=viewport.getBoundingClientRect?.()
  if(bounds&&records)host.style?.setProperty('--session-latest-bottom',Math.max(10,bounds.bottom-Math.min(bounds.bottom,records.bottom)+12)+'px')
}

export function createSealedViewer({send,fetchArchive,document,window,workerFactory=()=>new Worker(new URL('./session-archive-worker.js',import.meta.url),{type:'module'}),cleanupMs=10000,custodyFactory=acquireArchiveCustody,deleteStage=deleteArchiveStage,scrollIntent=createReaderScrollIntent(),getActivePosition=()=>null}) {
  let active=null,pending=null,sequence=0,authority=0,blocked=false,stopped='',authorityFence='',blockedNotice='',domEpoch='',indexRetry=''
  const clear=owned=>{
    if(!owned || active!==owned || owned.domCleared)return
    clearPageProgress(owned)
    owned.renderPending=null
    owned.pageAwaiting=null
    owned.latestJump=null
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
    owned.renderPending=null
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
    if(owned.latestJump)event(owned,'latest-error',{latestRequest:owned.latestJump.request,code:error.message})
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
    try{await rpc(owned,'adopt',{name:owned.stageName,custody:owned.custody,manifest:owned.manifest})}
    catch(error){
      // A previous derived index is disposable; rebuild from verified evidence.
      if(error.message!=='archive_index_stale' || indexRetry===owned.identity)throw error
      // Retirement joins this phase transition; do not await that join here.
      if(current(owned)){indexRetry=owned.identity;pending=owned.config;stopped='';void retire(owned)}
      return
    }
    if(current(owned)){
      const retry=owned.retryPage;owned.retryPage=null;owned.pagePending=false
      owned.readerReady=true
      const position=owned.config.historical?getActivePosition(owned.config.scope):null
      if(position&&!owned.latestJump&&!retry){owned.handoff={...position,intent:owned.viewportIntent || 0};event(owned,'follow',{manualUp:true,anchor:position.key,fragmentAnchor:position.fragment})}
      const target=owned.handoff?pieceRank(owned.handoff.key):null
      await page(owned,owned.latestJump?{last:true}:retry?.options || (target!==null?{...(target>0n?{after:rankKey(target-1n)}:{}),continuity:'handoff'}:owned.config.historical&&owned.config.anchor?{offset:BigInt(owned.config.anchor.split(':')[0]).toString()}:{last:true}),owned.latestJump?undefined:retry?.visibleKey)
    }
  }

  function retryPage(owned) {
    if(!current(owned)||!owned.failedPage||owned.pagePending||owned.markdownBusy)return
    owned.retryPage=owned.failedPage;owned.pagePending=true
    event(owned,'progress',{phase:'navigating'})
    owned.phaseTransition=(async()=>{
      const response=await rpc(owned,'release-reader')
      if(response.stage?.name!==owned.stageName||response.stage?.custody!==owned.custody||JSON.stringify(response.stage.manifest)!==JSON.stringify(owned.manifest))throw Error('archive_receipt_mismatch')
      terminateWorker(owned)
      if(current(owned))demand(owned,owned.config.apiUrl+owned.manifest.content_path,'revalidate')
    })().catch(()=>{if(current(owned)){cleanupFailed(owned);void retire(owned)}})
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
    if(!current(owned) || owned.pagePending || owned.pageAwaiting || owned.followPending || owned.markdownBusy)return
    const intent=owned.localIntent;owned.localIntent=null
    if(intent?.kind==='markdown'){
      if(intent.page){owned.localIntent={kind:'markdown',key:intent.key};void page(owned,{...intent.page,bodyKey:intent.key},intent.key)}
      else void readMarkdown(owned,intent.key)
    }
    else if(intent)void page(owned,intent.options)
  }
  const pieceRank=indexedCursorRank,rankKey=indexedCursorKey
  function sealedSourcePoint(viewport) {
    if(!viewport||!scrollIntent.manual())return null
    const box=viewport.getBoundingClientRect(),y=viewport.scrollHeight-viewport.scrollTop-viewport.clientHeight<=4?box.bottom-1:viewport.scrollTop<=1?box.top+1:box.top+box.height/2
    const gap=[...viewport.querySelectorAll('[data-history-gap="source"]')].find(node=>{const r=node.getBoundingClientRect();return visibleConversationNode(node,viewport)&&r.top<=y&&r.bottom>=y})
    if(!gap)return null
    const r=gap.getBoundingClientRect(),before=pieceRank(gap.dataset.gapBefore),after=pieceRank(gap.dataset.gapAfter),fraction=BigInt(Math.round(Math.max(0,Math.min(1,(y-r.top)/Math.max(1,r.height)))*10000))
    return {rank:before+(after-before)*fraction/10000n,y:y-box.top}
  }
  async function page(owned,options={},visibleKey) {
    if(!current(owned))return
    if(owned.pagePending || owned.pageAwaiting || owned.followPending || owned.markdownBusy){owned.localIntent={kind:'page',options:{...options}};return}
    owned.pagePending=true
    const windowRequest=++sequence
    owned.pageCommand=windowRequest
    const latest=options.last?owned.latestJump:null
    owned.renderPending=null;owned.boundaryIntent=owned.viewportIntent || 0
    let intent,previousTop,bounds,follow,sourcePoint,anchors,viewport
    const capturePosition=()=>{
      viewport=document.getElementById?.('session-records');intent=owned.viewportIntent || 0;previousTop=viewport?.scrollTop || 0;bounds=viewport?.getBoundingClientRect?.()
      follow=!scrollIntent.manual()&&viewport&&viewport.scrollHeight-viewport.scrollTop-viewport.clientHeight<=4;sourcePoint=sealedSourcePoint(viewport)
      anchors=bounds?[...(viewport.querySelectorAll?.('[data-fragment-path],[data-block-key],[data-group-key]') || [])].filter(node=>{const r=node.getBoundingClientRect();return !node.querySelector?.('[data-history-gap]')&&visibleConversationNode(node,viewport)&&r.bottom>bounds.top&&r.top<bounds.bottom}).slice(0,8).map(node=>{const attribute=node.hasAttribute('data-fragment-path')?'data-fragment-path':node.hasAttribute('data-block-key')?'data-block-key':'data-group-key';return {attribute,key:node.getAttribute(attribute),top:node.getBoundingClientRect().top-bounds.top}}):[]
      if(anchors.some(anchor=>anchor.attribute==='data-fragment-path'))sourcePoint=null
    }
    capturePosition()
    const operation={timer:null};owned.pageProgress=operation
    operation.timer=window.setTimeout(()=>{
      operation.timer=null
      if(current(owned) && owned.pagePending && owned.pageProgress===operation)event(owned,'progress',{phase:'navigating'})
    },100)
    try {
      let result=await rpc(owned,'page',{page:options})
      const transfer=owned.handoff
      if(current(owned)&&transfer&&!transfer.disclosures){
        const valid=()=>current(owned)&&owned.handoff===transfer&&transfer.intent===(owned.viewportIntent || 0)
        const cancelled=()=>{
          if(!current(owned))return
          if(owned.handoff===transfer)owned.handoff=null
          if(!owned.handoff&&!owned.localIntent&&!owned.latestJump)owned.localIntent={kind:'page',options:owned.seekStartIntent===(owned.viewportIntent || 0)?{}:{...options}}
        }
        if(!valid()){cancelled();return}
        const piece=result.rows.find(piece=>piece.key===transfer.key);let changed=false
        if(piece&&transfer.groupOpen&&piece.group&&!piece.groupOpen){await rpc(owned,'disclosure',{key:'group:'+piece.group,open:true});changed=true}
        if(!valid()){cancelled();return}
        if(piece&&transfer.pieceOpen&&!piece.open){await rpc(owned,'disclosure',{key:piece.key,open:true});changed=true}
        if(!valid()){cancelled();return}
        transfer.disclosures=true
        if(changed)result=await rpc(owned,'page',{page:options})
        if(!valid()){cancelled();return}
      }
      clearPageProgress(owned,operation)
      if(current(owned) && visibleKey && !result.rows.some(row=>row.key===visibleKey))throw Error('archive_piece_missing')
      if(current(owned)){
        // Input may keep moving the viewport while an indexed read is pending.
        // The visible publication-time text, not the request-time position,
        // owns both admission and the subsequent layout restore.
        capturePosition()
        if(!latest&&scrollIntent.manual()){
          const fragment=anchors.find(anchor=>anchor.attribute==='data-fragment-path'),visible=[...(viewport?.querySelectorAll?.('.session-record') || [])].find(node=>{const r=node.getBoundingClientRect();return bounds&&visibleConversationNode(node,viewport)&&r.bottom>bounds.top&&r.top<bounds.bottom})
          event(owned,'follow',{manualUp:true,anchor:sourcePoint?rankKey(sourcePoint.rank):visible?.dataset.blockKey || '',fragmentAnchor:sourcePoint?'':fragment?.key || ''})
        }
        // Preserve proven endpoints across bounded ordinary reads. `more` is
        // directional and may conservatively include an exact-size boundary.
        const ordinary=!options.query&&options.offset===undefined
        if(ordinary&&!options.after&&!options.before&&!options.last)owned.startKey=result.first
        if(ordinary&&options.last)owned.endKey=result.last
        const canPrevious=ordinary&&owned.startKey&&result.first===owned.startKey?false:!!options.after||!!options.offset||!!options.query||((!!options.before||!!options.last)&&!!result.more)
        const canNext=ordinary&&owned.endKey&&result.last===owned.endKey?false:!options.last&&(!!options.before||!!result.more)
        owned.page=options;owned.first=result.first;owned.last=result.last;owned.searchAfter=result.searchAfter;owned.searchCursor=result.searchCursor
        owned.visibleRows=result.rows;owned.canPrevious=canPrevious;owned.canNext=canNext;owned.failedPage=null
        owned.boundaryIntent=owned.viewportIntent || 0
        owned.pageAwaiting=windowRequest
        event(owned,'page',{...result,request:windowRequest,kind:undefined,purpose:options.continuity || '',representedBefore:options.after?options.after+':1':options.offset!==undefined?String(options.offset).padStart(19,'0')+':0000000':result.first,representedAfter:options.before || (result.last?result.last+':1':''),fromEnd:!!options.last||!!options.before,canPrevious,canNext})
        if(latest && owned.latestJump===latest){latest.phase='page-ready';latest.first=result.first;latest.last=result.last;const last=result.rows.at(-1);latest.markdown=last?.kind==='text'&&last.body?.format==='markdown'&&last.body.more?last.key:null}
        const revision=owned.presentationRevision=(owned.presentationRevision || 0)+1
        const settled=rendered=>{
          if(owned.pageAwaiting===windowRequest)owned.pageAwaiting=null
          drainLocalIntent(owned)
          window.requestAnimationFrame?.(()=>inspectSealedBoundary({target:rendered}))
        }
        const restore=attempt=>{
          if(latest&&owned.latestJump===latest)return
          if(!current(owned) || revision!==owned.presentationRevision)return
          const rendered=document.getElementById?.('session-records')
          if(rendered?.getAttribute?.('data-sealed-dom-owner')!==owned.config.stamp||rendered.getAttribute('data-sealed-dom-load')!==String(owned.config.load)||rendered.getAttribute('data-sealed-page-request')!==String(windowRequest)||rendered.getAttribute('data-sealed-page-first')!==result.first||rendered.getAttribute('data-sealed-page-last')!==result.last){if(attempt<8)window.requestAnimationFrame?.(()=>restore(attempt+1));else{owned.pageAwaiting=null;event(owned,'error',{code:'archive_page_dom_ack_timeout'})}return}
          syncWindow(owned,rendered)
          if(intent!==(owned.viewportIntent || 0)){owned.handoff=null;settled(rendered);return}
          if(owned.handoff){
            const handoff=owned.handoff
            if(handoff.intent!==(owned.viewportIntent || 0)){owned.handoff=null;settled(rendered);return}
            const leaf=[...rendered.querySelectorAll('[data-fragment-path]')].find(node=>node.dataset.fragmentPath===handoff.fragment&&!node.querySelector('[data-history-gap],[data-body-gap],[data-sealed-body-needed]'))
            if(handoff.payload&&handoff.bodyPart!==undefined){
              const part=[...rendered.querySelectorAll('[data-body-part-index]')].find(node=>node.dataset.bodyKey===handoff.key&&Number(node.dataset.bodyPartIndex)===handoff.bodyPart)
              const span=[...(part?.querySelectorAll('[data-payload-start]') || [])].find(node=>Number(node.dataset.payloadStart)<=handoff.localOffset&&Number(node.dataset.payloadStart)+node.textContent.length>handoff.localOffset)
              const range=span?payloadTextRange(span,handoff.localOffset-Number(span.dataset.payloadStart)):null,pre=span?.closest('pre')
              if(range&&pre&&pre.scrollHeight>pre.clientHeight+1){const within=handoff.payload.innerTop ?? Math.min(120,pre.clientHeight/3);pre.scrollTop+=range.getBoundingClientRect().top-pre.getBoundingClientRect().top-within}
              const bounds=range?.getBoundingClientRect()
              if(bounds?.height){rendered.scrollTop+=bounds.top-rendered.getBoundingClientRect().top-handoff.top;scrollIntent.positioned();owned.handoff=null;settled(rendered);return}
            }
            if(leaf&&visibleConversationNode(leaf,rendered)){const pre=leaf.closest('pre');if(pre&&handoff.innerScroll!==null&&handoff.innerScroll!==undefined)pre.scrollTop=handoff.innerScroll;rendered.scrollTop+=leaf.getBoundingClientRect().top-rendered.getBoundingClientRect().top-handoff.top;scrollIntent.positioned();owned.handoff=null;settled(rendered);return}
            const piece=owned.visibleRows.find(piece=>piece.key===handoff.key)
            if(!handoff.hydrated&&handoff.payload&&piece?.body?.format==='json'){
              handoff.hydrated=true
              void hydrateBody(owned,handoff.key,0,0,handoff).then(()=>{if(current(owned)&&owned.pageAwaiting===windowRequest)window.requestAnimationFrame?.(()=>restore(0))})
              return
            }
            if(!handoff.hydrated&&piece?.body?.format==='markdown'&&piece.body.more){
              handoff.hydrated=true
              void readMarkdown(owned,handoff.key).then(()=>{if(current(owned)&&owned.pageAwaiting===windowRequest)window.requestAnimationFrame?.(()=>restore(0))})
              return
            }
            if(attempt<8){window.requestAnimationFrame?.(()=>restore(attempt+1));return}
            owned.handoff=null;owned.pageAwaiting=null;event(owned,'error',{code:'archive_handoff_position_unavailable'});return
          }
          if(owned.seekStartIntent===intent){rendered.scrollTop=0;scrollIntent.positioned();if(!owned.canPrevious)owned.seekStartIntent=null;settled(rendered);return}
          if(!ownedPositionUnchanged(rendered,previousTop)){settled(rendered);return}
          const box=rendered.getBoundingClientRect?.()
          if(sourcePoint&&box){
            const gap=[...rendered.querySelectorAll('[data-history-gap="source"]')].find(node=>pieceRank(node.dataset.gapBefore)<=sourcePoint.rank&&pieceRank(node.dataset.gapAfter)>sourcePoint.rank)
            let point
            if(gap&&visibleConversationNode(gap,rendered)){const r=gap.getBoundingClientRect(),before=pieceRank(gap.dataset.gapBefore),after=pieceRank(gap.dataset.gapAfter);point=r.top+r.height*Number((sourcePoint.rank-before)*10000n/(after-before))/10000}
            else {const row=[...rendered.querySelectorAll('.session-record')].findLast(node=>pieceRank(node.dataset.blockKey)<=sourcePoint.rank);if(row)point=row.getBoundingClientRect().top}
            if(point!==undefined){rendered.scrollTop+=point-box.top-sourcePoint.y;scrollIntent.positioned();settled(rendered);return}
          }
          for(const anchor of anchors){const node=[...rendered.querySelectorAll(`[${anchor.attribute}]`)].find(node=>node.getAttribute(anchor.attribute)===anchor.key);if(node&&box){rendered.scrollTop+=node.getBoundingClientRect().top-box.top-anchor.top;scrollIntent.positioned();settled(rendered);return}}
          if(options.last||follow)rendered.scrollTop=rendered.scrollHeight
          else rendered.scrollTop=previousTop
          scrollIntent.positioned()
          settled(rendered)
        }
        window.requestAnimationFrame?.(()=>restore(0))
      }
    }catch(error){owned.failedPage={options:{...options},visibleKey};clearPageProgress(owned,operation);if(current(owned)){if(owned.latestJump){event(owned,'latest-error',{latestRequest:owned.latestJump.request,code:error.message});owned.latestJump=null}event(owned,'error',{code:error.message})}}finally{clearPageProgress(owned,operation);owned.pagePending=false;if(owned.latestJump?.phase==='page-ready')finishLatestPage(owned);else drainLocalIntent(owned)}
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
    owned.renderPending=null;owned.boundaryIntent=owned.viewportIntent || 0
    if(owned.pagePending||owned.markdownBusy){owned.localIntent={kind:'markdown',key};return}
    owned.markdownBusy=true
    const intent=owned.viewportIntent || 0,follow=!scrollIntent.manual()
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
    }catch(error){failure(owned,error)}finally{
      if(follow)owned.followPending=true
      owned.markdownBusy=false;drainLocalIntent(owned)
      window.requestAnimationFrame?.(()=>window.requestAnimationFrame?.(()=>{
        if(!current(owned))return
        const viewport=document.getElementById?.('session-records')
        if(follow&&intent===(owned.viewportIntent || 0)&&!scrollIntent.manual()&&viewport){viewport.scrollTop=viewport.scrollHeight;scrollIntent.positioned()}
        owned.followPending=false;drainLocalIntent(owned)
        if(viewport)inspectSealedBoundary({target:viewport})
      }))
    }
  }
  async function hydrateBody(owned,key,part=0,anchorPart=part,position=null) {
    if(!current(owned)||owned.pagePending||owned.markdownBusy)return
    owned.markdownBusy=true
    let viewport,box,intent,leaf,anchor,top,follow
    const capture=()=>{
      viewport=document.getElementById?.('session-records');box=viewport?.getBoundingClientRect?.();intent=owned.viewportIntent || 0
      leaf=[...(viewport?.querySelectorAll?.('[data-fragment-path]') || [])].find(node=>{const r=node.getBoundingClientRect();return !node.querySelector?.('[data-body-gap],[data-history-gap],[data-sealed-body-needed]')&&box&&visibleConversationNode(node,viewport)&&r.bottom>box.top&&r.top<box.bottom})
      anchor=leaf&&box?{node:leaf,top:leaf.getBoundingClientRect().top-box.top}:null;top=viewport?.scrollTop || 0;follow=!scrollIntent.manual()
    }
    capture()
    let body
    try{
      let result=await rpc(owned,'body',{key,part}),start=0
      if(position){
        const valid=()=>result.key===key&&result.body?.part===part&&result.body.format==='json'&&typeof result.body.text==='string'&&new TextEncoder().encode(result.body.text).length<=65536
        if(!valid())throw Error('archive_body_mismatch')
        while(current(owned)&&owned.handoff===position&&position.intent===(owned.viewportIntent || 0)&&result.body.more&&position.payload.offset>=start+result.body.text.length){
          if(!result.body.text.length||part>=100000)throw Error('archive_body_cursor_stalled')
          start+=result.body.text.length;part++;result=await rpc(owned,'body',{key,part})
          if(!valid())throw Error('archive_body_mismatch')
        }
        if(!current(owned)||owned.handoff!==position||position.intent!==(owned.viewportIntent || 0))return
        if(position.payload.offset>start+result.body.text.length)throw Error('archive_body_position_missing')
        position.bodyPart=part;position.localOffset=Math.max(0,Math.min(result.body.text.length-1,position.payload.offset-start));anchorPart=part
        position.fragment=key+':body:'+part+':text:'+payloadChunkStart(result.body,position.localOffset)
      }
      if(!current(owned)||result.key!==key||result.body.part!==part)return
      body=result.body
      capture()
      const request=++sequence,acknowledged=new Promise((resolve,reject)=>{const timer=window.setTimeout(()=>{owned.markdownWaiter=null;reject(Error('archive_body_ack_timeout'))},10000);owned.markdownWaiter={request,resolve,reject,timer}})
      event(owned,'body',{...result,request,anchorPart,seekStart:owned.seekStartIntent===intent,fragmentAnchor:position?.fragment || (owned.seekStartIntent===intent?'':leaf?.dataset.fragmentPath || ''),kind:undefined})
      await acknowledged
      if(result.body.format==='markdown'&&result.body.more)owned.localIntent={kind:'markdown',key}
    }catch(error){failure(owned,error)}finally{
      owned.markdownBusy=false;owned.followPending=true;drainLocalIntent(owned)
      window.requestAnimationFrame?.(()=>window.requestAnimationFrame?.(()=>{
        if(!current(owned))return
        const rendered=document.getElementById?.('session-records')
        if(rendered&&!position&&intent===(owned.viewportIntent || 0)){
          if(owned.seekStartIntent===intent)rendered.scrollTop=0
          else if(follow&&!scrollIntent.manual())rendered.scrollTop=rendered.scrollHeight
          else if(anchor?.node.isConnected)rendered.scrollTop+=anchor.node.getBoundingClientRect().top-rendered.getBoundingClientRect().top-anchor.top
          else rendered.scrollTop=top
          scrollIntent.positioned()
        }
        owned.followPending=false;drainLocalIntent(owned);if(rendered)inspectSealedBoundary({target:rendered})
      }))
    }
    return body
  }
  function acknowledgeRender(owned) {
    const handoff=owned?.renderPending
    if(!handoff || handoff.scheduled || !current(owned))return
    handoff.scheduled=true
    const inspect=attempt=>{
      if(!current(owned)||owned.renderPending!==handoff)return
      const viewport=document.getElementById?.('session-records')
      if(viewport?.getAttribute?.('data-sealed-dom-owner')===owned.config.stamp
        && viewport.getAttribute('data-sealed-dom-load')===String(owned.config.load)
        && viewport.getAttribute('data-sealed-first')===handoff.first && viewport.getAttribute('data-sealed-last')===handoff.last
        && viewport.getAttribute('data-render-request')===String(handoff.request)
        && viewport.getAttribute('data-render-part')===String(handoff.part)){
        if((owned.viewportIntent || 0)===handoff.intent){
          const box=viewport.getBoundingClientRect?.()
          const moved=ownedPositionUnchanged(viewport,handoff.top)?0:viewport.scrollTop-handoff.top
          if(handoff.head||owned.seekStartIntent===handoff.intent)viewport.scrollTop=0
          else if(handoff.anchors?.some(anchor=>anchor.node.isConnected)&&box){const anchor=handoff.anchors.find(anchor=>anchor.node.isConnected);viewport.scrollTop+=anchor.node.getBoundingClientRect().top-box.top-anchor.top+moved}
          else if(moved===0)viewport.scrollTop=handoff.tail?viewport.scrollHeight:handoff.top
          scrollIntent.positioned()
        }
        owned.boundaryIntent=owned.viewportIntent || 0
        owned.renderPosition={intent:owned.viewportIntent || 0,page:owned.pageCommand,part:handoff.part,top:viewport.scrollTop,epoch:scrollIntent.ownedPosition?.(viewport)?.epoch}
        owned.renderPending=null
        window.requestAnimationFrame?.(()=>inspectSealedBoundary({target:viewport}))
      }else if(attempt<4)window.requestAnimationFrame?.(()=>inspect(attempt+1))
      else handoff.scheduled=false
    }
    window.requestAnimationFrame?.(()=>inspect(0))
  }
  function refreshSealedLatest(owned) {
    const viewport=document.getElementById?.('session-records')
    if(!current(owned)||viewport?.getAttribute?.('data-sealed-dom-owner')!==owned.config.stamp)return
    syncWindow(owned,viewport)
    const logicalEnd=!!owned.readerReady&&!!owned.visibleRows?.length&&!owned.handoff&&!owned.canNext&&!owned.page.query&&viewport.getAttribute('data-render-more')!=='true'&&viewport.getAttribute('data-last-prose-more')!=='true'
    viewport.setAttribute?.('data-logical-latest',String(logicalEnd))
    scrollIntent.inspect(viewport,logicalEnd)
    viewport.setAttribute?.('data-reader-direction',String(scrollIntent.direction()))
    updateLatestOverlay(viewport,logicalEnd,scrollIntent.manual())
    if(owned.manualSent!==scrollIntent.manual()){
      owned.manualSent=scrollIntent.manual()
      const box=viewport.getBoundingClientRect?.(),visible=[...(viewport.querySelectorAll?.('.session-record') || [])].find(node=>{const r=node.getBoundingClientRect();return r.height>0&&box&&r.bottom>box.top})
      event(owned,'follow',{manualUp:owned.manualSent,anchor:visible?.getAttribute?.('data-block-key') || ''})
    }
  }
  function syncWindow(owned,viewport) {
    if(viewport?.getAttribute?.('data-sealed-page-request')!==String(owned.pageCommand))return
    owned.first=viewport.getAttribute('data-sealed-first') || ''
    owned.last=viewport.getAttribute('data-sealed-last') || ''
    owned.canPrevious=viewport.getAttribute('data-archive-can-previous')==='true'
    owned.canNext=viewport.getAttribute('data-archive-can-next')==='true'
  }
  function acknowledgeLatest(owned) {
    const latest=owned?.latestJump
    if(!latest || latest.phase!=='render' || latest.scheduled || !current(owned))return
    latest.scheduled=true
    const inspect=attempt=>{
      if(!current(owned)||owned.latestJump!==latest)return
      const viewport=owned.latestHost?.querySelector?.('[data-sealed-dom-owner]') || document.getElementById?.('session-records')
      if(viewport?.getAttribute?.('data-sealed-dom-owner')===owned.config.stamp && viewport.getAttribute('data-sealed-dom-load')===String(owned.config.load)
        &&viewport.getAttribute('data-latest-request')===String(latest.request)&&viewport.getAttribute('data-render-request')===String(latest.renderRequest)
        &&viewport.getAttribute('data-sealed-first')===latest.first&&viewport.getAttribute('data-sealed-last')===latest.last
        &&viewport.getAttribute('data-render-more')!=='true'&&viewport.getAttribute('data-last-prose-more')!=='true'){
        owned.latestJump=null
        owned.pageAwaiting=null
        if((owned.viewportIntent || 0)!==latest.intent)event(owned,'latest-cancelled',{latestRequest:latest.request})
        else {viewport.scrollTop=viewport.scrollHeight;scrollIntent.clear();viewport.focus?.({preventScroll:true});owned.boundaryIntent=owned.viewportIntent || 0;event(owned,'latest-complete',{latestRequest:latest.request})}
        refreshSealedLatest(owned)
      }else if(attempt<4)window.requestAnimationFrame?.(()=>inspect(attempt+1))
      else latest.scheduled=false
    }
    window.requestAnimationFrame?.(()=>inspect(0))
  }
  function finishLatestPage(owned) {
    const latest=owned.latestJump
    if(!latest||!current(owned))return
    const render=()=>{if(current(owned)&&owned.latestJump===latest){latest.phase='render';latest.renderRequest=++sequence;event(owned,'latest-render',{request:latest.renderRequest,latestRequest:latest.request});acknowledgeLatest(owned)}}
    if(latest.markdown){latest.phase='hydrating';void readMarkdown(owned,latest.markdown).then(render)}else render()
  }
  function beginLatest(owned,request) {
    boundary=''
    owned.latestCommand=request;owned.renderPending=null;owned.handoff=null
    owned.latestJump={request,intent:owned.viewportIntent || 0,phase:'page'}
    if(!owned.readerReady)return
    if(owned.failedPage){owned.failedPage={options:{last:true}};retryPage(owned)}
    else void page(owned,{last:true})
  }
  function renderPart(owned,part,direction) {
    if(!current(owned)||owned.renderPending||!Number.isSafeInteger(part)||part<0||part>100000)return
    const viewport=document.getElementById?.('session-records'),previous=Number(viewport?.getAttribute?.('data-render-part'))
    const request=++sequence,intent=owned.viewportIntent || 0
    const anchors=visibleRenderAnchors(viewport,previous)
    if(owned.seekStartIntent!==intent&&protectedRenderPoint(viewport,anchors,previous,part))return
    owned.renderPending={request,part,direction:direction || (part<previous?'previous':'next'),first:owned.first,last:owned.last,intent,top:viewport?.scrollTop || 0,head:viewport?.scrollTop<=1||owned.seekStartIntent===intent,tail:viewport&&viewport.scrollHeight-viewport.scrollTop-viewport.clientHeight<=4,anchors}
    owned.boundaryIntent=intent
    event(owned,'render',{request,part})
    acknowledgeRender(owned)
  }
  document.addEventListener('click',domEvent=>{
    const button=domEvent.target?.closest?.('[data-sealed-action]'),owned=active
    if(!button || !owned || !current(owned) || button.getAttribute('data-sealed-owner')!==owned.config.stamp)return
    const action=button.getAttribute('data-sealed-action');domEvent.preventDefault()
    if(owned.latestJump){event(owned,'latest-cancelled',{latestRequest:owned.latestJump.request});owned.latestJump=null}
    if(action==='cancel'){stopped=owned.identity;pending=null;clear(owned);event(owned,'cancelled');void retire(owned)}
    else if(action==='first')void page(owned)
    else if(action==='last')void page(owned,{last:true})
    else if(action==='next')void page(owned,{after:owned.last})
    else if(action==='search-next')void page(owned,{query:owned.page.query,after:owned.searchAfter,searchCursor:owned.searchCursor})
    else if(action==='previous')void page(owned,{before:owned.first})
    else if(action==='retry-page')retryPage(owned)
    else if(action==='body')void page(owned,{...owned.page,bodyKey:button.getAttribute('data-sealed-key'),bodyPart:Number(button.getAttribute('data-sealed-part'))})
    else if(action==='render')renderPart(owned,Number(button.getAttribute('data-sealed-part')))
    else if(action==='markdown')void readMarkdown(owned,button.getAttribute('data-sealed-key'))
    else if(action==='search'){const input=document.getElementById('sealed-search');void page(owned,{query:input?.value || ''})}
    else if(action==='position'){const input=document.getElementById('sealed-position');void page(owned,{offset:input?.value || '0'})}
  })
  let boundary=''
  for(const name of ['wheel','touchstart','touchmove','pointerdown','keydown'])document.addEventListener(name,domEvent=>{
    const viewport=document.getElementById?.('session-records')
    const button=domEvent.target?.closest?.('.session-jump-latest')
    if(button && active && button.getAttribute('data-latest-owner')===active.config.stamp)active.latestHost=button.closest('.session-viewer')
    if(active && viewport && (viewport===domEvent.target || viewport.contains?.(domEvent.target))){
      scrollIntent.note(domEvent,viewport)
      active.viewportIntent=(active.viewportIntent || 0)+1
      active.seekStartIntent=name==='keydown'&&domEvent.key==='Home'&&domEvent.isTrusted!==false&&domEvent.target===viewport?active.viewportIntent:null
      if(active.seekStartIntent===active.viewportIntent)boundary=''
      const forward=['ArrowDown','PageDown','End',' '],backward=['ArrowUp','PageUp','Home']
      const gesture=name==='wheel'&&typeof domEvent.deltaY==='number'&&domEvent.deltaY!==0 || name==='touchmove' || name==='pointerdown'&&domEvent.target===viewport || name==='keydown'&&(forward.includes(domEvent.key)||backward.includes(domEvent.key))
      if(gesture){active.handoff=null;active.boundaryGesture=active.viewportIntent;active.boundaryDirection=name==='wheel'?(domEvent.deltaY<0?'previous':'next'):name==='keydown'?(backward.includes(domEvent.key)?'previous':'next'):'';window.requestAnimationFrame?.(()=>inspectSealedBoundary({target:viewport}))}
    }
  },true)
  const inspectSealedBoundary=domEvent=>{
    const owned=active,viewport=domEvent.target
    if(owned)refreshSealedLatest(owned)
    if(!owned || !current(owned) || owned.pagePending || owned.pageAwaiting || owned.markdownBusy || owned.followPending || owned.latestJump || owned.failedPage || owned.renderPending || viewport?.id!=='session-records' || viewport.getAttribute?.('data-sealed-dom-owner')!==owned.config.stamp)return
    syncWindow(owned,viewport)
    const part=Number(viewport.getAttribute('data-render-part')),more=viewport.getAttribute('data-render-more')==='true'
    const box=viewport.getBoundingClientRect?.()
    const seekStart=owned.seekStartIntent===(owned.viewportIntent || 0)
    const wanted=seekStart?-1:scrollIntent.manual()?(scrollIntent.direction() || (owned.boundaryDirection==='previous'?-1:1)):1
    const near=node=>{const r=node.getBoundingClientRect();return box&&visibleConversationNode(node,viewport)&&r.bottom>box.top-192&&r.top<box.bottom+192}
    const body=[...(viewport.querySelectorAll?.('[data-sealed-body-needed]') || [])].find(near)
    if(body){if(body.dataset.bodyFormat==='markdown')void readMarkdown(owned,body.getAttribute('data-sealed-body-needed'));else void hydrateBody(owned,body.getAttribute('data-sealed-body-needed'));return}
    const bodyGap=[...(viewport.querySelectorAll?.('[data-body-gap]') || [])].find(node=>near(node)&&(wanted<0?Number(node.dataset.bodyPart)<Number(node.dataset.bodyAnchor):Number(node.dataset.bodyPart)>Number(node.dataset.bodyAnchor)))
    if(bodyGap){void hydrateBody(owned,bodyGap.dataset.bodyGap,seekStart?0:Number(bodyGap.dataset.bodyPart),seekStart?0:Number(bodyGap.dataset.bodyAnchor));return}
    const visibleText=[...viewport.querySelectorAll('[data-fragment-path]')].find(node=>{const r=node.getBoundingClientRect();return !node.querySelector('[data-history-gap],[data-body-gap],[data-sealed-body-needed]')&&visibleConversationNode(node,viewport)&&r.bottom>box.top&&r.top<box.bottom})
    const visibleSource=visibleText?.closest('.session-record')?.dataset.blockKey
    const sourceGap=[...(viewport.querySelectorAll?.('[data-history-gap="source"]') || [])].find(node=>{const r=node.getBoundingClientRect(),middle=box?box.top+box.height/2:0;return near(node)&&(wanted<0?r.top<=middle:r.bottom>=middle)&&(!visibleSource||(wanted<0?pieceRank(node.dataset.gapBefore)<pieceRank(visibleSource):pieceRank(node.dataset.gapAfter)>pieceRank(visibleSource)))})
    const nextPart=seekStart?0:wanted<0?Math.max(0,part-1):part+1
    const frontier=seekStart?0:presentationFrontier(viewport,part,wanted)
    const fragmentGap=[...(viewport.querySelectorAll?.('[data-fragment-from]') || [])].find(node=>near(node)&&presentationGapContains(node,frontier)&&(wanted<0?Number(node.dataset.fragmentTo)<part:Number(node.dataset.fragmentTo)>part))
    const rawBounds=sourceGap?.getBoundingClientRect(),middle=box?box.top+box.height/2:0
    const readingRawGap=rawBounds&&rawBounds.top<=middle&&rawBounds.bottom>=middle&&!visibleText
    if(fragmentGap&&(scrollIntent.manual()||seekStart)&&!readingRawGap){
      if(!seekStart&&owned.renderPosition?.intent<=(owned.viewportIntent || 0)&&owned.renderPosition.page===owned.pageCommand&&owned.renderPosition.part===part&&settledRenderPosition(viewport,owned.renderPosition,scrollIntent)&&!scrollIntent.consumeEdge?.(viewport,wanted))return
      // A tall table's omitted side cell can intersect the whole row. Its
      // rectangle proves proximity, not a position in intervening text.
      const target=nextPart
      if(target!==part){renderPart(owned,target,target<part?'previous':'next');return}
    }
    if(sourceGap&&scrollIntent.manual()){
      const leaf=[...viewport.querySelectorAll('[data-fragment-path]')].find(node=>{const r=node.getBoundingClientRect();return !node.querySelector('[data-history-gap]')&&visibleConversationNode(node,viewport)&&r.bottom>box.top&&r.top<box.bottom})
      const point=sealedSourcePoint(viewport),before=pieceRank(sourceGap.dataset.gapBefore),after=pieceRank(sourceGap.dataset.gapAfter),prefix=leaf&&sourceGap.getBoundingClientRect().top<leaf.getBoundingClientRect().top,target=leaf?(prefix?after-1n:before):point?.rank ?? before
      const key=[owned.identity,'gap',sourceGap.dataset.gapBefore,sourceGap.dataset.gapAfter,rankKey(target)].join(':')
      if(key===boundary)return
      boundary=key
      event(owned,'follow',{manualUp:scrollIntent.manual(),anchor:leaf?.closest('.session-record')?.dataset.blockKey || rankKey(target),fragmentAnchor:leaf?.dataset.fragmentPath || ''})
      void page(owned,{...(leaf?(prefix?{before:rankKey(after)}:{after:rankKey(before-1n)}):target>0n?{after:rankKey(target-1n)}:{}),continuity:leaf?'prefetch':'gap'});return
    }
    const items=[...(viewport.querySelectorAll?.('.session-record,[data-group-key]') || [])].filter(node=>visibleConversationNode(node,viewport)),first=items[0]?.getBoundingClientRect(),last=items.at(-1)?.getBoundingClientRect()
    const fill=!first||!last||last.bottom-first.top<viewport.clientHeight-4
    const above=viewport.scrollTop<=192,below=viewport.scrollHeight-viewport.scrollTop-viewport.clientHeight<=192
    const direction=(fill||scrollIntent.manual()&&above)&&owned.canPrevious?'previous':scrollIntent.manual()&&below&&owned.canNext?'next':''
    const key=[owned.identity,owned.first,owned.last,part,direction].join(':')
    if(!direction || key===boundary)return
    boundary=key;owned.boundaryIntent=owned.viewportIntent || 0
    const heights={},visible=items.find(node=>node.classList?.contains('session-record')&&near(node))
    for(const node of viewport.querySelectorAll?.('.session-record') || [])if(Object.keys(heights).length<200)heights[node.dataset.blockKey]=Math.max(0,Math.min(1000000,node.getBoundingClientRect().height))
    const textLeaf=[...viewport.querySelectorAll('[data-fragment-path]')].find(node=>{const r=node.getBoundingClientRect();return !node.querySelector('[data-history-gap]')&&visibleConversationNode(node,viewport)&&r.bottom>box.top&&r.top<box.bottom})
    event(owned,'follow',{manualUp:scrollIntent.manual(),anchor:visible?.dataset.blockKey || '',fragmentAnchor:textLeaf?.dataset.fragmentPath || '',heights})
    if(direction==='previous')void page(owned,{before:fill?viewport.getAttribute('data-sealed-scan-first') || owned.first:viewport.getAttribute('data-sealed-older-boundary') || owned.first,continuity:fill?'fill':'older'})
    else void page(owned,{after:viewport.getAttribute('data-sealed-newer-boundary') || owned.last,continuity:'newer'})
  }
  document.addEventListener('scroll',inspectSealedBoundary,true)
  document.addEventListener('toggle',domEvent=>{
    const node=domEvent.target,owned=active,key=node.getAttribute?.('data-sealed-key')
    if(!key || !owned || !current(owned) || node.getAttribute('data-sealed-owner')!==owned.config.stamp)return
    if(node.isConnected===false||!document.getElementById?.('session-records')?.contains?.(node))return
    event(owned,'disclosure',{key,open:node.open})
    void rpc(owned,'disclosure',{key,open:node.open}).then(()=>{
      if(!current(owned))return
      const viewport=document.getElementById?.('session-records')
      const admitted=node.isConnected!==false&&viewport?.contains?.(node)&&viewport.getAttribute('data-sealed-dom-owner')===owned.config.stamp&&viewport.getAttribute('data-sealed-page-request')===String(owned.pageCommand)&&key>=owned.first&&key<=owned.last
      // A queued native toggle can belong to the old, still-connected DOM
      // while a newer verified page waits for publication. Its body cannot
      // acquire an acknowledgement for a piece the accepted page evicted.
      if(admitted&&node.open&&!key.startsWith('group:')&&!owned.pagePending&&!owned.pageAwaiting&&!owned.followPending&&!owned.markdownBusy&&!owned.latestJump)return hydrateBody(owned,key,0)
      window.requestAnimationFrame?.(()=>inspectSealedBoundary({target:document.getElementById?.('session-records')}))
    }).catch(error=>failure(owned,error))
  },true)
  window.addEventListener?.('pagehide',()=>{pending=null;clear(active);void retire(active)})
  return {
    configure(config,request,markdownAck) {
      if(config?.active)scrollIntent.configure(config.scope || JSON.stringify([config.workspace,config.session,config.authority]))
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
      if(active)acknowledgeRender(active)
      if(active && current(active)){
        if(Number.isSafeInteger(config?.latestRequest)&&config.latestRequest>=0&&active.latestCommand!==config.latestRequest)beginLatest(active,config.latestRequest)
        if(config?.latestRequest===-1)active.latestJump=null
        acknowledgeLatest(active)
        const owner=active;window.requestAnimationFrame?.(()=>{if(current(owner))refreshSealedLatest(owner)})
      }
    },
    retireAuthority(){authorityFence=active?.config.stamp || '';authority++;pending=null;if(active){clear(active);event(active,'cancelled');void retire(active)}},
    inspectViewport(viewport){if(viewport)inspectSealedBoundary({target:viewport})},
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
  installEventPayload(window)
  let stamp = '', viewerStamp = '', generation = -1, framer = null, viewerIntent = 0
  let activeSave = null
  let reportedGroups=new Map()
  const send = (value,current=()=>true) => queueMicrotask(()=>{if(current())app.ports.sessionBridgeEvent.send(value)})
  const scrollIntent=createReaderScrollIntent()
  let activeReadingPosition=null
  const sealed=createSealedViewer({send,fetchArchive,document,window,workerFactory,scrollIntent,getActivePosition:scope=>activeReadingPosition?.scope===readerScopeKey(scope)&&activeReadingPosition.intent===latestIntent?activeReadingPosition:null})
  let heartbeatKey = ''
  let viewerScope = '', presentation = null
  let historyTransition = false, boundaryKey = '', restoreRevision = 0, resizeViewport=null, resizeSnapshot=null, resizeReady=false, lastFollowing=false, boundaryIntent=0
  let disclosures = new Map()
  const disclosureActions=new Map()
  let disclosureRequest=0,disclosureTimer=null,disclosureRetiredOwner='',disclosureRetry=null,disclosureRetriedGeneration=-1,disclosureFallback=null
  const disclosureNow=()=>window.performance?.now?.() ?? now()
  const clearDisclosureTimer=()=>{if(disclosureTimer!==null){(window.clearTimeout || clearTimeout)(disclosureTimer);disclosureTimer=null}}
  const syncDisclosureFailure=()=>{
    const viewport=document.getElementById?.('session-records'),host=viewport?.closest?.('.session-viewer'),node=host?.querySelector?.('[data-disclosure-error-owner]')
    const owned=node&&node.getAttribute('data-disclosure-error-owner')===viewerStamp&&node.getAttribute('data-disclosure-error-scope')===viewerScope
    const visibleModelError=viewport?.getAttribute?.('data-history-error')==='true'
    const failed=owned&&!visibleModelError&&[...disclosureActions.values()].some(action=>action.phase==='failed'&&!action.errorConfirmed&&currentDisclosure(action))
    if(disclosureFallback&&(!failed||disclosureFallback.node!==node)&&disclosureFallback.node.getAttribute?.('data-disclosure-error-owner')===disclosureFallback.owner)disclosureFallback.node.hidden=true
    disclosureFallback=failed?{node,owner:viewerStamp}:null
    if(owned&&node.hidden===!!failed)node.hidden=!failed
  }
  const retireDisclosures=()=>{clearDisclosureTimer();disclosureActions.clear();disclosureRetry=null;disclosureRetriedGeneration=-1;syncDisclosureFailure()}
  const currentDisclosure=action=>{
    const viewport=document.getElementById?.('session-records')
    return disclosureActions.get(action.namespace)===action && disclosures.has(action.namespace) && disclosures.get(action.namespace)===action.open && action.owner===viewerStamp && action.owner!==disclosureRetiredOwner && action.scope===viewerScope && viewport?.getAttribute?.('data-viewer-owner')===viewerStamp && viewport.getAttribute('data-history-mode')==='active' && action.node.isConnected!==false && viewport.contains?.(action.node) && disclosureKey(action.node)===action.namespace && action.node.open===action.open && (!action.ancestorNode||!action.open||action.ancestorNode.open)
  }
  const failDisclosure=action=>{
    if(!currentDisclosure(action))return
    if(action.phase==='error'){action.phase='failed';action.errorConfirmed=false;syncDisclosureFailure();return}
    if(action.phase==='failed')return
    action.phase='error';action.attempts=0;action.generation=-1;action.deadline=disclosureNow()+5000
    deliverDisclosure(action)
  }
  const expireDisclosures=()=>{
    clearDisclosureTimer()
    for(const action of disclosureActions.values()){
      if(!currentDisclosure(action)){disclosureActions.delete(action.namespace);continue}
      if(action.phase!=='failed'&&disclosureNow()>=action.deadline)failDisclosure(action)
    }
    const active=[...disclosureActions.values()].filter(action=>action.phase!=='failed')
    if(active.length){
      const deadline=Math.min(...active.map(action=>action.deadline))
      disclosureTimer=(window.setTimeout || setTimeout)(expireDisclosures,Math.max(1,deadline-disclosureNow()))
      disclosureTimer?.unref?.()
    }
    syncDisclosureFailure()
  }
  const deliverDisclosure=action=>{
    if(!currentDisclosure(action)){disclosureActions.delete(action.namespace);return}
    if(action.phase==='failed')return
    if(action.generation===generation)return
    if(action.attempts>=8||disclosureNow()>=action.deadline){failDisclosure(action);return}
    action.generation=generation;action.attempts++
    const expected=generation
    send({kind:action.phase==='error'?'viewer-disclosure-error':'viewer-disclosure',stamp:viewerStamp,generation:expected,scope:viewerScope,request:action.request,namespace:action.namespace,key:action.key,ancestor:action.ancestor,open:action.open},()=>currentDisclosure(action)&&action.generation===expected&&generation===expected)
  }
  const makeDisclosure=(node,namespace,key)=>{
    const group=node.closest?.('[data-group-key]'),ancestorNode=group&&group!==node?group:null
    return {owner:viewerStamp,scope:viewerScope,namespace,key,open:node.open,node,ancestorNode,ancestor:ancestorNode?.getAttribute('data-group-key') || '',request:++disclosureRequest,generation:-1,createdGeneration:generation,phase:'delivery',attempts:0,deadline:disclosureNow()+5000}
  }
  const retryDisclosures=()=>{
    const marker=disclosureRetry
    if(!marker||marker.stamp!==viewerStamp||marker.scope!==viewerScope||marker.generation>generation)return
    disclosureRetry=null;disclosureRetriedGeneration=marker.generation
    const viewport=document.getElementById?.('session-records')
    if(viewport?.getAttribute?.('data-viewer-owner')!==viewerStamp||viewport.getAttribute('data-history-mode')!=='active')return
    for(const node of viewport.querySelectorAll('details')){
      const namespace=disclosureKey(node),key=node.getAttribute('data-group-key') ?? node.getAttribute('data-event-key'),prior=disclosureActions.get(namespace)
      if(namespace===null||key===null||!disclosures.has(namespace)||disclosures.get(namespace)!==node.open||prior?.createdGeneration>marker.generation)continue
      disclosureActions.set(namespace,makeDisclosure(node,namespace,key))
    }
    for(const action of disclosureActions.values())deliverDisclosure(action)
    expireDisclosures()
  }
  let latestJump=null,latestIntent=0,followPosition=null
  let activeRenderPending=null,activeRenderPosition=null,pendingEnd=null,activePublication=0,seekStartIntent=null,handoffPosition=null
  const cancelHandoffPosition=ticket=>{
    if(handoffPosition!==ticket)return
    handoffPosition=null
    if(ticket?.waiting&&ticket.owner===viewerStamp&&ticket.scope===viewerScope&&ticket.revision===generation)send({kind:'viewer-handoff-cancelled',stamp:ticket.owner,generation:ticket.revision,publication:ticket.publication})
  }
  function positionAtTail(viewport) {
    viewport.scrollTop=viewport.scrollHeight
    // Its asynchronous scroll notification may arrive between finite byte
    // windows. Bind that owned position to current authority and user intent.
    followPosition={viewport,stamp:viewerStamp,generation,intent:viewerIntent,top:viewport.scrollTop}
    scrollIntent.positioned()
    captureResize()
  }
  const pendingTail = viewport => lastFollowing && followPosition?.pending===true && followPosition.viewport===viewport && followPosition.stamp===viewerStamp && followPosition.generation===generation && followPosition.intent===viewerIntent && Math.abs(viewport.scrollTop-followPosition.top)<=4
  let latestHost=null
  document.addEventListener('click',event=>{
    const button=event.target?.closest?.('.session-jump-latest')
    if(button?.getAttribute?.('data-latest-owner')===viewerStamp)latestHost=button.closest('.session-viewer')
  },true)
  const inspectActiveLatest = () => {
    const viewport=latestJump?.host?.querySelector?.('[data-history-mode="active"]') || document.getElementById?.('session-records')
    if(!viewport || viewport.getAttribute?.('data-viewer-owner')!==viewerStamp || viewport.getAttribute('data-history-mode')!=='active')return
    const logicalEnd=Number(viewport.getAttribute('data-history-after'))>=Number(viewport.getAttribute('data-history-latest'))&&viewport.getAttribute('data-render-more')!=='true'
    updateLatestOverlay(viewport,logicalEnd,scrollIntent.manual())
    const jump=latestJump
    if(!jump || jump.stamp!==viewerStamp || jump.generation!==generation || viewport.getAttribute('data-viewer-generation')!==String(jump.generation) || viewport.getAttribute('data-latest-request')!==String(jump.request))return
    if(jump.intent!==latestIntent){latestJump=null;send({kind:'latest-cancelled',stamp:jump.stamp,generation:jump.generation,request:jump.request});return}
    if(viewport.getAttribute('data-history-error')==='true'){latestJump=null;send({kind:'latest-error',stamp:jump.stamp,generation:jump.generation,request:jump.request});return}
    if(logicalEnd && viewport.getAttribute('data-history-loading')!=='true'){
      positionAtTail(viewport)
      scrollIntent.clear()
      updateLatestOverlay(viewport,true,false)
      viewport.focus?.({preventScroll:true})
      latestJump=null;send({kind:'latest-complete',stamp:jump.stamp,generation:jump.generation,request:jump.request})
    }
  }
  const disclosureKey = node => {
    const group = node.getAttribute?.('data-group-key')
    if(group!==null && group!==undefined)return 'group:'+group
    const event=node.getAttribute?.('data-event-key')
    return event!==null && event!==undefined ? 'event:'+event : null
  }
  const rememberActivePosition=viewport=>{
    if(!viewport?.querySelectorAll||viewport.getAttribute?.('data-history-mode')!=='active'||viewport.getAttribute('data-viewer-owner')!==viewerStamp)return null
    const box=viewport.getBoundingClientRect?.()
    if(!box)return null
    const leaf=[...viewport.querySelectorAll('[data-fragment-path]')].find(node=>{const r=node.getBoundingClientRect();return !node.querySelector('[data-history-gap],[data-body-gap],[data-sealed-body-needed]')&&visibleConversationNode(node,viewport)&&r.bottom>box.top&&r.top<box.bottom&&node.closest('.session-record')})
    const row=leaf?.closest('.session-record'),path=leaf?.dataset.fragmentPath,source=path?.match(/^(\d+:\d+):content(?:$|:)/)?.[1]
    if(!source||path.length>4096)return null
    const key=indexedCursorKey(indexedCursorRank(source))
    const payload=visiblePayloadPosition(viewport,leaf)
    const pre=leaf.closest('pre'),innerScroll=pre&&pre.scrollHeight>pre.clientHeight+1?pre.scrollTop:null
    activeReadingPosition={scope:readerScopeKey(viewerScope),key,fragment:path,intent:latestIntent,payload,innerScroll,pieceOpen:!!leaf.closest('[data-event-key]')?.open,groupOpen:!!leaf.closest('[data-group-key]')?.open,top:Math.max(-10000000,Math.min(10000000,payload?.top ?? leaf.getBoundingClientRect().top-box.top))}
    return {anchor:String(row.dataset.offset).padStart(20,'0'),fragmentAnchor:path}
  }
  const rememberPresentation = event => {
    const viewport=document.getElementById?.('session-records')
    if(!viewport?.querySelectorAll || !viewerScope || viewport.getAttribute?.('data-viewer-owner')!==viewerStamp)return
    if(handoffPosition&&handoffPosition.owner===viewerStamp&&handoffPosition.intent===viewerIntent)return
    const returning=event.type==='click' && (event.target?.closest?.('.task-session-transcript a[href]') || event.target?.closest?.('button.tab')?.textContent?.trim()==='Projects')
    if(!returning && !(event.type==='scroll' && event.target===viewport) && !(event.type==='toggle' && viewport.contains?.(event.target)))return
    rememberActivePosition(viewport)
    const box=viewport.getBoundingClientRect?.(),leaf=box?[...viewport.querySelectorAll('[data-fragment-path]')].find(node=>{const r=node.getBoundingClientRect();return !node.querySelector('[data-history-gap],[data-body-gap],[data-sealed-body-needed]')&&visibleConversationNode(node,viewport)&&r.bottom>box.top&&r.top<box.bottom}):null
    presentation={scope:viewerScope,top:viewport.scrollTop,point:leaf?visibleTextPoint(viewport,leaf):null,disclosures:[...viewport.querySelectorAll('details')].map(node=>[disclosureKey(node),node.open])}
  }
  // One bounded snapshot holds only position/disclosure flags, never payloads.
  // Capture before route handlers replace the task/full-session DOM.
  for(const event of ['click','scroll','toggle'])document.addEventListener(event,rememberPresentation,true)
  document.addEventListener('toggle',event=>{
    const viewport=document.getElementById?.('session-records'),node=event.target
    if(viewport?.getAttribute?.('data-history-mode')!=='active'||viewport.getAttribute('data-viewer-owner')!==viewerStamp||viewerStamp===disclosureRetiredOwner||!viewport.contains?.(node))return
    const key=node.getAttribute?.('data-group-key') ?? node.getAttribute?.('data-event-key')
    const cached=disclosureKey(node)
    if(cached!==null){disclosures.delete(cached);disclosures.set(cached,node.open);while(disclosures.size>400)disclosures.delete(disclosures.keys().next().value)}
    if(key!==null&&key!==undefined){
      expireDisclosures()
      if(!disclosureActions.has(cached)&&disclosureActions.size>=400)disclosureActions.delete(disclosureActions.keys().next().value)
      const previous=disclosureActions.get(cached)
      const action=previous&&previous.owner===viewerStamp&&previous.scope===viewerScope&&previous.node===node&&previous.open===node.open?previous:makeDisclosure(node,cached,key)
      disclosureActions.set(cached,action);deliverDisclosure(action);expireDisclosures()
    }
    scheduleContinuity()
  },true)
  const noteViewerIntent = event => {
    const viewport=document.getElementById?.('session-records')
    if(viewport && (event.target===viewport || viewport.contains?.(event.target))){
      if(event.type==='click'&&(event.isTrusted===false||!event.target?.closest?.('summary')))return
      if(event.type==='toggle'&&handoffPosition?.owner===viewerStamp&&handoffPosition.intent===viewerIntent&&handoffPosition.revision===generation&&(handoffPosition.publication===undefined||Number(viewport.getAttribute?.('data-window-publication'))===handoffPosition.publication)&&handoffPosition.disclosures.some(([key,open])=>key===disclosureKey(event.target)&&open===event.target.open))return
      scrollIntent.note(event,viewport)
      const continuingHome=event.type==='toggle'&&seekStartIntent===viewerIntent&&scrollIntent.pendingHome(viewport)
      const continuingEnd=event.type==='keydown' && event.key==='End' && pendingTail(viewport)
      viewerIntent++;if(event.type!=='toggle')latestIntent++
      if(continuingHome)seekStartIntent=viewerIntent
      if(event.type==='keydown'&&event.key==='Home'&&event.isTrusted!==false&&event.target===viewport)seekStartIntent=viewerIntent
      if(pendingEnd&&pendingEnd.intent!==latestIntent)pendingEnd=null
      if(continuingEnd)followPosition.intent=viewerIntent
      if(event.type==='keydown'&&event.key==='End'&&event.isTrusted!==false&&event.target===viewport){
        event.preventDefault?.();activeRenderPending=null
        const end={stamp:viewport.getAttribute?.('data-viewer-owner') || viewerStamp,intent:latestIntent,sent:-1};pendingEnd=end
        const settle=attempt=>{
          if(pendingEnd!==end||end.stamp!==viewerStamp||end.intent!==latestIntent)return
          const mounted=document.getElementById?.('session-records'),revision=Number(mounted?.getAttribute?.('data-viewer-generation') ?? generation)
          if(mounted?.getAttribute?.('data-viewer-owner')===end.stamp&&revision===generation&&end.sent!==revision){end.sent=revision;send({kind:'viewer-end',stamp:end.stamp,generation:revision})}
          if(attempt<8)requestAnimationFrame(()=>settle(attempt+1));else if(pendingEnd===end)pendingEnd=null
        }
        requestAnimationFrame(()=>settle(0))
      }
    }
  }
  // A scroll notification may lag the position change or come from browser
  // anchoring. Actual input/disclosure events own intent; the position guard
  // below also detects later programmatic scrolling in either direction.
  for(const event of ['click','toggle','wheel','touchstart','touchmove','pointerdown','keydown']) document.addEventListener(event,noteViewerIntent,true)
  const inspectBoundary = event => {
    const viewport=document.getElementById?.('session-records')
    if(!viewport || (event && event.target!==viewport) || viewport.getAttribute?.('data-viewer-owner')!==viewerStamp)return
    const attr=name=>viewport.getAttribute?.('data-history-'+name)
    if(attr('mode')!=='active')return
    if(handoffPosition?.failed&&attr('error')==='true')handoffPosition=null
    if(attr('error')==='true')return
    if(handoffPosition){
      if(handoffPosition.owner!==viewerStamp||handoffPosition.scope!==viewerScope||handoffPosition.revision!==generation||!handoffPosition.failed&&handoffPosition.intent!==viewerIntent)cancelHandoffPosition(handoffPosition)
      else return
    }
    const manual=scrollIntent.inspect(viewport,Number(attr('after'))>=Number(attr('latest'))&&viewport.getAttribute('data-render-more')!=='true')
    viewport.setAttribute?.('data-reader-direction',String(scrollIntent.direction()))
    inspectActiveLatest()
    const atEnd=viewport.scrollHeight-viewport.scrollTop-viewport.clientHeight<=48
    const logicalEnd=Number(attr('after'))>=Number(attr('latest'))&&viewport.getAttribute('data-render-more')!=='true'
    const ownedPosition=followPosition?.viewport===viewport && followPosition.stamp===viewerStamp && followPosition.generation===generation && followPosition.intent===viewerIntent && Math.abs(viewport.scrollTop-followPosition.top)<=4
    const continuing=lastFollowing && (attr('loading')==='true' || ownedPosition)
    if(pendingEnd||latestJump||viewport.getAttribute('data-latest-pending')==='true')return
    // Replacing rows can grow the DOM before the owned tail RAF runs. An
    // unchanged, intent-stamped position still belongs to that handoff.
    const reading=rememberActivePosition(viewport)
    if(event || manual===lastFollowing)send({kind:'viewer-follow',stamp:viewerStamp,generation,manualUp:manual,following:!manual,...(reading || {})})
    if(attr('loading')==='true')return
    if(activeRenderPending&&(activeRenderPending.stamp!==viewerStamp||activeRenderPending.generation!==generation))activeRenderPending=null
    if(activeRenderPending)return
    const box=viewport.getBoundingClientRect?.(),items=[...(viewport.querySelectorAll?.('.session-record,[data-group-key]') || [])].filter(node=>visibleConversationNode(node,viewport))
    const leaf=[...(viewport.querySelectorAll?.('[data-fragment-path]') || [])].find(node=>{const r=node.getBoundingClientRect();return !node.querySelector?.('[data-history-gap]')&&box&&visibleConversationNode(node,viewport)&&r.bottom>box.top&&r.top<box.bottom})
    const seekStart=seekStartIntent===viewerIntent,wanted=seekStart?-1:scrollIntent.direction() || -1,currentPart=Number(viewport.getAttribute?.('data-render-part') || 0)
    const nextPart=seekStart?0:wanted<0?Math.max(0,currentPart-1):currentPart+1
    const frontier=seekStart?0:presentationFrontier(viewport,currentPart,wanted)
    const fragmentGap=[...(viewport.querySelectorAll?.('[data-fragment-from]') || [])].find(node=>{const r=node.getBoundingClientRect();return box&&visibleConversationNode(node,viewport)&&presentationGapContains(node,frontier)&&r.bottom>box.top-192&&r.top<box.bottom+192&&(wanted<0?Number(node.dataset.fragmentTo)<currentPart:Number(node.dataset.fragmentTo)>currentPart)})
    if((manual||seekStart)&&fragmentGap){
      if(!seekStart&&activeRenderPosition?.stamp===viewerStamp&&activeRenderPosition.generation===generation&&activeRenderPosition.intent<=viewerIntent&&activeRenderPosition.part===currentPart&&settledRenderPosition(viewport,activeRenderPosition,scrollIntent)&&!scrollIntent.consumeEdge(viewport,wanted))return
      const target=nextPart
      if(target!==currentPart){
        const anchors=visibleRenderAnchors(viewport,currentPart)
        if(!seekStart&&protectedRenderPoint(viewport,anchors,currentPart,target))return
        const pending={stamp:viewerStamp,generation,part:target,intent:viewerIntent,top:viewport.scrollTop,head:viewport.scrollTop<=1||seekStartIntent===viewerIntent,tail:atEnd,anchors};activeRenderPending=pending
        send({kind:'viewer-render',stamp:viewerStamp,generation,part:target})
        const restore=attempt=>{
          if(activeRenderPending!==pending||pending.stamp!==viewerStamp||pending.generation!==generation){if(activeRenderPending===pending)activeRenderPending=null;return}
          const rendered=document.getElementById?.('session-records')
          if(Number(rendered?.dataset.renderPart)!==target){if(attempt<4)requestAnimationFrame(()=>restore(attempt+1));else activeRenderPending=null;return}
          if(viewerIntent===pending.intent){const box=rendered.getBoundingClientRect(),moved=ownedPositionUnchanged(rendered,pending.top)?0:rendered.scrollTop-pending.top,anchor=pending.anchors.find(anchor=>anchor.node.isConnected);if(pending.head)rendered.scrollTop=0;else if(anchor)rendered.scrollTop+=anchor.node.getBoundingClientRect().top-box.top-anchor.top+moved;else if(moved===0)rendered.scrollTop=pending.tail?rendered.scrollHeight:pending.top;scrollIntent.positioned()}
          activeRenderPosition={stamp:viewerStamp,generation,intent:viewerIntent,part:target,top:rendered.scrollTop,epoch:scrollIntent.ownedPosition?.(rendered)?.epoch};activeRenderPending=null;requestAnimationFrame(()=>inspectBoundary())
        }
        requestAnimationFrame(()=>restore(0));return
      }
    }
    const first=items[0]?.getBoundingClientRect(),last=items.at(-1)?.getBoundingClientRect()
    const contentHeight=first&&last?last.bottom-first.top:0,fill=contentHeight<viewport.clientHeight-4
    const firstMounted=[...(viewport.querySelectorAll?.('.session-record') || [])].find(node=>{const rect=node.getBoundingClientRect();return visibleConversationNode(node,viewport)&&box&&rect.bottom>box.top&&rect.top<box.bottom})
    const anchor=firstMounted?.getAttribute?.('data-offset'),heights={}
    for(const node of viewport.querySelectorAll?.('.session-record') || []){const offset=node.getAttribute('data-offset');if(offset!==null&&Object.keys(heights).length<200)heights[offset.padStart(20,'0')]=visibleConversationNode(node,viewport)?Math.max(0,Math.min(1000000,node.getBoundingClientRect().height)):0}
    const gap=[...(viewport.querySelectorAll?.('[data-history-gap="source"]') || [])].find(node=>{const r=node.getBoundingClientRect();return visibleConversationNode(node,viewport)&&box&&r.bottom>box.top-192&&r.top<box.bottom+192})
    const older=fill?Number(attr('scan-before'))>0:manual&&!!first&&!!box&&first.top>=box.top-192&&Number(attr('before'))>0
    const newer=manual&&!fill&&!!last&&!!box&&last.bottom<=box.bottom+192&&!logicalEnd
    const edge=gap?'gap':older?'older':newer?'newer':''
    const gapBox=gap?.getBoundingClientRect(),gapBefore=gap?.getAttribute('data-gap-before'),gapAfter=gap?.getAttribute('data-gap-after')
    const gapDirection=gapBox&&box&&(box.top+box.height/2-gapBox.top)<gapBox.height/2?'forward':'tail'
    const gapFraction=gapBox&&box?Math.max(0,Math.min(1,((atEnd?box.bottom-1:viewport.scrollTop<=1?box.top+1:box.top+box.height/2)-gapBox.top)/Math.max(1,gapBox.height))):0
    const key=[viewerStamp,generation,attr('before'),attr('after'),edge,gapBefore,gapAfter,gapDirection,Math.floor(gapFraction*100)].join(':')
    if(!edge || key===boundaryKey)return
    const closedGroups=[...(viewport.querySelectorAll?.('[data-group-key]') || [])].filter(node=>!node.open).map(node=>node.getAttribute('data-group-key')).slice(0,200)
    boundaryKey=key;boundaryIntent=viewerIntent;seekStartIntent=null;send({kind:'viewer-boundary',stamp:viewerStamp,generation,edge,fill,anchor:anchor?.padStart(20,'0') || '',fragmentAnchor:gap?'':leaf?.dataset.fragmentPath || '',heights,closedGroups,...(gap?{before:Number(gapBefore),after:Number(gapAfter),fraction:gapFraction,direction:gapDirection}:{})})
  }
  let continuityQueued=false
  const scheduleContinuity=()=>{
    if(continuityQueued)return
    const ownedViewport=document.getElementById?.('session-records')
    if(!ownedViewport?.getBoundingClientRect || !ownedViewport?.querySelectorAll || typeof requestAnimationFrame!=='function')return
    continuityQueued=true
    requestAnimationFrame(()=>{
      continuityQueued=false
      const viewport=document.getElementById?.('session-records')
      if(viewport?.getAttribute?.('data-history-mode')==='active')inspectBoundary()
      else sealed.inspectViewport?.(viewport)
    })
  }
  const continuityObserver=window.MutationObserver?new window.MutationObserver(records=>{
    expireDisclosures()
    if(records.some(record=>record.target?.closest?.('.session-viewer') || [...(record.addedNodes || [])].some(node=>node.matches?.('.session-viewer') || node.querySelector?.('.session-viewer'))))scheduleContinuity()
  }):null
  continuityObserver?.observe(document.body,{subtree:true,childList:true,attributes:true,attributeFilter:['data-history-error','data-history-loading','data-history-before','data-history-after','data-history-scan-before','data-sealed-page-request','data-render-request','data-render-part','open']})
  document.addEventListener('scroll',inspectBoundary,true)
  for(const name of ['wheel','keydown'])document.addEventListener(name,event=>{
    const viewport=document.getElementById?.('session-records')
    if(viewport && (event.target===viewport || viewport.contains?.(event.target)))requestAnimationFrame(()=>inspectBoundary({target:viewport}))
  },true)
  const sealedResizeWindow = viewport => JSON.stringify(['data-sealed-dom-owner','data-sealed-dom-load','data-sealed-page-request','data-render-request','data-render-part'].map(key=>viewport.getAttribute(key)))
  const sealedResizeFollowing = viewport => viewport.getAttribute('data-sealed-dom-owner')===viewerStamp && viewport.getAttribute('data-sealed-dom-load')===String(generation) && viewport.getAttribute('data-following')==='true' && viewport.getAttribute('data-logical-latest')==='true' && !scrollIntent.manual()
  const captureResize = () => {
    const viewport=document.getElementById?.('session-records'),bounds=viewport?.getBoundingClientRect?.()
    if(!bounds || viewport.getAttribute?.('data-viewer-owner')!==viewerStamp)return
    const mode=viewport.getAttribute('data-history-mode')
    resizeSnapshot={stamp:viewerStamp,generation,intent:viewerIntent,mode,sealedWindow:mode==='sealed'?sealedResizeWindow(viewport):null,sealedFollowing:mode==='sealed'&&sealedResizeFollowing(viewport),top:viewport.scrollTop,tail:viewport.scrollHeight-viewport.scrollTop-viewport.clientHeight<=48,
      anchors:[...(viewport.querySelectorAll?.('[data-block-key]') || [])].filter(node=>node.getBoundingClientRect().bottom>bounds.top).slice(0,8).map(node=>({key:node.getAttribute('data-block-key'),top:node.getBoundingClientRect().top-bounds.top}))}
  }
  document.addEventListener('scroll',event=>{if(event.target===document.getElementById?.('session-records'))captureResize()},true)
  const resizeObserver=window.ResizeObserver?new window.ResizeObserver(()=>{
    if(!resizeReady){resizeReady=true;captureResize();return}
    const saved=resizeSnapshot,viewport=resizeViewport
    if(!saved || saved.stamp!==viewerStamp || saved.generation!==generation || saved.intent!==viewerIntent || viewport!==document.getElementById?.('session-records') || saved.mode!==viewport.getAttribute('data-history-mode') || (saved.mode==='sealed'&&saved.sealedWindow!==sealedResizeWindow(viewport)) || !ownedPositionUnchanged(viewport,saved.top))return
    if(saved.tail&&(saved.mode==='sealed'?saved.sealedFollowing&&sealedResizeFollowing(viewport):lastFollowing))positionAtTail(viewport)
    else {
      const bounds=viewport.getBoundingClientRect(),nodes=[...viewport.querySelectorAll('[data-block-key]')]
      for(const anchor of saved.anchors){const node=nodes.find(node=>node.getAttribute('data-block-key')===anchor.key);if(node){viewport.scrollTop+=node.getBoundingClientRect().top-bounds.top-anchor.top;break}}
      scrollIntent.positioned()
    }
    captureResize()
    inspectActiveLatest()
    scheduleContinuity()
    if(viewport?.getAttribute?.('data-history-mode')==='sealed')updateLatestOverlay(viewport,viewport.getAttribute('data-logical-latest')==='true')
  }):null
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
  window.addEventListener?.('pagehide',()=>{disclosureRetiredOwner=viewerStamp;retireDisclosures()})
  app.ports.sessionBridgeCommand.subscribe(command => {
    if(command.disclosureRetry){
      if(command.stamp===viewerStamp&&command.viewerScope===viewerScope&&Number.isSafeInteger(command.generation)&&command.generation>disclosureRetriedGeneration&&(!disclosureRetry||command.generation>disclosureRetry.generation)){
        disclosureRetry={stamp:command.stamp,scope:command.viewerScope,generation:command.generation};retryDisclosures()
      }
      return
    }
    if(command.disclosureAck){
      const ack=command.disclosureAck,action=disclosureActions.get(ack.namespace)
      if(action&&command.stamp===viewerStamp&&action.owner===command.stamp&&action.scope===viewerScope&&ack.scope===action.scope&&ack.request===action.request&&ack.key===action.key&&ack.ancestor===action.ancestor&&ack.open===action.open){
        if(ack.error===true&&!ack.cancelled){action.phase='failed';action.errorConfirmed=true}
        else disclosureActions.delete(action.namespace)
        expireDisclosures()
      }
      return
    }
    if(command.stamp!==viewerStamp||(command.viewerScope&&command.viewerScope!==viewerScope))retireDisclosures()
    scrollIntent.configure(command.viewerScope || command.stamp)
    if(activeReadingPosition?.scope!==readerScopeKey(command.viewerScope || command.stamp))activeReadingPosition=null
    rememberActivePosition(document.getElementById?.('session-records'))
    const preservePresentation=command.preservePresentation ?? command.continueViewer
    if(command.stamp!==viewerStamp)handoffPosition=preservePresentation===true&&command.viewerScope===viewerScope&&presentation?.scope===viewerScope?{owner:command.stamp,scope:viewerScope,intent:viewerIntent,revision:command.generation,publication:command.publication?.request,waiting:command.publication?.pending===true,disclosures:presentation.disclosures}:null
    scheduleContinuity()
    sealed.configure(command.sealed,command.sealedRequest,command.sealedMarkdownAck)
    const downloadStamp = command.downloadStamp ?? command.stamp
    if (downloadStamp !== stamp) { stamp = downloadStamp; void retireSave() }
    if (command.stamp !== viewerStamp) {
      const continuing=command.continueViewer===true && command.viewerScope===viewerScope
      const carrying=preservePresentation===true && command.viewerScope===viewerScope
      viewerStamp = command.stamp
      latestJump=null
      pendingEnd=null
      seekStartIntent=null
      latestHost=null
      followPosition=null
      if(!continuing){framer=null;boundaryKey=''}
      if(!carrying){presentation=null;disclosures.clear()}
      generation = -1
      if(carrying && presentation?.scope===viewerScope){
        const saved=presentation,owner=viewerStamp,scope=viewerScope,intent=viewerIntent,revision=command.generation
        const barrier=handoffPosition
        const queuedViewport=document.getElementById?.('session-records')
        const queuedTop=queuedViewport?.getAttribute?.('data-viewer-owner')===owner?queuedViewport.scrollTop:0
        const restore=attempt=>{
          const retire=(cancelled=false)=>{if(cancelled)cancelHandoffPosition(barrier);else if(handoffPosition===barrier)handoffPosition=null}
          if(owner!==viewerStamp || scope!==viewerScope || generation!==revision || viewerIntent!==intent){retire(true);scheduleContinuity();return}
          const viewport=document.getElementById?.('session-records')
          const unavailable=()=>{if(attempt<8)requestAnimationFrame(()=>restore(attempt+1));else if(handoffPosition===barrier){barrier.disclosures=[];barrier.failed=true;presentation=null;send({kind:'viewer-handoff-error',stamp:owner,generation:revision,publication:barrier.publication ?? 0})}}
          if(!viewport?.querySelectorAll || viewport.getAttribute?.('data-viewer-owner')!==owner){unavailable();return}
          if(Math.abs(viewport.scrollTop-queuedTop)>4){retire(true);scheduleContinuity();return}
          if(barrier?.publication!==undefined&&Number(viewport.getAttribute?.('data-window-publication'))!==barrier.publication){unavailable();return}
          const states=new Map(saved.disclosures)
          for(const node of viewport.querySelectorAll('details')){
            const key=disclosureKey(node)
            if(states.has(key))node.open=states.get(key)
          }
          if(saved.point){
            const leaf=[...viewport.querySelectorAll('[data-fragment-path]')].find(node=>node.dataset.fragmentPath===saved.point.path)
            if(!leaf){unavailable();return}
            const pre=leaf.closest('pre');if(pre&&saved.point.innerScroll!==null)pre.scrollTop=saved.point.innerScroll
            const range=payloadTextRange(leaf,saved.point.offset),rect=range?.getBoundingClientRect()
            if(!rect?.height){unavailable();return}
            viewport.scrollTop+=rect.top-viewport.getBoundingClientRect().top-saved.point.top
          }else viewport.scrollTop=saved.top
          scrollIntent.positioned()
          retire()
          if(command.publication?.pending)send({kind:'viewer-positioned',stamp:owner,generation:revision,publication:command.publication.request,failed:false})
          activeRenderPosition={stamp:owner,generation:revision,intent,part:Number(viewport.getAttribute?.('data-render-part') || 0),top:viewport.scrollTop,epoch:scrollIntent.ownedPosition?.(viewport)?.epoch}
          captureResize()
          inspectActiveLatest()
        }
        requestAnimationFrame(()=>restore(0))
      }else if(carrying&&command.publication?.pending){
        handoffPosition={owner:viewerStamp,scope:viewerScope,revision:command.generation,intent:viewerIntent,publication:command.publication.request,disclosures:[],failed:true}
        send({kind:'viewer-handoff-error',stamp:viewerStamp,generation:command.generation,publication:command.publication.request})
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
    retryDisclosures()
    if(typeof command.viewerError==='boolean')for(const action of disclosureActions.values())if(action.phase==='error'||action.phase==='failed'){
      if(command.viewerError){action.phase='failed';action.errorConfirmed=true}
      else if(action.phase==='failed')action.errorConfirmed=false
    }
    // UI intent is replayed before this command's chunk can advance the model.
    // Exact generation checks for all accepted data remain unchanged.
    for(const action of disclosureActions.values())deliverDisclosure(action)
    expireDisclosures()
    activePublication=command.publication?.pending?command.publication.request:0
    if(command.latest?.pending){
      pendingEnd=null
      if(!latestJump||latestJump.request!==command.latest.request||latestJump.stamp!==viewerStamp)latestJump={request:command.latest.request,stamp:viewerStamp,generation,intent:latestIntent,host:latestHost}
      else latestJump.generation=generation
    }else latestJump=null
    if(command.latest)requestAnimationFrame(()=>requestAnimationFrame(inspectActiveLatest))
    const currentViewport=document.getElementById?.('session-records')
    lastFollowing=command.follow && currentViewport?.getAttribute?.('data-history-mode')!=='sealed'
    if(resizeObserver && currentViewport!==resizeViewport){resizeObserver.disconnect();resizeViewport=currentViewport;resizeSnapshot=null;resizeReady=false;if(currentViewport){captureResize();resizeObserver.observe(currentViewport)}}
    if (command.reset !== null) {
      const anchors=(framer?.readerAnchors() || []).filter(row=>row.offset>=command.reset && row.offset<(command.windowEnd ?? Infinity))
      const predecessor=command.direction==='forward'?framer?.predecessor(command.reset):null
      framer = createRecordFramer(command.reset,command.direction ? {direction:command.direction,aligned:command.aligned,anchors,predecessor,runAnchor:command.runAnchor,runRanges:command.runRanges,knownBefore:command.knownBefore,knownAfter:command.knownAfter} : {})
      reportedGroups=new Map()
      historyTransition = command.historical===true
      boundaryIntent=viewerIntent
    }
    if (!command.chunk) return
    const captured = { stamp:command.stamp, generation:command.generation, intent:viewerIntent }
    try {
      if (!framer || generation !== command.generation) throw new Error('retired_window')
      const bytes = decodeBase64(command.chunk.dataBase64)
      if (bytes.length !== command.chunk.nextOffset-command.chunk.offset) throw new Error('range_length')
      const viewport = document.getElementById('session-records')
      const atTail = !viewport || viewport.scrollHeight - viewport.scrollTop - viewport.clientHeight <= 48 || pendingTail(viewport)
      const previousTop = viewport?.scrollTop || 0
      const sourcePoint=(()=>{
        if(!viewport||command.follow)return null
        const box=viewport.getBoundingClientRect(),y=atTail?box.bottom-1:viewport.scrollTop<=1?box.top+1:box.top+box.height/2
        if([...viewport.querySelectorAll('[data-fragment-path]')].some(node=>{const r=node.getBoundingClientRect();return !node.querySelector('[data-history-gap]')&&visibleConversationNode(node,viewport)&&r.bottom>box.top&&r.top<box.bottom}))return null
        const gap=[...viewport.querySelectorAll('[data-history-gap="source"]')].find(node=>{const r=node.getBoundingClientRect();return visibleConversationNode(node,viewport)&&r.top<=y&&r.bottom>=y})
        if(!gap)return null
        const r=gap.getBoundingClientRect(),before=Number(gap.dataset.gapBefore),after=Number(gap.dataset.gapAfter)
        return {offset:before+(after-before)*(y-r.top)/Math.max(1,r.height),y:y-box.top}
      })()
      const anchors = (historyTransition || !atTail) && viewport?.getBoundingClientRect && viewport?.querySelectorAll ? (()=>{
        const bounds = viewport.getBoundingClientRect()
        const visible = selector => [...viewport.querySelectorAll(selector)].filter(node=>{
          const rect=node.getBoundingClientRect()
          return rect.height>0 && rect.bottom>bounds.top && rect.top<bounds.bottom
        }).slice(0,8).map(node=>{const attribute=node.hasAttribute('data-fragment-path')?'data-fragment-path':node.hasAttribute('data-block-key')?'data-block-key':'data-group-key';return {attribute,key:node.getAttribute(attribute),top:node.getBoundingClientRect().top-bounds.top}})
        return [...visible('[data-fragment-path]'),...visible('[data-block-key]'),...visible('[data-group-key]')]
      })() : []
      for(const node of viewport?.querySelectorAll?.('details') || []){
        const key=disclosureKey(node)
        if(key!==null){disclosures.delete(key);disclosures.set(key,node.open)}
      }
      while(disclosures.size>400)disclosures.delete(disclosures.keys().next().value)
      const result = framer.push(command.chunk.offset,bytes,command.chunk.final)
      const publicationPosition=command.follow?null:rememberActivePosition(viewport)
      // A finite captured end owns publication. A large initial window (or a
      // live burst) advances the bounded byte handshake without painting and
      // following every intermediate 64 KiB history fragment.
      if ((!result.rows.length && (!command.chunk.final || result.pendingBytes>0)) || (!command.chunk.final && (!command.direction || command.direction==='tail'))) { send({kind:'viewer-progress',...captured,through:result.through,pendingBytes:result.pendingBytes,notice:result.notice}); return }
      // Own the pending position before publication can grow the DOM. End
      // transfers existing follow intent; other input still retires it.
      const pendingFollow=command.follow && atTail ? {viewport,...captured,top:previousTop,pending:true} : null
      if(pendingFollow)followPosition=pendingFollow
      const groups={}
      for(const key of new Set([...reportedGroups.keys(),...Object.keys(result.groups || {})])){
        const delta=(result.groups?.[key] || 0)-(reportedGroups.get(key) || 0)
        if(delta)groups[key]=delta
      }
      reportedGroups=new Map(Object.entries(result.groups || {}))
      const revision=++restoreRevision
      send({kind:'viewer',...captured,...result,groups,publication:revision,...(publicationPosition?{position:publicationPosition}:{})})
      const publicationCurrent=()=>revision===restoreRevision&&viewerStamp===captured.stamp&&(generation===captured.generation||activePublication===revision)
      const afterPublication=callback=>{
        if(!command.publication){requestAnimationFrame(callback);return}
        const inspect=attempt=>{
          if(!publicationCurrent())return
          const rendered=document.getElementById?.('session-records'),published=rendered?.getAttribute?.('data-window-publication')
          if(rendered?.getAttribute?.('data-viewer-owner')===captured.stamp&&(published===null||published===undefined||published===String(revision))){callback(rendered);return}
          if(attempt<8)requestAnimationFrame(()=>inspect(attempt+1))
        }
        requestAnimationFrame(()=>inspect(0))
      }
      if(disclosures.size)afterPublication(()=>{
        if(!publicationCurrent()||viewerIntent!==captured.intent)return
        const rendered=document.getElementById?.('session-records')
        for(const node of rendered?.querySelectorAll?.('details') || []){const key=disclosureKey(node);if(disclosures.has(key))node.open=disclosures.get(key)}
      })
      if (pendingFollow) afterPublication(() => {
        if (!lastFollowing || followPosition!==pendingFollow || !publicationCurrent() || viewerIntent !== pendingFollow.intent) return
        const viewport = document.getElementById('session-records')
        if (viewport && (!pendingFollow.viewport || viewport===pendingFollow.viewport) && Math.abs(viewport.scrollTop-pendingFollow.top)<=4) positionAtTail(viewport)
        else followPosition=null
      })
      else if(sourcePoint)afterPublication(()=>{
        if(!publicationCurrent()||viewerIntent!==captured.intent)return
        const rendered=document.getElementById('session-records');if(!rendered||!ownedPositionUnchanged(rendered,previousTop))return
        const box=rendered.getBoundingClientRect(),gap=[...rendered.querySelectorAll('[data-history-gap="source"]')].find(node=>Number(node.dataset.gapBefore)<=sourcePoint.offset&&Number(node.dataset.gapAfter)>sourcePoint.offset)
        let target
        if(gap&&visibleConversationNode(gap,rendered)){const r=gap.getBoundingClientRect();target=r.top+r.height*(sourcePoint.offset-Number(gap.dataset.gapBefore))/(Number(gap.dataset.gapAfter)-Number(gap.dataset.gapBefore))}
        else {const rows=[...rendered.querySelectorAll('.session-record')].filter(node=>visibleConversationNode(node,rendered));const row=rows.findLast(node=>Number(node.dataset.offset)<=sourcePoint.offset)||rows[0];if(row)target=row.getBoundingClientRect().top}
        if(target!==undefined){rendered.scrollTop+=target-box.top-sourcePoint.y;scrollIntent.positioned()}
      })
      else if (anchors.length) afterPublication(()=>{
        if (!publicationCurrent() || viewerIntent !== captured.intent) return
        const viewport=document.getElementById('session-records')
        if (!viewport || !ownedPositionUnchanged(viewport,previousTop)) return
        const bounds=viewport.getBoundingClientRect()
        for(const anchor of anchors) {
          const node=[...viewport.querySelectorAll(`[${anchor.attribute}]`)].find(node=>node.getAttribute(anchor.attribute)===anchor.key)
          if(node && node.getBoundingClientRect().height>0) {
            viewport.scrollTop += node.getBoundingClientRect().top-bounds.top-anchor.top
            break
          }
        }
      })
      else if(historyTransition)afterPublication(()=>{
        if(!publicationCurrent() || viewerIntent!==captured.intent)return
        const rendered=document.getElementById?.('session-records')
        if(rendered){rendered.scrollTop=previousTop;scrollIntent.positioned()}
      })
      historyTransition=false
      const acknowledge=attempt=>{
        if(!publicationCurrent())return
        const rendered=document.getElementById?.('session-records'),published=rendered?.getAttribute?.('data-window-publication'),matching=rendered?.getAttribute?.('data-viewer-owner')===captured.stamp&&(published===null||published===undefined||published===String(revision))
        if(matching||attempt>=8){send({kind:'viewer-positioned',stamp:captured.stamp,generation:Number(rendered?.getAttribute?.('data-viewer-generation') ?? generation),publication:revision,failed:!matching});if(matching){captureResize();inspectBoundary()}return}
        requestAnimationFrame(()=>acknowledge(attempt+1))
      }
      if(command.publication)requestAnimationFrame(()=>requestAnimationFrame(()=>acknowledge(0)))
      else if(command.direction)requestAnimationFrame(()=>requestAnimationFrame(()=>{if(publicationCurrent()){captureResize();inspectBoundary()}}))
    } catch {
      send({ kind:'viewer-error',...captured })
    }
  })
  return {retireAuthority:()=>{disclosureRetiredOwner=viewerStamp;retireDisclosures();sealed.retireAuthority()}}
}
