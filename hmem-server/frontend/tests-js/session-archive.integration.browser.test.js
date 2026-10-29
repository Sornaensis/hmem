import test from 'node:test'
import assert from 'node:assert/strict'
import {spawn} from 'node:child_process'
import {createInterface} from 'node:readline'
import {createServer} from 'node:net'
import {mkdir,realpath,lstat,readFile,writeFile,readdir,rm,copyFile} from 'node:fs/promises'
import {resolve,relative,join,isAbsolute,sep} from 'node:path'
import {createHash,randomUUID} from 'node:crypto'
import {EventEmitter} from 'node:events'
import {PassThrough,Writable} from 'node:stream'
import {createReadStream,createWriteStream} from 'node:fs'
import {createGunzip,createInflateRaw,crc32} from 'node:zlib'
import {Readable,Transform} from 'node:stream'
import {pipeline,finished} from 'node:stream/promises'
import {chromium} from '@playwright/test'

const repo=resolve(import.meta.dirname,'../../..')
const taskRoot=join(repo,'.scratch','sealed-archives-4a40c671')
const scratchCeiling=8*1024**3
const smokeAllowance=1024**3
const integrationCases=Object.freeze({
  'integration-1MiB':{mebibytes:1,compatibility:false,timeoutMs:180000,internalMs:175000,reservedBytes:1024**3},
  'integration-100MiB':{mebibytes:100,compatibility:true,timeoutMs:600000,internalMs:595000,reservedBytes:2*1024**3},
  'integration-1024MiB':{mebibytes:1024,compatibility:true,timeoutMs:1200000,internalMs:1195000,reservedBytes:6*1024**3}
})
const retainedOwners=new Map()
const outputGuards=new WeakMap()
const task='4a40c671-2d77-4cca-be24-c7c25ef72337'
const pause=ms=>new Promise(done=>setTimeout(done,ms))
const inside=(parent,path)=>{const part=relative(parent,path);return part===''||(!part.startsWith('..'+sep)&&part!=='..'&&!isAbsolute(part))}
async function deadline(work,ms,label){let timer;try{return await Promise.race([work,new Promise((_,reject)=>{timer=setTimeout(()=>reject(Error(label+' deadline; actual capability retained')),ms)})])}finally{clearTimeout(timer)}}
async function unusedPort(){const listener=createServer();await new Promise((yes,no)=>{listener.once('error',no);listener.listen(0,'127.0.0.1',yes)});const port=listener.address().port;await new Promise((yes,no)=>listener.close(error=>error?no(error):yes()));return port}
async function digestFile(path){const {createReadStream}=await import('node:fs');const hash=createHash('sha256');for await(const bytes of createReadStream(path,{highWaterMark:65536}))hash.update(bytes);return hash.digest('hex')}

function boundedText(value,bytes=2048){
  let result=Buffer.from(String(value??'').replace(/\b(Bearer\s+)([^\s,;]+)/gi,'$1<redacted>').replace(/\b(password|secret|token|authorization)\s*[:=]\s*([^\s,;]+)/gi,'$1=<redacted>')).subarray(0,bytes).toString('utf8')
  while(Buffer.byteLength(result)>bytes)result=result.slice(0,-1)
  return result
}
function errorTree(error,budget={left:8},depth=0){
  if(!error||budget.left--<=0)return null
  const result={name:boundedText(error.name||'Error',64),message:boundedText(error.message||error,1024)}
  if(depth<3){
    if(error.cause)result.cause=errorTree(error.cause,budget,depth+1)
    if(Array.isArray(error.errors))result.errors=error.errors.slice(0,8).map(value=>errorTree(value,budget,depth+1)).filter(Boolean)
  }
  return result
}
async function writePacket(output,packet,timeoutMs=2000){
  const line=JSON.stringify(packet)+'\n';assert.ok(Buffer.byteLength(line)<=24576,'bounded operator packet')
  // A diagnostic write cannot replace an ownership failure or terminate the
  // supervisor while its native child is retained.
  let guard=outputGuards.get(output)
  if(!guard){guard={error:null};outputGuards.set(output,guard);output.on('error',error=>{guard.error=error})}
  await deadline(new Promise((yes,no)=>output.write(line,error=>error?no(error):yes())),timeoutMs,'operator diagnostic write')
  if(guard.error)throw guard.error
}
function failurePacket(run,owner,errors){const budget={left:8};return {archive_qualification_diagnostic:true,task,run:run?.id??null,phase:owner?.status()??{phase:'before-native-owner'},errors:errors.filter(Boolean).slice(0,4).map(value=>errorTree(value,budget)).filter(Boolean),stderr:boundedText(owner?.diagnostic(),8192),integration:run?.integrationDiagnostic??null,disposition:'failed; same owner remains reachable until actual cleanup; diagnostic expires at issue closure'}}

// With process isolation disabled these are the actual exec session's stdin
// and stdout, and these closures retain the actual child/pipes. No forwarding
// through a test reporter, PID reopening, or unregistered command is used.
function openOwnerRoute(input,output,owners=retainedOwners){
  let buffer=Buffer.alloc(0),discard=false,busy=false,closed=false,inputOpen=true,lastOutputError,ended,queued=0,retireRequested=false
  const inputEnded=new Promise(done=>{ended=done})
  async function reply(packet){try{await writePacket(output,packet);return true}catch(error){lastOutputError=error;return false}}
  async function receive(bytes){
    let request
    try{
      request=JSON.parse(bytes.toString('utf8'))
      assert.deepEqual(Object.keys(request).sort(),request.command==='admit'?['ack_id','command','id','kind','run','task']:['command','id','kind','run','task'])
      assert.equal(request.kind,'archive-qualification-owner');assert.equal(request.task,task)
      assert.ok(Number.isSafeInteger(request.id)&&request.id>0)
      assert.match(request.run,/^[a-f0-9]{8}-[a-f0-9]{4}-[a-f0-9]{4}-[a-f0-9]{4}-[a-f0-9]{12}$/)
      assert.ok(['status','admit','retry-cleanup','close'].includes(request.command))
      const owner=owners.get(request.run);assert.ok(owner,'unknown owned run')
      if(request.command==='status'){
        const sent=await reply({archive_qualification_owner:true,id:request.id,run:request.run,status:owner.status(),outputError:errorTree(lastOutputError)})
        if(sent)owner.acknowledgeStatus?.(request.id)
        return
      }
      if(request.command==='admit'){
        assert.ok(inputOpen,'operator transport closed before admission');assert.equal(typeof owner.admit,'function','run already admitted')
        owner.admit(request.ack_id)
        return await reply({archive_qualification_owner:true,id:request.id,run:request.run,admitted:true})
      }
      assert.equal(busy,false,'one actual-owner cleanup request at a time');busy=true
      try{
        if(request.command==='retry-cleanup')assert.equal(owner.status().recoveryState,'retained','native has not reported retained cleanup')
        let failure
        try{await owner.close()}catch(error){failure=error}
        // close may remove its registry entry. Keep this exact owner local
        // through the terminal reply, including diagnostics arriving after the
        // test body's earlier failure/cleanup attempt.
        const terminal=owner.status()
        const sent=await reply({archive_qualification_owner:true,id:request.id,run:request.run,status:terminal,failure:errorTree(failure),diagnostic:{stderr:boundedText(owner.diagnostic?.(),8192)}})
        const receipt=terminal.cleanupReceipt
        if(sent&&receipt?.actualHandleJoined===true&&receipt.pipesClosed===true&&terminal.pendingControls===0&&owners.size===0)retireRequested=true
      }finally{busy=false}
    }catch(error){await reply({archive_qualification_owner:true,id:Number.isSafeInteger(request?.id)?request.id:null,error:errorTree(error)})}
  }
  const data=chunk=>{
    const incoming=Buffer.from(chunk)
    for(let at=0;at<incoming.length;){
      const newline=incoming.indexOf(10,at),end=newline<0?incoming.length:newline
      if(!discard){
        if(buffer.length+end-at>8192){buffer=Buffer.alloc(0);discard=true;lastOutputError=Error('owner control frame exceeds 8192 bytes')}
        else buffer=Buffer.concat([buffer,incoming.subarray(at,end)])
      }
      if(newline<0)break
      if(!discard){
        if(queued<4){queued++;void receive(buffer).finally(()=>{queued--;if(retireRequested)retireRoute()})}
        else lastOutputError=Error('owner control admission full (four frames)')
      }
      buffer=Buffer.alloc(0);discard=false;at=newline+1
    }
  }
  const end=error=>{inputOpen=false;ended(error||Error('operator stdin EOF'))}
  function retireRoute(){
    if(owners.size)return false
    retireRequested=true
    if(queued||busy)return false
    if(!closed){input.off('data',data);input.off('end',end);input.off('error',end);input.pause();input.unref?.();closed=true}
    return true
  }
  input.on('data',data)
  input.once('end',end);input.once('error',end)
  return {inputEnded,isInputOpen:()=>inputOpen&&!input.destroyed&&!input.readableEnded,async announce(run){assert.ok(await reply({archive_qualification_owner:true,run:run.id,task,commands:['status','admit','retry-cleanup','close'],scope:'actual same-process stdin; status ACK then separate admit required before files/native'}),'operator announcement failed')},close:retireRoute,isOpen:()=>!closed}
}

function requireReachableOwnerRoute(){
  assert.ok(process.execArgv.includes('--test-isolation=none'),'native admission requires --test-isolation=none so exec stdin reaches the actual owner')
  assert.ok(process.stdin.isTTY===true&&process.stdin.readable&&!process.stdin.destroyed,'native admission requires an actual open TTY operator stdin')
  return openOwnerRoute(process.stdin,process.stdout)
}

function preAdmission(runId,owners=retainedOwners){
  let statusAck=null,admitted=false,resolveAdmission
  const admission=new Promise(done=>{resolveAdmission=done})
  const placeholder={status:()=>({phase:'pre-admission',nativeCreated:false,filesCreated:false,statusAck,admitted}),acknowledgeStatus(id){statusAck=id},admit(ack){assert.ok(Number.isSafeInteger(ack)&&ack===statusAck,'admit must bind the actually written status ACK');assert.equal(admitted,false);admitted=true;resolveAdmission({run:runId,statusAck:ack,transport:'actual TTY stdin round trip',nativeCreated:false,filesCreated:false})},async close(){throw Error('pre-admission run has no native child')}}
  owners.set(runId,placeholder)
  return {admission,placeholder}
}

// One cutoff for this smoke's foreground actions. The 35-second native close
// and five-second contained deletion retain their own cleanup reserve.
function archiveActionScope(endsAt,testSignal){
  const cutoff=endsAt-40000,abort=new AbortController()
  const stop=()=>abort.abort(testSignal?.reason||Error('test cancelled'))
  const timer=setTimeout(()=>abort.abort(Error('archive action cutoff; cleanup reserve begins')),Math.max(0,cutoff-performance.now()))
  if(testSignal?.aborted)stop();else testSignal?.addEventListener('abort',stop,{once:true})
  function remaining(maximum){
    abort.signal.throwIfAborted()
    const left=Math.floor(cutoff-performance.now())
    if(left<1){abort.abort(Error('archive action cutoff; cleanup reserve begins'));abort.signal.throwIfAborted()}
    return Math.min(maximum,left)
  }
  return {signal:abort.signal,remaining,
    control(owner,command,fields={}){return owner.control(command,fields,remaining(10000),abort.signal)},
    async fetchJson(url,fetchRequest=fetch,observation){
      const local=new AbortController(),cancel=()=>local.abort(abort.signal.reason)
      const timeout=setTimeout(()=>local.abort(Error('ordinary request deadline')),remaining(5000))
      abort.signal.addEventListener('abort',cancel,{once:true})
      let response,primary
      try{
        response=await fetchRequest(url,{signal:local.signal})
        if(observation){observation.headersAt=performance.now();observation.requestId=response.headers.get('X-Request-Id')}
        assert.equal(response.status,200)
        const value=await response.json()
        if(observation)observation.bodyAt=performance.now()
        return value
      }catch(error){primary=error;throw error}
      finally{
        local.abort();clearTimeout(timeout);abort.signal.removeEventListener('abort',cancel)
        // Await the actual read/cancellation promise; do not race and discard
        // an owned network body while releasing its native server underneath.
        if(response?.body&&!response.bodyUsed)try{await response.body.cancel()}catch(cleanup){throw primary?new AggregateError([primary,cleanup],'ordinary request and body cancellation failed'):cleanup}
      }
    },
    async pause(ms){
      const duration=Math.min(ms,remaining(ms))
      await new Promise((yes,no)=>{
        const cancelled=()=>{clearTimeout(wait);abort.signal.removeEventListener('abort',cancelled);no(abort.signal.reason)}
        const wait=setTimeout(()=>{abort.signal.removeEventListener('abort',cancelled);yes()},duration)
        abort.signal.addEventListener('abort',cancelled,{once:true})
      })
    },
    dispose(){clearTimeout(timer);testSignal?.removeEventListener('abort',stop)}
  }
}

// This is a configured contained-data ceiling, not an OS free-space guarantee.
// Successful runs retain only the aggregate receipt; failed cleanup preserves
// the exact owned run and never traverses an unrelated scratch tree.
async function scratchBytes(path){
  const root=await realpath(path);assert.ok(inside(await realpath(taskRoot),root))
  let bytes=0,entries=0;const expires=performance.now()+5000
  async function visit(current){for(const entry of await readdir(current,{withFileTypes:true})){
    assert.ok(performance.now()<expires,'finite scratch inventory deadline')
    assert.ok(++entries<=100000,'bounded scratch inventory');const next=join(current,entry.name),info=await lstat(next)
    assert.ok(!info.isSymbolicLink(),'qualification scratch cannot contain symbolic links')
    if(info.isDirectory())await visit(next);else {assert.ok(info.isFile());bytes+=info.size;assert.ok(bytes<=scratchCeiling,'configured 8 GiB Task8 scratch ceiling')}
  }}
  await visit(root);return bytes
}
async function admitRun(allowance){
  const used=await scratchBytes(taskRoot)
  assert.ok(used+allowance<=scratchCeiling,'Task8 retained inputs plus configured new-run allowance exceed 8 GiB; no launch admitted')
  return {usedBytes:used,reservedNewRunBytes:allowance,configuredCeilingBytes:scratchCeiling}
}

async function createRun(id,endsAt,allowance=smokeAllowance){
  assert.equal(process.platform,'win32','qualified platform is Windows')
  assert.equal(await realpath(repo),repo,'exact repository working directory')
  await mkdir(taskRoot,{recursive:true});assert.equal(await realpath(taskRoot),taskRoot,'exact contained Task8 root')
  const admission=await admitRun(allowance)
  const path=join(taskRoot,'run-'+id)
  await mkdir(path);await writeFile(join(path,'owner.json'),JSON.stringify({task,id,owner:'Task8 developer',expires:'aggregate approval/task closure; unresolved diagnostic at issue closure'}),{flag:'wx'})
  for(const name of ['tmp','home','config','appdata','localappdata'])await mkdir(join(path,name))
  return {id,path,admission,endsAt,allowance,setupEndsAt:Math.min(endsAt-40000,performance.now()+180000),nativeOwner:null,async dispose(){
    assert.equal(retainedOwners.has(id),false,'unconfirmed native cleanup retains the exact run and actual owner')
    const actual=await realpath(path),parent=await realpath(taskRoot)
    assert.ok(actual===path&&inside(parent,actual)&&actual!==parent,'verified current-run cleanup boundary')
    const owner=JSON.parse(await readFile(join(actual,'owner.json'),'utf8'));assert.equal(owner.task,task);assert.equal(owner.id,id)
    const remaining=Math.floor(Math.min(5000,endsAt-performance.now()))
    assert.ok(remaining>0,'absolute current-run deletion reserve expired; owned run retained')
    await deadline(rm(actual,{recursive:true,force:false,maxRetries:0}),remaining,'contained run cleanup')
  }}
}

async function startHarness(run){
  const configured=process.env.HMEM_ARCHIVE_INTEGRATION_HARNESS
  assert.ok(configured&&isAbsolute(configured),'explicit compiled hmem-test-harness path is required')
  const executable=await realpath(configured);assert.ok(inside(repo,executable),'harness must be a repository build input')
  assert.equal((await lstat(executable)).isFile(),true)
  assert.equal(executable.split(sep).at(-1),'hmem-test-harness.exe')
  const executableSha256=await digestFile(executable)
  assert.match(process.env.HMEM_ARCHIVE_INTEGRATION_HARNESS_SHA256||'',/^[a-f0-9]{64}$/,'frozen executable hash is required')
  assert.equal(executableSha256,process.env.HMEM_ARCHIVE_INTEGRATION_HARNESS_SHA256,'frozen harness input')
  const allowed=new Set(['PATH','SYSTEMROOT','SYSTEMDRIVE','WINDIR','COMSPEC','PATHEXT','LANG','NUMBER_OF_PROCESSORS'])
  const environment=Object.fromEntries(Object.entries(process.env).filter(([name])=>allowed.has(name.toUpperCase())))
  Object.assign(environment,{TEMP:join(run.path,'tmp'),TMP:join(run.path,'tmp'),HOME:join(run.path,'home'),USERPROFILE:join(run.path,'home'),APPDATA:join(run.path,'appdata'),LOCALAPPDATA:join(run.path,'localappdata'),HMEM_CONFIG_DIR:join(run.path,'config'),HMEM_TEST_REPO_ROOT:repo})
  const port=await unusedPort(),origin='http://127.0.0.1:'+port
  // Re-inventory the whole task root immediately before admission. Unknown
  // files and retained failed runs count; none is removed to make room.
  run.admission=await admitRun(run.allowance??smokeAllowance)
  assert.equal(run.operatorIsOpen(),true,'operator EOF/closed transport refuses native admission')
  run.actions.remaining(1)
  assert.ok(run.setupEndsAt===undefined||performance.now()<run.setupEndsAt,'finite setup/append admission before native launch')
  const child=spawn(executable,['local','--interactive','--port',String(port),'--archive-qualification-root',taskRoot,'+RTS','-N2','-RTS'],{cwd:repo,env:environment,windowsHide:true,stdio:['pipe','pipe','pipe']})
  const owner=ownHarnessChild(child,run,origin,executableSha256)
  try{
    const startupBudget=run.actions.remaining(Math.min(120000,(run.setupEndsAt??Infinity)-performance.now()))
    assert.ok(startupBudget>0,'overall smoke deadline reserves cleanup')
    let onAbort
    try{
      const cancelled=new Promise((_,no)=>{onAbort=()=>no(run.actions.signal.reason);run.actions.signal.addEventListener('abort',onAbort,{once:true})})
      const startup=await deadline(Promise.race([owner.startup,run.operatorClosed.then(error=>{throw error}),cancelled]),startupBudget,'owned sandbox startup')
      assert.equal(startup.kind,'ready','native startup transition: '+startup.kind)
    }finally{run.actions.signal.removeEventListener('abort',onAbort)}
    assert.equal(owner.coordinates.url,origin);assert.ok(inside(run.path,await realpath(owner.coordinates.root)))
    return owner
  }catch(primary){
    const failure=new AggregateError([primary],'Harness setup failed; actual registered owner remains available for same-handle cleanup')
    try{await writePacket(process.stderr,failurePacket(run,owner,[failure]))}catch(outputError){failure.errors.push(outputError)}
    throw failure
  }
}

function ownHarnessChild(child,run,origin,executableSha256,owners=retainedOwners){
  const pending=new Map(),coordinates={},terminalCauses=[];let sequence=0,stderr='',transportError,closedResult,closing,cleanupReceipt,recoveryState,recoveryReceipt,terminalError,notifyRetained
  let currentPhase='native-spawn-awaiting-output',phaseAtMs=performance.now(),startupDone
  const nativePhases=[]
  const startup=new Promise(done=>{startupDone=done})
  function phase(name){currentPhase=name;phaseAtMs=performance.now()}
  const retained=new Promise(done=>{notifyRetained=done})
  const closed=new Promise(done=>child.once('close',(code,signal)=>{closedResult={code,signal};phase('native-closed');startupDone({kind:'closed'});for(const request of pending.values())request.reject(Error('owned harness closed'));pending.clear();done(closedResult)}))
  child.on('error',error=>{transportError=error;phase('native-transport-error');startupDone({kind:'transport-error'})})
  child.stderr.on('data',bytes=>{stderr=Buffer.concat([Buffer.from(stderr),bytes]).subarray(-8192).toString('utf8')})
  const lines=createInterface({input:child.stdout})
  lines.on('line',line=>{
    if(Buffer.byteLength(line)>16384){transportError=Error('harness response frame limit');startupDone({kind:'transport-error'});return}
    if(line.startsWith('{')){try{
      const response=JSON.parse(line)
      if(response.qualification_phase){assert.ok(typeof response.qualification_phase==='string'&&response.qualification_phase.length<=64);phase(response.qualification_phase);nativePhases.push({phase:currentPhase,monotonicNs:boundedText(response.monotonic_ns,32),receivedAtMs:phaseAtMs});if(nativePhases.length>16)nativePhases.shift()}
      if(response.qualification_recovery){recoveryState=response.state;if(recoveryState==='retained'){phase('native-recovery-retained');notifyRetained();startupDone({kind:'retained'})}}
      if(response.qualification_control){const request=pending.get(response.id);if(request){if(request.command==='retry-cleanup'&&response.result?.cleanup_complete===true)recoveryReceipt=response.result;pending.delete(response.id);request.resolve(response.result)}}
    }catch{transportError=Error('invalid harness control frame');startupDone({kind:'transport-error'})}}
    for(const [label,key] of [['Sandbox root:','root'],['Static dir  :','static'],['Server URL  :','url']])if(line.startsWith(label))coordinates[key]=line.slice(label.length).trim()
    if(line.startsWith('Press Enter')){coordinates.ready=true;phase('ready-banner');startupDone({kind:'ready'})}
  })
  async function control(command,fields={},timeoutMs=command==='retry-cleanup'?25000:10000,signal){
    const cleanup=command==='stop'||command==='retry-cleanup'
    assert.ok(!transportError&&!closedResult&&(cleanup?pending.size<3&&![...pending.values()].some(request=>request.command===command):pending.size===0),'bounded owned control admission')
    signal?.throwIfAborted()
    assert.ok(Number.isFinite(timeoutMs)&&timeoutMs>=1&&timeoutMs<=(command==='retry-cleanup'?25000:10000),'bounded private control deadline')
    const id=++sequence
    return await new Promise((resolve,reject)=>{
      let reported=false,timer
      const removeSignal=()=>{clearTimeout(timer);signal?.removeEventListener('abort',cancelled)}
      const actual=(failure,result)=>{pending.delete(id);removeSignal();if(!reported){reported=true;failure?reject(failure):resolve(result)}}
      const logical=failure=>{removeSignal();if(!reported){reported=true;reject(failure)}}
      const cancelled=()=>logical(signal.reason)
      const entry={command,resolve:result=>actual(null,result),reject:failure=>actual(failure)}
      pending.set(id,entry)
      timer=setTimeout(()=>logical(Error('harness '+command+' deadline; actual request retained until response/native close')),timeoutMs)
      signal?.addEventListener('abort',cancelled,{once:true})
      child.stdin.write(JSON.stringify({id,command,...fields})+'\n',error=>{if(error)logical(error)})
      // A logical timeout/abort deliberately leaves entry in this same owner.
      // Only its response or confirmed native close clears that actual work.
    })
  }
  async function close(expires=performance.now()+35000){
    if(cleanupReceipt){if(terminalError)throw terminalError;return cleanupReceipt}
    if(closing)return closing
    closing=(async()=>{
      const failures=[]
      const closeControl=command=>control(command,{},Math.max(1,Math.min(command==='retry-cleanup'?25000:10000,expires-performance.now())))
      // Start the actual-close wait regardless of whether stop can be sent or
      // acknowledged. A failed stop does not discard a later joined receipt.
      const joining=deadline(closed,Math.max(1,expires-performance.now()),'owned harness shutdown').then(value=>({value}),error=>({error}))
      const retryAtStart=recoveryState==='retained'
      if(!closedResult)try{await closeControl(retryAtStart?'retry-cleanup':'stop')}catch(error){failures.push(error)}
      if(!retryAtStart&&!closedResult&&!recoveryReceipt){
        // Drain failure may be reported after the stop acknowledgement. React
        // to that actual transition, or closure/deadline, without polling.
        const transition=await Promise.race([retained.then(()=>({retained:true})),joining])
        if(transition.retained&&!closedResult)try{await closeControl('retry-cleanup')}catch(error){failures.push(error)}
      }
      const joined=await joining
      if(joined.error)failures.push(joined.error)
      else try{
        const result=joined.value
        assert.equal(result.signal,null)
        assert.ok(result.code===0||recoveryReceipt?.cleanup_complete,'nonzero exit requires explicit same-lifetime recovery receipt')
        assert.ok(child.stdin.destroyed&&child.stdout.destroyed&&child.stderr.destroyed,'actual owned pipes closed')
        if(coordinates.root)await assert.rejects(lstat(coordinates.root),{code:'ENOENT'},'owned PostgreSQL sandbox removed only after joins')
        cleanupReceipt={actualHandleJoined:true,pipesClosed:true,sandboxRemoved:coordinates.root?true:null,recovered:!!recoveryReceipt,scope:coordinates.root?'observed sandbox root removed after native closure':'native closure only; no sandbox root observed'}
        owners.delete(run.id)
        if(result.code!==0)failures.push(Error('original native failure preserved after verified recovery'))
        if(transportError)failures.push(transportError)
      }catch(error){failures.push(error)}
      for(const failure of failures)if(terminalCauses.length<8)terminalCauses.push(failure)
      if(terminalCauses.length){
        terminalError=new AggregateError([...terminalCauses],'Native stop/transport/join/cleanup causes retained; retry same registered owner')
        terminalError.cleanupReceipt=cleanupReceipt
        throw terminalError
      }
      return cleanupReceipt
    })()
    try{return await closing}finally{closing=null}
  }
  // Register closure before startup assertions so even a setup failure uses
  // the same native handle; no process-tree kill or later PID reopening.
  const owner={child,pipes:{stdin:child.stdin,stdout:child.stdout,stderr:child.stderr},closed,startup,control,close,origin,executableSha256,coordinates,diagnostic:()=>stderr,status:()=>({phase:currentPhase,phaseAtMs,nativePhases:[...nativePhases],nativePid:child.pid??null,closed:closedResult??null,recoveryState:recoveryState??null,cleanupReceipt:cleanupReceipt??null,terminalError:errorTree(terminalError,{left:3}),pendingControls:pending.size,actualOwnerRetained:owners.get(run.id)===owner})}
  run.nativeOwner=owner;owners.set(run.id,owner)
  return owner
}

// The original append arrays are regenerated one block at a time. This is the
// qualified Task6 raw recipe; the server, rather than Node, creates the gzip.
function rawRecipeBlock(first,count=64){
  let seed=0x51a7
  const pattern=Array.from({length:128},()=>{seed^=seed<<13;seed^=seed>>>17;seed^=seed<<5;return String.fromCharCode(65+(seed>>>0)%26)}).join('')
  const value={type:'response_item',timestamp:'2026-10-28T00:00:00Z',payload:{type:'message',role:'assistant',content:[{type:'output_text',text:'row 00000000 λ😀 '+pattern}]}}
  value.payload.content[0].text+='A'.repeat(1024-Buffer.byteLength(JSON.stringify(value)+'\r\n'))
  const template=Buffer.from(JSON.stringify(value)+'\r\n'),counter=template.indexOf('00000000')
  assert.equal(template.length,1024);assert.ok(count<=64)
  const block=Buffer.alloc(count*1024)
  for(let row=0;row<count;row++){template.copy(block,row*1024);block.write((first+row).toString(16).padStart(8,'0'),row*1024+counter,8,'ascii')}
  return block
}

async function integratedRequest(actions,url,{method='GET',body,status=200,timeoutMs=10000,externalSignal}={},consume=response=>response.json()){
  const abort=new AbortController(),cancel=()=>abort.abort(actions.signal.reason)
  const externalCancel=()=>abort.abort(externalSignal.reason)
  externalSignal?.throwIfAborted()
  assert.ok(timeoutMs>=1&&timeoutMs<=180000,'finite named integration HTTP operation')
  const timer=setTimeout(()=>abort.abort(Error('integrated HTTP request deadline')),actions.remaining(timeoutMs))
  actions.signal.addEventListener('abort',cancel,{once:true})
  externalSignal?.addEventListener('abort',externalCancel,{once:true})
  let response,primary
  try{
    response=await fetch(url,{method,body:body===undefined?undefined:JSON.stringify(body),headers:body===undefined?{}:{'Content-Type':'application/json'},signal:abort.signal})
    assert.ok((Array.isArray(status)?status:[status]).includes(response.status),method+' '+new URL(url).pathname+' returned '+response.status)
    return await consume(response,abort.signal)
  }catch(error){primary=error;throw error}
  finally{
    abort.abort();clearTimeout(timer);actions.signal.removeEventListener('abort',cancel)
    externalSignal?.removeEventListener('abort',externalCancel)
    if(response?.body&&!response.bodyUsed)try{await response.body.cancel()}catch(error){throw primary?new AggregateError([primary,error],'integrated HTTP and body cleanup failed'):error}
  }
}

function remainingView(actions,startedAt,budgetMs,now=performance.now()){
  const remaining=Math.floor(startedAt+budgetMs-now)
  assert.ok(remaining>0,'unchanged verified-view product deadline exhausted')
  return actions.remaining(remaining)
}

// Reuse the established persistent-context ownership pattern without its
// Task6-only profile path, mocked API routes, or fixture HTTP server.
function attachIntegratedBrowser(harness,run,actions){
  let launching,context,browser,closing,joined=false
  const observations=new Set()
  const nativeClose=harness.close,nativeStatus=harness.status
  harness.status=()=>({...nativeStatus(),browser:{launchPending:!!launching&&!context&&!joined,contextAcquired:!!context,joined,pendingObservations:observations.size}})
  harness.close=async(expires=performance.now()+35000)=>{
    if(launching&&!joined){
      const browserExpires=Math.min(expires,performance.now()+5000)
      // Keep the original launch capability reachable. No dependent teardown
      // or profile deletion follows a timed-out browser join.
      await deadline(launching.then(()=>{},()=>{}),Math.max(1,browserExpires-performance.now()),'actual persistent launch join')
      if(context){
        if(!closing){closing=context.close();closing.catch(()=>{closing=null})}
        await deadline(closing,Math.max(1,browserExpires-performance.now()),'actual persistent browser close')
        assert.equal(browser.isConnected(),false,'persistent context close joins its owned browser')
        await deadline(Promise.allSettled([...observations]),Math.max(1,browserExpires-performance.now()),'actual diagnostic request join after browser close')
      }
      joined=true
    }
    return await nativeClose(expires)
  }
  return {observe(pending){observations.add(pending);pending.then(()=>observations.delete(pending),()=>observations.delete(pending));return pending},async open(){
    const evidence=JSON.parse(await readFile(resolve('perf/session-archive.qualification.json'),'utf8')),image=evidence.browserImage
    assert.equal(await digestFile(image.path),image.sha256,'previously qualified browser image')
    const profile=join(run.path,'browser-profile');await mkdir(profile)
    assert.equal(await realpath(profile),profile)
    launching=chromium.launchPersistentContext(profile,{executablePath:image.path,headless:true,viewport:{width:1440,height:1000},timezoneId:'UTC',timeout:actions.remaining(15000)})
    launching=launching.then(value=>{context=value;browser=value.browser();return value})
    context=await launching
    context.setDefaultTimeout(actions.remaining(10000))
    const page=context.pages()[0]||await context.newPage()
    await page.addInitScript(()=>{
      window.HMEM_CONFIG={authMode:'local',apiUrl:location.origin,wsUrl:location.origin.replace(/^http/,'ws')+'/api/v1/ws',authTokenStorage:'memory'}
      const state=window.__integratedArchive={documentNonce:crypto.randomUUID(),events:[],bytes:0,complete:false,verified:false,rowsBeforeComplete:false,rowsBeforeVerified:false,held:false}
      const note=(kind,extra={})=>{if(state.events.length<64)state.events.push({kind,at:performance.now(),...extra})}
      const OriginalWorker=window.Worker
      window.Worker=class extends OriginalWorker{constructor(...args){super(...args);this.addEventListener('message',({data})=>{
        if(data.kind==='progress'&&['decoding','ready'].includes(data.phase)){
          if(data.phase==='ready')state.verified=true
          note('worker-'+data.phase,{compressedBytes:data.compressedBytes,rawBytes:data.rawBytes})
        }
      })}}
      const originalFetch=window.fetch
      window.fetch=async function(url,options){
        const response=await originalFetch.call(this,url,options)
        if(!/\/archive\/[^/]+\/content$/.test(new URL(url,location.href).pathname)||response.status!==200)return response
        state.bytes=0;state.complete=false;state.verified=false
        const reader=response.body.getReader()
        const body=new ReadableStream({async pull(controller){try{
          const part=await reader.read()
          if(part.done){state.held=true;note('content-eof-held');await new Promise(done=>{state.release=done});state.held=false;state.complete=true;note('content-complete',{bytes:state.bytes});controller.close();reader.releaseLock()}
          else{state.bytes+=part.value.byteLength;controller.enqueue(part.value)}
        }catch(error){controller.error(error)}},async cancel(reason){state.release?.();await reader.cancel(reason);reader.releaseLock()}})
        return new Response(body,{status:response.status,statusText:response.statusText,headers:response.headers})
      }
      new MutationObserver(()=>{
        if(document.querySelector('.session-sealed-viewer .session-record-text')){
          if(!state.complete)state.rowsBeforeComplete=true
          if(!state.verified)state.rowsBeforeVerified=true
          if(!state.events.some(event=>event.kind==='rows'))note('rows')
        }
      }).observe(document,{subtree:true,childList:true,characterData:true})
    })
    return {page,image:{sha256:image.sha256,path:image.path,profileMode:'task-owned persistent normal profile'},context}
  }}
}

async function bindServedAssets(harness,run,actions){
  const destination=await realpath(harness.coordinates.static)
  assert.ok(inside(await realpath(harness.coordinates.root),destination)&&inside(run.path,destination))
  assert.deepEqual(await readdir(destination),[],'owned sandbox static directory starts empty')
  const source=join(repo,'hmem-server','static'),assets=await readdir(join(source,'assets'))
  assert.ok(assets.length>0&&assets.length<=128)
  await mkdir(join(destination,'assets'))
  const paths=['index.html','hmem-runtime-config.js',...assets.filter(name=>/^[a-zA-Z0-9_.-]+\.(js|css)$/.test(name)).map(name=>'assets/'+name)]
  const receipt=[]
  for(const path of paths){
    actions.remaining(1)
    assert.equal((await lstat(join(source,path))).isFile(),true)
    await copyFile(join(source,path),join(destination,path))
    const expected=await digestFile(join(source,path))
    const observed=await integratedRequest(actions,harness.origin+'/'+path,{},async response=>{
      const hash=createHash('sha256');let bytes=0
      for await(const part of response.body){bytes+=part.length;assert.ok(bytes<=8*1024**2);hash.update(part)}
      return {path,bytes,sha256:hash.digest('hex')}
    })
    assert.equal(observed.sha256,expected,'actual served asset matches copied current build')
    receipt.push(observed)
  }
  assert.ok(receipt.some(value=>/session-archive-worker-.*\.js$/.test(value.path)))
  return receipt
}

const percentile95=values=>[...values].sort((a,b)=>a-b)[Math.ceil(values.length*.95)-1]
async function ordinaryForeground(actions,origin,result={}){
  Object.assign(result,{configuration:{endpoint:'/api/v1/workspaces',requests:40,concurrency:1,postResponseDelayMs:20,timer:'Node fetch through parsed JSON and request cleanup'},startedAt:performance.now(),samples:[],requests:{columns:['startAt','headersAt','bodyAt','endAt','requestId','bodyIdentity'],identityEncoding:'SHA256 of JSON.stringify(parsed response); not original wire octets',rows:[],bodyIdentities:[]}})
  for(let n=0;n<40;n++){
    const observed={},at=performance.now(),body=await actions.fetchJson(origin+'/api/v1/workspaces',fetch,observed),end=performance.now()
    result.samples.push(end-at)
    // Identity work is outside the unchanged end-to-end request timer and is
    // performed identically in both phases; no response body is retained.
    const bytes=Buffer.from(JSON.stringify(body));assert.ok(bytes.length<=16384,'bounded isolated workspace response')
    const identity={sha256:createHash('sha256').update(bytes).digest('hex'),bytes:bytes.length}
    let index=result.requests.bodyIdentities.findIndex(value=>value.sha256===identity.sha256)
    if(index<0){index=result.requests.bodyIdentities.length;result.requests.bodyIdentities.push(identity)}
    result.requests.rows.push([at,observed.headersAt,observed.bodyAt,end,observed.requestId,index])
    await actions.pause(20)
  }
  result.endedAt=performance.now();result.p95Ms=percentile95(result.samples)
  return result
}

// Passive Playwright response observation only. It never generates a request.
// Each actual body-read capability is retained by the existing browser owner.
function observeArchiveManifests(page,url,observe){
  const records=[],errors=[],requests=new Map();let closed=false,firstDone,readyDone,firstSettled=false,readySettled=false
  const first=new Promise((yes,no)=>{firstDone={yes,no}}),ready=new Promise((yes,no)=>{readyDone={yes,no}})
  first.catch(()=>{});ready.catch(()=>{})
  const fail=error=>{if(errors.length<4)errors.push(errorTree(error,{left:2}));if(!firstSettled){firstSettled=true;firstDone.no(error)}if(!readySettled){readySettled=true;readyDone.no(error)}}
  const onRequest=request=>{if(request.url()===url){if(requests.size>=4){fail(Error('bounded passive manifest request admission'));return}requests.set(request,performance.now())}}
  const onResponse=response=>{
    if(closed||response.url()!==url)return
    const request=response.request(),requestedAt=requests.get(request)??null,headersAt=performance.now()
    observe((async()=>{
      try{
        const bytes=await response.body();assert.ok(bytes.length<=16384,'bounded real manifest body')
        const value=JSON.parse(bytes.toString('utf8')),at=performance.now(),status=response.status()
        assert.ok(records.length<128,'bounded passive manifest observations')
        const record={requestedAt,headersAt,bodyAt:at,status,state:value.state,phase:value.progress?.phase??null,rawProgress:value.progress?.raw_bytes??null,compressedProgress:value.progress?.compressed_bytes??null,generation:value.generation_id??null}
        records.push(record)
        const active=status===202&&value.state==='preparing'&&['compressing','verifying'].includes(record.phase)
        const published=status===200&&value.state==='ready'
        assert.ok((status===202&&['preparing','retrying'].includes(value.state))||published,'real frontend manifest response')
        if(!firstSettled&&(active||published)){firstSettled=true;firstDone.yes({active,record,value})}
        if(published&&!readySettled){readySettled=true;readyDone.yes({record,value})}
      }catch(error){fail(error)}finally{requests.delete(request)}
    })())
  }
  const onClose=()=>stop()
  function stop(){if(closed)return;closed=true;page.off('request',onRequest);page.off('response',onResponse);page.off('close',onClose);if(!firstSettled||!readySettled)fail(Error('passive manifest observer retired before readiness'))}
  page.on('request',onRequest);page.on('response',onResponse);page.on('close',onClose)
  return {first,ready,stop,snapshot:()=>({scope:'passive real frontend response observations; queued Preparing is not active compression; event delivery uses caller monotonic time',records:[...records],errors:[...errors]})}
}

async function clockSnapshot(actions,harness){
  const metadataSentAt=performance.now(),value=await actions.control(harness,'snapshot'),metadataReceivedAt=performance.now()
  const sentAt=performance.now(),clock=await actions.control(harness,'status'),receivedAt=performance.now()
  assert.match(clock.db_clock_epoch_us,/^[0-9]+$/,'actual narrow-status PostgreSQL clock sample')
  return {sentAt,receivedAt,dbClockEpochUs:clock.db_clock_epoch_us,generation:value.archive_generation,postgres:value,metadataTiming:{sentAt:metadataSentAt,receivedAt:metadataReceivedAt,dbClockEpochUs:value.db_clock_epoch_us}}
}

async function awaitManifest(observation,actions,label,maximumMs=30000){
  let cancel
  const budget=actions.remaining(maximumMs)
  try{
    const cancelled=new Promise((_,no)=>{cancel=()=>no(actions.signal.reason);actions.signal.addEventListener('abort',cancel,{once:true})})
    // This retires only the logical wait. Actual response-body work stays in
    // the passive observer/browser owner and is joined by browser cleanup.
    return await deadline(Promise.race([observation,cancelled]),budget,label)
  }finally{actions.signal.removeEventListener('abort',cancel)}
}

function foregroundCoverage(before,after,load,expected,admitted){
  for(const sample of [before,after]){
    assert.ok(Number.isFinite(sample.sentAt)&&sample.receivedAt>=sample.sentAt)
    assert.match(sample.dbClockEpochUs,/^[0-9]+$/)
  }
  const generation=after.generation
  assert.ok(generation,'published generation snapshot required')
  assert.equal(generation.id,expected.generation);assert.equal(generation.session_id,expected.session)
  assert.equal(before.generation?.id,expected.generation,'admitted and published generation must match')
  assert.equal(before.generation?.session_id,expected.session)
  const created=Date.parse(generation.created_at),published=Date.parse(generation.published_at)
  assert.ok(Number.isFinite(created)&&Number.isFinite(published)&&published>=created,'actual persisted generation interval')
  const offsets=[before,after].map(value=>({low:value.sentAt-Number(value.dbClockEpochUs)/1000,high:value.receivedAt-Number(value.dbClockEpochUs)/1000}))
  const anchors=[before,after].map((value,index)=>({control:'status',sentAt:value.sentAt,receivedAt:value.receivedAt,roundTripMs:value.receivedAt-value.sentAt,dbClockEpochUs:value.dbClockEpochUs,offsetLowMs:offsets[index].low,offsetHighMs:offsets[index].high,metadataTiming:value.metadataTiming??null}))
  const offsetLow=Math.min(...offsets.map(value=>value.low))-1,offsetHigh=Math.max(...offsets.map(value=>value.high))+1
  const width=offsetHigh-offsetLow,inner={startAt:created+offsetHigh,endAt:published+offsetLow}
  const overlaps=load.requests.rows.filter(row=>row[0]>=inner.startAt&&row[3]<=inner.endAt).length
  const admittedDraft=admitted===true&&before.generation?.id===expected.generation&&before.generation?.session_id===expected.session&&before.generation?.published_at===null
  const qualified=admittedDraft&&width<=10&&load.samples.length===40&&load.requests.rows.length===40&&overlaps===40
  return {status:qualified?'qualified-coverage':'inconclusive',scope:'persisted draft-created to published processing/publication bracket; not uninterrupted compression CPU',requiredOverlap:40,observedOverlap:overlaps,admittedOwnedUnpublishedDraft:admittedDraft,clockMapping:{method:'union of narrow-status caller send/receive minus DB clock intervals; widened 1ms for timestamp precision; assumes no unobserved intervening clock step',samples:anchors,offsetLowMs:offsetLow,offsetHighMs:offsetHigh,uncertaintyWidthMs:width,maximumWidthMs:10},generation,conservativeInnerBracket:inner,reason:qualified?null:!admittedDraft?'owned unpublished draft was not observed before timed admission':width>10?'clock mapping uncertainty exceeds 10ms':'fewer than all40 timed requests fit inside the conservative processing/publication bracket'}
}

function foregroundResult(baseline,load,coverage){
  const result={status:coverage.status==='qualified-coverage'?'passed':'inconclusive',p95Ratio:load.p95Ms/baseline.p95Ms,maximumRatio:1.10,coverage}
  if(coverage.status==='qualified-coverage')assert.ok(load.p95Ms<=baseline.p95Ms*1.10,'matched ordinary foreground p95 <= 1.10 baseline')
  return result
}

async function writeCollectedForeground(runId,progress,ownerEndsAt,output=process.stderr,write=writePacket,now=()=>performance.now()){
  const errors=[],expires=Math.min(ownerEndsAt,now()+2000)
  const observed=progress.manifests
  const manifests=observed?{...observed,records:observed.records.slice(-16),totalObservations:observed.records.length}:null
  // These are already-collected rows, not foreground actions. One finite
  // allowance is shared by both writes and consumes the existing owner window.
  for(const name of ['baseline','load'])if(progress[name])try{
    const remaining=Math.floor(expires-now())
    if(remaining<1)throw Error('collected foreground diagnostic allowance expired; original cause preserved')
    await write(output,{archive_qualification_foreground:true,task,run:runId,phase:name,scope:'all actual timed samples from this failed case; not a qualification pass',data:progress[name],resume:progress.resume,coverage:progress.foregroundCoverage,manifests},remaining)
  }catch(error){errors.push(errorTree(error,{left:2}))}
  return errors
}

async function evidenceWindows(actions,origin,target,raw,outerEndsAt=Infinity){
  const samples=[],offsets=[0,1,Math.max(0,raw.bytes-65536),raw.bytes-1,raw.bytes]
  const expires=Math.min(outerEndsAt,performance.now()+actions.remaining(90000))
  const rangeRemaining=()=>{const left=Math.floor(expires-performance.now());assert.ok(left>0,'finite legacy range batch supervision');return actions.remaining(Math.min(30000,left))}
  for(let n=0;n<20;n++){
    const offset=offsets[n%offsets.length],start=performance.now()
    const value=await integratedRequest(actions,origin+target+'/evidence?offset='+offset+'&limit=65536',{timeoutMs:rangeRemaining()})
    const elapsed=performance.now()-start,bytes=Buffer.from(value.data_base64,'base64')
    assert.equal(value.offset,offset);assert.equal(bytes.length,Math.min(65536,raw.bytes-offset));assert.equal(value.next_offset,offset+bytes.length)
    const expected=Buffer.alloc(bytes.length)
    for(let copied=0;copied<expected.length;){
      const position=offset+copied,first=Math.floor(position/65536)*64,block=rawRecipeBlock(first),within=position%65536,take=Math.min(block.length-within,expected.length-copied)
      block.copy(expected,copied,within,within+take);copied+=take
    }
    assert.deepEqual(bytes,expected,'legacy evidence preserves original bytes across arbitrary offsets')
    samples.push(elapsed)
  }
  // Actual sequential next_offset advancement, beyond isolated corner reads.
  let offset=0;const digest=createHash('sha256')
  for(let n=0;n<8&&offset<raw.bytes;n++){
    const value=await integratedRequest(actions,origin+target+'/evidence?offset='+offset+'&limit=65536',{timeoutMs:rangeRemaining()}),bytes=Buffer.from(value.data_base64,'base64')
    assert.equal(value.offset,offset);assert.ok(bytes.length>0&&bytes.length<=65536);assert.equal(value.next_offset,offset+bytes.length)
    digest.update(bytes);offset=value.next_offset
  }
  return {scope:'20 matching raw/archive corner ranges plus eight sequential 64KiB windows; not a whole-input evidence-range traversal',samples,p95Ms:percentile95(samples),sequentialBytes:offset,sequentialSha256:digest.digest('hex')}
}

async function zipSlice(file,start,length,signal){
  assert.ok(length>=1&&length<=1024,'bounded ZIP metadata read')
  const stream=createReadStream(file,{start,end:start+length-1,highWaterMark:1024,signal}),result=Buffer.alloc(length);let offset=0
  try{for await(const bytes of stream){assert.ok(offset+bytes.length<=length);bytes.copy(result,offset);offset+=bytes.length}}
  finally{if(!stream.destroyed)stream.destroy();await finished(stream)}
  assert.ok(stream.closed,'actual ZIP metadata source closed');assert.equal(offset,length)
  return result
}

async function verifyTaskZip(actions,harness,run,workspace,ownedTask,session,raw){
  const file=join(run.path,'evidence-'+session+'.zip');assert.ok(inside(run.path,file))
  await admitRun(raw.bytes+16777216)
  const start=performance.now(),bulkBudgetMs=raw.bytes>=1073741824?180000:90000,phaseAbort=new AbortController(),cancel=()=>phaseAbort.abort(actions.signal.reason)
  const phaseTimer=setTimeout(()=>phaseAbort.abort(Error('whole ZIP download/metadata/inflate supervision deadline')),actions.remaining(bulkBudgetMs))
  actions.signal.addEventListener('abort',cancel,{once:true})
  try{
  const zip=await integratedRequest(actions,harness.origin+'/api/v1/workspaces/'+workspace+'/tasks/'+ownedTask+'/evidence.zip',{timeoutMs:bulkBudgetMs,externalSignal:phaseAbort.signal},async(response,signal)=>{
    const hash=createHash('sha256');let bytes=0
    const writer=createWriteStream(file,{flags:'wx',highWaterMark:65536})
    await pipeline(Readable.fromWeb(response.body),new Transform({transform(piece,_,done){bytes+=piece.length;if(bytes>raw.bytes+16777216){done(Error('bounded single-session ZIP size'));return}hash.update(piece);done(null,piece)}}),writer,{signal})
    assert.ok(writer.closed,'actual ZIP destination closed before verification')
    return {bytes,sha256:hash.digest('hex')}
  })
  assert.ok(zip.bytes>=98)
  const tail=await zipSlice(file,zip.bytes-98,98,phaseAbort.signal),header=await zipSlice(file,0,30,phaseAbort.signal)
  assert.equal(tail.readUInt32LE(0),0x06064b50);assert.equal(header.readUInt32LE(0),0x04034b50);assert.equal(header.readUInt16LE(8),8)
  assert.equal(tail.readBigUInt64LE(24),1n);assert.equal(tail.readBigUInt64LE(32),1n)
  const central=Number(tail.readBigUInt64LE(48)),begin=30+header.readUInt16LE(26)+header.readUInt16LE(28)
  assert.ok(begin<central-24&&central<zip.bytes-98)
  const descriptor=await zipSlice(file,central-24,24,phaseAbort.signal)
  assert.equal(descriptor.readUInt32LE(0),0x08074b50);assert.equal(descriptor.readBigUInt64LE(8),BigInt(central-24-begin));assert.equal(descriptor.readBigUInt64LE(16),BigInt(raw.bytes))
  const centralHeader=await zipSlice(file,central,46,phaseAbort.signal)
  assert.equal(centralHeader.readUInt32LE(0),0x02014b50)
  const name=await zipSlice(file,central+46,centralHeader.readUInt16LE(28),phaseAbort.signal);assert.ok(name.toString('utf8').includes(session),'single ZIP entry belongs to the owned session')
  assert.equal(centralHeader.readUInt16LE(30),28)
  const extra=await zipSlice(file,central+46+name.length,28,phaseAbort.signal)
  assert.equal(extra.readUInt16LE(0),1);assert.equal(extra.readUInt16LE(2),24);assert.equal(extra.readBigUInt64LE(4),BigInt(raw.bytes));assert.equal(extra.readBigUInt64LE(12),BigInt(central-24-begin));assert.equal(extra.readBigUInt64LE(20),0n)
  const source=createReadStream(file,{start:begin,end:central-25,highWaterMark:65536}),hash=createHash('sha256');let bytes=0,checksum=0
  await pipeline(source,createInflateRaw(),new Writable({write(piece,_,done){bytes+=piece.length;if(bytes>raw.bytes){done(Error('ZIP raw count overflow'));return}hash.update(piece);checksum=crc32(piece,checksum);done()}}),{signal:phaseAbort.signal})
  assert.ok(source.closed,'actual ZIP deflate source closed before removal')
  const rawSha256=hash.digest('hex');assert.equal(bytes,raw.bytes);assert.equal(rawSha256,raw.sha256)
  assert.equal(checksum,descriptor.readUInt32LE(4));assert.equal(checksum,centralHeader.readUInt32LE(16))
  phaseAbort.signal.throwIfAborted()
  await rm(file,{force:false})
  phaseAbort.signal.throwIfAborted()
  return {...zip,rawBytes:bytes,rawSha256,crc32:checksum.toString(16).padStart(8,'0'),elapsedMs:performance.now()-start,bulkBudgetMs,entries:1,scope:'complete actual ZIP64 single entry; streamed DEFLATE bytes/count/SHA256/CRC32 and descriptors/central directory; no full-input buffer'}
  }finally{clearTimeout(phaseTimer);actions.signal.removeEventListener('abort',cancel)}
}

async function archiveCompatibility(actions,harness,run,workspace,ownedTask,session,raw,proofs,firstArray,baseline){
  const target='/api/v1/workspaces/'+workspace+'/sessions/'+session,phases={}
  const postView=await actions.control(harness,'phase',{name:'compatibility_reclaim'})
  const initial=await actions.control(harness,'snapshot');assert.ok(initial.raw_rows>0)
  const begin=performance.now(),until=performance.now()+actions.remaining(90000)
  await actions.control(harness,'reclaim-enable')
  let state
  do{
    state=await actions.control(harness,'status')
    if(state.cleanup_complete===1&&Number(state.reclaimed_bytes)===raw.bytes)break
    assert.ok(performance.now()<until,'bounded raw-reclamation completion');await actions.pause(100)
  }while(true)
  await actions.control(harness,'pause');await actions.control(harness,'reclaim-disable')
  const reclaimed=await actions.control(harness,'snapshot');assert.equal(reclaimed.raw_rows,0)
  phases.reclaimMs=performance.now()-begin
  const archivedRanges=await evidenceWindows(actions,harness.origin,target,raw)
  assert.equal(archivedRanges.sequentialSha256,baseline.sequentialSha256)
  assert.ok(archivedRanges.p95Ms<=Math.max(500,2*baseline.p95Ms),'legacy range p95 <= max500ms/twice matching raw baseline')
  const verified=[],proofGroupEndsAt=performance.now()+actions.remaining(raw.bytes>=1073741824?120000:90000)
  for(const proof of proofs){
    const operation=randomUUID(),started=performance.now(),expires=proofGroupEndsAt
    const proofRemaining=()=>{const left=Math.floor(expires-performance.now());assert.ok(left>0,'bounded full historical proof group');return actions.remaining(Math.min(10000,left))}
    let value=await integratedRequest(actions,harness.origin+target+'/prefix-verifications',{method:'POST',timeoutMs:proofRemaining(),body:{operation_id:operation,expected_offset:proof.offset,expected_revision:proof.revision,sha256:proof.sha256}})
    while(value.state==='pending'){
      assert.ok(performance.now()<expires,'bounded full historical prefix proof');await actions.pause(20)
      value=await integratedRequest(actions,harness.origin+target+'/prefix-verifications/'+operation,{timeoutMs:proofRemaining()})
    }
    assert.equal(value.state,'verified');assert.equal(value.sha256,proof.sha256);assert.equal(value.expected_offset,proof.offset);assert.equal(value.expected_revision,proof.revision)
    verified.push({...proof,elapsedMs:performance.now()-started,operationId:operation})
  }
  const replay=await integratedRequest(actions,harness.origin+target+'/append',{method:'POST',body:{expected_offset:0,expected_revision:0,data_base64:firstArray.toString('base64'),allow_final_record:false}})
  assert.equal(replay.replayed,true);assert.equal(replay.session.committed_bytes,raw.bytes)
  const zip=await verifyTaskZip(actions,harness,run,workspace,ownedTask,session,raw)
  const memory=await actions.control(harness,'phase',{name:'compatibility_complete'})
  return {scope:'actual raw-only baseline then archived-only legacy paths after verified reclamation; restore remains pending',phases,rawBaseline:baseline,archivedRanges,proofs:verified,replay:{replayed:true,committedBytes:replay.session.committed_bytes},zip,storage:{beforeReclaim:initial,afterReclaim:reclaimed},application:memory,postViewApplication:postView}
}

async function retireArchiveDocument(page,actions,observe){
  const previous=await page.evaluate(()=>window.__integratedArchive.documentNonce)
  const workers=page.workers(),retired=observe(Promise.all(workers.map(worker=>new Promise(done=>worker.once('close',done)))))
  await page.goto('about:blank',{waitUntil:'load',timeout:actions.remaining(10000)})
  assert.equal(page.url(),'about:blank','actual blank document committed before fresh ready clock')
  await page.waitForFunction(()=>location.href==='about:blank'&&document.readyState==='complete',null,{timeout:actions.remaining(5000)})
  await deadline(retired,actions.remaining(5000),'previous document automation Worker close')
  assert.equal(page.workers().length,0,'previous document automation worker targets retired at committed blank document')
  assert.equal(await page.locator('.session-sealed-viewer .session-record-text').count(),0)
  // Chromium may leave about:blank uninstrumented. Its committed document and
  // closed prior worker targets prove retirement; only real app documents
  // must carry an init-script nonce.
  const blank=await page.evaluate(()=>window.__integratedArchive?.documentNonce??null)
  assert.notEqual(blank,previous,'blank navigation replaced the instrumented document')
  return {previous,blank}
}

async function integrateArchive(harness,run,actions,vector){
  const progress={phase:'served-assets'}
  run.integrationProgress=progress
  const servedAssets=await bindServedAssets(harness,run,actions)
  const browserOwner=attachIntegratedBrowser(harness,run,actions)
  progress.servedAssetCount=servedAssets.length;progress.phase='browser-launch'
  const setupRemaining=()=>{const left=Math.floor(run.setupEndsAt-performance.now());assert.ok(left>0,'case setup/append/raw-baseline supervision exhausted');return actions.remaining(Math.min(10000,left))}
  setupRemaining()
  const {page,image}=await browserOwner.open(),call=(path,request={})=>integratedRequest(actions,harness.origin+path,{...request,timeoutMs:setupRemaining()})
  run.captureIntegrationFailure=async ownerEndsAt=>{
    // The exact in-flight browser request remains with its browser owner even
    // if this bounded observation times out. Browser close joins it before
    // native teardown, and no transcript text is included in this snapshot.
    let client
    progress.manifests=run.manifestObserver?.snapshot()
    const metricOutputErrors=await writeCollectedForeground(run.id,progress,ownerEndsAt)
    try{
      const budget=actions.remaining(1000)
      client=await deadline(browserOwner.observe(page.evaluate(()=>{
        const s=window.__integratedArchive
        return {url:location.href,readyState:document.readyState,client:s?{documentNonce:s.documentNonce,held:s.held,bytes:s.bytes,complete:s.complete,verified:s.verified,rowsBeforeComplete:s.rowsBeforeComplete,rowsBeforeVerified:s.rowsBeforeVerified,events:s.events.slice(-16)}:null,dom:{viewerPresent:!!document.querySelector('.session-sealed-viewer'),recordCount:document.querySelectorAll('.session-sealed-viewer .session-record-text').length,verifiedLabel:[...document.querySelectorAll('.session-sealed-viewer *')].some(node=>node.childElementCount===0&&node.textContent==='Verified complete archive.'),alerts:[...document.querySelectorAll('[role="alert"]')].slice(0,2).map(node=>node.textContent.slice(0,256))}}
      })),budget,'bounded pre-cleanup client diagnostic')
    }catch(error){client={unavailable:errorTree(error,{left:2})}}
    run.integrationDiagnostic=boundedText(JSON.stringify({phase:progress.phase,actualBrowser:client,metricOutputErrors,partial:progress}),6144)
  }
  progress.phase='original-appends'
  const workspace=await call('/api/v1/workspaces',{method:'POST',body:{name:'Task8 integrated '+vector.mebibytes+' MiB',workspace_type:'repository'}})
  const ownedTask=await call('/api/v1/tasks',{method:'POST',body:{workspace_id:workspace.id,title:'Integrated sealed archive'}})
  await call('/api/v1/tasks/'+ownedTask.id,{method:'PUT',body:{status:'in_progress'}})
  const id=randomUUID(),base='/api/v1/workspaces/'+workspace.id+'/sessions',target=base+'/'+id
  await call(base,{method:'POST',body:{id,format_label:'codex-jsonl',title:'Qualify integrated sealed session archives',task_id:ownedTask.id}})
  const digest=createHash('sha256'),records=vector.mebibytes*1024,arrayBytes=vector.mebibytes===1?65536:1048576,proofs=[];let offset=0,revision=0,firstArray
  for(let first=0;first<records;first+=arrayBytes/1024){
    const block=Buffer.alloc(arrayBytes)
    for(let part=0;part<arrayBytes;part+=65536)rawRecipeBlock(first+part/1024).copy(block,part)
    digest.update(block);firstArray??=Buffer.from(block)
    const value=await call(target+'/append',{method:'POST',body:{expected_offset:offset,expected_revision:revision,data_base64:block.toString('base64'),allow_final_record:false}})
    offset=value.session.committed_bytes;revision=value.session.revision
    if(revision===1||offset===vector.mebibytes*1048576/2||offset===vector.mebibytes*1048576)proofs.push({offset,revision,sha256:digest.copy().digest('hex')})
  }
  const raw={recipe:'jsonl-1024-v1:xorshift32-0x51a7:hex-seq:utf8-crlf',bytes:offset,records,sha256:digest.digest('hex'),appendArrays:revision,appendArrayBytes:arrayBytes}
  progress.raw=raw;progress.phase='seal-preparing'
  const expectedHashes={1:'c6b5a46542811809a4676fc43fa923689df3375b80b8f57658b35e4b5cd41df6',100:'fef46e1443ee2ab82b783e407b8294c5d3cf95722e504fff651c343f46dc9fbb',1024:'54a9d911e7c8ea4c197aec21a35750d45275c946a13eff5722dbaacbc8e9098c'}
  assert.equal(raw.bytes,vector.mebibytes*1048576);assert.equal(raw.sha256,expectedHashes[vector.mebibytes])
  const sealStarted=performance.now()
  const sealed=await call(target+'/terminate',{method:'POST',body:{expected_offset:offset,expected_revision:revision,final_bytes:offset,final_sha256:raw.sha256}})
  assert.equal(sealed.state,'terminated')
  // /evidence is a saved-session surface. The worker is still paused, so this
  // matching baseline reads terminated original rows before any publication.
  const rawRanges=vector.compatibility?await evidenceWindows(actions,harness.origin,target,raw,run.setupEndsAt):null
  const preparing=await integratedRequest(actions,harness.origin+target+'/archive',{status:202})
  assert.equal(preparing.state,'preparing')
  const preparingSelected=performance.now()
  const manifestObserver=observeArchiveManifests(page,harness.origin+target+'/archive',browserOwner.observe)
  run.manifestObserver=manifestObserver
  await page.goto(harness.origin+'/workspace/'+workspace.id+'#tab=sessions&session='+id,{timeout:actions.remaining(10000)})
  await page.getByText(/Preparing/).first().waitFor({timeout:actions.remaining(10000)})
  assert.equal(await page.locator('.session-sealed-viewer .session-record-text').count(),0)
  await actions.control(harness,'phase',{name:'integration_idle_baseline'})
  await actions.pause(750)
  const idleMemory=await actions.control(harness,'phase',{name:'integration_warmups'})
  progress.idleBaseline=idleMemory
  for(let n=0;n<5;n++)await actions.fetchJson(harness.origin+'/api/v1/workspaces')
  await actions.control(harness,'phase',{name:'integration_foreground_baseline'})
  const baseline={};progress.baseline=baseline
  await ordinaryForeground(actions,harness.origin,baseline)
  const baselineMemory=await actions.control(harness,'phase',{name:'integration_archive_load'})
  progress.baseline=baseline;progress.baselineMemory=baselineMemory;progress.phase='publish-and-first-view'
  let foreground,manifest,readyAt,viewAt,heldAt,phase,load,loadError,before,after,admittedDraft,clientViewBudgetMs
  try{
    progress.resume={requestedAt:performance.now()}
    const preparationEndsAt=progress.resume.requestedAt+120000*Math.max(1,Math.ceil(raw.bytes/1073741824))+5000
    const preparationRemaining=()=>{const left=Math.floor(preparationEndsAt-performance.now());assert.ok(left>0,'case preparation/publication observation allowance exhausted');return left}
    await actions.control(harness,'resume')
    progress.resume.acknowledgedAt=performance.now()
    const admission=await awaitManifest(manifestObserver.first,actions,'passive frontend active-or-ready observation',preparationRemaining())
    before=await clockSnapshot(actions,harness)
    assert.ok(before.generation,'owned draft/publication snapshot required before timed admission')
    assert.equal(before.generation.session_id,id,'snapshot belongs to this owned session')
    admittedDraft=admission.active&&before.generation.published_at===null
    progress.admission={observation:admission.record,snapshot:{sentAt:before.sentAt,receivedAt:before.receivedAt,dbClockEpochUs:before.dbClockEpochUs,generation:before.generation},ownedUnpublishedDraft:admittedDraft}
    load={};progress.load=load
    foreground=ordinaryForeground(actions,harness.origin,load).then(value=>({value}),error=>({error}))
    const published=await awaitManifest(manifestObserver.ready,actions,'real frontend publication observation',preparationRemaining())
    manifest=published.value;readyAt=published.record.bodyAt
    clientViewBudgetMs=2*Number(manifest.compressed_bytes)/12500000*1000+120000*Math.max(1,Math.ceil(raw.bytes/1073741824))+5000
    assert.equal(manifest.workspace_id,workspace.id);assert.equal(manifest.session_id,id)
    progress.manifest=manifest;progress.sealToReadyMs=readyAt-sealStarted;progress.phase='first-content-eof'
    assert.equal(manifest.raw_sha256,raw.sha256);assert.equal(manifest.raw_bytes,String(raw.bytes));assert.equal(manifest.raw_records,String(raw.records))
    await page.waitForFunction(()=>window.__integratedArchive.held,null,{timeout:remainingView(actions,readyAt,clientViewBudgetMs)})
    heldAt=performance.now()
    assert.equal(await page.locator('.session-sealed-viewer .session-record-text').count(),0,'complete original receipt is required before any transcript display')
    assert.equal(await page.evaluate(()=>window.__integratedArchive.bytes),Number(manifest.compressed_bytes))
    await page.evaluate(()=>window.__integratedArchive.release())
    await page.getByText('Verified complete archive.',{exact:true}).waitFor({timeout:remainingView(actions,readyAt,clientViewBudgetMs)})
    await page.locator('.session-sealed-viewer .session-record-text').first().waitFor({timeout:remainingView(actions,readyAt,clientViewBudgetMs)})
    viewAt=performance.now()
    assert.ok(viewAt-readyAt<=clientViewBudgetMs,'unchanged observed-ready to verified-view deadline')
    progress.preparingSelectionToVerifiedViewMs=viewAt-preparingSelected
  }catch(error){loadError=error}
  finally{
    const joined=foreground?await foreground:{}
    if(joined.error)loadError=loadError?new AggregateError([loadError,joined.error],'integration and matched foreground failures'):joined.error
    load=joined.value??load
    try{phase=await actions.control(harness,'phase',{name:'integration_post_view'})}catch(error){loadError=loadError?new AggregateError([loadError,error],'integration and phase finalization failures'):error}
    progress.load=load;progress.loadMemory=phase
  }
  if(loadError)throw loadError
  after=await clockSnapshot(actions,harness)
  const coverage=foregroundCoverage(before,after,load,{generation:manifest.generation_id,session:id},admittedDraft)
  progress.foregroundCoverage=coverage;progress.manifests=manifestObserver.snapshot();manifestObserver.stop()
  progress.phase='independent-http-receipt'
  const transfer=await integratedRequest(actions,harness.origin+manifest.content_path,{timeoutMs:90000},async(response,signal)=>{
    assert.equal(response.headers.get('Content-Length'),manifest.compressed_bytes)
    assert.equal(response.headers.get('X-HMem-Archive-SHA256'),manifest.compressed_sha256)
    assert.equal(response.headers.get('Content-Encoding'),null)
    const compressed=createHash('sha256'),rawHash=createHash('sha256');let compressedBytes=0,rawBytes=0
    await pipeline(Readable.fromWeb(response.body),new Transform({transform(bytes,_,done){compressed.update(bytes);compressedBytes+=bytes.length;done(null,bytes)}}),createGunzip(),new Writable({write(bytes,_,done){rawHash.update(bytes);rawBytes+=bytes.length;done()}}),{signal})
    return {compressedBytes,compressedSha256:compressed.digest('hex'),rawBytes,rawSha256:rawHash.digest('hex')}
  })
  assert.equal(transfer.compressedSha256,manifest.compressed_sha256);assert.equal(transfer.compressedBytes,Number(manifest.compressed_bytes));assert.equal(transfer.rawSha256,raw.sha256);assert.equal(transfer.rawBytes,raw.bytes)
  progress.transfer=transfer;progress.phase='first-last-navigation'
  const navigation=[]
  for(const [name,text] of [['Last','row '+(records-1).toString(16).padStart(8,'0')],['First','row 00000000']]){
    const start=performance.now();await page.getByRole('button',{name,exact:true}).click({timeout:actions.remaining(5000)})
    await page.locator('.session-sealed-viewer .session-record-text').filter({hasText:text}).first().waitFor({timeout:actions.remaining(10000)})
    navigation.push({name,automationWallMs:performance.now()-start,text})
  }
  const client=await page.evaluate(()=>{const s=window.__integratedArchive;return {events:s.events,rowsBeforeComplete:s.rowsBeforeComplete,rowsBeforeVerified:s.rowsBeforeVerified,complete:s.complete,verified:s.verified}})
  progress.navigation=navigation
  assert.equal(client.rowsBeforeComplete,false);assert.equal(client.rowsBeforeVerified,false);assert.equal(client.verified,true)
  progress.phase='retire-previous-document'
  const documents=await retireArchiveDocument(page,actions,browserOwner.observe)
  const readySelected=performance.now()
  progress.phase='fresh-ready-content-eof'
  await page.goto(harness.origin+'/workspace/'+workspace.id+'#tab=sessions&session='+id,{timeout:remainingView(actions,readySelected,clientViewBudgetMs)})
  const readyNonce=await page.evaluate(()=>window.__integratedArchive.documentNonce)
  assert.match(readyNonce,/^[a-f0-9]{8}-[a-f0-9]{4}-[a-f0-9]{4}-[a-f0-9]{4}-[a-f0-9]{12}$/,'fresh real application init-script nonce')
  assert.notEqual(readyNonce,documents.previous,'fresh ready selection cannot reuse previous verified document')
  assert.notEqual(readyNonce,documents.blank,'session navigation committed another new instrumented document')
  await page.waitForFunction(()=>window.__integratedArchive.held,null,{timeout:remainingView(actions,readySelected,clientViewBudgetMs)})
  assert.equal(await page.evaluate(()=>window.__integratedArchive.bytes),Number(manifest.compressed_bytes),'fresh ready document received the complete compressed body again')
  assert.equal(await page.locator('.session-sealed-viewer .session-record-text').count(),0)
  await page.evaluate(()=>window.__integratedArchive.release())
  await page.getByText('Verified complete archive.',{exact:true}).waitFor({timeout:remainingView(actions,readySelected,clientViewBudgetMs)})
  await page.locator('.session-sealed-viewer .session-record-text').first().waitFor({timeout:remainingView(actions,readySelected,clientViewBudgetMs)})
  const readySelectionToVerifiedViewMs=performance.now()-readySelected
  assert.ok(readySelectionToVerifiedViewMs<=clientViewBudgetMs,'unchanged complete ready-view budget formula')
  progress.readySelectionToVerifiedViewMs=readySelectionToVerifiedViewMs;progress.phase='fixed-budget-assertions'
  const readyClient=await page.evaluate(()=>{const s=window.__integratedArchive;return {events:s.events,rowsBeforeComplete:s.rowsBeforeComplete,rowsBeforeVerified:s.rowsBeforeVerified,complete:s.complete,verified:s.verified}})
  assert.equal(readyClient.rowsBeforeComplete,false);assert.equal(readyClient.rowsBeforeVerified,false);assert.equal(readyClient.verified,true)
  const foregroundPerformance=foregroundResult(baseline,load,coverage)
  const baselineWorkingSet=Math.min(idleMemory.maximum_working_set_bytes,baselineMemory.maximum_working_set_bytes)
  const workingSetDelta=phase.maximum_working_set_bytes-baselineWorkingSet
  assert.ok(workingSetDelta<=128*1024**2,'application current working set incremental limit')
  assert.ok(phase.samples>=2&&baselineMemory.samples>=2)
  let compatibility
  if(vector.compatibility){
    progress.phase='post-reclaim-compatibility'
    compatibility=await archiveCompatibility(actions,harness,run,workspace.id,ownedTask.id,id,raw,proofs,firstArray,rawRanges)
    assert.ok(Math.max(compatibility.application.maximum_working_set_bytes,compatibility.postViewApplication.maximum_working_set_bytes)-baselineWorkingSet<=128*1024**2,'same application128MiB bound through both views/reclamation/proof/ZIP')
  }
  return {
    status:foregroundPerformance.status==='passed'?'current-slice-passed':'functional-success-performance-inconclusive',
    scope:'actual '+vector.mebibytes+' MiB PG publication, complete HTTP gzip/current client verified view'+(vector.compatibility?' and full post-reclaim proof/ZIP/legacy paths':'')+'; insufficient active coverage never qualifies foreground performance; restore and remaining matrix stay pending',
    caseBudget:{outerMs:vector.timeoutMs,internalMs:vector.internalMs,ownerCleanupMs:35000,deletionMs:5000,scratchReservationBytes:vector.reservedBytes,setupAppendRawBaselineMs:180000,bulkZipMs:raw.bytes>=1073741824?180000:90000,proofGroupMs:raw.bytes>=1073741824?120000:90000,reclamationMs:90000,archivedRangeBatchMs:90000,preparationObservationMs:125000,twoViews:'each uses the actual unchanged size-derived product formula',scope:'finite combined-case wrapper includes append/setup/raw ranges, preparation, both verified views, reclaim/range/proof/ZIP and reserved cleanup; restore is separately composed'},
    raw,manifest,transfer,compatibility,servedAssets,browser:image,client,readyClient,documents:{...documents,ready:readyNonce},navigation,
    navigationScope:'First/Last content fidelity and Node/Playwright wall time only; unchanged Task6 client-side 20-action navigation p95 remains a separate qualification',
    clocks:{scope:'Observed unadjusted qualification times. Seal-to-ready includes deliberate worker pause, raw baseline where enabled, idle sampling and foreground baseline. Preparing selection-to-view includes the remaining paused qualification work; both view clocks include deliberate EOF holds. No intervals are subtracted.',productDeadlineBoundaries:'unchanged formula applies independently from the actual ready manifest observation through first verified rows, and from fresh ready-session selection through verified rows; Preparing total remains separately reported',sealToReadyMs:readyAt-sealStarted,preparingSelectionToVerifiedViewMs:viewAt-preparingSelected,observedReadyToVerifiedViewMs:viewAt-readyAt,eofHeldAtMs:heldAt,readySelectionToVerifiedViewMs,clientViewBudgetMs},
    resume:progress.resume,manifestObservations:progress.manifests,foreground:{baseline,load,performance:foregroundPerformance},
    application:{scope:'current application process working set and CPU; PostgreSQL excluded',idleBaseline:idleMemory,foregroundBaseline:baselineMemory,load:phase,workingSetDeltaBytes:workingSetDelta},postgres:after.postgres
  }
}

test('integrated archive qualifier owns its sandbox and samples current application working set',
  {skip:process.env.HMEM_ARCHIVE_INTEGRATION_MODE!=='smoke'&&!integrationCases[process.env.HMEM_ARCHIVE_INTEGRATION_MODE],timeout:integrationCases[process.env.HMEM_ARCHIVE_INTEGRATION_MODE]?.timeoutMs??180000},async t=>{
    let run,harness,primary,cleanup,receipt,route,handshake,diagnosticError
    const vector=integrationCases[process.env.HMEM_ARCHIVE_INTEGRATION_MODE],runId=randomUUID(),endsAt=performance.now()+(vector?.internalMs??175000)
    const actions=archiveActionScope(endsAt,t.signal)
    try{
      route=requireReachableOwnerRoute()
      const preflight=preAdmission(runId)
      await route.announce({id:runId})
      handshake=await deadline(Promise.race([preflight.admission,route.inputEnded.then(error=>{throw error})]),20000,'pre-spawn operator status ACK/admit')
      actions.remaining(1)
      assert.equal(route.isInputOpen(),true,'operator closed before files/native')
      retainedOwners.delete(runId)
      run=await createRun(runId,endsAt,vector?.reservedBytes??smokeAllowance)
      run.operatorIsOpen=route.isInputOpen;run.operatorClosed=route.inputEnded;run.actions=actions
      harness=await startHarness(run)
      const status=await actions.control(harness,'status');assert.equal(status.runtime_safe,true);assert.equal(status.paused,true);assert.equal(status.reclaim_enabled,false);assert.equal(status.backfill_enabled,false)
      if(vector){
        receipt={scope:'actual '+vector.mebibytes+' MiB integration slice',task,handshake,input:{harnessSha256:harness.executableSha256},integration:await integrateArchive(harness,run,actions,vector),admission:run.admission}
      }else{
      await actions.control(harness,'phase',{name:'foreground_baseline'})
      const timings=[]
      for(let n=0;n<20;n++){const start=performance.now();await actions.fetchJson(harness.origin+'/api/v1/workspaces');timings.push(performance.now()-start);await actions.pause(20)}
      const phase=await actions.control(harness,'phase',{name:'idle'})
      assert.equal(phase.phase,'foreground_baseline');assert.ok(phase.samples>=2);assert.ok(phase.maximum_working_set_bytes>=phase.minimum_working_set_bytes);assert.ok(phase.maximum_gap_ms>0);assert.equal(phase.requested_interval_ms,20)
      const snapshot=await actions.control(harness,'snapshot');assert.ok(snapshot.database_bytes>0);assert.equal(snapshot.raw_rows,0)
      await actions.control(harness,'resume');await actions.control(harness,'pause')
      assert.equal((await actions.control(harness,'status')).reclaim_enabled,false)
      actions.remaining(1)
      const containedBytes=await scratchBytes(run.path)
      receipt={scope:'first-slice native harness/control/sampler smoke only; no archive integration acceptance',task,handshake,input:{harnessSha256:harness.executableSha256},phase,foregroundSamples:timings.length,snapshot,admission:run.admission,observedContainedBytes:containedBytes}
      }
    }catch(error){
      primary=error
      // Failure diagnostics and resource release share this absolute window;
      // an aborted action signal cannot discard already-collected evidence.
      if(run)run.cleanupEndsAt=Math.min(performance.now()+35000,endsAt-5000)
      try{await run?.captureIntegrationFailure?.(run.cleanupEndsAt)}catch(failure){diagnosticError=failure}
      try{
        const remaining=run?Math.floor(Math.min(2000,run.cleanupEndsAt-performance.now())):2000
        if(remaining<1)throw Error('owner diagnostic window expired; original cause preserved')
        await writePacket(process.stderr,failurePacket(run??{id:runId},harness??run?.nativeOwner,[primary]),remaining)
      }catch(failure){diagnosticError=failure}
    }
    finally{
      harness??=run?.nativeOwner
      if(harness)try{receipt={...receipt,cleanup:await harness.close(run.cleanupEndsAt??Math.min(performance.now()+35000,endsAt-5000))}}catch(error){cleanup=error}
      else retainedOwners.delete(runId)
      if(run&&!cleanup)try{await run.dispose()}catch(error){cleanup=error}
      route?.close()
      actions.dispose()
    }
    if(primary||cleanup){
      const causes=[primary,cleanup,diagnosticError].filter(Boolean)
      try{await writePacket(process.stderr,failurePacket(run??{id:runId},harness,causes))}catch(failure){causes.push(failure)}
      throw new AggregateError(causes,'Archive qualifier smoke failed; unresolved diagnostics remain owned by Task8 until issue closure')
    }
    assert.equal(receipt.cleanup.sandboxRemoved,true,'successful smoke requires observed sandbox cleanup')
    console.log(JSON.stringify(receipt))
  })

test('owner interface causal: actual status ACK gates admission and EOF creates no resource',async()=>{
  const input=new PassThrough(),output=new PassThrough(),owners=new Map(),id=randomUUID(),frames=[]
  output.on('data',bytes=>{for(const line of bytes.toString().trim().split('\n'))frames.push(JSON.parse(line))})
  const route=openOwnerRoute(input,output,owners),preflight=preAdmission(id,owners)
  const frame=(command,requestId,extra={})=>JSON.stringify({kind:'archive-qualification-owner',task,run:id,id:requestId,command,...extra})+'\n'
  input.write(frame('admit',1,{ack_id:2}));await new Promise(done=>setImmediate(done))
  assert.equal(preflight.placeholder.status().admitted,false);assert.ok(frames.at(-1).error)
  input.write(frame('status',2));await new Promise(done=>setImmediate(done));assert.equal(frames.at(-1).status.filesCreated,false)
  input.write(frame('admit',3,{ack_id:2}));const ack=await deadline(preflight.admission,1000,'fake admission');assert.equal(ack.statusAck,2)
  input.end();await route.inputEnded;assert.equal(route.isInputOpen(),false)
  await new Promise(done=>setImmediate(done))
  owners.delete(id);assert.equal(route.close(),true);output.destroy()
})

test('integration protocol causal: view waits consume one unchanged product deadline and the case cutoff',()=>{
  const requested=[],actions={remaining:ms=>{requested.push(ms);return Math.min(ms,70000)}}
  const budget=2*6000000/12500000*1000+120000+5000
  assert.equal(remainingView(actions,100,budget,200),70000)
  assert.equal(remainingView(actions,100,budget,100000),Math.floor(100+budget-100000))
  assert.ok(requested[1]<requested[0],'later transfer/verification waits cannot restart the full allowance')
  assert.throws(()=>remainingView(actions,100,budget,100+budget),/product deadline exhausted/)
  assert.throws(()=>remainingView({remaining(){throw Error('case cutoff')}},100,budget,200),/case cutoff/)
})

test('integration protocol causal: narrow status clock retains separate broad-metadata bounds',async()=>{
  const calls=[],sample=await clockSnapshot({control:async(_owner,command)=>{
    calls.push(command)
    return command==='snapshot'?{db_clock_epoch_us:'111000000',archive_generation:{id:'owned'},raw_rows:16}:{db_clock_epoch_us:'222000000',paused:false}
  }},{})
  assert.deepEqual(calls,['snapshot','status']);assert.equal(sample.dbClockEpochUs,'222000000');assert.equal(sample.metadataTiming.dbClockEpochUs,'111000000')
  assert.equal(sample.generation.id,'owned');assert.ok(sample.sentAt>=sample.metadataTiming.receivedAt);assert.ok(sample.receivedAt>=sample.sentAt)
  assert.equal(crc32(Buffer.from('123456789')),0xcbf43926)
  assert.equal(crc32(Buffer.from('456789'),crc32(Buffer.from('123'))),0xcbf43926,'streamed ZIP CRC state matches the independent standard vector')
})

test('integration protocol causal: aborted actions preserve collected rows inside the shared cleanup window',async()=>{
  const aborted=new AbortController();aborted.abort(Error('foreground cancelled'))
  const actions=archiveActionScope(performance.now()+39000,aborted.signal)
  try{
    assert.throws(()=>actions.remaining(2000),/foreground cancelled/)
    const primary=Error('original foreground assertion'),progress={baseline:{samples:Array(40).fill(1),requests:{rows:Array(40).fill([1,2,3,4,'id',0])}},load:{samples:Array(40).fill(2),requests:{rows:Array(40).fill([5,6,7,8,'id',0])}}}
    let clock=1000;const ownerEndsAt=36000,frames=[],allowances=[]
    const errors=await writeCollectedForeground('run',progress,ownerEndsAt,null,async(_output,packet,remaining)=>{frames.push(packet);allowances.push(remaining);clock+=750},()=>clock)
    assert.deepEqual(errors,[]);assert.deepEqual(frames.map(value=>value.data.samples.length),[40,40]);assert.deepEqual(allowances,[2000,1250])
    assert.equal(ownerEndsAt-clock,33500,'diagnostic time consumes the original owner deadline; no fresh35-second window')
    assert.equal(frames.reduce((sum,value)=>sum+value.data.requests.rows.length,0),80)
    progress.load.samples.length=13;progress.load.requests.rows.length=13
    let retainedPrimary
    try{throw primary}catch(error){
      const failures=await writeCollectedForeground('run',progress,ownerEndsAt,null,async(_output,packet)=>{if(packet.phase==='baseline')throw Error('diagnostic output failure');assert.equal(packet.data.requests.rows.length,13)},()=>clock)
      retainedPrimary=error;assert.equal(failures[0].message,'diagnostic output failure')
    }
    assert.equal(retainedPrimary,primary,'failed diagnostic write cannot replace the original caught failure')
  }finally{actions.dispose()}
})

test('integration protocol causal: passive manifest observations make no HTTP and distinguish queued from active',async()=>{
  const page=new EventEmitter(),pending=[],url='http://owned.test/sessions/s/archive',previousFetch=globalThis.fetch
  let generatedRequests=0,firstFinished=false
  globalThis.fetch=()=>{generatedRequests++;throw Error('observer must not generate HTTP')}
  try{
    const observer=observeArchiveManifests(page,url,promise=>{pending.push(promise);return promise})
    observer.first.then(()=>{firstFinished=true})
    const emit=async(value,status)=>{
      const request={url:()=>url},response={url:()=>url,request:()=>request,status:()=>status,body:async()=>Buffer.from(JSON.stringify(value))}
      page.emit('request',request);page.emit('response',response);await pending.at(-1)
    }
    await emit({state:'preparing',progress:{phase:'queued',raw_bytes:'0'}},202)
    assert.equal(firstFinished,false,'queued preparing cannot admit timed active work')
    await emit({state:'preparing',progress:{phase:'compressing',raw_bytes:'65536'}},202)
    assert.equal((await observer.first).active,true)
    await emit({state:'ready',generation_id:'g',progress:{phase:'ready'}},200)
    assert.equal((await observer.ready).value.generation_id,'g')
    assert.deepEqual(observer.snapshot().records.map(value=>value.phase),['queued','compressing','ready'])
    assert.equal(generatedRequests,0);observer.stop()
    assert.equal(page.listenerCount('request'),0);assert.equal(page.listenerCount('response'),0)
  }finally{globalThis.fetch=previousFetch}
})

test('integration protocol causal: conservative clock coverage cannot qualify missing active samples or waive p95',()=>{
  const epoch=1700000000000,expected={generation:'g',session:'s'}
  const before={sentAt:100,receivedAt:102,dbClockEpochUs:String(epoch*1000),generation:{id:'g',session_id:'s',created_at:new Date(epoch-50).toISOString(),published_at:null}}
  const after={sentAt:400,receivedAt:402,dbClockEpochUs:String((epoch+300)*1000),generation:{...before.generation,published_at:new Date(epoch+100).toISOString()}}
  const load={samples:Array(40).fill(1),p95Ms:1,requests:{rows:Array.from({length:40},(_,index)=>[80+index*2,80+index*2,81+index*2,81+index*2,'id',0])}}
  const covered=foregroundCoverage(before,after,load,expected,true)
  assert.equal(covered.status,'qualified-coverage');assert.equal(covered.observedOverlap,40)
  assert.equal(covered.clockMapping.samples.length,2);assert.equal(covered.clockMapping.samples[0].sentAt,100);assert.equal(covered.clockMapping.samples[0].receivedAt,102);assert.equal(covered.clockMapping.samples[0].roundTripMs,2);assert.equal(covered.clockMapping.samples[1].dbClockEpochUs,after.dbClockEpochUs)
  const missing=structuredClone(load);missing.requests.rows[0][0]=52
  const incomplete=foregroundCoverage(before,after,missing,expected,true)
  assert.equal(incomplete.status,'inconclusive');assert.equal(incomplete.observedOverlap,39)
  assert.equal(foregroundCoverage(before,{...after,receivedAt:420},load,expected,true).status,'inconclusive')
  assert.equal(foregroundCoverage(before,after,load,expected,false).status,'inconclusive')
  assert.throws(()=>foregroundCoverage(before,after,load,{...expected,generation:'foreign'},true),/equal/)
  assert.throws(()=>foregroundResult({p95Ms:1},{...load,p95Ms:1.2},covered),/1.10/)
  assert.equal(foregroundResult({p95Ms:1},{...load,p95Ms:1.2},incomplete).status,'inconclusive')
  assert.equal(foregroundResult({p95Ms:1},load,covered).maximumRatio,1.10)
})

test('integration document causal: blank commit and actual worker close precede fresh selection',async()=>{
  const order=[],worker=new EventEmitter();let url='http://fixture/workspace/x#session=y',nonce='original',reads=0,active=true,finishNavigation
  const page={
    async evaluate(){reads++;return nonce},
    workers(){return active?[worker]:[]},
    async goto(next,options){assert.equal(next,'about:blank');assert.equal(options.waitUntil,'load');order.push('blank-committed');url=next;nonce=null;finishNavigation=()=>{active=false;worker.emit('close')}},
    url:()=>url,
    locator:()=>({count:async()=>0}),
    async waitForFunction(){order.push('blank-load-confirmed')}
  }
  let observed
  const pending=retireArchiveDocument(page,{remaining:maximum=>maximum},promise=>{observed=promise;return promise})
  await new Promise(done=>setImmediate(done))
  assert.equal(reads,1,'fresh document check cannot pass an unclosed previous worker')
  finishNavigation();await observed
  assert.deepEqual(await pending,{previous:'original',blank:null});assert.deepEqual(order,['blank-committed','blank-load-confirmed'])
  const stale={...page,workers:()=>[],evaluate:async()=>'same-document'}
  await assert.rejects(retireArchiveDocument(stale,{remaining:maximum=>maximum},promise=>promise),/replaced the instrumented document/)
})

test('integration recipe causal: original append arrays reproduce qualified 1 MiB raw receipt',()=>{
  const digest=createHash('sha256');let bytes=0,records=0
  for(let first=0;first<1024;first+=64){
    const block=rawRecipeBlock(first)
    assert.equal(block.length,65536)
    for(const line of block.toString('utf8').split('\r\n').filter(Boolean)){
      const row=JSON.parse(line);assert.ok(row.payload.content[0].text.startsWith('row '+records.toString(16).padStart(8,'0')+' λ😀 '));records++
    }
    bytes+=block.length;digest.update(block)
  }
  assert.equal(records,1024);assert.equal(bytes,1048576)
  assert.equal(digest.digest('hex'),'c6b5a46542811809a4676fc43fa923689df3375b80b8f57658b35e4b5cd41df6')
})

test('owner interface causal: maintenance completion count cannot mask late retained cleanup',async()=>{
  for(const lateRecovery of [false,true]){
    const child=new EventEmitter();child.pid=102;child.stdin=new PassThrough();child.stdout=new PassThrough();child.stderr=new PassThrough()
    const run={id:randomUUID()},owners=new Map(),commands=[],owner=ownHarnessChild(child,run,'http://127.0.0.1:1','0'.repeat(64),owners)
    let finishedChild=false,closing
    const finish=code=>{if(finishedChild)return;finishedChild=true;child.stdin.destroy();child.stdout.destroy();child.stderr.destroy();child.emit('close',code,null)}
    child.stdin.on('data',bytes=>{
      const request=JSON.parse(bytes.toString());commands.push(request.command)
      if(request.command==='status')child.stdout.write(JSON.stringify({qualification_control:true,id:request.id,result:{cleanup_complete:1}})+'\n')
      else if(request.command==='stop'){
        child.stdout.write(JSON.stringify({qualification_control:true,id:request.id,result:{stopping:true}})+'\n')
        setImmediate(()=>lateRecovery?child.stdout.write(JSON.stringify({qualification_recovery:true,state:'retained'})+'\n'):finish(0))
      }else if(request.command==='retry-cleanup'){
        child.stdout.write(JSON.stringify({qualification_control:true,id:request.id,result:{cleanup_complete:true}})+'\n')
        setImmediate(()=>finish(1))
      }
    })
    // An unsolicited boolean has no pending command/ID authority either.
    child.stdout.write(JSON.stringify({qualification_control:true,id:99999,result:{cleanup_complete:true}})+'\n')
    try{
      assert.equal((await owner.control('status')).cleanup_complete,1)
      closing=owner.close()
      if(lateRecovery){
        await assert.rejects(deadline(closing,1000,'fake late recovery correlation'),error=>{
          assert.ok(error.errors?.some(cause=>cause.message==='original native failure preserved after verified recovery'));return true
        })
        assert.deepEqual(commands,['status','stop','retry-cleanup'])
        assert.equal(owner.status().cleanupReceipt.recovered,true,'only actual matching retry-cleanup boolean is recovery evidence')
        await assert.rejects(owner.close(),/Native stop\/transport\/join\/cleanup/)
      }else{
        const receipt=await deadline(closing,1000,'fake ordinary stop correlation')
        assert.equal(receipt.recovered,false);assert.deepEqual(commands,['status','stop'])
      }
      assert.equal(owner.status().pendingControls,0);assert.equal(owners.size,0)
    }finally{finish(1);if(closing)await closing.catch(()=>{});await owner.closed}
  }
})

test('owner interface causal: retained owner remains externally callable and error receipt never becomes success',async()=>{
  const child=new EventEmitter();child.pid=101;child.stdin=new PassThrough();child.stdout=new PassThrough();child.stderr=new PassThrough()
  const run={id:randomUUID()},owners=new Map(),owner=ownHarnessChild(child,run,'http://127.0.0.1:1','0'.repeat(64),owners)
  child.stdout.write(JSON.stringify({qualification_recovery:true,state:'retained'})+'\n')
  assert.equal((await owner.startup).kind,'retained')
  const input=new PassThrough(),output=new PassThrough(),frames=[]
  output.on('data',bytes=>{for(const line of bytes.toString().trim().split('\n'))frames.push(JSON.parse(line))})
  const route=openOwnerRoute(input,output,owners)
  child.stdin.on('data',bytes=>{
    const request=JSON.parse(bytes.toString());assert.equal(request.command,'retry-cleanup')
    child.stdout.write(JSON.stringify({qualification_control:true,id:request.id,result:{cleanup_complete:true}})+'\n')
    setImmediate(()=>{child.stdin.destroy();child.stdout.destroy();child.stderr.destroy();child.emit('close',1,null)})
  })
  input.write(JSON.stringify({kind:'archive-qualification-owner',task,run:run.id,id:1,command:'retry-cleanup'})+'\n')
  await deadline(owner.closed,1000,'fake retained closure');await new Promise(done=>setImmediate(done));await new Promise(done=>setImmediate(done))
  assert.equal(frames.at(-1).status.cleanupReceipt.actualHandleJoined,true);assert.ok(frames.at(-1).failure)
  await assert.rejects(owner.close(),/Native stop\/transport\/join\/cleanup/)
  assert.equal(owner.status().cleanupReceipt.sandboxRemoved,null,'no filesystem/PG cleanup claim from fake native closure')
  assert.equal(route.close(),true);input.destroy();output.destroy()
})

test('owner interface causal: late readiness stops actions and cancellation awaits its request',async()=>{
  const cancelled=new AbortController(),endsAt=performance.now()+40030,scope=archiveActionScope(endsAt,cancelled.signal)
  let joined=false,abortObserved=false,admitted=0
  try{
    const request=scope.fetchJson('http://127.0.0.1:1/api/v1/workspaces',async(_url,{signal})=>{
      await new Promise((yes,no)=>{
        const finish=()=>{abortObserved=true;setImmediate(()=>{joined=true;no(signal.reason)})}
        if(signal.aborted)finish();else signal.addEventListener('abort',finish,{once:true})
      })
    })
    cancelled.abort(Error('test abort causal'))
    await assert.rejects(request,/test abort causal/)
    assert.equal(abortObserved,true);assert.equal(joined,true,'request completion consumed before cleanup')
    assert.throws(()=>scope.control({control(){admitted++;return Promise.resolve()}},'status'),/test abort causal/)
    assert.equal(admitted,0)
    assert.ok(endsAt-performance.now()>39000,'late/cancelled actions retain native-close and deletion reserve')
  }finally{scope.dispose()}
  const late=archiveActionScope(performance.now()+39999,new AbortController().signal)
  try{assert.throws(()=>late.remaining(5000),/cutoff/)}finally{late.dispose()}
})

test('owner interface causal: late cleanup writes terminal diagnostics before retiring and keeps uncertainty reachable',async()=>{
  const input=new PassThrough(),owners=new Map(),id=randomUUID(),terminal=new AggregateError([Error('original startup failure')],'terminal failure preserved')
  let releaseReply,frame,cleanup=false,calls=0
  const output=new Writable({write(bytes,_encoding,done){frame=JSON.parse(bytes.toString());releaseReply=done}})
  const owner={
    status:()=>({recoveryState:'retained',pendingControls:cleanup?0:1,cleanupReceipt:cleanup?{actualHandleJoined:true,pipesClosed:true}:null}),
    diagnostic:()=>('late PG log; token=private-fixture-value\n'+'x'.repeat(9000)),
    async close(){calls++;if(cleanup)owners.delete(id);throw terminal}
  }
  owners.set(id,owner)
  const route=openOwnerRoute(input,output,owners)
  assert.equal(route.close(),false,'earlier failed-body cleanup cannot retire an uncertain owner')
  const send=requestId=>input.write(JSON.stringify({kind:'archive-qualification-owner',task,run:id,id:requestId,command:'retry-cleanup'})+'\n')
  send(1);await new Promise(done=>setImmediate(done))
  assert.equal(frame.status.cleanupReceipt,null);assert.equal(route.isOpen(),true)
  releaseReply();await new Promise(done=>setImmediate(done))
  assert.equal(route.isOpen(),true,'uncertain close remains reachable after its reply')
  cleanup=true;send(2);await new Promise(done=>setImmediate(done))
  assert.equal(owners.size,0);assert.equal(route.isOpen(),true,'registry removal is not written reply completion')
  assert.equal(frame.failure.message,'terminal failure preserved');assert.equal(frame.failure.errors[0].message,'original startup failure')
  assert.match(frame.diagnostic.stderr,/late PG log/);assert.ok(!frame.diagnostic.stderr.includes('private-fixture-value'));assert.ok(Buffer.byteLength(frame.diagnostic.stderr)<=8192)
  releaseReply();await new Promise(done=>setImmediate(done))
  assert.equal(route.isOpen(),false,'terminal reply completes before route retirement')
  assert.equal(calls,2);assert.equal(terminal.errors[0].message,'original startup failure')
  input.destroy();output.destroy()
})
