import assert from 'node:assert/strict'
import {spawn} from 'node:child_process'
import {createInterface} from 'node:readline'
import {resolve} from 'node:path'
import {fileURLToPath} from 'node:url'

export function ownOperatorInput(operator,childInput){
  let admissionSent=false
  return {
    get admissionSent(){return admissionSent},
    holdAdmission(){
      assert.equal(admissionSent,false)
      admissionSent=true
      operator.pipe(childInput,{end:false})
    },
    failBeforeJoin(){
      if(!admissionSent)childInput.end()
    },
    joined(){
      operator.unpipe(childInput)
      operator.pause()
      operator.unref?.()
      childInput.end()
    }
  }
}

function readPacket(line){
  try{return JSON.parse(line.replace(/^#\s+/,''))}catch{return null}
}

async function run(vector){
  assert.match(vector||'',/^(?:sanitized-(?:1|100|1024)|comparison-(?:100|1024)|historical-100)MiB$/)
  const frontend=fileURLToPath(new URL('../../',import.meta.url))
  const task='eab4ad17-d4bf-4153-814f-a3f1543768a7'
  const child=spawn(process.execPath,[
    '--test','--test-isolation=none',
    '--test-name-pattern=^integrated archive qualifier owns its sandbox',
    'tests-js/session-archive.integration.browser.test.js'
  ],{cwd:frontend,env:{...process.env,HMEM_ARCHIVE_INTEGRATION_MODE:vector,HMEM_SANITIZED_ARCHIVE_OWNER_PIPE:'1'},
    stdio:['pipe','pipe','pipe'],windowsHide:true})
  const operator=ownOperatorInput(process.stdin,child.stdin)
  const joined=new Promise(done=>child.once('close',(code,signal)=>done({code,signal})))
  let runId,admitted=false,failure,recoveryRequested=false
  function fail(error){
    if(!failure){
      failure=error
      process.stderr.write(JSON.stringify({archive_qualification_supervisor:true,task,run:runId??null,
        reason:String(error.message||error).slice(0,512),operatorStdinRetained:operator.admissionSent})+'\n')
    }
    operator.failBeforeJoin()
  }
  const admissionTimer=setTimeout(()=>fail(Error('No verified operator admission within 30 seconds')),30000)
  const send=(id,command,fields={})=>child.stdin.write(JSON.stringify({kind:'archive-qualification-owner',task,run:runId,id,command,...fields})+'\n')
  const output=(async()=>{
    for await(const line of createInterface({input:child.stdout})){
      process.stdout.write(line+'\n')
      const frame=readPacket(line)
      if(!frame?.archive_qualification_owner)continue
      try{
        if(frame.commands){
          assert.equal(frame.task,task);assert.equal(runId,undefined)
          assert.match(frame.run,/^[a-f0-9-]{36}$/);runId=frame.run;send(1,'status')
        }else if(frame.id===1){
          assert.equal(frame.run,runId);assert.equal(frame.status.filesCreated,false);assert.equal(frame.status.nativeCreated,false)
          // The child may own resources as soon as it receives admit, before
          // its acknowledgement reaches us. Keep the actual pipe from then on.
          operator.holdAdmission()
          send(2,'admit',{ack_id:1})
        }else if(frame.id===2){
          assert.equal(frame.run,runId);assert.equal(frame.admitted,true);admitted=true;clearTimeout(admissionTimer)
        }else if(frame.id===3){
          assert.equal(frame.run,runId)
          const receipt=frame.status?.cleanupReceipt
          assert.ok(!frame.failure&&receipt?.actualHandleJoined&&receipt.pipesClosed&&receipt.sandboxRemoved&&frame.status.pendingControls===0,
            'Automatic cleanup retry remains unresolved. Use this supervisor stdin for same-owner status, retry-cleanup or close with a request ID of 100 or more.')
        }
      }catch(error){fail(error)}
    }
  })().catch(fail)
  const diagnostics=(async()=>{
    for await(const line of createInterface({input:child.stderr})){
      process.stderr.write(line+'\n')
      const frame=readPacket(line)
      if(!recoveryRequested&&frame?.archive_qualification_diagnostic&&frame.run===runId&&frame.phase?.recoveryState==='retained'){
        recoveryRequested=true;send(3,'retry-cleanup')
      }
    }
  })().catch(fail)
  child.on('error',fail)
  child.stdin.on('error',fail)
  const [{code,signal}]=await Promise.all([joined,output,diagnostics])
  clearTimeout(admissionTimer)
  operator.joined()
  if(failure)throw failure
  assert.equal(admitted,true,'actual operator acknowledgement was required')
  assert.equal(signal,null)
  process.exitCode=code??1
}

if(process.argv[1]&&resolve(process.argv[1])===fileURLToPath(import.meta.url))await run(process.argv[2])
