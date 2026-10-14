// One unqualified scenario; never records, aggregates, retries or alters budgets.
import fs from 'node:fs'
import os from 'node:os'
import path from 'node:path'
import { createHash } from 'node:crypto'
import { chromium } from '@playwright/test'
import { createLiveDiagnostics, serializeLiveDiagnostic } from './live-diagnostics.mjs'
import { generateFixture } from './fixtures.mjs'
import { retireOwnedResources, hashJson, HARNESS_CONFIGURATION } from './contracts.mjs'
import { measureRun, staticServer, inputQualification } from './harness.mjs'

if(process.argv.length!==2 || process.env.HMEM_EVIDENCE_BASE_COMMIT!=='33abd80dd13dbc6c850397eeb13f79047d0f7c91')throw new Error('Use the due-removal base and no driver arguments')
const owner='1e420484-81b9-4251-b485-82abca34c800'
const parent=fs.realpathSync(os.tmpdir())
const archive=path.join(parent,'hmem-due-removal-failure-'+owner+'-20261014T213454Z')
const output=path.join(archive,'live-causal-diagnostic.json')
const expected=['failure-owner.json','final-working-tree.after.due-removal.v1.json','final-working-tree.complete.due-removal.v1.diff','final-working-tree.evidence-manifest.due-removal.v1.json','final-working-tree.trace-manifest.due-removal.v1.json','final-working-tree.validation-record.due-removal.v1.json'].sort()
const digest=b=>createHash('sha256').update(b).digest('hex')
function verifyArchive(){
  if(fs.realpathSync(archive)!==archive||fs.lstatSync(archive).isSymbolicLink()||JSON.stringify(fs.readdirSync(archive).sort())!==JSON.stringify(expected))throw new Error('Unexpected archive ownership or diagnostic already exists; no rerun')
  for(const name of expected){const info=fs.lstatSync(path.join(archive,name));if(info.isSymbolicLink()||!info.isFile()||info.size>(name==='failure-owner.json'?65536:64*1024*1024))throw new Error('Archive entry bound/type')}
  const metadata=JSON.parse(fs.readFileSync(path.join(archive,'failure-owner.json'),'utf8'))
  if(metadata.ownerTaskId!==owner||metadata.projectId!=='9f1a1a40-afc5-4825-851c-b71aaf498b58'||metadata.originals.length!==5)throw new Error('Archive owner mismatch')
  for(const original of metadata.originals){const file=path.join(archive,path.basename(original.path));if(!expected.includes(path.basename(file))||digest(fs.readFileSync(file))!==original.sha256||fs.statSync(file).size!==original.sizeBytes)throw new Error('Original failure receipt changed')}
}
verifyArchive()
const inputs=inputQualification(), collector=createLiveDiagnostics()
let server=null,browserServer=null,browser=null,result=null,failure=null,retirement
const startedAtUtc=new Date().toISOString()
try {
  server=await staticServer()
  browserServer=await chromium.launchServer({headless:true,args:['--js-flags=--expose-gc','--enable-precise-memory-info']})
  console.log('DIAGNOSTIC_ONLY_OWNED_CHROMIUM_PID='+browserServer.process()?.pid)
  browser=await chromium.connect(browserServer.wsEndpoint())
  const environment={os:`${os.type()} ${os.release()} ${os.arch()}`,cpu:`${os.cpus()[0]?.model||'unknown'} (${os.cpus().length} logical)`,ramBytes:os.totalmem(),node:process.version,chromium:browser.version(),viewport:`${HARNESS_CONFIGURATION.viewport.width}x${HARNESS_CONFIGURATION.viewport.height}`,headless:true}
  const run=await measureRun(browser,server.origin,generateFixture('large'),false,false,[],collector)
  result={environment,live:collector.snapshot(),liveBatch:{ms:run.liveBatch.ms,requests:run.liveBatch.requests,bytes:run.liveBatch.bytes,routes:run.liveBatch.routes,stabilityMs:run.liveBatch.stabilityMs,consoleErrors:run.consoleErrors}}
}catch(error){failure=error}
finally {
  retirement=await retireOwnedResources([
    {resource:'browser connection',close:async()=>{if(browser)await browser.close()}},
    {resource:'owned Chromium process',close:async()=>{if(browserServer){await browserServer.kill();const child=browserServer.process();if(child&&child.exitCode==null&&child.signalCode==null)throw new Error('Owned Chromium still alive')}}},
    {resource:'owned HTTP server',close:async()=>{if(server){server.server.closeAllConnections();await new Promise((resolve,reject)=>server.server.close(error=>error?reject(error):resolve()))}}}
  ],5000)
}
verifyArchive()
if(JSON.stringify(inputQualification())!==JSON.stringify(inputs))failure ||= new Error('Diagnostic inputs changed')
const receipt={schemaVersion:1,qualified:false,diagnosticTaskId:'ebeaaaf7-c364-4dc4-8755-b8dc96ec0df8',ownerTaskId:owner,projectId:'9f1a1a40-afc5-4825-851c-b71aaf498b58',invocation:'HMEM_EVIDENCE_BASE_COMMIT=33abd80dd13dbc6c850397eeb13f79047d0f7c91 node perf/diagnose-live.mjs',scenario:'exactly one unchanged large measureRun; no warmup, trace, qualification, or retry',startedAtUtc,finishedAtUtc:new Date().toISOString(),inputQualification:inputs,inputFingerprint:hashJson(inputs),result,retirement,failure:failure?.message?.slice(0,1024)||null,retention:'Final task '+owner+'; dispose at issue resolution or task/project closure',interpretation:'Readiness observations bracket existing evaluations. Fulfilment-to-readiness gaps do not separate browser decode, Elm update/render, scheduling or polling.'}
const bytes=serializeLiveDiagnostic(receipt)
fs.writeFileSync(output,bytes,{flag:'wx'})
console.log(JSON.stringify({output,sizeBytes:Buffer.byteLength(bytes),sha256:digest(bytes),qualified:false,retirementPassed:retirement.passed,failure:receipt.failure,liveMs:result?.liveBatch.ms}))
process.exitCode=failure||!retirement.passed?1:0
