import test from 'node:test'
import assert from 'node:assert/strict'
import {spawn} from 'node:child_process'
import {once} from 'node:events'
import {createInterface} from 'node:readline'
import {createServer} from 'node:net'
import {createHash,randomUUID} from 'node:crypto'
import {createReadStream} from 'node:fs'
import {mkdir,realpath,lstat,readdir,rm,copyFile} from 'node:fs/promises'
import {resolve,relative,isAbsolute,sep} from 'node:path'
import {chromium} from '@playwright/test'

const repo=resolve(import.meta.dirname,'../../..')
const hash=value=>createHash('sha256').update(value).digest('hex')
const inside=(root,path)=>{const part=relative(root,path);return !isAbsolute(part)&&part!=='..'&&!part.startsWith('..'+sep)}
const pause=ms=>new Promise(resolve=>setTimeout(resolve,ms))
async function bounded(work,ms,label){let timer;try{return await Promise.race([work,new Promise((_,reject)=>{timer=setTimeout(()=>reject(Error(label+' timed out')),ms)})])}finally{clearTimeout(timer)}}
async function until(check,label,ms=15000){const end=Date.now()+ms;while(Date.now()<end){const value=await check();if(value)return value;await pause(50)}throw Error(label+' did not converge')}
async function digest(path){const result=createHash('sha256');for await(const bytes of createReadStream(path))result.update(bytes);return result.digest('hex')}

test('matching backend occurrence metadata drives production session cards details dock and sealed reload',{
 skip:!process.env.HMEM_OCCURRENCE_HARNESS,timeout:180000
},async()=>{
 const executable=await realpath(process.env.HMEM_OCCURRENCE_HARNESS),expectedHash=process.env.HMEM_OCCURRENCE_HARNESS_SHA256
 assert.ok(inside(repo,executable));assert.equal((await lstat(executable)).isFile(),true);assert.match(expectedHash,/^[a-f0-9]{64}$/);assert.equal(await digest(executable),expectedHash)
 const scratch=await realpath(process.env.HMEM_OCCURRENCE_TEST_ROOT);assert.ok(inside(resolve(repo,'.tmp'),scratch));assert.equal((await lstat(scratch)).isSymbolicLink(),false)
 const owned=resolve(scratch,'native-'+randomUUID());await mkdir(owned);for(const name of ['tmp','home','config','appdata','localappdata'])await mkdir(resolve(owned,name))
 const listener=createServer();await new Promise(resolve=>listener.listen(0,'127.0.0.1',resolve));const port=listener.address().port;await new Promise(resolve=>listener.close(resolve))
 const origin='http://127.0.0.1:'+port,allowed=new Set(['PATH','SYSTEMROOT','SYSTEMDRIVE','WINDIR','COMSPEC','PATHEXT','LANG','NUMBER_OF_PROCESSORS'])
 const env=Object.fromEntries(Object.entries(process.env).filter(([name])=>allowed.has(name.toUpperCase())))
 Object.assign(env,{TEMP:resolve(owned,'tmp'),TMP:resolve(owned,'tmp'),HOME:resolve(owned,'home'),USERPROFILE:resolve(owned,'home'),APPDATA:resolve(owned,'appdata'),LOCALAPPDATA:resolve(owned,'localappdata'),HMEM_CONFIG_DIR:resolve(owned,'config'),HMEM_TEST_REPO_ROOT:repo})
 const child=spawn(executable,['local','--interactive','--port',String(port),'+RTS','-N2','-RTS'],{cwd:repo,env,windowsHide:true,stdio:['pipe','pipe','pipe']})
 let stderr='',sandbox,staticDir,readyResolve,readyReject,browser,context,phase='startup',cleaned=false
 const ready=new Promise((yes,no)=>{readyResolve=yes;readyReject=no}),closed=once(child,'close')
 child.stderr.on('data',bytes=>{stderr=(stderr+bytes.toString()).slice(-2048)})
 child.on('error',error=>readyReject(error));child.on('close',code=>readyReject(Error('harness closed during startup: '+code)))
 const lines=createInterface({input:child.stdout});lines.on('line',line=>{if(line.startsWith('Sandbox root:'))sandbox=line.slice('Sandbox root:'.length).trim();if(line.startsWith('Static dir  :'))staticDir=line.slice('Static dir  :'.length).trim();if(line.startsWith('Press Enter'))readyResolve()})
 const errors=[]
 try{
  await bounded(ready,60000,'sandbox startup');assert.ok(sandbox&&inside(owned,await realpath(sandbox)))
  phase='served asset binding';const destination=await realpath(staticDir);assert.ok(inside(await realpath(sandbox),destination));assert.deepEqual(await readdir(destination),[])
  const source=resolve(repo,'hmem-server/static'),assets=await readdir(resolve(source,'assets'));assert.ok(assets.length>0&&assets.length<=128);await mkdir(resolve(destination,'assets'))
  const served=[]
  for(const path of ['index.html','hmem-runtime-config.js','hmem-icon.svg','hmem-logo.svg',...assets.filter(name=>/^[a-zA-Z0-9_.-]+\.(js|css)$/.test(name)).map(name=>'assets/'+name)]){
   assert.equal((await lstat(resolve(source,path))).isFile(),true);await copyFile(resolve(source,path),resolve(destination,path));const response=await fetch(origin+'/'+path,{signal:AbortSignal.timeout(15000)});assert.equal(response.status,200);const bytes=Buffer.from(await response.arrayBuffer());assert.ok(bytes.length<=8*1024**2);const sha256=await digest(resolve(source,path));assert.equal(hash(bytes),sha256);served.push({path,sha256})
  }
  const request=async(path,method='GET',body)=>{
   const response=await fetch(origin+path,{method,signal:AbortSignal.timeout(15000),headers:{Authorization:'Bearer sandbox-local-bot-token','Content-Type':'application/json','X-HMem-Bot-Identity':JSON.stringify({role:'Occurrence integration','model-id':'test:fixture:default'})},...(body===undefined?{}:{body:JSON.stringify(body)})})
   const text=await response.text();assert.ok(response.ok,method+' '+path+' returned '+response.status+' '+text.slice(0,300));return text?JSON.parse(text):null
  }
  const workspace=(await request('/api/v1/workspaces')).items[0].id,project=await request('/api/v1/projects','POST',{workspace_id:workspace,name:'Occurrence integration'})
  const makeTask=async title=>{const task=await request('/api/v1/tasks','POST',{workspace_id:workspace,project_id:project.id,title});return request('/api/v1/tasks/'+task.id,'PUT',{status:'in_progress'})}
  const task=await makeTask('Read canonical occurrence'),other=await makeTask('Link without changing occurrence'),base='/api/v1/workspaces/'+workspace+'/sessions'
  const makeSession=async title=>request(base,'POST',{id:randomUUID(),format_label:'codex-jsonl',title,task_id:task.id})
  let active=await makeSession('Occurrence live'),bytes=Buffer.alloc(0);const empty=await makeSession('Occurrence empty')
  const first='2026-10-30T12:00:00Z',older='2026-10-30T09:00:00Z',later='2026-10-30T15:00:00Z'
  const raw=Buffer.from(JSON.stringify({type:'response_item',timestamp:first,payload:{type:'message',role:'assistant',content:[{type:'output_text',text:'A visible message establishes the maximum.'}]}})+'\n'+
   '{"type":"event_msg","timestamp":"'+first+'","payload":{"type":"future_event","n":9007199254740993,"overflow":1e400}}\n'+
   JSON.stringify({type:'event_msg',timestamp:older,payload:{type:'older_final',result:42}})+'\n')
  const append=async addition=>{const input={expected_offset:active.committed_bytes,expected_revision:active.revision,data_base64:addition.toString('base64')};const result=await request(base+'/'+active.id+'/append','POST',input);active=result.session;bytes=Buffer.concat([bytes,addition]);return input}
  phase='initial occurrence';const replay=await append(raw)
  const metadata=()=>request(base+'/'+active.id),known=async expected=>{const value=await until(async()=>{const value=await metadata();return value.last_activity_at===expected?value:null},'canonical occurrence '+expected);active=value;return value}
  await known(first);assert.equal((await request(base+'?activity=live,quiet')).items.find(row=>row.id===active.id).last_activity_at,first)
  phase='transport-only controls';await request(base+'/'+active.id+'/heartbeat','POST');await request(base+'/'+active.id+'/runtime','PUT',{expected_runtime_revision:0,runtime_identity:{model:'declared test fixture',role:'integration'}});await request(base+'/'+active.id+'/tasks/'+other.id,'PUT')
  assert.equal((await request(base+'/'+active.id+'/append','POST',replay)).replayed,true);assert.equal((await metadata()).last_activity_at,first);assert.equal((await request(base+'/'+empty.id)).last_activity_at,null)
  phase='production browser';browser=await chromium.launch({headless:true});context=await browser.newContext({viewport:{width:1440,height:1000},timezoneId:'UTC',acceptDownloads:true});const page=await context.newPage()
  page.on('pageerror',error=>errors.push(String(error)))
  const start=async id=>{await page.goto(origin+'/workspace/'+workspace+'#tab=sessions&session='+id);await page.locator('.session-detail-header').waitFor({timeout:10000}).catch(async error=>{throw new Error(error.message+'; browser '+(await page.locator('body').innerText()).slice(-1500)+'; errors '+errors.join(';'))})}
  const card=()=>page.locator('.session-summary').filter({has:page.locator('a[href*="session='+active.id+'"]')})
  const cardTime=async expected=>until(async()=>await card().locator('.session-latest-activity time').getAttribute('datetime')===expected,'card occurrence')
  await start(active.id);await cardTime(first)
  assert.equal(await page.locator('.session-detail-header .session-latest-activity time').getAttribute('datetime'),first)
  await until(async()=>await page.locator('.session-activity-group > summary time').getAttribute('datetime')===older,'group final time')
  await page.locator('.session-activity-group > summary').click();await page.locator('.session-content-event').first().locator('summary').click();assert.match(await page.locator('session-event-json pre').first().textContent(),/9007199254740993/)
  for(const width of [1440,430]){await page.setViewportSize({width,height:1000});assert.ok(await page.evaluate(()=>document.documentElement.scrollWidth<=innerWidth));await page.screenshot({path:resolve(owned,'session-'+width+'.png'),fullPage:false})}
  phase='unopened advance';await page.getByRole('link',{name:'Close session',exact:true}).click();await append(Buffer.from(JSON.stringify({type:'event_msg',timestamp:later,payload:{type:'later_event',result:true}})+'\n'));await known(later);await cardTime(later)
  await append(Buffer.from(JSON.stringify({type:'event_msg',timestamp:older,payload:{type:'older_arrival',result:true}})+'\n'));await known(later);await cardTime(later)
  await append(Buffer.from(JSON.stringify({type:'event_msg',payload:{type:'undated_final',result:true}})+'\n'));await known(later);await card().locator('a').click();await page.locator('.session-activity-group > summary').getByText('Time unavailable',{exact:true}).waitFor()
  phase='dock';await page.getByRole('button',{name:'Projects',exact:true}).click();await page.locator('#entity-'+project.id+' .tree-toggle').click();const taskCard=page.locator('#entity-'+task.id);await taskCard.waitFor();await taskCard.locator('.task-sessions-icon').click()
  await until(async()=>await taskCard.locator('.task-session-choice').filter({hasText:'Occurrence live'}).locator('.session-latest-activity time').getAttribute('datetime')===later,'dock occurrence')
  phase='sealing';active=await metadata();const sealed=await request(base+'/'+active.id+'/terminate','POST',{expected_offset:active.committed_bytes,expected_revision:active.revision,final_bytes:bytes.length,final_sha256:hash(bytes)});assert.equal(sealed.last_activity_at,later)
  await start(active.id);await page.getByText('Verified complete archive.',{exact:true}).waitFor({timeout:20000});assert.equal(await page.locator('.session-detail-header .session-latest-activity time').getAttribute('datetime'),later)
  await page.locator('.session-activity-group > summary').getByText('Time unavailable',{exact:true}).waitFor();await page.reload();await page.getByText('Verified complete archive.',{exact:true}).waitFor()
  assert.equal(await page.locator('.session-detail-header .session-latest-activity time').getAttribute('datetime'),later)
  const downloadReady=page.waitForEvent('download');await page.getByRole('button',{name:/Download session/}).click();const download=await downloadReady,parts=[];for await(const part of await download.createReadStream())parts.push(part);assert.deepEqual(Buffer.concat(parts),bytes)
  phase='historical import';const imported=await request(base+'/imports','POST',{session:{id:randomUUID(),format_label:'codex-jsonl',title:'Occurrence imported',task_id:task.id},final_bytes:raw.length,final_sha256:hash(raw)})
  const importedAppend=(await request(base+'/'+imported.id+'/append','POST',{expected_offset:0,expected_revision:imported.revision,data_base64:raw.toString('base64')})).session
  const importedSealed=await request(base+'/'+imported.id+'/terminate','POST',{expected_offset:raw.length,expected_revision:importedAppend.revision,final_bytes:raw.length,final_sha256:hash(raw)});assert.equal(importedSealed.last_activity_at,first)
  await start(imported.id);await page.getByText('Verified complete archive.',{exact:true}).waitFor({timeout:20000});assert.equal(await page.locator('.session-detail-header .session-latest-activity time').getAttribute('datetime'),first)
  phase='empty';await start(empty.id);await page.locator('.session-detail-header [aria-label="Last activity unavailable"]').waitFor();assert.equal(await page.locator('.session-detail-header .session-latest-activity time').count(),0)
  assert.deepEqual(errors,[])
  console.log(JSON.stringify({scope:'actual matching backend HTTP and production browser occurrence flow',harnessSha256:expectedHash,served,bytes:bytes.length,sha256:hash(bytes),visuals:[resolve(owned,'session-1440.png'),resolve(owned,'session-430.png')],checks:['list/detail/dock','transport/replay','unopened later/older/undated','group-final versus session-max','sealed reload/download','historical imported evidence','empty','1440/430 layout']}))
 }catch(error){throw new Error('Occurrence integration phase '+phase+': '+error.message,{cause:error})}
 finally{
  await context?.close();await browser?.close();child.stdin.end('\n');const [code]=await bounded(closed,45000,'sandbox cleanup');lines.close();assert.equal(code,0,'harness exit: '+stderr.slice(-600));
  if(sandbox)await assert.rejects(lstat(sandbox),{code:'ENOENT'})
  const actual=await realpath(owned);assert.ok(inside(scratch,actual));assert.equal((await lstat(actual)).isSymbolicLink(),false)
  // Screenshots are task-owned visual QA inputs; retain only until inspection.
  for(const name of await readdir(actual))if(!/^session-(1440|430)\.png$/.test(name)){
   assert.ok(['tmp','home','config','appdata','localappdata'].includes(name),'unexpected task-owned output')
   const target=resolve(actual,name);assert.ok(inside(actual,target));assert.equal((await lstat(target)).isSymbolicLink(),false);await rm(target,{recursive:true})
  }
  cleaned=true
 }
 assert.equal(cleaned,true)
})
