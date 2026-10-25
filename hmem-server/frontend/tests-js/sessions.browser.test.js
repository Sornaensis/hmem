import assert from 'node:assert/strict'
import test from 'node:test'
import { spawn, execFile } from 'node:child_process'
import { once } from 'node:events'
import { createInterface } from 'node:readline'
import { createServer } from 'node:net'
import { promisify } from 'node:util'
import { access, readFile, readdir, realpath, writeFile, appendFile } from 'node:fs/promises'
import { join, resolve, relative, delimiter } from 'node:path'
import { tmpdir } from 'node:os'
import { createHash, randomUUID } from 'node:crypto'
import { chromium, request } from '@playwright/test'
import { startSessionMcp, closeSessionMcps } from './support/session-mcp.js'
import { openSessionExplorer } from './support/sessions-ux-fixture.mjs'

const repo = resolve(import.meta.dirname, '../../..')
const frontend = resolve(import.meta.dirname, '..')
const pause = ms => new Promise(done => setTimeout(done, ms))
async function bounded(promise, ms, label) {
  let timer
  try { return await Promise.race([promise, new Promise((_, reject) => { timer = setTimeout(() => reject(new Error(label + ' timed out')), ms) })]) }
  finally { clearTimeout(timer) }
}
async function until(predicate, label, ms = 20000) {
  const end = Date.now() + ms
  while (Date.now() < end) { if (await predicate()) return; await pause(25) }
  throw new Error(label + ' did not converge')
}
async function currentTaskCanonicalReady(page, workspace, task) {
  await until(async () => {
    const url = new URL(page.url()), fragment = new URLSearchParams(url.hash.slice(1))
    return url.pathname === '/workspace/' + workspace && fragment.get('tab') === 'projects'
      && fragment.get('focus') === 'task:' + task
      && await page.locator('.connection-status.connected').count() === 1
  }, 'current task canonical readiness')
}
function requestScope(request) {
  const scope = request.postDataJSON()?.scope
  return { scope: scope?.scope, workspace: scope?.workspace_id ?? null }
}
async function port() {
  const server = createServer()
  await new Promise(done => server.listen(0, '127.0.0.1', done))
  const number = server.address().port
  await new Promise(done => server.close(done))
  return number
}
const sha = bytes => createHash('sha256').update(bytes).digest('hex')

test('production Sessions uses canonical authenticated resources and direct task links', { timeout: 240000 }, async () => {
  let harness, harnessClosed, build, buildClosed, browser, context, api, sandbox, postgresPid, page
  let failed
  const mcpClients=[]
  const reads = [],archiveTransport=[],noteArchive=event=>{archiveTransport.push(event);if(archiveTransport.length>32)archiveTransport.shift()};let sessionReadCount=0,harnessDiagnostic='',archivePreparationStart=0,documentGeneration=0
  try {
    const { stdout } = await promisify(execFile)('stack', ['path', '--local-install-root'], { cwd: repo, windowsHide: true, timeout: 30000 })
    const executable = join(stdout.trim(), 'bin', process.platform === 'win32' ? 'hmem-test-harness.exe' : 'hmem-test-harness')
    const executableHash = sha(await readFile(executable))
    const mcpExecutable=join(stdout.trim(),'bin',process.platform==='win32'?'hmem-mcp.exe':'hmem-mcp'),mcpHash=sha(await readFile(mcpExecutable))
    if(process.env.SESSION_STACK_HARNESS_SHA256)assert.equal(executableHash,process.env.SESSION_STACK_HARNESS_SHA256,'qualified current-source harness')
    if(process.env.SESSION_STACK_MCP_SHA256)assert.equal(mcpHash,process.env.SESSION_STACK_MCP_SHA256,'qualified current-source MCP executable')
    const number = await port(), origin = 'http://127.0.0.1:' + number
    const coordinates = {}
    harness = spawn(executable, ['deployed', '--interactive', '--port', String(number)], { cwd: repo, env: process.env, windowsHide: true, stdio: ['pipe','pipe','pipe'] })
    harnessClosed = once(harness, 'close'); harness.stderr.on('data',chunk=>{harnessDiagnostic=(harnessDiagnostic+chunk.toString()).slice(-2048)})
    createInterface({ input: harness.stdout }).on('line', line => {
      for (const [label,key] of [['Sandbox root:','root'],['Static dir  :','static'],['Server URL  :','url']]) if (line.startsWith(label)) coordinates[key] = line.slice(label.length).trim()
      if (line.startsWith('Press Enter')) coordinates.ready = true
      if (line.startsWith('Bearer PAT:')) coordinates.token=line.slice('Bearer PAT:'.length).trim()
    })
    await until(() => coordinates.ready || harness.exitCode !== null, 'sandbox startup', 120000)
    assert.equal(harness.exitCode, null); assert.equal(coordinates.url, origin)
    sandbox = await realpath(coordinates.root)
    const temporary = await realpath(tmpdir())
    assert.ok(relative(temporary,sandbox).startsWith('hmem-sandbox-'))
    assert.equal(relative(sandbox,await realpath(coordinates.static)), 'static')
    postgresPid = Number((await readFile(join(sandbox,'postgres/data/postmaster.pid'),'utf8')).split('\n')[0])
    const env = { ...process.env, VITE_HMEM_API_URL:origin, VITE_HMEM_WS_URL:'ws://127.0.0.1:'+number+'/api/v1/ws', VITE_HMEM_AUTH_MODE:'deployed' }
    const pathKey = Object.keys(env).find(key => key.toLowerCase() === 'path') || 'PATH'
    env[pathKey] = join(frontend,'node_modules/.bin') + delimiter + (env[pathKey] || '')
    build = spawn(process.execPath,['node_modules/vite/bin/vite.js','build','--outDir',coordinates.static,'--emptyOutDir'],{cwd:frontend,env,windowsHide:true,stdio:['ignore','pipe','pipe']})
    buildClosed = once(build,'close')
    let diagnostic = ''
    for (const stream of [build.stdout,build.stderr]) stream.on('data', chunk => { diagnostic = (diagnostic + chunk.toString()).slice(-4096) })
    assert.equal((await bounded(buildClosed,60000,'production build'))[0],0,diagnostic)
    const assets = Object.fromEntries(await Promise.all((await readdir(join(coordinates.static,'assets'))).map(async name => [name,sha(await readFile(join(coordinates.static,'assets',name)))])))
    api = await request.newContext({baseURL:origin,extraHTTPHeaders:{Cookie:'hmem_session=sandbox-session-token; hmem_csrf=sandbox-csrf-token','X-CSRF-Token':'sandbox-csrf-token'}})
    async function call(path,method='GET',data) {
      const response = await api.fetch(path,{method,data})
      assert.ok(response.ok(),method+' '+path+' returned '+response.status())
      const bytes = await response.body()
      return bytes.length ? JSON.parse(bytes.toString('utf8')) : null
    }
    const workspace = await call('/api/v1/workspaces','POST',{name:'Sessions browser fixture',workspace_type:'repository'})
    const id = randomUUID(), base = '/api/v1/workspaces/'+workspace.id+'/sessions'
    const task = await call('/api/v1/tasks','POST',{workspace_id:workspace.id,title:'Inspect session evidence'})
    const secondTask=await call('/api/v1/tasks','POST',{workspace_id:workspace.id,title:'Review shared capture'})
    for(const running of [task,secondTask])await call('/api/v1/tasks/'+running.id,'PUT',{status:'in_progress'})
    const identity={role:'Session workflow qualification','model-id':'test:production:default'}
    async function connectMcp(){
      assert.ok(coordinates.token,'sandbox seeded PAT available privately')
      const client=await startSessionMcp({executable:mcpExecutable,origin,token:coordinates.token,workspace:workspace.id,identity})
      mcpClients.push(client);return client
    }
    let uploader=await connectMcp()
    const managing=await connectMcp(),source=join(sandbox,'producer.jsonl')
    const bytes = Buffer.from('{"type":"message","role":"assistant","text":"Hello from a real session λ"}\r\n["unknown",42]\n','utf8')
    await writeFile(source,bytes)
    const started=await uploader.tool('session_upload_start',{session_id:id,path:source,format_label:'Provider-neutral JSONL',title:'Inspect live task session',task_id:task.id})
    assert.equal(started.status,'validating')
    async function uploaded(client,sid,offset){await until(async()=>{const status=await client.tool('session_upload_status',{session_id:sid});assert.notEqual(status.status,'failed','follower must not fail');return status.committed_offset===offset},'follower canonical committed offset',30000)}
    async function receipt(client,sid,control){
      const accepted=await client.tool(control,{session_id:sid});let result
      await until(async()=>{const state=await client.tool('session_upload_status',{session_id:sid});const operation=state.operation;assert.notEqual(state.status,'failed');if(operation?.id===accepted.requested_operation.id&&!operation.pending){result=operation.receipt;return true}return false},'finite verified capture receipt',30000)
      assert.equal(result.source_verified,true);return result
    }
    await uploaded(uploader,id,bytes.length)
    await managing.tool('session_associate',{session_id:id,task_id:task.id})
    await managing.tool('session_associate',{session_id:id,task_id:secondTask.id})
    const taskStatus=(await call('/api/v1/tasks/'+task.id)).status
    browser = await chromium.launch({headless:true})
    context = await browser.newContext({viewport:{width:1440,height:1000}})
    const sockets=[];let invalidResume=false,resyncs=0,ticketConflicts=0
    await context.routeWebSocket('**/api/v1/ws*', socket=>{
      const generation=documentGeneration
      const server=socket.connectToServer();sockets.push(socket)
      server.onMessage(message=>{if(archivePreparationStart){try{const frame=JSON.parse(String(message));if(frame.type==='resync_required'||(frame.type==='checkpoint'&&frame.catch_up==='complete'))noteArchive({phase:frame.type==='checkpoint'?'canonical-checkpoint':'canonical-resync-required',document:generation,at:Date.now()})}catch{}}socket.send(message)})
    })
    await context.route('**/api/v1/change-stream/ticket',async route=>{
      const body=route.request().postDataJSON()
      if(invalidResume&&body.scope.workspace_id===workspace.id){invalidResume=false;body.resume_token='expired-browser-resume';await route.continue({postData:JSON.stringify(body)})}
      else await route.continue()
    })
    // Production downloads use the browser's native OPFS staging and verified
    // bytes. The test reads the actual resulting browser download independently.
    await context.addInitScript(()=>{
      // Observe native XHR completion before Elm's subsequently registered
      // load/error listener can admit its replacement. Playwright's remote
      // requestfinished notification can arrive after that replacement starts.
      const NativeXHR=window.XMLHttpRequest,active=new Set(),metrics={peak:0,events:[]};let next=0
      window.__sessionHTTPMetrics=metrics
      window.XMLHttpRequest=class extends NativeXHR{
        constructor(){super();const finish=event=>{if(active.delete(this)){metrics.events.push({id:this.metricId,phase:event.type,path:this.metricPath,time:performance.now()});if(metrics.events.length>64)metrics.events.shift()}}
          for(const event of ['load','error','timeout','abort'])this.addEventListener(event,finish)
        }
        open(method,url,...rest){this.metricPath=new URL(url,location.href).pathname;return super.open(method,url,...rest)}
        send(...args){if(this.metricPath.includes('/sessions')){this.metricId=++next;active.add(this);metrics.peak=Math.max(metrics.peak,active.size);metrics.events.push({id:this.metricId,phase:'send',path:this.metricPath,time:performance.now()});if(metrics.events.length>64)metrics.events.shift()}return super.send(...args)}
      }
      const archive=window.__archiveTrace=[],record=phase=>{archive.push({...phase,at:Date.now()});if(archive.length>24)archive.shift()}
      const nativeFetch=window.fetch
      window.fetch=async function(url,options){if(!String(url).endsWith('/evidence.zip'))return nativeFetch.call(this,url,options)
        window.__archiveSignal=options?.signal
        options?.signal?.addEventListener('abort',()=>record({phase:'fetch-abort',reason:options.signal.reason?.name}))
        const response=await nativeFetch.call(this,url,options),getReader=response.body?.getReader.bind(response.body)
        if(getReader)response.body.getReader=(...args)=>{const reader=getReader(...args),read=reader.read.bind(reader);reader.read=async()=>{try{return await read()}catch(error){record({phase:'body-read-error',name:error.name,message:error.message});throw error}};return reader};return response
      }
      document.addEventListener('click',event=>{const button=event.target.closest?.('[data-session-archive]');if(button)record({phase:'document-click',enabled:!button.disabled,operation:JSON.parse(button.dataset.sessionArchive).operation})},true)
      const get=StorageManager.prototype.getDirectory
      StorageManager.prototype.getDirectory=async function(...args){record({phase:'storage-acquire'});const root=await get.apply(this,args),directory=await root.getDirectoryHandle('hmem-downloads',{create:true}),file=directory.getFileHandle.bind(directory)
        directory.getFileHandle=async(...fileArgs)=>{const handle=await file(...fileArgs),create=handle.createWritable.bind(handle);handle.createWritable=async(...writeArgs)=>{const writer=await create(...writeArgs),write=writer.write.bind(writer),close=writer.close.bind(writer);writer.write=async bytes=>{await write(bytes);record({phase:'native-write',bytes:bytes.length})};writer.close=async()=>{await close();record({phase:'native-close'})};return writer};return handle};return {getDirectoryHandle:async()=>directory}
      }

    })
    await context.addCookies([{name:'hmem_session',value:'sandbox-session-token',url:origin},{name:'hmem_csrf',value:'sandbox-csrf-token',url:origin}])
    page = await context.newPage()
    const nativeReceipts=[]
    async function completeDocument(label){const metrics=await page.evaluate(()=>window.__sessionHTTPMetrics);assert.ok(metrics.peak<=2,JSON.stringify({label,...metrics}));nativeReceipts.push({label,peak:metrics.peak})}
    page.on('response', response => {
      const path=new URL(response.url()).pathname
      if(path.endsWith('/evidence.zip'))noteArchive({phase:'response',status:response.status(),at:Date.now()})
      if(archivePreparationStart&&path.endsWith('/change-stream/resync'))noteArchive({phase:'canonical-resync-response',...requestScope(response.request()),status:response.status(),document:documentGeneration,at:Date.now()})
      if(path.includes('/sessions')) { sessionReadCount++;reads.push({path,status:response.status()}); if(reads.length>32) reads.shift() }
      if(path.endsWith('/change-stream/resync')&&response.ok())resyncs++
      if(path.endsWith('/change-stream/ticket')&&response.status()===409)ticketConflicts++
    })
    const errors = [],inflight=new Map(),peakSamples=[];let peakRequests=0
    page.on('request',request=>{
      // Full document replacement creates a new Elm application lifetime.
      // Late browser completion notifications from the retired document do
      // not belong to the new application's physical admission map.
      if(request.isNavigationRequest()&&request.frame()===page.mainFrame())documentGeneration++
      if(archivePreparationStart&&new URL(request.url()).pathname.endsWith('/change-stream/resync'))noteArchive({phase:'canonical-resync-request',...requestScope(request),document:documentGeneration,at:Date.now()})
      if(new URL(request.url()).pathname.includes('/sessions')){
        inflight.set(request,documentGeneration)
        const current=[...inflight].filter(([,generation])=>generation===documentGeneration)
        if(current.length>peakRequests){peakRequests=current.length;peakSamples.push(current.map(([item])=>new URL(item.url()).pathname));if(peakSamples.length>8)peakSamples.shift()}
      }
    })
    const finished=request=>inflight.delete(request)
    page.on('requestfinished',finished);page.on('requestfailed',finished)
    page.on('pageerror', error => errors.push(error.message))
    await page.goto(origin+'/workspace/'+workspace.id+'#tab=sessions&session='+id)
    await page.getByRole('heading',{name:'Sessions',exact:true}).waitFor()
    assert.equal(await page.getByRole('button',{name:'Live',exact:true}).getAttribute('aria-pressed'),'true')
    assert.equal(await page.getByLabel('Status',{exact:true}).count(),0)
    await page.getByRole('link',{name:task.title,exact:false}).waitFor()
    await until(async()=>await page.locator('.session-summary').count()===1,'initial canonical session list')
    await until(async()=>await page.locator('.session-record').count()===1,'initial byte viewer hides unknown JSON')
    assert.equal(await page.locator('.session-record-text').first().innerText(),'Hello from a real session λ')
    const extra=Buffer.from('{"text":"Arrived while following"}\n','utf8')
    await Promise.all([appendFile(source,extra),uploader.tool('task_detail',{task_id:task.id})])
    await uploaded(uploader,id,bytes.length+extra.length)
    await until(async()=>await page.locator('.session-record').count()===2,'follow catches up without duplicate records')
    assert.equal(await page.locator('.session-record-text').last().innerText(),'Arrived while following')
    await uploader.close({hard:true})
    assert.equal((await call(base+'/'+id)).state,'active','actual process loss never seals')
    const eof=Buffer.from('{"text":"Finite EOF 雪"}')
    await appendFile(source,eof)
    uploader=await connectMcp()
    await uploader.tool('session_upload_start',{session_id:id,path:source,format_label:'Provider-neutral JSONL',title:'Inspect live task session',task_id:task.id})
    await uploaded(uploader,id,bytes.length+extra.length)
    const flushed=await receipt(uploader,id,'session_upload_flush')
    assert.equal(flushed.committed_offset,bytes.length+extra.length+eof.length)
    assert.equal((await call(base+'/'+id)).state,'active','finite newline-free flush never seals')
    const separator=Buffer.from('\r\ntrue\n')
    await appendFile(source,separator)
    await uploaded(uploader,id,bytes.length+extra.length+eof.length+separator.length)
    const stopped=await receipt(uploader,id,'session_upload_stop')
    assert.equal((await call(base+'/'+id)).state,'active','stop never seals')
    const original=Buffer.concat([bytes,extra,eof,separator])
    assert.equal(stopped.committed_offset,original.length);assert.equal(stopped.sha256,sha(original))
    await appendFile(source,Buffer.from('{"text":"Later producer output is outside captured evidence"}\n'))
    await managing.tool('session_terminate',{session_id:id,expected_offset:stopped.committed_offset,expected_revision:stopped.revision,final_bytes:stopped.committed_offset,final_sha256:stopped.sha256})
    await managing.tool('session_terminate',{session_id:id,expected_offset:stopped.committed_offset,expected_revision:stopped.revision,final_bytes:stopped.committed_offset,final_sha256:stopped.sha256})
    const lateTask=await call('/api/v1/tasks','POST',{workspace_id:workspace.id,title:'Later evidence association'})
    await call('/api/v1/tasks/'+lateTask.id,'PUT',{status:'in_progress'})
    await managing.tool('session_associate',{session_id:id,task_id:lateTask.id})
    assert.equal((await call('/api/v1/tasks/'+task.id)).status,taskStatus,'capturing and sealing do not change task status')
    async function evidence(sid,expectedBytes,expectedHash){
      let next={name:'session_evidence',arguments:{session_id:sid,limit:65536}},offset=0;const hash=createHash('sha256')
      for(let pages=0;next;pages++){
        assert.ok(pages<256,'bounded evidence continuation count')
        const value=await managing.tool(next.name,next.arguments),part=Buffer.from(value.data_base64,'base64')
        assert.equal(value.offset,offset);assert.equal(value.next_offset,offset+part.length);assert.ok(part.length<=65536)
        hash.update(part);offset+=part.length;next=value.next_request
      }
      assert.equal(offset,expectedBytes);assert.equal(hash.digest('hex'),expectedHash)
    }
    await evidence(id,original.length,sha(original))
    const smallDownloadEvent=page.waitForEvent('download')
    await page.getByRole('button',{name:/Download session/}).click()
    const smallDownload=await smallDownloadEvent
    const downloadedSmall=await readFile(await smallDownload.path())
    const savedBytes=downloadedSmall.length
    const savedHash=createHash('sha256').update(downloadedSmall)
    assert.equal(savedBytes,original.length);assert.equal(savedHash.digest('hex'),sha(original))
    const taskLink=page.getByRole('link',{name:task.title,exact:false})
    if(await taskLink.count()===0)await page.getByRole('button',{name:'Show All Linked Tasks',exact:true}).click()
    await taskLink.click()
    await until(() => page.url().includes('focus=task'), 'task navigation')
    const focusedCard=page.locator('#entity-'+task.id)
    await focusedCard.locator('.btn-extras-toggle').click()
    await focusedCard.getByRole('button',{name:'Evidence',exact:true}).click()
    await focusedCard.locator('.task-session-evidence summary').click()
    await focusedCard.locator('.task-session-evidence a').first().waitFor()
    await focusedCard.locator('.task-session-evidence a').first().click()
    await until(()=>page.url().includes('session='+id),'task evidence navigates back to exact session')
    console.log(JSON.stringify({phase:'small-live-navigation-save',savedBytes,sha256:sha(original)}))
    // Larger than the viewer window and the four raw-range retention slots.
    // A near-1MiB record outside the tail stays in evidence, and a second
    // occurrence at the tail must survive several bounded HTTP chunks.
    const largeId=randomUUID(),parts=[],hash=createHash('sha256');let count=0,revision=0
    await call(base,'POST',{id:largeId,format_label:'Large arbitrary JSONL',title:'Inspect large task transcript',task_id:task.id})
    await managing.tool('session_associate',{session_id:largeId,task_id:task.id})
    assert.equal((await managing.tool('session_list',{task_id:task.id})).items.length,2,'many sessions link to one task')
    assert.equal((await managing.tool('session_tasks',{session_id:id})).items.length,3,'one session links to several tasks including a late link')
    async function appendPart(part){
      parts.push({offset:count,length:part.length});hash.update(part)
      const value=await call(base+'/'+largeId+'/append','POST',{expected_offset:count,expected_revision:revision,data_base64:part.toString('base64'),allow_final_record:false})
      count=value.session.committed_bytes;revision=value.session.revision
    }
    for(let i=0;i<4;i++)await appendPart(Buffer.from(JSON.stringify({type:'unknown-provider',text:'prefix-'+i+' '+ 'p'.repeat(480000)})+'\r\n'))
    const largeOffset=count,largeRecord=Buffer.from(JSON.stringify({text:'boundary '+ 'λ'.repeat(510000)})+'\r\n')
    // One canonical JSONL append; the viewer's 64KiB reads split the record.
    await appendPart(largeRecord)
    for(let i=0;i<9;i++)await appendPart(Buffer.from(JSON.stringify({text:'tail-'+i+' '+ 't'.repeat(260000)})+'\n'))
    const tailBoundaryOffset=count
    await appendPart(largeRecord)
    assert.ok(largeOffset<count-2097152,'the earlier identical occurrence remains outside the visible tail')
    assert.ok(tailBoundaryOffset>=count-2097152&&largeRecord.length>15*65536,'the final near1MiB occurrence is fully inside the finite tail and crosses many chunks')
    const largeHash=hash.digest('hex')
    let firstWindowFailed=false
    await context.route('**/sessions/'+largeId+'/transcript?*',async route=>{
      if(!firstWindowFailed){firstWindowFailed=true;await route.fulfill({status:500,contentType:'application/json',body:'{"error":"test_unavailable"}'})}
      else await route.continue()
    })
    await completeDocument('small-viewer-and-save')
    await page.goto(origin+'/workspace/'+workspace.id+'#tab=sessions&session='+largeId)
    await page.getByRole('alert').getByRole('button',{name:'Retry',exact:true}).click()
    await until(async()=>await page.locator('.session-record').filter({hasText:'boundary'}).count()===1,'bounded initial tail with large framed record',30000)
    assert.ok(await page.locator('.session-record').count()<=200)
    const viewport=page.locator('#session-records')
    const rows=await page.locator('.session-record').allTextContents()
    const snapshots=resyncs;invalidResume=true
    for(const socket of sockets.splice(0))await socket.close({code:1012,reason:'bounded recovery test'})
    await until(()=>ticketConflicts>0&&resyncs>snapshots,'genuine server resume rejection and canonical snapshot',30000)
    await until(async()=>await page.locator('.session-record').filter({hasText:'boundary'}).count()===1,'one large record after canonical resync',30000)
    assert.deepEqual(await page.locator('.session-record').allTextContents(),rows,'canonical snapshot restores current tail rows')
    console.log(JSON.stringify({phase:'following-canonical-resync',ticketConflicts,resyncs}))
    const offsets=await page.locator('.session-record').evaluateAll(elements=>elements.map(element=>element.getAttribute('data-offset')))
    assert.equal(new Set(offsets).size,offsets.length,'overlap contains no duplicate record offsets')
    const boundary=page.locator('.session-record').filter({hasText:'boundary'})
    assert.equal(await boundary.locator('.session-record-text').textContent(),JSON.parse(largeRecord.toString('utf8')).text)
    await call(base+'/'+largeId+'/terminate','POST',{expected_offset:count,expected_revision:revision,final_bytes:count,final_sha256:largeHash})
    const largeDownloadEvent=page.waitForEvent('download')
    await page.getByRole('button',{name:/Download session/}).click()
    const largeDownload=await largeDownloadEvent
    const downloadedLarge=await readFile(await largeDownload.path())
    const largeSavedBytes=downloadedLarge.length,independent=createHash('sha256').update(downloadedLarge)
    assert.equal(largeSavedBytes,count);assert.equal(independent.digest('hex'),largeHash)
    await evidence(largeId,count,largeHash)
    console.log(JSON.stringify({phase:'large-tail-window-and-verified-save',largeSavedBytes,largeHash,largeRecordBytes:largeRecord.length}))
    await page.setViewportSize({width:430,height:900})
    await page.locator('.session-detail').scrollIntoViewIfNeeded()
    assert.equal(await page.evaluate(()=>document.documentElement.scrollWidth<=innerWidth),true,'narrow layout does not overflow horizontally')
    await page.setViewportSize({width:1440,height:1000})
    // Filtering is performed before pagination even when both sealed sessions
    // sit behind more than fifty newer active summaries.
    for(let i=0;i<51;i++)await call(base,'POST',{id:randomUUID(),format_label:'Provider-neutral JSONL',title:'Empty active task session '+i,task_id:task.id})
    await page.getByRole('link',{name:'Close session',exact:true}).click()
    await until(async()=>await page.locator('.session-summary').count()===50,'first bounded fifty summaries')
    assert.equal((await call(base+'?state=terminated')).items.length,2,'saved filter remains available through canonical API')
    await page.getByRole('button',{name:'Quiet',exact:true}).click()
    await page.getByText('No sessions match these filters.',{exact:true}).waitFor()
    let heldRelease,heldStarted=false
    const held=new Promise(done=>{heldRelease=done})
    await context.route('**/sessions?*',async route=>{
      const url=new URL(route.request().url())
      if(!heldStarted&&url.searchParams.get('state')==='active'&&url.searchParams.get('freshness')==='fresh'){
        heldStarted=true
        const response=await route.fetch()
        await bounded(held,15000,'held canonical list response')
        await route.fulfill({response})
      }else await route.continue()
    })
    await page.getByRole('button',{name:'Live',exact:true}).click()
    await until(()=>heldStarted,'controlled canonical list request')
    await page.getByText('Loading sessions…',{exact:true}).waitFor()
    await page.getByRole('button',{name:'Quiet',exact:true}).click()
    await page.getByText('No sessions match these filters.',{exact:true}).waitFor()
    heldRelease()
    await pause(150)
    assert.equal(await page.locator('.session-summary').count(),0,'late old-query callback cannot replace current filtered page')
    await context.unroute('**/sessions?*')
    await page.getByRole('button',{name:'Live',exact:true}).click()
    await until(async()=>await page.locator('.session-summary').count()===50,'active page after stale callback')
    await page.getByRole('button',{name:'Older sessions',exact:true}).click()
    await until(async()=>await page.locator('.session-summary').count()===1,'keyset continuation final page')
    await page.locator('.session-summary a').click()
    await page.getByText('This session has no committed records.',{exact:true}).waitFor()
    await page.locator('.session-task-card').filter({hasText:task.title}).waitFor()
    assert.equal(await page.locator('.session-task-card').count(),1,'new empty sessions retain their required initial task')
    const empty=await call('/api/v1/workspaces','POST',{name:'Empty sessions scope',workspace_type:'repository'})
    await completeDocument('large-viewer-save-and-filter-paging')
    await page.goto(origin+'/workspace/'+empty.id+'#tab=sessions')
    await page.getByText('No sessions match these filters.',{exact:true}).waitFor()
    assert.equal(await page.locator('.session-record').count(),0,'workspace switch retires transcript display')
    const hiddenReads=sessionReadCount
    await page.getByRole('button',{name:'Projects',exact:true}).click()
    await pause(1200)
    assert.equal(sessionReadCount,hiddenReads,'hidden tab performs no session refresh')
    await completeDocument('empty-workspace-and-hidden-tab')
    // Independently seal through the ordinary managing client while a real
    // uploader is still alive and caught up; later source output is refused.
    const liveId=randomUUID(),livePath=join(sandbox,'live-producer.jsonl'),liveBytes=Buffer.from('null\r\n')
    await writeFile(livePath,liveBytes)
    await uploader.tool('session_upload_start',{session_id:liveId,path:livePath,format_label:'Provider-neutral JSONL',title:'Verify independent task session seal',task_id:secondTask.id})
    await uploaded(uploader,liveId,liveBytes.length)
    const live=await managing.tool('session_detail',{session_id:liveId})
    const liveSession=live.session??live
    await managing.tool('session_terminate',{session_id:liveId,expected_offset:liveSession.committed_bytes,expected_revision:liveSession.revision,final_bytes:liveBytes.length,final_sha256:sha(liveBytes)})
    await appendFile(livePath,Buffer.from('false\n'))
    await until(async()=>(await uploader.tool('session_upload_status',{session_id:liveId})).status==='terminated','live follower discovers normal MCP termination',30000)
    assert.equal((await call(base+'/'+liveId)).committed_bytes,liveBytes.length)
    // One binary, authenticated server export; the client never enumerates
    // evidence or assembles ZIP entries. Include more than one metadata page.
    const archiveExpected={[id]:sha(original),[largeId]:largeHash}
    for(let index=0;index<51;index++){
      const sid=randomUUID(),data=index%3===0?Buffer.alloc(0):Buffer.from(JSON.stringify({text:index%3===1?'雪 café':'repeated '.repeat(16000)})+'\r\n')
      await call(base,'POST',{id:sid,format_label:'Review / café ../capture',title:'Review captured task evidence',task_id:task.id})
      let offset=0,rev=0
      if(data.length){const appended=await call(base+'/'+sid+'/append','POST',{expected_offset:0,expected_revision:0,data_base64:data.toString('base64'),allow_final_record:false});offset=appended.session.committed_bytes;rev=appended.session.revision}
      await call(base+'/'+sid+'/terminate','POST',{expected_offset:offset,expected_revision:rev,final_bytes:data.length,final_sha256:sha(data)})
      await managing.tool('session_associate',{session_id:sid,task_id:task.id});archiveExpected[sid]=sha(data)
    }
    const excluded=randomUUID(),excludedBytes=Buffer.from('true\n')
    await call(base,'POST',{id:excluded,format_label:'Provider-neutral JSONL',title:'Only the other task',task_id:secondTask.id})
    const excludedAppend=await call(base+'/'+excluded+'/append','POST',{expected_offset:0,expected_revision:0,data_base64:excludedBytes.toString('base64'),allow_final_record:false})
    await call(base+'/'+excluded+'/terminate','POST',{expected_offset:excludedAppend.session.committed_bytes,expected_revision:excludedAppend.session.revision,final_bytes:excludedBytes.length,final_sha256:sha(excludedBytes)})
    await managing.tool('session_associate',{session_id:excluded,task_id:secondTask.id})
    // The harness PAT is a bot credential; declare its identity for the normal
    // POST change-stream bootstrap as required by the existing API contract.
    archivePreparationStart=Date.now()
    await context.setExtraHTTPHeaders({'X-HMem-Bot-Identity':JSON.stringify(identity)})
    await context.clearCookies()
    await page.evaluate(token=>localStorage.setItem('hmem-auth-token',token),coordinates.token)
    await page.goto(origin+'/workspace/'+workspace.id+'#tab=projects&focus=task:'+task.id)
    const archiveCard=page.locator('#entity-'+task.id)
    await archiveCard.locator('.btn-extras-toggle').click()
    await archiveCard.getByRole('button',{name:'Evidence',exact:true}).click()
    const archiveGroup=archiveCard.locator('.task-session-evidence')
    await archiveGroup.locator('summary').click()
    await archiveGroup.getByRole('button',{name:'↓ Download all',exact:true}).waitFor()
    // REST-backed cards can render before this new PAT document's initial
    // canonical snapshot retires prior download authority. Await live scopes.
    await currentTaskCanonicalReady(page,workspace.id,task.id)
    noteArchive({phase:'current-canonical-ready',workspace:workspace.id,document:documentGeneration,at:Date.now()})
    let binaryRequests=0
    const binaryHeaders=[]
    page.on('request',request=>{if(new URL(request.url()).pathname.endsWith('/evidence.zip')){binaryRequests++;binaryHeaders.push(request.allHeaders());noteArchive({phase:'request',at:Date.now()})}})
    page.on('requestfinished',request=>{if(new URL(request.url()).pathname.endsWith('/evidence.zip'))noteArchive({phase:'body-finished',at:Date.now()})})
    page.on('requestfailed',request=>{if(new URL(request.url()).pathname.endsWith('/evidence.zip'))noteArchive({phase:'body-failed',error:request.failure()?.errorText,at:Date.now()})})
    const archiveDownloadEvent=page.waitForEvent('download')
    await archiveGroup.getByRole('button',{name:'↓ Download all',exact:true}).click()
    const archiveDownload=await archiveDownloadEvent,archivePath=await archiveDownload.path()
    for(const headers of await Promise.all(binaryHeaders)){assert.ok(headers.authorization?.startsWith('Bearer '));assert.equal(headers.cookie,undefined)}
    const checked=await promisify(execFile)('python',['-c',`import sys,json,zipfile,hashlib
expected=json.loads(sys.argv[2])
with zipfile.ZipFile(sys.argv[1]) as z:
 assert z.testzip() is None
 assert len(z.namelist())==len(set(z.namelist()))==53
 for item in z.infolist():
  assert item.compress_type==zipfile.ZIP_DEFLATED
  key=next((key for key in expected if item.filename.endswith(key+'.jsonl')),None)
  assert key is not None and '/' not in item.filename and not item.filename.startswith('.')
  assert hashlib.sha256(z.read(item)).hexdigest()==expected[key]
 assert sum(i.compress_size for i in z.infolist())<sum(i.file_size for i in z.infolist())
print('independently verified 53 DEFLATE entries')`,archivePath,JSON.stringify(archiveExpected)],{windowsHide:true})
    assert.equal(binaryRequests,1,'one task-only server archive request')
    await until(async()=>await archiveGroup.getByRole('button',{name:'↓ Download all',exact:true}).isEnabled(),'archive completion')
    console.log(JSON.stringify({phase:'server-DEFLATE-task-evidence',entries:53,binaryRequests,verification:checked.stdout.trim(),zipBytes:(await readFile(archivePath)).length,auth:'bearer-only',transport:archiveTransport,native:await page.evaluate(()=>window.__archiveTrace)}))
    assert.deepEqual(errors,[])
    console.log(JSON.stringify({harness:executableHash,mcp:mcpHash,assets,chromium:browser.version(),viewport:[1440,1000],narrow:[430,900],phase:'follower-MCP-browser-resume-independent-seal-and-exact-evidence',savedBytes,sha256:sha(original),largeSavedBytes,largeHash,largeOffset,tailBoundaryOffset,largeRecordBytes:largeRecord.length,resyncs,ticketConflicts,nativeReceipts,playwrightPeak:peakRequests,summaries:53,download:'native-OPFS-and-browser-download'}))
  } catch (error) {
    noteArchive({phase:'catch-enter',at:Date.now()})
    if(page) {
      console.log(JSON.stringify({phase:'bounded-failure-diagnostic',path:await page.evaluate(()=>location.pathname+location.hash),reads,records:await page.locator('.session-record').count(),summaries:await page.locator('.session-summary').count(),status:(await page.locator('.session-download-status,.task-evidence-download').allTextContents()).slice(0,2),archiveTransport,archiveNative:await page.evaluate(()=>window.__archiveTrace),signal:await page.evaluate(()=>({aborted:window.__archiveSignal?.aborted,reason:window.__archiveSignal?.reason?.name})),harnessDiagnostic:harnessDiagnostic.replace(/Bearer\s+\S+/gi,'Bearer [redacted]')}))
      if(process.env.SESSION_STACK_DIAGNOSTICS)await page.screenshot({path:join(process.env.SESSION_STACK_DIAGNOSTICS,'sessions-stack-failure.png'),fullPage:true})
    }
    failed = error; throw error
  }
  finally {
    noteArchive({phase:'cleanup-begin',at:Date.now()})
    const failures = []
    async function finish(action) { try { await action() } catch(error) { failures.push(error.message) } }
    await finish(async () => { if(context) await bounded(context.close(),10000,'context close'); if(browser) await bounded(browser.close(),10000,'browser close') })
    await finish(async () => { if(api) await api.dispose() })
    await finish(async () => { await closeSessionMcps(mcpClients) })
    await finish(async () => { if(build && build.exitCode === null) { build.kill(); await bounded(buildClosed,10000,'build join') } })
    await finish(async () => { if(harness && harness.exitCode === null) { harness.stdin.end('\n'); await bounded(harnessClosed,30000,'sandbox cleanup') } })
    await finish(async () => { if(sandbox) { let remains=true; try { await access(sandbox) } catch(error) { if(error.code==='ENOENT') remains=false; else throw error } assert.equal(remains,false,'owned sandbox removed') } })
    await finish(async () => { if(postgresPid) { let alive=true; try { process.kill(postgresPid,0) } catch(error) { if(error.code==='ESRCH') alive=false; else throw error } assert.equal(alive,false,'owned PostgreSQL process exited') } })
    if(failed)console.log(JSON.stringify({phase:'archive-cleanup-attribution',archiveTransport}))
    if(failures.length) throw new AggregateError([...(failed?[failed]:[]),...failures.map(value => new Error(value))],'Sessions fixture cleanup failed')
  }
})

test('archive readiness waits for the current task initial canonical resync', { timeout: 30000 }, async () => {
  const h=await openSessionExplorer(),{page}=h,workspace=h.fixture.workspace.id,task=h.fixture.tasks[0].id
  let release,held=false,ready=false,canonical
  const barrier=new Promise(resolve=>{release=resolve})
  try {
    await page.route('**/api/v1/change-stream/resync',async route=>{
      if(!held&&requestScope(route.request()).workspace===workspace){held=true;await barrier}
      await route.fallback()
    })
    await h.start('tab=projects&focus=task:'+task,task)
    await until(()=>held,'held initial workspace resync')
    const card=page.locator('#entity-'+task)
    await card.locator('.btn-extras-toggle').click()
    await card.getByRole('button',{name:'Evidence',exact:true}).click()
    const evidence=card.locator('.task-session-evidence')
    await evidence.locator('summary').click()
    await evidence.getByRole('button',{name:'↓ Download all',exact:true}).waitFor()
    canonical=currentTaskCanonicalReady(page,workspace,task).then(()=>{ready=true})
    await page.evaluate(()=>new Promise(resolve=>requestAnimationFrame(()=>requestAnimationFrame(resolve))))
    assert.equal(await page.locator('.connection-status.connected').count(),0)
    assert.equal(ready,false,'a rendered evidence card does not establish canonical readiness')
    release()
    await canonical
    assert.equal(ready,true)
    assert.equal(await page.locator('.connection-status.connected').count(),1)
    assert.deepEqual(h.errors,[])
  } finally { release();await canonical?.catch(()=>{});await h.close() }
})
