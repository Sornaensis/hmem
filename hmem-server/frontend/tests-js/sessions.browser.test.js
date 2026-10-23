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
  const reads = [];let sessionReadCount=0
  try {
    const { stdout } = await promisify(execFile)('stack', ['path', '--local-install-root'], { cwd: repo, windowsHide: true, timeout: 30000 })
    const executable = join(stdout.trim(), 'bin', process.platform === 'win32' ? 'hmem-test-harness.exe' : 'hmem-test-harness')
    const executableHash = sha(await readFile(executable))
    const mcpExecutable=join(stdout.trim(),'bin',process.platform==='win32'?'hmem-mcp.exe':'hmem-mcp'),mcpHash=sha(await readFile(mcpExecutable))
    const number = await port(), origin = 'http://127.0.0.1:' + number
    const coordinates = {}
    harness = spawn(executable, ['deployed', '--interactive', '--port', String(number)], { cwd: repo, env: process.env, windowsHide: true, stdio: ['pipe','pipe','pipe'] })
    harnessClosed = once(harness, 'close'); harness.stderr.resume()
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
    const started=await uploader.tool('session_upload_start',{session_id:id,path:source,format_label:'Provider-neutral JSONL'})
    assert.equal(started.status,'validating')
    async function uploaded(client,sid,offset){await until(async()=>{const status=await client.tool('session_upload_status',{session_id:sid});assert.notEqual(status.status,'failed','follower must not fail');return status.committed_offset===offset},'follower canonical committed offset',30000)}
    async function receipt(client,sid,control){
      const accepted=await client.tool(control,{session_id:sid});let result
      await until(async()=>{const state=await client.tool('session_upload_status',{session_id:sid});const operation=state.operation;assert.notEqual(state.status,'failed');if(operation?.id===accepted.requested_operation.id&&!operation.pending){result=operation.receipt;return true}return false},'finite verified capture receipt',30000)
      assert.equal(result.source_verified,true);return result
    }
    await uploaded(uploader,id,bytes.length)
    const task = await call('/api/v1/tasks','POST',{workspace_id:workspace.id,title:'Inspect session evidence'})
    const secondTask=await call('/api/v1/tasks','POST',{workspace_id:workspace.id,title:'Review shared capture'})
    await managing.tool('session_associate',{session_id:id,task_id:task.id})
    await managing.tool('session_associate',{session_id:id,task_id:secondTask.id})
    const taskStatus=(await call('/api/v1/tasks/'+task.id)).status
    browser = await chromium.launch({headless:true})
    context = await browser.newContext({viewport:{width:1440,height:1000}})
    const sockets=[];let invalidResume=false,resyncs=0,ticketConflicts=0
    await context.routeWebSocket('**/api/v1/ws*', socket=>{
      const server=socket.connectToServer();sockets.push(socket)
      server.onMessage(message=>socket.send(message))
    })
    await context.route('**/api/v1/change-stream/ticket',async route=>{
      const body=route.request().postDataJSON()
      if(invalidResume&&body.scope.workspace_id===workspace.id){invalidResume=false;body.resume_token='expired-browser-resume';await route.continue({postData:JSON.stringify(body)})}
      else await route.continue()
    })
    // Only the chooser is substituted: this returns a real OPFS file handle
    // and its native writable. HTTP bytes, production writer/hash and close
    // remain unchanged. The native OS chooser is deliberately not exercised.
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
      window.showSaveFilePicker = async()=>{
        const root=await navigator.storage.getDirectory()
        const handle=await root.getFileHandle('session-evidence.jsonl',{create:true})
        window.__sessionEvidence=handle
        return handle
      }
    })
    await context.addCookies([{name:'hmem_session',value:'sandbox-session-token',url:origin},{name:'hmem_csrf',value:'sandbox-csrf-token',url:origin}])
    page = await context.newPage()
    const nativeReceipts=[]
    async function completeDocument(label){const metrics=await page.evaluate(()=>window.__sessionHTTPMetrics);assert.ok(metrics.peak<=2,JSON.stringify({label,...metrics}));nativeReceipts.push({label,peak:metrics.peak})}
    page.on('response', response => {
      const path=new URL(response.url()).pathname
      if(path.includes('/sessions')) { sessionReadCount++;reads.push({path,status:response.status()}); if(reads.length>32) reads.shift() }
      if(path.endsWith('/change-stream/resync')&&response.ok())resyncs++
      if(path.endsWith('/change-stream/ticket')&&response.status()===409)ticketConflicts++
    })
    const errors = [],inflight=new Map(),peakSamples=[];let peakRequests=0,documentGeneration=0
    page.on('request',request=>{
      // Full document replacement creates a new Elm application lifetime.
      // Late browser completion notifications from the retired document do
      // not belong to the new application's physical admission map.
      if(request.isNavigationRequest()&&request.frame()===page.mainFrame())documentGeneration++
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
    assert.equal(await page.getByLabel('Lifecycle',{exact:true}).evaluate(element=>element.selectedOptions[0]?.textContent),'All')
    assert.equal(await page.getByLabel('Heartbeat',{exact:true}).evaluate(element=>element.selectedOptions[0]?.textContent),'All')
    await page.getByRole('link',{name:task.title,exact:true}).waitFor()
    assert.equal(await page.locator('.session-summary').count(),1)
    await until(async()=>await page.locator('.session-record').count()===2,'initial byte viewer')
    assert.equal(await page.locator('.session-record-text').first().innerText(),'Hello from a real session λ')
    await page.getByRole('button',{name:'Pause',exact:true}).click()
    const extra=Buffer.from('{"text":"Arrived while paused"}\n','utf8')
    await Promise.all([appendFile(source,extra),uploader.tool('task_detail',{task_id:task.id})])
    await uploaded(uploader,id,bytes.length+extra.length)
    await page.getByText(extra.length+' new bytes',{exact:false}).waitFor()
    assert.equal(await page.locator('.session-record').count(),2,'pause preserves the readable window')
    await page.getByRole('button',{name:'Follow tail',exact:true}).click()
    await until(async()=>await page.locator('.session-record').count()===3,'follow catches up without duplicate records')
    assert.equal(await page.locator('.session-record-text').last().innerText(),'Arrived while paused')
    await uploader.close({hard:true})
    assert.equal((await call(base+'/'+id)).state,'active','actual process loss never seals')
    const eof=Buffer.from('{"text":"Finite EOF 雪"}')
    await appendFile(source,eof)
    uploader=await connectMcp()
    await uploader.tool('session_upload_start',{session_id:id,path:source,format_label:'Provider-neutral JSONL'})
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
    await page.getByRole('button',{name:'Save original evidence',exact:true}).waitFor()
    await page.getByRole('button',{name:'Save original evidence',exact:true}).click()
    await page.getByText('Saved '+original.length+' original bytes. SHA-256 verified.',{exact:true}).waitFor()
    const savedSize=await page.evaluate(async()=> (await window.__sessionEvidence.getFile()).size)
    const savedHash=createHash('sha256');let savedBytes=0
    for(let offset=0;offset<savedSize;offset+=65536){
      const part=await page.evaluate(async({offset})=>Array.from(new Uint8Array(await (await window.__sessionEvidence.getFile()).slice(offset,offset+65536).arrayBuffer())),{offset})
      savedHash.update(Buffer.from(part));savedBytes+=part.length
    }
    assert.equal(savedBytes,original.length);assert.equal(savedHash.digest('hex'),sha(original))
    await page.screenshot({path:join(tmpdir(),'sessions-first-page.png'),fullPage:true})
    await page.getByRole('link',{name:task.title,exact:true}).click()
    await until(() => page.url().includes('focus=task'), 'task navigation')
    await page.getByRole('region',{name:'Task sessions and evidence'}).getByRole('link').first().waitFor()
    await page.getByRole('region',{name:'Task sessions and evidence'}).getByRole('link').first().click()
    await until(()=>page.url().includes('session='+id),'task evidence navigates back to exact session')
    console.log(JSON.stringify({phase:'small-live-pause-navigation-save',savedBytes,sha256:sha(original)}))
    // Larger than the viewer window and the four raw-range retention slots.
    // The near-1MiB record is unreachable from the initial tail window and
    // must become readable through the bounded older-window boundary.
    const largeId=randomUUID(),parts=[],hash=createHash('sha256');let count=0,revision=0
    await call(base,'POST',{id:largeId,format_label:'Large arbitrary JSONL'})
    await managing.tool('session_associate',{session_id:largeId,task_id:task.id})
    assert.equal((await managing.tool('session_list',{task_id:task.id})).items.length,2,'many sessions link to one task')
    assert.equal((await managing.tool('session_tasks',{session_id:id})).items.length,3,'one session links to several tasks including a late link')
    async function appendPart(part){
      parts.push({offset:count,length:part.length});hash.update(part)
      const value=await call(base+'/'+largeId+'/append','POST',{expected_offset:count,expected_revision:revision,data_base64:part.toString('base64'),allow_final_record:false})
      count=value.session.committed_bytes;revision=value.session.revision
    }
    for(let i=0;i<4;i++)await appendPart(Buffer.from(JSON.stringify({type:'unknown-provider',text:'prefix-'+i+' '+ 'p'.repeat(480000)})+'\r\n'))
    const largeOffset=count,largeRecord=Buffer.from(JSON.stringify('boundary '+ 'λ'.repeat(510000))+'\r\n')
    // One canonical JSONL append; the viewer's 64KiB reads split the record.
    await appendPart(largeRecord)
    for(let i=0;i<9;i++)await appendPart(Buffer.from(JSON.stringify(['tail-'+i,'t'.repeat(260000)])+'\n'))
    const largeHash=hash.digest('hex')
    let firstWindowFailed=false
    await context.route('**/sessions/'+largeId+'/transcript?*',async route=>{
      if(!firstWindowFailed){firstWindowFailed=true;await route.fulfill({status:500,contentType:'application/json',body:'{"error":"test_unavailable"}'})}
      else await route.continue()
    })
    await completeDocument('small-viewer-and-save')
    await page.goto(origin+'/workspace/'+workspace.id+'#tab=sessions&session='+largeId)
    await page.getByRole('alert').getByRole('button',{name:'Retry',exact:true}).click()
    await until(async()=>await page.locator('.session-record').count()===8,'bounded initial tail window',30000)
    assert.ok(await page.locator('.session-record').count()<=100)
    await page.getByRole('button',{name:'Pause',exact:true}).click()
    const viewport=page.locator('#session-records')
    await viewport.evaluate(element=>{element.scrollTop=37})
    const anchor=await viewport.evaluate(element=>element.scrollTop),rows=await page.locator('.session-record').allTextContents()
    const snapshots=resyncs;invalidResume=true
    for(const socket of sockets.splice(0))await socket.close({code:1012,reason:'bounded recovery test'})
    await until(()=>ticketConflicts>0&&resyncs>snapshots,'genuine server resume rejection and canonical snapshot',30000)
    await page.getByRole('button',{name:'Follow tail',exact:true}).waitFor()
    await pause(300)
    assert.deepEqual(await page.locator('.session-record').allTextContents(),rows,'accepted snapshot preserves paused rows')
    assert.equal(await viewport.evaluate(element=>element.scrollTop),anchor,'late callbacks preserve paused scroll anchor')
    console.log(JSON.stringify({phase:'paused-canonical-resync',ticketConflicts,resyncs,anchor}))
    await page.getByRole('button',{name:'Load older window',exact:true}).click()
    await until(async()=>await page.locator('.session-record').filter({hasText:'boundary'}).count()===1,'one large record across more than four chunks',30000)
    const offsets=await page.locator('.session-record').evaluateAll(elements=>elements.map(element=>element.getAttribute('data-offset')))
    assert.equal(new Set(offsets).size,offsets.length,'overlap contains no duplicate record offsets')
    const boundary=page.locator('.session-record').filter({hasText:'boundary'})
    await boundary.locator('summary').click()
    assert.equal(await boundary.locator('.session-record-raw').textContent(),largeRecord.toString('utf8'))
    await call(base+'/'+largeId+'/terminate','POST',{expected_offset:count,expected_revision:revision,final_bytes:count,final_sha256:largeHash})
    await page.getByRole('button',{name:'Save original evidence',exact:true}).click()
    await page.getByText('Saved '+count+' original bytes. SHA-256 verified.',{exact:true}).waitFor({timeout:60000})
    const largeSavedSize=await page.evaluate(async()=> (await window.__sessionEvidence.getFile()).size),independent=createHash('sha256');let largeSavedBytes=0
    for(let offset=0;offset<largeSavedSize;offset+=65536){
      const part=await page.evaluate(async offset=>Array.from(new Uint8Array(await (await window.__sessionEvidence.getFile()).slice(offset,offset+65536).arrayBuffer())),offset)
      independent.update(Buffer.from(part));largeSavedBytes+=part.length
    }
    assert.equal(largeSavedBytes,count);assert.equal(independent.digest('hex'),largeHash)
    await evidence(largeId,count,largeHash)
    console.log(JSON.stringify({phase:'large-older-window-and-verified-save',largeSavedBytes,largeHash,largeRecordBytes:largeRecord.length}))
    await page.screenshot({path:join(tmpdir(),'sessions-large-desktop.png'),fullPage:true})
    await page.setViewportSize({width:430,height:900})
    await page.locator('.session-detail').scrollIntoViewIfNeeded()
    await page.screenshot({path:join(tmpdir(),'sessions-large-narrow.png'),fullPage:true})
    assert.equal(await page.evaluate(()=>document.documentElement.scrollWidth<=innerWidth),true,'narrow layout does not overflow horizontally')
    await page.setViewportSize({width:1440,height:1000})
    // Filtering is performed before pagination even when both sealed sessions
    // sit behind more than fifty newer active summaries.
    for(let i=0;i<51;i++)await call(base,'POST',{id:randomUUID(),format_label:'Empty active '+i})
    await page.getByRole('link',{name:'Close session',exact:true}).click()
    await until(async()=>await page.locator('.session-summary').count()===50,'first bounded fifty summaries')
    await page.getByLabel('Lifecycle',{exact:true}).selectOption('terminated')
    await until(async()=>await page.locator('.session-summary').count()===2,'sealed filter before pagination')
    await page.getByLabel('Lifecycle',{exact:true}).selectOption('active')
    await until(async()=>await page.locator('.session-summary').count()===50,'active first page')
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
    await page.getByLabel('Heartbeat',{exact:true}).selectOption('fresh')
    await until(()=>heldStarted,'controlled canonical list request')
    await page.getByText('Loading sessions…',{exact:true}).waitFor()
    await page.getByLabel('Lifecycle',{exact:true}).selectOption('terminated')
    await page.getByLabel('Heartbeat',{exact:true}).selectOption('')
    await until(async()=>await page.locator('.session-summary').count()===2,'new filter while old callback is held')
    heldRelease()
    await pause(150)
    assert.equal(await page.locator('.session-summary').count(),2,'late old-query callback cannot replace current filtered page')
    await context.unroute('**/sessions?*')
    await page.getByLabel('Lifecycle',{exact:true}).selectOption('active')
    await until(async()=>await page.locator('.session-summary').count()===50,'active page after stale callback')
    await page.getByRole('button',{name:'Older sessions',exact:true}).click()
    await until(async()=>await page.locator('.session-summary').count()===1,'keyset continuation final page')
    await page.locator('.session-summary a').click()
    await page.getByText('This session has no committed records.',{exact:true}).waitFor()
    await page.getByText('No linked tasks.',{exact:true}).waitFor()
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
    await uploader.tool('session_upload_start',{session_id:liveId,path:livePath,format_label:'Live independent seal'})
    await uploaded(uploader,liveId,liveBytes.length)
    const live=await managing.tool('session_detail',{session_id:liveId})
    const liveSession=live.session??live
    await managing.tool('session_terminate',{session_id:liveId,expected_offset:liveSession.committed_bytes,expected_revision:liveSession.revision,final_bytes:liveBytes.length,final_sha256:sha(liveBytes)})
    await appendFile(livePath,Buffer.from('false\n'))
    await until(async()=>(await uploader.tool('session_upload_status',{session_id:liveId})).status==='terminated','live follower discovers normal MCP termination',30000)
    assert.equal((await call(base+'/'+liveId)).committed_bytes,liveBytes.length)
    assert.deepEqual(errors,[])
    console.log(JSON.stringify({harness:executableHash,mcp:mcpHash,assets,chromium:browser.version(),viewport:[1440,1000],narrow:[430,900],phase:'follower-MCP-browser-resume-independent-seal-and-exact-evidence',savedBytes,sha256:sha(original),largeSavedBytes,largeHash,largeOffset,largeRecordBytes:largeRecord.length,resyncs,ticketConflicts,nativeReceipts,playwrightPeak:peakRequests,summaries:53,chooser:'OPFS-handle-only',nativeChooserTested:false}))
  } catch (error) {
    if(page) {
      console.log(JSON.stringify({phase:'bounded-failure-diagnostic',path:await page.evaluate(()=>location.pathname+location.hash),reads,records:await page.locator('.session-record').count(),summaries:await page.locator('.session-summary').count(),status:(await page.locator('.session-save [role=status]').allTextContents()).slice(0,2)}))
      await page.screenshot({path:join(tmpdir(),'sessions-first-page-failure.png'),fullPage:true})
    }
    failed = error; throw error
  }
  finally {
    const failures = []
    async function finish(action) { try { await action() } catch(error) { failures.push(error.message) } }
    await finish(async () => { if(context) await bounded(context.close(),10000,'context close'); if(browser) await bounded(browser.close(),10000,'browser close') })
    await finish(async () => { if(api) await api.dispose() })
    await finish(async () => { await closeSessionMcps(mcpClients) })
    await finish(async () => { if(build && build.exitCode === null) { build.kill(); await bounded(buildClosed,10000,'build join') } })
    await finish(async () => { if(harness && harness.exitCode === null) { harness.stdin.end('\n'); await bounded(harnessClosed,30000,'sandbox cleanup') } })
    await finish(async () => { if(sandbox) { let remains=true; try { await access(sandbox) } catch(error) { if(error.code==='ENOENT') remains=false; else throw error } assert.equal(remains,false,'owned sandbox removed') } })
    await finish(async () => { if(postgresPid) { let alive=true; try { process.kill(postgresPid,0) } catch(error) { if(error.code==='ESRCH') alive=false; else throw error } assert.equal(alive,false,'owned PostgreSQL process exited') } })
    if(failures.length) throw new AggregateError([...(failed?[failed]:[]),...failures.map(value => new Error(value))],'Sessions fixture cleanup failed')
  }
})
