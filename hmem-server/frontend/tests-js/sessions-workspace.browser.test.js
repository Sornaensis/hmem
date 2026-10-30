import assert from 'node:assert/strict'
import test from 'node:test'
import {gzipSync} from 'node:zlib'
import {createHash} from 'node:crypto'
import {openSessionExplorer} from './support/sessions-ux-fixture.mjs'
const until=async fn=>{const end=Date.now()+12000;while(Date.now()<end){if(await fn())return;await new Promise(r=>setTimeout(r,25))}throw Error('workspace sessions did not converge')}
const toggle=(page,name)=>page.getByRole('button',{name,exact:true}).click()
const count=(page,n)=>until(async()=>await page.locator('.session-summary').count()===n)

test('activity changes preserve zero-match project/task scopes and Quiet options survive a heartbeat crossing',{timeout:45000},async()=>{
 const h=await openSessionExplorer({savedCount:0}),{page}=h
 try{
  await h.start();await h.idle();await toggle(page,'Sessions');await count(page,1)
  const project=h.fixture.projects[0].id,task=h.fixture.tasks[1].id
  await page.getByRole('combobox',{name:'Project sessions'}).selectOption(project)
  await page.getByRole('combobox',{name:'Task sessions'}).selectOption(task)
  for(const name of ['Live','Completed','Quiet']){
   await toggle(page,name);await count(page,0)
   await until(()=>page.url().includes('project='+project)&&page.url().includes('task='+task))
   assert.equal(await page.getByRole('combobox',{name:'Project sessions'}).inputValue(),project)
   assert.equal(await page.getByRole('combobox',{name:'Task sessions'}).inputValue(),task)
  }
  const settled=Promise.all(['projects','tasks'].map(kind=>page.waitForResponse(response=>{const u=new URL(response.url());return u.pathname.endsWith('/filters')&&u.searchParams.get('activity')==='quiet'&&u.searchParams.get('kind')===kind})))
  await toggle(page,'Completed');await settled // Quiet alone; the only session is still Live.
  await count(page,0)
  const facetReads=h.reads.filter(r=>r.path.endsWith('/filters')).length
  h.active.last_seen_at=new Date(Date.now()-60000).toISOString()
  await page.clock.setFixedTime(new Date(Date.now()+60000))
  await count(page,1)
  assert.equal(h.reads.filter(r=>r.path.endsWith('/filters')).length,facetReads,'freshness-independent options need no timer reads')
  assert.equal(await page.getByRole('combobox',{name:'Task sessions'}).inputValue(),task)
  assert.deepEqual(h.errors,[])
 }finally{await h.close()}
})

test('sealed controls and maximum wrapped runtime remain reachable in a short narrow reader',{timeout:45000},async()=>{
 const runtime={model:'m'.repeat(256),role:'r'.repeat(128),harness:'h'.repeat(128),reasoning:'t'.repeat(128)}
 const destination='https://example.test/'+'x'.repeat(8400),markdown='[r]: '+destination+'\n\n'+Array.from({length:1000},()=>'[x][r]').join('\n\n')
 const h=await openSessionExplorer({runtime,transcriptBytes:Buffer.from(JSON.stringify({text:markdown})+'\n')}),{page}=h
 h.saved.title='First saved session'
 const second={...h.saved,id:'20000000-0000-4000-8000-000000000003',title:'Second saved session',created_at:new Date(Date.parse(h.saved.created_at)-1000).toISOString()}
 h.allSessions.push(second);h.savedSessions.push(second);h.content.set(second.id,h.content.get(h.saved.id));h.sessionLinks.set(second.id,new Set(h.sessionLinks.get(h.saved.id)))
 const hash=bytes=>createHash('sha256').update(bytes).digest('hex'),raw=h.content.get(h.saved.id),gzip=gzipSync(raw),generation='33333333-3333-4333-8333-333333333333'
 const manifest={workspace_id:h.fixture.workspace.id,session_id:h.saved.id,state:'ready',generation_id:generation,codec:'gzip-v1',raw_bytes:String(raw.length),raw_records:String(raw.toString('utf8').split('\n').filter(Boolean).length),raw_sha256:hash(raw),compressed_bytes:String(gzip.length),compressed_sha256:hash(gzip),content_path:`/api/v1/workspaces/${h.fixture.workspace.id}/sessions/${h.saved.id}/archive/${generation}/content`}
 try{
  await page.route('**/sessions/**/archive**',async route=>{
   const path=new URL(route.request().url()).pathname,session=h.allSessions.find(s=>path.includes(s.id))||h.saved
   if(!path.endsWith('/content'))return route.fulfill({json:{...manifest,session_id:session.id,content_path:`/api/v1/workspaces/${h.fixture.workspace.id}/sessions/${session.id}/archive/${generation}/content`}})
   const etag='"workspace-short-reader"'
   if(route.request().headers()['if-none-match'])return route.fulfill({status:304,headers:{ETag:etag}})
   return route.fulfill({headers:{ETag:etag,'Content-Type':'application/gzip','Content-Length':String(gzip.length),'X-HMem-Archive-Generation':generation,'X-HMem-Archive-SHA256':hash(gzip),'X-HMem-Archive-Raw-Bytes':String(raw.length),'X-HMem-Archive-Raw-SHA256':hash(raw)},body:gzip})
  })
  await page.setViewportSize({width:430,height:650});await h.start();await h.idle();await toggle(page,'Sessions');await toggle(page,'Live');await toggle(page,'Completed');await count(page,2)
  const before=await page.evaluate(()=>Object.fromEntries(['.sessions-page','.sessions-layout','.sessions-list','.session-hierarchy','.session-detail'].map(selector=>{const n=document.querySelector(selector),r=n.getBoundingClientRect();return [selector,{top:r.top,bottom:r.bottom,height:r.height,client:n.clientHeight,scroll:n.scrollHeight,row:getComputedStyle(n).gridRow,overflow:getComputedStyle(n).overflowY}]})))
  assert.ok(before['.session-hierarchy'].client>0,JSON.stringify(before))
  await page.getByText('First saved session',{exact:true}).click();await page.locator('#session-records .session-record').first().waitFor()
  assert.ok(await page.locator('#session-records').evaluate(n=>n.clientHeight>0),JSON.stringify(await page.evaluate(()=>Object.fromEntries(['.sessions-layout','.session-detail','.session-viewer','#session-records'].map(selector=>{const n=document.querySelector(selector),r=n.getBoundingClientRect();return [selector,{top:r.top,bottom:r.bottom,height:r.height,client:n.clientHeight,scroll:n.scrollHeight,overflow:getComputedStyle(n).overflowY}]})))))
  for(const zoom of [1,1.5]){
   await page.evaluate(z=>{document.body.style.zoom=String(z)},zoom)
   for(const selector of ['.session-detail-header','.session-sealed-viewer > .session-identity','.session-archive-footer','#session-records']){
    for(const element of await page.locator(selector).all()){
     await element.scrollIntoViewIfNeeded()
     assert.ok(await element.evaluate(node=>{const r=node.getBoundingClientRect(),main=document.querySelector('#main-content-scroll').getBoundingClientRect(),pane=document.querySelector('.sessions-layout').getBoundingClientRect();return r.height>0&&r.bottom>Math.max(main.top,pane.top)&&r.top<Math.min(main.bottom,pane.bottom)}),selector+' at '+zoom)
    }
   }
   const complete=page.getByRole('button',{name:'Read complete message',exact:true})
   if(await complete.count()){await complete.click();await page.getByText('Verified complete archive.',{exact:true}).waitFor()}
   await page.getByRole('button',{name:'Continue presentation',exact:true}).click()
   await until(async()=>await page.locator('.session-render-continuation').getAttribute('data-render-part')==='1')
   await page.getByRole('button',{name:'Previous presentation',exact:true}).click()
   await until(async()=>await page.locator('.session-render-continuation').getAttribute('data-render-part')==='0')
   assert.equal(await page.locator('#sealed-search,#sealed-position').count(),0)
   assert.equal(await page.locator('.session-archive-footer [data-sealed-action=first]').isDisabled(),true)
   assert.ok(await page.locator('#session-records').evaluate(n=>n.clientHeight>0))
   const sidebar=await page.evaluate(()=>Object.fromEntries(['.sessions-list','.session-hierarchy','.session-linked-tasks','.session-linked-content'].map(selector=>{const n=document.querySelector(selector),r=n.getBoundingClientRect();return [selector,{top:r.top,bottom:r.bottom,client:n.clientHeight,scroll:n.scrollHeight,overflow:getComputedStyle(n).overflowY}]})))
   assert.ok(sidebar['.session-hierarchy'].client>0,JSON.stringify({zoom,...sidebar}))
   const linked=page.locator('.session-linked-tasks')
   if(await linked.getAttribute('open')===null)await linked.locator(':scope > summary').click()
   assert.ok(await page.locator('.session-linked-content').evaluate(n=>n.clientHeight>0),JSON.stringify({zoom,...sidebar}))
   await linked.getByTitle('Jump to task',{exact:true}).first().click()
   await page.locator('#entity-'+h.fixture.tasks[0].id).waitFor();await page.goBack()
   await page.getByText('Verified complete archive.',{exact:true}).waitFor()
   const next=zoom===1?second:h.saved
   await page.locator('.session-summary-heading strong').filter({hasText:next.title}).click()
   await until(async()=>await page.locator('.session-detail-header h3').innerText()===next.title)
   await page.getByText('Verified complete archive.',{exact:true}).waitFor()
  }
  assert.deepEqual(h.errors,[])
 }finally{await h.close()}
})

test('workspace activity unions preserve scoped pagination, empty selection and stale reply authority',{timeout:60000},async()=>{
 const h=await openSessionExplorer({activeCount:2,savedCount:52}),{page}=h
 try{
  await h.start();await h.idle();await toggle(page,'Sessions');await count(page,1)
  for(const [name,expected] of [['Quiet',2],['Completed',50],['Live',50],['Quiet',50]]){await toggle(page,name);await count(page,expected)}
  // Completed alone crosses a bounded page and includes old sessions regardless of task state.
  assert.equal(await page.getByRole('button',{name:'Completed',exact:true}).getAttribute('aria-pressed'),'true')
  await page.getByRole('button',{name:'Older sessions',exact:true}).click();await count(page,2)
  await page.getByRole('button',{name:'Newer',exact:true}).click();await count(page,50)
  await until(async()=>await page.getByRole('combobox',{name:'Project sessions'}).locator('option').count()>1)
  await page.getByRole('combobox',{name:'Project sessions'}).selectOption(h.fixture.projects[0].id);await count(page,50)
  await page.getByRole('combobox',{name:'Task sessions'}).selectOption(h.fixture.tasks[1].id);await count(page,50)
  assert.ok(h.reads.some(r=>r.query.includes('activity=completed')&&r.query.includes('project_id=')&&r.query.includes('task_id=')))
  assert.ok(h.reads.filter(r=>r.path.endsWith('/filters')).every(r=>r.query.includes('activity=')))
  // A physical callback from the retired union cannot overwrite the new empty demand.
  let release,arrived=false
  const held=new Promise(resolve=>{release=resolve})
  await page.route('**/sessions?*',async route=>{
   const url=new URL(route.request().url())
   if(url.searchParams.get('activity')==='live,completed'){arrived=true;await held;try{await route.fulfill({json:{items:[h.active],next_cursor:null}})}catch{};return}
   return route.fallback()
  })
  try{
   await toggle(page,'Live');await until(()=>arrived)
   await toggle(page,'Completed');await toggle(page,'Live');release()
   await count(page,0);await page.getByText('No sessions match these filters.',{exact:true}).waitFor()
   assert.equal(await page.getByRole('button',{name:'Newer',exact:true}).isDisabled(),true)
   assert.equal(await page.getByRole('button',{name:'Older sessions',exact:true}).count(),0)
  }finally{release()}
  assert.deepEqual(h.errors,[])
 }finally{await h.close()}
})

test('workspace panes fit actual chrome with wrapped filters, long transcript and deep hierarchy',{timeout:60000},async()=>{
 const h=await openSessionExplorer({activeCount:25,linkedCount:18,extraRecords:[{text:'Long transcript paragraph.\n\n'.repeat(800)}]}),{page}=h
 try{
  for(const [index,s] of h.activeSessions.entries()){s.last_seen_at=new Date().toISOString();s.parent_session_id=index?h.activeSessions[index-1].id:null}
  h.fixture.projects[0].name='Project with a long filter name '.repeat(4)
  await page.setViewportSize({width:1440,height:1000});await h.start();await h.idle();await toggle(page,'Sessions');await page.locator('.session-summary a').first().click()
  await page.locator('#session-records .session-record').first().waitFor();await count(page,25)
  await page.locator('.session-linked-tasks > summary').click()
  for(const [width,height,zoom] of [[1440,1000,1],[900,950,1],[430,1100,1],[1440,1200,1.5]]){
   await page.setViewportSize({width,height});await page.evaluate(z=>{document.body.style.zoom=String(z)},zoom)
   await until(async()=>page.evaluate(()=>{
    const main=document.querySelector('#main-content-scroll').getBoundingClientRect(),layout=document.querySelector('.sessions-layout').getBoundingClientRect()
    return layout.bottom<=main.bottom+1&&layout.right<=main.right+1&&layout.height>0
   }))
   const geometry=await page.evaluate(()=>{
    const main=document.querySelector('#main-content-scroll'),layout=document.querySelector('.sessions-layout'),records=document.querySelector('#session-records'),hierarchy=document.querySelector('.session-hierarchy'),linked=document.querySelector('.session-linked-content'),sidebar=document.querySelector('.sessions-list')
    const rect=n=>{const r=n.getBoundingClientRect();return {top:r.top,bottom:r.bottom,left:r.left,right:r.right,height:r.height}}
    return {main:rect(main),layout:rect(layout),records:rect(records),hierarchy:rect(hierarchy),linked:rect(linked),sidebar:rect(sidebar),recordScroll:records.scrollHeight>records.clientHeight,hierarchyScroll:hierarchy.scrollHeight>hierarchy.clientHeight,linkedScroll:linked.scrollHeight>linked.clientHeight,pageWidth:document.documentElement.scrollWidth,viewport:innerWidth}
   })
   assert.ok(geometry.records.height>0,JSON.stringify(geometry))
   for(const name of ['records','sidebar'])assert.ok(geometry[name].bottom<=geometry.layout.bottom+1,JSON.stringify({width,zoom,...geometry}))
   for(const selector of ['.session-hierarchy','.session-linked-content']){
    const pane=page.locator(selector);await pane.scrollIntoViewIfNeeded()
    assert.ok(await pane.evaluate(n=>{const r=n.getBoundingClientRect(),side=n.closest('.sessions-list').getBoundingClientRect();return n.clientHeight>0&&r.bottom>side.top&&r.top<side.bottom}),selector+' is reachable within sidebar')
   }
   assert.ok(geometry.recordScroll);assert.ok(geometry.hierarchyScroll);assert.ok(geometry.linkedScroll)
   assert.ok(geometry.pageWidth<=geometry.viewport)
  }
  assert.deepEqual(h.errors,[])
 }finally{await h.close()}
})

test('Linked Tasks disclosure reuses read-only dependency cards with loading, error, paging and navigation',{timeout:45000},async()=>{
 const h=await openSessionExplorer({linkedCount:4}),{page}=h
 let phase='error',reads=0,releaseInitial
 const initial=new Promise(resolve=>{releaseInitial=resolve})
 try{
  await page.route('**/sessions/'+h.active.id+'/tasks?*',async route=>{
   reads++;if(reads===1)await initial;if(phase==='error')return route.fulfill({status:503,json:{error:'unavailable'}})
   const after=new URL(route.request().url()).searchParams.get('after_task_id'),tasks=h.fixture.tasks
   return route.fulfill({json:{items:(after?tasks.slice(2):tasks.slice(0,2)).map(t=>({id:t.id,title:t.title,status:t.status})),next_task_id:after?null:tasks[1].id}})
  })
  await h.start();await h.idle();await toggle(page,'Sessions');await page.locator('.session-summary a').first().click()
  const linked=page.locator('.session-linked-tasks'),summary=linked.locator(':scope > summary')
  await summary.focus();await page.keyboard.press('Enter')
  await linked.getByRole('status').waitFor();releaseInitial();await linked.getByRole('alert').waitFor();phase='ok';await linked.getByRole('button',{name:'Retry',exact:true}).click()
  await until(async()=>await linked.locator('.dep-item.popover-card').count()===2)
  const ordering=await page.evaluate(()=>document.querySelector('.session-linked-tasks').getBoundingClientRect().top<document.querySelector('.session-pane-label').getBoundingClientRect().top)
  assert.ok(ordering);assert.equal(await linked.getByTitle('Remove dependency').count(),0)
  await linked.getByRole('button',{name:'More tasks',exact:true}).click();await until(async()=>await linked.getByRole('button',{name:'First tasks',exact:true}).count()===1)
  assert.equal(await linked.locator('.popover-card-title').first().innerText(),h.fixture.tasks[2].title)
  const task=h.fixture.tasks[2];await linked.getByTitle('Jump to task',{exact:true}).first().click()
  await page.locator('#entity-'+task.id).waitFor();assert.ok(page.url().includes('focus=task'))
  assert.ok(reads>=3);assert.deepEqual(h.errors,[])
 }finally{releaseInitial();await h.close()}
})
