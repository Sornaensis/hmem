import assert from 'node:assert/strict'
import test from 'node:test'
import {spawnSync} from 'node:child_process'
import {mkdtemp,rm} from 'node:fs/promises'
import {tmpdir} from 'node:os'
import {join} from 'node:path'
import {createHash} from 'node:crypto'
import {openSessionExplorer} from './support/sessions-ux-fixture.mjs'
const until=async fn=>{const end=Date.now()+15000;while(Date.now()<end){if(await fn())return;await new Promise(resolve=>setTimeout(resolve,25))}throw Error('archive did not converge')}
async function openEvidence(h){await h.start();await h.idle();const card=h.page.locator('#entity-'+h.fixture.tasks[0].id);await card.locator('.btn-extras-toggle').click();await h.idle();const group=card.locator('.task-session-evidence');await group.locator('summary').click();return {card,group}}
function verify(path,expected){const result=spawnSync('python',['-c',`import sys,json,zipfile,hashlib\nexpected=json.loads(sys.stdin.buffer.read().decode('utf-8'))\nwith zipfile.ZipFile(sys.argv[1]) as z:\n assert z.testzip() is None\n assert len(z.namelist())==len(set(z.namelist()))==len(expected)\n for name in z.namelist():\n  key=next((key for key in expected if name.endswith(key+'.jsonl')),None)\n  assert key is not None,name\n  assert '/' not in name and not name.startswith('.')\n  assert 'café' in name or len(expected)<=1\n  assert hashlib.sha256(z.read(name)).hexdigest()==expected[key]\nprint('verified',len(expected),'entries')`,path],{input:JSON.stringify(expected),encoding:'utf8',windowsHide:true});assert.equal(result.status,0,result.stderr||result.stdout)}

test('Download all independently pages every evidence session into a verified browser ZIP', {timeout:90000},async()=>{
 for(const scenario of [{savedCount:0},{savedCount:1,fallback:true},{savedCount:53}]){
  const scratch=await mkdtemp(join(tmpdir(),'session-archive-')),h=await openSessionExplorer({savedCount:scenario.savedCount,activeCount:3})
  try{
   const{page}=h;await page.setViewportSize({width:1440,height:2400});const{card,group}=await openEvidence(h)
   if(scenario.savedCount>50){await group.getByRole('button',{name:'Older sessions',exact:true}).click();await until(async()=>await group.locator('.task-session-card').count()===3)}
   if(scenario.fallback)await page.evaluate(()=>Object.defineProperty(navigator.storage,'getDirectory',{configurable:true,value:async()=>{throw Error('private storage unavailable')}}))
   const event=page.waitForEvent('download');await group.getByRole('button',{name:'↓ Download all',exact:true}).click();const download=await event
   assert.ok(download.suggestedFilename().endsWith(h.fixture.tasks[0].id+'.zip'))
   const target=join(scratch,'all.zip');await download.saveAs(target);verify(target,Object.fromEntries(h.savedSessions.map(session=>[session.id,createHash('sha256').update(h.content.get(session.id)).digest('hex')])))
   await until(async()=>await group.getByRole('button',{name:'↓ Download all',exact:true}).isEnabled())
   if(scenario.savedCount>50)assert.equal(await group.locator('.task-session-card').count(),3,'visible section cursor remains on second page')
   const other=page.locator('#entity-'+h.fixture.tasks[1].id);await other.locator('.btn-extras-toggle').click();await h.idle();await other.locator('.task-session-evidence summary').click();assert.equal(await other.getByText(/Downloaded \d+ sessions/).count(),0,'archive feedback belongs to its initiating task')
   assert.ok(h.peak()<=2);for(const read of h.reads.filter(read=>read.path.endsWith('/evidence')))assert.ok(Number(new URLSearchParams(read.query).get('limit'))<=65536)
   for(const section of ['active','evidence'])for(const label of await card.locator('.task-session-'+section+' .task-session-card .card-meta').allTextContents())assert.match(label,/Last activity/)
   assert.deepEqual(h.errors,[])
  }finally{await h.close();await rm(scratch,{recursive:true,force:true})}
 }
})

test('task session grids remain compact for one item and display actual last activity at desktop and narrow widths', {timeout:40000},async()=>{
 for(const width of [1440,430]){
  const h=await openSessionExplorer({creator:{creator_actor_type:'bot',creator_bot_identity:{role:'Developer','model-id':'declared:model:high'}}})
  try{await h.page.setViewportSize({width,height:1600});const{card,group}=await openEvidence(h)
   const active=card.locator('.task-session-active');await active.locator('summary').click()
   for(const[section,session,source]of[[active,h.active,h.active.last_seen_at],[group,h.saved,h.saved.terminated_at||h.saved.last_seen_at]]){
    const item=section.locator('.task-session-card');await item.waitFor();assert.ok(await item.evaluate(el=>el.getBoundingClientRect().width)<= (width>600?301:430))
    const expected=await h.page.evaluate(value=>{const date=new Date(value),pad=v=>String(v).padStart(2,'0');return date.getFullYear()+'-'+pad(date.getMonth()+1)+'-'+pad(date.getDate())+' '+pad(date.getHours())+':'+pad(date.getMinutes())},source)
    await until(async()=>(await item.locator('.card-meta').innerText()).includes(expected));assert.notEqual(source,session.created_at)
    assert.ok((await item.locator('.session-identity').innerText()).includes('declared:model:high'))
   }
   assert.ok(await h.page.evaluate(()=>document.documentElement.scrollWidth<=innerWidth));assert.deepEqual(h.errors,[])
  }finally{await h.close()}
 }
})

test('failed, cancelled and retired archives emit nothing and clean private staging before retry', {timeout:90000},async()=>{
 for(const mode of ['receipt','cursor','cancel','scope']){
  const h=await openSessionExplorer({savedCount:2});let held,release,downloads=0
  try{const{page}=h,{card,group}=await openEvidence(h);page.on('download',()=>downloads++)
   if(mode==='cursor')await page.route('**/tasks/*/sessions?*',route=>{const url=new URL(route.request().url());if(url.searchParams.get('state')!=='terminated')return route.fallback();return route.fulfill({json:{items:[],next_cursor:{created_at:h.saved.created_at,id:h.saved.id}}})})
   else{await page.route('**/sessions/'+h.savedSessions[1].id+'/evidence?*',async route=>{
    if(mode==='receipt'){const session=h.savedSessions[1],data=h.content.get(session.id);return route.fulfill({json:{session:{...session,revision:99},offset:0,next_offset:data.length,data_base64:data.toString('base64'),has_more:false}})}
    held=true;await new Promise(resolve=>{release=resolve});return route.fallback()
   })}
   await group.getByRole('button',{name:'↓ Download all',exact:true}).click()
   if(mode==='cancel'||mode==='scope'){await until(()=>held);if(mode==='cancel')await group.getByRole('button',{name:'Cancel',exact:true}).click();else{await card.locator('.task-session-active summary').click();await card.locator('.task-session-active a').click();await page.locator('.session-detail').waitFor()}release()}
   else await group.getByText(/Archive could not finish|Download could not finish/).waitFor()
   await until(async()=>await page.evaluate(async()=>{try{const root=await(await navigator.storage.getDirectory()).getDirectoryHandle('hmem-downloads');for await(const name of root.keys())if(name.startsWith('hmem-download-'))return false;return true}catch{return true}}))
   assert.equal(downloads,0);assert.deepEqual(h.errors,[])
  }finally{release?.();await h.close()}
 }
})



test('Cancel during central-index finalization joins cleanup without emitting an archive', {timeout:30000},async()=>{
 const h=await openSessionExplorer({savedCount:1});let downloads=0
 try{const{page}=h,{group}=await openEvidence(h);page.on('download',()=>downloads++)
  await page.evaluate(()=>{const native=Blob.prototype.arrayBuffer;Blob.prototype.arrayBuffer=function(){window.archiveIndexReading=true;return new Promise((resolve,reject)=>{window.releaseArchiveIndex=()=>native.call(this).then(resolve,reject)})}})
  await group.getByRole('button',{name:'↓ Download all',exact:true}).click();await page.waitForFunction(()=>window.archiveIndexReading===true)
  await group.getByRole('button',{name:'Cancel',exact:true}).click();await page.evaluate(()=>{window.releaseArchiveIndex()})
  await until(async()=>await page.evaluate(async()=>{const root=await(await navigator.storage.getDirectory()).getDirectoryHandle('hmem-downloads');for await(const name of root.keys())if(name.startsWith('hmem-download-'))return false;return true}))
  await until(async()=>await group.getByRole('button',{name:'↓ Download all',exact:true}).isEnabled())
  assert.equal(downloads,0);assert.equal(await group.getByText(/cleanup could not be confirmed/).count(),0);assert.deepEqual(h.errors,[])
 }finally{await h.close()}
})
