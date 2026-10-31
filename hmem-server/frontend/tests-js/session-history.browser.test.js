import assert from 'node:assert/strict'
import test from 'node:test'
import {openSessionExplorer} from './support/sessions-ux-fixture.mjs'

const until=async fn=>{const end=Date.now()+15000;while(Date.now()<end){if(await fn())return;await new Promise(resolve=>setTimeout(resolve,25))}throw Error('Progressive conversation did not converge')}
const state=page=>page.locator('#session-records').evaluate(node=>({before:Number(node.dataset.historyBefore),after:Number(node.dataset.historyAfter),latest:Number(node.dataset.historyLatest),loading:node.dataset.historyLoading==='true',offsets:[...node.querySelectorAll('.session-record')].map(row=>Number(row.dataset.offset))}))

test('shared reader progressively reaches every occurrence across byte and row limits in both directions',{timeout:90000},async()=>{
 const records=Array.from({length:340},(_,i)=>({type:'event_msg',payload:{type:'agent_message',message:`Occurrence ${i} `+'x'.repeat(i%9===0?65000:30)}}))
 const lines=records.map(record=>JSON.stringify(record)+'\r\n'),bytes=Buffer.from(lines.join('')),expected=[];let cursor=0
 for(const line of lines){expected.push(cursor);cursor+=Buffer.byteLength(line)}
 assert.ok(bytes.length>2097152)
 const h=await openSessionExplorer({transcriptBytes:bytes}),{page}=h
 try{
  await h.start();await h.idle();await page.goto(h.origin+'/workspace/'+h.fixture.workspace.id+'#tab=sessions&session='+h.active.id);await page.locator('.session-record').first().waitFor()
  await until(async()=>!(await state(page)).loading)
  const backward=new Set((await state(page)).offsets)
  for(let pages=0;(await state(page)).before>0;pages++){
   assert.ok(pages<40,'history must progress in bounded pages')
   const previous=await state(page)
   await page.getByRole('button',{name:'Earlier messages',exact:true}).click()
   await until(async()=>{const current=await state(page);return !current.loading&&current.before<previous.before})
   for(const offset of (await state(page)).offsets)backward.add(offset)
  }
  assert.deepEqual([...backward].sort((a,b)=>a-b),expected)
  const forward=new Set((await state(page)).offsets)
  for(let pages=0;(await state(page)).after<bytes.length;pages++){
   assert.ok(pages<40,'newer traversal must progress in bounded pages')
   const previous=await state(page)
   await page.getByRole('button',{name:'Newer messages',exact:true}).click()
   await until(async()=>{const current=await state(page);return !current.loading&&current.after>previous.after})
   for(const offset of (await state(page)).offsets)forward.add(offset)
  }
  assert.deepEqual([...forward].sort((a,b)=>a-b),expected)
  assert.deepEqual(h.errors,[]);assert.ok(h.peak()<=2)
 }finally{await h.close()}
})

for(const suffix of ['', '\r\n'])test(`an initial lookback inside a large final record ${suffix?'with CRLF':'without LF'} expands to publish it`,{timeout:45000},async()=>{
 const bytes=Buffer.from(JSON.stringify({text:'Earlier record'})+'\n'+JSON.stringify({text:'Final large record '+'x'.repeat(180000)})+suffix)
 const h=await openSessionExplorer({transcriptBytes:bytes}),{page}=h
 try{
  await h.start();await h.idle();await page.goto(h.origin+'/workspace/'+h.fixture.workspace.id+'#tab=sessions&session='+h.active.id)
  await page.getByText(/^Final large record/).waitFor()
  await until(async()=>!(await state(page)).loading)
  assert.equal((await state(page)).after,bytes.length)
  assert.deepEqual(h.errors,[])
 }finally{await h.close()}
})

test('real boundary scrolling preserves the visible anchor through history, appends and resize',{timeout:45000},async()=>{
 const bytes=Buffer.from(Array.from({length:420},(_,i)=>JSON.stringify({type:'event_msg',payload:{type:'agent_message',message:`Reading anchor ${i} `+'word '.repeat(20)}})+'\n').join(''))
 const h=await openSessionExplorer({transcriptBytes:bytes}),{page}=h
 const anchor=()=>page.locator('#session-records').evaluate(node=>{const bounds=node.getBoundingClientRect(),row=[...node.querySelectorAll('[data-block-key]')].find(row=>row.getBoundingClientRect().bottom>bounds.top);return {key:row.dataset.blockKey,top:row.getBoundingClientRect().top-bounds.top}})
 const position=key=>page.locator('#session-records').evaluate((node,key)=>{const row=[...node.querySelectorAll('[data-block-key]')].find(row=>row.dataset.blockKey===key);return row?row.getBoundingClientRect().top-node.getBoundingClientRect().top:null},key)
 try{
  await h.start();await h.idle();await page.goto(h.origin+'/workspace/'+h.fixture.workspace.id+'#tab=sessions&session='+h.active.id)
  await page.locator('.session-record').first().waitFor();await until(async()=>!(await state(page)).loading)
  const viewport=page.locator('#session-records');await viewport.evaluate(node=>{node.scrollTop=0});await h.idle()
  const before=await state(page),reading=await anchor()
  await viewport.hover();await page.mouse.wheel(0,-400)
  await until(async()=>{const current=await state(page);return !current.loading&&current.before<before.before})
  await h.idle();assert.ok(Math.abs((await position(reading.key))-reading.top)<=4,'earlier page preserves the same visible occurrence')
  await h.appendRecords({text:'New live output while reading history'});await h.idle()
  assert.ok(Math.abs((await position(reading.key))-reading.top)<=4,'appends preserve a historical anchor')
  await page.setViewportSize({width:900,height:720});await h.idle()
  assert.ok(Math.abs((await position(reading.key))-reading.top)<=4,'resize preserves the same occurrence and intrarow position')
  assert.deepEqual(h.errors,[])
 }finally{await h.close()}
})
