import assert from 'node:assert/strict'
import test from 'node:test'
import {hierarchyFixture,openHierarchy} from './hierarchy-fixture.mjs'
const until=async fn=>{const end=Date.now()+12000;while(Date.now()<end){if(await fn())return;await new Promise(resolve=>setTimeout(resolve,25))}throw Error('Metadata layout did not converge')}

test('copy-only identifiers retain exact native copy feedback and card footers have one boundary',{timeout:30000},async()=>{
 const f=hierarchyFixture(),h=await openHierarchy(f),{page}=h
 try{
  await page.addInitScript(()=>{window.metadataCopies=[];window.metadataCopyFailure=false;Object.defineProperty(navigator.clipboard,'writeText',{value:async value=>{if(window.metadataCopyFailure)throw Error('controlled clipboard failure');window.metadataCopies.push(value)}})})
  await h.start();await h.idle()
  const copies=[['workspace',f.workspace.id],['project',f.projects[0].id],['task',f.tasks[0].id]]
  for(const [index,[kind,id]] of copies.entries()){
   if(kind!=='workspace')await h.scrollTo(kind+':'+id)
   const copy=page.getByRole('button',{name:'Copy '+kind+' ID: '+id,exact:true})
   assert.equal(await copy.innerText(),id);assert.equal(await copy.getAttribute('title'),'Copy '+kind+' ID: '+id);assert.equal(await copy.getAttribute('type'),'button')
   await copy.focus();await page.keyboard.press(index%2?'Space':'Enter');await until(async()=>await page.evaluate(()=>window.metadataCopies.length)===index+1)
   assert.equal(await page.evaluate(()=>window.metadataCopies.at(-1)),id)
   if(kind!=='workspace'){
    const footer=kind==='project'?page.locator('#entity-'+id+' > .card-project > .card-footer'):page.locator('#entity-'+id+' > .card-footer')
    const borders=await footer.evaluate(node=>({outer:getComputedStyle(node).borderTopWidth,inner:getComputedStyle(node.querySelector(':scope > .card-meta-group')).borderTopWidth,text:node.textContent}))
    assert.equal(borders.outer,'1px');assert.equal(borders.inner,'0px');assert.ok(borders.text.includes('Created:'));assert.ok(borders.text.includes('Updated:'));assert.ok(borders.text.includes(id))
   }
  }
  assert.equal(await page.getByText(/^UUID:?$/).count(),0);await page.getByText('Copied to clipboard',{exact:true}).first().waitFor()
  await page.evaluate(()=>{window.metadataCopyFailure=true});await page.getByRole('button',{name:'Copy workspace ID: '+f.workspace.id,exact:true}).click();await page.getByText('Unable to copy to clipboard',{exact:true}).waitFor()
  assert.deepEqual(h.errors,[]);assert.deepEqual(h.unhandled,[])
 }finally{await h.close()}
})

test('New Project shares the actual hierarchy width at desktop, narrow and zoom and retains inline creation',{timeout:30000},async()=>{
 const f=hierarchyFixture(),h=await openHierarchy(f),{page}=h
 try{
  await h.start();await h.idle();const create=page.getByRole('button',{name:'+ New Project',exact:true}),tree=page.locator('#hierarchy-viewport')
  for(const [width,zoom] of [[2560,1],[1440,1],[900,1],[430,1],[1440,1.5]]){
   await page.setViewportSize({width,height:1000});await page.evaluate(zoom=>{document.documentElement.style.zoom=String(zoom)},zoom);await h.idle()
   const [button,bounds]=await Promise.all([create.boundingBox(),tree.boundingBox()]);assert.ok(Math.abs(button.width-bounds.width)<1,JSON.stringify({width,zoom,button,bounds}));assert.ok(Math.abs(button.x-bounds.x)<1)
   assert.ok(await page.evaluate(()=>document.documentElement.scrollWidth<=innerWidth));assert.ok(await page.locator('[data-hierarchy-key]').count()<=25);assert.ok(h.caps().maxDetails<=6);assert.ok(h.caps().maxBranches<=4)
  }
  await page.evaluate(()=>{document.documentElement.style.zoom=''});await create.click();const input=page.getByPlaceholder('New project name...');await input.waitFor();await input.fill('A reviewable new project');await input.press('Escape');await create.waitFor()
  assert.deepEqual(h.errors,[]);assert.deepEqual(h.unhandled,[])
 }finally{await h.close()}
})
