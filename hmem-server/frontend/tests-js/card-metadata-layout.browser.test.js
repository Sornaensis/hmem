import assert from 'node:assert/strict'
import test from 'node:test'
import {hierarchyFixture,openHierarchy} from './hierarchy-fixture.mjs'
const until=async fn=>{const end=Date.now()+12000;while(Date.now()<end){if(await fn())return;await new Promise(resolve=>setTimeout(resolve,25))}throw Error('Metadata layout did not converge')}

test('copy-only identifiers retain exact native copy feedback and card footers have one boundary',{timeout:30000},async()=>{
 const f=hierarchyFixture(),h=await openHierarchy(f),{page}=h
 try{
  await page.addInitScript(()=>{window.metadataCopies=[];window.metadataCopyFailure=false;Object.defineProperty(navigator.clipboard,'writeText',{value:async value=>{if(window.metadataCopyFailure)throw Error('controlled clipboard failure');window.metadataCopies.push(value)}})})
  await h.start();await h.idle();await page.getByRole('button',{name:'Expand All',exact:true}).click();await h.idle()
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

test('New project fits the hierarchy search row and preserves keyboard creation',{timeout:30000},async()=>{
 const f=hierarchyFixture(),h=await openHierarchy(f),{page}=h
 const submissions=[]
 await page.route(url=>url.pathname==='/api/v1/projects',async route=>{
  const request=route.request()
  assert.equal(request.method(),'POST')
  const body=request.postDataJSON()
  submissions.push(body)
  const project={...f.projects[0],id:'toolbar-created-project',name:body.name}
  f.projects.push(project)
  await route.fulfill({status:201,json:project})
 })
 try{
  await h.start();await h.idle();const create=page.getByRole('button',{name:'New project',exact:true}),tree=page.locator('.hierarchy-toolbar')
  for(const [width,zoom] of [[2560,1],[1440,1],[900,1],[430,1],[1440,1.5]]){
   await page.setViewportSize({width,height:1000});await page.evaluate(zoom=>{document.documentElement.style.zoom=String(zoom)},zoom);await h.idle()
   const [button,bounds]=await Promise.all([create.boundingBox(),tree.boundingBox()]);assert.ok(button.width>0&&button.x>=bounds.x&&button.x+button.width<=bounds.x+bounds.width+1,JSON.stringify({width,zoom,button,bounds}));assert.ok(button.y>=bounds.y&&button.y+button.height<=bounds.y+bounds.height+1)
   assert.ok(await page.evaluate(()=>document.documentElement.scrollWidth<=innerWidth));assert.ok(await page.locator('[data-hierarchy-key]').count()<=25);assert.ok(h.caps().maxDetails<=6);assert.ok(h.caps().maxBranches<=4)
  }
  await page.evaluate(()=>{document.documentElement.style.zoom=''})
  await page.setViewportSize({width:390,height:1000})
  const search=page.getByPlaceholder('Filter project hierarchy...'),input=page.getByPlaceholder('New project name...')
  await search.focus()
  for(const name of ['Expand All','Collapse All','New project']){
   await page.keyboard.press('Tab')
   assert.equal(await page.getByRole('button',{name,exact:true}).evaluate(button=>document.activeElement===button),true)
  }
  await page.keyboard.press('Enter');await input.waitFor()
  await page.waitForFunction(()=>document.activeElement?.id==='inline-create-input')
  const [field,row]=await Promise.all([input.boundingBox(),tree.boundingBox()])
  assert.ok(field.x>=row.x&&field.x+field.width<=row.x+row.width+1)
  assert.ok(field.y>=row.y&&field.y+field.height<=row.y+row.height+1)
  await input.press('Enter')
  assert.equal(await input.isVisible(),true)
  await input.fill('Cancelled project');await input.press('Escape');await create.waitFor()
  assert.deepEqual(submissions,[])
  await create.click();await input.fill('A reviewable new project');await input.press('Enter')
  await page.getByText('Created project: A reviewable new project',{exact:true}).waitFor()
  await create.waitFor()
  assert.equal(submissions.length,1)
  assert.equal(submissions[0].name,'A reviewable new project')
  assert.equal(submissions[0].workspace_id,f.workspace.id)
  assert.deepEqual(h.errors,[]);assert.deepEqual(h.unhandled,[])
 }finally{await h.close()}
})

test('read-only empty hierarchy keeps search, actions and filters without New project',{timeout:20000},async()=>{
 const f=hierarchyFixture();f.projects=[];f.tasks=[]
 const h=await openHierarchy(f,[],null,{viewport:{width:320,height:950}}),{page}=h
 h.gate(receipt=>receipt.path==='/api/v1/session',{transform:session=>({...session,workspace:{...session.workspace,role:'viewer',can_edit:false,can_admin:false}})})
 try{
  await page.goto(h.origin+'/workspace/'+f.workspace.id+'#tab=projects')
  await page.locator('#hierarchy-viewport').waitFor({state:'attached',timeout:10000})
  await h.idle()
  assert.equal(await page.locator('[data-hierarchy-key]').count(),0)
  const toolbar=page.locator('.hierarchy-toolbar')
  assert.deepEqual(await toolbar.getByRole('button').allTextContents(),['Expand all','Collapse all'])
  assert.equal(await page.getByRole('button',{name:'New project',exact:true}).count(),0)
  const priority=page.getByRole('combobox',{name:'Priority',exact:true})
  assert.deepEqual(await priority.locator('option').allTextContents(),['Any','Exact','Above','Below'])
  await priority.selectOption('exact')
  const numeric=page.locator('.filter-priority-input')
  assert.equal(await numeric.getAttribute('min'),'0')
  assert.equal(await numeric.getAttribute('max'),'10')
  const filtered=page.waitForRequest(request=>{
   const url=new URL(request.url())
   return url.pathname.endsWith('/navigation')&&url.searchParams.get('priority_mode')==='exact'&&url.searchParams.get('priority_value')==='7'
  },{timeout:5000})
  await numeric.fill('7')
  await filtered;await h.idle()
  assert.equal(await numeric.inputValue(),'7')
  assert.equal(await page.locator('[data-hierarchy-key]').count(),0)
  await priority.selectOption('any')
  assert.equal(await numeric.count(),0)
  assert.equal(await page.evaluate(()=>document.documentElement.scrollWidth<=innerWidth),true)
  assert.deepEqual(h.errors,[]);assert.deepEqual(h.unhandled,[])
 }finally{await h.close()}
})
