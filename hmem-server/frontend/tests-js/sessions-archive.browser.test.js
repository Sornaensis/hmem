import assert from 'node:assert/strict'
import test from 'node:test'
import {openSessionExplorer} from './support/sessions-ux-fixture.mjs'
const until=async fn=>{const end=Date.now()+15000;while(Date.now()<end){if(await fn())return;await new Promise(resolve=>setTimeout(resolve,25))}throw Error('session cards did not converge')}
async function openEvidence(h){await h.start();await h.idle();const card=h.page.locator('#entity-'+h.fixture.tasks[0].id);await card.locator('.btn-extras-toggle').click();await h.idle();const group=card.locator('.task-session-evidence');await group.locator('summary').click();return {card,group}}

test('task session grids remain compact for one item and display actual last activity at desktop and narrow widths', {timeout:40000},async()=>{
 for(const width of [1440,430]){
  const h=await openSessionExplorer({runtime:{model:'runtime:model',role:'Developer'}})
  try{await h.page.setViewportSize({width,height:1600});const{card,group}=await openEvidence(h)
   await card.locator('.task-sessions-icon').click();const active=card.locator('.task-session-dock');await active.locator('.session-viewer .session-identity').waitFor()
   for(const[section,session,source]of[[active,h.active,h.active.last_seen_at],[group,h.saved,h.saved.terminated_at||h.saved.last_seen_at]]){
    const item=section.locator(section===active?'.task-session-choice':'.task-session-card');await item.waitFor();assert.ok(await item.evaluate(el=>el.getBoundingClientRect().width)<=430)
    const expected=await h.page.evaluate(value=>{const date=new Date(value),today=new Date(),pad=v=>String(v).padStart(2,'0');return (date.toDateString()===today.toDateString()?'':date.getFullYear()+'-'+pad(date.getMonth()+1)+'-'+pad(date.getDate())+' ')+pad(date.getHours())+':'+pad(date.getMinutes())},source)
    await until(async()=>(await item.locator('.session-latest-activity').innerText()).includes(expected));assert.notEqual(source,session.created_at)
    assert.ok((await (section===active?active.locator('.session-viewer .session-identity'):item.locator('.session-identity')).innerText()).includes('Developer'))
   }
   assert.ok(await h.page.evaluate(()=>document.documentElement.scrollWidth<=innerWidth));assert.deepEqual(h.errors,[])
  }finally{await h.close()}
 }
})

