import { hierarchyFixture, openHierarchy } from '../hierarchy-fixture.mjs'
import { createHash } from 'node:crypto'
export async function openSessionExplorer({many=false,empty=false,creator=null,runtime=null,housekeeping=false,activeCount=1,savedCount=1,linkedCount=2,encrypted=false,timezoneId='UTC',stamp=new Date().toISOString(),eventTimes=[],launchOptions={},extraRecords=[],transcriptBytes=null}={}) {
  const f=hierarchyFixture();f.projects=[{...f.projects[0],id:"50000000-0000-4000-8000-000000000001"}]
  f.tasks=Array.from({length:many?22:Math.max(2,linkedCount)},(_,i)=>({...f.tasks[0],id:`10000000-0000-4000-8000-${String(i+1).padStart(12,'0')}`,title:i===0?'Build session explorer':i===1?'Review captured conversations':'Task '+(i+1),description:'Keep conversations easy to explore.',parent_id:null,project_id:f.projects[0].id}))
  const records=[
    {type:'event_msg',timestamp:'2026-10-24T10:00:00Z',payload:{type:'user_message',message:'Show the **session explorer** and its connected tasks.'}},
    {type:'response_item',timestamp:'2026-10-24T10:00:00Z',payload:{type:'message',role:'user',content:[{type:'input_text',text:'Show the **session explorer** and its connected tasks.'}]}},
    {type:'response_item',payload:{type:'reasoning',summary:[{type:'summary_text',text:'Private reasoning stays collapsed.'}]}},
    {type:'response_item',payload:{type:'function_call',name:'read_file',arguments:'{"path":"src/Feature/Sessions.elm"}'}},
    {type:'response_item',payload:{type:'function_call_output',call_id:'call-1',output:'module Feature.Sessions exposing (view)'}},
    {type:'response_item',payload:{type:'message',role:'assistant',content:[{type:'output_text',text:'## A calmer session explorer\n\n- **Task sessions** stay connected.\n- Tool calls expand when needed.\n\n```elm\nview : Model -> Html Msg\n```\n\n[Reference](https://example.com) [Unsafe](javascript:alert(1))'}]}},
    {type:'assistant',message:{role:'assistant',content:[{type:'thinking',thinking:'Claude thinking is hidden too.'},{type:'tool_use',name:'Read',input:{file_path:'src/Types.elm'}}]}},
    {type:'user',message:{role:'user',content:[{type:'tool_result',tool_use_id:'tool-1',content:[{type:'text',text:'type alias Model = { sessions : SessionsModel }'}]}]}},
    {type:'assistant',message:{role:'assistant',content:[{type:'text',text:'The linked task cards and saved evidence are ready to explore.'}]}},
    {type:'mystery',payload:{preserved:true}},
  ]
  if(housekeeping)records.unshift(
    {type:'session_meta',payload:{id:'housekeeping-session'}},
    {type:'turn_context',payload:{cwd:'synthetic-workspace'}},
    {type:'event_msg',payload:{type:'token_count',info:{total_tokens:42}}},
    {type:'event_msg',payload:{type:'novel_event',preserved:true}},
  )
  if(encrypted)records.push({type:'response_item',payload:{type:'reasoning',encrypted_content:'ciphertext-must-stay-hidden'}},{type:'assistant',message:{content:[{type:'redacted_thinking',data:'claude-secret-ciphertext'}]}})
  records.push(...extraRecords)
  records.forEach((record,index)=>{record.timestamp=eventTimes[index] || (index<2?eventTimes[0]:'') || record.timestamp || stamp})
  const bytes=transcriptBytes===null?(empty?Buffer.alloc(0):Buffer.from(records.map(r=>JSON.stringify(r)).join('\n')+'\r\n')):Buffer.from(transcriptBytes)
  const activeId='20000000-0000-4000-8000-000000000001',savedId='20000000-0000-4000-8000-000000000002'
  const recordCount=transcriptBytes===null?(empty?0:records.length):bytes.toString('utf8').split('\n').filter(line=>line.trim()).length
  const base={workspace_id:f.workspace.id,format_label:'Codex · Session explorer',runtime_identity:runtime,runtime_revision:0,created_at:stamp,last_seen_at:stamp,committed_bytes:bytes.length,committed_records:recordCount,revision:1,ends_with_newline:true,...(creator||{})}
  const active={...base,id:activeId,state:'active'},saved={...base,id:savedId,format_label:'Claude · UX review',state:'terminated',terminated_at:stamp,final_bytes:bytes.length,final_sha256:createHash('sha256').update(bytes).digest('hex')}
  const content=new Map()
  const activeSessions=Array.from({length:activeCount},(_,i)=>({...active,id:i===0?active.id:`30000000-0000-4000-8000-${String(i+1).padStart(12,'0')}`,format_label:i===0?active.format_label:['Claude · Implementation','Codex · Follow-up'][i%2],created_at:new Date(Date.parse(stamp)-3600000-i*1000).toISOString(),last_seen_at:new Date(Date.parse(stamp)-i*60000).toISOString(),...(activeCount>1?{creator_actor_type:'bot',creator_bot_identity:{role:i%2?'Reviewer':'Developer','model-id':i%2?'claude:declared-review-model:high':'codex:gpt-6.1-sol:high'}}:{})}))
  const savedSessions=Array.from({length:savedCount},(_,i)=>{
    const data=savedCount===1?bytes:i%4===0?Buffer.alloc(0):i%4===1?Buffer.from('{"text":"λ café"}\r\n'):i%4===2?Buffer.from('[true,null]'):Buffer.from(JSON.stringify('x'.repeat(140000))+'\n')
    const session={...saved,id:i===0?saved.id:`40000000-0000-4000-8000-${String(i+1).padStart(12,'0')}`,format_label:savedCount===1?saved.format_label:i%2?'Review / café ../capture':'Review / café ../capture',created_at:new Date(Date.parse(stamp)-7200000-i*1000).toISOString(),last_seen_at:new Date(Date.parse(stamp)-600000-i*1000).toISOString(),terminated_at:i%3===2?null:new Date(Date.parse(stamp)-300000-i*1000).toISOString(),committed_bytes:data.length,final_bytes:data.length,final_sha256:createHash('sha256').update(data).digest('hex'),...(savedCount>1?{creator_actor_type:'bot',creator_bot_identity:{role:i%2?'Reviewer':'Developer','model-id':i%2?'claude:actual-review-model:high':'codex:actual-builder-model:medium'}}:{})};content.set(session.id,data);return session
  })
  // Keep exposed first records identical to the objects served by the fixture.
  Object.assign(active,activeSessions[0]);if(activeCount)activeSessions[0]=active
  if(savedCount){Object.assign(saved,savedSessions[0]);savedSessions[0]=saved}
  for(const session of activeSessions)content.set(session.id,bytes)
  const allSessions=[...activeSessions,...savedSessions].sort((a,b)=>b.created_at.localeCompare(a.created_at)||b.id.localeCompare(a.id))
  const h=await openHierarchy(f,[],null,{timezoneId},launchOptions),reads=[]
  let requestActive=0,peak=0
  const linked=new Set(many?[f.tasks.at(-1).id]:f.tasks.map(t=>t.id))
  const sessionLinks=new Map(allSessions.map(session=>[session.id,new Set(linked)]))
  const scoped=(task,project)=>{
    const projects=new Set([project]);for(let i=0;i<f.projects.length;i++)for(const p of f.projects)if(projects.has(p.parent_id))projects.add(p.id)
    const seen=new Set();let current=task
    while(current&&!seen.has(current.id)){seen.add(current.id);if(projects.has(current.project_id))return true;current=f.tasks.find(t=>t.id===current.parent_id)}return false
  }
  f.sessionActivity=id=>allSessions.some(session=>session.state==='active'&&sessionLinks.get(session.id)?.has(id))
  const eligible=()=>f.tasks.filter(t=>allSessions.some(session=>session.state==='active'&&sessionLinks.get(session.id)?.has(t.id)))
  await h.page.route('**/api/v1/**',async route=>{
    const u=new URL(route.request().url()),path=u.pathname
    if(!path.includes('/sessions'))return route.fallback()
    reads.push({path,query:u.search});requestActive++;peak=Math.max(peak,requestActive)
    try{
      const metadata=allSessions.find(session=>path.includes(session.id))||active
      let result
      if(path.endsWith('/filters')){
        const kind=u.searchParams.get('kind'),project=u.searchParams.get('project_id'),after=u.searchParams.get('after_id'),selected=u.searchParams.get('selected_id')
        const candidates=(kind==='projects'?f.projects.filter(p=>eligible().some(t=>scoped(t,p.id))).map(p=>({id:p.id,title:p.name})):eligible().filter(t=>!project||scoped(t,project)).map(t=>({id:t.id,title:t.title}))).sort((a,b)=>a.id.localeCompare(b.id))
        const remaining=candidates.filter(item=>!after||item.id>after),items=remaining.slice(0,50)
        result={items,next_id:remaining.length>50?items.at(-1).id:null,selected:candidates.find(item=>item.id===selected)||null}
      }
      else if(path.endsWith('/transcript')||path.endsWith('/evidence')){const data=content.get(metadata.id)||bytes,offset=Number(u.searchParams.get('offset')||0),end=Math.min(data.length,offset+Number(u.searchParams.get('limit')||65536));result={session:metadata,offset,next_offset:end,data_base64:data.subarray(offset,end).toString('base64'),has_more:end<data.length}}
      else if(path.endsWith('/tasks'))result={items:f.tasks.filter(t=>sessionLinks.get(metadata.id)?.has(t.id)).slice(0,linkedCount).map(t=>({id:t.id,title:t.title,status:t.status})),next_task_id:null}
      else if(allSessions.some(session=>path.endsWith(session.id)))result=metadata
      else {let items=allSessions;if(path.includes('/tasks/'))items=items.filter(session=>f.tasks.some(t=>sessionLinks.get(session.id)?.has(t.id)&&path.includes(t.id)));const project=u.searchParams.get('project_id'),task=u.searchParams.get('task_id');if(project)items=items.filter(session=>f.tasks.some(t=>sessionLinks.get(session.id)?.has(t.id)&&scoped(t,project)));if(task)items=items.filter(session=>sessionLinks.get(session.id)?.has(task)&&(!project||f.tasks.some(t=>t.id===task&&scoped(t,project))));if(u.searchParams.get('state'))items=items.filter(s=>s.state===u.searchParams.get('state'));if(u.searchParams.get('freshness'))items=items.filter(s=>(Date.now()-Date.parse(s.last_seen_at)<30000)===(u.searchParams.get('freshness')==='fresh'));const before=u.searchParams.get('before_created_at'),beforeId=u.searchParams.get('before_id');if(before)items=items.filter(session=>session.created_at<before||(session.created_at===before&&session.id<beforeId));const page=items.slice(0,50),last=page.at(-1);result={items:page,next_cursor:items.length>50?{created_at:last.created_at,id:last.id}:null}}
      await route.fulfill({json:result})
    }finally{requestActive--}
  })
  return {...h,start:async(fragment="tab=projects",anchor=f.projects[0].id)=>h.start(fragment,anchor),active,saved,bytes,activeSessions,savedSessions,allSessions,sessionLinks,content,reads,peak:()=>peak,async appendRecords(...added){
    const data=Buffer.concat([content.get(active.id),Buffer.from(added.map(record=>JSON.stringify(record)).join('\n')+'\n')]);content.set(active.id,data)
    Object.assign(active,{committed_bytes:data.length,committed_records:active.committed_records+added.length,revision:active.revision+1,last_seen_at:new Date().toISOString()})
    await h.page.evaluate(({workspace,session,revision})=>window.pushHierarchyFrames([{schema_version:1,type:'change',event:{schema_version:1,event_id:'append-'+revision,scope:'workspace',workspace_id:workspace,occurred_at:new Date().toISOString(),transaction:{id:'append-'+revision,cause:'rest',request_id:null},actor:{type:'system',id:null},entity:{type:'agent_session',id:session,action:'updated'},invalidations:[{kind:'entity',target:'agent_session:'+session},{kind:'collection',target:'sessions:'+workspace}]}}]),{workspace:f.workspace.id,session:active.id,revision:active.revision})
  },async associate(task){linked.add(task);sessionLinks.get(active.id)?.add(task);await h.page.evaluate(({workspace,session,task})=>window.pushHierarchyFrames([{schema_version:1,type:'change',event:{schema_version:1,event_id:'association-'+task,scope:'workspace',workspace_id:workspace,occurred_at:new Date().toISOString(),transaction:{id:'associate-'+task,cause:'rest',request_id:null},actor:{type:'system',id:null},entity:{type:'task_session',id:session+':'+task,action:'created'},invalidations:[{kind:'entity',target:'task_session:'+session+':'+task},{kind:'entity',target:'agent_session:'+session},{kind:'collection',target:'sessions:'+workspace},{kind:'collection',target:'task_sessions:'+task},{kind:'collection',target:'active_sessions:'+workspace}]}}]),{workspace:f.workspace.id,session:activeId,task})}}
}
