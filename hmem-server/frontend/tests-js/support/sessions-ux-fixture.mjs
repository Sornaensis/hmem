import { hierarchyFixture, openHierarchy } from '../hierarchy-fixture.mjs'
import { createHash } from 'node:crypto'
export async function openSessionExplorer({many=false,empty=false,creator=null,housekeeping=false}={}) {
  const f=hierarchyFixture();f.projects=[f.projects[0]]
  f.tasks=Array.from({length:many?22:2},(_,i)=>({...f.tasks[0],id:`10000000-0000-4000-8000-${String(i+1).padStart(12,'0')}`,title:i===0?'Build session explorer':i===1?'Review captured conversations':'Task '+(i+1),description:'Keep conversations easy to explore.',parent_id:null}))
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
  const bytes=empty?Buffer.alloc(0):Buffer.from(records.map(r=>JSON.stringify(r)).join('\n')+'\r\n')
  const stamp=new Date().toISOString();const activeId='20000000-0000-4000-8000-000000000001',savedId='20000000-0000-4000-8000-000000000002'
  const base={workspace_id:f.workspace.id,format_label:'Codex · Session explorer',created_at:stamp,last_seen_at:stamp,committed_bytes:bytes.length,committed_records:empty?0:records.length,revision:1,ends_with_newline:true,...(creator||{})}
  const active={...base,id:activeId,state:'active'},saved={...base,id:savedId,format_label:'Claude · UX review',state:'terminated',terminated_at:stamp,final_bytes:bytes.length,final_sha256:createHash('sha256').update(bytes).digest('hex')}
  const h=await openHierarchy(f),reads=[]
  let requestActive=0,peak=0
  const linked=new Set(many?[f.tasks.at(-1).id]:f.tasks.map(t=>t.id))
  await h.page.route('**/api/v1/**',async route=>{
    const u=new URL(route.request().url()),path=u.pathname
    if(!path.includes('/sessions'))return route.fallback()
    reads.push({path,query:u.search});requestActive++;peak=Math.max(peak,requestActive)
    try{
      const metadata=path.includes(savedId)?saved:active
      let result
      if(path.endsWith('/transcript')||path.endsWith('/evidence')){const offset=Number(u.searchParams.get('offset')||0),end=Math.min(bytes.length,offset+Number(u.searchParams.get('limit')||65536));result={session:metadata,offset,next_offset:end,data_base64:bytes.subarray(offset,end).toString('base64'),has_more:end<bytes.length}}
      else if(path.endsWith('/tasks'))result={items:f.tasks.slice(0,2).map(t=>({id:t.id,title:t.title,status:t.status})),next_task_id:null}
      else if(path.endsWith(activeId)||path.endsWith(savedId))result=metadata
      else {let items=[saved,active];if(path.includes('/tasks/')&&!f.tasks.some(t=>linked.has(t.id)&&path.includes(t.id)))items=[];if(u.searchParams.get('state'))items=items.filter(s=>s.state===u.searchParams.get('state'));result={items,next_cursor:null}}
      await route.fulfill({json:result})
    }finally{requestActive--}
  })
  return {...h,active,saved,bytes,reads,peak:()=>peak,async associate(task){linked.add(task);await h.page.evaluate(({workspace,session,task})=>window.pushHierarchyFrames([{schema_version:1,type:'change',event:{schema_version:1,event_id:'association-'+task,scope:'workspace',workspace_id:workspace,occurred_at:new Date().toISOString(),transaction:{id:'associate-'+task,cause:'rest',request_id:null},actor:{type:'system',id:null},entity:{type:'task_session',id:session+':'+task,action:'created'},invalidations:[{kind:'entity',target:'task_session:'+session+':'+task},{kind:'entity',target:'agent_session:'+session},{kind:'collection',target:'sessions:'+workspace},{kind:'collection',target:'task_sessions:'+task}]}}]),{workspace:f.workspace.id,session:activeId,task})}}
}
