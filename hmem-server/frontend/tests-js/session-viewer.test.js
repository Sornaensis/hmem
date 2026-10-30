import assert from 'node:assert/strict'
import test from 'node:test'
import { createRecordFramer, decodeBase64, MAX_VIEW_BYTES, MAX_VIEW_ROWS, MAX_OCCURRENCE_ROWS, MAX_OCCURRENCE_BYTES, presentation, toolPayload } from '../src/session-viewer.js'
import {copilotRecords,piRecords} from './support/native-session-fixtures.mjs'
const encode = value => new TextEncoder().encode(value)

test('meaningful unhandled Codex events retain exact inspectable JSON and activity source order',()=>{
 const payload={type:'future_event',message:'<script>inert</script>',details:{values:[null,false,7,'λ😀']}},values=[
  {type:'event_msg',timestamp:'2026-10-30T12:30:00Z',payload},
  {type:'event_msg',timestamp:'2026-10-30T11:30:00+01:00',payload:{type:'another_event',result:'older final event'}},
  {type:'event_msg',timestamp:'invalid',time:'2026-10-30T15:00:00Z',payload:{type:'last_event',result:42}},
  {type:'event_msg',payload:{type:'token_count',message:'hidden'}},
  {type:'event_msg',payload:{type:'agent_message',message:'Visible supported message'}}
 ]
 const bytes=encode(values.map(value=>JSON.stringify(value)+'\r\n').join('')),rows=createRecordFramer().push(0,bytes,true).rows
 assert.equal(rows.length,4);assert.deepEqual(rows.map(row=>row.time),['2026-10-30T12:30:00Z','2026-10-30T11:30:00+01:00','',''])
 assert.deepEqual(JSON.parse(JSON.parse(rows[0].text)[0].text),payload);assert.equal(JSON.parse(rows[0].text)[0].kind,'event')
 assert.equal(JSON.parse(rows[0].text)[0].label,'Event · future_event');assert.doesNotMatch(rows[0].text,/Unsupported transcript content/)
 assert.equal(rows[1].activityAnchors[0],rows[0].activityAnchors[0]);assert.equal(rows[2].activityAnchors[0],rows[0].activityAnchors[0])
 assert.equal(JSON.parse(rows[3].text)[0].kind,'text');assert.equal(rows[0].raw,JSON.stringify(values[0]));assert.equal(rows[0].delimiter,'\r\n')
 for(const timestamp of ['2026-02-30T00:00:00Z','2026-10-30T00:00:00',1793361600])assert.equal(presentation({type:'event_msg',timestamp,payload},'').time,'')
})

test('raw Codex event payloads retain scalar tokens and avoid recursion at ten thousand levels',()=>{
 const raw='{"type":"event_msg","payload":{"type":"precision","n":9007199254740993,"overflow":1e400,"underflow":1e-400,"negative":-0,"escaped":"\\u0061","duplicate":1,"duplicate":2}}'
 const row=createRecordFramer().push(0,encode(raw+'\n'),true).rows[0],source=JSON.parse(row.text)[0].text
 assert.equal(source,raw.slice(raw.indexOf('{',raw.indexOf('payload')), -1));assert.equal(row.raw,raw)
 for(const token of ['9007199254740993','1e400','1e-400','-0','\\u0061','"duplicate":1,"duplicate":2'])assert.ok(source.includes(token),token)
 const duplicate='{"payload":{"result":"first"},"type":"event_msg","pay\\u006coad":{"result":9007199254740993}}'
 assert.equal(JSON.parse(presentation(JSON.parse(duplicate),duplicate).text)[0].text,'{"result":9007199254740993}')
 const deep='{"type":"event_msg","payload":'+'['.repeat(10000)+'0'+']'.repeat(10000)+'}'
 const framed=createRecordFramer().push(0,encode(deep+'\n'),true).rows[0]
 assert.equal(framed.raw,deep);assert.equal(JSON.parse(framed.text)[0].text,'['.repeat(10000)+'0'+']'.repeat(10000))
 assert.equal(JSON.parse(presentation(JSON.parse(deep),'').text)[0].text,JSON.parse(framed.text)[0].text,'the no-source fallback is iterative too')
})

test('native Copilot data envelopes retain each request, execution and result occurrence with exact evidence offsets',()=>{
 const source=copilotRecords.map(value=>JSON.stringify(value)+'\r\n').join(''),bytes=encode(source),framer=createRecordFramer()
 for(let at=0;at<bytes.length;at+=37)framer.push(at,bytes.subarray(at,at+37),at+37>=bytes.length)
 const view=framer.snapshot(),blocks=view.rows.flatMap(row=>JSON.parse(row.text))
 assert.deepEqual(view.rows.map(row=>row.role),['user','assistant','assistant','tool','tool','assistant'])
 assert.deepEqual(blocks.map(block=>block.kind),['text','thinking','text','tool','tool','result','result','text'])
 assert.match(blocks[3].label,/Request · read_file · call-1/);assert.match(blocks[4].label,/Execution · read_file · call-1/)
 assert.match(blocks[5].label,/Result · call-1/);assert.match(blocks[6].label,/Error result · call-error/)
 assert.equal(JSON.parse(blocks[5].text).detailedContent,'complete hello λ😀');assert.equal(JSON.parse(blocks[6].text).error.code,'ENOENT')
 let offset=0;for(const value of copilotRecords){const raw=JSON.stringify(value),end=offset+encode(raw+'\r\n').length,row=view.rows.find(row=>row.offset===offset);if(value.type!=='session.start'){assert.equal(row.raw,raw);assert.equal(row.end,end);assert.equal(row.delimiter,'\r\n')}offset=end}
 assert.equal(view.through,bytes.length);assert.equal(view.pendingBytes,0)
 for(const value of [copilotRecords[2],copilotRecords[3]])assert.ok(createRecordFramer().push(0,encode(JSON.stringify(value)+'\n'),true).rows.length===1,'request-only and execution-only windows remain readable')
})

test('unsupported meaningful content is bounded and truthful while native metadata and deltas stay hidden',()=>{
 const values=[{type:'session_meta',payload:{secret:'hidden'}},{type:'turn_context',payload:{secret:'hidden'}},{type:'event_msg',payload:{type:'token_count',secret:'hidden'}},{type:'assistant.message_delta',data:{deltaContent:'partial hidden'}},{type:'future.output',data:{newContent:'do not dump '+ 'x'.repeat(10000)}},{type:'assistant',message:{content:[{type:'future_block',secret:'do not dump'}]}}]
 const bytes=encode(values.map(value=>JSON.stringify(value)).join('\n')+'\n'),view=createRecordFramer().push(0,bytes,true)
 assert.equal(view.rows.length,2);assert.equal(view.through,bytes.length);assert.equal(view.pendingBytes,0)
 for(const row of view.rows){assert.match(row.text,/Unsupported transcript content/);assert.ok(row.text.length<256);assert.doesNotMatch(row.text,/do not dump|partial hidden|secret/)}
 const missing=presentation({type:'tool.execution_complete',data:{toolCallId:'missing',success:true}},'')
 assert.match(missing.text,/No result recorded/)
 for(const content of ['', 'Mixed attachment message']){
  const attachment={type:'user.message',data:{content,attachments:[{type:'file',path:'private-path-must-not-dump'}]}},row=createRecordFramer().push(0,encode(JSON.stringify(attachment)+'\n'),true).rows[0]
  assert.ok(row);assert.match(row.text,/Copilot attachments/);assert.doesNotMatch(row.text,/private-path-must-not-dump/)
 }
 const image={...piRecords[1],message:{role:'user',content:[{type:'image',data:'base64-secret',mimeType:'image/png'}]}}
 const imageRow=createRecordFramer().push(0,encode(JSON.stringify(image)+'\n'),true).rows[0]
 assert.match(imageRow.text,/Unsupported transcript content \(image\)/);assert.doesNotMatch(imageRow.text,/base64-secret/)
 for(const metadata of [{type:'assistant.usage',data:{inputTokens:42}},{type:'session_info',id:'info',parentId:'p1',name:'private-session-name'},{type:'usage',id:'usage',parentId:'info',inputTokens:42}])assert.equal(createRecordFramer().push(0,encode(JSON.stringify(metadata)+'\n'),true).rows.length,0)
 for(const type of ['assistant.streaming_delta','session.resume','session.context_changed','session.info','session.plan_changed','session.workspace_file_changed','session.compaction_start','pending_messages.modified','command.queued','command.completed','permission.requested','permission.completed','hook.start','hook.end','skill.invoked','subagent.started','subagent.completed','subagent.selected','subagent.deselected','exit_plan_mode.completed'])assert.equal(createRecordFramer().push(0,encode(JSON.stringify({type,data:{routine:'hidden'}})+'\n'),true).rows.length,0,type+' is routine control metadata')
 for(const type of ['session.error','session.warning','subagent.failed','session.truncation','session.snapshot_rewind','session.compaction_complete','hook.end','command.completed','permission.completed'])assert.match(presentation({type,data:{error:{message:'reported failure or history change'}}},'').text,/Unsupported transcript content/,type+' must retain an indication')
 for(const data of [undefined,null,42,'invalid']){const row=presentation({type:'assistant.message',id:'malformed-copilot',parentId:null,data},'');assert.match(row.text,/Unsupported transcript content \(assistant.message\)/);assert.doesNotMatch(row.text,/pi history/)}
 for(const value of [{type:'mystery',payload:{preserved:true}},{type:'event_msg',payload:{type:'novel_event',preserved:true}},{type:'future',data:{id:'opaque-id',type:'marker',parentId:'opaque-parent',timestamp:'2026-10-30',version:3,preserved:false}}])assert.equal(createRecordFramer().push(0,encode(JSON.stringify(value)+'\n'),true).rows.length,0,'discriminator/flag-only metadata stays hidden')
 for(const content of [false,null,0,42,{},[],{unknown:true}])assert.match(presentation({type:'future.output',data:{content}},'').text,/Unsupported transcript content/)
 for(const data of [{answer:42},{novel:'meaningful output'},{nested:{child:{result:false}}},{newContent:{unknown:true}},{items:[false,true]}])assert.match(presentation({type:'future.output',data},'').text,/Unsupported transcript content/)
})

test('pi v3 source history distinguishes native parents, alternate branches and later roots without exposing hidden state',()=>{
 const bytes=encode(piRecords.map(value=>JSON.stringify(value)).join('\r\n')+'\r\n'),framer=createRecordFramer()
 for(let at=0;at<bytes.length;at+=19)framer.push(at,bytes.subarray(at,at+19),at+19>=bytes.length)
 const view=framer.snapshot(),contents=view.rows.map(row=>JSON.parse(row.text)),display=JSON.stringify(contents)
 assert.equal(view.rows.length,10);assert.equal(view.through,bytes.length);assert.equal(view.pendingBytes,0)
 for(const expected of ['root p1','entry p3 → parent p2','entry p5 → parent p1','root p13','alternate branches','ancestry is not expanded','Compaction summary','first kept p3','Branch summary','from p4','Visible pi custom message.','visible bash output','context edit recorded'])assert.ok(display.includes(expected),expected)
 for(const hidden of ['redacted-secret','opaque-secret','hidden-extension-state','hidden-custom-message','not-authoritative'])assert.ok(!display.includes(hidden),hidden)
 assert.ok(contents.flat().some(block=>block.kind==='encrypted'))
 assert.ok(contents.flat().some(block=>block.kind==='tool' && block.label.includes('pi-call')))
 assert.ok(contents.flat().some(block=>block.kind==='result' && block.label.includes('pi-call')))
 assert.ok(view.rows.every(row=>row.piVersion===3));assert.ok(!display.includes('version is not established'))
 let at=0;for(const value of piRecords){const raw=JSON.stringify(value),row=view.rows.find(row=>row.offset===at);if(row){assert.equal(row.raw,raw);assert.equal(row.end,at+encode(raw+'\r\n').length);assert.equal(row.delimiter,'\r\n')}at+=encode(raw+'\r\n').length}
})

test('known operation failure flags remain explicit without error details while successful controls stay hidden',()=>{
 const values=['hook.end','command.completed','permission.completed'].flatMap(type=>[{type,data:{success:true}},{type,data:{success:false}}]),raw=values.map(value=>JSON.stringify(value)+'\n').join(''),bytes=encode(raw),rows=createRecordFramer().push(0,bytes,true).rows
 assert.equal(rows.length,3)
 for(let at=0;at<rows.length;at++){
  const row=rows[at],value=values[at*2+1]
  assert.match(row.text,/Transcript operation failed/);assert.ok(row.text.includes(value.type));assert.equal(row.raw,JSON.stringify(value));assert.equal(row.end-row.offset,encode(row.raw+'\n').length)
  assert.match(presentation(value,'').text,/Transcript operation failed/);assert.equal(presentation(values[at*2],'').type,'unknown')
 }
})

test('pi header versions and bounded windows have explicit support and ancestry limits',()=>{
 for(const version of [1,2,99,'invalid']){
  const bytes=encode([{type:'session',version},piRecords[1]].map(value=>JSON.stringify(value)).join('\n')+'\n'),view=createRecordFramer().push(0,bytes,true)
  assert.equal(view.rows.length,2);assert.match(view.rows[1].text,/Unsupported transcript content/);assert.doesNotMatch(view.rows[1].text,/Inspect pi café/)
 }
 const bytes=encode('leading fragment\n'+JSON.stringify(piRecords[5])+'\n'),view=createRecordFramer(123).push(123,bytes,true)
 assert.equal(view.rows.length,1);assert.match(view.rows[0].text,/entry p5 → parent p1/);assert.match(view.rows[0].text,/parent context may be outside this window/);assert.match(view.rows[0].text,/Schema version is not established/)
})

test('pi history context does not let tool bursts evict the separate prose reserve',()=>{
 const framer=createRecordFramer(),feed=value=>{const bytes=encode(JSON.stringify(value)+'\n');return framer.push(framer.snapshot().through,bytes,true)}
 feed(piRecords[0]);feed(piRecords[1]);const userOffset=framer.snapshot().rows[0].offset
 for(let at=0;at<150;at++)feed({...piRecords[4],id:'tool-'+at,parentId:at?'tool-'+(at-1):'p1'})
 const rows=framer.snapshot().rows;assert.ok(rows.some(row=>row.offset===userOffset));assert.equal(rows.length,MAX_OCCURRENCE_ROWS+1)
})

test('visible prose has a separate bounded reserve from metadata and tool bursts with stable source run anchors',()=>{
 const framer=createRecordFramer(),feed=value=>{const bytes=encode(JSON.stringify(value)+'\n');let result;for(let i=0;i<bytes.length;i+=65536)result=framer.push(framer.snapshot().through,bytes.subarray(i,i+65536),i+65536>=bytes.length);return result}
 const prose=()=>({type:'response_item',payload:{type:'message',role:'assistant',content:[{type:'output_text',text:'Repeated agent message.'}]}})
 feed(prose());const firstMessage=framer.snapshot().rows[0].offset
 for(let i=0;i<240;i++)feed({type:'turn_context',payload:{index:i,padding:'x'.repeat(1000)}})
 const runOffset=framer.snapshot().through
 for(let i=0;i<180;i++)feed({type:'response_item',payload:{type:'function_call',name:'read',arguments:JSON.stringify({index:i})}})
 let view=framer.snapshot(),activity=view.rows.filter(row=>JSON.parse(row.text)[0].kind==='tool')
 assert.equal(view.rows.find(row=>row.offset===firstMessage).role,'assistant')
 assert.equal(activity.length,MAX_OCCURRENCE_ROWS);assert.ok(activity.every(row=>row.activityAnchors[0]===runOffset+':0'))
 feed(prose());const secondMessage=framer.snapshot().rows.at(-1).offset
 feed({type:'response_item',payload:{type:'function_call',name:'read',arguments:'{}'}})
 view=framer.snapshot();const last=view.rows.at(-1)
 assert.notEqual(last.activityAnchors[0],runOffset+':0','a prose boundary gives identical activity a distinct run')
 assert.deepEqual(view.rows.filter(row=>row.role==='assistant'&&JSON.parse(row.text)[0].kind==='text').map(row=>row.offset),[firstMessage,secondMessage],'identical later prose remains a distinct occurrence')
 assert.ok(view.rows.length<=MAX_VIEW_ROWS);assert.ok(view.retainedBytes<=MAX_VIEW_BYTES)
})

test('message and activity byte reserves remain independent under maximum-sized visible records',()=>{
 const framer=createRecordFramer(),feed=value=>{const bytes=encode(JSON.stringify(value)+'\n');for(let i=0;i<bytes.length;i+=65536)framer.push(framer.snapshot().through,bytes.subarray(i,i+65536),i+65536>=bytes.length)}
 for(let i=0;i<3;i++)feed({text:String(i)+'λ'.repeat(450000)})
 const keptMessages=framer.snapshot().rows.map(row=>row.offset)
 for(let i=0;i<3;i++)feed({type:'response_item',payload:{type:'function_call_output',output:'x'.repeat(900000)}})
 feed({type:'turn_context',payload:{padding:'x'.repeat(1000000)}})
 const view=framer.snapshot(),messages=view.rows.filter(row=>row.type!=='conversation'),activity=view.rows.filter(row=>row.type==='conversation')
 assert.deepEqual(messages.map(row=>row.offset),keptMessages)
 assert.equal(messages.length,2);assert.equal(activity.length,2)
 for(const rows of [messages,activity])assert.ok(rows.reduce((total,row)=>total+encode(row.raw).length+row.delimiter.length,0)<=MAX_OCCURRENCE_BYTES)
 assert.ok(view.retainedBytes<=MAX_VIEW_BYTES)
})

test('paired native envelopes keep the first real occurrence anchor through final-LF replacement and repeated events',()=>{
 const event={type:'event_msg',timestamp:'2026-10-24T10:00:00.000Z',payload:{type:'agent_reasoning',text:'same'}}
 const canonical={type:'response_item',timestamp:event.timestamp,payload:{type:'reasoning',summary:[{text:'same'}]}}
 const first=encode(JSON.stringify(event)+'\n'),second=encode(JSON.stringify(canonical)),framer=createRecordFramer()
 assert.equal(framer.push(0,first,true).rows[0].presentationOffset,0)
 let view=framer.push(first.length,second,true)
 assert.equal(view.rows.length,1);assert.equal(view.rows[0].offset,first.length);assert.equal(view.rows[0].presentationOffset,0)
 assert.equal(view.rows[0].raw,JSON.stringify(canonical));assert.equal(view.rows[0].delimiter,'')
 view=framer.push(first.length+second.length,encode('\r\n'),true)
 assert.equal(view.rows[0].presentationOffset,0);assert.equal(view.rows[0].delimiter,'\r\n')
 const nextOffset=view.through
 view=framer.push(nextOffset,first,true)
 assert.equal(view.rows.length,2);assert.equal(view.rows[1].presentationOffset,nextOffset)
 view=framer.push(view.through,encode(JSON.stringify(canonical)+'\n'),true)
 assert.equal(view.rows.length,2);assert.deepEqual(view.rows.map(row=>row.presentationOffset),[0,nextOffset])
 assert.ok(view.rows.every(row=>!('pairedOffset'in row)&&!('eventEnvelope'in row)))
})

test('trailing whitespace after a provisional paired EOF retains its exact source occurrence and run anchors',()=>{
 const event={type:'event_msg',timestamp:'2026-10-24T10:00:00Z',payload:{type:'agent_reasoning',text:'same'}}
 const canonical={type:'response_item',timestamp:event.timestamp,payload:{type:'reasoning',summary:[{text:'same'}]}}
 const first=encode(JSON.stringify(event)+'\n'),second=encode(JSON.stringify(canonical)),framer=createRecordFramer()
 framer.push(0,first,true);const provisional=framer.push(first.length,second,true).rows[0]
 const complete=framer.push(first.length+second.length,encode('  \r\n'),true).rows[0]
 assert.equal(complete.presentationOffset,provisional.presentationOffset);assert.deepEqual(complete.activityAnchors,provisional.activityAnchors)
 assert.equal(complete.raw,JSON.stringify(canonical)+'  ');assert.equal(complete.delimiter,'\r\n')
})

test('retained canonical anchors and consumed pairs ignore metadata without merging repeated occurrences',()=>{
 const event={type:'event_msg',timestamp:'2026-10-24T10:00:00Z',payload:{type:'agent_reasoning',text:'same'},padding:'x'.repeat(900000)}
 const canonical={type:'response_item',timestamp:event.timestamp,payload:{type:'reasoning',summary:[{text:'same'}]}}
 const later={...event,padding:''},framer=createRecordFramer()
 function feed(value){const bytes=encode(value);let view;for(let i=0;i<bytes.length;i+=65536)view=framer.push(framer.snapshot().through,bytes.subarray(i,i+65536),i+65536>=bytes.length);return view}
 feed(JSON.stringify(event)+'\n');const canonicalOffset=framer.snapshot().through
 feed(JSON.stringify(canonical)+'\n');const laterOffset=framer.snapshot().through
 feed(JSON.stringify(later)+'\n');feed(JSON.stringify('y'.repeat(900000))+'\n')
 const view=feed(JSON.stringify('z'.repeat(400000))+'\n')
 const visible=view.rows.filter(row=>row.type==='conversation')
 assert.deepEqual(visible.map(row=>row.offset),[canonicalOffset,laterOffset])
 assert.deepEqual(visible.map(row=>row.presentationOffset),[0,laterOffset])
 assert.ok(view.retainedBytes<=MAX_VIEW_BYTES)
})

test('canonical-first pairing does not resurrect its hidden event or consume a later identical canonical after eviction',()=>{
 const canonical={type:'response_item',timestamp:'2026-10-24T10:00:00Z',payload:{type:'reasoning',summary:[{text:'same'}]}}
 const event={type:'event_msg',timestamp:canonical.timestamp,payload:{type:'agent_reasoning',text:'same'}}
 const framer=createRecordFramer(),first=encode(JSON.stringify(canonical)+'\n'),second=encode(JSON.stringify(event)+'\n')
 framer.push(0,first,true);framer.push(first.length,second,true)
 const repeatedOffset=framer.snapshot().through
 framer.push(repeatedOffset,first,true)
 const churn=Array.from({length:MAX_OCCURRENCE_ROWS-1},(_,i)=>JSON.stringify({...canonical,payload:{type:'reasoning',summary:[{text:'new '+i}]}})+'\n').join('')
 const view=framer.push(framer.snapshot().through,encode(churn),true)
 const visible=view.rows.filter(row=>JSON.parse(row.text)[0].text==='same')
 assert.equal(visible.length,1);assert.equal(visible[0].offset,repeatedOffset);assert.equal(visible[0].presentationOffset,repeatedOffset)
 assert.ok(view.rows.length<=MAX_VIEW_ROWS)
})

test('both envelope directions retain one visible occurrence through a single metadata-heavy push',()=>{
 const event={type:'event_msg',timestamp:'2026-10-24T10:00:00Z',payload:{type:'agent_reasoning',text:'same'}}
 const canonical={type:'response_item',timestamp:event.timestamp,payload:{type:'reasoning',summary:[{text:'same'}]}}
 for(const eventFirst of [true,false]){
  const framer=createRecordFramer(),first=encode(JSON.stringify(eventFirst?event:canonical)+'\n')
  framer.push(0,first,true)
  const batch=encode(JSON.stringify(eventFirst?canonical:event)+'\n'+'null\n'.repeat(MAX_VIEW_ROWS-1))
  assert.ok(batch.length<=65536)
  const view=framer.push(first.length,batch,true),visible=view.rows.filter(row=>row.type==='conversation')
  assert.equal(visible.length,1)
  if(eventFirst){assert.equal(visible[0].offset,first.length);assert.equal(visible[0].presentationOffset,0);assert.equal(visible[0].raw,JSON.stringify(canonical));assert.equal(visible[0].delimiter,'\n')}
  assert.equal(view.through,first.length+batch.length);assert.ok(view.rows.length<=MAX_VIEW_ROWS)
 }
})

test('tool JSON is formatted and tokenized without changing payloads or growing unbounded spans',()=>{
 for(const value of [{path:'src/a.js',n:12,ok:true,missing:null},'{"query":"café"}', 'plain tool output',Array.from({length:10000},(_,i)=>i)]) {
  const result=toolPayload(value)
  assert.equal(result.tokens.map(token=>token.text).join(''),result.text)
  assert.ok(result.tokens.length<=2049)
  assert.doesNotThrow(()=>JSON.parse(result.text))
 }
 const call=JSON.parse(presentation({type:'response_item',payload:{type:'function_call',name:'mcp__hmem__task_detail',arguments:'{"task_id":"123"}'}},'').text)[0]
 assert.match(call.label,/Call · hmem · task_detail/)
 assert.ok(call.tokens.some(token=>token.kind==='key'))
})

test('encrypted provider thinking is represented only by its placeholder',()=>{
 for(const value of [{type:'response_item',payload:{type:'reasoning',encrypted_content:'secret',summary:[]}},{type:'assistant',message:{content:[{type:'redacted_thinking',data:'secret'}]}}]) {
  const blocks=JSON.parse(presentation(value,'').text)
  assert.deepEqual(blocks,[{kind:'encrypted',text:'',label:'Thinking...'}])
 }
})

test('frames split UTF-8/CRLF and arbitrary JSON before Unicode decoding', () => {
  const text = '{"text":"λ😀","type":"message","role":"assistant"}\r\n[1,"unknown"]\ntrue\nnull\n{"text":"scalar"}'
  const bytes = encode(text), framer = createRecordFramer()
  for(let i=0;i<bytes.length;i++) framer.push(i,bytes.subarray(i,i+1),i===bytes.length-1)
  const view = framer.snapshot()
  assert.equal(view.rows.length,2); assert.equal(view.rows[0].text,'λ😀'); assert.equal(view.rows[0].delimiter,'\r\n')
  assert.deepEqual(view.rows.map(row => JSON.parse(row.raw)),[{text:'λ😀',type:'message',role:'assistant'},{text:'scalar'}])
  assert.equal(view.through,bytes.length)
  const separator = encode('\r\n{"text":"next"}\n')
  const next = framer.push(bytes.length,separator,true)
  assert.equal(next.rows.length,3); assert.equal(next.rows[1].delimiter,'\r\n')
})

test('one 1MiB record survives more than four chunks while retained rows and bytes stay bounded', () => {
  const bytes = encode(JSON.stringify({text:'x'.repeat(1048500)})+'\n'), framer = createRecordFramer()
  for(let i=0;i<bytes.length;i+=65536) framer.push(i,bytes.subarray(i,i+65536),i+65536>=bytes.length)
  assert.equal(framer.snapshot().rows[0].raw.length,bytes.length-1)
  let offset = bytes.length
  for(let i=0;i<250;i++){ const chunk=encode(JSON.stringify({text:'small'+i})+'\n'); framer.push(offset,chunk,true); offset+=chunk.length }
  assert.ok(framer.snapshot().rows.length<=MAX_VIEW_ROWS); assert.ok(framer.snapshot().retainedBytes<=MAX_VIEW_BYTES)
})

test('finite mid-record windows omit leading fragments and preserve exact offsets without duplicate retries', () => {
  const framer=createRecordFramer(10), chunk=encode('fragment\r\n{"text":"kept"}\n{"partial":')
  const result=framer.push(10,chunk,true)
  assert.equal(result.rows.length,1); assert.equal(result.rows[0].offset,20); assert.ok(result.pendingBytes>0)
  assert.match(result.notice,/Incomplete record/)
  assert.deepEqual(framer.push(10,chunk,true),result)
  assert.throws(()=>framer.push(0,chunk),/range_gap/)
})

test('leading fragments are skipped silently while real finite boundary diagnostics remain',()=>{
 const framer=createRecordFramer(17),bytes=encode('omitted fragment\n{"text":"kept"}\n'),result=framer.push(17,bytes,true)
 assert.equal(result.notice,'');assert.equal(result.rows.length,1);assert.equal(result.rows[0].text,'kept');assert.equal(result.rows[0].offset,34)
 const partial=encode('{"text":"unfinished'),next=framer.push(result.through,partial,true)
 assert.match(next.notice,/Incomplete record/);assert.equal(next.rows[0].text,'kept')
})

test('rejects noncanonical base64 and overlarge records explicitly', () => {
  assert.deepEqual(decodeBase64('zrs='),encode('λ'))
  for(const value of ['Zh==','Zg','Zg==\n','____']) assert.throws(()=>decodeBase64(value),/invalid_base64/)
  const framer=createRecordFramer(), chunk=encode('x'.repeat(65536))
  for(let i=0;i<16;i++) framer.push(i*65536,chunk)
  assert.throws(()=>framer.push(1048576,chunk),/record_limit/)
})

test('finite boundary inside UTF-8 retains prior rows and completes exactly once later', () => {
  const bytes=encode('{"text":"before"}\n{"text":"λ"}\n'), split=bytes.indexOf(0xce)+1, framer=createRecordFramer()
  const initial=framer.push(0,bytes.subarray(0,split),true)
  assert.equal(initial.rows.length,1); assert.match(initial.notice,/Incomplete UTF-8/)
  const final=framer.push(split,bytes.subarray(split),true)
  assert.equal(final.rows.length,2); assert.equal(final.rows[1].text,'λ')
  assert.equal(final.notice,'')
})

test('enforces exact payload cap independently of LF and CRLF delimiters', () => {
  for(const delimiter of ['\n','\r\n']) for(const length of [1048576,1048577]) {
    const bytes=encode(JSON.stringify({text:'a'.repeat(length-11)})+delimiter), framer=createRecordFramer()
    const run=()=>{ for(let i=0;i<bytes.length;i+=65536) framer.push(i,bytes.subarray(i,i+65536),i+65536>=bytes.length) }
    if(length===1048576) {run();assert.equal(framer.snapshot().rows[0].raw.length,length)}
    else assert.throws(run,/record_limit/)
  }
})

test('overlapping finite older windows make a maximum-sized boundary record reachable',()=>{
  const before=encode('0\n'.repeat(1000000)), record=encode(JSON.stringify({text:'b'.repeat(1048565)})+'\r\n'), after=encode('true\n')
  const source=new Uint8Array(before.length+record.length+after.length)
  source.set(before);source.set(record,before.length);source.set(after,before.length+record.length)
  function window(begin,end) {
    const framer=createRecordFramer(begin)
    for(let offset=begin;offset<end;offset+=65536) framer.push(offset,source.subarray(offset,Math.min(end,offset+65536)),offset+65536>=end)
    return framer.snapshot()
  }
  const tailStart=2097152, tail=window(tailStart,source.length)
  assert.equal(tail.rows.some(row=>row.offset===before.length),false,'leading partial record omitted in tail')
  const olderStart=Math.max(0,tailStart-1048574),olderEnd=Math.min(source.length,tailStart+1048578)
  assert.ok(olderEnd-olderStart<=2097152)
  const older=window(olderStart,olderEnd), restored=older.rows.find(row=>row.offset===before.length)
  assert.equal(restored.raw.length,1048576);assert.equal(restored.delimiter,'\r\n')
  assert.equal(new Set(older.rows.map(row=>row.offset)).size,older.rows.length)
})


test('native Codex and Claude blocks retain full text and safely tolerate arbitrary nested JSON',()=>{
 const long='full message '.repeat(1000)
 const codex=presentation({type:'response_item',payload:{type:'message',role:'assistant',content:[{type:'output_text',text:long}]}},'')
 assert.equal(JSON.parse(codex.text)[0].text,long)
 assert.equal(presentation({type:'response_item',payload:{type:'message',role:{bad:true},content:42}},'').role,'assistant')
 assert.equal(JSON.parse(presentation({type:'response_item',payload:{type:'message',content:'scalar text'}},'').text)[0].text,'scalar text')
 const claude=presentation({type:'assistant',message:{role:'assistant',content:[{type:'thinking',thinking:'hidden'},{type:'tool_use',name:'Read',input:{file:'a'}},{type:'text',text:'**Ready**'}]}},'')
 assert.deepEqual(JSON.parse(claude.text).map(b=>b.kind),['thinking','tool','text'])
 for(const content of [null,true,42,'text',[null,false,42,'str',[],{type:'tool_result',content:42},{type:'tool_result',content:[null,42]},{type:'text',text:{bad:true}}]]) {
  assert.doesNotThrow(()=>presentation({type:'assistant',message:{content}},''))
  assert.doesNotThrow(()=>presentation({type:'response_item',payload:{type:'reasoning',summary:content}},''))
 }
 for(const value of [null,[],42,true,{unknown:'value'}])assert.equal(presentation(value,JSON.stringify(value)).type,'unknown')
})

test('native canonical messages suppress matching Codex event envelopes without altering offsets or raw bytes',()=>{
 const records=[{type:'event_msg',timestamp:'2026-10-24T10:00:00Z',payload:{type:'agent_message',message:'same'}},{type:'response_item',timestamp:'2026-10-24T10:00:00Z',payload:{type:'message',role:'assistant',content:[{type:'output_text',text:'same'}]}}]
 const source=records.map(r=>JSON.stringify(r)+'\r\n').join(''),bytes=encode(source),framer=createRecordFramer()
 const view=framer.push(0,bytes,true)
 assert.equal(view.rows.length,1);assert.equal(view.rows[0].offset,encode(JSON.stringify(records[0])+'\r\n').length)
 assert.equal(view.rows[0].raw,JSON.stringify(records[1]));assert.equal(view.rows[0].delimiter,'\r\n');assert.equal(view.through,bytes.length)
})


test('generic content selects its verified string and occurrence pairing preserves later repeated turns',()=>{
 assert.equal(presentation({text:42,content:'hello'},'').text,'hello')
 const message=(text,time)=>({type:'response_item',timestamp:time,payload:{type:'message',role:'user',content:[{type:'input_text',text}]}})
 const event=(text,time)=>({type:'event_msg',timestamp:time,payload:{type:'user_message',message:text}})
 for(const records of [
  [message('Continue','2026-10-24T10:00:00.000Z'),event('Continue','2026-10-24T10:00:00.005Z'),event('Continue','2026-10-24T10:01:00Z')],
  [event('Continue','2026-10-24T10:00:00Z'),message('Continue','2026-10-24T10:00:00Z'),event('Continue','2026-10-24T10:01:00Z')],
  [message('Continue','2026-10-24T10:00:00Z'),event('Continue','2026-10-24T10:10:00Z')],
  [message('Continue'),{type:'response_item',payload:{type:'message',role:'assistant',content:[{type:'output_text',text:'Done'}]}},event('Continue')],
 ]) {
  const bytes=encode(records.map(r=>JSON.stringify(r)).join('\n')+'\n')
  const view=createRecordFramer().push(0,bytes,true)
  assert.equal(view.rows.filter(row=>row.role==='user').length,2,'a matching canonical occurrence is never reused for a later turn')
 }
})


test('missing or invalid timestamps preserve ambiguous opposite envelope occurrences',()=>{
 for(const timestamp of [undefined,'invalid']){
  const records=[{type:'response_item',timestamp,payload:{type:'message',role:'user',content:[{type:'input_text',text:'Continue'}]}},{type:'event_msg',timestamp,payload:{type:'user_message',message:'Continue'}}]
  const bytes=encode(records.map(r=>JSON.stringify(r)).join('\n')+'\n')
  assert.equal(createRecordFramer().push(0,bytes,true).rows.length,2)
 }
})
