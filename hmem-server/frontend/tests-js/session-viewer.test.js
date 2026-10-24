import assert from 'node:assert/strict'
import test from 'node:test'
import { createRecordFramer, decodeBase64, MAX_VIEW_BYTES, MAX_VIEW_ROWS, presentation, toolPayload } from '../src/session-viewer.js'
const encode = value => new TextEncoder().encode(value)

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
  const text = '{"text":"λ😀","type":"message","role":"assistant"}\r\n[1,"unknown"]\ntrue\nnull\n"scalar"'
  const bytes = encode(text), framer = createRecordFramer()
  for(let i=0;i<bytes.length;i++) framer.push(i,bytes.subarray(i,i+1),i===bytes.length-1)
  const view = framer.snapshot()
  assert.equal(view.rows.length,5); assert.equal(view.rows[0].text,'λ😀'); assert.equal(view.rows[0].delimiter,'\r\n')
  assert.deepEqual(view.rows.map(row => JSON.parse(row.raw)),[{text:'λ😀',type:'message',role:'assistant'},[1,'unknown'],true,null,'scalar'])
  assert.equal(view.through,bytes.length)
  const separator = encode('\r\n{"text":"next"}\n')
  const next = framer.push(bytes.length,separator,true)
  assert.equal(next.rows.length,6); assert.equal(next.rows[4].delimiter,'\r\n')
})

test('one 1MiB record survives more than four chunks while retained rows and bytes stay bounded', () => {
  const bytes = encode(JSON.stringify('x'.repeat(1048500))+'\n'), framer = createRecordFramer()
  for(let i=0;i<bytes.length;i+=65536) framer.push(i,bytes.subarray(i,i+65536),i+65536>=bytes.length)
  assert.equal(framer.snapshot().rows[0].raw.length,1048502)
  let offset = bytes.length
  for(let i=0;i<250;i++){ const chunk=encode(JSON.stringify({text:'small'+i})+'\n'); framer.push(offset,chunk,true); offset+=chunk.length }
  assert.ok(framer.snapshot().rows.length<=MAX_VIEW_ROWS); assert.ok(framer.snapshot().retainedBytes<=MAX_VIEW_BYTES)
})

test('finite mid-record windows omit leading fragments and preserve exact offsets without duplicate retries', () => {
  const framer=createRecordFramer(10), chunk=encode('fragment\r\n{"text":"kept"}\n{"partial":')
  const result=framer.push(10,chunk,true)
  assert.equal(result.rows.length,1); assert.equal(result.rows[0].offset,20); assert.ok(result.pendingBytes>0)
  assert.deepEqual(framer.push(10,chunk,true),result)
  assert.throws(()=>framer.push(0,chunk),/range_gap/)
})

test('rejects noncanonical base64 and overlarge records explicitly', () => {
  assert.deepEqual(decodeBase64('zrs='),encode('λ'))
  for(const value of ['Zh==','Zg','Zg==\n','____']) assert.throws(()=>decodeBase64(value),/invalid_base64/)
  const framer=createRecordFramer(), chunk=encode('x'.repeat(65536))
  for(let i=0;i<16;i++) framer.push(i*65536,chunk)
  assert.throws(()=>framer.push(1048576,chunk),/record_limit/)
})

test('finite boundary inside UTF-8 retains prior rows and completes exactly once later', () => {
  const bytes=encode('true\n{"text":"λ"}\n'), split=bytes.indexOf(0xce)+1, framer=createRecordFramer()
  const initial=framer.push(0,bytes.subarray(0,split),true)
  assert.equal(initial.rows.length,1); assert.match(initial.notice,/Incomplete UTF-8/)
  const final=framer.push(split,bytes.subarray(split),true)
  assert.equal(final.rows.length,2); assert.equal(final.rows[1].text,'λ')
  assert.equal(final.notice,'')
})

test('enforces exact payload cap independently of LF and CRLF delimiters', () => {
  for(const delimiter of ['\n','\r\n']) for(const length of [1048576,1048577]) {
    const bytes=encode(JSON.stringify('a'.repeat(length-2))+delimiter), framer=createRecordFramer()
    const run=()=>{ for(let i=0;i<bytes.length;i+=65536) framer.push(i,bytes.subarray(i,i+65536),i+65536>=bytes.length) }
    if(length===1048576) {run();assert.equal(framer.snapshot().rows[0].raw.length,length)}
    else assert.throws(run,/record_limit/)
  }
})

test('overlapping finite older windows make a maximum-sized boundary record reachable',()=>{
  const before=encode('0\n'.repeat(1000000)), record=encode(JSON.stringify('b'.repeat(1048574))+'\r\n'), after=encode('true\n')
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
