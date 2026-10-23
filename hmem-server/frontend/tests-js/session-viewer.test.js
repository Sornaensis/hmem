import assert from 'node:assert/strict'
import test from 'node:test'
import { createRecordFramer, decodeBase64, MAX_VIEW_BYTES, MAX_VIEW_ROWS } from '../src/session-viewer.js'
const encode = value => new TextEncoder().encode(value)

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
