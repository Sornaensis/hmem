import test from 'node:test'
import assert from 'node:assert/strict'
import {createEventPayloadPager} from '../src/session-event-json.js'

test('event JSON continuation preserves exact values, inert markup and surrogate boundaries',()=>{
  for(const value of [{type:'future_event',input:{html:'<img src=x onerror=alert(1)>',values:[null,false,42,'λ😀']}},'literal JSON string',[],null]){
    const pager=createEventPayloadPager(JSON.stringify(value)),parts=[]
    for(let index=0;;index++){const body=pager.read(index);parts.push(body.text);assert.equal(body.tokens.map(token=>token.text).join(''),body.text);if(!body.more)break}
    assert.equal(parts.join(''),JSON.stringify(value,null,2));assert.deepEqual(JSON.parse(parts.join('')),value)
  }
  const value={text:'x'.repeat(16370)+'😀λend'},pager=createEventPayloadPager(JSON.stringify(value)),first=pager.read(0),second=pager.read(1)
  assert.equal(first.more,true);assert.equal(Buffer.from(first.text).toString(),first.text);assert.equal(first.text+second.text,JSON.stringify(value,null,2))
  assert.equal(pager.read(0),first);assert.ok(pager.retainedParts<=2)
})

test('deep event payloads keep formatting bounded and retain at most two parts',()=>{
  let value=Array(100000).fill('x');for(let depth=0;depth<1000;depth++)value=[value]
  const source=JSON.stringify(value);assert.ok(source.length<1048576)
  const pager=createEventPayloadPager(source)
  for(let index=0;index<8;index++){const body=pager.read(index);assert.ok(body.text.length<=16384);assert.equal(body.more,true);assert.ok(pager.retainedParts<=2)}
  assert.equal(pager.read(7).part,7);assert.equal(pager.read(6).part,6);assert.equal(pager.read(2).part,2)
  assert.throws(()=>pager.read(-1),/invalid_body_part/)
})

test('raw payload paging preserves number spellings, string escapes and duplicate keys',()=>{
 const source='{"n":9007199254740993,"overflow":1e400,"underflow":1e-400,"negative":-0,"escaped":"\\u0061","duplicate":1,"duplicate":2,"padding":"'+'x'.repeat(19000)+'"}'
 const pager=createEventPayloadPager(source),parts=[]
 for(let index=0;;index++){const body=pager.read(index);assert.ok(body.text.length<=16384);parts.push(body.text);if(!body.more)break}
 const result=parts.join('')
 for(const token of ['9007199254740993','1e400','1e-400','-0','\\u0061','"duplicate": 1','"duplicate": 2'])assert.ok(result.includes(token),token)
 assert.equal(pager.read(0).text,parts[0]);assert.equal(pager.read(1).text,parts[1]);assert.ok(pager.retainedParts<=2)
 const deep=createEventPayloadPager('['.repeat(10000)+'0'+']'.repeat(10000))
 for(let part=0;part<4;part++){const body=deep.read(part);assert.equal(body.text.length,16384);assert.equal(body.more,true)}
})
