import assert from 'node:assert/strict'
import test from 'node:test'
import { EventEmitter } from 'node:events'
import { startSessionMcp, closeSessionMcps } from './support/session-mcp.js'

test('a rejected or synchronously throwing client cleanup never skips the other owners',async()=>{
  const attempted=[],failure=new Error('controlled cleanup failure')
  await assert.rejects(closeSessionMcps([
    {close(){attempted.push('first');throw failure}},
    {async close(){attempted.push('second');await new Promise(done=>setImmediate(done));attempted.push('joined')}},
    {close(){attempted.push('third');return Promise.reject(failure)}}
  ]),error=>error instanceof AggregateError&&error.errors.length===2)
  assert.deepEqual(attempted,['first','second','third','joined'])
})

test('repeated close shares the complete finite cleanup promise even after native join timeout',async()=>{
  const child=new EventEmitter();let kills=0,ends=0
  child.stdout=new EventEmitter();child.stderr=new EventEmitter();child.stderr.resume=()=>{}
  child.stdin={end(){ends++},write(line,callback){const request=JSON.parse(line);queueMicrotask(()=>child.stdout.emit('data',Buffer.from(JSON.stringify({jsonrpc:'2.0',id:request.id,result:{}})+'\n')));callback()}}
  child.kill=()=>{kills++;return true}
  const client=await startSessionMcp({executable:'controlled',origin:'http://127.0.0.1',token:'test-fixture',workspace:'w',identity:{},spawnProcess:()=>child,cleanupTimeoutMs:10,forcedCleanupTimeoutMs:10})
  const first=client.close(),second=client.close()
  assert.equal(first,second)
  await assert.rejects(first,/cleanup deadline/)
  const afterFailure=client.close()
  assert.equal(afterFailure,first)
  await assert.rejects(afterFailure,/cleanup deadline/)
  assert.equal(ends,1);assert.equal(kills,1)
  // No OS process or pipe was created by this controlled spawn seam.
})
