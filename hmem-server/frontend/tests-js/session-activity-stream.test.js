import test from 'node:test'
import assert from 'node:assert/strict'
import {createSessionActivityStream} from '../src/session-activity-stream.js'
const tick=()=>new Promise(resolve=>setImmediate(resolve))
const response=()=>({ok:true,json:async()=>({ticket:'70000000-0000-4000-8000-000000000001',expires_at:'2099-01-01T00:00:00Z'})})
function fixture(){
 const requests=[],sockets=[],frames=[],timers=new Map();let active=0,peak=0,next=1
 class Socket{constructor(url){this.url=url;sockets.push(this)}close(){this.closed=true;this.onclose?.()}emit(frame){this.onmessage?.({data:JSON.stringify(frame)})}}
 const stream=createSessionActivityStream({wsUrl:'ws://test/api/v1/ws',WebSocketImpl:Socket,fetchTicket:({signal})=>{active++;peak=Math.max(peak,active);return new Promise(resolve=>requests.push({signal,finish(){active--;resolve(response())}}))},onFrame:frame=>frames.push(frame),setTimer:callback=>{const id=next++;timers.set(id,callback);return id},clearTimer:id=>timers.delete(id)})
 return {stream,requests,sockets,frames,timers,peak:()=>peak}
}
test('activity-only tickets retain one physical admission across retirement and ignore old socket frames',async()=>{
 const h=fixture();h.stream.configure('old');h.stream.configure('new');assert.equal(h.requests.length,1);assert.equal(h.requests[0].signal.aborted,true)
 h.requests[0].finish();await tick();assert.equal(h.requests.length,2);h.requests[1].finish();await tick();assert.equal(h.sockets.length,1)
 const socket=h.sockets[0];socket.emit({schema_version:1,type:'session_activity_ready'});assert.deepEqual(h.frames,[{owner:'new',frame:{schema_version:1,type:'session_activity_ready'}}])
 h.stream.configure('third');assert.equal(socket.closed,true);socket.emit({schema_version:1,type:'session_activity_changed',workspace_id:'80000000-0000-4000-8000-000000000001'});assert.equal(h.frames.length,1)
 h.stream.close();h.requests[2].finish();await tick();assert.equal(h.sockets.length,1);assert.equal(h.peak(),1);assert.equal(h.timers.size,0)
})
test('malformed controls fail closed with one retry timer and hidden retirement cancels reconnect',async()=>{
 const h=fixture();h.stream.configure('owner');h.requests[0].finish();await tick();const socket=h.sockets[0]
 socket.emit({schema_version:1,type:'change',event:{private:'not an activity control'}});assert.equal(socket.closed,true);assert.equal(h.frames.length,0);assert.equal(h.timers.size,1)
 socket.onclose();assert.equal(h.timers.size,1);h.stream.close();assert.equal(h.timers.size,0);assert.equal(h.requests.length,1)
})
test('fixed capability expiry retires only the activity transport and reconnects for a new readiness baseline',async()=>{
 const h=fixture();h.stream.configure('owner');h.requests[0].finish();await tick();const socket=h.sockets[0]
 socket.emit({schema_version:1,type:'session_activity_ready'});socket.emit({schema_version:1,type:'session_activity_expired'});assert.equal(socket.closed,true);assert.equal(h.frames.length,1,'expiry must not become canonical or identity state');assert.equal(h.timers.size,1)
 const [id,retry]=[...h.timers][0];h.timers.delete(id);retry();h.requests[1].finish();await tick();h.sockets[1].emit({schema_version:1,type:'session_activity_ready'});assert.equal(h.frames.length,2);h.stream.close();assert.equal(h.timers.size,0)
})
test('ticket and handshake deadlines abort or close before a single bounded reconnect',async()=>{
 const h=fixture();h.stream.configure('owner');assert.equal(h.timers.size,1)
 const [id,timeout]=[...h.timers][0];h.timers.delete(id);timeout();assert.equal(h.requests[0].signal.aborted,true);assert.equal(h.requests.length,1,'abort never releases physical occupancy early')
 h.requests[0].finish();await tick();assert.equal(h.sockets.length,0,'late successful completion cannot revive an aborted ticket');assert.equal(h.timers.size,1)
 const [ticketRetryId,ticketRetry]=[...h.timers][0];h.timers.delete(ticketRetryId);ticketRetry();h.requests[1].finish();await tick();assert.equal(h.sockets.length,1)
 const [readyId,readyTimeout]=[...h.timers][0];h.timers.delete(readyId);readyTimeout();assert.equal(h.sockets[0].closed,true);assert.equal(h.timers.size,1)
 const [retryId,retry]=[...h.timers][0];h.timers.delete(retryId);retry();assert.equal(h.requests.length,3);assert.equal(h.peak(),1)
 h.stream.close();h.requests[2].finish();await tick();assert.equal(h.timers.size,0)
})
test('rapid owner and visibility changes retain a closing socket until its exact delayed callback',async()=>{
 const requests=[],sockets=[],frames=[]
 class Socket{constructor(){sockets.push(this)}close(){this.closing=true}finish(){this.onclose?.()}emit(frame){this.onmessage?.({data:JSON.stringify(frame)})}}
 const stream=createSessionActivityStream({wsUrl:'ws://test/api/v1/ws',WebSocketImpl:Socket,fetchTicket:()=>new Promise(resolve=>requests.push(()=>resolve(response()))),onFrame:value=>frames.push(value)})
 stream.configure('a');requests[0]();await tick();const old=sockets[0]
 old.emit({type:'session_activity_ready',schema_version:1});stream.configure('b');stream.configure(null);stream.configure('a');assert.equal(old.closing,true);assert.equal(requests.length,1);assert.equal(sockets.length,1)
 old.emit({type:'session_activity_changed',schema_version:1,workspace_id:'80000000-0000-4000-8000-000000000001'});assert.equal(frames.length,1)
 old.finish();assert.equal(requests.length,2);requests[1]();await tick();assert.equal(sockets.length,2)
 old.finish();assert.equal(requests.length,2,'duplicate old close cannot release the new socket')
 stream.close();assert.equal(sockets[1].closing,true);sockets[1].finish();assert.equal(requests.length,2)
})
test('a ready deadline retires same-owner messages before the delayed physical close',async()=>{
 const requests=[],sockets=[],frames=[],timers=new Map();let next=1
 class Socket{constructor(){sockets.push(this)}close(){this.closing=true}finish(){this.onclose?.()}emit(frame){this.onmessage?.({data:JSON.stringify(frame)})}}
 const stream=createSessionActivityStream({wsUrl:'ws://test/api/v1/ws',WebSocketImpl:Socket,fetchTicket:()=>new Promise(resolve=>requests.push(()=>resolve(response()))),onFrame:value=>frames.push(value),setTimer:callback=>{const id=next++;timers.set(id,callback);return id},clearTimer:id=>timers.delete(id)})
 stream.configure('owner');requests[0]();await tick();const old=sockets[0]
 const [id,deadline]=[...timers][0];timers.delete(id);deadline();assert.equal(old.closing,true)
 old.emit({schema_version:1,type:'session_activity_ready'});old.emit({schema_version:1,type:'session_activity_changed',workspace_id:'80000000-0000-4000-8000-000000000001'});assert.equal(frames.length,0);assert.equal(requests.length,1);assert.equal(timers.size,0,'late ready cannot establish a baseline or rearm the retired phase')
 old.finish();assert.equal(timers.size,1);const [retryId,retry]=[...timers][0];timers.delete(retryId);retry();requests[1]();await tick();sockets[1].emit({schema_version:1,type:'session_activity_ready'});assert.equal(frames.length,1)
 old.emit({schema_version:1,type:'session_activity_ready'});assert.equal(frames.length,1);stream.close();sockets[1].finish();assert.equal(timers.size,0)
})
