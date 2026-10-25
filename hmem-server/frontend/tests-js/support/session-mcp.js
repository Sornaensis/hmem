import { spawn } from 'node:child_process'
import { once } from 'node:events'
import { StringDecoder } from 'node:string_decoder'

// Private, bounded stdio client for the freshly built production executable.
// Credentials remain in the child environment; stderr is drained, never echoed.
export async function startSessionMcp({executable,origin,token,workspace,identity,env=process.env,spawnProcess=spawn,cleanupTimeoutMs=15000,forcedCleanupTimeoutMs=10000,requestTimeoutMs=15000}) {
  if(!Number.isInteger(cleanupTimeoutMs)||cleanupTimeoutMs<1||cleanupTimeoutMs>15000||!Number.isInteger(forcedCleanupTimeoutMs)||forcedCleanupTimeoutMs<1||forcedCleanupTimeoutMs>10000)throw new Error('Invalid MCP cleanup bounds')
  if(!Number.isInteger(requestTimeoutMs)||requestTimeoutMs<1||requestTimeoutMs>120000)throw new Error('Invalid MCP observer request timeout')
  const child=spawnProcess(executable,['--server-url',origin],{env:{...env,HMEM_MCP_AUTH_TOKEN:token},windowsHide:true,stdio:['pipe','pipe','pipe']})
  const closed=once(child,'close').then(value=>value,()=>null),pending=new Map(),decoder=new StringDecoder('utf8');let buffer='',next=0,closing=false,cleanupPromise
  child.stderr.resume()
  const fail=code=>{for(const request of pending.values())request.reject(new Error(code));pending.clear()}
  child.stdout.on('data',bytes=>{
    buffer+=decoder.write(bytes)
    if(Buffer.byteLength(buffer)>3*1024*1024){fail('MCP response frame exceeds limit');child.kill();return}
    let end
    while((end=buffer.indexOf('\n'))!==-1){
      const line=buffer.slice(0,end);buffer=buffer.slice(end+1)
      let response
      try{response=JSON.parse(line)}catch{fail('Invalid MCP response frame');continue}
      const request=pending.get(String(response.id))
      if(request){pending.delete(String(response.id));response.error?request.reject(new Error('MCP protocol error')):request.resolve(response.result)}
    }
  })
  child.on('error',()=>fail('MCP process error'));child.on('close',()=>fail('MCP process closed'))
  async function rpc(method,params){
    if(closing||pending.size>=8)throw new Error('MCP client admission closed')
    const id=String(++next)
    return new Promise((resolve,reject)=>{
      const timer=setTimeout(()=>{pending.delete(id);reject(new Error('MCP request deadline'))},requestTimeoutMs)
      const finish=callback=>value=>{clearTimeout(timer);callback(value)}
      pending.set(id,{resolve:finish(resolve),reject:finish(reject)})
      child.stdin.write(JSON.stringify({jsonrpc:'2.0',id,method,params})+'\n',error=>{if(error){pending.delete(id);clearTimeout(timer);reject(new Error('MCP stdin failed'))}})
    })
  }
  function close({hard=false}={}){
    if(cleanupPromise)return cleanupPromise
    closing=true
    cleanupPromise=(async()=>{
    if(hard)child.kill();else child.stdin.end()
    async function join(ms){let timer;try{return await Promise.race([closed,new Promise((_,reject)=>{timer=setTimeout(()=>reject(new Error('MCP cleanup deadline')),ms)})])}finally{clearTimeout(timer)}}
    try{
      const result=await join(cleanupTimeoutMs)
      if(!result||(!hard&&result[0]!==0))throw new Error('MCP exited unsuccessfully')
      let alive=true
      try{process.kill(child.pid,0)}catch(error){if(error.code==='ESRCH')alive=false;else throw error}
      if(alive||!child.stdin.destroyed||!child.stdout.destroyed||!child.stderr.destroyed)throw new Error('MCP process or pipes remain after close')
    }
    catch(error){child.kill();await join(forcedCleanupTimeoutMs);throw error}
    finally{fail('MCP client closed')}
    })()
    return cleanupPromise
  }
  async function tool(name,args={}){
    const result=await rpc('tools/call',{name,arguments:args})
    if(result?.isError)throw new Error('MCP tool rejected: '+(result.structuredContent?.error?.code||'tool_error'))
    return result.structuredContent??result
  }
  try{
    await rpc('initialize',{})
    await tool('set_workspace',{workspace_id:workspace,identity})
    return {tool,close,pid:child.pid}
  }catch(error){await close();throw error}
}

export async function closeSessionMcps(clients){
  const results=await Promise.allSettled(clients.map(client=>Promise.resolve().then(()=>client.close())))
  const failures=results.filter(result=>result.status==='rejected').map(result=>result.reason)
  if(failures.length)throw new AggregateError(failures,'MCP client cleanup failed')
}
