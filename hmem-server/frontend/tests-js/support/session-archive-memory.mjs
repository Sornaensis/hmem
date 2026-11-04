import assert from 'node:assert/strict'

function diagnosticCategories(values) {
  const counts=Object.create(null),maximum=64;let uncounted=0
  for(const value of values){const name=typeof value==='string'?value:'<absent>';if(name.length>128 || (!(name in counts)&&Object.keys(counts).length===maximum)){uncounted++;continue}counts[name]=(counts[name] || 0)+1}
  return {counts,uncounted}
}
export function attributedProcessTotals(events,requestGuid,actual,levelOfDetail) {
  // Perfetto ec4b996c export_json.cc emits an internal snapshot row ID, not
  // Chromium's CDP request GUID. Bind the single snapshot to the successful
  // explicit GlobalMemoryDump interval; never infer an ID offset or use other rows.
  const requests=events.filter(event=>event.name==='GlobalMemoryDump')
  assert.equal(requests.length,2,'one explicit global request interval required')
  const begin=requests.find(event=>event.ph==='b'),end=requests.find(event=>event.ph==='e')
  assert.ok(begin&&end,'global request begin and end required')
  assert.equal(begin.args?.dump_guid,requestGuid,'global request GUID must match CDP receipt')
  assert.equal(begin.args?.success,true,'global request must report success')
  assert.equal(begin.args?.dump_type,'explicitly_triggered','explicit global request required')
  assert.equal(begin.args?.level_of_detail,levelOfDetail,'global request detail must match')
  const identity=event=>({pid:event.pid,scope:event.scope,id:event.id,id2:event.id2})
  assert.ok(typeof begin.id==='string'||typeof begin.id2?.local==='string'||typeof begin.id2?.global==='string','global request trace identity required')
  assert.deepEqual(identity(end),identity(begin),'global request end must match begin identity')
  assert.ok(Number.isFinite(begin.ts)&&Number.isFinite(end.ts)&&begin.ts<=end.ts,'ordered global request timestamps required')
  const owned=actual.map(process=>process.id).sort((a,b)=>a-b)
  const processBegins=events.filter(event=>event.name==='ProcessMemoryDump'&&event.ph==='b')
  assert.deepEqual(processBegins.map(event=>event.pid).sort((a,b)=>a-b),owned,'each owned PID must begin the requested dump exactly once')
  assert.ok(processBegins.every(event=>event.args?.dump_guid===requestGuid&&Number.isFinite(event.ts)&&event.ts>=begin.ts&&event.ts<=end.ts),'process request GUIDs and timestamps must match global interval')
  const selected=events.filter(event=>event.ph==='v'&&event.args?.dumps?.process_totals)
  assert.ok(selected.length,'explicit request must provide process totals')
  const first=selected[0]
  assert.ok(typeof first.id==='string'&&first.id.length>0&&Number.isFinite(first.ts),'exported snapshot identity and timestamp required')
  assert.ok(selected.every(event=>event.id===first.id&&event.ts===first.ts&&event.args.dumps.level_of_detail===levelOfDetail),'one unambiguous exported snapshot and detail required')
  assert.ok(first.ts>=begin.ts&&first.ts<=end.ts,'exported snapshot must lie inside requested interval')
  assert.deepEqual(selected.map(event=>event.pid).sort((a,b)=>a-b),owned,'requested snapshot must cover each owned PID exactly once')
  const largestAllocators=event=>Object.entries(event.args.dumps.allocators||{})
    .map(([name,value])=>({name:name.slice(0,128),bytes:Number.parseInt(value.attrs?.size?.value,16)}))
    .filter(value=>Number.isSafeInteger(value.bytes)&&value.bytes>=1048576)
    .sort((a,b)=>b.bytes-a.bytes).slice(0,12)
  return {attribution:{basis:'explicit-global-request-span',requestGuid,exportRowId:first.id,snapshotTimestamp:first.ts,levelOfDetail,requestBegin:memoryEventMetadata(begin),requestEnd:memoryEventMetadata(end),processRequestPids:owned},processes:selected.map(event=>({pid:event.pid,totals:event.args.dumps.process_totals,allocatorNames:Object.keys(event.args.dumps.allocators || {}).filter(name=>/partition|v8|level|sqlite|file|malloc/.test(name)).slice(0,24),...(levelOfDetail==='detailed'?{largestAllocators:largestAllocators(event)}:{}),exportRowId:event.id}))}
}
function memoryEventMetadata(event) {
  const short=value=>typeof value==='string'?value.slice(0,128):typeof value==='number'||typeof value==='boolean'?value:undefined
  return {pid:event.pid,tid:event.tid,ph:short(event.ph),id:short(event.id),id2:event.id2?{local:short(event.id2.local),global:short(event.id2.global)}:undefined,scope:short(event.scope),s:short(event.s),name:short(event.name),ts:event.ts,dur:event.dur,argsKeys:Object.keys(event.args || {}).slice(0,16),dumpsKeys:Object.keys(event.args?.dumps || {}).slice(0,16),levelOfDetail:short(event.args?.dumps?.level_of_detail),dumpGuid:short(event.args?.dump_guid),success:short(event.args?.success),dumpType:short(event.args?.dump_type),requestLevelOfDetail:short(event.args?.level_of_detail)}
}

export function privateFootprint(processes) {
  assert.ok(processes.length>0,'owned process footprint samples required')
  let total=0
  for(const process of processes){
    const value=process.totals?.private_footprint_bytes
    assert.ok(typeof value==='string'&&/^[a-f0-9]+$/i.test(value),'complete nonnegative hex private footprint required')
    const bytes=Number.parseInt(value,16)
    assert.ok(Number.isSafeInteger(bytes)&&bytes>=0,'safe process private footprint required')
    process.privateBytes=bytes
    total+=bytes
    assert.ok(Number.isSafeInteger(total),'safe total private footprint required')
  }
  assert.ok(total>0,'owned process private footprint must be positive')
  return total
}

export async function memorySnapshot(page,{levelOfDetail='detailed'}={}) {
  assert.ok(['background','light','detailed'].includes(levelOfDetail))
  const cdp=await page.context().browser().newBrowserCDPSession()
  let handle,text='',observedCharacters=0,observedUtf8Bytes=0,dumpGuid,traceLoss,observedDumpGuids=[],memoryMetadata
  try {
    await cdp.send('Tracing.start',{transferMode:'ReturnAsStream',traceConfig:{recordMode:'recordUntilFull',includedCategories:['disabled-by-default-memory-infra'],memoryDumpConfig:{triggers:[]}}})
    const dump=await cdp.send('Tracing.requestMemoryDump',{deterministic:false,levelOfDetail});dumpGuid=dump.dumpGuid
    assert.equal(dump.success,true,'native memory dump must be supported')
    const complete=new Promise(resolve=>cdp.once('Tracing.tracingComplete',resolve))
    await cdp.send('Tracing.end');const receipt=await complete;traceLoss=receipt.dataLossOccurred;assert.equal(receipt.dataLossOccurred,false,'native memory trace must explicitly report no data loss');handle=receipt.stream
    while(true){const part=await cdp.send('IO.read',{handle,size:65536});assert.notEqual(part.base64Encoded,true,'plain JSON diagnostic stream required');observedCharacters+=part.data.length;observedUtf8Bytes+=Buffer.byteLength(part.data);assert.ok(observedCharacters<=16777216,'bounded diagnostic trace');text+=part.data;if(part.eof)break}
    await cdp.send('IO.close',{handle});handle=null
    const events=JSON.parse(text).traceEvents
    observedDumpGuids=[...new Set(events.filter(event=>event.ph==='v'&&event.args?.dumps?.process_totals).map(event=>String(event.id).slice(0,128)))].slice(0,16)
    const actual=await cdp.send('SystemInfo.getProcessInfo'),targets=await cdp.send('Target.getTargets')
    memoryMetadata={ownedProcesses:actual.processInfo.map(value=>({pid:value.id,type:value.type})).slice(0,16),totals:events.filter(event=>event.ph==='v'&&event.args?.dumps?.process_totals).slice(0,16).map(memoryEventMetadata),requests:events.filter(event=>event.name==='GlobalMemoryDump'||event.name==='ProcessMemoryDump').slice(0,16).map(memoryEventMetadata)}
    const {processes,attribution}=attributedProcessTotals(events,dump.dumpGuid,actual.processInfo,levelOfDetail)
    for(const process of processes){process.type=actual.processInfo.find(value=>value.id===process.pid)?.type || 'unknown';process.name=events.find(value=>value.ph==='M'&&value.pid===process.pid&&value.name==='process_name')?.args?.name || ''}
    const missing=actual.processInfo.filter(value=>!processes.some(process=>process.pid===value.id))
    assert.equal(missing.length,0,'every browser-owned native process needs a memory sample: '+JSON.stringify(missing))
    assert.ok(processes.every(process=>process.type!=='unknown'),'native process identity required')
    const totalPrivateBytes=privateFootprint(processes)
    const browserVersion=await cdp.send('Browser.getVersion')
    return {requestGuid:dump.dumpGuid,exportRowId:attribution.exportRowId,attribution,traceLoss:!!receipt.dataLossOccurred,trace:{levelOfDetail,characters:observedCharacters,utf8Bytes:observedUtf8Bytes,categories:diagnosticCategories(events.map(event=>event.cat))},processes,browserVersion,workerTargets:targets.targetInfos.filter(target=>target.type==='worker').map(target=>({id:target.targetId,type:target.type,url:target.url})),totalPrivateBytes}
  }catch(error){
    error.diagnostic={levelOfDetail,deterministic:false,unit:'decoded UTF16 characters',capCharacters:16777216,requestedReadBytes:65536,observedCharacters,observedUtf8Bytes,dumpGuid,observedDumpGuids,traceLoss,memoryMetadata,categoriesInRetainedPrefix:diagnosticCategories((function*(){for(const match of text.matchAll(/"cat"\s*:\s*"([^"\\]*)"/g))yield match[1]})())}
    throw error
  }finally{if(handle)await cdp.send('IO.close',{handle});await cdp.detach()}
}
