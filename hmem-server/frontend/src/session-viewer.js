import {parseInstant} from './timestamp-display.js'

// Frame exact bytes before decoding UTF-8. A raw window and a JSONL record
// have separate bounds: one record may span more than four HTTP chunks.
export const MAX_RECORD_BYTES = 1048576
export const MAX_OCCURRENCE_ROWS = 100
export const MAX_OCCURRENCE_BYTES = 2097152
export const MAX_VIEW_ROWS = MAX_OCCURRENCE_ROWS * 2
export const MAX_VIEW_BYTES = MAX_OCCURRENCE_BYTES * 2
const decoder = new TextDecoder('utf-8', { fatal: true })
const equal = (a,b) => a.length === b.length && a.every((byte,index) => byte === b[index])

export function decodeBase64(value) {
  if (typeof value !== 'string' || value.length > 87384 || value.length % 4 || !/^(?:[A-Za-z0-9+/]{4})*(?:[A-Za-z0-9+/]{2}==|[A-Za-z0-9+/]{3}=)?$/.test(value)) throw new Error('invalid_base64')
  const binary = atob(value)
  const bytes = Uint8Array.from(binary, character => character.charCodeAt(0))
  if (btoa(binary) !== value) throw new Error('invalid_base64')
  return bytes
}

const string = value => typeof value === 'string' ? value : ''
const printable = value => typeof value === 'string' ? value : JSON.stringify(value, null, 2) || ''
const short = value => string(value).replace(/^mcp__/, '').replaceAll('__', ' · ').replace(/\s+/g,' ').slice(0,96)
const archiveBlocks=Symbol('archiveBlocks'),archiveVisible=Symbol('archiveVisible')
const blocksFor=row=>row[archiveBlocks] || JSON.parse(row.text)

const jsonSpace=character=>character===' ' || character==='\n' || character==='\r' || character==='\t'
const skipJsonSpace=(source,start)=>{while(jsonSpace(source[start]))start++;return start}
function jsonStringEnd(source,start) {
  for(let at=start+1;at<source.length;at++) {
    if(source[at]==='\\')at++
    else if(source[at]==='"')return at+1
  }
  throw Error('invalid_json_record')
}
function jsonValueEnd(source,start) {
  if(source[start]==='"')return jsonStringEnd(source,start)
  if(source[start]==='{' || source[start]==='[') {
    let depth=1,at=start+1
    while(depth && at<source.length) {
      if(source[at]==='"'){at=jsonStringEnd(source,at);continue}
      if(source[at]==='{' || source[at]==='[')depth++
      else if(source[at]==='}' || source[at]===']')depth--
      at++
    }
    if(depth)throw Error('invalid_json_record')
    return at
  }
  let at=start
  while(at<source.length && !jsonSpace(source[at]) && !',}]'.includes(source[at]))at++
  return at
}

// The record has already passed JSON validation. Locate the effective root
// payload without serializing its parsed Numbers or recursing through its tree.
// Escaped property names and duplicate root keys follow JSON.parse's last key.
function rawEventPayload(source) {
  if(typeof source!=='string')return null
  let at=skipJsonSpace(source,0),payload=null
  if(source[at]!=='{')return null
  at=skipJsonSpace(source,at+1)
  while(at<source.length && source[at]!=='}') {
    const keyEnd=jsonStringEnd(source,at),key=JSON.parse(source.slice(at,keyEnd))
    const begin=skipJsonSpace(source,skipJsonSpace(source,keyEnd)+1),end=jsonValueEnd(source,begin)
    if(key==='payload')payload=source.slice(begin,end)
    at=skipJsonSpace(source,end)
    if(source[at]!==',')break
    at=skipJsonSpace(source,at+1)
  }
  return payload
}

// Direct presentation callers may lack original text. Keep that fallback
// iterative too; production events always use their lossless raw payload span.
function compactPayload(value) {
  const stack=[{value}],parts=[]
  while(stack.length) {
    const item=stack.pop()
    if(item.literal!==undefined){parts.push(item.literal);continue}
    const value=item.value
    if(value && typeof value==='object') {
      const array=Array.isArray(value),keys=Object.keys(value)
      parts.push(array?'[':'{');stack.push({literal:array?']':'}'})
      for(let index=keys.length-1;index>=0;index--) {
        const key=keys[index]
        stack.push({value:value[key]})
        if(!array)stack.push({literal:JSON.stringify(key)+':'})
        if(index)stack.push({literal:','})
      }
    }else parts.push(JSON.stringify(value) ?? 'null')
  }
  return parts.join('')
}

// Unknown marker-only metadata has no conversational content. Inspect a finite
// number of values; ambiguous large/deep structures get a notice, never a dump.
function hasMeaningfulContent(value) {
  const pending=[{value,depth:0}],discriminators=new Set(['type','id','parentid','sessionid','messageid','toolcallid','timestamp','time','version'])
  let budget=256
  while(pending.length) {
    if(--budget<0)return true
    const {value,depth}=pending.pop()
    if(typeof value==='string'){if(value.trim())return true;continue}
    if(typeof value==='number')return true
    if(!value || typeof value!=='object')continue
    if(depth===16)return true
    if(Array.isArray(value)){if(value.length)return true;continue}
    const keys=Object.keys(value)
    if(keys.length>budget)return true
    for(const key of keys) {
      if(discriminators.has(key.toLowerCase()))continue
      if(/(?:content|message|text|output|result|summary|error|warning|answer)$|^(?:arguments|input|toolrequests|thinking|reasoningtext)$/i.test(key))return true
      pending.push({value:value[key],depth:depth+1})
    }
  }
  return false
}

export function toolPayload(value, field = 'input') {
  let parsed = value
  if (typeof value === 'string') { try { parsed = JSON.parse(value) } catch { parsed = {[field]:value} } }
  if (parsed === undefined) parsed = null
  const text = JSON.stringify(parsed, null, 2)
  return {text,tokens:payloadTokens(text)}
}

export function payloadTokens(text) {
  const tokens = []
  let through = 0
  for (const match of text.matchAll(/"(?:\\.|[^"\\])*"\s*:|"(?:\\.|[^"\\])*"|-?\d+(?:\.\d+)?(?:[eE][+-]?\d+)?|\b(?:true|false|null)\b|[^"\w-]+/g)) {
    if (match.index !== through) return [{text,kind:'punctuation'}]
    if (tokens.length === 2048) { tokens.push({text:text.slice(through),kind:'punctuation'}); return tokens }
    const [value] = match
    tokens.push({text:value,kind:value.trim().endsWith(':')?'key':value.startsWith('"')?'string':/^-?\d/.test(value)?'number':/^(true|false|null)$/.test(value)?'literal':'punctuation'})
    through = match.index + value.length
  }
  if (through !== text.length) return [{text,kind:'punctuation'}]
  return tokens
}

// Presentation never changes the stored record or its byte boundaries.
export function presentation(value, raw, {archive=false,piVersion=null}={}) {
  const object = value && typeof value === 'object' && !Array.isArray(value) ? value : {}
  const sourceTime = string(object.timestamp || object.time)
  const time = parseInstant(sourceTime) ? sourceTime : ''
  const block = (kind, text, label = '') => ({kind, text:string(text), label:string(label).slice(0,256)})
  // A bounded indication preserves deliberate metadata hiding without making
  // unsupported meaningful records look like an empty conversation.
  const unsupported = type => block('text',`Unsupported transcript content (${short(type) || 'unrecognized record'}). Original evidence is preserved.`)
  const tool = (kind, value, name = '') => {
    const field=kind === 'result'?'output':'input'
    let payload
    if(archive) {
      let parsed=value
      if(typeof value==='string'){try{parsed=JSON.parse(value)}catch{parsed={[field]:value}}}
      payload={text:JSON.stringify(parsed ?? null),tokens:[]}
    }else payload=toolPayload(value,field)
    const first = (archive ? prettyPayloadSlice(JSON.parse(payload.text)).text : payload.text).split('\n').map(line=>line.trim()).find(line=>line.includes(':')) || ''
    return {...block(kind,payload.text,`${kind === 'result'?'Result':'Call'}${name?' · '+short(name):''}${first?' · '+short(first.replace(/,$/,'')):''}`),tokens:payload.tokens}
  }
  const contentBlocks = content => (Array.isArray(content) ? content : [{type:typeof content === 'string'?'text':'unknown',text:content}]).flatMap(part => {
    if (!part || typeof part !== 'object' || Array.isArray(part)) return [unsupported('content block')]
    if (['text','input_text','output_text'].includes(part.type)) return typeof part.text === 'string' ? [block('text',part.text)] : [unsupported(part.type)]
    if (['redacted_thinking','encrypted_thinking'].includes(part.type) || (['thinking','reasoning'].includes(part.type) && (part.encrypted_content || part.encrypted || part.redacted===true))) return [block('encrypted','','Thinking...')]
    if (['thinking','reasoning','summary_text'].includes(part.type)) return [block('thinking',part.thinking || part.text,'Thinking')]
    if (part.type === 'tool_use') return [tool('tool',part.input,part.name)]
    if (part.type === 'toolCall') return [tool('tool',part.argumentsError?{notice:'Malformed tool arguments',error:part.argumentsError}:part.arguments,`${short(part.name)} · ${short(part.id)}`)]
    if (part.type === 'tool_result') return [tool('result',part.content,part.is_error ? 'error' : part.tool_use_id)]
    return [unsupported(part.type || 'attachment / content')]
  })
  const conversation = (role,blocks,extra={}) => {
    const row={type:'conversation',role:string(role).slice(0,256)||'assistant',time,text:JSON.stringify(blocks),...extra}
    // Archive indexing keeps one bounded row and its adjacent occurrence. These
    // private refs avoid reparsing normalized blocks and never enter JSON/IDB.
    if(archive)Object.defineProperty(row,archiveBlocks,{value:blocks})
    return row
  }
  const hidden=()=>({type:'unknown',role:'',time,text:''})
  if(object.ephemeral===true || ['assistant.message_delta','assistant.reasoning_delta','assistant.streaming_delta','tool.execution_partial_result','tool.execution_progress'].includes(object.type))return hidden()
  if(object.type==='session')return object.version===3?hidden():conversation('notice',[unsupported(`pi session version ${short(String(object.version))}`)],{piVersion:Number.isInteger(object.version)?object.version:'invalid'})
  const piTypes=['message','compaction','branch_summary','custom_message','model_change','thinking_level_change','custom','label','context_edit','session_info','usage']
  const piEntry=typeof object.id==='string' && Object.hasOwn(object,'parentId') && (piTypes.includes(object.type) || (!object.data && !string(object.type).includes('.')))
  if(piEntry || (object.type==='message' && object.message)) {
    if(['model_change','thinking_level_change','custom','label','session_info','usage'].includes(object.type))return hidden()
    if(object.type==='custom_message' && object.display!==true)return hidden()
    const extra={piVersion}
    const native=(role,blocks)=>{
      const relation=object.parentId===null?`root ${short(object.id)}`:`entry ${short(object.id)} → parent ${short(object.parentId) || 'unavailable'}`
      const context={...block('text',`pi history: ${relation}. Source order may include alternate branches; ancestry is not expanded and parent context may be outside this window.${piVersion===null?' Schema version is not established in this window.':''}`),nativeContext:true}
      return conversation(role,[context,...blocks],extra)
    }
    if(piVersion!==null && piVersion!==3)return native('notice',[unsupported(`pi session version ${short(String(piVersion))}`)])
    if(!piEntry || (object.parentId!==null && typeof object.parentId!=='string'))return native('notice',[unsupported('pi entry relationship')])
    if(object.type==='message') {
      const message=object.message || {},role=message.role
      if(['user','assistant','system'].includes(role))return native(role,contentBlocks(message.content))
      if(role==='toolResult')return native('tool',[tool('result',message.content,`${message.isError?'error · ':''}${short(message.toolName)} · ${short(message.toolCallId)}`)])
      if(role==='custom')return message.display===true?native('assistant',contentBlocks(message.content)):hidden()
      if(['branchSummary','compactionSummary'].includes(role))return native('notice',[block('text',`${role==='branchSummary'?'Branch summary':'Compaction summary'}: ${string(message.summary)}`)])
      if(role==='bashExecution')return native('tool',[tool('tool',{command:message.command},'bash'),tool('result',{output:message.output,exitCode:message.exitCode,cancelled:message.cancelled,truncated:message.truncated},'bash')])
      return native('notice',[unsupported(`pi message role ${short(role)}`)])
    }
    if(object.type==='custom_message')return native('assistant',contentBlocks(object.content))
    if(['compaction','branch_summary'].includes(object.type))return native('notice',[block('text',`${object.type==='compaction'?'Compaction summary':'Branch summary'}${object.type==='compaction'?' · first kept '+short(object.firstKeptEntryId):' · from '+short(object.fromId)}: ${string(object.summary)}`)])
    if(object.type==='context_edit')return native('notice',[block('text','Native context edit recorded. This source-order history preserves the original entries; the edit is not applied to the displayed transcript.')])
    return native('notice',[unsupported(`pi entry ${short(object.type)}`)])
  }
  // Copilot persisted session events use data, not a Codex payload envelope.
  // Requests and execution starts are distinct source events, explicitly named
  // and correlated rather than guessing that every request was executed.
  if (object.data && typeof object.data === 'object' && !Array.isArray(object.data)) {
    const data=object.data
    const nativeTool=(kind,value,name,id,phase)=>{
      const result=tool(kind,value,name)
      result.label=`${phase}${name?' · '+short(name):''}${id?' · '+short(id):''}`.slice(0,256)
      return result
    }
    if(object.type==='user.message') {
      const blocks=typeof data.content==='string'?[block('text',data.content)]:[unsupported(object.type)]
      if(Array.isArray(data.attachments) && data.attachments.length)blocks.push(unsupported(`Copilot attachments · ${data.attachments.length}`))
      else if(data.attachments && !Array.isArray(data.attachments))blocks.push(unsupported('Copilot attachments'))
      return conversation('user',blocks)
    }
    if(object.type==='assistant.message') {
      const blocks=[]
      if(typeof data.reasoningText==='string' && data.reasoningText)blocks.push(block('thinking',data.reasoningText,'Message thinking'))
      if(typeof data.content==='string' && data.content)blocks.push(block('text',data.content))
      if(Array.isArray(data.toolRequests))for(const request of data.toolRequests) {
        if(request && typeof request==='object')blocks.push(nativeTool('tool',request.arguments,request.name,request.toolCallId,'Request'))
        else blocks.push(unsupported('tool request'))
      }
      return conversation('assistant',blocks.length?blocks:[unsupported(object.type)])
    }
    if(object.type==='tool.execution_start')return conversation('assistant',[nativeTool('tool',data.arguments,data.toolName,data.toolCallId,'Execution')])
    if(object.type==='tool.execution_complete')return conversation('tool',[nativeTool('result',data.success===false?{error:data.error ?? data.result ?? 'No error detail recorded'}:data.result ?? {notice:'No result recorded'},'',data.toolCallId,data.success===false?'Error result':'Result')])
    if(object.type==='assistant.reasoning')return conversation('assistant',typeof data.content==='string'?[block('thinking',data.content,`Reasoning${data.reasoningId?' · '+short(data.reasoningId):''}`)]:[unsupported(object.type)])
  }
  if(['user.message','assistant.message','assistant.reasoning','tool.execution_start','tool.execution_complete'].includes(object.type))return conversation('notice',[unsupported(object.type)])
  if (object.type === 'response_item') {
    const item=object.payload || {}
    if(item.type === 'message') return conversation(item.role || 'assistant',contentBlocks(item.content))
    if(['function_call','custom_tool_call'].includes(item.type)) return conversation('assistant',[tool('tool',item.arguments ?? item.input,item.name)])
    if(['function_call_output','custom_tool_call_output'].includes(item.type)) return conversation('tool',[tool('result',item.output,item.call_id)])
    if(item.type === 'reasoning') return conversation('assistant',[item.encrypted_content ? block('encrypted','','Thinking...')
      : archive && !Array.isArray(item.summary) && typeof item.summary!=='string' ? {...block('thinking',JSON.stringify(item.summary) || '','Thinking'),pretty:true}
      : block('thinking', (Array.isArray(item.summary) ? item.summary.map(part=>part?.text || '').join('\n') : printable(item.summary)),'Thinking')])
  }
  if (['assistant','user'].includes(object.type) && object.message) {
    const message=object.message
    return conversation(message.role || object.type,typeof message.content === 'string' ? [block('text',message.content)] : contentBlocks(message.content))
  }
  if(object.type === 'event_msg' && ['agent_message','user_message','agent_reasoning'].includes(object.payload?.type)) {
    const payload=object.payload,thinking=payload.type==='agent_reasoning'
    return conversation(payload.type==='user_message'?'user':'assistant',[block(thinking?'thinking':'text',payload.message || payload.text)],{eventEnvelope:true})
  }
  if(object.type==='event_msg'){
    if(['token_count','turn_aborted','task_started','task_complete'].includes(object.payload?.type) || !hasMeaningfulContent(object))return hidden()
    return conversation('assistant',[block('event',rawEventPayload(raw) ?? compactPayload(object.payload ?? null),`Event${object.payload?.type?' · '+short(object.payload.type):''}`)])
  }
  if(typeof object.text === 'string' || typeof object.content === 'string') return {type:string(object.type),role:string(object.role),time,text:typeof object.text === 'string' ? object.text : object.content}
  const metadata=['metadata','session_meta','turn_context','session.start','session.shutdown','session.end','session.idle','session.usage_info','session.model_change','session.mode_changed','session.title_changed','assistant.turn_start','assistant.turn_end','assistant.usage',
    'session.resume','session.context_changed','session.info','session.plan_changed','session.workspace_file_changed','session.compaction_start','pending_messages.modified','command.queued','permission.requested','permission.completed','hook.start','skill.invoked','subagent.started','subagent.completed','subagent.selected','subagent.deselected','exit_plan_mode.completed']
  // Keep failure/control changes that could explain missing output visible.
  if(['hook.end','command.completed','permission.completed'].includes(object.type) && object.data?.success===false)return conversation('notice',[block('text',`Transcript operation failed (${short(object.type)}). Original evidence is preserved.`)])
  if(['hook.end','command.completed'].includes(object.type) && !object.data?.error && object.data?.success!==false)return hidden()
  const envelope=typeof object.type==='string' || ['payload','data','message','content'].some(key=>Object.hasOwn(object,key))
  if((!metadata.includes(object.type) || object.data?.error || object.data?.success===false) && envelope && hasMeaningfulContent(object))return conversation('notice',[unsupported(object.type)])
  return {type:'unknown',role:'',time,text:''}
}

// JSON.stringify(value,null,2) as bounded output pieces. A valid small deeply
// nested value can have hundreds of MiB of indentation; never form that string.
export function createPrettyPayloadReader(value,limit=16384) {
  function* chunks() {
    const stack=[{value,depth:0}]
    while(stack.length) {
      const item=stack.pop()
      if(item.keys) {
        if(item.index===item.keys.length){yield '\n'+' '.repeat(item.depth*2)+(item.array?']':'}');continue}
        yield (item.index?',\n':'\n')+' '.repeat((item.depth+1)*2)
        const key=item.keys[item.index++]
        if(!item.array)yield JSON.stringify(key)+': '
        stack.push(item,{value:item.value[key],depth:item.depth+1})
      }else if(item.value && typeof item.value==='object') {
        const array=Array.isArray(item.value),keys=Object.keys(item.value)
        yield array?'[':'{'
        if(keys.length)stack.push({...item,keys,index:0,array});else yield array?']':'}'
      }else yield JSON.stringify(item.value)
    }
  }
  return payloadPartReader(chunks(),limit)
}

// Format validated source tokens, preserving numeric precision, exponents,
// duplicate keys and string escapes. Neither parsing nor output uses recursion;
// even indentation is generated in finite pieces for deeply nested evidence.
export function createRawPayloadReader(source,limit=16384) {
  JSON.parse(source)
  function* chunks() {
    let at=0,depth=0,previous=''
    function* indent(){for(let left=depth*2;left>0;left-=8192)yield ' '.repeat(Math.min(8192,left))}
    while((at=skipJsonSpace(source,at))<source.length) {
      const begin=at,character=source[at]
      if(character==='"')at=jsonStringEnd(source,at)
      else if('{}[],:'.includes(character))at++
      else {while(at<source.length && !jsonSpace(source[at]) && !'{}[],:'.includes(source[at]))at++}
      const token=source.slice(begin,at)
      if(character==='{' || character==='[') {
        yield token;depth++
        const next=source[skipJsonSpace(source,at)]
        if(next!=='}' && next!==']'){yield '\n';yield* indent()}
      }else if(character==='}' || character===']') {
        depth--
        if(previous!=='{' && previous!=='['){yield '\n';yield* indent()}
        yield token
      }else if(character===','){yield ',\n';yield* indent()}
      else if(character===':')yield ': '
      else yield token
      previous=character
    }
  }
  return payloadPartReader(chunks(),limit)
}

function payloadPartReader(iterator,limit) {
  let pending='',at=0,done=false,part=0
  function advance(){while(at===pending.length&&!done){const next=iterator.next();done=!!next.done;pending=next.value || '';at=0}}
  return {read(){
    const parts=[];let size=0
    advance()
    while(!done && size<limit) {
      let end=Math.min(pending.length,at+limit-size)
      if(end<pending.length && /[\uD800-\uDBFF]/.test(pending[end-1]) && /[\uDC00-\uDFFF]/.test(pending[end]))end--
      if(end===at)break
      parts.push(pending.slice(at,end));size+=end-at;at=end
      advance()
    }
    return {text:parts.join(''),part:part++,more:!done}
  }}
}

export function prettyPayloadSlice(value,part=0,limit=16384) {
  if(!Number.isSafeInteger(part)||part<0)throw Error('invalid_body_part')
  const reader=createPrettyPayloadReader(value,limit);let result
  for(let index=0;index<=part;index++)result=reader.read()
  return result
}

export function visibleBlocks(row) {
  if (row.type === 'unknown') return []
  if (row.type !== 'conversation') return row.text.trim() ? [[0,{kind:'text',text:row.text,label:''}]] : []
  if(row[archiveVisible])return row[archiveVisible]
  const visible=blocksFor(row).map((part,index)=>[index,part]).filter(([,part])=>
    part.kind === 'encrypted' || (['text','thinking'].includes(part.kind) && part.text.trim())
    || (['tool','result','event'].includes(part.kind) && (part.text.trim() || part.label.trim())))
  if(row[archiveBlocks])Object.defineProperty(row,archiveVisible,{value:visible})
  return visible
}

export function pairOccurrence(previous,row) {
  if (!previous || previous.type !== 'conversation' || row.type !== 'conversation') return false
  const sameText=(first,second)=>{
    if(!first.pretty && !second.pretty)return first.text===second.text
    if(first.pretty && second.pretty)return first.text===second.text
    const deferred=first.pretty?first:second,plain=first.pretty?second:first
    if(!deferred.text)return !plain.text
    const reader=createPrettyPayloadReader(JSON.parse(deferred.text));let at=0
    while(true){const part=reader.read();if(plain.text.slice(at,at+part.text.length)!==part.text)return false;at+=part.text.length
      if(at>plain.text.length)return false;if(!part.more)return at===plain.text.length}
  }
  const sameContent=()=>{
    if(previous.role!==row.role)return false
    const before=blocksFor(previous),after=blocksFor(row)
    return before.length===after.length && before.every((block,index)=>block.kind===after[index].kind && sameText(block,after[index]))
  }
  const paired = Boolean(previous.eventEnvelope)!==Boolean(row.eventEnvelope)
    && (previous.pairedOffset === undefined || previous.pairedOffset === row.offset)
    && (row.pairedOffset === undefined || row.pairedOffset === previous.offset)
    && sameContent()
  const firstTime=Date.parse(previous.time),lastTime=Date.parse(row.time)
  if(paired && Number.isFinite(firstTime) && Number.isFinite(lastTime) && Math.abs(firstTime-lastTime)<=100) {
    const canonical = row.eventEnvelope ? previous : row
    canonical.presentationOffset = (typeof row.presentationOffset === 'string'
      ? BigInt(previous.presentationOffset) < BigInt(row.presentationOffset)
      : previous.presentationOffset < row.presentationOffset) ? previous.presentationOffset : row.presentationOffset
    previous.pairedOffset = row.offset; row.pairedOffset = previous.offset
    return true
  }
  return false
}

// Both adapters use source adjacency, including invisible metadata, and retain
// one previous occurrence plus the current activity anchor.
export function semanticTransition(row, previous, anchor) {
  const paired = pairOccurrence(previous,row)
  if (paired && !row.eventEnvelope) row.activityAnchors = previous.activityAnchors
  const visible = visibleBlocks(row)
  if (!row.activityAnchors) {
    row.activityAnchors = {}
    for (const [index,part] of visible) {
      if (part.nativeContext) continue
      if (part.kind === 'text') anchor = null
      else {
        anchor ??= `${row.presentationOffset}:${index}`
        row.activityAnchors[index] = anchor
      }
    }
  }
  row.category = visible.some(([,part])=>part.kind === 'text' && !part.nativeContext) ? 'message' : visible.length ? 'activity' : null
  return {row, previous:row.type === 'conversation' ? row : null, anchor, paired,
    retired:paired && previous.eventEnvelope ? previous.offset : null,
    hidden:row.eventEnvelope && row.pairedOffset !== undefined}
}

export function createRecordFramer(startOffset = 0) {
  if (!Number.isSafeInteger(startOffset) || startOffset < 0) throw new Error('invalid_offset')
  let through = startOffset, start = startOffset, skipping = startOffset !== 0
  let pending = [], pendingSize = 0, rows = [], retained = 0, lastInput, previousRecord, activityAnchor = null,piVersion=null
  let notice = ''
  function append(bytes) {
    if (!bytes.length) return
    if (pendingSize + bytes.length > MAX_RECORD_BYTES + 2) throw new Error('record_limit')
    pending.push(bytes.slice()); pendingSize += bytes.length
  }
  function record(end, delimiter, final) {
    const bytes = new Uint8Array(pendingSize)
    let cursor = 0
    for (const part of pending) { bytes.set(part,cursor); cursor += part.length }
    if (pendingSize > MAX_RECORD_BYTES) throw new Error('record_limit')
    let raw
    try { raw = decoder.decode(bytes) }
    catch (error) { if (final) { notice = 'Incomplete UTF-8 record at this finite window boundary.'; return }; throw error }
    let value
    try { value = JSON.parse(raw) } catch { if (final) { notice = 'Incomplete record at this finite window boundary.'; return }; throw new Error('invalid_json_record') }
    notice = ''
    if(value?.type==='session')piVersion=Number.isInteger(value.version)?value.version:'invalid'
    const row = { offset:start, presentationOffset:start, end, raw, delimiter, bytes:pendingSize + delimiter.length, ...presentation(value,raw,{piVersion}) }
    const replacement = previousRecord?.offset === start ? previousRecord : null
    if (replacement && (replacement.raw === raw || (replacement.type === row.type && replacement.role === row.role && replacement.time === row.time && replacement.text === row.text))) {
      row.presentationOffset = replacement.presentationOffset
      row.pairedOffset = replacement.pairedOffset
      row.activityAnchors = replacement.activityAnchors
    }
    const old = rows.findIndex(item => item.offset === start)
    if (old !== -1) {
      retained -= rows[old].bytes; rows.splice(old,1)
    }
    const transition = semanticTransition(row,replacement ? null : previousRecord,activityAnchor)
    activityAnchor = transition.anchor
    // Pairing is a property of adjacent source occurrences, not of the retained
    // display rows. Unknown metadata cannot pair two otherwise distant messages.
    previousRecord = row.type === 'conversation' ? row : null
    rows = rows.filter(item => {
      if (item.eventEnvelope && item.pairedOffset !== undefined) { retained -= item.bytes; return false }
      return true
    })
    if (row.category && !(row.eventEnvelope && row.pairedOffset !== undefined)) {
      rows.push(row); retained += row.bytes
      // Prose and activity have independent reserves. A long tool run or hidden
      // provider metadata must not consume the admitted conversation history.
      let count = rows.filter(item=>item.category === row.category).length
      let bytes = rows.filter(item=>item.category === row.category).reduce((total,item)=>total+item.bytes,0)
      while (count > MAX_OCCURRENCE_ROWS || bytes > MAX_OCCURRENCE_BYTES) {
        const index = rows.findIndex(item=>item.category === row.category)
        const [removed] = rows.splice(index,1)
        retained -= removed.bytes; bytes -= removed.bytes; count--
      }
    }
  }
  function snapshot() {
    return {rows:rows.map(({bytes,eventEnvelope,pairedOffset,category,...row})=>row),through,pendingBytes:pendingSize,retainedBytes:retained,notice}
  }

  return {
    push(offset, input, final = false) {
      if (!(input instanceof Uint8Array) || input.length > 65536) throw new Error('chunk_limit')
      if (offset !== through) {
        if (lastInput && offset === lastInput.offset && equal(input,lastInput.bytes)) return snapshot()
        throw new Error('range_gap')
      }
      lastInput = {offset,bytes:input.slice()}
      let begin = 0
      for (let index = 0; index < input.length; index++) {
        if (input[index] !== 10) continue
        if (skipping) { skipping = false; start = offset + index + 1 }
        else {
          append(input.subarray(begin,index))
          // CR belongs to the exact delimiter, even across HTTP boundaries.
          let delimiter = '\n'
          if (pendingSize && pending.at(-1).at(-1) === 13) {
            const part = pending.pop(); pendingSize--
            if (part.length > 1) pending.push(part.subarray(0,part.length-1))
            delimiter = '\r\n'
          }
          // A separator after an already displayed final EOF record updates
          // that row rather than creating another timeline entry.
          record(offset + index + 1,delimiter,false)
          pending = []; pendingSize = 0; start = offset + index + 1
        }
        begin = index + 1
      }
      if (!skipping) append(input.subarray(begin))
      through += input.length
      if (final && pendingSize && !skipping) record(through,'',true)
      return snapshot()
    },
    snapshot
  }
}
