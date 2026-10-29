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
export function presentation(value, raw, {archive=false}={}) {
  const object = value && typeof value === 'object' && !Array.isArray(value) ? value : {}
  const time = string(object.timestamp || object.time)
  const block = (kind, text, label = '') => ({kind, text:string(text), label:string(label).slice(0,256)})
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
    if (!part || typeof part !== 'object' || Array.isArray(part)) return [block('unknown',archive?'':printable(part),'Content')]
    if (['text','input_text','output_text'].includes(part.type)) return typeof part.text === 'string' ? [block('text',part.text)] : [block('unknown',archive?'':printable(part),'Content')]
    if (['redacted_thinking','encrypted_thinking'].includes(part.type) || (['thinking','reasoning'].includes(part.type) && (part.encrypted_content || part.encrypted))) return [block('encrypted','','Thinking...')]
    if (['thinking','reasoning','summary_text'].includes(part.type)) return [block('thinking',part.thinking || part.text,'Thinking')]
    if (part.type === 'tool_use') return [tool('tool',part.input,part.name)]
    if (part.type === 'tool_result') return [tool('result',part.content,part.is_error ? 'error' : part.tool_use_id)]
    return [block('unknown',archive?'':printable(part),'Attachment / content')]
  })
  const conversation = (role,blocks,extra={}) => {
    const row={type:'conversation',role:string(role).slice(0,256)||'assistant',time,text:JSON.stringify(blocks),...extra}
    // Archive indexing keeps one bounded row and its adjacent occurrence. These
    // private refs avoid reparsing normalized blocks and never enter JSON/IDB.
    if(archive)Object.defineProperty(row,archiveBlocks,{value:blocks})
    return row
  }
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
  if(typeof object.text === 'string' || typeof object.content === 'string') return {type:string(object.type),role:string(object.role),time,text:typeof object.text === 'string' ? object.text : object.content}
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
  const iterator=chunks();let pending='',at=0,done=false,part=0
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
    || (['tool','result'].includes(part.kind) && (part.text.trim() || part.label.trim())))
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
      if (part.kind === 'text') anchor = null
      else {
        anchor ??= `${row.presentationOffset}:${index}`
        row.activityAnchors[index] = anchor
      }
    }
  }
  row.category = visible.some(([,part])=>part.kind === 'text') ? 'message' : visible.length ? 'activity' : null
  return {row, previous:row.type === 'conversation' ? row : null, anchor, paired,
    retired:paired && previous.eventEnvelope ? previous.offset : null,
    hidden:row.eventEnvelope && row.pairedOffset !== undefined}
}

export function createRecordFramer(startOffset = 0) {
  if (!Number.isSafeInteger(startOffset) || startOffset < 0) throw new Error('invalid_offset')
  let through = startOffset, start = startOffset, skipping = startOffset !== 0
  let pending = [], pendingSize = 0, rows = [], retained = 0, lastInput, previousRecord, activityAnchor = null
  const leadingNotice = skipping ? 'Window begins inside a possible record; the leading fragment is omitted.' : ''
  let notice = leadingNotice
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
    notice = leadingNotice
    const row = { offset:start, presentationOffset:start, end, raw, delimiter, bytes:pendingSize + delimiter.length, ...presentation(value,raw) }
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
