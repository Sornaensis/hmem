// Frame exact bytes before decoding UTF-8. A raw window and a JSONL record
// have separate bounds: one record may span more than four HTTP chunks.
export const MAX_RECORD_BYTES = 1048576
export const MAX_VIEW_ROWS = 100
export const MAX_VIEW_BYTES = 2097152
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

export function toolPayload(value, field = 'input') {
  let parsed = value
  if (typeof value === 'string') { try { parsed = JSON.parse(value) } catch { parsed = {[field]:value} } }
  if (parsed === undefined) parsed = null
  const text = JSON.stringify(parsed, null, 2)
  const tokens = []
  let through = 0
  for (const match of text.matchAll(/"(?:\\.|[^"\\])*"\s*:|"(?:\\.|[^"\\])*"|-?\d+(?:\.\d+)?(?:[eE][+-]?\d+)?|\b(?:true|false|null)\b|[^"\w-]+/g)) {
    if (tokens.length === 2048) { tokens.push({text:text.slice(through),kind:'punctuation'}); break }
    const [value] = match
    tokens.push({text:value,kind:value.trim().endsWith(':')?'key':value.startsWith('"')?'string':/^-?\d/.test(value)?'number':/^(true|false|null)$/.test(value)?'literal':'punctuation'})
    through = match.index + value.length
  }
  return {text,tokens}
}

// Presentation never changes the stored record or its byte boundaries.
export function presentation(value, raw) {
  const object = value && typeof value === 'object' && !Array.isArray(value) ? value : {}
  const time = string(object.timestamp || object.time)
  const block = (kind, text, label = '') => ({kind, text:string(text), label:string(label).slice(0,256)})
  const tool = (kind, value, name = '') => {
    const payload = toolPayload(value,kind === 'result'?'output':'input')
    const first = payload.text.split('\n').map(line=>line.trim()).find(line=>line.includes(':')) || ''
    return {...block(kind,payload.text,`${kind === 'result'?'Result':'Call'}${name?' · '+short(name):''}${first?' · '+short(first.replace(/,$/,'')):''}`),tokens:payload.tokens}
  }
  const contentBlocks = content => (Array.isArray(content) ? content : [{type:typeof content === 'string'?'text':'unknown',text:content}]).flatMap(part => {
    if (!part || typeof part !== 'object' || Array.isArray(part)) return [block('unknown',printable(part),'Content')]
    if (['text','input_text','output_text'].includes(part.type)) return typeof part.text === 'string' ? [block('text',part.text)] : [block('unknown',printable(part),'Content')]
    if (['redacted_thinking','encrypted_thinking'].includes(part.type) || (['thinking','reasoning'].includes(part.type) && (part.encrypted_content || part.encrypted))) return [block('encrypted','','Thinking...')]
    if (['thinking','reasoning','summary_text'].includes(part.type)) return [block('thinking',part.thinking || part.text,'Thinking')]
    if (part.type === 'tool_use') return [tool('tool',part.input,part.name)]
    if (part.type === 'tool_result') return [tool('result',part.content,part.is_error ? 'error' : part.tool_use_id)]
    return [block('unknown',printable(part),'Attachment / content')]
  })
  const conversation = (role,blocks,extra={}) => ({type:'conversation',role:string(role).slice(0,256)||'assistant',time,text:JSON.stringify(blocks),...extra})
  if (object.type === 'response_item') {
    const item=object.payload || {}
    if(item.type === 'message') return conversation(item.role || 'assistant',contentBlocks(item.content))
    if(['function_call','custom_tool_call'].includes(item.type)) return conversation('assistant',[tool('tool',item.arguments ?? item.input,item.name)])
    if(['function_call_output','custom_tool_call_output'].includes(item.type)) return conversation('tool',[tool('result',item.output,item.call_id)])
    if(item.type === 'reasoning') return conversation('assistant',[item.encrypted_content ? block('encrypted','','Thinking...') : block('thinking', (Array.isArray(item.summary) ? item.summary.map(part=>part?.text || '').join('\n') : printable(item.summary)),'Thinking')])
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

export function createRecordFramer(startOffset = 0) {
  if (!Number.isSafeInteger(startOffset) || startOffset < 0) throw new Error('invalid_offset')
  let through = startOffset, start = startOffset, skipping = startOffset !== 0
  let pending = [], pendingSize = 0, rows = [], retained = 0, lastInput
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
    const old = rows.findIndex(item => item.offset === start)
    if (old !== -1) {
      const prior = rows[old]
      if (prior.raw === raw) { row.presentationOffset = prior.presentationOffset; row.pairedOffset = prior.pairedOffset }
      retained -= prior.bytes; rows.splice(old,1)
    }
    pairOccurrence(rows.at(-1),row)
    rows.push(row); retained += row.bytes
    while (rows.length > MAX_VIEW_ROWS || retained > MAX_VIEW_BYTES) { retained -= rows.shift().bytes }
  }
  function pairOccurrence(previous,row) {
    if (!previous || previous.type !== 'conversation' || row.type !== 'conversation') return
    const fingerprint = row => row.role+':'+JSON.stringify(JSON.parse(row.text).map(({kind,text})=>({kind,text})))
    const paired = Boolean(previous.eventEnvelope)!==Boolean(row.eventEnvelope)
      && (previous.pairedOffset === undefined || previous.pairedOffset === row.offset)
      && (row.pairedOffset === undefined || row.pairedOffset === previous.offset)
      && fingerprint(previous)===fingerprint(row)
    const firstTime=Date.parse(previous.time),lastTime=Date.parse(row.time)
    // Pair before bounded eviction, so an arriving canonical record retains
    // the occurrence identity even if this same push retires its envelope.
    if(paired && Number.isFinite(firstTime) && Number.isFinite(lastTime) && Math.abs(firstTime-lastTime)<=100) {
      const canonical = row.eventEnvelope ? previous : row
      canonical.presentationOffset = Math.min(previous.presentationOffset,row.presentationOffset)
      previous.pairedOffset = row.offset; row.pairedOffset = previous.offset
    }
  }
  function snapshot() {
    return {rows:rows.filter(row=>!(row.eventEnvelope && row.pairedOffset !== undefined)).map(({bytes,eventEnvelope,pairedOffset,...row})=>row),through,pendingBytes:pendingSize,retainedBytes:retained,notice}
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
