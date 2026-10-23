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

function presentation(value, raw) {
  const object = value !== null && typeof value === 'object' && !Array.isArray(value) ? value : {}
  const field = name => typeof object[name] === 'string' ? object[name].slice(0,256) : ''
  const content = typeof object.text === 'string' ? object.text : typeof object.content === 'string' ? object.content : raw
  return { type:field('type'), role:field('role'), time:field('timestamp') || field('time'), text:content.slice(0,4096) }
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
    const row = { offset:start, end, raw, delimiter, bytes:pendingSize + delimiter.length, ...presentation(value,raw) }
    const old = rows.findIndex(item => item.offset === start)
    if (old !== -1) { retained -= rows[old].bytes; rows.splice(old,1) }
    rows.push(row); retained += row.bytes
    while (rows.length > MAX_VIEW_ROWS || retained > MAX_VIEW_BYTES) { retained -= rows.shift().bytes }
  }
  function snapshot() { return { rows:rows.map(({bytes,...row}) => row), through, pendingBytes:pendingSize, retainedBytes:retained, notice } }
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
