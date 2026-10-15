const BATCH_SIZE = 128

// Require an explicit RFC3339 offset: browser-dependent local parsing must never
// reinterpret an API instant. Validate the calendar before Date can normalize it.
function parseInstant(source) {
  if (typeof source !== 'string') return null
  const match = /^(\d{4})-(\d{2})-(\d{2})[Tt](\d{2}):(\d{2}):(\d{2})(?:\.(\d+))?([Zz]|[+-]\d{2}:\d{2})$/.exec(source)
  if (!match) return null
  const [, yearText, monthText, dayText, hourText, minuteText, secondText, fraction = '', offset] = match
  const [year, month, day, hour, minute, second] = [yearText, monthText, dayText, hourText, minuteText, secondText].map(Number)
  const leap = year % 4 === 0 && (year % 100 !== 0 || year % 400 === 0)
  const days = [31, leap ? 29 : 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
  if (year < 1 || month < 1 || month > 12 || day < 1 || day > days[month - 1] || hour > 23 || minute > 59 || second > 59) return null
  let offsetMinutes = 0
  if (offset.toUpperCase() !== 'Z') {
    const offsetHour = Number(offset.slice(1, 3)), offsetMinute = Number(offset.slice(4, 6))
    if (offsetHour > 23 || offsetMinute > 59) return null
    offsetMinutes = (offsetHour * 60 + offsetMinute) * (offset[0] === '-' ? -1 : 1)
  }
  const date = new Date(0)
  date.setUTCFullYear(year, month - 1, day)
  date.setUTCHours(hour, minute, second, Number(fraction.slice(0, 3).padEnd(3, '0')))
  return new Date(date.getTime() - offsetMinutes * 60000)
}

export function createTimestampFormatter(timeZone = Intl.DateTimeFormat().resolvedOptions().timeZone) {
  const formatter = new Intl.DateTimeFormat('en-GB', {
    timeZone, calendar: 'gregory', numberingSystem: 'latn', hourCycle: 'h23',
    era: 'short', year: 'numeric', month: '2-digit', day: '2-digit', hour: '2-digit', minute: '2-digit'
  })
  return source => {
    const instant = parseInstant(source)
    if (!instant) return null
    const parts = Object.fromEntries(formatter.formatToParts(instant).map(({ type, value }) => [type, value]))
    // Offsets can move the endpoints of the supported four-digit calendar into
    // another era or a five-digit year. Do not silently misrepresent that date.
    if (parts.era !== 'AD' || parts.year.length > 4) return null
    const day = `${parts.year.padStart(4, '0')}-${parts.month}-${parts.day}`
    return { text: `${day} ${parts.hour}:${parts.minute}`, day }
  }
}

export function installTimestampDisplay(app, { format = createTimestampFormatter(), schedule = queueMicrotask } = {}) {
  if (!app.ports.formatTimestamps || !app.ports.timestampsFormatted) return { dispose() {} }
  let disposed = false
  const handle = request => {
    if (!request || typeof request.context !== 'string' || !Number.isSafeInteger(request.id) || !Array.isArray(request.sources) || request.sources.length > BATCH_SIZE) return
    const sources = [...new Set(request.sources)]
    if (!sources.every(source => typeof source === 'string')) return
    const values = sources.map(source => ({ source, local: format(source) }))
    // Avoid re-entering Elm while it is dispatching an outgoing port command.
    schedule(() => {
      if (!disposed) app.ports.timestampsFormatted.send({ context: request.context, id: request.id, values })
    })
  }
  app.ports.formatTimestamps.subscribe(handle)
  return { dispose() { disposed = true; app.ports.formatTimestamps.unsubscribe(handle) } }
}
