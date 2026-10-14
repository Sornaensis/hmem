// Passive host-clock observations, used only by the unqualified diagnostic driver.
export function createLiveDiagnostics() {
  const boundaries = {}, requests = [], byId = new Map()
  let requestCount = 0, lastId = 0, omittedPrepared = 0, omittedFulfilled = 0, evaluations = 0, first = null, decisive = null, latest = null
  let paints = 0, firstPaint = null, latestPaint = null, lastAt = -Infinity
  const time = at => {
    if (!Number.isFinite(at) || at < 0 || at < lastAt) throw new Error('Invalid or nonmonotonic host boundary')
    lastAt = at
    return at
  }
  const boundary = (name, at) => {
    if (boundaries[name] !== undefined) throw new Error('Repeated boundary: ' + name)
    boundaries[name] = time(at)
  }
  const fields = state => {
    const booleans = ['dispatchTurnComplete', 'loading', 'focused', 'anchorVisible', 'logicalNavigationComplete']
    if (booleans.some(key => typeof state[key] !== 'boolean') || !Number.isInteger(state.activeRequests) || state.activeRequests < 0 || !Number.isInteger(state.followUpRequests) || state.followUpRequests < 0) throw new Error('Malformed readiness state')
    return Object.fromEntries([...booleans, 'activeRequests', 'followUpRequests'].map(key => [key, state[key]]))
  }
  return {
    start(at) { boundary('dispatchStart', at) },
    dispatched(at) { boundary('dispatchReturned', at) },
    arrival(id, key, at) {
      if (!Number.isInteger(id) || id < 1 || (lastId && id !== lastId + 1) || typeof key !== 'string' || key.length > 128) throw new Error('Malformed request boundary')
      const arrivalAt = time(at)
      lastId = id
      requestCount += 1
      if (requests.length === 32) return
      const receipt = { id, key, arrivalAt }
      requests.push(receipt); byId.set(id, receipt)
    },
    prepared(id, at, status, bytes) {
      time(at)
      const receipt = byId.get(id)
      if (!Number.isInteger(id) || id > lastId || id < 1 || !Number.isInteger(status) || status < 100 || status > 599 || !Number.isInteger(bytes) || bytes < 0) throw new Error('Malformed response preparation')
      if (!receipt) { if (!requests.length || id <= requests.at(-1).id || ++omittedPrepared > requestCount - requests.length) throw new Error('Missing response arrival'); return }
      if (receipt.preparedAt !== undefined) throw new Error('Malformed response preparation')
      Object.assign(receipt, { preparedAt: at, status, bytes })
    },
    fulfilled(id, at, fulfilled) {
      time(at)
      const receipt = byId.get(id)
      if (!Number.isInteger(id) || id > lastId || id < 1 || typeof fulfilled !== 'boolean') throw new Error('Malformed fulfilment boundary')
      if (!receipt) { if (!requests.length || id <= requests.at(-1).id || ++omittedFulfilled > omittedPrepared) throw new Error('Missing response preparation'); return }
      if (receipt.preparedAt === undefined || receipt.fulfilledAt !== undefined) throw new Error('Malformed fulfilment boundary')
      Object.assign(receipt, { fulfilledAt: at, fulfilled })
    },
    readiness(at, state, ready, proof) {
      if (typeof ready !== 'boolean') throw new Error('Malformed readiness outcome')
      if (!Number.isFinite(proof?.startedAt) || !Number.isFinite(proof?.endedAt) || proof.startedAt < lastAt || proof.endedAt < proof.startedAt || proof.endedAt > at) throw new Error('Malformed logical proof interval')
      const value = { at: time(at), ...fields(state), ready, logicalProof: { startedAt: proof.startedAt, endedAt: proof.endedAt, durationMs: proof.endedAt - proof.startedAt } }
      evaluations += 1; first ||= value; latest = value
      if (ready) decisive = value
    },
    paintStarted(at) { boundary('paintStarted', at) },
    paintEnded(at, activeRequests, logicalNavigationComplete, proof = null) {
      if (!Number.isInteger(activeRequests) || activeRequests < 0 || typeof logicalNavigationComplete !== 'boolean') throw new Error('Malformed paint outcome')
      const value = { startedAt: boundaries.paintStarted, endedAt: time(at), activeRequests, logicalNavigationComplete }
      if (!Number.isFinite(value.startedAt)) throw new Error('Missing paint start')
      if (proof !== null) {
        if (!Number.isFinite(proof.startedAt) || !Number.isFinite(proof.endedAt) || proof.startedAt < at || proof.endedAt < proof.startedAt) throw new Error('Malformed postpaint proof interval')
        value.logicalProof = { startedAt: proof.startedAt, endedAt: time(proof.endedAt), durationMs: proof.endedAt - proof.startedAt }
      } else value.logicalProof = null
      paints += 1; firstPaint ||= value; latestPaint = value
      delete boundaries.paintStarted
    },
    settled(at) { boundary('settled', at) },
    stable(startedAt, endedAt) { boundary('stabilityStarted', startedAt); boundary('stabilityEnded', endedAt) },
    snapshot() {
      for (const name of ['dispatchStart', 'dispatchReturned', 'settled', 'stabilityStarted', 'stabilityEnded']) if (!Number.isFinite(boundaries[name])) throw new Error('Missing boundary: ' + name)
      if (!decisive || !latestPaint || latestPaint.activeRequests !== 0 || !latestPaint.logicalNavigationComplete || omittedFulfilled !== requestCount - requests.length || requests.some(r => r.preparedAt === undefined || r.fulfilledAt === undefined)) throw new Error('Incomplete live diagnostic phase')
      return { qualified: false, clockDomain: 'Node performance.now; browser clock is not combined', boundaries: { ...boundaries },
        intervals: { dispatchMs: boundaries.dispatchReturned - boundaries.dispatchStart, settleMs: boundaries.settled - boundaries.dispatchStart, twoPaintMs: latestPaint.endedAt - latestPaint.startedAt, stabilityMs: boundaries.stabilityEnded - boundaries.stabilityStarted },
        requests: requests.map(r => ({ ...r })), omittedRequests: requestCount - requests.length,
        readiness: { first, decisive, latest, evaluations, omittedEvaluations: evaluations - new Set([first, decisive, latest]).size },
        paints: { first: firstPaint, latest: latestPaint, attempts: paints, omittedAttempts: paints - new Set([firstPaint, latestPaint]).size } }
    }
  }
}

export function serializeLiveDiagnostic(receipt) {
  const bytes = JSON.stringify(receipt, null, 2) + '\n'
  if (Buffer.byteLength(bytes) > 256 * 1024) throw new Error('Live diagnostic exceeds 256 KiB')
  return bytes
}
