export function installAuditViewport(app) {
  let generation = 0
  app.ports.restoreAuditViewport?.subscribe(({ stamp, top }) => {
    const owned = ++generation
    if (!/^\d+:\d+$/.test(stamp) || !Number.isFinite(top)) return
    requestAnimationFrame(() => requestAnimationFrame(() => {
      if (owned !== generation) return
      const ledger = document.querySelector(`[data-audit-viewport="${stamp}"]`)
      const scroll = document.getElementById('main-content-scroll')
      if (ledger && scroll?.contains(ledger)) scroll.scrollTop = Math.max(0, top)
    }))
  })
}
