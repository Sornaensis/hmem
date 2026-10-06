import assert from 'node:assert/strict'
import test from 'node:test'
import { createHash } from 'node:crypto'
import { openDiscovery } from './observation-discovery-fixture.mjs'
import { revealObservationRow } from './observation-viewport-fixture.mjs'

const fullBoundary = '😀 '.repeat(104857) + 'END'
assert.equal(Buffer.byteLength(fullBoundary), 524288)
const longSubject = 'src/' + 'long-unicode-😀-segment/'.repeat(100) + 'Main.elm'
const longMultiline = 'First <script>window.previewUnsafe=true</script> 😀\n' + 'Multiline evidence\n'.repeat(300) + 'Full multiline tail'
const digest = value => createHash('sha256').update(value).digest('hex')
const representative = values => [
  { ...values[0], id: 'boundary', subject: longSubject, subjects: [{ subject_kind: 'file', subject: longSubject }, { subject_kind: 'glob', subject: 'src/**/*.elm' }, { subject_kind: 'file', subject: 'src/Extra.elm' }], content: fullBoundary, updated_at: '2026-10-06T12:35:56.123Z' },
  { ...values[1], id: 'multiline', content: longMultiline, updated_at: '2026-10-06T12:34:56Z' },
  { ...values[2], id: 'short', content: 'Short evidence.', updated_at: '2026-10-06T12:33:56Z' }
]

for (const width of [1440, 320]) test('production ' + width + ' CSS-pixel previews bound scan geometry and preserve full content/provenance', { timeout: 60000 }, async () => {
  const h = await openDiscovery({ width, height: 900 }, representative)
  try {
    await h.page.addInitScript(() => { window.previewCopies = []; Object.defineProperty(navigator.clipboard, 'writeText', { value: async value => { window.previewCopies.push(value) } }) })
    await h.start()
    const cards = h.page.locator('.observation-card')
    assert.equal(await cards.count(), 3)
    const geometry = await cards.evaluateAll(elements => elements.map(element => {
      const preview = element.querySelector('.observation-summary'), subject = element.querySelector('.observation-subject')
      return { nameLength: [...element.getAttribute('aria-label')].length, previewLength: [...preview.textContent].length,
        previewHeight: preview.getBoundingClientRect().height, previewLine: parseFloat(getComputedStyle(preview).lineHeight),
        subjectHeight: subject.getBoundingClientRect().height, subjectLine: parseFloat(getComputedStyle(subject).lineHeight),
        subjectLength: [...subject.textContent].length, nested: element.querySelectorAll('button,summary,details,input,a').length,
        height: element.getBoundingClientRect().height }
    }))
    for (const measured of geometry) {
      assert.ok(measured.nameLength <= 180 && measured.previewLength <= 240 && measured.subjectLength <= 96, JSON.stringify(measured))
      assert.ok(measured.previewHeight <= measured.previewLine * 3 + 1 && measured.subjectHeight <= measured.subjectLine * 2 + 1, JSON.stringify(measured))
      assert.equal(measured.nested, 0)
      assert.ok(measured.height < 420, JSON.stringify(measured))
    }
    assert.equal(await cards.first().locator('.observation-summary').textContent().then(value => value.endsWith('…')), true)
    assert.equal(await h.page.evaluate(() => !!window.previewUnsafe), false)
    assert.equal(await h.page.locator('.observation-sha').first().textContent(), 'Provenance revision: ' + h.sha.slice(0, 12) + '…')
    assert.match(await h.page.locator('.observation-updated').first().textContent(), /2026-10-06 12:35:56\.123 UTC/)
    const originalStamp = await h.page.locator('#observation-panel').getAttribute('data-observation-context'), before = h.receipts.length
    const disclosure = h.page.locator('.observation-card-provenance').first()
    await disclosure.locator('summary').focus(); await h.page.keyboard.press('Enter')
    assert.equal(await disclosure.getAttribute('open'), '')
    assert.deepEqual(await disclosure.locator('.observation-subject-row .observation-subject-copy').allTextContents(), [longSubject, 'src/**/*.elm', 'src/Extra.elm'])
    assert.equal(await disclosure.locator('.observation-detail-sha code').textContent(), h.sha)
    await disclosure.getByRole('button', { name: 'Copy full revision', exact: true }).click()
    await h.page.waitForFunction(() => window.previewCopies.length === 1, null, { timeout: 5000 })
    assert.deepEqual(await h.page.evaluate(() => window.previewCopies), [h.sha])
    assert.equal(h.receipts.length, before)
    assert.equal(await h.page.locator('#observation-panel').getAttribute('data-observation-context'), originalStamp)
    await disclosure.locator('summary').focus(); await h.page.keyboard.press('Space')
    assert.equal(await disclosure.getAttribute('open'), null)
    const cardId = await cards.first().getAttribute('id')
    await cards.first().focus()
    const activationStart = performance.now()
    await h.page.keyboard.press('Enter'); await h.idle()
    await h.page.waitForFunction(() => document.activeElement?.id === 'observation-detail-heading', null, { timeout: 5000 })
    const detail = h.page.locator('.observation-detail-card')
    const activationMs = performance.now() - activationStart
    assert.ok(activationMs < 5000, 'Boundary detail must become usable within the existing focus deadline')
    const reader = detail.locator('#observation-content-reader')
    assert.equal(await reader.getAttribute('readonly'), '')
    assert.equal(digest(await reader.inputValue()), digest(fullBoundary))
    await reader.focus(); await h.page.keyboard.press('Control+End')
    assert.ok(await reader.evaluate(element => element.scrollTop > 0 && element.selectionStart === element.value.length))
    await detail.getByRole('button', { name: 'Copy full content', exact: true }).click()
    await h.page.waitForFunction(() => window.previewCopies.length === 2, null, { timeout: 5000 })
    assert.equal(digest(await h.page.evaluate(() => window.previewCopies[1])), digest(fullBoundary))
    assert.deepEqual(await detail.locator('.observation-subject-row .observation-subject-copy').allTextContents(), [longSubject, 'src/**/*.elm', 'src/Extra.elm'])
    assert.equal(await detail.locator('.observation-detail-sha code').textContent(), h.sha)
    assert.match(await detail.locator('.observation-detail-meta').textContent(), /Content updated2026-10-06 12:35:56\.123 UTC/)
    await h.page.getByRole('button', { name: 'Back to results', exact: true }).click()
    await h.page.waitForFunction(id => document.activeElement?.id === id, cardId, { timeout: 5000 })
    const multiline = h.page.locator('.observation-result[data-observation-id="multiline"] .observation-card')
    await revealObservationRow(h.page, multiline); await multiline.click(); await h.idle()
    assert.equal(await h.page.locator('.observation-detail-content').textContent(), longMultiline)
    assert.equal(await h.page.locator('.observation-detail-content script').count(), 0)
    const overflow = await h.page.evaluate(() => document.documentElement.scrollWidth - innerWidth)
    assert.ok(overflow <= 1, 'Horizontal overflow ' + overflow)
    console.log(JSON.stringify({ fixture: 'Observation preview', width, boundaryBytes: Buffer.byteLength(fullBoundary), activationMs: Math.round(activationMs), maximumCardHeight: Math.round(Math.max(...geometry.map(value => value.height))), maximumNameCodepoints: Math.max(...geometry.map(value => value.nameLength)), overflow }))
  } catch (error) {
    const state = await h.page.evaluate(() => ({ focus: document.activeElement?.id, stamp: document.getElementById('observation-panel')?.dataset.observationContext,
      loading: [...document.querySelectorAll('.loading-indicator')].map(element => element.textContent), detail: !!document.getElementById('observation-detail'),
      detailLength: document.querySelector('.observation-detail-content')?.textContent.length }))
    throw new Error(error.message + ' ' + JSON.stringify({ state, errors: h.errors, requests: h.receipts.map(({ endpoint, method, done }) => ({ endpoint, method, done })) }))
  } finally { await h.close() }
})

test('production short narrow reader bounds enlarged text and copies canonical CRLF plus astral content', { timeout: 60000 }, async () => {
  const canonical = 'Line 😀\r\n'.repeat(2048)
  assert.ok(Buffer.byteLength(canonical) > 16384)
  const h = await openDiscovery({ width: 320, height: 200 }, values => [{ ...values[0], id: 'crlf', content: canonical }])
  try {
    await h.page.addInitScript(() => { window.previewCopies = []; Object.defineProperty(navigator.clipboard, 'writeText', { value: async value => { window.previewCopies.push(value) } }) })
    await h.start(); await h.page.addStyleTag({ content: 'html { font-size: 200%; }' })
    await h.page.locator('.observation-card').click(); await h.idle()
    await h.page.waitForFunction(() => document.activeElement?.id === 'observation-detail-heading', null, { timeout: 5000 })
    const reader = h.page.getByLabel('Full observation content (read only)', { exact: true })
    assert.equal(digest(await reader.inputValue()), digest(canonical.replaceAll('\r\n', '\n')))
    const geometry = await reader.evaluate(element => ({ height: element.getBoundingClientRect().height, limit: innerHeight * 0.7, textSize: getComputedStyle(document.documentElement).fontSize }))
    assert.equal(geometry.textSize, '32px'); assert.ok(geometry.height <= geometry.limit + 1, JSON.stringify(geometry))
    const before = h.receipts.length
    await h.page.getByRole('button', { name: 'Copy full content', exact: true }).click()
    await h.page.waitForFunction(() => window.previewCopies.length === 1, null, { timeout: 5000 })
    assert.equal(digest(await h.page.evaluate(() => window.previewCopies[0])), digest(canonical))
    assert.equal(h.receipts.length, before)
    assert.ok(await h.page.evaluate(() => document.documentElement.scrollWidth - innerWidth <= 1))
    console.log(JSON.stringify({ fixture: 'Observation CRLF reader', width: 320, height: 200, textSize: geometry.textSize, readerHeight: Math.round(geometry.height), canonicalBytes: Buffer.byteLength(canonical), nativeDisplayLineEndings: 'LF', canonicalClipboardPort: 'exact CRLF' }))
  } finally { await h.close() }
})
