import assert from 'node:assert/strict'
import { readFile, unlink } from 'node:fs/promises'
import { createServer } from 'node:http'
import { resolve } from 'node:path'
import test from 'node:test'
import { chromium } from '@playwright/test'

const fixtureBundle = resolve(process.cwd(), '.timeline-fixture-test.js')

function fixtureDocument() {
  return `<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8" />
  <meta name="viewport" content="width=device-width, initial-scale=1.0" />
  <title>Timeline fixture</title>
  <link rel="stylesheet" href="/style.css" />
</head>
<body>
  <div id="app"></div>
  <script src="/TimelineFixture.js"></script>
  <script>Elm.TimelineFixture.init({ node: document.getElementById('app') })</script>
</body>
</html>`
}

async function startFixtureServer() {
  const [ bundle, stylesheet ] = await Promise.all([
    readFile(fixtureBundle),
    readFile(resolve(process.cwd(), 'src/style.css')),
  ])
  const server = createServer((request, response) => {
    if (request.url === '/TimelineFixture.js') {
      response.writeHead(200, { 'content-type': 'application/javascript; charset=utf-8' })
      response.end(bundle)
      return
    }

    if (request.url === '/style.css') {
      response.writeHead(200, { 'content-type': 'text/css; charset=utf-8' })
      response.end(stylesheet)
      return
    }

    response.writeHead(200, { 'content-type': 'text/html; charset=utf-8' })
    response.end(fixtureDocument())
  })
  await new Promise((resolveServer) => server.listen(0, '127.0.0.1', resolveServer))
  const address = server.address()
  if (!address || typeof address === 'string') throw new Error('Fixture server did not receive a TCP port')
  return { server, url: `http://127.0.0.1:${address.port}` }
}

async function waitForSelection(page, day) {
  await waitForRange(page, `2026-01-${day}T00:00:00Z`)
}

async function waitForRange(page, since) {
  await page.waitForFunction((selectedDay) => {
    const selection = document.querySelector('[data-testid="timeline-fixture-selection"]')
    return selection?.textContent?.includes(selectedDay)
  }, since)
}

async function waitForNoSelection(page) {
  await page.waitForFunction(() => document.querySelector('[data-testid="timeline-fixture-selection"]')?.textContent === 'No bucket selected')
}

test('single lifecycle chart preserves counts, accessible controls and bounded target spacing', async () => {
  const { server, url } = await startFixtureServer()
  let browser
  try {
    browser = await chromium.launch({ headless: true })
    const context = await browser.newContext({ viewport: { width: 366, height: 768 } })
    const page = await context.newPage()
    await page.goto(url)
    assert.equal(await page.locator('.timeline-line-chart-panel').count(), 1)
    assert.equal(await page.locator('.timeline-series-toggle').count(), 9)
    assert.equal(await page.locator('.timeline-value-table').count(), 0)
    assert.equal(await page.getByLabel('Time window').inputValue(), '30')
    assert.equal(await page.locator('.timeline-graph-control select').nth(1).inputValue(), 'week')
    assert.equal(await page.locator('input[type="date"]').count(), 0)
    const styles = await page.locator('.timeline-line-chart').evaluate(chart => ({ fill: getComputedStyle(chart.querySelector('.timeline-line')).fill, pointerEvents: getComputedStyle(chart.querySelector('.timeline-line')).pointerEvents, marker: getComputedStyle(chart.querySelector('.timeline-point')).fill }))
    assert.equal(styles.fill, 'none'); assert.equal(styles.pointerEvents, 'none'); assert.notEqual(styles.marker, 'none')
    const point = (action, index) => page.locator('#timeline-point-lifecycle-' + action + '-' + index)
    assert.match(await point('created', 0).getAttribute('aria-label'), /, 6, not selected$/)
    const tasksToggle = page.getByRole('button', { name: 'Tasks', exact: true })
    await tasksToggle.focus()
    assert.equal(await tasksToggle.evaluate(button => document.activeElement === button && getComputedStyle(button).outlineStyle !== 'none' && parseFloat(getComputedStyle(button).outlineWidth) > 0), true)
    await page.getByRole('button', { name: 'Tasks', exact: true }).click()
    await page.waitForFunction(() => document.getElementById('timeline-point-lifecycle-created-0')?.getAttribute('aria-label').endsWith(', 3, not selected'))
    assert.match(await point('created', 0).getAttribute('aria-label'), /, 3, not selected$/)
    await page.getByRole('button', { name: 'Tasks', exact: true }).click()
    const cancel = page.getByRole('button', { name: 'Cancel', exact: true })
    await cancel.focus(); await page.keyboard.press('Space')
    await page.waitForFunction(() => !document.querySelector('.timeline-line.timeline-series-cancelled'))
    await page.keyboard.press('Enter'); await point('cancelled', 0).waitFor()
    await point('completed', 0).click(); await waitForSelection(page, '01')
    await point('archived', 1).click(); await waitForSelection(page, '02')
    await point('cancelled', 2).click(); await waitForSelection(page, '03')
    assert.match(await point('created', 2).getAttribute('aria-label'), /2026-01-03 00:00 to 2026-01-04 00:00 exclusive/)
    await page.getByTestId('fixture-spike').click(); await waitForNoSelection(page)
    await point('deleted', 1).focus()
    assert.equal(await point('deleted', 1).evaluate(control => document.activeElement === control && getComputedStyle(control).outlineStyle !== 'none' && parseFloat(getComputedStyle(control).outlineWidth) > 0), true)
    await page.keyboard.press('Enter'); await waitForSelection(page, '02')
    assert.equal(await page.getByTestId('timeline-fixture-card-count').textContent(), '1')
    assert.equal(await page.getByTestId('timeline-fixture-card-outside').count(), 0)
    const scrollBefore = await page.evaluate(() => window.scrollY)
    await page.keyboard.press('Space'); await waitForNoSelection(page)
    assert.equal(await page.evaluate(() => window.scrollY), scrollBefore)
    assert.equal(await page.getByTestId('timeline-fixture-card-count').textContent(), '2')
    const desktop = await browser.newPage({ viewport: { width: 1440, height: 900 } })
    await desktop.goto(url); assert.equal(await desktop.locator('.timeline-line-chart-panel').count(), 1); await desktop.close()
    await page.getByTestId('fixture-many-buckets').click()
    await page.waitForFunction(() => document.querySelectorAll('g.timeline-point-control').length === 1830)
    assert.equal(await page.locator('g.timeline-point-control[tabindex="0"]').count(), 5)
    assert.equal(await page.locator('.timeline-svg-scroll').evaluate(scroll => scroll.scrollWidth > scroll.clientWidth), true)
    const geometry = await page.locator('.timeline-line-chart').evaluate(chart => {
      const width = Number(chart.getAttribute('width'))
      const controls = [...chart.querySelectorAll('g.timeline-point-control')]
      const allInBounds = controls.every(control => { const box = control.getBBox(); return box.width >= 22 && box.height >= 22 && box.x >= 0 && box.x + box.width <= width })
      const zeroTargets = controls.filter(control => ['completed', 'archived', 'cancelled'].some(key => control.classList.contains('timeline-series-' + key))).map(control => control.getBBox()).sort((a, b) => a.x - b.x)
      const reachableZeros = zeroTargets.every((box, index) => index === 0 || zeroTargets[index - 1].x + zeroTargets[index - 1].width <= box.x)
      const pathsMatch = [...chart.querySelectorAll('.timeline-line')].every(line => {
        const key = [...line.classList].find(name => name.startsWith('timeline-series-'))
        return [...chart.querySelectorAll('g.' + key + ' .timeline-point')].every(marker => line.getAttribute('d').includes(marker.getAttribute('cx') + ' ' + marker.getAttribute('cy')))
      })
      return { allInBounds, reachableZeros, pathsMatch }
    })
    assert.deepEqual(geometry, { allInBounds: true, reachableZeros: true, pathsMatch: true })
    const ranges = await page.locator('g.timeline-series-created').evaluateAll(points => points.map(point => point.getAttribute('aria-label').match(/, (\d{4}-\d{2}-\d{2} \d{2}:\d{2}) to (\d{4}-\d{2}-\d{2} \d{2}:\d{2}) exclusive/)))
    assert.equal(ranges.every((range, index) => range && Date.parse(range[1]) < Date.parse(range[2]) && (index === 0 || ranges[index - 1][2] === range[1])), true)
    await point('created', 0).focus()
    for (const index of [30, 60, 90, 120, 150, 180]) { await page.keyboard.press('PageDown'); await page.waitForFunction(id => document.activeElement?.id === id, 'timeline-point-lifecycle-created-' + index) }
    for (const index of [181, 182, 183]) { await page.keyboard.press('ArrowRight'); await page.waitForFunction(id => document.activeElement?.id === id, 'timeline-point-lifecycle-created-' + index) }
    await page.keyboard.press('Enter'); await waitForRange(page, '2028-07-02T00:00:00Z')
    await page.keyboard.press('End'); await page.waitForFunction(() => document.activeElement?.id === 'timeline-point-lifecycle-created-365'); assert.equal(await point('created', 365).getAttribute('tabindex'), '0')
    await point('created', 365).scrollIntoViewIfNeeded(); await point('created', 365).click(); await waitForRange(page, '2028-12-31T00:00:00Z')
    await page.getByTestId('fixture-spike').click()
    await page.waitForFunction(() => document.querySelectorAll('g.timeline-point-control').length === 15)
    assert.equal(await point('created', 2).getAttribute('tabindex'), '0')
    await page.getByTestId('fixture-zero').click(); assert.match(await point('completed', 0).getAttribute('aria-label'), /, 0, not selected$/)
    await page.getByTestId('fixture-graph-error').click(); await page.getByText('Deterministic graph error', { exact: true }).waitFor()
    assert.equal(await page.getByTestId('timeline-fixture-card-count').textContent(), '2')
    assert.equal(await page.getByTestId('timeline-fixture-card-inside').count(), 1)
    assert.equal(await page.getByTestId('timeline-fixture-card-outside').count(), 1)
    assert.equal(await page.getByTestId('timeline-fixture-card-inside').textContent(), 'Fixture inside event')
    assert.equal(await page.getByTestId('timeline-fixture-card-outside').textContent(), 'Fixture outside event')
    await page.getByTestId('fixture-empty').click(); await page.getByText('No lifecycle activity in this date range.', { exact: true }).waitFor()
  } finally {
    await browser?.close()
    await new Promise((resolveServer, rejectServer) => server.close(error => error ? rejectServer(error) : resolveServer()))
    await unlink(fixtureBundle)
  }
})
