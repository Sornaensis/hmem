import assert from 'node:assert/strict'
import { mkdir, readFile, unlink } from 'node:fs/promises'
import { createServer } from 'node:http'
import { join, resolve } from 'node:path'
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

async function clickMarker(page, point) {
  await point.scrollIntoViewIfNeeded()
  const box = await point.locator('.timeline-point').boundingBox()
  await page.mouse.click(box.x + box.width / 2, box.y + box.height / 2)
}

async function renderedGeometry(page) {
  return page.locator('.timeline-line-chart').evaluate(chart => {
    const center = (element, x, y) => new DOMPoint(x, y).matrixTransform(element.getScreenCTM())
    const near = (a, b) => Math.abs(a - b) < 0.01
    const controls = [...chart.querySelectorAll('g.timeline-point-control')]
    const pathsMatch = [...chart.querySelectorAll('.timeline-line')].every(line => {
      const series = [...line.classList].find(name => name.startsWith('timeline-series-'))
      const vertices = [...line.getAttribute('d').matchAll(/[ML] ([\d.e+-]+) ([\d.e+-]+)/g)].map(match => center(line, Number(match[1]), Number(match[2])))
      const markers = [...chart.querySelectorAll('g.' + series + ' .timeline-point')]
      return vertices.length === markers.length && markers.every((marker, index) => {
        const position = center(marker, marker.cx.baseVal.value, marker.cy.baseVal.value)
        return near(position.x, vertices[index].x) && near(position.y, vertices[index].y)
      })
    })
    const sharedX = controls.every(control => {
      const marker = control.querySelector('.timeline-point'), hit = control.querySelector('.timeline-point-hitarea')
      const index = Number(control.id.split('-').at(-1)), reference = chart.querySelector('#timeline-point-lifecycle-created-' + index + ' .timeline-point')
      const point = center(marker, marker.cx.baseVal.value, marker.cy.baseVal.value), target = center(hit, hit.cx.baseVal.value, hit.cy.baseVal.value)
      return near(point.x, target.x) && near(point.y, target.y) && (!reference || near(point.x, center(reference, reference.cx.baseVal.value, reference.cy.baseVal.value).x))
    })
    const labelsMatch = [...chart.querySelectorAll('.timeline-chart-x-axis')].every(label => {
      const control = controls.find(control => control.getAttribute('aria-label').includes(', ' + label.textContent + ','))
      const marker = control?.querySelector('.timeline-point')
      return marker && near(center(label, label.x.baseVal[0].value, 0).x, center(marker, marker.cx.baseVal.value, 0).x)
    })
    const labels = [...chart.querySelectorAll('.timeline-chart-axis,.timeline-chart-x-axis')].map(label => {
      const bounds = label.getBoundingClientRect(), transform = label.getScreenCTM()
      return { text: label.textContent, width: bounds.width, height: bounds.height, a: transform.a, d: transform.d }
    })
    const allInBounds = controls.every(control => { const box = control.getBBox(); return box.width >= 22 && box.height >= 22 && box.x >= 0 && box.x + box.width <= chart.getBoundingClientRect().width })
    return { pathsMatch, sharedX, labelsMatch, allInBounds, labels, width: chart.getBoundingClientRect().width, height: chart.getBoundingClientRect().height }
  })
}

test('single lifecycle chart preserves counts, accessible controls and shared bucket geometry', async () => {
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
    await clickMarker(page, point('completed', 0)); await waitForSelection(page, '01')
    await clickMarker(page, point('archived', 1)); await waitForSelection(page, '02')
    await clickMarker(page, point('cancelled', 2)); await waitForSelection(page, '03')
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
    let referenceLabels
    for (const width of [2560, 1440, 366, 2560]) {
      await page.setViewportSize({ width, height: 900 })
      await page.evaluate(() => new Promise(resolve => requestAnimationFrame(() => requestAnimationFrame(resolve))))
      const geometry = await renderedGeometry(page)
      assert.ok(geometry.pathsMatch && geometry.sharedX && geometry.labelsMatch && geometry.allInBounds, JSON.stringify(geometry))
      assert.equal(geometry.height, 280)
      assert.ok(geometry.labels.every(label => label.a === 1 && label.d === 1 && label.height >= 8 && label.height <= 18), JSON.stringify(geometry.labels))
      if (referenceLabels) assert.deepEqual(geometry.labels, referenceLabels, 'rendered glyph bounds stay constant through resize')
      else referenceLabels = geometry.labels
      if (width === 2560) assert.ok(geometry.width > 2200, 'plot uses the wide viewport')
      if (process.env.HMEM_TIMELINE_GEOMETRY_SHOTS && [2560, 366].includes(width)) {
        await mkdir(process.env.HMEM_TIMELINE_GEOMETRY_SHOTS, { recursive: true })
        await page.screenshot({ path: join(process.env.HMEM_TIMELINE_GEOMETRY_SHOTS, 'chart-' + width + '.png') })
      }
    }
    await page.setViewportSize({ width: 366, height: 768 })
    await page.getByTestId('fixture-many-buckets').click()
    await page.waitForFunction(() => document.querySelectorAll('g.timeline-point-control').length === 1830)
    assert.equal(await page.locator('g.timeline-point-control[tabindex="0"]').count(), 5)
    assert.equal(await page.locator('.timeline-svg-scroll').evaluate(scroll => scroll.scrollWidth > scroll.clientWidth), true)
    const geometry = await renderedGeometry(page)
    assert.ok(geometry.allInBounds && geometry.sharedX && geometry.pathsMatch && geometry.labelsMatch, JSON.stringify(geometry))
    const ranges = await page.locator('g.timeline-series-created').evaluateAll(points => points.map(point => point.getAttribute('aria-label').match(/, (\d{4}-\d{2}-\d{2} \d{2}:\d{2}) to (\d{4}-\d{2}-\d{2} \d{2}:\d{2}) exclusive/)))
    assert.equal(ranges.every((range, index) => range && Date.parse(range[1]) < Date.parse(range[2]) && (index === 0 || ranges[index - 1][2] === range[1])), true)
    await point('created', 0).focus()
    for (const index of [30, 60, 90, 120, 150, 180]) { await page.keyboard.press('PageDown'); await page.waitForFunction(id => document.activeElement?.id === id, 'timeline-point-lifecycle-created-' + index) }
    for (const index of [181, 182, 183]) { await page.keyboard.press('ArrowRight'); await page.waitForFunction(id => document.activeElement?.id === id, 'timeline-point-lifecycle-created-' + index) }
    await page.keyboard.press('Enter'); await waitForRange(page, '2028-07-02T00:00:00Z')
    await page.keyboard.press('End'); await page.waitForFunction(() => document.activeElement?.id === 'timeline-point-lifecycle-created-365'); assert.equal(await point('created', 365).getAttribute('tabindex'), '0')
    await clickMarker(page, point('created', 365)); await waitForRange(page, '2028-12-31T00:00:00Z')
    await page.getByTestId('fixture-spike').click()
    await page.waitForFunction(() => document.querySelectorAll('g.timeline-point-control').length === 15)
    assert.equal(await point('created', 2).getAttribute('tabindex'), '0')
    await page.getByTestId('fixture-zero').click()
    await page.waitForFunction(() => document.getElementById('timeline-point-lifecycle-created-1')?.getAttribute('aria-label').endsWith(', 6, not selected'))
    await page.evaluate(() => new Promise(resolve => requestAnimationFrame(() => requestAnimationFrame(resolve))))
    assert.match(await point('completed', 0).getAttribute('aria-label'), /, 0, not selected$/)
    assert.ok((await renderedGeometry(page)).sharedX)
    for (const action of ['created', 'completed', 'deleted', 'archived', 'cancelled']) {
      await page.locator('g.timeline-series-' + action + '[tabindex="0"]').focus(); await page.keyboard.press('Home')
      await page.waitForFunction(id => document.activeElement?.id === id, 'timeline-point-lifecycle-' + action + '-0')
      await page.keyboard.press('Enter'); await waitForSelection(page, '01')
      assert.equal(await point(action, 0).evaluate(control => document.activeElement === control), true, action + ' retains keyboard focus')
      await page.keyboard.press('Space'); await waitForNoSelection(page)
    }
    await clickMarker(page, point('created', 0)); await waitForSelection(page, '01')
    await page.getByTestId('fixture-one-bucket').click(); await waitForNoSelection(page)
    const single = await renderedGeometry(page)
    assert.ok(single.pathsMatch && single.sharedX && single.labelsMatch && single.allInBounds, JSON.stringify(single))
    assert.equal(await page.locator('g.timeline-point-control').count(), 5)
    assert.equal(await point('created', 0).locator('.timeline-point').evaluate(marker => marker.cx.baseVal.value === marker.ownerSVGElement.getBoundingClientRect().width / 2), true)
    await clickMarker(page, point('cancelled', 0)); await waitForSelection(page, '01')
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
