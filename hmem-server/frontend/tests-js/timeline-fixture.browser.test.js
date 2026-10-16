import assert from 'node:assert/strict'
import { mkdir, readFile, unlink } from 'node:fs/promises'
import { createServer } from 'node:http'
import { join, resolve } from 'node:path'
import test from 'node:test'
import { chromium } from '@playwright/test'

const fixtureBundle = resolve(process.cwd(), '.timeline-fixture-test.js')

async function startFixtureServer() {
  const [ bundle, stylesheet, timestampBridge, entrypoint, fixtureHtml ] = await Promise.all([
    readFile(fixtureBundle),
    readFile(resolve(process.cwd(), 'src/style.css')),
    readFile(resolve(process.cwd(), 'src/timestamp-display.js')),
    readFile(resolve(process.cwd(), 'src/timeline-fixture.js')),
    readFile(resolve(process.cwd(), 'timeline-fixture.html'), 'utf8'),
  ])
  const server = createServer((request, response) => {
    if (request.url === '/TimelineFixture.js') {
      response.writeHead(200, { 'content-type': 'application/javascript; charset=utf-8' })
      response.end(bundle)
      return
    }

    if (request.url === '/src/timestamp-display.js') {
      response.writeHead(200, { 'content-type': 'application/javascript; charset=utf-8' })
      response.end(timestampBridge)
      return
    }

    if (request.url === '/src/style.css') {
      response.writeHead(200, { 'content-type': 'text/css; charset=utf-8' })
      response.end(stylesheet)
      return
    }

    if (request.url === '/src/timeline-fixture.js') { response.writeHead(200, { 'content-type': 'application/javascript' }); response.end(entrypoint); return }
    // The compiler import is replaced only by a module shim around the exact
    // compiled Elm program; HTML and browser bootstrap come from the real fixture.
    if (request.url === '/src/TimelineFixture.elm') { response.writeHead(200, { 'content-type': 'application/javascript' }); response.end('export const Elm = window.Elm'); return }
    response.writeHead(200, { 'content-type': 'text/html; charset=utf-8' })
    response.end(fixtureHtml.replace('<script type="module"', '<script src="/TimelineFixture.js"></script><script type="module"'))
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
    return selection?.dataset.since === selectedDay
  }, since)
}

async function waitForNoSelection(page) {
  await page.waitForFunction(() => document.querySelector('[data-testid="timeline-fixture-selection"]')?.textContent === 'No bucket selected')
}

async function waitForFormattedPoints(page) {
  await page.waitForFunction(() => [...document.querySelectorAll('g.timeline-point-control')]
    .every(point => !point.getAttribute('aria-label').includes('…')))
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
      const control = controls.find(control => control.getAttribute('aria-label').includes(', ' + label.textContent + ' to '))
      const marker = control?.querySelector('.timeline-point')
      return marker && near(center(label, label.x.baseVal[0].value, 0).x, center(marker, marker.cx.baseVal.value, 0).x)
    })
    const labels = [...chart.querySelectorAll('.timeline-chart-axis,.timeline-chart-x-axis')].map(label => {
      const bounds = label.getBoundingClientRect(), transform = label.getScreenCTM()
      return { text: label.textContent, width: bounds.width, height: bounds.height, a: transform.a, d: transform.d }
    })
    const allInBounds = controls.every(control => { const box = control.getBBox(); return box.width >= 22 && box.height >= 22 && box.x >= 0 && box.x + box.width <= chart.getBoundingClientRect().width })
    const chartBounds = chart.getBoundingClientRect()
    const axisBounds = [...chart.querySelectorAll('.timeline-chart-x-axis')].map(label => label.getBoundingClientRect())
    const axisSeparated = axisBounds.every((box, index) => index === 0 || axisBounds[index - 1].right + 4 <= box.left)
    const axisInBounds = axisBounds.every(box => box.left >= chartBounds.left && box.right <= chartBounds.right && box.top >= chartBounds.top && box.bottom <= chartBounds.bottom)
    return { pathsMatch, sharedX, labelsMatch, allInBounds, axisSeparated, axisInBounds, labels, width: chartBounds.width, height: chartBounds.height }
  })
}

async function verifyOpenInventory(page) {
  const active = page.locator('.timeline-open-active')
  const bar = index => page.locator('#timeline-point-open-items-' + index)
  const waitActive = index => page.waitForFunction(expected => document.querySelector('.timeline-open-active')?.dataset.bucketIndex === String(expected), index)
  const leave = async () => { await page.mouse.move(5, 5) }
  const shot = async name => {
    if (!process.env.HMEM_TIMELINE_GEOMETRY_SHOTS) return
    await mkdir(process.env.HMEM_TIMELINE_GEOMETRY_SHOTS, { recursive: true })
    await page.screenshot({ path: join(process.env.HMEM_TIMELINE_GEOMETRY_SHOTS, name + '.png'), fullPage: true })
  }
  const hoverBar = async index => {
    await bar(index).scrollIntoViewIfNeeded()
    const box = await bar(index).locator('.timeline-open-hitarea').boundingBox()
    await page.mouse.move(box.x + box.width / 2, box.y + box.height - 4)
    await waitActive(index)
  }
  await page.setViewportSize({ width: 1440, height: 1000 })
  await leave()
  assert.equal(await page.locator('.timeline-open-idle').count(), 3)
  assert.equal(await page.locator('.timeline-open-idle .timeline-open-segment').count(), 6)
  assert.equal(await page.locator('.timeline-open-idle').first().evaluate(node => getComputedStyle(node).opacity), '0.65')
  assert.equal(await active.count(), 0)
  const alignment = await page.locator('.timeline-line-chart').evaluate(chart => [...chart.querySelectorAll('.timeline-open-idle')].every((bar, index) => {
    const project = bar.querySelector('.timeline-open-projects'), task = bar.querySelector('.timeline-open-tasks')
    const point = chart.querySelector('#timeline-point-lifecycle-created-' + index + ' .timeline-point')
    const pb = project.getBoundingClientRect(), tb = task.getBoundingClientRect(), cb = point.getBoundingClientRect()
    return Math.abs(pb.x + pb.width / 2 - cb.x - cb.width / 2) < 0.1 && Math.abs(tb.bottom - pb.top) < 0.1 && pb.height >= 0 && tb.height >= 0
  }))
  assert.ok(alignment, 'segments stack at the shared line bucket center')
  await shot('open-idle-desktop')
  let requests = 0
  const countRequest = () => { requests++ }
  page.on('request', countRequest)
  await hoverBar(1)
  assert.equal(await active.evaluate(node => getComputedStyle(node).opacity), '1')
  assert.equal(await active.evaluate(node => getComputedStyle(node).pointerEvents), 'none')
  assert.equal(await active.locator('.timeline-open-segment').count(), 2)
  assert.equal(await active.evaluate(node => node === node.ownerSVGElement.lastElementChild), true, 'whole active paint is above all paths and controls')
  const projects = await page.locator('.timeline-open-idle[data-bucket-index="1"] .timeline-open-projects').boundingBox()
  const tasks = await page.locator('.timeline-open-idle[data-bucket-index="1"] .timeline-open-tasks').boundingBox()
  for (const y of [projects.y + projects.height - 2, projects.y - 1, projects.y + 1, tasks.y + 2]) {
    await page.mouse.move(projects.x + projects.width / 2, y)
    await waitActive(1)
  }
  const point = page.locator('#timeline-point-lifecycle-deleted-1')
  const marker = await point.locator('.timeline-point').boundingBox()
  await page.mouse.move(marker.x + marker.width / 2, marker.y + marker.height / 2)
  await waitActive(1)
  assert.equal(await page.evaluate(({ x, y }) => document.elementFromPoint(x, y)?.closest('.timeline-point-control')?.id, { x: marker.x + marker.width / 2, y: marker.y + marker.height / 2 }), 'timeline-point-lifecycle-deleted-1', 'foreground bar does not intercept the lifecycle point')
  assert.match(await point.locator('title').textContent(), /Lifecycle activity — Delete/)
  await clickMarker(page, point); await waitForSelection(page, '02')
  assert.match(await page.locator('.timeline-open-tooltip').textContent(), /90 open projects; 270 open tasks \+ subtasks; 360 open items total.*As of 2026-01-03 00:00/)
  await shot('open-hover-desktop')
  assert.equal(requests, 0, 'hover and chart selection in the fixture add no transport')
  page.off('request', countRequest)
  await leave()
  await bar(0).focus(); await waitActive(0)
  assert.equal(await bar(0).evaluate(node => document.activeElement === node && getComputedStyle(node).outlineStyle !== 'none'), true)
  await shot('open-focus-desktop')
  await hoverBar(1)
  await page.getByRole('button', { name: 'Tasks', exact: true }).focus()
  await waitActive(1) // Blur clears focus without clearing a different hovered bucket.
  await leave(); await active.waitFor({ state: 'detached' })
  await bar(0).focus(); await waitActive(0)
  await page.keyboard.press('ArrowRight'); await waitActive(1)
  assert.equal(await bar(1).getAttribute('tabindex'), '0')
  await page.keyboard.press('End'); await waitActive(2)
  await page.keyboard.press('Enter'); await waitForSelection(page, '03')
  await page.keyboard.press('Space'); await waitForNoSelection(page)
  await page.keyboard.press('Home'); await waitActive(0)
  await page.getByRole('button', { name: 'Tasks', exact: true }).focus()
  await active.waitFor({ state: 'detached' })
  const axis = await page.locator('.timeline-open-axis').allTextContents()
  for (const action of ['Create', 'Complete', 'Delete', 'Archive', 'Cancel']) await page.getByRole('button', { name: action, exact: true }).click()
  await page.waitForFunction(() => document.querySelectorAll('.timeline-line').length === 0)
  assert.equal(await page.locator('.timeline-line').count(), 0)
  assert.equal(await page.locator('.timeline-open-idle').count(), 3)
  assert.deepEqual(await page.locator('.timeline-open-axis').allTextContents(), axis)
  await page.getByText('Open work by bucket', { exact: true }).click()
  assert.match(await page.locator('.timeline-open-table').textContent(), /Open projects.*Open tasks \+ subtasks.*Total.*90.*270.*360/s)
  await page.getByText('Open work by bucket', { exact: true }).click()
  for (const action of ['Create', 'Complete', 'Delete', 'Archive', 'Cancel']) await page.getByRole('button', { name: action, exact: true }).click()
  await page.waitForFunction(() => document.querySelectorAll('.timeline-line').length === 5)
  await page.setViewportSize({ width: 366, height: 900 })
  await leave()
  await shot('open-narrow')
  const narrowScroll = page.locator('.timeline-svg-scroll')
  await narrowScroll.evaluate(node => { node.scrollLeft = node.scrollWidth - node.clientWidth })
  assert.equal(await narrowScroll.evaluate(node => node.scrollLeft > 0), true)
  assert.equal(await narrowScroll.evaluate(node => {
    const viewport = node.getBoundingClientRect()
    return [...node.querySelectorAll('.timeline-open-axis,.timeline-axis-title-right')].every(label => {
      const box = label.getBoundingClientRect()
      return box.left >= viewport.left && box.right <= viewport.right
    })
  }), true, 'narrow horizontal scroll reaches all RHS labels and its title')
  for (const fixture of ['stock-spike', 'line-spike']) {
    await page.getByTestId('fixture-' + fixture).click()
    await page.waitForFunction(() => document.querySelectorAll('.timeline-open-idle').length === 2)
    await waitForFormattedPoints(page)
    const expected = fixture === 'stock-spike' ? '4000000' : '3'
    await page.waitForFunction(value => [...document.querySelectorAll('.timeline-open-axis')].some(label => label.textContent === value), expected)
    assert.ok((await page.locator('.timeline-open-axis').allTextContents()).includes(expected))
    assert.equal(await page.locator('.timeline-open-idle').evaluateAll(bars => bars.some(bar => /NaN|Infinity/.test(bar.innerHTML))), false)
    assert.equal(await page.locator('.timeline-open-axis').evaluateAll(labels => labels.every(label => {
      const box = label.getBoundingClientRect(), chart = label.ownerSVGElement.getBoundingClientRect()
      return box.left >= chart.left && box.right <= chart.right
    })), true, 'RHS integer labels fit the reserved plot margin')
  }
  await page.getByTestId('fixture-gaps').click()
  await page.waitForFunction(() => document.querySelectorAll('.timeline-open-gap').length === 2)
  assert.equal(await page.locator('.timeline-open-idle').count(), 1)
  await leave(); await bar(1).focus()
  assert.match(await bar(1).getAttribute('aria-label'), /history unavailable/)
  assert.equal(await active.count(), 0)
  await bar(2).focus()
  assert.match(await bar(2).getAttribute('aria-label'), /Future bucket; open work is not projected/)
  await page.getByTestId('fixture-zero').click()
  await page.waitForFunction(() => document.querySelectorAll('.timeline-open-idle').length === 3)
  assert.equal(await page.locator('.timeline-open-idle .timeline-open-segment').evaluateAll(parts => parts.every(part => Number(part.getAttribute('height')) === 0)), true)
  assert.deepEqual(await page.locator('.timeline-open-axis').allTextContents(), ['0'])
  await leave(); await bar(0).focus()
  assert.match(await bar(0).getAttribute('aria-label'), /0 open projects; 0 open tasks \+ subtasks; 0 open items total/)
  await page.getByTestId('fixture-one-bucket').click()
  await page.waitForFunction(() => document.querySelectorAll('.timeline-open-control').length === 1)
  assert.equal(await active.count(), 0, 'data replacement retires hover and focus paint')
  await page.getByTestId('fixture-empty').click()
  await page.locator('.timeline-line-chart').waitFor({ state: 'detached' })
  assert.equal(await page.locator('.timeline-line-chart').count(), 0)
  await page.reload()
  await waitForFormattedPoints(page)
}

test('single lifecycle chart preserves counts, accessible controls and shared bucket geometry', async () => {
  const { server, url } = await startFixtureServer()
  let browser
  try {
    browser = await chromium.launch({ headless: true })
    const context = await browser.newContext({ viewport: { width: 366, height: 768 }, timezoneId: 'UTC' })
    const page = await context.newPage()
    await page.goto(url)
    await waitForFormattedPoints(page)
    assert.equal(await page.locator('.timeline-line-chart-panel').count(), 1)
    assert.equal(await page.locator('.timeline-series-toggle').count(), 9)
    assert.equal(await page.locator('.timeline-value-table').count(), 0)
    assert.equal(await page.getByLabel('Time window').inputValue(), '30')
    assert.equal(await page.locator('.timeline-graph-control select').nth(1).inputValue(), 'week')
    assert.equal(await page.locator('input[type="date"]').count(), 0)
    await verifyOpenInventory(page)
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
      assert.ok(geometry.axisSeparated && geometry.axisInBounds, 'localized axis text overlaps or clips: ' + JSON.stringify(geometry))
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
    assert.equal(await page.locator('.timeline-open-idle').count(), 366)
    assert.equal(await page.locator('.timeline-open-control[tabindex="0"]').count(), 1)
    await waitForFormattedPoints(page)
    assert.equal(await page.locator('g.timeline-point-control[tabindex="0"]').count(), 5)
    assert.equal(await page.locator('.timeline-svg-scroll').evaluate(scroll => scroll.scrollWidth > scroll.clientWidth), true)
    const geometry = await renderedGeometry(page)
    assert.ok(geometry.allInBounds && geometry.sharedX && geometry.pathsMatch && geometry.labelsMatch, JSON.stringify(geometry))
    assert.ok(geometry.axisSeparated && geometry.axisInBounds, 'many-bucket axis text overlaps or clips: ' + JSON.stringify(geometry))
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
    await waitForFormattedPoints(page)
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

    // Every new page begins with preloaded buckets and a formatting Cmd in init.
    // Awaiting real labels proves the first request reaches the installed bridge.
    for (const [timezoneId, start, end] of [
      ['Europe/Copenhagen', '2026-01-01 01:00', '2026-01-02 01:00'],
      ['America/Los_Angeles', '2025-12-31 16:00', '2026-01-01 16:00'],
      ['Asia/Kathmandu', '2026-01-01 05:45', '2026-01-02 05:45']
    ]) {
      const zoned = await browser.newContext({ timezoneId, viewport: { width: 366, height: 768 } })
      try {
        const localPage = await zoned.newPage()
        await localPage.goto(url)
        await waitForFormattedPoints(localPage)
        const firstPoint = localPage.locator('#timeline-point-lifecycle-created-0')
        assert.equal(await localPage.locator('.timeline-chart-x-axis').first().textContent(), start)
        assert.ok((await firstPoint.getAttribute('aria-label')).includes(`${start} to ${end} exclusive, 6, not selected`))
        assert.ok((await firstPoint.locator('title').textContent()).includes(`[${start}, ${end})`))
        assert.equal((await firstPoint.getAttribute('aria-label')).includes('Jan 1'), false)
        const geometry = await renderedGeometry(localPage)
        assert.ok(geometry.pathsMatch && geometry.sharedX && geometry.labelsMatch && geometry.allInBounds)
        assert.ok(geometry.axisSeparated && geometry.axisInBounds, timezoneId + ' axis text overlaps or clips')
        await clickMarker(localPage, firstPoint)
        await waitForSelection(localPage, '01')
        assert.equal(await localPage.getByTestId('timeline-fixture-card-count').textContent(), '0')
        assert.match(await firstPoint.getAttribute('aria-label'), /, 6, selected$/)
        await localPage.reload()
        await waitForFormattedPoints(localPage)
        assert.equal(await localPage.locator('.timeline-chart-x-axis').first().textContent(), start)
        if (timezoneId === 'Europe/Copenhagen') {
          await localPage.getByTestId('fixture-dst').click()
          await localPage.waitForFunction(() => document.querySelectorAll('g.timeline-point-control').length === 10)
          await waitForFormattedPoints(localPage)
          assert.ok((await localPage.locator('#timeline-point-lifecycle-created-0').getAttribute('aria-label'))
            .includes('2026-03-29 01:00 to 2026-03-30 02:00 exclusive'))
          assert.ok((await localPage.locator('#timeline-point-lifecycle-created-1').getAttribute('aria-label'))
            .includes('2026-10-25 02:00 to 2026-10-26 01:00 exclusive'))
        }
      } finally {
        await zoned.close()
      }
    }
    assert.equal(await page.getByTestId('timeline-fixture-card-inside').textContent(), 'Fixture inside event')
    assert.equal(await page.getByTestId('timeline-fixture-card-outside').textContent(), 'Fixture outside event')
    await page.getByTestId('fixture-empty').click(); await page.getByText('No lifecycle activity in this date range.', { exact: true }).waitFor()
  } finally {
    await browser?.close()
    await new Promise((resolveServer, rejectServer) => server.close(error => error ? rejectServer(error) : resolveServer()))
    await unlink(fixtureBundle)
  }
})
