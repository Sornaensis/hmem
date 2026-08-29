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

test('deterministic timeline fixture renders accessible line charts and reachable coincident targets', async () => {
  const { server, url } = await startFixtureServer()
  let browser

  try {
    browser = await chromium.launch({ headless: true })
    const context = await browser.newContext({ viewport: { width: 366, height: 768 } })
    const page = await context.newPage()
    await page.goto(url)
    await assert.deepEqual(await page.evaluate(async () => ({ cookies: document.cookie, localStorageEntries: localStorage.length, sessionStorageEntries: sessionStorage.length, cacheKeys: await caches.keys() })), { cookies: '', localStorageEntries: 0, sessionStorageEntries: 0, cacheKeys: [] })

    await assert.equal(await page.locator('.timeline-line-chart-panel').count(), 3)
    await assert.equal(await page.locator('.timeline-series-toggle').count(), 4)
    await assert.deepEqual(await page.locator('.timeline-series-toggle').evaluateAll((buttons) => buttons.map((button) => button.getAttribute('aria-pressed'))), [ 'true', 'true', 'true', 'true' ])
    await assert.equal(await page.locator('.timeline-value-table').count(), 3)
    await assert.deepEqual(await page.locator('.timeline-value-table caption').allTextContents(), [ 'Create values by UTC bucket', 'Complete values by UTC bucket', 'Delete values by UTC bucket' ])

    const styles = await page.locator('.timeline-line-chart').first().evaluate((chart) => {
      const line = chart.querySelector('.timeline-line')
      const marker = chart.querySelector('.timeline-point')
      return { lineFill: getComputedStyle(line).fill, linePointerEvents: getComputedStyle(line).pointerEvents, markerFill: getComputedStyle(marker).fill }
    })
    assert.equal(styles.lineFill, 'none')
    assert.equal(styles.linePointerEvents, 'none')
    assert.notEqual(styles.markerFill, 'none')
    const geometry = await page.locator('.timeline-line-chart').first().evaluate((chart) => {
      const width = Number(chart.getAttribute('width'))
      return Array.from(chart.querySelectorAll('.timeline-line')).map((line) => {
        const key = Array.from(line.classList).find((className) => className.startsWith('timeline-series-'))
        const markers = Array.from(chart.querySelectorAll(`g.${key} .timeline-point`))
        const first = markers[0].getAttribute('cx')
        const last = markers.at(-1).getAttribute('cx')
        return {
          first: Number(first),
          last: Number(last),
          inBounds: markers.every((marker) => {
            const x = Number(marker.getAttribute('cx'))
            return x >= 11 && x <= width - 11
          }),
          pathMatchesMarkers: line.getAttribute('d').startsWith(`M ${first} `) && markers.every((marker) => line.getAttribute('d').includes(`${marker.getAttribute('cx')} ${marker.getAttribute('cy')}`)),
        }
      })
    })
    assert.equal(geometry.length, 4)
    assert.equal(geometry.every((series) => series.inBounds && series.pathMatchesMarkers), true)
    const narrowTargets = await page.locator('.timeline-line-chart').first().evaluate((chart) => {
      const width = Number(chart.getAttribute('width'))
      const controls = Array.from(chart.querySelectorAll('g.timeline-point-control'))
      const boxes = controls.map((control) => ({
        label: control.getAttribute('aria-label'),
        box: control.getBBox(),
      }))
      const buckets = new Map()
      for (const entry of boxes) {
        const bucket = entry.label.split(', ')[2]
        buckets.set(bucket, [ ...(buckets.get(bucket) ?? []), entry.box ])
      }
      return {
        allVisibleTargetsInBounds: boxes.every(({ box }) => box.width >= 22 && box.height >= 22 && box.x >= 0 && box.x + box.width <= width),
        coincidentTargetsDoNotOverlap: Array.from(buckets.values()).every((bucketBoxes) => {
          const ordered = bucketBoxes.sort((left, right) => left.x - right.x)
          return ordered.every((box, index) => index === 0 || ordered[index - 1].x + ordered[index - 1].width <= box.x)
        }),
      }
    })
    assert.deepEqual(narrowTargets, { allVisibleTargetsInBounds: true, coincidentTargetsDoNotOverlap: true })
    await assert.match(await page.getByRole('button', { name: /^Create, Projects, Jan 1/ }).getAttribute('aria-label'), /, 1, not selected$/)
    await assert.match(await page.getByRole('button', { name: /^Create, Tasks, Jan 1/ }).getAttribute('aria-label'), /, 3, not selected$/)
    await assert.match(await page.getByRole('button', { name: /^Create, Subtasks, Jan 1/ }).getAttribute('aria-label'), /, 2, not selected$/)
    await assert.match(await page.getByRole('button', { name: /^Create, Observations, Jan 1/ }).getAttribute('aria-label'), /, 0, not selected$/)

    const desktopContext = await browser.newContext({ viewport: { width: 1440, height: 900 } })
    const desktop = await desktopContext.newPage()
    await desktop.goto(url)
    await assert.equal(await desktop.locator('.timeline-line-chart-panel').count(), 3)
    await assert.equal(await desktop.locator('.timeline-line-chart').first().evaluate((chart) => getComputedStyle(chart.querySelector('.timeline-line')).fill), 'none')
    await desktopContext.close()

    const tasksToggle = page.getByRole('button', { name: 'Tasks', exact: true })
    await tasksToggle.focus()
    await assert.equal(await tasksToggle.evaluate((button) => document.activeElement === button && getComputedStyle(button).outlineStyle !== 'none'), true)
    await page.keyboard.press('Enter')
    await page.waitForFunction(() => Array.from(document.querySelectorAll('.timeline-series-toggle')).find((button) => button.textContent?.trim() === 'Tasks')?.getAttribute('aria-pressed') === 'false')
    await page.keyboard.press('Space')
    await page.waitForFunction(() => Array.from(document.querySelectorAll('.timeline-series-toggle')).find((button) => button.textContent?.trim() === 'Tasks')?.getAttribute('aria-pressed') === 'true')

    const tableBucket = page.getByRole('button', { name: /Show events for Jan 1/ }).first()
    await tableBucket.focus()
    await assert.equal(await tableBucket.evaluate((button) => document.activeElement === button && getComputedStyle(button).outlineStyle !== 'none'), true)
    await page.keyboard.press('Space')
    await waitForSelection(page, '01')
    await page.keyboard.press('Enter')
    await waitForNoSelection(page)

    await page.getByTestId('fixture-zero').click()
    await waitForNoSelection(page)
    await page.waitForFunction(() => Array.from(document.querySelectorAll('[role="button"]')).some((point) => point.getAttribute('aria-label')?.endsWith(', 0, not selected')))
    await page.getByTestId('fixture-spike').click()
    await page.waitForFunction(() => Array.from(document.querySelectorAll('[role="button"]')).some((point) => point.getAttribute('aria-label')?.endsWith(', 9, not selected')))

    await page.getByRole('button', { name: /Show events for Jan 2/ }).first().click()
    await waitForSelection(page, '02')

    await page.getByTestId('fixture-spike').click()
    await waitForNoSelection(page)
    const createProjectJan2 = page.getByRole('button', { name: /Create, Projects, Jan 2/ })
    await createProjectJan2.click()
    await waitForSelection(page, '02')
    await createProjectJan2.click()
    await waitForNoSelection(page)
    const createObservationJan3 = page.getByRole('button', { name: /Create, Observations, Jan 3/ })
    await createObservationJan3.click()
    await waitForSelection(page, '03')
    await assert.match(await createObservationJan3.getAttribute('aria-label'), /2026-01-03T00:00:00Z to 2026-01-04T00:00:00Z exclusive/)

    // Complete values are intentionally all zero; these independently named, coincident points
    // must each be pointer reachable rather than intercepting one another.
    await page.getByTestId('fixture-spike').click()
    await waitForNoSelection(page)
    await page.getByRole('button', { name: /Complete, Projects, Jan 1/ }).click()
    await waitForSelection(page, '01')
    await page.getByRole('button', { name: /Complete, Subtasks, Jan 2/ }).click()
    await waitForSelection(page, '02')

    await page.getByTestId('fixture-spike').click()
    const deleteTaskJan2 = page.getByRole('button', { name: /Delete, Tasks, Jan 2/ })
    await deleteTaskJan2.focus()
    await assert.equal(await deleteTaskJan2.evaluate((point) => document.activeElement === point && getComputedStyle(point).outlineStyle !== 'none'), true)
    await page.evaluate(() => window.scrollTo(0, 240))
    const scrollBeforeSpace = await page.evaluate(() => window.scrollY)
    await page.keyboard.press('Enter')
    await waitForSelection(page, '02')
    await assert.equal(await page.getByTestId('timeline-fixture-card-count').textContent(), '1')
    await assert.equal(await page.getByTestId('timeline-fixture-card-inside').count(), 1)
    await assert.equal(await page.getByTestId('timeline-fixture-card-outside').count(), 0)
    await page.keyboard.press('Space')
    await waitForNoSelection(page)
    await assert.equal(await page.evaluate(() => window.scrollY), scrollBeforeSpace)
    await assert.equal(await page.getByTestId('timeline-fixture-card-count').textContent(), '2')

    await page.getByRole('button', { name: 'Projects', exact: true }).click()
    await page.waitForFunction(() => document.querySelector('.timeline-series-toggle')?.getAttribute('aria-pressed') === 'false')
    await assert.equal(await page.getByRole('button', { name: 'Projects', exact: true }).getAttribute('aria-pressed'), 'false')

    await page.getByTestId('fixture-many-buckets').click()
    await page.waitForFunction(() => Number(document.querySelector('.timeline-line-chart')?.getAttribute('width')) > 7000)
    const manyScroll = page.locator('.timeline-svg-scroll').first()
    await assert.equal(await manyScroll.evaluate((scroll) => scroll.scrollWidth > scroll.clientWidth), true)
    const manyObservationPoints = page.locator('.timeline-line-chart').first().locator('g.timeline-series-observations')
    await assert.equal(await manyObservationPoints.count(), 366)
    await assert.equal(await page.locator('.timeline-line-chart g.timeline-point-control[tabindex="0"]').count(), 12)
    const manyRanges = await manyObservationPoints.evaluateAll((points) => points.map((point) => point.getAttribute('aria-label')).map((label) => {
      const match = label.match(/, (\d{4}-\d{2}-\d{2}T00:00:00Z) to (\d{4}-\d{2}-\d{2}T00:00:00Z) exclusive/)
      return { start: match?.[1], end: match?.[2] }
    }))
    assert.equal(manyRanges.every((range, index) => Number.isFinite(Date.parse(range.start)) && Number.isFinite(Date.parse(range.end)) && Date.parse(range.start) < Date.parse(range.end) && (index === 0 || manyRanges[index - 1].end === range.start)), true)
    const manyGeometry = await page.locator('.timeline-line-chart').first().evaluate((chart) => {
      const width = Number(chart.getAttribute('width'))
      return Array.from(chart.querySelectorAll('.timeline-line')).every((line) => {
        const key = Array.from(line.classList).find((className) => className.startsWith('timeline-series-'))
        const markers = Array.from(chart.querySelectorAll(`g.${key} .timeline-point`))
        return [ 0, 183, 365 ].every((index) => {
          const marker = markers[index]
          const x = Number(marker.getAttribute('cx'))
          return x >= 11 && x <= width - 11 && line.getAttribute('d').includes(`${marker.getAttribute('cx')} ${marker.getAttribute('cy')}`)
        })
      })
    })
    assert.equal(manyGeometry, true)

    const manyTargetGeometry = await page.locator('.timeline-line-chart').first().evaluate((chart) => {
      const width = Number(chart.getAttribute('width'))
      const series = [ 'projects', 'tasks', 'subtasks', 'observations' ]
      const buckets = series.map((key) => Array.from(chart.querySelectorAll(`g.timeline-series-${key}`)))
      return [ 0, 183, 365 ].every((index) => {
        const boxes = buckets
          .map((points) => points[index]?.getBBox())
          .filter(Boolean)
          .sort((left, right) => left.x - right.x)
        return boxes.every((box, boxIndex) => box.width >= 22 && box.height >= 22 && box.x >= 0 && box.x + box.width <= width && (boxIndex === 0 || boxes[boxIndex - 1].x + boxes[boxIndex - 1].width <= box.x))
      })
    })
    assert.equal(manyTargetGeometry, true)

    const equalYCrossBucketTargets = await page.locator('.timeline-line-chart').nth(1).evaluate((chart) => {
      const width = Number(chart.getAttribute('width'))
      const boxes = Array.from(chart.querySelectorAll('g.timeline-point-control'))
        .map((point) => point.getBBox())
        .sort((left, right) => left.x - right.x)
      return boxes.length === 1464 && boxes.every((box, index) => box.width >= 22 && box.height >= 22 && box.x >= 0 && box.x + box.width <= width && (index === 0 || boxes[index - 1].x + boxes[index - 1].width <= box.x))
    })
    assert.equal(equalYCrossBucketTargets, true)

    const firstRovingPoint = page.locator('#timeline-point-created-observations-0')
    await firstRovingPoint.focus()
    for (const target of [ 30, 60, 90, 120, 150, 180 ]) {
      await page.keyboard.press('PageDown')
      await page.waitForFunction((id) => document.activeElement?.id === id, `timeline-point-created-observations-${target}`)
    }
    await assert.equal(await page.locator('#timeline-point-created-observations-180').evaluate((point) => document.activeElement === point && point.getAttribute('tabindex') === '0'), true)
    for (const target of [ 181, 182, 183 ]) {
      await page.keyboard.press('ArrowRight')
      await page.waitForFunction((id) => document.activeElement?.id === id, `timeline-point-created-observations-${target}`)
    }
    await assert.equal(await page.locator('#timeline-point-created-observations-183').evaluate((point) => document.activeElement === point && point.getAttribute('tabindex') === '0'), true)
    await page.keyboard.press('Enter')
    await waitForRange(page, '2028-07-02T00:00:00Z')
    await page.keyboard.press('End')
    await assert.equal(await page.locator('#timeline-point-created-observations-365').evaluate((point) => document.activeElement === point && point.getAttribute('tabindex') === '0'), true)

    const firstManyPoint = manyObservationPoints.nth(0)
    const middleManyPoint = manyObservationPoints.nth(183)
    const lastManyPoint = manyObservationPoints.nth(365)
    const firstManyLabel = await firstManyPoint.getAttribute('aria-label')
    await firstManyPoint.click()
    await waitForRange(page, firstManyLabel.match(/, (\d{4}-\d{2}-\d{2}T00:00:00Z) to/)[1])
    const middleManyLabel = await middleManyPoint.getAttribute('aria-label')
    await middleManyPoint.click()
    await waitForRange(page, middleManyLabel.match(/, (\d{4}-\d{2}-\d{2}T00:00:00Z) to/)[1])
    await manyScroll.evaluate((scroll) => { scroll.scrollLeft = scroll.scrollWidth })
    await lastManyPoint.scrollIntoViewIfNeeded()
    await assert.equal(await lastManyPoint.evaluate((point) => {
      const pointBox = point.getBoundingClientRect()
      const scrollBox = point.closest('.timeline-svg-scroll').getBoundingClientRect()
      return pointBox.width >= 22 && pointBox.height >= 22 && pointBox.left >= scrollBox.left && pointBox.right <= scrollBox.right
    }), true)
    const lastManyLabel = await lastManyPoint.getAttribute('aria-label')
    await lastManyPoint.click()
    await waitForRange(page, lastManyLabel.match(/, (\d{4}-\d{2}-\d{2}T00:00:00Z) to/)[1])

    await page.getByTestId('fixture-spike').click()
    await page.waitForFunction(() => document.querySelectorAll('.timeline-line-chart').item(0)?.querySelectorAll('g.timeline-point-control').length === 12)
    await assert.equal(await page.locator('.timeline-line-chart g.timeline-point-control[tabindex="0"]').count(), 12)
    await assert.equal(await page.locator('#timeline-point-created-observations-2').getAttribute('tabindex'), '0')

    await page.getByTestId('fixture-graph-error').click()
    await page.waitForFunction(() => document.body.textContent?.includes('Deterministic graph error'))
    await assert.equal(await page.getByTestId('timeline-fixture-card-count').textContent(), '2')
    await assert.equal(await page.getByTestId('timeline-fixture-card-inside').count(), 1)
    await assert.equal(await page.getByTestId('timeline-fixture-card-outside').count(), 1)
    await assert.equal(await page.getByTestId('timeline-fixture-card-inside').textContent(), 'Fixture inside event')
    await assert.equal(await page.getByTestId('timeline-fixture-card-outside').textContent(), 'Fixture outside event')

    await page.getByTestId('fixture-empty').click()
    await page.waitForFunction(() => document.body.textContent?.includes('No lifecycle activity in this date range.'))
  } finally {
    await browser?.close()
    await new Promise((resolveServer, rejectServer) => server.close((error) => error ? rejectServer(error) : resolveServer()))
    await unlink(fixtureBundle)
  }
})
