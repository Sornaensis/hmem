import { generateFixture, paginate, queryObservations } from './fixtures.mjs'
import { assertFiveSamples, median, nearestRankP95 } from './contracts.mjs'

export const OBSERVATION_SCALING_CONTRACT = Object.freeze({
  revision: 'observation-scaling.v1', paths: ['src/Shared.elm', 'src/Observation-0.elm'],
  pageSize: 50, largeLoaded: 150, boundaryBytes: 512 * 1024,
  traceCaptureOptions: Object.freeze({ screenshots: false, snapshots: true, sources: false }),
  localEndpoint: 'activation/input through two animation frames, without waiting for HTTP',
  matchPageEndpoint: 'activation through expected loaded count and transport quiescence',
  researchTriggers: { activeEditorHeapBytes: 64 * 1024 * 1024, observationLiveFollowUps: 12 }
})

export function observationTraceCaptureOptions(scaling) {
  return scaling ? OBSERVATION_SCALING_CONTRACT.traceCaptureOptions : { screenshots: true, snapshots: true, sources: false }
}

export function traceAdmissionReceipt(sizeBytes, limitBytes, captureOptions) {
  if (!Number.isSafeInteger(sizeBytes) || sizeBytes < 0) throw new Error('Invalid actual trace size')
  return { sizeBytes, limitBytes, captureOptions, capturedRun: 'large sample 5/5 after two warmups', withinLimit: sizeBytes <= limitBytes }
}

// An overlay leaves historical generators/digests and their retained receipts intact.
export function generateObservationScalingFixture(size) {
  const fixture = generateFixture(size)
  fixture.observations = fixture.observations.map((observation, index, all) => {
    const subjects = [
      { subject_kind: 'file', subject: `src/Observation-${index % 32}.elm` },
      { subject_kind: 'glob', subject: 'src/**/*.elm' },
      { subject_kind: 'file', subject: 'src/Shared.elm' }
    ]
    const prefix = observation.content + '\nObserved behavior: multiline evidence remains plain text.\n'
    const content = index === all.length - 1
      ? prefix + 'x'.repeat(OBSERVATION_SCALING_CONTRACT.boundaryBytes - Buffer.byteLength(prefix))
      : prefix + 'Details: ordered paths, immutable provenance, and a bounded preview.\n'.repeat(30)
    return { ...observation, subjects, subject_kind: subjects[0].subject_kind, subject: subjects[0].subject, content }
  })
  fixture.observationScaling = { ...OBSERVATION_SCALING_CONTRACT, boundaryId: fixture.observations.at(-1).id }
  return fixture
}

export function queryObservationScalingMatches(fixture, query) {
  if (!fixture.observationScaling) throw new Error('Observation match overlay is required')
  if (!Array.isArray(query.paths) || !query.paths.length || query.paths.some(value => !/^src\/(?:[A-Za-z0-9_.-]+\/)*[A-Za-z0-9_.-]+\.elm$/.test(value) || value.split('/').some(part => part === '.' || part === '..'))) throw new Error('Unsupported concrete scaling path')
  const candidates = []
  // Reuse existing ranking/filter semantics across the complete backing collection.
  for (let offset = 0; ; offset += 200) {
    const page = queryObservations(fixture, { query: query.query, gitSha: query.git_sha, offset, limit: 200 })
    candidates.push(...page.items)
    if (!page.has_more) break
  }
  const matches = candidates.flatMap(observation => {
    const path_matches = query.paths.flatMap(path => {
      const matched_subjects = observation.subjects.filter(subject => {
        if (query.subject_kind && subject.subject_kind !== query.subject_kind) return false
        if (subject.subject_kind === 'file') return subject.subject === path
        if (subject.subject !== 'src/**/*.elm') throw new Error('Unsupported scaling glob: ' + subject.subject)
        return /^src\/(?:[^/]+\/)*[^/]+\.elm$/.test(path)
      })
      return matched_subjects.length ? [{ path, matched_subjects }] : []
    })
    return path_matches.length ? [{ observation, path_matches }] : []
  })
  return paginate(matches, query.offset, query.limit)
}

export function observationScalingFrames(fixture) {
  return fixture.liveFrames.map((frame, index) => ({ ...frame, event: { ...frame.event,
    event_id: 'observation-scaling-' + String(index + 1).padStart(3, '0'),
    entity: { type: 'observation', id: fixture.observationScaling.boundaryId, action: 'updated' },
    invalidations: [{ kind: 'entity', target: 'observation:' + fixture.observationScaling.boundaryId }]
  } }))
}

export function aggregateObservationScaling(runs) {
  if (!runs.some(run => run.observationScaling)) return null
  const scenarios = runs.map(run => run.observationScaling)
  if (scenarios.some(value => !value || !value.pages.length)) throw new Error('Every scaling sample must contain completed Match paging')
  const rawMs = assertFiveSamples(scenarios.map(value => Math.max(...value.pages.map(page => page.ms))), 'ordered Match load more')
  return { contract: OBSERVATION_SCALING_CONTRACT,
    matchLoadMore: { rawMs, medianMs: median(rawMs), p95Ms: nearestRankP95(rawMs), requestCounts: scenarios.map(value => Math.max(...value.pages.map(page => page.count))), pages: scenarios.map(value => value.pages) },
    research: { activeEditorHeapBytes: scenarios.map(value => value.activeEditorHeapBytes), live: scenarios.map(value => value.live), triggers: scenarios.map(value => value.researchTriggers) }
  }
}

export function observationScalingMetrics(summary, loadBudget, comparableEnvironment) {
  if (!summary) return []
  const load = summary.matchLoadMore, requests = Math.max(...load.requestCounts)
  return [
    { name: 'ordered Match load-more requests', actual: requests, expected: loadBudget.maxRequests, pass: requests <= loadBudget.maxRequests, category: 'exact' },
    { name: 'ordered Match load-more p95 ms', actual: load.p95Ms, expected: loadBudget.maxP95Ms, pass: !comparableEnvironment || load.p95Ms <= loadBudget.maxP95Ms, category: comparableEnvironment ? 'pinned-environment' : 'informational' }
  ]
}
