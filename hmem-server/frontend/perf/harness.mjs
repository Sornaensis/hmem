import { createHash, randomUUID } from 'node:crypto'
import { assertQualificationEnvironment, createPhysicalRequestAccounting, observationDiscoveryReceipt } from './contracts.mjs'
import { createObservationDiscoveryFixture } from './fixtures.mjs'
import { execFileSync } from 'node:child_process'
import fs from 'node:fs'
import http from 'node:http'
import os from 'node:os'
import path from 'node:path'
import { fileURLToPath } from 'node:url'
import { chromium } from '@playwright/test'
import { scanObservationRows, revealObservationRow } from '../tests-js/observation-viewport-fixture.mjs'
import { evidenceProfile, evidenceOwnerFields, isolatedRecordOutputs, assertEvidenceIdentity, prepareEvidenceScratch, createEvidenceOperations, checkQualification, startupEvidenceDisposition, persistOrRetainFailureDiagnostics } from './evidence-profile.mjs'
import { OBSERVATION_SCALING_CONTRACT, generateObservationScalingFixture, queryObservationScalingMatches, observationScalingFrames, aggregateObservationScaling, observationScalingMetrics, observationTraceCaptureOptions, traceAdmissionReceipt } from './observation-scaling.mjs'
import { createColdDiagnostics, createEvidenceCapture, persistEvidenceAttempt, createUsablePaintReadiness, createNavigationCompletionIndex, assertNavigationCapacity, retireOwnedResources, assertCompleteNavigationStream, createHierarchyObserverLedger, assertFiveSamples, TIMELINE_CURRENT_CONTRACT, timelineReceipt, assertTimelineReceipt, timelineChartExpectation, OBSERVATION_PAGINATION_CONTRACT, SUBJECT_TREE_LIVE_FILTERS_CONTRACT, assertObservationContentScopeUrl, observationPageReceipt, assertObservationPageReceipt, aggregateObservationLoadMore, observationPaginationMetrics, BASE_COMMIT, HARNESS_CONFIGURATION, hashJson, liveSettleReady, liveTimingSummary, liveWholeWorkspaceReload, LIVE_OBSERVATION_REFRESH_CONTRACT, classifyLiveObservationHydration, aggregateLiveHydration, assertNavigationBatchOutcomes, navigationBatchTransportProof, median, nearestRankP95, perfApiRouteKey, renderBudgetEvaluation, renderMaximum, representativeReadiness, transportContractReady } from './contracts.mjs'
import { DIRECT_FOCUS_CONTRACT, OBSERVATION_MEASURED_QUERY, TIMELINE_BROWSER_NOW, PLANNING_COUNTS_CONTRACT, deepFocusFixture, directFocusFixture, fixtureHash, generateFixture, navigationBranchResponse, navigationBatchDescriptors, navigationBatchResponse, navigationFocusResponse, navigationSummariesResponse, paginate, projectOverviewResponse, queryObservationCounts, queryObservationFacets, queryObservations, queryProjects, queryTasks, queryTimelineBuckets, queryTimelineEvents, snapshotHash, snapshotItems, taskOverviewResponse, workspacePlanningCounts, workspaceShellSnapshotItems } from './fixtures.mjs'

const here = path.dirname(fileURLToPath(import.meta.url))
const frontendRoot = path.dirname(here)
const staticRoot = path.resolve(frontendRoot, '..', 'static')
const baselinePath = path.join(here, 'baseline.v1.json')
const traceManifestPath = path.join(here, 'trace-manifest.v1.json')
const budgets = JSON.parse(fs.readFileSync(path.join(here, 'budgets.v1.json'), 'utf8'))
const mode = process.argv[2] || 'check'
const recordAuthorized = process.argv.includes('--authorize-baseline')
const afterArtifactAuthorized = process.argv.includes('--authorize-after-artifact')
const outputArgument = process.argv.indexOf('--output')
const traceOutputArgument = process.argv.indexOf('--trace-output')
// A task-local evidence run can name its immutable review base without
// changing the approved baseline or the default compatibility record.
const evidenceBaseCommit = process.env.HMEM_EVIDENCE_BASE_COMMIT || '818cc3cc634bfa8c0ebe14c13fc70b4c2d059e83'
const evidencePlan = evidenceProfile(evidenceBaseCommit, frontendRoot, os.tmpdir())
const evidenceTask = evidencePlan.task
const evidenceRevision = evidencePlan.revision
const traceCaptureOptions = observationTraceCaptureOptions(evidencePlan.isolatedEvidence)
const evidenceOperations = createEvidenceOperations(evidencePlan, os.tmpdir())
let attemptState = null
const legacyAfterArtifactPath = path.join(here, 'final-working-tree.after.v1.json')
const legacyAfterTraceArtifactPath = path.join(here, 'final-working-tree.trace-manifest.v1.json')
const evidenceManifestPath = evidencePlan.files.manifest
const evidenceDiffPath = evidencePlan.files.diff
const validationRecordPath = evidencePlan.files.validation
const afterArtifactPath = evidencePlan.files.after
const afterTraceArtifactPath = evidencePlan.files.traceManifest
const requestedRecordOutputPath = outputArgument === -1 ? baselinePath : path.resolve(frontendRoot, process.argv[outputArgument + 1] || '')
const requestedRecordTraceManifestPath = traceOutputArgument === -1 ? traceManifestPath : path.resolve(frontendRoot, process.argv[traceOutputArgument + 1] || '')
const isolatedOutputs = mode === 'record' ? isolatedRecordOutputs(evidencePlan, requestedRecordOutputPath, requestedRecordTraceManifestPath, legacyAfterArtifactPath, legacyAfterTraceArtifactPath) : null
const recordOutputPath = isolatedOutputs?.output || (evidenceTask && requestedRecordOutputPath === legacyAfterArtifactPath ? afterArtifactPath : requestedRecordOutputPath)
const recordTraceManifestPath = isolatedOutputs?.traceManifest || (evidenceTask && requestedRecordTraceManifestPath === legacyAfterTraceArtifactPath ? afterTraceArtifactPath : requestedRecordTraceManifestPath)
const WARMUPS = HARNESS_CONFIGURATION.warmups
const PRODUCTION_SNAPSHOT_PROFILE = 'workspace_shell_v1'
const SAMPLES = HARNESS_CONFIGURATION.samples
const expandedScratch = path.resolve(frontendRoot, '..', '..', '.scratch', 'expanded-navigation-perf')
const tracePath = evidencePlan.trace || (evidenceRevision === 'expanded-hierarchy.v1' ? path.join(expandedScratch, 'large-expanded-trace.zip') : path.join(here, '.artifacts', 'large-baseline-trace.zip'))

if (!['record', 'check'].includes(mode)) throw new Error('usage: node perf/harness.mjs <record|check> [--output path --trace-output path]')
if ((outputArgument !== -1 && !process.argv[outputArgument + 1]) || (traceOutputArgument !== -1 && !process.argv[traceOutputArgument + 1])) throw new Error('--output and --trace-output require paths')
if (mode === 'record' && recordOutputPath === baselinePath && !recordAuthorized) throw new Error('record mode overwrites the approved baseline; rerun with --authorize-baseline')
if (mode === 'record' && recordOutputPath !== baselinePath && !afterArtifactAuthorized) throw new Error('after-artifact record requires --authorize-after-artifact and must not overwrite baseline.v1.json')

function platformExecutable(name) {
  return process.platform === 'win32' ? `${name}.cmd` : name
}

function requiredCommandVersion(label, command, args) {
  try {
    const executable = process.platform === 'win32' && command.toLowerCase().endsWith('.cmd') ? (process.env.ComSpec || 'cmd.exe') : command
    const parameters = executable === command ? args : ['/d', '/s', '/c', command, ...args]
    const version = execFileSync(executable, parameters, { cwd: frontendRoot, encoding: 'utf8', stdio: ['ignore', 'pipe', 'pipe'] }).trim()
    if (!version) throw new Error('empty version output')
    return version
  } catch (error) {
    throw new Error(`required ${label} version probe failed (${command} ${args.join(' ')}): ${error.message}`)
  }
}

function runTaskEvidencePrerequisites() {
  if (mode !== 'record' || !evidenceTask || !afterArtifactAuthorized) return
  const command = platformExecutable('npm')
  const executable = process.platform === 'win32' && command.toLowerCase().endsWith('.cmd') ? (process.env.ComSpec || 'cmd.exe') : command
  const parameters = executable === command ? ['run', 'perf:self-check'] : ['/d', '/s', '/c', command, 'run', 'perf:self-check']
  const startedAtUtc = new Date().toISOString(), started = performance.now()
  execFileSync(executable, parameters, { cwd: frontendRoot, stdio: 'inherit' })
  return { command: 'npm run perf:self-check', exitCode: 0, startedAtUtc, finishedAtUtc: new Date().toISOString(), durationMs: Math.round(performance.now() - started) }
}

function sha256File(file) {
  return createHash('sha256').update(fs.readFileSync(file)).digest('hex')
}

function normalizedRepositoryPath(file) {
  return path.relative(path.resolve(frontendRoot, '..', '..'), file).split(path.sep).join('/')
}

function evidenceArtifactPath(file) {
  return evidencePlan.isolatedEvidence ? path.resolve(file) : normalizedRepositoryPath(file)
}

function writeEvidence(file, bytes) {
  evidenceOperations.write(file, bytes)
}

export function untrackedFileDiff(repositoryRoot, relativePath, capture = createEvidenceCapture({ cwd: repositoryRoot })) {
  return capture.read(['diff', '--binary', '--no-index', '--no-ext-diff', '--src-prefix=a/', '--dst-prefix=b/', '--', '/dev/null', relativePath],
    { label: 'untracked diff ' + relativePath, acceptedExitCodes: [0, 1] })
}

function sourceProvenance(capture = createEvidenceCapture({ cwd: path.resolve(frontendRoot, '..', '..') }), baseCommit = evidenceBaseCommit) {
  const repositoryRoot = path.resolve(frontendRoot, '..', '..')
  const headCommit = capture.read(['rev-parse', 'HEAD'], { label: 'current source revision' }).toString('utf8').trim()
  const orderedCommits = capture.read(['rev-list', '--reverse', baseCommit + '..' + headCommit], { label: 'ordered source commit chain' }).toString('utf8').trim().split(/\r?\n/).filter(Boolean)
  return { reviewBaseCommit: baseCommit, headCommit, orderedCommits, workingTreeQualification: 'exact source and production-asset SHA-256 list; complete diff from review base retained separately' }
}

export function inputQualification({ plan = evidencePlan, io = fs, capture = createEvidenceCapture({ cwd: path.resolve(frontendRoot, '..', '..') }) } = {}) {
  const collect = directory => io.readdirSync(directory, { withFileTypes: true }).flatMap(entry => {
    const file = path.join(directory, entry.name)
    return entry.isDirectory() ? collect(file) : [file]
  }).sort()
  const source = [...collect(path.join(frontendRoot, 'src')),
    ...io.readdirSync(here).filter(name => name.endsWith('.mjs')).sort().map(name => path.join(here, name)),
    ...(plan.rendering || plan.strictSourceIdentity ? [path.join(frontendRoot, 'tests-js/observation-viewport-fixture.mjs')] : []),
    ...['package.json', 'package-lock.json', 'README.md'].map(name => path.join(frontendRoot, name))]
    .map(file => ({ path: normalizedRepositoryPath(file), sha256: createHash('sha256').update(io.readFileSync(file)).digest('hex') }))
  const productionAssets = collect(staticRoot).map(file => ({ path: normalizedRepositoryPath(file), sha256: createHash('sha256').update(io.readFileSync(file)).digest('hex') }))
  return { evidenceBaseCommit: plan.baseCommit, source, productionAssets,
    ...(plan.strictSourceIdentity ? { implementationCommit: capture.read(['rev-parse', 'HEAD'], { label: 'qualified implementation revision' }).toString('utf8').trim() } : {}) }
}

export function assertQualifiedInputs(expected, actual, message) {
  if (JSON.stringify(actual) !== JSON.stringify(expected)) throw new Error(message)
}

export function finalWorkingTreeEvidence({ plan = evidencePlan, io = fs, write = writeEvidence,
  capture = createEvidenceCapture({ cwd: path.resolve(frontendRoot, '..', '..') }) } = {}) {
  const evidenceBaseCommit = plan.baseCommit, evidenceTask = plan.task
  const evidenceDiffPath = plan.files.diff, evidenceManifestPath = plan.files.manifest, validationRecordPath = plan.files.validation
  const sha256File = file => createHash('sha256').update(io.readFileSync(file)).digest('hex')
  const evidenceArtifactPath = file => plan.isolatedEvidence ? path.resolve(file) : normalizedRepositoryPath(file)
  const repositoryRoot = path.resolve(frontendRoot, '..', '..')
  // Review subjects deliberately exclude generated evidence from their source
  // patch. Retain v1 alongside v2 here so a historical artifact cannot leak
  // into the 04fd complete diff merely because it remains untracked locally.
  const artifactPaths = new Set(['v1', 'v2', 'v3', 'expanded-hierarchy.v1'].flatMap(revision => [
    path.join(here, `final-working-tree.after.${revision}.json`),
    path.join(here, `final-working-tree.trace-manifest.${revision}.json`),
    path.join(here, `final-working-tree.evidence-manifest.${revision}.json`),
    path.join(here, `final-working-tree.validation-record.${revision}.json`)
  ]).concat([evidenceDiffPath, path.join(here, 'final-working-tree.complete.diff')]).map(normalizedRepositoryPath))
  const trackedPaths = capture.read(['diff', '--name-only', evidenceBaseCommit, '--'], { label: 'tracked path list' }).toString('utf8')
    .trim().split(/\r?\n/).filter(Boolean)
    .filter(relative => !artifactPaths.has(relative) && !relative.startsWith('.scratch/expanded-navigation-perf/'))
  const trackedDiff = trackedPaths.length === 0
    ? Buffer.alloc(0)
    : capture.read(['diff', '--binary', '--no-ext-diff', evidenceBaseCommit, '--', ...trackedPaths], { label: 'tracked complete diff' })
  const untrackedPaths = capture.read(['ls-files', '--others', '--exclude-standard'], { label: 'untracked path list' }).toString('utf8')
    .trim().split(/\r?\n/).filter(Boolean)
    .filter(relative => !artifactPaths.has(relative) && !relative.startsWith('.scratch/expanded-navigation-perf/'))
  const diff = Buffer.concat([trackedDiff, ...untrackedPaths.map(relative => untrackedFileDiff(repositoryRoot, relative, capture))])
  write(evidenceDiffPath, diff)
  const baselineAtBase = capture.read(['show', `${evidenceBaseCommit}:hmem-server/frontend/perf/baseline.v1.json`], { label: 'immutable baseline at review base' })
  const baselineNow = io.readFileSync(baselinePath)
  const changedPaths = [...new Set([...trackedPaths, ...untrackedPaths])]
    .map(relative => path.join(repositoryRoot, relative))
    .filter(io.existsSync)
    .map(file => ({ path: normalizedRepositoryPath(file), sha256: sha256File(file) }))
  // `check` has no record CLI output arguments, so its final manifest refresh
  // must still hash the selected task-local after artifacts rather than the
  // immutable baseline inputs.
  const evidenceRecordPath = evidenceTask ? plan.files.after : recordOutputPath
  const evidenceTracePath = evidenceTask ? plan.files.traceManifest : recordTraceManifestPath
  const artifacts = [evidenceRecordPath, evidenceTracePath, evidenceDiffPath]
    .concat(io.existsSync(validationRecordPath) ? [validationRecordPath] : [])
    .map(file => ({ path: evidenceArtifactPath(file), sha256: sha256File(file), sizeBytes: io.statSync(file).size }))
  const manifest = {
    schemaVersion: 1,
    taskId: evidenceTask?.taskId || null,
    ...evidenceOwnerFields(plan),
    parentTaskId: evidenceTask?.parentTaskId || null,
    measurementRevision: plan.revision,
    measurementPhase: HARNESS_CONFIGURATION.scenarioIsolation,
    sourceProvenance: sourceProvenance(capture, evidenceBaseCommit),
    qualificationPassed: io.existsSync(validationRecordPath) && JSON.parse(io.readFileSync(validationRecordPath, 'utf8')).passed === true,
    evidenceBaseCommit,
    normalization: 'repository-relative POSIX paths; SHA-256 of exact file bytes; binary Git diff without external diff drivers; untracked source files represented by deterministic no-index additions; generated evidence artifacts separately hash-listed',
    recipe: `HMEM_EVIDENCE_BASE_COMMIT=${evidenceBaseCommit} npm run perf:record-after`,
    equality: {
      baselinePath: normalizedRepositoryPath(baselinePath),
      source: `${evidenceBaseCommit}:hmem-server/frontend/perf/baseline.v1.json`,
      byteForByteEqual: baselineAtBase.equals(baselineNow),
      baseSha256: createHash('sha256').update(baselineAtBase).digest('hex'),
      workingTreeSha256: sha256File(baselinePath)
    },
    artifacts,
    changedPaths,
    retention: plan.retention,
    completeDiff: { path: evidenceArtifactPath(evidenceDiffPath), sha256: sha256File(evidenceDiffPath), sizeBytes: io.statSync(evidenceDiffPath).size }
  }
  if (plan.strictSourceIdentity) {
    manifest.traceArchive = { path: plan.trace, available: io.existsSync(plan.trace) }
    if (manifest.qualificationPassed && manifest.traceArchive.available) throw new Error('Task qualification cannot retain the temporary trace archive')
    const validation = JSON.parse(io.readFileSync(validationRecordPath, 'utf8'))
    assertEvidenceIdentity(plan, validation)
    if (validation.inputQualification?.implementationCommit !== manifest.sourceProvenance.headCommit) throw new Error('Task implementation revision changed before evidence finalization')
    assertEvidenceIdentity(plan, manifest)
  }
  if (!manifest.equality.byteForByteEqual) throw new Error('immutable baseline differs from evidence base')
  write(evidenceManifestPath, `${JSON.stringify(manifest, null, 2)}\n`)
}

// Bind proof intent to the actual source-ordered rendered Retry button, never DTO bytes.
export async function retryNavigationKind(page, tracker, owner, kind) {
  const selection = tracker.navigationProof.retrySelection(owner, kind)
  const assertQuiet = () => {
    if (!tracker.navigationProof.retryOwnerQuiet(selection) || (tracker.activeBranchOwners.get(owner) || 0) !== 0
      || (tracker.activeBranchOwners.get('workspace_root') || 0) !== 0 || tracker.activeSnapshots !== 0) throw new Error('Retry owner is unsettled before click')
  }
  assertQuiet()
  const selector = owner === 'workspace_root'
    ? '.hierarchy-row[data-hierarchy-key=' + JSON.stringify('root-status:' + kind) + ']'
    : '.hierarchy-row[data-hierarchy-key=' + JSON.stringify('status:' + owner) + '] .card-description-error'
  const button = page.locator(selector)
    .getByRole('button', { name: owner === 'workspace_root' ? 'Retry loading ' + kind + 's' : 'Retry', exact: true }).nth(owner === 'workspace_root' ? 0 : selection.buttonIndex)
  await button.waitFor({ state: 'visible', timeout: 5000 })
  assertQuiet()
  tracker.navigationProof.armRetry(selection, true)
  try { await button.click({ timeout: 5000 }) }
  finally { tracker.navigationProof.cancelRetry(selection) }
}

export function navigationPageReceipts(tracker, fromIndex = 0) {
  return [...tracker.requests.filter(request => request.key === 'navigation:branch'), ...(tracker.navigationOwnerPages || [])]
    .filter(request => (request.requestIndex ?? 0) >= fromIndex)
    .sort((left, right) => (left.requestIndex ?? 0) - (right.requestIndex ?? 0) || (left.ownerIndex ?? 0) - (right.ownerIndex ?? 0))
}

export function assertLinkedNavigationPage(tracker, page) {
  if (!page.logical) return
  const transport = tracker.requests[page.requestIndex]
  if (!transport || transport.requestId !== page.transportRequestId || page.method !== 'POST' || page.counted !== false || page.bytes !== 0) throw new Error('Unlinked logical navigation page')
  navigationBatchTransportProof(transport, page.workspaceId)
  const descriptors = navigationBatchDescriptors(transport.batchBody, transport.url), descriptor = descriptors[page.ownerIndex]
  const outcome = transport.batchResponse.outcomes[page.ownerIndex]
  if (!descriptor || descriptor.requestId !== page.ownerRequestId || descriptor.logicalUrl !== page.url || hashJson(outcome.response || null) !== hashJson(page.navigation)
    || outcome.status !== page.navigationStatus || !transport.ownerPages.includes(page)) throw new Error('Foreign logical navigation page')
}

export function fixtureResponder(fixture, tracker, options = {}) {
  const discovery = options.discovery || createObservationDiscoveryFixture(fixture,options.discoveryOptions)
  const snapshots = options.snapshots || snapshotItems(fixture)
  const resyncGate = options.resyncGate || null
  const focusContinuationPause = options.focusContinuationPause || null
  const projects = new Map(fixture.projects.map(item => [item.id, item]))
  const tasks = new Map(fixture.tasks.map(item => [item.id, item]))
  const observations = new Map(fixture.observations.map(item => [item.id, item]))
  const reply = async (route, request, value, status = 200, explicitKey = null) => {
    const body = value == null ? '' : JSON.stringify(value)
    const key = explicitKey || perfApiRouteKey(request.url(), request.method())
    if (!key) throw new Error(`implemented perf response has no route contract: ${request.method()} ${request.url()}`)
    const receipt = tracker.arrivals.get(request)
    if (!receipt) throw new Error('response without registered arrival')
    receipt.key = key
    receipt.status = status
    receipt.workspaceId ||= value?.workspace_id || null
    receipt.bytes = Buffer.byteLength(body)
    if (key === 'observations:discovery') receipt.discovery = observationDiscoveryReceipt(receipt.requestBody,value,status,false,receipt.rawRequestBody,body)
    if (key === 'navigation:batch') receipt.batchResponse = value
    tracker.bytes[key] = (tracker.bytes[key] || 0) + receipt.bytes
    if (key === 'navigation:branch') { receipt.navigation = value; receipt.navigationStatus = status }
    if (key === 'observations:list') receipt.observationPage = observationPageReceipt(request.url(), request.method(), value, status)
    if (key.startsWith('observations:entity:')) receipt.observationEntity = { id: value?.id || null, workspaceId: value?.workspace_id || null,
      dtoHash: hashJson(value), bytes: receipt.bytes }
    if (key === 'timeline:events' || key === 'timeline:buckets') receipt.timeline = timelineReceipt(request.url(), request.method(), value, status)
    const gate = receipt.gate
    let fulfilled = false
    tracker.liveDiagnostics?.prepared(receipt.requestId, performance.now(), status, receipt.bytes)
    if (gate && !gate.used) { gate.used = true; gate.signalPaused(); await (gate.waitForRelease) }
    try {
      await route.fulfill({ status, contentType: 'application/json', body,
        ...(receipt.paintReceiptId ? { headers: { 'Server-Timing': 'hmem_receipt;desc="' + receipt.paintReceiptId + '"' } } : {}) })
      fulfilled = true
    } finally {
      tracker.liveDiagnostics?.fulfilled(receipt.requestId, performance.now(), fulfilled)
      receipt.fulfilled = fulfilled
      tracker.physicalAccounting.finish(receipt.requestId,body,fulfilled)
      if (receipt.discovery) receipt.discovery.fulfilled = fulfilled
      if (receipt.observationPage) receipt.observationPage.response.fulfilled = fulfilled
      if (receipt.timeline) receipt.timeline.response.fulfilled = fulfilled
      if (key === 'navigation:branch') receipt.navigationFulfilled = fulfilled
      if (status < 400 && key === 'change-stream:resync') {
        const requestBody = request.postDataJSON()
        if (!requestBody.page_token) {
          tracker.model.snapshotGeneration += 1
          tracker.model.snapshotItems = 0
          tracker.model.snapshotPages = 0
          tracker.model.snapshotComplete = false
          tracker.model.snapshotProfile = value.snapshot_profile
        }
        tracker.model.snapshotItems += value.items.length
        tracker.model.snapshotPages += 1
        tracker.model.snapshotComplete = value.has_more === false
        if (tracker.model.snapshotComplete) tracker.navigationProof?.completeSnapshot(receipt.proofSnapshot, value.snapshot_profile)
      }
      if (fulfilled && status < 400 && key === 'timeline:events') tracker.model.timelineEvents = value.items.length
      if (fulfilled && status < 400 && key === 'timeline:buckets') {
        tracker.model.timelineBuckets = value.buckets.length
        tracker.model.timelineBucketRequest = { since: value.since, until: value.until, bucket: value.bucket }
      }
      if (fulfilled && receipt.paintSession) tracker.paintReadiness.completeSession(receipt.paintSession, { canRead: status < 400 && value?.workspace?.can_read === true, workspaceId: value?.workspace?.workspace_id }, performance.now())
      if (fulfilled && receipt.paintSnapshot && status < 400) tracker.paintReadiness.completeSnapshot(receipt.paintSnapshot, { profile: value.snapshot_profile, workspaceId: value.items.find(item => item.kind === 'workspace')?.data?.id, items: value.items.length, complete: value.has_more === false }, performance.now())
      if (receipt.proofSession !== undefined) tracker.navigationProof.completeSession(receipt.proofSession, status < 400 && value?.workspace?.can_read === true)
      if (receipt.proofNavigation) tracker.navigationProof.complete(receipt.proofNavigation, value?.projects && value?.tasks ? {
        projects: { ids: value.projects.items.map(item => item.id), hasMore: value.projects.has_more },
        tasks: { ids: value.tasks.items.map(item => item.id), hasMore: value.tasks.has_more }
      } : null, status)
      if (fulfilled && status < 400) rememberNavigationMetadata(tracker, receipt, value)
      if (receipt.ownerPages) {
        let outcomes = null
        if (fulfilled && status === 200) {
          try { outcomes = assertNavigationBatchOutcomes(fixture.workspace.id, receipt.batchBody.requests, value) } catch (error) { receipt.batchFailure = error.message }
        }
        for (const [index, owner] of receipt.ownerPages.entries()) {
          const outcome = outcomes?.[index]
          owner.navigation = outcome?.response || null; owner.navigationStatus = outcome?.status || (status >= 400 ? status : 502)
          owner.navigationFulfilled = fulfilled; owner.fulfilled = fulfilled; owner.done = true; owner.status = owner.navigationStatus
          if (owner.proofNavigation) tracker.navigationProof.complete(owner.proofNavigation, owner.navigation ? {
            projects: { ids: owner.navigation.projects.items.map(item => item.id), hasMore: owner.navigation.projects.has_more },
            tasks: { ids: owner.navigation.tasks.items.map(item => item.id), hasMore: owner.navigation.tasks.has_more }
          } : null, owner.navigationStatus)
          if (fulfilled && owner.navigationStatus === 200) rememberNavigationMetadata(tracker, owner, owner.navigation)
          tracker.activeBranches--; tracker.activeExpandedBranches--
          const remaining = tracker.activeBranchOwners.get(owner.branchOwner) - 1
          if (remaining === 0) tracker.activeBranchOwners.delete(owner.branchOwner); else tracker.activeBranchOwners.set(owner.branchOwner, remaining)
        }
      }
      receipt.done = true
      tracker.active -= 1
      if (key === 'navigation:branch') {
        tracker.activeBranches -= 1; tracker['active' + receipt.branchClass + 'Branches'] -= 1
        const remaining = tracker.activeBranchOwners.get(receipt.branchOwner) - 1
        if (remaining === 0) tracker.activeBranchOwners.delete(receipt.branchOwner)
        else tracker.activeBranchOwners.set(receipt.branchOwner, remaining)
      }
      if (key === 'change-stream:resync') tracker.activeSnapshots -= 1
      if (key.includes(':entity:') && (key.startsWith('projects:') || key.startsWith('tasks:'))) tracker.activeDetails -= 1
      tracker.completed += 1
      tracker.lastActivityAt = performance.now()
      tracker.coldDiagnostics.completion({ at: tracker.lastActivityAt, requestId: receipt.requestId, key, bytes: receipt.bytes, status, fulfilled, paintReceiptId: receipt.paintReceiptId ?? null, lifetime: tracker.paintReadiness?.lifetime() })
    }
  }
  return async route => {
    const request = route.request()
    const url = new URL(request.url())
    const pathname = url.pathname
    const contractedKey = perfApiRouteKey(url, request.method())
    const key = contractedKey || 'unimplemented:' + request.method() + ':' + pathname
    const receipt = { requestId: tracker.requests.length + 1, requestIndex: tracker.requests.length, runId: tracker.paintRunId,
      workspaceId: pathname.match(/^\/api\/v1\/workspaces\/([^/]+)(?:\/|$)/)?.[1] || url.searchParams.get('workspace_id')
        || (key === 'observations:count' ? request.postDataJSON()?.workspace_id : null),
      key, metadataEpoch: tracker.metadataEpoch, method: request.method(), url: request.url(), bytes: 0, at: performance.now(), done: false }
    const rawRequestBody = typeof request.postData === 'function' ? request.postData() || ''
      : request.method() === 'POST' && typeof request.postDataJSON === 'function' ? JSON.stringify(request.postDataJSON()) : ''
    tracker.physicalAccounting.begin(receipt.requestId,rawRequestBody)
    receipt.requestBytes = Buffer.byteLength(rawRequestBody || '')
    if (key === 'observations:discovery') {
      receipt.rawRequestBody = rawRequestBody
      receipt.requestBody = request.postDataJSON()
      receipt.workspaceId = receipt.requestBody?.workspace_id || null
    }
    if (tracker.navigationProof) {
      if (key === 'session' || key === 'change-stream:resync') receipt.paintReceiptId = tracker.paintRunId + '-' + tracker.requests.length
      if (key === 'session') { tracker.metadataEpoch++; tracker.navigationMetadata.clear(); receipt.metadataEpoch = tracker.metadataEpoch; receipt.proofSession = tracker.navigationProof.beginSession(); receipt.paintSession = tracker.paintReadiness.beginSession(receipt.at, receipt.paintReceiptId, request.url()) }
      if (key === 'change-stream:resync') { receipt.proofSnapshot = tracker.navigationProof.admitSnapshot(!request.postDataJSON().page_token); receipt.paintSnapshot = tracker.paintReadiness.admitSnapshot(!request.postDataJSON().page_token, receipt.at, receipt.paintReceiptId, request.url()) }
      if (key === 'navigation:branch' && pathname === '/api/v1/workspaces/' + tracker.workspaceId + '/navigation') {
        const kind = url.searchParams.get('parent_kind')
        receipt.proofNavigation = tracker.navigationProof.admit({
          owner: kind === 'workspace_root' ? kind : kind + ':' + url.searchParams.get('parent_id'), context: navigationFilterContext(url),
          projectIncluded: url.searchParams.get('include_projects') !== 'false', taskIncluded: url.searchParams.get('include_tasks') !== 'false',
          ...Object.fromEntries(['project', 'task'].flatMap(kind => [['Offset', 'offset'], ['Limit', 'limit']].map(([field, parameter]) => [kind + field, Number(url.searchParams.get(kind + '_' + parameter))])))
        })
      }
    }
    tracker.coldDiagnostics.arrival({ at: receipt.at, requestId: tracker.requests.length + 1, key, url: request.url(), paintReceiptId: receipt.paintReceiptId ?? null, lifetime: tracker.paintReadiness?.lifetime() });
    tracker.requests.push(receipt); tracker.arrivals.set(request, receipt)
    tracker.liveDiagnostics?.arrival(receipt.requestId, key, receipt.at)
    tracker.active += 1; tracker.lastActivityAt = receipt.at
    tracker.counts[key] = (tracker.counts[key] || 0) + 1
    if (key === 'navigation:batch') {
      try {
        receipt.batchBody = request.postDataJSON()
        const descriptors = navigationBatchDescriptors(receipt.batchBody, request.url())
        receipt.ownerPages = descriptors.map((descriptor, ownerIndex) => {
          const logicalUrl = new URL(descriptor.logicalUrl)
          const owner = { key: 'navigation:branch', logical: true, counted: false, method: 'POST', url: descriptor.logicalUrl,
            transportRequestId: receipt.requestId, transportUrl: receipt.url, ownerRequestId: descriptor.requestId, ownerIndex,
            requestId: receipt.requestId + ':' + descriptor.requestId, requestIndex: receipt.requestIndex, runId: receipt.runId,
            workspaceId: receipt.workspaceId, metadataEpoch: receipt.metadataEpoch, at: receipt.at, bytes: 0, done: false,
            branchOwner: descriptor.owner, branchClass: 'Expanded', batchBody: receipt.batchBody }
          if (tracker.navigationProof) owner.proofNavigation = tracker.navigationProof.admit({ workspaceId: receipt.workspaceId,
            owner: descriptor.owner, parentKind: descriptor.parent.kind, parentId: descriptor.parent.parent_id, context: navigationFilterContext(logicalUrl),
            projectIncluded: descriptor.options.includeProjects, taskIncluded: descriptor.options.includeTasks,
            projectOffset: descriptor.options.projectOffset, taskOffset: descriptor.options.taskOffset, projectLimit: descriptor.options.projectLimit, taskLimit: descriptor.options.taskLimit })
          tracker.activeBranchOwners.set(owner.branchOwner, (tracker.activeBranchOwners.get(owner.branchOwner) || 0) + 1)
          tracker.activeBranches++; tracker.activeExpandedBranches++
          tracker.maxBranches = Math.max(tracker.maxBranches, tracker.activeBranches)
          tracker.maxExpandedBranches = Math.max(tracker.maxExpandedBranches, tracker.activeExpandedBranches)
          return owner
        })
        ;(tracker.navigationOwnerPages ||= []).push(...receipt.ownerPages)
      } catch (error) { receipt.batchFailure = error.message; return reply(route, request, { error: 'Invalid navigation batch' }, 400) }
    }
    if (key === 'change-stream:resync') tracker.activeSnapshots += 1
    if (key === 'navigation:branch') {
      const kind = url.searchParams.get('parent_kind')
      receipt.branchOwner = kind === 'workspace_root' ? kind : kind + ':' + url.searchParams.get('parent_id')
      tracker.activeBranchOwners.set(receipt.branchOwner, (tracker.activeBranchOwners.get(receipt.branchOwner) || 0) + 1)
      receipt.branchClass = url.searchParams.get('parent_kind') === 'workspace_root' ? 'Root' : 'Expanded'
      tracker.activeBranches += 1; tracker.maxBranches = Math.max(tracker.maxBranches, tracker.activeBranches)
      tracker['active' + receipt.branchClass + 'Branches'] += 1
      tracker['max' + receipt.branchClass + 'Branches'] = Math.max(tracker['max' + receipt.branchClass + 'Branches'], tracker['active' + receipt.branchClass + 'Branches'])
    }
    if (key.includes(':entity:') && (key.startsWith('projects:') || key.startsWith('tasks:'))) { tracker.activeDetails += 1; tracker.maxDetails = Math.max(tracker.maxDetails, tracker.activeDetails) }
    if (!contractedKey) {
      const exact = `${request.method()} ${request.url()}`
      tracker.unhandledApiRoutes.push(exact)
      return reply(route, request, { error: `unimplemented perf API route: ${exact}` }, 501, `unimplemented:${request.method()}:${pathname}`)
    }
    if (pathname === '/api/v1/session') return reply(route, request, {
      auth_mode: 'local', principal: { actor_type: 'user', actor_id: 'perf-user', actor_label: 'Performance User', authority: 'local', grant_user_id: null },
      global_permissions: { create_workspace: false, superadmin: false },
      workspace: { workspace_id: fixture.workspace.id, role: 'owner', can_read: true, can_edit: true, can_admin: true }
    })
    if (pathname === '/api/v1/change-stream/resync') {
      const body = request.postDataJSON()
      const profile = body.snapshot_profile || 'full_v1'
      const selectedSnapshots = profile === 'workspace_shell_v1' ? workspaceShellSnapshotItems(fixture) : snapshots
      const offset = body.page_token ? Number(String(body.page_token).split(':')[1]) : 0
      const pageSize = Number(body.page_size) || 100
      if (resyncGate && !resyncGate.used && offset === resyncGate.pauseOffset) {
        receipt.gate = resyncGate
      }
      const page = selectedSnapshots.slice(offset, offset + pageSize)
      const hasMore = offset + pageSize < selectedSnapshots.length
      return reply(route, request, { snapshot_profile: profile, items: page, has_more: hasMore, ...(hasMore ? { next_page_token: `offset:${offset + pageSize}` } : { resume_token: `fixture-${fixture.size}-resume` }) })
    }
    if (pathname === '/api/v1/change-stream/ticket') return reply(route, request, { ticket: `fixture-${fixture.size}-ticket`, expires_at: '2099-01-01T00:00:00Z' })
    if (pathname === '/api/v1/workspaces') return reply(route, request, paginate([fixture.workspace], url.searchParams.get('offset'), url.searchParams.get('limit')))
    if (pathname === `/api/v1/workspaces/${fixture.workspace.id}`) return reply(route, request, fixture.workspace)
    if (pathname === `/api/v1/workspaces/${fixture.workspace.id}/planning-counts`) {
      const value = workspacePlanningCounts(fixture)
      return reply(route, request, value || { error: 'workspace not found' }, value ? 200 : 404)
    }
    if (pathname === `/api/v1/workspaces/${fixture.workspace.id}/navigation`) {
      const value = navigationBranchResponse(fixture, {
      parentKind: url.searchParams.get('parent_kind'), parentId: url.searchParams.get('parent_id'),
      includeProjects: url.searchParams.get('include_projects'), includeTasks: url.searchParams.get('include_tasks'),
      projectLimit: url.searchParams.get('project_limit'), projectOffset: url.searchParams.get('project_offset'),
      taskLimit: url.searchParams.get('task_limit'), taskOffset: url.searchParams.get('task_offset'),
      showOnly: url.searchParams.get('show_only'), projectStatuses: url.searchParams.getAll('project_status'), taskStatuses: url.searchParams.getAll('task_status'),
      priorityMode: url.searchParams.get('priority_mode'), priorityValue: url.searchParams.get('priority_value'), query: url.searchParams.get('query')
      })
      return reply(route, request, options.navigationResponseTransform ? options.navigationResponseTransform(value, url) : value, options.navigationResponseStatus ? options.navigationResponseStatus(url) : 200)
    }
    if (pathname === `/api/v1/workspaces/${fixture.workspace.id}/navigation/summaries`) {
      const body = request.postDataJSON()
      const value = navigationSummariesResponse(fixture, body.project_ids || [], body.task_ids || [])
      return reply(route, request, value || { error: 'invalid summary batch' }, value ? 200 : 400)
    }
    if (pathname === `/api/v1/workspaces/${fixture.workspace.id}/navigation/batch`) {
      const value = navigationBatchResponse(fixture, receipt.batchBody, request.url())
      for (const [index, outcome] of value.outcomes.entries()) {
        const logicalUrl = new URL(receipt.ownerPages[index].url)
        if (options.navigationResponseTransform && outcome.status === 200) outcome.response = options.navigationResponseTransform(outcome.response, logicalUrl)
        if (options.navigationResponseStatus) { const status = options.navigationResponseStatus(logicalUrl); if (status >= 400) { delete outcome.response; outcome.status = status; outcome.error = 'Controlled navigation failure' } }
      }
      return reply(route, request, value)
    }
    const navigationFocus = pathname.match(new RegExp(`^/api/v1/workspaces/${fixture.workspace.id}/navigation/focus/(project|task)/([^/]+)$`))
    if (navigationFocus) {
      const entityId = decodeURIComponent(navigationFocus[2])
      const ancestorOffset = Number(url.searchParams.get('ancestor_offset') || 0)
      const value = navigationFocusResponse(fixture, navigationFocus[1], entityId, ancestorOffset)
      if (focusContinuationPause && !focusContinuationPause.used && navigationFocus[1] === focusContinuationPause.entityType && entityId === focusContinuationPause.entityId && ancestorOffset === focusContinuationPause.ancestorOffset) {
        receipt.gate = focusContinuationPause
      }
      return reply(route, request, value || { error: 'not found' }, value ? 200 : 404)
    }
    if (pathname === `/api/v1/workspaces/${fixture.workspace.id}/memberships`) return reply(route, request, {
      items: [{ workspace_id: fixture.workspace.id, user_id: 'perf-user', role: 'owner', granted_by: null, created_at: fixture.workspace.created_at, updated_at: fixture.workspace.updated_at }], has_more: false
    })
    if (pathname === '/api/v1/projects') return reply(route, request, queryProjects(fixture, {
      status: url.searchParams.get('status'), offset: url.searchParams.get('offset'), limit: url.searchParams.get('limit')
    }))
    if (pathname === '/api/v1/tasks') return reply(route, request, queryTasks(fixture, {
      projectId: url.searchParams.get('project_id'), status: url.searchParams.get('status'), priority: url.searchParams.get('priority'),
      offset: url.searchParams.get('offset'), limit: url.searchParams.get('limit')
    }))
    if (['/api/v1/observations', '/api/v1/observations/count'].includes(pathname)) {
      if (url.searchParams.getAll('query_scope').length > 1) throw new Error('Duplicate Observation query scope')
      if (options.contentOnly) assertObservationContentScopeUrl(url.href)
    }
    if (pathname === '/api/v1/observations/discovery' && request.method() === 'POST') {
      const result = discovery.handle(receipt.requestBody,options.discoveryAudience || 'fixture-reader')
      return reply(route,request,result.value,result.status)
    }
    if (pathname === '/api/v1/observations') return reply(route, request, queryObservations(fixture, {
      queryScope: url.searchParams.get('query_scope'), query: url.searchParams.get('query'), subjectKind: url.searchParams.get('subject_kind'), subject: url.searchParams.get('subject'), gitSha: url.searchParams.get('git_sha'),
      currentGitSha: url.searchParams.get('current_git_sha'), historyGitSha: url.searchParams.get('history_git_sha'),
      offset: url.searchParams.get('offset'), limit: url.searchParams.get('limit')
    }))
    if (pathname === '/api/v1/observations/subject-facets') return reply(route, request, queryObservationFacets(fixture, {
      query: url.searchParams.get('query'), subjectKind: url.searchParams.get('subject_kind'), gitSha: url.searchParams.get('git_sha'),
      currentGitSha: url.searchParams.get('current_git_sha'), historyGitSha: url.searchParams.get('history_git_sha'),
      offset: url.searchParams.get('offset'), limit: url.searchParams.get('limit')
    }))
    if (pathname === '/api/v1/observations/match') return reply(route, request, fixture.observationScaling
      ? queryObservationScalingMatches(fixture, request.postDataJSON()) : { items: [], has_more: false })
    if (pathname === '/api/v1/observations/count' && request.method() === 'POST') return reply(route, request, queryObservationCounts(fixture, { ...request.postDataJSON(), query_scope: url.searchParams.get('query_scope') }))
    const projectOverview = pathname.match(/^\/api\/v1\/projects\/([^/]+)\/overview$/)
    if (projectOverview) {
      return reply(route, request, projectOverviewResponse(fixture, decodeURIComponent(projectOverview[1])))
    }
    const taskOverview = pathname.match(/^\/api\/v1\/tasks\/([^/]+)\/overview$/)
    if (taskOverview) {
      return reply(route, request, taskOverviewResponse(fixture, decodeURIComponent(taskOverview[1])))
    }
    const projectEntity = pathname.match(/^\/api\/v1\/projects\/([^/]+)$/)
    if (projectEntity) {
      const value = projects.get(decodeURIComponent(projectEntity[1]))
      return reply(route, request, value || { error: 'project not found' }, value ? 200 : 404)
    }
    const taskEntity = pathname.match(/^\/api\/v1\/tasks\/([^/]+)$/)
    if (taskEntity) {
      const value = tasks.get(decodeURIComponent(taskEntity[1]))
      return reply(route, request, value || { error: 'task not found' }, value ? 200 : 404)
    }
    const observationEntity = pathname.match(/^\/api\/v1\/observations\/([^/]+)$/)
    if (observationEntity) {
      const value = observations.get(decodeURIComponent(observationEntity[1]))
      return reply(route, request, value || { error: 'observation not found' }, value ? 200 : 404)
    }
    if (pathname === `/api/v1/workspaces/${fixture.workspace.id}/timeline`) {
      return reply(route, request, queryTimelineEvents(fixture, {
        entityType: url.searchParams.get('entity_type'), eventType: url.searchParams.get('event_type'), since: url.searchParams.get('since'), until: url.searchParams.get('until'),
        offset: url.searchParams.get('offset'), limit: url.searchParams.get('limit')
      }))
    }
    if (pathname === `/api/v1/workspaces/${fixture.workspace.id}/timeline/buckets`) {
      try {
        return reply(route, request, queryTimelineBuckets(fixture, {
          since: url.searchParams.get('since'), until: url.searchParams.get('until'), bucket: url.searchParams.get('bucket') || 'week'
        }))
      } catch (error) {
        throw new Error(`Timeline bucket fixture request rejected like production API: ${error.message}`)
      }
    }
    const exact = `${request.method()} ${request.url()}`
    tracker.unhandledApiRoutes.push(exact)
    return reply(route, request, { error: `contracted but unimplemented perf API route: ${exact}` }, 501, `unimplemented:${request.method()}:${pathname}`)
  }
}

export async function staticServer() {
  const server = http.createServer((request, response) => {
    const pathname = new URL(request.url, 'http://local').pathname
    if (pathname === '/hmem-runtime-config.js') {
      response.writeHead(200, { 'Content-Type': 'application/javascript', 'Cache-Control': 'no-store' })
      response.end("window.HMEM_CONFIG={authMode:'local',apiUrl:window.location.origin,wsUrl:'ws://perf.invalid/api/v1/ws',authTokenStorage:'memory'};")
      return
    }
    if (pathname === '/favicon.ico') {
      response.writeHead(204); response.end(); return
    }
    const relative = pathname === '/' || pathname.startsWith('/workspace/') ? 'index.html' : pathname.replace(/^\//, '')
    const file = path.resolve(staticRoot, relative)
    if (!file.startsWith(staticRoot) || !fs.existsSync(file) || !fs.statSync(file).isFile()) {
      response.writeHead(404); response.end('not found'); return
    }
    const contentType = file.endsWith('.html') ? 'text/html' : file.endsWith('.js') ? 'application/javascript' : file.endsWith('.css') ? 'text/css' : 'application/octet-stream'
    response.writeHead(200, { 'Content-Type': contentType, 'Cache-Control': 'no-store' })
    fs.createReadStream(file).pipe(response)
  })
  await new Promise(resolve => server.listen(0, '127.0.0.1', resolve))
  return { server, origin: `http://127.0.0.1:${server.address().port}` }
}

const fakeWebSocketScript = `
(() => {
  Date.now = () => ${Date.parse(TIMELINE_BROWSER_NOW)};
  const sockets = [];
  class PerfWebSocket {
    static OPEN = 1; static CONNECTING = 0; static CLOSED = 3;
    constructor(url) { this.url=url; this.readyState=0; sockets.push(this); setTimeout(() => { this.readyState=1; if (this.onopen) this.onopen({type:'open'}); setTimeout(() => this.__emit({schema_version:1,type:'checkpoint',catch_up:'complete',resume_token:'fixture-live'}), 0); }, 0); }
    send() {}
    close() { this.readyState=3; if (this.onclose) this.onclose({code:1000,reason:'fixture'}); }
    addEventListener(type, fn) { this['on'+type]=fn; }
    removeEventListener(type, fn) { if (this['on'+type]===fn) this['on'+type]=null; }
    __emit(frame) { if (this.readyState===1 && this.onmessage) this.onmessage({data:JSON.stringify(frame)}); }
  }
  window.__perfRenderMaximum={nodes:0,rows:0,hierarchyRows:0,observers:0};
  const ledger=(${createHierarchyObserverLedger.toString()})();
  const NativeObserver=window.ResizeObserver;
  if(NativeObserver) window.ResizeObserver=class extends NativeObserver {
    observe(row,...args){super.observe(row,...args);if(row.hasAttribute('data-hierarchy-key')) ledger.observe(this,row);window.__perfRenderMaximum.observers=ledger.metrics().maximum}
    unobserve(row){super.unobserve(row);ledger.unobserve(this,row)}
    disconnect(){super.disconnect();ledger.disconnect(this)}
  };
  new MutationObserver(()=>{const m=window.__perfRenderMaximum;m.nodes=Math.max(m.nodes,document.querySelectorAll('*').length);m.rows=Math.max(m.rows,document.querySelectorAll('.card-project,.card-task,.card-subtask,.observation-card,.timeline-event-card,.timeline-value-table tbody tr,.timeline-point-control').length);m.hierarchyRows=Math.max(m.hierarchyRows,document.querySelectorAll('[data-hierarchy-key]').length)}).observe(document,{childList:true,subtree:true});
  window.WebSocket=PerfWebSocket;
  window.__perfPushFrames=(frames) => { const open=sockets.filter(socket => socket.readyState===1); const marker={frames:frames.length,sockets:open.length,resumeToken:'fixture-live-after',dispatchTurnComplete:false}; window.__perfLastPush=marker; for (const frame of frames) for (const socket of open) socket.__emit(frame); for (const socket of open) socket.__emit({schema_version:1,type:'checkpoint',catch_up:'complete',resume_token:'fixture-live-after'}); setTimeout(() => { marker.dispatchTurnComplete=true; }, 0); return open.length; };
})();`

async function requiredDoubleFrame(page, selector) {
  return page.evaluate(async value => {
    const element = document.querySelector(value)
    if (!element) throw new Error('required interaction target missing: ' + value)
    const start = performance.now()
    element.click()
    await new Promise(resolve => requestAnimationFrame(() => requestAnimationFrame(resolve)))
    return performance.now() - start
  }, selector)
}

export async function requiredLiveInputDoubleFrame(page, selector, value) {
  return page.evaluate(async ({ selector, value }) => {
    const element = document.querySelector(selector)
    if (!(element instanceof HTMLInputElement)) throw new Error('required live input target missing: ' + selector)
    const start = performance.now()
    element.focus()
    element.value = value
    element.dispatchEvent(new Event('input', { bubbles: true }))
    await new Promise(resolve => requestAnimationFrame(() => requestAnimationFrame(resolve)))
    return performance.now() - start
  }, { selector, value })
}

async function requiredDoubleFrameByText(page, selector, label) {
  return page.evaluate(async ({ selector, label }) => {
    const element = Array.from(document.querySelectorAll(selector)).find(value => value.textContent.trim() === label)
    if (!element) throw new Error('required interaction target missing: ' + selector + ' text=' + label)
    const start = performance.now()
    element.click()
    await new Promise(resolve => requestAnimationFrame(() => requestAnimationFrame(resolve)))
    return performance.now() - start
  }, { selector, label })
}

function assertNoUnhandledApiRoutes(tracker) {
  assertNavigationCapacity({ aggregate: tracker.maxBranches, expanded: tracker.maxExpandedBranches, details: tracker.maxDetails })
  if (tracker.unhandledApiRoutes.length > 0) {
    throw new Error(`unimplemented perf API route(s): ${tracker.unhandledApiRoutes.join(', ')}`)
  }
}

function representativeProjectAnchors(fixture) {
  const statusOrder = { active: 0, paused: 1, completed: 2, archived: 3 }
  const roots = fixture.projects
    .filter(project => project.parent_id == null)
    .sort((left, right) => (statusOrder[left.status] - statusOrder[right.status]) || (right.priority - left.priority) || left.name.localeCompare(right.name))
  if (roots.length === 0) throw new Error('fixture requires a root project anchor')
  const first = roots[0]
  // The bounded bootstrap deliberately owns just the root slice. Tree-child
  // rendering belongs to the follow-on lazy-branch scenario, not cold readiness.
  return { first: first.id, expanded: first.id, selected: fixture.projects.at(-1).id }
}

async function waitForTransportQuiescence(page, tracker, label, quietMs = 250, timeoutMs = 30000) {
  const deadline = performance.now() + timeoutMs
  while (performance.now() < deadline) {
    assertNoUnhandledApiRoutes(tracker)
    if (tracker.active === 0 && performance.now() - tracker.lastActivityAt >= quietMs) {
      await page.evaluate(() => new Promise(resolve => requestAnimationFrame(() => requestAnimationFrame(resolve))))
      if (tracker.active === 0 && performance.now() - tracker.lastActivityAt >= quietMs) return
    }
    await page.waitForTimeout(25)
  }
  throw new Error(`${label} did not reach transport/UI quiescence within ${timeoutMs}ms (active=${tracker.active})`)
}

async function projectUiSignals(page, anchorId) {
  return page.evaluate(id => ({
    loading: Boolean(document.querySelector('.loading-indicator')),
    focused: Boolean(document.querySelector('.focus-breadcrumb-bar')),
    anchorVisible: Boolean(document.querySelector(`#entity-${id}`))
  }), anchorId)
}

export function workspaceReadinessSnapshot(anchorId) {
  const host = document.getElementById('main-content-scroll'), viewport = document.getElementById('hierarchy-viewport')
  const anchor = document.getElementById('entity-' + anchorId), active = document.activeElement
  const rect = element => element ? { top: element.getBoundingClientRect().top, bottom: element.getBoundingClientRect().bottom, height: element.getBoundingClientRect().height } : null
  const hostRect = rect(host), anchorRect = rect(anchor), viewportRect = rect(viewport)
  const rows = [...document.querySelectorAll('[data-hierarchy-key]')]
  const key = element => element?.getAttribute('data-hierarchy-key')?.slice(0, 128) || null
  return { hash: location.hash.slice(0, 512), loading: [...document.querySelectorAll('.loading-indicator')].slice(0, 8).map(element => element.textContent.slice(0, 128)),
    focused: !!document.querySelector('.focus-breadcrumb-bar'),
    tabs: [...document.querySelectorAll('.tabs button')].slice(0, 8).map(element => ({ text: element.textContent.slice(0, 64), class: element.className.slice(0, 128), selected: element.getAttribute('aria-selected') })),
    filters: [...document.querySelectorAll('.filter-bar button,.filter-bar input,.filter-bar select')].slice(0, 20).map(element => ({ id: element.id.slice(0, 64), text: element.textContent.slice(0, 64), value: element.value?.slice(0, 128), class: element.className.slice(0, 128), pressed: element.getAttribute('aria-pressed') })),
    anchor: { id: anchorId.slice(0, 128), present: !!anchor, rect: anchorRect, withinHost: !!(anchorRect && hostRect && anchorRect.top >= hostRect.top && anchorRect.bottom <= hostRect.bottom) },
    scroll: host ? { top: host.scrollTop, clientHeight: host.clientHeight, scrollHeight: host.scrollHeight, rect: hostRect,
      viewportOrigin: viewportRect ? viewportRect.top - hostRect.top + host.scrollTop : null,
      hostRelativeTop: viewportRect ? Math.max(0, hostRect.top - viewportRect.top) : null } : null,
    hierarchy: viewport ? { context: viewport.getAttribute('data-hierarchy-context')?.slice(0, 2048) || null, rect: viewportRect, mountedCount: rows.length,
      keys: rows.slice(0, 25).map(element => ({ key: key(element), index: element.getAttribute('data-hierarchy-index')?.slice(0, 16), rect: rect(element) })),
      activeKey: key(active?.closest('[data-hierarchy-key]')), anchorKey: key(anchor?.closest('[data-hierarchy-key]')) } : null,
    active: active ? { id: active.id.slice(0, 128), tag: active.tagName } : null }
}

export function scrollProjectsRoot(workspaceId) {
  const host = document.getElementById('main-content-scroll'), viewport = document.getElementById('hierarchy-viewport')
  const tab = document.querySelector('.tabs button.active')
  let context
  try { context = JSON.parse(viewport?.getAttribute('data-hierarchy-context') || 'null') } catch { context = null }
  // Each measurement owns a new browser context and its first authorized session.
  if (!host || !viewport || tab?.textContent !== 'Projects' || location.pathname !== '/workspace/' + workspaceId
    || new URLSearchParams(location.hash.slice(1)).get('tab') !== 'projects'
    || context?.workspace !== workspaceId || context.epoch !== 1 || context.paintReady !== true
    || !Number.isSafeInteger(context.generation) || context.generation < 0 || !Number.isSafeInteger(context.revision) || context.revision < 0) {
    throw new Error('Projects return requires the current authorized Projects viewport and native scroll host')
  }
  const before = host.scrollTop
  host.scrollTop = 0
  return { method: 'native scroll host to root after measured click and two paints', before, after: host.scrollTop,
    workspace: context.workspace, epoch: context.epoch, generation: context.generation, revision: context.revision }
}

export async function measureProjectsTabReturn(page, workspaceId) {
  const ms = await requiredDoubleFrame(page, '.tabs button:nth-child(1)')
  const setup = await page.evaluate(scrollProjectsRoot, workspaceId)
  return { ms, setup }
}

async function waitForWorkspaceReady(page, tracker, fixture, anchorId, label) {
  const expectedSnapshotItems = workspaceShellSnapshotItems(fixture).length
  const expectedPages = Math.ceil(expectedSnapshotItems / DIRECT_FOCUS_CONTRACT.pageSize)
  const modelReady = () => transportContractReady({ protocol: 'bounded', transportedItems: tracker.model.snapshotItems, expectedItems: expectedSnapshotItems, fullBackingItems: snapshotItems(fixture).length, transportedPages: tracker.model.snapshotPages, expectedPages, complete: tracker.model.snapshotComplete })
  const deadline = performance.now() + 30000
  let lastUi
  while (performance.now() < deadline) {
    assertNoUnhandledApiRoutes(tracker)
    const ui = await projectUiSignals(page, anchorId)
    lastUi = ui
    if (representativeReadiness({
      modelComplete: modelReady(),
      activeRequests: tracker.active,
      loading: ui.loading,
      focused: ui.focused,
      anchorVisible: ui.anchorVisible
    })) {
      await waitForTransportQuiescence(page, tracker, label)
      const stableUi = await projectUiSignals(page, anchorId)
      lastUi = stableUi
      if (representativeReadiness({ modelComplete: modelReady(), activeRequests: tracker.active, loading: stableUi.loading, focused: stableUi.focused, anchorVisible: stableUi.anchorVisible })) return
    }
    await page.waitForTimeout(25)
  }
  let ui, timer
  try { ui = await Promise.race([page.evaluate(workspaceReadinessSnapshot, anchorId), new Promise((_, reject) => { timer = setTimeout(() => reject(new Error('Workspace diagnostic capture exceeded 1000ms')), 1000) })]) }
  catch (error) { ui = { captureError: error.message.slice(0, 256) } }
  finally { clearTimeout(timer) }
  const diagnostic = { label, lastPoll: lastUi, ui, modelReady: modelReady(), model: { profile: tracker.model.snapshotProfile, items: tracker.model.snapshotItems, expectedItems: expectedSnapshotItems, pages: tracker.model.snapshotPages, expectedPages, complete: tracker.model.snapshotComplete },
    active: tracker.active, activeBranches: tracker.activeBranches, activeRootBranches: tracker.activeRootBranches,
    arrivals: tracker.requests.slice(-8).map(request => ({ method: request.method, url: request.url.slice(0, 512), done: request.done ?? null, fulfilled: request.navigationFulfilled ?? request.timeline?.response.fulfilled ?? null, status: request.navigationStatus ?? request.timeline?.response.status ?? null })) }
  throw Object.assign(new Error(`${label} readiness failed: ${tracker.model.snapshotProfile} snapshot ${tracker.model.snapshotItems}/${expectedSnapshotItems} shell items, ${tracker.model.snapshotPages}/${expectedPages} pages, complete=${tracker.model.snapshotComplete}, navigation=${tracker.counts['navigation:branch'] || 0}, active=${tracker.active}, anchor=${anchorId}; UI ${JSON.stringify(diagnostic)}`), { workspaceReadiness: diagnostic })
}

export async function waitForFirstUsefulViewport(page, tracker, fixture, anchorId) {
  const deadline = performance.now() + 30000
  while (performance.now() < deadline) {
    assertNoUnhandledApiRoutes(tracker)
    const startedAt = performance.now()
    const observedLifetime = tracker.paintReadiness.lifetime()
    const observation = await page.evaluate(async id => {
      const startedAt = performance.now()
      await new Promise(resolve => requestAnimationFrame(() => requestAnimationFrame(resolve)))
      const frameAt = performance.now()
      const anchor = document.getElementById('entity-' + id), scroll = document.getElementById('main-content-scroll')
      const row = anchor?.getBoundingClientRect(), viewport = scroll?.getBoundingClientRect()
      const anchorVisible = !!(row && viewport && row.height > 0 && row.bottom > viewport.top && row.top < viewport.bottom)
      const loading = !!document.querySelector('.loading-indicator')
      const painted = anchorVisible && !loading
      const receipts = performance.getEntriesByType('resource').flatMap(entry => (entry.serverTiming || [])
        .filter(metric => metric.name === 'hmem_receipt')
        .map(metric => ({ id: metric.description, url: entry.name, startTime: entry.startTime, responseEnd: entry.responseEnd })))
      let domStamp = null
      try { domStamp = JSON.parse(document.getElementById('hierarchy-viewport')?.dataset.hierarchyContext) } catch {}
      return { painted, anchorVisible, loading, domStamp, origin: location.origin, receipts, clock: { startedAt, frameAt, endedAt: performance.now() } }
    }, anchorId)
    const returnedAt = performance.now()
    const ready = observation?.painted && tracker.paintReadiness.readyAt(observedLifetime, observation)
    tracker.coldDiagnostics.evaluation({ startedAt, returnedAt, ready: ready === true,
      ...(observation?.painted ? tracker.paintDecision() : { reason: 'unusable-dom', observedLifetime, currentLifetime: tracker.paintReadiness.lifetime(),
        observation: { clock: observation?.clock, origin: observation?.origin, receipts: observation?.receipts?.slice(0, 8),
          receiptCount: observation?.receipts?.length ?? 0, omittedReceipts: Math.max(0, (observation?.receipts?.length ?? 0) - 8) } }),
      dom: { anchorVisible: observation?.anchorVisible, loading: observation?.loading, stamp: observation?.domStamp } })
    if (ready) return
  }
  throw new Error('Authorized shell and first painted root anchor were not ready')
}

export async function navigateToFirstUsefulViewport(page, origin, tracker, fixture, anchorId) {
  await page.goto(`${origin}/workspace/${fixture.workspace.id}`, { waitUntil: 'domcontentloaded' })
  await waitForFirstUsefulViewport(page, tracker, fixture, anchorId)
}

function advertisedNavigationOwners(fixture) {
  return [...new Set([
    ...fixture.projects.filter(item => item.parent_id != null).map(item => 'project:' + item.parent_id),
    ...fixture.tasks.map(item => item.parent_id ? 'task:' + item.parent_id : item.project_id ? 'project:' + item.project_id : null).filter(Boolean)
  ])]
}

// Only this benchmark's updated-entity frames are supported. Expected next
// placement/count/rank metadata comes from fixture fields, never response IDs.
export function liveNavigationInvalidatedOwners(fixture, ledger, frames, url) {
  if (!(ledger instanceof Map) || !(url instanceof URL) || url.pathname !== `/api/v1/workspaces/${fixture.workspace.id}/navigation`
    || !['', 'all', 'projects', 'tasks'].includes(url.searchParams.get('show_only') || '')
    || !['', 'true'].includes(url.searchParams.get('show_empty_projects') || '')
    || !['', 'any', 'exact', 'above', 'below'].includes(url.searchParams.get('priority_mode') || '')
    || url.searchParams.getAll('project_status').some(value => !['active', 'paused', 'completed', 'archived'].includes(value))
    || url.searchParams.getAll('task_status').some(value => !['todo', 'in_progress', 'blocked', 'done', 'cancelled'].includes(value))) throw new Error('Unsupported live navigation context')
  const targets = new Set(), previous = new Map(ledger)
  const key = (kind, id) => kind + ':' + id
  for (const frame of frames) {
    const event = frame?.event, entity = event?.entity
    if (frame?.schema_version !== 1 || frame.type !== 'change' || event?.schema_version !== 1 || event.scope !== 'workspace' || event.workspace_id !== fixture.workspace.id
      || !['project', 'task', 'observation'].includes(entity?.type) || entity.action !== 'updated'
      || JSON.stringify(event.invalidations) !== JSON.stringify([{ kind: 'entity', target: key(entity.type, entity.id) }])) throw new Error('Unsupported live navigation mutation')
    const values = fixture[entity.type === 'project' ? 'projects' : entity.type === 'task' ? 'tasks' : 'observations']
    if (!values.some(item => item.id === entity.id)) throw new Error('Unknown live navigation fixture entity')
    if (entity.type !== 'observation') targets.add(key(entity.type, entity.id))
  }
  const owner = (kind, item) => item.parent_id ? key(kind, item.parent_id) : kind === 'task' && item.project_id ? key('project', item.project_id) : 'workspace_root'
  const projectRoots = [...targets].filter(value => value.startsWith('project:')).map(value => value.slice(8))
  const within = (id, root, seen = new Set()) => {
    if (id === root) return true
    if (seen.has(id)) throw new Error('Cyclic live navigation metadata')
    seen.add(id)
    const item = previous.get(key('project', id))
    return item?.parent_id ? within(item.parent_id, root, seen) : false
  }
  for (const [target, item] of previous) if (projectRoots.some(root => within(target.startsWith('project:') ? item.id : item.project_id, root))) targets.add(target)
  const next = new Map(previous)
  for (const target of targets) {
    const [kind, id] = target.split(':'), item = fixture[kind + 's'].find(item => item.id === id)
    if (!item) throw new Error('Unsupported live navigation deletion')
    const directProjects = kind === 'project' ? fixture.projects.filter(child => child.parent_id === id).length : 0
    const directTasks = fixture.tasks.filter(child => kind === 'task' ? child.parent_id === id : child.project_id === id && child.parent_id == null)
    next.set(target, { ...item, direct_project_count: directProjects, direct_task_count: directTasks.length, direct_subtask_count: directTasks.length,
      has_children: directProjects + directTasks.length > 0, readiness_rollup: { in_progress_subtask_count: directTasks.filter(child => child.status === 'in_progress').length } })
  }
  const filtered = url.searchParams.get('show_empty_projects') === 'false' || !['', 'all'].includes(url.searchParams.get('show_only') || '')
    || url.searchParams.getAll('project_status').length > 0 || url.searchParams.getAll('task_status').length > 0
    || !['', 'any'].includes(url.searchParams.get('priority_mode') || '') || !!url.searchParams.get('query')?.trim()
  const queried = !!url.searchParams.get('query')?.trim(), owners = new Set()
  const path = (source, target, seen = new Set()) => {
    if (seen.has(target)) throw new Error('Cyclic live navigation owner path')
    seen.add(target); owners.add(target)
    if (!filtered || target === 'workspace_root') return
    const [kind] = target.split(':'), item = source.get(target)
    path(source, item ? owner(kind, item) : 'workspace_root', seen)
  }
  const rank = item => item.status === 'blocked' && (item.readiness_rollup?.in_progress_subtask_count || 0) > 0 ? 'in_progress' : item.status
  for (const target of targets) {
    const [kind] = target.split(':'), old = previous.get(target), item = next.get(target)
    if (!old) path(next, owner(kind, item))
    else {
      const placement = old.parent_id !== item.parent_id || old.status !== item.status || old.priority !== item.priority
        || String(kind === 'project' ? old.name : old.title).toLowerCase() !== String(kind === 'project' ? item.name : item.title).toLowerCase()
        || (kind === 'task' && (old.project_id !== item.project_id || rank(old) !== rank(item))) || queried
      const children = old.has_children !== item.has_children || (kind === 'project'
        ? old.direct_project_count !== item.direct_project_count || old.direct_task_count !== item.direct_task_count
        : old.direct_subtask_count !== item.direct_subtask_count)
      if (placement) { path(previous, owner(kind, old)); path(next, owner(kind, item)) }
      if (children) path(next, target)
    }
  }
  return [...owners].sort()
}

function rememberNavigationMetadata(tracker, receipt, value) {
  if (!tracker.navigationMetadata || receipt.metadataEpoch !== tracker.metadataEpoch) return
  const remember = (kind, items) => { for (const item of items) if (item.workspace_id === tracker.workspaceId) tracker.navigationMetadata.set(kind + ':' + item.id, item) }
  if (receipt.key === 'navigation:branch') {
    const stamp = receipt.proofNavigation
    if (!stamp || stamp.retired || !stamp.done || stamp.error || stamp.state?.latest !== stamp) return
    remember('project', value.projects.items); remember('task', value.tasks.items)
  } else if (receipt.key === 'navigation:summaries') {
    remember('project', value.projects); remember('task', value.tasks)
  } else if (receipt.key === 'navigation:focus') {
    for (const item of [value.target, ...value.ancestors]) remember(item.entity_type, [item.summary])
  }
}

export function prepareLiveNavigationProof(tracker, fixture, frames) {
  // The supported scenario has one delivery and no earlier canonical summary or
  // focus read in this tracker. Separate focus scenarios use separate trackers.
  // Enforce that prefix rather than approximating Elm's per-target read guards.
  if (tracker.liveNavigationPrepared) throw new Error('Live navigation proof delivery is one-shot')
  const root = tracker.requests.findLast(request => request.key === 'navigation:branch' && new URL(request.url).searchParams.get('parent_kind') === 'workspace_root')
  if (!root || !root.done || !root.navigationFulfilled || root.navigationStatus !== 200 || tracker.active !== 0 || !tracker.navigationProof?.ready()) throw new Error('Live navigation context is not settled')
  if ((tracker.counts['navigation:summaries'] || 0) !== 0 || (tracker.counts['navigation:focus'] || 0) !== 0
    || tracker.requests.some(request => request.method !== 'GET' && !(request.method === 'POST'
      && ['change-stream:resync', 'change-stream:ticket', 'observations:count', 'observations:match', 'navigation:batch'].includes(request.key)))) throw new Error('Unsupported live navigation read/mutation prefix')
  assertNoUnhandledApiRoutes(tracker)
  const owners = liveNavigationInvalidatedOwners(fixture, tracker.navigationMetadata, frames, new URL(root.url))
  tracker.liveNavigationPrepared = true
  tracker.metadataEpoch++
  tracker.navigationProof.invalidateOwners(owners)
  return owners
}

// This oracle reads fixture fields independently of the response producer. Task
// ancestors are retained by matching descendants; projects retain the existing
// outer lifecycle/priority gate when empty projects are shown.
function filteredNavigationMembers(fixture, url) {
  const showOnly = url.searchParams.get('show_only') || 'all'
  const statuses = kind => url.searchParams.getAll(kind + '_status')
  const priority = item => {
    const mode = url.searchParams.get('priority_mode') || 'any', value = Number(url.searchParams.get('priority_value'))
    return mode === 'any' || (Number.isInteger(value) && ((mode === 'exact' && item.priority === value)
      || (mode === 'above' && item.priority > value) || (mode === 'below' && item.priority < value)))
  }
  const lifecycle = (kind, item) => !statuses(kind).length || statuses(kind).includes(item.status)
  const needle = (url.searchParams.get('query') || '').trim().toLowerCase()
  const matches = (kind, item) => lifecycle(kind, item) && priority(item)
    && [kind === 'project' ? item.name : item.title, item.description].some(value => String(value || '').toLowerCase().includes(needle))
  const projectChildren = new Map(), taskChildren = new Map(), projectTasks = new Map()
  const append = (groups, owner, item) => { if (!groups.has(owner)) groups.set(owner, []); groups.get(owner).push(item) }
  for (const item of fixture.projects) append(projectChildren, item.parent_id, item)
  for (const item of fixture.tasks) {
    append(taskChildren, item.parent_id, item)
    if (item.project_id != null) append(projectTasks, item.project_id, item)
  }
  const recursive = calculate => {
    const memo = new Map(), visiting = new Set()
    const read = item => {
      if (memo.has(item.id)) return memo.get(item.id)
      if (visiting.has(item.id)) throw new Error('Cyclic navigation fixture')
      visiting.add(item.id)
      const result = calculate(item, read)
      visiting.delete(item.id); memo.set(item.id, result)
      return result
    }
    return read
  }
  const taskMatch = recursive((item, read) => matches('task', item) || (taskChildren.get(item.id) || []).some(read))
  const projectMatch = recursive((item, read) => matches('project', item) || (projectChildren.get(item.id) || []).some(read))
  const projectTaskMatch = recursive((item, read) => (projectTasks.get(item.id) || []).some(taskMatch) || (projectChildren.get(item.id) || []).some(read))
  return {
    projects: fixture.projects.filter(item => showOnly !== 'tasks' && (url.searchParams.get('show_empty_projects') === 'false'
      ? projectTaskMatch(item)
      : lifecycle('project', item) && priority(item) && (projectMatch(item) || (showOnly !== 'projects' && projectTaskMatch(item))))),
    tasks: fixture.tasks.filter(item => showOnly !== 'projects' && taskMatch(item))
  }
}

export function completedExpandedMembership(tracker, fixture) {
  const navigation = navigationPageReceipts(tracker).filter(request => request.navigation)
  const advertised = new Set(advertisedNavigationOwners(fixture))
  const textOrder = (a, b) => a < b ? -1 : a > b ? 1 : 0
  const ordered = (kind, values) => [...values].sort((a, b) => {
    const rank = kind === 'project' ? { active: 0, paused: 1, completed: 2, archived: 3 } : { todo: 0, in_progress: 1, blocked: 2, done: 3, cancelled: 4 }
    const title = item => (kind === 'project' ? item.name : item.title).toLowerCase()
    return (rank[a.status] - rank[b.status]) || b.priority - a.priority || textOrder(title(a), title(b)) || textOrder(a.id, b.id)
  }).map(item => item.id)
  const rootRequests = navigation.filter(request => new URL(request.url).searchParams.get('parent_kind') === 'workspace_root')
  if (!rootRequests.length) throw new Error('Missing demand-driven root page')
  const first = new URL(rootRequests[0].url)
  const context = url => navigationFilterContext(url) + '|' + (url.searchParams.get('show_empty_projects') || 'true')
  const expectedContext = context(first), members = filteredNavigationMembers(fixture, first)
  const pagesFor = (requests, kind) => requests.flatMap(request => {
    assertLinkedNavigationPage(tracker, request)
    const url = new URL(request.url), flag = 'include_' + kind + 's', values = url.searchParams.getAll(flag)
    if (context(url) !== expectedContext) throw new Error('Mixed navigation filter lifetimes')
    if (request.done !== true || request.navigationFulfilled !== true || request.navigationStatus !== 200) throw new Error('Unfulfilled navigation membership page')
    if ((request.method !== 'GET' && !(request.logical === true && request.method === 'POST' && request.counted === false && request.transportRequestId != null)) || url.pathname !== `/api/v1/workspaces/${fixture.workspace.id}/navigation` || request.navigation.workspace_id !== fixture.workspace.id
      || request.navigation.parent?.kind !== url.searchParams.get('parent_kind')
      || (url.searchParams.get('parent_kind') !== 'workspace_root' && request.navigation.parent?.parent_id !== url.searchParams.get('parent_id'))) throw new Error('Foreign navigation membership page')
    if (values.length > 1 || (values.length && !['true', 'false'].includes(values[0]))) throw new Error('Invalid navigation demand boolean')
    const page = request.navigation[kind + 's']
    if (!page || !Array.isArray(page.items) || typeof page.has_more !== 'boolean') throw new Error('Malformed navigation page')
    if (values[0] === 'false') {
      if (page.items.length || page.has_more || Object.keys(page).sort().join(',') !== 'has_more,items') throw new Error('Malformed skipped navigation page')
      return []
    }
    const offset = Number(url.searchParams.get(kind + '_offset')), limit = Number(url.searchParams.get(kind + '_limit'))
    if (!Number.isInteger(offset) || offset < 0 || limit !== 50) throw new Error('Invalid requested navigation pagination')
    return [{ offset, limit, ids: page.items.map(item => item.id), hasMore: page.has_more, done: request.done }]
  })
  // Validate skipped shapes and contexts even on pages outside the displayed tree.
  for (const kind of ['project', 'task']) pagesFor(navigation, kind)
  const projectIds = new Set(), taskIds = new Set()
  for (const [kind, expected, collected] of [
    ['project', ordered('project', members.projects.filter(item => item.parent_id == null)), projectIds],
    ['task', ordered('task', members.tasks.filter(item => item.parent_id == null && item.project_id == null)), taskIds]
  ]) {
    const pages = pagesFor(rootRequests, kind), prefix = []
    if (!pages.length) throw new Error('Missing requested root stream: ' + kind)
    let terminalPage = null
    for (const page of pages) {
      if (terminalPage && page.offset === terminalPage.offset && !page.hasMore && JSON.stringify(page.ids) === JSON.stringify(terminalPage.ids)) continue
      if (page.offset !== prefix.length || terminalPage || JSON.stringify(page.ids) !== JSON.stringify(expected.slice(page.offset, page.offset + 50))
        || page.hasMore !== (page.offset + 50 < expected.length)) throw new Error('Root membership/order mismatch: ' + kind)
      prefix.push(...page.ids)
      if (!page.hasMore) terminalPage = page
    }
    // Root prefixes remain manual demand; do not infer unrequested later pages.
    for (const id of prefix) collected.add(id)
  }
  const pending = [...projectIds].map(id => ['project', id]).concat([...taskIds].map(id => ['task', id]))
  const seen = new Set()
  let expandedBranches = 0, terminalStreams = 0
  for (let index = 0; index < pending.length; index++) {
    const [kind, id] = pending[index], key = kind + ':' + id
    if (seen.has(key)) continue
    seen.add(key)
    const expectedProjects = ordered('project', kind === 'project' ? members.projects.filter(item => item.parent_id === id) : [])
    const expectedTasks = ordered('task', members.tasks.filter(item => kind === 'task' ? item.parent_id === id : item.parent_id == null && item.project_id === id))
    if (expectedProjects.length === 0 && expectedTasks.length === 0 && !advertised.has(key)) continue
    expandedBranches += 1
    const pages = navigation.filter(request => { const url = new URL(request.url); return url.searchParams.get('parent_kind') === kind && url.searchParams.get('parent_id') === id })
    for (const [childKind, expected, collected] of [['project', expectedProjects, projectIds], ['task', expectedTasks, taskIds]]) {
      const stream = assertCompleteNavigationStream(pagesFor(pages, childKind), expected, key + ':' + childKind)
      terminalStreams += 1
      for (const child of stream.ids) { collected.add(child); pending.push([childKind, child]) }
    }
  }
  return { expandedBranches, projectIds: projectIds.size, taskIds: taskIds.size, terminalStreams, membershipOrdered: true, entityKeys: [...projectIds].map(id => 'project:' + id).concat([...taskIds].map(id => 'task:' + id)) }
}

export const SETTLED_NAVIGATION_FEEDBACK_CONTRACT = Object.freeze({
  revision: 'settled-root-continuation.v1', rootKinds: ['project', 'task'], rootPageSize: 50,
  rootFilters: { show_empty_projects: 'true', priority_mode: 'any' },
  rootDemand: 'Enabled Load more controls corroborated by current admitted fulfilled ordered root prefixes with remaining members.',
  expandedBranches: 'All admitted descendant streams are independently terminal; every branch feedback row fails.'
})

export function settledNavigationSource() {
  const files = ['src/Api.elm', 'src/Feature/DataLoading.elm', 'src/Feature/Cards.elm']
  const api = fs.readFileSync(path.join(frontendRoot, files[0]), 'utf8')
  const loading = fs.readFileSync(path.join(frontendRoot, files[1]), 'utf8')
  const cards = fs.readFileSync(path.join(frontendRoot, files[2]), 'utf8')
  if (!api.includes('/navigation?parent_kind=') || !api.includes('&project_limit=50&project_offset=') || !api.includes('&task_limit=50&task_offset=')
    || !loading.includes('&show_empty_projects=') || !loading.includes('&priority_mode=any')
    || !cards.includes('viewViewportRootStatus') || !cards.includes('LoadRootNavigationPage kind')) throw new Error('Settled root navigation source contract changed')
  return files.map(relative => { const file = path.join(frontendRoot, relative); return { path: normalizedRepositoryPath(file), sha256: sha256File(file) } })
}

export function hierarchyFeedbackRows() {
  return [...document.querySelectorAll('[data-hierarchy-key^="status:"], [data-hierarchy-key^="root-status:"]')].map(row => ({
    key: row.dataset.hierarchyKey.slice(0, 256), text: row.textContent.trim().slice(0, 256),
    status: !!row.querySelector('[role="status"],.loading-indicator,.card-description-error,.hierarchy-branch-status'),
    buttons: [...row.querySelectorAll('button')].slice(0, 3).map(button => ({ text: button.textContent.trim().slice(0, 128), disabled: button.disabled,
      continuation: button.classList.contains('navigation-load-more') }))
  }))
}

export function assertSettledHierarchyFeedback(rows, tracker, fixture) {
  const failure = reason => { throw Object.assign(new Error('Settled expanded hierarchy feedback: ' + reason), { navigationFeedback: { reason, rows: Array.isArray(rows) ? rows.slice(0, 4) : null, activeRootBranches: tracker.activeRootBranches } }) }
  if (!Array.isArray(rows)) failure('Missing actual row snapshot')
  if (!rows.length) return { rootContinuationKinds: [] }
  if (!tracker.navigationProof?.ready() || tracker.activeRootBranches !== 0) failure('Navigation lifetime is not settled')
  const roots = tracker.requests.filter(request => request.key === 'navigation:branch' && new URL(request.url).searchParams.get('parent_kind') === 'workspace_root')
  const latest = roots.at(-1)?.proofNavigation
  if (!latest || latest.owner !== 'workspace_root' || latest.retired || !latest.done || latest.error || latest.state?.latest !== latest || latest.state.unknown) failure('Missing current admitted root lifetime')
  const latestRequest = roots.findLast(request => request.proofNavigation === latest)
  const expected = expectedNavigationChildren({ ...fixture, ...filteredNavigationMembers(fixture, new URL(latestRequest.url)) }).workspace_root
  const current = roots.filter(request => request.proofNavigation?.epoch === latest.epoch && request.proofNavigation?.rootEpoch === latest.rootEpoch && request.proofNavigation?.state === latest.state)
  const kinds = []
  for (const row of rows) {
    const kind = row.key === 'root-status:project' ? 'project' : row.key === 'root-status:task' ? 'task' : null
    if (!kind || kinds.includes(kind) || row.status !== false || row.text !== 'Load more ' + kind + 's' || row.buttons?.length !== 1
      || row.buttons[0].text !== row.text || row.buttons[0].disabled !== false || row.buttons[0].continuation !== true) failure('Unexpected loading, error, branch, malformed or disabled row')
    const state = latest.state.kinds[kind]
    if (!state || state.pending || state.paused || state.more !== true) failure('Root kind has no settled continuation')
    const pages = current.filter(request => request.proofNavigation.slots[kind]).sort((a, b) => a.proofNavigation.slots[kind].offset - b.proofNavigation.slots[kind].offset)
    const prefix = []
    for (const request of pages) {
      const url = new URL(request.url), stamp = request.proofNavigation, slot = stamp.slots[kind], page = request.navigation?.[kind + 's']
      const ids = page?.items?.map(item => item.id)
      const parameters = [...url.searchParams.keys()]
      const required = ['parent_kind', 'project_limit', 'project_offset', 'task_limit', 'task_offset', 'show_empty_projects', 'priority_mode']
      const demand = ['include_projects', 'include_tasks'], statuses = ['project_status', 'task_status'], allowed = [...required, ...demand, ...statuses]
      if ((request.method !== 'GET' && !(request.logical === true && request.method === 'POST' && request.counted === false && request.transportRequestId != null)) || url.pathname !== `/api/v1/workspaces/${fixture.workspace.id}/navigation`
        || required.some(name => !url.searchParams.has(name)) || parameters.filter(name => !statuses.includes(name)).some(name => url.searchParams.getAll(name).length !== 1)
        || statuses.some(name => new Set(url.searchParams.getAll(name)).size !== url.searchParams.getAll(name).length) || parameters.some(name => !allowed.includes(name))
        || demand.some(name => url.searchParams.has(name) && !['true', 'false'].includes(url.searchParams.get(name)))
        || url.searchParams.get('include_' + kind + 's') === 'false'
        || url.searchParams.get('parent_kind') !== 'workspace_root' || url.searchParams.get('show_empty_projects') !== 'true'
        || ['project', 'task'].some(kind => url.searchParams.get(kind + '_limit') !== '50' || url.searchParams.get(kind + '_offset') !== String(stamp[kind + 'Offset']))
        || navigationFilterContext(url) !== latest.context
        || stamp.retired || stamp.context !== navigationFilterContext(url) || !stamp.done || stamp.error
        || request.done !== true || request.navigationFulfilled !== true || request.navigationStatus !== 200
        || !slot.done || slot.error || slot.limit !== 50 || slot.offset !== prefix.length
        || url.searchParams.get(kind + '_offset') !== String(slot.offset) || url.searchParams.get(kind + '_limit') !== '50'
        || JSON.stringify(ids) !== JSON.stringify(expected[kind + 's'].slice(slot.offset, slot.offset + 50))
        || page.has_more !== (slot.offset + 50 < expected[kind + 's'].length)
        || JSON.stringify(slot.stream?.ids) !== JSON.stringify(ids) || slot.stream.hasMore !== page.has_more) failure('Root wire/admission/order/prefix mismatch')
      prefix.push(...ids)
    }
    if (!prefix.length || prefix.length >= expected[kind + 's'].length || pages.at(-1).navigation[kind + 's'].has_more !== true) failure('No independently expected remaining root members')
    kinds.push(kind)
  }
  return { rootContinuationKinds: kinds }
}

export function collectSettledHierarchyFeedback(feedback, rows, tracker, fixture) {
  if (feedback.failure) throw feedback.failure
  try { assertSettledHierarchyFeedback(rows, tracker, fixture) }
  catch (error) { feedback.failure = error; throw error }
  for (const row of rows) feedback.set(row.key, row)
}

export async function scrollExpandedHierarchy(page, expectedKeys, tracker, fixture) {
  const keys = new Set(), deadline = performance.now() + 30000
  const feedback = new Map()
  await page.locator('#main-content-scroll').evaluate(element => { element.scrollTop = 0 })
  let end = false
  while (performance.now() < deadline) {
    await page.evaluate(() => new Promise(resolve => requestAnimationFrame(() => requestAnimationFrame(resolve))))
    for (const key of await page.locator('[data-hierarchy-key]').evaluateAll(rows => rows.map(row => row.dataset.hierarchyKey))) keys.add(key)
    collectSettledHierarchyFeedback(feedback, await page.evaluate(hierarchyFeedbackRows), tracker, fixture)
    if (end) break
    end = await page.locator('#main-content-scroll').evaluate(element => { const before = element.scrollTop; element.scrollTop += element.clientHeight / 2; return element.scrollTop === before })
  }
  if (!end) throw new Error('Finite expanded hierarchy scroll did not reach its end')
  const missing = expectedKeys.filter(key => !keys.has(key))
  if (missing.length) throw new Error('Expanded members were not scroll-reachable: ' + missing.slice(0, 5).join(', '))
  const settledFeedback = assertSettledHierarchyFeedback([...feedback.values()], tracker, fixture)
  await page.locator('#main-content-scroll').evaluate(element => { element.scrollTop = 0 })
  await page.evaluate(() => new Promise(resolve => requestAnimationFrame(() => requestAnimationFrame(resolve))))
  return { reachedEnd: true, distinctMountedKeys: keys.size, ...settledFeedback }
}

async function restoreWorkspace(page, tracker, fixture, anchorId, label) {
  await page.evaluate(() => { window.location.hash = 'tab=projects' })
  await waitForWorkspaceReady(page, tracker, fixture, anchorId, label)
}


function navigationFilterContext(url) {
  return JSON.stringify(['show_only', 'priority_mode', 'priority_value', 'query', 'project_status', 'task_status'].map(key => [key, url.searchParams.getAll(key)]))
}

function expectedNavigationChildren(fixture) {
  const children = { workspace_root: { projects: [], tasks: [] } }
  for (const project of fixture.projects) children['project:' + project.id] = { projects: [], tasks: [] }
  for (const task of fixture.tasks) children['task:' + task.id] = { projects: [], tasks: [] }
  for (const project of fixture.projects) children[project.parent_id ? 'project:' + project.parent_id : 'workspace_root']?.projects.push(project)
  for (const task of fixture.tasks) {
    const owner = task.parent_id ? 'task:' + task.parent_id : task.project_id ? 'project:' + task.project_id : 'workspace_root'
    if (owner) children[owner]?.tasks.push(task)
  }
  const order = (kind, values) => [...values].sort((a, b) => {
    const rank = kind === 'project' ? { active: 0, paused: 1, completed: 2, archived: 3 } : { todo: 0, in_progress: 1, blocked: 2, done: 3, cancelled: 4 }
    const title = item => (kind === 'project' ? item.name : item.title).toLowerCase()
    const first = title(a), second = title(b)
    return rank[a.status] - rank[b.status] || b.priority - a.priority || (first < second ? -1 : first > second ? 1 : a.id < b.id ? -1 : a.id > b.id ? 1 : 0)
  }).map(item => item.id)
  return Object.fromEntries(Object.entries(children).map(([key, value]) => [key, { projects: order('project', value.projects), tasks: order('task', value.tasks) }]))
}

export async function waitForLiveSettle(page, tracker, before, anchorId, logicalReady, timeoutMs = 30000) {
  const deadline = performance.now() + timeoutMs
  while (performance.now() < deadline) {
    assertNoUnhandledApiRoutes(tracker)
    const ui = await projectUiSignals(page, anchorId)
    const dispatchTurnComplete = await page.evaluate(() => window.__perfLastPush?.dispatchTurnComplete === true)
    const followUpRequests = tracker.requests.length - before.index
    const proofStartedAt = tracker.liveDiagnostics ? performance.now() : null
    const logicalNavigationComplete = logicalReady()
    const proofEndedAt = tracker.liveDiagnostics ? performance.now() : null
    const state = { dispatchTurnComplete, activeRequests: tracker.active, loading: ui.loading, focused: ui.focused, anchorVisible: ui.anchorVisible, followUpRequests, logicalNavigationComplete }
    const ready = liveSettleReady(state)
    tracker.liveDiagnostics?.readiness(performance.now(), state, ready, { startedAt: proofStartedAt, endedAt: proofEndedAt })
    if (ready) {
      tracker.liveDiagnostics?.paintStarted(performance.now())
      await page.evaluate(() => new Promise(resolve => requestAnimationFrame(() => requestAnimationFrame(resolve))))
      const paintEndedAt = tracker.liveDiagnostics ? performance.now() : null
      const recheckStartedAt = tracker.liveDiagnostics && tracker.active === 0 ? performance.now() : null
      const paintReady = tracker.active === 0 && logicalReady()
      const recheckEndedAt = recheckStartedAt === null ? null : performance.now()
      tracker.liveDiagnostics?.paintEnded(paintEndedAt, tracker.active, paintReady, recheckStartedAt === null ? null : { startedAt: recheckStartedAt, endedAt: recheckEndedAt })
      if (paintReady) return performance.now()
    }
    await page.waitForTimeout(10)
  }
  throw new Error(`live batch did not settle within ${timeoutMs}ms`)
}

async function assertLiveStability(page, tracker, durationMs = 500) {
  const startedAt = performance.now()
  const requestCount = tracker.requests.length
  const completed = tracker.completed
  await page.waitForTimeout(durationMs)
  await page.evaluate(() => new Promise(resolve => requestAnimationFrame(() => requestAnimationFrame(resolve))))
  assertNoUnhandledApiRoutes(tracker)
  if (tracker.active !== 0 || tracker.requests.length !== requestCount || tracker.completed !== completed) {
    const late = tracker.requests.slice(requestCount).map(request => `${request.method} ${request.url}`)
    throw new Error(`late live activity during ${durationMs}ms stability window: ${late.join(', ') || `active=${tracker.active}`}`)
  }
  return { startedAt, endedAt: performance.now(), requests: 0 }
}

async function domMetrics(page) {
  return page.evaluate(() => ({
    nodes: document.querySelectorAll('*').length,
    rows: document.querySelectorAll('.card-project,.card-task,.card-subtask,.observation-card,.timeline-event-card,.timeline-value-table tbody tr,.timeline-point-control').length,
    projects: document.querySelectorAll('.card-project').length,
    tasks: document.querySelectorAll('.card-task,.card-subtask').length,
    observations: document.querySelectorAll('.observation-card').length,
    timelineEvents: document.querySelectorAll('.timeline-event-card').length,
    timelineRows: document.querySelectorAll('.timeline-value-table tbody tr,.timeline-point-control').length
  }))
}

function requestSnapshot(tracker) {
  return { index: tracker.requests.length, completed: tracker.completed, counts: { ...tracker.counts } }
}

function requestDelta(tracker, before) {
  const requests = tracker.requests.slice(before.index)
  return {
    count: requests.length,
    bytes: requests.reduce((sum, request) => sum + request.bytes, 0),
    routes: requests.reduce((acc, request) => ({ ...acc, [request.key]: (acc[request.key] || 0) + 1 }), {}),
    routeBytes: requests.reduce((acc, request) => ({ ...acc, [request.key]: (acc[request.key] || 0) + request.bytes }), {})
  }
}

export function observationPaginationSource() {
  const file = path.join(frontendRoot, 'src/Feature/Observation.elm')
  const source = fs.readFileSync(file, 'utf8')
  const pageSize = Number(source.match(/pageSize\s*:\s*Int\s+pageSize\s*=\s*(\d+)/)?.[1])
  if (pageSize !== OBSERVATION_PAGINATION_CONTRACT.pageSize) throw new Error('Observation pagination source contract changed')
  return { path: normalizedRepositoryPath(file), symbol: 'pageSize', pageSize, sha256: sha256File(file) }
}

export function liveObservationRefreshSource() {
  const files = ['src/Feature/Observation.elm', 'src/Feature/WebSocket.elm', 'src/Feature/DataLoading.elm']
  const observation = fs.readFileSync(path.join(frontendRoot, files[0]), 'utf8')
  const socket = fs.readFileSync(path.join(frontendRoot, files[1]), 'utf8')
  if (!observation.includes('targetOffset = Basics.max pageSize') || !observation.includes('next < old.targetOffset')
    || !observation.includes('fetchRefreshPage 0 prepared') || !observation.includes('requestSessionEpoch = sessionEpoch')
    || !socket.includes('Observation.refreshActiveResults')) throw new Error('Live retained Observation refresh source changed')
  return files.map(relative => { const file = path.join(frontendRoot, relative); return { path: normalizedRepositoryPath(file), sha256: sha256File(file) } })
}

export function observationResetProof(tracker, fixture, page, expected) {
  const request = tracker.requests.findLast(request => request.observationPage === page)
  if (!request) throw new Error('Executed reset has no actual request receipt')
  return { runId: tracker.paintRunId, workspaceId: fixture.workspace.id, requestId: request.requestId, requestIndex: request.requestIndex,
    snapshotGeneration: tracker.model.snapshotGeneration, receipt: request, membership: { executed: true, expected },
    recipe: 'reset-timeline-projects-separate-focus-projects-live.v1', recipeCompleted: false }
}

export function assertObservationPagination(tracker, before, fixture, offset, query, label, queryScope = null) {
  const pages = tracker.requests.slice(before.index).filter(request => request.key === 'observations:list')
  if (pages.length !== 1) throw new Error(label + ' requires exactly one actual Observation list page, observed ' + pages.length)
  const expected = queryObservations(fixture, { query, queryScope, offset, limit: OBSERVATION_PAGINATION_CONTRACT.pageSize })
  return assertObservationPageReceipt(pages[0].observationPage, { workspaceId: fixture.workspace.id, offset, query, queryScope,
    expectedIds: expected.items.map(item => item.id), hasMore: expected.has_more })
}

export function timelineSource() {
  const sources = ['src/Feature/Timeline.elm', 'src/Api.elm', 'src/Helpers.elm'].map(relative => {
    const file = path.join(frontendRoot, relative)
    return { path: normalizedRepositoryPath(file), sha256: sha256File(file) }
  })
  const source = fs.readFileSync(path.join(frontendRoot, 'src/Feature/Timeline.elm'), 'utf8')
  if (!/histogramWindow = "30"/.test(source) || !/histogramBucket = "week"/.test(source)
    || !source.includes('Maybe.withDefault 30 (String.toInt window) - 1') || !source.includes('nowMillis + millisPerDay')
    || !source.includes('timeline-point-control') || !source.includes('Lifecycle activity')
    || !source.includes('Lifecycle line actions') || !source.includes('Lifecycle line entities')
    || !source.includes('Svg.text (actionLabel ++ " by interval")') || !source.includes(TIMELINE_CURRENT_CONTRACT.description)
    || !source.includes('key = "observations", label = "Observations"')) throw new Error('Timeline current source contract changed')
  return { window: '30', bucket: 'week', sources }
}

export function assertTimelineTransport(tracker, before, fixture, options) {
  const requests = tracker.requests.slice(before.index)
  const expectedEvents = queryTimelineEvents(fixture, { limit: 50 })
  const expectedBuckets = queryTimelineBuckets(fixture, TIMELINE_CURRENT_CONTRACT.query)
  const receipts = {}
  for (const [kind, value] of [['events', expectedEvents], ['buckets', expectedBuckets]]) {
    const actual = requests.filter(request => request.key === 'timeline:' + kind)
    if (actual.length !== 1) throw new Error('Timeline requires exactly one actual ' + kind + ' response, observed ' + actual.length)
    receipts[kind] = assertTimelineReceipt(actual[0].timeline, fixture.workspace.id, kind, value)
  }
  if (tracker.model.timelineEvents !== expectedEvents.items.length || tracker.model.timelineBuckets !== expectedBuckets.buckets.length
    || JSON.stringify(tracker.model.timelineBucketRequest) !== JSON.stringify(TIMELINE_CURRENT_CONTRACT.query)) throw new Error('Timeline intercepted model does not match the current fulfilled DTOs')
  return { receipts, chart: timelineChartExpectation(expectedEvents, expectedBuckets, options) }
}

export function timelineDomReady(expected) {
  const panel = document.querySelector('.timeline-panel')
  if (!panel || document.querySelector('.timeline-panel .loading-indicator,.timeline-graph-error,.timeline-stale-notice,.timeline-state .btn-secondary,.timeline-selection-note')
    || [...document.querySelectorAll('.timeline-graph-state')].some(node => node.textContent.includes('Loading'))
    || JSON.stringify([...document.querySelectorAll('.timeline-event-card')].map(node => node.id)) !== JSON.stringify(expected.eventIds)) return false
  const charts = [...document.querySelectorAll('svg.timeline-line-chart')]
  if (charts.length !== 1 || charts[0].namespaceURI !== 'http://www.w3.org/2000/svg'
    || charts[0].getAttribute('aria-labelledby') !== 'timeline-chart-lifecycle-title timeline-chart-lifecycle-description'
    || document.getElementById('timeline-chart-lifecycle-title')?.textContent !== expected.title
    || document.getElementById('timeline-chart-lifecycle-description')?.textContent !== expected.description) return false
  const controls = [...document.querySelectorAll('.timeline-graph-controls select')]
  if (controls.length !== 2 || controls[0].value !== expected.window || controls[1].value !== expected.bucket) return false
  const paths = [...document.querySelectorAll('svg.timeline-line-chart path.timeline-line')]
  if (paths.length !== expected.series.length || !expected.series.every((action, index) => paths[index].classList.contains('timeline-series-' + action)
    && paths[index].namespaceURI === 'http://www.w3.org/2000/svg' && paths[index].getAttribute('d')?.trim())) return false
  const toggles = [...document.querySelectorAll('[aria-label="Lifecycle line actions"] .timeline-series-toggle')]
  if (toggles.length !== expected.actions.length || !expected.actions.every((action, index) => toggles[index].classList.contains('timeline-series-' + action) && toggles[index].getAttribute('aria-pressed') === 'true')) return false
  const entities = ['projects', 'tasks', 'subtasks', 'observations']
  const entityToggles = [...document.querySelectorAll('[aria-label="Lifecycle line entities"] .timeline-series-toggle')]
  if (entityToggles.length !== entities.length || !entities.every((entity, index) => entityToggles[index].classList.contains('timeline-series-' + entity) && entityToggles[index].getAttribute('aria-pressed') === 'true')) return false
  const points = [...document.querySelectorAll('.timeline-point-control')]
  return points.length === expected.points.length && expected.points.every((point, index) => {
    const node = points[index]
    return node.id === point.id && node.namespaceURI === 'http://www.w3.org/2000/svg'
      && node.getAttribute('role') === point.role && node.getAttribute('tabindex') === point.tabindex
      && node.getAttribute('aria-pressed') === point.pressed && node.getAttribute('aria-label') === point.label
      && node.querySelectorAll('circle.timeline-point-hitarea').length === 1 && node.querySelectorAll('circle.timeline-point').length === 1
  })
}

export function timelineDomSnapshot() {
  const text = selector => document.querySelector(selector)?.textContent?.slice(0, 256) || null
  const toggles = (label, limit) => [...document.querySelectorAll('[aria-label="' + label + '"] .timeline-series-toggle')].slice(0, limit).map(node => ({ classes: node.getAttribute('class')?.slice(0, 128) || null, pressed: node.getAttribute('aria-pressed')?.slice(0, 16) || null }))
  return { panel: !!document.querySelector('.timeline-panel'), eventCards: document.querySelectorAll('.timeline-event-card').length,
    charts: document.querySelectorAll('svg.timeline-line-chart').length, paths: document.querySelectorAll('path.timeline-line').length,
    points: document.querySelectorAll('.timeline-point-control').length, legacyRows: document.querySelectorAll('.timeline-value-table tbody tr').length,
    selectedPoints: document.querySelectorAll('.timeline-point-selected').length, selection: !!document.querySelector('.timeline-selection-note'),
    controls: [...document.querySelectorAll('.timeline-graph-controls select')].slice(0, 2).map(node => node.value.slice(0, 32)),
    loading: !!document.querySelector('.timeline-panel .loading-indicator') || [...document.querySelectorAll('.timeline-graph-state')].some(node => node.textContent.includes('Loading')),
    error: text('.timeline-graph-error,.timeline-stale-notice,.timeline-state .btn-secondary'),
    lifecycleToggles: toggles('Lifecycle line actions', 5), entityToggles: toggles('Lifecycle line entities', 4),
    pointLabels: [...document.querySelectorAll('.timeline-point-control')].slice(0, 25).map(node => node.getAttribute('aria-label')?.slice(0, 256) || null) }
}

export function timelineBrowserTimeZone() { return Intl.DateTimeFormat().resolvedOptions().timeZone }

export async function waitTimeline(page, tracker, before, fixture) {
  let timer
  try {
    const timeZone = await page.evaluate(timelineBrowserTimeZone)
    if (typeof timeZone !== 'string' || !timeZone) throw new Error('Timeline browser timezone is missing')
    const chart = timelineChartExpectation(queryTimelineEvents(fixture, { limit: 50 }), queryTimelineBuckets(fixture, TIMELINE_CURRENT_CONTRACT.query), { timeZone })
    await page.waitForFunction(timelineDomReady, chart, { timeout: 30000 })
    await waitForTransportQuiescence(page, tracker, 'Timeline')
    return assertTimelineTransport(tracker, before, fixture, { timeZone })
  } catch (error) {
    let dom
    try { dom = await Promise.race([page.evaluate(timelineDomSnapshot), new Promise((_, reject) => { timer = setTimeout(() => reject(new Error('Timeline diagnostic capture exceeded 1000ms')), 1000) })]) }
    catch (captureError) { dom = { captureError: captureError.message.slice(0, 256) } }
    finally { clearTimeout(timer) }
    const diagnostic = { phase: attemptState?.phase || 'Timeline', dom, active: tracker.active, model: { events: tracker.model.timelineEvents, buckets: tracker.model.timelineBuckets, request: tracker.model.timelineBucketRequest },
      requests: tracker.requests.slice(before.index).filter(request => request.key.startsWith('timeline:')).slice(0, 4).map(request => ({ method: request.method, url: request.url.slice(0, 512), fulfilled: request.timeline?.response.fulfilled ?? null, status: request.timeline?.response.status ?? null })) }
    throw Object.assign(new Error(error.message + '; Timeline ' + JSON.stringify(diagnostic), { cause: error }), { timelineQualification: diagnostic })
  }
}

function assertSingleCappedBranchRequest(tracker, before, workspaceId, parentId, label) {
  const requests = navigationPageReceipts(tracker, before.index)
  if (requests.length !== 1) throw new Error(`${label} expected exactly one bounded navigation-branch request, observed ${requests.length}`)
  const url = new URL(requests[0].url)
  const expectedPath = `/api/v1/workspaces/${workspaceId}/navigation`
  if (url.pathname !== expectedPath
    || url.searchParams.get('parent_kind') !== 'project'
    || url.searchParams.get('parent_id') !== parentId
    || url.searchParams.get('project_offset') !== '0'
    || url.searchParams.get('task_offset') !== '0'
    || url.searchParams.get('project_limit') !== '50'
    || url.searchParams.get('task_limit') !== '50') {
    throw new Error(`${label} used an unexpected navigation-branch request: ${requests[0].url}`)
  }
  return requests[0]
}

async function measureLargeBranchExpandCollapse(page, tracker, fixture, anchorId) {
  const expectedBranch = navigationBranchResponse(fixture, {
    parentKind: 'project', parentId: anchorId, projectLimit: 50, taskLimit: 50
  })
  const descendants = [...expectedBranch.projects.items, ...expectedBranch.tasks.items]
  if (descendants.length === 0 || descendants.length > 100
    || expectedBranch.projects.items.length > 50 || expectedBranch.tasks.items.length > 50) {
    throw new Error('large expand/collapse fixture requires one non-empty, 50-item-capped project branch')
  }
  const descendantId = descendants[0].id
  const toggleSelector = `#entity-${anchorId} .tree-toggle`
  const toggle = page.locator(toggleSelector)
  if (await toggle.count() !== 1) throw new Error(`large expand/collapse expected one root toggle, observed ${await toggle.count()}`)

  // Bootstrap renders roots open and drains their expanded descendants.
  // Close once to create the user-visible lazy-load state; this preparatory
  // transition must remain local and cannot recursively fetch a branch.
  const prepareBefore = requestSnapshot(tracker)
  await toggle.click()
  await page.waitForFunction(selector => document.querySelector(selector)?.textContent.trim() === '\u25b6', toggleSelector)
  await waitForTransportQuiescence(page, tracker, 'large branch collapse preparation')
  if (requestDelta(tracker, prepareBefore).count !== 0) throw new Error('collapsing an unloaded large branch issued a request')

  const collapsedDom = await domMetrics(page)
  const expandBefore = requestSnapshot(tracker)
  const expandMs = await requiredDoubleFrame(page, toggleSelector)
  await page.waitForFunction(id => Boolean(document.querySelector(`#entity-${id}`)), descendantId, { timeout: 30000 })
  await waitForTransportQuiescence(page, tracker, 'large branch expansion')
  for (const request of navigationPageReceipts(tracker, expandBefore.index)) { const url = new URL(request.url); if (url.searchParams.get('project_limit') !== '50' || url.searchParams.get('task_limit') !== '50') throw new Error('Expansion abandoned capped transport') }
  const expandedDom = await domMetrics(page)
  if (!await page.locator('#entity-' + descendantId).count()) throw new Error('Expansion did not mount the authoritative first child')
  const expand = {
    ms: expandMs,
    ...requestDelta(tracker, expandBefore),
    branch: { parentId: anchorId, descendantId, projectItems: expectedBranch.projects.items.length, taskItems: expectedBranch.tasks.items.length },
    dom: { before: collapsedDom, after: expandedDom }
  }

  const collapseBefore = requestSnapshot(tracker)
  const collapseMs = await requiredDoubleFrame(page, toggleSelector)
  await page.waitForFunction(id => !document.querySelector(`#entity-${id}`), descendantId, { timeout: 30000 })
  await waitForTransportQuiescence(page, tracker, 'large branch collapse')
  const collapseDelta = requestDelta(tracker, collapseBefore)
  if (collapseDelta.count !== 0) throw new Error(`collapsing a loaded large branch issued ${collapseDelta.count} request(s)`)
  const recollapsedDom = await domMetrics(page)
  if (await page.locator('#entity-' + descendantId).count()) throw new Error('Collapse retained the authoritative child')
  const collapse = {
    ms: collapseMs,
    ...collapseDelta,
    branch: { parentId: anchorId, descendantId, descendantUnmounted: true },
    dom: { before: expandedDom, after: recollapsedDom }
  }
  return { expand, collapse }
}

function validateRequestDelta(delta, label) {
  if (delta == null || typeof delta !== 'object') throw new Error(`${label} request delta must be an object`)
  const fields = Object.keys(delta).sort()
  if (JSON.stringify(fields) !== JSON.stringify(['bytes', 'count', 'routeBytes', 'routes'])) {
    throw new Error(`${label} request delta must contain count, bytes, routes, and routeBytes`)
  }
  if (!Number.isInteger(delta.count) || delta.count < 0 || !Number.isInteger(delta.bytes) || delta.bytes < 0) {
    throw new Error(`${label} request delta count and bytes must be non-negative integers`)
  }
  for (const [route, count] of Object.entries(delta.routes)) {
    if (!route || !Number.isInteger(count) || count <= 0) throw new Error(`${label} route count is invalid for ${route}`)
  }
  for (const [route, bytes] of Object.entries(delta.routeBytes)) {
    if (!route || !Number.isInteger(bytes) || bytes < 0) throw new Error(`${label} route bytes are invalid for ${route}`)
  }
  if (Object.keys(delta.routes).length !== Object.keys(delta.routeBytes).length || Object.keys(delta.routes).some(route => !Object.hasOwn(delta.routeBytes, route))) {
    throw new Error(`${label} request delta route and byte keys differ`)
  }
  if (Object.values(delta.routes).reduce((sum, count) => sum + count, 0) !== delta.count) throw new Error(`${label} request delta count does not equal route counts`)
  if (Object.values(delta.routeBytes).reduce((sum, bytes) => sum + bytes, 0) !== delta.bytes) throw new Error(`${label} request delta bytes do not equal route bytes`)
  return delta
}

// Each index keeps the existing authority/lifetime state machine. Actual events
// are mirrored so a filtered pass cannot borrow a neutral pass's coverage.
function contextualNavigationProof(fixture) {
  const proofs = new Map(), lifecycle = [], admissions = new WeakMap()
  let active
  const proofFor = context => {
    if (!proofs.has(context)) {
      const entries = JSON.parse(context)
      const url = new URL('http://fixture')
      if (!Array.isArray(entries) || entries.length !== 6) throw new Error('Invalid navigation filter context')
      for (const [key, values] of entries) {
        if (!['show_only', 'priority_mode', 'priority_value', 'query', 'project_status', 'task_status'].includes(key) || !Array.isArray(values)) throw new Error('Invalid navigation filter context')
        for (const value of values) url.searchParams.append(key, value)
      }
      if (navigationFilterContext(url) !== context) throw new Error('Invalid navigation filter context')
      const members = filteredNavigationMembers(fixture, url), children = expectedNavigationChildren({ ...fixture, ...members })
      const proof = createNavigationCompletionIndex({ children, rootProjects: children.workspace_root.projects, rootTasks: children.workspace_root.tasks, context, advertisedOwners: advertisedNavigationOwners(fixture) })
      for (const [method, args] of lifecycle) proof[method](...args)
      proofs.set(context, proof)
    }
    return proofs.get(context)
  }
  active = proofFor(navigationFilterContext(new URL('http://fixture?priority_mode=any')))
  const broadcast = (method, args) => {
    const results = [...proofs.values()].map(proof => proof[method](...args))
    lifecycle.push([method, args])
    return results[0]
  }
  return {
    ...Object.fromEntries(['beginSession', 'completeSession', 'admitSnapshot', 'completeSnapshot'].map(method => [method, (...args) => broadcast(method, args)])),
    admit(request) {
      const selected = proofFor(request.context)
      if (request.owner === 'workspace_root') active = selected
      const stamps = [...proofs.values()].map(proof => [proof, proof.admit(request)])
      const stamp = stamps.find(([proof]) => proof === active)[1]
      admissions.set(stamp, stamps)
      return stamp
    },
    complete(stamp, ...args) { for (const [proof, counterpart] of admissions.get(stamp) || []) proof.complete(counterpart, ...args) },
    ...Object.fromEntries(['retrySelection', 'retryOwnerQuiet', 'armRetry', 'cancelRetry', 'ready'].map(method => [method, (...args) => active[method](...args)])),
    setCollapsed(keys) { for (const proof of proofs.values()) proof.setCollapsed(keys) },
    replaceChildren(...args) { for (const proof of proofs.values()) proof.replaceChildren(...args) },
    invalidateOwners(keys) { for (const proof of proofs.values()) proof.invalidateOwners(keys) }
  }
}

export function createTracker(fixture = null) {
  let paintDecision = null
  const coldDiagnostics = createColdDiagnostics()
  const navigationProof = fixture && contextualNavigationProof(fixture)
  return { physicalAccounting:createPhysicalRequestAccounting(), navigationProof, navigationMetadata: fixture && new Map(), metadataEpoch: 0, liveNavigationPrepared: false, coldDiagnostics, paintDecision: () => paintDecision, paintRunId: randomUUID(), paintReadiness: fixture && createUsablePaintReadiness(fixture.workspace.id, workspaceShellSnapshotItems(fixture).length, decision => { paintDecision = decision }), workspaceId: fixture?.workspace.id, activeBranchOwners: new Map(), activeSnapshots: 0,
    requests: [], arrivals: new WeakMap(), counts: {}, bytes: {}, active: 0, activeBranches: 0, activeExpandedBranches: 0, activeRootBranches: 0, activeDetails: 0, maxBranches: 0, maxExpandedBranches: 0, maxRootBranches: 0, maxDetails: 0, completed: 0, lastActivityAt: performance.now(), unhandledApiRoutes: [],
    model: { snapshotGeneration: 0, snapshotItems: 0, snapshotPages: 0, snapshotComplete: false, snapshotProfile: null, timelineEvents: 0, timelineBuckets: 0, timelineBucketRequest: null }
  }
}

function createResyncGate(pauseOffset) {
  let pause
  let release
  const waitForPause = new Promise(resolve => { pause = resolve })
  const waitForRelease = new Promise(resolve => { release = resolve })
  return { pauseOffset, used: false, waitForPause, waitForRelease, signalPaused: pause, release }
}

async function requiredPromiseWithin(promise, timeoutMs, label) {
  let timeout
  try {
    return await Promise.race([
      promise,
      new Promise((_, reject) => { timeout = setTimeout(() => reject(new Error(`${label} did not occur within ${timeoutMs}ms`)), timeoutMs) })
    ])
  } finally {
    clearTimeout(timeout)
  }
}

async function waitForCurrentSnapshotTransport(page, tracker, fixture, label, timeoutMs = 60000) {
  const expectedItems = workspaceShellSnapshotItems(fixture).length
  const expectedPages = Math.ceil(expectedItems / DIRECT_FOCUS_CONTRACT.pageSize)
  const ready = () => transportContractReady({
    protocol: 'bounded', transportedItems: tracker.model.snapshotItems, expectedItems, fullBackingItems: snapshotItems(fixture).length,
    transportedPages: tracker.model.snapshotPages, expectedPages, complete: tracker.model.snapshotComplete
  })
  const deadline = performance.now() + timeoutMs
  while (performance.now() < deadline) {
    assertNoUnhandledApiRoutes(tracker)
    if (ready() && tracker.active === 0) {
      await waitForTransportQuiescence(page, tracker, label)
      if (ready() && tracker.active === 0) return { items: expectedItems, pages: expectedPages }
    }
    await page.waitForTimeout(25)
  }
  throw new Error(`${label} failed: ${tracker.model.snapshotProfile} snapshot ${tracker.model.snapshotItems}/${expectedItems} shell items, ${tracker.model.snapshotPages}/${expectedPages} pages, complete=${tracker.model.snapshotComplete}, active=${tracker.active}`)
}

async function measureDirectFocus(browser, origin, fixture) {
  const direct = directFocusFixture(fixture)
  const canonicalSnapshots = workspaceShellSnapshotItems(fixture)
  if (direct.targetProject.parent_id != null) throw new Error(`direct-focus target ${direct.targetProject.id} is not a root project`)
  const tracker = createTracker()
  const context = await browser.newContext({ viewport: HARNESS_CONFIGURATION.viewport })
  await context.addInitScript(fakeWebSocketScript)
  const page = await context.newPage()
  const consoleErrors = []
  page.on('console', message => { if (message.type() === 'error') consoleErrors.push(message.text()) })
  page.on('pageerror', error => consoleErrors.push(error.message))
  await page.route('**/api/v1/**', fixtureResponder(fixture, tracker, { snapshots: canonicalSnapshots }))
  const targetId = direct.targetProject.id
  const directEntityKey = 'navigation:focus'
  const attemptBefore = requestSnapshot(tracker)
  const focusStart = performance.now()
  let requestedBeforeFullResync = false
  let renderedBeforeFullResync = false
  let focusObservedAt = focusStart
  let observationElapsedMs = 0
  try {
    await page.goto(`${origin}/workspace/${fixture.workspace.id}#tab=projects&focus=project:${targetId}`, { waitUntil: 'domcontentloaded' })
    const observationStartedAt = performance.now()
    const observationDeadline = observationStartedAt + DIRECT_FOCUS_CONTRACT.observationDeadlineMs
    while (performance.now() < observationDeadline) {
      assertNoUnhandledApiRoutes(tracker)
    requestedBeforeFullResync = (tracker.counts[directEntityKey] || 0) > 0
      renderedBeforeFullResync = await page.locator(`#entity-${targetId}`).count() > 0 && await page.locator('.focus-breadcrumb-bar').count() > 0
      if (requestedBeforeFullResync && renderedBeforeFullResync) break
      await page.waitForTimeout(10)
    }
    focusObservedAt = performance.now()
    observationElapsedMs = focusObservedAt - observationStartedAt
  } finally {}
  const attemptDelta = requestDelta(tracker, attemptBefore)
  const focusRequests = tracker.requests.slice(attemptBefore.index).filter(request =>
    request.key === directEntityKey
    || request.url.includes(`/api/v1/workspaces/${fixture.workspace.id}/navigation/focus/project/${targetId}`)
  )
  const productFailureReason = requestedBeforeFullResync && renderedBeforeFullResync
    ? null
    : requestedBeforeFullResync
      ? 'focus path requested the target but did not render its breadcrumb/card during focus-first shell bootstrap'
      : renderedBeforeFullResync
        ? 'focus path rendered the target without issuing its direct entity request during focus-first shell bootstrap'
        : 'focus path issued no direct entity request and rendered no target during focus-first shell bootstrap'
  const full = await waitForCurrentSnapshotTransport(page, tracker, fixture, 'direct-focus released shell resync')
  await page.waitForSelector('.focus-breadcrumb-bar', { timeout: 30000 })
  await page.waitForSelector(`#entity-${targetId}`, { timeout: 30000 })
  const renderedAfterShellResync = true
  assertNoUnhandledApiRoutes(tracker)
  if (consoleErrors.length > 0) throw new Error(`direct-focus browser console error(s): ${consoleErrors.join(' | ')}`)
  await context.close()
  return {
    ms: focusObservedAt - focusStart,
    observationDeadlineMs: DIRECT_FOCUS_CONTRACT.observationDeadlineMs,
    observationElapsedMs,
    targetAbsentBeforeCanonicalResync: true,
    targetId,
    targetParentId: direct.targetProject.parent_id,
    initialSnapshotItems: 0,
    initialSnapshotPages: 0,
    snapshotPauseUsed: false,
    directFocusRequested: requestedBeforeFullResync,
    directFocusRendered: renderedBeforeFullResync,
    productFailureReason,
    entityLookupRequests: focusRequests.filter(request => request.key === directEntityKey).length,
    count: focusRequests.length,
    focusRequestRoutes: focusRequests.reduce((values, request) => ({ ...values, [request.key]: (values[request.key] || 0) + 1 }), {}),
    attemptRouteDelta: attemptDelta,
    canonicalSnapshotItems: full.items,
    canonicalSnapshotPages: full.pages,
    canonicalSnapshotProfile: PRODUCTION_SNAPSHOT_PROFILE,
    canonicalSnapshotHash: hashJson(workspaceShellSnapshotItems(fixture)),
    fullBackingCanonicalSnapshotHash: snapshotHash(fixture),
    renderedAfterShellResync
  }
}

// This is deliberately outside the recorded small/large measurements: it
// exercises continuation against a derived test fixture without changing the
// immutable baseline fixture hashes or the production direct-focus budget.
async function verifyDeepFocusContinuation(browser, origin) {
  const deep = deepFocusFixture()
  const fixture = deep.fixture
  const tracker = createTracker()
  const context = await browser.newContext({ viewport: HARNESS_CONFIGURATION.viewport })
  await context.addInitScript(fakeWebSocketScript)
  const page = await context.newPage()
  const consoleErrors = []
  page.on('console', message => { if (message.type() === 'error') consoleErrors.push(message.text()) })
  page.on('pageerror', error => consoleErrors.push(error.message))
  await page.route('**/api/v1/**', fixtureResponder(fixture, tracker, { snapshots: workspaceShellSnapshotItems(fixture) }))
  try {
    await page.goto(`${origin}/workspace/${fixture.workspace.id}#tab=projects&focus=project:${deep.targetProject.id}`, { waitUntil: 'domcontentloaded' })
    await page.waitForSelector(`#entity-${deep.targetProject.id}`, { timeout: 30000 })
    await page.waitForSelector('.focus-breadcrumb-bar', { timeout: 30000 })
    const deadline = performance.now() + 30000
    const offsets = () => tracker.requests
      .filter(request => request.key === 'navigation:focus')
      .map(request => Number(new URL(request.url).searchParams.get('ancestor_offset')))
    while (performance.now() < deadline) {
      assertNoUnhandledApiRoutes(tracker)
      if (tracker.active === 0 && JSON.stringify(offsets()) === JSON.stringify([0, 64, 128])) break
      await page.waitForTimeout(25)
    }
    const observedOffsets = offsets()
    if (JSON.stringify(observedOffsets) !== JSON.stringify([0, 64, 128])) throw new Error(`deep focus expected bounded ancestor offsets 0,64,128; received ${observedOffsets.join(',')}`)
    if (await page.locator(`#entity-${deep.targetProject.id}`).count() !== 1) throw new Error('deep focus continuation lost the target card')
    await page.waitForFunction(
      expectedCount => document.querySelectorAll('.focus-breadcrumb-bar .focus-crumb').length === expectedCount,
      131,
      { timeout: 30000 }
    )
    const crumbNames = (await page.locator('.focus-breadcrumb-bar .focus-crumb').allTextContents()).slice(1)
    const expectedCrumbNames = Array.from({ length: 130 }, (_, index) => `Deep focus project ${String(index + 1).padStart(3, '0')}`)
    if (JSON.stringify(crumbNames) !== JSON.stringify(expectedCrumbNames) || new Set(crumbNames).size !== crumbNames.length) throw new Error(`deep focus breadcrumb was incomplete, reordered, or duplicated: expected ${expectedCrumbNames.length} ordered crumbs, received ${crumbNames.length}`)
    if (consoleErrors.length > 0) throw new Error(`deep-focus continuation browser console error(s): ${consoleErrors.join(' | ')}`)
    return { ancestorCount: deep.ancestorCount, offsets: observedOffsets, requests: observedOffsets.length, targetRendered: true, breadcrumbCount: crumbNames.length, breadcrumbOrdered: true, terminal: true }
  } finally {
    await context.close()
  }
}

async function verifyStaleDeepFocusContinuation(browser, origin) {
  const deep = deepFocusFixture()
  const fixture = deep.fixture
  const replacement = fixture.projects.find(project => project.name === 'Deep focus project 100')
  if (!replacement) throw new Error('deep focus fixture did not contain the stale-response replacement target')
  const tracker = createTracker()
  let releasePause
  let signalPaused
  const pause = {
    entityType: 'project',
    entityId: deep.targetProject.id,
    ancestorOffset: 64,
    used: false,
    waitForRelease: new Promise(resolve => { releasePause = resolve }),
    signalPaused: () => signalPaused()
  }
  const paused = new Promise(resolve => { signalPaused = resolve })
  const context = await browser.newContext({ viewport: HARNESS_CONFIGURATION.viewport })
  await context.addInitScript(fakeWebSocketScript)
  const page = await context.newPage()
  try {
    await page.route('**/api/v1/**', fixtureResponder(fixture, tracker, { snapshots: workspaceShellSnapshotItems(fixture), focusContinuationPause: pause }))
    await page.goto(`${origin}/workspace/${fixture.workspace.id}#tab=projects&focus=project:${deep.targetProject.id}`, { waitUntil: 'domcontentloaded' })
    await requiredPromiseWithin(paused, 30000, 'stale focus continuation gate')
    await page.evaluate(projectId => { window.location.hash = `tab=projects&focus=project:${projectId}` }, replacement.id)
    await page.waitForSelector(`#entity-${replacement.id}`, { timeout: 30000 })
    releasePause()
    await page.waitForTimeout(250)
    const oldTerminalPageRequested = tracker.requests.some(request => {
      const url = new URL(request.url)
      return request.key === 'navigation:focus'
        && url.pathname.endsWith(`/project/${deep.targetProject.id}`)
        && Number(url.searchParams.get('ancestor_offset')) === 128
    })
    if (oldTerminalPageRequested) throw new Error('stale deep-focus continuation scheduled its old terminal page after the focus changed')
    return { staleContinuationRejected: true }
  } finally {
    releasePause()
    await context.close()
  }
}

async function measureObservationScaling(page, tracker, fixture, cdp, blankHeap, dom, interactions) {
  const contract = fixture.observationScaling
  await requiredDoubleFrameByText(page, '#observation-panel button', 'Files')
  await page.locator('#observation-match-paths').fill(contract.paths.join('\n'))
  await page.locator('.observation-match-apply').click()
  const waitLoaded = async count => {
    await page.waitForFunction(expected => document.querySelector('.observation-mode-announcement')?.textContent.includes(expected + ' matching observations loaded'), count)
    await waitForTransportQuiescence(page, tracker, 'ordered Observation match page')
  }
  await waitLoaded(50)
  const pages = []
  for (let count = 50; count < Math.min(contract.largeLoaded, fixture.observations.length);) {
    const before = requestSnapshot(tracker), started = performance.now()
    await page.locator('.observation-load-more').click()
    count = Math.min(count + 50, fixture.observations.length)
    await waitLoaded(count)
    pages.push({ loaded: count, ms: performance.now() - started, ...requestDelta(tracker, before) })
    dom.push({ tab: 'match-loaded-' + count, ...(await domMetrics(page)) })
  }
  const controls = evidencePlan.rendering
    ? [...(await scanObservationRows(page)).groups.keys()]
    : await page.locator('.observation-subject-group-toggle').evaluateAll(elements => elements.map(element => element.getAttribute('aria-controls')))
  if (controls.length < 4 || new Set(controls).size !== controls.length) throw new Error('Expected distinct overlapping subject groups across both ordered paths')
  for (const [index, id] of controls.entries()) {
    const selector = evidencePlan.rendering
      ? '[data-observation-group=' + JSON.stringify(id) + '] .observation-subject-group-toggle'
      : '.observation-subject-group-toggle[aria-controls="' + id + '"]'
    if (evidencePlan.rendering) await revealObservationRow(page, page.locator(selector))
    const before = requestSnapshot(tracker)
    const ms = await requiredDoubleFrame(page, selector)
    const delta = requestDelta(tracker, before)
    if (delta.count !== 0) throw new Error('Subject disclosure unexpectedly requested HTTP')
    interactions['observationGroup' + index + 'Ms'] = { ms, ...delta }
    dom.push({ tab: 'match-expanded-' + index, ...(await domMetrics(page)) })
  }
  const boundary = fixture.observations.find(value => value.id === contract.boundaryId)
  const card = page.locator('.observation-card').filter({ hasText: boundary.content.slice(0, 17) }).first()
  let logicalReachability = null
  if (evidencePlan.rendering) {
    const scanned = await scanObservationRows(page)
    const expected = queryObservationScalingMatches(fixture, { paths: contract.paths, offset: 0, limit: contract.largeLoaded }).items
    const contextual = expected.reduce((count, item) => count + item.path_matches.reduce((total, match) => total + match.matched_subjects.length, 0), 0)
    if (scanned.cards.size !== contextual || new Set([...scanned.cards.values()].map(row => row.id)).size !== expected.length) throw new Error('Expanded Observation logical membership was not completely scroll-reachable')
    logicalReachability = { canonicalMembers: expected.length, contextualCards: scanned.cards.size, groups: scanned.groups.size, maximumMountedRows: scanned.maxMounted }
    await revealObservationRow(page, card)
  }
  await card.click()
  await page.locator('#observation-content-reader').waitFor()
  await waitForTransportQuiescence(page, tracker, '512 KiB Observation detail')
  if (await page.locator('#observation-content-reader').inputValue() !== boundary.content) throw new Error('Large native reader did not expose the exact fixture content')
  dom.push({ tab: 'boundary-detail', ...(await domMetrics(page)) })
  const editBefore = requestSnapshot(tracker)
  const editMs = await requiredDoubleFrame(page, '#observation-edit')
  interactions.observationEditorOpenMs = { ms: editMs, ...requestDelta(tracker, editBefore) }
  const draftBefore = requestSnapshot(tracker)
  const draft = boundary.content.slice(0, -1) + 'y'
  const draftMs = await page.evaluate(async content => {
    const input = document.querySelector('#observation-edit-content')
    if (!input) throw new Error('Boundary editor missing')
    const start = performance.now()
    input.value = content; input.dispatchEvent(new Event('input', { bubbles: true }))
    await new Promise(resolve => requestAnimationFrame(() => requestAnimationFrame(resolve)))
    return performance.now() - start
  }, draft)
  const draftDelta = requestDelta(tracker, draftBefore)
  if (draftDelta.count !== 0) throw new Error('Draft input unexpectedly requested HTTP')
  interactions.observationBoundaryDraftMs = { ms: draftMs, ...draftDelta }
  dom.push({ tab: 'boundary-editor', ...(await domMetrics(page)) })
  await cdp.send('HeapProfiler.collectGarbage')
  const activeEditorHeapBytes = Math.max(0, (await cdp.send('Runtime.getHeapUsage')).usedSize - blankHeap)
  const liveBefore = requestSnapshot(tracker), liveStart = performance.now()
  // A real-shaped canonical invalidation batch, backed by updated fixture DTOs.
  boundary.content = boundary.content.slice(0, -1) + 'z'
  boundary.content_version = '20000000-0000-4000-8000-000000000001'
  boundary.updated_at = '2027-01-01T00:00:00Z'
  const frames = observationScalingFrames(fixture)
  if (await page.evaluate(values => window.__perfPushFrames(values), frames) !== 1) throw new Error('Observation batch requires one socket')
  await page.waitForFunction(() => window.__perfLastPush?.dispatchTurnComplete === true && document.querySelector('.observation-edit-conflict'))
  await waitForTransportQuiescence(page, tracker, 'Observation live batch with protected editor')
  const live = { frames: frames.length, ms: performance.now() - liveStart, ...requestDelta(tracker, liveBefore) }
  if (await page.locator('#observation-edit-content').inputValue() !== draft) throw new Error('Observation live batch replaced the protected draft')
  dom.push({ tab: 'boundary-editor-post-live', ...(await domMetrics(page)) })
  await requiredDoubleFrameByText(page, '.observation-edit-actions button', 'Cancel')
  await requiredDoubleFrameByText(page, '#observation-panel button', 'All')
  await waitObservationMembers(page, 50)
  await waitForTransportQuiescence(page, tracker, 'flat Observation restore after scaling research')
  return { contract: OBSERVATION_SCALING_CONTRACT, loaded: Math.min(contract.largeLoaded, fixture.observations.length), expandedGroups: controls.length,
    pages, activeEditorHeapBytes, live, ...(logicalReachability ? { logicalReachability } : {}),
    researchTriggers: { activeEditorHeap: activeEditorHeapBytes > contract.researchTriggers.activeEditorHeapBytes, observationLiveFollowUps: live.count > contract.researchTriggers.observationLiveFollowUps } }
}

export function observationMembershipReady({ expected, requireIdle = false }) {
  const viewport = document.getElementById('observation-viewport')
  const raw = viewport?.dataset.observationLoadedCount
  const valid = typeof raw === 'string' && /^(0|[1-9][0-9]*)$/.test(raw) && Number.isSafeInteger(Number(raw))
  const count = viewport ? (valid ? Number(raw) : null) : document.querySelectorAll('.observation-card').length
  return count === expected && (!requireIdle || !document.querySelector('.observation-state-loading'))
}

export function observationMembershipSnapshot({ expected, label, phase, requireIdle }) {
  const viewport = document.getElementById('observation-viewport')
  const raw = viewport?.dataset.observationLoadedCount
  const valid = typeof raw === 'string' && /^(0|[1-9][0-9]*)$/.test(raw) && Number.isSafeInteger(Number(raw))
  const error = document.querySelector('.observation-state-error')
  return { expected, label, phase, requireIdle, viewportPresent: !!viewport,
    rawLogicalCount: raw == null ? null : String(raw).slice(0, 64), logicalCount: valid ? Number(raw) : null,
    mountedCards: document.querySelectorAll('.observation-card').length,
    loading: !!document.querySelector('.observation-state-loading'), error: !!error,
    errorText: error?.textContent?.slice(0, 256) || null,
    layoutReady: viewport?.dataset.observationLayoutReady?.slice(0, 32) || null,
    context: viewport?.dataset.observationViewportContext?.slice(0, 512) || null }
}

export async function waitObservationMembers(page, count, label = 'Observation membership', requireIdle = false) {
  const previousPhase = attemptState?.phase
  const phase = previousPhase ? previousPhase + ': ' + label : label
  if (attemptState) attemptState.phase = phase
  try {
    await page.waitForFunction(observationMembershipReady, { expected: count, requireIdle }, { timeout: 30000 })
  } catch (error) {
    if (error.name !== 'TimeoutError') throw error
    let diagnostic
    let diagnosticTimer
    try {
      diagnostic = await Promise.race([
        page.evaluate(observationMembershipSnapshot, { expected: count, label, phase, requireIdle }),
        new Promise((_, reject) => { diagnosticTimer = setTimeout(() => reject(new Error('Observation membership diagnostic capture exceeded 1000ms')), 1000) })
      ])
    }
    catch (captureError) { diagnostic = { expected: count, label, phase, captureError: captureError.message.slice(0, 256) } }
    finally { clearTimeout(diagnosticTimer) }
    throw Object.assign(new Error(error.message + '; Observation membership ' + JSON.stringify(diagnostic), { cause: error }), { observationMembership: diagnostic })
  }
  if (attemptState) attemptState.phase = previousPhase
}

export async function measureRun(browser, origin, fixture, measured, trace, diagnosticRuns, liveDiagnostics = null) {
  const runPhase = attemptState?.phase || fixture.size + ' warmup'
  const stage = label => { if (attemptState) attemptState.phase = runPhase + ': ' + label }
  if (fixture.observationScaling) fixture = { ...fixture, observations: fixture.observations.map(value => ({ ...value })) }
  const anchors = representativeProjectAnchors(fixture)
  const tracker = createTracker(fixture)
  const observationStartupBefore = requestSnapshot(tracker)
  diagnosticRuns.push({ fixture: fixture.size, measured, trace, diagnostics: tracker.coldDiagnostics })
  const context = await browser.newContext({ viewport: HARNESS_CONFIGURATION.viewport })
  await context.addInitScript(fakeWebSocketScript)
  if (trace) await context.tracing.start(traceCaptureOptions)
  const page = await context.newPage()
  const consoleErrors = []
  page.on('console', message => { if (message.type() === 'error') consoleErrors.push(message.text()) })
  page.on('pageerror', error => consoleErrors.push(error.message))
  await page.route('**/api/v1/**', fixtureResponder(fixture, tracker, { contentOnly: evidencePlan.subjectTreeLiveFilters }))
  const cdp = await context.newCDPSession(page)
  const blankHeap = (await cdp.send('Runtime.getHeapUsage')).usedSize
  const coldStart = performance.now()
  await navigateToFirstUsefulViewport(page, origin, tracker, fixture, anchors.first)
  const coldMs = performance.now() - coldStart
  const coldEnd = tracker.requests.length
  tracker.coldDiagnostics.cut(performance.now(), coldEnd, tracker.completed, tracker.active)
  const coldRequests = tracker.requests.slice(0, coldEnd)
  const cold = {
    diagnostics: tracker.coldDiagnostics.snapshot(),
    phase: 'authorized-painted-root-viewport.v1', activeRequestsAtCut: tracker.active, completedRequestsAtCut: tracker.completed,
    ms: coldMs, requests: coldRequests.length, bytes: coldRequests.reduce((sum, request) => sum + request.bytes, 0),
    routeCounts: { ...tracker.counts }, routeBytes: { ...tracker.bytes },
    entityOverviewRequests: (tracker.counts['projects:overview'] || 0) + (tracker.counts['tasks:overview'] || 0),
    canonicalSnapshotItems: tracker.model.snapshotItems,
    canonicalSnapshotPages: tracker.counts['change-stream:resync'] || 0
  }
  const backgroundStarted = performance.now()
  await waitForWorkspaceReady(page, tracker, fixture, anchors.first, 'complete expanded hierarchy')
  const completion = completedExpandedMembership(tracker, fixture)
  stage('expanded navigation scroll')
  const scrollCompletion = await scrollExpandedHierarchy(page, completion.entityKeys, tracker, fixture)
  stage('expanded navigation detail settle')
  await waitForTransportQuiescence(page, tracker, 'expanded hierarchy scroll detail demand')
  delete completion.entityKeys
  const background = { ms: performance.now() - backgroundStarted, completeRequests: tracker.requests.length, completeBytes: tracker.requests.reduce((sum, request) => sum + request.bytes, 0), afterViewportRequests: tracker.requests.length - coldEnd, maxPhysicalBranches: tracker.maxBranches, maxPhysicalExpandedBranches: tracker.maxExpandedBranches, maxPhysicalRootBranches: tracker.maxRootBranches, maxPhysicalDetails: tracker.maxDetails, ...completion, ...scrollCompletion }
  assertNavigationCapacity({ aggregate: background.maxPhysicalBranches, expanded: background.maxPhysicalExpandedBranches, details: background.maxPhysicalDetails })
  const dom = []
  dom.push({ tab: 'projects', ...(await domMetrics(page)) })
  const interactions = {}
  const tabSwitches = {}
  const filters = {}
  stage('Projects interactions')
  if (fixture.size === 'large') {
    const branchInteractions = await measureLargeBranchExpandCollapse(page, tracker, fixture, anchors.expanded)
    interactions.branchExpandMs = branchInteractions.expand
    interactions.branchCollapseMs = branchInteractions.collapse
    dom.push({ tab: 'large-branch-expanded', ...branchInteractions.expand.dom.after })
    dom.push({ tab: 'large-branch-collapsed', ...branchInteractions.collapse.dom.after })
  }
  const taskFilterBefore = requestSnapshot(tracker)
  const taskFilterMs = await requiredDoubleFrameByText(page, '.filter-bar button', 'Tasks')
  await page.waitForFunction(() => document.querySelectorAll('.card-project').length === 0 && document.querySelectorAll('.card-task,.card-subtask').length > 0)
  await waitForTransportQuiescence(page, tracker, 'Tasks filter')
  interactions.taskFilterMs = { ms: taskFilterMs, ...requestDelta(tracker, taskFilterBefore) }
  const taskFilterResetBefore = requestSnapshot(tracker)
  const taskFilterResetMs = await requiredDoubleFrameByText(page, '.filter-bar button', 'All')
  await waitForWorkspaceReady(page, tracker, fixture, anchors.first, 'All filter representative restore')
  interactions.taskFilterResetMs = { ms: taskFilterResetMs, ...requestDelta(tracker, taskFilterResetBefore) }

  const observationsTabBefore = requestSnapshot(tracker)
  const obsTabMs = await requiredDoubleFrame(page, '.tabs button:nth-child(2)')
  await page.waitForSelector('#observation-results')
  const observationPageSize = OBSERVATION_PAGINATION_CONTRACT.pageSize
  await waitObservationMembers(page, Math.min(observationPageSize, fixture.observations.length), 'Observations initial page')
  await waitForTransportQuiescence(page, tracker, 'Observations initial page')
  const initialObservationPage = assertObservationPagination(tracker, observationStartupBefore, fixture, 0, null, 'Observations initial page', evidencePlan.subjectTreeLiveFilters ? 'content' : null)
  interactions.observationsTabMs = { ms: obsTabMs, ...requestDelta(tracker, observationsTabBefore) }
  tabSwitches.observations = interactions.observationsTabMs
  dom.push({ tab: 'observations', ...(await domMetrics(page)) })
  if (evidencePlan.rendering && (await scanObservationRows(page)).cards.size !== Math.min(observationPageSize, fixture.observations.length)) throw new Error('Initial flat Observation membership was not scroll-reachable')
  const loadButton = page.locator('.observation-load-more')
  let observationLoadMore
  if (!initialObservationPage.response.hasMore) {
    if (fixture.size !== 'small' || fixture.observations.length !== 60 || await loadButton.count() !== 0) throw new Error('Small Observation first page must be complete and have no load-more action')
    observationLoadMore = { applicable: false, reason: 'terminal-first-page', loaded: 60, hasMore: false, buttonAbsent: true, receipt: initialObservationPage }
  } else {
    const loadBefore = requestSnapshot(tracker)
    const loadStart = performance.now()
    if (fixture.size !== 'large' || await loadButton.count() !== 1 || !await loadButton.isEnabled()) throw new Error('Large Observation load-more must be an enabled actual action')
    await loadButton.click()
    await waitObservationMembers(page, 2 * observationPageSize, 'Observation load more')
    await waitForTransportQuiescence(page, tracker, 'Observation load more')
    const ms = performance.now() - loadStart
    const receipt = assertObservationPagination(tracker, loadBefore, fixture, observationPageSize, null, 'Observation load more', evidencePlan.subjectTreeLiveFilters ? 'content' : null)
    observationLoadMore = { applicable: true, exercised: true, loaded: 2 * observationPageSize, ms, ...requestDelta(tracker, loadBefore), receipt }
    if (evidencePlan.rendering && (await scanObservationRows(page)).cards.size !== 2 * observationPageSize) throw new Error('Paged flat Observation membership was not scroll-reachable')
  }
  const filterInput = page.locator('#observation-query')
  if (await filterInput.count() !== 1) throw new Error(`required Observation query target count was ${await filterInput.count()}`)
  const filterBefore = requestSnapshot(tracker)
  const observationFilterStart = performance.now()
  let observationFilterMs
  if (evidencePlan.subjectTreeLiveFilters) observationFilterMs = await requiredLiveInputDoubleFrame(page, '#observation-query', OBSERVATION_MEASURED_QUERY)
  else {
    await filterInput.fill(OBSERVATION_MEASURED_QUERY)
    observationFilterMs = await requiredDoubleFrame(page, '.observation-filter-apply')
  }
  await waitObservationMembers(page, 1, 'Observation filter', true)
  await waitForTransportQuiescence(page, tracker, 'Observation filter')
  const filteredObservationPage = assertObservationPagination(tracker, filterBefore, fixture, 0, OBSERVATION_MEASURED_QUERY, 'Observation filter', evidencePlan.subjectTreeLiveFilters ? 'content' : null)
  const observationFilterSettledMs = performance.now() - observationFilterStart
  interactions.observationFilterMs = { ms: observationFilterMs, ...requestDelta(tracker, filterBefore) }
  filters.observations = interactions.observationFilterMs
  const observationFilterResetBefore = requestSnapshot(tracker)
  const observationFilterResetStart = performance.now()
  let observationFilterResetMs
  if (evidencePlan.subjectTreeLiveFilters) observationFilterResetMs = await requiredLiveInputDoubleFrame(page, '#observation-query', '')
  else {
    await filterInput.fill('')
    observationFilterResetMs = await requiredDoubleFrame(page, '.observation-filter-apply')
  }
  await waitObservationMembers(page, Math.min(observationPageSize, fixture.observations.length), 'Observation filter reset')
  await waitForTransportQuiescence(page, tracker, 'Observation filter reset')
  const resetObservationPage = assertObservationPagination(tracker, observationFilterResetBefore, fixture, 0, null, 'Observation filter reset', evidencePlan.subjectTreeLiveFilters ? 'content' : null)
  const observationFilterResetSettledMs = performance.now() - observationFilterResetStart
  const resetProof = observationResetProof(tracker, fixture, resetObservationPage, Math.min(observationPageSize, fixture.observations.length))
  interactions.observationFilterResetMs = { ms: observationFilterResetMs, ...requestDelta(tracker, observationFilterResetBefore) }
  filters.observationReset = interactions.observationFilterResetMs

  const observationScaling = fixture.observationScaling ? await measureObservationScaling(page, tracker, fixture, cdp, blankHeap, dom, interactions) : null

  const timelineTabBefore = requestSnapshot(tracker)
  stage('Timeline')
  const timelineTabMs = await requiredDoubleFrame(page, '.tabs button:nth-child(3)')
  const timelineQualification = await waitTimeline(page, tracker, timelineTabBefore, fixture)
  interactions.timelineTabMs = { ms: timelineTabMs, ...requestDelta(tracker, timelineTabBefore) }
  tabSwitches.timeline = interactions.timelineTabMs
  dom.push({ tab: 'timeline', ...(await domMetrics(page)) })

  const projectsTabBefore = requestSnapshot(tracker)
  stage('Projects tab restore')
  const returnProjects = await measureProjectsTabReturn(page, fixture.workspace.id)
  await waitForWorkspaceReady(page, tracker, fixture, anchors.first, 'Projects tab representative restore')
  interactions.projectsTabMs = { ms: returnProjects.ms, restoreSetup: returnProjects.setup, ...requestDelta(tracker, projectsTabBefore) }
  tabSwitches.projects = interactions.projectsTabMs
  stage('direct focus')
  const directFocus = await measureDirectFocus(browser, origin, fixture)

  stage('pre-live Projects restore')
  await restoreWorkspace(page, tracker, fixture, anchors.first, 'pre-live representative Projects restore')
  resetProof.recipeCompleted = true

  const collapsedBranches = fixture.size === 'large' ? ['project:' + anchors.expanded] : []
  tracker.navigationProof.setCollapsed(collapsedBranches)
  const logicalReady = () => tracker.navigationProof.ready()
  const liveBefore = requestSnapshot(tracker)
  stage('live batch')
  const liveStart = performance.now()
  if (liveDiagnostics) { tracker.liveDiagnostics = liveDiagnostics; liveDiagnostics.start(liveStart) }
  const requiredNavigationOwners = prepareLiveNavigationProof(tracker, fixture, fixture.liveFrames)
  const pushedSockets = await page.evaluate(frames => window.__perfPushFrames(frames), fixture.liveFrames)
  liveDiagnostics?.dispatched(performance.now())
  if (pushedSockets !== 1) throw new Error(`live fixture expected one open socket, observed ${pushedSockets}`)
  await page.waitForFunction(expected => window.__perfLastPush?.frames === expected && window.__perfLastPush?.resumeToken === 'fixture-live-after', fixture.liveFrames.length)
  const settledAt = await waitForLiveSettle(page, tracker, liveBefore, anchors.first, logicalReady)
  liveDiagnostics?.settled(settledAt)
  const stability = await assertLiveStability(page, tracker, 500)
  liveDiagnostics?.stable(stability.startedAt, stability.endedAt)
  delete tracker.liveDiagnostics
  const liveTiming = liveTimingSummary({ startedAt: liveStart, settledAt, stabilityStartedAt: stability.startedAt, stabilityEndedAt: stability.endedAt })
  stage('post-live readiness')
  const postLiveUi = await projectUiSignals(page, anchors.first)
  const expectedSnapshotItems = workspaceShellSnapshotItems(fixture).length
  const expectedSnapshotPages = Math.ceil(expectedSnapshotItems / DIRECT_FOCUS_CONTRACT.pageSize)
  if (!representativeReadiness({
    modelComplete: transportContractReady({ protocol: 'bounded', transportedItems: tracker.model.snapshotItems, expectedItems: expectedSnapshotItems, fullBackingItems: snapshotItems(fixture).length, transportedPages: tracker.model.snapshotPages, expectedPages: expectedSnapshotPages, complete: tracker.model.snapshotComplete }),
    activeRequests: tracker.active,
    loading: postLiveUi.loading,
    focused: postLiveUi.focused,
    anchorVisible: postLiveUi.anchorVisible
  })) throw new Error('post-live representative workspace readiness failed')
  const liveDelta = requestDelta(tracker, liveBefore)
  const entityRoutes = Object.entries(liveDelta.routes).filter(([key]) => key.includes(':entity:'))
  const duplicateRequests = entityRoutes.reduce((sum, [, count]) => sum + Math.max(0, count - 1), 0)
  const updatedObservationIds = [...new Set(fixture.liveFrames.filter(frame => frame.event.workspace_id === fixture.workspace.id && frame.event.entity.type === 'observation' && frame.event.entity.action === 'updated').map(frame => frame.event.entity.id))]
  const hydration = classifyLiveObservationHydration({ queryScope: evidencePlan.subjectTreeLiveFilters ? 'content' : null, runId: tracker.paintRunId, workspaceId: fixture.workspace.id,
    total: fixture.observations.length, expectedPrefix: queryObservations(fixture, { offset: 0, limit: 200 }).items.map(item => item.id),
    updatedObservationIds,
    expectedObservationEntities: Object.fromEntries(fixture.observations.filter(value => updatedObservationIds.includes(value.id)).map(value => [value.id, { hash: hashJson(value), bytes: Buffer.byteLength(JSON.stringify(value)) }])),
    reset: resetProof, intervening: tracker.requests.slice(resetProof.requestIndex + 1, liveBefore.index), requests: tracker.requests.slice(liveBefore.index),
    liveStartIndex: liveBefore.index, delta: liveDelta, active: tracker.active, unknownRoutes: tracker.unhandledApiRoutes, snapshotGeneration: tracker.model.snapshotGeneration })
  const liveBatch = {
    requiredNavigationOwners, frames: fixture.liveFrames.length, sockets: pushedSockets, ms: liveTiming.settleMs, stabilityMs: liveTiming.stabilityMs, stabilityRequests: stability.requests,
    ...liveDelta, requests: liveDelta.count, duplicateRequests,
    wholeWorkspaceReload: liveWholeWorkspaceReload(liveDelta.routes), hydration
  }
  dom.push({ tab: 'post-live-projects', ...(await domMetrics(page)) })

  stage('Projects heap after garbage collection')
  await cdp.send('HeapProfiler.collectGarbage')
  const heap = await cdp.send('Runtime.getHeapUsage')
  assertNoUnhandledApiRoutes(tracker)
  const lifetimeRender = await page.evaluate(() => window.__perfRenderMaximum)
  dom.push({tab: 'lifetime-high-water', nodes: lifetimeRender.nodes, rows: lifetimeRender.rows})
  if (lifetimeRender.hierarchyRows > 31 || lifetimeRender.observers > 31) throw new Error('Mounted hierarchy/observer high-water exceeded 25 ordinary plus six active pins')
  const renderMaximums = renderMaximum(dom)
  const result = {
    fixture: fixture.size, measured, cold, background, lifetimeRender, tabSwitches, filters, interactions, observationLoadMore, observationScaling, directFocus, liveBatch, dom,
    ...(evidencePlan.subjectTreeLiveFilters ? { observationFilterSettlement: { filterMs: observationFilterSettledMs, resetMs: observationFilterResetSettledMs } } : {}),
    observationPagination: { revision: OBSERVATION_PAGINATION_CONTRACT.revision, initial: initialObservationPage, filtered: filteredObservationPage, reset: resetObservationPage },
    timelineQualification: { revision: TIMELINE_CURRENT_CONTRACT.revision, ...timelineQualification },
    maxDomNodes: renderMaximums.nodes, maxCollectionRows: renderMaximums.rows,
    attributableHeapBytes: Math.max(0, heap.usedSize - blankHeap),
    heapPoint: HARNESS_CONFIGURATION.heapPoint,
    readiness: { protocol: PRODUCTION_SNAPSHOT_PROFILE, fullBackingSnapshotItems: snapshotItems(fixture).length, expectedSnapshotPages, transportedSnapshotItems: tracker.model.snapshotItems, transportedSnapshotPages: tracker.model.snapshotPages, snapshotComplete: tracker.model.snapshotComplete, timelineEvents: tracker.model.timelineEvents, timelineBuckets: tracker.model.timelineBuckets, timelineBucketRequest: tracker.model.timelineBucketRequest, anchors },
    consoleErrors
  }
  if (trace) {
    evidenceOperations.verify()
    fs.mkdirSync(path.dirname(tracePath), { recursive: true })
    evidenceOperations.verify()
    await context.tracing.stop({ path: tracePath })
  }
  await context.close()
  return result
}

function aggregates(runs) {
  if (runs.length !== SAMPLES) throw new Error(`aggregate requires exactly ${SAMPLES} measured runs`)
  const values = (key, label) => assertFiveSamples(runs.map(key), label)
  const cold = values(run => run.cold.ms, 'cold')
  const focus = values(run => run.directFocus.ms, 'direct focus')
  if (runs.some(run => run.fixture !== runs[0].fixture)) throw new Error('Observation pagination samples changed scale')
  const observationLoadMore = aggregateObservationLoadMore(runs.map(run => run.observationLoadMore), runs[0].fixture, evidencePlan.subjectTreeLiveFilters ? 'content' : null)
  const live = values(run => run.liveBatch.ms, 'live batch')
  const interactionNames = Object.keys(runs[0].interactions).sort()
  if (interactionNames.length === 0) throw new Error('at least one named local interaction is required')
  const localScenarios = Object.fromEntries(interactionNames.map(name => {
    if (runs.some(run => JSON.stringify(Object.keys(run.interactions).sort()) !== JSON.stringify(interactionNames))) {
      throw new Error('named interaction set changed between samples')
    }
    const rawMs = values(run => run.interactions[name].ms, `local interaction ${name}`)
    const requestDeltas = runs.map(run => validateRequestDelta({
      count: run.interactions[name].count,
      bytes: run.interactions[name].bytes,
      routes: run.interactions[name].routes,
      routeBytes: run.interactions[name].routeBytes
    }, `local interaction ${name}`))
    return [name, {
      rawMs, medianMs: median(rawMs), p95Ms: nearestRankP95(rawMs), requestDeltas,
      requestCounts: requestDeltas.map(delta => delta.count), fixtureBytes: requestDeltas.map(delta => delta.bytes),
      routeCounts: requestDeltas.map(delta => delta.routes), routeBytes: requestDeltas.map(delta => delta.routeBytes)
    }]
  }))
  const liveRequestDeltas = runs.map(run => validateRequestDelta({
    count: run.liveBatch.count,
    bytes: run.liveBatch.bytes,
    routes: run.liveBatch.routes,
    routeBytes: run.liveBatch.routeBytes
  }, 'live batch'))
  return {
    observationScaling: aggregateObservationScaling(runs),
    ...(evidencePlan.subjectTreeLiveFilters ? { observationFilterSettlement: Object.fromEntries(['filterMs', 'resetMs'].map(key => { const rawMs = values(run => run.observationFilterSettlement[key], 'live filter settlement ' + key); return [key, { rawMs, medianMs: median(rawMs), p95Ms: nearestRankP95(rawMs) }] })) } : {}),
    background: runs.map(run => run.background),
    lifetimeRender: runs.map(run => run.lifetimeRender),
    cold: { rawMs: cold, medianMs: median(cold), p95Ms: nearestRankP95(cold), requestCounts: runs.map(run => run.cold.requests), fixtureBytes: runs.map(run => run.cold.bytes), entityOverviewRequests: runs.map(run => run.cold.entityOverviewRequests), canonicalSnapshotItems: runs.map(run => run.cold.canonicalSnapshotItems), canonicalSnapshotPages: runs.map(run => run.cold.canonicalSnapshotPages) },
    render: { maxDomNodes: Math.max(...runs.map(run => run.maxDomNodes)), maxCollectionRows: Math.max(...runs.map(run => run.maxCollectionRows)) },
    localInteractions: { scenarios: localScenarios, worstP95Ms: Math.max(...Object.values(localScenarios).map(value => value.p95Ms)) },
    directFocus: {
      rawMs: focus, medianMs: median(focus), p95Ms: nearestRankP95(focus),
      requestCounts: runs.map(run => run.directFocus.count), entityLookupRequests: runs.map(run => run.directFocus.entityLookupRequests),
      requested: runs.map(run => run.directFocus.directFocusRequested), rendered: runs.map(run => run.directFocus.directFocusRendered),
      failureReasons: runs.map(run => run.directFocus.productFailureReason)
    },
    observationLoadMore,
    liveBatch: {
      rawMs: live, medianMs: median(live), p95Ms: nearestRankP95(live), requestDeltas: liveRequestDeltas,
      requestCounts: liveRequestDeltas.map(delta => delta.count), fixtureBytes: liveRequestDeltas.map(delta => delta.bytes),
      routeCounts: liveRequestDeltas.map(delta => delta.routes), routeBytes: liveRequestDeltas.map(delta => delta.routeBytes),
      duplicateRequests: runs.map(run => run.liveBatch.duplicateRequests), wholeWorkspaceReload: runs.some(run => run.liveBatch.wholeWorkspaceReload),
      hydration: runs.map(run => run.liveBatch.hydration), hydrationPassed: aggregateLiveHydration(runs.map(run => run.liveBatch.hydration), evidencePlan.subjectTreeLiveFilters ? 'content' : null).passed
    },
    heap: { values: runs.map(run => run.attributableHeapBytes), maxAttributableBytes: Math.max(...runs.map(run => run.attributableHeapBytes)) },
    consoleErrors: runs.flatMap(run => run.consoleErrors)
  }
}

function evaluate(result, comparableEnvironment = true) {
  const target = budgets.large
  const small = result.aggregates.small
  const large = result.aggregates.large
  const metrics = []
  const add = (name, actual, expected, pass, category = 'exact') => metrics.push({ name, actual, expected, pass, category })
  add('cold HTTP requests', Math.max(...large.cold.requestCounts), target.cold.maxHttpRequests, Math.max(...large.cold.requestCounts) <= target.cold.maxHttpRequests)
  add('cold fixture bytes', Math.max(...large.cold.fixtureBytes), target.cold.maxFixtureBytes, Math.max(...large.cold.fixtureBytes) <= target.cold.maxFixtureBytes)
  add('entity overview requests', Math.max(...large.cold.entityOverviewRequests), target.cold.maxEntityOverviewRequests, Math.max(...large.cold.entityOverviewRequests) <= target.cold.maxEntityOverviewRequests)
  add('canonical snapshot items', Math.min(...large.cold.canonicalSnapshotItems), target.cold.requiredCanonicalSnapshotItems, large.cold.canonicalSnapshotItems.every(value => value === target.cold.requiredCanonicalSnapshotItems))
  add('canonical snapshot pages', Math.min(...large.cold.canonicalSnapshotPages), target.cold.requiredCanonicalSnapshotPages, large.cold.canonicalSnapshotPages.every(value => value === target.cold.requiredCanonicalSnapshotPages))
  const requestDelta = Math.max(...large.cold.requestCounts) - Math.max(...small.cold.requestCounts)
  add('small-to-large request delta', requestDelta, target.scaling.maxSmallToLargeRequestDelta, requestDelta <= target.scaling.maxSmallToLargeRequestDelta)
  const renderEvaluation = renderBudgetEvaluation({ nodes: large.render.maxDomNodes, rows: large.render.maxCollectionRows }, target.render)
  add('DOM nodes', large.render.maxDomNodes, target.render.maxDomNodes, renderEvaluation.nodesPass)
  add('rendered collection rows/cards', large.render.maxCollectionRows, target.render.maxCollectionRows, renderEvaluation.rowsPass)
  add('direct focus requests', Math.max(...large.directFocus.requestCounts), target.directFocus.maxRequests, Math.max(...large.directFocus.requestCounts) <= target.directFocus.maxRequests)
  add('direct focus requested', large.directFocus.requested.every(Boolean), target.directFocus.requireRequested, large.directFocus.requested.every(Boolean) === target.directFocus.requireRequested)
  add('direct focus rendered', large.directFocus.rendered.every(Boolean), target.directFocus.requireRendered, large.directFocus.rendered.every(Boolean) === target.directFocus.requireRendered)
  metrics.push(...observationPaginationMetrics(small.observationLoadMore, large.observationLoadMore, target.observationLoadMore, comparableEnvironment))
  metrics.push(...observationScalingMetrics(large.observationScaling, target.observationLoadMore, comparableEnvironment))
  add('live follow-up requests', Math.max(...large.liveBatch.requestCounts), target.liveBatch.maxFollowUpRequests, Math.max(...large.liveBatch.requestCounts) <= target.liveBatch.maxFollowUpRequests)
  add('small live whole-workspace hydration', small.liveBatch.hydrationPassed, true, small.liveBatch.hydrationPassed === true)
  add('live whole-workspace hydration', large.liveBatch.hydrationPassed, true, large.liveBatch.hydrationPassed === true)
  add('live repeated-target duplicate requests', Math.max(...large.liveBatch.duplicateRequests), target.liveBatch.maxDuplicateRequestsPerRepeatedTarget, Math.max(...large.liveBatch.duplicateRequests) <= target.liveBatch.maxDuplicateRequestsPerRepeatedTarget)
  add('browser console errors', large.consoleErrors.length, 0, large.consoleErrors.length === 0)
  for (const [scenario, values] of Object.entries(large.localInteractions.scenarios)) {
    add(`local interaction ${scenario} p95 ms`, values.p95Ms, target.localInteraction.maxP95Ms, !comparableEnvironment || values.p95Ms <= target.localInteraction.maxP95Ms, comparableEnvironment ? 'pinned-environment' : 'informational')
  }
  for (const [name, actual, expected] of [
    ['cold median ms', large.cold.medianMs, target.cold.maxMedianMs], ['cold p95 ms', large.cold.p95Ms, target.cold.maxP95Ms],
    ['local interaction worst p95 ms', large.localInteractions.worstP95Ms, target.localInteraction.maxP95Ms], ['direct focus p95 ms', large.directFocus.p95Ms, target.directFocus.maxP95Ms],
    ['live batch p95 ms', large.liveBatch.p95Ms, target.liveBatch.maxP95Ms],
    ['attributable heap bytes', large.heap.maxAttributableBytes, target.heap.maxAttributableBytes]
  ]) add(name, actual, expected, !comparableEnvironment || actual <= expected, comparableEnvironment ? 'pinned-environment' : 'informational')
  return { comparableEnvironment, passed: metrics.every(metric => metric.pass), metrics }
}

export function verifyTaskTrace({ plan = evidencePlan, io = fs, operations = evidenceOperations, captureOptions = traceCaptureOptions, onAdmission = () => {} } = {}) {
  operations.verify()
  if (!io.existsSync(plan.trace)) throw new Error(`record trace was not created: ${plan.trace}`)
  const sizeBytes = io.statSync(plan.trace).size
  const admission = traceAdmissionReceipt(sizeBytes, 64 * 1024 * 1024, captureOptions)
  onAdmission(admission)
  if (!admission.withinLimit) throw Object.assign(new Error('Task temporary trace exceeds its 64 MiB bound: ' + sizeBytes + ' bytes'), { traceAdmission: admission })
  const digest = () => createHash('sha256').update(io.readFileSync(plan.trace)).digest('hex')
  const firstHash = digest(), secondHash = digest()
  if (firstHash !== secondHash) throw new Error('record trace hash was not reproducible during verification')
  return { sha256: firstHash, sizeBytes }
}

export function retireTaskTrace(plan, operations, io = fs) {
  operations.verify()
  try { operations.remove(plan.trace) } catch (error) { if (error.code !== 'ENOENT') throw error }
  const temporary = path.dirname(plan.trace)
  if (!io.existsSync(temporary)) return
  if (io.readdirSync(temporary).length !== 0) throw new Error('Unexpected temporary trace remains in task scratch')
  operations.verify()
  io.rmdirSync(temporary)
}

function persistQualification(result, prerequisite, qualifiedInputs, command, retirement) {
  if (!retirement.passed) throw new Error('Cannot finalize qualification before every owned resource retires')
  assertQualifiedInputs(qualifiedInputs, inputQualification(), 'qualification inputs changed before finalization')
  if (evidencePlan.strictSourceIdentity) {
    if (fs.existsSync(tracePath)) throw new Error('Task qualification cannot retain the temporary trace archive')
    result.trace = { ...result.trace, archiveAvailable: false }
    assertEvidenceIdentity(evidencePlan, result)
  }
  if (mode === 'record') {
    const traceManifest = { schemaVersion: 1, baseCommit: BASE_COMMIT, taskId: evidenceTask?.taskId || null, ...evidenceOwnerFields(evidencePlan), evidenceBaseCommit, measurementRevision: evidenceRevision, inputQualification: qualifiedInputs, sourceProvenance: result.sourceProvenance, contracts: result.contracts, trace: result.trace, retirement }
    writeEvidence(recordOutputPath, JSON.stringify(result, null, 2) + '\n')
    writeEvidence(recordTraceManifestPath, JSON.stringify(traceManifest, null, 2) + '\n')
    if (recordOutputPath !== baselinePath && !evidencePlan.qualifiedRetirement) finalWorkingTreeEvidence()
  }
  if (evidencePlan.qualifiedRetirement) {
    const previous = mode === 'check' ? JSON.parse(fs.readFileSync(validationRecordPath, 'utf8')) : null
    if (previous) assertEvidenceIdentity(evidencePlan, previous)
    if (previous && (previous.taskId !== evidenceTask.taskId || JSON.stringify(previous.inputQualification) !== JSON.stringify(qualifiedInputs))) throw new Error('validation record does not match the qualified record inputs')
    const commands = previous ? [...previous.commands, command] : [prerequisite, command]
    const validation = {
      schemaVersion: 1, taskId: evidenceTask.taskId, parentTaskId: evidenceTask.parentTaskId, evidenceBaseCommit, measurementRevision: evidenceRevision,
      ...evidenceOwnerFields(evidencePlan),
      inputQualification: qualifiedInputs, sourceProvenance: result.sourceProvenance, commands, retirement,
      passed: mode === 'check' && checkQualification({ recordEvaluation: previous?.recordEvaluation, checkEvaluation: result.evaluation, commands, retirement }).passed,
      recordEvaluation: previous?.recordEvaluation || result.evaluation,
      check: mode === 'check' ? { environment: result.environment, runs: result.runs, aggregates: result.aggregates, evaluation: result.evaluation } : null
    }
    writeEvidence(validationRecordPath, JSON.stringify(validation, null, 2) + '\n')
    finalWorkingTreeEvidence()
  }
  if (mode === 'record') console.log('AUTHORIZED RECORD finalized after owned retirement: ' + recordOutputPath + '; actual budget evaluation is preserved.')
  if (mode === 'check' && evidenceTask && !evidencePlan.qualifiedRetirement && result.evaluation.passed && fs.existsSync(validationRecordPath)) finalWorkingTreeEvidence()
}

function qualificationAttempt(command, qualifiedInputs, retirement, action, coldDiagnostics = []) {
  if (!evidencePlan.qualifiedRetirement) return action()
  return persistEvidenceAttempt({
    write: writeEvidence,
    remove: file => evidenceOperations.remove(file),
    verify: evidenceOperations.verify,
    manifestPath: evidenceManifestPath, validationPath: validationRecordPath,
    failureValidation: error => ({ schemaVersion: 1, taskId: evidenceTask.taskId, parentTaskId: evidenceTask.parentTaskId,
      ...evidenceOwnerFields(evidencePlan), ...(evidencePlan.strictSourceIdentity ? { sourceProvenance: attemptState.sourceProvenance } : {}),
      evidenceBaseCommit, measurementRevision: evidenceRevision, inputQualification: qualifiedInputs,
      commands: [{ ...command, exitCode: 2 }], retirement, coldDiagnostics, passed: false, failure: error.message,
      ...(error.observationMembership ? { observationMembership: error.observationMembership } : {}),
      ...(error.timelineQualification ? { timelineQualification: error.timelineQualification } : {}),
      ...(error.navigationFeedback ? { navigationFeedback: error.navigationFeedback } : {}),
      ...(error.workspaceReadiness ? { workspaceReadiness: error.workspaceReadiness } : {}),
      ...(error.traceAdmission ? { traceAdmission: error.traceAdmission } : {}) }),
    failureManifest: error => ({ schemaVersion: 1, taskId: evidenceTask.taskId, evidenceBaseCommit, measurementRevision: evidenceRevision,
      ...evidenceOwnerFields(evidencePlan), ...(evidencePlan.strictSourceIdentity ? { sourceProvenance: attemptState.sourceProvenance } : {}),
      qualificationPassed: false, failure: error.message, retirement,
      ...(error.observationMembership ? { observationMembership: error.observationMembership } : {}),
      ...(error.timelineQualification ? { timelineQualification: error.timelineQualification } : {}),
      ...(error.navigationFeedback ? { navigationFeedback: error.navigationFeedback } : {}),
      ...(error.workspaceReadiness ? { workspaceReadiness: error.workspaceReadiness } : {}),
      ...(error.traceAdmission ? { traceAdmission: error.traceAdmission } : {}),
      validationRecord: { path: evidenceArtifactPath(validationRecordPath), sha256: sha256File(validationRecordPath) } })
  }, action)
}



async function main() {
  attemptState = { startedAtUtc: new Date().toISOString(), started: performance.now(), phase: 'startup', pendingFailure: false, traceVerified: false, retirement: null }
  if (!process.env.HMEM_EVIDENCE_BASE_COMMIT) throw new Error('Set HMEM_EVIDENCE_BASE_COMMIT explicitly before record or check; no implicit historical evidence writes')
  prepareEvidenceScratch(evidencePlan, os.tmpdir())
  if (evidencePlan.isolatedEvidence) {
    evidenceOperations.verify()
    const manifest = fs.existsSync(evidenceManifestPath) ? JSON.parse(fs.readFileSync(evidenceManifestPath, 'utf8')) : null
    if (manifest) assertEvidenceIdentity(evidencePlan, manifest)
    const disposition = startupEvidenceDisposition(manifest)
    attemptState.pendingFailure = disposition.pendingFailure
    if (disposition.invalidate) evidenceOperations.remove(evidenceManifestPath)
  }
  // The final v2 write is deliberately last: its fixture/self-check validation
  // must succeed before any review artifact is replaced.
  const commandStartedAtUtc = new Date().toISOString(), commandStarted = performance.now()
  attemptState.phase = 'prerequisite self-check'
  const prerequisite = runTaskEvidencePrerequisites()
  if (!fs.existsSync(path.join(staticRoot, 'index.html'))) throw new Error(`production build missing at ${staticRoot}; run npm run build first`)
  const generate = evidencePlan.scaling ? generateObservationScalingFixture : generateFixture
  const fixtures = { small: generate('small'), large: generate('large') }
  const qualifiedInputs = inputQualification()
  const qualifiedProvenance = sourceProvenance()
  attemptState.sourceProvenance = qualifiedProvenance
  const contracts = {
    budgetsHash: hashJson(budgets),
    ...(evidencePlan.subjectTreeLiveFilters ? { subjectTreeLiveFiltersContractHash: hashJson(SUBJECT_TREE_LIVE_FILTERS_CONTRACT) } : {}),
    configurationHash: hashJson(HARNESS_CONFIGURATION),
    directFocusContractHash: hashJson(DIRECT_FOCUS_CONTRACT),
    observationPaginationContractHash: hashJson(OBSERVATION_PAGINATION_CONTRACT),
    observationPaginationSource: observationPaginationSource(),
    timelineCurrentContractHash: hashJson(TIMELINE_CURRENT_CONTRACT),
    timelineCurrentSource: timelineSource(),
    settledNavigationFeedbackContractHash: hashJson(SETTLED_NAVIGATION_FEEDBACK_CONTRACT),
    settledNavigationFeedbackSource: settledNavigationSource(),
    liveObservationRefreshContractHash: hashJson(LIVE_OBSERVATION_REFRESH_CONTRACT),
    liveObservationRefreshSource: liveObservationRefreshSource(),
    ...(evidencePlan.scaling ? { observationScalingContractHash: hashJson(OBSERVATION_SCALING_CONTRACT) } : {}),
    ...(evidencePlan.planningCounts ? { planningCountsContractHash: hashJson({ ...PLANNING_COUNTS_CONTRACT, traceCaptureOptions }) } : {}),
    ...(evidencePlan.rendering ? { observationReachabilityContractHash: hashJson({ revision: 'logical-scroll.v1', flat: [50, 100], match: [50, 100, 150], completeOrderedContextualCards: true, physicalBudgetSelectorsUnchanged: true, scansOutsideMeasuredActionIntervals: true }) } : {}),
    fixtures: { small: fixtureHash(fixtures.small), large: fixtureHash(fixtures.large) },
    snapshots: { small: snapshotHash(fixtures.small), large: snapshotHash(fixtures.large) }
  }
  let recordedBaseline = null
  let recordedTraceManifest = null
  if (mode === 'check') {
    if (!fs.existsSync(baselinePath)) throw new Error(`approved baseline missing: ${baselinePath}`)
    if (!fs.existsSync(traceManifestPath)) throw new Error(`trace provenance manifest missing: ${traceManifestPath}`)
    const immutableBaseline = JSON.parse(fs.readFileSync(baselinePath, 'utf8'))
    const immutableTraceManifest = JSON.parse(fs.readFileSync(traceManifestPath, 'utf8'))
    if (immutableBaseline.baseCommit !== BASE_COMMIT) throw new Error(`baseline base commit mismatch: expected ${BASE_COMMIT}, observed ${immutableBaseline.baseCommit}`)
    if (JSON.stringify(immutableTraceManifest.trace) !== JSON.stringify(immutableBaseline.trace)) throw new Error('immutable baseline and trace provenance differ')
    if (immutableBaseline.recordAuthorization !== 'explicit --authorize-baseline') throw new Error('baseline lacks explicit record authorization provenance')
    if (!fs.existsSync(afterArtifactPath) || !fs.existsSync(afterTraceArtifactPath)) throw new Error('final working-tree after evidence is missing; run npm run perf:record-after')
    recordedBaseline = JSON.parse(fs.readFileSync(afterArtifactPath, 'utf8'))
    recordedTraceManifest = JSON.parse(fs.readFileSync(afterTraceArtifactPath, 'utf8'))
    assertEvidenceIdentity(evidencePlan, recordedBaseline)
    assertEvidenceIdentity(evidencePlan, recordedTraceManifest)
    assertQualifiedInputs(recordedBaseline.inputQualification, qualifiedInputs, 'recorded source/production asset fingerprints differ from current inputs')
    assertQualifiedInputs(recordedTraceManifest.inputQualification, qualifiedInputs, 'trace input fingerprints differ from current inputs')
    if (recordedBaseline.baseCommit !== BASE_COMMIT) throw new Error(`after-artifact base commit mismatch: expected ${BASE_COMMIT}, observed ${recordedBaseline.baseCommit}`)
    if (JSON.stringify(recordedBaseline.contracts) !== JSON.stringify(contracts)) throw new Error(`baseline fixture/configuration/budget contracts do not match current inputs`)
    if (JSON.stringify(recordedTraceManifest.contracts) !== JSON.stringify(contracts)) throw new Error(`after trace manifest fixture/configuration/budget contracts do not match current inputs`)
    if (JSON.stringify(recordedTraceManifest.trace) !== JSON.stringify(recordedBaseline.trace)) throw new Error('after trace manifest and after artifact provenance differ')
    if (recordedBaseline.recordAuthorization !== 'explicit --authorize-after-artifact') throw new Error('after artifact lacks explicit record authorization provenance')
  }
  const packageVersion = relativePath => {
    const file = path.join(frontendRoot, 'node_modules', ...relativePath, 'package.json')
    if (!fs.existsSync(file)) throw new Error(`required package metadata missing: ${file}`)
    const version = JSON.parse(fs.readFileSync(file, 'utf8')).version
    if (!version) throw new Error(`required package version missing: ${file}`)
    return version
  }
  const toolVersions = {
    node: requiredCommandVersion('Node', process.execPath, ['--version']),
    npm: requiredCommandVersion('npm', platformExecutable('npm'), ['--version']),
    elm: requiredCommandVersion('Elm', path.join(frontendRoot, 'node_modules', '.bin', platformExecutable('elm')), ['--version']),
    vite: packageVersion(['vite']),
    playwright: packageVersion(['@playwright', 'test'])
  }
  attemptState.phase = 'browser setup'
  const server = await staticServer()
  let browser = null, browserServer = null, measuredResult = null, failure = null, retirement = null
  const diagnosticRuns = []
  try {
    browserServer = await chromium.launchServer({ headless: true, args: ['--js-flags=--expose-gc', '--enable-precise-memory-info'] })
    console.log('OWNED_CHROMIUM_PID=' + browserServer.process()?.pid)
    browser = await chromium.connect(browserServer.wsEndpoint())
    const environment = {
      os: `${os.type()} ${os.release()} ${os.arch()}`, cpu: `${os.cpus()[0]?.model || 'unknown'} (${os.cpus().length} logical)`, ramBytes: os.totalmem(),
      browserControl: 'Playwright BrowserServer/connect with explicit owned process retirement',
      ...toolVersions, chromium: browser.version(), viewport: `${HARNESS_CONFIGURATION.viewport.width}x${HARNESS_CONFIGURATION.viewport.height}`, headless: true
    }
    environment.fingerprint = hashJson(environment)
    if (evidencePlan.liveObservationSearch) assertQualificationEnvironment(environment)
    attemptState.phase = 'deep-focus prerequisite'
    process.stdout.write('deep-focus continuation... ')
    const staleFocusContinuation = await verifyStaleDeepFocusContinuation(browser, server.origin)
    const deepFocusContinuation = { ...await verifyDeepFocusContinuation(browser, server.origin), ...staleFocusContinuation }
    process.stdout.write(`${deepFocusContinuation.requests} bounded requests\n`)
    const runs = { small: [], large: [] }
    for (const size of ['small', 'large']) {
      for (let index = 0; index < WARMUPS + SAMPLES; index += 1) {
        const measured = index >= WARMUPS
        process.stdout.write(`${size} ${measured ? `sample ${index - WARMUPS + 1}/${SAMPLES}` : `warmup ${index + 1}/${WARMUPS}`}... `)
        const captureTrace = mode === 'record' && size === 'large' && measured && index === WARMUPS + SAMPLES - 1
        attemptState.phase = size + ' ' + (measured ? 'sample ' + (index - WARMUPS + 1) : 'warmup ' + (index + 1))
        const result = await measureRun(browser, server.origin, fixtures[size], measured, captureTrace, diagnosticRuns)
        if (measured) runs[size].push(result)
        process.stdout.write(`${Math.round(result.cold.ms)}ms, ${result.cold.requests} requests\n`)
      }
    }
    let trace = recordedBaseline?.trace || null
    if (mode === 'record') {
      attemptState.phase = 'trace verification'
      evidenceOperations.verify()
      if (!fs.existsSync(tracePath)) throw new Error(`record trace was not created: ${tracePath}`)
      const traceSizeBytes = fs.statSync(tracePath).size
      if (evidencePlan.isolatedEvidence) {
        const admission = traceAdmissionReceipt(traceSizeBytes, 64 * 1024 * 1024, traceCaptureOptions)
        console.log('TRACE_ARCHIVE_ADMISSION=' + JSON.stringify(admission))
        if (!admission.withinLimit) throw Object.assign(new Error('Task temporary trace exceeds its 64 MiB bound: ' + traceSizeBytes + ' bytes'), { traceAdmission: admission })
      }
      const verified = evidencePlan.strictSourceIdentity
        ? verifyTaskTrace({ onAdmission: () => {} })
        : { sha256: sha256File(tracePath) }
      if (!evidencePlan.strictSourceIdentity && verified.sha256 !== sha256File(tracePath)) throw new Error('record trace hash was not reproducible during verification')
      attemptState.traceVerified = true
      trace = {
        logicalName: 'large measured sample 5/5 Playwright trace',
        sha256: verified.sha256,
        sizeBytes: traceSizeBytes,
        ...(evidencePlan.isolatedEvidence ? { captureOptions: traceCaptureOptions, temporaryLimitBytes: 64 * 1024 * 1024 } : {}),
        capturedRun: 'large sample 5/5 after two warmups',
        verifiedDuringRecord: true,
        retention: evidencePlan.isolatedEvidence ? 'temporary trace under the verified outside-repository task directory; removed before successful qualification; verified provenance remains in the retained trace manifest' : evidenceRevision === 'expanded-hierarchy.v1' ? 'generated under task-owned .scratch/expanded-navigation-perf, intentionally removed by finite owned cleanup before success finalization; this versioned manifest retains verified provenance and does not claim the opaque archive is available' : 'generated under ignored perf/.artifacts, intentionally removed after record/check; this versioned manifest retains verified provenance and does not claim the opaque archive is available'
      }
    }
    const result = {
      schemaVersion: 1, baseCommit: BASE_COMMIT, recordedAtUtc: new Date().toISOString(),
      recordAuthorization: mode === 'record'
        ? (recordOutputPath === baselinePath ? 'explicit --authorize-baseline' : 'explicit --authorize-after-artifact')
        : recordedBaseline.recordAuthorization,
      environment, configuration: HARNESS_CONFIGURATION, contracts,
      observationPaginationContract: OBSERVATION_PAGINATION_CONTRACT,
      timelineCurrentContract: TIMELINE_CURRENT_CONTRACT,
      settledNavigationFeedbackContract: SETTLED_NAVIGATION_FEEDBACK_CONTRACT,
      liveObservationRefreshContract: LIVE_OBSERVATION_REFRESH_CONTRACT,
      taskId: evidenceTask?.taskId || null, parentTaskId: evidenceTask?.parentTaskId || null, evidenceBaseCommit,
      ...evidenceOwnerFields(evidencePlan),
      inputQualification: qualifiedInputs, sourceProvenance: qualifiedProvenance, measurementRevision: evidenceRevision,
      fixtures: {
        schemaVersion: fixtures.large.schemaVersion, seed: fixtures.large.seed, directFocusContract: DIRECT_FOCUS_CONTRACT,
        deepFocusContinuation,
        small: { hash: fixtureHash(fixtures.small), snapshotHash: snapshotHash(fixtures.small), scale: fixtures.small.scale, directFocusTarget: directFocusFixture(fixtures.small).targetProject },
        large: { hash: fixtureHash(fixtures.large), snapshotHash: snapshotHash(fixtures.large), scale: fixtures.large.scale, directFocusTarget: directFocusFixture(fixtures.large).targetProject }
      },
      runs, aggregates: { small: aggregates(runs.small), large: aggregates(runs.large) },
      trace,
      hotspots: [
        { downstreamTask: 'bd2eba73-6e33-4bc1-8a45-3b024c662343', evidence: ['src/Feature/DataLoading.elm:297 recursive 200-row project paging then per-project overview fan-out', 'src/Feature/DataLoading.elm:349 recursive 200-row task paging then per-task overview fan-out', 'canonical resync snapshot pages are counted by change-stream:resync'] },
        { downstreamTask: '2503e08f-ff82-4f2c-adff-24e14fec8299', evidence: ['Projects/Observations/Timeline DOM and input-to-next-paint measurements', '50-frame canonical mixed/repeated-target request coalescing and settle measurement'] }
      ]
    }
    let comparable = true
    if (mode === 'check') comparable = recordedBaseline.environment.fingerprint === environment.fingerprint
    result.evaluation = evaluate(result, comparable)
    attemptState.phase = 'budget evaluation and input verification'
    assertQualifiedInputs(qualifiedInputs, inputQualification(), 'qualification inputs changed during measurement')
    measuredResult = result
    for (const metric of result.evaluation.metrics) console.log((metric.pass ? 'PASS' : 'FAIL') + ' ' + metric.name + ': ' + metric.actual + ' (budget ' + metric.expected + (metric.category === 'informational' ? ', informational environment' : '') + ')')
  } catch (error) {
    failure = error
  } finally {
    retirement = await retireOwnedResources([
      { resource: 'browser connection', close: async () => { if (browser) await browser.close() } },
      { resource: 'owned Chromium process', close: async () => {
        if (browserServer) {
          await browserServer.kill()
          const child = browserServer.process()
          if (child && child.exitCode == null && child.signalCode == null) throw new Error('Owned Chromium still alive after retirement')
        }
      } },
      { resource: 'owned HTTP server', close: async () => {
        server.server.closeAllConnections()
        await new Promise((resolve, reject) => server.server.close(error => error ? reject(error) : resolve()))
      } },
      { resource: 'task trace', close: async () => {
        if (!evidencePlan.qualifiedRetirement) return
        if (evidencePlan.strictSourceIdentity) { retireTaskTrace(evidencePlan, evidenceOperations); return }
        if (evidencePlan.isolatedEvidence) evidenceOperations.verify()
        try { evidenceOperations.remove(tracePath) } catch (error) { if (error.code !== 'ENOENT') throw error }
        if (evidencePlan.isolatedEvidence) {
          const temporary = path.dirname(tracePath)
          const contents = await fs.promises.readdir(temporary).catch(error => { if (error.code === 'ENOENT') return null; throw error })
          if (contents?.length === 0) { evidenceOperations.verify(); await fs.promises.rmdir(temporary) }
          else if (contents) throw new Error('Unexpected temporary trace remains in task scratch')
          return
        }
        const remaining = await fs.promises.readdir(expandedScratch).catch(error => { if (error.code === 'ENOENT') return null; throw error })
        if (remaining?.length === 0) await fs.promises.rmdir(expandedScratch)
      } }
    ], 5000)
  }
  const command = {
    command: 'node perf/harness.mjs ' + mode,
    invocation: 'HMEM_EVIDENCE_BASE_COMMIT=' + evidenceBaseCommit + ' npm run perf:' + (mode === 'record' ? 'record-after' : 'check'),
    measuredPhase: 'node harness only; preceding npm production build is excluded',
    exitCode: mode === 'check' && measuredResult && !measuredResult.evaluation.passed ? 1 : 0,
    startedAtUtc: commandStartedAtUtc, finishedAtUtc: new Date().toISOString(), durationMs: Math.round(performance.now() - commandStarted)
  }
  attemptState.retirement = retirement
  if (mode === 'check' && measuredResult && !failure && retirement.passed && evidencePlan.qualifiedRetirement) {
    evidenceOperations.verify()
    const previous = JSON.parse(fs.readFileSync(validationRecordPath, 'utf8'))
    assertEvidenceIdentity(evidencePlan, previous)
    command.exitCode = checkQualification({ recordEvaluation: previous.recordEvaluation, checkEvaluation: measuredResult.evaluation,
      commands: [...previous.commands, command], retirement }).exitCode
  }
  if (!retirement.passed) failure = Object.assign(new Error((failure ? failure.message + '; ' : '') + 'owned cleanup failed: ' + retirement.receipts.filter(receipt => !receipt.passed).map(receipt => receipt.resource + ': ' + receipt.error).join('; ')), { ...(failure?.traceAdmission ? { traceAdmission: failure.traceAdmission } : {}), ...(failure?.observationMembership ? { observationMembership: failure.observationMembership } : {}), ...(failure?.timelineQualification ? { timelineQualification: failure.timelineQualification } : {}), ...(failure?.navigationFeedback ? { navigationFeedback: failure.navigationFeedback } : {}), ...(failure?.workspaceReadiness ? { workspaceReadiness: failure.workspaceReadiness } : {}) })
  if (failure) {
    persistOrRetainFailureDiagnostics(attemptState, () => qualificationAttempt(command, qualifiedInputs, retirement, () => { throw failure }, diagnosticRuns.map(run => ({ ...run, diagnostics: run.diagnostics.snapshot() }))))
    throw failure
  }
  measuredResult.retirement = retirement
  attemptState.phase = 'evidence persistence'
  const persistence = persistOrRetainFailureDiagnostics(attemptState, () => qualificationAttempt(command, qualifiedInputs, retirement, () => persistQualification(measuredResult, prerequisite, qualifiedInputs, command, retirement), diagnosticRuns.map(run => ({ ...run, diagnostics: run.diagnostics.snapshot() }))))
  if (persistence.preserved) throw new Error('Previous failed diagnostics remain unresolved; no evidence replaced')
  if (mode === 'check') process.exitCode = command.exitCode
}

if (path.resolve(process.argv[1] || '') === fileURLToPath(import.meta.url)) main().catch(error => {
  console.error(error.stack || error)
  if (evidencePlan.isolatedEvidence) console.error('BOUNDED_FAILURE_RECEIPT=' + JSON.stringify({
    taskId: evidenceTask.taskId, evidenceBaseCommit, phase: attemptState?.phase,
    startedAtUtc: attemptState?.startedAtUtc, elapsedMs: attemptState ? Math.round(performance.now() - attemptState.started) : null,
    failure: error.message.slice(0, 1000), traceAdmission: error.traceAdmission || null,
    observationMembership: error.observationMembership || null,
    timelineQualification: error.timelineQualification || null,
    navigationFeedback: error.navigationFeedback || null,
    workspaceReadiness: error.workspaceReadiness || null,
    captureOptions: traceCaptureOptions, retirement: attemptState?.retirement,
    priorFailureDiagnosticsPreserved: !!attemptState?.pendingFailure && !(attemptState.traceVerified && attemptState.retirement?.passed),
    retainedPaths: [validationRecordPath, evidenceManifestPath]
  }))
  process.exitCode = 2
})
