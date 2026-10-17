import path from 'node:path'
import fs from 'node:fs'
import { atomicEvidenceWrite } from './contracts.mjs'

export const OBSERVATION_SCALING_BASE = 'c5230077a643d06e7ca5a4ff6b5616cfb90862ec'
export const OBSERVATION_SCALING_TASK = 'b9f1de65-f016-4178-89a0-ac13a5d7fff8'
export const OBSERVATION_SCALING_REVISION = 'observation-scaling.v1'
export const OBSERVATION_RENDERING_BASE = 'ed3aca53afe346f928dc00f2e2a7b0a8e07aaecb'
export const OBSERVATION_RENDERING_TASK = 'cbb38fd2-fc89-449c-a308-168374f23f82'
export const OBSERVATION_RENDERING_REVISION = 'observation-rendering.v1'
export const PLANNING_COUNTS_BASE = 'f546cafd26adae37772d2383262e87596ea7151b'
export const PLANNING_COUNTS_TASK = 'b401c358-1a68-404e-bd18-86f40bb3b771'
export const PLANNING_COUNTS_REVISION = 'planning-counts.v1'
export const DUE_REMOVAL_BASE = '33abd80dd13dbc6c850397eeb13f79047d0f7c91'
export const DUE_REMOVAL_TASK = '1e420484-81b9-4251-b485-82abca34c800'
export const DUE_REMOVAL_PROJECT = '9f1a1a40-afc5-4825-851c-b71aaf498b58'
export const DUE_REMOVAL_REVISION = 'due-removal.v1'
export const LOADING_EFFICIENCY_BASE = 'dc44350bcb8360a7979d82f33271ad8dcdcb9b83'
export const LOADING_EFFICIENCY_TASK = '0a677d44-065a-4205-af9b-6e1d9b36a833'
export const LOADING_EFFICIENCY_PROJECT = 'd2ba5afb-4f51-4328-aa1a-fcb95510f495'
export const LOADING_EFFICIENCY_REVISION = 'loading-efficiency.v1'
const legacyBase = '818cc3cc634bfa8c0ebe14c13fc70b4c2d059e83'
const historicalTasks = new Map([
  ['04fd7ba26b1a63b5f1c601939b046ef2fe5f51d6', { revision: 'v2', taskId: '2503e08f-ff82-4f2c-adff-24e14fec8299' }],
  ['1bdbe3d071d1fbbb994bc515841103adedff77ec', { revision: 'v3', taskId: '755a7286-6da5-494e-9b9b-fa1edd43d0b0' }],
  ['d9ff753763f086fa4faef078b451f2e48ca1a7a4', { revision: 'expanded-hierarchy.v1', taskId: '555ebf6e-8442-43f4-a75d-c33fcffc76f4', parentTaskId: '6a3f31c2-be12-4aa8-a460-8ec3274332a3' }],
  [OBSERVATION_SCALING_BASE, { revision: OBSERVATION_SCALING_REVISION, taskId: OBSERVATION_SCALING_TASK }],
  [OBSERVATION_RENDERING_BASE, { revision: OBSERVATION_RENDERING_REVISION, taskId: OBSERVATION_RENDERING_TASK }],
  [PLANNING_COUNTS_BASE, { revision: PLANNING_COUNTS_REVISION, taskId: PLANNING_COUNTS_TASK, parentTaskId: 'e692aa02-9515-4265-956e-aac699d6e214' }],
  [DUE_REMOVAL_BASE, { revision: DUE_REMOVAL_REVISION, taskId: DUE_REMOVAL_TASK, projectId: DUE_REMOVAL_PROJECT }],
  [LOADING_EFFICIENCY_BASE, { revision: LOADING_EFFICIENCY_REVISION, taskId: LOADING_EFFICIENCY_TASK, projectId: LOADING_EFFICIENCY_PROJECT }]
])

export function evidenceProfile(baseCommit, frontendRoot, temporaryRoot) {
  if (baseCommit !== legacyBase && !historicalTasks.has(baseCommit)) throw new Error('Unmapped evidence base: ' + baseCommit)
  const task = historicalTasks.get(baseCommit) || null
  const revision = task?.revision || 'v1'
  const rendering = revision === OBSERVATION_RENDERING_REVISION
  const scaling = revision === OBSERVATION_SCALING_REVISION || rendering
  const planningCounts = revision === PLANNING_COUNTS_REVISION
  const dueRemoval = revision === DUE_REMOVAL_REVISION
  const loadingEfficiency = revision === LOADING_EFFICIENCY_REVISION
  const strictSourceIdentity = dueRemoval || loadingEfficiency
  const isolatedEvidence = scaling || planningCounts || dueRemoval || loadingEfficiency
  const prefix = loadingEfficiency ? 'hmem-loading-efficiency-' : dueRemoval ? 'hmem-due-removal-' : planningCounts ? 'hmem-planning-counts-' : rendering ? 'hmem-observation-rendering-' : 'hmem-observation-scaling-'
  const root = isolatedEvidence ? path.resolve(temporaryRoot, prefix + task.taskId) : path.join(frontendRoot, 'perf')
  const repositoryRoot = path.resolve(frontendRoot, '../..')
  const fromRepository = path.relative(repositoryRoot, root)
  if (isolatedEvidence && !fromRepository.startsWith('..' + path.sep) && !path.isAbsolute(fromRepository)) throw new Error('Task evidence must be outside the repository')
  const files = {
    after: path.join(root, `final-working-tree.after.${revision}.json`),
    traceManifest: path.join(root, `final-working-tree.trace-manifest.${revision}.json`),
    validation: path.join(root, `final-working-tree.validation-record.${revision}.json`),
    manifest: path.join(root, `final-working-tree.evidence-manifest.${revision}.json`),
    diff: path.join(root, isolatedEvidence || revision === 'expanded-hierarchy.v1' ? `final-working-tree.complete.${revision}.diff` : 'final-working-tree.complete.diff')
  }
  return { baseCommit, task, revision, scaling, rendering, planningCounts, dueRemoval, loadingEfficiency, strictSourceIdentity, isolatedEvidence, root, files,
    qualifiedRetirement: isolatedEvidence || revision === 'expanded-hierarchy.v1',
    trace: isolatedEvidence ? path.join(root, 'temporary', loadingEfficiency ? 'large-loading-efficiency-trace.zip' : dueRemoval ? 'large-due-removal-trace.zip' : planningCounts ? 'large-planning-counts-trace.zip' : 'large-observation-trace.zip') : null,
    retention: loadingEfficiency ? `Final verification task ${LOADING_EFFICIENCY_TASK}, project ${LOADING_EFFICIENCY_PROJECT}; retain the five task artifacts until project closure plus 30 days, then dispose; remove temporary trace before success; targeted failure diagnostics expire at resolution or task closure` : dueRemoval ? `Final verification task ${DUE_REMOVAL_TASK}, project ${DUE_REMOVAL_PROJECT}; retain the five task artifacts until project closure plus 30 days, then dispose; remove temporary trace before success; targeted failure diagnostics expire at resolution or task closure` : planningCounts ? 'Frontend task owner; retain the five task artifacts until project closure plus 30 days, then dispose; remove temporary trace before success; targeted failure diagnostics expire at resolution or task closure' : scaling ? 'Frontend owner; retain the five task artifacts until project closure plus 30 days; remove temporary trace before success' : null }
}

export function evidenceOwnerFields(profile) {
  return profile.loadingEfficiency ? { projectId: LOADING_EFFICIENCY_PROJECT } : profile.dueRemoval ? { projectId: DUE_REMOVAL_PROJECT } : {}
}

function assertIsolatedProfile(profile) {
  if (profile.loadingEfficiency) {
    const names = { after: 'after', traceManifest: 'trace-manifest', validation: 'validation-record', manifest: 'evidence-manifest', diff: 'complete' }
    if (profile.baseCommit !== LOADING_EFFICIENCY_BASE || profile.revision !== LOADING_EFFICIENCY_REVISION
      || profile.scaling || profile.rendering || profile.planningCounts || profile.dueRemoval || !profile.strictSourceIdentity || !profile.isolatedEvidence || !profile.qualifiedRetirement
      || profile.task?.taskId !== LOADING_EFFICIENCY_TASK || profile.task?.projectId !== LOADING_EFFICIENCY_PROJECT
      || path.basename(profile.root) !== 'hmem-loading-efficiency-' + LOADING_EFFICIENCY_TASK || Object.keys(profile.files).length !== 5
      || Object.entries(names).some(([key, name]) => profile.files[key] !== path.join(profile.root, `final-working-tree.${name}.${LOADING_EFFICIENCY_REVISION}.${key === 'diff' ? 'diff' : 'json'}`))
      || profile.trace !== path.join(profile.root, 'temporary', 'large-loading-efficiency-trace.zip')) throw new Error('Loading-efficiency evidence profile owner/base/revision/root mismatch')
    return
  }
  if (!profile.dueRemoval) return
  const expectedRootName = 'hmem-due-removal-' + DUE_REMOVAL_TASK
  const expectedFiles = ['after', 'traceManifest', 'validation', 'manifest', 'diff']
  const names = ['after', 'trace-manifest', 'validation-record', 'evidence-manifest', 'complete']
  if (profile.baseCommit !== DUE_REMOVAL_BASE || profile.revision !== DUE_REMOVAL_REVISION
    || profile.scaling || profile.rendering || !profile.isolatedEvidence || !profile.qualifiedRetirement
    || profile.task?.taskId !== DUE_REMOVAL_TASK || profile.task?.projectId !== DUE_REMOVAL_PROJECT
    || path.basename(profile.root) !== expectedRootName || Object.keys(profile.files).length !== 5
    || expectedFiles.some((key, index) => profile.files[key] !== path.join(profile.root, `final-working-tree.${names[index]}.${DUE_REMOVAL_REVISION}.${key === 'diff' ? 'diff' : 'json'}`))
    || profile.trace !== path.join(profile.root, 'temporary', 'large-due-removal-trace.zip')) throw new Error('Due-removal evidence profile owner/base/revision/root mismatch')
}

export function isolatedRecordOutputs(profile, requestedOutput, requestedTrace, legacyOutput, legacyTrace) {
  if (!profile.isolatedEvidence) return null
  assertIsolatedProfile(profile)
  if (![profile.files.after, legacyOutput].includes(requestedOutput) || ![profile.files.traceManifest, legacyTrace].includes(requestedTrace)) throw new Error('Isolated record paths must match the task-owned evidence profile')
  return { output: profile.files.after, traceManifest: profile.files.traceManifest }
}

export function assertEvidenceIdentity(profile, record) {
  assertIsolatedProfile(profile)
  if (profile.isolatedEvidence && (record?.taskId !== profile.task.taskId || record?.evidenceBaseCommit !== profile.baseCommit || record?.measurementRevision !== profile.revision)) throw new Error('Task evidence task/base/revision identity mismatch')
  if (profile.strictSourceIdentity && (record?.projectId !== profile.task.projectId
    || record?.sourceProvenance?.reviewBaseCommit !== profile.baseCommit
    || !/^[a-f0-9]{40}$/.test(record?.sourceProvenance?.headCommit || '')
    || (record.inputQualification && record.inputQualification.implementationCommit !== record.sourceProvenance.headCommit))) throw new Error('Task evidence project/source revision identity mismatch')
}

const admittedScratch = new WeakMap()
const directoryIdentity = info => `${info.dev}:${info.ino}`

function inspectEvidenceScratch(profile, temporaryRoot, io, transientFiles = new Set()) {
  if (!profile.isolatedEvidence) return
  assertIsolatedProfile(profile)
  const parent = io.realpathSync(temporaryRoot)
  const expected = path.join(parent, path.basename(profile.root))
  if (path.resolve(profile.root) !== expected) throw new Error('Task scratch parent is not the verified temporary directory')
  const rootInfo = io.lstatSync(profile.root), parentInfo = io.lstatSync(parent)
  if (rootInfo.isSymbolicLink() || !rootInfo.isDirectory() || io.realpathSync(profile.root) !== expected) throw new Error('Task scratch must not redirect through a symlink')
  const identity = { parent, parentIdentity: directoryIdentity(parentInfo), rootIdentity: directoryIdentity(rootInfo) }
  const admitted = admittedScratch.get(profile)
  if (admitted && JSON.stringify(admitted) !== JSON.stringify(identity)) throw new Error('Task scratch parent/root lifetime changed')
  const allowed = new Set([...Object.values(profile.files).map(file => path.basename(file)), 'temporary', ...[...transientFiles].map(file => path.basename(file))])
  for (const name of io.readdirSync(profile.root)) {
    if (!allowed.has(name)) throw new Error('Unexpected file in task-owned Task scratch: ' + name)
    const info = io.lstatSync(path.join(profile.root, name))
    if (info.isSymbolicLink() || (name === 'temporary' ? !info.isDirectory() : !info.isFile())) throw new Error('Task scratch contains a redirected or unsupported entry')
    if (name !== 'temporary' && info.size > 64 * 1024 * 1024) throw new Error('Task evidence exceeds its 64 MiB per-file scratch bound')
  }
  const temporary = path.dirname(profile.trace)
  if (io.existsSync(temporary) && (io.realpathSync(temporary) !== temporary || io.readdirSync(temporary).some(name => name !== path.basename(profile.trace)))) throw new Error('Unexpected temporary trace ownership')
  if (io.existsSync(profile.trace) && (!io.lstatSync(profile.trace).isFile() || io.lstatSync(profile.trace).isSymbolicLink())) throw new Error('Task trace must be an owned regular file')
  return identity
}

export function prepareEvidenceScratch(profile, temporaryRoot, io = fs) {
  if (!profile.isolatedEvidence) return
  assertIsolatedProfile(profile)
  if (admittedScratch.has(profile)) return verifyEvidenceScratch(profile, temporaryRoot, io)
  const parent = io.realpathSync(temporaryRoot)
  if (path.resolve(profile.root) !== path.join(parent, path.basename(profile.root))) throw new Error('Task scratch parent is not the verified temporary directory')
  if (!io.existsSync(profile.root)) io.mkdirSync(profile.root)
  admittedScratch.set(profile, inspectEvidenceScratch(profile, temporaryRoot, io))
}

export function verifyEvidenceScratch(profile, temporaryRoot, io = fs, transientFiles = new Set()) {
  if (!profile.isolatedEvidence) return
  if (!admittedScratch.has(profile)) throw new Error('Task scratch ownership has not been admitted; persistence stopped')
  try { inspectEvidenceScratch(profile, temporaryRoot, io, transientFiles) }
  catch (error) { throw new Error(error.message + '; evidence persistence stopped; no fallback writes or removals', { cause: error }) }
}

export function createEvidenceOperations(profile, temporaryRoot, io = fs) {
  const transientFiles = new Set()
  const verify = () => verifyEvidenceScratch(profile, temporaryRoot, io, transientFiles)
  return {
    verify,
    remove(file) {
      if (profile.isolatedEvidence && ![...Object.values(profile.files), profile.trace].includes(file)) throw new Error('Unowned task evidence removal')
      verify(); io.unlinkSync(file)
    },
    write(file, bytes) {
      assertEvidenceWrite(profile, file, bytes)
      verify()
      atomicEvidenceWrite(file, bytes, {
        write(temporary, value, options) { verify(); transientFiles.add(temporary); io.writeFileSync(temporary, value, options) },
        rename(temporary, destination) { verify(); io.renameSync(temporary, destination) },
        remove(temporary) {
          // Only this atomic operation can authorize its transient filename.
          verify()
          if (!transientFiles.has(temporary)) return
          try { io.unlinkSync(temporary) } finally { transientFiles.delete(temporary) }
        }
      })
    }
  }
}

export function assertEvidenceWrite(profile, file, bytes) {
  if (!profile.isolatedEvidence) return
  assertIsolatedProfile(profile)
  if (!Object.values(profile.files).includes(file)) throw new Error('Unowned Task evidence output')
  if (Buffer.byteLength(bytes) > 64 * 1024 * 1024) throw new Error('Task evidence exceeds its 64 MiB per-file scratch bound')
}

// Preserve the existing Observation profile interface.
export const scalingRecordOutputs = isolatedRecordOutputs
export const prepareScalingScratch = prepareEvidenceScratch
export const verifyScalingScratch = verifyEvidenceScratch
export const assertScalingEvidenceWrite = assertEvidenceWrite

export function checkQualification({ recordEvaluation, checkEvaluation, commands, retirement }) {
  const passed = recordEvaluation?.passed === true && checkEvaluation?.passed === true
    && retirement?.passed === true && commands?.length >= 3
    && commands[0]?.command === 'npm run perf:self-check'
    && commands.some(command => command?.command === 'node perf/harness.mjs record')
    && commands.at(-1)?.command === 'node perf/harness.mjs check'
    && commands.every(command => command?.exitCode === 0)
  return { passed, exitCode: passed ? 0 : 1 }
}

export function startupEvidenceDisposition(manifest) {
  return { invalidate: manifest?.qualificationPassed === true,
    pendingFailure: manifest?.qualificationPassed === false && typeof manifest.failure === 'string' }
}

export function persistOrRetainFailureDiagnostics({ pendingFailure, traceVerified, retirement }, action) {
  if (pendingFailure && !(traceVerified && retirement?.passed === true)) return { preserved: true }
  return { preserved: false, result: action() }
}
