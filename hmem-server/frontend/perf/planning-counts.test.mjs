import test from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import path from 'node:path'
import { createHash } from 'node:crypto'
import { PLANNING_COUNTS_BASE, PLANNING_COUNTS_TASK, PLANNING_COUNTS_REVISION, OBSERVATION_SCALING_BASE, OBSERVATION_RENDERING_BASE, evidenceProfile, isolatedRecordOutputs, assertEvidenceIdentity, assertEvidenceWrite, prepareEvidenceScratch, createEvidenceOperations, checkQualification, startupEvidenceDisposition, persistOrRetainFailureDiagnostics } from './evidence-profile.mjs'
import { generateFixture, fixtureHash, workspacePlanningCounts } from './fixtures.mjs'
import { perfApiRouteKey, persistEvidenceAttempt, retireOwnedResources, HARNESS_CONFIGURATION, BASE_COMMIT } from './contracts.mjs'
import { fixtureResponder, createTracker, inputQualification, assertQualifiedInputs, finalWorkingTreeEvidence } from './harness.mjs'
import { observationTraceCaptureOptions, traceAdmissionReceipt } from './observation-scaling.mjs'

const frontend = path.resolve('fixture-root/hmem-server/frontend')
const temporary = path.resolve('outside-temporary')
const plan = () => evidenceProfile(PLANNING_COUNTS_BASE, frontend, temporary)
const identity = profile => ({ taskId: profile.task.taskId, evidenceBaseCommit: profile.baseCommit, measurementRevision: profile.revision })

test('planning evidence is isolated without enabling Observation measurements or altering historical paths', () => {
  const profile = plan()
  assert.equal(profile.task.taskId, PLANNING_COUNTS_TASK)
  assert.equal(profile.task.parentTaskId, 'e692aa02-9515-4265-956e-aac699d6e214')
  assert.equal(profile.revision, PLANNING_COUNTS_REVISION)
  assert.equal(profile.isolatedEvidence, true)
  assert.equal(profile.qualifiedRetirement, true)
  assert.equal(profile.scaling, false)
  assert.equal(profile.rendering, false)
  assert.equal(profile.root, path.join(temporary, 'hmem-planning-counts-' + PLANNING_COUNTS_TASK))
  assert.equal(new Set(Object.values(profile.files)).size, 5)
  for (const file of Object.values(profile.files)) {
    assert.equal(path.dirname(file), profile.root)
    assert.ok(path.basename(file).includes('.planning-counts.v1.'))
  }
  assert.equal(profile.trace, path.join(profile.root, 'temporary/large-planning-counts-trace.zip'))
  assert.match(profile.retention, /closure plus 30 days, then dispose/)
  assert.throws(() => evidenceProfile(PLANNING_COUNTS_BASE, frontend, frontend), /outside/)
  assert.throws(() => evidenceProfile('unmapped', frontend, temporary), /Unmapped/)
  for (const [base, prefix] of [[OBSERVATION_SCALING_BASE, 'hmem-observation-scaling-'], [OBSERVATION_RENDERING_BASE, 'hmem-observation-rendering-']]) {
    const historical = evidenceProfile(base, frontend, temporary)
    assert.equal(historical.scaling, true)
    assert.equal(historical.root, path.join(temporary, prefix + historical.task.taskId))
    assert.equal(path.basename(historical.trace), 'large-observation-trace.zip')
    assert.equal(historical.retention, 'Frontend owner; retain the five task artifacts until project closure plus 30 days; remove temporary trace before success')
  }
  for (const base of ['818cc3cc634bfa8c0ebe14c13fc70b4c2d059e83', '04fd7ba26b1a63b5f1c601939b046ef2fe5f51d6', '1bdbe3d071d1fbbb994bc515841103adedff77ec', 'd9ff753763f086fa4faef078b451f2e48ca1a7a4']) {
    const historical = evidenceProfile(base, frontend, temporary)
    assert.equal(historical.isolatedEvidence, false)
    assert.equal(historical.root, path.join(frontend, 'perf'))
  }
  assert.equal(BASE_COMMIT, 'ca04f51c3494c83c433d12bd7791f89be876daea')
  assert.equal(HARNESS_CONFIGURATION.warmups, 2)
  assert.equal(HARNESS_CONFIGURATION.samples, 5)
  // Current due-free generator; legacy payload parity is proved in due-removal.test.mjs.
  assert.equal(fixtureHash(generateFixture('small')), '0c425c77e5069b8d4cf26dbdebcf94037a22ee977e61d63ab3cceb1a7f090bc4')
  assert.equal(fixtureHash(generateFixture('large')), 'b98d53a1a1ca28641dc90cbb598f3026949edf702f0ae4696614520b5140dd63')
})

test('planning output authorization rejects baseline recording, redirects and foreign identity', () => {
  const profile = plan()
  const legacyOutput = path.join(frontend, 'perf/final-working-tree.after.v1.json')
  const legacyTrace = path.join(frontend, 'perf/final-working-tree.trace-manifest.v1.json')
  for (const [output, trace] of [[legacyOutput, legacyTrace], [profile.files.after, profile.files.traceManifest]]) {
    assert.deepEqual(isolatedRecordOutputs(profile, output, trace, legacyOutput, legacyTrace), { output: profile.files.after, traceManifest: profile.files.traceManifest })
  }
  for (const output of [path.join(frontend, 'perf/baseline.v1.json'), path.join(temporary, 'arbitrary.json')]) {
    assert.throws(() => isolatedRecordOutputs(profile, output, legacyTrace, legacyOutput, legacyTrace), /task-owned/)
  }
  assert.throws(() => isolatedRecordOutputs(profile, legacyOutput, path.join(temporary, 'arbitrary-trace.json'), legacyOutput, legacyTrace), /task-owned/)
  assert.doesNotThrow(() => assertEvidenceIdentity(profile, identity(profile)))
  for (const field of Object.keys(identity(profile))) assert.throws(() => assertEvidenceIdentity(profile, { ...identity(profile), [field]: 'foreign' }), /identity mismatch/)
  assert.throws(() => assertEvidenceIdentity(profile, null), /identity mismatch/)
  assert.doesNotThrow(() => assertEvidenceWrite(profile, profile.files.after, '{}'))
  assert.throws(() => assertEvidenceWrite(profile, legacyOutput, '{}'), /Unowned/)
  assert.throws(() => assertEvidenceWrite(profile, profile.files.after, Buffer.alloc(64 * 1024 * 1024 + 1)), /64 MiB/)
})

// Virtual filesystem: all ownership faults remain in memory, with no test scratch.
function scratchFixture() {
  const profile = plan()
  const entries = new Map([[temporary, { kind: 'directory', ino: 1 }], [profile.root, { kind: 'directory', ino: 2 }]])
  const operations = []
  const missing = () => Object.assign(new Error('Missing virtual file'), { code: 'ENOENT' })
  const io = {
    existsSync: file => entries.has(file),
    realpathSync: file => { const info = entries.get(file); if (!info) throw missing(); return info.real || file },
    lstatSync: file => { const info = entries.get(file); if (!info) throw missing(); return { dev: 1, ino: info.ino, size: info.bytes?.length || info.size || 0, isSymbolicLink: () => info.kind === 'symlink', isDirectory: () => info.kind === 'directory', isFile: () => info.kind === 'file' } },
    readdirSync: directory => [...entries.keys()].filter(file => path.dirname(file) === directory).map(file => path.basename(file)),
    mkdirSync: file => { operations.push(['mkdir', file]); entries.set(file, { kind: 'directory', ino: 4 }) },
    writeFileSync: (file, bytes) => { operations.push(['write', file]); entries.set(file, { kind: 'file', ino: 5, bytes: Buffer.from(bytes) }) },
    renameSync: (source, destination) => { operations.push(['rename', source, destination]); entries.set(destination, entries.get(source)); entries.delete(source) },
    unlinkSync: file => { operations.push(['remove', file]); if (!entries.delete(file)) throw missing() },
    readFileSync: (file, encoding) => { const bytes = entries.get(file)?.bytes; if (!bytes) throw missing(); return encoding ? bytes.toString(encoding) : bytes }
  }
  io.statSync = io.lstatSync
  const owned = createEvidenceOperations(profile, temporary, io)
  const attempt = action => persistEvidenceAttempt({ manifestPath: profile.files.manifest, validationPath: profile.files.validation, verify: owned.verify, write: owned.write, remove: owned.remove,
    failureValidation: error => ({ ...identity(profile), passed: false, failure: error.message }), failureManifest: error => ({ ...identity(profile), qualificationPassed: false, failure: error.message }) }, action)
  return { profile, entries, operations, io, owned, attempt }
}

test('planning ownership rejects redirected, replaced, unadmitted and undeclared storage without fallback', () => {
  for (const fault of ['unadmitted', 'symlink', 'parent-replaced', 'root-replaced', 'unexpected-file', 'oversized-file']) {
    const h = scratchFixture()
    if (fault !== 'unadmitted') prepareEvidenceScratch(h.profile, temporary, h.io)
    if (fault === 'symlink') Object.assign(h.entries.get(h.profile.root), { kind: 'symlink', real: path.resolve('foreign') })
    if (fault === 'parent-replaced') h.entries.get(temporary).ino = 100
    if (fault === 'root-replaced') h.entries.get(h.profile.root).ino = 100
    if (fault === 'unexpected-file') h.entries.set(path.join(h.profile.root, 'unowned.json'), { kind: 'file' })
    if (fault === 'oversized-file') h.entries.set(h.profile.files.after, { kind: 'file', size: 64 * 1024 * 1024 + 1 })
    assert.throws(() => h.owned.write(h.profile.files.validation, '{}'), /ownership|symlink|lifetime|Unexpected|64 MiB/)
    assert.throws(() => h.owned.remove(h.profile.files.manifest), /ownership|symlink|lifetime|Unexpected|64 MiB/)
    assert.throws(() => h.attempt(() => { throw new Error('cleanup failed') }), /ownership|symlink|lifetime|Unexpected|64 MiB/)
    assert.deepEqual(h.operations, [], fault)
  }
  const h = scratchFixture()
  prepareEvidenceScratch(h.profile, temporary, h.io)
  assert.throws(() => h.owned.remove(path.join(temporary, 'foreign')), /Unowned/)
  h.owned.write(h.profile.files.validation, '{}')
  assert.equal([...h.entries.keys()].some(file => file.endsWith('.tmp')), false)
  const write = h.io.writeFileSync
  h.io.writeFileSync = (file, bytes) => { write(file, bytes); h.entries.get(h.profile.root).ino = 999 }
  assert.throws(() => h.owned.write(h.profile.files.manifest, '{}'), /lifetime/)
  assert.equal(h.operations.at(-1)[0], 'write', 'no rename/removal after root replacement')
})

test('planning producer binds complete diff, current commit chain and five artifacts to its original base', () => {
  const h = scratchFixture()
  const baseline = Buffer.from('{"immutable":"baseline"}\n')
  for (const file of Object.values(h.profile.files)) h.entries.set(file, { kind: 'file', ino: 3, bytes: Buffer.from('{}') })
  h.entries.get(h.profile.files.validation).bytes = Buffer.from(JSON.stringify({ ...identity(h.profile), passed: true }))
  const read = h.io.readFileSync
  h.io.readFileSync = (file, encoding) => file === path.resolve('perf/baseline.v1.json') ? (encoding ? baseline.toString(encoding) : baseline) : read(file, encoding)
  const head = '1'.repeat(40), intermediate = '2'.repeat(40), reads = []
  const capture = { read(args) {
    reads.push(args)
    if (args[0] === 'show') return baseline
    if (args[0] === 'rev-parse') return Buffer.from(head + '\n')
    if (args[0] === 'rev-list') return Buffer.from(intermediate + '\n' + head + '\n')
    assert.ok(['diff', 'ls-files'].includes(args[0]))
    return Buffer.alloc(0)
  } }
  prepareEvidenceScratch(h.profile, temporary, h.io)
  finalWorkingTreeEvidence({ plan: h.profile, io: h.io, write: h.owned.write, capture })
  const manifest = JSON.parse(h.io.readFileSync(h.profile.files.manifest, 'utf8'))
  assertEvidenceIdentity(h.profile, manifest)
  assert.equal(manifest.qualificationPassed, true)
  assert.equal(manifest.sourceProvenance.reviewBaseCommit, PLANNING_COUNTS_BASE)
  assert.equal(manifest.sourceProvenance.headCommit, head)
  assert.deepEqual(manifest.sourceProvenance.orderedCommits, [intermediate, head])
  assert.deepEqual(manifest.artifacts.map(value => value.path), [h.profile.files.after, h.profile.files.traceManifest, h.profile.files.diff, h.profile.files.validation])
  assert.ok(reads.some(args => args[0] === 'show' && args[1].startsWith(PLANNING_COUNTS_BASE + ':')))
  assert.equal(h.entries.size, 7, 'one virtual parent/root and five declared artifacts')
})

test('planning failure invalidates stale success and preserves only unresolved diagnostic evidence', () => {
  const h = scratchFixture()
  prepareEvidenceScratch(h.profile, temporary, h.io)
  h.owned.write(h.profile.files.manifest, JSON.stringify({ ...identity(h.profile), qualificationPassed: true }))
  assert.deepEqual(startupEvidenceDisposition(JSON.parse(h.io.readFileSync(h.profile.files.manifest, 'utf8'))), { invalidate: true, pendingFailure: false })
  assert.throws(() => h.attempt(() => { throw new Error('original assertion failed') }), /original assertion failed/)
  const failed = JSON.parse(h.io.readFileSync(h.profile.files.manifest, 'utf8'))
  assertEvidenceIdentity(h.profile, failed)
  assert.equal(failed.qualificationPassed, false)
  const disposition = startupEvidenceDisposition(failed)
  assert.equal(disposition.pendingFailure, true)
  let replaced = false
  assert.deepEqual(persistOrRetainFailureDiagnostics({ ...disposition, traceVerified: false, retirement: { passed: true } }, () => { replaced = true }), { preserved: true })
  assert.equal(replaced, false)
  assert.equal(persistOrRetainFailureDiagnostics({ ...disposition, traceVerified: true, retirement: { passed: true } }, () => { replaced = true }).preserved, false)
  assert.equal(replaced, true)
})

test('planning qualification rejects failed evaluations, prerequisite commands, trace admission and retirement', async () => {
  const commands = [{ command: 'npm run perf:self-check', exitCode: 0 }, { command: 'node perf/harness.mjs record', exitCode: 0 }, { command: 'node perf/harness.mjs check', exitCode: 0 }]
  const valid = { commands, recordEvaluation: { passed: true }, checkEvaluation: { passed: true }, retirement: { passed: true } }
  assert.deepEqual(checkQualification(valid), { passed: true, exitCode: 0 })
  for (const invalid of [{ recordEvaluation: { passed: false } }, { checkEvaluation: { passed: false } }, { retirement: { passed: false } }, { commands: commands.slice(1) }, { commands: commands.map(command => ({ ...command, exitCode: 2 })) }]) assert.equal(checkQualification({ ...valid, ...invalid }).passed, false)
  const calls = []
  const retirement = await retireOwnedResources([{ resource: 'browser', close: async () => { calls.push('browser'); throw new Error('retirement failed') } }, { resource: 'trace', close: async () => { calls.push('trace') } }], 100)
  assert.deepEqual(calls, ['browser', 'trace'])
  assert.equal(checkQualification({ ...valid, retirement }).passed, false)
  const options = observationTraceCaptureOptions(plan().isolatedEvidence)
  assert.deepEqual(options, { screenshots: false, snapshots: true, sources: false })
  assert.equal(traceAdmissionReceipt(64 * 1024 * 1024, 64 * 1024 * 1024, options).withinLimit, true)
  assert.equal(traceAdmissionReceipt(64 * 1024 * 1024 + 1, 64 * 1024 * 1024, options).withinLimit, false)
})

test('planning qualification rejects current source and production-asset drift', () => {
  const profile = plan()
  const recorded = inputQualification({ plan: profile })
  assert.ok(recorded.source.some(value => value.path.endsWith('/perf/fixtures.mjs')))
  assert.ok(recorded.source.some(value => value.path.endsWith('/perf/evidence-profile.mjs')))
  for (const subject of [recorded.source.find(value => value.path.endsWith('/perf/fixtures.mjs')), recorded.productionAssets[0]]) {
    const changed = inputQualification({ plan: profile, io: { ...fs, readFileSync(file, ...args) {
      return file.split(path.sep).join('/').endsWith(subject.path) ? Buffer.concat([fs.readFileSync(file), Buffer.from('\nchanged bytes')]) : fs.readFileSync(file, ...args)
    } } })
    assert.notEqual(createHash('sha256').update(JSON.stringify(changed)).digest('hex'), createHash('sha256').update(JSON.stringify(recorded)).digest('hex'))
    assert.throws(() => assertQualifiedInputs(recorded, changed, 'planning qualification input drift'), /input drift/)
  }
  assert.doesNotThrow(() => assertQualifiedInputs(recorded, inputQualification({ plan: profile }), 'planning qualification input drift'))
})

test('planning counts use open current identities once regardless of hierarchy, assignment or closed ancestors', () => {
  const fixture = generateFixture('small')
  const workspace = fixture.workspace.id
  const project = (id, status, extra = {}) => ({ id, status, workspace_id: workspace, ...extra })
  const task = project
  fixture.projects = [project('parent', 'completed'), project('child', 'paused', { parent_id: 'parent' }), project('open', 'active'), project('archived', 'archived'), project('deleted', 'active', { deleted_at: '2026-01-01' }), project('foreign', 'active', { workspace_id: 'other' })]
  fixture.tasks = [task('unassigned', 'todo', { project_id: null, parent_id: null }), task('parent-task', 'in_progress', { project_id: 'child' }), task('subtask', 'blocked', { parent_id: 'parent-task' }), task('done', 'done'), task('cancelled', 'cancelled'), task('deleted', 'todo', { deleted_at: '2026-01-01' }), task('foreign', 'todo', { workspace_id: 'other' })]
  fixture.projects.push({ ...fixture.projects[1] })
  fixture.tasks.push({ ...fixture.tasks[2] })
  assert.deepEqual(workspacePlanningCounts(fixture), { open_projects: 2, open_tasks: 3 })
})

test('planning fixture wire fields match the committed production decoder and backend DTO', () => {
  const decoder = fs.readFileSync('src/Api.elm', 'utf8').split('workspacePlanningCountsDecoder =')[1].split('fetchWorkspacePlanningCounts :')[0]
  const fields = [...decoder.matchAll(/D\.field "([^"]+)" D\.int/g)].map(match => match[1])
  assert.deepEqual(fields, ['open_projects', 'open_tasks'])
  assert.deepEqual(Object.keys(workspacePlanningCounts(generateFixture('small'))), fields)
  const dto = fs.readFileSync('../../hmem-core/src/HMem/Types.hs', 'utf8').split('data WorkspacePlanningCounts =')[1].split('instance ToJSON WorkspacePlanningCounts')[0]
  assert.match(dto, /openProjects :: Int/)
  assert.match(dto, /openTasks :: Int/)
})

test('actual planning route accounts each current live status/deletion response and rejects wrong method/workspace', async () => {
  const fixture = generateFixture('small')
  const tracker = createTracker(fixture), respond = fixtureResponder(fixture, tracker)
  const pathname = '/api/v1/workspaces/' + fixture.workspace.id + '/planning-counts'
  const reply = async (suffix = pathname, method = 'GET') => {
    let response
    const request = { url: () => 'http://perf.local' + suffix, method: () => method }
    await respond({ request: () => request, fulfill: async value => { response = value } })
    return { ...response, value: JSON.parse(response.body) }
  }
  assert.equal(perfApiRouteKey(pathname + '?query=ignored', 'GET'), 'workspaces:planning-counts')
  assert.equal(perfApiRouteKey(pathname, 'POST'), null)
  assert.equal(perfApiRouteKey(pathname + '/extra', 'GET'), null)
  const first = await reply()
  assert.equal(first.status, 200)
  assert.deepEqual(first.value, workspacePlanningCounts(fixture))
  const openTask = fixture.tasks.find(task => ['todo', 'in_progress', 'blocked'].includes(task.status))
  const openProject = fixture.projects.find(project => ['active', 'paused'].includes(project.status))
  fixture.tasks = fixture.tasks.map(task => task.id === openTask.id ? { ...task, status: 'cancelled' } : task)
  fixture.projects = fixture.projects.map(project => project.id === openProject.id ? { ...project, deleted_at: '2026-01-01' } : project)
  const refreshed = await reply()
  assert.deepEqual(refreshed.value, { open_projects: first.value.open_projects - 1, open_tasks: first.value.open_tasks - 1 })
  fixture.tasks.push({ ...openTask, id: 'new-live-subtask', parent_id: openTask.id, project_id: null, status: 'todo' })
  const created = await reply()
  assert.equal(created.value.open_tasks, first.value.open_tasks)
  assert.equal(tracker.counts['workspaces:planning-counts'], 3)
  assert.equal(tracker.bytes['workspaces:planning-counts'], first.body.length + refreshed.body.length + created.body.length)
  assert.equal(tracker.active, 0)
  assert.ok(tracker.requests.every(receipt => receipt.done && receipt.key === 'workspaces:planning-counts'))
  assert.equal((await reply(pathname, 'POST')).status, 501)
  assert.equal((await reply(pathname.replace(fixture.workspace.id, 'foreign'))).status, 501)
  fixture.workspace.deleted_at = '2026-01-01'
  assert.equal((await reply()).status, 404)
})
