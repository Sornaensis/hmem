import { execFileSync } from 'node:child_process'
import fs from 'node:fs'
import path from 'node:path'
import { fileURLToPath } from 'node:url'

const here = path.dirname(fileURLToPath(import.meta.url))
const frontendRoot = path.dirname(here)
const repositoryRoot = path.resolve(frontendRoot, '..', '..')
const baseCommit = '1bdbe3d071d1fbbb994bc515841103adedff77ec'
const recordPath = path.join(here, 'final-working-tree.validation-record.v3.json')
const npm = process.platform === 'win32' ? 'npm.cmd' : 'npm'
const environment = { ...process.env, HMEM_EVIDENCE_BASE_COMMIT: baseCommit }
const records = []

function run(label, args) {
  const startedAtUtc = new Date().toISOString()
  const started = performance.now()
  try {
    const executable = process.platform === 'win32' ? (process.env.ComSpec || 'cmd.exe') : npm
    const commandArgs = process.platform === 'win32' ? ['/d', '/s', '/c', npm, ...args] : args
    execFileSync(executable, commandArgs, { cwd: frontendRoot, env: environment, stdio: 'inherit' })
    records.push({ command: label, exitCode: 0, startedAtUtc, finishedAtUtc: new Date().toISOString(), durationMs: Math.round(performance.now() - started) })
  } catch (error) {
    records.push({ command: label, exitCode: Number.isInteger(error.status) ? error.status : 1, startedAtUtc, finishedAtUtc: new Date().toISOString(), durationMs: Math.round(performance.now() - started) })
    fs.writeFileSync(recordPath, `${JSON.stringify({ schemaVersion: 1, taskId: '755a7286-6da5-494e-9b9b-fa1edd43d0b0', evidenceBaseCommit: baseCommit, commands: records, passed: false }, null, 2)}\n`)
    throw error
  }
}

function runStack(label, target) {
  const startedAtUtc = new Date().toISOString()
  const started = performance.now()
  try {
    execFileSync('stack', ['test', target], { cwd: repositoryRoot, env: environment, stdio: 'inherit' })
    records.push({ command: label, exitCode: 0, startedAtUtc, finishedAtUtc: new Date().toISOString(), durationMs: Math.round(performance.now() - started) })
  } catch (error) {
    records.push({ command: label, exitCode: Number.isInteger(error.status) ? error.status : 1, startedAtUtc, finishedAtUtc: new Date().toISOString(), durationMs: Math.round(performance.now() - started) })
    fs.writeFileSync(recordPath, `${JSON.stringify({ schemaVersion: 1, taskId: '755a7286-6da5-494e-9b9b-fa1edd43d0b0', evidenceBaseCommit: baseCommit, commands: records, passed: false }, null, 2)}\n`)
    throw error
  }
}

run('npm test', ['test'])
run('npm run build', ['run', 'build'])
run('npm run perf:self-check', ['run', 'perf:self-check'])
runStack('stack test hmem-core:hmem-core-test', 'hmem-core:hmem-core-test')
runStack('stack test hmem-server:hmem-server-test', 'hmem-server:hmem-server-test')
run('HMEM_EVIDENCE_BASE_COMMIT=1bdbe3d npm run perf:record-after', ['run', 'perf:record-after'])
run('HMEM_EVIDENCE_BASE_COMMIT=1bdbe3d npm run perf:check', ['run', 'perf:check'])

fs.writeFileSync(recordPath, `${JSON.stringify({ schemaVersion: 1, taskId: '755a7286-6da5-494e-9b9b-fa1edd43d0b0', evidenceBaseCommit: baseCommit, commands: records, passed: true, summary: { commandCount: records.length, totalDurationMs: records.reduce((total, record) => total + record.durationMs, 0) } }, null, 2)}\n`)

// The final check sees the complete immutable record and refreshes the exact
// manifest last; it leaves the source and approved baseline untouched.
execFileSync('node', ['perf/harness.mjs', 'check'], { cwd: frontendRoot, env: environment, stdio: 'inherit' })
