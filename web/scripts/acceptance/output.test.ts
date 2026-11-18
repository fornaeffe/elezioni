import { afterEach, expect, it, vi } from 'vitest';
import { mkdtemp, readFile, rm } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import type { TestCase, TestResult } from '@playwright/test/reporter';
import { runLoggedCommand } from './logged-command';
import { campaignFiles, campaignProgress, campaignResult, campaignSummary } from './campaign-output';
import CompactReporter from './compact-reporter';

const directories: string[] = [];
async function temporary() { const path = await mkdtemp(join(tmpdir(), 'elezioni-acceptance-output-')); directories.push(path); return path; }
afterEach(async () => { vi.restoreAllMocks(); await Promise.all(directories.splice(0).map(path => rm(path, { recursive: true, force: true }))); });

it('retains large stdout and stderr in full, bounds failure output and preserves exit codes', async () => {
  const cwd = await temporary(), log = join(cwd, 'stage.log');
  const stdout = vi.spyOn(console, 'log').mockImplementation(() => {});
  const stderr = vi.spyOn(console, 'error').mockImplementation(() => {});
  const result = await runLoggedCommand({ name: 'failure', command: process.execPath, cwd, log,
    args: ['-e', "require('fs').writeSync(1, 'BEGIN' + 'x'.repeat(100000)); require('fs').writeSync(2, 'STDERR-END'); process.exitCode=7;"] });
  expect(result).toMatchObject({ status: 'failed', exitCode: 7, log });
  const contents = await readFile(log, 'utf8');
  expect(contents).toHaveLength(100015);
  expect(contents).toMatch(/^BEGINx+STDERR-END$/);
  expect(stdout.mock.calls.flat().join('\n').length).toBeLessThan(1000);
  expect(stderr.mock.calls.flat().join('\n')).toContain('STDERR-END');
  expect(stderr.mock.calls.flat().join('\n').length).toBeLessThan(4000);
});

it('records successful commands and missing-executable failures without hanging', async () => {
  const cwd = await temporary();
  vi.spyOn(console, 'log').mockImplementation(() => {});
  vi.spyOn(console, 'error').mockImplementation(() => {});
  expect(await runLoggedCommand({ name: 'success', command: process.execPath, args: ['-e', "console.log('OK')"], cwd, log: join(cwd, 'success.log') }))
    .toMatchObject({ status: 'passed', exitCode: 0 });
  const missing = await runLoggedCommand({ name: 'missing', command: join(cwd, 'missing-executable'), args: [], cwd, log: join(cwd, 'missing.log') });
  expect(missing.status).toBe('failed');
  expect(missing.error).toContain('ENOENT');
  expect(await readFile(missing.log, 'utf8')).toContain('Spawn error:');
});

it('keeps genuine progress counters and does not fabricate unavailable counts', () => {
  const input = { model: 'M02', law: 'reform', simulations: 1000, seconds: 65, privateBytes: 2 * 1024 ** 3 };
  const line = campaignProgress({ ...input, phase: 'Esecuzione\nOperazione in corso\nEsecuzione\ncompute · 235/1000\nAnnulla\nM02 · dati verificati' });
  expect(line).toContain('compute · 235/1000');
  expect(line).toContain('elapsed=65s; private=2.00 GiB');
  expect(line).not.toMatch(/\n|Annulla|Operazione in corso/);
  const unavailable = campaignProgress({ ...input, phase: 'Esecuzione\nAnnulla' });
  expect(unavailable).toContain('requested=1000');
  expect(unavailable).not.toContain('0/1000');
});

it('summarizes legal failures separately and retains evidence paths and unknown values', () => {
  const files = campaignFiles({ CP_RESOURCE_OUTPUT: '.', CP_RESOURCE_MODEL: 'M01', CP_RESOURCE_LAW: 'rosatellum', CP_RESOURCE_SIMULATIONS: '1000' });
  const summary = campaignSummary({ state: 'passed', simulations: 1000, outcomes: { complete: 985, partial: 0, failed: 15 },
    legalAcceptance: { accepted: true, acceptedFailureIds: Array.from({ length: 15 }, (_, i) => i + 1), unexpectedSimulationIds: [], unmatchedErrorSimulationIds: [] },
    infrastructureFailures: [], pageErrors: [] }, files);
  expect(summary.outcomes).toEqual({ complete: 985, partial: 0, failed: 15 });
  expect(summary.legalAcceptance).toMatchObject({ accepted: true, approvedFailureCount: 15 });
  expect(summary).toMatchObject({ receipt: files.receipt, log: files.log, tests: files.tests, infrastructureFailureCount: 0, archive: null });
  expect(campaignResult(summary)).toContain('complete/partial/failed=985/0/15; legal=true approvedFailures=15 unexpected=0 unmatched=0');
  expect(campaignResult(summary)).not.toContain('\n');
  expect(campaignSummary({}, files)).toMatchObject({ outcomes: null, legalAcceptance: null, infrastructureFailureCount: null, pageErrorCount: null });
});

it('keeps all reporter output on disk while forwarding compact progress and bounded failures', async () => {
  const cwd = await temporary(), log = join(cwd, 'campaign.log');
  const stdout = vi.spyOn(console, 'log').mockImplementation(() => {});
  const stderr = vi.spyOn(console, 'error').mockImplementation(() => {});
  const reporter = new CompactReporter({ log, report: join(cwd, 'tests.json') });
  reporter.onBegin();
  reporter.onStdOut('verbose output\n[campaign] compute 5/10\n');
  reporter.onStdErr(Buffer.from('server stderr\n'));
  const failure = 'FULL-ERROR-START' + 'x'.repeat(10000) + 'ERROR-END';
  reporter.onTestEnd({ expectedStatus: 'passed' } as TestCase, { status: 'failed', errors: [{ message: failure }] } as TestResult);
  reporter.onEnd({ status: 'failed', duration: 10, startTime: new Date() });
  expect(await readFile(log, 'utf8')).toContain(failure);
  expect(await readFile(log, 'utf8')).toContain('server stderr');
  expect(stdout.mock.calls.flat().join('\n')).toContain('[campaign] compute 5/10');
  expect(stdout.mock.calls.flat().join('\n')).not.toContain('verbose output');
  expect(stderr.mock.calls.flat().join('\n')).toContain('ERROR-END');
  expect(stderr.mock.calls.flat().join('\n').length).toBeLessThan(4000);
  const listing = new CompactReporter({ log: join(cwd, 'list.log'), report: join(cwd, 'list.json') });
  listing.onBegin();
  listing.onEnd({ status: 'passed', duration: 0, startTime: new Date() });
  expect(stdout.mock.lastCall?.[0]).toContain('Resource campaign not executed');
});
