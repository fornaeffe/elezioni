import { expect, test } from '@playwright/test';
import { execFile } from 'node:child_process';
import { promisify } from 'node:util';
import { mkdir, readFile, writeFile } from 'node:fs/promises';
import { resolve } from 'node:path';
import { existsSync } from 'node:fs';
import authored from '../../../data/politics-default-scenario.json' with { type: 'json' };
import { politicsDraftKey } from '../../src/lib/politics/files/draft';
import { politicsSessionsKey } from '../../src/lib/politics/workspace-controller';
import { REFORM, type PoliticsScenario, type LawId, type GeneratorId } from '../../src/lib/politics/scenario/schema';
import { requirePair } from '../../src/lib/politics/scenario/registry';
import { archiveDigest } from '../fixtures/archive-digest';
import { openResultTransfers, selectedCommon } from '../fixtures/common-page';
import { assessCampaignLegalOutcomes, type CampaignLegalError } from '../../scripts/acceptance/common-politics-outcomes';

const execute = promisify(execFile), base = process.env.BASE_PATH ?? '';
test.afterEach(async ({}, info) => {
  if (info.status === 'passed') return;
  const law = process.env.CP_RESOURCE_LAW ?? REFORM, model = process.env.CP_RESOURCE_MODEL ?? 'M02';
  const receipt = resolve(process.env.CP_RESOURCE_OUTPUT ?? '../data/derived/common-politics-resources',
    `${model}-${law}-${process.env.CP_RESOURCE_SIMULATIONS ?? '1000'}-${process.env.CP_RESOURCE_WORKERS ?? 'auto'}.json`);
  const report = JSON.parse(await readFile(receipt, 'utf8').catch(() => '{}'));
  await mkdir(resolve(receipt, '..'), { recursive: true });
  await writeFile(receipt, JSON.stringify({ ...report, state: 'failed', failure: {
    category: report.outcomes?.partial || report.outcomes?.failed ? 'legal-or-non-computable' : 'infrastructure-or-acceptance',
    status: info.status, errors: info.errors.map(e => e.message) } }, null, 2) + '\n');
});
test('national common execution resources with renderer-wide process sampling', async ({ page, browser }) => {
  if (process.platform !== 'win32') throw new Error('This process-memory receipt requires Windows');
  const simulations = Number(process.env.CP_RESOURCE_SIMULATIONS ?? 1000), initial = authored as unknown as PoliticsScenario;
  if (!Number.isSafeInteger(simulations) || simulations < 1 || simulations > 1000) throw new Error('Campaign size must be from 1 to 1000');
  const law = (process.env.CP_RESOURCE_LAW ?? REFORM) as LawId, model = (process.env.CP_RESOURCE_MODEL ?? 'M02') as GeneratorId;
  requirePair(law, model);
  const scenario: PoliticsScenario = { ...initial, selection: { ...initial.selection, scrutiny: { ...initial.selection.scrutiny, id: law },
    generatorsByLaw: { ...initial.selection.generatorsByLaw, [law]: { id: model, version: model === 'M02' ? '3.0.0' : '2.0.0' } } },
    execution: { ...initial.execution, simulations, workers: process.env.CP_RESOURCE_WORKERS
      ? { kind: 'fixed', count: Number(process.env.CP_RESOURCE_WORKERS) } : initial.execution.workers } };
  const directory = resolve(process.env.CP_RESOURCE_OUTPUT ?? '../data/derived/common-politics-resources'); await mkdir(directory, { recursive: true });
  const receipt = resolve(directory, `${model}-${law}-${simulations}-${process.env.CP_RESOURCE_WORKERS ?? 'auto'}.json`);
  for (const pin of [scenario.data.historical, scenario.data.catalogue, scenario.data.electorate,
    scenario.data.referenceGeography, ...Object.values(scenario.data.lawAssets).flatMap(a => [a.geography, a.seats, ...a.models.M01, ...a.models.M02])])
    if (!existsSync(resolve('static', pin.uri.replace(/^\//, '')))) throw new Error(`Missing production resource: ${pin.uri}`);
  const root = await browser.newBrowserCDPSession(), samples: object[] = [], started = performance.now();
  let crash = false; const pageErrors: string[] = [];
  page.on('crash', () => { crash = true; }); page.on('pageerror', e => pageErrors.push(e.message));
  const sample = async (phase: string) => {
    const pids = (await root.send('SystemInfo.getProcessInfo')).processInfo.filter(p => p.type === 'renderer').map(p => p.id);
    if (!pids.length || !pids.every(Number.isSafeInteger)) throw new Error('Missing renderer process IDs');
    const output = await execute('pwsh.exe', ['-NoProfile', '-NonInteractive', '-Command',
      `$p=@(Get-Process -Id ${pids.join(',')} -ErrorAction Stop); @{privateBytes=($p|Measure-Object PrivateMemorySize64 -Sum).Sum;workingSetBytes=($p|Measure-Object WorkingSet64 -Sum).Sum}|ConvertTo-Json -Compress`],
    { windowsHide: true, timeout: 5000 });
    const row = { seconds: (performance.now() - started) / 1000, phase, ...JSON.parse(output.stdout) };
    samples.push(row); return row;
  };
  await page.goto(base + '/');
  await page.evaluate(({ key, scenario }) => localStorage.setItem(key, JSON.stringify(scenario)), { key: politicsDraftKey, scenario });
  const hints = await page.evaluate(() => ({ hardwareConcurrency: navigator.hardwareConcurrency, deviceMemory: (navigator as Navigator & { deviceMemory?: number }).deviceMemory }));
  await page.goto(base + '/politics');
  const preparationStarted = performance.now();
  for (;;) {
    const status = await page.locator('aside[aria-label="Stato dell\'applicazione"]').innerText();
    await sample(status);
    if (status.includes('Modello pronto')) break;
    if (crash || pageErrors.length || performance.now() - preparationStarted > 180_000) throw new Error(`Preparation failed: ${status}; ${pageErrors.join(';')}`);
    await page.waitForTimeout(5000);
  }
  const preparationMs = performance.now() - preparationStarted;
  await page.getByRole('button', { name: 'Avvia simulazione', exact: true }).click();
  const statusBox = page.locator('aside[aria-label="Stato dell\'applicazione"]');
  await statusBox.getByRole('button', { name: /^Stato:/ }).click();
  let lastLog = 0;
  for (;;) {
    if (crash) throw new Error('Renderer crashed during the national campaign');
    const phase = await statusBox.innerText();
    if (await statusBox.evaluate(node => node.classList.contains('error')) || pageErrors.length)
      throw new Error(`National campaign failed: ${phase}; ${pageErrors.join(';')}`);
    const row = await sample(phase);
    if (performance.now() - lastLog >= 30_000) {
      console.log(`${phase}; private ${(row.privateBytes / 1024 ** 3).toFixed(2)} GiB`); lastLog = performance.now();
      await writeFile(receipt, JSON.stringify({ schemaVersion: 1, state: 'running', browser: browser.version(), hints,
        law, model, simulations, requestedWorkers: scenario.execution.workers,
        identity: { selection: scenario.selection, data: scenario.data, execution: scenario.execution }, samples, pageErrors }, null, 2) + '\n');
    }
    if (/Esecuzione completata|Esecuzione interrotta/.test(phase)) break;
    await page.waitForTimeout(5000);
  }
  const stored = await page.evaluate(async key => {
    const index = JSON.parse(localStorage.getItem(key)!);
    const db = await new Promise<IDBDatabase>((resolve, reject) => { const r = indexedDB.open('elezioni-result-chunks-v1'); r.onsuccess = () => resolve(r.result); r.onerror = () => reject(r.error); });
    const read = <T>(r: IDBRequest<T>) => new Promise<T>((resolve, reject) => { r.onsuccess = () => resolve(r.result); r.onerror = () => reject(r.error); });
    try {
      const record = await read(db.transaction('results').objectStore('results').get(index.selected));
      // Preserve exact legal reasons before the isolated browser context closes.
      // Decode only one bounded warning chunk at a time, never the full archive.
      const legalErrors: CampaignLegalError[] = [];
      for (let sequence = 0; sequence < (record.warningChunks ?? 0); sequence++) {
        const chunk = await read(db.transaction('chunks').objectStore('chunks').get([record.id, -1, sequence]));
        const rows = JSON.parse(await new Response(new Blob([chunk.bytes]).stream().pipeThrough(new DecompressionStream('gzip'))).text());
        for (const row of rows) if (row[3] === 'error') legalErrors.push({code:row[0],simulationId:row[2],message:row[4],chamber:row[7],scope:row[11],reason:row[13]});
      }
      return { result: record.result, execution: record.politics.run.execution, session: record.politics.run.session,
        legalErrors,
        manifestJsonBytes: new TextEncoder().encode(JSON.stringify(record)).byteLength, fittedState: record.fittedState,
        checkpointInline: record.politics.checkpoint !== null, checkpointRecords: await read(db.transaction('checkpoints').objectStore('checkpoints').count()) };
    } finally { db.close(); }
  }, politicsSessionsKey);
  const executionFinished = performance.now();
  const privatePeaks = samples as { privateBytes: number; workingSetBytes: number }[];
  const outcomes = { complete: 0, partial: 0, failed: 0 };
  for (const simulation of stored.session.simulations as { outcome: keyof typeof outcomes }[]) outcomes[simulation.outcome]++;
  const infrastructureFailures = stored.result.warnings.filter((w: { code: string; severity: string }) => w.severity === 'error' && w.code.startsWith('RESULT_'));
  const legalAcceptance = assessCampaignLegalOutcomes(law, stored.session.simulations, stored.legalErrors);
  const report = { schemaVersion: 1, state: 'finished', browser: browser.version(), hints, law, model, simulations, requestedWorkers: scenario.execution.workers,
    preparationMs, totalMs: executionFinished - started, peakPrivateBytes: Math.max(...privatePeaks.map(s => s.privateBytes)),
    peakWorkingSetBytes: Math.max(...privatePeaks.map(s => s.workingSetBytes)), outcomes, legalAcceptance, infrastructureFailures, samples, stored, pageErrors,
    qualification: 'Sampled Windows private renderer processes including nested workers and native allocations; not JS heap, available RAM or a guaranteed maximum.' };
  await writeFile(receipt, JSON.stringify(report, null, 2) + '\n');
  const archiveStarted = performance.now(), archiveSamples = samples.length;
  await openResultTransfers(page);
  let pendingSample: Promise<void> | undefined, samplingError: unknown;
  const timer = setInterval(() => {
    if (pendingSample) return;
    pendingSample = sample('archive export/import').then(() => {}, error => { samplingError = error; }).finally(() => { pendingSample = undefined; });
  }, 5000);
  try {
    const archive = receipt.replace(/\.json$/, '.archive.json.gz'), roundtrip = archive + '.roundtrip.gz';
    const download = page.waitForEvent('download', { timeout: 20 * 60 * 1000 });
    await page.getByRole('button', { name: 'Scarica archivio risultati JSON.gz', exact: true }).click();
    await (await download).saveAs(archive);
    await page.getByLabel('Importa risultati', { exact: true }).setInputFiles(archive);
    await openResultTransfers(page);
    await expect(page.getByRole('combobox', { name: 'Sessione dei risultati', exact: true })).not.toHaveValue(stored.session.id, { timeout: 20 * 60 * 1000 });
    const imported = await selectedCommon(page);
    expect(imported.politics.run.execution).toEqual(stored.execution);
    expect(imported.politics.run.session).toEqual(stored.session);
    const reexport = page.waitForEvent('download', { timeout: 20 * 60 * 1000 });
    await page.getByRole('button', { name: 'Scarica archivio risultati JSON.gz', exact: true }).click();
    await (await reexport).saveAs(roundtrip);
    const content = await archiveDigest(archive);
    expect(await archiveDigest(roundtrip)).toEqual(content);
    await sample('archive verified');
    const memory = samples.slice(archiveSamples) as { privateBytes: number; workingSetBytes: number }[];
    Object.assign(report, { archive: { path: archive, roundtrip, content, elapsedMs: performance.now() - archiveStarted,
      peakPrivateBytes: Math.max(...memory.map(s => s.privateBytes)), peakWorkingSetBytes: Math.max(...memory.map(s => s.workingSetBytes)) } });
    await writeFile(receipt, JSON.stringify(report, null, 2) + '\n');
  } finally { clearInterval(timer); await pendingSample; await root.detach(); }
  expect(samplingError).toBeUndefined();
  expect(stored.result.status).toBe('completed'); expect(stored.result.completedSimulations).toBe(simulations);
  expect(stored.session.state).toBe('complete'); expect(stored.session.simulations.map((s: { id: number }) => s.id)).toEqual(Array.from({ length: simulations }, (_, i) => i + 1));
  expect(stored.checkpointInline).toBe(false); expect(pageErrors).toEqual([]);
  expect(legalAcceptance.accepted, 'Preserve approved scrutiny errors; reject changed or unexplained outcomes').toBe(true);
  expect(infrastructureFailures).toEqual([]);
  if (model === 'M02') { expect(stored.fittedState).toBe('politics-checkpoint'); expect(stored.checkpointRecords).toBe(1); }
  expect(report.totalMs).toBeLessThan(60 * 60 * 1000);
  report.state = 'passed'; await writeFile(receipt, JSON.stringify(report, null, 2) + '\n');
});
