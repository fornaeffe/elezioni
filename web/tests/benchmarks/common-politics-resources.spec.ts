import { expect, test } from '@playwright/test';
import { execFile } from 'node:child_process';
import { promisify } from 'node:util';
import { mkdir, writeFile } from 'node:fs/promises';
import { resolve } from 'node:path';
import { existsSync } from 'node:fs';
import authored from '../../../data/politics-default-scenario.json' with { type: 'json' };
import { politicsDraftKey } from '../../src/lib/politics/files/draft';
import { politicsSessionsKey } from '../../src/lib/politics/workspace-controller';
import { REFORM, type PoliticsScenario, type LawId, type GeneratorId } from '../../src/lib/politics/scenario/schema';
import { requirePair } from '../../src/lib/politics/scenario/registry';

const execute = promisify(execFile), base = process.env.BASE_PATH ?? '';
test('national common execution resources with renderer-wide process sampling', async ({ page, browser }) => {
  if (process.platform !== 'win32') throw new Error('This process-memory receipt requires Windows');
  const simulations = Number(process.env.CP_RESOURCE_SIMULATIONS ?? 1000), initial = authored as unknown as PoliticsScenario;
  if (!Number.isSafeInteger(simulations) || simulations < 1 || simulations > 1000) throw new Error('Campaign size must be from 1 to 1000');
  const law = (process.env.CP_RESOURCE_LAW ?? REFORM) as LawId, model = (process.env.CP_RESOURCE_MODEL ?? 'M02') as GeneratorId;
  requirePair(law, model);
  const scenario: PoliticsScenario = { ...initial, selection: { ...initial.selection, scrutiny: { ...initial.selection.scrutiny, id: law },
    generatorsByLaw: { ...initial.selection.generatorsByLaw, [law]: { id: model, version: '2.0.0' } } },
    execution: { ...initial.execution, simulations, workers: process.env.CP_RESOURCE_WORKERS
      ? { kind: 'fixed', count: Number(process.env.CP_RESOURCE_WORKERS) } : initial.execution.workers } };
  const directory = resolve('../data/derived/common-politics-resources'); await mkdir(directory, { recursive: true });
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
    const status = await page.locator('.editor-status summary').innerText();
    await sample(status);
    if (status.includes('Modello pronto')) break;
    if (crash || pageErrors.length || performance.now() - preparationStarted > 180_000) throw new Error(`Preparation failed: ${status}; ${pageErrors.join(';')}`);
    await page.waitForTimeout(5000);
  }
  const preparationMs = performance.now() - preparationStarted;
  await page.getByRole('button', { name: 'Avvia simulazione', exact: true }).click();
  let lastLog = 0;
  for (;;) {
    if (crash) throw new Error('Renderer crashed during the national campaign');
    const phase = await page.locator('section[aria-labelledby="run-title"] [role=status]').innerText();
    const row = await sample(phase);
    if (performance.now() - lastLog >= 30_000) {
      console.log(`${phase}; private ${(row.privateBytes / 1024 ** 3).toFixed(2)} GiB`); lastLog = performance.now();
      await writeFile(receipt, JSON.stringify({ schemaVersion: 1, state: 'running', browser: browser.version(), hints,
        law, model, simulations, requestedWorkers: scenario.execution.workers, samples, pageErrors }, null, 2) + '\n');
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
      return { result: record.result, execution: record.politics.run.execution, session: record.politics.run.session,
        manifestJsonBytes: new TextEncoder().encode(JSON.stringify(record)).byteLength, fittedState: record.fittedState,
        checkpointInline: record.politics.checkpoint !== null, checkpointRecords: await read(db.transaction('checkpoints').objectStore('checkpoints').count()) };
    } finally { db.close(); }
  }, politicsSessionsKey);
  await root.detach();
  const privatePeaks = samples as { privateBytes: number; workingSetBytes: number }[];
  const report = { schemaVersion: 1, browser: browser.version(), hints, law, model, simulations, requestedWorkers: scenario.execution.workers,
    preparationMs, totalMs: performance.now() - started, peakPrivateBytes: Math.max(...privatePeaks.map(s => s.privateBytes)),
    peakWorkingSetBytes: Math.max(...privatePeaks.map(s => s.workingSetBytes)), samples, stored, pageErrors,
    qualification: 'Sampled Windows private renderer processes including nested workers and native allocations; not JS heap, available RAM or a guaranteed maximum.' };
  await writeFile(receipt, JSON.stringify(report, null, 2) + '\n');
  expect(stored.result.status).toBe('completed'); expect(stored.result.completedSimulations).toBe(simulations);
  expect(stored.session.state).toBe('complete'); expect(stored.session.simulations.map((s: { id: number }) => s.id)).toEqual(Array.from({ length: simulations }, (_, i) => i + 1));
  expect(stored.checkpointInline).toBe(false); expect(pageErrors).toEqual([]);
  if (model === 'M02') { expect(stored.fittedState).toBe('politics-checkpoint'); expect(stored.checkpointRecords).toBe(1); }
  expect(report.totalMs).toBeLessThan(60 * 60 * 1000);
});
