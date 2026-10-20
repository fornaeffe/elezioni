import { existsSync, mkdirSync, readFileSync, readdirSync, writeFileSync } from 'node:fs';
import { resolve, dirname } from 'node:path';
import { cpus, totalmem } from 'node:os';
import { expect, test } from '@playwright/test';
import catalog from '../../src/lib/politics/reform-2026/data/generation-catalog.generated.json' with { type: 'json' };
import type { ReformGenerationScenario } from '../../src/lib/politics/reform-2026/generation-types';
import type { SimulationResult } from '../../src/lib/core/types';

const values = (name: string, defaults: number[]) => process.env[name]?.split(',').map(Number) ?? defaults;
const counts = values('REFORM_BENCHMARK_COUNTS', [10, 100, 1000]);
const sizes = values('REFORM_BENCHMARK_WORKERS', [1, 2, 4]);
const scenario: ReformGenerationScenario = JSON.parse(readFileSync(new URL(`../../src/lib/politics/reform-2026/data/${catalog.defaultsPath}`, import.meta.url), 'utf8'));

test('production reform pool: throughput, phase costs, sampled heaps and outcome invariance', async ({ browser, page, context }) => {
  test.setTimeout(60 * 60 * 1000);
  expect(existsSync(resolve('static', catalog.path)), 'Required production reform bundle; generate it before benchmarking').toBe(true);
  for (const value of [...counts, ...sizes]) expect(Number.isSafeInteger(value) && value >= 1).toBe(true);
  // Controlled resource hints permit testing four workers even when Chromium's
  // privacy-coarsened RAM hint is 8 GiB. This never changes application defaults.
  const hints = `Object.defineProperty(navigator, 'hardwareConcurrency', { value: 8 }); Object.defineProperty(navigator, 'deviceMemory', { value: 16 });\n`;
  await context.route('**/simulation.worker-*.js', async route => {
    const response = await route.fetch();
    await route.fulfill({ response, body: hints + await response.text() });
  });
  const base = process.env.BASE_PATH ?? '';
  await page.goto(`${base}/politics`);
  const workerName = readdirSync('build/_app/immutable/workers').find(name => name.startsWith('simulation.worker-'))!;
  const workerUrl = `${base}/_app/immutable/workers/${workerName}`;
  const cdp = await browser.newBrowserCDPSession();
  await cdp.send('Target.setDiscoverTargets', { discover: true });
  const sessions = new Map<string, { sessionId: string; role: string; maximumHeap: number }>();
  let rpc = 0;
  const replies = new Map<number, (value: any) => void>();
  cdp.on('Target.receivedMessageFromTarget', event => {
    const message = JSON.parse(event.message);
    if (message.id) { replies.get(message.id)?.(message.result); replies.delete(message.id); }
  });
  const workerCall = async (sessionId: string, method: string) => {
    const id = ++rpc;
    let timer: ReturnType<typeof setTimeout>;
    const response = new Promise<any>(resolve => {
      replies.set(id, value => { clearTimeout(timer); resolve(value); });
      timer = setTimeout(() => { replies.delete(id); resolve(null); }, 1000);
    });
    try { await cdp.send('Target.sendMessageToTarget', { sessionId, message: JSON.stringify({ id, method }) }); }
    catch { replies.delete(id); clearTimeout(timer!); return null; }
    return response;
  };
  const report = { created: new Date().toISOString(), browser: browser.version(), node: process.version,
    platform: process.platform, cpu: cpus()[0].model, physicalMemoryGiB: totalmem() / 1024 ** 3,
    algorithm: scenario.algorithmId, snapshot: catalog.snapshotVersion, model: catalog.modelVersion, seed: scenario.seed,
    resourceHints: { hardwareConcurrency: 8, deviceMemory: 16, controlledForBenchmark: true },
    notes: ['Heaps are periodic unforced JavaScript samples, not absolute peaks or process RAM.',
      'Phase totals accumulate performance.now() durations within workers; they overlap and are not measured process CPU time or end-to-end elapsed time.',
      'Presentation includes row derivation; storage includes transport decoding, compression and durable writes.',
      'Each measured run starts a new coordinator and compute pool; production load/verification/preparation is included in elapsed time.'],
    runs: [] as unknown[] };
  const output = resolve('../test/fixtures/benchmarks/browser_reform_workers.json');
  const save = () => { mkdirSync(dirname(output), { recursive: true }); writeFileSync(output, `${JSON.stringify(report, null, 2)}\n`); };
  const references = new Map<number, unknown>();
  try {
    for (const simulations of counts) for (const computeWorkers of sizes) {
      sessions.clear();
      let finished = false, failure: unknown;
      const running = page.evaluate(async ({ workerUrl, scenario, simulations, computeWorkers }) => {
        const worker = new Worker(workerUrl, { type: 'module' });
        const state = window as unknown as { reformBenchmarkProgress: number };
        state.reformBenchmarkProgress = 0;
        try {
          const result = await new Promise<SimulationResult>((resolve, reject) => {
            worker.onmessage = event => {
              if (event.data.type === 'progress') state.reformBenchmarkProgress = event.data.completed;
              if (event.data.type === 'result') resolve(event.data);
            };
            worker.onerror = event => reject(new Error(event.message));
            worker.postMessage({ kind: 'politiche', basePath: new URL(workerUrl, location.href).pathname.split('/_app/')[0], scenario,
              scrutinyAlgorithmId: scenario.algorithmId, seed: scenario.seed, electionDate: scenario.electionDate,
              dataVersion: scenario.snapshotVersion, simulations, computeWorkers });
          });
          // Delete measured data outside the worker's benchmark interval.
          if (result.storage) await new Promise<void>((resolve, reject) => {
            worker.onmessage = event => event.data.ok ? resolve() : reject(new Error(event.data.error.message));
            worker.postMessage({ type: 'result-service', requestId: 1, action: 'delete', resultId: result.storage!.id, owner: 'direct-worker' });
          });
          return result;
        } finally { worker.terminate(); }
      }, { workerUrl, scenario, simulations, computeWorkers });
      void running.then(() => { finished = true; }, error => { failure = error; finished = true; });
      let lastLog = 0, maximumCombinedHeap = 0, maximumActiveComputeWorkers = 0;
      while (!finished) {
        const { targetInfos } = await cdp.send('Target.getTargets');
        const targets = targetInfos.filter(t => t.type === 'worker' && /(?:simulation|reform-compute)\.worker-/.test(t.url));
        maximumActiveComputeWorkers = Math.max(maximumActiveComputeWorkers, targets.filter(t => t.url.includes('reform-compute')).length);
        let combined = 0;
        await Promise.all(targets.map(async target => {
          try {
            let session = sessions.get(target.targetId);
            if (!session) {
              const { sessionId } = await cdp.send('Target.attachToTarget', { targetId: target.targetId, flatten: false });
              session = { sessionId, role: target.url.includes('reform-compute') ? 'compute' : 'coordinator', maximumHeap: 0 };
              sessions.set(target.targetId, session);
            }
            const heap = await workerCall(session.sessionId, 'Runtime.getHeapUsage');
            if (heap?.usedSize) { session.maximumHeap = Math.max(session.maximumHeap, heap.usedSize); combined += heap.usedSize; }
          } catch { /* A completed pool can disappear between discovery and attach. */ }
        }));
        maximumCombinedHeap = Math.max(maximumCombinedHeap, combined);
        if (Date.now() - lastLog >= 15000) {
          const done = await page.evaluate(() => (window as unknown as { reformBenchmarkProgress: number }).reformBenchmarkProgress);
          console.log('REFORM_POOL_PROGRESS', JSON.stringify({ simulations, computeWorkers, done })); lastLog = Date.now();
        }
        if (!finished) await page.waitForTimeout(250);
      }
      if (failure) throw failure;
      const result = await running;
      expect(result.status, JSON.stringify(result.warnings)).toBe('completed');
      expect(result.storage!.completedSimulations).toBe(simulations);
      expect(result.benchmark.execution!.computeWorkers).toBe(Math.min(computeWorkers, simulations));
      const invariant = { tables: result.tables, rowCounts: result.storage!.tables.map(t => ({ name: t.name, rows: t.rows })), warningCounts: result.storage!.warningCounts };
      if (references.has(simulations)) expect(invariant).toEqual(references.get(simulations));
      else references.set(simulations, invariant);
      report.runs.push({ simulations, computeWorkers, elapsedMs: result.benchmark.elapsedMs, execution: result.benchmark.execution,
        maximumCombinedHeap, maximumActiveComputeWorkers,
        heaps: [...sessions.values()].map(({ role, maximumHeap }) => ({ role, maximumHeap })), overview: result.tables[0].rows[0] });
      save();
      console.log('REFORM_POOL_RESULT', JSON.stringify(report.runs.at(-1)));
    }
  } finally { save(); await cdp.detach(); }
});
