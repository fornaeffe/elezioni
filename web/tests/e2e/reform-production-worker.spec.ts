import { createHash } from 'node:crypto';
import { fileURLToPath } from 'node:url';
import { gzipSync } from 'fflate';
import { build } from 'vite';
import { expect, test } from '@playwright/test';
import { generationFixture } from '../../src/lib/politics/reform-2026/generation.fixture';
import type { SimulationResult, SimulationWorkerMessage } from '../../src/lib/core/types';

// Exercise the actual production worker with a test-only immutable catalog and
// constructed bundle, so paired execution remains covered in a clean checkout.
let code = '';
const fixture = generationFixture();
const raw = new TextEncoder().encode(JSON.stringify(fixture.snapshot)), compressed = gzipSync(raw);
const hash = (bytes: Uint8Array) => createHash('sha256').update(bytes).digest('hex');
const descriptor = { path: 'data/reform-test.json.gz', bytes: compressed.length, sha256: hash(compressed), uncompressedSha256: hash(raw) };
test.beforeAll(async () => {
  const bundled = await build({ configFile: false, logLevel: 'error',
    resolve: { alias: { $lib: fileURLToPath(new URL('../../src/lib', import.meta.url)) } },
    plugins: [{ name: 'constructed-reform-catalog', enforce: 'pre', load(id) {
      if (id.replaceAll('\\', '/').endsWith('/generation-catalog.generated.json')) return JSON.stringify(descriptor);
    } }],
    build: { write: false, minify: false, lib: { entry: fileURLToPath(new URL('../../src/lib/workers/simulation.worker.ts', import.meta.url)), formats: ['es'], fileName: 'production-worker' } }
  });
  for (const output of Array.isArray(bundled) ? bundled : [bundled]) if ('output' in output) {
    for (const chunk of output.output) if (chunk.type === 'chunk' && chunk.isEntry) code = chunk.code;
  }
  expect(code.length).toBeGreaterThan(0);
});

test('cancellation and a closed worker retain published runs without rerolling or replacing the denominator', async ({ page, context }) => {
  await context.route('**/constructed-production-worker.js', route => route.fulfill({ contentType: 'text/javascript', body: code }));
  await context.route('**/elezioni/data/reform-test.json.gz', route => route.fulfill({ contentType: 'application/gzip', body: Buffer.from(compressed) }));
  await page.goto(`${process.env.BASE_PATH ?? ''}/`);
  const result = await page.evaluate(async scenario => {
    let worker = new Worker('/constructed-production-worker.js', { type: 'module' });
    const owner = 'cancellation-test', firstId = crypto.randomUUID(), closedId = crypto.randomUUID();
    const request = (id: string) => ({ kind: 'politiche', resultId: id, resultOwner: owner, basePath: '/elezioni', scenario,
      scrutinyAlgorithmId: scenario.algorithmId, electionDate: scenario.electionDate, seed: scenario.seed, dataVersion: scenario.snapshotVersion, simulations: 100 });
    const cancelled = await new Promise<SimulationResult>((resolve, reject) => {
      let sent = false;
      worker.onmessage = event => {
        const m = event.data;
        if (m.type === 'progress' && m.phase === 'simulate' && m.completed >= 2 && !sent) {
          sent = true; worker.postMessage({ type: 'result-service', requestId: 1, action: 'cancel', resultId: firstId, owner });
        }
        if (m.type === 'result') resolve(m);
      };
      worker.onerror = event => reject(new Error(event.message)); worker.postMessage(request(firstId));
    });
    await new Promise<void>((resolve, reject) => {
      worker.onmessage = event => {
        const m = event.data;
        if (m.type === 'progress' && m.phase === 'simulate' && m.completed >= 3) { worker.terminate(); resolve(); }
      };
      worker.onerror = event => reject(new Error(event.message)); worker.postMessage(request(closedId));
    });
    worker = new Worker('/constructed-production-worker.js', { type: 'module' });
    try {
      const restored = await new Promise<any>((resolve, reject) => {
        worker.onmessage = event => event.data.ok ? resolve(event.data.data) : reject(new Error(event.data.error.message));
        worker.onerror = event => reject(new Error(event.message));
        worker.postMessage({ type: 'result-service', requestId: 2, action: 'manifest', resultId: closedId, owner });
      });
      return { cancelled, restored };
    } finally { worker.terminate(); }
  }, fixture.scenario);
  expect(result.cancelled.status).toBe('not_implemented');
  expect(result.cancelled.storage!.state).toBe('interrupted');
  expect(result.cancelled.storage!.completedSimulations).toBeGreaterThanOrEqual(1);
  expect(result.cancelled.storage!.completedSimulations).toBeLessThan(100);
  expect(result.cancelled.benchmark.simulations).toBe(100);
  expect(result.restored.result.storage.state).toBe('interrupted');
  expect(result.restored.result.storage.completedSimulations).toBeGreaterThanOrEqual(1);
  expect(result.restored.result.benchmark.simulations).toBe(100);
  expect(result.restored.result.warnings).toContainEqual(expect.objectContaining({ code: 'RESULT_INTERRUPTED' }));
});

test('actual worker preserves paired global IDs over the 50-run chunk boundary and replays without production fixtures', async ({ page, context }) => {
  test.setTimeout(120000);
  await context.route('**/constructed-production-worker.js', route => route.fulfill({ contentType: 'text/javascript', body: code }));
  await context.route('**/elezioni/data/reform-test.json.gz', route => route.fulfill({ contentType: 'application/gzip', body: Buffer.from(compressed) }));
  await page.goto(`${process.env.BASE_PATH ?? ''}/`);
  const response = await page.evaluate(async scenario => {
    const worker = new Worker('/constructed-production-worker.js', { type: 'module' });
    const execute = (simulations: number) => new Promise<{ result: SimulationResult; progress: SimulationWorkerMessage[] }>((resolve, reject) => {
      const progress: SimulationWorkerMessage[] = [];
      worker.onmessage = e => { if (e.data.type === 'result') resolve({ result: e.data, progress }); else progress.push(e.data); };
      worker.onerror = e => reject(new Error(e.message));
      worker.postMessage({ kind: 'politiche', basePath: '/elezioni', scenario, scrutinyAlgorithmId: scenario.algorithmId,
        electionDate: scenario.electionDate, seed: scenario.seed, dataVersion: scenario.snapshotVersion, simulations });
    });
    const query = (id: string, table: number, simulation?: number, offset = 0, limit = 1000) => new Promise<import('../../src/lib/core/types').ResultTable>((resolve, reject) => {
      worker.onmessage = e => { if (e.data.type === 'result-service') { if (e.data.ok) resolve(e.data.data); else reject(new Error(e.data.error.message)); } };
      worker.postMessage({ type: 'result-service', requestId: 1, action: 'page', owner: 'direct-worker', resultId: id, table, simulation, offset, limit });
    });
    try {
      const first = await execute(51), replay = await execute(1), invalid = await execute(0);
      const index = (name: string) => first.result.tables.findIndex(t => t.name === name);
      const firstId = first.result.storage!.id, replayId = replay.result.storage!.id;
      const runs = await query(firstId, index('Reform runs'));
      const entitlements = [];
      for (const sim of [1, 50, 51]) entitlements.push(...(await query(firstId, index('Vote-based entitlements'), sim)).rows);
      const firstLedger = await query(firstId, index('Seat entitlement and election locations'), 1);
      const replayLedger = await query(replayId, index('Seat entitlement and election locations'), 1);
      return { first, replay, invalid, runs, entitlements, firstLedger, replayLedger };
    }
    finally { worker.terminate(); }
  }, fixture.scenario);
  expect(response.first.result.status).toBe('completed');
  expect(response.runs.rows.map(r => r.Simulation)).toEqual(Array.from({ length: 51 }, (_, i) => i + 1));
  for (const sim of [1, 50, 51]) expect(response.entitlements.filter(r => r.Simulation === sim).map(r => r.Chamber)).toEqual(['camera', 'senato']);
  expect(response.firstLedger.rows).toEqual(response.replayLedger.rows);
  expect(response.first.progress).toContainEqual(expect.objectContaining({ phase: 'scrutinize', completed: 51, total: 51 }));
  expect(response.invalid.result.warnings).toContainEqual(expect.objectContaining({ code: 'WORKER_ERROR', severity: 'error', message: expect.stringContaining('positive safe integer') }));
});
