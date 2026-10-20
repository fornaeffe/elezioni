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
    try { const first = await execute(51), replay = await execute(1), invalid = await execute(0); return { first, replay, invalid }; }
    finally { worker.terminate(); }
  }, fixture.scenario);
  expect(response.first.result.status).toBe('completed');
  const table = (r: SimulationResult, name: string) => r.tables.find(t => t.name === name)!;
  expect(table(response.first.result, 'Reform runs').rows.map(r => r.Simulation)).toEqual(Array.from({ length: 51 }, (_, i) => i + 1));
  const entitlement = table(response.first.result, 'Vote-based entitlements');
  for (const sim of [1, 50, 51]) expect(entitlement.rows.filter(r => r.Simulation === sim).map(r => r.Chamber)).toEqual(['camera', 'senato']);
  expect(table(response.first.result, 'Seat entitlement and election locations').rows.filter(r => r.Simulation === 1)).toEqual(table(response.replay.result, 'Seat entitlement and election locations').rows);
  expect(response.first.progress).toContainEqual(expect.objectContaining({ phase: 'scrutinize', completed: 51, total: 51 }));
  expect(response.invalid.result.warnings).toContainEqual(expect.objectContaining({ code: 'WORKER_ERROR', severity: 'error', message: expect.stringContaining('positive safe integer') }));
});
