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
const assets = new Map<string, string | Uint8Array>();
const resourceHints = `Object.defineProperty(navigator, 'hardwareConcurrency', { value: 8 }); Object.defineProperty(navigator, 'deviceMemory', { value: 16 });\n`;
const fixture = generationFixture();
const raw = new TextEncoder().encode(JSON.stringify(fixture.snapshot)), compressed = gzipSync(raw);
const hash = (bytes: Uint8Array) => createHash('sha256').update(bytes).digest('hex');
const descriptor = { path: 'data/reform-test.json.gz', bytes: compressed.length, sha256: hash(compressed), uncompressedSha256: hash(raw) };
test.beforeAll(async () => {
  const catalogPlugin = () => ({ name: 'constructed-reform-catalog', enforce: 'pre' as const, load(id: string) {
    if (id.replaceAll('\\', '/').endsWith('/generation-catalog.generated.json')) return JSON.stringify(descriptor);
  } });
  const bundled = await build({ configFile: false, logLevel: 'error',
    resolve: { alias: { $lib: fileURLToPath(new URL('../../src/lib', import.meta.url)) } },
    plugins: [catalogPlugin()], worker: { plugins: () => [catalogPlugin()] },
    build: { write: false, minify: false, lib: { entry: fileURLToPath(new URL('../../src/lib/workers/simulation.worker.ts', import.meta.url)), formats: ['es'], fileName: 'production-worker' } }
  });
  for (const output of Array.isArray(bundled) ? bundled : [bundled]) if ('output' in output) {
    for (const chunk of output.output) {
      if (chunk.type === 'chunk' && chunk.isEntry) code = chunk.code;
      else if (chunk.type === 'asset') assets.set(chunk.fileName, chunk.source);
    }
  }
  expect(code.length).toBeGreaterThan(0);
});

test.beforeEach(async ({ context }) => {
  // Resource hints on WorkerNavigator are independent of window test overrides.
  await context.route('**/constructed-production-worker.js', route => route.fulfill({ contentType: 'text/javascript', body: resourceHints + code }));
  await context.route('**/assets/reform-compute.worker-*.js', route => {
    const name = new URL(route.request().url()).pathname.replace(/^\//, ''), source = assets.get(name);
    if (!source) return route.abort();
    return route.fulfill({ contentType: 'text/javascript', body: typeof source === 'string' ? source : Buffer.from(source) });
  });
});

test('cancellation and a closed worker retain published runs without rerolling or replacing the denominator', async ({ page, context, browser }) => {
  await context.route('**/elezioni/data/reform-test.json.gz', route => route.fulfill({ contentType: 'application/gzip', body: Buffer.from(compressed) }));
  await page.goto(`${process.env.BASE_PATH ?? ''}/`);
  const result = await page.evaluate(async scenario => {
    let worker = new Worker('/constructed-production-worker.js', { type: 'module' });
    const owner = 'cancellation-test', firstId = crypto.randomUUID(), closedId = crypto.randomUUID();
    const request = (id: string) => ({ kind: 'politiche', resultId: id, resultOwner: owner, basePath: '/elezioni', scenario,
      scrutinyAlgorithmId: scenario.algorithmId, electionDate: scenario.electionDate, seed: scenario.seed, dataVersion: scenario.snapshotVersion, simulations: 100, computeWorkers: 3 });
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
  const cdp = await browser.newBrowserCDPSession();
  try {
    await cdp.send('Target.setDiscoverTargets', { discover: true });
    await expect.poll(async () => (await cdp.send('Target.getTargets')).targetInfos
      .filter(target => target.type === 'worker' && target.url.includes('reform-compute.worker-')).length).toBe(0);
  } finally { await cdp.detach(); }
});

test('actual worker preserves paired global IDs over the 50-run chunk boundary and replays without production fixtures', async ({ page, context }) => {
  test.setTimeout(120000);
  await context.route('**/elezioni/data/reform-test.json.gz', route => route.fulfill({ contentType: 'application/gzip', body: Buffer.from(compressed) }));
  await page.goto(`${process.env.BASE_PATH ?? ''}/`);
  const response = await page.evaluate(async scenario => {
    const worker = new Worker('/constructed-production-worker.js', { type: 'module' });
    const execute = (simulations: number, computeWorkers = 1) => new Promise<{ result: SimulationResult; progress: SimulationWorkerMessage[] }>((resolve, reject) => {
      const progress: SimulationWorkerMessage[] = [];
      worker.onmessage = e => { if (e.data.type === 'result') resolve({ result: e.data, progress }); else progress.push(e.data); };
      worker.onerror = e => reject(new Error(e.message));
      worker.postMessage({ kind: 'politiche', basePath: '/elezioni', scenario, scrutinyAlgorithmId: scenario.algorithmId,
        electionDate: scenario.electionDate, seed: scenario.seed, dataVersion: scenario.snapshotVersion, simulations, computeWorkers });
    });
    const query = (id: string, table: number, simulation?: number, offset = 0, limit = 1000) => new Promise<import('../../src/lib/core/types').ResultTable>((resolve, reject) => {
      worker.onmessage = e => { if (e.data.type === 'result-service') { if (e.data.ok) resolve(e.data.data); else reject(new Error(e.data.error.message)); } };
      worker.postMessage({ type: 'result-service', requestId: 1, action: 'page', owner: 'direct-worker', resultId: id, table, simulation, offset, limit });
    });
    try {
      const first = await execute(51, 4), replay = await execute(1), invalid = await execute(0), invalidWorkers = await execute(1, 6);
      const index = (name: string) => first.result.tables.findIndex(t => t.name === name);
      const firstId = first.result.storage!.id, replayId = replay.result.storage!.id;
      const runs = await query(firstId, index('Reform runs'));
      const entitlements = [];
      for (const sim of [1, 50, 51]) entitlements.push(...(await query(firstId, index('Vote-based entitlements'), sim)).rows);
      const firstLedger = await query(firstId, index('Seat entitlement and election locations'), 1);
      const replayLedger = await query(replayId, index('Seat entitlement and election locations'), 1);
      const diagnostics = await new Promise<any>((resolve, reject) => {
        worker.onmessage = event => event.data.ok ? resolve(event.data.data) : reject(new Error(event.data.error.message));
        worker.postMessage({ type: 'result-service', requestId: 2, action: 'diagnostics', owner: 'direct-worker', resultId: firstId });
      });
      return { first, replay, invalid, invalidWorkers, runs, entitlements, firstLedger, replayLedger, diagnostics };
    }
    finally { worker.terminate(); }
  }, fixture.scenario);
  expect(response.first.result.status).toBe('completed');
  expect(response.runs.rows.map(r => r.Simulation)).toEqual(Array.from({ length: 51 }, (_, i) => i + 1));
  for (const sim of [1, 50, 51]) expect(response.entitlements.filter(r => r.Simulation === sim).map(r => r.Chamber)).toEqual(['camera', 'senato']);
  expect(response.firstLedger.rows).toEqual(response.replayLedger.rows);
  expect(response.diagnostics.requested).toBe(51);
  expect(response.diagnostics.processed).toBe(51);
  expect(response.diagnostics.warnings.reasons.length).toBeGreaterThan(0);
  expect(response.diagnostics.warnings.reasons.reduce((sum: number, r: { occurrences: number }) => sum + r.occurrences, 0)).toBe(response.diagnostics.warnings.occurrences);
  expect(response.first.progress).toContainEqual(expect.objectContaining({ phase: 'scrutinize', completed: 51, total: 51 }));
  expect(response.invalid.result.warnings).toContainEqual(expect.objectContaining({ code: 'WORKER_ERROR', severity: 'error', message: expect.stringContaining('positive safe integer') }));
  expect(response.invalidWorkers.result.warnings).toContainEqual(expect.objectContaining({ code: 'WORKER_ERROR', severity: 'error', message: expect.stringContaining('1 to 5') }));
});

test('1/2/4 compute workers preserve all exported rows, warnings and shared occurrence counts', async ({ page, context }) => {
  test.setTimeout(120000);
  await context.route('**/elezioni/data/reform-test.json.gz', route => route.fulfill({ contentType: 'application/gzip', body: Buffer.from(compressed) }));
  await page.goto(`${process.env.BASE_PATH ?? ''}/`);
  const report = await page.evaluate(async scenario => {
    const worker = new Worker('/constructed-production-worker.js', { type: 'module' });
    try {
      const hashes: string[] = [], executions: unknown[] = [];
      for (const computeWorkers of [1, 2, 4]) {
        const result = await new Promise<SimulationResult>((resolve, reject) => {
          worker.onmessage = event => { if (event.data.type === 'result') resolve(event.data); };
          worker.onerror = event => reject(new Error(event.message));
          worker.postMessage({ kind: 'politiche', basePath: '/elezioni', scenario, scrutinyAlgorithmId: scenario.algorithmId,
            electionDate: scenario.electionDate, seed: scenario.seed, dataVersion: scenario.snapshotVersion, simulations: 7, computeWorkers });
        });
        if (result.status !== 'completed') throw new Error(JSON.stringify(result.warnings));
        executions.push(result.benchmark.execution);
        const file = await new Promise<File>((resolve, reject) => {
          worker.onmessage = event => event.data.ok ? resolve(event.data.data) : reject(new Error(event.data.error.message));
          worker.postMessage({ type: 'result-service', requestId: computeWorkers, action: 'export', resultId: result.storage!.id, owner: 'direct-worker', format: 'json' });
        });
        const payload = await new Response(file.stream().pipeThrough(new DecompressionStream('gzip'))).json();
        if (computeWorkers === 1) {
          // Versioned events prove exact membership after import. The legacy
          // ledger alone may only retain the first affected simulation.
          const resultId = crypto.randomUUID();
          const service = (action: string, fields = {}) => new Promise<any>(resolve => {
            worker.onmessage = event => resolve(event.data);
            worker.postMessage({ type: 'result-service', requestId: 99, action, resultId, owner: 'direct-worker', ...fields });
          });
          const imported = await service('import', { file });
          if (!imported.ok) throw new Error(imported.error.message);
          const diagnostics = await service('diagnostics');
          const native = await service('diagnostics', { resultId: result.storage!.id });
          if (!diagnostics.ok || !native.ok || JSON.stringify(diagnostics.data) !== JSON.stringify(native.data)) throw new Error('Versioned import lost diagnostic membership');
          await service('delete');
          const legacyId = crypto.randomUUID();
          const legacy = structuredClone(payload);
          legacy.result.tables = legacy.result.tables.filter((table: { name: string }) => table.name !== 'Diagnostic events v1');
          const oldImport = await service('import', { resultId: legacyId, file: new File([JSON.stringify(legacy)], 'legacy.json') });
          if (!oldImport.ok) throw new Error(oldImport.error.message);
          const unknown = await service('diagnostics', { resultId: legacyId });
          if (unknown.ok || !unknown.error.message.includes('Per-simulation diagnostic ledger')) throw new Error('Imported aggregation invented diagnostic frequencies');
          await service('delete', { resultId: legacyId });
        }
        payload.exportedAt = 'fixed';
        delete payload.result.benchmark.startedAt; delete payload.result.benchmark.elapsedMs; delete payload.result.benchmark.execution;
        const digest = await crypto.subtle.digest('SHA-256', new TextEncoder().encode(JSON.stringify(payload)));
        hashes.push([...new Uint8Array(digest)].map(n => n.toString(16).padStart(2, '0')).join(''));
      }
      return { hashes, executions };
    } finally { worker.terminate(); }
  }, fixture.scenario);
  expect(new Set(report.hashes).size).toBe(1);
  expect(report.executions).toEqual([1, 2, 4].map(count => expect.objectContaining({ requestedWorkers: count, computeWorkers: count, maximumWorkers: 5 })));
});

test('native compute worker crashes interrupt the session rather than becoming legal run failures', async ({ page, context }) => {
  await context.route('**/assets/reform-compute.worker-*.js', route => {
    const source = assets.get(new URL(route.request().url()).pathname.replace(/^\//, ''))!;
    const text = typeof source === 'string' ? source : new TextDecoder().decode(source);
    const crash = `\nconst originalComputeHandler = self.onmessage; self.onmessage = event => { if (event.data.action === 'compute' && event.data.simulation === 2) throw new Error('Injected compute crash'); return originalComputeHandler(event); };`;
    return route.fulfill({ contentType: 'text/javascript', body: text + crash });
  });
  await context.route('**/elezioni/data/reform-test.json.gz', route => route.fulfill({ contentType: 'application/gzip', body: Buffer.from(compressed) }));
  await page.goto(`${process.env.BASE_PATH ?? ''}/`);
  const result = await page.evaluate(scenario => new Promise<SimulationResult>((resolve, reject) => {
    const worker = new Worker('/constructed-production-worker.js', { type: 'module' });
    worker.onmessage = event => { if (event.data.type === 'result') { worker.terminate(); resolve(event.data); } };
    worker.onerror = event => { worker.terminate(); reject(new Error(event.message)); };
    worker.postMessage({ kind: 'politiche', basePath: '/elezioni', scenario, scrutinyAlgorithmId: scenario.algorithmId,
      electionDate: scenario.electionDate, seed: scenario.seed, dataVersion: scenario.snapshotVersion, simulations: 10, computeWorkers: 2 });
  }), fixture.scenario);
  expect(result.storage!.state).toBe('interrupted');
  expect(result.storage!.completedSimulations).toBe(1);
  expect(result.benchmark.simulations).toBe(10);
  expect(result.warnings).toContainEqual(expect.objectContaining({ code: 'RESULT_EXECUTION_ERROR', severity: 'error', message: expect.stringContaining('Injected compute crash') }));
  expect(result.warnings.some(w => w.code === 'R26_RUN_ERROR')).toBe(false);
});

test('a quota failure during a parallel run hides unpublished chunks and preserves the durable prefix', async ({ page, context }) => {
  const injection = `\nconst originalChunkAdd = IDBObjectStore.prototype.add; let secondRunChunks = 0;
    IDBObjectStore.prototype.add = function(value, key) {
      if (this.name === 'chunks' && value.simulation === 2 && ++secondRunChunks === 2) throw new DOMException('Injected parallel quota failure', 'QuotaExceededError');
      return key === undefined ? originalChunkAdd.call(this, value) : originalChunkAdd.call(this, value, key);
    };`;
  await context.route('**/constructed-production-worker.js', route => route.fulfill({ contentType: 'text/javascript', body: resourceHints + code + injection }));
  await context.route('**/elezioni/data/reform-test.json.gz', route => route.fulfill({ contentType: 'application/gzip', body: Buffer.from(compressed) }));
  await page.goto(`${process.env.BASE_PATH ?? ''}/`);
  const report = await page.evaluate(async scenario => {
    const worker = new Worker('/constructed-production-worker.js', { type: 'module' });
    try {
      const result = await new Promise<SimulationResult>((resolve, reject) => {
        worker.onmessage = event => { if (event.data.type === 'result') resolve(event.data); };
        worker.onerror = event => reject(new Error(event.message));
        worker.postMessage({ kind: 'politiche', basePath: '/elezioni', scenario, scrutinyAlgorithmId: scenario.algorithmId,
          electionDate: scenario.electionDate, seed: scenario.seed, dataVersion: scenario.snapshotVersion, simulations: 10, computeWorkers: 2 });
      });
      const table = result.tables.findIndex(t => t.name === 'Reform runs');
      const runs = await new Promise<any>((resolve, reject) => {
        worker.onmessage = event => event.data.ok ? resolve(event.data.data) : reject(new Error(event.data.error.message));
        worker.postMessage({ type: 'result-service', requestId: 1, action: 'page', resultId: result.storage!.id, owner: 'direct-worker', table });
      });
      return { result, runs };
    } finally { worker.terminate(); }
  }, fixture.scenario);
  expect(report.result.storage!.state).toBe('interrupted');
  expect(report.result.storage!.completedSimulations).toBe(1);
  expect(report.runs.rows.map((row: { Simulation: number }) => row.Simulation)).toEqual([1]);
  expect(report.result.warnings).toContainEqual(expect.objectContaining({ code: 'RESULT_STORAGE_ERROR', message: expect.stringContaining('QuotaExceededError') }));
});
