import { fileURLToPath } from 'node:url';
import { build } from 'vite';
import { expect, test } from '@playwright/test';
import { commonPoliticsFixture } from '../fixtures/common-politics-input';
import { preparePoliticsExecution } from '../../src/lib/politics/execution';
import { requiredAssets } from '../../src/lib/politics/scenario/dependencies';
import { ROSATELLUM, REFORM } from '../../src/lib/politics/scenario/schema';
import { computePoliticsRun } from '../../src/lib/workers/politics-compute';
import { implementationKey } from '../../scripts/data-preparation/fast-share-release-io';

let workerCode = '';
test.beforeAll(async () => {
  const built = await build({ configFile: false, logLevel: 'error',
    define: { __COMMON_POLITICS_IMPLEMENTATION__: JSON.stringify(await implementationKey(fileURLToPath(new URL('../../../', import.meta.url)), 'web/src/lib/workers/politics-compute.ts')) },
    resolve: { alias: { $lib: fileURLToPath(new URL('../../src/lib', import.meta.url)) } },
    build: { write: false, minify: false, lib: { entry: fileURLToPath(new URL('../../src/lib/workers/reform-compute.worker.ts', import.meta.url)), formats: ['es'], fileName: 'common-compute' } } });
  for (const output of Array.isArray(built) ? built : [built]) if ('output' in output)
    for (const chunk of output.output) if (chunk.type === 'chunk' && chunk.isEntry) workerCode = chunk.code;
  expect(workerCode.length).toBeGreaterThan(0);
});
for (const [law, model] of [[ROSATELLUM, 'M01'], [REFORM, 'M01'], [REFORM, 'M02']] as const)
  test(`common worker ${law}/${model} validates and streams reproducible runs`, async ({ page, context }) => {
    const f = await commonPoliticsFixture(law, model), prepared = await preparePoliticsExecution(f.scenario, f.readers);
    const packet = { implementationId: prepared.implementationId, scenario: f.scenario, keys: prepared.keys,
      assets: requiredAssets(f.scenario).map(a => ({ id: a.id, bytes: [...f.bytes.get(a.id)!] })),
      sourceChunks: f.original.source.payload.events.map(e => ({ path: e.chunk.path, bytes: [...f.original.files.get(e.chunk.path)!] })) };
    await context.route('**/common-politics-compute.js', route => route.fulfill({ contentType: 'text/javascript', body: workerCode }));
    await page.goto(`${process.env.BASE_PATH ?? ''}/`);
    const runs = await page.evaluate(async packet => {
      const worker = new Worker(`${location.pathname}common-politics-compute.js`, { type: 'module' });
      let requestId = 0;
      const call = (request: Record<string, unknown>) => new Promise<unknown>((resolve, reject) => {
        worker.onmessage = event => event.data.ok ? resolve(event.data.data) : reject(new Error(event.data.error));
        worker.onerror = event => reject(new Error(event.message));
        worker.postMessage({ ...request, requestId: ++requestId });
      });
      try {
        await call({ action: 'init-politics', packet: { ...packet,
          assets: packet.assets.map(a => ({ ...a, bytes: Uint8Array.from(a.bytes) })),
          sourceChunks: packet.sourceChunks.map(a => ({ ...a, bytes: Uint8Array.from(a.bytes) })) } });
        const runs = [];
        for (const simulation of [7, 1, 7]) {
          const ready = await call({ action: 'compute', simulation });
          const chunks: { table: number; rows: unknown[] }[] = [];
          for (;;) {
            const chunk = await call({ action: 'pull' }) as { table: number; buffer: ArrayBuffer } | null;
            if (!chunk) break;
            if (!(chunk.buffer instanceof ArrayBuffer) || chunk.buffer.byteLength > 8 * 1024 * 1024) throw new Error('Unbounded/invalid result chunk');
            chunks.push({ table: chunk.table, rows: JSON.parse(new TextDecoder().decode(chunk.buffer)) });
          }
          runs.push({ ready, chunks });
        }
        return runs;
      } finally { worker.terminate(); }
    }, packet);
    const first = runs[0].ready as ReturnType<typeof computePoliticsRun>['ready'];
    expect(first.counts).toEqual(computePoliticsRun(prepared, 7).ready.counts);
    expect(first.simulation).toBe(7);
    const stableChunks = (run: typeof runs[number]) => run.chunks.map(c => ({ ...c, rows: c.table < 0 ? c.rows : c.rows.map(row =>
      (row as unknown[]).filter((_, i) => !['Tempo ms', 'Tempo medio ms'].includes((run.ready as typeof first).tables[c.table].columns[i]))) }));
    expect(stableChunks(runs[0])).toEqual(stableChunks(runs[2]));
    expect((runs[1].ready as typeof first).simulation).toBe(1);
    expect(runs[0].chunks.some(c => c.table >= 0 && c.rows.length)).toBe(true);
  });
