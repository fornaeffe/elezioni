import { fileURLToPath } from 'node:url';
import { expect, test } from '@playwright/test';
import { build } from 'vite';
import { completeScrutinyFixture } from '../../src/lib/politics/reform-2026/scrutiny.fixture';
import type { PoliticsSimulationResult } from '../../src/lib/politics/simulation-scrutiny';

type WorkerResponse = { ok: true; result: PoliticsSimulationResult } | { ok: false; message: string };

let workerCode = '';
test.beforeAll(async () => {
  const bundled = await build({ configFile: false, logLevel: 'error',
    resolve: { alias: { $lib: fileURLToPath(new URL('../../src/lib', import.meta.url)) } },
    build: { write: false, minify: false, lib: {
      entry: fileURLToPath(new URL('../fixtures/reform-scrutiny.worker.ts', import.meta.url)), formats: ['es'], fileName: 'reform-worker'
    } }
  });
  const outputs = Array.isArray(bundled) ? bundled : [bundled];
  for (const output of outputs) if ('output' in output) for (const chunk of output.output) if (chunk.type === 'chunk' && chunk.isEntry) workerCode = chunk.code;
  expect(workerCode.length).toBeGreaterThan(0);
});

test('direct reform registry runs in a browser module worker and transfers replayable paired results', async ({ page, context }) => {
  await context.route('**/r26-test-worker.js', route => route.fulfill({ contentType: 'text/javascript', body: workerCode }));
  await page.goto('/');
  const input = completeScrutinyFixture(60);
  const result = await page.evaluate(async input => {
    const worker = new Worker('/r26-test-worker.js', { type: 'module' });
    const call = (data: unknown) => new Promise<WorkerResponse>((resolve, reject) => {
      worker.onmessage = e => resolve(e.data);
      worker.onerror = e => reject(new Error(e.message));
      worker.postMessage(data);
    });
    try {
      const request = { algorithmId: input.algorithmId, simulationId: input.simulationId, input };
      const first = await call({ request }), replay = await call({ request });
      const unavailable = await call({ request, production: true });
      const unknown = await call({ request: { ...request, algorithmId: 'unknown' } });
      return { first, deterministic: JSON.stringify(first) === JSON.stringify(replay), unavailable, unknown };
    } finally { worker.terminate(); }
  }, input);
  expect(result.first.ok).toBe(true);
  if (!result.first.ok || result.first.result.algorithmId !== 'politiche-reform-2026-v1') throw new Error('Expected reform worker output');
  expect(result.first.result).toMatchObject({ algorithmId: input.algorithmId, simulationId: input.simulationId,
    result: { status: 'complete', scope: 'domestic-initial', renewedChambers: ['camera', 'senato'] } });
  expect(result.first.result.result.seats).toHaveLength(588);
  expect(result.deterministic).toBe(true);
  expect(result.unavailable).toMatchObject({ ok: false, message: expect.stringContaining('production-unavailable') });
  expect(result.unknown).toMatchObject({ ok: false, message: expect.stringContaining('unknown-algorithm') });
});
