import { fileURLToPath } from 'node:url';
import { expect, test } from '@playwright/test';
import { build } from 'vite';
import { completeScrutinyFixture, coalitionScrutinyFixture } from '../../src/lib/politics/reform-2026/scrutiny.fixture';
import type { PoliticsSimulationResult } from '../../src/lib/politics/simulation-scrutiny';
import { generationFixture } from '../../src/lib/politics/reform-2026/generation.fixture';
import { generateReformSimulation, prepareReformGeneration } from '../../src/lib/politics/reform-2026/generation';
import { runPoliticsReformScrutiny } from '../../src/lib/politics/reform-2026/scrutiny';

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
  await page.goto(`${process.env.BASE_PATH ?? ''}/`);
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
  expect(result.first.result.result.interpretations).toContainEqual({ id: 'I11', version: 'I11-v1' });
  expect(result.first.result.result.chambers[0].recovery!.excludedLists).toContainEqual({ territoryId: 'T', listId: 'C', reason: 'zero-territorial-entitlement' });
  expect(result.deterministic).toBe(true);
  expect(result.unavailable.ok).toBe(true);
  expect(result.unknown).toMatchObject({ ok: false, message: expect.stringContaining('unknown-algorithm') });
});

test('provisional I10 resolves coalition recovery in the browser and preserves policy warnings', async ({ page, context }) => {
  await context.route('**/r26-test-worker.js', route => route.fulfill({ contentType: 'text/javascript', body: workerCode }));
  await page.goto(`${process.env.BASE_PATH ?? ''}/`);
  const input = coalitionScrutinyFixture();
  if (input.renewal.kind !== 'joint') throw new Error();
  const unavailable = new Set(input.renewal.camera.ordinarySlates.filter(s => s.listId === 'A' || s.listId === 'C' && s.collegeId === 'P00').flatMap(s => s.candidateIds));
  for (const p of input.candidates) if (unavailable.has(p.id)) p.availability = { kind: 'unavailable', reason: 'post-filing shortage' };
  const reply = await page.evaluate(input => new Promise<WorkerResponse>((resolve, reject) => {
    const worker = new Worker('/r26-test-worker.js', { type: 'module' });
    worker.onmessage = e => { worker.terminate(); resolve(e.data); };
    worker.onerror = e => { worker.terminate(); reject(new Error(e.message)); };
    worker.postMessage({ request: { algorithmId: input.algorithmId, simulationId: input.simulationId, input } });
  }), input);
  expect(reply.ok).toBe(true);
  if (!reply.ok || reply.result.algorithmId !== 'politiche-reform-2026-v1') throw new Error('Expected reform worker output');
  expect(reply.result.result.status).toBe('complete');
  expect(reply.result.result.seats).toHaveLength(588);
  expect(reply.result.result.interpretations).toContainEqual({ id: 'I10', version: 'I10-v1' });
  expect(reply.result.result.warnings.some(w => w.interpretationId === 'I10')).toBe(true);
});

test('reform scenario generation and full scrutiny replay identically in a browser worker', async ({ page, context }) => {
  await context.route('**/r26-test-worker.js', route => route.fulfill({ contentType: 'text/javascript', body: workerCode }));
  await page.goto(`${process.env.BASE_PATH ?? ''}/`);
  const fixture = generationFixture();
  const expected = runPoliticsReformScrutiny(generateReformSimulation(prepareReformGeneration(fixture.scenario, fixture.snapshot), 7).input);
  const reply = await page.evaluate(generation => new Promise<WorkerResponse>((resolve, reject) => {
    const worker = new Worker('/r26-test-worker.js', { type: 'module' });
    worker.onmessage = e => { worker.terminate(); resolve(e.data); };
    worker.onerror = e => { worker.terminate(); reject(new Error(e.message)); };
    worker.postMessage({ generation });
  }), { ...fixture, simulationId: 7 });
  expect(reply.ok).toBe(true);
  if (!reply.ok || reply.result.algorithmId !== 'politiche-reform-2026-v1') throw new Error('Expected generated reform output');
  expect(reply.result.result).toEqual(expected);
});
