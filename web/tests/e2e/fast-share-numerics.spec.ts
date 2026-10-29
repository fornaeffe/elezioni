import { createHash } from 'node:crypto';
import { fileURLToPath } from 'node:url';
import { mkdir, writeFile } from 'node:fs/promises';
import { build } from 'vite';
import { test, expect, type Page } from '@playwright/test';
import oracle from '../fixtures/fast-share-reference.generated.json' with { type: 'json' };
import { fixtureTarget } from '../fixtures/fast-share-gates';
import { reducedShareInput } from '../fixtures/fast-share-input';
import { fitGaussian, fitReference, fitProfiles } from '../../src/lib/politics/reform-2026/vote-share/index';
import type { NumericalWorkerRequest } from '../fixtures/fast-share.worker';

let workerCode = '';
const base = process.env.BASE_PATH ?? '';
const hash = (values: Float64Array) => createHash('sha256').update(new Uint8Array(values.buffer)).digest('hex');
test.beforeAll(async () => {
  const result = await build({ configFile: false, logLevel: 'error',
    build: { write: false, minify: false, lib: {
      entry: fileURLToPath(new URL('../fixtures/fast-share.worker.ts', import.meta.url)),
      formats: ['es'], fileName: 'fv02-worker'
    } }
  });
  for (const bundle of Array.isArray(result) ? result : [result]) if ('output' in bundle)
    for (const chunk of bundle.output) if (chunk.type === 'chunk' && chunk.isEntry) workerCode = chunk.code;
  expect(workerCode.length).toBeGreaterThan(0);
});
test.beforeEach(async ({ page, context }) => {
  await context.route('**/fv02-numerics.worker.js', route => route.fulfill({ contentType: 'text/javascript', body: workerCode }));
  await page.goto(base + '/');
});
async function call(page: Page, request: NumericalWorkerRequest): Promise<Record<string, any>> {
  return page.evaluate(async ({ request, url }) => {
    const worker = new Worker(url, { type: 'module' });
    try {
      return await new Promise<Record<string, any>>((resolve, reject) => {
        worker.onmessage = event => resolve(event.data);
        worker.onerror = event => reject(new Error(event.message));
        worker.postMessage(request);
      });
    } finally { worker.terminate(); }
  }, { request, url: base + '/fv02-numerics.worker.js' });
}
for (const fixture of oracle.quadrature) test(fixture.id + ' has identical accepted Node/browser draws for all frozen seeds', async ({ page }) => {
  const target = fixtureTarget(fixture), times: number[] = [];
  for (let i = 0; i < 3; i++) {
    const seed = 'fv02-numeric-' + i, expected = fitGaussian(target, { seed });
    const result = await call(page, { action: 'sample', seed, target: {
      components: target.components, blocks: target.blocks, precision: [...target.precision], linear: [...target.linear]
    } });
    expect(result.ok, result.error).toBe(true);
    expect(result.hash).toBe(hash(expected.values));
    expect(result.drawsPerChain).toBe(expected.drawsPerChain);
    expect(result.diagnostics).toEqual(expected.diagnostics);
    times.push(result.milliseconds);
  }
  test.info().annotations.push({ type: 'worker-timings-ms', description: JSON.stringify(times) });
});
test('complete reduced C/E fitting and empirical profiles run off the main thread without production assets', async ({ page, browser }) => {
  const input = reducedShareInput(), expected = fitReference(input.national, 'fv02-worker');
  const result = await call(page, { action: 'reference', source: input.national, seed: 'fv02-worker', fit: true, options: { solver: 'matrix-free' } });
  expect(result.ok, result.error).toBe(true);
  // Matrix-free versus dense changes arithmetic roundoff; compare the same solver on both sides.
  const sameSolver = fitReference(input.national, 'fv02-worker', { solver: 'matrix-free' });
  expect(result.hashes).toEqual({ political: hash(sameSolver.political.values), participation: hash(sameSolver.participation.values) });
  expect(expected.political.diagnostics.passed).toBe(true);
  const profile = await call(page, { action: 'profiles', source: input.profiles }), local = fitProfiles(input.profiles);
  expect(profile.ok, profile.error).toBe(true);
  expect(profile.means).toEqual([...local.means]); expect(profile.profileSd).toEqual([...local.profileSd]);
  const report = { base, browserVersion: browser.version(), referenceMilliseconds: result.milliseconds,
    profileMilliseconds: profile.milliseconds, sourceWorkspaceBytes: result.audit.workspaceBytes,
    profileWorkspaceBytes: profile.audit.workspaceBytes, nodeBrowserIdentical: true };
  await mkdir('../data/derived/fv02-numerics', { recursive: true });
  await writeFile('../data/derived/fv02-numerics/browser-' + (base ? 'project-base' : 'root') + '.json', JSON.stringify(report, null, 2) + '\n');
});
test('invalid targets and oversized workspace fail explicitly in the worker', async ({ page }) => {
  const bad = await call(page, { action: 'sample', seed: 'fv02-error', target: { components: 2, blocks: 1, precision: [0, 0, 0, 0], linear: [0, 0] } });
  expect(bad.ok).toBe(false); expect(bad.error).toContain('SPD');
  const bounded = await call(page, { action: 'reference', source: reducedShareInput().national, seed: 'x', options: { maximumWorkspaceBytes: 10 } });
  expect(bounded.ok).toBe(false); expect(bounded.error).toContain('FIT_WORKSPACE_LIMIT');
});
