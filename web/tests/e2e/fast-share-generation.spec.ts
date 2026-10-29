import { createHash } from 'node:crypto';
import { fileURLToPath } from 'node:url';
import { mkdir, writeFile } from 'node:fs/promises';
import { build } from 'vite';
import { test, expect } from '@playwright/test';
import { fastGenerationFixture } from '../fixtures/fast-generation-input';
import { prepareFastGeneration } from '../../src/lib/politics/reform-2026/vote-share/generation';
import { calibrateReferences } from '../../src/lib/politics/reform-2026/vote-share/reference-family';
import type { FastGenerationRequest } from '../fixtures/fast-generation.worker';
let code = '';
const base = process.env.BASE_PATH ?? '';
test.beforeAll(async () => {
  const result = await build({
    configFile: false,
    logLevel: 'error',
    build: {
      write: false,
      minify: false,
      lib: {
        entry: fileURLToPath(new URL('../fixtures/fast-generation.worker.ts', import.meta.url)),
        formats: ['es'],
        fileName: 'fv04-worker'
      }
    }
  });
  for (const bundle of Array.isArray(result) ? result : [result])
    if ('output' in bundle)
      for (const chunk of bundle.output) if (chunk.type === 'chunk' && chunk.isEntry) code = chunk.code;
  expect(code.length).toBeGreaterThan(0);
});
test('FV-04 physical counts, legal assembly and uncertain references match Node in a browser worker', async ({
  page,
  context,
  browser
}) => {
  await context.route('**/fv04-generation.worker.js', (route) =>
    route.fulfill({ contentType: 'text/javascript', body: code })
  );
  await page.goto(base + '/');
  const f = fastGenerationFixture();
  f.scenario.references = [
    {
      id: 'ref-a',
      scope: { kind: 'national' },
      channel: 'common',
      subject: { kind: 'list', id: f.scenario.lists.camera[0].id },
      mean: 0.4,
      sdPercentagePoints: 2
    }
  ];
  const request: FastGenerationRequest = {
    scenario: f.scenario,
    assets: f.assets,
    context: f.context,
    simulationIds: [7, 2],
    chunkSize: 7
  };
  const p = prepareFastGeneration(f.scenario, f.assets, f.context, { chunkSize: 1 });
  const expected = request.simulationIds.map((id) => {
    const { input } = p.run(id);
    return { id, hash: createHash('sha256').update(JSON.stringify(input)).digest('hex') };
  });
  const result = await page.evaluate(
    async ({ request, url }) => {
      const worker = new Worker(url, { type: 'module' });
      try {
        return await new Promise<any>((resolve, reject) => {
          worker.onmessage = (e) => resolve(e.data);
          worker.onerror = (e) => reject(new Error(e.message));
          worker.postMessage(request);
        });
      } finally {
        worker.terminate();
      }
    },
    { request, url: base + '/fv04-generation.worker.js' }
  );
  expect(result.ok, result.error).toBe(true);
  expect(result.results.map((r: any) => ({ id: r.id, hash: r.hash }))).toEqual(expected);
  const calibration = calibrateReferences(
    [
      { id: 'a', members: ['a'], mean: 0.3, sdPercentagePoints: 2 },
      { id: 'b', members: ['b'], mean: 0.4, sdPercentagePoints: 3 }
    ],
    true
  );
  calibration.mu.forEach((x, i) => expect(result.calibration.mu[i]).toBeCloseTo(x, 10));
  calibration.sigma.forEach((x, i) => expect(result.calibration.sigma[i]).toBeCloseTo(x, 10));
  expect(result.results.every((r: any) => r.audit.projectionResidual <= 1e-9)).toBe(true);
  test.info().annotations.push({ type: 'worker-ms', description: String(result.milliseconds) });
  await mkdir('../data/derived/fv04-generation', { recursive: true });
  await writeFile(
    '../data/derived/fv04-generation/browser-' + (base ? 'project-base' : 'root') + '.json',
    JSON.stringify(
      {
        base,
        browserVersion: browser.version(),
        node: process.version,
        inputHashes: expected,
        nodeBrowserInputByteIdentical: true,
        calibrationTolerance: 1e-10,
        workerMilliseconds: result.milliseconds,
        qualification:
          'One test-worker run, structured-cloned prepared assets and intercepted worker script. Functional parity only; not a production latency or browser process memory benchmark.'
      },
      null,
      2
    ) + '\n'
  );
});
