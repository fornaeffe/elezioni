import { existsSync } from 'node:fs';
import { readFile, mkdir, writeFile } from 'node:fs/promises';
import { resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import { createHash } from 'node:crypto';
import { build } from 'vite';
import { test, expect, type Page } from '@playwright/test';
import {
  loadCandidate,
  type CandidatePointer,
} from '../../src/lib/politics/reform-2026/vote-share/release';
import {
  readBytes,
  implementationKey,
} from '../../scripts/data-preparation/fast-share-release-io';
import type { ReleaseWorkerRequest } from '../fixtures/fast-release.worker';
const root = fileURLToPath(new URL('../../../', import.meta.url)),
  directory = resolve(root, 'web/static/data/reform-2026/fast-share'),
  base = process.env.BASE_PATH ?? '';
test.skip(
  !existsSync(resolve(directory, 'candidate.json')),
  'Run data:reform:shares:release to prepare required production candidate assets. Release checks never skip missing assets.',
);
let code = '',
  implementation = '',
  expected: {
    catalogId: string;
    inputHash: string;
    fitHash: string;
    profileFile: string;
    sourceId: string;
  };
test.beforeAll(async () => {
  implementation = await implementationKey(
    root,
    'web/src/lib/politics/reform-2026/vote-share/generation.ts',
  );
  const result = await build({
    configFile: false,
    logLevel: 'error',
    build: {
      write: false,
      minify: false,
      lib: {
        entry: fileURLToPath(
          new URL('../fixtures/fast-release.worker.ts', import.meta.url),
        ),
        formats: ['es'],
        fileName: 'fv05-worker',
      },
    },
  });
  for (const bundle of Array.isArray(result) ? result : [result])
    if ('output' in bundle)
      for (const chunk of bundle.output)
        if (chunk.type === 'chunk' && chunk.isEntry) code = chunk.code;
  const pointer: CandidatePointer = JSON.parse(
      await readFile(resolve(directory, 'candidate.json'), 'utf8'),
    ),
    loaded = await loadCandidate(pointer, readBytes(directory), {
      expectedGenerationImplementationId: implementation,
    }),
    run = loaded.prepared!.run(1),
    hash = (b: Uint8Array | string) =>
      createHash('sha256').update(b).digest('hex');
  expected = {
    catalogId: pointer.catalogId,
    inputHash: hash(JSON.stringify(run.input)),
    fitHash: hash(new Uint8Array(loaded.assets.fit.political.values.buffer)),
    profileFile: loaded.catalog.assets.profiles.path,
    sourceId: loaded.catalog.sourceId,
  };
});
test.beforeEach(async ({ context, page }) => {
  await context.route('**/fv05-release.worker.js', (r) =>
    r.fulfill({ contentType: 'text/javascript', body: code }),
  );
  await page.goto(base + '/');
});
async function call(page: Page, action: ReleaseWorkerRequest['action']) {
  return page.evaluate(
    async ({ action, base, implementation }) => {
      let heartbeats = 0;
      const timer = setInterval(() => heartbeats++, 20),
        worker = new Worker(base + '/fv05-release.worker.js', {
          type: 'module',
        });
      try {
        return await new Promise<any>((done, reject) => {
          worker.onmessage = (e) => done({ ...e.data, heartbeats });
          worker.onerror = (e) => reject(new Error(e.message));
          worker.postMessage({
            action,
            root: new URL(
              base + '/data/reform-2026/fast-share/',
              location.origin,
            ).href,
            implementation,
          });
        });
      } finally {
        worker.terminate();
        clearInterval(timer);
      }
    },
    { action, base, implementation },
  );
}
test('default precomputed buffers load from the static build and generate Node-identical legal input without refit data', async ({
  page,
  browser,
}) => {
  const result = await call(page, 'generate');
  expect(result.ok, result.error).toBe(true);
  expect(result.catalogId).toBe(expected.catalogId);
  expect(result.fitHash).toBe(expected.fitHash);
  expect(result.inputHash).toBe(expected.inputHash);
  expect(result.counts).toEqual({ camera: 46120143, senato: 46120143 });
  expect(result.projectionResidual).toBeLessThanOrEqual(1e-9);
  expect(result.requests).not.toContain(result.sourceMetadata);
  expect(result.reportedEncodedBytes).toBe(result.expectedBytes);
  expect(result.heartbeats).toBeGreaterThan(0);
  await mkdir('../data/derived/fv05-acceptance', { recursive: true });
  await writeFile(
    '../data/derived/fv05-acceptance/browser-' +
      (base ? 'project-base' : 'root') +
      '.json',
    JSON.stringify(
      {
        base,
        browser: browser.version(),
        ...result,
        qualification:
          'One test-worker run with actual static candidate HTTP assets. Encoded lengths are server-reported Content-Length; body bytes may be transparently decoded and are checked against decoded SHA-256. Node/browser parity, not browser process memory, default predictive acceptance or estimation-worker lifecycle.',
      },
      null,
      2,
    ) + '\n',
  );
});
test('all original history and poll metadata remains available for demand-loaded refitting', async ({
  page,
}) => {
  const result = await call(page, 'source');
  expect(result.ok, result.error).toBe(true);
  expect(result.sourceId).toBe(expected.sourceId);
  expect(result.events).toHaveLength(10);
  expect(result.events).toContainEqual({ id: 'senato-2018', minimumAge: 25 });
  expect(result.rows).toBeGreaterThan(79040);
});
test('missing required profiles fail explicitly instead of fitting or substituting another state', async ({
  context,
  page,
}) => {
  await context.route('**/' + expected.profileFile, (r) =>
    r.fulfill({ status: 404, body: 'missing required profile state' }),
  );
  const result = await call(page, 'generate');
  expect(result.ok).toBe(false);
  expect(result.error).toContain('HTTP 404');
  expect(result.inputHash).toBeUndefined();
});
