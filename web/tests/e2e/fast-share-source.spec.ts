import { readFile, mkdir, writeFile } from 'node:fs/promises';
import { resolve, dirname, basename } from 'node:path';
import { fileURLToPath } from 'node:url';
import { build } from 'vite';
import { test, expect } from '@playwright/test';
import { encodeSource } from '../../src/lib/politics/reform-2026/vote-share/source-codec';
import { compileSource } from '../../src/lib/politics/reform-2026/vote-share/source-compiler';
import { originalShareFixture, sourceSelection } from '../fixtures/fast-share-source-input';
const base = process.env.BASE_PATH ?? ''; let workerCode = '';
test.beforeAll(async () => {
  const result = await build({ configFile: false, logLevel: 'error', build: { write: false, minify: false,
    lib: { entry: fileURLToPath(new URL('../fixtures/fast-share-source.worker.ts', import.meta.url)), formats: ['es'], fileName: 'fv03-worker' } } });
  for (const bundle of Array.isArray(result) ? result : [result]) if ('output' in bundle)
    for (const c of bundle.output) if (c.type === 'chunk' && c.isEntry) workerCode = c.code;
});
test('portable original observations compile identically in Node and a browser worker', async ({ page, context }) => {
  await context.route('**/fv03-worker.js', route => route.fulfill({ contentType: 'text/javascript', body: workerCode }));
  await page.goto(base + '/');
  const result = await page.evaluate(async url => {
    const worker = new Worker(url, { type: 'module' });
    try { return await new Promise<any>((resolve, reject) => { worker.onmessage = e => resolve(e.data); worker.onerror = e => reject(new Error(e.message)); worker.postMessage({ action: 'fixture' }); }); }
    finally { worker.terminate(); }
  }, base + '/fv03-worker.js');
  const encoded = await encodeSource(originalShareFixture()), expected = await compileSource(encoded.envelope, async p => encoded.assets.get(p)!, sourceSelection());
  expect(result.ok, result.error).toBe(true); expect(result.id).toBe(encoded.envelope.id);
  expect(result.nationalInputKey).toBe(expected.nationalInputKey); expect(result.profileInputKey).toBe(expected.profileInputKey);
  expect(result.national).toEqual(expected.national);
});
// Opt-in real asset gate. No production-data fallback and no required check skip:
// FV-08 must set this path after preparing/verifying its pinned release sources.
if (process.env.FV03_SOURCE_ASSET) test('complete compact source streams and validates in a browser worker', async ({ page, context, browser }) => {
  const assetFile = resolve(process.env.FV03_SOURCE_ASSET!), directory = dirname(assetFile);
  await context.route('**/fv03-worker.js', route => route.fulfill({ contentType: 'text/javascript', body: workerCode }));
  await context.route('**/fv03-source/*', async route => {
    const name = basename(new URL(route.request().url()).pathname);
    if (!/^[\w.-]+$/.test(name)) return route.abort();
    await route.fulfill({ contentType: name.endsWith('.json') ? 'application/json' : 'application/gzip', body: await readFile(name === 'descriptor.json' ? assetFile : resolve(directory, name)) });
  });
  await page.goto(base + '/');
  const result = await page.evaluate(async ({ url, root }) => {
    const worker = new Worker(url, { type: 'module' });
    try { return await new Promise<any>((resolve, reject) => { worker.onmessage = e => resolve(e.data); worker.onerror = e => reject(new Error(e.message)); worker.postMessage({ action: 'full', root }); }); }
    finally { worker.terminate(); }
  }, { url: base + '/fv03-worker.js', root: base + '/fv03-source' });
  expect(result.ok, result.error).toBe(true); expect(result.events).toHaveLength(10); expect(result.targets).toBe(7904); expect(result.physicalColleges).toBe(86);
  await mkdir('../data/derived/fv03-acceptance', { recursive: true });
  await writeFile('../data/derived/fv03-acceptance/browser-' + (base ? 'project-base' : 'root') + '.json', JSON.stringify({ ...result, browserVersion: browser.version(), base }, null, 2) + '\n');
});
