/** Seed retained result-table evidence directly into test storage. External old
 * politics files are rejected by CP-05; presentation tests do not exercise import. */
import { fileURLToPath } from 'node:url';
import { build } from 'vite';
import type { Page } from '@playwright/test';

let bundle: Promise<string> | undefined;
const installed = new WeakSet<Page>();
export async function seedPresentationSession(page: Page, payload: unknown) {
  bundle ??= (async () => {
    const bundled = await build({ configFile: false, logLevel: 'error',
      resolve: { alias: { $lib: fileURLToPath(new URL('../../src/lib', import.meta.url)) } },
      build: { write: false, minify: false, lib: { entry: fileURLToPath(new URL('./presentation-session.worker.ts', import.meta.url)), formats: ['es'], fileName: 'presentation-fixture' } } });
    for (const output of Array.isArray(bundled) ? bundled : [bundled]) if ('output' in output)
      for (const chunk of output.output) if (chunk.type === 'chunk' && chunk.isEntry) return chunk.code;
    throw new Error('Missing presentation fixture bundle');
  })();
  const code = await bundle;
  if (!installed.has(page)) {
    await page.route('**/presentation-fixture.js', route => route.fulfill({ contentType: 'text/javascript', body: code }));
    installed.add(page);
  }
  await page.evaluate(payload => new Promise<void>((resolve, reject) => {
    const worker = new Worker('/presentation-fixture.js', { type: 'module' });
    worker.onerror = e => { worker.terminate(); reject(new Error(e.message)); };
    worker.onmessage = ({ data }) => {
      worker.terminate();
      if (data.error) { reject(new Error(data.error)); return; }
      const key = 'elezioni:politics-reform-2026:fast-results:v1';
      sessionStorage.setItem(key + ':owner', data.owner);
      sessionStorage.setItem(key, JSON.stringify([{ id: data.resultId, label: 'Retained presentation evidence' }]));
      resolve();
    };
    worker.postMessage(payload);
  }), payload);
  await page.reload();
}
