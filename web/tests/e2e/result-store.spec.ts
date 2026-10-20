import { fileURLToPath } from 'node:url';
import { build } from 'vite';
import { expect, test } from '@playwright/test';

test('chunk boundaries, shared metadata, streaming JSON/CSV, quota isolation and scoped cleanup', async ({ page, context }) => {
  const bundled = await build({ configFile: false, logLevel: 'error',
    resolve: { alias: { $lib: fileURLToPath(new URL('../../src/lib', import.meta.url)) } },
    build: { write: false, minify: false, lib: { entry: fileURLToPath(new URL('../fixtures/result-store.worker.ts', import.meta.url)), formats: ['es'], fileName: 'store-worker' } }
  });
  let code = '';
  for (const output of Array.isArray(bundled) ? bundled : [bundled]) if ('output' in output) for (const chunk of output.output) if (chunk.type === 'chunk' && chunk.isEntry) code = chunk.code;
  await context.route('**/result-store-test.js', route => route.fulfill({ contentType: 'text/javascript', body: code }));
  await page.goto(`${process.env.BASE_PATH ?? ''}/`);
  const result = await page.evaluate(() => new Promise<any>((resolve, reject) => {
    const worker = new Worker('/result-store-test.js', { type: 'module' });
    worker.onmessage = e => { worker.terminate(); resolve(e.data); };
    worker.onerror = e => reject(new Error(e.message)); worker.postMessage({});
  }));
  expect(result.ok, result.error).toBe(true);
  expect(result.boundaryPage.rows).toHaveLength(50);
  expect(result.boundaryPage.rows[0].Name).toContain('250');
  expect(result.boundaryPage.rows[49].Name).toContain('299');
  expect(result.metadata.rows).toEqual([{ Kind: 'source', Value: 'shared', Occurrences: 2 }]);
  expect(result.importedRows[0]).toHaveLength(600);
  expect(result.importedRows[0][1]).toEqual([1, 'Città "Europa" 1 \\ 😀', null]);
  expect(result.importedWarnings).toHaveLength(1);
  expect(result.csvMatches).toBe(true);
  expect(result.invalidImportRejected).toBe(true); expect(result.invalidImportRemoved).toBe(true);
  expect(result.reorderedPage).toEqual(result.boundaryPage);
  expect(result.quotaError).toBe('QuotaExceededError');
  expect(result.retainedRows).toBe(600);
  expect(result.afterFailureRows).toBe(0);
  expect(result.foreignDenied).toBe(true); expect(result.stillPresent).toBe(true);
});
