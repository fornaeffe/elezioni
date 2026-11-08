import { fileURLToPath } from 'node:url';
import { build } from 'vite';
import { expect, test } from '@playwright/test';
import { archive } from '../../src/lib/politics/files/files.fixture';

test('common archive codecs stream gzip and commit validated sessions in an actual browser worker', async ({ page }) => {
  const bundled = await build({ configFile: false, logLevel: 'error',
    resolve: { alias: { $lib: fileURLToPath(new URL('../../src/lib', import.meta.url)) } },
    build: { write: false, minify: false, lib: { entry: fileURLToPath(new URL('../fixtures/common-politics-files.worker.ts', import.meta.url)), formats: ['es'], fileName: 'common-files' } } });
  let code = '';
  for (const output of Array.isArray(bundled) ? bundled : [bundled]) if ('output' in output)
    for (const chunk of output.output) if (chunk.type === 'chunk' && chunk.isEntry) code = chunk.code;
  await page.route('**/common-files-test.js', route => route.fulfill({ contentType: 'text/javascript', body: code }));
  await page.goto(`${process.env.BASE_PATH ?? ''}/`);
  const result = await page.evaluate(() => new Promise<any>((resolve, reject) => {
    const worker = new Worker('/common-files-test.js', { type: 'module' });
    worker.onmessage = ({ data }) => { worker.terminate(); resolve(data); };
    worker.onerror = e => { worker.terminate(); reject(new Error(e.message)); }; worker.postMessage({});
  }));
  expect(result.ok, result.error).toBe(true);
  const original = archive();
  expect(result.rows).toEqual(original.run.result.tables[0].rows);
  expect(result.warnings).toEqual(original.run.result.warnings);
  expect(result.header.run.executedScenario).toEqual(original.run.executedScenario);
  expect(result.header.run.session).toEqual(original.run.session);
  expect(result.metadata.selection).toEqual(original.run.executedScenario.selection);
  expect(result.committed).toBe(true); expect(result.aborted).toBe(true); expect(result.rejected).toBe(true);
});
