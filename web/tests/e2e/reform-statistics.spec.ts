import { fileURLToPath } from 'node:url';
import { build } from 'vite';
import { expect, test } from '@playwright/test';
import { readFileSync } from 'node:fs';
import { resolve } from 'node:path';
import catalog from '../../src/lib/politics/reform-2026/data/generation-catalog.generated.json' with { type: 'json' };
import type { ReformGenerationScenario } from '../../src/lib/politics/reform-2026/generation-types';
import { multipleSeriesResult, statisticalResult } from '../fixtures/statistical-result';

test('disk statistics preserve exact ranks, unknown outcomes, bounded pages, ownership and archive content beyond 1000 runs', async ({ page, context }) => {
  const bundled = await build({ configFile: false, logLevel: 'error', resolve: { alias: { $lib: fileURLToPath(new URL('../../src/lib', import.meta.url)) } },
    build: { write: false, minify: false, lib: { entry: fileURLToPath(new URL('../fixtures/statistics.worker.ts', import.meta.url)), formats: ['es'], fileName: 'statistics-worker' } } });
  let code = '';
  for (const output of Array.isArray(bundled) ? bundled : [bundled]) if ('output' in output) for (const chunk of output.output) if (chunk.type === 'chunk' && chunk.isEntry) code = chunk.code;
  await context.route('**/statistics-test.js', route => route.fulfill({ contentType: 'text/javascript', body: code }));
  await page.goto(`${process.env.BASE_PATH ?? ''}/`);
  const result = await page.evaluate(() => new Promise<any>((resolve, reject) => {
    const worker = new Worker('/statistics-test.js', { type: 'module' });
    worker.onmessage = e => { worker.terminate(); resolve(e.data); }; worker.onerror = e => reject(new Error(e.message)); worker.postMessage({});
  }));
  expect(result.ok, result.error).toBe(true);
  expect(result.overview).toMatchObject({ requested: 1201, saved: 1201, legacy: false });
  expect(result.stats).toMatchObject({ requested: 1201, known: 1200, missingY: 1, missingX: 0 });
  const values = Array.from({ length: 1201 }, (_, i) => ((i + 1) % 101) / 10).sort((a, b) => a - b);
  expect(result.stats.distribution).toMatchObject({ count: 1201, min: 0, max: 10, q1: values[300], median: values[600], q3: values[900] });
  expect(result.stats.bins.flatMap((b: any) => b.outcomes).filter((o: any) => o.seats === null).reduce((n: number, o: any) => n + o.count, 0)).toBe(1);
  expect(result.college.distribution.min).toBe(12.345678 + .000001);
  expect(result.college.bins).toHaveLength(1);
  expect(result.entitlement.known).toBe(1201);
  expect(result.entitlement.bins[0].outcomes).toEqual([{ seats: 1, count: 1201 }]);
  expect(result.pages.map((p: any) => p.length)).toEqual([256, 256, 256, 256, 177]);
  expect(result.pages.at(-1).last).toBe(1201);
  expect(result.foreignDenied).toBe(true); expect(result.archiveUnchanged).toBe(true); expect(result.removed).toBe(true);
  expect(result.cacheFailure).toBe(true); expect(result.canonicalRetained).toBe(true); expect(result.partialCacheRemoved).toBe(true); expect(result.rebuiltKnown).toBe(1201);
});

test('reform chart families render all available points lazily and keep old archives readable', async ({ page }) => {
  test.setTimeout(90000);
  const errors: string[] = []; page.on('pageerror', e => errors.push(e.message));
  const scenario = JSON.parse(readFileSync(resolve('src/lib/politics/reform-2026/data', catalog.defaultsPath), 'utf8')) as ReformGenerationScenario, archive = statisticalResult(scenario);
  await page.goto(`${process.env.BASE_PATH ?? ''}/politics`);
  await page.getByLabel('Algoritmo di scrutinio').selectOption(scenario.algorithmId);
  await page.getByTestId('reform-file-input').setInputFiles({ name: 'constructed-statistics.json', mimeType: 'application/json', buffer: Buffer.from(JSON.stringify(archive)) });
  const statistics = page.getByLabel('Statistiche riforma', { exact: true });
  await expect(statistics.getByRole('heading', { name: 'Eletti medi e percentuali sui voti validi' })).toBeVisible({ timeout: 30000 });
  await expect(statistics).toContainText('1201/1201');
  await expect(statistics.getByRole('region', { name: 'Percentuali simulate per lista', exact: true })).toBeVisible({ timeout: 30000 });
  const scatter = statistics.getByRole('region', { name: 'Eletti e percentuali', exact: true });
  expect(await scatter.locator('canvas').count()).toBe(0);
  await scatter.locator('summary').click();
  const canvas = scatter.locator('canvas'); await canvas.scrollIntoViewIfNeeded();
  await expect(canvas).toHaveAttribute('data-complete', 'true'); await expect(canvas).toHaveAttribute('data-points', '1200');
  await expect(scatter).toContainText('1 esiti ignoti');
  await scatter.getByRole('combobox', { name: 'Raggruppamento', exact: true }).selectOption('subject');
  await expect(canvas).toHaveAttribute('data-complete', 'true');
  await expect(canvas).toHaveAttribute('data-points', '1200');
  await scatter.getByRole('combobox', { name: 'Ramo', exact: true }).selectOption('senato');
  await expect(canvas).toHaveCount(0);
  await expect(scatter).toContainText('Nessuna percentuale disponibile');
  await scatter.locator('summary').click(); await expect(scatter.getByRole('combobox')).toHaveCount(0);
  const spines = statistics.getByRole('region', { name: 'Spinogrammi degli eletti', exact: true });
  await spines.locator(':scope > summary').click();
  await spines.locator('.series-panel').scrollIntoViewIfNeeded();
  await expect(spines.getByText('?', { exact: true })).toBeVisible();
  await spines.getByRole('button').first().click(); await expect(page.getByRole('dialog')).toBeVisible();
  await page.keyboard.press('Escape'); await expect(page.getByRole('dialog')).toHaveCount(0);
  await statistics.getByText('Eletti nel collegio plurinominale', { exact: true }).click();
  await page.getByLabel('Cerca Collegio del grafico').fill('prova 2');
  await page.getByRole('combobox', { name: 'Collegio del grafico', exact: true }).selectOption({ label: `camera · ${scenario.lists.camera[0].name} · Collegio di prova 2` });
  await page.getByRole('combobox', { name: 'Misura del collegio', exact: true }).selectOption('entitled');
  await statistics.locator('.series-panel').last().scrollIntoViewIfNeeded();
  await expect(statistics).toContainText('1201/1201 coppie note');
  await statistics.getByText('Tabella riepilogativa degli eletti e delle percentuali', { exact: true }).click();
  await expect(statistics.getByRole('table')).toContainText('Non disponibile');
  await page.screenshot({ path: 'test-results/reform-statistics.png', fullPage: true });
  await page.setViewportSize({ width: 390, height: 844 });
  const localControl = page.getByRole('combobox', { name: 'Collegio del grafico', exact: true });
  await localControl.scrollIntoViewIfNeeded();
  expect(await localControl.evaluate(e => e.getBoundingClientRect().right)).toBeLessThanOrEqual(390);
  expect(await statistics.evaluate(e => e.scrollWidth)).toBeLessThanOrEqual(await statistics.evaluate(e => e.clientWidth));
  await statistics.screenshot({ path: 'test-results/reform-statistics-mobile.png' });
  const old = structuredClone(archive); old.result.tables = old.result.tables.filter(t => !t.name.includes('series v1'));
  const previous = await page.getByLabel('Risultati locali').inputValue();
  await page.getByTestId('reform-file-input').setInputFiles({ name: 'old-statistics.json', mimeType: 'application/json', buffer: Buffer.from(JSON.stringify(old)) });
  await expect(page.getByLabel('Risultati locali')).not.toHaveValue(previous);
  await expect(statistics).toContainText('Archivio precedente', { timeout: 30000 });
  await statistics.getByText('Eletti nel collegio plurinominale', { exact: true }).click();
  await expect(statistics).toContainText('Dati locali non disponibili');
  expect(errors).toEqual([]);
});

test('reform selectors display one chart per family and replace the selected data', async ({ page }) => {
  const errors: string[] = []; page.on('pageerror', e => errors.push(e.message));
  const scenario = JSON.parse(readFileSync(resolve('src/lib/politics/reform-2026/data', catalog.defaultsPath), 'utf8')) as ReformGenerationScenario;
  await page.goto(`${process.env.BASE_PATH ?? ''}/politics`);
  await page.getByLabel('Algoritmo di scrutinio').selectOption(scenario.algorithmId);
  await page.getByTestId('reform-file-input').setInputFiles({ name: 'multiple-series.json', mimeType: 'application/json', buffer: Buffer.from(JSON.stringify(multipleSeriesResult(scenario))) });
  const statistics = page.getByLabel('Statistiche riforma', { exact: true });
  const means = statistics.locator('.paired-bars');
  await expect(means).toHaveCount(1);
  await expect(means.locator('.row')).toHaveCount(2);
  await expect(means).toContainText('camera list 1');
  await statistics.getByRole('combobox', { name: 'Ramo delle medie', exact: true }).selectOption('senato');
  await expect(means).toHaveCount(1);
  await expect(means).toContainText('senato list 1');
  await expect(means).not.toContainText('camera list');

  const boxes = statistics.getByRole('region', { name: 'Percentuali simulate per lista', exact: true });
  await expect(boxes.locator('.boxplot-panel')).toHaveCount(1);
  await expect(boxes.locator('.boxplot-row')).toHaveCount(2);
  await expect(boxes).toContainText('camera list 1');
  await statistics.getByRole('combobox', { name: 'Ramo dei boxplot', exact: true }).selectOption('senato');
  await expect(boxes.locator('.boxplot-panel')).toHaveCount(1);
  await expect(boxes).toContainText('senato list 1');
  await expect(boxes).not.toContainText('camera list');

  await expect(statistics.getByRole('region', { name: 'Eletti e percentuali per coalizione', exact: true })).toHaveCount(0);
  for (const family of ['Eletti e percentuali', 'Spinogrammi degli eletti']) {
    const region = statistics.getByRole('region', { name: family, exact: true });
    await expect(region.locator('.series-panel')).toHaveCount(0);
    await region.locator(':scope > summary').click();
    for (const chamber of ['camera', 'senato']) for (const kind of ['list', 'subject']) {
      await region.getByRole('combobox', { name: 'Ramo', exact: true }).selectOption(chamber);
      await region.getByRole('combobox', { name: 'Raggruppamento', exact: true }).selectOption(kind);
      const entity = region.getByRole('combobox', { name: kind === 'list' ? 'Lista' : 'Coalizione / lista singola', exact: true });
      await expect(entity.locator('option')).toHaveCount(2);
      for (const number of [2, 1]) {
        const name = `${chamber} ${kind} ${number}`;
        await entity.selectOption({ label: name });
        const panel = region.locator('.series-panel');
        await expect(panel).toHaveCount(1);
        await expect(panel).toHaveAttribute('aria-label', `${chamber} · ${name}`);
        await panel.scrollIntoViewIfNeeded();
        const unknown = (chamber === 'camera' ? 0 : 4) + (kind === 'list' ? 0 : 2) + number - 1;
        await expect(panel).toContainText(`${12 - unknown}/12 coppie note`);
        if (family === 'Eletti e percentuali') {
          const canvas = panel.locator('canvas');
          await expect(canvas).toHaveCount(1);
          await expect(canvas).toHaveAttribute('data-complete', 'true');
          await expect(canvas).toHaveAttribute('data-points', String(12 - unknown));
        } else await expect(panel.locator('.spine-plot')).toHaveCount(1);
      }
    }
    await region.locator(':scope > summary').click();
    await expect(region.locator('.series-panel')).toHaveCount(0);
  }
  await page.setViewportSize({ width: 390, height: 844 });
  const scatter = statistics.getByRole('region', { name: 'Eletti e percentuali', exact: true });
  await scatter.locator(':scope > summary').click();
  for (const control of await scatter.getByRole('combobox').all()) {
    expect(await control.evaluate(e => e.getBoundingClientRect().right)).toBeLessThanOrEqual(390);
  }
  expect(await statistics.evaluate(e => e.scrollWidth)).toBeLessThanOrEqual(await statistics.evaluate(e => e.clientWidth));
  await scatter.screenshot({ path: 'test-results/reform-chart-selectors-mobile.png' });
  expect(errors).toEqual([]);
});
