import { fileURLToPath } from 'node:url';
import { build } from 'vite';
import { expect, test } from '@playwright/test';
import { readFileSync } from 'node:fs';
import { resolve } from 'node:path';
import catalog from '../../src/lib/politics/reform-2026/data/generation-catalog.generated.json' with { type: 'json' };
import type { ReformGenerationScenario } from '../../src/lib/politics/reform-2026/generation-types';
import { multipleSeriesResult, statisticalResult, refinementResult } from '../fixtures/statistical-result';

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
  for (const filter of ['awarded', 'not-awarded']) {
    const subset = Array.from({ length: 600 }, (_, i) => ((2 * i + (filter === 'awarded' ? 1 : 2)) % 101) / 10).sort((a, b) => a - b);
    const quantile = (p: number) => { const rank = 599 * p, lower = Math.floor(rank); return subset[lower] + (subset[Math.ceil(rank)] - subset[lower]) * (rank - lower); };
    expect(result.filtered[filter]).toMatchObject({ requested: 600, known: filter === 'awarded' ? 600 : 599, missingX: 0, missingY: filter === 'awarded' ? 0 : 1,
      distribution: { count: 600, min: subset[0], max: subset[599], q1: quantile(.25), median: quantile(.5), q3: quantile(.75) } });
    expect(result.filtered[filter].bins.reduce((n: number, b: any) => n + b.count, 0)).toBe(600);
    expect(result.filtered[filter].bins[0].high).toBe(quantile(.1));
  }
  expect(result.filteredEntitlement).toMatchObject({ requested: 600, known: 600 });
  expect(result.filteredEntitlement.bins[0].outcomes).toEqual([{ seats: 1, count: 600 }]);
  expect(result.pages.map((p: any) => p.length)).toEqual([256, 256, 256, 256, 177]);
  expect(result.pages.at(-1).last).toBe(1201);
  expect(result.foreignDenied).toBe(true); expect(result.archiveUnchanged).toBe(true); expect(result.removed).toBe(true);
  expect(result.cacheFailure).toBe(true); expect(result.canonicalRetained).toBe(true); expect(result.partialCacheRemoved).toBe(true); expect(result.rebuiltKnown).toBe(1201);
});

test('college spinogram filters joint bonus decisions and reports plotted simulations, missing values and empty subsets', async ({ page }) => {
  const errors: string[] = []; page.on('pageerror', e => errors.push(e.message));
  const scenario = JSON.parse(readFileSync(resolve('src/lib/politics/reform-2026/data', catalog.defaultsPath), 'utf8')) as ReformGenerationScenario;
  const archive = refinementResult(scenario), localTable = archive.result.tables.find(t => t.name === 'Reform college series v1')!;
  for (const row of localTable.rows) {
    const simulation = Number(row[localTable.columns.indexOf('Simulation')]);
    row[localTable.columns.indexOf('Vote share %')] = simulation === 7 ? null : simulation;
    row[localTable.columns.indexOf('Elected')] = simulation === 2 ? null : simulation % 3;
  }
  await page.goto(`${process.env.BASE_PATH ?? ''}/politics`);
  await page.getByLabel('Algoritmo di scrutinio').selectOption(scenario.algorithmId);
  await page.getByTestId('reform-file-input').setInputFiles({ name: 'bonus-filter.json', mimeType: 'application/json', buffer: Buffer.from(JSON.stringify(archive)) });
  const statistics = page.getByLabel('Statistiche riforma', { exact: true });
  const local = statistics.locator('details').filter({ has: page.locator('summary', { hasText: /^Eletti nel collegio plurinominale$/ }) });
  await local.locator(':scope > summary').click();
  const filter = local.getByRole('combobox', { name: 'Simulazioni del collegio', exact: true });
  const panel = local.locator('.series-panel');
  await panel.scrollIntoViewIfNeeded();
  await expect(filter).toHaveValue('all');
  await expect(panel).toContainText('Simulazioni nel grafico: 11 · Simulazioni selezionate: 12');
  await expect(panel).toContainText('10/12 coppie note · 1 esiti ignoti con percentuale nota · 1 percentuali non disponibili');
  await filter.selectOption('awarded');
  await expect(panel).toContainText('Simulazioni nel grafico: 6 · Simulazioni selezionate: 6');
  await expect(panel).toContainText('5/6 coppie note');
  await expect(panel.locator('.spine-plot')).toHaveCount(1);
  await local.getByLabel('Misura del collegio').selectOption('entitled');
  await expect(filter).toHaveValue('awarded');
  await expect(panel).toContainText('6/6 coppie note');
  await filter.selectOption('not-awarded');
  await expect(panel).toContainText('Simulazioni nel grafico: 4 · Simulazioni selezionate: 5');
  await expect(panel).toContainText('4/5 coppie note · 0 esiti ignoti con percentuale nota · 1 percentuali non disponibili');
  await local.getByLabel('Ramo del collegio').selectOption('senato');
  await local.getByLabel('Lista del collegio').selectOption('list:constructed-2');
  await local.getByRole('combobox', { name: 'Collegio del grafico', exact: true }).selectOption('college-2');
  await expect(filter).toHaveValue('not-awarded');
  await expect(panel).toContainText('Simulazioni nel grafico: 4 · Simulazioni selezionate: 5');
  await filter.selectOption('all');
  await expect(panel).toContainText('Simulazioni nel grafico: 11 · Simulazioni selezionate: 12');
  await page.setViewportSize({ width: 390, height: 844 });
  await filter.scrollIntoViewIfNeeded();
  expect(await filter.evaluate(e => e.getBoundingClientRect().right)).toBeLessThanOrEqual(390);
  await local.screenshot({ path: 'test-results/reform-college-bonus-filter-mobile.png' });
  // An older archive with no bonus decision cannot establish either subset.
  await page.getByTestId('reform-file-input').setInputFiles({ name: 'unknown-bonus.json', mimeType: 'application/json', buffer: Buffer.from(JSON.stringify(statisticalResult(scenario, 12))) });
  await local.locator(':scope > summary').click();
  for (const value of ['awarded', 'not-awarded']) {
    await filter.selectOption(value);
    await panel.scrollIntoViewIfNeeded();
    await expect(panel).toContainText('Simulazioni nel grafico: 0 · Simulazioni selezionate: 0');
    await expect(panel).toContainText('Nessun dato disponibile per il filtro selezionato.');
    await expect(panel.locator('.spine-plot')).toHaveCount(0);
  }
  await filter.selectOption('all');
  await expect(panel).toContainText('Simulazioni nel grafico: 12 · Simulazioni selezionate: 12');
  expect(errors).toEqual([]);
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
  await expect(spines.locator('details')).toHaveCount(0);
  await spines.getByRole('button').first().click(); await expect(page.getByRole('dialog')).toBeVisible();
  await page.keyboard.press('Escape'); await expect(page.getByRole('dialog')).toHaveCount(0);
  await statistics.getByText('Eletti nel collegio plurinominale', { exact: true }).click();
  await page.getByLabel('Cerca Collegio del grafico').fill('prova 2');
  await page.getByRole('combobox', { name: 'Collegio del grafico', exact: true }).selectOption({ label: 'Collegio di prova 2' });
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
  await statistics.getByText('Probabilità di elezione', { exact: true }).click();
  await expect(statistics).toContainText('Dati delle posizioni non disponibili');
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


test('refinements preserve probabilities, coalition data, bonus decisions, local selections and compact feedback', async ({ page }) => {
  const errors: string[] = []; page.on('pageerror', e => errors.push(e.message));
  const scenario = JSON.parse(readFileSync(resolve('src/lib/politics/reform-2026/data', catalog.defaultsPath), 'utf8')) as ReformGenerationScenario;
  await page.goto(`${process.env.BASE_PATH ?? ''}/politics`);
  await page.getByLabel('Algoritmo di scrutinio').selectOption(scenario.algorithmId);
  await page.getByTestId('reform-file-input').setInputFiles({ name: 'refinements.json', mimeType: 'application/json', buffer: Buffer.from(JSON.stringify(refinementResult(scenario))) });
  const statistics = page.getByLabel('Statistiche riforma', { exact: true });
  await expect(statistics.locator('.paired-bars')).toBeVisible();
  await statistics.getByLabel('Raggruppamento delle medie').selectOption('subject');
  await expect(statistics.locator('.paired-bars')).toContainText('camera subject 1');
  await statistics.getByLabel('Raggruppamento dei boxplot').selectOption('subject');
  const boxes = statistics.getByRole('region', { name: 'Percentuali simulate per coalizione / lista singola', exact: true });
  await expect(boxes).toContainText('camera subject 2');
  await statistics.getByLabel('Ramo dei boxplot').selectOption('senato');
  await expect(boxes).toContainText('senato subject 2');
  await expect(boxes.locator('.boxplot-panel')).toHaveCount(1);
  const bonus = statistics.getByRole('region', { name: 'Vincitori del premio', exact: true });
  await bonus.locator('summary').click();
  await expect(bonus.locator('.bonus-row')).toHaveCount(2);
  await expect(bonus.locator('.bonus-row').first()).toContainText('≥ 25%');
  await bonus.getByLabel('Ramo del premio').selectOption('senato');
  await expect(bonus).toContainText('senato subject 1');
  const probabilities = statistics.getByRole('region', { name: 'Probabilità di elezione', exact: true });
  await probabilities.locator('summary').click();
  const table = probabilities.getByRole('table');
  await expect(table.locator('thead th')).toHaveCount(8);
  await expect(table.locator('tbody tr')).toHaveCount(2);
  await expect(table.locator('tbody tr').first()).toContainText('50%');
  await expect(table.locator('tbody tr').first()).toContainText('≥ 33,3%');
  await expect(table.locator('tbody tr').first()).toContainText('—');
  await probabilities.getByLabel('Lista delle probabilità').selectOption('list:constructed-2');
  await expect(table.locator('tbody tr')).toHaveCount(3);
  await probabilities.getByLabel('Ramo delle probabilità').selectOption('senato');
  await expect(table.locator('tbody tr').first()).toContainText('100%');
  const local = statistics.locator('details').filter({ has: page.locator('summary', { hasText: /^Eletti nel collegio plurinominale$/ }) });
  await local.locator(':scope > summary').click();
  const list = local.getByLabel('Lista del collegio');
  await list.selectOption('list:constructed-2');
  await local.getByRole('combobox', { name: 'Collegio del grafico', exact: true }).selectOption('college-2');
  await expect(list).toHaveValue('list:constructed-2');
  await list.selectOption('list:constructed-1');
  await local.getByRole('combobox', { name: 'Collegio del grafico', exact: true }).selectOption('college-3');
  await expect(list).toHaveValue('list:constructed-2');
  await local.getByLabel('Ramo del collegio').selectOption('senato');
  await expect(list).toHaveValue('list:constructed-2');
  const single = page.getByText('Seggi della singola simulazione', { exact: true });
  await expect(page.getByRole('figure')).not.toBeVisible();
  await single.click(); await expect(page.getByRole('figure')).toBeVisible();
  const activity = page.getByLabel('Stato applicazione', { exact: true });
  await expect(activity).not.toHaveAttribute('open');
  expect(await activity.evaluate(e => getComputedStyle(e).position)).toBe('fixed');
  await activity.locator('summary').click(); await expect(activity).toHaveAttribute('open', '');
  await page.getByLabel('Data proiezione', { exact: true }).fill('2099-01-01');
  const toasts = page.getByLabel('Messaggi di interazione');
  await expect(toasts).toContainText('Projection date is after election date');
  await page.screenshot({ path: 'test-results/reform-refinements-feedback.png' });
  await toasts.getByRole('button', { name: 'Chiudi messaggio' }).click();
  await expect(toasts.getByRole('alert')).toHaveCount(0);
  await expect(page.getByRole('button', { name: 'Esegui riforma', exact: true })).toBeDisabled();
  await page.getByLabel('Data proiezione', { exact: true }).fill(scenario.projectionDate);
  await page.getByTestId('reform-file-input').setInputFiles({ name: 'invalid.json', mimeType: 'application/json', buffer: Buffer.from('{') });
  await expect(toasts.getByRole('alert')).toHaveCount(1);
  await toasts.getByRole('button', { name: 'Chiudi messaggio' }).click();
  await expect(toasts.getByRole('alert')).toHaveCount(0);
  await page.screenshot({ path: 'test-results/reform-refinements-desktop.png', fullPage: true });
  await page.setViewportSize({ width: 390, height: 844 });
  await probabilities.scrollIntoViewIfNeeded();
  expect(await statistics.evaluate(e => e.scrollWidth)).toBeLessThanOrEqual(await statistics.evaluate(e => e.clientWidth));
  await page.screenshot({ path: 'test-results/reform-refinements-mobile.png', fullPage: true });
  await page.route(`**/${catalog.path}`, route => route.fulfill({ status: 404, body: 'missing' }));
  await page.getByLabel('Simulazioni', { exact: true }).fill('1');
  await page.getByRole('button', { name: 'Esegui riforma', exact: true }).click();
  await expect(page.getByTestId('reform-phase')).toHaveText('not_implemented');
  await expect(page.getByLabel('Risultati riforma', { exact: true })).toContainText('Missing production bundle');
  await expect(toasts.getByRole('alert')).toHaveCount(0);
  expect(errors).toEqual([]);
});
